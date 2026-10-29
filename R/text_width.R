# text_width.R -- Font-aware text width estimation for column sizing.
#
# Provides:
#   text_width_in()   -- estimated display width of a string in inches
#   auto_col_widths() -- suggested column widths (twips) for a data.frame

# Courier New character widths at 12pt in points.
# All characters are equal-width (monospace), so a single constant is used.
.COURIER_CHAR_W_PT_AT_12 <- 7.22

# Arial approximate average character width at 12pt in points.
# This is a simplified constant; for production use consider a per-glyph table.
.ARIAL_CHAR_W_PT_AT_12 <- 6.0

# Internal: character width in inches for one character.
.char_width_in <- function(font_lower, size_pt) {
  base_w <- switch(font_lower,
    courier     = .COURIER_CHAR_W_PT_AT_12,
    courier_new = .COURIER_CHAR_W_PT_AT_12,
    arial       = .ARIAL_CHAR_W_PT_AT_12,
    .COURIER_CHAR_W_PT_AT_12            # fallback: treat as Courier
  )
  (base_w * size_pt / 12) / 72          # scale to size, convert pt -> inches
}


#' Estimate the display width of a text string
#'
#' Returns an estimated display width in **inches** for a character string
#' rendered in the given font and size.  For Courier New (monospace) the
#' estimate is reliable; for proportional fonts (Arial) it is an average-width
#' approximation.
#'
#' @param text A character vector.  `NA` is treated as `""`.
#' @param font Font name: `"Courier New"` / `"courier_new"`, `"Courier"`, or
#'   `"Arial"` (case and spaces do not matter).  `NULL` (default) is the
#'   package's font, `getOption("rtfreporter.font")` ("Courier New"), as for every
#'   other `font =`.  Unrecognised values fall back to Courier New.
#' @param font_size_half_points Font size in **half-points** (the unit used by
#'   RTF and the document's `default_format$font_size_half_points`).  Default
#'   `18` = 9 pt.
#' @param size_half_points *Superseded* by `font_size_half_points` (the name
#'   every other function uses); documented, still working, not warned.
#'
#' @return A numeric vector of estimated widths in inches (same length as
#'   `text`).
#'
#' @examples
#' text_width_in("Hello, World!")          # ~0.88 inches at 9pt Courier New
#' text_width_in("abc", font_size_half_points = 24)   # 12pt
#'
#' @export
text_width_in <- function(text, font = NULL, font_size_half_points = 18L,
                          size_half_points = NULL) {
  text <- ifelse(is.na(text), "", as.character(text))
  size_pt  <- as.numeric(size_half_points %||% font_size_half_points) / 2
  char_w   <- .char_width_in(.font_key(font), size_pt)
  .text_columns(text, font) * char_w
}

# A font as the width tables know it: "Courier New" -> "courier_new"; NULL
# is the package's font (the option), as for every other `font =` (#594).
# A font named per script slot is measured by its `latin` family.
.font_key <- function(font) {
  f <- font %||% .opt("rtfreporter.font") %||% "courier_new"
  if (.is_slotted(f)) f <- f[["latin"]]
  tolower(gsub("[ -]+", "_", trimws(f)))
}

# How many character columns `text` takes.  A single family counts every
# character as one, as it always has.  A font with an `east_asian` slot sets
# CJK in a font whose ideographs, kana, Hangul and fullwidth forms are twice
# the width of its Latin letters, so those count as two.
.text_columns <- function(text, font = NULL) {
  f <- font %||% .opt("rtfreporter.font")
  n <- nchar(text, type = "chars")
  if (.is_slotted(f) && !is.na(f["east_asian"])) n <- n + .ea_wide_count(text)
  n
}

# Unicode East_Asian_Width W and F, as the blocks a CJK document actually
# uses -- the ranges below, not the full UAX #11 table.
.EA_WIDE_RANGES <- matrix(c(
  0x1100, 0x115F,   # Hangul Jamo (leading consonants)
  0x2E80, 0x303E,   # CJK radicals, Kangxi, ideographic description, CJK punctuation
  0x3041, 0x33FF,   # Hiragana, Katakana, Bopomofo, Hangul compat., Kanbun, CJK compat.
  0x3400, 0x4DBF,   # CJK Unified Ideographs Extension A
  0x4E00, 0x9FFF,   # CJK Unified Ideographs
  0xA000, 0xA4CF,   # Yi
  0xA960, 0xA97F,   # Hangul Jamo Extended-A
  0xAC00, 0xD7A3,   # Hangul Syllables
  0xF900, 0xFAFF,   # CJK Compatibility Ideographs
  0xFE10, 0xFE19,   # Vertical forms
  0xFE30, 0xFE6F,   # CJK compatibility forms, small form variants
  0xFF00, 0xFF60,   # Fullwidth forms
  0xFFE0, 0xFFE6,   # Fullwidth signs
  0x1F300, 0x1F64F, # Pictographs, emoticons
  0x20000, 0x3FFFD  # CJK Extensions B and beyond (planes 2-3)
), ncol = 2L, byrow = TRUE)

# Per string, the number of East Asian wide / fullwidth characters.
.ea_wide_count <- function(text) {
  text <- as.character(text)
  text[is.na(text)] <- ""
  vapply(text, function(t) {
    cp <- utf8ToInt(enc2utf8(t))
    if (!length(cp) || anyNA(cp) || all(cp < 0x1100)) return(0L)
    sum(vapply(seq_len(nrow(.EA_WIDE_RANGES)), function(i) {
      sum(cp >= .EA_WIDE_RANGES[i, 1L] & cp <= .EA_WIDE_RANGES[i, 2L])
    }, integer(1L)))
  }, integer(1L), USE.NAMES = FALSE)
}


#' Automatically calculate column widths for a data.frame
#'
#' Scans the content of each column (header labels and data values) to derive
#' suggested column widths in **twips**.  The widths can be passed directly to
#' [rtftable()]'s `column_widths_twips` argument.
#'
#' @param data A `data.frame`.
#' @param col_header Column header labels.  `NULL` (default) uses `names(data)`.
#'   Accepts the same formats as `rtftable(col_header = ...)`: a character
#'   vector, a pipe-delimited string, or a list of character vectors (only the
#'   first row is used for width estimation).
#' @param font Font name passed to [text_width_in()].  `NULL` (default) is
#'   the package's font, `getOption("rtfreporter.font")`.
#' @param font_size_half_points Font size in half-points.  Default `18` (= 9 pt).
#' @param df,size_half_points *Superseded* by `data` and
#'   `font_size_half_points` (the names the other functions use); documented,
#'   still working, not warned.
#' @param table_width_twips If not `NULL`, the column widths are scaled so that
#'   their sum equals this value.  Useful for fitting tables to a fixed page
#'   width.
#' @param min_col_width_twips Minimum width per column in twips.  Default
#'   `720` (= 0.5 inch).
#' @param col_padding_twips Extra twips added to each column width to account
#'   for cell padding and inter-column spacing.  Default `288` (= 0.2 inch).
#' @param protect_cols Integer column indices to keep at their natural
#'   (content) width when `table_width_twips` forces the table *narrower* than
#'   its natural width.  Only the remaining columns are shrunk to fit, so e.g.
#'   a row-label column (`protect_cols = 1`) stays readable while the data
#'   columns absorb the squeeze.  Protection is dropped if it would push the
#'   scalable columns below `min_col_width_twips`.  When the table is scaled
#'   *up*, the protected columns likewise keep their natural width and only the
#'   others grow to the target.  No effect when `table_width_twips` is `NULL`.
#'   Default none.
#'
#' @return An integer vector of column widths in twips, one per column of `data`.
#'
#' @examples
#' df <- data.frame(
#'   USUBJID  = c("SUBJ-001", "SUBJ-002"),
#'   TREATMENT = c("Placebo", "Active"),
#'   AGE      = c(45L, 62L)
#' )
#' widths <- auto_col_widths(df, table_width_twips = 14400L)
#' tbl <- rtftable(df, column_widths_twips = widths)
#'
#' @export
auto_col_widths <- function(data = NULL,
                             col_header         = NULL,
                             font               = NULL,
                             font_size_half_points = 18L,
                             table_width_twips  = NULL,
                             min_col_width_twips = 720L,
                             col_padding_twips  = 288L,
                             protect_cols       = integer(0),
                             df                 = NULL,
                             size_half_points   = NULL) {
  df <- data %||% df
  if (!is.data.frame(df)) stop("`data` must be a data.frame.", call. = FALSE)
  ncols   <- ncol(df)
  size_pt <- as.numeric(size_half_points %||% font_size_half_points) / 2

  # Resolve header labels (use first row when multi-row).
  hdr_labels <- if (is.null(col_header)) {
    names(df)
  } else {
    # Normalize: pipe string -> char vector; list -> first row.
    h <- col_header
    if (is.character(h) && length(h) == 1L && grepl("|", h, fixed = TRUE)) {
      h <- trimws(strsplit(h, "|", fixed = TRUE)[[1]])
    }
    if (is.list(h)) h <- h[[1L]]     # first header row only
    h
  }
  if (length(hdr_labels) < ncols) {
    hdr_labels <- c(hdr_labels, rep("", ncols - length(hdr_labels)))
  }

  # Longest single line (split on CR/LF) of each string -- a multi-line cell
  # (e.g. a column header like "Placebo\nN = 86") only needs to be as wide as
  # its widest line, not the sum of its lines.
  .max_line_nchar <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    vapply(strsplit(x, "\r\n|\r|\n"), function(lines) {
      if (length(lines) == 0L) 0L else max(nchar(lines, type = "chars"))
    }, integer(1L))
  }

  # Maximum content width per column (header vs data), in inches.
  char_w <- .char_width_in(.font_key(font), size_pt)
  col_max_in <- vapply(seq_len(ncols), function(j) {
    hdr_w  <- max(0L, .max_line_nchar(hdr_labels[j] %||% "")) * char_w
    data_w <- max(0L, .max_line_nchar(df[[j]])) * char_w
    max(hdr_w, data_w)
  }, numeric(1L))

  # Convert to twips and add padding.
  col_w <- as.integer(round(col_max_in * 1440)) + as.integer(col_padding_twips)
  col_w <- pmax(col_w, as.integer(min_col_width_twips))

  # Scale to table_width_twips when requested.
  if (!is.null(table_width_twips)) {
    total_w <- as.integer(table_width_twips)
    protect <- intersect(as.integer(protect_cols), seq_len(ncols))

    # Columns named in `protect_cols` are kept at their natural (content) width
    # while the remaining columns are scaled to make the row sum equal
    # `total_w`.  This lets the row-label column stay readable (no mid-word
    # wrapping) when a wide table is squeezed onto the page; only the data
    # columns shrink (their headers may wrap instead).  Protection is dropped
    # if it would leave the scalable columns below their minimum width.
    scalable <- setdiff(seq_len(ncols), protect)
    if (length(protect) && length(scalable)) {
      fixed_w     <- sum(col_w[protect])
      remaining   <- total_w - fixed_w
      min_needed  <- length(scalable) * as.integer(min_col_width_twips)
      if (remaining >= min_needed) {
        nat_scalable <- sum(col_w[scalable])
        if (nat_scalable > 0L) {
          col_w[scalable] <- as.integer(round(col_w[scalable] *
                                                remaining / nat_scalable))
          col_w[scalable] <- pmax(col_w[scalable],
                                   as.integer(min_col_width_twips))
        }
        drift <- total_w - sum(col_w)
        last  <- scalable[length(scalable)]
        col_w[last] <- col_w[last] + drift
        return(col_w)
      }
      # Fall through to uniform scaling when protection cannot be honoured.
    }

    total_natural <- sum(col_w)
    if (total_natural > 0L) {
      col_w <- as.integer(round(col_w * total_w / total_natural))
      # Absorb rounding drift in the last column.
      col_w[ncols] <- total_w - sum(col_w[-ncols])
      col_w <- pmax(col_w, as.integer(min_col_width_twips))
    }
  }

  col_w
}

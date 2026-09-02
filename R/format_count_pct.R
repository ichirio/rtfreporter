# ============================================================================
#  format_count_pct() / realign_count_pct()
# ============================================================================
#
#  Clinical TFL cells of the form "n (xx.x)" need consistent display widths
#  so that columns line up in a monospaced renderer.  These two functions
#  produce the same output, from two different starting points:
#
#    format_count_pct(count, pct)
#        - inputs: numeric vectors `count` and `pct`
#        - output: padded "n (xx.x)" strings, equal-width
#
#    realign_count_pct(strings)
#        - inputs: character vector already containing "n (xx.x)" cells
#                  (e.g. extracted from a gt body)
#        - output: same cells re-padded for equal width.  Non-matching
#                  strings are passed through unchanged.
#
#  Width rules -- every branch produces a 10-character string, and the
#  closing parenthesis lands at column 10 so it lines up across rows:
#
#       count missing (NA/NaN)    ->  the `na` token, right-justified in
#                                     the count field ("" = empty cell)
#       count = 0, or pct missing ->  "%3d       "           (no paren)
#       pct  >= 100               ->  "%3d  (%3d)"           e.g. " 30  (100)"
#       0 < pct < 10              ->  "%3d  (%3.1f)"         e.g. "  5  (5.0)"
#       10 <= pct < 100           ->  "%3d (%4.1f)"          e.g. " 14 (50.0)"
#
#  Padding spaces are converted to non-breaking spaces (U+00A0) by default
#  so RTF / Word does not collapse them.
# ============================================================================


#' Format count + percent cells to a uniform display width
#'
#' Returns each pair `(count[i], pct[i])` as a padded `"n (xx.x)"`
#' string suitable for monospaced clinical TFL alignment.  The four
#' width branches match the convention in the rtfreporter Issue #2
#' reference helper.
#'
#' @param count Integer / numeric vector of counts.  A `0` count produces the
#'   count-only branch (no parentheses); a **missing** count produces the `na`
#'   token instead.
#' @param pct   Numeric vector of percentages.  By default expressed
#'   as a *fraction* in `[0, 1]`; pass `pct_unit = "percent"` if your
#'   values are already in `[0, 100]`.  Recycled against `count` if
#'   one argument is length 1.
#' @param pct_unit Either `"fraction"` (default, `0..1`) or
#'   `"percent"` (`0..100`).
#' @param nbsp Character used to replace the padding spaces.  Default
#'   is the non-breaking space (Unicode code point U+00A0) so that RTF
#'   and Word do not collapse leading whitespace.  Pass `" "` (regular
#'   space) for plain-text output.
#' @param pct_sign Logical (default `FALSE`).  When `TRUE`, a literal
#'   `%` is placed before the closing parenthesis (e.g. `" 14 (50.0%)"`)
#'   and every branch is one character wider so the `)` still aligns.
#' @param na Text to print when the **count** is missing (`NA`, or `NaN` --
#'   R counts it as missing).  Right-justified in the count field, so its right
#'   edge lands on the ones digit, then padded to the branch width.  The
#'   default `""` returns an empty cell.  A missing *percent* alongside a real
#'   count is not missing data: the count still prints, on its own.  `Inf` /
#'   `-Inf` counts print as `"Inf"` / `"-Inf"` -- an infinity means a division
#'   by zero upstream, and hiding it would hide the bug.
#'
#' @return Character vector the same length as `count` / `pct`.
#'
#' @examples
#' # Fractions (the default)
#' format_count_pct(c(5L, 14L, 30L), c(0.05, 0.50, 1.00))
#'
#' # Percent values
#' format_count_pct(c(5L, 14L, 30L), c(5, 50, 100), pct_unit = "percent")
#'
#' # Plain spaces if the output is going to plain text rather than RTF
#' format_count_pct(7L, 0.333, nbsp = " ")
#'
#' # A missing count, shown as "-" under the ones digit
#' format_count_pct(c(5L, NA), c(0.05, NA), na = "-", nbsp = " ")
#'
#' @seealso [realign_count_pct()] for the same widths starting from
#'   already-formatted strings.
#' @export
format_count_pct <- function(count, pct,
                              pct_unit = c("fraction", "percent"),
                              nbsp     = "\u00a0",
                              pct_sign = FALSE,
                              na       = "") {
  pct_unit <- match.arg(pct_unit)
  na <- .check_na_text(na)
  if (!is.numeric(count) || !is.numeric(pct)) {
    stop("`count` and `pct` must both be numeric.", call. = FALSE)
  }
  n_in <- max(length(count), length(pct))
  if (length(count) == 1L) count <- rep(count, n_in)
  if (length(pct)   == 1L) pct   <- rep(pct,   n_in)
  if (length(count) != length(pct)) {
    stop("`count` and `pct` must have the same length (or one of them ",
         "be length 1).", call. = FALSE)
  }
  if (pct_unit == "fraction") pct <- pct * 100

  # When pct_sign = TRUE a "%" is added before the closing paren and every
  # branch is one character wider, so the ")" still aligns across cells.
  out <- vapply(seq_len(n_in), function(i) {
    c1 <- count[i]; p <- pct[i]
    w <- if (pct_sign) 11L else 10L                    # full width of a cell
    if (is.na(c1)) {
      # The count itself is missing -- print the token in the count field.
      if (!nzchar(na)) return("")
      raw <- formatC(formatC(na, width = 3L, flag = ""), width = w, flag = "-")
    } else if (!is.finite(c1)) {
      # Inf / -Inf is not missing; show it rather than hide a bad numerator.
      raw <- formatC(formatC(as.character(c1), width = 3L, flag = ""),
                     width = w, flag = "-")
    } else if (!is.finite(p) || c1 == 0) {
      # A real count with no usable percent still prints, on its own.
      raw <- if (pct_sign) sprintf("%3d        ", as.integer(c1))
             else          sprintf("%3d       ",  as.integer(c1))
    } else if (p >= 100) {
      # Two spaces before '(' so the ')' aligns with the other
      # paren-bearing branches.
      raw <- if (pct_sign) sprintf("%3d  (%3d%%)", as.integer(c1), round(p))
             else          sprintf("%3d  (%3d)",   as.integer(c1), round(p))
    } else if (p < 10) {
      raw <- if (pct_sign) sprintf("%3d  (%3.1f%%)", as.integer(c1), p)
             else          sprintf("%3d  (%3.1f)",   as.integer(c1), p)
    } else {
      raw <- if (pct_sign) sprintf("%3d (%4.1f%%)", as.integer(c1), p)
             else          sprintf("%3d (%4.1f)",   as.integer(c1), p)
    }
    raw
  }, character(1L))

  if (!identical(nbsp, " ")) out <- gsub(" ", nbsp, out, fixed = TRUE)
  out
}


#' Re-align existing "n (xx.x)" strings to a uniform display width
#'
#' Scans `x` for cells matching the clinical-TFL pattern `"n (xx.x)"`
#' (e.g. `"5 (33.3)"`), parses the count and percent, and reformats
#' them through [format_count_pct()] so every cell is the same width.
#' Cells that do not match are returned unchanged.
#'
#' This is the function `paginate()` invokes internally when
#' `align_count_pct = TRUE` (see [paginate()]).  It is exported so it
#' can be applied directly to a data.frame column outside of any
#' pagination context.
#'
#' @param x Character vector.  Cells that match the regex
#'   `^\\d+ \\(\\d+(\\.\\d+)?\\)$` are reformatted; all others are
#'   returned unchanged.
#' @param nbsp Padding character (see [format_count_pct()]).
#' @param na Text to print for a missing cell -- an `NA` in `x`, or a cell that
#'   already holds this token because [as_rtftables()] substituted it.  It is
#'   right-justified in the count field, so its right edge lands on the ones
#'   digit.  The default `""` leaves the cell empty, as before.  Text that is
#'   neither the token nor a count/percent cell (`"NE"`, `"n/a"`, free text) is
#'   still returned unchanged and unpadded.
#'
#' @return Character vector the same length as `x`.
#'
#' @examples
#' realign_count_pct(c("5 (33.3)", "12 (100.0)", "0 (0.0)",
#'                     "not a count", "1 (5.0)", "1 (50.0)"))
#'
#' # A missing cell, shown as "-" under the ones digit
#' realign_count_pct(c("5 (33.3)", NA, "12 (100.0)"), na = "-", nbsp = " ")
#'
#' @seealso [format_count_pct()] for the numeric -> string variant.
#' @export
realign_count_pct <- function(x, nbsp = "\u00a0", na = "") {
  if (is.null(x) || length(x) == 0L) return(x)
  na <- .check_na_text(na)
  if (!is.character(x)) x <- as.character(x)
  out <- x
  # Optional trailing "%" inside the parens is captured so that cells like
  # "8 (28.6%)" (e.g. from tern::count_occurrences) are realigned WITH the
  # "%" preserved.  Cells without "%" keep the original "n (xx.x)" form.
  rx  <- "^\\s*(\\d+)\\s*\\((\\d+(?:\\.\\d+)?)(%?)\\)\\s*$"
  m   <- regmatches(x, regexec(rx, x))
  hit <- vapply(m, function(g) length(g) == 4L && !is.na(g[1L]) &&
                               nzchar(g[1L]), logical(1L))
  # A missing cell has no "%" of its own to go by, so it follows whatever the
  # rest of the column does -- every branch is one character wider with a "%".
  # And with no count/percent cell anywhere in the column there is nothing to
  # line the token up WITH, so it is left alone rather than padded to a width
  # this column never uses.
  col_pct_sign <- any(vapply(m[hit], function(g) nzchar(g[4L]), logical(1L)))
  pad_na       <- nzchar(na) && any(hit)
  for (i in seq_along(x)) {
    if (hit[i]) {
      g        <- m[[i]]
      n        <- as.integer(g[2L])
      pct      <- as.numeric(g[3L])
      pct_sign <- nzchar(g[4L])
      out[i] <- format_count_pct(n, pct, pct_unit = "percent",
                                 nbsp = nbsp, pct_sign = pct_sign)
    } else if (nzchar(na) &&
               (is.na(x[i]) || identical(trimws(x[i]), na))) {
      out[i] <- if (pad_na) {
        format_count_pct(NA_integer_, NA_real_, pct_unit = "percent",
                         nbsp = nbsp, pct_sign = col_pct_sign, na = na)
      } else {
        na                       # shown, but with nothing to line it up with
      }
    }
  }
  out
}


# Internal: realign the count-percent cells of every character column of `df`
# except the first (the row label by clinical convention).  Used by
# paginate(align_count_pct = TRUE).
#
# Only cells of the form "integer (real)" -- the real part optionally ending in
# "%" -- and cells holding the `na` token are reformatted by
# realign_count_pct().  Everything else (a bare integer / plain N such as "86",
# a continuous statistic like "75.2 (8.59)" whose "count" is not a bare
# integer, free text, and empty cells) is returned UNCHANGED.  Bare integers are NOT padded (#80 wrongly padded them against the
# count-percent cells; that is removed -- #148).
.realign_count_pct_df <- function(df, nbsp = "\u00a0", na = "") {
  if (ncol(df) < 2L) return(df)
  df[, -1L] <- lapply(df[, -1L, drop = FALSE], function(col) {
    if (!is.character(col)) return(col)
    realign_count_pct(col, nbsp = nbsp, na = na)
  })
  df
}

# Internal utility: inches to twips.
.in_to_twips <- function(x) {
  as.integer(round(x * 1440))
}

# Make page dimensions agree with the page orientation, so the RTF orientation
# flag and the paper dimensions never contradict each other.
#
#   * orientation = "landscape" -> width is the LONG side, height the short one.
#   * orientation = "portrait"  -> width is the short side, height the long one.
#   * orientation = NULL        -> inferred from the dimensions
#                                  (width >= height => landscape, else portrait).
#
# Returns list(orientation, width_twips, height_twips).
.orient_page <- function(width_twips, height_twips, orientation = NULL) {
  width_twips  <- as.integer(width_twips)
  height_twips <- as.integer(height_twips)
  long  <- max(width_twips, height_twips)
  short <- min(width_twips, height_twips)
  if (is.null(orientation)) {
    orientation <- if (width_twips >= height_twips) "landscape" else "portrait"
    return(list(orientation = orientation,
                width_twips = width_twips, height_twips = height_twips))
  }
  if (!orientation %in% c("landscape", "portrait")) {
    stop("`orientation` must be \"landscape\" or \"portrait\".", call. = FALSE)
  }
  if (orientation == "landscape") {
    list(orientation = "landscape", width_twips = long,  height_twips = short)
  } else {
    list(orientation = "portrait",  width_twips = short, height_twips = long)
  }
}

# Named paper-size presets. Portrait base dimensions in inches (width <= height);
# `.resolve_page_geometry()` orients them. Extend this list to add more sizes.
.paper_sizes <- list(
  letter = c(8.5,     11),        # 8.5 x 11"
  legal  = c(8.5,     14),        # 8.5 x 14"
  a4     = c(8.2677,  11.6929),   # 210 x 297 mm
  a3     = c(11.6929, 16.5354),   # 297 x 420 mm
  a5     = c(5.8268,   8.2677)    # 148 x 210 mm
)

# Resolve the final page orientation + paper dimensions (twips) from a `page`
# spec list, per the rules finalized in issue #110:
#
#   1. Explicit `width_in` / `height_in` WIN and are used **as given**; the
#      orientation is inferred from them (width >= height => landscape). An
#      explicit `orientation` that contradicts the dimensions emits a warning --
#      the dimensions are NOT swapped. If `paper_size` is also supplied it is
#      ignored (with a warning).
#   2. Else `paper_size` (+ `orientation`) selects a preset, oriented to match
#      `orientation` (default "landscape").
#   3. Else the package default: landscape letter.
#
# Returns list(orientation, width_twips, height_twips).
.resolve_page_geometry <- function(ps) {
  ps <- ps %||% list()
  orientation <- ps$orientation
  if (!is.null(orientation) && !orientation %in% c("landscape", "portrait")) {
    stop("`orientation` must be \"landscape\" or \"portrait\".", call. = FALSE)
  }
  has_dims <- !is.null(ps$width_in) || !is.null(ps$height_in)

  if (has_dims) {
    if (!is.null(ps$paper_size)) {
      warning("Both `paper_size` and `width_in`/`height_in` were supplied; the ",
              "explicit dimensions take precedence and `paper_size` is ignored.",
              call. = FALSE)
    }
    w <- ps$width_in  %||% 11
    h <- ps$height_in %||% 8.5
    inferred <- if (w >= h) "landscape" else "portrait"
    if (!is.null(orientation) && orientation != inferred) {
      warning(sprintf(
        paste0("`orientation = \"%s\"` contradicts the given dimensions ",
               "(%g x %g in, => %s); using the dimensions as given."),
        orientation, w, h, inferred), call. = FALSE)
    }
    return(list(orientation  = inferred,
                width_twips  = .in_to_twips(w),
                height_twips = .in_to_twips(h)))
  }

  paper <- ps$paper_size %||% "letter"
  key   <- tolower(paper)
  if (!key %in% names(.paper_sizes)) {
    stop(sprintf("Unknown `paper_size` \"%s\". Known sizes: %s.",
                 paper, paste(names(.paper_sizes), collapse = ", ")),
         call. = FALSE)
  }
  base <- .paper_sizes[[key]]
  .orient_page(.in_to_twips(base[1L]), .in_to_twips(base[2L]),
               orientation %||% "landscape")
}

# Internal utility: merge two named lists (override wins).
.merge_list <- function(base, override) {
  if (is.null(override)) return(base)
  out <- base
  for (nm in names(override)) out[[nm]] <- override[[nm]]
  out
}

# Internal utility: validate positive integer-like index.
.assert_index <- function(x, max_value, label) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1L || x > max_value) {
    stop(sprintf("%s is out of range.", label), call. = FALSE)
  }
  as.integer(x)
}

# Internal utility: normalize a raw content value to rtftable or rtfplot.
.normalize_content <- function(item) {
  if (is.null(item)) return(NULL)
  if (inherits(item, "rtftable") || inherits(item, "rtfplot")) return(item)
  if (is.data.frame(item)) return(rtftable(data = item))
  stop("content must be an rtftable, rtfplot, or data.frame.", call. = FALSE)
}

# -- Internal S3 object constructors ------------------------------------------

# rtf_page: structured page object stored in rtfreport$pages
#   title    -- character vector (multi-line, center)
#   content  -- rtftable | rtfplot | NULL (exactly 1 per page)
#   footnote -- character vector (multi-line, rendered as left-aligned paragraphs)
.new_page <- function(title = NULL, content = NULL, footnote = NULL) {
  structure(
    list(title = title, content = content, footnote = footnote),
    class = "rtf_page"
  )
}

# rtf_sect: structured section definition stored in rtfreport$sections
#   header    -- rtf_header() | named vector | NULL (NULL = inherit from previous)
#   footer    -- rtf_footer() | named vector | NULL (NULL = inherit from previous)
#   from_page -- integer: first page this section applies to
.new_sect <- function(header = NULL, footer = NULL, from_page = NULL) {
  structure(
    list(header = header, footer = footer, from_page = from_page),
    class = "rtf_sect"
  )
}

# -- Internal helper: update a single row in a header/footer rows list ---------

.update_hf_rows <- function(hf, row, content) {
  if (!is.list(hf) || is.null(hf$rows)) {
    stop("First argument must be an rtf_header() or rtf_footer() object.", call. = FALSE)
  }
  row <- as.integer(row)
  if (row < 1L) stop("`row` must be >= 1.", call. = FALSE)
  # Normalize to a plain character vector, but preserve names.
  if (is.character(content) && !is.list(content)) {
    nm      <- names(content)
    content <- as.vector(content)
    if (length(nm)) names(content) <- nm
  }

  current_rows <- hf$rows
  n_current    <- length(current_rows)

  if (row > n_current) {
    # Fill gap with empty center rows, then append content at target row.
    n_gap <- row - n_current - 1L
    if (n_gap > 0L) {
      gap <- replicate(n_gap, c(c = ""), simplify = FALSE)
      current_rows <- c(current_rows, gap)
    }
    current_rows <- c(current_rows, list(content))
  } else {
    # Replace existing row.
    current_rows[[row]] <- content
  }

  hf$rows <- current_rows
  hf
}

#' Update a specific row in an `rtf_header()` object
#'
#' Adds a new row or replaces an existing row in a header/footer object.
#' If `row` is beyond the current number of rows, intermediate rows are
#' auto-filled with empty center-aligned rows (`c(c = "")`).
#'
#' @param header An `rtf_header()` object (returned by `rtf_header()`).
#' @param row Integer. Target row number (1-based).
#' @param content A named character vector for the row (e.g.
#'   `c(l = "Left", r = "Right")`). See `rtf_header()` for column rules.
#'
#' @return A modified `rtf_header()` object.
#'
#' @examples
#' hdr <- rtf_header(rows = list(
#'   c(l = "Protocol: XXX-001", r = "Company"),
#'   c(l = "Table 14.1.1",     r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}")
#' ))
#'
#' hdr <- update_header_row(hdr, row = 2, content = c(l = "Table 14.2.1", r = "Page {AUTO_PAGE}"))
#' hdr <- update_header_row(hdr, row = 3, content = c(c = "Draft - Confidential"))
#' hdr <- update_header_row(hdr, row = 5, content = c(l = "Run date: 2026-01-01"))
#'
#' @export
update_header_row <- function(header, row, content) {
  .update_hf_rows(header, row, content)
}

#' @rdname update_header_row
#' @param footer An `rtf_footer()` object (returned by `rtf_footer()`).
#' @export
update_footer_row <- function(footer, row, content) {
  .update_hf_rows(footer, row, content)
}

# -- rtf_header() / rtf_footer() ----------------------------------------------

#' Create a header or footer object for a section
#'
#' `rtf_header()` and `rtf_footer()` create structured header/footer objects
#' that can be passed to `rtf_section()`. Use [update_header_row()] /
#' [update_footer_row()] to add or replace individual rows after creation.
#'
#' @param rows The header (or footer) content, row by row: a single named
#'   character vector for one row, or a `list` of them for several rows. Within
#'   a row, the name of each element chooses its column:
#'   \describe{
#'     \item{`l`}{left-aligned text}
#'     \item{`c`}{centred text}
#'     \item{`r`}{right-aligned text}
#'   }
#'   e.g. `c(l = "Protocol XYZ-001", r = "Page {AUTO_PAGE}")`. Cell text may
#'   contain **page-number tokens** that the renderer substitutes: `{AUTO_PAGE}`
#'   (the current page, updated live by the viewer), `{AUTO_TOTAL_PAGES}` (the
#'   document total), and `{PAGE}` / `{TOTAL_PAGES}` (static numbers baked in at
#'   render time).
#' @param border An [rtf_border()] object controlling the border applied to
#'   all rows of the header/footer table. `NULL` = no border (default for
#'   header). Use [rtf_border(top = TRUE)] for a horizontal dividing line (default
#'   for footer).
#' @param width_twips Integer. Table width in twips. `NULL` (default) uses the
#'   full writable width (page width minus margins).
#' @param width Band width in the shared vocabulary: `"page"` (the writable
#'   width, the default), a fraction in `(0, 1]` of it, or twips. `width_twips`
#'   is the older absolute-only form and wins when both are given.
#' @param font_size_half_points Font size for this band, in half-points.
#'   `NULL` (default) inherits the document size. Setting it also recomputes
#'   the row height from that size unless `row_height_twips` is given.
#' @param font Font family for this band, e.g. `"Arial"`. `NULL` (default)
#'   uses the document font. A family not already in the document's
#'   `font_table` is added to it automatically, the way a colour is.
#' @param markup Cell-text markup for this band; `NULL` (default) inherits the
#'   document setting. See [rtftable()].
#' @param row_height_twips Integer. Row height in twips. `NULL` (default) reads
#'   the value from `inst/resources/rtfreporter_defaults.R`.
#' @param cell_padding_left_twips,cell_padding_right_twips Integer cell padding
#'   on the left / right side of each header (or footer) cell, matching the
#'   content-table convention. `NULL` (default) reads from
#'   `inst/resources/rtfreporter_defaults.R` (0L for both since v0.0.21).
#'
#' @return A named list with elements `rows`, `border`, `width_twips`, and
#'   `row_height_twips`.
#'
#' @examples
#' hdr <- rtf_header(
#'   rows = list(
#'     c(l = "Protocol: RTF-101", r = "ACME Pharma"),
#'     c(l = "Table 14.1.1",     r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}")
#'   )
#' )
#' ftr <- rtf_footer(c(l = "Confidential"))
#'
#' hdr <- update_header_row(hdr, row = 3, content = c(c = "Draft"))
#'
#' @export
rtf_header <- function(rows,
                        border                   = NULL,
                        width_twips              = NULL,
                        width                    = NULL,
                        row_height_twips         = NULL,
                        font_size_half_points    = NULL,
                        font                     = NULL,
                        markup                   = NULL,
                        cell_padding_left_twips  = NULL,
                        cell_padding_right_twips = NULL) {
  if (!is.null(border) && !inherits(border, "rtf_border")) {
    stop("`border` must be NULL or an rtf_border object.", call. = FALSE)
  }
  if (is.character(rows)) rows <- list(rows)
  if (!is.list(rows)) stop("`rows` must be a named character vector or list of named vectors.", call. = FALSE)
  width <- .check_block_width(width, "width")
  list(rows = rows, border = border, width_twips = width_twips,
       width = width,
       font_size_half_points =
         .check_font_size(font_size_half_points, "font_size_half_points"),
       font = .check_font(font, "font"),
       markup = if (is.null(markup)) NULL else .resolve_markup(markup),
       row_height_twips         = row_height_twips,
       cell_padding_left_twips  = cell_padding_left_twips,
       cell_padding_right_twips = cell_padding_right_twips)
}

#' @rdname rtf_header
#' @export
rtf_footer <- function(rows,
                        border                   = rtf_border(top = TRUE),
                        width_twips              = NULL,
                        width                    = NULL,
                        row_height_twips         = NULL,
                        font_size_half_points    = NULL,
                        font                     = NULL,
                        markup                   = NULL,
                        cell_padding_left_twips  = NULL,
                        cell_padding_right_twips = NULL) {
  if (!is.null(border) && !inherits(border, "rtf_border")) {
    stop("`border` must be NULL or an rtf_border object.", call. = FALSE)
  }
  if (is.character(rows)) rows <- list(rows)
  if (!is.list(rows)) stop("`rows` must be a named character vector or list of named vectors.", call. = FALSE)
  width <- .check_block_width(width, "width")
  list(rows = rows, border = border, width_twips = width_twips,
       width = width,
       font_size_half_points =
         .check_font_size(font_size_half_points, "font_size_half_points"),
       font = .check_font(font, "font"),
       markup = if (is.null(markup)) NULL else .resolve_markup(markup),
       row_height_twips         = row_height_twips,
       cell_padding_left_twips  = cell_padding_left_twips,
       cell_padding_right_twips = cell_padding_right_twips)
}

# ============================================================================
# Internal S3 type: rtfreport
# ============================================================================
#
# Structure:
#   document  -- font_table, color_table, default_page, default_format
#   pages[]   -- list of rtf_page objects: list(title, content, footnote)
#   sections[]-- list of rtf_sect objects: list(header, footer, from_page)
#
# Section-to-page mapping (resolved at render time):
#   Sections sorted by from_page. Each section covers pages from its
#   from_page up to (but not including) the next section's from_page.
#   The first section always covers from page 1.
#   If sections is empty, .rtfreport_validate() auto-creates one empty
#   default section.

# Constructor: build a fresh rtfreport with default document settings.
.new_rtfreport <- function(font_table     = NULL,
                           color_table    = NULL,
                           default_page   = NULL,
                           default_format = NULL) {
  if (is.null(font_table))  font_table  <- list(list(name = .opt("rtfreporter.font")))
  if (is.null(color_table)) color_table <- c("#000000")
  if (is.null(default_page)) {
    # Resolve the default geometry from the configurable page options (single
    # source of truth: .rtfreporter_factory_defaults()).
    geo <- .resolve_page_geometry(list(
      paper_size  = .opt("rtfreporter.page.paper_size"),
      orientation = .opt("rtfreporter.page.orientation")
    ))
    default_page <- list(
      paper               = .opt("rtfreporter.page.paper_size"),
      orientation         = geo$orientation,
      width_twips         = geo$width_twips,
      height_twips        = geo$height_twips,
      margin_top_twips    = .in_to_twips(.opt("rtfreporter.page.margin_top_in")),
      margin_bottom_twips = .in_to_twips(.opt("rtfreporter.page.margin_bottom_in")),
      margin_left_twips   = .in_to_twips(.opt("rtfreporter.page.margin_left_in")),
      margin_right_twips  = .in_to_twips(.opt("rtfreporter.page.margin_right_in"))
    )
  }
  if (is.null(default_format)) {
    default_format <- list(
      font_index            = 0L,
      font_size_half_points = as.integer(.opt("rtfreporter.font_size_half_points")),
      line_spacing          = 1L
    )
  }
  structure(
    list(
      document = list(
        font_table     = font_table,
        color_table    = color_table,
        default_page   = default_page,
        default_format = default_format
      ),
      pages    = list(),
      sections = list()
    ),
    class = "rtfreport"
  )
}

# -- Document defaults -------------------------------------------------------

.rtfreport_set_default_page <- function(report, page) {
  report$document$default_page <- .merge_list(report$document$default_page, page)
  report
}

.rtfreport_set_default_format <- function(report, fmt) {
  report$document$default_format <- .merge_list(report$document$default_format, fmt)
  report
}

.rtfreport_set_font_table <- function(report, font_table) {
  report$document$font_table <- font_table
  report
}

.rtfreport_set_color_table <- function(report, color_table) {
  report$document$color_table <- color_table
  report
}

# -- Page ops ----------------------------------------------------------------

.rtfreport_add_page <- function(report, title = NULL, content = NULL, footnote = NULL) {
  if (!is.null(content)) content <- .normalize_content(content)
  page <- .new_page(title = title, content = content, footnote = footnote)
  report$pages[[length(report$pages) + 1L]] <- page
  report
}

# -- Section ops -------------------------------------------------------------

.rtfreport_add_section <- function(report, header = NULL, footer = NULL,
                                    from_page = NULL) {
  if (!is.null(from_page)) from_page <- as.integer(from_page)
  sec <- .new_sect(header = header, footer = footer, from_page = from_page)
  report$sections[[length(report$sections) + 1L]] <- sec
  report
}

# -- Validation --------------------------------------------------------------

.rtfreport_validate <- function(report) {
  # Auto-create a default empty section if none is defined.
  if (length(report$sections) == 0L) {
    message("No sections defined -- creating a default empty section (no header/footer).")
    report$sections[[1L]] <- .new_sect()
  }
  if (length(report$pages) == 0L) {
    stop("rtfreport must contain at least one page.", call. = FALSE)
  }
  for (i in seq_along(report$pages)) {
    ct <- report$pages[[i]]$content
    if (!is.null(ct) && !inherits(ct, "rtftable") && !inherits(ct, "rtfplot")) {
      stop(sprintf("Page %d content must be rtftable, rtfplot, or NULL.", i),
           call. = FALSE)
    }
  }
  report
}


#' Print an rtfreport object
#'
#' Prints a compact summary of the internal `rtfreport` render product: its
#' page and section counts, the default page geometry (paper, orientation and
#' size in inches), and the font / colour table sizes.
#'
#' Most users never build an `rtfreport` directly: assemble documents with
#' [rtf_document()] and the pipe verbs (that object has its own `print` method),
#' then hand them to [generate_rtfreport()]. An `rtfreport` is the lower-level
#' render product that `generate_rtfreport()` also accepts, and this method lets
#' you inspect one if you hold it.
#'
#' @param x An `rtfreport` object.
#' @param ... Additional arguments (unused).
#'
#' @return `x`, invisibly. Called for the side effect of printing the summary.
#'
#' @examples
#' df  <- data.frame(Parameter = c("Age", "Sex"),
#'                   Value = c("54.2 (11.3)", "31 (51.7%)"),
#'                   stringsAsFactors = FALSE)
#' doc <- rtf_document() |>
#'   rtf_tables(rtftable(df, border = "tfl"),
#'              titles = list("Table 14.1.1"))
#'
#' # generate_rtfreport() returns the rendered report invisibly, so capture it
#' # to inspect what was written: how many pages, sections, titles.
#' f   <- tempfile(fileext = ".rtf")
#' rep <- generate_rtfreport(doc, f, overwrite = TRUE)
#' print(rep)
#' unlink(f)
#'
#' @export
print.rtfreport <- function(x, ...) {
  np <- length(x$pages)
  ns <- length(x$sections)
  cat(sprintf("<rtfreport> %d page%s, %d section%s\n",
              np, if (np == 1L) "" else "s",
              ns, if (ns == 1L) "" else "s"))

  pg <- x$document$default_page
  if (!is.null(pg)) {
    in_of <- function(t) if (is.null(t)) NA_real_ else t / 1440
    cat(sprintf("  Page:    %s %s, %.2f x %.2f in\n",
                pg$paper %||% "?", pg$orientation %||% "?",
                in_of(pg$width_twips), in_of(pg$height_twips)))
  }

  nf  <- length(x$document$font_table)
  ncl <- length(x$document$color_table)
  fn1 <- tryCatch(x$document$font_table[[1L]]$name, error = function(e) NULL)
  cat(sprintf("  Fonts:   %d%s   Colors: %d\n",
              nf, if (!is.null(fn1)) paste0(" (", fn1, ")") else "", ncl))
  invisible(x)
}

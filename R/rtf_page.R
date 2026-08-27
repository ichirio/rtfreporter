# ============================================================================
#  rtf_page() / rtf_default_format() -- structured settings as S3 objects
# ============================================================================
#
#  These constructors turn the two structured `rtf_document()` parameters into
#  first-class objects whose *defaults are visible in their own Usage*, instead
#  of an opaque named list.  They are the recommended way to set page geometry
#  and document-wide formatting:
#
#      rtf_document(page = rtf_page(paper_size = "A4", orientation = "portrait"))
#
#  Site defaults (issue #111) still work: an argument the caller did NOT pass
#  falls back to the corresponding `rtfreporter.*` option (else the factory
#  baseline shown in the signature).  So the resolution order is unchanged --
#  explicit argument > `rtfreporter.*` option > factory default.
# ============================================================================

# Validate a single positive-inches value (or NULL).
.chk_pos_in <- function(x, nm) {
  if (!is.null(x) && (!is.numeric(x) || length(x) != 1L || is.na(x) || x <= 0)) {
    stop(sprintf("`%s` must be a single positive number of inches, or NULL.", nm),
         call. = FALSE)
  }
  invisible(x)
}

#' Page geometry for an RTF document
#'
#' Builds the `page` setting for [rtf_document()] / [rtf_config()] as a
#' structured `rtf_page` object, so the available options and their **defaults**
#' are visible right here in the signature (rather than buried in a named list).
#'
#' The default is **landscape Letter** with 0.9" top/bottom and 0.6" left/right
#' margins. A *site* can change any default by setting the matching
#' `rtfreporter.*` option (e.g. in `Rprofile.site`): an argument you do not pass
#' falls back to that option, so the resolution order is **explicit argument >
#' `rtfreporter.*` option > the factory default shown below** (see
#' [rtfreporter_options()]).
#'
#' @param paper_size A named preset (case-insensitive): `"letter"` (8.5x11"),
#'   `"legal"` (8.5x14"), `"A4"` (210x297mm), `"A3"`, or `"A5"`.
#' @param orientation `"landscape"` or `"portrait"`.
#' @param width_in,height_in Explicit page size in inches. When supplied these
#'   **win** over `paper_size`, and the orientation is *inferred* from them
#'   (`width_in >= height_in` means landscape). `NULL` (default) uses
#'   `paper_size`.
#' @param margin_top_in,margin_bottom_in,margin_left_in,margin_right_in The four
#'   page margins, in inches.
#' @param header_dist_in,footer_dist_in Distance (inches) of the header / footer
#'   band from the page edge. `NULL` (default) uses the **full** corresponding
#'   top / bottom margin (so the band sits at the margin boundary).
#'
#' @return An `rtf_page` object (a classed named list) for `rtf_document(page =)`.
#'
#' @seealso [rtf_document()], [rtf_default_format()], [rtfreporter_options()].
#'
#' @examples
#' # Defaults made explicit (landscape Letter):
#' rtf_page()
#'
#' # Wide TFL: narrow the left/right margins from the 0.75" default to 0.5"
#' # (still clears the eCTD 0.375" floor) to gain an inch of table width.
#' rtf_page(margin_left_in = 0.5, margin_right_in = 0.5)
#'
#' # A4 portrait with tighter margins:
#' rtf_page(paper_size = "A4", orientation = "portrait",
#'          margin_left_in = 0.75, margin_right_in = 0.75)
#'
#' # Custom dimensions (orientation inferred -> portrait):
#' rtf_page(width_in = 8.5, height_in = 14)
#'
#' doc <- rtf_document(page = rtf_page(paper_size = "A4", orientation = "portrait"))
#'
#' @export
rtf_page <- function(paper_size       = "letter",
                     orientation      = "landscape",
                     width_in         = NULL,
                     height_in        = NULL,
                     margin_top_in    = 0.75,
                     margin_bottom_in = 0.75,
                     margin_left_in   = 0.75,
                     margin_right_in  = 0.75,
                     header_dist_in   = NULL,
                     footer_dist_in   = NULL) {
  # Site defaults (#111): only an argument the caller did NOT pass falls back to
  # the rtfreporter.* option (the literal in the signature is the factory value).
  if (missing(paper_size))       paper_size       <- .opt("rtfreporter.page.paper_size")
  if (missing(orientation))      orientation      <- .opt("rtfreporter.page.orientation")
  if (missing(margin_top_in))    margin_top_in    <- .opt("rtfreporter.page.margin_top_in")
  if (missing(margin_bottom_in)) margin_bottom_in <- .opt("rtfreporter.page.margin_bottom_in")
  if (missing(margin_left_in))   margin_left_in   <- .opt("rtfreporter.page.margin_left_in")
  if (missing(margin_right_in))  margin_right_in  <- .opt("rtfreporter.page.margin_right_in")

  if (!is.null(paper_size) &&
      (!is.character(paper_size) || length(paper_size) != 1L)) {
    stop("`paper_size` must be a single preset name (e.g. \"A4\") or NULL.",
         call. = FALSE)
  }
  if (!is.null(orientation) &&
      !(is.character(orientation) && length(orientation) == 1L &&
        orientation %in% c("landscape", "portrait"))) {
    stop("`orientation` must be \"landscape\" or \"portrait\".", call. = FALSE)
  }
  .chk_pos_in(width_in, "width_in");         .chk_pos_in(height_in, "height_in")
  .chk_pos_in(margin_top_in, "margin_top_in")
  .chk_pos_in(margin_bottom_in, "margin_bottom_in")
  .chk_pos_in(margin_left_in, "margin_left_in")
  .chk_pos_in(margin_right_in, "margin_right_in")
  .chk_pos_in(header_dist_in, "header_dist_in")
  .chk_pos_in(footer_dist_in, "footer_dist_in")

  structure(
    list(paper_size = paper_size, orientation = orientation,
         width_in = width_in, height_in = height_in,
         margin_top_in = margin_top_in, margin_bottom_in = margin_bottom_in,
         margin_left_in = margin_left_in, margin_right_in = margin_right_in,
         header_dist_in = header_dist_in, footer_dist_in = footer_dist_in),
    class = "rtf_page"
  )
}

#' @export
print.rtf_page <- function(x, ...) {
  size <- if (!is.null(x$width_in) || !is.null(x$height_in)) {
    sprintf("%s x %s in", x$width_in %||% "?", x$height_in %||% "?")
  } else {
    sprintf("%s (%s)", x$paper_size %||% "letter", x$orientation %||% "landscape")
  }
  cat("<rtf_page>", size, "\n")
  cat(sprintf("  margins (in): top %s, bottom %s, left %s, right %s\n",
              x$margin_top_in, x$margin_bottom_in,
              x$margin_left_in, x$margin_right_in))
  invisible(x)
}

#' Document-wide default formatting for an RTF document
#'
#' Builds the `default_format` setting for [rtf_document()] / [rtf_config()] as a
#' structured `rtf_default_format` object, so its options and **defaults** are
#' visible in the signature. Every value is a *default*: a per-module setting on
#' [rtftable()] / [rtf_header()] / [rtf_footer()] / [rtf_table_style()] always
#' overrides it, and a site can change a default via the matching `rtfreporter.*`
#' option (an unset argument falls back to it; see [rtfreporter_options()]).
#'
#' @param font_size_half_points Body font size in half-points (`18` = 9 pt).
#' @param row_height_twips Default row height (twips) for every table-shaped
#'   element (content table, page header / footer, title / footnote). `NULL`
#'   (default) keeps the font-aware baseline.
#' @param cell_padding_left_twips,cell_padding_right_twips Default cell padding
#'   (twips, border-to-text). `NULL` (default) keeps the resource baseline (0).
#' @param markup Cell-text markup: `"script"` (`^{}`/`_{}` super/subscript, the
#'   default), `"relational"` (`>=`/`<=` to the symbols), `"all"`, or `"none"`.
#'   See [rtftable()].
#' @param title_format How the page **title** renders: `"text"` (default, plain
#'   centred paragraphs) or `"table"` (a content-width single-column table).
#' @param footnote_format How the **footnote** renders: `"table"` (default,
#'   content-width table with the separator rule) or `"text"` (plain paragraphs).
#' @param title_width,footnote_width Width of the title / footnote block:
#'   `"content"` (the default -- follow the table body), `"page"` (the writable
#'   width, margins excluded), a fraction in `(0, 1]` of the writable width, or
#'   twips. `NULL` keeps the default. Applies to the `"table"` form; the
#'   `"text"` form is plain paragraphs and already spans the page.
#'   `footnote_width = "page"` is the way to reach the full width **and** keep
#'   the separator rule, which `footnote_format = "text"` cannot draw.
#'
#' @return An `rtf_default_format` object for `rtf_document(default_format =)`.
#'
#' @seealso [rtf_document()], [rtf_page()], [rtftable()].
#'
#' @examples
#' # Defaults made explicit:
#' rtf_default_format()
#'
#' # 10 pt, a fixed row height, and the >= / <= symbol conversion on:
#' rtf_default_format(font_size_half_points = 20L, row_height_twips = 240L,
#'                    markup = "all")
#'
#' doc <- rtf_document(default_format = rtf_default_format(font_size_half_points = 20L))
#'
#' @export
rtf_default_format <- function(font_size_half_points    = 18L,
                               row_height_twips         = NULL,
                               cell_padding_left_twips  = NULL,
                               cell_padding_right_twips = NULL,
                               markup                   = "script",
                               title_format             = "text",
                               footnote_format          = "table",
                               title_width              = NULL,
                               footnote_width           = NULL) {
  if (missing(font_size_half_points))
    font_size_half_points <- .opt("rtfreporter.font_size_half_points")
  if (missing(markup))          markup          <- .opt("rtfreporter.markup")
  if (missing(title_format))    title_format    <- .opt("rtfreporter.title_format")
  if (missing(footnote_format)) footnote_format <- .opt("rtfreporter.footnote_format")
  if (missing(title_width))     title_width     <- .opt("rtfreporter.title_width")
  if (missing(footnote_width))  footnote_width  <- .opt("rtfreporter.footnote_width")

  .chk_pos_twips <- function(x, nm) {
    if (!is.null(x) && (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0)) {
      stop(sprintf("`%s` must be a single non-negative integer (twips) or NULL.",
                   nm), call. = FALSE)
    }
  }
  if (!is.numeric(font_size_half_points) || length(font_size_half_points) != 1L ||
      is.na(font_size_half_points) || font_size_half_points <= 0) {
    stop("`font_size_half_points` must be a single positive integer.", call. = FALSE)
  }
  .chk_pos_twips(row_height_twips, "row_height_twips")
  .chk_pos_twips(cell_padding_left_twips, "cell_padding_left_twips")
  .chk_pos_twips(cell_padding_right_twips, "cell_padding_right_twips")
  markup <- .resolve_markup(markup)            # validates the tokens
  title_format    <- .resolve_text_block_format(title_format, "text")
  footnote_format <- .resolve_text_block_format(footnote_format, "table")
  title_width     <- .check_block_width(title_width, "title_width")
  footnote_width  <- .check_block_width(footnote_width, "footnote_width")

  structure(
    list(font_size_half_points = as.integer(font_size_half_points),
         row_height_twips = if (is.null(row_height_twips)) NULL else as.integer(row_height_twips),
         cell_padding_left_twips = if (is.null(cell_padding_left_twips)) NULL else as.integer(cell_padding_left_twips),
         cell_padding_right_twips = if (is.null(cell_padding_right_twips)) NULL else as.integer(cell_padding_right_twips),
         markup = markup, title_format = title_format,
         footnote_format = footnote_format,
         title_width = title_width, footnote_width = footnote_width),
    class = "rtf_default_format"
  )
}

#' @export
print.rtf_default_format <- function(x, ...) {
  cat("<rtf_default_format>\n")
  cat(sprintf("  font %s half-points; row height %s; padding L/R %s/%s twips\n",
              x$font_size_half_points, x$row_height_twips %||% "auto",
              x$cell_padding_left_twips %||% "0", x$cell_padding_right_twips %||% "0"))
  cat(sprintf("  markup [%s]; title_format \"%s\"; footnote_format \"%s\"\n",
              paste(x$markup, collapse = ", "), x$title_format, x$footnote_format))
  invisible(x)
}

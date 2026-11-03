# ============================================================================
#  rtf_page() -- structured page settings as an S3 object
# ============================================================================
#
#  This constructor turns the structured `rtf_document(page = )` parameter into
#  a first-class object whose *defaults are visible in its own Usage*, instead
#  of an opaque named list.  It is the recommended way to set page geometry
#  (document-wide formatting is rtf_style()):
#
#      rtf_document(page = rtf_page(paper_size = "A4", orientation = "portrait"))
#
#  Site defaults (issue #111) still work: an argument the caller did NOT pass
#  falls back to the corresponding `rtfreporter.*` option (else the factory
#  baseline shown in the signature).  So the resolution order is unchanged --
#  explicit argument > `rtfreporter.*` option > factory default.
# ============================================================================

# Validate a single positive-inches value (or NULL).  Inf used to pass and
# be written as NA twips.
.chk_pos_in <- function(x, nm) {
  if (!is.null(x) && (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
                      x <= 0)) {
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
#' The default is **landscape Letter** with 0.75" margins on all four sides. A *site* can change any default by setting the matching
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
#'   `paper_size`. With dimensions given, `paper_size` and `orientation` stay
#'   unset unless you pass them, so only a value you passed is warned about.
#' @param margin_top_in,margin_bottom_in,margin_left_in,margin_right_in The four
#'   page margins, in inches.  Each inch value here is one positive finite
#'   number; another (`0`, `Inf`, `NA`, a string) is an error.  The margins
#'   must leave a writable area: when the document is written, left + right
#'   at least the page width, or top + bottom at least the page height, is an
#'   error (checked then, because a margin can also come from an
#'   `rtfreporter.page.*` option).
#' @param header_dist_in,footer_dist_in Distance (inches) of the header / footer
#'   band from the page edge. `NULL` (default) uses the **full** corresponding
#'   top / bottom margin (so the band sits at the margin boundary).  A
#'   distance of at least the page height is an error when the document is
#'   written.
#'
#' @return An `rtf_page` object (a classed named list) for `rtf_document(page =)`.
#'
#' @seealso [rtf_document()], [rtf_style()], [rtfreporter_options()].
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
  # With explicit dimensions an unpassed paper_size / orientation stays unset:
  # the dimensions decide, and only a value the caller gave is warned about.
  has_dims <- !is.null(width_in) || !is.null(height_in)
  if (missing(paper_size))
    paper_size  <- if (has_dims) NULL else .opt("rtfreporter.page.paper_size")
  if (missing(orientation))
    orientation <- if (has_dims) NULL else .opt("rtfreporter.page.orientation")
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

# ============================================================================
#  rtf_watermark() -- a diagonal watermark behind the page body
# ============================================================================
#
#  Mechanism
#  ---------
#  The shape is emitted inside the section's `{\header ...}` group.  That is
#  what Word's own Insert > Watermark writes, and it is the portable choice:
#  a header repeats on every page of its section, and it stays scoped to that
#  section, so `assemble_rtf()` cannot let one deliverable's watermark bleed
#  into the next.  The document-level `{\*\background}` alternative is neither
#  per-section nor widely honoured.
#
#  A word on the shape properties
#  ------------------------------
#  shapeType 136 is the plain-text WordArt shape; `fGtext 1` turns the text
#  into WordArt so it can be rotated and scaled as a unit.  `rotation` is a
#  fixed-point number -- degrees << 16.  `shpfblwtxt1` puts the shape behind
#  the text, which is the whole point.  `shpbxpage` / `shpbypage` anchor the
#  offsets to the page rather than the column, so the shape lands in the same
#  place whatever the header contains.
# ============================================================================


#' Build a page watermark
#'
#' Builds the `watermark` setting for [rtf_document()] -- a diagonal word such
#' as `"DRAFT"` or `"CONFIDENTIAL"` drawn behind the page body, on every page.
#'
#' The watermark is written into the section header, so it repeats per page and
#' stays scoped to its section: concatenating with [assemble_rtf()] cannot leak
#' one document's watermark into the next.  A section can override the
#' document-wide setting by passing `watermark` in its
#' `rtf_section(secinfo = )`, including `watermark = NA` to turn it off for
#' that section alone.
#'
#' @param text The watermark word. A zero-length or empty string means no
#'   watermark.
#' @param font_size_half_points Size in half-points, as everywhere else in the
#'   package (`144L` = 72 pt, the default).
#' @param color Fill colour as `"#RRGGBB"`. The default is a light grey that
#'   stays readable behind body text.
#' @param angle Rotation in degrees, counter-clockwise negative. `-45` (the
#'   default) is the conventional bottom-left to top-right diagonal.
#' @param font Font family. `NULL` (default) uses the document font.
#' @param width_in,height_in Size of the shape's bounding box in inches. The
#'   defaults suit a single word on a Letter/A4 page; a long phrase needs a
#'   wider box.
#'
#' @return An `rtf_watermark` object for `rtf_document(watermark = )`.
#'
#' @seealso [rtf_document()], [rtf_section()], [rtf_page()].
#'
#' @examples
#' rtf_watermark("DRAFT")
#'
#' # Bigger, redder, horizontal:
#' rtf_watermark("DO NOT DISTRIBUTE", font_size_half_points = 96L,
#'               color = "#E8B4B4", angle = 0, width_in = 7)
#'
#' @export
rtf_watermark <- function(text,
                          font_size_half_points = 144L,
                          color                 = "#C8C8C8",
                          angle                 = -45,
                          font                  = NULL,
                          width_in              = 6,
                          height_in             = 2) {
  if (missing(text) || is.null(text) || length(text) == 0L) {
    stop("`text` is required; give the word to draw, e.g. \"DRAFT\".",
         call. = FALSE)
  }
  text <- as.character(text)[1L]
  if (is.na(text)) text <- ""

  if (!is.numeric(font_size_half_points) || length(font_size_half_points) != 1L ||
      is.na(font_size_half_points) || font_size_half_points <= 0) {
    stop("`font_size_half_points` must be one positive number.", call. = FALSE)
  }
  if (!is.character(color) || length(color) != 1L || is.na(color) ||
      !grepl("^#[0-9A-Fa-f]{6}$", color)) {
    stop("`color` must be one \"#RRGGBB\" string; got '", color, "'.",
         call. = FALSE)
  }
  if (!is.numeric(angle) || length(angle) != 1L || is.na(angle)) {
    stop("`angle` must be one number, in degrees.", call. = FALSE)
  }
  for (nm in c("width_in", "height_in")) {
    v <- get(nm)
    if (!is.numeric(v) || length(v) != 1L || is.na(v) || v <= 0) {
      stop("`", nm, "` must be one positive number, in inches.", call. = FALSE)
    }
  }
  if (!is.null(font) && (!is.character(font) || length(font) != 1L)) {
    stop("`font` must be NULL or one font-family name.", call. = FALSE)
  }

  structure(
    list(
      text                  = text,
      font_size_half_points = as.integer(font_size_half_points),
      color                 = color,
      angle                 = as.numeric(angle),
      font                  = font,
      width_in              = as.numeric(width_in),
      height_in             = as.numeric(height_in)
    ),
    class = "rtf_watermark"
  )
}

#' @export
print.rtf_watermark <- function(x, ...) {
  cat("<rtf_watermark>\n")
  cat("  text  : ", x$text, "\n", sep = "")
  cat("  size  : ", x$font_size_half_points / 2, " pt\n", sep = "")
  cat("  color : ", x$color, "\n", sep = "")
  cat("  angle : ", x$angle, " deg\n", sep = "")
  cat("  box   : ", x$width_in, " x ", x$height_in, " in\n", sep = "")
  if (!is.null(x$font)) cat("  font  : ", x$font, "\n", sep = "")
  invisible(x)
}


# -- Internal -----------------------------------------------------------------

# Accept what a user may plausibly write: an rtf_watermark, a bare string
# (the common case -- `watermark = "DRAFT"`), or NULL / NA for "none".
.normalize_watermark <- function(x) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "rtf_watermark")) {
    return(if (nzchar(x$text)) x else NULL)
  }
  if (length(x) == 1L && is.na(x)) return(NULL)
  if (is.character(x) && length(x) == 1L) {
    return(if (nzchar(x)) rtf_watermark(x) else NULL)
  }
  stop("`watermark` must be an rtf_watermark object, a single string, ",
       "or NULL / NA for none.", call. = FALSE)
}

# "#RRGGBB" -> the BGR integer an Office shape property wants.
.watermark_bgr <- function(hex) {
  r <- strtoi(substr(hex, 2L, 3L), 16L)
  g <- strtoi(substr(hex, 4L, 5L), 16L)
  b <- strtoi(substr(hex, 6L, 7L), 16L)
  as.integer(b * 65536L + g * 256L + r)
}

# The `{\shp ...}` group, centred on a page `page_w_twips` x `page_h_twips`.
# Returns "" when there is nothing to draw, so callers can paste0() blindly.
.render_watermark_rtf <- function(wm, page_w_twips, page_h_twips,
                                  default_font = "Times New Roman") {
  wm <- .normalize_watermark(wm)
  if (is.null(wm)) return("")

  w  <- as.integer(round(wm$width_in  * 1440))
  h  <- as.integer(round(wm$height_in * 1440))
  # Centre the box on the page.  Offsets are page-anchored (shpbxpage /
  # shpbypage), so they do not move with the header's own content.
  left <- as.integer(round((page_w_twips - w) / 2))
  top  <- as.integer(round((page_h_twips - h) / 2))

  sp <- function(name, value) {
    sprintf("{\\sp{\\sn %s}{\\sv %s}}", name, value)
  }

  paste0(
    "{\\shp{\\*\\shpinst",
    "\\shpleft", left, "\\shptop", top,
    "\\shpright", left + w, "\\shpbottom", top + h,
    "\\shpfhdr1\\shpbxpage\\shpbypage\\shpwr3\\shpwrk0\\shpfblwtxt1\\shpz0",
    sp("shapeType", 136L),
    sp("fFilled", 1L),
    sp("fillColor", .watermark_bgr(wm$color)),
    sp("fLine", 0L),
    # Behind the text.  `\shpfblwtxt1` says it at the destination level; the
    # shape property is what Word itself writes and what readers that ignore
    # the destination flag (LibreOffice among them) actually honour.
    sp("fBehindDocument", 1L),
    # Position relative to the page, matching the page-anchored offsets above.
    sp("posrelh", 1L),
    sp("posrelv", 1L),
    sp("rotation", format(as.integer(round(wm$angle * 65536)), scientific = FALSE)),
    sp("fGtext", 1L),
    sp("gtextUNICODE", .toc_escape(wm$text)),
    sp("gtextFont", wm$font %||% default_font),
    sp("gtextSize", wm$font_size_half_points * 10L),
    sp("fHidden", 0L),
    "}}"
  )
}

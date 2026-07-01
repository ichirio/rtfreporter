# rtfplot: figure object for embedding PNG/JPEG images into RTF reports.
#
# Reads the image file, extracts dimensions, and stores the binary data
# location plus metadata.  The renderer (.render_rtfplot) reads the file and
# emits a \pict RTF command.

# Read PNG image dimensions from the IHDR chunk (bytes 17-24).
.read_png_dims <- function(path) {
  raw <- readBin(path, "raw", n = 24L)
  if (length(raw) < 24L) stop("File too short to be a valid PNG.", call. = FALSE)
  # PNG signature: bytes 1-8.  IHDR chunk: bytes 9-12 (length), 13-16 (type),
  # 17-20 (width), 21-24 (height) -- all big-endian.
  to_int <- function(b) sum(as.integer(b) * c(16777216L, 65536L, 256L, 1L))
  list(width = to_int(raw[17:20]), height = to_int(raw[21:24]))
}

# Read JPEG image dimensions by scanning for the SOF marker.
.read_jpeg_dims <- function(path) {
  raw <- readBin(path, "raw", n = file.info(path)$size)
  n   <- length(raw)
  i   <- 3L  # Skip the 2-byte SOI marker (FF D8).
  while (i <= n - 4L) {
    if (raw[i] == as.raw(0xFF)) {
      marker <- raw[i + 1L]
      # SOF0/SOF1/SOF2 markers carry frame dimensions.
      if (marker %in% as.raw(c(0xC0, 0xC1, 0xC2))) {
        height <- as.integer(raw[i + 5L]) * 256L + as.integer(raw[i + 6L])
        width  <- as.integer(raw[i + 7L]) * 256L + as.integer(raw[i + 8L])
        return(list(width = width, height = height))
      }
      # Skip past this marker segment.
      seg_len <- as.integer(raw[i + 2L]) * 256L + as.integer(raw[i + 3L])
      i <- i + seg_len + 2L
    } else {
      i <- i + 1L
    }
  }
  stop("Could not locate SOF marker to read JPEG dimensions.", call. = FALSE)
}

# Read the pixel density (DPI) of a PNG from its `pHYs` chunk, if present.
# Returns list(dpi_x, dpi_y) when the chunk records dots-per-metre (unit = 1),
# else NULL. The `pHYs` chunk always precedes the first `IDAT`, so the scan
# stops there. CRCs are not validated (we only read structure).
.read_png_density <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  readBin(con, "raw", 8L)                       # skip the PNG signature
  to_uint <- function(b) sum(as.numeric(b) * c(16777216, 65536, 256, 1))
  repeat {
    len_raw <- readBin(con, "raw", 4L)
    if (length(len_raw) < 4L) break
    len  <- to_uint(len_raw)
    type <- rawToChar(readBin(con, "raw", 4L))
    if (identical(type, "pHYs")) {
      d <- readBin(con, "raw", 9L)
      if (length(d) < 9L) return(NULL)
      ppux <- to_uint(d[1:4]); ppuy <- to_uint(d[5:8]); unit <- as.integer(d[9])
      if (unit == 1L && ppux > 0 && ppuy > 0) {
        return(list(dpi_x = ppux * 0.0254, dpi_y = ppuy * 0.0254))
      }
      return(NULL)
    }
    if (identical(type, "IDAT") || identical(type, "IEND")) break
    readBin(con, "raw", len + 4L)               # skip chunk data + CRC
  }
  NULL
}

# Read the pixel density (DPI) of a JPEG from its JFIF `APP0` segment, if
# present. Returns list(dpi_x, dpi_y) when a real density is recorded
# (units = 1 -> DPI, units = 2 -> dots-per-cm), else NULL.
.read_jpeg_density <- function(path) {
  raw <- readBin(path, "raw", n = file.info(path)$size)
  n   <- length(raw)
  i   <- 3L
  while (i <= n - 4L) {
    if (raw[i] != as.raw(0xFF)) { i <- i + 1L; next }
    marker <- raw[i + 1L]
    if (marker == as.raw(0xE0) && i + 15L <= n &&
        identical(rawToChar(raw[(i + 4L):(i + 7L)]), "JFIF")) {
      units <- as.integer(raw[i + 11L])
      xd    <- as.integer(raw[i + 12L]) * 256L + as.integer(raw[i + 13L])
      yd    <- as.integer(raw[i + 14L]) * 256L + as.integer(raw[i + 15L])
      if (units == 1L && xd > 0 && yd > 0) return(list(dpi_x = xd,        dpi_y = yd))
      if (units == 2L && xd > 0 && yd > 0) return(list(dpi_x = xd * 2.54, dpi_y = yd * 2.54))
      return(NULL)
    }
    # Standalone markers (SOI/EOI/RSTn/TEM) carry no length; others do.
    if (marker %in% as.raw(c(0xD8, 0xD9, 0x01)) ||
        (marker >= as.raw(0xD0) && marker <= as.raw(0xD7))) {
      i <- i + 2L
    } else {
      seg_len <- as.integer(raw[i + 2L]) * 256L + as.integer(raw[i + 3L])
      i <- i + 2L + seg_len
    }
  }
  NULL
}

# Resolve the display size (twips) of an rtfplot, honouring explicit
# `width_twips` / `height_twips` and otherwise defaulting to the image's native
# size at its DPI (100% scale). A missing DPI falls back to the
# `rtfreporter.figure.default_dpi` option. Setting exactly one of width/height
# derives the other from the native aspect ratio. Shared by the renderer, the
# title/footnote width helpers and `print()` so they all agree.
.rtfplot_display_twips <- function(x) {
  fallback <- as.numeric(.opt("rtfreporter.figure.default_dpi") %||% 96)
  dpi_x <- x$dpi_x; if (is.null(dpi_x) || is.na(dpi_x) || dpi_x <= 0) dpi_x <- fallback
  dpi_y <- x$dpi_y; if (is.null(dpi_y) || is.na(dpi_y) || dpi_y <= 0) dpi_y <- fallback
  native_w <- as.integer(round(x$img_width  / dpi_x * 1440))
  native_h <- as.integer(round(x$img_height / dpi_y * 1440))
  uw <- x$width_twips; uh <- x$height_twips
  w <- if (!is.null(uw)) uw
       else if (!is.null(uh)) as.integer(round(uh * x$img_width / x$img_height))
       else native_w
  h <- if (!is.null(uh)) uh
       else if (!is.null(uw)) as.integer(round(uw * x$img_height / x$img_width))
       else native_h
  list(w = w, h = h, dpi_x = dpi_x, dpi_y = dpi_y,
       native_w = native_w, native_h = native_h)
}

#' Create an RTF figure object
#'
#' Embeds a PNG or JPEG image into the RTF output.
#' The result can be passed directly to `rtf_tables()` in a pipe chain.
#'
#' @param path Path to a PNG or JPEG image file.
#' @param width_twips Display width in twips. `NULL` (default) uses the image's
#'   **native size at its embedded DPI** (100% scale). If only `height_twips` is
#'   given, the width is derived from the native aspect ratio.
#' @param height_twips Display height in twips. `NULL` (default) uses the native
#'   size at the image's DPI, or -- when `width_twips` is given -- the height
#'   derived from the native aspect ratio.
#' @param align Horizontal alignment: `"center"` (default), `"left"`, or `"right"`.
#'
#' @details
#' The native size is read from the file's resolution metadata (PNG `pHYs`
#' chunk, JPEG JFIF density). A 2500 x 1438 px image saved at 300 DPI therefore
#' embeds at 2500/300 x 1438/300 in. When the file carries no DPI, the
#' `rtfreporter.figure.default_dpi` option (factory `96`) is assumed. There is no
#' automatic page-fit cap; give an explicit `width_twips` to shrink a figure that
#' is wider than the page.
#'
#' @return An `rtfplot` (S3) object suitable for use in `rtf_tables()`.
#'
#' @examples
#' \dontrun{
#' fig <- rtfplot("scatter.png", width_twips = 9000L)
#'
#' doc <- rtf_document() %>%
#'   rtf_section(page = 1, secinfo = list(
#'     header = rtf_header(rows = list(c(l = "Figure 14.1")))
#'   )) %>%
#'   rtf_tables(list(fig))
#'
#' generate_rtfreport(doc, "output.rtf", overwrite = TRUE)
#' }
#'
#' @export
rtfplot <- function(path, width_twips = NULL, height_twips = NULL,
                    align = "center") {
  if (!file.exists(path)) {
    stop(sprintf("Image file not found: %s", path), call. = FALSE)
  }
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("png", "jpg", "jpeg")) {
    stop("rtfplot supports PNG and JPEG files only.", call. = FALSE)
  }
  img_type <- if (ext == "png") "png" else "jpeg"

  dims <- if (img_type == "png") .read_png_dims(path) else .read_jpeg_dims(path)
  density <- if (img_type == "png") .read_png_density(path) else .read_jpeg_density(path)

  if (!align %in% c("left", "center", "right")) {
    stop("`align` must be 'left', 'center', or 'right'.", call. = FALSE)
  }

  structure(
    list(
      path         = path,
      width_twips  = if (!is.null(width_twips))  as.integer(width_twips)  else NULL,
      height_twips = if (!is.null(height_twips)) as.integer(height_twips) else NULL,
      align        = align,
      img_width    = dims$width,
      img_height   = dims$height,
      img_type     = img_type,
      dpi_x        = density$dpi_x,
      dpi_y        = density$dpi_y
    ),
    class = "rtfplot"
  )
}


#' Print an rtfplot object
#'
#' Prints a compact summary of an [rtfplot()] figure: the image type and file,
#' the image's native pixel size and DPI, the display size that will be embedded
#' (in inches and twips), and the alignment.
#'
#' @param x An `rtfplot` object.
#' @param ... Additional arguments (unused).
#'
#' @return `x`, invisibly. Called for the side effect of printing the summary.
#'
#' @examples
#' \dontrun{
#' print(rtfplot("scatter.png", width_twips = 9000L))
#' }
#'
#' @export
print.rtfplot <- function(x, ...) {
  cat(sprintf("<rtfplot> %s: %s\n", toupper(x$img_type), basename(x$path)))
  disp <- .rtfplot_display_twips(x)
  dpi_known <- !is.null(x$dpi_x) && !is.na(x$dpi_x)
  dpi_str <- if (dpi_known) sprintf("%g x %g dpi", round(x$dpi_x), round(x$dpi_y))
             else sprintf("dpi unknown, assuming %g", disp$dpi_x)
  cat(sprintf("  Native size:  %d x %d px  (%s)\n",
              x$img_width, x$img_height, dpi_str))
  twips_in <- function(t) sprintf("%.2f in (%d twips)", t / 1440, t)
  cat(sprintf("  Display:      %s wide x %s\n", twips_in(disp$w), twips_in(disp$h)))
  cat(sprintf("  Align:        %s\n", x$align))
  invisible(x)
}

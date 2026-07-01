# rtfplot.R -- figure object for embedding PNG / JPEG into RTF.

# Helper: write a 1x1 valid PNG to a tempfile and return its path.
# We hand-write the bytes (74-byte minimal RGB PNG) so the test works
# in any environment — no graphics device required.
.tmp_png <- function() {
  bytes <- as.raw(c(
    # PNG signature
    0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A,
    # IHDR chunk (length, type, width=1, height=1, bit-depth=8, color=2 RGB,
    # compression=0, filter=0, interlace=0, CRC)
    0x00, 0x00, 0x00, 0x0D,
    0x49, 0x48, 0x44, 0x52,
    0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01,
    0x08, 0x02, 0x00, 0x00, 0x00,
    0x90, 0x77, 0x53, 0xDE,
    # IDAT chunk (length 12, type, deflated single white pixel, CRC)
    0x00, 0x00, 0x00, 0x0C,
    0x49, 0x44, 0x41, 0x54,
    0x08, 0x99, 0x63, 0xF8, 0xFF, 0xFF, 0xFF, 0x3F,
    0x00, 0x05, 0xFE, 0x02, 0xFE, 0xDC, 0xCC, 0x59,
    0xE7,
    # IEND chunk
    0x00, 0x00, 0x00, 0x00,
    0x49, 0x45, 0x4E, 0x44,
    0xAE, 0x42, 0x60, 0x82
  ))
  f <- tempfile(fileext = ".png")
  writeBin(bytes, f)
  f
}

# ──────── Construction ────────────────────────────────────────────────────

test_that("rtfplot() constructs an S3 object from a PNG path", {
  f   <- .tmp_png(); on.exit(unlink(f), add = TRUE)
  fig <- rtfplot(f)
  expect_s3_class(fig, "rtfplot")
  expect_identical(fig$path, f)
  expect_identical(fig$img_type, "png")
  expect_gt(fig$img_width,  0L)
  expect_gt(fig$img_height, 0L)
  expect_identical(fig$align, "center")     # default
})

test_that("rtfplot(width_twips, height_twips, align) propagate", {
  f <- .tmp_png(); on.exit(unlink(f), add = TRUE)
  fig <- rtfplot(f, width_twips = 7200L, height_twips = 3600L,
                  align = "left")
  expect_identical(fig$width_twips,  7200L)
  expect_identical(fig$height_twips, 3600L)
  expect_identical(fig$align,        "left")
})

test_that("rtfplot() rejects non-existent files", {
  expect_error(rtfplot("/nope/does/not/exist.png"), "not found")
})

test_that("rtfplot() rejects unsupported extensions", {
  f <- tempfile(fileext = ".bmp"); file.create(f)
  on.exit(unlink(f), add = TRUE)
  expect_error(rtfplot(f), "PNG and JPEG")
})

test_that("rtfplot() rejects invalid align", {
  f <- .tmp_png(); on.exit(unlink(f), add = TRUE)
  expect_error(rtfplot(f, align = "justify"), "align")
})

# ──────── DPI / native-size default ───────────────────────────────────────

# 600x300 PNG carrying a pHYs chunk of 11811 ppm (~300 dpi).
.tmp_png_dpi <- function() {
  bytes <- as.raw(c(
    0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A,
    # IHDR: width=600 (0x0258), height=300 (0x012C)
    0x00, 0x00, 0x00, 0x0D, 0x49, 0x48, 0x44, 0x52,
    0x00, 0x00, 0x02, 0x58, 0x00, 0x00, 0x01, 0x2C,
    0x08, 0x02, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00,                      # dummy CRC (not validated)
    # pHYs: 11811 x 11811 ppm, unit = 1 (metre)
    0x00, 0x00, 0x00, 0x09, 0x70, 0x48, 0x59, 0x73,
    0x00, 0x00, 0x2E, 0x23, 0x00, 0x00, 0x2E, 0x23, 0x01,
    0x00, 0x00, 0x00, 0x00,                      # dummy CRC
    # IDAT + IEND (content irrelevant to the parser)
    0x00, 0x00, 0x00, 0x0C, 0x49, 0x44, 0x41, 0x54,
    0x08, 0x99, 0x63, 0xF8, 0xFF, 0xFF, 0xFF, 0x3F,
    0x00, 0x05, 0xFE, 0x02, 0xFE, 0xDC, 0xCC, 0x59, 0xE7,
    0x00, 0x00, 0x00, 0x00, 0x49, 0x45, 0x4E, 0x44,
    0xAE, 0x42, 0x60, 0x82
  ))
  f <- tempfile(fileext = ".png"); writeBin(bytes, f); f
}

test_that("rtfplot() reads PNG DPI and defaults to native size (100%)", {
  f <- .tmp_png_dpi(); on.exit(unlink(f), add = TRUE)
  fig <- rtfplot(f)
  expect_equal(fig$img_width, 600L)
  expect_equal(fig$dpi_x, 300, tolerance = 0.01)
  # Native display: 600 px / 300 dpi * 1440 = 2880 twips wide, 300/300 -> 1440.
  disp <- rtfreporter:::.rtfplot_display_twips(fig)
  expect_equal(disp$w, 2880L)
  expect_equal(disp$h, 1440L)
})

test_that("a native-size figure emits its \\picwgoal in twips", {
  f <- .tmp_png_dpi(); on.exit(unlink(f), add = TRUE)
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_figures(list(rtfplot(f)))            # no explicit width -> native
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  generate_rtfreport(doc, out, overwrite = TRUE)
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(txt, "\\\\picwgoal2880")
  expect_match(txt, "\\\\pichgoal1440")
})

test_that("a PNG without pHYs falls back to the default-DPI option", {
  f <- .tmp_png(); on.exit(unlink(f), add = TRUE)   # 1x1, no pHYs
  fig <- rtfplot(f)
  expect_null(fig$dpi_x)
  old <- options(rtfreporter.figure.default_dpi = 144L)
  on.exit(options(old), add = TRUE)
  disp <- rtfreporter:::.rtfplot_display_twips(fig)
  expect_equal(disp$w, as.integer(round(1 / 144 * 1440)))   # 10 twips
})

# ──────── End-to-end render via rtf_figures() / generate_rtfreport() ──────

test_that("a figure can be embedded into a generated RTF document", {
  f <- .tmp_png(); on.exit(unlink(f), add = TRUE)
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1,
                     secinfo = list(header = NULL, footer = NULL))
  doc <- rtf_figures(doc, list(rtfplot(f, width_twips = 4320L)))

  out <- tempfile(fileext = ".rtf")
  on.exit(unlink(out), add = TRUE)
  expect_invisible(generate_rtfreport(doc, out, overwrite = TRUE))
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 0L)
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  # \pict signals an embedded picture, \pngblip identifies the format.
  expect_match(txt, "\\\\pict")
  expect_match(txt, "\\\\pngblip")
})

# ──────── JPEG path (covers .read_jpeg_dims) ──────────────────────────────

# Minimal valid baseline JPEG (1x1 white) -- crafted by hand so the
# test works in any environment without a graphics device.
.tmp_jpeg <- function() {
  bytes <- as.raw(c(
    # SOI
    0xFF, 0xD8,
    # APP0 / JFIF (16 bytes payload incl. length)
    0xFF, 0xE0, 0x00, 0x10,
    0x4A, 0x46, 0x49, 0x46, 0x00,
    0x01, 0x01, 0x00,
    0x00, 0x01, 0x00, 0x01,
    0x00, 0x00,
    # DQT (luma quantization table, length 67 = 0x43, all 1s)
    0xFF, 0xDB, 0x00, 0x43, 0x00,
    rep(0x01, 64L),
    # SOF0 (length 11 = 0x0B, precision 8, height=0x0001, width=0x0001,
    # 1 component, id=1, h/v sampling=0x11, qt=0)
    0xFF, 0xC0, 0x00, 0x0B, 0x08,
    0x00, 0x01,        # height
    0x00, 0x01,        # width
    0x01,              # nf
    0x01, 0x11, 0x00,
    # DHT (DC table 0, 31 bytes payload)
    0xFF, 0xC4, 0x00, 0x1F, 0x00,
    rep(0x00, 16L),
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
    0x08, 0x09, 0x0A, 0x0B,
    # DHT (AC table 0, 181 bytes payload) -- minimal, all zero
    0xFF, 0xC4, 0x00, 0xB5, 0x10,
    rep(0x00, 178L),
    # SOS (length 8 = 0x0008, 1 component selector)
    0xFF, 0xDA, 0x00, 0x08, 0x01, 0x01, 0x00, 0x00, 0x3F, 0x00,
    # Single scan byte (white-ish) then EOI
    0x80,
    0xFF, 0xD9
  ))
  f <- tempfile(fileext = ".jpg")
  writeBin(bytes, f)
  f
}

# 1x1 JPEG whose JFIF APP0 records units = 1 (DPI), 300 x 300.
.tmp_jpeg_dpi <- function() {
  bytes <- as.raw(c(
    0xFF, 0xD8,
    0xFF, 0xE0, 0x00, 0x10,
    0x4A, 0x46, 0x49, 0x46, 0x00,
    0x01, 0x01,
    0x01,              # units = 1 (dpi)
    0x01, 0x2C,        # Xdensity = 300
    0x01, 0x2C,        # Ydensity = 300
    0x00, 0x00,
    0xFF, 0xDB, 0x00, 0x43, 0x00, rep(0x01, 64L),
    0xFF, 0xC0, 0x00, 0x0B, 0x08, 0x00, 0x01, 0x00, 0x01,
    0x01, 0x01, 0x11, 0x00,
    0xFF, 0xC4, 0x00, 0x1F, 0x00, rep(0x00, 16L),
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
    0x08, 0x09, 0x0A, 0x0B,
    0xFF, 0xC4, 0x00, 0xB5, 0x10, rep(0x00, 178L),
    0xFF, 0xDA, 0x00, 0x08, 0x01, 0x01, 0x00, 0x00, 0x3F, 0x00,
    0x80, 0xFF, 0xD9
  ))
  f <- tempfile(fileext = ".jpg"); writeBin(bytes, f); f
}

test_that("rtfplot() reads JPEG JFIF density (units = dpi)", {
  f <- .tmp_jpeg_dpi(); on.exit(unlink(f), add = TRUE)
  fig <- rtfplot(f)
  expect_equal(fig$dpi_x, 300)
  expect_equal(fig$dpi_y, 300)
})

test_that("rtfplot() reads dimensions from a JPEG file", {
  f <- .tmp_jpeg(); on.exit(unlink(f), add = TRUE)
  fig <- rtfplot(f)
  expect_s3_class(fig, "rtfplot")
  expect_identical(fig$img_type, "jpeg")
  expect_identical(fig$img_width,  1L)
  expect_identical(fig$img_height, 1L)
})

test_that("rtfplot() with .jpeg extension also works", {
  jpg <- .tmp_jpeg()
  jpeg_path <- sub("\\.jpg$", ".jpeg", jpg)
  file.rename(jpg, jpeg_path)
  on.exit(unlink(jpeg_path), add = TRUE)
  fig <- rtfplot(jpeg_path)
  expect_identical(fig$img_type, "jpeg")
})

test_that("a JPEG figure renders \\jpegblip in the RTF body", {
  f <- .tmp_jpeg(); on.exit(unlink(f), add = TRUE)
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_figures(list(rtfplot(f, width_twips = 4320L)))
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  generate_rtfreport(doc, out, overwrite = TRUE)
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(txt, "\\\\pict")
  expect_match(txt, "\\\\jpegblip")
})

# ──────── Edge / error branches ───────────────────────────────────────────

test_that(".read_png_dims errors on a truncated PNG", {
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f), add = TRUE)
  writeBin(as.raw(c(0x89, 0x50, 0x4E, 0x47)), f)  # only 4 bytes
  expect_error(rtfplot(f), "too short")
})

test_that(".read_jpeg_dims errors when no SOF marker is found", {
  f <- tempfile(fileext = ".jpg")
  on.exit(unlink(f), add = TRUE)
  # SOI + APP0 stub + EOI -- no SOF0/1/2 frame marker at all.
  bytes <- as.raw(c(
    0xFF, 0xD8,
    0xFF, 0xE0, 0x00, 0x10,
    0x4A, 0x46, 0x49, 0x46, 0x00,
    0x01, 0x01, 0x00,
    0x00, 0x01, 0x00, 0x01,
    0x00, 0x00,
    0xFF, 0xD9
  ))
  writeBin(bytes, f)
  expect_error(rtfplot(f), "SOF marker")
})

test_that("rtfplot() accepts each align value", {
  f <- .tmp_png(); on.exit(unlink(f), add = TRUE)
  for (a in c("left", "center", "right")) {
    expect_identical(rtfplot(f, align = a)$align, a)
  }
})

test_that("rtfplot() rejects unrecognised extension via file_ext()", {
  # Empty extension is also unsupported.
  f <- tempfile(); file.create(f); on.exit(unlink(f), add = TRUE)
  expect_error(rtfplot(f), "PNG and JPEG")
})

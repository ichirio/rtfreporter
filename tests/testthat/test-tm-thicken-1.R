# Tests added so that a requirement whose every mapped test checked only a part of
# it has one test that checks what it states (batch 1: assemble_rtf, assemble_folder,
# rtfplot / rtf_figures).

# ---- helpers (prefixed .t1 so that no other test file is touched) ----------------

.t1_dir <- function() {
  d <- tempfile("t1-")
  dir.create(d)
  d
}

# a base plot with small margins
.t1_pl <- function() {
  graphics::par(mar = rep(0.1, 4))
  graphics::plot(1)
}

# a PNG of w x h pixels at res dpi
.t1_png <- function(dir, name = "p.png", w = 400, h = 300, res = 100) {
  f <- file.path(dir, name)
  # the cairo device writes res = as the file's density; macOS's default
  # (quartz) writes 72 dpi whatever res is
  type <- if (isTRUE(unname(capabilities("cairo")))) "cairo" else getOption("bitmapType")
  grDevices::png(f, width = w, height = h, res = res, type = type)
  .t1_pl()
  grDevices::dev.off()
  f
}

# a file whose running header has the centred cells given (one row each)
.t1_hdr_file <- function(path, cells) {
  rows <- vapply(cells, function(x) {
    paste0("\\trowd\\pard\\intbl\\qc\\li0\\ri0 ", x, "\\cell\\row")
  }, character(1L), USE.NAMES = FALSE)
  writeLines(c("{\\rtf1\\ansi", paste0("{\\header ", paste(rows, collapse = " "), "}"),
               "\\sectd", "BODY", "}"), path)
  path
}

# the RTF a document writes, as one string with the line ends dropped
.t1_rtf <- function(doc) {
  out <- tempfile(fileext = ".rtf")
  on.exit(unlink(out), add = TRUE)
  generate_rtfreport(doc, out, overwrite = TRUE)
  paste(readLines(out, warn = FALSE), collapse = "")
}

# ---- assemble_folder --------------------------------------------------------

test_that("FT-AS5001 [FR-AS148]: assemble_folder(dir) without output_file returns the table of contents (natural file order, then by table number) with the seven columns and writes nothing", {
  d <- .t1_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  # the table numbers run against the file names: t10.rtf is Table 14.1.2, t2.rtf is
  # Table 14.1.10; b9.rtf and b10.rtf have no number
  .t1_hdr_file(file.path(d, "t10.rtf"), c("Table 14.1.2", "Two"))
  .t1_hdr_file(file.path(d, "t2.rtf"), c("Table 14.1.10", "Ten"))
  .t1_hdr_file(file.path(d, "b10.rtf"), "Listing B ten")
  .t1_hdr_file(file.path(d, "b9.rtf"), "Listing B nine")
  writeLines("not an rtf file", file.path(d, "notes.txt"))
  before <- list.files(d, all.files = TRUE, recursive = TRUE)
  sp <- assemble_folder(d)
  expect_s3_class(sp, "data.frame")
  expect_identical(names(sp),
                   c("order", "file", "table", "heading", "label", "level", "pages"))
  # the numbered files by number (14.1.2 before 14.1.10), then the others in natural
  # order of their names (b9 before b10)
  expect_identical(basename(sp$file), c("t10.rtf", "t2.rtf", "b9.rtf", "b10.rtf"))
  expect_identical(sp$order, 1:4)
  expect_identical(sp$table, c("14.1.2", "14.1.10", NA, NA))
  # nothing else is written: not a book, not a spec file
  expect_identical(list.files(d, all.files = TRUE, recursive = TRUE), before)
})

# ---- rtfplot ------------------------------------------------------------------

test_that("FT-AS5002 [FR-AS201]: rtfplot(align = ) is center by default, or left or right, and sets the paragraph's alignment in the RTF", {
  d <- .t1_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- .t1_png(d)
  expect_identical(rtfplot(f)$align, "center")
  want <- c(left = "\\ql", center = "\\qc", right = "\\qr")
  for (a in names(want)) {
    rp <- rtfplot(f, align = a)
    expect_identical(rp$align, a)
    txt <- .t1_rtf(rtf_figures(rtf_document(), rp))
    expect_true(grepl(paste0(want[[a]], "{\\pict"), txt, fixed = TRUE), info = a)
  }
})

test_that("FT-AS5003 [FR-AS211]: rtfplot() of a missing path stops with Image file not found: <path>, and of another extension (or none) with the PNG, JPEG and EMF message", {
  d <- .t1_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  nf <- file.path(d, "nope.png")
  expect_error(rtfplot(nf), paste0("Image file not found: ", nf), fixed = TRUE)
  msg <- "rtfplot supports PNG, JPEG and EMF files only."
  for (nm in c("x.gif", "x.svg", "x", "x.png.txt")) {
    p <- file.path(d, nm)
    writeLines("x", p)
    expect_error(rtfplot(p), msg, fixed = TRUE, info = nm)
  }
})

test_that("FT-AS5004 [FR-AS212]: rtfplot() of a PNG shorter than 24 bytes, and of a JPEG with no frame marker, stops with the messages", {
  d <- .t1_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  short <- file.path(d, "short.png")
  writeBin(as.raw(1:23), short)
  expect_error(rtfplot(short), "File too short to be a valid PNG.", fixed = TRUE)
  # 24 bytes is long enough to be read
  ok <- file.path(d, "ok.png")
  grDevices::png(ok, width = 20, height = 10, res = 100)
  .t1_pl()
  grDevices::dev.off()
  expect_gte(file.size(ok), 24)
  expect_s3_class(rtfplot(ok), "rtfplot")
  # a JPEG: the start-of-image marker, one APPn segment and no frame (SOF) marker
  nosof <- file.path(d, "nosof.jpg")
  writeBin(as.raw(c(0xFF, 0xD8, 0xFF, 0xE0, 0x00, 0x02, 0xFF, 0xD9)), nosof)
  expect_error(rtfplot(nosof), "Could not locate SOF marker to read JPEG dimensions.",
               fixed = TRUE)
})

# ---- rtfplot / rtf_figures: the picture in the RTF --------------------------------

test_that("FT-AS5005 [FR-AS214]: a PNG and a JPEG are embedded as \\pict with \\pngblip or \\jpegblip, the pixel size, the display size in twips and the file's bytes as hex", {
  skip_if_not(isTRUE(unname(capabilities("jpeg"))))
  d <- .t1_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  hex <- function(f) {
    toupper(paste(sprintf("%02x", as.integer(readBin(f, "raw", file.size(f)))),
                  collapse = ""))
  }
  # 400 x 300 px at 100 dpi: 4 x 3 in = 5760 x 4320 twips
  png_f <- .t1_png(d, "a.png", 400, 300, 100)
  txt <- .t1_rtf(rtf_figures(rtf_document(), png_f))
  expect_true(grepl("\\pict\\pngblip\\picw400\\pich300\\picwgoal5760\\pichgoal4320",
                    txt, fixed = TRUE))
  expect_true(grepl(hex(png_f), txt, fixed = TRUE))
  expect_false(grepl("\\jpegblip", txt, fixed = TRUE))
  # a JPEG: 200 x 100 px; given 2880 x 1440 twips as the display size
  jpg <- file.path(d, "b.jpg")
  grDevices::jpeg(jpg, width = 200, height = 100)
  .t1_pl()
  grDevices::dev.off()
  txt <- .t1_rtf(rtf_figures(rtf_document(), jpg, width_twips = 2880, height_twips = 1440))
  expect_true(grepl("\\pict\\jpegblip\\picw200\\pich100\\picwgoal2880\\pichgoal1440",
                    txt, fixed = TRUE))
  expect_true(grepl(hex(jpg), txt, fixed = TRUE))
  expect_false(grepl("\\pngblip", txt, fixed = TRUE))
})

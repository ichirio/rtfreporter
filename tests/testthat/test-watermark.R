# rtf_watermark() -- the diagonal word drawn behind the page body.

.wm_doc <- function(..., n_pages = 1L) {
  doc <- rtf_document(...)
  doc <- rtf_section(doc, page = 1, secinfo = list(
    header = rtf_header(rows = list(c(l = "Protocol RTF-101")))))
  rtf_tables(doc, replicate(n_pages,
                            data.frame(A = 1L, B = "x", stringsAsFactors = FALSE),
                            simplify = FALSE))
}

.wm_render <- function(doc) {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

# ──────── constructor ─────────────────────────────────────────────────────

test_that("rtf_watermark() validates its arguments", {
  expect_error(rtf_watermark(), "`text` is required")
  expect_error(rtf_watermark("D", font_size_half_points = 0), "positive number")
  expect_error(rtf_watermark("D", color = "red"), "#RRGGBB")
  expect_error(rtf_watermark("D", angle = "x"), "must be one number")
  expect_error(rtf_watermark("D", width_in = -1), "`width_in`")
  expect_s3_class(rtf_watermark("DRAFT"), "rtf_watermark")
})

test_that("a bare string is accepted as a watermark", {
  wm <- rtfreporter:::.normalize_watermark("DRAFT")
  expect_s3_class(wm, "rtf_watermark")
  expect_identical(wm$text, "DRAFT")
  # Nothing to draw -> nothing at all.
  expect_null(rtfreporter:::.normalize_watermark(NULL))
  expect_null(rtfreporter:::.normalize_watermark(NA))
  expect_null(rtfreporter:::.normalize_watermark(""))
  expect_error(rtfreporter:::.normalize_watermark(list(1, 2)), "rtf_watermark")
})

test_that("the colour is converted to the BGR integer a shape property wants", {
  # #C8C8C8 is grey, so B, G and R are equal: 0xC8C8C8 = 13158600.
  expect_identical(rtfreporter:::.watermark_bgr("#C8C8C8"), 13158600L)
  # Pure red #FF0000 -> B=0, G=0, R=255 -> 255.
  expect_identical(rtfreporter:::.watermark_bgr("#FF0000"), 255L)
  # Pure blue #0000FF -> B=255 -> 255 * 65536.
  expect_identical(rtfreporter:::.watermark_bgr("#0000FF"), 16711680L)
})

# ──────── rendering ───────────────────────────────────────────────────────

test_that("no watermark by default -- output carries no shape", {
  expect_false(grepl("shpinst", .wm_render(.wm_doc()), fixed = TRUE))
})

test_that("a document watermark emits one shape per section, behind the text", {
  txt <- .wm_render(.wm_doc(watermark = "DRAFT", n_pages = 3L))
  expect_identical(length(gregexpr("shpinst", txt, fixed = TRUE)[[1L]]), 1L)
  expect_match(txt, "DRAFT", fixed = TRUE)
  # Behind the text, stated both ways: the destination flag and the shape
  # property (readers honour one or the other).
  expect_match(txt, "shpfblwtxt1", fixed = TRUE)
  expect_match(txt, "fBehindDocument", fixed = TRUE)
  # In the header group, which is what repeats it on every page of the section.
  expect_match(txt, "{\\header", fixed = TRUE)
  expect_match(txt, "shpfhdr1", fixed = TRUE)
})

test_that("the shape is emitted even when the section has no header text", {
  doc <- rtf_document(watermark = "DRAFT")
  doc <- rtf_section(doc, page = 1, secinfo = list(header = NULL, footer = NULL))
  doc <- rtf_tables(doc, list(data.frame(A = 1L)))
  expect_match(.wm_render(doc), "shpinst", fixed = TRUE)
})

test_that("a section overrides the document watermark, and NA turns it off", {
  doc <- rtf_document(watermark = "DRAFT")
  doc <- rtf_section(doc, page = 1, secinfo = list(
    header = rtf_header(rows = list(c(l = "S1")))))
  doc <- rtf_section(doc, page = 2, secinfo = list(
    header = rtf_header(rows = list(c(l = "S2"))),
    watermark = rtf_watermark("CONFIDENTIAL")))
  doc <- rtf_section(doc, page = 3, secinfo = list(
    header = rtf_header(rows = list(c(l = "S3"))),
    watermark = NA))
  doc <- rtf_tables(doc, replicate(3L, data.frame(A = 1L), simplify = FALSE))
  txt <- .wm_render(doc)
  # Two shapes: section 1 inherits DRAFT, section 2 overrides, section 3 opts out.
  expect_identical(length(gregexpr("shpinst", txt, fixed = TRUE)[[1L]]), 2L)
  expect_match(txt, "DRAFT", fixed = TRUE)
  expect_match(txt, "CONFIDENTIAL", fixed = TRUE)
})

test_that("rtf_config() sets and clears the watermark on a composed document", {
  doc <- .wm_doc()
  expect_false(grepl("shpinst", .wm_render(doc), fixed = TRUE))
  expect_match(.wm_render(rtf_config(doc, watermark = "DRAFT")),
               "shpinst", fixed = TRUE)
  # NA clears; NULL means "leave alone", so it cannot.
  withdrawn <- rtf_config(rtf_config(doc, watermark = "DRAFT"), watermark = NA)
  expect_false(grepl("shpinst", .wm_render(withdrawn), fixed = TRUE))
})

test_that("angle is written as the fixed-point number a shape wants", {
  txt <- .wm_render(.wm_doc(watermark = rtf_watermark("D", angle = -45)))
  expect_match(txt, paste0("{\\sn rotation}{\\sv ", -45 * 65536, "}"),
               fixed = TRUE)
})

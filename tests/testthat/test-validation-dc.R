# The document area's leaf requirements that had no test (the deviations of
# the first validation version): each test names its FT id and its FRs.

test_that("FT-DC219 [FR-DC136 FR-DC137]: rtf_config() refuses a watermark that is not one and a font_lang that is not a named vector", {
  doc <- rtf_document()
  expect_error(rtf_config(doc, watermark = list(1, 2)),
               "`watermark` must be an rtf_watermark object, a single string, or NULL / NA for none.",
               fixed = TRUE)
  expect_error(rtf_config(doc, font_lang = "ja"),
               "`font_lang` must be a named character vector of languages per font slot",
               fixed = TRUE)
  # the valid forms pass through
  expect_s3_class(rtf_config(doc, watermark = "DRAFT"), "rtf_document")
  expect_s3_class(rtf_config(doc, font_lang = c(east_asian = "ja")), "rtf_document")
})

test_that("FT-DC220 [FR-DC166 FR-DC167]: a section without a header or footer of its own keeps the previous section's; rtf_header(\"\") gives a blank band", {
  t1 <- as_rtftable(data.frame(a = 1))
  t2 <- as_rtftable(data.frame(b = 2))
  t3 <- as_rtftable(data.frame(c = 3))
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1, header = rtf_header("HDRONE"), footer = rtf_footer("FTRONE"))
  doc <- rtf_section(doc, page = 2)
  doc <- rtf_section(doc, page = 3, header = rtf_header(""))
  doc <- rtf_tables(doc, list(t1, t2, t3))
  s <- .render_to_string(doc)
  n <- function(pat) lengths(regmatches(s, gregexpr(pat, s)))
  # the header is written for the first and the second section (inherited),
  # the third has a band with nothing in it; the footer reaches all three
  expect_identical(n("HDRONE"), 2L)
  expect_identical(n("FTRONE"), 3L)
  expect_identical(n("\\\\header\\b"), 3L)
  # the first section's NULL leaves its pages without a header
  doc0 <- rtf_document()
  doc0 <- rtf_section(doc0, page = 1)
  doc0 <- rtf_tables(doc0, list(t1))
  expect_identical(lengths(regmatches(.render_to_string(doc0), gregexpr("\\\\header\\b", .render_to_string(doc0)))), 0L)
})

test_that("FT-DC221 [FR-DC238]: rtf_header_source(level = ) takes explicit, default or all", {
  t1 <- rtftable(data.frame(a = 1, b = 2))
  expect_error(rtf_header_source(t1, level = "x"), "'arg' should be one of")
  expect_type(rtf_header_source(t1, level = "all"), "character")
})

test_that("FT-DC222 [FR-DC255 FR-DC256 FR-DC257 FR-DC259]: rtf_replace_text() reads and writes in the encoding given, matches literally, and checks its arguments", {
  read_raw <- function(p) rawToChar(readBin(p, what = "raw", n = file.info(p)$size))
  # the encoding: a Latin-1 file is read and written as Latin-1
  p <- tempfile(fileext = ".rtf")
  on.exit(unlink(c(p, paste0(p, ".bak"))), add = TRUE)
  writeBin(iconv("café DRAFT", from = "UTF-8", to = "latin1", toRaw = TRUE)[[1L]], p)
  rtf_replace_text(p, "DRAFT", "FINAL", encoding = "latin1", backup = FALSE)
  expect_identical(iconv(read_raw(p), from = "latin1", to = "UTF-8"), "café FINAL")
  # literal matching: text split by a control word is not matched
  q <- tempfile(fileext = ".rtf")
  on.exit(unlink(q), add = TRUE)
  writeBin(charToRaw("DR\\b0 AFT and DRAFT"), q)
  rtf_replace_text(q, "DRAFT", "FINAL", backup = FALSE)
  expect_identical(read_raw(q), "DR\\b0 AFT and FINAL")
  # the guards
  expect_error(rtf_replace_text(c("a", "b"), "x", "y"), "`input_file` must be a single file path.", fixed = TRUE)
  expect_error(rtf_replace_text(1, "x", "y"), "`input_file` must be a single file path.", fixed = TRUE)
  expect_error(rtf_replace_text(q), "Both `target` and `replacement` must be supplied.", fixed = TRUE)
  expect_error(rtf_replace_text(q, "x"), "Both `target` and `replacement` must be supplied.", fixed = TRUE)
})

test_that("FT-DC223 [FR-DC267]: rtfreporter_reset_defaults() returns the factory defaults invisibly", {
  old <- options(rtfreporter.rounding = "sas")
  on.exit(options(old), add = TRUE)
  v <- withVisible(rtfreporter_reset_defaults())
  expect_false(v$visible)
  expect_type(v$value, "list")
  expect_true(length(v$value) > 0L)
})

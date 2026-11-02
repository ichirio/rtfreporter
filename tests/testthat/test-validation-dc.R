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

# A one-table document with the given header and footer, and the text of its
# header or footer group as written.
.vg_doc <- function(header = NULL, footer = NULL, doc = rtf_document()) {
  rtf_section(doc, page = 1, header = header, footer = footer) |>
    rtf_tables(list(rtftable(data.frame(A = c("a1", "a2"), B = c("b1", "b2")))))
}
.vg_band <- function(doc, band = "header") {
  s <- .render_to_string(doc)
  regmatches(s, regexpr(paste0("\\{\\\\", band, " [^\n]*"), s))
}

test_that("FT-DC241 [FR-DC325 FR-DC326]: rtf_header() / rtf_footer() rows place each cell by its name; border is the band's rule", {
  # one row: l and r share the writable width (13680 twips) equally
  h <- .vg_band(.vg_doc(rtf_header(c(l = "HL", r = "HR"))))
  expect_true(grepl("\\cellx6840\\cellx13680\\ql\\li0\\ri0 HL\\cell\\qr\\li0\\ri0 HR\\cell\\row",
                    h, fixed = TRUE))
  # several rows, one table row each; c is centred
  h <- .vg_band(.vg_doc(rtf_header(list(c(l = "R1"), c(c = "R2")))))
  expect_identical(lengths(regmatches(h, gregexpr("\\\\row", h))), 2L)
  expect_true(grepl("\\qc\\li0\\ri0 R2\\cell", h, fixed = TRUE))
  f <- .vg_band(.vg_doc(footer = rtf_footer(c(l = "FL", c = "FC", r = "FR"))), "footer")
  expect_true(grepl("\\ql\\li0\\ri0 FL\\cell\\qc\\li0\\ri0 FC\\cell\\qr\\li0\\ri0 FR\\cell", f, fixed = TRUE))
  expect_error(rtf_header(c(left = "x")),
               "`rtf_header()` row 1: a cell may only be named l, c or r (left, centre, right); got 'left'.",
               fixed = TRUE)
  expect_error(rtf_footer(1), "`rows` must be a named character vector or list of named vectors.",
               fixed = TRUE)
  # border: none on the header and a top rule on the footer by default
  expect_false(grepl("clbrdr", .vg_band(.vg_doc(rtf_header(c(l = "H")))), fixed = TRUE))
  f <- .vg_band(.vg_doc(footer = rtf_footer(c(l = "F"))), "footer")
  expect_true(grepl("\\clbrdrt\\brdrs\\brdrw15\\cellx13680", f, fixed = TRUE))
  f <- .vg_band(.vg_doc(footer = rtf_footer(c(l = "F"), border = NULL)), "footer")
  expect_false(grepl("clbrdr", f, fixed = TRUE))
  h <- .vg_band(.vg_doc(rtf_header(c(l = "H"), border = rtf_border(bottom = TRUE))))
  expect_true(grepl("\\clbrdrb\\brdrs\\brdrw15\\cellx13680", h, fixed = TRUE))
  expect_error(rtf_footer("x", border = "top"), "`border` must be NULL or an rtf_border object.",
               fixed = TRUE)
})

test_that("FT-DC242 [FR-DC327 FR-DC328 FR-DC330]: a band's width, font size and left / right padding", {
  # width: a fraction of the writable width, or twips; width_twips wins
  h <- .vg_band(.vg_doc(rtf_header(c(l = "HL", r = "HR"), width = 0.5)))
  expect_true(grepl("\\cellx3420\\cellx6840\\ql", h, fixed = TRUE))
  f <- .vg_band(.vg_doc(footer = rtf_footer(c(l = "F"), width = 6000)), "footer")
  expect_true(grepl("\\cellx6000\\ql", f, fixed = TRUE))
  f <- .vg_band(.vg_doc(footer = rtf_footer(c(l = "F"), width_twips = 4000L, width = "page")), "footer")
  expect_true(grepl("\\cellx4000\\ql", f, fixed = TRUE))
  h <- .vg_band(.vg_doc(rtf_header(c(l = "H"), width_twips = 5000L)))
  expect_true(grepl("\\cellx5000\\ql", h, fixed = TRUE))
  # font size: written on the cells, the row height following it
  h <- .vg_band(.vg_doc(rtf_header(c(l = "H"), font_size_half_points = 24L)))
  expect_true(grepl("\\trrh290", h, fixed = TRUE))
  expect_true(grepl("\\fs24 H\\cell", h, fixed = TRUE))
  f <- .vg_band(.vg_doc(footer = rtf_footer(c(l = "F"), font_size_half_points = 16L)), "footer")
  expect_true(grepl("\\trrh210", f, fixed = TRUE))
  expect_true(grepl("\\fs16 F\\cell", f, fixed = TRUE))
  # left / right padding
  h <- .vg_band(.vg_doc(rtf_header(c(l = "H"), cell_padding_left_twips = 77L,
                                   cell_padding_right_twips = 88L)))
  expect_true(grepl("\\ql\\li77\\ri88 H\\cell", h, fixed = TRUE))
  f <- .vg_band(.vg_doc(footer = rtf_footer(c(l = "F"), cell_padding_left_twips = 5L,
                                            cell_padding_right_twips = 6L)), "footer")
  expect_true(grepl("\\ql\\li5\\ri6 F\\cell", f, fixed = TRUE))
})

test_that("FT-DC243 [FR-DC329]: a band's markup is its own, NULL inheriting the document's", {
  h <- .vg_band(.vg_doc(rtf_header(c(l = "H >= 1"), markup = "relational")))
  expect_true(grepl("H \\u8805? 1\\cell", h, fixed = TRUE))
  h <- .vg_band(.vg_doc(rtf_header(c(l = "H >= 1"))))
  expect_true(grepl("H >= 1\\cell", h, fixed = TRUE))
  f <- .vg_band(.vg_doc(footer = rtf_footer(c(l = "x^{2}"))), "footer")
  expect_true(grepl("x{\\super 2}\\cell", f, fixed = TRUE))
  f <- .vg_band(.vg_doc(footer = rtf_footer(c(l = "x^{2}"), markup = "none")), "footer")
  expect_true(grepl("x^\\{2\\}\\cell", f, fixed = TRUE))
})

test_that("FT-DC244 [FR-DC331]: drop_empty_rows leaves out a band row whose own tokens are all empty", {
  doc <- function(drop) {
    .vg_doc(rtf_header(list(c(l = "PROT"), c(l = "<{POP}>"), c(c = "MID")), drop_empty_rows = drop),
            rtf_footer(list(c(r = "FOOT"), c(l = "({POP})")), drop_empty_rows = drop),
            doc = rtf_document(tokens = list(POP = "")))
  }
  nrow_of <- function(x) lengths(regmatches(x, gregexpr("\\\\row", x)))
  kept <- doc(FALSE)
  expect_identical(nrow_of(.vg_band(kept)), 3L)
  expect_true(grepl("<>\\cell", .vg_band(kept), fixed = TRUE))
  expect_identical(nrow_of(.vg_band(kept, "footer")), 2L)
  dropped <- doc(TRUE)
  expect_identical(nrow_of(.vg_band(dropped)), 2L)
  expect_true(grepl("PROT\\cell", .vg_band(dropped), fixed = TRUE))
  expect_true(grepl("MID\\cell", .vg_band(dropped), fixed = TRUE))
  expect_identical(nrow_of(.vg_band(dropped, "footer")), 1L)
  expect_error(rtf_header(c(l = "x"), drop_empty_rows = NA),
               "`drop_empty_rows` must be TRUE or FALSE.", fixed = TRUE)
})

# The rows of a rendered document that carry `text` in a cell.
.vg_rows <- function(s, text) {
  r <- regmatches(s, gregexpr("\\\\trowd.*?\\\\row", s))[[1L]]
  r[grepl(text, r, fixed = TRUE)]
}
.vg_titled <- function() {
  rtf_document() |>
    rtf_section(page = 1, header = rtf_header(c(l = "HDR")), footer = NULL) |>
    rtf_tables(list(rtftable(data.frame(A = c("a1 >= 1", "a2"), B = c("b1", "b2")))),
               titles = list("TTL"), footnotes = list("FNT"))
}

test_that("FT-DC245 [FR-DC311]: the title and footnote format and width options reach a document that sets none", {
  s <- .render_to_string(.vg_titled())
  expect_length(.vg_rows(s, "TTL"), 0L)                     # text by default
  expect_true(grepl("\\cellx13680\\ql\\li0\\ri0 FNT\\cell", .vg_rows(s, "FNT"), fixed = TRUE))
  old <- options(rtfreporter.title_format = "table", rtfreporter.title_width = 0.5,
                 rtfreporter.footnote_width = 0.25,
                 rtfreporter.footnote_format = getOption("rtfreporter.footnote_format"))
  on.exit(options(old), add = TRUE)
  s <- .render_to_string(.vg_titled())
  expect_true(grepl("\\cellx6840", .vg_rows(s, "TTL"), fixed = TRUE))
  expect_true(grepl("\\cellx3420", .vg_rows(s, "FNT"), fixed = TRUE))
  options(rtfreporter.footnote_format = "text")
  s <- .render_to_string(.vg_titled())
  expect_length(.vg_rows(s, "FNT"), 0L)
  expect_true(grepl("FNT", s, fixed = TRUE))
})

test_that("FT-DC246 [FR-DC315]: the font size, row height, left / right padding and markup options reach a document that sets none", {
  s <- .render_to_string(.vg_titled())
  expect_true(grepl("\\widowctrl\\fs18", s, fixed = TRUE))
  expect_true(grepl("\\trrh230", .vg_rows(s, "a1"), fixed = TRUE))
  expect_true(grepl("\\ql\\li0\\ri0 a1 >= 1\\cell", .vg_rows(s, "a1"), fixed = TRUE))
  old <- options(rtfreporter.font_size_half_points = 20L, rtfreporter.row_height_twips = 300L,
                 rtfreporter.cell_padding_left_twips = 33L,
                 rtfreporter.cell_padding_right_twips = 44L, rtfreporter.markup = "relational")
  on.exit(options(old), add = TRUE)
  s <- .render_to_string(.vg_titled())
  expect_true(grepl("\\widowctrl\\fs20", s, fixed = TRUE))
  body <- .vg_rows(s, "a1")
  expect_true(grepl("\\trrh300", body, fixed = TRUE))
  expect_true(grepl("\\ql\\li33\\ri44 a1 \\u8805? 1\\cell", body, fixed = TRUE))
  # the header band takes the row height and padding too
  expect_true(grepl("\\trrh300\\cellx13680\\ql\\li33\\ri44 HDR", .vg_rows(s, "HDR"), fixed = TRUE))
})

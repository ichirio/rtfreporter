# The footnote block follows its content's placement as the title block does
# (#968): a centred figure or table has its footnote rows centred (\trqc) under
# it, a right-aligned one \trqr, a left one nothing.

.fp_png <- function() testthat::test_path("fixtures", "validation", "dot.png")

.fp_doc_fig <- function(align, style = NULL) {
  rtf_document(style = style) |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_figures(list(rtfplot(.fp_png(), width_twips = 4320L, height_twips = 2880L,
                             align = align)),
                titles = "Figure 14.2.1", footnotes = c("Note A", "Note B"))
}

.fp_doc_tbl <- function(align, style = NULL) {
  tb <- rtftable(data.frame(A = 1L, B = "x", stringsAsFactors = FALSE),
                 table_align = align)
  rtf_document(style = style) |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_tables(list(tb), titles = list(c("Table 14.1.1", "Summary")),
               footnotes = list(c("Note A", "Note B")))
}

# the table rows (one string each) whose text holds `text`
.fp_rows <- function(txt, text) {
  rows <- regmatches(txt, gregexpr("\\\\trowd[^\n]*", txt))[[1L]]
  rows[grepl(text, rows, fixed = TRUE)]
}

# the rows of the footnote block: written after the content, found by text
.fp_fn <- function(txt) .fp_rows(txt, "Note ")

test_that("FT-PL4001 [FR-DC383]: a centred figure's footnote rows are centred (trqc)", {
  fn <- .fp_fn(.render_to_string(.fp_doc_fig("center")))
  expect_length(fn, 2L)
  expect_true(all(grepl("\\trqc", fn, fixed = TRUE)))
  expect_false(any(grepl("\\trqr", fn, fixed = TRUE)))
})

test_that("FT-PL4002 [FR-DC383]: a right-aligned figure's footnote rows are right (trqr)", {
  fn <- .fp_fn(.render_to_string(.fp_doc_fig("right")))
  expect_length(fn, 2L)
  expect_true(all(grepl("\\trqr", fn, fixed = TRUE)))
  expect_false(any(grepl("\\trqc", fn, fixed = TRUE)))
})

test_that("FT-PL4003 [FR-DC383]: a left figure's footnote rows carry neither trqc nor trqr", {
  fn <- .fp_fn(.render_to_string(.fp_doc_fig("left")))
  expect_length(fn, 2L)
  expect_false(any(grepl("\\trqc", fn, fixed = TRUE)))
  expect_false(any(grepl("\\trqr", fn, fixed = TRUE)))
})

test_that("FT-PL4004 [FR-DC383]: a table's footnote rows follow table_align center / right / left", {
  fc <- .fp_fn(.render_to_string(.fp_doc_tbl("center")))
  fr <- .fp_fn(.render_to_string(.fp_doc_tbl("right")))
  fl <- .fp_fn(.render_to_string(.fp_doc_tbl("left")))
  expect_length(fc, 2L)
  expect_true(all(grepl("\\trqc", fc, fixed = TRUE)))
  expect_true(all(grepl("\\trqr", fr, fixed = TRUE)))
  expect_false(any(grepl("\\\\trq[cr]", fl)))
})

test_that("FT-PL4005 [FR-DC383]: footnote_width = \"page\" gives the full width and the content's placement", {
  # the help: footnote_width sets the width only; the placement still follows
  # the content, so a page-wide block is centred / left as its content is
  fc <- .fp_fn(.render_to_string(.fp_doc_fig("center", rtf_style(footnote_width = "page"))))
  expect_length(fc, 2L)
  expect_true(all(grepl("\\trqc", fc, fixed = TRUE)))
  expect_true(all(grepl("\\cellx13680", fc, fixed = TRUE)))
  fl <- .fp_fn(.render_to_string(.fp_doc_fig("left", rtf_style(footnote_width = "page"))))
  expect_false(any(grepl("\\\\trq[cr]", fl)))
})

test_that("FT-PL4006 [FR-DC383]: the title block keeps its placement; a page header and footer stay at the margin", {
  txt <- .render_to_string(.fp_doc_tbl("center", rtf_style(title_format = "table")))
  ti <- .fp_rows(txt, "Table 14.1.1")
  expect_length(ti, 1L)
  expect_true(all(grepl("\\trqc", ti, fixed = TRUE)))
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = rtf_header(c(l = "HDR")),
                                         footer = rtf_footer(c(l = "FTR")))) |>
    rtf_figures(list(rtfplot(.fp_png(), width_twips = 4320L, height_twips = 2880L)),
                footnotes = "Note A")
  txt <- .render_to_string(doc)
  expect_true(all(grepl("\\trqc", .fp_rows(txt, "Note A"), fixed = TRUE)))
  expect_false(any(grepl("\\\\trq[cr]", .fp_rows(txt, "HDR"))))
  expect_false(any(grepl("\\\\trq[cr]", .fp_rows(txt, "FTR"))))
})

test_that("FT-PL4007 [FR-DC383]: footnote_format = \"text\" writes paragraphs, with no table placement", {
  txt <- .render_to_string(.fp_doc_fig("center", rtf_style(footnote_format = "text")))
  expect_false(any(grepl("\\\\trq[cr]", .fp_rows(txt, "Note A"))))
  expect_match(txt, "Note A", fixed = TRUE)
})

# the last \cellx of a row string
.fp_last_cellx <- function(rows) {
  vapply(rows, function(r) {
    m <- regmatches(r, gregexpr("cellx[0-9]+", r))[[1L]]
    as.integer(sub("cellx", "", m[length(m)], fixed = TRUE))
  }, 0L, USE.NAMES = FALSE)
}

.fp_doc_narrow <- function(align) {
  tb <- rtftable(data.frame(A = 1L, B = "x", stringsAsFactors = FALSE),
                 table_align = align, table_width_twips = 6000L)
  rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_tables(list(tb), footnotes = list(c("Note A", "Note B")))
}

test_that("FT-PL4008 [FR-DC383]: by default the footnote is as wide as a table narrower than the page, centred or left", {
  for (al in c("center", "left")) {
    txt <- .render_to_string(.fp_doc_narrow(al))
    body <- .fp_rows(txt, "x")
    fn <- .fp_fn(txt)
    expect_true(all(.fp_last_cellx(body) == 6000L))
    expect_identical(.fp_last_cellx(fn), c(6000L, 6000L))
  }
  fn <- .fp_fn(.render_to_string(.fp_doc_narrow("center")))
  expect_true(all(grepl("\\trqc", fn, fixed = TRUE)))
})

test_that("FT-PL4009 [FR-DC383]: by default a figure's footnote is as wide as the picture, not the page", {
  fn <- .fp_fn(.render_to_string(.fp_doc_fig("center")))
  expect_identical(.fp_last_cellx(fn), c(4320L, 4320L))
  expect_true(all(grepl("\\trqc", fn, fixed = TRUE)))
})

test_that("FT-PL4010 [FR-DC383]: footnote_width = \"page\" keeps the page width under a narrow table", {
  doc <- rtf_document(style = rtf_style(footnote_width = "page")) |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_tables(list(rtftable(data.frame(A = 1L, B = "x", stringsAsFactors = FALSE),
                             table_width_twips = 6000L)),
               footnotes = list(c("Note A", "Note B")))
  fn <- .fp_fn(.render_to_string(doc))
  expect_identical(.fp_last_cellx(fn), c(13680L, 13680L))
})

# The characters of data-raw/gen_watermark_example.R: a document with the
# non-ASCII text a report uses, written as \uN? escapes, with the markup's sub-
# and superscripts, and a Japanese line in the east_asian font slot.

.wc_doc <- function() {
  df <- data.frame(a = c("n", "Mean"), b = c("24", "1.0"), stringsAsFactors = FALSE)
  rtf_document(page = rtf_page(orientation = "landscape"),
               font_table = list(list(name = c(latin = "Courier New",
                                               east_asian = "MS Gothic"))),
               font_lang = c(east_asian = "ja"),
               watermark = rtf_watermark("DRAFT")) |>
    rtf_tables(rtftable(df, border = "tfl",
                        col_header = c("A", "Plasma (ng/mL); \u03bb_{z} = 0.18 h^{-1}, t\u00bd = 3.8 h"))) |>
    rtf_footnotes(list(c(
      "C_{max} (\u03bcg/mL); AUC in ng\u00b7h/mL; \u03bb_{z}; t\u00bd.",
      "Mean \u00b1 SD; \u2264 0.500 ng/mL; \u2265 80%; 25 \u00b0C.",
      "\u6ce8: \u6570\u5024")))
}

test_that("FT-PL4011 [FR-DC338 FR-TB558]: lambda, mu, the middle dot, plus-minus, the signs, the degree and one half are \\uN? escapes, with sub- and superscripts", {
  txt <- .render_to_string(.wc_doc())
  expect_match(txt, "\\u955?{\\sub z} = 0.18 h{\\super -1}, t\\u189? = 3.8 h", fixed = TRUE)
  expect_match(txt, "C{\\sub max} (\\u956?g/mL); AUC in ng\\u183?h/mL; \\u955?{\\sub z}; t\\u189?.", fixed = TRUE)
  expect_match(txt, "Mean \\u177? SD; \\u8804? 0.500 ng/mL; \\u8805? 80%; 25 \\u176?C.", fixed = TRUE)
  # nothing above 127 is written as it is
  expect_false(any(utf8ToInt(txt) > 127L))
})

test_that("FT-PL4012 [FR-DC338 FR-DC339]: a Japanese line is in the east_asian font (MS Gothic, dbch) and written as \\uN? escapes", {
  txt <- .render_to_string(.wc_doc())
  expect_match(txt, "\\f1\\fnil\\fcharset128 MS Gothic;", fixed = TRUE)
  ja <- grep("\\u27880?: \\u25968?\\u20516?", strsplit(txt, "\n", fixed = TRUE)[[1L]],
             value = TRUE, fixed = TRUE)
  expect_length(ja, 1L)
  expect_match(ja, "\\dbch\\af1\\langfe1041\\loch\\f0 \\u27880?", fixed = TRUE)
})

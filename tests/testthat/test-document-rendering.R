# Document-level rendering contracts promised by the page-setup article:
# font_table / color_table / default font size must flow into the generated RTF.
# (Row-height cascade, named-list pagination, and orientation are covered in
# test-default-row-height.R / test-as-rtftables.R / test-page-orientation.R.)

.simple_doc <- function(...) {
  df <- data.frame(A = c("1", "2"), B = c("x", "y"), stringsAsFactors = FALSE)
  rtf_document(...) |>
    rtf_tables(as_rtftables(df))
}

test_that("a custom font_table font is written into the RTF \\fonttbl", {
  txt <- .render_to_string(.simple_doc(font_table = list(list(name = "Arial"))))
  expect_match(txt, "\\\\fonttbl")
  expect_match(txt, "Arial")
})

test_that("the default font is Courier when font_table is not set", {
  expect_match(.render_to_string(.simple_doc()), "Courier")
})

test_that("border colours are auto-collected into the RTF \\colortbl", {
  # The rendered palette is built from the BORDER colours actually used in the
  # report (the document `color_table` argument is not consumed by the
  # renderer -- see #36). A coloured border injects its colour into \colortbl.
  df  <- data.frame(A = c("1", "2"), B = c("x", "y"), stringsAsFactors = FALSE)
  hdr <- rtf_header(rows = list(c(l = "X")),
                    border = rtf_border(bottom = rtfreporter:::.rtf_border_side(color = "#1F4E79")))
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = hdr, footer = NULL)) |>
    rtf_tables(as_rtftables(df))
  txt <- .render_to_string(doc)
  expect_match(txt, "\\\\colortbl")
  # #1F4E79 = rgb(31, 78, 121)
  expect_match(txt, "\\\\red31\\\\green78\\\\blue121")
})

test_that("hardened RTF preamble: codepage, \\uc1, charset, widowctrl (#82)", {
  txt <- .render_to_string(.simple_doc())
  expect_match(txt, "\\\\ansicpg1252")
  expect_match(txt, "\\\\uc1")
  expect_match(txt, "\\\\deflang1033")
  expect_match(txt, "\\\\fcharset0")
  expect_match(txt, "\\\\widowctrl")
})

test_that("header/footer band distance is emitted and coordinated with margins (#82)", {
  # Default top/bottom margin is 0.75in = 1080 twips, so headery/footery = 1080.
  df  <- data.frame(A = c("1", "2"), B = c("x", "y"), stringsAsFactors = FALSE)
  doc <- rtf_document() |>
    rtf_tables(as_rtftables(df)) |>
    rtf_section(page = 1, secinfo = list(header = "H", footer = "F"))
  txt <- .render_to_string(doc)
  expect_match(txt, "\\\\headery1080")
  expect_match(txt, "\\\\footery1080")
})

test_that("the document font size is emitted as a document-level \\fs<n>", {
  txt22 <- .render_to_string(
    .simple_doc(default_format = list(font_size_half_points = 22L)))
  expect_match(txt22, "\\\\fs22\\b")

  # Default is 9pt = \fs18.
  expect_match(.render_to_string(.simple_doc()), "\\\\fs18\\b")
})

test_that("a multi-page table wraps each \\page in empty paragraphs (#130, #138)", {
  # group_safe with no page-number token used to emit a bare `\page` straight
  # after a `\row`; Word then ignored the break and rendered the next page flush
  # against the previous one.  Every `\page` must now be the wrapped form
  # {\pard\fs2\par}\page{\pard\fs2\par} so Word honours it.
  df <- data.frame(grp = c("A", "A", "B", "B", "C", "C"),
                   val = as.character(1:6), stringsAsFactors = FALSE)
  pages <- as_rtftables(df, split = "group_safe", max_rows = 2,
                        group_col = "grp", group_by = "value")
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_tables(pages)
  txt <- .render_to_string(doc)

  expect_length(pages, 3L)
  # Two page breaks for three pages, each the full wrapped form (an empty 1pt
  # paragraph, the \page, and another empty paragraph).
  n_page    <- length(gregexpr("\\\\page(?![a-z])", txt, perl = TRUE)[[1]])
  n_wrapped <- length(gregexpr(
    "\\{\\\\pard\\\\fs2\\\\par\\}\\\\page\\{\\\\pard\\\\fs2\\\\par\\}",
    txt)[[1]])
  expect_equal(n_page, 2L)
  expect_equal(n_wrapped, 2L)
  # No bare `\page` survives directly after a `\row` (the old broken form).
  expect_false(grepl("\\\\row\\\\page", txt))
  # The final page is terminated with a \pard before the document close.
  expect_match(txt, "\\\\pard\\s*\\}\\s*$")
})

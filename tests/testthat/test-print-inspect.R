# print.rtftable / print.rtf_document -- build-time text layout inspectors.

test_that("print.rtftable shows headers, alignment, widths and border", {
  rt <- rtftable(
    data.frame(Char = c("Age", "Sex"), Placebo = c("1", "2"),
               Active = c("3", "4"), stringsAsFactors = FALSE),
    col_header = "Characteristic | Placebo | Active")
  out <- paste(capture.output(print(rt)), collapse = "\n")
  expect_match(out, "<rtftable>")
  expect_match(out, "3 columns x 2 data rows")
  expect_match(out, "Characteristic \\| Placebo \\| Active")
  expect_match(out, "Body alignment")
  expect_match(out, "Column widths")
  expect_match(out, "Border:")
  # returns invisibly
  expect_identical(withVisible(print(rt))$visible, FALSE)
})

test_that("print.rtftable surfaces title/footnote attributes", {
  rt <- rtftable(data.frame(a = 1, b = 2))
  attr(rt, "rtf_titles")    <- c("My Title", "Subtitle")
  attr(rt, "rtf_footnotes") <- "A footnote"
  out <- paste(capture.output(print(rt)), collapse = "\n")
  expect_match(out, "Title:")
  expect_match(out, "My Title")
  expect_match(out, "Footnote:")
})

test_that("print.rtf_document outlines page, sections (header/footer) and content", {
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(
      header = rtf_header(rows = list(
        c(l = "Acme", r = "Page {PAGE} of {TOTAL_PAGES}"),
        c(c = "Table 14.1"))),
      footer = rtf_footer(c(l = "Confidential")))) |>
    rtf_tables(list(data.frame(A = 1:2, B = c("x", "y"), stringsAsFactors = FALSE)),
               titles = list(c("Demographics")))
  out <- paste(capture.output(print(doc)), collapse = "\n")
  expect_match(out, "<rtf_document>")
  expect_match(out, "Page:")
  expect_match(out, "Sections \\(1\\)")
  # header cells, including the page-number token, are shown
  expect_match(out, "L: Acme")
  expect_match(out, "Page \\{PAGE\\} of \\{TOTAL_PAGES\\}")
  expect_match(out, "C: Table 14.1")
  expect_match(out, "footer:")
  expect_match(out, "Content blocks \\(1\\)")
  expect_match(out, "\\[1\\] table")
  expect_match(out, "title:.*Demographics")
})

test_that("print.rtf_document handles an empty document", {
  out <- paste(capture.output(print(rtf_document())), collapse = "\n")
  expect_match(out, "Sections \\(0\\)")
  expect_match(out, "Content blocks \\(0\\)")
})

# A page header / footer band row given as a list takes bold =, italic = and
# underline = (the words a title row uses), each applied to every cell of
# that row only (GAPS G14).  Footnote rows of the form list(columns = )
# share the code.

.be_df <- function() data.frame(A = c("1", "2"), B = c("x", "y"),
                                stringsAsFactors = FALSE)

.be_render <- function(doc) {
  d <- tempfile("be-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- file.path(d, "out.rtf")
  generate_rtfreport(doc, f, overwrite = TRUE)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

.be_doc <- function(header = NULL, footer = NULL, footnotes = NULL) {
  doc <- rtf_document() |>
    rtf_section(page = 1, header = header, footer = footer) |>
    rtf_tables(list(.be_df()))
  if (!is.null(footnotes)) doc <- rtf_footnotes(doc, list(footnotes))
  doc
}

# The rows of the {\header ...} (or {\footer ...}) group, as text.
.be_band <- function(txt, which = "header") {
  g <- regmatches(txt, regexpr(sprintf("\\{\\\\%s [^\n]*", which), txt))
  rows <- strsplit(g, "\\row", fixed = TRUE)[[1L]]
  rows[grepl("\\trowd", rows, fixed = TRUE)]
}
# the emphasis words a row's cell text carries (not \brdr / \b0 / \i0 / \ulnone)
.be_has <- function(row, word) grepl(paste0("\\", word, " "), row, fixed = TRUE)

test_that("bold / italic / underline are written in that row's cells only", {
  b <- .be_band(.be_render(.be_doc(rtf_header(list(
    c(l = "Example Pharma", r = "Page {PAGE}"),
    list(columns = c(l = "Protocol: EXP-001", c = "ITT", r = "Draft"),
         bold = TRUE, italic = TRUE, underline = TRUE),
    c(c = "Table 14.1.1"))))))
  expect_length(b, 3L)
  for (w in c("b", "i", "ul")) {
    expect_false(.be_has(b[1L], w))
    expect_true(.be_has(b[2L], w))
    expect_false(.be_has(b[3L], w))
  }
  # every cell of the row, each closed in its own cell
  cells <- strsplit(b[2L], "\\cell", fixed = TRUE)[[1L]]
  cells <- cells[grepl("Protocol|ITT|Draft", cells)]
  expect_length(cells, 3L)
  expect_true(all(grepl("\\b \\i \\ul ", cells, fixed = TRUE)))
  expect_true(all(grepl("\\ulnone \\i0 \\b0 ", cells, fixed = TRUE)))
})

test_that("each field stands alone, and a footer row takes them too", {
  b <- .be_band(.be_render(.be_doc(footer = rtf_footer(list(
    list(columns = c(l = "Confidential"), italic = TRUE),
    list(columns = c(r = "Example Pharma"), underline = TRUE))))), "footer")
  expect_length(b, 2L)
  expect_true(.be_has(b[1L], "i"))
  expect_false(.be_has(b[1L], "b") || .be_has(b[1L], "ul"))
  expect_true(.be_has(b[2L], "ul"))
  expect_false(.be_has(b[2L], "b") || .be_has(b[2L], "i"))
})

test_that("the fields work with the row's border, colour and tokens and markup", {
  txt <- .be_render(.be_doc(rtf_header(list(
    list(columns = c(l = "Study ^{a} {AUTO_PAGE}", r = "Page {PAGE} of {TOTAL_PAGES}"),
         bold = TRUE, color = "#C00000",
         border = rtf_border(bottom = TRUE)),
    c(l = "Example Pharma")))))
  b <- .be_band(txt)
  expect_length(b, 2L)
  r <- b[1L]
  expect_true(.be_has(r, "b"))
  expect_true(grepl("\\clbrdrb", r, fixed = TRUE))      # the row's border
  expect_match(r, "\\\\cf[0-9]+ \\\\b ")                 # colour outermost
  expect_true(grepl("\\super", r, fixed = TRUE))        # ^{} inside the bold
  expect_false(grepl("^{", r, fixed = TRUE))
  expect_true(grepl("\\chpgn", r, fixed = TRUE))       # the AUTO_PAGE field
  expect_false(grepl("{AUTO_PAGE}", r, fixed = TRUE))
  expect_true(grepl("Page 1 of 1", r, fixed = TRUE))    # static tokens filled
  # the markup and the field sit between \b and \b0
  expect_match(r, "\\\\b .*\\\\super.*\\\\chpgn.*\\\\b0 ")
  expect_false(.be_has(b[2L], "b"))
})

test_that("bold = FALSE and the fields left out write the same RTF", {
  rows_plain <- list(list(columns = c(l = "Example Pharma", r = "Page {PAGE}")),
                     c(c = "Draft"))
  rows_false <- list(list(columns = c(l = "Example Pharma", r = "Page {PAGE}"),
                          bold = FALSE, italic = FALSE, underline = FALSE),
                     c(c = "Draft"))
  expect_identical(.be_render(.be_doc(rtf_header(rows_plain))),
                   .be_render(.be_doc(rtf_header(rows_false))))
})

test_that("a bad bold / italic / underline stops, naming the row", {
  expect_error(
    rtf_header(list(c(l = "a"), list(columns = c(l = "b"), bold = "yes"))),
    "`rtf_header()` row 2's `bold` must be TRUE or FALSE.", fixed = TRUE)
  expect_error(
    rtf_footer(list(list(columns = c(l = "b"), italic = NA))),
    "`rtf_footer()` row 1's `italic` must be TRUE or FALSE.", fixed = TRUE)
  expect_error(
    rtf_header(list(list(columns = c(l = "b"), underline = c(TRUE, FALSE)))),
    "`rtf_header()` row 1's `underline` must be TRUE or FALSE.", fixed = TRUE)
  expect_error(
    rtf_header(list(list(columns = c(l = "b"), bold = 1))),
    "row 1's `bold` must be TRUE or FALSE", fixed = TRUE)
  expect_error(
    .be_render(.be_doc(footnotes = list(
      "a", list(columns = c(l = "b"), bold = "x")))),
    "`rtf_footnotes()` row 2's `bold` must be TRUE or FALSE.", fixed = TRUE)
})

test_that("a footnote row list(columns = ) takes the fields, in both forms", {
  txt <- .be_render(.be_doc(footnotes = list(
    "Note 1",
    list(columns = c(l = "Source: ADSL", r = "Example Pharma"),
         bold = TRUE, underline = TRUE))))
  r <- strsplit(txt, "\\row", fixed = TRUE)[[1L]]
  r1 <- r[grepl("Note 1", r, fixed = TRUE)]
  r2 <- r[grepl("Source: ADSL", r, fixed = TRUE)]
  expect_false(.be_has(r1, "b"))
  expect_true(.be_has(r2, "b") && .be_has(r2, "ul"))
  expect_false(.be_has(r2, "i"))
  expect_true(grepl("\\b \\ul Example Pharma", r2, fixed = TRUE))

  # the paragraph form (footnote_format = "text") writes the emphasis too
  doc <- rtf_document(style = rtf_style(footnote_format = "text")) |>
    rtf_tables(list(.be_df())) |>
    rtf_footnotes(list(list("Note 1",
                            list(columns = "Source: ADSL", italic = TRUE))))
  txt <- .be_render(doc)
  expect_true(grepl("\\i Source: ADSL\\i0 ", txt, fixed = TRUE))
  expect_false(grepl("\\i Note 1", txt, fixed = TRUE))

  # an unknown field still stops, naming what a list row takes
  expect_error(
    .be_render(.be_doc(footnotes = list(list(columns = c(l = "b"), bolder = TRUE)))),
    "takes columns =, row_height_twips =, border =, bold =, italic = and underline =",
    fixed = TRUE)
})

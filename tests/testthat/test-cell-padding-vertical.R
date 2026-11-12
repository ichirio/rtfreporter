# Top / bottom cell padding (#125): written into each cell definition, before
# its \cellx, as Word writes it -- the top as \clpadl<N>\clpadfl3 (Word reads
# \clpadl as the top padding), the bottom as \clpadb<N>\clpadfb3.  0 / NULL
# write nothing.  And a title, footnote or header / footer row's own height.

# rtf_default_format() / default_format = are deprecated (#804): these tests
# check what they still do, not the warning (test-rtf-style.R does).
silence_deprecation("rtf_default_format")

.vpad_df <- function() data.frame(A = 1:2, B = c("x", "y"))

# The rows of a rendered document that carry `text` in a cell.
.vpad_row <- function(txt, text) {
  rows <- regmatches(txt, gregexpr("\\\\trowd.*?\\\\row", txt))[[1L]]
  rows[grepl(text, rows, fixed = TRUE)]
}

.vpad_doc <- function(fmt = rtf_default_format(title_format = "table"),
                      header = rtf_header(c(l = "HDR")),
                      footer = rtf_footer(c(l = "FTR")),
                      tbl = rtftable(.vpad_df())) {
  rtf_document(default_format = fmt) |>
    rtf_section(page = 1, secinfo = list(header = header, footer = footer)) |>
    rtf_tables(list(tbl)) |>
    rtf_titles(list("TTL")) |>
    rtf_footnotes(list("FNT"))
}

test_that("FT-DC226 [FR-DC298 FR-DC299]: the document's top / bottom padding goes into every cell definition; 0 or NULL writes nothing", {
  txt <- .render_to_string(.vpad_doc(rtf_default_format(
    title_format = "table", cell_padding_top_twips = 30L,
    cell_padding_bottom_twips = 45L)))
  pad <- "\\clpadl30\\clpadfl3\\clpadb45\\clpadfb3"
  for (cell in c("HDR", "FTR", "TTL", "FNT", " A\\cell", " x\\cell")) {
    row <- .vpad_row(txt, cell)
    expect_length(row, 1L)
    expect_true(grepl(paste0(pad, "\\cellx"), row, fixed = TRUE), info = cell)
  }
  # every cell of a table row: two columns, two definitions
  expect_identical(lengths(regmatches(.vpad_row(txt, " x\\cell"),
                                      gregexpr("clpadl30", .vpad_row(txt, " x\\cell"),
                                               fixed = TRUE))), 2L)
  # only the side given
  txt_t <- .render_to_string(.vpad_doc(rtf_default_format(
    title_format = "table", cell_padding_top_twips = 30L)))
  expect_true(grepl("\\clpadl30\\clpadfl3\\cellx", txt_t, fixed = TRUE))
  expect_false(grepl("clpadb", txt_t, fixed = TRUE))

  # 0 and NULL: nothing written, the file byte for byte as before
  base <- .render_to_string(.vpad_doc())
  zero <- .render_to_string(.vpad_doc(rtf_default_format(
    title_format = "table", cell_padding_top_twips = 0L,
    cell_padding_bottom_twips = 0L)))
  expect_false(grepl("clpad", base, fixed = TRUE))
  expect_identical(zero, base)

  fmt <- rtf_default_format(cell_padding_top_twips = 30L)
  expect_identical(fmt$cell_padding_top_twips, 30L)
  expect_null(fmt$cell_padding_bottom_twips)
  expect_output(print(fmt), "padding T/B 30/0 twips")
  expect_false(any(grepl("T/B", capture.output(print(rtf_default_format())))))

  expect_error(rtf_default_format(cell_padding_top_twips = -1),
               "`cell_padding_top_twips` must be a single non-negative integer (twips) or NULL.",
               fixed = TRUE)
  expect_error(rtf_default_format(cell_padding_bottom_twips = c(1, 2)),
               "`cell_padding_bottom_twips` must be a single non-negative integer",
               fixed = TRUE)
  expect_error(rtf_default_format(cell_padding_bottom_twips = NA),
               "`cell_padding_bottom_twips`", fixed = TRUE)
})

test_that("FT-DC227 [FR-DC300]: rtf_header() / rtf_footer(cell_padding_top_twips, cell_padding_bottom_twips) pad the band's cells, over the document's", {
  doc <- .vpad_doc(
    rtf_default_format(cell_padding_top_twips = 30L, cell_padding_bottom_twips = 30L),
    header = rtf_header(list(c(l = "HDR", r = "R1")), cell_padding_top_twips = 90L),
    footer = rtf_footer(c(l = "FTR"), cell_padding_bottom_twips = 0L))
  txt <- .render_to_string(doc)
  hdr <- .vpad_row(txt, "HDR")
  # the band's top, the document's bottom; on both cells
  expect_identical(lengths(regmatches(hdr, gregexpr(
    "\\clpadl90\\clpadfl3\\clpadb30\\clpadfb3\\cellx", hdr, fixed = TRUE))), 2L)
  ftr <- .vpad_row(txt, "FTR")
  # the footer's top rule stays, the padding follows it; bottom 0 is nothing
  expect_true(grepl("\\clbrdrt\\brdrs\\brdrw15\\clpadl30\\clpadfl3\\cellx", ftr,
                    fixed = TRUE))
  expect_false(grepl("clpadb", ftr, fixed = TRUE))

  h <- rtf_header(c(l = "x"), cell_padding_top_twips = 12, cell_padding_bottom_twips = 0)
  expect_identical(h$cell_padding_top_twips, 12L)
  expect_identical(h$cell_padding_bottom_twips, 0L)
  expect_error(rtf_header(c(l = "x"), cell_padding_top_twips = -5),
               "`cell_padding_top_twips` must be a single non-negative integer (twips) or NULL.",
               fixed = TRUE)
  expect_error(rtf_footer(c(l = "x"), cell_padding_bottom_twips = "a"),
               "`cell_padding_bottom_twips` must be a single non-negative integer",
               fixed = TRUE)
})

test_that("FT-DC228 [FR-DC301 FR-DC302]: a header / footer row given as a list has its own height, over the band's", {
  doc <- .vpad_doc(
    header = rtf_header(list(c(l = "H1"),
                             list(columns = c(l = "H2", r = "R2"), row_height_twips = 500L),
                             list(columns = c(c = "H3"))),
                        row_height_twips = 300L),
    footer = rtf_footer(list(list(columns = c(l = "F1"), row_height_twips = 0L),
                             c(l = "F2"))))
  txt <- .render_to_string(doc)
  expect_match(.vpad_row(txt, "H1"), "^\\\\trowd\\\\trrh300\\\\")
  expect_match(.vpad_row(txt, "H2"), "^\\\\trowd\\\\trrh500\\\\")
  expect_match(.vpad_row(txt, "H3"), "^\\\\trowd\\\\trrh300\\\\")
  expect_match(.vpad_row(txt, "F1"), "^\\\\trowd\\\\trrh0\\\\")
  expect_match(.vpad_row(txt, "F2"), "^\\\\trowd\\\\trrh230\\\\")

  # a row without a height of its own is written as before
  plain <- .render_to_string(.vpad_doc(
    header = rtf_header(list(c(l = "H1"), c(l = "H2")), row_height_twips = 300L)))
  listed <- .render_to_string(.vpad_doc(
    header = rtf_header(list(c(l = "H1"), list(columns = c(l = "H2"))),
                        row_height_twips = 300L)))
  expect_identical(listed, plain)

  expect_error(rtf_header(list(c(l = "a"), list(columns = c(l = "b"), row_height_twips = -1))),
               "`rtf_header()` row 2's `row_height_twips` must be a single non-negative integer (twips) or NULL.",
               fixed = TRUE)
  expect_error(rtf_footer(list(list(columns = c(l = "b"), row_height_twips = c(1, 2)))),
               "`rtf_footer()` row 1's `row_height_twips`", fixed = TRUE)
})

test_that("FT-DC229 [FR-DC303 FR-DC304]: a title row's row_height_twips is that row's height in the table form", {
  doc <- rtf_document(default_format = rtf_default_format(title_format = "table")) |>
    rtf_tables(list(.vpad_df())) |>
    rtf_titles(list(list("T1", list(text = "T2", row_height_twips = 444L),
                         list(text = "", row_height_twips = 120L),
                         list(text = "T4", bold = FALSE))),
               row_height_twips = 260L)
  txt <- .render_to_string(doc)
  expect_match(.vpad_row(txt, " T1\\b0"), "^\\\\trowd\\\\trrh260\\\\")
  expect_match(.vpad_row(txt, " T2\\b0"), "^\\\\trowd\\\\trrh444\\\\")
  expect_match(.vpad_row(txt, " T4\\cell"), "^\\\\trowd\\\\trrh260\\\\")
  # the blank row too
  expect_true(any(grepl("^\\\\trowd\\\\trrh120\\\\", regmatches(
    txt, gregexpr("\\\\trowd.*?\\\\row", txt))[[1L]])))

  bad <- rtf_document() |> rtf_tables(list(.vpad_df())) |>
    rtf_titles(list(list("T1", list(text = "T2", row_height_twips = -3))))
  expect_error(.render_to_string(bad),
               "`rtf_titles()` row 2's `row_height_twips` must be a single non-negative integer (twips) or NULL.",
               fixed = TRUE)
})

test_that("FT-DC230 [FR-DC305 FR-DC306]: a footnote row given as list(columns = , row_height_twips = ) has its own height", {
  doc <- rtf_document() |>
    rtf_tables(list(.vpad_df())) |>
    rtf_footnotes(list(list("F1",
                            list(columns = "F2", row_height_twips = 555L),
                            list(columns = c(l = "F3", r = "R3"), row_height_twips = 333L),
                            list(columns = "F4"))))
  txt <- .render_to_string(doc)
  expect_match(.vpad_row(txt, "F1"), "^\\\\trowd\\\\trrh230\\\\")
  expect_match(.vpad_row(txt, "F2"), "^\\\\trowd\\\\trrh555\\\\")
  f3 <- .vpad_row(txt, "F3")
  expect_match(f3, "^\\\\trowd\\\\trrh333\\\\")
  expect_match(f3, "\\\\qr\\\\li0\\\\ri0 R3\\\\cell")
  expect_match(.vpad_row(txt, "F4"), "^\\\\trowd\\\\trrh230\\\\")

  # the paragraph form has no rows: the text is kept, the height has no place
  txt_p <- .render_to_string(rtf_document(
    default_format = rtf_default_format(footnote_format = "text")) |>
      rtf_tables(list(.vpad_df())) |>
      rtf_footnotes(list(list(list(columns = "F2", row_height_twips = 555L)))))
  expect_match(txt_p, "\\\\pard\\\\ql\\\\li0\\\\ri0 F2\\\\par")

  .fn_err <- function(row) {
    .render_to_string(rtf_document() |> rtf_tables(list(.vpad_df())) |>
                        rtf_footnotes(list(list("F1", row))))
  }
  expect_error(.fn_err(list(columns = "F2", row_height_twips = -1)),
               "`rtf_footnotes()` row 2's `row_height_twips` must be a single non-negative integer (twips) or NULL.",
               fixed = TRUE)
  expect_error(.fn_err(list(columns = "F2", colour = "red")),
               "`rtf_footnotes()` row 2: a row given as a list takes columns =, row_height_twips =, border =, bold =, italic = and underline =; got 'colour'.",
               fixed = TRUE)
  # a list without columns is the old styled row, still refused
  expect_error(.fn_err(list(text = "F2")), "Per-row styling lists are no longer supported",
               fixed = TRUE)
})

test_that("FT-TB695 [FR-TB544 FR-TB545 FR-TB546 FR-PL427]: rtf_table_style(), rtftable() and plan_style() pad the table's header and data cells", {
  st <- rtf_table_style(cell_padding_top_twips = 20L, cell_padding_bottom_twips = 25L)
  expect_identical(st$cell_padding_top_twips, 20L)
  expect_output(print(st), "cell_padding : T=20 B=25")
  expect_false(any(grepl("T=", capture.output(print(rtf_table_style())))))
  expect_identical(rtf_table_style_with(st, cell_padding_top_twips = 5L)$cell_padding_top_twips,
                   5L)

  df <- data.frame(A = c("a", "b"), B = c("1.5", "22.25"))
  tbl <- rtftable(df, style = st,
                  col_header = list(list(col_cell(1:2, "AB")), c("A", "B")),
                  blank_rows = 1L)
  txt <- .render_to_string(rtf_document() |> rtf_tables(list(tbl)))
  pad <- "\\clvertalb\\clpadl20\\clpadfl3\\clpadb25\\clpadfb3\\cellx"
  rows <- regmatches(txt, gregexpr("\\\\trowd.*?\\\\row", txt))[[1L]]
  body <- rows[grepl("cellx", rows) & !grepl("\\trrh0", rows, fixed = TRUE)]
  # spanning row (one cell), label row and data rows (two cells each)
  n_pad <- vapply(rows, function(r) lengths(regmatches(r, gregexpr(pad, r, fixed = TRUE))),
                  integer(1L), USE.NAMES = FALSE)
  n_cell <- vapply(rows, function(r) lengths(regmatches(r, gregexpr("\\cellx", r, fixed = TRUE))),
                   integer(1L), USE.NAMES = FALSE)
  blank <- grepl("\\trgaph0\\trleft0", rows, fixed = TRUE)
  expect_identical(n_pad[!blank & n_cell > 0L], n_cell[!blank & n_cell > 0L])
  expect_true(any(blank))
  expect_false(any(grepl("clpad", rows[blank], fixed = TRUE)))   # the spacer stays bare
  expect_gt(length(body), 3L)

  # an explicit rtftable() value wins over the style's
  tbl2 <- rtftable(df, style = st, cell_padding_top_twips = 0L)
  txt2 <- .render_to_string(rtf_document() |> rtf_tables(list(tbl2)))
  expect_false(grepl("clpadl", txt2, fixed = TRUE))
  expect_true(grepl("\\clvertalb\\clpadb25\\clpadfb3\\cellx", txt2, fixed = TRUE))

  # decimal-split halves are padded like any cell
  dtxt <- .render_to_string(rtf_document() |> rtf_tables(list(
    set_decimal_split(rtftable(df, cell_padding_bottom_twips = 15L), "B"))))
  drow <- .vpad_row(dtxt, "22")
  expect_identical(lengths(regmatches(drow, gregexpr("\\clpadb15\\clpadfb3\\cellx", drow,
                                                     fixed = TRUE))),
                   lengths(regmatches(drow, gregexpr("\\cellx", drow, fixed = TRUE))))

  # plan_style() passes them to the pages
  pages <- table_plan(df) |>
    plan_style(cell_padding_top_twips = 40L, cell_padding_bottom_twips = 10L) |>
    plan_apply()
  expect_identical(pages[[1L]]$cell_padding_top_twips, 40L)
  expect_identical(pages[[1L]]$cell_padding_bottom_twips, 10L)

  expect_error(rtftable(df, cell_padding_top_twips = -1),
               "`cell_padding_top_twips` must be a single non-negative integer (twips) or NULL.",
               fixed = TRUE)
  expect_error(rtf_table_style(cell_padding_bottom_twips = "x"),
               "`cell_padding_bottom_twips` must be a single non-negative integer",
               fixed = TRUE)
})

test_that("FT-IX026 [FR-IX395]: top / bottom padding: the module's own value wins over the style's, the document default, the option and the baseline", {
  df <- .vpad_df()
  old <- options(rtfreporter.cell_padding_top_twips = 11L,
                 rtfreporter.cell_padding_bottom_twips = 12L)
  on.exit(options(old), add = TRUE)
  # the option, under nothing else
  txt <- .render_to_string(rtf_document() |> rtf_tables(list(rtftable(df))))
  expect_true(grepl("\\clpadl11\\clpadfl3\\clpadb12\\clpadfb3\\cellx", txt, fixed = TRUE))
  # the document default over the option
  fmt <- rtf_default_format(cell_padding_top_twips = 21L, cell_padding_bottom_twips = 22L)
  txt <- .render_to_string(rtf_document(default_format = fmt) |>
                             rtf_tables(list(rtftable(df))))
  expect_true(grepl("\\clpadl21\\clpadfl3\\clpadb22\\clpadfb3\\cellx", txt, fixed = TRUE))
  expect_false(grepl("clpadl11", txt, fixed = TRUE))
  # a style over the document; the table's own over the style
  st <- rtf_table_style(cell_padding_top_twips = 31L)
  txt <- .render_to_string(rtf_document(default_format = fmt) |>
                             rtf_tables(list(rtftable(df, style = st))))
  expect_true(grepl("\\clpadl31\\clpadfl3\\clpadb22\\clpadfb3\\cellx", txt, fixed = TRUE))
  txt <- .render_to_string(rtf_document(default_format = fmt) |>
                             rtf_tables(list(rtftable(df, style = st,
                                                      cell_padding_top_twips = 41L))))
  expect_true(grepl("\\clpadl41\\clpadfl3\\clpadb22\\clpadfb3\\cellx", txt, fixed = TRUE))
  # a band's own over the document
  txt <- .render_to_string(rtf_document(default_format = fmt) |>
    rtf_section(page = 1, secinfo = list(
      header = rtf_header(c(l = "HDR"), cell_padding_bottom_twips = 52L),
      footer = NULL)) |>
    rtf_tables(list(rtftable(df))))
  expect_true(grepl("\\clpadl21\\clpadfl3\\clpadb52\\clpadfb3\\cellx",
                    .vpad_row(txt, "HDR"), fixed = TRUE))
})

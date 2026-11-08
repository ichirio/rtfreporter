# The table area's leaf requirements that had no test (the deviations of
# the first validation version): each test names its FT id and its FRs.

test_that("FT-TB678 [FR-TB303 FR-TB314 FR-TB309]: set_blank_rows() refuses what is not a data frame and a group_col that is not there; a plain list of forms is unioned", {
  d <- data.frame(a = c("x", "y", "z"), b = 1:3)
  expect_error(set_blank_rows(list(a = 1)), "`data` must be a data.frame (or tibble).", fixed = TRUE)
  expect_error(set_blank_rows(d, blank_rows = "between_groups", group_col = "nope"),
               "`group_col` 'nope' not found in the table.", fixed = TRUE)
  expect_error(set_blank_rows(d, blank_rows = "between_groups", group_col = 9),
               "`group_col` index 9 out of range (1..2).", fixed = TRUE)
  # every entry's positions, duplicates dropped
  out <- set_blank_rows(d, blank_rows = list(1L, c(1L, 2L)))
  expect_identical(attr(out, "rtf_blank_rows"), c(1L, 2L))
})

test_that("FT-TB679 [FR-TB322 FR-TB323]: blank_rows_by_change(group_by = \"filled\") starts a group at a non-empty cell; \"auto\" picks the detection from the content", {
  e <- data.frame(l = c("G1", NA, "", "G2", ""), v = 1:5, stringsAsFactors = FALSE)
  filled <- set_blank_rows(e, blank_rows = blank_rows_by_change("l", group_by = "filled"))
  # NA and "" are members of the group above: one blank, before G2
  expect_identical(attr(filled, "rtf_blank_rows"), 3L)
  auto <- set_blank_rows(e, blank_rows = blank_rows_by_change("l", group_by = "auto"))
  expect_identical(attr(auto, "rtf_blank_rows"), 3L)
  # on a column of plain values, "auto" reads it as "value"
  v <- data.frame(l = c("A", "A", "B", "B"), v = 1:4, stringsAsFactors = FALSE)
  expect_identical(attr(set_blank_rows(v, blank_rows = blank_rows_by_change("l", group_by = "auto")),
                        "rtf_blank_rows"),
                   attr(set_blank_rows(v, blank_rows = blank_rows_by_change("l", group_by = "value")),
                        "rtf_blank_rows"))
})

test_that("FT-TB680 [FR-TB336 FR-TB337 FR-TB340]: blank_rows_by_rule(where = \"after\") puts the blank after the match; where is matched; a rule never falls on the first or last row", {
  f <- data.frame(l = c("Total", "a", "Total", "b"), v = 1:4, stringsAsFactors = FALSE)
  after <- set_blank_rows(f, blank_rows = blank_rows_by_rule("l", "^Total", where = "after"))
  expect_identical(attr(after, "rtf_blank_rows"), c(1L, 3L))
  expect_error(blank_rows_by_rule("l", "^Total", where = "between"), "should be one of")
  # a separator: "before" a match on row 1 and "after" a match on the last row are dropped
  before <- set_blank_rows(f, blank_rows = blank_rows_by_rule("l", "^Total", where = "before"))
  expect_identical(attr(before, "rtf_blank_rows"), 2L)
  g <- data.frame(l = c("a", "Total", "b", "Total"), v = 1:4, stringsAsFactors = FALSE)
  last <- set_blank_rows(g, blank_rows = blank_rows_by_rule("l", "^Total", where = "after"))
  expect_identical(attr(last, "rtf_blank_rows"), 2L)
  p <- as_rtftables(g, blank_rows = blank_rows_by_rule("l", "^Total", where = "after"))[[1L]]
  expect_false(nrow(g) %in% p$blank_rows)
})

test_that("FT-TB681 [FR-TB368 FR-TB373]: auto_col_widths() refuses what is not a data frame; col_padding_twips is added to every measured width", {
  expect_error(auto_col_widths(list(a = 1)), "`data` must be a data.frame.", fixed = TRUE)
  d <- data.frame(a = strrep("x", 40), b = strrep("y", 60), stringsAsFactors = FALSE)
  w0 <- auto_col_widths(d, col_padding_twips = 0L)
  w <- auto_col_widths(d)
  expect_identical(as.integer(w - w0), c(288L, 288L))
})

test_that("FT-TB682 [FR-TB377 FR-TB379 FR-TB380 FR-TB382 FR-TB385 FR-TB386]: default_col_widths(): its guards, col_header, digits, cells read as text and a table with no data column", {
  d <- data.frame(a = c("x", "y"), b = 1:2, c = c("p", "q"), stringsAsFactors = FALSE)
  expect_error(default_col_widths(list(a = 1)), "`data` must be a data.frame.", fixed = TRUE)
  expect_error(default_col_widths(d, row_title = "x"),
               "`row_title` column(s) not found in data: x", fixed = TRUE)
  expect_error(default_col_widths(d, row_title = 9),
               "`row_title` indices must be within 1..3.", fixed = TRUE)
  expect_error(default_col_widths(d, row_title = TRUE),
               "`row_title` must be NULL, an integer vector, or column names.", fixed = TRUE)
  # col_header: a long label widens its column; a header counts by its longest word,
  # and of a list of rows only the last is counted
  # (in a narrow table, where a column's need decides its share)
  k <- data.frame(a = "l", b = "x", c = "y", stringsAsFactors = FALSE)
  long <- default_col_widths(k, col_header = c("a", "Treatment_group_label_long", "c"), width_chars = 30)
  expect_gt(long[["b"]], default_col_widths(k, width_chars = 30)[["b"]])
  # many short words count as one: as wide as the one-word header
  expect_identical(default_col_widths(k, col_header = c("a", "Treat group label long xx", "c"),
                                      width_chars = 30),
                   default_col_widths(k, col_header = c("a", "Treat", "c"), width_chars = 30))
  expect_identical(default_col_widths(k, col_header = list(c("x", "Treatment_group_label_long", "x"),
                                                           c("a", "b", "c")), width_chars = 30),
                   default_col_widths(k, col_header = c("a", "b", "c"), width_chars = 30))
  # digits: the decimals the proportions are rounded to
  e <- data.frame(a = "label", b = strrep("x", 7), c = strrep("y", 11), stringsAsFactors = FALSE)
  w1 <- default_col_widths(e)
  w3 <- default_col_widths(e, digits = 3L)
  expect_identical(w1, round(w1, 1))
  expect_identical(w3, round(w3, 3))
  # every cell is text, NA is empty, a multi-line cell is its longest line
  expect_identical(default_col_widths(data.frame(a = "l", b = c(NA, "abc"), stringsAsFactors = FALSE)),
                   default_col_widths(data.frame(a = "l", b = c("", "abc"), stringsAsFactors = FALSE)))
  expect_identical(default_col_widths(data.frame(a = "l", b = "abc\nx", stringsAsFactors = FALSE)),
                   default_col_widths(data.frame(a = "l", b = "abc", stringsAsFactors = FALSE)))
  expect_identical(default_col_widths(data.frame(a = "l", b = 123)),
                   default_col_widths(data.frame(a = "l", b = "123", stringsAsFactors = FALSE)))
  # no data column apart from the row headings, or an empty row_title: equal widths
  expect_identical(unname(default_col_widths(data.frame(a = c("x", "y")), row_title = 1)), 1)
  expect_identical(unname(default_col_widths(d, row_title = integer(0))), c(1, 1, 1))
})

test_that("FT-TB683 [FR-TB399 FR-TB400 FR-TB402 FR-TB404 FR-TB407 FR-TB409]: style_body(): every column by default, colour, and its guards", {
  d <- data.frame(a = c("x", "y", "z"), b = 1:3, c = c("p", "q", "r"), stringsAsFactors = FALSE)
  t <- rtftable(d)
  s <- style_body(t, rows = 1, bold = TRUE)
  expect_identical(s$cell_styles[[1L]]$bold, c(TRUE, TRUE, TRUE))
  # names and positions mixed with a list()
  m <- style_body(t, rows = 1, cols = list("a", 3L), bold = TRUE)
  expect_identical(m$cell_styles[[1L]]$bold, c(TRUE, NA, TRUE))
  col <- style_body(t, rows = 2, color = "#FF0000")
  expect_identical(col$cell_styles[[2L]]$color, rep("#FF0000", 3L))
  expect_match(.render_to_string(.test_doc_with(col)), "\\red255\\green0\\blue0", fixed = TRUE)
  expect_error(style_body(t, cols = "x"), "`style_body(cols)` column 'x' not found in the table.", fixed = TRUE)
  expect_error(style_body(t, cols = 9), "`style_body(cols)` index 9 out of range (1..3).", fixed = TRUE)
  for (a in c("bold", "italic", "underline")) {
    args <- list(t)
    args[[a]] <- "yes"
    expect_error(do.call(style_body, args), sprintf("`style_body(%s = )` must be TRUE or FALSE.", a),
                 fixed = TRUE)
  }
  expect_error(style_body(t, align = "middle"),
               "`style_body(align = )` must be \"left\", \"center\", or \"right\".", fixed = TRUE)
  expect_error(style_body(t, border = "single"),
               "`style_body(border = )` must be an rtf_border() object.", fixed = TRUE)
})

test_that("FT-TB684 [FR-TB419]: style_cols() refuses a flag that is not TRUE or FALSE, naming the argument", {
  t <- rtftable(data.frame(a = 1, b = 2))
  for (a in c("bold", "italic", "underline", "header_bold", "header_italic")) {
    args <- list(t, cols = 1L)
    args[[a]] <- "yes"
    expect_error(do.call(style_cols, args), sprintf("`style_cols(%s = )` must be TRUE or FALSE.", a),
                 fixed = TRUE)
  }
})

test_that("FT-TB685 [FR-TB441 FR-TB468 FR-TB470]: style_zone() and .rtf_table_border() refuse a zone that is not a border", {
  t <- rtftable(data.frame(a = 1, b = 2))
  expect_error(style_zone(t, header = "single"),
               "`style_zone(header = )` must be an rtf_border() object.", fixed = TRUE)
  expect_error(style_zone(t, body = "single"),
               "`style_zone(body = )` must be an rtf_border() object.", fixed = TRUE)
  expect_error(rtfreporter:::.rtf_table_border(outer = "single"),
               "`outer` must be NULL or an rtf_border object (the table's four outermost edges).",
               fixed = TRUE)
  expect_error(rtfreporter:::.rtf_table_border(inside_h = rtf_border(top = TRUE)),
               "`inside_h` must be NULL or an rtf_border_line object.", fixed = TRUE)
})

test_that("FT-TB686 [FR-TB485 FR-TB488 FR-TB505 FR-TB508]: the border shorthand .rtf_border_tfl() stops with rtf_border_line()'s message on a bad style or width", {
  f <- rtfreporter:::.rtf_border_tfl
  expect_error(f(style = "nope"), "should be one of")
  expect_error(f(width = 0), "`width` must be a positive integer (twips).",
               fixed = TRUE)
})

test_that("FT-TB687 [FR-TB453]: rtf_table_style(row_height_twips = ) is the row height; NULL is the font-aware default", {
  expect_identical(rtf_table_style(row_height_twips = 400)$row_height_twips, 400L)
  expect_null(rtf_table_style()$row_height_twips)
  d <- data.frame(a = 1, b = 2)
  s <- .render_to_string(.test_doc_with(rtftable(d, style = rtf_table_style(row_height_twips = 400))))
  expect_match(s, "\\trrh400", fixed = TRUE)
})

test_that("FT-TB688 [FR-TB165]: as_rtftables(border = , style = ) reach every page; the default border is the tfl preset", {
  d <- data.frame(a = letters[1:6], b = 1:6, stringsAsFactors = FALSE)  # six groups of one row
  def <- as_rtftables(d, max_rows = 3, split = "group_force")
  expect_length(def, 2L)
  tfl <- rtftable(d[1:3, ], border = "tfl")
  for (p in def) expect_identical(p$border, tfl$border)
  none <- as_rtftables(d, max_rows = 3, split = "group_force", border = "none")
  for (p in none) expect_identical(p$border, rtftable(d[1:3, ], border = "none")$border)
  sty <- rtf_table_style(row_height_twips = 400)
  for (p in as_rtftables(d, max_rows = 3, split = "group_force", style = sty)) {
    expect_match(.render_to_string(.test_doc_with(p)), "\\trrh400", fixed = TRUE)
  }
})

test_that("FT-TB689 [FR-TB206]: stub_cols() reads the hierarchy values as character, an NA as empty", {
  d <- data.frame(g = factor(c("A", "A", "B")), l = c(1, NA, 3), v = 1:3)
  out <- stub_cols(d, vars = c("g", "l"))
  expect_type(out[[1L]], "character")
  lab <- trimws(out[[1L]], whitespace = "[\\h\\v]")   # the indent is non-breaking spaces
  expect_true(all(c("A", "B", "1", "3") %in% lab))
  expect_false(any(lab == "NA", na.rm = TRUE))
  # the same table from character columns
  ch <- data.frame(g = c("A", "A", "B"), l = c("1", "", "3"), v = 1:3, stringsAsFactors = FALSE)
  expect_identical(out[[1L]], stub_cols(ch, vars = c("g", "l"))[[1L]])
})

test_that("FT-TB690 [FR-TB236]: add_header_row() refuses an unnamed label row of the wrong length, and a .position that is not top or bottom", {
  t <- rtftable(data.frame(a = 1, b = 2, c = 3))
  expect_error(add_header_row(t, c("A", "B")),
               "`col_header` row 1 has 2 labels but the table has 3 columns", fixed = TRUE)
  expect_error(add_header_row(t, c("A", "B", "C"), .position = "middle"), "should be one of")
})

test_that("FT-IX025 [FR-IX340]: rtf_border(all = , top = ): the side named explicitly wins over all, whichever order they are written in", {
  a <- rtf_border(all = rtf_border_line("double"), top = rtf_border_line("dot"))
  b <- rtf_border(top = rtf_border_line("dot"), all = rtf_border_line("double"))
  expect_identical(a, b)
  expect_identical(a$top$style, "dot")
  for (side in c("bottom", "left", "right")) expect_identical(a[[side]]$style, "double")
})

# The table rows of a one-table document, as written.
.vg_body <- function(tbl) {
  doc <- rtf_document() |> rtf_section(page = 1) |> rtf_tables(list(tbl))
  s <- .render_to_string(doc)
  regmatches(s, gregexpr("\\\\trowd[^\n]*?\\\\row", s))[[1L]]
}
.vg_df <- function() data.frame(A = c("a1", "a2"), B = c("b1", "b2"), C = c("c1", "c2"))

test_that("FT-TB696 [FR-TB547 FR-TB548 FR-TB549]: rtftable() row_title, table_width_pct_of_writable and table_align", {
  # row headings are left-aligned, the other columns centred
  expect_true(grepl("\\ql\\li0\\ri0 a1\\cell\\qc\\li0\\ri0 b1\\cell\\qc",
                    .vg_body(rtftable(.vg_df()))[2L], fixed = TRUE))
  expect_true(grepl("\\ql\\li0\\ri0 a1\\cell\\ql\\li0\\ri0 b1\\cell\\qc",
                    .vg_body(rtftable(.vg_df(), row_title = c(1, 2)))[2L], fixed = TRUE))
  expect_true(grepl("\\ql\\li0\\ri0 a1\\cell\\ql\\li0\\ri0 b1\\cell\\qc",
                    .vg_body(rtftable(.vg_df(), row_title = c("A", "B")))[2L], fixed = TRUE))
  # half the writable width (13680 twips)
  expect_true(grepl("\\cellx6840\\ql", .vg_body(rtftable(.vg_df(), table_width_pct_of_writable = 0.5))[2L],
                    fixed = TRUE))
  expect_true(grepl("\\cellx13680\\ql", .vg_body(rtftable(.vg_df()))[2L], fixed = TRUE))
  # placement
  expect_false(any(grepl("\\\\trq[lcr]", .vg_body(rtftable(.vg_df())))))
  expect_true(all(grepl("\\trqc", .vg_body(rtftable(.vg_df(), table_align = "center")), fixed = TRUE)))
  expect_true(all(grepl("\\trqr", .vg_body(rtftable(.vg_df(), table_align = "right")), fixed = TRUE)))
  expect_error(rtftable(.vg_df(), table_align = "middle"),
               "`table_align` must be 'left', 'center', or 'right'.", fixed = TRUE)
})

test_that("FT-TB697 [FR-TB550 FR-TB551 FR-TB558]: rtftable() font size, font and markup are the table's own", {
  r <- .vg_body(rtftable(.vg_df(), font_size_half_points = 24L))[2L]
  expect_true(grepl("\\trrh290", r, fixed = TRUE))
  expect_true(grepl("\\fs24 a1\\cell", r, fixed = TRUE))
  expect_false(grepl("\\fs", .vg_body(rtftable(.vg_df()))[2L], fixed = TRUE))
  # a family not in the font table is added to it
  doc <- rtf_document() |> rtf_section(page = 1) |> rtf_tables(list(rtftable(.vg_df(), font = "Arial")))
  s <- .render_to_string(doc)
  expect_true(grepl("{\\f1\\fnil\\fcharset0 Arial;}", s, fixed = TRUE))
  expect_true(grepl("\\f1 a1\\cell", .vg_body(rtftable(.vg_df(), font = "Arial"))[2L], fixed = TRUE))
  # markup
  d <- data.frame(A = "x >= 1", B = "y^{2}")
  expect_true(grepl("x >= 1\\cell\\qc\\li0\\ri0 y{\\super 2}\\cell", .vg_body(rtftable(d))[2L], fixed = TRUE))
  expect_true(grepl("x \\u8805? 1\\cell\\qc\\li0\\ri0 y^\\{2\\}\\cell",
                    .vg_body(rtftable(d, markup = "relational"))[2L], fixed = TRUE))
  expect_true(grepl("x >= 1\\cell\\qc\\li0\\ri0 y^\\{2\\}\\cell",
                    .vg_body(rtftable(d, markup = "none"))[2L], fixed = TRUE))
})

test_that("FT-TB698 [FR-TB552 FR-TB553 FR-TB554 FR-TB555]: rtftable() exact row heights, header and blank row heights, and cell_valign", {
  expect_true(all(startsWith(.vg_body(rtftable(.vg_df(), row_height_exact = TRUE)), "\\trowd\\trrh-230")))
  expect_true(all(startsWith(.vg_body(rtftable(.vg_df())), "\\trowd\\trrh230")))
  r <- .vg_body(rtftable(.vg_df(), header_row_height_twips = 400L))
  expect_true(startsWith(r[1L], "\\trowd\\trrh400"))
  expect_true(startsWith(r[2L], "\\trowd\\trrh230"))
  r <- .vg_body(rtftable(.vg_df(), blank_rows = 1L, blank_row_height_twips = 111L))
  expect_identical(r[3L], "\\trowd\\trgaph0\\trleft0\\trrh111\\clvertalb\\cellx13680 \\cell\\row")
  expect_true(startsWith(r[2L], "\\trowd\\trrh230"))
  r <- .vg_body(rtftable(.vg_df(), blank_rows = 1L))
  expect_true(startsWith(r[3L], "\\trowd\\trgaph0\\trleft0\\trrh230"))
  # vertical alignment
  expect_true(grepl("\\clvertalb\\cellx", .vg_body(rtftable(.vg_df()))[2L], fixed = TRUE))
  expect_true(grepl("\\clvertalt\\cellx", .vg_body(rtftable(.vg_df(), cell_valign = "top"))[2L], fixed = TRUE))
  expect_true(grepl("\\clvertalc\\cellx", .vg_body(rtftable(.vg_df(), cell_valign = "center"))[2L], fixed = TRUE))
  expect_error(rtftable(.vg_df(), cell_valign = "middle"),
               "`cell_valign` must be 'top', 'center', or 'bottom'.", fixed = TRUE)
})

test_that("FT-TB699 [FR-TB556 FR-TB557]: rtftable() cell_styles override single cells; blank_row_normalize detects and collapses blank rows", {
  r <- .vg_body(rtftable(.vg_df(), cell_styles = list(list(bold = c(NA, TRUE, NA)), NULL)))
  expect_true(grepl("a1\\cell\\qc\\li0\\ri0 \\b b1\\b0 \\cell\\qc\\li0\\ri0 c1\\cell", r[2L], fixed = TRUE))
  expect_false(grepl("\\b ", r[3L], fixed = TRUE))
  # an empty data row after a separator: detected, then the two collapsed
  d <- data.frame(A = c("a1", "", "a3"), B = c("b1", NA, "b3"))
  blank <- "\\trowd\\trgaph0\\trleft0\\trrh230\\clvertalb\\cellx13680 \\cell\\row"
  both <- .vg_body(rtftable(d, blank_rows = 1L))
  expect_length(both, 4L)
  expect_identical(both[3L], blank)
  detect <- .vg_body(rtftable(d, blank_rows = 1L, blank_row_normalize = "detect"))
  expect_identical(detect[3:4], c(blank, blank))
  none <- .vg_body(rtftable(d, blank_rows = 1L, blank_row_normalize = "none"))
  expect_length(none, 5L)
  expect_true(grepl("\\ql\\li0\\ri0 \\cell\\qc\\li0\\ri0 \\cell\\row", none[4L], fixed = TRUE))
})

test_that("FT-TB700 [FR-TB559]: options(rtfreporter.print_ascii = TRUE) prints an rtftable with ASCII rules", {
  t <- rtftable(data.frame(A = c("a1", "a2"), B = c("b1", "b2")))
  old <- options(rtfreporter.print_ascii = TRUE)
  on.exit(options(old), add = TRUE)
  out <- capture.output(print(t))
  expect_identical(out[1L], "------")
  expect_true(all(!grepl("[^ -~]", out)))
  options(rtfreporter.print_ascii = FALSE)
  out <- capture.output(print(t))
  if (isTRUE(l10n_info()[["UTF-8"]]) || identical(l10n_info()[["codepage"]], 65001L)) {
    expect_identical(out[1L], strrep("─", 6L))
  } else {
    expect_identical(out[1L], "------")
  }
})

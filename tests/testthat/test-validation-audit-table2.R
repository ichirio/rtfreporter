# Help-to-FR audit, batch 3: as_rtftable(), rtf_table_style() and its two
# helpers, the post-hoc verbs of R/style_verbs.R, stub_cols(), stub_spec(),
# and the three as_rtftables() "What is carried" claims batch 2 left.
#
# The help is the functional requirement: every statement in a function's Rd
# is an active FR in inst/validation/requirements.csv, and every FR has a
# test.  For each function below, one row per claim of its roxygen: the
# claim (short), the FR that states it, and the test that checks it.  The
# tests below this table close the gaps the audit found.  A test written
# before the audit and mapped by hand in inst/validation/test_map.csv is
# named by its FT id.
#
# as_rtftable()
#   claim                                                   | FR          | test
#   one rtftable: as_rtftables(split = "none")[[1]]         | FR-TB101    | FT-TB027, FT-TB755
#   x: gt, gtsummary, rtables, rlistings, flextable, hux, df| FR-TB102    | FT-TB161, FT-TB195
#   a gt_group / tbl_split of one table; of more an error   | FR-TB103-105| FT-TB192, FT-TB191, FT-TB200
#   anything else an error                                  | FR-TB106    | FT-TB198
#   the rendered body; what is carried (as as_rtftables())  | FR-TB114, FR-TB577-582 | FT-TB002, FT-TB727 to FT-TB752
#   not carried: fills, fonts, gt theme borders, Markdown   | FR-TB584, FR-TB578 | FT-TB756, FT-TB771, FT-TB737
#   read_meta                                               | FR-TB107    | FT-TB189
#   ...: forwarded; explicit values win; the other settings | FR-TB108/109| FT-TB190, FT-TB149
#   the example                                             | FR-TB101    | FT-TB755
#
# as_rtftables(), the claims batch 2 left
#   a gt grouped by several columns: a row per combination | FR-TB585    | FT-TB770
#   gt::html() label read as its text, <br> a line break    | FR-TB584    | FT-TB771
#   group_col "Naming it": gt ids, V1, V2, ... for the rest | FR-TB586    | FT-TB757
#
# rtf_table_style()
#   a record of defaults; a snapshot at construction        | FR-TB445    | FT-TB656
#   border_*: the five zones; not an rtf_border an error    | FR-TB446/447| FT-TB640, FT-TB657
#   header_align NULL inherits; header_bold / italic        | FR-TB448/449| FT-TB649, FT-TB663
#   align NULL per column; bold / italic / underline        | FR-TB450/451| FT-TB644
#   cell padding left / right; top / bottom                 | FR-TB452, FR-TB544 | FT-TB575, FT-TB695
#   row_height_twips                                        | FR-TB453    | FT-TB687
#   align / header_align and the twips checked (fixed here) | FR-TB587    | FT-TB758
#   @return; the example                                    | FR-TB445/446| FT-TB759
#
# rtf_table_style_with()
#   a copy with fields replaced; the original unchanged     | FR-TB460/461| FT-TB660
#   not a style / an unknown field an error                 | FR-TB462/463| FT-TB658, FT-TB659
#   values checked as rtf_table_style(); NULL the default;  | FR-TB588    | FT-TB760
#     unnamed fields an error                               |             |
#   the example                                             | FR-TB460    | FT-TB761
#
# rtf_table_style_tfl()
#   rules on the header block only; no bold; fresh object   | FR-TB456/457| FT-TB598, FT-TB593
#   spanning cells auto-underline                           | FR-TB458    | FT-TB594
#   to change a field, derive with _with(); the examples    | FR-TB459    | FT-TB660, FT-TB761
#
# style_header() / style_cols() / style_body() / style_zone() / add_header_row()
#   a modified copy; a list of pages styled page by page    | FR-TB394/414/425/439/442 | FT-TB619, FT-TB629, FT-TB631, FT-TB636, FT-TB428
#   last writer wins, per side; NULL keeps, "none" erases  | FR-IX332    | FT-TB632, FT-TB636
#   Body cells: style_body() over col_spec; border on zone | FR-TB401-408, FR-IX331 | FT-TB622, FT-TB626
#   Body cells: no per-row height or padding                | FR-TB589    | FT-TB763
#   Header rows: label-row styling shared; promotion        | FR-TB433/434, FR-IX333 | FT-TB614, FT-TB615
#   row / cols (intersects) / label; guards                 | FR-TB426-429, FR-TB435 | FT-TB634, FT-TB512, FT-TB633
#   cols: help pointed to a section as_rtftables() does not | FR-TB428    | FT-TB634
#     have (help fixed: rtf_columns(), group_col)           |             |
#   border / align / bold / italic / underline; guards      | FR-TB430-432| FT-TB631, FT-TB614, FT-TB512
#   style_cols: align, flags, indent, colour, fills, border | FR-TB415-423| FT-TB629, FT-TB630, FT-TB478
#   style_body: rows / cols / flags / background; guards    | FR-TB395-413| FT-TB624, FT-TB509, FT-TB683
#   style_zone: the zones; guards                           | FR-TB440/441| FT-TB636, FT-TB685
#   add_header_row: the row; .position; no header           | FR-TB231-234| FT-TB426, FT-TB425
#   add_header_row: a wrong-length row / .position an error | FR-TB236    | FT-TB764
#   the examples                                            | FR-TB425/430/439/233 | FT-TB762
#
# set_col_header()
#   final-table coordinates; rows; nothing clears           | FR-TB212-214| FT-TB343, FT-TB347, FT-TB348
#   a fully named row is a patch; a partly named row        | FR-TB215    | FT-TB126, FT-TB765
#   col_cell pos; unknown names; not cells                  | FR-TB216-219| FT-TB347, FT-TB345, FT-TB096
#   align; guards                                           | FR-TB220/221| FT-TB349, FT-DC043
#   values / by; guards                                     | FR-TB222-226| FT-TB081, FT-TB082, FT-TB083
#   a list of pages; multi-DF; after paginate_cols()        | FR-TB227, FR-IX325 | FT-TB346, FT-TB274
#   the example                                             | FR-TB212    | FT-TB765
#
# set_decimal_split()
#   two cells, before / after the separator; RTF only       | FR-TB341/363/364 | FT-TB547, FT-TB541, FT-TB551
#   x a table or a list of pages                            | FR-TB342/343| FT-TB556, FT-TB558
#   cols: required, NULL clears (help fixed)                | FR-TB344-347| FT-TB559, FT-TB561, FT-TB766
#   ratio / decimal_mark / pad / min / max / compound       | FR-TB349-358| FT-TB548, FT-TB538, FT-TB522
#   which cells are split; spaces; per table                | FR-TB359-362| FT-TB538, FT-TB552, FT-TB554, FT-TB539
#   the example                                             | FR-TB363    | FT-TB766
#
# rtf_columns()
#   the final names; a list's first page; guard             | FR-DC287-289| FT-DC043, FT-DC193
#   the example                                             | FR-DC287    | FT-TB765
#
# stub_cols()
#   one indented stub column, label rows, the rest after    | FR-TB193    | FT-TB393
#   downstream: auto indent, (Cont.), between_groups        | FR-TB590    | FT-TB767
#   runs; more levels; empty parents                        | FR-TB195/196| FT-TB381, FT-TB392, FT-TB382
#   group-summary rows; group_summary                       | FR-TB199    | FT-TB383, FT-TB386
#   label attributes kept; zero rows                        | FR-TB202    | FT-TB389, FT-TB395
#   vars; one column the stub as it is                      | FR-TB194    | FT-TB388, FT-TB390
#   label / indent / layout / label_span; guards            | FR-TB197-205| FT-TB384, FT-TB401, FT-TB398, FT-TB396, FT-TB394
#   the examples                                            | FR-TB193/196/199 | FT-TB768
#
# stub_spec()
#   one object for as_rtftables(stub = ); the defaults      | FR-TB207/208| FT-TB420, FT-TB421
#   vars required; layout checked; print                    | FR-TB209-211| FT-TB424, FT-TB414, FT-TB415
#   vars of one column; the stub_vars arguments removed in  | FR-TB591    | FT-TB769
#     0.9.0 (help fixed: it said "at least two" and that    |             |
#     they still work with a warning)                       |             |
#   the examples                                            | FR-TB207    | FT-TB769

.a3_rows <- function(tbl) {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(rtf_tables(rtf_document(), list(tbl)), f)
  grep("\\trowd", readLines(f, warn = FALSE), fixed = TRUE, value = TRUE)
}
.a3_has <- function(x, pat) any(grepl(pat, x, fixed = TRUE))
.a3_ae <- function() {
  data.frame(soc = c("Organ class A", "Organ class A", "Organ class B"),
             pt = c("Term 1", "Term 2", "Term 3"),
             n = c("3 (2.1%)", "1 (0.7%)", "5 (3.5%)"))
}

test_that("FT-TB755 [FR-TB101]: as_rtftable() is as_rtftables(split = \"none\")[[1]]; the example", {
  skip_if_not_installed("gt")
  d <- data.frame(mpg = c("21.0", "22.8"), cyl = c("6", "4"), gear = c("4", "4"))
  g <- gt::cols_align(gt::cols_label(gt::gt(d), mpg = "MPG", cyl = "Cyl"),
                      "right", columns = c("mpg", "cyl"))
  tbl <- as_rtftable(g)
  expect_s3_class(tbl, "rtftable")
  expect_identical(tbl, as_rtftables(g, split = "none")[[1L]])
  expect_identical(unlist(tbl$col_header), c("MPG", "Cyl", "gear"))
  expect_identical(vapply(tbl$col_spec[1:2], function(c) c$align, ""), c("right", "right"))
})

test_that("FT-TB756 [FR-TB584]: gt fills, fonts and Markdown formatting are not carried", {
  skip_if_not_installed("gt")
  d <- data.frame(lab = c("x1", "x2"), val = c("1", "2"))
  g <- gt::cols_label(gt::gt(d), lab = gt::md("**Lab**"))
  g <- gt::tab_style(g, gt::cell_fill("red"), gt::cells_body(columns = "val"))
  g <- gt::tab_style(g, gt::cell_text(font = "Arial", size = gt::px(20)),
                     gt::cells_body(columns = "lab"))
  tbl <- as_rtftable(g)
  expect_identical(unlist(tbl$col_header), c("Lab", "val"))
  expect_null(tbl$cell_styles)
  r <- .a3_rows(tbl)
  expect_false(.a3_has(r, "\\clcbpat"))
  expect_false(.a3_has(r, "\\b "))
  expect_false(.a3_has(r, "\\fs40"))
})

test_that("FT-TB757 [FR-TB586]: the body columns are gt's ids, or V1, V2, ... for rtables, flextable and huxtable", {
  d <- data.frame(lab = c("x1", "x2"), val = c("1", "2"))
  if (requireNamespace("gt", quietly = TRUE)) {
    expect_identical(names(as_rtftable(gt::gt(d))$data), c("lab", "val"))
  }
  if (requireNamespace("gtsummary", quietly = TRUE)) {
    s <- gtsummary::tbl_summary(data.frame(ARM = c("A", "A", "B"), SEX = c("F", "M", "F")),
                                by = "ARM")
    expect_identical(names(as_rtftable(s)$data), c("label", "stat_1", "stat_2"))
  }
  if (requireNamespace("flextable", quietly = TRUE)) {
    expect_identical(names(as_rtftable(flextable::flextable(d))$data), c("V1", "V2"))
  }
  if (requireNamespace("huxtable", quietly = TRUE)) {
    expect_identical(names(as_rtftable(huxtable::as_hux(d))$data), c("V1", "V2"))
  }
  skip_if_not_installed("rtables")
  lyt <- rtables::analyze(rtables::split_cols_by(rtables::basic_table(), "ARM"), "v",
                          function(x) rtables::in_rows(n = length(x)))
  tb <- rtables::build_table(lyt, data.frame(ARM = c("A", "B", "B"), v = c(1, 2, 3)))
  expect_identical(names(as_rtftable(tb)$data), c("V1", "V2", "V3"))
  # an integer group_col works whatever the names are
  expect_length(as_rtftables(tb, split = "by_value", group_col = 1L), 1L)
})

test_that("FT-TB758 [FR-TB587]: rtf_table_style() checks align, header_align and the twips arguments", {
  expect_error(rtf_table_style(align = "middle"),
               "`align` must be \"left\", \"center\" or \"right\".", fixed = TRUE)
  expect_error(rtf_table_style(header_align = 3),
               "`header_align` must be \"left\", \"center\" or \"right\".", fixed = TRUE)
  for (a in c("row_height_twips", "cell_padding_left_twips", "cell_padding_right_twips")) {
    msg <- sprintf("`%s` must be a single non-negative integer (twips) or NULL.", a)
    expect_error(do.call(rtf_table_style, stats::setNames(list("a"), a)), msg, fixed = TRUE)
    expect_error(do.call(rtf_table_style, stats::setNames(list(-5), a)), msg, fixed = TRUE)
  }
  s <- rtf_table_style(align = "right", header_align = "left", row_height_twips = 300,
                       cell_padding_left_twips = 40L, cell_padding_right_twips = NULL)
  expect_identical(c(s$align, s$header_align), c("right", "left"))
  expect_identical(c(s$row_height_twips, s$cell_padding_left_twips), c(300L, 40L))
  expect_null(s$cell_padding_right_twips)
  expect_true(.a3_has(.a3_rows(rtftable(data.frame(a = "1"), style = s)), "\\trrh300"))
})

test_that("FT-TB759 [FR-TB445 FR-TB446]: the rtf_table_style() example: a header rule and a last-row rule", {
  tfl_style <- rtf_table_style(
    border_header   = rtf_border(top = TRUE, bottom = TRUE),
    border_last_row = rtf_border(bottom = TRUE),
    header_bold     = FALSE,
    header_align    = NULL
  )
  expect_s3_class(tfl_style, "rtf_table_style")
  dfs <- list(data.frame(Parameter = "Age", Value = "75.1"),
              data.frame(Parameter = "Sex", Value = "53%"))
  tbls <- lapply(dfs, function(df) rtftable(df, style = tfl_style))
  r <- .a3_rows(tbls[[1L]])
  expect_length(r, 2L)
  expect_true(grepl("\\clbrdrt", r[1L], fixed = TRUE) && grepl("\\clbrdrb", r[1L], fixed = TRUE))
  expect_false(grepl("\\b ", r[1L], fixed = TRUE))
  # header alignment inherits the data: Parameter left, Value centred
  expect_true(grepl("\\ql\\li0\\ri0 Parameter", r[1L], fixed = TRUE))
  expect_true(grepl("\\qc\\li0\\ri0 Value", r[1L], fixed = TRUE))
  expect_true(grepl("\\clbrdrb", r[2L], fixed = TRUE))
  expect_false(grepl("\\clbrdrt", r[2L], fixed = TRUE))
})

test_that("FT-TB760 [FR-TB588]: rtf_table_style_with() checks as rtf_table_style() does; NULL is the default", {
  base <- rtf_table_style(header_bold = TRUE, align = "right")
  expect_error(rtf_table_style_with(base, border_header = "x"),
               "`border_header` must be NULL or an rtf_border object.", fixed = TRUE)
  expect_error(rtf_table_style_with(base, align = "middle"),
               "`align` must be \"left\", \"center\" or \"right\".", fixed = TRUE)
  expect_error(rtf_table_style_with(base, row_height_twips = "a"),
               "`row_height_twips` must be a single non-negative integer", fixed = TRUE)
  reset <- rtf_table_style_with(base, header_bold = NULL, align = NULL)
  expect_false(reset$header_bold)
  expect_null(reset$align)
  expect_true(base$header_bold)
  expect_error(rtf_table_style_with(base, TRUE),
               "The fields given to `rtf_table_style_with()` must be named", fixed = TRUE)
  expect_identical(rtf_table_style_with(base), base)
})

test_that("FT-TB761 [FR-TB459 FR-TB460]: the rtf_table_style_tfl() and rtf_table_style_with() examples", {
  base <- rtf_table_style_tfl()
  v <- rtf_table_style_with(base, align = "center", row_height_twips = 280L)
  expect_identical(c(v$align, as.character(v$row_height_twips)), c("center", "280"))
  expect_null(base$align)
  heavy <- rtf_table_style_with(rtf_table_style_tfl(), header_bold = TRUE,
                                border_last_row = rtf_border(bottom = TRUE))
  r <- .a3_rows(rtftable(data.frame(Parameter = c("Age", "Sex"), Value = c("75.1", "53%")),
                         style = heavy))
  expect_true(grepl("\\b Parameter", r[1L], fixed = TRUE))
  expect_true(grepl("\\clbrdrb", r[3L], fixed = TRUE))
  expect_false(grepl("\\clbrdr", r[2L], fixed = TRUE))
  r <- .a3_rows(rtftable(data.frame(Parameter = "Age", Value = "75.1"), style = base))
  expect_true(grepl("\\clbrdrt", r[1L], fixed = TRUE) && grepl("\\clbrdrb", r[1L], fixed = TRUE))
  expect_false(grepl("\\clbrdr", r[2L], fixed = TRUE))
})

test_that("FT-TB762 [FR-TB425 FR-TB430 FR-TB439 FR-TB233]: the style_header() examples", {
  df <- data.frame(Item = c("Age", "Sex"), A = 1:2, B = 3:4, C = 5:6)
  hdr <- rtf_col_header(
    list(col_cell(1, ""), col_cell(c(2, 4), "Treatment")),
    list(col_cell(1, ""), col_cell(c(2, 4), "(N = 254)")),
    c("Item", "Placebo", "Drug A", "Drug B")
  )
  tbl <- rtftable(df, col_header = hdr, border = "tfl")
  tbl <- style_header(tbl, row = 2, cols = 2:4,
                      border = rtf_border(top = TRUE, bottom = "none"))
  tbl <- style_body(style_cols(tbl, cols = c("B", "C"), align = "center"),
                    rows = ~ Item == "Age", bold = TRUE)
  tbl <- style_zone(tbl, last_row = rtf_border(bottom = "double"))
  tbl <- add_header_row(tbl, list(col_cell(c(2, 4), "STUDY01")), .position = "top")
  r <- .a3_rows(tbl)
  expect_length(r, 6L)
  expect_true(grepl("STUDY01", r[1L], fixed = TRUE))
  # a rule above the "(N = 254)" cell, none on the cell under it
  n254 <- r[3L]
  expect_true(grepl("(N = 254)", n254, fixed = TRUE))
  expect_true(grepl("\\clbrdrt\\brdrs\\brdrw15\\clvertalb\\cellx13680", n254, fixed = TRUE))
  expect_false(grepl("\\clbrdrb", n254, fixed = TRUE))
  # the Age row bold in every column, the Sex row not
  expect_true(grepl("\\b Age\\b0", r[5L], fixed = TRUE))
  expect_true(grepl("\\b 5\\b0", r[5L], fixed = TRUE))
  expect_false(grepl("\\b ", r[6L], fixed = TRUE))
  # the double rule under the last row
  expect_identical(lengths(regmatches(r[6L], gregexpr("\\clbrdrb\\brdrdb", r[6L], fixed = TRUE))), 4L)
})

test_that("FT-TB763 [FR-TB589]: style_body() has no per-row height or padding", {
  t0 <- rtftable(data.frame(a = c("1", "2")))
  for (a in c("row_height_twips", "cell_padding_left_twips", "cell_padding_top_twips")) {
    expect_warning(t1 <- do.call(style_body, c(list(t0), stats::setNames(list(300), a))),
                   sprintf("`style_body()`: unknown argument `%s` ignored.", a), fixed = TRUE)
    expect_identical(t1$row_height_twips, t0$row_height_twips)
    expect_identical(.a3_rows(t1), .a3_rows(t0))
  }
})

test_that("FT-TB764 [FR-TB236]: add_header_row() with a wrong-length row or .position stops", {
  t0 <- rtftable(data.frame(Item = "x", A = "1", B = "2"))
  expect_error(add_header_row(t0, c("a", "b")),
               "`col_header` row 1 has 2 labels but the table has 3 columns", fixed = TRUE)
  expect_error(add_header_row(t0, c("a", "b")),
               "Give one label per column, or name the ones to change: c(Item = \"...\").",
               fixed = TRUE)
  expect_error(add_header_row(t0, c("a", "b", "c"), .position = "mid"),
               "'arg' should be one of", fixed = TRUE)
  # a named row of fewer labels is a patch
  t1 <- add_header_row(t0, c(A = "Arm A"))
  expect_identical(t1$col_header, list(c("Item", "Arm A", "B"), c("Item", "A", "B")))
})

test_that("FT-TB765 [FR-TB215 FR-TB212 FR-DC287]: set_col_header() takes a partly named row; the examples", {
  df <- data.frame(row_label = c("A", "B"), g1 = 1:2, g2 = 3:4, Total = 5:6)
  tbl <- rtftable(df)
  expect_identical(rtf_columns(tbl), c("row_label", "g1", "g2", "Total"))
  expect_identical(rtf_columns(rtftable(data.frame(Item = "x", A = 1, B = 2))),
                   c("Item", "A", "B"))
  # a fully named row is a patch
  expect_identical(set_col_header(tbl, c(g1 = "Low", g2 = "High"))$col_header,
                   list(c("row_label", "Low", "High", "Total")))
  # a partly named row: an unnamed value that is a column name goes there,
  # any other by its position
  x <- set_col_header(tbl, c("row_label", "First", g2 = "High", "Total"))
  expect_identical(x$col_header, list(c("row_label", "First", "High", "Total")))
  tbl <- set_col_header(
    tbl,
    list(col_cell("row_label", ""), col_cell(c("g1", "g2"), "Treatment")),
    c(row_label = "Category", g1 = "Low", g2 = "High", Total = "Total")
  )
  expect_length(tbl$col_header, 2L)
  expect_identical(tbl$col_header[[2L]], c("Category", "Low", "High", "Total"))
  sp <- tbl$col_header[[1L]]
  lab <- vapply(sp, function(c) c$label, "")
  expect_true("Treatment" %in% lab)
  tr <- sp[[match("Treatment", lab)]]
  expect_identical(c(tr$from, tr$to), c(2L, 3L))
})

test_that("FT-TB766 [FR-TB346 FR-TB345 FR-TB363]: set_decimal_split() needs cols, NULL clears; the example", {
  df <- data.frame(Statistic = c("Mean", "SD", "p-value"),
                   Value = c("12.3", "1.05", "<0.001"))
  expect_error(set_decimal_split(rtftable(df)), "`cols` is required", fixed = TRUE)
  tbl <- set_decimal_split(rtftable(df), cols = "Value")
  expect_false(is.null(tbl$decimal_split))
  expect_null(set_decimal_split(tbl, cols = NULL)$decimal_split)
  r <- .a3_rows(tbl)
  expect_true(.a3_has(r, "\\qr\\li0\\ri0 12\\cell\\ql\\li0\\ri0 .3\\cell"))
  expect_true(.a3_has(r, "\\qr\\li0\\ri0 <0\\cell\\ql\\li0\\ri0 .001\\cell"))
  expect_identical(tbl$data, df)
})

test_that("FT-TB767 [FR-TB590]: stub_cols() output goes on through as_rtftables() as an indented table", {
  ae <- .a3_ae()
  ae <- rbind(ae[1:2, ], data.frame(soc = "Organ class A", pt = "Term 4", n = "2 (1.4%)"), ae[3, ])
  tb <- stub_cols(ae, vars = c("soc", "pt"))
  # the groups are found on the indentation
  b <- as_rtftables(tb, blank_rows = "between_groups")[[1L]]
  expect_identical(as.integer(b$blank_rows), 4L)
  # a group cut by the page continues with " (Cont.)"
  pages <- as_rtftables(tb, split = "group_force", max_rows = 3)
  expect_identical(pages[[1L]]$data[[1L]][1L], "Organ class A")
  expect_identical(pages[[2L]]$data[[1L]][1L], "Organ class A (Cont.)")
  # group_safe keeps a label row with its children
  pages <- as_rtftables(tb, split = "group_safe", max_rows = 5)
  expect_identical(vapply(pages, function(p) p$data[[1L]][1L], ""),
                   c("Organ class A", "Organ class B"))
})

test_that("FT-TB768 [FR-TB193 FR-TB196 FR-TB199]: the stub_cols() examples", {
  ae <- .a3_ae()
  s <- stub_cols(ae, vars = c("soc", "pt"), label = "System Organ Class / Preferred Term")
  expect_identical(names(s), c("System Organ Class / Preferred Term", "n"))
  expect_identical(nrow(s), 5L)
  expect_identical(s$n, c(NA, "3 (2.1%)", "1 (0.7%)", NA, "5 (3.5%)"))
  nb <- "    "
  expect_identical(s[[1L]][1:2], c("Organ class A", paste0(nb, "Term 1")))
  # a summary row stays flush left
  ae2 <- rbind(data.frame(soc = "", pt = "Any adverse event", n = "9 (6.3%)"), ae)
  tbl <- stub_cols(ae2, vars = c("soc", "pt"))
  expect_identical(names(tbl)[1L], "soc / pt")
  expect_identical(tbl[[1L]][1L], "Any adverse event")
  pages <- as_rtftables(tbl, split = "group_force", max_rows = 4)
  expect_identical(vapply(pages, function(p) nrow(p$data), 1L), c(4L, 2L))
  # AE style: the NA leaf's statistic is folded onto the label row
  ae_sum <- data.frame(soc = rep("Organ class A", 3L), pt = c(NA, "Term 1", "Term 2"),
                       n = c("4 (2.8%)", "3 (2.1%)", "1 (0.7%)"))
  f <- stub_cols(ae_sum, vars = c("soc", "pt"))
  expect_identical(f[[1L]], c("Organ class A", paste0(nb, c("Term 1", "Term 2"))))
  expect_identical(f$n, ae_sum$n)
})

test_that("FT-TB769 [FR-TB591 FR-TB207]: stub_spec() of one column; the stub_vars arguments are gone; the examples", {
  d <- data.frame(soc = c("S1", "S1", "S2"), pt = c("p1", "p2", "p3"), n = c("1", "2", "3"))
  one <- stub_spec("soc", label = "Organ")
  p <- as_rtftables(d, stub = one)[[1L]]
  expect_identical(names(p$data), c("Organ", "pt", "n"))
  expect_identical(p$data$Organ, d$soc)
  expect_identical(names(as_rtftables(d, stub = stub_spec("soc"))[[1L]]$data), names(d))
  expect_error(as_rtftables(d, stub_vars = c("soc", "pt")), "stub_vars")
  expect_error(as_rtftables(d, stub_label = "x"), "stub_label")
  s1 <- stub_spec(c("SOC", "PT"), label = "System Organ Class / Preferred Term")
  s2 <- stub_spec(c("SOC", "PT"), layout = "columns")
  s3 <- stub_spec(c("SOC", "PT"), indent = 0, label_span = TRUE)
  expect_true(all(vapply(list(s1, s2, s3), inherits, TRUE, "rtf_stub_spec")))
  expect_identical(c(s1$layout, s2$layout, s3$layout), c("merged", "columns", "merged"))
  expect_output(print(s3), "label_span : TRUE", fixed = TRUE)
})

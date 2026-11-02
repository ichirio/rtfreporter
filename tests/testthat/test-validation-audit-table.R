# Help-to-FR audit, batch 2: the table exports rtftable(), rtf_tables() and
# as_rtftables().
#
# The help is the functional requirement: every statement in a function's Rd
# is an active FR in inst/validation/requirements.csv, and every FR has a
# test.  For each function below, one row per claim of its roxygen: the
# claim (short), the FR that states it, and the test that checks it.  The
# tests below this table close the gaps the audit found.  A test written
# before the audit and mapped by hand in inst/validation/test_map.csv is
# named by its FT id.
#
# rtftable()
#   claim                                                   | FR          | test
#   data: a data.frame, or a list of them (multi-DF); guards| FR-TB561    | FT-TB706
#   col_header NULL: the names; unnamed vector by position  | FR-TB562    | FT-TB707
#   col_header named: name-aware; unknown / twice an error  | FR-TB562    | FT-TB707
#   col_header list of rows, spanning rows; col_cell names  | FR-TB563    | FT-TB708
#   col_header per data frame in multi-DF mode              | FR-TB563    | FT-TB708
#   col_header_align: NULL follows the data; one or ncol    | FR-TB564    | FT-TB709
#   col_spec: col by index or name, its fields; guards      | FR-TB565    | FT-TB710
#   row_title: heading columns left, others centred         | FR-TB547    | FT-TB696
#   row_title overridden by col_spec / style / source       | FR-IX336    | FT-TB644
#   border "tfl" / "none" (help fixed: no last-row rule)    | FR-TB566    | FT-TB711
#   border rtf_border(): the whole table                    | FR-IX338    | FT-TB456
#   border rtf_table_style(): its zones                     | FR-TB455    | FT-TB602
#   blank_rows: positions 0 / k / -1, by_change, by_rule,   | FR-TB567    | FT-TB712
#     a list unioned; multi-DF positions only               |             |
#   read_attributes: the rtf_blank_rows attribute           | FR-IX327    | FT-TB444
#   style: defaults only, explicit arguments win            | FR-IX335    | FT-TB575
#   style: a snapshot at construction                       | FR-TB445    | FT-TB656
#   col_rel_width, column_widths_twips, table_width_twips,  | FR-TB568    | FT-TB713
#     table_width_pct                                       |             |
#   table_width_pct_of_writable                             | FR-TB548    | FT-TB696
#   table_align                                             | FR-TB549    | FT-TB696
#   row_height_twips: NULL inherits, else the font's height | FR-TB569    | FT-TB714
#   font_size_half_points / font                            | FR-TB550/551| FT-TB697
#   row_height_exact                                        | FR-TB552    | FT-TB698
#   header_row_height_twips / blank_row_height_twips        | FR-TB553/554| FT-TB698
#   cell_padding_left / right: NULL inherits, else 0        | FR-TB570    | FT-TB715
#   cell_padding_top / bottom                               | FR-TB545/546| FT-TB695
#   cell_valign                                             | FR-TB555    | FT-TB698
#   cell_styles                                             | FR-TB556    | FT-TB699
#   blank_row_normalize                                     | FR-TB557    | FT-TB699
#   markup                                                  | FR-TB558    | FT-TB697
#   twips arguments not integers are an error (fixed here)  | FR-TB571    | FT-TB716
#   table_width_pct_of_writable outside (0, 1] an error     | FR-TB572    | FT-TB717
#     (fixed here); table_width_pct outside (0, 100]        |             |
#   @return an rtftable for rtf_tables(); the examples      | FR-TB573    | FT-TB718
#
# rtf_tables()
#   each element of tables one page; returns the document   | FR-TB168    | FT-TB325
#   tables: plan, as_rtftables() pages, data.frame, ...     | FR-TB170    | FT-TB329
#   a registered as_rtftables() method                      | FR-TB112    | FT-TB009
#   gt / gtsummary read as as_rtftables() reads them        | FR-TB192    | FT-TB691
#   titles: blocks, one per page or one for all             | FR-TB172    | FT-TB436
#   titles: paragraphs, a table with title_format = "table" | FR-DC311    | FT-DC235
#     (help fixed: it said a table always)                  |             |
#   footnotes: rows strings or c(l =, r =), left-aligned,   | FR-TB173    | FT-TB719
#     no rule but rtf_footnotes(border = ) (help, FR fixed) |             |#   page-number tokens; page values in titles / footnotes   | FR-TB176    | FT-TB224
#   auto_section: a section where the name changes; "...n"  | FR-TB177    | FT-TB331
#   auto_section: an empty name continues the section       | FR-IX313    | FT-DC025
#   section_label_align                                     | FR-TB178    | FT-TB223
#   auto_title: the name as the last title row              | FR-TB179    | FT-TB044
#   auto_title: the same under either title_format          | FR-TB574    | FT-TB720
#   title_label_align                                       | FR-TB180/181| FT-TB045, FT-TB048
#   page_break: TRUE / FALSE, one or one each; guards       | FR-TB182/183| FT-TB164, FT-TB169
#   page_break: one section heading a page; a later warns   | FR-IX103    | FT-IX003
#   first_rows: the rows on the shared page; guards         | FR-TB184/185| FT-TB165
#   space_before; guards                                    | FR-TB186/187| FT-TB168, FT-TB169
#   cont_title; guards                                      | FR-TB188/189| FT-TB214, FT-TB165
#   titles / footnotes carried on the pages                 | FR-TB174    | FT-TB035
#   Page values: a title its table's, a band the first's   | FR-IX104    | FT-IX004
#   Page values: own tokens win; unknown left as written    | FR-IX315    | FT-DC139
#   Page values: on rtf_paginate_meta as page_values        | FR-TB132    | FT-TB226
#   Page values: rtables / tfrmt pages carry theirs         | FR-TB583    | FT-TB753
#   @details / example: a data.frame gets the defaults      | FR-TB170    | FT-TB330
#
# as_rtftables()
#   a list of rtftable pages; named when value-based        | FR-TB111    | FT-TB032
#   an S3 generic; a package registers its class            | FR-TB112    | FT-TB009
#   titles / source notes as rtf_titles / rtf_footnotes     | FR-TB113    | FT-TB035
#   x: the sources; gt_group / tbl_split expanded           | FR-TB114/115| FT-TB002, FT-TB193
#   a list flattened; several pages get "name...n" (help    | FR-TB116    | FT-TB723
#     and FR fixed)                                         |             |
#   unsupported input an error                              | FR-TB117    | FT-TB028
#   read_meta TRUE / FALSE / tokens; data.frame labels;     | FR-TB118-120| FT-TB037, FT-TB141, FT-TB336
#     tokens of other adapters ignored on a data.frame      |             |
#   split strategies; a custom function; its guards         | FR-TB121-123| FT-TB033, FT-TB134, FT-TB139
#   group_force against group_safe: 30 / 24 and 26 / 26     | FR-TB575    | FT-TB725
#   max_rows; required; optional for by_value (help and FR | FR-TB124/125| FT-TB721, FT-TB377
#     fixed)                                                |             |
#   split_rows                                              | FR-TB126    | FT-TB376
#   group_col; a factor; unknown an error                   | FR-TB127/128| FT-TB107, FT-TB100
#   group_by                                                | FR-TB129/130| FT-TB179, FT-TB178
#   page_by: names, "...n", group_col, order, per partition | FR-TB131-133, FR-IX321 | FT-TB287, FT-TB285, FT-TB292, FT-TB290
#   sort_by / sort_desc; guards; applied first              | FR-TB134-136, FR-IX323 | FT-TB359, FT-TB362, FT-TB370, FT-TB361
#   cont_label / min_group_rows                             | FR-TB137/138| FT-TB110, FT-TB118
#   blank_rows; guards                                      | FR-TB139/140| FT-TB058, FT-TB306
#   blank_rows -1: after each page's last row (fixed here)  | FR-TB139    | FT-TB754
#   blank_row_first / blank_row_end / count_blank_rows      | FR-TB141/142, FR-IX324 | FT-TB115, FT-TB112, FT-TB119
#   align_count_pct / cell_format; precedence; guards       | FR-TB143-146, FR-IX316 | FT-TB013, FT-TB062, FT-TB071
#   na: where, not missing, NaN / Inf, order; guards        | FR-TB147-149| FT-TB072, FT-TB172, FT-TB073
#   collapse_repeats                                        | FR-TB152    | FT-TB099
#   drop_cols; guards; coordinates                          | FR-TB153/154, FR-IX322 | FT-TB147, FT-TB155, FT-TB151
#   drop_cols of group_col: the marker not shown, the       | FR-TB576    | FT-TB726
#     by_value names kept                                   |             |
#   listing; built body; rlistings; defaults only           | FR-TB155-157, FR-IX318/351/355 | FT-TB113, FT-TB218, FT-TB217, FT-TB216
#   stub; built before pagination; by_value exception       | FR-TB158/161, FR-IX320 | FT-TB413, FT-TB016, FT-TB017
#   header_sep                                              | FR-TB162    | FT-TB203
#   auto_width / table_width_twips / the width vocabulary   | FR-TB163/164, FR-IX101/319 | FT-TB015, FT-TB014, FT-IX001, FT-TB429
#   border / style passed on                                | FR-TB165    | FT-TB688
#   first_rows; with page_by / custom / by_value an error   | FR-TB150/151| FT-TB165, FT-TB722
#   ...: forwarded to rtftable(); explicit values win       | FR-TB166, FR-IX317 | FT-TB131, FT-TB190
#   cell_styles follow their rows                           | FR-TB167    | FT-TB361
#   What is carried: gt / gtsummary                         | FR-TB577    | FT-TB727 to FT-TB731
#   What is carried: gt tab_style() styles                  | FR-TB578    | FT-TB732 to FT-TB737
#   What is carried: rtables / tern                         | FR-TB579    | FT-TB738 to FT-TB742
#   What is carried: flextable                              | FR-TB580    | FT-TB743 to FT-TB746
#   What is carried: huxtable                               | FR-TB581    | FT-TB747 to FT-TB750
#   What is carried: a gt with several stub columns         | FR-TB582    | FT-TB751, FT-TB752
#   (not yet audited: a gt grouped by several columns, gt::html() labels
#    and <br>, the column names of each source for group_col -- batch 3)
#   the examples                                            | FR-TB121/126/139 | FT-TB724

.at_rs <- function(doc) {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}
.at_one <- function(tbl, doc = rtf_document()) .at_rs(rtf_tables(doc, list(tbl)))
.at_df <- function() data.frame(g1 = c("a", "b"), g2 = c("1", "2"), g3 = c("x", "y"))
.at_rows <- function(s) strsplit(s, "\n", fixed = TRUE)[[1L]]
.at_line <- function(s, txt) grep(txt, .at_rows(s), fixed = TRUE, value = TRUE)

test_that("FT-TB706 [FR-TB561]: rtftable(data = ) is a data frame, or a list of data frames of the same width", {
  t1 <- rtftable(.at_df())
  expect_identical(t1$data, .at_df())
  expect_null(t1$data_list)
  m <- rtftable(list(.at_df(), .at_df()[1, ]))
  expect_length(m$data_list, 2L)
  # each frame its own header; the widths shared
  s <- .at_one(rtftable(list(.at_df(), .at_df()),
                        col_header = list(c("H1", "H2", "H3"), c("K1", "K2", "K3"))))
  expect_true(all(vapply(c("H1", "K3"), function(x) grepl(x, s, fixed = TRUE), TRUE)))
  cellx <- function(l) regmatches(l, gregexpr("\\\\cellx[0-9]+", l))[[1L]]
  expect_identical(cellx(.at_line(s, "H1")), cellx(.at_line(s, "K1")))
  expect_error(rtftable("x"), "`data` must be a data.frame or a non-empty list of data.frames.",
               fixed = TRUE)
  expect_error(rtftable(list()), "`data` must be a data.frame or a non-empty list", fixed = TRUE)
  expect_error(rtftable(list(.at_df(), "b")), "`data[[2]]` must be a data.frame.", fixed = TRUE)
  expect_error(rtftable(list(.at_df(), .at_df()[, 1:2])),
               "All data.frames in `data` must have the same number of columns.", fixed = TRUE)
})

test_that("FT-TB707 [FR-TB562]: col_header NULL is the names; a vector by position, or by name when named", {
  expect_identical(.at_line(.at_one(rtftable(.at_df())), "g1")[1L] != "", TRUE)
  expect_identical(rtftable(.at_df(), col_header = c("A", "B", "C"))$col_header,
                   list(c("A", "B", "C")))
  # named: order-independent; an unnamed element that is a column name goes there
  expect_identical(rtftable(.at_df(), col_header = c(g3 = "G3", g1 = "G1", "g2"))$col_header,
                   list(c("G1", "g2", "G3")))
  expect_error(rtftable(.at_df(), col_header = c(zz = "Z", "b", "c")),
               "col_header: unknown column name \"zz\".", fixed = TRUE)
  expect_error(rtftable(.at_df(), col_header = c(g1 = "A", g1 = "B", "c")),
               "is targeted twice", fixed = TRUE)
  expect_error(rtftable(.at_df(), col_header = c(g1 = "A", "g1", "c")),
               "is targeted twice", fixed = TRUE)
})

test_that("FT-TB708 [FR-TB563]: col_header as a list of rows, spanning rows, col_cell() by name and per data frame", {
  h <- list(list(col_cell(c("g2", "g3"), "Drug A")), c("G1", "G2", "G3"))
  t <- rtftable(.at_df(), col_header = h)
  expect_length(t$col_header, 2L)
  span <- t$col_header[[1L]]
  expect_identical(vapply(span, function(c) c$label, ""), c("", "Drug A"))
  expect_identical(c(span[[2L]]$from, span[[2L]]$to), c(2L, 3L))
  s <- .at_one(t)
  # the rows top to bottom: the spanner, then the labels
  expect_lt(regexpr("Drug A", s), regexpr("G2", s))
  # the legacy spanning row
  t2 <- rtftable(.at_df(), col_header = list(list(list(from = 1, to = 2, label = "AB")),
                                             c("A", "B", "C")))
  expect_identical(t2$col_header[[1L]][[1L]]$label, "AB")
  # multi-DF: one header per data frame
  m <- rtftable(list(.at_df(), .at_df()), col_header = list(c("A", "B", "C"), c("D", "E", "F")))
  expect_identical(m$col_header_list, list(list(c("A", "B", "C")), list(c("D", "E", "F"))))
})

test_that("FT-TB709 [FR-TB564]: col_header_align follows the data alignment, or one value, or one per column", {
  t <- rtftable(.at_df(), col_spec = list(list(col = 2, align = "right")))
  expect_identical(vapply(t$col_spec, function(c) c$header_align %||% c$align, ""),
                   c("left", "right", "center"))
  s <- .at_one(t)
  expect_true(grepl("\\qr\\li0\\ri0 g2", s, fixed = TRUE))
  t <- rtftable(.at_df(), col_header_align = "right")
  expect_identical(vapply(t$col_spec, function(c) c$header_align, ""), rep("right", 3L))
  t <- rtftable(.at_df(), col_header_align = c("left", "center", "right"))
  expect_identical(vapply(t$col_spec, function(c) c$header_align, ""), c("left", "center", "right"))
  expect_error(rtftable(.at_df(), col_header_align = c("left", "right")),
               "must have length 1 or 3", fixed = TRUE)
  expect_error(rtftable(.at_df(), col_header_align = "justify"), "left.*center.*right")
})

test_that("FT-TB710 [FR-TB565]: col_spec targets a column by index or name and styles it", {
  t <- rtftable(.at_df(), col_spec = list(
    list(col = "g2", align = "right", bold = TRUE, italic = TRUE, underline = TRUE,
         indent_twips = 200, color = "#FF0000"),
    list(col = 3, header_align = "left", header_bold = TRUE, header_italic = TRUE)))
  cs <- t$col_spec
  expect_identical(cs[[2L]]$align, "right")
  expect_true(cs[[2L]]$bold && cs[[2L]]$italic && cs[[2L]]$underline)
  expect_identical(c(cs[[3L]]$header_align, cs[[3L]]$header_bold, cs[[3L]]$header_italic),
                   c("left", "TRUE", "TRUE"))
  s <- .at_one(t)
  # the colour is added to the colour table and written on the column's cells
  expect_true(grepl("\\red255\\green0\\blue0;", s, fixed = TRUE))
  l <- .at_line(s, " 1\\")
  expect_true(grepl("\\qr\\li200", l, fixed = TRUE))
  expect_true(grepl("\\b\\i\\ul", gsub(" ", "", l), fixed = TRUE) ||
                all(vapply(c("\\b", "\\i", "\\ul", "\\cf"), function(x) grepl(x, l, fixed = TRUE), TRUE)))
  expect_error(rtftable(.at_df(), col_spec = "x"), "`col_spec` must be a list", fixed = TRUE)
  expect_error(rtftable(.at_df(), col_spec = list(list(align = "right"))),
               "must be a list with a `col` key", fixed = TRUE)
  expect_error(rtftable(.at_df(), col_spec = list(list(col = "nope"))), "not found in data",
               fixed = TRUE)
  expect_error(rtftable(.at_df(), col_spec = list(list(col = 5))), "out of range", fixed = TRUE)
})

test_that("FT-TB711 [FR-TB566]: border \"tfl\" rules the column header only; \"none\" draws nothing", {
  df <- data.frame(A = c("r1", "r2", "r3"))
  s <- .at_one(rtftable(df))
  hdr <- .at_line(s, " A\\cell")
  expect_true(grepl("\\clbrdrt", hdr, fixed = TRUE) && grepl("\\clbrdrb", hdr, fixed = TRUE))
  # the help said a rule fell under the last row; the preset has none (help
  # fixed in this audit)
  for (r in c("r1", "r2", "r3")) {
    expect_false(grepl("\\clbrdr", .at_line(s, paste0(r, "\\cell")), fixed = TRUE), info = r)
  }
  expect_false(grepl("\\clbrdr", .at_one(rtftable(df, border = "none")), fixed = TRUE))
})

test_that("FT-TB712 [FR-TB567]: blank_rows takes positions, change and rule specs, and a list of them", {
  df <- data.frame(g = c("a", "a", "b", "b"), v = 1:4)
  # 0 before the first row, k after row k, -1 after the last
  expect_identical(rtftable(df, blank_rows = c(0, 2, -1))$blank_rows, c(0L, 2L, 4L))
  expect_identical(rtftable(df, blank_rows = blank_rows_by_change(
    "g", include_before_first = FALSE, include_after_last = FALSE))$blank_rows, 2L)
  expect_identical(rtftable(df, blank_rows = blank_rows_by_rule("g", "^b$", where = "after"))$blank_rows,
                   c(3L, 4L))
  # a list: the positions unioned
  expect_identical(rtftable(df, blank_rows = list(0, blank_rows_by_rule("g", "^b$", where = "before"),
                                                  c(2, -1)))$blank_rows,
                   c(0L, 2L, 3L, 4L))
  s <- .at_one(rtftable(df, blank_rows = 2))
  expect_identical(lengths(regmatches(s, gregexpr("\\\\row", s))), 6L)
  # multi-DF: positions only, applied to every frame
  m <- list(df, df)
  expect_identical(rtftable(m, blank_rows = c(1, -1))$blank_rows, c(-1L, 1L))
  expect_error(rtftable(m, blank_rows = blank_rows_by_change("g")),
               "In multi-DF mode, `blank_rows` must be an integer vector", fixed = TRUE)
  expect_error(rtftable(m, blank_rows = -2), "`blank_rows` integers must be -1, 0, or positive.",
               fixed = TRUE)
})

test_that("FT-TB713 [FR-TB568]: col_rel_width, column_widths_twips, table_width_twips and table_width_pct size the columns", {
  cellx <- function(s, txt) {
    as.integer(sub("\\\\cellx", "", regmatches(.at_line(s, txt)[1L],
                                                 gregexpr("\\\\cellx[0-9]+", .at_line(s, txt)[1L]))[[1L]]))
  }
  df <- .at_df()
  # relative widths apportion the total the table is given
  x <- cellx(.at_one(rtftable(df, col_rel_width = c(2, 1, 1), table_width_twips = 8000)), "x\\cell")
  expect_identical(diff(c(0L, x)), c(4000L, 2000L, 2000L))
  # absolute widths win over relative ones, their sum the width
  x <- cellx(.at_one(rtftable(df, col_rel_width = c(2, 1, 1),
                              column_widths_twips = c(3000, 2000, 1000))), "x\\cell")
  expect_identical(diff(c(0L, x)), c(3000L, 2000L, 1000L))
  # table_width_pct is table_width_pct_of_writable * 100
  a <- .at_one(rtftable(df, table_width_pct = 50))
  b <- .at_one(rtftable(df, table_width_pct_of_writable = 0.5))
  expect_identical(cellx(a, "x\\cell"), cellx(b, "x\\cell"))
  expect_identical(max(cellx(a, "x\\cell")), 13680L %/% 2L)
})

test_that("FT-TB714 [FR-TB569]: row_height_twips left NULL inherits the style, the document, the option, else the font's height", {
  h <- function(s) unique(regmatches(.at_line(s, "x\\cell"), regexpr("\\\\trrh-?[0-9]+", .at_line(s, "x\\cell"))))
  df <- .at_df()
  expect_identical(h(.at_one(rtftable(df))), "\\trrh230")
  expect_identical(h(.at_one(rtftable(df, style = rtf_table_style(row_height_twips = 444L)))),
                   "\\trrh444")
  expect_identical(h(.at_one(rtftable(df), rtf_document(style = rtf_style(row_height_twips = 400L)))),
                   "\\trrh400")
  old <- options(rtfreporter.row_height_twips = 333L)
  on.exit(options(old), add = TRUE)
  expect_identical(h(.at_one(rtftable(df))), "\\trrh333")
  options(old)
  # the table's own wins over all of them
  expect_identical(h(.at_one(rtftable(df, row_height_twips = 500L),
                             rtf_document(style = rtf_style(row_height_twips = 400L)))), "\\trrh500")
})

test_that("FT-TB715 [FR-TB570]: the left and right padding left NULL inherit the style, the document, the option, else 0", {
  l <- function(s) .at_line(s, "x\\cell")
  df <- .at_df()
  expect_true(grepl("\\ql\\li0\\ri0 x", l(.at_one(rtftable(df, col_spec = list(list(col = 3, align = "left"))))),
                    fixed = TRUE))
  expect_true(grepl("\\li55\\ri66 x", l(.at_one(rtftable(df, cell_padding_left_twips = 55,
                                                         cell_padding_right_twips = 66))), fixed = TRUE))
  st <- rtf_table_style(cell_padding_left_twips = 11L, cell_padding_right_twips = 12L)
  expect_true(grepl("\\li11\\ri12 x", l(.at_one(rtftable(df, style = st))), fixed = TRUE))
  doc <- rtf_document(style = rtf_style(cell_padding_left_twips = 77L))
  expect_true(grepl("\\li77\\ri0 x", l(.at_one(rtftable(df), doc)), fixed = TRUE))
  old <- options(rtfreporter.cell_padding_left_twips = 88L)
  on.exit(options(old), add = TRUE)
  expect_true(grepl("\\li88\\ri0 x", l(.at_one(rtftable(df))), fixed = TRUE))
})

test_that("FT-TB716 [FR-TB571]: rtftable()'s twips arguments are single non-negative integers", {
  # these used to be written into the file as NA, or failed later with an
  # unrelated message (fixed in this audit)
  for (a in c("row_height_twips", "header_row_height_twips", "blank_row_height_twips",
              "cell_padding_left_twips", "cell_padding_right_twips", "table_width_twips")) {
    msg <- sprintf("`%s` must be a single non-negative integer (twips) or NULL.", a)
    for (bad in list("a", -1, c(1, 2), NA)) {
      expect_error(do.call(rtftable, c(list(.at_df()), stats::setNames(list(bad), a))),
                   msg, fixed = TRUE, info = a)
    }
  }
  t <- rtftable(.at_df(), row_height_twips = 300, cell_padding_left_twips = 0)
  expect_identical(c(t$row_height_twips, t$cell_padding_left_twips), c(300L, 0L))
})

test_that("FT-TB717 [FR-TB572]: table_width_pct_of_writable is a fraction in (0, 1], table_width_pct a percentage in (0, 100]", {
  # these used to be kept as given (fixed in this audit)
  for (bad in list(0, 2, "x", c(0.2, 0.3), NA)) {
    expect_error(rtftable(.at_df(), table_width_pct_of_writable = bad),
                 "`table_width_pct_of_writable` must be a number in (0, 1].", fixed = TRUE)
  }
  expect_identical(rtftable(.at_df(), table_width_pct_of_writable = 1)$table_width_pct_of_writable, 1)
  for (bad in list(0, 200, "x")) {
    expect_error(rtftable(.at_df(), table_width_pct = bad),
                 "`table_width_pct` must be a number in (0, 100].", fixed = TRUE)
  }
})

test_that("FT-TB718 [FR-TB573]: rtftable() returns an rtftable that rtf_tables() places; the examples render", {
  df <- data.frame(Subject = c("001", "002"), Age = c(34L, 45L))
  tbl <- rtftable(df)
  expect_s3_class(tbl, "rtftable")
  dm <- data.frame(Parameter = c("Age (years)", "  Mean (SD)", "  Median"),
                   Placebo = c("", "75.1 (8.2)", "76.0"), Active = c("", "74.3 (7.9)", "75.0"))
  tbl <- rtftable(dm, col_header = list(list(list(from = 2, to = 3, label = "Treatment",
                                                   underline = TRUE)),
                                        c("Parameter", "Placebo", "Active")),
                  col_spec = list(list(col = 1, align = "left")), col_rel_width = c(2, 1, 1),
                  border = "tfl")
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_tables(tbl, titles = list("Table 14.1.1"))
  s <- .at_rs(doc)
  expect_true(grepl("Treatment", s, fixed = TRUE))
  expect_true(grepl("Table 14.1.1", s, fixed = TRUE))
  # the spanner over the two arms, the row-label column twice as wide
  x <- as.integer(sub("\\\\cellx", "", regmatches(.at_line(s, "76.0")[1L],
                                                     gregexpr("\\\\cellx[0-9]+", .at_line(s, "76.0")[1L]))[[1L]]))
  w <- diff(c(0L, x))
  expect_equal(w[1L] / w[2L], 2, tolerance = 0.01)
})

test_that("FT-TB719 [FR-TB173]: footnote rows are strings or c(l =, r =), left-aligned, with no rule unless rtf_footnotes(border = )", {
  doc <- rtf_document()
  t1 <- list(rtftable(data.frame(A = 1)))
  s <- .at_rs(rtf_tables(doc, t1, footnotes = list(list("FNX", c(l = "FNL", r = "FNR")))))
  l <- .at_line(s, "FNX")
  expect_true(grepl("\\ql", l, fixed = TRUE))
  expect_false(grepl("brdr", l, fixed = TRUE))
  l <- .at_line(s, "FNL")
  expect_true(grepl("\\qr", l, fixed = TRUE) && grepl("FNR", l, fixed = TRUE))
  # the help said a row could set its own border, as a title row can; a styled
  # row list stops (help and FR fixed in this audit)
  expect_error(.at_rs(rtf_tables(doc, t1, footnotes = list(list(list(text = "FNY"))))),
               "A footnote row is a string or a named vector", fixed = TRUE)
  s <- .at_rs(rtf_footnotes(rtf_tables(doc, t1), "FNZ", border = rtf_border(top = TRUE)))
  expect_true(grepl("\\clbrdrt", .at_line(s, "FNZ"), fixed = TRUE))
})

test_that("FT-TB720 [FR-TB574]: auto_title writes the name the same under either title_format, and composes with auto_section", {
  tl <- list(Alpha = rtftable(data.frame(x = 1)), Beta = rtftable(data.frame(x = 2)))
  titles_of <- function(fmt) {
    s <- .at_rs(rtf_tables(rtf_document(style = rtf_style(title_format = fmt)), tl,
                           titles = list("TT"), auto_title = TRUE))
    l <- grep("TT|Alpha|Beta", .at_rows(s), value = TRUE)
    sub(".*\\\\b (.*)\\\\b0.*", "\\1", l)
  }
  expect_identical(titles_of("text"), c("TT", "Alpha", "TT", "Beta"))
  expect_identical(titles_of("table"), titles_of("text"))
  # with auto_section, the name goes in the section header too
  doc <- rtf_document() |>
    rtf_section(secinfo = list(header = rtf_header(c(l = "HDR"))))
  s <- .at_rs(rtf_tables(doc, tl, auto_title = TRUE, auto_section = TRUE))
  hdr <- grep("\\{\\\\header", .at_rows(s), value = TRUE)
  expect_length(hdr, 2L)
  expect_true(grepl("Alpha", hdr[1L], fixed = TRUE) && grepl("Beta", hdr[2L], fixed = TRUE))
  expect_identical(lengths(regmatches(s, gregexpr("Alpha", s, fixed = TRUE))), 2L)
})

test_that("FT-TB721 [FR-TB124]: max_rows is optional for split = \"by_value\": a value's longer page is cut, the pages keeping its name", {
  bv <- data.frame(g = rep(c("A", "B"), c(5, 2)), v = 1:7)
  p <- as_rtftables(bv, split = "by_value", group_col = "g", max_rows = 2)
  expect_identical(names(p), c("A...1", "A...2", "A...3", "A...4", "B"))
  expect_identical(unname(vapply(p, function(x) nrow(x$data), 1L)), rep(2L, 5L))
  # without max_rows, one page per value
  expect_identical(names(as_rtftables(bv, split = "by_value", group_col = "g")), c("A", "B"))
  # ignored by "none" and "rows"
  expect_length(as_rtftables(bv, max_rows = 2), 1L)
  expect_length(as_rtftables(bv, split = "rows", split_rows = 4, max_rows = 2), 2L)
})

test_that("FT-TB722 [FR-TB151]: as_rtftables(first_rows = ) with split = \"by_value\" stops", {
  bv <- data.frame(g = rep(c("A", "B"), c(5, 2)), v = 1:7)
  expect_error(as_rtftables(bv, split = "by_value", group_col = "g", first_rows = 2),
               "`first_rows` paginates by rows or groups: not split = \"by_value\".", fixed = TRUE)
})

test_that("FT-TB723 [FR-TB116]: a list input names its pages, several pages of one input with the \"...n\" tail", {
  df <- data.frame(x = 1:6)
  p <- as_rtftables(list(X = df, Y = df[1:2, , drop = FALSE]), split = "group_force",
                    max_rows = 3, min_group_rows = 0)
  expect_identical(names(p), c("X...1", "X...2", "Y"))
  # the tail is not part of the heading auto_title prints
  s <- .at_rs(rtf_tables(rtf_document(), p, auto_title = TRUE))
  expect_false(grepl("X...1", s, fixed = TRUE))
  expect_identical(lengths(regmatches(s, gregexpr("\\b X\\b0", s, fixed = TRUE))), 2L)
})

test_that("FT-TB724 [FR-TB121 FR-TB126 FR-TB139]: the examples of as_rtftables() give the pages they say", {
  df <- data.frame(Parameter = c("Age", "  Mean", "  SD", "Sex", "  F", "  M"),
                   Value = c("", "75.1", "8.2", "", "53%", "47%"))
  rows <- function(p) unname(vapply(p, function(x) nrow(x$data), 1L))
  expect_length(as_rtftables(df), 1L)
  three <- function(df, ...) list(df[1:3, ], df[-(1:3), ])
  expect_length(as_rtftables(df, split = three), 2L)
  expect_identical(rows(as_rtftables(df, split = "rows", split_rows = c(4, 6))), c(3L, 2L, 1L))
  expect_identical(rows(as_rtftables(df, split = "group_force", max_rows = 2)), rep(2L, 4L))
  expect_identical(as_rtftables(df, blank_rows = c(3, -1))[[1L]]$blank_rows, c(3L, 6L))
})

test_that("FT-TB725 [FR-TB575]: group_force cuts every max_rows rows where group_safe keeps the groups whole", {
  g <- data.frame(lab = unlist(lapply(1:10, function(i) c(paste("G", i), "  a", "  b", "  c"))),
                  v = "1")
  printed <- function(split) {
    p <- as_rtftables(g, split = split, max_rows = 30, count_blank_rows = TRUE,
                      blank_rows = "between_groups", blank_row_first = TRUE, blank_row_end = TRUE)
    unname(vapply(p, function(x) nrow(x$data) + length(x$blank_rows), 1L))
  }
  expect_identical(printed("group_safe"), c(26L, 26L))
  expect_identical(printed("group_force"), c(30L, 24L))
  # the sixth group is cut: page 2 opens with its continuation row
  p <- as_rtftables(g, split = "group_force", max_rows = 30, count_blank_rows = TRUE,
                    blank_rows = "between_groups", blank_row_first = TRUE, blank_row_end = TRUE)
  expect_identical(p[[2L]]$data$lab[1L], "G 6 (Cont.)")
})

test_that("FT-TB726 [FR-TB576]: a dropped group_col still names by_value pages, and hides the group_force marker", {
  gd <- data.frame(grp = c("G1", "G1", "G1", "G2"), lab = c("a", "b", "c", "d"))
  p <- as_rtftables(gd, split = "by_value", group_col = "grp", drop_cols = "grp")
  expect_identical(names(p), c("G1", "G2"))
  expect_identical(names(p[[1L]]$data), "lab")
  p <- as_rtftables(gd, split = "group_force", max_rows = 2, group_col = "grp", group_by = "value",
                    drop_cols = "grp", min_group_rows = 0)
  expect_identical(names(p[[2L]]$data), "lab")
  expect_false(any(grepl("Cont.", unlist(lapply(p, function(x) x$data$lab)), fixed = TRUE)))
  # on the visible column the marker shows
  p <- as_rtftables(gd[, c(2, 1)], split = "group_force", max_rows = 2, group_col = "grp",
                    group_by = "value", min_group_rows = 0)
  expect_true(grepl("(Cont.)", p[[2L]]$data$grp[1L], fixed = TRUE))
})

test_that("FT-TB754 [FR-TB139]: as_rtftables(blank_rows = -1) puts a blank after the last row of every page", {
  # -1 used to be dropped as a negative position (fixed in this audit)
  df <- data.frame(x = sprintf("r%d", 1:5))
  expect_identical(as_rtftables(df, blank_rows = c(0, -1))[[1L]]$blank_rows, c(0L, 5L))
  p <- as_rtftables(df, split = "rows", split_rows = 4, blank_rows = -1)
  expect_identical(lapply(p, function(x) x$blank_rows), list(3L, 2L))
  s <- .at_one(p[[2L]])
  expect_match(.at_rows(s)[grep("r5\\cell", .at_rows(s), fixed = TRUE) + 1L],
               "\\cell\\row", fixed = TRUE)
})

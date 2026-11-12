# Thickening of the functional tests, batch 11: the adapters and the input
# of as_rtftables() / as_rtftable() / rtf_tables() (area TB).  Each block
# asserts what its FR states, the figures worked out in a comment.
# FT ids TB6101 up.

.t11_df <- function() data.frame(a = c("x", "y"), b = c(1.5, 2.5), stringsAsFactors = FALSE)

# the nbsp / spaces of an indent made plain
.t11_plain <- function(x) gsub("\u00a0", " ", x, fixed = TRUE)

# a small rtables tree: rows F / Mean / M / Mean
.t11_tree <- function() {
  skip_if_not_installed("rtables")
  lyt <- rtables::basic_table()
  lyt <- rtables::split_cols_by(lyt, "ARM")
  lyt <- rtables::split_rows_by(lyt, "SEX")
  lyt <- rtables::analyze(lyt, "AGE", afun = function(x) {
    rtables::in_rows("Mean" = rtables::rcell(mean(x), format = "xx.x"))
  })
  d <- data.frame(ARM = rep(c("A", "B"), each = 2), SEX = rep(c("F", "M"), 2),
                  AGE = c(40, 50, 30, 36), stringsAsFactors = FALSE)
  rtables::build_table(lyt, d)
}

# a gtsummary table with 2 variables and a tbl_split of two members
.t11_gts <- function() {
  skip_if_not_installed("gtsummary")
  gtsummary::tbl_summary(gtsummary::trial, by = trt, include = c(age, grade, stage, marker))
}

test_that("FT-TB6101 [FR-TB102]: as_rtftable(x = ) takes a gt_tbl, a gtsummary table, an rtables tree, a listing_df, a flextable, a huxtable, a data.frame and a tibble", {
  skip_if_not_installed("gt")
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("rlistings")
  skip_if_not_installed("flextable")
  skip_if_not_installed("huxtable")
  d <- .t11_df()
  one <- function(x) as_rtftable(x)
  g <- one(gt::gt(d))
  expect_s3_class(g, "rtftable")
  expect_identical(unname(unlist(g$data)), c("x", "y", "1.5", "2.5"))

  s <- one(.t11_gts())
  expect_s3_class(s, "rtftable")
  expect_identical(.t11_plain(s$data[[1L]][1:2]), c("Age", "    Unknown"))        # a label and its indented row

  tr <- one(.t11_tree())
  expect_s3_class(tr, "rtftable")
  expect_identical(tr$data[[1L]], c("F", "  Mean", "M", "  Mean"))

  lst <- rlistings::as_listing(data.frame(a = c("x", "y"), b = c("p", "q"),
                                          stringsAsFactors = FALSE), key_cols = "a")
  l <- one(lst)
  expect_s3_class(l, "rtftable")
  expect_identical(unname(unlist(l$data)), c("x", "y", "p", "q"))

  f <- one(flextable::flextable(d))
  expect_s3_class(f, "rtftable")
  expect_identical(unname(unlist(f$data)), c("x", "y", "1.5", "2.5"))

  h <- one(huxtable::hux(d))
  expect_s3_class(h, "rtftable")
  expect_identical(unname(unlist(h$data)), c("x", "y", "1.5", "2.5"))

  df <- one(d)
  expect_s3_class(df, "rtftable")
  expect_identical(df$data$a, c("x", "y"))
  expect_identical(df$data$b, c(1.5, 2.5))
  tb <- one(tibble::as_tibble(d))
  expect_s3_class(tb, "rtftable")
  expect_identical(tb$data$a, c("x", "y"))
  expect_identical(tb$data$b, c(1.5, 2.5))
})

test_that("FT-TB6102 [FR-TB103]: as_rtftable() converts the one table of a gt_group() , a gt_split(), a tfrmt page_plan render and a one-member tbl_split", {
  skip_if_not_installed("gt")
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("tfrmt")
  d <- .t11_df()
  want <- unname(unlist(as_rtftable(gt::gt(d))$data))
  expect_identical(want, c("x", "y", "1.5", "2.5"))
  grp <- gt::gt_group(gt::gt(d))
  expect_s3_class(grp, "gt_group")
  expect_identical(unname(unlist(as_rtftable(grp)$data)), want)
  sp <- gt::gt_split(gt::gt(d), row_every_n = 2)           # 2 rows every 2: one member
  expect_s3_class(sp, "gt_group")
  expect_identical(unname(unlist(as_rtftable(sp)$data)), want)

  dat <- expand.grid(group = "g1", label = paste0("lbl", 1:2), column = c("A", "B"),
                     param = "n", stringsAsFactors = FALSE)
  dat$value <- seq_len(nrow(dat))
  spec <- tfrmt::tfrmt(
    group = group, label = label, column = column, param = param, value = value,
    body_plan = tfrmt::body_plan(
      tfrmt::frmt_structure(group_val = ".default", label_val = ".default", tfrmt::frmt("x"))),
    page_plan = tfrmt::page_plan(tfrmt::page_structure(group_val = ".default")))
  pg <- tfrmt::print_to_gt(spec, dat)
  expect_s3_class(pg, "gt_group")
  r <- as_rtftable(pg)
  expect_s3_class(r, "rtftable")
  expect_identical(r$data$A, c("", "1", "2"))              # group row, then lbl1 = 1, lbl2 = 2
  expect_identical(r$data$B, c("", "3", "4"))

  skip_if_not("tbl_split_by_rows" %in% getNamespaceExports("gtsummary"))
  ts <- gtsummary::tbl_split_by_rows(.t11_gts(), variables = age)
  one <- structure(list(ts[[1L]]), class = class(ts))
  expect_identical(as_rtftable(one)$data, as_rtftable(ts[[1L]])$data)
  expect_identical(.t11_plain(as_rtftable(one)$data[[1L]][1:2]), c("Age", "    Unknown"))
})

test_that("FT-TB6103 [FR-TB109]: as_rtftable(drop_cols, sort_by, collapse_repeats, listing, header_sep, stub) act on the one page as in as_rtftables()", {
  s <- data.frame(soc = c("S1", "S1", "S2"), pt = c("p1", "p2", "p3"),
                  n = c("1", "2", "3"), x = c("q", "r", "s"), stringsAsFactors = FALSE)
  # drop_cols removes the column
  d <- as_rtftable(s, drop_cols = "x")
  expect_identical(names(d$data), c("soc", "pt", "n"))
  expect_identical(d, as_rtftables(s, drop_cols = "x")[[1L]])
  # sort_by with sort_desc puts n = 3, 2, 1
  so <- as_rtftable(s, sort_by = "n", sort_desc = TRUE)
  expect_identical(so$data$n, c("3", "2", "1"))
  expect_identical(so, as_rtftables(s, sort_by = "n", sort_desc = TRUE)[[1L]])
  # collapse_repeats blanks the second S1
  cr <- as_rtftable(s, collapse_repeats = "soc")
  expect_identical(cr$data$soc, c("S1", NA, "S2"))     # the repeat is NA (printed blank)
  expect_identical(cr, as_rtftables(s, collapse_repeats = "soc")[[1L]])
  # header_sep rebuilds a two-row header from "T|n", "T|pct"
  h <- data.frame(lab = c("a", "b"), `T|n` = c("1", "2"), `T|pct` = c("3", "4"),
                  check.names = FALSE, stringsAsFactors = FALSE)
  hd <- as_rtftable(h, header_sep = "|")
  expect_length(hd$col_header, 2L)
  expect_identical(hd$col_header[[2L]], c("lab", "n", "pct"))
  expect_identical(hd$col_header[[1L]][[2L]]$label, "T")
  expect_identical(hd, as_rtftables(h, header_sep = "|")[[1L]])
  # stub builds the indented label column, groups from soc / pt
  st <- as_rtftable(s, stub = stub_spec(c("soc", "pt")))
  expect_identical(st, as_rtftables(s, stub = stub_spec(c("soc", "pt")))[[1L]])
  expect_identical(.t11_plain(st$data[[1L]]), c("S1", "    p1", "    p2", "S2", "    p3"))
  # listing reshapes the data.frame as a listing body
  ad <- data.frame(id = c("01", "02"), term = c("HEAD", "BACK"), stringsAsFactors = FALSE)
  sp <- listing_spec(list(listing_col("id", width = 8, label = "Subject"),
                          listing_col("term", width = 10, label = "Term")))
  li <- as_rtftable(ad, listing = sp)
  expect_identical(li, as_rtftables(ad, listing = sp)[[1L]])
  expect_identical(li$col_header[[1L]], c("Subject", "", "Term"))   # a gutter column between
})

test_that("FT-TB6104 [FR-TB114]: as_rtftables(x = ) takes every kind of source and a list of them; the body is the rendered body", {
  skip_if_not_installed("gt")
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("rlistings")
  skip_if_not_installed("flextable")
  skip_if_not_installed("huxtable")
  d <- data.frame(g = c("A", "A", "B", "B"), v = c("a", "b", "c", "d"),
                  n = 1:4, h = 5:8, stringsAsFactors = FALSE)
  # gt: the hidden column h is dropped; 4 rows + 2 group rows (A, B) interleaved,
  # the grouped rows indented in the label text
  gb <- as_rtftables(gt::cols_hide(gt::gt(d, groupname_col = "g"), h))[[1L]]$data
  expect_identical(names(gb), c("v", "n"))
  expect_identical(nrow(gb), 6L)
  expect_identical(.t11_plain(gb$v), c("A", "  a", "  b", "B", "  c", "  d"))
  expect_identical(gb$n, c("", "1", "2", "", "3", "4"))
  # gt_group: each member is a page set
  grp <- as_rtftables(gt::gt_group(gt::gt(d[1:2, c("v", "n")]), gt::gt(d[3:4, c("v", "n")])))
  expect_length(grp, 2L)
  expect_identical(grp[[2L]]$data$v, c("c", "d"))
  # gtsummary: the label column carries the indent; the by-columns are the others
  gs <- as_rtftables(.t11_gts())[[1L]]$data
  expect_identical(.t11_plain(gs[[1L]][1:3]), c("Age", "    Unknown", "Grade"))
  # gtsummary tbl_split: a page set per member (2 members: age and the rest)
  skip_if_not("tbl_split_by_rows" %in% getNamespaceExports("gtsummary"))
  ts <- as_rtftables(gtsummary::tbl_split_by_rows(.t11_gts(), variables = age))
  expect_length(ts, 2L)
  expect_identical(ts[[1L]]$data[[1L]][1L], "Age")
  # rtables: the indent is in the label text
  tr <- as_rtftables(.t11_tree())[[1L]]$data
  expect_identical(tr[[1L]], c("F", "  Mean", "M", "  Mean"))
  # rlistings
  lst <- rlistings::as_listing(data.frame(a = c("x", "y"), b = c("p", "q"),
                                          stringsAsFactors = FALSE), key_cols = "a")
  expect_identical(unname(unlist(as_rtftables(lst)[[1L]]$data)), c("x", "y", "p", "q"))
  # flextable: the col_keys helper column h is dropped
  ft <- flextable::flextable(data.frame(a = c("x", "y"), b = c(1, 2), h = c(9, 9),
                                        stringsAsFactors = FALSE), col_keys = c("a", "b"))
  expect_identical(unname(unlist(as_rtftables(ft)[[1L]]$data)), c("x", "y", "1", "2"))
  # huxtable
  hx <- huxtable::hux(data.frame(a = c("x", "y"), b = c(1, 2), stringsAsFactors = FALSE))
  expect_identical(unname(unlist(as_rtftables(hx)[[1L]]$data)), c("x", "y", "1", "2"))
  # data.frame and tibble: the columns as they are
  expect_identical(as_rtftables(d)[[1L]]$data$v, c("a", "b", "c", "d"))
  expect_identical(names(as_rtftables(d)[[1L]]$data), c("g", "v", "n", "h"))
  expect_identical(as_rtftables(tibble::as_tibble(d))[[1L]]$data$n, 1:4)
  # a list of these: a page set for each, in order
  lst2 <- as_rtftables(list(d, hx, gt::gt(d[1:2, c("v", "n")])))
  expect_length(lst2, 3L)
  expect_identical(names(lst2[[1L]]$data), c("g", "v", "n", "h"))
  expect_identical(unname(unlist(lst2[[2L]]$data)), c("x", "y", "1", "2"))
  expect_identical(lst2[[3L]]$data$v, c("a", "b"))
})

test_that("FT-TB6105 [FR-TB115]: a gt_group, a tfrmt page_plan render and a tbl_split give one page set per member; an empty gt_group gives list()", {
  skip_if_not_installed("gt")
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("tfrmt")
  d4 <- data.frame(a = as.character(1:4), b = letters[1:4], stringsAsFactors = FALSE)
  # gt_group of 2 members of 2 rows: a page set each; max_rows = 1 cuts each into 2 pages
  sp <- gt::gt_group(gt::gt(d4[1:2, ]), gt::gt(d4[3:4, ]))
  one <- as_rtftables(sp)
  expect_length(one, 2L)
  expect_identical(one[[1L]]$data$a, c("1", "2"))
  expect_identical(one[[2L]]$data$a, c("3", "4"))
  many <- as_rtftables(sp, split = "group_force", max_rows = 1)
  expect_length(many, 4L)
  expect_identical(unname(vapply(many, function(p) p$data$a, "")), c("1", "2", "3", "4"))
  # gt_split() by columns: a member per slice
  cs <- as_rtftables(gt::gt_split(gt::gt(d4), col_slice_at = "a"))
  expect_length(cs, 2L)
  expect_identical(names(cs[[2L]]$data), "b")

  dat <- expand.grid(group = c("g1", "g2"), label = paste0("lbl", 1:2),
                     column = "A", param = "n", stringsAsFactors = FALSE)
  dat$value <- seq_len(nrow(dat))
  spec <- tfrmt::tfrmt(
    group = group, label = label, column = column, param = param, value = value,
    body_plan = tfrmt::body_plan(
      tfrmt::frmt_structure(group_val = ".default", label_val = ".default", tfrmt::frmt("x"))),
    page_plan = tfrmt::page_plan(tfrmt::page_structure(group_val = ".default")))
  tp <- as_rtftables(tfrmt::print_to_gt(spec, dat))
  expect_length(tp, 2L)                                    # one member per group value
  expect_identical(.t11_plain(tp[[1L]]$data$rowname)[1L], "g1")
  expect_identical(.t11_plain(tp[[2L]]$data$rowname)[1L], "g2")
  expect_identical(tp[[1L]]$data$A, c("", "1", "3"))       # g1 rows are 1 and 3 in dat
  expect_identical(tp[[2L]]$data$A, c("", "2", "4"))

  skip_if_not("tbl_split_by_rows" %in% getNamespaceExports("gtsummary"))
  ts <- gtsummary::tbl_split_by_rows(.t11_gts(), variables = c(age, grade))
  pages <- as_rtftables(ts)
  expect_length(pages, length(ts))
  expect_gt(length(pages), 1L)
  expect_identical(pages[[1L]]$data[[1L]][1L], "Age")
  expect_identical(pages[[2L]]$data[[1L]][1L], "Grade")

  expect_identical(as_rtftables(gt::gt_group()), list())
})

test_that("FT-TB6106 [FR-TB116]: a list is flattened in order: one page keeps its name, several get name...n, a named split gets <list name>.<page name>, an empty list is list()", {
  d <- data.frame(grp = c("A", "A", "B"), v = c("1", "2", "3"), stringsAsFactors = FALSE)
  df <- data.frame(x = 1:6)
  p <- as_rtftables(list(X = df, Y = df[1:2, , drop = FALSE]), split = "group_force",
                    max_rows = 3, min_group_rows = 0)
  # X is 6 rows: 2 pages of 3; Y is 2 rows: one page
  expect_identical(names(p), c("X...1", "X...2", "Y"))
  expect_identical(p[[1L]]$data$x, c("1", "2", "3"))
  expect_identical(p[[2L]]$data$x, c("4", "5", "6"))
  expect_identical(p[[3L]]$data$x, c("1", "2"))
  # one input making one page keeps its name, in order, beside one whose split names its pages
  q <- as_rtftables(list(One = d[1:2, ], Z = d, Last = d[3L, ]),
                    split = "by_value", group_col = "grp")
  expect_identical(names(q), c("One", "Z.A", "Z.B", "Last"))
  expect_identical(q$Z.B$data$v, "3")
  expect_identical(q$Last$data$v, "3")
  expect_identical(as_rtftables(list(), split = "by_value", group_col = "grp"), list())
  expect_identical(as_rtftables(list()), list())
  # without a split that names, one page keeps the list name
  r <- as_rtftables(list(a = df, b = df))
  expect_identical(names(r), c("a", "b"))
  # a split input of a list: <list name>.<page name>
  s <- as_rtftables(list(X = d), split = "by_value", group_col = "grp")
  expect_identical(names(s), c("X.A", "X.B"))
  expect_identical(s$X.A$data$v, c("1", "2"))
})

test_that("FT-TB6107 [FR-TB118]: read_meta is TRUE by default, FALSE reads the body only, tokens read what they name (gt, gtsummary, rtables, rlistings, data.frame)", {
  skip_if_not_installed("gt")
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("rtables")
  skip_if_not_installed("rlistings")
  g <- gt::gt(data.frame(a = c("x", "y"), b = c(1.5, 2.5), stringsAsFactors = FALSE))
  g <- gt::tab_spanner(g, "S", columns = c(a, b))
  g <- gt::tab_header(g, title = "TT", subtitle = "SS")
  g <- gt::tab_source_note(g, "SN")
  g <- gt::cols_width(g, a ~ gt::px(100), b ~ gt::px(300))     # 100 px = 1500 twips, 300 px = 4500
  g <- gt::cols_align(g, "right", columns = b)
  g <- gt::cols_label(g, a = "LA")
  g <- gt::tab_style(g, gt::cell_text(weight = "bold"), gt::cells_body(columns = b, rows = 1))
  # the default and TRUE read all of it
  for (all in list(as_rtftables(g)[[1L]], as_rtftables(g, read_meta = TRUE)[[1L]])) {
    expect_length(all$col_header, 2L)
    expect_identical(all$col_header[[2L]], c("LA", "b"))
    expect_identical(all$col_header[[1L]][[1L]]$label, "S")
    expect_identical(all$col_spec[[2L]]$align, "right")
    expect_identical(all$column_widths_twips, c(1500L, 4500L))
    expect_identical(attr(all, "rtf_titles"), c("TT", "SS"))
    expect_identical(attr(all, "rtf_footnotes"), "SN")
    expect_false(is.null(all$cell_styles))
  }
  # FALSE: the rendered body only
  off <- as_rtftables(g, read_meta = FALSE)[[1L]]
  expect_identical(unname(unlist(off$data)), c("x", "y", "1.5", "2.5"))
  expect_null(off$col_header)
  expect_null(off$column_widths_twips)
  expect_null(attr(off, "rtf_titles"))
  expect_null(attr(off, "rtf_footnotes"))
  expect_null(off$cell_styles)
  expect_identical(off$col_spec[[2L]]$align, "center")         # the default, not the gt's "right"
  # each token reads its own piece and nothing else
  tok <- function(t) as_rtftables(g, read_meta = t)[[1L]]
  ch <- tok("col_header")
  expect_identical(ch$col_header, list(c("LA", "b")))
  expect_null(ch$column_widths_twips); expect_null(attr(ch, "rtf_titles")); expect_null(ch$cell_styles)
  sp <- tok("spanning")
  expect_length(sp$col_header, 2L)
  expect_identical(sp$col_header[[1L]][[1L]]$label, "S")
  expect_identical(sp$col_header[[2L]], c("a", "b"))             # the label row keeps the names
  al <- tok("alignment")
  expect_identical(al$col_spec[[2L]]$align, "right")
  expect_null(al$col_header); expect_null(al$column_widths_twips)
  wd <- tok("widths")
  expect_identical(wd$column_widths_twips, c(1500L, 4500L))
  expect_null(wd$col_header); expect_null(attr(wd, "rtf_titles"))
  ti <- tok("titles")
  expect_identical(attr(ti, "rtf_titles"), c("TT", "SS"))
  expect_null(attr(ti, "rtf_footnotes")); expect_null(ti$col_header)
  fo <- tok("footnotes")
  expect_identical(attr(fo, "rtf_footnotes"), "SN")
  expect_null(attr(fo, "rtf_titles"))
  st <- tok("styles")
  expect_false(is.null(st$cell_styles))
  expect_null(st$column_widths_twips); expect_null(st$col_header)

  # gtsummary reads through the same tokens
  gs <- .t11_gts()
  expect_false(is.null(as_rtftables(gs)[[1L]]$col_header))
  expect_null(as_rtftables(gs, read_meta = FALSE)[[1L]]$col_header)
  expect_false(is.null(as_rtftables(gs, read_meta = "col_header")[[1L]]$col_header))
  expect_null(as_rtftables(gs, read_meta = "titles")[[1L]]$col_header)

  # rtables: the indentation is in the labels whatever read_meta says; footnote_marks is its token
  tr <- .t11_tree()
  expect_identical(as_rtftables(tr, read_meta = FALSE)[[1L]]$data[[1L]], c("F", "  Mean", "M", "  Mean"))
  expect_null(as_rtftables(tr, read_meta = FALSE)[[1L]]$col_header)
  expect_false(is.null(as_rtftables(tr)[[1L]]$col_header))
  expect_identical(as_rtftables(tr, read_meta = "footnote_marks")[[1L]]$data[[1L]],
                   c("F", "  Mean", "M", "  Mean"))
  expect_no_error(as_rtftables(tr, read_meta = c("col_header", "footnote_marks")))
  # rlistings takes the same tokens, footnote_marks included
  lst <- rlistings::as_listing(data.frame(a = c("x", "y"), b = c("p", "q"),
                                          stringsAsFactors = FALSE), key_cols = "a")
  expect_no_error(as_rtftables(lst, read_meta = "footnote_marks"))
  expect_null(as_rtftables(lst, read_meta = FALSE)[[1L]]$col_header)
  expect_false(is.null(as_rtftables(lst)[[1L]]$col_header))

  # a data.frame: the "labels" token
  df <- data.frame(a = 1:2, b = 3:4)
  attr(df$a, "label") <- "Label A"
  expect_identical(as_rtftables(df)[[1L]]$col_header, list(c("Label A", "b")))
  expect_identical(as_rtftables(df, read_meta = "labels")[[1L]]$col_header, list(c("Label A", "b")))
  expect_null(as_rtftables(df, read_meta = FALSE)[[1L]]$col_header)
})

test_that("FT-TB6108 [FR-TB148]: na = counts NaN as missing, Inf / -Inf print, the strings 'NA' and 'NaN' stay, the NAs collapse_repeats writes stay blank", {
  m <- data.frame(g = c("a", "a", "b", "b", "b"), v = c(NaN, -Inf, Inf, NA, 2),
                  s = c("NA", "NaN", "NA", "NaN", "x"), stringsAsFactors = FALSE)
  b <- as_rtftables(m, na = "-")[[1L]]$data
  expect_identical(b$v, c("-", "-Inf", "Inf", "-", "2"))      # NaN and NA missing; the infinities not
  expect_identical(b$s, c("NA", "NaN", "NA", "NaN", "x"))      # strings never touched
  # collapse_repeats writes NA for the repeats of g: they are not given na = "-"
  cr <- as_rtftables(m, na = "-", collapse_repeats = "g")[[1L]]$data
  expect_identical(cr$g, c("a", NA, "b", NA, NA))
  expect_identical(cr$v, c("-", "-Inf", "Inf", "-", "2"))
  # and per page: split in 2 rows pages (rows 1-2, rows 3-5); the repeat is NA again,
  # the first row of each page shows its value
  pg <- as_rtftables(m, na = "-", collapse_repeats = "g", split = "rows", split_rows = 3)
  expect_length(pg, 2L)
  expect_identical(pg[[1L]]$data$g, c("a", NA))
  expect_identical(pg[[2L]]$data$g, c("b", NA, NA))
  expect_identical(pg[[2L]]$data$v, c("Inf", "-", "2"))
  # rendered, the collapsed repeats are blank cells, the missing value is the dash, no literal NA
  r <- as_rtftables(m[, c("g", "v")], na = "-", collapse_repeats = "g")
  txt <- .render_to_string(.test_doc_with(r[[1L]]))
  expect_false(grepl("NA", txt, fixed = TRUE))
  expect_match(txt, "-Inf", fixed = TRUE)
  # 5 body rows: g is "a" and "b" once each (3 blank repeats); v is "-" twice (NaN and NA)
  expect_identical(lengths(regmatches(txt, gregexpr("\\ql\\li0\\ri0 \\cell", txt, fixed = TRUE))), 3L)
  expect_identical(lengths(regmatches(txt, gregexpr("\\qc\\li0\\ri0 -\\cell", txt, fixed = TRUE))), 2L)
})

test_that("FT-TB6109 [FR-TB155]: as_rtftables(listing = ) is NULL by default and reshapes a data.frame or tibble as build_listing() does", {
  adsl <- data.frame(
    USUBJID = c("01-701-1015", "01-701-1023", "01-701-1028"),
    AEDECOD = c("HYPERGLYCAEMIA", "UPPER RESPIRATORY TRACT INFECTION", "SKIN ULCER"),
    AELOC   = c("UPPER", NA, "LOWER"),
    ARM     = c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"),
    stringsAsFactors = FALSE)
  sp <- listing_spec(list(
    listing_col("USUBJID", width = 11, label = "Unique\nSubject ID"),
    listing_col(c("AEDECOD", "AELOC"), width = 16, label = "Preferred Term/\nLocation"),
    listing_col("ARM", width = 12, label = "Treatment Arm")))
  # NULL by default: the data.frame as it is, one row per record
  plain <- as_rtftables(adsl)[[1L]]
  expect_identical(names(plain$data), names(adsl))
  expect_identical(nrow(plain$data), 3L)
  expect_identical(names(formals(as_rtftables.default))[names(formals(as_rtftables.default)) == "listing"], "listing")
  expect_null(formals(as_rtftables.default)$listing)
  for (src in list(adsl, tibble::as_tibble(adsl))) {
    tb <- as_rtftables(src, listing = sp)[[1L]]
    b <- tb$data
    # the body is the one build_listing() makes
    built <- build_listing(adsl, sp)
    expect_identical(b$USUBJID, built$USUBJID)
    expect_identical(b$AEDECOD, built$AEDECOD)
    expect_identical(b$ARM, built$ARM)
    # 3 records of 3, 4 and 3 lines (the second wraps to 4 lines): 10 rows, a blank row after each
    expect_identical(nrow(b), 10L)
    expect_identical(names(b), c("USUBJID", ".sp1", "AEDECOD", ".sp2", "ARM"))   # gutters between
    expect_identical(b$USUBJID, c("01-701-1015", "", "", "01-701-1023", "", "", "",
                                  "01-701-1028", "", ""))
    expect_identical(b$AEDECOD, c("HYPERGLYCAEMIA/", "UPPER", "", "UPPER", "RESPIRATORY",
                                  "TRACT INFECTION", "", "SKIN ULCER/", "LOWER", ""))
    expect_identical(b$ARM, c("Placebo", "", "", "Xanomeline", "High Dose", "", "",
                              "Xanomeline", "Low Dose", ""))
    # the spec supplies the header, the relative widths (width, gutter 1) and left alignment
    expect_identical(tb$col_header[[1L]][c(1L, 3L)], c("Unique\nSubject ID", "Preferred Term/\nLocation"))
    expect_identical(tb$col_rel_width, c(11, 1, 16, 1, 12))
    expect_identical(tb$col_spec[[1L]]$align, "left")
    expect_identical(tb$col_spec[[3L]]$align, "left")
    expect_identical(tb$col_spec[[5L]]$align, "left")
  }
})

test_that("FT-TB6110 [FR-TB119]: a data.frame's column label is its header label (on by default, 'labels'), a column without one keeps its name, tokens of other adapters are ignored", {
  skip_if_not_installed("gt")
  df <- data.frame(a = 1:2, b = 3:4, c = 5:6)
  attr(df$a, "label") <- "Label A"
  attr(df$c, "label") <- "Label C"
  want <- list(c("Label A", "b", "Label C"))               # b has no label: keeps its name
  expect_identical(as_rtftables(df)[[1L]]$col_header, want)
  expect_identical(as_rtftables(df, read_meta = TRUE)[[1L]]$col_header, want)
  expect_identical(as_rtftables(df, read_meta = "labels")[[1L]]$col_header, want)
  expect_identical(as_rtftables(tibble::as_tibble(df))[[1L]]$col_header, want)
  # switched off: the names
  expect_null(as_rtftables(df, read_meta = FALSE)[[1L]]$col_header)
  expect_identical(as.vector(as_rtftable(df, read_meta = FALSE)$data$a), 1:2)
  # tokens that mean something to the other adapters only are ignored (no error), and the
  # labels are not read when "labels" is not among the tokens
  expect_no_error(x <- as_rtftables(df, read_meta = c("titles", "widths", "spanning")))
  expect_null(x[[1L]]$col_header)
  expect_identical(as_rtftables(df, read_meta = c("labels", "titles", "styles"))[[1L]]$col_header, want)
  g <- gt::tab_header(gt::gt(data.frame(p = "x", stringsAsFactors = FALSE)), title = "TT")
  # a list mixing a gt table and the data frame shares one read_meta: the gt reads the title,
  # the data frame ignores the token
  only_titles <- as_rtftables(list(g, df), read_meta = "titles")
  expect_identical(attr(only_titles[[1L]], "rtf_titles"), "TT")
  expect_null(only_titles[[2L]]$col_header)
  # an unusable label (empty, NA, two strings) is skipped: the name stays
  bad <- data.frame(a = 1:2, b = 3:4, c = 5:6)
  attr(bad$a, "label") <- ""
  attr(bad$b, "label") <- NA_character_
  attr(bad$c, "label") <- c("x", "y")
  expect_null(as_rtftables(bad)[[1L]]$col_header)            # no usable label: the names print
  expect_identical(names(as_rtftables(bad)[[1L]]$data), c("a", "b", "c"))
})

test_that("FT-TB6111 [FR-TB161]: the stub is built before pagination, so drop_cols, group_col, sort_by, group_by = 'indent' and the splits refer to the reshaped body; a source with an indented stub gets no second one", {
  skip_if_not_installed("gt")
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("tfrmt")
  nb <- intToUtf8(160L)
  ae <- data.frame(SOC = c("Cardiac", "Cardiac", "GI", "GI"),
                   PT = c("Arr", "Tach", "Nau", "Vom"),
                   N = c("3", "2", "5", "1"), M = c("a", "b", "c", "d"),
                   stringsAsFactors = FALSE)
  st <- stub_spec(c("SOC", "PT"))
  # the reshaped body: "SOC / PT", N, M; 6 rows (2 SOC label rows and 4 leaves)
  body <- as_rtftables(ae, stub = st)[[1L]]$data
  expect_identical(names(body), c("SOC / PT", "N", "M"))
  expect_identical(nrow(body), 6L)
  expect_identical(gsub(nb, " ", body[[1L]], fixed = TRUE),
                   c("Cardiac", "    Arr", "    Tach", "GI", "    Nau", "    Vom"))
  # drop_cols and sort_by by position (3 is M in the reshaped body, N in the input)
  r <- as_rtftables(ae, stub = st, drop_cols = 3, sort_by = 2, sort_desc = TRUE)[[1L]]
  expect_identical(names(r$data), c("SOC / PT", "N"))
  expect_identical(r$data$N[1:4], c("5", "3", "2", "1"))
  # drop_cols by the name of a column that is still there
  expect_identical(names(as_rtftables(ae, stub = st, drop_cols = "N")[[1L]]$data), c("SOC / PT", "M"))
  # group_col names the reshaped stub column; group_by = "indent" keeps each SOC with its leaves:
  # max_rows 3 gives Cardiac (3 rows) | GI (3 rows)
  for (gc in list("SOC / PT", 1L)) {
    p <- as_rtftables(ae, stub = st, split = "group_safe", max_rows = 3, group_col = gc,
                      group_by = "indent")
    expect_length(p, 2L)
    expect_identical(gsub(nb, " ", p[[1L]]$data[[1L]], fixed = TRUE), c("Cardiac", "    Arr", "    Tach"))
    expect_identical(gsub(nb, " ", p[[2L]]$data[[1L]], fixed = TRUE), c("GI", "    Nau", "    Vom"))
  }
  # a group-aware split: max_rows 4 with group_safe still cuts between the groups
  p4 <- as_rtftables(ae, stub = st, split = "group_safe", max_rows = 4, group_by = "indent")
  expect_identical(vapply(p4, function(x) nrow(x$data), 1L), c(3L, 3L))

  # a source that already has an indented stub is not given another one
  gs <- .t11_gts()
  expect_identical(as_rtftables(gs, stub = stub_spec("label"))[[1L]]$data, as_rtftables(gs)[[1L]]$data)
  expect_identical(names(as_rtftables(gs, stub = stub_spec("label"))[[1L]]$data), c("label", "stat_1", "stat_2"))
  tr <- .t11_tree()
  expect_identical(as_rtftables(tr, stub = stub_spec("V1"))[[1L]]$data, as_rtftables(tr)[[1L]]$data)

  dat <- expand.grid(group = c("g1", "g2"), label = paste0("lbl", 1:2), column = "A",
                     param = "n", stringsAsFactors = FALSE)
  dat$value <- seq_len(nrow(dat))
  spec <- tfrmt::tfrmt(
    group = group, label = label, column = column, param = param, value = value,
    body_plan = tfrmt::body_plan(
      tfrmt::frmt_structure(group_val = ".default", label_val = ".default", tfrmt::frmt("x"))),
    row_grp_plan = tfrmt::row_grp_plan(
      label_loc = tfrmt::element_row_grp_loc(location = "indent")))
  tg <- tfrmt::print_to_gt(spec, dat)
  tb0 <- as_rtftables(tg)[[1L]]$data
  tb1 <- as_rtftables(tg, stub = stub_spec("rowname"))[[1L]]$data
  expect_identical(names(tb1), names(tb0))
  expect_identical(unname(unlist(tb1)), unname(unlist(tb0)))       # the same cells, no extra stub
  expect_identical(.t11_plain(tb0[[1L]])[1:2], c("g1", "  lbl1")) # indented by the tfrmt
})

# a data.frame of one row with these column names, and its header as text: a spanning row
# as "from-to:label" cells joined with "|", the leaf row as the names joined with "/"
.t11_hdr <- function(nm, ...) {
  df <- stats::setNames(data.frame(matrix("1", 1L, length(nm)), stringsAsFactors = FALSE), nm)
  ch <- as_rtftables(df, ...)[[1L]]$col_header
  vapply(ch, function(r) {
    if (is.character(r)) paste(r, collapse = "/")
    else paste(vapply(r, function(c) sprintf("%d-%d:%s", c$from, c$to, c$label), ""), collapse = "|")
  }, "")
}

test_that("FT-TB6112 [FR-TB162]: header_sep rebuilds the spanning header from the names: stacked rows, merged adjacent cells, leaf names, blank cells, the default separators, NULL, literal separators", {
  # three levels: Trt (columns 2-5) over A (2-3) and B (4-5) over the leaves; label has no
  # separator and keeps its name on the leaf row, Total____n has two segments
  nm <- c("label", "Trt____A____n", "Trt____A____pct", "Trt____B____n", "Trt____B____pct", "Total____n")
  expect_identical(.t11_hdr(nm),
                   c("1-1:|2-5:Trt|6-6:", "1-1:|2-3:A|4-5:B|6-6:Total", "label/n/pct/n/pct/n"))
  # the same label in the same place under another ancestor, or not adjacent, is not merged
  expect_identical(.t11_hdr(c("a", "A____x", "B____x", "A____y")),
                   c("1-1:|2-2:A|3-3:B|4-4:A", "a/x/x/y"))
  expect_identical(.t11_hdr(c("a", "A____x", "A____y", "B____y", "B____z")),
                   c("1-1:|2-3:A|4-5:B", "a/x/y/y/z"))
  # a doubled "____" is a blank cell at that level: Trt________pct is Trt, "", pct
  expect_identical(.t11_hdr(c("label", "Trt____n", "Trt________pct")),
                   c("1-2:|3-3:Trt", "1-1:|2-2:Trt|3-3:", "label/n/pct"))
  # the default recognizes both "____" and tfrmt's "___tlang_delim___", alone or alike
  expect_identical(.t11_hdr(c("label", "T___tlang_delim___A", "T___tlang_delim___B")),
                   c("1-1:|2-3:T", "label/A/B"))
  expect_identical(.t11_hdr(c("label", "T____A", "T____B")), c("1-1:|2-3:T", "label/A/B"))
  expect_identical(.t11_hdr(c("label", "T____A", "T___tlang_delim___B")), c("1-1:|2-3:T", "label/A/B"))
  # NULL disables it: no spanning header, the names stay
  df <- stats::setNames(data.frame("1", "2", "3"), c("label", "T____A", "T____B"))
  expect_null(as_rtftables(df, header_sep = NULL)[[1L]]$col_header)
  expect_identical(names(as_rtftables(df, header_sep = NULL)[[1L]]$data), c("label", "T____A", "T____B"))
  # a name with no separator at all: no header is built
  expect_null(as_rtftables(stats::setNames(data.frame("1", "2"), c("a", "b")))[[1L]]$col_header)
  # a separator of your own, literal text: "." "|" and "\E" (nothing is a regular expression)
  expect_identical(.t11_hdr(c("a", "T.A.1", "T.A.2", "T.B.1"), header_sep = "."),
                   c("1-1:|2-4:T", "1-1:|2-3:A|4-4:B", "a/1/2/1"))
  expect_identical(.t11_hdr(c("a", "A|x", "A|y"), header_sep = "|"), c("1-1:|2-3:A", "a/x/y"))
  expect_identical(.t11_hdr(c("a", "A\\Ex", "A\\Ey"), header_sep = "\\E"), c("1-1:|2-3:A", "a/x/y"))
  # "T____A" is not split by "."; the default separators are replaced, not added to
  expect_null(as_rtftables(df, header_sep = ".")[[1L]]$col_header)
  # several separators: the longest matching one wins
  expect_identical(.t11_hdr(c("id", "A.|x", "A.|y"), header_sep = c(".", ".|")), c("1-1:|2-3:A", "id/x/y"))
  expect_identical(.t11_hdr(c("id", "A.|x", "A.|y"), header_sep = c(".|", ".")), c("1-1:|2-3:A", "id/x/y"))
  # a bad type stops
  expect_error(as_rtftables(df, header_sep = 1),
               "`header_sep` must be NULL or a character vector of separators.", fixed = TRUE)
})

# the cell right edges (twips) of the first row of a one-table document
.t11_cellx <- function(tb) {
  txt <- .render_to_string(.test_doc_with(tb))
  l <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
  l <- l[grepl("cellx", l, fixed = TRUE)][1L]
  as.integer(sub("cellx", "", regmatches(l, gregexpr("cellx[0-9]+", l))[[1L]], fixed = TRUE))
}

test_that("FT-TB6113 [FR-TB163]: auto_width = TRUE sizes each column to its widest content via auto_col_widths() and divides the table, it does not set its width; FALSE is the default", {
  df <- data.frame(lab = c("a", "bbbbbbbbbbbbbbbbbb"), x = c("1", "2"), z = c("1", "2"),
                   stringsAsFactors = FALSE)
  names(df)[2L] <- "A very long header label here"
  expect_identical(formals(as_rtftables.default)$auto_width, FALSE)
  on <- as_rtftables(df, auto_width = TRUE)[[1L]]
  off <- as_rtftables(df, auto_width = FALSE)[[1L]]
  # the widths are those of auto_col_widths() on the data and the header labels
  w <- auto_col_widths(df, col_header = names(df))
  expect_identical(on$column_widths_twips, as.integer(w))
  expect_identical(w, c(2237L, 3429L, 720L))                  # widest content: header 29 letters, cell 18, 1
  expect_gt(w[2L], w[1L])
  expect_gt(w[1L], w[3L])
  # FALSE is the default: nothing is sized, the same as leaving it out
  expect_null(off$column_widths_twips)
  expect_identical(as_rtftables(df)[[1L]]$column_widths_twips, off$column_widths_twips)
  # the width of the table is not its business: given a width, the right edge is that width
  # either way and only the division of the columns differs
  on6 <- as_rtftables(df, auto_width = TRUE, table_width_twips = 6000L)[[1L]]
  off6 <- as_rtftables(df, auto_width = FALSE, table_width_twips = 6000L)[[1L]]
  ex_on <- .t11_cellx(on6)
  ex_off <- .t11_cellx(off6)
  expect_identical(ex_on[3L], 6000L)
  expect_identical(ex_off[3L], 6000L)
  expect_false(identical(ex_on, ex_off))
  expect_identical(ex_on, cumsum(on6$column_widths_twips))     # the right edges are the widths added up
  # an explicit width of the columns still wins over auto_width
  ex <- as_rtftables(df, auto_width = TRUE, column_widths_twips = c(1000L, 2000L, 3000L))[[1L]]
  expect_identical(ex$column_widths_twips, c(1000L, 2000L, 3000L))
})

test_that("FT-TB6114 [FR-TB164]: table_width_twips is the table's total width in twips, NULL by default, as rtftable(table_width_twips = ) means it, with or without auto_width", {
  df <- data.frame(lab = c("a", "b"), x = c("1", "2"), z = c("3", "4"), stringsAsFactors = FALSE)
  expect_null(formals(as_rtftables.default)$table_width_twips)
  expect_null(as_rtftables(df)[[1L]]$table_width_twips)
  # NULL: the writable page width, 13680 twips (the default page); 3 equal columns of 4560
  expect_identical(.t11_cellx(as_rtftables(df)[[1L]])[3L], 13680L)
  for (aw in c(FALSE, TRUE)) {
    tb <- as_rtftables(df, table_width_twips = 6000L, auto_width = aw)[[1L]]
    expect_identical(tb$table_width_twips, 6000L)
    expect_identical(.t11_cellx(tb)[3L], 6000L)               # exactly this wide
  }
  # col_rel_width apportions it: 1 : 2 : 3 of 6000 is 1000, 2000, 3000
  rel <- as_rtftables(df, table_width_twips = 6000L, col_rel_width = c(1, 2, 3))[[1L]]
  expect_identical(.t11_cellx(rel), c(1000L, 3000L, 6000L))
  # as rtftable() means it
  rt <- rtftable(df, table_width_twips = 6000L, col_rel_width = c(1, 2, 3))
  expect_identical(.t11_cellx(rt), .t11_cellx(rel))
  expect_identical(.t11_cellx(rtftable(df, table_width_twips = 6000L))[3L], 6000L)
  # and wider than the page it is that wide (not capped by the page)
  expect_identical(.t11_cellx(as_rtftables(df, table_width_twips = 9000L)[[1L]])[3L], 9000L)
})

test_that("FT-TB6115 [FR-TB166]: as_rtftables(...) forwards col_header, col_spec, row_title, col_rel_width, column_widths_twips, row_height_twips and cell_styles to every page; an explicit value wins over the source's; col_header is resolved after the stub and drop_cols", {
  skip_if_not_installed("gt")
  d <- data.frame(a = c("1", "2", "3"), b = c("4", "5", "6"), c = c("7", "8", "9"),
                  stringsAsFactors = FALSE)
  bold1 <- lapply(1:3, function(i) list(bold = i == 1L))
  # split_rows = 2 gives 2 pages (row 1 | rows 2-3); c is dropped, so 2 columns print
  p <- as_rtftables(d, split = "rows", split_rows = 2, drop_cols = "c",
                    col_header = c("A", "B"), col_spec = list(list(col = 2, align = "right")),
                    row_title = 2, col_rel_width = c(1, 3), row_height_twips = 300L)
  expect_length(p, 2L)
  for (pg in p) {
    expect_identical(pg$col_header, list(c("A", "B")))        # against the 2 printed columns
    expect_identical(pg$col_spec[[2L]]$align, "right")
    expect_identical(pg$row_title, 2L)
    expect_identical(pg$col_rel_width, c(1, 3))
    expect_identical(pg$row_height_twips, 300L)
  }
  w <- as_rtftables(d, split = "rows", split_rows = 2, column_widths_twips = c(1000L, 2000L, 3000L))
  expect_identical(w[[1L]]$column_widths_twips, c(1000L, 2000L, 3000L))
  expect_identical(w[[2L]]$column_widths_twips, c(1000L, 2000L, 3000L))
  cs <- as_rtftables(d, split = "rows", split_rows = 2, cell_styles = bold1)
  expect_length(cs, 2L)
  expect_true(cs[[1L]]$cell_styles[[1L]]$bold)               # row 1 is bold, on page 1
  expect_false(isTRUE(cs[[2L]]$cell_styles[[1L]]$bold))
  # the value is the one rtftable() gets
  expect_identical(rtftable(d, row_title = 2, row_height_twips = 300L)$row_title, 2L)
  # a col_header is resolved after the stub folds the hierarchy: 2 columns, not the input's 4
  s <- data.frame(soc = c("S1", "S1"), pt = c("p1", "p2"), n = c("1", "2"), x = c("q", "r"),
                  stringsAsFactors = FALSE)
  sp <- as_rtftables(s, stub = stub_spec(c("soc", "pt")), drop_cols = "x", col_header = c("Term", "N"))[[1L]]
  expect_identical(sp$col_header, list(c("Term", "N")))
  expect_error(as_rtftables(s, stub = stub_spec(c("soc", "pt")), drop_cols = "x",
                            col_header = c("a", "b", "c", "d")),
               "the label row has 4 labels but the table has 2 printed columns", fixed = TRUE)
  # an explicit value wins over the one read from the source (a gt table's alignment and widths)
  g <- gt::gt(data.frame(a = c("x", "y"), b = c(1.5, 2.5), stringsAsFactors = FALSE))
  g <- gt::cols_width(g, a ~ gt::px(100), b ~ gt::px(300))
  g <- gt::cols_align(g, "right", columns = b)
  expect_identical(as_rtftables(g)[[1L]]$col_spec[[2L]]$align, "right")
  expect_identical(as_rtftables(g)[[1L]]$column_widths_twips, c(1500L, 4500L))
  e <- as_rtftables(g, col_spec = list(list(col = 2, align = "left")),
                    column_widths_twips = c(1000L, 2000L))[[1L]]
  expect_identical(e$col_spec[[2L]]$align, "left")
  expect_identical(e$column_widths_twips, c(1000L, 2000L))
})

test_that("FT-TB6116 [FR-TB167]: a caller's cell_styles with one element per body row follow their rows through the stub, the sort and the page split", {
  d <- data.frame(k = c("c", "a", "b"), v = c("3", "1", "2"), stringsAsFactors = FALSE)
  # row i is bold when i is 1, i.e. the row "c / 3" in the caller's order
  sty <- lapply(1:3, function(i) list(bold = i == 1L))
  bolds <- function(pages) lapply(pages, function(p) vapply(p$cell_styles, function(s) isTRUE(s$bold), NA))
  # the sort moves "c" to the last row: the bold follows it
  s <- as_rtftables(d, sort_by = "k", cell_styles = sty)[[1L]]
  expect_identical(s$data$k, c("a", "b", "c"))
  expect_identical(bolds(list(s))[[1L]], c(FALSE, FALSE, TRUE))
  # sorted descending, "c" is first
  sd <- as_rtftables(d, sort_by = "k", sort_desc = TRUE, cell_styles = sty)[[1L]]
  expect_identical(bolds(list(sd))[[1L]], c(TRUE, FALSE, FALSE))
  # the split: 3 sorted rows in pages of 2 rows; the bold row is the 3rd, on page 2 (one row)
  pg <- as_rtftables(d, sort_by = "k", cell_styles = sty, split = "group_force", max_rows = 2, group_col = 1)
  expect_length(pg, 2L)
  expect_identical(vapply(pg, function(p) length(p$cell_styles), 1L), c(2L, 1L))   # not handed whole
  expect_identical(bolds(pg), list(c(FALSE, FALSE), TRUE))
  # through the stub: the rows of the stubbed body are 2 SOC label rows and 3 leaves
  st <- data.frame(soc = c("S1", "S1", "S2"), pt = c("p1", "p2", "p3"), n = c("1", "2", "3"),
                   stringsAsFactors = FALSE)
  sty3 <- lapply(1:3, function(i) list(bold = i == 3L))
  q <- as_rtftables(st, cell_styles = sty3, stub = stub_spec(c("soc", "pt")))[[1L]]
  expect_identical(.t11_plain(q$data[[1L]]), c("S1", "    p1", "    p2", "S2", "    p3"))
  expect_length(q$cell_styles, 5L)
  expect_identical(bolds(list(q))[[1L]], c(FALSE, FALSE, FALSE, FALSE, TRUE))   # p3 keeps its style
})

test_that("FT-TB6117 [FR-TB170]: rtf_tables(tables = ) takes a table_plan, as_rtftables() pages, a bare data.frame, an rtftable, an rtfplot, a gt_tbl, a gtsummary table, a list of these and an object with a registered as_rtftables() method", {
  skip_if_not_installed("cards")
  skip_if_not_installed("gt")
  skip_if_not_installed("gtsummary")
  content <- function(doc) lapply(doc$contents, function(b) b$content)
  # a table_plan: its pages as plan_apply() makes them
  ard <- cards::ard_tabulate(cards::ADSL, by = ARM, variables = c(SEX, AGEGR1))
  p <- suppressMessages(table_plan(normalize_ard(ard), cols = "ARM"))
  p <- plan_paginate_rows(p, max_rows = 4, split = "group_force")
  pages <- suppressMessages(plan_apply(p, "pages"))
  expect_gt(length(pages), 1L)
  dp <- suppressMessages(rtf_tables(rtf_document(), p))
  expect_length(dp$contents, length(pages))
  expect_identical(content(dp)[[2L]]$data, pages[[2L]]$data)
  # the output of as_rtftables(): a block per page
  d4 <- data.frame(Item = paste("Item", 1:4), A = 1:4, stringsAsFactors = FALSE)
  pg <- as_rtftables(d4, split = "rows", split_rows = 3)
  da <- rtf_tables(rtf_document(), pg)
  expect_length(da$contents, 2L)
  expect_identical(content(da)[[2L]]$data$Item, c("Item 3", "Item 4"))
  # a bare data.frame: one simple page with rtftable() defaults
  dd <- rtf_tables(rtf_document(), d4)
  expect_length(dd$contents, 1L)
  expect_s3_class(content(dd)[[1L]], "rtftable")
  expect_identical(content(dd)[[1L]]$data, rtftable(d4)$data)
  expect_identical(content(dd)[[1L]]$border, rtftable(d4)$border)
  # an rtftable, with the format it was given
  rt <- rtftable(d4, table_width_twips = 6000L)
  dr <- rtf_tables(rtf_document(), rt)
  expect_identical(content(dr)[[1L]]$table_width_twips, 6000L)
  expect_identical(content(dr)[[1L]]$data, rt$data)
  # an rtfplot
  png <- tempfile(fileext = ".png")
  on.exit(unlink(png), add = TRUE)
  grDevices::png(png, width = 200, height = 150)
  graphics::plot(1)
  grDevices::dev.off()
  dg <- rtf_tables(rtf_document(), rtfplot(png))
  expect_s3_class(content(dg)[[1L]], "rtfplot")
  # a gt_tbl and a gtsummary table
  g <- gt::gt(data.frame(a = c("x", "y"), b = c("1", "2"), stringsAsFactors = FALSE))
  d3 <- rtf_tables(rtf_document(), g)
  expect_s3_class(content(d3)[[1L]], "rtftable")
  expect_identical(content(d3)[[1L]]$data$a, c("x", "y"))
  tb <- gtsummary::tbl_summary(gtsummary::trial, include = c(age, grade))
  ds <- rtf_tables(rtf_document(), tb)
  expect_identical(content(ds)[[1L]]$data$label[1L], "Age")
  expect_identical(content(ds)[[1L]]$data, as_rtftables(tb)[[1L]]$data)
  # an object of a class with a registered as_rtftables() method, alone and in a list
  registerS3method("as_rtftables", "rtfr_t11_source",
                   function(x, ...) as_rtftables(x$df, ...), envir = asNamespace("rtfreporter"))
  src <- structure(list(df = data.frame(P = c("A", "B"), V = c("1", "2"), stringsAsFactors = FALSE)),
                   class = "rtfr_t11_source")
  dm <- rtf_tables(rtf_document(), src)
  expect_length(dm$contents, 1L)
  expect_identical(content(dm)[[1L]]$data$P, c("A", "B"))
  # a list of these, in order
  dl <- rtf_tables(rtf_document(), list(g, data.frame(c = "z", stringsAsFactors = FALSE),
                                        rtfplot(png), src, rt))
  expect_identical(vapply(content(dl), function(b) class(b)[1L], ""),
                   c("rtftable", "rtftable", "rtfplot", "rtftable", "rtftable"))
  expect_identical(content(dl)[[2L]]$data$c, "z")
  expect_identical(content(dl)[[4L]]$data$V, c("1", "2"))
})

test_that("FT-TB6118 [FR-TB192]: rtf_tables() given a gt or gtsummary table, alone or in a list, reads its labels, alignment, titles and source notes as as_rtftables() does; read_gt is gone", {
  skip_if_not_installed("gt")
  skip_if_not_installed("gtsummary")
  g <- gt::gt(data.frame(a = c("x", "y"), b = c(1.5, 2.5), stringsAsFactors = FALSE))
  g <- gt::cols_label(g, a = "GA", b = "GB")
  g <- gt::cols_align(g, "right", columns = b)
  g <- gt::tab_header(g, title = "TT", subtitle = "SS")
  g <- gt::tab_source_note(g, "SN")
  gs <- gtsummary::modify_source_note(
    gtsummary::modify_header(
      gtsummary::modify_column_alignment(.t11_gts(), columns = stat_1, align = "right"),
      label = "**Var**"),
    "Src N")
  df <- data.frame(c = "z", stringsAsFactors = FALSE)
  for (src in list(g, gs)) {
    own <- as_rtftables(src)[[1L]]
    for (doc in list(rtf_tables(rtf_document(), src), rtf_tables(rtf_document(), list(src, df)),
                     rtf_tables(rtf_document(), list(df, src)))) {
      blk <- doc$contents[[if (length(doc$contents) == 2L && identical(doc$contents[[1L]]$content$data$c, "z")) 2L else 1L]]
      expect_identical(blk$content$col_header, own$col_header)          # the labels
      expect_identical(blk$content$col_spec, own$col_spec)              # the alignment
      expect_identical(blk$title, attr(own, "rtf_titles"))              # the titles
      expect_identical(blk$footnote, attr(own, "rtf_footnotes"))        # the source notes
      expect_identical(blk$content$data, own$data)
    }
  }
  # the values, worked out: gt
  b <- rtf_tables(rtf_document(), g)$contents[[1L]]
  expect_identical(b$content$col_header, list(c("GA", "GB")))
  expect_identical(b$content$col_spec[[2L]]$align, "right")
  expect_identical(b$title, c("TT", "SS"))
  expect_identical(b$footnote, "SN")
  # the values, worked out: gtsummary
  s <- rtf_tables(rtf_document(), gs)$contents[[1L]]
  expect_identical(s$content$col_header[[1L]][1L], "Var")
  expect_identical(s$content$col_spec[[2L]]$align, "right")
  expect_true("Src N" %in% s$footnote)
  # read_gt = was removed in 0.9.0
  expect_error(rtf_tables(rtf_document(), g, read_gt = TRUE), "unused argument")
})

test_that("FT-TB6119 [FR-TB577]: from a gt_tbl or gtsummary table as_rtftables() reads the labels, alignment, spanners, widths (px and pct), title and subtitle, source notes and the in-cell footnote marks", {
  skip_if_not_installed("gt")
  skip_if_not_installed("gtsummary")
  g <- gt::gt(data.frame(a = c("x", "y"), b = c(1.5, 2.5), c = c(3, 4)))
  g <- gt::cols_label(g, a = "LA")
  g <- gt::cols_align(g, "right", columns = c)
  g <- gt::tab_spanner(g, "Sp", columns = c(b, c))
  g <- gt::tab_header(g, title = "TT", subtitle = "SS")
  g <- gt::tab_source_note(g, "SN")
  g <- gt::tab_footnote(g, "FN1", locations = gt::cells_body(columns = b, rows = 1))
  px <- gt::cols_width(g, a ~ gt::px(60), b ~ gt::px(120), c ~ gt::px(180))
  p <- as_rtftables(px)[[1L]]
  expect_identical(p$col_header[[2L]], c("LA", "b", "c"))                  # the column labels
  expect_identical(p$col_spec[[3L]]$align, "right")                         # the per-column alignment
  expect_identical(vapply(p$col_header[[1L]], function(z) c(z$from, z$to), integer(2L)),
                   matrix(c(1L, 1L, 2L, 3L), nrow = 2L))                    # the spanner over 2-3
  expect_identical(vapply(p$col_header[[1L]], function(z) z$label, ""), c("", "Sp"))
  expect_identical(p$column_widths_twips, c(900L, 1800L, 2700L))            # px * 15: 60, 120, 180
  expect_null(p$col_rel_width)
  expect_identical(attr(p, "rtf_titles"), c("TT", "SS"))                    # the title and subtitle
  expect_identical(attr(p, "rtf_footnotes"), c("FN1", "SN"))                # the note, then the source note
  expect_identical(.t11_plain(p$data$b), c("^{1} 1.5", "2.5"))                          # the mark as ^{N}
  # pct widths go to col_rel_width
  pc <- as_rtftables(gt::cols_width(g, a ~ gt::pct(20), b ~ gt::pct(30), c ~ gt::pct(50)))[[1L]]
  expect_identical(pc$col_rel_width, c(20, 30, 50))
  expect_null(pc$column_widths_twips)
  # a gtsummary table: labels, alignment, spanner, footnote text
  gs <- gtsummary::modify_spanning_header(
    gtsummary::modify_footnote(
      gtsummary::modify_column_alignment(.t11_gts(), columns = stat_1, align = "right"), label = "Foot L"),
    c(stat_1, stat_2) ~ "Trt")
  s <- as_rtftables(gs)[[1L]]
  expect_identical(s$col_header[[2L]], c("Characteristic", "Drug A  \nN = 98", "Drug B  \nN = 102"))
  expect_identical(s$col_spec[[2L]]$align, "right")
  expect_identical(vapply(s$col_header[[1L]], function(z) z$label, ""), c("", "Trt"))
  expect_true("Foot L" %in% attr(s, "rtf_footnotes"))
})

test_that("FT-TB6120 [FR-TB578]: explicit gt::tab_style() declarations are carried (borders, bold, italic, underline, align on spanners, labels, body and stub cells); a label border promotes the label row; the verbs override; transparent theme borders are not carried", {
  skip_if_not_installed("gt")
  d <- data.frame(Item = c("Age", "Sex"), P = c("1", "2"), A = c("3", "4"), B = c("5", "6"),
                  stringsAsFactors = FALSE)
  txt <- function(a) gt::cell_text(weight = "bold", style = "italic", decorate = "underline", align = a)
  g <- gt::gt(d, rowname_col = "Item")
  g <- gt::tab_spanner(g, label = "Sp", columns = c(P, A, B), id = "sp")
  g <- gt::tab_style(g, txt("left"), gt::cells_column_spanners("sp"))
  g <- gt::tab_style(g, gt::cell_borders(sides = "bottom", style = "double", weight = gt::px(2)),
                     gt::cells_column_labels(columns = A))
  g <- gt::tab_style(g, txt("right"), gt::cells_body(columns = P, rows = 2))
  g <- gt::tab_style(g, gt::cell_borders(sides = "top", style = "solid", weight = gt::px(1)),
                     gt::cells_body(columns = B, rows = 1))
  g <- gt::tab_style(g, txt("center"), gt::cells_stub(rows = 2))
  g <- gt::tab_style(g, gt::cell_borders(sides = "bottom", color = "transparent"),
                     gt::cells_body(columns = A, rows = 2))
  p <- as_rtftables(g)[[1L]]
  expect_identical(names(p$data), c("rowname", "P", "A", "B"))
  # the spanner: bold, italic, underline, left
  sp <- p$col_header[[1L]][[2L]]
  expect_identical(sp$label, "Sp")
  expect_true(sp$bold); expect_true(sp$italic); expect_true(sp$underline)
  expect_identical(sp$align, "left")
  # a border on a column label promotes the label row to single-column cells: one per column
  lab <- p$col_header[[2L]]
  expect_false(is.character(lab))
  expect_length(lab, 4L)
  expect_identical(vapply(lab, function(z) z$label, ""), c("", "P", "A", "B"))
  expect_identical(lab[[3L]]$border$bottom$style, "double")
  expect_identical(lab[[3L]]$border$bottom$width, 30L)           # 2 px = 30 twips
  expect_null(lab[[2L]]$border)
  # the body: row 2 / column P bold italic underline right; row 1 / column B a single top border
  cs <- p$cell_styles
  expect_identical(cs[[2L]]$bold, c(TRUE, TRUE, NA, NA))
  expect_identical(cs[[2L]]$italic, c(TRUE, TRUE, NA, NA))
  expect_identical(cs[[2L]]$underline, c(TRUE, TRUE, NA, NA))
  expect_identical(cs[[2L]]$align, c("center", "right", NA, NA))  # the stub cell is centred, P right
  expect_identical(cs[[1L]]$border[[4L]]$top$style, "single")
  expect_identical(cs[[1L]]$border[[4L]]$top$width, 15L)          # 1 px = 15 twips
  # the transparent border on A / row 2 is not carried: the row has no border entry
  expect_null(cs[[2L]]$border)
  # the other verbs override what was read
  o <- style_header(p, row = 1, cols = 2:4, bold = FALSE)
  expect_false(isTRUE(o$col_header[[1L]][[2L]]$bold))
  ob <- style_body(p, rows = 2, cols = 2, bold = FALSE)
  expect_identical(ob$cell_styles[[2L]]$bold, c(TRUE, FALSE, NA, NA))
  oc <- style_cols(p, cols = 2, align = "left")
  expect_identical(oc$col_spec[[2L]]$align, "left")
  # without the "styles" token, none of it is read
  none <- as_rtftables(g, read_meta = c("col_header", "spanning"))[[1L]]
  expect_null(none$cell_styles)
  expect_true(is.character(none$col_header[[2L]]))
  # transparent borders alone: nothing carried, the label row not promoted
  tg <- gt::tab_style(gt::gt(d), gt::cell_borders(sides = "top", color = "#FFFFFF00"),
                      gt::cells_column_labels(columns = B))
  tp <- as_rtftables(tg)[[1L]]
  expect_null(tp$cell_styles)
  expect_true(is.character(tp$col_header[[1L]]))
})

test_that("FT-TB6121 [FR-TB579]: from an rtables VTableTree as_rtftables() reads the column labels, row labels with their indent in the stub, the nested column split as a spanning header, titles and footers, and the {N} marks as ^{N}", {
  skip_if_not_installed("rtables")
  skip_if_not_installed("formatters")
  df <- data.frame(ARM = rep(c("A", "B"), each = 4), SEX = rep(c("F", "M"), 4),
                   RACE = rep(c("W", "B"), each = 2, times = 2),
                   AGE = c(40, 50, 30, 36, 41, 51, 31, 37), stringsAsFactors = FALSE)
  afun <- function(x) rtables::in_rows(
    "Mean" = rtables::rcell(mean(x), format = "xx.x", footnotes = list("Mean note")))
  lyt <- rtables::basic_table(title = "TTL", subtitles = "SUB", main_footer = "MF", prov_footer = "PF")
  lyt <- rtables::split_cols_by(lyt, "ARM")
  lyt <- rtables::split_cols_by(lyt, "RACE")
  lyt <- rtables::split_rows_by(lyt, "SEX")
  lyt <- rtables::analyze(lyt, "AGE", afun = afun)
  tr <- rtables::build_table(lyt, df)
  p <- as_rtftables(tr)[[1L]]
  # the row labels: SEX rows flush left, the analysis row indented (2 spaces) in the stub
  expect_identical(p$data[[1L]], c("F", "  Mean", "M", "  Mean"))
  # means: ARM A / W: F 40, M 50; A / B: F 30, M 36; B / W: 41, 51; B / B: 31, 37 -- the group
  # rows are empty and the cells carry the footnote mark as ^{1}
  expect_identical(.t11_plain(p$data[[2L]]), c("", "40.0 ^{1}", "", "50.0 ^{1}"))
  expect_identical(.t11_plain(p$data[[3L]]), c("", "30.0 ^{1}", "", "36.0 ^{1}"))
  expect_identical(.t11_plain(p$data[[4L]]), c("", "41.0 ^{1}", "", "51.0 ^{1}"))
  expect_identical(.t11_plain(p$data[[5L]]), c("", "31.0 ^{1}", "", "37.0 ^{1}"))
  # the column labels: the leaf row is the RACE labels, the ARM labels span 2 columns each
  expect_length(p$col_header, 2L)
  expect_identical(p$col_header[[2L]], c("", "W", "B", "W", "B"))
  spans <- p$col_header[[1L]]
  expect_identical(vapply(spans, function(z) z$label, ""), c("", "A", "B"))
  expect_identical(vapply(spans, function(z) z$from, 1L), c(1L, 2L, 4L))
  expect_identical(vapply(spans, function(z) z$to, 1L), c(1L, 3L, 5L))
  # the titles and the footers (the footnote text first)
  expect_identical(attr(p, "rtf_titles"), c("TTL", "SUB"))
  expect_identical(attr(p, "rtf_footnotes"), c("{1} - Mean note", "MF", "PF"))
  # a table without a nested split has one label row and no spanning row
  one <- rtables::analyze(rtables::split_cols_by(rtables::basic_table(), "ARM"), "AGE", afun = afun)
  p1 <- as_rtftables(rtables::build_table(one, df))[[1L]]
  expect_identical(p1$col_header, list(c("", "A", "B")))
})

test_that("FT-TB6122 [FR-TB580]: from a flextable as_rtftables() reads the displayed text (labels and colformat_*() included), the leaf labels and spanning header, the per-column alignment, the caption as title and the footer as footnote", {
  skip_if_not_installed("flextable")
  d <- data.frame(a = c("x", "y"), b = c(1.234, 2.5), c = c(1234, 20), stringsAsFactors = FALSE)
  ft <- flextable::flextable(d)
  ft <- flextable::set_header_labels(ft, a = "LA", b = "LB")
  ft <- flextable::colformat_double(ft, j = "b", digits = 1)               # 1.234 -> 1.2, 2.5 -> 2.5
  ft <- flextable::colformat_num(ft, j = "c", big.mark = ",")              # 1234 -> 1,234
  ft <- flextable::add_header_row(ft, values = c("", "Spn"), colwidths = c(1, 2))
  ft <- flextable::align(ft, j = "b", align = "center", part = "body")
  ft <- flextable::align(ft, j = "c", align = "right", part = "body")
  ft <- flextable::set_caption(ft, "Cap T")
  ft <- flextable::add_footer_lines(ft, "Foot 1")
  p <- as_rtftables(ft)[[1L]]
  expect_identical(unname(unlist(p$data)), c("x", "y", "1.2", "2.5", "1,234", "20"))   # as displayed
  # the leaf labels (set_header_labels) and the spanning row over columns 2-3
  expect_length(p$col_header, 2L)
  expect_identical(p$col_header[[2L]], c("LA", "LB", "c"))
  expect_identical(vapply(p$col_header[[1L]], function(z) z$label, ""), c("", "Spn"))
  expect_identical(vapply(p$col_header[[1L]], function(z) c(z$from, z$to), integer(2L)),
                   matrix(c(1L, 1L, 2L, 3L), nrow = 2L))
  expect_identical(vapply(p$col_spec, function(s) s$align, ""), c("left", "center", "right"))
  expect_identical(attr(p, "rtf_titles"), "Cap T")
  expect_identical(attr(p, "rtf_footnotes"), "Foot 1")
  # without a caption or footer there is no title or footnote
  q <- as_rtftables(flextable::flextable(d))[[1L]]
  expect_null(attr(q, "rtf_titles"))
  expect_null(attr(q, "rtf_footnotes"))
  # a formatter set with set_formatter() is read as displayed too
  f <- as_rtftables(flextable::set_formatter(flextable::flextable(d), b = function(x) sprintf("<%.1f>", x)))
  expect_identical(f[[1L]]$data[[2L]], c("<1.2>", "<2.5>"))
})

test_that("FT-TB6123 [FR-TB581]: from a huxtable as_rtftables() reads the displayed text (number_format applied), the leaf labels and spanning header, the per-column alignment and the caption as the title; no footnotes", {
  skip_if_not_installed("huxtable")
  d <- data.frame(a = c("x", "y"), b = c(1.234, 2.5), c = c(10, 20), stringsAsFactors = FALSE)
  hx <- huxtable::as_hux(d, add_colnames = TRUE)
  hx <- huxtable::insert_row(hx, "", "Sp", "", after = 0)
  hx <- huxtable::set_colspan(hx, 1, 2, 2)
  hx <- huxtable::set_header_rows(hx, 1:2, TRUE)
  hx <- huxtable::set_number_format(hx, huxtable::everywhere, 2, 2)           # 2 digits on column b
  hx <- huxtable::set_align(hx, huxtable::everywhere, 3, "right")
  hx <- huxtable::set_caption(hx, "HCap")
  p <- as_rtftables(hx)[[1L]]
  expect_identical(unname(unlist(p$data)), c("x", "y", "1.23", "2.50", "10", "20"))
  expect_length(p$col_header, 2L)
  expect_identical(p$col_header[[2L]], c("a", "b", "c"))                  # the leaf labels
  expect_identical(vapply(p$col_header[[1L]], function(z) z$label, ""), c("", "Sp"))
  expect_identical(vapply(p$col_header[[1L]], function(z) c(z$from, z$to), integer(2L)),
                   matrix(c(1L, 1L, 2L, 3L), nrow = 2L))               # Sp spans columns 2-3
  expect_identical(p$col_spec[[3L]]$align, "right")
  expect_identical(attr(p, "rtf_titles"), "HCap")
  expect_null(attr(p, "rtf_footnotes"))                                   # huxtable has no footnotes
  # no caption, no title; a number_format of 0 digits rounds
  h2 <- huxtable::set_number_format(huxtable::hux(data.frame(a = "x", b = 1.6, stringsAsFactors = FALSE)),
                                    huxtable::everywhere, 2, 0)
  p2 <- as_rtftables(h2)[[1L]]
  expect_identical(p2$data[[2L]], "2")
  expect_null(attr(p2, "rtf_titles"))
})

# ---- the flextable and huxtable that the RTF writer must take (FR-TB632, FR-TB633) ----

.t11_ft_df <- function() {
  data.frame(Param = c("Age", "Weight", "Height"), Mean = c(55.1234, 70.5, 170.25),
             SD = c(10.5, 12.25, 8), N = c(30L, 30L, 29L), stringsAsFactors = FALSE)
}

# pages of x written as RTF without an error: the pages and the RTF text
.t11_render <- function(x, ...) {
  pages <- expect_no_error(as_rtftables(x, ...))
  out <- tempfile(fileext = ".rtf")
  on.exit(unlink(out), add = TRUE)
  doc <- rtf_tables(rtf_document(), pages)
  expect_no_error(generate_rtfreport(doc, out, overwrite = TRUE))
  list(pages = pages, txt = paste(readLines(out, warn = FALSE), collapse = "\n"))
}

# the body cells as a matrix, trimmed
.t11_cells <- function(rt) {
  m <- as.matrix(rt$data)
  m[] <- trimws(m, whitespace = "[\\h\\v]")
  unname(m)
}

.t11_n <- function(txt, s) {
  m <- gregexpr(s, txt, fixed = TRUE)[[1L]]
  if (m[1L] < 0L) 0L else length(m)
}

# no markup left in the RTF text, no literal NA
.t11_clean <- function(txt) {
  for (m in c("<br>", "<tab>", "&amp;", "&lt;", "&nbsp;"))
    expect_false(grepl(m, txt, fixed = TRUE), info = m)
  expect_false(grepl("(^|[ }])NA(\\\\cell|\\\\line|\\\\par| |$)", txt), info = "literal NA")
}

test_that("FT-TB6124 [FR-TB632]: a flextable with header labels, merges, colformat_*(), footers and footnote() marks, or no rows, is read and written without an error: the displayed values, escaped, once per merged area, no markup, no literal NA", {
  skip_if_not_installed("flextable")
  ft <- flextable::flextable
  d <- .t11_ft_df()
  # header labels: & < braces, a backslash, non-ASCII text and NA
  x <- flextable::set_header_labels(ft(d), Param = "P & Q <1>", Mean = "Mean {kg} \\ 50%",
                                    SD = "α Ü", N = NA)
  r <- .t11_render(x)
  expect_identical(unlist(r$pages[[1L]]$col_header),
                   c("P & Q <1>", "Mean {kg} \\ 50%", "α Ü", ""))
  .t11_clean(r$txt)
  for (s in c("P & Q <1>", "Mean \\{kg\\} \\\\ 50%", "\\u945?", "\\u220?", "170.25"))
    expect_true(grepl(s, r$txt, fixed = TRUE), info = s)
  # separate_header(): a spanner over the columns that share a prefix; add_header_lines(): one cell
  sh <- .t11_render(flextable::separate_header(ft(data.frame(a_x = 1, a_y = 2, b_x = 3)), split = "_"))
  hd <- sh$pages[[1L]]$col_header
  expect_identical(vapply(hd[[1L]], function(s) s$label, ""), c("a", "b"))
  expect_identical(unlist(hd[[2L]]), c("x", "y", "x"))
  hl <- .t11_render(flextable::add_header_lines(ft(d), "Line one"))
  expect_length(hl$pages[[1L]]$col_header, 2L)
  expect_true(grepl("Line one", hl$txt, fixed = TRUE))
  # merges: merge_h(), merge_v() and merge_none(), merge_at() in the body, the header, the footer
  h <- .t11_render(flextable::merge_h(ft(data.frame(a = c("x", "y"), b = c("x", "z"), c = c("x", "z")))))
  expect_identical(.t11_cells(h$pages[[1L]]), rbind(c("x", "", ""), c("y", "z", "")))
  expect_identical(.t11_n(h$txt, "x\\cell"), 1L)                  # once for the merged area
  v <- flextable::merge_v(ft(data.frame(a = c("x", "x", "y"), b = c(1, 2, 3))), j = "a")
  rv <- .t11_render(v)
  expect_identical(.t11_cells(rv$pages[[1L]])[, 1L], c("x", "", "y"))
  expect_identical(.t11_n(rv$txt, "x\\cell"), 1L)
  expect_identical(.t11_cells(.t11_render(flextable::merge_none(v))$pages[[1L]])[, 1L], c("x", "x", "y"))
  b <- .t11_render(flextable::merge_at(ft(d), i = 1:2, j = 2:3))
  m <- .t11_cells(b$pages[[1L]])
  expect_identical(m[1:2, 2:3], matrix(c("55.1234", "", "", ""), 2L, 2L))
  expect_identical(m[3L, ], c("Height", "170.2500", "8.00", "29"))
  hh <- .t11_render(flextable::merge_at(ft(d), i = 1, j = 1:2, part = "header"))
  expect_identical(unlist(hh$pages[[1L]]$col_header), c("Param", "", "SD", "N"))
  fm <- .t11_render(flextable::merge_at(flextable::add_footer_lines(ft(d), "Footer text"),
                                        i = 1, j = 1:4, part = "footer"))
  expect_identical(attr(fm$pages[[1L]], "rtf_footnotes"), "Footer text")
  expect_identical(.t11_n(fm$txt, "Footer text"), 1L)
  # colformat_*() and na_str: displayed; a missing value empty unless na_str says otherwise
  dn <- data.frame(n = c(1234.5678, NA, -0.5), i = c(1L, NA, 30L), ch = c("a", NA, "c"),
                   stringsAsFactors = FALSE)
  y <- flextable::colformat_num(ft(dn), j = "n", big.mark = ",", prefix = "$", suffix = " kg", na_str = "--")
  y <- flextable::colformat_int(y, j = "i", na_str = "missing")
  y <- flextable::colformat_char(y, j = "ch", prefix = ">", suffix = "<", na_str = "(none)")
  ry <- .t11_render(y)
  expect_identical(.t11_cells(ry$pages[[1L]]),
                   cbind(c("$1,234.568 kg", "--", "$-0.500 kg"), c("1", "missing", "30"),
                         c(">a<", "(none)", ">c<")))
  expect_identical(.t11_cells(.t11_render(ft(dn))$pages[[1L]])[2L, ], c("", "", ""))
  .t11_clean(ry$txt)
  sf <- .t11_render(flextable::set_formatter(ft(d), Mean = function(x) sprintf("%.1f%%", x)))
  expect_identical(.t11_cells(sf$pages[[1L]])[, 2L], c("55.1%", "70.5%", "170.2%"))
  # footers: lines (an empty one dropped), a footer row and lines together, footnote() marks as text
  f1 <- .t11_render(flextable::add_footer_lines(ft(d), c("N1: abc", "", "N2: a & b <i>")))
  expect_identical(attr(f1$pages[[1L]], "rtf_footnotes"), c("N1: abc", "N2: a & b <i>"))
  f2 <- .t11_render(flextable::add_footer_lines(
    flextable::add_footer_row(ft(d), values = c("L", "M", "R"), colwidths = c(1, 2, 1)), "Last line"))
  expect_identical(attr(f2$pages[[1L]], "rtf_footnotes"), c("L M R", "Last line"))
  fn <- .t11_render(flextable::footnote(ft(d), i = 1, j = 2, ref_symbols = "a",
                                        value = flextable::as_paragraph("Mean of two"), part = "body"))
  expect_identical(.t11_cells(fn$pages[[1L]])[1L, 2L], "55.1234a")     # the mark is text in the cell
  expect_identical(attr(fn$pages[[1L]], "rtf_footnotes"), "aMean of two")
  .t11_clean(fn$txt)
  # a tab and a line break in a cell are not <tab> / <br>
  tb <- .t11_render(flextable::compose(ft(d), i = 1, j = 1,
                                       value = flextable::as_paragraph("a\tb", "\n", "c")))
  expect_identical(tb$pages[[1L]]$data$V1[1L], "a\tb\nc")
  .t11_clean(tb$txt)
  # no rows: the header, the caption and the footer are written
  z <- .t11_render(flextable::add_footer_lines(
    flextable::set_caption(flextable::set_header_labels(ft(d[0, ]), Param = "Parameter"),
                           "Table 0: Empty"), "Note z"))
  expect_identical(nrow(z$pages[[1L]]$data), 0L)
  expect_identical(unlist(z$pages[[1L]]$col_header), c("Parameter", "Mean", "SD", "N"))
  for (s in c("Parameter", "Table 0: Empty", "Note z"))
    expect_true(grepl(s, z$txt, fixed = TRUE), info = s)
  .t11_clean(z$txt)
})

test_that("FT-TB6125 [FR-TB633]: a huxtable with number_format, a caption, merged cells, header_rows() or no rows is read and written without an error: the displayed values, escaped, once per merged area, no markup, no literal NA, an NA or empty caption gives no title", {
  skip_if_not_installed("huxtable")
  hx <- huxtable::as_hux(.t11_ft_df(), add_colnames = TRUE)
  # number_format: a sprintf format, a function, scientific, NA, a number of digits
  h1 <- hx
  huxtable::number_format(h1)[, 2] <- "%.1f%%"
  r1 <- .t11_render(h1)
  expect_identical(.t11_cells(r1$pages[[1L]])[, 2L], c("55.1%", "70.5%", "170.2%"))
  h2 <- hx
  huxtable::number_format(h2)[, 2] <- list(function(v) format(v, big.mark = ",", nsmall = 1))
  expect_identical(.t11_cells(.t11_render(h2)$pages[[1L]])[, 2L], c("55.1234", "70.5", "170.25"))
  hs <- huxtable::huxtable(a = c(1234567.891, -0.00001234, NA, 0), add_colnames = FALSE)
  huxtable::number_format(hs) <- "%.2e"
  rs <- .t11_render(hs)
  expect_identical(.t11_cells(rs$pages[[1L]])[, 1L], c("1.23e+06", "-1.23e-05", "", "0.00e+00"))
  .t11_clean(rs$txt)
  hn <- huxtable::huxtable(a = c(1234567.891, 12.5, NA), add_colnames = FALSE)
  huxtable::number_format(hn) <- NA
  expect_identical(.t11_cells(.t11_render(hn)$pages[[1L]])[, 1L], c("1234567.891", "12.5", ""))
  h3 <- hx
  huxtable::number_format(h3) <- 2
  expect_identical(.t11_cells(.t11_render(h3)$pages[[1L]])[, 4L], c("30.00", "30.00", "29.00"))
  # captions: special characters escaped; two lines; NA and empty give no title; bottom, with a label
  hc <- .t11_render(huxtable::set_caption(hx, "Table 1: A & B <c> {d}"))
  expect_identical(attr(hc$pages[[1L]], "rtf_titles"), "Table 1: A & B <c> {d}")
  expect_true(grepl("Table 1: A & B <c> \\{d\\}", hc$txt, fixed = TRUE))
  expect_identical(attr(.t11_render(huxtable::set_caption(hx, "Line1\nLine2"))$pages[[1L]], "rtf_titles"),
                   "Line1\nLine2")
  for (cp in list(NA_character_, ""))
    expect_null(attr(.t11_render(huxtable::set_caption(hx, cp))$pages[[1L]], "rtf_titles"))
  hb <- huxtable::set_label(huxtable::set_caption_pos(huxtable::set_caption(hx, "Cap"), "bottom"), "tab:x")
  expect_identical(attr(.t11_render(hb)$pages[[1L]], "rtf_titles"), "Cap")
  # merges once: colspan(), rowspan(), merge_cells(), merge_across(), merge_down()
  c1 <- hx
  huxtable::colspan(c1)[2, 1] <- 2
  expect_identical(.t11_cells(.t11_render(c1)$pages[[1L]])[1L, ], c("Age", "", "10.5", "30"))
  c2 <- hx
  huxtable::rowspan(c2)[2, 1] <- 2
  r2 <- .t11_render(c2)
  expect_identical(.t11_cells(r2$pages[[1L]])[, 1L], c("Age", "", "Height"))
  expect_identical(.t11_n(r2$txt, "Age\\cell"), 1L)
  m3 <- .t11_cells(.t11_render(huxtable::merge_cells(hx, 2:3, 1:2))$pages[[1L]])
  expect_identical(m3[1:2, 1:2], matrix(c("Age", "", "", ""), 2L, 2L))
  expect_identical(.t11_cells(.t11_render(huxtable::merge_across(hx, 2, 1:3))$pages[[1L]])[1L, ],
                   c("Age", "", "", "30"))
  expect_identical(.t11_cells(.t11_render(huxtable::merge_down(hx, 2:4, 1))$pages[[1L]])[, 1L],
                   c("Age", "", ""))
  # header_rows(): a body row flagged stays in the body; none flagged: a blank header
  hr <- hx
  huxtable::header_rows(hr)[3] <- TRUE
  rr <- .t11_render(hr)
  expect_identical(unlist(rr$pages[[1L]]$col_header), c("Param", "Mean", "SD", "N"))
  expect_identical(.t11_cells(rr$pages[[1L]])[, 1L], c("Age", "Weight", "Height"))
  h0 <- hx
  huxtable::header_rows(h0) <- FALSE
  r0 <- .t11_render(h0)
  expect_identical(unlist(r0$pages[[1L]]$col_header), rep("", 4L))
  expect_identical(.t11_cells(r0$pages[[1L]])[, 1L], c("Param", "Age", "Weight", "Height"))
  .t11_clean(r0$txt)
  # no rows: the header, the caption and the footnote are written
  z <- huxtable::add_footnote(huxtable::set_caption(huxtable::as_hux(.t11_ft_df()[0, ]), "Table 0: Empty"),
                              "Note z")
  rz <- .t11_render(z)
  expect_identical(dim(rz$pages[[1L]]$data), c(0L, 4L))
  expect_identical(unlist(rz$pages[[1L]]$col_header), c("Param", "Mean", "SD", "N"))
  expect_identical(attr(rz$pages[[1L]], "rtf_footnotes"), "Note z")
  expect_true(grepl("Table 0: Empty", rz$txt, fixed = TRUE))
  .t11_clean(rz$txt)
})

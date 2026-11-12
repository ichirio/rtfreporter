# Tests added to close the gaps found by the Traceability Matrix check (2026-11-12):
# each asserts what its requirement states.

# ---- helpers (prefixed .g4 so that no other test file is touched) ------------

# the non-breaking space the formatters pad with
.g4_nbsp <- "\u00a0"

# a label column without its indent (spaces or non-breaking spaces)
.g4_unindent <- function(x) trimws(gsub("\u00a0", " ", x, fixed = TRUE))

# page bodies of a list of rtftables, without the pagination bookkeeping
.g4_bodies <- function(pages) {
  lapply(pages, function(p) {
    d <- p$data
    attr(d, "rtf_paginate_meta") <- NULL
    d
  })
}

# a small rtables tree: rows F / Mean / M / Mean (the rows under a group are indented)
.g4_tree <- function() {
  skip_if_not_installed("rtables")
  skip_if_not_installed("formatters")
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

# listing data and spec
.g4_adsl <- function() {
  data.frame(
    USUBJID = c("01-701-1015", "01-701-1023", "01-701-1028"),
    AEDECOD = c("HYPERGLYCAEMIA", "UPPER RESPIRATORY TRACT INFECTION", "SKIN ULCER"),
    AELOC   = c("UPPER", NA, "LOWER"),
    ARM     = c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"),
    stringsAsFactors = FALSE
  )
}
.g4_spec <- function() {
  listing_spec(list(
    listing_col("USUBJID", width = 11, label = "Unique\nSubject ID"),
    listing_col(c("AEDECOD", "AELOC"), width = 16, label = "Preferred Term/\nLocation"),
    listing_col("ARM", width = 12, label = "Treatment Arm")
  ))
}

# an SOC / PT hierarchy
.g4_soc <- function() {
  data.frame(soc = c("S1", "S1", "S2"), pt = c("p1", "p2", "p3"),
             n = c("1", "2", "3"), x = c("q", "r", "s"), stringsAsFactors = FALSE)
}

# ---- as_rtftable() -----------------------------------------------------------

test_that("FT-TB3000 [FR-TB102]: as_rtftable() takes a gt_tbl, an rtables tree, a listing_df and a huxtable", {
  skip_if_not_installed("gt")
  skip_if_not_installed("huxtable")
  skip_if_not_installed("rlistings")
  g <- as_rtftable(gt::gt(data.frame(a = c("x", "y"), b = c(1.5, 2.5),
                                     stringsAsFactors = FALSE)))
  expect_s3_class(g, "rtftable")
  expect_identical(g$data$a, c("x", "y"))
  expect_identical(g$data$b, c("1.5", "2.5"))

  tr <- as_rtftable(.g4_tree())
  expect_s3_class(tr, "rtftable")
  expect_identical(unname(unlist(tr$data[1:2, 1])), c("F", "  Mean"))
  expect_identical(tr$data[[2]], c("", "40.0", "", "50.0"))
  expect_identical(tr$data[[3]], c("", "30.0", "", "36.0"))

  lst <- rlistings::as_listing(data.frame(a = c("x", "y"), b = c("p", "q"),
                                          stringsAsFactors = FALSE), key_cols = "a")
  expect_true(inherits(lst, "listing_df"))
  l <- as_rtftable(lst)
  expect_s3_class(l, "rtftable")
  expect_identical(unname(unlist(l$data)), c("x", "y", "p", "q"))

  h <- as_rtftable(huxtable::hux(data.frame(a = c("x", "y"), b = c(1, 2),
                                            stringsAsFactors = FALSE)))
  expect_s3_class(h, "rtftable")
  expect_identical(unname(unlist(h$data)), c("x", "y", "1", "2"))
})

test_that("FT-TB3001 [FR-TB103]: as_rtftable() converts the one table of a gt_group and of a tbl_split", {
  skip_if_not_installed("gt")
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("tfrmt")
  d2 <- data.frame(a = c("x", "y"), b = c(1.5, 2.5), stringsAsFactors = FALSE)
  sp <- gt::gt_split(gt::gt(d2), row_every_n = 2)
  expect_s3_class(sp, "gt_group")
  expect_identical(as_rtftable(sp)$data, as_rtftable(gt::gt(d2))$data)

  # a tfrmt page_plan render with one page
  dat <- expand.grid(group = "g1", label = paste0("lbl", 1:2), column = c("A", "B"),
                     param = "n", stringsAsFactors = FALSE)
  dat$value <- seq_len(nrow(dat))
  spec <- tfrmt::tfrmt(
    group = group, label = label, column = column, param = param, value = value,
    body_plan = tfrmt::body_plan(
      tfrmt::frmt_structure(group_val = ".default", label_val = ".default",
                            tfrmt::frmt("x"))),
    page_plan = tfrmt::page_plan(tfrmt::page_structure(group_val = ".default")))
  pg <- tfrmt::print_to_gt(spec, dat)
  expect_s3_class(pg, "gt_group")
  member <- gt::grp_pull(pg, 1)
  expect_identical(as_rtftable(pg)$data, as_rtftable(member)$data)
  expect_gt(nrow(as_rtftable(pg)$data), 0L)

  skip_if_not("tbl_split_by_rows" %in% getNamespaceExports("gtsummary"))
  ts <- gtsummary::tbl_split_by_rows(
    gtsummary::tbl_summary(gtsummary::trial, by = trt, include = c(age, grade)),
    variables = age)
  one <- structure(list(ts[[1]]), class = class(ts))
  expect_identical(as_rtftable(one)$data, as_rtftable(ts[[1]])$data)
})

test_that("FT-TB3002 [FR-TB107]: as_rtftable(read_meta) reads all by default and only the tokens given", {
  skip_if_not_installed("gt")
  g <- gt::gt(data.frame(a = c("x", "y"), b = c(1.5, 2.5), stringsAsFactors = FALSE))
  g <- gt::tab_header(gt::tab_spanner(g, "S", columns = c(a, b)), title = "TT")
  all <- as_rtftable(g)
  expect_length(all$col_header, 2L)            # the spanner row and the label row
  expect_identical(all$col_header[[1L]][[1L]]$label, "S")
  expect_identical(attr(all, "rtf_titles"), "TT")

  hdr <- as_rtftable(g, read_meta = "col_header")
  expect_identical(hdr$col_header, list(c("a", "b")))
  expect_null(attr(hdr, "rtf_titles"))

  none <- as_rtftable(g, read_meta = FALSE)
  expect_null(none$col_header)
  expect_null(attr(none, "rtf_titles"))
})

test_that("FT-TB3003 [FR-TB108]: as_rtftable(...) forwards to rtftable(); an explicit value wins", {
  skip_if_not_installed("gt")
  g <- gt::gt(data.frame(a = c("x", "y"), b = c(1.5, 2.5), stringsAsFactors = FALSE))
  g <- gt::cols_label(g, a = "GA", b = "GB")
  g <- gt::cols_width(g, a ~ gt::px(100), b ~ gt::px(300))
  own <- as_rtftable(g)
  expect_identical(own$column_widths_twips, c(1500L, 4500L))   # the gt's own widths
  expect_identical(own$col_header, list(c("GA", "GB")))

  tb <- as_rtftable(g, col_rel_width = c(1, 3), col_header = c("P", "Q"))
  expect_identical(tb$col_rel_width, c(1, 3))
  expect_identical(tb$col_header, list(c("P", "Q")))
})

test_that("FT-TB3004 [FR-TB109]: as_rtftable() acts as as_rtftables()[[1]] for the other settings", {
  s <- .g4_soc()
  stub <- stub_spec(c("soc", "pt"))
  expect_identical(as_rtftable(s, stub = stub), as_rtftables(s, stub = stub)[[1L]])
  expect_identical(as_rtftable(s, drop_cols = "x"), as_rtftables(s, drop_cols = "x")[[1L]])
  expect_identical(as_rtftable(s, sort_by = "n", sort_desc = TRUE),
                   as_rtftables(s, sort_by = "n", sort_desc = TRUE)[[1L]])
  expect_identical(as_rtftable(s, collapse_repeats = "soc"),
                   as_rtftables(s, collapse_repeats = "soc")[[1L]])
  h <- data.frame(lab = c("a", "b"), `T|n` = c("1", "2"), `T|pct` = c("3", "4"),
                  check.names = FALSE, stringsAsFactors = FALSE)
  expect_identical(as_rtftable(h, header_sep = "|"), as_rtftables(h, header_sep = "|")[[1L]])
  expect_length(as_rtftable(h, header_sep = "|")$col_header, 2L)
  expect_identical(as_rtftable(.g4_adsl(), listing = .g4_spec()),
                   as_rtftables(.g4_adsl(), listing = .g4_spec())[[1L]])
})

# ---- as_rtftables(): result, sources, read_meta ------------------------------

test_that("FT-TB3005 [FR-TB111]: the list is named for a value-based split and a named list", {
  d <- data.frame(grp = c("A", "A", "B"), v = c("1", "2", "3"), stringsAsFactors = FALSE)
  res <- as_rtftables(d, split = "by_value", group_col = "grp")
  expect_identical(names(res), c("A", "B"))
  expect_identical(res$A$data$v, c("1", "2"))
  expect_identical(res$B$data$v, "3")

  lst <- as_rtftables(list(a = data.frame(x = "1"), b = data.frame(x = "2")))
  expect_identical(names(lst), c("a", "b"))
  expect_null(names(as_rtftables(d)))
})

test_that("FT-TB3006 [FR-TB114]: as_rtftables() gives the rendered body of each source", {
  skip_if_not_installed("gt")
  skip_if_not_installed("huxtable")
  skip_if_not_installed("flextable")
  skip_if_not_installed("rlistings")
  d <- data.frame(g = c("A", "A", "B", "B"), v = c("a", "b", "c", "d"),
                  n = 1:4, h = 5:8, stringsAsFactors = FALSE)
  gt_body <- as_rtftables(gt::cols_hide(gt::gt(d, groupname_col = "g"), h))[[1L]]$data
  expect_identical(names(gt_body), c("v", "n"))                 # the hidden column is gone
  expect_identical(nrow(gt_body), 6L)                           # 4 rows + 2 group rows
  expect_identical(gsub(.g4_nbsp, " ", gt_body$v, fixed = TRUE),
                   c("A", "  a", "  b", "B", "  c", "  d"))
  expect_identical(gt_body$n, c("", "1", "2", "", "3", "4"))

  tr <- as_rtftables(.g4_tree())[[1L]]$data
  expect_identical(tr[[1L]], c("F", "  Mean", "M", "  Mean"))   # the indent is in the label

  lst <- rlistings::as_listing(data.frame(a = c("x", "y"), b = c("p", "q"),
                                          stringsAsFactors = FALSE), key_cols = "a")
  expect_identical(unname(unlist(as_rtftables(lst)[[1L]]$data)), c("x", "y", "p", "q"))

  hx <- huxtable::hux(data.frame(a = c("x", "y"), b = c(1, 2), stringsAsFactors = FALSE))
  expect_identical(unname(unlist(as_rtftables(hx)[[1L]]$data)), c("x", "y", "1", "2"))

  ft <- flextable::flextable(data.frame(a = c("x", "y"), b = c(1, 2), h = c(9, 9),
                                        stringsAsFactors = FALSE),
                             col_keys = c("a", "b"))
  fb <- as_rtftables(ft)[[1L]]$data
  expect_identical(names(fb), c("V1", "V2"))                    # the helper column is gone
  expect_identical(unname(unlist(fb)), c("x", "y", "1", "2"))
})

test_that("FT-TB3007 [FR-TB115]: a gt_group gives one page set per member table", {
  skip_if_not_installed("gt")
  skip_if_not_installed("tfrmt")
  d4 <- data.frame(a = as.character(1:4), b = letters[1:4], stringsAsFactors = FALSE)
  sp <- gt::gt_split(gt::gt(d4), col_slice_at = "a")
  pages <- as_rtftables(sp)
  expect_length(pages, 2L)
  expect_identical(names(pages[[1L]]$data), "a")
  expect_identical(pages[[1L]]$data$a, as.character(1:4))
  expect_identical(names(pages[[2L]]$data), "b")
  expect_identical(pages[[2L]]$data$b, letters[1:4])

  dat <- expand.grid(group = c("g1", "g2"), label = paste0("lbl", 1:3),
                     column = c("A", "B"), param = "n", stringsAsFactors = FALSE)
  dat$value <- seq_len(nrow(dat))
  spec <- tfrmt::tfrmt(
    group = group, label = label, column = column, param = param, value = value,
    body_plan = tfrmt::body_plan(
      tfrmt::frmt_structure(group_val = ".default", label_val = ".default",
                            tfrmt::frmt("x"))),
    page_plan = tfrmt::page_plan(tfrmt::page_structure(group_val = ".default")))
  pg <- tfrmt::print_to_gt(spec, dat)
  tp <- as_rtftables(pg)
  expect_length(tp, 2L)
  expect_identical(.g4_unindent(tp[[1L]]$data$rowname), c("g1", "lbl1", "lbl2", "lbl3"))
  expect_identical(tp[[1L]]$data$A, c("", "1", "3", "5"))
  expect_identical(tp[[1L]]$data$B, c("", "7", "9", "11"))
  expect_identical(.g4_unindent(tp[[2L]]$data$rowname), c("g2", "lbl1", "lbl2", "lbl3"))
  expect_identical(tp[[2L]]$data$A, c("", "2", "4", "6"))
  expect_identical(tp[[2L]]$data$B, c("", "8", "10", "12"))
})

test_that("FT-TB3008 [FR-TB116]: a list input names pages '<list name>.<page name>'; an empty list gives list()", {
  d <- data.frame(grp = c("A", "A", "B"), v = c("1", "2", "3"), stringsAsFactors = FALSE)
  res <- as_rtftables(list(X = d), split = "by_value", group_col = "grp")
  expect_identical(names(res), c("X.A", "X.B"))
  expect_identical(res$X.A$data$v, c("1", "2"))
  expect_identical(as_rtftables(list()), list())
})

test_that("FT-TB3009 [FR-TB118]: as_rtftables(read_meta) reads all by default and only the token given", {
  skip_if_not_installed("gt")
  g <- gt::gt(data.frame(a = c("x", "y"), b = c(1.5, 2.5), stringsAsFactors = FALSE))
  g <- gt::tab_spanner(g, "S", columns = c(a, b))
  g <- gt::tab_header(g, title = "TT")
  g <- gt::tab_source_note(g, "SN")
  g <- gt::cols_width(g, a ~ gt::px(100), b ~ gt::px(300))
  g <- gt::cols_align(g, "right", columns = b)
  g <- gt::tab_style(g, gt::cell_text(weight = "bold"),
                     gt::cells_body(columns = b, rows = 1))
  all <- as_rtftables(g)[[1L]]
  expect_identical(attr(all, "rtf_titles"), "TT")
  expect_identical(attr(all, "rtf_footnotes"), "SN")
  expect_length(all$col_header, 2L)                          # spanning
  expect_identical(all$column_widths_twips, c(1500L, 4500L))  # widths
  expect_false(is.null(all$cell_styles))                      # styles

  only <- function(tok) as_rtftables(g, read_meta = tok)[[1L]]
  t <- only("titles")
  expect_identical(attr(t, "rtf_titles"), "TT")
  expect_null(attr(t, "rtf_footnotes"))
  expect_null(t$col_header)
  expect_null(t$column_widths_twips)
  f <- only("footnotes")
  expect_identical(attr(f, "rtf_footnotes"), "SN")
  expect_null(attr(f, "rtf_titles"))
  w <- only("widths")
  expect_identical(w$column_widths_twips, c(1500L, 4500L))
  expect_null(attr(w, "rtf_titles"))
  expect_null(w$col_header)
  a <- only("alignment")
  expect_identical(a$col_spec[[2L]]$align, "right")
  expect_null(a$column_widths_twips)
  expect_null(attr(a, "rtf_titles"))
  s <- only("styles")
  expect_false(is.null(s$cell_styles))
  expect_null(s$column_widths_twips)
  expect_null(attr(s, "rtf_titles"))
  expect_null(only("titles")$cell_styles)

  # a data.frame: the "labels" token reads the column label attributes
  df <- data.frame(a = 1:2, b = 3:4)
  attr(df$a, "label") <- "Label A"
  expect_identical(as_rtftables(df, read_meta = "labels")[[1L]]$col_header,
                   list(c("Label A", "b")))
  expect_null(as_rtftables(df, read_meta = FALSE)[[1L]]$col_header)
})

test_that("FT-TB3010 [FR-TB118]: the rtables 'footnote_marks' token alone converts the marks, and only that", {
  skip_if_not_installed("rtables")
  skip_if_not_installed("formatters")
  lyt <- rtables::basic_table(title = "Table 1", main_footer = "Main footer")
  lyt <- rtables::split_cols_by(lyt, "ARM")
  lyt <- rtables::analyze(lyt, "AGE", afun = function(x) {
    rtables::in_rows("Mean" = rtables::rcell(mean(x), format = "xx.x",
                                             footnotes = list("a note")))
  })
  d <- data.frame(ARM = c("A", "A"), AGE = c(40, 42), stringsAsFactors = FALSE)
  tr <- rtables::build_table(lyt, d)
  fm <- as_rtftables(tr, read_meta = "footnote_marks")[[1L]]
  expect_identical(fm$data[[2L]], "41.0 ^{1}")
  expect_null(attr(fm, "rtf_titles"))
  expect_null(attr(fm, "rtf_footnotes"))
  expect_null(fm$col_header)
  expect_identical(as_rtftables(tr, read_meta = "titles")[[1L]]$data[[2L]], "41.0 {1}")
})

test_that("FT-TB3011 [FR-TB120]: an unknown read_meta token and a wrong type stop with the given message", {
  skip_if_not_installed("gt")
  g <- gt::gt(data.frame(a = "x", stringsAsFactors = FALSE))
  expect_error(
    as_rtftables(g, read_meta = "bogus"),
    "Unknown gt `read_meta` token(s): 'bogus'.  Allowed: 'col_header', 'alignment', 'spanning', 'widths', 'titles', 'footnotes', 'styles'",
    fixed = TRUE)
  msg <- "`read_meta` must be FALSE/TRUE or a character vector of tokens."
  df <- data.frame(a = "x", stringsAsFactors = FALSE)
  expect_error(as_rtftables(df, read_meta = NA), msg, fixed = TRUE)
  expect_error(as_rtftables(df, read_meta = 1), msg, fixed = TRUE)
  expect_error(as_rtftables(df, read_meta = c(TRUE, FALSE)), msg, fixed = TRUE)
  expect_error(as_rtftables(df, read_meta = NA_character_), msg, fixed = TRUE)
  expect_error(as_rtftables(g, read_meta = 1), msg, fixed = TRUE)
})

# ---- as_rtftables(): split, max_rows, split_rows, group_col, group_by --------

test_that("FT-TB3012 [FR-TB121]: group_safe writes the continuation label for a group over max_rows; none is the default", {
  d <- data.frame(grp = c(rep("A", 6), "B", "B"), v = as.character(1:8),
                  stringsAsFactors = FALSE)
  p <- as_rtftables(d, split = "group_safe", max_rows = 4, group_col = "grp",
                    group_by = "value")
  expect_length(p, 3L)
  expect_identical(p[[1L]]$data$v, as.character(1:4))
  expect_identical(p[[2L]]$data$grp, c("A (Cont.)", "A", "A"))
  expect_identical(p[[2L]]$data$v, c("", "5", "6"))
  expect_identical(p[[3L]]$data$grp, c("B", "B"))
  p2 <- as_rtftables(d, split = "group_safe", max_rows = 4, group_col = "grp",
                     group_by = "value", cont_label = " (cont.)")
  expect_identical(p2[[2L]]$data$grp[1L], "A (cont.)")

  # "none" is the default: one page, whatever max_rows says
  expect_length(as_rtftables(d, max_rows = 2), 1L)
  expect_identical(as_rtftables(d, max_rows = 2)[[1L]]$data$v, as.character(1:8))
  expect_identical(.g4_bodies(as_rtftables(d)), .g4_bodies(as_rtftables(d, split = "none")))
})

test_that("FT-TB3013 [FR-TB122]: a custom split is called with the documented arguments on the cell-formatted body", {
  d <- data.frame(grp = c(rep("A", 6), "B", "B"), v = as.character(1:8),
                  stringsAsFactors = FALSE)
  seen <- NULL
  rule <- function(df, max_rows, group_col, group_by, cont_label, min_group_rows) {
    seen <<- list(df = df, max_rows = max_rows, group_col = group_col,
                  group_by = group_by, cont_label = cont_label,
                  min_group_rows = min_group_rows)
    list(first = df[1:3, ], second = df[4:8, ])
  }
  res <- as_rtftables(d, split = rule, max_rows = 3, group_col = "grp",
                      group_by = "value", cont_label = " (more)", min_group_rows = 4L,
                      cell_format = function(x) paste0("<", x, ">"))
  expect_identical(names(seen), c("df", "max_rows", "group_col", "group_by",
                                  "cont_label", "min_group_rows"))
  expect_equal(seen$max_rows, 3)
  expect_identical(seen$group_col, "grp")
  expect_identical(seen$group_by, "value")
  expect_identical(seen$cont_label, " (more)")
  expect_identical(seen$min_group_rows, 4L)
  expect_identical(seen$df$v, paste0("<", 1:8, ">"))          # the cell-formatted body
  expect_identical(seen$df$grp, d$grp)                        # column 1 is left alone
  expect_identical(names(res), c("first", "second"))          # list names become page names
  expect_identical(res$second$data$v, paste0("<", 4:8, ">"))

  msg <- "A custom `split` function must return a non-empty list of data.frames."
  expect_error(as_rtftables(d, split = function(df, ...) list()), msg, fixed = TRUE)
  expect_error(as_rtftables(d, split = function(df, ...) 1), msg, fixed = TRUE)
  expect_error(as_rtftables(d, split = function(df, ...) list(1)), msg, fixed = TRUE)
})

test_that("FT-TB3014 [FR-TB124]: max_rows is ignored by none and rows and is required by group_safe", {
  d <- data.frame(grp = rep(c("A", "B", "C"), each = 2), v = as.character(1:6),
                  stringsAsFactors = FALSE)
  expect_length(as_rtftables(d, max_rows = 3), 1L)                  # split = "none"
  r <- as_rtftables(d, split = "rows", split_rows = 4, max_rows = 1)
  expect_equal(vapply(r, function(p) nrow(p$data), integer(1)), c(3L, 3L))
  gs <- as_rtftables(d, split = "group_safe", max_rows = 2)
  expect_equal(vapply(gs, function(p) nrow(p$data), integer(1)), c(2L, 2L, 2L))
  expect_identical(gs[[2L]]$data$grp, c("B", "B"))
  expect_error(as_rtftables(d, split = "group_safe"),
               "`max_rows` is required when split = \"group_safe\".", fixed = TRUE)
  expect_error(as_rtftables(d, split = "group_force"),
               "`max_rows` is required when split = \"group_force\".", fixed = TRUE)
})

test_that("FT-TB3015 [FR-TB126]: split_rows = c(20, 40) cuts 1:19, 20:39, 40:nrow and is ignored by the other splits", {
  d <- data.frame(a = as.character(1:60), b = 1:60, stringsAsFactors = FALSE)
  p <- as_rtftables(d, split = "rows", split_rows = c(20, 40))
  expect_equal(vapply(p, function(x) nrow(x$data), integer(1)), c(19L, 20L, 21L))
  expect_identical(p[[1L]]$data$a, as.character(1:19))
  expect_identical(p[[2L]]$data$a, as.character(20:39))
  expect_identical(p[[3L]]$data$a, as.character(40:60))
  for (s in c("none", "group_safe", "group_force", "by_value")) {
    with_sr <- as_rtftables(d, split = s, max_rows = 25, split_rows = c(20, 40))
    without <- as_rtftables(d, split = s, max_rows = 25)
    expect_identical(.g4_bodies(with_sr), .g4_bodies(without), info = s)
  }
  expect_length(as_rtftables(d, split = "none", split_rows = c(20, 40)), 1L)
  expect_length(as_rtftables(d, split = "group_force", max_rows = 25,
                             split_rows = c(20, 40)), 3L)
})

test_that("FT-TB3016 [FR-TB127]: group_col by name or position, NULL = column 1, a factor is coerced for the continuation label", {
  f <- data.frame(x = c("a", "b", "c", "d"), grp = c("A", "A", "B", "B"),
                  v = 1:4, stringsAsFactors = FALSE)
  by_name <- as_rtftables(f, split = "group_force", max_rows = 3, group_col = "grp")
  by_pos  <- as_rtftables(f, split = "group_force", max_rows = 3, group_col = 2)
  expect_identical(.g4_bodies(by_name), .g4_bodies(by_pos))
  expect_equal(vapply(by_name, function(p) nrow(p$data), integer(1)), c(2L, 2L))

  g <- data.frame(grp = c("A", "A", "A", "B"), v = 1:4, stringsAsFactors = FALSE)
  for (s in c("group_safe", "group_force", "by_value")) {
    dflt <- as_rtftables(g, split = s, max_rows = 2)
    expect_identical(.g4_bodies(dflt),
                     .g4_bodies(as_rtftables(g, split = s, max_rows = 2, group_col = 1)),
                     info = s)
    expect_identical(.g4_bodies(dflt),
                     .g4_bodies(as_rtftables(g, split = s, max_rows = 2, group_col = "grp")),
                     info = s)
  }
  expect_identical(names(as_rtftables(g, split = "by_value", max_rows = 2)),
                   c("A...1", "A...2", "B"))

  gf <- data.frame(grp = factor(c("A", "A", "A", "B")), v = 1:4)
  for (s in c("group_safe", "by_value")) {
    pg <- as_rtftables(gf, split = s, max_rows = 2, group_col = "grp")
    expect_length(pg, 3L)
    expect_true(all(vapply(pg, function(p) is.character(p$data$grp), logical(1))), info = s)
    expect_identical(pg[[2L]]$data$grp, c("A (Cont.)", "A"), info = s)
  }
})

test_that("FT-TB3017 [FR-TB130]: a split or group_by that is not listed is refused by match.arg()", {
  d <- data.frame(grp = c("A", "B"), v = 1:2, stringsAsFactors = FALSE)
  expect_error(as_rtftables(d, split = "nonsense"),
               "should be one of .none., .rows., .group_safe., .group_force., .by_value.")
  expect_error(as_rtftables(d, split = "group_safe", max_rows = 2, group_by = "nonsense"),
               "should be one of .auto., .indent., .value., .filled.")
})

# ---- as_rtftables(): page_by, sort ------------------------------------------

test_that("FT-TB3018 [FR-TB131]: page_by gathers a scattered value in the body's order; NULL is one page", {
  s <- data.frame(pg = c("P1", "P2", "P1"), v = c("a", "b", "c"), stringsAsFactors = FALSE)
  p <- as_rtftables(s, page_by = "pg")
  expect_identical(names(p), c("P1", "P2"))
  expect_identical(p$P1$data$v, c("a", "c"))
  expect_identical(p$P2$data$v, "b")
  one <- as_rtftables(s)
  expect_length(one, 1L)
  expect_null(names(one))
  expect_identical(one[[1L]]$data$v, c("a", "b", "c"))
})

test_that("FT-TB3019 [FR-TB133]: page_by on a column the body lacks stops with the message", {
  s <- data.frame(pg = c("P1", "P2"), v = c("a", "b"), stringsAsFactors = FALSE)
  expect_error(as_rtftables(s, page_by = "nope"),
               "`page_by` column 'nope' not found in the table.", fixed = TRUE)
})

test_that("FT-TB3020 [FR-TB134]: sort_by orders the rows before the pages are cut", {
  d <- data.frame(g = c("x", "x", "y", "y", "z"), k = c(5, 3, 4, 1, 2),
                  stringsAsFactors = FALSE)
  p <- as_rtftables(d, sort_by = "k", split = "group_force", max_rows = 2, group_col = "g")
  expect_length(p, 3L)
  expect_equal(p[[1L]]$data$k, c(1, 2))
  expect_equal(p[[2L]]$data$k, c(3, 4))
  expect_equal(p[[3L]]$data$k, 5)
  expect_identical(p[[1L]]$data$g, c("y", "z"))
})

test_that("FT-TB3021 [FR-TB135]: sort_desc is ignored without sort_by; FALSE and NULL are ascending", {
  d <- data.frame(g = c("x", "x", "y", "y", "z"), k = c(5, 3, 4, 1, 2),
                  stringsAsFactors = FALSE)
  expect_equal(as_rtftables(d, sort_desc = TRUE)[[1L]]$data$k, d$k)
  expect_equal(as_rtftables(d, sort_by = "k", sort_desc = FALSE)[[1L]]$data$k, 1:5)
  expect_equal(as_rtftables(d, sort_by = "k", sort_desc = NULL)[[1L]]$data$k, 1:5)
  expect_equal(as_rtftables(d, sort_by = "k")[[1L]]$data$k, 1:5)
  expect_equal(as_rtftables(d, sort_by = "k", sort_desc = TRUE)[[1L]]$data$k, 5:1)
})

# ---- as_rtftables(): blank rows ---------------------------------------------

test_that("FT-TB3022 [FR-TB139]: blank_rows NULL gives none; integer positions and a list of forms are taken", {
  df <- data.frame(a = c("p", "q", "r", "s", "t"), stringsAsFactors = FALSE)
  expect_null(as_rtftables(df)[[1L]]$blank_rows)
  expect_null(as_rtftables(df, blank_rows = NULL)[[1L]]$blank_rows)
  expect_identical(as_rtftables(df, blank_rows = c(2, 4))[[1L]]$blank_rows, c(2L, 4L))
  mixed <- as_rtftables(df, blank_rows = list(2, blank_rows_by_rule("a", "^s")))
  expect_identical(mixed[[1L]]$blank_rows, c(2L, 3L))   # "^s" is row 4: a blank before it
  zero <- as_rtftables(df, blank_rows = c(0, -1))[[1L]]$blank_rows
  expect_true(0L %in% zero)
  # NULL: an rtf_blank_rows attribute already on the input is honoured
  expect_identical(as_rtftables(set_blank_rows(df, c(1, 3)))[[1L]]$blank_rows, c(1L, 3L))
})

test_that("FT-TB3023 [FR-TB140]: a blank_rows entry of no accepted form stops", {
  df <- data.frame(a = c("p", "q"), stringsAsFactors = FALSE)
  expect_error(as_rtftables(df, blank_rows = list(TRUE)),
               "Unrecognised `blank_rows` entry: TRUE", fixed = TRUE)
  expect_error(as_rtftables(df, blank_rows = "nope"),
               "Unrecognised `blank_rows` entry: nope", fixed = TRUE)
})

# ---- as_rtftables(): cell formats, na ---------------------------------------

test_that("FT-TB3024 [FR-TB143]: align_count_pct is ignored when cell_format is given", {
  cn <- data.frame(l = c("a", "b"), v = c("5 (10.0)", "10 (5.0)"), stringsAsFactors = FALSE)
  fmt <- function(x) paste0("[", x, "]")
  both <- as_rtftables(cn, align_count_pct = TRUE, cell_format = fmt)[[1L]]$data
  expect_identical(both$v, c("[5 (10.0)]", "[10 (5.0)]"))
  expect_identical(both, as_rtftables(cn, cell_format = fmt)[[1L]]$data)
  # the built-in name behaves the same
  right <- as_rtftables(cn, cell_format = "right_align")[[1L]]$data
  expect_identical(as_rtftables(cn, align_count_pct = TRUE,
                                cell_format = "right_align")[[1L]]$data, right)
  # off: no alignment
  expect_identical(as_rtftables(cn)[[1L]]$data$v, c("5 (10.0)", "10 (5.0)"))
})

test_that("FT-TB3025 [FR-TB144]: align_count_pct of no accepted kind stops with the message", {
  cn <- data.frame(l = "a", v = "5 (10.0)", stringsAsFactors = FALSE)
  expect_error(
    as_rtftables(cn, align_count_pct = 3),
    "`align_count_pct` must be TRUE / FALSE, the name of a built-in cell format, or a cell-format function.",
    fixed = TRUE)
})

test_that("FT-TB3026 [FR-TB145]: one formatter hits every data column but column 1; NULL leaves the body", {
  c2 <- data.frame(l = c("a", "bb"), x = c("5", "10"), y = c("1 (2)", "33 (4)"),
                   stringsAsFactors = FALSE)
  nb <- .g4_nbsp
  bare <- as_rtftables(c2, cell_format = "count_paren_bare")[[1L]]$data
  expect_identical(bare$l, c("a", "bb"))                      # the label column is left alone
  expect_identical(bare$x, c(paste0(nb, "5"), "10"))          # a bare count is padded
  expect_identical(bare$y, c(paste0(nb, "1", nb, "(2)"), paste0("33", nb, "(4)")))
  plain <- as_rtftables(c2, cell_format = "count_paren")[[1L]]$data
  expect_identical(plain$x, c("5", "10"))                     # not padded without bare
  expect_identical(plain$y, bare$y)
  expect_identical(as_rtftables(c2, cell_format = "fmt_count_paren_bare")[[1L]]$data, bare)
  expect_identical(as_rtftables(c2)[[1L]]$data$x, c("5", "10"))
  expect_identical(as_rtftables(c2)[[1L]]$data$y, c("1 (2)", "33 (4)"))
  expect_identical(as_rtftables(c2, cell_format = NULL)[[1L]]$data$y, c("1 (2)", "33 (4)"))
})

test_that("FT-TB3027 [FR-TB147]: na prints in every column, with and without a formatter; '' leaves the cell empty", {
  n <- data.frame(l = c("a", NA), v = c("1 (2.0)", NA), w = c(NA, "22 (3.0)"),
                  stringsAsFactors = FALSE)
  plain <- as_rtftables(n, na = "n/a")[[1L]]$data
  expect_identical(plain$l, c("a", "n/a"))
  expect_identical(plain$v, c("1 (2.0)", "n/a"))
  expect_identical(plain$w, c("n/a", "22 (3.0)"))
  aligned <- as_rtftables(n, na = "n/a", align_count_pct = TRUE)[[1L]]$data
  expect_identical(aligned$l, c("a", "n/a"))
  expect_true(grepl("n/a", aligned$v[2L], fixed = TRUE))
  expect_true(grepl("n/a", aligned$w[1L], fixed = TRUE))
  fmt <- as_rtftables(n, na = "n/a", cell_format = "right_align")[[1L]]$data
  expect_identical(fmt$l, c("a", "n/a"))
  expect_true(grepl("n/a", fmt$v[2L], fixed = TRUE))
  expect_true(grepl("n/a", fmt$w[1L], fixed = TRUE))
  empty <- as_rtftables(n, na = "")[[1L]]$data
  expect_true(is.na(empty$l[2L]) || identical(empty$l[2L], ""))
  expect_true(is.na(empty$v[2L]) || identical(empty$v[2L], ""))
  expect_true(is.na(empty$w[1L]) || identical(empty$w[1L], ""))
  empty_fmt <- as_rtftables(n, na = "", cell_format = "right_align")[[1L]]$data
  expect_identical(empty_fmt$v[2L], "")
  expect_identical(empty_fmt$w[1L], "")
})

test_that("FT-TB3028 [FR-TB148]: na treats NaN as missing, prints Inf and -Inf, and leaves the strings 'NA' and 'NaN'", {
  m <- data.frame(l = letters[1:5], v = c(NaN, -Inf, Inf, NA, 2),
                  s = c("NA", "NaN", "NA", "NaN", "x"), stringsAsFactors = FALSE)
  b <- as_rtftables(m, na = "-")[[1L]]$data
  expect_identical(b$v, c("-", "-Inf", "Inf", "-", "2"))
  expect_identical(b$s, c("NA", "NaN", "NA", "NaN", "x"))
})

test_that("FT-TB3029 [FR-TB151]: first_rows with a custom split stops with the message", {
  d <- data.frame(g = c("x", "x", "y", "y", "z"), k = c(5, 3, 4, 1, 2),
                  stringsAsFactors = FALSE)
  expect_error(
    as_rtftables(d, split = function(df, ...) list(df), first_rows = 2),
    "`first_rows` paginates one table by rows or groups: not with `page_by` or a custom `split`.",
    fixed = TRUE)
})

# ---- as_rtftables(): listing, stub, widths, forwarded settings -----------------

test_that("FT-TB3030 [FR-TB155]: a listing spec joins, wraps and gutters the columns and gives the header", {
  tb <- as_rtftables(.g4_adsl(), listing = .g4_spec())[[1L]]
  b <- tb$data
  expect_identical(names(b), c("USUBJID", ".sp1", "AEDECOD", ".sp2", "ARM"))
  # a blank row after each record (rows 3, 7 and 10); the cells wrapped to their widths
  expect_identical(b$USUBJID, c("01-701-1015", "", "", "01-701-1023", "", "", "",
                                "01-701-1028", "", ""))
  expect_identical(b$AEDECOD, c("HYPERGLYCAEMIA/", "UPPER", "", "UPPER", "RESPIRATORY",
                                "TRACT INFECTION", "", "SKIN ULCER/", "LOWER", ""))
  expect_identical(b$ARM, c("Placebo", "", "", "Xanomeline", "High Dose", "", "",
                            "Xanomeline", "Low Dose", ""))
  expect_identical(b$.sp1, rep("", 10L))
  expect_identical(b$.sp2, rep("", 10L))
  expect_identical(tb$col_header,
                   list(c("Unique\nSubject ID", "", "Preferred Term/\nLocation", "",
                          "Treatment
Arm")))   # wrapped to the width of 12
  expect_identical(tb$col_rel_width, c(11, 1, 16, 1, 12))
  expect_identical(tb$col_spec[[1L]]$align, "left")
})

test_that("FT-TB3031 [FR-TB157]: listing on a source it cannot apply to stops with each message", {
  skip_if_not_installed("rlistings")
  skip_if_not_installed("gt")
  lst <- rlistings::as_listing(data.frame(a = c("x", "y"), b = c("p", "q"),
                                          stringsAsFactors = FALSE), key_cols = "a")
  expect_error(
    as_rtftables(lst, listing = .g4_spec()),
    "`listing` does not apply to an rlistings listing (`listing_df`): rlistings has already laid it out.  Drop `listing` and pass it to `as_rtftables()` as it is.",
    fixed = TRUE)
  expect_error(as_rtftables(42, listing = .g4_spec()),
               "`listing` applies to a data.frame or tibble source; got 'numeric'.",
               fixed = TRUE)
  expect_error(as_rtftables(list(1), listing = .g4_spec()),
               "`listing` applies to a data.frame or tibble source; got 'numeric'.",
               fixed = TRUE)
  expect_error(as_rtftables(gt::gt(data.frame(a = 1)), listing = .g4_spec()),
               "`listing` applies to a data.frame or tibble source; got 'gt_tbl/list'.",
               fixed = TRUE)
})

test_that("FT-TB3032 [FR-TB161]: drop_cols, sort_by and group_by refer to the reshaped body; a stubbed source gets no second stub", {
  st <- .g4_soc()
  stub <- stub_spec(c("soc", "pt"))
  r <- as_rtftables(st, stub = stub, drop_cols = "x", sort_by = "n", sort_desc = TRUE)[[1L]]
  expect_identical(names(r$data), c("soc / pt", "n"))
  expect_identical(.g4_unindent(r$data[[1L]]), c("p3", "p2", "p1", "S1", "S2"))
  expect_identical(r$data$n, c("3", "2", "1", NA, NA))
  # by position: 3 is "x" and 2 is "n" in the reshaped body (not in the input)
  r2 <- as_rtftables(st, stub = stub, drop_cols = 3, sort_by = 2, sort_desc = TRUE)[[1L]]
  expect_identical(r2$data, r$data)

  p <- as_rtftables(st, stub = stub, split = "group_safe", max_rows = 3, group_by = "indent")
  expect_length(p, 2L)
  expect_identical(.g4_unindent(p[[1L]]$data[[1L]]), c("S1", "p1", "p2"))
  expect_identical(.g4_unindent(p[[2L]]$data[[1L]]), c("S2", "p3"))

  # an rtables tree already has an indented stub: none is added
  tr <- .g4_tree()
  with_stub <- as_rtftables(tr, stub = stub_spec("V1"))[[1L]]
  expect_identical(.g4_bodies(list(with_stub)), .g4_bodies(as_rtftables(tr)))
  expect_identical(ncol(with_stub$data), 3L)
  pg <- as_rtftables(tr, split = "group_safe", max_rows = 2, group_by = "indent")
  expect_length(pg, 2L)
  expect_identical(pg[[1L]]$data[[1L]], c("F", "  Mean"))
  expect_identical(pg[[2L]]$data[[1L]], c("M", "  Mean"))
})

test_that("FT-TB3033 [FR-TB163]: auto_width sizes each column to its widest content and does not change the table width", {
  df <- data.frame(lab = c("a", "bbbbbbbbbbbbbbbbbb"), x = c("1", "2"), z = c("1", "2"),
                   stringsAsFactors = FALSE)
  names(df)[2L] <- "A very long header label here"
  on <- as_rtftables(df, auto_width = TRUE)[[1L]]
  off <- as_rtftables(df, auto_width = FALSE)[[1L]]
  w <- on$column_widths_twips
  expect_length(w, 3L)
  expect_gt(w[2L], w[1L])           # the 29-letter header label beats the 18-letter cell
  expect_gt(w[1L], w[3L])           # the 18-letter cell beats the 1-letter column
  expect_null(off$column_widths_twips)
  expect_identical(.g4_bodies(list(as_rtftables(df)[[1L]])), .g4_bodies(list(off)))
  expect_identical(as_rtftables(df)[[1L]]$column_widths_twips, off$column_widths_twips)
  expect_identical(on$table_width_twips, off$table_width_twips)   # it divides the columns only
  both <- as_rtftables(df, auto_width = TRUE, table_width_twips = 6000L)[[1L]]
  expect_identical(both$table_width_twips, 6000L)
  expect_equal(sum(both$column_widths_twips), 6000)
  expect_gt(both$column_widths_twips[2L], both$column_widths_twips[1L])
})

test_that("FT-TB3034 [FR-TB164]: table_width_twips is NULL by default and holds with and without auto_width", {
  df <- data.frame(lab = c("a", "b"), x = c("1", "2"), stringsAsFactors = FALSE)
  expect_null(as_rtftables(df)[[1L]]$table_width_twips)
  on <- as_rtftables(df, auto_width = TRUE, table_width_twips = 6000L)[[1L]]
  off <- as_rtftables(df, auto_width = FALSE, table_width_twips = 6000L)[[1L]]
  expect_identical(on$table_width_twips, 6000L)
  expect_identical(off$table_width_twips, 6000L)
  expect_equal(sum(on$column_widths_twips), 6000)
})

test_that("FT-TB3035 [FR-TB166]: forwarded arguments reach every page; explicit wins; col_header is resolved after the stub", {
  skip_if_not_installed("gt")
  x1 <- data.frame(a = c("1", "2"), b = c("3", "4"), stringsAsFactors = FALSE)
  x2 <- data.frame(a = "5", b = "6", stringsAsFactors = FALSE)
  p <- as_rtftables(list(a = x1, b = x2), col_rel_width = c(2, 1))
  expect_identical(names(p), c("a", "b"))
  expect_identical(p$a$col_rel_width, c(2, 1))
  expect_identical(p$b$col_rel_width, c(2, 1))

  g <- gt::gt(data.frame(a = c("x", "y"), b = c(1.5, 2.5), stringsAsFactors = FALSE))
  g <- gt::cols_label(g, a = "GA", b = "GB")
  g <- gt::cols_width(g, a ~ gt::px(100), b ~ gt::px(300))
  expect_identical(as_rtftables(g)[[1L]]$col_header, list(c("GA", "GB")))
  r <- as_rtftables(g, col_header = c("U", "V"), col_rel_width = c(1, 1))[[1L]]
  expect_identical(r$col_header, list(c("U", "V")))
  expect_identical(r$col_rel_width, c(1, 1))

  st <- data.frame(soc = c("S1", "S1", "S2"), pt = c("p1", "p2", "p3"),
                   n = c("1", "2", "3"), stringsAsFactors = FALSE)
  s <- as_rtftables(st, stub = stub_spec(c("soc", "pt")), col_header = c("Term", "N"))[[1L]]
  expect_identical(s$col_header, list(c("Term", "N")))     # two printed columns
  expect_error(
    as_rtftables(st, stub = stub_spec(c("soc", "pt")), col_header = c("a", "b", "c")),
    "the label row has 3 labels but the table has 2 printed columns", fixed = TRUE)
})

test_that("FT-TB3036 [FR-TB167]: cell_styles of the caller, one per row, follow their rows through the stub and the split", {
  d <- data.frame(a = c("a", "b", "c"), b = 1:3, stringsAsFactors = FALSE)
  bold_row <- function(k) lapply(1:3, function(i) list(bold = i == k))
  p <- as_rtftables(d, cell_styles = bold_row(1), split = "group_force", max_rows = 2,
                    group_col = 1)
  expect_length(p, 2L)
  expect_identical(vapply(p[[1L]]$cell_styles, function(s) isTRUE(s$bold), NA), c(TRUE, FALSE))
  expect_length(p[[2L]]$cell_styles, 1L)
  expect_false(isTRUE(p[[2L]]$cell_styles[[1L]]$bold))

  st <- data.frame(soc = c("S1", "S1", "S2"), pt = c("p1", "p2", "p3"),
                   n = c("1", "2", "3"), stringsAsFactors = FALSE)
  q <- as_rtftables(st, cell_styles = bold_row(3), stub = stub_spec(c("soc", "pt")),
                    split = "group_force", max_rows = 2, group_col = 1)
  expect_length(q, 3L)
  # pages: [S1, p1], [S1 (Cont.), p2], [S2, p3]; the style of row 3 (p3) is on the last row
  expect_identical(vapply(q, function(pg) length(pg$cell_styles), 1L), c(2L, 2L, 2L))
  bold <- lapply(q, function(pg) vapply(pg$cell_styles, function(s) isTRUE(s$bold), NA))
  expect_identical(bold[[1L]], c(FALSE, FALSE))
  expect_identical(bold[[2L]], c(FALSE, FALSE))
  expect_identical(bold[[3L]], c(FALSE, TRUE))
  expect_identical(.g4_unindent(q[[3L]]$data[[1L]]), c("S2", "p3"))
})

# ---- rtf_tables() -----------------------------------------------------------------

# the RTF text of a document
.g4_rtf <- function(doc) {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

# how many times `what` is in `txt` (fixed text)
.g4_count <- function(txt, what) lengths(regmatches(txt, gregexpr(what, txt, fixed = TRUE)))

# a small data frame of n rows
.g4_df <- function(n, prefix = "Item") {
  data.frame(Item = sprintf("%s %02d", prefix, seq_len(n)), A = seq_len(n),
             stringsAsFactors = FALSE)
}

# the backslash of RTF control words
.g4_bs <- "\\"

test_that("FT-TB3037 [FR-TB170]: rtf_tables() takes a plan, as_rtftables() pages, a gt, a gtsummary table, a plot and a list", {
  skip_if_not_installed("cards")
  skip_if_not_installed("gt")
  skip_if_not_installed("gtsummary")
  ard <- cards::ard_tabulate(cards::ADSL, by = ARM, variables = c(SEX, AGEGR1))
  p <- suppressMessages(table_plan(normalize_ard(ard), cols = "ARM"))
  p <- plan_paginate_rows(p, max_rows = 4, split = "group_force")
  pages <- suppressMessages(plan_apply(p, "pages"))
  expect_gt(length(pages), 1L)
  doc <- suppressMessages(rtf_tables(rtf_document(), p))
  expect_length(doc$contents, length(pages))
  for (i in seq_along(pages)) {
    expect_s3_class(doc$contents[[i]]$content, "rtftable")
    expect_identical(doc$contents[[i]]$content$data, pages[[i]]$data)
  }

  pg <- as_rtftables(.g4_df(4), split = "rows", split_rows = 3)
  d2 <- rtf_tables(rtf_document(), pg)
  expect_length(d2$contents, 2L)
  expect_identical(d2$contents[[2L]]$content$data$Item, c("Item 03", "Item 04"))

  g <- gt::gt(data.frame(a = c("x", "y"), b = c("1", "2"), stringsAsFactors = FALSE))
  d3 <- rtf_tables(rtf_document(), g)
  expect_s3_class(d3$contents[[1L]]$content, "rtftable")
  expect_identical(d3$contents[[1L]]$content$data$a, c("x", "y"))

  tb <- gtsummary::tbl_summary(gtsummary::trial, include = c(age, grade))
  d4 <- rtf_tables(rtf_document(), tb)
  expect_s3_class(d4$contents[[1L]]$content, "rtftable")
  expect_identical(d4$contents[[1L]]$content$data, as_rtftables(tb)[[1L]]$data)
  expect_identical(d4$contents[[1L]]$content$data$label[1L], "Age")

  png <- tempfile(fileext = ".png")
  on.exit(unlink(png), add = TRUE)
  grDevices::png(png, width = 200, height = 150)
  graphics::plot(1)
  grDevices::dev.off()
  d5 <- rtf_tables(rtf_document(), rtfplot(png))
  expect_length(d5$contents, 1L)
  expect_s3_class(d5$contents[[1L]]$content, "rtfplot")

  d6 <- rtf_tables(rtf_document(), list(g, data.frame(c = "z", stringsAsFactors = FALSE),
                                        rtfplot(png)))
  expect_identical(vapply(d6$contents, function(b) class(b$content)[1L], ""),
                   c("rtftable", "rtftable", "rtfplot"))
  expect_identical(d6$contents[[2L]]$content$data$c, "z")
})

test_that("FT-TB3038 [FR-TB172]: titles: one block per table, NULL none, styled rows, blank rows, centred and bold by default", {
  d <- .g4_df(2)
  two <- rtf_tables(rtf_document(), list(d, d), titles = list(list("A"), list("B")))
  expect_identical(two$contents[[1L]]$title, list("A"))
  expect_identical(two$contents[[2L]]$title, list("B"))
  one <- rtf_tables(rtf_document(), list(d, d), titles = list(list("Same")))
  expect_identical(one$contents[[2L]]$title, list("Same"))
  none <- rtf_tables(rtf_document(), list(d), titles = NULL)
  expect_null(none$contents[[1L]]$title)

  doc <- rtf_tables(rtf_document(), list(d), titles = list(list(
    "", list(text = "x", align = "left", italic = TRUE), "Plain")))
  txt <- .g4_rtf(doc)
  bs <- .g4_bs
  # the blank row, the styled row (left, italic, still bold), the plain row (centred, bold)
  expect_true(grepl(paste0(bs, "pard", bs, "qc", bs, "li0", bs, "ri0", bs, "par"), txt,
                    fixed = TRUE))
  expect_true(grepl(paste0(bs, "pard", bs, "ql", bs, "li0", bs, "ri0 ", bs, "b ", bs, "i x",
                           bs, "i0 ", bs, "b0 ", bs, "par"), txt, fixed = TRUE))
  expect_true(grepl(paste0(bs, "pard", bs, "qc", bs, "li0", bs, "ri0 ", bs, "b Plain",
                           bs, "b0 ", bs, "par"), txt, fixed = TRUE))
})

test_that("FT-TB3039 [FR-TB173]: footnotes: one block per table, NULL none, left-aligned and no rule by default", {
  d <- .g4_df(2)
  two <- rtf_tables(rtf_document(), list(d, d), footnotes = list(list("F1"), list("F2")))
  expect_identical(two$contents[[1L]]$footnote, list("F1"))
  expect_identical(two$contents[[2L]]$footnote, list("F2"))
  none <- rtf_tables(rtf_document(), list(d), footnotes = NULL)
  expect_null(none$contents[[1L]]$footnote)

  txt <- .g4_rtf(two)
  bs <- .g4_bs
  row <- function(w) paste0(bs, "trowd", bs, "trrh230", bs, "cellx13680", bs, "ql", bs, "li0",
                            bs, "ri0 ", w, bs, "cell", bs, "row")
  expect_true(grepl(row("F1"), txt, fixed = TRUE))   # left-aligned, no border control words
  expect_true(grepl(row("F2"), txt, fixed = TRUE))
})

test_that("FT-TB3040 [FR-TB175]: titles and footnotes of the wrong length stop with the full message", {
  d <- data.frame(A = 1L)
  expect_error(
    rtf_tables(rtf_document(), list(d), footnotes = list("a", "b")),
    "`footnotes` has 2 blocks but there is 1 page: give one block per page, or one block (a list of length 1) for every page.",
    fixed = TRUE)
  expect_error(
    rtf_tables(rtf_document(), list(d), titles = list("a", "b")),
    "`titles` has 2 blocks but there is 1 page: give one block per page, or one block (a list of length 1) for every page.",
    fixed = TRUE)
  expect_error(rtf_tables(rtf_document(), list(d), titles = 1),
               "`titles` must be a list (or NULL).", fixed = TRUE)
})

test_that("FT-TB3041 [FR-TB176]: page-number tokens in a title and a footnote reach the file", {
  d <- .g4_df(2)
  txt <- .g4_rtf(rtf_tables(
    rtf_document(), list(d, d),
    titles = list("T {PAGE}/{TOTAL_PAGES} {AUTO_TOTAL_PAGES}"),
    footnotes = list(list("G {PAGE} of {TOTAL_PAGES}"))))
  bs <- .g4_bs
  field <- paste0("{", bs, "field{", bs, "*", bs, "fldinst NUMPAGES}{", bs, "fldrslt ")
  # {PAGE} and {TOTAL_PAGES} are filled with the page's number and the count
  expect_true(grepl("T 1/2 ", txt, fixed = TRUE))
  expect_true(grepl("T 2/2 ", txt, fixed = TRUE))
  expect_true(grepl("G 1 of 2", txt, fixed = TRUE))
  expect_true(grepl("G 2 of 2", txt, fixed = TRUE))
  # {AUTO_TOTAL_PAGES} is a NUMPAGES field the viewer computes
  expect_gte(.g4_count(txt, field), 2L)
  expect_false(grepl("{TOTAL_PAGES}", txt, fixed = TRUE))
  expect_false(grepl("{PAGE}", txt, fixed = TRUE))
})

test_that("FT-TB3042 [FR-TB177]: auto_section opens a section where the name changes and appends the heading to the common header", {
  d <- function(v) data.frame(a = v, stringsAsFactors = FALSE)
  base <- rtf_section(rtf_document(),
                      secinfo = list(header = rtf_header(rows = list(c(l = "Study")))))
  p <- list(A = d("1"), A = d("2"), B = d("3"))
  r <- .pipe_doc_to_rtfreport(rtf_tables(base, p, auto_section = TRUE))
  expect_length(r$sections, 2L)
  expect_identical(r$sections[[1L]]$from_page, 1L)
  expect_identical(r$sections[[2L]]$from_page, 3L)
  rows1 <- r$sections[[1L]]$header$rows
  expect_identical(unname(vapply(rows1, function(x) x[[1L]], "")), c("Study", "A"))
  expect_identical(unname(vapply(r$sections[[2L]]$header$rows, function(x) x[[1L]], "")),
                   c("Study", "B"))

  # an empty name continues the open section
  p2 <- list(d("1"), d("2"), d("3"))
  names(p2) <- c("A", "", "B")
  r2 <- .pipe_doc_to_rtfreport(rtf_tables(base, p2, auto_section = TRUE))
  expect_length(r2$sections, 2L)
  expect_identical(r2$sections[[2L]]$from_page, 3L)

  # NULL (the default) takes what the pages say, else FALSE
  expect_length(.pipe_doc_to_rtfreport(rtf_tables(base, p))$sections, 1L)
  expect_length(.pipe_doc_to_rtfreport(rtf_tables(base, p, auto_section = FALSE))$sections, 1L)
  pages <- as_rtftables(p)
  expect_identical(names(pages), c("A...1", "A...2", "B"))
  for (i in seq_along(pages)) attr(pages[[i]], "rtf_heading_at") <- "section"
  expect_length(.pipe_doc_to_rtfreport(rtf_tables(base, pages))$sections, 2L)
  expect_length(.pipe_doc_to_rtfreport(rtf_tables(base, pages,
                                                  auto_section = FALSE))$sections, 1L)
})

test_that("FT-TB3043 [FR-TB178]: section_label_align sets the alignment of the section label row", {
  d <- function(v) data.frame(a = v, stringsAsFactors = FALSE)
  base <- rtf_section(rtf_document(),
                      secinfo = list(header = rtf_header(rows = list(c(l = "Study")))))
  p <- list(A = d("1"), B = d("2"))
  bs <- .g4_bs
  label_row <- function(w, q) {
    paste0(bs, "cellx13680", bs, "q", q, bs, "li0", bs, "ri0 ", w, bs, "cell", bs, "row")
  }
  aligns <- c(left = "l", center = "c", right = "r")
  for (arg in names(aligns)) {
    al <- aligns[[arg]]
    txt <- .g4_rtf(rtf_tables(base, p, auto_section = TRUE, section_label_align = arg))
    expect_true(grepl(label_row("A", al), txt, fixed = TRUE), info = arg)
    expect_true(grepl(label_row("B", al), txt, fixed = TRUE), info = arg)
    # the common header row ("Study") stays left-aligned
    expect_true(grepl(label_row("Study", "l"), txt, fixed = TRUE), info = arg)
  }
  # the default is "left"
  dflt <- .g4_rtf(rtf_tables(base, p, auto_section = TRUE))
  expect_true(grepl(label_row("A", "l"), dflt, fixed = TRUE))
})

test_that("FT-TB3044 [FR-TB182]: page_break takes one value each; TRUE is the default; later pages and the first content begin a page", {
  df <- function(n) .g4_df(n)
  doc <- rtf_tables(rtf_document(), list(df(3), df(3), df(3)),
                    page_break = c(TRUE, FALSE, FALSE))
  expect_identical(vapply(doc$contents, `[[`, NA, "page_break"), c(TRUE, FALSE, FALSE))
  expect_length(.pipe_doc_to_rtfreport(doc)$pages, 1L)       # the three tables share a page
  mid <- rtf_tables(rtf_document(), list(df(3), df(3), df(3)),
                    page_break = c(TRUE, TRUE, FALSE))
  expect_length(.pipe_doc_to_rtfreport(mid)$pages, 2L)

  explicit <- rtf_tables(rtf_document(), list(df(3), df(3)), page_break = TRUE)
  omitted <- rtf_tables(rtf_document(), list(df(3), df(3)))
  expect_identical(explicit, omitted)
  expect_identical(vapply(omitted$contents, `[[`, NA, "page_break"), c(TRUE, TRUE))
  expect_length(.pipe_doc_to_rtfreport(omitted)$pages, 2L)

  # the first content has no page to flow onto; a table's own later pages begin a page
  first <- rtf_tables(rtf_document(), list(df(3)), page_break = FALSE)
  expect_length(.pipe_doc_to_rtfreport(first)$pages, 1L)
  expect_identical(.g4_count(.g4_rtf(first), paste0(.g4_bs, "page")), 0L)
  later <- rtf_tables(rtf_tables(rtf_document(), list(df(5))),
                      as_rtftables(df(60), split = "rows", split_rows = 25),
                      page_break = FALSE)
  expect_identical(vapply(later$contents, `[[`, NA, "page_break"), c(TRUE, FALSE, TRUE))
  expect_length(.pipe_doc_to_rtfreport(later)$pages, 2L)
})

test_that("FT-TB3045 [FR-TB184]: first_rows on a data frame and an rtftable: every later page has its title, header and footnote", {
  base <- rtf_tables(rtf_document(), list(.g4_df(3)), titles = list("Table 1"))
  hdr <- paste0("Item", .g4_bs, "cell")
  for (src in list(.g4_df(10, "Row"), rtftable(.g4_df(10, "Row")))) {
    doc <- rtf_tables(base, src, titles = list("Table 2"), footnotes = list("Source note"),
                      page_break = FALSE, first_rows = 5)
    rep <- .pipe_doc_to_rtfreport(doc)
    expect_length(rep$pages, 2L)
    expect_identical(nrow(rep$pages[[1L]]$segments[[2L]]$content$data), 5L)  # the shared page
    expect_identical(nrow(rep$pages[[2L]]$content$data), 5L)                 # the rest
    txt <- .g4_rtf(doc)
    expect_identical(.g4_count(txt, "Table 2"), 2L)        # the shared page and the next
    expect_identical(.g4_count(txt, "Source note"), 2L)
    expect_identical(.g4_count(txt, hdr), 3L)              # Table 1's header and both of Table 2
  }
})

test_that("FT-TB3046 [FR-TB188]: cont_title is a line under the title of the pages after the shared one; NULL gives none", {
  base <- rtf_tables(rtf_document(), list(.g4_df(3)), titles = list("Table 1"))
  doc <- rtf_tables(base, .g4_df(10, "Row"), titles = list("Table 2"), page_break = FALSE,
                    first_rows = 3, cont_title = "(continued)")
  txt <- .g4_rtf(doc)
  expect_identical(.g4_count(txt, "(continued)"), 1L)       # the one page after the shared
  at <- gregexpr("Table 2", txt, fixed = TRUE)[[1L]]
  cont <- regexpr("(continued)", txt, fixed = TRUE)
  hdr <- gregexpr(paste0("Item", .g4_bs, "cell"), txt, fixed = TRUE)[[1L]]
  expect_gt(cont, at[2L])                                   # under the title of that page
  expect_lt(cont, hdr[3L])                                  # above its column header
  expect_lt(at[1L], cont)

  none <- rtf_tables(base, .g4_df(10, "Row"), titles = list("Table 2"), page_break = FALSE,
                     first_rows = 3)
  expect_identical(.g4_count(.g4_rtf(none), "(continued)"), 0L)
  expect_identical(.g4_count(.g4_rtf(none), "Table 2"), 2L)
})

test_that("FT-TB3047 [FR-TB192]: rtf_tables() reads a gt as as_rtftables() does, alone and in a list", {
  skip_if_not_installed("gt")
  g <- gt::gt(data.frame(a = c("x", "y"), b = c(1.5, 2.5), stringsAsFactors = FALSE))
  g <- gt::cols_label(g, a = "GA", b = "GB")
  g <- gt::cols_align(g, "right", columns = b)
  g <- gt::tab_header(g, title = "TT")
  g <- gt::tab_source_note(g, "SN")
  own <- as_rtftables(g)[[1L]]
  for (doc in list(rtf_tables(rtf_document(), g),
                   rtf_tables(rtf_document(), list(g, .g4_df(2))))) {
    blk <- doc$contents[[1L]]
    expect_identical(blk$content$col_header, list(c("GA", "GB")))      # the labels
    expect_identical(blk$content$col_spec[[2L]]$align, "right")        # the alignment
    expect_identical(blk$content$col_spec, own$col_spec)
    expect_identical(blk$title, "TT")                                  # the title
    expect_identical(blk$footnote, "SN")                               # the source note
    expect_identical(blk$content$data$b, c("1.5", "2.5"))
  }
})

# ---- stub_cols(), stub_spec() ---------------------------------------------------

# the data frame of a stub, without the pagination bookkeeping
.g4_plain <- function(d) {
  attr(d, "rtf_paginate_meta") <- NULL
  attr(d, "rtf_stub_src") <- NULL
  d
}

test_that("FT-TB3048 [FR-TB199]: group_summary folds an empty ('' or NA) and a parent-equal leaf onto the group's label row", {
  nb4 <- strrep(.g4_nbsp, 4L)
  mk <- function(pt) data.frame(soc = c("S1", "S1", "S1", "S2", "S2"), pt = pt,
                                n = as.character(1:5), stringsAsFactors = FALSE)
  na_leaf <- mk(c(NA, "p1", "p2", "S2", "p3"))
  em_leaf <- mk(c("", "p1", "p2", "S2", "p3"))
  vars <- c("soc", "pt")
  # an "" leaf folds like an NA leaf
  e_na <- stub_cols(na_leaf, vars = vars, group_summary = "empty")
  e_em <- stub_cols(em_leaf, vars = vars, group_summary = "empty")
  expect_identical(e_em, e_na)
  expect_identical(e_em[[1L]], c("S1", paste0(nb4, "p1"), paste0(nb4, "p2"), "S2",
                                 paste0(nb4, "S2"), paste0(nb4, "p3")))
  expect_identical(e_em$n, c("1", "2", "3", NA, "4", "5"))
  # "all" does both folds (the S2 leaf equals its parent): no row of its own
  all <- stub_cols(em_leaf, vars = vars, group_summary = "all")
  expect_identical(all[[1L]], c("S1", paste0(nb4, "p1"), paste0(nb4, "p2"), "S2",
                                paste0(nb4, "p3")))
  expect_identical(all$n, as.character(1:5))
  expect_identical(stub_cols(em_leaf, vars = vars, group_summary = c("empty", "parent")), all)
  # "parent" only folds the parent-equal leaf; the "" leaf keeps an indented row
  par <- stub_cols(em_leaf, vars = vars, group_summary = "parent")
  expect_identical(par[[1L]], c("S1", nb4, paste0(nb4, "p1"), paste0(nb4, "p2"), "S2",
                                paste0(nb4, "p3")))
  expect_identical(par$n, c(NA, "1", "2", "3", "4", "5"))
  # NULL disables like "none": every leaf is a row, S2's too
  none <- stub_cols(em_leaf, vars = vars, group_summary = "none")
  expect_identical(stub_cols(em_leaf, vars = vars, group_summary = NULL), none)
  expect_identical(nrow(none), 7L)
  expect_identical(none$n, c(NA, "1", "2", "3", NA, "4", "5"))
})

test_that("FT-TB3049 [FR-TB200]: the merged layout gives one indented stub column in place of the hierarchy", {
  d <- data.frame(group = c("A", "A", "B"), label = c("x", "y", "z"), v = c("1", "2", "3"),
                  stringsAsFactors = FALSE)
  out <- stub_cols(d, vars = c("group", "label"))
  nb4 <- strrep(.g4_nbsp, 4L)
  expect_identical(names(out), c("group / label", "v"))   # the hierarchy columns are gone
  expect_identical(out[[1L]], c("A", paste0(nb4, "x"), paste0(nb4, "y"), "B", paste0(nb4, "z")))
  expect_identical(out$v, c(NA, "1", "2", NA, "3"))
  expect_identical(stub_cols(d, vars = c("group", "label"), layout = "merged"), out)
})

test_that("FT-TB3050 [FR-TB207]: stub_spec() carries group_summary and as_rtftables() folds as stub_cols() does", {
  d <- data.frame(SOC = c("S1", "S1", "S1", "S2", "S2"), PT = c("", "p1", "p2", "S2", "p3"),
                  N = as.character(1:5), stringsAsFactors = FALSE)
  sp <- stub_spec(c("SOC", "PT"), group_summary = "all")
  expect_identical(sp$group_summary, "all")
  via_spec <- .g4_plain(as_rtftables(d, stub = sp)[[1L]]$data)
  expect_identical(via_spec, .g4_plain(stub_cols(d, vars = c("SOC", "PT"),
                                                 group_summary = "all")))
  expect_identical(nrow(via_spec), 5L)
  none <- stub_spec(c("SOC", "PT"), group_summary = "none")
  expect_identical(nrow(as_rtftables(d, stub = none)[[1L]]$data), 7L)
})

test_that("FT-TB3051 [FR-TB208]: stub_spec() takes the defaults of stub_cols()", {
  sp <- stub_spec(c("SOC", "PT"))
  expect_identical(sp$group_summary, c("empty", "parent"))
  expect_identical(sp$group_summary, eval(formals(stub_cols)$group_summary))
  expect_null(sp$label)
  expect_identical(sp$indent, 4L)
  expect_identical(sp$layout, "merged")
  expect_false(sp$label_span)
})

test_that("FT-TB3052 [FR-TB210]: stub_spec(layout) is refused at construction; the other settings when the stub is built", {
  expect_error(stub_spec(c("SOC", "PT"), layout = "nope"),
               "should be one of .merged., .columns.")
  sp <- stub_spec(c("SOC", "PT"), indent = -1)          # no error here
  expect_s3_class(sp, "rtf_stub_spec")
  d <- data.frame(SOC = c("S1", "S1"), PT = c("a", "b"), N = c("1", "2"),
                  stringsAsFactors = FALSE)
  expect_error(as_rtftables(d, stub = sp),
               "`indent` must be a single non-negative integer.", fixed = TRUE)
})

test_that("FT-TB3053 [FR-TB211]: print() of a stub_spec shows the vars, layout and indent", {
  sp <- stub_spec(c("SOC", "PT"), indent = 2, layout = "columns")
  expect_output(print(sp), "<rtf_stub_spec>", fixed = TRUE)
  expect_output(print(sp), "vars +: SOC, PT")
  expect_output(print(sp), "layout +: columns")
  expect_output(print(sp), "indent +: 2")
})

# ---- set_col_header(), add_header_row(), header_map() -------------------------------

test_that("FT-TB3054 [FR-TB212]: set_col_header() cells by visible position land on the final printed columns", {
  d <- data.frame(x = c("q", "q"), soc = c("S1", "S1"), pt = c("a", "b"), n = c("1", "2"),
                  m = c("3", "4"), stringsAsFactors = FALSE)
  pg <- as_rtftables(d, drop_cols = "x", stub = stub_spec(c("soc", "pt")))[[1L]]
  expect_identical(names(pg$data), c("soc / pt", "n", "m"))
  by_pos <- set_col_header(pg, list(col_cell(2:3, "Span")), c("Lab", "N", "M"))
  expect_identical(by_pos$col_header,
                   list(list(list(from = 1L, to = 1L, label = ""),
                             list(from = 2L, to = 3L, label = "Span")),
                        c("Lab", "N", "M")))
  by_name <- set_col_header(pg, list(col_cell(c("n", "m"), "Span")), c("Lab", "N", "M"))
  expect_identical(by_name$col_header, by_pos$col_header)
  one <- set_col_header(pg, list(col_cell(1L, "Only first")))     # 1 is the stub, not "x"
  expect_identical(one$col_header[[1L]][[1L]],
                   list(from = 1L, to = 1L, label = "Only first"))
})

test_that("FT-TB3055 [FR-TB213]: set_col_header(...) takes a prebuilt header, one argument as a whole header, several as rows", {
  t2 <- rtftable(data.frame(A = 1, B = 2))
  pre <- set_col_header(t2, rtf_col_header(list(col_cell(1:2, "Top")), c("a", "b")))
  expect_identical(pre$col_header, list(list(list(from = 1L, to = 2L, label = "Top")),
                                        c("a", "b")))
  one <- set_col_header(t2, c("A1", "B1"))
  expect_identical(one$col_header, list(c("A1", "B1")))
  two <- set_col_header(t2, c("A1", "B1"), c("A2", "B2"))
  expect_identical(two$col_header, list(c("A1", "B1"), c("A2", "B2")))
})

test_that("FT-TB3056 [FR-TB216]: a col_cell pos may be a selector; one matching no column or non-adjacent columns is an error", {
  d <- data.frame(lab = "x", `T1__n` = "1", `T1__p` = "2", `T2__n` = "3",
                  check.names = FALSE, stringsAsFactors = FALSE)
  tb <- rtftable(d)
  ok <- set_col_header(tb, list(col_cell(col_key("T1", sep = "__", part = 1), "Group 1")),
                       c("l", "n", "p", "n"))
  expect_identical(ok$col_header[[1L]][[2L]], list(from = 2L, to = 3L, label = "Group 1"))
  expect_error(
    set_col_header(tb, list(col_cell(col_key("n", sep = "__", part = 2), "X")),
                   c("l", "a", "b", "c")),
    "col_key(\"n\") matched non-adjacent data columns (2, 4); a header cell can only span a contiguous range.",
    fixed = TRUE)
  expect_error(
    set_col_header(tb, list(col_cell(col_key("zzz", sep = "__", part = 1), "X")),
                   c("l", "a", "b", "c")),
    "col_key(\"zzz\") matched no data columns.", fixed = TRUE)
  expect_error(set_col_header(tb, list(col_cell(c("lab", "zzz"), "X"))),
               "`pos` column name \"zzz\" not found in data columns.", fixed = TRUE)
  expect_error(set_col_header(tb, list(col_cell(c(1L, 9L), "X"))),
               "`pos` 1-9 outside data column range 1..4.", fixed = TRUE)
})

# six column pages: two groups of period, each with two page_by values
.g4_vt_pages <- function() {
  d <- data.frame(period = c("P1", "P1", "P2", "P2"), pg = c("1", "2", "1", "2"),
                  lab = c("x", "y", "z", "w"), v1 = "1", v2 = "2", stringsAsFactors = FALSE)
  as_rtftables(d, split = "by_value", group_col = "period", page_by = "pg",
               drop_cols = c("pg", "period"))
}

test_that("FT-TB3057 [FR-TB223]: set_col_header(by = c('group', 'rows')) matches on the combination", {
  pg <- .g4_vt_pages()
  expect_identical(names(pg), c("P1...1", "P1...2", "P2...1", "P2...2"))
  hh <- rtf_col_header(list(col_cell(2:3, "Grp (N={n})")), c("Lab", "a", "b"))
  vals <- expand.grid(group = c("P1", "P2"), rows = c("1", "2"), stringsAsFactors = FALSE)
  vals$n <- seq_len(nrow(vals))          # (P1,1)=1 (P2,1)=2 (P1,2)=3 (P2,2)=4
  out <- set_col_header(pg, hh, values = vals, by = c("group", "rows"))
  expect_identical(unname(vapply(out, function(p) p$col_header[[1L]][[2L]]$label, "")),
                   c("Grp (N=1)", "Grp (N=3)", "Grp (N=2)", "Grp (N=4)"))
  # "name": the unique page name, "...n" included
  byname <- set_col_header(pg, hh, values = data.frame(name = names(pg), n = 11:14,
                                                       stringsAsFactors = FALSE), by = "name")
  expect_identical(unname(vapply(byname, function(p) p$col_header[[1L]][[2L]]$label, "")),
                   paste0("Grp (N=", 11:14, ")"))
  # "group", the default, with no group key of its own: the page name without its "...n"
  named <- as_rtftables(list(A = data.frame(g = c("u", "v", "w"), k = 1:3,
                                            stringsAsFactors = FALSE)),
                        split = "rows", split_rows = c(2, 3))
  expect_identical(names(named), c("A...1", "A...2", "A...3"))
  h2 <- rtf_col_header(list(col_cell(1:2, "N={n}")), c("g", "k"))
  expect_message(
    seven <- set_col_header(named, h2, values = data.frame(group = "A", n = 7,
                                                           stringsAsFactors = FALSE)),
    "no group axis, so `by = \"group\"` matches on the page name instead", fixed = TRUE)
  expect_identical(unname(vapply(seven, function(p) p$col_header[[1L]][[1L]]$label, "")),
                   rep("N=7", 3L))
})

test_that("FT-TB3058 [FR-TB227]: set_col_header() on a named list returns a list of the same names", {
  d1 <- data.frame(Item = "Age", A = "1", B = "3", stringsAsFactors = FALSE)
  out <- set_col_header(list(a = rtftable(d1), b = rtftable(d1)), c("I", "P", "D"))
  expect_type(out, "list")
  expect_identical(names(out), c("a", "b"))
  expect_identical(out$a$col_header, list(c("I", "P", "D")))
  expect_identical(out$b$col_header, list(c("I", "P", "D")))
  # a table of several data frames takes the header for each of them
  m <- set_col_header(rtftable(list(d1, d1)), c("i1", "p1", "d1"), c("i2", "p2", "d2"))
  expect_length(m$col_header_list, 2L)
  expect_identical(m$col_header_list[[1L]], list(c("i1", "p1", "d1"), c("i2", "p2", "d2")))
  expect_identical(m$col_header_list[[2L]], m$col_header_list[[1L]])
})

test_that("FT-TB3059 [FR-TB232]: add_header_row() places a named character row by column name", {
  t2 <- rtftable(data.frame(a = "1", b = "2", stringsAsFactors = FALSE))
  x <- add_header_row(t2, c(b = "B2", a = "A2"))
  expect_identical(x$col_header[[1L]], c("A2", "B2"))
  expect_identical(x$col_header[[2L]], c("a", "b"))
  y <- add_header_row(t2, c(b = "B2", a = "A2"), .position = "bottom")
  expect_identical(y$col_header[[length(y$col_header)]], c("A2", "B2"))
})

test_that("FT-TB3060 [FR-TB251]: header_map() has one row per header cell of every page; a label cell covers one column", {
  pg <- set_col_header(.g4_vt_pages(), list(col_cell(2:3, "Grp")), c("Lab", "a", "b"))
  hm <- header_map(pg)
  expect_identical(names(hm), c("page", "name", "group", "rows", "row", "cell", "from", "to",
                                "text"))
  # per page: a spanning row of 2 cells (the unspanned column 1 and "Grp") and 3 labels
  expect_identical(nrow(hm), 4L * (2L + 3L))
  expect_identical(hm$page, rep(1:4, each = 5L))
  expect_identical(hm$row, rep(c(1L, 1L, 2L, 2L, 2L), 4L))
  label_cells <- hm[hm$row == 2L, ]
  expect_true(all(label_cells$from == label_cells$to))
  expect_identical(label_cells$from[1:3], 1:3)
  expect_identical(label_cells$text[1:3], c("Lab", "a", "b"))
  span <- hm[hm$row == 1L & hm$text == "Grp", ]
  expect_identical(nrow(span), 4L)
  expect_true(all(span$from == 2L & span$to == 3L & span$from < span$to))
})

test_that("FT-TB3061 [FR-TB253]: header_map() gives group (else the name without '...n') and rows (NA when none)", {
  pg <- set_col_header(.g4_vt_pages(), list(col_cell(2:3, "Grp")), c("Lab", "a", "b"))
  hm <- header_map(pg)
  first <- hm[!duplicated(hm$page), ]
  expect_identical(first$name, c("P1...1", "P1...2", "P2...1", "P2...2"))
  expect_identical(first$group, c("P1", "P1", "P2", "P2"))   # the by_value key
  expect_identical(first$rows, c("1", "2", "1", "2"))        # the page_by value
  d <- data.frame(p = c("u", "v"), k = 1:2, stringsAsFactors = FALSE)
  named <- header_map(list(A = rtftable(d, col_header = c("p", "k")),
                           B = rtftable(d, col_header = c("p", "k"))))
  expect_identical(named$group[!duplicated(named$page)], c("A", "B"))   # no group key: the name
  expect_identical(named$rows[!duplicated(named$page)], c(NA_character_, NA_character_))
  tail <- as_rtftables(list(A = d), split = "rows", split_rows = 2)
  expect_identical(names(tail), c("A...1", "A...2"))
  hm2 <- header_map(set_col_header(tail, c("p", "k")))
  expect_identical(hm2$group[!duplicated(hm2$page)], c("A", "A"))        # "...n" stripped
  expect_true(all(is.na(hm2$rows)))
})

test_that("FT-TB3062 [FR-TB254]: header_map() gives no row to a page without a header; a zero-row frame has the same columns", {
  d <- data.frame(p = c("u", "v"), k = 1:2, stringsAsFactors = FALSE)
  with_hdr <- rtftable(d, col_header = c("p", "k"))
  mixed <- header_map(list(with_hdr, rtftable(d)))
  expect_identical(unique(mixed$page), 1L)
  expect_identical(nrow(mixed), 2L)
  none <- header_map(rtftable(d))
  expect_identical(nrow(none), 0L)
  expect_identical(names(none), names(header_map(with_hdr)))
  expect_identical(vapply(none, class, ""), vapply(header_map(with_hdr), class, ""))
})

test_that("FT-TB3063 [FR-TB260]: add_cont_label() puts NA in a non-character column and resets the row names", {
  df <- data.frame(g = c("B", "B"), n = c(3, 4), stringsAsFactors = FALSE)
  rownames(df) <- c("r7", "r9")
  out <- add_cont_label(df, "G")
  expect_identical(out$g, c("G (Cont.)", "B", "B"))
  expect_identical(out$n, c(NA, 3, 4))
  expect_identical(rownames(out), c("1", "2", "3"))
  expect_identical(attr(out, "row.names"), 1:3)
})

# ---- paginate_cols() -------------------------------------------------------------------

.g4_pc_df <- function() {
  data.frame(P = c("a", "b"), A_n = c("1", "2"), A_m = c("3", "4"), B_n = c("5", "6"),
             B_m = c("7", "8"), stringsAsFactors = FALSE)
}

.g4_pc_hdr <- function() {
  list(list(col_cell(c(2, 3), "Alpha"), col_cell(c(4, 5), "Beta")),
       c("P", "n", "m", "n", "m"))
}

test_that("FT-TB3064 [FR-TB271]: each column block's header holds the kept column and that block's labels", {
  tb <- rtftable(.g4_pc_df(), col_header = .g4_pc_hdr())
  pg <- paginate_cols(tb, at = 4, fit = FALSE)
  expect_length(pg, 2L)
  expect_identical(names(pg[[1L]]$data), c("P", "A_n", "A_m"))
  expect_identical(pg[[1L]]$col_header,
                   list(list(list(from = 1L, to = 1L, label = ""),
                             list(from = 2L, to = 3L, label = "Alpha")),
                        c("P", "n", "m")))
  expect_identical(names(pg[[2L]]$data), c("P", "B_n", "B_m"))
  expect_identical(pg[[2L]]$col_header,
                   list(list(list(from = 1L, to = 1L, label = ""),
                             list(from = 2L, to = 3L, label = "Beta")),
                        c("P", "n", "m")))
})

test_that("FT-TB3065 [FR-TB272]: a list of pages is cut alike; positions refer to the printed columns", {
  tb <- rtftable(.g4_pc_df(), col_header = .g4_pc_hdr())
  out <- paginate_cols(list(a = tb, b = tb), at = 4, fit = FALSE)
  expect_length(out, 4L)
  expect_identical(names(out), c("a...1", "b...1", "a...2", "b...2"))
  cols <- vapply(out, function(p) paste(names(p$data), collapse = ","), "")
  expect_identical(unname(cols), c("P,A_n,A_m", "P,A_n,A_m", "P,B_n,B_m", "P,B_n,B_m"))

  # after drop_cols, a stub and a user col_header: position 3 is "m" of the printed table
  d <- data.frame(x = c("q", "q"), soc = c("S1", "S1"), pt = c("a", "b"), n = c("1", "2"),
                  m = c("3", "4"), k = c("5", "6"), stringsAsFactors = FALSE)
  pg <- as_rtftables(d, drop_cols = "x", stub = stub_spec(c("soc", "pt")),
                     col_header = c("Term", "N", "M", "K"))[[1L]]
  expect_identical(names(pg$data), c("soc / pt", "n", "m", "k"))
  cut <- paginate_cols(pg, at = 3, fit = FALSE)
  expect_identical(names(cut[[1L]]$data), c("soc / pt", "n"))
  expect_identical(names(cut[[2L]]$data), c("soc / pt", "m", "k"))
  expect_identical(cut[[1L]]$col_header, list(c("Term", "N")))
  expect_identical(cut[[2L]]$col_header, list(c("Term", "M", "K")))
})

test_that("FT-TB3066 [FR-TB283]: paginate_cols() with no way to cut, or two, stops with the full message", {
  tb <- rtftable(.g4_pc_df())
  expect_error(paginate_cols(tb),
               "One of `at` (the columns to cut before), `cut_by` or `every` is required.",
               fixed = TRUE)
  expect_error(paginate_cols(tb, at = 2, cut_by = list(2:3)),
               "Give one of `at` or `cut_by`, not both.", fixed = TRUE)
  expect_error(paginate_cols(tb, at = 2, every = 2),
               "`paginate_cols()`: say where to cut one way -- `at`, `cut_by` or `every`.",
               fixed = TRUE)
  expect_error(paginate_cols(tb, cut_by = list(2:3), every = 2),
               "`paginate_cols()`: say where to cut one way -- `at`, `cut_by` or `every`.",
               fixed = TRUE)
})

test_that("FT-TB3067 [FR-TB285]: every column kept stops with the message of at/every or of cut_by", {
  tb <- rtftable(.g4_pc_df())
  expect_error(paginate_cols(tb, at = 4, keep = 1:5),
               "`paginate_cols()` produced no column blocks: every column is a kept (row-heading) column.",
               fixed = TRUE)
  expect_error(paginate_cols(tb, cut_by = "_", keep = 1:5),
               "`cut_by` left no columns to split: every column is a kept column.",
               fixed = TRUE)
})

test_that("FT-TB3068 [FR-TB286]: paginate_cols() on a list that is not pages, or pages of differing columns, stops", {
  expect_error(
    paginate_cols(list(1, 2), at = 2),
    "`paginate_cols()` on a list expects every element to be an rtftable (as returned by as_rtftables()); element 1 is 'numeric'.",
    fixed = TRUE)
  d <- .g4_pc_df()
  expect_error(
    paginate_cols(list(rtftable(d), rtftable(d[1:3])), at = 2),
    "`paginate_cols()` needs every page to have the same columns; page 1 has 5 and page 2 has 3.",
    fixed = TRUE)
})

# ---- blank rows ------------------------------------------------------------------------

.g4_blank_df <- function() {
  data.frame(g = c("A", "A", "B", "B", "C"), v = 1:5, stringsAsFactors = FALSE)
}

test_that("FT-TB3069 [FR-TB301]: set_blank_rows() gives sorted, unique integer positions and leaves the data", {
  df <- .g4_blank_df()
  out <- set_blank_rows(df, blank_rows = c(3, 1, 3, 0))
  expect_identical(attr(out, "rtf_blank_rows"), c(0L, 1L, 3L))
  plain <- out
  attr(plain, "rtf_blank_rows") <- NULL
  expect_identical(plain, df)
})

test_that("FT-TB3070 [FR-TB306]: set_blank_rows() drops an NA and a position above nrow(data) without a warning; -1 is after the last row", {
  df <- .g4_blank_df()
  expect_identical(attr(set_blank_rows(df, blank_rows = c(-1, 2)), "rtf_blank_rows"),
                   c(2L, nrow(df)))
  expect_no_warning(out <- set_blank_rows(df, blank_rows = c(NA, 99, 2)))
  expect_identical(attr(out, "rtf_blank_rows"), 2L)
  expect_no_warning(out2 <- set_blank_rows(df, blank_rows = c(-5, 2)))
  expect_identical(attr(out2, "rtf_blank_rows"), 2L)
})

test_that("FT-TB3071 [FR-TB308]: a by_change spec is resolved on this data frame, each page on its own rows", {
  df <- .g4_blank_df()
  expect_identical(attr(set_blank_rows(df, blank_rows = blank_rows_by_change("g")),
                        "rtf_blank_rows"), c(2L, 4L))
  d2 <- data.frame(grp = c("A", "A", "B", "B", "C", "C"), v = 1:6, stringsAsFactors = FALSE)
  p <- as_rtftables(d2, blank_rows = blank_rows_by_change("grp"), split = "rows",
                    split_rows = 4)
  expect_length(p, 2L)
  expect_identical(p[[1L]]$data$grp, c("A", "A", "B"))
  expect_identical(p[[1L]]$blank_rows, 2L)    # A,A | B
  expect_identical(p[[2L]]$data$grp, c("B", "C", "C"))
  expect_identical(p[[2L]]$blank_rows, 1L)    # B | C,C
})

test_that("FT-TB3072 [FR-TB309]: a plain list of forms is unioned, duplicates dropped", {
  df <- .g4_blank_df()
  out <- set_blank_rows(df, blank_rows = list(1L, blank_rows_by_change("g"),
                                              blank_rows_by_rule("g", "^C")))
  # 1; the group changes after rows 2 and 4; "^C" (row 5) gets a blank before it: 4 again
  expect_identical(attr(out, "rtf_blank_rows"), c(1L, 2L, 4L))
})

test_that("FT-TB3073 [FR-TB313]: set_blank_rows(group_col) names the column groups are detected on", {
  d3 <- data.frame(id = c("x1", "x2", "x3", "x4"), grp = c("A", "A", "B", "B"),
                   stringsAsFactors = FALSE)
  at <- function(...) attr(set_blank_rows(d3, "between_groups", ...), "rtf_blank_rows")
  expect_identical(at(group_col = 2L), 2L)
  expect_identical(at(group_col = "grp"), 2L)
  expect_identical(at(group_col = NULL), c(1L, 2L, 3L))      # column 1: every id differs
  expect_identical(at(), c(1L, 2L, 3L))
  # used only with "between_groups"
  expect_identical(attr(set_blank_rows(d3, c(1, 2), group_col = 2L), "rtf_blank_rows"),
                   c(1L, 2L))
})

test_that("FT-TB3074 [FR-TB315]: set_blank_rows(group_by) auto equals the matching mode; a bogus value is refused", {
  d4 <- data.frame(g = c("A", "", "", "B", "", "C"), v = 1:6, stringsAsFactors = FALSE)
  at <- function(...) attr(set_blank_rows(d4, "between_groups", ...), "rtf_blank_rows")
  expect_identical(at(group_by = "filled"), c(3L, 5L))
  expect_identical(at(), at(group_by = "filled"))
  expect_identical(at(group_by = "auto"), at(group_by = "filled"))
  expect_identical(at(group_by = "value"), c(1L, 3L, 4L, 5L))
  expect_error(at(group_by = "bogus"), "should be one of .auto., .indent., .value., .filled.")
  d5 <- data.frame(g = c("P", "  a", "  b", "Q", "  c"), v = 1:5, stringsAsFactors = FALSE)
  at5 <- function(...) attr(set_blank_rows(d5, "between_groups", ...), "rtf_blank_rows")
  expect_identical(at5(group_by = "indent"), 3L)
  expect_identical(at5(), 3L)
})

test_that("FT-TB3075 [FR-TB317]: blank_rows_by_change() holds its four settings and no positions; set_blank_rows() resolves it", {
  x <- blank_rows_by_change(c("a", "b"), group_by = "value", include_before_first = FALSE,
                            include_after_last = TRUE)
  expect_s3_class(x, "rtf_blank_rows_by_change")
  expect_identical(names(unclass(x)),
                   c("cols", "group_by", "include_before_first", "include_after_last"))
  expect_identical(x$cols, c("a", "b"))
  expect_identical(x$group_by, "value")
  expect_false(x$include_before_first)
  expect_true(x$include_after_last)
  df <- .g4_blank_df()
  expect_identical(attr(set_blank_rows(df, blank_rows = blank_rows_by_change("g")),
                        "rtf_blank_rows"), c(2L, 4L))
  expect_null(attr(df, "rtf_blank_rows"))                    # it inserted nothing by itself
})

test_that("FT-TB3076 [FR-TB318]: a change in any of the columns gets a blank row", {
  df <- data.frame(a = c("p", "p", "q", "q", "q"), b = c("u", "v", "v", "w", "w"),
                   stringsAsFactors = FALSE)
  expect_identical(rtftable(df, blank_rows = blank_rows_by_change("a"))$blank_rows,
                   c(0L, 2L, 5L))
  expect_identical(rtftable(df, blank_rows = blank_rows_by_change("b"))$blank_rows,
                   c(0L, 1L, 3L, 5L))
  expect_identical(rtftable(df, blank_rows = blank_rows_by_change(c("a", "b")))$blank_rows,
                   c(0L, 1L, 2L, 3L, 5L))
})

test_that("FT-TB3077 [FR-TB323]: blank_rows_by_change(group_by = 'auto') on an indented column acts as 'indent'", {
  d6 <- data.frame(g = c("P", "  a", "  b", "Q", "  c"), stringsAsFactors = FALSE)
  auto <- rtftable(d6, blank_rows = blank_rows_by_change("g", group_by = "auto"))$blank_rows
  indent <- rtftable(d6, blank_rows = blank_rows_by_change("g", group_by = "indent"))$blank_rows
  expect_identical(indent, c(0L, 3L, 5L))
  expect_identical(auto, indent)
  value <- rtftable(d6, blank_rows = blank_rows_by_change("g"))$blank_rows   # "value" default
  expect_identical(value, c(0L, 1L, 2L, 3L, 4L, 5L))
})

test_that("FT-TB3078 [FR-TB325 FR-TB326]: any include_before_first / include_after_last other than TRUE counts as FALSE", {
  df <- data.frame(a = c("p", "p", "q", "q", "q"), stringsAsFactors = FALSE)
  for (v in list("yes", 1, NA, FALSE)) {
    before <- rtftable(df, blank_rows = blank_rows_by_change("a", include_before_first = v))
    expect_identical(before$blank_rows, c(2L, 5L), info = format(v))      # no 0
    after <- rtftable(df, blank_rows = blank_rows_by_change("a", include_after_last = v))
    expect_identical(after$blank_rows, c(0L, 2L), info = format(v))       # no nrow(data)
  }
  expect_identical(rtftable(df, blank_rows = blank_rows_by_change("a"))$blank_rows,
                   c(0L, 2L, 5L))
})

test_that("FT-TB3079 [FR-TB327]: a column that is not in the data warns and the other columns still apply", {
  df <- data.frame(a = c("p", "p", "q", "q", "q"), stringsAsFactors = FALSE)
  expect_warning(
    tb <- rtftable(df, blank_rows = blank_rows_by_change(c("x", "a"))),
    "blank_rows_by_change column 'x' not found; ignored.", fixed = TRUE)
  expect_identical(tb$blank_rows, c(0L, 2L, 5L))
})

test_that("FT-TB3080 [FR-TB328]: one row has no change to find: only the before-first and after-last rows", {
  one <- data.frame(g = "A", stringsAsFactors = FALSE)
  expect_identical(rtftable(one, blank_rows = blank_rows_by_change("g"))$blank_rows,
                   c(0L, 1L))
  none <- blank_rows_by_change("g", include_before_first = FALSE, include_after_last = FALSE)
  expect_length(rtftable(one, blank_rows = none)$blank_rows, 0L)
})

test_that("FT-TB3081 [FR-TB329]: a by_change spec resolved on a page never falls on its first or last row", {
  df <- .g4_blank_df()
  spec <- blank_rows_by_change("g", include_before_first = TRUE, include_after_last = TRUE)
  pos <- attr(set_blank_rows(df, blank_rows = spec), "rtf_blank_rows")
  expect_identical(pos, c(2L, 4L))
  expect_false(any(pos %in% c(0L, nrow(df))))
  pg <- as_rtftables(df, blank_rows = spec)[[1L]]
  expect_identical(pg$blank_rows, c(2L, 4L))
})

test_that("FT-TB3082 [FR-TB330]: blank_rows_by_rule() holds col, pattern and where", {
  x <- blank_rows_by_rule("lab", "^Total", "after")
  expect_s3_class(x, "rtf_blank_rows_by_rule")
  expect_identical(names(unclass(x)), c("col", "pattern", "where"))
  expect_identical(x$col, "lab")
  expect_identical(x$pattern, "^Total")
  expect_identical(x$where, "after")
  expect_identical(blank_rows_by_rule("lab", "^Total")$where, "before")
})

test_that("FT-TB3083 [FR-TB333]: a missing value in the column never matches the pattern", {
  df <- data.frame(lab = c("a", NA, "Total"), stringsAsFactors = FALSE)
  expect_identical(rtftable(df, blank_rows = blank_rows_by_rule("lab", "."))$blank_rows,
                   c(0L, 2L))                    # rows 1 and 3, a blank before each
  expect_identical(rtftable(df, blank_rows = blank_rows_by_rule("lab", ".", "after"))$blank_rows,
                   c(1L, 3L))
})

# ---- set_decimal_split() ---------------------------------------------------------------

.g4_ds_df <- function() {
  data.frame(Item = c("a", "b"), Mean = c("12.5", "3.25"), SD = c("1.0", "0.5"),
             stringsAsFactors = FALSE)
}

test_that("FT-TB3084 [FR-TB341]: the console preview of a split table shows the cells unsplit", {
  tb <- rtftable(.g4_ds_df())
  sp <- set_decimal_split(tb, cols = 2)
  expect_false(is.null(sp$decimal_split))
  expect_identical(sp$data, tb$data)
  shown <- capture.output(print(sp))
  expect_identical(shown, capture.output(print(tb)))
  expect_true(any(grepl("12.5", shown, fixed = TRUE)))
  expect_true(any(grepl("3.25", shown, fixed = TRUE)))
})

test_that("FT-TB3085 [FR-TB343]: set_decimal_split() on a list of non-tables stops with the message", {
  expect_error(
    set_decimal_split(list(1, 2), cols = 1),
    "`set_decimal_split()` on a list expects every element to be an rtftable (as returned by as_rtftables()); element 1 is 'numeric'.",
    fixed = TRUE)
})

test_that("FT-TB3086 [FR-TB344]: set_decimal_split(cols) is stored as positions of the final printed columns", {
  tb <- rtftable(.g4_ds_df())
  expect_identical(set_decimal_split(tb, cols = 2L)$decimal_split$cols, 2L)
  expect_identical(set_decimal_split(tb, cols = list("Mean", 3L))$decimal_split$cols, c(2L, 3L))
  d <- data.frame(x = c("q", "q"), soc = c("S1", "S1"), pt = c("a", "b"), n = c("1.5", "2.25"),
                  m = c("3", "4"), stringsAsFactors = FALSE)
  pg <- as_rtftables(d, drop_cols = "x", stub = stub_spec(c("soc", "pt")))[[1L]]
  expect_identical(names(pg$data), c("soc / pt", "n", "m"))
  expect_identical(set_decimal_split(pg, cols = "m")$decimal_split$cols, 3L)
  expect_identical(set_decimal_split(pg, cols = list("n", 3L))$decimal_split$cols, c(2L, 3L))
})

test_that("FT-TB3087 [FR-TB347]: set_decimal_split(cols) of a missing column or an outside position stops with the message", {
  tb <- rtftable(.g4_ds_df())
  expect_error(set_decimal_split(tb, cols = "Nope"),
               "`set_decimal_split(cols)` column 'Nope' not found in the table.", fixed = TRUE)
  expect_error(set_decimal_split(tb, cols = 9),
               "`set_decimal_split(cols)` index 9 out of range (1..3).", fixed = TRUE)
})

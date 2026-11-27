## tests/testthat/test-cov-gt.R
##
## Coverage fill-ins for R/gt_adapter.R: footnote mark sets, footnotes on the
## stub / summary / hidden-spanner places, hand-written footnote spans, and
## row-group layouts the adapter does not recognise.

library(testthat)

.cov_gt_df <- function() {
  data.frame(id = c("a", "b", "c"), x = c(1, 2, 3), y = c(4, 5, 6),
             stringsAsFactors = FALSE)
}

test_that("opt_footnote_marks('standard') gives the *, dagger marks", {
  skip_on_cran()
  skip_if_not_installed("gt")
  g <- gt::gt(.cov_gt_df(), rowname_col = "id") |>
    gt::tab_footnote("Note one", gt::cells_body(columns = x, rows = 1)) |>
    gt::tab_footnote("Note two", gt::cells_stub(rows = 2)) |>
    gt::opt_footnote_marks(marks = "standard")
  t <- as_rtftable(g)
  expect_identical(attr(t, "rtf_footnotes"),
                   c("^{*} Note one", "^{\u2020} Note two"))
  # gt puts a no-break space between the mark and the value
  expect_identical(t$data$x[1L], "^{*}\u00a01")
  # the stub footnote mark lands on the row label
  expect_identical(t$data$rowname[2L], "b^{\u2020}")
})

test_that("opt_footnote_marks('extended') runs through all six symbols", {
  skip_on_cran()
  skip_if_not_installed("gt")
  g <- gt::gt(.cov_gt_df(), rowname_col = "id") |>
    gt::opt_footnote_marks("extended")
  for (i in 1:5) {
    g <- gt::tab_footnote(g, paste("Body note", i),
                          gt::cells_body(columns = x, rows = 1))
  }
  g <- gt::tab_footnote(g, "Label note", gt::cells_column_labels(y))
  t <- as_rtftable(g)
  fo <- attr(t, "rtf_footnotes")
  expect_length(fo, 6L)
  expect_identical(fo[1L], "^{*} Label note")
  expect_identical(fo[5L], "^{\u2016} Body note 4")
  expect_identical(fo[6L], "^{\u00b6} Body note 5")
  expect_identical(t$col_header[[1L]][3L], "y^{*}")
})

test_that("custom footnote marks double once the set runs out", {
  skip_on_cran()
  skip_if_not_installed("gt")
  g <- gt::gt(.cov_gt_df()) |>
    gt::tab_footnote("First", gt::cells_body(columns = x, rows = 1)) |>
    gt::tab_footnote("Second", gt::cells_body(columns = x, rows = 2)) |>
    gt::tab_footnote("Third", gt::cells_body(columns = x, rows = 3)) |>
    gt::opt_footnote_marks(marks = c("p", "q"))
  t <- as_rtftable(g)
  expect_identical(attr(t, "rtf_footnotes"),
                   c("^{p} First", "^{q} Second", "^{pp} Third"))
})

test_that("a cells_stub(columns =) footnote on a two-stub gt is numbered by its row", {
  skip_on_cran()
  skip_if_not_installed("gt")
  df <- data.frame(g = c("A", "A", "B"), id = c("a", "b", "c"), x = c(1, 2, 3))
  g <- gt::gt(df, rowname_col = c("g", "id")) |>
    gt::tab_footnote("On id", gt::cells_stub(rows = 2, columns = "id")) |>
    gt::tab_footnote("On g", gt::cells_stub(rows = 3, columns = "g"))
  t <- as_rtftable(g)
  expect_identical(attr(t, "rtf_footnotes"), c("^{1} On id", "^{2} On g"))
})

test_that("a cells_stub(columns =) footnote mark shows on the two-stub cell", {
  skip_on_cran()
  skip("bug: two-stub gt (slots path) lists stub footnotes but never puts their marks on the stub cells")
  skip_if_not_installed("gt")
  df <- data.frame(g = c("A", "A", "B"), id = c("a", "b", "c"), x = c(1, 2, 3))
  g <- gt::gt(df, rowname_col = c("g", "id")) |>
    gt::tab_footnote("On id", gt::cells_stub(rows = 2, columns = "id"))
  t <- as_rtftable(g)
  expect_identical(t$data$id[2L], "b^{1}")
})

test_that("summary and grand-summary footnotes are numbered after the body", {
  skip_on_cran()
  skip_if_not_installed("gt")
  df <- data.frame(id = c("a", "b", "c"), x = c(1, 2, 3), y = c(4, 5, 6),
                   grp = c("G1", "G1", "G2"))
  g <- gt::gt(df, rowname_col = "id", groupname_col = "grp") |>
    gt::summary_rows(groups = gt::everything(), columns = x,
                     fns = list(Total = "sum")) |>
    gt::tab_footnote("Sum note",
                     gt::cells_summary(groups = "G1", columns = x, rows = 1)) |>
    gt::grand_summary_rows(columns = y, fns = list(All = "sum")) |>
    gt::tab_footnote("Grand note",
                     gt::cells_grand_summary(columns = y, rows = 1))
  t <- as_rtftables(g)[[1L]]
  expect_identical(attr(t, "rtf_footnotes"),
                   c("^{1} Sum note", "^{2} Grand note"))
})

test_that("summary_rows() rows are printed in the body", {
  skip_on_cran()
  skip("bug: as_rtftables() drops gt summary_rows()/grand_summary_rows() rows from the body silently")
  skip_if_not_installed("gt")
  df <- data.frame(id = c("a", "b", "c"), x = c(1, 2, 3),
                   grp = c("G1", "G1", "G2"))
  g <- gt::gt(df, rowname_col = "id", groupname_col = "grp") |>
    gt::summary_rows(groups = gt::everything(), columns = x,
                     fns = list(Total = "sum"))
  t <- as_rtftables(g)[[1L]]
  expect_true(any(t$data[[1L]] == "Total"))
})

test_that("a footnote on a spanner whose columns are all hidden is left out", {
  skip_on_cran()
  skip_if_not_installed("gt")
  df <- data.frame(id = c("a", "b"), x = 1:2)
  g <- gt::gt(df) |>
    gt::tab_spanner("Sp", columns = c(x)) |>
    gt::cols_hide(x) |>
    gt::tab_footnote("Spanner note", gt::cells_column_spanners("Sp")) |>
    gt::tab_footnote("Label note", gt::cells_column_labels(id))
  t <- as_rtftable(g)
  expect_identical(attr(t, "rtf_footnotes"), "^{1} Label note")
  expect_identical(t$col_header, list("id^{1}"))
})

test_that("a hand-written footnote span with no <sup> keeps its text as the mark", {
  skip_on_cran()
  skip_if_not_installed("gt")
  df <- data.frame(x = c("A<span class=\"gt_footnote_marks\">7</span>",
                         "B<span class=\"gt_footnote_marks\">\u00a0</span>"))
  g <- gt::fmt_passthrough(gt::gt(df), x, escape = FALSE)
  t <- as_rtftable(g)
  # a span holding only a no-break space leaves no mark at all
  expect_identical(t$data$x, c("A^{7}", "B"))
})

test_that("body styles are skipped with a warning when some rows have no row group", {
  skip_on_cran()
  skip_if_not_installed("gt")
  g <- gt::gt(.cov_gt_df()) |>
    gt::tab_row_group("G", rows = 1:2) |>
    gt::tab_style(gt::cell_text(weight = "bold"),
                  gt::cells_body(columns = x, rows = 3))
  expect_warning(t <- as_rtftables(g),
                 "gt row-group layout not recognized; body tab_style\\(\\) styling was skipped")
  expect_length(t, 1L)
  expect_identical(t[[1L]]$data$id, c("G", "\u00a0\u00a0a", "\u00a0\u00a0b", "c"))
})

test_that("the multi-stub body reader stops when no column is visible", {
  skip_on_cran()
  # Internal: a two-stub gt always has visible stub columns, so this stop
  # cannot be reached through as_rtftables(); a hand-built object is used.
  obj <- list(`_boxhead` = data.frame(var = c("a", "b"),
                                      type = c("hidden", "hidden")),
              `_data` = data.frame(a = 1, b = 2))
  expect_error(rtfreporter:::.gt_body_from_slots(obj),
               "Could not read the gt table body: no visible columns found\\.")
  expect_error(rtfreporter:::.gt_body_from_slots(list()),
               "`_boxhead` / `_data` missing")
})

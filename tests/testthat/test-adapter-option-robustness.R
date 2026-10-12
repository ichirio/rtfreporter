## tests/testthat/test-adapter-option-robustness.R
##
## Regression guard for rtfreporter's headline promise: reading ANY table
## object directly.  These tests run the three priority object types -- gt,
## gtsummary (-> gt), and rtables/tern (VTableTree) -- through as_rtftable()
## under a wide range of *option-triggered* layouts that have historically
## stressed the adapters (multi-stub gt, row groups, summary rows, merged
## columns, multi-level column splits, hierarchical / strata tables, ...), and
## assert each is read without error into a sane body.  Version-robust: they
## check structure (no error, column count, header stacking, titles), not
## framework-specific cell strings.  All skip when their package is absent.

library(testthat)

# Small synthetic analysis dataset (USUBJID so tern occurrence functions run).
.opt_df <- function() {
  set.seed(1L)
  n <- 40L
  data.frame(
    USUBJID = sprintf("S%03d", seq_len(n)),
    ARM  = factor(sample(c("A", "B"), n, TRUE), levels = c("A", "B")),
    SEX  = factor(sample(c("M", "F"), n, TRUE)),
    AGE  = round(stats::rnorm(n, 55, 10)),
    RESP = factor(sample(c("CR", "PD"), n, TRUE)),
    stringsAsFactors = FALSE
  )
}

# Build the table lazily, read it, and assert a sane rtftable body.
.expect_reads <- function(make, min_cols = 1L) {
  tbl <- suppressWarnings(suppressMessages(make()))
  rt  <- expect_no_error(as_rtftable(tbl, read_meta = TRUE))
  expect_s3_class(rt, "rtftable")
  expect_true(is.data.frame(rt$data))
  expect_gte(ncol(rt$data), min_cols)
  invisible(rt)
}


# ── gt option matrix ──────────────────────────────────────────────────────────

test_that("gt: a range of option-driven layouts read without error", {
  skip_if_not_installed("gt")
  df <- .opt_df()[1:9, ]
  cases <- list(
    groupname_col       = function() gt::gt(df, rowname_col = "SEX",
                                             groupname_col = "ARM"),
    row_group_as_column = function() gt::gt(df, rowname_col = "SEX",
                                            groupname_col = "ARM",
                                            row_group_as_column = TRUE),
    tab_row_group       = function() gt::tab_row_group(
                            gt::gt(df[, c("SEX", "AGE")]), label = "Grp",
                            rows = 1:3),
    grand_summary_rows  = function() gt::grand_summary_rows(
                            gt::gt(df[, "AGE", drop = FALSE]), columns = "AGE",
                            fns = list(Mean ~ mean(.))),
    cols_merge_range    = function() gt::cols_merge_range(
                            gt::gt(data.frame(a = 1:3, lo = 4:6, hi = 7:9)),
                            col_begin = lo, col_end = hi),
    nested_spanners     = function() gt::tab_spanner(
                            gt::tab_spanner(gt::gt(data.frame(a = 1, b1 = 2,
                              b2 = 3)), "S", c(b1, b2)), "TOP", c(a, b1, b2)),
    sub_missing         = function() gt::sub_missing(
                            gt::gt(data.frame(a = c(1, NA))), columns = a),
    zero_row            = function() gt::gt(data.frame(a = character(0),
                                                       b = numeric(0))),
    single_column       = function() gt::gt(data.frame(only = c("a", "b"))),
    markdown            = function() gt::fmt_markdown(
                            gt::gt(data.frame(a = c("**x**", "_y_"))), a)
  )
  for (nm in names(cases)) {
    info <- paste0("gt case: ", nm)
    expect_no_error(as_rtftable(suppressWarnings(cases[[nm]]()),
                                read_meta = TRUE))
  }
})

test_that("gt: nested spanners become a stacked multi-row col_header", {
  skip_if_not_installed("gt")
  g <- gt::gt(data.frame(a = 1, b1 = 2, b2 = 3))
  g <- gt::tab_spanner(g, "S",   c(b1, b2))
  g <- gt::tab_spanner(g, "TOP", c(a, b1, b2))
  kw <- rtfreporter:::.gt_to_rtftable_kwargs(g)
  expect_true(is.list(kw$col_header))          # stacked, not a flat vector
  expect_gte(length(kw$col_header), 2L)
})


# ── gtsummary option matrix ───────────────────────────────────────────────────

test_that("gtsummary: a range of tbl_* layouts read without error", {
  skip_on_cran()                  # one of the slowest blocks (~4 s); CI runs it
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("gt")
  df <- .opt_df()
  .expect_reads(function() gtsummary::tbl_summary(df[, c("ARM", "AGE", "SEX")],
                                                  by = ARM), min_cols = 2L)
  .expect_reads(function() gtsummary::add_overall(
    gtsummary::tbl_summary(df[, c("ARM", "AGE")], by = ARM)))
  .expect_reads(function() gtsummary::add_p(
    gtsummary::tbl_summary(df[, c("ARM", "AGE", "SEX")], by = ARM)))
  .expect_reads(function() gtsummary::tbl_continuous(df, variable = AGE,
                                                     by = ARM, include = SEX))
  # (tbl_regression is covered by test-gtsummary-adapter.R; its tidy backend is
  # an environment-fragile dependency we don't re-exercise here.)
  .expect_reads(function() gtsummary::tbl_stack(list(
    gtsummary::tbl_summary(df[, "AGE", drop = FALSE]),
    gtsummary::tbl_summary(df[, "SEX", drop = FALSE]))))
  .expect_reads(function() gtsummary::bold_labels(
    gtsummary::tbl_summary(df[, c("ARM", "AGE")], by = ARM)))
})

test_that("gtsummary: tbl_merge spanners survive as a stacked col_header", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("gt")
  df <- .opt_df()
  tm <- suppressMessages(gtsummary::tbl_merge(
    list(gtsummary::tbl_summary(df[, c("ARM", "AGE")], by = ARM),
         gtsummary::tbl_summary(df[, c("ARM", "SEX")], by = ARM)),
    tab_spanner = c("**G1**", "**G2**")))
  kw <- rtfreporter:::.gt_to_rtftable_kwargs(gtsummary::as_gt(tm))
  expect_true(is.list(kw$col_header))
  expect_gte(length(kw$col_header), 2L)
})

test_that("gtsummary: hierarchical (AE-shape) tables read without error", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("cards")
  skip_if_not_installed("gt")
  df <- .opt_df()
  .expect_reads(function() gtsummary::tbl_hierarchical(
    df, variables = c(RESP), by = ARM, denominator = df, id = USUBJID,
    overall_row = TRUE), min_cols = 2L)
})


# ── rtables / tern option matrix ──────────────────────────────────────────────

test_that("rtables/tern: a range of layouts read without error", {
  skip_if_not_installed("rtables")
  skip_if_not_installed("formatters")
  df <- .opt_df()
  bt <- rtables::basic_table
  .expect_reads(function()
    rtables::build_table(rtables::analyze(rtables::split_cols_by(bt(), "ARM"),
                         "AGE", afun = mean), df), min_cols = 2L)
  .expect_reads(function()                                   # 2-level col split
    rtables::build_table(rtables::analyze(rtables::split_cols_by(
      rtables::split_cols_by(bt(), "ARM"), "SEX"), "AGE", afun = mean), df),
    min_cols = 3L)
  .expect_reads(function()                                   # col counts
    rtables::build_table(rtables::analyze(rtables::split_cols_by(
      bt(show_colcounts = TRUE), "ARM"), "AGE", afun = mean), df))
  .expect_reads(function()                                   # nested row split
    rtables::build_table(rtables::analyze(rtables::split_rows_by(
      rtables::split_cols_by(bt(), "ARM"), "SEX"), "AGE", afun = mean), df))
  .expect_reads(function()                                   # content rows
    rtables::build_table(rtables::analyze(rtables::summarize_row_groups(
      rtables::split_rows_by(rtables::split_cols_by(bt(), "ARM"), "SEX")),
      "AGE", afun = mean), df))
})

test_that("rtables/tern: tern analysis functions read without error", {
  skip_if_not_installed("tern")
  skip_if_not_installed("rtables")
  df <- .opt_df()
  bt <- rtables::basic_table
  .expect_reads(function()
    rtables::build_table(tern::analyze_vars(rtables::split_cols_by(bt(), "ARM"),
                         "AGE"), df), min_cols = 2L)
  .expect_reads(function()
    rtables::build_table(tern::count_occurrences(
      rtables::split_cols_by(bt(), "ARM"), vars = "RESP"), df), min_cols = 2L)
})

test_that("rtables: multi-level column split stacks the col_header", {
  skip_if_not_installed("rtables")
  skip_if_not_installed("formatters")
  df <- .opt_df()
  t2 <- rtables::build_table(
    rtables::analyze(rtables::split_cols_by(
      rtables::split_cols_by(rtables::basic_table(), "ARM"), "SEX"),
      "AGE", afun = mean), df)
  kw <- rtfreporter:::.rtables_to_rtftable_kwargs(t2)
  expect_true(is.list(kw$col_header))
  expect_gte(length(kw$col_header), 2L)
})

test_that("rtables: titles and footers are read into page blocks", {
  skip_if_not_installed("rtables")
  skip_if_not_installed("formatters")
  df <- .opt_df()
  t <- rtables::build_table(
    rtables::analyze(rtables::split_cols_by(rtables::basic_table(), "ARM"),
                     "AGE", afun = mean), df)
  rtables::main_title(t)  <- "My Title"
  rtables::main_footer(t) <- "My Footer"
  kw <- rtfreporter:::.rtables_to_rtftable_kwargs(t)
  expect_true("My Title" %in% kw$titles_block)
  expect_true("My Footer" %in% kw$footnotes_block)
})

test_that("rtables: a table with no data rows reads to a zero-row body", {
  skip_if_not_installed("rtables")
  skip_if_not_installed("formatters")
  df <- .opt_df()
  # label rows only (split with no analyze) -> structural rows, no data rows
  t <- rtables::build_table(
    rtables::split_rows_by(rtables::split_cols_by(rtables::basic_table(),
                                                  "ARM"), "SEX"), df)
  rt <- expect_no_error(as_rtftable(t, read_meta = TRUE))
  expect_equal(nrow(rt$data), 0L)
})

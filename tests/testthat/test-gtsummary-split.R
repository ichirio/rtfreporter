## tests/testthat/test-gtsummary-split.R
##
## gtsummary tbl_split support (#215): the container returned by
## tbl_split_by_rows() / tbl_split_by_columns() is unwrapped explicitly into
## its member gtsummary tables (previously this only worked by accident via
## the generic list branch).  All tests skip when gtsummary is absent.

library(testthat)

# A two-member tbl_split from the bundled trial data.  Skips (via the caller's
# skip_if_not_installed) when gtsummary lacks tbl_split_by_rows() (< 2.1).
.make_tbl_split <- function() {
  skip_if_not(
    "tbl_split_by_rows" %in% getNamespaceExports("gtsummary"),
    "gtsummary has no tbl_split_by_rows()"
  )
  gtsummary::tbl_summary(gtsummary::trial,
                         by = trt, include = c(age, grade)) |>
    gtsummary::tbl_split_by_rows(variables = age)
}

# ── detection & expansion ─────────────────────────────────────────────────────

test_that(".is_gtsummary_split() detects tbl_split containers only", {
  skip_if_not_installed("gtsummary")
  ts <- .make_tbl_split()
  expect_true(rtfreporter:::.is_gtsummary_split(ts))
  expect_false(rtfreporter:::.is_gtsummary_split(ts[[1]]))
  expect_false(rtfreporter:::.is_gtsummary_split(list(a = 1)))
})

test_that(".gtsummary_split_tables() returns the plain member list", {
  skip_if_not_installed("gtsummary")
  ts   <- .make_tbl_split()
  tbls <- rtfreporter:::.gtsummary_split_tables(ts)
  expect_identical(class(tbls), "list")
  expect_length(tbls, 2L)
  expect_true(all(vapply(tbls, inherits, logical(1), "gtsummary")))
})

# ── as_rtftables() ────────────────────────────────────────────────────────────

test_that("as_rtftables() expands a tbl_split into one page per member", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("gt")
  ts    <- .make_tbl_split()
  pages <- as_rtftables(ts)
  expect_length(pages, 2L)
  expect_true(all(vapply(pages, inherits, logical(1), "rtftable")))
  # Same bodies as converting the members directly.
  direct <- as_rtftables(unclass(ts))
  expect_identical(lapply(pages,  `[[`, "data"),
                   lapply(direct, `[[`, "data"))
})

# ── as_rtftable() ─────────────────────────────────────────────────────────────

test_that("as_rtftable() unwraps a single-member tbl_split", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("gt")
  ts     <- .make_tbl_split()
  single <- structure(list(ts[[1]]), class = class(ts))
  tbl    <- as_rtftable(single)
  expect_s3_class(tbl, "rtftable")
})

test_that("as_rtftable() rejects a multi-member tbl_split informatively", {
  skip_if_not_installed("gtsummary")
  ts <- .make_tbl_split()
  expect_error(as_rtftable(ts), "holding 2 tables.*as_rtftables")
})

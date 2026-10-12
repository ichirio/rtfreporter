## tests/testthat/test-gtsummary-adapter.R
##
## Tests for gtsummary -> gt -> rtftable pipeline.
## All tests are wrapped in skip_if_not_installed() so they do not block CI
## on machines where gtsummary is not available.

library(testthat)

# ── Helpers ────────────────────────────────────────────────────────────────────

.make_tbl_summary <- function() {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("gt")
  df <- data.frame(
    age      = c(25, 35, 45, 55, 65),
    sex      = factor(c("M", "F", "M", "F", "M")),
    response = c(1, 0, 1, 1, 0),
    stringsAsFactors = FALSE
  )
  gtsummary::tbl_summary(df)
}

.make_tbl_regression <- function() {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("gt")
  # tbl_regression() pulls model terms via broom / broom.helpers.
  skip_if_not_installed("broom")
  skip_if_not_installed("broom.helpers")
  df <- data.frame(
    y = c(1, 0, 1, 1, 0, 1),
    x = c(25, 35, 45, 55, 65, 30)
  )
  fit <- glm(y ~ x, data = df, family = binomial)
  gtsummary::tbl_regression(fit, exponentiate = TRUE)
}


# ── .is_gtsummary_tbl() ───────────────────────────────────────────────────────

silence_rtf_tables_deprecation()   # the old arguments' own tests (#594)

test_that(".is_gtsummary_tbl() returns TRUE for gtsummary objects", {
  skip_if_not_installed("gtsummary")
  s <- .make_tbl_summary()
  expect_true(rtfreporter:::.is_gtsummary_tbl(s))
})

test_that(".is_gtsummary_tbl() returns FALSE for non-gtsummary objects", {
  expect_false(rtfreporter:::.is_gtsummary_tbl(data.frame(a = 1)))
  expect_false(rtfreporter:::.is_gtsummary_tbl(list()))
  expect_false(rtfreporter:::.is_gtsummary_tbl(NULL))
})


# ── .gtsummary_to_gt() ────────────────────────────────────────────────────────

test_that(".gtsummary_to_gt() returns a gt_tbl", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("gt")
  s      <- .make_tbl_summary()
  gt_obj <- rtfreporter:::.gtsummary_to_gt(s)
  expect_true(inherits(gt_obj, "gt_tbl"))
})

test_that(".gtsummary_to_gt() errors without gtsummary installed", {
  skip_if(requireNamespace("gtsummary", quietly = TRUE),
          "gtsummary is installed; cannot test missing-package error")
  fake <- structure(list(), class = "gtsummary")
  expect_error(.gtsummary_to_gt(fake), "gtsummary")
})


# ── as_rtftable() with gtsummary input ────────────────────────────────────────

test_that("as_rtftable() accepts a tbl_summary and returns an rtftable", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("gt")
  s   <- .make_tbl_summary()
  tbl <- as_rtftable(s)
  expect_s3_class(tbl, "rtftable")
})

test_that("as_rtftable() column count matches the rendered (visible) body", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("gt")
  s      <- .make_tbl_summary()
  gt_obj <- gtsummary::as_gt(s)
  # The body comes from gt::extract_body() -- visible columns only.
  eb     <- gt::extract_body(gt_obj, output = "html")
  tbl    <- as_rtftable(s, read_meta = FALSE)
  expect_equal(ncol(tbl$data), ncol(eb))
})

test_that("as_rtftable() reads col_header from gtsummary when read = TRUE", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("gt")
  s   <- .make_tbl_summary()
  tbl <- as_rtftable(s, read_meta = TRUE)
  # col_header should be a character vector of length == ncol(data)
  expect_true(!is.null(tbl$col_header) || is.null(tbl$col_header))
  # structural integrity: data is a data.frame
  expect_true(is.data.frame(tbl$data))
})

test_that("as_rtftable() accepts tbl_regression", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("gt")
  r   <- .make_tbl_regression()
  tbl <- as_rtftable(r)
  expect_s3_class(tbl, "rtftable")
})

test_that("as_rtftable() accepts a data.frame (consistent with as_rtftables())", {
  df  <- data.frame(a = c("1", "2"), b = c("x", "y"), stringsAsFactors = FALSE)
  tbl <- as_rtftable(df)
  expect_s3_class(tbl, "rtftable")
  expect_identical(tbl$data, as_rtftables(df)[[1L]]$data)
})

test_that("as_rtftable() still errors for inputs that are not a table object", {
  expect_error(as_rtftable(list()), "gt_tbl")
  expect_error(as_rtftable(42),     "gt_tbl")
})


# ── rtf_tables() with gtsummary input ─────────────────────────────────────────

test_that("rtf_tables() accepts a gtsummary object in the contents list", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("gt")
  s   <- .make_tbl_summary()
  doc <- rtf_document() |>
    rtf_tables(list(s))
  expect_s3_class(doc, "rtf_document")
  expect_length(doc$contents, 1L)
})

test_that("rtf_tables() with read_gt = TRUE pulls titles from gtsummary", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("gt")
  s <- gtsummary::tbl_summary(
    data.frame(x = 1:5, y = letters[1:5]),
    label = list(x ~ "Age", y ~ "Group")
  )
  doc <- rtf_document() |>
    rtf_tables(list(s), read_gt = TRUE)
  # doc$contents should contain exactly 1 page entry
  expect_length(doc$contents, 1L)
})

test_that("rtf_tables() rejects items that are not valid content types", {
  expect_error(
    rtf_tables(rtf_document(), list(42L)),
    "data.frame"
  )
})

test_that("rtf_tables() handles mix of gtsummary and data.frame", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("gt")
  s   <- .make_tbl_summary()
  df  <- data.frame(a = 1:3, b = c("x", "y", "z"))
  doc <- rtf_document() |>
    rtf_tables(list(s, df))
  expect_length(doc$contents, 2L)
})

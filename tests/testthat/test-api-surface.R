## tests/testthat/test-api-surface.R
##
## #346: the API-review measure. Deprecated functions still work, so they still
## appear in NAMESPACE, but they are not part of what a reader has to learn --
## they are scheduled for bulk removal before the CRAN submission. The number
## that matters is therefore the export count MINUS the deprecated set, and
## these tests keep both honest.

library(testthat)

.dep <- rtfreporter:::.deprecated_exports

.exports <- function() {
  ns <- asNamespace("rtfreporter")
  sort(getNamespaceExports(ns))
}

.reset_deprecation <- function() {
  st <- rtfreporter:::.deprecation_state
  rm(list = ls(st), envir = st)
}

test_that("every name on the deprecated list is actually exported", {
  expect_true(all(.dep %in% .exports()),
              info = paste(setdiff(.dep, .exports()), collapse = ", "))
})

test_that("every deprecated function still works and warns once", {
  b <- rtf_border(top = TRUE)
  calls <- list(
    rtf_border_top    = function() rtf_border_top(),
    rtf_border_bottom = function() rtf_border_bottom(),
    rtf_border_box    = function() rtf_border_box(),
    rtf_border_none   = function() rtf_border_none(),
    rtf_border_with   = function() rtf_border_with(b, bottom = TRUE),
    rtf_border_tfl    = function() rtf_border_tfl(),
    rtf_table_border  = function() rtf_table_border(header = b)
  )
  expect_setequal(names(calls), .dep)

  for (nm in names(calls)) {
    .reset_deprecation()
    expect_warning(value <- calls[[nm]](), "deprecated", info = nm)
    expect_false(is.null(value), info = nm)          # still does its job
    expect_silent(calls[[nm]]())                     # ... and only warns once
  }
  .reset_deprecation()
})

test_that("the deprecated spellings still produce the new values", {
  suppressWarnings({
    expect_identical(rtf_border_none(),  rtf_border())
    expect_identical(rtf_border_top(),   rtf_border(top = TRUE))
    expect_identical(rtf_border_bottom(), rtf_border(bottom = TRUE))
    expect_identical(rtf_border_box(),   rtf_border(all = TRUE))
    b <- rtf_border(top = TRUE)
    expect_identical(rtf_border_with(b, bottom = TRUE),
                     rtf_border(top = TRUE, bottom = TRUE))
  })
})

test_that("one border constructor is left once the deprecated ones are set aside", {
  border_api <- grep("^rtf_border|^rtf_table_border$", .exports(), value = TRUE)
  expect_length(border_api, 9L)                       # what NAMESPACE still holds
  # Two, doing different jobs: which edges, and what the line is.
  expect_setequal(setdiff(border_api, .dep),
                  c("rtf_border", "rtf_border_side"))
})

test_that("the effective export count is the reviewed number", {
  # 81 exports, 7 of them deprecated and slated for removal before CRAN.
  expect_length(.exports(), 81L)
  expect_length(setdiff(.exports(), .dep), 74L)
})

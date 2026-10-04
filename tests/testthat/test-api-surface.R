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
  # paginate() keeps its own once-a-session flag
  rm(list = ls(rtfreporter:::.paginate_depr_env),
     envir = rtfreporter:::.paginate_depr_env)
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
    rtf_table_border  = function() rtf_table_border(header = b),
    # 0.8.2.9013, the pre-CRAN API review (iteration 1)
    rtf_border_side   = function() rtf_border_side(),
    add_col_header_row = function()
      add_col_header_row(rtf_col_header(c("a", "b")), c("A", "B")),
    col_header_from_names = function() col_header_from_names(c("a", "x__b")),
    set_header_cell   = function()
      set_header_cell(rtftable(data.frame(a = 1, b = 2), col_header = c("A", "B")),
                      col_cell(c(1, 2), "AB"), row = 1),
    update_header_row = function()
      update_header_row(rtf_header(c(l = "x")), 2, c(l = "y")),
    update_footer_row = function()
      update_footer_row(rtf_footer(c(l = "x")), 2, c(l = "y")),
    paginate          = function() paginate(data.frame(a = 1))
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
  expect_length(border_api, 10L)                      # what NAMESPACE still holds
  # Two, doing different jobs: which edges, and what the line is.
  expect_setequal(setdiff(border_api, .dep),
                  c("rtf_border", "rtf_border_line"))
})

test_that("the effective export count is the reviewed number", {
  # 125 exports, 14 of them deprecated and slated for removal before CRAN.
  # (91 since #463 added rtfreporter_ai_manual(); 92 since #476 added
  # round_num(); 126 since #491 brought the ARD table engine --
  # 34 functions -- over from tflspec; 122 since #498 folded plan_fmt()
  # into plan_digits() and the three style verbs into plan_cell_style();
  # 123 since #529 added rtf_text_tokens(); 124 since #536 added
  # plan_header_tokens(); 125 since the pre-CRAN API review, iteration 1,
  # renamed rtf_border_side() to rtf_border_line() and deprecated the old
  # name with six others.)
  expect_length(.exports(), 125L)
  expect_length(setdiff(.exports(), .dep), 111L)
})

test_that("the old argument names still work, with a warning", {
  .reset_deprecation()
  d <- data.frame(a = 1, b = 2)
  expect_warning(t1 <- as_rtftable(gt_obj = d), "deprecated")
  expect_identical(t1, as_rtftable(d))
  .reset_deprecation()
  sp <- list(list(from = 1, to = 2, label = "AB"))
  expect_warning(t2 <- rtftable(d, spanning_header = sp), "deprecated")
  expect_identical(t2$spanning_header, sp)
  expect_silent(rtftable(d, spanning_header = sp))     # once a session
  .reset_deprecation()
})

test_that("a border line saved under the old class name still reads", {
  old <- structure(list(style = "single", width = 15L, color = NULL),
                   class = "rtf_border_side")
  expect_silent(rtf_border(top = old))
})

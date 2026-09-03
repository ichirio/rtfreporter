## tests/testthat/test-cont-label-factor.R
##
## #352 -- the group-aware splits write the " (Cont.)" label back into the group
## column.  A factor cannot hold a value outside its levels, so a data.frame
## silently stored NA (with a warning) and a tibble errored.  The group column
## is now coerced to character just before the split -- after sort_by, which
## must still order by the factor's LEVELS.

library(testthat)

.fd <- function(levels = c("A", "B")) {
  data.frame(
    grp = factor(rep(c("A", "B"), each = 5), levels = levels),
    lbl = paste0("row", 1:10),
    n   = as.character(1:10),
    stringsAsFactors = FALSE
  )
}
.col1 <- function(p, i) as.character(p[[i]]$data[[1L]])

test_that("a factor group_col carries the (Cont.) label under group_force", {
  p <- expect_silent(
    as_rtftables(.fd(), split = "group_force", max_rows = 4,
                 group_col = 1, group_by = "value"))
  # Every continuation page opens with the repeated label, not an NA cell.
  conts <- unlist(lapply(seq_along(p), function(i) .col1(p, i)[1L]))
  expect_true(any(grepl(" (Cont.)", conts, fixed = TRUE)))
  expect_false(anyNA(unlist(lapply(seq_along(p), function(i) .col1(p, i)))))
  expect_identical(.col1(p, 2L)[1L], "A (Cont.)")
})

test_that("group_safe reaches the same path when one group overflows", {
  # group_safe force-splits any single group larger than max_rows, so it hit
  # the same assignment -- just later than group_force did.
  p <- expect_silent(
    as_rtftables(.fd(), split = "group_safe", max_rows = 4,
                 group_col = 1, group_by = "value"))
  expect_identical(.col1(p, 2L)[1L], "A (Cont.)")
})

test_that("a tibble with a factor group_col no longer errors", {
  skip_if_not_installed("tibble")
  p <- expect_silent(
    as_rtftables(tibble::as_tibble(.fd()), split = "group_force", max_rows = 4,
                 group_col = 1, group_by = "value"))
  expect_identical(.col1(p, 2L)[1L], "A (Cont.)")
})

test_that("sort_by still orders a factor group_col by its levels", {
  # The coercion must happen AFTER the sort: on levels c("B", "A") the B rows
  # come first, which is not the alphabetical order a character column gives.
  p <- as_rtftables(.fd(levels = c("B", "A")), split = "group_force",
                    max_rows = 4, group_col = 1, group_by = "value",
                    sort_by = 1)
  expect_identical(.col1(p, 1L)[1L], "B")
  expect_identical(.col1(p, 2L)[1L], "B (Cont.)")
  expect_identical(.col1(p, 3L)[1L], "A")
})

test_that("add_cont_label() accepts a factor label column", {
  chunk <- .fd()[1:2, , drop = FALSE]
  out   <- expect_silent(add_cont_label(chunk, label = "A"))
  expect_identical(as.character(out[[1L]])[1L], "A (Cont.)")
  expect_identical(nrow(out), 3L)
  # The whole column is coerced, so the rbind() cannot leave a mixed column.
  expect_type(out[[1L]], "character")
})

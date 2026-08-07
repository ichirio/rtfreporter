## tests/testthat/test-collapse-repeats-fn.R
##
## collapse_repeats() -- the standalone data.frame helper (whole-frame version
## of the per-page as_rtftables(collapse_repeats=) argument, tested separately
## in test-collapse-repeats.R).

library(testthat)

df3 <- function() {
  data.frame(
    grp = c("A", "A", "A", "B", "B"),
    sub = c("x", "x", "y", "x", "x"),
    n   = 1:5,
    stringsAsFactors = FALSE
  )
}

test_that("a single column blanks consecutive repeats, keeping the first", {
  out <- collapse_repeats(df3(), cols = "grp")
  expect_identical(out$grp, c("A", NA, NA, "B", NA))
  expect_identical(out$sub, df3()$sub)     # untouched
  expect_identical(out$n,   df3()$n)
})

test_that("suppression is hierarchical (a change in an earlier column resets)", {
  out <- collapse_repeats(df3(), cols = c("grp", "sub"))
  expect_identical(out$grp, c("A", NA, NA, "B", NA))
  # sub "y" (row 3) shows because it changed within grp A; row 4 "x" shows
  # because grp reset to B; row 5 repeats -> NA.
  expect_identical(out$sub, c("x", NA, "y", "x", NA))
})

test_that("integer positions and a mixing list both work", {
  out1 <- collapse_repeats(df3(), cols = 1L)
  expect_identical(out1$grp, c("A", NA, NA, "B", NA))
  out2 <- collapse_repeats(df3(), cols = list("grp", 2L))
  expect_identical(out2$sub, c("x", NA, "y", "x", NA))
})

test_that("no rows are removed and other columns are preserved", {
  out <- collapse_repeats(df3(), cols = c("grp", "sub"))
  expect_identical(nrow(out), 5L)
  expect_identical(out$n, 1:5)
})

test_that("errors on non-data.frame and unknown column", {
  expect_error(collapse_repeats(1:5, cols = 1), "must be a data.frame")
  expect_error(collapse_repeats(df3(), cols = "nope"), "not found")
})

test_that("matches the as_rtftables() argument on a single (unpaginated) page", {
  via_arg <- as_rtftables(df3(), collapse_repeats = c("grp", "sub"))[[1L]]
  via_fun <- collapse_repeats(df3(), cols = c("grp", "sub"))
  expect_identical(as.character(via_arg$data$grp), as.character(via_fun$grp))
  expect_identical(as.character(via_arg$data$sub), as.character(via_fun$sub))
})

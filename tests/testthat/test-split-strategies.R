## tests/testthat/test-split-strategies.R
##
## #334 -- the five page_split_*() factories were retired.  A strategy is named
## and its settings are ordinary arguments alongside it, so every setting has
## exactly one declaration site.  These re-assert, through the string form,
## what test-page-split-factories.R used to cover.

library(testthat)

.sd <- function() {
  data.frame(
    G = c(rep("A", 3), rep("B", 3), rep("C", 3)),
    N = as.character(1:9),
    stringsAsFactors = FALSE
  )
}
.lay <- function(p) vapply(p, function(x) paste(x$data$G, collapse = ""), "")

test_that("the factories are gone", {
  for (f in c("page_split_none", "page_split_rows", "page_split_group_safe",
              "page_split_group_force", "page_split_by_value")) {
    expect_false(f %in% getNamespaceExports("rtfreporter"), info = f)
    expect_false(exists(f, envir = asNamespace("rtfreporter"), inherits = FALSE),
                 info = f)
  }
  # ... and so is the machinery that reconciled a spec's group_col with the
  # top-level one (#328); with one declaration site there is nothing to
  # reconcile.
  expect_false(exists(".split_spec_tag", envir = asNamespace("rtfreporter"),
                      inherits = FALSE))
})

test_that("every strategy is reachable by name", {
  d <- .sd()
  expect_length(as_rtftables(d, split = "none"), 1L)
  # split_rows means break BEFORE those rows, so 4 and 7 cut on the group
  # boundaries of a 3-3-3 table.
  expect_identical(.lay(as_rtftables(d, split = "rows", split_rows = c(4, 7))),
                   c("AAA", "BBB", "CCC"))
  expect_identical(.lay(as_rtftables(d, split = "group_safe", max_rows = 4,
                                     group_col = "G")),
                   c("AAA", "BBB", "CCC"))
  expect_identical(unname(.lay(as_rtftables(d, split = "by_value",
                                            group_col = "G"))),
                   c("AAA", "BBB", "CCC"))
  gf <- as_rtftables(d, split = "group_force", max_rows = 4, group_col = "G")
  expect_gt(length(gf), 1L)
})

test_that("by_value names its pages from group_col", {
  expect_identical(names(as_rtftables(.sd(), split = "by_value",
                                      group_col = "G")),
                   c("A", "B", "C"))
})

test_that("the strategies that need max_rows still say so", {
  d <- .sd()
  expect_error(as_rtftables(d, split = "group_safe", group_col = "G"),
               "max_rows")
  expect_error(as_rtftables(d, split = "group_force", group_col = "G"),
               "max_rows")
  expect_error(as_rtftables(d, split = "rows"), "split_rows")
})

test_that("group_col reaches BOTH pagination and the blank rows", {
  # The single-declaration-site property, asserted directly: there is now no
  # second place to put it, and the one place feeds everything.
  d <- data.frame(PT = c("p1", "p2", "p3", "p4", "p5", "p6"),
                  G  = c("A", "A", "A", "B", "B", "B"),
                  stringsAsFactors = FALSE)
  p <- as_rtftables(d, split = "group_safe", max_rows = 99, group_col = "G",
                    blank_rows = "between_groups")
  expect_length(p, 1L)
  expect_identical(p[[1L]]$blank_rows, 3L)     # SOC changes after row 3
})

test_that("a custom split function is still accepted", {
  # The factories were pre-built instances of this contract; the contract
  # itself is unchanged.
  halves <- function(df, ...) {
    k <- ceiling(nrow(df) / 2)
    list(df[seq_len(k), , drop = FALSE],
         df[seq.int(k + 1L, nrow(df)), , drop = FALSE])
  }
  p <- as_rtftables(.sd(), split = halves)
  expect_length(p, 2L)
  expect_identical(attr(p[[1L]], "rtf_paginate_meta")$strategy %||% "custom",
                   "custom")
})

test_that("a custom split must return a non-empty list of data.frames", {
  expect_error(as_rtftables(.sd(), split = function(df, ...) "nope"),
               "non-empty list")
})

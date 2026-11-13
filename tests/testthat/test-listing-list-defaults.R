## tests/testthat/test-listing-list-defaults.R
##
## A list of data frames with `listing =` (#1052): each element gets the
## defaults a single data frame gets on the `listing =` route (FR-IX318) --
## group_col on the record column, group_by = "value", a "group_safe" split
## when max_rows is set, the record column dropped, blank_row_first,
## col_header, col_rel_width and collapse_repeats -- and the caller's
## arguments still win.

library(testthat)

# Fictional data: two listings, one record per source row.
.t78_d1 <- function() {
  data.frame(S = rep(c("S1", "S2", "S3"), each = 2L),
             A = sprintf("AV%d", 1:6),
             B = sprintf("P%02d", 1:6), stringsAsFactors = FALSE)
}
.t78_d2 <- function() {
  data.frame(S = rep(c("T1", "T2"), c(3L, 2L)),
             A = sprintf("BV%d", 1:5),
             B = sprintf("Q%02d", 1:5), stringsAsFactors = FALSE)
}
.t78_spec <- function(...) {
  listing_spec(list(listing_col("S", width = 6), listing_col("A", width = 6),
                    listing_col("B", width = 3)), ...)
}

# The written RTF, as one string.
.t78_rtf <- function(pages) {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(rtf_tables(rtf_document(), pages), f)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}
.t78_rows <- function(pages) {
  unname(vapply(pages, function(p) nrow(p$data), 1L))
}
.t78_meta <- function(p) attr(p$data, "rtf_paginate_meta", exact = TRUE)
.t78_bodies <- function(pages) unname(lapply(pages, function(p) p$data))

# The list's pages against each data frame's own, with the same arguments.
.t78_same_as_alone <- function(spec, ...) {
  lst   <- as_rtftables(list(.t78_d1(), .t78_d2()), listing = spec, ...)
  alone <- c(as_rtftables(.t78_d1(), listing = spec, ...),
             as_rtftables(.t78_d2(), listing = spec, ...))
  expect_identical(.t78_bodies(lst), .t78_bodies(alone))
  expect_identical(.t78_rtf(lst), .t78_rtf(alone))
  lst
}

test_that("FT-TB7801 [FR-IX318]: a list of data frames with listing = and max_rows pages each element as that data frame alone, the record column dropped and a record kept whole", {
  lst <- .t78_same_as_alone(.t78_spec(), max_rows = 5)
  # a record is its row and a blank row: 2 records to a page, never cut at 5
  expect_identical(.t78_rows(lst), c(4L, 4L, 4L, 4L, 4L, 2L))
  for (p in lst) {
    expect_identical(.t78_meta(p)$strategy, "group_safe")
    expect_identical(.t78_meta(p)$group_col, ".rtf_record")
    expect_identical(names(p$data), c("S", ".sp1", "A", ".sp2", "B"))
  }
  # each record's A and B print on one page of the written RTF
  txt <- strsplit(.t78_rtf(lst), "\\\\page(?![a-z])", perl = TRUE)[[1L]]
  page_of <- function(v) which(vapply(txt, grepl, NA, pattern = v,
                                      fixed = TRUE))
  a <- c(sprintf("AV%d", 1:6), sprintf("BV%d", 1:5))
  b <- c(sprintf("P%02d", 1:6), sprintf("Q%02d", 1:5))
  for (k in seq_along(a)) {
    expect_length(page_of(a[k]), 1L)
    expect_identical(page_of(a[k]), page_of(b[k]))
  }
  # the spec's blank_row_first reaches each element too
  first <- .t78_same_as_alone(.t78_spec(blank_row_first = TRUE), max_rows = 5)
  for (p in first) {
    expect_identical(attr(p$data, "rtf_blank_rows", exact = TRUE), 0L)
  }
})

test_that("FT-TB7802 [FR-IX318]: in a list of data frames the caller's split, group_col, group_by and drop_cols still win over the listing's defaults", {
  # split = "none": one page per element, max_rows or not
  none <- .t78_same_as_alone(.t78_spec(), max_rows = 5, split = "none")
  expect_identical(.t78_rows(none), c(12L, 10L))
  expect_identical(.t78_meta(none[[1L]])$strategy, "none")
  # split = "rows": each element cut where split_rows says, not by max_rows
  byrow <- .t78_same_as_alone(.t78_spec(), max_rows = 5, split = "rows",
                              split_rows = 3L)
  expect_identical(.t78_rows(byrow), c(2L, 10L, 2L, 8L))
  expect_identical(.t78_meta(byrow[[1L]])$strategy, "rows")
  # group_col / group_by / drop_cols the caller passes
  mine <- .t78_same_as_alone(.t78_spec(), max_rows = 5, group_col = "S",
                             group_by = "indent", drop_cols = "A",
                             blank_row_first = FALSE)
  expect_identical(.t78_meta(mine[[1L]])$group_col, "S")
  expect_identical(names(mine[[1L]]$data), c("S", ".sp1", ".sp2", "B"))
  expect_identical(.t78_rows(mine), c(4L, 4L, 4L, 4L, 4L, 2L))
})

test_that("FT-TB7803 [FR-IX318]: a list of data frames with listing = and no max_rows gives one page per element", {
  lst <- .t78_same_as_alone(.t78_spec())
  expect_identical(.t78_rows(lst), c(12L, 10L))
  for (p in lst) {
    expect_identical(.t78_meta(p)$strategy, "none")
    expect_false(".rtf_record" %in% names(p$data))
  }
})

test_that("FT-TB7804 [FR-IX318]: every listing = default and every override, at once, on each data frame of a list", {
  spec <- listing_spec(list(
    listing_col("S", width = 6, label = "Subject", collapse_repeats = TRUE),
    listing_col("A", width = 6, label = "Term"),
    listing_col("B", width = 3, label = "Cd")), blank_row_first = TRUE)
  # the defaults, each element as it is alone
  own <- .t78_same_as_alone(spec, max_rows = 5)
  for (p in own) {
    expect_identical(.t78_meta(p)$strategy, "group_safe")            # split
    expect_identical(.t78_meta(p)$group_col, ".rtf_record")          # group_col
    expect_false(".rtf_record" %in% names(p$data))                   # drop_cols
    expect_identical(attr(p$data, "rtf_blank_rows", exact = TRUE), 0L) # blank_row_first
    expect_identical(p$col_header[[1L]][c(3L, 5L)], c("Term", "Cd")) # col_header
    expect_identical(p$col_rel_width, c(6, 1, 6, 1, 3))              # col_rel_width
  }
  # collapse_repeats: a subject's second record leaves its key blank
  expect_identical(own[[1L]]$data$S, c("S1", NA, NA, NA))
  # group_by = "value" on a group column the caller names: every change of S
  # starts a group, so the pages fill to max_rows; "indent" keeps a record
  # and its blank row together
  plain <- .t78_spec()
  value <- .t78_same_as_alone(plain, max_rows = 5, group_col = "S")
  expect_identical(.t78_rows(value), c(5L, 5L, 2L, 5L, 5L))
  indent <- .t78_same_as_alone(plain, max_rows = 5, group_col = "S",
                               group_by = "indent")
  expect_identical(.t78_rows(indent), c(4L, 4L, 4L, 4L, 4L, 2L))
  # every override the caller passes, each element as it is alone
  mine <- .t78_same_as_alone(spec, max_rows = 5, split = "none",
                             drop_cols = "A", blank_row_first = FALSE,
                             collapse_repeats = "B")
  expect_length(mine, 2L)
  for (p in mine) {
    expect_identical(.t78_meta(p)$strategy, "none")
    expect_identical(names(p$data), c("S", ".sp1", ".sp2", "B"))
    expect_null(attr(p$data, "rtf_blank_rows", exact = TRUE))
  }
  # collapse_repeats = "B": the subject is no longer blanked
  expect_false(anyNA(mine[[1L]]$data$S[c(1L, 3L)]))
  hdr <-.t78_same_as_alone(spec, col_header = list(c("X", "Y", "Z", "V", "W")),
                            col_rel_width = c(5, 1, 5, 1, 5))
  for (p in hdr) {
    expect_identical(p$col_header, list(c("X", "Y", "Z", "V", "W")))
    expect_identical(p$col_rel_width, c(5, 1, 5, 1, 5))
  }
  # group_col = "S" the caller passes
  expect_identical(.t78_meta(.t78_same_as_alone(spec, max_rows = 5,
                                                group_col = "S")[[1L]])$group_col,
                   "S")
})

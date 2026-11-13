## tests/testthat/test-rlistings-paging.R
##
## An rlistings listing paged with max_rows (#1045): `max_rows` alone gives
## a "group_safe" split on the listing's first key column, so a subject's
## rows stay together when they fit.  A split, group_col or group_by the
## caller passes wins, and without max_rows the listing is one page.
## All tests skip when rlistings / formatters are not installed.

library(testthat)

# Fictional data: three subjects of 4, 2 and 3 rows.
.t77_dat <- function() {
  data.frame(
    USUBJID = rep(c("S-001", "S-002", "S-003"), c(4L, 2L, 3L)),
    AESEV   = c("MILD", "MILD", "SEVERE", "SEVERE", "MILD", "MODERATE",
                "MILD", "MILD", "MILD"),
    AEDECOD = c("Headache", "Nausea", "Rash", "Cough", "Fatigue", "Pyrexia",
                "Dizziness", "Insomnia", "Back pain"),
    stringsAsFactors = FALSE
  )
}

.t77_listing <- function(dat = .t77_dat(), key_cols = "USUBJID") {
  skip_if_not_installed("rlistings")
  skip_if_not_installed("formatters")
  rlistings::as_listing(dat, key_cols = key_cols)
}

# The written RTF, as one string.
.t77_rtf <- function(pages) {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(rtf_tables(rtf_document(), pages), f)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

# The subjects each page of the written RTF prints, the pages cut at its
# page breaks.
.t77_subjects <- function(pages) {
  txt <- strsplit(.t77_rtf(pages), "\\\\page(?![a-z])", perl = TRUE)[[1L]]
  ids <- c("S-001", "S-002", "S-003")
  lapply(txt, function(t) ids[vapply(ids, grepl, NA, x = t, fixed = TRUE)])
}

.t77_rows <- function(pages) {
  unname(vapply(pages, function(p) nrow(p$data), 1L))
}
.t77_meta <- function(p) attr(p$data, "rtf_paginate_meta", exact = TRUE)

test_that("FT-TB7701 [FR-IX418]: max_rows alone pages an rlistings listing by subject, each subject whole when it fits", {
  l <- .t77_listing()
  pages <- as_rtftables(l, max_rows = 5)
  # S-002 (2 rows) does not fit under S-001 (4): it opens the next page
  expect_identical(.t77_rows(pages), c(4L, 5L))
  expect_identical(.t77_meta(pages[[1L]])$strategy, "group_safe")
  expect_identical(.t77_meta(pages[[1L]])$group_col, 1L)
  expect_identical(pages[[2L]]$data[[1L]], c("S-002", "", "S-003", "", ""))
  expect_identical(.t77_subjects(pages), list("S-001", c("S-002", "S-003")))
  # a group starts at a filled key cell: a key with a leading space still
  # makes one group per subject
  d <- .t77_dat()
  d$USUBJID <- paste0(" ", d$USUBJID)
  expect_identical(.t77_rows(as_rtftables(.t77_listing(d), max_rows = 5)),
                   c(4L, 5L))
})

test_that("FT-TB7702 [FR-IX418]: a split the caller passes wins over the rlistings default", {
  l <- .t77_listing()
  rows <- as_rtftables(l, max_rows = 5, split = "rows", split_rows = 3L)
  expect_identical(.t77_rows(rows), c(2L, 7L))
  expect_identical(.t77_meta(rows[[1L]])$strategy, "rows")
  expect_null(.t77_meta(rows[[1L]])$group_col)
  expect_identical(.t77_subjects(rows), list("S-001", c("S-002", "S-003")))
  none <- as_rtftables(l, max_rows = 5, split = "none")
  expect_length(none, 1L)
  expect_identical(.t77_rows(none), 9L)
  expect_identical(.t77_subjects(none), list(c("S-001", "S-002", "S-003")))
})

test_that("FT-TB7703 [FR-IX418]: a group_col or group_by the caller passes wins over the first key column", {
  l2 <- .t77_listing(key_cols = c("USUBJID", "AESEV"))
  # default: the first key, USUBJID; S-001 (4 rows) is cut, S-002 kept whole
  def <- as_rtftables(l2, max_rows = 3)
  expect_identical(.t77_rows(def), c(2L, 3L, 2L, 3L))
  expect_identical(.t77_meta(def[[1L]])$group_col, 1L)
  # group_col = 2: the groups are AESEV runs, and S-002 is split
  own <- as_rtftables(l2, max_rows = 3, group_col = 2L)
  expect_identical(.t77_rows(own), c(2L, 3L, 1L, 3L))
  expect_identical(.t77_meta(own[[1L]])$group_col, 2L)
  expect_identical(own[[3L]]$data[[3L]], "Pyrexia")
  expect_identical(.t77_subjects(own),
                   list("S-001", "S-002", character(0), "S-003"))
  # group_by = "value" on the blanked key: the blank run after S-002's first
  # row is a group of its own, so S-002 is split
  l <- .t77_listing()
  val <- as_rtftables(l, max_rows = 5, group_by = "value")
  expect_identical(.t77_rows(val), c(5L, 4L))
  expect_identical(.t77_subjects(val),
                   list(c("S-001", "S-002"), "S-003"))
})

test_that("FT-TB7704 [FR-IX418]: the first key column is found by name when the display columns put it elsewhere", {
  l <- .t77_listing()
  rlistings::listing_dispcols(l) <- c("AEDECOD", "USUBJID", "AESEV")
  pages <- as_rtftables(l, max_rows = 5)
  expect_identical(pages[[1L]]$data[[1L]],
                   c("Headache", "Nausea", "Rash", "Cough"))
  expect_identical(.t77_rows(pages), c(4L, 5L))
  expect_identical(.t77_meta(pages[[1L]])$group_col, 2L)
  expect_identical(pages[[2L]]$data[[2L]], c("S-002", "", "S-003", "", ""))
  expect_identical(.t77_subjects(pages), list("S-001", c("S-002", "S-003")))
})

test_that("FT-TB7705 [FR-IX418]: without max_rows an rlistings listing is one page, as before", {
  l <- .t77_listing()
  pages <- as_rtftables(l)
  expect_length(pages, 1L)
  expect_identical(.t77_rows(pages), 9L)
  expect_identical(.t77_meta(pages[[1L]])$strategy, "none")
  expect_null(.t77_meta(pages[[1L]])$group_col)
  expect_identical(.t77_rtf(pages),
                   .t77_rtf(as_rtftables(l, split = "none")))
  expect_identical(.t77_subjects(pages), list(c("S-001", "S-002", "S-003")))
})

test_that("FT-TB7706 [FR-IX418]: a subject longer than max_rows is cut as group_safe cuts it, the key repeated with the continuation label", {
  l <- .t77_listing()
  pages <- as_rtftables(l, max_rows = 3)
  expect_identical(.t77_rows(pages), c(2L, 3L, 2L, 3L))
  expect_true(all(.t77_rows(pages) <= 3L))
  expect_identical(pages[[2L]]$data[[1L]], c("S-001 (Cont.)", "", ""))
  expect_identical(pages[[2L]]$data[[3L]], c("", "Rash", "Cough"))
  expect_identical(.t77_subjects(pages),
                   list("S-001", "S-001", "S-002", "S-003"))
  expect_true(grepl("S-001 (Cont.)", .t77_rtf(pages), fixed = TRUE))
})

test_that("FT-TB7707 [FR-IX418]: an rlistings listing with no key columns groups on column 1", {
  l0 <- .t77_listing(key_cols = character(0))
  pages <- as_rtftables(l0, max_rows = 5)
  expect_identical(.t77_meta(pages[[1L]])$strategy, "group_safe")
  expect_null(.t77_meta(pages[[1L]])$group_col)
  expect_identical(.t77_rows(pages), c(4L, 5L))
  expect_identical(.t77_subjects(pages), list("S-001", c("S-002", "S-003")))
})

test_that("FT-TB7708 [FR-IX418]: every rlistings default and every override, at once", {
  l <- .t77_listing()
  meta1 <- function(pages) .t77_meta(pages[[1L]])
  # max_rows, split left out: group_safe on the first key, whole subjects
  def <- as_rtftables(l, max_rows = 5)
  expect_identical(meta1(def)$strategy, "group_safe")
  expect_identical(meta1(def)$group_col, 1L)
  expect_identical(.t77_subjects(def), list("S-001", c("S-002", "S-003")))
  # a group-aware split given: still the first key
  expect_identical(meta1(as_rtftables(l, max_rows = 5,
                                      split = "group_force"))$group_col, 1L)
  expect_length(as_rtftables(l, split = "by_value"), 3L)
  # by name, wherever the display columns put the key
  lr <- l
  rlistings::listing_dispcols(lr) <- c("AEDECOD", "USUBJID", "AESEV")
  expect_identical(meta1(as_rtftables(lr, max_rows = 5))$group_col, 2L)
  # group_by = "filled": a key with a leading space, which "auto" reads as
  # indentation
  d <- .t77_dat()
  d$USUBJID <- paste0(" ", d$USUBJID)
  ls <- .t77_listing(d)
  expect_identical(.t77_rows(as_rtftables(ls, max_rows = 5)), c(4L, 5L))
  expect_identical(.t77_rows(as_rtftables(ls, max_rows = 5,
                                          group_by = "auto")), c(5L, 4L))
  # a longer subject is cut, the key repeated with the continuation label
  long <- as_rtftables(l, max_rows = 3)
  expect_identical(.t77_rows(long), c(2L, 3L, 2L, 3L))
  expect_identical(long[[2L]]$data[[1L]][1L], "S-001 (Cont.)")
  # no key columns: column 1
  l0 <- .t77_listing(key_cols = character(0))
  expect_null(meta1(as_rtftables(l0, max_rows = 5))$group_col)
  # the caller's split, group_col and group_by win
  expect_identical(.t77_rows(as_rtftables(l, max_rows = 5, split = "rows",
                                          split_rows = 3L)), c(2L, 7L))
  expect_length(as_rtftables(l, max_rows = 5, split = "none"), 1L)
  expect_identical(meta1(as_rtftables(l, max_rows = 5,
                                      group_col = 3L))$group_col, 3L)
  expect_identical(.t77_rows(as_rtftables(l, max_rows = 5,
                                          group_by = "value")), c(5L, 4L))
  # without max_rows: one page, as before
  expect_length(as_rtftables(l), 1L)
  # each listing of a list takes the defaults
  lst <- as_rtftables(list(a = l, b = l), max_rows = 5)
  expect_identical(.t77_rows(lst), c(4L, 5L, 4L, 5L))
  expect_identical(names(lst), c("a...1", "a...2", "b...1", "b...2"))
  expect_length(as_rtftables(list(a = l, b = l), max_rows = 5,
                             split = "none"), 2L)
})

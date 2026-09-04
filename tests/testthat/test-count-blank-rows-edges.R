# count_blank_rows and the PAGE EDGES (#362).
#
# `count_blank_rows = TRUE` means `max_rows` is the number of rows the page
# actually prints.  `blank_row_first` / `blank_row_end` print rows too, so they
# have to be counted -- they were not, and a page could print `max_rows + 2`.
#
# The counting is exact rather than a flat reserve of two: `blank_row_normalize`
# merges a page-edge blank into an adjacent blank row, so the edge costs a row
# only when the row it sits against is not blank itself.  A listing ends every
# record's block with a blank row, which is exactly that case.

# Records of `lines` rows each, followed by a blank row -- the shape
# build_listing() produces.
.blocks <- function(n_rec, lines) {
  rows <- character(0); rec <- integer(0)
  for (i in seq_len(n_rec)) {
    rows <- c(rows, paste0("REC", sprintf("%02d", i), "-L", seq_len(lines)), "")
    rec  <- c(rec, rep(i, lines + 1L))
  }
  # `.rec` is the hidden record column build_listing() emits: it groups the
  # pages and is dropped before rendering, exactly as a listing's is.
  data.frame(A = rows, B = "", .rec = rec, stringsAsFactors = FALSE)
}

# The rows a page prints: its data rows plus the blank rows that do not merge
# into one already there.
.printed <- function(page) {
  d  <- page$data
  bl <- attr(d, "rtf_blank_rows", exact = TRUE)
  if (is.null(bl)) return(nrow(d))
  blank_row <- function(i) {
    if (i < 1L || i > nrow(d)) return(FALSE)
    all(!nzchar(trimws(vapply(d, function(cl) {
      v <- as.character(cl[[i]]); if (is.na(v)) "" else v
    }, character(1L)))))
  }
  extra <- vapply(bl, function(p) {
    if (p == 0L) !blank_row(1L) else !blank_row(as.integer(p))
  }, logical(1L))
  nrow(d) + sum(extra)
}


test_that("the page edges now count toward max_rows", {
  body <- .blocks(20, 3)          # blocks of 4 rows: 3 lines + a blank

  # Before #362 this fitted 10 blocks (40 data rows) and printed 41.
  pages <- as_rtftables(body, split = "group_safe", max_rows = 40,
                        group_col = ".rec", group_by = "value", drop_cols = ".rec",
                        blank_row_first = TRUE, blank_row_end = TRUE,
                        count_blank_rows = TRUE)
  for (p in pages) expect_lte(.printed(p), 40L)
})

test_that("count_blank_rows = FALSE still counts data rows only", {
  body  <- .blocks(20, 3)
  pages <- as_rtftables(body, split = "group_safe", max_rows = 40,
                        group_col = ".rec", group_by = "value", drop_cols = ".rec",
                        blank_row_first = TRUE, blank_row_end = TRUE,
                        count_blank_rows = FALSE)
  # The edges are page furniture and do not count, so the data fills the
  # budget exactly -- the documented FALSE behaviour, unchanged.
  expect_equal(nrow(pages[[1L]]$data), 40L)
  expect_gt(.printed(pages[[1L]]), 40L)
})

test_that("an edge against a blank row costs nothing, so the page is not wasted", {
  # Each block ends in a blank, so `blank_row_end` merges with it: only
  # `blank_row_first` costs a row, and 9 blocks (36 rows + 1) fit in 40 while
  # 10 (40 + 1) do not.
  body  <- .blocks(20, 3)
  pages <- as_rtftables(body, split = "group_safe", max_rows = 40,
                        group_col = ".rec", group_by = "value", drop_cols = ".rec",
                        blank_row_first = TRUE, blank_row_end = TRUE,
                        count_blank_rows = TRUE)
  expect_equal(nrow(pages[[1L]]$data), 36L)
  expect_equal(.printed(pages[[1L]]), 37L)

  # A flat reserve of two would have stopped at 8 blocks (32 rows); this does
  # not, because it asks what the page would print.
  expect_gt(nrow(pages[[1L]]$data), 32L)
})

test_that("only the edges that are switched on are charged for", {
  body <- .blocks(20, 3)
  fit  <- function(...) {
    nrow(as_rtftables(body, split = "group_safe", max_rows = 40,
                      group_col = ".rec", group_by = "value", drop_cols = ".rec",
                      count_blank_rows = TRUE, ...)[[1L]]$data)
  }
  expect_equal(fit(), 40L)                              # no edges: 10 blocks
  expect_equal(fit(blank_row_end = TRUE), 40L)          # merges: still 10
  expect_equal(fit(blank_row_first = TRUE), 36L)        # costs 1: 9 blocks
})

test_that("a body that does NOT end in a blank pays for both edges", {
  # No trailing blank per record, so `blank_row_end` has nothing to merge with.
  body <- data.frame(A = paste0("r", 1:40), B = "",
                     .rec = 1:40, stringsAsFactors = FALSE)
  pages <- as_rtftables(body, split = "group_safe", max_rows = 10,
                        group_col = ".rec", group_by = "value", drop_cols = ".rec",
                        blank_row_first = TRUE, blank_row_end = TRUE,
                        count_blank_rows = TRUE)
  expect_equal(nrow(pages[[1L]]$data), 8L)     # 8 data + 2 edges = 10
  for (p in pages) expect_lte(.printed(p), 10L)
})

test_that("the group-safe budget matches the hand-written listing splitter", {
  # The pipeline of Discussion #356 cut pages so that
  #   1 (the page's leading blank row) + k * block <= max_rows,
  # because that blank row was part of the data.  With the edges counted this
  # is the same arithmetic, so the same number of records lands on a page.
  for (lines in c(3, 4, 7, 9)) {
    block <- lines + 1L
    pages <- as_rtftables(.blocks(40, lines), split = "group_safe",
                          max_rows = 40, group_col = ".rec", group_by = "value", drop_cols = ".rec",
                          blank_row_first = TRUE, blank_row_end = TRUE,
                          count_blank_rows = TRUE)
    expect_equal(nrow(pages[[1L]]$data), (40L %/% block) * block - block *
                   as.integer((40L %/% block) * block + 1L > 40L),
                 info = sprintf("block of %d rows", block))
  }
})

test_that("a listing reaches the same budget through as_rtftables(listing = )", {
  src <- data.frame(
    A = vapply(1:20, function(i)
      paste(paste0("REC", sprintf("%02d", i), "-L", 1:3), collapse = "\n"),
      character(1L)),
    B = "", stringsAsFactors = FALSE)
  spec <- listing_spec(list(listing_col("A"), listing_col("B")),
                       spacer = FALSE)

  pages <- as_rtftables(src, listing = spec, max_rows = 40,
                        blank_row_end = TRUE, count_blank_rows = TRUE)
  expect_equal(nrow(pages[[1L]]$data), 36L)
  for (p in pages) expect_lte(.printed(p), 40L)
})

test_that("group_force honours the edges too", {
  # One group of 30 rows, no internal blanks: with both edges counted a page
  # can hold 8 of its rows, not 10.
  body <- data.frame(A = c("G", rep("", 29)), B = paste0("r", 1:30),
                     stringsAsFactors = FALSE)
  pages <- as_rtftables(body, split = "group_force", max_rows = 10,
                        group_col = "A", group_by = "filled",
                        blank_row_first = TRUE, blank_row_end = TRUE,
                        count_blank_rows = TRUE)
  for (p in pages) expect_lte(.printed(p), 10L)
})

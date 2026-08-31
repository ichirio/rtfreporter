## tests/testthat/test-count-blank-rows-groups.R
##
## #330 -- count_blank_rows = TRUE materialises empty marker rows before the
## split.  Those empty cells used to change what group_by = "auto" detected
## ("value" -> "filled"), which made every row its own group and let
## split = "group_safe" cut a group in half.

library(testthat)

.g330 <- function(sizes) {
  data.frame(G = rep(LETTERS[seq_along(sizes)], sizes),
             N = as.character(seq_len(sum(sizes))),
             stringsAsFactors = FALSE)
}

.split_groups <- function(pages) {
  seen <- unlist(lapply(pages, function(p) unique(p$data$G)))
  unique(seen[duplicated(seen)])
}

test_that("group_safe keeps groups whole with count_blank_rows (#330)", {
  d <- .g330(c(3, 3, 3))
  p <- as_rtftables(d, split = "group_safe", max_rows = 7, group_col = "G",
                    blank_rows = "between_groups", count_blank_rows = TRUE)
  expect_identical(.split_groups(p), character(0))
})

test_that("... across a range of shapes and budgets", {
  for (sizes in list(c(3, 3, 3), c(4, 4, 4), c(2, 5, 3), c(3, 4, 2, 3),
                     c(5, 2, 5))) {
    for (mr in c(6L, 7L, 8L, 9L)) {
      p <- as_rtftables(.g330(sizes), split = "group_safe", max_rows = mr,
                        group_col = "G", blank_rows = "between_groups",
                        count_blank_rows = TRUE)
      expect_identical(.split_groups(p), character(0),
                       info = sprintf("sizes %s, max_rows %d",
                                      paste(sizes, collapse = "-"), mr))
    }
  }
})

test_that("min_group_rows is respected with count_blank_rows", {
  p <- as_rtftables(.g330(c(3, 3, 3)), split = "group_safe", max_rows = 7,
                    group_col = "G", blank_rows = "between_groups",
                    count_blank_rows = TRUE)
  for (pg in p) {
    tb <- table(pg$data$G)
    # A group present on a page brings at least min_group_rows (2) rows with
    # it, unless the whole group is smaller than that.
    expect_true(all(as.integer(tb) >= 2L))
  }
})

test_that("auto detection is settled on the un-materialised body", {
  plain      <- c("A", "A", "A", "B", "B", "B", "C", "C", "C")
  with_marks <- c("A", "A", "A", "", "B", "B", "B", "", "C", "C", "C")
  # This IS the trap: the same column reads differently once markers are in.
  expect_identical(rtfreporter:::.detect_group_mode(plain), "value")
  expect_identical(rtfreporter:::.detect_group_mode(with_marks), "filled")

  # ... so the pages must match what "value" would give, not "filled".
  d <- .g330(c(3, 3, 3))
  auto <- as_rtftables(d, split = "group_safe", max_rows = 7, group_col = "G",
                       blank_rows = "between_groups", count_blank_rows = TRUE)
  value <- as_rtftables(d, split = "group_safe", max_rows = 7, group_col = "G",
                        group_by = "value", blank_rows = "between_groups",
                        count_blank_rows = TRUE)
  expect_identical(lapply(auto, function(p) p$data),
                   lapply(value, function(p) p$data))
})

test_that("a marker row never opens a group in indent / filled mode", {
  d  <- .g330(c(3, 3))
  dm <- rtfreporter:::.materialize_blank_markers(d, 3L)
  for (mode in c("indent", "filled")) {
    info <- rtfreporter:::.compute_group_info(dm, 1L, group_by = mode)
    expect_false(info$headers[[4L]])       # row 4 is the marker
  }
})

test_that("count_blank_rows = FALSE is unchanged", {
  d <- .g330(c(3, 3, 3))
  p <- as_rtftables(d, split = "group_safe", max_rows = 7, group_col = "G",
                    blank_rows = "between_groups")
  expect_identical(vapply(p, function(x) paste(x$data$G, collapse = ""), ""),
                   c("AAABBB", "CCC"))
})

test_that("blank_row_first / blank_row_end still work on their own", {
  d <- .g330(c(4, 4, 4))
  none <- as_rtftables(d, split = "group_safe", max_rows = 5, group_col = "G")
  expect_true(all(vapply(none, function(p) is.null(p$blank_rows) ||
                           !length(p$blank_rows), logical(1L))))
  end <- as_rtftables(d, split = "group_safe", max_rows = 5, group_col = "G",
                      blank_row_end = TRUE)
  for (p in end) expect_true(nrow(p$data) %in% p$blank_rows)
})

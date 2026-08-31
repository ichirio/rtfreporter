# count_blank_rows = TRUE: blank separator rows count toward max_rows during
# pagination (materialise-then-collapse), default FALSE keeps current behaviour.

.df9 <- function() {
  data.frame(
    label = c("Group A", "  a1", "  a2", "Group B", "  b1", "  b2",
              "Group C", "  c1", "  c2"),
    v = as.character(1:9), stringsAsFactors = FALSE
  )
}

.total <- function(page) nrow(page$data) + length(page$blank_rows)

test_that("default (FALSE) does not count blanks: a page can exceed max_rows", {
  pages <- as_rtftables(.df9(), split = "group_safe", max_rows = 6,
                        blank_rows = "between_groups")
  # group_safe packs A+B (6 data) on page 1, plus a between-group blank -> 7.
  expect_equal(nrow(pages[[1L]]$data), 6L)
  expect_true(.total(pages[[1L]]) > 6L)        # overflows the visual budget
})

test_that("count_blank_rows = TRUE keeps every page within max_rows", {
  pages <- as_rtftables(.df9(), split = "group_safe", max_rows = 6,
                        blank_rows = "between_groups", count_blank_rows = TRUE)
  for (p in pages) expect_lte(.total(p), 6L)   # data + blanks <= max_rows
  # Each page ends up holding exactly one group here, so there is no BETWEEN
  # to separate and no separator survives -- see the page-edge rule (#332).
  for (p in pages) expect_null(p$blank_rows)
})

test_that("a separator survives when a page really holds two groups", {
  # Budget for A + separator + B + separator (3 + 1 + 3 + 1), so page 1 has a
  # genuine between at row 3 -- and none at row 6, its last.
  pages <- as_rtftables(.df9(), split = "group_safe", max_rows = 8,
                        blank_rows = "between_groups", count_blank_rows = TRUE)
  expect_identical(nrow(pages[[1L]]$data), 6L)
  expect_identical(pages[[1L]]$blank_rows, 3L)
  for (p in pages) expect_lte(.total(p), 8L)
})

test_that("count_blank_rows counts an existing rtf_blank_rows attribute", {
  df <- set_blank_rows(.df9(), blank_rows = "between_groups")
  expect_false(is.null(attr(df, "rtf_blank_rows")))
  pages <- as_rtftables(df, split = "group_safe", max_rows = 6,
                        count_blank_rows = TRUE)   # no blank_rows arg
  for (p in pages) expect_lte(.total(p), 6L)
})

test_that("a leading blank is suppressed at the top of a page", {
  # Position 0 (before first row) must never appear as a page's first row.
  pages <- as_rtftables(.df9(), split = "group_force", max_rows = 4,
                        blank_rows = c(0L, 3L), count_blank_rows = TRUE)
  for (p in pages) expect_false(0L %in% p$blank_rows)
})

test_that("the marker column never leaks into the rendered table", {
  pages <- as_rtftables(.df9(), split = "group_safe", max_rows = 6,
                        blank_rows = "between_groups", count_blank_rows = TRUE)
  for (p in pages) expect_false(".__rtf_blank__" %in% names(p$data))
  # And it renders end-to-end.
  doc <- rtf_document() |> rtf_tables(pages)
  f <- tempfile(fileext = ".rtf"); on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  expect_gt(file.info(f)$size, 0)
})

test_that("count_blank_rows with no blanks behaves like the default", {
  a <- as_rtftables(.df9(), split = "group_safe", max_rows = 6)
  b <- as_rtftables(.df9(), split = "group_safe", max_rows = 6,
                    count_blank_rows = TRUE)
  expect_equal(length(a), length(b))
  expect_identical(lapply(a, function(p) p$data),
                   lapply(b, function(p) p$data))
})

# ---- #332: a separator never occupies a page edge ---------------------------

test_that("between_groups never lands on a page's first or last row", {
  d <- data.frame(G = rep(c("A", "B", "C"), each = 3),
                  N = as.character(1:9), stringsAsFactors = FALSE)
  for (mr in c(3L, 4L, 6L, 7L)) {
    for (cnt in c(FALSE, TRUE)) {
      p <- as_rtftables(d, split = "group_safe", max_rows = mr, group_col = "G",
                        group_by = "value", blank_rows = "between_groups",
                        count_blank_rows = cnt)
      for (pg in p) {
        b <- pg$blank_rows
        if (!is.null(b)) {
          expect_false(0L %in% b, info = sprintf("max_rows %d, count %s", mr, cnt))
          expect_false(nrow(pg$data) %in% b,
                       info = sprintf("max_rows %d, count %s", mr, cnt))
        }
      }
    }
  }
})

test_that("blank_rows_by_change() never lands on a page edge either", {
  d <- data.frame(G = rep(c("A", "B", "C"), each = 3),
                  N = as.character(1:9), stringsAsFactors = FALSE)
  p <- as_rtftables(d, split = "group_safe", max_rows = 3, group_col = "G",
                    blank_rows = blank_rows_by_change("G"))
  # Before #332 every page came back with c(0, 3) -- both edges.
  for (pg in p) expect_null(pg$blank_rows)
})

test_that("the page-level settings still own the edges", {
  d <- data.frame(G = rep(c("A", "B", "C"), each = 3),
                  N = as.character(1:9), stringsAsFactors = FALSE)
  e <- as_rtftables(d, split = "group_safe", max_rows = 3, group_col = "G",
                    blank_rows = "between_groups", blank_row_end = TRUE)
  for (pg in e) expect_true(nrow(pg$data) %in% pg$blank_rows)
  f <- as_rtftables(d, split = "group_safe", max_rows = 3, group_col = "G",
                    blank_rows = "between_groups", blank_row_first = TRUE)
  for (pg in f) expect_true(0L %in% pg$blank_rows)
})

test_that("an explicit position is honoured wherever it lands", {
  d <- data.frame(G = rep(c("A", "B", "C"), each = 3),
                  N = as.character(1:9), stringsAsFactors = FALSE)
  # Per page, "after row 3" IS the last row -- a direct request, not a
  # separator, so it stays.
  p <- as_rtftables(d, split = "group_safe", max_rows = 3, group_col = "G",
                    blank_rows = 3)
  for (pg in p) expect_identical(pg$blank_rows, 3L)
})

test_that("a mixed spec trims only the separator half", {
  d <- data.frame(G = rep(c("A", "B", "C"), each = 3),
                  N = as.character(1:9), stringsAsFactors = FALSE)
  p <- as_rtftables(d, split = "group_safe", max_rows = 3, group_col = "G",
                    blank_rows = list("between_groups", 1))
  for (pg in p) expect_identical(pg$blank_rows, 1L)
})

test_that(".split_blank_spec partitions the two kinds", {
  s <- rtfreporter:::.split_blank_spec(list("between_groups", 3,
                                            blank_rows_by_change("G")))
  expect_length(s$separator, 2L)
  expect_length(s$explicit, 1L)
  expect_identical(s$explicit[[1L]], 3)

  plain <- rtfreporter:::.split_blank_spec(3)
  expect_null(plain$separator)
  expect_identical(plain$explicit[[1L]], 3)

  # A classed spec is itself a list; it must not be treated as a collection.
  one <- rtfreporter:::.split_blank_spec(blank_rows_by_change("G"))
  expect_length(one$separator, 1L)
  expect_null(one$explicit)
})

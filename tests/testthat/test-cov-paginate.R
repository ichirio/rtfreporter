# test-cov-paginate.R -- edge paths of row pagination (R/paginate.R) and of
# column pagination (R/paginate_cols.R): empty bodies, cuts that land on a
# counted blank row, separators that open a page, listing records cut at a
# page edge, and column pages of a table whose relative widths do not fit.

.cov_pages_data <- function(pages) lapply(pages, function(p) p$data)

# ---- R/paginate.R ----------------------------------------------------------

test_that("group_safe on an empty body gives one empty page", {
  skip_on_cran()
  d0 <- data.frame(g = character(0), v = integer(0))
  pages <- as_rtftables(d0, split = "group_safe", max_rows = 5,
                        group_col = "g")
  expect_length(pages, 1L)
  expect_identical(nrow(pages[[1L]]$data), 0L)
  expect_identical(names(pages[[1L]]$data), c("g", "v"))
})

test_that("by_value on an empty body gives one empty page", {
  skip_on_cran()
  d0 <- data.frame(g = character(0), v = integer(0))
  pages <- as_rtftables(d0, split = "by_value", group_col = "g")
  expect_length(pages, 1L)
  expect_identical(nrow(pages[[1L]]$data), 0L)
})

test_that("page_by on an empty body falls back to one unsplit page", {
  skip_on_cran()
  d0 <- data.frame(g = character(0), v = integer(0))
  pages <- as_rtftables(d0, page_by = "g", max_rows = 5)
  expect_length(pages, 1L)
  expect_identical(nrow(pages[[1L]]$data), 0L)
})

test_that("group_force moves the cut onto a counted separator at the page edge", {
  skip_on_cran()
  # Three groups of three rows with a counted separator between them:
  # max_rows = 4 less the counted trailing edge leaves 3 rows, and the row
  # after them is a separator, so the cut merges with it instead of adding a
  # blank of its own.
  d <- data.frame(g = rep(c("A", "B", "C"), each = 3L),
                  v = paste0("r", 1:9), stringsAsFactors = FALSE)
  pages <- as_rtftables(d, split = "group_force", max_rows = 4,
                        group_col = "g", group_by = "value",
                        blank_rows = "between_groups", blank_row_end = TRUE,
                        count_blank_rows = TRUE)
  expect_length(pages, 3L)
  got <- lapply(.cov_pages_data(pages), function(p) p$v)
  expect_identical(got, list(paste0("r", 1:3), paste0("r", 4:6),
                             paste0("r", 7:9)))
  # One blank at the foot of every page, never two.
  for (p in pages) expect_identical(as.integer(p$blank_rows), 3L)
})

test_that("group_safe holds back a group of separators only and drops it at a page top", {
  skip_on_cran()
  # An NA in the group column is a run of its own under group_by = "value",
  # and the counted separator after it carries the NA down, so it forms a
  # group holding only that separator.  It is held back, costs a row where
  # the next group would join the page, and is not printed at a page top.
  d <- data.frame(g = c("A", "A", NA, "B", "B"), v = c("1", "2", "3", "4", "5"),
                  stringsAsFactors = FALSE)
  pages <- as_rtftables(d, split = "group_safe", max_rows = 3,
                        group_col = "g", group_by = "value",
                        blank_rows = "between_groups", count_blank_rows = TRUE)
  got <- lapply(.cov_pages_data(pages), function(p) p$v)
  expect_identical(got, list(c("1", "2"), "3", c("4", "5")))
  for (p in pages) expect_length(p$blank_rows, 0L)
})

test_that("group_force moves the cut back to the start of a listing record", {
  skip_on_cran()
  # Records of three printed lines plus a blank, two of them in subject S1:
  # a cut after seven rows would fall inside record 2, so it moves back to
  # where record 2 starts and record 2 goes whole to the next page.
  src <- data.frame(
    G = c("S1", "S1", "S2"),
    A = c("R1-a\nR1-b\nR1-c", "R2-a\nR2-b\nR2-c", "R3-a\nR3-b\nR3-c"),
    stringsAsFactors = FALSE)
  spec <- listing_spec(list(listing_col("G"), listing_col("A")),
                       spacer = FALSE)
  pages <- as_rtftables(src, listing = spec, max_rows = 7,
                        split = "group_force", group_col = "G")
  got <- vapply(pages, function(p) paste(p$data$A, collapse = "|"), "")
  expect_identical(got, c("R1-a|R1-b|R1-c|", "R2-a|R2-b|R2-c|",
                          "R3-a|R3-b|R3-c|"))
})

test_that(".row_is_blank() is FALSE outside the rows and TRUE on a marker row", {
  skip_on_cran()
  # Internal: every caller passes a row inside the chunk, and the marker rows
  # are added inside .paginate_df(), so this is unit-tested directly.
  rib <- rtfreporter:::.row_is_blank
  d <- data.frame(a = c("x", ""), .__rtf_blank__ = c(0L, 1L),
                  check.names = FALSE, stringsAsFactors = FALSE)
  expect_false(rib(d, 0L))
  expect_false(rib(d, 3L))
  expect_false(rib(d, NA_integer_))
  expect_false(rib(NULL, 1L))
  expect_false(rib(d, 1L))
  expect_true(rib(d, 2L))
})

test_that(".edge_cost() counts each switched-on edge against a non-blank row", {
  skip_on_cran()
  # Internal helper with no caller in the package today: unit-tested directly.
  ec <- rtfreporter:::.edge_cost
  d <- data.frame(a = c("", "x", "y"), stringsAsFactors = FALSE)
  expect_identical(ec(NULL, TRUE, TRUE), 0L)
  expect_identical(ec(d[0L, , drop = FALSE], TRUE, TRUE), 0L)
  expect_identical(ec(d, edge_first = TRUE, edge_end = TRUE), 1L)
  expect_identical(ec(d, edge_first = FALSE, edge_end = TRUE), 1L)
  expect_identical(ec(d, edge_first = TRUE, edge_end = FALSE), 0L)
  expect_identical(ec(d[2:3, , drop = FALSE], TRUE, TRUE), 2L)
})

# ---- R/paginate_cols.R -----------------------------------------------------

.cov_wide <- function() {
  data.frame(id = c("a", "b"), x1 = 1:2, x2 = 3:4, x3 = 5:6)
}

test_that("paginate_cols() counts columns when col_rel_width does not match the table", {
  skip_on_cran()
  # Two weights for four columns: the share of a page is its column count,
  # so block 2 (three columns) is 1.5 times block 1 (two).
  t1 <- rtftable(.cov_wide(), col_rel_width = c(2, 1))
  expect_warning(pages <- paginate_cols(t1, at = 3, keep = 1),
                 "column block 2 totals more than block 1")
  expect_length(pages, 2L)
  expect_identical(names(pages[[1L]]$data), c("id", "x1"))
  expect_identical(names(pages[[2L]]$data), c("id", "x2", "x3"))
  expect_null(pages[[1L]]$table_width_pct_of_writable)
  expect_equal(pages[[2L]]$table_width_pct_of_writable, 1.5)
})

test_that("paginate_cols(fit = FALSE) measures against the column count when the weights do not match", {
  skip_on_cran()
  t1 <- rtftable(.cov_wide(), col_rel_width = c(2, 1))
  expect_no_warning(pages <- paginate_cols(t1, at = 3, keep = 1, fit = FALSE))
  expect_equal(pages[[1L]]$table_width_pct_of_writable, 2 / 4)
  expect_equal(pages[[2L]]$table_width_pct_of_writable, 3 / 4)
})

test_that("paginate_cols() counts columns when every relative width is zero", {
  skip_on_cran()
  t0 <- rtftable(.cov_wide(), col_rel_width = c(0, 0, 0, 0))
  pages <- paginate_cols(t0, at = 4, keep = 1, fit = FALSE)
  expect_equal(pages[[1L]]$table_width_pct_of_writable, 3 / 4)
  expect_equal(pages[[2L]]$table_width_pct_of_writable, 2 / 4)
})

test_that("paginate_cols() clips a legacy spanning_header and drops a span left with no column", {
  skip_on_cran()
  # A built table's `spanning_header` field (the removed argument's legacy
  # slot, still read by the renderer) is set by hand here.
  tbl <- rtftable(.cov_wide())
  tbl$spanning_header <- list(list(label = "Span", from = 2L, to = 2L))
  pages <- suppressWarnings(paginate_cols(tbl, at = 3, keep = 1))
  sp1 <- pages[[1L]]$spanning_header
  expect_length(sp1, 1L)
  expect_identical(sp1[[1L]]$label, "Span")
  expect_equal(c(sp1[[1L]]$from, sp1[[1L]]$to), c(2, 2))
  expect_null(pages[[2L]]$spanning_header)
})

test_that("allow_span_break = FALSE resolves a selector `pos` before refusing the cut", {
  skip_on_cran()
  tbl <- rtftable(.cov_wide())
  tbl$spanning_header <- list(list(label = "Sel",
                                   pos = function(n) n %in% c("x1", "x2")))
  expect_error(paginate_cols(tbl, at = 3, keep = 1, allow_span_break = FALSE),
               "would break the spanning header cell 'Sel' \\(columns 2\\.\\.3\\) at column 3")
})

test_that("allow_span_break = FALSE passes over a selector `pos` that fails", {
  skip_on_cran()
  tbl <- rtftable(.cov_wide())
  tbl$spanning_header <- list(list(label = "Bad",
                                   pos = function(n) stop("no such column")))
  pages <- suppressWarnings(
    paginate_cols(tbl, at = 3, keep = 1, allow_span_break = FALSE))
  expect_length(pages, 2L)
})

test_that(".resolve_col_blocks() refuses explicit blocks that are not a list", {
  skip_on_cran()
  # paginate_cols() sends a list `cut_by` here and anything else to `by`, so
  # this guard is reached only directly.
  ref <- .cov_wide()
  expect_error(rtfreporter:::.resolve_col_blocks(NULL, c(2, 3), ref, 1L),
               "`cut_by` must be a list of column blocks \\(names or positions\\)")
})

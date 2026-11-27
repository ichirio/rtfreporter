## tests/testthat/test-cov-rtftable.R
##
## Coverage fill-ins for R/rtftable.R: multi-data.frame col_header forms,
## label-row length checks, the console view of a spanning row with a gap,
## and the summary()/print() meta lines (fixed widths, markup, cell styles).

library(testthat)

.cov_rt_df <- function() {
  data.frame(a = c("x", "y"), b = 1:2, c = 3:4, stringsAsFactors = FALSE)
}

test_that("multi-df: a list of labels longer than the tables is one shared header", {
  skip_on_cran()
  d <- .cov_rt_df()
  t <- rtftable(list(d, d), col_header = list("Item", "B", "C"))
  expect_length(t$col_header_list, 2L)
  expect_identical(t$col_header_list[[1L]], t$col_header_list[[2L]])
  expect_identical(t$col_header_list[[1L]], list("Item", "B", "C"))
})

test_that("multi-df: a header list with a non-label element is refused", {
  skip_on_cran()
  d <- .cov_rt_df()
  expect_error(rtftable(list(d, d), col_header = list("Item", 1)),
               "Multi-row col_header must contain only character vectors")
})

test_that("a short label row names the first column in its message", {
  skip_on_cran()
  # Internal: every API path passes the data.frame's names, so the branch
  # for no column names (which falls back to "col") needs a direct call.
  expect_error(
    rtfreporter:::.check_label_row_length(c("A", "B"), 1L, 3L, NULL),
    "row 1 has 2 labels but the table has 3 columns\\.  .*c\\(col = ")
})

test_that("a pos-style header row needs the column count", {
  skip_on_cran()
  # Internal: every caller passes `ncol_df`; this stop is a guard.
  expect_error(
    rtfreporter:::.normalize_col_header_rows(list(list(list(pos = 1, label = "A")))),
    "Internal: ncol_df required to normalize pos-style rows\\.")
})

test_that("format() leaves the uncovered first column blank in a spanning row", {
  skip_on_cran()
  t <- rtftable(.cov_rt_df(),
                col_header = list(list(list(from = 2, to = 3, label = "Treat")),
                                  c("Item", "B", "C")))
  out <- format(t)
  span_line <- out[grepl("Treat", out, fixed = TRUE)]
  expect_length(span_line, 1L)
  # column 1 ("Item", 4 wide) is padded out before the spanner label
  expect_match(span_line, "^ {4,}Treat$")
  expect_true(any(grepl("^Item +B +C$", out)))
})

test_that("summary() lists fixed widths, markup and cell styles", {
  skip_on_cran()
  t <- rtftable(.cov_rt_df(), column_widths_twips = c(1000, 2000, 3000),
                markup = "script",
                cell_styles = list(list(bold = TRUE), NULL))
  out <- capture.output(summary(t))
  expect_true("  Widths:     fixed (1000, 2000, 3000 twips)" %in% out)
  expect_true("  Markup:     script" %in% out)
  expect_true("  Cell styles: set (2 rows)" %in% out)
  expect_identical(out[1L], "<rtftable> 2 rows x 3 columns")
})

test_that("print() ends with the same meta lines for a one-row table", {
  skip_on_cran()
  t <- rtftable(data.frame(a = "\u00e9t\u00e9", b = 1L),
                column_widths_twips = c(1440, 1440))
  out <- capture.output(print(t))
  expect_true("<rtftable> 1 row x 2 columns" %in% out)
  expect_true("  Widths:     fixed (1440, 1440 twips)" %in% out)
  expect_false(any(grepl("Cell styles", out, fixed = TRUE)))
})

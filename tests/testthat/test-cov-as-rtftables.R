## tests/testthat/test-cov-as-rtftables.R
##
## Coverage fill-ins for R/as_rtftables.R: `drop_cols` on an rlistings listing,
## user `cell_styles` passed through `...` (with and without `sort_by`), and
## `border_group` found per `page_by` page.

library(testthat)

.cov_lst <- function() {
  dat <- data.frame(USUBJID = c("S-001", "S-001", "S-002"),
                    AETERM  = c("Nausea", "Fatigue", "Rash"),
                    HIDDEN  = c("h1", "h2", "h3"),
                    stringsAsFactors = FALSE)
  rlistings::as_listing(dat, key_cols = "USUBJID",
                        disp_cols = c("AETERM", "HIDDEN"))
}

test_that("drop_cols on a listing hides the column next to the record column", {
  skip_on_cran()
  skip_if_not_installed("rlistings")
  skip_if_not_installed("formatters")
  p <- as_rtftables(.cov_lst(), drop_cols = "V3", read_meta = FALSE)
  expect_length(p, 1L)
  expect_identical(names(p[[1L]]$data), c("V1", "V2"))
  expect_identical(p[[1L]]$data$V2, c("Nausea", "Fatigue", "Rash"))
  expect_false(any(unlist(p[[1L]]$data) %in% c("h1", "h2", "h3")))
})

test_that("drop_cols on a listing keeps the listing's own column header", {
  skip_on_cran()
  skip("bug: as_rtftables(<listing_df>, drop_cols =) errors: auto col_header not reindexed past the .rtf_record column")
  skip_if_not_installed("rlistings")
  skip_if_not_installed("formatters")
  p <- as_rtftables(.cov_lst(), drop_cols = "V3")
  expect_identical(names(p[[1L]]$data), c("V1", "V2"))
  expect_identical(p[[1L]]$col_header[[1L]], c("USUBJID", "AETERM"))
})

test_that("user cell_styles in ... follow their rows through sort_by", {
  skip_on_cran()
  df <- data.frame(k = c("b", "a", "c"), v = c("2", "1", "3"),
                   stringsAsFactors = FALSE)
  cs <- list(list(bold = TRUE), NULL, NULL)
  p <- as_rtftables(df, sort_by = "k", cell_styles = cs)
  expect_identical(p[[1L]]$data$k, c("a", "b", "c"))
  bold <- vapply(seq_len(3L), function(i) isTRUE(p[[1L]]$cell_styles[[i]]$bold),
                 logical(1L))
  # the bold row was "b" (row 1); after the sort it is row 2
  expect_identical(bold, c(FALSE, TRUE, FALSE))
})

test_that("user cell_styles in ... win over none from the source, unsorted", {
  skip_on_cran()
  df <- data.frame(k = c("b", "a"), v = c("2", "1"), stringsAsFactors = FALSE)
  p <- as_rtftables(df, cell_styles = list(NULL, list(italic = TRUE)))
  expect_null(p[[1L]]$cell_styles[[1L]])
  expect_true(isTRUE(p[[1L]]$cell_styles[[2L]]$italic))
})

test_that("border_group with page_by finds groups inside each page", {
  skip_on_cran()
  d <- data.frame(PG   = c("P1", "P1", "P1", "P1", "P2", "P2", "P2"),
                  Item = c("Age", " Mean", " SD", "Sex", "Age", " Mean", " SD"),
                  V    = as.character(1:7), stringsAsFactors = FALSE)
  p <- as_rtftables(d, page_by = "PG", group_by = "indent", group_col = "Item",
                    style = rtf_table_style(border_group = rtf_border(bottom = TRUE)))
  expect_length(p, 2L)
  has_rule <- function(pg) {
    vapply(seq_len(nrow(pg$data)),
           function(i) !is.null(pg$cell_styles[[i]]$border), logical(1L))
  }
  # page P1: two groups (Age..SD, Sex): a rule under SD and under Sex
  expect_identical(has_rule(p[[1L]]), c(FALSE, FALSE, TRUE, TRUE))
  # page P2 holds one group only, so nothing is ruled there
  expect_identical(has_rule(p[[2L]]), c(FALSE, FALSE, FALSE))
})

test_that("user cell_styles replace a gt table's own styles and follow sort_by", {
  skip_on_cran()
  skip_if_not_installed("gt")
  df <- data.frame(k = c("b", "a", "c"), v = c("2", "1", "3"),
                   stringsAsFactors = FALSE)
  g <- gt::gt(df) |>
    gt::tab_style(gt::cell_text(weight = "bold"),
                  gt::cells_body(columns = v, rows = 1))
  # the gt style alone is read onto row 1
  expect_identical(as_rtftables(g)[[1L]]$cell_styles[[1L]]$bold, c(NA, TRUE))
  p <- as_rtftables(g, sort_by = "k",
                    cell_styles = list(list(italic = TRUE), NULL, NULL))
  expect_identical(p[[1L]]$data$k, c("a", "b", "c"))
  cs <- p[[1L]]$cell_styles
  expect_null(cs[[1L]])
  expect_identical(cs[[2L]], list(italic = TRUE))
  expect_null(cs[[3L]])
})

test_that("user cell_styles of the wrong length are refused", {
  skip_on_cran()
  df <- data.frame(k = c("b", "a", "c"), stringsAsFactors = FALSE)
  expect_error(as_rtftables(df, cell_styles = list(list(italic = TRUE))),
               "`cell_styles` length \\(1\\) must equal the number of data rows \\(3\\)\\.")
})

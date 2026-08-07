## tests/testthat/test-set-blank-rows-verb.R
##
## set_blank_rows() as an S3 generic: rtftable + list methods (post-hoc, the
## `as_rtftables() |> set_blank_rows()` pipeline) plus the data.frame method
## (used internally by paginate(); its behaviour is tested in
## test-set-blank-rows.R).

library(testthat)

df_indent <- function() {
  data.frame(label = c("Group A", "  a1", "  a2", "Group B", "  b1"),
             v = 1:5, stringsAsFactors = FALSE)
}
df_ab <- function() {
  data.frame(g = c("A", "A", "B", "B"), v = 1:4, stringsAsFactors = FALSE)
}

# ── rtftable method ────────────────────────────────────────────────────────

test_that("set_blank_rows() on an rtftable sets $blank_rows", {
  tbl <- as_rtftables(df_indent())[[1L]]
  out <- set_blank_rows(tbl, blank_rows = "between_groups", group_col = "label",
                        group_by = "indent", blank_row_end = TRUE)
  expect_s3_class(out, "rtftable")
  expect_identical(out$blank_rows, c(3L, 5L))
})

test_that("the rtftable verb matches the as_rtftables() blank_rows argument", {
  via_arg <- as_rtftables(df_indent(), blank_rows = "between_groups",
                          group_col = "label", group_by = "indent")[[1L]]
  via_fun <- set_blank_rows(as_rtftables(df_indent())[[1L]],
                            blank_rows = "between_groups",
                            group_col = "label", group_by = "indent")
  expect_identical(via_fun$blank_rows, via_arg$blank_rows)
})

test_that("blank_rows_by_change() spec works through the rtftable verb", {
  tbl <- as_rtftables(df_ab())[[1L]]
  out <- set_blank_rows(tbl, blank_rows = blank_rows_by_change(
    "g", include_before_first = FALSE, include_after_last = FALSE))
  expect_identical(out$blank_rows, 2L)
})

# ── list method (per page) ─────────────────────────────────────────────────

test_that("set_blank_rows() maps over a page list (per page)", {
  pages <- c(as_rtftables(df_ab()), as_rtftables(df_ab()))   # two 1-page tables
  out <- set_blank_rows(pages, blank_rows = "between_groups", group_col = "g")
  expect_length(out, 2L)
  expect_identical(out[[1L]]$blank_rows, 2L)   # A -> B transition, per page
  expect_identical(out[[2L]]$blank_rows, 2L)
})

# ── data.frame method still works (paginate's internal path) ───────────────

test_that("set_blank_rows() on a data.frame still attaches the attribute", {
  o <- set_blank_rows(df_indent(), blank_rows = "between_groups",
                      group_col = "label", group_by = "indent",
                      blank_row_end = TRUE)
  expect_true(is.data.frame(o))
  expect_identical(attr(o, "rtf_blank_rows"), c(3L, 5L))
})

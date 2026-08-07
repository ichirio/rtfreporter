## tests/testthat/test-blank-rows-group-by.R
##
## Unified blank-row group recognition (#263):
##   1. blank_rows_by_change(group_by = ) honours value/indent/filled/auto.
##   2. blank_rows = "between_groups" honours group_by (was always "auto").
##   3. blank_rows_by_change()/blank_rows_by_rule() work in as_rtftables().

library(testthat)

# Indented label column: "Group A" / "Group B" are headers; "  a1" etc. members.
df_indent <- function() {
  data.frame(label = c("Group A", "  a1", "  a2", "Group B", "  b1"),
             v = 1:5, stringsAsFactors = FALSE)
}
df_val <- function() {
  data.frame(g = c("A", "A", "B", "B", "C"), v = 1:5, stringsAsFactors = FALSE)
}

# ── 1. blank_rows_by_change(group_by=) ─────────────────────────────────────

test_that("blank_rows_by_change default group_by is 'value' (backward compatible)", {
  tbl <- rtftable(df_val(), blank_rows = blank_rows_by_change(
    "g", include_before_first = FALSE, include_after_last = FALSE))
  expect_identical(tbl$blank_rows, c(2L, 4L))
})

test_that("blank_rows_by_change(group_by='indent') uses indent detection", {
  val <- rtftable(df_indent(), blank_rows = blank_rows_by_change(
    "label", group_by = "value",
    include_before_first = FALSE, include_after_last = FALSE))$blank_rows
  ind <- rtftable(df_indent(), blank_rows = blank_rows_by_change(
    "label", group_by = "indent",
    include_before_first = FALSE, include_after_last = FALSE))$blank_rows
  expect_identical(val, c(1L, 2L, 3L, 4L))   # every value differs
  expect_identical(ind, 3L)                  # only Group A -> Group B
})

test_that("blank_rows_by_change() rejects a bad group_by", {
  expect_error(blank_rows_by_change("g", group_by = "nope"), "should be one of")
})

# ── 2. between_groups honours group_by ─────────────────────────────────────

test_that("blank_rows = 'between_groups' respects an explicit group_by", {
  val <- as_rtftables(df_indent(), blank_rows = "between_groups",
                      group_col = "label", group_by = "value")[[1L]]$blank_rows
  ind <- as_rtftables(df_indent(), blank_rows = "between_groups",
                      group_col = "label", group_by = "indent")[[1L]]$blank_rows
  expect_identical(val, c(1L, 2L, 3L, 4L))
  expect_identical(ind, 3L)
})

test_that("set_blank_rows(group_by=) threads through", {
  out <- set_blank_rows(df_indent(), blank_rows = "between_groups",
                        group_col = "label", group_by = "indent")
  expect_identical(attr(out, "rtf_blank_rows"), 3L)
})

# ── 3. by_change / by_rule now work inside as_rtftables() ──────────────────

test_that("as_rtftables() accepts blank_rows_by_change() (previously errored)", {
  p <- as_rtftables(df_val(), blank_rows = blank_rows_by_change(
    "g", include_before_first = FALSE, include_after_last = FALSE))
  expect_identical(p[[1L]]$blank_rows, c(2L, 4L))
})

test_that("as_rtftables() accepts blank_rows_by_rule()", {
  p <- as_rtftables(df_indent(), blank_rows = blank_rows_by_rule(
    col = "label", pattern = "^Group", where = "before"))[[1L]]
  # blank before each "Group ..." header row (rows 1 and 4 -> positions 0 and 3)
  expect_true(3L %in% p$blank_rows)
})

test_that("the two paths agree: by_change(group_by=X) == between_groups+group_by=X", {
  for (mode in c("value", "indent")) {
    a <- as_rtftables(df_indent(), blank_rows = blank_rows_by_change(
      "label", group_by = mode,
      include_before_first = FALSE, include_after_last = FALSE))[[1L]]$blank_rows
    b <- as_rtftables(df_indent(), blank_rows = "between_groups",
                      group_col = "label", group_by = mode)[[1L]]$blank_rows
    expect_identical(a, b)
  }
})

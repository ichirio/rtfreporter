# rtftable() construction: defensive-contract / boundary-value / multi-DF tests.
# These exercise public input-validation behaviour, not internals.

df2 <- function() data.frame(a = c("1", "2"), b = c("x", "y"),
                             stringsAsFactors = FALSE)

# ── Argument validation (equivalence partitioning + boundary values) ─────────


test_that('rtftable(border = "none") is accepted and means no borders', {
  none <- rtftable(df2(), border = "none")
  null <- rtftable(df2(), border = NULL)
  expect_s3_class(none, "rtftable")
  expect_null(none$border)                       # no borders
  expect_identical(none$border, null$border)     # same as border = NULL
})

test_that("rtftable() rejects a non-rtf_table_style `style`", {
  expect_error(rtftable(df2(), style = "nope"),
               "must be an rtf_table_style")
})

test_that("rtftable() validates enumerated arguments", {
  expect_error(rtftable(df2(), table_align = "middle"),
               "'left', 'center', or 'right'")
  expect_error(rtftable(df2(), cell_valign = "middle"),
               "'top', 'center', or 'bottom'")
})

test_that("rtftable() requires a scalar-logical row_height_exact", {
  expect_error(rtftable(df2(), row_height_exact = "yes"), "TRUE or FALSE")
  expect_error(rtftable(df2(), row_height_exact = c(TRUE, FALSE)),
               "TRUE or FALSE")
})

test_that("table_width_pct boundary-value analysis", {
  # Valid upper boundary.
  expect_silent(rtftable(df2(), table_width_pct = 100))
  # Just outside the valid range.
  expect_error(rtftable(df2(), table_width_pct = 0),   "\\(0, 100\\]")
  expect_error(rtftable(df2(), table_width_pct = 101), "\\(0, 100\\]")
  # 50% maps to a 0.5 fraction.
  tbl <- rtftable(df2(), table_width_pct = 50)
  expect_equal(tbl$table_width_pct_of_writable, 0.5)
})

test_that("cell_styles must be a list whose length equals the data rows", {
  expect_error(rtftable(df2(), cell_styles = "x"), "must be a list")
  # df2() has 2 rows; a length-1 list is a boundary mismatch.
  expect_error(rtftable(df2(), cell_styles = list(NULL)),
               "must equal the number of data rows")
  expect_silent(rtftable(df2(), cell_styles = list(NULL, NULL)))
})

# ── Multi-DF mode ────────────────────────────────────────────────────────────

test_that("rtftable() builds a multi-DF table and stores data_list", {
  tbl <- rtftable(list(df2(), df2()))
  expect_s3_class(tbl, "rtftable")
  expect_null(tbl$data)
  expect_length(tbl$data_list, 2L)
})

test_that("multi-DF construction validates its inputs", {
  expect_error(rtftable(list(df2(), "x")),    "must be a data.frame")
  expect_error(rtftable(list(df2(),
                             data.frame(a = 1, b = 2, c = 3))),
               "same number of columns")
  expect_error(rtftable(list()),
               "data.frame or a non-empty list")
})

test_that("multi-DF blank_rows accepts integer positions only", {
  # by_change / by_rule specs are unsupported in multi-DF mode.
  expect_error(rtftable(list(df2(), df2()),
                        blank_rows = blank_rows_by_change("a")),
               "integer vector of positions")
  # Out-of-domain integer (< -1) is rejected (boundary just below -1).
  expect_error(rtftable(list(df2(), df2()), blank_rows = -2L),
               "must be -1, 0, or positive")
  # -1 (= after last row) and 0 (= before first) are the valid boundaries.
  expect_silent(rtftable(list(df2(), df2()), blank_rows = c(-1L, 0L, 1L)))
})

# ── row_title + default alignment (#98) ──────────────────────────────────────

df7 <- function() data.frame(Lab = "x", A = "1", B = "2", C = "3",
                             D = "4", E = "5", F = "6", stringsAsFactors = FALSE)
.aligns <- function(tbl) vapply(tbl$col_spec, function(s) s$align, character(1))

test_that("default alignment: first column left, the rest centre (#98)", {
  expect_identical(.aligns(rtftable(df7())),
                   c("left", rep("center", 6L)))
})

test_that("row_title designates the left-aligned heading columns (#98)", {
  expect_identical(.aligns(rtftable(df7(), row_title = c(1, 2))),
                   c("left", "left", rep("center", 5L)))
  # by name
  expect_identical(.aligns(rtftable(df7(), row_title = "Lab")),
                   c("left", rep("center", 6L)))
})

test_that("explicit col_spec align overrides the row_title default (#98)", {
  tbl <- rtftable(df7(), col_spec = list(list(col = 2, align = "right")))
  expect_identical(.aligns(tbl)[2L], "right")
  # column headers follow the data alignment by default
  expect_identical(tbl$col_spec[[2L]]$header_align, "right")
})

test_that("row_title validates its range / names (#98)", {
  expect_error(rtftable(df7(), row_title = 99), "must be within")
  expect_error(rtftable(df7(), row_title = "Nope"), "not found in data")
})


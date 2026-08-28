# Padding and floor for the two halves of a decimal-split column (#304).
#
# The raw glyph counts alone leave the integer half cramped whenever the
# integers are short: a column of `0.0000` values measures 1 against 5 and
# takes one sixth of the column.

ratio <- function(...) rtfreporter:::.decimal_split_ratio(...)

# split point inside a single 1200-twip data column, measured from the
# column's left edge
split_at <- function(...) {
  df <- data.frame(P = c("a", "b"), V = c("0.0000", "0.0125"),
                   stringsAsFactors = FALSE)
  tbl <- rtftable(df, border = "none",
                  column_widths_twips = c(1200L, 1200L)) |>
    set_decimal_split(cols = 2L, ...)
  o   <- rtfreporter:::.render_rtftable(tbl, 13680L)
  hit <- grep(".0000", o, fixed = TRUE, value = TRUE)[1L]
  m   <- regmatches(hit, gregexpr("cellx[0-9]+", hit))[[1L]]
  as.integer(sub("cellx", "", m[2L])) - 1200L
}

# ── the rule ───────────────────────────────────────────────────────────────

test_that("a short integer part holds the 3.5 : 6 baseline", {
  expect_equal(ratio(1L, 5L), 3.5 / 9.5, tolerance = 1e-9)
  expect_equal(ratio(2L, 3L), 3.5 / 9.5, tolerance = 1e-9)
  expect_equal(ratio(3L, 5L), 3.5 / 9.5, tolerance = 1e-9)
})

test_that("anything longer scales from its own measurement plus the padding", {
  expect_equal(ratio(4L, 5L), 4.5 / 10.5, tolerance = 1e-9)
  expect_equal(ratio(3L, 8L), 3.5 / 12.5, tolerance = 1e-9)
  expect_equal(ratio(5L, 2L), 5.5 / 11.5, tolerance = 1e-9)
})

test_that("the floor is applied after the padding, per half", {
  # left floored, right measured
  expect_equal(ratio(1L, 9L), 3.5 / 13.5, tolerance = 1e-9)
  # left measured, right floored
  expect_equal(ratio(9L, 1L), 9.5 / 15.5, tolerance = 1e-9)
})

test_that("pad and floor are configurable", {
  expect_equal(ratio(1L, 5L, pad_chars = c(0, 0), min_chars = c(0, 0)),
               1 / 6, tolerance = 1e-9)
  expect_equal(ratio(1L, 5L, pad_chars = c(1, 0), min_chars = c(0, 0)),
               2 / 7, tolerance = 1e-9)
  expect_equal(ratio(1L, 5L, pad_chars = c(0, 0), min_chars = c(4, 4)),
               4 / 9, tolerance = 1e-9)
})

test_that("the ratio stays inside (0, 1)", {
  for (wl in 1:12) for (wr in 1:12) {
    r <- ratio(wl, wr)
    expect_gt(r, 0)
    expect_lt(r, 1)
  }
})

# ── through the renderer ───────────────────────────────────────────────────

test_that("the default widens the integer half of a `0.0000` column", {
  expect_identical(split_at(), 442L)
})

test_that("pad and floor of zero restore the pre-#304 split point", {
  expect_identical(split_at(pad_chars = c(0, 0), min_chars = c(0, 0)), 200L)
})

test_that("an explicit ratio still overrides the rule", {
  expect_identical(split_at(ratio = 0.25), 300L)
})

# ── the stored metadata ────────────────────────────────────────────────────

test_that("the settings are carried on the table", {
  df  <- data.frame(P = "a", V = "1.5", stringsAsFactors = FALSE)
  tbl <- rtftable(df) |> set_decimal_split(cols = 2L)
  expect_identical(tbl$decimal_split$pad_chars, c(0.5, 1))
  expect_identical(tbl$decimal_split$min_chars, c(3.5, 6))
})

test_that("they survive paginate_cols()", {
  df <- data.frame(P = "a", Q = "b", V1 = "1.5", V2 = "2.5",
                   stringsAsFactors = FALSE)
  pages <- rtftable(df, column_widths_twips = c(1000L, 1000L, 1000L, 1000L)) |>
    set_decimal_split(cols = 3:4, pad_chars = c(1, 2)) |>
    paginate_cols(at = 4L, carry = 1:2)
  expect_identical(pages[[1L]]$decimal_split$pad_chars, c(1, 2))
  expect_identical(pages[[2L]]$decimal_split$pad_chars, c(1, 2))
})

test_that("both arguments are validated", {
  df <- data.frame(P = "a", V = "1.5", stringsAsFactors = FALSE)
  expect_error(set_decimal_split(rtftable(df), cols = 2L, pad_chars = 1),
               "two non-negative numbers")
  expect_error(set_decimal_split(rtftable(df), cols = 2L,
                                 pad_chars = c(-1, 2)),
               "two non-negative numbers")
  expect_error(set_decimal_split(rtftable(df), cols = 2L, min_chars = "x"),
               "two non-negative numbers")
})

test_that("NULL means the documented defaults", {
  expect_equal(ratio(1L, 5L, pad_chars = NULL, min_chars = NULL),
               3.5 / 9.5, tolerance = 1e-9)
})

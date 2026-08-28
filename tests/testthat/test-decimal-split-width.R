# Padding, floor and cap for the two halves of a decimal-split column (#304).
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

# ── the baseline ───────────────────────────────────────────────────────────

test_that("a short column holds the 4 : 6 baseline", {
  expect_equal(ratio(1L, 5L), 0.4, tolerance = 1e-9)   # 0.0000
  expect_equal(ratio(2L, 3L), 0.4, tolerance = 1e-9)   # 12.50
  expect_equal(ratio(3L, 5L), 0.4, tolerance = 1e-9)   # exactly the baseline
  expect_equal(ratio(2L, 5L), 0.4, tolerance = 1e-9)   # <0.500
})

test_that("a longer half scales from its own measurement", {
  expect_equal(ratio(4L, 5L), 5 / 11, tolerance = 1e-9)    # 1104.5
  expect_equal(ratio(4L, 3L), 5 / 11, tolerance = 1e-9)    # 1439.5
  expect_equal(ratio(5L, 2L), 0.5,    tolerance = 1e-9)    # 12345.6
})

test_that("the floor is applied after the padding, per half", {
  expect_equal(ratio(1L, 9L), 4 / 14,  tolerance = 1e-9)   # left floored
  expect_equal(ratio(9L, 1L), 10 / 16, tolerance = 1e-9)   # right floored
})

# ── the cap ────────────────────────────────────────────────────────────────

test_that("past max_chars the raw measured ratio is used", {
  expect_equal(ratio(3L, 8L), 3 / 11, tolerance = 1e-9)    # 123.4567
  expect_equal(ratio(6L, 5L), 6 / 11, tolerance = 1e-9)
  expect_equal(ratio(4L, 7L), 4 / 11, tolerance = 1e-9)
})

test_that("the cap boundary is exclusive -- 10 keeps the allowance", {
  expect_equal(ratio(4L, 6L), 5 / 12, tolerance = 1e-9)    # total 10
  expect_equal(ratio(4L, 7L), 4 / 11, tolerance = 1e-9)    # total 11
})

test_that("max_chars = Inf never drops the allowance", {
  expect_equal(ratio(3L, 8L, max_chars = Inf), 4 / 13, tolerance = 1e-9)
})

test_that("the cap allows an extreme ratio when the content demands it", {
  expect_equal(ratio(1L, 12L), 1 / 13, tolerance = 1e-9)
  expect_equal(ratio(12L, 1L), 12 / 13, tolerance = 1e-9)
})

# ── configurability ────────────────────────────────────────────────────────

test_that("pad, floor and cap are configurable", {
  expect_equal(ratio(1L, 5L, pad_chars = c(0, 0), min_chars = c(0, 0)),
               1 / 6, tolerance = 1e-9)
  expect_equal(ratio(1L, 5L, pad_chars = c(1, 0), min_chars = c(0, 0)),
               2 / 7, tolerance = 1e-9)
  expect_equal(ratio(1L, 5L, pad_chars = c(0, 0), min_chars = c(4, 4)),
               4 / 9, tolerance = 1e-9)
  expect_equal(ratio(1L, 5L, max_chars = 5), 1 / 6, tolerance = 1e-9)
})

test_that("the ratio stays inside (0, 1)", {
  for (wl in 1:14) for (wr in 1:14) {
    r <- ratio(wl, wr)
    expect_gt(r, 0)
    expect_lt(r, 1)
  }
})

# ── through the renderer ───────────────────────────────────────────────────

test_that("the default widens the integer half of a `0.0000` column", {
  expect_identical(split_at(), 480L)
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
  expect_identical(tbl$decimal_split$pad_chars, c(1, 1))
  expect_identical(tbl$decimal_split$min_chars, c(4, 6))
  expect_identical(tbl$decimal_split$max_chars, 10)
})

test_that("they survive paginate_cols()", {
  df <- data.frame(P = "a", Q = "b", V1 = "1.5", V2 = "2.5",
                   stringsAsFactors = FALSE)
  pages <- rtftable(df, column_widths_twips = c(1000L, 1000L, 1000L, 1000L)) |>
    set_decimal_split(cols = 3:4, pad_chars = c(1, 2), max_chars = 20) |>
    paginate_cols(at = 4L, carry = 1:2)
  expect_identical(pages[[1L]]$decimal_split$pad_chars, c(1, 2))
  expect_identical(pages[[2L]]$decimal_split$max_chars, 20)
})

test_that("the arguments are validated", {
  df <- data.frame(P = "a", V = "1.5", stringsAsFactors = FALSE)
  expect_error(set_decimal_split(rtftable(df), cols = 2L, pad_chars = 1),
               "two non-negative numbers")
  expect_error(set_decimal_split(rtftable(df), cols = 2L,
                                 pad_chars = c(-1, 2)),
               "two non-negative numbers")
  expect_error(set_decimal_split(rtftable(df), cols = 2L, min_chars = "x"),
               "two non-negative numbers")
  expect_error(set_decimal_split(rtftable(df), cols = 2L, max_chars = 0),
               "single positive number")
  expect_error(set_decimal_split(rtftable(df), cols = 2L, max_chars = c(1, 2)),
               "single positive number")
})

test_that("NULL means the documented defaults", {
  expect_equal(ratio(1L, 5L, pad_chars = NULL, min_chars = NULL,
                     max_chars = NULL), 0.4, tolerance = 1e-9)
})

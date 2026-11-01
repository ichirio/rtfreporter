# text_width.R -- font-aware character / column-width estimators.

# ──────── text_width_in ───────────────────────────────────────────────────

test_that("text_width_in() returns a numeric the same length as input", {
  out <- text_width_in(c("hello", "world!", ""))
  expect_type(out, "double")
  expect_length(out, 3L)
})

test_that("text_width_in() treats NA as empty string (width 0)", {
  out <- text_width_in(c("abc", NA))
  expect_gt(out[1L], 0)
  expect_identical(out[2L], 0)
})

test_that("text_width_in() scales linearly with character count (Courier)", {
  w1 <- text_width_in("a")
  w5 <- text_width_in("aaaaa")
  expect_equal(w5, 5 * w1, tolerance = 1e-10)
})

test_that("text_width_in() scales with font_size_half_points (larger size -> wider)", {
  w_9pt  <- text_width_in("hello", font_size_half_points = 18L)   # 9pt
  w_12pt <- text_width_in("hello", font_size_half_points = 24L)   # 12pt
  expect_gt(w_12pt, w_9pt)
})

test_that("text_width_in() accepts arial / courier_new / courier", {
  for (f in c("courier_new", "courier", "arial")) {
    expect_type(text_width_in("xy", font = f), "double")
  }
})

test_that("text_width_in() falls back to Courier on unknown font", {
  w_unk <- text_width_in("xyz", font = "nonexistent")
  w_cou <- text_width_in("xyz", font = "courier")
  expect_equal(w_unk, w_cou)
})

# ──────── auto_col_widths ─────────────────────────────────────────────────

test_that("auto_col_widths() returns integer widths in twips, one per column", {
  df <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002"),
    TRT     = c("Placebo", "Active"),
    AGE     = c(45L, 62L),
    stringsAsFactors = FALSE
  )
  w <- auto_col_widths(df)
  expect_type(w, "integer")
  expect_length(w, ncol(df))
  expect_true(all(w > 0L))
})

test_that("auto_col_widths(table_width_twips = N) scales to sum exactly N", {
  df <- data.frame(a = "abc", b = "defg", c = "hij", stringsAsFactors = FALSE)
  w  <- auto_col_widths(df, table_width_twips = 14400L)
  expect_identical(sum(w), 14400L)
})

test_that("auto_col_widths(col_header = ...) includes header text in width", {
  df  <- data.frame(short = "x", long = "y", stringsAsFactors = FALSE)
  w_no_hdr   <- auto_col_widths(df)
  w_with_hdr <- auto_col_widths(df,
                                col_header = c("a very wide header label", "h2"))
  expect_gt(w_with_hdr[1L], w_no_hdr[1L])
})

test_that("auto_col_widths(min_col_width_twips = N) enforces a floor", {
  df <- data.frame(x = "a", stringsAsFactors = FALSE)
  w  <- auto_col_widths(df, min_col_width_twips = 2000L)
  expect_gte(w[1L], 2000L)
})

test_that("auto_col_widths() accepts pipe-delimited col_header string", {
  df <- data.frame(a = "x", b = "y", c = "z", stringsAsFactors = FALSE)
  w  <- auto_col_widths(df, col_header = "A | B | C")
  expect_length(w, 3L)
})

test_that("auto_col_widths() measures the longest line of multi-line cells", {
  df <- data.frame(x = "a", stringsAsFactors = FALSE)
  w_one  <- auto_col_widths(df, col_header = "Xanomeline High Dose")
  w_wrap <- auto_col_widths(df, col_header = "Xanomeline High Dose\nN = 72")
  # The newline version must not be wider: its longest line is the same.
  expect_equal(w_wrap[1L], w_one[1L])
})

test_that("auto_col_widths(protect_cols=) keeps a column at natural width when shrinking", {
  df <- data.frame(
    label = "AMERICAN INDIAN OR ALASKA NATIVE",
    a = "x", b = "y", c = "z", d = "w", stringsAsFactors = FALSE)
  hdr <- c("", "Xanomeline High Dose (N=72)", "Xanomeline Low Dose (N=96)",
           "Placebo (N=86)", "Total (N=306)")
  natural   <- auto_col_widths(df, col_header = hdr)
  protected <- auto_col_widths(df, col_header = hdr,
                               table_width_twips = 9000L, protect_cols = 1L)
  # Forced narrower overall, but column 1 keeps its natural width.
  expect_equal(sum(protected), 9000L)
  expect_equal(protected[1L], natural[1L])
  # The data columns absorbed the squeeze.
  expect_lt(sum(protected[-1L]), sum(natural[-1L]))
})

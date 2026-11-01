# The format area's leaf requirements that had no test (the deviations of
# the first validation version): each test names its FT id and its FRs.

.rounding_msg <- paste0("`rounding` must be \"r\" (half to even, as base::round() does) or \"sas\" ",
                        "(half away from zero, as SAS ROUND() does).")
.na_msg <- paste0("`na` must be a single string -- the text printed for a missing value ",
                  "(e.g. \"-\" or \"NA\").  \"\" (the default) leaves the cell empty.")

test_that("FT-FM087 [FR-FM128 FR-FM174 FR-FM196]: fmt_round(), fmt_numeric() and fmt_count_pct() refuse a rounding other than r or sas; fmt_count_pct() a pct_unit it does not know", {
  d <- data.frame(b = c(1.234, 5.678))
  expect_error(fmt_round(1.5, rounding = "x"), .rounding_msg, fixed = TRUE)
  expect_error(fmt_numeric(d, cols = "b", digits = 1, rounding = "x"), .rounding_msg, fixed = TRUE)
  expect_error(fmt_count_pct(1, 0.5, rounding = "x"), .rounding_msg, fixed = TRUE)
  expect_error(fmt_count_pct(1, 0.5, pct_unit = "x"), "should be one of")
})

test_that("FT-FM088 [FR-FM147]: fmt_signif() refuses a small other than signif or fixed", {
  expect_error(fmt_signif(0.5, small = "x"), "should be one of")
})

test_that("FT-FM089 [FR-FM168 FR-FM172 FR-FM173]: fmt_numeric() refuses several by columns, a column that is not there, and a signif or digits that is not one non-negative integer", {
  d <- data.frame(a = c("x", "y"), b = c(1.234, 5.678), c = c("g", "h"))
  fm <- list(x = list(digits = 1), y = list(digits = 2))
  expect_error(fmt_numeric(d, cols = "b", by = c("a", "c"), formats = fm),
               "`fmt_numeric(by = )` must name a single column.", fixed = TRUE)
  expect_error(fmt_numeric(d, cols = "zz"), "`fmt_numeric(cols)` column 'zz' not found in the table.",
               fixed = TRUE)
  expect_error(fmt_numeric(d, cols = 9), "`fmt_numeric(cols)` index 9 out of range (1..3).", fixed = TRUE)
  expect_error(fmt_numeric(d, cols = "b", by = "zz", formats = fm),
               "`fmt_numeric(by)` column 'zz' not found in the table.", fixed = TRUE)
  expect_error(fmt_numeric(d, cols = "b", by = 9, formats = fm),
               "`fmt_numeric(by)` index 9 out of range (1..3).", fixed = TRUE)
  for (v in list(-1, c(1, 2), "a", NA, 1.5)) {
    expect_error(fmt_numeric(d, cols = "b", digits = v),
                 "`fmt_numeric(digits = )` must be a single non-negative integer.", fixed = TRUE)
    expect_error(fmt_numeric(d, cols = "b", signif = v),
                 "`fmt_numeric(signif = )` must be a single non-negative integer.", fixed = TRUE)
  }
  # a rule's own digits / signif is checked the same way
  expect_error(fmt_numeric(d, cols = "b", by = "a", formats = list(x = list(digits = -1), y = list(digits = 1))),
               "`fmt_numeric(digits = )` must be a single non-negative integer.", fixed = TRUE)
  expect_error(fmt_numeric(d, cols = "b", by = "a", formats = list(x = list(signif = "a"), y = list(digits = 1))),
               "`fmt_numeric(signif = )` must be a single non-negative integer.", fixed = TRUE)
  expect_error(fmt_numeric(d, cols = "b", by = "a", formats = list(x = list(digits = 0.5), y = list(digits = 1))),
               "`fmt_numeric(digits = )` must be a single non-negative integer.", fixed = TRUE)
  # a whole number given as a double is fine
  expect_identical(fmt_numeric(d, cols = "b", digits = 1)$b, c("1.2", "5.7"))
})

test_that("FT-FM098 [FR-FM127 FR-FM146]: fmt_round() and fmt_signif() refuse a fractional digits as they refuse a negative one (#785)", {
  expect_error(fmt_round(1.234, digits = 1.5), "`fmt_round(digits = )` must be a single non-negative integer.",
               fixed = TRUE)
  expect_error(fmt_signif(1.234, digits = 2.5), "`fmt_signif(digits = )` must be a single non-negative integer.",
               fixed = TRUE)
  expect_identical(fmt_round(1.234, digits = 1), "1.2")
})

test_that("FT-FM090 [FR-FM195 FR-FM211 FR-FM239]: fmt_count_pct(), realign_count_pct() and fmt_value_paren() refuse an na that is not one string", {
  expect_error(fmt_count_pct(1, 0.5, na = NA), .na_msg, fixed = TRUE)
  expect_error(fmt_count_pct(1, 0.5, na = c("a", "b")), .na_msg, fixed = TRUE)
  expect_error(suppressWarnings(realign_count_pct("1 (5.0)", na = 1)), .na_msg, fixed = TRUE)
  expect_error(fmt_value_paren("1 (2)", na = c("a", "b")), .na_msg, fixed = TRUE)
})

test_that("FT-FM091 [FR-FM115 FR-FM136 FR-FM133]: fmt_round() keeps 2 decimals and fmt_signif() 3 digits by default; a value below 1 rounding up to 1 is recomputed", {
  expect_identical(fmt_round(1.23456), "1.23")
  expect_identical(fmt_round(1.23456), fmt_round(1.23456, digits = 2L))
  expect_identical(fmt_signif(123.456), "123")
  expect_identical(fmt_signif(0.0123456), fmt_signif(0.0123456, digits = 3L))
  expect_identical(fmt_signif(0.99996, 4), "1.000")
})

test_that("FT-FM092 [FR-FM160]: fmt_numeric(small = ) is signif by default, or fixed, for the signif form and for rules that name none", {
  d <- data.frame(b = c(0.000123, 12.3))
  expect_identical(fmt_numeric(d, cols = "b", signif = 2)$b, c(fmt_signif(0.000123, 2), fmt_signif(12.3, 2)))
  expect_identical(fmt_numeric(d, cols = "b", signif = 2, small = "fixed")$b,
                   c(fmt_signif(0.000123, 2, small = "fixed"), fmt_signif(12.3, 2, small = "fixed")))
  # a rule with no small of its own takes the call's
  r <- data.frame(g = c("x", "y"), b = c(0.000123, 0.000123))
  out <- fmt_numeric(r, cols = "b", by = "g", formats = list(x = list(signif = 2), y = list(signif = 2)),
                     small = "fixed")
  expect_identical(out$b, rep(fmt_signif(0.000123, 2, small = "fixed"), 2L))
})

test_that("FT-FM093 [FR-FM199]: format_count_pct() passes pct_unit, nbsp, pct_sign, na and rounding to fmt_count_pct()", {
  a <- suppressWarnings(format_count_pct(c(1, 10, NA), c(50, 25, NA), pct_unit = "percent", nbsp = " ",
                                         pct_sign = TRUE, na = "-", rounding = "sas"))
  b <- fmt_count_pct(c(1, 10, NA), c(50, 25, NA), pct_unit = "percent", nbsp = " ",
                     pct_sign = TRUE, na = "-", rounding = "sas")
  expect_identical(a, b)
})

test_that("FT-FM094 [FR-FM237 FR-FM243]: fmt_value_paren(nbsp = ) and fmt_right_align(): the padding is non-breaking by default, plain with nbsp = \" \"; every space in a padded cell is replaced", {
  x <- c("1.2 (3.4)", "10.25 (3.4)")
  expect_true(all(grepl(" ", fmt_value_paren(x))))
  plain <- fmt_value_paren(x, nbsp = " ")
  expect_false(any(grepl(" ", plain)))
  expect_identical(gsub(" ", " ", fmt_value_paren(x)), plain)
  # fmt_right_align: a number is text, and the space inside a padded cell goes too
  expect_identical(gsub(" ", "_", fmt_right_align(c(1, 100))), c("__1", "100"))
  expect_identical(gsub(" ", "_", fmt_right_align(c("a b", "long text"))), c("______a_b", "long_text"))
})

test_that("FT-FM095 [FR-FM104 FR-FM113]: round_num(): away from a half the two roundings agree; a fractional digits is cut to an integer", {
  x <- c(1.234, 5.6789, -3.14159, 100.001)
  expect_identical(round_num(x, 2, rounding = "r"), round_num(x, 2, rounding = "sas"))
  expect_identical(round_num(1.2345, 1.9), round_num(1.2345, 1))
  expect_silent(round_num(1.2345, 1.9))
})

test_that("FT-FM096 [FR-FM123 FR-FM163 FR-FM191]: an NA prints the na text in its place, Inf prints as Inf, a zero-length count and pct give zero-length text", {
  expect_identical(fmt_round(c(1.234, NA), na = "-"), c("1.23", "-"))
  out <- fmt_numeric(data.frame(b = c(Inf, -Inf, 1.5)), cols = "b", digits = 1)
  expect_identical(out$b, c("Inf", "-Inf", "1.5"))
  expect_identical(fmt_count_pct(integer(0), numeric(0)), character(0))
})

test_that("FT-FM097 [FR-FM215 FR-FM235]: fmt_count_paren() and fmt_value_paren() return an empty x as it is; numbers are read as text; nested parentheses are left alone", {
  expect_identical(fmt_count_paren(character(0)), character(0))
  expect_identical(fmt_count_paren(c(12, 3)), fmt_count_paren(c("12", "3")))
  expect_identical(fmt_value_paren(character(0)), character(0))
  expect_identical(fmt_value_paren("1.2 (3.4 (5))"), "1.2 (3.4 (5))")
})

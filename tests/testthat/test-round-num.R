# One rounding rule for the whole package (#476).

test_that("round_num() rounds half to even under 'r' and away from zero under 'sas'", {
  x <- c(0.5, 1.5, 2.5, -0.5, -2.5)
  expect_equal(round_num(x, rounding = "r"),   c(0, 2, 2, 0, -2))
  expect_equal(round_num(x, rounding = "sas"), c(1, 2, 3, -1, -3))
  # a binary representation error is absorbed, as SAS's fuzz does
  expect_equal(round_num(2.675, 2, rounding = "sas"), 2.68)
  expect_equal(round_num(c(NA, Inf), 1, rounding = "sas"), c(NA, Inf))
})

test_that("rounding = NULL reads rtfreporter.rounding, and an explicit value wins", {
  withr::local_options(rtfreporter.rounding = NULL)
  expect_equal(round_num(2.5), 2)                       # factory default "r"
  withr::local_options(rtfreporter.rounding = "sas")
  expect_equal(round_num(2.5), 3)
  expect_equal(round_num(2.5, rounding = "r"), 2)
  expect_identical(rtfreporter_options()$rtfreporter.rounding, "sas")
})

test_that("round_num() refuses a bad family, bad digits and non-numeric input", {
  expect_error(round_num(1, rounding = "excel"), "must be \"r\"")
  expect_error(round_num(1, digits = -1), "non-negative")
  expect_error(round_num("1"), "numeric input")
  withr::local_options(rtfreporter.rounding = "SAS")
  expect_error(round_num(1), "must be \"r\"")
})

test_that("every formatter follows the option", {
  withr::local_options(rtfreporter.rounding = "sas")
  expect_identical(fmt_round(23.445, 2), "23.45")
  expect_identical(fmt_signif(23.445, 4), "23.45")
  expect_identical(fmt_numeric(data.frame(a = 23.445), "a", digits = 2)$a,
                   "23.45")
  expect_identical(format_count_pct(1L, 1 / 16, nbsp = " "), "  1  (6.3)")
  withr::local_options(rtfreporter.rounding = "r")
  expect_identical(fmt_round(23.445, 2), "23.44")
  expect_identical(format_count_pct(1L, 1 / 16, nbsp = " "), "  1  (6.2)")
  expect_identical(format_count_pct(1L, 1 / 16, nbsp = " ", rounding = "sas"),
                   "  1  (6.3)")
})

test_that("format_count_pct() picks its width from the ROUNDED percent", {
  # 9.96 prints as 10.0, so it takes the two-digit width, not the < 10 one
  expect_identical(format_count_pct(5L, 9.96, pct_unit = "percent", nbsp = " "),
                   format_count_pct(5L, 10,   pct_unit = "percent", nbsp = " "))
  # 99.96 prints as 100, the whole-number branch
  expect_identical(format_count_pct(5L, 99.96, pct_unit = "percent", nbsp = " "),
                   "  5  (100)")
})

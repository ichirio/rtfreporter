# Tests for the numeric -> character display formatters.

# ──────── fmt_signif(): the specified rule ─────────────────────────────────

test_that("fmt_signif() counts total digits including the integer part", {
  expect_equal(fmt_signif(c(0, 10.2, 103.4, 20.333333, 23.4463), digits = 4),
               c("0.000", "10.20", "103.4", "20.33", "23.45"))
})

test_that("fmt_signif() keeps trailing zeros", {
  expect_equal(fmt_signif(10.2, 4), "10.20")
  expect_equal(fmt_signif(5, 4),    "5.000")
  expect_equal(fmt_signif(500, 4),  "500.0")
})

test_that("an integer part longer than digits prints whole", {
  # integer digits are never dropped; decimals clamp at zero
  expect_equal(fmt_signif(12345.6, 4),  "12346")
  expect_equal(fmt_signif(987654.3, 4), "987654")
})

test_that("decimals are recomputed when rounding grows the integer part", {
  expect_equal(fmt_signif(99.995, 4), "100.0")   # not "100.00"
  expect_equal(fmt_signif(9.9999, 4), "10.00")   # not "10.000"
})

test_that("negative numbers count the digits of the magnitude", {
  expect_equal(fmt_signif(-20.333, 4), "-20.33")
  expect_equal(fmt_signif(-0.5, 4),    "-0.5000")
})

# ──────── values below 1 ───────────────────────────────────────────────────

test_that("small = 'signif' counts from the first significant digit", {
  expect_equal(fmt_signif(c(0.333333, 0.0004567, 0.00998), 4),
               c("0.3333", "0.0004567", "0.009980"))
})

test_that("small = 'fixed' applies the integer-counting rule throughout", {
  expect_equal(fmt_signif(c(0.333333, 0.0004567, 0.00998), 4, small = "fixed"),
               c("0.333", "0.000", "0.010"))
})

test_that("zero prints digits - 1 decimals under both", {
  expect_equal(fmt_signif(0, 4), "0.000")
  expect_equal(fmt_signif(0, 4, small = "fixed"), "0.000")
  expect_equal(fmt_signif(0, 1), "0")
})

# ──────── rounding mode ────────────────────────────────────────────────────

test_that("rounding = 'r' is base::round (banker's)", {
  expect_equal(fmt_round(23.445, 2), "23.44")
  expect_equal(fmt_round(2.675, 2),  "2.67")
  expect_equal(fmt_round(0.125, 2),  "0.12")
})

test_that("rounding = 'sas' rounds half away from zero", {
  expect_equal(fmt_round(23.445, 2, "sas"), "23.45")
  expect_equal(fmt_round(2.675, 2, "sas"),  "2.68")
  expect_equal(fmt_round(0.125, 2, "sas"),  "0.13")
  # ... including through a binary representation that sits just below the half
  expect_equal(fmt_round(-2.675, 2, "sas"), "-2.68")
})

test_that("the two modes agree away from exact halves", {
  x <- c(1.234, 5.6789, -3.14159, 100.001)
  expect_equal(fmt_round(x, 2), fmt_round(x, 2, "sas"))
})

# ──────── fmt_round() ──────────────────────────────────────────────────────

test_that("fmt_round() prints every requested decimal", {
  expect_equal(fmt_round(c(2.5, 20.333, 100), 2), c("2.50", "20.33", "100.00"))
  expect_equal(fmt_round(2.5, 0), "2")           # banker's: 2.5 -> 2
  expect_equal(fmt_round(2.5, 0, "sas"), "3")
})

# ──────── missing and non-finite ───────────────────────────────────────────

test_that("NA and NaN both become the na string", {
  expect_equal(fmt_signif(c(NA, NaN), 4), c("", ""))
  expect_equal(fmt_round(c(NA, NaN), 2), c("", ""))
  expect_equal(fmt_signif(c(NA, NaN), 4, na = "NE"), c("NE", "NE"))
})

test_that("Inf passes through", {
  expect_equal(fmt_signif(c(Inf, -Inf), 4), c("Inf", "-Inf"))
  expect_equal(fmt_round(Inf, 2), "Inf")
})

# ──────── independence from session options ────────────────────────────────

test_that("output does not depend on options(scipen) or options(digits)", {
  # as.character(123456) is "1.23456e+05" under scipen = -100 -- the defect
  # these functions exist to remove
  op <- options(scipen = -100, digits = 3)
  on.exit(options(op), add = TRUE)
  expect_equal(fmt_signif(123456, 6), "123456")
  expect_equal(fmt_round(1e5, 1),     "100000.0")
  expect_equal(fmt_signif(1e-4, 4),   "0.0001000")

  options(scipen = 100, digits = 15)
  expect_equal(fmt_signif(123456, 6), "123456")
  expect_equal(fmt_round(1e5, 1),     "100000.0")
})

test_that("scientific notation never appears", {
  x <- c(1e5, 1e-4, 1e15, 1e-15)
  expect_false(any(grepl("e", fmt_round(x, 3), fixed = TRUE)))
  expect_false(any(grepl("e", fmt_signif(x, 4), fixed = TRUE)))
})

# ──────── argument checking ────────────────────────────────────────────────

test_that("character input is refused", {
  expect_error(fmt_signif("1.5"), "numeric input")
  expect_error(fmt_round("1.5"),  "numeric input")
})

test_that("digits must be a single non-negative integer", {
  expect_error(fmt_signif(1, -1),      "non-negative")
  expect_error(fmt_round(1, c(1, 2)),  "non-negative")
  expect_error(fmt_signif(1, "four"),  "non-negative")
})

# ──────── fmt_numeric(): whole-column form ─────────────────────────────────

.df <- function() {
  data.frame(stat = c("n", "Mean", "SD"),
             trt  = c(24, 902.3312, 230.1234),
             note = c("a", "b", "c"),
             stringsAsFactors = FALSE)
}

test_that("fmt_numeric() formats the selected numeric column", {
  out <- fmt_numeric(.df(), cols = "trt", signif = 4)
  expect_equal(out$trt, c("24.00", "902.3", "230.1"))
  expect_type(out$trt, "character")
})

test_that("fmt_numeric() leaves other columns and their order alone", {
  out <- fmt_numeric(.df(), cols = "trt", signif = 4)
  expect_equal(names(out), names(.df()))
  expect_equal(out$stat, .df()$stat)
  expect_equal(out$note, .df()$note)
  expect_equal(nrow(out), nrow(.df()))
})

test_that("a selected column that is not numeric is left alone", {
  out <- fmt_numeric(.df(), cols = c("trt", "note"), signif = 4)
  expect_equal(out$note, .df()$note)
  expect_equal(out$trt, c("24.00", "902.3", "230.1"))
})

test_that("fmt_numeric() with no numeric column among cols is a no-op", {
  expect_identical(fmt_numeric(.df(), cols = "note", signif = 4), .df())
})

test_that("fmt_numeric() accepts digits as well as signif", {
  expect_equal(fmt_numeric(.df(), cols = "trt", digits = 1)$trt,
               c("24.0", "902.3", "230.1"))
})

test_that("fmt_numeric() insists on exactly one of signif / digits", {
  expect_error(fmt_numeric(.df(), cols = "trt"), "exactly one")
  expect_error(fmt_numeric(.df(), cols = "trt", signif = 4, digits = 2),
               "exactly one")
})

# ──────── fmt_numeric(): the carrier column ────────────────────────────────

.stub_df <- function() {
  data.frame(
    `Nominal Time (h)` = c("0.5 h", "  n", "  Mean", "  CV%"),
    Day1 = c(NA, 24, 902.3312, 15.4321),
    Day2 = c(NA, 24, 88.0125, 19.8765),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

test_that("the carrier may be the row-heading column, matched after trimming", {
  out <- fmt_numeric(.stub_df(), cols = c("Day1", "Day2"),
                     by = "Nominal Time (h)",
                     formats = list(n = list(digits = 0),
                                    Mean = list(signif = 4),
                                    `CV%` = list(digits = 1)))
  expect_equal(out$Day1, c("", "24", "902.3", "15.4"))
  expect_equal(out$Day2, c("", "24", "88.01", "19.9"))
  # the carrier column itself is untouched, indent and all
  expect_equal(out$`Nominal Time (h)`, .stub_df()$`Nominal Time (h)`)
})

test_that("a row whose selected cells are all missing needs no rule", {
  # the "0.5 h" group label has no format entry and must not error
  out <- fmt_numeric(.stub_df(), cols = "Day1", by = "Nominal Time (h)",
                     formats = list(n = list(digits = 0),
                                    Mean = list(signif = 4),
                                    `CV%` = list(digits = 1)))
  expect_equal(out$Day1[1L], "")
})

test_that("an unmatched key with a non-missing cell is an error", {
  expect_error(
    fmt_numeric(.stub_df(), cols = "Day1", by = "Nominal Time (h)",
                formats = list(n = list(digits = 0))),
    "no format for")
})

test_that(".default covers the unmatched keys", {
  out <- fmt_numeric(.stub_df(), cols = "Day1", by = "Nominal Time (h)",
                     formats = list(n = list(digits = 0),
                                    .default = list(signif = 4)))
  expect_equal(out$Day1, c("", "24", "902.3", "15.43"))
})

test_that("a rule may override `small`", {
  d <- data.frame(k = c("a", "b"), v = c(0.0004567, 0.0004567),
                  stringsAsFactors = FALSE)
  out <- fmt_numeric(d, cols = "v", by = "k",
                     formats = list(a = list(signif = 4),
                                    b = list(signif = 4, small = "fixed")))
  expect_equal(out$v, c("0.0004567", "0.000"))
})

test_that("carrier form validates its arguments", {
  d <- .stub_df()
  expect_error(fmt_numeric(d, cols = "Day1", by = "Nominal Time (h)",
                           formats = list(n = list())),
               "exactly one")
  expect_error(fmt_numeric(d, cols = "Day1", by = "Nominal Time (h)",
                           formats = list(n = list(signif = 4, digits = 2))),
               "exactly one")
  expect_error(fmt_numeric(d, cols = "Day1", by = "Nominal Time (h)",
                           formats = list(n = 4)),
               "must be a list")
  expect_error(fmt_numeric(d, cols = "Day1", by = "Nominal Time (h)",
                           signif = 4, formats = list(n = list(digits = 0))),
               "not both")
  expect_error(fmt_numeric(d, cols = "Day1", by = "Nominal Time (h)"),
               "named `formats` list")
})

test_that("fmt_numeric() needs a data frame", {
  expect_error(fmt_numeric(1:3, cols = 1, signif = 4), "data frame")
})

# ──────── it composes with the renderer ────────────────────────────────────

test_that("formatting first makes the rendered text deterministic", {
  op <- options(scipen = -100)
  on.exit(options(op), add = TRUE)
  d <- data.frame(lab = "x", v = 123456)

  raw <- rtfreporter:::.render_rtftable(rtftable(d, border = "none"), 9360L)
  expect_true(any(grepl("1.23456e+05", raw, fixed = TRUE)))   # the defect

  fmt <- rtfreporter:::.render_rtftable(
    rtftable(fmt_numeric(d, cols = "v", digits = 0), border = "none"), 9360L)
  expect_false(any(grepl("e+05", fmt, fixed = TRUE)))
  expect_true(any(grepl("123456", fmt, fixed = TRUE)))
})

test_that("the formatted text is what set_decimal_split() splits on", {
  d   <- data.frame(lab = c("a", "b"), v = c(1 / 3, 12.5))
  tbl <- rtftable(fmt_numeric(d, cols = "v", signif = 4), border = "none") |>
    set_decimal_split(cols = "v")
  cx   <- rtfreporter:::.compute_cellx(2L, 9360L, tbl)
  plan <- rtfreporter:::.decimal_split_plan(tbl, cx, tbl$col_spec, "script")
  # "0.3333" / "12.50" -- not the 15-digit as.character() string
  expect_equal(plan$parts[[2L]]$left,  c("0", "12"))
  expect_equal(plan$parts[[2L]]$right, c(".3333", ".50"))
})

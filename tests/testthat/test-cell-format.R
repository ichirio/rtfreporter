# Pluggable cell-format functions: fmt_right_align(), fmt_count_paren(),
# and the as_rtftables(cell_format = ) wiring.

NBSP <- intToUtf8(160L)
unbsp <- function(x) gsub(NBSP, " ", x, fixed = TRUE)   # nbsp -> space for asserts

test_that("fmt_right_align right-justifies non-empty cells, leaves blanks", {
  out <- fmt_right_align(c("5", "120", "7", ""))
  expect_identical(unbsp(out), c("  5", "120", "  7", ""))
  expect_length(out, 4L)
})

test_that("fmt_count_paren aligns only parenthetical cells; bare counts untouched", {
  out <- unbsp(fmt_count_paren(c("1 (1.2%)", "0", "11 (3.6%)", "108 (35.3%)")))
  # counts right-justified in a 3-wide field, percentages right-justified inside
  # the parentheses (so decimals line up).  The lone "0" has no parentheses,
  # so it is returned UNCHANGED (not padded).
  expect_identical(out, c("  1 ( 1.2%)", "0",
                          " 11 ( 3.6%)", "108 (35.3%)"))
  # the three parenthetical cells share one width
  expect_true(all(nchar(out[c(1, 3, 4)]) == nchar(out[1L])))
})

test_that("fmt_count_paren_bare also pads a bare lone count", {
  out <- unbsp(fmt_count_paren_bare(c("1 (1.2%)", "0", "11 (3.6%)", "108 (35.3%)")))
  expect_identical(out, c("  1 ( 1.2%)", "  0        ",
                          " 11 ( 3.6%)", "108 (35.3%)"))
  expect_true(all(nchar(out) == nchar(out[1L])))   # every cell same width
})

test_that("fmt_count_paren copes with mixed tfrmt notations", {
  out <- unbsp(fmt_count_paren(c("2 ( 2.8%)", "70 (100%)", "3 (<1%)")))
  expect_true(all(nchar(out) == nchar(out[1L])))   # equal width -> aligned
})

test_that("fmt_count_paren leaves non-count and bare-count cells unchanged", {
  expect_identical(fmt_count_paren(c("Mean (SD)", "n/a", "", "0", "75.2 (8.6)")),
                   c("Mean (SD)", "n/a", "", "0", "75.2 (8.6)"))
})

test_that("as_rtftables(cell_format = fn) applies to data columns only", {
  df <- data.frame(lab = c("A", "B"), x = c("1 (1.2%)", "3 (9.9%)"),
                   stringsAsFactors = FALSE)
  p <- as_rtftables(df, cell_format = fmt_count_paren)[[1L]]
  expect_identical(p$data[[1L]], c("A", "B"))            # col 1 untouched
  expect_true(all(nchar(p$data[[2L]]) == nchar(p$data[[2L]][1L])))
})

test_that("as_rtftables(cell_format = list(...)) targets columns positionally", {
  df <- data.frame(a = c("1 (1.2%)", "0"), b = c("5", "120"),
                   stringsAsFactors = FALSE)
  p <- as_rtftables(df, cell_format = list(NULL, fmt_right_align))[[1L]]
  expect_identical(p$data[[1L]], c("1 (1.2%)", "0"))     # col 1 untouched
  expect_identical(unbsp(p$data[[2L]]), c("  5", "120")) # col 2 right-aligned
})

test_that("cell_format takes precedence over align_count_pct", {
  df <- data.frame(lab = c("A"), x = c("5 (5.0)"), stringsAsFactors = FALSE)
  p <- as_rtftables(df, align_count_pct = TRUE,
                    cell_format = fmt_right_align)[[1L]]
  # fmt_right_align keeps the content (just nbsp-pads); the count-pct realigner
  # would have widened it.  So the un-nbsp'd value is the original.
  expect_identical(unbsp(p$data[[2L]]), "5 (5.0)")
})

test_that("cell_format function returning wrong length errors", {
  df <- data.frame(lab = "A", x = "1", stringsAsFactors = FALSE)
  expect_error(as_rtftables(df, cell_format = function(x) character(0)),
               "same length")
})


# ---------------------------------------------------------------------------
# na = : the text printed for a missing value (#350)
# ---------------------------------------------------------------------------

test_that("na = '' keeps the previous behaviour: missing cells are empty", {
  expect_identical(unbsp(fmt_count_paren(c("1 (1.2%)", NA, "108 (35.3%)"))),
                   c("  1 ( 1.2%)", "", "108 (35.3%)"))
  expect_identical(unbsp(fmt_right_align(c("5", "120", NA))),
                   c("  5", "120", ""))
})

test_that("fmt_count_paren right-justifies the na token in the count field", {
  out <- unbsp(fmt_count_paren(c("1 (1.2%)", NA, "108 (35.3%)"), na = "-"))
  expect_identical(out, c("  1 ( 1.2%)", "  -        ", "108 (35.3%)"))
  # the token's right edge lands on the ones digit, and the cell keeps the
  # column's full width
  expect_identical(substr(out[2L], 3L, 3L), "-")
  expect_true(all(nchar(out) == nchar(out[1L])))
})

test_that("a two-character na token still ends on the ones digit", {
  out <- unbsp(fmt_count_paren(c("1 (1.2%)", NA, "108 (35.3%)"), na = "NE"))
  expect_identical(substr(out[2L], 2L, 3L), "NE")
  expect_true(all(nchar(out) == nchar(out[1L])))
})

test_that("a na token wider than every count widens the count field", {
  out <- unbsp(fmt_count_paren(c("1 (1.2%)", NA, "9 (3.6%)"), na = "N/A"))
  expect_identical(out, c("  1 (1.2%)", "N/A       ", "  9 (3.6%)"))
  expect_true(all(nchar(out) == nchar(out[1L])))
})

test_that("a cell that already holds the token is aligned like a missing one", {
  # as_rtftables() substitutes first and formats second, so the aligner only
  # ever sees the token as ordinary text -- this is that path.
  out <- unbsp(fmt_count_paren(c("1 (1.2%)", "-", "108 (35.3%)"), na = "-"))
  expect_identical(out[2L], "  -        ")
})

test_that("text that is not missing is still returned unchanged and unpadded", {
  out <- fmt_count_paren(c("1 (1.2%)", "NE", "n/a", "75.2 (8.6)"), na = "-")
  expect_identical(out[2:4], c("NE", "n/a", "75.2 (8.6)"))
})

test_that("fmt_right_align right-justifies the na token with the rest", {
  expect_identical(unbsp(fmt_right_align(c("5", "120", NA), na = "-")),
                   c("  5", "120", "  -"))
})

test_that("fmt_count_paren_bare takes na too", {
  out <- unbsp(fmt_count_paren_bare(c("1 (1.2%)", "0", NA), na = "-"))
  expect_identical(out, c("1 (1.2%)", "0       ", "-       "))
})

test_that("na must be a single string", {
  expect_error(fmt_count_paren("1 (1.2%)", na = NA), "single string")
  expect_error(fmt_count_paren("1 (1.2%)", na = c("-", "x")), "single string")
  expect_error(fmt_right_align("5", na = 1L), "single string")
})

test_that(".call_cell_format only passes na to functions that declare it", {
  seen <- NULL
  f_no <- function(x) { seen <<- "no"; x }
  f_yes <- function(x, na = "") { seen <<- na; x }
  rtfreporter:::.call_cell_format(f_no, "a", "-")
  expect_identical(seen, "no")
  rtfreporter:::.call_cell_format(f_yes, "a", "-")
  expect_identical(seen, "-")
})

# ──────── fmt_value_paren(): text values, ones-digit parentheticals ─────────

test_that("fmt_value_paren aligns text values and pads bare cells", {
  out <- unbsp(fmt_value_paren(c("86", "12 (14.0)", "1 (<1)", "n=3 (3.5)")))
  expect_identical(out, c(" 86       ", " 12 (14.0)",
                          "  1   (<1)", "n=3  (3.5)"))
  expect_true(all(nchar(out) == nchar(out[1L])))   # one column width
})

test_that("fmt_value_paren follows the original parenthesis spec", {
  # The maintainer's case (#421): the padding falls BEFORE the "(", a zero
  # count drops its parenthetical, and "(100)" is flush against its ")".
  out <- unbsp(fmt_value_paren(c("12 (100)", "6 (50.0)", "1 (8.3)", "0 (0.0)")))
  expect_identical(out, c("12  (100)", " 6 (50.0)", " 1  (8.3)", " 0       "))
  expect_true(all(nchar(out) == nchar(out[1L])))
  # every ")" in the same column, and no padding inside the parentheses
  expect_identical(unique(regexpr(")", out[1:3], fixed = TRUE)), 9L)
  expect_false(any(grepl("( ", out, fixed = TRUE)))   # nothing after "("
  expect_false(any(grepl(" )", out, fixed = TRUE)))   # nothing before ")"
  # the decimal cells line up on the point (and so on the ones digit); the
  # decimal-less "(100)" does not, which is the point of the rule
  expect_identical(unique(regexpr(".", out[2:3], fixed = TRUE)), 7L)
})

test_that("fmt_value_paren keeps a parenthetical that is not all zeros", {
  out <- unbsp(fmt_value_paren(c("5 (12.5)", "0 (BLQ)", "0 (0.0)")))
  expect_identical(out, c("5 (12.5)", "0  (BLQ)", "0       "))
})

test_that("fmt_value_paren right-justifies a parenthetical with no digits", {
  expect_identical(unbsp(fmt_value_paren(c("5 (n/a)", "120 (ND)"))),
                   c("  5 (n/a)", "120  (ND)"))
})

test_that("fmt_value_paren leaves cells it cannot parse unchanged", {
  x   <- c("12 (14.0)", "", "Mean (SD) by visit", "(100%)", "  ")
  out <- unbsp(fmt_value_paren(x))
  expect_identical(out[2:5], x[2:5])      # empty / trailing text / no value
})

test_that("fmt_value_paren lines the na token up with the values", {
  out <- unbsp(fmt_value_paren(c("1 (1.2%)", NA, "108 (35.3%)"), na = "-"))
  expect_identical(out, c("  1  (1.2%)", "  -        ", "108 (35.3%)"))
})

test_that("fmt_value_paren copes with a column of bare values only", {
  expect_identical(unbsp(fmt_value_paren(c("5", "120", "7", ""))),
                   c("  5", "120", "  7", ""))
})

# ──────── naming a built-in ────────────────────────────────────────────────

test_that("cell_format accepts a built-in's name, with or without the prefix", {
  x <- c("86", "12 (14.0)")
  expect_identical(rtfreporter:::.builtin_cell_format("value_paren"),
                   fmt_value_paren)
  expect_identical(rtfreporter:::.builtin_cell_format("fmt_value_paren"),
                   fmt_value_paren)
  expect_identical(rtfreporter:::.builtin_cell_format("count_pct"),
                   realign_count_pct)
  df <- data.frame(lab = c("A", "B"), x = x, stringsAsFactors = FALSE)
  expect_identical(as_rtftables(df, cell_format = "value_paren")[[1L]]$data$x,
                   as_rtftables(df, cell_format = fmt_value_paren)[[1L]]$data$x)
})

test_that("a list of cell formats may name built-ins", {
  df <- data.frame(a = c("1 (1.2%)", "0"), b = c("5", "120"),
                   stringsAsFactors = FALSE)
  p <- as_rtftables(df, cell_format = list(NULL, "right_align"))[[1L]]
  expect_identical(p$data[[1L]], c("1 (1.2%)", "0"))     # col 1 untouched
  expect_identical(unbsp(p$data[[2L]]), c("  5", "120"))
})

test_that("an unknown built-in name errors and lists the built-ins", {
  df <- data.frame(lab = "A", x = "1 (1.2%)", stringsAsFactors = FALSE)
  expect_error(as_rtftables(df, cell_format = "nope"),
               "not a built-in cell format")
  expect_error(as_rtftables(df, cell_format = "nope"), "value_paren")
  expect_error(as_rtftables(df, cell_format = list(NULL, "nope")),
               "not a built-in cell format")
  expect_error(as_rtftables(df, cell_format = TRUE), "must be a function")
})

test_that("align_count_pct takes a name or a function as well as TRUE", {
  df <- data.frame(lab = c("A", "B"), x = c("86", "12 (14.0)"),
                   stringsAsFactors = FALSE)
  expect_identical(as_rtftables(df, align_count_pct = "value_paren")[[1L]]$data$x,
                   as_rtftables(df, cell_format = fmt_value_paren)[[1L]]$data$x)
  expect_identical(as_rtftables(df, align_count_pct = fmt_right_align)[[1L]]$data$x,
                   as_rtftables(df, cell_format = fmt_right_align)[[1L]]$data$x)
})

test_that("align_count_pct = TRUE / FALSE keep their old meaning", {
  df <- data.frame(lab = c("A", "B"), x = c("5 (5.0)", "12 (100.0)"),
                   stringsAsFactors = FALSE)
  expect_identical(as_rtftables(df, align_count_pct = TRUE)[[1L]]$data$x,
                   realign_count_pct(df$x))
  expect_identical(as_rtftables(df, align_count_pct = FALSE)[[1L]]$data$x, df$x)
})

test_that("align_count_pct rejects what is neither a switch nor a format", {
  df <- data.frame(lab = "A", x = "1 (1.2%)", stringsAsFactors = FALSE)
  expect_error(as_rtftables(df, align_count_pct = 3), "align_count_pct")
  expect_error(as_rtftables(df, align_count_pct = "nope"),
               "not a built-in cell format")
})

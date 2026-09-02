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

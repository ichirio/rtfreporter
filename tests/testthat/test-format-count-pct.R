# format_count_pct() / realign_count_pct() — uniform "n (xx.x)" widths.

NBSP <- intToUtf8(160L)
unbsp <- function(x) gsub(NBSP, " ", x, fixed = TRUE)

# ──────── format_count_pct: numeric inputs ───────────────────────────────

test_that("format_count_pct() produces 10-char-wide strings (NBSP padded)", {
  # Fractions (default)
  out <- format_count_pct(c(5L, 14L, 30L), c(0.053, 0.500, 1.000))
  expect_identical(nchar(out), c(10L, 10L, 10L))
})

test_that("format_count_pct() respects pct_unit = 'percent'", {
  out <- format_count_pct(c(5L, 14L, 30L), c(5, 50, 100),
                           pct_unit = "percent")
  expect_identical(nchar(out), c(10L, 10L, 10L))
  expect_match(out[3L], "100")             # 100% rendered without decimals
})

test_that("100% branch right-aligns the closing paren (width 10, ends with ')')", {
  # Use plain spaces so we can eyeball the result; the alignment is
  # independent of nbsp.
  out <- format_count_pct(c(5L, 14L, 30L), c(5, 50, 100),
                           pct_unit = "percent",
                           nbsp     = " ")
  expect_identical(out, c("  5  (5.0)", " 14 (50.0)", " 30  (100)"))
  # Every paren-bearing row ends with ')':
  expect_true(all(endsWith(out, ")")))
  # The ')' sits at column 10 on every row:
  expect_identical(regexpr("\\)", out, perl = TRUE), c(10L, 10L, 10L),
                   ignore_attr = TRUE)
})

test_that("format_count_pct() handles count = 0 (count-only branch)", {
  out_0 <- format_count_pct(0L, 0.0)
  expect_identical(nchar(out_0), 10L)
  # No parenthesis on the zero branch
  expect_false(grepl("\\(", out_0))
})

test_that("format_count_pct() prints the na token for a missing count (#350)", {
  # Was " NA       ", a side effect of sprintf("%3d", NA_integer_) rather than
  # anything anyone asked for.  The default is now an empty cell.
  expect_identical(format_count_pct(NA_integer_, NA_real_), "")
  expect_identical(format_count_pct(NaN, NaN), "")
  out <- unbsp(format_count_pct(NA_integer_, NA_real_, na = "-"))
  expect_identical(out, "  -       ")      # right edge on the ones digit
  expect_identical(nchar(out), 10L)
  expect_identical(nchar(unbsp(format_count_pct(NA_integer_, NA_real_,
                                                na = "-", pct_sign = TRUE))),
                   11L)
})

test_that("format_count_pct() keeps a real count whose percent is missing", {
  # A missing percent is not missing data -- the count still prints on its own.
  expect_identical(unbsp(format_count_pct(5L, NA_real_, na = "-")), "  5       ")
  expect_identical(unbsp(format_count_pct(5L, Inf,      na = "-")), "  5       ")
})

test_that("format_count_pct() shows Inf rather than hiding it as missing", {
  # An infinite count means a division by zero upstream; "-" would hide it.
  expect_identical(unbsp(format_count_pct(Inf, 0.5, na = "-")), "Inf       ")
})

test_that("format_count_pct() picks the < 10 vs >= 10 branch correctly", {
  small <- format_count_pct(1L, 5,    pct_unit = "percent")
  big   <- format_count_pct(7L, 33.3, pct_unit = "percent")
  expect_match(small, "\\(5\\.0\\)")        # one-digit pct -> X.Y
  expect_match(big,   "\\(33\\.3\\)")        # two-digit pct -> XX.Y
})

test_that("format_count_pct() supports plain spaces via nbsp = ' '", {
  out <- format_count_pct(7L, 0.333, nbsp = " ")
  expect_match(out, "  7 \\(33\\.3\\)")     # padding is regular spaces
})

test_that("format_count_pct() recycles length-1 against the other vector", {
  out <- format_count_pct(c(1L, 2L, 3L), 0.5)        # pct recycled
  expect_length(out, 3L)
  out <- format_count_pct(2L, c(0.10, 0.50, 0.95))   # count recycled
  expect_length(out, 3L)
})

test_that("format_count_pct() rejects mismatched non-recyclable lengths", {
  expect_error(format_count_pct(1:3, c(0.1, 0.2)), "same length")
})

# ──────── realign_count_pct: string inputs ────────────────────────────────

test_that("realign_count_pct() reformats matching cells; passes through others", {
  inp <- c("5 (33.3)", "12 (100.0)", "0 (0.0)", "not a count", "1 (5.0)")
  out <- realign_count_pct(inp)
  # Lengths uniform for matching cells
  matching_idx <- c(1L, 2L, 3L, 5L)
  expect_identical(nchar(out[matching_idx]),
                   rep(10L, length(matching_idx)))
  # Non-matching cell passes through
  expect_identical(out[4L], "not a count")
})

test_that("realign_count_pct() handles empty / NULL / non-character input gracefully", {
  expect_identical(realign_count_pct(character(0)), character(0))
  expect_null(realign_count_pct(NULL))
  # Numeric input is coerced
  expect_identical(realign_count_pct(c(1L, 2L)), c("1", "2"))
})

# ──────── paginate(align_count_pct = TRUE) integration ────────────────────

test_that("paginate(align_count_pct = TRUE) realigns columns 2..N", {
  df <- data.frame(
    label = c("Sex", "  Female", "  Male"),
    a     = c("",        "16 (53.3)",  "14 (46.7)"),
    b     = c("",        "1 (5.0)",    "12 (100.0)"),
    stringsAsFactors = FALSE
  )
  pages <- paginate(df, align_count_pct = TRUE)
  out_a <- pages[[1L]]$a
  # Count-percent cells are aligned: within a column every non-empty cell has
  # the same (content-adaptive) width, the count right-justified.
  non_empty <- nzchar(out_a)
  expect_equal(length(unique(nchar(out_a[non_empty]))), 1L)
  expect_true(grepl(")$", out_a[2L]))
})

test_that("paginate(align_count_pct = FALSE) leaves columns untouched (default)", {
  df <- data.frame(
    label = c("Sex", "  F"),
    a     = c("",    "5 (33.3)"),
    stringsAsFactors = FALSE
  )
  pages <- paginate(df)                    # default = FALSE
  expect_identical(pages[[1L]]$a[2L], "5 (33.3)")
})

test_that(".realign_count_pct_df() leaves an integer-only column untouched (#80)", {
  # A plain count column with no "n (xx.x)" cells must NOT be padded:
  # values like "3" must stay flush-left with no leading spaces inserted.
  df <- data.frame(
    label = c("Group", "  A", "  B"),
    n     = c("",      "3",   "12"),
    stringsAsFactors = FALSE
  )
  out <- rtfreporter:::.realign_count_pct_df(df, nbsp = " ")
  expect_identical(out$n, c("", "3", "12"))
})

test_that(".realign_count_pct_df() reformats only 'integer (real)' cells (#148)", {
  # align_count_pct only reformats "integer (real)" count-percent cells (the
  # real part may end in "%").  A bare integer (a plain N) and a continuous
  # statistic like "75.2 (8.6)" (whose "count" is not a bare integer) must pass
  # through UNCHANGED, even when the column also contains count-percent cells.
  df <- data.frame(
    label = c("N", "  Mean (SD)", "Sex", "  Female", "  Male"),
    a     = c("86", "75.2 (8.6)", "", "16 (53.3%)", "8 (46.7%)"),
    stringsAsFactors = FALSE
  )
  out <- rtfreporter:::.realign_count_pct_df(df, nbsp = " ")
  expect_identical(out$a[1L], "86")           # bare N untouched
  expect_identical(out$a[2L], "75.2 (8.6)")   # continuous stat untouched
  expect_identical(out$a[3L], "")             # empty untouched
  # The count-percent cells ARE reformatted to a common width, count
  # right-justified (the 3-wide count field, so "8" -> "  8 (...)").
  expect_equal(nchar(out$a[4L]), nchar(out$a[5L]))
  expect_match(out$a[5L], "^\\s+8 ")
  expect_true(endsWith(out$a[4L], ")"))
})

test_that("realign_count_pct() aligns percent-sign cells, keeping the %", {
  out <- realign_count_pct(c("8 (28.6%)", "10 (35.7%)", "3 (100%)", "5 (5.0%)"),
                           nbsp = " ")
  # All re-padded to the same display width, '%' preserved, ')' last char.
  expect_true(all(nchar(out) == nchar(out[1L])))
  expect_true(all(endsWith(out, "%)")))
  expect_true(any(grepl("8 (28.6%)", out, fixed = TRUE)))
})

test_that("format_count_pct(pct_sign = TRUE) adds % and stays width-aligned", {
  a <- format_count_pct(c(5L, 14L, 30L), c(5, 50, 100),
                        pct_unit = "percent", nbsp = " ", pct_sign = TRUE)
  expect_true(all(nchar(a) == nchar(a[1L])))
  expect_true(all(endsWith(a, "%)")))
  # pct_sign = FALSE is unchanged (no %).
  b <- format_count_pct(14L, 50, pct_unit = "percent", nbsp = " ")
  expect_false(grepl("%", b))
})


# ──────── realign_count_pct / as_rtftables: na = (#350) ──────────────────

test_that("realign_count_pct() leaves NA alone by default", {
  out <- realign_count_pct(c("5 (33.3)", NA))
  expect_identical(unbsp(out[1L]), "  5 (33.3)")
  expect_identical(out[2L], NA_character_)
})

test_that("realign_count_pct() aligns the na token in the count field", {
  out <- unbsp(realign_count_pct(c("5 (33.3)", NA, "12 (100.0)"), na = "-"))
  expect_identical(out[2L], "  -       ")
  expect_true(all(nchar(out) == 10L))
})

test_that("realign_count_pct() follows the column's percent sign", {
  out <- unbsp(realign_count_pct(c("5 (33.3%)", NA), na = "-"))
  expect_true(all(nchar(out) == 11L))    # the "%" widens every branch
})

test_that("realign_count_pct() does not pad the token in a column with no counts", {
  # Nothing to line it up WITH, so the fixed 10-wide layout is not imposed.
  expect_identical(realign_count_pct(c("text", NA), na = "-"), c("text", "-"))
})

test_that("realign_count_pct() leaves non-missing text unchanged", {
  out <- realign_count_pct(c("5 (33.3)", "NE", "NA (NA)"), na = "-")
  expect_identical(out[2:3], c("NE", "NA (NA)"))
})

test_that("as_rtftables(na =) substitutes with align_count_pct off", {
  df <- data.frame(P = c("a", "b"), A = c("5 (33.3)", NA),
                   stringsAsFactors = FALSE)
  body <- as_rtftables(df, na = "-")[[1L]]$data
  expect_identical(body$A, c("5 (33.3)", "-"))   # substituted, not padded
})

test_that("as_rtftables(na =) and align_count_pct compose", {
  df <- data.frame(P = c("a", "b"), A = c("5 (33.3)", NA),
                   stringsAsFactors = FALSE)
  body <- as_rtftables(df, align_count_pct = TRUE, na = "-")[[1L]]$data
  expect_identical(unbsp(body$A), c("  5 (33.3)", "  -       "))
})

test_that("as_rtftables(na =) reaches the row-label column too", {
  df <- data.frame(P = c("a", NA), A = c("5 (33.3)", "1 (2.0)"),
                   stringsAsFactors = FALSE)
  body <- as_rtftables(df, align_count_pct = TRUE, na = "-")[[1L]]$data
  expect_identical(body$P, c("a", "-"))          # column 1 is never padded
})

test_that("as_rtftables(na =) treats NaN as missing and leaves Inf visible", {
  df <- data.frame(P = c("a", "b", "c"), V = c(1, NaN, Inf),
                   stringsAsFactors = FALSE)
  body <- as_rtftables(df, na = "-")[[1L]]$data
  expect_identical(body$V, c("1", "-", "Inf"))
})

test_that("as_rtftables(na =) does not disturb collapse_repeats blanks", {
  # collapse_repeats writes NA per page AFTER the split to blank out repeated
  # values; those must stay blank, not become the token.
  df <- data.frame(G = c("A", "A", "B"), V = c("1", "2", "3"),
                   stringsAsFactors = FALSE)
  body <- as_rtftables(df, collapse_repeats = "G", na = "-")[[1L]]$data
  expect_identical(body$G, c("A", NA, "B"))
})

test_that("as_rtftables(na =) forwards to a cell_format that declares it", {
  df <- data.frame(P = c("a", "b"), A = c("1 (1.2%)", NA),
                   stringsAsFactors = FALSE)
  body <- as_rtftables(df, cell_format = fmt_count_paren, na = "-")[[1L]]$data
  expect_identical(unbsp(body$A), c("1 (1.2%)", "-       "))
})

test_that("as_rtftables(na =) validates its argument", {
  df <- data.frame(P = "a", A = "1", stringsAsFactors = FALSE)
  expect_error(as_rtftables(df, na = NA), "single string")
  expect_error(as_rtftables(df, na = c("-", "x")), "single string")
})

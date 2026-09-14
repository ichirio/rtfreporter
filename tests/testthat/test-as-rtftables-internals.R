# Pure-function unit tests (equivalence partitioning) for as_rtftables /
# as_rtftable internals, plus behavioural tests for the auto_width path and
# the remaining rtf_replace_text() argument-validation branches.

# ── .flatten_col_header_labels(): pick the widest label per column ───────────

test_that(".flatten_col_header_labels handles the empty / degenerate cases", {
  f <- rtfreporter:::.flatten_col_header_labels
  expect_null(f(NULL, 3L))                       # no header
  expect_null(f(c("A", "B"), 0L))                # no columns (boundary)
})

test_that(".flatten_col_header_labels takes a single character row verbatim", {
  f <- rtfreporter:::.flatten_col_header_labels
  expect_identical(f(c("A", "BB", "C"), 3L), c("A", "BB", "C"))
})

test_that(".flatten_col_header_labels keeps the longest label across rows", {
  f <- rtfreporter:::.flatten_col_header_labels
  hdr <- list(c("A", "B"), c("LongerA", "B"))
  expect_identical(f(hdr, 2L), c("LongerA", "B"))
})

test_that(".flatten_col_header_labels ignores spanning cells but uses pos cells", {
  f <- rtfreporter:::.flatten_col_header_labels
  # col_header is a list of ROWS; a cell-spec row is itself a list of cells.
  # Spanning cell (from != to) contributes to no single column.
  span_row <- list(list(list(from = 1L, to = 2L, label = "WIDE SPAN")))
  expect_identical(f(span_row, 2L), c("", ""))
  # pos cell targets exactly one column.
  pos_row <- list(list(list(pos = 2L, label = "Hello")))
  expect_identical(f(pos_row, 2L), c("", "Hello"))
  # Out-of-range positions are ignored (no error, no effect).
  oor_row <- list(list(list(pos = 5L, label = "X")))
  expect_identical(f(oor_row, 2L), c("", ""))
})

# ── .merge_col_spec(): user fields win, gt fills the gaps ────────────────────

test_that(".merge_col_spec covers the NULL combinations", {
  m <- rtfreporter:::.merge_col_spec
  expect_null(m(NULL, NULL))
  gt <- list(list(col = 1L, align = "right"))
  expect_identical(m(NULL, gt), gt)            # user NULL -> gt
  usr <- list(list(col = 1L, align = "left"))
  expect_identical(m(usr, NULL), usr)          # gt NULL -> user
})

test_that(".merge_col_spec merges per column with user precedence", {
  m <- rtfreporter:::.merge_col_spec
  usr <- list(list(col = 1L, align = "left"))
  gt  <- list(list(col = 1L, align = "right", bold = TRUE),
              list(col = 2L, align = "center"))
  out <- m(usr, gt)
  # Column 1: user's align wins; gt's bold is preserved.
  c1 <- Filter(function(e) identical(e$col, 1L), out)[[1L]]
  expect_identical(c1$align, "left")
  expect_true(isTRUE(c1$bold))
  # Column 2 (gt-only) survives -> two columns total.
  expect_length(out, 2L)
})

# ── auto_width behaviour in as_rtftables() ───────────────────────────────────

test_that("auto_width sizes the column with the longest content wider", {
  df <- data.frame(
    short = c("a", "b"),
    long  = c("a very long cell value that should not wrap", "x"),
    stringsAsFactors = FALSE
  )
  tbl <- as_rtftables(df, auto_width = TRUE)[[1L]]
  w <- tbl$column_widths_twips
  expect_false(is.null(w))
  expect_true(w[2L] > w[1L])     # the long column is wider
})

test_that("auto_width scales to table_width_twips when supplied", {
  df <- data.frame(a = c("x", "y"), b = c("pp", "qq"),
                   stringsAsFactors = FALSE)
  tbl <- as_rtftables(df, auto_width = TRUE, table_width_twips = 5000L)[[1L]]
  expect_equal(sum(tbl$column_widths_twips), 5000L, tolerance = 2)
})

test_that("an explicit column_widths_twips overrides auto_width", {
  df <- data.frame(a = c("x", "y"), b = c("pp", "qq"),
                   stringsAsFactors = FALSE)
  tbl <- as_rtftables(df, auto_width = TRUE,
                      column_widths_twips = c(1000L, 2000L))[[1L]]
  expect_identical(tbl$column_widths_twips, c(1000L, 2000L))
})

# ── format_count_pct(): input validation (public formatter contract) ─────────

test_that("format_count_pct validates its inputs", {
  expect_error(format_count_pct("5", 0.5), "must both be numeric")
  expect_error(format_count_pct(c(1L, 2L), c(0.1, 0.2, 0.3)),
               "same length")
})

# ── align_count_pct = TRUE: lone integers left untouched (#148) ─────────────

test_that("align_count_pct leaves a lone count untouched, aligns paren cells", {
  df <- data.frame(label = c("x", "y"),
                   b     = c("12 (50%)", "3"),
                   stringsAsFactors = FALSE)
  out <- as_rtftables(df, align_count_pct = TRUE)[[1L]]$data
  # A lone count ("3") is NOT a count-percent cell and must pass through as-is.
  expect_identical(out$b[2L], "3")
  # The row-label column (column 1) is never touched.
  expect_identical(out$label, df$label)
})

# ── rtf_replace_text(): remaining argument-validation branches ───────────────

test_that("rtf_replace_text validates input_file and required args", {
  expect_error(rtf_replace_text(c("a", "b"), "x", "y"),
               "single file path")
  p <- tempfile(fileext = ".rtf")
  writeBin(charToRaw("hello"), p)
  on.exit(unlink(p), add = TRUE)
  expect_error(rtf_replace_text(p),               "must be supplied")
  expect_error(rtf_replace_text(p, "hello"),      "must be supplied")
})

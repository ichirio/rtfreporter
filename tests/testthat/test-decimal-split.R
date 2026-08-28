# Tests for set_decimal_split() -- render-time decimal-point alignment.

W <- 9360L

.df <- function() {
  data.frame(
    Statistic = c("n", "Mean", "SD", "Median", "p-value", "Category"),
    Value     = c("12", "3.45", "-0.7", "100.0", "<0.001", "n (%)"),
    Other     = c("1", "2", "3", "4", "5", "6"),
    stringsAsFactors = FALSE
  )
}

# ──────── cell classification ──────────────────────────────────────────────

test_that(".decimal_split_cells splits at the first separator", {
  p <- rtfreporter:::.decimal_split_cells(c("3.45", "12", "100.0", "8.125"))
  expect_equal(p$left,  c("3", "12", "100", "8"))
  expect_equal(p$right, c(".45", "", ".0", ".125"))
  expect_true(all(p$split))
})

test_that("a relational or sign prefix travels with the left half", {
  p <- rtfreporter:::.decimal_split_cells(c("<0.001", ">=0.5", "-0.7", "+1.25"))
  expect_equal(p$left,  c("<0", ">=0", "-0", "+1"))
  expect_equal(p$right, c(".001", ".5", ".7", ".25"))
  expect_true(all(p$split))
})

test_that("a suffix travels with the right half", {
  p <- rtfreporter:::.decimal_split_cells(c("45.6%", "12.3^{a}"))
  expect_equal(p$left,  c("45", "12"))
  expect_equal(p$right, c(".6%", ".3^{a}"))
  expect_true(all(p$split))
})

test_that("free text is not split and keeps its original value", {
  p <- rtfreporter:::.decimal_split_cells(c("n (%)", "Grade 3 or higher", "NE"))
  expect_equal(p$left, c("n (%)", "Grade 3 or higher", "NE"))
  expect_equal(p$right, c("", "", ""))
  expect_false(any(p$split))
})

test_that("compound values are excluded by default and opt-in-able", {
  x <- c("12.3 (4.56)", "45 (67.8%)")
  p <- rtfreporter:::.decimal_split_cells(x)
  expect_false(any(p$split))
  expect_equal(p$left, x)

  q <- rtfreporter:::.decimal_split_cells(x, include_compound = TRUE)
  expect_true(all(q$split))
  expect_equal(q$left,  c("12", "45 (67"))
  expect_equal(q$right, c(".3 (4.56)", ".8%)"))
})

test_that("empty and NA cells split into two empty halves", {
  p <- rtfreporter:::.decimal_split_cells(c("", NA, "   "))
  expect_equal(p$left,  c("", "", ""))
  expect_equal(p$right, c("", "", ""))
  expect_true(all(p$split))
})

test_that("NBSP padding left by fmt_right_align() does not block the split", {
  padded <- fmt_right_align(c("5.25", "120.5"))
  p <- rtfreporter:::.decimal_split_cells(padded)
  expect_true(all(p$split))
  expect_equal(p$left,  c("5", "120"))
  expect_equal(p$right, c(".25", ".5"))
})

test_that("a non-default decimal_mark is honoured", {
  p <- rtfreporter:::.decimal_split_cells(c("3,45", "12"), decimal_mark = ",")
  expect_equal(p$left,  c("3", "12"))
  expect_equal(p$right, c(",45", ""))
})

# ──────── width measurement ────────────────────────────────────────────────

test_that(".decimal_split_width counts a relational pair as one glyph", {
  expect_equal(rtfreporter:::.decimal_split_width(">=0", "relational"), 2L)
  expect_equal(rtfreporter:::.decimal_split_width(">=0", character(0)), 3L)
})

test_that(".decimal_split_width ignores non-printing script markers", {
  expect_equal(rtfreporter:::.decimal_split_width(".3^{a}", "script"), 3L)
})

# ──────── the plan ─────────────────────────────────────────────────────────

test_that("the plan splits only the selected column and keeps the total width", {
  tbl <- set_decimal_split(rtftable(.df(), col_rel_width = c(3, 2, 2)),
                           cols = "Value")
  cx   <- rtfreporter:::.compute_cellx(3L, W, tbl)
  plan <- rtfreporter:::.decimal_split_plan(tbl, cx, tbl$col_spec, "script")

  expect_equal(plan$n0, 3L)
  expect_equal(plan$n1, 4L)
  expect_equal(plan$do_split, c(FALSE, TRUE, FALSE))
  expect_equal(plan$cellx[plan$n1], cx[3L])           # same right edge
  expect_equal(plan$cellx[c(1L, 3L, 4L)], cx)         # original edges kept
  expect_true(plan$cellx[1L] < plan$cellx[2L])
  expect_true(plan$cellx[2L] < plan$cellx[3L])
})

test_that("the split point follows the widest left / right part", {
  # left widest "100" (3), right widest ".001" (4).  With pad and floor
  # switched off that is the raw 3/7 of the column (#304).
  tbl <- set_decimal_split(rtftable(.df(), col_rel_width = c(3, 2, 2)),
                           cols = "Value",
                           pad_chars = c(0, 0), min_chars = c(0, 0))
  cx   <- rtfreporter:::.compute_cellx(3L, W, tbl)
  plan <- rtfreporter:::.decimal_split_plan(tbl, cx, tbl$col_spec, "script")
  expect_equal(plan$cellx[2L],
               cx[1L] + round((cx[2L] - cx[1L]) * 3 / 7))
})

test_that("by default each half is padded and floored (#304)", {
  # max(3 + 0.5, 3.5) = 3.5 against max(4 + 1, 6) = 6
  tbl <- set_decimal_split(rtftable(.df(), col_rel_width = c(3, 2, 2)),
                           cols = "Value")
  cx   <- rtfreporter:::.compute_cellx(3L, W, tbl)
  plan <- rtfreporter:::.decimal_split_plan(tbl, cx, tbl$col_spec, "script")
  expect_equal(plan$cellx[2L],
               cx[1L] + round((cx[2L] - cx[1L]) * 3.5 / 9.5))
})

test_that("an explicit ratio overrides the automatic one", {
  tbl <- set_decimal_split(rtftable(.df(), col_rel_width = c(3, 2, 2)),
                           cols = "Value", ratio = 0.5)
  cx   <- rtfreporter:::.compute_cellx(3L, W, tbl)
  plan <- rtfreporter:::.decimal_split_plan(tbl, cx, tbl$col_spec, "script")
  expect_equal(plan$cellx[2L], cx[1L] + round((cx[2L] - cx[1L]) * 0.5))
})

test_that("a column with no decimal separator anywhere is left alone", {
  tbl  <- set_decimal_split(rtftable(.df()), cols = "Other")
  cx   <- rtfreporter:::.compute_cellx(3L, W, tbl)
  expect_null(rtfreporter:::.decimal_split_plan(tbl, cx, tbl$col_spec, "script"))
})

test_that("the plan scans every data.frame of a multi-DF table", {
  d1 <- data.frame(a = "x", v = "1.5", stringsAsFactors = FALSE)
  d2 <- data.frame(a = "y", v = "1000.25", stringsAsFactors = FALSE)
  tbl <- set_decimal_split(rtftable(list(d1, d2)), cols = "v")
  cx   <- rtfreporter:::.compute_cellx(2L, W, tbl)
  plan <- rtfreporter:::.decimal_split_plan(tbl, cx, tbl$col_spec, "script")
  expect_equal(plan$parts[[2L]]$left,  c("1", "1000"))
  expect_equal(plan$parts[[2L]]$right, c(".5", ".25"))
})

# ──────── rendering ────────────────────────────────────────────────────────

test_that("data rows gain a cell while the header keeps the original geometry", {
  tbl <- set_decimal_split(rtftable(.df(), col_rel_width = c(3, 2, 2)),
                           cols = "Value")
  out <- rtfreporter:::.render_rtftable(tbl, W)
  cx  <- rtfreporter:::.compute_cellx(3L, W, tbl)

  n_cells <- function(s) lengths(regmatches(s, gregexpr("\\\\cellx", s)))
  expect_equal(n_cells(out[1L]), 3L)       # header row: unsplit
  expect_equal(n_cells(out[2L]), 4L)       # "12"      : split
  expect_equal(n_cells(out[7L]), 3L)       # "n (%)"   : merged back

  # every row still ends at the same right edge
  expect_true(all(grepl(paste0("\\\\cellx", cx[3L], "[^0-9]"), out)))
})

test_that("the halves are right- then left-aligned and carry the split text", {
  tbl <- set_decimal_split(rtftable(.df(), col_rel_width = c(3, 2, 2)),
                           cols = "Value")
  out <- rtfreporter:::.render_rtftable(tbl, W)
  expect_match(out[3L], "\\\\qr\\\\li0\\\\ri0 3\\\\cell", fixed = FALSE)
  expect_match(out[3L], "\\\\ql\\\\li0\\\\ri0 [.]45\\\\cell", fixed = FALSE)
})

test_that("a non-eligible row keeps the column's own alignment", {
  tbl <- set_decimal_split(rtftable(.df(), col_rel_width = c(3, 2, 2)),
                           cols = "Value")
  out <- rtfreporter:::.render_rtftable(tbl, W)
  expect_match(out[7L], "\\\\qc\\\\li0\\\\ri0 n \\(%\\)\\\\cell")
})

test_that("the interior edge of the pair carries no padding", {
  tbl <- rtftable(.df(), col_rel_width = c(3, 2, 2),
                  cell_padding_left_twips = 72L,
                  cell_padding_right_twips = 72L) |>
    set_decimal_split(cols = "Value")
  out <- rtfreporter:::.render_rtftable(tbl, W)
  # left half: outer \li72 kept, interior \ri0;  right half: \li0, outer \ri72
  expect_match(out[3L], "\\\\qr\\\\li72\\\\ri0 3\\\\cell")
  expect_match(out[3L], "\\\\ql\\\\li0\\\\ri72 [.]45\\\\cell")
})

test_that("the interior vertical rule is suppressed inside the pair", {
  vert <- rtf_border(left  = rtf_border_side("single"),
                     right = rtf_border_side("single"))
  tbl <- rtftable(.df(), col_rel_width = c(3, 2, 2),
                  border = rtf_table_border(body = vert)) |>
    set_decimal_split(cols = "Value")
  out  <- rtfreporter:::.render_rtftable(tbl, W)
  cx   <- rtfreporter:::.compute_cellx(3L, W, tbl)
  plan <- rtfreporter:::.decimal_split_plan(tbl, cx, tbl$col_spec, "script")

  # A cell definition is "<border cmds>\cellx<pos>", so the commands of the
  # cell ending at one \cellx sit in the chunk opened by the PREVIOUS one.
  cmds_after <- function(row, pos) {
    chunks <- strsplit(row, "\\\\cellx")[[1L]]
    hit <- grep(paste0("^", pos, "\\\\"), chunks)
    if (!length(hit)) return(NA_character_)
    chunks[[hit[1L]]]
  }
  left_def  <- cmds_after(out[3L], plan$cellx[1L])   # ends at the interior edge
  right_def <- cmds_after(out[3L], plan$cellx[2L])   # ends at the column edge

  expect_true(grepl("clbrdrl", left_def))    # outer edge kept
  expect_false(grepl("clbrdrr", left_def))   # interior edge suppressed
  expect_false(grepl("clbrdrl", right_def))  # interior edge suppressed
  expect_true(grepl("clbrdrr", right_def))   # outer edge kept
})

test_that("split rendering leaves the table's total width untouched", {
  plain <- rtftable(.df(), col_rel_width = c(3, 2, 2))
  split <- set_decimal_split(plain, cols = "Value")
  expect_equal(rtfreporter:::.content_width_twips(split, W),
               rtfreporter:::.content_width_twips(plain, W))
})

test_that("a table without the option renders byte-identically", {
  plain <- rtftable(.df(), col_rel_width = c(3, 2, 2))
  on_then_off <- set_decimal_split(
    set_decimal_split(plain, cols = "Value"), cols = NULL)
  expect_identical(rtfreporter:::.render_rtftable(on_then_off, W),
                   rtfreporter:::.render_rtftable(plain, W))
})

test_that("per-cell styles follow the column onto both halves", {
  tbl <- rtftable(.df(), col_rel_width = c(3, 2, 2)) |>
    style_body(rows = 2, cols = 2, bold = TRUE) |>
    set_decimal_split(cols = "Value")
  out <- rtfreporter:::.render_rtftable(tbl, W)
  expect_match(out[3L], "\\\\qr\\\\li0\\\\ri0 \\\\b 3\\\\b0 \\\\cell")
  expect_match(out[3L], "\\\\ql\\\\li0\\\\ri0 \\\\b [.]45\\\\b0 \\\\cell")
})

test_that("a per-cell align override cannot break a split pair", {
  tbl <- rtftable(.df(), col_rel_width = c(3, 2, 2)) |>
    style_body(rows = 2, cols = 2, align = "center") |>
    set_decimal_split(cols = "Value")
  out <- rtfreporter:::.render_rtftable(tbl, W)
  expect_match(out[3L], "\\\\qr\\\\li0\\\\ri0 3\\\\cell")
  expect_match(out[3L], "\\\\ql\\\\li0\\\\ri0 [.]45\\\\cell")
})

test_that("spanning headers still render over the original columns", {
  tbl <- rtftable(
    .df(), col_rel_width = c(3, 2, 2),
    col_header = list(
      list(col_cell(c(2, 3), "Statistics")),
      c("Statistic", "Value", "Other")
    )
  ) |> set_decimal_split(cols = "Value")
  out <- rtfreporter:::.render_rtftable(tbl, W)
  n_cells <- function(s) lengths(regmatches(s, gregexpr("\\\\cellx", s)))
  expect_equal(n_cells(out[1L]), 2L)      # spanning row: "Statistic" + span
  expect_equal(n_cells(out[2L]), 3L)      # label row
  expect_equal(n_cells(out[3L]), 4L)      # first data row is split
})

# ──────── the verb ─────────────────────────────────────────────────────────

test_that("set_decimal_split() stores resolved column positions", {
  tbl <- set_decimal_split(rtftable(.df()), cols = "Value")
  expect_equal(tbl$decimal_split$cols, 2L)
  expect_equal(tbl$decimal_split$decimal_mark, ".")
  expect_false(tbl$decimal_split$include_compound)
  expect_null(tbl$decimal_split$ratio)
})

test_that("set_decimal_split(cols = NULL) clears the setting", {
  tbl <- set_decimal_split(rtftable(.df()), cols = "Value")
  expect_null(set_decimal_split(tbl, cols = NULL)$decimal_split)
})

test_that("set_decimal_split() never touches the data", {
  df  <- .df()
  tbl <- set_decimal_split(rtftable(df), cols = "Value")
  expect_identical(tbl$data, df)
  expect_equal(ncol(tbl$data), 3L)
})

test_that("set_decimal_split() validates its arguments", {
  tbl <- rtftable(.df())
  expect_error(set_decimal_split(tbl, cols = "Nope"), "Nope")
  expect_error(set_decimal_split(tbl, cols = "Value", ratio = 0),
               "strictly between 0 and 1")
  expect_error(set_decimal_split(tbl, cols = "Value", ratio = 1.5),
               "strictly between 0 and 1")
  expect_error(set_decimal_split(tbl, cols = "Value", decimal_mark = ""),
               "non-empty string")
})

test_that("set_decimal_split() maps over a page list", {
  pages <- list(rtftable(.df()), rtftable(.df()))
  out   <- set_decimal_split(pages, cols = "Value")
  expect_length(out, 2L)
  expect_equal(out[[1L]]$decimal_split$cols, 2L)
  expect_equal(out[[2L]]$decimal_split$cols, 2L)
})

test_that("set_decimal_split() rejects a list that is not pages", {
  expect_error(set_decimal_split(list(1, 2), cols = 1),
               "set_decimal_split")
})

# Tests for paginate_cols() -- horizontal (column-wise) pagination.

W <- 13680L

.df <- function() {
  data.frame(
    Parameter = c("Age (years)", "  Mean", "  SD", "Weight (kg)", "  Mean", "  SD"),
    A_n    = c("", "86", "86", "", "86", "86"),
    A_mean = c("", "45.2", "12.3", "", "78.4", "9.1"),
    B_n    = c("", "84", "84", "", "84", "84"),
    B_mean = c("", "44.8", "11.9", "", "80.5", "10.2"),
    C_n    = c("", "85", "85", "", "85", "85"),
    C_mean = c("", "46.1", "13.0", "", "79.2", "8.8"),
    stringsAsFactors = FALSE
  )
}

.hdr <- function() {
  list(
    list(col_cell(c(2, 3), "Placebo"), col_cell(c(4, 5), "Active 10mg"),
         col_cell(c(6, 7), "Active 20mg")),
    c("Parameter", "n", "Mean", "n", "Mean", "n", "Mean")
  )
}

.widths <- function(tbl) {
  n <- ncol(if (!is.null(tbl$data)) tbl$data else tbl$data_list[[1L]])
  diff(c(0L, rtfreporter:::.compute_cellx(n, W, tbl)))
}

# ──────── blocks and carry columns ─────────────────────────────────────────

test_that("`at` cuts before the named columns", {
  pages <- paginate_cols(rtftable(.df()), at = c(4, 6))
  expect_length(pages, 3L)
  expect_equal(names(pages[[1L]]$data), c("Parameter", "A_n", "A_mean"))
  expect_equal(names(pages[[2L]]$data), c("Parameter", "B_n", "B_mean"))
  expect_equal(names(pages[[3L]]$data), c("Parameter", "C_n", "C_mean"))
})

test_that("`at` accepts column names", {
  pages <- paginate_cols(rtftable(.df()), at = c("B_n", "C_n"))
  expect_length(pages, 3L)
  expect_equal(names(pages[[2L]]$data), c("Parameter", "B_n", "B_mean"))
})

test_that("`cols` gives the blocks explicitly", {
  pages <- paginate_cols(rtftable(.df()), cols = list(2:3, 6:7))
  expect_length(pages, 2L)
  expect_equal(names(pages[[1L]]$data), c("Parameter", "A_n", "A_mean"))
  expect_equal(names(pages[[2L]]$data), c("Parameter", "C_n", "C_mean"))
})

test_that("the carry columns default to row_title and are never duplicated", {
  tbl <- rtftable(.df(), row_title = 1L)
  pages <- paginate_cols(tbl, at = 4)
  expect_equal(names(pages[[1L]]$data), c("Parameter", "A_n", "A_mean"))
  # the carry column is stripped from the block, not printed twice
  expect_equal(sum(names(pages[[1L]]$data) == "Parameter"), 1L)
  expect_equal(pages[[1L]]$row_title, 1L)
})

test_that("`carry` overrides the row-heading columns", {
  pages <- paginate_cols(rtftable(.df()), at = 4, carry = c("Parameter", "A_n"))
  expect_equal(names(pages[[1L]]$data), c("Parameter", "A_n", "A_mean"))
  expect_equal(names(pages[[2L]]$data),
               c("Parameter", "A_n", "B_n", "B_mean", "C_n", "C_mean"))
})

test_that("carry = integer(0) repeats nothing", {
  pages <- paginate_cols(rtftable(.df()), at = 4, carry = integer(0))
  expect_equal(names(pages[[1L]]$data), c("Parameter", "A_n", "A_mean"))
  expect_equal(names(pages[[2L]]$data), c("B_n", "B_mean", "C_n", "C_mean"))
})

test_that("paginate_cols() validates its arguments", {
  tbl <- rtftable(.df())
  expect_error(paginate_cols(tbl), "`at`")
  expect_error(paginate_cols(tbl, at = 4, cols = list(2:3)), "not both")
  expect_error(paginate_cols(tbl, at = 1), "nothing before column 1")
  expect_error(paginate_cols(tbl, at = 99), "at")
  expect_error(paginate_cols(tbl, cols = 2:3), "list of column blocks")
})

test_that("splitting a table that is all carry columns errors", {
  tbl <- rtftable(.df())
  expect_error(paginate_cols(tbl, at = 4, carry = 1:7), "carry")
})

# ──────── column widths: same column, same width ───────────────────────────

test_that("col_rel_width keeps each column exactly as wide as in the full table", {
  tbl <- rtftable(.df(), col_rel_width = c(3, 2, 2, 2, 2, 2, 2))
  full  <- .widths(tbl)
  pages <- paginate_cols(tbl, at = c(4, 6))
  expect_equal(.widths(pages[[1L]]), full[c(1L, 2L, 3L)])
  expect_equal(.widths(pages[[2L]]), full[c(1L, 4L, 5L)])
  expect_equal(.widths(pages[[3L]]), full[c(1L, 6L, 7L)])
})

test_that("column_widths_twips are carried through verbatim", {
  aw  <- c(3000L, 1800L, 1800L, 1800L, 1800L, 1740L, 1740L)
  tbl <- rtftable(.df(), column_widths_twips = aw)
  pages <- paginate_cols(tbl, at = c(4, 6))
  expect_equal(.widths(pages[[1L]]), aw[c(1L, 2L, 3L)])
  expect_equal(.widths(pages[[3L]]), aw[c(1L, 6L, 7L)])
  # absolute widths are not rescaled
  expect_null(pages[[1L]]$table_width_pct_of_writable)
})

test_that("equal distribution keeps column widths within rounding drift", {
  tbl   <- rtftable(.df())
  full  <- .widths(tbl)
  pages <- paginate_cols(tbl, at = c(4, 6))
  expect_true(all(abs(.widths(pages[[1L]]) - full[c(1L, 2L, 3L)]) <= 2L))
  expect_true(all(abs(.widths(pages[[2L]]) - full[c(1L, 4L, 5L)]) <= 2L))
})

test_that("the width scale composes with an explicit table width", {
  tbl <- rtftable(.df(), col_rel_width = rep(1, 7),
                  table_width_pct_of_writable = 0.5)
  pages <- paginate_cols(tbl, at = 4)
  # 3 of 7 columns kept -> 0.5 * 3/7
  expect_equal(pages[[1L]]$table_width_pct_of_writable, 0.5 * 3 / 7)
})

test_that("an absolute table_width_twips is scaled rather than a pct added", {
  tbl <- rtftable(.df(), col_rel_width = rep(1, 7), table_width_twips = 7000L)
  pages <- paginate_cols(tbl, at = 4)
  expect_equal(pages[[1L]]$table_width_twips, as.integer(round(7000 * 3 / 7)))
  expect_null(pages[[1L]]$table_width_pct_of_writable)
})

# ──────── headers ──────────────────────────────────────────────────────────

test_that("spanning cells are clipped to each page's columns", {
  pages <- paginate_cols(rtftable(.df(), col_header = .hdr()), at = c(4, 6))
  labs <- function(p) vapply(p$col_header[[1L]],
                             function(c) c$label %||% "", character(1L))
  expect_equal(labs(pages[[1L]]), c("", "Placebo"))
  expect_equal(labs(pages[[2L]]), c("", "Active 10mg"))
  expect_equal(labs(pages[[3L]]), c("", "Active 20mg"))
})

test_that("leaf header labels follow the kept columns", {
  pages <- paginate_cols(rtftable(.df(), col_header = .hdr()), at = c(4, 6))
  expect_equal(pages[[1L]]$col_header[[2L]], c("Parameter", "n", "Mean"))
})

test_that("a cut inside a spanning group repeats the label by default", {
  pages <- paginate_cols(rtftable(.df(), col_header = .hdr()), at = 3)
  labs <- function(p) vapply(p$col_header[[1L]],
                             function(c) c$label %||% "", character(1L))
  expect_true("Placebo" %in% labs(pages[[1L]]))
  expect_true("Placebo" %in% labs(pages[[2L]]))
})

test_that("allow_span_break = FALSE rejects a cut inside a spanning cell", {
  tbl <- rtftable(.df(), col_header = .hdr())
  expect_error(paginate_cols(tbl, at = 3, allow_span_break = FALSE),
               "Placebo")
  # a cut on a group boundary is still fine
  expect_length(paginate_cols(tbl, at = c(4, 6), allow_span_break = FALSE), 3L)
})

test_that("a standalone spanning_header is clipped too", {
  tbl <- rtftable(.df(),
                  spanning_header = list(list(from = 2, to = 3, label = "P"),
                                         list(from = 4, to = 5, label = "A")))
  pages <- paginate_cols(tbl, at = 4)
  expect_length(pages[[1L]]$spanning_header, 1L)
  expect_equal(pages[[1L]]$spanning_header[[1L]]$label, "P")
  expect_equal(pages[[2L]]$spanning_header[[1L]]$label, "A")
})

# ──────── other per-column state ───────────────────────────────────────────

test_that("col_spec follows the kept columns", {
  tbl <- rtftable(.df()) |> style_cols(cols = "C_mean", bold = TRUE)
  pages <- paginate_cols(tbl, at = c(4, 6))
  expect_false(isTRUE(pages[[1L]]$col_spec[[3L]]$bold))
  expect_true(isTRUE(pages[[3L]]$col_spec[[3L]]$bold))
  expect_length(pages[[1L]]$col_spec, 3L)
})

test_that("cell_styles follow the kept columns", {
  tbl <- rtftable(.df()) |> style_body(rows = 2, cols = "C_mean", bold = TRUE)
  pages <- paginate_cols(tbl, at = c(4, 6))
  expect_true(isTRUE(pages[[3L]]$cell_styles[[2L]]$bold[3L]))
  expect_length(pages[[1L]]$cell_styles[[2L]]$bold, 3L)
})

test_that("row_title is remapped onto the kept columns", {
  tbl <- rtftable(.df(), row_title = c(1L, 2L))
  pages <- paginate_cols(tbl, at = 4, carry = c(1L, 2L))
  expect_equal(pages[[1L]]$row_title, c(1L, 2L))
})

test_that("blank rows survive the column subset", {
  tbl <- rtftable(.df(), blank_rows = 3L)
  pages <- paginate_cols(tbl, at = 4)
  expect_equal(pages[[1L]]$blank_rows, 3L)
  expect_equal(pages[[2L]]$blank_rows, 3L)
})

test_that("a multi-DF table is split on every constituent frame", {
  d <- .df()
  tbl <- rtftable(list(d, d))
  pages <- paginate_cols(tbl, at = 4)
  expect_length(pages[[1L]]$data_list, 2L)
  expect_equal(names(pages[[1L]]$data_list[[2L]]),
               c("Parameter", "A_n", "A_mean"))
})

# ──────── page lists: ordering and names ───────────────────────────────────

test_that("the row page is the outer level: across first, then down", {
  d  <- .df()
  t1 <- rtftable(d[1:3, , drop = FALSE])          # row band 1
  t2 <- rtftable(d[4:6, , drop = FALSE])          # row band 2
  pages <- paginate_cols(list(t1, t2), at = c(4, 6))
  expect_length(pages, 6L)

  # 2 row bands x 3 column blocks ->
  #   row1/col1 row1/col2 row1/col3 row2/col1 row2/col2 row2/col3
  blocks <- vapply(pages, function(p) names(p$data)[2L], character(1L))
  expect_equal(blocks, c("A_n", "B_n", "C_n", "A_n", "B_n", "C_n"))

  band <- vapply(pages, function(p) p$data$Parameter[1L], character(1L))
  expect_equal(band, c(rep(d$Parameter[1L], 3L), rep(d$Parameter[4L], 3L)))
})

test_that("page names are carried through unchanged", {
  d  <- .df()
  pg <- list(one = rtftable(d[1:3, , drop = FALSE]),
             one = rtftable(d[4:6, , drop = FALSE]))
  out <- paginate_cols(pg, at = 4)
  expect_equal(names(out), rep("one", 4L))
})

test_that("an unnamed page list stays unnamed", {
  expect_null(names(paginate_cols(list(rtftable(.df())), at = 4)))
})

test_that("paginate_cols() rejects a list that is not pages", {
  expect_error(paginate_cols(list(1, 2), at = 2), "paginate_cols")
})

test_that("paginate_cols() rejects pages with differing columns", {
  d <- .df()
  expect_error(
    paginate_cols(list(rtftable(d), rtftable(d[1:4])), at = 4),
    "same columns")
})

test_that("an empty list returns an empty list", {
  expect_equal(paginate_cols(list(), at = 2), list())
})

# ──────── it renders ───────────────────────────────────────────────────────

test_that("column pages render, each keeping the full table's right edge", {
  tbl   <- rtftable(.df(), col_header = .hdr(), border = "tfl",
                    column_widths_twips = c(3000L, 1800L, 1800L, 1800L,
                                             1800L, 1800L, 1800L))
  pages <- paginate_cols(tbl, at = c(4, 6))
  for (p in pages) {
    out <- rtfreporter:::.render_rtftable(p, W)
    expect_true(length(out) > 0L)
    expect_true(all(grepl("\\\\cellx6600([^0-9]|$)", out)))   # 3000+1800+1800
  }
})

test_that("a table that is not paginated is untouched", {
  tbl   <- rtftable(.df(), col_header = .hdr(), border = "tfl")
  pages <- paginate_cols(tbl, cols = list(2:7))
  expect_identical(rtfreporter:::.render_rtftable(pages[[1L]], W),
                   rtfreporter:::.render_rtftable(tbl, W))
})

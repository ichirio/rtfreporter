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
  pages <- paginate_cols(tbl, at = 4, width = "keep")
  expect_equal(names(pages[[1L]]$data), c("Parameter", "A_n", "A_mean"))
  # the carry column is stripped from the block, not printed twice
  expect_equal(sum(names(pages[[1L]]$data) == "Parameter"), 1L)
  expect_equal(pages[[1L]]$row_title, 1L)
})

test_that("`carry` overrides the row-heading columns", {
  pages <- paginate_cols(rtftable(.df()), at = 4, carry = c("Parameter", "A_n"), width = "keep")
  expect_equal(names(pages[[1L]]$data), c("Parameter", "A_n", "A_mean"))
  expect_equal(names(pages[[2L]]$data),
               c("Parameter", "A_n", "B_n", "B_mean", "C_n", "C_mean"))
})

test_that("carry = integer(0) repeats nothing", {
  pages <- paginate_cols(rtftable(.df()), at = 4, carry = integer(0), width = "keep")
  expect_equal(names(pages[[1L]]$data), c("Parameter", "A_n", "A_mean"))
  expect_equal(names(pages[[2L]]$data), c("B_n", "B_mean", "C_n", "C_mean"))
})

test_that("paginate_cols() validates its arguments", {
  tbl <- rtftable(.df())
  expect_error(paginate_cols(tbl), "`at`")
  expect_error(paginate_cols(tbl, at = 4, cols = list(2:3)), "not at and cols")
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
  pages <- paginate_cols(tbl, at = c(4, 6), width = "keep")
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
  pages <- paginate_cols(tbl, at = c(4, 6), width = "keep")
  expect_true(all(abs(.widths(pages[[1L]]) - full[c(1L, 2L, 3L)]) <= 2L))
  expect_true(all(abs(.widths(pages[[2L]]) - full[c(1L, 4L, 5L)]) <= 2L))
})

test_that("the width scale composes with an explicit table width", {
  tbl <- rtftable(.df(), col_rel_width = rep(1, 7),
                  table_width_pct_of_writable = 0.5)
  pages <- paginate_cols(tbl, at = 4, width = "keep")
  # 3 of 7 columns kept -> 0.5 * 3/7
  expect_equal(pages[[1L]]$table_width_pct_of_writable, 0.5 * 3 / 7)
})

test_that("an absolute table_width_twips is scaled rather than a pct added", {
  tbl <- rtftable(.df(), col_rel_width = rep(1, 7), table_width_twips = 7000L)
  pages <- paginate_cols(tbl, at = 4, width = "keep")
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
  pages <- paginate_cols(rtftable(.df(), col_header = .hdr()), at = 3,
                         width = "keep")
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
  pages <- paginate_cols(tbl, at = 4, width = "keep")
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
  pages <- paginate_cols(tbl, at = 4, carry = c(1L, 2L), width = "keep")
  expect_equal(pages[[1L]]$row_title, c(1L, 2L))
})

test_that("blank rows survive the column subset", {
  tbl <- rtftable(.df(), blank_rows = 3L)
  pages <- paginate_cols(tbl, at = 4, width = "keep")
  expect_equal(pages[[1L]]$blank_rows, 3L)
  expect_equal(pages[[2L]]$blank_rows, 3L)
})

test_that("a multi-DF table is split on every constituent frame", {
  d <- .df()
  tbl <- rtftable(list(d, d))
  pages <- paginate_cols(tbl, at = 4, width = "keep")
  expect_length(pages[[1L]]$data_list, 2L)
  expect_equal(names(pages[[1L]]$data_list[[2L]]),
               c("Parameter", "A_n", "A_mean"))
})

# ──────── page lists: ordering and names ───────────────────────────────────

test_that("across = the column block advances first", {
  d  <- .df()
  t1 <- rtftable(d[1:3, , drop = FALSE])          # row band 1
  t2 <- rtftable(d[4:6, , drop = FALSE])          # row band 2
  pages <- paginate_cols(list(t1, t2), at = c(4, 6))
  expect_length(pages, 6L)

  # 2 row bands x 3 column blocks, the page number advancing ACROSS ->
  #   col1/row1 col1/row2 col2/row1 col2/row2 col3/row1 col3/row2
  blocks <- vapply(pages, function(p) names(p$data)[2L], character(1L))
  expect_equal(blocks, rep(c("A_n", "B_n", "C_n"), each = 2L))

  band <- vapply(pages, function(p) p$data$Parameter[1L], character(1L))
  expect_equal(band, rep(d$Parameter[c(1L, 4L)], 3L))
})

test_that("down = the row page advances first, its blocks following it", {
  d  <- .df()
  t1 <- rtftable(d[1:3, , drop = FALSE])          # row band 1
  t2 <- rtftable(d[4:6, , drop = FALSE])          # row band 2
  pages <- paginate_cols(list(t1, t2), at = c(4, 6), page_order = "down")
  expect_length(pages, 6L)

  # 2 row bands x 3 column blocks, the page number advancing DOWN ->
  #   row1/col1 row1/col2 row1/col3 row2/col1 row2/col2 row2/col3
  blocks <- vapply(pages, function(p) names(p$data)[2L], character(1L))
  expect_equal(blocks, c("A_n", "B_n", "C_n", "A_n", "B_n", "C_n"))

  band <- vapply(pages, function(p) p$data$Parameter[1L], character(1L))
  expect_equal(band, c(rep(d$Parameter[1L], 3L), rep(d$Parameter[4L], 3L)))
})

test_that("page_order only reorders: the same pages come back either way", {
  d  <- .df()
  pg <- list(rtftable(d[1:3, , drop = FALSE]), rtftable(d[4:6, , drop = FALSE]))
  a <- paginate_cols(pg, at = c(4, 6))                          # across
  b <- paginate_cols(pg, at = c(4, 6), page_order = "down")     # down
  # down index (row i, block bi) -> across index
  expect_equal(a, b[c(1L, 4L, 2L, 5L, 3L, 6L)])
})

test_that("page_order is a no-op on a single row page", {
  tbl <- rtftable(.df())
  expect_equal(paginate_cols(tbl, at = c(4, 6), page_order = "down"),
               paginate_cols(tbl, at = c(4, 6)))
})

test_that("a name follows its row page whatever the order", {
  d  <- .df()
  pg <- list(g1 = rtftable(d[1:3, , drop = FALSE]),
             g2 = rtftable(d[4:6, , drop = FALSE]))
  base <- rtfreporter:::.page_name_base
  # "down": the row page advances first, so its two column pages are adjacent
  expect_equal(base(names(paginate_cols(pg, at = 4, width = "keep",
                                        page_order = "down"))),
               c("g1", "g1", "g2", "g2"))
  # "across": the column block advances first, so equal headings interleave
  expect_equal(base(names(paginate_cols(pg, at = 4, width = "keep"))),
               c("g1", "g2", "g1", "g2"))
})

test_that("`page_order` is validated", {
  expect_error(paginate_cols(rtftable(.df()), at = 4, page_order = "sideways"),
               "arg")
})

test_that("page names are carried through, kept addressable", {
  d  <- .df()
  pg <- list(one = rtftable(d[1:3, , drop = FALSE]),
             one = rtftable(d[4:6, , drop = FALSE]))
  out <- paginate_cols(pg, at = 4, width = "keep")
  # the HEADING is carried onto every column page ...
  expect_equal(rtfreporter:::.page_name_base(names(out)), rep("one", 4L))
  # ... and the list stays addressable by name
  expect_equal(names(out), paste0("one...", 1:4))
  expect_false(is.null(out[["one...3"]]))
})

test_that("an unnamed page list stays unnamed", {
  expect_null(names(paginate_cols(list(rtftable(.df())), at = 4, width = "keep")))
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

# ──────── width = "fill" / "keep" (#289) ───────────────────────────────────

.rel_tbl <- function(nvis = 8L) {
  d <- data.frame(Parameter = c("n", "Mean"), stringsAsFactors = FALSE)
  for (v in paste0("D", seq_len(nvis))) d[[v]] <- c("86", "45.2")
  rtftable(d, col_rel_width = c(3, rep(1, nvis)), border = "tfl")
}

.tot <- function(p) {
  n <- ncol(p$data); rtfreporter:::.compute_cellx(n, W, p)[n]
}
.w <- function(p) {
  n <- ncol(p$data); diff(c(0L, rtfreporter:::.compute_cellx(n, W, p)))
}

test_that("width = 'fill' is the default and fills the sheet", {
  pages <- paginate_cols(.rel_tbl(), at = 6L)      # 4 + 4 visits
  expect_equal(.tot(pages[[1L]]), W)
  expect_equal(.tot(pages[[2L]]), W)
})

test_that("the ratio unit is fixed by page 1 and reused", {
  pages <- paginate_cols(.rel_tbl(), at = 6L)
  # a ratio-1 column, and the ratio-3 stub, are identical on both pages
  expect_equal(.w(pages[[1L]]), .w(pages[[2L]]))
  w1 <- .w(pages[[1L]])
  expect_equal(round(w1[1L] / w1[2L]), 3)          # the 3:1 ratio survives
})

test_that("a shorter final block gives a shorter page, same unit", {
  # 9 visits cut 4 + 5 would overflow; cut 5 + 4 keeps the widest first
  pages <- paginate_cols(.rel_tbl(9L), at = 7L)    # 5 + 4 visits
  expect_equal(.tot(pages[[1L]]), W)
  expect_lt(.tot(pages[[2L]]), W)
  # the ratio-1 column is the same width on both
  expect_equal(.w(pages[[1L]])[2L], .w(pages[[2L]])[2L])
})

test_that("four even blocks each fill the sheet", {
  pages <- paginate_cols(.rel_tbl(), at = c(4L, 6L, 8L))
  expect_length(pages, 4L)
  for (p in pages) expect_equal(.tot(p), W)
})

test_that("width = 'keep' preserves the pre-split widths", {
  full  <- .rel_tbl()
  wf    <- .w(full)
  pages <- paginate_cols(full, at = 6L, width = "keep")
  # within the rounding drift .compute_cellx() absorbs in a page's last column
  expect_true(all(abs(.w(pages[[1L]]) - wf[1:5]) <= 2L))
  expect_lt(.tot(pages[[1L]]), W)                  # a shorter page
})

test_that("a block wider than block 1 warns under 'fill'", {
  expect_warning(paginate_cols(.rel_tbl(), at = 5L),   # 3 + 5 visits
                 "wider than the sheet")
  expect_silent(paginate_cols(.rel_tbl(), at = 6L))    # 4 + 4: fine
  # "keep" cannot overflow, so it never warns
  expect_silent(paginate_cols(.rel_tbl(), at = 5L, width = "keep"))
})

test_that("equal distribution follows the same rule", {
  d <- data.frame(Parameter = c("n", "Mean"), stringsAsFactors = FALSE)
  for (v in paste0("D", 1:8)) d[[v]] <- c("86", "45.2")
  pages <- paginate_cols(rtftable(d, border = "tfl"), at = 6L)
  expect_equal(.tot(pages[[1L]]), W)
  expect_equal(.w(pages[[1L]]), .w(pages[[2L]]))
})

test_that("absolute widths ignore `width` entirely", {
  d <- data.frame(Parameter = c("n", "Mean"), stringsAsFactors = FALSE)
  for (v in paste0("D", 1:4)) d[[v]] <- c("86", "45.2")
  aw  <- c(2000L, 900L, 900L, 900L, 900L)
  tbl <- rtftable(d, column_widths_twips = aw, border = "tfl")
  a <- paginate_cols(tbl, at = 4L, width = "fill")
  b <- paginate_cols(tbl, at = 4L, width = "keep")
  expect_equal(.w(a[[1L]]), aw[1:3])
  expect_equal(.w(a[[1L]]), .w(b[[1L]]))
  expect_null(a[[1L]]$table_width_pct_of_writable)
})

test_that("`width` is validated", {
  expect_error(paginate_cols(.rel_tbl(), at = 6L, width = "nope"), "arg")
})

# ──────── by = : blocks from the column names (#435) ───────────────────────

.wide <- function() {
  d <- data.frame(Parameter = c("n", "Mean (SD)"), stringsAsFactors = FALSE)
  for (a in c("Placebo", "HOGE-001")) for (v in c("Day 1", "Day 2", "Day 8")) {
    d[[paste0(a, "____", v)]] <- "1"
  }
  d
}

test_that("by = a separator cuts on the part before it", {
  pg <- paginate_cols(rtftable(.wide()), by = "____", carry = 1, width = "keep")
  expect_length(pg, 2L)
  expect_identical(names(pg[[1L]]$data),
                   c("Parameter", paste0("Placebo____Day ", c(1, 2, 8))))
  expect_identical(names(pg[[2L]]$data),
                   c("Parameter", paste0("HOGE-001____Day ", c(1, 2, 8))))
})

test_that("by = a key vector cuts on a grouping that is not in the names", {
  pg <- paginate_cols(rtftable(.wide()), by = c(NA, rep(c("A", "B"), each = 3L)),
                      carry = 1, width = "keep")
  expect_length(pg, 2L)
  expect_identical(vapply(pg, function(p) ncol(p$data), integer(1L)),
                   c(4L, 4L))
})

test_that("at / cols / by are mutually exclusive", {
  expect_error(paginate_cols(rtftable(.wide()), at = 3, by = "____"),
               "not at and by")
})

# ──────── col_header = : written once, for the whole table ─────────────────

test_that("col_header = \"names\" builds the two-level header per page", {
  pg <- paginate_cols(rtftable(.wide()), by = "____", carry = 1,
                      col_header = "names", width = "keep")
  h1 <- pg[[1L]]$col_header
  # the label row: the stub keeps its name, the rest keep the visit
  expect_identical(h1[[2L]], c("Parameter", "Day 1", "Day 2", "Day 8"))
  # the spanning row: one cell over the group's columns
  span <- Filter(function(c1) nzchar(c1$label %||% ""), h1[[1L]])
  expect_length(span, 1L)
  expect_identical(span[[1L]]$label, "Placebo")
  expect_identical(c(span[[1L]]$from, span[[1L]]$to), c(2L, 4L))
  expect_identical(Filter(function(c1) nzchar(c1$label %||% ""),
                          pg[[2L]]$col_header[[1L]])[[1L]]$label, "HOGE-001")
})

test_that("col_header = \"names\" needs a separator `by`", {
  expect_error(paginate_cols(rtftable(.wide()), at = 5, col_header = "names"),
               "needs `by`")
})

test_that("a full-table col_header is sliced to each page", {
  hdr <- c("Label", paste("Visit", 1:6))
  pg  <- paginate_cols(rtftable(.wide()), at = 5, carry = 1,
                       col_header = hdr, width = "keep")
  expect_identical(unlist(pg[[1L]]$col_header),
                   c("Label", "Visit 1", "Visit 2", "Visit 3"))
  expect_identical(unlist(pg[[2L]]$col_header),
                   c("Label", "Visit 4", "Visit 5", "Visit 6"))
})

test_that("a col_header of the wrong width is refused", {
  expect_error(paginate_cols(rtftable(.wide()), at = 5, col_header = c("a", "b")),
               "label row has 2 labels")
})

# ──────── the width guard on the after-the-split paths ─────────────────────

test_that("set_col_header() refuses a header wider than the page", {
  pg <- paginate_cols(rtftable(.wide()), at = 5, carry = 1, width = "keep")
  expect_error(set_col_header(pg, c("Label", paste("Visit", 1:6))),
               "the table has 4 printed columns")
  # ... and points at the fix
  expect_error(set_col_header(pg, c("Label", paste("Visit", 1:6))),
               "BEFORE the column split")
})

test_that("rtf_tables(col_header = ) refuses it too", {
  pg  <- paginate_cols(rtftable(.wide()), at = 5, carry = 1, width = "keep")
  doc <- rtf_document() |>
    rtf_section(secinfo = list(header = NULL, footer = NULL))
  expect_error(rtf_tables(doc, pg, col_header = c("Label", paste("Visit", 1:6))),
               "rtf_tables(col_header)", fixed = TRUE)
})

test_that("a right-width header still applies to every page", {
  pg <- paginate_cols(rtftable(.wide()), at = 5, carry = 1, width = "keep")
  out <- set_col_header(pg, c("Lab", "A", "B", "C"))
  expect_identical(unlist(out[[1L]]$col_header), c("Lab", "A", "B", "C"))
  expect_identical(unlist(out[[2L]]$col_header), c("Lab", "A", "B", "C"))
})

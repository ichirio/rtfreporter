# Integration test: the PK concentration example of data-raw/gen_pk_conc.R.
#
# It is the only place where set_decimal_split() (#273) and paginate_cols()
# (#275) meet: the column split has to re-index `decimal_split$cols` onto the
# columns each page keeps.  The two features were built on separate branches,
# so nothing else covers that interaction.

VISITS <- c("Day 1", "Day 7", "Day 14", "Day 28", "Day 56", "Day 84")
STATS  <- c("n", "Mean", "SD", "CV%", "Median", "Min, Max")
W      <- rtfreporter:::.default_writable_twips()

# A miniature of the generated table: two nominal time points, six visits, and
# one cell of every kind the split has to cope with.
.pk_df <- function() {
  cell <- function(stat, t, v) {
    if (t == "24 h" && v == 1L) {
      return(switch(stat, "n" = "24", "Min, Max" = "BLQ, BLQ", "BLQ"))
    }
    if (t == "24 h" && v == 2L && stat %in% c("Mean", "Median")) return("<0.500")
    base <- c(902.33, 956.47, 1104.5, 88.012, 9.0125, 45.678)[v]
    switch(stat,
      "n"        = "24",
      "Mean"     = format(base, nsmall = 0L, trim = TRUE),
      "SD"       = format(round(base * 0.2, 2), nsmall = 0L, trim = TRUE),
      "CV%"      = "19.9",
      "Median"   = format(round(base * 0.97, 2), nsmall = 0L, trim = TRUE),
      "Min, Max" = paste(round(base * 0.55, 2), round(base * 1.62, 1), sep = ", "),
      "")
  }
  rows <- list()
  for (t in c("0.5 h", "24 h")) {
    rows[[length(rows) + 1L]] <- c(t, rep("", length(VISITS)))
    for (s in STATS) {
      rows[[length(rows) + 1L]] <- c(
        paste0("  ", s),
        vapply(seq_along(VISITS), function(v) cell(s, t, v), character(1L)))
    }
  }
  d <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(d) <- c("Nominal Time (h)", VISITS)
  d
}

# The pipeline the example uses: row split -> decimal split -> column split.
.pk_pages <- function(max_rows = 7L) {
  as_rtftables(
    .pk_df(),
    split = "group_safe", group_by = "indent", max_rows = max_rows,
    col_header = list(
      list(col_cell(c(2, 7), "Plasma Concentration (ng/mL)")),
      c("Nominal Time (h)", VISITS)
    ),
    col_rel_width = c(2.4, rep(1.6, length(VISITS))),
    border = "tfl"
  ) |>
    set_decimal_split(cols = VISITS) |>
    paginate_cols(at = c(4, 6))
}

.n_cells <- function(s) lengths(regmatches(s, gregexpr("\\\\cellx", s)))

# ──────── the two features compose ─────────────────────────────────────────

test_that("the PK pipeline yields row bands x column blocks", {
  pages <- .pk_pages()
  expect_length(pages, 2L * 3L)          # 2 time points, 3 column blocks
  expect_true(all(vapply(pages, inherits, logical(1L), "rtftable")))
})

test_that("pages come out row-band-outer", {
  pages  <- .pk_pages()
  blocks <- lapply(pages, function(p) names(p$data)[-1L])
  expect_equal(blocks, rep(list(VISITS[1:2], VISITS[3:4], VISITS[5:6]), 2L))
  # the first band holds the 0.5 h group, the second the 24 h group
  first <- vapply(pages, function(p) p$data[[1L]][1L], character(1L))
  expect_equal(first, c(rep("0.5 h", 3L), rep("24 h", 3L)))
})

test_that("decimal_split$cols is re-indexed onto each page's columns", {
  pages <- .pk_pages()
  for (p in pages) {
    expect_equal(p$decimal_split$cols, c(2L, 3L))
    expect_equal(ncol(p$data), 3L)
  }
})

test_that("the split still fires after the column split", {
  page <- .pk_pages()[[1L]]
  cx   <- rtfreporter:::.compute_cellx(3L, W, page)
  plan <- rtfreporter:::.decimal_split_plan(page, cx, page$col_spec, "script")
  expect_false(is.null(plan))
  expect_equal(plan$do_split, c(FALSE, TRUE, TRUE))
  expect_equal(plan$n1, 5L)              # 1 stub + 2 visits x 2 cells
})

test_that("a page from the last column block splits its own visits", {
  page <- .pk_pages()[[3L]]              # Day 56 / Day 84
  cx   <- rtfreporter:::.compute_cellx(3L, W, page)
  plan <- rtfreporter:::.decimal_split_plan(page, cx, page$col_spec, "script")
  expect_equal(plan$do_split, c(FALSE, TRUE, TRUE))
})

# ──────── the row-heading stub ─────────────────────────────────────────────

test_that("the time-point stub is repeated on every page", {
  pages <- .pk_pages()
  for (p in pages) {
    expect_equal(names(p$data)[1L], "Nominal Time (h)")
    expect_equal(p$row_title, 1L)
  }
})

test_that("the stub column is never split", {
  page <- .pk_pages()[[1L]]
  expect_false(1L %in% page$decimal_split$cols)   # column 1 is the stub
  expect_equal(min(page$decimal_split$cols), 2L)
})

# ──────── widths ───────────────────────────────────────────────────────────

test_that("every page has identical column widths", {
  pages <- .pk_pages()
  w <- lapply(pages, function(p)
    diff(c(0L, rtfreporter:::.compute_cellx(3L, W, p))))
  expect_true(all(vapply(w, identical, logical(1L), w[[1L]])))
})

test_that("the stub keeps the width it has in the full table", {
  full <- as_rtftables(.pk_df(), col_rel_width = c(2.4, rep(1.6, 6L)))[[1L]]
  wf   <- diff(c(0L, rtfreporter:::.compute_cellx(7L, W, full)))
  page <- .pk_pages()[[1L]]
  wp   <- diff(c(0L, rtfreporter:::.compute_cellx(3L, W, page)))
  expect_equal(wp, wf[c(1L, 2L, 3L)])
})

# ──────── rendering: one column, two cells ─────────────────────────────────

test_that("a concentration row renders two cells per visit", {
  out <- rtfreporter:::.render_rtftable(.pk_pages()[[1L]], W)
  mean_row <- grep("Mean", out, fixed = TRUE)[1L]
  expect_equal(.n_cells(out[mean_row]), 5L)        # stub + 2 visits x 2
  expect_match(out[mean_row], "\\\\qr\\\\li0\\\\ri0 902\\\\cell")
  expect_match(out[mean_row], "\\\\ql\\\\li0\\\\ri0 [.]33\\\\cell")
})

test_that("an integer-only n row leaves the decimal half empty", {
  out <- rtfreporter:::.render_rtftable(.pk_pages()[[1L]], W)
  n_row <- grep(" n\\\\cell", out)[1L]
  expect_equal(.n_cells(out[n_row]), 5L)
  expect_match(out[n_row], "\\\\qr\\\\li0\\\\ri0 24\\\\cell\\\\ql\\\\li0\\\\ri0 \\\\cell")
})

test_that("a compound Min, Max cell falls back to one merged cell", {
  out <- rtfreporter:::.render_rtftable(.pk_pages()[[1L]], W)
  mm  <- grep("Min, Max", out, fixed = TRUE)[1L]
  expect_equal(.n_cells(out[mm]), 3L)              # stub + one cell per visit
})

test_that("an all-BLQ visit column is left unsplit on that page", {
  # The 24 h band holds only "BLQ" / "BLQ, BLQ" / "24" for Day 1 -- no cell
  # carries a decimal separator, so that column is not split AT ALL on this
  # page (the plan skips it), while Day 7 still is.  The decision is per page,
  # which is what keeps a page's own geometry consistent.
  page <- .pk_pages()[[4L]]                        # 24 h band, Day 1 / Day 7
  expect_true(all(page$data[[2L]][nzchar(page$data[[2L]])] %in%
                    c("24", "BLQ", "BLQ, BLQ")))

  cx   <- rtfreporter:::.compute_cellx(3L, W, page)
  plan <- rtfreporter:::.decimal_split_plan(page, cx, page$col_spec, "script")
  expect_equal(plan$do_split, c(FALSE, FALSE, TRUE))
  expect_null(plan$parts[[2L]])
  expect_equal(plan$n1, 4L)                        # stub + Day 1 + Day 7 x 2
})

test_that("a <LLOQ value splits with its prefix on the left", {
  page <- .pk_pages()[[4L]]
  cx   <- rtfreporter:::.compute_cellx(3L, W, page)
  plan <- rtfreporter:::.decimal_split_plan(page, cx, page$col_spec, "script")

  lloq <- plan$parts[[3L]]                         # Day 7
  j    <- which(page$data[[3L]] == "<0.500")[1L]
  expect_true(lloq$split[j])
  expect_equal(lloq$left[j],  "<0")
  expect_equal(lloq$right[j], ".500")

  out <- rtfreporter:::.render_rtftable(page, W)
  row <- grep("0\\\\cell", out)
  expect_true(any(grepl("\\\\qr\\\\li0\\\\ri0 <0\\\\cell", out)))
  expect_true(any(grepl("\\\\ql\\\\li0\\\\ri0 [.]500\\\\cell", out)))
})

test_that("BLQ text renders as plain cell content", {
  page <- .pk_pages()[[4L]]
  out  <- rtfreporter:::.render_rtftable(page, W)
  expect_true(any(grepl("BLQ\\\\cell", out)))
})

# ──────── the header block ─────────────────────────────────────────────────

test_that("the spanning header is clipped to each page's visits", {
  pages <- .pk_pages()
  for (p in pages) {
    labs <- vapply(p$col_header[[1L]], function(c) c$label %||% "",
                   character(1L))
    expect_true("Plasma Concentration (ng/mL)" %in% labs)
  }
})

test_that("header rows keep the un-split column geometry", {
  out <- rtfreporter:::.render_rtftable(.pk_pages()[[1L]], W)
  expect_equal(.n_cells(out[1L]), 2L)   # stub + the clipped spanning cell
  expect_equal(.n_cells(out[2L]), 3L)   # stub + one label per visit
  expect_match(out[2L], "Day 1")
  expect_match(out[2L], "Day 7")
})

test_that("every page renders and ends at the same right edge", {
  pages <- .pk_pages()
  right <- rtfreporter:::.compute_cellx(3L, W, pages[[1L]])[3L]
  for (p in pages) {
    out <- rtfreporter:::.render_rtftable(p, W)
    expect_true(length(out) > 0L)
    expect_true(all(grepl(paste0("\\\\cellx", right, "([^0-9]|$)"), out)))
  }
})

# ──────── the shipped generator ────────────────────────────────────────────

test_that("data-raw/gen_pk_conc.R is present and self-consistent", {
  path <- testthat::test_path("..", "..", "data-raw", "gen_pk_conc.R")
  skip_if_not(file.exists(path), "data-raw/ is not in the installed package")
  src <- readLines(path, warn = FALSE)
  expect_true(any(grepl("set_decimal_split", src)))
  expect_true(any(grepl("paginate_cols", src)))
  expect_true(any(grepl("inst/rtf-examples/pk-concentration.rtf", src)))
})

# Integration test: the PK concentration example of data-raw/gen_pk_conc.R.
#
# It is the only place where set_decimal_split() (#273) and paginate_cols()
# (#275) meet: the column split has to re-index `decimal_split$cols` onto the
# columns each page keeps.  The two features were built on separate branches,
# so nothing else covers that interaction.

VISITS <- c("Day 1", "Day 7", "Day 14", "Day 28", "Day 42", "Day 56",
            "Day 70", "Day 84", "Day 112", "Day 140", "Day 168", "Day 196")
STATS  <- c("n", "Mean", "SD", "CV%", "Median", "Min, Max")
W      <- rtfreporter:::.default_writable_twips()
HALF   <- length(VISITS) / 2L

# A miniature of the generated table: two nominal time points, the full set of
# visits, and one cell of every kind the split has to cope with.
.pk_df <- function() {
  cell <- function(stat, t, v) {
    if (t == "24 h" && v == 1L) {
      return(switch(stat, "n" = "24", "Min, Max" = "BLQ, BLQ", "BLQ"))
    }
    if (t == "24 h" && v == 2L && stat %in% c("Mean", "Median")) return("<0.500")
    # Three significant figures, so the decimal count varies BETWEEN STATS --
    # which is what the split is for.  The widest cell of every visit column is
    # the fixed-width "Min, Max" pair, so all visit columns come out the same
    # width, exactly as in the generated example.
    fmt <- function(x) {
      if (x >= 1000) sprintf("%.1f", x)
      else if (x >= 100) sprintf("%.2f", x)
      else if (x >= 10)  sprintf("%.3f", x)
      else               sprintf("%.4f", x)
    }
    base <- 1000 + 25 * v
    switch(stat,
      "n"        = "24",
      "Mean"     = fmt(base),
      "SD"       = fmt(base * 0.2),
      "CV%"      = "19.9",
      "Median"   = fmt(base * 0.97),
      "Min, Max" = sprintf("%.2f, %.1f", base * 0.55, base * 1.62),
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

# Content-derived ABSOLUTE widths, as the example uses: relative widths are
# normalised to the page, so a table sized that way always fits and there is
# nothing for paginate_cols() to do.
.pk_widths <- function() {
  d <- .pk_df()
  auto_col_widths(d, col_header = names(d))
}

# The un-split table, to check that it does NOT fit on the page.
.pk_full <- function() {
  as_rtftables(.pk_df(), column_widths_twips = .pk_widths(),
               border = "tfl")[[1L]]
}

# The pipeline the example uses: row split -> decimal split -> column split.
.pk_pages <- function(max_rows = 7L) {
  as_rtftables(
    .pk_df(),
    split = "group_safe", group_by = "indent", max_rows = max_rows,
    col_header = list(
      list(col_cell(c(2, length(VISITS) + 1L),
                    "Plasma Concentration (ng/mL)")),
      c("Nominal Time (h)", VISITS)
    ),
    column_widths_twips = .pk_widths(),
    border = "tfl"
  ) |>
    set_decimal_split(cols = VISITS) |>
    paginate_cols(at = HALF + 2L)  # cut the visits down the middle
}

# The two column blocks the example produces.
.BLOCK1 <- VISITS[seq_len(HALF)]                        # Day 1 .. Day 56
.BLOCK2 <- VISITS[seq.int(HALF + 1L, length(VISITS))]   # Day 70 .. Day 196

.n_cells <- function(s) lengths(regmatches(s, gregexpr("\\\\cellx", s)))

# The decimal-split plan a page would render with.
.plan_of <- function(page) {
  cx <- rtfreporter:::.compute_cellx(ncol(page$data), W, page)
  rtfreporter:::.decimal_split_plan(page, cx, page$col_spec, "script")
}

# ──────── the two features compose ─────────────────────────────────────────

test_that("the PK pipeline yields row bands x column blocks", {
  pages <- .pk_pages()
  expect_length(pages, 2L * 2L)          # 2 time points, 2 column blocks
  expect_true(all(vapply(pages, inherits, logical(1L), "rtftable")))
})

test_that("pages come out row-band-outer", {
  pages  <- .pk_pages()
  blocks <- lapply(pages, function(p) names(p$data)[-1L])
  expect_equal(blocks, rep(list(.BLOCK1, .BLOCK2), 2L))
  # the first band holds the 0.5 h group, the second the 24 h group
  first <- vapply(pages, function(p) p$data[[1L]][1L], character(1L))
  expect_equal(first, c(rep("0.5 h", 2L), rep("24 h", 2L)))
})

test_that("decimal_split$cols is re-indexed onto each page's columns", {
  pages <- .pk_pages()
  for (p in pages) {
    # every visit column of the page, never the stub
    expect_equal(p$decimal_split$cols, seq.int(2L, ncol(p$data)))
    expect_equal(ncol(p$data), HALF + 1L)
  }
})

test_that("the split still fires after the column split", {
  plan <- .plan_of(.pk_pages()[[1L]])    # 0.5 h band, Day 1 .. Day 56
  expect_false(is.null(plan))
  expect_equal(plan$do_split, c(FALSE, rep(TRUE, HALF)))
  expect_equal(plan$n1, 1L + 2L * HALF)  # stub + 6 visits x 2 cells
})

test_that("a page from the last column block splits its own visits", {
  plan <- .plan_of(.pk_pages()[[2L]])    # 0.5 h band, Day 70 .. Day 196
  expect_equal(plan$do_split, c(FALSE, rep(TRUE, HALF)))
  expect_equal(plan$n1, 1L + 2L * HALF)
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

test_that("the whole table does not fit, but each column block does", {
  # This is what makes the example worth having: sized by its own content the
  # table is far wider than the sheet, so the column split is not decorative.
  full <- .pk_full()
  nf   <- ncol(full$data)
  wide <- rtfreporter:::.compute_cellx(nf, W, full)[nf]
  expect_gt(wide, W)
  expect_gt(wide / W, 1.5)                          # ~1.73x the page

  for (p in .pk_pages()) {
    n <- ncol(p$data)
    expect_lte(rtfreporter:::.compute_cellx(n, W, p)[n], W)
  }
})

test_that("each block fills most of the page", {
  for (p in .pk_pages()) {
    n     <- ncol(p$data)
    total <- rtfreporter:::.compute_cellx(n, W, p)[n]
    expect_gt(total / W, 0.9)                       # ~94% -- little slack
  }
})

test_that("every column keeps the width it has in the full table", {
  wf    <- .pk_widths()
  pages <- .pk_pages()
  w     <- function(p) {
    n <- ncol(p$data); diff(c(0L, rtfreporter:::.compute_cellx(n, W, p)))
  }
  expect_equal(w(pages[[1L]]), wf[c(1L, seq_len(HALF) + 1L)])
  expect_equal(w(pages[[2L]]), wf[c(1L, seq.int(HALF + 2L, length(wf)))])
  expect_equal(w(pages[[1L]])[1L], wf[1L])          # the stub is unchanged
})

test_that("pages of the same block have identical widths", {
  pages <- .pk_pages()
  w <- function(p) diff(c(0L, rtfreporter:::.compute_cellx(ncol(p$data), W, p)))
  expect_equal(w(pages[[1L]]), w(pages[[3L]]))      # both are block 1
  expect_equal(w(pages[[2L]]), w(pages[[4L]]))      # both are block 2
})

# ──────── rendering: one column, two cells ─────────────────────────────────

test_that("a concentration row renders two cells per visit", {
  out <- rtfreporter:::.render_rtftable(.pk_pages()[[1L]], W)
  mean_row <- grep("Mean", out, fixed = TRUE)[1L]
  expect_equal(.n_cells(out[mean_row]), 1L + 2L * HALF)  # stub + 6 x 2
  # Mean = "1025.0" -> one decimal
  expect_match(out[mean_row], "\\\\qr\\\\li0\\\\ri0 1025\\\\cell")
  expect_match(out[mean_row], "\\\\ql\\\\li0\\\\ri0 [.]0\\\\cell")

  # ... while SD = "205.00" carries two, on the same column: differing decimal
  # counts are exactly what the split has to line up
  sd_row <- grep(" SD\\\\cell", out)[1L]
  expect_match(out[sd_row], "\\\\qr\\\\li0\\\\ri0 205\\\\cell")
  expect_match(out[sd_row], "\\\\ql\\\\li0\\\\ri0 [.]00\\\\cell")
})

test_that("an integer-only n row leaves the decimal half empty", {
  out <- rtfreporter:::.render_rtftable(.pk_pages()[[1L]], W)
  n_row <- grep(" n\\\\cell", out)[1L]
  expect_equal(.n_cells(out[n_row]), 1L + 2L * HALF)
  expect_match(out[n_row], "\\\\qr\\\\li0\\\\ri0 24\\\\cell\\\\ql\\\\li0\\\\ri0 \\\\cell")
})

test_that("a compound Min, Max cell falls back to one merged cell", {
  out <- rtfreporter:::.render_rtftable(.pk_pages()[[1L]], W)
  mm  <- grep("Min, Max", out, fixed = TRUE)[1L]
  expect_equal(.n_cells(out[mm]), 1L + HALF)       # stub + one cell per visit
})

test_that("an all-BLQ visit column is left unsplit on that page", {
  # The 24 h band holds only "BLQ" / "BLQ, BLQ" / "24" for Day 1 -- no cell
  # carries a decimal separator, so that column is not split AT ALL on this
  # page (the plan skips it), while the other visits still are.  The decision
  # is per page, which is what keeps a page's own geometry consistent.
  page <- .pk_pages()[[3L]]                # 24 h band, Day 1 .. Day 28
  expect_true(all(page$data[[2L]][nzchar(page$data[[2L]])] %in%
                    c("24", "BLQ", "BLQ, BLQ")))

  plan <- .plan_of(page)
  expect_equal(plan$do_split, c(FALSE, FALSE, rep(TRUE, HALF - 1L)))
  expect_null(plan$parts[[2L]])
  expect_equal(plan$n1, 2L + 2L * (HALF - 1L))  # stub + Day 1 + 5 x 2
})

test_that("a <LLOQ value splits with its prefix on the left", {
  page <- .pk_pages()[[3L]]
  plan <- .plan_of(page)

  lloq <- plan$parts[[3L]]                 # Day 7
  j    <- which(page$data[[3L]] == "<0.500")[1L]
  expect_true(lloq$split[j])
  expect_equal(lloq$left[j],  "<0")
  expect_equal(lloq$right[j], ".500")

  out <- rtfreporter:::.render_rtftable(page, W)
  expect_true(any(grepl("\\\\qr\\\\li0\\\\ri0 <0\\\\cell", out)))
  expect_true(any(grepl("\\\\ql\\\\li0\\\\ri0 [.]500\\\\cell", out)))
})

test_that("BLQ text renders as plain cell content", {
  out <- rtfreporter:::.render_rtftable(.pk_pages()[[3L]], W)
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
  expect_equal(.n_cells(out[2L]), 1L + HALF)   # stub + one label per visit
  for (v in .BLOCK1) expect_match(out[2L], v, fixed = TRUE)
})

test_that("every row of a page ends at that page's right edge", {
  pages <- .pk_pages()
  for (p in pages) {
    n     <- ncol(p$data)
    right <- rtfreporter:::.compute_cellx(n, W, p)[n]
    out   <- rtfreporter:::.render_rtftable(p, W)
    expect_true(length(out) > 0L)
    # header rows, split data rows and merged data rows all differ in cell
    # count, but every one of them closes at the same edge
    expect_true(all(grepl(paste0("\\\\cellx", right, "([^0-9]|$)"), out)))
  }
})

test_that("both blocks end at the same right edge", {
  pages <- .pk_pages()
  edge  <- function(p) {
    n <- ncol(p$data); rtfreporter:::.compute_cellx(n, W, p)[n]
  }
  # the two blocks hold the same number of equally wide visit columns
  expect_equal(edge(pages[[1L]]), edge(pages[[2L]]))
  expect_equal(edge(pages[[1L]]), edge(pages[[3L]]))
  expect_equal(edge(pages[[2L]]), edge(pages[[4L]]))
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

## tests/testthat/test-adapter-pagination.R
##
## The shared as_rtftables() pipeline -- metadata extraction -> grouping ->
## pagination -> blank rows -> collapse_repeats -- must work for EVERY table
## object, not just plain data.frames.  These tests paginate a multi-group
## fixture across multiple pages through each adapter (gt, gtsummary,
## rtables/tern, flextable, huxtable) and assert the body survives the round
## trip.  All tests skip when the adapter's package is not installed (#134).

library(testthat)

# Paginate `obj` with `split = "group_safe"` and assert the shared pipeline
# produced a genuine multi-group, multi-page result with no row loss.  The
# fixtures are sized so no single group exceeds `max_rows`, so group_safe never
# force-splits a group and therefore emits no "(Cont.)" rows -- the concatenated
# page bodies must equal the un-paginated body exactly.
.expect_multipage_roundtrip <- function(obj, group_col, group_by, max_rows,
                                        min_pages = 2L) {
  # Strip pagination bookkeeping attributes so the comparison is about the cell
  # data only (each chunk carries its own rtf_paginate_meta / rtf_blank_rows).
  .plain <- function(d) {
    for (a in c("rtf_paginate_meta", "rtf_blank_rows")) attr(d, a) <- NULL
    rownames(d) <- NULL
    as.data.frame(d, stringsAsFactors = FALSE)
  }

  full  <- as_rtftable(obj)$data                       # one-page reference body
  pages <- as_rtftables(obj, split = "group_safe", max_rows = max_rows,
                        group_col = group_col, group_by = group_by)

  expect_gte(length(pages), min_pages)                 # multiple pages
  expect_true(all(vapply(pages, inherits, logical(1L), "rtftable")))

  # Re-assemble the body from the pages and compare to the reference (data only).
  rebuilt <- do.call(rbind, lapply(pages, function(p) p$data))
  expect_equal(.plain(rebuilt), .plain(full))

  # The grouping must have spread across pages (no page holds everything).
  expect_true(all(vapply(pages, function(p) nrow(p$data) < nrow(full),
                         logical(1L))))
  invisible(pages)
}


# ── gt: groupname_col -> a heading row a group, its rows indented (#650) ─────

.make_gt_grouped <- function() {
  skip_if_not_installed("gt")
  gdf <- data.frame(
    grp = c("A", "A", "A", "B", "B", "B", "C", "C", "C"),
    lab = rep(c("Mean", "SD", "N"), 3L),
    val = as.character(1:9),
    stringsAsFactors = FALSE
  )
  gt::gt(gdf, groupname_col = "grp")
}

test_that("as_rtftables(gt): multi-group table paginates across pages (group_safe)", {
  pages <- .expect_multipage_roundtrip(.make_gt_grouped(),
                                       group_col = 1L, group_by = "auto", max_rows = 4L)
  # a page starts at a group's heading (a cell that is not indented), and
  # holds that group whole: the heading and its three rows
  nb <- intToUtf8(160L)
  for (p in pages) {
    first <- p$data[[1L]]
    expect_false(startsWith(first[1L], nb))
    expect_identical(nrow(p$data), 4L)
  }
})


# ── gtsummary: several variables -> NBSP-indented groups (indent style) ───────

.make_gtsummary_grouped <- function() {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("gt")
  dd <- data.frame(
    sex  = factor(c("M", "F", "M", "F", "M", "F")),
    arm  = factor(c("A", "A", "B", "B", "C", "C")),
    resp = factor(c("Yes", "No", "Yes", "No", "Yes", "No")),
    stringsAsFactors = FALSE
  )
  gtsummary::tbl_summary(dd)                            # 3 variable groups
}

test_that("as_rtftables(gtsummary): multi-variable summary paginates across pages", {
  .expect_multipage_roundtrip(.make_gtsummary_grouped(),
                              group_col = 1L, group_by = "indent", max_rows = 5L)
})


# ── rtables / tern: split_rows_by -> space-indented row groups (indent) ───────

.make_rtables_grouped <- function() {
  skip_if_not_installed("rtables")
  skip_if_not_installed("formatters")
  lyt <- rtables::basic_table()
  lyt <- rtables::split_cols_by(lyt, "ARM")
  lyt <- rtables::split_rows_by(lyt, "RACE")
  lyt <- rtables::analyze(lyt, "AGE", afun = function(x) rtables::in_rows(
    "Mean" = rtables::rcell(mean(x), format = "xx.x"),
    "SD"   = rtables::rcell(sd(x),   format = "xx.x")))
  set.seed(1)
  df <- data.frame(
    ARM  = rep(c("A", "B"), each = 18),
    RACE = rep(c("WHITE", "BLACK", "ASIAN"), 12),
    AGE  = rnorm(36, 40, 5)
  )
  rtables::build_table(lyt, df)                         # 3 RACE groups x (Mean/SD)
}

test_that("as_rtftables(rtables): row-group table paginates across pages", {
  .expect_multipage_roundtrip(.make_rtables_grouped(),
                              group_col = 1L, group_by = "indent", max_rows = 4L)
})


# ── flextable: plain grouped data.frame -> V1 repeated value (value) ──────────

.make_flextable_grouped <- function() {
  skip_if_not_installed("flextable")
  fdf <- data.frame(
    Grp  = c("A", "A", "B", "B", "C", "C"),
    Stat = rep(c("Mean", "SD"), 3L),
    Val  = as.character(1:6),
    stringsAsFactors = FALSE
  )
  flextable::flextable(fdf)
}

test_that("as_rtftables(flextable): multi-group table paginates across pages", {
  .expect_multipage_roundtrip(.make_flextable_grouped(),
                              group_col = 1L, group_by = "value", max_rows = 3L)
})


# ── huxtable: plain grouped data.frame -> V1 repeated value (value) ───────────

.make_huxtable_grouped <- function() {
  skip_if_not_installed("huxtable")
  hdf <- data.frame(
    Grp  = c("A", "A", "B", "B", "C", "C"),
    Stat = rep(c("Mean", "SD"), 3L),
    Val  = as.character(1:6),
    stringsAsFactors = FALSE
  )
  huxtable::as_hux(hdf)
}

test_that("as_rtftables(huxtable): multi-group table paginates across pages", {
  .expect_multipage_roundtrip(.make_huxtable_grouped(),
                              group_col = 1L, group_by = "value", max_rows = 3L)
})


# ── Whole pipeline: collapse_repeats + blank_rows through the adapters ────────
# Value-style group columns (flextable / huxtable) carry the key
# on every row, so collapse_repeats should blank the repeats per page while
# pagination still splits on the original values.

test_that("collapse_repeats + blank_rows run through value-style adapters across pages", {
  fixtures <- list(
    flextable = function() .make_flextable_grouped(),
    huxtable  = function() .make_huxtable_grouped()
  )
  for (nm in names(fixtures)) {
    obj   <- fixtures[[nm]]()                           # skips inside if absent
    pages <- as_rtftables(
      obj, split = "group_safe", max_rows = 4L,
      group_col = 1L, group_by = "value",
      collapse_repeats = 1L, blank_row_end = TRUE
    )
    expect_gte(length(pages), 2L)
    for (p in pages) {
      expect_s3_class(p, "rtftable")
      g <- p$data[[1L]]
      # Per page the first cell keeps its label; at least one later repeat in
      # the page is blanked to NA (each page has >= 2 rows of one group here).
      expect_false(is.na(g[1L]))
      expect_true(anyNA(g))
    }
  }
})

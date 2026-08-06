## tests/testthat/test-set-col-header.R
##
## set_col_header() / rtf_columns(): configure a column header against the
## FINAL printed table (name- and final-position-based), and the as_rtftables()
## delegation that makes col_header = ... resolve in final coordinates too.

library(testthat)

# ── rtf_columns() ──────────────────────────────────────────────────────────

test_that("rtf_columns() returns the final body column names", {
  tbl <- rtftable(data.frame(Item = "x", A = 1, B = 2))
  expect_identical(rtf_columns(tbl), c("Item", "A", "B"))
})

test_that("rtf_columns() on a page list reports the first page's columns", {
  pages <- as_rtftables(data.frame(g = c("a", "b"), v = 1:2),
                        split = "by_value", group_col = "g")
  expect_true(is.list(pages) && !inherits(pages, "rtftable"))
  expect_identical(rtf_columns(pages), c("g", "v"))
})

# ── set_col_header(): names + final positions ──────────────────────────────

test_that("set_col_header() places a spanning cell + labels by column name", {
  df  <- data.frame(row_label = "x", g1 = 1, g2 = 2, Total = 3)
  tbl <- rtftable(df)
  tbl <- set_col_header(
    tbl,
    list(col_cell("row_label", ""), col_cell(c("g1", "g2"), "Treatment")),
    c(row_label = "Category", g1 = "Low", g2 = "High", Total = "Total")
  )
  span <- tbl$col_header[[1L]]
  expect_identical(span[[2L]]$from, 2L)   # g1
  expect_identical(span[[2L]]$to,   3L)   # g2
  expect_identical(span[[2L]]$label, "Treatment")
  expect_identical(tbl$col_header[[2L]], c("Category", "Low", "High", "Total"))
})

test_that("set_col_header() accepts final positions and a prebuilt rtf_col_header", {
  tbl <- rtftable(data.frame(A = 1, B = 2, C = 3, D = 4))
  hdr <- rtf_col_header(list(col_cell(1L, ""), col_cell(c(2L, 4L), "Group")),
                        c("Item", "N", "Mean", "SD"))
  tbl <- set_col_header(tbl, hdr)
  expect_length(tbl$col_header, 2L)
  expect_identical(tbl$col_header[[1L]][[2L]]$from, 2L)
  expect_identical(tbl$col_header[[1L]][[2L]]$to,   4L)
})

test_that("set_col_header() errors on an unknown name and lists the columns", {
  tbl <- rtftable(data.frame(A = 1, B = 2))
  expect_error(set_col_header(tbl, list(col_cell("zzz", "X"))),
               "not found in data columns.*\"A\", \"B\"")
})

test_that("set_col_header(align =) sets per-column header alignment", {
  tbl <- rtftable(data.frame(A = 1, B = 2, C = 3))
  tbl <- set_col_header(tbl, c("a", "b", "c"), align = "left")
  aligns <- vapply(tbl$col_spec, function(s) s$header_align, character(1L))
  expect_identical(aligns, c("left", "left", "left"))
  tbl <- set_col_header(tbl, c("a", "b", "c"),
                        align = c("left", "center", "right"))
  aligns <- vapply(tbl$col_spec, function(s) s$header_align, character(1L))
  expect_identical(aligns, c("left", "center", "right"))
})

test_that("set_col_header() with no rows clears the header", {
  tbl <- rtftable(data.frame(A = 1, B = 2), col_header = c("X", "Y"))
  tbl <- set_col_header(tbl)
  expect_null(tbl$col_header)
})

# ── set_col_header() on a page list ────────────────────────────────────────

test_that("set_col_header() maps over every page of an as_rtftables() list", {
  pages <- as_rtftables(data.frame(g = c("a", "a", "b"), lab = c("x", "y", "z"),
                                   v = 1:3),
                        split = "by_value", group_col = "g", drop_cols = "g")
  pages <- set_col_header(pages, c(lab = "Label", v = "Value"), align = "center")
  for (p in pages) {
    expect_identical(unlist(p$col_header), c("Label", "Value"))
  }
})

# ── as_rtftables(col_header =) now resolves in FINAL coordinates ────────────

test_that("as_rtftables(col_header =) with stub_vars + drop_cols uses final coords", {
  # Input has 9 columns, LBTOX_LBL first.  stub_vars folds group1+label into a
  # stub at position 1; LBTOX_LBL is dropped.  Final printed columns are:
  #   row_label, Grade 0..Grade 4, Total  (7 columns)
  tbl_df <- data.frame(
    LBTOX_LBL = rep(c("Chem", "Hema"), each = 3),
    group1    = rep("Any", 6),
    label     = rep(c("A", "B", "C"), 2),
    `Grade 0` = 1:6, `Grade 1` = 7:12, `Grade 2` = 13:18,
    `Grade 3` = 19:24, `Grade 4` = 25:30, Total = 31:36,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  hdr <- rtf_col_header(
    list(col_cell("row_label", "Timepoint"), col_cell(c("Grade 0", "Total"), "HOGE")),
    c("  Category", "G0", "G1", "G2", "G3", "G4", "Total")
  )
  out <- as_rtftables(
    tbl_df, read_meta = FALSE, split = "by_value",
    group_col = "LBTOX_LBL", drop_cols = "LBTOX_LBL",
    stub_vars = c("group1", "label"), stub_label = "row_label",
    col_rel_width = c(5, rep(1, 6)), col_header = hdr
  )
  tbl <- out[[1L]]
  expect_identical(rtf_columns(tbl),
                   c("row_label", "Grade 0", "Grade 1", "Grade 2",
                     "Grade 3", "Grade 4", "Total"))
  span <- tbl$col_header[[1L]]
  expect_identical(span[[1L]]$label, "Timepoint")
  expect_identical(c(span[[1L]]$from, span[[1L]]$to), c(1L, 1L))
  hoge <- Filter(function(c) identical(c$label, "HOGE"), span)[[1L]]
  expect_identical(c(hoge$from, hoge$to), c(2L, 7L))   # Grade 0 .. Total
  expect_identical(tbl$col_header[[2L]],
                   c("  Category", "G0", "G1", "G2", "G3", "G4", "Total"))
})

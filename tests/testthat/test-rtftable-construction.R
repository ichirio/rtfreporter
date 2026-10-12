# rtftable() construction: defensive-contract / boundary-value / multi-DF tests,
# plus the rtf_tables() override path validating consistently with the
# constructor.  These exercise public input-validation behaviour, not internals.

df2 <- function() data.frame(a = c("1", "2"), b = c("x", "y"),
                             stringsAsFactors = FALSE)

# ── Argument validation (equivalence partitioning + boundary values) ─────────

silence_rtf_tables_deprecation()   # the old arguments' own tests (#594)

test_that('rtftable(border = "none") is accepted and means no borders', {
  none <- rtftable(df2(), border = "none")
  null <- rtftable(df2(), border = NULL)
  expect_s3_class(none, "rtftable")
  expect_null(none$border)                       # no borders
  expect_identical(none$border, null$border)     # same as border = NULL
})

test_that('rtf_tables() accepts a "none" border override', {
  tbl <- rtftable(df2(), border = "tfl")
  out <- rtf_tables(rtf_document(), tbl, border = "none")$contents[[1L]]
  expect_null(out$border)
})

test_that("rtftable() rejects a non-rtf_table_style `style`", {
  expect_error(rtftable(df2(), style = "nope"),
               "must be an rtf_table_style")
})

test_that("rtftable() validates enumerated arguments", {
  expect_error(rtftable(df2(), table_align = "middle"),
               "'left', 'center', or 'right'")
  expect_error(rtftable(df2(), cell_valign = "middle"),
               "'top', 'center', or 'bottom'")
})

test_that("rtftable() requires a scalar-logical row_height_exact", {
  expect_error(rtftable(df2(), row_height_exact = "yes"), "TRUE or FALSE")
  expect_error(rtftable(df2(), row_height_exact = c(TRUE, FALSE)),
               "TRUE or FALSE")
})

test_that("table_width_pct boundary-value analysis", {
  # Valid upper boundary.
  expect_silent(rtftable(df2(), table_width_pct = 100))
  # Just outside the valid range.
  expect_error(rtftable(df2(), table_width_pct = 0),   "\\(0, 100\\]")
  expect_error(rtftable(df2(), table_width_pct = 101), "\\(0, 100\\]")
  # 50% maps to a 0.5 fraction.
  tbl <- rtftable(df2(), table_width_pct = 50)
  expect_equal(tbl$table_width_pct_of_writable, 0.5)
})

test_that("cell_styles must be a list whose length equals the data rows", {
  expect_error(rtftable(df2(), cell_styles = "x"), "must be a list")
  # df2() has 2 rows; a length-1 list is a boundary mismatch.
  expect_error(rtftable(df2(), cell_styles = list(NULL)),
               "must equal the number of data rows")
  expect_silent(rtftable(df2(), cell_styles = list(NULL, NULL)))
})

# ── Multi-DF mode ────────────────────────────────────────────────────────────

test_that("rtftable() builds a multi-DF table and stores data_list", {
  tbl <- rtftable(list(df2(), df2()))
  expect_s3_class(tbl, "rtftable")
  expect_null(tbl$data)
  expect_length(tbl$data_list, 2L)
})

test_that("multi-DF construction validates its inputs", {
  expect_error(rtftable(list(df2(), "x")),    "must be a data.frame")
  expect_error(rtftable(list(df2(),
                             data.frame(a = 1, b = 2, c = 3))),
               "same number of columns")
  expect_error(rtftable(list()),
               "data.frame or a non-empty list")
})

test_that("multi-DF blank_rows accepts integer positions only", {
  # by_change / by_rule specs are unsupported in multi-DF mode.
  expect_error(rtftable(list(df2(), df2()),
                        blank_rows = blank_rows_by_change("a")),
               "integer vector of positions")
  # Out-of-domain integer (< -1) is rejected (boundary just below -1).
  expect_error(rtftable(list(df2(), df2()), blank_rows = -2L),
               "must be -1, 0, or positive")
  # -1 (= after last row) and 0 (= before first) are the valid boundaries.
  expect_silent(rtftable(list(df2(), df2()), blank_rows = c(-1L, 0L, 1L)))
})

# ── rtf_tables() override path validates like the constructor ────────────────

test_that("rtf_tables() overrides reject invalid values consistently", {
  doc <- rtf_document()
  tbl <- rtftable(df2())
  expect_error(rtf_tables(doc, tbl, table_width_pct = 200),
               "\\(0, 100\\]")
  expect_error(rtf_tables(doc, tbl, table_align = "middle"),
               "'left', 'center', or 'right'")
  expect_error(rtf_tables(doc, tbl, row_height_exact = "x"),
               "TRUE or FALSE")
})

test_that("rtf_tables() override actually replaces the field", {
  doc <- rtf_document()
  tbl <- rtftable(df2(), table_align = "left")
  doc <- rtf_tables(doc, tbl, table_align = "center")
  expect_identical(doc$contents[[1L]]$table_align, "center")
})

test_that("rtf_tables() applies the full range of explicit overrides", {
  tbl <- rtftable(df2())
  out <- rtf_tables(
    rtf_document(), tbl,
    col_rel_width            = c(2, 1),
    column_widths_twips      = c(1200L, 800L),
    row_height_twips         = 300L,
    header_row_height_twips  = 280L,
    blank_row_height_twips   = 120L,
    cell_padding_left_twips  = 40L,
    cell_padding_right_twips = 50L,
    cell_valign              = "center",
    border                   = NULL,
    col_header               = c("Col A", "Col B"),
    blank_rows               = c(1L),
    col_spec                 = list(list(col = 1L, align = "right"))
  )$contents[[1L]]

  expect_equal(out$column_widths_twips, c(1200L, 800L))
  expect_identical(out$row_height_twips, 300L)
  expect_identical(out$header_row_height_twips, 280L)
  expect_identical(out$blank_row_height_twips, 120L)
  expect_identical(out$cell_padding_left_twips, 40L)
  expect_identical(out$cell_padding_right_twips, 50L)
  expect_identical(out$cell_valign, "center")
  expect_equal(out$blank_rows, 1L)
  # col_spec override: align applied and header_align re-inherits it.
  expect_identical(out$col_spec[[1L]]$align, "right")
  expect_identical(out$col_spec[[1L]]$header_align, "right")
})

test_that("rtf_tables() col_spec override requires a `col` key", {
  tbl <- rtftable(df2())
  expect_error(
    rtf_tables(rtf_document(), tbl, col_spec = list(list(align = "right"))),
    "must be a list with a `col` key"
  )
})

test_that("rtf_tables() override clears blank_rows when passed NULL explicitly", {
  tbl <- rtftable(df2(), blank_rows = c(1L))
  out <- rtf_tables(rtf_document(), tbl, blank_rows = NULL)$contents[[1L]]
  expect_null(out$blank_rows)
})

# ── row_title + default alignment (#98) ──────────────────────────────────────

df7 <- function() data.frame(Lab = "x", A = "1", B = "2", C = "3",
                             D = "4", E = "5", F = "6", stringsAsFactors = FALSE)
.aligns <- function(tbl) vapply(tbl$col_spec, function(s) s$align, character(1))

test_that("default alignment: first column left, the rest centre (#98)", {
  expect_identical(.aligns(rtftable(df7())),
                   c("left", rep("center", 6L)))
})

test_that("row_title designates the left-aligned heading columns (#98)", {
  expect_identical(.aligns(rtftable(df7(), row_title = c(1, 2))),
                   c("left", "left", rep("center", 5L)))
  # by name
  expect_identical(.aligns(rtftable(df7(), row_title = "Lab")),
                   c("left", rep("center", 6L)))
})

test_that("explicit col_spec align overrides the row_title default (#98)", {
  tbl <- rtftable(df7(), col_spec = list(list(col = 2, align = "right")))
  expect_identical(.aligns(tbl)[2L], "right")
  # column headers follow the data alignment by default
  expect_identical(tbl$col_spec[[2L]]$header_align, "right")
})

test_that("row_title validates its range / names (#98)", {
  expect_error(rtftable(df7(), row_title = 99), "must be within")
  expect_error(rtftable(df7(), row_title = "Nope"), "not found in data")
})

test_that("rtf_tables(row_title=) re-aligns only still-default columns (#98)", {
  tbl <- rtftable(df7())                                  # col1 left, rest center
  out <- rtf_tables(rtf_document(), tbl, row_title = c(1, 2))$contents[[1L]]
  expect_identical(.aligns(out), c("left", "left", rep("center", 5L)))
})

test_that("rtf_tables(row_title=) applies to bare data.frames in bulk (#98)", {
  out <- rtf_tables(rtf_document(), list(df7(), df7()),
                    row_title = c(1, 2))$contents
  expect_identical(.aligns(out[[1L]]), c("left", "left", rep("center", 5L)))
  expect_identical(.aligns(out[[2L]]), c("left", "left", rep("center", 5L)))
})

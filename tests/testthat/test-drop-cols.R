## tests/testthat/test-drop-cols.R
##
## as_rtftables(drop_cols=): hide a column from the printed pages while still
## using it for pagination / grouping.  Covers the core hide behaviour, the
## metadata reindexing (col_header flat + spanning, col_spec, widths,
## col_header_align, row_title, cell_styles), and validation.

library(testthat)

df4 <- function() {
  data.frame(
    grp   = c("A", "A", "B", "B"),
    Label = c("x1", "x2", "y1", "y2"),
    N     = c("1", "2", "3", "4"),
    Pct   = c("10", "20", "30", "40"),
    stringsAsFactors = FALSE
  )
}

# ── core: hide a grouping column ──────────────────────────────────────────────

test_that("drop_cols removes the named column from every page", {
  res <- as_rtftables(df4(), drop_cols = "grp")
  expect_length(res, 1L)
  expect_identical(names(res[[1L]]$data), c("Label", "N", "Pct"))
})

test_that("drop_cols accepts an integer index", {
  res <- as_rtftables(df4(), drop_cols = 2L)        # drop "Label"
  expect_identical(names(res[[1L]]$data), c("grp", "N", "Pct"))
})

test_that("drop_cols accepts several columns (a list mixing names + indices)", {
  res <- as_rtftables(df4(), drop_cols = list("grp", 3L))
  expect_identical(names(res[[1L]]$data), c("Label", "Pct"))
})

test_that("a hidden column still drives by_value pagination + page names", {
  res <- as_rtftables(df4(), split = "by_value", group_col = "grp",
                      drop_cols = "grp")
  expect_length(res, 2L)
  expect_identical(names(res), c("A", "B"))
  # grp gone from the rendered pages, the rest kept
  expect_identical(names(res[[1L]]$data), c("Label", "N", "Pct"))
  expect_identical(res[[1L]]$data$Label, c("x1", "x2"))
})

test_that("a hidden carrier column still drives group_force pagination", {
  res <- as_rtftables(df4(), split = "group_force", group_col = 1L,
                      max_rows = 2L, drop_cols = 1L)
  expect_length(res, 2L)
  expect_false("grp" %in% names(res[[1L]]$data))
})

test_that("drop_cols is honoured through a list input", {
  res <- as_rtftables(list(t1 = df4(), t2 = df4()), drop_cols = "grp")
  expect_length(res, 2L)
  expect_identical(names(res[[1L]]$data), c("Label", "N", "Pct"))
  expect_identical(names(res[[2L]]$data), c("Label", "N", "Pct"))
})

test_that("as_rtftable() inherits drop_cols", {
  rt <- as_rtftable(df4(), drop_cols = "grp")
  expect_s3_class(rt, "rtftable")
  expect_identical(names(rt$data), c("Label", "N", "Pct"))
})

# ── metadata reindexing ───────────────────────────────────────────────────────

test_that("user col_header is final-coords; widths / col_header_align still reindex", {
  # A USER col_header now addresses the FINAL printed columns (3 after drop),
  # so it is written for those 3 columns directly -- not the pre-drop 4.
  # Widths / col_header_align are unchanged (still pre-drop, reindexed).
  rt <- as_rtftables(
    df4(), drop_cols = "grp",
    col_header       = c("Subject", "Count", "Percent"),   # final 3 columns
    col_rel_width    = c(1, 3, 2, 2),                       # pre-drop 4 -> reindexed
    col_header_align = c("left", "left", "center", "center")
  )[[1L]]
  expect_identical(unlist(rt$col_header), c("Subject", "Count", "Percent"))
  expect_identical(rt$col_rel_width, c(3, 2, 2))
  expect_identical(ncol(rt$data), 3L)
})

test_that("col_spec reindexes: integer remapped, dropped entry removed, name kept", {
  rt <- as_rtftables(
    df4(), drop_cols = 2L,                      # drop "Label"
    col_spec = list(
      list(col = 1L,    align = "left"),        # grp  -> kept, remap to 1
      list(col = 2L,    align = "right"),       # Label-> dropped, entry gone
      list(col = "Pct", align = "right")        # by name, survives
    )
  )[[1L]]
  # normalized col_spec is positional, one entry per kept column
  expect_length(rt$col_spec, 3L)
  aligns <- vapply(rt$col_spec, function(s) s$align, character(1L))
  expect_identical(aligns, c("left", "center", "right"))  # grp, N(default), Pct
})

test_that("user spanning header is written in final-column coordinates", {
  # Final columns after dropping "grp" are: Label, N, Pct.  The span over
  # N,Pct is therefore final positions 2-3 (no reindexing for user headers).
  sph <- list(
    list(col_cell(pos = c(2, 3), label = "Stats")),
    c("Label", "N", "Pct")
  )
  rt <- as_rtftables(df4(), drop_cols = "grp", col_header = sph)[[1L]]
  expect_identical(rt$col_header[[2L]], c("Label", "N", "Pct"))
  span <- Filter(function(c) identical(c$label, "Stats"), rt$col_header[[1L]])
  expect_length(span, 1L)
  expect_identical(c(span[[1L]]$from, span[[1L]]$to), c(2L, 3L))
})

test_that(".reindex_col_header drops a spanning cell fully inside removed columns", {
  # The AUTO (adapter-derived) header still travels through this reindexer;
  # a cell covering only dropped columns is removed.
  ch <- list(
    list(col_cell(pos = c(1, 2), label = "Hidden"),
         col_cell(pos = c(3, 4), label = "Stats")),
    c("grp", "Label", "N", "Pct")
  )
  out <- rtfreporter:::.reindex_col_header(ch, keep = c(3L, 4L), n0 = 4L)
  labels <- vapply(out[[1L]], function(c) if (is.null(c$label)) "" else c$label,
                   character(1L))
  expect_false("Hidden" %in% labels)
  expect_true("Stats" %in% labels)
  expect_identical(out[[2L]], c("N", "Pct"))
})

test_that("cell_styles per-column vectors reindex to kept columns", {
  cs <- replicate(4, list(bold = c(TRUE, FALSE, FALSE, TRUE)),
                  simplify = FALSE)
  rt <- as_rtftables(df4(), drop_cols = 1L, cell_styles = cs)[[1L]]
  expect_identical(length(rt$cell_styles[[1L]]$bold), 3L)
  expect_identical(rt$cell_styles[[1L]]$bold, c(FALSE, FALSE, TRUE))
})

test_that("row_title reindexes; dropped names fall away", {
  # row_title names grp + Label; grp is dropped, Label remains the heading col
  rt <- as_rtftables(df4(), drop_cols = "grp",
                     row_title = c("grp", "Label"))[[1L]]
  # Label is now column 1 and left-aligned (row-heading default)
  expect_identical(rt$col_spec[[1L]]$align, "left")
})

# ── validation ────────────────────────────────────────────────────────────────

test_that("drop_cols cannot remove every column", {
  expect_error(as_rtftables(df4(), drop_cols = c(1L, 2L, 3L, 4L)),
               "leave at least one column")
})

test_that("drop_cols rejects unknown names and out-of-range indices", {
  expect_error(as_rtftables(df4(), drop_cols = "nope"), "not found")
  expect_error(as_rtftables(df4(), drop_cols = 99L), "out of range")
})

test_that("drop_cols = NULL is a no-op", {
  res <- as_rtftables(df4(), drop_cols = NULL)
  expect_identical(names(res[[1L]]$data), c("grp", "Label", "N", "Pct"))
})

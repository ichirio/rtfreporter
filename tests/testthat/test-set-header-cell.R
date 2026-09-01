## tests/testthat/test-set-header-cell.R
##
## set_header_cell(): pinpoint set/merge of individual column-header cells
## (spanning, borders, alignment) on a built table, preserving other cells.

library(testthat)

df5 <- function() {
  data.frame(Item = "x", g1 = 1, g2 = 2, g3 = 3, Total = 4,
             stringsAsFactors = FALSE)
}
tbl5 <- function() {
  rtftable(df5(), col_header = c("Item", "N", "Mean", "SD", "Total"))
}
span_of <- function(row, label) {
  Filter(function(c) identical(c$label, label), row)[[1L]]
}

# ── merge single cells into a span (on a label row) ────────────────────────

test_that("merges separate cells into one span, preserving the others", {
  tbl <- set_header_cell(tbl5(), col_cell(c("g1", "g3"), "Statistics"), row = 1)
  r <- tbl$col_header[[1L]]
  expect_true(is.list(r))                 # promoted to cells
  expect_length(r, 3L)                    # Item | Statistics | Total
  s <- span_of(r, "Statistics")
  expect_identical(c(s$from, s$to), c(2L, 4L))
  # untargeted cells preserved
  expect_identical(span_of(r, "Item")$from,  1L)
  expect_identical(span_of(r, "Total")$from, 5L)
})

test_that("numeric positions work the same as names", {
  tbl <- set_header_cell(tbl5(), col_cell(c(2L, 4L), "Statistics"), row = 1)
  expect_identical(c(span_of(tbl$col_header[[1L]], "Statistics")$from,
                     span_of(tbl$col_header[[1L]], "Statistics")$to),
                   c(2L, 4L))
})

test_that("several cells can be placed at once", {
  tbl <- set_header_cell(tbl5(),
    col_cell(c("g1", "g2"), "A"),
    col_cell("g3", "B"),
    row = 1)
  r <- tbl$col_header[[1L]]
  expect_identical(c(span_of(r, "A")$from, span_of(r, "A")$to), c(2L, 3L))
  expect_identical(c(span_of(r, "B")$from, span_of(r, "B")$to), c(4L, 4L))
})

# ── borders / alignment carried by col_cell() ──────────────────────────────

test_that("border and alignment on the col_cell are applied", {
  tbl <- set_header_cell(tbl5(),
    col_cell(c("g1", "g3"), "Stat", align = "center",
             border = rtf_border(bottom = rtfreporter:::.rtf_border_side("none"))),
    row = 1)
  s <- span_of(tbl$col_header[[1L]], "Stat")
  expect_identical(s$align, "center")
  expect_s3_class(s$border, "rtf_border")
})

# ── end-to-end render ──────────────────────────────────────────────────────

test_that("the merged span renders", {
  tbl <- set_header_cell(tbl5(), col_cell(c("g1", "g3"), "Statistics"), row = 1)
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_tables(list(tbl))
  txt <- .render_to_string(doc)
  expect_match(txt, "Statistics")
  expect_match(txt, "Total")
})

# ── boundary alignment on an existing spanning row ─────────────────────────

test_that("a target that would split an existing span errors", {
  tbl <- rtftable(df5(), col_header = rtf_col_header(
    list(col_cell(1, ""), col_cell(c(2, 4), "Stat"), col_cell(5, "")),
    c("Item", "N", "Mean", "SD", "Total")))
  # cols 4-5 do not align to the [2-4] span's boundaries
  expect_error(
    set_header_cell(tbl, col_cell(c("g3", "Total"), "X"), row = 1),
    "do not align")
})

test_that("requested cells that overlap each other error", {
  expect_error(
    set_header_cell(tbl5(), col_cell(c("g1", "g2"), "A"),
                    col_cell(c("g2", "g3"), "B"), row = 1),
    "overlap")
})

# ── argument validation ────────────────────────────────────────────────────

test_that("validation errors", {
  expect_error(set_header_cell(tbl5(), col_cell(1, "X"), row = 99), "must be in 1")
  expect_error(set_header_cell(tbl5(), row = 1), "at least one col_cell")
  expect_error(set_header_cell(tbl5(), "nope", row = 1), "col_cell")
  expect_error(set_header_cell(tbl5(), col_cell(1, "X")), "`row`.*required")
})

# ── list method (per page) ─────────────────────────────────────────────────

test_that("set_header_cell() maps over a page list", {
  pages <- as_rtftables(df5(), col_header = c("Item", "N", "Mean", "SD", "Total"))
  out <- set_header_cell(pages, col_cell(c("g1", "g3"), "Statistics"), row = 1)
  expect_identical(c(span_of(out[[1L]]$col_header[[1L]], "Statistics")$from,
                     span_of(out[[1L]]$col_header[[1L]], "Statistics")$to),
                   c(2L, 4L))
})

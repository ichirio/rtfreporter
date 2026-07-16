## tests/testthat/test-border-colors.R
##
## #224: border colours on table cells must reach the output as \brdrcf
## commands (they were collected into \colortbl but never referenced).
## Covers every border channel: zone, col_spec, col_cell, cell_styles, and
## the gt "styles" reader.

library(testthat)

.bc_rtf <- function(tbl) {
  f <- tempfile(fileext = ".rtf")
  generate_rtfreport(rtf_document() |> rtf_tables(list(tbl)), f)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

.bc_df <- function() {
  data.frame(a = c("1", "2"), b = c("3", "4"), stringsAsFactors = FALSE)
}

RED_TBL <- "\\\\red255\\\\green0\\\\blue0"

test_that("zone border colours render as \\brdrcf", {
  tbl <- rtftable(.bc_df(), border = rtf_table_border(
    body = rtf_border(bottom = rtf_border_side(color = "#FF0000"))))
  rtf <- .bc_rtf(tbl)
  expect_match(rtf, RED_TBL)                       # in the colour table
  expect_match(rtf, "\\\\clbrdrb\\\\brdrs\\\\brdrw15\\\\brdrcf[0-9]+")
})

test_that("col_cell() header border colours render as \\brdrcf", {
  hdr <- rtf_col_header(
    list(col_cell(1, ""),
         col_cell(2, "B", border = rtf_border(
           bottom = rtf_border_side("double", color = "#003366")))),
    c("A", "B")
  )
  rtf <- .bc_rtf(rtftable(.bc_df(), col_header = hdr, border = "tfl"))
  expect_match(rtf, "\\\\red0\\\\green51\\\\blue102")
  expect_match(rtf, "\\\\clbrdrb\\\\brdrdb\\\\brdrw15\\\\brdrcf[0-9]+")
})

test_that("col_spec border colours render as \\brdrcf", {
  tbl <- rtftable(.bc_df(), border = "tfl",
                  col_spec = list(list(col = 2, border = rtf_border(
                    bottom = rtf_border_side("thick", color = "#FF0000")))))
  expect_match(.bc_rtf(tbl), "\\\\brdrth\\\\brdrw15\\\\brdrcf[0-9]+")
})

test_that("style_body(border = ) colours render as \\brdrcf", {
  tbl <- rtftable(.bc_df(), border = "tfl") |>
    style_body(rows = 1, cols = 2, border = rtf_border(
      bottom = rtf_border_side(color = "#FF0000")))
  rtf <- .bc_rtf(tbl)
  expect_match(rtf, RED_TBL)
  expect_match(rtf, "\\\\clbrdrb\\\\brdrs\\\\brdrw15\\\\brdrcf[0-9]+")
})

test_that("a coloured gt tab_style border arrives coloured end-to-end", {
  skip_if_not_installed("gt")
  g <- gt::gt(.bc_df()) |>
    gt::tab_style(style = gt::cell_borders(sides = "bottom",
                                           color = "#FF0000",
                                           weight = gt::px(2)),
                  locations = gt::cells_body(columns = b, rows = 1))
  rtf <- .bc_rtf(as_rtftable(g, read_meta = TRUE))
  expect_match(rtf, RED_TBL)
  expect_match(rtf, "\\\\clbrdrb\\\\brdrs\\\\brdrw30\\\\brdrcf[0-9]+")
})

test_that("black borders emit no \\brdrcf (RTF default colour)", {
  tbl <- rtftable(.bc_df(), border = "tfl") |>
    style_body(rows = 1, cols = 2,
               border = rtf_border(bottom = rtf_border_side()))
  expect_false(grepl("\\\\brdrcf", .bc_rtf(tbl)))
})

## tests/testthat/test-col-header-from-names.R
##
## col_header_from_names(): build a spanning rtf_col_header from delimited
## column names (the reconstruction as_rtftables() applies automatically).

library(testthat)

test_that("delimited names build a two-row spanning header", {
  hdr <- col_header_from_names(
    c("Item", "Drug A____N", "Drug A____Mean", "Drug B____N", "Drug B____Mean"))
  expect_s3_class(hdr, "rtf_col_header")
  expect_length(hdr, 2L)                       # spanning row + leaf row

  # Top row: Item bottom-aligned (blank on top), then Drug A / Drug B spans.
  top <- hdr[[1L]]
  labs <- vapply(top, function(c) c$label, character(1L))
  expect_true("Drug A" %in% labs)
  expect_true("Drug B" %in% labs)
  drugA <- Filter(function(c) identical(c$label, "Drug A"), top)[[1L]]
  expect_identical(drugA$pos, c(2L, 3L))       # spans cols 2-3

  # Leaf row: one label per column.
  expect_identical(hdr[[2L]], c("Item", "N", "Mean", "N", "Mean"))
})

test_that("names with no separator yield a single flat label row", {
  hdr <- col_header_from_names(c("A", "B", "C"))
  expect_s3_class(hdr, "rtf_col_header")
  expect_length(hdr, 1L)
  expect_identical(hdr[[1L]], c("A", "B", "C"))
})

test_that("a data.frame is accepted (its names are used)", {
  df  <- data.frame(Item = 1, `G1____N` = 1, `G1____M` = 1, check.names = FALSE)
  hdr <- col_header_from_names(df)
  expect_identical(hdr[[2L]], c("Item", "N", "M"))
  expect_identical(Filter(function(c) identical(c$label, "G1"), hdr[[1L]])[[1L]]$pos,
                   c(2L, 3L))
})

test_that("a custom separator is honoured", {
  hdr <- col_header_from_names(c("id", "A|n", "A|m"), sep = "|")
  expect_identical(hdr[[2L]], c("id", "n", "m"))
  expect_identical(Filter(function(c) identical(c$label, "A"), hdr[[1L]])[[1L]]$pos,
                   c(2L, 3L))
})

test_that("the result applies through set_col_header()", {
  df  <- data.frame(Item = "x", A_N = 1L, A_M = 2.5, B_N = 3L, B_M = 4.5,
                    stringsAsFactors = FALSE)
  hdr <- col_header_from_names(
    c("Item", "Drug A____N", "Drug A____Mean", "Drug B____N", "Drug B____Mean"))
  tbl <- set_col_header(rtftable(df), hdr)
  expect_length(tbl$col_header, 2L)
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_tables(list(tbl))
  txt <- .render_to_string(doc)
  expect_match(txt, "Drug A")
  expect_match(txt, "Mean")
})

test_that("col_header_from_names() errors on empty input", {
  expect_error(col_header_from_names(character(0)), "non-empty")
})

## tests/testthat/test-border-inside.R
##
## #340: Word-style "outer frame / inside horizontal / inside vertical" border
## specification on rtf_border() and rtf_table_border().
##
## The whole feature is additive: with every new argument left NULL the output
## must be byte-identical to what the package produced before, which is what
## the "unchanged" tests below pin down.

library(testthat)

.bi_df <- function() {
  data.frame(
    C = c("Age", "  Mean", "Sex", "  Male"),
    A = c("", "62.1", "", "45"),
    B = c("", "61.4", "", "41"),
    stringsAsFactors = FALSE
  )
}

.bi_hdr <- function() {
  rtf_col_header(
    list(col_cell(1, ""), col_cell(c(2, 3), "Treatment Group")),
    c("Characteristic", "Drug A", "Drug B")
  )
}

.bi_rtf <- function(tbl) {
  f <- tempfile(fileext = ".rtf")
  generate_rtfreport(rtf_document() |> rtf_tables(list(tbl)), f)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

# Per-row, per-cell summary of which edges each cell carries: one character
# vector per table row, one string per cell, letters drawn from t/b/l/r.
.bi_sides <- function(tbl) {
  rtf  <- .bi_rtf(tbl)
  rows <- Filter(function(r) grepl("trowd", r, fixed = TRUE),
                 strsplit(rtf, "\\row", fixed = TRUE)[[1L]])
  lapply(rows, function(r) {
    defs <- strsplit(r, "\\cellx", fixed = TRUE)[[1L]]
    defs <- defs[-length(defs)]
    vapply(defs, function(d) {
      m <- gregexpr("clbrdr([tblr])", d)[[1L]]
      if (m[1L] == -1L) return("-")
      hits <- vapply(regmatches(d, gregexpr("clbrdr[tblr]", d))[[1L]],
                     function(x) substr(x, nchar(x), nchar(x)), character(1L))
      paste(sort(unique(hits)), collapse = "")
    }, character(1L), USE.NAMES = FALSE)
  })
}

.bi_tbl <- function(border) {
  rtftable(.bi_df(), col_header = .bi_hdr(),
           column_widths_twips = c(4320L, 2880L, 2880L), border = border)
}


# -- constructors ---------------------------------------------------------------

test_that("rtf_border() carries inside_h / inside_v, defaulting to NULL", {
  b <- rtf_border()
  expect_null(b$inside_h)
  expect_null(b$inside_v)

  s <- rtf_border_side("double", 30L)
  b <- rtf_border(inside_h = s, inside_v = s)
  expect_identical(b$inside_h, s)
  expect_identical(b$inside_v, s)
})

test_that("rtf_border() validates the two new slots", {
  expect_error(rtf_border(inside_h = "single"), "inside_h", fixed = TRUE)
  expect_error(rtf_border(inside_v = list(style = "single")), "inside_v",
               fixed = TRUE)
})

test_that("rtf_border_with() can set and preserve inside_h / inside_v", {
  s <- rtf_border_side()
  b <- rtf_border(top = s, inside_v = s)
  expect_identical(rtf_border_with(b, bottom = s)$inside_v, s)   # preserved
  expect_identical(rtf_border_with(b, inside_h = s)$inside_h, s) # set
  expect_identical(rtf_border_with(b, inside_h = s)$top, s)      # untouched
})

test_that("rtf_table_border() validates outer / inside_h / inside_v", {
  expect_error(rtf_table_border(outer = rtf_border_side()), "outer",
               fixed = TRUE)
  expect_error(rtf_table_border(inside_h = rtf_border()), "inside_h",
               fixed = TRUE)
  expect_error(rtf_table_border(inside_v = "single"), "inside_v", fixed = TRUE)
})

test_that(".expand_table_border() folds the shortcuts into the zones", {
  s  <- rtf_border_side()
  tb <- .expand_table_border(
    rtf_table_border(outer = rtf_border_box(), inside_h = s), has_header = TRUE)

  expect_null(tb$outer)         # consumed
  expect_null(tb$inside_h)
  expect_identical(tb$header$top, s)        # table's top edge
  expect_identical(tb$last_row$bottom, s)   # table's bottom edge
  expect_identical(tb$body$inside_h, s)     # between data rows
  # An outer frame with no inside_v asked for plants an explicit "no line", so
  # left/right stay outer edges instead of reaching every cell.
  expect_identical(tb$body$inside_v$style, "none")
})

test_that(".expand_table_border() is a no-op when no shortcut is given", {
  tb <- rtf_table_border(body = rtf_border(bottom = rtf_border_side()))
  expect_identical(.expand_table_border(tb), tb)
})


# -- rendering ------------------------------------------------------------------

test_that("outer + inside_h gives a frame and row rules but no column rules", {
  sides <- .bi_sides(.bi_tbl(rtf_table_border(outer = rtf_border_box(),
                                              inside_h = rtf_border_side())))
  # Topmost row (the spanning row) carries the table's top edge.
  expect_true(grepl("t", sides[[1L]][[1L]], fixed = TRUE))
  # Every data row: bottom everywhere, left on the first cell, right on the last,
  # and nothing on the interior cell.
  for (i in 3:6) {
    expect_identical(sides[[i]], c("bl", "b", "br"))
  }
})

test_that("inside_v adds the column rules that inside_h alone does not", {
  sides <- .bi_sides(.bi_tbl(rtf_table_border(outer = rtf_border_box(),
                                              inside_h = rtf_border_side(),
                                              inside_v = rtf_border_side())))
  for (i in 3:6) {
    expect_identical(sides[[i]], c("blr", "blr", "blr"))
  }
})

test_that("outer alone frames the table without interior rules", {
  sides <- .bi_sides(.bi_tbl(rtf_table_border(outer = rtf_border_box())))
  for (i in 3:5) {
    expect_identical(sides[[i]], c("l", "-", "r"))   # no interior rules at all
  }
  expect_identical(sides[[6L]], c("bl", "b", "br"))  # last row: bottom edge
})

test_that("inside_v lands on cell boundaries, not column boundaries", {
  # The spanning row has two cells: the stub and a cell covering columns 2-3.
  # No rule may appear inside the merged cell.
  sides <- .bi_sides(.bi_tbl(rtf_table_border(outer = rtf_border_box(),
                                              inside_v = rtf_border_side())))
  expect_length(sides[[1L]], 2L)
  expect_true(grepl("r", sides[[1L]][[1L]], fixed = TRUE))   # the inside rule
  expect_true(grepl("l", sides[[1L]][[1L]], fixed = TRUE))   # the outer edge
})

test_that("a zone-level inside_v = \"none\" turns left/right into outer edges", {
  s <- rtf_border_side()
  sides <- .bi_sides(.bi_tbl(rtf_table_border(
    body = rtf_border(left = s, right = s,
                      inside_v = rtf_border_side("none")))))
  for (i in 3:6) {
    expect_identical(sides[[i]], c("l", "-", "r"))
  }
})

test_that("explicit zone arguments still win over the shortcuts", {
  tb <- .expand_table_border(
    rtf_table_border(outer    = rtf_border_box(),
                     inside_h = rtf_border_side(),
                     body     = rtf_border(inside_h = rtf_border_side("none"))),
    has_header = TRUE)
  expect_identical(tb$body$inside_h$style, "none")
  expect_identical(tb$header$top$style, "single")   # untouched by the override
})

test_that("with no header, the outer top edge lands on the first data row", {
  tb <- .expand_table_border(rtf_table_border(outer = rtf_border_box()),
                             has_header = FALSE)
  expect_null(tb$header$top)
  expect_identical(tb$first_row$top$style, "single")
})


# -- unchanged behaviour --------------------------------------------------------

test_that("zone-level left/right without inside_v still reach every cell", {
  s <- rtf_border_side()
  sides <- .bi_sides(.bi_tbl(rtf_table_border(
    body = rtf_border(left = s, right = s))))
  for (i in 3:6) {
    expect_identical(sides[[i]], c("lr", "lr", "lr"))
  }
})

test_that("the tfl preset renders exactly as before", {
  sides <- .bi_sides(.bi_tbl("tfl"))
  expect_identical(sides[[1L]][[1L]], "t")           # top rule, stub cell
  expect_identical(sides[[2L]], c("b", "b", "b"))    # bottom of header block
  for (i in 3:6) expect_identical(sides[[i]], c("-", "-", "-"))
})

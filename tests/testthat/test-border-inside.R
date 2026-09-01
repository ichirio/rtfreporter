## tests/testthat/test-border-inside.R
##
## #342: one border type. rtf_border() describes a *selection*, and where it
## applies is decided by where it is attached.
##
##   top / bottom / left / right  -> the selection's outer edges, always
##   inside_h / inside_v          -> the rules inside it, absent = no rule
##
## There is no second reading to switch to, so an edge never means something
## different depending on which other arguments are present. #340 briefly had
## such a mode; these tests pin down that it is gone.

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

# Per-row, per-cell summary of which edges each cell carries: one character
# vector per table row, one string per cell, letters drawn from t/b/l/r.
.bi_sides <- function(tbl) {
  f <- tempfile(fileext = ".rtf")
  generate_rtfreport(rtf_document() |> rtf_tables(list(tbl)), f)
  rtf  <- paste(readLines(f, warn = FALSE), collapse = "\n")
  rows <- Filter(function(r) grepl("trowd", r, fixed = TRUE),
                 strsplit(rtf, "\\row", fixed = TRUE)[[1L]])
  lapply(rows, function(r) {
    defs <- strsplit(r, "\\cellx", fixed = TRUE)[[1L]]
    defs <- defs[-length(defs)]
    vapply(defs, function(d) {
      hits <- regmatches(d, gregexpr("clbrdr[tblr]", d))[[1L]]
      if (!length(hits)) return("-")
      paste(sort(unique(substr(hits, nchar(hits), nchar(hits)))), collapse = "")
    }, character(1L), USE.NAMES = FALSE)
  })
}

.bi_tbl <- function(border) {
  rtftable(.bi_df(), col_header = .bi_hdr(),
           column_widths_twips = c(4320L, 2880L, 2880L), border = border)
}

# The deprecation warnings fire once per session; reset so each test sees them.
.bi_reset <- function() {
  rm(list = ls(rtfreporter:::.deprecation_state),
     envir = rtfreporter:::.deprecation_state)
}

S    <- rtfreporter:::.rtf_border_side()
NONE <- rtfreporter:::.rtf_border_side("none")
FRAME <- function(...) rtf_border(top = S, bottom = S, left = S, right = S, ...)


# -- constructor ----------------------------------------------------------------

test_that("rtf_border() carries inside_h / inside_v, defaulting to no rule", {
  b <- rtf_border()
  expect_null(b$inside_h)
  expect_null(b$inside_v)

  s <- rtfreporter:::.rtf_border_side("double", 30L)
  b <- rtf_border(inside_h = s, inside_v = s)
  expect_identical(b$inside_h, s)
  expect_identical(b$inside_v, s)
})

test_that("rtf_border() validates the two new slots", {
  expect_error(rtf_border(inside_h = "wiggly"), "inside_h", fixed = TRUE)
  expect_error(rtf_border(inside_v = list(style = "single")), "inside_v",
               fixed = TRUE)
  # ... and accepts the three legal spellings.
  expect_identical(rtf_border(inside_h = TRUE)$inside_h$style, "single")
  expect_identical(rtf_border(inside_v = "double")$inside_v$style, "double")
  expect_identical(rtf_border(inside_v = FALSE)$inside_v$style, "none")
})

test_that("rtf_border() records which inside_* the caller named", {
  expect_identical(attr(rtf_border(left = S), "inside_named"),
                   c(h = FALSE, v = FALSE))
  expect_identical(attr(rtf_border(left = S, inside_v = NONE), "inside_named"),
                   c(h = FALSE, v = TRUE))
})

test_that("rtf_border(from = ) sets and preserves inside_h / inside_v", {
  b <- rtf_border(top = S, inside_v = S)
  expect_identical(rtf_border(from = b, bottom = S)$inside_v, S)   # preserved
  expect_identical(rtf_border(from = b, inside_h = S)$inside_h, S) # set
  expect_identical(rtf_border(from = b, inside_h = S)$top, S)      # untouched
})


# -- the one rule ---------------------------------------------------------------

test_that("a whole-table rtf_border frames the table and nothing else", {
  sides <- .bi_sides(.bi_tbl(FRAME()))
  expect_true(grepl("t", sides[[1L]][[1L]], fixed = TRUE))   # table top
  for (i in 2:5) expect_identical(sides[[i]], c("l", "-", "r"))
  expect_identical(sides[[6L]], c("bl", "b", "br"))          # table bottom
})

test_that("inside_h adds the rules between rows", {
  sides <- .bi_sides(.bi_tbl(FRAME(inside_h = S)))
  for (i in 2:6) expect_identical(sides[[i]], c("bl", "b", "br"))
})

test_that("inside_v adds the rules between cells", {
  sides <- .bi_sides(.bi_tbl(FRAME(inside_h = S, inside_v = S)))
  for (i in 2:6) expect_identical(sides[[i]], c("blr", "blr", "blr"))
})

test_that("edges are outer-only whether or not inside_* is named", {
  # The whole point of #342: naming inside_v must not change what left/right
  # mean, only add rules of its own.
  a <- .bi_sides(.bi_tbl(FRAME()))
  b <- .bi_sides(.bi_tbl(FRAME(inside_v = NONE)))
  expect_identical(a, b)
})

test_that("inside_v lands on cell boundaries, not column boundaries", {
  # The spanning row is two cells: the stub, and one covering columns 2-3.
  # No rule may appear inside the merged cell.
  sides <- .bi_sides(.bi_tbl(FRAME(inside_v = S)))
  expect_length(sides[[1L]], 2L)
  expect_true(grepl("l", sides[[1L]][[1L]], fixed = TRUE))   # outer edge
  expect_true(grepl("r", sides[[1L]][[1L]], fixed = TRUE))   # interior rule
})

test_that("a zone border reads the same way, scoped to that row kind", {
  # bottom alone = the body block's outer bottom, i.e. the last data row only.
  t1 <- style_zone(.bi_tbl("none"),
                   body = rtf_border(bottom = S, inside_h = NONE))
  s1 <- .bi_sides(t1)
  for (i in 3:5) expect_identical(s1[[i]], c("-", "-", "-"))
  expect_identical(s1[[6L]], c("b", "b", "b"))

  # inside_h asks for the rule between the body's rows as well.
  t2 <- style_zone(.bi_tbl("none"), body = rtf_border(bottom = S, inside_h = S))
  s2 <- .bi_sides(t2)
  for (i in 3:6) expect_identical(s2[[i]], c("b", "b", "b"))
})

test_that("an interior horizontal rule is emitted once, not as both edges", {
  tbl <- style_zone(.bi_tbl("none"),
                    body = rtf_border(inside_h = S))
  sides <- .bi_sides(tbl)
  # Rows 3..5 carry the rule as their bottom; none of them carries a top.
  for (i in 3:5) expect_identical(sides[[i]], c("b", "b", "b"))
  expect_identical(sides[[6L]], c("-", "-", "-"))   # last row: no outer bottom
})


# -- expansion ------------------------------------------------------------------

test_that(".expand_table_border() folds a whole-table border into the zones", {
  tb <- rtfreporter:::.expand_table_border(
    rtfreporter:::.rtf_table_border(outer = FRAME(), inside_h = S),
    has_header = TRUE)

  expect_null(tb$outer)                       # consumed
  expect_null(tb$inside_h)
  expect_identical(tb$header$top, S)          # table's top edge
  expect_identical(tb$last_row$bottom, S)     # table's bottom edge
  expect_identical(tb$body$inside_h, S)       # between data rows
  expect_null(tb$body$inside_v)               # nothing planted: no rule asked for
})

test_that(".expand_table_border() is a no-op when no shortcut is given", {
  tb <- rtfreporter:::.rtf_table_border(body = rtf_border(bottom = S))
  expect_identical(rtfreporter:::.expand_table_border(tb), tb)
})

test_that("explicit zone arguments still win over a whole-table border", {
  tb <- rtfreporter:::.expand_table_border(
    rtfreporter:::.rtf_table_border(outer    = FRAME(),
                                    inside_h = S,
                                    body     = rtf_border(inside_h = NONE)),
    has_header = TRUE)
  expect_identical(tb$body$inside_h$style, "none")
  expect_identical(tb$header$top$style, "single")
})

test_that("with no header, the table's top edge lands on the first data row", {
  tb <- rtfreporter:::.expand_table_border(
    rtfreporter:::.rtf_table_border(outer = FRAME()), has_header = FALSE)
  expect_null(tb$header$top)
  expect_identical(tb$first_row$top$style, "single")
})


# -- deprecation ----------------------------------------------------------------

test_that("rtf_table_border() warns once and still builds the same value", {
  .bi_reset()
  expect_warning(tb <- rtf_table_border(header = rtf_border(top = S)),
                 "deprecated")
  expect_silent(tb2 <- rtf_table_border(header = rtf_border(top = S)))
  expect_identical(tb, tb2)
  expect_identical(tb, rtfreporter:::.rtf_table_border(header = rtf_border(top = S)))
})

test_that("the changed edge reading warns once, and only when it matters", {
  .bi_reset()
  # A zone border with left/right and no inside_v named: this is the shape whose
  # reading changed, so it warns -- once.
  vert <- rtf_border(left = S, right = S)
  expect_warning(style_zone(.bi_tbl("none"), body = vert), "outer")
  expect_silent(style_zone(.bi_tbl("none"), body = vert))   # once per session

  # Naming the interior rule says which reading you mean, so no warning.
  .bi_reset()
  expect_silent(style_zone(.bi_tbl("none"),
                           body = rtf_border(left = S, right = S, inside_v = S)))

  # A single-column table has no interior boundary to disagree about.
  .bi_reset()
  expect_silent(style_zone(rtftable(data.frame(a = c("1", "2")), border = "none"),
                           body = vert))

  # A whole-table border was an error before 0.5.0, so it has no old reading.
  .bi_reset()
  expect_silent(.bi_tbl(rtf_border(left = S, right = S)))
})

test_that("the tfl preset renders exactly as before and never warns", {
  .bi_reset()
  expect_silent(sides <- .bi_sides(.bi_tbl("tfl")))
  expect_identical(sides[[1L]][[1L]], "t")           # top rule, stub cell
  expect_identical(sides[[2L]], c("b", "b", "b"))    # bottom of header block
  for (i in 3:6) expect_identical(sides[[i]], c("-", "-", "-"))
})


# -- building versus layering ---------------------------------------------------

test_that("a call without `from` builds: unnamed sides are unset", {
  b <- rtf_border(top = TRUE)
  b2 <- rtf_border(bottom = TRUE)          # a fresh call, not an addition
  expect_null(b2$top)
  expect_identical(b2$bottom$style, "single")
})

test_that("`from` layers: unnamed sides survive with their own look", {
  b <- rtf_border(top = TRUE, color = "#C9372C", width = 30L)
  b <- rtf_border(from = b, bottom = TRUE, color = "#1F6FEB")
  b <- rtf_border(from = b, left = TRUE, right = TRUE,
                  style = "double", width = 45L, color = "#1A7F37")

  # Each layer's style/width/colour applies only to the sides it named.
  expect_identical(b$top$color,    "#C9372C")
  expect_identical(b$top$width,    30L)
  expect_identical(b$bottom$color, "#1F6FEB")
  expect_identical(b$bottom$width, 15L)
  expect_identical(b$left$style,   "double")
  expect_identical(b$right$color,  "#1A7F37")
  expect_null(b$inside_h)
})

test_that("a side can be set, changed or erased by a layer, but not unset", {
  b <- rtf_border(top = TRUE, bottom = TRUE)
  expect_identical(rtf_border(from = b, top = "double")$top$style, "double")
  expect_identical(rtf_border(from = b, top = FALSE)$top$style,    "none")
  # NULL means "say nothing here", so the side keeps what `from` had.
  expect_identical(rtf_border(from = b, top = NULL)$top, b$top)
})

test_that("unset and erased are different states", {
  expect_null(rtf_border()$top)                       # unset: inherits
  expect_identical(rtf_border(all = FALSE)$top$style, "none")  # erased
})

test_that("FALSE and \"none\" are interchangeable, whatever the call's line is", {
  expect_identical(rtf_border(top = TRUE, bottom = FALSE),
                   rtf_border(top = "single", bottom = "none"))
  expect_identical(rtf_border(top = TRUE, bottom = FALSE,
                              style = "double", width = 30L),
                   rtf_border(top = "double", bottom = "none", width = 30L))
})

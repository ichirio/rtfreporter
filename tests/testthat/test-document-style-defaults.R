## tests/testthat/test-document-style-defaults.R
##
## Document-wide style defaults (row height, cell padding) resolved through
## option / style, overridable per module (issue #124).

library(testthat)

# Render a document to RTF text.
.render_text <- function(doc) {
  f <- tempfile(fileext = ".rtf"); on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}
# Distinct \trrh values (row heights) present in the RTF.
.heights <- function(s) {
  sort(unique(regmatches(s, gregexpr("trrh-?[0-9]+", s))[[1L]]))
}
# Distinct \li values (left paragraph indent = left cell padding) present.
.lefts <- function(s) {
  sort(unique(regmatches(s, gregexpr("\\\\li[0-9]+", s))[[1L]]))
}

.df  <- data.frame(A = c("x", "y"), B = c("1", "2"), stringsAsFactors = FALSE)
.hdr <- rtf_header(rows = list(c(l = "L", r = "R")))
.ftr <- rtf_footer(rows = list(c(l = "F")))

.full_doc <- function(tbl = .df) {
  rtf_document() |>
    rtf_tables(tbl) |>
    rtf_titles(list("Title")) |>
    rtf_footnotes(list("Note")) |>
    rtf_section(page = 1, secinfo = list(header = .hdr, footer = .ftr))
}

# Always restore factory defaults so tests do not leak option state.
.with_reset <- function(code) {
  on.exit(rtfreporter_reset_defaults(), add = TRUE)
  force(code)
}


# -- backward compatibility --------------------------------------------------

test_that("defaults are unchanged (font-aware 230 twips, 0 padding)", {
  s <- .render_text(.full_doc())
  expect_identical(.heights(s), "trrh230")
  expect_identical(.lefts(s), "\\li0")
})


# -- document-wide via option ------------------------------------------------

test_that("rtfreporter.row_height_twips sets every element's row height", {
  .with_reset({
    options(rtfreporter.row_height_twips = 400L)
    s <- .render_text(.full_doc())
    # The ONLY row height present is the document default -> header band,
    # title, footnote, column-header and data rows all use it.
    expect_identical(.heights(s), "trrh400")
  })
})

test_that("rtfreporter.cell_padding_left_twips sets every element's left padding", {
  .with_reset({
    options(rtfreporter.cell_padding_left_twips = 120L)
    s <- .render_text(.full_doc())
    expect_identical(.lefts(s), "\\li120")
  })
})


# -- document-wide via style (per document) ----------------------------------

test_that("rtf_config(style=) sets the document-wide defaults", {
  d <- .full_doc() |>
    rtf_config(style = rtf_style(row_height_twips = 350L,
                                 cell_padding_left_twips = 90L))
  s <- .render_text(d)
  expect_identical(.heights(s), "trrh350")
  expect_identical(.lefts(s), "\\li90")
})


# -- per-module override beats the document default --------------------------

test_that("rtftable(row_height_twips=) overrides the document default", {
  .with_reset({
    options(rtfreporter.row_height_twips = 400L)
    d <- rtf_document() |>
      rtf_tables(rtftable(.df, row_height_twips = 500L)) |>
      rtf_section(page = 1, secinfo = list(header = .hdr, footer = NULL))
    s <- .render_text(d)
    # Content rows use 500 (per-module); the header band inherits the doc 400.
    expect_setequal(.heights(s), c("trrh400", "trrh500"))
  })
})

test_that("rtftable(cell_padding_left_twips=) overrides the document default", {
  .with_reset({
    options(rtfreporter.cell_padding_left_twips = 120L)
    d <- rtf_document() |>
      rtf_tables(rtftable(.df, cell_padding_left_twips = 40L)) |>
      rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL))
    s <- .render_text(d)
    # The content cells use the table's own 40 (the blank title-gap row still
    # inherits the document default, which is the intended behaviour).
    expect_true("\\li40" %in% .lefts(s))
  })
})

test_that("explicit padding of 0 is honoured (not treated as 'unset')", {
  .with_reset({
    options(rtfreporter.cell_padding_left_twips = 120L)
    d <- rtf_document() |>
      rtf_tables(rtftable(.df, cell_padding_left_twips = 0L)) |>
      rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL))
    s <- .render_text(d)
    # Explicit 0 wins for the content cells (distinct from the document 120,
    # which only the blank title-gap row inherits).
    expect_true("\\li0" %in% .lefts(s))
  })
})


# -- rtf_table_style seeds padding (the repaired dead-code path) -------------

test_that("rtf_table_style cell padding seeds an unset rtftable", {
  style <- rtf_table_style(cell_padding_left_twips = 60L)
  tbl   <- rtftable(.df, style = style)
  expect_identical(tbl$cell_padding_left_twips, 60L)
  # An explicit table padding still wins over the style.
  tbl2  <- rtftable(.df, style = style, cell_padding_left_twips = 10L)
  expect_identical(tbl2$cell_padding_left_twips, 10L)
})


# -- options surface ---------------------------------------------------------

test_that("the new keys are part of the configurable defaults", {
  opts <- rtfreporter_options()
  expect_true(all(c("rtfreporter.row_height_twips",
                    "rtfreporter.cell_padding_left_twips",
                    "rtfreporter.cell_padding_right_twips") %in% names(opts)))
})

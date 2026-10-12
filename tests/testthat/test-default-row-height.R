# Font-size-aware default row height is applied uniformly across all
# table-shaped elements (RTF header, footer, table body).

silence_rtf_tables_deprecation()   # the old arguments' own tests (#594)

test_that(".default_row_height_twips() honors lookup, linear fallback, and clamp", {
  expect_identical(rtfreporter:::.default_row_height_twips(18L), 230L)  # 9pt
  expect_identical(rtfreporter:::.default_row_height_twips(24L), 290L)  # 12pt
  # Out-of-table → linear fallback
  expect_identical(rtfreporter:::.default_row_height_twips(40L),
                   as.integer(round(40 * 12.8)))
  # Below the min → clamped to 180L
  expect_identical(rtfreporter:::.default_row_height_twips(2L), 180L)
})

test_that("page header, page footer, table body all emit \\trrh230 at 9pt", {
  df <- data.frame(A = 1:2, B = c("x", "y"), stringsAsFactors = FALSE)
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1, secinfo = list(
    header = rtf_header(rows = list(c(l = "TITLE", r = "Page {AUTO_PAGE}"))),
    footer = rtf_footer(rows = list(c(c = "FOOTER")))
  ))
  doc <- rtf_tables(doc, list(df))

  # Add a footnote via the internal helper to also exercise that block.
  rep <- rtfreporter:::.pipe_doc_to_rtfreport(doc)
  rep$pages[[1L]]$footnote <- "Note: example."
  txt <- .render_to_string(rep)

  # 1 page header + 1 page footer + 1 col-header + ≥2 body rows.
  expect_gte(length(gregexpr("\\\\trrh230\\b", txt)[[1L]]), 4L)
  expect_false(grepl("\\\\trrh360\\b", txt))
})

test_that("per-element overrides win over the document-wide default", {
  df <- data.frame(A = 1:2, B = c("x", "y"), stringsAsFactors = FALSE)
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1, secinfo = list(
    header = rtf_header(rows = list(c(l = "T")), row_height_twips = 500L),
    footer = rtf_footer(rows = list(c(l = "F")), row_height_twips = 400L)
  ))
  doc <- rtf_tables(doc, list(df), row_height_twips = 320L)

  txt <- .render_to_string(doc)
  expect_match(txt, "\\\\trrh500\\b")
  expect_match(txt, "\\\\trrh400\\b")
  expect_match(txt, "\\\\trrh320\\b")
})

test_that("a larger document font shifts the default row height upward", {
  df <- data.frame(A = 1:2, B = c("x", "y"), stringsAsFactors = FALSE)
  doc <- rtf_document(default_format = list(font_size_half_points = 24L))
  doc <- rtf_section(doc, page = 1, secinfo = list(
    header = rtf_header(rows = list(c(l = "T")))
  ))
  doc <- rtf_tables(doc, list(df))

  txt <- .render_to_string(doc)
  expect_match(txt, "\\\\trrh290\\b")   # 12pt → 290
  expect_false(grepl("\\\\trrh230\\b", txt))
})

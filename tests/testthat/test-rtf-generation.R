# End-to-end RTF file generation through the pipe API.

silence_rtf_tables_deprecation()   # the old arguments' own tests (#594)

test_that("generate_rtfreport() writes a valid non-empty RTF file", {
  df_safety <- data.frame(
    Event    = c("Headache", "Nausea", "Dizziness"),
    Mild     = c(5, 3, 2),
    Moderate = c(2, 1, 1),
    Severe   = c(0, 0, 1)
  )
  df_efficacy <- data.frame(
    Response = c("Complete", "Partial", "No Response"),
    Count    = c(25, 15, 10),
    Percent  = c("50%", "30%", "20%")
  )

  doc <- rtf_document()
  doc <- rtf_tables(doc, list(df_safety, df_efficacy),
                    border = "tfl", row_height_twips = 280L)
  doc <- rtf_section(doc, page = 1, secinfo = list(
    header = rtf_header(rows = list(l = "Clinical Report", r = "Safety")),
    footer = rtf_footer(rows = list(c = "Page 1"))
  ))
  doc <- rtf_section(doc, page = 2, secinfo = list(
    header = rtf_header(rows = list(l = "Clinical Report", r = "Efficacy")),
    footer = rtf_footer(rows = list(c = "Page 2"))
  ))

  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)

  expect_invisible(generate_rtfreport(doc, f, overwrite = TRUE))
  expect_true(file.exists(f))
  expect_gt(file.info(f)$size, 0L)

  header_bytes <- readChar(f, nchars = 6L)
  expect_match(header_bytes, "^\\{\\\\rtf")
})

test_that("generate_rtfreport() refuses to overwrite without overwrite = TRUE", {
  df <- data.frame(A = 1L, B = "x", stringsAsFactors = FALSE)
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1,
                     secinfo = list(header = NULL, footer = NULL))
  doc <- rtf_tables(doc, list(df))

  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)

  generate_rtfreport(doc, f)
  # written over by default (#594): a report program is run again and again
  expect_no_error(generate_rtfreport(doc, f))
  expect_error(generate_rtfreport(doc, f, overwrite = FALSE), "already exists")
})

test_that("generate_rtfreport() rejects unsupported report types", {
  expect_error(generate_rtfreport(list(foo = 1), tempfile()),
               "rtf_document.*rtfreport")
})

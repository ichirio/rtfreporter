# The text colour of a header or footer band, and of one of its rows (#42).

hfc_rtf <- function(header = NULL, footer = NULL) {
  doc <- rtf_document() |>
    rtf_section(header = header, footer = footer) |>
    rtf_tables(list(data.frame(A = 1)))
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

test_that("FT-DC224 [FR-DC295 FR-DC296]: a band's colour, and a row's over it, are written and in the colour table", {
  txt <- hfc_rtf(
    header = rtf_header(list(c(l = "Protocol XYZ", r = "Page {PAGE}"),
                             list(columns = c(l = "DRAFT"), color = "#c00000")),
                        color = "#7F7F7F"),
    footer = rtf_footer(c(l = "Confidential"), color = "#7F7F7F"))
  # the colour table: black, white, then the colours used
  expect_match(txt, "\\red127\\green127\\blue127;", fixed = TRUE)
  expect_match(txt, "\\red192\\green0\\blue0;", fixed = TRUE)
  tbl <- regmatches(txt, regexpr("\\{\\\\colortbl[^}]*\\}", txt))
  entries <- strsplit(sub("^\\{\\\\colortbl;", "", tbl), ";", fixed = TRUE)[[1L]]
  grey <- which(entries == "\\red127\\green127\\blue127")
  red  <- which(entries == "\\red192\\green0\\blue0")
  # each cell of the band in the band's colour, the DRAFT row in its own
  expect_match(txt, sprintf("\\cf%d Protocol XYZ\\cf1 ", grey), fixed = TRUE)
  expect_match(txt, sprintf("\\cf%d Page 1\\cf1 ", grey), fixed = TRUE)
  expect_match(txt, sprintf("\\cf%d DRAFT\\cf1 ", red), fixed = TRUE)
  expect_match(txt, sprintf("\\cf%d Confidential\\cf1 ", grey), fixed = TRUE)
  # no colour: the text as before, no \cf
  plain <- hfc_rtf(header = rtf_header(c(l = "Protocol XYZ")))
  expect_false(grepl("\\cf[0-9]+ Protocol", plain))
})

test_that("FT-DC225 [FR-DC297]: a colour that is not one 6-digit hex string is refused", {
  msg <- "must be NULL or a 6-digit hex colour"
  for (bad in list("red", "#FFF", c("#000000", "#FFFFFF"), NA_character_, 1)) {
    expect_error(rtf_header(c(l = "x"), color = bad), msg, fixed = TRUE)
    expect_error(rtf_footer(c(l = "x"), color = bad), msg, fixed = TRUE)
  }
  expect_error(rtf_header(list(c(l = "x"), list(columns = c(l = "y"), color = "red"))),
               "`rtf_header()` row 2's `color` must be NULL", fixed = TRUE)
})

test_that("FT-DC5107 [FR-DC384]: a title row's colour that is not one 6-digit hex string is refused (#986)", {
  write_with <- function(titles) {
    doc <- rtf_document() |> rtf_tables(list(data.frame(A = 1)))
    doc <- rtf_titles(doc, titles)
    f <- tempfile(fileext = ".rtf")
    on.exit(unlink(f), add = TRUE)
    generate_rtfreport(doc, f, overwrite = TRUE)
    paste(readLines(f, warn = FALSE), collapse = "\n")
  }
  msg <- "`color` must be NULL or a 6-digit hex colour such as \"#7F7F7F\"."
  for (bad in list("red", "#FFF", c("#000000", "#FFFFFF"), NA_character_, 1)) {
    expect_error(write_with(list(list(list(text = "T", color = bad)))), msg, fixed = TRUE)
  }
  out <- write_with(list(list(list(text = "T", color = "#c00000"),
                              list(text = "S", color = "#7f7f7f"))))
  tbl <- regmatches(out, regexpr("\\{\\\\colortbl[^}]*\\}", out))
  expect_false(grepl("NA", tbl, fixed = TRUE))
  expect_match(tbl, "\\red192\\green0\\blue0;", fixed = TRUE)
  expect_match(tbl, "\\red127\\green127\\blue127;", fixed = TRUE)
})

# Cell background colour (`\clcbpat`) -- #400.

.bg_df <- function() data.frame(
  Parameter = c("ALT (U/L)", "AST (U/L)", "Bili (mg/dL)"),
  Placebo   = c("22.1", "19.8", "0.6"),
  Drug      = c("58.4", "61.2", "1.9"),
  stringsAsFactors = FALSE)

.bg_render <- function(tb) {
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1, secinfo = list(
    header = rtf_header(rows = list(c(l = "BG")))))
  doc <- rtf_tables(doc, list(tb))
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

.n_shade <- function(txt) {
  m <- gregexpr("clcbpat", txt, fixed = TRUE)[[1L]]
  if (m[1L] == -1L) 0L else length(m)
}

test_that("no background by default -- output is unshaded", {
  expect_identical(.n_shade(.bg_render(as_rtftables(.bg_df())[[1L]])), 0L)
})

test_that("style_cols(background =) fills that column's body cells", {
  tb  <- style_cols(as_rtftables(.bg_df())[[1L]], cols = 1, background = "#EFEFEF")
  # Three body rows, one shaded cell each; the header is untouched.
  expect_identical(.n_shade(.bg_render(tb)), 3L)
})

test_that("header_background fills the header cells only", {
  tb  <- style_cols(as_rtftables(.bg_df())[[1L]], cols = 1,
                    header_background = "#D6E4F0")
  expect_identical(.n_shade(.bg_render(tb)), 1L)
})

test_that("style_body(background =) overrides the column's fill", {
  tb <- as_rtftables(.bg_df())[[1L]]
  tb <- style_cols(tb, cols = 3, background = "#EFEFEF")
  tb <- style_body(tb, rows = 2, cols = 3, background = "#F8D7DA")
  txt <- .bg_render(tb)
  # Still one shaded cell per body row, but row 2 uses the other colour.
  expect_identical(.n_shade(txt), 3L)
  # Both colours reached the colour table, so both indices exist.
  idx <- unique(regmatches(txt, gregexpr("clcbpat[0-9]+", txt))[[1L]])
  expect_length(idx, 2L)
})

test_that("the fill goes in the cell definition, not the cell content", {
  tb  <- style_cols(as_rtftables(.bg_df())[[1L]], cols = 1, background = "#EFEFEF")
  txt <- .bg_render(tb)
  # `\clcbpat` must sit before the `\cellx` that closes the definition, never
  # after the `\cell` that ends the content.
  expect_match(txt, "clcbpat[0-9]+\\\\clvertal[a-z]+\\\\cellx")
})

test_that("a background colour is added to the document colour table", {
  tb  <- style_cols(as_rtftables(.bg_df())[[1L]], cols = 1, background = "#123456")
  txt <- .bg_render(tb)
  # RTF colour table entries are \redNN\greenNN\blueNN; 0x12/0x34/0x56.
  expect_match(txt, "red18\\\\green52\\\\blue86")
})

test_that("an unknown or empty background is simply not drawn", {
  expect_identical(rtfreporter:::.cell_shading_cmd(NULL, list()), "")
  expect_identical(rtfreporter:::.cell_shading_cmd(NA_character_, list()), "")
  expect_identical(rtfreporter:::.cell_shading_cmd("", list()), "")
  expect_identical(rtfreporter:::.cell_shading_cmd("#FFFFFF", NULL), "")
  expect_identical(rtfreporter:::.cell_shading_cmd("#FFFFFF", list("#000000" = 1L)), "")
  expect_identical(rtfreporter:::.cell_shading_cmd("#FFFFFF", list("#FFFFFF" = 7L)),
                   paste0(intToUtf8(92), "clcbpat7"))
})

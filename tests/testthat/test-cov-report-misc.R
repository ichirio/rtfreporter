# Coverage of the report-level helpers the other tests leave out: a page
# orientation option that is not valid, font tables given as slot vectors or
# with a name no East Asian code page writes, and print() of a block whose
# content is a multi data frame table, opens an auto section, or is neither
# a table nor a figure.

test_that("generate_rtfreport() refuses an orientation option that is not landscape / portrait", {
  skip_on_cran()
  old <- options(rtfreporter.page.orientation = "sideways")
  on.exit(options(old), add = TRUE)
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  doc <- rtf_document(page = list(paper_size = "a4")) |>
    rtf_tables(as_rtftables(data.frame(A = 1)))
  expect_error(generate_rtfreport(doc, f),
               "`orientation` must be \"landscape\" or \"portrait\".", fixed = TRUE)
})

test_that("rtf_document() checks a font_table entry given as a bare slot vector", {
  skip_on_cran()
  doc <- rtf_document(font_table = list(c(east_asian = "MS Mincho", latin = "Arial")))
  # checked as any font = is: put in slot order, latin first
  expect_identical(doc$document$font_table[[1L]],
                   c(latin = "Arial", east_asian = "MS Mincho"))
  expect_error(rtf_document(font_table = list(c(east_asian = "MS Mincho"))),
               "must name a `latin` font")
})

test_that("print() of a report labels a slotted first font by its slots", {
  skip_on_cran()
  doc <- rtf_document(font_table = list(list(
    name = c(latin = "Arial", east_asian = "MS Mincho")))) |>
    rtf_tables(as_rtftables(data.frame(A = 1)))
  # an rtfreport is only made inside generate_rtfreport(); print() of one is
  # reached through the internal converter, as test-print-methods.R does
  rep <- rtfreporter:::.pipe_doc_to_rtfreport(doc)
  out <- paste(capture.output(print(rep)), collapse = "\n")
  expect_match(out, "Fonts:   1 (latin = Arial, east_asian = MS Mincho)", fixed = TRUE)
})

test_that("a font name with a Han character outside every CJK code page keeps charset 0", {
  skip_on_cran()
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  # U+20000 (CJK Extension B) is in none of CP932 / CP936 / CP950 / CP949
  doc <- rtf_document(font_table = list(list(name = "\U00020000Sans"))) |>
    rtf_tables(as_rtftables(data.frame(A = 1)))
  generate_rtfreport(doc, f)
  out <- paste(readLines(f, warn = FALSE), collapse = "\n")
  # written as \uN? (a UTF-16 surrogate pair), not as code page bytes
  expect_match(out, "{\\fonttbl{\\f0\\fnil\\fcharset0 \\u-10176?\\u-9216?Sans;}}",
               fixed = TRUE)
})

test_that("print() of a block counts the rows of every data frame of a multi-DF table", {
  skip_on_cran()
  tbl <- rtftable(list(data.frame(A = c("a", "b")), data.frame(A = "c")))
  doc <- rtf_document() |> rtf_tables(tbl)
  out <- capture.output(print(doc$contents[[1L]]))
  expect_identical(out[1L], "<rtf_block> rtftable 3 rows (group 1, page 1)")
})

test_that("print() of a block says which auto section it opens", {
  skip_on_cran()
  doc <- rtf_document() |>
    rtf_tables(list(Demographics = as_rtftables(data.frame(A = 1))[[1L]]),
               auto_section = TRUE)
  out <- capture.output(print(doc$contents[[1L]]))
  expect_identical(out[1L], "<rtf_block> rtftable 1 row (group 1, page 1)")
  expect_identical(out[3L], "  opens the auto section \"Demographics\"")
})

test_that("print() of a block whose content is neither a table nor a figure names its class", {
  skip_on_cran()
  doc <- rtf_document() |> rtf_tables(as_rtftables(data.frame(A = 1)))
  b <- doc$contents[[1L]]
  # rtf_tables() / rtf_figures() only make table and figure blocks; a block
  # edited by hand falls back to the content's class
  b$content <- structure(list(), class = c("my_content", "list"))
  out <- capture.output(res <- print(b))
  expect_identical(out[1L], "<rtf_block> my_content (group 1, page 1)")
  expect_identical(res, b)
})

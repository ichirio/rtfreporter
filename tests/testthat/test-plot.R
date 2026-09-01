# S3 plot() methods — smoke-test that each renders without error and
# returns the input invisibly.  Plot output is rendered to a temporary
# PDF device so tests run headless.

.with_pdf <- function(expr) {
  f <- tempfile(fileext = ".pdf")
  on.exit({ if (dev.cur() > 1L) dev.off(); unlink(f) }, add = TRUE)
  grDevices::pdf(f)
  force(expr)
}

test_that("plot.rtf_border_side() renders without error", {
  side <- rtf_border_side("thick", 20L, "#003366")
  .with_pdf({
    out <- plot(side)
    expect_identical(out, side)
  })
})

test_that("plot.rtf_border() renders without error", {
  b <- rtf_border(top = rtf_border_side(),
                  bottom = rtf_border_side("double", 20L))
  .with_pdf({
    out <- plot(b)
    expect_identical(out, b)
  })
})

test_that("plot.rtf_table_border() renders without error", {
  tb <- rtfreporter:::.rtf_border_tfl()
  .with_pdf({
    out <- plot(tb)
    expect_identical(out, tb)
  })
})

test_that("plot.rtftable() handles single-DF, multi-DF and empty data", {
  df <- data.frame(A = 1:3, B = c("x","y","z"), C = c(1.1, 2.2, 3.3),
                   stringsAsFactors = FALSE)
  tbl <- rtftable(df, col_rel_width = c(2, 1, 1))
  .with_pdf(expect_identical(plot(tbl), tbl))

  tbl_multi <- rtftable(list(df, df))
  .with_pdf(expect_identical(plot(tbl_multi), tbl_multi))

  tbl_empty <- rtftable(data.frame())
  .with_pdf(expect_identical(plot(tbl_empty), tbl_empty))
})

test_that("plot.rtf_document() renders single-page and multi-page docs", {
  df  <- data.frame(A = 1:2, B = c("x","y"), stringsAsFactors = FALSE)
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1, secinfo = list(header = NULL, footer = NULL))
  doc <- rtf_tables(doc, list(df, df, df, df, df))
  .with_pdf(expect_identical(plot(doc), doc))
  # max_pages truncation path
  .with_pdf(expect_identical(plot(doc, max_pages = 2L), doc))
})

test_that("plot.rtf_document() handles a doc with no content", {
  doc <- rtf_document()
  .with_pdf(expect_identical(plot(doc), doc))
})

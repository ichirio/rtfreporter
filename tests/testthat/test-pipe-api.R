# Pipe Composition API — document creation, configuration, content addition,
# section definition, deprecated formatters, print method, full workflow.

test_that("rtf_document() returns the expected S3 structure with defaults", {
  doc <- rtf_document()
  expect_s3_class(doc, "rtf_document")
  expect_setequal(names(doc),
                  c("document", "contents", "titles", "footnotes", "sections"))
  expect_identical(doc$document$page$orientation, "landscape")
  expect_identical(doc$document$page$width_in, 11)
  expect_identical(doc$document$page$height_in, 8.5)
})

test_that("rtf_config() returns an updated copy without mutating the original", {
  doc  <- rtf_document()
  doc2 <- rtf_config(doc, page = list(orientation = "portrait"))
  expect_identical(doc2$document$page$orientation, "portrait")
  expect_identical(doc$document$page$orientation, "landscape")
})

test_that("rtf_tables() promotes bare data.frames and applies shared formatting", {
  df1 <- data.frame(A = 1:3, B = c("x", "y", "z"))
  df2 <- data.frame(ID = c(10, 20, 30), Value = c(100, 200, 300))

  doc <- rtf_tables(rtf_document(), list(df1, df2),
                    col_rel_width = c(1, 2), row_height_twips = 280L)
  expect_length(doc$contents, 2L)
  expect_s3_class(doc$contents[[1L]], "rtftable")
  expect_identical(doc$contents[[1L]]$col_rel_width, c(1, 2))
  expect_identical(doc$contents[[1L]]$row_height_twips, 280L)
  expect_identical(doc$contents[[2L]]$col_rel_width, c(1, 2))
})

test_that("explicit rtf_tables() args override pre-built rtftable settings", {
  df1 <- data.frame(A = 1:3, B = c("x", "y", "z"))
  df2 <- data.frame(ID = c(10, 20, 30), Value = c(100, 200, 300))
  custom_tbl <- rtftable(df2, col_rel_width = c(3, 1))

  # col_rel_width is passed explicitly -> overrides BOTH the bare data.frame
  # build and the pre-built rtftable's own c(3, 1).
  doc <- rtf_tables(rtf_document(), list(df1, custom_tbl),
                    col_rel_width = c(1, 2))
  expect_identical(doc$contents[[1L]]$col_rel_width, c(1, 2))
  expect_identical(doc$contents[[2L]]$col_rel_width, c(1, 2))
})

test_that("pre-built rtftable() keeps settings NOT explicitly overridden", {
  df2 <- data.frame(ID = c(10, 20, 30), Value = c(100, 200, 300))
  custom_tbl <- rtftable(df2, col_rel_width = c(3, 1),
                         table_align = "center")

  # Only table_align is passed -> col_rel_width stays the table's own c(3, 1).
  doc <- rtf_tables(rtf_document(), list(custom_tbl), table_align = "right")
  expect_identical(doc$contents[[1L]]$col_rel_width, c(3, 1))
  expect_identical(doc$contents[[1L]]$table_align, "right")
})

test_that("multi-content-per-page is rejected", {
  df1 <- data.frame(A = 1:3, B = c("x", "y", "z"))
  df2 <- data.frame(ID = c(10, 20, 30), Value = c(100, 200, 300))
  expect_error(
    rtf_tables(rtf_document(), list(df1, list(df2))),
    "list element"
  )
})

test_that("rtf_section() supports single and multiple page assignments", {
  df1 <- data.frame(A = 1:3, B = c("x", "y", "z"))
  df2 <- data.frame(ID = c(10, 20, 30), Value = c(100, 200, 300))

  d1 <- rtf_section(rtf_tables(rtf_document(), list(df1, df2)),
                    page = 1, secinfo = list(header = NULL, footer = NULL))
  expect_length(d1$sections, 1L)
  expect_false(is.null(d1$sections[["1"]]))

  d2 <- rtf_section(rtf_tables(rtf_document(), list(df1, df2)),
                    page = c(1, 2),
                    secinfo = list(
                      list(header = NULL, footer = NULL),
                      list(header = NULL, footer = NULL)
                    ))
  expect_length(d2$sections, 2L)
})

test_that("deprecated rtf_*_format() functions warn and return the doc unchanged", {
  df <- data.frame(A = 1:3)
  base_doc <- rtf_tables(rtf_document(), list(df))
  for (fn_name in c("rtf_table_format", "rtf_header_format",
                    "rtf_footer_format", "rtf_figure_format")) {
    fn <- get(fn_name, envir = asNamespace("rtfreporter"))
    expect_warning(out <- fn(base_doc, pages = "all", border = "tfl"),
                   regexp = "deprecated", ignore.case = TRUE)
    expect_identical(out, base_doc)
  }
})

test_that("print(rtf_document) outlines the document structure", {
  df1 <- data.frame(A = 1:3, B = c("x", "y", "z"))
  doc <- rtf_section(rtf_tables(rtf_document(), list(df1, df1, df1)),
                     page = 1, secinfo = list(header = NULL, footer = NULL))
  expect_output(print(doc), "<rtf_document>")
  expect_output(print(doc), "Content blocks \\(3\\)")
})

test_that("end-to-end pipe-style workflow builds a valid document", {
  df1 <- data.frame(A = 1:3, B = c("x", "y", "z"))
  df2 <- data.frame(ID = c(10, 20, 30), Value = c(100, 200, 300))

  doc <- rtf_document()
  doc <- rtf_config(doc, page = list(
    orientation = "landscape", width_in = 11, height_in = 8.5
  ))
  doc <- rtf_tables(doc, list(df1, df2),
                    border = "tfl", row_height_twips = 280L,
                    col_rel_width = c(1, 2))
  doc <- rtf_section(doc, page = 1,
                     secinfo = list(header = list(l = "Section 1", r = "Page 1"),
                                    footer = list(c = "Footer 1")))
  doc <- rtf_section(doc, page = 2,
                     secinfo = list(header = list(l = "Section 2", r = "Page 2"),
                                    footer = list(c = "Footer 2")))

  expect_length(doc$contents, 2L)
  expect_length(doc$sections, 2L)
  expect_s3_class(doc$contents[[1L]], "rtftable")
  expect_identical(doc$contents[[1L]]$col_rel_width, c(1, 2))
})

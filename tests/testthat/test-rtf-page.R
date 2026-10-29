# rtf_page() / rtf_default_format(): structured settings with visible defaults
# whose unset arguments still fall back to the rtfreporter.* options (#111, #152).

test_that("rtf_page() defaults equal the factory baseline and carry the class", {
  p <- rtf_page()
  expect_s3_class(p, "rtf_page")
  expect_identical(p$paper_size,  rtfreporter:::.opt("rtfreporter.page.paper_size"))
  expect_identical(p$orientation, rtfreporter:::.opt("rtfreporter.page.orientation"))
  expect_identical(p$margin_top_in, rtfreporter:::.opt("rtfreporter.page.margin_top_in"))
  expect_null(p$width_in)
  expect_null(p$header_dist_in)
})

test_that("rtf_page() explicit args win and validate", {
  p <- rtf_page(paper_size = "A4", orientation = "portrait",
                width_in = 8.5, height_in = 14)
  expect_identical(p$paper_size, "A4")
  expect_identical(p$orientation, "portrait")
  expect_identical(p$width_in, 8.5)
  expect_error(rtf_page(orientation = "sideways"), "landscape")
  expect_error(rtf_page(margin_top_in = -1), "positive")
  expect_error(rtf_page(width_in = "wide"), "positive")
})

test_that("an unset rtf_page() arg falls back to the rtfreporter.* option (#111)", {
  old <- options(rtfreporter.page.orientation = "portrait",
                 rtfreporter.page.paper_size = "A4")
  on.exit(options(old), add = TRUE)
  expect_identical(rtf_page()$orientation, "portrait")          # unset -> option
  expect_identical(rtf_page()$paper_size,  "A4")
  expect_identical(rtf_page(orientation = "landscape")$orientation, "landscape") # explicit wins
})

test_that("rtf_default_format() defaults equal the factory baseline", {
  d <- rtf_default_format()
  expect_s3_class(d, "rtf_default_format")
  expect_identical(d$font_size_half_points,
                   as.integer(rtfreporter:::.opt("rtfreporter.font_size_half_points")))
  expect_identical(d$markup,          rtfreporter:::.opt("rtfreporter.markup"))
  expect_identical(d$title_format,    rtfreporter:::.opt("rtfreporter.title_format"))
  expect_identical(d$footnote_format, rtfreporter:::.opt("rtfreporter.footnote_format"))
  expect_null(d$row_height_twips)
})

test_that("rtf_default_format() validates and resolves tokens", {
  expect_setequal(rtf_default_format(markup = "all")$markup, c("script", "relational"))
  expect_error(rtf_default_format(markup = "bogus"), "subset")
  expect_error(rtf_default_format(title_format = "fancy"), "text.*table")
  expect_error(rtf_default_format(font_size_half_points = 0), "positive")
})

test_that("rtf_document() accepts the constructors and renders identically to a list", {
  df  <- data.frame(A = "x", stringsAsFactors = FALSE)
  render <- function(pg) {
    doc <- rtf_document(page = pg) |>
      rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
      rtf_tables(as_rtftables(df))
    f <- tempfile(fileext = ".rtf"); on.exit(unlink(f), add = TRUE)
    generate_rtfreport(doc, f, overwrite = TRUE)
    paste(readLines(f, warn = FALSE), collapse = "")
  }
  obj  <- render(rtf_page(paper_size = "A4", orientation = "portrait"))
  lst  <- render(list(paper_size = "A4", orientation = "portrait"))
  expect_match(obj, "paperw11905")          # A4 portrait width
  expect_false(grepl("lndscpsxn", obj))     # portrait
  expect_identical(obj, lst)                # constructor == list
})

test_that("rtf_default_format() flows through to the rendered document", {
  df  <- data.frame(A = "x", stringsAsFactors = FALSE)
  doc <- rtf_document(default_format = rtf_default_format(font_size_half_points = 22L)) |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_tables(as_rtftables(df))
  f <- tempfile(fileext = ".rtf"); on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  expect_match(paste(readLines(f, warn = FALSE), collapse = ""), "\\\\fs22")
})

test_that("dimensions alone leave paper_size / orientation unset, without warnings (#761)", {
  p <- rtf_page(width_in = 8.5, height_in = 14)
  expect_null(p$paper_size)
  expect_null(p$orientation)
  doc <- rtf_document(page = p) |>
    rtf_tables(as_rtftables(data.frame(a = "x", b = 1)))
  f <- tempfile(fileext = ".rtf"); on.exit(unlink(f), add = TRUE)
  expect_no_warning(generate_rtfreport(doc, f, overwrite = TRUE))
  # a value the caller did give beside the dimensions is still warned about
  expect_warning(
    generate_rtfreport(rtf_document(page = rtf_page(orientation = "landscape",
                                                    width_in = 8.5, height_in = 14)) |>
                         rtf_tables(as_rtftables(data.frame(a = "x"))), f, overwrite = TRUE),
    "contradicts")
})

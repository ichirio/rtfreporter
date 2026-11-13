# plan_titles(border = ) / plan_footnotes(border = ): a table's own title or
# footnote block border, kept with its content, over the document's.

.pb_tbl <- function() data.frame(
  Parameter = c("Subjects", "Age, mean (SD)"),
  Placebo   = c("86", "75.2 (8.59)"),
  Active    = c("84", "75.7 (8.29)"),
  stringsAsFactors = FALSE)

.pb_render <- function(doc) {
  d <- tempfile("pb-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- file.path(d, "out.rtf")
  generate_rtfreport(doc, f, overwrite = TRUE)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

.pb_lines <- function(txt, pat) {
  grep(pat, strsplit(txt, "\n", fixed = TRUE)[[1L]], value = TRUE)
}

.pb_doc <- function(pages, format = "table", ...) {
  rtf_document() |>
    rtf_section(page = 1, header = NULL, footer = NULL) |>
    rtf_tables(pages) |>
    rtf_config(style = rtf_style(title_format = format, ...))
}

.pb_pages <- function(tb = NULL, fb = NULL) {
  table_plan(.pb_tbl()) |>
    plan_titles("Table 14.1.1", "Demographics", border = tb) |>
    plan_footnotes("Note: Example Pharma data.", "Source: ADSL.", border = fb) |>
    plan_apply()
}

test_that("plan_titles(border = ) is the title block's border, the whole block", {
  pages <- .pb_pages(tb = rtf_border(top = TRUE, bottom = TRUE))
  expect_s3_class(attr(pages[[1L]], "rtf_titles_border"), "rtf_border")
  r <- .pb_lines(.pb_render(.pb_doc(pages)), "Table 14\\.1\\.1|Demographics")
  expect_length(r, 2L)
  expect_true(grepl("clbrdrt", r[1L], fixed = TRUE))
  expect_false(grepl("clbrdrb", r[1L], fixed = TRUE))
  expect_true(grepl("clbrdrb", r[2L], fixed = TRUE))
  expect_false(grepl("clbrdrt", r[2L], fixed = TRUE))
})

test_that("plan_footnotes(border = ) is the footnote block's border", {
  r <- .pb_lines(.pb_render(.pb_doc(.pb_pages(fb = rtf_border(top = TRUE, bottom = TRUE)))),
                 "Note: Example|Source: ADSL")
  expect_length(r, 2L)
  expect_true(grepl("clbrdrt", r[1L], fixed = TRUE))
  expect_false(grepl("clbrdrb", r[1L], fixed = TRUE))
  expect_true(grepl("clbrdrb", r[2L], fixed = TRUE))
})

test_that("a plan's border wins over rtf_titles(border = ) and rtf_style()", {
  pages <- .pb_pages(tb = rtf_border(bottom = "double"),
                     fb = rtf_border(top = "double"))
  doc <- .pb_doc(pages,
                 titles = list(border = rtf_border(top = TRUE)),
                 footnotes = list(border = rtf_border(bottom = TRUE)))
  txt <- .pb_render(doc)
  ti <- .pb_lines(txt, "Table 14\\.1\\.1|Demographics")
  expect_false(any(grepl("clbrdrt", ti, fixed = TRUE)))
  expect_true(grepl("clbrdrb\\brdrdb", ti[2L], fixed = TRUE))
  fn <- .pb_lines(txt, "Note: Example|Source: ADSL")
  expect_true(grepl("clbrdrt\\brdrdb", fn[1L], fixed = TRUE))
  expect_false(any(grepl("clbrdrb", fn, fixed = TRUE)))
  # rtf_titles(border = ) on the document: the content's own still wins
  doc2 <- .pb_doc(pages) |>
    rtf_titles(list(c("Table 14.1.1", "Demographics")), border = rtf_border(top = TRUE))
  ti2 <- .pb_lines(.pb_render(doc2), "Table 14\\.1\\.1|Demographics")
  expect_false(any(grepl("clbrdrt", ti2, fixed = TRUE)))
  expect_true(grepl("clbrdrb\\brdrdb", ti2[2L], fixed = TRUE))
})

test_that("each content keeps its own border; one without takes the document's", {
  with <- .pb_pages(tb = rtf_border(bottom = "double"))
  without <- table_plan(.pb_tbl()) |> plan_titles("Table 14.1.2") |> plan_apply()
  doc <- rtf_document() |>
    rtf_section(page = 1, header = NULL, footer = NULL) |>
    rtf_tables(with) |>
    rtf_tables(without) |>
    rtf_config(style = rtf_style(title_format = "table",
                                 titles = list(border = rtf_border(bottom = TRUE))))
  txt <- .pb_render(doc)
  expect_true(grepl("clbrdrb\\brdrdb", .pb_lines(txt, "Demographics"), fixed = TRUE))
  t2 <- .pb_lines(txt, "Table 14\\.1\\.2")
  expect_true(grepl("clbrdrb\\brdrs", t2, fixed = TRUE))
})

test_that("a border's colour reaches the colour table", {
  pages <- .pb_pages(tb = rtf_border(bottom = rtf_border_line(color = "#C00000")))
  txt <- .pb_render(.pb_doc(pages))
  expect_match(txt, "\\red192\\green0\\blue0", fixed = TRUE)
  expect_match(.pb_lines(txt, "Demographics"), "brdrcf", fixed = TRUE)
})

test_that("no border given: the same RTF as before", {
  plain <- table_plan(.pb_tbl()) |>
    plan_titles("Table 14.1.1", "Demographics") |>
    plan_footnotes("Note: Example Pharma data.", "Source: ADSL.") |>
    plan_apply()
  expect_null(attr(plain[[1L]], "rtf_titles_border"))
  expect_identical(.pb_render(.pb_doc(.pb_pages())), .pb_render(.pb_doc(plain)))
  # the block holds no border field when none was given
  doc <- .pb_doc(plain)
  expect_false("title_border" %in% names(unclass(doc$contents)[[1L]]))
})

test_that("in the text format a plan's border warns once and draws nothing", {
  msg <- "drawn only when title_format / footnote_format = \"table\""
  pages <- .pb_pages(tb = rtf_border(bottom = TRUE))
  w <- character(0)
  txt_b <- withCallingHandlers(.pb_render(.pb_doc(pages, "text")),
                               warning = function(c) {
                                 w <<- c(w, conditionMessage(c))
                                 invokeRestart("muffleWarning")
                               })
  expect_length(w, 1L)
  expect_match(w, msg, fixed = TRUE)
  expect_identical(txt_b, .pb_render(.pb_doc(.pb_pages(), "text")))
})

test_that("plan_titles(border = ) / plan_footnotes(border = ) are checked", {
  expect_error(table_plan(.pb_tbl()) |> plan_titles("T", border = "bottom"),
               "plan_titles(border = ) must be NULL or an rtf_border() object.",
               fixed = TRUE)
  expect_error(table_plan(.pb_tbl()) |> plan_footnotes("F", border = TRUE),
               "plan_footnotes(border = ) must be NULL or an rtf_border() object.",
               fixed = TRUE)
  # a later call without border keeps the earlier call's
  p <- table_plan(.pb_tbl()) |>
    plan_titles("T", border = rtf_border(top = TRUE)) |>
    plan_titles("T2") |>
    plan_apply()
  expect_s3_class(attr(p[[1L]], "rtf_titles_border"), "rtf_border")
})

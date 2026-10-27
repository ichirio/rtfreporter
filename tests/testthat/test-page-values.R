# A page's values by column: {<column>} tokens in titles, footnotes,
# section labels and header / footer bands (R/page_values.R).

pv_data <- function() {
  data.frame(PARAM = rep(c("ALT", "AST"), each = 4),
             SEX = rep(c("F", "F", "M", "M"), 2),
             Item = sprintf("Row %d", 1:8), N = 1:8)
}
pv_render <- function(doc) {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}
pv_values <- function(pages) {
  unname(lapply(pages, function(p) attr(p$data, "rtf_paginate_meta")$page_values))
}
# The text of each page: what is between one section / page break and the next
pv_pages <- function(out) strsplit(out, "\\\\sect\\b|\\\\page\\b", perl = TRUE)[[1L]]

test_that("page_by records each page's column and value", {
  p <- as_rtftables(pv_data(), page_by = "PARAM")
  expect_identical(pv_values(p), list(c(PARAM = "ALT"), c(PARAM = "AST")))
  # the joined value that names the page stays as it was
  expect_identical(attr(p[[2]]$data, "rtf_paginate_meta")$page_by, "AST")
  p2 <- as_rtftables(pv_data(), page_by = c("PARAM", "SEX"))
  expect_identical(pv_values(p2)[[3]], c(PARAM = "AST", SEX = "F"))
  # a value split on a named group column says it too
  p3 <- as_rtftables(pv_data(), split = "by_value", group_col = "PARAM")
  expect_identical(pv_values(p3), list(c(PARAM = "ALT"), c(PARAM = "AST")))
  # an unnamed cut has none
  expect_null(pv_values(as_rtftables(pv_data(), max_rows = 4))[[1]])
})

test_that("one page_by column fills titles, footnotes and the header band", {
  p <- as_rtftables(pv_data(), page_by = "PARAM", drop_cols = "PARAM")
  doc <- rtf_document() |>
    rtf_section(header = rtf_header(c(l = "Parameter: {PARAM}", r = "Page {PAGE}"))) |>
    rtf_tables(p, titles = "Summary of {PARAM}", footnotes = "Shown: {PARAM}")
  out <- pv_render(doc)
  pg <- pv_pages(out)
  expect_match(out, "Parameter: ALT", fixed = TRUE)
  expect_match(out, "Parameter: AST", fixed = TRUE)
  expect_match(out, "Summary of ALT", fixed = TRUE)
  expect_match(out, "Summary of AST", fixed = TRUE)
  expect_match(out, "Shown: AST", fixed = TRUE)
  expect_false(grepl("{PARAM}", out, fixed = TRUE))
  # each page's band carries its own value
  expect_identical(sum(grepl("Parameter: ALT", pg, fixed = TRUE)), 1L)
  expect_true(grepl("Summary of AST", pg[grep("Parameter: AST", pg, fixed = TRUE)], fixed = TRUE))
})

test_that("two page_by columns: each token takes its own column", {
  p <- as_rtftables(pv_data(), page_by = c("PARAM", "SEX"))
  doc <- rtf_document() |>
    rtf_section(footer = rtf_footer(c(l = "{PARAM} / {SEX}"))) |>
    rtf_tables(p, titles = "Table: {SEX} ({PARAM})")
  out <- pv_render(doc)
  for (v in c("ALT / F", "ALT / M", "AST / F", "AST / M")) expect_match(out, v, fixed = TRUE)
  expect_match(out, "Table: M (AST)", fixed = TRUE)
})

test_that("a band with a column token is written for each page", {
  p <- as_rtftables(pv_data(), page_by = "PARAM")
  plain <- pv_render(rtf_document() |> rtf_section(header = rtf_header("Study X")) |>
                       rtf_tables(p))
  tok <- pv_render(rtf_document() |> rtf_section(header = rtf_header("Study {PARAM}")) |>
                     rtf_tables(p))
  n_hdr <- function(x) lengths(regmatches(x, gregexpr("{\\header", x, fixed = TRUE)))
  expect_identical(n_hdr(plain), 1L)
  expect_identical(n_hdr(tok), 2L)
})

test_that("a section label with a token takes the value of its page", {
  p <- as_rtftables(pv_data(), page_by = "PARAM")
  names(p) <- rep("Parameter {PARAM}", length(p))
  doc <- rtf_document() |> rtf_section(header = rtf_header("Study X")) |>
    rtf_tables(p, auto_section = TRUE)
  out <- pv_render(doc)
  expect_match(out, "Parameter ALT", fixed = TRUE)
  expect_match(out, "Parameter AST", fixed = TRUE)
})

test_that("a token for a column the page lacks is left as written", {
  p <- as_rtftables(pv_data(), page_by = "PARAM")
  doc <- rtf_document() |>
    rtf_section(header = rtf_header("Visit {VISIT} n={n}")) |>
    rtf_tables(p, titles = "{SEX}: {N} {PARAM}")
  out <- pv_render(doc)
  expect_match(out, "Visit \\{VISIT\\} n=\\{n\\}", fixed = TRUE)
  expect_match(out, "\\{SEX\\}: \\{N\\} ALT", fixed = TRUE)
  # a band naming no column of the pages is still written once
  expect_identical(lengths(regmatches(out, gregexpr("{\\header", out, fixed = TRUE))), 1L)
  # a table with no page values leaves every token alone
  out2 <- pv_render(rtf_document() |> rtf_tables(list(pv_data()), titles = "{PARAM}"))
  expect_match(out2, "\\{PARAM\\}", fixed = TRUE)
})

test_that("rtfreporter's own tokens and one's own win over a column's name", {
  d <- pv_data()
  d$PAGE <- "col"
  d$STUDY <- "col"
  p <- as_rtftables(d, page_by = c("PARAM", "PAGE", "STUDY"))
  out <- pv_render(rtf_document(tokens = list(STUDY = "ABC")) |>
                     rtf_tables(p, titles = "{PAGE} {STUDY} {PARAM}"))
  expect_match(out, "1 ABC ALT", fixed = TRUE)
})

test_that("on a page of several tables each title takes its own; the band the first", {
  pa <- as_rtftables(pv_data()[1:4, ], page_by = "PARAM")
  pb <- as_rtftables(pv_data()[5:8, ], page_by = "PARAM")
  nov <- as_rtftables(pv_data()[1:2, c("Item", "N")])
  doc <- rtf_document() |>
    rtf_section(header = rtf_header("Header {PARAM}")) |>
    rtf_tables(nov, titles = "None {PARAM}") |>
    rtf_tables(pa, titles = "First {PARAM}", page_break = FALSE) |>
    rtf_tables(pb, titles = "Second {PARAM}", page_break = FALSE)
  lay <- rtf_layout(doc)
  expect_length(lay$pages, 1L)
  expect_identical(lay$pages[[1]]$values, c(PARAM = "ALT"))
  expect_null(lay$pages[[1]]$segments[[1]]$values)
  expect_identical(lay$pages[[1]]$segments[[3]]$values, c(PARAM = "AST"))
  expect_warning(out <- pv_render(doc), "Page 1: \\{PARAM\\} is 'ALT'")
  expect_match(out, "Header ALT", fixed = TRUE)
  expect_match(out, "None \\{PARAM\\}", fixed = TRUE)
  expect_match(out, "First ALT", fixed = TRUE)
  expect_match(out, "Second AST", fixed = TRUE)
  # no warning when the band does not use the column
  doc2 <- rtf_document() |> rtf_tables(pa, titles = "{PARAM}") |>
    rtf_tables(pb, titles = "{PARAM}", page_break = FALSE)
  expect_no_warning(pv_render(doc2))
})

test_that("rtables and tfrmt pages carry the values they were cut on", {
  skip_if_not_installed("rtables")
  adsl <- data.frame(ARM = rep(c("A", "B"), 4), SEX = rep(c("F", "M"), each = 4),
                     AGE = c(30, 41, 52, 33, 44, 25, 36, 47))
  lyt <- rtables::basic_table() |> rtables::split_cols_by("ARM") |>
    rtables::split_rows_by("SEX", page_by = TRUE,
                           split_fun = rtables::keep_split_levels(c("F", "M"))) |>
    rtables::analyze("AGE")
  tt <- rtables::build_table(lyt, adsl)
  p <- as_rtftables(rtables::paginate_table(tt, lpp = NULL, cpp = NULL))
  expect_identical(pv_values(p), list(c(SEX = "F"), c(SEX = "M")))
  # a table not paginated so has none
  expect_null(pv_values(as_rtftables(tt))[[1]])

  skip_if_not_installed("tfrmt")
  skip_if_not_installed("gt")
  d <- data.frame(PARAM = rep(c("ALT", "AST"), each = 4),
                  label = rep(c("n", "Mean"), 4),
                  column = rep(rep(c("A", "B"), each = 2), 2),
                  param = rep(c("n", "mean"), 4), value = 1:8)
  tf <- tfrmt::tfrmt(group = "PARAM", label = "label", column = "column",
                     param = "param", value = "value",
                     body_plan = tfrmt::body_plan(tfrmt::frmt_structure(
                       group_val = ".default", label_val = ".default", tfrmt::frmt("xx"))),
                     page_plan = tfrmt::page_plan(
                       tfrmt::page_structure(group_val = list(PARAM = ".default")),
                       note_loc = "preheader"))
  g <- as_rtftables(tfrmt::print_to_gt(tf, d))
  expect_identical(pv_values(g), list(c(PARAM = "ALT"), c(PARAM = "AST")))
})

test_that("a plan's plan_paginate_group() pages carry the group column", {
  ard <- data.frame(PARAM = rep(c("ALT", "AST"), each = 2),
                    stat = c("n", "mean", "n", "mean"), A = 1:4)
  p <- as_rtftables(plan_paginate_group(table_plan(ard), col = "PARAM"))
  expect_identical(pv_values(p), list(c(PARAM = "ALT"), c(PARAM = "AST")))
})

test_that("the tfrmt page note is read as name: value pairs", {
  tbl <- list(`_heading` = list(preheader = "PARAM: A, B, VISIT: V1"))
  expect_identical(rtfreporter:::.tfrmt_page_note_values(tbl),
                   c(PARAM = "A, B", VISIT = "V1"))
  expect_null(rtfreporter:::.tfrmt_page_note_values(
    list(`_heading` = list(subtitle = "Safety population"))))
})

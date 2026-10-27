# Drawing a page: a page of one segment and a page of several by the same
# code (R/render_page.R)

.rp_df <- function(n) data.frame(Item = sprintf("Row %02d", seq_len(n)), N = seq_len(n))
.rp_bs <- intToUtf8(92)

test_that("a later segment's font and colour are in the document's tables", {
  doc <- rtf_document() |> rtf_tables(list(.rp_df(3)), titles = "T1") |>
    rtf_tables(list(rtftable(.rp_df(3), font = "Arial",
                             col_spec = list(list(col = 2, color = "#FF0000")))),
               titles = "T2", page_break = FALSE)
  txt <- .render_to_string(doc)
  expect_true(grepl(paste0(.rp_bs, "f1", .rp_bs, "fnil", .rp_bs, "fcharset0 Arial;"), txt,
                    fixed = TRUE))
  expect_true(grepl(paste0(.rp_bs, "red255", .rp_bs, "green0", .rp_bs, "blue0;"), txt,
                    fixed = TRUE))
  expect_true(grepl(paste0(.rp_bs, "f1 Row 01"), txt, fixed = TRUE))
  expect_true(grepl(paste0(.rp_bs, "cf3 1"), txt, fixed = TRUE))
})

test_that("a page with no content draws its title and footnote", {
  rep <- .rtfreport_add_page(.new_rtfreport(), title = "Only a title", footnote = "and a note")
  txt <- suppressMessages(.render_to_string(rep))
  expect_true(grepl("Only a title", txt, fixed = TRUE))
  expect_true(grepl("and a note", txt, fixed = TRUE))
  expect_false(grepl(paste0(.rp_bs, "pict"), txt, fixed = TRUE))
})

test_that(".render_content(): one method per kind of content; none for no content", {
  expect_identical(.render_content(NULL, list()), character(0))
  registerS3method(".render_content", "rp_test_content",
                   function(x, ctx) paste("drawn", x$label), envir = asNamespace("rtfreporter"))
  expect_identical(.render_content(structure(list(label = "here"), class = "rp_test_content"),
                                   list()),
                   "drawn here")
})

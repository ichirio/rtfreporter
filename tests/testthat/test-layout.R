# The blocks of a document (R/blocks.R) and rtf_layout(): its contents as
# printed pages (R/layout.R)

.ly_df <- function(n, prefix = "Row") {
  data.frame(Item = sprintf("%s %02d", prefix, seq_len(n)), N = seq_len(n),
             stringsAsFactors = FALSE)
}
.ly_png <- function() {
  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = 300, height = 150)
  graphics::plot(1)
  grDevices::dev.off()
  f
}

test_that("each content is a block with its own title, footnote and placement", {
  doc <- rtf_document() |>
    rtf_tables(list(A = .ly_df(3), B = .ly_df(4)), titles = list("T1", "T2"),
               footnotes = "F") |>
    rtf_tables(list(.ly_df(2)), page_break = FALSE, space_before = 2)
  expect_s3_class(doc$contents, "rtf_contents")
  expect_length(doc$contents, 3L)
  b <- doc$contents[[2L]]
  expect_s3_class(b, "rtf_block")
  expect_named(b, c("content", "group", "page_in_group", "title", "footnote",
                    "page_break", "space_before", "cont_title", "section"))
  expect_s3_class(b$content, "rtftable")
  expect_identical(b$title, "T2")
  expect_identical(b$footnote, "F")
  expect_identical(names(doc$contents), c("A", "B", ""))
  expect_identical(unname(vapply(doc$contents, `[[`, NA, "page_break")), c(TRUE, TRUE, FALSE))
  expect_identical(unname(vapply(doc$contents, `[[`, 1L, "space_before")), c(0L, 0L, 2L))
  expect_identical(unname(vapply(doc$contents, `[[`, 1L, "group")), 1:3)
  # `[` keeps the class
  expect_s3_class(doc$contents[2:3], "rtf_contents")
  expect_length(doc$contents[2:3], 2L)
})

test_that("rtf_titles() / rtf_footnotes() write every block's, NULL included", {
  doc <- rtf_document() |>
    rtf_tables(list(.ly_df(3), .ly_df(3)), titles = "T", footnotes = "F") |>
    rtf_titles(list("New 1", NULL)) |>
    rtf_footnotes("G")
  expect_identical(.doc_titles(doc), list("New 1", NULL))
  expect_identical(.doc_footnotes(doc), list("G", "G"))
  # the field is kept, its value NULL
  expect_named(doc$contents[[2L]], names(doc$contents[[1L]]))
  expect_error(rtf_titles(doc, list("a", "b", "c")), "has 3 blocks")
})

test_that("rtf_layout(): a block begins a page unless it goes on the one before", {
  pages <- as_rtftables(.ly_df(30), split = "rows", split_rows = 11)
  doc <- rtf_document() |>
    rtf_tables(list(Summary = .ly_df(5)), titles = "Table 1") |>
    rtf_tables(pages, page_break = FALSE) |>
    rtf_tables(list(.ly_df(4)), page_break = FALSE, space_before = 1) |>
    rtf_tables(list(.ly_df(2)))
  lay <- rtf_layout(doc)
  expect_s3_class(lay, "rtf_pages")
  expect_identical(lay$n_blocks, 5L)
  # the table's later page begins a page; the next flows onto it
  segs <- lapply(lay$pages, function(p) vapply(p$segments, `[[`, 1L, "block"))
  expect_identical(segs, list(1:2, 3:4, 5L))
  expect_identical(vapply(lay$pages, `[[`, 1L, "number"), 1:3)
  s <- lay$pages[[1L]]$segments[[1L]]
  expect_identical(s$name, "Summary")
  expect_identical(s$title, "Table 1")
  expect_identical(lay$pages[[2L]]$segments[[2L]]$space_before, 1L)
  expect_identical(lapply(lay$pages[[2L]]$segments, `[[`, "page_in_group"), list(2L, 1L))
  # a document's first content begins its first page, whatever it says
  first <- rtf_document() |> rtf_tables(list(.ly_df(2)), page_break = FALSE)
  expect_length(rtf_layout(first)$pages, 1L)
  expect_length(rtf_layout(rtf_document())$pages, 0L)
  expect_error(rtf_layout(list()), "rtf_document")
})

test_that("rtf_layout(): the sections the pages are in", {
  doc <- rtf_document() |>
    rtf_section(header = rtf_header("All")) |>
    rtf_tables(list(.ly_df(2))) |>
    rtf_tables(list("Part A" = .ly_df(2), "Part A" = .ly_df(2), "Part B" = .ly_df(2)),
               auto_section = TRUE) |>
    rtf_section(page = 4, header = rtf_header("Own"))
  lay <- rtf_layout(doc)
  expect_identical(vapply(lay$pages, `[[`, NA, "section_start"), c(TRUE, TRUE, FALSE, TRUE))
  expect_identical(vapply(lay$pages, `[[`, 1L, "section"), c(1L, 2L, 2L, 3L))
  # an explicit section on the page wins over the auto one (#548)
  expect_identical(lapply(lay$pages, `[[`, "section_label"),
                   list(NULL, "Part A", NULL, NULL))
  # each block that opens a section says so
  expect_identical(unname(lapply(doc$contents, `[[`, "section")),
                   list(NULL, list(label = "Part A", align = "left"), NULL,
                        list(label = "Part B", align = "left")))
})

test_that("rtf_layout(): a page's section is the first block on it with one (12.3)", {
  sec <- function(lay) lapply(lay$pages, `[[`, "section_label")
  a <- function(...) rtf_tables(..., auto_section = TRUE)
  # 1. the first has one, the second none: the first's
  d1 <- rtf_document() |> a(list("A" = .ly_df(2))) |>
    rtf_tables(list(.ly_df(2)), page_break = FALSE)
  expect_identical(sec(rtf_layout(d1)), list("A"))
  # 2. the first none, the second one: the second's
  d2 <- rtf_document() |> rtf_tables(list(.ly_df(2))) |>
    a(list("B" = .ly_df(2)), page_break = FALSE)
  expect_identical(sec(rtf_layout(d2)), list("B"))
  # 3. both, different: the first's, and a warning; no page is added
  d3 <- rtf_document() |> a(list("A" = .ly_df(2))) |>
    a(list("B" = .ly_df(2)), page_break = FALSE) |>
    a(list("C" = .ly_df(2)), page_break = FALSE)
  expect_warning(lay3 <- rtf_layout(d3), "page 1 holds the auto_section headings 'A' and 'B'")
  expect_length(lay3$pages, 1L)
  expect_identical(sec(lay3), list("A"))
  #    the same heading twice says nothing
  d3b <- rtf_document() |> a(list("A" = .ly_df(2))) |>
    a(list("A" = .ly_df(2)), page_break = FALSE)
  expect_no_warning(rtf_layout(d3b))
  # 4. none on a page: it stays in the section before it
  d4 <- rtf_document() |> a(list("A" = .ly_df(2))) |> rtf_tables(list(.ly_df(2)))
  lay4 <- rtf_layout(d4)
  expect_identical(vapply(lay4$pages, `[[`, 1L, "section"), c(1L, 1L))
  # an explicit section on the page wins (#548)
  d5 <- d2 |> rtf_section(page = 1, header = rtf_header("Own"))
  expect_identical(sec(rtf_layout(d5)), list(NULL))
  # print() shows the layout without the warning
  expect_silent(capture.output(print(d3)))
})

test_that(".rtf_content(): pages that know which table they are a page of", {
  pages <- as_rtftables(.ly_df(30), split = "rows", split_rows = c(11, 21))
  # a list of built pages keeps its tables together
  p <- .rtf_content(c(list(rtftable(.ly_df(2))), pages, list(rtftable(.ly_df(3)))))
  expect_length(p, 5L)
  expect_identical(attr(p, "group"), c(1L, 2L, 2L, 2L, 3L))
  expect_identical(attr(p, "page_in_group"), c(1L, 1L, 2L, 3L, 1L))
  # a later page with nothing before it is still a later page
  expect_identical(attr(.rtf_content(pages[2:3]), "page_in_group"), 2:3)
  # a data frame is a page of rtftable()'s; a figure is drawn
  png <- .ly_png()
  on.exit(unlink(png), add = TRUE)
  q <- .rtf_content(list(a = .ly_df(2), b = png))
  expect_s3_class(q[[1L]], "rtftable")
  expect_s3_class(q[[2L]], "rtfplot")
  expect_named(q, c("a", "b"))
  expect_error(.rtf_content(list(.ly_df(2), 42)), "Item 2")
})

test_that(".content_split(): first_rows on a plan, a data frame, an rtftable, a list of one", {
  rows <- function(p) vapply(p, function(x) nrow(x$data), 1)
  expect_identical(rows(.content_split(.ly_df(20), 6L)), c(6, 14))
  expect_identical(rows(.content_split(rtftable(.ly_df(20)), 6L)), c(6, 14))
  expect_identical(rows(.content_split(list(.ly_df(20)), 6L)), c(6, 14))
  expect_length(.content_split(rtftable(.ly_df(4)), 6L), 1L)
  expect_error(.content_split(list(.ly_df(2), .ly_df(2)), 1L), "takes one table")
  expect_error(.content_split("x", 1L), "takes one table")
  p <- .rtf_content(.ly_df(20), first_rows = 6)
  expect_identical(attr(p, "group"), c(1L, 1L))
  expect_identical(attr(p, "page_in_group"), 1:2)
})

test_that("print() shows each page's tables (rows) and figures (size)", {
  png <- .ly_png()
  on.exit(unlink(png), add = TRUE)
  pages <- as_rtftables(.ly_df(30), split = "rows", split_rows = 11)
  doc <- rtf_document() |>
    rtf_tables(list(Demog = .ly_df(12)), titles = c("Table 1", "Safety"),
               footnotes = "Source") |>
    rtf_tables(pages, titles = "Listing 1", page_break = FALSE, space_before = 1) |>
    rtf_figures(rtfplot(png, width_twips = 4320L), titles = "Figure 1")
  out <- capture.output(print(doc))
  expect_identical(out, c(
    "rtf_document: 4 blocks on 3 pages (letter landscape, 9 pt)",
    " page 1  [sec 1]  Demog  rtftable 12 rows        title 2  foot 1",
    "      +                  rtftable 10 rows (1/2)  title 1  space 1",
    " page 2                  rtftable 20 rows (2/2)  title 1",
    " page 3                  rtfplot 3.0 x 1.5 in    title 1"))
  expect_identical(capture.output(print(rtf_layout(doc)))[-1L], out[-1L])
  expect_match(capture.output(print(rtf_layout(doc)))[1L], "^rtf_pages: 4 blocks")
  expect_output(print(doc$contents), "<rtf_contents> 4 blocks")
  expect_output(print(doc$contents[[2L]]), "rtftable 10 rows (group 2, page 1)", fixed = TRUE)
})

test_that("a document whose blocks all break writes what each block alone would", {
  one <- rtf_document() |> rtf_tables(list(.ly_df(3)), titles = "A")
  two <- one |> rtf_tables(list(.ly_df(4)), titles = "B")
  r <- .pipe_doc_to_rtfreport(two)
  expect_length(r$pages, 2L)
  # (a page is the segments on it: here one each)
  expect_identical(lengths(lapply(r$pages, `[[`, "segments")), c(1L, 1L))
  expect_identical(r$pages[[2L]]$title, "B")
})

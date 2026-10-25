# rtf_tables() / rtf_figures(page_break = FALSE): a content under the one
# before it, on the page that one ends on (flow placement)

.fl_df <- function(n, prefix = "Item") {
  data.frame(Item = sprintf("%s %02d", prefix, seq_len(n)), A = seq_len(n),
             B = sprintf("%.2f", seq_len(n) / 7), stringsAsFactors = FALSE)
}
.fl_pages <- function(doc) .pipe_doc_to_rtfreport(doc)$pages
.fl_rows <- function(p) {
  bl <- p$blocks %||% list(list(content = p$content))
  vapply(bl, function(b) {
    if (inherits(b$content, "rtftable")) nrow(b$content$data) else NA_real_
  }, 1)
}
.fl_count <- function(txt, what) lengths(regmatches(txt, gregexpr(what, txt, fixed = TRUE)))
.fl_page_cmd <- paste0(intToUtf8(92), "page")
.fl_png <- function(w = 400, h = 300) {
  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = w, height = h)
  graphics::plot(1)
  grDevices::dev.off()
  f
}

test_that("the default and page_break = TRUE leave the document as it was", {
  d1 <- rtf_document() |> rtf_tables(list(.fl_df(5))) |> rtf_tables(list(.fl_df(6)))
  d2 <- rtf_document() |> rtf_tables(list(.fl_df(5))) |>
    rtf_tables(list(.fl_df(6)), page_break = TRUE)
  expect_null(d1$placement)
  expect_null(d2$placement)
  expect_identical(.render_to_string(d1), .render_to_string(d2))
})

test_that("page_break = FALSE: the table goes on under the one before, whole; nothing measured", {
  doc <- rtf_document() |>
    rtf_tables(list(.fl_df(8)), titles = list("Table 1")) |>
    rtf_tables(list(.fl_df(70, "Row")), titles = list("Listing 1"), page_break = FALSE) |>
    rtf_tables(list(.fl_df(5, "More")), page_break = FALSE)
  pg <- .fl_pages(doc)
  expect_length(pg, 1L)
  expect_identical(.fl_rows(pg[[1L]]), c(8, 70, 5))
  txt <- .render_to_string(doc)
  expect_identical(.fl_count(txt, .fl_page_cmd), 0L)
  expect_lt(regexpr("Table 1", txt, fixed = TRUE), regexpr("Listing 1", txt, fixed = TRUE))
  # (no header row is marked for the reader to repeat)
  expect_identical(.fl_count(txt, paste0(intToUtf8(92), "trhdr")), 0L)
})

test_that("first_rows: those rows on the shared page, the rest by the table's own max_rows", {
  skip_if_not_installed("cards")
  ard <- cards::bind_ard(
    cards::ard_tabulate(cards::ADSL, variables = ARM),
    cards::ard_tabulate(cards::ADSL, by = ARM, variables = c(AGEGR1, SEX, RACE, ETHNIC, DCREASCD)))
  p <- table_plan(normalize_ard(ard), cols = "ARM") |> plan_paginate_rows(max_rows = 12, split = "group_force")
  whole <- as_rtftables(p)
  n_all <- sum(vapply(whole, function(x) nrow(x$data), 1))
  doc <- rtf_document() |>
    rtf_tables(list(.fl_df(5)), titles = list("Table 1")) |>
    rtf_tables(p, titles = list("Table 2"), page_break = FALSE, first_rows = 4,
               cont_title = "(continued)")
  pg <- .fl_pages(doc)
  expect_identical(.fl_rows(pg[[1L]]), c(5, 4))
  later <- vapply(pg[-1L], function(x) nrow(x$content$data), 1)
  expect_true(all(later <= 12))
  # every row once (group_force repeats a cut group's heading, "(Cont.)")
  cont <- sum(vapply(pg[-1L], function(x) sum(grepl("(Cont.)", x$content$data[[1L]],
                                                     fixed = TRUE)), 1))
  expect_identical(4 + sum(later) - cont, n_all)
  txt <- .render_to_string(doc)
  # every later page: its title, the continued line, its column header
  expect_identical(.fl_count(txt, "Table 2"), length(pg))
  expect_identical(.fl_count(txt, "(continued)"), length(pg) - 1L)
  expect_identical(.fl_count(txt, .fl_page_cmd), length(pg) - 1L)
})

test_that("first_rows on a data frame and an rtftable; as_rtftables(first_rows = )", {
  pages <- as_rtftables(.fl_df(50, "Row"), split = "group_force", max_rows = 20, first_rows = 7)
  expect_identical(vapply(pages, function(x) nrow(x$data), 1), c(7, 20, 20, 3))
  expect_identical(pages[[2L]]$data$Item[1L], "Row 08")
  expect_identical(attr(pages[[2L]]$data, "rtf_paginate_meta")$page_index, 2L)
  # split = "rows": its positions are the whole table's
  pr <- as_rtftables(.fl_df(30), split = "rows", split_rows = c(11, 21), first_rows = 5)
  expect_identical(vapply(pr, function(x) nrow(x$data), 1), c(5, 5, 10, 10))
  expect_error(as_rtftables(.fl_df(10), page_by = "Item", first_rows = 2), "first_rows")
  base <- rtf_document() |> rtf_tables(list(.fl_df(3)))
  d1 <- rtf_tables(base, .fl_df(20, "Row"), page_break = FALSE, first_rows = 6)
  expect_identical(lapply(.fl_pages(d1), .fl_rows), list(c(3, 6), 14))
  d2 <- rtf_tables(base, list(rtftable(.fl_df(20, "Row"))), page_break = FALSE, first_rows = 6)
  expect_identical(lapply(.fl_pages(d2), .fl_rows), list(c(3, 6), 14))
  # all of it fits the rows given: one block, nothing after
  d3 <- rtf_tables(base, .fl_df(4, "Row"), page_break = FALSE, first_rows = 6)
  expect_length(.fl_pages(d3), 1L)
  # pages already built take it where they are built
  expect_error(rtf_tables(base, as_rtftables(.fl_df(20), split = "rows", split_rows = 11),
                          page_break = FALSE, first_rows = 6),
               "as_rtftables(first_rows = )", fixed = TRUE)
  expect_error(rtf_tables(base, .fl_df(20), first_rows = 6), "page_break = FALSE")
  expect_error(rtf_tables(base, .fl_df(20), page_break = FALSE, first_rows = 0), "first_rows")
  expect_error(rtf_tables(base, .fl_df(20), page_break = FALSE, cont_title = "x"), "first_rows")
})

test_that("a plan's or as_rtftables()'s later pages begin a page of their own", {
  pages <- as_rtftables(.fl_df(60, "Row"), split = "rows", split_rows = 25)
  doc <- rtf_document() |> rtf_tables(list(.fl_df(5))) |>
    rtf_tables(pages, page_break = FALSE)
  expect_identical(vapply(doc$placement, `[[`, NA, "page_break"), c(TRUE, FALSE, TRUE))
  pg <- .fl_pages(doc)
  expect_length(pg, 2L)
  expect_identical(.fl_rows(pg[[1L]]), c(5, nrow(pages[[1L]]$data)))
  # the document's first content: no page before it, nothing to flow onto
  d0 <- rtf_document() |> rtf_tables(list(.fl_df(5)), page_break = FALSE)
  expect_length(.fl_pages(d0), 1L)
})

test_that("space_before adds empty lines between the two", {
  base <- rtf_document() |> rtf_tables(list(.fl_df(3)))
  a <- .render_to_string(rtf_tables(base, list(.fl_df(3)), page_break = FALSE))
  b <- .render_to_string(rtf_tables(base, list(.fl_df(3)), page_break = FALSE, space_before = 2))
  blank <- paste0("{", intToUtf8(92), "pard", intToUtf8(92), "par}")
  expect_identical(.fl_count(b, blank) - .fl_count(a, blank), 2L)
})

test_that("rtf_figures(page_break = FALSE): a figure under the content before it, whatever its size", {
  png_path <- .fl_png()
  on.exit(unlink(png_path), add = TRUE)
  doc <- rtf_document() |>
    rtf_tables(list(.fl_df(40)), titles = list("Table 1")) |>
    rtf_figures(list(rtfplot(png_path, width_twips = 9000L)), titles = list("Figure 1"),
                page_break = FALSE)
  pg <- .fl_pages(doc)
  expect_length(pg, 1L)
  expect_true(inherits(pg[[1L]]$blocks[[2L]]$content, "rtfplot"))
  txt <- .render_to_string(doc)
  expect_identical(.fl_count(txt, .fl_page_cmd), 0L)
  pict <- regexpr(paste0(intToUtf8(92), "pict"), txt, fixed = TRUE)
  expect_gt(pict, regexpr("Item 40", txt, fixed = TRUE))
  expect_gt(pict, regexpr("Figure 1", txt, fixed = TRUE))
  # a document with flowing tables: a figure of page_break = TRUE its own page
  d2 <- rtf_document() |> rtf_tables(list(.fl_df(3))) |>
    rtf_tables(list(.fl_df(3)), page_break = FALSE) |>
    rtf_figures(list(rtfplot(png_path)))
  expect_length(d2$placement, 3L)
  expect_length(.fl_pages(d2), 2L)
  small <- rtfplot(png_path, width_twips = 2000L)
  expect_error(rtf_figures(rtf_document(), list(small, small), page_break = c(TRUE, FALSE, TRUE)),
               "rtf_figures(): `page_break`", fixed = TRUE)
})

test_that("a table that flows opens no auto section, and says so", {
  doc <- rtf_document() |>
    rtf_tables(list("Part A" = .fl_df(4))) |>
    rtf_tables(list("Part B" = .fl_df(4)), auto_section = TRUE, page_break = FALSE)
  expect_warning(r <- .pipe_doc_to_rtfreport(doc), "opens no section")
  expect_length(r$pages, 1L)
})

test_that("the arguments are checked", {
  d <- rtf_document()
  expect_error(rtf_tables(d, list(.fl_df(2), .fl_df(2)), page_break = c(TRUE, FALSE, TRUE)),
               "one for every")
  expect_error(rtf_tables(d, list(.fl_df(2)), page_break = NA), "TRUE or FALSE")
  expect_error(rtf_tables(d, list(.fl_df(2)), space_before = -1), "space_before")
})

test_that(".rtftable_cut(): the rows, cell styles and blank rows divided; one data frame only", {
  tbl <- rtftable(.fl_df(6))
  tbl$blank_rows <- c(2L, 3L, 5L)
  attr(tbl$data, "rtf_titles") <- list("T")
  cut <- .rtftable_cut(tbl, 3L)
  expect_identical(nrow(cut[[1L]]$data), 3L)
  expect_identical(cut[[2L]]$data$Item, sprintf("Item %02d", 4:6))
  expect_identical(cut[[1L]]$blank_rows, 2L)
  expect_identical(cut[[2L]]$blank_rows, 2L)
  expect_identical(attr(cut[[2L]]$data, "rtf_titles"), list("T"))
  expect_identical(cut[[2L]]$col_header, tbl$col_header)
  expect_error(.rtftable_cut(tbl, 6L), "within the rows")
  expect_error(.rtftable_cut(rtftable(list(.fl_df(2), .fl_df(2))), 1L), "one data frame")
})

test_that("a document the reader lays out: {PAGE} / {TOTAL_PAGES} are the reader's own numbers", {
  hdr <- function(d) d |> rtf_section(header = rtf_header(list(c(r = "Page {PAGE} of {TOTAL_PAGES}"))))
  txt <- function(d) {
    f <- tempfile(fileext = ".rtf")
    on.exit(unlink(f))
    generate_rtfreport(d, f)
    paste(readLines(f, warn = FALSE), collapse = "\n")
  }
  chpgn <- paste0(intToUtf8(92), "chpgn")
  # flowed: the page number and the total are the reader's fields
  flowed <- txt(hdr(rtf_document()) |> rtf_tables(list(.fl_df(5))) |>
                  rtf_tables(list(.fl_df(60)), page_break = FALSE))
  expect_true(grepl(chpgn, flowed, fixed = TRUE))
  expect_true(grepl("NUMPAGES", flowed, fixed = TRUE))
  expect_false(grepl("Page 1 of", flowed, fixed = TRUE))
  # not flowed: as before, the numbers written
  paged <- txt(hdr(rtf_document()) |> rtf_tables(list(.fl_df(5))) |> rtf_tables(list(.fl_df(6))))
  expect_true(grepl("Page 1 of 2", paged, fixed = TRUE))
  expect_false(grepl(chpgn, paged, fixed = TRUE))
})

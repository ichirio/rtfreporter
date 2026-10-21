# rtf_tables(page_break = FALSE): a table on what is left of the page
# before it (flow placement, Phase 1: tables)

.fl_df <- function(n, prefix = "Item") {
  data.frame(Item = sprintf("%s %02d", prefix, seq_len(n)), A = seq_len(n),
             B = sprintf("%.2f", seq_len(n) / 7), stringsAsFactors = FALSE)
}
.fl_pages <- function(doc) .pipe_doc_to_rtfreport(doc)$pages
.fl_rows <- function(p) {
  bl <- p$blocks %||% list(list(content = p$content))
  vapply(bl, function(b) nrow(b$content$data), 1)
}
.fl_count <- function(txt, what) lengths(regmatches(txt, gregexpr(what, txt, fixed = TRUE)))
.fl_page_cmd <- paste0(intToUtf8(92), "page")

test_that("the default and page_break = TRUE leave the document as it was", {
  d1 <- rtf_document() |> rtf_tables(list(.fl_df(5))) |> rtf_tables(list(.fl_df(6)))
  d2 <- rtf_document() |> rtf_tables(list(.fl_df(5))) |>
    rtf_tables(list(.fl_df(6)), page_break = TRUE)
  expect_null(d1$placement)
  expect_null(d2$placement)
  expect_identical(.render_to_string(d1), .render_to_string(d2))
})

test_that("a short table goes under the one before, on its page", {
  doc <- rtf_document() |>
    rtf_tables(list(.fl_df(8)), titles = list("Table 1")) |>
    rtf_tables(list(.fl_df(10, "Row")), titles = list("Listing 1"), page_break = FALSE)
  pg <- .fl_pages(doc)
  expect_length(pg, 1L)
  expect_length(pg[[1L]]$blocks, 2L)
  expect_identical(.fl_rows(pg[[1L]]), c(8, 10))
  txt <- .render_to_string(doc)
  expect_identical(.fl_count(txt, .fl_page_cmd), 0L)
  # each its own title, the second under the first table
  expect_lt(regexpr("Table 1", txt, fixed = TRUE), regexpr("Listing 1", txt, fixed = TRUE))
})

test_that("fit = TRUE: a long table is cut where the page ends; the rest begins the next, headers again", {
  doc <- rtf_document() |>
    rtf_tables(list(.fl_df(8)), titles = list("Table 1")) |>
    rtf_tables(list(.fl_df(70, "Row")), titles = list("Listing 1"), page_break = FALSE,
               fit = TRUE, cont_title = "(continued)")
  pg <- .fl_pages(doc)
  expect_length(pg, 2L)
  first <- .fl_rows(pg[[1L]])
  expect_identical(first[1L], 8)
  expect_gte(first[2L], 3)
  expect_identical(sum(first[-1L]) + sum(.fl_rows(pg[[2L]])), 70)
  # the rest: its first row the row after the cut
  expect_identical(pg[[2L]]$content$data$Item[1L], sprintf("Row %02d", first[2L] + 1L))
  txt <- .render_to_string(doc)
  expect_identical(.fl_count(txt, .fl_page_cmd), 1L)
  expect_identical(.fl_count(txt, "(continued)"), 1L)
  expect_identical(.fl_count(txt, "Listing 1"), 2L)
  # the column header: the table's, and the listing's on each of its pages
  expect_identical(lengths(regmatches(txt, gregexpr("Item[^ 0-9]", txt))), 3L)
})

test_that("fit = TRUE, fewer than min_rows left: the table begins a new page", {
  doc <- rtf_document() |>
    rtf_tables(list(.fl_df(44))) |>
    rtf_tables(list(.fl_df(30, "Row")), page_break = FALSE, fit = TRUE, min_rows = 10)
  pg <- .fl_pages(doc)
  expect_length(pg, 2L)
  expect_null(pg[[1L]]$blocks)
  expect_identical(.fl_rows(pg[[2L]]), 30)
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

test_that("a figure after flowing tables has a page of its own", {
  png_path <- tempfile(fileext = ".png")
  on.exit(unlink(png_path), add = TRUE)
  grDevices::png(png_path, width = 400, height = 300)
  graphics::plot(1)
  grDevices::dev.off()
  doc <- rtf_document() |> rtf_tables(list(.fl_df(3))) |>
    rtf_tables(list(.fl_df(3)), page_break = FALSE) |>
    rtf_figures(list(rtfplot(png_path)))
  expect_length(doc$placement, 3L)
  expect_length(.fl_pages(doc), 2L)
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
  expect_error(rtf_tables(d, list(.fl_df(2)), min_rows = 0), "min_rows")
  expect_error(rtf_tables(d, list(.fl_df(2)), space_before = -1), "space_before")
  expect_error(rtf_tables(d, list(.fl_df(2)), cont_title = c("a", "b")), "cont_title")
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

test_that("a group's heading is not left last on the page", {
  d <- data.frame(Term = c("SOC A", "  pt 1", "  pt 2", "SOC B", "  pt 3", "  pt 4"),
                  N = c("3", "1", "2", "2", "1", "1"), stringsAsFactors = FALSE)
  m <- .flow_metrics(rtf_document(), .new_rtfreport())
  one <- .flow_table_heights(rtftable(d), m)$body[1L]
  room <- .flow_block_height(NULL, NULL, NULL, m) + .flow_table_heights(rtftable(d), m)$head +
    4 * one + 1
  expect_identical(.flow_rows_fit(rtftable(d), NULL, NULL, room, m), 3L)
})

test_that("a row's wrapping text counts its lines", {
  m <- .flow_metrics(rtf_document(), .new_rtfreport())
  long <- data.frame(x = strrep("word ", 200), y = "1", stringsAsFactors = FALSE)
  short <- data.frame(x = "word", y = "1", stringsAsFactors = FALSE)
  expect_gt(.flow_table_heights(rtftable(long), m)$body,
            2 * .flow_table_heights(rtftable(short), m)$body)
})

# ---- phase 2: figures --------------------------------------------------------

.fl_png <- function(w, h) {
  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = w, height = h)
  graphics::plot(1)
  grDevices::dev.off()
  f
}

test_that("rtf_figures(page_break = FALSE): a figure under the table when the page holds it", {
  png_path <- .fl_png(400, 300)
  on.exit(unlink(png_path), add = TRUE)
  doc <- rtf_document() |>
    rtf_tables(list(.fl_df(5)), titles = list("Table 1")) |>
    rtf_figures(list(rtfplot(png_path, width_twips = 4000L)), titles = list("Figure 1"),
                page_break = FALSE)
  pg <- .fl_pages(doc)
  expect_length(pg, 1L)
  expect_true(inherits(pg[[1L]]$blocks[[2L]]$content, "rtfplot"))
  txt <- .render_to_string(doc)
  expect_identical(.fl_count(txt, .fl_page_cmd), 0L)
  pict <- regexpr(paste0(intToUtf8(92), "pict"), txt, fixed = TRUE)
  expect_gt(pict, regexpr("Item 05", txt, fixed = TRUE))
  expect_gt(pict, regexpr("Figure 1", txt, fixed = TRUE))
})

test_that("fit = TRUE: a figure the page cannot hold begins the next page, not cut", {
  png_path <- .fl_png(400, 300)
  on.exit(unlink(png_path), add = TRUE)
  doc <- rtf_document() |>
    rtf_tables(list(.fl_df(30))) |>
    rtf_figures(list(rtfplot(png_path, width_twips = 9000L)), page_break = FALSE, fit = TRUE)
  pg <- .fl_pages(doc)
  expect_length(pg, 2L)
  expect_null(pg[[2L]]$blocks)
  expect_true(inherits(pg[[2L]]$content, "rtfplot"))
  # two figures, the second under the first when both fit
  small <- rtfplot(png_path, width_twips = 2000L)
  d2 <- rtf_document() |> rtf_figures(list(small, small), page_break = FALSE)
  expect_length(.fl_pages(d2), 1L)
  expect_error(rtf_figures(rtf_document(), list(small, small), page_break = c(TRUE, FALSE, TRUE)),
               "rtf_figures(): `page_break`", fixed = TRUE)
})

# ---- the default: nothing measured, the reader carries the rest -------------

test_that("page_break = FALSE alone: the whole table goes on, uncut; Word carries the rest", {
  doc <- rtf_document() |>
    rtf_tables(list(.fl_df(8)), titles = list("Table 1")) |>
    rtf_tables(list(.fl_df(70, "Row")), titles = list("Listing 1"), page_break = FALSE,
               cont_title = "(continued)") |>
    rtf_tables(list(.fl_df(5, "More")), page_break = FALSE)
  pg <- .fl_pages(doc)
  expect_length(pg, 1L)
  expect_identical(.fl_rows(pg[[1L]]), c(8, 70, 5))
  txt <- .render_to_string(doc)
  expect_identical(.fl_count(txt, .fl_page_cmd), 0L)
  # (cont_title belongs to fit = TRUE)
  expect_identical(.fl_count(txt, "(continued)"), 0L)
  # a figure the same: under the table, whatever its size
  png_path <- .fl_png(400, 300)
  on.exit(unlink(png_path), add = TRUE)
  d2 <- rtf_tables(rtf_document(), list(.fl_df(40))) |>
    rtf_figures(list(rtfplot(png_path, width_twips = 9000L)), page_break = FALSE)
  expect_length(.fl_pages(d2), 1L)
  expect_error(rtf_tables(rtf_document(), list(.fl_df(2)), fit = NA), "`fit` is TRUE or FALSE")
})

test_that("the header rows of a table that runs on repeat on the reader's next page", {
  trhdr <- paste0(intToUtf8(92), "trhdr")
  sp <- rtftable(.fl_df(4, "Sp"), spanning_header = list(list(from = 2L, to = 3L, label = "Values")))
  doc <- rtf_document() |>
    rtf_tables(list(.fl_df(3)), titles = list("Table 1")) |>
    rtf_tables(list(sp), page_break = FALSE)
  txt <- .render_to_string(doc)
  # the flowing table's spanning row and label row; not the first table's
  expect_identical(.fl_count(txt, trhdr), 2L)
  rows <- strsplit(txt, paste0(intToUtf8(92), "row"), fixed = TRUE)[[1L]]
  marked <- grep(trhdr, rows, fixed = TRUE)
  expect_true(all(grepl("Values|Item", rows[marked])))
  # a document without page_break = FALSE: none, anywhere
  plain <- .render_to_string(rtf_document() |> rtf_tables(list(.fl_df(3))) |>
                               rtf_tables(list(sp)))
  expect_identical(.fl_count(plain, trhdr), 0L)
})

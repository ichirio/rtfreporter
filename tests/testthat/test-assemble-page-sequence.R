# The page numbers of an assembled file (#1049): every page is numbered in one
# Arabic sequence from the first page, a cover and the table of contents
# included, and the total is all pages.  Through assemble_rtf() /
# assemble_folder(), read back from the RTF written.  Fictional data only.
#
# A page's number is its position: the file writes no page-number command, so
# the reader counts the pages from the first.  A position is counted from the
# page starts before it: a section that starts a page (\sbkpage), a page break
# (\page) and the table of contents' own page breaks (\pagebb).

# One input of `n` pages, each page a one-row table "<tag> row p<i>".  `band`:
# "header" puts "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}" in the header;
# "slot" puts a {BOOK_PAGE} slot in the footer instead.
.ps_input <- function(dir, tag, n, band = c("header", "slot")) {
  band <- match.arg(band)
  hf <- if (band == "header") {
    list(header = rtf_header(rows = list(c(l = paste("Study EX-1", tag),
                                           r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}"))),
         footer = NULL)
  } else {
    list(header = rtf_header(rows = list(c(l = paste("Study EX-1", tag)))),
         footer = rtf_footer(c(c = "{BOOK_PAGE}")))
  }
  doc <- rtf_section(rtf_document(), page = 1, secinfo = hf)
  tbls <- lapply(seq_len(n), function(i) {
    data.frame(A = sprintf("%s row p%d", tag, i), stringsAsFactors = FALSE)
  })
  doc <- rtf_tables(doc, tbls, titles = list(c(paste("Table", tag))))
  f <- file.path(dir, paste0("t_", gsub(".", "_", tag, fixed = TRUE), ".rtf"))
  generate_rtfreport(doc, f, overwrite = TRUE)
  f
}

# Three inputs of 3, 2 and 2 pages: 7 body pages
.ps_inputs <- function(dir, band = "header") {
  c(.ps_input(dir, "14.1.1", 3L, band), .ps_input(dir, "14.1.2", 2L, band),
    .ps_input(dir, "14.1.3", 2L, band))
}
.ps_markers <- c(sprintf("14.1.1 row p%d", 1:3), sprintf("14.1.2 row p%d", 1:2),
                 sprintf("14.1.3 row p%d", 1:2))

.ps_read <- function(f) paste(readLines(f, warn = FALSE), collapse = "\n")

# The page a position of the file's text is on
.ps_page_at <- function(txt, pos) {
  head <- substr(txt, 1L, pos - 1L)
  n <- function(rx) {
    m <- gregexpr(rx, head, perl = TRUE)[[1L]]
    if (m[1L] == -1L) 0L else length(m)
  }
  n("\\\\sbkpage") + n("\\\\page(?![a-zA-Z])") + n("\\\\pagebb")
}
.ps_page_of <- function(txt, text) {
  pos <- regexpr(text, txt, fixed = TRUE)[[1L]]
  stopifnot(pos > 0L)
  .ps_page_at(txt, pos)
}

# What a band prints on page `page`: its \chpgn is that page's number, its
# NUMPAGES field the field's cached total.  `band` is "header" or "footer";
# the band is the last one written before `text`.
.ps_printed <- function(txt, text, band, page) {
  pos <- regexpr(text, txt, fixed = TRUE)[[1L]]
  bands <- gregexpr(paste0("\\{\\\\", band, "[^\n]*"), txt)[[1L]]
  k <- max(which(bands < pos))
  b <- substr(txt, bands[k], bands[k] + attr(bands, "match.length")[k] - 1L)
  b <- gsub("\\{\\\\field\\{\\\\\\*\\\\fldinst NUMPAGES\\}\\{\\\\fldrslt ([0-9]+)\\}\\}",
            "\\1", b)
  b <- gsub("\\chpgn ", as.character(page), b, fixed = TRUE)
  regmatches(b, regexpr("(Page|Book) [0-9]+ of [0-9]+", b))
}

.ps_pageref <- function(txt) {
  m <- regmatches(txt, gregexpr("PAGEREF [^ ]+ \\\\\\\\h\\}\\{\\\\fldrslt [0-9]+", txt))[[1L]]
  as.integer(sub(".*fldrslt ", "", m))
}
.ps_bookmark_pages <- function(txt) {
  pos <- gregexpr("\\{\\\\\\*\\\\bkmkstart ", txt)[[1L]]
  vapply(pos, function(p) .ps_page_at(txt, p), 0L)
}
.ps_numpages <- function(txt) {
  m <- regmatches(txt, gregexpr("fldinst NUMPAGES\\}\\{\\\\fldrslt [0-9]+", txt))[[1L]]
  as.integer(sub(".*fldrslt ", "", m))
}

.ps_cover <- list(title = "Study EX-1", subtitle = "Tables")

test_that("FT-AS7201 [FR-AS130]: a cover, a one-page table of contents and 7 body pages are numbered 1 of 9 .. 9 of 9: no restart word, every NUMPAGES cache 9, the first table's entry caches page 3", {
  d <- tempfile("ps")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- .ps_inputs(d)
  out <- file.path(d, "book.rtf")
  assemble_rtf(f, out, cover = .ps_cover, toc = "auto")
  txt <- .ps_read(out)
  # no page-number command anywhere: one sequence from the first page
  expect_identical(regmatches(txt, gregexpr("\\\\pgn[a-z]+", txt))[[1L]], character(0))
  # 9 pages: the cover is page 1, the table of contents page 2
  expect_identical(.ps_page_at(txt, nchar(txt) + 1L), 9L)
  expect_identical(.ps_page_of(txt, "\\fs44 Study EX-1"), 1L)
  expect_identical(.ps_page_of(txt, "\\fs28 Table of Contents"), 2L)
  # the body pages: positions 3 .. 9, and what their headers print
  pages <- vapply(.ps_markers, function(m) .ps_page_of(txt, m), 0L, USE.NAMES = FALSE)
  expect_identical(pages, 3:9)
  printed <- vapply(seq_along(.ps_markers), function(i) {
    .ps_printed(txt, .ps_markers[i], "header", pages[i])
  }, "")
  expect_identical(printed, sprintf("Page %d of 9", 3:9))
  # every total is all pages, the inputs' own included
  expect_identical(unique(.ps_numpages(txt)), 9L)
  # the table of contents: the first table on page 3, each entry the page its
  # file starts on
  expect_identical(.ps_pageref(txt), c(3L, 6L, 8L))
  expect_identical(.ps_pageref(txt), .ps_bookmark_pages(txt))
})

test_that("FT-AS7202 [FR-AS272 FR-AS138]: the same book with book_page: the filled slots read Book 3 of 9 on the first table's page .. Book 9 of 9 on the last", {
  d <- tempfile("ps")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- .ps_inputs(d, band = "slot")
  out <- file.path(d, "book.rtf")
  assemble_rtf(f, out, cover = .ps_cover, toc = "auto",
               book_page = "Book {AUTO_PAGE} of {AUTO_TOTAL_PAGES}")
  txt <- .ps_read(out)
  expect_identical(regmatches(txt, gregexpr("\\\\pgn[a-z]+", txt))[[1L]], character(0))
  expect_false(grepl("rtfreporterbookpage", txt, fixed = TRUE))   # every slot filled
  pages <- vapply(.ps_markers, function(m) .ps_page_of(txt, m), 0L, USE.NAMES = FALSE)
  expect_identical(pages, 3:9)
  printed <- vapply(seq_along(.ps_markers), function(i) {
    .ps_printed(txt, .ps_markers[i], "footer", pages[i])
  }, "")
  expect_identical(printed, sprintf("Book %d of 9", 3:9))
  expect_identical(unique(.ps_numpages(txt)), 9L)
  expect_identical(.ps_pageref(txt)[1L], 3L)
  expect_identical(.ps_pageref(txt), .ps_bookmark_pages(txt))
})

test_that("FT-AS7203 [FR-AS270]: without a cover the table of contents is page 1 of 8, the first table 2 of 8 and the last 8 of 8, through assemble_rtf() and assemble_folder() alike", {
  d <- tempfile("ps")
  o <- tempfile("ps-out")
  dir.create(d)
  dir.create(o)
  on.exit(unlink(c(d, o), recursive = TRUE), add = TRUE)
  f <- .ps_inputs(d)
  check <- function(out) {
    txt <- .ps_read(out)
    expect_identical(regmatches(txt, gregexpr("\\\\pgn[a-z]+", txt))[[1L]], character(0))
    expect_identical(.ps_page_at(txt, nchar(txt) + 1L), 8L)
    expect_identical(.ps_page_of(txt, "\\fs28 Table of Contents"), 1L)
    pages <- vapply(.ps_markers, function(m) .ps_page_of(txt, m), 0L, USE.NAMES = FALSE)
    expect_identical(pages, 2:8)
    printed <- vapply(seq_along(.ps_markers), function(i) {
      .ps_printed(txt, .ps_markers[i], "header", pages[i])
    }, "")
    expect_identical(printed, sprintf("Page %d of 8", 2:8))
    expect_identical(unique(.ps_numpages(txt)), 8L)
    expect_identical(.ps_pageref(txt), c(2L, 5L, 7L))
    expect_identical(.ps_pageref(txt), .ps_bookmark_pages(txt))
  }
  out_rtf <- file.path(o, "rtf.rtf")
  assemble_rtf(f, out_rtf, toc = "auto")
  check(out_rtf)
  out_folder <- file.path(o, "folder.rtf")
  assemble_folder(d, out_folder)
  check(out_folder)
})

test_that("FT-AS7204 [FR-AS271]: toc_page_numbering, whatever its value, stops assemble_rtf() and assemble_folder() with a message saying it was removed and to drop it; nothing is written", {
  d <- tempfile("ps")
  o <- tempfile("ps-out")
  dir.create(d)
  dir.create(o)
  on.exit(unlink(c(d, o), recursive = TRUE), add = TRUE)
  f <- .ps_inputs(d)
  out <- file.path(o, "book.rtf")
  sf <- file.path(o, "spec.csv")
  msg <- function(fn) {
    paste0("`", fn, "()`: `toc_page_numbering` was removed.  The pages of an ",
           "assembled file are numbered in one sequence from the first page (a ",
           "cover and the table of contents included), and the total counts ",
           "every page.\n  Drop the `toc_page_numbering` argument.")
  }
  for (pn in list("roman", "decimal", "none", NA)) {
    expect_error(assemble_rtf(f, out, toc = "auto", toc_page_numbering = pn),
                 msg("assemble_rtf"), fixed = TRUE)
    expect_error(assemble_folder(d, out, toc_page_numbering = pn),
                 msg("assemble_folder"), fixed = TRUE)
    # without an output_file too, before the spec file is written
    expect_error(assemble_folder(d, spec_file = sf, toc_page_numbering = pn),
                 msg("assemble_folder"), fixed = TRUE)
  }
  expect_false(file.exists(out))
  expect_false(file.exists(sf))
  # any other argument assemble_rtf() does not have is refused as well
  expect_error(assemble_rtf(f, out, toc = "auto", toc_numbering = "roman"),
               "`assemble_rtf()`: unused argument `toc_numbering`.", fixed = TRUE)
})

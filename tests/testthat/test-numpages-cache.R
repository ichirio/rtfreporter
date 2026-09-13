# assemble_rtf() must leave every NUMPAGES cache agreeing with the book (#415).

.np_rtf <- function(tag, n_pages, with_total = TRUE) {
  right <- if (with_total) "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}" else "Page {AUTO_PAGE}"
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1, secinfo = list(
    header = rtf_header(rows = list(c(l = tag, r = right))), footer = NULL))
  doc <- rtf_tables(doc, replicate(n_pages,
                                   data.frame(A = 1L, stringsAsFactors = FALSE),
                                   simplify = FALSE),
                    titles = list(c(tag)))
  f <- tempfile(fileext = ".rtf")
  generate_rtfreport(doc, f, overwrite = TRUE)
  f
}

# every cached NUMPAGES result in a file, as integers
.np_caches <- function(f) {
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
  m <- regmatches(txt, gregexpr("NUMPAGES\\}\\{\\\\fldrslt [^{}]*\\}", txt))[[1L]]
  sub(".*fldrslt ", "", sub("\\}$", "", m))
}

test_that("each deliverable bakes its own page count", {
  expect_identical(.np_caches(.np_rtf("T1", 3L)), "3")
  expect_identical(.np_caches(.np_rtf("T2", 2L)), "2")
})

test_that("assembling retotals every cache to the book (#415)", {
  f1 <- .np_rtf("T1", 3L); f2 <- .np_rtf("T2", 2L)
  out <- tempfile(fileext = ".rtf")
  on.exit(unlink(c(f1, f2, out)), add = TRUE)
  assemble_rtf(c(f1, f2), out, overwrite = TRUE)
  # Both inputs said 3 and 2; the book is 5 pages.
  expect_identical(unique(.np_caches(out)), "5")
  expect_length(.np_caches(out), 2L)
})

test_that("the retotal counts the front matter", {
  f1 <- .np_rtf("T1", 3L); f2 <- .np_rtf("T2", 2L)
  out <- tempfile(fileext = ".rtf")
  on.exit(unlink(c(f1, f2, out)), add = TRUE)
  assemble_rtf(c(f1, f2), out, overwrite = TRUE,
               toc = c("T1", "T2"), toc_page_numbering = "decimal")
  expect_identical(unique(.np_caches(out)), "6")   # 3 + 2 + the TOC page
})

test_that("a book_page slot and the inputs' own fields agree", {
  mk <- function(tag, n) {
    doc <- rtf_document()
    doc <- rtf_section(doc, page = 1, secinfo = list(
      header = rtf_header(rows = list(
        c(l = tag, r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}"))),
      footer = rtf_footer(c(c = "{BOOK_PAGE}"))))
    doc <- rtf_tables(doc, replicate(n, data.frame(A = 1L), simplify = FALSE),
                      titles = list(c(tag)))
    f <- tempfile(fileext = ".rtf"); generate_rtfreport(doc, f, overwrite = TRUE); f
  }
  f1 <- mk("T1", 3L); f2 <- mk("T2", 2L)
  out <- tempfile(fileext = ".rtf")
  on.exit(unlink(c(f1, f2, out)), add = TRUE)
  assemble_rtf(c(f1, f2), out, overwrite = TRUE,
               book_page = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}")
  # 2 header fields + 2 filled slots, all reading the book's 5.
  expect_identical(unique(.np_caches(out)), "5")
  expect_length(.np_caches(out), 4L)
})

test_that("a book whose inputs use no page tokens is untouched", {
  f1 <- .np_rtf("T1", 2L, with_total = FALSE)
  f2 <- .np_rtf("T2", 2L, with_total = FALSE)
  out1 <- tempfile(fileext = ".rtf"); out2 <- tempfile(fileext = ".rtf")
  on.exit(unlink(c(f1, f2, out1, out2)), add = TRUE)
  assemble_rtf(c(f1, f2), out1, overwrite = TRUE)
  assemble_rtf(c(f1, f2), out2, overwrite = TRUE)
  expect_length(.np_caches(out1), 0L)
  expect_identical(readLines(out1, warn = FALSE), readLines(out2, warn = FALSE))
})

test_that("the retotal rewrites only the cache, never the instruction", {
  f1 <- .np_rtf("T1", 3L); f2 <- .np_rtf("T2", 2L)
  out <- tempfile(fileext = ".rtf")
  on.exit(unlink(c(f1, f2, out)), add = TRUE)
  assemble_rtf(c(f1, f2), out, overwrite = TRUE)
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_identical(
    length(gregexpr("\\{\\\\field\\{\\\\\\*\\\\fldinst NUMPAGES\\}", txt)[[1L]]), 2L)
  # \chpgn is a control word, not a field: nothing to retotal, nothing touched.
  expect_match(txt, "\\\\chpgn")
})

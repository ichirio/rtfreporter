# {BOOK_PAGE}: a slot the deliverable reserves and assemble_rtf() fills (#413).

.slot_rtf <- function(tag, n_pages, slot = TRUE, band = "footer") {
  foot <- if (band == "footer") {
    rtf_footer(rows = list(c(c = "Confidential"),
                           c(c = if (slot) "{BOOK_PAGE}" else "")))
  } else {
    rtf_footer(c(c = "Confidential"))
  }
  head <- if (band == "header") {
    rtf_header(rows = list(c(l = tag, r = "Page {PAGE} of {TOTAL_PAGES}"),
                           c(r = if (slot) "{BOOK_PAGE}" else "")))
  } else {
    rtf_header(rows = list(c(l = tag, r = "Page {PAGE} of {TOTAL_PAGES}")))
  }
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1, secinfo = list(header = head, footer = foot))
  doc <- rtf_tables(doc, replicate(n_pages,
                                   data.frame(A = 1L, B = "x",
                                              stringsAsFactors = FALSE),
                                   simplify = FALSE),
                    titles = list(c(tag)))
  f <- tempfile(fileext = ".rtf")
  generate_rtfreport(doc, f, overwrite = TRUE)
  f
}

.slot_marker <- function() {
  rtfreporter:::.load_rtf_commands()$fields$book_page_slot
}

.read <- function(f) paste(readLines(f, warn = FALSE), collapse = "\n")

# ──────── the deliverable on its own ──────────────────────────────────────

test_that("{BOOK_PAGE} renders as an empty ignorable destination", {
  txt <- .read(.slot_rtf("T1", 2L))
  # The token itself must never survive into the output.
  expect_false(grepl("{BOOK_PAGE}", txt, fixed = TRUE))
  # What is emitted is a group a reader skips, so nothing is printed.
  expect_true(grepl(.slot_marker(), txt, fixed = TRUE))
  expect_match(.slot_marker(), "^\\{\\\\\\*\\\\")
})

test_that("the reserved row keeps its height, so filling it cannot shift anything", {
  # Same document, slot vs a plain empty row: the body must be identical.
  a <- .read(.slot_rtf("T1", 2L, slot = TRUE))
  b <- .read(.slot_rtf("T1", 2L, slot = FALSE))
  strip <- function(x) gsub(.slot_marker(), "", x, fixed = TRUE)
  expect_identical(strip(a), b)
})

# ──────── assembled ───────────────────────────────────────────────────────

test_that("assemble_rtf(book_page =) fills the slot with the book's numbers", {
  f1 <- .slot_rtf("T1", 3L); f2 <- .slot_rtf("T2", 2L)
  out <- tempfile(fileext = ".rtf")
  on.exit(unlink(c(f1, f2, out)), add = TRUE)
  assemble_rtf(c(f1, f2), out, overwrite = TRUE,
               book_page = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}")
  txt <- .read(out)
  expect_false(grepl(.slot_marker(), txt, fixed = TRUE))   # every slot filled
  expect_match(txt, "\\\\chpgn")
  expect_match(txt, "NUMPAGES")
  # The cached total is the BOOK's, not either input's (3 + 2, no front matter).
  expect_match(txt, "NUMPAGES\\}\\{\\\\fldrslt 5\\}")
})

test_that("the cached total counts the front matter too", {
  f1 <- .slot_rtf("T1", 3L); f2 <- .slot_rtf("T2", 2L)
  out <- tempfile(fileext = ".rtf")
  on.exit(unlink(c(f1, f2, out)), add = TRUE)
  assemble_rtf(c(f1, f2), out, overwrite = TRUE,
               toc = c("T1", "T2"), toc_page_numbering = "decimal",
               book_page = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}")
  # 3 + 2 body pages plus the TOC page.
  expect_match(.read(out), "NUMPAGES\\}\\{\\\\fldrslt 6\\}")
})

test_that("omitting book_page leaves the slots alone", {
  f1 <- .slot_rtf("T1", 2L); f2 <- .slot_rtf("T2", 2L)
  out <- tempfile(fileext = ".rtf")
  on.exit(unlink(c(f1, f2, out)), add = TRUE)
  assemble_rtf(c(f1, f2), out, overwrite = TRUE)
  expect_true(grepl(.slot_marker(), .read(out), fixed = TRUE))
})

test_that("the slot works in the header band as well as the footer", {
  f1 <- .slot_rtf("T1", 2L, band = "header")
  f2 <- .slot_rtf("T2", 2L, band = "header")
  out <- tempfile(fileext = ".rtf")
  on.exit(unlink(c(f1, f2, out)), add = TRUE)
  assemble_rtf(c(f1, f2), out, overwrite = TRUE,
               book_page = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}")
  txt <- .read(out)
  expect_false(grepl(.slot_marker(), txt, fixed = TRUE))
  # The per-table static numbers are untouched by the fill.
  expect_match(txt, "Page 1 of 2", fixed = TRUE)
})

test_that("static tokens are rejected in book_page", {
  f1 <- .slot_rtf("T1", 2L); f2 <- .slot_rtf("T2", 2L)
  out <- tempfile(fileext = ".rtf")
  on.exit(unlink(c(f1, f2, out)), add = TRUE)
  for (bad in c("Page {PAGE}", "of {TOTAL_PAGES}")) {
    expect_error(
      assemble_rtf(c(f1, f2), out, overwrite = TRUE, book_page = bad),
      "static", info = bad)
  }
  # {AUTO_PAGE} must not be mistaken for {PAGE}.
  expect_silent(assemble_rtf(c(f1, f2), out, overwrite = TRUE,
                             book_page = "Page {AUTO_PAGE}"))
})

# Validation tests: the page numbering of an assembled document --
# assemble_rtf() / assemble_folder() with toc_page_numbering, a cover, a
# table of contents of one or two pages, and inputs whose pages are written
# as page breaks or as sections -- a decision table over every combination,
# checked on the RTF written.  Helpers prefixed .vn_.  Fictional data only.
#
# ---------------------------------------------------------------------------
#  Design
# ---------------------------------------------------------------------------
#  The rules, as read from the code (R/assemble_rtf.R assemble_rtf(),
#  .build_toc_section(), .insert_pgnrestart(), .toc_entry_pages(),
#  .count_rtf_pages(), .retotal_numpages(); R/generate_rtfreport.R
#  .render_tokens(), .uses_static_page_token()):
#
#    commands  only a table of contents brings page-number commands: "roman"
#              writes \pgnrestart\pgnlcrm on the contents section and
#              \pgnrestart\pgndec after the first \sectd of the body;
#              "decimal" \pgnrestart\pgndec on the contents section only;
#              "none" nothing.  Without a contents nothing is written,
#              whatever toc_page_numbering says; a cover adds no command.
#    cached    each contents entry's PAGEREF caches the page its file starts
#              on: front + 1 + the pages of the files before it, front being
#              0 ("roman": the body restarts), the contents' pages
#              ("decimal": the contents restart at 1, a cover not counted) or
#              the cover and the contents ("none").
#    total     every NUMPAGES cache (the {AUTO_TOTAL_PAGES} of every input)
#              becomes the book's pages: the inputs', one for a cover, the
#              contents' own.
#    inputs    a footer with the static {PAGE} makes each page of a
#              paginated table a section of its own (\sbkpage), so {PAGE} is
#              that page's number; {AUTO_PAGE} only, the pages are \page
#              breaks in one section.  assemble_rtf() counts either; the
#              static numbers stay as written, {AUTO_PAGE} stays \chpgn.
#
#  Fixture: three inputs, t1 (two pages, static and dynamic tokens: two
#  sections), t2 (two pages, dynamic only: one section, one \page), t3 (one
#  page, static).  A contents of two pages is two entries whose labels are
#  long enough to fill one page each.
#
#  (A) FT-AS126, decision table, every combination: toc_page_numbering
#  (none, roman, decimal) x cover (no, yes) x contents (none, one page, two
#  pages) x inputs (t2 + t3; t1 + t2 + t3).  36 rows.  Checked in the file:
#  the \pgn commands and where they sit; the cached PAGEREF numbers; every
#  NUMPAGES cache; the pages (\sbkpage + \page + the contents' own \pagebb
#  breaks = the book's pages); the static numbers and \chpgn as the inputs
#  wrote them; no field without a contents.  FR-AS125 FR-AS126 FR-AS130
#  FR-AS131 FR-AS132 FR-AS140 FR-AS141 FR-IX111 FR-IX383 FR-IX384 FR-AS235
#  FR-IX396.
#
#  (B) FT-AS127, decision table: assemble_folder() x toc_page_numbering x
#  cover, 6 rows: the same numbers and commands as assemble_rtf() with the
#  contents the folder gives.  FR-IX383 FR-IX385.
#
#  Not repeated: FT-IX034 (the cached numbers with and without a cover, one
#  contents page, two one-page inputs).
# ---------------------------------------------------------------------------

.vn_tok <- function(x, rx) regmatches(x, gregexpr(rx, x, perl = TRUE))[[1L]]
.vn_read <- function(f) paste(readLines(f, warn = FALSE), collapse = "\n")
.vn_count <- function(x, rx) length(.vn_tok(x, rx))

# One input: `npg` pages, a footer with the static tokens or not.
.vn_input <- function(f, title, npg, static) {
  pages <- lapply(seq_len(npg), function(i) rtftable(data.frame(Parameter = sprintf("%s page %d", title, i), Value = "x")))
  ft <- c(r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}")
  if (static) ft <- c(l = "Pg {PAGE} of {TOTAL_PAGES}", ft)
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(footer = rtf_footer(ft))) |>
    rtf_tables(pages, titles = list(title))
  generate_rtfreport(doc, f, overwrite = TRUE)
  f
}

.vn_fixture <- function(dir) {
  list(t1 = .vn_input(file.path(dir, "t-14-1.rtf"), "Table 14.1", 2L, TRUE),
       t2 = .vn_input(file.path(dir, "t-14-2.rtf"), "Table 14.2", 2L, FALSE),
       t3 = .vn_input(file.path(dir, "t-14-3.rtf"), "Table 14.3", 1L, TRUE))
}

.vn_pgn <- function(x) .vn_tok(x, "\\\\pgn[a-z]+")
.vn_cached <- function(x) as.integer(sub(".*fldrslt ", "", .vn_tok(x, "PAGEREF[^}]*\\}\\{\\\\fldrslt [0-9]+")))
.vn_numpages <- function(x) as.integer(sub(".*fldrslt ", "", .vn_tok(x, "NUMPAGES\\}\\{\\\\fldrslt [0-9]+")))
.vn_static <- function(x) .vn_tok(x, "Pg [0-9]+ of [0-9]+")

test_that("FT-AS126 [FR-AS125 FR-AS126 FR-AS130 FR-AS131 FR-AS132 FR-AS140 FR-AS141 FR-IX111 FR-IX383 FR-IX384 FR-AS235 FR-IX396]: decision table -- toc_page_numbering x cover x contents pages x inputs", {
  d <- tempfile("vn-in-")
  o <- tempfile("vn-out-")
  dir.create(d)
  dir.create(o)
  on.exit(unlink(c(d, o), recursive = TRUE), add = TRUE)
  fx <- .vn_fixture(d)
  inputs <- list(two = c(fx$t2, fx$t3), three = c(fx$t1, fx$t2, fx$t3))
  npages <- c(t1 = 2L, t2 = 2L, t3 = 1L)
  src <- lapply(fx, .vn_read)
  # the inputs as written: t1 and t3 a section a page, t2 a \page break
  expect_identical(vapply(src, .vn_count, 0L, "\\\\sbkpage"), c(t1 = 2L, t2 = 1L, t3 = 1L))
  expect_identical(vapply(src, .vn_count, 0L, "\\\\page(?![a-zA-Z])"), c(t1 = 0L, t2 = 1L, t3 = 0L))
  expect_identical(.vn_static(src$t1), c("Pg 1 of 2", "Pg 2 of 2"))
  long <- strrep("Example Pharma listing of adverse events by system organ class ", 36)
  dt <- expand.grid(num = c("none", "roman", "decimal"), cover = c(FALSE, TRUE),
                    toc = c("none", "one", "two"), inputs = c("two", "three"), stringsAsFactors = FALSE)
  expect_identical(nrow(dt), 36L)
  for (i in seq_len(nrow(dt))) {
    r <- dt[i, ]
    label <- paste(r$num, r$cover, r$toc, r$inputs, sep = " / ")
    files <- inputs[[r$inputs]]
    np <- unname(npages[sub("^t-14-", "t", sub("[.]rtf$", "", basename(files)))])
    toc <- switch(r$toc, none = NULL,
                  one = sprintf("Table %d", seq_along(files)),
                  two = c(long, long, "Table 3")[seq_along(files)])
    tp <- switch(r$toc, none = 0L, one = 1L, two = 2L)
    out <- file.path(o, sprintf("book-%02d.rtf", i))
    args <- list(files, out, toc = toc, toc_page_numbering = r$num)
    if (r$cover) args$cover <- list(title = "Example Pharma STUDY-01", subtitle = "Tables")
    do.call(assemble_rtf, args)
    x <- .vn_read(out)
    total <- sum(np) + r$cover + tp
    # the commands
    pgn <- if (r$toc == "none") character(0) else
      switch(r$num, none = character(0), roman = c("\\pgnrestart", "\\pgnlcrm", "\\pgnrestart", "\\pgndec"),
             decimal = c("\\pgnrestart", "\\pgndec"))
    expect_identical(.vn_pgn(x), pgn, label = label)
    if (r$toc != "none" && r$num == "roman") {
      at <- function(rx) regexpr(rx, x, perl = TRUE)[[1L]]
      # i, ii on the contents; 1 again from the first table on
      expect_lt(at("\\\\pgnlcrm"), at("Table of Contents"), label = label)
      expect_lt(at("Table of Contents"), at("\\\\pgndec"), label = label)
      expect_lt(at("\\\\pgndec"), at("Table 14[.][0-9] page 1"), label = label)
    }
    # the cached numbers
    front <- switch(r$num, roman = 0L, decimal = tp, none = r$cover + tp)
    want <- if (r$toc == "none") integer(0) else as.integer(front + 1L + cumsum(c(0L, np[-length(np)])))
    expect_identical(.vn_cached(x), want, label = label)
    if (r$toc == "none") expect_false(grepl("HYPERLINK", x, fixed = TRUE), label = label)
    # every total, the inputs' included
    n_tot <- sum(vapply(src[sub("^t-14-", "t", sub("[.]rtf$", "", basename(files)))], function(s) length(.vn_numpages(s)), 0L))
    expect_identical(.vn_numpages(x), rep(as.integer(total), n_tot), label = label)
    # the pages: sections, page breaks and the contents' own breaks
    expect_identical(.vn_count(x, "\\\\pagebb"), max(tp - 1L, 0L), label = label)
    expect_identical(.vn_count(x, "\\\\sbkpage") + .vn_count(x, "\\\\page(?![a-zA-Z])") + .vn_count(x, "\\\\pagebb"),
                     as.integer(total), label = label)
    # the static numbers as the inputs wrote them; {AUTO_PAGE} still a field
    s <- src[sub("^t-14-", "t", sub("[.]rtf$", "", basename(files)))]
    expect_identical(.vn_static(x), unname(unlist(lapply(s, .vn_static))), label = label)
    expect_identical(.vn_count(x, "\\\\chpgn"), sum(vapply(s, .vn_count, 0L, "\\\\chpgn")), label = label)
  }
})

test_that("FT-AS127 [FR-IX383 FR-IX385]: decision table -- assemble_folder() numbers as assemble_rtf() does with the same contents", {
  d <- tempfile("vn-fold-")
  o <- tempfile("vn-fold-out-")
  dir.create(d)
  dir.create(o)
  on.exit(unlink(c(d, o), recursive = TRUE), add = TRUE)
  f1 <- .vn_input(file.path(d, "t-14-1.rtf"), "Table 14.1", 2L, TRUE)
  f2 <- .vn_input(file.path(d, "t-14-2.rtf"), "Table 14.2", 2L, FALSE)
  dt <- expand.grid(num = c("none", "roman", "decimal"), cover = c(FALSE, TRUE), stringsAsFactors = FALSE)
  for (i in seq_len(nrow(dt))) {
    r <- dt[i, ]
    label <- paste(r$num, r$cover, sep = " / ")
    cover <- if (r$cover) list(title = "Example Pharma STUDY-01") else NULL
    of <- file.path(o, sprintf("folder-%d.rtf", i))
    orr <- file.path(o, sprintf("rtf-%d.rtf", i))
    res <- assemble_folder(d, of, toc_page_numbering = r$num, cover = cover)
    assemble_rtf(toc = res$spec, output_file = orr, toc_page_numbering = r$num, cover = cover)
    x <- .vn_read(of)
    y <- .vn_read(orr)
    front <- switch(r$num, roman = 0L, decimal = 1L, none = r$cover + 1L)
    expect_identical(.vn_cached(x), as.integer(front + c(1L, 3L)), label = label)
    expect_identical(.vn_cached(x), .vn_cached(y), label = label)
    expect_identical(.vn_pgn(x), .vn_pgn(y), label = label)
    expect_identical(.vn_numpages(x), rep(4L + r$cover + 1L, 3L), label = label)
  }
})

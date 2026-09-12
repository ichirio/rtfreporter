# assemble_rtf.R -- concatenate multiple rtfreporter-generated RTFs.

.write_demo_rtf <- function(title) {
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1, secinfo = list(
    header = rtf_header(rows = list(
      c(l = "Protocol RTF-101", r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}")
    )),
    footer = rtf_footer(c(c = "Confidential"))
  ))
  doc <- rtf_tables(doc, list(
    data.frame(A = 1:2, B = c("x", "y"), stringsAsFactors = FALSE)
  ), titles = list(c(title)))
  f <- tempfile(fileext = ".rtf")
  generate_rtfreport(doc, f, overwrite = TRUE)
  f
}

test_that('assemble_rtf(toc = "auto") extracts titles, incl. non-ASCII (#60)', {
  f1 <- .write_demo_rtf("Table 14.1.1 Demographics")
  f2 <- .write_demo_rtf("Lab µg/mL summary")   # mu -> escaped \u181? in RTF
  out <- tempfile(fileext = ".rtf")
  on.exit(unlink(c(f1, f2, out)), add = TRUE)
  # Regression: this used to error (gsub() with a function replacement).
  expect_silent(assemble_rtf(c(f1, f2), out, toc = "auto", overwrite = TRUE))
  rtf <- paste(readLines(out, warn = FALSE), collapse = "\n")
  # The extracted non-ASCII title is re-escaped into the TOC entry.
  expect_true(grepl("\\u181?g/mL", rtf, fixed = TRUE))
})

test_that("assemble_rtf() concatenates 2 files into one document with \\sect breaks", {
  f1 <- .write_demo_rtf("Table 14.1.1")
  f2 <- .write_demo_rtf("Table 14.2.1")
  on.exit({ unlink(c(f1, f2)) }, add = TRUE)

  out <- tempfile(fileext = ".rtf")
  on.exit(unlink(out), add = TRUE)

  expect_invisible(assemble_rtf(c(f1, f2), out, overwrite = TRUE))
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 0L)

  lines <- readLines(out, warn = FALSE)
  joined <- paste(lines, collapse = "\n")
  # Both source titles appear in the assembled output
  expect_match(joined, "Table 14\\.1\\.1")
  expect_match(joined, "Table 14\\.2\\.1")
  # Exactly one inter-section break (between the two source files)
  expect_identical(sum(grepl("^\\\\sect$", lines)), 1L)
  # Document still ends with a single closing "}"
  expect_identical(trimws(tail(lines, 1)), "}")
})

test_that("assemble_rtf() concatenates more than 2 files", {
  fs <- vapply(1:3, function(i) .write_demo_rtf(sprintf("T %d", i)),
               character(1L))
  on.exit(unlink(fs), add = TRUE)
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  assemble_rtf(fs, out, overwrite = TRUE)
  lines <- readLines(out, warn = FALSE)
  # N - 1 section breaks between N source files
  expect_identical(sum(grepl("^\\\\sect$", lines)), 2L)
})

test_that("assemble_rtf() requires at least 2 input files", {
  expect_error(assemble_rtf(character(0), tempfile()),
               "at least 2")
  expect_error(assemble_rtf("only_one.rtf", tempfile()),
               "at least 2")
})

test_that("assemble_rtf() errors if any input file does not exist", {
  f1 <- .write_demo_rtf("X"); on.exit(unlink(f1), add = TRUE)
  expect_error(
    assemble_rtf(c(f1, "/no/such/file.rtf"), tempfile()),
    "not found")
})

test_that("assemble_rtf() refuses to overwrite without overwrite = TRUE", {
  f1 <- .write_demo_rtf("A"); f2 <- .write_demo_rtf("B")
  on.exit(unlink(c(f1, f2)), add = TRUE)
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  assemble_rtf(c(f1, f2), out, overwrite = TRUE)
  expect_error(assemble_rtf(c(f1, f2), out), "already exists")
})

test_that("assemble_rtf() refuses non-rtfreporter RTFs (missing \\sectd)", {
  fake <- tempfile(fileext = ".rtf"); on.exit(unlink(fake), add = TRUE)
  writeLines(c("{\\rtf1\\ansi", "Hello world.", "}"), fake)
  f1  <- .write_demo_rtf("Real");  on.exit(unlink(f1), add = TRUE)
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  expect_error(assemble_rtf(c(f1, fake), out, overwrite = TRUE),
               "sectd")
})

# ──────── TOC + bookmarks (v0.0.29+) ──────────────────────────────────────

test_that("assemble_rtf(toc = ...) inserts a TOC page with HYPERLINK + PAGEREF fields", {
  f1 <- .write_demo_rtf("Table 14.1.1")
  f2 <- .write_demo_rtf("Table 14.2.1")
  on.exit(unlink(c(f1, f2)), add = TRUE)

  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  assemble_rtf(c(f1, f2), out,
               toc        = c("T14.1.1 Demographics", "T14.2.1 AE Summary"),
               overwrite  = TRUE)

  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")

  # TOC title and BOTH labels appear, plus an extra \sect for the TOC page
  expect_match(txt, "Table of Contents")
  expect_match(txt, "T14\\.1\\.1 Demographics")
  expect_match(txt, "T14\\.2\\.1 AE Summary")

  # Field codes present: HYPERLINK + PAGEREF (escaped backslashes in regex)
  expect_match(txt, "HYPERLINK")
  expect_match(txt, "PAGEREF")

  # Bookmarks inserted for both source files
  expect_match(txt, "bkmkstart tfl_")
  expect_match(txt, "bkmkend tfl_")

  # 2 bookmarkstart entries — one per source file
  expect_identical(length(gregexpr("bkmkstart tfl_", txt)[[1L]]), 2L)
})

test_that("assemble_rtf(toc = NULL) is byte-identical to the legacy behaviour", {
  f1 <- .write_demo_rtf("A"); f2 <- .write_demo_rtf("B")
  on.exit(unlink(c(f1, f2)), add = TRUE)

  out_legacy <- tempfile(fileext = ".rtf"); on.exit(unlink(out_legacy), add = TRUE)
  out_null   <- tempfile(fileext = ".rtf"); on.exit(unlink(out_null),   add = TRUE)
  assemble_rtf(c(f1, f2), out_legacy, overwrite = TRUE)
  assemble_rtf(c(f1, f2), out_null,   toc = NULL, overwrite = TRUE)

  expect_identical(readLines(out_legacy, warn = FALSE),
                   readLines(out_null,   warn = FALSE))
})

test_that("assemble_rtf(toc) length must match input_files length", {
  f1 <- .write_demo_rtf("X"); f2 <- .write_demo_rtf("Y")
  on.exit(unlink(c(f1, f2)), add = TRUE)
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  expect_error(
    assemble_rtf(c(f1, f2), out, toc = "only one entry", overwrite = TRUE),
    "same length"
  )
})

test_that("bookmark name sanitiser strips .rtf and replaces invalid chars", {
  s <- rtfreporter:::.sanitize_bookmark(
    c("table 14.1.1.rtf", "subjects-screened.rtf",
      "1starts-with-digit.rtf", "T_2.rtf")
  )
  # No spaces, dashes, or dots; first char is a letter
  expect_false(any(grepl("[ \\-\\.]", s)))
  expect_true(all(grepl("^[A-Za-z]", s)))
  # Length cap
  long <- paste0(strrep("x", 60), ".rtf")
  expect_lte(nchar(rtfreporter:::.sanitize_bookmark(long)), 32L)
})

test_that("duplicate bookmark names get suffixed (.1, .2 ...)", {
  # Two files whose basenames sanitise to the same bookmark
  d1 <- tempfile(); dir.create(d1); on.exit(unlink(d1, recursive = TRUE), add = TRUE)
  d2 <- tempfile(); dir.create(d2); on.exit(unlink(d2, recursive = TRUE), add = TRUE)
  f1 <- file.path(d1, "x.rtf"); f2 <- file.path(d2, "x.rtf")
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1, secinfo = list(header = NULL, footer = NULL))
  doc <- rtf_tables(doc, list(data.frame(a = 1L)))
  generate_rtfreport(doc, f1, overwrite = TRUE)
  generate_rtfreport(doc, f2, overwrite = TRUE)

  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  assemble_rtf(c(f1, f2), out,
               toc       = c("entry A", "entry B"),
               overwrite = TRUE)
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  # Both bookmarks present and distinct (suffixed)
  expect_match(txt, "tfl_x_1")
  expect_match(txt, "tfl_x_2")
})

test_that("toc_leader = 'none' suppresses the dot leader", {
  f1 <- .write_demo_rtf("X"); f2 <- .write_demo_rtf("Y")
  on.exit(unlink(c(f1, f2)), add = TRUE)
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  assemble_rtf(c(f1, f2), out,
               toc        = c("A", "B"),
               toc_leader = "none",
               overwrite  = TRUE)
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_false(grepl("\\\\tldot", txt))
})

test_that("custom bookmark_prefix is honoured", {
  f1 <- .write_demo_rtf("X"); f2 <- .write_demo_rtf("Y")
  on.exit(unlink(c(f1, f2)), add = TRUE)
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  assemble_rtf(c(f1, f2), out,
               toc             = c("A", "B"),
               bookmark_prefix = "studyX_",
               overwrite       = TRUE)
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(txt, "bkmkstart studyX_")
  expect_false(grepl("bkmkstart tfl_", txt))
})

# ──────── toc = "auto" — auto-extract titles from input RTFs ──────────────

test_that("toc = 'auto' extracts the first centred-bold title of each input", {
  f1 <- .write_demo_rtf("My Auto Title 14.1.1")
  f2 <- .write_demo_rtf("Adverse Events Summary")
  on.exit(unlink(c(f1, f2)), add = TRUE)

  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  assemble_rtf(c(f1, f2), out, toc = "auto", overwrite = TRUE)
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(txt, "My Auto Title 14\\.1\\.1")
  expect_match(txt, "Adverse Events Summary")
})

test_that("toc = 'auto' falls back to basename when no title is detected", {
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1, secinfo = list(header = NULL, footer = NULL))
  doc <- rtf_tables(doc, list(data.frame(a = 1L)),
                    titles = list(character(0)))      # suppress the title
  f <- tempfile(pattern = "no_title_here", fileext = ".rtf")
  generate_rtfreport(doc, f, overwrite = TRUE)
  f2 <- .write_demo_rtf("Real Title")
  on.exit(unlink(c(f, f2)), add = TRUE)

  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  assemble_rtf(c(f, f2), out, toc = "auto", overwrite = TRUE)
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  # First entry is basename-derived (file 1 has no title)
  expect_match(txt, "no_title_here")
  expect_match(txt, "Real Title")
})

# ──────── Structured TOC: toc_heading() + toc_entry() ─────────────────────

test_that("toc_heading() and toc_entry() build the expected S3 objects", {
  h <- toc_heading("EFFICACY", level = 1)
  expect_s3_class(h, "rtf_toc_heading")
  expect_identical(h$label, "EFFICACY")
  expect_identical(h$level, 1L)

  e <- toc_entry("Table 14.1.1", file = "t14_1_1.rtf", level = 2)
  expect_s3_class(e, "rtf_toc_entry")
  expect_identical(e$label, "Table 14.1.1")
  expect_identical(e$file,  "t14_1_1.rtf")
  expect_identical(e$level, 2L)
})

test_that("structured toc list assigns entries to files in declaration order", {
  f1 <- .write_demo_rtf("A"); f2 <- .write_demo_rtf("B"); f3 <- .write_demo_rtf("C")
  on.exit(unlink(c(f1, f2, f3)), add = TRUE)

  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  assemble_rtf(
    c(f1, f2, f3), out,
    toc = list(
      toc_heading("EFFICACY ANALYSES"),
      toc_entry("Table 14.1.1 Demographics"),       # auto-bound to f1
      toc_heading("SAFETY ANALYSES"),
      toc_entry("Table 14.2.1 Adverse Events"),     # auto-bound to f2
      toc_entry("Listing 16.1 Disposition")         # auto-bound to f3
    ),
    overwrite = TRUE
  )
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(txt, "EFFICACY ANALYSES")
  expect_match(txt, "SAFETY ANALYSES")
  expect_match(txt, "Demographics")
  expect_match(txt, "Adverse Events")
  expect_match(txt, "Disposition")
  # 3 bookmarks (one per source file)
  expect_identical(length(gregexpr("bkmkstart tfl_", txt)[[1L]]), 3L)
})

test_that("toc_entry(file = ...) supports both path and integer index", {
  f1 <- .write_demo_rtf("X"); f2 <- .write_demo_rtf("Y")
  on.exit(unlink(c(f1, f2)), add = TRUE)
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  expect_invisible(
    assemble_rtf(c(f1, f2), out,
                 toc = list(
                   toc_entry("Entry A", file = f1),
                   toc_entry("Entry B", file = 2L)
                 ),
                 overwrite = TRUE)
  )
})

test_that("toc_entry(file) referring to a missing input file errors clearly", {
  f1 <- .write_demo_rtf("X"); f2 <- .write_demo_rtf("Y")
  on.exit(unlink(c(f1, f2)), add = TRUE)
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  expect_error(
    assemble_rtf(c(f1, f2), out,
                 toc = list(toc_entry("X", file = "ghost.rtf")),
                 overwrite = TRUE),
    "not in"
  )
})

# ──────── Page-numbering modes ─────────────────────────────────────────────

test_that("toc_page_numbering = 'roman' inserts \\pgnlcrm and restarts body", {
  f1 <- .write_demo_rtf("X"); f2 <- .write_demo_rtf("Y")
  on.exit(unlink(c(f1, f2)), add = TRUE)
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  assemble_rtf(c(f1, f2), out,
               toc                = c("A", "B"),
               toc_page_numbering = "roman",
               overwrite          = TRUE)
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(txt, "\\\\pgnlcrm")
  expect_match(txt, "\\\\pgnrestart\\\\pgndec")    # body restart
})

test_that("toc_page_numbering = 'none' (default) omits both restart and pgnlcrm", {
  f1 <- .write_demo_rtf("X"); f2 <- .write_demo_rtf("Y")
  on.exit(unlink(c(f1, f2)), add = TRUE)
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  assemble_rtf(c(f1, f2), out, toc = c("A", "B"), overwrite = TRUE)
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_false(grepl("\\\\pgnlcrm",            txt))
  expect_false(grepl("\\\\pgnrestart\\\\pgndec", txt))
})

# ──────── Cover page ──────────────────────────────────────────────────────

test_that("cover = list(...) inserts a cover section before the TOC", {
  f1 <- .write_demo_rtf("X"); f2 <- .write_demo_rtf("Y")
  on.exit(unlink(c(f1, f2)), add = TRUE)
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  assemble_rtf(
    c(f1, f2), out,
    cover = list(
      title    = "Study XYZ-001",
      subtitle = "Final Statistical Report",
      date     = "2026-05-29",
      version  = "v1.0",
      meta     = c("Confidential", "Prepared by ACME Pharma")
    ),
    toc       = c("A", "B"),
    overwrite = TRUE
  )
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(txt, "Study XYZ-001")
  expect_match(txt, "Final Statistical Report")
  expect_match(txt, "2026-05-29")
  expect_match(txt, "v1\\.0")
  expect_match(txt, "Confidential")
  expect_match(txt, "Prepared by ACME Pharma")
})

test_that("cover can be used without a TOC", {
  f1 <- .write_demo_rtf("X"); f2 <- .write_demo_rtf("Y")
  on.exit(unlink(c(f1, f2)), add = TRUE)
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  assemble_rtf(
    c(f1, f2), out,
    cover     = list(title = "Cover Only Test"),
    overwrite = TRUE
  )
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(txt, "Cover Only Test")
  # No TOC means no HYPERLINK fields
  expect_false(grepl("HYPERLINK", txt))
})

test_that("cover fields that are NULL / empty are silently skipped", {
  f1 <- .write_demo_rtf("X"); f2 <- .write_demo_rtf("Y")
  on.exit(unlink(c(f1, f2)), add = TRUE)
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  expect_invisible(
    assemble_rtf(c(f1, f2), out,
                 cover     = list(title = "Only Title"),
                 overwrite = TRUE)
  )
})

# ──────── Backward compat: toc = NULL still byte-identical ────────────────

test_that("toc = NULL + cover = NULL is byte-identical to the legacy behaviour", {
  f1 <- .write_demo_rtf("A"); f2 <- .write_demo_rtf("B")
  on.exit(unlink(c(f1, f2)), add = TRUE)
  out_old <- tempfile(fileext = ".rtf"); on.exit(unlink(out_old), add = TRUE)
  out_new <- tempfile(fileext = ".rtf"); on.exit(unlink(out_new), add = TRUE)
  assemble_rtf(c(f1, f2), out_old, overwrite = TRUE)
  assemble_rtf(c(f1, f2), out_new,
               toc = NULL, cover = NULL,
               toc_page_numbering = "none",
               overwrite = TRUE)
  expect_identical(readLines(out_old, warn = FALSE),
                   readLines(out_new, warn = FALSE))
})

# ──────── Static {PAGE} / {TOTAL_PAGES} preservation ──────────────────────

# Same shape as .write_demo_rtf() but uses the STATIC tokens that are
# baked in at render time (per-section page number / per-file total).
.write_static_pageno_rtf <- function(title, n_pages = 2L) {
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1, secinfo = list(
    header = rtf_header(rows = list(
      c(l = "Protocol RTF-101",
        r = "Page {PAGE} of {TOTAL_PAGES}")    # <- static, not AUTO
    )),
    footer = rtf_footer(c(c = "Confidential"))
  ))
  # n_pages tables -> n_pages output pages
  tbls <- replicate(n_pages,
                    data.frame(A = 1:2, B = c("x", "y"),
                                stringsAsFactors = FALSE),
                    simplify = FALSE)
  doc <- rtf_tables(doc, tbls,
                    titles = lapply(seq_along(tbls),
                                     function(i) c(sprintf("%s p%d", title, i))))
  f <- tempfile(fileext = ".rtf")
  generate_rtfreport(doc, f, overwrite = TRUE)
  f
}

test_that("static {PAGE}/{TOTAL_PAGES} both bake into source AS-IS and survive assembly", {
  # As of v0.0.32, both {PAGE} and {TOTAL_PAGES} are STATIC integers
  # baked in at render time.  Per-file totals freeze at the source
  # file's own page count and are wrong (but stable) after assembly.
  # This is the documented "static tokens reflect only the source file"
  # behaviour.
  f1 <- .write_static_pageno_rtf("FileA", n_pages = 2L)
  f2 <- .write_static_pageno_rtf("FileB", n_pages = 3L)
  on.exit(unlink(c(f1, f2)), add = TRUE)

  txt1 <- paste(readLines(f1, warn = FALSE), collapse = "\n")
  txt2 <- paste(readLines(f2, warn = FALSE), collapse = "\n")
  # Each source file has BOTH the section's first-page number AND the
  # document total baked in as literal integers.
  expect_match(txt1, "Page 1 of 2")
  expect_match(txt2, "Page 1 of 3")
  # No dynamic field codes for these STATIC tokens.
  expect_false(grepl("\\\\chpgn", txt1))

  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  assemble_rtf(c(f1, f2), out, overwrite = TRUE)
  joined <- paste(readLines(out, warn = FALSE), collapse = "\n")

  # Both per-file static literals stay verbatim in the assembled output.
  expect_match(joined, "Page 1 of 2")
  expect_match(joined, "Page 1 of 3")
})

test_that("static {PAGE}/{TOTAL_PAGES} numbers survive when cover / toc are added", {
  f1 <- .write_static_pageno_rtf("X", n_pages = 2L)
  f2 <- .write_static_pageno_rtf("Y", n_pages = 4L)
  on.exit(unlink(c(f1, f2)), add = TRUE)

  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  assemble_rtf(c(f1, f2), out,
               cover = list(title = "Demo"),
               toc   = c("Entry A", "Entry B"),
               toc_page_numbering = "roman",
               overwrite = TRUE)
  joined <- paste(readLines(out, warn = FALSE), collapse = "\n")

  # The static "Page 1 of 2" / "Page 1 of 4" baked into the source
  # files is still verbatim in the assembled output — even though we
  # added a cover page, a TOC, and Roman page numbering on top.
  expect_match(joined, "Page 1 of 2")
  expect_match(joined, "Page 1 of 4")
})

# ──────── PDF outline markers (\outlinelevel) for the bookmark panel ──────

test_that("toc-enabled assembly emits \\outlinelevel paragraphs (PDF outline panel)", {
  f1 <- .write_demo_rtf("X"); f2 <- .write_demo_rtf("Y")
  on.exit(unlink(c(f1, f2)), add = TRUE)
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  assemble_rtf(c(f1, f2), out,
               toc       = c("Table A", "Table B"),
               overwrite = TRUE)
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(txt, "\\\\outlinelevel0")
  # One outline paragraph per source file when toc is given
  expect_identical(length(gregexpr("\\\\outlinelevel0", txt)[[1L]]), 2L)
  expect_match(txt, "Table A")
  expect_match(txt, "Table B")
})

test_that("outline labels fall back to basename when a file has no toc_entry", {
  f1 <- .write_demo_rtf("X"); f2 <- .write_demo_rtf("Y")
  # Rename so we can spot the basename in the output
  f2b <- file.path(dirname(f2), "fallback_label_42.rtf")
  file.rename(f2, f2b)
  on.exit(unlink(c(f1, f2b)), add = TRUE)
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  assemble_rtf(c(f1, f2b), out,
               toc = list(toc_entry("Only entry for first", file = f1)),
               overwrite = TRUE)
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(txt, "Only entry for first")
  expect_match(txt, "fallback_label_42")
})

test_that("toc = NULL emits no \\outlinelevel paragraphs (clean legacy output)", {
  f1 <- .write_demo_rtf("X"); f2 <- .write_demo_rtf("Y")
  on.exit(unlink(c(f1, f2)), add = TRUE)
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  assemble_rtf(c(f1, f2), out, overwrite = TRUE)
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_false(grepl("\\\\outlinelevel", txt))
})

test_that("AUTO_PAGE source files keep their dynamic field codes after assembly", {
  # The existing .write_demo_rtf() uses {AUTO_PAGE} / {AUTO_TOTAL_PAGES}.
  # Verify both field codes are still present in the assembled output so
  # the viewer can recompute them.
  f1 <- .write_demo_rtf("Auto A"); f2 <- .write_demo_rtf("Auto B")
  on.exit(unlink(c(f1, f2)), add = TRUE)
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  assemble_rtf(c(f1, f2), out, overwrite = TRUE)
  joined <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(joined, "\\\\chpgn")               # \chpgn for AUTO_PAGE
  expect_match(joined, "NUMPAGES")                # NUMPAGES field
})

test_that("TOC label characters get RTF-escaped (backslash, braces, unicode)", {
  f1 <- .write_demo_rtf("X"); f2 <- .write_demo_rtf("Y")
  on.exit(unlink(c(f1, f2)), add = TRUE)
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  assemble_rtf(c(f1, f2), out,
               toc       = c("A {special} \\char",
                              "B with greek α"),
               overwrite = TRUE)
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  # Brace escapes
  expect_match(txt, "\\\\\\{special\\\\\\}")
  # Backslash escape
  expect_match(txt, "\\\\\\\\char")
  # Unicode -> RTF \uN?
  expect_match(txt, "\\\\u945\\?")
})

# ──────────────────────────────────────────────────────────────────────────
#  v0.0.33 regression: outline-paragraph + cover-paragraph font-size leak
# ──────────────────────────────────────────────────────────────────────────
#
# Pre-v0.0.33 `.insert_bookmark()` emitted
#   \pard\plain\fs2\sa0\sb0\outlinelevel0 LABEL\par
# and `.build_cover_section()` / `.build_toc_section()` emitted
#   \pard\qc\b\fs44 TEXT\b0\fs0\par
# Neither group was wrapped in `{ ... }`, so the character-format state
# (\fs2 from the outline paragraph, \fs0 from the cover/TOC closings)
# bled across `\par` into the following body-table content, making the
# cell text effectively invisible while the table borders still rendered.
#
# The fix wraps every formatted paragraph in `{ ... }` so the format
# state is local.  Below we lock that down structurally.

test_that("outline paragraph uses \\cf2\\fs2\\sl1\\slmult0 in a balanced group", {
  f1 <- .write_demo_rtf("Body 1")
  f2 <- .write_demo_rtf("Body 2")
  on.exit(unlink(c(f1, f2)), add = TRUE)
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  assemble_rtf(c(f1, f2), out,
               toc       = c("Entry A", "Entry B"),
               overwrite = TRUE)
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  # v0.0.37: outline label uses
  #   \cf2          -- white text colour (invisible)
  #   \fs2          -- 1-pt fallback size
  #   \sl1\slmult0  -- exact line spacing 1 twip (~0 px), guarantees
  #                    no layout shift even if a converter ignores \fs2
  # inside a balanced { ... } group so format state cannot leak.
  expect_match(txt,
               "\\{\\\\pard\\\\plain\\\\cf2\\\\fs2\\\\sl1\\\\slmult0\\\\sa0\\\\sb0\\\\outlinelevel0[^\\}]*\\\\par\\}")
  # No `}` after \par would mean format state leaks.
  expect_false(
    grepl("\\\\cf2\\\\fs2\\\\sl1\\\\slmult0[^\\}]*\\\\par(?!\\})",
          txt, perl = TRUE)
  )
  # Colour-table must contain white at index 2.
  expect_match(txt,
               "\\\\colortbl;\\\\red0\\\\green0\\\\blue0;\\\\red255\\\\green255\\\\blue255;")
})

test_that("outline paragraph is inserted AFTER section preamble (no border bleed)", {
  # Regression for the "phantom rule under header on AE first page"
  # bug.  Pre-v0.0.37, the outline paragraph was placed BETWEEN
  # `\sectd` and the section properties (\sbkpage, {\header},
  # {\footer}) -- so it was rendered in a half-configured section,
  # picking up the previous section's lingering paragraph state.
  # The fix moves the insertion point to AFTER all section preamble.
  f1 <- .write_demo_rtf("Body 1")
  f2 <- .write_demo_rtf("Body 2")
  on.exit(unlink(c(f1, f2)), add = TRUE)
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  assemble_rtf(c(f1, f2), out,
               toc       = c("A", "B"),
               overwrite = TRUE)
  lines <- readLines(out, warn = FALSE)

  # For the SECOND source file (file index >= 2 in the assembled
  # output) the order must be: \sectd, \sbkpage..., {\header...},
  # {\footer...}, bookmark, outline paragraph.  We check that the
  # bookmark line follows a {\footer ...} line (rtfreporter always
  # emits a footer in `.write_demo_rtf`).
  bm_idx <- grep("\\\\bkmkstart tfl_", lines)
  expect_true(length(bm_idx) >= 2L)
  # The line immediately preceding each bookmark insertion must be
  # the footer wrapper (or the header wrapper if no footer is set).
  for (j in bm_idx) {
    prev <- trimws(lines[j - 1L])
    expect_true(
      grepl("^\\{\\\\footer\\b", prev) || grepl("^\\{\\\\header\\b", prev),
      info = sprintf("bookmark at line %d should follow {\\footer} or {\\header}; got: %s",
                     j, prev)
    )
  }
})

test_that("cover paragraphs do not emit bare \\fs0 (leak source)", {
  f1 <- .write_demo_rtf("Body 1")
  f2 <- .write_demo_rtf("Body 2")
  on.exit(unlink(c(f1, f2)), add = TRUE)
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  assemble_rtf(c(f1, f2), out,
               cover = list(title    = "Study XYZ",
                            subtitle = "Subtitle line",
                            date     = "2026-05-29",
                            version  = "v1.0",
                            meta     = c("Confidential", "Sponsor only")),
               toc       = c("Entry A", "Entry B"),
               overwrite = TRUE)
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  # Old-style "\fs0\par" closings must not appear -- they were the
  # leak source that drove body font size to zero.
  expect_false(grepl("\\\\fs0\\\\par", txt))
  # Cover title must still be rendered with its 22-pt font.
  expect_match(txt, "\\\\fs44\\s+Study XYZ")
})

test_that("body table fs values survive cover+TOC prefix (no leak)", {
  # Direct end-to-end check: the body table content from each source
  # must keep its own font sizing intact after assembly.  We just
  # confirm the body's table-cell content from rtfreporter's table
  # renderer still appears after the cover + TOC prefix has been
  # inserted.
  f1 <- .write_demo_rtf("Body 1")
  f2 <- .write_demo_rtf("Body 2")
  on.exit(unlink(c(f1, f2)), add = TRUE)
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  assemble_rtf(c(f1, f2), out,
               cover = list(title = "Cover Title"),
               toc       = c("Entry A", "Entry B"),
               overwrite = TRUE)
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  # Body cell content (the literal "Body 1" / "Body 2" titles) must
  # still appear after the cover + TOC prefix.
  expect_match(txt, "Body 1")
  expect_match(txt, "Body 2")
  # Sanity: the cover-style "\fs0\par" closing must not appear (it was
  # the original leak source -- v0.0.33 wrapped cover paragraphs in
  # groups instead).  The outline paragraph's `\fs0\sa0\sb0...\par}`
  # is allowed (and necessary) because it's already wrapped in a
  # `{ ... }` group; we already test that structure above.
  expect_false(grepl("\\\\fs0\\\\par", txt))
})

test_that(".count_rtf_pages counts one page per section break", {
  f <- .write_demo_rtf("One page")
  expect_equal(rtfreporter:::.count_rtf_pages(readLines(f, warn = FALSE)), 1L)
})

# One `rtf_section` holding several rendered pages.  `cuts` are row positions
# to break before, so the table comes out as length(cuts) + 1 pages.
.write_multipage_rtf <- function(title, cuts) {
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1, secinfo = list(
    header = rtf_header(rows = list(
      c(l = "Protocol RTF-101", r = "Page {AUTO_PAGE}")
    ))
  ))
  df <- data.frame(A = 1:60, B = sprintf("r%02d", 1:60), stringsAsFactors = FALSE)
  pages <- if (is.null(cuts)) {
    as_rtftables(df)
  } else {
    as_rtftables(df, split = "rows", split_rows = cuts)
  }
  doc <- rtf_tables(doc, pages, titles = list(c(title)))
  f <- tempfile(fileext = ".rtf")
  generate_rtfreport(doc, f, overwrite = TRUE)
  f
}

test_that(".count_rtf_pages counts in-section page breaks too (#401)", {
  # A section is emitted per `rtf_section`, not per page, so counting
  # `\sbkpage` alone reported 1 for every one of these.
  for (cuts in list(NULL, 20L, c(20L, 40L), c(10L, 20L, 30L, 40L, 50L))) {
    f     <- .write_multipage_rtf("Multi", cuts)
    lines <- readLines(f, warn = FALSE)
    expect_equal(rtfreporter:::.count_rtf_pages(lines),
                 length(cuts) + 1L,
                 info = paste("cuts =", paste(cuts, collapse = ",")))
  }
})

test_that(".count_rtf_pages does not mistake other \\page... words for a break", {
  # `\pagebb` is a paragraph property, not a page break; `\sbkpage` must not
  # be double-counted by the `\page` pattern either.
  expect_equal(rtfreporter:::.count_rtf_pages(
    c("\\sectd\\sbkpage", "\\pard\\pagebb hello\\par")), 1L)
  expect_equal(rtfreporter:::.count_rtf_pages(character(0)), 0L)
})

test_that("assemble_rtf TOC caches cumulative page numbers (decimal = continuous)", {
  f1 <- .write_demo_rtf("First")
  f2 <- .write_demo_rtf("Second")
  out <- tempfile(fileext = ".rtf")
  assemble_rtf(c(f1, f2), out, overwrite = TRUE,
    toc = list(toc_entry("First",  file = f1, level = 1),
               toc_entry("Second", file = f2, level = 1)),
    toc_page_numbering = "decimal")
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  # Each TOC entry caches its page number in the PAGEREF field's result.
  hits  <- regmatches(txt, gregexpr("PAGEREF.{1,60}?fldrslt [0-9]+", txt))[[1L]]
  pages <- as.integer(sub(".*fldrslt ([0-9]+)$", "\\1", hits))
  # TOC is page 1; the two 1-page tables start on pages 2 and 3.
  expect_equal(pages, c(2L, 3L))
})

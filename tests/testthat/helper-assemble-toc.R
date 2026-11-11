# helper-assemble-toc.R -- small inputs and fixed cases for the TOC tests
# (test-assemble-toc-title.R).  The file names are fixed, because a TOC
# bookmark is made from the file name; the data are fictional.

# A fresh folder for a test's input files (the test removes it)
.tt_dir <- function() {
  d <- tempfile("tt")
  dir.create(d)
  d
}

# One input RTF, a table titled `title`, in `dir`; `pages` > 1 adds that many
# page breaks to it (so it counts as that many pages).
.tt_input <- function(dir, name, title = NULL, pages = 1L, rows = 2L,
                      tokens = NULL, font = NULL) {
  f <- file.path(dir, name)
  # (the font option is read when the document is made)
  old <- options(rtfreporter.font = font %||% getOption("rtfreporter.font"))
  on.exit(options(old))
  doc <- rtf_document(tokens = tokens)
  doc <- rtf_tables(doc, data.frame(A = seq_len(rows), B = "x"),
                    titles = if (!is.null(title)) list(c(title)))
  generate_rtfreport(doc, f, overwrite = TRUE)
  if (pages > 1L) {
    x <- readLines(f, warn = FALSE)
    n <- length(x)
    x <- c(x[-n], rep("\\page", pages - 1L), x[n])
    writeLines(x, f, useBytes = TRUE)
  }
  f
}

# Inputs from a named character vector: names are the files, values the titles
.tt_inputs <- function(dir, titles, pages = 1L, ...) {
  pages <- rep_len(pages, length(titles))
  vapply(seq_along(titles), function(i) {
    .tt_input(dir, paste0(names(titles)[i], ".rtf"), titles[[i]], pages[i], ...)
  }, character(1L))
}

# assemble_rtf() of `files`, the assembled file's lines
.tt_asm <- function(files, ..., asm = assemble_rtf) {
  out <- file.path(dirname(files[1L]), "assembled.rtf")
  asm(files, out, overwrite = TRUE, ...)
  readLines(out, warn = FALSE)
}

# The paragraphs the TOC (and a cover) write: `{\pard ... \par}` lines, but
# not the invisible outline paragraphs.
.tt_paras <- function(lines) {
  p <- grep("^\\{\\\\pard", lines, value = TRUE)
  p[!grepl("^\\{\\\\pard\\\\plain", p)]
}

# The TOC's own paragraphs: the title, the blank line, then headings / entries
# (`cover = TRUE`: the cover's paragraphs come first, up to its `\sect`).
.tt_toc <- function(lines, cover = FALSE) {
  if (cover) {
    sect <- which(lines == "\\sect")
    lines <- lines[(sect[1L] + 1L):sect[2L]]
  }
  .tt_paras(lines)
}

# The entries: the paragraphs with a PAGEREF
.tt_entries <- function(lines) {
  grep("PAGEREF", .tt_paras(lines), value = TRUE)
}

# The cached page numbers of the entries, in order
.tt_pages <- function(lines) {
  e <- .tt_entries(lines)
  as.integer(sub(".*PAGEREF [^ ]+ \\\\\\\\h\\}\\{\\\\fldrslt ([0-9]+)\\}\\}.*$",
                 "\\1", e))
}

# The label of each entry (the HYPERLINK's result)
.tt_labels <- function(lines) {
  e <- .tt_entries(lines)
  sub(".*\\{\\\\fldrslt ((?:[^{}\\\\]|\\\\.)*)\\}\\}\\\\tab.*$", "\\1", e, perl = TRUE)
}

# The text of each heading paragraph (bold 11 pt, no PAGEREF)
.tt_headings <- function(lines) {
  p <- .tt_paras(lines)
  h <- grep("\\\\b\\\\fs22 ", p, value = TRUE)
  sub("^.*\\\\fs22 (.*)\\\\par\\}$", "\\1", h)
}

# bytes of a file with the CR removed
.tt_bytes <- function(path) {
  x <- readBin(path, "raw", file.size(path))
  x[x != as.raw(13L)]
}

# -- the fixed cases (fixtures/toc-<case>.rtf are what they write) ------------

.tt_cover <- list(title = "Study XYZ-101", subtitle = "Final Analysis",
                  date = "2026-11-12", version = "v1.0",
                  meta = c("Confidential", "Prepared by Example Pharma"))

# defaults, a cover, a table with headings, roman TOC pages (the fixture is
# written by the code before toc_title / toc_style / label-from-title existed)
.tt_case_baseline <- function(dir, asm = assemble_rtf) {
  f <- .tt_inputs(dir, c(t_14_1_1 = "Table 14.1.1 Demographics",
                         t_14_2_1 = "Table 14.2.1 Adverse Events",
                         l_16_1_1 = "Listing 16.1.1 Adverse events"))
  toc <- data.frame(file = f, heading = c("Efficacy", "Safety", "Safety"),
                    label = c("Table 14.1.1 Demographics",
                              "Table 14.2.1 Adverse Events",
                              "Listing 16.1.1 Adverse events"),
                    level = c(2L, 2L, 3L))
  out <- file.path(dir, "assembled.rtf")
  asm(f, out, cover = .tt_cover, toc = toc, toc_page_numbering = "roman")
  out
}

# page numbers crossing a digit boundary: entries start on pages 2, 9, 10, 98,
# 99 and 100
.tt_case_digits <- function(dir) {
  f <- .tt_inputs(dir,
    c(t_14_1_1 = "Table 14.1.1 Demographics", t_14_1_2 = "Table 14.1.2 Disposition",
      t_14_2_1 = "Table 14.2.1 Adverse Events", t_14_2_2 = "Table 14.2.2 Laboratory",
      t_14_3_1 = "Table 14.3.1 Vital Signs", t_14_3_2 = "Table 14.3.2 ECG"),
    pages = c(7L, 1L, 88L, 1L, 1L, 1L))
  out <- file.path(dir, "assembled.rtf")
  assemble_rtf(f, out, toc = "auto")
  out
}

# a label that wraps (level 2 and level 3), the right tab and the leader on it
.tt_long_label <- function() {
  paste("Table 14.3.2 Summary of treatment-emergent adverse events by system",
        "organ class, preferred term and maximum severity in the safety",
        "population, all randomised subjects who received at least one dose",
        "of study treatment, excluding those from site 99")
}
.tt_case_longlabel <- function(dir) {
  f <- .tt_inputs(dir, c(t_14_1_1 = "Table 14.1.1 Demographics",
                         t_14_3_2 = "Table 14.3.2 Adverse Events",
                         t_14_3_3 = "Table 14.3.3 Laboratory"))
  toc <- data.frame(file = f, label = c("Table 14.1.1 Demographics",
                                        .tt_long_label(),
                                        .tt_long_label()),
                    level = c(2L, 2L, 3L))
  out <- file.path(dir, "assembled.rtf")
  assemble_rtf(f, out, toc = toc)
  out
}

# a TOC of two pages: a heading that would close page 1 goes over to page 2;
# roman TOC numbering
.tt_case_multipage <- function(dir) {
  f <- .tt_inputs(dir, c(t_14_1_1 = "Table 14.1.1 Demographics",
                         t_14_2_1 = "Table 14.2.1 Adverse Events",
                         t_14_3_1 = "Table 14.3.1 Laboratory"))
  n <- 40L
  toc <- data.frame(file = f[rep_len(1:3, n)],
                    label = sprintf("Table 14.%d.1 Summary %d", seq_len(n), seq_len(n)),
                    heading = c(rep(NA, 34L), "Safety", rep(NA, n - 35L)),
                    level = 2L)
  out <- file.path(dir, "assembled.rtf")
  assemble_rtf(f, out, toc = toc, toc_page_numbering = "roman")
  out
}

# a cover, then the TOC, then the body; roman TOC numbering
.tt_case_coverroman <- function(dir) {
  f <- .tt_inputs(dir, c(t_14_1_1 = "Table 14.1.1 Demographics",
                         t_14_2_1 = "Table 14.2.1 Adverse Events",
                         t_14_3_1 = "Table 14.3.1 Laboratory"),
                  pages = c(2L, 1L, 3L))
  out <- file.path(dir, "assembled.rtf")
  assemble_rtf(f, out, cover = .tt_cover, toc = "auto",
               toc_page_numbering = "roman")
  out
}

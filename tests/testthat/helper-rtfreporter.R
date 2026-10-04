# Shared helpers for the testthat suite.

# Build a tiny end-to-end document with a single table.  Useful for tests
# that exercise the renderer without caring about the exact content.
.test_doc_with <- function(tbl,
                            header = NULL,
                            footer = NULL) {
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1, secinfo = list(header = header,
                                                    footer = footer))
  doc <- rtf_tables(doc, list(tbl))
  doc
}

# Render an rtf_document (or rtfreport) to a temp file and return its
# contents collapsed into a single string for grepl-based assertions.
.render_to_string <- function(report) {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(report, f, overwrite = TRUE)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

# The tests of a deprecated function (or argument) check what it still does,
# not its warning (test-api-surface.R checks that): mark it as already warned.
local_deprecated <- function(...) {
  st <- rtfreporter:::.deprecation_state
  for (k in c(...)) assign(k, TRUE, envir = st)
  invisible()
}

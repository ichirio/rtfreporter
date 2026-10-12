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

# rtf_tables()'s 26 table-format arguments and `read_gt` are deprecated
# (#594, removed in 0.9.0) but behave as before until then.  A test file that
# tests that behaviour calls this first, so the once-a-session warnings are
# taken as said; test-api-surface.R checks the warnings themselves.
silence_rtf_tables_deprecation <- function() {
  silence_deprecation(paste0("rtf_tables_", rtfreporter:::.rtf_tables_fmt_args))
}

# The same for deprecated functions (their keys: the function's name), whose
# own tests run until 0.9.0 removes them
silence_deprecation <- function(keys) {
  st <- rtfreporter:::.deprecation_state
  for (k in keys) assign(k, TRUE, envir = st)
  invisible()
}

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

# Whether the session's native encoding can hold every character of `x`.  A
# non-ASCII file name or argument name (a Japanese folder, `p<U+5024> = `)
# cannot be made in a C locale: R stops ("unable to translate ... to native
# encoding") or turns it into "<U+5024>".  A UTF-8 locale holds them all.
native_can_hold <- function(x) {
  !anyNA(iconv(enc2utf8(as.character(x)), "UTF-8", ""))
}

# Skip the rest of a test when the native encoding cannot hold `x` -- only
# for a case that cannot work there at all.  Call it at the end of a block, so
# the block's other cases have run.
skip_unless_native_can_hold <- function(x) {
  skip_if_not(native_can_hold(x), "A non-ASCII name the native encoding cannot hold")
}

# Run `code` with LC_CTYPE set to C (an ASCII native encoding), the locale put
# back after; skip when this R cannot set it.
with_c_ctype <- function(code) {
  old <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", old), add = TRUE)
  if (!nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", "C")))) {
    skip("LC_CTYPE cannot be set to C")
  }
  force(code)
}

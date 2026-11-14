# ============================================================================
#  A table-of-contents book to check in Word (#1056, #1068)
# ============================================================================
#
#  A thin wrapper: the books are built by .tb_build_books() in
#  tests/testthat/helper-toc-books.R, the one copy of the code, which
#  test-assemble-toc-books.R also runs to rebuild them and compare their bytes
#  with the fixtures.  What the books hold is described there.  This script
#  attaches rtfreporter from the source tree (data-raw/_load.R), sources the
#  helper by its path from the package root, and writes:
#
#    <out_dir>/inputs/*.rtf     the 36 reports
#    <out_dir>/toc-book-default.rtf   the default TOC style
#    <out_dir>/toc-book-hanging.rtf   toc_style = list(entry = list(hanging = TRUE))
#
#  The two books are committed as tests/testthat/fixtures/toc-word-check-
#  default.rtf and toc-word-check-hanging.rtf.
#
#  Run from the package root, with the folder to write to:
#      Rscript data-raw/gen_toc_word_check.R <out_dir>

source("data-raw/_load.R")
source("tests/testthat/helper-toc-books.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Usage: Rscript data-raw/gen_toc_word_check.R <out_dir>")

books <- .tb_build_books(args[1L])
for (out in books) message(sprintf("%s: %.0f KB", out, file.size(out) / 1024))
message(length(list.files(file.path(args[1L], "inputs"), "[.]rtf$")), " reports")

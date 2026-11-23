# ============================================================================
#  A table with a DRAFT watermark and non-ASCII text, to judge in Word
# ============================================================================
#
#  The PK concentration table of data-raw/gen_pk_conc.R (same data and code)
#  on a document with rtf_document(watermark = ).  A person can see on a real,
#  full page whether the watermark is diagonal, light, behind the table on
#  every page, and the table readable over it.
#
#  The page also carries the characters a report uses beyond ASCII, for the
#  same look in Word: lambda, mu, the middle dot, the plus-minus, less-or-equal
#  and greater-or-equal signs, the degree sign, the vulgar one half, sub- and
#  superscripts next to Greek letters (the package's markup: _{ } and ^{ }),
#  and one Japanese line in a Japanese font (the east_asian font slot).  They
#  are written as \u escapes so the code stays ASCII.
#
#  The document is built in tests/testthat/helper-word-checked-examples.R
#  (.wck_watermark_doc(), which holds a copy of gen_pk_conc.R's table), the
#  code a test rebuilds the file with (#1071).
#
#  Writes inst/rtf-examples/watermark-example.rtf.  Run from the package root:
#      Rscript data-raw/gen_watermark_example.R

source("data-raw/_load.R")
source("tests/testthat/helper-word-checked-examples.R")

outfile <- "inst/rtf-examples/watermark-example.rtf"

generate_rtfreport(.wck_watermark_doc(), outfile, overwrite = TRUE)
message(sprintf("%s: %.0f KB", outfile, file.size(outfile) / 1024))

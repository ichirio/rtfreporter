# ============================================================================
#  A figure page to judge in Word -- mean change from baseline by visit
# ============================================================================
#
#  The structure fixture tests/testthat/fixtures/validation/figure.rtf embeds a
#  1x1 PNG, so a person cannot judge it.  This example draws a real plot with
#  base graphics (fictional data) at 8 x 5 inches and 300 dpi, and puts it on
#  a landscape letter page with a running header, a footer, three title lines
#  and two footnote lines, the figure centred.  The titles and footnotes follow
#  the figure's placement (#968): they sit centred under / above it.
#
#  The plot and the document are built in
#  tests/testthat/helper-word-checked-examples.R (.wck_figure_png(),
#  .wck_figure_doc()), the code a test rebuilds the file with (#1071).
#
#  Writes inst/rtf-examples/figure-example.rtf (the plot is drawn into a
#  temporary PNG that the RTF embeds).  Run from the package root:
#      Rscript data-raw/gen_figure_example.R

source("data-raw/_load.R")
source("tests/testthat/helper-word-checked-examples.R")

outfile <- "inst/rtf-examples/figure-example.rtf"
pngfile <- tempfile("figure-example-", fileext = ".png")

.wck_figure_png(pngfile)
generate_rtfreport(.wck_figure_doc(pngfile), outfile, overwrite = TRUE)
message(sprintf("%s: %.0f KB", outfile, file.size(outfile) / 1024))

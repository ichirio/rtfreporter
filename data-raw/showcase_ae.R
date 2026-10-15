# data-raw/showcase_ae.R
# ---------------------------------------------------------------------------
# Regenerate the Adverse Events (AE) showcase RTFs.
#
# SINGLE SOURCE OF TRUTH: the article vignettes/articles/showcase-ae.Rmd.  This
# script does NOT duplicate any table-building logic -- it extracts the article's
# own code chunks with knitr::purl() and runs them, so the committed
# inst/rtf-examples/showcase/ae_*.rtf can never drift from the article again.
#
# The article's display-only chunks (the snapshot helper, the `library()` setup,
# and every `img-*` screenshot chunk) are marked `purl = FALSE`, so only the
# data / furniture / framework chunks run here.  Each framework chunk ends with
# `generate_rtfreport(doc, "ae_<framework>.rtf", ...)` -- written relative to the
# working directory, which we point at the output folder below.
#
# Requires every framework the article uses (gtsummary, cards, tfrmt, Tplyr,
# flextable, huxtable, rtables, tern, ...) to be installed.
#
# Each RTF's picture, ae_<framework>.png beside it, is made from the RTF
# here: its first page through LibreOffice, cropped as every picture of the
# site is (data-raw/rtf_png.R), so the showcase pictures look like the other
# articles' and cannot drift from the RTF.
#
# Run with:  Rscript data-raw/showcase_ae.R
# ---------------------------------------------------------------------------

suppressMessages(devtools::load_all(".", quiet = TRUE))

rmd    <- "vignettes/articles/showcase-ae.Rmd"
outdir <- "inst/rtf-examples/showcase"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# Extract the article's runnable code chunks (purl = FALSE chunks are skipped).
code <- knitr::purl(rmd, output = tempfile(fileext = ".R"),
                    quiet = TRUE, documentation = 0)

cat("Generating AE showcase RTFs from", rmd, "...\n")
old <- setwd(outdir)
on.exit(setwd(old), add = TRUE)
source(code, local = new.env())          # data + furniture + framework chunks
setwd(old)

source(file.path("data-raw", "rtf_png.R"))   # soffice, rtf_to_png()
for (f in sort(list.files(outdir, "^ae_.*\\.rtf$"))) {
  png <- file.path(outdir, sub("[.]rtf$", ".png", f))
  rtf_to_png(file.path(outdir, f), png)
  cat(sprintf("  wrote %s (%d bytes) and its picture\n", f, file.size(file.path(outdir, f))))
}

## ---------------------------------------------------------------------------
##  Pictures of the article "A listing and its summary in one table:
##  plan_append()" (vignettes/articles/plan-append.Rmd and its -ja twin).
##
##  The page writes its RTF file into RTFREPORTER_ARTICLE_OUT; this script
##  renders it with that folder set, converts the file to PDF through
##  LibreOffice and writes the pages the article shows as PNG into
##  vignettes/articles/img/plan-append/ -- committed there, beside the page,
##  so the pictures show on GitHub as well as on the site.
##
##  Run from the package root, after a change to the page's report:
##    Rscript data-raw/gen_plan_append.R
## ---------------------------------------------------------------------------
pkgload::load_all(".", quiet = TRUE)
source(file.path("data-raw", "rtf_png.R"))   # soffice, squash_blank_band()

root <- getwd()
out <- file.path(tempdir(), "plan-append")
dir.create(out, showWarnings = FALSE)
out <- normalizePath(out)
Sys.setenv(RTFREPORTER_ARTICLE_OUT = out)
rmarkdown::render(file.path(root, "vignettes", "articles", "plan-append.Rmd"),
                  output_format = rmarkdown::md_document(), output_dir = out,
                  knit_root_dir = out, quiet = TRUE,
                  envir = new.env(parent = globalenv()))
setwd(root)

img <- file.path(root, "vignettes", "articles", "img", "plan-append")
dir.create(img, showWarnings = FALSE, recursive = TRUE)
# file -> the pages the article shows
shots <- list(pk_conc = c(1L, 3L))
for (f in names(shots)) {
  rtf <- file.path(out, paste0(f, ".rtf"))
  env <- if (.Platform$OS.type == "unix") "LD_LIBRARY_PATH=" else character()
  system2(soffice, c("--headless", "--convert-to", "pdf", "--outdir",
                     shQuote(out), shQuote(rtf)), stdout = FALSE, stderr = FALSE, env = env)
  pdf <- file.path(out, paste0(f, ".pdf"))
  for (p in shots[[f]]) {
    raw <- file.path(out, sprintf("%s-%d.png", f, p))
    png::writePNG(pdftools::pdf_render_page(pdf, page = p, dpi = 110, numeric = TRUE), raw)
    squash_blank_band(raw, file.path(img, sprintf("%s_page%d.png", f, p)))
  }
}
list.files(img)

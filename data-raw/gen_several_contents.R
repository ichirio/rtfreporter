## ---------------------------------------------------------------------------
##  Pictures of the article "Several contents on one page"
##  (vignettes/articles/several-contents.Rmd and its -ja twin).
##
##  The page writes its RTF files into RTFREPORTER_ARTICLE_OUT; this script
##  renders it with that folder set, converts each file to PDF through
##  LibreOffice and writes the pages the article shows as PNG into
##  vignettes/articles/img/several-contents/ -- committed there, beside the
##  page, so the pictures show on GitHub as well as on the site.
##
##  Run from the package root, after a change to the page's reports:
##    Rscript data-raw/gen_several_contents.R
## ---------------------------------------------------------------------------
pkgload::load_all(".", quiet = TRUE)
source(file.path("data-raw", "rtf_png.R"))   # soffice, squash_blank_band()

root <- getwd()
out <- file.path(tempdir(), "several-contents")
dir.create(out, showWarnings = FALSE)
out <- normalizePath(out)
Sys.setenv(RTFREPORTER_ARTICLE_OUT = out)
rmarkdown::render(file.path(root, "vignettes", "articles", "several-contents.Rmd"),
                  output_format = rmarkdown::md_document(), output_dir = out,
                  knit_root_dir = out, quiet = TRUE,
                  envir = new.env(parent = globalenv()))
setwd(root)

img <- file.path(root, "vignettes", "articles", "img", "several-contents")
dir.create(img, showWarnings = FALSE, recursive = TRUE)
# file -> the pages the article shows
shots <- list(a_table_table = 1L, b_table_figure = 1L, c_pk = 1L, d_pk = 1:2, t_tokens = 2L)
for (f in names(shots)) {
  rtf <- file.path(out, paste0(f, ".rtf"))
  env <- if (.Platform$OS.type == "unix") "LD_LIBRARY_PATH=" else character()
  system2(soffice, c("--headless", "--convert-to", "pdf", "--outdir",
                     shQuote(out), shQuote(rtf)), stdout = FALSE, stderr = FALSE, env = env)
  pdf <- file.path(out, paste0(f, ".pdf"))
  pages <- shots[[f]]
  for (p in pages) {
    raw <- file.path(out, sprintf("%s-%d.png", f, p))
    png::writePNG(pdftools::pdf_render_page(pdf, page = p, dpi = 110, numeric = TRUE), raw)
    name <- if (f %in% c("a_table_table", "b_table_figure", "c_pk")) f
            else sprintf("%s_page%d", f, p)
    squash_blank_band(raw, file.path(img, paste0(name, ".png")))
  }
}
list.files(img)

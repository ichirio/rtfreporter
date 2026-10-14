## ---------------------------------------------------------------------------
##  Pictures for the Gallery article (vignettes/articles/gallery.Rmd), the
##  README's examples (man/figures/readme-ard-example.png,
##  readme-tern-example.png, readme-gtsummary-example.png) and Get started's
##  adverse-events table and three short examples
##  (man/figures/get-started-ae.png, get-started-dm-p.png, -ae5.png, -orr.png;
##  the last three from the Tables from an ARD article).
##
##  The pictures are made from the code that is shown, not from a copy of it:
##    * the Gallery article and Get started are rendered with
##      RTFREPORTER_ARTICLE_OUT set, so every RTF they write lands in one
##      folder;
##    * the ```r blocks of README.md that write a file are run as they stand.
##  LibreOffice turns each RTF into a PDF, and the first page of the PDF is
##  rendered to a PNG (pdftools) -- sharper text than LibreOffice's own PNG
##  export.  The empty band between the table and the running footer is cut
##  down so the picture stays small, and the PNG is quantised (pngquant, when
##  installed).
##
##  Run from the package root, after a change to the article or the README
##  example:
##    Rscript data-raw/gen_gallery.R
##
##  Needs LibreOffice (soffice), pdftools, png, cards, gtsummary and
##  pharmaverseadam; pngquant / optipng are used when they are on the PATH.
## ---------------------------------------------------------------------------
pkgload::load_all(".", quiet = TRUE)

source(file.path("data-raw", "rtf_png.R"))   # soffice, rtf_to_png()

## 1. The Gallery article --------------------------------------------------
out <- file.path(tempdir(), "gallery")
dir.create(out, showWarnings = FALSE)
Sys.setenv(RTFREPORTER_ARTICLE_OUT = normalizePath(out))
rmarkdown::render(file.path("vignettes", "articles", "gallery.Rmd"),
                  output_format = rmarkdown::md_document(),
                  output_dir = tempdir(), quiet = TRUE)

figs <- file.path("inst", "rtf-examples", "gallery")
dir.create(figs, showWarnings = FALSE, recursive = TRUE)
for (f in list.files(out, pattern = "[.]rtf$", full.names = TRUE)) {
  rtf_to_png(f, file.path(figs, sub("[.]rtf$", ".png", basename(f))))
}

## 2. The README's examples -----------------------------------------------
## Each ```r block that writes a file, in order, in one session (the tern and
## gtsummary examples use the ARD example's `adsl`): t_dm.rtf,
## t_dm_tern.rtf, t_dm_gtsummary.rtf.
readme <- readLines("README.md", encoding = "UTF-8")
blocks <- lapply(grep("^``` ?r$", readme), function(open) {
  close <- open + grep("^```$", readme[-seq_len(open)])[1L]
  readme[(open + 1L):(close - 1L)]
})
# the blocks that write a file, and the study's header and footer they use
blocks <- Filter(function(b) any(grepl("generate_rtfreport(", b, fixed = TRUE)) ||
                   any(grepl("study_doc <- function", b, fixed = TRUE)),
                 blocks)
code   <- unlist(blocks)
code   <- code[!grepl("^library\\(rtfreporter\\)", code)]   # load_all() above
wd <- tempfile("readme-")
dir.create(wd)
old <- setwd(wd)
eval(parse(text = code), envir = new.env())
setwd(old)
# (file.path() drops names: they are put back)
readme_png <- c(t_dm = "readme-ard-example.png", t_dm_tern = "readme-tern-example.png",
                t_dm_gtsummary = "readme-gtsummary-example.png")
readme_png[] <- file.path("man", "figures", readme_png)
for (f in names(readme_png)) {
  rtf_to_png(file.path(wd, paste0(f, ".rtf")), readme_png[[f]])
}

## 3. Get started's adverse-events table -------------------------------------
gs <- file.path(tempdir(), "get-started")
dir.create(gs, showWarnings = FALSE)
Sys.setenv(RTFREPORTER_ARTICLE_OUT = normalizePath(gs))
rmarkdown::render(file.path("vignettes", "rtfreporter.Rmd"),
                  output_format = rmarkdown::md_document(),
                  output_dir = tempdir(), quiet = TRUE)
gs_png <- file.path("man", "figures", "get-started-ae.png")
rtf_to_png(file.path(gs, "t_ae.rtf"), gs_png)

## 4. Get started's three short examples, from the Tables from an ARD article
ta <- file.path(tempdir(), "tables-from-ard")
dir.create(ta, showWarnings = FALSE)
Sys.setenv(RTFREPORTER_ARTICLE_OUT = normalizePath(ta))
rmarkdown::render(file.path("vignettes", "articles", "tables-from-ard.Rmd"),
                  output_format = rmarkdown::md_document(),
                  output_dir = tempdir(), quiet = TRUE)
ta_png <- c(t_dm_p = "get-started-dm-p.png", t_ae5 = "get-started-ae5.png",
            t_orr = "get-started-orr.png")
ta_png[] <- file.path("man", "figures", ta_png)
for (f in names(ta_png)) {
  rtf_to_png(file.path(ta, paste0(f, ".rtf")), ta_png[[f]])
}

pngs <- c(list.files(figs, pattern = "[.]png$", full.names = TRUE),
          readme_png, gs_png, ta_png)
message(paste(sprintf("%-50s %6.1f KB", pngs, file.size(pngs) / 1024),
              collapse = "\n"))

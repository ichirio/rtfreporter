## ---------------------------------------------------------------------------
##  Pictures for the Gallery article (vignettes/articles/gallery.Rmd), the
##  README's examples (man/figures/readme-ard-example.png,
##  readme-tern-example.png, readme-gtsummary-example.png) and Get started's
##  adverse-events table and three short examples
##  (man/figures/get-started-ae.png, -dm.png, -dm-p.png, -ae5.png, -orr.png;
##  the last three from the Tables from an ARD article), and an assembled
##  file's table of contents and the page its link opens, for Rendering,
##  post-processing and assembly (inst/rtf-examples/assemble/).
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

## 3. Get started's table ----------------------------------------------------
gs <- file.path(tempdir(), "get-started")
dir.create(gs, showWarnings = FALSE)
Sys.setenv(RTFREPORTER_ARTICLE_OUT = normalizePath(gs))
rmarkdown::render(file.path("vignettes", "rtfreporter.Rmd"),
                  output_format = rmarkdown::md_document(),
                  output_dir = tempdir(), quiet = TRUE)
rtf_to_png(file.path(gs, "t_dm.rtf"), file.path("man", "figures", "get-started-dm.png"))

## 4. Get started's four other tables, from the Tables from an ARD article
ta <- file.path(tempdir(), "tables-from-ard")
dir.create(ta, showWarnings = FALSE)
Sys.setenv(RTFREPORTER_ARTICLE_OUT = normalizePath(ta))
rmarkdown::render(file.path("vignettes", "articles", "tables-from-ard.Rmd"),
                  output_format = rmarkdown::md_document(),
                  output_dir = tempdir(), quiet = TRUE)
ta_png <- c(t_ae = "get-started-ae.png", t_dm_p = "get-started-dm-p.png",
            t_ae5 = "get-started-ae5.png", t_orr = "get-started-orr.png")
ta_png[] <- file.path("man", "figures", ta_png)
for (f in names(ta_png)) {
  rtf_to_png(file.path(ta, paste0(f, ".rtf")), ta_png[[f]])
}

## 5. An assembled file, for Rendering, post-processing and assembly ---------
## The Gallery's outputs assembled with the call the article shows, turned
## into a PDF by LibreOffice; the cover and the table of contents side by
## side with one entry boxed, and the page that entry's link opens.
asm <- file.path(tempdir(), "assemble")
dir.create(asm, showWarnings = FALSE)
old <- setwd(out)
files <- c("t_dm.rtf", "t_ae.rtf", "t_vs.rtf", "l_sae.rtf")   # the Gallery's outputs
assemble_rtf(
  files, file.path(asm, "XYZ-001_tables.rtf"), overwrite = TRUE,
  cover = list(title = "Study XYZ-001", subtitle = "Tables and Listings",
               version = "Draft 1"),
  toc = data.frame(
    file    = files,
    heading = c("DEMOGRAPHICS", "SAFETY", "SAFETY", "LISTINGS"),
    label   = c("Table 14.1.1  Demographic Characteristics",
                "Table 14.3.1.1  Treatment-Emergent Adverse Events by SOC and PT",
                "Table 14.3.5.1  Vital Signs by Visit",
                "Listing 16.2.7.2  Severe Treatment-Emergent Adverse Events")),
  toc_page_numbering = "roman")
setwd(old)
env <- if (.Platform$OS.type == "unix") "LD_LIBRARY_PATH=" else character()
system2(soffice, c("--headless", "--convert-to", "pdf", "--outdir", shQuote(asm),
                   shQuote(file.path(asm, "XYZ-001_tables.rtf"))),
        stdout = FALSE, stderr = FALSE, env = env)
pdf <- file.path(asm, "XYZ-001_tables.pdf")
stopifnot(length(pdftools::pdf_toc(pdf)$children) == length(files))  # a bookmark a file

asm_figs <- file.path("inst", "rtf-examples", "assemble")
dir.create(asm_figs, showWarnings = FALSE, recursive = TRUE)
dpi <- 110; k <- dpi / 72
pg <- function(i) pdftools::pdf_render_page(pdf, page = i, dpi = dpi, numeric = TRUE)[, , 1:3]
txt <- pdftools::pdf_text(pdf)
target <- which(grepl("Treatment-Emergent Adverse Events by SOC and PT", txt))
target <- target[target > 2L][1L]                  # past the cover and the TOC
cover <- pg(1L); toc <- pg(2L)
gap <- array(0.85, c(dim(cover)[1L], 24L, 3L))
img <- array(1, c(dim(cover)[1L], dim(cover)[2L] + dim(gap)[2L] + dim(toc)[2L], 3L))
img[, seq_len(dim(cover)[2L]), ] <- cover
img[, dim(cover)[2L] + seq_len(dim(gap)[2L]), ] <- gap
off <- dim(cover)[2L] + dim(gap)[2L]
img[, off + seq_len(dim(toc)[2L]), ] <- toc
d <- pdftools::pdf_data(pdf)[[2L]]                 # the TOC page's words (points)
row <- d[abs(d$y - d$y[d$text == "14.3.1.1"][1L]) < 3, ]
x0 <- off + (min(row$x) - 4) * k; x1 <- off + (max(row$x + row$width) + 4) * k
y0 <- (min(row$y) - 3) * k;       y1 <- (max(row$y + row$height) + 3) * k
ylab <- (max(d$y[d$text == "Listing"]) + 34) * k
h <- min(dim(img)[1L], round(ylab + 40)); w <- dim(img)[2L]
img <- img[seq_len(h), , , drop = FALSE]
grDevices::png(file.path(asm_figs, "toc.png"), width = w, height = h)
par(mar = c(0, 0, 0, 0)); plot.new()
plot.window(c(0, w), c(h, 0), xaxs = "i", yaxs = "i")
rasterImage(img, 0, h, w, 0)
rect(x0, y0, x1, y1, border = "#d62728", lwd = 3)
xa <- off + (min(d$x[d$text %in% c("DEMOGRAPHICS", "SAFETY", "LISTINGS")]) - 10) * k
segments(xa, ylab - 8, xa, (y0 + y1) / 2, col = "#d62728", lwd = 3)
arrows(xa, (y0 + y1) / 2, x0 - 3, (y0 + y1) / 2, col = "#d62728", lwd = 3,
       length = 0.1)
text(xa, ylab + 4,
     sprintf("Click the entry: the PDF opens the AE table at page %d.", target),
     adj = c(0, 0.5), col = "#d62728", font = 2, cex = 1.25)
grDevices::dev.off()
body <- tempfile(fileext = ".png")
png::writePNG(pdftools::pdf_render_page(pdf, page = target, dpi = 144,
                                        numeric = TRUE), body)
squash_blank_band(body, file.path(asm_figs, "target.png"))
asm_png <- file.path(asm_figs, c("toc.png", "target.png"))

pngs <- c(list.files(figs, pattern = "[.]png$", full.names = TRUE),
          readme_png, gs_png, ta_png, asm_png)
message(paste(sprintf("%-50s %6.1f KB", pngs, file.size(pngs) / 1024),
              collapse = "\n"))

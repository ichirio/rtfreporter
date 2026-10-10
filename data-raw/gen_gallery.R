## ---------------------------------------------------------------------------
##  Pictures for the Gallery article (vignettes/articles/gallery.Rmd) and the
##  README's 30-second example (man/figures/readme-30s-example.png).
##
##  The pictures are made from the code that is shown, not from a copy of it:
##    * the Gallery article is rendered with RTFREPORTER_ARTICLE_OUT set, so
##      every RTF it writes lands in one folder;
##    * the first ```r block of README.md is run as it stands.
##  LibreOffice then turns the first page of each RTF into a PNG.  The empty
##  band between the table and the running footer is cut down so the picture
##  stays small, and the PNG is quantised (pngquant, when installed).
##
##  Run from the package root, after a change to the article or the README
##  example:
##    Rscript data-raw/gen_gallery.R
##
##  Needs LibreOffice (soffice) and the png package; pngquant / optipng are
##  used when they are on the PATH.
## ---------------------------------------------------------------------------
pkgload::load_all(".", quiet = TRUE)

soffice <- Filter(function(p) nzchar(p) && file.exists(p), c(
  "C:/Program Files/LibreOffice/program/soffice.exe",
  "C:/Program Files (x86)/LibreOffice/program/soffice.exe",
  Sys.which("soffice")))[1]
if (is.na(soffice)) stop("LibreOffice (soffice) is needed for the pictures.")

## RTF -> PNG of page 1, written as <png>.  Only the first section (page) of
## the file is converted: LibreOffice 24.2 runs a short page on into the next
## section instead of starting a new page, which a picture of page 1 should
## not show.
rtf_to_png <- function(rtf, png) {
  tmp <- tempfile("png-")
  dir.create(tmp)
  txt <- readLines(rtf, warn = FALSE)
  cut <- which(txt == "\\sect")[1L]
  if (!is.na(cut)) txt <- c(txt[seq_len(cut - 1L)], "}")
  rtf <- file.path(tmp, basename(rtf))
  writeLines(txt, rtf)
  tmp <- file.path(tmp, "out")
  dir.create(tmp)
  # R's LD_LIBRARY_PATH (R's own lib folder first) stops LibreOffice from
  # finding its libraries on Linux, so it is cleared for this one call.
  env <- if (.Platform$OS.type == "unix") "LD_LIBRARY_PATH=" else character()
  system2(soffice, c("--headless", "--convert-to", "png", "--outdir",
                     shQuote(tmp), shQuote(normalizePath(rtf))),
          stdout = FALSE, stderr = FALSE, env = env)
  out <- list.files(tmp, pattern = "[.]png$", full.names = TRUE)
  if (length(out) != 1L) stop("LibreOffice did not convert ", rtf)
  squash_blank_band(out, png)
}

## Shorten every run of blank rows taller than `keep` pixels to `keep`, so a
## page with a short table keeps its running header AND footer but loses the
## empty middle.  Then trim the outer margins to `pad` pixels.
squash_blank_band <- function(src, dest, keep = 36L, pad = 24L) {
  img <- png::readPNG(src)
  if (length(dim(img)) == 2L) img <- array(img, c(dim(img), 1L))
  lum <- apply(img[, , seq_len(min(3L, dim(img)[3L])), drop = FALSE], c(1, 2),
               mean)
  ink_row <- apply(lum < 0.95, 1, any)
  ink_col <- apply(lum < 0.95, 2, any)
  r <- rle(ink_row)
  keep_rows <- unlist(lapply(seq_along(r$lengths), function(i) {
    n <- r$lengths[i]
    rep(c(TRUE, FALSE), c(if (r$values[i]) n else min(n, keep),
                          if (r$values[i]) 0L else max(0L, n - keep)))
  }))
  rows <- which(keep_rows)
  ink  <- which(ink_row[rows])
  rows <- rows[max(1L, min(ink) - pad):min(length(rows), max(ink) + pad)]
  cols <- which(ink_col)
  cols <- max(1L, min(cols) - pad):min(ncol(lum), max(cols) + pad)
  png::writePNG(img[rows, cols, , drop = FALSE], dest)
  if (nzchar(Sys.which("pngquant"))) {
    system2("pngquant", c("--force", "--skip-if-larger", "--quality=60-90",
                          "--ext", ".png", shQuote(dest)))
  }
  if (nzchar(Sys.which("optipng"))) {
    system2("optipng", c("-quiet", "-o2", shQuote(dest)))
  }
  invisible(dest)
}

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

## 2. The README's 30-second example ----------------------------------------
readme <- readLines("README.md", encoding = "UTF-8")
open   <- grep("^``` ?r$", readme)[1L]
close  <- open + grep("^```$", readme[-seq_len(open)])[1L]
code   <- readme[(open + 1L):(close - 1L)]
code   <- code[!grepl("^library\\(rtfreporter\\)", code)]   # load_all() above
wd <- tempfile("readme-")
dir.create(wd)
old <- setwd(wd)
eval(parse(text = code), envir = new.env())
setwd(old)
rtf_to_png(file.path(wd, "t_dm.rtf"),
           file.path("man", "figures", "readme-30s-example.png"))

pngs <- c(list.files(figs, pattern = "[.]png$", full.names = TRUE),
          file.path("man", "figures", "readme-30s-example.png"))
message(paste(sprintf("%-50s %6.1f KB", pngs, file.size(pngs) / 1024),
              collapse = "\n"))

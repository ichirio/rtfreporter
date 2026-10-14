## ---------------------------------------------------------------------------
##  RTF -> PNG for the site's pictures, shared by data-raw/gen_gallery.R and
##  data-raw/gen_plan_articles.R: page 1 of the RTF through LibreOffice (PDF)
##  and pdftools, the empty band cut down, the margins trimmed.
## ---------------------------------------------------------------------------

soffice <- Filter(function(p) nzchar(p) && file.exists(p), c(
  "C:/Program Files/LibreOffice/program/soffice.exe",
  "C:/Program Files (x86)/LibreOffice/program/soffice.exe",
  Sys.which("soffice")))[1]
if (is.na(soffice)) stop("LibreOffice (soffice) is needed for the pictures.")

## RTF -> PNG of page 1, written as <png>.  Only the first section (page) of
## the file is converted: LibreOffice 24.2 runs a short page on into the next
## section instead of starting a new page, which a picture of page 1 should
## not show.
rtf_to_png <- function(rtf, png, dpi = 144) {
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
  system2(soffice, c("--headless", "--convert-to", "pdf", "--outdir",
                     shQuote(tmp), shQuote(normalizePath(rtf))),
          stdout = FALSE, stderr = FALSE, env = env)
  pdf <- list.files(tmp, pattern = "[.]pdf$", full.names = TRUE)
  if (length(pdf) != 1L) stop("LibreOffice did not convert ", rtf)
  out <- sub("[.]pdf$", ".png", pdf)
  png::writePNG(pdftools::pdf_render_page(pdf, page = 1, dpi = dpi,
                                          numeric = TRUE), out)
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
  img <- img[rows, cols, , drop = FALSE]
  # A page with no colour (every table and listing) is written as grey, a
  # third of the size.
  rgb <- img[, , seq_len(min(3L, dim(img)[3L])), drop = FALSE]
  if (dim(rgb)[3L] == 3L &&
      max(abs(rgb[, , 1] - rgb[, , 2]), abs(rgb[, , 2] - rgb[, , 3])) < 0.02) {
    img <- rgb[, , 1]
  }
  png::writePNG(img, dest)
  if (nzchar(Sys.which("pngquant"))) {
    system2("pngquant", c("--force", "--skip-if-larger", "--quality=60-90",
                          "--ext", ".png", shQuote(dest)))
  }
  if (nzchar(Sys.which("optipng"))) {
    system2("optipng", c("-quiet", "-o2", shQuote(dest)))
  }
  invisible(dest)
}

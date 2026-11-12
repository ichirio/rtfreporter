# Set a PNG file's density (its pHYs chunk) to `dpi`, whatever device drew
# it: macOS's default png device (quartz) writes 72 dpi whatever `res =` is,
# and its cairo device needs XQuartz.  Any pHYs chunk is replaced by one
# right after IHDR.  Used by the figure tests' fixtures.

.png_xor32 <- function(a, b) {
  bitwXor(a %/% 65536, b %/% 65536) * 65536 + bitwXor(a %% 65536, b %% 65536)
}

.png_crc_table <- local({
  tab <- NULL
  function() {
    if (is.null(tab)) {
      tab <<- vapply(0:255, function(n) {
        c <- n
        for (k in 1:8) {
          c <- if (c %% 2 == 1) .png_xor32(0xEDB88320, c %/% 2) else c %/% 2
        }
        c
      }, numeric(1))
    }
    tab
  }
})

.png_crc32 <- function(bytes) {
  tab <- .png_crc_table()
  crc <- 0xFFFFFFFF
  for (b in as.integer(bytes)) {
    crc <- .png_xor32(tab[.png_xor32(crc, b) %% 256 + 1], crc %/% 256)
  }
  .png_xor32(crc, 0xFFFFFFFF)
}

.png_u32 <- function(x) as.raw(c(x %/% 16777216, x %/% 65536 %% 256, x %/% 256 %% 256, x %% 256))

.png_set_dpi <- function(f, dpi) {
  raw <- readBin(f, "raw", file.size(f))
  out <- list(raw[1:8])
  i <- 9L
  ppm <- round(dpi / 0.0254)
  while (i <= length(raw)) {
    len <- sum(as.numeric(raw[i:(i + 3L)]) * c(16777216, 65536, 256, 1))
    type <- rawToChar(raw[(i + 4L):(i + 7L)])
    end <- i + 11L + len
    if (type != "pHYs") out[[length(out) + 1L]] <- raw[i:end]
    if (type == "IHDR") {
      body <- c(charToRaw("pHYs"), .png_u32(ppm), .png_u32(ppm), as.raw(1L))
      out[[length(out) + 1L]] <- c(.png_u32(9), body, .png_u32(.png_crc32(body)))
    }
    i <- end + 1L
  }
  writeBin(unlist(out), f)
  invisible(f)
}

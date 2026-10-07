# data-raw/logo.R -- the hex logo, and the favicons made from it.
#
# rtfreporter, tflspec and tflplanner share one logo family, and this script
# is the same file in all three repositories: it draws the three stickers,
# writes the files of the package it is run in, and puts the three side by
# side in data-raw/logo-family.png.  Change the family here and copy the file
# to the other two repositories, so that they stay alike.
#
# Run it from the package root:  Rscript data-raw/logo.R
#
# It writes
#   man/figures/logo.svg, man/figures/logo.png   the sticker (518 x 600)
#   pkgdown/favicon/*                             the site's favicons
#   data-raw/logo-family.png                      the three, side by side
#
# The family: a pointy-top hexagon, a navy body, a border in the package's
# accent colour, a pictogram on a white page (the same paper, ink and
# corner radius in all three) and the package name in white, Source Sans 3
# Bold, the same size in all three.
#
# The SVG is plain SVG, written as text; the name is drawn as outlines, so
# it needs no font to show.  The outlines come from Source Sans 3 Bold
# (SIL Open Font License 1.1, https://github.com/adobe-fonts/source-sans),
# downloaded from a pinned release and checked by its MD5 sum.  The PNGs
# are rendered from the SVG with rsvg.
#
# Needs (developer only, not in DESCRIPTION): systemfonts (>= 1.1.0, for
# glyph_outline()), rsvg and magick.  pkgdown::build_favicons() is not used:
# it sends the logo to the RealFaviconGenerator web service.

stopifnot(
  file.exists("DESCRIPTION"),
  packageVersion("systemfonts") >= "1.1.0",
  requireNamespace("rsvg", quietly = TRUE),
  requireNamespace("magick", quietly = TRUE)
)
pkg <- unname(read.dcf("DESCRIPTION", fields = "Package")[1, 1])

# -- palette -----------------------------------------------------------------

navy  <- "#1f4e79"  # body; the pkgdown sites' primary colour
deep  <- "#163a5c"  # the page's shadow
paper <- "#f7f9fc"
ink   <- "#1f4e79"  # rules and strong lines on the page
soft  <- "#a9bdd2"  # weak lines and cells on the page
white <- "#ffffff"

family <- list(
  rtfreporter = list(accent = "#4fa3e0"),  # sky blue
  tflspec     = list(accent = "#3dbb8a"),  # green
  tflplanner  = list(accent = "#f2a33a")   # amber
)

# -- geometry ----------------------------------------------------------------

W <- 518; H <- 600                 # the hexSticker proportions
cx <- W / 2; cy <- H / 2
R <- 296                           # circumradius of the outer hexagon
border <- 26

hex_points <- function(r) {
  a <- (90 + 60 * 0:5) * pi / 180
  paste(sprintf("%.2f,%.2f", cx + r * cos(a), cy - r * sin(a)), collapse = " ")
}

f <- function(x) sprintf("%.2f", x)

rect <- function(x, y, w, h, fill, r = 0) {
  sprintf('<rect x="%s" y="%s" width="%s" height="%s" rx="%s" fill="%s"/>',
          f(x), f(y), f(w), f(h), f(r), fill)
}

# A line with round ends, as a thick stroke.
bar <- function(x1, y1, x2, y2, col, lwd) {
  sprintf(paste0('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="%s" ',
                 'stroke-width="%s" stroke-linecap="round"/>'),
          f(x1), f(y1), f(x2), f(y2), col, f(lwd))
}

# A white page with a shadow; every pictogram is drawn on these.
page <- function(x, y, w, h) {
  c(rect(x + 7, y + 9, w, h, deep, 14), rect(x, y, w, h, paper, 14))
}

# -- pictograms: drawn around (0, 0), about 260 x 230 ------------------------

# A report page: a title, and a table with the clinical rules (above and
# below the column headers and at the foot) and no other lines.
picto_rtfreporter <- function(accent) {
  x0 <- -92; w <- 184; y0 <- -115; h <- 230
  cols <- c(-60, 0, 50)
  rows <- c(22, 50, 78)
  c(page(x0, y0, w, h),
    bar(-45, -82, 45, -82, accent, 16),
    bar(-28, -58, 28, -58, soft, 10),
    bar(-68, -30, 68, -30, ink, 6),
    bar(-60, -14, -32, -14, ink, 9), bar(-10, -14, 10, -14, ink, 9),
    bar(40, -14, 60, -14, ink, 9),
    bar(-68, 2, 68, 2, ink, 6),
    unlist(lapply(rows, function(y) c(
      bar(-60, y, -28, y, soft, 10),
      bar(-8, y, 8, y, soft, 10),
      bar(42, y, 58, y, soft, 10)
    ))),
    bar(-68, 96, 68, 96, ink, 6))
}

# A spec sheet (a grid with a header row) and an arrow to R code (indented
# lines).
picto_tflspec <- function(accent) {
  sheet <- function(x0, y0) {
    w <- 104; h <- 150
    xs <- x0 + c(12, 46, 75); cw <- c(28, 23, 17)
    ys <- y0 + 44 + 24 * 0:3
    c(page(x0, y0, w, h),
      rect(x0 + 10, y0 + 14, w - 20, 18, accent, 4),
      unlist(lapply(ys, function(y)
        sprintf('%s%s%s', rect(xs[1], y, cw[1], 14, soft, 3),
                rect(xs[2], y, cw[2], 14, soft, 3),
                rect(xs[3], y, cw[3], 14, soft, 3)))))
  }
  code <- function(x0, y0) {
    w <- 104; h <- 150
    ind <- c(0, 16, 16, 32, 16, 0)
    len <- c(60, 52, 40, 30, 46, 20)
    col <- c(ink, accent, soft, accent, soft, ink)
    ys <- y0 + 24 + 21 * 0:5
    c(page(x0, y0, w, h),
      unlist(Map(function(y, i, l, cl) bar(x0 + 16 + i, y, x0 + 16 + i + l, y, cl, 10),
                 ys, ind, len, col)))
  }
  arrow <- sprintf(paste0('<path d="M -17 -24 L 3 0 L -17 24" fill="none" ',
                          'stroke="%s" stroke-width="13" stroke-linecap="round" ',
                          'stroke-linejoin="round"/>'), accent)
  c(sheet(-140, -88), code(36, -88), arrow)
}

# A planning board: three columns of cards, one of them picked out.
picto_tflplanner <- function(accent) {
  x0 <- -132; w <- 264; y0 <- -100; h <- 196
  colx <- x0 + 14 + 82 * 0:2
  cw <- 72
  cards <- list(c(0, 1, 2), c(0, 1), c(0))
  c(page(x0, y0, w, h),
    unlist(lapply(1:3, function(j) bar(colx[j] + 8, y0 + 22, colx[j] + 46, y0 + 22, ink, 10))),
    unlist(lapply(1:3, function(j) vapply(cards[[j]], function(k) {
      fill <- if (j == 2 && k == 1) accent else soft
      rect(colx[j], y0 + 42 + 48 * k, cw, 38, fill, 7)
    }, ""))))
}

# -- the name, as outlines ---------------------------------------------------

font_url <- paste0("https://raw.githubusercontent.com/adobe-fonts/source-sans/",
                   "3.052R/TTF/SourceSans3-Bold.ttf")
font_md5 <- "a7e469a26b59dfad765c65ae082c5223"
font <- file.path(tempdir(), "SourceSans3-Bold.ttf")
if (!file.exists(font)) utils::download.file(font_url, font, mode = "wb", quiet = TRUE)
stopifnot(unname(tools::md5sum(font)) == font_md5)

name_size <- 80   # the same for all three; "rtfreporter", the longest, fits
name_base <- 446  # baseline

name_path <- function(name) {
  s <- systemfonts::shape_string(name, path = font, size = name_size)
  x0 <- cx - s$metrics$width / 2
  d <- unlist(lapply(seq_len(nrow(s$shape)), function(i) {
    o <- systemfonts::glyph_outline(s$shape$index[i], font, size = name_size,
                                    tolerance = 0.05)
    if (!nrow(o)) return(NULL)
    vapply(split(o, o$contour), function(p) {
      xy <- sprintf("%.2f %.2f", x0 + s$shape$x_offset[i] + p$x, name_base - p$y)
      paste0("M", xy[1], " L", paste(xy[-1], collapse = " "), " Z")
    }, "")
  }))
  sprintf('<path fill="%s" d="%s"/>', white, paste(d, collapse = " "))
}

# -- the sticker -------------------------------------------------------------

# `name = FALSE` gives the favicon: the hexagon and a larger pictogram.
sticker <- function(p, name = TRUE) {
  accent <- family[[p]]$accent
  picto <- get(paste0("picto_", p))(accent)
  tf <- if (name) sprintf("translate(%s 226) scale(1.12)", f(cx))
        else sprintf("translate(%s %s) scale(1.45)", f(cx), f(cy + 4))
  c('<?xml version="1.0" encoding="UTF-8"?>',
    sprintf(paste0('<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" ',
                   'viewBox="0 0 %d %d">'), W, H, W, H),
    sprintf("<title>%s</title>", p),
    sprintf('<polygon points="%s" fill="%s"/>', hex_points(R), accent),
    sprintf('<polygon points="%s" fill="%s"/>', hex_points(R - border), navy),
    sprintf('<g transform="%s">', tf), picto, "</g>",
    if (name) name_path(p),
    "</svg>")
}

# Without the time stamps magick writes, so that a re-run changes nothing.
save <- function(img, file, ...) magick::image_write(magick::image_strip(img), file, ...)

render <- function(svg, file, width, height = width * H / W) {
  tmp <- tempfile(fileext = ".svg")
  writeLines(svg, tmp)
  rsvg::rsvg_png(tmp, file, width = round(width), height = round(height))
}

# A square image of the sticker, for the favicons.
square <- function(svg, size, bg = "none") {
  tmp <- tempfile(fileext = ".png")
  render(svg, tmp, height = size, width = size * W / H)
  img <- magick::image_read(tmp)
  magick::image_extent(magick::image_background(img, bg), sprintf("%dx%d", size, size),
                       gravity = "center", color = bg)
}

# -- write -------------------------------------------------------------------

stopifnot(pkg %in% names(family))
dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("pkgdown/favicon", recursive = TRUE, showWarnings = FALSE)

svg <- sticker(pkg)
writeLines(svg, "man/figures/logo.svg")
render(svg, "man/figures/logo.png", W)

icon <- sticker(pkg, name = FALSE)
writeLines(icon, "pkgdown/favicon/favicon.svg")
save(square(icon, 96), "pkgdown/favicon/favicon-96x96.png")
save(square(icon, 180, white), "pkgdown/favicon/apple-touch-icon.png")
for (n in c(192, 512)) {
  save(square(icon, n, white),
       sprintf("pkgdown/favicon/web-app-manifest-%dx%d.png", n, n))
}
save(magick::image_join(lapply(c(16, 32, 48), function(n) square(icon, n))),
     "pkgdown/favicon/favicon.ico", format = "ico")
writeLines(c(
  "{",
  sprintf('  "name": "%s",', pkg),
  sprintf('  "short_name": "%s",', pkg),
  '  "icons": [',
  '    {', '      "src": "web-app-manifest-192x192.png",', '      "sizes": "192x192",',
  '      "type": "image/png",', '      "purpose": "maskable"', '    },',
  '    {', '      "src": "web-app-manifest-512x512.png",', '      "sizes": "512x512",',
  '      "type": "image/png",', '      "purpose": "maskable"', '    }',
  '  ],',
  '  "theme_color": "#ffffff",',
  '  "background_color": "#ffffff",',
  '  "display": "standalone"',
  "}"), "pkgdown/favicon/site.webmanifest")

# The family, side by side, 300 px high each.
stickers <- lapply(names(family), function(p) {
  tmp <- tempfile(fileext = ".png")
  render(sticker(p), tmp, 300 * W / H)
  magick::image_border(magick::image_read(tmp), "none", "20x20")
})
save(magick::image_append(do.call(c, stickers)), "data-raw/logo-family.png")

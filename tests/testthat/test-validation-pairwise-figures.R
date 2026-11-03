# Validation tests: the figures -- rtfplot() / rtf_figures() / rtf_tables()
# with a figure / print() -- a pairwise set checked on the RTF written and the
# rtfplot made, and a decision table of the display size.  Helpers prefixed
# .vf_.  Fictional drawings only.
#
# ---------------------------------------------------------------------------
#  Design
# ---------------------------------------------------------------------------
#  The rules, as read from the help (rtfplot(), rtf_figures(),
#  print.rtfplot()) and the code (R/rtfplot.R, R/pipe-composition.R
#  rtf_figures(), R/generate_rtfreport.R .render_rtfplot()):
#
#    source   a PNG, JPEG or EMF file is embedded as it is (\pngblip,
#             \jpegblip, \emfblip), its \picw / \pich the pixel size (an
#             EMF's frame in 0.01 mm) and its native size the pixels at the
#             file's dpi (an EMF's frame at 2540 a inch); a plot object (a
#             function that draws, a grid grob) is drawn into a PNG at
#             render_width x render_height inches (default 6.5 x 4.5) and
#             render_dpi (default 300) with png_device ("auto", "png",
#             "ragg"), so its pixels are inches x dpi and its native size
#             the inches, whatever the dpi or device.  render_* and
#             png_device are refused for a file (FR-AS209), so they are
#             "na" with one.
#    display  width_twips / height_twips: none -> the native size; one ->
#             the other from the pixel aspect ratio; both -> as given
#             (\picwgoal / \pichgoal).
#    align    \qc / \ql / \qr on the picture's paragraph: rtf_figures(align
#             = ) for a path or a plot object, an rtfplot() keeps its own.
#    via      rtfplot() then rtf_figures(); rtf_figures() on the path or
#             object with the arguments; rtf_tables() of an rtfplot().
#    several  each figure a block of its own: a new page (\page) by default,
#             with page_break = FALSE under the one before after
#             space_before empty paragraphs.
#    blocks   a title block above its figure, a footnote below it as a
#             one-column table as wide as the figure (\cellx = display
#             width).
#
#  (A) FT-AS143, pairwise.  Factors and levels:
#    source   png (300 x 200 px at 150 dpi), jpeg (240 x 160 px at 120 dpi),
#             emf (4 x 3 in; skipped without devEMF), fn (a function that
#             draws), grob (grid::rectGrob())
#    size     na, default, small (3 x 2 in)
#    dpi      na, default, 72, 150
#    device   na, auto, png, ragg
#    display  native, width (5000), height (2000), both (4000 x 1500)
#    align    center, left, right
#    via      rtfplot, figures, tables
#    count    1, 2 (page_break TRUE), 2flow (the second page_break = FALSE,
#             space_before = 1)
#    blocks   none, titles, both (titles and footnotes)
#  Constraint: a file takes na for size, dpi and device, a plot object none
#  of them (FR-AS209).  12,636 valid rows; the candidates are every 41st of
#  them, checked to hold every valid pair of levels, and .allpairs() picks
#  the rows covering every pair.  Oracle: per figure the blip, \picw /
#  \pich, \picwgoal / \pichgoal, the \q before \pict, the number of
#  pictures and of \page, the title before and the footnote after its
#  picture with the footnote's \cellx the display width; print() of the
#  rtfplot.  FR-AS179 FR-AS180 FR-AS181 FR-AS185 FR-AS186 FR-AS187 FR-AS189
#  FR-AS191 FR-AS193 FR-AS194 FR-AS197 FR-AS198 FR-AS199 FR-AS201 FR-AS203
#  FR-AS204 FR-AS206 FR-AS214 FR-AS233 FR-AS239 FR-AS241 FR-AS248.
#
#  (B) FT-AS144, decision table: the display size against the native size.
#    width_twips  height_twips  display
#    NULL         NULL          native w x native h
#    w            NULL          w x round(w * px_h / px_w)
#    NULL         h             round(h * px_w / px_h) x h
#    w            h             w x h
#  x a file without a resolution (shown at the option
#  rtfreporter.figure.default_dpi: 96, then 192) x a plot drawn at 150 and at
#  600 dpi (the same inches).  FR-AS199 FR-AS200 FR-AS204.
# ---------------------------------------------------------------------------

.VF_FILE <- list(png = list(px = c(300L, 200L), dpi = 150, blip = "pngblip"),
                 jpeg = list(px = c(240L, 160L), dpi = 120, blip = "jpegblip"),
                 emf = list(px = c(10160L, 7620L), dpi = 2540, blip = "emfblip"))
.VF_STRIDE <- 41L

# The three files of the set, drawn once.
.vf_files <- function(dir) {
  draw <- function() grid::grid.rect(gp = grid::gpar(fill = "grey80"))
  out <- c(png = file.path(dir, "fig.png"), jpeg = file.path(dir, "fig.jpg"),
           emf = file.path(dir, "fig.emf"))
  grDevices::png(out[["png"]], width = 300, height = 200, res = 150)
  draw(); grDevices::dev.off()
  grDevices::jpeg(out[["jpeg"]], width = 240, height = 160, res = 120)
  draw(); grDevices::dev.off()
  if (requireNamespace("devEMF", quietly = TRUE)) {
    devEMF::emf(out[["emf"]], width = 4, height = 3)
    draw(); grDevices::dev.off()
  }
  out
}

.vf_design <- function() {
  lv <- list(source = c("png", "jpeg", "emf", "fn", "grob"),
             size = c("na", "default", "small"),
             dpi = c("na", "default", "72", "150"),
             device = c("na", "auto", "png", "ragg"),
             display = c("native", "width", "height", "both"),
             align = c("center", "left", "right"),
             via = c("rtfplot", "figures", "tables"),
             count = c("1", "2", "2flow"),
             blocks = c("none", "titles", "both"))
  full <- expand.grid(lv, stringsAsFactors = FALSE)
  file <- full$source %in% names(.VF_FILE)
  na <- full$size == "na" & full$dpi == "na" & full$device == "na"
  none <- full$size != "na" & full$dpi != "na" & full$device != "na"
  valid <- full[(file & na) | (!file & none), , drop = FALSE]
  rownames(valid) <- NULL
  every <- unique(unlist(lapply(utils::combn(names(lv), 2L, simplify = FALSE), function(q) {
    paste(q[1L], valid[[q[1L]]], q[2L], valid[[q[2L]]], sep = "|")
  })))
  cand <- valid[seq(1L, nrow(valid), by = .VF_STRIDE), , drop = FALSE]
  rownames(cand) <- NULL
  list(pw = .allpairs(cand), every = every, n_valid = nrow(valid))
}

# What one case should write: pixels, native and display twips, blip.
.vf_expect <- function(cs) {
  if (cs$source %in% names(.VF_FILE)) {
    f <- .VF_FILE[[cs$source]]
    px <- f$px; native <- as.integer(round(px / f$dpi * 1440)); blip <- f$blip
  } else {
    inch <- if (cs$size == "small") c(3, 2) else c(6.5, 4.5)
    dpi <- switch(cs$dpi, default = 300, as.numeric(cs$dpi))
    px <- as.integer(round(inch * dpi)); native <- as.integer(inch * 1440); blip <- "pngblip"
  }
  disp <- switch(cs$display,
                 native = native,
                 width = c(5000L, as.integer(round(5000 * px[2L] / px[1L]))),
                 height = c(as.integer(round(2000 * px[1L] / px[2L])), 2000L),
                 both = c(4000L, 1500L))
  list(px = px, disp = disp, blip = blip)
}

test_that("FT-AS143 [FR-AS248]: figures pairwise -- source x render size x dpi x device x display size x align x route x count x titles", {
  des <- .vf_design()
  expect_identical(des$n_valid, 12636L)
  expect_setequal(des$pw$need, des$every)
  expect_setequal(des$pw$covered, des$every)
  cases <- des$pw$rows
  expect_lte(nrow(cases), 40L)

  dir <- tempfile("vf")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  files <- .vf_files(dir)
  has_emf <- file.exists(files[["emf"]])
  ran <- 0L

  for (r in seq_len(nrow(cases))) {
    cs <- as.list(cases[r, , drop = FALSE])
    if (cs$source == "emf" && !has_emf) next   # devEMF not installed
    lab <- paste0("case ", r, " (", paste(names(cs), unlist(cs), sep = "=", collapse = ", "), ")")
    x <- switch(cs$source,
                fn = function() grid::grid.rect(gp = grid::gpar(fill = "grey80")),
                grob = grid::rectGrob(gp = grid::gpar(fill = "grey80")),
                files[[cs$source]])
    draw <- list()
    if (cs$size == "small") draw[c("render_width", "render_height")] <- list(3, 2)
    if (!cs$dpi %in% c("na", "default")) draw$render_dpi <- as.numeric(cs$dpi)
    if (cs$device != "na") draw$png_device <- cs$device
    disp_arg <- switch(cs$display, native = list(),
                       width = list(width_twips = 5000L),
                       height = list(height_twips = 2000L),
                       both = list(width_twips = 4000L, height_twips = 1500L))
    n <- if (cs$count == "1") 1L else 2L
    titles <- if (cs$blocks != "none") as.list(paste0("Figure 9.", seq_len(n), " Fictional"))
    footnotes <- if (cs$blocks == "both") as.list(paste0("Note ", seq_len(n), " fictional"))
    page_break <- if (cs$count == "2flow") c(TRUE, FALSE) else TRUE
    obj <- NULL
    doc <- if (cs$via == "figures") {
      do.call(rtf_figures, c(list(rtf_document(), rep(list(x), n), align = cs$align,
                                  titles = titles, footnotes = footnotes,
                                  page_break = page_break, space_before = 1L),
                             disp_arg, draw))
    } else {
      obj <- do.call(rtfplot, c(list(x, align = cs$align), disp_arg, draw))
      add <- if (cs$via == "tables") rtf_tables else rtf_figures
      add(rtf_document(), rep(list(obj), n), titles = titles, footnotes = footnotes,
          page_break = page_break, space_before = 1L)
    }
    path <- file.path(dir, "out.rtf")
    generate_rtfreport(doc, path, overwrite = TRUE)
    txt <- readLines(path, warn = FALSE)
    txt <- paste(txt[!grepl("^[0-9A-F]+$", txt)], collapse = "\n")   # the hex apart
    ex <- .vf_expect(cs)
    al <- c(center = "qc", left = "ql", right = "qr")[[cs$align]]
    pict <- sprintf("\\pard\\%s{\\pict\\%s\\picw%d\\pich%d\\picwgoal%d\\pichgoal%d",
                    al, ex$blip, ex$px[1L], ex$px[2L], ex$disp[1L], ex$disp[2L])
    at <- gregexpr(pict, txt, fixed = TRUE)[[1L]]
    expect_identical(sum(at > 0L), n, label = paste(lab, "pictures"))
    expect_identical(lengths(regmatches(txt, gregexpr("\\pict", txt, fixed = TRUE))), n,
                     label = lab)
    expect_identical(lengths(regmatches(txt, gregexpr("\\page", txt, fixed = TRUE))),
                     if (cs$count == "2") 1L else 0L, label = paste(lab, "pages"))
    if (cs$count == "2flow") {
      expect_true(grepl("{\\pard\\par}", txt, fixed = TRUE), label = paste(lab, "space"))
    }
    for (i in seq_len(n)) {
      p_at <- at[i]
      if (!is.null(titles)) {
        t_at <- regexpr(paste0("\\b ", titles[[i]], "\\b0"), txt, fixed = TRUE)
        expect_true(t_at > 0L && t_at < p_at && (i == 1L || t_at > at[i - 1L]),
                    label = paste(lab, "title", i))
      }
      if (!is.null(footnotes)) {
        f_at <- regexpr(sprintf("\\cellx%d\\ql\\li0\\ri0 %s\\cell", ex$disp[1L], footnotes[[i]]),
                        txt, fixed = TRUE)
        expect_true(f_at > p_at && (i == n || f_at < at[i + 1L]),
                    label = paste(lab, "footnote", i))
      }
    }
    if (!is.null(obj)) {
      out <- utils::capture.output(res <- print(obj))
      expect_identical(res, obj, label = lab)
      expect_true(any(grepl(sprintf("\\(%d twips\\) wide x .* \\(%d twips\\)", ex$disp[1L],
                                    ex$disp[2L]), out)), label = paste(lab, "print"))
      expect_true(any(grepl(paste0("Align: +", cs$align, "$"), out)), label = paste(lab, "print"))
      expect_true(any(grepl(paste0("^<rtfplot> ", toupper(sub("blip", "", ex$blip))), out)),
                  label = paste(lab, "print"))
    }
    ran <- ran + 1L
  }
  expect_gte(ran, nrow(cases) - sum(cases$source == "emf"))
})

test_that("FT-AS144 [FR-AS199 FR-AS200 FR-AS204]: figures decision table -- the display size against the native size", {
  dir <- tempfile("vf")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  # a PNG with no resolution: written by hand, no pHYs chunk
  png <- file.path(dir, "nodpi.png")
  grDevices::png(png, width = 400, height = 100)
  grid::grid.rect(); grDevices::dev.off()
  px <- c(400L, 100L)
  given <- list(none = list(), w = list(width_twips = 3000L), h = list(height_twips = 600L),
                both = list(width_twips = 3000L, height_twips = 900L))
  disp <- function(nat, g) {
    switch(g, none = nat, w = c(3000L, as.integer(round(3000 * px[2L] / px[1L]))),
           h = c(as.integer(round(600 * px[1L] / px[2L])), 600L), both = c(3000L, 900L))
  }
  old <- options(rtfreporter.figure.default_dpi = NULL)
  on.exit(options(old), add = TRUE)
  # the file's own dpi, when the device wrote one, else the option's
  own <- rtfplot(png)$dpi_x
  for (dpi in c(96, 192)) {
    options(rtfreporter.figure.default_dpi = dpi)
    use <- if (is.null(own) || is.na(own)) dpi else own
    nat <- as.integer(round(px / use * 1440))
    for (g in names(given)) {
      o <- do.call(rtfplot, c(list(png), given[[g]]))
      d <- .rtfplot_display_twips(o)
      expect_identical(c(d$w, d$h), disp(nat, g), label = paste("file", dpi, g))
    }
  }
  options(old)
  # a plot object: the same inches at 150 and at 600 dpi
  for (dpi in c(150, 600)) {
    for (g in names(given)) {
      o <- do.call(rtfplot, c(list(grid::rectGrob(), render_width = 2, render_height = 0.5,
                                   render_dpi = dpi), given[[g]]))
      expect_identical(c(o$img_width, o$img_height), as.integer(c(2, 0.5) * dpi))
      d <- .rtfplot_display_twips(o)
      expect_identical(c(d$w, d$h), disp(c(2880L, 720L), g), label = paste("plot", dpi, g))
    }
  }
})

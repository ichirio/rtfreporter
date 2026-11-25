# The two examples in inst/rtf-examples/ that a person opened in Word, built
# through the public functions only (#1071).  The generators in data-raw/
# source this file and write the shipped files; a test rebuilds each into a
# temporary file and compares it with the shipped one, so a change that
# alters a checked file fails.
#
#   .wck_watermark_doc()         inst/rtf-examples/watermark-example.rtf
#       (data-raw/gen_watermark_example.R; checked in Word 2026-11-13)
#   .wck_figure_png(pngfile)     the plot the figure example embeds
#   .wck_figure_doc(pngfile)     inst/rtf-examples/figure-example.rtf
#       (data-raw/gen_figure_example.R; checked in Word 2026-11-13)
#
# Sourced by the data-raw scripts from the package root, so it defines
# functions only and needs nothing from testthat.

# Runs `code` after set.seed(seed) and puts the caller's random state back, so
# a test that builds an example leaves the other tests' random numbers alone.
.wck_seeded <- function(seed, code) {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had) old <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  on.exit({
    if (had) {
      assign(".Random.seed", old, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)
  set.seed(seed)
  force(code)
}

# ---- the watermark example ------------------------------------------------------

# The PK concentration table: the pages and the running header.  A copy of
# data-raw/gen_pk_conc.R (the original, which writes
# inst/rtf-examples/pk-concentration.rtf) up to the line that builds its
# document; keep the two the same.  `span` is the label of the spanning
# column header.
.wck_pk_parts <- function(span = "Plasma Concentration (ng/mL)") {
  visits <- c("Day 1", "Day 7", "Day 14", "Day 28", "Day 42", "Day 56",
              "Day 70", "Day 84", "Day 112", "Day 140", "Day 168", "Day 196")
  times  <- c(0.5, 1, 2, 4, 8, 12, 24)
  lloq   <- 0.500

  # one-compartment, first-order absorption, mild accumulation over visits
  conc <- function(t, visit_i) {
    ka <- 1.2; ke <- 0.18
    shape <- exp(-ke * t) - exp(-ka * t)
    peak  <- max(exp(-ke * times) - exp(-ka * times))
    1500 * (1 + 0.06 * (visit_i - 1L)) * shape / peak
  }
  # three significant figures
  fmt <- function(x) {
    if (is.na(x)) return("")
    if (x >= 1000) sprintf("%.1f", x)
    else if (x >= 100) sprintf("%.2f", x)
    else if (x >= 10)  sprintf("%.3f", x)
    else               sprintf("%.4f", x)
  }
  cell <- function(stat, t, visit_i) {
    m <- conc(t, visit_i)
    if (t == 24 && visit_i == 1L) {
      return(if (stat == "n") "24" else if (stat == "Min, Max") "BLQ, BLQ" else "BLQ")
    }
    if (t == 24 && visit_i == 2L && stat %in% c("Mean", "Median")) {
      return(sprintf("<%.3f", lloq))
    }
    switch(stat,
      "n"        = "24",
      "Mean"     = fmt(m),
      "SD"       = fmt(m * runif(1L, 0.14, 0.26)),
      "CV%"      = sprintf("%.1f", runif(1L, 14, 26)),
      "Median"   = fmt(m * runif(1L, 0.94, 1.06)),
      "Min, Max" = paste(fmt(m * 0.55), fmt(m * 1.62), sep = ", "),
      ""
    )
  }
  stats <- c("n", "Mean", "SD", "CV%", "Median", "Min, Max")

  pk <- .wck_seeded(277L, {
    rows <- list()
    for (t in times) {
      rows[[length(rows) + 1L]] <- c(sprintf("%g h", t), rep("", length(visits)))
      for (s in stats) {
        rows[[length(rows) + 1L]] <- c(
          paste0("  ", s),
          vapply(seq_along(visits), function(v) cell(s, t, v), character(1L))
        )
      }
    }
    as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  })
  names(pk) <- c("Nominal Time (h)", visits)

  widths <- auto_col_widths(pk, col_header = names(pk))
  half <- length(visits) / 2L

  pages <- as_rtftables(
      pk,
      split      = "group_safe",
      group_by   = "indent",
      max_rows   = 21L,
      blank_rows = blank_rows_by_change("Nominal Time (h)", group_by = "indent"),
      blank_row_first = TRUE,
      blank_row_end   = TRUE,
      col_header = list(
        list(col_cell(c(2, length(visits) + 1L), span)),
        c("Nominal Time (h)", visits)
      ),
      column_widths_twips = widths,
      border = "tfl"
    ) |>
    set_decimal_split(cols = visits) |>
    paginate_cols(at = half + 2L, order = "down")

  pk_header <- rtf_header(rows = list(
    c(l = "Drug Co., Ltd.",    r = "CONFIDENTIAL"),
    c(l = "Protocol: RTF-101", r = "Page {PAGE} of {TOTAL_PAGES}"),
    c(c = "Table 14.2.1"),
    c(c = "Summary of Plasma Concentrations by Nominal Time and Visit"),
    c(c = "<Pharmacokinetic Analysis Set>")
  ))
  list(pages = pages, header = pk_header)
}

# The PK table on a document with a DRAFT watermark, lambda_z and the
# half-life in its spanning header, footnotes with the characters a report
# uses beyond ASCII and a Japanese line in the east_asian font slot.
.wck_watermark_doc <- function() {
  pk <- .wck_pk_parts(span = paste0("Plasma Concentration (ng/mL)",
                                    ";  \u03bb_{z} = 0.18 h^{-1},  t\u00bd = 3.8 h"))
  fn_chars <- c(
    paste0("C_{max} = maximum concentration (\u03bcg/mL = 1000 ng/mL); ",
           "AUC in ng\u00b7h/mL; \u03bb_{z} = terminal rate constant; ",
           "t\u00bd = half-life."),
    paste0("Mean \u00b1 SD.  BLQ = below the limit of quantitation ",
           "(\u2264 0.500 ng/mL); subjects with compliance \u2265 80%.  ",
           "Samples assayed at 25 \u00b0C."),
    paste0("\u6ce8: \u6570\u5024\u306f\u67b6\u7a7a\u306e\u30c7\u30fc\u30bf",
           "\u3067\u3059\u3002 (Japanese note)")
  )
  footnotes <- lapply(pk$pages, function(p) fn_chars)

  # Courier New for Latin text, MS Gothic for Japanese
  doc <- rtf_document(
      page = rtf_page(orientation = "landscape"),
      font_table = list(list(name = c(latin = "Courier New", east_asian = "MS Gothic"))),
      font_lang = c(east_asian = "ja"),
      watermark = rtf_watermark("DRAFT")) |>
    rtf_section(page = 1, secinfo = list(header = pk$header))
  for (p in pk$pages) doc <- rtf_tables(doc, p)
  rtf_footnotes(doc, footnotes)
}

# ---- the figure example ---------------------------------------------------------

# Draws the plot (mean change from baseline in systolic blood pressure by
# visit, fictional data) into `pngfile`: 2400 x 1500 px at 300 dpi = 8 x 5 in,
# base graphics.  Returns `pngfile`.
.wck_figure_png <- function(pngfile) {
  visits <- c("2", "4", "8", "12", "16", "24")
  wk     <- c(2, 4, 8, 12, 16, 24)
  d <- .wck_seeded(968L, {
    mean_p <- -1.0 - 3.2 * (1 - exp(-wk / 6)) + rnorm(length(wk), 0, 0.25)
    mean_d <- -1.5 - 11.5 * (1 - exp(-wk / 7)) + rnorm(length(wk), 0, 0.35)
    sem_p  <- runif(length(wk), 1.3, 1.9)
    sem_d  <- runif(length(wk), 1.2, 1.8)
    list(mean_p = mean_p, mean_d = mean_d, sem_p = sem_p, sem_d = sem_d)
  })

  grDevices::png(pngfile, width = 2400, height = 1500, res = 300)
  on.exit(grDevices::dev.off(), add = TRUE)
  op <- graphics::par(mar = c(4.2, 4.6, 0.8, 0.8), cex = 0.9)
  on.exit(graphics::par(op), add = TRUE, after = FALSE)
  yl <- range(c(d$mean_p - d$sem_p, d$mean_p + d$sem_p,
                d$mean_d - d$sem_d, d$mean_d + d$sem_d))
  graphics::plot(NA, xlim = c(1, 25), ylim = c(floor(yl[1]) - 1, ceiling(yl[2]) + 1),
                 xaxt = "n", xlab = "Visit (week)",
                 ylab = "Mean change from baseline in SBP (mmHg)", las = 1)
  graphics::axis(1, at = wk, labels = visits)
  graphics::abline(h = 0, lty = 3, col = "grey50")
  dodge <- 0.25
  for (s in list(list(m = d$mean_p, e = d$sem_p, x = wk - dodge, col = "grey35", pch = 16L),
                 list(m = d$mean_d, e = d$sem_d, x = wk + dodge, col = "black", pch = 17L))) {
    graphics::arrows(s$x, s$m - s$e, s$x, s$m + s$e, angle = 90, code = 3,
                     length = 0.04, col = s$col)
    graphics::lines(s$x, s$m, col = s$col, lty = if (s$pch == 16L) 2L else 1L)
    graphics::points(s$x, s$m, pch = s$pch, col = s$col, cex = 1.2)
  }
  graphics::legend("bottomleft", c("Placebo (N=86)", "Drug 10 mg (N=84)"),
                   pch = c(16L, 17L), lty = c(2L, 1L), col = c("grey35", "black"),
                   bty = "n")
  invisible(pngfile)
}

# The figure page: the 8 x 5 in picture centred on a landscape letter page,
# a running header, a footer, three title lines and two footnote lines.
.wck_figure_doc <- function(pngfile) {
  fig_header <- rtf_header(rows = list(
    c(l = "Drug Co., Ltd.",    r = "CONFIDENTIAL"),
    c(l = "Protocol: RTF-101", r = "Page {PAGE} of {TOTAL_PAGES}")
  ))
  fig_footer <- rtf_footer(c(l = "Figure 14.2.1 - fictional data, for layout review"))

  rtf_document(page = rtf_page(orientation = "landscape")) |>
    rtf_section(page = 1, secinfo = list(header = fig_header, footer = fig_footer)) |>
    rtf_figures(
      list(rtfplot(pngfile, width_twips = 11520L, height_twips = 7200L,
                   align = "center")),
      titles = c("Figure 14.2.1",
                 "Mean Change from Baseline in Systolic Blood Pressure by Visit",
                 "Safety Population"),
      footnotes = c("Error bars show the standard error of the mean.",
                    "Source: data-raw/gen_figure_example.R (fictional data)"))
}

# ---- comparing ------------------------------------------------------------------

# The file's text with the CRs removed and the hex data of every \pict group
# replaced by "<pict data>": the picture's control words (\pngblip, \picw,
# \pich, \picwgoal, \pichgoal) stay, only the bytes the device wrote go.
.wck_blank_pict <- function(path) {
  x <- readBin(path, "raw", file.size(path))
  x <- rawToChar(x[x != as.raw(13L)])
  gsub("(\\\\pict(?:\\\\[a-z]+-?[0-9]*)*\\s*)[0-9A-Fa-f\n]+\\}",
       "\\1<pict data>}", x, perl = TRUE, useBytes = TRUE)
}

# The shipped file inst/rtf-examples/<name>, installed or in the source tree.
.wck_example <- function(name) {
  f <- system.file("rtf-examples", name, package = "rtfreporter")
  if (!nzchar(f)) f <- test_path("..", "..", "inst", "rtf-examples", name)
  f
}

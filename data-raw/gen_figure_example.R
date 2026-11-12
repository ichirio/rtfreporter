# ============================================================================
#  A figure page to judge in Word -- mean change from baseline by visit
# ============================================================================
#
#  The structure fixture tests/testthat/fixtures/validation/figure.rtf embeds a
#  1x1 PNG, so a person cannot judge it.  This example draws a real plot with
#  base graphics (fictional data) at 8 x 5 inches and 300 dpi, and puts it on
#  a landscape letter page with a running header, a footer, three title lines
#  and two footnote lines, the figure centred.  The titles and footnotes follow
#  the figure's placement (#968): they sit centred under / above it.
#
#  Writes inst/rtf-examples/figure-example.rtf (the plot is drawn into a
#  temporary PNG that the RTF embeds).  Run from the package root:
#      Rscript data-raw/gen_figure_example.R

source("data-raw/_load.R")

outfile <- "inst/rtf-examples/figure-example.rtf"
pngfile <- tempfile("figure-example-", fileext = ".png")

# -- fictional data: mean change from baseline in systolic blood pressure ----
set.seed(968)
visits <- c("2", "4", "8", "12", "16", "24")
wk     <- c(2, 4, 8, 12, 16, 24)
mean_p <- -1.0 - 3.2 * (1 - exp(-wk / 6)) + rnorm(length(wk), 0, 0.25)
mean_d <- -1.5 - 11.5 * (1 - exp(-wk / 7)) + rnorm(length(wk), 0, 0.35)
sem_p  <- runif(length(wk), 1.3, 1.9)
sem_d  <- runif(length(wk), 1.2, 1.8)

# -- the plot: 2400 x 1500 px at 300 dpi = 8 x 5 in ---------------------------
grDevices::png(pngfile, width = 2400, height = 1500, res = 300)
op <- graphics::par(mar = c(4.2, 4.6, 0.8, 0.8), cex = 0.9)
yl <- range(c(mean_p - sem_p, mean_p + sem_p, mean_d - sem_d, mean_d + sem_d))
graphics::plot(NA, xlim = c(1, 25), ylim = c(floor(yl[1]) - 1, ceiling(yl[2]) + 1),
               xaxt = "n", xlab = "Visit (week)",
               ylab = "Mean change from baseline in SBP (mmHg)", las = 1)
graphics::axis(1, at = wk, labels = visits)
graphics::abline(h = 0, lty = 3, col = "grey50")
dodge <- 0.25
for (s in list(list(m = mean_p, e = sem_p, x = wk - dodge, col = "grey35", pch = 16L),
               list(m = mean_d, e = sem_d, x = wk + dodge, col = "black", pch = 17L))) {
  graphics::arrows(s$x, s$m - s$e, s$x, s$m + s$e, angle = 90, code = 3,
                   length = 0.04, col = s$col)
  graphics::lines(s$x, s$m, col = s$col, lty = if (s$pch == 16L) 2L else 1L)
  graphics::points(s$x, s$m, pch = s$pch, col = s$col, cex = 1.2)
}
graphics::legend("bottomleft", c("Placebo (N=86)", "Drug 10 mg (N=84)"),
                 pch = c(16L, 17L), lty = c(2L, 1L), col = c("grey35", "black"),
                 bty = "n")
graphics::par(op)
grDevices::dev.off()

# -- the page: 8 x 5 in picture, centred --------------------------------------
fig_header <- rtf_header(rows = list(
  c(l = "Drug Co., Ltd.",    r = "CONFIDENTIAL"),
  c(l = "Protocol: RTF-101", r = "Page {PAGE} of {TOTAL_PAGES}")
))
fig_footer <- rtf_footer(c(l = "Figure 14.2.1 - fictional data, for layout review"))

doc <- rtf_document(page = rtf_page(orientation = "landscape")) |>
  rtf_section(page = 1, secinfo = list(header = fig_header, footer = fig_footer)) |>
  rtf_figures(
    list(rtfplot(pngfile, width_twips = 11520L, height_twips = 7200L,
                 align = "center")),
    titles = c("Figure 14.2.1",
               "Mean Change from Baseline in Systolic Blood Pressure by Visit",
               "Safety Population"),
    footnotes = c("Error bars show the standard error of the mean.",
                  "Source: data-raw/gen_figure_example.R (fictional data)"))

generate_rtfreport(doc, outfile, overwrite = TRUE)
message(sprintf("%s: %.0f KB", outfile, file.size(outfile) / 1024))

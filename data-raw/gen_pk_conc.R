# ============================================================================
#  PK concentration summary -- visits across the columns
# ============================================================================
#
#  A pharmacokinetic concentration table is the natural case for all three
#  features at once:
#
#    * every cell is a concentration, and PK reports carry three significant
#      figures, so the decimal places differ row by row (1234.5 / 456.78 /
#      89.012 / 5.0123).  set_decimal_split() renders each visit column as TWO
#      cells so the points line up whatever the font.
#    * the visits run ACROSS the columns, so the table is genuinely too wide
#      for one page -> paginate_cols().
#    * the nominal time points make it too tall as well -> row pagination.
#
#  Writes inst/rtf-examples/pk-concentration.rtf.  Run from the package root:
#      Rscript data-raw/gen_pk_conc.R

library(rtfreporter)

outfile <- "inst/rtf-examples/pk-concentration.rtf"

# ── the analysis data ──────────────────────────────────────────────────────

# Twelve visits, because the point of the example is a table that genuinely
# does NOT fit: at the width its own content needs (see below) only six visits
# fit on a landscape sheet, so the full table is 1.73x too wide and each half
# fills 94% of the page.
VISITS <- c("Day 1", "Day 7", "Day 14", "Day 28", "Day 42", "Day 56",
            "Day 70", "Day 84", "Day 112", "Day 140", "Day 168", "Day 196")
TIMES  <- c(0.5, 1, 2, 4, 8, 12, 24)
LLOQ   <- 0.500

# One-compartment, first-order absorption, with mild accumulation over visits.
.conc <- function(t, visit_i) {
  ka <- 1.2; ke <- 0.18
  shape <- exp(-ke * t) - exp(-ka * t)
  peak  <- max(exp(-ke * TIMES) - exp(-ka * TIMES))
  1500 * (1 + 0.06 * (visit_i - 1L)) * shape / peak
}

# PK convention: three significant figures, so the decimal count varies with
# the magnitude -- exactly what the decimal split is for.
.fmt <- function(x) {
  if (is.na(x)) return("")
  if (x >= 1000) sprintf("%.1f", x)
  else if (x >= 100) sprintf("%.2f", x)
  else if (x >= 10)  sprintf("%.3f", x)
  else               sprintf("%.4f", x)
}

set.seed(277)

.cell <- function(stat, t, visit_i) {
  m <- .conc(t, visit_i)
  # Late samples at the first visit are still below the limit of quantitation;
  # one cell sits just under the LLOQ.  Both exercise the non-numeric and the
  # relational-prefix branches of the split.
  if (t == 24 && visit_i == 1L) {
    return(if (stat == "n") "24" else if (stat == "Min, Max") "BLQ, BLQ" else "BLQ")
  }
  if (t == 24 && visit_i == 2L && stat %in% c("Mean", "Median")) {
    return(sprintf("<%.3f", LLOQ))
  }
  switch(stat,
    "n"        = "24",
    "Mean"     = .fmt(m),
    "SD"       = .fmt(m * runif(1L, 0.14, 0.26)),
    "CV%"      = sprintf("%.1f", runif(1L, 14, 26)),
    "Median"   = .fmt(m * runif(1L, 0.94, 1.06)),
    "Min, Max" = paste(.fmt(m * 0.55), .fmt(m * 1.62), sep = ", "),
    ""
  )
}

STATS <- c("n", "Mean", "SD", "CV%", "Median", "Min, Max")

rows <- list()
for (t in TIMES) {
  # An unindented label opens a group; the indented statistics belong to it.
  # That is the convention `group_by = "indent"` detects, so a time point is
  # never split across a row page.
  rows[[length(rows) + 1L]] <-
    c(sprintf("%g h", t), rep("", length(VISITS)))
  for (s in STATS) {
    rows[[length(rows) + 1L]] <- c(
      paste0("  ", s),
      vapply(seq_along(VISITS), function(v) .cell(s, t, v), character(1L))
    )
  }
}

pk <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
names(pk) <- c("Nominal Time (h)", VISITS)

# ── size the columns from the content ──────────────────────────────────────
#
# ABSOLUTE widths, not `col_rel_width`: relative widths are normalised across
# the page by `.compute_cellx()`, so a table sized that way is always exactly
# as wide as the sheet and can never be too wide to fit -- which would leave
# nothing for `paginate_cols()` to do.  `auto_col_widths()` reports what the
# cells actually need instead:
#
#     stub "Nominal Time (h)"   2021 twips   (widest cell "Min, Max")
#     each visit column         1804 twips   (widest cell "496.28, 1461.8")
#
# so only six visits fit on a landscape sheet's 13680 writable twips.  With
# twelve the table is 23669 twips -- 1.73x too wide -- and each half comes to
# 12845, or 94% of the page.
#
# `as_rtftables(auto_width = TRUE)` is deliberately NOT used: it clamps the
# total to the writable width when the natural size exceeds it, squeezing the
# columns back onto one page.
widths <- auto_col_widths(pk, col_header = names(pk))

HALF <- length(VISITS) / 2L        # cut the visits down the middle

# ── build, then split on BOTH axes ─────────────────────────────────────────

pages <- as_rtftables(
    pk,
    split      = "group_safe",     # never cut a time point in half
    group_by   = "indent",
    max_rows   = 21L,              # 3 time points per row page
    blank_rows = blank_rows_by_change("Nominal Time (h)",
                                      group_by = "indent"),
    col_header = list(
      list(col_cell(c(2, length(VISITS) + 1L),
                    "Plasma Concentration (ng/mL)")),
      c("Nominal Time (h)", VISITS)
    ),
    column_widths_twips = widths,
    border = "tfl"
  ) |>
  # one column of two cells: integer part right-aligned, decimals left-aligned
  set_decimal_split(cols = VISITS) |>
  # then cut the visits in half, repeating the time-point stub on both pages.
  # A column keeps its width whatever page it lands on, so each block is the
  # stub plus six visits = 12845 twips, 94% of the sheet.
  paginate_cols(at = HALF + 2L)

titles <- lapply(seq_along(pages), function(i)
  c("Table 14.2.1",
    "Summary of Plasma Concentrations by Nominal Time and Visit",
    "Pharmacokinetic Analysis Set"))

footnotes <- lapply(seq_along(pages), function(i)
  c("CV% = coefficient of variation.  BLQ = below the limit of quantitation",
    sprintf("(LLOQ = %.3f ng/mL).  Concentrations are summarised to three", LLOQ),
    "significant figures; the decimal points are aligned by column splitting."))

doc <- rtf_document(page = rtf_page(orientation = "landscape"))
for (p in pages) doc <- rtf_tables(doc, p)
doc <- rtf_titles(doc, titles)
doc <- rtf_footnotes(doc, footnotes)

generate_rtfreport(doc, outfile, overwrite = TRUE)

n_blocks <- length(unique(vapply(pages, function(p)
  paste(names(p$data)[-1L], collapse = "|"), character(1L))))
message(sprintf("%s: %d pages (%d row bands x %d column blocks)",
                outfile, length(pages), length(pages) / n_blocks, n_blocks))

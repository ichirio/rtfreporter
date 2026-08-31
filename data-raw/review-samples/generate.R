# rtfreporter -- review samples
#
# Each of the five reports is built TWICE, with the SAME full feature set:
# once with as_rtftables(), once with rtf_plan() + layers.  Earlier versions of
# this file compared one call with one call, which could only ever show a
# rename; a full-set table is where "one big function" and "small layers"
# actually differ.
#
# Writes, per case:  <id>_old.rtf, <id>_new.rtf
# and 00_code.R with both spellings side by side.

setwd("C:/Users/ichir/AppData/Local/Temp/claude/C--Users-ichir/0e8953d4-534f-4543-9b50-71c7b61ba96a/scratchpad/plan-wt")
suppressMessages(pkgload::load_all(".", quiet = TRUE))

OUT <- "data-raw/review-samples"

# ---------------------------------------------------------------- data ----
dd <- system.file("extdata", package = "rtfreporter")

## DM -- demographics.  A hidden ORD column carries the shell's row order,
## which is not the alphabetical one.
dm <- readRDS(file.path(dd, "demog_p1.rds"))
dm <- cbind(ORD = seq_len(nrow(dm)), dm)

## AE -- SOC / PT with a hidden SOCORD (medical order, not alphabetical) and
## delimited column names that encode a spanning header.
set.seed(11)
soc <- c("Cardiac disorders", "Gastrointestinal disorders",
         "Nervous system disorders", "Infections and infestations")
pts <- list(c("Atrial fibrillation", "Bradycardia", "Tachycardia"),
            c("Nausea", "Vomiting", "Diarrhoea", "Constipation"),
            c("Headache", "Dizziness"),
            c("Nasopharyngitis", "Urinary tract infection", "Pneumonia"))
socord <- c(3L, 1L, 4L, 2L)
ae <- do.call(rbind, lapply(seq_along(soc), function(i) {
  n <- length(pts[[i]])
  data.frame(SOCORD = socord[i], SOC = soc[i], PT = pts[[i]],
             `Drug A____n (%)`  = sprintf("%d (%.1f%%)", sample(1:12, n), runif(n, 1, 9)),
             `Drug A____Events` = as.character(sample(1:20, n)),
             `Drug B____n (%)`  = sprintf("%d (%.1f%%)", sample(1:12, n), runif(n, 1, 9)),
             `Drug B____Events` = as.character(sample(1:20, n)),
             stringsAsFactors = FALSE, check.names = FALSE)
}))

## PK -- Time / Statistic stub, visits across the columns.
VIS   <- c("Day 1", "Day 7", "Day 14", "Day 28")
TIMES <- c(0.5, 1, 2, 4, 8, 12, 24)
STATS <- c("n", "Mean", "SD", "CV%", "Median", "Min, Max")
set.seed(277)
rows <- list()
for (t in TIMES) {
  rows[[length(rows) + 1L]] <- c(sprintf("%g h", t), "", rep("", length(VIS)))
  for (s in STATS) {
    rows[[length(rows) + 1L]] <- c(sprintf("%g h", t), s,
      vapply(seq_along(VIS), function(v) sprintf("%.2f", runif(1, 1, 2000)),
             character(1L)))
  }
}
pk <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
names(pk) <- c("Time", "Statistic", VIS)

## LB -- shift table.  PARAMCD groups the table but is never printed.
lb <- readRDS(file.path(dd, "lab_alt.rds"))
lb$PARAMCD <- "ALT"
lb <- lb[, c("PARAMCD", setdiff(names(lb), "PARAMCD"))]

# --------------------------------------------------------------- specs ----
widths_ae <- c(4200L, 1500L, 1200L, 1500L, 1200L)
widths_dm <- c(3600L, 1800L, 1800L, 1800L)

cases <- list(

  list(id = "01_DM",
       title = "DM  Demographics -- hidden sort carrier, spanning header, grouped blanks, group-safe pages",
       old_src = '
pages <- as_rtftables(
  dm,
  sort_by             = "ORD",
  drop_cols           = "ORD",
  group_col           = "Label",
  blank_rows          = "between_groups",
  count_blank_rows    = TRUE,
  split               = "group_safe",
  max_rows            = 10,
  border              = "tfl",
  column_widths_twips = widths_dm,
  # PRE-drop coordinates: ORD is still column 1 here even though drop_cols
  # removes it, so the treatment columns are 3, 4, 5 -- not the 2, 3, 4 they
  # occupy in the printed table.
  col_spec            = list(list(col = 3, align = "center"),
                             list(col = 4, align = "center"),
                             list(col = 5, align = "center"))
) |>
  set_col_header(rtf_col_header(
    list(col_cell(1, ""), col_cell(c(2, 3), "Treatment Group"), col_cell(4, "")),
    c("Characteristic", "Drug A", "Drug B", "Total")))',
       new_src = '
pages <- rtf_plan(dm) |>
  plan_roles(ORD    = role("sort", order = 1),
             `Drug A` = role("display", align = "center"),
             `Drug B` = role("display", align = "center"),
             Total    = role("display", align = "center")) |>
  plan_hide("ORD") |>
  plan_group("Label") |>
  plan_blanks("between_groups") |>
  plan_pages(max_rows = 10, count_blanks = TRUE) |>
  plan_header(list(col_cell(2, ""), col_cell(c(3, 4), "Treatment Group"),
                   col_cell(5, "")),
              c("ORD", "Characteristic", "Drug A", "Drug B", "Total")) |>
  plan_style(border = "tfl", widths = widths_dm) |>
  rtf_pages()',
       old = function() {
         p <- as_rtftables(dm, sort_by = "ORD", drop_cols = "ORD",
                           group_col = "Label", blank_rows = "between_groups",
                           count_blank_rows = TRUE, split = "group_safe",
                           max_rows = 10L, border = "tfl",
                           column_widths_twips = widths_dm,
                           col_spec = list(list(col = 3, align = "center"),
                                           list(col = 4, align = "center"),
                                           list(col = 5, align = "center")))
         set_col_header(p, rtf_col_header(
           list(col_cell(1, ""), col_cell(c(2, 3), "Treatment Group"),
                col_cell(4, "")),
           c("Characteristic", "Drug A", "Drug B", "Total")))
       },
       new = function() {
         rtf_plan(dm) |>
           plan_roles(ORD = role("sort", order = 1),
                      `Drug A` = role("display", align = "center"),
                      `Drug B` = role("display", align = "center"),
                      Total    = role("display", align = "center")) |>
           plan_hide("ORD") |>
           plan_group("Label") |>
           plan_blanks("between_groups") |>
           plan_pages(max_rows = 10L, count_blanks = TRUE) |>
           plan_header(list(col_cell(2, ""), col_cell(c(3, 4), "Treatment Group"),
                            col_cell(5, "")),
                       c("ORD", "Characteristic", "Drug A", "Drug B", "Total")) |>
           plan_style(border = "tfl", widths = widths_dm) |>
           rtf_pages()
       }),

  list(id = "02_AE",
       title = "AE  Adverse events -- SOC/PT stub with a custom label, hidden sort carrier, spanning header, grouped blanks",
       old_src = '
# sort_by cannot reach PT: the stub merged it away, and every column argument
# runs in POST-stub coordinates.  So the ordering happens outside the call.
ae_sorted <- ae[order(ae$SOCORD, ae$PT), ]

pages <- as_rtftables(
  ae_sorted,
  stub_vars           = c("SOC", "PT"),
  stub_label          = "System Organ Class / Preferred Term",
  drop_cols           = "SOCORD",
  group_col           = "System Organ Class / Preferred Term",  # the stub_label
  group_by            = "indent",
  blank_rows          = "between_groups",
  count_blank_rows    = TRUE,
  split               = "group_safe",
  max_rows            = 12,
  border              = "tfl",
  column_widths_twips = widths_ae,
  # A THIRD coordinate system: post-stub but PRE-drop.  The Events columns
  # are source 5 and 7, final 3 and 5, and neither of those is what goes here.
  col_spec            = list(list(col = 4, align = "right"),
                             list(col = 6, align = "right"))
)',
       new_src = '
pages <- rtf_plan(ae) |>
  plan_stub(c("SOC", "PT"), label = "System Organ Class / Preferred Term") |>
  plan_roles(SOCORD = role("sort", order = 1), PT = role("sort", order = 2),
             `Drug A____Events` = role("display", align = "right"),
             `Drug B____Events` = role("display", align = "right")) |>
  plan_hide("SOCORD") |>
  plan_group("SOC", mode = "indent") |>
  plan_blanks("between_groups") |>
  plan_pages(max_rows = 12, count_blanks = TRUE) |>
  plan_style(border = "tfl", widths = widths_ae) |>
  rtf_pages()',
       old = function() {
         ae_sorted <- ae[order(ae$SOCORD, ae$PT), ]
         as_rtftables(ae_sorted, stub_vars = c("SOC", "PT"),
                      stub_label = "System Organ Class / Preferred Term",
                      drop_cols = "SOCORD",
                      group_col = "System Organ Class / Preferred Term",
                      group_by = "indent", blank_rows = "between_groups",
                      count_blank_rows = TRUE, split = "group_safe",
                      max_rows = 12L, border = "tfl",
                      column_widths_twips = widths_ae,
                      col_spec = list(list(col = 4, align = "right"),
                                      list(col = 6, align = "right")))
       },
       new = function() {
         rtf_plan(ae) |>
           plan_stub(c("SOC", "PT"),
                     label = "System Organ Class / Preferred Term") |>
           plan_roles(SOCORD = role("sort", order = 1),
                      PT = role("sort", order = 2),
                      `Drug A____Events` = role("display", align = "right"),
                      `Drug B____Events` = role("display", align = "right")) |>
           plan_hide("SOCORD") |>
           plan_group("SOC", mode = "indent") |>
           plan_blanks("between_groups") |>
           plan_pages(max_rows = 12L, count_blanks = TRUE) |>
           plan_style(border = "tfl", widths = widths_ae) |>
           rtf_pages()
       }),

  list(id = "03_PK",
       title = "PK  Concentrations -- Time/Statistic stub, visits across the columns, spanning header, grouped blanks",
       old_src = '
pages <- as_rtftables(
  pk,
  stub_vars        = c("Time", "Statistic"),
  group_col        = "Time / Statistic",     # the generated stub name
  group_by         = "indent",
  blank_rows       = "between_groups",
  count_blank_rows = TRUE,
  split            = "group_safe",
  max_rows         = 21,
  border           = "tfl",
  col_spec         = list(list(col = 2, align = "right"),
                          list(col = 3, align = "right"),
                          list(col = 4, align = "right"),
                          list(col = 5, align = "right"))
) |>
  set_col_header(rtf_col_header(
    list(col_cell(1, ""), col_cell(c(2, 5), "Visit")),
    c("Time / Statistic", "Day 1", "Day 7", "Day 14", "Day 28")))',
       new_src = '
pages <- rtf_plan(pk) |>
  plan_stub(c("Time", "Statistic")) |>
  plan_roles(`Day 1` = role("display", align = "right"),
             `Day 7` = role("display", align = "right"),
             `Day 14` = role("display", align = "right"),
             `Day 28` = role("display", align = "right")) |>
  plan_group("Time", mode = "indent") |>
  plan_blanks("between_groups") |>
  plan_pages(max_rows = 21, count_blanks = TRUE) |>
  plan_header(list(col_cell(c(1, 2), ""), col_cell(c(3, 6), "Visit")),
              c("Time", "Statistic", "Day 1", "Day 7", "Day 14", "Day 28")) |>
  plan_style(border = "tfl") |>
  rtf_pages()',
       old = function() {
         p <- as_rtftables(pk, stub_vars = c("Time", "Statistic"),
                           group_col = "Time / Statistic", group_by = "indent",
                           blank_rows = "between_groups",
                           count_blank_rows = TRUE, split = "group_safe",
                           max_rows = 21L, border = "tfl",
                           col_spec = list(list(col = 2, align = "right"),
                                           list(col = 3, align = "right"),
                                           list(col = 4, align = "right"),
                                           list(col = 5, align = "right")))
         set_col_header(p, rtf_col_header(
           list(col_cell(1, ""), col_cell(c(2, 5), "Visit")),
           c("Time / Statistic", "Day 1", "Day 7", "Day 14", "Day 28")))
       },
       new = function() {
         rtf_plan(pk) |>
           plan_stub(c("Time", "Statistic")) |>
           plan_roles(`Day 1`  = role("display", align = "right"),
                      `Day 7`  = role("display", align = "right"),
                      `Day 14` = role("display", align = "right"),
                      `Day 28` = role("display", align = "right")) |>
           plan_group("Time", mode = "indent") |>
           plan_blanks("between_groups") |>
           plan_pages(max_rows = 21L, count_blanks = TRUE) |>
           plan_header(list(col_cell(c(1, 2), ""), col_cell(c(3, 6), "Visit")),
                       c("Time", "Statistic", "Day 1", "Day 7", "Day 14",
                         "Day 28")) |>
           plan_style(border = "tfl") |>
           rtf_pages()
       }),

  list(id = "04_LB",
       title = "LB  Shift table -- hidden grouping carrier, three-arm spanning header, grouped blanks",
       old_src = '
pages <- as_rtftables(
  lb,
  group_col        = "PARAMCD",
  drop_cols        = "PARAMCD",
  blank_rows       = "between_groups",
  count_blank_rows = TRUE,
  border           = "tfl"
) |>
  set_col_header(rtf_col_header(
    list(col_cell(1, ""), col_cell(c(2, 7), "Drug A"),
         col_cell(c(8, 13), "Drug B"), col_cell(c(14, 19), "Total")),
    c("Baseline", rep(c("G0", "G1", "G2", "G3", "G4", "Total"), 3))))',
       new_src = '
pages <- rtf_plan(lb) |>
  plan_group("PARAMCD") |>
  plan_hide("PARAMCD") |>
  plan_blanks("between_groups") |>
  plan_header(list(col_cell(c(1, 2), ""), col_cell(c(3, 8), "Drug A"),
                   col_cell(c(9, 14), "Drug B"), col_cell(c(15, 20), "Total")),
              c("PARAMCD", "Baseline",
                rep(c("G0", "G1", "G2", "G3", "G4", "Total"), 3))) |>
  plan_style(border = "tfl") |>
  rtf_pages()',
       old = function() {
         p <- as_rtftables(lb, group_col = "PARAMCD", drop_cols = "PARAMCD",
                           blank_rows = "between_groups",
                           count_blank_rows = TRUE, border = "tfl")
         set_col_header(p, rtf_col_header(
           list(col_cell(1, ""), col_cell(c(2, 7), "Drug A"),
                col_cell(c(8, 13), "Drug B"), col_cell(c(14, 19), "Total")),
           c("Baseline", rep(c("G0", "G1", "G2", "G3", "G4", "Total"), 3))))
       },
       new = function() {
         rtf_plan(lb) |>
           plan_group("PARAMCD") |>
           plan_hide("PARAMCD") |>
           plan_blanks("between_groups") |>
           plan_header(list(col_cell(c(1, 2), ""), col_cell(c(3, 8), "Drug A"),
                            col_cell(c(9, 14), "Drug B"),
                            col_cell(c(15, 20), "Total")),
                       c("PARAMCD", "Baseline",
                         rep(c("G0", "G1", "G2", "G3", "G4", "Total"), 3))) |>
           plan_style(border = "tfl") |>
           rtf_pages()
       }),

  list(id = "05_AE_by_SOC",
       title = "AE by SOC  One page per SOC -- hidden carrier, spanning header, per-page naming",
       old_src = '
ae_sorted <- ae[order(ae$SOCORD, ae$PT), ]

pages <- as_rtftables(
  ae_sorted,
  drop_cols           = "SOCORD",
  group_col           = "SOC",
  split               = "by_value",
  border              = "tfl",
  column_widths_twips = c(2600L, 3000L, 1500L, 1200L, 1500L, 1200L),
  # Same data and same intent as 02_AE, but there is no stub here, so the same
  # two columns are 5 and 7 instead of 4 and 6.
  col_spec            = list(list(col = 5, align = "right"),
                             list(col = 7, align = "right"))
)',
       new_src = '
pages <- rtf_plan(ae) |>
  plan_roles(SOCORD = role("sort", order = 1), PT = role("sort", order = 2),
             `Drug A____Events` = role("display", align = "right"),
             `Drug B____Events` = role("display", align = "right")) |>
  plan_hide("SOCORD") |>
  plan_group("SOC") |>
  plan_pages(per_group = TRUE) |>
  plan_style(border = "tfl",
             widths = c(2600L, 3000L, 1500L, 1200L, 1500L, 1200L)) |>
  rtf_pages()',
       old = function() {
         ae_sorted <- ae[order(ae$SOCORD, ae$PT), ]
         as_rtftables(ae_sorted, drop_cols = "SOCORD", group_col = "SOC",
                      split = "by_value", border = "tfl",
                      column_widths_twips = c(2600L, 3000L, 1500L, 1200L,
                                              1500L, 1200L),
                      col_spec = list(list(col = 5, align = "right"),
                                      list(col = 7, align = "right")))
       },
       new = function() {
         rtf_plan(ae) |>
           plan_roles(SOCORD = role("sort", order = 1),
                      PT = role("sort", order = 2),
                      `Drug A____Events` = role("display", align = "right"),
                      `Drug B____Events` = role("display", align = "right")) |>
           plan_hide("SOCORD") |>
           plan_group("SOC") |>
           plan_pages(per_group = TRUE) |>
           plan_style(border = "tfl",
                      widths = c(2600L, 3000L, 1500L, 1200L, 1500L, 1200L)) |>
           rtf_pages()
       })
)

# ------------------------------------------------------------- render ----
render <- function(pages, f) {
  doc <- rtf_document(page = rtf_page(orientation = "landscape"))
  for (p in pages) doc <- rtf_tables(doc, p)
  generate_rtfreport(doc, f, overwrite = TRUE)
  readLines(f, warn = FALSE)
}

sz <- function(src) {
  ln <- trimws(strsplit(src, "\n")[[1]])
  ln <- ln[nzchar(ln) & !grepl("^#", ln)]
  c(lines = length(ln), chars = sum(nchar(gsub(" +", " ", ln))))
}

con <- file(file.path(OUT, "00_code.R"), "w", encoding = "UTF-8")
writeLines(c("# rtfreporter -- review samples",
             "#",
             "# Each block builds the SAME report two ways, with the SAME full",
             "# feature set.  See 00_SETUP.md for switching between versions."), con)

cat(sprintf("%-14s %-7s %-7s %-9s %s\n", "case", "old pg", "new pg", "rtf diff", "old / new size"))
cat(strrep("-", 76), "\n")
for (cs in cases) {
  o <- cs$old(); n <- cs$new()
  lo <- render(o, file.path(OUT, sprintf("%s_old.rtf", cs$id)))
  ln <- render(n, file.path(OUT, sprintf("%s_new.rtf", cs$id)))
  so <- sz(cs$old_src); sn <- sz(cs$new_src)
  same <- identical(lo, ln)
  cat(sprintf("%-14s %-7d %-7d %-9s %d ln %d ch / %d ln %d ch\n",
              cs$id, length(o), length(n),
              if (same) "identical" else sprintf("%d lines", sum(lo != ln)),
              so["lines"], so["chars"], sn["lines"], sn["chars"]))
  writeLines(c("", paste0("# ", strrep("=", 70)),
               paste0("# ", cs$title),
               paste0("# ", strrep("=", 70)),
               "", "# --- OLD (as_rtftables) ---", cs$old_src,
               "", "# --- NEW (rtf_plan + layers) ---", cs$new_src), con)
}
close(con)
cat("\nwrote", OUT, "\n")

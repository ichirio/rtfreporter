# ============================================================================
#  Definition workbooks for five reports -- DM, AE, ORR, LB shift, PK (#474)
# ============================================================================
#
#  Run from the repository root:
#
#      Rscript data-raw/ard-spec-examples/make-examples.R
#
#  For each report this script
#    1. builds the example ARD (the same code as Discussion #473),
#    2. writes its definition workbook to inst/extdata/ard-spec/<id>.xlsx,
#    3. reads the workbook back and runs rtf_plan(spec = ) on the ARD,
#    4. checks the table data frame is IDENTICAL to ard_spread() written
#       out, and the finished rtftable pages to the report's plan code.
#
#  It also writes study.xlsx: all five in one workbook, keyed by output_id,
#  with the study's rounding on its `study` sheet, and the shared n (%)
#  template written once, on a `cells` row whose output_id is blank.
# ============================================================================

suppressMessages({
  if (requireNamespace("pkgload", quietly = TRUE) &&
      file.exists("R/ard-experimental.R")) {
    pkgload::load_all(".", quiet = TRUE)
  } else {
    library(rtfreporter)
  }
  library(cards)
  library(dplyr)
})

out_dir <- file.path("inst", "extdata", "ard-spec")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# One data.frame per sheet, NA where a cell is blank.  `tbl()` keeps the
# sheets readable here: one line per spreadsheet row.
tbl <- function(...) {
  d <- rbind.data.frame(..., stringsAsFactors = FALSE,
                        make.row.names = FALSE)
  d[] <- lapply(d, function(v) { v[v %in% ""] <- NA; v })
  d
}

# Two checks per report: the table data frame against ard_spread() written
# out, and the finished rtftable pages against the report's plan code.  The
# column header is not in the workbook yet, so both sides add the same one.
check <- function(id, ard_n, spec_path, code_tbl, code_plan, header,
                  pages_n = ard_n) {
  sp <- read_ard_spec(spec_path, output_id = id)
  from_spec <- apply_plan(rtf_plan(ard_n, spec = sp, notes = FALSE), "table")
  ok <- isTRUE(all.equal(as.data.frame(from_spec), as.data.frame(code_tbl)))
  pg_spec <- apply_plan(header(rtf_plan(pages_n, spec = sp, notes = FALSE)),
                        "pages")
  pg_code <- apply_plan(code_plan, "pages")
  ok_pg <- isTRUE(all.equal(pg_spec, pg_code))
  np <- if (inherits(pg_spec, "rtftable")) 1L else length(pg_spec)
  cat(sprintf(
    "  %-4s %-11s table %3d x %2d == code: %-5s  pages %2d == plan code: %s\n",
    id, basename(spec_path), nrow(from_spec), ncol(from_spec),
    if (ok) "TRUE" else "FALSE", np,
    if (ok_pg) "TRUE" else "FALSE  <-- MISMATCH"))
  if (!ok) print(all.equal(as.data.frame(from_spec), as.data.frame(code_tbl)))
  if (!ok_pg) print(utils::head(all.equal(pg_spec, pg_code), 10))
  invisible(pg_spec)
}

# add the table-half sheets to a spec built above
with_pages <- function(sp, layout = NULL, columns = NULL, style = NULL) {
  x <- unclass(sp)
  x$layout <- layout; x$columns <- columns; x$style <- style
  ard_spec(x)
}

specs <- list()

# ---------------------------------------------------------------- 1. DM
ADSL <- cards::ADSL |> mutate(TRT01P = "XXXXX", HTBL = HEIGHTBL)
ard_dm <- ard_stack(
  ADSL, .by = TRT01P,
  ard_continuous(variables = c(AGE, HTBL),
                 statistic = ~ continuous_summary_fns(
                   c("N", "mean", "sd", "median", "min", "max"))),
  ard_categorical(variables = c(AGEGR1, SEX, ETHNIC),
                  statistic = ~ c("n", "p")),
  .total_n = TRUE)

specs$DM <- ard_spec(
  study = c(rounding = "sas"),
  tables = tbl(
    list(output_id = "DM", cols = "TRT01P", rows = "group = variable",
         label = "", sort = "",
         note = "Demographics; one arm")),
  variables = tbl(
    list(output_id = "DM", variable = "AGE",    label = "Age (years) [a]",                  order = 1, levels = ""),
    list(output_id = "DM", variable = "AGEGR1", label = "Age (group1) (years) [n (%)] [a]", order = 2, levels = "<65 | 65-80 | >80"),
    list(output_id = "DM", variable = "SEX",    label = "Sex [n (%)]",                      order = 3, levels = "F | M"),
    list(output_id = "DM", variable = "ETHNIC", label = "Ethnicity [n (%)]",                order = 4, levels = ""),
    list(output_id = "DM", variable = "HTBL",   label = "Baseline Height (cm)",             order = 5, levels = "")),
  cells = tbl(
    list(output_id = "DM", variable = "continuous",  context = "", row = "n",         when = "", template = "{N}",                digits = "0",   signif = ""),
    list(output_id = "DM", variable = "continuous",  context = "", row = "Mean (SD)", when = "", template = "{mean} ({sd})",      digits = "2,3", signif = ""),
    list(output_id = "DM", variable = "continuous",  context = "", row = "Median",    when = "", template = "{median}",           digits = "2",   signif = ""),
    list(output_id = "DM", variable = "continuous",  context = "", row = "Min, Max",  when = "", template = "{min}, {max}",       digits = "1",   signif = ""),
    list(output_id = "DM", variable = "categorical", context = "", row = "",          when = "", template = "{n:.0f} ({p:.1f%})", digits = "",    signif = "")))

dm_code <- ard_dm |>
  ard_normalize() |>
  ard_spread(
    cols     = "TRT01P",
    rows     = c(group = "variable"),
    labels   = c(AGE    = "Age (years) [a]",
                 AGEGR1 = "Age (group1) (years) [n (%)] [a]",
                 SEX    = "Sex [n (%)]",
                 ETHNIC = "Ethnicity [n (%)]",
                 HTBL   = "Baseline Height (cm)"),
    levels   = list(AGEGR1 = c("<65", "65-80", ">80"), SEX = c("F", "M")),
    cells    = list(
      continuous  = c("n"         = "{N:.0f}",
                      "Mean (SD)" = "{mean:.2f} ({sd:.3f})",
                      "Median"    = "{median:.2f}",
                      "Min, Max"  = "{min:.1f}, {max:.1f}"),
      categorical = "{n:.0f} ({p:.1f%})"),
    rounding = "sas", notes = FALSE)

# ---------------------------------------------------------------- 2. AE
set.seed(7)
ADSL2 <- cards::ADSL |>
  mutate(TR01AG1  = as.character(ARM),
         SEROSTAT = sample(c("Positive", "Negative"), n(), TRUE))
AE <- cards::ADAE |>
  inner_join(ADSL2 |> select(USUBJID, TR01AG1, SEROSTAT), by = "USUBJID")
ard_ae <- ard_stack_hierarchical(
  AE, variables = c(AEBODSYS, AEDECOD), by = c(TR01AG1, SEROSTAT),
  denominator = ADSL2, id = USUBJID, over_variables = TRUE)
ard_ae <- ard_ae[ard_ae$context != "tabulate", ]

specs$AE <- ard_spec(
  study = c(rounding = "sas"),
  tables = tbl(
    list(output_id = "AE", cols = "TR01AG1 | SEROSTAT",
         rows = "group1 = AEBODSYS", label = "label = AEDECOD",
         sort = ".overall | group1 | .depth | -n | label",
         note = "TEAE by SOC / PT, frequency descending")),
  variables = tbl(
    list(output_id = "AE", variable = "TR01AG1",  label = "", order = NA,
         levels = "Placebo | Xanomeline Low Dose | Xanomeline High Dose"),
    list(output_id = "AE", variable = "SEROSTAT", label = "", order = NA,
         levels = "Positive | Negative")),
  cells = tbl(
    list(output_id = "AE", variable = "", context = "", row = "", when = "",
         template = "{n:.0f} ({p:.1f%})", digits = "", signif = "")))

ae_n <- ard_normalize(ard_ae, hierarchy = c("AEBODSYS", "AEDECOD"),
                      overall = "Any TEAE")
ae_code <- ard_spread(
  ae_n,
  cols   = c("TR01AG1", "SEROSTAT"),
  rows   = c(group1 = "AEBODSYS"),
  label  = c(label  = "AEDECOD"),
  levels = list(TR01AG1  = c("Placebo", "Xanomeline Low Dose",
                             "Xanomeline High Dose"),
                SEROSTAT = c("Positive", "Negative")),
  cells  = "{n:.0f} ({p:.1f%})",
  sort   = c(".overall", "group1", ".depth", "-n", "label"),
  rounding = "sas", notes = FALSE)

# ---------------------------------------------------------------- 3. ORR
set.seed(3)
RS <- cards::ADSL |>
  select(USUBJID, TRT01P) |>
  mutate(BORCAT = sample(c("CR", "PR", "SD"), n(), TRUE, c(.15, .35, .5)),
         RESPFL = BORCAT %in% c("CR", "PR"))
ard_orr <- cards::ADSL |>
  select(USUBJID, TRT01P) |>
  left_join(RS |> select(USUBJID, BORCAT, RESPFL), by = "USUBJID") |>
  group_by(TRT01P, BORCAT) |>
  cardx::ard_categorical_ci(variables = RESPFL, method = "wilson")
ard_orr <- ard_orr[ard_orr$stat_name %in%
                     c("N", "n", "estimate", "conf.low", "conf.high"), ]

specs$ORR <- ard_spec(
  study = c(rounding = "sas"),
  tables = tbl(
    list(output_id = "ORR", cols = "TRT01P | variable",
         rows = "grp1 = group2 | grp2 = group2_level", label = "NA",
         sort = "FALSE", sep = "_",
         note = "`variable` (n / orr_ci) is derived with mutate() before the spread")),
  cells = tbl(
    list(output_id = "ORR", variable = "n",      context = "", row = "1", when = "",              template = "{N:.0f}",                          digits = "", signif = ""),
    list(output_id = "ORR", variable = "orr_ci", context = "", row = "1", when = "n == 0",        template = "0",                                digits = "", signif = ""),
    list(output_id = "ORR", variable = "orr_ci", context = "", row = "1", when = "estimate == 1", template = "{n:.0f} (100)",                    digits = "", signif = ""),
    list(output_id = "ORR", variable = "orr_ci", context = "", row = "1", when = "",              template = "{n:.0f} ({estimate:.1f%})",        digits = "", signif = ""),
    list(output_id = "ORR", variable = "orr_ci", context = "", row = "2", when = "",              template = "{conf.low:.1f%}, {conf.high:.1f%}", digits = "", signif = "")))

orr_n <- ard_orr |>
  ard_normalize() |>
  mutate(variable = if_else(stat_name == "N", "n", "orr_ci"))
orr_code <- ard_spread(
  orr_n,
  cols  = c("TRT01P", "variable"), sep = "_",
  rows  = c(grp1 = "group2", grp2 = "group2_level"),
  label = NA,
  cells = list(
    n      = c("1" = "{N:.0f}"),
    orr_ci = ard_cells(
      "1" = c(n == 0        ~ "0",
              estimate == 1 ~ "{n:.0f} (100)",
                              "{n:.0f} ({estimate:.1f%})"),
      "2" = "{conf.low:.1f%}, {conf.high:.1f%}")),
  sort = FALSE, rounding = "sas", notes = FALSE)

# ---------------------------------------------------------------- 4. LB
set.seed(11)
LB <- cards::ADSL |> select(USUBJID) |>
  tidyr::crossing(LBTOX_LBL = c("Alanine Aminotransferase", "Hemoglobin")) |>
  mutate(BASEGR  = sample(c("Grade 0", "Grade 1", "Grade 2"), n(), TRUE),
         WORSTGR = sample(c("Grade 0", "Grade 1", "Grade 2", "Grade 3"),
                          n(), TRUE))
ard_lb <- bind_rows(
  ard_categorical(LB, by = c(LBTOX_LBL, BASEGR), variables = WORSTGR),
  ard_categorical(mutate(LB, WORSTGR = "Total"),
                  by = c(LBTOX_LBL, BASEGR), variables = WORSTGR),
  ard_categorical(mutate(LB, BASEGR  = "Total"),
                  by = c(LBTOX_LBL, BASEGR), variables = WORSTGR),
  ard_categorical(mutate(LB, BASEGR = "Total", WORSTGR = "Total"),
                  by = c(LBTOX_LBL, BASEGR), variables = WORSTGR))

specs$LB <- ard_spec(
  study = c(rounding = "sas"),
  tables = tbl(
    list(output_id = "LB", cols = "BASEGR",
         rows = 'LBTOX_LBL = LBTOX_LBL | group1 = "Worst Post-Baseline Values"',
         label = "label = .label", sort = "",
         note = "Shift from baseline grade to worst post-baseline grade")),
  variables = tbl(
    list(output_id = "LB", variable = "BASEGR",  label = "", order = NA,
         levels = "Grade 0 | Grade 1 | Grade 2 | Total"),
    list(output_id = "LB", variable = "WORSTGR", label = "", order = NA,
         levels = "Grade 0 | Grade 1 | Grade 2 | Grade 3 | Total")),
  cells = tbl(
    list(output_id = "LB", variable = "", context = "", row = "", when = "",
         template = "{n:.0f} ({p:.1f%})", digits = "", signif = "")))

lb_code <- ard_lb |>
  ard_normalize() |>
  ard_spread(
    cols   = "BASEGR",
    rows   = c(LBTOX_LBL = "LBTOX_LBL",
               group1    = ~ "Worst Post-Baseline Values"),
    label  = c(label = ".label"),
    cells  = "{n:.0f} ({p:.1f%})",
    levels = list(BASEGR  = c("Grade 0", "Grade 1", "Grade 2", "Total"),
                  WORSTGR = c("Grade 0", "Grade 1", "Grade 2", "Grade 3",
                              "Total")),
    rounding = "sas", notes = FALSE)

# ---------------------------------------------------------------- 5. PK
set.seed(3)
TIMEPOINTS <- paste("Timepoint", 1:29)
PK <- cards::ADSL |>
  select(USUBJID) |>
  tidyr::crossing(ANALYTE = c("Drug X", "Metabolite Y"), ATPT = TIMEPOINTS) |>
  mutate(AVAL = abs(rnorm(n(), 120, 40)))
ard_pk <- ard_stack(
  PK, .by = c(ANALYTE, ATPT),
  ard_continuous(variables = AVAL,
                 statistic = ~ continuous_summary_fns(
                   c("N", "mean", "sd", "median", "min", "max"))))
stat_levels <- c("N", "Mean", "SD", "Median", "Min", "Max")

specs$PK <- ard_spec(
  study = c(rounding = "sas"),
  tables = tbl(
    list(output_id = "PK", cols = "ATPT", rows = "Analyte = ANALYTE",
         label = "Statistics = stat_label", stats = "rows", sort = "",
         note = "One statistic per row, raw values; digits are set on the display side")),
  variables = tbl(
    list(output_id = "PK", variable = "Statistics", label = "", order = NA,
         levels = paste(stat_levels, collapse = " | ")),
    list(output_id = "PK", variable = "ATPT", label = "", order = NA,
         levels = paste(TIMEPOINTS, collapse = " | "))))

pk_code <- ard_pk |>
  ard_normalize() |>
  ard_spread(
    cols   = "ATPT",
    rows   = c(Analyte = "ANALYTE"),
    label  = c(Statistics = "stat_label"),
    stats  = "rows",
    levels = list(Statistics = stat_levels, ATPT = TIMEPOINTS),
    rounding = "sas", notes = FALSE)


# ============================================================================
#  The table half: layout / columns / style, and each report's plan code
# ============================================================================
#
#  The plan code is the one in Discussion #473, for these example ARDs.  The
#  workbook says everything in it but the column header, which both sides add.

# ---------------------------------------------------------------- DM
dm_header <- function(p) plan_col_header(p, n = TRUE, rtf_col_header(
  c("",               "{col}"),
  c("Characteristic", "(N={n})")))
specs$DM <- with_pages(specs$DM,
  layout = tbl(list(output_id = "DM", stub_into = "row_label",
                    stub_before = "TRUE", blank_where = "between_groups",
                    blank_first = "TRUE", blank_last = "TRUE",
                    pages_max_rows = "21", pages_split = "group_safe")),
  columns = tbl(list(output_id = "DM", column = "row_label", width = "5"),
                list(output_id = "DM", column = ".values",   width = "2")),
  style = tbl(list(output_id = "DM", align_count_pct = "TRUE")))
dm_plan <- ard_dm |>
  ard_normalize() |>
  rtf_plan(cols = "TRT01P", rows = c(group = "variable"), notes = FALSE) |>
  plan_labels(c(AGE    = "Age (years) [a]",
                AGEGR1 = "Age (group1) (years) [n (%)] [a]",
                SEX    = "Sex [n (%)]",
                ETHNIC = "Ethnicity [n (%)]",
                HTBL   = "Baseline Height (cm)")) |>
  plan_levels(AGEGR1 = c("<65", "65-80", ">80"), SEX = c("F", "M")) |>
  plan_cells(continuous  = c("n"         = "{N:.0f}",
                             "Mean (SD)" = "{mean:.2f} ({sd:.3f})",
                             "Median"    = "{median:.2f}",
                             "Min, Max"  = "{min:.1f}, {max:.1f}"),
             categorical = "{n:.0f} ({p:.1f%})") |>
  plan_digits(rounding = "sas") |>
  plan_stub(into = "row_label", before = TRUE) |>
  plan_blanks(where = "between_groups", first = TRUE, last = TRUE) |>
  plan_paginate_rows(max_rows = 21, split = "group_safe") |>
  plan_style(widths = c(5, 2), align_count_pct = TRUE) |>
  dm_header()

# ---------------------------------------------------------------- AE
ae_header <- function(p) plan_col_header(p, n = TRUE, rtf_col_header(
  c(list(col_cell(1, "")),
    lapply(1:3, function(i)
      col_cell(c(2 * i, 1 + 2 * i), "{col1}\n(N={n:sum})\n n (%)"))),
  c(list(col_cell(1, "")),
    lapply(1:3, function(i)
      col_cell(c(2 * i, 1 + 2 * i), "",
               border = rtf_border(top = "single", bottom = "none")))),
  c("System Organ Class\n   Preferred Term", "{col2}\n(N={n})")))
specs$AE <- with_pages(specs$AE,
  layout = tbl(list(output_id = "AE", stub_into = "row_label",
                    stub_before = "TRUE", blank_where = "between_groups",
                    blank_first = "TRUE", blank_last = "TRUE",
                    blank_counted = "TRUE", pages_max_rows = "25",
                    pages_split = "group_force")),
  columns = tbl(list(output_id = "AE", column = "row_label", width = "40"),
                list(output_id = "AE", column = ".values",   width = "10")),
  style = tbl(list(output_id = "AE", align_count_pct = "TRUE",
                   row_height_twips = "210")))
ae_plan <- ae_n |>
  rtf_plan(cols  = c("TR01AG1", "SEROSTAT"),
           rows  = c(group1 = "AEBODSYS"),
           label = c(label  = "AEDECOD"), notes = FALSE) |>
  plan_levels(TR01AG1  = c("Placebo", "Xanomeline Low Dose",
                           "Xanomeline High Dose"),
              SEROSTAT = c("Positive", "Negative")) |>
  plan_sort(".overall", "group1", ".depth", "-n", "label") |>
  plan_cells("{n:.0f} ({p:.1f%})") |>
  plan_digits(rounding = "sas") |>
  plan_stub(into = "row_label", before = TRUE) |>
  plan_blanks(where = "between_groups", first = TRUE, last = TRUE,
              counted = TRUE) |>
  plan_paginate_rows(max_rows = 25, split = "group_force") |>
  plan_style(widths = c(40, 10), align_count_pct = TRUE,
             row_height_twips = 210L) |>
  ae_header()

# ---------------------------------------------------------------- ORR
orr_header <- function(p) plan_col_header(p, function(n, tbl) {
  arms <- unique(sub("_(n|orr_ci)$", "", names(tbl)[-(1:2)]))
  rtf_col_header(
    c(list(col_cell(c(1, 2), "")),
      lapply(seq_along(arms), function(i)
        col_cell(c(1 + 2 * i, 2 + 2 * i), arms[i]))),
    c(list(col_cell(c(1, 2), "Subgroup")),
      unlist(lapply(seq_along(arms), function(i)
        list(col_cell(1 + 2 * i, "N"),
             col_cell(2 + 2 * i, "ORR(%) 90%CI"))), recursive = FALSE)))
})
orr_arms <- c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose")
orr_cols <- list(
  list(output_id = "ORR", column = "grp1", width = "42", row_title = "TRUE"),
  list(output_id = "ORR", column = "grp2", width = "28", row_title = "TRUE"))
for (a in orr_arms) {
  orr_cols <- c(orr_cols, list(
    list(output_id = "ORR", column = paste0(a, "_n"),      width = "10",
         row_title = ""),
    list(output_id = "ORR", column = paste0(a, "_orr_ci"), width = "14",
         row_title = "")))
}
specs$ORR <- with_pages(specs$ORR,
  layout = tbl(list(output_id = "ORR", group_mode = "value",
                    group_collapse = "1 | 2",
                    blank_where = "between_groups", blank_first = "TRUE",
                    blank_last = "TRUE", pages_max_rows = "20",
                    pages_split = "group_safe")),
  columns = do.call(tbl, orr_cols),
  style = tbl(list(output_id = "ORR", align_count_pct = "FALSE")))
orr_plan <- orr_n |>
  rtf_plan(cols  = c("TRT01P", "variable"), sep = "_",
           rows  = c(grp1 = "group2", grp2 = "group2_level"),
           label = NA, notes = FALSE) |>
  plan_sort(FALSE) |>
  plan_cells(
    n      = c("1" = "{N:.0f}"),
    orr_ci = ard_cells(
      "1" = c(n == 0        ~ "0",
              estimate == 1 ~ "{n:.0f} (100)",
                              "{n:.0f} ({estimate:.1f%})"),
      "2" = "{conf.low:.1f%}, {conf.high:.1f%}")) |>
  plan_digits(rounding = "sas") |>
  plan_row_group(mode = "value", collapse = c(1L, 2L)) |>
  plan_blanks(where = "between_groups", first = TRUE, last = TRUE) |>
  plan_paginate_rows(max_rows = 20, split = "group_safe") |>
  plan_style(widths = c(42, 28, rep(c(10, 14), 3)),
             align_count_pct = FALSE, row_title = c(1, 2)) |>
  orr_header()

# ---------------------------------------------------------------- LB
lb_header <- function(p) plan_col_header(p, n = TRUE, rtf_col_header(
  list(col_cell(1L, "Timepoint"),
       col_cell(c(2L, 5L), "Treatment\n(N={n})\n n (%)\n<Baseline>")),
  c("  Category", "Grade 0", "Grade 1", "Grade 2", "Total")))
specs$LB <- with_pages(specs$LB,
  layout = tbl(list(output_id = "LB", stub_into = "row_label",
                    group_page = "TRUE", group_show = "FALSE",
                    blank_first = "TRUE", blank_last = "TRUE")),
  columns = tbl(list(output_id = "LB", column = "row_label", width = "5"),
                list(output_id = "LB", column = ".values",   width = "1")),
  style = tbl(list(output_id = "LB", align_count_pct = "TRUE")))
lb_pages_n <- ard_normalize(ard_lb, drop_contexts = "attributes")
lb_plan <- lb_pages_n |>
  rtf_plan(cols  = "BASEGR",
           rows  = c(LBTOX_LBL = "LBTOX_LBL",
                     group1    = ~ "Worst Post-Baseline Values"),
           label = c(label = ".label"), notes = FALSE) |>
  plan_levels(BASEGR  = c("Grade 0", "Grade 1", "Grade 2", "Total"),
              WORSTGR = c("Grade 0", "Grade 1", "Grade 2", "Grade 3",
                          "Total")) |>
  plan_cells("{n:.0f} ({p:.1f%})") |>
  plan_digits(rounding = "sas") |>
  plan_stub(into = "row_label") |>
  plan_paginate_group(show = FALSE) |>
  plan_blanks(first = TRUE, last = TRUE) |>
  plan_style(widths = c(5, rep(1, 4)), align_count_pct = TRUE) |>
  lb_header()

# ---------------------------------------------------------------- PK
pk_header <- function(p) plan_col_header(p, function(n, tbl) list(
  list(col_cell(1, ""), col_cell(2, ""),
       col_cell(c(3, length(tbl)), "Scheduled Timepoint")),
  names(tbl)))
pk_formats <- list(N = list(digits = 0), Mean = list(signif = 4),
                   SD = list(signif = 5), Median = list(signif = 4),
                   Min = list(signif = 3), Max = list(signif = 3))
# the statistics' formats: rows of `cells` with no template
pk_cells <- do.call(tbl, lapply(names(pk_formats), function(st) list(
  output_id = "PK", variable = "", context = "", row = st, when = "",
  template = "",
  digits = if (!is.null(pk_formats[[st]]$digits))
             as.character(pk_formats[[st]]$digits) else "",
  signif = if (!is.null(pk_formats[[st]]$signif))
             as.character(pk_formats[[st]]$signif) else "")))
x <- unclass(specs$PK); x$cells <- pk_cells; specs$PK <- ard_spec(x)
specs$PK <- with_pages(specs$PK,
  layout = tbl(list(output_id = "PK", group_collapse = "TRUE",
                    blank_where = "between_groups", blank_first = "TRUE",
                    blank_last = "TRUE", pages_max_rows = "21",
                    colpages_every = "13", colpages_carry = "1 | 2")),
  columns = tbl(
    list(output_id = "PK", column = "Analyte",    width = "3",
         row_title = "TRUE", decimal_split = ""),
    list(output_id = "PK", column = "Statistics", width = "3",
         row_title = "TRUE", decimal_split = ""),
    list(output_id = "PK", column = ".values",    width = "2",
         row_title = "",     decimal_split = "TRUE")))
pk_plan <- ard_normalize(ard_pk) |>
  rtf_plan(cols  = "ATPT", rows = c(Analyte = "ANALYTE"),
           label = c(Statistics = "stat_label"), stats = "rows",
           notes = FALSE) |>
  plan_levels(Statistics = stat_levels, ATPT = TIMEPOINTS) |>
  plan_digits(rounding = "sas") |>
  plan_fmt(by = "Statistics", formats = pk_formats) |>
  plan_row_group(collapse = TRUE) |>
  plan_blanks(where = "between_groups", first = TRUE, last = TRUE) |>
  plan_paginate_rows(max_rows = 21) |>
  plan_style(widths = c(3, 3, 2), row_title = 1:2) |>
  pk_header() |>
  plan_after(function(x) {
    d <- if (inherits(x, "rtftable")) x$data else x[[1L]]$data
    set_decimal_split(x, cols = setdiff(names(d), c("Analyte", "Statistics")))
  }) |>
  plan_paginate_cols(every = 13, carry = 1:2)

# ------------------------------------------------------ write and check
# A `_README` sheet explains the columns to whoever opens the file.  The
# reader ignores every sheet whose name starts with `_`, so it is safe to
# keep, edit or delete.
readme <- utils::read.csv(file.path("data-raw", "ard-spec-examples",
                                    "readme-sheet.csv"),
                          stringsAsFactors = FALSE, fileEncoding = "UTF-8",
                          check.names = FALSE)
write_book <- function(spec, path) {
  write_ard_spec(spec, path)
  nms <- readxl::excel_sheets(path)
  sheets <- lapply(nms, function(s)
    as.data.frame(readxl::read_excel(path, sheet = s, col_types = "text")))
  names(sheets) <- nms
  writexl::write_xlsx(c(list(`_README` = readme), sheets), path)
}

run_checks <- function(path_of) {
  check("DM",  ard_normalize(ard_dm), path_of("DM"),  dm_code,  dm_plan,
        dm_header)
  check("AE",  ae_n,                  path_of("AE"),  ae_code,  ae_plan,
        ae_header)
  check("ORR", orr_n,                 path_of("ORR"), orr_code, orr_plan,
        orr_header)
  check("LB",  ard_normalize(ard_lb), path_of("LB"),  lb_code,  lb_plan,
        lb_header, pages_n = lb_pages_n)
  check("PK",  ard_normalize(ard_pk), path_of("PK"),  pk_code,  pk_plan,
        pk_header)
}

cat("\nOne workbook per report:\n")
for (id in names(specs)) {
  write_book(specs[[id]], file.path(out_dir, paste0(id, ".xlsx")))
}
run_checks(function(id) file.path(out_dir, paste0(id, ".xlsx")))

# ------------------------------------------ one study workbook, all five
# The rounding family is one per study, on the `study` sheet.  What the
# reports share is written ONCE, on a row with a blank output_id -- here the
# n (%) template on `cells`.  A lookup that finds nothing more specific
# falls through to it, so a report's row that only restates it -- AE's and
# LB's catch-all, DM's `categorical` -- is dropped rather than kept twice.
all_sheet <- function(s) do.call(rbind, lapply(specs, function(x) x[[s]]))
study_cells <- all_sheet("cells")
default_tpl <- "{n:.0f} ({p:.1f%})"
restates <- !is.na(study_cells$template) &
  study_cells$template == default_tpl &
  is.na(study_cells$row) & is.na(study_cells$when) &
  is.na(study_cells$digits) & is.na(study_cells$signif) &
  is.na(study_cells$context) &
  (is.na(study_cells$variable) | study_cells$variable == "categorical")
study_cells <- rbind(
  tbl(list(output_id = "", variable = "", context = "", row = "", when = "",
           template = default_tpl, digits = "", signif = "")),
  study_cells[!restates, , drop = FALSE])
study <- ard_spec(all_sheet("tables"), all_sheet("variables"), study_cells,
                  study = c(rounding = "sas"), layout = all_sheet("layout"),
                  columns = all_sheet("columns"), style = all_sheet("style"))
study_path <- file.path(out_dir, "study.xlsx")
write_book(study, study_path)

cat("\nThe same five from one study workbook (study.xlsx):\n")
run_checks(function(id) study_path)
cat("\nwritten to", normalizePath(out_dir), "\n")

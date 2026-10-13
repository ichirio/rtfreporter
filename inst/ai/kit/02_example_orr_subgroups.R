library(rtfreporter)
library(cards)
library(dplyr)

# ---- 0. the ARD (when it is not given): the data, then cards / cardx ---------
adsl <- pharmaverseadam::adsl |> filter(SAFFL == "Y")
bor  <- pharmaverseadam::adrs_onco |> filter(PARAMCD == "BOR") |> select(USUBJID, AVALC)
anl <- adsl |>
  select(USUBJID, TR01AG1 = TRT01A, SEX, AGEGR1) |>
  inner_join(bor, by = "USUBJID") |>
  mutate(RESPFL = factor(if_else(AVALC %in% c("CR", "PR"), "Y", "N"), levels = c("Y", "N")),
         ALL    = "All")

# the arm's own count (the header's N), the ORR with its 90% CI by subgroup
orr <- function(.x) {
  cardx::ard_categorical_ci(.x, variables = RESPFL, value = RESPFL ~ "Y",
                            method = "clopper-pearson", conf.level = 0.90)
}
ard <- bind_ard(
  ard_tabulate(anl, variables = TR01AG1),
  ard_strata(anl, .by = TR01AG1, .strata = ALL,    .f = orr),
  ard_strata(anl, .by = TR01AG1, .strata = SEX,    .f = orr),
  ard_strata(anl, .by = TR01AG1, .strata = AGEGR1, .f = orr),
  ard_strata(anl, .by = TR01AG1, .strata = c(AGEGR1, SEX), .f = orr))

# ---- 1. the table: the plan over the ARD -----------------------------------
tbl_plan <- ard |>
  normalize_ard(stack = list(subgroup = list("ALL", "SEX", "AGEGR1", c("AGEGR1", "SEX")))) |>
  table_plan(
    cols  = ".stat_group",
    rows  = c(arm = "TR01AG1", grp1 = "subgroup", grp2 = "subgroup_level"),
    label = NA) |>
  plan_stat_groups(n = "N", orr_ci = c("n", "estimate", "conf.low", "conf.high")) |>
  plan_paginate_group(col = "arm", keep = FALSE,
                      heading = "Treatment: {value} (N={n})") |>
  plan_labels(subgroup = c(ALL = "All Subjects", SEX = "Sex", AGEGR1 = "Age Group",
                           "AGEGR1 / SEX" = "Age Group by Sex")) |>
  plan_sort(FALSE) |>
  plan_cells(
    n      = c("1" = "{N:.0f}"),
    orr_ci = cell_rows(
      "1" = c(n == 0        ~ "0",
              estimate == 1 ~ "{n:.0f} (100)",
              "{n:.0f} ({estimate:.1f%})"),
      "2" = "{conf.low:.1f%}, {conf.high:.1f%}")) |>
  plan_row_group(mode = "value", collapse = c("grp1", "grp2")) |>
  plan_blanks(where = "between_groups", first = TRUE, last = TRUE) |>
  plan_columns(widths = c(grp1 = 42, grp2 = 28, .values = 15),
               row_title = c("grp1", "grp2")) |>
  plan_style(align_count_pct = FALSE) |>
  plan_col_header(header = rtf_col_header(
    list(
      col_cell(c(1, 2), "Subgroup"),
      col_cell(3, "N"),
      col_cell(4, "ORR(%) 90%CI")
    )
  ))

# ---- 2. the document: section, titles, footnotes ---------------------------
titles <- list(
  c(""),
  c("Table 15.2.2.1.b"),
  c("Summary of Overall Response per Investigator - Subgroup Analysis - Phase 2 Part"),
  c("<Full Analysis Set>"),
  c("")
)

footer_texts <- list(
  c(l = "Objective response rate is defined as proportion of patients with complete or partial response per investigator assessment."),
  c(l = "ORR = Objective Response Rate, CI = Confidence Interval, BRCA = Breast Cancer Susceptibility Gene, PARPi = Poly-ADP Ribose polymerase Inhibitors, NE = Not Estimable."),
  c(l = "{PROGRAM_FULL}    Generated on: {DATETIME}")
)

rep <- rtf_document() |>
  rtf_section(
    header = rtf_header(c(list(c(l = "Protocol: CDISCPILOT01", r = "Page {PAGE} of {TOTAL_PAGES}")), titles)),
    footer = rtf_footer(footer_texts)) |>
  rtf_tables(tbl_plan, auto_section = TRUE)

# ---- 3. the file ------------------------------------------------------------
generate_rtfreport(rep, file.path(output_path, paste0(report_name, ".rtf")), overwrite = TRUE)

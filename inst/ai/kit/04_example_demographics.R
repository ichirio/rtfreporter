library(rtfreporter)
library(cards)

# ---- 0. the ARD (when it is not given): summaries by arm and overall, the tests
adsl <- pharmaverseadam::adsl[pharmaverseadam::adsl$SAFFL == "Y", ]
adsl$TRT01A <- factor(adsl$TRT01A, levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
ard <- bind_ard(
  ard_stack(adsl, .by = TRT01A, .overall = TRUE,                 # .overall: the Total column's rows
    ard_summary(variables = AGE, statistic = ~ continuous_summary_fns(
      c("N", "mean", "sd", "median", "p25", "p75", "min", "max"))),
    ard_tabulate(variables = c(AGEGR1, SEX, RACE))),
  cardx::ard_stats_kruskal_test(adsl, by = TRT01A, variables = AGE),   # the p-values: the ARD's, never computed here
  cardx::ard_stats_chisq_test(adsl, by = TRT01A, variables = c(AGEGR1, SEX, RACE)))

# ---- 1. the table: the plan over the ARD -----------------------------------
tbl_plan <- ard |>
  normalize_ard() |>
  table_plan(cols = "TRT01A", rows = c(group = "variable")) |>
  plan_cells(
    continuous  = c("n"               = "{N:.0f}",
                    "Mean (SD)"       = "{mean:.1f} ({sd:.2f})",
                    "Median (Q1, Q3)" = "{median:.1f} ({p25:.1f}, {p75:.1f})",
                    "Min, Max"        = "{min:.0f}, {max:.0f}"),
    categorical = c(n == 0 ~ "0", "{n:.0f} ({p:.1f%})")) |>
  plan_total(label = "Total") |>
  plan_compare(p = c(p.value < 0.001 ~ "<0.001", "{p.value:.3f}")) |>   # on each variable's first row
  plan_labels(c(AGE = "Age (years)", AGEGR1 = "Age group, n (%)", SEX = "Sex, n (%)", RACE = "Race, n (%)")) |>
  plan_levels(SEX = c("F", "M")) |>
  plan_stub(name = "Characteristic") |>
  plan_blanks(where = "between_groups", first = TRUE, last = TRUE) |>
  plan_columns(widths = c(4, 2, 2, 2, 2, 1.5)) |>
  plan_style(border = "tfl", align_count_pct = TRUE) |>
  plan_col_header(values = list(n = TRUE), lines = list(          # a cell's name = its columns
    c(Characteristic = "",               .values = "{col}",   p = "p-value"),
    c(Characteristic = "Characteristic", .values = "(N={n})", p = "")))

# ---- 2. the document: section, titles, footnotes ---------------------------
titles <- list(
  c("Table 14.1.1"),
  c("Demographic and Baseline Characteristics"),
  c("Safety Analysis Set"))
footnotes <- list(
  c(l = "p-values: Kruskal-Wallis test for age, chi-square test for the categorical characteristics."),
  c(l = "{PROGRAM_FULL}    Generated on: {DATETIME}"))
rep <- rtf_document() |>
  rtf_section(
    header = rtf_header(c(list(c(l = "Protocol: CDISCPILOT01", r = "Page {PAGE} of {TOTAL_PAGES}")), titles)),
    footer = rtf_footer(footnotes)) |>
  rtf_tables(tbl_plan)

# ---- 3. the file ------------------------------------------------------------
generate_rtfreport(rep, file.path(output_path, paste0(report_name, ".rtf")), overwrite = TRUE)

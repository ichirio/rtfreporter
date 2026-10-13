library(rtfreporter)
library(cards)

# ---- 0. the ARD (when it is not given): TEAEs by SOC and PT, each arm's N ---
adsl <- pharmaverseadam::adsl[pharmaverseadam::adsl$SAFFL == "Y", ]
adae <- pharmaverseadam::adae[pharmaverseadam::adae$TRTEMFL %in% "Y" & pharmaverseadam::adae$SAFFL %in% "Y", ]
arms <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
adsl$TRT01A <- factor(adsl$TRT01A, levels = arms)
adae$TRT01A <- factor(adae$TRT01A, levels = arms)
ard <- ard_stack_hierarchical(adae, variables = c(AEBODSYS, AEDECOD), by = TRT01A,
                              denominator = adsl, id = USUBJID, over_variables = TRUE)

# ---- 1. the table: the plan over the ARD -----------------------------------
tbl_plan <- ard |>
  normalize_ard(hierarchy = c("AEBODSYS", "AEDECOD"), overall = "Subjects with at least one TEAE") |>
  table_plan(cols = "TRT01A", rows = c(SOC = "AEBODSYS", PT = "AEDECOD"), label = NA) |>
  plan_sort(".overall", "-n@SOC", "SOC", ".depth", "-n", "PT") |>   # the any row first; SOCs by count; PTs by count
  plan_filter(p >= 0.05) |>                                        # only the PTs with 5% or more in any arm
  plan_cells(c(n == 0 ~ "0", "{n:.0f} ({p:.1f%})")) |>
  plan_stub(name = "System Organ Class\n  Preferred Term", indent = 2) |>
  plan_paginate_rows(max_rows = 30, split = "group_safe") |>
  plan_blanks(where = "between_groups") |>
  plan_columns(widths = c(5, 2, 2, 2)) |>
  plan_style(border = "tfl", align_count_pct = TRUE) |>
  plan_col_header(values = list(n = TRUE), header = rtf_col_header(
    c("", "{col}"),
    c("", "(N={n})"),
    c("", "n (%)")))

# ---- 2. the document: section, titles, footnotes ---------------------------
titles <- list(
  c("Table 14.3.1.2"),
  c("Treatment-Emergent Adverse Events by System Organ Class and Preferred Term"),
  c("Safety Analysis Set"))
footnotes <- list(
  c(l = "Subjects are counted once per system organ class and once per preferred term."),
  c(l = "Percentages are based on the number of subjects in the safety analysis set."),
  c(l = "{PROGRAM_FULL}    Generated on: {DATETIME}"))
rep <- rtf_document() |>
  rtf_section(
    header = rtf_header(c(list(c(l = "Protocol: CDISCPILOT01", r = "Page {PAGE} of {TOTAL_PAGES}")), titles)),
    footer = rtf_footer(footnotes)) |>
  rtf_tables(tbl_plan)

# ---- 3. the file ------------------------------------------------------------
generate_rtfreport(rep, file.path(output_path, paste0(report_name, ".rtf")), overwrite = TRUE)

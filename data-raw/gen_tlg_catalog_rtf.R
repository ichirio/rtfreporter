# =============================================================================
# data-raw/gen_tlg_catalog_rtf.R
#
# Regenerate the example RTF files used in
# vignettes/articles/tlg-catalog.Rmd to a KNOWN folder, so you can open them
# in Word / LibreOffice and take screenshots for the article.
#
# The table objects are the pharmaverse example objects (demographic + adverse
# events), built verbatim; rtfreporter only adds the RTF-rendering step.
#
# Run from the repo root (with the dev package installed):
#     Rscript data-raw/gen_tlg_catalog_rtf.R
#
# Output: ./inst/rtf-examples/*.rtf  -- these files are committed so the
# article can link to them on GitHub for download.
# =============================================================================

library(rtfreporter)
library(tern)
library(rtables)
library(gtsummary)
library(tfrmt)
library(cards)
library(forcats)
library(dplyr)

out_dir <- file.path("inst", "rtf-examples")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

adsl <- pharmaverseadam::adsl
adae <- pharmaverseadam::adae

# Demographic data prep -- verbatim from the pharmaverse "Demographic Table"
# example (drops screen failures; recodes SEX / AGEGR1; sets variable labels).
adsl_dm <- pharmaverseadam::adsl |>
  filter(!ACTARM %in% "Screen Failure") |>
  mutate(
    SEX = case_match(SEX, "M" ~ "MALE", "F" ~ "FEMALE"),
    AGEGR1 = case_when(
      between(AGE, 18, 40) ~ "18-40",
      between(AGE, 41, 64) ~ "41-64",
      AGE > 64            ~ ">=65") |>
      factor(levels = c("18-40", "41-64", ">=65"))) |>
  labelled::set_variable_labels(
    AGE = "Age (yr)", AGEGR1 = "Age group", SEX = "Sex", RACE = "Race")

make_header <- function(table_no, title, subtitle = "Safety Analysis Set") {
  rtf_header(rows = list(
    c(l = "HOGESTER Co. Limited", r = "CONFIDENTIAL"),
    c(l = "Protocol: RTF-101",     r = "Page {PAGE} of {TOTAL_PAGES}"),
    c(c = paste0("Table ", table_no)),
    c(c = title),
    c(c = paste0("<", subtitle, ">"))
  ))
}
make_footer <- function(built_with) {
  rtf_footer(c(l = paste0(
    "Table object built with ", built_with,
    " (Pharmaverse Example); rendered to RTF by rtfreporter.")))
}
# AE tables (5 columns): row-label column left, data columns centred.
ae_col_spec <- c(
  list(list(col = 1L, align = "left")),
  lapply(2:5, function(j) list(col = j, align = "center")))
demog_title <- "Demographic and Baseline Characteristics"
ae_title    <- "Adverse Events by System Organ Class and Preferred Term"

.write <- function(doc, file, npages = NULL) {
  path <- file.path(out_dir, file)
  generate_rtfreport(doc, path, overwrite = TRUE)
  message("wrote ", path, if (!is.null(npages)) paste0(" (", npages, " pages)"))
  path
}

# ---- 1a. Demographics (tern + rtables) -------------------------------------
adsl2 <- adsl_dm |> df_explicit_na()
vars <- c("AGE", "AGEGR1", "SEX", "RACE")
var_labels <- c("Age (yr)", "Age group", "Sex", "Race")
dm_tern <- build_table(
  basic_table(show_colcounts = TRUE) |>
    split_cols_by(var = "ACTARM") |>
    add_overall_col("All Patients") |>
    analyze_vars(vars = vars, var_labels = var_labels),
  adsl2)
out_dm_tern <- .write(
  rtf_document() |>
    rtf_section(page = 1, secinfo = list(
      header = make_header("14.1.1a", paste(demog_title, "(tern)")),
      footer = make_footer("tern + rtables"))) |>
    rtf_tables(as_rtftables(dm_tern, blank_rows = "between_groups",
                          auto_width = TRUE, cell_format = fmt_count_paren)),
  "pharmaverse-demographic-tern.rtf")

# ---- 1b. Demographics (gtsummary + cards) ----------------------------------
theme_gtsummary_compact()
ard <- ard_stack(adsl_dm,
  ard_continuous(variables = AGE),
  ard_categorical(variables = c(AGEGR1, SEX, RACE)),
  .by = ACTARM, .attributes = TRUE)
dm_gts <- tbl_ard_summary(cards = ard, by = ACTARM,
  include = c(AGE, AGEGR1, SEX, RACE), type = AGE ~ "continuous2",
  statistic = AGE ~ c("{N}", "{mean} ({sd})",
                      "{median} ({p25}, {p75})", "{min}, {max}")) |>
  bold_labels() |>
  modify_header(all_stat_cols() ~ "**{level}**  \nN = {n}") |>
  modify_footnote(everything() ~ NA)
out_dm_gts <- .write(
  rtf_document() |>
    rtf_section(page = 1, secinfo = list(
      header = make_header("14.1.1b", paste(demog_title, "(gtsummary)")),
      footer = make_footer("gtsummary + cards"))) |>
    rtf_tables(as_rtftables(dm_gts, blank_rows = "between_groups",
                          auto_width = TRUE, cell_format = fmt_count_paren)),
  "pharmaverse-demographic-gtsummary.rtf")

# ---- 1c. Demographics (tfrmt + cards) --------------------------------------
ard <- ard_stack(adsl_dm,
  ard_continuous(variables = AGE,
    statistic = ~ continuous_summary_fns(c("N", "mean", "sd", "min", "max"))),
  ard_categorical(variables = c(AGEGR1, SEX, RACE)),
  .by = ACTARM, .overall = TRUE, .total_n = TRUE)
ard_tbl <- ard |>
  shuffle_card(fill_overall = "Total") |>
  prep_big_n(vars = "ACTARM") |>
  prep_combine_vars(vars = c("AGE", "AGEGR1", "SEX", "RACE")) |>
  prep_label() |>
  group_by(ACTARM, stat_variable) |>
  mutate(across(c(variable_level, label), ~ ifelse(stat_name == "N", "n", .x))) |>
  ungroup() |> unique() |>
  mutate(ord1 = fct_inorder(stat_variable) |> fct_relevel("SEX", after = 0) |> as.numeric(),
         ord2 = ifelse(label == "n", 1, 2)) |>
  mutate(stat_variable = case_when(
    stat_variable == "AGE"    ~ "Age (YEARS) at First Dose",
    stat_variable == "AGEGR1" ~ "Age Group (YEARS) at First Dose",
    stat_variable == "SEX"    ~ "Sex",
    stat_variable == "RACE"   ~ "High Level Race", .default = stat_variable)) |>
  select(ACTARM, stat_variable, label, stat_name, stat, ord1, ord2) |> unique()
dm_tfrmt <- tfrmt(group = stat_variable, label = label, param = stat_name,
  value = stat, column = ACTARM, sorting_cols = c(ord1, ord2),
  body_plan = body_plan(
    frmt_structure(".default", ".default", frmt("xxx")),
    frmt_structure(".default", ".default",
      frmt_combine("{n} ({p}%)", n = frmt("xxx"),
                   p = frmt("xx", transform = ~ . * 100)))),
  big_n = big_n_structure(param_val = "bigN", n_frmt = frmt(" (N=xx)")),
  col_plan = col_plan(-starts_with("ord")),
  col_style_plan = col_style_plan(col_style_structure(
    col = c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose", "Total"),
    align = "left")),
  row_grp_plan = row_grp_plan(row_grp_structure(
    ".default", element_block(post_space = " ")))) |>
  print_to_gt(ard_tbl)
.write(
  rtf_document() |>
    rtf_section(page = 1, secinfo = list(
      header = make_header("14.1.1c", paste(demog_title, "(tfrmt)")),
      footer = make_footer("tfrmt + cards"))) |>
    rtf_tables(as_rtftables(dm_tfrmt, auto_width = TRUE,
                          cell_format = fmt_count_paren)),
  "pharmaverse-demographic-tfrmt.rtf")

# ---- AE data prep ----------------------------------------------------------
adsl_ae <- adsl |> df_explicit_na()
adae_ae <- adae |> df_explicit_na() |>
  var_relabel(AEBODSYS = "MedDRA System Organ Class",
              AEDECOD  = "MedDRA Preferred Term") |>
  filter(SAFFL == "Y")

# ---- 2a. Adverse events (tern + rtables, paginated) ------------------------
split_fun <- drop_split_levels
ae_tern <- build_table(
  basic_table(show_colcounts = TRUE) |>
    split_cols_by(var = "ACTARM") |>
    add_overall_col(label = "All Patients") |>
    analyze_num_patients(vars = "USUBJID", .stats = c("unique", "nonunique"),
      .labels = c(
        unique    = "Total number of patients with at least one adverse event",
        nonunique = "Overall total number of events")) |>
    split_rows_by("AEBODSYS", child_labels = "visible", nested = FALSE,
                  split_fun = split_fun, label_pos = "topleft",
                  split_label = obj_label(adae_ae$AEBODSYS)) |>
    summarize_num_patients(var = "USUBJID", .stats = c("unique", "nonunique"),
      .labels = c(
        unique    = "Total number of patients with at least one adverse event",
        nonunique = "Total number of events")) |>
    count_occurrences(vars = "AEDECOD", .indent_mods = -1L) |>
    append_varlabels(adae_ae, "AEDECOD", indent = 1L),
  df = adae_ae, alt_counts_df = adsl_ae)
ae_tern_pages <- as_rtftables(ae_tern, split = "group_safe", max_rows = 36,
                              blank_rows = "between_groups",
                              cell_format = fmt_count_paren_bare,
                              col_rel_width = c(0.50, 0.125, 0.125, 0.125, 0.125),
                              col_spec = ae_col_spec,
                              row_height_twips = 200)
out_ae_tern <- .write(
  rtf_document() |>
    rtf_section(page = 1, secinfo = list(
      header = make_header("14.3.1a", paste(ae_title, "(tern)")),
      footer = make_footer("tern + rtables"))) |>
    rtf_tables(ae_tern_pages),
  "pharmaverse-adverse-events-tern.rtf", length(ae_tern_pages))

# ---- 2b. Adverse events (cards + tfrmt, paginated) -------------------------
# Data prep verbatim from the pharmaverse cards + tfrmt example: safety
# population, treatment-emergent events.
adsl_ae_tf <- pharmaverseadam::adsl |> filter(SAFFL == "Y")
adae_ae_tf <- pharmaverseadam::adae |> filter(SAFFL == "Y" & TRTEMFL == "Y")
ae_ard <- ard_stack_hierarchical(
  data = adae_ae_tf, by = ACTARM, variables = c(AEBODSYS, AEDECOD),
  statistic = ~ c("n", "p"), denominator = adsl_ae_tf, id = USUBJID,
  over_variables = TRUE, overall = TRUE)
ae_tot <- ard_stack_hierarchical(
  data = mutate(adae_ae_tf, ACTARM = "All Patients"), by = ACTARM,
  variables = c(AEBODSYS, AEDECOD),
  denominator = mutate(adsl_ae_tf, ACTARM = "All Patients"),
  statistic = ~ c("n", "p"), id = USUBJID,
  over_variables = TRUE, overall = TRUE) |>
  filter(group2 == "ACTARM" | variable == "ACTARM")
ae_card <- bind_ard(ae_ard, ae_tot) |>
  shuffle_card(fill_hierarchical_overall = "ANY EVENT") |>
  prep_big_n(vars = "ACTARM") |>
  prep_hierarchical_fill(vars = c("AEBODSYS", "AEDECOD"), fill = "ANY EVENT") |>
  mutate(ACTARM = ifelse(ACTARM == "Overall ACTARM", "All Patients", ACTARM))
ord_soc <- ae_card |>
  filter(ACTARM == "All Patients", stat_name == "n", AEDECOD == "ANY EVENT") |>
  arrange(desc(stat)) |> mutate(ord1 = row_number()) |> select(AEBODSYS, ord1)
ord_pt <- ae_card |>
  filter(ACTARM == "All Patients", stat_name == "n") |>
  group_by(AEBODSYS) |> arrange(desc(stat)) |>
  mutate(ord2 = row_number()) |> select(AEBODSYS, AEDECOD, ord2)
ae_card <- ae_card |>
  full_join(ord_soc, by = "AEBODSYS") |>
  full_join(ord_pt, by = c("AEBODSYS", "AEDECOD")) |>
  select(AEBODSYS, AEDECOD, ord1, ord2, stat, stat_name, ACTARM)
ae_tfrmt <- tfrmt_n_pct(n = "n", pct = "p",
  pct_frmt_when = frmt_when(
    "==1" ~ frmt("(100%)"), ">=0.995" ~ frmt("(>99%)"), "==0" ~ frmt(""),
    "<=0.01" ~ frmt("(<1%)"), "TRUE" ~ frmt("(xx.x%)", transform = ~ . * 100))) |>
  tfrmt(group = AEBODSYS, label = AEDECOD, param = stat_name, value = stat,
    column = ACTARM, sorting_cols = c(ord1, ord2),
    col_plan = col_plan("System Organ Class / Preferred Term" = AEBODSYS,
      Placebo, `Xanomeline High Dose`, `Xanomeline Low Dose`, -ord1, -ord2),
    row_grp_plan = row_grp_plan(row_grp_structure(
      group_val = ".default", element_block(post_space = " "))),
    big_n = big_n_structure(param_val = "bigN", n_frmt = frmt(" (N=xx)"))) |>
  print_to_gt(ae_card)
ae_hdr <- c("System Organ Class /\nPreferred Term",
            "Placebo\n(N=86)",
            "Xanomeline\nHigh Dose\n(N=72)",
            "Xanomeline\nLow Dose\n(N=96)",
            "All Patients\n(N=254)")
ae_tfrmt_pages <- as_rtftables(ae_tfrmt, split = "group_force", max_rows = 36,
                               col_header = ae_hdr,
                               col_rel_width = c(0.50, 0.125, 0.125, 0.125, 0.125),
                               col_spec = ae_col_spec,
                               cell_format = fmt_count_paren_bare,
                               row_height_twips = 200)
.write(
  rtf_document() |>
    rtf_section(page = 1, secinfo = list(
      header = make_header("14.3.1b", paste(ae_title, "(tfrmt)")),
      footer = make_footer("tfrmt + cards"))) |>
    rtf_tables(ae_tfrmt_pages),
  "pharmaverse-adverse-events-tfrmt.rtf", length(ae_tfrmt_pages))

# ---- 3. Assembled deliverable (with a Table of Contents) -------------------
assemble_rtf(
  c(out_dm_gts, out_ae_tern),
  file.path(out_dir, "pharmaverse-assembled.rtf"),
  overwrite = TRUE,
  toc = list(
    toc_heading("DEMOGRAPHIC DATA", level = 1),
    toc_entry("Table 14.1.1b  Demographic and Baseline Characteristics (gtsummary)",
              file = out_dm_gts, level = 2),
    toc_heading("SAFETY DATA", level = 1),
    toc_entry("Table 14.3.1a  Adverse Events by SOC and Preferred Term (tern)",
              file = out_ae_tern, level = 2)),
  toc_title = "Table of Contents",
  toc_page_numbering = "decimal")
message("wrote ", file.path(out_dir, "pharmaverse-assembled.rtf"))

message("\nDone. Open the .rtf files in ", normalizePath(out_dir),
        "\nScreenshots (PNG) go to vignettes/articles/figures/ as:",
        "\n  pharmaverse-demographic-{tern,gtsummary,tfrmt}.png",
        "\n  pharmaverse-adverse-events-{tern,tfrmt}.png",
        "\n  pharmaverse-assembled.png  pharmaverse-assembled-toc.png")

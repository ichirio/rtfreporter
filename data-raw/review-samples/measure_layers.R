# Where does the layer form pay for itself?
#
# The five review samples are all "one call vs one call", so they can only
# measure a rename.  This measures three spellings per case -- old, the plan
# shorthand, and the plan LAYERS -- across a rising feature count, which is
# the axis the design is actually being judged on.

setwd("C:/Users/ichir/AppData/Local/Temp/claude/C--Users-ichir/0e8953d4-534f-4543-9b50-71c7b61ba96a/scratchpad/plan-wt")
suppressMessages(pkgload::load_all(".", quiet = TRUE))

sz <- function(src) {
  ln <- trimws(strsplit(src, "\n")[[1]])
  ln <- ln[nzchar(ln) & !grepl("^#", ln)]
  c(lines = length(ln), chars = sum(nchar(gsub(" +", " ", ln))))
}

cases <- list(
  list(id = "01_DM", feats = 3,
       old = 'as_rtftables(dm, split = "group_safe", max_rows = 10,
             group_col = "Label", border = "tfl")',
       short = 'rtf_plan_from(dm, group = "Label", max_rows = 10, border = "tfl")',
       layer = 'rtf_plan_from(dm) |>
  plan_group("Label") |>
  plan_pages(max_rows = 10) |>
  plan_style(border = "tfl")'),

  list(id = "04_LB", feats = 4,
       old = 'as_rtftables(lb, group_col = "PARAMCD", drop_cols = "PARAMCD",
             blank_rows = "between_groups", border = "tfl")',
       short = 'rtf_plan_from(lb, group = "PARAMCD", hide = "PARAMCD",
              blanks = "between_groups", border = "tfl")',
       layer = 'rtf_plan_from(lb) |>
  plan_group("PARAMCD") |>
  plan_hide("PARAMCD") |>
  plan_blanks("between_groups") |>
  plan_style(border = "tfl")'),

  list(id = "02_AE", feats = 6,
       old = 'as_rtftables(ae, stub_vars = c("SOC", "PT"), split = "group_safe",
             max_rows = 12, group_by = "indent",
             blank_rows = "between_groups", border = "tfl")',
       short = 'rtf_plan_from(ae, stub = c("SOC", "PT"), group = "SOC",
              group_mode = "indent", blanks = "between_groups",
              max_rows = 12, border = "tfl")',
       layer = 'rtf_plan_from(ae) |>
  plan_stub(c("SOC", "PT")) |>
  plan_group("SOC", mode = "indent") |>
  plan_blanks("between_groups") |>
  plan_pages(max_rows = 12) |>
  plan_style(border = "tfl")'),

  list(id = "FULLSET", feats = 10,
       old = 'ae_sorted <- ae[order(ae$SOCORD, ae$PT), ]
as_rtftables(ae_sorted,
  stub_vars = c("SOC", "PT"),
  stub_label = "System Organ Class / Preferred Term",
  drop_cols = "SOCORD",
  group_col = "System Organ Class / Preferred Term",
  group_by = "indent",
  blank_rows = "between_groups",
  count_blank_rows = TRUE,
  split = "group_safe",
  max_rows = 12,
  border = "tfl",
  column_widths_twips = widths,
  col_spec = list(list(col = 3, align = "right"),
                  list(col = 5, align = "right")))',
       short = 'rtf_plan_from(ae, stub = c("SOC", "PT"), group = "SOC",
              group_mode = "indent", sort = c("SOCORD", "PT"),
              hide = "SOCORD", blanks = "between_groups",
              max_rows = 12, border = "tfl", widths = widths) |>
  plan_stub(c("SOC", "PT"), label = "System Organ Class / Preferred Term") |>
  plan_roles(`Drug A____Events` = role("display", align = "right"),
             `Drug B____Events` = role("display", align = "right"))',
       layer = 'rtf_plan_from(ae) |>
  plan_stub(c("SOC", "PT"), label = "System Organ Class / Preferred Term") |>
  plan_roles(SOCORD = role("sort", order = 1), PT = role("sort", order = 2),
             `Drug A____Events` = role("display", align = "right"),
             `Drug B____Events` = role("display", align = "right")) |>
  plan_hide("SOCORD") |>
  plan_group("SOC", mode = "indent") |>
  plan_blanks("between_groups") |>
  plan_pages(max_rows = 12) |>
  plan_style(border = "tfl", widths = widths)')
)

cat(sprintf("%-9s %5s | %-13s | %-13s | %-13s | %s\n",
            "case", "feats", "old", "shorthand", "layers", "layers vs old"))
cat(strrep("-", 84), "\n")
for (cs in cases) {
  o <- sz(cs$old); s <- sz(cs$short); l <- sz(cs$layer)
  cat(sprintf("%-9s %5d | %3d ln %5d ch | %3d ln %5d ch | %3d ln %5d ch | %+5.0f%% chars\n",
              cs$id, cs$feats, o["lines"], o["chars"], s["lines"], s["chars"],
              l["lines"], l["chars"], 100 * (l["chars"] / o["chars"] - 1)))
}

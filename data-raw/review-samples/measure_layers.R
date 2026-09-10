# Where does the layer form pay for itself?
#
# The five review samples are all "one call vs one call", so they can only
# measure a rename.  This measures two spellings per case -- old and the plan
# LAYERS -- across a rising feature count, which is the axis the design is
# actually being judged on.
#
# The `rtf_plan_from()` shorthand this file used to measure as a third column
# was retired: 21 arguments were not easier to learn than 30, and it gave the
# package a second place to write a setting.  There is one spelling now.
#
# 06_LST is the case where the old side wins on characters by the largest
# margin, and it is worth seeing why: `listing = listing_spec(...)` is one
# argument that sets four others (group_col, group_by, split, drop_cols).
# Short, and four facts you cannot read off the call.

# Run from the repository root (the package worktree).
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
       layer = 'rtf_plan(dm) |>
  plan_group("Label") |>
  plan_pages(max_rows = 10) |>
  plan_style(border = "tfl")'),

  list(id = "04_LB", feats = 4,
       old = 'as_rtftables(lb, group_col = "PARAMCD", drop_cols = "PARAMCD",
             blank_rows = "between_groups", border = "tfl")',
       layer = 'rtf_plan(lb) |>
  plan_group("PARAMCD") |>
  plan_hide("PARAMCD") |>
  plan_blanks("between_groups") |>
  plan_style(border = "tfl")'),

  list(id = "02_AE", feats = 6,
       old = 'as_rtftables(ae, stub_vars = c("SOC", "PT"), split = "group_safe",
             max_rows = 12, group_by = "indent",
             blank_rows = "between_groups", border = "tfl")',
       layer = 'rtf_plan(ae) |>
  plan_stub(c("SOC", "PT")) |>
  plan_group("SOC", mode = "indent") |>
  plan_blanks("between_groups") |>
  plan_pages(max_rows = 12) |>
  plan_style(border = "tfl")'),

  list(id = "06_LST", feats = 5,
       old = 'ad_sorted <- ad[order(ad$ORD), ]
as_rtftables(ad_sorted, listing = listing_spec(listing_cols),
             max_rows = 16, border = "tfl")',
       layer = 'rtf_plan(ad) |>
  plan_listing(listing_cols) |>
  plan_roles(ORD = role("sort")) |>
  plan_blanks(first = TRUE) |>
  plan_pages(max_rows = 16) |>
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
       layer = 'rtf_plan(ae) |>
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

cat(sprintf("%-9s %5s | %-13s | %-13s | %s\n",
            "case", "feats", "old", "layers", "layers vs old"))
cat(strrep("-", 70), "\n")
for (cs in cases) {
  o <- sz(cs$old); l <- sz(cs$layer)
  cat(sprintf("%-9s %5d | %3d ln %5d ch | %3d ln %5d ch | %+5.0f%% chars\n",
              cs$id, cs$feats, o["lines"], o["chars"],
              l["lines"], l["chars"], 100 * (l["chars"] / o["chars"] - 1)))
}

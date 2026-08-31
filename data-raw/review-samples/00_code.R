# rtfreporter -- review samples
#
# Each block builds the SAME report two ways, with the SAME full
# feature set.  See 00_SETUP.md for switching between versions.

# ======================================================================
# DM  Demographics -- hidden sort carrier, spanning header, grouped blanks, group-safe pages
# ======================================================================

# --- OLD (as_rtftables) ---

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
    c("Characteristic", "Drug A", "Drug B", "Total")))

# --- NEW (rtf_plan + layers) ---

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
  rtf_pages()

# ======================================================================
# AE  Adverse events -- SOC/PT stub with a custom label, hidden sort carrier, spanning header, grouped blanks
# ======================================================================

# --- OLD (as_rtftables) ---

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
)

# --- NEW (rtf_plan + layers) ---

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
  rtf_pages()

# ======================================================================
# PK  Concentrations -- Time/Statistic stub, visits across the columns, spanning header, grouped blanks
# ======================================================================

# --- OLD (as_rtftables) ---

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
    c("Time / Statistic", "Day 1", "Day 7", "Day 14", "Day 28")))

# --- NEW (rtf_plan + layers) ---

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
  rtf_pages()

# ======================================================================
# LB  Shift table -- hidden grouping carrier, three-arm spanning header, grouped blanks
# ======================================================================

# --- OLD (as_rtftables) ---

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
    c("Baseline", rep(c("G0", "G1", "G2", "G3", "G4", "Total"), 3))))

# --- NEW (rtf_plan + layers) ---

pages <- rtf_plan(lb) |>
  plan_group("PARAMCD") |>
  plan_hide("PARAMCD") |>
  plan_blanks("between_groups") |>
  plan_header(list(col_cell(c(1, 2), ""), col_cell(c(3, 8), "Drug A"),
                   col_cell(c(9, 14), "Drug B"), col_cell(c(15, 20), "Total")),
              c("PARAMCD", "Baseline",
                rep(c("G0", "G1", "G2", "G3", "G4", "Total"), 3))) |>
  plan_style(border = "tfl") |>
  rtf_pages()

# ======================================================================
# AE by SOC  One page per SOC -- hidden carrier, spanning header, per-page naming
# ======================================================================

# --- OLD (as_rtftables) ---

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
)

# --- NEW (rtf_plan + layers) ---

pages <- rtf_plan(ae) |>
  plan_roles(SOCORD = role("sort", order = 1), PT = role("sort", order = 2),
             `Drug A____Events` = role("display", align = "right"),
             `Drug B____Events` = role("display", align = "right")) |>
  plan_hide("SOCORD") |>
  plan_group("SOC") |>
  plan_pages(per_group = TRUE) |>
  plan_style(border = "tfl",
             widths = c(2600L, 3000L, 1500L, 1200L, 1500L, 1200L)) |>
  rtf_pages()

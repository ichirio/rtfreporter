# rtfreporter -- review samples
#
# Each block builds the SAME report two ways.  Switch libraries
# (see 00_SETUP.md) and run whichever half your session supports.

# ======================================================================
# DM  Demographics: grouping + group-safe pagination
# ======================================================================

# --- OLD (as_rtftables) ---
as_rtftables(dm,
             split     = "group_safe",
             max_rows  = 10,
             group_col = "Label",
             border    = "tfl")

# --- NEW (rtf_plan_from) ---
rtf_plan_from(dm, group = "Label", max_rows = 10, border = "tfl")

# ======================================================================
# AE  SOC / PT stub + blank rows between SOCs
# ======================================================================

# --- OLD (as_rtftables) ---
as_rtftables(ae,
             stub_vars  = c("SOC", "PT"),
             split      = "group_safe",
             max_rows   = 12,
             group_by   = "indent",
             blank_rows = "between_groups",
             border     = "tfl")

# --- NEW (rtf_plan_from) ---
rtf_plan_from(ae,
              stub       = c("SOC", "PT"),
              group      = "SOC",
              group_mode = "indent",
              blanks     = "between_groups",
              max_rows   = 12,
              border     = "tfl")

# ======================================================================
# PK  Time / Statistic stub, visits across the columns
# ======================================================================

# --- OLD (as_rtftables) ---
as_rtftables(pk,
             stub_vars  = c("Time", "Statistic"),
             split      = "group_safe",
             max_rows   = 21,
             group_by   = "indent",
             blank_rows = "between_groups",
             border     = "tfl")

# --- NEW (rtf_plan_from) ---
rtf_plan_from(pk,
              stub       = c("Time", "Statistic"),
              group      = "Time",
              group_mode = "indent",
              blanks     = "between_groups",
              max_rows   = 21,
              border     = "tfl")

# ======================================================================
# LB  Shift table grouped by a column that is never printed
# ======================================================================

# --- OLD (as_rtftables) ---
as_rtftables(lb,
             group_col  = "PARAMCD",
             drop_cols  = "PARAMCD",
             blank_rows = "between_groups",
             border     = "tfl")

# --- NEW (rtf_plan_from) ---
rtf_plan_from(lb,
              group  = "PARAMCD",
              hide   = "PARAMCD",
              blanks = "between_groups",
              border = "tfl")

# ======================================================================
# AE  One page per SOC, each page named after it
# ======================================================================

# --- OLD (as_rtftables) ---
as_rtftables(ae,
             split     = "by_value",
             group_col = "SOC",
             border    = "tfl")

# --- NEW (rtf_plan_from) ---
rtf_plan_from(ae, group = "SOC", per_group = TRUE,
              border = "tfl")


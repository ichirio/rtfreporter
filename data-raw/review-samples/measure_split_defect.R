# The two-declaration-site defect (#328) and its structural fix (#334).
#
# This file used to REPRODUCE the defect: `group_col` could be written at top
# level OR inside a page_split_*() spec, and only the top-level one reached the
# blank-row decision -- silently, with no error and no warning.
#
# main has since retired the five page_split_*() factories (#334), so the
# second declaration site is gone.  What this file now checks is that the shape
# cannot be written any more, and that the one remaining site works.
#
# Run from the repository root (the package worktree).
suppressMessages(pkgload::load_all(".", quiet = TRUE))

## PT first, SOC second -- so the "first column" fallback is NOT the group column.
ae <- data.frame(
  PT  = c("Atrial fibrillation", "Bradycardia", "Tachycardia", "Palpitations",
          "Headache", "Dizziness", "Somnolence", "Tremor",
          "Nasopharyngitis", "Pneumonia", "Sinusitis"),
  SOC = c(rep("Cardiac disorders", 4), rep("Nervous system disorders", 4),
          rep("Infections", 3)),
  N   = as.character(1:11),
  stringsAsFactors = FALSE
)

br <- function(p) paste(p[[1]]$blank_rows, collapse = ",")

cat("=== blank_rows = \"between_groups\", group column is SOC (column 2) ===\n\n")
cat("1. group_col = \"SOC\"  at top level                 : ",
    br(as_rtftables(ae, group_col = "SOC", blank_rows = "between_groups")), "\n")
cat("2. group_col = \"SOC\"  inside page_split_group_safe(): ",
    tryCatch({
      br(as_rtftables(ae,
                      split = page_split_group_safe(group_col = "SOC",
                                                    max_rows = 99),
                      blank_rows = "between_groups"))
    }, error = function(e) paste("ERROR:", conditionMessage(e))), "\n")
cat("3. group_col nowhere                                : ",
    br(as_rtftables(ae, blank_rows = "between_groups")), "\n")

cat("\n正しいのは 1 だけ（SOC が変わる 4 と 8）。\n")
cat("2 は #334 で書けなくなりました。以前は「指定しているのに 3 と同じ」、\n")
cat("つまり指定が空行判定に届かないまま、エラーも警告も出ない形でした。\n")
cat("宣言の場所が 1 つになったので、この不具合の形は再現できません。\n")

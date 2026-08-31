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
    br(as_rtftables(ae, split = page_split_group_safe(group_col = "SOC", max_rows = 99),
                    blank_rows = "between_groups")), "\n")
cat("3. group_col nowhere                                : ",
    br(as_rtftables(ae, blank_rows = "between_groups")), "\n")

cat("\n正しいのは 1 だけ（SOC が変わる 4 と 8）。\n")
cat("2 は group_col を指定しているのに 3 と同じ = 指定が空行判定に届いていない。\n")
cat("エラーも警告もありません。\n")

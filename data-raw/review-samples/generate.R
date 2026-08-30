setwd("C:/Yrepo/rtfreporter")
suppressMessages(devtools::load_all(".", quiet = TRUE))

OUT <- "C:/Users/ichir/AppData/Local/Temp/claude/C--Users-ichir/b4487e19-8699-4c2f-a213-30dd694f23aa/scratchpad/review"
unlink(OUT, recursive = TRUE); dir.create(OUT, recursive = TRUE)

dd <- system.file("extdata", package = "rtfreporter")
dm <- readRDS(file.path(dd, "demog_p1.rds"))
lb <- readRDS(file.path(dd, "lab_alt.rds")); lb$PARAMCD <- "ALT"

set.seed(11)
soc <- c("Cardiac disorders", "Gastrointestinal disorders",
         "Nervous system disorders", "Infections and infestations")
pts <- list(c("Atrial fibrillation", "Bradycardia", "Tachycardia"),
            c("Nausea", "Vomiting", "Diarrhoea", "Constipation"),
            c("Headache", "Dizziness"),
            c("Nasopharyngitis", "Urinary tract infection", "Pneumonia"))
ae <- do.call(rbind, lapply(seq_along(soc), function(i) {
  data.frame(SOC = soc[i], PT = pts[[i]],
             DrugA = sprintf("%d (%.1f%%)", sample(1:12, length(pts[[i]])),
                             runif(length(pts[[i]]), 1, 9)),
             DrugB = sprintf("%d (%.1f%%)", sample(1:12, length(pts[[i]])),
                             runif(length(pts[[i]]), 1, 9)),
             stringsAsFactors = FALSE)
}))

VIS <- c("Day 1", "Day 7", "Day 14", "Day 28")
TIMES <- c(0.5, 1, 2, 4, 8, 12, 24)
STATS <- c("n", "Mean", "SD", "CV%", "Median", "Min, Max")
set.seed(277)
rows <- list()
for (t in TIMES) {
  rows[[length(rows) + 1L]] <- c(sprintf("%g h", t), "", rep("", length(VIS)))
  for (s in STATS) rows[[length(rows) + 1L]] <- c(sprintf("%g h", t), s,
    vapply(seq_along(VIS), function(v) sprintf("%.2f", runif(1, 1, 2000)),
           character(1L)))
}
pk <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
names(pk) <- c("Time", "Statistic", VIS)

# ── the five cases ─────────────────────────────────────────────────────────
cases <- list(
  list(id = "01_DM", title = "DM  Demographics: grouping + group-safe pagination",
    old_src = 'as_rtftables(dm,
             split     = "group_safe",
             max_rows  = 10,
             group_col = "Label",
             border    = "tfl")',
    new_src = 'rtf_plan_from(dm, group = "Label", max_rows = 10, border = "tfl")',
    old = function() as_rtftables(dm, split = "group_safe", max_rows = 10L,
                                  group_col = "Label", border = "tfl"),
    new = function() rtf_plan_from(dm, group = "Label", max_rows = 10L,
                                   border = "tfl")),

  list(id = "02_AE", title = "AE  SOC / PT stub + blank rows between SOCs",
    old_src = 'as_rtftables(ae,
             stub_vars  = c("SOC", "PT"),
             split      = "group_safe",
             max_rows   = 12,
             group_by   = "indent",
             blank_rows = "between_groups",
             border     = "tfl")',
    new_src = 'rtf_plan_from(ae,
              stub       = c("SOC", "PT"),
              group      = "SOC",
              group_mode = "indent",
              blanks     = "between_groups",
              max_rows   = 12,
              border     = "tfl")',
    old = function() as_rtftables(ae, stub_vars = c("SOC", "PT"),
      split = "group_safe", max_rows = 12L, group_by = "indent",
      blank_rows = "between_groups", border = "tfl"),
    new = function() rtf_plan_from(ae, stub = c("SOC", "PT"), group = "SOC",
      group_mode = "indent", blanks = "between_groups", max_rows = 12L,
      border = "tfl")),

  list(id = "03_PK", title = "PK  Time / Statistic stub, visits across the columns",
    old_src = 'as_rtftables(pk,
             stub_vars  = c("Time", "Statistic"),
             split      = "group_safe",
             max_rows   = 21,
             group_by   = "indent",
             blank_rows = "between_groups",
             border     = "tfl")',
    new_src = 'rtf_plan_from(pk,
              stub       = c("Time", "Statistic"),
              group      = "Time",
              group_mode = "indent",
              blanks     = "between_groups",
              max_rows   = 21,
              border     = "tfl")',
    old = function() as_rtftables(pk, stub_vars = c("Time", "Statistic"),
      split = "group_safe", max_rows = 21L, group_by = "indent",
      blank_rows = "between_groups", border = "tfl"),
    new = function() rtf_plan_from(pk, stub = c("Time", "Statistic"),
      group = "Time", group_mode = "indent", blanks = "between_groups",
      max_rows = 21L, border = "tfl")),

  list(id = "04_LB", title = "LB  Shift table grouped by a column that is never printed",
    old_src = 'as_rtftables(lb,
             group_col  = "PARAMCD",
             drop_cols  = "PARAMCD",
             blank_rows = "between_groups",
             border     = "tfl")',
    new_src = 'rtf_plan_from(lb,
              group  = "PARAMCD",
              hide   = "PARAMCD",
              blanks = "between_groups",
              border = "tfl")',
    old = function() as_rtftables(lb, group_col = "PARAMCD",
      drop_cols = "PARAMCD", blank_rows = "between_groups", border = "tfl"),
    new = function() rtf_plan_from(lb, group = "PARAMCD", hide = "PARAMCD",
      blanks = "between_groups", border = "tfl")),

  list(id = "05_AE_by_SOC", title = "AE  One page per SOC, each page named after it",
    old_src = 'as_rtftables(ae,
             split     = "by_value",
             group_col = "SOC",
             border    = "tfl")',
    new_src = 'rtf_plan_from(ae, group = "SOC", per_group = TRUE,
              border = "tfl")',
    old = function() as_rtftables(ae, split = "by_value", group_col = "SOC",
                                  border = "tfl"),
    new = function() rtf_plan_from(ae, group = "SOC", per_group = TRUE,
                                   border = "tfl"))
)

render <- function(x, f) {
  doc <- rtf_document(page = rtf_page(orientation = "landscape"))
  if (inherits(x, "rtf_plan")) doc <- rtf_tables(doc, x)
  else for (p in x) doc <- rtf_tables(doc, p)
  generate_rtfreport(doc, f, overwrite = TRUE)
  readLines(f, warn = FALSE)
}

code <- file.path(OUT, "00_code.R")
con <- file(code, "w", encoding = "UTF-8")
writeLines(c("# rtfreporter -- review samples",
             "#",
             "# Each block builds the SAME report two ways.  Switch libraries",
             "# (see 00_SETUP.md) and run whichever half your session supports.",
             ""), con)

cat(sprintf("%-14s %-8s %-8s %s\n", "case", "pages", "RTF", "title"))
for (cs in cases) {
  fo <- file.path(OUT, sprintf("%s_old.rtf", cs$id))
  fn <- file.path(OUT, sprintf("%s_new.rtf", cs$id))
  a <- cs$old()
  lo <- render(a, fo)
  ln <- render(cs$new(), fn)
  cat(sprintf("%-14s %-8d %-8s %s\n", cs$id, length(a),
              if (identical(lo, ln)) "IDENTICAL" else "DIFFERS", cs$title))
  writeLines(c(paste0("# ", strrep("=", 70)),
               paste0("# ", cs$title),
               paste0("# ", strrep("=", 70)),
               "",
               "# --- OLD (as_rtftables) ---",
               cs$old_src,
               "",
               "# --- NEW (rtf_plan_from) ---",
               cs$new_src,
               ""), con)
}
close(con)
cat("\nwrote", length(list.files(OUT)), "files to", OUT, "\n")

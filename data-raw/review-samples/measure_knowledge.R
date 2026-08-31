# What must you KNOW to write the full-set table?
#
# Character count is a wash (measured separately: layers are +8..+16% for
# small tables and break even at ten features).  The design is not being
# judged on characters, so this measures the thing it IS being judged on:
# how much you must hold in your head, and how much of it you cannot read
# off your own data.

setwd("C:/Users/ichir/AppData/Local/Temp/claude/C--Users-ichir/0e8953d4-534f-4543-9b50-71c7b61ba96a/scratchpad/plan-wt")
suppressMessages(pkgload::load_all(".", quiet = TRUE))

rule <- function() cat(strrep("-", 78), "\n")

cat("\n############ 1. NAMES YOU MUST KNOW ############\n\n")

old_args <- c("stub_vars", "stub_label", "drop_cols", "group_col", "group_by",
              "blank_rows", "count_blank_rows", "split", "max_rows", "border",
              "column_widths_twips", "col_spec")
new_fns  <- c("rtf_plan_from", "plan_stub", "plan_roles", "plan_hide",
              "plan_group", "plan_blanks", "plan_pages", "plan_style")
new_args <- c("label", "order", "align", "mode", "max_rows", "border", "widths")

cat("OLD: one function, ", length(old_args), " argument names, all in one flat list\n", sep = "")
cat("     ", paste(old_args, collapse = ", "), "\n\n")
cat("NEW: ", length(new_fns), " layer functions, ", length(new_args),
    " argument names spread across them\n", sep = "")
cat("     ", paste(new_fns, collapse = ", "), "\n")
cat("     ", paste(new_args, collapse = ", "), "\n")
cat("\n-> a wash on raw count.  The difference is not HOW MANY, it is WHICH.\n")

cat("\n############ 2. THINGS NOT READABLE FROM YOUR DATA ############\n")
cat("   (values you cannot look up in names(df) -- you must have learnt them)\n\n")

cat("OLD\n")
cat("  1. \"group_safe\"  -- a strategy name.  Also \"group_force\", \"by_value\",\n")
cat("                     \"rows\", \"none\".  Which one keeps a group whole?\n")
cat("  2. \"indent\"      -- that group detection reads indentation, not a column\n")
cat("  3. \"System Organ Class / Preferred Term\"\n")
cat("                  -- the GENERATED name of the merged stub column, which\n")
cat("                     group_col must be given.  It is whatever stub_label\n")
cat("                     said; without stub_label it is \"SOC / PT\".\n")
cat("  4. col = 3, col = 5\n")
cat("                  -- POSITIONS after the stub merged two columns and\n")
cat("                     drop_cols removed a third.  In the source they are 5 and 7.\n")
cat("  5. that sort_by cannot reach PT at all, so the sort happens OUTSIDE the call\n")
cat("  -> 5 facts, none of them in your data\n\n")

cat("NEW\n")
cat("  1. \"indent\"      -- same fact, same place\n")
cat("  -> 1 fact.  Every column is named as it is named in `ae`:\n")
cat("     SOC, PT, SOCORD, `Drug A____Events`, `Drug B____Events`\n")

cat("\n############ 3. IS A MISTAKE CAUGHT? ############\n\n")
set.seed(11)
soc <- c("Cardiac disorders","Gastrointestinal disorders")
ae2 <- data.frame(SOCORD = c(2L,2L,1L), SOC = c(soc[2],soc[2],soc[1]),
                  PT = c("p2","p1","p3"), N = c("1","2","3"),
                  stringsAsFactors = FALSE)

probe <- function(lbl, expr) {
  r <- try(eval(expr), silent = TRUE)
  cat(sprintf("  %-52s %s\n", lbl,
      if (inherits(r, "try-error"))
        paste("ERROR:", sub("\n.*", "", conditionMessage(attr(r, "condition"))))
      else "accepted, no message"))
}
cat("OLD -- name the stub column the way it appears in your data:\n")
probe('group_col = "SOC"', quote(as_rtftables(ae2, stub_vars=c("SOC","PT"), group_col="SOC")))
probe('sort_by  = "PT"',   quote(as_rtftables(ae2, stub_vars=c("SOC","PT"), sort_by="PT")))
cat("\nOLD -- put group_col inside the split strategy instead of at top level:\n")
probe('split = page_split_group_safe(group_col = "SOC")',
      quote(as_rtftables(ae2, split = page_split_group_safe(group_col = "SOC"),
                         max_rows = 2, blank_rows = "between_groups")))
cat("     (^ this one is the dangerous shape: it is ACCEPTED, and the blank\n")
cat("        rows silently come out wrong -- see the opening post)\n")

cat("\nNEW -- the same intent, named in source coordinates:\n")
probe('plan_group("SOC") after plan_stub(c("SOC","PT"))',
      quote(plan_tables(rtf_plan_from(ae2) |> plan_stub(c("SOC","PT")) |>
                        plan_group("SOC", mode="indent"))))
probe('plan_roles(PT = role("sort"))',
      quote(plan_tables(rtf_plan_from(ae2) |> plan_stub(c("SOC","PT")) |>
                        plan_roles(PT = role("sort", order = 1)))))
probe('plan_group("NOPE") -- a column that does not exist',
      quote(plan_tables(rtf_plan_from(ae2) |> plan_group("NOPE"))))

cat("\n############ 4. CAN YOU ADD ONE FEATURE WITHOUT TOUCHING THE REST? ############\n\n")
cat("The question a maintainer actually faces six months later.\n\n")
cat("OLD: adding a hidden sort carrier to the AE table means\n")
cat("     - adding sort_by=, discovering it cannot see PT after the stub,\n")
cat("     - moving the sort out of the call entirely,\n")
cat("     - and re-checking that drop_cols/group_col still name the right thing.\n")
cat("     The change is not local: three arguments interact.\n\n")
cat("NEW: one more line.\n")
cat("     plan_roles(SOCORD = role(\"sort\", order = 1)) |> plan_hide(\"SOCORD\")\n")
cat("     Nothing above or below it changes.\n")

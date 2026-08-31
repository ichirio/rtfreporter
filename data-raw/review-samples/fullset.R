# ============================================================================
#  Full-set comparison: everything a shippable AE table actually needs.
#
#  The five existing samples compare one call with one call, so they can only
#  show a rename.  This one turns on the whole feature set at once -- spanning
#  column header, hidden sort carrier, merged indented stub, blank rows between
#  groups, group-safe pagination, per-column alignment, widths, borders --
#  which is where "one big function" and "small layers" actually differ.
# ============================================================================

setwd("C:/Users/ichir/AppData/Local/Temp/claude/C--Users-ichir/0e8953d4-534f-4543-9b50-71c7b61ba96a/scratchpad/plan-wt")
suppressMessages(pkgload::load_all(".", quiet = TRUE))

## ---- the source table --------------------------------------------------
## Deliberately awkward in the ordinary ways: a carrier column that decides
## the order but must not print, and delimited names that encode a spanning
## header.
set.seed(11)
soc <- c("Cardiac disorders", "Gastrointestinal disorders",
         "Nervous system disorders", "Infections and infestations")
pts <- list(c("Atrial fibrillation", "Bradycardia", "Tachycardia"),
            c("Nausea", "Vomiting", "Diarrhoea", "Constipation"),
            c("Headache", "Dizziness"),
            c("Nasopharyngitis", "Urinary tract infection", "Pneumonia"))
socord <- c(3L, 1L, 4L, 2L)          # medical order, not alphabetical

ae <- do.call(rbind, lapply(seq_along(soc), function(i) {
  n <- length(pts[[i]])
  data.frame(
    SOCORD = socord[i],
    SOC    = soc[i],
    PT     = pts[[i]],
    `Drug A____n (%)`   = sprintf("%d (%.1f%%)", sample(1:12, n), runif(n, 1, 9)),
    `Drug A____Events`  = as.character(sample(1:20, n)),
    `Drug B____n (%)`   = sprintf("%d (%.1f%%)", sample(1:12, n), runif(n, 1, 9)),
    `Drug B____Events`  = as.character(sample(1:20, n)),
    stringsAsFactors = FALSE, check.names = FALSE)
}))

widths <- c(4200L, 1500L, 1200L, 1500L, 1200L)

## ======================================================================
## OLD -- as_rtftables()
## ======================================================================
old_src <- '
# sort_by cannot reach PT: the stub merged it away, and every column argument
# runs in POST-stub coordinates.  So the ordering has to happen outside the API.
ae_sorted <- ae[order(ae$SOCORD, ae$PT), ]

pages_old <- as_rtftables(
  ae_sorted,
  stub_vars           = c("SOC", "PT"),
  stub_label          = "System Organ Class / Preferred Term",
  drop_cols           = "SOCORD",
  group_col           = "System Organ Class / Preferred Term",
  group_by            = "indent",
  blank_rows          = "between_groups",
  count_blank_rows    = TRUE,
  split               = "group_safe",
  max_rows            = 12,
  border              = "tfl",
  column_widths_twips = widths,
  # col_spec is POSITIONAL, and the positions are the ones left AFTER the stub
  # merged two columns into one and drop_cols removed a third.  "Events" is
  # source column 5 and 7; here it is 3 and 5.
  col_spec            = list(list(col = 3, align = "right"),
                             list(col = 5, align = "right"))
)'

ae_sorted <- ae[order(ae$SOCORD, ae$PT), ]

pages_old <- as_rtftables(
  ae_sorted,
  stub_vars           = c("SOC", "PT"),
  stub_label          = "System Organ Class / Preferred Term",
  drop_cols           = "SOCORD",
  group_col           = "System Organ Class / Preferred Term",
  group_by            = "indent",
  blank_rows          = "between_groups",
  count_blank_rows    = TRUE,
  split               = "group_safe",
  max_rows            = 12,
  border              = "tfl",
  column_widths_twips = widths,
  # col_spec is POSITIONAL, and the positions are the ones left AFTER the stub
  # merged two columns into one and drop_cols removed a third.  "Events" is
  # source column 5 and 7; here it is 3 and 5.
  col_spec            = list(list(col = 3, align = "right"),
                             list(col = 5, align = "right"))
)

## ======================================================================
## NEW -- layers
## ======================================================================
new_src <- '
plan_new <- rtf_plan_from(ae) |>
  plan_stub(c("SOC", "PT"), label = "System Organ Class / Preferred Term") |>
  plan_roles(SOCORD = role("sort", order = 1), PT = role("sort", order = 2)) |>
  plan_hide("SOCORD") |>
  plan_group("SOC", mode = "indent") |>
  plan_blanks("between_groups") |>
  plan_pages(max_rows = 12) |>
  plan_roles(`Drug A____Events` = role("display", align = "right"),
             `Drug B____Events` = role("display", align = "right")) |>
  plan_style(border = "tfl", widths = widths)'

plan_new <- rtf_plan_from(ae) |>
  plan_stub(c("SOC", "PT"), label = "System Organ Class / Preferred Term") |>
  plan_roles(SOCORD = role("sort", order = 1), PT = role("sort", order = 2)) |>
  plan_hide("SOCORD") |>
  plan_group("SOC", mode = "indent") |>
  plan_blanks("between_groups") |>
  plan_pages(max_rows = 12) |>
  plan_roles(`Drug A____Events` = role("display", align = "right"),
             `Drug B____Events` = role("display", align = "right")) |>
  plan_style(border = "tfl", widths = widths)

pages_new <- plan_tables(plan_new)

## ---- do they agree? ----------------------------------------------------
cat("=========== OUTPUT EQUIVALENCE ===========\n")
cat("old pages:", length(pages_old), "  new pages:", length(pages_new), "\n")
cmp <- plan_compare("fullset", pages_old, pages_new)
show_comparison(cmp)

cat("\n=========== PAGE 1, OLD ===========\n")
print(pages_old[[1]])
cat("\n=========== PAGE 1, NEW ===========\n")
print(pages_new[[1]])

## ---- measurements ------------------------------------------------------
nz <- function(s) sum(nchar(trimws(strsplit(s, "\n")[[1]])) > 0)
cat("\n=========== SIZE ===========\n")
cat(sprintf("old: %2d non-blank lines, %4d characters\n",
            nz(old_src), nchar(gsub("[ \n]", "", old_src))))
cat(sprintf("new: %2d non-blank lines, %4d characters\n",
            nz(new_src), nchar(gsub("[ \n]", "", new_src))))

## ---- write the RTF pair, next to the other review samples ---------------
render_to <- function(pages, f) {
  doc <- rtf_document(page = rtf_page(orientation = "landscape"))
  for (p in pages) doc <- rtf_tables(doc, p)
  generate_rtfreport(doc, f, overwrite = TRUE)
}
render_to(pages_old, "data-raw/review-samples/06_FULLSET_old.rtf")
render_to(pages_new, "data-raw/review-samples/06_FULLSET_new.rtf")
cat("\nwrote 06_FULLSET_old.rtf / 06_FULLSET_new.rtf\n")

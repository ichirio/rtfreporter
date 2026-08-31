# border_zones.R -- What does each argument of rtf_table_border() actually draw?
#
# Written for the API review (issue #336, discussion #324).  The five zone
# arguments are rendered in five different colours so the mapping
# "argument -> rule on the page" can be read straight off the output.
#
#   spanning  red     the spanning-header row(s)
#   header    blue    the column-header block (top of the first header row,
#                     bottom of the last one) -- and the automatic group
#                     underline under a multi-column spanning cell
#   body      green   every data row
#   first_row amber   the first data row (merged on top of `body`)
#   last_row  purple  the last data row  (merged on top of `body`)
#
# Run from the package root:
#   Rscript data-raw/review-samples/border_zones.R
# Writes output/border_zones.rtf -- open it in Word next to border_zones.svg.

# Render through the source tree, never a stale install (#306).
source("data-raw/_load.R")

RED    <- "#C9372C"   # spanning
BLUE   <- "#1F6FEB"   # header
GREEN  <- "#1A7F37"   # body
AMBER  <- "#B36B00"   # first_row
PURPLE <- "#8250DF"   # last_row

rule <- function(color) rtf_border_side(style = "single", width = 30L, color = color)

zones <- rtf_table_border(
  spanning  = rtf_border(top    = rule(RED)),
  header    = rtf_border(top    = rule(BLUE), bottom = rule(BLUE)),
  body      = rtf_border(bottom = rule(GREEN)),
  first_row = rtf_border(bottom = rule(AMBER)),
  last_row  = rtf_border(bottom = rule(PURPLE))
)

df <- data.frame(
  Characteristic = c("Age (years)", "  Mean (SD)", "  Median",
                     "Sex, n (%)", "  Male", "  Female"),
  A = c("", "62.1 (10.2)", "61.5", "", "45 (52.3)", "41 (47.7)"),
  B = c("", "61.4 (9.8)", "60.0", "", "41 (48.8)", "43 (51.2)"),
  stringsAsFactors = FALSE
)

tbl <- rtftable(
  df,
  col_header = rtf_col_header(
    list(col_cell(1, ""), col_cell(c(2, 3), "Treatment Group")),
    c("Characteristic", "Drug A (N=86)", "Drug B (N=84)")
  ),
  column_widths_twips = c(4320L, 2880L, 2880L),
  col_spec = list(list(col = 1, align = "left"),
                  list(col = 2, align = "center"),
                  list(col = 3, align = "center")),
  row_height_twips = 260L,
  border = zones
)

doc <- rtf_document() |>
  rtf_config(page = list(orientation = "landscape",
                         margin_top_in = 1, margin_bottom_in = 1,
                         margin_left_in = 1, margin_right_in = 1)) |>
  rtf_tables(list(tbl))

dir.create("output", showWarnings = FALSE)
out <- "output/border_zones.rtf"
generate_rtfreport(doc, out, overwrite = TRUE)
cat("Generated:", normalizePath(out), "\n")

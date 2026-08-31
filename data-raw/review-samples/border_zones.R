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
# The second table on page 2 does the same for rtf_border() itself: the four
# sides of ONE row, each in its own colour.
#
#   top     pink     the rule above the row
#   bottom  teal     the rule below the row
#   left    olive    the left edge of EVERY cell in the row
#   right   indigo   the right edge of EVERY cell in the row
#
# left/right are emitted per cell (.build_cell_defs applies the same border
# commands to every \cellx), so a row-level rtf_border(left=, right=) draws a
# vertical rule at every column boundary -- not just the table's outer frame.
#
# Run from the package root:
#   Rscript data-raw/review-samples/border_zones.R
# Writes output/border_zones.rtf -- open it in Word next to border_zones.svg
# and border_cell.svg.

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

make_tbl <- function(border) rtftable(
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
  border = border
)

# -- Page 2: rtf_border() -- the four sides of one row --------------------------
#
# A different palette on purpose: these four colours are SIDES, not zones.
PINK   <- "#BF3989"   # top
TEAL   <- "#0B7285"   # bottom
OLIVE  <- "#9A6700"   # left
INDIGO <- "#6639BA"   # right
GREY   <- "#8C959F"   # plain header rules, so the demo row stands alone

four_sides <- rtf_border(
  top    = rule(PINK),
  bottom = rule(TEAL),
  left   = rule(OLIVE),
  right  = rule(INDIGO)
)

# The same rtf_border() carried by one zone: `last_row` says WHICH row,
# `four_sides` says WHAT that row's border is.
sides <- rtf_table_border(
  header   = rtf_border(top = rule(GREY), bottom = rule(GREY)),
  last_row = four_sides
)

doc <- rtf_document() |>
  rtf_config(page = list(orientation = "landscape",
                         margin_top_in = 1, margin_bottom_in = 1,
                         margin_left_in = 1, margin_right_in = 1)) |>
  rtf_tables(list(make_tbl(zones))) |>
  rtf_tables(list(make_tbl(sides)))

dir.create("output", showWarnings = FALSE)
out <- "output/border_zones.rtf"
generate_rtfreport(doc, out, overwrite = TRUE)
cat("Generated:", normalizePath(out), "\n")

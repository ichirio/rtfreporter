# Lay another plan's rows under a table's

`plan_append()` builds one table from two sources: the rows of `.plan`
(the **parent**: say, a listing of each subject's concentrations) and
under them the rows of `part` (the **child**: their summary statistics),
on the same columns. Each is an ordinary plan, run to its table on its
own; the tables are stacked before the pages are cut, so a page holds
every part's rows of its group. The whole table's settings — header,
widths, pages, style, titles — are the parent's, written before or after
`plan_append()`.

## Usage

``` r
plan_append(.plan, part, name = NULL, absent = c("error", "skip"))
```

## Arguments

- .plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md):
  the parent, whose column axis and header the table has.

- part:

  Another
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md):
  its rows go **under** the rows of `.plan`. Several `plan_append()`
  calls stack their parts in the order written. The part's plan says how
  its own rows are made and nothing about the whole table (see *Parts*
  below).

- name:

  The part's name, for its messages and for
  [`plan_layers()`](https://ichirio.github.io/rtfreporter/reference/plan_layers.md):
  `"part2"`, `"part3"`, ... by default (the parent is `"part1"`). One
  non-empty string, not already taken.

- absent:

  What a page group (a value of `plan_paginate_rows(page_by = )`) the
  part has no rows for does: `"error"` (default: a report is not defined
  by a group that is missing), or `"skip"` (that page has no rows of
  this part).

## Value

The plan, with one more layer.
[`plan_apply()`](https://ichirio.github.io/rtfreporter/reference/plan_apply.md)
of it gives the stacked table, with a `.part` column naming each row's
part.

## Details

A listing cannot be appended to: a plan that carries
[`plan_listing()`](https://ichirio.github.io/rtfreporter/reference/plan_listing.md)
stops here, as the parent or as a part, because a listing's columns are
the records' own, while the stacked parts are a `label` and value
columns. To show a listing with a summary, make them separate contents
of the document:
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
for the listing, then
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
for the summary with `page_break = FALSE` (see the article "Several
contents on one page").

## Parts

Each `plan_append()` call adds a part of its own, below the ones added
before it; a later call does not replace an earlier one.

`plan_append(part)` lays another plan's rows under this plan's. The
first plan (the **parent**) is an ordinary table with its column axis
and header; a **part** (the child) is an ordinary plan too, written as
any table is, and may carry only the verbs that make its rows:
[`plan_cells()`](https://ichirio.github.io/rtfreporter/reference/plan_cells.md),
[`plan_digits()`](https://ichirio.github.io/rtfreporter/reference/plan_digits.md),
[`plan_labels()`](https://ichirio.github.io/rtfreporter/reference/plan_labels.md),
[`plan_levels()`](https://ichirio.github.io/rtfreporter/reference/plan_levels.md)
of its row keys,
[`plan_stub()`](https://ichirio.github.io/rtfreporter/reference/plan_stub.md),
[`plan_row_group()`](https://ichirio.github.io/rtfreporter/reference/plan_row_group.md),
[`plan_filter()`](https://ichirio.github.io/rtfreporter/reference/plan_filter.md),
[`plan_sort()`](https://ichirio.github.io/rtfreporter/reference/plan_sort.md),
[`plan_nest()`](https://ichirio.github.io/rtfreporter/reference/plan_nest.md),
[`plan_total()`](https://ichirio.github.io/rtfreporter/reference/plan_total.md),
[`plan_compare()`](https://ichirio.github.io/rtfreporter/reference/plan_compare.md),
[`plan_stat_groups()`](https://ichirio.github.io/rtfreporter/reference/plan_stat_groups.md),
[`plan_hide()`](https://ichirio.github.io/rtfreporter/reference/plan_hide.md),
[`plan_empty()`](https://ichirio.github.io/rtfreporter/reference/plan_empty.md),
[`plan_cell_style()`](https://ichirio.github.io/rtfreporter/reference/plan_cell_style.md)
with a condition, and `plan_blanks(first =, last =)` (a blank row above
or below its own rows). Its `cols` key is the parent's (or left out).

The whole table's settings are the parent's, written anywhere in the
parent, before or after `plan_append()`, the later of two winning as
always: the column order
([`plan_levels()`](https://ichirio.github.io/rtfreporter/reference/plan_levels.md)
of the column key), the header
([`plan_col_header()`](https://ichirio.github.io/rtfreporter/reference/plan_col_header.md)),
the widths and decimal split
([`plan_columns()`](https://ichirio.github.io/rtfreporter/reference/plan_columns.md)),
the pages
([`plan_paginate_rows()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_rows.md),
[`plan_paginate_cols()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_cols.md)),
the blank rows between parts (`plan_blanks(where = "between_parts")`,
and `first` / `last` at the table's top and bottom), the style
([`plan_style()`](https://ichirio.github.io/rtfreporter/reference/plan_style.md)),
titles, footnotes and the steps after. A part that carries one of them
stops.

What a part does not say, it takes from the parent, key by key: the cell
templates, the digits, the labels, the levels of its row keys, the stub
fold; what it says itself wins. The empty-cell text is each part's own
(`plan_cells(na = )`, empty by default): the parent's is no statistic's.
Its row keys and label are folded into one label column (the parent's
`plan_stub(name =, indent =)`), so every part ends as
`[page keys] [label] [value columns]`: a constant row key
(`rows = c(block = ~ "Summary Statistics")`) is the heading row above
its rows, as in any table, and stays empty. Columns a part does not
have, and cells it has no value for, print its own empty-cell text.

The parts are stacked before the pages are cut: a page of `page_by`
value X holds the parent's rows of X, then each part's rows of X,
matched by **value**. A blank row between the parts
(`plan_blanks(where = "between_parts")`) is a row of the stack, so it
takes a place in `max_rows`, and a group cut by a page break is
continued on the next page by its heading row with `cont_label`. Every
part's data must have the `page_by` columns (it is not written to every
page by default); a value the parent has and a part has not stops,
unless that part says `absent = "skip"`; a value only a part has makes
no page.

A part run on its own (`plan_apply(part, "table")`) is the same rows for
every group at once, with only what it says itself: that is the whole
difference from the part in the table.

## See also

[plan_verbs](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
for every verb.

Other plan verbs:
[`plan_after()`](https://ichirio.github.io/rtfreporter/reference/plan_after.md),
[`plan_blanks()`](https://ichirio.github.io/rtfreporter/reference/plan_blanks.md),
[`plan_cell_style()`](https://ichirio.github.io/rtfreporter/reference/plan_cell_style.md),
[`plan_cells()`](https://ichirio.github.io/rtfreporter/reference/plan_cells.md),
[`plan_col_header()`](https://ichirio.github.io/rtfreporter/reference/plan_col_header.md),
[`plan_columns()`](https://ichirio.github.io/rtfreporter/reference/plan_columns.md),
[`plan_compare()`](https://ichirio.github.io/rtfreporter/reference/plan_compare.md),
[`plan_digits()`](https://ichirio.github.io/rtfreporter/reference/plan_digits.md),
[`plan_empty()`](https://ichirio.github.io/rtfreporter/reference/plan_empty.md),
[`plan_filter()`](https://ichirio.github.io/rtfreporter/reference/plan_filter.md),
[`plan_footnotes()`](https://ichirio.github.io/rtfreporter/reference/plan_footnotes.md),
[`plan_hide()`](https://ichirio.github.io/rtfreporter/reference/plan_hide.md),
[`plan_labels()`](https://ichirio.github.io/rtfreporter/reference/plan_labels.md),
[`plan_levels()`](https://ichirio.github.io/rtfreporter/reference/plan_levels.md),
[`plan_listing()`](https://ichirio.github.io/rtfreporter/reference/plan_listing.md),
[`plan_nest()`](https://ichirio.github.io/rtfreporter/reference/plan_nest.md),
[`plan_paginate_cols()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_cols.md),
[`plan_paginate_group()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_group.md),
[`plan_paginate_rows()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_rows.md),
[`plan_row_group()`](https://ichirio.github.io/rtfreporter/reference/plan_row_group.md),
[`plan_sort()`](https://ichirio.github.io/rtfreporter/reference/plan_sort.md),
[`plan_stat_groups()`](https://ichirio.github.io/rtfreporter/reference/plan_stat_groups.md),
[`plan_stub()`](https://ichirio.github.io/rtfreporter/reference/plan_stub.md),
[`plan_style()`](https://ichirio.github.io/rtfreporter/reference/plan_style.md),
[`plan_titles()`](https://ichirio.github.io/rtfreporter/reference/plan_titles.md),
[`plan_total()`](https://ichirio.github.io/rtfreporter/reference/plan_total.md)

## Examples

``` r
# Invented concentrations: three subjects, three time points; S-02's
# 4 h sample is below the limit of quantitation (NA)
conc <- data.frame(
  USUBJID = rep(c("S-01", "S-02", "S-03"), each = 3),
  ATPT    = rep(c("1 h", "2 h", "4 h"), 3),
  AVAL    = c(12.1, 8.4, 3.2, 10.5, 7.9, NA, 14.0, 9.6, 4.1))
listing <- table_plan(conc, cols = "ATPT", rows = c(subject = ~ "{USUBJID}"),
                      value = "AVAL") |>
  plan_cells("{value:.1f}", na = "BLQ")

# Their summary, one row a statistic, under one heading row
smry <- data.frame(
  ATPT      = rep(c("1 h", "2 h", "4 h"), each = 2),
  variable  = "AVAL", .label = NA, .kind = "continuous",
  stat_name = c("N", "mean"),
  stat      = c(3, 12.20, 3, 8.633, 2, 3.65))
summary <- table_plan(smry, cols = "ATPT", rows = c(block = ~ "Summary")) |>
  plan_cells(c(N = "{N:.0f}", Mean = "{mean:.2f}"))

# Minimal: the summary's rows under the listing's
listing |> plan_append(summary) |> plan_apply("table")
#>      label   1 h  2 h  4 h .part
#> 1     S-01  12.1  8.4  3.2 part1
#> 2     S-02  10.5  7.9  BLQ part1
#> 3     S-03  14.0  9.6  4.1 part1
#> 4  Summary                 part2
#> 5        N     3    3    2 part2
#> 6     Mean 12.20 8.63 3.65 part2

# The whole table's settings are the parent's: a blank row between the
# parts, and one header for both
pages <- listing |>
  plan_append(summary, name = "summary") |>
  plan_blanks(where = "between_parts") |>
  plan_col_header(rtf_col_header(c("Subject", "{col}"))) |>
  plan_apply()
pages[[1]]
#> ───────────────────────────
#> Subject    1 h   2 h   4 h 
#> ───────────────────────────
#> S-01      12.1   8.4   3.2 
#> S-02      10.5   7.9   BLQ 
#> S-03      14.0   9.6   4.1 
#>                            
#> Summary                    
#>     N       3     3     2  
#>     Mean  12.20  8.63  3.65
#> 
#> <rtftable> 7 rows x 4 columns
#>   Columns:    Subject | 1 h | 2 h | 4 h
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     auto / inherited
#> 
```

# Keep columns out of the printed table

`plan_hide()` names the columns that do their work without being
printed: a sort carrier, the key a page break reads, an identifier a
style condition tests. The column stays in the table until the pages are
made, so the other verbs can still read it; it is
`as_rtftables(drop_cols = )`. A column that a verb already needs — a
page key, a sort key — is hidden more simply by that verb's own
`keep = FALSE`.

## Usage

``` r
plan_hide(.plan, ...)
```

## Arguments

- .plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).

- ...:

  The names of the columns not printed, as strings or character vectors.
  A position among the printed columns (`row_title`, `collapse`, unnamed
  `widths`) leaves a hidden column out.

## Value

The plan, with one more layer.

## See also

[plan_verbs](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
for every verb.

Other plan verbs:
[`plan_after()`](https://ichirio.github.io/rtfreporter/reference/plan_after.md),
[`plan_append()`](https://ichirio.github.io/rtfreporter/reference/plan_append.md),
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
# A finished table with the visit's number beside its name
vs <- data.frame(
  VISITNUM = c(3, 1, 2),
  Visit    = c("Week 8", "Baseline", "Week 4"),
  Placebo  = c("128.4", "131.2", "129.9"),
  Active   = c("121.7", "130.8", "125.3"))

# Minimal: the number is not printed
table_plan(vs) |> plan_hide("VISITNUM") |> plan_apply()
#> [[1]]
#> ─────────────────────────
#> Visit     Placebo  Active
#> ─────────────────────────
#> Week 8     128.4   121.7 
#> Baseline   131.2   130.8 
#> Week 4     129.9   125.3 
#> 
#> <rtftable> 3 rows x 3 columns
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     auto / inherited
#> 
#> 

# The number still orders the visits, then stays out of sight
table_plan(vs) |>
  plan_sort("VISITNUM") |>
  plan_hide("VISITNUM") |>
  plan_titles("Systolic blood pressure (mmHg), mean") |>
  plan_apply()
#> [[1]]
#> Systolic blood pressure (mmHg), mean
#> ─────────────────────────
#> Visit     Placebo  Active
#> ─────────────────────────
#> Baseline   131.2   130.8 
#> Week 4     129.9   125.3 
#> Week 8     128.4   121.7 
#> 
#> <rtftable> 3 rows x 3 columns
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     auto / inherited
#>   Titles:     1 line(s)
#> 
#> 
```

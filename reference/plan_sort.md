# Put the rows in order

`plan_sort()` says the order of the rows: by the values of columns, or
by frequency. On a plan with an ARD half it sorts before the cells are
filled, while the statistics are still there to sort on (`-n`, the most
frequent first); on a plan of a finished table or a listing it sorts the
table. It is one verb whichever of the two does the work.

## Usage

``` r
plan_sort(.plan, ..., stat = NULL, keep = NULL)
```

## Arguments

- .plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).

- ...:

  The sort keys in priority order, each a string; a leading `-` sorts
  descending, ties go to the keys that follow. They are
  [`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)'s
  `sort`, where each form is described in full:

  a column

  :   a row key or the label column by its output name (`"label"`); on a
      finished table or a listing, any column (`"USUBJID"`).

  a statistic

  :   `"-n"`: that statistic totalled across the columns, the most
      frequent first.

  `"<stat>[<col>]"`

  :   the statistic in **one** column: `"-n[Active]"`, `"-n[Total]"`.

  `"<stat>@<key>"`

  :   the statistic of the group's own row, carried to every row of the
      group: `"-n@soc"` orders the SOCs by their own count and keeps
      each SOC's PTs with it.

  `"max(<stat>)"`, `"min(<stat>)"`

  :   the largest (smallest) value across the columns instead of the
      total: `"-max(n)"`.

  `"levels(<key>)"`

  :   the values
      [`plan_levels()`](https://ichirio.github.io/rtfreporter/reference/plan_levels.md)
      names for that key first, in its order.

  `".overall"`, `".depth"`

  :   the overall rows first; a level's own row before the rows nested
      under it.

  One logical is `widen_ard(sort = )` as it is: `TRUE` brings a key's
  separate blocks together, `FALSE` moves only what an order declares.

- stat:

  The statistic totalled into `.sort_stat` for a frequency order
  (`widen_ard(sort_stat = )`), one name, e.g. `stat = "n"`.

- keep:

  `TRUE` or `FALSE`; `FALSE` also hides the sort keys that are columns:
  a sort carrier needed and not wanted. Names that are not columns (a
  statistic, `".depth"`) are ignored rather than refused. Left out
  (`NULL`): `TRUE`, or what an earlier `plan_sort()` said – a later call
  that leaves it out keeps it.

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
[`plan_hide()`](https://ichirio.github.io/rtfreporter/reference/plan_hide.md),
[`plan_labels()`](https://ichirio.github.io/rtfreporter/reference/plan_labels.md),
[`plan_levels()`](https://ichirio.github.io/rtfreporter/reference/plan_levels.md),
[`plan_listing()`](https://ichirio.github.io/rtfreporter/reference/plan_listing.md),
[`plan_nest()`](https://ichirio.github.io/rtfreporter/reference/plan_nest.md),
[`plan_paginate_cols()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_cols.md),
[`plan_paginate_group()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_group.md),
[`plan_paginate_rows()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_rows.md),
[`plan_row_group()`](https://ichirio.github.io/rtfreporter/reference/plan_row_group.md),
[`plan_stat_groups()`](https://ichirio.github.io/rtfreporter/reference/plan_stat_groups.md),
[`plan_stub()`](https://ichirio.github.io/rtfreporter/reference/plan_stub.md),
[`plan_style()`](https://ichirio.github.io/rtfreporter/reference/plan_style.md),
[`plan_titles()`](https://ichirio.github.io/rtfreporter/reference/plan_titles.md),
[`plan_total()`](https://ichirio.github.io/rtfreporter/reference/plan_total.md)

## Examples

``` r
ae <- data.frame(
  ARM       = rep(c("Placebo", "Active"), each = 4),
  variable  = "AEDECOD",
  .label    = rep(c("Headache", "Nausea", "Dizziness", "Rash"), 2),
  .kind     = "categorical",
  stat_name = "n",
  stat      = c(3, 1, 0, 2, 6, 4, 1, 0))
p <- table_plan(ae, cols = "ARM", rows = c(group = "variable")) |>
  plan_cells(categorical = "{n}")

# Minimal: the most frequent term first, over both arms
p |> plan_sort("-n") |> plan_apply("table")
#>     group     label Placebo Active
#> 1 AEDECOD  Headache       3      6
#> 2 AEDECOD    Nausea       1      4
#> 3 AEDECOD      Rash       2      0
#> 4 AEDECOD Dizziness       0      1

# Ordered by the active arm's count, ties alphabetical
p |> plan_sort("-n[Active]", "label") |> plan_apply("table")
#>     group     label Placebo Active
#> 1 AEDECOD  Headache       3      6
#> 2 AEDECOD    Nausea       1      4
#> 3 AEDECOD Dizziness       0      1
#> 4 AEDECOD      Rash       2      0

# On a listing it sorts the records
recs <- data.frame(USUBJID = c("01-002", "01-001", "01-001"),
                   AETERM  = c("Fatigue", "Nausea", "Headache"))
pages <- table_plan(recs) |>
  plan_sort("USUBJID", "AETERM") |>
  plan_listing(listing_col("USUBJID", label = "Subject"),
               listing_col("AETERM", label = "Adverse event")) |>
  plan_apply()
pages[[1]]
#> ─────────────────────────
#> Subject     Adverse event
#> ─────────────────────────
#> 01-001      Headache     
#>                          
#> 01-001      Nausea       
#>                          
#> 01-002      Fatigue      
#>                          
#> 
#> <rtftable> 6 rows x 3 columns
#>   Columns:    Subject |  | Adverse event
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     relative 7:1:13
#> 
```

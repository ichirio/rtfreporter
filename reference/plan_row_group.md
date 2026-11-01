# Say how a run of rows sharing a value shows

`plan_row_group()` says what a **row group** is inside the body — where
one run of equal values ends and the next begins — and whether the
repeated value is printed once and then left blank. The group it finds
is what the blank rows between groups
([`plan_blanks()`](https://ichirio.github.io/rtfreporter/reference/plan_blanks.md))
and a page split that keeps groups whole
([`plan_paginate_rows()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_rows.md))
read. It does not make the row headings: folding the keys into one
indented column is
[`plan_stub()`](https://ichirio.github.io/rtfreporter/reference/plan_stub.md).

## Usage

``` r
plan_row_group(.plan, mode = NULL, collapse = NULL, group_col = NULL)
```

## Arguments

- .plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).

- mode:

  How a group boundary is found, `as_rtftables(group_by = )`:

  `"value"`

  :   each run of equal values.

  `"indent"`

  :   a row starts a group when its cell is not indented — the indent
      `plan_stub(indent = )` writes.

  `"filled"`

  :   a row starts a group when its cell is not empty.

  `"auto"`

  :   worked out from the column.

  The rows are grouped by the outermost row key (`table_plan(rows = )`);
  folded into a stub, by the stub's headings.

- collapse:

  The columns whose repeated value is printed once and then blank,
  `as_rtftables(collapse_repeats = )`: by name
  (`collapse = c("grp1", "grp2")`, the row keys' names) or by position,
  counted on the **printed** columns (a hidden key left out).

- group_col:

  On a plan of a table that is already built:
  `as_rtftables(group_col = )`, the one column whose runs are the
  groups. An ARD plan names it as its outermost row key
  (`table_plan(rows = )`) and refuses this; a page per value is
  [`plan_paginate_group()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_group.md).

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
[`plan_sort()`](https://ichirio.github.io/rtfreporter/reference/plan_sort.md),
[`plan_stat_groups()`](https://ichirio.github.io/rtfreporter/reference/plan_stat_groups.md),
[`plan_stub()`](https://ichirio.github.io/rtfreporter/reference/plan_stub.md),
[`plan_style()`](https://ichirio.github.io/rtfreporter/reference/plan_style.md),
[`plan_titles()`](https://ichirio.github.io/rtfreporter/reference/plan_titles.md),
[`plan_total()`](https://ichirio.github.io/rtfreporter/reference/plan_total.md)

## Examples

``` r
# A finished table of adverse events, one row a preferred term
ae <- data.frame(
  SOC     = rep(c("Gastrointestinal", "Nervous system"), each = 2),
  PT      = c("Nausea", "Vomiting", "Dizziness", "Headache"),
  Placebo = c("3", "1", "2", "5"),
  Active  = c("6", "2", "4", "7"))

# Minimal: each SOC printed once, on its first row
table_plan(ae) |> plan_row_group(collapse = "SOC") |> plan_apply()
#> [[1]]
#> ────────────────────────────────────────────
#> SOC                  PT      Placebo  Active
#> ────────────────────────────────────────────
#> Gastrointestinal   Nausea       3       6   
#>                   Vomiting      1       2   
#> Nervous system    Dizziness     2       4   
#>                   Headache      5       7   
#> 
#> <rtftable> 4 rows x 4 columns
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     auto / inherited
#> 
#> 

# The runs of SOC are the groups: a blank row between them (after
# row 2), and a page break that keeps each group whole
pages <- table_plan(ae) |>
  plan_row_group(mode = "value", collapse = "SOC", group_col = "SOC") |>
  plan_blanks(where = "between_groups") |>
  plan_apply()
pages[[1]]$blank_rows
#> [1] 2
pages <- table_plan(ae) |>
  plan_row_group(mode = "value", collapse = "SOC", group_col = "SOC") |>
  plan_paginate_rows(max_rows = 3, split = "group_safe") |>
  plan_apply()
length(pages)  # one SOC a page
#> [1] 2
```

# Drop rows by a condition over their statistics

`plan_filter()` keeps the rows whose statistics meet a condition —
"preferred terms in 5% or more of the subjects in any arm" — and drops
the rest, before the cells are made and the rows sorted. The conditions
are R over a record's statistics and keys, the language of
[`plan_cells()`](https://ichirio.github.io/rtfreporter/reference/plan_cells.md)'
guards; nothing is computed, `p` is the ARD's. The columns stay. In a
SOC / PT table a SOC goes with its last PT. A later `plan_filter()`
replaces the earlier one as a whole: the last call is the filter.

## Usage

``` r
plan_filter(.plan, ..., any = TRUE, parents = c("keep", "drop"), by = NULL)
```

## Arguments

- .plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).

- ...:

  The conditions, unnamed, all of them holding:
  `plan_filter(p >= 0.05)`, `plan_filter(n >= 3, ARM != "Placebo")`. A
  record whose statistics do not answer them does not count, and a row
  no column answers stays (the notes say how many).

- any:

  `TRUE` (default): a row stays when **any** column's record meets the
  conditions; `FALSE`: only when **every** column's does.

- parents:

  What a parent row (a SOC, its PT key empty) does:

  `"keep"`

  :   (default) it stays exactly when one of its children does, so no
      SOC is left without its PTs and none is kept for its own count.

  `"drop"`

  :   it is judged by its own counts, as any row.

  An overall row (`normalize_ard(overall = )`, "Any TEAE") always stays.

- by:

  A row key whose rows are judged together (with the keys outside it),
  by its name. `NULL` (default): each row on its own.

## Value

The plan, with one more layer.

## See also

[plan_verbs](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
for every verb;
[`plan_empty()`](https://ichirio.github.io/rtfreporter/reference/plan_empty.md)
for what a table left with no rows says.

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
# Invented adverse-event counts, 20 subjects an arm
ae <- data.frame(
  ARM       = rep(c("Placebo", "Active"), each = 4),
  variable  = "AEDECOD",
  .label    = rep(c("Headache", "Nausea", "Dizziness", "Rash"), 2),
  .kind     = "categorical",
  stat_name = "n",
  stat      = c(3, 1, 0, 2, 6, 4, 1, 0))
ae <- rbind(ae, transform(ae, stat_name = "p", stat = stat / 20))
p <- table_plan(ae, cols = "ARM", rows = c(group = "variable")) |>
  plan_cells(categorical = "{n} ({p:.0f%})", notes = FALSE)

# Minimal: the terms in 10% or more of any arm
p |> plan_filter(p >= 0.1) |> plan_apply("table")
#>     group    label Placebo Active
#> 1 AEDECOD Headache  3 (15) 6 (30)
#> 2 AEDECOD   Nausea   1 (5) 4 (20)
#> 3 AEDECOD     Rash  2 (10)  0 (0)

# The terms seen in both arms: every column must have a case
p |> plan_filter(n >= 1, any = FALSE) |> plan_apply("table")
#>     group    label Placebo Active
#> 1 AEDECOD Headache  3 (15) 6 (30)
#> 2 AEDECOD   Nausea   1 (5) 4 (20)
```

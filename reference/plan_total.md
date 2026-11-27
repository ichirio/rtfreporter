# Add a Total column (or row) from the overall statistics

`plan_total()` adds a **Total column**: the statistics an ARD carries
with no value of the column key — the same analysis run without its
`by`, such as cards' `ard_stack(.overall = TRUE)` — placed as one more
column, last or first. Nothing is computed: the cells are the ARD's own
overall rows, so an ARD whose column variable already has a `"Total"`
level needs no `plan_total()`. It sits with the row and column verbs,
before the display ones; the header's `{n}` over it is the study total
the ARD states.

## Usage

``` r
plan_total(.plan, label = NULL, position = NULL, direction = NULL)
```

## Arguments

- .plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).

- label:

  The heading of the Total column (or the label of the Total row), one
  text. Left out (`NULL`): `"Total"`, or what an earlier `plan_total()`
  said – a later call that leaves it out keeps it.

- position:

  Where it goes among the column key's values: `"last"` or `"first"`.
  Left out (`NULL`): `"last"`, or what an earlier `plan_total()` said.

- direction:

  What is added (left out, `NULL`: `"cols"`, or what an earlier
  `plan_total()` said):

  `"cols"`

  :   the Total **column**, from the records with no value of a column
      key. With two or more column keys the key folded is the first one,
      from the innermost out, that has overall rows (records with no
      value of it and a value of every other column key):
      `cols = c(TRTA, AESEV)` gives each arm a total column from its
      records with no `AESEV` (an AE at any grade), placed among that
      arm's grades; with no such records, the records with no `TRTA` and
      a grade fold `TRTA` instead, one Total column per grade.

  `"rows"`

  :   the Total **row**, a shift table's margin: read from the column
      key tabulated by the row keys, without the body's variable
      (`ard_tabulate(x, variables = base_gr, by = PARAM)` beside the
      body's `variables = worst_gr, by = c(PARAM, base_gr)`), labelled
      `label` and placed by `position`.

  `"both"`

  :   both, with the corner (a row key's own tabulation) as the Total
      row's Total column.

  Only the body's analyses are read: a test or a difference has no
  column key either, and is
  [`plan_compare()`](https://ichirio.github.io/rtfreporter/reference/plan_compare.md)'s.

## Value

The plan, with one more layer.

## See also

[plan_verbs](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
for every verb;
[`normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.md)
for the overall rows.

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
[`plan_sort()`](https://ichirio.github.io/rtfreporter/reference/plan_sort.md),
[`plan_stat_groups()`](https://ichirio.github.io/rtfreporter/reference/plan_stat_groups.md),
[`plan_stub()`](https://ichirio.github.io/rtfreporter/reference/plan_stub.md),
[`plan_style()`](https://ichirio.github.io/rtfreporter/reference/plan_style.md),
[`plan_titles()`](https://ichirio.github.io/rtfreporter/reference/plan_titles.md)

## Examples

``` r
# Invented counts: an ARM of NA is the overall row (both arms together)
st <- data.frame(
  ARM       = rep(c("Placebo", "Active", NA), each = 2),
  variable  = "SEX",
  .label    = rep(c("F", "M"), 3),
  .kind     = "categorical",
  stat_name = "n",
  stat      = c(12, 8, 9, 11, 21, 19))
p <- table_plan(st, cols = "ARM", rows = c(group = "variable")) |>
  plan_cells(categorical = "{n}")

# Minimal: a "Total" column after the arms
p |> plan_total() |> plan_apply("table")
#>   group label Placebo Active Total
#> 1   SEX     F      12      9    21
#> 2   SEX     M       8     11    19

# The study's wording, and the total before the arms
p |>
  plan_total(label = "All subjects", position = "first") |>
  plan_apply("table")
#>   group label All subjects Placebo Active
#> 1   SEX     F           21      12      9
#> 2   SEX     M           19       8     11

# Two column keys, arm by visit, and no overall rows for the visit:
# the arm is folded, a Total column for each visit
st2 <- data.frame(
  ARM       = rep(c("Placebo", "Active", NA), each = 2),
  AVISIT    = rep(c("Week 1", "Week 2"), 3),
  variable  = "AVAL",
  .label    = NA_character_,
  .kind     = "continuous",
  stat_name = "mean",
  stat      = c(1.5, 2.5, 1.8, 2.9, 1.6, 2.7))
table_plan(st2, cols = c("ARM", "AVISIT"), rows = c(group = "variable")) |>
  plan_cells(continuous = c(Mean = "{mean:.1f}")) |>
  plan_total() |>
  plan_apply("table")
#>   group label Placebo____Week 1 Placebo____Week 2 Active____Week 1
#> 1  AVAL  Mean               1.5               2.5              1.8
#>   Active____Week 2 Total____Week 1 Total____Week 2
#> 1              2.9             1.6             2.7
```

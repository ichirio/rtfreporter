# Say what a table with no events shows

`plan_empty()` gives an empty table a row that says so —
`"No subjects met the criteria."` — and, if asked, a footnote. A table
is empty when it has no body row (every row filtered out by
[`plan_filter()`](https://ichirio.github.io/rtfreporter/reference/plan_filter.md),
or none in the data) or when its overall row is 0 in every column; a
table that is not empty is as it was. So one plan serves a study whether
or not the events happened.

## Usage

``` r
plan_empty(
  .plan,
  text = "No subjects met the criteria.",
  when = NULL,
  position = NULL,
  footnote = NULL
)
```

## Arguments

- .plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).

- text:

  The row's text, one string, in the row label column (the first column
  when there is none); `NULL` for no row. Left out: the default
  sentence, or what an earlier `plan_empty()` said – a later call that
  leaves it out keeps it.

- when:

  When the table is empty (left out, `NULL`: `"auto"`, or what an
  earlier `plan_empty()` said):

  `"auto"`

  :   either of the two below.

  `"no_rows"`

  :   no body row, after
      [`plan_filter()`](https://ichirio.github.io/rtfreporter/reference/plan_filter.md).

  `"overall_zero"`

  :   its overall row (`normalize_ard(overall = )`) is 0 in every
      column.

- position:

  Where the row goes: `"last"` (after the rows: the overall row's 0
  stays), `"first"` or `"replace"` (the row alone). Left out (`NULL`):
  `"last"`, or what an earlier `plan_empty()` said.

- footnote:

  A line added to the table's footnotes when it is empty, after any
  [`plan_footnotes()`](https://ichirio.github.io/rtfreporter/reference/plan_footnotes.md);
  `NULL` (default) none.

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
# Invented counts: no subject had a serious adverse event
sae <- data.frame(ARM = c("Placebo", "Active"),
                  .label = "Any serious adverse event", .overall = TRUE,
                  .kind = "categorical", stat_name = "n", stat = 0)

# Minimal: the 0 row, then the sentence
table_plan(sae, cols = "ARM") |>
  plan_cells(categorical = "{n}") |>
  plan_empty() |>
  plan_apply("table")
#>                           label Placebo Active
#> 1     Any serious adverse event       0      0
#> 2 No subjects met the criteria.               

# A table of the events seen in 5 or more subjects of an arm, where
# none was: the study's sentence and a footnote saying why
ae <- data.frame(
  ARM       = rep(c("Placebo", "Active"), 3),
  .label    = rep(c("Any adverse event", "Headache", "Rash"), each = 2),
  .overall  = rep(c(TRUE, FALSE, FALSE), each = 2),
  .kind     = "categorical", stat_name = "n",
  stat      = c(3, 4, 2, 3, 1, 1))
pages <- table_plan(ae, cols = "ARM") |>
  plan_cells(categorical = "{n}", notes = FALSE) |>
  plan_filter(n >= 5) |>
  plan_stub(name = "Adverse event") |>
  plan_empty("No event reached 5 subjects in an arm.",
             footnote = "Events in 5 or more subjects of an arm are shown.") |>
  plan_apply()
pages[[1]]
#> ───────────────────────────────────────────────────────
#> Adverse event                           Placebo  Active
#> ───────────────────────────────────────────────────────
#> Any adverse event                          3       4   
#> No event reached 5 subjects in an arm.                 
#> Events in 5 or more subjects of an arm are shown.
#> 
#> <rtftable> 2 rows x 3 columns
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     auto / inherited
#>   Footnotes:  1 line(s)
#> 
```

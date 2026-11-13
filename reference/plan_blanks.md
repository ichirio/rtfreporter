# Say where the blank rows go

`plan_blanks()` places the blank separator rows: between the row groups,
after each record of a listing, at the top or bottom of every page, or
at positions given. They are added after the page split, so by default
they do not count toward a row budget
([`plan_paginate_rows()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_rows.md));
`counted = TRUE` makes the budget mean the rows the page prints. The
rows are kept as positions on each page (`pages[[1]]$blank_rows`) and
appear when the page is rendered.

## Usage

``` r
plan_blanks(.plan, where = NULL, first = NULL, last = NULL, counted = NULL)
```

## Arguments

- .plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).

- where:

  Where blank rows go in the body, `as_rtftables(blank_rows = )`:

  `"between_groups"`

  :   at every change of group, as
      [`plan_row_group()`](https://ichirio.github.io/rtfreporter/reference/plan_row_group.md)
      finds it.

  `"records"`

  :   after each record of a listing (`listing_spec(blank_row = TRUE)`).

  `"none"`

  :   none: a listing's records follow each other with no blank row
      between them (`blank_row = FALSE`).

  positions

  :   an integer vector, a blank row after each given row; `0` before
      the first, `-1` after the last. The positions count rows of the
      whole body, not of each page: a blank goes after that body row on
      the page it lands on, once, and one after a row that ends a page
      stays at that page's foot (not carried to the next). A fraction or
      a position past the body's last row stops at
      [`plan_apply()`](https://ichirio.github.io/rtfreporter/reference/plan_apply.md),
      naming the positions, as `as_rtftables(blank_rows = )` does.

  a rule

  :   a
      [`blank_rows_by_change()`](https://ichirio.github.io/rtfreporter/reference/blank_rows_by_change.md)
      or
      [`blank_rows_by_rule()`](https://ichirio.github.io/rtfreporter/reference/blank_rows_by_rule.md)
      object, or a [`list()`](https://rdrr.io/r/base/list.html)
      combining any of these.

- first, last:

  `TRUE` adds one blank row at the top (`first`) or the bottom (`last`)
  of every page,
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)'s
  `blank_row_first` / `blank_row_end`. On a listing `first = FALSE`
  takes away the one the multiline type puts at the top of each page.

- counted:

  `TRUE` counts the blank rows toward `max_rows`,
  `as_rtftables(count_blank_rows = )`; `FALSE` (default) does not, so a
  printed page can be taller than `max_rows`.

## Value

The plan, with one more layer.

## See also

[plan_verbs](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
for every verb.

Other plan verbs:
[`plan_after()`](https://ichirio.github.io/rtfreporter/reference/plan_after.md),
[`plan_append()`](https://ichirio.github.io/rtfreporter/reference/plan_append.md),
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
st <- data.frame(
  ARM       = rep(c("Placebo", "Active"), each = 3),
  variable  = rep(c("AGE", "SEX", "SEX"), 2),
  .label    = rep(c(NA, "F", "M"), 2),
  .kind     = rep(c("continuous", "categorical", "categorical"), 2),
  stat_name = rep(c("mean", "n", "n"), 2),
  stat      = c(61.2, 12, 8, 63.5, 9, 11))
p <- table_plan(st, cols = "ARM", rows = c(group = "variable")) |>
  plan_cells(continuous = c(Mean = "{mean:.1f}"), categorical = "{n}") |>
  plan_labels(AGE = "Age (years)", SEX = "Sex") |>
  plan_stub(name = "Characteristic")

# Minimal: a blank row between the two blocks (after printed row 2)
pages <- p |> plan_blanks(where = "between_groups") |> plan_apply()
pages[[1]]$blank_rows
#> [1] 2

# The same, with one more at the top and the bottom of the page
pages <- p |>
  plan_blanks(where = "between_groups", first = TRUE, last = TRUE) |>
  plan_apply()
pages[[1]]$blank_rows
#> [1] 0 2 5

# A listing puts one after each record; a compact one puts none
recs <- data.frame(USUBJID = c("01-001", "01-001", "01-002"),
                   AETERM  = c("Headache", "Nausea", "Fatigue"))
table_plan(recs) |>
  plan_listing(listing_col("USUBJID", label = "Subject"),
               listing_col("AETERM", label = "Adverse event")) |>
  plan_blanks(where = "none") |>
  plan_apply()
#> [[1]]
#> ─────────────────────────
#> Subject     Adverse event
#> ─────────────────────────
#> 01-001      Headache     
#> 01-001      Nausea       
#> 01-002      Fatigue      
#> 
#> <rtftable> 3 rows x 3 columns
#>   Columns:    Subject |  | Adverse event
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     relative 7:1:13
#> 
#> 
```

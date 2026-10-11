# The populations a column header's `{n}` can say

Which numbers the ARD states for `{n}`, before choosing one: a GUI that
asks "what does the header's `{n}` count?" shows each choice with its
values. A table whose pages are split by a group value (a lab parameter,
with
[`plan_paginate_group()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md))
has two:

## Usage

``` r
plan_n_candidates(plan)
```

## Arguments

- plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md)
  with `cols`.

## Value

A data frame: `scope` (`"all"`, `"page"` or `"table"`), `page` (the
page's group value; `NA` but on `"page"` rows), `column` (the spread
column's key, as `{n:<column>}` names it; `NA` for the population over
all the columns, what a spanning cell's `{n}` reads), `value`. Attribute
`differ`: `TRUE` when a page's numbers and the table's are not the same,
so which one `{n}` says is a choice (left unmade, the pages' are used,
with a warning); `page_col`: the column the pages are split on (`NULL`
when they are not).

## Details

- `scope = "page"`: each page's own – the rows carrying the page's key
  (the subjects with that test), what
  `plan_col_header(values = list(n = "page"))` prints;

- `scope = "table"`: the table's – the rows without the page key (the
  analysis set), the same on every page, what `n = "table"` prints.

A table not split so has one, `scope = "all"`. The numbers are the ones
[`plan_col_header()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
reads with `values = list(n = TRUE)` or a scope: only a number the ARD
states as a population size; a column it does not state has no row.

## See also

[`plan_col_header()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md),
[`plan_header_tokens()`](https://ichirio.github.io/rtfreporter/reference/plan_header_tokens.md)

## Examples

``` r
if (requireNamespace("cards", quietly = TRUE)) {
  ard <- normalize_ard(cards::ard_stack(cards::ADSL, .by = ARM,
    cards::ard_summary(variables = AGE)))
  plan_n_candidates(table_plan(ard, cols = "ARM"))
}
#>   scope page               column value
#> 1   all <NA>              Placebo    86
#> 2   all <NA> Xanomeline High Dose    84
#> 3   all <NA>  Xanomeline Low Dose    84
#> 4   all <NA>                 <NA>   254
```

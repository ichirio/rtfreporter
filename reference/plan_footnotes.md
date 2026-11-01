# Put a block of footnotes below the table

`plan_footnotes()` gives the footnote lines printed a blank line below
the table on each page, in the body of the page — not the section
footer. One block serves every page; `pages` gives each page its own.
The plan attaches them as the pages' `rtf_footnotes` attribute, which
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
prints as
[`rtf_footnotes()`](https://ichirio.github.io/rtfreporter/reference/rtf_footnotes.md)
does.

## Usage

``` r
plan_footnotes(.plan, ..., pages = NULL)
```

## Arguments

- .plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).

- ...:

  The rows of one block, used on every page: strings, one a line, or a
  character vector of them.

- pages:

  A list with one block per page, when the pages do not share one. Give
  `...` or `pages`, never both.

## Value

The plan, with one more layer.

## See also

[plan_verbs](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
for every verb;
[`plan_titles()`](https://ichirio.github.io/rtfreporter/reference/plan_titles.md)
for the block above;
[`plan_empty()`](https://ichirio.github.io/rtfreporter/reference/plan_empty.md)
for a footnote added only when a table is empty.

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
tbl <- data.frame(
  Parameter = c("Subjects", "Female, n (%)"),
  Placebo   = c("86", "53 (61.6)"),
  Active    = c("84", "50 (59.5)"))

# Minimal: one footnote line
pages <- table_plan(tbl) |>
  plan_footnotes("Percentages are of the subjects in each arm.") |>
  plan_apply()
attr(pages[[1]], "rtf_footnotes")
#> [1] "Percentages are of the subjects in each arm."

# Two pages cut by a row budget share one block of two lines: each
# page carries the same block
long <- data.frame(Subject = sprintf("01-%03d", 1:6),
                   Age = c(61, 58, 70, 66, 54, 49))
pages <- table_plan(long) |>
  plan_paginate_rows(max_rows = 3, split = "group_force") |>
  plan_footnotes(c("Age is at screening.", "Source: invented data.")) |>
  plan_apply()
lapply(pages, attr, "rtf_footnotes")
#> [[1]]
#> [1] "Age is at screening."   "Source: invented data."
#> 
#> [[2]]
#> [1] "Age is at screening."   "Source: invented data."
#> 
```

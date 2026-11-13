# Put a block of titles above the table

`plan_titles()` gives the title lines printed above the table on each
page, in the body of the page and the width of the content — not the
section header, which is the page's own furniture. One block serves
every page; `pages` gives each page its own. The plan attaches them as
the pages' `rtf_titles` attribute, which
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
prints as
[`rtf_titles()`](https://ichirio.github.io/rtfreporter/reference/rtf_titles.md)
does.

## Usage

``` r
plan_titles(.plan, ..., pages = NULL, border = NULL)
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
  `...` or `pages`, never both: a three-row title on a three-page table
  cannot be told apart from three one-row titles.

- border:

  `NULL` (default) for none, or an
  [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  for the title block as a whole on every page of this table, read as
  [`rtf_titles()`](https://ichirio.github.io/rtfreporter/reference/rtf_titles.md)`(border = )`
  is: `top` above the first row, `bottom` below the last row, `inside_h`
  between rows, `left` / `right` on every row. It is this table's own,
  so it wins over the document's
  [`rtf_titles()`](https://ichirio.github.io/rtfreporter/reference/rtf_titles.md)`(border = )`
  and
  [`rtf_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_style.md)`(titles = list(border = ))`.
  It is drawn with `rtf_style(title_format = "table")`; the text form
  ignores it, with a warning.

## Value

The plan, with one more layer.

## See also

[plan_verbs](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
for every verb;
[`plan_footnotes()`](https://ichirio.github.io/rtfreporter/reference/plan_footnotes.md)
for the block below;
[`plan_paginate_group()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_group.md)
(`heading_at = "title"`) for a title line named after each page's value.

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
[`plan_total()`](https://ichirio.github.io/rtfreporter/reference/plan_total.md)

## Examples

``` r
tbl <- data.frame(
  Parameter = c("Subjects", "Age, mean (SD)"),
  Placebo   = c("86", "75.2 (8.59)"),
  Active    = c("84", "75.7 (8.29)"))

# Minimal: two title lines on the one page
pages <- table_plan(tbl) |>
  plan_titles("Table 14.1.1", "Demographics") |>
  plan_apply()
attr(pages[[1]], "rtf_titles")
#> [1] "Table 14.1.1" "Demographics"

# A page per laboratory parameter, each with its own table number
lab <- data.frame(
  PARAM   = rep(c("ALT (U/L)", "AST (U/L)"), each = 2),
  Visit   = rep(c("Baseline", "Week 4"), 2),
  Placebo = c("22.1", "23.0", "25.4", "24.8"),
  Active  = c("21.8", "30.2", "24.9", "29.1"))
pages <- table_plan(lab) |>
  plan_paginate_group(col = "PARAM", keep = FALSE) |>
  plan_titles(pages = list(c("Table 14.3.1", "Alanine aminotransferase"),
                           c("Table 14.3.2", "Aspartate aminotransferase"))) |>
  plan_apply()
lapply(pages, attr, "rtf_titles")
#> $`ALT (U/L)`
#> [1] "Table 14.3.1"             "Alanine aminotransferase"
#> 
#> $`AST (U/L)`
#> [1] "Table 14.3.2"               "Aspartate aminotransferase"
#> 
```

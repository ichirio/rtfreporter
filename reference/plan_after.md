# Run your own functions on the finished pages

`plan_after()` adds steps of your own: functions run, in the order
written, on the finished pages, after the header, the decimal alignment
and the cell styles are on them. Each takes the pages and returns them,
so any function of rtfreporter that works on pages can be a step. It is
the way out for what no verb declares, and the plan cannot read it: see
the section below for the declarations to prefer.

## Usage

``` r
plan_after(.plan, ...)
```

## Arguments

- .plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).

- ...:

  The steps, each a function of the pages (a list of
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)s)
  that returns them, `function(pages) style_header(pages, bold = TRUE)`.
  Anything else is an error.

## Value

The plan, with one more layer.

## plan_after() is the way out, not the way in

`plan_after()` runs functions of your own on the finished pages. It is
for what the plan cannot **declare** — a step with no verb — and it is
the only verb whose content the plan cannot read: a workbook
(`tflspec::tfl_as_table_spec()`) cannot carry it, and the columns it
names are positions on the pages, which a reordered table does not keep.
The usual reasons to reach for it each have a declaration, which names
columns and goes into a workbook:

- `set_decimal_split(x, cols = 3:31)` is declared as
  `plan_columns(decimal = ".values")`

- `paginate_cols(x, ...)` is declared as
  `plan_paginate_cols(every = , at = , keep = )`

- `rtftable(col_rel_width = )` by position is declared as
  `plan_columns(widths = c(Analyte = 3, .values = 2))`

- [`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md)
  /
  [`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md)
  is declared as
  [`plan_col_header()`](https://ichirio.github.io/rtfreporter/reference/plan_col_header.md)

- `as_rtftables(align_count_pct = TRUE)` is declared as
  `plan_style(align_count_pct = TRUE)`

- [`fmt_numeric()`](https://ichirio.github.io/rtfreporter/reference/fmt_numeric.md)
  is declared as `plan_digits(<column> = 2)`,
  `plan_digits(.rows = c(Mean = 1))`

- `paginate()` is declared as
  [`plan_paginate_rows()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_rows.md)

- `style_header(x, ...)` is declared as
  `plan_cell_style(header = TRUE, ...)`

- `style_cols(x, ...)` is declared as `plan_cell_style(cols = , ...)`

- bold / colour / alignment of body cells, by condition is declared as
  `plan_cell_style(where = ~ ..., ...)`

- `style_zone(x, ...)` is declared as
  `plan_style(border_header = , border_body = , ...)`

## See also

[plan_verbs](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
for every verb.

Other plan verbs:
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
[`plan_titles()`](https://ichirio.github.io/rtfreporter/reference/plan_titles.md),
[`plan_total()`](https://ichirio.github.io/rtfreporter/reference/plan_total.md)

## Examples

``` r
tbl <- data.frame(
  Parameter = c("Subjects", "Age, mean (SD)"),
  Placebo   = c("86", "75.2 (8.59)"),
  Active    = c("84", "75.7 (8.29)"))

# Minimal: a step that returns the pages as they are
pages <- table_plan(tbl) |>
  plan_after(function(pages) pages) |>
  plan_apply()
length(pages)
#> [1] 1

# Two steps, run in order on the finished pages: one stamps a draft
# title on every page, the next adds a line under it
stamp <- function(pages) {
  for (i in seq_along(pages)) attr(pages[[i]], "rtf_titles") <- "DRAFT"
  pages
}
add_line <- function(pages) {
  for (i in seq_along(pages)) {
    attr(pages[[i]], "rtf_titles") <-
      c(attr(pages[[i]], "rtf_titles"), "Not for distribution")
  }
  pages
}
pages <- table_plan(tbl) |>
  plan_after(stamp, add_line) |>
  plan_apply()
attr(pages[[1]], "rtf_titles")
#> [1] "DRAFT"                "Not for distribution"
```

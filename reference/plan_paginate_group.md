# Start a new page for each value of a column

`plan_paginate_group()` is the **group** axis of pagination: every value
of one column becomes its own page, however long, and the page is named
after it — the line `rtf_tables(auto_section = TRUE)` cuts a section on,
so this verb decides what a section is. It is the only verb that makes a
page per value with no row budget;
[`plan_paginate_rows()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_rows.md)
is the row axis (with `page_by`, a page per value cut by rows too), and
[`plan_paginate_cols()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_cols.md)
the column axis.

## Usage

``` r
plan_paginate_group(
  .plan,
  col = NULL,
  keep = TRUE,
  heading = NULL,
  heading_at = NULL
)
```

## Arguments

- .plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).

- col:

  The column whose value starts a new page
  ([`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)'s
  `page_by`, or `split = "by_value"` with a
  [`plan_stub()`](https://ichirio.github.io/rtfreporter/reference/plan_stub.md)).
  Left out, it is the outermost row key. It is a **page** key, the same
  on every row of a page, so it is never a row group: blank rows between
  groups, `collapse`, `row_title`, the stub and the column positions
  start at the first printed row key.

- keep:

  `FALSE` also hides the page key. A column can be **needed and not
  wanted** — the key a page break reads — and the verb that needs it is
  the one place that knows, so it says so here instead of the name being
  written again in a
  [`plan_hide()`](https://ichirio.github.io/rtfreporter/reference/plan_hide.md).

- heading:

  The page's name as a template, in place of the bare value:
  `"Treatment: {value} (N={n})"`. `{value}` is the page's value as
  printed; `{n}` its population **as the ARD states it** — the page
  variable's own tabulation,
  `cards::ard_tabulate(adsl, variables = TRT01A)` bound into the ARD
  (the `.key_own` rows
  [`normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.md)
  keeps), never an analysis's `N`. What the ARD does not state is `NA`,
  with a warning. `{PROGRAM}`, `{DATETIME}` and the page tokens are left
  for the renderer.

- heading_at:

  Where the page's name prints:

  `"section"`

  :   in the section header, what `rtf_tables(auto_section = TRUE)`
      does.

  `"title"`

  :   as the last line of the titles above the table
      (`auto_title = TRUE`).

  `"none"`

  :   nowhere.

  `NULL`

  :   (default) not said:
      [`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)'s
      own default, nowhere.

  It is the default of
  [`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)'s
  `auto_section` / `auto_title` for these pages, so
  `rtf_tables(doc, plan)` needs neither; an `auto_section` /
  `auto_title` given to
  [`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
  wins.

## Value

The plan, with one more layer.

## See also

[plan_verbs](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
for every verb;
[`plan_paginate_rows()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_rows.md)
for a row budget.

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
# A finished table of two laboratory parameters
lab <- data.frame(
  PARAM   = rep(c("ALT (U/L)", "AST (U/L)"), each = 2),
  Visit   = rep(c("Baseline", "Week 4"), 2),
  Placebo = c("22.1", "23.0", "25.4", "24.8"),
  Active  = c("21.8", "30.2", "24.9", "29.1"))

# Minimal: a page per parameter, each named after it
pages <- table_plan(lab) |> plan_paginate_group(col = "PARAM") |> plan_apply()
names(pages)
#> [1] "ALT (U/L)" "AST (U/L)"

# The parameter leaves the body and names the page instead, as the
# last title line
pages <- table_plan(lab) |>
  plan_paginate_group(col = "PARAM", keep = FALSE,
                      heading = "Parameter: {value}", heading_at = "title") |>
  plan_apply()
names(pages)
#> [1] "Parameter: ALT (U/L)" "Parameter: AST (U/L)"
pages[[2]]
#> ─────────────────────────
#> Visit     Placebo  Active
#> ─────────────────────────
#> Baseline   25.4     24.9 
#> Week 4     24.8     29.1 
#> 
#> <rtftable> 2 rows x 3 columns
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     auto / inherited
#> 
```

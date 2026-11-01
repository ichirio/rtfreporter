# Fold the row keys into one row-heading column

`plan_stub()` makes the **stub**: the row keys (the block of each
analysis variable, the category under it) folded into one indented
column with a heading row per block, as
[`stub_cols()`](https://ichirio.github.io/rtfreporter/reference/stub_cols.md)
does. It belongs to the display half of the pipe, after the cells and
rows are declared and before the header and pages. By default the fold
happens inside
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md),
after grouping and pagination have read the keys.

## Usage

``` r
plan_stub(
  .plan,
  vars = NULL,
  name = NULL,
  indent = NULL,
  group_summary = NULL,
  before = FALSE
)
```

## Arguments

- .plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).

- vars:

  The row keys to fold, as `stub_cols(vars = )`. Left out, they are
  worked out from the plan: the names of `table_plan(rows = )` and its
  label column, without a grouping carrier. With one column (a table of
  one row heading), that column is the stub, renamed `name`, and nothing
  is folded.

- name:

  The name of the folded column, `stub_cols(label = )`, and so its
  header text unless
  [`plan_col_header()`](https://ichirio.github.io/rtfreporter/reference/plan_col_header.md)
  says otherwise: `"Characteristic"`. Not the same as
  `table_plan(label = )`, the column whose **values** are the row text.

- indent:

  The non-breaking spaces per nesting level, `stub_cols(indent = )`
  (default 4). `plan_row_group(mode = "indent")` reads the indent this
  writes.

- group_summary:

  Which leaf values mark a row as its group's summary, folding its
  statistics onto the group's heading row, as
  `stub_cols(group_summary = )`: `"empty"`, `"parent"`, `"all"` or
  `"none"`.

- before:

  `FALSE` (default) folds the stub inside
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md),
  after grouping and pagination have had their say. `TRUE` folds it
  first, with
  [`stub_cols()`](https://ichirio.github.io/rtfreporter/reference/stub_cols.md),
  which is what a condition of
  [`plan_cell_style()`](https://ichirio.github.io/rtfreporter/reference/plan_cell_style.md)
  needs: only then can it see the rows that will be printed. The two do
  **not** always give the same table: folded first, the row keys are
  gone before a page split or a row group could read them, so
  `plan_paginate_group(col = )` naming one of them no longer finds it.

## Value

The plan, with one more layer.

## See also

[plan_verbs](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
for every verb;
[`stub_cols()`](https://ichirio.github.io/rtfreporter/reference/stub_cols.md).

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
  plan_labels(AGE = "Age (years)", SEX = "Sex")

# Minimal: one stub column with a heading row per variable
plan_apply(p |> plan_stub(name = "Characteristic"), "pages")[[1]]
#> ───────────────────────────────
#> Characteristic  Placebo  Active
#> ───────────────────────────────
#> Age (years)                    
#>     Mean         61.2     63.5 
#> Sex                            
#>     F             12       9   
#>     M              8       11  
#> 
#> <rtftable> 5 rows x 3 columns
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     auto / inherited
#> 

# A narrower indent, folded first so that a condition can read the
# printed rows: the heading rows (no indent) in bold
pages <- p |>
  plan_stub(name = "Characteristic", indent = 2, before = TRUE) |>
  plan_cell_style(cols = "Characteristic",
                  bold = ~ !grepl("^ ", Characteristic)) |>
  plan_apply("pages")
pages[[1]]
#> ───────────────────────────────
#> Characteristic  Placebo  Active
#> ───────────────────────────────
#> Age (years)                    
#>   Mean           61.2     63.5 
#> Sex                            
#>   F               12       9   
#>   M                8       11  
#> 
#> <rtftable> 5 rows x 3 columns
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     auto / inherited
#>   Cell styles: set (5 rows)
#> 
```

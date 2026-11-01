# Cut the pages by a row budget

`plan_paginate_rows()` is the **row** axis of pagination: how many body
rows a page holds and what a page break may cut — a group kept whole, or
cut with a continuation label. With `page_by` the body is first
partitioned by the value of a column (a period, a cohort), each value a
page named after it, and the row budget applies within one partition. A
page per value with no row budget is
[`plan_paginate_group()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_group.md);
the column axis is
[`plan_paginate_cols()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_cols.md).

## Usage

``` r
plan_paginate_rows(
  .plan,
  max_rows = NULL,
  split = NULL,
  break_before = NULL,
  min_group_rows = NULL,
  cont_label = NULL,
  page_by = NULL,
  heading = NULL,
  heading_at = NULL
)
```

## Arguments

- .plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).

- max_rows:

  The most body rows a page holds, for the `"group_safe"` and
  `"group_force"` splits (which need it). Blank rows count only with
  `plan_blanks(counted = TRUE)`.

- split:

  How a page break is placed, `as_rtftables(split = )`:

  `"none"`

  :   (default) one page; `max_rows` is not read.

  `"group_safe"`

  :   whole groups to a page: a group that does not fit starts the next
      page; one longer than a page is cut, with a continuation label.

  `"group_force"`

  :   a cut every `max_rows` rows wherever it falls, a group straddling
      it continued on the next page; only `min_group_rows` pulls a cut
      back.

  `"rows"`

  :   a cut before each row of `break_before`.

  a function

  :   a split of your own, as
      [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
      takes it.

- break_before:

  With `split = "rows"`: the row positions to cut before,
  `as_rtftables(split_rows = )`.

- min_group_rows:

  The fewest rows of a group left on a page or carried to the next, to
  avoid a widow or an orphan.

- cont_label:

  The text added to a group's heading on the page it is continued on,
  `as_rtftables(cont_label = )`.

- page_by:

  The column(s) whose value partitions the body **first**, each value a
  page named after it, `as_rtftables(page_by = )`; the settings above
  then apply within one partition. The BY column is printed unless
  hidden
  ([`plan_hide()`](https://ichirio.github.io/rtfreporter/reference/plan_hide.md));
  it is a page key, never a row group.

- heading, heading_at:

  With `page_by`: the BY page's name as a template and where it prints,
  as
  [`plan_paginate_group()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_group.md)'s.
  With one `page_by` column `{value}` is its value; with several the
  heading carries a token for each column by its name,
  `"Analyte: {ANALYTE}"`, and no `{value}`.

## Value

The plan, with one more layer.

## See also

[plan_verbs](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
for every verb;
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
for the splits in full.

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
[`plan_row_group()`](https://ichirio.github.io/rtfreporter/reference/plan_row_group.md),
[`plan_sort()`](https://ichirio.github.io/rtfreporter/reference/plan_sort.md),
[`plan_stat_groups()`](https://ichirio.github.io/rtfreporter/reference/plan_stat_groups.md),
[`plan_stub()`](https://ichirio.github.io/rtfreporter/reference/plan_stub.md),
[`plan_style()`](https://ichirio.github.io/rtfreporter/reference/plan_style.md),
[`plan_titles()`](https://ichirio.github.io/rtfreporter/reference/plan_titles.md),
[`plan_total()`](https://ichirio.github.io/rtfreporter/reference/plan_total.md)

## Examples

``` r
# Invented records of twelve subjects
subj <- data.frame(Subject = sprintf("01-%03d", 1:12),
                   Age = c(61, 58, 70, 66, 54, 49, 73, 62, 68, 57, 60, 71))

# Minimal: five rows a page
pages <- table_plan(subj) |>
  plan_paginate_rows(max_rows = 5, split = "group_force") |>
  plan_apply()
vapply(pages, function(pg) nrow(pg$data), integer(1))
#> [1] 5 5 2

# A laboratory value over two periods: a page per period, each named
# after it, cut every two rows; the period itself is not printed
lab <- data.frame(
  Period  = rep(c("Period 1", "Period 2"), each = 4),
  Subject = rep(sprintf("01-%03d", 1:4), 2),
  ALT     = c(22, 25, 31, 19, 24, 27, 29, 20))
pages <- table_plan(lab) |>
  plan_paginate_rows(max_rows = 2, split = "group_force",
                     page_by = "Period", heading = "{value}",
                     heading_at = "title") |>
  plan_hide("Period") |>
  plan_apply()
length(pages)
#> [1] 4
pages[[2]]
#> ────────────
#> Subject  ALT
#> ────────────
#> 01-003   31 
#> 01-004   19 
#> 
#> <rtftable> 2 rows x 2 columns
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     auto / inherited
#> 
```

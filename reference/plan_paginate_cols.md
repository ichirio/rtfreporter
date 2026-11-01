# Cut a wide table into column blocks

`plan_paginate_cols()` is the **column** axis of pagination: a table too
wide for one page is cut into blocks of columns, each block a page that
repeats the row headings. Say where to cut one way — before named
columns (`at`), into given blocks or by a key in the column names
(`cut_by`), or every so many columns (`every`, counted when the table
exists, so a study with more visits needs nothing rewritten). With
`order` it also says how the three axes nest: this one, the row axis
([`plan_paginate_rows()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_rows.md))
and the group axis
([`plan_paginate_group()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_group.md)).

## Usage

``` r
plan_paginate_cols(
  .plan,
  at = NULL,
  cut_by = NULL,
  every = NULL,
  keep = NULL,
  col_header = NULL,
  fit = NULL,
  allow_span_break = NULL,
  order = NULL
)
```

## Arguments

- .plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).

- at:

  The columns to cut **before**, as names or positions,
  [`paginate_cols()`](https://ichirio.github.io/rtfreporter/reference/paginate_cols.md)'s
  `at`: `at = c(4, 6)` makes blocks `1:3`, `4:5` and `6:` the last.

- cut_by:

  Where to cut other than by position, one of:

  a list of column blocks

  :   `list(2:3, 4:5)`.

  a separator

  :   one string found in the column names: with `"____"`,
      `"Placebo____Day 1"` keys on `"Placebo"`, and each run of one key
      is a block.

  one key per column

  :   a vector as long as the columns; each run of one key is a block.

- every:

  Cut a block every this many columns, counting only the ones a block
  does not keep. A table no wider than one block is not cut.

- keep:

  The columns every block repeats, such as the row headings; by default
  the table's row titles (column 1 unless
  [`plan_columns()`](https://ichirio.github.io/rtfreporter/reference/plan_columns.md)
  says otherwise).

- col_header:

  The header each block carries, written once for the whole table in its
  coordinates and sliced to each block,
  [`paginate_cols()`](https://ichirio.github.io/rtfreporter/reference/paginate_cols.md)'s
  `col_header`. `NULL` slices the header the table has.

- fit:

  `TRUE` puts every block's widths on page 1's scale (the default: page
  1 fills the sheet and a ratio is the same width throughout); `FALSE`
  keeps each column's own width.

- allow_span_break:

  `TRUE` (default) allows a cut inside a spanning header cell.

- order:

  The order the pages come out in, as the **axes** of the split,
  outermost first (`paginate_cols(order = )`): any of `"group"`,
  `"rows"` and `"cols"`, or the shorthands `"across"` and `"down"`. Axes
  left out follow in that default order.

## Value

The plan, with one more layer.

## See also

[plan_verbs](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
for every verb;
[`paginate_cols()`](https://ichirio.github.io/rtfreporter/reference/paginate_cols.md)
for the split in full.

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
# Invented laboratory means at five visits: too wide for one page
wide <- data.frame(
  Parameter = c("ALT (U/L)", "AST (U/L)"),
  Day1  = c("22.1", "25.4"), Day8  = c("23.0", "24.8"),
  Day15 = c("30.2", "29.1"), Day22 = c("28.4", "27.0"),
  Day29 = c("26.3", "26.1"))

# Minimal: two visits a page, the parameter repeated on each
pages <- table_plan(wide) |>
  plan_paginate_cols(every = 2, keep = "Parameter") |>
  plan_apply()
length(pages)
#> [1] 3
lapply(pages, function(pg) names(pg$data))
#> [[1]]
#> [1] "Parameter" "Day1"      "Day8"     
#> 
#> [[2]]
#> [1] "Parameter" "Day15"     "Day22"    
#> 
#> [[3]]
#> [1] "Parameter" "Day29"    
#> 

# The first two weeks on one page and the rest on the next, each column
# keeping its width
pages <- table_plan(wide) |>
  plan_paginate_cols(at = "Day15", keep = "Parameter", fit = FALSE) |>
  plan_apply()
lapply(pages, function(pg) names(pg$data))
#> [[1]]
#> [1] "Parameter" "Day1"      "Day8"     
#> 
#> [[2]]
#> [1] "Parameter" "Day15"     "Day22"     "Day29"    
#> 
pages[[2]]
#> ──────────────────────────────
#> Parameter  Day15  Day22  Day29
#> ──────────────────────────────
#> ALT (U/L)  30.2   28.4   26.3 
#> AST (U/L)  29.1   27.0   26.1 
#> 
#> <rtftable> 2 rows x 4 columns
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     relative 16.5:20.7:20.7:20.7
#> 
```

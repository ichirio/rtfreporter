# Declare the column header, and the numbers it prints

`plan_col_header()` gives the column header, built with the same
[`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md)
as everywhere else (or a row at a time with `lines`), and says what its
`{tokens}` print: `{col}` the column, `{n}` the column's population,
read from the ARD or given. A row shorter than the table has its last
cell repeated over the value columns, whose names are not known until
the table exists, so one header serves a study with two arms or five. It
is a display verb: it runs on the pages, as
[`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md).

## Usage

``` r
plan_col_header(
  .plan,
  header = NULL,
  values = NULL,
  header_sep = NULL,
  col_header_align = NULL,
  lines = NULL,
  span = "each"
)
```

## Arguments

- .plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).

- header:

  The header, one of:

  an [`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md)

  :   the ordinary case. Its cells may carry `{col}` (the column; with
      several `cols` keys the **leaf**, and `{col1}`, `{col2}`, ... the
      levels in order) and `{n}`. A row already the right length, and a
      cell with no token, are untouched.

  a data frame of cells

  :   one row a cell, with the `col_header` sheet's columns (`line`,
      `cols`, `span`, `text`, borders ...), placed on each page's
      columns when it is made; how `tflspec::tfl_table_code()` writes a
      workbook's header.

  a function

  :   of the resolved `n` (and, with two arguments, the finished table),
      when the header has to be computed.

  The tokens a cell may use besides `{col}` and `{n}`: `{n:sum}`, the
  number totalled over the columns the cell covers (a spanner over one
  arm shows that arm's N, one over all of them the study total), and
  `{n:<column>}`, the number of one named column (`"A={n:Placebo}"`).
  `{n1}`, `{n2}`, ... name a depth of the keys.
  [`print()`](https://rdrr.io/r/base/print.html) of the plan lists every
  token it offers, with its values; see also
  [`plan_header_tokens()`](https://ichirio.github.io/rtfreporter/reference/plan_header_tokens.md).

- values:

  What the `{tokens}` take. A named list, **each name a token**; one
  value that is not a list is taken as `n`:

  `list(n = TRUE)`

  :   (or `TRUE`) the population of each column, read from the ARD at
      every depth of the `cols` keys. Only a number the ARD states as a
      population size is read, the first of: a total stated by group
      (`..ard_total_n..` keyed by the column,
      `cards::ard_strata(.by = TRT01A, .f = ~ cards::ard_total_n(.x))`);
      a cards sentinel's `N`
      (`ard_stack_hierarchical(over_variables = TRUE)`, the stack's
      denominator); the column variable's own tabulation (the `.key_own`
      rows
      [`normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.md)
      keeps from `ard_stack(.by = )`); an analysis `N` that is a
      denominator by construction or that two variables agree on. So a
      header N that differs from the percentages' denominator is a total
      by group. What cannot be read prints `NA`, with one warning that
      says why.

  `list(n = "page")`, `list(n = "table")`

  :   for pages split by a group value
      ([`plan_paginate_group()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_group.md)):
      each page's own population (the ARD rows carrying the page key),
      or the analysis set's (the rows without it).
      `list(n = "page", N = "table")` gives both, as `{n}` and `{N}`.
      `n = TRUE` reads the page's, then the table's, and warns when both
      are stated and differ.

  numbers

  :   given by hand: a vector named by column key at any depth,
      `n = c(Placebo = 20, Active = 20)`, or one unnamed number for
      every cell. After a workbook (`tflspec::tfl_table_plan()`) a later
      `plan_col_header(values = )` supplies the numbers and keeps the
      workbook's header.

  a function

  :   of the data, returning one of the above, for a number neither can
      find.

  more names

  :   further tokens: `list(n = TRUE, total = 254)` makes `{total}`
      print 254 in a spanner over each column's `{n}`.

  A **data frame** of per-page values is
  [`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md)'s
  own `values`: one row per page key, a column per `{token}`. The study
  total is never put in every column: it answers a one-column table and
  a cell over all the columns. Keep the header consistent with the body:
  the percentages are over the denominator cards used.

  A later `plan_col_header(values = )` replaces the earlier `values` as
  a whole: a token or column the later call leaves out is not kept.

- header_sep:

  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)'s
  `header_sep`: the separator a plain table's column names are split on
  into spanning header rows.

- col_header_align:

  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)'s
  `col_header_align`.

- lines:

  The header **a row at a time**, instead of `header`: a list, one
  element a header row (the top first), each a named character vector. A
  cell's name is the columns it sits on (a column name, `.values` for
  every value column, a position or range such as `3:5`, or
  `KEY = value`), its value the text, with the same tokens:
  `list(c(label = "", .values = "{col}"), c(label = "Characteristic", .values = "(N={n})"))`.

- span:

  With `lines`: how a cell over several columns is made, one value for
  every cell or a list as `lines` is (each element the spans of that
  row's cells, named as its cells are):

  `"each"`

  :   (default) a cell a column.

  `"one"`

  :   one cell over them all: a spanner.

  a key's name

  :   a cell per value of that key.

## Value

The plan, with one more layer.

## See also

[plan_verbs](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
for every verb;
[`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md),
[`plan_header_tokens()`](https://ichirio.github.io/rtfreporter/reference/plan_header_tokens.md),
[`plan_n_candidates()`](https://ichirio.github.io/rtfreporter/reference/plan_n_candidates.md).

Other plan verbs:
[`plan_after()`](https://ichirio.github.io/rtfreporter/reference/plan_after.md),
[`plan_append()`](https://ichirio.github.io/rtfreporter/reference/plan_append.md),
[`plan_blanks()`](https://ichirio.github.io/rtfreporter/reference/plan_blanks.md),
[`plan_cell_style()`](https://ichirio.github.io/rtfreporter/reference/plan_cell_style.md),
[`plan_cells()`](https://ichirio.github.io/rtfreporter/reference/plan_cells.md),
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
  ARM       = rep(c("Placebo", "Active"), each = 2),
  variable  = "SEX",
  .label    = rep(c("F", "M"), 2),
  .kind     = "categorical",
  stat_name = "n",
  stat      = c(12, 8, 9, 11))
p <- table_plan(st, cols = "ARM", rows = c(group = "variable")) |>
  plan_cells(categorical = "{n}") |>
  plan_stub(name = "Characteristic")

# Minimal: the stub's heading, then each arm's name (the row is short:
# "{col}" repeats over the value columns)
pages <- p |>
  plan_col_header(rtf_col_header(c("Characteristic", "{col}"))) |>
  plan_apply()
pages[[1]]
#> ───────────────────────────────
#> Characteristic  Placebo  Active
#> ───────────────────────────────
#> SEX                            
#>     F             12       9   
#>     M              8       11  
#> 
#> <rtftable> 3 rows x 3 columns
#>   Columns:    Characteristic | Placebo | Active
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     auto / inherited
#> 

# Each arm with its N (given here; an ARD from cards states them), and
# a spanner over both that totals them
pages <- p |>
  plan_col_header(
    values = list(n = c(Placebo = 20, Active = 20)),
    lines  = list(c(Characteristic = "", .values = "Treatment (N={n:sum})"),
                  c(Characteristic = "", .values = "{col}"),
                  c(Characteristic = "Characteristic", .values = "(N={n})")),
    span   = list("one", "each", "each")) |>
  plan_apply()
pages[[1]]
#> ────────────────────────────────
#>                 Treatment (N=40)
#>                 Placebo   Active
#> Characteristic   (N=20)   (N=20)
#> ────────────────────────────────
#> SEX                             
#>     F              12       9   
#>     M              8        11  
#> 
#> <rtftable> 3 rows x 3 columns
#>   Columns:    Characteristic | Placebo | Active
#>   Header rows: 3 (spanning)
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     auto / inherited
#> 
```

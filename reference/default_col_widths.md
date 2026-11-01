# The column widths of a table that gives none

The proportions
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
uses when a table gives no widths (no `col_rel_width`,
`column_widths_twips`, or a plan's `plan_columns(widths = )`): the
row-heading columns as wide as their longest line, within a share of the
table (at most 40%; less with many data columns, down to 20%; 5 points
more when the rows are indented), the data columns sharing the rest
equally, and a data column whose longest cell does not fit widened from
the others. A program that wants these as its starting point writes them
out: `col_rel_width = default_col_widths(data)` gives the same table (to
a few twips of rounding at the default `digits = 1`), and is then
changed by hand.

## Usage

``` r
default_col_widths(
  data,
  row_title = NULL,
  col_header = NULL,
  width_chars = 120,
  digits = 1L
)
```

## Arguments

- data:

  A data frame: the table's body, as it prints.

- row_title:

  The row-heading columns, by position or name; `NULL` (default) is
  column 1, as in
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md).

- col_header:

  The column header's labels (one row, or a list of rows: only the last
  is counted), or `NULL` for `names(data)`. A header wraps, so it counts
  by its longest word.

- width_chars:

  The table's width in characters of its font: 120 (the default) is a
  landscape letter or A4 page in a 9-point monospace font. It decides
  whether a long cell fits its share.

- digits:

  The proportions are rounded to this many decimals and scaled so the
  smallest data column is 1.

## Value

A named numeric vector, one per column of `data`: relative widths, as
`col_rel_width` (and `plan_columns(widths = )`) take them.

## See also

[`auto_col_widths()`](https://ichirio.github.io/rtfreporter/reference/auto_col_widths.md)
(widths in twips from the content alone).

## Examples

``` r
df <- data.frame(Characteristic = c("Age (years)", "  Mean (SD)", "  Median"),
                 Placebo = c("", "75.2 (8.59)", "76.0"),
                 Active  = c("", "74.4 (7.89)", "75.0"))
default_col_widths(df)
#> Characteristic        Placebo         Active 
#>            0.4            1.0            1.0 
```

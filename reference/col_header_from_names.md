# Build a spanning column header from delimited column names

Reconstructs a multi-row, spanning
[`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md)
by parsing the nesting encoded in delimited column names – e.g.
`"Drug A____N"`, `"Drug A____Mean"`, `"Drug B____N"`, `"Drug B____Mean"`
becomes a `Drug A` / `Drug B` spanning row over an `N` / `Mean` leaf
row. Horizontally adjacent columns that share a label **and** the same
ancestor path are merged into one spanning cell; columns with fewer
segments (e.g. an id column with no separator) are bottom-aligned so
their label sits on the leaf row with blank cells above.

## Usage

``` r
col_header_from_names(names, sep = .default_header_seps())
```

## Arguments

- names:

  A character vector of column names, or a `data.frame` (its
  [`names()`](https://rdrr.io/r/base/names.html) are used).

- sep:

  Character vector of separator(s) to split names on; the longest
  matching separator wins. Default recognises `"____"`
  (`ydisctools::pivot_stats_wider()`) and `"___tlang_delim___"` (tfrmt's
  column delimiter). A doubled separator yields an empty (blank) cell at
  that level.

## Value

An
[`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md).
When no name splits into more than one segment, a single flat label row
of `names`.

## Details

This is the same reconstruction
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
applies automatically to a plain data.frame; exposing it lets you build
the header explicitly and pass it to
[`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md)
or `rtftable(col_header = )`.

## See also

[`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md)
to apply it,
[`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md)
/
[`col_cell()`](https://ichirio.github.io/rtfreporter/reference/col_cell.md)
for the pieces.

## Examples

``` r
col_header_from_names(
  c("Item", "Drug A____N", "Drug A____Mean", "Drug B____N", "Drug B____Mean")
)
#> <rtf_col_header -- 2 rows>
#>   [1] cells: @1, Drug A@2-3, Drug B@4-5
#>   [2] labels: "Item", "N", "Mean", "N", "Mean"
```

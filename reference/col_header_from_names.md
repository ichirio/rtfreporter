# Build a spanning column header from delimited column names (deprecated)

**Deprecated** in 0.8.x (warns once a session, still works); removed in
0.9.0. It is what
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
already does with a plain data.frame's names (`header_sep =`) and what a
plan does with its column keys, so there is nothing to call.

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

Reconstructs a multi-row, spanning
[`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md)
by parsing the nesting encoded in delimited column names – e.g.
`"Drug A____N"`, `"Drug A____Mean"`, `"Drug B____N"`, `"Drug B____Mean"`
becomes a `Drug A` / `Drug B` spanning row over an `N` / `Mean` leaf
row. Horizontally adjacent columns that share a label **and** the same
ancestor path are merged into one spanning cell; columns with fewer
segments (e.g. an id column with no separator) are bottom-aligned so
their label sits on the leaf row with blank cells above.

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
# instead: as_rtftables() makes the same header from the names
df <- data.frame(Item = "x", "Drug A____N" = 1, "Drug A____Mean" = 2,
                 check.names = FALSE)
as_rtftables(df)[[1]]$col_header
#> [[1]]
#> [[1]][[1]]
#> [[1]][[1]]$from
#> [1] 1
#> 
#> [[1]][[1]]$to
#> [1] 1
#> 
#> [[1]][[1]]$label
#> [1] ""
#> 
#> 
#> [[1]][[2]]
#> [[1]][[2]]$from
#> [1] 2
#> 
#> [[1]][[2]]$to
#> [1] 3
#> 
#> [[1]][[2]]$label
#> [1] "Drug A"
#> 
#> 
#> 
#> [[2]]
#> [1] "Item" "N"    "Mean"
#> 
```

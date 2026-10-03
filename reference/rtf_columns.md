# Column names of a finished table's body

Returns the body column names of an
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
– the **final, printed** columns, in order. Use it to see exactly which
names (and positions)
[`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md),
[`style_cols()`](https://ichirio.github.io/rtfreporter/reference/style_header.md),
[`style_header()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)
etc. address, before writing a header. On a list of pages (an
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
result) it returns the first page's columns (pages share the same column
structure).

## Usage

``` r
rtf_columns(x, ...)

# S3 method for class 'rtftable'
rtf_columns(x, ...)

# S3 method for class 'list'
rtf_columns(x, ...)
```

## Arguments

- x:

  An
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md),
  or a list of them (pages from
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)).

- ...:

  Unused.

## Value

A character vector of column names.

## See also

[`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md).

## Examples

``` r
tbl <- rtftable(data.frame(Item = "x", A = 1, B = 2))
rtf_columns(tbl)
#> [1] "Item" "A"    "B"   
```

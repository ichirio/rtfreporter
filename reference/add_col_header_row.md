# Append (or prepend) a row to an `rtf_col_header` (deprecated)

**Deprecated** in 0.8.x (warns once a session, still works); removed in
0.9.0. Write the row in
[`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md)
itself – it takes the rows top first, cell rows and label rows alike:
`rtf_col_header(list(col_cell(c(2, 3), "Drug A")), c("Item", "N", "Mean"))`.

## Usage

``` r
add_col_header_row(hdr, row, .position = c("bottom", "top"))
```

## Arguments

- hdr:

  An
  [`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md),
  or any value accepted by `rtftable(col_header = ...)`.
  Non-`rtf_col_header` inputs are promoted automatically.

- row:

  One header row: a character vector or a list of cell specs.

- .position:

  `"bottom"` (default) appends below the existing rows; `"top"` prepends
  above.

## Value

A new `rtf_col_header`.

## Examples

``` r
# instead: the rows top first, in one rtf_col_header()
rtf_col_header(
  list(col_cell(1, ""), col_cell(c(2, 3), "Drug A"), col_cell(c(4, 5), "Drug B")),
  c("Item", "N", "Mean", "N", "Mean"))
#> <rtf_col_header -- 2 rows>
#>   [1] cells: @1, Drug A@2-3, Drug B@4-5
#>   [2] labels: "Item", "N", "Mean", "N", "Mean"
```

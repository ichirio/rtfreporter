# Build a multi-row column-header specification

Collects column-header rows, top-to-bottom, into a single object that
can be passed to `rtftable(col_header = ...)`. Each argument is one row;
a row may be either:

## Usage

``` r
rtf_col_header(...)
```

## Arguments

- ...:

  Header rows in render order (top first).

## Value

A list of class `"rtf_col_header"`.

## Details

- a character vector:

  one label per data column (legacy form), or

- a list of
  [`col_cell()`](https://ichirio.github.io/rtfreporter/reference/col_cell.md)
  objects:

  for a row with single and/or spanning cells.

## Examples

``` r
if (FALSE) { # \dontrun{
rtf_col_header(
  list(col_cell(1, ""), col_cell(c(2, 5), "Treatment")),
  list(col_cell(1, ""),
       col_cell(c(2, 3), "Drug A"),
       col_cell(c(4, 5), "Drug B")),
  c("Item", "N", "Mean", "N", "Mean")
)
} # }
```

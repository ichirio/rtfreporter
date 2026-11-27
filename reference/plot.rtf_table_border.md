# Visualise an `rtf_table_border`

Draws a schematic table with the four zones (header, body, first_row,
last_row) coloured to show which one provides which border.

## Usage

``` r
# S3 method for class 'rtf_table_border'
plot(x, ...)
```

## Arguments

- x:

  An `rtf_table_border` object: a table's resolved per-zone borders,
  e.g. the `border` of an
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md).

- ...:

  Unused.

## Value

Invisibly returns `x`.

## Examples

``` r
tbl <- rtftable(data.frame(A = 1:2, B = 3:4), border = "tfl")
plot(tbl$border)          # preview the clinical TFL border zones
```

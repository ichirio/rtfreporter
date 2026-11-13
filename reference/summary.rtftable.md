# Summarise an rtftable object

Prints the compact metadata block for an
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
– dimensions, leaf labels, row-title columns, border and column-width
mode, and any attached title / footnote counts – without the visual
table body.

## Usage

``` r
# S3 method for class 'rtftable'
summary(object, ...)
```

## Arguments

- object:

  An `rtftable` object.

- ...:

  Additional arguments (unused).

## Value

`object`, invisibly.

## See also

[`print.rtftable()`](https://ichirio.github.io/rtfreporter/reference/print.rtftable.md)
for the same block plus the table body,
[`format.rtftable()`](https://ichirio.github.io/rtfreporter/reference/format.rtftable.md)
to capture the rendered lines as a character vector.

## Examples

``` r
dm <- data.frame(
  Characteristic = c("Age (years)", "  Mean (SD)", "Sex", "  Female, n (%)"),
  `Drug A`       = c("", "54.2 (11.3)", "", "31 (51.7%)"),
  `Drug B`       = c("", "56.8 (10.1)", "", "27 (46.6%)"),
  check.names = FALSE, stringsAsFactors = FALSE
)
tbl <- rtftable(dm, border = "tfl", col_rel_width = c(2, 1, 1))

# The metadata block on its own -- dimensions, header rows, widths, borders
summary(tbl)
#> <rtftable> 4 rows x 3 columns
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     relative 2:1:1
#> 
```

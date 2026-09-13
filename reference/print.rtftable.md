# Print an rtftable object

Prints a visual preview of an
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
– the laid-out cells with column (and spanning) headers, per-column
alignment, and horizontal rules drawn where the table's border zones are
set – followed by a compact metadata block (dimensions, leaf labels,
row-title columns, border/width mode, and any attached title / footnote
counts). This mirrors printing a `gt_tbl`, `rtables` `VTableTree`, or
`gtsummary` object: the goal is to eyeball the table, not just its
metadata. For an internal multi-table rtftable (one carrying several
data.frames), the first table is rendered.

## Usage

``` r
# S3 method for class 'rtftable'
print(x, n = 10L, ...)
```

## Arguments

- x:

  An `rtftable` object.

- n:

  Number of body rows to render (default `10`).

- ...:

  Additional arguments (unused).

## Value

`x`, invisibly. Called for the side effect of printing.

## See also

[`format.rtftable()`](https://ichirio.github.io/rtfreporter/reference/format.rtftable.md)
to capture the rendered lines,
[`summary.rtftable()`](https://ichirio.github.io/rtfreporter/reference/summary.rtftable.md)
for the metadata block on its own.

## Examples

``` r
df <- data.frame(
  Characteristic = c("Age (years)", "  Mean (SD)", "Sex", "  Female"),
  `Drug A`       = c("", "75.1 (8.2)", "", "53 (54%)"),
  check.names    = FALSE
)
print(rtftable(df, col_header = c("Characteristic", "Drug A\nN = 98")))
#> ──────────────────────────
#>                   Drug A  
#> Characteristic    N = 98  
#> ──────────────────────────
#> Age (years)               
#>   Mean (SD)     75.1 (8.2)
#> Sex                       
#>   Female         53 (54%) 
#> 
#> <rtftable> 4 rows x 2 columns
#>   Columns:    Characteristic | Drug A N = 98
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     auto / inherited
#> 
```

# Clinical TFL preset (table style)

Returns a freshly constructed `rtf_table_style` matching the standard
clinical TFL preset: borders are applied to the **column-header block
only** (top on the topmost header row, bottom on the bottommost;
multi-col spanning auto-underlines). **The data section carries no
borders by default.** No vertical lines. No bold headers.

## Usage

``` r
rtf_table_style_tfl()
```

## Value

An `rtf_table_style` object.

## Details

To override one or more fields, pipe through
[`rtf_table_style_with()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style_with.md):


      heavy <- rtf_table_style_with(rtf_table_style_tfl(),
                header_bold = TRUE,
                border_last_row = rtf_border(bottom = TRUE))

## Examples

``` r
style <- rtf_table_style_tfl()
rtftable(data.frame(Parameter = "Age", Value = "75.1"), style = style)
#> ────────────────
#> Parameter  Value
#> ────────────────
#> Age        75.1 
#> 
#> <rtftable> 1 row x 2 columns
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     auto / inherited
#> 
```

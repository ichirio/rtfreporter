# Return a copy of an `rtf_table_style` with selected fields replaced

Non-mutating derivation: returns a new `rtf_table_style` whose listed
fields are overridden. Unknown field names raise an error.

## Usage

``` r
rtf_table_style_with(style, ...)
```

## Arguments

- style:

  An
  [`rtf_table_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style.md)
  object.

- ...:

  Named field overrides. Allowed names match the arguments of
  [`rtf_table_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style.md).

## Value

A new `rtf_table_style` object.

## Examples

``` r
base <- rtf_table_style_tfl()
rtf_table_style_with(base, align = "center", row_height_twips = 280L)
#> <rtf_table_style>
#>   borders:
#>     header    : <rtf_border>
#>     spanning  : none
#>     body      : none
#>     first_row : none
#>     last_row  : none
#>   header_align : (inherit align)
#>   header_bold  : FALSE
#>   align        : center
#>   bold         : FALSE
#>   cell_padding : L=(default) R=(default)
```

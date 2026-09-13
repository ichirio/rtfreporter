# Inspect the active rtfreporter defaults

Returns a named list of the currently **resolved** `rtfreporter.*`
default values – the option value where one is set (e.g. in
`Rprofile.site`), otherwise the factory baseline. Use it to record the
configuration a report was generated under (a useful audit trail for
validated/reproducible runs).

## Usage

``` r
rtfreporter_options()
```

## Value

A named list of resolved default values.

## See also

[`rtfreporter_reset_defaults()`](https://ichirio.github.io/rtfreporter/reference/rtfreporter_reset_defaults.md)
to restore the factory baseline.

## Examples

``` r
rtfreporter_options()
#> $rtfreporter.page.paper_size
#> [1] "letter"
#> 
#> $rtfreporter.page.orientation
#> [1] "landscape"
#> 
#> $rtfreporter.page.margin_top_in
#> [1] 0.75
#> 
#> $rtfreporter.page.margin_bottom_in
#> [1] 0.75
#> 
#> $rtfreporter.page.margin_left_in
#> [1] 0.75
#> 
#> $rtfreporter.page.margin_right_in
#> [1] 0.75
#> 
#> $rtfreporter.font
#> [1] "Courier"
#> 
#> $rtfreporter.font_size_half_points
#> [1] 18
#> 
#> $rtfreporter.row_height_twips
#> NULL
#> 
#> $rtfreporter.cell_padding_left_twips
#> NULL
#> 
#> $rtfreporter.cell_padding_right_twips
#> NULL
#> 
#> $rtfreporter.markup
#> [1] "script"
#> 
#> $rtfreporter.title_format
#> [1] "text"
#> 
#> $rtfreporter.footnote_format
#> [1] "table"
#> 
#> $rtfreporter.title_width
#> NULL
#> 
#> $rtfreporter.footnote_width
#> NULL
#> 
#> $rtfreporter.figure.default_dpi
#> [1] 96
#> 
```

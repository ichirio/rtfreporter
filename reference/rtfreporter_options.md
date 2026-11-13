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

## Details

Two of the options are about figures
([`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md),
[`rtf_figures()`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.md)):

- `rtfreporter.figure.default_dpi`:

  The DPI assumed for an image file that records none (factory `96`).

- `rtfreporter.png_device`:

  The PNG device a plot object is drawn with: `"auto"` (factory),
  `"ragg"` or `"png"`. The default follows
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html):
  ragg
  ([`ragg::agg_png()`](https://ragg.r-lib.org/reference/agg_png.html))
  when it is installed,
  [`grDevices::png()`](https://rdrr.io/r/grDevices/png.html) otherwise;
  `"png"` pins the base device. Files given as paths are embedded as
  they are, whatever it says.

One is the document's font:

- `rtfreporter.font`:

  The default of `rtf_document(font_table = )` and of every `font =`
  (factory `"Courier New"`). A family name that is not ASCII, such as a
  Japanese one, is written in the font table as Word writes it: in its
  East Asian code page with the matching `\fcharset`, and with its Latin
  name in `\falt` for common families (see *Font names that are not
  ASCII* in
  [`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md)).

One is about the table of contents of
[`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md):

- `rtfreporter.toc_style`:

  The size and boldness of its text, the default of
  `assemble_rtf(toc_style = )`: a list with the keys `title`, `heading`
  and `entry` (each `list(size = <points>, bold = TRUE/FALSE)`) and
  `gap` (`list(size = )`, the blank line below the title), merged over
  the built-in values key by key (factory `NULL`: title 14 pt bold,
  heading 11 pt bold, entry 10 pt, gap 9 pt). `entry` also takes
  `hanging` (`FALSE`, `TRUE` for the title's wrapped lines lined up
  after its number, or a positive whole number of twips): a hanging
  indent for an entry's wrapped lines.

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
#> [1] "Courier New"
#> 
#> $rtfreporter.font_size_half_points
#> [1] 18
#> 
#> $rtfreporter.font_lang
#> NULL
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
#> $rtfreporter.cell_padding_top_twips
#> NULL
#> 
#> $rtfreporter.cell_padding_bottom_twips
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
#> $rtfreporter.png_device
#> [1] "auto"
#> 
#> $rtfreporter.rounding
#> [1] "r"
#> 
#> $rtfreporter.ard_pipe
#> NULL
#> 
#> $rtfreporter.toc_style
#> NULL
#> 
```

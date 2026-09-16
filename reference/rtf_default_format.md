# Document-wide default formatting for an RTF document

Builds the `default_format` setting for
[`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md)
/
[`rtf_config()`](https://ichirio.github.io/rtfreporter/reference/rtf_config.md)
as a structured `rtf_default_format` object, so its options and
**defaults** are visible in the signature. Every value is a *default*: a
per-module setting on
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
/
[`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
/
[`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
/
[`rtf_table_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style.md)
always overrides it, and a site can change a default via the matching
`rtfreporter.*` option (an unset argument falls back to it; see
[`rtfreporter_options()`](https://ichirio.github.io/rtfreporter/reference/rtfreporter_options.md)).

## Usage

``` r
rtf_default_format(
  font_size_half_points = 18L,
  row_height_twips = NULL,
  cell_padding_left_twips = NULL,
  cell_padding_right_twips = NULL,
  markup = "script",
  title_format = "text",
  footnote_format = "table",
  title_width = NULL,
  footnote_width = NULL
)
```

## Arguments

- font_size_half_points:

  Body font size in half-points (`18` = 9 pt).

- row_height_twips:

  Default row height (twips) for every table-shaped element (content
  table, page header / footer, title / footnote). `NULL` (default) keeps
  the font-aware baseline.

- cell_padding_left_twips, cell_padding_right_twips:

  Default cell padding (twips, border-to-text). `NULL` (default) keeps
  the resource baseline (0).

- markup:

  Cell-text markup: `"script"` (`^{}`/`_{}` super/subscript, the
  default), `"relational"` (`>=`/`<=` to the symbols), `"all"`, or
  `"none"`. See
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md).

- title_format:

  How the page **title** renders: `"text"` (default, plain centred
  paragraphs) or `"table"` (a content-width single-column table).

- footnote_format:

  How the **footnote** renders: `"table"` (default, content-width table
  with the separator rule) or `"text"` (plain paragraphs).

- title_width, footnote_width:

  Width of the title / footnote block: `"content"` (the default – follow
  the table body), `"page"` (the writable width, margins excluded), a
  fraction in `(0, 1]` of the writable width, or twips. `NULL` keeps the
  default. Applies to the `"table"` form; the `"text"` form is plain
  paragraphs and already spans the page. `footnote_width = "page"` is
  the way to reach the full width **and** keep the separator rule, which
  `footnote_format = "text"` cannot draw.

## Value

An `rtf_default_format` object for `rtf_document(default_format =)`.

## See also

[`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md),
[`rtf_page()`](https://ichirio.github.io/rtfreporter/reference/rtf_page.md),
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md).

## Examples

``` r
# Defaults made explicit:
rtf_default_format()
#> <rtf_default_format>
#>   font 18 half-points; row height auto; padding L/R 0/0 twips
#>   markup [script]; title_format "text"; footnote_format "table"

# 10 pt, a fixed row height, and the >= / <= symbol conversion on:
rtf_default_format(font_size_half_points = 20L, row_height_twips = 240L,
                   markup = "all")
#> <rtf_default_format>
#>   font 20 half-points; row height 240; padding L/R 0/0 twips
#>   markup [script, relational]; title_format "text"; footnote_format "table"

doc <- rtf_document(default_format = rtf_default_format(font_size_half_points = 20L))
```

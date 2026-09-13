# Shared table style

Bundles table-wide formatting defaults – borders, alignment, bold, cell
padding, row height – into a single record that can be passed as the
`style =` argument of
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md).
Each
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
call snapshots the style fields it needs at construction time, so the
style object behaves like an immutable template.

## Usage

``` r
rtf_table_style(
  border_header = NULL,
  border_spanning = NULL,
  border_body = NULL,
  border_first_row = NULL,
  border_last_row = NULL,
  header_align = NULL,
  header_bold = FALSE,
  header_italic = FALSE,
  align = "left",
  bold = FALSE,
  italic = FALSE,
  underline = FALSE,
  cell_padding_left_twips = NULL,
  cell_padding_right_twips = NULL,
  row_height_twips = NULL
)
```

## Arguments

- border_header, border_spanning, border_body, border_first_row,
  border_last_row:

  [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  objects (or `NULL`) controlling each zone of the table.

- header_align, header_bold, header_italic:

  Defaults for column-header row formatting. `header_align = NULL` means
  "inherit `align`".

- align, bold, italic, underline:

  Defaults for data-row formatting.

- cell_padding_left_twips, cell_padding_right_twips:

  Cell padding (twips) used by both column-header and data cells.

- row_height_twips:

  Row height (twips); `NULL` = font-aware default.

## Value

A list of class `"rtf_table_style"`.

## Details

Use
[`rtf_table_style_with()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style_with.md)
(or simply construct a fresh style) to derive a variant.

## Examples

``` r
if (FALSE) { # \dontrun{
tfl_style <- rtf_table_style(
  border_header   = rtf_border(top = TRUE, bottom = TRUE),
  border_last_row = rtf_border(bottom = TRUE),
  header_bold     = FALSE,
  header_align    = NULL    # inherit data alignment
)

tbls <- lapply(dfs, function(df) rtftable(df, style = tfl_style))
} # }
```

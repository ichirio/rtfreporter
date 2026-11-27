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
  border_group = NULL,
  header_align = NULL,
  header_bold = FALSE,
  header_italic = FALSE,
  align = NULL,
  bold = FALSE,
  italic = FALSE,
  underline = FALSE,
  cell_padding_left_twips = NULL,
  cell_padding_right_twips = NULL,
  row_height_twips = NULL,
  cell_padding_top_twips = NULL,
  cell_padding_bottom_twips = NULL
)
```

## Arguments

- border_header, border_spanning, border_body, border_first_row,
  border_last_row:

  [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  objects (or `NULL`) controlling each zone of the table.

- border_group:

  An
  [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  (or `NULL`, the default: no rule) drawn under the **last row of each
  row group** – the groups `as_rtftables(group_by = )` finds. It is
  written by
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
  (and so by
  [`plan_style()`](https://ichirio.github.io/rtfreporter/reference/plan_style.md));
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  alone has no groups and ignores it. See
  [`plan_style()`](https://ichirio.github.io/rtfreporter/reference/plan_style.md)
  for the last-row and page-break rules.

- header_align, header_bold, header_italic:

  Defaults for column-header row formatting. `header_align = NULL` means
  "inherit `align`"; otherwise `"left"`, `"center"` or `"right"`.

- align, bold, italic, underline:

  Defaults for data-row formatting. `align = NULL` (the default) leaves
  each column's own default alone — row-title columns left, the others
  centred (see
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)'s
  `row_title`); `"left"` / `"center"` / `"right"` sets every column. Any
  other `align` or `header_align` is an error; a flag other than `TRUE`
  is stored as `FALSE`.

- cell_padding_left_twips, cell_padding_right_twips:

  Cell padding (twips) used by both column-header and data cells; `NULL`
  leaves the document's padding.

- row_height_twips:

  Row height (twips); `NULL` = font-aware default. This and the four
  padding arguments are each one non-negative integer or `NULL`;
  anything else is an error.

- cell_padding_top_twips, cell_padding_bottom_twips:

  Cell padding above / below the text (twips) of column-header and data
  cells; `NULL` leaves the document's padding.

## Value

A list of class `"rtf_table_style"`.

## Details

Use
[`rtf_table_style_with()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style_with.md)
(or simply construct a fresh style) to derive a variant.

## Examples

``` r
tfl_style <- rtf_table_style(
  border_header   = rtf_border(top = TRUE, bottom = TRUE),
  border_last_row = rtf_border(bottom = TRUE),
  header_bold     = FALSE,
  header_align    = NULL    # inherit data alignment
)

dfs <- list(data.frame(Parameter = "Age", Value = "75.1"),
            data.frame(Parameter = "Sex", Value = "53%"))
tbls <- lapply(dfs, function(df) rtftable(df, style = tfl_style))
```

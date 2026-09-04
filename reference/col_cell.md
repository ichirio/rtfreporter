# Column-header cell specification

Convenience constructor for a single cell in a column-header row passed
to
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
(via `col_header =`) or
[`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md).

## Usage

``` r
col_cell(
  pos,
  label = "",
  align = NULL,
  bold = FALSE,
  italic = FALSE,
  underline = FALSE,
  border = NULL
)
```

## Arguments

- pos:

  Cell position, one of:

  - a numeric of length 1 (single column) or length 2 (`c(start, end)`,
    inclusive) – `start <= end` required, values `>= 1`; or

  - a character of length 1 (single column name) or length 2
    (`c(start_name, end_name)`) referring to data columns by name. Name
    resolution (and the `start <= end` check for named ranges) happens
    when the header is attached to a table.

- label:

  Character scalar. Cell text; may be `""`.

- align:

  Optional `"left"`, `"center"`, or `"right"`. `NULL` (default) inherits
  the leftmost covered column's `header_align`.

- bold, italic, underline:

  Logical. Default `FALSE`.

- border:

  Optional
  [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  applied to **this header cell only**, overriding the zone border and
  the automatic group-underline. This is how you fine-tune individual
  rules in a multi-row header – for example removing the bottom line
  under one spanning cell with `border = rtf_border(bottom = "none")`,
  or adding a thicker rule under one column. `NULL` (default) inherits
  the normal zone borders.

## Value

A list of class `"rtf_col_cell"`.

## Details

Use `pos = 1` for a single-column cell and `pos = c(start, end)` for a
cell that spans several data columns. Positions are always relative to
the underlying data columns, not to the previous header row.

`pos` may instead be **column name(s)** (character), resolved against
the data columns when the header is attached to a table. This makes a
spanning cell robust to column reordering and errors out on an unknown
name – for example `col_cell(c("g1", "g3"), "Drug A")` spans from the
column named `g1` to the one named `g3`, and
`col_cell("total", "Total")` targets a single named column.

## Examples

``` r
col_cell(1, "Item")
#> <col_cell pos=1 label="Item">
col_cell(c(2, 5), "Treatment", align = "center", underline = TRUE)
#> <col_cell pos=2..5 label="Treatment", align=center [u]>

# Remove the group underline under just this spanning cell:
col_cell(c(2, 3), "Drug A",
         border = rtf_border(bottom = "none"))
#> <col_cell pos=2..3 label="Drug A">
```

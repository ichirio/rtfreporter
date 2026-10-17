# Single-edge border specification

Defines the line style, weight, and colour for one edge of a cell. Use
this as an argument to
[`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md).

## Usage

``` r
rtf_border_line(style = "single", width = 15L, color = NULL)

rtf_border_side(style = "single", width = 15L, color = NULL)
```

## Arguments

- style:

  Line style. One of `"single"` (default), `"double"`, `"thick"`,
  `"dash"`, `"dot"`, or `"none"`. Use `"none"` to build an *explicit
  no-line* side: unlike `NULL` (which simply leaves a side unset), a
  `"none"` side **overrides** any inherited border when it is merged on
  top of another spec. This is how a per-cell border can remove an
  automatically-drawn rule – e.g. suppressing the group underline under
  one spanning column-header cell.

- width:

  Line weight in twips. Default `15` (about 0.5 pt). Ignored when
  `style = "none"`.

- color:

  Line colour. `NULL` (default) = black. Or a 6-digit hex string such as
  `"#003366"`.

  `rtf_border_side()` is its old name: **deprecated** in 0.8.x (it warns
  once a session and still works) and removed in 0.9.0.

## Value

A list of class `"rtf_border_line"`.

## See also

[`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
to assemble sides into a border, attached to `rtftable(border = )` for
the whole table or to
[`style_zone()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)
for one kind of row.

## Examples

``` r
TRUE                                   # thin black rule (~0.5 pt)
#> [1] TRUE
rtf_border_line(style = "double", width = 30L, color = "#003366")
#> <rtf_border_line: double, 30 twips, color=#003366>
"none"   # explicit "no line" that removes an inherited rule
#> [1] "none"
```

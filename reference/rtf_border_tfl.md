# Clinical TFL-style table border preset

Returns an
[`rtf_table_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_border.md)
matching the standard clinical TFL style: **borders are applied to the
column-header block only**, with no borders in the data area by default.
Specifically:

## Usage

``` r
rtf_border_tfl(style = "single", width = 15L, color = NULL)
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

  Line weight in twips. Default `15` ≈ 0.5 pt. Ignored when
  `style = "none"`.

- color:

  Line colour. `NULL` (default) = black. Or a 6-digit hex string such as
  `"#003366"`.

## Value

An `rtf_table_border` object.

## Details

- `header$top` – top border on the topmost header row

- `header$bottom` – bottom border on the bottommost header row

- A multi-column spanning cell additionally receives a bottom border
  (group underline) **only where the column grouping changes below it**
  – that is, when the next header row subdivides the columns the span
  covers. A span repeated unchanged on the following row is not
  underlined. This is added automatically by the renderer.

- No vertical lines.

- **No borders on the data section** (`body` / `first_row` / `last_row`
  all `NULL`). Callers who want a bottom rule under the last data row
  can set it explicitly:
  `rtf_table_border(last_row = rtf_border(bottom = TRUE))`.

## Examples

``` r
rtf_border_tfl()                         # the standard clinical TFL rules
#> <rtf_table_border>
#>   header    : T=single/15 B=single/15 L=none R=none
#>   spanning  : none
#>   body      : none
#>   first_row : none
#>   last_row  : none
rtf_border_tfl(width = 30L)              # heavier rules
#> <rtf_table_border>
#>   header    : T=single/30 B=single/30 L=none R=none
#>   spanning  : none
#>   body      : none
#>   first_row : none
#>   last_row  : none
rtftable(data.frame(A = 1:2), border = rtf_border_tfl())
#> ─
#> A
#> ─
#> 1
#> 2
#> 
#> <rtftable> 2 rows x 1 columns
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     auto / inherited
#> 
```

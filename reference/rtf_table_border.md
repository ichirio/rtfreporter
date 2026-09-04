# Per-zone border specification for a table

Specifies borders for each logical zone of an
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md).
Each zone is either `NULL` (no border) or an
[`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
object. `first_row` and `last_row` are *overrides* merged on top of the
`body` spec.

## Usage

``` r
rtf_table_border(
  header = NULL,
  spanning = NULL,
  body = NULL,
  first_row = NULL,
  last_row = NULL,
  outer = NULL,
  inside_h = NULL,
  inside_v = NULL
)
```

## Arguments

- header:

  [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  for column-header rows. `NULL` = none.

- spanning:

  [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  for spanning-header rows. `NULL` = none.

- body:

  [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  for data rows. `NULL` = none.

- first_row:

  [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  override for the first data row.

- last_row:

  [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  override for the last data row.

- outer:

  [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  for the table's four outermost edges, or `NULL` (default). See the
  section above.

- inside_h, inside_v:

  Side values for the rules between rows and between cells, or `NULL`
  (default).

## Value

A list of class `"rtf_table_border"`.

## Deprecated since 0.5.0

A border is now written once with
[`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md),
and *where* it applies is decided by where you attach it:


      rtftable(border = rtf_border(...))                    # the whole table
      style_zone(header = rtf_border(...), body = ...)      # one kind of row
      col_cell(border = rtf_border(...))                    # one cell

`outer =` becomes the four edges of that
[`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md);
`inside_h =` and `inside_v =` keep their names. This function still
works and still returns the same value, but warns once per session.

## Writing borders before and after 0.5.0

Two things changed at 0.5.0. `rtf_table_border()` is deprecated, so a
border is aimed with `rtftable(border = )` or
[`style_zone()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)
instead; and an edge now always means the **outer** edge of the
selection, so the rules *inside* it have to be asked for by name. Each
case below, `s` being an a border side:


      ## rules above and below the column header  (unchanged in meaning)
      was:  rtftable(df, border = rtf_table_border(
                       header = rtf_border(top = s, bottom = s)))
      now:  rtftable(df, border = "none") |>
              style_zone(header = rtf_border(top = s, bottom = s))

      ## a rule under the last data row                      (unchanged)
      was:  rtf_table_border(last_row = rtf_border(bottom = s))
      now:  style_zone(last_row = rtf_border(bottom = s))

      ## a rule under EVERY data row              (bottom -> inside_h)
      was:  rtf_table_border(body = rtf_border(bottom = s))
      now:  style_zone(body = rtf_border(inside_h = s))

      ## a vertical rule at every column boundary  (left/right -> inside_v)
      was:  rtf_table_border(body = rtf_border(left = s, right = s))
      now:  style_zone(body = rtf_border(left = s, right = s, inside_v = s))

      ## a grid around every data cell
      was:  rtf_table_border(body = rtf_border(all = TRUE))
      now:  style_zone(body = rtf_border(top = s, bottom = s, left = s,
                                         right = s, inside_h = s, inside_v = s))

      ## an outer frame only, no rules inside      (was not expressible)
      now:  rtftable(df, border = rtf_border(top = s, bottom = s,
                                             left = s, right = s))

      ## frame plus a rule under every row -- the listing look
      was:  four style_header() / style_body() calls on the edge columns
      now:  rtftable(df, border = rtf_border(top = TRUE, bottom = TRUE,
                                             left = TRUE, right = TRUE,
                                             inside_h = TRUE))

At 0.6.0 the remaining constructors folded in here too, so a side is
written as a value rather than built:


      rtf_border_none()                 ->  rtf_border()
      rtf_border_top()                  ->  rtf_border(top = TRUE)
      rtf_border_bottom()               ->  rtf_border(bottom = TRUE)
      rtf_border_box()                  ->  rtf_border(all = TRUE)
      rtf_border_with(b, bottom = x)    ->  layer at the attach point
      rtf_border_tfl()                  ->  border = "tfl", rtf_table_style_tfl()

All of them still work and warn once per session; they are scheduled for
removal before the CRAN submission.

`border = "tfl"` and
[`rtf_border_tfl()`](https://ichirio.github.io/rtfreporter/reference/rtf_border_tfl.md)
are unaffected, as is any border on a single cell
([`col_cell()`](https://ichirio.github.io/rtfreporter/reference/col_cell.md),
`cell_styles`): a cell has no inside, so its four edges never meant
anything else.

The old and new readings are spelled identically, so rtfreporter warns
once per session when it meets a border that would have rendered
differently before. Naming `inside_h` / `inside_v` says which you mean
and silences it – use `"none"` for "no rule there".

## See also

[`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
for the pieces; pass the result as `rtftable(border = )`.

## Examples

``` r
s  <- TRUE
df <- data.frame(A = c("1", "2"), B = c("x", "y"))

# The clinical TFL look, spelled out one row kind at a time.
rtftable(df, border = "none") |>
  style_zone(header   = rtf_border(top = s, bottom = s),
             last_row = rtf_border(bottom = s))
#> ────
#> A  B
#> ────
#> 1  x
#> 2  y
#> ────
#> 
#> <rtftable> 2 rows x 2 columns
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     auto / inherited
#> 

# The listing look: outer frame plus a rule under every row, and no vertical
# rules between the columns.
rtftable(df, border = rtf_border(top = s, bottom = s, left = s, right = s,
                                 inside_h = s))
#> A  B
#> ────
#> 1  x
#> 2  y
#> ────
#> 
#> <rtftable> 2 rows x 2 columns
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     auto / inherited
#> 

# A full grid: add the rules between the cells.
rtftable(df, border = rtf_border(top = s, bottom = s, left = s, right = s,
                                 inside_h = s, inside_v = s))
#> A  B
#> ────
#> 1  x
#> 2  y
#> ────
#> 
#> <rtftable> 2 rows x 2 columns
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     auto / inherited
#> 

# Deprecated: the same thing written the old way.
if (FALSE) { # \dontrun{
rtf_table_border(header = rtf_border(top = s, bottom = s))
} # }
```

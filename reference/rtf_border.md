# Four-edge border specification for a cell or row

Specifies borders for up to four sides (top, bottom, left, right). Each
side is either `NULL` (no border) or a border side.

## Usage

``` r
rtf_border(
  all = NULL,
  top = NULL,
  bottom = NULL,
  left = NULL,
  right = NULL,
  inside_h = NULL,
  inside_v = NULL
)
```

## Arguments

- all:

  Shorthand for `top`, `bottom`, `left` and `right` at once. A side
  named explicitly wins over it.

- top, bottom, left, right:

  The selection's four outer edges. A side takes a value from either of
  two families – `TRUE`/`FALSE`, or a style name – or `NULL` to leave it
  unset. See *Writing a side* below.

- inside_h, inside_v:

  The rules drawn *between* the selection's rows (`inside_h`) and
  *between* its cells (`inside_v`). Same values as the edges; `NULL`
  (default) means no rule there.

## Value

A list of class `"rtf_border"`.

## Details

To add to a border already in place, attach a second one where the first
is: the places it attaches to merge side by side (see *Building versus
layering*).

## What a border applies to

An `rtf_border` describes a **selection**, and *where* it applies is
decided by where you attach it – the same way Word's border dialog acts
on whatever is selected:

|                                                   |                  |
|---------------------------------------------------|------------------|
| `rtftable(border = )`                             | the whole table  |
| `style_zone(header = , body = , ...)`             | that kind of row |
| `col_cell(border = )`, `cell_styles`              | one cell         |
| `rtf_header(border = )` / `rtf_footer(border = )` | that block       |

There is one rule, with no special cases:

- `top` / `bottom` / `left` / `right` are the selection's **outer**
  edges.

- `inside_h` / `inside_v` are the rules **inside** it, and an absent one
  means no rule there.

## Writing a side

Every side takes a value from one of two families. Stay inside one
family within a call and it reads evenly:


      logical     rtf_border(top = TRUE,     bottom = FALSE)
      style name  rtf_border(top = "single", bottom = "none")

The two agree: `TRUE` is
[`rtf_border_line()`](https://ichirio.github.io/rtfreporter/reference/rtf_border_line.md)
with its defaults (a single rule), and `FALSE` is `"none"` – an explicit
*no line*, which erases a rule the selection would otherwise inherit.
The style names are `"single"`, `"double"`, `"thick"`, `"dash"`, `"dot"`
and `"none"`.

Both are shorthands for the third spelling, an
[`rtf_border_line()`](https://ichirio.github.io/rtfreporter/reference/rtf_border_line.md),
which is what a side actually holds. Reach for it when a line needs a
weight or a colour of its own – and since each side carries its own, one
call is always enough:


      rtf_border(all = "double")                              # four edges alike
      rtf_border(top = rtf_border_line(color = "#C9372C"),    # ... or all four
                 bottom = rtf_border_line("double", 30L))     #     different

`FALSE` and `"none"` are always interchangeable.

So an edge never means something different depending on which other
arguments are present. On a row containing a spanning cell, `inside_v`
lands on **cell** boundaries rather than column boundaries: nothing is
drawn inside a merged cell. A single cell has no inside, so both are
ignored there.

## Building versus layering

A side is in one of three states, and all three matter:

- unset (`NULL`):

  nothing is said about it, so it inherits whatever the enclosing
  selection supplies.

- erased (`FALSE` / `"none"`):

  an explicit *no line*, which overrides an inherited rule.

- a rule (`TRUE` / a style name):

  an
  [`rtf_border_line()`](https://ichirio.github.io/rtfreporter/reference/rtf_border_line.md):
  its defaults for `TRUE`, that style for a name.

A call to `rtf_border()` says everything about the border it returns:
every side not named is unset. Adding to a border already in place is
the job of the place it is attached to –
[`style_zone()`](https://ichirio.github.io/rtfreporter/reference/style_header.md),
[`style_header()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)
and
[`style_body()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)
all merge **side by side**, so a second call adds to the first rather
than replacing it:


      tbl |>
        style_zone(header = rtf_border(top = rtf_border_line(color = "#C9372C"))) |>
        style_zone(header = rtf_border(bottom = TRUE))   # the top rule survives

A later layer can change a side or erase it with `FALSE`; a side left
`NULL` says nothing and leaves what was there alone.

One distinction worth keeping straight: `rtf_border(all = FALSE)` says
"explicitly no rule anywhere", which erases what a border would
otherwise inherit, while `rtf_border()` says nothing at all and
inherits.

## Where a border goes

A border is aimed with `rtftable(border = )` for the whole table or with
[`style_zone()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)
for one kind of row. An edge always means the **outer** edge of the
selection, so the rules *inside* it are asked for by name with
`inside_h` / `inside_v`. With `s` a border side:


      ## rules above and below the column header
      rtftable(df, border = "none") |>
        style_zone(header = rtf_border(top = s, bottom = s))

      ## a rule under the last data row
      style_zone(last_row = rtf_border(bottom = s))

      ## a rule under EVERY data row
      style_zone(body = rtf_border(inside_h = s))

      ## a vertical rule at every column boundary
      style_zone(body = rtf_border(left = s, right = s, inside_v = s))

      ## a grid around every data cell
      style_zone(body = rtf_border(top = s, bottom = s, left = s,
                                   right = s, inside_h = s, inside_v = s))

      ## an outer frame only, no rules inside
      rtftable(df, border = rtf_border(top = s, bottom = s,
                                       left = s, right = s))

      ## frame plus a rule under every row -- the listing look
      rtftable(df, border = rtf_border(top = TRUE, bottom = TRUE,
                                       left = TRUE, right = TRUE,
                                       inside_h = TRUE))

`border = "tfl"` sets the standard table look. A border on a single cell
([`col_cell()`](https://ichirio.github.io/rtfreporter/reference/col_cell.md),
`cell_styles`) has no inside, so its four edges are just that cell's
edges.

## Examples

``` r
rtf_border(top = TRUE, bottom = TRUE)  # top + bottom
#> <rtf_border>
#>   top     : single, 15 twips
#>   bottom  : single, 15 twips
#>   left    : none
#>   right   : none
rtf_border(bottom = rtf_border_line(color = "#003366"))          # blue underline
#> <rtf_border>
#>   top     : none
#>   bottom  : single, 15 twips, color=#003366
#>   left    : none
#>   right   : none

# A whole table: frame plus a rule under every row, no vertical rules.
s <- TRUE
rtf_border(top = s, bottom = s, left = s, right = s, inside_h = s)
#> <rtf_border>
#>   top     : single, 15 twips
#>   bottom  : single, 15 twips
#>   left    : single, 15 twips
#>   right   : single, 15 twips
#>   inside_h: single, 15 twips

# A rule between the cells but no outer edges.
rtf_border(inside_v = TRUE)
#> <rtf_border>
#>   top     : none
#>   bottom  : none
#>   left    : none
#>   right   : none
#>   inside_v: single, 15 twips

# Either family, but one at a time: these two agree.
identical(rtf_border(top = TRUE,     bottom = FALSE),
          rtf_border(top = "single", bottom = "none"))
#> [1] TRUE

# A weight or a colour of its own needs the side value.
rtf_border(top    = rtf_border_line("double", 30L, "#003366"),
           bottom = rtf_border_line(color = "#C9372C"))
#> <rtf_border>
#>   top     : double, 30 twips, color=#003366
#>   bottom  : single, 15 twips, color=#C9372C
#>   left    : none
#>   right   : none
```

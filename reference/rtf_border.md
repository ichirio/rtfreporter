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

rtf_border_none()

rtf_border_top(style = "single", width = 15L, color = NULL)

rtf_border_bottom(style = "single", width = 15L, color = NULL)

rtf_border_box(style = "single", width = 15L, color = NULL)
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

- style, width, color:

  Line style, weight in twips and colour, as in
  [`rtf_border_side()`](https://ichirio.github.io/rtfreporter/reference/rtf_border_side.md).

## Value

A list of class `"rtf_border"`.

## Details

To derive a new border from an existing one, pass it as `from`.

## Functions

- `rtf_border_none()`: Deprecated. Write `rtf_border()`.

- `rtf_border_top()`: Deprecated. Write `rtf_border(top = TRUE)`.

- `rtf_border_bottom()`: Deprecated. Write `rtf_border(bottom = TRUE)`.

- `rtf_border_box()`: Deprecated. Write `rtf_border(all = TRUE)`.

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

The two agree: `TRUE` is a rule in this call's `style` / `width` /
`color`, and `FALSE` is `"none"` – an explicit *no line*, which erases a
rule the selection would otherwise inherit. The style names are
`"single"`, `"double"`, `"thick"`, `"dash"`, `"dot"` and `"none"`.

Both are shorthands for the third spelling, an
[`rtf_border_side()`](https://ichirio.github.io/rtfreporter/reference/rtf_border_side.md),
which is what a side actually holds. Reach for it when a line needs a
weight or a colour of its own – and since each side carries its own, one
call is always enough:


      rtf_border(all = "double")                              # four edges alike
      rtf_border(top = rtf_border_side(color = "#C9372C"),    # ... or all four
                 bottom = rtf_border_side("double", 30L))     #     different

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

  drawn in this call's `style`, `width` and `color`.

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
        style_zone(header = rtf_border(top = rtf_border_side(color = "#C9372C"))) |>
        style_zone(header = rtf_border(bottom = TRUE))   # the top rule survives

A later layer can change a side or erase it with `FALSE`; a side left
`NULL` says nothing and leaves what was there alone.

One distinction worth keeping straight: `rtf_border(all = FALSE)` says
"explicitly no rule anywhere", which erases what a border would
otherwise inherit, while `rtf_border()` says nothing at all and
inherits.

## Writing borders before and after 0.5.0

Two things changed at 0.5.0.
[`rtf_table_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_border.md)
is deprecated, so a border is aimed with `rtftable(border = )` or
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

## Examples

``` r
rtf_border(top = TRUE, bottom = TRUE)  # top + bottom
#> <rtf_border>
#>   top     : single, 15 twips
#>   bottom  : single, 15 twips
#>   left    : none
#>   right   : none
rtf_border(bottom = rtf_border_side(color = "#003366"))          # blue underline
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
rtf_border(top    = rtf_border_side("double", 30L, "#003366"),
           bottom = rtf_border_side(color = "#C9372C"))
#> <rtf_border>
#>   top     : double, 30 twips, color=#003366
#>   bottom  : single, 15 twips, color=#C9372C
#>   left    : none
#>   right   : none
```

# Restyle an existing rtftable (post-hoc styling verbs)

These verbs edit an already-built
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
– typically one produced by
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
with `read_meta = TRUE`, where labels, alignment and spanning are
already correct and only a detail needs to change. Each verb returns a
modified copy (plain copy-on-modify S3) and is an S3 generic with two
methods: one for a single `rtftable`, one for a **list of pages** as
returned by
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
(the edit is applied to every page). They are designed to chain with the
native pipe:

## Usage

``` r
style_header(x, ...)

# S3 method for class 'rtftable'
style_header(
  x,
  row = NULL,
  cols = NULL,
  label = NULL,
  border = NULL,
  align = NULL,
  bold = NULL,
  italic = NULL,
  underline = NULL,
  ...
)

# S3 method for class 'list'
style_header(x, ...)

style_cols(x, ...)

# S3 method for class 'rtftable'
style_cols(
  x,
  cols = NULL,
  align = NULL,
  bold = NULL,
  italic = NULL,
  underline = NULL,
  indent_twips = NULL,
  color = NULL,
  border = NULL,
  header_align = NULL,
  header_bold = NULL,
  header_italic = NULL,
  ...
)

# S3 method for class 'list'
style_cols(x, ...)

style_body(x, ...)

# S3 method for class 'rtftable'
style_body(
  x,
  rows = NULL,
  cols = NULL,
  bold = NULL,
  italic = NULL,
  underline = NULL,
  indent_twips = NULL,
  color = NULL,
  align = NULL,
  border = NULL,
  ...
)

# S3 method for class 'list'
style_body(x, rows = NULL, ...)

style_zone(x, ...)

# S3 method for class 'rtftable'
style_zone(
  x,
  header = NULL,
  spanning = NULL,
  body = NULL,
  first_row = NULL,
  last_row = NULL,
  ...
)

# S3 method for class 'list'
style_zone(x, ...)

add_header_row(x, ...)

# S3 method for class 'rtftable'
add_header_row(x, row, .position = c("top", "bottom"), ...)

# S3 method for class 'list'
add_header_row(x, ...)
```

## Arguments

- x:

  An
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md),
  or a list of them (pages from
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)).

- ...:

  Passed between methods.

- row:

  Integer header-row index(es), 1 = top row. `NULL` (default) targets
  every header row.

- cols:

  Data-column selection: integer positions and/or column names of the
  table body (see the *What the columns are called* section of
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)).
  `NULL` (default) = all columns. Mix names and positions with a
  [`list()`](https://rdrr.io/r/base/list.html)
  ([`c()`](https://rdrr.io/r/base/c.html) would coerce the numbers to
  strings – same convention as `drop_cols` / `sort_by`). A spanning cell
  is targeted when its span **intersects** `cols`.

- label:

  Optional replacement label(s), recycled over the targeted cells in
  order.

- border:

  An
  [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  merged onto the targeted cells (side-by-side; existing sides survive
  where the new border leaves them `NULL`). In `style_body()` the merge
  lands on top of the row's zone border; in `style_cols()` it becomes
  the column's header-cell border.

- align:

  `"left"`, `"center"`, or `"right"`.

- bold, italic, underline:

  `TRUE`/`FALSE`. (On a character label row `underline` triggers the
  cell promotion described above.)

- indent_twips:

  Integer left indent for the column's body cells.

- color:

  Body text colour, `"#RRGGBB"`.

- header_align, header_bold, header_italic:

  Header-label styling for the selected columns (what the character
  label rows render with).

- rows:

  Body-row selection: `NULL` (all rows), integer positions, a logical
  vector over all body rows, a predicate `function(data)` returning one
  logical per row, or a one-sided formula evaluated inside the body data
  (columns visible as bare names, e.g. `rows = ~ label == "Mean"`). On a
  **page list**, integer and logical selections are rejected –
  page-local row numbers are ambiguous across pages – so use a
  predicate/formula (evaluated per page) or `NULL`.

- header, spanning, body, first_row, last_row:

  Per-zone
  [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  overrides, merged side-by-side onto the table's current
  [`rtf_table_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_border.md)
  (see the *Borders* article for what each zone covers).

- .position:

  `"top"` (default) prepends the new row above the existing header rows;
  `"bottom"` appends below.

## Value

An object of the same shape as `x` (rtftable, or list of pages),
modified.

## Details

    pages <- as_rtftables(gt_obj, read_meta = TRUE) |>
      style_header(row = 2, cols = 2:4,
                   border = rtf_border(top    = TRUE,
                                       bottom = "none")) |>
      style_cols(cols = "AGE", align = "center") |>
      style_body(rows = ~ label == "Mean", bold = TRUE)

The merge rule everywhere is the border rule the renderer already
documents, generalized: **last writer wins, per side / per field**. A
border passed to a verb is merged side-by-side onto whatever is already
there (`NULL` sides leave the existing side alone; use `"none"` for an
explicit "no line").

## Body cells

`style_body()` overrides, per cell, everything the data-row renderer
otherwise takes from `col_spec` – `bold` / `italic` / `underline` /
`indent_twips` / `color` / `align` – plus `border`, which merges on top
of the row's resolved zone border (`body` crossed with `first_row` /
`last_row`), per side. So a rule under a summary row is
`style_body(rows = ~ Item == "Total", border = rtf_border(bottom = TRUE))`,
and an `"none"` side erases a zone rule on the selected cells. Row
height and cell padding are deliberately *not* per-row properties – they
stay uniform per table / document (see `rtf_document(default_format)`
and the `rtfreporter.*` options).

## Header rows and the two row kinds

A column header holds two kinds of rows (see
[`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md)):
**cell rows** (lists of
[`col_cell()`](https://ichirio.github.io/rtfreporter/reference/col_cell.md)
spans – spanning rows, or any row built from cells) and one or more
**character label rows**. `style_header()` patches cell rows directly.
On a label row, `bold` / `italic` / `align` are stored in the per-column
`col_spec` header styling (note: that styling is shared by *all* label
rows of the table), and a `border` or `underline` request – which the
label-row renderer cannot express per cell – first **promotes** the row
to an equivalent list of single-column cells (labels and current header
styling are copied over, so the rendered output is unchanged; the
promoted row renders through the `spanning` border zone, which falls
back to the `header` zone when unset).

## Examples

``` r
df <- data.frame(Item = c("Age", "Sex"), A = 1:2, B = 3:4, C = 5:6)
hdr <- rtf_col_header(
  list(col_cell(1, ""), col_cell(c(2, 4), "Treatment")),
  list(col_cell(1, ""), col_cell(c(2, 4), "(N = 254)")),
  c("Item", "Placebo", "Drug A", "Drug B")
)
tbl <- rtftable(df, col_header = hdr, border = "tfl")

# Solid rule above -- and no rule below -- the "(N = 254)" cell only:
tbl <- tbl |>
  style_header(row = 2, cols = 2:4,
               border = rtf_border(top    = TRUE,
                                   bottom = "none"))

# Centre the body of columns B and C, bold the "Age" row:
tbl <- tbl |>
  style_cols(cols = c("B", "C"), align = "center") |>
  style_body(rows = ~ Item == "Age", bold = TRUE)

# Double rule under the whole table:
tbl <- tbl |>
  style_zone(last_row = rtf_border(bottom = "double"))

# Add a top header row after the fact:
tbl <- tbl |>
  add_header_row(list(col_cell(c(2, 4), "STUDY01")), .position = "top")
```

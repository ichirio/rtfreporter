# Create an RTF table object

Constructs a table object with full formatting control. The result can
be passed directly to
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
in a pipe chain.

## Usage

``` r
rtftable(
  data,
  col_header = NULL,
  col_header_align = NULL,
  spanning_header = NULL,
  col_spec = NULL,
  row_title = NULL,
  border = "tfl",
  blank_rows = NULL,
  read_attributes = TRUE,
  style = NULL,
  col_rel_width = NULL,
  column_widths_twips = NULL,
  table_width_twips = NULL,
  table_width_pct_of_writable = NULL,
  table_width_pct = NULL,
  table_align = "left",
  row_height_twips = NULL,
  font_size_half_points = NULL,
  font = NULL,
  row_height_exact = FALSE,
  header_row_height_twips = NULL,
  blank_row_height_twips = NULL,
  cell_padding_left_twips = NULL,
  cell_padding_right_twips = NULL,
  cell_valign = "bottom",
  cell_styles = NULL,
  blank_row_normalize = c("detect", "collapse"),
  markup = NULL
)
```

## Arguments

- data:

  A `data.frame`, or a `list` of `data.frame`s (multi-DF mode). Multi-DF
  mode renders each data.frame with its own column headers but shares
  column widths and border settings.

- col_header:

  The column header. One of:

  `NULL`

  :   (default) use the column names of `data`.

  a character vector

  :   one label per column – a single header row. By default labels are
      placed **by position**. If the vector is **named**, placement
      becomes name-aware and order-independent (a safeguard against
      column reordering): a named element (`g1 = "G1"`) labels the
      column named `g1` and errors on an unknown or duplicate name; an
      unnamed element is matched as a column name if one exists,
      otherwise placed at its position; two elements resolving to the
      same column is an error. A fully-unnamed vector keeps the exact
      legacy positional behaviour.

  a list of rows

  :   each row is either a character vector (a label row) or a spanning
      row (`list(list(from, to, label, underline), ...)`), rendered top
      to bottom.

  a list of per-DF specs

  :   in multi-DF mode, one of the above per `data.frame` (same length
      as `data`).

  Spanning cells built with
  [`col_cell()`](https://ichirio.github.io/rtfreporter/reference/col_cell.md)
  may also address columns by name (e.g.
  `col_cell(c("g1", "g3"), "Drug A")`).

- col_header_align:

  Column-header text alignment, applied across header rows. `NULL`
  (default) inherits each column's `align` value from `col_spec` (i.e.
  column headers follow the data alignment). `"center"` / `"left"` /
  `"right"` applies a single value to every column; a character vector
  of length `ncol` overrides per-column.

- spanning_header:

  A standalone spanning row placed **above** the `col_header` rows. Each
  element: `list(from, to, label, underline)`. Kept for backward
  compatibility – new code should put spanning rows directly inside
  `col_header`.

- col_spec:

  Per-column formatting, as a `list` of per-column specs. Each spec is a
  named list identifying its column with `col`, plus any of:

  `col`

  :   the integer column index (or column name) the spec targets.

  `align`

  :   data alignment `"left"` / `"center"` / `"right"` (overrides the
      `row_title`-derived default below).

  `bold`, `italic`, `underline`

  :   logical text decorations.

  `indent_twips`

  :   integer left indent of the cell text.

  `color`

  :   a `"#RRGGBB"` hex string – the column's **text colour** (added to
      the document colour table automatically).

  `header_align`, `header_bold`, `header_italic`

  :   the same, applied to this column's **header** cell.

  e.g.
  `list(list(col = 1, align = "left"), list(col = 2, bold = TRUE))`.

- row_title:

  Which columns are **row-heading** columns. `NULL` (default) means the
  first column only; otherwise an integer vector of column indices or a
  character vector of column names (e.g. `row_title = c(1, 2)`). This
  sets the per-column **default data alignment**: row-heading columns
  default to `"left"` and every other column defaults to `"center"`.
  Explicit `col_spec` alignment, an `rtf_table_style`, or alignment read
  from a gt/rtables source all still override this default; column
  headers follow the data alignment via the usual cascade.

- border:

  The table borders. One of:

  `"tfl"`

  :   (default) the clinical TFL preset: header top + bottom rules and a
      bottom rule on the last row.

  `"none"`

  :   no borders.

  an [`rtf_table_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_border.md) object

  :   full per-zone control.

  an [`rtf_table_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style.md) object

  :   its border zones are used.

- blank_rows:

  Where to insert blank separator rows. One of – or a `list` combining
  any of (positions are unioned):

  an integer vector

  :   positions: `0` = before the first row, `k` = after data row `k`,
      `-1` = after the last row.

  a [`blank_rows_by_change()`](https://ichirio.github.io/rtfreporter/reference/blank_rows_by_change.md) spec

  :   insert when a column value changes.

  a [`blank_rows_by_rule()`](https://ichirio.github.io/rtfreporter/reference/blank_rows_by_rule.md) spec

  :   insert before / after rows matching a regular expression.

- read_attributes:

  Logical. When `TRUE` (default), read recognised attributes off `data`
  for use as fallback defaults – currently
  `attr(data, "rtf_blank_rows")` is folded into `blank_rows` when the
  argument is `NULL`. Set `FALSE` to ignore attributes.

- style:

  Optional shared `rtf_table_style` (S3). Provides default values for
  borders, alignment, cell padding, etc.; explicit arguments to
  `rtftable()` always override. Snapshot semantics: each `rtftable()`
  call captures the style's current state at construction.

- col_rel_width:

  Numeric vector of relative column widths (e.g. `c(2, 1, 1)` makes the
  first column twice as wide as the others).

- column_widths_twips:

  Integer vector of absolute column widths in twips. Overrides
  `col_rel_width`.

- table_width_twips:

  Total table width in twips.

- table_width_pct_of_writable:

  Table width as a fraction 0-1 of the writable page width.

- table_width_pct:

  Table width as a percentage 0-100 of the writable page width
  (convenience alias for `table_width_pct_of_writable * 100`).

- table_align:

  Horizontal placement: `"left"` (default), `"center"`, or `"right"`.

- row_height_twips:

  Row height for data rows in twips. `NULL` (default) uses the
  document-wide default from `rtfreporter_defaults.R` (font-size-aware).
  A positive integer specifies an explicit value.

- font_size_half_points:

  Font size for this table, in half-points. `NULL` (default) inherits
  the document size. Setting it also recomputes the row height from that
  size unless `row_height_twips` is given.

- font:

  Font family for this table, e.g. `"Arial"`. `NULL` (default) uses the
  document font. A family not already in the document's `font_table` is
  added to it automatically, the way a colour is.

- row_height_exact:

  Logical. `TRUE` = exact (clipped); `FALSE` = minimum.

- header_row_height_twips:

  Row height for column-header rows.

- blank_row_height_twips:

  Row height for blank separator rows.

- cell_padding_left_twips:

  Left cell padding in twips (default 0 since v0.0.21; cell content sits
  flush against the cell border).

- cell_padding_right_twips:

  Right cell padding in twips (default 0).

- cell_valign:

  Vertical alignment: `"bottom"` (default), `"top"`, or `"center"`.

- cell_styles:

  `NULL` (default), or a list of length `nrow(data)`. Each element is
  either `NULL` (no per-cell override for that row) or a named list with
  optional vectors of length `ncol(data)`:

  `bold`

  :   logical – overrides `col_spec[[j]]$bold` when non-`NA`.

  `italic`

  :   logical – overrides `col_spec[[j]]$italic`.

  `underline`

  :   logical – overrides `col_spec[[j]]$underline`.

  `indent_twips`

  :   integer – overrides `col_spec[[j]]$indent_twips` (replaces, does
      not add to, the column default).

  `color`

  :   character `"#RRGGBB"` – per-cell **text colour**, overriding
      `col_spec[[j]]$color`. `NA` means "use the column colour".

  `align`

  :   character `"left"` / `"center"` / `"right"` – per-cell alignment,
      overriding `col_spec[[j]]$align`. `NA` means "use the column
      alignment".

  `border`

  :   a **list** of
      [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
      / `NULL` per column – per-cell border, merged side-by-side on top
      of the row's resolved zone border (`body` / `first_row` /
      `last_row`); an side of style `"none"` erases a zone rule.

  `NA` entries within a vector mean "no override; use the column
  default". This argument is populated automatically by
  [`as_rtftable()`](https://ichirio.github.io/rtfreporter/reference/as_rtftable.md)
  when reading from a `gt_tbl` or gtsummary table with `read = TRUE`.

- blank_row_normalize:

  How blank rows are normalised at render time. A character vector of
  zero or more of:

  `"detect"`

  :   a **data** row whose every cell is `NA` / `""` (empty or
      ASCII-whitespace only) is treated as a blank row and rendered as a
      single full-width cell, like an explicit blank separator row,
      instead of one empty cell per column.

  `"collapse"`

  :   a run of two or more consecutive blank rows (separator rows and/or
      `"detect"`-detected empty data rows) is reduced to a single blank
      row.

  Default `c("detect", "collapse")` (both on). Pass `"none"`, `NULL`, or
  `character(0)` to disable. Both behaviours act per rendered table, so
  for a paginated table they apply per page (i.e. after the split).

- markup:

  Which cell-text markup is applied at render time, as a character
  vector of zero or more of:

  `"script"`

  :   `^{...}` renders as superscript (`\\super`) and `_{...}` as
      subscript (`\\sub`).

  `"relational"`

  :   `">="` is converted to `U+2265` and `"<="` to `U+2264`.

  `"all"` enables both; `"none"` / `character(0)` enables neither.
  `NULL` (default) **inherits** the document default
  (`rtf_document(default_format = list(markup = ))` / the
  `rtfreporter.markup` option), which is `"script"` – so super/subscript
  (e.g. adapter footnote marks `^{N}`) work while the `>=` / `<=` symbol
  conversion is **opt-in**. Applies to all cell text: data cells, column
  / spanning headers, and title / footnote blocks.

## Value

An `rtftable` (S3) object suitable for use in
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md).

## Examples

``` r
# 1. Simplest: a data.frame straight to a table (column names become the
#    header).
df <- data.frame(Subject = c("001", "002"), Age = c(34L, 45L))
tbl <- rtftable(df)

# 2. A clinical-style table: a wide left-aligned row-label column, a spanning
#    "Treatment" header over the two arms, and the TFL border preset.
dm <- data.frame(
  Parameter = c("Age (years)", "  Mean (SD)", "  Median"),
  Placebo   = c("", "75.1 (8.2)", "76.0"),
  Active    = c("", "74.3 (7.9)", "75.0"),
  stringsAsFactors = FALSE
)
tbl <- rtftable(
  dm,
  col_header = list(
    list(list(from = 2, to = 3, label = "Treatment", underline = TRUE)),
    c("Parameter", "Placebo", "Active")
  ),
  col_spec      = list(list(col = 1, align = "left")),
  col_rel_width = c(2, 1, 1),
  border        = "tfl"
)

doc <- rtf_document() |>
  rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
  rtf_tables(tbl, titles = list("Table 14.1.1"))
f <- tempfile(fileext = ".rtf")
generate_rtfreport(doc, f, overwrite = TRUE)
unlink(f)
```

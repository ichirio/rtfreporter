# Assign content titles to pages

Replace the title of every content already added with the supplied
values. `titles` must have length equal to the number of contents (table
or figure pages) already added via
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
/
[`rtf_figures()`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.md),
**or length 1** – a single block is then applied to every content
(common title). Each content is a page of its own unless it was added
with `page_break = FALSE`;
[`rtf_layout()`](https://ichirio.github.io/rtfreporter/reference/rtf_layout.md)
shows them.

## Usage

``` r
rtf_titles(
  doc,
  titles,
  font_size_half_points = NULL,
  row_height_twips = NULL,
  markup = NULL,
  font = NULL,
  align = NULL,
  border = NULL
)
```

## Arguments

- doc:

  An rtf_document object.

- titles:

  A list of length = number of contents, or length 1 (common). A page's
  heading (`rtf_tables(auto_title = TRUE)`, a plan's
  `plan_paginate_group(heading_at = "title")`) is not replaced: it stays
  as the block's last row, under the titles given, or at the block's
  `{PAGE_HEADING}` row. That row – a title row whose whole text is
  `{PAGE_HEADING}`, e.g. `list(text = "{PAGE_HEADING}", align = "left")`
  – says where the heading goes among the other rows; it keeps the row's
  `align`, `bold` and so on, and the heading's text fills it. A page
  with no heading loses the row, so one block serves with and without
  headings. The marker is the row's whole text: with other text, or
  twice in a block, it is an error.

- font_size_half_points, row_height_twips, markup, align, font:

  Style for this block, over the document's
  [`rtf_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_style.md)
  (its section, then its shared level). Anything left `NULL` is
  inherited.

  `font`

  :   names a family (e.g. `"Arial"`); one not already in the document's
      `font_table` is added to it automatically, the way a colour is. A
      named vector gives one family per script slot (`latin`,
      `east_asian`, `complex`, optional `hansi`), e.g.
      `c(latin = "Times New Roman", east_asian = "MS Mincho")`; see
      [`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md).

  `font_size_half_points`, `row_height_twips`

  :   resolve **together**: a size given without a height recomputes the
      height from that size rather than inheriting one chosen for a
      different size, and an explicit height always wins.

  `align`

  :   sets the block's default row alignment; a per-row `align` still
      beats it.

- border:

  `NULL` (default) for no rule, or an
  [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  for the title block as a whole, read as
  [`rtf_footnotes()`](https://ichirio.github.io/rtfreporter/reference/rtf_footnotes.md)'s
  is: `top` above the first row, `bottom` below the last row, `inside_h`
  between rows, `left` / `right` on the outer edges of every row. A
  row's own `border` is merged side by side over it, so the sides the
  row sets win. It is drawn in the table form
  (`rtf_style(title_format = "table")`); the text form ignores it and
  warns.

## Value

Modified rtf_document.

## Details

Each element is a title **block**: either a character vector (one entry
per row, default styling) or a list of rows, where a row is a string or
a styled
`list(text=, align=, bold=, italic=, underline=, color=, border=, row_height_twips=)`.
An empty string (`""`), or `NA`, is a blank row. The block renders as
centred paragraphs, or, with `rtf_style(title_format = "table")`, as a
single-column table the same width as the content; title rows default to
centred + bold. A row's `row_height_twips` is that row's height, over
the block's `row_height_twips`; it applies to the table form
(`rtf_style(title_format = "table")`), as the block's does. So does a
row's `border`: the text form draws none, ignores one you set, and
warns.

## Examples

``` r
df1 <- data.frame(Parameter = "Age", Value = "75.1")
df2 <- data.frame(Parameter = "Sex", Value = "53%")
doc <- rtf_document() |>
  rtf_tables(list(df1, df2)) |>
  rtf_titles(list(
    c("Table 14.1.1", "", "Safety Population"),
    "Table 14.1.2"
  ))
```

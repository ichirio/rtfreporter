# Define sections for pages

Map page numbers to sections with headers/footers. Pages are
automatically numbered based on content order (starting at 1).

## Usage

``` r
rtf_section(doc, page = NULL, secinfo = NULL, header = NULL, footer = NULL)
```

## Arguments

- doc:

  An `rtf_document` object.

- page:

  Where this section starts. Pages are auto-numbered from the content
  order (starting at 1), so this is a **page number**. A single integer
  starts one section at that page; a vector starts several sections at
  once (its length must match the number of sections in `secinfo`).

- secinfo:

  *Superseded* by `header` / `footer` (documented, not warned): the same
  as a named list, `list(header = , footer = )`, and the way to give
  several sections at once – a `list` of such lists, one per entry of
  `page`. A name other than `header` / `footer` / `watermark` is an
  error (it used to be ignored, the band it meant left out), and so is a
  header / footer that is neither an
  [`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
  /
  [`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md),
  its rows nor its text.

- header, footer:

  The section's running header and footer: an
  [`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
  /
  [`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md),
  or `NULL`. `NULL` means **inherit**: a section with no header (footer)
  of its own keeps the previous section's, so only the first section's
  `NULL` leaves the pages without one. For a blank band after a section
  that has one, pass an empty one: `rtf_header("")`.

## Value

The `rtf_document` with the section definition(s) added.

## See also

[`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
/
[`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
to build the header / footer, and
[`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md)
for the document and its `page` geometry.

## Examples

``` r
df  <- data.frame(Parameter = "Age, Mean (SD)", Value = "75.1 (8.2)")
h1  <- rtf_header(c(l = "Table 14.1.1", r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}"))
h2  <- rtf_header(c(l = "Table 14.2.1", r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}"))
ftr <- rtf_footer(c(l = "Confidential"))

# One header / footer applied to the whole document:
doc <- rtf_document() |>
  rtf_tables(list(df, df)) |>
  rtf_section(header = h1, footer = ftr)

# A second section, with a different header, starting at page 2:
doc <- rtf_document() |>
  rtf_tables(list(df, df)) |>
  rtf_section(page = 1, header = h1, footer = ftr) |>
  rtf_section(page = 2, header = h2)        # the footer is inherited
```

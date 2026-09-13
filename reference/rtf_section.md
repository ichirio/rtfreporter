# Define sections for pages

Map page numbers to sections with headers/footers. Pages are
automatically numbered based on content order (starting at 1).

## Usage

``` r
rtf_section(doc, page = NULL, secinfo)
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

  The section definition(s). A single section is a named list:

  `header`

  :   an
      [`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
      object, or `NULL` for no header

  `footer`

  :   an
      [`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
      object, or `NULL` for no footer

  For several sections, pass a `list` of such section lists – one per
  entry of `page`.

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
  rtf_section(page = 1, secinfo = list(header = h1, footer = ftr))

# A second section, with a different header, starting at page 2:
doc <- rtf_document() |>
  rtf_tables(list(df, df)) |>
  rtf_section(page = 1, secinfo = list(header = h1, footer = ftr)) |>
  rtf_section(page = 2, secinfo = list(header = h2, footer = ftr))
```

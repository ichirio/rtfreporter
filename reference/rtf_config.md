# Configure document-level settings

Update the **document-level** settings (font, colour, page, default
format) of an existing `rtf_document`, leaving its content, titles and
sections untouched. Only the arguments you pass are changed (`NULL` = no
change).

## Usage

``` r
rtf_config(
  doc,
  font_table = NULL,
  color_table = NULL,
  page = NULL,
  default_format = NULL
)
```

## Arguments

- doc:

  An rtf_document object.

- font_table:

  Optional font table; replaces the current one.

- color_table:

  Optional colour table; replaces the current one.

- page:

  New page geometry, **merged per key** onto the current page: an
  [`rtf_page()`](https://ichirio.github.io/rtfreporter/reference/rtf_page.md)
  object, or a named list with the same keys (see
  [`rtf_page()`](https://ichirio.github.io/rtfreporter/reference/rtf_page.md)).

- default_format:

  New document-wide default formatting, **merged per key** onto the
  current defaults: an
  [`rtf_default_format()`](https://ichirio.github.io/rtfreporter/reference/rtf_default_format.md)
  object, or a named list with the same keys (see
  [`rtf_default_format()`](https://ichirio.github.io/rtfreporter/reference/rtf_default_format.md)).
  Each is a document-wide *default* that any per-module setting
  overrides.

## Value

Modified rtf_document object (new copy, original unchanged).

## Details

The point of `rtf_config()` is *deriving variants from an
already-composed document*. Build the report once – add tables/figures
and sections – then produce alternative outputs (a different paper size,
font, or default text size) by changing only the relevant setting and
rendering each copy. Because an `rtf_document` is immutable, each call
returns a fresh copy and the original is left intact, so the variants
are independent.

`page` and `default_format` are **merged per key**: passing
`page = list(width_in = 8.27, height_in = 11.69)` changes only those two
keys and keeps the document's existing orientation and margins.
`font_table` and `color_table` are replaced as a whole.

## See also

[`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md)
(the constructor),
[`rtf_page()`](https://ichirio.github.io/rtfreporter/reference/rtf_page.md)
/
[`rtf_default_format()`](https://ichirio.github.io/rtfreporter/reference/rtf_default_format.md)
for the settings.

## Examples

``` r
# Compose once, then render two paper-size variants from the SAME content.
base <- rtf_document() |>
  rtf_tables(data.frame(Parameter = "Age", Value = "75.1")) |>
  rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL))

letter <- base                                   # landscape Letter (default)
a4     <- base |> rtf_config(page = rtf_page(paper_size = "A4"))

# Only the page changed; the content/section are shared.
identical(letter$contents, a4$contents)
#> [1] TRUE
```

# Visualise an `rtf_document`

Draws a grid of page thumbnails. Each thumbnail shows the title /
content / footnote regions, with header and footer bands sketched in
grey.

## Usage

``` r
# S3 method for class 'rtf_document'
plot(x, max_pages = 12L, ...)
```

## Arguments

- x:

  An
  [`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md)
  object.

- max_pages:

  Maximum number of pages to draw (default `12`). Larger documents are
  truncated with a note.

- ...:

  Unused.

## Value

Invisibly returns `x`.

## Examples

``` r
doc <- rtf_document() |>
  rtf_tables(data.frame(Parameter = "Age", Value = "75.1"))
if (FALSE) { # \dontrun{
plot(doc)        # preview the document's pages on screen
} # }
```

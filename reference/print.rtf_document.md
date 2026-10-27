# Print an rtf_document object

Print an rtf_document object

## Usage

``` r
# S3 method for class 'rtf_document'
print(x, ...)
```

## Arguments

- x:

  An rtf_document object.

- ...:

  Additional arguments (unused).

## Value

`x`, invisibly. Called for the side effect of printing the document's
layout, as
[`rtf_layout()`](https://ichirio.github.io/rtfreporter/reference/rtf_layout.md)
makes it: the number of contents (blocks) and printed pages, the page
and font size, and one line per content – its page, the section a page
begins, its name, the rows of a table or the size of a figure, and the
rows of its title and footnote.

## Examples

``` r
print(rtf_document())
#> rtf_document: no contents yet (letter landscape, 9 pt)
rtf_document() |>
  rtf_tables(list(Demog = data.frame(A = 1:3)), titles = "Table 1")
#> rtf_document: 1 block on 1 page (letter landscape, 9 pt)
#>  page 1  [sec 1]  Demog  rtftable 3 rows  title 1
```

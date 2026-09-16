# Build a structured TOC heading

Use inside
[`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md)'s
`toc =` list to insert a section heading (no clickable link, no page
number) above a group of
[`toc_entry()`](https://ichirio.github.io/rtfreporter/reference/toc_entry.md)s.

## Usage

``` r
toc_heading(label, level = 1L)
```

## Arguments

- label:

  Character; the heading text.

- level:

  Integer; 1 (default) or 2. Controls indent depth in the rendered TOC.

## Value

A list of class `"rtf_toc_heading"`.

## Examples

``` r
toc_heading("EFFICACY ANALYSES", level = 1)
#> $label
#> [1] "EFFICACY ANALYSES"
#> 
#> $level
#> [1] 1
#> 
#> attr(,"class")
#> [1] "rtf_toc_heading"
```

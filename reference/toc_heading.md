# Build a structured TOC heading (deprecated)

**Deprecated** in 0.8.x (warns once a session, still works); removed in
0.9.0. Give
[`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md)
the table of contents as a table: a row whose `heading` column is filled
starts a heading.

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

## Details

Use inside
[`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md)'s
`toc =` list to insert a section heading (no clickable link, no page
number) above a group of
[`toc_entry()`](https://ichirio.github.io/rtfreporter/reference/toc_entry.md)s.

## Examples

``` r
# instead: the heading column of the table of contents
data.frame(heading = c("EFFICACY ANALYSES", NA), label = c("Table 14.2.1", "Table 14.2.2"),
           file = c("t14_2_1.rtf", "t14_2_2.rtf"))
#>             heading        label        file
#> 1 EFFICACY ANALYSES Table 14.2.1 t14_2_1.rtf
#> 2              <NA> Table 14.2.2 t14_2_2.rtf
```

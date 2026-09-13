# Build a structured TOC entry

Use inside
[`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md)'s
`toc =` list to add a clickable TOC entry pointing at one of the
`input_files`.

## Usage

``` r
toc_entry(label, file = NULL, level = 2L)
```

## Arguments

- label:

  Character; the entry text.

- file:

  Either a path that appears in `input_files`, an integer 1-based index
  into `input_files`, or `NULL` (default: consume the next file in
  order).

- level:

  Integer; 1 to 3. Indent depth in the rendered TOC (1 = flush left, 2 =
  small indent, ...).

## Value

A list of class `"rtf_toc_entry"`.

## Examples

``` r
toc_entry("Table 14.1.1 Demographics",  file = "t14_1_1.rtf", level = 2)
#> $label
#> [1] "Table 14.1.1 Demographics"
#> 
#> $file
#> [1] "t14_1_1.rtf"
#> 
#> $level
#> [1] 2
#> 
#> attr(,"class")
#> [1] "rtf_toc_entry"
toc_entry("Listing 16.1 Disposition")   # auto-bound to the next file
#> $label
#> [1] "Listing 16.1 Disposition"
#> 
#> $file
#> NULL
#> 
#> $level
#> [1] 2
#> 
#> attr(,"class")
#> [1] "rtf_toc_entry"
```

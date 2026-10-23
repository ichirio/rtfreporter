# Build a structured TOC entry (deprecated)

**Deprecated** in 0.8.x (warns once a session, still works); removed in
0.9.0. Give
[`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md)
the table of contents as a table: one row per file (`file`, `label`,
`level`).

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

## Details

Use inside
[`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md)'s
`toc =` list to add a clickable TOC entry pointing at one of the
`input_files`.

## Examples

``` r
# instead: one row of the table of contents
data.frame(file = "t14_1_1.rtf", label = "Table 14.1.1 Demographics", level = 2)
#>          file                     label level
#> 1 t14_1_1.rtf Table 14.1.1 Demographics     2
```

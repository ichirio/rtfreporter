# Show the column header of every page, cell by cell

One row per header cell of every page: which page it is on, the keys
that page was cut at, which columns the cell covers, and the text it
ends up with. It is the check for a header built from a template and a
`values` table – the mapping is visible at a glance, and assertable in a
test.

    pages |> set_col_header(hdr, values = vals) |> header_map()
    #>  page name              group   rows row cell from to text
    #>  1    Period 1...1      Period 1  1    1    2    2    9 "Placebo\n(N=120)..."

## Usage

``` r
header_map(x)
```

## Arguments

- x:

  An
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  or a list of them (pages).

## Value

A `data.frame` with one row per header cell: `page`, `name`, `group`,
`rows`, `row` (which header row), `cell`, `from`, `to`, `text`. A page
with no column header contributes nothing.

## See also

[`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md),
whose `values` argument this checks.

## Examples

``` r
df <- data.frame(Parameter = c("Mean", "SD"), A = c("1", "2"),
                 B = c("3", "4"), stringsAsFactors = FALSE)
tbl <- rtftable(df) |> set_col_header(c("Parameter", "Drug", "Placebo"))
header_map(tbl)
#>   page name group rows row cell from to      text
#> 1    1 <NA>  <NA> <NA>   1    1    1  1 Parameter
#> 2    1 <NA>  <NA> <NA>   1    2    2  2      Drug
#> 3    1 <NA>  <NA> <NA>   1    3    3  3   Placebo
```

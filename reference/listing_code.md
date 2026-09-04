# Print a listing spec as the code that would build it

Turns a
[`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md)
into the source you would have written by hand, so a spec that came out
of
[`fit_listing_widths()`](https://ichirio.github.io/rtfreporter/reference/fit_listing_widths.md)
can be pasted into the program and tuned there. The measurement is a
starting point; the code is where the decisions get made and reviewed.

## Usage

``` r
listing_code(spec, name = NULL, indent = 2L)
```

## Arguments

- spec:

  A
  [`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md).

- name:

  Name to assign the spec to, e.g. `"listing"` produces
  `listing <- listing_spec(...)`. `NULL` (default) writes the call
  alone.

- indent:

  Number of spaces the
  [`listing_col()`](https://ichirio.github.io/rtfreporter/reference/listing_col.md)
  calls are indented by (default `2`).

## Value

A character vector of source lines, one per line of code, with class
`rtf_listing_code` – printing it shows the code ready to copy.

## Details

Only what differs from the listing's own defaults is written out, so the
result reads like something a person wrote rather than a dump of every
setting.

A spec straight from
[`fit_listing_widths()`](https://ichirio.github.io/rtfreporter/reference/fit_listing_widths.md)
therefore comes out in full – `width`, `rel_width` and `label` on every
column – because the fit wrote all three down. That is the point: it is
a template to edit.

## See also

[`fit_listing_widths()`](https://ichirio.github.io/rtfreporter/reference/fit_listing_widths.md),
which proposes the widths this writes out.

## Examples

``` r
spec <- listing_spec(list(
  listing_col("USUBJID", width = 15, label = "Unique\nSubject ID"),
  listing_col(c("AGE", "SEX"), width = 12, layout = "flow"),
  listing_col("STAGE", width = 9)
))

listing_code(spec)
#> listing_spec(list(
#>   listing_col("USUBJID", width = 15,
#>     label = "Unique\nSubject ID"),
#>   listing_col(c("AGE", "SEX"), width = 12, layout = "flow"),
#>   listing_col("STAGE", width = 9)
#> ))
listing_code(spec, name = "listing")
#> listing <- listing_spec(list(
#>   listing_col("USUBJID", width = 15,
#>     label = "Unique\nSubject ID"),
#>   listing_col(c("AGE", "SEX"), width = 12, layout = "flow"),
#>   listing_col("STAGE", width = 9)
#> ))

# The usual round trip: measure, print, paste, tune.
code <- listing_code(spec, name = "listing")
writeLines(code)
#> listing <- listing_spec(list(
#>   listing_col("USUBJID", width = 15,
#>     label = "Unique\nSubject ID"),
#>   listing_col(c("AGE", "SEX"), width = 12, layout = "flow"),
#>   listing_col("STAGE", width = 9)
#> ))
```

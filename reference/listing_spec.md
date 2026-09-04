# Bundle the settings for one listing

Collects the columns of a listing and the settings that apply to all of
them, so that
[`build_listing()`](https://ichirio.github.io/rtfreporter/reference/build_listing.md)
and `as_rtftables(listing = )` take a single argument and read exactly
the same object – neither one carries a copy of the other's argument
list.

## Usage

``` r
listing_spec(
  cols,
  type = "multiline",
  sep = NULL,
  spacer = NULL,
  spacer_rel_width = NULL,
  blank_row = NULL,
  blank_row_first = NULL,
  align = NULL,
  layout = NULL,
  record = TRUE
)
```

## Arguments

- cols:

  The printed columns, in order: a list of
  [`listing_col()`](https://ichirio.github.io/rtfreporter/reference/listing_col.md)
  objects. A bare string (or a character vector) stands for
  [`listing_col()`](https://ichirio.github.io/rtfreporter/reference/listing_col.md)
  on it, so `cols = c("USUBJID", "AGE")` means two unwrapped columns.

- type:

  Listing template name; see *Listing types*. Default `"multiline"`.

- sep:

  Default separator for a
  [`listing_col()`](https://ichirio.github.io/rtfreporter/reference/listing_col.md)
  that does not set its own. `NULL` (default) takes the template's.

- spacer:

  Logical. Insert a narrow blank column between each pair of printed
  columns. `NULL` (default) takes the template's.

- spacer_rel_width:

  Relative width of those gutter columns. `NULL` (default) takes the
  template's.

- blank_row:

  Logical. End each record's block with a blank row, so one subject is
  visibly separated from the next. `NULL` (default) takes the
  template's.

- blank_row_first:

  Logical. Start each page with a blank row. Passed on as
  `as_rtftables(blank_row_first = )`, which is what makes it *per page*
  rather than once per listing. `NULL` (default) takes the template's.

- align:

  Default alignment for columns that do not set their own. `NULL`
  (default) takes the template's.

- layout:

  Default cell layout for columns that do not set their own: `"stack"`
  breaks after every separator, `"flow"` fills each line as far as the
  column's `width` allows. See
  [`listing_col()`](https://ichirio.github.io/rtfreporter/reference/listing_col.md).
  `NULL` (default) takes the template's.

- record:

  `TRUE` (default) appends the hidden record column under its standard
  name, `FALSE` appends none, or a single string names it yourself. See
  *The record column*.

## Value

An object of class `rtf_listing_spec`.

## Listing types

`type` names a template that supplies every default below, and any
argument you pass explicitly overrides it – the same relationship
`rtftable(border = "tfl")` has with its preset. One type ships:

- `"multiline"`:

  The layout a wide clinical listing usually wants: a `"/"` separator,
  gutter columns between the printed ones, a blank row after each record
  and one at the top of every page, everything left aligned, and text
  wrapped at the separator first and at word boundaries only where a
  piece is still too long.

Adding a type later changes no signature: it is one entry in the
internal registry, and it brings its own wrapping rule with it.

## The record column

A listing wraps one source row over several physical rows, so a page
break must not land inside one. `record` asks
[`build_listing()`](https://ichirio.github.io/rtfreporter/reference/build_listing.md)
to append a hidden column holding the source row number;
`as_rtftables(listing = )` then points `group_col` at it, splits with
`"group_safe"` and lists it in `drop_cols`, so it decides the page
breaks and is never printed. Set `record = FALSE` only if you intend to
paginate some other way.

## See also

[`listing_col()`](https://ichirio.github.io/rtfreporter/reference/listing_col.md)
for one column;
[`build_listing()`](https://ichirio.github.io/rtfreporter/reference/build_listing.md),
which applies this to a data.frame;
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md),
whose `listing` argument does the same thing inside the rendering
pipeline.

## Examples

``` r
spec <- listing_spec(list(
  listing_col("USUBJID", width = 15, label = "Unique\nSubject ID"),
  listing_col(c("SEX", "AGE"), width = 12, label = "Sex/\nAge"),
  listing_col("ARM", width = 20, label = "Treatment Arm")
))
spec
#> <rtf_listing_spec>
#>   type    : multiline
#>   columns : 3 (+ gutters)
#>     - USUBJID  <- USUBJID  [wrap 15]
#>     - SEX  <- SEX / AGE  [wrap 12]
#>     - ARM  <- ARM  [wrap 20]
#>   record  : .rtf_record

# Bare names are columns too: two unwrapped columns, no gutters.
listing_spec(c("USUBJID", "ARM"), spacer = FALSE)
#> <rtf_listing_spec>
#>   type    : multiline
#>   columns : 2
#>     - USUBJID  <- USUBJID
#>     - ARM  <- ARM
#>   record  : .rtf_record
```

# One printed column of a listing

Describes a single column of a listing: which source variables it is
built from, how they are joined, how wide it may be before its text
wraps onto a further physical row, and what its header says. A list of
these is what
[`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md)
takes, and
[`build_listing()`](https://ichirio.github.io/rtfreporter/reference/build_listing.md)
turns into a data.frame.

## Usage

``` r
listing_col(
  vars,
  sep = NULL,
  width = NULL,
  label = NULL,
  name = NULL,
  rel_width = NULL,
  align = NULL,
  layout = NULL,
  collapse_repeats = FALSE
)
```

## Arguments

- vars:

  Character. One or more source column names, joined with `sep` in the
  order given. Missing (`NA`) and empty values are skipped, so a record
  missing its middle value does not print a doubled separator.

- sep:

  Separator for `vars`. `NULL` (default) takes the listing's own (`"/"`
  under the `"multiline"` type).

- width:

  Integer or `NULL`. Maximum characters per physical row before the cell
  wraps. `NULL` (default) never wraps this column.

- label:

  Column header text. A line break starts a further header row, as
  everywhere else in rtfreporter, and what you write is used **exactly**
  – you laid the lines out, so it is never re-wrapped. `NULL` (default)
  **derives** the header from the data: each source column's `label`
  attribute when it has one, otherwise its name, joined with `sep` and a
  line break, then wrapped to `width` so it cannot be wider than the
  column it sits over. `""` asks for a deliberately empty header.

- name:

  Output column name. `NULL` (default) uses the first entry of `vars`;
  [`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md)
  makes the set unique if two columns collide.

- rel_width:

  Relative width of this column in the rendered table. `NULL` (default)
  uses `width` when there is one, otherwise the longest line of `label`.

- align:

  `"left"`, `"center"` or `"right"`. `NULL` (default) takes the
  listing's own (`"left"` under the `"multiline"` type).

- layout:

  How a cell lays its parts out: `"stack"` breaks after **every**
  separator, so each source column starts its own line – the
  conventional listing look, and it keeps a column reading down the
  page. `"flow"` treats the separator as a break *opportunity* and fills
  each line as far as `width` allows, so a column of short parts (an age
  and a sex, a value and its unit) does not spend two rows on four
  characters. `NULL` (default) takes the listing's own (`"stack"` under
  `"multiline"`). With no `width` there is nothing to lay out and both
  behave alike.

- collapse_repeats:

  Logical (default `FALSE`). Mark this as a **key** column: its value is
  carried down every physical row of a record, and
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
  then blanks the repeats. Carrying and delegating rather than blanking
  here is what makes the value **reappear at the top of the next page**
  – the suppression happens per page, after the split, so a record
  continued across a page break still shows its subject. A cell that
  already wraps onto several lines is padded as usual: there is no one
  value to repeat.

## Value

An object of class `rtf_listing_col`.

## Details

`vars` may name **several** columns: their values are joined with `sep`,
missing and empty values dropped, so a column reading
`"ADENOCARCINOMA/BRCA1/GRADE 3"` is written
`listing_col(c("HIST", "BRCA", "HISTGRD"))` rather than pasted by hand
upstream.

`width` is a **display width in characters** (a full-width CJK glyph
counts as two), not a rendered width: it decides where the text of this
column breaks onto another physical row, and so how tall each record's
block is. It does not set the column's width in the table – that is
`rel_width`, which defaults to `width` when you give one.

## See also

[`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md),
which collects these;
[`build_listing()`](https://ichirio.github.io/rtfreporter/reference/build_listing.md),
which applies them.

## Examples

``` r
# One source column, wrapped at 15 characters.
listing_col("USUBJID", width = 15, label = "Unique\nSubject ID")
#> <rtf_listing_col>
#>   name  : USUBJID
#>   vars  : USUBJID
#>   width : 15
#>   label : Unique / Subject ID

# Three source columns in one printed column, joined with "/".
listing_col(c("DISPTPD", "BRCA", "HIST"), width = 22,
            label = "Primary Diagnosis/\nAny (BRCA) Mutations/\nHistology")
#> <rtf_listing_col>
#>   name  : DISPTPD
#>   vars  : DISPTPD, BRCA, HIST
#>   width : 22
#>   label : Primary Diagnosis/ / Any (BRCA) Mutations/ / Histology

# No wrapping, and a header only.
listing_col("STAGE", label = "Stage at\nInitial\nDiagnosis")
#> <rtf_listing_col>
#>   name  : STAGE
#>   vars  : STAGE
#>   width : (no wrap)
#>   label : Stage at / Initial / Diagnosis

# Header left to the data's own labels; short parts kept side by side.
listing_col(c("AGE", "SEX"), width = 12, layout = "flow")
#> <rtf_listing_col>
#>   name  : AGE
#>   vars  : AGE, SEX
#>   width : 12

# A key column: printed once per record, again atop the next page.
listing_col("USUBJID", width = 15, collapse_repeats = TRUE)
#> <rtf_listing_col>
#>   name  : USUBJID
#>   vars  : USUBJID
#>   width : 15
```

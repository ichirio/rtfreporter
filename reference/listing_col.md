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

  A positive whole number or `NULL`. Maximum characters per physical row
  before the cell wraps. `NULL` (default) never wraps this column. A `/`
  (the separator) inside a value is not a place to break while the value
  fits: see `layout`.

- label:

  The column header. `NULL` (default) **derives** the header from the
  data – each source column's `label` attribute when it has one,
  otherwise its name, joined with `sep` – or from
  `fit_listing_widths(labels = )`.

  A character vector

  :   is the lines – one element, one line – and so is a string that
      carries its own line breaks: both say "I have laid this out", and
      neither is re-wrapped.

  A single string with no breaks

  :   is words for the package to lay out: it is wrapped to `width` by
      the same rule the cells use (see
      [`listing_wrap()`](https://ichirio.github.io/rtfreporter/reference/listing_wrap.md)),
      or left as one line when there is no `width`. On a column of one
      variable a `/` in it is words, not a join: `"Age/Sex"` in a
      `width` of 10 stays on one line, and breaks after the `/` only
      when it is too wide.

  `""`

  :   asks for a deliberately empty header.

- name:

  Output column name. `NULL` (default) uses the first entry of `vars`;
  [`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md)
  makes the set unique if two columns collide.

- rel_width:

  Relative width of this column in the rendered table, a positive finite
  number. `NULL` (default) uses `width` when there is one, otherwise the
  longest line of `label`; with neither, 10.

- align:

  `"left"`, `"center"` or `"right"`. `NULL` (default) takes the
  listing's own (`"left"` under the `"multiline"` type).

- layout:

  How a cell lays its parts out. `NULL` (default) takes the listing's
  own (`"stack"` under `"multiline"`). With no `width` there is nothing
  to lay out and both behave alike.

  `"stack"`

  :   puts each source column on its own line – the conventional listing
      look, and it keeps a column reading down the page. The separator
      is what breaks **between** the variables joined in a cell; a
      separator inside one variable's value is data. So every variable's
      value is laid out on its own and the lines are stacked: a value
      that fits stays on one line (`64/M` in a `width` of 10 is one
      line), and only a value too wide breaks, after its separator
      first. A column of one variable has nothing to stack, so it is
      simply laid out as `"flow"` is.

  `"flow"`

  :   treats the separator as a break *opportunity* and fills each line
      as far as `width` allows, across the variables, so a column of
      short parts (an age and a sex, a value and its unit) does not
      spend two rows on four characters.

- collapse_repeats:

  Logical (default `FALSE`). Mark this as a **key** column: its value is
  carried down every physical row of a record (its blank row too), and
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
  then blanks the repeats – so the value prints once per run of records
  that share it, blank rows between them or not.

  Why delegated

  :   Carrying and delegating rather than blanking here is what makes
      the value **reappear at the top of the next page** – the
      suppression happens per page, after the split, so a record
      continued across a page break still shows its subject.

  Wrapped cells

  :   A cell that already wraps onto several lines is padded as usual,
      but a record whose lines equal the previous record's are blanked
      all the same: a repeat is blanked whether or not the value wraps.

## Value

An object of class `rtf_listing_col`.

## Details

`vars` may name **several** columns: their values are joined with `sep`,
missing and empty values dropped, so a column reading
`"HYPERGLYCAEMIA/UPPER/GRADE 3"` is written
`listing_col(c("AEDECOD", "AELOC", "AESEV"))` rather than pasted by hand
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
listing_col(c("DCDECOD", "AELOC", "AEDECOD"), width = 22,
            label = "Disposition/\nLocation/\nPreferred Term")
#> <rtf_listing_col>
#>   name  : DCDECOD
#>   vars  : DCDECOD, AELOC, AEDECOD
#>   width : 22
#>   label : Disposition/ / Location/ / Preferred Term

# No wrapping, and a header only.
listing_col("AETOXGR", label = "Grade at\nInitial\nDiagnosis")
#> <rtf_listing_col>
#>   name  : AETOXGR
#>   vars  : AETOXGR
#>   width : (no wrap)
#>   label : Grade at / Initial / Diagnosis

# Header left to the data's own labels; short parts kept side by side.
listing_col(c("AGE", "SEX"), width = 12, layout = "flow")
#> <rtf_listing_col>
#>   name  : AGE
#>   vars  : AGE, SEX
#>   width : 12

# A vector is the header's lines, exactly as given.
listing_col("USUBJID", width = 15, label = c("Unique", "Subject ID"))
#> <rtf_listing_col>
#>   name  : USUBJID
#>   vars  : USUBJID
#>   width : 15
#>   label : Unique / Subject ID

# One string and a width: the package lays it out.
listing_col("AEDECOD", width = 16, label = "Preferred term of the event")
#> <rtf_listing_col>
#>   name  : AEDECOD
#>   vars  : AEDECOD
#>   width : 16
#>   label : Preferred term of the event

# A key column: printed once per record, again atop the next page.
listing_col("USUBJID", width = 15, collapse_repeats = TRUE)
#> <rtf_listing_col>
#>   name  : USUBJID
#>   vars  : USUBJID
#>   width : 15
```

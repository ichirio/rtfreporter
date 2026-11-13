# Reshape a data.frame into a listing body

Turns source data into the data.frame a listing prints: source variables
joined into their printed columns, long cells broken over as many
physical rows as they need, gutter columns between the printed ones, a
blank row after each record, and a hidden record column so a page break
never lands inside a record.

## Usage

``` r
build_listing(data, spec)
```

## Arguments

- data:

  A `data.frame` (or tibble) of source data – one row per record. An
  rlistings `listing_df` is **not** accepted: it has already been laid
  out by rlistings, and goes straight to
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md).

- spec:

  A
  [`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md).

## Value

A `data.frame`: the printed columns in order, gutter columns between
them, and (unless `record = FALSE`) the hidden record column last. It
carries the spec as the attribute `rtf_listing`, which
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
reads.

## Details

This is preparation only – the result is an ordinary data.frame, and
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
does the rendering. The result carries the spec as an attribute, so the
two halves compose:


      build_listing(adsl, spec) |> as_rtftables(max_rows = 40)
      as_rtftables(adsl, listing = spec, max_rows = 40)   # the same thing

The second form is the usual one; reach for this one to look at (or
patch) the reshaped data before it is rendered.

## How a cell wraps

Under the `"multiline"` type, a cell made of several variables puts each
variable on its own line – the separator breaks **between** the
variables joined in a cell, and a separator inside one variable's value
is data. Each value is laid out by itself: it stays on one line if it
fits `width` (a `64/M` is one line in a `width` of 10), and if it is too
wide it breaks after its separator first; a piece that is still too long
breaks again at a word boundary (after a space, comma or hyphen); and a
token that is *still* too wide is hard-split, so **every line fits the
column it was measured against**. A column of one variable is laid out
the same way, as one value. A line break already in the data is honoured
before any of this. The header is laid out like the cells.

With `layout = "flow"` the variables are not stacked: the cell is
composed and its separators are places to break, filling each line.

`width` is a **display width**: a full-width (CJK) glyph occupies two
monospaced columns and is counted as two, so a Japanese listing wrapped
to 20 really does fit in twenty.

Every column of one record is padded to the tallest, so the record's
rows stay aligned across columns.

## See also

[`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md)
and
[`listing_col()`](https://ichirio.github.io/rtfreporter/reference/listing_col.md)
for the settings;
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md),
whose `listing` argument runs this inside the pipeline.

## Examples

``` r
adsl <- data.frame(
  USUBJID = c("01-701-1015", "01-701-1023"),
  AEDECOD = c("HYPERGLYCAEMIA", "ACUTE KIDNEY INJURY"),
  AELOC   = c("UPPER", NA),
  ARM     = c("Placebo", "Xanomeline High Dose"),
  stringsAsFactors = FALSE
)

spec <- listing_spec(list(
  listing_col("USUBJID", width = 11, label = "Unique\nSubject ID"),
  listing_col(c("AEDECOD", "AELOC"), width = 16,
              label = "Preferred Term/\nLocation"),
  listing_col("ARM", width = 12, label = "Treatment Arm")
))

body <- build_listing(adsl, spec)
body
#>       USUBJID .sp1         AEDECOD .sp2        ARM .rtf_record
#> 1 01-701-1015      HYPERGLYCAEMIA/         Placebo           1
#> 2                            UPPER                           1
#> 3                                                            1
#> 4 01-701-1023         ACUTE KIDNEY      Xanomeline           2
#> 5                           INJURY       High Dose           2
#> 6                                                            2
```

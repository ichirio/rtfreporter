# Propose the listing's column widths from the page and the data

Fills in the `width` of every
[`listing_col()`](https://ichirio.github.io/rtfreporter/reference/listing_col.md)
that does not set one, by measuring what the column demands and fitting
the demands into the width the page actually leaves. The result is the
same
[`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md),
ready to hand to
[`build_listing()`](https://ichirio.github.io/rtfreporter/reference/build_listing.md)
or `as_rtftables(listing = )` – or to
[`listing_code()`](https://ichirio.github.io/rtfreporter/reference/listing_code.md),
which prints it as source to paste and tune.

## Usage

``` r
fit_listing_widths(
  data,
  spec,
  page = NULL,
  font = "courier_new",
  size_half_points = 18L,
  total_width = NULL,
  labels = NULL,
  header_lines = 4L,
  min_width = 6L,
  probs = 0.9
)
```

## Arguments

- data:

  The source data, as passed to
  [`build_listing()`](https://ichirio.github.io/rtfreporter/reference/build_listing.md).

- spec:

  A
  [`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md).

- page:

  Page settings as an
  [`rtf_page()`](https://ichirio.github.io/rtfreporter/reference/rtf_page.md)
  object (or a plain list of the same fields) – the paper size,
  orientation and margins the listing will be rendered on. `NULL`
  (default) uses rtfreporter's own page defaults. Ignored when
  `total_width` is given.

- font, size_half_points:

  The font the listing renders in, used to turn the page's width into a
  number of characters. `size_half_points` is **half-points**, as
  everywhere in RTF: 8pt is `16L`, 9pt (the default) is `18L`. Getting
  it wrong silently changes the budget – at 8pt a landscape A4 with
  half-inch margins holds 159 characters, at 9pt only 141. Pass the same
  values the document uses in `rtf_document(default_format = )`.

- total_width:

  Character budget for the whole listing, gutters included. `NULL`
  (default) computes it from `page`, `font` and `size_half_points`. Give
  it directly when you know the budget and would rather not describe the
  page.

- labels:

  Named character vector giving the words for the **source** variables,
  e.g. `c(USUBJID = "Unique Subject ID", AGE = "Age")`. Used for data
  that carries no `label` attributes – a CSV, a frame built in the
  program, a [`subset()`](https://rdrr.io/r/base/subset.html) that
  dropped them. A column's header is still derived from these: the
  labels of its source variables are joined with its separator and
  wrapped to the width just fitted, so where the lines break is still
  worked out for you. Precedence: `listing_col(label = )`, then
  `labels`, then the variable's `label` attribute, then its name. A
  define/spec extract converts directly:
  `setNames(spec$label, spec$variable)`.

- header_lines:

  How many lines a header may reasonably occupy (default `4`). A column
  is asked to be at least wide enough for its header at that height, so
  a long label buys width in proportion to how tall it would otherwise
  make the header block. `Inf` asks for nothing on the header's behalf
  beyond the token it cannot break, which is the data-driven fit.

- min_width:

  Integer. The narrowest a fitted column may be (default `6`).

- probs:

  Quantile of a column's cell widths taken as its demand (default
  `0.9`).

## Value

The `listing_spec`, with a `width` on every column. It carries the
measurement as an attribute, which
[`print()`](https://rdrr.io/r/base/print.html) shows.

## How a width is chosen

The page decides the budget. The sheet's writable width (paper and
orientation minus the side margins) is divided by the width of one
character in `font` at `size_half_points`, which gives the number of
characters the listing has to spend. The gutter columns are subtracted
first – they are printed too – and so is every `width` you set yourself,
because those are decisions, not proposals.

A gutter costs `listing_spec(spacer_rel_width = )` characters, and the
default of `1` is a full character each: on a nine-gutter listing that
is close to 6% of the page. A fraction is allowed –
`spacer_rel_width = 0.25` gives the hairline divider a hand-tuned
listing usually uses.

Each remaining column's **demand** is the largest of four things:

- the `probs` quantile of the display widths of its composed cells – a
  quantile rather than the maximum, so one unusually long value wraps
  instead of pushing every other column narrow;

- the width its header needs to be no more than `header_lines` tall;

- the widest token its header cannot break, below which the header would
  be cut mid-word;

- `min_width`.

The second is what stops a long label being answered with a very narrow
column. A header wraps, so it does not need its full length – but
wrapping a 73-character label into nine characters makes a **ten-line
block, printed at the top of every page**. `header_lines` says how tall
a header may reasonably be, and the label's width divided by it is what
the column needs to get there. Lower it for a listing whose headers must
stay readable; raise it (or use `Inf`) to let the data have the page.

The demands are then scaled to the budget. Where scaling would take a
column below the width its header needs, the column is raised back to it
and the characters are taken from the columns that have room to spare –
**a header is never cut mid-word**. Only if the headers alone cannot fit
the page does that guarantee give way.

Where the data carries no `label` attributes, `labels` supplies the
words, and the measurement uses those.

Widths are display widths, so a full-width (CJK) glyph counts as two
throughout.

## See also

[`listing_code()`](https://ichirio.github.io/rtfreporter/reference/listing_code.md),
which turns the result into pasteable source;
[`listing_col()`](https://ichirio.github.io/rtfreporter/reference/listing_col.md)
for setting a width yourself;
[`auto_col_widths()`](https://ichirio.github.io/rtfreporter/reference/auto_col_widths.md),
the same idea for a table's columns in twips.

## Examples

``` r
adsl <- data.frame(
  USUBJID = c("01-701-1015", "01-701-1023"),
  HIST    = c("ADENOCARCINOMA", "SQUAMOUS CELL CARCINOMA OF THE LUNG"),
  BRCA    = c("BRCA1", NA),
  STAGE   = c("IIIB", "IV"),
  stringsAsFactors = FALSE
)

spec <- listing_spec(list(
  listing_col("USUBJID"),
  listing_col(c("HIST", "BRCA")),
  listing_col("STAGE")
))

# Landscape A4, half-inch margins, 8pt Courier.
fitted <- fit_listing_widths(
  adsl, spec,
  page = rtf_page(paper_size = "A4", orientation = "landscape",
                  margin_left_in = 0.5, margin_right_in = 0.5),
  size_half_points = 16L
)
fitted
#> <rtf_listing_spec>
#>   type    : multiline
#>   columns : 3 (+ gutters)
#>     - USUBJID  <- USUBJID  [wrap 34]
#>     - HIST  <- HIST / BRCA  [wrap 104]
#>     - STAGE  <- STAGE  [wrap 19]
#>   record  : .rtf_record
#>   fitted  : 157 + 2 gutter of 159 characters
#>     USUBJID  width =  34 fit (demand 11.0)
#>     HIST     width = 104 fit (demand 33.5)
#>     STAGE    width =  19 fit (demand 6.0)
#>   Paste listing_code(spec) into your program, then tune by eye.

# A width you set yourself is kept, and the rest fit around it.
spec2 <- listing_spec(list(
  listing_col("USUBJID", width = 12),
  listing_col(c("HIST", "BRCA")),
  listing_col("STAGE")
))
fit_listing_widths(adsl, spec2, total_width = 60)
#> <rtf_listing_spec>
#>   type    : multiline
#>   columns : 3 (+ gutters)
#>     - USUBJID  <- USUBJID  [wrap 12]
#>     - HIST  <- HIST / BRCA  [wrap 39]
#>     - STAGE  <- STAGE  [wrap 7]
#>   record  : .rtf_record
#>   fitted  : 58 + 2 gutter of 60 characters
#>     USUBJID  width =  12 set (demand 11.0)
#>     HIST     width =  39 fit (demand 33.5)
#>     STAGE    width =   7 fit (demand 6.0)
#>   Paste listing_code(spec) into your program, then tune by eye.
```

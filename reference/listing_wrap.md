# Break a listing cell or header into the lines it occupies

The rule
[`build_listing()`](https://ichirio.github.io/rtfreporter/reference/build_listing.md)
applies to every cell, exposed so it can be used on its own – to preview
where a column will break, or to lay a header out by hand and hand the
result to `listing_col(label = )`, which takes a character vector as its
lines.

## Usage

``` r
listing_wrap(text, width, sep = "/", layout = c("stack", "flow"))
```

## Arguments

- text:

  The text to break. A vector is wrapped element by element.

- width:

  Maximum display width per line, or `NULL` for no limit.

- sep:

  The separator to break after (default `"/"`). `""` or `NULL` skips
  step 1.

- layout:

  `"stack"` (default) breaks after **every** separator; `"flow"` treats
  it as a break opportunity and fills the line. See
  [`listing_col()`](https://ichirio.github.io/rtfreporter/reference/listing_col.md).

## Value

For a length-1 `text`, a character vector of lines; for a longer one, a
list of such vectors.

## Details

It is **exactly** the `"multiline"` type's own rule, so
`listing_spec(wrap = listing_wrap)` changes nothing, and it is the
reference implementation of that contract: a custom `wrap` is easiest to
write by delegating to this and adjusting around it. See
[`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md)
for the contract in full, and
[`listing_wrap_code()`](https://ichirio.github.io/rtfreporter/reference/listing_wrap_code.md)
for this rule's own source when the change belongs INSIDE it.

## The rule

1.  **After the separator.** `"COMPLETED/BRCA1"` prefers to break
    between its parts, and the separator stays at the end of the line it
    closes – the look a stacked listing column is expected to have.

2.  **At a word boundary** – after a space, a comma or a hyphen –
    filling each line as far as `width` allows.

3.  **Hard split**, for a token still too wide on its own. Every line
    returned therefore fits `width`, which is what lets the row count
    describe the page (see
    [`build_listing()`](https://ichirio.github.io/rtfreporter/reference/build_listing.md)).

`width` is a **display width**: a full-width (CJK) glyph counts as two.
A line break already in `text` is honoured before any of this. With no
`width` there is nothing to break against and `text` is returned as it
stands.

## See also

[`listing_col()`](https://ichirio.github.io/rtfreporter/reference/listing_col.md),
whose `width` applies this to a column's cells and whose `label` accepts
the result as header lines.

## Examples

``` r
listing_wrap("COMPLETED/BRCA1/ADENOCARCINOMA", 22)
#> [1] "COMPLETED/"     "BRCA1/"         "ADENOCARCINOMA"

# A token with nowhere to break is split, so every line fits.
listing_wrap("Immunohistochemistry", 8)
#> [1] "Immunohi" "stochemi" "stry"    

# Lay a header out by hand, then hand the lines to listing_col().
listing_col("HIST", width = 16,
            label = listing_wrap("Histology of the tumour", 16))
#> <rtf_listing_col>
#>   name  : HIST
#>   vars  : HIST
#>   width : 16
#>   label : Histology of / the tumour

# "flow" keeps short parts side by side.
listing_wrap("40/F", 20)
#> [1] "40/" "F"  
listing_wrap("40/F", 20, layout = "flow")
#> [1] "40/F"
```

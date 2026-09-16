# The measurements a listing's wrapping rule is built on

The three primitives
[`listing_wrap()`](https://ichirio.github.io/rtfreporter/reference/listing_wrap.md)
uses to lay text out, exported so that a rule of your own – see
[`listing_wrap_code()`](https://ichirio.github.io/rtfreporter/reference/listing_wrap_code.md)
– shares them rather than carrying a copy. They are useful on their own
too: a listing's arithmetic is done in DISPLAY widths, where a
full-width (CJK) glyph counts as two, and
[`nchar()`](https://rdrr.io/r/base/nchar.html) alone would let a
Japanese column ask for 20 columns and take up to 40.

## Usage

``` r
listing_disp_width(x)

listing_take(x, width)

listing_split_after(text, sep)
```

## Arguments

- x, text:

  Text to measure, cut or split. `listing_disp_width()` is vectorised;
  the other two take one string.

- width:

  Maximum display width.

- sep:

  The separator to break after. `""` or `NULL` returns `text` unsplit.

## Value

`listing_disp_width()` an integer vector; `listing_take()` a single
string; `listing_split_after()` a character vector of pieces.

## Details

`listing_disp_width()` measures. `listing_take()` cuts: the longest
prefix that still fits, never `""` for non-empty text, so a caller
looping on the remainder always makes progress even where one glyph is
wider than the whole column. `listing_split_after()` breaks after each
separator and keeps it at the end of the piece it closes, which is the
look a stacked listing column is expected to have.

## See also

[`listing_wrap()`](https://ichirio.github.io/rtfreporter/reference/listing_wrap.md),
the rule these build;
[`listing_wrap_code()`](https://ichirio.github.io/rtfreporter/reference/listing_wrap_code.md)
to fork that rule.

## Examples

``` r
listing_disp_width(c("ABC", "あいう"))   # 3 and 6
#> [1] 3 6
listing_take("ADENOCARCINOMA", 6)
#> [1] "ADENOC"
listing_split_after("COMPLETED/BRCA1", "/")
#> [1] "COMPLETED/" "BRCA1"     
```

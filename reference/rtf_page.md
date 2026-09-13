# Page geometry for an RTF document

Builds the `page` setting for
[`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md)
/
[`rtf_config()`](https://ichirio.github.io/rtfreporter/reference/rtf_config.md)
as a structured `rtf_page` object, so the available options and their
**defaults** are visible right here in the signature (rather than buried
in a named list).

## Usage

``` r
rtf_page(
  paper_size = "letter",
  orientation = "landscape",
  width_in = NULL,
  height_in = NULL,
  margin_top_in = 0.75,
  margin_bottom_in = 0.75,
  margin_left_in = 0.75,
  margin_right_in = 0.75,
  header_dist_in = NULL,
  footer_dist_in = NULL
)
```

## Arguments

- paper_size:

  A named preset (case-insensitive): `"letter"` (8.5x11"), `"legal"`
  (8.5x14"), `"A4"` (210x297mm), `"A3"`, or `"A5"`.

- orientation:

  `"landscape"` or `"portrait"`.

- width_in, height_in:

  Explicit page size in inches. When supplied these **win** over
  `paper_size`, and the orientation is *inferred* from them
  (`width_in >= height_in` means landscape). `NULL` (default) uses
  `paper_size`.

- margin_top_in, margin_bottom_in, margin_left_in, margin_right_in:

  The four page margins, in inches.

- header_dist_in, footer_dist_in:

  Distance (inches) of the header / footer band from the page edge.
  `NULL` (default) uses the **full** corresponding top / bottom margin
  (so the band sits at the margin boundary).

## Value

An `rtf_page` object (a classed named list) for `rtf_document(page =)`.

## Details

The default is **landscape Letter** with 0.9" top/bottom and 0.6"
left/right margins. A *site* can change any default by setting the
matching `rtfreporter.*` option (e.g. in `Rprofile.site`): an argument
you do not pass falls back to that option, so the resolution order is
**explicit argument \> `rtfreporter.*` option \> the factory default
shown below** (see
[`rtfreporter_options()`](https://ichirio.github.io/rtfreporter/reference/rtfreporter_options.md)).

## See also

[`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md),
[`rtf_default_format()`](https://ichirio.github.io/rtfreporter/reference/rtf_default_format.md),
[`rtfreporter_options()`](https://ichirio.github.io/rtfreporter/reference/rtfreporter_options.md).

## Examples

``` r
# Defaults made explicit (landscape Letter):
rtf_page()
#> <rtf_page> letter (landscape) 
#>   margins (in): top 0.75, bottom 0.75, left 0.75, right 0.75

# Wide TFL: narrow the left/right margins from the 0.75" default to 0.5"
# (still clears the eCTD 0.375" floor) to gain an inch of table width.
rtf_page(margin_left_in = 0.5, margin_right_in = 0.5)
#> <rtf_page> letter (landscape) 
#>   margins (in): top 0.75, bottom 0.75, left 0.5, right 0.5

# A4 portrait with tighter margins:
rtf_page(paper_size = "A4", orientation = "portrait",
         margin_left_in = 0.75, margin_right_in = 0.75)
#> <rtf_page> A4 (portrait) 
#>   margins (in): top 0.75, bottom 0.75, left 0.75, right 0.75

# Custom dimensions (orientation inferred -> portrait):
rtf_page(width_in = 8.5, height_in = 14)
#> <rtf_page> 8.5 x 14 in 
#>   margins (in): top 0.75, bottom 0.75, left 0.75, right 0.75

doc <- rtf_document(page = rtf_page(paper_size = "A4", orientation = "portrait"))
```

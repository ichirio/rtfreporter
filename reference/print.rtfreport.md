# Print an rtfreport object

Prints a compact summary of the internal `rtfreport` render product: its
page and section counts, the default page geometry (paper, orientation
and size in inches), and the font / colour table sizes.

## Usage

``` r
# S3 method for class 'rtfreport'
print(x, ...)
```

## Arguments

- x:

  An `rtfreport` object.

- ...:

  Additional arguments (unused).

## Value

`x`, invisibly. Called for the side effect of printing the summary.

## Details

Most users never build an `rtfreport` directly: assemble documents with
[`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md)
and the pipe verbs (that object has its own `print` method), then hand
them to
[`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md).
An `rtfreport` is the lower-level render product that
[`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md)
also accepts, and this method lets you inspect one if you hold it.

## Examples

``` r
df  <- data.frame(Parameter = c("Age", "Sex"),
                  Value = c("54.2 (11.3)", "31 (51.7%)"),
                  stringsAsFactors = FALSE)
doc <- rtf_document() |>
  rtf_tables(rtftable(df, border = "tfl"),
             titles = list("Table 14.1.1"))

# generate_rtfreport() returns the rendered report invisibly, so capture it
# to inspect what was written: how many pages, sections, titles.
f   <- tempfile(fileext = ".rtf")
rep <- generate_rtfreport(doc, f, overwrite = TRUE)
print(rep)
#> [1] "/tmp/Rtmp2STEp5/file1e3a20722e45.rtf"
unlink(f)
```

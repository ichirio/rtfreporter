# Print an rtfplot object

Prints a compact summary of an
[`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md)
figure: the image type and file, the image's native pixel size and DPI,
the display size that will be embedded (in inches and twips), and the
alignment.

## Usage

``` r
# S3 method for class 'rtfplot'
print(x, ...)
```

## Arguments

- x:

  An `rtfplot` object.

- ...:

  Additional arguments (unused).

## Value

`x`, invisibly. Called for the side effect of printing the summary.

## Examples

``` r
png_path <- tempfile(fileext = ".png")
grDevices::png(png_path, width = 600, height = 400)
graphics::plot(1:10, main = "A figure")
grDevices::dev.off()
#> agg_record_1e594df0444f 
#>                       2 
print(rtfplot(png_path, width_twips = 9000L))
#> <rtfplot> PNG: file1e595ebe0d5b.png
#>   Native size:  600 x 400 px  (dpi unknown, assuming 96)
#>   Display:      6.25 in (9000 twips) wide x 4.17 in (6000 twips)
#>   Align:        center
```

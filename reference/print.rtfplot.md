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
if (FALSE) { # \dontrun{
print(rtfplot("scatter.png", width_twips = 9000L))
} # }
```

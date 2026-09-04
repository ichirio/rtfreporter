# Visualise an `rtftable`

Draws a wireframe of the table: column-header band, data band (with the
row count annotated), spanning headers if any, and column widths drawn
proportional to the table's effective layout. Borders are sketched at
the table's outer frame and at zone boundaries (TFL preset is shown by
default).

## Usage

``` r
# S3 method for class 'rtftable'
plot(x, width = 8, ...)
```

## Arguments

- x:

  An
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  object.

- width:

  Plot width in inches (the page's writable width is assumed to be 10
  inches = landscape letter minus margins). Default `8`.

- ...:

  Unused.

## Value

Invisibly returns `x`.

## Examples

``` r
tbl <- rtftable(data.frame(Parameter = "Age", Value = "75.1"), border = "tfl")
if (FALSE) { # \dontrun{
plot(tbl)        # a quick on-screen preview of the table layout
} # }
```

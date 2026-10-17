# Restore the factory default options

Re-applies the package's factory baseline to every `rtfreporter.*`
option, discarding any site or session overrides. Use this to recover a
known state if a configuration has been changed in error.

## Usage

``` r
rtfreporter_reset_defaults()
```

## Value

Invisibly, the factory default list that was applied.

## See also

[`rtfreporter_options()`](https://ichirio.github.io/rtfreporter/reference/rtfreporter_options.md)
to inspect the active values.

## Examples

``` r
old <- options(rtfreporter.font = "Arial")
rtfreporter_reset_defaults()       # back to "Courier"
getOption("rtfreporter.font")
#> [1] "Courier"
options(old)
```

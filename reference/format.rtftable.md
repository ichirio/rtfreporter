# Render an rtftable body as console text

Lays an
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
out as a text grid – column headers (including spanning headers), the
rendered cells, per-column alignment, and horizontal rules where the
table's border zones are set – so the result previews what the RTF will
look like. Used by
[`print.rtftable()`](https://ichirio.github.io/rtfreporter/reference/print.rtftable.md);
call it directly to capture the rendered lines.

## Usage

``` r
# S3 method for class 'rtftable'
format(x, n = 10L, ...)
```

## Arguments

- x:

  An `rtftable` object.

- n:

  Number of body rows to render (default `10`).

- ...:

  Additional arguments (unused).

## Value

A character vector, one element per console line.

## See also

[`print.rtftable()`](https://ichirio.github.io/rtfreporter/reference/print.rtftable.md),
[`summary.rtftable()`](https://ichirio.github.io/rtfreporter/reference/summary.rtftable.md).

## Examples

``` r
dm <- data.frame(
  Characteristic = c("Age (years)", "  Mean (SD)", "  Median"),
  `Drug A`       = c("", "54.2 (11.3)", "55.0"),
  check.names = FALSE, stringsAsFactors = FALSE
)
tbl <- rtftable(dm, border = "tfl")

# Capture the console rendering instead of printing it -- useful for
# snapshot tests, or to paste a preview into an issue.
lines <- format(tbl)
length(lines)
#> [1] 6
writeLines(head(lines, 4))
#> ───────────────────────────
#> Characteristic    Drug A   
#> ───────────────────────────
#> Age (years)                

# Show more body rows than print() does by default
invisible(format(tbl, n = 3))
```

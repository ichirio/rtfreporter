# Bundle the row-stub settings for `as_rtftables()`

Collects everything
[`stub_cols()`](https://ichirio.github.io/rtfreporter/reference/stub_cols.md)
accepts into one object, so
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
takes a single `stub =` argument instead of one argument per setting.
The arguments are
[`stub_cols()`](https://ichirio.github.io/rtfreporter/reference/stub_cols.md)'s
own, so the two never drift apart.

## Usage

``` r
stub_spec(
  vars,
  label = NULL,
  indent = 4L,
  group_summary = c("empty", "parent"),
  layout = c("merged", "columns"),
  label_span = FALSE
)
```

## Arguments

- vars:

  The hierarchy columns, **parent first, leaf last** – at least two. See
  [`stub_cols()`](https://ichirio.github.io/rtfreporter/reference/stub_cols.md).

- label, indent, group_summary, layout, label_span:

  Passed to
  [`stub_cols()`](https://ichirio.github.io/rtfreporter/reference/stub_cols.md);
  see there for the full description of each.

## Value

An object of class `rtf_stub_spec`.

## Details

The `stub_vars` / `stub_label` / `stub_indent` / `stub_group_summary`
arguments of
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
are **superseded** by this: they still work and are not deprecated, but
they cannot reach `layout` or `label_span`, and new settings will only
be added here.

## See also

[`stub_cols()`](https://ichirio.github.io/rtfreporter/reference/stub_cols.md),
which does the work and documents every setting;
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md),
which consumes this through its `stub` argument.

## Examples

``` r
# The clinical indented stub, as as_rtftables(stub_vars = ) builds it.
stub_spec(c("SOC", "PT"), label = "System Organ Class / Preferred Term")
#> <rtf_stub_spec>
#>   vars       : SOC, PT 
#>   layout     : merged 
#>   indent     : 4 
#>   label      : System Organ Class / Preferred Term 

# Keep the hierarchy columns instead, with the group value on its own row.
stub_spec(c("SOC", "PT"), layout = "columns")
#> <rtf_stub_spec>
#>   vars       : SOC, PT 
#>   layout     : columns 
#>   indent     : 4 

# A merged stub whose group rows span the table.
stub_spec(c("SOC", "PT"), indent = 0, label_span = TRUE)
#> <rtf_stub_spec>
#>   vars       : SOC, PT 
#>   layout     : merged 
#>   indent     : 0 
#>   label_span : TRUE
```

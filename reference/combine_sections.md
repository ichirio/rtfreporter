# Combine table page lists into one auto-sectioned list

Assembles several converted tables – each an
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
or a list of them (typically an
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
result) – into a single flat list ready for
`rtf_tables(..., auto_section = TRUE)`, where every argument becomes
**one RTF section**.

## Usage

``` r
combine_sections(...)
```

## Arguments

- ...:

  Named arguments, each either an `rtftable` or a list of `rtftable`s
  (e.g. the result of
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)).
  Each argument **name** becomes a section heading. An unnamed argument
  is appended to the previous section (its pages all fall through).

## Value

A single flat, named list of `rtftable` objects suitable for
`rtf_tables(result, auto_section = TRUE)`.

## Details

`auto_section = TRUE` cuts a new section at each *named* element of the
list and drops that name into the section header; empty / unnamed
elements fall through into the current section. So to give a multi-page
table a single heading you name its **first** page and leave the rest
unnamed. `combine_sections()` does exactly that bookkeeping: each
argument's name is placed on its first page and the remaining pages are
blanked, so one logical (possibly paginated) table renders as one clean
section.

This is a thin, format-agnostic convenience – it only manipulates the
names of a list of `rtftable`s, with no knowledge of the source object.
It is not required: leaving every page named (or unnamed) simply yields
one section per page, which is harmless (it matches the common
one-page-one-section convention); `combine_sections()` is for the tidier
one-section-per-table layout.

For a *single* table whose grouping variable is a real **column** of the
body, you usually do not need this at all:
`as_rtftables(x, split = "by_value", group_col = ...)` already names
each page by the group value, so `auto_section = TRUE` gives one section
per group directly.

## See also

[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
(which produces the page lists, and whose `split = "by_value"` already
names pages per group),
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
for the `auto_section` argument.

## Examples

``` r
df <- data.frame(
  Characteristic = c("Age", "Sex", "Race", "Region"),
  Value          = c("75", "F", "White", "US"),
  stringsAsFactors = FALSE)
dm <- as_rtftables(df)                                  # one page
ae <- as_rtftables(df, split = "rows", split_rows = 2)  # two pages

# One clean section per table (the AE table's two pages share one section).
sections <- combine_sections(Demographics = dm, `Adverse Events` = ae)
names(sections)            # "Demographics" "Adverse Events" ""
#> [1] "Demographics"   "Adverse Events" ""              

if (FALSE) { # \dontrun{
doc <- rtf_document() |>
  rtf_section(secinfo = list(header = my_header)) |>
  rtf_tables(sections, auto_section = TRUE)
} # }
```

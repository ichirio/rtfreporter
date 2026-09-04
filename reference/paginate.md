# Split a table object into per-page data.frames (deprecated)

**Deprecated.** Use
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
instead, which both paginates *and* reads the source table's metadata
(column labels, alignment, spanning headers, per-cell styles, titles,
footnotes) into ready-to-render
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
page objects. `paginate()` only ever extracted the rendered body, so gt
metadata was silently lost when paginating a `gt_tbl`.

Single entry point that converts various supported table objects into a
list of data.frames, one per page, ready to be passed to
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
(each data.frame carries an `rtf_blank_rows` attribute that
`rtftable(read_attributes = TRUE)` consumes automatically).

Dispatch is by S3 class:

- `paginate.gt_tbl()` – for
  [`gt::gt()`](https://gt.rstudio.com/reference/gt.html) tables.
  Requires the optional `gt` package; an informative error is raised
  otherwise.

- `paginate.data.frame()` – for plain data.frames / tibbles. Where the
  real work happens; the gt method extracts a data.frame and delegates
  here.

- `paginate.list()` – recurses into every element and concatenates the
  resulting pages in input order.

New table-object types can be supported by adding another
`paginate.<class>()` method – see `vignette("paginate")` for an example.

## Usage

``` r
paginate(x, ...)

# Default S3 method
paginate(x, ...)

# S3 method for class 'gt_tbl'
paginate(x, ...)

# S3 method for class 'list'
paginate(x, ...)

# S3 method for class 'data.frame'
paginate(x, ...)
```

## Arguments

- x:

  A supported table object: a `gt_tbl` (from
  [`gt::gt()`](https://gt.rstudio.com/reference/gt.html)), a plain
  `data.frame` / tibble, or a `list` of either. List names are
  propagated to the output (one input -\> one page keeps the input name;
  one input -\> many pages produces `name.1`, `name.2`, ...).

- ...:

  Pagination controls, forwarded to the internal splitter and shared
  with
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md):
  `max_rows`, `split`, `split_rows`, `group_col`, `cont_label`,
  `blank_rows`, `blank_row_first`, `blank_row_end`, `align_count_pct`,
  `na`. See
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
  for the full description of each.

## Value

A list of data.frames (tibbles if the input was a tibble or `gt_tbl`),
one element per page. Each element carries:

- `attr(., "rtf_blank_rows")` – integer positions consumed by
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  when `read_attributes = TRUE`.

- `attr(., "rtf_paginate_meta")` – list with `strategy`, `page_index`,
  `total_pages`, `group_col`, `page_name`.

When `split = "by_value"` – or when the caller passed a *named* `list`
and no splitting happened – each element ALSO carries the group label as
its [`names()`](https://rdrr.io/r/base/names.html) entry. Pass the
resulting list directly to `rtf_tables(pages, auto_section = TRUE)` to
get one RTF section per page name.

## Examples

``` r
# -------------------------------------------------------------------------
# 1. Indent-based grouping (default)
#
# Column 1 carries the visual hierarchy via leading whitespace:
#   "Demographics"   - non-space first char -> opens group 1
#   "    Age"        - leading space         -> sub-row of group 1
#   "        Female" - deeper indent         -> still group 1
#   "Vital signs"    - non-space first char -> opens group 2
#   ... etc.
# No `group_col` argument is needed; the indent IS the signal.
# -------------------------------------------------------------------------
df <- data.frame(
  label = c(
    "Demographics",
    "    Age, mean (SD)",
    "    Sex, n (%)",
    "        Female",
    "        Male",
    "Vital signs",
    "    Systolic BP",
    "    Diastolic BP",
    "    Heart rate",
    "Lab values",
    "    Hemoglobin",
    "    Platelets"
  ),
  v = 1:12,
  stringsAsFactors = FALSE
)

pages <- paginate(
  df,
  max_rows        = 6,                  # at most 6 body rows / page
  split           = "group_safe",       # never break a group across pages
  blank_rows      = "between_groups",   # blank row between consecutive groups
  blank_row_first = TRUE,               # also a blank at the page top
  blank_row_end   = TRUE                # also a blank at the page bottom
)
#> Warning: '.warn_paginate_deprecated' is deprecated.
#> Use 'as_rtftables' instead.
#> See help("Deprecated") and help("rtfreporter-deprecated").

length(pages)                           # 3 pages (Demo / Vital / Lab)
#> [1] 3
lapply(pages, function(p) p$label)
#> [[1]]
#> [1] "Demographics"       "    Age, mean (SD)" "    Sex, n (%)"    
#> [4] "        Female"     "        Male"      
#> 
#> [[2]]
#> [1] "Vital signs"      "    Systolic BP"  "    Diastolic BP" "    Heart rate"  
#> 
#> [[3]]
#> [1] "Lab values"     "    Hemoglobin" "    Platelets" 
#> 
lapply(pages, attr, "rtf_blank_rows")   # e.g. page 1: c(0, 5)
#> [[1]]
#> [1] 0 5
#> 
#> [[2]]
#> [1] 0 4
#> 
#> [[3]]
#> [1] 0 3
#> 

if (FALSE) { # \dontrun{
# -------------------------------------------------------------------------
# 2. End-to-end: rtf_tables() picks up the blank-row attribute
# -------------------------------------------------------------------------
doc <- rtf_document() |>
  rtf_section(page = 1, secinfo = list(header = my_hdr)) |>
  rtf_tables(pages)         # one page per data.frame
generate_rtfreport(doc, "demo.rtf", overwrite = TRUE)

# -------------------------------------------------------------------------
# 3. gt input — same arguments, just hand a gt_tbl in
# -------------------------------------------------------------------------
pages <- paginate(my_gt_tbl, max_rows = 20, split = "group_force")

# -------------------------------------------------------------------------
# 4. List of gt tables (e.g. one per table number) — recurses and flattens
# -------------------------------------------------------------------------
all_pages <- paginate(list(t1_gt, t2_gt), max_rows = 20,
                      split = "group_force")

# -------------------------------------------------------------------------
# 5. Explicit group_col when the grouping signal isn't column 1's indent
# -------------------------------------------------------------------------
pages <- paginate(df, max_rows = 30, split = "group_safe",
                   group_col = "Visit")     # RLE on the Visit column

# -------------------------------------------------------------------------
# 6. split = "by_value": one page per group value, named by the value
# -------------------------------------------------------------------------
df <- data.frame(
  visit = c("Week 1","Week 1","Week 2","Week 2","Week 4"),
  val   = c(10, 11, 20, 22, 30)
)
pages <- paginate(df, split = "by_value", group_col = "visit")
names(pages)                 # "Week 1", "Week 2", "Week 4"

# Hand straight to rtf_tables(auto_section = TRUE) — one RTF section
# per visit, with the visit name as the section heading.
doc <- rtf_document() |>
  rtf_section(secinfo = list(header = my_hdr)) |>
  rtf_tables(pages, auto_section = TRUE)

# -------------------------------------------------------------------------
# 7. Named list input: names round-trip through paginate()
# -------------------------------------------------------------------------
pages_in <- list(
  "Table 14.1.1" = tibble::tibble(x = 1:3),
  "Table 14.2.1" = tibble::tibble(x = 4:6)
)
pages <- paginate(pages_in)              # no split, names preserved
names(pages)                              # "Table 14.1.1" "Table 14.2.1"
} # }
```

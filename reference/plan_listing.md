# Lay a frame of records out as a listing

`plan_listing()` makes the plan a **listing**: a data frame of subject
records (SDTM or ADaM, nothing summarised) printed through
[`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md),
with columns that can stack several variables in one cell and wrap long
text. A record wrapped over several rows is never cut by a page break.
Its arguments are
[`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md)'s
own, unchanged; the blank rows are
[`plan_blanks()`](https://ichirio.github.io/rtfreporter/reference/plan_blanks.md)'s,
as for a table.

## Usage

``` r
plan_listing(
  .plan,
  ...,
  type = NULL,
  sep = NULL,
  spacer = NULL,
  spacer_rel_width = NULL,
  layout = NULL,
  wrap = NULL,
  align = NULL,
  record = NULL
)
```

## Arguments

- .plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md)
  of a data frame of records.

- ...:

  The printed columns, in order:
  [`listing_col()`](https://ichirio.github.io/rtfreporter/reference/listing_col.md)s,
  or bare column names.

- type:

  The listing template,
  [`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md)'s
  `type`; the one that ships is `"multiline"` (the default): a `"/"`
  separator, gutter columns, a blank row after each record and one at
  the top of each page, text wrapped at the separator.

- sep, align, layout:

  The defaults for the columns that set none of their own: the separator
  between the variables of one cell, the alignment, and `"stack"` (a
  line a part) or `"flow"` (filled to the column's width). `NULL` takes
  the template's.

- spacer, spacer_rel_width:

  Whether a narrow gutter column goes between the printed columns, and
  its relative width. `NULL` takes the template's.

- wrap:

  The rule that breaks a cell into lines, a function as
  [`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md)
  describes; `NULL` the type's own
  ([`listing_wrap()`](https://ichirio.github.io/rtfreporter/reference/listing_wrap.md)).

- record:

  The hidden record column that keeps a record on one page: `TRUE` (the
  default, `NULL`) under its standard name, `FALSE` none, or a string
  naming it.

## Value

The plan, with one more layer.

## See also

[plan_verbs](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
for every verb;
[`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md)
and
[`listing_col()`](https://ichirio.github.io/rtfreporter/reference/listing_col.md)
for the listing in full.

Other plan verbs:
[`plan_after()`](https://ichirio.github.io/rtfreporter/reference/plan_after.md),
[`plan_append()`](https://ichirio.github.io/rtfreporter/reference/plan_append.md),
[`plan_blanks()`](https://ichirio.github.io/rtfreporter/reference/plan_blanks.md),
[`plan_cell_style()`](https://ichirio.github.io/rtfreporter/reference/plan_cell_style.md),
[`plan_cells()`](https://ichirio.github.io/rtfreporter/reference/plan_cells.md),
[`plan_col_header()`](https://ichirio.github.io/rtfreporter/reference/plan_col_header.md),
[`plan_columns()`](https://ichirio.github.io/rtfreporter/reference/plan_columns.md),
[`plan_compare()`](https://ichirio.github.io/rtfreporter/reference/plan_compare.md),
[`plan_digits()`](https://ichirio.github.io/rtfreporter/reference/plan_digits.md),
[`plan_empty()`](https://ichirio.github.io/rtfreporter/reference/plan_empty.md),
[`plan_filter()`](https://ichirio.github.io/rtfreporter/reference/plan_filter.md),
[`plan_footnotes()`](https://ichirio.github.io/rtfreporter/reference/plan_footnotes.md),
[`plan_hide()`](https://ichirio.github.io/rtfreporter/reference/plan_hide.md),
[`plan_labels()`](https://ichirio.github.io/rtfreporter/reference/plan_labels.md),
[`plan_levels()`](https://ichirio.github.io/rtfreporter/reference/plan_levels.md),
[`plan_nest()`](https://ichirio.github.io/rtfreporter/reference/plan_nest.md),
[`plan_paginate_cols()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_cols.md),
[`plan_paginate_group()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_group.md),
[`plan_paginate_rows()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_rows.md),
[`plan_row_group()`](https://ichirio.github.io/rtfreporter/reference/plan_row_group.md),
[`plan_sort()`](https://ichirio.github.io/rtfreporter/reference/plan_sort.md),
[`plan_stat_groups()`](https://ichirio.github.io/rtfreporter/reference/plan_stat_groups.md),
[`plan_stub()`](https://ichirio.github.io/rtfreporter/reference/plan_stub.md),
[`plan_style()`](https://ichirio.github.io/rtfreporter/reference/plan_style.md),
[`plan_titles()`](https://ichirio.github.io/rtfreporter/reference/plan_titles.md),
[`plan_total()`](https://ichirio.github.io/rtfreporter/reference/plan_total.md)

## Examples

``` r
# Invented adverse-event records
recs <- data.frame(
  USUBJID = c("01-001", "01-001", "01-002"),
  AETERM  = c("Headache", "Nausea", "Fatigue"),
  AESEV   = c("MILD", "MODERATE", "MILD"),
  ASTDY   = c(3, 10, 5))

# Minimal: two columns; the multiline type puts a blank row after each
# record
pages <- table_plan(recs) |>
  plan_listing(listing_col("USUBJID", label = "Subject"),
               listing_col("AETERM", label = "Adverse event")) |>
  plan_apply()
pages[[1]]
#> ─────────────────────────
#> Subject     Adverse event
#> ─────────────────────────
#> 01-001      Headache     
#>                          
#> 01-001      Nausea       
#>                          
#> 01-002      Fatigue      
#>                          
#> 
#> <rtftable> 6 rows x 3 columns
#>   Columns:    Subject |  | Adverse event
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     relative 7:1:13
#> 

# The event and its severity stacked in one cell (a line each, within
# ten characters), the study day beside it; no gutter columns and no
# blank rows, for a compact listing
pages <- table_plan(recs) |>
  plan_listing(listing_col("USUBJID", label = "Subject"),
               listing_col(c("AETERM", "AESEV"), width = 10,
                           label = "Adverse event/\nSeverity"),
               listing_col("ASTDY", label = "Study day"),
               spacer = FALSE) |>
  plan_blanks(where = "none", first = FALSE) |>
  plan_apply()
pages[[1]]
#> ──────────────────────────────────
#>          Adverse event/           
#> Subject  Severity        Study day
#> ──────────────────────────────────
#> 01-001   Headache/       3        
#>          MILD                     
#> 01-001   Nausea/         10       
#>          MODERATE                 
#> 01-002   Fatigue/        5        
#>          MILD                     
#> 
#> <rtftable> 6 rows x 3 columns
#>   Columns:    Subject | Adverse event/ Severity | Study day
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     relative 7:10:9
#> 
```

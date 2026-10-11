# Flatten an ARD into an explicitly keyed long table

Step one of the ARD conversion: turn the `group1 / group1_level`
*positional* pairs into columns named after the grouping variables,
flatten every list-column, attach the formatted statistic (`stat_fmt`),
and – when the ARD is hierarchical – fold each level's own summary rows
into the right key column and record the nesting depth.

## Usage

``` r
normalize_ard(
  x,
  keys = NULL,
  hierarchy = character(),
  overall = NULL,
  drop_contexts = c("attributes", "total_n"),
  drop_key_variables = FALSE
)
```

## Arguments

- x:

  A cards/cardx ARD.

- keys:

  Character vector of grouping-variable names to materialise as columns.
  `NULL` (default) uses every name found in the `group*` columns. This
  reads the tibble's rows, not its attributes.

- hierarchy:

  Character vector naming a nested hierarchy, outermost first – e.g.
  `c("AEBODSYS", "AEDECOD")`. For these variables the rows where
  `variable == <name>` are that level's own summary rows, and are folded
  into the matching key column.

- overall:

  Label to give the `..ard_hierarchical_overall..` rows produced by
  `cards::ard_stack_hierarchical(over_variables = TRUE)`, e.g.
  `"Any TEAE"`. They are placed on the first `hierarchy` column. `NULL`
  (default) leaves them alone.

- drop_contexts:

  `context` values to discard. The default drops the `attributes` rows
  (whose `stat` is a vector and cannot be flattened) and the `total_n`
  row. Dropping the total does not lose the NUMBER: when
  `..ard_total_n..` states one, it is kept on the `"ard_total_n"`
  attribute, because it is a denominator a column header asks for even
  though it is not a table statistic. Name only `"attributes"` to keep
  the row itself.

- drop_key_variables:

  The rows that describe a key variable itself – the
  `context == "tabulate"` counts of the by-variable – are no table cell,
  but they are often the only place an ARD states each column's size.
  `FALSE` (default) keeps them and marks them `.key_own = TRUE`, so
  [`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)
  leaves them out of the body while a column header can still read them.
  `TRUE` removes them outright.

## Value

A data frame with one row per ARD statistic: the key columns, the ARD's
own `variable` / `variable_level` / `context` / `stat_name` /
`stat_label` / `stat` / `stat_fmt`, the structural classification
`.kind` (`"continuous"` / `"categorical"`, see
[ard-tables](https://ichirio.github.io/rtfreporter/reference/ard-tables.md)),
`.label` (the deepest non-missing hierarchy value, or `variable_level`),
`.label_order` (that label's position in the level order its factor
declared, `NA` when it declared none), `.overall`, `.key_own` (`TRUE` on
a key variable's own tabulation, see `drop_key_variables`), and `.depth`
— 1 = outermost within a `hierarchy`, 0 = a row of one that is not one
of its levels, and `NA` throughout when no `hierarchy` was given, since
depth only means something inside one.

## Details

The result is a plain data frame that you can keep manipulating with
base R or dplyr before handing it to
[`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md).
That is the intended route for anything
[`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)
does not do by itself: marginal totals, derived rows, custom sorting.

## Working on the result before [`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)

The result is a plain data frame; reshaping it in between is the point
of the two-stage split. **Everything
[`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)
reads is in the columns**, so
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
[`filter()`](https://rdrr.io/r/stats/filter.html), `arrange()`,
`bind_rows()`, `select()` and base `[` are all safe, and so is a
one-pipe `normalize_ard() |> ... |> widen_ard()`. A manipulation that
really does break the rows is still caught – two values arriving in one
cell is an error, not a silent overwrite.

One attribute is left, `"ard_ignored"`, and nothing reads it to build
the table: it is a report about rows that are no longer in the frame, so
there is no column it could be. Dropping it (`mutate()` and `select()`,
base [`transform()`](https://rdrr.io/r/base/transform.html) and
[`subset()`](https://rdrr.io/r/base/subset.html) do) only makes `notes`
report the discards from
[`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)
alone.

## Factor levels

cards stores the level of a factor variable as a one-element factor, so
the flattening has to take the label: a level comes back as `"<65"`, not
as the integer code `1`. The order the factor declared is kept too, as
the `.label_order` column – each row's position within its variable's
declared levels – and
[`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)
rebuilds that variable's row order from it unless `levels` says
otherwise. Relabelling a level keeps its position, so indenting `"Mild"`
to `" Mild"` with `mutate()` still sorts where `"Mild"` was declared.

A **key** column holds one variable, so it can carry its order itself: a
key that was a factor in the data (a treatment variable declared
`factor(levels = c("Low", "Placebo", "High"))`) comes back a factor with
those levels, unused ones included, and
[`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)
lays the columns – or the rows, for a row key – out in that order
however the frame was reordered in between. `levels` still overrides it.
A key that was character stays character.

## See also

[`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md),
[`list_ard_keys()`](https://ichirio.github.io/rtfreporter/reference/list_ard_keys.md)

## Examples

``` r
if (requireNamespace("cards", quietly = TRUE)) {
  ard <- cards::ard_stack(
    cards::ADSL, .by = ARM,
    cards::ard_summary(variables = AGE),
    cards::ard_tabulate(variables = SEX))
  flat <- normalize_ard(ard)    # one row per statistic, keys as columns
  head(flat[, c("ARM", "variable", ".label", "stat_name", "stat", ".kind")])
}
#>       ARM variable .label stat_name      stat      .kind
#> 1 Placebo      AGE   <NA>         N 86.000000 continuous
#> 2 Placebo      AGE   <NA>      mean 75.209302 continuous
#> 3 Placebo      AGE   <NA>        sd  8.590167 continuous
#> 4 Placebo      AGE   <NA>    median 76.000000 continuous
#> 5 Placebo      AGE   <NA>       p25 69.000000 continuous
#> 6 Placebo      AGE   <NA>       p75 82.000000 continuous
```

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
  drop_key_variables = FALSE,
  stack = NULL,
  stack_sep = " / ",
  stack_nest = FALSE,
  combine = NULL
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
  `"Any TEAE"`. They are placed on the first `hierarchy` column, so
  `hierarchy` is needed with it. `NULL` (default) drops them, recorded
  in the `"ard_ignored"` attribute.

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

- stack:

  Subgroups stacked into one pair of keys: a named list of **layers**.
  The name is the new key; each layer is one variable or several
  crossed.

  What it adds

  :   `stack = list(subgroup = list("ALL", "SEX", "AGEGR1", c("AGEGR1", "SEX")))`
      adds `subgroup` (the layer: `"SEX"`, or crossed `"AGEGR1 / SEX"`)
      and `subgroup_level` (its level: `"F"`, `"65-80 / F"`) — what
      `group2` / `group2_level` say in an ARD whose subgroups all came
      in that slot, but **whatever slot** each came in (a subgroup is in
      `group3` as soon as a strata or a `by =` has one variable more,
      and a crossed one, `ard_strata(.strata = c(AGEGR1, SEX))`, is in
      both).

  Short forms

  :   A character vector is layers of one variable each
      (`c("ALL", "SEX", "AGEGR1")`); a crossed layer of three or more is
      the same rule. **Group slots** stand for what the ARD has there:
      `stack = list(subgroup = c("group2", "group3"))` is each
      combination of variables in those slots, in the order first seen
      (one variable, or several crossed in slot order). The layers
      written out are the general form.

  Which row goes where

  :   A row goes to the layer whose variables are **exactly** the
      stacked variables it has; a row whose combination no layer names
      is an error that says which layer to add. A row with none of them
      (an analysis by the arm alone) keeps both `NA`; an overall row is
      a constant stratum (`ALL = "All"`) stacked first, which the ARD
      and the ARS then state.

  Refused

  :   Slots that also hold a variable coming in a slot not named
      (`by = c(AGEGR1, TRT)` puts the arm in `group2`); a `hierarchy`
      does not stack.

  In the table

  :   `table_plan(rows = c(grp1 = "subgroup", grp2 = "subgroup_level"))`
      lays the layers one under another in the order written, each one's
      levels in their own order (crossed: the first variable turning
      slowest);
      [`plan_labels()`](https://ichirio.github.io/rtfreporter/reference/plan_labels.md)
      names them
      (`plan_labels(subgroup = c(SEX = "Sex", "AGEGR1 / SEX" = "Age Group by Sex"))`).
      The variables' own columns stay, so a table of crossed rows only
      can still be `rows = c("AGEGR1", "SEX")`.

  A complete ORR-by-subgroup example is in the article *Tables from an
  ARD, step by step*, *Subgroups*.

- stack_sep:

  What joins a crossed layer's names and levels: `" / "`.

- stack_nest:

  `TRUE` puts a crossed layer's outer levels in a key of their own,
  `<name>_outer` (`65-80`), and its innermost in `<name>_level` (`F`),
  so
  `rows = c(grp = "subgroup", outer = "subgroup_outer", lvl = "subgroup_level")`
  with
  [`plan_stub()`](https://ichirio.github.io/rtfreporter/reference/plan_stub.md)
  prints `Age Group by Sex`, `65-80` under it, `F` under that — while a
  single layer, whose outer key is `NA`, keeps its two levels with no
  empty heading. `FALSE` (default): the joined form, `65-80 / F`.

- combine:

  Variables the data keeps apart, read as one table dimension: a named
  list, each name a new name and each element the two or more variables
  read under it. The use is a lab shift table by direction: ADaM keeps
  the grade toward low and toward high in `ATOXGRL` / `ATOXGRH` (and
  `BTOXGRL` / `BTOXGRH` at baseline), the ARD is tabulated per direction
  under those names with a direction column of its own (`tox_dir`), and
  `combine = list(worst_gr = c("ATOXGRL", "ATOXGRH"), base_gr = c("BTOXGRL", "BTOXGRH"))`
  makes one column dimension (`base_gr`) and one row variable
  (`worst_gr`) of them. It renames only: an old name in `variable`
  becomes the new one, and old key columns fold into one key under the
  new name, at the first one's place, its levels the union of theirs in
  the order written (a factor where any was one). It knows nothing about
  directions. Stops on a row that has values of two variables of one
  element, a variable in two elements or in `hierarchy`, a variable the
  ARD does not have, and a new name that is already a column, a variable
  or a `stack` name. Done before `stack`, so a stacked key may name a
  combined one. `NULL` (default): nothing is combined. The whole table
  is in the article *Tables from an ARD, step by step*, *Lab shift table
  by direction*.

## Value

A data frame with one row per ARD statistic. The analysis variables do
not become columns: each stays a value of `variable`, with its rows
beside it. The columns: the key columns, the ARD's own `variable` /
`variable_level` / `context` / `stat_name` / `stat_label` / `stat` /
`stat_fmt`, the structural classification `.kind` (`"continuous"` /
`"categorical"`, see
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
`bind_rows()`,
[`select()`](https://dplyr.tidyverse.org/reference/select.html) and base
`[` are all safe, and so is a one-pipe
`normalize_ard() |> ... |> widen_ard()`. A manipulation that really does
break the rows is still caught – two values arriving in one cell is an
error, not a silent overwrite.

The study total
([`cards::ard_total_n()`](https://pharmaverse.github.io/cards/latest-tag/reference/ard_total_n.html),
a header's `{n}`) is the column `.total_n` – kept by
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
[`filter()`](https://rdrr.io/r/stats/filter.html) and base
[`transform()`](https://rdrr.io/r/base/transform.html) – as well as the
attribute `"ard_total_n"`; a
[`select()`](https://dplyr.tidyverse.org/reference/select.html) that
leaves the column out leaves the header's `{n}` without it.

One attribute is left, `"ard_ignored"`, and nothing reads it to build
the table: it is a report about rows that are no longer in the frame, so
there is no column it could be. Dropping it
([`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) and
[`select()`](https://dplyr.tidyverse.org/reference/select.html), base
[`transform()`](https://rdrr.io/r/base/transform.html) and
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
to `" Mild"` with
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) still
sorts where `"Mild"` was declared.

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

# combine: ADaM's Low and High grades as one dimension of a shift table
if (requireNamespace("cards", quietly = TRUE)) {
  gr <- function(x) factor(x, levels = c("0", "1", "2"))
  adlb <- data.frame(USUBJID = c("01", "02", "03"),
                     BTOXGRL = gr(c("0", "1", NA)), ATOXGRL = gr(c("1", "1", NA)),
                     BTOXGRH = gr(c(NA, "0", "0")), ATOXGRH = gr(c(NA, "2", "0")))
  lo <- adlb[!is.na(adlb$BTOXGRL), ]
  lo$tox_dir <- "Low"
  hi <- adlb[!is.na(adlb$BTOXGRH), ]
  hi$tox_dir <- "High"
  ard <- cards::bind_ard(
    cards::ard_tabulate(lo, variables = ATOXGRL, by = c(tox_dir, BTOXGRL)),
    cards::ard_tabulate(hi, variables = ATOXGRH, by = c(tox_dir, BTOXGRH)))
  shift <- normalize_ard(ard, combine = list(worst_gr = c("ATOXGRL", "ATOXGRH"),
                                             base_gr  = c("BTOXGRL", "BTOXGRH")))
  shift[shift$stat_name == "n", c("tox_dir", "base_gr", "variable", ".label", "stat")]
}
#>    tox_dir base_gr variable .label stat
#> 1      Low       0 worst_gr      0    0
#> 4      Low       0 worst_gr      1    1
#> 7      Low       0 worst_gr      2    0
#> 10     Low       1 worst_gr      0    0
#> 13     Low       1 worst_gr      1    1
#> 16     Low       1 worst_gr      2    0
#> 19     Low       2 worst_gr      0    0
#> 22     Low       2 worst_gr      1    0
#> 25     Low       2 worst_gr      2    0
#> 28    High       0 worst_gr      0    1
#> 31    High       0 worst_gr      1    0
#> 34    High       0 worst_gr      2    1
#> 37    High       1 worst_gr      0    0
#> 40    High       1 worst_gr      1    0
#> 43    High       1 worst_gr      2    0
#> 46    High       2 worst_gr      0    0
#> 49    High       2 worst_gr      1    0
#> 52    High       2 worst_gr      2    0
```

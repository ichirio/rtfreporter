# Turn a normalized ARD into a wide table data.frame

Step two of the ARD conversion. `widen_ard()` takes the long table from
[`normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.md)
(possibly after you have added rows of your own), builds one character
cell per template, and pivots the column keys across.

## Usage

``` r
widen_ard(
  x,
  cols,
  rows = NULL,
  label = ".label",
  cells = "{n} ({p})",
  stats = c("cells", "rows"),
  value = c("stat", "stat_fmt"),
  levels = NULL,
  labels = NULL,
  sort = FALSE,
  sep = "____",
  rounding = NULL,
  sort_stat = NULL,
  na = NA_character_,
  notes = TRUE
)
```

## Arguments

- x:

  A data frame from
  [`normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.md).

- cols:

  Column keys, outermost first. Multiple keys are pasted with `sep`,
  producing the `"Placebo____Day 1"` names that
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)'s
  `col_header` already reads as a spanning header. May be named, in
  which case the name is ignored for the column text.

- rows:

  Row keys, in output order – **any** column of the normalized frame,
  not a fixed one: the ARD's own `variable` when the row groups are the
  analysis variables (a demographics table), or a grouping variable's
  name when they are not (`"AEBODSYS"`). A named vector renames them,
  and the name is only the output column's name:
  `rows = c(group = "variable")` and `rows = c(group1 = "AEBODSYS")`
  differ in *what* they group by, not in kind.

  Left `NULL` on a flat ARD carrying more than one analysis variable, it
  defaults to `c(group = "variable")`, since that is the only thing left
  to group those rows by. One analysis variable, or any `hierarchy`,
  leaves it empty.

  A **formula** element is a template here too, which is how a constant
  row-group heading stops needing a `mutate()` of its own:
  `rows = c(param = "PARAM", grp = ~ "Worst Post-Baseline Values")`. A
  bare string is still a column name, so nothing already written changes
  meaning.

- label:

  Source of the row label, as a single (optionally named) reference.
  Default `".label"`, which
  [`normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.md)
  sets to the deepest hierarchy value, or to `variable_level` when there
  is no hierarchy. `NULL` drops the label column, which is what you want
  when every `cells` entry is named.

  `NA` builds the column, uses it to tell the rows apart, and then
  **drops it**: a recipe whose names are a row index — `"1"` for an
  estimate line and `"2"` for the confidence interval under it — needs
  them to separate two rows of one record, and does not want a column of
  1s and 2s in the result. `NULL` leaves the label out of the row
  identity altogether, so those two rows collide.

  An element that is a **formula** is a template over the record rather
  than a column name, and the chain works as `cells` does: the first
  element whose guard holds wins, and `~ "..."` is the unguarded one.
  Inside a template `{column}` interpolates, and `{.label}` is the label
  this row would otherwise carry. So indenting the severities under
  `Any` is a rule rather than a
  [`paste0()`](https://rdrr.io/r/base/paste.html) on `.label`:
  `label = c(.label %in% c("Mild", "Severe") ~ " {.label}", ~ "{.label}")`.

- cells:

  The cell recipes. A **character vector** is one recipe, used for every
  variable:

  - `"{n} ({p})"` – one row, labelled from `label`;

  - `c("{n} ({p})", "{n}")` – one row, the first template that resolves;

  - `c(n == 0 ~ "0", "{n} ({p})")` – the same chain, its first element
    **guarded**: a `condition ~ template` element applies only when the
    condition holds, so a guard that is false and a template that has no
    value for one of its tokens fail the same way, and the chain moves
    on. The condition is ordinary R, evaluated with the record's
    statistics by name (`n`, `p`, `mean`, ...) plus its own columns
    (`variable`, `.label`, `.depth`, `.kind` and the keys), and it may
    name the caller's variables too;

  - `c("Mean (SD)" = "{mean} ({sd})", "Min, Max" = "{min}, {max}")` –
    one row per element, the name being the row label.

  A **list** is instead a map, looked up by analysis variable, then by
  `context`, then by the structural kind, then by `"default"`; each of
  its elements is a recipe of the three shapes above. The container
  decides: `c("Mean (SD)" = ..., "Min, Max" = ...)` is a two-row recipe,
  while `list(continuous = ..., categorical = ...)` is a map. A list
  with no names is a chain, which is what
  [`c()`](https://rdrr.io/r/base/c.html) returns once a guard is in it.
  For a named row whose value is itself a chain, use
  [`cell_rows()`](https://ichirio.github.io/rtfreporter/reference/cell_rows.md).

  Statistics no template names are simply not read, which is how an ARD
  that also carries `method`, `alternative`, `conf.level` or a p-value
  converts without any filtering.

  See
  [ard-tables](https://ichirio.github.io/rtfreporter/reference/ard-tables.md)
  for the `{token:spec}` grammar.

- stats:

  `"cells"` (default) builds character cells from `cells`. `"rows"`
  ignores `cells` and gives every statistic its own row, labelled with
  `stat_label` – the shape a PK concentration table wants.

- value:

  Which of the ARD's two values `stats = "rows"` puts in the cell: the
  raw numeric `"stat"` (the default, so the table can still be aligned
  with
  [`set_decimal_split()`](https://ichirio.github.io/rtfreporter/reference/set_decimal_split.md))
  or `"stat_fmt"`, the string cards already formatted. Ignored when
  `stats = "cells"`, where the template says which it wants, token by
  token.

- levels:

  Named list of level orders, e.g.
  `list(TRT01P = c("Placebo", "Drug"), AGEGR = c("<65", ">=65"))`. A
  name may be

  - a **column key** – it then fixes the order of the spread columns,
    which is what keeps a hand-written `col_header` over the arm it
    names;

  - a **row key**, by either its source column or its renamed output
    column – it becomes a factor and drives the row sort;

  - an **analysis variable** – it orders that variable's rows in the
    label column, without your having to know what the label column is
    called. Variables you leave out keep their `cells` templates' order.

- labels:

  Named character vector recoding key *values* to display text, e.g.
  `c(AGE = "Age (years)", SEX = "Sex [n (\%)]")`. When a column is
  recoded and has no explicit `levels`, the order of `labels` becomes
  its level order. The two-parallel-vector spelling works just as well –
  `stats::setNames(group_labels, group_vars)` – but note that
  [`setNames()`](https://rdrr.io/r/stats/setNames.html) gives an element
  an `NA` name rather than complaining when the two vectors are
  different lengths, so that entry would never apply; every element must
  be named, and an unnamed one is an error here rather than a silent
  omission.

  One vector is one dictionary for the **whole table**, and the same
  value can mean two things on the two axes — a shift table's `"0"` is
  `"Grade 0"` down the side and `"Baseline 0"` across the top. Scope it
  the way `levels` already is, with a **list keyed by column**, matched
  on the output name then the source name, with `.default` covering the
  rest:

      labels = list(BGRADE = c("0" = "Baseline 0"),
                    WORST  = c("0" = "Grade 0"))

  A scope may name an **analysis variable** too:
  `labels = list(SEX = c(F = "Female", M = "Male"))` recodes that
  variable's levels in the label column, as
  `levels = list(SEX = c("M", "F"))` orders them (the order is written
  in the values, the label column prints the text). Its entry under the
  variable's own name is the variable's label:
  `SEX = c(SEX = "Sex", F = "Female", M = "Male")`.

- sort:

  `FALSE` (default) moves a row only where somebody **declared** an
  order. A key is a factor exactly when `levels`, `labels` or the data
  itself made it one — and making a column a factor is how a table says
  what its order is — so a factor key is sorted on, and so is the label
  column when `levels` gave it an order. Everything else stays where the
  data put it, and a declared order nested inside a plain key's block
  sorts within that block. Nothing is ever alphabetised behind your
  back. (The cells are gathered by their key before this, so a block is
  whole either way; what is left is where the blocks sit.)

  **Usually you write nothing here, or you name the keys.** A
  **character vector** names them, in priority order, each optionally
  prefixed `-` for descending. `TRUE` is the third, rarer answer: it
  **groups**, bringing a plain key's separate blocks together in the
  order they first appear, which no list of keys says without also
  choosing an order for them:

  `".overall"`

  :   the hierarchical-overall rows (an `Any TEAE` block) first.

  `".depth"`

  :   a level's own summary row before the rows nested under it.

  a column

  :   any row key or the label column, by its output name.

  a statistic

  :   that statistic totalled across the spread columns – what a
      descending-frequency AE table sorts on.

  So `sort = c(".overall", "soc", ".depth", "-n", "term")` is the whole
  of an AE table's row order, and needs neither `sort_stat` nor an
  `arrange()` afterwards.

- sep:

  Separator pasted between multiple `cols` keys.

- rounding:

  Tie-breaking rule for `{x:.1f}`-style tokens: `"r"` or `"sas"`.
  `NULL`, the default, reads `getOption("rtfreporter.rounding")` — the
  package's one rule, base R's half-to-even unless the study set `"sas"`
  once. It does **not** reach `{x}` or `{x:stat_fmt}`, which take a
  string cards already rounded (half away from zero, as it happens). See
  [`round_num()`](https://ichirio.github.io/rtfreporter/reference/round_num.md).

- sort_stat:

  Name of a statistic to total across the spread columns and attach as a
  numeric `.sort_stat` column – what a descending-frequency AE table
  sorts on. `NULL` (default) adds nothing.

- na:

  Value to put in a cell no template could fill.

- notes:

  Report what was **not** used. Every stage discards ARD rows – the
  `attributes` and `total_n` rows, the key variables' own tabulations,
  rows with no value for a `cols` key, and every statistic no template
  named – and discarding them in silence is how a mis-typed `cells`
  looks exactly like a correct one. `TRUE` (default) prints a summary,
  `FALSE` says nothing, and `"attr"` also attaches the per-variable
  detail as the `"ard_ignored"` attribute. It is not attached by default
  because the result is a plain data frame that you will compare against
  whatever you built before, and an extra attribute makes
  [`all.equal()`](https://rdrr.io/r/base/all.equal.html) report a
  difference that is not in the table.

  `"applied"` adds the other half: **which template produced each
  cell**, with the guard that let it through when it had one. A bare
  template is its own explanation, but a guarded chain is not — the
  finished cell cannot tell you which of its three candidates you got —
  so this is the companion to `cells` guards rather than a
  general-purpose log.

## Value

A data frame: the `rows` columns, the label column, then one column per
column key.

## Details

One ARD record becomes one table row. Layouts that put a single record
on two printed lines are deliberately out of scope – do those
afterwards, on the returned data frame.

## See also

[`normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.md),
`plan_template(form = "widen")`

## Examples

``` r
if (requireNamespace("cards", quietly = TRUE)) {
  ard <- cards::ard_stack(
    cards::ADSL, .by = ARM,
    cards::ard_summary(variables = AGE),
    cards::ard_tabulate(variables = SEX))
  normalize_ard(ard) |>
    widen_ard(cols = "ARM", rows = c(group = "variable"),
              cells = list(AGE = c("Mean (SD)" = "{mean:.1f} ({sd:.2f})"),
                           SEX = "{n} ({p:.1f%})"),
              notes = FALSE)
}
#>   group     label     Placebo Xanomeline High Dose Xanomeline Low Dose
#> 1   AGE Mean (SD) 75.2 (8.59)          74.4 (7.89)         75.7 (8.29)
#> 2   SEX         F   53 (61.6)            40 (47.6)           50 (59.5)
#> 3   SEX         M   33 (38.4)            44 (52.4)           34 (40.5)
```

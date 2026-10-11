# Tables from a cards/cardx ARD

A small family that turns an **ARD** (Analysis Results Data, as produced
by cards and cardx) into the table `data.frame` that
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
consumes. The target is *one ARD record, one table row*. Layouts that
print one record over two lines, or that need derived rows such as
marginal totals, stay a human job – do them on the returned data frame,
or on the long frame from
[`normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.md).

## The functions

- [`list_ard_keys()`](https://ichirio.github.io/rtfreporter/reference/list_ard_keys.md):

  What keys, variables, contexts and statistics an ARD actually holds.

- [`normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.md):

  ARD to a flat, explicitly keyed long table.

- [`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md):

  Long table to the wide table data.frame.

- `plan_template(form = "widen")`:

  Emit runnable conversion code for a given ARD.

- [`overall_row()`](https://ichirio.github.io/rtfreporter/reference/overall_row.md):

  Where the table's overall row comes from.

- [`pull_ard()`](https://ichirio.github.io/rtfreporter/reference/pull_ard.md):

  A statistic keyed like the spread columns, for a column header or an
  overall row.

- tflspec:

  The Excel definition of a table (`tflspec::tfl_table_spec()`) and
  `tflspec::tfl_table_plan()`, which builds a plan from it.

## Nothing is read from the object's attributes

A cards ARD carries attributes as well as rows, but they are not a
contract: `attr(ard, "args")` lists `by` and `variables` in a different
order per generator and cannot tell them apart, it keeps only the
**first** operand's value after
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html),
and it is not updated when the ARD is filtered. The ARD's class survives
`bind_rows()` with a differently shaped ARD, so dispatching on it is no
safer. Every structural decision here comes from an explicit argument
instead; only the tibble's rows are inspected.

## The cell template grammar

A template is a string with `{...}` tokens naming statistics:

|  |  |
|----|----|
| `{mean}` | the value cards itself formatted (`fmt_fun`) |
| `{mean:.1f}` | 1 decimal place, rounded per `rounding` |
| `{p:.1f\%}` | multiplied by 100 first, then 1 decimal |
| `{mean:.3s}` | 3 significant digits |
| `{n:d}` | integer |
| `{n:stat}` | the `stat` column, [`as.character()`](https://rdrr.io/r/base/character.html), untouched |
| `{mean:stat_fmt}` | the `stat_fmt` column, demanded |

An ARD carries two values per statistic and both are reachable: a token
the two specs that reach them are spelled as the ARD spells them, so
there is no mapping to learn. `:stat_fmt` takes the string cards
formatted and **errors** when this ARD has none, because that is a
demand; `:stat` takes the value untouched, character statistics such as
`method` included; any other spec formats `stat` here. A **bare** token
is the forgiving one — it prefers `stat_fmt` and falls back to `stat` —
since `stat_fmt` is optional and an ARD built without `fmt_fun` would
otherwise produce nothing. The two differ for a proportion: cards writes
`61.6` into `stat_fmt` while `stat` holds `0.616`, so `{p}` and
`{p:.1f\%}` agree and `{p:.1f}` does not. For `stats = "rows"`, where a
value goes into the cell without a template, `widen_ard(value = )` makes
the same choice. A template whose statistics are not all present yields
`NA`, which is what lets `c("{n} ({p})", "{n}")` act as a fallback
chain. A **named** vector of templates produces one table row per
element, the name being the row label: that is how `Min` and `Max`
become a single `Min, Max` line.

## How a `cells` entry is chosen (and why it survives a cards upgrade)

`context` is a cards implementation detail and it moves.
[`ard_continuous()`](https://pharmaverse.github.io/cards/latest-tag/reference/deprecated.html)
stamps `"continuous"`, but its 0.9 rename
[`ard_summary()`](https://pharmaverse.github.io/cards/latest-tag/reference/ard_summary.html)
stamps `"summary"`;
[`ard_categorical()`](https://pharmaverse.github.io/cards/latest-tag/reference/deprecated.html)
stamps `"categorical"`, but
[`ard_tabulate()`](https://pharmaverse.github.io/cards/latest-tag/reference/ard_tabulate.html)
stamps `"tabulate"`. Keying `cells` on the context alone would tie your
script to one cards generation and produce **no cells at all** against
another.

So every summary – a variable under one context – is also classified
from what its rows actually contain, its **kind**:

- `"categorical"`:

  the summary has levels to enumerate (some `variable_level` is
  present): a factor, a dichotomous value of interest, a hierarchy term.

- `"continuous"`:

  it does not – one row per statistic of one numeric variable.

A variable both summarised and tabulated in one ARD is two summaries,
and gets one kind for each. That reading is structural, so it is the
same on every cards version, past and future. A `cells` entry is then
matched in this order:

1.  the analysis variable's own name;

2.  `context`, with the known spellings treated as equivalent;

3.  the kind, likewise;

4.  `"default"`.

Step 2 lets a context-specific entry win where you wrote one – cardx's
`"proportion_ci"`, `"survival"`, `"stats_t_test"` and friends. Step 3 is
what keeps `continuous` / `categorical` (or `summary` / `tabulate`,
either spelling) working when cards renames a verb again.

[`list_ard_keys()`](https://ichirio.github.io/rtfreporter/reference/list_ard_keys.md)
prints both: the contexts, which are version-specific, and the kinds,
which are not.

## Where a value lives depends on how the ARD was built

Two things a table needs have no fixed home in an ARD, because cards
offers more than one way to produce them. Neither is guessed:

- the overall row:

  `ard_stack_hierarchical(over_variables = TRUE)` writes an "any event"
  row as the sentinel variable `..ard_hierarchical_overall..`; summarise
  each level separately and bind the results and there is no sentinel –
  that block counts the treatment itself, so the arm sits in `variable`
  / `variable_level`.
  [`overall_row()`](https://ichirio.github.io/rtfreporter/reference/overall_row.md)
  says which.

- the denominator:

  in one ARD `stat_name == "N"` is the per-arm denominator on the
  summary rows and the **study** total on the by variable's own rows,
  where the per-arm count is `n` instead; and an author may compute
  their own.
  [`pull_ard()`](https://ichirio.github.io/rtfreporter/reference/pull_ard.md)
  names the statistic and lists the candidates when the choice is
  ambiguous.

Column headers are rtfreporter's own job – `col_header` takes a plain
character vector – so
[`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)
builds the body only, and
[`pull_ard()`](https://ichirio.github.io/rtfreporter/reference/pull_ard.md)
is there when the header needs a number that must agree with the
percentages.

## See also

[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md),
[`stub_cols()`](https://ichirio.github.io/rtfreporter/reference/stub_cols.md)

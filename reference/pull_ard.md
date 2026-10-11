# Pull a statistic out of an ARD, keyed like the spread columns

An ARD carries more than the table's body: the denominator behind every
percentage, the subject count per arm, a total the author computed
themselves. Those belong in the **column header** (`Placebo\\nN = 86`)
or in an overall row, not in a body cell, so
[`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)
puts them nowhere. `pull_ard()` reads one out, keyed exactly like the
spread columns – `"Placebo____F"` for a crossed header – ready to paste
into a `col_header`.

## Usage

``` r
pull_ard(
  x,
  cols,
  stat = "N",
  variable = NULL,
  context = NULL,
  levels = NULL,
  sep = "____"
)
```

## Arguments

- x:

  A cards/cardx ARD.

- cols:

  The column key(s), named as in
  [`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)
  – the grouping variable's own name, not its `group*` position.

- stat:

  Statistic to read; `"N"` by default.

- variable, context:

  Restrict to this analysis variable and/or this `context`. Use them
  when the error message says the choice is ambiguous, or to ask for the
  key variable's own rows.

- levels:

  Optional level order for the keys, so the result lines up with the
  table's columns.

- sep:

  Separator between multiple `cols` keys; match
  [`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md).

## Value

A named vector, one element per column key.

## Details

Taking the number from the ARD rather than counting the data again is
the point: it is by construction the number the percentages used.

## Which N

There is no one place an ARD keeps "the N".
`cards::ard_stack(.by = TRT)` writes the per-arm count as `n` on the
by-variable's own rows and the **study** total as `N` on those same
rows, while every summary row carries its own denominator as `N`;
[`ard_total_n()`](https://pharmaverse.github.io/cards/latest-tag/reference/ard_total_n.html)
adds `..ard_total_n..`; and an author may compute their own, as a `bigN`
statistic in a solicited-AE table. Guessing between them produces a
plausible wrong number in a column header, so this function does not
guess:

- `stat` names the statistic, and defaults to `"N"`.

- The **key variables' own tabulations are excluded** by default,
  because there `N` is the study total rather than the column's
  denominator. Set `variable` to one of them to ask for those rows on
  purpose.

- If what is left still offers more than one answer, the call **fails
  with the candidates listed**, so you can pin it with `variable` /
  `context`.

## See also

[`list_ard_keys()`](https://ichirio.github.io/rtfreporter/reference/list_ard_keys.md),
which lists every statistic an ARD carries;
[`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)

## Examples

``` r
if (requireNamespace("cards", quietly = TRUE)) {
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  ard <- cards::ard_stack(adsl, .by = TRT,
                          cards::ard_summary(variables = AGE))
  n <- pull_ard(ard, cols = "TRT")            # the denominator, per arm
  paste0(names(n), "\\nN = ", n)
}
#> [1] "Placebo\\nN = 86"              "Xanomeline High Dose\\nN = 84"
#> [3] "Xanomeline Low Dose\\nN = 84" 
```

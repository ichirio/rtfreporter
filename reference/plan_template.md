# Write the plan for you

The counterpart of `plan_template(form = "widen")` for the deferred
form: reads an ARD and writes a runnable
[`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md)
pipeline, filled in with the keys, hierarchy, contexts and statistics it
actually found.

## Usage

``` r
plan_template(
  x,
  cols = NULL,
  hierarchy = character(),
  form = c("plan", "widen"),
  file = NULL,
  pipe = NULL
)
```

## Arguments

- x:

  A cards / cardx ARD (or anything
  [`normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.md)
  takes).

- cols:

  The column keys, as
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md)'s
  `cols`; `NULL` takes the first key the ARD has.

- hierarchy:

  The hierarchy columns, as
  [`normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.md)'s.

- form:

  `"plan"` (default) writes the deferred form, a
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md)
  pipeline; `"widen"` writes the immediate form,
  [`normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.md)
  into
  [`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)
  with the seam left open for a
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).

- file:

  When given, the code is also written there.

- pipe:

  `"|>"` or `"%>%"`; `NULL` follows `getOption("rtfreporter.ard_pipe")`,
  then RStudio's own preference, then `%>%`.

## Value

The generated code, a character vector of class
`"rtfreporter_plan_code"`, one element per line. Its print method shows
the code, so typing the call prints it and assigning the result prints
nothing.

## Details

It writes **both halves** — the ARD to the table, and the table to the
RTF pages — because a plan that stops at the table is a plan that made
you look up the `stub` and the header somewhere else. The display half
is a starting point and is meant to be edited: only
[`plan_stub()`](https://ichirio.github.io/rtfreporter/reference/plan_stub.md)
is derivable from the ARD, and the rest are display decisions nothing
can guess.

## See also

[`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md),
[`plan_apply()`](https://ichirio.github.io/rtfreporter/reference/plan_apply.md)

## Examples

``` r
if (requireNamespace("cards", quietly = TRUE)) {
  ard <- cards::ard_stack(
    cards::ADSL, .by = ARM,
    cards::ard_summary(variables = AGE),
    cards::ard_tabulate(variables = SEX))
  plan_template(ard, cols = "ARM")          # prints the code
  code <- plan_template(ard, cols = "ARM")  # prints nothing
}
```

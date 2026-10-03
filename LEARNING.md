# Learning Notes — class systems in rtfreporter

`rtfreporter` is **entirely S3**. Nothing in `R/` defines an R6,
Reference or S4 class.

That is worth a document because it was not the starting point: earlier
versions used R6 widely without justification, then narrowed it to a
single “this is where R6 truly shines” class — and in the end that one
did not survive either. The reasoning is recorded here so the invariant
is a conclusion somebody can check, rather than a rule nobody remembers
the reason for.

The rule of thumb:

> **Default to S3. Reach for reference semantics only when they
> genuinely change what the API can do — and be honest about whether the
> promise they make is one the code actually keeps.**

------------------------------------------------------------------------

## Summary table

| Concept | Class system | File | Notes |
|----|----|----|----|
| `rtf_border_side` | **S3** (tagged list) | [`R/rtf_border.R`](https://ichirio.github.io/rtfreporter/R/rtf_border.R) | Tiny immutable value object (style + width + colour). |
| `rtf_border` | **S3** (tagged list) | [`R/rtf_border.R`](https://ichirio.github.io/rtfreporter/R/rtf_border.R) | Four-edge spec; the one border constructor. |
| `rtf_table_style` | **S3** (tagged list) | [`R/rtf_table_style.R`](https://ichirio.github.io/rtfreporter/R/rtf_table_style.R) | Bundle of table defaults; derive variants with [`rtf_table_style_with()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style_with.md). Snapshot semantics. |
| `rtfreport` | **S3** (internal tagged list) | [`R/rtfreport.R`](https://ichirio.github.io/rtfreporter/R/rtfreport.R) | Internal scaffold built by the pipe adapter; the renderer consumes it. |
| `rtftable` | **S3** (tagged list) | [`R/rtftable.R`](https://ichirio.github.io/rtfreporter/R/rtftable.R) | Public content record built by [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md). |
| `rtfplot` | **S3** (tagged list) | [`R/rtfplot.R`](https://ichirio.github.io/rtfreporter/R/rtfplot.R) | Public content record built by [`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md). |
| `rtf_page`, `rtf_sect` | **S3** (tagged lists) | [`R/rtfreport.R`](https://ichirio.github.io/rtfreporter/R/rtfreport.R) | Data records sitting inside `rtfreport$pages` / `$sections`. |
| `rtf_document` (pipe API) | **S3** | [`R/pipe-composition.R`](https://ichirio.github.io/rtfreporter/R/pipe-composition.R) | Immutable functional composition — each pipe step returns a fresh copy. |
| `rtf_col_header`, `rtf_col_cell` | **S3** (tagged lists) | [`R/col_header.R`](https://ichirio.github.io/rtfreporter/R/col_header.R) | Header rows and cells; `col_cell(pos = )` may hold a selector function. |
| `rtf_stub_spec` | **S3** (tagged list) | [`R/stub_spec.R`](https://ichirio.github.io/rtfreporter/R/stub_spec.R) | The row-stub settings, as one object. |
| `rtf_blank_rows_by_change` / `_by_rule` | **S3** (tagged lists) | [`R/blank_rows.R`](https://ichirio.github.io/rtfreporter/R/blank_rows.R) | Tiny specification records. |
| `rtf_auto_section_item` | **S3** (tagged list) | [`R/pipe-composition.R`](https://ichirio.github.io/rtfreporter/R/pipe-composition.R) | A render-time sentinel; pure data. |

------------------------------------------------------------------------

## Why S3 (everywhere)

S3 is R-idiomatic, lightweight, and has properties that matter for a
data-manipulation / reporting package:

- **[`dput()`](https://rdrr.io/r/base/dput.html) /
  [`str()`](https://rdrr.io/r/utils/str.html) /
  [`print()`](https://rdrr.io/r/base/print.html) show real content.**
  Debugging is easy and serialization
  ([`saveRDS()`](https://rdrr.io/r/base/readRDS.html) /
  [`readRDS()`](https://rdrr.io/r/base/readRDS.html)) round-trips
  cleanly.
- **Pure values compose with a pipe.** Each step returns a new copy and
  reasoning is purely functional.
- **Copy-on-modify is the default.** A border or style handed to many
  tables can never be mutated through a back door.
- **No extra dependency.** S3 is part of base R, so it costs nothing in
  `Imports:` — which matters here, where `Imports:` is deliberately
  limited to packages that ship with R.

------------------------------------------------------------------------

## Why R6 was removed

Earlier versions used R6 for `rtfreport_r6`, `rtftable_r6`,
`rtfplot_r6`, `rtf_border` and `rtf_table_style`. Each was revisited and
found to be paying complexity without delivering anything in return.

### 1. `rtfreport_r6` / `rtftable_r6` / `rtfplot_r6` — short-lived scaffolds

These existed only inside `.pipe_doc_to_rtfreport()` and the renderer;
users never held one. Construction did a few mutations (`add_page()`,
`add_section()`) and then the object was read once. An S3 list with
functional helpers (`.rtfreport_add_page(rep, ...)` returning a new
copy) does the same job in fewer lines, with the bonus that the result
is [`dput()`](https://rdrr.io/r/base/dput.html)-able and serializable.

### 2. `rtf_border` (R6) — chained builders that nobody used

The R6 implementation exposed `$set_top()`, `$with_top()`,
`$apply_override()`, `$override()` and so on. Outside one internal call
site (`generate_rtfreport.R`’s spanning-row border resolution) and the
package’s own tests, none of them were used. That single internal site
was a one-liner which became
`.merge_rtf_border(eff, rtf_border(bottom = ...))` under the S3 design —
simpler, not harder.

### 3. `rtf_table_style` (R6) — the “shared mutable theme” that wasn’t

This was billed as the canonical R6 win: define a theme once, hand the
same instance to many tables, mutate it, watch every table reflect the
change.

In practice it only worked for nested `rtf_border` mutations that
happened to be passed through `as_table_border()` unchanged. Every
scalar field (`header_bold`, `header_align`, `cell_padding_*`, …) was
**snapshotted by the rtftable constructor at build time**, so mutating
the style afterwards was silently ignored. The promised semantics were
not the delivered ones, and the gap was invisible: nothing errored, the
change simply did not appear.

The S3 model — build the style, hand it to tables, derive variants with
[`rtf_table_style_with()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style_with.md)
— is honest about what actually happens.

------------------------------------------------------------------------

## …and then the last R6 class went too

For a while this document ended differently. It described one surviving
R6 class, `rtf_theme`, as the deliberate counter-example: a theme
*should* be shared, mutated after construction, and observed by the
renderer, and S3 cannot do that without rebuilding every table or
threading the theme through every render call.

The argument was sound. The feature still did not survive, and the
reason is the more useful lesson:

- Making it work needed a render-time hook (`.refresh_theme()`) that
  re-snapshotted the theme before delegating to the normal renderer — a
  second path through rendering, for one class.
- It made R6 a dependency of one narrow feature, against an otherwise
  dependency-free package, so the feature had to degrade gracefully when
  R6 was absent — more branching, for one class.
- And it was solving a problem the package did not actually have. A
  clinical deliverable is generated by a script that runs once, top to
  bottom. Nobody builds fifty tables, then retunes a theme, then
  re-renders *in the same session*: they edit the script and run it
  again. The “mutate and observe” win was real in the abstract and worth
  nothing in the workflow.

So `rtf_theme` was deleted (the R6 path went in v0.0.41) and the package
became uniformly S3. What is left is
[`rtf_table_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style.md),
which does the useful half — one record of defaults, handed to many
tables — with honest snapshot semantics.

**The lesson is not “R6 is bad”.** It is that a design argument can be
correct in general and still lose on the particular workflow, and that a
class system earns its place by what the code around it needs, not by
what it would enable in principle.

------------------------------------------------------------------------

## What this means for users

- Every public object is a plain S3 list. `inherits(x, "rtf_border")`,
  field access via `x$top` / `x[["top"]]`, and `unclass(x)` all work as
  expected.
- Build a border with
  [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md),
  and one edge with
  [`rtf_border_side()`](https://ichirio.github.io/rtfreporter/reference/rtf_border_side.md).
  The per-shape constructors
  ([`rtf_border_top()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md),
  [`rtf_border_box()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md),
  [`rtf_border_with()`](https://ichirio.github.io/rtfreporter/reference/rtf_border_with.md),
  …) are **deprecated** and scheduled for removal before CRAN.
- Derive a style from another with
  `rtf_table_style_with(s, header_bold = TRUE)`.
- There is no shared-mutable theme. To give many tables the same look,
  build one
  [`rtf_table_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style.md)
  and pass it to each; to change the look, change the script and run it
  again.

------------------------------------------------------------------------

For the architecture these classes sit in — the object model, the
rendering pipeline and the
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
hub — see the [Architecture &
internals](https://ichirio.github.io/rtfreporter/articles/architecture.html)
article.

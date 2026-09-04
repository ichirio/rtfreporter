# Learning Notes — class systems in rtfreporter

`rtfreporter` is almost entirely S3. There is exactly **one** R6 class
(`rtf_theme`), kept as a deliberate example of the one place R6 truly
shines. This document records the design reasoning, because earlier
versions of the codebase used R6 widely without justification and the
path to the current shape is itself instructive.

The rule of thumb:

> **Default to S3. Reach for R6 only when reference semantics genuinely
> change what the API can do — and document the choice next to the class
> definition.**

In this package S3 covers every value-like object (borders, styles,
tables, figures, documents, etc.). R6 is used in exactly one place,
`rtf_theme()`, because — and only because — multiple
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
objects need to share a *mutable* defaults record and observe each
other’s changes at render time. See “The one R6 class” below for the
full story.

------------------------------------------------------------------------

## Summary table

| Concept | Class system | File | Notes |
|----|----|----|----|
| `rtf_border_side` | **S3** (tagged list) | [`R/rtf_border.R`](https://ichirio.github.io/rtfreporter/R/rtf_border.R) | Tiny immutable value object (style + width + colour). |
| `rtf_border` | **S3** (tagged list) | [`R/rtf_border.R`](https://ichirio.github.io/rtfreporter/R/rtf_border.R) | Four-edge spec; derive variants with [`rtf_border_with()`](https://ichirio.github.io/rtfreporter/reference/rtf_border_with.md). |
| `rtf_table_border` | **S3** (tagged list) | [`R/rtf_border.R`](https://ichirio.github.io/rtfreporter/R/rtf_border.R) | Passive grouping of `rtf_border`s by zone. |
| `rtf_table_style` | **S3** (tagged list) | [`R/rtf_table_style.R`](https://ichirio.github.io/rtfreporter/R/rtf_table_style.R) | Bundle of table defaults; derive variants with [`rtf_table_style_with()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style_with.md). Snapshot semantics. |
| `rtf_theme` | **R6** (optional) | [`R/rtf_theme.R`](https://ichirio.github.io/rtfreporter/R/rtf_theme.R) | The one R6 class. Shared *mutable* theme: mutate once → every referencing table reflects the change at the next render. |
| `rtfreport` | **S3** (internal tagged list) | [`R/rtfreport.R`](https://ichirio.github.io/rtfreporter/R/rtfreport.R) | Internal scaffold built by the pipe adapter; the renderer consumes it. |
| `rtftable` | **S3** (tagged list) | [`R/rtftable.R`](https://ichirio.github.io/rtfreporter/R/rtftable.R) | Public content record built by [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md). |
| `rtfplot` | **S3** (tagged list) | [`R/rtfplot.R`](https://ichirio.github.io/rtfreporter/R/rtfplot.R) | Public content record built by [`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md). |
| `rtf_page`, `rtf_sect` | **S3** (tagged lists) | [`R/rtfreport.R`](https://ichirio.github.io/rtfreporter/R/rtfreport.R) | Data records sitting inside `rtfreport$pages` / `$sections`. |
| `rtf_document` (pipe API) | **S3** | [`R/pipe-composition.R`](https://ichirio.github.io/rtfreporter/R/pipe-composition.R) | Immutable functional composition — `%>%` returns a fresh copy each step. |
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
  ([`saveRDS()`](https://rdrr.io/r/base/readRDS.html) / `loadRDS()`)
  round-trips cleanly.
- **Pure values compose with `%>%`.** Each pipe step returns a new copy
  and reasoning is purely functional.
- **Copy-on-modify is the default.** A border or style handed to many
  tables can never be mutated through a back door.
- **No extra dependency.** S3 is part of base R; there is no `Imports:`
  cost.

------------------------------------------------------------------------

## Why R6 was removed

Earlier versions used R6 for `rtfreport_r6`, `rtftable_r6`,
`rtfplot_r6`, `rtf_border`, and `rtf_table_style`. Each of those choices
was revisited and found to be paying complexity without delivering
anything in return.

### 1. `rtfreport_r6` / `rtftable_r6` / `rtfplot_r6` — short-lived scaffolds

These existed only inside `.pipe_doc_to_rtfreport()` and the renderer;
users never held one. Construction did a few mutations (`add_page()`,
`add_section()`) and then the object was read once. An S3 list with
functional helpers (`.rtfreport_add_page(rep, ...)` returning a new
copy) does the same job in fewer lines, with the bonus that the result
is [`dput()`](https://rdrr.io/r/base/dput.html)-able and serializable.

### 2. `rtf_border` (R6) — chained builders that nobody used

The R6 implementation exposed `$set_top()`, `$with_top()`,
`$apply_override()`, `$override()` etc. Outside one internal call site
(`generate_rtfreport.R`’s spanning-row border resolution) and the
package’s own tests, none of these were used. The single internal site
was a one-liner that became
`.merge_rtf_border(eff, rtf_border(bottom = ...))` under the S3 design —
simpler, not harder. Users who do want non-mutating derivation now call
`rtf_border_with(b, top = ...)`.

### 3. `rtf_table_style` (R6) — the “shared mutable theme” that wasn’t

This was billed as the canonical R6 win: define a theme once, hand the
same instance to many tables, mutate it, watch every table reflect the
change. In practice this only worked for nested `rtf_border` mutations
that happened to be passed through `as_table_border()` unchanged. Every
scalar field (`header_bold`, `header_align`, `cell_padding_*`, …) was
**snapshotted by the rtftable constructor at build time**, so mutating
the style after construction was silently ignored. The promised
semantics were inconsistent; the simpler S3 model (build the style, hand
it to tables, derive variants with
[`rtf_table_style_with()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style_with.md))
is honest about what actually happens.

------------------------------------------------------------------------

## The one R6 class: `rtf_theme`

`rtf_theme` is the only R6 object in `rtfreporter`. It is kept as a
focused, well-justified counter-example to the “everything S3” default.

### What makes it the right tool here

The classic snapshot pattern — `rtftable(df, style = my_style)` — bakes
the style fields into the rtftable at construction. Mutating `my_style`
afterwards does not affect tables that already snapshotted it. That is
the correct, predictable S3 behaviour.

A *theme*, by contrast, is meant to be:

- shared by many tables that should all look the same,
- mutated after construction to retune the look, and
- observed by the renderer so that the next render reflects the change.

`rtf_theme` is an R6 class whose instances are stored on each
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
by **reference**. At render time `.refresh_theme(tbl)` re-snapshots the
theme’s *current* state and applies it before delegating to the regular
renderer. S3 cannot do this without either (a) rebuilding every table
after each mutation, or (b) passing the theme through every render call
— both clumsy.

``` r

theme <- rtf_theme(header_bold = FALSE)
t1 <- rtftable(df1, theme = theme)
t2 <- rtftable(df2, theme = theme)

# Render — both tables have non-bold headers.

theme$header_bold <- TRUE

# Render again — both tables now show bold headers, no rebuild required.
```

Explicit
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
arguments (e.g. `col_header_align = "right"`) still beat the theme
defaults, so users get the best of both worlds: opinionated central
defaults plus per-table overrides.

### Why it is an *optional* dependency

The R6 package is declared in `Suggests:`, not `Imports:`. `rtf_theme()`
gracefully errors with installation instructions if R6 is missing. All
other rtfreporter features — including the snapshot-style
`rtf_table_style` — work without R6 being installed. Users who do not
want a runtime dependency on R6 simply ignore `rtf_theme()`; users who
want shared mutable themes opt in with `install.packages("R6")`.

------------------------------------------------------------------------

## What this means for users

- All public objects except `rtf_theme()` are plain S3 lists.
  `inherits(x, "rtf_border")`, field access via `x$top` / `x[["top"]]`,
  and `unclass(x)` all work as expected.
- To derive a border from another, use
  `rtf_border_with(b, bottom = ...)`.
- To derive a style from another, use
  `rtf_table_style_with(s, header_bold = TRUE)`.
- If you want *broadcast* updates across many tables, attach a shared
  `rtf_theme()` instead of
  [`rtf_table_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style.md).
  Mutate the theme in place and the change shows up in every referencing
  table on the next render.

See `vignette("class-systems", package = "rtfreporter")` for a hands-on
walk-through of the S3 vs R6 design choices in this package.

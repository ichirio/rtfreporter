# Changelog

All notable changes to rtfreporter are documented in this file. Changes
are recorded for **major.minor** version releases only (v0.1.0, v0.2.0,
etc.). Patch and development versions (v0.1.1, v0.0.6, v0.0.dev, etc.)
are not recorded.

------------------------------------------------------------------------

## v0.4.0

> **Status**: Published as the current stable version — a MINOR feature
> release rolling up all development since v0.3.4. Because this is a
> general feature release (not the CRAN-prep milestone), the roadmap
> shifts up by one MINOR: **v0.5.0** CRAN-submission preparation,
> **v0.6.0** CRAN registration, **v1.0.0** stable API
> (`lifecycle: experimental` removed).

Highlights since v0.3.4:

- New table-object adapters —
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
  /
  [`as_rtftable()`](https://ichirio.github.io/rtfreporter/reference/as_rtftable.md)
  now read **flextable** and **huxtable** tables (joining gt, gtsummary,
  rtables/tern).
- `group_by` group-detection modes (`"auto"` / `"indent"` / `"value"` /
  `"filled"`), decoupled from `group_col`.
- `collapse_repeats` — per-page repeat suppression (blank consecutive
  repeated values, hierarchically across columns).
- `blank_row_normalize` — render all-empty data rows as a single
  full-width blank row and collapse consecutive blanks (default on).
- Document-wide style defaults (row height / cell padding) and
  configurable `rtfreporter.*` options.
- Titles / footnotes render as content-width tables with per-row
  styling.
- Page-setup presets (`paper_size`) and rationalized page geometry.
- Bug fix — the multi-page `\page` break is now wrapped in empty
  paragraphs (`{\pard\fs2\par}\page{\pard\fs2\par}`), so Word honours
  it.
- Testing — multi-group / multi-page pagination coverage for every
  adapter.

See [`NEWS.md`](https://ichirio.github.io/rtfreporter/NEWS.md) for the
full per-item list.

------------------------------------------------------------------------

## v0.3.4

> **Status**: Published as the current stable version. This is an
> **exceptional patch-level release** — it pins the development line
> (which had inadvertently advanced to 0.3.x) as a stable reference
> point. Planned milestones from here: **v0.4.0** CRAN-submission
> preparation, **v0.5.0** CRAN registration, **v1.0.0** stable API
> (`lifecycle: experimental` removed).

Rolls up all development since v0.1.0. Highlights:

- Fine-grained column-header borders — per-cell `col_cell(border = )`
  and `rtf_border_side("none")` (explicit no-line) for adding/removing
  individual header rules; new “Borders and rules” article.
- More robust RTF output — `\headery` / `\footery` coordinated with the
  margins, and a hardened preamble (`\ansicpg1252`, `\deflang1033`,
  `\uc1`, `\fcharset0`, `\widowctrl`).
- Bug fix — `align_count_pct = TRUE` no longer over-formats integer-only
  columns.
- Internal cleanup — removed long-deprecated no-op `rtf_*_format()`
  functions and the thin constructor wrapper layer; pure S3 throughout.
- Documentation — custom `cell_format` contract, exhaustive
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
  help, and a concrete contributor backlog/tracking scheme.

See [`NEWS.md`](https://ichirio.github.io/rtfreporter/NEWS.md) for the
full per-item list.

------------------------------------------------------------------------

## v0.1.0

> **Status**: First public GitHub release. CRAN submission is deferred
> to v0.2.0.

### ✨ Features (v0.0.38–v0.0.45) — gt, gtsummary & rtables/tern integration

rtfreporter can now build RTF tables directly from
[gt](https://gt.rstudio.com),
[gtsummary](https://www.danieldsjoberg.com/gtsummary/), (via gt)
[tfrmt](https://gsk-biostatistics.github.io/tfrmt/), and the
[rtables](https://insightsengineering.github.io/rtables/) /
[tern](https://insightsengineering.github.io/tern/) family.

- **rtables / tern** (v0.0.45): any `VTableTree` is read through
  [`formatters::matrix_form()`](https://rdrr.io/pkg/formatters/man/matrix_form.html)
  – leaf + spanning column headers, per-column alignment, the row-label
  stub with indentation, titles / subtitles, the main / provenance
  footers and referential footnote texts, and in-cell `{N}` marks
  rewritten to `^{N}`. `formatters`, `rtables`, `tern` added to
  `Suggests`.

- **[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)**
  is the single entry point: it reads a table object’s metadata **and**
  paginates the body, returning a list of ready-to-render `rtftable`
  objects (one per page). gtsummary tables are converted to `gt_tbl`
  automatically via
  [`gtsummary::as_gt()`](https://www.danieldsjoberg.com/gtsummary/reference/as_gt.html).
  [`as_rtftable()`](https://ichirio.github.io/rtfreporter/reference/as_rtftable.md)
  (singular) is the one-page convenience wrapper.

- Metadata read from a gt object: column labels, per-column alignment,
  multi-level spanning headers, column widths, hidden-column removal,
  stub / row-group rows, title + subtitle, source notes, table
  footnotes, **per-cell bold / italic / underline / indent**
  (`tab_style(cell_text(...))`), and **in-cell footnote marks** rendered
  as `^{N}` superscripts.

- [`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
  reads the `rtf_titles` / `rtf_footnotes` attributes that
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
  puts on each page, so titles / footnotes flow through with no extra
  flag.

- New `rtftable(cell_styles = )` field for per-cell formatting.

- **Deprecated**:
  [`paginate()`](https://ichirio.github.io/rtfreporter/reference/paginate.md)
  (use
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md));
  `rtf_tables(read_gt = )` is now a legacy direct-gt path.

- **Packaging**: source directory renamed `r/` → `R/`; `R CMD check`
  passes with 0 errors / 0 warnings.

### 🔴 Breaking Changes (v0.0.19) — R6 removed; full S3 architecture

Every internal R6 class has been replaced with a plain S3 list and the
`Imports: R6` dependency dropped. Rationale: the R6 features the package
used (in-place mutation, chained builders, shared mutable themes) were
either snapshotted away at construction time or used at exactly one
internal call site — none of them survived to the rendered output. The
S3 design is simpler, [`dput()`](https://rdrr.io/r/base/dput.html)-able,
and serialises cleanly with
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html).

User-facing class names:

| Before (R6)            | After (S3)                                    |
|------------------------|-----------------------------------------------|
| `rtftable_r6`          | `rtftable`                                    |
| `rtfplot_r6`           | `rtfplot`                                     |
| `rtfreport_r6`         | `rtfreport`                                   |
| `rtf_border` (R6)      | `rtf_border` (S3 — class name unchanged)      |
| `rtf_table_style` (R6) | `rtf_table_style` (S3 — class name unchanged) |

API surface changes:

- Method chains (`b$set_top()`, `b$with_top()`, `b$apply_override()`,
  `b$override()`) → removed. Use `rtf_border_with(b, top = ...)` for
  non-mutating derivation.
- `style$clone_with(...)` → replaced by
  `rtf_table_style_with(style, ...)`.
- `rtf_table_style$new(...)` → call `rtf_table_style(...)` as a
  function.
- `rtftable_r6$new(...)` / `rtfplot_r6$new(...)` → call
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  /
  [`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md)
  (public wrappers were already in place).
- The internal pipe-adapter renamed: `.pipe_doc_to_r6_report()` →
  `.pipe_doc_to_rtfreport()`.
- `inherits(x, "rtftable_r6")` / `inherits(x, "rtfplot_r6")` /
  `inherits(x, "rtfreport_r6")` → use `"rtftable"` / `"rtfplot"` /
  `"rtfreport"`.
- The “share a style; mutate it; every existing table reflects the
  change” behaviour is gone — it never worked consistently anyway. Build
  a new style with
  [`rtf_table_style_with()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style_with.md)
  and hand it to new tables.

`LEARNING.md` rewritten to describe the unified S3 design and the
reasoning behind retiring R6.

### 🔴 Breaking Changes (v0.0.18) — data section has no default borders

The
[`rtf_border_tfl()`](https://ichirio.github.io/rtfreporter/reference/rtf_border_tfl.md)
preset previously set `last_row = rtf_border(bottom = ...)`, which drew
a horizontal line under the last data row (and, with
`blank_rows = c(-1)`, the line appeared *above* the trailing blank row).
Per the locked design contract that **default borders belong to the
column-header block only**, the preset is now:

``` r

rtf_table_border(
  header   = rtf_border(top = s, bottom = s),   # outer frame of header block
  spanning = NULL,                               # falls back to header
  body     = NULL,
  first_row = NULL,
  last_row  = NULL                               # was: rtf_border(bottom = s)
)
```

- The column-header block keeps its outer frame: top border on the
  topmost header row, bottom border on the bottommost.

- Multi-column spanning cells still auto-receive a bottom border (group
  underline) when they are not the last header row.

- **No horizontal or vertical lines anywhere in the data section by
  default.** Trailing blank rows from `blank_rows = c(-1)` no longer
  acquire a phantom line above them.

- Callers who relied on the old TFL look can opt back in explicitly:

  ``` r

  border = rtf_table_border(
    header   = rtf_border(top = rtf_border_side(), bottom = rtf_border_side()),
    last_row = rtf_border(bottom = rtf_border_side())
  )
  ```

[`rtf_table_style_tfl()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style_tfl.md)
is updated in lockstep: its `border_last_row` field is now left at
`NULL`.

### 📚 Documentation (v0.0.17)

#### Quickstart vignette: new sections for v0.0.11–v0.0.16 features

Three new numbered sections added to `rtfreporter-quickstart.Rmd`:

- **§14 Blank separator rows** — demonstrates all three `blank_rows`
  specification modes (integer positions,
  [`blank_rows_by_change()`](https://ichirio.github.io/rtfreporter/reference/blank_rows_by_change.md),
  [`blank_rows_by_rule()`](https://ichirio.github.io/rtfreporter/reference/blank_rows_by_rule.md)),
  their combination via [`list()`](https://rdrr.io/r/base/list.html),
  and the `read_attributes = TRUE` data-frame attribute fallback.
- **§15 Titles & footnotes per page** — shows the
  `rtf_tables(..., titles = ..., footnotes = ...)` flow, the standalone
  [`rtf_titles()`](https://ichirio.github.io/rtfreporter/reference/rtf_titles.md)
  /
  [`rtf_footnotes()`](https://ichirio.github.io/rtfreporter/reference/rtf_footnotes.md)
  setters, and the current text-paragraph rendering (centred-bold
  titles, divider-line footnotes).
- **§16 Shared table style (R6 theme)** — minimal example of
  [`rtf_table_style_tfl()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style_tfl.md)
  shared across many tables, plus the `style$clone_with(...)`
  non-mutating derivation. Cross-references
  [LEARNING.md](https://ichirio.github.io/rtfreporter/LEARNING.md).

The Quick-reference table at the bottom is expanded to include the
parameters added during the v0.0.11–v0.0.16 arc: `col_header_align`,
`read_attributes`, `style`, plus the `titles` / `footnotes` pipe-API
extras on
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
/
[`rtf_figures()`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.md).

#### All vignette / article HTML re-rendered

- `inst/doc/rtfreporter-pipes.html`,
  `inst/doc/rtfreporter-quickstart.html` rebuilt via
  `devtools::build_vignettes()`.
- `vignettes/articles/external-api.html`, `internal-design.html`
  re-rendered via Quarto.

### 🔴 Breaking Changes (v0.0.16) — column-header border defaults

The `border$header` zone now describes the **outer frame** of the
column-header block, not “every header row” individually. The default
behaviour for the column-header (the topmost block of the table,
including the legacy standalone `spanning_header` argument and every
`col_header` row) is:

| Rule | Effect |
|----|----|
| **Top border** | Applied to the **topmost** header row only. |
| **Bottom border** | Applied to the **bottommost** header row only. |
| **Multi-col spanning** | Cells that cover more than one column get an extra **bottom** border separating them from the more granular row below — the typical “underline the group label” look. |
| **Vertical borders** | None by default. |
| **Override** | `col_spec[[j]]$border` still wins on the column-header row for that single column; explicit `border = ...` argument still flows through. |

With the new semantics, a two-row header looks like:

    ─────────────────────────  (top border on row 1)
    │      │ Drug A  │ Drug B │
    │      ────────── ─────── (bottom under each multi-col span)
    │ Item │ N │ Mean │ N │ Mean │
    ─────────────────────────  (bottom border on row 2 — last header row)

The
[`rtf_border_tfl()`](https://ichirio.github.io/rtfreporter/reference/rtf_border_tfl.md)
preset is unchanged in shape but its meaning shifts under the new
outer-frame interpretation. Existing user code that relied on “border on
every header row” needs to migrate to per-column overrides via
`col_spec[[j]]$border`.

### 🐛 Bug fix (v0.0.16) — last-row border on single-row data

When the data frame had exactly one row, neither `border$first_row` nor
`border$last_row` was applied — the only row took only the `first_row`
branch of an `if/else if` chain that bypassed `last_row`. In the
standard TFL preset (`first_row = NULL`,
`last_row = rtf_border(bottom = ...)`), the table therefore had no
bottom border on its single data row. Both overrides are now merged
correctly when `nrows == 1`.

`.effective_row_border()` itself also stopped silently dropping the
override when the base border was NULL — `merge(NULL, X)` now returns
`X` instead of `NULL`.

### 🔴 Breaking Changes (v0.0.15)

#### Spanning rows: bold default flipped to FALSE; new optional `bold`, `italic`

Spanning-header cells were rendered in **bold** unconditionally. They
now default to **normal** weight, matching the policy already adopted
for column-header rows in v0.0.11. Three new optional per-cell fields
let callers opt in:

| Field       | Default | Effect                                             |
|-------------|---------|----------------------------------------------------|
| `bold`      | `FALSE` | Wrap label in `\b ... \b0`.                        |
| `italic`    | `FALSE` | Wrap label in `\i ... \i0`.                        |
| `underline` | `FALSE` | Wrap label in `\ul ... \ulnone` (already existed). |

``` r

spanning_header = list(
  list(from = 2, to = 3, label = "Drug A", underline = TRUE),               # plain weight, underlined
  list(from = 4, to = 5, label = "Drug B", underline = TRUE, bold = TRUE)   # bold + underlined
)
```

### 📋 Specification consolidation: column-header alignment

The cascade is unchanged from v0.0.12 but the contract is now pinned by
a dedicated test file
([tests/test-header-alignment.R](https://ichirio.github.io/rtfreporter/tests/test-header-alignment.R),
15 assertions):

1.  **Default**: column-header alignment follows the data column’s
    alignment (`col_spec[[j]]$align`).
2.  **Spanning row**: alignment defaults to the leftmost covered
    column’s resolved `header_align` (i.e. it inherits one level down
    the hierarchy).
3.  **Per-table override**: `col_header_align` (scalar or length-ncol
    character vector) sets the table-wide header alignment.
4.  **Per-column override**: `col_spec[[j]]$header_align` always wins.

### ✨ Features (v0.0.14)

#### Spanning rows inherit alignment from the level below

`spanning_header` cells previously rendered with a hard-coded `\qc`
(centre) alignment. They now inherit the alignment of the level
immediately below — i.e. the leftmost covered column’s `header_align`
(which itself cascades from `col_spec[[j]]$align`). Resolution order per
spanning cell:

1.  `sp$align` — explicit per-cell override.
2.  `col_spec[[sp$from]]$header_align` — inherit from the level below.
3.  `"center"` — final fallback.

``` r

# Numeric columns are right-aligned;
# the spanning labels above them now follow suit by default.
rtftable(df,
  col_header = c("Item", "N", "Mean", "N", "Mean"),
  col_spec   = list(
    list(col = 1, align = "left"),
    list(col = 2, align = "right"),
    list(col = 3, align = "right"),
    list(col = 4, align = "right"),
    list(col = 5, align = "right")
  ),
  spanning_header = list(
    list(from = 2, to = 3, label = "Drug A (N=30)", underline = TRUE),
    list(from = 4, to = 5, label = "Drug B (N=30)", underline = TRUE)
  ))
# → "Drug A (N=30)" and "Drug B (N=30)" are right-aligned.
```

`list(from, to, label, underline, align = "center")` still works for an
explicit override. Same rule applies to spanning rows declared inline
inside the `col_header` list.

### ✨ Features (v0.0.13 — Step C of the style/spec refactor)

#### `rtf_border` is now R6 (backward compatible)

The four-edge cell-border class is upgraded from an S3 list to an R6
object. The R6 instance still carries `class = "rtf_border"`, so
existing user code that does `inherits(x, "rtf_border")` and field
access via `x$top` / `x[["top"]]` continues to work.

The new R6 form adds chainable builder methods and explicit clone /
override semantics:

``` r

b <- rtf_border()$set_top(rtf_border_side())$set_bottom(rtf_border_side("double"))

b2 <- b$with_right(rtf_border_side("dot"))   # non-mutating clone

b$apply_override(other_border)               # mutating in place
b3 <- b$override(other_border)               # non-mutating clone
```

This is the one place in rtfreporter where reference semantics are
intentionally exposed to users — see
[LEARNING.md](https://ichirio.github.io/rtfreporter/LEARNING.md) for the
design rationale.

#### `rtf_table_style` — shared mutable theme (R6)

A new R6 class for **table-wide style templates that multiple tables
share by reference**. Define a “company TFL theme” once, hand the same
instance to many
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
calls, and tweak it later:

``` r

tfl <- rtf_table_style_tfl()                 # built-in TFL preset

tables <- lapply(dfs, function(df) rtftable(df, style = tfl))

# Build new tables after a tweak picks up the change
tfl$header_bold <- TRUE
```

`style$clone_with(field = value, ...)` returns a non-mutating
derivation. `style$as_table_border()` converts the zone borders into the
existing `rtf_table_border` form consumed by the renderer.

The `style` argument was added to
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
and the corresponding flow on
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md).
Explicit arguments always take precedence over the style’s defaults.

#### Per-column border in `col_spec`

`col_spec` entries gain a `border` slot
([`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
instance) that overrides the zone border for that column on the
column-header row. Useful for the clinical-TFL idiom of drawing a bottom
line under one group of columns only.

``` r

rtftable(df,
  border   = rtf_table_border(header = rtf_border(top = rtf_border_side(),
                                                    bottom = rtf_border_side())),
  col_spec = list(
    list(col = "Total",
         border = rtf_border(bottom = rtf_border_side("double", 20L)))
  ))
```

The renderer merges (zone border × column border) per cell so each
column header cell can have its own border profile.

#### LEARNING.md

A new top-level document,
[LEARNING.md](https://ichirio.github.io/rtfreporter/LEARNING.md),
explains the R6 vs S3 design decisions across the package. Useful for
the package’s secondary goal of being a small but realistic case study
in R object systems.

### ✨ Features (v0.0.12 — Step B of the style/spec refactor)

#### `blank_rows`: three combinable specification modes

The `blank_rows` argument now accepts three orthogonal specification
modes that can be freely combined inside a list (positions are unioned,
deduplicated, and sorted):

1.  **Integer positions** — `c(0, 5, -1)`. `0` is before the first data
    row, `k` is after row `k`, and `-1` means “after the last data row”.
    Out-of-range integers warn and are dropped.

2.  **By variable change** — `blank_rows_by_change(cols, ...)`. Inserts
    a blank separator whenever the value of any listed column differs
    from the previous row. `include_before_first` and
    `include_after_last` (both `TRUE` by default) add rows at the start
    and end.

3.  **By rule** — `blank_rows_by_rule(col, pattern, where)`. Inserts a
    blank row before or after every data row whose value in `col`
    matches the regular expression `pattern`.

``` r

rtftable(df, blank_rows = list(
  c(-1),                                            # after the last row
  blank_rows_by_change("Visit",
    include_before_first = FALSE,
    include_after_last   = FALSE),                  # at every Visit change
  blank_rows_by_rule("Parameter", "^Total", "before")
))
```

#### `read_attributes = TRUE`: data-frame attribute fallback

[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
(and the same path inside
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md))
now reads recognised attributes off the input data frame as fallback
defaults. The first attribute consumed is `attr(data, "rtf_blank_rows")`
— its numeric value seeds `blank_rows` when the argument is left `NULL`.
Set `read_attributes = FALSE` to suppress.

This is intentionally an extensible mechanism: future attributes
(`rtf_col_header_align`, `rtf_col_spec`, …) can be added without
changing call sites.

#### `col_header_align`: cascade rule for column-header alignment

[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
gains a `col_header_align` argument. The default `header_align` for
column headers now **inherits the data alignment**
(`col_spec[[j]]$align`) instead of the previous hard-coded `"center"`.

Resolution precedence (highest first):

1.  `col_spec[[j]]$header_align` (per-column override)
2.  `col_header_align[j]` (table-wide; scalar or length-ncol)
3.  `col_spec[[j]]$align` (inherit data alignment)

#### `col_header`: spanning rows mixed with label rows

`col_header` can now be a list whose elements are either:

- character vectors — regular label rows, one entry per data column;
- spanning rows — `list(list(from, to, label, underline), ...)`, with
  the same structure that has always been accepted by `spanning_header`.

This makes multi-row column headers with spanning groups expressible
inline:

``` r

rtftable(df, col_header = list(
  # Row 1: spanning header
  list(
    list(from = 2, to = 3, label = "Drug A (N=30)", underline = TRUE),
    list(from = 4, to = 5, label = "Drug B (N=30)", underline = TRUE)
  ),
  # Row 2: regular labels
  c("Item", "N", "Mean", "N", "Mean")
))
```

The standalone `spanning_header` argument is retained for backward
compatibility.

### 🔴 Breaking Changes (v0.0.11)

#### Magic blank-row tokens removed; title / footnote return to text paragraphs

The short-lived `{HALF_BLANK_ROW}` / `{BLANK_ROW}` magic tokens
introduced in v0.0.9 are removed. Blank rows inside titles, footnotes,
and
[`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
/
[`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
rows are now expressed with the empty string `""` only, and there is no
half-height shortcut. (Per-row twips control is deferred to a later
release.)

Rendering changes:

- **Title** is back to ordinary RTF paragraphs (was a 1-column table in
  v0.0.9–v0.0.10). Each element of the character vector becomes one
  `\par`. `""` yields a blank paragraph at the document font size.
  `title = NULL` (the default) emits one blank centred paragraph; use
  `title = character(0)` to suppress the block entirely. The default
  alignment is centre and non-empty lines are bold.
- **Footnote** is now also a paragraph block (was a 1×1 table, then a
  N×1 table). Each element is one paragraph; the first paragraph carries
  the top border (`\brdrt\brdrs\brdrw15`) acting as the visual separator
  from the content above.
- **[`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
  /
  [`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)**
  rows no longer recognise magic tokens. `c(c = "")` is a normal-height
  empty row.

#### Column-header bold default flipped to `FALSE`

[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
previously rendered column headers in bold. The default is now normal
weight (`header_bold = FALSE`). Set it via `col_spec` per column to
restore bold.

#### Header / footer cells now share the content-table cell padding

Cells emitted by
[`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
/
[`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
were rendered without `\li` / `\ri` padding, while content-table cells
used 72 twips on both sides — so header text sat flush against the cell
border. v0.0.10 emits the same `\li{pad_l}\ri{pad_r}` for header and
footer cells (and for the half-blank row variant). Defaults come from
`inst/resources/rtfreporter_defaults.R`:

``` r

default_cell_padding_left_twips  = 72L
default_cell_padding_right_twips = 72L
```

[`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
and
[`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
gain `cell_padding_left_twips` / `cell_padding_right_twips` arguments
for per-block overrides (NULL = inherit from the resource file).

### ✨ Features

#### Title and footnote blocks: per-row rendering + magic blank-row tokens

Titles and footnotes are now rendered as N×1 RTF tables (one row per
element of the character vector). Two magic tokens control blank rows in
title, footnote,
[`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md),
and
[`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
row content:

| Token                          | Effect                                   |
|--------------------------------|------------------------------------------|
| `"{HALF_BLANK_ROW}"`           | Empty row at half the default row height |
| `"{BLANK_ROW}"` (or just `""`) | Empty row at the default row height      |

Default title behaviour changed: a page with `title = NULL` now emits
one `{HALF_BLANK_ROW}` automatically so there is always a small visual
gap between the page header and the content. Use `title = character(0)`
to suppress the title block entirely.

``` r

rtf_tables(doc, list(df1, df2),
  titles    = list(c("Table 14.1.1", "{HALF_BLANK_ROW}", "Safety Population"),
                   "Table 14.1.2"),
  footnotes = list(c("Note 1: foo.", "{HALF_BLANK_ROW}", "Note 2: bar."),
                   NULL)
)
```

#### `rtf_tables()` / `rtf_figures()` gain `titles` and `footnotes` arguments

Each is a list parallel to `tables` / `figures`. Per-element `NULL`
falls back to the default (title = one `{HALF_BLANK_ROW}`, footnote = no
block).

#### `rtf_titles()` / `rtf_footnotes()` standalone setters

Replace per-page titles / footnotes after content has been added. Length
must equal the current number of pages.

### 🔴 Breaking Changes

#### Default row height is now font-size-aware and unified across elements

Previously the package shipped two unrelated default row heights: the
RTF page header / footer used `360` twips (from
`rtfreporter_defaults.R`), while table bodies emitted **no** `\trrh` at
all when `row_height_twips` was left at its `0L` default. Footnotes had
no row height either. As a result, raising the document font size left
the visual layout out of sync.

In v0.0.8 every table-shaped element (RTF header, RTF footer, table
column header rows, table data rows, blank separator rows, footnote
rows) shares a single, font-size-aware default looked up from
`inst/resources/rtfreporter_defaults.R`:

``` r

default_row_height_twips_by_font_half_points = list(
  "16" = 210L,   #  8pt
  "18" = 230L,   #  9pt  ← package default
  "20" = 250L,   # 10pt
  "22" = 270L,   # 11pt
  "24" = 290L,   # 12pt
  ...
)
```

Admins can edit the table (or the linear fallback below it) without
touching package code. Explicit per-element values still take
precedence.

**Migration:** Existing code that explicitly sets
`row_height_twips = 280L` (etc.) is unaffected. Code relying on
`row_height_twips = 0L` to mean “automatic / no `\trrh`” continues to
work — `0L` is still accepted as the legacy value. The new default
behaviour applies whenever `row_height_twips` is `NULL` (the new default
in
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
/
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)).

------------------------------------------------------------------------

#### `default_format` now propagates from `rtf_document()` to the renderer

Previously, `default_format = list(font_size_half_points = 24L, ...)`
passed to
[`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md)
was stored on the S3 object but the pipe-to-R6 adapter dropped it, so
the renderer always used the built-in 9pt default. v0.0.8 wires
`default_format` through to the R6 report so font size, line spacing,
etc. actually take effect (and feed the new row-height lookup above).

------------------------------------------------------------------------

### 🔴 Breaking Changes

#### Pipe API formatting moved into `rtf_tables()` / `rtf_figures()`

The four standalone format functions did not actually affect rendered
RTF output — values were stored but never read by the renderer. They are
now deprecated and replaced by formatting arguments on the constructor
functions:

``` r

# Old (v0.0.6, never worked):
doc <- rtf_document() %>%
  rtf_tables(list(df1, df2)) %>%
  rtf_table_format(pages = "all", border = "tfl", row_height_twips = 280L)

# New (v0.0.7):
doc <- rtf_document() %>%
  rtf_tables(list(df1, df2),
             border = "tfl", row_height_twips = 280L,
             col_rel_width = c(2, 1, 1))
```

Affected functions: `rtf_table_format()`, `rtf_header_format()`,
`rtf_footer_format()`, `rtf_figure_format()`. They remain available as
[`.Deprecated()`](https://rdrr.io/r/base/Deprecated.html) no-ops;
planned for removal in a future release.

**Migration:** Move each formatting argument directly onto
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
or
[`rtf_figures()`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.md)
(for shared defaults across bare `data.frame` / path items), or onto
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
/
[`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md)
(for per-item control). For header/footer border and row height, set
them on
[`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
/
[`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
directly.

------------------------------------------------------------------------

#### One content per page enforced

Each element of
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
/
[`rtf_figures()`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.md)
now corresponds to exactly one page (one table **or** one figure). The
previously documented but silently broken pattern
`list(df1, list(rtfplot(...), rtftable(df2)))` (multiple items on one
page) is rejected at validation time.

**Migration:** Place each table / figure on its own page (consecutive
pages in the same section share the section header).

------------------------------------------------------------------------

#### `rtf_figures()` runtime bug fixed

Previously crashed at render time because paths were wrapped in
`list(path)` but the renderer expected `rtfplot_r6` objects. Paths are
now promoted to `rtfplot_r6`, and `width_twips` / `height_twips` /
`align` may be supplied to
[`rtf_figures()`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.md)
directly. Pre-built
[`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md)
objects in `figures` are accepted unchanged.

------------------------------------------------------------------------

### 🔴 Breaking Changes

#### R6 classes are now private; public API is S3 functions only

Previously, users called R6 class constructors directly:

``` r

# Old (v0.0.5):
report <- rtfreport$new()
tbl    <- rtftable$new(df, ...)
fig    <- rtfplot$new("path/to/img.png")
```

In v0.1.0, use S3 wrapper functions:

``` r

# New (v0.1.0):
report <- rtfreport()
tbl    <- rtftable(df, ...)
fig    <- rtfplot("path/to/img.png")
```

**Migration:** Replace all `ClassName$new(...)` with `ClassName(...)`.  
**Reason:** Cleaner public API; internal R6 classes renamed to
`rtftable_r6`, `rtfplot_r6`, `rtfreport_r6` for clarity.

------------------------------------------------------------------------

#### Content type auto-detection; explicit `type` field no longer required

Previously, `add_page()` required explicit block specifications with
`type`:

``` r

# Old (v0.0.5):
report$add_page(
  section_index = sec,
  content = list(
    list(type = "table", data = rtftable$new(df, ...)),
    list(type = "figure", data = rtfplot$new("img.png"))
  )
)
```

In v0.1.0, objects are auto-detected from their class:

``` r

# New (v0.1.0):
report$add_page(
  section_index = sec,
  content = list(
    rtftable(df, ...),
    rtfplot("img.png")
  )
)
```

**Auto-detection rules:** - `rtftable_r6` → `type = "table"` -
`rtfplot_r6` → `type = "figure"` - `data.frame` → `type = "table"` -
File path (`character(1)`) → `type = "figure"`

**Migration:** Simplify content lists by passing objects directly.
Backward compatibility: explicit `type` field still works internally.

------------------------------------------------------------------------

### ✨ Features

#### Multi-line content titles and footer notes

`title` and `footer_notes` in `add_page()` now accept character vectors
for multiple lines:

``` r

report$add_page(
  section_index = sec,
  title = c("Line 1", "Line 2"),
  footer_notes = c("Note 1", "Note 2")
)
```

------------------------------------------------------------------------

### 🔧 Internal Changes

- R6 classes renamed for clarity:
  - `rtftable` → `rtftable_r6`
  - `rtfplot` → `rtfplot_r6`
  - `rtfreport` → `rtfreport_r6`
- New internal helper `.normalize_content_item()` for block type
  auto-detection
- S3 wrapper functions created in `r/wrappers.R`

------------------------------------------------------------------------

## v0.0.5 and earlier

Not recorded (pre-v0.1.0).

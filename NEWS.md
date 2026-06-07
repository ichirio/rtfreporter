# rtfreporter (development version)

## rtfreporter 0.0.75

### Redesigned `print()` / `plot()` as build-time layout inspectors

`print()` and `plot()` for documents and tables are now purpose-built to verify
layout *before* rendering to RTF.  `print.rtftable()` shows, as plain text, the
column-header rows, body alignment, column widths, border configuration, and any
title/footnote attributes.  `print.rtf_document()` outlines the page setup, each
section's header/footer cells (including page-number tokens such as
`{PAGE}`/`{TOTAL_PAGES}`), and the content blocks with their titles and sizes.
`plot()` is the quick visual companion: it sketches the real column-header
labels in their columns rather than a featureless grey strip.

## rtfreporter 0.0.64

### Article: pharmaverse code folded, table print + snapshot shown

The *From pharmaverse tables to RTF reports* article is restructured so the
pharmaverse example code no longer clutters the page: each table's full recipe
(the pharmaverse build plus the rtfreporter rendering) is now a single folded
`<details>` block -- collapsed by default, expandable, and copyable in one go.
Below each fold the article shows just the printed table object and a snapshot
of the rendered RTF.  Snapshots live in `vignettes/articles/figures/`
(placeholders are shown until the PNGs are added).

## rtfreporter 0.0.63

### Pagination: widow control and tail packing for split groups

When a single group was larger than `max_rows` and had to be force-split, the
continuation could be left with only one row (a `(Cont.)` header plus a single
child) stranded on a near-empty page.  Two fixes:

* **Widow control.** `min_group_rows` (default 2) now also applies to the
  *continuation* of a force-split group: the cut is pulled back so the next
  page carries at least `min_group_rows` of the group's rows.
* **Tail packing (`split = "group_safe"`).** The tail of a force-split group is
  now kept in the page buffer, so the following whole groups pack onto it
  instead of each split group's remainder getting its own sparse page.

Together these remove the near-empty continuation pages (e.g. the
adverse-events tern table dropped from 12 to 11 pages with no 2-row page).

## rtfreporter 0.0.62

### `fmt_count_paren()` now aligns the percentages too

`fmt_count_paren()` is reworked to scan the column and right-justify both the
integer count **and** the number inside the parentheses, so every cell ends up
the same width and a column lines up on the count digit and the percentage --
e.g. `"10 (11.6%)"` over `" 4 ( 4.7%)"`.  Because all cells share one width it
aligns under centre alignment (which the adverse-events tables use), and it
adapts to the column's actual digit counts, so it also handles the 4-digit
event totals that the fixed-width `realign_count_pct()` could not.  Both
adverse-events tables in the article now use it via `cell_format`.

## rtfreporter 0.0.61

### Pluggable cell formatting

`as_rtftables()` gains a `cell_format` argument: a function (applied to every
data column) or a list of functions (one per column) that re-formats the body
cells for monospaced alignment, applied just before pagination.  A format
function follows a small contract -- it takes one column (a character vector)
and returns a character vector of the same length, padding with the
non-breaking space -- documented in `?as_rtftables` and the article.

Two ready-made formatters are provided alongside the existing
`realign_count_pct()`:

* `fmt_count_paren()` aligns an integer count followed by *any* parenthetical
  (e.g. a column mixing `"2 ( 2.8%)"`, `"3 (<1%)"`, `"70 (100%)"` and a lone
  `"0"`), so the digits and a bare zero line up.  The adverse-events tfrmt
  table in the article now uses it, fixing the misaligned `0` cells.
* `fmt_right_align()` -- the minimal "right-justify a column" formatter, used
  in the docs as the template for writing your own.

## rtfreporter 0.0.60

### TLG article: downloadable RTFs, single group gaps, wider label column

* The rendered example RTFs are now committed under
  [`inst/rtf-examples/`](https://github.com/ichirio/rtfreporter/tree/main/inst/rtf-examples)
  and the article links to that folder.  File names mark the source
  pharmaverse table (e.g. `pharmaverse-adverse-events-tfrmt.rtf`).
* Fixed a doubled blank row between groups in the tfrmt tables: that object
  already bakes a group gap via `element_block(post_space = " ")`, so the
  article no longer also passes `blank_rows = "between_groups"` (which had
  produced two blank rows, e.g. between "ANY EVENT" and the first SOC).
* The adverse-events row-label column is widened to 50% so the
  "... (Cont.)" continuation labels fit on one line.

## rtfreporter 0.0.59

### Pagination: widow/orphan control for group headers

`as_rtftables()` (and `paginate()`) gain a `min_group_rows` argument
(default `2`).  In the group-aware splits a page no longer ends on a group
header that has fewer than `min_group_rows` of its child rows on that page:
the whole group is moved to the next page instead.  This fixes cases like a
lone "MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS" system-organ-class
header stranded at the foot of a page with none of its preferred terms.  Set
`min_group_rows = 0` to restore the previous behaviour.

## rtfreporter 0.0.58

### TLG article: adverse-events alignment, three-line title, tweaks

* **Column alignment fixed.**  The adverse-events tables now force the
  row-label column left-aligned and every data column centred via `col_spec`,
  so the "All Patients" column (which had come through left-aligned from the
  source object) lines up with the rest.
* The running-header title is now **three centred lines** -- `Table N`, the
  descriptive title, and `<Safety Analysis Set>`.
* `max_rows` for the adverse-events tables trimmed to 36 (from 38).
* Example company name updated.

## rtfreporter 0.0.57

### TLG article: more rows per adverse-events page

The adverse-events tables now fit `max_rows = 38` rows per printed page (up
from 30), using the page space that was previously left blank.  The tern
table drops from 14 to 11 pages and the tfrmt table from 11 to 9, with the
same wide first column, shorter row height and (for tfrmt) "(Cont.)"
continuation rows.

## rtfreporter 0.0.56

### TLG article: title block in the running header; both pagination modes shown

* The full title block now lives in the **running header** (normal weight, not
  bold): a single centred `Table N  <title> (builder)` line over a centred
  `<Safety Analysis Set>` subtitle, repeated on every page.  The bold content
  titles are replaced by a single empty content title (`""`), which inserts
  one blank line between the header and the table.
* The adverse-events section now spells out the **two pagination strategies**
  it already used: `split = "group_safe"` (tern) keeps each SOC whole, while
  `split = "group_force"` (tfrmt) breaks inside a long SOC and repeats the
  SOC label with `" (Cont.)"` at the top of the next page (via `cont_label`).

No package code changed in this release; it documents and demonstrates the
existing `cont_label` continuation behaviour and the empty-title blank-line
convention.

## rtfreporter 0.0.55

### Bug fix: NBSP-indented rows no longer treated as group headers

`as_rtftables()` detects row groups from the indentation of the first column.
It only recognised a regular space or tab as indent, so gt/tfrmt tables --
which bake row-label indentation as **non-breaking spaces** (U+00A0) -- had
*every* indented sub-row mistaken for a new group.  With
`blank_rows = "between_groups"` that inserted a blank row after almost every
line, roughly doubling the rendered rows and overflowing the printed page
(e.g. the adverse-events `tfrmt` table rendered ~608 rows instead of ~340).
A non-breaking space now counts as indentation, so blanks appear only between
real groups.

### TLG article: adverse-events tables now paginate cleanly

In the *From pharmaverse tables to RTF reports* article both adverse-events
tables (tern and tfrmt) now use a wide first column
(`col_rel_width = c(0.40, 0.15, 0.15, 0.15, 0.15)`) so the long SOC / preferred
-term labels stay on one line, a shorter `row_height_twips = 200` so a full 30
rows fit one printed page, and -- for the tfrmt table -- two/three-line column
headers (`Arm` over `(N=xx)`) so the data columns can be narrow.  The tables
now hold their intended 14 / 11 pages instead of overflowing in Word.

## rtfreporter 0.0.54

### `as_rtftables(auto_width = TRUE)` sizes columns to their content

New `auto_width` argument for `as_rtftables()`: when `TRUE`, each column is
sized to its widest content (column-header label or data cell) so that long
row labels and column headers no longer wrap mid-word.  The widths are
computed once on the whole table and applied to every page, so paginated
pages stay aligned.

* The optional `table_width_twips` argument scales the auto-sized columns to a
  given total (e.g. the writable page width), so the table fills -- or fits
  within -- the page.
* When the table is squeezed *narrower* than its natural width, the row-label
  column (column 1) is kept at its natural width and only the data columns
  shrink, so row labels stay readable while the data-column headers absorb the
  squeeze.

Supporting improvements to `auto_col_widths()`:

* it now measures the **longest line** of a multi-line cell (e.g. a wrapped
  column header like `"Placebo\nN = 86"`) instead of the whole string, and
* gains a `protect_cols` argument to hold chosen columns at their natural
  width when scaling down.

The *From pharmaverse tables to RTF reports* article uses `auto_width = TRUE`
for the demographics tables so long race labels such as
`AMERICAN INDIAN OR ALASKA NATIVE` are no longer split across lines.

## rtfreporter 0.0.53

### Article reframed as a continuation of the pharmaverse examples

The cookbook article (now *From pharmaverse tables to RTF reports*) is
rewritten to make rtfreporter's role explicit: it **complements** the
pharmaverse, adding only the last-mile RTF rendering step.  It now takes the
table objects from the official
[pharmaverse examples](https://pharmaverse.github.io/examples/) -- the
*demographic* and *adverse events* TLGs -- **verbatim**, and renders each to
RTF, demonstrating that rtfreporter can read objects from as many pharmaverse
table packages as possible:

* **Demographics**, built three ways -- **tern + rtables**,
  **gtsummary + cards**, **tfrmt + cards** -- each rendered to RTF.
* **Adverse events**, built two ways -- **tern + rtables** and
  **tfrmt + cards** -- paginated across 14 / 11 pages.
* the gtsummary demographics and the multi-page tern AE table are assembled
  into one deliverable with `assemble_rtf()`.

A separate *Using rtfreporter* section now collects the rtfreporter-specific
options (headers/footers, pagination, count-percent alignment).  `forcats`
added to `Suggests` (used by the pharmaverse tfrmt demographic example).

## rtfreporter 0.0.52

### Adverse-events cookbook now uses the real pharmaverse example objects

The `tlg-catalog` adverse-events section now builds the table from the
`pharmaverseadam` ADaM data, using the exact table objects from the
[pharmaverse adverse-events
example](https://pharmaverse.github.io/examples/tlg/adverse_events.html) --
**two ways**:

* **tern + rtables** (full SOC/PT layout with unique / non-unique patient and
  event counts), paginated with `split = "group_safe"` (each SOC kept whole);
* **cards + tfrmt** (ARD-driven, frequency-sorted), paginated with
  `split = "group_force"`.

With 23 system-organ classes and 242 preferred terms the table spans **14**
(tern) / **11** (tfrmt) pages, exercising real multi-page pagination, repeated
column/running headers, blank rows between groups, and `(Cont.)` markers.

`cards` and `pharmaverseadam` added to `Suggests`.

## rtfreporter 0.0.51

### TLG cookbook article reworked to follow the pharmaverse examples

The `tlg-catalog` article now uses the **CDISC pilot ADaM data** from
`random.cdisc.data` (the data the NEST catalog uses) and mirrors the
pharmaverse TLG examples:

* the **Demographic** table is built three ways -- **tern + rtables**,
  **gtsummary**, and **tfrmt** -- and each is converted to the same clinical
  RTF;
* the **Adverse Events** table is **paginated across pages**
  (`split = "group_safe"`, `max_rows`, with `blank_rows = "between_groups"`),
  showing the running header and repeated column headers;
* the article ends by **assembling** the demographics and the multi-page AE
  table into one deliverable with `assemble_rtf()`.

`random.cdisc.data` added to `Suggests`.

## rtfreporter 0.0.50

### Fixes for gt / gtsummary reading

* **Markdown bold markers stripped.**  gtsummary writes `**Placebo**` (and
  similar) into `by`-group column headers; rtfreporter does not render
  Markdown, so the literal `**` used to show through.  Extracted labels /
  titles / footnotes now have `**...**` markers removed.

### Count alignment: lone zero counts

* `align_count_pct = TRUE` now also right-pads bare integer cells (e.g. a
  lone `0` for a zero count) to the column width, so they line up with the
  `"n (xx.x%)"` cells instead of sitting flush-left.

### TLG cookbook article

* Fixed the tfrmt example (use `column = ARM` directly; a redundant column
  variable was leaking an extra `ARM` column into the output), added
  group-separating blank rows (`blank_rows = "between_groups"`) to the
  demographics and AE tables, shortened the provenance footer, and dropped
  the trivial subject-listing example.

## rtfreporter 0.0.49

### Fix: rtables / tern row-label indentation

`as_rtftables()` now calls `formatters::matrix_form(x, indent_rownames = TRUE)`,
so nested row labels (e.g. preferred terms under an SOC in a tern AE table)
are rendered with their indentation -- exactly as rtables itself prints
them.  Previously the indentation was lost and nested rows came out
flush-left.

### New: percent-sign support in count alignment

`format_count_pct()` gains a `pct_sign` argument, and `realign_count_pct()`
(used by `align_count_pct = TRUE`) now recognises `"n (xx.x%)"` cells --
e.g. from `tern::count_occurrences()` -- and re-pads them to a uniform
width while keeping the `%`.  Cells without a `%` are unchanged.

## rtfreporter 0.0.48

### New article: TLG cookbook (pharmaverse -> RTF)

A worked, catalog-style article (`vignette("tlg-catalog")` on the site)
showing how to take a table object built with **gtsummary**, **tern +
rtables**, or **tfrmt** -- or a plain `data.frame`/tibble listing -- and
render it to a clinical RTF page with `as_rtftables()` +
`generate_rtfreport()`.  Uses fully simulated, self-contained CDISC-style
data (no bundled pilot data), and ends with assembling a multi-table
deliverable via `assemble_rtf()`.  `tidyr` added to `Suggests`.

## rtfreporter 0.0.47

### License changed to Apache 2.0

Relicensed from MIT to **Apache License 2.0** to align with the pharmaverse
ecosystem (admiral, rtables, tern, ... are Apache 2.0) and to provide an
explicit patent grant, which is preferred in regulated / corporate
settings.  `DESCRIPTION` now reads `License: Apache License (>= 2)`; the
full text is in `LICENSE.md`.  Done while the package has a single
copyright holder, so no contributor relicensing consent was required.

### Project infrastructure

* Added GitHub pull-request and issue templates (`.github/`).
* `CONTRIBUTING.md` documents how to become a contributor, the
  issue → merge lifecycle, the CI workflows, and project tracking.

## rtfreporter 0.0.46

### Fixes & changes to the gt / gtsummary / tfrmt reading path

The gt body is now taken from `gt::extract_body()` (the route the deprecated
`paginate()` used) instead of `as.data.frame()`.  This fixes several
regressions and simplifies what is read:

* **Clean body.**  Only the *visible* columns appear, so hidden / helper
  columns -- such as tfrmt's `..tfrmt_row_grp_lbl` -- are dropped (even with
  `read_meta = FALSE`).  Row-group rows render as genuinely empty cells
  instead of leaving a stray newline (gt's `<br />` placeholder), and the
  indentation of nested labels is preserved.
* **Only render-relevant metadata is read.**  Column labels, alignment,
  spanning headers, widths, title/subtitle, footnotes/source notes, and
  in-cell footnote marks (rewritten to `^{N}`).  Per-cell bold/italic from
  `gt::tab_style()`, cell fills and Markdown are **not** read -- RTF cannot
  reproduce them.  See the new *What is carried, by source* table in
  `?as_rtftables`.
* **Header alignment inheritance restored.**  When you override a column's
  `align` via `rtf_tables()`, the column header (and any spanning header
  above it) now follows the new alignment again, unless `header_align` is
  set explicitly -- matching the construction-time cascade.

### Renamed argument

* `as_rtftables(read = )` / `as_rtftable(read = )` are renamed to
  **`read_meta`** ("read the source table's metadata").  `read_meta = FALSE`
  yields the rendered body only.

## rtfreporter 0.0.45

### New: rtables / tern input

`as_rtftables()` (and `as_rtftable()`) now accept any rtables `VTableTree`
-- the `TableTree` / `ElementaryTable` objects produced by **rtables** and
by **tern** analysis functions.  The table is read through its canonical
`formatters::matrix_form()` representation, so the extracted content
matches what rtables itself renders:

* Leaf and multi-level **spanning column headers** (from nested
  `split_cols_by()`).
* Per-column **alignment**.
* The **row-label stub** with its **indentation** (mapped to per-cell
  `indent_twips`); rtables label / group-header rows come through as
  ordinary rows (already interleaved by `matrix_form()`).
* **Titles**: main title + subtitles.
* **Footnotes**: referential footnote texts plus the main and provenance
  footers, appended to the page footnote block.
* In-cell **footnote marks** `{N}` (e.g. `"37.7 {1}"`) rewritten to
  rtfreporter `^{N}` superscript markup.

`read = TRUE` (default) reads all of the above; pass a character vector of
tokens (`"col_header"`, `"alignment"`, `"spanning"`, `"titles"`,
`"footnotes"`, `"indent"`, `"footnote_marks"`) for selective control, or
`read = FALSE` for the formatted body only.

`rtables`, `formatters` and `tern` are added to `Suggests`.

## rtfreporter 0.0.44

### New: `as_rtftables()` -- one entry point for gt / gtsummary -> RTF

`as_rtftables()` converts a table object (`gt_tbl`, a **gtsummary**
table, a `data.frame`/tibble, or a list of these) into a list of
ready-to-render [rtftable()] objects -- one per page.  It both reads the
source table's metadata **and** paginates the body in a single call, so
the long-standing gap where `paginate()` on a `gt_tbl` silently dropped
all gt metadata is closed.

* Page-level title / source-note blocks travel with each page as the
  `rtf_titles` / `rtf_footnotes` attributes, which `rtf_tables()` now
  reads automatically -- no `read_gt` flag required.
* Per-cell styles are sliced to match each page; shared header / width /
  spanning metadata is replicated onto every page.
* `as_rtftable()` (singular) is retained as a convenience wrapper that
  returns a single `rtftable` (`= as_rtftables(split = "none")[[1]]`).

### New: gtsummary input

Any gtsummary table (`tbl_summary()`, `tbl_regression()`, `tbl_merge()`,
`tbl_stack()`, ...) is accepted directly; it is converted to a `gt_tbl`
via `gtsummary::as_gt()` first.  Row indentation and bold group-header
rows are not reproducible in RTF and are dropped; everything structural
comes through.

### New: per-cell styles, footnote marks, HTML cleanup (gt "Phase D")

`read = TRUE` now also reads, from a gt object:

* `"styles"` -- `tab_style(cell_text(...))` bold / italic / underline /
  indent, stored on the new `rtftable$cell_styles` field and applied
  per cell at render time.
* `"footnote_marks"` -- gt's in-cell `<sup>N</sup>` footnote marks are
  converted to rtfreporter `^{N}` superscript markup.
* `"strip_html"` -- stray HTML in cell values is removed (`<br>` becomes
  a line break).

`rtftable()` gains a `cell_styles` argument for setting these directly.

### Deprecated

* `paginate()` is **deprecated** in favour of `as_rtftables()`.  It still
  works (and still returns per-page data.frames for backward
  compatibility), warning once per session.
* `rtf_tables(read_gt = )` is now a legacy path for handing a raw
  `gt_tbl` straight in; prefer converting with `as_rtftables()` first.

### Packaging

* Source directory renamed `r/` -> `R/` so `R CMD check` is clean
  (0 errors, 0 warnings).

## rtfreporter 0.0.41

### Breaking change: removed `rtf_theme()` and the R6 dependency

`rtf_theme()` was a small R6 class kept for the "broadcast-mutable
defaults" use case -- mutate `theme$header_bold <- TRUE` once and
every referencing table picks the change up at the next render.
In practice this is also straightforward to do with the immutable
S3 [rtf_table_style()] (build a fresh style once, then pass it
to every table you build), and the second class was carrying its
own weight in maintenance, in coverage drag (R6 method bodies
are hard for `covr` to instrument cleanly), and in a `globalVariables("self")`
hack to silence `R CMD check`.

Removed:

* `rtf_theme()`, `rtf_theme_tfl()`, the `theme =` argument of
  [rtftable()], the `.refresh_theme()` renderer hook, the
  `R6` Suggests entry, the `globalVariables("self")` declaration.
* `vignettes/class-systems.Rmd` (the S3-vs-R6 tour was the only
  remaining R6 user once the class itself was gone).

Migration:

```r
# Before (R6)
theme <- rtf_theme(header_bold = FALSE)
t1 <- rtftable(df1, theme = theme)
t2 <- rtftable(df2, theme = theme)
theme$header_bold <- TRUE      # broadcast: both tables follow

# After (S3, snapshot)
style <- rtf_table_style(header_bold = TRUE)
t1 <- rtftable(df1, style = style)
t2 <- rtftable(df2, style = style)
```

The pipe API was always S3-only, so no pipe-style code is affected.

### Test coverage lifted past 90%

The full suite gained 17 targeted "fill-in" tests in a new
`tests/testthat/test-coverage-fillins.R`, plus the rtf_theme
removal dropped a chunk of hard-to-instrument R6 code from the
denominator.  Net: total coverage `87.71% -> 91.88%`
(test count `1092 -> 1085` -- net negative because removing R6
also removed 31 tests).

## rtfreporter 0.0.40

### gt integration -- Phase C (completes the v0.1.0 roadmap)

Adds the final two `read_gt = ...` tokens from
`specs/gt-integration-spec.md`.  Combined with Phase A (v0.0.38) and
Phase B (v0.0.39), the GT bridge now supports **all nine** documented
tokens.  `read_gt = TRUE` automatically expands to the full set.

* **`"footnotes"`** -- table-level only.  Every `tab_footnote()`
  text (regardless of its anchor: column label, body cell,
  standalone, etc.) is flattened to plain text and prepended to the
  page footnote block.  When `"source_notes"` is also active,
  footnote texts come ABOVE source notes -- matching gt's vertical
  layout.  **Cell-mark injection is deferred to a later release.**

* **`"stub"`** -- two transformations:
    1. The groupname column (`gt(df, groupname_col = ...)`,
       boxhead `type == "row_group"`) is dropped from the data.
       Group-transition rows are then interleaved into the body
       wherever `_stub_df$group_id` changes; each transition row
       carries the group label in the leftmost cell.
    2. `tab_stubhead(label = ...)` is applied as the first
       column's header label, overriding any cols_label value for
       the stub column.

  Combines cleanly with `"hidden"` (both can drop columns) and with
  `"spanning"` (stubhead label lands on the bottom-row labels under
  the spanner stack).

### Vignette

`vignettes/gt-integration.Rmd` walks through the full workflow: a
quick-start, a TFL-style adverse-events example end-to-end (group
rows + stubhead + spanners + footnotes + source note), and the
explicit-argument precedence rules.

### Test additions

`tests/testthat/test-gt-adapter.R` gains 21 new tests for Phase C:
the two new extractors individually, the row-interleaver, four
`.gt_to_rtftable_kwargs()` integration paths (footnotes-only,
footnotes+source_notes, stub+col_header, stub+spanning, stub+hidden),
and one full end-to-end render with `read_gt = TRUE` that exercises
every Phase A/B/C token at once.

Full suite: 1092 PASS / 0 FAIL.

## rtfreporter 0.0.39

### gt integration -- Phase B

Adds the three "structural" tokens from `specs/gt-integration-spec.md`
on top of the Phase A vocabulary:

* **`"spanning"`** -- multi-level spanner labels from
  `gt_obj[["_spanners"]]` are stacked as additional rows ABOVE the
  column labels in `col_header`.  Higher `spanner_level` -> drawn
  higher in the rendered header.  Spanners that cover non-contiguous
  visible columns are skipped (rtfreporter requires contiguity).
* **`"widths"`** -- per-column widths from
  `gt_obj[["_boxhead"]]$column_width` are translated to one of:
  - `column_widths_twips` (when all values are `"NNpx"`), using
    1 px = 15 twips (the 96 dpi CSS convention).
  - `col_rel_width` (when all values are `"NN%"`).
  - `NULL` otherwise (mixed, missing, or unparsable).
* **`"hidden"`** -- columns whose `_boxhead$type == "hidden"` are
  dropped from the extracted data.frame, the column labels, the
  alignment vector, the widths, and the spanner column-index
  mapping (so spanner rows stay aligned with the visible columns).

`read_gt = TRUE` now expands to *all seven* Phase-A + Phase-B tokens
(was four).  Adding spanning / widths / hidden tokens is automatic
when upgrading: code that already wrote `read_gt = TRUE` becomes
strictly more powerful with no source changes.

Two-way independence:

* `"hidden"` can be used by itself -- it just shrinks the data.
* `"spanning"` without `"col_header"` falls back to using the data
  column names as the bottom row of the header stack.
* `"widths"` without `"hidden"` keeps the gt width vector at its
  original length; the renderer fails fast if the count mismatches
  the data.frame.

Precedence is unchanged: explicit `rtf_tables()` /
`rtftable()` arguments always beat the gt-extracted values, including
the new `column_widths_twips` / `col_rel_width` slots.

Test additions: 36 new tests in `tests/testthat/test-gt-adapter.R`
covering the three new extractors, the token resolver's new return
shape, three end-to-end render checks (including a combined
spanner + hidden + widths integration), and the precedence rule
for every new slot.

## rtfreporter 0.0.38

### gt integration -- Phase A (preparing v0.1.0)

This release lays down the first slice of the
[gt](https://gt.rstudio.com) -> rtfreporter bridge described in
`specs/gt-integration-spec.md`.  A `gt_tbl` can now be passed straight
into `rtf_tables()`, and the package optionally reads four
"shape-preserving" gt attributes:

* **`col_header`** -- column labels from `gt_obj[["_boxhead"]]$column_label`
* **`alignment`**  -- per-column alignment from
  `gt_obj[["_boxhead"]]$column_align`
* **`titles`**     -- title + subtitle from `gt_obj[["_heading"]]`,
                      mapped to the page's `titles[[i]]` block
* **`source_notes`** -- source notes from
  `gt_obj[["_source_notes"]]`, mapped to the page's `footnotes[[i]]` block

#### New API

```r
# Accept a gt_tbl directly; pull every Phase-A attribute through.
doc |> rtf_tables(list(my_gt), read_gt = TRUE)

# Selective opt-in.
doc |> rtf_tables(list(my_gt), read_gt = c("col_header", "titles"))

# Standalone wrapper (returns a plain rtftable).
as_rtftable(my_gt, read = TRUE)
```

`read_gt = FALSE` (the new default) keeps every gt_tbl item treated as
a bare data.frame via `as.data.frame()` -- backward-compatible with
v0.0.37 code that already paginated gt's *rendered* table.

#### Precedence

Explicit `rtf_tables()` / `rtf_titles()` / `rtf_footnotes()` arguments
always beat the gt-extracted values.  Per-column `col_spec` entries
are deep-merged: a user-supplied `align = "left"` for column 1
overrides gt's column 1 alignment but leaves gt's alignment in
place for columns the user did not mention.

#### Forward compatibility

Tokens for Phase B (`"spanning"`, `"widths"`, `"hidden"`) and Phase C
(`"footnotes"`, `"stub"`) are recognised today but currently emit a
"not yet implemented" warning and are silently dropped.  Code written
against the full token list will Just Work once those phases land.

`gt` stays in `Suggests` -- the bridge guards with
`requireNamespace("gt")` and raises a clear error if a `gt_tbl` is fed
in without gt installed.  The 18 new tests in
`tests/testthat/test-gt-adapter.R` skip cleanly when gt is absent.

## rtfreporter 0.0.37

### Bug fix: phantom rule under header on assembled file's first page

When `assemble_rtf(toc = ...)` produced an assembled deliverable, the
first page of every source-file body section showed a faint horizontal
rule directly under the page header.

Root cause: `.insert_bookmark()` placed the bookmark/outline paragraph
**between `\sectd` and the section's page-property + header/footer
declarations**.  RTF parsers treat a paragraph emitted in that window
as part of the **previous** section's flow, so any lingering paragraph
state from the previous section's last paragraph (typically a footnote
with `\brdrt\brdrs\brdrw15`) bled onto the new section's header band
and was rendered as a thin rule.

The fix advances the insertion point past the section preamble
(`\sectd`, `\sbkpage...`, `{\header ...}`, `{\footer ...}`) so the
bookmark and outline paragraph land in the new section's BODY where
they belong.

### Hardened invisibility: `\sl1\slmult0` exact line spacing

The outline paragraph now also emits `\sl1\slmult0` -- exact line
spacing of 1 twip (~1/1440 inch).  This pins the paragraph's rendered
height to ~0 px regardless of the converter's `\fs` interpretation,
guaranteeing the outline label cannot push a borderline-fitting
table from one page onto a second.  `\cf2\fs2` are retained as
belt-and-braces (white text + 1-pt size).

### ydisctools alignment audit -- what we kept and what we adopted

After auditing Yenu's `ydisctools::assemble_rtf()` for the proven-good
RTF idioms, the conclusions are:

* **Adopted** -- white-on-white invisibility via `\cf2` (v0.0.36) and
  exact-line-spacing belt-and-braces (v0.0.37).
* **Adopted in spirit** -- bookmark + outline paragraph placed in a
  position that does not bleed into the new section's header band.
  (We place them AFTER the section preamble; ydisctools places them
  BEFORE `\sectd`.  Both approaches avoid the bleed; placement after
  preamble keeps the anchor on the correct page for `PAGEREF`
  resolution.)
* **Kept rtfreporter** -- multi-level `toc_heading()` / `toc_entry()`
  layout, `PAGEREF` fields for dynamic TOC page numbers (ydisctools
  bakes them statically), `\*\fldinst` per the RTF spec, full
  per-page static `{PAGE}` token support.

## rtfreporter 0.0.36

### Outline label is now hidden via white-on-white (`\cf2\fs2`)

v0.0.35 set the PDF-outline label to `\fs0` (font size 0) hoping for
zero rendered height.  **LibreOffice interprets `\fs0` as "use
default"** and renders the label at the body font size (~12 pt), which
made the source-file title appear *twice* near the top of each table
section.

Re-investigating Yenu's `ydisctools` showed the actual invisibility
mechanism there is `\cf<white>` (white text colour), not `\fs0`.
rtfreporter v0.0.36 adopts the same approach:

* `.build_color_table_rtf()` now always emits **white at colour-table
  index 2** (between black at 1 and any user colours at 3+).
* `.build_color_index_map()` shifts user colour indices by +2 so the
  hex-to-index lookup remains correct.
* `.insert_bookmark()` (assemble_rtf) renders the outline label with
  `\cf2\fs2` — white text at 1 pt — inside a balanced `{ ... }`
  group.  White-on-white is invisible regardless of the PDF
  converter's `\fs0` interpretation, and 1 pt keeps the line height
  to ~1 px so no vertical space leaks either.

Verified: the LibreOffice-rendered `output/demo/*_assembled_*.pdf`
files no longer show the source-file title above each body section,
and the PDF outline (bookmark panel) still lists all three sources.

## rtfreporter 0.0.35

### Outline label is now fully invisible (`\fs0`)

`.insert_bookmark()`'s outline-label paragraph now uses `\fs0` (font
size 0) instead of `\fs2` (1 pt).  v0.0.31's 1-pt text was faintly
visible at the top of each source-file body section; the new size-0
text occupies no rendered space at all while LibreOffice still
recognises the paragraph as a heading via `\outlinelevel`, so the PDF
outline / bookmark panel is unchanged.

This matches the trick used by Yenu's `ydisctools` (which combines
`\fs0` with a white `\cf` colour for double safety).

The corresponding regression test in `tests/testthat/test-assemble-rtf.R`
was updated to expect the `\fs0`-shaped balanced group.

## rtfreporter 0.0.34

### Bug fix: `{PAGE}` now increments across sub-pages of one rtf_section

In v0.0.32 `{PAGE}` was correctly switched from a dynamic field to a
static integer, but it baked the **rtf_section's first-page number**
into the header text — and an `rtf_section` whose `rtf_tables()` adds
multiple sub-pages emitted a single RTF `\header` block shared across
all of them.  Result: every sub-page in such a section showed "Page 1"
instead of 1, 2, 3, …

`generate_rtfreport()` now detects when a header or footer contains
`{PAGE}` and promotes every sub-page boundary inside that
rtf_section from a `\page` break to a `\sect` break, re-emitting
`\sectd` + page settings + `{\header}` + `{\footer}` for each sub-page
with the correct baked-in number.  AUTO-only headers stay on the cheap
one-`\header`-per-rtf_section path (no extra section breaks emitted).

Three new tests in `tests/testthat/test-page-tokens.R` lock the
behaviour:
* "Page 1 of 3", "Page 2 of 3", "Page 3 of 3" all present in a single
  3-sub-page section,
* `\sect` breaks are emitted at every sub-page boundary,
* AUTO-only headers still emit zero internal `\sect` breaks.

## rtfreporter 0.0.33

### Bug fix: character-format leak from cover / TOC / outline paragraphs

When `assemble_rtf()` was called with `cover = list(...)` or `toc = ...`,
the front-matter paragraphs emitted bare character-format properties
that bled across `\par` into the following body content:

* `.insert_bookmark()` emitted `\pard\plain\fs2\sa0\sb0\outlinelevel0 LABEL\par`
  — the `\fs2` (1-pt) outline-paragraph font carried forward into the
  body table, shrinking every cell's text to invisible.
* `.build_cover_section()` and `.build_toc_section()` emitted
  `\pard\qc\b\fs44 TEXT\b0\fs0\par` — the closing `\fs0` (size 0)
  similarly leaked.

The visible symptom: assembled PDFs / RTFs rendered table **borders**
correctly, but every cell appeared empty.  The pre-v0.0.31 (no-cover,
no-outline) path was unaffected, which is why older outputs looked fine.

Every formatted paragraph in `.insert_bookmark()`, `.build_cover_section()`,
and `.build_toc_section()` is now wrapped in an RTF `{ ... }` group, so
its character-format state is local and cannot bleed into adjacent
content.  Regression tests in `tests/testthat/test-assemble-rtf.R`
lock the fix down structurally.

### Demo script — both no-cover and with-cover variants

`output/gen_assemble_demo.R` now produces **four** assembled outputs:

* `auto_assembled_default.{rtf,pdf}`   — AUTO sources, TOC only (no cover)
* `auto_assembled_full.{rtf,pdf}`      — AUTO sources, cover + TOC
* `static_assembled_default.{rtf,pdf}` — STATIC sources, TOC only (no cover)
* `static_assembled_full.{rtf,pdf}`    — STATIC sources, cover + TOC

The "default" variants reflect the common eCTD deliverable layout
where a cover page is *not* attached and the TOC alone serves as the
front matter.  No change to `assemble_rtf()`'s own default — `cover = NULL`
has always been the default.

## rtfreporter 0.0.32

### Bug fix: `{PAGE}` is now a STATIC integer (was incorrectly dynamic)

Previously `{PAGE}` was substituted with RTF's `\chpgn` dynamic field —
identical to `{AUTO_PAGE}`.  This contradicted the documented spec, which
states that `{PAGE}` should be a **static** integer baked in at render
time (the section's first-page number).  Effectively `{PAGE}` and
`{AUTO_PAGE}` produced indistinguishable output, defeating the purpose of
having two separate tokens.

`generate_rtfreport()` now substitutes `{PAGE}` with the literal
first-page number of the section being rendered.  Combined with the
already-static `{TOTAL_PAGES}`, the documented `Page {PAGE} of
{TOTAL_PAGES}` idiom now correctly emits e.g. `Page 1 of 3` as
literal text.

Token semantics — recap:

| Token                 | Kind    | Resolves to                       |
|-----------------------|---------|-----------------------------------|
| `{AUTO_PAGE}`         | dynamic | `\chpgn` (per-page, viewer)       |
| `{AUTO_TOTAL_PAGES}`  | dynamic | NUMPAGES field (viewer)           |
| `{SECTION_PAGES}`     | dynamic | SECTIONPAGES field (viewer)       |
| `{PAGE}`              | static  | section's first-page integer      |
| `{TOTAL_PAGES}`       | static  | document total page integer       |

A new `tests/testthat/test-page-tokens.R` file locks these semantics
with 7 dedicated tests.  `output/gen_assemble_demo.R` was rewritten to
produce two independent demo sets (an AUTO-only set and a STATIC-only
set), each with its own TOC/cover/PDF — reflecting realistic deployment
where a deliverable uses one numbering style throughout.

## rtfreporter 0.0.31

### `assemble_rtf()` — PDF outline / bookmark panel support

When `toc` is supplied, `assemble_rtf()` now also emits a tiny
`\outlinelevel0` paragraph next to each per-file bookmark.  RTF
viewers ignore it for display purposes (1-pt font, no surrounding
spacing), but RTF→PDF converters such as **LibreOffice** translate
the `\outlinelevel` mark into a real **PDF outline entry** — i.e.,
the bookmark panel on the left edge of an Adobe Reader / Word
window.  This makes the assembled deliverable navigable in PDF
form, satisfying a common eCTD leaf-file expectation.

The outline label is taken from the corresponding `toc_entry()`'s
`label` when present; otherwise it falls back to
`sub(".rtf$", "", basename(file))`.  `toc = NULL` emits no outline
paragraphs (output is unchanged from v0.0.30).

### Static `{TOTAL_PAGES}` documented + tested across assembly

Tests now lock the two distinct token semantics in
`assemble_rtf()`:

* **`{AUTO_PAGE}` / `{AUTO_TOTAL_PAGES}` / `{PAGE}`** — all emit
  RTF *dynamic field codes* (`\chpgn` and `NUMPAGES`).  They
  recompute correctly across the assembled document.
* **`{TOTAL_PAGES}`** — baked into the source as a literal integer
  at render time.  After assembly it still reflects only the
  source file's own page count (the documented limitation).

## rtfreporter 0.0.30

### `assemble_rtf()` — multi-level TOC, auto-extraction, cover page, page numbering

Building on v0.0.29's clickable TOC, this release covers the
remaining requests for a full deliverable-package workflow.

**`toc = "auto"`** — auto-extract each input file's title (the
first centred-bold paragraph emitted by `.render_title_text()`)
and use it as a TOC entry.  Falls back to the file's basename if
no title is detected.

```r
assemble_rtf(c("t14_1_1.rtf", "t14_2_1.rtf"), "out.rtf",
             toc = "auto", overwrite = TRUE)
```

**Structured / multi-level TOC** via two new helper constructors:

* **`toc_heading(label, level)`** — non-clickable section heading
  rendered bold without a page number.
* **`toc_entry(label, file, level)`** — clickable entry pointing
  to a per-source-file bookmark.  `file =` can be a path that
  appears in `input_files`, an integer 1-based index, or `NULL`
  (consume the next file in declaration order).

```r
assemble_rtf(
  c("t14_1_1.rtf", "t14_2_1.rtf", "l16_1.rtf"),
  "tfl_package.rtf",
  toc = list(
    toc_heading("EFFICACY ANALYSES"),
    toc_entry("Table 14.1.1 Demographics"),       # auto-bound to file 1
    toc_heading("SAFETY ANALYSES"),
    toc_entry("Table 14.2.1 Adverse Events"),     # auto-bound to file 2
    toc_heading("LISTINGS"),
    toc_entry("Listing 16.1 Subject Disposition") # auto-bound to file 3
  ),
  overwrite = TRUE
)
```

**`toc_page_numbering`** controls how the TOC pages number themselves:

* `"none"` (default) — Arabic numbering flows continuously
  from page 1 (TOC counts as page 1, body continues from there).
* `"roman"` — TOC pages use lowercase Roman numerals (i, ii, iii);
  body restarts at 1 with Arabic numerals.
* `"decimal"` — TOC pages and body pages each start at 1.

The body-restart is achieved by injecting `\pgnrestart\pgndec`
right after the first body section's `\sectd`.

**`cover = list(...)`** — optional cover page section before the
TOC.  Recognised fields: `title`, `subtitle`, `date`, `version`,
`meta` (character vector).  Each is rendered as a centred line
with size hierarchy (title 22pt → meta 10pt).  Any field that is
`NULL` or empty is silently skipped.

```r
assemble_rtf(
  c("t14_1_1.rtf", "t14_2_1.rtf"),
  "tfl_package.rtf",
  cover = list(
    title    = "Study XYZ-001",
    subtitle = "Final Statistical Report",
    date     = "2026-05-29",
    version  = "v1.0",
    meta     = c("Confidential", "Prepared by ACME Pharma")
  ),
  toc                = "auto",
  toc_page_numbering = "roman",
  overwrite          = TRUE
)
```

### Backward compatibility

`toc = NULL` AND `cover = NULL` AND `toc_page_numbering = "none"`
still produces a byte-for-byte identical output to the v0.0.28
behaviour — locked by a regression test.

## rtfreporter 0.0.29

### `assemble_rtf()` — clickable Table of Contents + bookmarks

`assemble_rtf()` gains a TOC option for the common "deliverable
package" workflow.  Pass a character vector of labels (one per
input file) to render a clickable Table of Contents on the front
page of the assembled document:

```r
assemble_rtf(
  input_files = c("t14_1_1.rtf", "t14_2_1.rtf", "l16_1.rtf"),
  output_file = "tfl_package.rtf",
  toc         = c("Table 14.1.1 Demographics",
                  "Table 14.2.1 Adverse Events",
                  "Listing 16.1 Subject Disposition"),
  overwrite   = TRUE
)
```

* Each TOC entry is an **RTF `HYPERLINK` field** pointing to a
  per-source-file bookmark inserted right after that section's
  `\sectd`.  Clicking the entry in Word / Pages jumps to the
  corresponding table.
* Each row also ends with a `PAGEREF` field, so the page number
  next to each entry is **calculated by the viewer** when the file
  opens — no hard-coded numbers, no stale TOC.
* Bookmark names auto-derive from each input file's basename
  (sanitised to `^[A-Za-z][A-Za-z0-9_]{0,31}$`).  Duplicate-name
  clashes (two files called `x.rtf` in different folders) are
  resolved automatically with `_1` / `_2` suffixes.
* `toc_leader = "dot"` (default) draws a dotted leader between
  each label and its page number; `"none"` leaves whitespace.
* `bookmark_prefix` (default `"tfl_"`) lets you namespace
  bookmarks when later combining multiple assembled outputs.

The pre-existing call shape (`assemble_rtf(input_files,
output_file, overwrite)`) is unchanged.  `toc = NULL` (the
default) produces a byte-for-byte identical output to the
pre-v0.0.29 behaviour — covered by a regression test.

The originally-proposed *combined-document page number in the
footer* feature was **withdrawn**: injecting it into the existing
footer would shift the rest of the footer downward, and a separate
footer band would clash with the source documents' own footers.
Use `{AUTO_PAGE}` / `{AUTO_TOTAL_PAGES}` in your `rtf_footer()`
when you generate each source file instead — those are dynamic
fields that already recompute across the assembled document.

## rtfreporter 0.0.28

### Coverage lift — Phase 1 of 3 (67% → 73%)

Backfill of the four files that were at 0% coverage:

* `R/assemble_rtf.R` (0% → **95%**) — five new tests cover the
  happy path (2-file and N-file concatenation), input-count
  validation, missing-file detection, overwrite guard, and
  rejection of non-rtfreporter RTFs (no `\sectd`).
* `R/rtfplot.R` (0% → **56%**) — six new tests cover construction
  from a PNG, propagation of `width_twips` / `height_twips` /
  `align`, file-not-found / unsupported-extension errors,
  invalid-align rejection, and end-to-end embedding into a
  generated RTF via `rtf_figures()` (checking the output contains
  `\pict` and `\pngblip`).  A 74-byte hand-rolled 1x1 RGB PNG is
  constructed inside the test, so no graphics device is required.
* `R/text_width.R` (0% → **93%**) — eleven new tests cover
  `text_width_in()` (length parity, NA handling, linear scaling
  with character count, scaling with `size_half_points`, font
  variants including unknown-font fallback) and `auto_col_widths()`
  (per-column integer widths, exact scaling to `table_width_twips`,
  header-text inclusion, `min_col_width_twips` floor, pipe-string
  header).
* `R/zzz.R` (0% → **100%**) — four new tests cover the `.onLoad`
  hook's namespace presence, its idempotent re-invocation, the
  populated `.rtf_theme_class` R6 generator after package load,
  and the no-op re-init via `.init_rtf_theme_class()`.

Per-file post-Phase-1 coverage:

  R/wrappers.R                   100%
  R/zzz.R                        100%
  R/assemble_rtf.R                95%
  R/plot.R                        94%
  R/text_width.R                  93%
  R/format_count_pct.R            93%
  R/set_blank_rows.R              92%
  R/paginate.R                    90%
  R/blank_rows.R                  78%
  R/generate_rtfreport.R          77%
  R/pipe-composition.R            75%
  R/rtf_table_style.R             75%
  R/rtftable.R                    66%
  R/rtfreport.R                   62%
  R/col_header.R                  59%
  R/rtfplot.R                     56%
  R/rtf_border.R                  52%
  R/rtf_theme.R                   15%
  TOTAL                          73.04%

53 new test expectations; total **455 PASS** under `devtools::test()`.

### Codecov floor locked at 70%

`codecov.yml` switched the project-level status from
`informational: true / target: auto` to `informational: false /
target: 70%`.  Any PR that drops total coverage below 70% will now
fail the CI check, preventing silent erosion as new features land.

Phase 2 (target 80%) and Phase 3 (target 90%) will land closer to
v0.1.0 / v0.2.x respectively.

## rtfreporter 0.0.27

### `format_count_pct()` — right-align the 100% branch

The 100% branch used to render `" 30 (100) "` (closing paren in
column 9, trailing space at column 10), so it did not line up with
the other paren-bearing branches.  It now renders `" 30  (100)"` —
an extra space before `(` so the closing paren sits at column 10
just like `" 14 (50.0)"` and `"  5  (5.0)"`.  All paren-bearing
rows now share the same right edge.

## rtfreporter 0.0.26

### New utility functions extracted from `paginate()`

* **`format_count_pct(count, pct, pct_unit, nbsp)`** — produces
  uniform-width `"n (xx.x)"` strings from numeric `count` and `pct`
  vectors.  Port of the reference helper from Issue \#2 (clinical
  TFL column alignment).  Four width branches:
  count-only when count is `NA` or `0`, `n (100)` at exactly 100%,
  one-digit pct gets an extra space (`n  (X.Y)`), two-digit pct
  uses `n (XX.Y)`.  Padding spaces become NBSP (`U+00A0`) by
  default so RTF / Word does not collapse them.

* **`realign_count_pct(strings, nbsp)`** — same width policy but
  starting from already-formatted strings (e.g. those `gt` emits
  in the rendered body).  Cells matching `"n (xx.x)"` are parsed
  and reformatted; non-matching cells pass through unchanged.

* **`set_blank_rows(df, blank_rows, blank_row_first, blank_row_end,
  group_col)`** — exposes the blank-row attribute logic that
  `paginate()` had internally.  Takes ONE data.frame (already at
  page size), resolves the blank spec, and writes the result onto
  `attr(df, "rtf_blank_rows")` for `rtftable(read_attributes = TRUE)`
  to consume.  Use this when you already do your own paging and only
  need the blank-row mechanism.

### `paginate()` integration

* `paginate(..., align_count_pct = TRUE)` runs
  `realign_count_pct()` on every character column other than column 1
  *before* splitting, so every page inherits the cleaned-up cells.
  Default is `FALSE` (no automatic rewriting).
* Internally, `paginate()` now delegates the per-page blank-row
  work to `set_blank_rows()`; the two APIs cannot drift apart.

## rtfreporter 0.0.25

### `paginate()` — list-name propagation, `split = "by_value"`, clearer help

* **Named-list inputs round-trip through `paginate()`.**  When the
  input is a `list` whose elements have names, those names are now
  carried through to the output:

    * input → 1 page  → the chunk keeps the input name
    * input → many pages → chunks named `<name>.1`, `<name>.2`, …
    * unnamed input → no name (as before)

  So `paginate(list("Table 14.1.1" = tbl1, "Table 14.2.1" = tbl2))`
  returns a 2-element named list, and the names flow on to
  `rtf_tables(pages, auto_section = TRUE)` if you choose to use it.

* **New `split = "by_value"` mode.**  One page per detected group,
  *never* packed.  Each returned page is named by the group's label
  (the indented col-1 text when `group_col = NULL`, otherwise the
  value of `df[[group_col]]`).  Combine with
  `rtf_tables(pages, auto_section = TRUE)` to render one RTF section
  per group value (e.g. one section per study visit).  A group that
  exceeds `max_rows` is internally force-split with `(Cont.)` and
  the sub-chunks get suffixed names (`<label>.1`, `<label>.2`, …).

* The page-name (when present) is also surfaced in
  `attr(., "rtf_paginate_meta")$page_name` for programmatic access.

* **`?paginate` reference page reformatted.**  Each `@param` now
  stands on its own paragraph with enumeration bullets where
  appropriate, instead of running together as a single dense block.
  Two new worked examples (sections 6–7) show the `"by_value"` mode
  and the named-list round-trip.

## rtfreporter 0.0.24

### `paginate()` — preserve the input's class chain (tibble in → tibble out)

* `paginate(<gt_tbl>)` no longer down-grades the extracted body to a
  plain `data.frame`.  Since `gt::extract_body()` returns a tibble
  and gt-based workflows are tibble-native, every returned page is
  now also a tibble (`tbl_df` / `tbl` / `data.frame`).
* `paginate(<tibble>)` likewise returns tibbles on every page,
  including pages that came out of the group-force continuation path
  (where an internal `rbind()` would otherwise drop the tibble class).
* `paginate(<data.frame>)` still returns plain data.frames, as
  before.

Implementation: the input's `class()` chain is captured once and
re-applied to each returned chunk after the splitter has finished.

## rtfreporter 0.0.23

### `paginate()` enhancements

* **`blank_row_first = TRUE` / `blank_row_end = TRUE`** — new
  per-page boolean arguments that auto-insert a blank row at the top
  / bottom of *every* returned page (positions `0` and `nrow(chunk)`
  respectively).  Combine with `blank_rows = "between_groups"` to
  get the clinical-TFL pattern of blank-before-each-group +
  blank-at-page-edges.
* **Indent-based grouping clarified.**  The auto-detected group
  semantics (default `group_col = NULL`) are now explicitly
  documented as indent-based: only rows whose first column starts
  with a non-space character open a new top-level group; rows with
  leading whitespace (any depth) stay in the current group.
  Multi-level layouts like

  ```
  group1
      sub-row 1
      sub-row 2
          sub-sub-row 3
  group2
      ...
  ```

  therefore split / blank correctly out of the box, using `group1`
  and `group2` as the unit.

## rtfreporter 0.0.22

### New features

* **`paginate()` — unified table-object to per-page data.frame list**
  ([#2](https://github.com/ichirio/rtfreporter/issues/2)).  Single S3
  entry point that converts a [gt::gt()] table, a plain `data.frame`,
  or a `list` of either into a list of data.frames sized for one
  RTF page each, ready to be passed to `rtf_tables()`.

  Splitting strategies:

  - `split = "none"` (default) — pass through as a single page.
  - `split = "rows"` — manual force-split at the row indices in
    `split_rows`.
  - `split = "group_safe"` — pack whole groups onto each page; spill on
    overflow.  A single group larger than `max_rows` is force-split
    with `(Cont.)` continuation rows.
  - `split = "group_force"` — cut every `max_rows`; when a cut lands
    inside a group, the next page begins with a `<label> (Cont.)`
    header row whose summary value cells are blanked out.

  Groups are either auto-detected from leading-whitespace indentation
  on the first column (default) or specified explicitly via
  `group_col =`.

  Blank-row positions can be supplied as an integer vector, as the
  keyword `"between_groups"` (auto-inserts a blank between detected
  groups), or as a `list(...)` combining both.  Positions are
  attached as an `rtf_blank_rows` attribute on each returned
  data.frame and consumed automatically by
  `rtftable(read_attributes = TRUE)`.

  Extending to a new table-object type is one S3 method:

  ```r
  paginate.your_class <- function(x, ...) {
    df <- ...extract a data.frame from x...
    paginate.data.frame(df, ...)
  }
  ```

  Added to `Suggests:`: `gt (>= 0.9.0)`.

## rtfreporter 0.0.21

### New features

* **Unified column-header API.** Multi-row column headers can now be
  written in a single uniform form using `pos =` cell specs, regardless
  of whether the top-most row is a spanning row or a plain label row.
  Previously the first row had to use either `col_header` (no spanning)
  or `spanning_header` (separate argument); now spanning at every level
  (including the first) is expressed the same way.

  - `col_cell(pos, label, ...)` — one cell.  `pos = 1` is a single
    column; `pos = c(2, 5)` spans data columns 2–5.  Positions always
    refer to the underlying data columns.
  - `rtf_col_header(row1, row2, ...)` — collect rows top-to-bottom.
  - `add_col_header_row(hdr, row, .position = c("bottom", "top"))` —
    incremental builder.

  Example:

  ```r
  rtftable(df, col_header = rtf_col_header(
    list(col_cell(1, ""), col_cell(c(2, 5), "Treatment")),
    list(col_cell(1, ""),
         col_cell(c(2, 3), "Drug A"),
         col_cell(c(4, 5), "Drug B")),
    c("Item", "N", "Mean", "N", "Mean")
  ))
  ```

  A bare list of `col_cell()`s is auto-wrapped as a single header row,
  so `col_header = list(col_cell(1, "Item"), col_cell(c(2, 3), "M (SD)"))`
  works.

  All previously-accepted forms (`c("A", "B", "C")`, mixed lists with
  `list(from, to, label, ...)` specs, and the standalone
  `spanning_header =` argument) continue to work unchanged.

### Default changes (potentially visible to existing reports)

* **Cell padding default lowered to 0 twips** (was 72 twips ≈ 0.05 inch
  on both sides).  Header, footer and content-table cells now sit flush
  against the cell border by default, matching the typical clinical
  TFL look.  Callers who want the old behaviour can opt in:
  `rtftable(..., cell_padding_left_twips = 72L, cell_padding_right_twips = 72L)`.
  Reflected in `inst/resources/rtfreporter_defaults.R`.
* **Document margins enlarged ~20%** to give a little more breathing
  room around the writable area:
    * top / bottom: 0.75 inch → **0.9 inch**
    * left / right: 0.5 inch → **0.6 inch**
  Callers passing explicit page metrics are unaffected.

## rtfreporter 0.0.20

### Infrastructure

* Added a `pkgdown` site published to GitHub Pages at
  <https://ichirio.github.io/rtfreporter/>.
* Migrated the test suite from bare `tests/test-*.R` scripts to
  `testthat` (edition 3) under `tests/testthat/`.
* Added GitHub Actions workflows for `R CMD check` (Ubuntu, macOS,
  Windows × R-release / devel / oldrel-1), `pkgdown` deployment, and
  test coverage reporting via `covr` + Codecov.
* `DESCRIPTION` modernised: full author/maintainer information, `URL`,
  `BugReports`, expanded `Description`, `Depends: R (>= 4.1)`, and
  optional `Suggests:` for `testthat` and `covr`.
* `LICENSE` converted to CRAN's `YEAR` / `COPYRIGHT HOLDER` template
  with the full MIT text moved to `LICENSE.md`.
* `README.md` rewritten with installation instructions, a runnable
  30-second example, and badges.
* Community files added: `CODE_OF_CONDUCT.md`, `CONTRIBUTING.md`,
  GitHub issue and pull-request templates.

## rtfreporter 0.0.19

### Breaking changes

* **Removed all R6 usage.** `rtftable_r6`, `rtfplot_r6`, `rtfreport_r6`,
  and the R6 versions of `rtf_border` and `rtf_table_style` are now
  plain S3 records. The class names lose the `_r6` suffix
  (`inherits(x, "rtftable")` etc.). `Imports: R6` was dropped from
  `DESCRIPTION`.
* The R6 method chains `b$set_top()` / `b$with_top()` /
  `b$apply_override()` / `b$override()` / `style$clone_with()` /
  `style$as_table_border()` are gone. Replacements:
  - `rtf_border_with(b, top = ..., bottom = ..., ...)` for
    non-mutating border derivation.
  - `rtf_table_style_with(style, ...)` for non-mutating style
    derivation.
  - `rtf_table_style(...)` is now called as a function (not
    `rtf_table_style$new(...)`).
* The "share a style instance, mutate it, see every existing table
  reflect the change" pattern no longer applies. Each `rtftable()`
  call snapshots the style fields it needs at construction time. Build
  a new style with `rtf_table_style_with()` and pass it to new tables.

### Other

* Public S3 print methods are now exported for `rtf_border`,
  `rtf_border_side`, `rtf_table_border`, and `rtf_table_style`.

## rtfreporter 0.0.18

* Data section now has **no default borders**. The TFL preset
  `rtf_border_tfl()` no longer paints a horizontal rule under the last
  data row; default borders belong to the column-header block only.
  Callers who want a bottom rule under the last data row can still set
  it explicitly:
  `rtf_table_border(last_row = rtf_border(bottom = rtf_border_side()))`.

# Earlier versions

See [`CHANGELOG.md`](CHANGELOG.md) for the detailed pre-v0.0.18 history.

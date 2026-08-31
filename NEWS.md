# rtfreporter (development version)

### Bug fixes

- **`count_blank_rows = TRUE` let `split = "group_safe"` cut a group across a
  page boundary** (#330). Counting blanks materialises them as empty marker
  rows before the split, and those empty cells changed what
  `group_by = "auto"` detected:

  ```r
  .detect_group_mode(c("A","A","A","B","B","B"))       #> "value"
  .detect_group_mode(c("A","A","A","","B","B","B"))    #> "filled"
  ```

  Under `"filled"` every non-empty cell opens a group, so every row became
  its own group and `group_safe` was free to cut anywhere -- silently, and
  violating `min_group_rows` at the same time. Of 20 shape / `max_rows`
  combinations, 14 split at least one group; none do now. `group_by =
  "auto"` is resolved before the markers go in, and a marker can no longer
  open a group in `"indent"` / `"filled"` mode either.

  `blank_row_first` / `blank_row_end` were never involved and are unchanged.

- **`group_col` given only inside a `page_split_*()` spec did not reach
  `blank_rows` / `collapse_repeats`** (#328). The group column is used by two
  things -- pagination, and the group-dependent row settings
  (`blank_rows = "between_groups"`, `collapse_repeats`,
  `blank_rows_by_change()`) -- but the factories captured it in the returned
  closure's environment, where only pagination could see it. Writing
  `split = page_split_group_safe(group_col = "SOC")` therefore produced
  different output from `split = "group_safe", group_col = "SOC"`, with no
  error and no warning:

  ```
  group_col at top level        blank_rows 4, 8           correct
  group_col inside the split    blank_rows 1,2,...,10      wrong
  group_col nowhere             blank_rows 1,2,...,10
  ```

  The specs now record their `group_col` / `group_by` as attributes and
  `as_rtftables()` adopts them, so both spellings mean the same thing. An
  explicit top-level argument still wins.

- **A cell-level `rtf_border()` passed to `rtftable(border = )` silently
  rendered no borders at all** (#326). `border` there takes a *table-level*
  border (5 zones); an `rtf_border()` is a classed **named list**, so it
  reached the plain-list fallback in `.normalize_table_border()`, which looks
  for zone names, found only `top`/`bottom`/`left`/`right`, and returned a
  border with every zone `NULL`. The output was byte-identical to
  `border = NULL` -- the caller asked for a rule and every rule disappeared,
  with no error and no warning. It is now an error naming the correct
  spelling. The opposite mix-up was already guarded in `col_cell()`, so the
  pair is now symmetric.

- **An rlistings listing is no longer silently mis-read as a plain data.frame**
  (#322). `rlistings::as_listing()` returns a `listing_df`, declared as
  `setOldClass(c("listing_df", "tbl_df", "tbl", "data.frame"))` -- an S3
  tibble, not an S4 `VTableTree`. `.is_rtables_tbl()` therefore rejected it,
  and because it genuinely *is* a `data.frame` it fell through to the
  plain-data.frame branch and rendered with no error at all, discarding the
  listing's own decisions:

  - a column excluded via `disp_cols` **was printed** into the deliverable;
  - key-column repeat suppression was lost, so the key repeated on every row;
  - `main_title()` / `subtitles()` / `main_footer()` / `prov_footer()` were
    dropped.

  `listing_df` is now detected **before** the data.frame branch and read
  through `formatters::matrix_form()` -- the same canonical structure the
  rtables adapter already consumes, and which honours `listing_dispcols()` by
  construction. `as_rtftable()` accepts one too. Add `rlistings` to
  `Suggests:`; it stays out of `Imports:`.

  Note: the suppression arrives already baked into the cell strings, so unlike
  `as_rtftables(collapse_repeats = )` a suppressed key does **not** reprint at
  the top of a new page.

- **A title or footnote block could come back as a `list`** instead of a
  character vector. The pieces were `c()`-ed together, and the same accessor
  returns different types across sources -- `formatters::mf_rfnotes()` is
  `character(0)` for an rtables table but an empty `list()` for an rlistings
  listing -- so one list among the arguments turned the whole result into a
  list. The pieces are now unlisted and coerced before use.

### Internal

- The MatrixPrintForm reader behind the rtables adapter is factored out as
  `.mpf_to_rtftable_kwargs()` and shared with the new rlistings adapter. It was
  already typed on the `MatrixPrintForm` rather than on `VTableTree` -- it
  touches no rtables-specific slot -- so no logic changed;
  `.rtables_to_rtftable_kwargs()` remains as a typed wrapper and its call sites
  and tests are untouched.

### Documentation

- **`cards` + `gtsummary` must be updated together** (#319). The
  hierarchical-AE chunks in `showcase-ae` failed with `The following columns
  are not present: ...` -- not a bug in the code but a **version mismatch**:
  gtsummary's hierarchical verbs hand their ARD to `cards`, which validates it
  in `check_ard_structure()`, and gtsummary 2.5.0 declared only
  `cards (>= 0.7.1)` so R installs it happily beside cards 0.9.0. gtsummary
  2.6.0 declares `cards (>= 0.9.0)` and every chunk runs again. The article's
  **cards + gtsummary** section now says so.

- **Every article now says where to go next** (#317). Measured first: eight of
  the twenty-five articles were **orphans** -- no other article linked to them
  -- and one of those was `rtfreporter-pipes`, which the index itself tells
  readers to open after the Quickstart. Thirteen articles carried no onward
  link at all. Fifteen articles gain a `Where next` block naming two or three
  specific follow-ons and why, so the set now reads as a path rather than a
  pile. **Orphans: 8 -> 0.**

- **The articles point at the new recipes page** (#317). Nothing referenced
  `?rtfreporter-recipes` when it landed; all fifteen do now.

- **The Quickstart's code actually runs.** It set `eval = FALSE` for the whole
  document, so its sixteen chunks were printed but never executed and could
  rot unnoticed. Checked by extracting and running them: **16 of 16 pass**, and
  every render already went to `tempfile()`, so the vignette now evaluates.
  A broken example fails the build instead of reaching a reader.

- **Build artefacts no longer land in the repository root.** Building the
  articles left twelve `.rtf` files there, because several showcase chunks
  render to a fixed filename rather than `tempfile()`. Those, and `docs/`, are
  now ignored. The underlying fixed filenames are tracked in #319.

### Documentation

- **Four complete recipes** (#317). A new `?rtfreporter-recipes` page carries
  runnable end-to-end examples for the four table shapes a clinical report is
  mostly made of -- demographics, adverse events, PK concentrations and a
  laboratory shift -- each ending in a rendered RTF. They are shown together
  because **their arguments barely overlap**: a table of which setting each
  shape actually uses makes the differences visible at a glance, and the page
  names the commonest mistake (reaching for `stub_vars` on a table with no
  hierarchy, or omitting it on one that has).

- **Every reference section now says what it acts on** (#317). The index
  listed 18 groups without stating which object a function takes, so a reader
  landing on `rtf_border()` could not tell the table side from the document
  side. Each description now gives the group's purpose, the object it acts on
  and -- for anything usable in more than one place -- where it may be passed
  and which wins. Measured while writing them: of the value constructors only
  `rtf_page()` and `rtf_default_format()` are document-only, while borders and
  styles are accepted at four levels.

- **Examples now render** (#317). An audit found 39 of 77 help topics carrying
  examples of ten lines or fewer and only 10 ending in a rendered RTF -- most
  pages showed a call but never a result. The `\dontrun{}` around renders that
  needed nothing but a filename is replaced by a real `tempfile()` render, so
  those examples run and `R CMD check` exercises them. `page_split`'s example
  referenced an undefined object and could not be copied at all; it now builds
  its data, and shows the `group_col`-inside-a-factory pitfall. The three
  topics with no examples -- `format.rtftable()`, `summary.rtftable()`,
  `print.rtfreport()` -- have one.

  All 78 topics with examples now run clean.

### New features

- **`stub_spec()` folds every stub setting into one argument** (#314).
  `as_rtftables()` now takes a single `stub =`:

  ```r
  as_rtftables(x, stub = stub_spec(c("SOC", "PT"), layout = "columns"))
  as_rtftables(x, stub = c("SOC", "PT"))          # shorthand for the vars
  ```

  `stub_spec()` takes `stub_cols()`'s own arguments, so the two cannot drift
  apart, and `as_rtftables()` never has to grow another argument when
  `stub_cols()` learns something new. This is the idiom the package already
  uses for `blank_rows = blank_rows_by_change(...)`, `page = rtf_page(...)`
  and `default_format = rtf_default_format(...)`.

  `stub_vars` / `stub_label` / `stub_indent` / `stub_group_summary` are
  **superseded**: they still work and are not deprecated, but they cannot
  reach `layout` or `label_span`, and new settings are added to `stub_spec()`
  only. Passing both forms is an error rather than a silent precedence rule.

- **`layout` and `label_span` now work from `as_rtftables()`** (#314, closing
  the gap left by #312). `layout = "columns"` keeps every column where it is,
  so none of the column-indexed metadata moves and the **source column widths
  survive** -- unlike the merged layout, which cannot carry them through.
  `label_span` is converted to a per-row cell style as the stub is built, so
  it rides the machinery that already slices styles per page and needs no
  re-indexing through the split.

### New features

- **`stub_cols()` gains `layout` and `label_span`** (#312), two row-heading
  layouts added to the existing verb rather than to a new one.

  `layout = "columns"` **keeps** the hierarchy columns, in their original
  positions and with their own headers. A group value moves onto its own row
  and is blank on that group's member rows, whose leaf is indented in the leaf
  column:

  ```
  group  label     HOGE
  grp1
           label1  1 (1.11)
           label2  2 (2.22)
  grp2
           label3  3 (3.33)
  ```

  Nothing is merged -- the group value is simply displayed in its own column.

  `label_span = TRUE` renders each group's label row of the **merged** layout
  as one cell spanning the table, instead of a stub cell followed by empty
  ones. `FALSE` by default, so the historical look is unchanged. A
  group-summary row folded onto a label row is never spanned: it carries
  statistics in the other columns.

  The two options are contradictory in combination and error rather than
  silently ignoring each other, as does `label` (which names a merged column)
  with `layout = "columns"`.

  Both layouts come out of the row walk `stub_cols()` already ran, so
  `group_summary`, `group_by = "indent"` detection, `blank_rows =
  "between_groups"` and the group-aware page splits keep working unchanged.
  The span is carried as a per-row cell style, which means it rides the
  machinery that already slices styles per page and re-indexes them through
  `paginate_cols()` -- a column page merges exactly its own columns, with no
  bookkeeping of its own.

  Not yet wired into `as_rtftables(stub_vars = )`, which still builds the
  merged layout; use `stub_cols()` ahead of it for now.

### New features

- **`rtf_tables(auto_title = TRUE)` prints each page's name above its table**
  (#310). The counterpart to `auto_section`, which puts the same name in the
  section's running header: `auto_title` appends it as the **last row of that
  table's title block**, left-aligned, so it sits immediately above the table.

  ```r
  pages <- as_rtftables(df, split = "by_value", group_col = "COHORT")
  doc   <- rtf_document() |> rtf_tables(pages, auto_title = TRUE)
  ```

  Titles already present are kept and the name goes after them -- whether they
  came from `titles =` or were carried on the object as the `rtf_titles`
  attribute by `as_rtftables()`. A table with no titles gets the name as its
  whole block. `title_label_align` moves it (`"left"` by default, mirroring
  `section_label_align`).

  Both title formats behave identically, and neither renderer needed changing:
  `title_format = "text"` and `"table"` already resolve their rows through the
  same normaliser, which honours a per-row alignment.

  Gated exactly as `auto_section` is -- a named list, and only the elements
  with a non-empty name. The two compose, putting the name in both places.

### New features

- **An optional package below its declared minimum now warns** (#308).
  `DESCRIPTION` has always constrained `gt (>= 0.9.0)`,
  `gtsummary (>= 1.7.0)`, `formatters (>= 0.5.0)`, `rtables (>= 0.6.0)`,
  `flextable (>= 0.9.0)` and `huxtable (>= 5.0.0)`, but the runtime guards
  asked `requireNamespace()` only. An installation that satisfied "is it
  there?" but not the version sailed past and failed later inside the
  conversion, where the message said nothing about the version.

  Every adapter guard now goes through one helper. Missing is an error, as
  before and with the same wording; too old is a warning naming the installed
  version, the required one and the upgrade command, after which the call
  proceeds -- an older release often still handles the simple cases, and a
  hard failure over a *Suggests* constraint would be the worse trade. The
  warning fires once per package per session, so a guard inside a loop does
  not flood the console.

  The minimum is read from rtfreporter's own `DESCRIPTION` at run time, so the
  numbers live in exactly one place: adding a constraint there is the whole
  change, and no version literal is repeated in R code.

### Bug fixes

- **A command template with an unsupplied placeholder is now an error**
  (#306). `.cmd_fmt()` substituted only the keys it was handed and left
  everything else verbatim, so a template that gained a slot the renderer did
  not fill produced RTF carrying the literal `{name}`. Every slot a template
  declares must now be supplied. Detection is narrow enough not to fire on
  RTF's own groups, which always open with a control word (`{\header`,
  `{\fonttbl`) while escaped braces in text are `\{`.

- **The pharmaverse catalog examples are regenerated** (#306). All six shipped
  with `{orientation_cmd}`, `{header_dist_twips}` and `{footer_dist_twips}`
  in their preamble -- groups of plain text sitting in the document body ahead
  of `\sectd`, which a reader renders as visible text on the first page.
  `\headery` and `\footery` were left without their numeric argument and the
  document-level `\landscape` was missing. Their margins move 864 -> 1080
  twips, the generator setting none and the page defaults being 0.75in; page
  counts are unchanged.

- **The `data-raw/` generators render through the source tree** (#306). Each
  now `source("data-raw/_load.R")`, which attaches the package with
  `pkgload::load_all()` instead of `library(rtfreporter)`. A committed example
  is a derived file of the code beside it, not of whatever build happens to be
  installed -- that mismatch is what put the placeholders in the examples in
  the first place.

### Bug fixes

- **`set_decimal_split()` pads, floors and caps each half's width** (#304).
  The ratio came from the measured glyph counts alone, so a column of `0.0000`
  values measured 1 against 5 and gave the integer half one sixth of the
  column -- at 1200 twips, 200 twips for a single digit, hard against the cell
  edge. Each half now gets a padding allowance and a floor, in character-width
  units:

  ```
  w_eff = max(measured + pad_chars, min_chars)
  ```

  The defaults `pad_chars = c(1, 1)` and `min_chars = c(4, 6)` give each half
  one character of breathing room over a three-digit integer part and a
  five-character decimal part, so anything at or below that size holds a
  `4 : 6` baseline and anything larger scales from its own measurement. A
  `0.0000` column goes from `r = 0.167` to `0.400` (480 twips instead of 200,
  at a 1200-twip column).

  Past `max_chars` -- the measured halves together, default `10` -- the
  allowance is dropped and the raw measured proportions are used however
  lopsided. A column that long needs every twip it has, and reserving room it
  does not use would squeeze the digits it does: `123.4567` data measures 3
  against 8 and keeps its own `3/11 = 0.273` rather than being widened to
  `4/13`.

  The floor applies per half, so a column with **fewer** than five decimal
  characters reserves the baseline six on the right and its integer half
  narrows relative to before: `1439.5 / 88.012` style data moves from
  `4/7 = 0.571` to `5/11 = 0.455`. Pass
  `pad_chars = c(0, 0), min_chars = c(0, 0)` for the previous raw ratio,
  `max_chars = Inf` to keep the allowance at any width, or an explicit `ratio`
  to fix the split point outright.

  `inst/rtf-examples/pk-concentration.rtf` is regenerated accordingly. That
  file was also stale against the footnote rework (#297) and now renders the
  footnote band as an independent borderless table, as current code emits.

### New features

- **`rtf_tables()` accepts `font_size_half_points` and `font`** (#299), so a
  table's typography can be set from the document side rather than only on the
  table. `font` and `font_size_half_points` had landed on `rtftable()` (#292,
  #293) but not here, which left the table as the only page element whose
  typography lived on the **content object**: the header / footer band carries
  its own via `rtf_header()` / `rtf_footer()`, and the title / footnote blocks
  via `rtf_titles()` / `rtf_footnotes()` on the document. Both levels now work,
  as `row_height_twips` already did — set it on `rtftable()` when building the
  table, override it from `rtf_tables()` when placing it in a document, where
  the explicitly-passed value wins. Keeping them in one place also keeps the
  #292 coupling readable: a size given at either level still recomputes the row
  height unless a height is given too.

- **Per-element font family, on a variable-length font table** (#293, closing
  the ground of #44). The document carried exactly one font: the resource
  template was fixed at a single `{\fonttbl{\f0 ...}}` entry and the renderer
  only ever read `font_table[[1]]$name`, so there was no `\f1` to select. The
  table is now built from every family the document mentions, and
  `rtftable()`, `rtf_header()` / `rtf_footer()` and `rtf_titles()` /
  `rtf_footnotes()` each take `font`. A family not already declared is added
  automatically — the same resolve-and-declare the colour table already does:

  ```r
  rtf_document() |>
    rtf_tables(rtftable(df, font = "Arial")) |>
    rtf_titles(list("Table 14.1.1"), font = "Calibri")
  #> {\fonttbl{\f0\fnil\fcharset0 Courier;}{\f1\fnil\fcharset0 Calibri;}
  #>           {\f2\fnil\fcharset0 Arial;}}
  ```

  Index 0 stays the document default, so an element that asks for nothing —
  or asks for the default by name — emits no `\f` at all and a single-font
  document renders exactly as before. `font` pairs with
  `font_size_half_points` from #292, emitting `\f1\fs24` together.

  This completes the three-part per-element style work: widths (#291), size /
  height / markup / alignment (#292), and family here.

### Breaking changes

- **The footnote block now uses the page footer's mechanism, as a table of its
  own** (#296). Two visible changes:

  - **No separator rule by default.** The footnote used to draw a top rule on
    its first row, and because `rtf_border_tfl()` sets only the *header's*
    borders, that rule was the only line under the table. It is gone; ask for
    it with `rtf_footnotes(border = rtf_border_top())`.
  - **Per-row styling is removed.** The
    `list(text = , bold = , italic = , underline = , color = , border = )` row
    form no longer works. Style is set for the block instead, via
    `rtf_footnotes(font_size_half_points = , row_height_twips = , markup = ,
    align = , border = )`.

  In exchange a footnote row may now be `c(l = , c = , r = )` — up to three
  cells positioned left / centre / right, exactly as `rtf_footer()` takes them
  — so "Source: ADSL" on the left and a run date on the right finally fit on
  one line. A bare string lands in whichever slot `align` selects, so plain
  text stays left-aligned. Page-number tokens (`{PAGE}`, `{AUTO_PAGE}`, …) are
  expanded now, as they are in the footer.

  The block is also emitted as an **independent table**: RTF merges
  consecutive `\trowd` runs that no paragraph separates, so the footnote was
  previously the body table wearing different `\cellx` values. A separating
  paragraph makes the `footnote_width` of #291 genuinely its own.

  `footnote_format = "text"` still renders plain paragraphs; a
  `c(l = , c = , r = )` row cannot be expressed that way and raises a clear
  error pointing at the table form.

### Bug fixes

- **`rtf_header(markup = )` / `rtf_footer(markup = )` had no effect** — the
  argument was added in #292 but never reached the renderer, so `^{a}` in a
  header stayed literal. The band honours it now, inheriting the document
  setting when unset.

### New features

- **Per-element font size, row height, markup and block alignment** (#292).
  `rtf_document(default_format = )` stays the baseline; the table body
  (`rtftable(font_size_half_points = )`), the header / footer band
  (`rtf_header()` / `rtf_footer()`) and the title / footnote blocks
  (`rtf_titles()` / `rtf_footnotes()`) may each override it, and anything left
  `NULL` is inherited. **Font size and row height resolve together**: an element
  that sets a size without a height has the height recomputed from that size,
  rather than inheriting one chosen for a different size, while an explicit
  height always wins — so `font_size_half_points = 24` alone yields `\fs24`
  with the matching `\trrh290`, and a document `row_height_twips` no longer
  leaks across a size change. `row_height_twips = 0` keeps meaning "automatic".
  `align` on a title or footnote sets that block's default row alignment; a
  per-row `align` still beats it, and block placement continues to follow the
  content. `markup` can now be switched on for one block alone. Nothing changes
  when nothing is set — asserted byte-identical.

- **Per-element widths for the header / footer, table and footnote** (#291),
  in one vocabulary: `"content"` (follow the table body), `"page"` (the
  writable width, margins excluded), a fraction in `(0, 1]` of it, or twips.
  `rtf_default_format()` gains `title_width` / `footnote_width` (with
  `rtfreporter.title_width` / `rtfreporter.footnote_width` behind them), and
  `rtf_header()` / `rtf_footer()` gain `width` — the older `width_twips` stays
  as the absolute form and wins when both are given. The table body already had
  its own arguments. **Every default is unchanged**, and output is
  byte-identical when nothing is set. Notably `footnote_width = "page"` is the
  way to reach the full page width **and** keep the separator rule:
  `footnote_format = "text"` widens the block but renders plain paragraphs,
  which cannot draw a rule (a row-level `border` is silently ignored there).
  It also matters with `paginate_cols()`, where a narrow column block would
  otherwise drag the footnote in with it.

### Breaking changes

- **`paginate_cols()` now recomputes relative widths from a page-1 ratio unit**
  (#289). Splitting a `col_rel_width` table used to scale each page down to the
  kept columns' share of the whole, so a block of four visits out of eight
  filled only 64% of the sheet — the ratios were preserved but the table was
  simply small. A ratio states a proportion *within the page*, so the split now
  **fixes the twips-per-ratio unit on page 1 and reuses it on every page**:
  page 1, and any block with the same ratio total, fills the sheet, while a
  ratio-1 column stays the same width throughout and a shorter final block
  yields a shorter page. The new `width` argument selects this — `"fill"` is the
  default, `"keep"` restores the previous behaviour. Absolute
  `column_widths_twips` are unaffected (there is nothing to rescale), so the
  shipped PK example is unchanged. A block totalling **more** ratio than block 1
  would run off the sheet under `"fill"`; `paginate_cols()` warns and names it —
  put the widest block first, or use `"keep"`.

### New features

- **`fmt_signif()` / `fmt_round()` / `fmt_numeric()` format numeric columns for
  display** (#287). A column still numeric when it reaches the renderer was
  converted with `as.character()` — 15 digits, scientific notation outside
  `1e-4 .. 1e5`, and, worst for a deliverable, **dependent on the session's
  `options(scipen)`** (`as.character(123456)` is `"123456"` under
  `scipen = 100` but `"1.23456e+05"` under `scipen = -100`). These functions
  turn numbers into the report's text beforehand, deterministically.
  `fmt_signif()` counts **total printed digits including the integer part** —
  the convention a SAP means by "4 significant digits", not `signif()`: `0` →
  `"0.000"`, `10.2` → `"10.20"`, `103.4` → `"103.4"`, `23.4463` → `"23.45"`.
  Values below 1 keep true significant figures by default (`0.0004567` stays
  `0.0004567`, which matters near a limit of quantitation);
  `small = "fixed"` applies the integer-counting rule throughout. Decimals are
  recomputed when rounding grows the integer part (`99.995` → `"100.0"`), and
  an integer part longer than the request simply prints whole (`12345.6` →
  `"12346"`). `fmt_round()` fixes the decimal places instead. Both offer
  `rounding = "sas"` (half away from zero, with an ulp correction so `2.675`
  cannot fall the wrong way) beside the default `"r"` (`base::round()`,
  banker's). `fmt_numeric()` applies either across a data frame, with `by`
  naming a **carrier column** — which may be the row-heading column itself,
  matched after trimming — so one column can carry a different format per
  statistic. `NA` and `NaN` both render as the `na` string (default `""`);
  character columns are taken as already formatted and are never touched.

### Documentation

- **The PK concentration example gains a running header with page numbers**
  (#281), in the same shape as the pharmaverse examples: two administrative
  rows (`CONFIDENTIAL`, `Protocol: RTF-101` / `Page {PAGE} of
  {TOTAL_PAGES}`) followed by the table's title block. The titles move out of
  `rtf_titles()` and into the header so they repeat on all six pages, which is
  what a table split across two axes needs — and the reader finally gets a page
  number. Footnotes stay below the table; the example deliberately has no page
  footer.

- **The PK concentration example now actually needs its column split** (#279).
  Sized with `col_rel_width`, the table was exactly as wide as the sheet --
  relative widths are normalised across the page by `.compute_cellx()`, so a
  table sized that way always fits and `paginate_cols()` had nothing to do; the
  two pages came out 73% and 47% full. It is now sized with **absolute widths
  from `auto_col_widths()`** (stub 2021 twips, each visit 1804) and the study
  extended to **twelve visits**, which makes the full table 23669 twips --
  **1.73x too wide for the 13680 writable twips** -- and each of the two visit
  blocks 12845, or **94% of the page**. `inst/rtf-examples/README.md` gains an
  entry for the file and names the two screenshots the example needs
  (`pk-concentration.png` for page 1, `pk-concentration-cols2.png` for page 2),
  since a single page cannot show a horizontal split. The article explains why
  `auto_width = TRUE` is not used here: it clamps the total to the writable
  width when the natural size exceeds it, squeezing the columns back onto one
  page.

### New features

- **Worked example: a PK concentration summary with both pagination axes**
  (#277) -- `data-raw/gen_pk_conc.R` builds a pharmacokinetic concentration
  table with the **visits across the columns**, so it is too wide *and* too
  tall for one page, and renders it into
  `inst/rtf-examples/pk-concentration.rtf`. It combines `set_decimal_split()`
  (one column, two cells) with row pagination and `paginate_cols()`: three time
  bands by three visit blocks, each page carrying the nominal-time stub at
  identical column widths. A new integration test covers the interaction the
  two features could not test separately -- `paginate_cols()` re-indexing
  `decimal_split$cols` onto each page's columns -- plus the `BLQ`, `<0.500`,
  integer-`n` and `Min, Max` cell kinds. Documents that the split decision is
  made **per page**, so an all-`BLQ` visit column is left unsplit on that page
  while the others split it.

- **`paginate_cols()` paginates a table horizontally, by column** (#275) --
  the counterpart to the row pagination `as_rtftables()` performs. A table too
  wide for the page is cut into column blocks (`at =` names the columns to cut
  **before**, or `cols =` states the blocks), with the row-heading column(s)
  repeated on every page. Row splitting happens first and the column split is
  applied to its result, and the **row page is the outer level**: a row band
  sweeps every column block before the next band starts, so the reader goes
  across the table first and then down it (two row pages by three column
  blocks give `row1/col1`, `row1/col2`, `row1/col3`, `row2/col1`, …).
  Positions refer
  to the **final printed columns**, matching `set_col_header()`. The carry set
  defaults to the table's `row_title`; `carry =` overrides it. **A column is
  the same width on every page it appears on** in all three width modes --
  each page's table width is scaled to the kept columns' share so relative
  widths do not stretch back out to the margins, which means a narrow block
  gives a narrow page. Spanning header cells are clipped per page; a cut
  inside a spanning group is allowed by default (the group label repeats) and
  `allow_span_break = FALSE` rejects it. Page names are carried through
  unchanged, so column pages stay in one `auto_section` section. Works on an
  rtftable or an `as_rtftables()` page list.
- **`set_decimal_split()` lines a column up on its decimal point** (#273) -- a
  post-hoc verb that renders the selected body column(s) as **two adjacent RTF
  cells**: the part before the first decimal separator (right-aligned) and the
  separator plus everything after it (left-aligned). The points then line up
  exactly whatever the font, instead of merely right-aligning the last
  character. It is an **output option, not a data transformation**: the data
  frame is never rewritten, the table's total width is unchanged (only the
  column's own width is divided, sized automatically from the widest left and
  right part, or by an explicit `ratio`), and the column-header block still
  renders over the original single column. A relational or sign prefix
  (`<0.001`, `>=0.5`, `-0.7`) travels with the left half and a suffix
  (`45.6%`, `12.3^{a}`) with the right, so both hang outside the aligned point.
  Free text (`"n (%)"`) and -- unless `include_compound = TRUE` -- compound
  values (`"12.3 (4.56)"`) are rendered as one cell across the pair, exactly as
  today. Works on an rtftable or an `as_rtftables()` page list. The console
  preview does not show the split.

- **`set_header_cell()` sets/merges individual column-header cells** (#271) --
  a post-hoc verb that places one or more [col_cell()]s (by column name or
  position, spanning via `c(a, b)`, with borders / alignment / decorations)
  into a chosen header `row` of a built table, **keeping the other cells
  intact**. Merging currently-separate cells into one spanning cell is the core
  use. Works on an rtftable or an `as_rtftables()` page list. A target span must
  align to existing cell boundaries (it will not split an existing span).

### Breaking changes

- **`collapse_repeats()` is now a post-hoc verb on a built table** (#265) -- its
  first argument is an [rtftable()] or a **list of pages** (an `as_rtftables()`
  result), not a bare data.frame. On a page **list it collapses each page
  independently** (runs reset at page breaks), so
  `collapse_repeats(as_rtftables(x, ...), cols)` now matches
  `as_rtftables(x, collapse_repeats = cols, ...)` exactly. `cols` resolve against
  the table's final columns. The data.frame signature was removed (a `.default`
  method errors with guidance to build the table first).

### New features

- **Unified blank-row group recognition** (#263):
  - `blank_rows_by_change()` gains a **`group_by`** argument (`"value"` default,
    plus `"indent"` / `"filled"` / `"auto"`) -- the same group detection the
    pagination splits and `blank_rows = "between_groups"` use. The standalone
    spec is now a full-power equivalent that carries its own `group_by`.
  - `blank_rows_by_change()` and `blank_rows_by_rule()` **now work inside
    `as_rtftables()` / `paginate()`** (they previously errored with
    *"Unrecognised blank_rows entry"*); they are resolved per page.
- **`collapse_repeats()` post-hoc verb** (#261, reworked in #265) -- blanks
  consecutive repeated values in a built [rtftable()] or a list of pages;
  hierarchical suppression; suppressed cells become `NA`; no rows removed. See
  *Breaking changes* for the signature.
- **`col_header_from_names()` builds a spanning header from delimited column
  names** (#259) -- the reconstruction `as_rtftables()` applies automatically to
  a plain data.frame (e.g. `"Drug A____N"` / `"Drug A____Mean"` -> a `Drug A`
  span over `N` / `Mean`) is now a public helper. Pass a character vector (or a
  data.frame) and an optional `sep`; get back an `rtf_col_header` ready for
  `set_col_header()` or `rtftable(col_header = )`. (Internally, the
  `.split_names_to_col_header()` logic moved from `as_rtftables.R` to
  `col_header.R`; no behaviour change.)

### Documentation

- **`as_rtftables()` header / styling `...` arguments documented as superseded**
  (#257) -- a new *Superseded arguments* section steers users to the dedicated
  post-hoc verbs (`set_col_header()`, `style_cols()`, `style_header()`,
  `add_header_row()`) for anything beyond a simple one-row header. The arguments
  keep working unchanged (no warning); this is documentation only.

### Internal

- **`as_rtftables.R` split into focused files** (#255) -- the self-contained
  internal helpers moved out of the ~1337-line orchestrator into
  `R/col_resolve.R` (`.resolve_col_indices()`), `R/sort_body.R` (sort helpers),
  `R/col_drop.R` (`drop_cols` hiding + metadata reindexing), and
  `R/stub_vars.R` (`stub_vars` stub build + reindex). No behaviour change.

### New features

- **`rtf_header_source()` deparses a table's column header back to editable
  `rtf_col_header()` source** (#253) -- render the current header as name-based
  `rtf_col_header(...)` text, tweak the labels/spans, and re-apply with
  `set_col_header()`. Three verbosity `level`s (`"explicit"` / `"default"` /
  `"all"`); a `snippet = TRUE` mode wraps it in a pipeable
  `set_col_header(...) |> style_zone(...)` that also reproduces header text
  alignment and header-zone borders; and `add_span_level = TRUE` previews adding
  a hierarchy level (the `stub` column stays, the rest are bundled under one
  spanning cell to label).
- **`set_col_header()` configures a column header against the final printed
  table** (#251) -- a new post-hoc verb (sibling of `style_header()` /
  `add_header_row()`) that sets the whole column header of a finished
  `rtftable`, or every page of an `as_rtftables()` list, resolving each cell by
  **column name** or **visible position** (1 = first printed column). Because it
  runs on the finished table there is no intermediate layout to reason about --
  no hidden `drop_cols`, no `stub_vars` position shifting. Pair it with the new
  **`rtf_columns()`** accessor (the final body column names) to verify names and
  order before writing a header. `add_header_row()` now also resolves cells by
  name.
- **`col_header` label rows can be placed by column name** (#248) -- a *named*
  character vector such as
  `c(row_label = "  Category", g1 = "G1", g2 = "G2", g3 = "G3", t = "Total")`
  is now resolved against the data columns by name instead of by position, so a
  header no longer lands on the wrong columns when the data column order differs
  from what the author assumed. A named element targets that column (unknown or
  duplicate name is an error); an unnamed element is matched as a column name if
  one exists, otherwise placed positionally; two elements resolving to the same
  column is an error. A fully-unnamed vector keeps the exact legacy positional
  behaviour.
- **`col_cell()` spanning cells can address columns by name** (#248) -- `pos`
  now also accepts column-name string(s), e.g. `col_cell(c("g1", "g3"), "Drug")`
  or `col_cell("total", "Total")`, resolved against the data columns (unknown
  name is an error). Numeric `pos` is unchanged.

### Breaking changes

- **A user-supplied `col_header` in `as_rtftables()` is now resolved against the
  final printed columns** (#251), not the pre-`drop_cols` / post-`stub_vars`
  intermediate body. Headers written in the old coordinates must be renumbered
  to the visible columns (e.g. with a hidden first column, a span over the six
  data columns changes from `col_cell(c(3, 8), ...)` to
  `col_cell(c(2, 7), ...)`, or better, address columns by name). `col_spec`,
  `col_rel_width`, `row_title` and `cell_styles` are unchanged (still reindexed
  from the input coordinates); adapter-derived (automatic) headers are also
  unchanged.

### Documentation

- **New article: "A tfrmt table with several stub columns: one for sections,
  the rest as a stub"** (#246) -- a runnable recipe for a tfrmt
  `location = "column"` table whose three stub columns are split one way for
  `auto_section` sections (lab test code) and merged the other way into a
  two-level indented stub (a fixed heading + Toxicity Grade).

### Bug fixes

- **`blank_rows = "between_groups"` now honours `group_by`** (#263) -- it always
  used `"auto"` detection regardless of the `group_by` argument; `set_blank_rows()`
  gains a matching `group_by` argument. (`group_by` defaults preserve prior
  output.)
- **`as_rtftables(split = "by_value", stub_vars = )` now splits by `group_col`
  *before* building the stub** (#244) -- for a `by_value` split each group is an
  independent section, so the indented stub is now built per page after the
  split. Previously the stub was built once on the whole body, which (a)
  fragmented the split to one page per row and produced nameless `group_0` pages
  when the outer `group_col` was blanked on the inserted label rows, and (b)
  collapsed a constant intermediate hierarchy level (e.g. `LBTOX_LBL / group1 /
  label` with a fixed `group1`) into a single stub label row that spanned every
  group. Put the inner hierarchy in `stub_vars` and the outer level in
  `group_col`. Every other split keeps the previous stub-then-paginate order.

### New features

- **`as_rtftables()` gains `stub_vars` (plus `stub_label` / `stub_indent` /
  `stub_group_summary`)** (#242) -- folds [stub_cols()] into the extraction
  pipeline so a clinical indented stub can be built **after** a table is pulled
  out of its source, on any input that carries the hierarchy as separate
  columns (a plain `data.frame`, a `gt(df)`, a multi-group-column table, or a
  tfrmt `row_grp_plan(location = "column")` table) -- no manual extract →
  `stub_cols()` → re-feed round-trip. The named columns are merged into one
  stub column at position 1; `drop_cols` / `group_col` / `sort_by` and the
  `group_by = "indent"` group-aware splits then operate on the reshaped,
  post-stub columns. Position-indexed metadata (`col_header` including spanning
  headers, `col_spec`, `col_header_align`, per-cell `cell_styles`) is reindexed
  automatically. `stub_vars = NULL` (default) is a no-op, so existing calls and
  direct `stub_cols()` usage are unaffected.

- **`print()` on an `rtftable` now renders a visual preview of the table**
  (#229) -- the laid-out cells with column (and spanning) headers, per-column
  alignment, and horizontal rules drawn where the table's border zones are
  actually set, so the console shows what the RTF will look like (much like
  printing a `gt_tbl`, `rtables` `VTableTree`, or `gtsummary` object) instead
  of only metadata. Long bodies truncate with a "… N more rows" note; an
  internal multi-table `rtftable` renders its first table. The rendered lines
  are available on their own via the new `format()` method, and the previous
  compact metadata block via the new `summary()` method (it is still appended
  below the table by `print()`). Box-drawing rules degrade to ASCII in a
  non-UTF-8 locale or under `options(rtfreporter.print_ascii = TRUE)`.

- **`stub_cols()` now folds AE-style group-summary rows onto the group label
  row** (#231). When a hierarchy row's **leaf** is `NA` / `""`, or repeats its
  deepest non-empty parent value, that row supplies the group's own summary
  statistic (e.g. a SOC-level adverse-event count); its cells are placed on the
  group's label row instead of producing a valueless label row plus a phantom
  indented leaf. Controlled by the new `group_summary` argument (default
  `c("empty", "parent")`; set `"none"` for the previous behaviour). Demographic
  tables -- where the leaf is a statistic name such as `"n"` / `"Mean (SD)"`,
  never `NA` and never equal to the parent -- are unchanged, and the folding
  nests at intermediate levels for `vars` deeper than two.

### Behavior changes

- **`as_rtftables()` / `as_rtftable()` now carry a gt table's explicit
  `tab_style()` styling** (#223) -- the new gt `"styles"` `read_meta` token,
  enabled by the default `read_meta = TRUE`. Carried, because the `rtftable`
  class can hold them: `cell_borders()` and the `cell_text()`
  bold / italic / underline / align properties on column spanners, column
  labels and body/stub cells, plus body text colour (so e.g.
  `gtsummary::bold_labels()` now arrives bold). A border/underline on a
  column-label cell promotes the label row to single-column cells -- the
  same mechanism `style_header()` uses. The `style_*()` verbs still override
  whatever was read (adapters populate first; verbs win per side / per
  field). *Not* carried: `cell_fill()`, fonts/sizes, header text colour, and
  gt's **theme** borders (`tab_options()`) -- rtfreporter's `border` presets
  keep governing the frame. Pass `read_meta = FALSE` or a token list without
  `"styles"` for the previous behaviour. **Transparent** gt borders are
  likewise skipped (#226): tfrmt overlays transparent borders everywhere to
  hide gt's theme lines, and an invisible border carries no author intent --
  a plain `tfrmt::print_to_gt()` table converts exactly as before.

- **`as_rtftables()` / `as_rtftable()` now read a plain data.frame's column
  `label` attributes** (the haven / labelled / xportr convention) **as header
  labels** (#210) -- the `"labels"` `read_meta` token, enabled by the default
  `read_meta = TRUE`. A labelled data.frame that previously showed its raw
  column names now shows the labels; pass `read_meta = FALSE` (or drop the
  attributes) for the old headers. Columns without a label keep their name,
  the display names feed the `header_sep` spanning reconstruction, and an
  explicit `col_header` still always wins. Token resolution is lenient for
  data.frames (unknown tokens are ignored) so a `list()` mixing table objects
  and data.frames can share one `read_meta`.

- **`rtfplot()` / `rtf_figures()` now embed a figure at its native size (100%)
  by default** (#208), instead of scaling it to `min(writable width, 9in)`. The
  display size is the image's pixel dimensions divided by its embedded DPI (read
  from the PNG `pHYs` chunk / JPEG JFIF density) -- e.g. a 2500 x 1438 px image
  at 300 DPI embeds at 2500/300 x 1438/300 in. Files with no resolution metadata
  assume the new **`rtfreporter.figure.default_dpi`** option (factory `96`). An
  explicit `width_twips` / `height_twips` still wins, and the native size is not
  auto-capped to the page -- pass `width_twips` to shrink an over-wide figure.

### New features

- **Post-hoc styling verbs** (#218): `style_header()`, `style_cols()`,
  `style_body()`, `style_zone()` and `add_header_row()` restyle an
  already-built `rtftable` -- or every page of an `as_rtftables()` list at
  once -- addressing header rows, body rows and columns by position, so a
  table read with `read_meta = TRUE` keeps everything the adapter extracted
  and only the requested detail changes:

  ```r
  pages <- as_rtftables(gt_obj, read_meta = TRUE) |>
    style_header(row = 2, cols = 2:4,
                 border = rtf_border(top    = rtf_border_side("single"),
                                     bottom = rtf_border_side("none"))) |>
    style_body(rows = ~ label == "Mean", bold = TRUE)
  ```

  Merge semantics everywhere are the border rule generalized: last writer
  wins, per side / per field. `style_body(rows = )` takes positions, a
  logical vector, a predicate function or a one-sided formula (evaluated in
  the body data); on a page list only the predicate forms are allowed, since
  page-local row numbers are ambiguous. A `border`/`underline` request on a
  character label row transparently promotes it to an equivalent cell row
  (render-equivalence is pinned by a test). See the new *Level 4* section of
  the Borders article.

- **Per-cell body alignment and borders** (#221): `cell_styles` gains
  `align` (`NA` = inherit the column alignment) and `border` (a list of
  `rtf_border()` per column, merged side-by-side on top of the row's
  resolved zone border -- so `rtf_border_side("none")` erases a zone rule),
  and `style_body()` gains the matching `align = ` / `border = ` arguments.
  This completes the invariant that every visual attribute the data-row
  renderer takes from `col_spec` or the border zones can be overridden per
  cell through the one `cell_styles` channel, with the same
  last-writer-wins-per-side rule. The classic uses are a rule under a
  summary/total row and one row's alignment flipped:

  ```r
  tbl |> style_body(rows = ~ Item == "Total",
                    border = rtf_border(bottom = rtf_border_side("single")))
  ```

  Per-row row height and cell padding are deliberately **not** added: TFL
  conventions keep them uniform, and they are already settable per table and
  document-wide; vertical cell padding remains with the parked 4-direction
  `\clpad*` work (#125).

- **`as_rtftables()` now accepts a `gt_group`** (#214) -- gt's multi-table
  container from `gt::gt_group()` / `gt::gt_split()`, and what tfrmt's
  `print_to_gt()` returns when the spec has a `page_plan`. Each member table
  becomes its own page set (expanded via the exported `gt::grp_pull()`), so a
  paginated tfrmt render can be handed over directly. `as_rtftable()` unwraps
  a single-member `gt_group` and errors informatively for a multi-member one.
  (Previously a `gt_group` slipped into the list branch, which iterated its
  internal slots and failed with a misleading `"got 'logical'"` error.)

- **gtsummary `tbl_split` containers are now supported by design** (#215) --
  `tbl_split_by_rows()` / `tbl_split_by_columns()` output is unwrapped
  explicitly into its member gtsummary tables, one page set per member
  (before, it only happened to work through the generic list branch, with no
  tests or documentation guarding it). `as_rtftable()` unwraps a
  single-member `tbl_split` and errors informatively for a multi-member one,
  mirroring the `gt_group` behaviour.

- New **`stub_cols()`** (#210): finish a tidy "hierarchy columns + statistic
  columns" data.frame for clinical display by merging the hierarchy columns
  (parent first, leaf last) into **one indented stub column** -- each parent
  value becomes a full-width label row (the other columns render blank) and
  the leaf rows are indented with non-breaking spaces. The output is a plain
  data.frame, so `as_rtftables()`'s indent-based group detection, the
  group-aware splits (`(Cont.)`, widow/orphan control) and
  `blank_rows = "between_groups"` all work on it unchanged. An `NA` / empty
  parent cell emits no label row and no indent, keeping summary rows (e.g.
  "Any adverse event") flush left. See the *Importing tables* article, which
  also maps `reporter::create_table()` spec features to their rtfreporter
  equivalents.
- `as_rtftables()` / `as_rtftable()` gain **`header_sep`**, reconstructing a
  **spanning (multi-row) column header** from a plain data.frame's delimited
  column names. The default recognizes both `"____"` (from
  `ydisctools::pivot_stats_wider()`) and `"___tlang_delim___"` (tfrmt's column
  delimiter); pass your own separator(s) to override, or `NULL` to disable.
  Adjacent columns that share a label and ancestor path merge into a spanning
  cell; columns without a separator (id columns) keep their name on the leaf
  row. With `"____"` a doubled separator (`"________"`) inserts a blank cell at
  that header level. Only applies to plain data.frames -- gt / gtsummary /
  rtables / tern / flextable / huxtable already carry real spanning metadata.
  See the *Importing tables* article.
- New **`combine_sections()`** assembles several converted tables -- each an
  `rtftable` or an `as_rtftables()` page list -- into one flat, named list ready
  for `rtf_tables(..., auto_section = TRUE)`, so each argument renders as **one
  clean RTF section** (its name placed on the first page, the remaining pages
  falling through into the same section). This is the tidy way to section
  multi-page or row-grouped tables (gt / gtsummary / rtables / tern), where the
  grouping is not a body column; for column-grouped bodies, `as_rtftables(x,
  split = "by_value")` already names pages per group. See the *Headers and
  footers* article.

- `print()` methods for the three central objects -- **`rtftable`**, **`rtfplot`**
  and **`rtfreport`** -- now show a concise, reporting-oriented summary instead of
  dumping the raw list (#196). For an `rtftable`: body dimensions, the column
  (leaf) labels, row-title column(s), border and width mode, any attached title /
  footnote line counts, and a short preview of the rendered body. For an
  `rtfplot`: the image's native pixel size versus the embedded display size (in
  twips and inches) and alignment. For an `rtfreport`: page / section counts, the
  page geometry, and the font / colour table sizes.

### Behavior changes

- **Default page margins are now based on the FDA eCTD *Portable Document Format
  (PDF) Specifications* minimums** (#204, #206): a uniform **`0.75"`** on all four
  sides (was `0.9"` top/bottom and `0.6"` left/right). The bottom leaves room for
  an overall page-number footer. A wide TFL that does not fit the 0.75" left/right
  can trade horizontal room by narrowing them to eCTD's `0.5"` (still clears the
  0.375" floor) -- per document via `rtf_document(page = ...)` / `rtf_page()`, or
  site-wide via the `rtfreporter.page.margin_left_in` / `.margin_right_in`
  options. See the *Page setup* article ("Changing the margins").
- **The header / footer band distance (`\headery` / `\footery`) now defaults to
  the full top / bottom margin, not half of it** (#204), so the band sits at the
  margin boundary. With the new margins `\headery` / `\footery` go from `648` to
  `1080` twips. Set `header_dist_in` / `footer_dist_in` (or the
  `rtfreporter.page.*` options) to pin either explicitly, as before.

### Bug fixes

- **Border colours on table cells now actually render** (#224). A coloured
  [rtf_border_side()] used on a zone (`rtf_table_border()`), a column
  (`col_spec$border`), a header cell (`col_cell(border = )`) or a body cell
  (`cell_styles` / `style_body(border = )`) was collected into the RTF
  colour table but the `\brdrcf` command was never emitted for table-cell
  borders (only the page header/footer band passed the colour map), so
  every such rule rendered black. The colour map is now threaded through all
  cell-definition writers, and the colour collector also picks up header-cell
  and per-cell border colours -- so a coloured `gt::tab_style()` border
  arrives coloured end-to-end via the `"styles"` token.

- `as_rtftables()` / `as_rtftable()` no longer error on a `gt_tbl` with **more
  than one stub column** (#193). `gt::extract_body()` assumes a single stub and
  fails with *"the condition has length > 1"* on such tables -- which `tfrmt`
  produces via `row_grp_plan(label_loc = element_row_grp_loc(location =
  "column"))`. The gt adapter now detects the multi-stub case and reads the body
  from the table's own `_data` / `_boxhead` slots instead (list access only, no
  gt internals), so the group and label columns come through as ordinary
  columns.

### New features

- `as_rtftables()` / `as_rtftable()` gain **`sort_by`** / **`sort_desc`** to
  order the body rows **before pagination** (#183). Name (or index) the sort
  keys -- in the input body's coordinates, the same space as `group_col` /
  `drop_cols` -- with optional per-key descending direction. Because the sort
  runs first, group detection, `(Cont.)` labels and `blank_rows` positions all
  see the sorted order; a sort key may also be listed in `drop_cols` to order on
  a column that is not printed (a hidden "sort-key" carrier column). The sort is
  stable and `NA` keys sort last. (Intended for flat / tabular bodies; on a gt /
  rtables body whose group + child rows are already interleaved, sort the source
  data before building the table instead.)

- `as_rtftables()` / `as_rtftable()` gain a **`drop_cols`** argument to hide a
  column from the printed table while still using it for pagination / grouping
  (#184). Name (or index) the columns -- in the input body's coordinates, the
  same space as `group_col` / `collapse_repeats` -- and they stay present
  through the split (so `group_col`, `collapse_repeats`, and
  `blank_rows_by_change()` can reference them), then are removed from every page
  before it is rendered. This makes a column usable as a hidden grouping /
  sort-key / carrier column without it appearing in the report. All
  position-indexed metadata (`col_header` incl. spanning headers, `col_spec`,
  column widths, `col_header_align`, `row_title`, per-cell `cell_styles`) is
  reindexed automatically to the remaining columns; `drop_cols` must leave at
  least one column to display.

### Documentation

- `showcase` RTF generators now **derive from the articles** instead of
  duplicating them (#179). `data-raw/showcase_dm.R` / `showcase_ae.R` are now
  ~20-line drivers that `knitr::purl()` the matching `showcase-*.Rmd` and run its
  code chunks, writing `inst/rtf-examples/showcase/{dm,ae}_*.rtf`. The article is
  the single source of truth, so the committed RTFs can no longer drift from the
  shown code (the previous duplication caused exactly that -- e.g. a footnote fix
  landed in the article but not the generator). The articles' display-only chunks
  (snapshot helper, `library()` setup, `img-*` chunks) are marked `purl = FALSE`.
  The old generators' pre-#169 helpers (`reorder_ae` / `add_any_ae_row` /
  `render_*` wrappers) are retired with the duplication. All DM + AE RTFs were
  regenerated from the articles. Refs #146.

- `showcase-ae` article + `data-raw/showcase_ae.R` generator + regenerated
  `inst/rtf-examples/showcase/ae_*.rtf` (#177):
  - **Zero cells now align.** `fmt_ae` previously collapsed a zero cell
    "0 (0.0%)" to a bare "0", which the count/percent aligner then left ragged.
    It now does the reverse -- **expands** a bare "0" to "0 (0.0%)" -- and is used
    by *every* framework including tern (whose `count_occurrences()` emits bare
    zeros), so all seven tables show an aligned `0 (0.0%)`.
  - **A SOC now splits across a page with `(Cont.)`.** `AE_MAX_ROWS` 32 -> 16, so
    the table runs to **3 pages** and a System Organ Class is continued with
    `(Cont.)` at the top of the next page (verified with the real splitter).
  - **Generator caught up to the article.** It still passed `read_meta = TRUE`
    (so the committed RTFs carried the gtsummary `{n} ({p}%)` footnote that #175
    removed from the article) and used the old collapse / `max_rows = 32`; it now
    matches, and the AE RTFs were regenerated.
  - **`AE_SORT` and the PT orderings move back to the data block** (revising
    #176), each commented with which blocks use it, so every framework block is
    copy-paste-runnable once the data block has run. The single-use helpers
    `add_overall_row()` / `bake_indent()` stay in their blocks. Refs #146.

- `showcase-ae` article, three fixes from maintainer review (#175): (1) lower
  `AE_MAX_ROWS` 32 -> 30 so page 1 still fits once Word re-renders the RTF; (2)
  pass `read_meta = FALSE` in every framework block so the gtsummary hierarchical
  tables' `{n} ({p}%)` statistic footnote (which appeared as stray lines below
  the table) is no longer read -- each block already supplies the column header /
  spec / widths and the footer, so no framework metadata is needed; (3) localise
  variables -- `AE_SORT` (gtsummary-family only) moves to the gtsummary block,
  the `.pt_order` / `.keep_pt` / `.soc_pt` orderings (transpose-and-set only) move
  to the Tplyr block, and the single-use helpers `add_overall_row()` /
  `bake_indent()` move into the cards + gtsummary and flextable / huxtable blocks
  respectively; only genuinely shared furniture stays in the setup block. Refs
  #146.

- `showcase` article **split into two**: `showcase-dm.Rmd` (*"Same report, every
  framework: Demographics"*) and `showcase-ae.Rmd` (*"... Adverse events"*)
  (#173). The old single-page URL `articles/showcase.html` now redirects to
  `articles/showcase-dm.html`. The AE article is now self-contained (it builds
  `adsl` itself) and cross-links the DM article, and vice versa.

- `showcase` AE article: restructured so rtfreporter's own functions are used
  **directly** instead of behind a `render_ae()` wrapper (#173), matching the DM
  treatment in #171. The shared furniture (`rtf_header()` / `rtf_footer()` + the
  column header / spec / widths) is defined **once**, and each framework block
  ends with the full `as_rtftables()` -> `rtf_document() |> rtf_section() |>
  rtf_tables()` -> `generate_rtfreport()` pipeline written out in the open. The
  footer is now a single shared `ae_footer` (was built per framework inside the
  wrapper); rendered RTF is unchanged. Refs #146.

- `showcase` article, DM chapter: restructured so rtfreporter's own functions are
  used **directly** instead of behind a `render_dm()` wrapper (#171). The shared
  report furniture (`rtf_header()` / `rtf_footer()` plus the column header, spec
  and widths) is now defined **once**, and each framework block ends with the full
  `as_rtftables()` -> `rtf_document() |> rtf_section() |> rtf_tables()` ->
  `generate_rtfreport()` pipeline written out in the open. The overrides the old
  helper applied (uniform column header, 40:20:20:20 widths, alignment, group
  blanks) are now expressed as plain `as_rtftables()` arguments
  (`col_header` / `col_spec` / `col_rel_width` / `blank_rows = "between_groups"`),
  so the rendered RTF is unchanged. Refs #146.

- `showcase` article, AE gtsummary / cards + gtsummary examples: the canonical
  SOC/PT order (SOCs alphabetical, PTs by total subject count descending, ties
  A -> Z) is now produced by gtsummary's **native** `sort_hierarchical()` with a
  per-variable `sort` (`list(AESOC ~ "alphanumeric", AEDECOD ~ "descending")`),
  replacing the custom `reorder_ae()` `$table_body` surgery, which is removed
  (#169). The standalone `tbl_hierarchical()` overall-row label is set via the
  native `label` argument. Verified byte-identical row order / render to the
  previous helper. `.pt_order` is kept for the Tplyr / tfrmt transpose-and-set
  blocks. Refs #146.

- `showcase` article, AE cards + gtsummary example: the any-AE overall row is now
  derived **generically from the ARD** instead of by re-counting the raw `adae`
  data frame (#167). A second `cards::ard_stack_hierarchical(..., over_variables =
  TRUE)` computes the distinct-subject "any event" count, and a generic
  `add_overall_row(tbl, ard)` reads it straight off that ARD (mapping by-levels to
  the table's `stat_*` columns via its own header, formatting with the table's
  glue `statistic`). `reorder_ae()` now takes `pt_order` explicitly, and the
  opaque `ae_gtsummary_tbl()` wrapper is gone -- the `tbl_hierarchical()` build is
  shown inline -- so the helpers stay generic / package-idiomatic rather than
  sample-specific. Rendered numbers unchanged. Refs #146.

- `showcase` article *"Same report, every table framework"*: every code block is
  now **copy-paste runnable, from data creation to the written `.rtf`** (#165).
  The article-only helpers that the prose referred to (`reorder_ae()`, `fmt_ae`,
  `add_any_ae_row()`, `bake_indent()`, `ae_gtsummary_tbl()`, and a `render_dm()`
  / `render_ae()` furniture helper) are now **defined in the article's setup
  blocks**, the abbreviated `...` reshapes (Tplyr DM, cards + tfrmt DM/AE, Tplyr
  AE) are spelled out in full, and each framework block ends by rendering its
  actual `.rtf` -- faithful to the `data-raw/showcase_*.R` generators that
  produced the screenshots. Chunks stay `eval = FALSE` (the screenshots are
  unchanged); run a section's setup block once, then any framework block. Refs
  #146.

- `tlg-catalog` article: the data preparation is now **verbatim from the
  pharmaverse examples** (#163). The demographic table drops screen failures
  (`!ACTARM %in% "Screen Failure"`) and recodes `SEX` / `AGEGR1`, so the
  "All Patients" column is N=254 (not 306). The cards + tfrmt adverse-events
  table now uses the example's safety / treatment-emergent filters
  (`SAFFL == "Y"`, `SAFFL == "Y" & TRTEMFL == "Y"`). The tern adverse-events
  table already matched. Generator `data-raw/gen_tlg_catalog_rtf.R` and the
  article chunks regenerated.

- New showcase article *"Same report, every table framework"* (#146): the same
  demographics (DM) report built from gtsummary, gtsummary on a cards/cardx ARD,
  rtables/tern, tfrmt and Tplyr, each rendered to RTF by rtfreporter --
  contrasting the two metadata philosophies (let rtfreporter *read* the
  framework's headers via `read_meta`, vs. *set* them in rtfreporter on a
  body-only result). The tfrmt example is shown **two ways** -- formatted by
  tfrmt, and formatted by rtfreporter (`align_count_pct`) from the same table --
  to contrast who owns the `n (x.x%)` count/percent alignment. Generator in
  `data-raw/showcase_dm.R`.
- Showcase gained an **Adverse Events (AE)** section (#146): subjects with a
  treatment-emergent AE by System Organ Class / Preferred Term. The **SOC-level
  count sits on the SOC row itself** (an independent distinct-subject count, not
  the sum of its PTs), with the overall any-AE on top and PTs (in >= 3% of any
  group) indented under each SOC. SOCs are listed alphabetically; PTs within a
  SOC are ordered by subject count (all arms) then alphabetically. Zero cells
  render as a bare `0` at both levels, and the row-label column header is
  left-aligned. Built with rtables/tern (`summarize_row_groups()` for the SOC row)
  and
  **paginated** by rtfreporter with `split = "group_force"` so a class split
  across a page repeats with `(Cont.)`. Generator in `data-raw/showcase_ae.R`.
- The AE showcase now also builds the same table with **gtsummary**
  (`tbl_hierarchical()`), **cards + gtsummary** (`tbl_ard_hierarchical()` on a
  hierarchical ARD), **Tplyr** (transpose-and-set) and **cards + tfrmt** -- the
  tfrmt version uses a `group == label` row to put the SOC count on the SOC row.
  The same gtsummary table is also converted to a **flextable** and a
  **huxtable** (`as_flex_table()` / `as_hux_table()`) and read back. All **seven**
  renderings produce the same numbers and SOC / PT order, paginated by
  rtfreporter.

### New features

- New constructors **`rtf_page()`** and **`rtf_default_format()`** turn the
  structured `rtf_document()` / `rtf_config()` settings into first-class S3
  objects whose **defaults are visible in their own signature / Usage** (#152):
  `rtf_document(page = rtf_page(paper_size = "A4", orientation = "portrait"))`.
  Site defaults still apply -- an argument you do not pass falls back to the
  matching `rtfreporter.*` option (explicit argument > option > the factory
  default shown in the signature). A plain named `list` is still accepted, so
  existing code is unchanged.

- The page **title** now renders as plain **text paragraphs** across the
  writable page width again (centred over the page), instead of the
  content-width single-column table introduced in v0.4.0 (#144). The legacy
  table form is available with `rtf_document(default_format =
  list(title_format = "table"))` (or the `rtfreporter.title_format` option).
  **Footnotes are unchanged** -- they keep the content-width table form (so the
  separator rule is preserved); switch them with `footnote_format = "text"` if
  desired. The text form honours each line's `align` / `bold` / `italic` /
  `underline` / `color` and the document-wide cell padding; a row `border` only
  applies in the table form.

- `rtftable()` gains a **`markup`** argument (forwarded by `as_rtftables()` /
  `rtf_tables()`, with a document-wide default via
  `rtf_document(default_format = list(markup = ))` / the `rtfreporter.markup`
  option) controlling cell-text markup with two tokens (#142): `"script"` turns
  `^{...}` / `_{...}` into super/subscript, and `"relational"` converts the
  literal `">="` / `"<="` into the `>=` / `<=` symbols (U+2265 / U+2264).
  `"all"` enables both, `"none"` neither. **The default is `"script"`**, which
  changes prior behaviour: `>=` / `<=` are now left **literal** unless you
  request `"relational"` / `"all"`, while super/subscript (e.g. adapter footnote
  marks `^{N}`) keep working. Applies to data cells, column / spanning headers,
  and title / footnote blocks.

### Bug fixes

- `as_rtftables(align_count_pct = TRUE)` no longer mis-aligns mixed columns.
  Only `"integer (real)"` count-percent cells are reformatted by
  `realign_count_pct()` (the real part may end in `%`); a bare integer (a plain
  N such as `"86"`) and a continuous statistic (`"75.2 (8.59)"`, whose "count"
  is not a bare integer) are now left **unchanged**. Previously (the #80
  behaviour) a bare integer was right-padded to the column's *maximum* width --
  which included non-count cells like `"75.2 (8.59)"` (12 chars) -- so it gained
  stray leading spaces and the count digits did not line up, worst with a `%`.
  The `realign_count_pct()` / `format_count_pct()` conversion itself is unchanged
  (e.g. `"0 (0.0%)"` still becomes `"0"`, `"10 (100.0)"` still `"10 (100)"`)
  (#148).

### Documentation

- Reference pages are being made easier to read (#150): list-valued parameters
  now break their keys out with a `\describe{}` list (so each sub-option is
  headlined), and examples are fuller and story-driven. First batch:
  `rtf_document()` (the `page` / `default_format` keys + a full worked example),
  `rtf_section()` (the `secinfo` keys + a self-contained example), and
  `rtf_header()` / `rtf_footer()` (the `l` / `c` / `r` cell rule + page-number
  tokens). More functions to follow.
- Continued the sweep (#150): the `read_gt` token list in `rtf_tables()`, the
  accepted row forms in `rtf_col_header()`, and the `cover` fields / `toc`
  shapes / `toc_page_numbering` options in `assemble_rtf()` now use `\describe{}`
  lists instead of plain bullet prose.


# rtfreporter 0.4.0

### New features

- `rtftable()` gains a **`blank_row_normalize`** argument controlling two
  render-time blank-row clean-ups, forwarded by `as_rtftables()` / `rtf_tables()`
  (#136): `"detect"` renders a data row whose every cell is `NA` / `""` (empty or
  ASCII-whitespace only) as a single full-width blank row instead of one empty
  cell per column; `"collapse"` reduces a run of two or more consecutive blank
  rows (separators and/or detected empty rows) to one. Default
  `c("detect", "collapse")` (both on); pass `"none"` / `NULL` to disable. Both
  act on the rendered table, so for a paginated table they apply per page (after
  the split). NBSP indentation is not whitespace-empty, so indented sub-rows are
  never mistaken for blanks. **Note:** with the default on, an existing all-empty
  data row now renders as one merged blank cell rather than N empty cells.

- `as_rtftables()` / `as_rtftable()` gain a **`collapse_repeats`** argument that
  blanks **consecutive repeated values** in the named column(s) -- repeat
  suppression for row-label / key columns (#131). Only the first value of each
  run is kept; the rest become `NA` (rendered as an empty cell, so no row is
  removed). Pass a character / integer vector of columns. With several columns
  the suppression is **hierarchical**: the first column collapses on its own
  value, each later column on its *combination* with all earlier listed columns
  (a change in any higher column restarts the lower column's run). It runs **per
  page, after the split**, so pagination still sees the original repeated values
  -- group boundaries and `" (Cont.)"` labels stay correct, and a group
  continued onto the next page repeats its label at the top. (In `group_by`
  terms: value-based grouping happens first, then the column is collapsed to a
  `"filled"`-style display.)

- `as_rtftables()` group-aware pagination (`split = "group_safe"` /
  `"group_force"` / `"by_value"`) gains a **`group_by`** argument that selects
  *how* a group boundary is detected, decoupled from `group_col` (which now
  only selects *which* column) (#128):
  - `"indent"` -- a non-empty, non-whitespace-leading cell starts a group
    (the previous behaviour, now usable on any column, not just column 1);
  - `"value"` -- maximal runs of the same value form a group;
  - `"filled"` -- a non-empty cell starts a group, `NA` / `""` cells are
    members (**new**);
  - `"auto"` (default) -- picks one of the above from the column content, so
    existing tables are unaffected.

  `group_col` now accepts an integer index and defaults to column 1, and the
  grouping column need not be the first column. (For gt/gtsummary the body keeps
  gt's column ids, e.g. `"label"`; for rtables/flextable/huxtable the columns
  are renamed `V1`, `V2`, ... -- so an integer index is the most portable.)

- `as_rtftables()` / `as_rtftable()` can now read **huxtable** objects (#120),
  joining gt, gtsummary, rtables/tern and flextable. The *displayed* text is
  read with the huxtable's `number_format` applied; header rows
  (`header_rows()`) become the column header, `colspan()` becomes the spanning
  header, per-column `align()` is carried, and the `caption()` becomes the page
  title. (A huxtable is a `data.frame` subclass, so it is detected before the
  plain-data.frame path.) huxtable has no footnote concept, so none is
  extracted; per-cell styling is not carried. New `read_meta` tokens for
  huxtable: `"col_header"`, `"alignment"`, `"spanning"`, `"titles"`.

- **Document-wide style defaults** for row height and cell padding (#124). Set
  `row_height_twips`, `cell_padding_left_twips` and `cell_padding_right_twips`
  once for a whole report -- via `rtf_document(default_format = list(...))` /
  `rtf_config(default_format = list(...))`, or globally as the
  `rtfreporter.row_height_twips` / `rtfreporter.cell_padding_left_twips` /
  `rtfreporter.cell_padding_right_twips` options -- and they apply uniformly to
  the content table, the page header/footer band, and the title/footnote
  blocks. They are **defaults**: a per-module value (`rtftable()`,
  `rtf_header()`, `rtf_footer()`, `rtf_table_style()`) always overrides them.
  Resolution is highest-wins: per-module > document `default_format` >
  `rtfreporter.*` option > font-aware / resource baseline. Default output is
  unchanged.

  As part of this, `rtftable()` / `rtf_tables()` `cell_padding_left_twips` /
  `cell_padding_right_twips` now default to `NULL` ("inherit") instead of `0L`,
  so an unset padding can pick up the document default while an explicit `0`
  still renders flush. (This also repairs a dormant `rtf_table_style()` path
  whose cell padding never seeded the table.)

- `as_rtftables()` / `as_rtftable()` can now read **flextable** objects (#119),
  joining gt, gtsummary and rtables/tern. The *displayed* cell text is read via
  flextable's exported introspection API (`information_data_chunk()` /
  `information_data_paragraph()`), so `set_header_labels()` relabelling and
  `colformat_*()` formatting are honoured. Column (leaf) labels, spanning
  headers, per-column alignment, the caption (title) and footer lines
  (footnotes) are carried; per-cell styling, explicit widths and `footnote()`
  reference marks are not. New `read_meta` tokens for flextable: `"col_header"`,
  `"alignment"`, `"spanning"`, `"titles"`, `"footnotes"`.

- Titles and footnotes are reworked (#117):
  - They now render as a **single-column table the same width as the content**
    (table or figure), so the blocks line up with the content above/below
    instead of spanning the full page width.
  - `titles` / `footnotes` accept a **length-1** list as well as one-per-page:
    a single block is applied to **every page** (common title/footnote). Applies
    to `rtf_titles()`, `rtf_footnotes()`, `rtf_tables()`, and `rtf_figures()`.
  - **Per-row styling**: a block element may be a styled
    `list(text=, align=, bold=, italic=, underline=, color=, border=)` (in
    addition to a plain character vector). Defaults are unchanged (title rows
    centred + bold; footnote rows left, with a top-rule separator on the first
    row), and the footnote separator is now overridable per row via `border=`.

- Package defaults are now **configurable options** read through the
  function/`page`-key defaults. Every `rtfreporter.*` default (paper size,
  orientation, margins, font, font size) resolves as: explicit argument >
  `getOption("rtfreporter.*")` > factory baseline. A site can set its own
  defaults in `Rprofile.site` -- e.g. `options(rtfreporter.font = "Arial")` --
  and every report inherits them, while the `rtfreporter.*` keys remain a single
  source of truth seeded at load. New helpers `rtfreporter_options()` (snapshot
  the resolved configuration, useful as an audit trail) and
  `rtfreporter_reset_defaults()` (restore the factory baseline) (#111).

- The header/footer band distance is now an **explicit** page setting,
  `header_dist_in` / `footer_dist_in` (or the matching `rtfreporter.page.*`
  options), instead of being computed silently. When left unset it still
  defaults to half the top/bottom margin, preserving previous output (#111).

- Page geometry gains a `paper_size` preset key: `page = list(paper_size =
  "A4")` selects a named size (`"letter"`, `"legal"`, `"A4"`, `"A3"`, `"A5"`,
  case-insensitive) and orients it (`paper_size = "A4"` is A4 landscape in one
  line). Geometry resolution is also rationalized: explicit `width_in` /
  `height_in` now **win and are used as given**, with the orientation *inferred*
  from them; an `orientation` that contradicts the dimensions warns and the
  dimensions are kept (this **supersedes** the #106 behaviour where orientation
  re-oriented the given dimensions). A `paper_size` supplied alongside explicit
  dimensions is ignored with a warning. Applies to `rtf_document(page=)` and
  `rtf_config(page=)` (#110).

- `rtf_config()` now **merges `page` and `default_format` per key** instead of
  replacing the whole block, so changing only the paper size (for example
  `rtf_config(page = list(width_in = 11.69, height_in = 8.27))`) keeps the
  document's existing orientation and margins. This clarifies the split between
  the two functions: `rtf_document()` *constructs* a new document from defaults,
  while `rtf_config()` *edits* an already-composed document (content and
  sections preserved), changing only the keys you pass. `font_table` /
  `color_table` are still whole-object replacements (#108).

- Page `orientation` is now authoritative: when set, the long/short page sides
  are assigned to match it, so `rtf_document(page = list(orientation =
  "landscape", width_in = 8.27, height_in = 11.69))` (A4 sizes in either order)
  reliably produces A4 landscape. When `orientation` is omitted it is inferred
  from the dimensions (`width_in >= height_in` means landscape), and an invalid
  orientation is now rejected. Applies to `rtf_document(page=)` and
  `rtf_config(page=)` (#106).

### Internal

- Added multi-group / multi-page pagination tests for **every** table-object
  adapter (gt, gtsummary, rtables/tern, flextable, huxtable), exercising the
  shared `as_rtftables()` pipeline (grouping, pagination, blank rows,
  `collapse_repeats`) end to end. Test-only; no behaviour change (#134).

### Bug fixes

- Multi-page documents now insert a page break that Word actually honours. The
  break between pages is emitted as `{\pard\fs2\par}\page{\pard\fs2\par}` -- the
  form r2rtf and reporter use -- with the `\page` flanked by empty paragraphs;
  previously a bare `\page` after a table `\row` was dropped by Word, so the next
  page rendered flush against the previous one. Section-separated pages
  (`\sect` + `\sectd\sbkpage`) were unaffected. Most visible with
  `as_rtftables(split = "group_safe")` and other plain (no page-number token)
  multi-page splits (#130, #138).

- The automatic spanning-header **group underline** is now drawn only where the
  column grouping actually changes below the cell (the next header row
  subdivides the span). Previously every multi-column spanning cell that was not
  on the last header row was underlined, so a span repeated unchanged across two
  header rows got a spurious rule between them (#102).

### New features

- New `row_title` argument on `rtftable()` (and `rtf_tables()` /
  `as_rtftables()`) names the **row-heading columns** — an integer vector of
  column indices or column names, defaulting to the first column
  (e.g. `row_title = c(1, 2)`) (#98).

### Behaviour change

- The per-column **default data alignment** now depends on `row_title`:
  row-heading columns default to `"left"` and every other column defaults to
  `"center"` (previously every column defaulted to `"left"`). Explicit
  `col_spec` alignment, an `rtf_table_style`, or alignment read from a
  gt/rtables source still override the default; column headers continue to
  follow their column's data alignment (#98).

# rtfreporter 0.3.4

This release rolls up all development work since v0.1.0 and is published as the
current stable version. Going forward the planned milestones are **v0.4.0**
(CRAN-submission preparation), **v0.5.0** (CRAN registration) and **v1.0.0**
(stable API; the `lifecycle: experimental` badge is removed).

### Documentation

- README "Status & roadmap" rewritten to reflect the current state: `v0.3.4` is
  the latest release, with the development version on `main` advancing only the
  patch per PR, and the v0.4.0 / v0.5.0 / v1.0.0 milestones laid out. A
  `version-guard` CI workflow blocks an unintended MINOR/MAJOR bump (a labelled
  `release` PR is required) (#83, #92, #94).
- `as_rtftables()` help is now exhaustive and per-argument. The pagination
  controls that were lumped into a single `@param` block are split into one
  entry each (`split`, `max_rows`, `split_rows`, `group_col`, `cont_label`,
  `blank_rows`, `blank_row_first`/`blank_row_end`, `align_count_pct`), `split`
  enumerates every strategy and what it requires, `blank_rows` documents all
  accepted value forms (integer positions incl. `0`/`-1`,
  `blank_rows_by_change()`, `blank_rows_by_rule()`, and combining `list()`s),
  and runnable `@examples` now cover the common arguments (#85).
- The "Importing tables" article now documents how to write a custom
  `cell_format` function: the calling contract (one column-vector argument,
  same-length character return, leave non-matching cells unchanged, pad with
  the non-breaking space), how single-function vs list dispatch picks columns,
  and a complete runnable example that appends a unit and right-aligns a column
  via a one-argument closure (#84).
- CONTRIBUTING.md now describes a concrete backlog/progress-tracking scheme
  for multiple contributors: a "rtfreporter roadmap" GitHub Projects board with
  defined Status columns (Backlog/Ready/In progress/In review/Blocked/Done),
  Priority/Area/Agent custom fields, built-in automation, and the one-time
  maintainer setup commands -- alongside how it relates to the existing
  `exec:*` dispatch labels and release milestones (#83).

### Output robustness

- Generated RTF now emits `\headery` / `\footery` (header/footer band distance
  from the page edge), set to half the top/bottom margin so the header and
  footer sit inside the margin instead of at Word's built-in default, which
  could place them outside the body (#82).
- Hardened the document preamble for portable, renderer-independent output:
  `\ansicpg1252` (code page), `\deflang1033` (default language), `\uc1`
  (one fallback char per `\uN` Unicode escape, matching the `\uN?` form this
  package emits), `\fcharset0` on the font, and `\widowctrl` (#82).

### New features

- Fine-grained column-header borders. `col_cell()` gains a `border` argument:
  an `rtf_border()` applied to that one header cell, overriding the zone border
  and the automatic group underline. Combined with the new
  `rtf_border_side("none")` ("explicit no line", which overrides on merge),
  this lets you add or remove individual rules in a multi-row header -- e.g.
  suppress the underline beneath a single spanning cell (#81).
- New article "Borders and rules" documenting the three levels of border
  control (preset, `rtf_table_border()` zones, per-column `col_spec` and
  per-cell `col_cell()`) with runnable examples (#81).

### Bug fixes

- `as_rtftables(align_count_pct = TRUE)` no longer reformats integer-only
  columns. Previously a plain count column (cells like `3`, with no
  `n (xx.x)` cells anywhere in the column) had leading spaces wrongly inserted.
  Bare-integer alignment is now applied only to columns that actually mix
  integers with count-percent cells (#80).

### Internal: constructor cleanup (no API change)

Collapsed the thin `rtftable()` / `rtfplot()` wrapper layer (a vestige of the
R6 era) into the public constructors: the exported `rtftable()` / `rtfplot()`
are now the implementations directly (the internal `.new_rtftable()` /
`.new_rtfplot()` and `R/wrappers.R` are gone). No change to behaviour, exported
names, or signatures (#72).

### Breaking changes: removed no-op deprecated functions

Pre-CRAN cleanup of long-deprecated no-ops:

- Removed `rtf_table_format()`, `rtf_header_format()`, `rtf_footer_format()`,
  and `rtf_figure_format()` (they only warned and returned the document
  unchanged). Pass the formatting arguments to `rtf_tables()` / `rtf_figures()`,
  or build items with `rtftable()` / `rtfplot()`.
- Removed the deprecated `top_border` argument of `rtf_header()` /
  `rtf_footer()`. Use `border = rtf_border_top()` (or `border = NULL`). (#70)

### Bug fix: `assemble_rtf(toc = "auto")` no longer errors

`assemble_rtf(toc = "auto")` errored on any input file that had a title
(`.extract_first_title()` used a function replacement in `gsub()`, which base R
does not support). Title extraction now un-escapes `\uN?` sequences via
`regmatches()`, so auto-TOC works -- including non-ASCII titles (#60).

### Count blank rows toward `max_rows` during pagination (opt-in)

`as_rtftables(count_blank_rows = TRUE)` now counts blank separator rows toward
`max_rows`, so a paginated page (data rows + blanks) does not overflow the
budget. The blank positions resolved from `blank_rows` (and from any
`rtf_blank_rows` attribute already on the input) are materialised before the
split and re-attached per page afterwards, with a leading blank suppressed at
the top of each page. `blank_row_first` / `blank_row_end` remain post-split page
furniture and are not counted. Default `FALSE` keeps the previous behaviour
(blanks added after the split, not counted) (#58).

### Bug fix: `border = "none"` now works

`rtftable(border = "none")` (and the `rtf_tables()` `border = "none"` override)
were documented as "no borders" but errored. `"none"` is now accepted as an
alias for `NULL` (no borders) (#54).

### `as_rtftable()` now accepts a data.frame / tibble

`as_rtftable()` (singular) previously rejected plain data.frames even though
`as_rtftables()` accepted them. It now accepts a `data.frame` / tibble too
(returning one `rtftable`), making the singular and plural forms consistent
(#48).

### Cell text colour, and `color_table` is now wired in

Tables can now carry **text colour**: set `color = "#RRGGBB"` on a `col_spec`
entry for a whole column, or on a `cell_styles` row (a length-`ncol` vector,
`NA` = no override) for individual cells. Colours are rendered as `\cf` and
added to the document colour table automatically. The `color_table` argument of
`rtf_document()` is now wired into the rendered palette (it was previously
ignored); black and white remain reserved. (Cell background fill and
header/footer text colour are planned follow-ups.) (#39)

### Bug fix: portrait orientation is now honored

`rtf_document(page = list(orientation = "portrait"))` now produces a portrait
document. The document preamble previously hardcoded `\landscape`, which forced
the first section landscape in many viewers regardless of the setting; the
document-level `\landscape` is now emitted only for landscape pages (matching
the already-correct section-level behaviour) (#31).

### Test suite strengthened (coverage > 90%)

Added methodology-driven tests — defensive-contract / error-path coverage,
boundary-value analysis, equivalence partitioning on pure helpers, and
behavioural checks of the `auto_width` path — covering `rtftable()` argument
validation, multi-DF construction, the `rtf_tables()` override path,
`.flatten_col_header_labels()`, the count/percent formatters, and the remaining
`rtf_replace_text()` validation. Package coverage rises from ~89% to >90% (#25).

### Built-in pagination strategies exposed as reusable factories

The built-in page-split strategies are now available as exported factory
functions — `page_split_none()`, `page_split_rows()`, `page_split_group_safe()`,
`page_split_group_force()`, `page_split_by_value()` — each returning a function
usable as `as_rtftables(split = )`. The string forms (`split = "group_safe"`,
…) are exact aliases that delegate to these factories, so built-in and custom
splitters now share one contract and can be reused or composed (#22).

### `as_rtftables(split = )` accepts a custom pagination function

`split` may now be a **function** for bespoke page-break rules, in addition to
the built-in strategies. It is called on the (cell-formatted) body and must
return a list of data.frames (one per page; named elements become page names);
the shared pipeline — blank rows, metadata, per-page assembly, and
header/width/style replication — is applied to its output unchanged. The new
`add_cont_label()` helper re-creates the `" (Cont.)"` continuation row for
custom splitters (#21).

# rtfreporter 0.1.0

First public GitHub release. (CRAN submission is deferred to v0.2.0.)

### `rtf_replace_text()` — post-processing find/replace on a rendered RTF

New helper for the "last mile" of TLG production: perform find-and-replace
directly on a generated `.rtf` file. Supports fixed or regex targets,
case-insensitive matching, vectorised target/replacement, in-place editing with
an automatic `.bak`, or writing to a separate `output_file` (#9).

### `rtf_tables()` accepts a single content item without `list()`

`rtf_tables()` now auto-wraps a single content item, so you can write
`rtf_tables(tbl)` instead of `rtf_tables(list(tbl))`. A bare `data.frame`,
`rtftable()`, `rtfplot()`, `gt_tbl`, or gtsummary table is accepted directly;
passing a `list` of items continues to work unchanged (#3).

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

----

**Earlier versions:** see [`CHANGELOG.md`](CHANGELOG.md) for the detailed
pre-v0.0.18 history.

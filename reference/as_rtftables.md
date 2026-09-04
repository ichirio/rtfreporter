# Convert a table object into rtfreporter table pages

`as_rtftables()` is the single entry point for turning a *table object*
into a list of
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
objects – one per RTF page – ready to hand straight to
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md).
It unifies two jobs that used to be split across
[`paginate()`](https://ichirio.github.io/rtfreporter/reference/paginate.md)
(page splitting) and `rtf_tables(read_gt = )` (metadata extraction):

## Usage

``` r
as_rtftables(
  x,
  read_meta = TRUE,
  max_rows = NULL,
  split = c("none", "rows", "group_safe", "group_force", "by_value"),
  split_rows = NULL,
  group_col = NULL,
  group_by = c("auto", "indent", "value", "filled"),
  sort_by = NULL,
  sort_desc = NULL,
  cont_label = " (Cont.)",
  min_group_rows = 2L,
  blank_rows = NULL,
  blank_row_first = FALSE,
  blank_row_end = FALSE,
  count_blank_rows = FALSE,
  align_count_pct = FALSE,
  cell_format = NULL,
  na = "",
  collapse_repeats = NULL,
  drop_cols = NULL,
  listing = NULL,
  stub = NULL,
  stub_vars = NULL,
  stub_label = NULL,
  stub_indent = 4L,
  stub_group_summary = c("empty", "parent"),
  header_sep = .default_header_seps(),
  auto_width = FALSE,
  table_width_twips = NULL,
  border = "tfl",
  style = NULL,
  ...
)
```

## Arguments

- x:

  A `gt_tbl`, a `gt_group`, a gtsummary table (or `tbl_split`
  container), an rtables/tern `VTableTree`, an rlistings `listing_df`, a
  `flextable`, a `huxtable`, a `data.frame` / tibble, or a `list` of
  these.

- read_meta:

  Controls metadata extraction from the source table: `TRUE` (default,
  read everything in the table above), `FALSE` (use only the rendered
  body – equivalent to the old
  [`paginate()`](https://ichirio.github.io/rtfreporter/reference/paginate.md)),
  or a character vector of tokens. Tokens for gt/gtsummary:
  `"col_header"`, `"alignment"`, `"spanning"`, `"widths"`, `"titles"`,
  `"footnotes"`, `"styles"` (explicit
  [`tab_style()`](https://gt.rstudio.com/reference/tab_style.html)
  borders and text styles; see *What is carried*). For rtables/tern:
  `"col_header"`, `"alignment"`, `"spanning"`, `"titles"`,
  `"footnotes"`, `"indent"`, `"footnote_marks"`; **rlistings takes the
  same tokens**, because the two share one `MatrixPrintForm` reader. For
  flextable: `"col_header"`, `"alignment"`, `"spanning"`, `"titles"`,
  `"footnotes"`. For huxtable: `"col_header"`, `"alignment"`,
  `"spanning"`, `"titles"`. For a plain data.frame / tibble the single
  token is `"labels"`: a column's `label` attribute (the haven /
  labelled / xportr convention) is used as its header label (columns
  without one keep their name). Unknown tokens are *ignored* for
  data.frames – a [`list()`](https://rdrr.io/r/base/list.html) mixing
  table objects and data.frames shares one `read_meta`, so adapter
  tokens must not error on the data.frame elements.

- max_rows:

  Integer or `NULL`. Maximum body rows per page for the `"group_safe"` /
  `"group_force"` splits (required by them). Ignored by `"none"`,
  `"rows"` (which uses `split_rows`) and `"by_value"`.

- split:

  How to break the body into pages. A strategy name:

  `"none"`

  :   (default) one page; no row limit checked.

  `"rows"`

  :   fixed chunk size; requires `split_rows`.

  `"group_safe"`

  :   fill up to `max_rows` but never split a group (defined by
      `group_col`) across a page; requires `max_rows`.

  `"group_force"`

  :   like `"group_safe"`, but a single group larger than `max_rows` may
      span pages with a continuation label; requires `max_rows`.

  `"by_value"`

  :   one page per distinct value of `group_col`; the pages are named by
      that value.

  `split` may also be a **custom function** for bespoke page-break
  rules. It is called as
  `split(df, max_rows = , group_col = , group_by = , cont_label = , min_group_rows = )`
  on the (cell-formatted) body and must return a **non-empty list of
  data.frames** – one per page. Named list elements become page names
  (as with `"by_value"`). Your function implements only the split; the
  shared pipeline (blank rows, metadata, per-page assembly, and header /
  width / style replication) is applied to its output unchanged. Write
  the function with a `...` so it tolerates the context arguments it
  does not use, and see
  [`add_cont_label()`](https://ichirio.github.io/rtfreporter/reference/add_cont_label.md)
  for re-creating the `" (Cont.)"` continuation row.

- split_rows:

  Integer or `NULL`. Rows per page for `split = "rows"` (required by it;
  ignored otherwise).

- group_col:

  Character, integer, or `NULL`. The column the group-aware splits
  (`"group_safe"`, `"group_force"`, `"by_value"`) detect groups on,
  given by name or position. `NULL` (default) uses **column 1**. This
  selects only the *column*; how a group boundary is found on it is set
  by `group_by`. Note that for gt / gtsummary the body keeps gt's column
  **ids** (e.g. `"label"`, `"stat_1"`), and for rtables / flextable /
  huxtable the columns are renamed `V1`, `V2`, ... – so an **integer**
  index is the most portable. A **factor** group column is rendered as
  its labels: it is coerced to character just before the split, so the
  group-aware splits can write the `cont_label` suffix into it. This
  happens *after* `sort_by`, which still orders a factor by its
  **levels**.

- group_by:

  How a group boundary is found on `group_col`:

  `"auto"`

  :   (default) pick from the column content: leading indentation
      present -\> `"indent"`; else interspersed empty cells -\>
      `"filled"`; else -\> `"value"`.

  `"indent"`

  :   a row starts a group when its `group_col` cell is non-empty and
      does **not** begin with whitespace (space / tab / non-breaking
      space); indented or empty cells are members. The typical clinical
      row-label layout (gt / tfrmt bake indentation as NBSP).

  `"value"`

  :   each maximal run of rows sharing the same `group_col` value is one
      group.

  `"filled"`

  :   a row starts a group when its `group_col` cell is non-empty; only
      `NA` / `""` cells are members (the label appears once, on the
      group's first row).

- sort_by:

  Columns to **order the body rows by, before pagination**, or `NULL`
  (default, keep the input order). A character / integer vector (or a
  [`list()`](https://rdrr.io/r/base/list.html) to mix names and indices)
  in the **input body's** coordinates – the same space as `group_col`,
  `collapse_repeats` and `drop_cols`. The sort is applied *first*, so
  group detection, `(Cont.)` labels and `blank_rows` positions all see
  the sorted order. A sort key may also be listed in `drop_cols` to
  order on a column that is not printed (the classic hidden "sort-key"
  carrier column). The sort is **stable** (rows that compare equal keep
  their input order) and `NA` keys sort **last** regardless of
  `sort_desc`. Note that `sort_by` reorders rendered body rows as-is, so
  it suits flat / tabular bodies (a `data.frame`, or a pre-flattened
  table); on a gt / rtables body whose group-label and child rows are
  already interleaved it would scramble that visual hierarchy – sort the
  source data before building such a table instead.

- sort_desc:

  Sort direction for `sort_by`: `NULL`/`FALSE` (default, all ascending),
  a single `TRUE` (all `sort_by` columns descending), or a logical
  vector the same length as `sort_by` (one direction per key). Ignored
  when `sort_by` is `NULL`.

- cont_label:

  Character (default `" (Cont.)"`). Suffix appended to a group's label
  on the second and later pages it continues onto (the group-aware
  splits), marking a continued group.

- min_group_rows:

  Integer (default `2`). Widow/orphan control for the group-aware splits
  (`"group_force"`, `"group_safe"`, `"by_value"`): when a page would end
  on a group that *starts* on that page while showing fewer than
  `min_group_rows` of the group's child rows, the whole group is moved
  to the next page. This prevents a lone group header being stranded at
  the foot of a page with none (or too few) of its members. Set to `0`
  to disable (the previous behaviour).

- blank_rows:

  Where to insert blank separator rows in the body. `NULL` (default)
  inserts none (but an `rtf_blank_rows` attribute already on the input
  is still honoured). Accepts any of – or a
  [`list()`](https://rdrr.io/r/base/list.html) combining:

  an integer vector of positions

  :   a blank row is inserted *after* each given data-row index; `0` =
      before the first row, `-1` = after the last row (e.g.
      `c(0, 5, -1)`).

  `"between_groups"`

  :   a blank at every group transition on `group_col`, using this
      call's `group_by` detection (auto / indent / value / filled).

  a [`blank_rows_by_change()`](https://ichirio.github.io/rtfreporter/reference/blank_rows_by_change.md) object

  :   insert a blank whenever the group of one or more columns changes –
      the spec carries its own `group_by`.

  a [`blank_rows_by_rule()`](https://ichirio.github.io/rtfreporter/reference/blank_rows_by_rule.md) object

  :   insert a blank before / after rows whose column matches a regular
      expression.

  For example `blank_rows = list(c(-1), blank_rows_by_change("Visit"))`
  adds a trailing blank and a blank at every change of `Visit`. By
  default these blanks are added *after* the split and do not count
  toward `max_rows`; set `count_blank_rows = TRUE` to make `max_rows`
  mean the rows the page actually prints.

- blank_row_first, blank_row_end:

  Logical (default `FALSE`). Add a single blank row at the very top
  (`blank_row_first`) or bottom (`blank_row_end`) of **every** page, as
  page furniture. They are applied after the split, and whether they
  count toward `max_rows` is `count_blank_rows`: under `TRUE` they do,
  so `max_rows` is the number of rows the page prints; under `FALSE`
  (the default) they do not, and the printed page can be up to two rows
  taller than `max_rows`.

- count_blank_rows:

  Logical (default `FALSE`). What `max_rows` counts.

  `TRUE`

  :   `max_rows` is the number of rows the page **prints**: body rows,
      blank rows already in the data, the blanks `blank_rows` inserts
      between groups, **and the `blank_row_first` / `blank_row_end` page
      edges**. A page never overflows the budget. The edges are counted
      as they actually render: a page-edge blank merges with a blank row
      it sits against (see `blank_row_normalize`), so it costs a row
      only where there is no blank row there already – which is why a
      listing, whose every record block ends in a blank row, does not
      lose a row at the foot of each page.

  `FALSE`

  :   (default) `max_rows` counts only the rows that were in the input.
      Nothing inserted counts, at the page edges or between groups, so
      the printed page can be taller than `max_rows`.

  Under `TRUE` the blank positions resolved from `blank_rows` (and from
  any `rtf_blank_rows` attribute already on the input) are materialised
  before the split and re-attached per page afterwards, with a leading
  blank suppressed at the top of each page.

- align_count_pct:

  Logical (default `FALSE`). Shorthand to realign `"n (xx.x)"`
  count/percent cells to a uniform width before pagination (the built-in
  [`realign_count_pct()`](https://ichirio.github.io/rtfreporter/reference/realign_count_pct.md)).
  Ignored when `cell_format` is supplied, which takes precedence.
  Neither one controls whether missing values are substituted – that is
  `na` – only whether the substituted token is aligned with the counts.

- cell_format:

  Optional cell re-formatter applied column-by-column to the body
  **before** pagination, for monospaced alignment. Either a single
  function – applied to every data column (columns 2..N; the row-label
  column 1 is left alone) – or a list of functions taken positionally
  (`cell_format[[j]]` for column `j`; non-function entries are skipped).
  Each function takes one column (a character vector) and returns a
  character vector of the same length; see
  [`fmt_count_paren()`](https://ichirio.github.io/rtfreporter/reference/fmt_count_paren.md)
  /
  [`fmt_right_align()`](https://ichirio.github.io/rtfreporter/reference/fmt_right_align.md)
  for built-ins and the contract for writing your own. When supplied it
  takes precedence over `align_count_pct`.

- na:

  Text to print for a **missing value** (default `""`, an empty cell –
  the previous behaviour). Applied to every column, the row-label column
  included, and **independently of `align_count_pct` / `cell_format`** –
  those decide whether the substituted token is also *aligned*, not
  whether the substitution happens. With one of them set, the token is
  right-justified in the count field so its right edge lands on the ones
  digit:

        1 ( 1.2%)
      108 (35.3%)
        -            <- na = "-"
       NE            <- na = "NE", right edge still under the ones digit

  Text already in the data that is *not* missing (`"NE"`, `"n/a"`, a
  literal `"-"` you typed yourself) does not match the count pattern and
  passes through unchanged, with no padding.

  `NaN` counts as missing (R's own
  [`is.na()`](https://rdrr.io/r/base/NA.html) says so, and in a TFL a
  `NaN` is a `0/0` percentage). `Inf` / `-Inf` do **not**: they print as
  `"Inf"` / `"-Inf"`, because an infinity means a division by zero
  upstream and rendering it as `"-"` would hide the bug. The *strings*
  `"NA"` / `"NaN"` are never touched – `"NA"` can be legitimate data.

  The substitution happens before the split, so the `NA`s
  `collapse_repeats` writes per page to blank out repeated values stay
  blank.

- collapse_repeats:

  Columns in which to blank **consecutive repeated values** (repeat
  suppression), or `NULL` (default, off). A character / integer vector
  naming the columns in priority order. Within each column, only the
  first value of a run is kept; the rest of the run is replaced with
  `NA` (which renders as an empty cell – no row is removed, only the
  display text is suppressed). When several columns are given the
  suppression is **hierarchical**: the first column is collapsed on its
  own value, and each later column on the *combination* of itself with
  all earlier listed columns (a change in any higher column restarts the
  lower column's run). This runs **per page, after the split**, so the
  pagination still sees the original repeated values – group boundaries
  and `(Cont.)` labels stay correct, and a group continued onto the next
  page shows its label again at the top. (In `group_by` terms:
  value-based grouping happens first, then the column is collapsed to a
  `"filled"`-style display.)

- drop_cols:

  Columns to **hide from the printed table while still using them for
  pagination / grouping**, or `NULL` (default, drop nothing). A
  character / integer vector (or a
  [`list()`](https://rdrr.io/r/base/list.html) to mix names and indices)
  naming the columns in the **input body's** coordinates – the same
  coordinate space as `group_col` and `collapse_repeats`. The named
  columns stay present through the split (so `group_col`,
  `collapse_repeats` and
  [`blank_rows_by_change()`](https://ichirio.github.io/rtfreporter/reference/blank_rows_by_change.md)
  can reference them), then are removed from **every page** before that
  page's table is rendered. This makes a column usable as a hidden
  grouping / sort-key / carrier column without it appearing in the
  report. Position-indexed metadata (`col_spec`, column widths,
  `col_header_align`, `row_title`, and per-cell `cell_styles`) is
  reindexed automatically to the remaining columns. A **user-supplied
  `col_header`** is the exception: it is applied against the **final
  printed columns** (see the `col_header` argument and
  [`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md)),
  so you write its positions / names for the columns that remain – not
  the pre-drop layout. `drop_cols` must leave at least one column to
  display. Note that for `split = "by_value"` the page **names** come
  from the `group_col` value, so that column may be dropped and the
  pages are still named by it; but under `"group_safe"` /
  `"group_force"` the `" (Cont.)"` marker is written into the
  `group_col` cell, so if `group_col` is itself dropped the marker is
  not shown (group on the visible label column if the marker is wanted).

- listing:

  Listing settings, as a
  [`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md)
  object, or `NULL` (default, no listing preparation). Applies to a
  **data.frame / tibble** source:
  [`build_listing()`](https://ichirio.github.io/rtfreporter/reference/build_listing.md)
  reshapes it into a listing body – source variables joined into their
  printed columns, long cells wrapped over several physical rows, gutter
  columns, a blank row after each record – and the spec's columns then
  supply the `col_header`, the relative widths and the (left) alignment,
  so none of the three has to be written out by hand. The hidden record
  column is used to keep a record whole across a page break: `group_col`
  points at it, `split` becomes `"group_safe"` when `max_rows` is set,
  and it is added to `drop_cols`. Every one of those is a **default** –
  an argument you pass yourself is never overridden. A body that has
  already been through
  [`build_listing()`](https://ichirio.github.io/rtfreporter/reference/build_listing.md)
  carries its own spec, so `as_rtftables()` picks it up and passing
  `listing` as well is an error. An rlistings `listing_df` is rejected:
  it is already laid out, and needs no `listing` argument.

- stub:

  Row-stub settings, as a
  [`stub_spec()`](https://ichirio.github.io/rtfreporter/reference/stub_spec.md)
  object – or, as a shorthand for `stub_spec(vars)`, a bare vector of
  hierarchy columns (parent first, leaf last; at least two). `NULL`
  (default) builds no stub. This is the **one** argument for everything
  [`stub_cols()`](https://ichirio.github.io/rtfreporter/reference/stub_cols.md)
  can do, including `layout` and `label_span`, which the superseded
  `stub_vars` family cannot reach. Passing both `stub` and any of the
  `stub_vars` family is an error.

  The stub is applied to the **extracted body** – after the table is
  pulled out of its source, before pagination – so any input that
  carries the hierarchy as *separate columns* (a plain `data.frame`, a
  `gt(df)`, a multi-group-column table, or a tfrmt
  `row_grp_plan(location = "column")` table) gains a clinical stub. With
  `layout = "merged"` the named columns are consumed and replaced by one
  stub column at position 1 and the remaining columns keep their order;
  with `layout = "columns"` every column stays where it is. Either way
  `drop_cols` / `group_col` / `sort_by` (plus `group_by = "indent"`
  detection and the group-aware splits) then refer to the reshaped body.
  **Exception: with `split = "by_value"` the body is split by
  `group_col` *first* and the stub is built per page afterwards** – each
  value becomes an independent section, so `group_col` here refers to
  the **pre-stub** columns and is not folded into the stub. This keeps a
  fixed intermediate hierarchy level (e.g. `LBTOX_LBL / group1 / label`
  with a constant `group1`) from collapsing into a single stub label row
  that spans every group; put the inner levels in the stub and the outer
  level in `group_col`. Position-indexed metadata (`col_spec`,
  `col_header_align`, per-cell `cell_styles`) is reindexed
  automatically; a user-supplied `col_header` is instead resolved
  against the final columns (see
  [`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md)).
  Sources that already render an indented stub (rtables / tern, tfrmt
  indented, gtsummary) come out pre-merged, so a stub does not apply to
  them. Under `layout = "merged"` the source column widths are **not**
  carried through the merge – use `auto_width = TRUE` or pass explicit
  widths; `layout = "columns"` keeps them.

- stub_vars:

  **Superseded** by `stub`; still supported and not deprecated, but it
  cannot reach `layout` or `label_span`, and new stub settings are added
  to
  [`stub_spec()`](https://ichirio.github.io/rtfreporter/reference/stub_spec.md)
  only. Hierarchy columns to merge into a single **indented stub**
  column (parent first, leaf last; at least two), or `NULL` (default, no
  stub built). Applies
  [`stub_cols()`](https://ichirio.github.io/rtfreporter/reference/stub_cols.md)
  to the **extracted body** – after the table is pulled out of its
  source, before pagination – so any input that carries the hierarchy as
  *separate columns* (a plain `data.frame`, a `gt(df)`, a
  multi-group-column table, or a tfrmt
  `row_grp_plan(location = "column")` table) gains a clinical stub. The
  named columns are consumed and replaced by one stub column at position
  1; the remaining columns keep their order, and `drop_cols` /
  `group_col` / `sort_by` (plus `group_by = "indent"` detection and the
  group-aware splits) then refer to the reshaped, **post-stub** columns.
  **Exception: with `split = "by_value"` the body is split by
  `group_col` *first* and the stub is built per page afterwards** – each
  value becomes an independent section, so `group_col` here refers to
  the **pre-stub** columns and is not folded into the stub. This keeps a
  fixed intermediate hierarchy level (e.g. `LBTOX_LBL / group1 / label`
  with a constant `group1`) from collapsing into a single stub label row
  that spans every group; put the inner levels in `stub_vars` and the
  outer level in `group_col`. Position-indexed metadata (`col_spec`,
  `col_header_align`, per-cell `cell_styles`) is reindexed
  automatically; a user-supplied `col_header` is instead resolved
  against the final columns (which already include the stub at position
  1 – see
  [`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md)).
  Sources that already render an indented stub (rtables / tern, tfrmt
  indented, gtsummary) come out pre-merged, so `stub_vars` does not
  apply to them. Column widths from the source are **not** carried
  through the merge – use `auto_width = TRUE` or pass explicit widths.

- stub_label, stub_indent, stub_group_summary:

  **Superseded** by `stub` (see
  [`stub_spec()`](https://ichirio.github.io/rtfreporter/reference/stub_spec.md)).
  Forwarded to
  [`stub_cols()`](https://ichirio.github.io/rtfreporter/reference/stub_cols.md)
  when `stub_vars` is set: the merged stub column's name / header
  (`NULL` joins the merged columns' display names with `" / "`), the
  non-breaking spaces per nesting level (default `4`), and which leaf
  values fold a row onto its group label row (default
  `c("empty", "parent")`). Ignored when `stub_vars` is `NULL`.

- header_sep:

  Separator(s) used to reconstruct a **spanning (multi-row) column
  header** from a plain **data.frame**'s column names. A bare data.frame
  carries no spanning metadata, so the nesting is parsed out of the
  names: each name is split on `header_sep` into segments that become
  stacked header rows, and horizontally adjacent columns sharing a label
  *and* the same ancestor path are merged into one spanning cell.
  Columns with no separator (e.g. id columns such as `label`) keep their
  name on the bottom (leaf) row with blank cells above. The default
  recognizes **both** built-in delimiters: `"____"` (from
  `ydisctools::pivot_stats_wider()`) and `"___tlang_delim___"` (tfrmt's
  column delimiter). Pass your own separator(s) to override, or `NULL`
  to disable (use the plain `names(data)` single-row header). Only
  applies to plain data.frame input – gt / gtsummary / rtables / tern /
  flextable / huxtable already carry real spanning metadata. With the
  `"____"` separator a doubled separator (`"________"`) yields an empty
  middle segment, i.e. a **blank cell** at that header level (handy to
  align a column that skips an intermediate spanner level). An explicit
  `col_header` (passed via `...`) always wins.

- auto_width:

  Logical (default `FALSE`). When `TRUE`, each column is sized to its
  widest content (column header label or data cell) via
  [`auto_col_widths()`](https://ichirio.github.io/rtfreporter/reference/auto_col_widths.md),
  so long row labels and column headers do not wrap. The widths are
  computed once on the full table and applied to every page, keeping
  paginated pages aligned. Ignored if you pass an explicit
  `column_widths_twips` or `col_rel_width`.

- table_width_twips:

  Optional total table width in twips, used only when
  `auto_width = TRUE`. When supplied, the auto-sized columns are scaled
  so their widths sum to this value (e.g. to fill, or fit within, the
  writable page width). `NULL` (default) uses each column's natural
  content width, but **capped at the default page's writable width**
  (landscape Letter, 0.75in margins) so a naturally over-wide table is
  scaled down to fit the page without you having to compute the width.

- border, style:

  Passed to
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  for every page. `border` defaults to `"tfl"`.

- ...:

  Further arguments forwarded to
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  for every page (e.g. `col_header`, `col_spec`, `row_title`,
  `col_rel_width`, `row_height_twips`). `row_title` names the
  row-heading columns (default: column 1) and sets the per-column
  default alignment (heading columns left, others centre). Explicit
  values always win over the gt-extracted ones.

  A user-supplied `col_header` is resolved against the **final printed
  columns** – after `stub_vars` folding and `drop_cols` removal – so its
  [`col_cell()`](https://ichirio.github.io/rtfreporter/reference/col_cell.md)
  positions and names refer to the columns you actually see (position 1
  = first printed column; the `stub_vars` stub is that column). This is
  equivalent to building the pages and then calling
  [`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md);
  use
  [`rtf_columns()`](https://ichirio.github.io/rtfreporter/reference/rtf_columns.md)
  to list the final names first. (`col_spec` / `col_rel_width` /
  `row_title` still use the pre-drop input coordinates.)

## Value

A list of `rtftable` objects, one per page. When the split is
value-based (or the input was a named list) the list is named.

## Details

1.  **Read the table's metadata** (only the parts the RTF renderer can
    use; see *What is carried* below).

2.  **Paginate.** The rendered body is split into per-page chunks using
    the same strategies the old
    [`paginate()`](https://ichirio.github.io/rtfreporter/reference/paginate.md)
    offered (`split`, `max_rows`, `group_col`, blank-row controls, ...).
    The shared header / width / spanning metadata is replicated onto
    every page.

The page-level title / source-note blocks travel with each returned
rtftable as the attributes `rtf_titles` / `rtf_footnotes`, which
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
consumes automatically.

Supported inputs: `gt_tbl`, `gt_group` (gt's multi-table container from
[`gt::gt_group()`](https://gt.rstudio.com/reference/gt_group.html) /
[`gt::gt_split()`](https://gt.rstudio.com/reference/gt_split.html) – and
what tfrmt's `print_to_gt()` returns when the spec has a `page_plan`;
expanded to one page set per member table), gtsummary tables (including
a `tbl_split` container from
[`gtsummary::tbl_split_by_rows()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_split_by.html)
/
[`tbl_split_by_columns()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_split_by.html),
likewise expanded member by member), rtables/tern `VTableTree` tables,
rlistings `listing_df` listings, `flextable` tables, `huxtable` tables,
plain `data.frame` / tibble, or a `list` of any of these (the list is
flattened, names propagated as `name`, `name.1`, `name.2`, ...). Figures
are out of scope – use
[`rtf_figures()`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.md)
for those.

## What is carried, by source

The body is always the table's *rendered* body – for gt/gtsummary via
[`gt::extract_body()`](https://gt.rstudio.com/reference/extract_body.html),
for rtables/tern and rlistings via
[`formatters::matrix_form()`](https://rdrr.io/pkg/formatters/man/matrix_form.html).
Only visible columns appear (hidden / helper columns such as tfrmt's
`..tfrmt_row_grp_lbl` are dropped), row-group / stub rows are already
interleaved, and indentation is rendered into the label text. (A gt
table with more than one stub column – e.g. a tfrmt
`row_grp_plan(label_loc = element_row_grp_loc(location = "column"))`
layout – is read from the table's own `_data` / `_boxhead` slots
instead, as
[`gt::extract_body()`](https://gt.rstudio.com/reference/extract_body.html)
does not support multiple stub columns; the group and label columns then
come through as ordinary columns.) On top of that body the following
*metadata* is read:

|  |  |  |  |  |
|----|----|----|----|----|
| **Metadata** | **gt / gtsummary** | **rtables / tern** | **flextable** | **huxtable** |
| Column (leaf) labels | yes | yes | yes | yes |
| Per-column alignment | yes | yes | yes | yes |
| Spanning headers | yes | yes | yes | yes |
| Column widths | yes (px/pct) | – | – | – |
| Title + subtitle | yes | yes | yes (caption) | yes (caption) |
| Footnotes / source notes | yes | yes | yes (footer) | – |
| In-cell footnote marks | yes (superscript) | yes (superscript) | – | – |
| Row-group rows + indent | yes (rendered) | yes (rendered) | yes (rendered) | yes (rendered) |
| [`tab_style()`](https://gt.rstudio.com/reference/tab_style.html) cell styles | yes (see below) | – | – | – |

**Explicit
[`gt::tab_style()`](https://gt.rstudio.com/reference/tab_style.html)
declarations are carried** (the `"styles"` token, on by default):
[`cell_borders()`](https://gt.rstudio.com/reference/cell_borders.html)
and the [`cell_text()`](https://gt.rstudio.com/reference/cell_text.html)
bold / italic / underline / align properties on column spanners, column
labels and body/stub cells, plus body text colour – everything the
`rtftable` class can hold. A border or underline on a column-label cell
promotes the label row to single-column cells (the same mechanism
[`style_header()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)
uses). The
[`style_header()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)
/
[`style_body()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)
/
[`style_cols()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)
verbs still override what was read – adapters populate first, verbs win
per side / per field. *Not* carried:
[`cell_fill()`](https://gt.rstudio.com/reference/cell_fill.html), fonts
and sizes, header text colour, and gt's **theme** borders
([`tab_options()`](https://gt.rstudio.com/reference/tab_options.html)) –
those describe gt's own default look; rtfreporter's `border` presets
govern the frame instead.

For flextable the *displayed* text is read (header labels set via
`set_header_labels()` and `colformat_*()` formatting included), not the
raw `$body$dataset`. Cells composed of images / equations and
`footnote()` reference marks are not carried. For huxtable the displayed
text is likewise read (its `number_format` applied); huxtable has no
footnote concept, so only the caption (a page title) is carried.

**Not carried** (RTF cannot reproduce these, or they belong to the
source's own theme): cell background colours / fills, font and size
styling, gt theme borders
([`tab_options()`](https://gt.rstudio.com/reference/tab_options.html)),
header text colour, and Markdown formatting inside labels or titles. For
a plain `data.frame` / tibble two pieces of metadata are read: column
`label` attributes become header labels (the `"labels"` token, see
`read_meta`), and delimited column names are reconstructed into a
spanning header (see `header_sep`); everything else (`col_spec`, widths,
...) you set yourself. See also
[`stub_cols()`](https://ichirio.github.io/rtfreporter/reference/stub_cols.md)
for finishing a tidy hierarchy-column data.frame into the indented stub
layout beforehand.

## Superseded arguments

These `...` arguments still work and are handy for simple one-shot
tables, but dedicated post-hoc verbs supersede them for anything beyond
a single label row – they address the **final** printed columns (by name
or visible position), are inspectable, and compose in a pipe:

- `col_header` -\>
  [`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md)
  (multi-row / spanning headers, name-based;
  [`rtf_columns()`](https://ichirio.github.io/rtfreporter/reference/rtf_columns.md)
  lists the columns and
  [`rtf_header_source()`](https://ichirio.github.io/rtfreporter/reference/rtf_header_source.md)
  shows the current header as editable source).

- column text styling / added header rows -\>
  [`style_cols()`](https://ichirio.github.io/rtfreporter/reference/style_header.md),
  [`style_header()`](https://ichirio.github.io/rtfreporter/reference/style_header.md),
  [`add_header_row()`](https://ichirio.github.io/rtfreporter/reference/style_header.md).

Prefer the verbs for complex headers; the arguments are kept for
convenience and backward compatibility.

## See also

[`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md)
/
[`rtf_columns()`](https://ichirio.github.io/rtfreporter/reference/rtf_columns.md)
/
[`rtf_header_source()`](https://ichirio.github.io/rtfreporter/reference/rtf_header_source.md)
and the styling verbs
[`style_cols()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)
/
[`style_header()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)
/
[`add_header_row()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)
(which supersede the header / styling `...` arguments);
[`as_rtftable()`](https://ichirio.github.io/rtfreporter/reference/as_rtftable.md)
for the single-page convenience wrapper;
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
to append the result to a document.

## Examples

``` r
# A plain data.frame: one page, no splitting.
df <- data.frame(
  Parameter = c("Age", "  Mean", "  SD", "Sex", "  F", "  M"),
  Value     = c("", "75.1", "8.2", "", "53%", "47%"),
  stringsAsFactors = FALSE
)
pages <- as_rtftables(df)               # length-1 list of rtftable
length(pages)
#> [1] 1

# Fixed-size pagination: 3 body rows per page.
pages <- as_rtftables(df, split = "rows", split_rows = 3)
length(pages)                           # 2 pages
#> [1] 2

# Blank separator rows: after row 3 and after the last row.
pages <- as_rtftables(df, blank_rows = c(3, -1))
pages[[1]]$blank_rows
#> [1] 3

if (FALSE) { # \dontrun{
library(gtsummary)
tbl <- trial |>
  tbl_summary(by = trt) |>
  as_rtftables()                       # list of rtftable pages

doc <- rtf_document() |>
  rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
  rtf_tables(tbl)                       # titles / footnotes flow through
generate_rtfreport(doc, "out.rtf", overwrite = TRUE)
} # }
```

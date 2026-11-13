# Convert a table object into rtfreporter table pages

`as_rtftables()` is the single entry point for turning a *table object*
into a list of
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
objects – one per RTF page – ready to hand straight to
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md).
It unifies two jobs that used to be split across `paginate()` (page
splitting) and `rtf_tables(read_gt = )` (metadata extraction), both
removed in 0.9.0:

## Usage

``` r
as_rtftables(x, ...)

# Default S3 method
as_rtftables(
  x,
  read_meta = TRUE,
  max_rows = NULL,
  split = c("none", "rows", "group_safe", "group_force", "by_value"),
  split_rows = NULL,
  group_col = NULL,
  group_by = c("auto", "indent", "value", "filled"),
  page_by = NULL,
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
  header_sep = .default_header_seps(),
  auto_width = FALSE,
  table_width_twips = NULL,
  border = "tfl",
  style = NULL,
  first_rows = NULL,
  ...
)
```

## Arguments

- x:

  A `gt_tbl`, a `gt_group`, a gtsummary table (or `tbl_split`
  container), an rtables/tern `VTableTree`, an rlistings `listing_df`, a
  `flextable`, a `huxtable`, a `data.frame` / tibble, or a `list` of
  these.

- ...:

  Further arguments forwarded to
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  for every page (e.g. `col_header`, `col_spec`, `row_title`,
  `col_rel_width`, `row_height_twips`). Explicit values always win over
  the gt-extracted ones.

  `row_title`

  :   names the row-heading columns (default: column 1) and sets the
      per-column default alignment (heading columns left, others
      centre).

  `col_header`

  :   A user-supplied `col_header` is resolved against the **final
      printed columns** – after `stub` folding and `drop_cols` removal –
      so its
      [`col_cell()`](https://ichirio.github.io/rtfreporter/reference/col_cell.md)
      positions and names refer to the columns you actually see
      (position 1 = first printed column; the `stub` column is that
      column). This is equivalent to building the pages and then calling
      [`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md);
      use
      [`rtf_columns()`](https://ichirio.github.io/rtfreporter/reference/rtf_columns.md)
      to list the final names first.

  `col_spec`, `col_rel_width`, `row_title`

  :   still use the pre-drop input coordinates.

- read_meta:

  Controls metadata extraction from the source table:

  `TRUE`

  :   (default) read everything in the table above.

  `FALSE`

  :   use only the rendered body – equivalent to the old `paginate()`.
      `NULL` is the same.

  a character vector

  :   of tokens, by source below.

  Anything else (`NA`, a number, two logicals, a token `NA`) is an
  error, for every source. The tokens:

  gt / gtsummary

  :   `"col_header"`, `"alignment"`, `"spanning"`, `"widths"`,
      `"titles"`, `"footnotes"`, `"footnote_marks"` (the footnote marks
      gt prints on the column labels, spanning headers, title, subtitle
      and row-group labels, as `^{N}` after the text, and each footnote
      line starting with its mark, `^{N}` then the note, in gt's order;
      the marks in the body cells are always written), `"styles"`
      (explicit
      [`tab_style()`](https://gt.rstudio.com/reference/tab_style.html)
      borders and text styles; see *What is carried*).

  rtables / tern

  :   `"col_header"`, `"alignment"`, `"spanning"`, `"titles"`,
      `"footnotes"`, `"footnote_marks"` (the indentation is always
      written into the labels); **rlistings takes the same tokens**,
      because the two share one `MatrixPrintForm` reader.

  flextable

  :   `"col_header"`, `"alignment"`, `"spanning"`, `"titles"`,
      `"footnotes"`.

  huxtable

  :   `"col_header"`, `"alignment"`, `"spanning"`, `"titles"`,
      `"footnotes"`.

  data.frame / tibble

  :   The single token `"labels"`: a column's `label` attribute (the
      haven / labelled / xportr convention) is used as its header label
      (columns without one keep their name). Tokens that only another
      source reads are *skipped* for data.frames (and every other source
      skips the data.frame's `"labels"`) – a
      [`list()`](https://rdrr.io/r/base/list.html) mixing table objects
      and data.frames shares one `read_meta`, so a token must not error
      on the elements that do not read it. A token that no source reads
      (a typo) is an error.

- max_rows:

  A positive whole number, or `NULL`; anything else is an error,
  whatever the split. Maximum body rows per page for the `"group_safe"`
  / `"group_force"` splits (required by them); with `1` a continued
  group gets no continuation row. **This is the setting for fixed-size
  pages** – `max_rows = 25` gives pages of 25 rows. Ignored by `"none"`
  and `"rows"` (which cuts at the positions in `split_rows`). Optional
  for `"by_value"`: a value's page longer than `max_rows` is cut as
  `"group_force"` cuts, its pages named by the value with the `"...n"`
  tail (`"A...1"`, `"A...2"`). For a `listing` and for an rlistings
  listing, `max_rows` alone switches `split` to `"group_safe"` (see
  `listing`).

- split:

  How to break the body into pages. A strategy name (a unique prefix
  such as `"group_s"` is enough; another value is an error):

  `"none"`

  :   (default) one page; no row limit checked.

  `"rows"`

  :   cut before each row position given in `split_rows`; requires
      `split_rows`. For fixed-size pages use `"group_safe"` /
      `"group_force"` with `max_rows`.

  `"group_safe"`

  :   pack **whole groups** onto a page: a group that does not fit in
      what is left starts the next page instead. A group that on its own
      exceeds `max_rows` has to be split, and is, with a continuation
      label. Requires `max_rows`. **This is the one that keeps a group
      together.**

  `"group_force"`

  :   cut **every** `max_rows` rows, wherever that falls: a group
      straddling the cut is split and the next page opens with a
      continuation label repeating its heading. Pages come out the same
      height, which is why a report asks for it — but it will split a
      group that would have fitted whole on the next page, so it is not
      `"group_safe"` with an escape hatch. Only `min_group_rows` pulls a
      cut back, and only to avoid a widow or an orphan. Requires
      `max_rows`. Ten groups of four rows with `max_rows = 30`,
      `blank_rows = "between_groups"`, `blank_row_first` and
      `blank_row_end` and the blanks counted
      (`count_blank_rows = TRUE`): `"group_safe"` prints 26 and 26 rows,
      `"group_force"` 30 and 24 with the sixth group cut.

  `"by_value"`

  :   one page per distinct value of `group_col`; the pages are named by
      that value. A page gathers every row with its value, scattered or
      not, in the body's own order.

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
  for re-creating the `" (Cont.)"` continuation row. A plan passes it
  the same way: `plan_paginate_rows(split = f)`.

- split_rows:

  Integer vector or `NULL`. Row **positions** to cut before, for
  `split = "rows"` (required by it; ignored otherwise).
  `split_rows = c(20, 40)` yields pages `1:19`, `20:39`, `40:nrow`. It
  is not a page size: for pages of a fixed number of rows use `max_rows`
  with `split = "group_safe"` or `"group_force"`. The positions are
  whole numbers in any order; one outside `2:nrow` cuts nothing.

- group_col:

  Character, integer, or `NULL`. The column the group-aware splits
  (`"group_safe"`, `"group_force"`, `"by_value"`) detect groups on,
  given by name or position (a whole number in `1..ncol`; `TRUE`, a
  fraction or a factor is an error, as in every column argument). `NULL`
  (default) uses **column 1** (for an rlistings listing, its first key
  column; see `listing`). This selects only the *column*; how a group
  boundary is found on it is set by `group_by`.

  Naming it

  :   For gt / gtsummary the body keeps gt's column **ids** (e.g.
      `"label"`, `"stat_1"`), and for rtables / flextable / huxtable the
      columns are renamed `V1`, `V2`, ... – so an **integer** index is
      the most portable.

  A factor

  :   A **factor** group column is rendered as its labels: it is coerced
      to character just before the split, so the group-aware splits can
      write the `cont_label` suffix into it. This happens *after*
      `sort_by`, which still orders a factor by its **levels**.

- group_by:

  How a group boundary is found on `group_col` (a unique prefix of a
  name is enough; another value is an error):

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

- page_by:

  Column(s) whose value makes a **page** and **names** it, or `NULL`
  (default, off). The body is partitioned by the `page_by` value(s)
  **first**, and every other pagination setting – `split`, `max_rows`,
  `group_col`, `group_by`, `min_group_rows`, `cont_label`, `blank_rows`,
  `collapse_repeats` – then applies **within** one partition. This is
  the two-level clinical layout: a BY variable (period, cohort, analysis
  set) owns the page, while an inner group (a parameter block) is kept
  whole inside it.

      as_rtftables(lab, page_by = "period",   # a page per period, named by it
                   split = "group_safe",      # from here on: inside one period
                   group_by = "indent", max_rows = 20,
                   drop_cols = "period")

  Page names

  :   the **outermost** value alone, carried by every page it makes: the
      `page_by` value, or – when the split names its own pages
      (`"by_value"`) and the group is therefore the outer axis – the
      group's. The two are never joined. A page name is the heading
      `rtf_tables(auto_section = TRUE)` / `auto_title = TRUE` prints and
      the line a section breaks on, so joining them would cut one group
      into a section per BY value; both values stay readable in the
      page's `rtf_paginate_meta` (`page_group` / `page_by`), and by
      column as `page_values` (`c(PARAM = "ALT")`), which a title or
      band writes as `{PARAM}` (see *Page values* in
      [`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)).
      Pages that share a heading are numbered with a **`"...n"` tail** –
      `"Period 1...1"`, `"Period 1...2"` – so the list stays
      addressable, R returning the *first* match for a repeated list
      name (which is what `pages[["Period 1"]]` and RStudio's
      [`View()`](https://rdrr.io/r/utils/View.html) would show). The
      tail is not part of the heading: `auto_section` / `auto_title`
      strip it, so the printed heading is `"Period 1"` and its pages are
      one section. A heading used once carries no tail.

  `group_col`

  :   when left `NULL`, defaults to the first column **not** named in
      `page_by`, so `group_by = "indent"` (which reads the group column)
      never lands on the BY column – where every row of a partition
      carries the same value and there is no structure to find.

  Order of operations

  :   `page_by` cuts pages, then `split` cuts rows inside each one, then
      [`paginate_cols()`](https://ichirio.github.io/rtfreporter/reference/paginate_cols.md)
      cuts columns on the result (its `order` decides the final page
      sequence). With `split = "by_value"` the **group is the outer
      axis** and `page_by` the inner one – a group's pages stay together
      (and share its name), the BY values running in order inside it –
      so the three come out as `G` / `C` / `P` under `order = "across"`
      and `G` / `P` / `C` under `"down"`.

  Per partition

  :   `split_rows` positions, `blank_rows` positions (`0` / `-1`
      included), `count_blank_rows` accounting and any `rtf_blank_rows`
      attribute on the input are all resolved inside the partition. `na`
      and `cell_format`, by contrast, run **body-wide** before the
      partition, so one column width is shared by every page.

  A page gathers its rows

  :   a value that comes back later in the body belongs to the page it
      names, not to a new one, and the rows keep the order the body had
      – inside a page and between pages. `split = "by_value"` reads
      `group_col` the same way. Needing the body sorted first would be
      SAS's contract, not R's, so a merely interleaved body loses
      nothing; use `sort_by` when you want an order, not to make the
      pages whole.

  A row without a BY value

  :   a row whose `page_by` value is `NA` or blank (`""`) is not given a
      page of its own: it takes the next value in the body, or – at the
      foot of the body, where there is none – the one before it, and
      prints on that value's page under its title and header. A body
      with no value at all is one page, named `""`. This is meant for a
      stub's label rows (a row without its own BY value belongs to the
      group around it), so a real missing value in a data frame lands on
      a neighbouring value's page. To give it a page of its own, fill
      the value first, e.g.
      `lab$period[is.na(lab$period)] <- "Missing"`. A listing cut with
      [`plan_paginate_rows()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_rows.md)
      `(page_by = )` does the opposite: a record with a blank BY value
      is a page of its own, named by `na_label`.

  `page_by` does **not** imply `drop_cols`: name the column there too to
  keep it out of the printed table. Like `group_col` / `sort_by` /
  `drop_cols` it addresses the body **after** a `stub` reshape, so a
  column the stub consumed cannot be a `page_by` key (use
  `split = "by_value"`, which splits before the stub is built). With
  `page_by = NULL` nothing changes anywhere – `"by_value"` still takes
  its page names from `group_col`.

- sort_by:

  Columns to **order the body rows by, before pagination**, or `NULL`
  (default, keep the input order). A character / integer vector (or a
  [`list()`](https://rdrr.io/r/base/list.html) to mix names and indices)
  in the **input body's** coordinates – the same space as `group_col`,
  `collapse_repeats` and `drop_cols`.

  Order of work

  :   The sort is applied *first*, so group detection, `(Cont.)` labels
      and `blank_rows` positions all see the sorted order.

  Hidden keys

  :   A sort key may also be listed in `drop_cols` to order on a column
      that is not printed (the classic hidden "sort-key" carrier
      column).

  Ties and `NA`

  :   The sort is **stable** (rows that compare equal keep their input
      order) and `NA` keys sort **last** regardless of `sort_desc`.

  Which bodies

  :   `sort_by` reorders rendered body rows as-is, so it suits flat /
      tabular bodies (a `data.frame`, or a pre-flattened table); on a gt
      / rtables body whose group-label and child rows are already
      interleaved it would scramble that visual hierarchy – sort the
      source data before building such a table instead.

- sort_desc:

  Sort direction for `sort_by`: `NULL`/`FALSE` (default, all ascending),
  a single `TRUE` (all `sort_by` columns descending), or a logical
  vector the same length as `sort_by` (one direction per key). Ignored
  when `sort_by` is `NULL`.

- cont_label:

  Character (default `" (Cont.)"`). Suffix appended to a group's label
  on the second and later pages it continues onto (the group-aware
  splits), marking a continued group. One string; `""` repeats the label
  alone. A group whose only rows past a cut are blank rows – a listing
  record's blank row, or the separators `blank_rows` counts – is not
  continued: no continuation row, and those blank rows, which would open
  the next page, are dropped.

- min_group_rows:

  A non-negative whole number (default `2`). Widow/orphan control for
  the group-aware splits (`"group_force"`, `"group_safe"`,
  `"by_value"`): when a page would end on a group that *starts* on that
  page while showing fewer than `min_group_rows` of the group's child
  rows (the rows after its first row), the whole group is moved to the
  next page; and a cut inside a group is pulled back so at least
  `min_group_rows` of its rows go on to the next page. This prevents a
  lone group header being stranded at the foot of a page with none (or
  too few) of its members. Set to `0` to disable (the previous
  behaviour).

- blank_rows:

  Where to insert blank separator rows in the body. `NULL` (default)
  inserts none (but an `rtf_blank_rows` attribute already on the input
  is still honoured). Accepts any of – or a
  [`list()`](https://rdrr.io/r/base/list.html) combining:

  an integer vector of positions

  :   a blank row is inserted *after* each given data-row index; `0` =
      before the first row, `-1` = after the last row (e.g.
      `c(0, 5, -1)`). A position is a whole number in `-1..nrow` of the
      whole body (after `stub`, before pagination): a fraction, `NA`, or
      a position past the body's last row (or below `-1`) stops, naming
      the positions. The positions count rows of the whole body, not of
      each page: `c(3, 12)` puts one blank after body row 3 and one
      after body row 12, once each, on the pages those rows land on, and
      a page that no position falls on gets none. `0` is before the
      body's first row (top of page 1) and `-1` after its last (foot of
      the last page). A blank after a row that ends a page stays at the
      foot of that page; it is not carried to the top of the next one.
      Under `count_blank_rows = TRUE` the blank takes a row of the page,
      so where its page has no room for it (its row fills the page to
      `max_rows`, or `0` does not fit beside the first group) it is
      dropped, not carried over.

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
  before the split and re-attached per page afterwards. A separator
  blank is never left at a page's top or foot; an integer position is
  the whole body's, so `0` takes a row at the top of page 1 only, and a
  position that the cut leaves at the top of a later page (it follows
  the previous page's last row) is dropped rather than carried over; no
  page is made of blank rows alone.

- align_count_pct:

  The cell alignment to apply before pagination.

  `FALSE`

  :   (default) none.

  `TRUE`

  :   the built-in cell format `"count_pct"`, which realigns
      `"n (xx.x)"` count/percent cells to a uniform width.

  a name or a function

  :   The **name of any built-in** (`"count_paren"`, `"value_paren"`,
      `"right_align"`, ... – the `fmt_` prefix is optional) or a
      function of your own, in which case it behaves exactly like
      `cell_format`.

  Ignored when `cell_format` is supplied, which takes precedence.
  Neither one controls whether missing values are substituted – that is
  `na` – only whether the substituted token is aligned with the counts.

- cell_format:

  Optional cell re-formatter applied column-by-column to the body
  **before** pagination, for monospaced alignment.

  One formatter

  :   applied to every data column (columns 2..N; the row-label column 1
      is left alone).

  A list

  :   of them taken positionally (`cell_format[[j]]` for column `j`;
      entries that are neither a function nor a name are skipped).

  A formatter

  :   is a **function** taking one column (a character vector) and
      returning a character vector of the same length, or the **name**
      of a built-in as a string – `"right_align"`, `"count_paren"`,
      `"count_paren_bare"`, `"value_paren"`, `"count_pct"`, with the
      `fmt_` prefix optional, so `"fmt_count_paren"` works too.

  Precedence

  :   When supplied it takes precedence over `align_count_pct`.

      as_rtftables(x, cell_format = "value_paren")
      as_rtftables(x, cell_format = list(NULL, "right_align", my_fmt))

  See
  [`fmt_count_paren()`](https://ichirio.github.io/rtfreporter/reference/fmt_count_paren.md)
  /
  [`fmt_value_paren()`](https://ichirio.github.io/rtfreporter/reference/fmt_value_paren.md)
  /
  [`fmt_right_align()`](https://ichirio.github.io/rtfreporter/reference/fmt_right_align.md)
  for what each one does, and the contract in
  [`?fmt_right_align`](https://ichirio.github.io/rtfreporter/reference/fmt_right_align.md)
  for writing your own.

- na:

  Text to print for a **missing value** (default `""`, an empty cell –
  the previous behaviour).

  Where

  :   Applied to every column, the row-label column included, and
      **independently of `align_count_pct` / `cell_format`** – those
      decide whether the substituted token is also *aligned*, not
      whether the substitution happens. With one of them set, the token
      is right-justified in the count field so its right edge lands on
      the ones digit (below).

  Not missing

  :   Text already in the data that is *not* missing (`"NE"`, `"n/a"`, a
      literal `"-"` you typed yourself) does not match the count pattern
      and passes through unchanged, with no padding. The *strings*
      `"NA"` / `"NaN"` are never touched – `"NA"` can be legitimate
      data.

  `NaN`, `Inf`

  :   `NaN` counts as missing (R's own
      [`is.na()`](https://rdrr.io/r/base/NA.html) says so, and in a TFL
      a `NaN` is a `0/0` percentage). `Inf` / `-Inf` do **not**: they
      print as `"Inf"` / `"-Inf"`, because an infinity means a division
      by zero upstream and rendering it as `"-"` would hide the bug.

  Order of work

  :   The substitution happens before the split, so the `NA`s
      `collapse_repeats` writes per page to blank out repeated values
      stay blank.

        1 ( 1.2%)
      108 (35.3%)
        -            <- na = "-"
       NE            <- na = "NE", right edge still under the ones digit

- collapse_repeats:

  Columns in which to blank **consecutive repeated values** (repeat
  suppression), or `NULL` (default, off). A character / integer vector
  naming the columns in priority order.

  What is blanked

  :   Within each column, only the first value of a run is kept; the
      rest of the run is replaced with `NA` (which renders as an empty
      cell – no row is removed, only the display text is suppressed).

  Several columns

  :   The suppression is **hierarchical**: the first column is collapsed
      on its own value, and each later column on the *combination* of
      itself with all earlier listed columns (a change in any higher
      column restarts the lower column's run).

  When

  :   This runs **per page, after the split**, so the pagination still
      sees the original repeated values – group boundaries and `(Cont.)`
      labels stay correct, and a group continued onto the next page
      shows its label again at the top. (In `group_by` terms:
      value-based grouping happens first, then the column is collapsed
      to a `"filled"`-style display.)

- drop_cols:

  Columns to **hide from the printed table while still using them for
  pagination / grouping**, or `NULL` (default, drop nothing). A
  character / integer vector (or a
  [`list()`](https://rdrr.io/r/base/list.html) to mix names and indices)
  naming the columns in the **input body's** coordinates – the same
  coordinate space as `group_col` and `collapse_repeats`.

  What stays

  :   The named columns stay present through the split (so `group_col`,
      `collapse_repeats` and
      [`blank_rows_by_change()`](https://ichirio.github.io/rtfreporter/reference/blank_rows_by_change.md)
      can reference them), then are removed from **every page** before
      that page's table is rendered. This makes a column usable as a
      hidden grouping / sort-key / carrier column without it appearing
      in the report. `drop_cols` must leave at least one column to
      display.

  Reindexed

  :   Position-indexed metadata (`col_spec`, column widths,
      `col_header_align`, `row_title`, and per-cell `cell_styles`) is
      reindexed automatically to the remaining columns.

  `col_header`

  :   A **user-supplied `col_header`** is the exception: it is applied
      against the **final printed columns** (see the `col_header`
      argument and
      [`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md)),
      so you write its positions / names for the columns that remain –
      not the pre-drop layout.

  With `split`

  :   For `split = "by_value"` the page **names** come from the
      `group_col` value, so that column may be dropped and the pages are
      still named by it; but under `"group_safe"` / `"group_force"` the
      `" (Cont.)"` marker is written into the `group_col` cell, so if
      `group_col` is itself dropped the marker is not shown (group on
      the visible label column if the marker is wanted).

- listing:

  Listing settings, as a
  [`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md)
  object, or `NULL` (default, no listing preparation). Applies to a
  **data.frame / tibble** source.

  What it does

  :   [`build_listing()`](https://ichirio.github.io/rtfreporter/reference/build_listing.md)
      reshapes the source into a listing body – source variables joined
      into their printed columns, long cells wrapped over several
      physical rows, gutter columns, a blank row after each record – and
      the spec's columns then supply the `col_header`, the relative
      widths and the (left) alignment, so none of the three has to be
      written out by hand.

  Keeping a record whole

  :   The hidden record column is used to keep a record whole across a
      page break: `group_col` points at it, `split` becomes
      `"group_safe"` when `max_rows` is set, and it is added to
      `drop_cols`. Every one of those is a **default** – an argument you
      pass yourself is never overridden.

  Already built

  :   A body that has already been through
      [`build_listing()`](https://ichirio.github.io/rtfreporter/reference/build_listing.md)
      carries its own spec, so `as_rtftables()` picks it up and passing
      `listing` as well is an error. An rlistings `listing_df` is
      rejected: it is already laid out, and needs no `listing` argument.

  An rlistings listing

  :   Pages by its first key column instead: `split` becomes
      `"group_safe"` when `max_rows` is set, and a group-aware split
      gets `group_col` on the listing's first key column – found by name
      among the display columns, wherever `listing_dispcols()` puts it –
      with `group_by = "filled"`, since rlistings prints a key once per
      run. A subject's rows then stay on one page when they fit; a
      longer one is cut as `"group_safe"` cuts it, the key repeated with
      `cont_label`. A listing with no key columns groups on column 1.
      These are **defaults** too: `split`, `group_col` and `group_by`
      you pass yourself win, and without `max_rows` the listing is one
      page.

- stub:

  Row-stub settings, as a
  [`stub_spec()`](https://ichirio.github.io/rtfreporter/reference/stub_spec.md)
  object – or, as a shorthand for `stub_spec(vars)`, a bare vector of
  hierarchy columns (parent first, leaf last; one column is the stub as
  it is). `NULL` (default) builds no stub. This is the **one** argument
  for everything
  [`stub_cols()`](https://ichirio.github.io/rtfreporter/reference/stub_cols.md)
  can do, including `layout` and `label_span`.

  Applied to

  :   The **extracted body** – after the table is pulled out of its
      source, before pagination – so any input that carries the
      hierarchy as *separate columns* (a plain `data.frame`, a `gt(df)`,
      a multi-group-column table, or a tfrmt
      `row_grp_plan(location = "column")` table) gains a clinical stub.

  `layout`

  :   With `layout = "merged"` the named columns are consumed and
      replaced by one stub column at position 1 and the remaining
      columns keep their order; with `layout = "columns"` every column
      stays where it is. Either way `drop_cols` / `group_col` /
      `sort_by` (plus `group_by = "indent"` detection and the
      group-aware splits) then refer to the reshaped body.

  `split = "by_value"`

  :   **Exception:** the body is split by `group_col` *first* and the
      stub is built per page afterwards – each value becomes an
      independent section, so `group_col` here refers to the
      **pre-stub** columns and is not folded into the stub. This keeps a
      fixed intermediate hierarchy level (e.g. `LBCAT / group1 / label`
      with a constant `group1`) from collapsing into a single stub label
      row that spans every group; put the inner levels in the stub and
      the outer level in `group_col`.

  Metadata

  :   Position-indexed metadata (`col_spec`, `col_header_align`,
      per-cell `cell_styles`) is reindexed automatically; a
      user-supplied `col_header` is instead resolved against the final
      columns (see
      [`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md)).

  Sources that do not need it

  :   Sources that already render an indented stub (rtables / tern,
      tfrmt indented, gtsummary) come out pre-merged, so a stub does not
      apply to them.

  Widths

  :   Under `layout = "merged"` the source column widths are **not**
      carried through the merge – use `auto_width = TRUE` or pass
      explicit widths; `layout = "columns"` keeps them.

- header_sep:

  Separator(s) used to reconstruct a **spanning (multi-row) column
  header** from a plain **data.frame**'s column names. Only applies to
  plain data.frame input – gt / gtsummary / rtables / tern / flextable /
  huxtable already carry real spanning metadata. An explicit
  `col_header` (passed via `...`) always wins.

  How

  :   A bare data.frame carries no spanning metadata, so the nesting is
      parsed out of the names: each name is split on `header_sep` into
      segments that become stacked header rows, and horizontally
      adjacent columns sharing a label *and* the same ancestor path are
      merged into one spanning cell. Columns with no separator (e.g. id
      columns such as `label`) keep their name on the bottom (leaf) row
      with blank cells above.

  Default

  :   Recognizes **both** built-in delimiters: `"____"` (from
      `ydisctools::pivot_stats_wider()`) and `"___tlang_delim___"`
      (tfrmt's column delimiter).

  Your own

  :   Pass your own separator(s), a character vector, to override, or
      `NULL` (or `character(0)`) to disable (use the plain `names(data)`
      single-row header). `NA` and `""` entries are ignored; another
      type is an error.

  Blank cells

  :   With the `"____"` separator a doubled separator (`"________"`)
      yields an empty middle segment, i.e. a **blank cell** at that
      header level (handy to align a column that skips an intermediate
      spanner level).

- auto_width:

  Logical (default `FALSE`). When `TRUE`, each column is sized to its
  widest content (column header label or data cell) via
  [`auto_col_widths()`](https://ichirio.github.io/rtfreporter/reference/auto_col_widths.md),
  so long labels do not wrap. It decides how the columns divide the
  table, not how wide the table is: the total comes from
  `table_width_twips`, else `table_width_pct`, else the natural content
  width capped at the default page's writable width. An explicit
  `column_widths_twips` or `col_rel_width` always wins over it.

- table_width_twips:

  Total table width in **twips**, or `NULL` (default). This is the same
  setting as `rtftable(table_width_twips = )` and means the same thing:
  the table is exactly this wide, and `col_rel_width` apportions it. It
  is independent of `auto_width` – it says how wide the TABLE is, while
  `auto_width` decides how the COLUMNS divide it – and it wins over
  `table_width_pct`, the same total said as a share of the page. `NULL`
  leaves the total to `table_width_pct` if given, and otherwise to the
  writable page width.

  The whole vocabulary, in the order it resolves:

  `column_widths_twips`

  :   per column, absolute – wins outright, and the table's total is
      their sum.

  `col_rel_width`

  :   per column, relative – apportions whatever the total turns out to
      be.

  `table_width_twips`

  :   the total, absolute.

  `table_width_pct` / `table_width_pct_of_writable`

  :   the total, as a share of the writable page width (paper minus the
      side margins). Used only when `table_width_twips` is not given:
      the twips win. Give one of the two percentages, not both; given
      both, `table_width_pct` wins, as in
      [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md).

  nothing

  :   the writable width: the row-heading columns by their content, the
      data columns sharing the rest
      ([`default_col_widths()`](https://ichirio.github.io/rtfreporter/reference/default_col_widths.md)).

  All of them are
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)'s,
  and mean here exactly what they mean there.

- border, style:

  Passed to
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  for every page. `border` defaults to `"tfl"`.

- first_rows:

  The rows of the first page (a whole number, 1 or more; as many as the
  table has or more is one page), the rest paginated as the other
  arguments say (`max_rows`, `split`): the page a table shares with the
  content before it,
  [`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)`(page_break = FALSE)`.
  Under `"group_safe"` / `"group_force"` a group the cut falls inside
  goes on with its continuation row on the next page, counted in its
  `max_rows`, as a cut by `max_rows` does. A **listing** record is not
  cut: the first page ends before the record the row count falls in.
  `NULL` (the default): the first page as the others. Not with
  `page_by`, `split = "by_value"` or a custom `split`.

## Value

A list of `rtftable` objects, one per page. When the split is
value-based (or the input was a named list) the list is named.

## Details

1.  **Read the table's metadata** (only the parts the RTF renderer can
    use; see *What is carried* below).

2.  **Paginate.** The rendered body is split into per-page chunks using
    the same strategies the old `paginate()` offered (`split`,
    `max_rows`, `group_col`, blank-row controls, ...). The shared header
    / width / spanning metadata is replicated onto every page.

The page-level title / source-note blocks travel with each returned
rtftable as the attributes `rtf_titles` / `rtf_footnotes`, which
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
consumes automatically.

Supported inputs: `gt_tbl`, `gt_group` (gt's multi-table container from
[`gt::gt_group()`](https://gt.rstudio.com/reference/gt_group.html) /
[`gt::gt_split()`](https://gt.rstudio.com/reference/gt_split.html) – and
what tfrmt's `print_to_gt()` returns when the spec has a `page_plan`;
expanded to one page set per member table; a style on a cell of a
row-split member, `gt::gt_split(row_every_n = )`, needs gt 1.4.0 or
later, as older gt stops on it), gtsummary tables (including a
`tbl_split` container from
[`gtsummary::tbl_split_by_rows()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_split_by.html)
/
[`tbl_split_by_columns()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_split_by.html),
likewise expanded member by member), rtables/tern `VTableTree` tables,
rlistings `listing_df` listings, `flextable` tables, `huxtable` tables,
plain `data.frame` / tibble, or a `list` of any of these (the list is
flattened, names propagated as `name`, repeated on every page it makes,
with the `"...n"` tail of pages that share a heading: `"name...1"`,
`"name...2"`). Figures are out of scope – use
[`rtf_figures()`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.md)
for those.

## What is carried, by source

The body is always the table's *rendered* body – for gt/gtsummary via
[`gt::extract_body()`](https://gt.rstudio.com/reference/extract_body.html),
for rtables/tern and rlistings via
[`formatters::matrix_form()`](https://pharmaverse.github.io/formatters/latest-tag/reference/matrix_form.html).
Only visible columns appear (hidden / helper columns such as tfrmt's
`..tfrmt_row_grp_lbl` are dropped), row-group / stub rows are already
interleaved, and indentation is rendered into the label text. (A gt
table with more than one stub column – e.g. a tfrmt
`row_grp_plan(label_loc = element_row_grp_loc(location = "column"))`
layout – is read from the table's own `_data` / `_boxhead` slots
instead, as
[`gt::extract_body()`](https://gt.rstudio.com/reference/extract_body.html)
does not support multiple stub columns; the group and label columns then
come through as ordinary columns, each headed as gt prints it: by its
own stubhead label when the stubhead has one per stub column
([`gt::tab_stubhead()`](https://gt.rstudio.com/reference/tab_stubhead.html),
or a tfrmt `col_plan()` that renames them), the first by a single
stubhead label and the others empty, and all empty when no stubhead is
set – never by the variable name. A gt table grouped by several columns,
`gt(groupname_col = c("ARM", "SEX"))`, gets one group row per
combination, named as gt names it: "A - F".) On top of that body the
following *metadata* is read:

|  |  |  |  |  |
|----|----|----|----|----|
| **Metadata** | **gt / gtsummary** | **rtables / tern** | **flextable** | **huxtable** |
| Column (leaf) labels | yes | yes | yes | yes |
| Per-column alignment | yes | yes | yes | yes |
| Spanning headers | yes | yes | yes | yes |
| Column widths | yes (px/pct) | – | – | – |
| Title + subtitle | yes (caption too) | yes | yes (caption) | yes (caption) |
| Footnotes / source notes | yes | yes | yes (footer) | yes (`add_footnote()` rows) |
| In-cell footnote marks | yes (superscript, labels and footnote lines too) | yes (superscript) | as text | – |
| Row-group rows + indent | yes (rendered) | yes (rendered) | group rows, no indent | as typed, no indent |
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

**A spanner is aligned as its source prints it**, not after the columns
under it: gt and gtsummary (and tfrmt, through gt) centre it, unless
`tab_style(cell_text(align = ), locations = cells_column_spanners())`
sets it (with the `"styles"` token); rtables / tern centre it; flextable
takes the alignment of its first header cell (`align(part = "header")`);
huxtable the `align()` of its first cell (`"."`, decimal alignment, as
right). A spanner you write yourself
([`col_cell()`](https://ichirio.github.io/rtfreporter/reference/col_cell.md),
[`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md),
[`plan_col_header()`](https://ichirio.github.io/rtfreporter/reference/plan_col_header.md),
or one rebuilt from a data frame's names with `header_sep`) takes its
leftmost column's header alignment unless `col_cell(align = )` is given.

A gt caption
([`gt::tab_caption()`](https://gt.rstudio.com/reference/tab_caption.html),
and so gtsummary's
[`modify_caption()`](https://www.danieldsjoberg.com/gtsummary/reference/modify_caption.html))
is the first row of the page title, above the
[`tab_header()`](https://gt.rstudio.com/reference/tab_header.html) title
and subtitle, as gt prints it; on a `gt_group` / `gt_split` each
member's own caption is read. It is read with the `"titles"` token, so
titles you give yourself (`rtf_tables(titles = )`,
[`rtf_titles()`](https://ichirio.github.io/rtfreporter/reference/rtf_titles.md))
replace it as they replace the gt title.

For flextable the *displayed* text is read (header labels set via
`set_header_labels()` and `colformat_*()` formatting included), not the
raw `$body$dataset`. Cells composed of images / equations are not
carried. A `footnote()` reference mark is read as plain text, not
superscript: it follows the cell's text (`"55.1a"`) and leads its footer
line (`"aNote"`). The group rows `as_grouped_data()` makes are rows of
text (`"g: A"`) and the rows under them get no indent (the cell padding
is not read). For huxtable the displayed text is likewise read (its
`number_format` applied), and the caption is the page title. huxtable
has no footnote concept of its own; the rows `add_footnote()` appends (a
last row whose first cell spans the table) are read as the footnote
lines, not as body rows – with one column the shape cannot be told from
a row, so they stay in the body then. Only the `header_rows()` from the
first row on are the column header, and the text of a cell is read as
typed (leading spaces kept; `left_padding()` is not read, so there is no
indent).

**Not carried** (RTF cannot reproduce these, or they belong to the
source's own theme): cell background colours / fills, font and size
styling, gt theme borders
([`tab_options()`](https://gt.rstudio.com/reference/tab_options.html)),
header text colour, and Markdown formatting inside labels, titles or a
caption (its bold markers go: `**Table 9** Summary` is read as
`Table 9 Summary`; a
[`gt::html()`](https://gt.rstudio.com/reference/html.html) label is read
as its text, a `<br>` as a line break). For a plain `data.frame` /
tibble two pieces of metadata are read: column `label` attributes become
header labels (the `"labels"` token, see `read_meta`), and delimited
column names are reconstructed into a spanning header (see
`header_sep`); everything else (`col_spec`, widths, ...) you set
yourself. See also
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

## Supplying a source from another package

`as_rtftables()` is an S3 generic. Everything listed above is handled by
the default method, because those packages do not know rtfreporter, so
rtfreporter carries their adapters. A package that *does* depend on
rtfreporter can make its own class a source by registering a method,
`as_rtftables.<class>(x, ...)`. The method must return what the default
returns: a list of `rtftable` pages, usually by building a table and
calling `as_rtftables()` on it.
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
then accepts the object directly, converting it the way it converts a
`gt_tbl`. rtfreporter itself names no such class.

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

# A custom page-break rule: the first three rows, then the rest.
three <- function(df, ...) list(df[1:3, ], df[-(1:3), ])
length(as_rtftables(df, split = three))  # 2 pages
#> [1] 2
length(pages)
#> [1] 1

# Cut before the given row positions: pages 1:3, 4:5, 6:6.
pages <- as_rtftables(df, split = "rows", split_rows = c(4, 6))
vapply(pages, function(p) nrow(p$data), integer(1))   # 3 2 1
#> [1] 3 2 1

# Fixed-size pages -- a different setting.  `"group_force"` also repeats the
# group label as a "(Cont.)" row when a cut lands inside a group, so the six
# body rows come out as four pages of two rather than three.
pages <- as_rtftables(df, split = "group_force", max_rows = 2)
vapply(pages, function(p) nrow(p$data), integer(1))   # 2 2 2 2
#> [1] 2 2 2 2

# Blank separator rows: after row 3 and after the last row.
pages <- as_rtftables(df, blank_rows = c(3, -1))
pages[[1]]$blank_rows
#> [1] 3 6

if (requireNamespace("gtsummary", quietly = TRUE)) {
  tbl <- gtsummary::trial |>
    gtsummary::tbl_summary(by = trt) |>
    as_rtftables()                     # list of rtftable pages
  doc <- rtf_document() |>
    rtf_tables(tbl)                     # titles / footnotes flow through
  generate_rtfreport(doc, tempfile(fileext = ".rtf"), overwrite = TRUE)
}
```

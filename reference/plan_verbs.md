# Declare the table, one layer at a time

Each verb adds a layer to an
[`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).
**A later layer wins.** The roles — which column goes across, which go
down, which carries the row identity — are said once, on
[`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md);
these verbs are the things a report really does declare twice. Their
arguments are the ones
[`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)
and
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
already take, so the layering is the only new idea.

## Usage

``` r
plan_levels(plan, ..., .drop_empty = NULL)

plan_labels(plan, ...)

plan_cells(plan, ..., stats = NULL, value = NULL, na = NULL, notes = NULL)

plan_digits(plan, ..., rounding = NULL)

plan_stub(
  plan,
  vars = NULL,
  name = NULL,
  indent = NULL,
  group_summary = NULL,
  before = FALSE
)

plan_cell_style(
  plan,
  cols = NULL,
  header = FALSE,
  where = NULL,
  bold = NULL,
  italic = NULL,
  align = NULL,
  color = NULL,
  background = NULL,
  border = NULL,
  underline = NULL,
  indent_twips = NULL
)

plan_paginate_group(plan, col = NULL, keep = TRUE)

plan_row_group(plan, mode = NULL, collapse = NULL, group_col = NULL)

plan_nest(plan, ...)

plan_total(plan, label = "Total", position = c("last", "first"))

plan_hide(plan, ...)

plan_sort(plan, ..., stat = NULL, keep = TRUE)

plan_blanks(plan, where = NULL, first = NULL, last = NULL, counted = NULL)

plan_paginate_rows(
  plan,
  max_rows = NULL,
  split = NULL,
  break_before = NULL,
  min_group_rows = NULL,
  cont_label = NULL,
  page_by = NULL
)

plan_style(
  plan,
  border = NULL,
  align_count_pct = NULL,
  font = NULL,
  font_size_half_points = NULL,
  row_height_twips = NULL,
  row_height_exact = NULL,
  header_row_height_twips = NULL,
  blank_row_height_twips = NULL,
  cell_padding_left_twips = NULL,
  cell_padding_right_twips = NULL,
  cell_valign = NULL,
  table_align = NULL,
  markup = NULL,
  blank_row_normalize = NULL,
  border_header = NULL,
  border_spanning = NULL,
  border_body = NULL,
  border_first_row = NULL,
  border_last_row = NULL,
  header_align = NULL,
  header_bold = NULL,
  header_italic = NULL,
  align = NULL,
  bold = NULL,
  italic = NULL,
  underline = NULL,
  table_width_twips = NULL,
  table_width_pct = NULL,
  table_width_pct_of_writable = NULL
)

plan_col_header(
  plan,
  header = NULL,
  values = NULL,
  header_sep = NULL,
  col_header_align = NULL,
  lines = NULL,
  span = "each"
)

plan_columns(
  plan,
  widths = NULL,
  decimal = NULL,
  row_title = NULL,
  auto_width = NULL,
  sep = NULL,
  cell_format = NULL,
  column_widths_twips = NULL
)

plan_listing(
  plan,
  ...,
  type = NULL,
  sep = NULL,
  spacer = NULL,
  spacer_rel_width = NULL,
  layout = NULL,
  wrap = NULL
)

plan_titles(plan, ..., pages = NULL)

plan_footnotes(plan, ..., pages = NULL)

plan_after(plan, ...)

plan_paginate_cols(
  plan,
  at = NULL,
  cut_by = NULL,
  every = NULL,
  keep = NULL,
  col_header = NULL,
  fit = NULL,
  allow_span_break = NULL,
  order = NULL
)
```

## Arguments

- plan:

  An
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).

- ...:

  For `plan_cells()`, exactly what `widen_ard(cells = )` takes: one bare
  entry, or entries named by variable, `context`, kind (`continuous` /
  `categorical`) or `default`.

  `plan_digits()` takes the same keys, with the digits as the value. A
  value is **one number**, for every token in that entry, or a vector
  **named by statistic** — a house rule is rarely one number. The two
  keys combine, which is the point of the verb:

      plan_digits(continuous  = c(mean = 2, sd = 3, median = 2),
                  categorical = c(p = 1)) |>      # the house rule
        plan_digits(AGE = c(mean = 1, sd = 2))    # AGE only

  A value is the **decimals**, or `"4s"` for **4 significant digits** —
  the token grammar's own distinction (`{mean:.4f}` / `{mean:.4s}`), not
  a second argument to learn:

      plan_digits(continuous = c(mean = "4s", sd = "5s", n = 0))

  A statistic the narrower entry says nothing about falls through to the
  wider one, so AGE's `median` stays at 2 above. Digits only reach a
  token that left the question open (`{mean}`, or `{p:\%}`): one that
  answered it (`{mean:.2f}`) keeps its answer, so a template meant to be
  tuned is written open.

  A **finished table** has no templates to fill — a source that is
  already the table, or statistics laid out as rows
  (`plan_cells(stats = "rows")`) — so there `plan_digits()` formats the
  numbers themselves, with
  [`fmt_numeric()`](https://ichirio.github.io/rtfreporter/reference/fmt_numeric.md):
  a key that names a **column** formats that column, and `.rows` formats
  the value columns by the text of the label column:

      plan_digits(.rows = c(N = 0, Mean = 1, SD = "3s"))

  A key that reaches neither a variable nor a column is an error, not
  silence.

  For `plan_nest()`, one entry per nested variable: its name, and the
  variable and level its rows go under,
  `plan_nest(RACESUB = c(RACE = "Asian"))`. The level is matched as the
  table shows it (the ARD's level: a code list's label). The nested rows
  follow that level's row, one indent step deeper (the stub's `indent`,
  default 4), in the parent's group; their own heading is dropped, their
  order and text stay `plan_levels()`' and `plan_labels()`'. The rows
  key that carries the variable
  (`table_plan(rows = c(group = "variable"))`) is what is read.

  For `plan_levels()` and `plan_labels()`, one entry per column or
  analysis variable: an order (`AGEGR1 = c("<65", "65-74")`), or the
  text values are printed as (`AGE = "Age (years)"`). Both merge one
  **key** at a time, so a later layer adds a variable without restating
  the rest — which is the whole reason these two are layers and the
  roles are not. A `plan_labels()` entry whose value is itself a named
  vector applies to that **column** only: a shift table's `"0"` is
  `"Grade 0"` down the side and `"Baseline 0"` across the top.

- .drop_empty:

  For `plan_levels()`: variables whose levels **no record has** are not
  shown – a level whose `n` is 0 (or missing) in every column, as an ARD
  made with a code list's full set of levels has (a factor's unused
  level, counted 0). `NULL` (default): every level the data has is
  shown. A level that some column counts stays; a variable summarised
  without `n` is untouched. Several calls add up.

- stats, value, na, notes:

  For `plan_cells()`: how a cell is made, as
  [`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)
  takes them — `stats = "cells"` (default) fills a template per cell and
  `"rows"` makes each statistic a row of its own; `value` is which of
  `stat` / `stat_fmt` a `{x}` reads; `na` what fills a cell no template
  could; `notes = FALSE` stops the report of the statistics no template
  used. They hold however the plan is run, by
  [`plan_apply()`](https://ichirio.github.io/rtfreporter/reference/plan_apply.md)
  or by `rtf_tables(doc, plan)`. On a plan of a table that is already
  built (no statistics), `na` alone is taken, and is
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)'s
  `na`: what a missing value prints as.

- rounding:

  For `plan_digits()`: the tie-breaking family for the run, as
  `widen_ard(rounding = )` takes it. Last wins, like every other layer.

- vars, name, indent, group_summary:

  For `plan_stub()`: the row keys to fold into one stub column and how,
  as
  [`stub_cols()`](https://ichirio.github.io/rtfreporter/reference/stub_cols.md)
  takes them. `name` is the NAME the folded column gets
  (`stub_cols(label = )`), which is a different thing from
  `table_plan(label = )` — the column whose VALUES are the row text.
  `vars` is derived when left out.

- before:

  For `plan_stub()`: `FALSE` (default) folds the stub inside
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md),
  after grouping and pagination have had their say. `TRUE` folds it
  first, with
  [`stub_cols()`](https://ichirio.github.io/rtfreporter/reference/stub_cols.md),
  which is what `plan_cell_style()` needs — only then can a condition
  see the rows that will be printed. The two do **not** always give the
  same table: folded first, the row keys are gone before a page split or
  a row group could read them, so `plan_paginate_group(col = )` naming
  one of them no longer finds it. That is why this is a choice and not
  worked out for you.

- cols:

  For `plan_cell_style()`: the columns styled, by name (`.values` for
  every value column); left out, every column.

- header:

  For `plan_cell_style()`: `TRUE` styles the column header
  ([`style_header()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)).
  For `plan_col_header()`: the header, built with the same
  [`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md)
  as everywhere else — or a **function** of the resolved `n` (and, with
  two arguments, the finished table) when it has to be computed — or a
  **data frame of cells**, one row a cell, with the `col_header` sheet's
  columns (`line`, `cols`, `span`, `text`, borders ...), placed on each
  page's columns when it is made; this is how
  `tflspec::tfl_table_code()` writes a workbook's header. The plan adds
  two things to a header it is given, both of which used to need a
  function:

  - `{n:sum}` is that number **totalled over the columns the cell
    covers**, so a spanner over one arm's two columns shows that arm's N
    and one over all of them shows the study total — neither written
    down. A cell outside the data (the stub) totals every column. A
    spanner's `{col1}`, `{col2}`, ... are the levels its columns
    **agree** on, which is the arm name a spanning cell wants;

  - `{n:<column>}` names **one** of the values, for a cell that has to
    say a number belonging to a column it does not sit over —
    `"A={n:Placebo} B={n:Xanomeline High Dose}"`;

  - **[`print()`](https://rdrr.io/r/base/print.html) lists every token
    this plan offers, with its VALUES**, under `header tokens` — the
    numbers a `{n}` holds and the text a `{col1}` prints as — because
    they come from the ARD and from the spread, and neither is visible
    in the call. The `{col...}` ones appear once the table has been
    built at least once;

  - its cells may carry `{col}` and `{n}` — the column and its
    denominator (or the single one there is). Several `cols` keys make a
    name like "Placebo\_\_\_\_Negative", which nobody wants printed, so
    `{col}` is the **leaf** and `{col1}`, `{col2}`, ... are the levels
    in order; with one key the leaf is the whole name. The hierarchy
    itself needs no header —
    [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
    builds the spanning rows from the same separator, merging the cells.
    What each level READS is `plan_labels()`'s business, since it
    recodes the values the name is made of;

  - a row **shorter** than the table has its last cell repeated over the
    spread columns, whose names are not known until the table exists.

      plan_col_header(values = list(n = TRUE), rtf_col_header(
        c("",               "{col}"),
        c("Characteristic", "(N={n})")))

  A row already the right length, and a cell with no token in it, are
  untouched — so a spanner, a border or a cell that reads the finished
  table is written exactly as it always was.

- where:

  For `plan_cell_style()`: a one-sided formula over the table's columns
  choosing the rows, e.g. `where = ~ is.na(label)`. For `plan_blanks()`:
  `as_rtftables(blank_rows = )`, or `"records"` for a blank row after
  each record of a listing.

- bold, italic, align, color, background:

  For `plan_cell_style()`: how the cells look. A **value**
  (`bold = TRUE`, `color = "#CC0000"`) applies to the cells `cols` /
  `header` / `where` choose. A **one-sided formula** computes the value
  row by row over the table's columns, `NA` leaving the column default
  alone — `bold = ~ is.na(label)`,
  `color = list(Placebo = ~ ifelse(n > 50, "#CC0000", NA))`, a named
  list scoping it to columns. Formula styles see the printed rows only
  with `plan_stub(before = TRUE)`. For `plan_style()`, `align`, `bold`
  and `italic` are the body's default look,
  [`rtf_table_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style.md)'s
  fields.

- border, align_count_pct, font, font_size_half_points,
  row_height_twips, row_height_exact, header_row_height_twips,
  blank_row_height_twips, cell_padding_left_twips,
  cell_padding_right_twips, cell_valign, table_align, markup,
  blank_row_normalize:

  For `plan_style()`: the settings of the **whole table**, by
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)'s
  and
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)'s
  names. What a table has per column or per cell is another verb's —
  `plan_columns()`, `plan_cell_style()`, `plan_col_header()`,
  `plan_blanks()` — so this list is the whole of it. For
  `plan_cell_style()`, `border` is a column's border
  ([`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)).

- underline:

  For `plan_style()`: the body's default, an
  [`rtf_table_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style.md)
  field like `align`, `bold` and `italic` (which are that too in
  `plan_style()`). For `plan_cell_style()`: the cells' underline, as
  `bold`.

- indent_twips:

  For `plan_cell_style()`: the left indent of the cells' text,
  [`style_cols()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)'s
  `indent_twips` (not on the header).

- col:

  For `plan_paginate_group()`: the column whose value starts a new page,
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)'s
  `group_col` with `split = "by_value"`. Left out, it is the outermost
  row key. The page is **named** after the value, which is the line
  `rtf_tables(auto_section = TRUE)` cuts a section on — so this verb
  decides what a section is. It is the only verb that makes a page per
  value.

- keep:

  `FALSE` also hides the column the verb names: the page key for
  `plan_paginate_group()`, the sort keys for `plan_sort()`. A column can
  be **needed and not wanted** — the key a page break reads, a sort
  carrier — and the verb that needs it is the one place that knows, so
  it says so there instead of the name being written again in a
  `plan_hide()`. Names that are not columns (a statistic, `".depth"`)
  are ignored rather than refused.

- mode, collapse:

  For `plan_row_group()`: what a run of rows sharing a value is, and how
  the repeat shows. `mode` is `as_rtftables(group_by = )`, which is how
  a group BOUNDARY is found — `"value"` (each run of equal values),
  `"indent`" (a row starts a group when its cell is not indented),
  `"filled"` (when its cell is not empty), or `"auto"`. `collapse` is
  `collapse_repeats`: a repeated value printed once and then blank.

  `mode = "indent"` **reads** indentation to find the boundary;
  `plan_stub(indent = )` **writes** it. They are not the same knob, and
  a stub written with `indent` is exactly what that mode then reads.

  The rows are grouped by the outermost row key (`table_plan(rows = )`);
  folded into a stub, by the stub's headings.

- group_col:

  For `plan_row_group()`, on a plan of a table that is already built:
  `as_rtftables(group_col = )`, the column whose runs are the groups
  (what `split = "group_safe"`, `mode` and the blank rows between groups
  read), with the pages still cut by rows. An ARD plan names it as its
  outermost row key (`table_plan(rows = )`) and refuses this; a page per
  value is `plan_paginate_group()`.

- label, position:

  For `plan_total()`: the heading of the **Total column** (`"Total"`)
  and where it goes among the column key's values (`"last"`, `"first"`).
  Its cells are cards' own overall rows — the statistics with no value
  of the column key, from the same analysis without its `by`
  (`cards::ard_tabulate(adsl, variables = RACE)` bound under
  `cards::ard_tabulate(adsl, by = ARM, variables = RACE)`, or
  `cards::ard_stack(.overall = TRUE)`) — so the ARD keeps no `ARM` value
  the data does not have. Its `{n}` in the column header is the study
  total the ARD states (the column key tabulated on its own, or
  [`cards::ard_total_n()`](https://pharmaverse.github.io/cards/latest-tag/reference/ard_total_n.html)).
  One column key only.

- stat:

  For `plan_sort()`: the statistic totalled into `.sort_stat` for a
  frequency order (`widen_ard(sort_stat = )`), e.g. `stat = "n"`.

- first, last, counted:

  For `plan_blanks()`:
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)'s
  `blank_row_first`, `blank_row_end` and `count_blank_rows`.

- max_rows, split, break_before, min_group_rows, cont_label:

  For `plan_paginate_rows()`: the row budget and what a page break may
  cut —
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)'s
  `max_rows`, `split`, `split_rows`, `min_group_rows` and `cont_label`.
  This is the **row** axis; a page per value (the **group** axis) is
  `plan_paginate_group()`, and the **column** axis is
  `plan_paginate_cols()`.

- page_by:

  For `plan_paginate_rows()`:
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)'s
  `page_by` – the column(s) whose value partitions the body **first**,
  each value a page named after it; the row settings then apply
  **within** one partition (a period, a cohort), so a BY page can still
  be cut by a row budget. `plan_paginate_group()` is the other way to
  page by a value: one page per value however long it is, with no row
  budget. The BY column is printed unless hidden (`plan_hide()`).

- border_header, border_spanning, border_body, border_first_row,
  border_last_row:

  For `plan_style()`: the rules of one kind of row, by the names
  [`rtf_table_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style.md)
  gives them. Say the table's rules one way: `border = "tfl"`, or these.

- header_align, header_bold, header_italic:

  For `plan_style()`: the whole header's default look,
  [`rtf_table_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style.md)'s
  fields; with the `border_*` zones and `align` ... `underline` they
  make the `as_rtftables(style = )` object. One column or one cell is
  `plan_cell_style()`.

- table_width_twips, table_width_pct, table_width_pct_of_writable:

  For `plan_style()`: the table's width, by
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)'s
  names (and
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)'s
  `table_width_twips`).

- values:

  For `plan_col_header()`: what the header's `{tokens}` take. The
  **population** each column describes — its analysis set, the number a
  header prints as `(N=86)` — is `values = list(n = TRUE)` (or just
  `TRUE`), read from the data, keyed by the same `cols` / `levels`
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md)
  was given, **at every depth of the keys**: with
  `cols = c("TRT", "SEX")` both the arm (`"Placebo"`) and the arm x sex
  cell (`"Placebo____F"`) are looked up. Only a number the ARD **states
  as a population size** is read:

  1.  a **cards sentinel**'s `N` keyed by exactly those keys —
      `..ard_hierarchical_overall..` from
      `cards::ard_stack_hierarchical(over_variables = TRUE)`, each arm's
      denominator; never its `n`, the subjects with an event that the
      "Any" row shows. Taken only when there is one kind of sentinel;

  2.  the column variable's **own tabulation** — the per-arm `n` of
      `cards::ard_stack(.by = )`, which
      [`normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.md)
      keeps as `.key_own` rows — where its counts add up to the `N`
      those rows state (the population split by arm, not the treatment
      counted as an event). At depth *k* it is `cols[k]` tabulated
      within `cols[1..k-1]`: cards tabulates each `.by` variable on its
      own, so `ard_stack(.by = c(TRT, SEX))` states the arm but not the
      arm x sex cell — bind
      `cards::ard_tabulate(adsl, by = TRT, variables = SEX)` to state
      that;

  3.  an analysis summary's `N` only where it is a denominator by
      construction (a hierarchical summary, a percentage of the
      `denominator =` data) or where **two or more different variables
      agree on it for every column**. One variable's `N` is the count of
      its **non-missing** values — the arm size only if nothing is
      missing, which the ARD cannot show — and an `N` per visit or per
      parameter is not a column's at all.
      [`pull_ard()`](https://ichirio.github.io/rtfreporter/reference/pull_ard.md)
      is not asked.

  The study total (`..ard_total_n..`, the one
  [`normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.md)
  remembers as it drops that row, or the `N` the column variable's own
  tabulation states) is **never put in every column**: it answers a
  one-column table and a cell over all the columns.

  **Pages split by a group value** (`plan_paginate_group()`: a lab
  parameter, a visit) have **two populations**, and which one the header
  says is the author's choice:

  - `values = list(n = "page")` — each page's own, the subjects with
    that test: the ARD rows **carrying** the page key, e.g.
    `cards::ard_tabulate(adlb, by = PARAM, variables = BGRADE)`, which
    states each baseline column's N and the page's total;

  - `list(n = "table")` — the analysis set: the ARD rows **without** the
    page key, e.g. `cards::ard_total_n(adsl)` or the treatment tabulated
    from ADSL;

  - `list(n = "page", N = "table")` — both, as `{n}` and `{N}`.

  `n = TRUE` reads the page's rows, then the table's for what they lack,
  and **warns** when the ARD states both and they differ. Neither is
  filled in from outside the ARD: a population it does not state is
  `NA`. Keep the header consistent with the body — the percentages are
  over the denominator cards used, so a header saying the analysis set
  wants an ARD built with that `denominator =`. The workbook says the
  same on `tables$header_n`.

  What cannot be read is printed as **`NA`**, and one **warning** lists
  every such cell with the reason (`only AGE states an N (79)`,
  `the analysis variables state different N`, ...). The table is still
  built; a guessed number would look exactly like a right one.
  [`print()`](https://rdrr.io/r/base/print.html) shows the resolved
  values and what is not resolved.

  In a header cell, `{n}` is the number of **what the cell stands for**:
  its column; over a spanner, the level its columns agree on (the arm
  over its F and M); over all columns, the total. `{n1}`, `{n2}`, ...
  name a depth from any cell — `"{col2} {n2}"` under `"{col1} {n1}"`.

  To **give the numbers yourself**, `n` is a vector named by column key,
  at any depth — `c(Placebo = 86, "Placebo____F" = 53, ...)` — or a
  function of the data returning one; a single unnamed number fills
  every cell. After a workbook (`tflspec::tfl_table_plan()`), a later
  `plan_col_header(values = ...)` supplies the numbers and keeps the
  workbook's header.

  A **data frame** of per-page values is
  [`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md)'s
  own `values =`: one row per page key, a column per `{token}`. \#' A
  **function** of the data covers what neither can find, and a **named
  list** of either supplies several — and then **each name is a token**,
  which is how one header says two numbers with no function at all: the
  study total in a spanner and each column's own underneath it.

      plan_col_header(
        values = list(n = TRUE, total = 254),
        rtf_col_header(
          list(col_cell(1, ""), col_cell(c(2, 4), "All (N={total})")),
          c("",               "{col}"),
          c("Characteristic", "(N={n})")))

  An entry keyed by column fills each column with its own; a single
  number fills every cell. `{n}` is the entry called `n`, or the only
  entry when there is one. The resolved value is also what `header =` is
  called with when it is a function.

- header_sep, col_header_align:

  For `plan_col_header()`:
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)'s
  `header_sep` (the separator a plain table's column names are split on
  into spanning header rows) and
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)'s
  `col_header_align`.

- lines:

  For `plan_col_header()`: the header **a row at a time**, as it reads,
  instead of `header`: a list, one element a header row (the top first),
  each a named character vector – a cell's name the columns it sits on
  (a column name, `.values` for every value column, a position or range
  such as `3:5`, or `KEY = value`), its value the text, with the same
  tokens (`{col}`, `{n}`, `{n:sum}` ...):

      plan_col_header(lines = list(
        c(row_label = "",               .values = "{col}"),
        c(row_label = "Characteristic", .values = "(N={n})")))

  It is the data frame of cells written another way (one row a cell:
  `line`, `cols`, `text`, `span`), so it does all that does.

- span:

  For `plan_col_header(lines = )`: how a cell over several columns is
  made – `"each"` (the default: a cell a column), `"one"` (one cell over
  them all) or a key's name (a cell per value of that key: a spanner).
  One value for every cell, or a list as `lines` is, each element the
  spans of that row's cells, named as its cells are (a cell not named
  there is `"each"`).

- widths:

  For `plan_columns()`: the relative column widths,
  `rtftable(col_rel_width = )`. **Named by column**
  (`c(row_label = 5, .values = 2)`, `.values` for every value column) a
  reordered table keeps them, and that is what the `columns` sheet's
  `width` is; unnamed, they are one a column in order, as
  `col_rel_width` itself.

- decimal:

  For `plan_columns()`: the columns whose numbers line up at the decimal
  point (`.values` for every value column) — the `columns` sheet's
  `decimal_split`.

- row_title, auto_width:

  For `plan_columns()`: the row-heading columns
  (`rtftable(row_title = )`) and whether each column is sized to its
  content (`as_rtftables(auto_width = )`).

- sep:

  For `plan_columns()`: the separator several `cols` keys are joined
  with in the value columns' names, `"____"` by default —
  `"Placebo____F"`. The spanning header is built by splitting on it, so
  a key value that contains it is refused. For `plan_listing()`,
  [`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md)'s
  `sep`.

- cell_format, column_widths_twips:

  For `plan_columns()`:
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)'s
  `cell_format` (a formatter, or a list of them one a column) and
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)'s
  `column_widths_twips`, as they are.

- type, spacer, spacer_rel_width, layout, wrap:

  For `plan_listing()`:
  [`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md)'s
  own arguments, unchanged. `...` there takes the
  [`listing_col()`](https://ichirio.github.io/rtfreporter/reference/listing_col.md)s.
  A blank row between records is `plan_blanks(where = "records")`, one
  at the top of each page `plan_blanks(first = TRUE)`, and a column's
  alignment is its own (`listing_col(align = )`).

- pages:

  For `plan_titles()` / `plan_footnotes()`: a list with one block per
  page, when the pages do not share a block. `...` is the rows of a
  single block used on every page; give one or the other, never both,
  because a three-row title on a three-page table cannot be told apart
  from three one-row titles.

- at, cut_by, col_header, fit, allow_span_break, order:

  For `plan_paginate_cols()`, in
  [`paginate_cols()`](https://ichirio.github.io/rtfreporter/reference/paginate_cols.md)'s
  terms: `at` the columns to cut before; `cut_by` a list of column
  blocks (`paginate_cols(cols = )`), or a separator found in the column
  names / one key per column (`paginate_cols(by = )`); `col_header` what
  the header becomes; `fit` `TRUE` (every block's widths on page 1's
  scale, `width = "fill"`) or `FALSE` (each column keeps its width,
  `"keep"`); `order` `paginate_cols(page_order = )`, the order the three
  axes nest in, outermost first: `"group"`, `"rows"`, `"cols"`, or the
  shorthands `"across"` and `"down"`. For `plan_paginate_cols()` `keep`
  is the columns every block repeats (`paginate_cols(carry = )`).

- every:

  For `plan_paginate_cols()`: cut a block every this many columns,
  counting only the ones a block does not keep. This is `at` without
  writing down how many columns one study had — the plan is deferred, so
  it counts them when the table exists. Give one of `at`, `cut_by` or
  `every`.

## Value

The plan, with one more layer.

## Where each argument goes

Each verb has **one job**, and its arguments keep the names of the
function that does it. This is what each one hands on.

- `plan_cells(..., stats, value, na, notes)`: how a cell is made. Goes
  to
  [`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md):
  `cells`, `stats`, `value`, `na`, `notes`; a finished table
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md):
  `na`.

- `plan_digits(..., rounding)`: the digits. Goes to the open tokens of
  the templates; on a finished table
  [`fmt_numeric()`](https://ichirio.github.io/rtfreporter/reference/fmt_numeric.md).

- `plan_levels()`, `plan_labels()`: the order and text of values. Goes
  to
  [`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md):
  `levels`, `labels`.

- `plan_total(label, position)`: a Total column from the ARD's overall
  rows. Goes to
  [`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md):
  those rows as one more value of the column key, and its place in
  `levels`; the header's `{n}` there is the study total.
  `plan_levels(.drop_empty = )` leaves out the levels no record has,
  before the table is made.

- `plan_sort(..., stat, keep)`: the row order. Goes to
  [`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md):
  `sort`, `sort_stat`; a finished table
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md):
  `sort_by`, `sort_desc` from `-name`.

- `plan_stub(vars, name, indent, group_summary, before)`: the row
  headings. Goes to
  [`stub_cols()`](https://ichirio.github.io/rtfreporter/reference/stub_cols.md):
  `vars`, `label`, `indent`, `group_summary`.

- `plan_cell_style(cols, header, where, bold, italic, align, color, background, border, underline, indent_twips)`:
  how cells look. Goes to
  [`style_header()`](https://ichirio.github.io/rtfreporter/reference/style_header.md),
  [`style_cols()`](https://ichirio.github.io/rtfreporter/reference/style_header.md),
  or
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)'s
  `cell_styles` for a condition.

- `plan_paginate_group(col, keep)`: a page per value. Goes to
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md):
  `split = "by_value"`, `group_col`; `keep = FALSE` adds it to
  `drop_cols`.

- `plan_row_group(mode, collapse, group_col)`: groups down the body.
  Goes to
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md):
  `group_by`, `collapse_repeats`, `group_col` (a finished table).

- `plan_hide(...)`: columns not printed. Goes to
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md):
  `drop_cols`.

- `plan_blanks(where, first, last, counted)`: blank rows. Goes to
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md):
  `blank_rows`, `blank_row_first`, `blank_row_end`, `count_blank_rows`;
  a listing's `where = "records"` is
  [`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md)'s
  `blank_row`.

- `plan_paginate_rows(max_rows, split, break_before, min_group_rows, cont_label, page_by)`:
  the row budget, inside the BY pages. Goes to
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md):
  `max_rows`, `split`, `split_rows`, `min_group_rows`, `cont_label`,
  `page_by`.

- `plan_paginate_cols(at, cut_by, every, keep, col_header, fit, allow_span_break, order)`:
  column blocks. Goes to
  [`paginate_cols()`](https://ichirio.github.io/rtfreporter/reference/paginate_cols.md):
  `at`, `cols` / `by`, `carry`, `col_header`, `width`,
  `allow_span_break`, `page_order`.

- `plan_style(border, ..., border_header, ..., header_bold, ..., table_width_twips, ...)`:
  the whole table. Goes to
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  /
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
  by the same names; `border_*` and the default look (`header_align`,
  `header_bold`, `header_italic`, `align`, `bold`, `italic`,
  `underline`) via
  [`rtf_table_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style.md).

- `plan_columns(widths, decimal, row_title, auto_width, sep, cell_format, column_widths_twips)`:
  the columns. Goes to
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md):
  `col_rel_width`, `row_title`, `column_widths_twips`;
  [`set_decimal_split()`](https://ichirio.github.io/rtfreporter/reference/set_decimal_split.md):
  `cols`;
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md):
  `auto_width`, `cell_format`;
  [`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md):
  `sep`.

- `plan_col_header(header, values, header_sep, col_header_align, lines, span)`:
  the column header. Goes to
  [`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md):
  the header (or `lines`, the header a row at a time) and a data frame
  of `values`; a population fills its `{n}` tokens;
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md):
  `header_sep`;
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md):
  `col_header_align`.

- `plan_listing(..., type, sep, spacer, spacer_rel_width, layout, wrap)`:
  a listing. Goes to
  [`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md),
  the same names.

- `plan_titles()`, `plan_footnotes()`: the blocks above and below. Goes
  to
  [`rtf_titles()`](https://ichirio.github.io/rtfreporter/reference/rtf_titles.md),
  [`rtf_footnotes()`](https://ichirio.github.io/rtfreporter/reference/rtf_footnotes.md).

- `plan_after(...)`: anything else. Goes to your functions of the pages.

`plan_columns()` declares the columns **of this table** — widths,
alignment, the key separator — and is not
[`rtf_columns()`](https://ichirio.github.io/rtfreporter/reference/rtf_columns.md),
which addresses the columns of finished pages by their printed names.

## plan_after() is the way out, not the way in

`plan_after()` runs functions of your own on the finished pages. It is
for what the plan cannot **declare** — a step with no verb — and it is
the only verb whose content the plan cannot read: a workbook
(`tflspec::tfl_as_table_spec()`) cannot carry it, and the columns it
names are positions on the pages, which a reordered table does not keep.
The usual reasons to reach for it each have a declaration, which names
columns and goes into a workbook:

- `set_decimal_split(x, cols = 3:31)` is declared as
  `plan_columns(decimal = ".values")`

- `paginate_cols(x, ...)` is declared as
  `plan_paginate_cols(every = , at = , keep = )`

- `rtftable(col_rel_width = )` by position is declared as
  `plan_columns(widths = c(Analyte = 3, .values = 2))`

- [`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md)
  /
  [`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md)
  is declared as `plan_col_header()`

- [`realign_count_pct()`](https://ichirio.github.io/rtfreporter/reference/realign_count_pct.md)
  is declared as `plan_style(align_count_pct = TRUE)`

- [`fmt_numeric()`](https://ichirio.github.io/rtfreporter/reference/fmt_numeric.md)
  is declared as `plan_digits(<column> = 2)`,
  `plan_digits(.rows = c(Mean = 1))`

- [`paginate()`](https://ichirio.github.io/rtfreporter/reference/paginate.md)
  is declared as `plan_paginate_rows()`

- `style_header(x, ...)` is declared as
  `plan_cell_style(header = TRUE, ...)`

- `style_cols(x, ...)` is declared as `plan_cell_style(cols = , ...)`

- bold / colour / alignment of body cells, by condition is declared as
  `plan_cell_style(where = ~ ..., ...)`

- `style_zone(x, ...)` is declared as
  `plan_style(border_header = , border_body = , ...)`

The styles `plan_cell_style()` declares run on the pages in the order
written (after the header and the decimal alignment, before any
`plan_after()` step). Its `cols` may be column names, and `.values`
stands for every value column, so a reordered table keeps them.

## See also

[`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md),
[`plan_apply()`](https://ichirio.github.io/rtfreporter/reference/plan_apply.md)

## Examples

``` r
if (requireNamespace("cards", quietly = TRUE)) {
  # One ARD, normalized once; each block below adds verbs to `base`.
  adsl <- cards::ADSL
  adsl$ARM <- factor(adsl$ARM)
  ard <- cards::ard_stack(
    adsl, .by = ARM,
    cards::ard_summary(variables = AGE),
    cards::ard_tabulate(variables = c(SEX, AGEGR1)),
    .overall = TRUE)
  base <- table_plan(normalize_ard(ard), cols = "ARM",
                     rows = c(group = "variable"))

  # -- The cells: templates, digits, the order and text of values --------
  p <- base |>
    plan_cells(continuous  = c("Mean (SD)" = "{mean} ({sd})",
                               "Median"    = "{median}"),
               categorical = "{n} ({p:%})", notes = FALSE) |>
    plan_digits(1) |>                                   # the house rule
    plan_digits(AGE = c(mean = 1, sd = 2)) |>           # one variable
    plan_levels(SEX = c("M", "F")) |>
    plan_labels(c(AGE = "Age (years)", SEX = "Sex", AGEGR1 = "Age group"))
  plan_apply(p, "table")

  # -- The rows: stub, groups, blank rows, order, hidden columns ---------
  p <- p |>
    plan_stub(name = "Characteristic", before = TRUE) |>
    plan_row_group(mode = "indent") |>
    plan_blanks(where = "between_groups", last = TRUE) |>
    plan_total(label = "Total")
  plan_apply(p, "table")

  # -- The columns, the header, the look ---------------------------------
  p <- p |>
    plan_columns(widths = c(Characteristic = 3, .values = 2)) |>
    plan_col_header(values = list(n = TRUE),
                    rtf_col_header(c("", "{col}"),
                                   c("Characteristic", "(N={n})"))) |>
    plan_style(border = "tfl", align_count_pct = TRUE) |>
    plan_cell_style(cols = "Characteristic", italic = ~ grepl("^ ", Characteristic))

  # -- Pages: row budget, column blocks, titles and footnotes ------------
  p <- p |>
    plan_paginate_rows(max_rows = 20) |>
    plan_titles(c("Table 14.1.1", "Demographics")) |>
    plan_footnotes("Percentages are of the subjects in each arm.") |>
    plan_after(function(pages) pages)                   # your own step
  length(plan_apply(p))                                 # the pages
  plan_layers(p)

  # -- A page per value, a column split, nesting, sort, hide -------------
  ard_bm <- cards::ard_summary(adsl, by = c(SEX, ARM), variables = c(AGE, BMIBL))
  q <- table_plan(normalize_ard(ard_bm), cols = "ARM",
                  rows = c(SEX = "SEX", group = "variable")) |>
    plan_cells(continuous = c("n" = "{N}", "Mean (SD)" = "{mean} ({sd})"),
               notes = FALSE) |>
    plan_digits(1) |>
    plan_stub(name = "Parameter") |>
    plan_paginate_group(col = "SEX") |>                 # a page per sex
    plan_paginate_cols(every = 2, keep = "Parameter")   # two arms a page
  length(plan_apply(q))

  # -- Rows under one level of another variable; frequency order --------
  ard_r <- cards::ard_tabulate(adsl, by = ARM, variables = c(RACE, ETHNIC))
  table_plan(normalize_ard(ard_r), cols = "ARM",
             rows = c(group = "variable")) |>
    plan_cells(categorical = "{n}", notes = FALSE) |>
    plan_nest(ETHNIC = c(RACE = "WHITE")) |>
    plan_sort("-n") |>
    plan_apply("table")
}
#>   group                            label Placebo Xanomeline High Dose
#> 1  RACE                            WHITE      78                   74
#> 2  RACE           NOT HISPANIC OR LATINO      83                   81
#> 3  RACE               HISPANIC OR LATINO       3                    3
#> 4  RACE        BLACK OR AFRICAN AMERICAN       8                    9
#> 5  RACE AMERICAN INDIAN OR ALASKA NATIVE       0                    1
#>   Xanomeline Low Dose
#> 1                  78
#> 2                  78
#> 3                   6
#> 4                   6
#> 5                   0

# The display verbs also lay out a finished data frame -- no ARD needed.
tbl <- data.frame(
  Parameter = c("Subjects", "Age, mean (SD)", "Female, n (%)"),
  Placebo   = c("86", "75.2 (8.59)", "53 (61.6)"),
  Active    = c("84", "75.7 (8.29)", "50 (59.5)"))
pages <- table_plan(tbl) |>
  plan_columns(widths = c(Parameter = 3, .values = 2)) |>
  plan_hide("Active") |>
  plan_style(border = "tfl") |>
  plan_titles("Table 1  Summary") |>
  plan_apply()
pages[[1]]
#>      Table 1  Summary      
#> ───────────────────────────
#> Parameter         Placebo  
#> ───────────────────────────
#> Subjects            86     
#> Age, mean (SD)  75.2 (8.59)
#> Female, n (%)    53 (61.6) 
#> 
#> <rtftable> 3 rows x 2 columns
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     relative 3:2
#>   Titles:     1 line(s)
#> 

# A listing of records.
recs <- data.frame(USUBJID = c("01-001", "01-001", "01-002"),
                   AETERM  = c("Headache", "Nausea", "Fatigue"),
                   AESEV   = c("MILD", "MODERATE", "MILD"))
listing <- table_plan(recs) |>
  plan_sort("USUBJID", "AETERM") |>
  plan_listing(listing_col("USUBJID", label = "Subject"),
               listing_col("AETERM", label = "Adverse event"),
               listing_col("AESEV", label = "Severity")) |>
  plan_apply()
length(listing)
#> [1] 1
```

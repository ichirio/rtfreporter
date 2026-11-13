# Package index

## Tables from an ARD — reading the ARD

The recommended start: an analysis results dataset (cards / cardx) made
ready for a table. **Acts on:** an ARD data frame.
[`normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.md)
flattens it – groups and hierarchy become ordinary key columns, and
`stack =` puts subgroups from different variables one under another –
and
[`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)
does the whole ARD half in one call, giving a table data frame now.
[`list_ard_keys()`](https://ichirio.github.io/rtfreporter/reference/list_ard_keys.md),
[`pull_ard()`](https://ichirio.github.io/rtfreporter/reference/pull_ard.md),
[`overall_row()`](https://ichirio.github.io/rtfreporter/reference/overall_row.md)
and
[`cell_rows()`](https://ichirio.github.io/rtfreporter/reference/cell_rows.md)
look inside an ARD and shape its rows. Start with the *Get started*
article.

- [`ard-tables`](https://ichirio.github.io/rtfreporter/reference/ard-tables.md)
  : Tables from a cards/cardx ARD
- [`normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.md)
  : Flatten an ARD into an explicitly keyed long table
- [`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)
  : Turn a normalized ARD into a wide table data.frame
- [`list_ard_keys()`](https://ichirio.github.io/rtfreporter/reference/list_ard_keys.md)
  : What is actually inside an ARD
- [`pull_ard()`](https://ichirio.github.io/rtfreporter/reference/pull_ard.md)
  : Pull a statistic out of an ARD, keyed like the spread columns
- [`overall_row()`](https://ichirio.github.io/rtfreporter/reference/overall_row.md)
  : Where the table's overall row comes from
- [`cell_rows()`](https://ichirio.github.io/rtfreporter/reference/cell_rows.md)
  : Several rows in one cell recipe, each with its own fallback chain

## Tables from an ARD — the plan

[`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md)
names the roles (what goes across, what goes down; `show_keys =` also
shows a subgroup key’s own counts as rows) and each `plan_*()` verb adds
one declaration. Nothing runs until the plan is handed to
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
or
[`plan_apply()`](https://ichirio.github.io/rtfreporter/reference/plan_apply.md);
a later layer wins. The overview of every verb is `plan_verbs`, and each
has a page of its own; by job:

- **cells**:
  [`plan_cells()`](https://ichirio.github.io/rtfreporter/reference/plan_cells.md),
  [`plan_digits()`](https://ichirio.github.io/rtfreporter/reference/plan_digits.md),
  [`plan_stat_groups()`](https://ichirio.github.io/rtfreporter/reference/plan_stat_groups.md)
- **rows**:
  [`plan_filter()`](https://ichirio.github.io/rtfreporter/reference/plan_filter.md),
  [`plan_sort()`](https://ichirio.github.io/rtfreporter/reference/plan_sort.md)
  (`-n@SOC`, `-n[Total]`),
  [`plan_levels()`](https://ichirio.github.io/rtfreporter/reference/plan_levels.md),
  [`plan_labels()`](https://ichirio.github.io/rtfreporter/reference/plan_labels.md),
  [`plan_nest()`](https://ichirio.github.io/rtfreporter/reference/plan_nest.md)
- **more columns**:
  [`plan_total()`](https://ichirio.github.io/rtfreporter/reference/plan_total.md),
  [`plan_compare()`](https://ichirio.github.io/rtfreporter/reference/plan_compare.md)
- **layout**:
  [`plan_stub()`](https://ichirio.github.io/rtfreporter/reference/plan_stub.md),
  [`plan_row_group()`](https://ichirio.github.io/rtfreporter/reference/plan_row_group.md),
  [`plan_blanks()`](https://ichirio.github.io/rtfreporter/reference/plan_blanks.md),
  [`plan_hide()`](https://ichirio.github.io/rtfreporter/reference/plan_hide.md),
  [`plan_columns()`](https://ichirio.github.io/rtfreporter/reference/plan_columns.md),
  [`plan_cell_style()`](https://ichirio.github.io/rtfreporter/reference/plan_cell_style.md),
  [`plan_style()`](https://ichirio.github.io/rtfreporter/reference/plan_style.md)
- **pages**:
  [`plan_paginate_group()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_group.md),
  [`plan_paginate_rows()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_rows.md),
  [`plan_paginate_cols()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_cols.md)
- **header and text**:
  [`plan_col_header()`](https://ichirio.github.io/rtfreporter/reference/plan_col_header.md),
  [`plan_titles()`](https://ichirio.github.io/rtfreporter/reference/plan_titles.md),
  [`plan_footnotes()`](https://ichirio.github.io/rtfreporter/reference/plan_footnotes.md)
- **the rest**:
  [`plan_listing()`](https://ichirio.github.io/rtfreporter/reference/plan_listing.md),
  [`plan_append()`](https://ichirio.github.io/rtfreporter/reference/plan_append.md),
  [`plan_after()`](https://ichirio.github.io/rtfreporter/reference/plan_after.md)

The *plan verbs* article has an at-a-glance table. The Excel definition
of a table lives in tflspec (`tflspec::tfl_table_plan()`).

- [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md)
  : A deferred, last-wins plan for a table
- [`plan_verbs`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
  : Declare the table, one layer at a time
- [`plan_cells()`](https://ichirio.github.io/rtfreporter/reference/plan_cells.md)
  : Say how each cell is made from the statistics
- [`plan_digits()`](https://ichirio.github.io/rtfreporter/reference/plan_digits.md)
  : Set the digits of the numbers in the cells
- [`plan_filter()`](https://ichirio.github.io/rtfreporter/reference/plan_filter.md)
  : Drop rows by a condition over their statistics
- [`plan_sort()`](https://ichirio.github.io/rtfreporter/reference/plan_sort.md)
  : Put the rows in order
- [`plan_levels()`](https://ichirio.github.io/rtfreporter/reference/plan_levels.md)
  : Order the values of a key, and the analysis variables
- [`plan_labels()`](https://ichirio.github.io/rtfreporter/reference/plan_labels.md)
  : Give values and variables the text they print as
- [`plan_total()`](https://ichirio.github.io/rtfreporter/reference/plan_total.md)
  : Add a Total column (or row) from the overall statistics
- [`plan_stub()`](https://ichirio.github.io/rtfreporter/reference/plan_stub.md)
  : Fold the row keys into one row-heading column
- [`plan_col_header()`](https://ichirio.github.io/rtfreporter/reference/plan_col_header.md)
  : Declare the column header, and the numbers it prints
- [`plan_append()`](https://ichirio.github.io/rtfreporter/reference/plan_append.md)
  : Lay another plan's rows under a table's
- [`plan_stat_groups()`](https://ichirio.github.io/rtfreporter/reference/plan_stat_groups.md)
  : Split one record's statistics into named groups
- [`plan_nest()`](https://ichirio.github.io/rtfreporter/reference/plan_nest.md)
  : Put one variable's rows under one level of another
- [`plan_empty()`](https://ichirio.github.io/rtfreporter/reference/plan_empty.md)
  : Say what a table with no events shows
- [`plan_compare()`](https://ichirio.github.io/rtfreporter/reference/plan_compare.md)
  : Add columns read from comparison records
- [`plan_row_group()`](https://ichirio.github.io/rtfreporter/reference/plan_row_group.md)
  : Say how a run of rows sharing a value shows
- [`plan_blanks()`](https://ichirio.github.io/rtfreporter/reference/plan_blanks.md)
  : Say where the blank rows go
- [`plan_hide()`](https://ichirio.github.io/rtfreporter/reference/plan_hide.md)
  : Keep columns out of the printed table
- [`plan_cell_style()`](https://ichirio.github.io/rtfreporter/reference/plan_cell_style.md)
  : Style cells by column, in the header, or by a condition
- [`plan_paginate_group()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_group.md)
  : Start a new page for each value of a column
- [`plan_paginate_rows()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_rows.md)
  : Cut the pages by a row budget
- [`plan_paginate_cols()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_cols.md)
  : Cut a wide table into column blocks
- [`plan_columns()`](https://ichirio.github.io/rtfreporter/reference/plan_columns.md)
  : Declare the columns: widths, decimal alignment and row titles
- [`plan_style()`](https://ichirio.github.io/rtfreporter/reference/plan_style.md)
  : Set the look of the whole table
- [`plan_titles()`](https://ichirio.github.io/rtfreporter/reference/plan_titles.md)
  : Put a block of titles above the table
- [`plan_footnotes()`](https://ichirio.github.io/rtfreporter/reference/plan_footnotes.md)
  : Put a block of footnotes below the table
- [`plan_listing()`](https://ichirio.github.io/rtfreporter/reference/plan_listing.md)
  : Lay a frame of records out as a listing
- [`plan_after()`](https://ichirio.github.io/rtfreporter/reference/plan_after.md)
  : Run your own functions on the finished pages
- [`plan_apply()`](https://ichirio.github.io/rtfreporter/reference/plan_apply.md)
  : Run a plan, or look inside it
- [`plan_template()`](https://ichirio.github.io/rtfreporter/reference/plan_template.md)
  : Write the plan for you

### Looking inside a plan

- [`plan_layers()`](https://ichirio.github.io/rtfreporter/reference/plan_layers.md)
  : What a plan declares, and what it resolved to

- [`plan_header_tokens()`](https://ichirio.github.io/rtfreporter/reference/plan_header_tokens.md)
  : The tokens a plan's column header may carry, with their values

- [`plan_n_candidates()`](https://ichirio.github.io/rtfreporter/reference/plan_n_candidates.md)
  :

  The populations a column header's `{n}` can say

## Bring your own table — as_rtftables()

The second path: a table already built elsewhere – a
[`gt::gt()`](https://gt.rstudio.com/reference/gt.html) table, a
gtsummary table, an rtables/tern `VTableTree`, a tfrmt, flextable or
huxtable table, an rlistings `listing_df`, or a plain data.frame –
turned into `rtftable` objects, reading the source’s metadata and
paginating in one call. **Acts on:** the source object. **Produces:**
`rtftable` objects.
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
returns one per page (a list);
[`as_rtftable()`](https://ichirio.github.io/rtfreporter/reference/as_rtftable.md)
returns a single object.
[`stub_cols()`](https://ichirio.github.io/rtfreporter/reference/stub_cols.md)
prepares a tidy data.frame beforehand by merging hierarchy columns into
one indented stub column;
[`stub_spec()`](https://ichirio.github.io/rtfreporter/reference/stub_spec.md)
carries the same settings into `as_rtftables(stub = )`.

- [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
  : Convert a table object into rtfreporter table pages

- [`as_rtftable()`](https://ichirio.github.io/rtfreporter/reference/as_rtftable.md)
  : Convert one table object to a single rtftable

- [`combine_sections()`](https://ichirio.github.io/rtfreporter/reference/combine_sections.md)
  : Combine table page lists into one auto-sectioned list

- [`stub_cols()`](https://ichirio.github.io/rtfreporter/reference/stub_cols.md)
  : Merge hierarchy columns into one indented stub column

- [`stub_spec()`](https://ichirio.github.io/rtfreporter/reference/stub_spec.md)
  :

  Bundle the row-stub settings for
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)

## Document, rendering & tokens

[`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md)
creates the document – with the program, the `program_fallback` and
tokens of your own (`tokens =`) for its headers and footers; the section
and content calls below add to it;
[`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md)
writes the file. **Acts on:** the `rtf_document` object.
[`rtf_page()`](https://ichirio.github.io/rtfreporter/reference/rtf_page.md)
and
[`rtf_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_style.md)
are the only settings that are document-only – everything else in this
reference acts on a table
([`rtf_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_style.md)
gathers every module’s look in one object).
[`rtf_config()`](https://ichirio.github.io/rtfreporter/reference/rtf_config.md)
edits an already-composed document;
[`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md)
starts a new one.
[`rtf_text_tokens()`](https://ichirio.github.io/rtfreporter/reference/rtf_text_tokens.md)
lists every `{TOKEN}` a header, footer, title or footnote can carry
(`{PAGE}`, `{PROGRAM}`, `{PROGRAM_FULL}`, `{DATETIME}`, your own).

- [`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md)
  : Create an RTF document for pipe composition
- [`rtf_config()`](https://ichirio.github.io/rtfreporter/reference/rtf_config.md)
  : Configure document-level settings
- [`rtf_page()`](https://ichirio.github.io/rtfreporter/reference/rtf_page.md)
  : Page geometry for an RTF document
- [`rtf_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_style.md)
  : One style object for every module of a document
- [`rtf_watermark()`](https://ichirio.github.io/rtfreporter/reference/rtf_watermark.md)
  : Build a page watermark
- [`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md)
  : Generate an RTF file from a report object
- [`rtf_text_tokens()`](https://ichirio.github.io/rtfreporter/reference/rtf_text_tokens.md)
  : The tokens a page's text may carry
- [`print(`*`<rtf_document>`*`)`](https://ichirio.github.io/rtfreporter/reference/print.rtf_document.md)
  : Print an rtf_document object
- [`print(`*`<rtfreport>`*`)`](https://ichirio.github.io/rtfreporter/reference/print.rtfreport.md)
  : Print an rtfreport object

## Sections — headers & footers

A section applies a running header / footer to a range of pages. **Acts
on:** the `rtf_document` object. Headers and footers are themselves
small tables: build their rows with
[`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
/
[`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
(a list of rows – to change one, make it again).

- [`rtf_section()`](https://ichirio.github.io/rtfreporter/reference/rtf_section.md)
  : Define sections for pages
- [`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
  [`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
  : Create a header or footer object for a section

## Page content — tables, titles & footnotes

Place a table on the page, with its titles and footnotes. **Acts on:**
the `rtf_document` object –
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
takes the `rtftable` objects the sections below produce. **Note the two
levels:**
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
sets how a table looks; passing the same setting to
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
overrides it *for that placement only*, leaving the table object
untouched.
[`rtf_layout()`](https://ichirio.github.io/rtfreporter/reference/rtf_layout.md)
shows how the contents fall on the printed pages (`page_break = FALSE`
puts one under the content before it).

- [`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
  : Add content pages to document
- [`rtf_titles()`](https://ichirio.github.io/rtfreporter/reference/rtf_titles.md)
  : Assign content titles to pages
- [`rtf_footnotes()`](https://ichirio.github.io/rtfreporter/reference/rtf_footnotes.md)
  : Assign content footnotes to pages
- [`rtf_layout()`](https://ichirio.github.io/rtfreporter/reference/rtf_layout.md)
  [`print(`*`<rtf_pages>`*`)`](https://ichirio.github.io/rtfreporter/reference/rtf_layout.md)
  : Lay a document's contents out as printed pages
- [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  : Create an RTF table object
- [`print(`*`<rtftable>`*`)`](https://ichirio.github.io/rtfreporter/reference/print.rtftable.md)
  : Print an rtftable object
- [`format(`*`<rtftable>`*`)`](https://ichirio.github.io/rtfreporter/reference/format.rtftable.md)
  : Render an rtftable body as console text
- [`summary(`*`<rtftable>`*`)`](https://ichirio.github.io/rtfreporter/reference/summary.rtftable.md)
  : Summarise an rtftable object

## Package defaults

Inspect and reset the `rtfreporter.*` option defaults (paper size,
orientation, margins, font, font size). **Acts on:** nothing – these set
the fallbacks every other function uses when you do not pass a value. A
site can set its own in `Rprofile.site`; see the page-setup article.

- [`rtfreporter_options()`](https://ichirio.github.io/rtfreporter/reference/rtfreporter_options.md)
  : Inspect the active rtfreporter defaults
- [`rtfreporter_reset_defaults()`](https://ichirio.github.io/rtfreporter/reference/rtfreporter_reset_defaults.md)
  : Restore the factory default options

## Recipes — four complete tables from data frames

Copy-and-run examples for the four table shapes a clinical report is
mostly made of: demographics, adverse events, PK concentrations and a
laboratory shift table. Each runs end to end, data in and RTF out, and
they are shown together because **their arguments barely overlap** –
what a demographics table needs, an adverse-events table does not.

- [`rtfreporter-recipes`](https://ichirio.github.io/rtfreporter/reference/rtfreporter-recipes.md)
  : Four complete recipes: DM, AE, PK and LB

## Column headers

Build and edit the column-header block. **Acts on:** an `rtftable` –
there is no header setting at document level.
[`col_cell()`](https://ichirio.github.io/rtfreporter/reference/col_cell.md)
describes one header cell and is accepted by
[`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md),
`rtftable(col_header = )` and `rtf_tables(col_header = )`. A whole
header built with
[`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md)
goes to the same places plus
[`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md).
Coordinates are those of the **finished** table, so a merged stub counts
as one column.
[`col_key()`](https://ichirio.github.io/rtfreporter/reference/col_key.md)
lets a cell name the columns it covers by a column-name segment instead
of by position.

- [`col_cell()`](https://ichirio.github.io/rtfreporter/reference/col_cell.md)
  : Column-header cell specification

- [`col_key()`](https://ichirio.github.io/rtfreporter/reference/col_key.md)
  : Select header columns by a column-name segment

- [`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md)
  : Build a multi-row column-header specification

- [`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md)
  : Set the whole column header of a finished table (final-table
  coordinates)

- [`header_map()`](https://ichirio.github.io/rtfreporter/reference/header_map.md)
  : Show the column header of every page, cell by cell

- [`rtf_columns()`](https://ichirio.github.io/rtfreporter/reference/rtf_columns.md)
  : Column names of a finished table's body

- [`rtf_header_source()`](https://ichirio.github.io/rtfreporter/reference/rtf_header_source.md)
  :

  Deparse a table's column header back to editable
  [`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md)
  source

## Post-hoc styling verbs

Restyle a table you have already built, without rebuilding it. **Acts
on:** an `rtftable`, or a list of them – every verb here has a list
method, so one call styles every page. These are the last word: they
override whatever
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
or the source adapter set.
[`set_decimal_split()`](https://ichirio.github.io/rtfreporter/reference/set_decimal_split.md)
is the exception in kind – it does not change the data, only how a
numeric column is rendered.

- [`style_header()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)
  [`style_cols()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)
  [`style_body()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)
  [`style_zone()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)
  [`add_header_row()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)
  : Restyle an existing rtftable (post-hoc styling verbs)
- [`collapse_repeats()`](https://ichirio.github.io/rtfreporter/reference/collapse_repeats.md)
  : Blank out consecutive repeated values in a finished table
- [`set_decimal_split()`](https://ichirio.github.io/rtfreporter/reference/set_decimal_split.md)
  : Line up the decimal points of a numeric column

## Built-in cell-format functions

Re-shape the TEXT of cells so counts and percentages line up. **Acts
on:** a data.frame or a character vector, **before** it becomes a table.
Pass one to `rtftable(cell_format = )` / `as_rtftables(cell_format = )`
to apply it during the build instead.

- [`fmt_count_pct()`](https://ichirio.github.io/rtfreporter/reference/fmt_count_pct.md)
  : Format count + percent cells to a uniform display width
- [`fmt_count_paren()`](https://ichirio.github.io/rtfreporter/reference/fmt_count_paren.md)
  : Align "count (parenthetical)" cells
- [`fmt_value_paren()`](https://ichirio.github.io/rtfreporter/reference/fmt_value_paren.md)
  : Align "value (parenthetical)" cells, text values included
- [`fmt_right_align()`](https://ichirio.github.io/rtfreporter/reference/fmt_right_align.md)
  : Right-align the cells of a column to a common width

## Numeric display formatters & rounding

Turn numbers into the strings a clinical table prints – significant
digits, rounding, alignment. **Acts on:** a data.frame or a numeric
vector, **before** it becomes a table. Numbers reaching
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
unformatted are converted with
[`as.character()`](https://rdrr.io/r/base/character.html), which is
rarely what a report wants, so format first.
[`round_num()`](https://ichirio.github.io/rtfreporter/reference/round_num.md)
is the rounding rule every formatter (and a plan’s templates) uses – R’s
half-to-even, or SAS’s half-away-from-zero with
`options(rtfreporter.rounding = "sas")`.

- [`fmt_signif()`](https://ichirio.github.io/rtfreporter/reference/fmt_signif.md)
  : Format numbers to a number of significant digits
- [`fmt_round()`](https://ichirio.github.io/rtfreporter/reference/fmt_round.md)
  : Format numbers to a fixed number of decimal places
- [`fmt_numeric()`](https://ichirio.github.io/rtfreporter/reference/fmt_numeric.md)
  : Format the numeric columns of a table for display
- [`round_num()`](https://ichirio.github.io/rtfreporter/reference/round_num.md)
  : Round the way R rounds, or the way SAS rounds

## Blank rows

Insert blank separator rows by position, by value change, or by rule.

- [`set_blank_rows()`](https://ichirio.github.io/rtfreporter/reference/set_blank_rows.md)
  : Attach blank-row positions to a data.frame
- [`blank_rows_by_change()`](https://ichirio.github.io/rtfreporter/reference/blank_rows_by_change.md)
  : Blank-row specification: insert when a variable's value changes
- [`blank_rows_by_rule()`](https://ichirio.github.io/rtfreporter/reference/blank_rows_by_rule.md)
  : Blank-row specification: insert before/after rows matching a pattern

## Pagination strategies & helpers

Split a table across pages. **Acts on:** the body of a table being
built. Vertical splitting is chosen with `as_rtftables(split = )` – a
strategy name (`"group_safe"`, `"group_force"`, `"by_value"`, `"rows"`,
`"none"`) with its settings given as ordinary arguments alongside it, or
a function of your own for a bespoke rule.
[`paginate_cols()`](https://ichirio.github.io/rtfreporter/reference/paginate_cols.md)
is the horizontal counterpart: it acts on a FINISHED `rtftable`,
splitting a too-wide table by column and repeating the row headings on
every page.

- [`paginate_cols()`](https://ichirio.github.io/rtfreporter/reference/paginate_cols.md)
  : Paginate a table horizontally, by columns
- [`add_cont_label()`](https://ichirio.github.io/rtfreporter/reference/add_cont_label.md)
  : Prepend a continuation label row to a paginated chunk

## Column-width utilities

Measure text and size columns to their content. **Acts on:** a
data.frame; returns widths in twips for
`rtftable(column_widths_twips = )`. Use
[`auto_col_widths()`](https://ichirio.github.io/rtfreporter/reference/auto_col_widths.md)
when long labels wrap; `as_rtftables(auto_width = TRUE)` does the same
during the build, clamped to the writable page width.

- [`text_width_in()`](https://ichirio.github.io/rtfreporter/reference/text_width_in.md)
  : Estimate the display width of a text string
- [`auto_col_widths()`](https://ichirio.github.io/rtfreporter/reference/auto_col_widths.md)
  : Automatically calculate column widths for a data.frame
- [`default_col_widths()`](https://ichirio.github.io/rtfreporter/reference/default_col_widths.md)
  : The column widths of a table that gives none

## Listings (source data → listing body)

Reshape source data into what a listing prints, before any of it is
rendered: source variables joined into one printed column, a long cell
wrapped over several physical rows, narrow gutter columns, and a blank
row after each record. **Acts on:** a data.frame / tibble. **Produces:**
a data.frame, which
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
renders.
[`listing_col()`](https://ichirio.github.io/rtfreporter/reference/listing_col.md)
describes one printed column and
[`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md)
the listing as a whole;
[`build_listing()`](https://ichirio.github.io/rtfreporter/reference/build_listing.md)
applies the spec, and `as_rtftables(listing = )` runs the same work
inside the pipeline and derives the header, the widths and the alignment
from it. An rlistings `listing_df` needs none of this – it is already
laid out, and goes straight to
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md).
[`fit_listing_widths()`](https://ichirio.github.io/rtfreporter/reference/fit_listing_widths.md)
proposes every column’s width from the page and the data, and
[`listing_code()`](https://ichirio.github.io/rtfreporter/reference/listing_code.md)
prints the spec as source to paste and tune.
[`listing_wrap()`](https://ichirio.github.io/rtfreporter/reference/listing_wrap.md)
is the rule that breaks a cell into lines, and
[`listing_wrap_code()`](https://ichirio.github.io/rtfreporter/reference/listing_wrap_code.md)
writes that rule out as source when a listing needs a different one –
calling
[`listing_disp_width()`](https://ichirio.github.io/rtfreporter/reference/listing_measures.md),
[`listing_take()`](https://ichirio.github.io/rtfreporter/reference/listing_measures.md)
and
[`listing_split_after()`](https://ichirio.github.io/rtfreporter/reference/listing_measures.md),
the measurements a rule is built on, rather than copying them.
[`catx()`](https://ichirio.github.io/rtfreporter/reference/catx.md) is
the join a listing column makes, exported for the columns you build
yourself.

- [`listing_col()`](https://ichirio.github.io/rtfreporter/reference/listing_col.md)
  : One printed column of a listing
- [`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md)
  : Bundle the settings for one listing
- [`build_listing()`](https://ichirio.github.io/rtfreporter/reference/build_listing.md)
  : Reshape a data.frame into a listing body
- [`fit_listing_widths()`](https://ichirio.github.io/rtfreporter/reference/fit_listing_widths.md)
  : Propose the listing's column widths from the page and the data
- [`listing_code()`](https://ichirio.github.io/rtfreporter/reference/listing_code.md)
  : Print a listing spec as the code that would build it
- [`listing_wrap()`](https://ichirio.github.io/rtfreporter/reference/listing_wrap.md)
  : Break a listing cell or header into the lines it occupies
- [`listing_wrap_code()`](https://ichirio.github.io/rtfreporter/reference/listing_wrap_code.md)
  : The default wrapping rule, as source to edit
- [`listing_disp_width()`](https://ichirio.github.io/rtfreporter/reference/listing_measures.md)
  [`listing_take()`](https://ichirio.github.io/rtfreporter/reference/listing_measures.md)
  [`listing_split_after()`](https://ichirio.github.io/rtfreporter/reference/listing_measures.md)
  : The measurements a listing's wrapping rule is built on
- [`catx()`](https://ichirio.github.io/rtfreporter/reference/catx.md) :
  Join values with a separator, skipping the missing ones

## Figures

Place a plot or an image file on a page.
[`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md)
wraps a PNG / JPEG (or a plot object, saved for you);
[`rtf_figures()`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.md)
adds figures to the document, with titles and footnotes, as
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
does for tables. **Acts on:** the `rtf_document` object.

- [`rtf_figures()`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.md)
  : Add figure content to document
- [`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md)
  : Create an RTF figure object
- [`print(`*`<rtfplot>`*`)`](https://ichirio.github.io/rtfreporter/reference/print.rtfplot.md)
  : Print an rtfplot object

## Assembling a deliverable & post-processing

Join finished RTF FILES into one, with a table of contents and
continuous page numbering. **Acts on:** files on disk, not R objects –
this is the step after
[`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md).
The table of contents is a table (file, label, heading, level) –
[`assemble_folder()`](https://ichirio.github.io/rtfreporter/reference/assemble_folder.md)
makes it from a folder – or the path of one (.xlsx / .csv).
[`rtf_replace_text()`](https://ichirio.github.io/rtfreporter/reference/rtf_replace_text.md)
edits a rendered file in place – text a report needs only at delivery
time.

- [`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md)
  : Assemble multiple RTF files into one
- [`assemble_folder()`](https://ichirio.github.io/rtfreporter/reference/assemble_folder.md)
  : Assemble every RTF in a folder into one TOC deliverable
- [`rtf_replace_text()`](https://ichirio.github.io/rtfreporter/reference/rtf_replace_text.md)
  : Replace text inside a generated RTF file

## Borders

Two constructors, and only two:
[`rtf_border_line()`](https://ichirio.github.io/rtfreporter/reference/rtf_border_line.md)
describes one edge – its line style, width and colour – and
[`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
collects edges into the border of a cell, a row or a zone, including the
rules *inside* a selection (`inside_h`, `inside_v`). **Where usable:**
both are accepted by `rtftable(border = )`, `rtf_tables(border = )` and
the `style_*()` verbs;
[`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
is also accepted by `col_cell(border = )`. There is **no border setting
at document level**. Later wins, per edge – a `style_*()` call overrides
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md),
which overrides the adapter. The constructors these two replaced are
listed under *Deprecated* at the bottom of this page;
[`?rtf_border`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
pairs the old and the new spelling for every case.

- [`rtf_border_line()`](https://ichirio.github.io/rtfreporter/reference/rtf_border_line.md)
  : Single-edge border specification
- [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  : Four-edge border specification for a cell or row

## Shared table styles (S3, snapshot)

One object carrying borders, alignment, padding and row height, so a set
of tables can share a look. **Where usable:** `rtftable(style = )` and
`rtf_tables(style = )`.
[`rtf_table_style_with()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style_with.md)
derives a variant from an existing style, changing only the fields you
name.

- [`rtf_table_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style.md)
  : Shared table style

- [`rtf_table_style_with()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style_with.md)
  :

  Return a copy of an `rtf_table_style` with selected fields replaced

- [`rtf_table_style_tfl()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style_tfl.md)
  : Clinical TFL preset (table style)

## Visual preview (S3 plot methods)

Look at a table, a border or a document without writing a file. **Acts
on:** whichever object you pass; nothing is modified. These are for
checking work in progress, not for producing output.

- [`plot(`*`<rtf_border>`*`)`](https://ichirio.github.io/rtfreporter/reference/plot.rtf_border.md)
  :

  Visualise an `rtf_border`

- [`plot(`*`<rtf_border_line>`*`)`](https://ichirio.github.io/rtfreporter/reference/plot.rtf_border_line.md)
  :

  Visualise an `rtf_border_line`

- [`plot(`*`<rtf_table_border>`*`)`](https://ichirio.github.io/rtfreporter/reference/plot.rtf_table_border.md)
  :

  Visualise an `rtf_table_border`

- [`plot(`*`<rtftable>`*`)`](https://ichirio.github.io/rtfreporter/reference/plot.rtftable.md)
  :

  Visualise an `rtftable`

- [`plot(`*`<rtf_document>`*`)`](https://ichirio.github.io/rtfreporter/reference/plot.rtf_document.md)
  :

  Visualise an `rtf_document`

## Working with an AI assistant

The package is too new to be in a chat model’s training data, so an
assistant has to be told the API before it writes any. Two briefings
ship inside the package for that – one for *using* rtfreporter, one for
working *on* it – and this returns the copy installed alongside your
version, which is the one that matches what you actually have.

- [`rtfreporter_ai_manual()`](https://ichirio.github.io/rtfreporter/reference/rtfreporter_ai_manual.md)
  : The AI assistant manuals that ship with this package

## Validating rtfreporter in your environment

The templates a company copies and edits to validate the package for its
own use – a Validation Plan, user requirements linked to the package’s
Functional Requirements, UAT cases on dummy data, the Traceability
Matrix (URS -\> FR -\> UAT) and a Validation Summary Report.

- [`rtfreporter_validation_kit()`](https://ichirio.github.io/rtfreporter/reference/rtfreporter_validation_kit.md)
  : Copy the validation kit: templates to validate rtfreporter in your
  environment

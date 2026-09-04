# Package index

## Start here — four complete recipes

Copy-and-run examples for the four table shapes a clinical report is
mostly made of: demographics, adverse events, PK concentrations and a
laboratory shift table. Each runs end to end, data in and RTF out, and
they are shown together because **their arguments barely overlap** –
what a demographics table needs, an adverse-events table does not.

- [`rtfreporter-recipes`](https://ichirio.github.io/rtfreporter/reference/rtfreporter-recipes.md)
  : Four complete recipes: DM, AE, PK and LB

## Document & rendering

Start here.
[`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md)
creates the document; the section and content calls below add to it;
[`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md)
writes the file. **Acts on:** the `rtf_document` object.
[`rtf_page()`](https://ichirio.github.io/rtfreporter/reference/rtf_page.md)
and
[`rtf_default_format()`](https://ichirio.github.io/rtfreporter/reference/rtf_default_format.md)
are the only settings that are document-only – everything else in this
reference acts on a table.
[`rtf_config()`](https://ichirio.github.io/rtfreporter/reference/rtf_config.md)
edits an already-composed document;
[`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md)
starts a new one.

- [`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md)
  : Create an RTF document for pipe composition
- [`rtf_config()`](https://ichirio.github.io/rtfreporter/reference/rtf_config.md)
  : Configure document-level settings
- [`rtf_page()`](https://ichirio.github.io/rtfreporter/reference/rtf_page.md)
  : Page geometry for an RTF document
- [`rtf_default_format()`](https://ichirio.github.io/rtfreporter/reference/rtf_default_format.md)
  : Document-wide default formatting for an RTF document
- [`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md)
  : Generate an RTF file from a report object
- [`print(`*`<rtf_document>`*`)`](https://ichirio.github.io/rtfreporter/reference/print.rtf_document.md)
  : Print an rtf_document object
- [`print(`*`<rtfreport>`*`)`](https://ichirio.github.io/rtfreporter/reference/print.rtfreport.md)
  : Print an rtfreport object

## Package defaults

Inspect and reset the `rtfreporter.*` option defaults (paper size,
orientation, margins, font, font size). **Acts on:** nothing – these set
the fallbacks every other function uses when you do not pass a value. A
site can set its own in `Rprofile.site`; see the page-setup article.

- [`rtfreporter_options()`](https://ichirio.github.io/rtfreporter/reference/rtfreporter_options.md)
  : Inspect the active rtfreporter defaults
- [`rtfreporter_reset_defaults()`](https://ichirio.github.io/rtfreporter/reference/rtfreporter_reset_defaults.md)
  : Restore the factory default options

## Sections — headers & footers

A section applies a running header / footer to a range of pages. **Acts
on:** the `rtf_document` object. Headers and footers are themselves
small tables: build their rows with
[`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
/
[`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
and edit one row later with
[`update_header_row()`](https://ichirio.github.io/rtfreporter/reference/update_header_row.md)
/
[`update_footer_row()`](https://ichirio.github.io/rtfreporter/reference/update_header_row.md).

- [`rtf_section()`](https://ichirio.github.io/rtfreporter/reference/rtf_section.md)
  : Define sections for pages

- [`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
  [`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
  : Create a header or footer object for a section

- [`update_header_row()`](https://ichirio.github.io/rtfreporter/reference/update_header_row.md)
  [`update_footer_row()`](https://ichirio.github.io/rtfreporter/reference/update_header_row.md)
  :

  Update a specific row in an
  [`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
  object

## Page content — tables & figures

Place a table or a figure on the page, with its titles and footnotes.
**Acts on:** the `rtf_document` object –
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
takes the `rtftable` objects the sections below produce. **Note the two
levels:**
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
sets how a table looks; passing the same setting to
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
overrides it *for that placement only*, leaving the table object
untouched.

- [`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
  : Add content pages to document
- [`rtf_figures()`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.md)
  : Add figure content to document
- [`rtf_titles()`](https://ichirio.github.io/rtfreporter/reference/rtf_titles.md)
  : Assign content titles to pages
- [`rtf_footnotes()`](https://ichirio.github.io/rtfreporter/reference/rtf_footnotes.md)
  : Assign content footnotes to pages
- [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  : Create an RTF table object
- [`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md)
  : Create an RTF figure object
- [`print(`*`<rtftable>`*`)`](https://ichirio.github.io/rtfreporter/reference/print.rtftable.md)
  : Print an rtftable object
- [`format(`*`<rtftable>`*`)`](https://ichirio.github.io/rtfreporter/reference/format.rtftable.md)
  : Render an rtftable body as console text
- [`summary(`*`<rtftable>`*`)`](https://ichirio.github.io/rtfreporter/reference/summary.rtftable.md)
  : Summarise an rtftable object
- [`print(`*`<rtfplot>`*`)`](https://ichirio.github.io/rtfreporter/reference/print.rtfplot.md)
  : Print an rtfplot object

## Importing tables (gt / gtsummary / rtables / rlistings → rtftable)

Turn a table object – a
[`gt::gt()`](https://gt.rstudio.com/reference/gt.html) table, a
gtsummary table, an rtables/tern `VTableTree`, an rlistings
`listing_df`, or a plain data.frame – into `rtftable` objects, reading
the source’s metadata and paginating in one call. **Acts on:** the
source object. **Produces:** `rtftable` objects.
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
- [`catx()`](https://ichirio.github.io/rtfreporter/reference/catx.md) :
  Join values with a separator, skipping the missing ones

## Column headers

Build and edit the column-header block. **Acts on:** an `rtftable` –
there is no header setting at document level.
[`col_cell()`](https://ichirio.github.io/rtfreporter/reference/col_cell.md)
describes one header cell and is accepted by
[`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md),
`rtftable(col_header = )`, `rtf_tables(col_header = )` and
[`set_header_cell()`](https://ichirio.github.io/rtfreporter/reference/set_header_cell.md).
A whole header built with
[`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md)
goes to the same places plus
[`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md).
Coordinates are those of the **finished** table, so a merged stub counts
as one column.

- [`col_cell()`](https://ichirio.github.io/rtfreporter/reference/col_cell.md)
  : Column-header cell specification

- [`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md)
  : Build a multi-row column-header specification

- [`col_header_from_names()`](https://ichirio.github.io/rtfreporter/reference/col_header_from_names.md)
  : Build a spanning column header from delimited column names

- [`add_col_header_row()`](https://ichirio.github.io/rtfreporter/reference/add_col_header_row.md)
  :

  Append (or prepend) a row to an `rtf_col_header`

- [`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md)
  : Set the whole column header of a finished table (final-table
  coordinates)

- [`set_header_cell()`](https://ichirio.github.io/rtfreporter/reference/set_header_cell.md)
  : Set or merge individual column-header cells (spanning, borders,
  alignment)

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

- [`format_count_pct()`](https://ichirio.github.io/rtfreporter/reference/format_count_pct.md)
  : Format count + percent cells to a uniform display width
- [`realign_count_pct()`](https://ichirio.github.io/rtfreporter/reference/realign_count_pct.md)
  : Re-align existing "n (xx.x)" strings to a uniform display width
- [`fmt_count_paren()`](https://ichirio.github.io/rtfreporter/reference/fmt_count_paren.md)
  : Align "count (parenthetical)" cells
- [`fmt_count_paren_bare()`](https://ichirio.github.io/rtfreporter/reference/fmt_count_paren_bare.md)
  : Align "count (parenthetical)" cells, including bare counts
- [`fmt_right_align()`](https://ichirio.github.io/rtfreporter/reference/fmt_right_align.md)
  : Right-align the cells of a column to a common width

## Numeric display formatters

Turn numbers into the strings a clinical table prints – significant
digits, rounding, alignment. **Acts on:** a data.frame or a numeric
vector, **before** it becomes a table. Numbers reaching
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
unformatted are converted with
[`as.character()`](https://rdrr.io/r/base/character.html), which is
rarely what a report wants, so format first.

- [`fmt_signif()`](https://ichirio.github.io/rtfreporter/reference/fmt_signif.md)
  : Format numbers to a number of significant digits
- [`fmt_round()`](https://ichirio.github.io/rtfreporter/reference/fmt_round.md)
  : Format numbers to a fixed number of decimal places
- [`fmt_numeric()`](https://ichirio.github.io/rtfreporter/reference/fmt_numeric.md)
  : Format the numeric columns of a table for display

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

## Borders

Two constructors, and only two:
[`rtf_border_side()`](https://ichirio.github.io/rtfreporter/reference/rtf_border_side.md)
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

- [`rtf_border_side()`](https://ichirio.github.io/rtfreporter/reference/rtf_border_side.md)
  : Single-edge border specification
- [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  [`rtf_border_none()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  [`rtf_border_top()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  [`rtf_border_bottom()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  [`rtf_border_box()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
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

## Visual preview (S3 plot methods)

Look at a table, a border or a document without writing a file. **Acts
on:** whichever object you pass; nothing is modified. These are for
checking work in progress, not for producing output.

- [`plot(`*`<rtf_border>`*`)`](https://ichirio.github.io/rtfreporter/reference/plot.rtf_border.md)
  :

  Visualise an `rtf_border`

- [`plot(`*`<rtf_border_side>`*`)`](https://ichirio.github.io/rtfreporter/reference/plot.rtf_border_side.md)
  :

  Visualise an `rtf_border_side`

- [`plot(`*`<rtf_table_border>`*`)`](https://ichirio.github.io/rtfreporter/reference/plot.rtf_table_border.md)
  :

  Visualise an `rtf_table_border`

- [`plot(`*`<rtftable>`*`)`](https://ichirio.github.io/rtfreporter/reference/plot.rtftable.md)
  :

  Visualise an `rtftable`

- [`plot(`*`<rtf_document>`*`)`](https://ichirio.github.io/rtfreporter/reference/plot.rtf_document.md)
  :

  Visualise an `rtf_document`

## Assembling multiple RTF files

Join finished RTF FILES into one, with a table of contents and
continuous page numbering. **Acts on:** files on disk, not R objects –
this is the step after
[`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md).
A spec
([`assemble_spec()`](https://ichirio.github.io/rtfreporter/reference/assemble_spec.md),
a .csv/.xlsx) names the files and their TOC entries.

- [`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md)
  : Assemble multiple RTF files into one
- [`assemble_files()`](https://ichirio.github.io/rtfreporter/reference/assemble_files.md)
  : Collect the RTF files in a folder
- [`assemble_folder()`](https://ichirio.github.io/rtfreporter/reference/assemble_folder.md)
  : Assemble every RTF in a folder into one TOC deliverable
- [`assemble_spec()`](https://ichirio.github.io/rtfreporter/reference/assemble_spec.md)
  : Build an assembly spec (one editable row per RTF)
- [`assemble_from_spec()`](https://ichirio.github.io/rtfreporter/reference/assemble_from_spec.md)
  : Assemble RTF files from an assembly spec
- [`assemble_toc()`](https://ichirio.github.io/rtfreporter/reference/assemble_toc.md)
  : Build a TOC definition from a set of RTF files
- [`toc_heading()`](https://ichirio.github.io/rtfreporter/reference/toc_heading.md)
  : Build a structured TOC heading
- [`toc_entry()`](https://ichirio.github.io/rtfreporter/reference/toc_entry.md)
  : Build a structured TOC entry

## Post-processing

Edit a rendered RTF file in place – substituting text a report needs
only at delivery time, such as a run date or a draft watermark. **Acts
on:** a file on disk.

- [`rtf_replace_text()`](https://ichirio.github.io/rtfreporter/reference/rtf_replace_text.md)
  : Replace text inside a generated RTF file

## Superseded

Still working and **not** scheduled for removal: newer work goes into
the replacement each page names, but existing code need not change.

- [`paginate()`](https://ichirio.github.io/rtfreporter/reference/paginate.md)
  : Split a table object into per-page data.frames (deprecated)

## Deprecated – scheduled for removal

The border constructors that
[`rtf_border_side()`](https://ichirio.github.io/rtfreporter/reference/rtf_border_side.md)
and
[`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
replaced at 0.5.0 / 0.6.0. They still work and each warns once per
session, naming its replacement, but they will be **removed in one batch
before the CRAN submission** – do not write new code against them.
[`?rtf_border`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
pairs the old and the new spelling for every case.
[`rtf_border_none()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md),
[`rtf_border_top()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md),
[`rtf_border_bottom()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
and
[`rtf_border_box()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
are documented on the
[`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
page itself.

- [`rtf_border_with()`](https://ichirio.github.io/rtfreporter/reference/rtf_border_with.md)
  :

  Return a copy of an `rtf_border` with selected sides replaced

- [`rtf_table_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_border.md)
  : Per-zone border specification for a table

- [`rtf_border_tfl()`](https://ichirio.github.io/rtfreporter/reference/rtf_border_tfl.md)
  : Clinical TFL-style table border preset

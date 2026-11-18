# rtfreporter 0.9.0

First CRAN release.  rtfreporter writes clinical tables, listings and figures
as RTF files.  It starts from an analysis results dataset (ARD), maps it to a
table with a plan, and renders the result into RTF documents that can be
assembled into one deliverable.  It also reads tables from gt, gtsummary,
rtables, tfrmt, flextable and huxtable.

rtfreporter needs R 4.3 or later.

## Main features

Data in

- `normalize_ard()` reads a cards ARD into one tidy form.  It can stack
  parallel subgroups (`stack`, `stack_nest`) and name the hierarchy.
  `combine` reads variables the data keeps apart as one table dimension,
  for example ADaM's grades toward low and toward high in a lab shift
  table.
  `widen_ard()`, `pull_ard()` and `list_ard_keys()` work on the same data
  without a plan.

Table plans

- `table_plan()` maps an ARD to rows and columns.  The `plan_*()` verbs
  refine it: `plan_cells()`, `plan_digits()`, `plan_labels()`,
  `plan_levels()`, `plan_col_header()`, `plan_stub()`, `plan_row_group()`,
  `plan_style()`, `plan_cell_style()`, `plan_columns()`, `plan_titles()`,
  `plan_footnotes()`.  `plan_append()` stacks further tables under the
  first.  `plan_template()` writes starting
  code for an ARD.
- More verbs: `plan_total()` (Total column or row), `plan_compare()`
  (p-values and differences as columns), `plan_stat_groups()` (one record's
  statistics in several columns), `plan_filter()`, `plan_nest()`,
  `plan_sort()`, `plan_hide()`, `plan_empty()`, `plan_blanks()`.
- Pages: `plan_paginate_rows()`, `plan_paginate_group()` and
  `plan_paginate_cols()`.  `plan_header_tokens()` and `plan_n_candidates()`
  list the values a header's `{n}` can take.
- `as_rtftables()` brings a finished table from gt, gtsummary, rtables,
  tfrmt, flextable, huxtable or a data frame into the same pipeline.  From
  gt and gtsummary it reads the caption, titles, spanners, stub, row groups,
  cell styles, footnotes and their marks; split gt groups keep their own
  rows.

Listings

- `plan_listing()`, `listing_spec()` and `listing_col()` describe a listing.
  `build_listing()` and `fit_listing_widths()` lay it out;
  `collapse_repeats()` blanks repeated key values.

Figures

- `rtfplot()` and `rtf_figures()` place ggplot2 plots, files or drawing
  functions.  `render_width`, `render_height`, `render_dpi` and `png_device`
  set the drawing; `render_format = "emf"` draws a vector EMF picture (with
  devEMF).  Plots are drawn with ragg when it is installed.

RTF output and page layout

- `rtf_document()` sets the page, fonts, tokens and the output `file`.
  `rtf_section()` adds sections with `header` and `footer`;
  `rtf_header()`, `rtf_footer()`, `rtf_titles()`, `rtf_footnotes()` and
  `rtf_watermark()` fill the page.
- `rtf_tables()` and `rtf_figures()` add content.  With `page_break = FALSE`
  a table or figure goes under the content before it.
- A font can be set per script (`latin`, `east_asian`, `complex`) and
  `font_lang` sets the language, so East Asian text works in Word and
  LibreOffice.
- Borders belong to a whole block: the `border` of `rtf_header()`,
  `rtf_footer()`, `rtf_titles()` and `rtf_footnotes()` draws above, below and
  between the block's rows.  `plan_titles(border = )` and
  `plan_footnotes(border = )` give one table its own block border.
- A font family name that is not ASCII (for example a Japanese or Chinese
  font) is written in its code page with a Latin alternative, so Word shows
  it.
- `generate_rtfreport()` writes the file.  `rtf_style()`, `rtf_page()`,
  `rtf_table_style()`, `rtf_table_style_tfl()` and `set_col_header()` style
  the document, tables and headers.

Assembling

- `assemble_rtf()` joins RTF files with a cover, table of contents and page
  numbers.  `assemble_folder()` reads a folder into a table of contents and
  can assemble it.  Entries are labelled by the files' titles;
  `toc_style` sets the sizes, bold and an optional hanging indent.

Validation material

- `rtfreporter_validation_kit()` copies templates for a company's own
  validation.  The package ships its requirements, traceability matrix and
  quality plan in `inst/validation`.
- `rtfreporter_ai_manual()` writes the user and developer manuals, and a
  folder to attach to an AI chat.

## Changes for users of the pre-CRAN development versions

Removed functions.  They warned in 0.8.x and are gone.

- `paginate()` is `as_rtftables()`.
- `rtf_border_none()`, `rtf_border_top()`, `rtf_border_bottom()`,
  `rtf_border_box()`, `rtf_border_with()`, `rtf_border_tfl()` and
  `rtf_table_border()` are `rtf_border()`, `rtf_table_style()` and the
  `border` argument.
- `add_col_header_row()`, `col_header_from_names()` and `set_header_cell()`:
  write the rows in `rtf_col_header()` or `set_col_header()`, add a row with
  `add_header_row()`, restyle cells with `style_header()`.
- `update_header_row()` and `update_footer_row()`: build the band again with
  `rtf_header(rows = )` or `rtf_footer(rows = )`.
- `assemble_files()`, `assemble_spec()`, `assemble_toc()`,
  `assemble_from_spec()`, `toc_heading()` and `toc_entry()`:
  use `assemble_folder(dir)` (without `output_file` it returns the table of
  contents) and `assemble_rtf(toc = )`.
- `read_gt` is `as_rtftables()`.

Renamed functions and arguments.  Old names are not kept, except where
noted.

- `format_count_pct()` is `fmt_count_pct()`.
- `fmt_count_paren_bare(x)` is `fmt_count_paren(x, bare = TRUE)`.
- `realign_count_pct()` is `as_rtftables(align_count_pct = TRUE)`; the cell
  format `"realign_count_pct"` is `"count_pct"`.
- `rtf_border_side()` is `rtf_border_line()`.
- `rtfreporter_ai_manual(file = )` is `dir = `.
- `set_blank_rows(df = )` and `add_cont_label(chunk = )` are `data = `.
- `as_rtftable(gt_obj = )` is `x = `.
- `size_half_points` is `font_size_half_points` in `text_width_in()`,
  `auto_col_widths()` and `fit_listing_widths()`; `auto_col_widths(df = )`
  is `data = `.
- `generate_rtfreport(report = )` is `doc = `.  `report =` still works
  but is superseded.
- Every `plan_*()` function names its first argument `.plan`.  Piped code is
  unchanged; write `.plan = `, not `plan = `.

Removed arguments.

- The 26 table-format arguments of `rtf_tables()` (`border`,
  `col_rel_width`, `font` and the rest) are gone.  Give them to `rtftable()`,
  `as_rtftables()` or `plan_style()`.
- `rtftable(spanning_header = )`: put the spanning row first in
  `col_header = list(<spanning row>, <label row>)`.
- `as_rtftables(stub_vars = , stub_label = , stub_indent = ,
  stub_group_summary = )`: write `stub = stub_spec(vars, label = , indent = ,
  group_summary = )`.
- `paginate_cols()` takes `cut_by`, `every`, `keep`, `fit` and `order`.  The
  older `cols`, `by`, `carry`, `width` and `page_order` still work but are
  superseded.
- `assemble_rtf(toc_page_numbering = )` is gone: every page of the output,
  cover and table of contents included, is numbered in one sequence, and
  the total is all pages.

Changed defaults and behaviour.

- `generate_rtfreport(overwrite = TRUE)` is the default.  `file_path` is
  `NULL` by default: name the file there or in `rtf_document(file = )`.
- `rtf_table_style(align = NULL)`: a style that does not say `align` no
  longer left-aligns the value columns.  Write `align = "left"` for the old
  look.
- `text_width_in()`, `auto_col_widths()` and `fit_listing_widths()` default
  to `font = NULL`, the package font.
- `rtf_tables(auto_section = , auto_title = )` default to `NULL`: they follow
  `plan_paginate_group(heading_at = )`, else `FALSE`.
- `plan_columns(row_title = )` and `plan_row_group(collapse = )` count the
  printed columns, as `widths` does.  A hidden key no longer counts.
- A page key (`plan_paginate_rows(page_by = )`, `plan_paginate_group(col = )`)
  is never a row group for blank rows, `collapse`, `row_title` and the stub.
- `plan_labels()` and `widen_ard(labels = )` set text only.  They no longer
  put labelled variables first; the order is `plan_levels()` or the ARD's.
- A declared level order now orders a table with no row keys.
- A header's `{n:sum}` leaves the Total column out.
- A second call of `plan_stub()`, `plan_total()`, `plan_empty()`,
  `plan_sort()` or `plan_paginate_group()` keeps the arguments it leaves
  out from the earlier call; it no longer resets them to the defaults.
- `blank_rows` positions (and `set_blank_rows()`, `plan_blanks(where = )`)
  count the rows of the whole body, not of each page: each blank row
  appears once.
- In a `multiline` listing a `/` inside a value (such as `64/M`) is data:
  the value is not broken there.
- An rlistings listing with `max_rows`, and each listing of a list given to
  `as_rtftables()`, is paged as a listing alone is.
- `as_rtftables()` aligns a spanning header as its source prints it
  (centred for gt, gtsummary, tfrmt and rtables).  Footnote lines read from
  gt start with their marks.
- Table of contents entries made with `toc = "auto"` use all of a file's
  title rows; the page numbers end at the right margin.
- The package reads its resource files only from the installed package.
- `{DATE}` is not a token.  Use `{DATETIME:%Y-%m-%d}`.
- `plan_sort()` puts a group's own row before its members.
- A gt table with `groupname_col` comes through `as_rtftables()` with the
  groups as heading rows.
- Plot objects are drawn with ragg when it is installed.  Set
  `options(rtfreporter.png_device = "png")` for the earlier PNG bytes.
- cards 0.7.0 or later is needed to run the examples (`ard_summary()`,
  `ard_tabulate()`).
- Mistakes that were ignored now stop with a message: a `secinfo` list with
  another name, a header cell not named `l`, `c` or `r`, a positional
  column-header row with too few labels, `rtf_document(page = "a4")`.

# rtfreporter 0.8.2

This and the earlier versions were development versions before the first
CRAN release, used on GitHub only.
The changes that matter to their users are listed above under "Changes for
users of the pre-CRAN development versions".

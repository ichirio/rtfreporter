# rtfreporter 0.9.0

Initial CRAN release.

rtfreporter writes the tables, listings and figures of a clinical study
report as RTF files.  It needs R 4.3 or later.

- Tables from an analysis results dataset (ARD, as built by cards and
  cardx): `table_plan()` maps the ARD to rows and columns, and the
  `plan_*()` verbs set formats, labels, totals, comparisons, sorting,
  styles and pagination.
- Tables made with gt, gtsummary, rtables, tfrmt, flextable or huxtable, or
  a data frame, come in through `as_rtftables()`.
- Listings: `plan_listing()` and `listing_spec()` lay out, wrap and page a
  listing by subject.
- Figures: `rtfplot()` and `rtf_figures()` place ggplot2 plots, image files
  or drawings as PNG or EMF.
- Pages: `rtf_document()`, `rtf_section()` and their parts set the page,
  fonts (including East Asian fonts), headers, footers, titles, footnotes,
  borders and page-number fields; `generate_rtfreport()` writes the file.
- Assembling: `assemble_rtf()` and `assemble_folder()` join RTF files into
  one document with a cover, table of contents and page numbers.
- Validation material: `rtfreporter_validation_kit()` copies templates for
  a company's own validation, and the package ships its requirements,
  traceability matrix and quality plan in `inst/validation`.

## Known issues

- `plan_total(direction = "cols")` or `"both"` in a shift table (the arm
  and the post-baseline category as two column levels) reads the BIGN rows
  as rows of the Total column and ignores `position = "last"` (#1103).
  The default Total row (`direction = "rows"`) is correct.

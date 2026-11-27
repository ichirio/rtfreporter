# Changelog

## rtfreporter 0.9.0

Initial CRAN release.

rtfreporter writes the tables, listings and figures of a clinical study
report as RTF files. It needs R 4.3 or later.

- Tables from an analysis results dataset (ARD, as built by cards and
  cardx):
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md)
  maps the ARD to rows and columns, and the `plan_*()` verbs set
  formats, labels, totals, comparisons, sorting, styles and pagination.
- Tables made with gt, gtsummary, rtables, tfrmt, flextable or huxtable,
  or a data frame, come in through
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md).
- Listings:
  [`plan_listing()`](https://ichirio.github.io/rtfreporter/reference/plan_listing.md)
  and
  [`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md)
  lay out, wrap and page a listing by subject.
- Figures:
  [`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md)
  and
  [`rtf_figures()`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.md)
  place ggplot2 plots, image files or drawings as PNG or EMF.
- Pages:
  [`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md),
  [`rtf_section()`](https://ichirio.github.io/rtfreporter/reference/rtf_section.md)
  and their parts set the page, fonts (including East Asian fonts),
  headers, footers, titles, footnotes, borders and page-number fields;
  [`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md)
  writes the file.
- Assembling:
  [`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md)
  and
  [`assemble_folder()`](https://ichirio.github.io/rtfreporter/reference/assemble_folder.md)
  join RTF files into one document with a cover, table of contents and
  page numbers.
- Validation material:
  [`rtfreporter_validation_kit()`](https://ichirio.github.io/rtfreporter/reference/rtfreporter_validation_kit.md)
  copies templates for a company’s own validation, and the package ships
  its requirements, traceability matrix and quality plan in
  `inst/validation`.

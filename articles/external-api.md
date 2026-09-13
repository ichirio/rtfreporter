# External API specification

[日本語](https://ichirio.github.io/rtfreporter/articles/external-api-ja.md)

This is the contributor-facing reference for rtfreporter’s **public
API**: the exported functions, the **S3-class objects they return**, and
the pipe flow that ties them together. For the big picture see
[Architecture &
internals](https://ichirio.github.io/rtfreporter/articles/architecture.md);
for the internal data structures see [Internal class design
(S3)](https://ichirio.github.io/rtfreporter/articles/internal-design.md).

> rtfreporter is **pure S3** with no hard runtime dependencies. Every
> public object is a plain `list` carrying an S3 `class` attribute, so
> results are [`dput()`](https://rdrr.io/r/base/dput.html)-able, easy to
> inspect with [`str()`](https://rdrr.io/r/utils/str.html), and
> serialise without surprises.

## Terminology

“S3 functions” can be ambiguous, so to be precise:

- **Constructor functions** — exported functions that build and return
  an **object of an S3 class**
  (e.g. [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  returns an object of class `"rtftable"`). These are most of the public
  API below.
- **S3 generics / methods** —
  [`print()`](https://rdrr.io/r/base/print.html) and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) dispatch on
  the object’s class
  (e.g. [`print.rtftable()`](https://ichirio.github.io/rtfreporter/reference/print.rtftable.md)).

When this document says “the object returned by `f()`”, it means the
S3-class object a constructor produces – not a generic.

## The pipe flow

A report is built by piping an `rtf_document` through content and
section calls, then rendering:

``` r

library(rtfreporter)

rtf_document() |>                                  # -> rtf_document
  rtf_tables(as_rtftables(df)) |>                  # add a content page
  rtf_section(page = 1, secinfo = list(            # assign a header/footer
    header = rtf_header(...), footer = rtf_footer(...))) |>
  generate_rtfreport("out.rtf", overwrite = TRUE)  # render to a file
```

[`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md)
and the `rtf_*()` pipe verbs return a **new** `rtf_document`
(copy-on-modify); nothing is mutated in place.

## Public classes and their constructors

| Class | Constructor(s) | Role |
|----|----|----|
| `rtf_document` | [`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md) | The document being assembled (pages + sections + document-wide settings). |
| `rtftable` | [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md), [`as_rtftable()`](https://ichirio.github.io/rtfreporter/reference/as_rtftable.md), [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md) | One table (or one page of a paginated table). |
| `rtfplot` | [`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md) | One embedded figure. |
| `rtf_border` | [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md), `TRUE`, `rtf_border(top = TRUE)` / `_bottom()` / `_box()` / `_none()`, [`rtf_border_with()`](https://ichirio.github.io/rtfreporter/reference/rtf_border_with.md) | A border for a selection: its outer edges plus the rules between the rows (`inside_h`) and between the cells (`inside_v`) inside it. What it applies to is decided by where you attach it. |
| `rtf_table_border` | [`rtf_table_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_border.md), [`rtf_border_tfl()`](https://ichirio.github.io/rtfreporter/reference/rtf_border_tfl.md) — **both deprecated** | The five-zone map. Superseded: attach an [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md) to `rtftable(border = )` for the whole table, or to [`style_zone()`](https://ichirio.github.io/rtfreporter/reference/style_header.md) for one kind of row; the TFL preset is `border = "tfl"` or [`rtf_table_style_tfl()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style_tfl.md). |
| `rtf_table_style` | [`rtf_table_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style.md), [`rtf_table_style_with()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style_with.md), [`rtf_table_style_tfl()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style_tfl.md) | A reusable style (borders + padding + row-height defaults), captured by snapshot at construction. |
| `rtf_col_header` | [`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md), [`col_cell()`](https://ichirio.github.io/rtfreporter/reference/col_cell.md), [`add_col_header_row()`](https://ichirio.github.io/rtfreporter/reference/add_col_header_row.md) | Multi-row / spanning column headers. |

Not every public function is a constructor.
[`rtf_config()`](https://ichirio.github.io/rtfreporter/reference/rtf_config.md)
is a **configurator**, not a constructor: it takes an `rtf_document`,
updates only the non-`NULL` fields, and returns a copy of the **same**
`rtf_document` class (it sets no new class) – just like the other pipe
verbs
([`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md),
[`rtf_section()`](https://ichirio.github.io/rtfreporter/reference/rtf_section.md),
…). Likewise
[`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
/
[`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
return plain (unclassed) lists describing a section’s header / footer,
and
[`update_header_row()`](https://ichirio.github.io/rtfreporter/reference/update_header_row.md)
/
[`update_footer_row()`](https://ichirio.github.io/rtfreporter/reference/update_header_row.md)
return an updated copy.

> **`rtf_document` vs `rtfreport`.** `rtf_document` is the *public*
> object you build and hold via the pipe. It is converted to a separate
> *internal* class, `rtfreport`, only at render time (by
> [`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md));
> see [Internal class design
> (S3)](https://ichirio.github.io/rtfreporter/articles/internal-design.md).
> You do not construct or handle `rtfreport` directly.

## Content, sections and rendering

- **Content** — `rtf_tables(doc, tables, ...)` and
  `rtf_figures(doc, figures, ...)` append content pages (one element =
  one page);
  [`rtf_titles()`](https://ichirio.github.io/rtfreporter/reference/rtf_titles.md)
  /
  [`rtf_footnotes()`](https://ichirio.github.io/rtfreporter/reference/rtf_footnotes.md)
  attach per-page title/footnote blocks. Each returns the modified
  `rtf_document`.
- **Sections** — `rtf_section(doc, page, secinfo)` assigns a
  header/footer to a page range (an overlay on the flat page list;
  `NULL` inherits the previous section).
- **Render** — `generate_rtfreport(doc, file, overwrite)` writes the
  `.rtf` and returns the path invisibly.

## Importing and paginating tables

`as_rtftables(x, ...)` is the unified entry point that converts a `gt` /
gtsummary / rtables-`VTableTree` / `data.frame` / tibble (or a list of
these) into a **list of `rtftable` page objects**, reading the source
metadata and paginating in one call. `as_rtftable(x, ...)` is the
single-page form (one `rtftable`). Pagination strategies are chosen by
name (`split = "group_safe"` and the rest, with their settings passed
alongside) and a custom `split=` function is accepted; blank separator
rows are controlled by `blank_rows` /
[`set_blank_rows()`](https://ichirio.github.io/rtfreporter/reference/set_blank_rows.md)
(and may be counted toward `max_rows` with `count_blank_rows = TRUE`).
The body can also be **ordered** before the split (`sort_by` /
`sort_desc`), **grouped** on a chosen column (`group_col` / `group_by`,
plus `collapse_repeats` for repeat suppression), and **columns hidden**
from the printed pages while still driving that grouping/ordering
(`drop_cols` — e.g. a sort-key carrier column). All of these share the
input body’s column coordinates; see [Paginating with
`as_rtftables()`](https://ichirio.github.io/rtfreporter/articles/pagination.md)
for the full treatment.

## Output helpers

- [`rtf_replace_text()`](https://ichirio.github.io/rtfreporter/reference/rtf_replace_text.md)
  — post-processing find/replace on a rendered `.rtf`.
- [`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md)
  (and
  [`assemble_files()`](https://ichirio.github.io/rtfreporter/reference/assemble_files.md)
  /
  [`assemble_folder()`](https://ichirio.github.io/rtfreporter/reference/assemble_folder.md)
  /
  [`assemble_spec()`](https://ichirio.github.io/rtfreporter/reference/assemble_spec.md)
  /
  [`assemble_from_spec()`](https://ichirio.github.io/rtfreporter/reference/assemble_from_spec.md)
  /
  [`assemble_toc()`](https://ichirio.github.io/rtfreporter/reference/assemble_toc.md),
  [`toc_heading()`](https://ichirio.github.io/rtfreporter/reference/toc_heading.md)
  /
  [`toc_entry()`](https://ichirio.github.io/rtfreporter/reference/toc_entry.md))
  — combine several RTFs into one deliverable with a table of contents
  and bookmarks.

## S3 generics

[`print()`](https://rdrr.io/r/base/print.html) methods exist for the
document and the spec objects;
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods give
base-graphics wireframes of borders / tables / the document for
sanity-checking a layout before rendering.
[`print()`](https://rdrr.io/r/base/print.html) on an `rtftable` renders
a visual preview of the table – the laid-out cells with column (and
spanning) headers, per-column alignment and the border rules – so you
can eyeball the content in the console
([`format()`](https://rdrr.io/r/base/format.html) returns those lines,
[`summary()`](https://rdrr.io/r/base/summary.html) the compact metadata
block). See the [function
reference](https://ichirio.github.io/rtfreporter/reference/index.md) for
the full per-function detail.

# Architecture & internals (for contributors)

[In
Japanese](https://ichirio.github.io/rtfreporter/articles/architecture-ja.md)

This article is the English entry point for people who want to **extend
or contribute to** rtfreporter. It gives the big picture; the two
deep-dive documents are linked below (each opens in English with a
Japanese toggle).

> 🌐 **Design specifications:** [Internal class design
> (S3)](https://ichirio.github.io/rtfreporter/articles/internal-design.md)
> · [External API
> specification](https://ichirio.github.io/rtfreporter/articles/external-api.md)

## Design principles

- **S3 throughout.** Every object is a plain S3 list, with no external
  OOP framework. Objects are therefore
  [`dput()`](https://rdrr.io/r/base/dput.html)-able and serialise
  cleanly with [`saveRDS()`](https://rdrr.io/r/base/readRDS.html).
- **Zero hard runtime dependencies.** `Imports:` is only `methods`
  (ships with R). Every optional integration (gt, gtsummary,
  rtables/tern, ggplot2, …) lives in `Suggests:` and is reached through
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) with a
  helpful error if absent.
- **twips are the only length unit** used internally (1 inch = 1440
  twips).
- **Immutable, functional helpers.** Internal mutators return a *new*
  copy of the object rather than mutating in place.
- **“none” means omit.** A border side of `"none"` emits no RTF command
  at all, rather than an explicit zero-width border.

## Repository layout

    rtfreporter/
    ├── R/
    │   ├── pipe-composition.R   # public pipe API: rtf_document/section/tables/figures
    │   ├── rtftable.R           # rtftable() S3 object + normalisation helpers
    │   ├── rtfplot.R            # rtfplot() S3 object
    │   ├── rtfreport.R          # internal rtfreport document object + rtf_header/footer
    │   ├── generate_rtfreport.R # the RTF renderer (S3 doc -> .rtf text)
    │   ├── rtf_border.R         # border hierarchy (S3)
    │   ├── rtf_table_style.R    # shared table style (S3, snapshot semantics)
    │   ├── col_header.R         # unified column-header API (col_cell / rtf_col_header)
    │   ├── as_rtftables.R       # table-object -> list of rtftable pages (the hub)
    │   ├── as_rtftable.R        # single-page convenience wrapper
    │   ├── gt_adapter.R         # gt / gtsummary metadata extraction
    │   ├── rtables_adapter.R    # rtables / tern metadata extraction
    │   ├── paginate.R           # (deprecated) pagination generic + internal core
    │   ├── blank_rows.R         # blank-row specifications + attribute reader
    │   └── ...
    ├── inst/resources/          # admin-tunable RTF command templates & defaults
    ├── tests/testthat/          # the test suite (S3, edition 3)
    └── vignettes/               # user guides (root) + articles/ (site-only)

## The object model

    rtf_document          (public S3, built by the pipe API)
      ├── contents[]      one entry per page: rtftable | rtfplot
      ├── titles[]        per-page title block (character vector)
      ├── footnotes[]     per-page footnote block
      └── sections[]      header/footer per page-range (via from_page)

    rtftable              one table (single- or multi-data.frame)
      ├── data / data_list
      ├── col_header(_list)   character vector | stacked spanning rows
      ├── col_spec            per-column align / bold / italic / indent / border
      ├── cell_styles         per-cell overrides (bold/italic/underline/indent)
      ├── border              rtf_table_border (zones)
      └── width / height / padding / valign fields

    rtfplot               one embedded PNG/JPEG figure

`rtf_document` is the public, pipe-friendly surface. Internally it is
converted to an `rtfreport` object by `.pipe_doc_to_rtfreport()`,
validated by `.rtfreport_validate()`, and handed to the renderer.

## The rendering pipeline

    rtf_document()  |>  rtf_section()  |>  rtf_tables() / rtf_figures()
            │
            ▼   .pipe_doc_to_rtfreport()         (adapter: pipe API -> rtfreport)
       rtfreport
            │   .rtfreport_validate()            (auto-default section, checks)
            ▼
       generate_rtfreport(doc, "out.rtf")        (renderer in generate_rtfreport.R)
            │   .render_rtftable() / .render_rtfplot()
            ▼
       .rtf  text file

The renderer is pure string assembly driven by templates in
`inst/resources/rtf_commands.R`; size/spacing defaults live in
`inst/resources/rtfreporter_defaults.R` so they can be tuned without
touching code.

## The table-object hub: `as_rtftables()`

[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
is the single entry point that turns an external *table object* into a
list of `rtftable` pages. It supports `gt_tbl`, **gtsummary** tables,
**rtables/tern** (`VTableTree`), and plain `data.frame`/tibble (or a
list of any of these).

Every source is reduced to one common intermediate — a **kwargs list** —
which the rest of the pipeline consumes identically:

``` r
list(
  data            = <data.frame>,   # the rendered body
  col_header      = <chr | stacked spanning rows>,
  col_spec        = <list(list(col=, align=, ...), ...)>,
  cell_styles     = <list per row, or NULL>,
  titles_block    = <chr | NULL>,   # -> page title
  footnotes_block = <chr | NULL>    # -> page footnote
)
```

`gt_adapter.R` and `rtables_adapter.R` each produce exactly this shape.
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
then (optionally) **sorts** the body rows before the split (`sort_by` /
`sort_desc`, so grouping sees the ordered rows), paginates the body
(`.paginate_df()`), slices `cell_styles` per page, replicates the shared
header/width/spanning metadata onto every page, and attaches the
title/footnote blocks as the `rtf_titles` / `rtf_footnotes` attributes
that
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
reads. A column can be carried through the split for grouping/ordering
and then **hidden** from the rendered pages (`drop_cols`): the columns
stay present for `.paginate_df()`, then `.apply_col_drop()` removes them
and reindexes every position-indexed argument (header incl. spanning,
`col_spec`, widths, `row_title`, `cell_styles`) to the remaining columns
just before each page’s
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
is built.

Because the adapters all converge on one shape, **adding a new source
type is a small, well-contained job** — see [Adding a table-object
adapter](https://ichirio.github.io/rtfreporter/articles/extending-adapters.md).

## Where to read more

- Per-function reference: the
  [Reference](https://ichirio.github.io/rtfreporter/reference/index.md)
  index.
- The exact public S3 contract: [External API
  specification](https://ichirio.github.io/rtfreporter/articles/external-api.md).
- The internal class design and helper catalogue: [Internal class design
  (S3)](https://ichirio.github.io/rtfreporter/articles/internal-design.md).
- Contribution workflow and code style:
  [CONTRIBUTING.md](https://github.com/ichirio/rtfreporter/blob/main/CONTRIBUTING.md).

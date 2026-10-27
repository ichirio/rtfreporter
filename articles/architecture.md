# Architecture & internals (for contributors)

[In
Japanese](https://ichirio.github.io/rtfreporter/articles/architecture-ja.md)

This article is the English entry point for people who want to **extend
or contribute to** rtfreporter. It gives the big picture; the two
deep-dive documents are linked below (each opens in English with a
Japanese toggle).

> 🌐 **Design specification:** [Internal class design
> (S3)](https://ichirio.github.io/rtfreporter/articles/internal-design.md)

## Design principles

- **S3 throughout.** Every object is a plain S3 list, with no external
  OOP framework. Objects are therefore
  [`dput()`](https://rdrr.io/r/base/dput.html)-able and serialise
  cleanly with [`saveRDS()`](https://rdrr.io/r/base/readRDS.html).
- **Zero third-party runtime dependencies.** `Imports:` holds only
  packages that ship with R (`grDevices`, `grid`, `methods`, `stats`,
  `utils`). Every optional integration (gt, gtsummary, rtables/tern,
  flextable, huxtable, ggplot2, …) lives in `Suggests:` and is reached
  through `.need_pkg()`, which errors helpfully when the package is
  absent and warns when it is too old.
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
    │   ├── blocks.R             # rtf_block / rtf_contents: a document's contents
    │   ├── content.R            # .rtf_content(): anything that can be content, as content pages
    │   ├── layout.R             # rtf_layout(): the contents as printed pages
    │   ├── flow.R               # page_break = FALSE / first_rows: cutting a table for a shared page
    │   ├── render_page.R        # drawing a page: its segments; .render_content() per kind
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
      ├── contents        rtf_contents: one rtf_block per content page
      │     └── rtf_block   content (rtftable | rtfplot), title, footnote,
      │                     page_break, space_before, group, page_in_group
      └── sections[]      header/footer per page-range (via from_page)

    rtf_layout(doc) -> rtf_pages: the blocks as printed pages (a block with
      page_break = FALSE goes on the page before it; nothing is measured)

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

## The public classes and their constructors

| Class | Constructor(s) | Role |
|----|----|----|
| `rtf_document` | [`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md) | The document being assembled (pages + sections + document-wide settings). |
| `rtftable` | [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md), [`as_rtftable()`](https://ichirio.github.io/rtfreporter/reference/as_rtftable.md), [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md) | One table (or one page of a paginated table). |
| `rtfplot` | [`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md) | One embedded figure. |
| `rtf_border` | [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md), `TRUE`, `rtf_border(top = TRUE)` (the older [`rtf_border_top()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md) / `_bottom()` / `_box()` / `_none()` / [`rtf_border_with()`](https://ichirio.github.io/rtfreporter/reference/rtf_border_with.md) are **deprecated**) | A border for a selection: its outer edges plus the rules between the rows (`inside_h`) and between the cells (`inside_v`) inside it. What it applies to is decided by where you attach it. |
| `rtf_table_border` | [`rtf_table_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_border.md), [`rtf_border_tfl()`](https://ichirio.github.io/rtfreporter/reference/rtf_border_tfl.md) — **both deprecated** | The five-zone map. Superseded: attach an [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md) to `rtftable(border = )` for the whole table, or to [`style_zone()`](https://ichirio.github.io/rtfreporter/reference/style_header.md) for one kind of row; the TFL preset is `border = "tfl"` or [`rtf_table_style_tfl()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style_tfl.md). |
| `rtf_table_style` | [`rtf_table_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style.md), [`rtf_table_style_with()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style_with.md), [`rtf_table_style_tfl()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style_tfl.md) | A reusable style (borders + padding + row-height defaults), captured by snapshot at construction. |
| `rtf_col_header` | [`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md), [`col_cell()`](https://ichirio.github.io/rtfreporter/reference/col_cell.md) | Multi-row / spanning column headers. |

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
return plain (unclassed) lists describing a section’s header / footer:
to change a row, make it again.

> **`rtf_document` vs `rtfreport`.** `rtf_document` is the *public*
> object you build and hold via the pipe. It is converted to a separate
> *internal* class, `rtfreport`, only at render time (by
> [`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md));
> see [Internal class design
> (S3)](https://ichirio.github.io/rtfreporter/articles/internal-design.md).
> You do not construct or handle `rtfreport` directly.

[`print()`](https://rdrr.io/r/base/print.html) on an `rtftable` previews
the laid-out cells in the console
([`format()`](https://rdrr.io/r/base/format.html) gives those lines,
[`summary()`](https://rdrr.io/r/base/summary.html) the metadata);
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) draws a
wireframe of a table’s borders or of the document.

## The rendering pipeline

    rtf_document()  |>  rtf_section()  |>  rtf_tables() / rtf_figures()
            │   .rtf_content()                   (any source -> content pages, one block each)
            ▼
       rtf_document$contents                     (rtf_blocks: content, title, footnote, page_break)
            │   rtf_layout()                     (blocks -> printed pages, each its segments)
            ▼   .pipe_doc_to_rtfreport()         (adapter: pipe API -> rtfreport)
       rtfreport
            │   .rtfreport_validate()            (auto-default section, checks)
            ▼
       generate_rtfreport(doc, "out.rtf")        (renderer in generate_rtfreport.R)
            │   .render_page_segments()          (each page: title, content, footnote per segment)
            │   .render_content()                (S3: the rtftable and rtfplot methods)
            ▼
       .rtf  text file

Every page is drawn by the same code, whether it holds one content or
several (`page_break = FALSE`): the segments one after another, each
with its title and footnote, a thin separating paragraph between them.
So a table looks the same on a page of its own and under another table,
and the title / footnote width and format are decided in one place. The
content itself is drawn by the internal generic `.render_content()`; a
new kind of content adds a method there (and comes in through
`.rtf_content()`), the page drawing is not touched. The font and colour
tables at the head of the file are collected from every segment of every
page.

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
- The internal class design and helper catalogue: [Internal class design
  (S3)](https://ichirio.github.io/rtfreporter/articles/internal-design.md).
- Contribution workflow and code style:
  [CONTRIBUTING.md](https://github.com/ichirio/rtfreporter/blob/main/CONTRIBUTING.md).

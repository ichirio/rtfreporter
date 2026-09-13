# Internal class design (S3)

This document describes rtfreporter’s **internal data structures** – the
S3 objects the renderer works with – for people extending the package.
For the public API see [External API
specification](https://ichirio.github.io/rtfreporter/articles/external-api.md).

> Everything is **pure S3**: plain `list`s with a `class` attribute, no
> external OOP framework. Public objects use copy-on-modify semantics,
> so the pipe API returns a new object at each step rather than mutating
> in place.

## Two-layer model

A document is **not** a deep hierarchy. It is a flat sequence of pages
plus a set of section overlays:

    rtf_document (public, pipe-built)
     └─ converted at render time to →  rtfreport (internal)
          ├─ document   : font_table, color_table, default_page, default_format
          ├─ pages[]    : rtf_page    (one content item per page)
          └─ sections[] : rtf_sect    (header/footer assigned to a page range)

- **Pages are flat** at the document level; **1 page = 1 content** (an
  `rtftable` or an `rtfplot`).
- **Sections are an overlay**: each `rtf_sect` assigns a header/footer
  starting at a page and applying until the next section. They do not
  contain content.

### This is the Builder pattern

The two layers are a deliberate use of the **builder pattern**, not a
redundant pair of document classes:

- `rtf_document` is the **builder** – a convenient, incrementally-grown
  object. The pipe verbs
  ([`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md),
  [`rtf_section()`](https://ichirio.github.io/rtfreporter/reference/rtf_section.md),
  …) each return an updated `rtf_document`, accumulating content and
  section definitions in the order the author writes them.
- `rtfreport` is the **product** – the normalised, render-ready form the
  renderer consumes (content paired with its title/footnote into
  `rtf_page`s, section definitions resolved to page ranges).
- `.pipe_doc_to_rtfreport()` is the **build step** that turns one into
  the other, run once at render time inside
  [`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md).

Keeping the two separate means the *authoring* API and the *rendering*
model can evolve independently, and each stays simple: the builder never
has to be render-ready, and the product never has to support incremental
edits.

## The objects

### `rtf_document` (public)

Built by the pipe API
([`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md) +
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
/
[`rtf_figures()`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.md)
/
[`rtf_section()`](https://ichirio.github.io/rtfreporter/reference/rtf_section.md)
/ …). Slots: `document` (font/color/page/format), `contents` (the
per-page content list), `titles`, `footnotes`, and `sections`. Immutable
in use – each pipe verb returns a copy.

### `rtfreport` (internal)

The render-ready form produced from an `rtf_document` by
`.pipe_doc_to_rtfreport()`. It holds the resolved `document` defaults, a
list of `rtf_page` objects, and a list of `rtf_sect` objects.
Construction-time validation lives in `.rtfreport_validate()`.

### `rtf_page` (internal, S3 tagged list)

`structure(list(title, content, footnote), class = "rtf_page")` – one
page:

- `title` — character vector (centred lines above the content);
- `content` — an `rtftable` or `rtfplot` (or `NULL`);
- `footnote` — character vector rendered below the content.

### `rtf_sect` (internal, S3 tagged list)

`structure(list(header, footer, from_page), class = "rtf_sect")`:

- `header` / `footer` — a header/footer description, or `NULL` to
  inherit from the previous section;
- `from_page` — the first page this section applies to.

`.resolve_sections()` maps each `from_page` to a `(from_page, to_page)`
range at render time.

### `rtftable` (public)

The richest object: the table data plus all formatting. Single-DF mode
stores `data`; multi-DF mode stores `data_list`. Plus `col_header` /
`col_header_list`, `col_spec` (per-column: align, bold/italic/underline,
indent, text `color`, …), `border` (an `rtf_table_border`),
`blank_rows`, width settings, row heights, cell padding/valign, and
`cell_styles` (per-cell overrides).
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
attaches page-level `rtf_titles` / `rtf_footnotes` as attributes for
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
to consume. [`print()`](https://rdrr.io/r/base/print.html) renders a
visual preview of the table (headers, cells, rules, alignment);
[`format()`](https://rdrr.io/r/base/format.html) returns those lines and
[`summary()`](https://rdrr.io/r/base/summary.html) the compact metadata
block.

### `rtfplot` (public)

`structure(list(path, width_twips, height_twips, align), class = "rtfplot")`.

### Style and border objects

`rtf_border` (per-side line specs), `rtf_table_border` (zone borders for
a table), and `rtf_table_style` (a reusable defaults bundle) are all
plain S3 lists. `rtf_table_style` uses **snapshot** semantics – a table
captures the style’s state at construction;
[`rtf_table_style_with()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style_with.md)
derives a modified copy without mutating the original.

## Rendering

[`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md)
converts the `rtf_document` to an `rtfreport`, resolves sections,
computes the colour table (from the border colours used), and emits the
RTF: a font table, colour table, page settings, then per section a
`{\header}` / `{\footer}` followed by the page content (tables via the
row renderers, figures via the picture renderer). Page-number tokens
(`{AUTO_PAGE}` / `{AUTO_TOTAL_PAGES}` / `{PAGE}`) are substituted here.
Helper functions (`.resolve_sections()`, `.render_footnote_table()`,
`.compute_content_width()`, …) live in `R/generate_rtfreport.R`.

One rendering step deliberately works on a *derived* geometry rather
than the table’s own:
[`set_decimal_split()`](https://ichirio.github.io/rtfreporter/reference/set_decimal_split.md)
(`R/decimal_split.R`). Because RTF lets each row declare its own cells,
`.decimal_split_plan()` builds an expanded set of `\cellx` positions –
one extra edge inside each split column – that **only the data rows
use**. The whole column-header block keeps rendering over the original
columns, so no header or spanning renderer knows the feature exists, and
the cumulative positions still end at the same right edge, leaving the
table width, the title/footnote band and pagination untouched. The plan
is derived at render time from `tbl$decimal_split` and never written
back to the object; when it is `NULL` every renderer takes its original
path.

## Why S3 here

S3 fits this design because the objects are mostly **data**: they are
built once, passed down a pipe, and rendered. Plain lists are
transparent
([`str()`](https://rdrr.io/pkg/rtables/man/int_methods.html)),
serialisable, and dependency-free. Mutability is not needed – the pipe
API never exposes shared state – so copy-on-modify S3 is both simpler
and safer than a reference system would be.

## S3 conventions & class roles

Knowing **why** a class exists tells you how to work with it. In
rtfreporter an S3 class plays one of three roles:

1.  **Generic dispatch.** A few classes carry
    [`print()`](https://rdrr.io/r/base/print.html) /
    [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods
    (for readable console output and base-graphics layout previews).
    [`paginate()`](https://ichirio.github.io/rtfreporter/reference/paginate.md)
    is also an S3 generic, but it dispatches on the *input* type
    (`data.frame` / `gt_tbl` / `list`), not on a rtfreporter class.
2.  **Type tag for [`inherits()`](https://rdrr.io/r/base/class.html).**
    Most classes exist to be recognised by `inherits(x, "<class>")` in
    ordinary `if` branches – the renderer and the pipe verbs switch on
    object kind this way. There is no method involved.
3.  **Internal structural marker.** `rtf_page` / `rtf_sect` carry a
    class purely for readability and
    [`str()`](https://rdrr.io/pkg/rtables/man/int_methods.html) /
    debugging output; nothing dispatches on them and the renderer
    reaches their fields by name.

| Class | Role |
|----|----|
| `rtf_document` | dispatch (`print`, `plot`) + [`inherits()`](https://rdrr.io/r/base/class.html) |
| `rtftable` | dispatch (`print`, `format`, `summary`, `plot`) + [`inherits()`](https://rdrr.io/r/base/class.html) |
| `rtf_border_side`, `rtf_border`, `rtf_table_border` | dispatch (`print`, `plot`) + [`inherits()`](https://rdrr.io/r/base/class.html) |
| `rtf_table_style`, `rtf_col_cell`, `rtf_col_header` | dispatch (`print`) + [`inherits()`](https://rdrr.io/r/base/class.html) |
| `rtfplot` | dispatch (`print`) + [`inherits()`](https://rdrr.io/r/base/class.html) |
| `rtf_blank_rows_by_change`, `rtf_blank_rows_by_rule` | [`inherits()`](https://rdrr.io/r/base/class.html) (blank-row resolver dispatch) |
| `rtf_toc_heading`, `rtf_toc_entry` | [`inherits()`](https://rdrr.io/r/base/class.html) (TOC normalisation) |
| `rtfreport`, `rtf_auto_section_item` | [`inherits()`](https://rdrr.io/r/base/class.html) (internal) |
| `rtf_page`, `rtf_sect` | internal structural marker (no dispatch) |

### Conventions to follow when extending

- **Construct** with `structure(list(...), class = "<class>")`, inside a
  public constructor or a `.new_*()` internal helper.
- **Test types** with `inherits(x, "<class>")`, not
  `class(x) == "<class>"` (robust when an object carries more than one
  class).
- **Validate** with `stop(msg, call. = FALSE)` (no leading function-name
  noise).
- **Copy-on-modify.** Every modifier – the pipe verbs and the `*_with()`
  derivers
  ([`rtf_table_style_with()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style_with.md))
  – returns a **new** object; nothing is mutated in place.
- **No class hierarchies.** Classes are single, flat tags. The border
  family is *composition*, not inheritance: an `rtf_table_border`
  contains `rtf_border` objects, each of which contains
  `rtf_border_side` objects.
- Adding a class only pays off if something will **dispatch on it or
  [`inherits()`](https://rdrr.io/r/base/class.html)-check it** –
  otherwise a plain list is enough.

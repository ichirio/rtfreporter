# rtfreporter — AI developer manual

**This manual documents the rtfreporter 0.7.56 codebase.**
Check it matches the tree you are working in — `DESCRIPTION`'s `Version:`.
If they differ, trust the tree, not this file.

**Repo:** <https://github.com/ichirio/rtfreporter> ·
**Docs:** <https://ichirio.github.io/rtfreporter/>

> **How to use this file.** Attach it at the start of a chat session and say:
> *"Use this manual when working on the rtfreporter codebase."*
> 日本語で質問しても構いません（本文は英語ですが、回答は質問の言語で返ります）。

> **Scope: working *on* the package** — changing `R/`, adding an adapter,
> writing tests, opening a PR. *Using* rtfreporter to produce reports is the
> companion **AI user manual**
> (`https://ichirio.github.io/rtfreporter/ai/rtfreporter-ai-user-manual.md`).
> Attach **one** of the two, not both: the user manual forbids touching
> internals, this one requires it.
>
> This file serves **both access levels** — outside contributors working from
> a fork, and repository collaborators pushing branches directly. The work is
> the same for both; only §10 differs, and it says which row applies to you.

---

## 0. Invariants — never break these

These are not style preferences. A change that violates one is wrong even if
the tests pass.

1. **S3 throughout.** Every object is a plain S3 list. No R6, no S4, no
   R7/S7. The last R6 path was deleted in v0.0.41; nothing in `R/` mentions
   R6 today. Objects stay `dput()`-able and `saveRDS()`-clean.
2. **No third-party runtime dependency.** `Imports:` holds only packages that
   ship with R (`grDevices`, `grid`, `methods`, `utils`). Every integration —
   gt, gtsummary, rtables/tern, flextable, huxtable, ggplot2 — lives in
   `Suggests:` and is reached through `.need_pkg()`. Never move one to
   `Imports:`.
3. **twips are the only internal length unit** (1 inch = 1440 twips). Font
   sizes are half-points. Convert at the boundary, never in the middle.
4. **ASCII-only in R code.** Use `\uXXXX` escapes for anything else.
   Non-ASCII is fine in comments and roxygen, not in code.
5. **`NAMESPACE` is hand-managed.** `devtools::document()` regenerates
   `man/*.Rd`; it must not rewrite `NAMESPACE`. Edit exports deliberately.
6. **Immutable helpers.** An internal mutator returns a *new* copy; it never
   modifies its argument in place.
7. **`"none"` means omit.** A border side of `"none"` emits no RTF command at
   all — not a zero-width border.
8. **One output style.** The package renders the conventional clinical TFL
   layout and nothing else. A feature that would not appear on a real
   clinical TFL is out of scope; say so rather than adding it.

---

## 1. Orientation

`DESCRIPTION` is at the repository root — **the repo is the package**.

```
R/            52 files, ~22k lines      man/          generated, committed
tests/testthat/  ~110 files             vignettes/    user guides
inst/resources/  RTF command templates  vignettes/articles/  site-only articles
inst/rtf-examples/  committed sample output    data-raw/  generators (never shipped)
pkgdown/assets/  static site files (this manual)
```

Authoritative prose, in order of usefulness:

| Topic | Document |
|---|---|
| Architecture, object model, pipeline | `vignettes/articles/architecture.Rmd` |
| Adding a table-object adapter | `vignettes/articles/extending-adapters.Rmd` |
| Workflow, branching, versioning, releases | `CONTRIBUTING.md` — its *Contributor vs Collaborator* table is the authority on the fork/direct split. Note its line saying branch protection on `main` is off is stale: `main` now requires 7 green checks and one approving review. |
| Orientation + invariants | `AGENTS.md` |
| Why S3 and not R6 | `LEARNING.md` — **partly stale**: its "one R6 class" section describes `rtf_theme`, which no longer exists. The reasoning for choosing S3 is still sound; the inventory is not. |
| Public API surface | `vignettes/articles/external-api.Rmd` |

If a document and the code disagree, fix whichever is wrong — do not paper
over it.

---

## 2. The object model

```
rtf_document          public S3, built by the pipe API
  ├── contents[]      one entry per page: rtftable | rtfplot
  ├── titles[]        per-page title block (character vector)
  ├── footnotes[]     per-page footnote block
  └── sections[]      header/footer per page-range (via from_page)

rtftable              one table (single- or multi-data.frame)
  ├── data / data_list
  ├── col_header(_list)   character labels row | stacked spanning rows
  ├── col_spec            per-column align / bold / italic / indent / border
  ├── cell_styles         per-cell overrides, one element per body row
  ├── border              rtf_table_border (zones)
  └── width / height / padding / valign fields

rtfplot               one embedded PNG/JPEG figure
```

`rtf_document` is the public surface. Internally it becomes an `rtfreport`
via `.pipe_doc_to_rtfreport()`, is checked by `.rtfreport_validate()`, and is
handed to the renderer.

---

## 3. The rendering pipeline

```
rtf_document() |> rtf_section() |> rtf_tables() / rtf_figures()
        │
        ▼  .pipe_doc_to_rtfreport()      pipe API -> internal rtfreport
   rtfreport
        │  .rtfreport_validate()         auto-default section, checks
        ▼
   generate_rtfreport(doc, "out.rtf")    R/generate_rtfreport.R
        │  .render_rtftable() / .render_rtfplot()
        ▼
   .rtf text
```

The renderer is **pure string assembly**. Command templates live in
`inst/resources/rtf_commands.R`; size and spacing defaults in
`inst/resources/rtfreporter_defaults.R`, so they can be tuned without touching
code. Nothing in the renderer should compute layout policy — that belongs
upstream, in the object.

---

## 4. `as_rtftables()` — the hub, and the kwargs contract

`R/as_rtftables.R` is the single entry point from an external *table object*
to a list of `rtftable` pages. Every source is reduced to one intermediate,
the **kwargs list**, and the rest of the pipeline consumes it identically:

```r
list(
  data            = <data.frame>,   # REQUIRED: the rendered body (character cells are fine)
  col_header      = <chr vector | stacked spanning rows>,
  col_spec        = list(list(col = 1L, align = "left"), ...),
  cell_styles     = <list, one element per data row, or NULL>,
  titles_block    = <chr vector | NULL>,   # becomes the page title
  footnotes_block = <chr vector | NULL>    # becomes the page footnote
)
```

Everything else — sorting, pagination, slicing `cell_styles` per page,
replicating shared metadata onto every page, attaching the title/footnote
blocks as the `rtf_titles` / `rtf_footnotes` attributes `rtf_tables()` reads —
is `as_rtftables()`'s job. **An adapter never builds an `rtftable`.**

Order inside `as_rtftables()`, which matters when adding a hook:

```
extract (adapter)
  -> stub fold (stub_vars / stub = stub_spec(), listing = hook)
  -> sort (sort_by / sort_desc)        so grouping sees ordered rows
  -> paginate (.paginate_df())         drop_cols are still PRESENT here
  -> .apply_col_drop()                 removes them, reindexes every
                                       position-indexed argument
  -> rtftable() per page
```

That ordering is why a column can decide the page breaks and never be
printed. Any new position-indexed argument must be added to the reindexers in
`R/col_drop.R`, or it will silently point at the wrong column after a drop.

---

## 5. Adding a table-object adapter

Six steps; the full version is `vignettes/articles/extending-adapters.Rmd`.

1. **Detector** — a cheap, side-effect-free predicate in
   `R/<pkg>_adapter.R`: `.is_mypkg_tbl <- function(x) inherits(x, "mypkg_table")`
   (S4: `isS4(x) && methods::is(x, "MyClass")`).
2. **Token resolution** — `read =` lets users opt in/out of metadata. Mirror
   the gt / rtables resolvers: `.MYPKG_TOKENS_ALL <- c("col_header", "alignment", ...)`.
3. **Extractor** — return the kwargs list of §4. Guard the optional package
   with `.need_pkg()`, not a bare `requireNamespace()`.
4. **Wire it in** — three edits in `R/as_rtftables.R`: the list-input guard
   must not mistake your object for a list of items; the dispatch chain; and
   optionally the `as_rtftable()` singular wrapper.
5. **Declare** — add the package to `Suggests:` in `DESCRIPTION`, with a
   minimum version if needed. Never `Imports:`.
6. **Tests and docs** — `tests/testthat/test-mypkg-adapter.R` with every test
   wrapped in `skip_if_not_installed("mypkg")`; document the new `read`
   tokens in `?as_rtftables`; `NEWS.md` entry; `devtools::document()`.

Existing adapters to copy from: `gt_adapter.R` (gt / gtsummary / tfrmt),
`rtables_adapter.R` (rtables / tern), `flextable_adapter.R`,
`huxtable_adapter.R`, `rlistings_adapter.R`.

---

## 6. The post-hoc verb pattern

`R/style_verbs.R` holds `style_header()` / `style_cols()` / `style_body()` /
`style_zone()` / `add_header_row()` / `set_col_header()`. Every one of them:

* is an **S3 generic with an `rtftable` method and a `list` method** — one
  call styles a single table or every page of an `as_rtftables()` result;
* is applied **eagerly** — there is no lazy layer machinery;
* merges **last-writer-wins, per side / per field** (the renderer's border
  rule, generalised).

The `list` method maps over pages via `.style_map_pages()`, which insists
every element is an `rtftable` and names the offending index if not. Add a new
verb by following that shape, not by inventing a second dispatch style.

Coordinate systems differ and the difference is deliberate:

| Entry point | Positions refer to |
|---|---|
| `rtftable(col_header = )` / `as_rtftables(col_header = )` | the **source body**, before `drop_cols` / `stub_vars` |
| `set_col_header()`, `style_*()` | the **final printed table** |

`rtf_columns()` lists the final names; `header_map()` shows where each header
cell actually landed. Use them in tests instead of reaching into `$col_header`.

---

## 7. Code conventions

**Naming.** Exported functions are `snake_case` and mostly `rtf_*` /
`as_rtf*` / `style_*` / `fmt_*`. Internals are `.dot_prefixed` and are never
exported. Package-local constants are `.SCREAMING_SNAKE`.

**Errors.** One shape throughout — backtick the argument, say what was got,
say what to do, and suppress the call:

```r
stop(sprintf(
  "`%s(rows = )` positions must be in 1..%d.", verb, total), call. = FALSE)
```

A long message may continue onto further lines to name the fix. Messages are
part of the API: several tests match on them, so changing one means updating
its test deliberately, not incidentally.

**Optional packages.** `.need_pkg("gt")` — missing is an error, too old is a
warning and the call proceeds. Minimum versions come from rtfreporter's own
`DESCRIPTION` at run time, so a version literal is never repeated in R code.

**Defaults.** Configurable defaults are ordinary options under
`rtfreporter.*`, seeded by `.onLoad()` without clobbering a site's existing
values. The single source of truth is `.rtfreporter_factory_defaults()` in
`R/defaults.R`. Resolution order: explicit argument → option → factory
default. Do not add a second defaults mechanism.

**Deprecation.** `.deprecated_exports` in `R/rtf_border.R` lists what is on
the way out; `.deprecate_once()` warns once per session. Deprecate in a minor,
remove only in a major.

---

## 8. Tests

```r
devtools::test()          # all must pass before merge
devtools::test_active_file()
lintr::lint_package()     # must be clean; config in .lintr (lints R/ only)
```

* testthat **edition 3**, one `test-<topic>.R` per source area.
* Fixtures are local `.` -prefixed helpers at the top of the file
  (`.sv_df()`, `.sv_tbl()`), not shared globals.
* For rendering behaviour, **render and match the RTF text** rather than
  inspecting the object — that is what `.sv_rtf()` / `.sv_row_counts()` do.
  A test that only checks the object can pass while the output is wrong.
* Guard every optional-package test with `skip_if_not_installed()`.
* When you fix a bug, add the failing case **and** the neighbouring cases
  that already worked, so the next change cannot re-break the boundary.

---

## 9. Documentation

* **roxygen** above the function; `devtools::document()` regenerates
  `man/*.Rd` and **both are committed in the same PR**.
* **`NAMESPACE` is hand-edited.** Never let a tool rewrite it.
* `_pkgdown.yml` has a `reference:` section listing every topic — a new
  exported function must be added to it or the pkgdown build fails on an
  unlisted topic.
* Articles live in `vignettes/articles/` (site only); user-facing vignettes in
  `vignettes/`. Several articles have a `-ja` Japanese twin — update both.
* `NEWS.md` every PR; `CHANGELOG.md` additionally for a minor/major release.
* Regenerate committed examples from the working tree, never from an
  installed build: `Rscript data-raw/gen_tlg_catalog_rtf.R` from the package
  root (the generators `source("data-raw/_load.R")`, which uses
  `pkgload::load_all()`). An installed build lagging the tree is how six
  examples once shipped with a literal `{orientation_cmd}` in the preamble.

---

## 10. Workflow

```
issue ─▶ agree approach ─▶ topic branch ─▶ PR ─▶ green CI ─▶ review ─▶ merge
```

Everyone follows the same lifecycle, the same branch names and the same
version rule. **One thing differs — where the branch lives — and it follows
from write access.** Check it before generating any `git push` or
`gh pr create`:

```bash
gh api repos/ichirio/rtfreporter --jq .permissions.push   # true = Collaborator
```

| | **Contributor** (`push: false`) | **Collaborator** (`push: true`) |
|---|---|---|
| Push the branch to | **your fork** (`git remote add upstream` the canonical repo, keep your `main` pristine and fast-forward it from upstream) | `ichirio/rtfreporter` directly |
| Open the PR | `gh pr create --repo ichirio/rtfreporter --base main --head <you>:<branch>` | `gh pr create --base main` |
| Apply labels | a Collaborator does it for you | yourself |
| Releases — minor/major bump, tag, GitHub Release | not available | Collaborators only |

**Stop at "the PR is open."** Reviewing, approving and merging are human
decisions: `main` requires an approving review, and GitHub does not accept a
self-approval, so an agent cannot complete a merge legitimately on its own.
Report that the PR is ready and green, and wait to be asked — never merge on
your own initiative, and never reach for an administrative override to get
past a protection rule unless the repository owner has told you to in this
session.

* **Every change starts as an issue.** `exec:agent` on an issue means an
  agent may implement it; `exec:human` / `exec:hold` / no label mean it may
  not — report the issue and stop.
* **Branch name:** `<type>/<issue>-<slug>` — `feat` `fix` `docs` `chore`
  `refactor` `test` `perf` `ci`. Bare issue number, no `#`.
  Example: `fix/453-named-label-row-guard`.
* **PR body** carries `Closes #N` (or `Refs #N` for one cut of an umbrella
  issue, which stays open as a tracker with a checklist).
* **Version:** each PR raises `DESCRIPTION` `Version:` by **exactly one
  PATCH**. A MINOR or MAJOR bump fails `version-guard` unless the PR carries
  the `release` label. Digits and dots only — `0.1.0-alpha` is a malformed R
  version.
* **CI (all must be green):** `R-CMD-check` (matrix, 0 errors / 0 warnings),
  `test-coverage`, `pkgdown`, `version-guard`, `lint`.
* **Before pushing:** `devtools::document()`, `devtools::test()`,
  `lintr::lint_package()`, `devtools::check()`.

---

## 11. Source-file map

| Area | Files |
|---|---|
| Public pipe API | `pipe-composition.R` |
| Objects | `rtftable.R`, `rtfplot.R`, `rtfreport.R`, `rtf_page.R` |
| Renderer | `generate_rtfreport.R` (+ `inst/resources/rtf_commands.R`) |
| The hub | `as_rtftables.R`, `as_rtftable.R` |
| Adapters | `gt_adapter.R`, `rtables_adapter.R`, `flextable_adapter.R`, `huxtable_adapter.R`, `rlistings_adapter.R` |
| Column headers | `col_header.R`, `col_header_values.R`, `col_resolve.R`, `header_source.R` |
| Stub / hierarchy | `stub.R`, `stub_spec.R`, `stub_vars.R` |
| Listings | `listing.R`, `listing_fit.R` |
| Pagination | `paginate.R`, `paginate_cols.R`, `col_drop.R`, `sort_body.R` |
| Blank rows | `blank_rows.R`, `set_blank_rows.R` |
| Styling & borders | `style_verbs.R`, `element_style.R`, `rtf_border.R`, `rtf_table_style.R` |
| Widths & text metrics | `block_width.R`, `text_width.R`, `decimal_split.R` |
| Formatting | `format_count_pct.R`, `num_format.R`, `cell_format.R`, `catx.R`, `collapse_repeats.R` |
| Assembly | `assemble_rtf.R`, `assemble_spec.R`, `rtf_replace_text.R` |
| Infrastructure | `defaults.R`, `zzz.R`, `need_pkg.R`, `dots_check.R`, `font_table.R` |

---

## 12. Traps that have actually bitten

| Trap | What to do |
|---|---|
| A new position-indexed argument silently misaligns after `drop_cols` | add it to the reindexers in `col_drop.R` |
| A header written for the whole table, applied after `paginate_cols()` | set it **before** the column split, or pass `paginate_cols(col_header = )` |
| A guard that checks length but ignores `names()` | see #453 — decide on the shape the caller actually wrote |
| Committed examples regenerated from an installed build | always run the `data-raw/` generators from the package root |
| A new export missing from `_pkgdown.yml` `reference:` | the pkgdown workflow fails on an unlisted topic |
| Moving an optional package to `Imports:` "just to simplify" | it breaks invariant 2; use `.need_pkg()` |
| Fixing a doc without its `-ja` twin | the Japanese article silently goes stale |
| Trusting a prose document over the code | `LEARNING.md` still documents `rtf_theme`; `architecture.Rmd` still says `Imports:` is only `methods`. Check `R/` and `DESCRIPTION`. |
| Non-ASCII slipping into R code | `R CMD check` flags it; use `\uXXXX` |

**When something is not covered here:** read the article named in §1 rather
than inferring from the code alone — several decisions are recorded only in
prose, with the reasoning that makes them worth keeping.

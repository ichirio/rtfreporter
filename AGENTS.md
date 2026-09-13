# Agent / contributor instructions for rtfreporter

This repository **is the R package** (`DESCRIPTION` is at the root).
This file is a short orientation for agents and new contributors; the
detailed, authoritative docs are linked below.

## Read these first

| Topic | Document |
|----|----|
| Architecture & internals (the big picture, source-file map) | `vignettes/articles/architecture.Rmd` |
| Adding a new table-object source (gt / rtables / …) | `vignettes/articles/extending-adapters.Rmd` |
| Contribution workflow, code style, **branching, versioning & releases** | `CONTRIBUTING.md` |
| In-depth design specs (English, with `-ja` toggle) | `vignettes/articles/internal-design.Rmd`, `external-api.Rmd` |

There is no longer a separate `specs/` folder — those documents above
are the single source of truth. If a doc and the code disagree, fix
whichever is wrong (open an issue if the design itself is in question).

## Invariants — do not break

- **S3 throughout.** Every object is a plain S3 list; no R6 (removed in
  v0.0.19).
- **Zero hard runtime dependencies.** `Imports:` is `methods` only.
  Every optional integration (gt, gtsummary, rtables/tern, ggplot2, …)
  lives in `Suggests:` and is reached through
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html).
- **twips only** (1 inch = 1440 twips) as the internal length unit.
- **ASCII-only R code** (use `\uXXXX` escapes); non-ASCII is fine in
  comments and roxygen, not in code.
- **`NAMESPACE` is hand-managed.** Regenerate `man/*.Rd` with
  `devtools::document()` and commit them.

## Workflow

1.  Branch off `main` (see *Branching & collaboration* in
    `CONTRIBUTING.md`).
2.  Change `R/`, `tests/testthat/`, and the roxygen docs together.
3.  Run `devtools::document()`, `devtools::test()`, `devtools::check()`
    (target: 0 errors / 0 warnings).
4.  Update `NEWS.md` (and `CHANGELOG.md` for a minor/major release).
5.  Open a pull request against `main`.

## Layout (standard R-package set)

    R/            man/            tests/testthat/    vignettes/ (+ articles/)
    inst/         data-raw/       pkgdown/           .github/
    DESCRIPTION   NAMESPACE       NEWS.md  CHANGELOG.md  README.md  LICENSE

The authoritative per-file map lives in the architecture article above.
`data-raw/` holds developer-only scripts (sample-data prep + demo
generators); it is `.Rbuildignore`d and never shipped.

## Tests

``` r

devtools::test()    # all tests must pass before merging to main
```

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
| In-depth design specs (English, with `-ja` toggle) | `vignettes/articles/internal-design.Rmd`, `architecture.Rmd` |

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
- **The three packages.** Dependencies point one way: rtfreporter \<-
  tflspec \<- tflplanner. rtfreporter knows nothing of the other two.
- **Fictional data only.** No company, study or patient data in code,
  tests, fixtures, examples, issues or pull requests, and none sent to
  an AI service.

## Tests and code

- **No withr** (it is not a dependency): use
  [`tempfile()`](https://rdrr.io/r/base/tempfile.html),
  [`dir.create()`](https://rdrr.io/r/base/files2.html) and
  `on.exit(unlink(...))`.
- While you work, run the tests of the area you changed and `lintr` on
  the changed files; before you push, the four pre-push checks (*Pull
  requests* in `CONTRIBUTING.md`).
- Run tests where they are; never copy them into a temporary folder
  (`testthat` would read that folder’s `setup*.R`).
- Point `R_USER_CONFIG_DIR`, `R_USER_DATA_DIR` and `R_USER_CACHE_DIR` at
  a work folder, never at the user’s own.
- Run the tests in one R session too
  ([`testthat::test_dir()`](https://testthat.r-lib.org/reference/test_dir.html)
  on the installed package), not only one file at a time: per-file runs
  miss interference between tests.
- Split a source file before it passes 2,000 lines.

## Workflow

1.  Branch off `main` (see *Branching & collaboration* in
    `CONTRIBUTING.md`). If you work for a contributor, you do the git
    work too: *Git work for a contributor* in
    `inst/ai/rtfreporter-ai-dev-manual.md` (updating the fork, rebasing
    a topic branch, what to do when CI or a review asks).

2.  Change `R/`, `tests/testthat/`, and the roxygen docs together.

3.  Run `devtools::document()`, then the four pre-push checks listed in
    *Pull requests* in `CONTRIBUTING.md` (set-up: *Development setup*
    there).

4.  **Record the change in the PR description.** A PR never edits
    `NEWS.md`, `CHANGELOG.md` or `Version:`, and there is no separate
    memo file. Fill the template’s *User-visible change (for NEWS)*
    field without being asked: one or two bullets in NEWS style (what a
    user can now do, or what is fixed, named by its function or
    argument), or “none” for an internal change; then “Breaking: no”, or
    “Breaking: yes” with the migration. The maintainers write `NEWS.md`
    from the merged PRs at release. For example:

    ``` markdown
    ## User-visible change (for NEWS)

    - `rtfplot()` and `rtf_figures()` take `render_format = "emf"`: a plot object is
      drawn as EMF with devEMF (in Suggests), at `render_width` x `render_height`
      inches.

    Breaking: no
    ```

5.  Open a pull request against `main`.

Every behaviour change needs a requirement in
`inst/validation/requirements.csv` and a test named with its FT id (or a
`traceability_matrix.csv` row for an existing test), in the same PR,
with `validation/trace.R` and the validation tests passing and a
`deviations.csv` row where a test is not possible; see *Quality
assurance* in `CONTRIBUTING.md` for the full rule.

**Versioning.** Standard R scheme: a release is `X.Y.Z`, development is
`X.Y.Z.9000`, and the three-component part names the last release. The
maintainers set the version when they tag or release; a PR leaves
`Version:` alone, and `version-guard` accepts a PR with no version
change. Touching `X`, `Y` or `Z` is a release action and fails
`version-guard` without the `release` label. See *Versioning & releases*
in `CONTRIBUTING.md`.

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

The full list of pre-push checks is in *Pull requests* in
`CONTRIBUTING.md`.

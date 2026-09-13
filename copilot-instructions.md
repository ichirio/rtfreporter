# Copilot instructions for rtfreporter

This repository **is the R package** `rtfreporter` (`DESCRIPTION` is at
the repository root — there is no nested package directory and no other
language package).

Please follow the project’s standard contributor guidance rather than
duplicating it here:

- **Orientation & invariants:**
  [AGENTS.md](https://ichirio.github.io/AGENTS.md)
- **Workflow, code style, branching, versioning & releases:**
  [CONTRIBUTING.md](https://ichirio.github.io/CONTRIBUTING.md)
- **Architecture & how to extend it:**
  `vignettes/articles/architecture.Rmd`,
  `vignettes/articles/extending-adapters.Rmd`

Key invariants: S3 throughout (no R6); zero hard runtime dependencies
(`Imports: methods` only, optional packages in `Suggests` guarded by
[`requireNamespace()`](https://rdrr.io/r/base/ns-load.html)); twips as
the only internal length unit; ASCII-only R code; `NAMESPACE` is
hand-managed (regenerate `man/*.Rd` with `devtools::document()`).

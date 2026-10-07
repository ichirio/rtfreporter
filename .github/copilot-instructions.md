# Copilot instructions for rtfreporter

This repository **is the R package** `rtfreporter` (`DESCRIPTION` is at the
repository root — there is no nested package directory and no other
language package).

The reference for working on the code is the **AI developer manual**,
`inst/ai/rtfreporter-ai-dev-manual.md` (from R:
`rtfreporter::rtfreporter_ai_manual("dev")`).  Follow it rather than
anything duplicated here; where this file and the manual disagree, the
manual wins.  Its §14 summarises the repository rules (Code of Conduct,
PR / issue templates, the four pre-push checks, versioning + `NEWS.md`,
`cran-comments.md`).

- **Orientation & invariants:** [AGENTS.md](../AGENTS.md)
- **Workflow, code style, branching, versioning & releases:**
  [CONTRIBUTING.md](../CONTRIBUTING.md)
- **Architecture & how to extend it:**
  `vignettes/articles/architecture.Rmd`,
  `vignettes/articles/extending-adapters.Rmd`

Key invariants: S3 throughout (no R6); no third-party runtime dependency
(`Imports:` holds only packages that ship with R; optional packages live in
`Suggests` and are reached through `.need_pkg()`); twips as the only
internal length unit; ASCII-only R code; `NAMESPACE` is hand-managed
(regenerate `man/*.Rd` with `devtools::document()`).

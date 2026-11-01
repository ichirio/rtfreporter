# Notes for Claude Code (in addition to AGENTS.md)

@AGENTS.md

## The three packages

- Dependency direction: rtfreporter \<- tflspec \<- tflplanner.
  rtfreporter knows nothing of the other two; tflspec’s SPEC mirrors
  rtfreporter’s API, so a new feature starts here and the others follow.

## Pull requests

- Do not change `Version`, `NEWS.md` or the AI manuals’ version line in
  a PR (this overrides step 4 above). Put the NEWS point in the PR
  description; the version is bumped only for a tagged snapshot or a
  CRAN submission.
- Branch from `main`; do not stack PRs. One open PR at a time per group
  of files.
- When a PR is ready, add the `automerge` label. If it must wait for
  another PR, write one line in its description:
  `merge-after: tflspec#123`.
- Before a PR: the tests of what changed, and lint on the changed files.
  The full suite and `R CMD check --as-cran` run in CI and before a
  release.

## Tests and code

- Do not use withr (it is not a dependency): use
  [`tempfile()`](https://rdrr.io/r/base/tempfile.html),
  [`dir.create()`](https://rdrr.io/r/base/files2.html) and
  `on.exit(unlink(...))`.
- Never copy tests into `$TEMP`; run them where they are.
- Point `R_USER_CONFIG_DIR`, `R_USER_DATA_DIR` and `R_USER_CACHE_DIR` at
  a work folder, never at the user’s own.
- Run the tests in one R session too
  ([`testthat::test_dir()`](https://testthat.r-lib.org/reference/test_dir.html)
  on the installed package), not only one file at a time.
- R 4.6.1 (`C:/Program Files/R/R-4.6.1/bin/Rscript.exe` on Windows).
- Split a file before it passes 2,000 lines.

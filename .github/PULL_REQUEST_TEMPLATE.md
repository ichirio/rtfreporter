<!--
Thanks for contributing to rtfreporter!
Please read CONTRIBUTING.md first, and fill in the sections below.
Keep the PR focused on one logical change.
-->

## Summary

<!-- What does this PR do, and why?  One or two sentences. -->

## Related issue

<!-- Link the issue this addresses, e.g. "Closes #12".  Open an issue first
     for anything non-trivial so the approach can be agreed before coding. -->
Closes #

## Type of change

- [ ] Bug fix (no API change)
- [ ] New feature / enhancement (backward compatible)
- [ ] Breaking change (describe the migration below)
- [ ] Documentation / tests / internal only

## Requirements, tests and regression

<!-- inst/validation/: the FRs added or changed, and the tests (FT) that
     check them; for a change to the RTF written, the reference outputs
     compared byte for byte with main and the ones changed on purpose. -->

- FRs added / changed:
- Tests (FT ids) added / changed:
- RTF outputs changed on purpose (others identical):

## Checklist

- [ ] Branched off the latest `main`; the PR targets `main`.
- [ ] Added or updated **tests** under `tests/testthat/` for the change.
- [ ] Updated **roxygen** docs and ran `devtools::document()` (committed `man/*.Rd`).
- [ ] `devtools::test()` passes locally.
- [ ] `devtools::check()` is clean (0 errors / 0 warnings).
- [ ] Updated `NEWS.md` (and `CHANGELOG.md` for a minor/major change).
- [ ] R code is ASCII-only; no new hard dependency (optional packages stay in `Suggests`).

## Notes for reviewers

<!-- Anything that needs special attention: design trade-offs, breaking
     changes + migration, screenshots of rendered RTF, etc. -->

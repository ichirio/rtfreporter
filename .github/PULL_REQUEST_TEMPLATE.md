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

## User-visible change (for NEWS)

<!-- This is the record of the change: a PR never edits NEWS.md, CHANGELOG.md
     or Version, and the maintainers write NEWS from the merged PRs at release.
     One or two bullets in NEWS style, or "none" for an internal change. -->

-

Breaking: no  <!-- or "yes", and the migration: what old code must change to -->

## Requirements, tests and regression

<!-- inst/validation/: the FRs added or changed, and the tests (FT) that
     check them; for a change to the RTF written, the reference outputs
     compared byte for byte with main and the ones changed on purpose. -->

- FRs added / changed:
- Tests (FT ids) added / changed:
- RTF outputs changed on purpose (others identical; each changed reference
  read again against its code and its row in
  `inst/validation/reference_review.csv` updated):

## Checklist

- [ ] Branched off the latest `main`; the PR targets `main`.
- [ ] Added or updated **tests** under `tests/testthat/` for the change.
- [ ] For a behaviour change: FRs in `inst/validation/requirements.csv`, a
      test per leaf FR (named with its FT id, or a row in
      `inst/validation/traceability_matrix.csv`), a `deviations.csv` row where no test
      is possible; `validation/trace.R` and the `test-validation*.R` tests pass.
- [ ] Each new mapping checked: I read the FR and the test, and its expectations
      assert what the FR states; a row in `inst/validation/traceability_review.csv`
      quotes them as the evidence.
- [ ] Updated **roxygen** docs and ran `devtools::document()` (committed `man/*.Rd`).
- [ ] The four pre-push checks pass (see *Pull requests* in
      `CONTRIBUTING.md`): `R CMD check --as-cran` on the built tarball,
      `NOT_CRAN=true devtools::test()`, `pkgdown::check_pkgdown()`,
      `lintr::lint_package()`.
- [ ] I did not edit `NEWS.md`, `CHANGELOG.md` or `Version:`; the *User-visible
      change* field above is filled.
- [ ] R code is ASCII-only; no new hard dependency (optional packages stay in `Suggests`).

## Notes for reviewers

<!-- Anything that needs special attention: design trade-offs, breaking
     changes + migration, screenshots of rendered RTF, etc. -->

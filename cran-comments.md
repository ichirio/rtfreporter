## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.
* The maintainer is the person named as `cre` in DESCRIPTION.

## Test environments

* local: Windows 11, R 4.4.2 and R 4.6.1
* GitHub Actions: ubuntu-latest (devel, release, oldrel-1, 4.2),
  macos-latest (release), windows-latest (release)
* win-builder: R-devel, R-release, R-oldrelease
* R-hub: linux, macos-arm64, windows, and one sanitizer run

(Results: to be filled from the release test run before submission.)

## Notes for the reviewers

* The examples write only to `tempdir()`; the two that open a plot window
  run only in an interactive session.  Examples that use a suggested
  package (gt, gtsummary) run only when it is installed.
* Tests and vignettes that use a suggested package skip without it.  No
  test reaches the network or an external program.
* The screenshots of the example RTF output are not part of the package
  (they are on the package's website); the example `.rtf` files are.

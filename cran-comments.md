## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Test environments

* local: Windows 11, R 4.4.2
* GitHub Actions: ubuntu-latest (devel, release, oldrel-1), macos-latest
  (release), windows-latest (release)
* win-builder: R-devel (to be run before submission)

## Notes for the reviewers

* The examples write only to `tempdir()`; the two that open a plot window
  run only in an interactive session.  Examples that use a suggested
  package (gt, gtsummary) run only when it is installed.
* Tests and vignettes that use a suggested package skip without it.  No
  test reaches the network or an external program.
* The screenshots of the example RTF output are not part of the package
  (they are on the package's website); the example `.rtf` files are.

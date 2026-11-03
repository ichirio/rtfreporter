## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

(`R CMD check --as-cran`, with the PDF and HTML manuals, the remote
incoming checks and every suggested package installed.  Run on a version
number that still carried a development part, it also showed "Version
contains large components"; that note is gone with the release version.)

## Test environments

* local: Windows 11, R 4.6.1
* GitHub Actions: ubuntu-latest (R devel, release, oldrel-1 and 4.2),
  macos-latest (release), windows-latest (release).  On R 4.2 a suggested
  package that cannot be installed there is left out and the check lets a
  suggested package be missing.
* win-builder: R-devel and R-release (to be run before submission)

## Notes for the reviewers

* The examples write only to `tempdir()`; the two that open a plot window
  run only in an interactive session.  Examples that use a suggested
  package (gt, gtsummary, cards, ggplot2) run only when it is installed.
* Suggested packages are used only behind `requireNamespace()` or
  `skip_if_not_installed()`, in the examples, tests and vignettes alike.
  That includes ragg (a plot is drawn with it when installed, else with
  `grDevices::png()`) and devEMF (only a test uses it, to make an EMF file
  to embed; that test is skipped without it).
* No test reaches the network.  Two tests call a program outside R and run
  only where it is there: `git`, in a source checkout (skipped in the
  installed package's tests), and LibreOffice (skipped on CRAN).
* The screenshots of the example RTF output are not part of the package
  (they are on the package's website); the example `.rtf` files are.

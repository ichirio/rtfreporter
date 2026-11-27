## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.  The one NOTE is from "checking CRAN
  incoming feasibility":
  * "New submission".
  * On win-builder, "Possibly misspelled words in DESCRIPTION: ARD, RTF,
    TFLs".  These are abbreviations, spelled correctly, and each is
    spelled out in the Description where it first appears there: "Rich
    Text Format (RTF)", "Tables, Listings and Figures (TFLs)" and
    "analysis results dataset (ARD, ...)".  RTF is also in the Title.
* The installed size is about 7 MB (6.9 MB on Windows), mainly
  validation documents for users' own GxP checks (inst/validation, about
  2 MB), the R code, the help pages and the built vignettes; reference RTF
  outputs used by the tests and vignettes are under 1 MB.

There are no reverse dependencies (this is the package's first release).

## Test environments

* local: Windows 11 x64, R 4.6.1 (ucrt)
* GitHub Actions: ubuntu-latest (R devel, release, oldrel-1),
  macos-latest (release), windows-latest (release), and ubuntu-24.04
  with R 4.3 and every dependency at the minimum version DESCRIPTION
  names
* win-builder: R-devel, R-release, R-oldrelease

The package has no compiled code, so it was not run on R-hub.

## Results

* local, Windows 11, R 4.6.1, `R CMD check --as-cran` of the tarball
  built from the tag v0.9.0, with the PDF and HTML manuals:
  0 errors | 0 warnings | 1 note ("New submission")
* local, Windows 11, R 4.6.1, every suggested package installed, the
  tests of the tag v0.9.0: FAIL 0 | WARN 2 | SKIP 8 | PASS 26416 (both
  warnings are "package 'cards' / 'magrittr' was built under R version
  4.6.2" from the local library, not from rtfreporter)
* With the CRAN settings (`NOT_CRAN` unset), the same tests: FAIL 0 |
  WARN 0 | SKIP 1616 | PASS 6010.  The skipped ones are the slow tests
  (more than about 0.15 s each), the validation families and the tests
  of suggested packages; see the notes below.
* GitHub Actions, R-CMD-check on the tag v0.9.0: all six jobs passed
  (0 errors, 0 warnings)
* win-builder R-devel (2026-11-17 r90926 ucrt): 0 errors | 0 warnings |
  1 note ("New submission", and the abbreviations above)
* win-builder R-release (R 4.6.2): 0 errors | 0 warnings | 1 note
  ("New submission", and the abbreviations above)
* win-builder R-oldrelease (R 4.5.3): 0 errors | 0 warnings | 1 note
  ("New submission", and the abbreviations above)

## Notes for the reviewers

* The examples write only to `tempdir()`; the two that open a plot window
  run only in an interactive session.  Examples that use a suggested
  package (cards, gt, gtsummary) run only when it is installed.
* Tests and vignettes that use a suggested package skip without it:
  suggested packages are used only behind `requireNamespace()` or
  `skip_if_not_installed()`.  That includes ragg (a plot is drawn with it
  when installed, else with `grDevices::png()`) and devEMF (used only when
  a plot is asked to be drawn as EMF, `render_format = "emf"`, which
  without it stops with a message saying how to install it; its tests are
  skipped without it).  No test reaches the network.  The few tests that
  call an external program (LibreOffice to render an RTF, git in a source
  checkout) skip on CRAN and wherever the program is absent.
* To keep the check time short, every slow test (more than about 0.15 s
  under the CRAN settings) and the slow validation test families
  (pairwise and decision-table grids, gap and coverage scenarios) skip on
  CRAN with `skip_on_cran()`.  Every test runs on GitHub Actions
  (`NOT_CRAN=true`) for each change and before each release.
* The screenshots of example output are the PNG files in `man/figures`,
  for the README and the package's website; the example `.rtf` files
  are in the package too.
* The installed size may give an "installed package size" NOTE (see
  above).  The sub-directories of 1 MB or more are R (1.6 MB), help
  (1.3 MB) and validation (1.9 MB: the requirements, the traceability
  between them and the tests, and the review records; the tests read
  them).  inst/validation is kept in the package so that a user can
  re-run the validation on their own installation.

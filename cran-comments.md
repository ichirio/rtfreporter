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
* GitHub Actions, R-CMD-check on the tag v0.9.0: all six jobs passed
  (0 errors, 0 warnings)
* win-builder R-devel (2026-11-17 r90926 ucrt): 0 errors | 0 warnings |
  1 note ("New submission", and the abbreviations above)
* win-builder R-release (R 4.6.2): 0 errors | 0 warnings | 1 note
  ("New submission", and the abbreviations above)
* win-builder R-oldrelease (R 4.5.3): 0 errors | 0 warnings | 1 note
  ("New submission", and the abbreviations above)

## Notes for the reviewers

* The examples write files only to `tempdir()`.  Three `plot()` examples
  (of a border, a border line and a table's border zones) draw on the
  current graphics device, in a check the examples' PDF; the two that open
  a preview window run only in an interactive session.  Examples that use a
  suggested package (cards, gt, gtsummary) run only when it is installed.
* Tests and vignettes that use a suggested package skip without it:
  suggested packages are used only behind `requireNamespace()` or
  `skip_if_not_installed()`.  That includes ragg (a plot is drawn with it
  when installed, else with `grDevices::png()`) and devEMF (used only when
  a plot is asked to be drawn as EMF, `render_format = "emf"`, which
  without it stops with a message saying how to install it; its tests are
  skipped without it).  No test reaches the network.  The few tests that
  call an external program (LibreOffice to render an RTF, git in a source
  checkout) skip on CRAN and wherever the program is absent.
* To keep the check time short, the slow validation test families
  (pairwise and decision-table grids, gap and coverage scenarios) skip on
  CRAN with `skip_on_cran()`.  Every test runs on GitHub Actions
  (`NOT_CRAN=true`) for each change and before each release.
* The screenshots of example output are the PNG files in `man/figures`,
  for the README and the package's website; the example `.rtf` files
  are in the package too.
* The installed size is about 8 MB, which may give an "installed package
  size" NOTE.  Most of it is two folders the package is validated with:
  `inst/rtf-examples` (about 5 MB, the reference RTF outputs the tests
  compare against and the user opens as examples) and `inst/validation`
  (about 2 MB, the requirements, the traceability between them and the
  tests, and the review records; the tests read them).  They are kept in
  the package so that a user can re-run the validation on their own
  installation.

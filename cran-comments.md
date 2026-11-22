## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.  The one NOTE is the "New submission" note.

There are no reverse dependencies (this is the package's first release).

## Test environments

* local: Windows 11, R 4.6.1
* GitHub Actions: ubuntu-latest (devel, release, oldrel-1),
  macos-latest (release), windows-latest (release), and ubuntu-24.04
  with R 4.3 and every dependency at the minimum version DESCRIPTION
  names
* win-builder: R-devel, R-release, R-oldrelease
* R-hub: linux, macos-arm64, windows, and one sanitizer run

Results of the release test run:

* local Windows 11, R 4.6.1, every suggested package installed, on the release
  candidate (fd1a4af): tests FAIL 0 | WARN 0 | SKIP 6 | PASS 26383
* GitHub Actions, win-builder, R-hub: (to be filled before submission)

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

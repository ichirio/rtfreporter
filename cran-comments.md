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
  candidate (b782f92): tests FAIL 0 | WARN 0 | SKIP 6 | PASS 26383
* GitHub Actions, win-builder, R-hub: (to be filled before submission)

## Notes for the reviewers

* The examples write only to `tempdir()`; the two that open a plot window
  run only in an interactive session.  Examples that use a suggested
  package (gt, gtsummary) run only when it is installed.
* Tests and vignettes that use a suggested package skip without it.  No
  test reaches the network.  The few tests that call an external program
  (LibreOffice to render an RTF, git in a source checkout) skip on CRAN
  and wherever the program is absent.
* The screenshots of the example RTF output are not part of the package
  (they are on the package's website); the example `.rtf` files are.

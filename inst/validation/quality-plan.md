# rtfreporter: Quality Plan

This plan says how rtfreporter's quality is assured for a release, and what
the Quality Report at that release must show.  It is written once for the
first release made under it and again for each major version.  The way changes are made
-- issues, pull requests, CI, branches, releases -- is in
[CONTRIBUTING.md](https://github.com/ichirio/rtfreporter/blob/main/CONTRIBUTING.md) (*Quality assurance*, *Branching &
collaboration*, *Continuous integration*, *Versioning & releases*) and is not
repeated here; who is responsible for what is in section 7.

## 1. Scope

The package's exported functions: what each does, the arguments and data it
is meant to handle, the misuse it stops, and how the functions act together
where that is a risk.  Not in scope: the packages rtfreporter depends on
(their own quality is theirs), and a user's own use of the package in their
studies (the user's validation, for which `inst/validation/` holds the
user's templates).

## 2. A risk-based approach

Not every combination of every argument can be tested, and not every one
needs to be.  The requirements are written where a wrong result would reach
the user's report, and deepest where it is likeliest or hardest to see:

- every behaviour, argument (and meaningful value) and data shape of every
  exported function;
- every misuse the package is meant to stop, with the error or warning it
  gives -- an unintended use must stop, not produce a wrong file;
- combinations only where they are a risk: a precedence or override, an
  argument that changes another's meaning, a combination that once had a
  bug, and a few scenarios from data to an RTF file compared byte for byte.
  Pairwise combinations are not required.

## 3. Requirements and tests

- **Functional Requirements (FR)** are `inst/validation/requirements.csv`,
  a hierarchy (`parent`): an area, a function, its leaves.  An id is short --
  the area's two letters and a number (`FR-PL101`) -- and never reused.
  A parent is of kind `group`; each leaf is of one of five kinds:
  `behaviour`, `argument`, `data`, `guard`, `interaction` (the last under
  `FR-IX000`, apart from the functions').
- **Functional Tests (FT)** are testthat blocks mapped to the leaves by the
  developer making the change: in `inst/validation/traceability_matrix.csv`, or by a new
  test's own name (`"FT-PL<nnn> [FR-PL157 FR-PL159]: ..."`).  A test that
  merely calls a function is not coverage.  A parent requirement is covered
  when every leaf under it is.
- **The Traceability Matrix (FR -> FT)** (`traceability_matrix.xlsx`) holds the
  mapping and the check of each mapping (`traceability_review.csv`,
  section 3a), no results; the results are the tests' log.

## 3a. Test design, and why it shows the requirements are met

Section 2 says where the tests go, and section 3 says how they are named
and mapped.  This section says why passing them shows the requirements are
met, and where that argument stops.

### How the FRs are derived, and how the list is checked

An FR is written from what the package states about itself: the exported
functions, the help of each (its behaviour, its arguments and meaningful
values, the data it takes) and the guards in the code.  Each function gets
leaves of five kinds: `behaviour`, `argument`, `data`, `guard` and
`interaction` (section 3).

The list is checked for completeness by tests and tables, not by promise:

- `test-validation-coverage.R` fails when an exported function has an
  argument that no leaf of that function names, or when the package reads an
  `rtfreporter.*` option that no leaf names.  Deprecated exports are left
  out.  The known gaps are listed in
  `tests/testthat/validation-coverage-gaps.csv` (empty today); the list may
  only shrink.
- `test-validation-leaves.R` fails when an active leaf has neither a test
  nor a row in `deviations.csv`.
- The help-to-FR audits (`tests/testthat/test-validation-audit-*.R`) go
  through the help of a function claim by claim, and name for each claim the
  FR that states it and the test that checks it.
- `validation/trace.R` writes two tables for the Quality Report.
  `functions-<tag>.csv` has one row per exported function: its leaves by
  kind, how many have a test, its arguments, those a leaf names, those a
  test gives, and those no leaf names.  `candidates-<tag>.csv` lists the
  `test_that()` blocks that call a function in scope but are not mapped to
  any FR.  The tables decide nothing and never count as coverage.  They
  show where to look: an argument no leaf names, or a test nobody mapped.

What these checks do not show: that the help is complete or right, or that
a leaf states a behaviour well.  An FR is only as good as the reading that
wrote it.

### The techniques, and where they are used

- **Equivalence classes and boundary values** are used for arguments and
  data.  A leaf of kind `argument` or `data` stands for a class of values
  that the function treats alike: each meaningful value of an argument, and
  each data shape (no rows, one row, many, `NA`, factor levels, one or many
  groups, long or non-ASCII text).  Values at the edge of a rule get their
  own leaf where the edge matters.
- **Error guessing and regression cases** are used for misuse and for past
  bugs.  A leaf of kind `guard` is a misuse the package stops, with the
  error or warning it gives, and its test checks that message.  A bug that
  was fixed gets a leaf (under `FR-IX003` when it was a combination) and a
  test that names the issue.
- **Interaction leaves** (`FR-IX000`) cover where two settings meet: a
  precedence or override (`FR-IX001`), an argument that changes another's
  meaning (`FR-IX002`), and combinations that once had a bug (`FR-IX003`).
  Only these combinations are tested, not all of them.
- **End-to-end scenarios** (`FR-IX004`, `FR-IX131` to `FR-IX134`) run a
  whole program from data to an RTF file and compare the file, line for
  line, with a reference kept under `tests/testthat/fixtures/validation/`.
  The programs use fictional data and nothing that varies between runs
  (`tests/testthat/helper-validation-scenarios.R`).  A change that alters
  the RTF on purpose regenerates the references and says so in its pull
  request (CONTRIBUTING.md, *Regression*); every other output stays
  identical.  A change that alters a file by accident fails the comparison.
- **Pairwise combinations are not required.**  Section 2 gives the reason:
  the risk is in the few combinations where one setting acts on another, and
  those have their own leaves.  In some areas with many settings that act on
  one table (`plan_style()`, `auto_col_widths()`, the listing builders,
  `fmt_numeric()` and others), a test file also runs a pairwise set next to
  a decision table (`tests/testthat/test-validation-pairwise-*.R`, with
  `helper-allpairs.R`).  That is extra evidence in those areas.  No
  acceptance criterion depends on it.

### What counts as a test of a leaf

A test counts for a leaf when it checks what the leaf states: the value the
function returns, the RTF it writes, or the error or warning it gives for a
misuse.  A block that only calls the function, or only checks that it does
not fail, does not count (section 3).  The developer who makes the change
maps a test to a leaf, in the test's name or in `traceability_matrix.csv`.

What the repository enforces by machine: a leaf has a test or a deviation
(`test-validation-leaves.R`); an FT id is not given twice; each exported
argument is named by a leaf (`test-validation-coverage.R`); and the tests
pass on every CI job before a change is merged (section 7).

Each mapping is checked when it is made.  Whoever maps a test to a leaf
reads the leaf and the test, and records in
`inst/validation/traceability_review.csv` that the test's expectations
assert what the leaf states: all of it (`asserts`), or a part of it
(`part`) when the leaf's tests together assert all of it
(`fr_covered = yes`).  The record quotes the expectation lines as the
evidence.  `test-validation-leaves.R` fails when a mapping has no such row,
when a row is for a mapping that is gone, and when a leaf's tests together
do not assert all of it.  A test that asserts none of a leaf is not mapped
to it.  The Traceability Matrix shows the check in its `checked` and
`evidence` columns.

The whole matrix was checked this way on 2026-11-12, mapping by mapping,
before this rule was written: 1913 active leaves and their 4029 mappings.
173 mappings whose test asserts none of the leaf were taken out.  482
leaves had a part no test asserted; 460 tests were added for them
(`test-tm-gaps-*.R`).  Where the package did not do what a leaf states, the
package was corrected (three defects); where a leaf's statement was out of
date, the statement was corrected (four leaves).

### Functional verification and regression

The requirements are verified functionally by the tests mapped to them:
each asserts, for its leaf, the value returned, the error or warning, or the
RTF written -- the control words and text the leaf's statement implies
(alignment such as `\qc`, borders such as `\clbrdrb`, widths such as
`\cellx`, fonts, page setup, the text).  Comparing an output with a stored
reference RTF is not functional verification: it shows that the output has
not changed, not that it is right.  The reference RTFs are regression
tests, for a new version of this package and for a port of it to another
language, and they are made from outputs that the functional tests have
verified.

### How the expected result is known (the oracle)

A test needs an expected result that does not come from the code under
test.  The tests use these sources:

- **A rule stated in the help**, written out in the test file's header as
  its design, with the expected value worked from it (for example
  `test-validation-pairwise-fmt-numeric.R`).  Literal anchors come from the
  help's own tables, such as how 23.445 rounds to two decimals under the two
  rounding rules.
- **A calculation by hand**: the pages of ten rows worked out by hand
  (`test-validation-paginate.R`), and a column's width worked from the
  glyph widths of the font (`test-validation-pairwise-widths.R`).
- **A direct call as the reference of a plan**: the same table made with
  `as_rtftables()` or `rtftable()` and the arguments given directly, which
  `plan_apply()` must match (`test-validation-pairwise-plan-style.R`).
- **Hand-built input** whose result is known by construction: ARDs built by
  hand in the shape `cards` writes, from fictional counts
  (`test-validation-ar.R`), and a 1 x 1 PNG written by hand for the figure
  scenario.
- **Reference RTF files** for the end-to-end scenarios, as a regression
  oracle only (above).  The package itself writes them, when a scenario is
  made or deliberately changed.  They show that the output has not changed.
  That the stored output was right is a reading: each reference is read
  against the code that made it, control word by control word (page setup,
  fonts, the cells' `\cellx` widths worked from the widths asked, borders,
  alignment, the text and the values the data gives), and the reading is
  recorded in `inst/validation/reference_review.csv` with the file's md5, a
  verdict, what was checked and, for a wrong one, its issue.  What only a
  person can judge -- how Word lays the page out -- is marked
  `pending-human` until a person has looked.  `test-validation-references.R`
  fails when a reference is new or written again and its reading is not
  recorded.  The references show stability; the other sources show
  correctness.

### Why line coverage is an aid only

Line coverage (the test-coverage workflow, and the Quality Report's
attachment) shows which lines no test runs.  It does not show that a test
checks what a line does, and it cannot see a requirement with no code, or a
misuse nothing stops.  So it is read to find code with no test, and never
counts for an FR (section 4).  The repository sets a floor for the project's
coverage (`codecov.yml`) and records no figure in this plan.  The Quality
Report states the figure from the release's run.

### The environment strategy

Section 5 lists the environments.  They are chosen to stand for the range
the package supports:

| Supported | Stands for it | Why that is enough |
|---|---|---|
| The lowest R: `Depends: R (>= 4.2)` in `DESCRIPTION` | Ubuntu, R 4.2 | It is the oldest R the package says it works on, so the use of a newer R feature fails here. |
| The R versions in use now | Ubuntu, R oldrel-1 and release; Windows and macOS, release; the local R 4.4.2 and 4.6.1 | Between them they run the current and the previous release of R. |
| The next R | Ubuntu, R devel | A change in R that will break the package shows here first. |
| Windows, macOS and Linux | Windows, macOS and Ubuntu, each with at least one CI job | Each system is run, with the tests and R CMD check. |
| The minimum versions of the suggested packages: each `(>= x)` in `DESCRIPTION` | Ubuntu, R 4.2, with each suggested package that has a minimum installed at exactly that version (the CI job *dependency minimums*) | The use of a feature newer than a minimum fails here. |
| The latest versions of the suggested packages | Every other CI job and the local runs, which install the newest versions their R allows | A new version of a dependency that breaks the package shows here. |

The package imports only packages that ship with R (`DESCRIPTION`), so
there is no range of imported versions to cover.  The suggested packages
(`cards`, `gt`, `rtables`, `ggplot2`, `devEMF` and others) are used where a
function adapts to them, and by the tests that need them.  Each minimum in
`DESCRIPTION` is the first version that has every function and argument the
package and its tests use; `inst/validation/dependency-minimums.md` gives,
for each package, what needs which version and where that is recorded.  A
suggested package with no minimum is one whose every version that installs
beside the others has what the package uses.  So two points of each range
are tested: the minimums, in the job *dependency minimums*, and the latest
versions, in the other jobs and the local runs.  When a test fails at a
minimum, the minimum is raised.

What is not tested, and the risk accepted:

- **A suggested package absent.**  Only the two R 4.2 jobs run with
  `force-suggests` false, and they leave out `tern` and `devEMF`, which
  cannot be installed on that R.  The other
  jobs have the suggested packages installed.  A test that needs one is
  skipped when it is absent (`skip_if_not_installed()`).  The risk is a
  failure on a system without one that no job shows.
- **Versions between the minimum and the latest** of a suggested package,
  and a mix of them other than the two tested.  `tern`'s minimum is not
  tested either: the job *dependency minimums* runs on R 4.2, where `tern`
  cannot be installed.
- **Other versions of an operating system**, other architectures, and other
  locales than those of the CI images and the maintainers' PC.
- **Another RTF reader.**  The tests compare the RTF text.  They do not open
  it in a word processor.

This plan accepts these risks.  Each is a limit of what the tests run on,
and the Quality Report names them again for each release.

The Quality Report's sections on the test environments and on the supported
environments (the report is made at a release) list each environment
actually run, with its versions and result, and say which one stands for
which supported range.  Section 4, criterion 3, requires the tests to pass
on every one of them.

### Residual risk, and how a deviation is accepted

What is left after the above: a check of a mapping that was wrong (it is
recorded with its evidence, so it can be read again); behaviour the FRs do
not state because the help does not; a reading of a reference RTF that
missed something, which affects regression only; and the environments that
are not tested.

A requirement the tests cannot check is a deviation: one row of
`inst/validation/deviations.csv` with the FRs (`ids`), the `item`, the
`risk` (high, medium or low) and its basis, the `reason` it is accepted, and
the `action` planned.  The author or maintainer accepts it (section 7).
There is no high-risk deviation without a reason, and none that is not yet
assessed (section 4, criterion 2).  The Quality Report lists the deviations
with their reasons.  When a test is added, its row is removed.

## 4. Acceptance criteria

A release is accepted -- the Quality Report concludes *validation-ready* --
when all of these hold:

1. every leaf FR mapped to tests has its tests pass, the guards and the
   interactions included;
2. every leaf FR with no test is a deviation in
   `inst/validation/deviations.csv` with its risk (high, medium, low, and
   why) and the reason it is accepted; there is no high-risk deviation
   without a reason, and none not yet assessed;
3. the tests pass on every environment of section 5, and R CMD check
   succeeds on every CI environment;
4. the changes were made under the responsibilities of section 7.

Line coverage and the arguments the tests give are aids, not criteria.

Only the CI run of a published release can conclude *validation-ready*.  A
run on a maintainer's PC, or a CI run started by hand, writes the same
report marked **DRAFT**, whatever it finds: a DRAFT concludes nothing, and
its conclusion names what is not yet met.  The conclusion is the
maintainers' own; no third-party audit is made.

## 5. Environments

- **CI** (7): the R-CMD-check workflow's matrix -- Ubuntu (R devel,
  release, oldrel-1, 4.2), macOS (release), Windows (release) -- and its
  job *dependency minimums* (Ubuntu, R 4.2, the suggested packages at the
  minimum versions `DESCRIPTION` names), at the release's commit; the tests
  and R CMD check.
- **Local** (2): the maintainers' PC, two versions of R (now 4.4.2 and
  4.6.1), the tests run by hand with `validation/env.R` before the release;
  the records are committed in `validation/env-local/`.

## 6. The documents, and when

| When | Plan | Report |
|---|---|---|
| The first release under this plan, and each major version | this **Quality Plan** (by hand) | the **Quality Report** (`quality-report-<tag>.pdf`), from 0.9.0 on: made by CI at the published release from the records -- the results (overall and by area, the skipped tests with their reasons), the environments' table, the supported environments and the tested ones that stand for them, the deviations and why accepted, the conclusion against section 4, how to read the attachments; a CI run started by hand makes it marked DRAFT (kept as the run's artifact `quality-report`, not attached to a release); attached to the release with the Traceability Matrix (`traceability_matrix-<tag>.xlsx`), every environment's sessionInfo and tests' log (`environments-<tag>.txt`), the tests' log of the mapped tests (`test-log-<tag>.txt`) and the tables behind the results (`ft-<tag>.csv`, `functions-<tag>.csv`, `candidates-<tag>.csv`) |
| Releases between them | the **Change Request Plan**: the release's GitHub milestone, each issue with its impact, risk and related FRs | the **Change Report**: made from the merged pull requests and issues, the FRs added or changed, the regression evidence (the tests, the byte comparison of the reference outputs), the environments; the deviations' reasons by hand |

What GitHub already records -- issues, pull requests, the CI runs, the
releases -- is referred to, not copied.

### When a dependency changes

A new version of a package rtfreporter uses (an import, a suggested
package, or R) is followed by a release of rtfreporter in one of two ways:

- when rtfreporter can follow it and still work with the versions it
  supported before (the minimums in `DESCRIPTION` stay), it is a **minor
  version**;
- when it cannot, and that new version of the dependency must be required
  (a minimum in `DESCRIPTION` is raised for it), it is a **major version**.

The minimums and why each is what it is are in
`inst/validation/dependency-minimums.md`; a release that raises one says so
there and in `NEWS.md`.

## 7. Roles and responsibilities

| Role | Responsible for |
|---|---|
| Development team | Evaluating each issue and deciding whether it is taken up. |
| Contributor (works from a fork) or collaborator (works in the repository): the developer making the change | Making the change with its tests and requirements, mapping the tests to the requirements, running the tests, and opening a pull request. |
| Author / maintainer | Reviewing and approving a contributor's pull request. Merging a change into the release branch only after all tests, of the change and for regression, have passed in all CI environments. Running the local environments of section 5 before a release, accepting any deviation, making the release, and the conclusion of the Quality Report. |

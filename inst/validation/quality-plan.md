# rtfreporter: Quality Plan

This plan says how rtfreporter's quality is assured for a release, and what
the Quality Report at that release must show.  It is written once for the
first release made under it and again for each major version.  The way changes are made
-- issues, pull requests, review, CI, branches, releases -- is in
[CONTRIBUTING.md](https://github.com/ichirio/rtfreporter/blob/main/CONTRIBUTING.md) (*Quality assurance*, *Branching &
collaboration*, *Continuous integration*, *Versioning & releases*) and is not
repeated here.

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
- **Functional Tests (FT)** are testthat blocks mapped to the leaves by a
  person who read them: in `inst/validation/test_map.csv`, or by a new
  test's own name (`"FT-PL<nnn> [FR-PL157 FR-PL159]: ..."`).  A test that
  merely calls a function is not coverage.  A parent requirement is covered
  when every leaf under it is.
- **The Traceability Matrix (FR -> FT)** (`traceability.xlsx`) holds the
  mapping only, no results; the results are the tests' log.

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
4. the changes were made as CONTRIBUTING.md says (a pull request, reviewed,
   with every CI workflow passing before the merge).

Line coverage and the arguments the tests give are aids, not criteria.

Only the CI run of a published release can conclude *validation-ready*.  A
run on a maintainer's PC, or a CI run started by hand, writes the same
report marked **DRAFT**, whatever it finds.  The conclusion is the
maintainers' own; no third-party audit is made.

## 5. Environments

- **CI** (6): the R-CMD-check workflow's matrix -- Ubuntu (R devel,
  release, oldrel-1, 4.2), macOS (release), Windows (release) -- at the
  release's commit; the tests and R CMD check.
- **Local** (2): the maintainers' PC, two versions of R (now 4.4.2 and
  4.6.1), the tests run by hand with `validation/env.R` before the release;
  the records are committed in `validation/env-local/`.

## 6. The documents, and when

| When | Plan | Report |
|---|---|---|
| The first release under this plan, and each major version | this **Quality Plan** (by hand) | the **Quality Report** (`quality-report-<tag>.pdf`): made by CI at the published release from the records -- the environments' table, the results, the deviations and why accepted, the conclusion, how to read the attachments; attached to the release with the Traceability Matrix (`traceability-<tag>.xlsx`), every environment's sessionInfo and tests' log (`environments-<tag>.txt`), the tests' log of the mapped tests (`test-log-<tag>.txt`) and the tables behind the results (`ft-<tag>.csv`, `functions-<tag>.csv`, `candidates-<tag>.csv`) |
| Releases between them | the **Change Request Plan**: the release's GitHub milestone, each issue with its impact, risk and related FRs | the **Change Report**: made from the merged pull requests and issues, the FRs added or changed, the regression evidence (the tests, the byte comparison of the reference outputs), the environments; the deviations' reasons by hand |

What GitHub already records -- issues, pull requests and their reviews, the
CI runs, the releases -- is referred to, not copied.

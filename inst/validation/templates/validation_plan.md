# Validation Plan: rtfreporter for TFL production

> **Template.** Everything below is fictional: "Example Pharma", its study
> EX-2026-001 and the names are made up. Replace the text in `<...>` and the
> example content with your own, and follow your company's procedures where
> they say more.

| | |
|---|---|
| Document id | `<VP-RTF-001>` |
| Version | `<1.0>` |
| Company | Example Pharma (fictional) |
| System | R package rtfreporter, version `<x.y.z>` (from `packageVersion("rtfreporter")`) |
| Author | `<name, role>` |
| Approved by | `<name, role, date, signature>` |

## 1. Purpose

This plan says how Example Pharma confirms that rtfreporter, installed in its
statistical computing environment, is fit to write the tables, listings and
figures (TFLs) of its clinical study reports as RTF files.

## 2. Scope

**In scope**

- rtfreporter version `<x.y.z>`, installed from `<CRAN / a GitHub release tag>`
  into `<the validated R library, path>`.
- R version `<4.x.y>` on `<OS>`, and the packages rtfreporter imports, at the
  versions recorded in the Validation Summary Report.
- **Intended use**: statistical programmers write R programs that turn an
  analysis results dataset (ARD, from `cards` / `cardx`) or a finished table
  into RTF pages -- column headers, pagination, titles, footnotes, running
  headers and footers -- for the TFLs of clinical study reports and
  submissions. The numbers come from the ARD or the table; rtfreporter lays
  them out.

**Out of scope**

- The statistics: the ARD's numbers are made, and validated, by the packages
  and programs that compute them (`cards`, `cardx`, your own).
- The other packages rtfreporter can read from (gt, gtsummary, rtables, tern,
  tfrmt, flextable, huxtable): validated on their own if used.
- The programs of a study: each is validated (double programming or review)
  under the study's own procedures.
- Viewing and printing the RTF (Word, LibreOffice): they are tools of the
  reader.

## 3. Roles

| Role | Who (fictional) | Responsibility |
|---|---|---|
| System owner | `<Head of Statistical Programming>` | Approves this plan and the Summary Report; decides on deviations. |
| Validation lead | `<Lead programmer>` | Writes the URS and UAT cases, builds the matrix, writes the Summary Report. |
| Testers | `<Two statistical programmers>` | Run the UAT cases and record the results. A tester does not test a case they wrote. |
| Quality assurance | `<QA representative>` | Reviews that the plan was followed. |

## 4. Approach: risk-based

rtfreporter is open-source software with its own quality records. Example
Pharma relies on them for what the package does in general, and tests itself
only what its own use needs, as deeply as the risk calls for.

**What the package already provides** (supplier evidence, attached to each
GitHub release of rtfreporter; also in this kit):

| Package document | What it shows |
|---|---|
| Quality Plan (`quality-plan.md`) | How the package's quality is assured: scope, the risk-based requirements, the acceptance criteria, the environments. |
| Functional Requirements (`requirements.csv`) | What each exported function does, its arguments, the data it handles and the misuse it stops: one id (`FR-...`) each. |
| Functional Tests and the Traceability Matrix (`traceability.xlsx`, release asset) | The tests of each FR (FR -> FT). |
| Quality Report (release asset) | The results on every CI environment (Linux, macOS, Windows; several R versions), the deviations (an FR with no test, and why it is accepted) and the conclusion; with every environment's sessionInfo and test log (`environments.txt`). |

**What Example Pharma checks itself**

1. **Installation**: the package and its dependencies install into the
   validated library; `packageVersion("rtfreporter")` is the version in
   scope; the package's own tests pass there (`testthat::test_package("rtfreporter")`
   or `R CMD check` of the source package) -- record the log.
2. **User requirements** (`urs.csv`): what our TFLs need, each linked to the
   FRs it relies on.
3. **UAT** (`uat_cases.csv`): our own cases on dummy data, run in our
   environment. Depth by priority:
   - `High`: at least one case, with an objective check (an independent
     number, or a checksum) where one is possible;
   - `Medium`: at least one case; a visual check is enough;
   - `Low`: covered by the package's evidence (the FR's tests passed in the
     Quality Report) or by one visual check.
4. **Traceability** (`build_user_matrix.R`): every UR maps to FRs and to UAT
   cases; every FR named exists in the version in scope.

## 5. Acceptance criteria

rtfreporter `<x.y.z>` is accepted for the intended use when all of these hold:

1. The installation check passed and its log is filed.
2. The package's Quality Report for `<x.y.z>` concludes *validation-ready*,
   and none of its deviations touches a FR linked to a `High` UR without an
   assessment by the system owner.
3. Every UR has at least one UAT case, and every case has a result.
4. Every UAT case passed, or its failure is a deviation assessed and
   accepted (section 6).
5. The matrix shows no UR without a case and no FR id unknown to the version.

## 6. Deviations

A UAT case that fails, or cannot be run as written, is a deviation. For each:

1. Record it in the Summary Report: the case, what happened, the cause.
2. Classify it: **a defect of rtfreporter** (report it as an issue on the
   package's GitHub, with a reproducible example on dummy data), **a defect of
   the case** (fix the case and rerun), or **an environment problem** (fix and
   rerun).
3. Assess the risk to the intended use (high, medium, low) and decide:
   accept with a workaround (say which, and tell the users), wait for a fixed
   version, or exclude the feature from the intended use.

A high-risk deviation with no workaround blocks acceptance.

## 7. Deliverables

| Document | File |
|---|---|
| This plan | `validation_plan.md` |
| User Requirement Specification | `urs.csv` (columns explained in `urs.md`) |
| UAT cases and their results | `uat_cases.csv` / `uat_cases.xlsx` |
| Traceability Matrix (URS -> FR -> UAT) | `user_traceability.xlsx` (from `build_user_matrix.R`) |
| Validation Summary Report | `validation_summary_report.md` |
| Supplier evidence | the package's Quality Plan, requirements, and the release's Quality Report and attachments |

## 8. A new version of rtfreporter

Before moving to a new version: read its NEWS and the Change Report of its
release; list the FRs added, changed or removed; rerun the UAT cases whose
`fr_ids` include them (and the `High` ones); update the URS if the intended
use changes; write a short Summary Report for the new version.

## 9. References

- rtfreporter Quality Plan: `quality-plan.md` (in this kit, and
  `system.file("validation", "quality-plan.md", package = "rtfreporter")`).
- rtfreporter Functional Requirements: `requirements.csv`.
- rtfreporter releases (Quality Report, traceability, environments):
  <https://github.com/ichirio/rtfreporter/releases>
- The article "Validating rtfreporter in your environment":
  <https://ichirio.github.io/rtfreporter/articles/validation.html>
- `<Your company's SOP on computerized system validation>`

# Validating rtfreporter in your environment

A company that uses R for clinical reports validates each package for
its own use, in its own environment. rtfreporter gives you two things
for that: its own quality records, and templates for yours.

## What the package provides

The package keeps three records of what it does and how it is tested.

- **Functional Requirements (FR).** One row per thing a function does,
  an argument, a data shape it handles, a misuse it stops, or a risky
  combination. Each has an id such as `FR-PL161`. They are in
  `inst/validation/requirements.csv`.
- **Functional Tests (FT).** The package’s tests, each mapped to the FRs
  it checks by the developer making the change.
- **The Traceability Matrix (FR -\> FT).** Every FR with its tests. An
  FR with no test is listed as a deviation, with its risk and why it is
  accepted.

From 0.9.0 on, CI writes the **Quality Report** at each release and
attaches it to the GitHub release as `quality-report-<tag>.pdf`. It
holds:

- the results: the requirements with a test and the tests that pass,
  overall and by area, and each skipped test with its reason;
- the environments the tests ran on (six in CI – Linux, macOS and
  Windows, several R versions, among them the minimum environment, with
  R and every dependency at its minimum version – and two runs on the
  maintainers’ PC), each with its result;
- the environments the package supports (the R version, the operating
  systems, the suggested packages) and, for each, the tested environment
  that stands for it, with what is not tested and the risk accepted;
- the deviations, and the conclusion.

Only the report of a published release can conclude *validation-ready*.
A report from a CI run started by hand is marked DRAFT and concludes
nothing: take the report of the release you install. The release carries
the evidence too, each file’s name ending in the tag:

| Your question | Attachment |
|----|----|
| Which tests check this requirement? | `traceability_matrix-<tag>.xlsx` |
| Did the tests pass? | `test-log-<tag>.txt` |
| What exactly did each environment run with? | `environments-<tag>.txt` (every environment’s sessionInfo and test log) |
| What does this test check? Which tests exist for no requirement? | `ft-<tag>.csv`; `candidates-<tag>.csv` |
| How deeply is this function covered? | `functions-<tag>.csv` |

The rules behind the report are the package’s **Quality Plan**
(`inst/validation/quality-plan.md`, also in the kit below).

You use these as supplier evidence. You do not repeat them.

How the approach works, and a map of all the documents, are in [How
rtfreporter is
validated](https://ichirio.github.io/rtfreporter/articles/validation-approach.md).

## What you do

1.  **Copy the kit** into a folder of your own.

    ``` r

    library(rtfreporter)
    rtfreporter_validation_kit("rtfreporter-validation")
    ```

    The folder holds:

    | File | What it is |
    |----|----|
    | `validation_plan.md` | The Validation Plan: scope, intended use, roles, approach, acceptance criteria, deviations. |
    | `urs.csv`, `urs.md` | The User Requirement Specification (URS): your requirements (`UR-NNN`), each linked to FR ids. `urs.md` explains the columns of the URS and of the UAT cases. |
    | `uat_cases.csv`, `uat_cases.xlsx` | Your user acceptance test (UAT) cases (`UAT-NNN`) on dummy data, with columns to record the results. |
    | `build_user_matrix.R` | Writes your Traceability Matrix (URS -\> FR -\> UAT). |
    | `validation_summary_report.md` | The Validation Summary Report you fill at the end. |
    | `quality-plan.md`, `requirements.csv` | The package’s Quality Plan and FRs, for reference. |

    The examples are fictional (a company called “Example Pharma”).
    Replace them with yours.

2.  **Write the Validation Plan.** Say which version, which environment
    and what use. Decide how deeply to test each priority.

3.  **Adapt the URS.** Keep the needs your reports have, drop the
    others, add your own. For each, find the FRs it relies on in
    `requirements.csv` (filter on the function you call).

4.  **Run the UAT cases** in your environment, on dummy data. Record
    what you saw, the result, who and when. A failure is a deviation:
    record it, find its cause, and decide (the Validation Plan, section
    6).

5.  **Build the matrix.**

    ``` r

    kit_dir <- "rtfreporter-validation"
    source(file.path(kit_dir, "build_user_matrix.R"))
    ```

    Or from a shell, in the folder that holds the kit:
    `Rscript rtfreporter-validation/build_user_matrix.R rtfreporter-validation`.
    It writes `user_traceability_matrix.csv` and
    `user_traceability_matrix.xlsx` into the kit folder.

6.  **Fill the Validation Summary Report**, and attach the package’s
    Quality Report of the release for the same version (not a DRAFT).
    Check that your environment is one the report’s
    supported-environments section covers, and read its deviations
    against your URS.

## How they connect

The FR ids join your records to the package’s.

    your need          the package's requirement      the package's tests
    UR-008  ----------> FR-PL161 decimal alignment ----> FT (in traceability_matrix.xlsx)
       |
       +--> UAT-011  your check in your environment

- Your matrix (URS -\> FR -\> UAT) shows that every need has a check of
  your own.
- The package’s matrix (FR -\> FT) and Quality Report show that the same
  FRs are tested, and passed, on every environment of its Quality Plan,
  and which supported environments those stand for.
- Together they cover a need from your words down to a test.

## A new version

Read the version’s NEWS and the release’s records. Find the FRs added,
changed or removed. Rerun the UAT cases that name them, and the
high-priority ones. Write a short Validation Summary Report for the new
version (the Validation Plan, section 8).

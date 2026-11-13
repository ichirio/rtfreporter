# Copy the validation kit: templates to validate rtfreporter in your environment

rtfreporter ships its own quality records – the Functional Requirements
(FR), the tests mapped to them and, at each release, a Quality Report.
Your company still has to show that the package is fit for *its* use in
*its* environment. This copies the templates for that into a folder, to
edit and fill:

## Usage

``` r
rtfreporter_validation_kit(dir, overwrite = FALSE)
```

## Arguments

- dir:

  The folder to write into. Created if it does not exist.

- overwrite:

  `FALSE` (default) refuses a folder that already has files in it;
  `TRUE` writes into it, replacing the kit's files of the same name.

## Value

The paths of the files written, invisibly.

## Details

- `validation_plan.md`:

  The Validation Plan: purpose, scope and intended use, roles, the
  risk-based approach (what the package's evidence covers, what you
  check), acceptance criteria, deviations.

- `urs.csv`, `urs.md`:

  The User Requirement Specification: your needs (`UR-NNN`), each linked
  to the package's FR ids; `urs.md` explains the columns of this file
  and of the UAT cases.

- `uat_cases.csv`, `uat_cases.xlsx`:

  User acceptance test cases (`UAT-NNN`) on dummy data, with blank
  `actual`, `result`, `tester` and `date` to fill.

- `build_user_matrix.R`:

  Writes your Traceability Matrix (URS -\> FR -\> UAT):
  `user_traceability_matrix.csv`, and `user_traceability_matrix.xlsx`
  when writexl is installed.

- `validation_summary_report.md`:

  The Validation Summary Report you fill: environment, UAT results,
  deviations, conclusion.

- `quality-plan.md`, `requirements.csv`:

  The package's own Quality Plan and Functional Requirements, for
  reference.

The example content is fictional (a company "Example Pharma"); replace
it with your own.

From 0.9.0 on, the package's Quality Report is attached to each GitHub
release of rtfreporter as `quality-report-<tag>.pdf`. It states the
results (overall and by area, with the reason for each skipped test),
the environments the tests ran on and how they stand for the
environments the package supports, the deviations, and the conclusion.
Use the report of the release you install: a report from a CI run
started by hand is marked DRAFT and concludes nothing. The same release
carries the evidence, each file's name ending in the tag:
`traceability_matrix-<tag>.xlsx` (which tests check each requirement),
`environments-<tag>.txt` (every environment's sessionInfo and test log),
`test-log-<tag>.txt` (the mapped tests' log) and the tables behind the
results, `ft-<tag>.csv`, `functions-<tag>.csv` and
`candidates-<tag>.csv`.

## See also

The article "Validating rtfreporter in your environment",
<https://ichirio.github.io/rtfreporter/articles/validation.html>.

## Examples

``` r
kit <- rtfreporter_validation_kit(file.path(tempdir(), "rtfreporter-validation"))
basename(kit)
#> [1] "build_user_matrix.R"          "uat_cases.csv"               
#> [3] "uat_cases.xlsx"               "urs.csv"                     
#> [5] "urs.md"                       "validation_plan.md"          
#> [7] "validation_summary_report.md" "quality-plan.md"             
#> [9] "requirements.csv"            
unlink(dirname(kit[1]), recursive = TRUE)
```

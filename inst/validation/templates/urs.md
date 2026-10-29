# User Requirement Specification (URS) and UAT cases: the columns

These are templates. The content is fictional (a company called "Example
Pharma" and its study EX-2026-001). Replace it with your own needs, keep the
columns, and keep the ids unique.

## `urs.csv`: what your work needs from rtfreporter

| Column | What to write |
|---|---|
| `id` | `UR-NNN`, unique, never reused (a removed requirement keeps its number). |
| `statement` | One need of your work, in your words: the output, not the function. |
| `priority` | `High`, `Medium` or `Low`: how bad a wrong result would be for your reports. It sets how deeply you test (see the Validation Plan). |
| `fr_ids` | The rtfreporter Functional Requirements (FR) the need relies on, separated by `;`. They are the ids of `requirements.csv` (in this kit). Each FR has its own tests in the package; the package's Quality Report says whether they passed. |

To find FR ids: open `requirements.csv`, filter on `functions` (the
function you will call) or search `statement`. Prefer the leaves (the rows no
other row names as `parent`): they are what the package's tests check. A
parent (`kind = group`) is fine to say "all of this function".

## `uat_cases.csv` (and `uat_cases.xlsx`): how you check it yourself

| Column | What to write |
|---|---|
| `id` | `UAT-NNN`, unique. |
| `ur_ids` | The URs the case checks, separated by `;`. Every UR should have at least one case. |
| `fr_ids` | The FRs the case exercises, separated by `;` (a subset of its URs' `fr_ids`). |
| `steps` | What to run, on dummy data only (the package's suggested `cards` and `pharmaverseadam` data, or your own synthetic data). |
| `expected` | What a correct output looks like: a number to compare, a checksum to match, or "Visual check" with what to look at. |
| `actual` | Filled when run: what you saw. |
| `result` | Filled when run: `Pass`, `Fail`, or `Deviation` (see the Validation Plan). |
| `tester` | Filled when run: who ran it. |
| `date` | Filled when run: when (YYYY-MM-DD). |

`uat_cases.xlsx` is the same table, for testers who record in Excel. The csv
is the master: if you record in the xlsx, save it back as `uat_cases.csv`
before you build the matrix.

## The matrix

`build_user_matrix.R` reads `urs.csv`, `uat_cases.csv` and
`requirements.csv` and writes `user_traceability.csv` and `.xlsx`: one row per
(UR, FR) pair, with the UAT cases that check it and their results, and one
row per UR with its status (`pass`, `not run`, `fail or deviation`,
`no UAT case`).

```sh
Rscript build_user_matrix.R path/to/kit
```

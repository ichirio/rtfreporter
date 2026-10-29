# Validation Summary Report: rtfreporter for TFL production

> **Template.** Everything below is fictional: "Example Pharma" and the names
> are made up. Fill in the `<...>` and replace the examples with your own
> results.

| | |
|---|---|
| Document id | `<VSR-RTF-001>` |
| Plan | `<VP-RTF-001>`, version `<1.0>` |
| Company | Example Pharma (fictional) |
| System | rtfreporter `<x.y.z>` |
| Author | `<name, role, date>` |
| Approved by | `<name, role, date, signature>` |

## 1. Summary

`<One paragraph: what was validated, for what use, and the conclusion.>`

## 2. Environment

| Item | Value |
|---|---|
| Operating system | `<e.g. Windows Server 2022 / RHEL 9>` |
| R | `<4.x.y>` |
| rtfreporter | `<x.y.z>`, installed from `<CRAN / GitHub release tag>` on `<date>` |
| Library | `<path of the validated library>` |
| Dependencies | `<attach the output of sessionInfo() and of installed.packages()[, c("Package", "Version")] for the library>` |

## 3. Installation check

| Check | Result | Evidence |
|---|---|---|
| Installed into the validated library | `<Pass / Fail>` | `<install log, file name>` |
| `packageVersion("rtfreporter")` is `<x.y.z>` | `<Pass / Fail>` | `<console output>` |
| The package's tests pass in this environment | `<Pass / Fail>` | `<test log, file name>` |

## 4. Supplier evidence

| Document | Version / date | Conclusion |
|---|---|---|
| rtfreporter Quality Report | `<x.y.z>`, `<date>` | `<validation-ready / ...>` |
| Its deviations relevant to our URs | `<none / list the FR ids and our assessment>` | |

## 5. UAT results

| | Count |
|---|---|
| User requirements | `<15>` |
| UAT cases | `<16>` |
| Passed | `<n>` |
| Failed | `<n>` |
| Deviations accepted | `<n>` |
| URs with no case | `<0>` |

The full results are in `uat_cases.csv`; the matrix (URS -> FR -> UAT) is
`user_traceability.xlsx`, version `<date>`.

## 6. Deviations

| Id | UAT case | What happened | Cause | Risk | Decision |
|---|---|---|---|---|---|
| `<DEV-001>` | `<UAT-NNN>` | `<...>` | `<rtfreporter / case / environment>` | `<high / medium / low>` | `<accepted with workaround ... / fixed and rerun / excluded from use>` |

`<None>` if there were none.

## 7. Conclusion

`<rtfreporter x.y.z is / is not fit for the intended use stated in the
Validation Plan, with the restrictions: ...>`

The system may be released for production use from `<date>`.

## 8. Attachments

- Validation Plan `<VP-RTF-001>`
- `urs.csv`, `uat_cases.csv` (with results), `user_traceability.xlsx`
- Installation and test logs, sessionInfo
- rtfreporter `<x.y.z>` Quality Report and its attachments (from the GitHub
  release)

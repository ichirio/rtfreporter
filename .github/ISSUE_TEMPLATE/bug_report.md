---
name: Bug report
about: Report something that does not work as documented
title: "[Bug] "
labels: bug
assignees: ""
---

## What happened

<!-- A clear description of the problem: what you expected vs. what occurred. -->

## Reproducible example

<!-- The smallest self-contained snippet that triggers it.  A
     reprex::reprex() is ideal.  Paste the code AND its output. -->

```r
library(rtfreporter)
# ...
```

## For rendering problems

<!-- If the RTF looks wrong, attach the offending .rtf (or the code that
     produces it) and, if possible, a screenshot of how it opens in
     Word / LibreOffice. -->

## Session info

<!-- Paste the output of sessionInfo() (R version, OS, package versions). -->

```r
# sessionInfo()
```

## Impact, risk and the requirements it touches

<!-- For the maintainers' change control (inst/validation/quality-plan.md).
     Leave blank if you are not sure; a maintainer fills it in. -->

- Functions and outputs affected:
- Risk (high / medium / low) and why:
- Functional requirements (FR ids, inst/validation/requirements.csv):

## Checklist

- [ ] I searched existing issues and this is not a duplicate.
- [ ] I included a minimal reproducible example.

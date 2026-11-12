---
name: pre-pr
description: The checks to run before opening a pull request in rtfreporter, tflspec or tflplanner.
disable-model-invocation: true
---

Run these in order. Stop at the first failure and report it.

1. Run the tests of the changed area.
2. Run `lintr::lint_package()`. It must report 0 lints.
3. List the `pkg::` uses and `requireNamespace()` calls added in the diff, and
   check each is declared in `DESCRIPTION`.
4. When a change in tflspec can break tflplanner, run tflplanner's tests too.
5. Report each result in the pull request description, in the form the pull request template asks for.

Do not edit `NEWS.md`, `CHANGELOG.md` or `Version:`.

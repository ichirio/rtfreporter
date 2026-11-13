# Developing with AI agents

[In
Japanese](https://ichirio.github.io/rtfreporter/articles/ai-agents-ja.md)

This article is for a contributor who works with an **agent**: a tool
that edits files and runs commands in the repository. Two such tools are
covered: the Claude desktop app (its Code tab) and VS Code with GitHub
Copilot’s agent. This page gives what is the same for both. The steps
for each tool are on its own page:

- [Set-up in the Claude desktop
  app](https://ichirio.github.io/rtfreporter/articles/ai-agents-claude-app.md)
- [Set-up in VS Code with GitHub
  Copilot](https://ichirio.github.io/rtfreporter/articles/ai-agents-vscode-copilot.md)

Advice that is the article’s own, and not from a tool’s documentation,
is said to be so.

## 1. What this article is for

Some pages already cover nearby ground. This one does not repeat them.

- `CONTRIBUTING.md` and the [architecture
  article](https://ichirio.github.io/rtfreporter/articles/architecture.md)
  give the rules: branches, labels, review, versioning.
- [Developing with an AI
  assistant](https://ichirio.github.io/rtfreporter/articles/ai-development.md)
  is for a chat assistant that you give the AI developer manual.
- This article is about the set-up of an agent, how to work with it, and
  how to check what it made.

One set-up serves the three packages, rtfreporter, tflspec and
tflplanner, because one change often crosses them. Where a package
differs, the text says so.

Two things are outside this article: writing analysis code for a study
(see [Asking an AI chat for a table’s plan
code](https://ichirio.github.io/rtfreporter/articles/ai-plan-code.md)),
and the AI kits that the packages ship for their users.

## 2. The environment

An agent installs branch builds and runs tests. If your own R library is
on the path, a test can pass only because of a package that you happen
to have installed. An install can also overwrite a package that you use
for other work. So give the agent a separate **work library** and a
separate **work home**.

- **R version.** `DESCRIPTION` (`Depends:`) holds the minimum, not the
  version to use. The versions that are tested are in *Development
  setup* in `CONTRIBUTING.md` and in the CI matrix in
  `.github/workflows/R-CMD-check.yaml`. Do not copy a version from this
  page.
- **Folders.** Before the first R call, set `R_LIBS_USER`, `HOME` and
  the `R_USER_*` folders (`R_USER_CACHE_DIR`, `R_USER_DATA_DIR`,
  `R_USER_CONFIG_DIR`) to folders of the work area. This holds for a
  one-line `Rscript -e` call too. Set `TMP`, `TEMP` and `TMPDIR` as
  well, so that no file lands in the system temporary folder.
- **Windows.** Setting `HOME` alone is not enough: R still finds the
  user library. Set `R_LIBS_USER` too.
- **Which R.** Call the R that the project names, by its full path. Do
  not use whichever `Rscript` comes first on the path.

An example, with a fictional work folder `C:/work/tfl`. Make the folders
first.

``` text
WORK_ROOT=C:/work/tfl
R_LIBS_USER=C:/work/tfl/lib
HOME=C:/work/tfl/home
R_USER_CACHE_DIR=C:/work/tfl/home/cache
R_USER_DATA_DIR=C:/work/tfl/home/data
R_USER_CONFIG_DIR=C:/work/tfl/home/config
TMP=C:/work/tfl/tmp
TEMP=C:/work/tfl/tmp
TMPDIR=C:/work/tfl/tmp
```

The work library starts empty. Fill it with the project’s R and these
variables:
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
for the packages in *Development setup* in `CONTRIBUTING.md`, or
`devtools::install_dev_deps()` in the clone (install devtools first).

Run the script below first in every session. It prints the facts that
matter and stops if a library or folder outside the work area is in use.
Set `WORK_ROOT` to the work folder before you run it. The page shows the
script and does not run it.

Show code

``` r

# check-env.R -- run first: Rscript check-env.R
# WORK_ROOT is the folder that holds the work library and the work home.
win <- .Platform$OS.type == "windows"
norm <- function(path) {
  p <- sub("/+$", "", normalizePath(path, winslash = "/", mustWork = FALSE))
  if (win) tolower(p) else p   # drive and folder case do not matter on Windows
}
root <- Sys.getenv("WORK_ROOT")
if (!nzchar(root) || !dir.exists(root)) {
  stop("Set WORK_ROOT to an existing work folder first.", call. = FALSE)
}
root <- norm(root)
# TRUE for the root itself and for a path below it; "C:/work-old" is not.
under_root <- function(path) {
  p <- norm(path)
  nzchar(path) && (p == root || startsWith(p, paste0(root, "/")))
}

cat("R:", R.version.string, "\n")
cat("R home:", R.home(), "\n")
cat("Library paths:\n")
print(.libPaths())

for (pkg in c("rtfreporter", "tflspec", "tflplanner")) {
  loc <- system.file(package = pkg)
  cat(sprintf("%-12s %s\n", pkg, if (nzchar(loc)) loc else "(not installed)"))
}
for (pkg in c("testthat", "lintr", "roxygen2", "covr")) {
  ver <- tryCatch(as.character(utils::packageVersion(pkg)),
                  error = function(e) "(not installed)")
  cat(sprintf("%-12s %s\n", pkg, ver))
}

# The base library and the site library of this R are allowed and listed;
# every other library must be in WORK_ROOT.
allowed <- unique(c(.Library, .Library.site))
cat("Allowed (base and site):\n", paste0("  ", allowed, collapse = "\n"), "\n")
libs <- .libPaths()
libs <- libs[!norm(libs) %in% norm(allowed)]
outside <- libs[!vapply(libs, under_root, logical(1))]

# R_LIBS_USER must be set, and each folder in it must exist and be in WORK_ROOT.
# (.libPaths() leaves out a folder that does not exist.)
user_libs <- strsplit(Sys.getenv("R_LIBS_USER"), .Platform$path.sep)[[1]]
bad_user <- length(user_libs) == 0L ||
  !all(vapply(user_libs, function(p) under_root(p) && dir.exists(p), logical(1)))

dirs <- Sys.getenv(c("HOME", "R_USER_CACHE_DIR", "R_USER_DATA_DIR",
                     "R_USER_CONFIG_DIR", "TMP", "TEMP", "TMPDIR"))
bad_dirs <- names(dirs)[!vapply(dirs, under_root, logical(1))]

if (length(outside) > 0L || bad_user || length(bad_dirs) > 0L) {
  if (length(outside) > 0L) {
    cat("Libraries outside WORK_ROOT:\n", paste0("  ", outside, collapse = "\n"), "\n")
  }
  if (bad_user) {
    cat("R_LIBS_USER is unset, missing, or outside WORK_ROOT.\n")
  }
  if (length(bad_dirs) > 0L) {
    cat("Folders outside WORK_ROOT (or unset):", paste(bad_dirs, collapse = ", "), "\n")
  }
  stop("A library or folder outside the work area is in use. Fix the environment first.",
       call. = FALSE)
}
cat("Environment OK.\n")
```

**Data.** Use fictional data only (Example Pharma) and the sample
studies that the packages ship. Never put company, study or patient data
in code, tests, issues, pull requests or an agent’s session.

## 3. What the harness holds

The **harness** is everything around the agent: its instructions, its
reusable tasks, its permissions and the checks that decide.

### Instruction files

Keep one source and short pointers to it, not three copies. The files
are:

- `AGENTS.md`: the shared orientation. This repository has one.
- `CLAUDE.md`: read by Claude Code. This repository has one. It imports
  `@AGENTS.md` and adds the notes for Claude Code. Keep it that way.
- `.github/copilot-instructions.md`: read by Copilot. This repository
  has one.

What they say should be short:

- where the authoritative documents are (`CONTRIBUTING.md`, the
  architecture article, the AI developer manual);
- which branch to work from;
- what an agent must not touch: `Version:` in `DESCRIPTION`, `NEWS.md`
  and `CHANGELOG.md`;
- the test and lint commands;
- when to stop and ask.

Claude Code reads `CLAUDE.md` at the start of each session. It reads
`AGENTS.md` only when there is no `CLAUDE.md`, so a `CLAUDE.md` that
wants the shared text imports it, as this repository’s does. Copilot in
VS Code reads `.github/copilot-instructions.md` or `AGENTS.md`. Each
set-up page shows how to confirm that the file was read. An instruction
file is a request, not a lock: Claude Code’s documentation says so, and
says that a hook is what blocks an action.

### The pre-PR check as a reusable task

Both tools can keep a task that you start by name. The list is the same,
so either tool makes the same check. It is a faster subset, to run on
each change:

1.  Run the tests of the changed area.
2.  Run `lintr::lint_package()`. The result must be 0 lints.
3.  Check that every new `pkg::` use or
    [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) call is
    declared in `DESCRIPTION`.
4.  When a change in tflspec can break tflplanner, run tflplanner’s
    tests too.
5.  Report each result in the pull request description, in the form the
    pull request template asks for.

This task is not the four pre-push checks of `CONTRIBUTING.md`:
`R CMD check --as-cran`, `devtools::test()`,
[`pkgdown::check_pkgdown()`](https://pkgdown.r-lib.org/reference/check_pkgdown.html)
and lintr. CI runs the full set, and so does a maintainer before a
release.

The Claude desktop app holds the task as a **skill**, and VS Code as a
**prompt file**. The set-up pages show both. The repository ships the
skill, as `.claude/skills/pre-pr/SKILL.md`; it has no prompt file.

### Checks that decide

An agent’s judgment is not a check. These are:

- `testthat`, run as in `CONTRIBUTING.md`;
- `lintr`, with the repository’s `.lintr`;
- `R CMD check`;
- `cran-readiness`, which is run by hand before a CRAN submission
  (`R CMD check --as-cran` with the manual, URLs, spelling);
- `version-guard`, a CI check that refuses a change of the version to a
  release position unless the pull request has the `release` label;
- the `lint` workflow, which runs lintr in CI;
- coverage, in `test-coverage.yaml`;
- undeclared dependencies: search the diff for `pkg::` and
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) and
  compare the names with `DESCRIPTION`.
- a scan of the dependencies for known vulnerabilities. GitHub’s list of
  ecosystems that Dependabot supports has GitHub Actions, and no R row
  was found. This repository has no `.github/dependabot.yml` file today,
  and its security settings (read 2026-11-08) show Dependabot security
  updates and secret scanning as disabled. No scanner is set up for the
  R dependencies.

### Checks on each edit

Some rules are better enforced at each edit than at the end: lint a
changed R file, or refuse an edit to `Version:` in `DESCRIPTION`, to
`NEWS.md` or to `CHANGELOG.md`. Claude Code does this with **hooks**. VS
Code has hooks too, in Preview. The Claude page shows an example set,
which is in `.claude/` in the repository (in
[`next`](https://rdrr.io/r/base/Control.html) now, and in `main` with
the next merge). The VS Code page shows the format of a hook. Where a
tool cannot do it, the pre-PR check and CI cover the same ground.

## 4. The workflow

### The loop

Issue, branch, small pull request, CI, review, merge.

- One change per pull request, with `Closes #n` in its description.
- A contributor opens the pull request against `main`. A maintainer (a
  Collaborator, with write access) works on a topic branch that goes to
  [`next`](https://rdrr.io/r/base/Control.html), which is merged to
  `main` about once a week (*Branching & collaboration* in
  `CONTRIBUTING.md`). `AGENTS.md`, which Copilot’s instructions point
  to, names `main` only, so a maintainer who uses Copilot follows
  `CONTRIBUTING.md`.
- The description holds the *User-visible change (for NEWS)* text. The
  agent does not edit `NEWS.md` or the version.
- The agent opens the pull request. It does not merge it. The pause for
  review is the point.

### Test first

Ask the agent to write the test that fails before the fix, and to show
it failing. Then it fixes the code. For a bug report, the reproduction
comes first and goes into the issue or the pull request.

### What an agent does well

- Reproducing a problem and narrowing down its cause.
- A mechanical edit across many files.
- Writing tests.
- Translating text under a rule that you state.
- Bringing a branch up to date with its base (`main`, or
  [`next`](https://rdrr.io/r/base/Control.html) for a maintainer).

### What a person must check

Whatever the agent reports:

- **The numbers in an output.** Compare them with a count made another
  way (section 5).
- **Screens.** Look at them in a real browser.
- **Design decisions, API names, and anything users see.**
- **The next package.** A change in one package must not break the one
  after it (rtfreporter, tflspec, tflplanner).
- **That “done” matches the commit.** Read the diff, not the summary.

## 5. Checking the finished material

A table that looks right can still be wrong. The check is a comparison
with numbers that you worked out without the package.

### Hand-worked expected tables

Make a small fictional dataset (Example Pharma). Work out every count,
percent and N by hand, once, and keep the result next to the test. The
test compares the generated table with that result, cell by cell. It
does not compare it with the code’s own earlier output.

### An example: blank values

Data read from a `.xpt` file gives a missing character value as `""`,
not `NA`. A table that counts causes then treats `""` as a category. It
prints an extra blank row, and its percents look right.

A test on in-memory data with `NA` passes and hides this. The
hand-worked table, made from data that was written and read back as the
study stores it, catches it.

### The order

1.  Take the data as the study stores it (write it to `.xpt` and read it
    back).
2.  Make the expected table by hand.
3.  Run the package.
4.  Compare the two, cell by cell.
5.  Then a person looks at the page.

When a check has found a problem once, keep it as a regression test.

## 6. The two tools, feature by feature

| Job | Claude desktop app (Code tab) | VS Code with Copilot |
|----|----|----|
| Standing instructions for the repository | `CLAUDE.md`. `AGENTS.md` is read only when there is no `CLAUDE.md`, unless `CLAUDE.md` imports it. | `.github/copilot-instructions.md`, or `AGENTS.md`. |
| A reusable task (the pre-PR check) | A skill: `.claude/skills/<name>/SKILL.md`, started with `/name`. | A prompt file: `.github/prompts/<name>.prompt.md`, started with `/name`. It runs only in the Local harness, which is deprecated. |
| Running tests and lint | Runs commands with its Bash tool. | Runs commands in the integrated terminal (Local sessions). |
| What it may run without asking | Permission rules and permission modes. | Terminal auto-approve rules (`chat.tools.terminal.autoApprove`). |
| Checks on each edit | Hooks in `.claude/settings.json`. | Hooks (Preview) in `.github/hooks/*.json`. |
| Handing a piece of work to a helper | Separate sessions run in parallel; with **Worktree** on, each has its own worktree. | Subagents, which work in their own context. |
| Reading an Issue or a pull request | `gh` in the terminal. The app shows a pull request’s CI status after you open it. | `gh` in the terminal. The VS Code pages read for this set-up do not say how Copilot reads an Issue or a pull request. |

The facts in this table come from the tools’ own documentation, read on
2026-11-08; the set-up pages link the pages. Two facts about the Claude
app were seen in a test in the app, and the Claude page says which.
Tools change fast, so check the tool’s documentation when a step does
not work as written.

## Where next

- [Set-up in the Claude desktop
  app](https://ichirio.github.io/rtfreporter/articles/ai-agents-claude-app.md)
- [Set-up in VS Code with GitHub
  Copilot](https://ichirio.github.io/rtfreporter/articles/ai-agents-vscode-copilot.md)
- `CONTRIBUTING.md` for the rules, and the [AI developer
  manual](https://ichirio.github.io/rtfreporter/ai/rtfreporter-ai-dev-manual-0.8.2.md)
  for the briefing to attach to a chat.
- [Developing with an AI
  assistant](https://ichirio.github.io/rtfreporter/articles/ai-development.md)
  for a chat assistant.

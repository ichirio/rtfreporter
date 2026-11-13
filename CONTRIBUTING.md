# Contributing to rtfreporter

Thanks for taking the time to contribute! Issues and pull requests are
very welcome at <https://github.com/ichirio/rtfreporter>.

By participating in this project you agree to abide by its
[Code of Conduct](CODE_OF_CONDUCT.md).

## Becoming a contributor

There is **no application or invitation step** — anyone can contribute:

1. Open or comment on an [issue](https://github.com/ichirio/rtfreporter/issues)
   to report a bug or propose a change.  (Issue forms guide you: *Bug
   report* / *Feature request*.)
2. For anything non-trivial, agree the approach on the issue first.
3. Fork the repo, create a topic branch, make the change, and open a
   pull request (the PR template's checklist walks you through it).
4. A maintainer reviews and approves your pull request; once it is approved
   and all of CI is green, it is merged.

After a couple of merged, good-quality PRs you may be offered **write
access / maintainer status** (the ability to push branches, label issues,
and review PRs) — just ask, or it will be offered.  Contributions of every
size are valued, including docs, tests, and bug reports.

## Contributor vs Collaborator: two ways to push your work

Everyone follows the **same** issue → branch → PR → CI → merge lifecycle
and the **same** branch-naming rules (below).  The only thing that differs is
**where your topic branch lives**, which depends on whether you have write
access to this repository.

| | **Contributor** (no write access) | **Collaborator** (write access) |
|---|---|---|
| Who | Anyone — this is the default. | Maintainers / trusted regulars (offered after a few good PRs). |
| Repo you push to | **Your fork** (`<you>/rtfreporter`). | **This repo** (`ichirio/rtfreporter`) directly. |
| Open the PR | From your fork's branch → `ichirio/rtfreporter:main` (a *cross-fork* PR). | From the branch → `main`, same repo. |
| Label / merge | A Collaborator does it for you. | Yourself. |

Both paths are standard **GitHub flow**; follow the row that matches your
access.

### Contributor workflow (fork → pull request)

```bash
# 1. Fork on GitHub (the "Fork" button), then clone YOUR fork:
git clone https://github.com/<you>/rtfreporter.git
cd rtfreporter

# 2. Add the canonical repo as "upstream" (one time only):
git remote add upstream https://github.com/ichirio/rtfreporter.git

# 3. Before each new piece of work, sync your fork's main with upstream
#    (VS Code and browser routes: see "Keep your fork and clone up to date"):
git checkout main
git fetch upstream
git merge --ff-only upstream/main      # fast-forward only; no local commits on main
git push origin main                   # keep your fork's main current

# 4. Cut a topic branch (see naming rules below) and do the work:
git checkout -b <type>/<issue>-<slug>
#   ... edit, then ...
git add -A
git commit -m "Short imperative summary (Closes #<issue>)"

# 5. Push the branch to YOUR fork and open the PR against upstream:
git push -u origin <type>/<issue>-<slug>
gh pr create --repo ichirio/rtfreporter --base main \
  --head <you>:<type>/<issue>-<slug>
#   (or use the "Compare & pull request" button GitHub shows after the push)
```

Keep `main` on your fork **pristine** (never commit to it directly) so the
fast-forward sync in step 3 always succeeds.  If your branch falls behind
`main` while in review, `git fetch upstream && git rebase upstream/main` on
the branch, then `git push --force-with-lease origin <branch>` (the topic
branch only, never `main`).  The agent's version of these steps, with conflict
handling, is *Git work for a contributor* in
`inst/ai/rtfreporter-ai-dev-manual.md`.

### Keep your fork and clone up to date

A fork does not update itself.  Sync it **before every new piece of work**,
and once the hard way if yours is far behind.  You can use the browser,
the VS Code Source Control view, or the command line.

#### (a) First time, or a fork that is far behind

> **Warning.**  These steps make your `main` equal to upstream's `main`.
> Any commit on your `main` that upstream does not have is **discarded**.
> To keep such work, first put it on a branch:
> `git branch my-old-work main`.

- **Browser.**  Open your fork on GitHub, click **Sync fork**, then
  **Update branch**.  If GitHub offers **Discard commits**, choosing it
  discards the fork's own commits on `main`.  Then pull `main` into your
  clone (see (b)).
- **VS Code.**  In Source Control, open the `...` menu, then
  **Remote** > **Add Remote...** (Command Palette: *Git: Add Remote...*),
  name it `upstream` and enter
  `https://github.com/ichirio/rtfreporter.git`.  Then *Git: Fetch From All
  Remotes*, *Git: Checkout to...* `main`, and *Git: Merge Branch...*
  `upstream/main`.  VS Code has no hard reset; if your `main` has commits
  of its own, use the terminal (**Terminal** > **New Terminal**) for the
  command-line steps below.  Finish with **Push**.
- **Command line.**

  ```bash
  git remote add upstream https://github.com/ichirio/rtfreporter.git
  git fetch upstream
  git checkout main
  git reset --hard upstream/main
  git push origin main --force-with-lease
  ```

#### (b) Every time before new work

- **Browser.**  On your fork, **Sync fork** > **Update branch**.  Then in
  VS Code check out `main` and **Pull** (`...` menu > **Pull**).
- **VS Code.**  The same as the command line: *Git: Checkout to...* `main`;
  *Git: Fetch From All Remotes*; *Git: Merge Branch...* `upstream/main`;
  **Push** (`...` menu > **Push**); then *Git: Create Branch...* with your
  new branch name.
- **Command line.**

  ```bash
  git checkout main
  git fetch upstream
  git merge --ff-only upstream/main
  git push
  git checkout -b <type>/<issue>-<slug>
  ```

If you have the `gh` CLI, `gh repo sync <you>/rtfreporter` updates the fork
on GitHub (optional); then `git pull` in your clone.

### Collaborator workflow (direct branch)

With write access you skip the fork and the `upstream` remote — `origin`
already *is* `ichirio/rtfreporter`:

```bash
git clone https://github.com/ichirio/rtfreporter.git
cd rtfreporter
git checkout main && git pull
git checkout -b <type>/<issue>-<slug>
#   ... edit / commit ...
git push -u origin <type>/<issue>-<slug>
gh pr create --base main               # head is the branch you just pushed
```

`main` is protected: a change reaches it only through a pull request, once the
required checks pass (see *Continuous integration*).  A maintainer reviews and
approves a contributor's pull request.  Releases — minor/major version bumps, tags, GitHub
Releases — are a Collaborator-only action (see *Versioning & releases*).

## Issue → merge lifecycle

```
issue ──▶ discuss / agree approach ──▶ branch off main ──▶ open PR
   ▲                                                          │
   │                                                          ▼
   └──────────────── (a check fails) ◀────── tests + all of CI
                                                              │
                                                              ▼
                                                      merge to main
                                                       (issue closed
                                                        via "Closes #N")
```

* **Issue** — describe the problem/idea; the development team evaluates it,
  and once it is decided the issue's status changes (its labels: `bug`,
  `enhancement`, `good first issue`, `exec:*`, ...).
* **Branch + PR** — a collaborator branches in this repository, a contributor
  in a fork (see *Branching & collaboration* below).  Run the tests, then open
  the PR.  Reference the issue with `Closes #N` so it auto-closes on merge.
* **CI** — CI runs the tests of the change and the regression tests, on every
  environment; the required checks must pass (see *Continuous integration*).
  A maintainer reviews and approves a contributor's pull request.
* **Merge** — the maintainer merges to `main`; the branch is deleted.

## Execution-disposition labels (`exec:*`)

Issues carry two independent kinds of label.  The **type** labels (`bug`,
`enhancement`, `documentation`, …) say *what* the issue is.  The **`exec:`**
labels say *how it should be acted on* — in particular whether the project's
coding agent may pick it up.  The `exec:` prefix and a shared colour family keep
the two axes from being confused; at most one `exec:*` label applies at a time.

| Label | Meaning | Agent behaviour |
|-------|---------|-----------------|
| `exec:agent`  | Pre-approved for the agent to implement. | The agent works the issue (issue → branch → PR). |
| `exec:human`  | A human will do this; the **assignee** names who. | The agent does **not** act. |
| `exec:hold`   | Pending a maintainer decision. | The agent does **not** act; it only reports that the issue exists. |
| `exec:wontdo` | Decided not to implement. | Ignored (or proposed for closing). |

An issue with **no** `exec:*` label is treated like `exec:hold`: the agent
reports it but does not start work.  To hand a freshly-filed (e.g. web-created)
issue to the agent, add `exec:agent`.

The `exec:*` and `priority:*` labels guide the maintainers' own coding agent;
an outside contributor needs none of them and may take any issue a maintainer
has pointed them to.

**Future agents.**  While there is a single agent, `exec:agent` is unambiguous.
When more than one agent exists, split it into suffixed labels
(`exec:agent-claude`, `exec:agent-codex`, …) so each agent picks up only its own.

## Reporting bugs

Open an issue at
<https://github.com/ichirio/rtfreporter/issues> and include:

- A short description of what you expected vs. what happened.
- A **minimal reproducible example** (a `reprex::reprex()` is ideal):
  the smallest self-contained snippet that triggers the problem.
- The output of `sessionInfo()` (R version, OS, package versions).
- For rendering problems, the offending `.rtf` (or the code that
  produces it) and, where possible, a screenshot of how it opens in
  Word / LibreOffice.

Please search existing issues first to avoid duplicates.

## Requesting features

Feature requests are welcome as issues. Clinical TFL use cases are the
primary focus, so describing the real-world table/listing/figure you are
trying to produce helps a lot.

## Pull requests

1. **Discuss first** for anything non-trivial — open an issue so we agree
   on the approach before you invest time.
2. Fork the repository and create a topic branch off `main`.
3. Follow the existing code style (see below).
4. Add or update **tests** under `tests/testthat/` for any behaviour
   change, and **documentation** (roxygen2 comments) for any exported
   function.  Explain a feature directly; do not compare it with another
   package (naming a package is fine where it is the actual subject).
5. Make sure the checks pass locally.  These are *the four pre-push
   checks*; run `devtools::document()` first (NAMESPACE is hand-managed),
   then all four, and report the results in the PR description:

   ```r
   devtools::document()      # regenerate man/*.Rd, commit them
   ```

   ```bash
   R CMD build . && R CMD check --as-cran rtfreporter_*.tar.gz  # 0 errors / 0 warnings
   NOT_CRAN=true Rscript -e 'devtools::test()'   # full suite, nothing skipped on CRAN grounds
   Rscript -e 'pkgdown::check_pkgdown()'         # when exports, Rd or _pkgdown.yml change
   Rscript -e 'lintr::lint_package()'            # must be clean
   ```

6. **Record the change in the PR description.**  A PR never edits `NEWS.md`,
   `CHANGELOG.md` or `Version:`, and there is no separate memo file.  Fill the
   template's *User-visible change (for NEWS)* field without being asked: one or
   two bullets in NEWS style (what a user can now do, or what is fixed, named by
   its function or argument), or "none" for an internal change; then "Breaking:
   no", or "Breaking: yes" with the migration.  The maintainers write `NEWS.md`
   from the merged PRs at release.  For example:

   ```markdown
   ## User-visible change (for NEWS)

   - `rtfplot()` and `rtf_figures()` take `render_format = "emf"`: a plot object is
     drawn as EMF with devEMF (in Suggests), at `render_width` x `render_height`
     inches.

   Breaking: no
   ```

   The maintainers bump the version when they tag or release.
7. Open the pull request against `main` with a clear description and a
   reference to the related issue (e.g. `Closes #12`).

## Non-code contributions: rendered-output snapshots

Not every contribution is code.  Docs, tests, and **screenshots of how a
generated `.rtf` opens in Microsoft Word** are all welcome — the showcase
articles (`vignettes/articles/showcase-dm.Rmd`, `showcase-ae.Rmd`) display a
real Word rendering of each example beside the code that produced it.  This is
a great first contribution and needs no R toolchain.

**How the snapshots are wired.**  Each example commits its `.rtf` to
`inst/rtf-examples/showcase/` — these are the generated *source of truth*
(refreshed by `data-raw/showcase_dm.R` / `showcase_ae.R`).  A matching `.png`
sits next to it; the article's `.snapshot("<name>.png")` helper looks the PNG
up **by exact filename** and embeds it, falling back to a "not captured yet"
placeholder when the PNG is still a stand-in.  So a snapshot contribution is
simply: **replace the placeholder PNG with a real Word screenshot of the same
basename.**

**To capture and contribute a snapshot:**

1. Sync your fork and cut a branch (Contributor or Collaborator flow above).
   Snapshots are part of the long-running showcase effort (tracker **#146**),
   so reference it with **`Refs #146`** (not `Closes`) and scope the branch,
   e.g. `docs/146-showcase-snapshots`.
2. For each example, open its `.rtf` from `inst/rtf-examples/showcase/` in
   **Word** and screenshot the rendered page.  Keep captures consistent:
   - 100 % zoom, white page background, no ruler/ribbon in frame;
   - crop to the page content (table, titles, footnotes), not the Word chrome;
   - if a table runs across several pages (some AE examples do, with a
     `(Cont.)` continuation), capture the page(s) the article describes.
3. Save each screenshot as a **PNG with the identical basename** as the
   `.rtf`, overwriting the placeholder — e.g. `dm_gtsummary.rtf` →
   `dm_gtsummary.png`.  Do **not** rename: a mismatch silently falls back to
   the placeholder.
4. Commit only the PNGs and open the PR:

   ```bash
   git add inst/rtf-examples/showcase/*.png
   git commit -m "Capture Word snapshots for showcase examples (Refs #146)"
   git push -u origin docs/146-showcase-snapshots
   # Contributor: gh pr create --repo ichirio/rtfreporter --base main \
   #                --head <you>:docs/146-showcase-snapshots
   # Collaborator: gh pr create --base main
   ```

For a **pure snapshot** contribution you do *not* regenerate the `.rtf` files
or touch R code — they are already committed.  (Only if an example's *content*
were wrong would you edit the article and rerun its `data-raw/` driver, which
is a separate, code-level change.)  CI does not build the articles under
`R-CMD-check`, but the **pkgdown** workflow does — so a corrupt or mis-named
PNG surfaces there; check that workflow goes green on your PR.

The current showcase set is **6 DM** examples (`dm_gtsummary`,
`dm_gtsummary_ard`, `dm_rtables`, `dm_tfrmt`, `dm_tfrmt_rtfreporter`,
`dm_tplyr`) and **7 AE** examples (`ae_tern`, `ae_gtsummary`,
`ae_gtsummary_ard`, `ae_tplyr`, `ae_tfrmt`, `ae_flextable`, `ae_huxtable`).

## Branching & collaboration

The project uses a simple **GitHub-flow** model — a single long-lived
branch plus short-lived topic branches.  This scales cleanly from one
contributor to several.

- **`main` is the single source of truth.**  It must always be
  releasable: green on the required checks and `0 errors / 0 warnings` on
  `R CMD check`.  The
  current *development version* lives here.
- **`next` gathers the maintainers' work that is not urgent.**  Their
  topic branches go to `next` once their own checks pass; `next` goes to
  `main` about once a week, by a pull request (a merge commit) that runs
  every CI workflow, on every environment.  A PR into `next` runs `lint` and
  the tests on Ubuntu (R release); every environment runs on `next` itself,
  before it reaches `main`.  An urgent fix goes to `main` as a small PR of its
  own, which runs them all, as a contributor's PR does.
  Contributors open pull requests against `main`.  The `next` branch, the
  `automerge` label and `merge-after:` lines are the maintainers' own
  workflow; a maintainer adds any label that is needed.
- **No direct commits to `main`.**  `main` is protected: a pull request is
  required, the required checks (`ubuntu-latest (release)`,
  `ubuntu-latest (devel)`, `ubuntu-latest (oldrel-1)`, `macos-latest
  (release)`, `windows-latest (release)`, `pkgdown` and `test-coverage`) must
  pass before merge, and a maintainer reviews and approves a contributor's
  pull request.
- **One topic branch per change**, cut from the latest `main`.  Name it
  **`<type>/<issue>-<slug>`** -- lowercase, hyphen-separated, short
  (3-5 words):

  ```
  fix/42-empty-cell-newline       feat/57-read-tfrmt-tables
  docs/60-contributing-guide      refactor/12-gt-adapter
  ```

  * **`<type>`** uses the Conventional Commits vocabulary:
    `feat`, `fix`, `docs`, `chore`, `refactor`, `test`, `perf`, `ci`.
  * **`<issue>`** is the related issue number (omit only for quick
    exploratory work with no issue yet; add it once an issue exists).
    Note: use the bare number -- `#` is not allowed in branch names.
  * **`<slug>`** is a couple of words describing the change.

  Tip: GitHub's *Create a branch* button on an issue (the Development
  panel) generates a correctly-named, auto-linked branch for you.  Either
  way, put `Closes #<issue>` in the PR description so the issue closes on
  merge.

- **Umbrella (tracker) issues spanning several PRs.**  A large effort -- a
  multi-cut article, a docs sweep, a multi-batch refactor -- may land as
  **several PRs under one issue**, with that issue kept **open as a
  tracker** (precedents: #150 -> PR #151 + #153; #146 -> PR #147 + later
  cuts).  When you do this:

  * Each interim PR references the tracker with **`Refs #<issue>`** (not
    `Closes`), so merging it does **not** close the tracker.  The **final**
    PR may use `Closes #<issue>` (or close it by hand) once the whole scope
    is done.
  * Keep a **checklist in the issue body** of what is still outstanding, and
    add a short **dated progress comment** each time a PR lands, so the
    record survives even as the plan evolves.
  * Give each cut's branch a **scope segment** so sibling branches/PRs stay
    distinct: **`<type>/<issue>-<scope>-<slug>`** -- e.g.
    `docs/146-showcase-dm`, `docs/146-showcase-ae` -- and mirror the scope in
    the PR title (`Showcase (DM): ...`).

- **Keep branches small and short-lived.**  Rebase (or merge) the latest
  `main` into your branch regularly so the eventual PR is a small,
  reviewable diff with no stale conflicts.
- **Regenerate and commit derived files.**  Run `devtools::document()`
  and commit the updated `man/*.Rd` in the same PR.  `NAMESPACE` is
  **hand-managed** — edit it deliberately, do not let a tool overwrite it.
- **Regenerate `inst/rtf-examples/` from the package root.**  The generators
  in `data-raw/` `source("data-raw/_load.R")`, which attaches the package
  **from the working tree** via `pkgload::load_all()` rather than from
  whatever is installed:

  ```r
  Rscript data-raw/gen_tlg_catalog_rtf.R      # from the package root
  ```

  Never regenerate a committed example through `library(rtfreporter)`.  An
  installed build that lags the working tree renders old code against new
  resources, which is how six examples came to ship with literal
  `{orientation_cmd}` in their preamble (#306).
- **Merging.**  Squash- or merge-commit per the maintainer's preference;
  make sure the PR title/commit message is meaningful.  The branch is
  deleted after merge.
- **Releases are cut from `main`** by the maintainer via tags — see
  *Versioning & releases* below.  Day-to-day contributors do not tag or
  publish releases.
- **(Optional) maintenance branches.**  Only if an older major version
  must be patched after a newer major has shipped, create a long-lived
  `release/v<MAJOR>` branch for back-ports.  This is not needed during
  normal single-line development.

## Continuous integration (GitHub Actions)

These workflows run on pushes and pull requests (under
`.github/workflows/`).  A PR into `main` merges when the required checks are
green, a contributor's once a maintainer has also reviewed and approved it;
`lint` and `version-guard` also run on it.

CI runs when the pull request is opened, not on pushes to the branches of
your fork.  The workflows of a first-time contributor's PR wait until a
maintainer approves them.

| Workflow | File | What it does |
|----------|------|--------------|
| **R-CMD-check** | `R-CMD-check.yaml` | `R CMD check` on a matrix of OS / R versions (the standard `r-lib/actions` recipe), and one more job, the *minimum environment*: R and each dependency at the minimum `DESCRIPTION` names, the rest from CRAN as on a dated Posit Package Manager snapshot (the release jobs are the *recommended environment*, the latest R with the latest packages) (`inst/validation/dependency-minimums.md` says why each is what it is).  Must be 0 errors / 0 warnings. |
| **test-coverage** | `test-coverage.yaml` | Runs the testthat suite under `covr` and uploads coverage to Codecov. |
| **pkgdown** | `pkgdown.yaml` | Builds the documentation site and (on `main` / on a published release) deploys it to the `gh-pages` branch. |
| **version-guard** | `version-guard.yaml` | Fails a PR that raises the **MINOR or MAJOR** position of `DESCRIPTION` `Version:` unless the PR carries the `release` label (see *Versioning & releases*). A PATCH bump or no change always passes. |
| **lint** | `lint.yaml` | Runs `lintr::lint_package()` and fails on any lint. Scope and rules are set by the repository-root `.lintr` (currently lints `R/` only). |
| **quality-report** | the last job of `R-CMD-check.yaml` | Only for a published release (and by hand): gathers every environment's record and writes the Quality Report, attached to the release (see *Quality assurance*). A run by hand writes a DRAFT, kept as the run's artifact. Never on a push or a PR. |

R-CMD-check, test-coverage, pkgdown and lint also accept `workflow_dispatch`
(run-on-demand from the Actions tab).  Releases additionally re-trigger
pkgdown and R-CMD-check (with its quality-report job) on `release:
published`.
Before pushing, reproduce CI locally with `devtools::document()` and the
four pre-push checks (see *Pull requests*).

## Project tracking (GitHub Projects / labels)

Backlog and progress are tracked on GitHub so that, as the project grows
beyond a single maintainer, anyone can see *what is planned, what is in
flight, and who is acting on it* at a glance.  Three mechanisms work
together; each answers a different question.

### The three axes

| Mechanism | Answers | Source of truth for |
|-----------|---------|---------------------|
| **Labels** | *What kind* of work, and *who may act* | triage + agent dispatch |
| **Milestones** | *Which release* it targets | release planning |
| **Project board** | *What stage* it is at right now | day-to-day progress |

* **Labels.**  Two independent families (see *Execution-disposition labels*
  above): the **type** labels (`bug`, `enhancement`, `documentation`,
  `good first issue`, `help wanted`) and the **`exec:*`** labels
  (`exec:agent` / `exec:human` / `exec:hold` / `exec:wontdo`).  The `exec:*`
  label remains the **authoritative dispatch signal** — the board visualises
  status but never overrides who is allowed to start work.
* **Milestones** group issues for a target release (e.g. `v0.1.0`),
  mirroring the major/minor entries in `NEWS.md` / `CHANGELOG.md`.

### The board — "rtfreporter roadmap"

A single org/user-level **GitHub Projects (v2)** board gives the kanban view.
Issues *and* PRs are added as items; the board is for coordination only — the
record of *what shipped* stays in `NEWS.md` / `CHANGELOG.md`.

**Status** (single-select; the columns):

| Status | Meaning | Typical label/PR state |
|--------|---------|------------------------|
| **Backlog** | Captured, not yet scheduled. | no `exec:*`, or `exec:hold` |
| **Ready** | Approved and scoped; ready to pick up. | `exec:agent` / `exec:human` (assignee set) |
| **In progress** | Someone (or the agent) is actively working it. | branch exists |
| **In review** | PR open, CI running or green, awaiting merge. | open PR |
| **Blocked** | Needs a decision or an upstream fix. | `exec:hold` |
| **Done** | Merged / closed. | issue closed via `Closes #N` |

**Custom fields** (so the board can be sliced):

* **Priority** — `P0` (urgent) / `P1` (normal) / `P2` (someday).
* **Area** — `renderer` / `adapters` (gt, rtables/tern) / `pagination` /
  `borders` / `docs` / `infra-ci`.
* **Agent** — `claude` / `codex` / `human`.  This mirrors the planned
  `exec:agent-<name>` split so multi-agent dispatch and the board stay in
  sync.

**Built-in automation** (Project *Workflows* tab — no code):

* *Item added to project* → set **Status = Backlog**.
* *Issue/PR opened* (auto-add) → added to the board.
* *Pull request opened* → **Status = In review**.
* *Issue or PR closed* → **Status = Done**.

### One-time setup (maintainer)

Creating the board needs the `project` scope, which the default `gh` login
does not carry.  A maintainer runs this **once**:

```bash
gh auth refresh -s project,read:project          # grant the scope

# create the board and capture its number
gh project create --owner ichirio --title "rtfreporter roadmap"

# add the custom single-select fields (repeat --single-select-option per value)
gh project field-create <N> --owner ichirio --name Priority \
  --data-type SINGLE_SELECT --single-select-options P0,P1,P2
gh project field-create <N> --owner ichirio --name Area \
  --data-type SINGLE_SELECT \
  --single-select-options renderer,adapters,pagination,borders,docs,infra-ci
gh project field-create <N> --owner ichirio --name Agent \
  --data-type SINGLE_SELECT --single-select-options claude,codex,human
```

Then, in the board's web UI, enable **Workflows** (the four automations
above) and turn on **auto-add** for the `rtfreporter` repository.  Existing
open issues can be bulk-added from the board's *＋ Add items* search.

Until the board exists, labels + milestones already give a usable backlog
view via the Issues tab (filter by `exec:agent`, by milestone, or by type).

## Quality assurance: requirements, tests, traceability

What GitHub already records -- issues, pull requests, the CI runs, the
releases -- is not written again; this section says only what it does not.

- **Functional Requirements (FR)** are `inst/validation/requirements.csv`,
  one row a requirement, a hierarchy: each row names its `parent`
  (`FR-PL000` the plan verbs > `FR-PL004` plan_blanks() > `FR-PL182` ...: an
  id is short, the area's two letters and a number of three or four digits,
  the next free one in its area; the `parent` column
  holds the hierarchy, and a number once given is never reused).  A parent
  is of kind `group`.  Tests are mapped to the leaves; a parent is covered
  when every leaf under it has a test.  A user's requirement may be traced
  to an FR at any level.  The leaves are finer than a function: each
  function has leaves of five kinds --
  - `behaviour`: each thing it does;
  - `argument`: each argument, and each meaningful value or combination;
  - `data`: each data shape it is meant to handle where that matters (no
    rows, NA, factor levels, one or many groups, long or non-ASCII text);
  - `guard`: each unintended use it stops -- a wrong argument, type or a
    contradictory combination -- and the error or warning it gives;
  - `interaction`: how functions or arguments act together, where that is
    a risk -- a precedence or override, an argument that changes another's
    meaning, a combination that once had a bug, a scenario from data to RTF
    (compared byte for byte).  Not every combination: these, under their
    own group `FR-IX000`, apart from the functions' rows.
  A PR that adds or changes a behaviour adds or changes its FRs in the same
  PR, with `since` the next planned release (`0.9.0` now); an FR is never
  renumbered, a withdrawn one is `superseded` or
  `removed` (`draft`: a coarse row not yet split, not counted).
- **Functional Tests (FT)** are the `test_that()` blocks that check an FR,
  as the developer making the change decided -- not a block that merely calls
  the function.  A new test names its id and FRs at its head
  (`"FT-PL<nnn> [FR-PL157 FR-PL159]: plan_columns(widths = ) ..."`, three or four
  digits, the
  next unused FT number in its area);
  an existing one is mapped by hand in `inst/validation/traceability_matrix.csv`.  An
  FR no test checks is a deviation until a test is added.
  `validation/trace.R` lists the blocks that call a function in scope but
  are not mapped (candidates to read); it never counts them.
- **What a PR that adds or changes a behaviour does**, in that PR, for a
  contributor or an AI agent alike:
  - add or change the FRs in `inst/validation/requirements.csv`;
  - give every new or changed leaf FR a test.  Name the FT id and FRs at the
    head of a new test; add a row to `inst/validation/traceability_matrix.csv` (file,
    test, ft, fr) only for an existing test, never for both: an FT id given
    twice fails;
  - check every new mapping: read the FR and the test, and add a row to
    `inst/validation/traceability_review.csv` (`ft`, `fr`, `verdict`
    `asserts` or `part`, `evidence` the expectation lines that assert it,
    `fr_covered` `yes` when the FR's tests together assert all it states).
    A test that asserts none of an FR is not mapped to it, and a mapping
    with no row fails `test-validation-leaves.R`.  A new row goes to its
    place, never to the end: by `fr` in the order of `requirements.csv`,
    then by `ft` with the number read as a number (`FT-AR063` before
    `FT-AR3000`); `inst/validation/reference_review.csv` is kept by `file`
    the same way.  `test-validation-table-order.R` checks both, so two PRs
    that add rows change different lines;
  - name every new argument in a leaf FR of its function, as `arg =` or
    bare inside `f(...)`; the plain word in prose does not count, and an
    argument without such an FR fails `test-validation-coverage.R`;
  - where a test is not possible, add a row to `inst/validation/deviations.csv`
    (`ids`, `item`, `risk`, `risk_basis`, `reason`, `action`) with the risk
    and the reason; an FR with neither a test nor a deviation fails
    `test-validation-leaves.R`, and a deviation for an FR that has a test
    is removed;
  - keep `Rscript validation/trace.R` running and `test-validation.R`,
    `test-validation-leaves.R` and `test-validation-coverage.R` passing.
- **The Traceability Matrix (FR -> FT)** is the mapping alone, made by
  `validation/trace.R`; it holds no results.  The tests' results are the CI
  record; a failing test fails the PR's checks, so it cannot be merged.
- **Regression**: every PR runs the whole test suite and R CMD check (CI).
  A PR that changes the RTF written says so: it compares the reference
  outputs byte for byte with `main` and lists, in its description, the
  outputs it changes on purpose; every other output is identical.
  The validation scenarios (FR-IX131 .. FR-IX134) are such references under
  `tests/testthat/fixtures/`: their tests compare the file written now with
  the one kept, line for line; a deliberate change regenerates them
  (`tests/testthat/helper-validation-scenarios.R` says how).
- **At a release**, each R-CMD-check job writes its environment
  (`validation/env.R`: sessionInfo, the dependencies' versions, R CMD
  check's result, the tests' log), and the workflow's last job gathers them
  with the maintainers' local runs (`validation/env-local/`, the same
  script run by hand) into the Quality Report: one table of
  every environment, the results, the deviations and why they are
  accepted, the conclusion, how to read the attachments.  The PDF, the
  Traceability Matrix (`traceability_matrix.xlsx`), every environment's
  sessionInfo and log (`environments.txt`), the mapped tests' log
  (`test-log.txt`) and the tables behind the results (`ft.csv`,
  `functions.csv`, `candidates.csv`) are attached to the release, each name
  carrying the tag (an artifact would be deleted after 90 days).  Only this
  run concludes *validation-ready*; a run by hand, in CI or on a PC
  (`Rscript validation/report.R`), writes the same report marked DRAFT.

## Code style

- **S3 throughout**; the package has zero hard runtime dependencies
  (only `methods`, which ships with R). Optional integrations (gt,
  gtsummary, rtables/tern, ...) live in `Suggests` and must be guarded
  with `requireNamespace(...)`.
- twips are the only internal length unit.
- Keep R source ASCII-only (use `\uXXXX` escapes); non-ASCII is fine in
  comments and roxygen but not in code.
- Two-space indentation, `<-` for assignment, and `snake_case` for
  public functions; internal helpers are prefixed with a dot
  (`.helper_name`).
- **Lint** with [`lintr`](https://lintr.r-lib.org/): run
  `lintr::lint_package()` before opening a PR (the **lint** CI workflow runs
  the same check and must pass). The enforced rule set lives in the
  repository-root `.lintr`. It is intentionally a baseline: several purely
  stylistic linters (line length, indentation, braces, ...) are deferred to a
  future dedicated formatting pass — see the comments in `.lintr` — so adding
  them back is a follow-up, not a blocker today.
- See
  [`AGENTS.md`](https://github.com/ichirio/rtfreporter/blob/main/AGENTS.md)
  for the repository layout and the internal architecture, and
  [`LEARNING.md`](https://github.com/ichirio/rtfreporter/blob/main/LEARNING.md)
  for the design rationale.

## Development setup

Use R 4.3 or later (the oldest version CI tests).

```r
# tools for the pre-push checks; they are not in Suggests
install.packages(c("devtools", "lintr", "pkgdown", "rcmdcheck", "spelling"))
devtools::install_dev_deps()   # installs Suggests used by tests/vignettes
devtools::load_all()           # load the package for interactive work
```

## Versioning & releases

rtfreporter follows the **standard R versioning scheme**: a released version
has three components, a development version has a fourth.

| | Form | Example | Who changes it |
|---|---|---|---|
| **Release** | `MAJOR.MINOR.PATCH` | `0.8.0` | a release action — Collaborators, `release` label |
| **Development** | `MAJOR.MINOR.PATCH.9000` | `0.8.0.9000` | any PR, freely |

The three-component part always names **the last release**, so `0.8.0.9000`
reads as *"in development, after 0.8.0"*.  `packageVersion("rtfreporter")`
therefore answers a question worth asking — which release am I on, and am I
ahead of it — and an ordinary PR never touches it.

This is what `usethis::use_dev_version()` produces and what
[r-pkgs.org](https://r-pkgs.org/lifecycle.html) describes; it is chosen so that
contributors, CRAN and the tooling all read our numbers the same way.

> **Gotcha:** the `DESCRIPTION` `Version:` field must be **digits and dots
> only**.  A suffix such as `0.8.0-rc` makes R raise a `Malformed package
> version` error.  The `-rc` style, if ever needed, belongs only on the git tag
> / GitHub Release name.

> **Versions only ever increase.**  R compares them field by field, so
> `0.4.0.9000 < 0.7.56`; going backwards silently stops
> `install_github()` from seeing updates.  `version-guard` refuses it.

### What each position means

- **Development version — `vX.Y.Z.9000`.**  The rolling, in-progress version
  that lives on `main` between releases.  Bug fixes, internal refactors,
  documentation and new work all land here first.  Installable **only from
  GitHub** (`remotes::install_github("ichirio/rtfreporter")`); **never**
  submitted to CRAN.
- **Patch release — `vX.Y.Z` (Z ≥ 1).**  A published release containing fixes
  only, no new API.
- **Minor release — `vX.Y.0` (Y ≥ 1).**  A published release adding features
  that are **backward compatible within the same major version**.  A minor may
  *deprecate* a function (it keeps working and warns) but must **never remove
  or break** existing public behaviour.  Documented in `NEWS.md` and
  `CHANGELOG.md`, tagged, given a GitHub Release, and (once the package is on
  CRAN) submitted to CRAN.
- **Major release — `vX.0.0`.**  A published release that **may contain
  breaking changes**, and the **only** place where functions deprecated in
  earlier minors may be removed.  Every breaking change is documented under a
  "Breaking changes" heading in `NEWS.md`.
  - **`v1.0.0` specifically** is cut once user feedback and any resulting
    spec changes have settled and the public API is worth declaring stable.
    At v1.0.0 the **`lifecycle: experimental` badge is removed** and any "the
    API may change" wording is dropped.  See *Pre-1.0 is different* below.

### Backward-compatibility contract

- Within one major version, **no minor or patch release breaks user code.**
- Deprecation lifecycle: *deprecate in a minor* (function still works + warns)
  → *remove only in the next major*.
- **A new version of a dependency** (an import, a suggested package, or R):
  when rtfreporter can follow it and still work with the versions it
  supported before, that is a **minor** release; when it cannot, and the new
  version must be required (a minimum in `DESCRIPTION` is raised), that is a
  **major** release.  The minimums and their reasons are in
  `inst/validation/dependency-minimums.md` (the Quality Plan, section 6).
- **Except while `MAJOR` is 0** — see below.

### Pre-1.0 is different

**`MAJOR 0` means the package is not formally released.**  This is semver's
own reading of a zero major, and it is the one rtfreporter takes: within
`0.y.z` the contract above does not bind, and a MINOR may remove or change
public API.  Every such change is still documented under *Breaking changes*
in `NEWS.md` — the exemption is from the version-number rule, not from telling
people.

That is what makes the plan below legal:

| | |
|---|---|
| **v0.9.0** | Removes the deprecated exports (the seven border constructors and the others deprecated in 0.8.x); `R CMD check` clean, coverage, docs polish.  Under a 1.x contract removing them would require a major; at `0.y.z` a minor may. |
| **v1.0.0** | A stable API, once feedback and any resulting spec changes have settled.  From here the contract binds for real: no minor or patch may break user code again. |

The order is deliberate: users' feedback tells you whether the API is worth
freezing; declaring 1.0.0 first would be promising stability before anyone had
tried it.

### Procedure — an ordinary development PR

**An ordinary PR never touches `Version:` or `NEWS.md`.**

The development counter — the fourth position — signals *"depend on this"*.
The maintainers raise it when something downstream needs to name a
development state, as part of how they release.  `version-guard` accepts a PR
with no version change.

1. Leave `Version:` unchanged.  The maintainers set the version when they
   tag or release.
2. Leave `NEWS.md` and `CHANGELOG.md` unchanged.  Fill the template's
   *User-visible change (for NEWS)* field (see *Pull requests*); the
   maintainers write `NEWS.md` from the merged PRs at release.
3. Run `devtools::document()` and the four pre-push checks (see *Pull
   requests*).
4. No git tag, no GitHub Release, no CRAN submission for a development bump.

> **Enforced by CI.**  `version-guard` fails any PR that changes `X`, `Y` or
> `Z` without the `release` label, and any PR that lowers the version.  A
> release is therefore a deliberate, labelled action — never an accident of a
> merge.

Because an ordinary PR usually leaves the version alone, **two open PRs no
longer collide over the same field** — the problem the old "bump exactly one
PATCH per PR" rule created for itself.  If two PRs do both bump the counter,
the second to merge rebases and re-bumps.

### Releases are Collaborator actions

Raising `MAJOR`, `MINOR` or `PATCH` is *not* part of an ordinary contribution
PR.  It is a release, started from a dedicated **release Issue** and carried
out with the procedure below.  **At present only repository Collaborators may
open a release Issue and perform a release.**

### Procedure — cutting a release (`vX.Y.Z`)

1. Confirm `main` is green on all CI workflows and `devtools::check()` is
   `0 errors / 0 warnings`.
2. In `NEWS.md`, rename the `(development version)` heading to
   `# rtfreporter X.Y.Z` and tidy the notes.  Update `CHANGELOG.md` for a
   minor or major release.
3. Set `DESCRIPTION` → `Version: X.Y.Z` — dropping the `.9000`.
4. Refresh docs (`devtools::document()`), update the README roadmap / badges
   if needed, and confirm the pkgdown site builds.
5. Commit as `release: vX.Y.Z`, open a PR **with the `release` label**, and
   merge once CI is green.
6. Tag and publish from the merge commit:

   ```bash
   git tag vX.Y.Z
   git push origin vX.Y.Z
   gh release create vX.Y.Z --title "rtfreporter X.Y.Z" \
     --notes-file <notes-from-NEWS> --latest
   ```

   Publishing the release runs R-CMD-check's quality-report job, which
   attaches the Quality Report and its attachments to it.  Before step 5,
   on the release branch (`Version: X.Y.Z`), run
   `Rscript validation/env.R "<OS>, R <version>" local` on the
   maintainers' PC for each R version of the Quality Plan and commit
   `validation/env-local/` in the release PR: the report counts only the
   records of the version it reports.
7. **(Once on CRAN)** run the CRAN pre-checks and submit —
   `devtools::check_win_devel()`, `urlchecker::url_check()`,
   `devtools::release()`.
8. **Open the next development cycle** in a follow-up PR: set `DESCRIPTION` to
   `X.Y.Z.9000` and add a fresh `# rtfreporter (development version)` heading
   to `NEWS.md`.  (`usethis::use_dev_version()` does both.)

### Procedure — an urgent fix while `main` carries a feature

The awkward case: `main` sits at `X.Y.Z.9000` with a finished feature on it,
and a bug needs a release **now**.

**There is no such thing as a patch that contains a feature.**  Releasing
`main` releases the feature too, and that is a MINOR.  So the question is not
"how do I get a patch out" but:

> **Is the feature's API ready to freeze?**

Once it is on CRAN you owe it backward compatibility, so an argument name or a
default that is still under discussion is a reason to hold it back — not the
size of the change, and not how finished the code is.  `main` is green by
definition; that was never the risk.

| | |
|---|---|
| **API settled** | Release `main` as a **MINOR**.  Ship both, say so in `NEWS.md`.  This is the normal answer: CRAN wants at most a release every month or two, so batching a fix with a feature is what the cadence is *for*. |
| **API not settled** | Cut a **hotfix** from the release tag, below. |

A hotfix also costs downstream less, which matters here more than in most
packages: a validated clinical pipeline re-validates on every upgrade, and
"this release changed one file" is a far cheaper conversation than "this
release changed forty".

```bash
git checkout -b hotfix/0.8.1 v0.8.0     # from the TAG, not from main
git cherry-pick <the fix>
#   DESCRIPTION -> 0.8.1
#   NEWS.md     -> `# rtfreporter 0.8.1`, that one fix and nothing else
#   open the PR WITH the `release` label, merge, then:
git tag v0.8.1 && git push origin v0.8.1
gh release create v0.8.1 --title "rtfreporter 0.8.1" --notes-file <notes> --latest
```

Then — **and this step is not optional**:

```bash
# merge the hotfix back into main and re-open the cycle on top of it
git checkout main && git merge hotfix/0.8.1
#   DESCRIPTION -> 0.8.1.9000
```

Skip it and `main` still reads `0.8.0.9000`, which is **lower than the version
you just released**:

```r
package_version("0.8.1") > package_version("0.8.0.9000")   # TRUE
```

`update.packages()` would then replace a development install with the release
build, silently removing the unreleased feature from that user's library, and
the "three-component part names the last release" invariant stays broken until
someone notices.  Moving `main` to `0.8.1.9000` restores both.

An out-of-cycle CRAN submission is acceptable when it is a genuine user-facing
fix; say so in `cran-comments.md` rather than letting the maintainers wonder.

### Procedure — major release (`vX.0.0`)

Everything in the release procedure, **plus**:

1. **Remove** functions deprecated in earlier minors, and describe each removal
   and its migration path under "Breaking changes" in `NEWS.md`.
2. Audit for and document any other breaking change.
3. For **v1.0.0**: remove the
   `lifecycle: experimental` badge (set the lifecycle to *stable*) in the
   README and `DESCRIPTION`, and drop any "experimental / API may change"
   wording.

### History — how the numbering got here

Until v0.8.0 the rule was *"each pull request raises the development version by
exactly one PATCH"*.  That made the PATCH position a build counter rather than
a patch-release number, and it ran away: 146 PRs after v0.4.0 the development
version read `0.7.56`, having passed through `0.4.84`, `0.5.0`, `0.5.1` and
`0.6.0` — consuming the very numbers the roadmap had reserved for
*CRAN-submission preparation* and *CRAN registration*, without ever releasing
them.

`v0.8.0` is the release that closes that line.  From it, development continues
at `0.8.0.9000` under the rule above.  The numbers before v0.8.0 are left as
they are: R versions must increase, so the history cannot be renumbered, and
rewriting it would only invalidate the tags and installs that already exist.

### First release (`v0.1.0`) — one-time cleanup

Follow the minor-release procedure, and additionally **delete the
pre-v0.1.0 `*-alpha` tags and GitHub Releases** so the published history
starts clean at v0.1.0:

```bash
for t in v0.0.1-alpha v0.0.2-alpha v0.0.3-alpha v0.0.4-alpha r-v0.0.1; do
  gh release delete "$t" --yes        2>/dev/null || true   # delete the GitHub Release
  git push origin ":refs/tags/$t"     2>/dev/null || true   # delete the remote tag
  git tag -d "$t"                     2>/dev/null || true   # delete the local tag
done
```

From v0.1.0 onward, use clean `vX.Y.Z` tags only (no `-alpha` suffix, no
`r-` prefix), and mark the newest release `--latest`.

## Code of conduct

This project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By contributing, you agree to abide by its
terms.

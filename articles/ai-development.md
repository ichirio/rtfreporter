# Developing with an AI assistant (for contributors)

[In
Japanese](https://ichirio.github.io/rtfreporter/articles/ai-development-ja.md)

Most work on rtfreporter now happens with a chat assistant in the loop.
This article is how to do that well: what to give the assistant before
it writes anything, where its judgement is worth trusting, and where it
is not.

If you would rather work by hand, everything here is optional — skip to
[Working without an assistant](#working-without-an-assistant) at the
end.

## The problem it solves

rtfreporter is young and is not in any model’s training data. Asked for
rtfreporter code, an assistant reaches for the package it *does* know —
`r2rtf` — and produces `rtf_body()`, `rtf_colheader()`,
`create_table()`. Where it does reach for the right function it invents
plausible arguments. None of it is flagged as a guess, so you end up
checking every name against the reference, which is slower than writing
the code yourself.

## Step 1 — attach the developer manual

Download it and attach it as the **first** thing in the session:

> **[⬇
> rtfreporter-ai-dev-manual.md](https://ichirio.github.io/rtfreporter/ai/rtfreporter-ai-dev-manual.md)**

Then say, in as many words:

> Use this manual when working on the rtfreporter codebase.

It is one self-contained file of about 18 KB — roughly 5,000 tokens, so
it sits comfortably in a session alongside a long conversation. It
carries the invariants, the S3 object model, the rendering pipeline, the
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
kwargs contract, how to add a table-object adapter, the code / test /
docs conventions, a source-file map, and the traps that have actually
bitten.

### Attach one manual, not two

There is a second file, the [AI **user**
manual](https://ichirio.github.io/rtfreporter/ai/rtfreporter-ai-user-manual.md),
for people *using* the package to build reports. Do not attach both.

They carry opposite instructions: the user manual’s first rule is “only
the exported API, never touch internals”, and the developer manual
requires the opposite. Given both, an assistant arbitrates between them
and follows neither cleanly. Each file opens with a scope line saying
which it is, so check the one you attached is the one you meant.

## Step 2 — say which access level you have

The work is identical whether or not you can push to this repository;
only the git commands differ. The manual tells the assistant to check:

``` bash
gh api repos/ichirio/rtfreporter --jq .permissions.push   # true = Collaborator
```

but it is faster to say it up front — *“I’m working from a fork”* or *“I
have write access”* — so the branch and PR commands come out right the
first time.

## Step 3 — work the issue

The manual already knows the lifecycle, so a useful opening prompt is
short:

> Issue \#123 asks for X. Read the manual’s section 5, propose the
> change, and stop before writing any code.

Then, once you agree on the approach:

> Implement it. Branch `feat/123-slug`, update `R/`, the tests and the
> roxygen together, run `devtools::document()`, `devtools::test()` and
> [`lintr::lint_package()`](https://lintr.r-lib.org/reference/lint.html),
> add a `NEWS.md` entry, and open the PR with `Closes #123`.

Every item in that list is in the manual; naming them anyway is cheap
and makes the omissions obvious when you review.

Note what is *not* in it: a version bump. Under the standard R scheme
(`X.Y.Z` released, `X.Y.Z.9000` in development) an ordinary PR leaves
`DESCRIPTION` alone unless the change is one somebody needs to name, and
`version-guard` rejects a PR that touches `X.Y.Z` without the `release`
label. An assistant that “helpfully” bumps the version is making a
release, and will be told so by CI.

## What to trust, and what to check yourself

An assistant with the manual is reliable about **shape**: which function
to call, what the argument does, which file a change belongs in, what
the test should assert. Those are exactly the facts the manual carries.

It is not reliable about the things the manual cannot contain:

- **How the RTF actually renders.** Nothing in the pipeline proves a
  table looks right in Word — column widths, page breaks, whether a
  footnote collides with the bottom margin. Open the `.rtf`.
- **Clinical correctness.** Whether a denominator, a shift-table
  category or a footnote matches the SAP is a human judgement, and a
  confident wrong answer here is the expensive kind.
- **Whether the feature belongs in the package at all.** The scope cap
  (invariant 8) is a design position, not a rule that can be checked
  mechanically. An assistant will happily implement something that
  should have been declined.
- **API design.** Argument names and defaults are the part users live
  with longest; they deserve a human decision and an issue discussion.

A good split: let the assistant draft the change and the tests, and
spend your own attention on the rendered output and on whether the
design is right.

## Where the assistant stops

It opens the PR. It does not merge.

`main` requires seven green checks **and** one approving review, and
GitHub does not accept a self-approval — so an agent cannot legitimately
complete a merge on its own. The manual says so explicitly, and tells it
not to reach for an administrative override unless the repository owner
asks for one in that session.

Expect the assistant to report “the PR is open and CI is green” and
wait. That pause is the review, and it is the point.

## Why the manuals do not go stale

A stale manual is worse than no manual, because an assistant cannot tell
the difference — it will quote a function that was removed with exactly
the same confidence it quotes one that exists. So both manuals are
tested, and drift fails CI:

- the user manual’s function list must equal
  [`getNamespaceExports()`](https://rdrr.io/r/base/ns-reflect.html)
  **exactly** — no omission, no invented name;
- no deprecated export may appear in any code block, and every
  deprecated export must be documented as deprecated;
- every idiom the user manual teaches is executed against the package,
  including the ones it documents as *errors*;
- the developer manual’s source-file map, `Imports:` list and S3-only
  claim are checked against the tree.

See `tests/testthat/test-ai-user-manual.R` and `test-ai-dev-manual.R`.
If you change the public API, those tests are where you will be told
that the manuals need updating too — please update them in the same PR.

## Working without an assistant

Nothing above is required. The manual is a condensed briefing built
*from* the long-form documentation, and that documentation remains the
authority — when the two disagree, the articles win and the manual is
the bug.

Read these in order:

- [Architecture &
  internals](https://ichirio.github.io/rtfreporter/articles/architecture.md)
  — the big picture, the object model, the rendering pipeline and the
  repository layout. Start here.
- [Adding a table-object
  adapter](https://ichirio.github.io/rtfreporter/articles/extending-adapters.md)
  — the six-step how-to for supporting a new table package.
- [External API
  specification](https://ichirio.github.io/rtfreporter/articles/external-api.md)
  — the public surface in detail.
- [Internal class design
  (S3)](https://ichirio.github.io/rtfreporter/articles/internal-design.md)
  — the deep-dive design document.
- `CONTRIBUTING.md` — the contribution workflow, branching, versioning
  and releases, including the *Contributor vs Collaborator* split.
- `AGENTS.md` — the same orientation in one page, for a coding agent or
  a new contributor in a hurry.

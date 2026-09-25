# The AI assistant manuals that ship with this package

rtfreporter is too new to be in any chat model's training data: asked
for rtfreporter code, an assistant reaches for `r2rtf`'s verbs or
invents arguments, and flags neither as a guess. The fix is to give it
the facts first, and these two files are those facts, each sized to sit
in one chat session.

## Usage

``` r
rtfreporter_ai_manual(which = c("user", "dev"), file = NULL, overwrite = FALSE)
```

## Arguments

- which:

  `"user"` (default) or `"dev"` – see *Which manual*.

- file:

  Optional destination. When given, the manual is copied there (ready to
  attach to a chat session) and the destination is returned invisibly. A
  directory is accepted, and the file keeps its own name.

- overwrite:

  Overwrite `file` if it already exists. Default `FALSE`.

## Value

The path to the manual – the installed file when `file` is `NULL`,
otherwise the copy, returned invisibly.

## Details

`rtfreporter_ai_manual()` returns the path to the copy **installed with
this package**, so the manual you attach always describes the version
you actually have. The same files are published on the documentation
site, but that copy tracks the development version – when the two
differ, this one is the one that matches your installation.

## Which manual

- `"user"` (default):

  For *using* rtfreporter: the workflow, every
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
  argument, the four clinical table shapes, headers and page tokens,
  listings, figures, borders, and the complete export list.

- `"dev"`:

  For working *on* the package: the invariants, the S3 object model, the
  rendering pipeline, the adapter contract, and the test / docs /
  release conventions.

Attach **one** of them, never both: the user manual forbids touching
internals and the developer manual requires it, so an assistant given
both follows neither cleanly.

## Examples

``` r
# where the manual lives
rtfreporter_ai_manual()
#> [1] "/home/runner/work/_temp/Library/rtfreporter/ai/rtfreporter-ai-user-manual.md"

# read it here, or copy it out to attach to a chat session
writeLines(head(readLines(rtfreporter_ai_manual()), 3))
#> # rtfreporter — AI user manual
#> 
#> **This manual documents rtfreporter 0.8.0.9002** (the development
rtfreporter_ai_manual("dev", file = tempfile(fileext = ".md"))
```

# Build an assembly spec (one editable row per RTF)

Reads the table number and title from each RTF's running header (the
format written by rtfreporter's headers) and returns a `data.frame` –
the **assembly spec** – with one row per file. Edit it (rename labels,
add section `heading`s, change `level`, reorder rows, or drop rows) and
pass it to
[`assemble_from_spec()`](https://ichirio.github.io/rtfreporter/reference/assemble_from_spec.md).

## Usage

``` r
assemble_spec(dir = NULL, files = NULL, recursive = FALSE)
```

## Arguments

- dir:

  Directory to scan (ignored if `files` is given).

- files:

  Optional explicit vector of `.rtf` paths (overrides `dir`).

- recursive:

  Passed to
  [`assemble_files()`](https://ichirio.github.io/rtfreporter/reference/assemble_files.md)
  when scanning `dir`.

## Value

A `data.frame` with columns:

- `order`:

  integer, the assembly order (editable).

- `file`:

  path to the `.rtf`.

- `table`:

  table number read from the header (or `NA`).

- `heading`:

  section heading to print above this entry in the TOC (`NA` = none);
  fill these in to group entries.

- `label`:

  the TOC entry text (defaults to `"Table N <title>"`).

- `level`:

  TOC indent level of the entry (default `2`).

- `pages`:

  page count of the file (informational).

## See also

[`assemble_from_spec()`](https://ichirio.github.io/rtfreporter/reference/assemble_from_spec.md),
[`assemble_folder()`](https://ichirio.github.io/rtfreporter/reference/assemble_folder.md).

## Examples

``` r
if (FALSE) { # \dontrun{
spec <- assemble_spec("output/tfl")   # one editable row per file
spec$heading[spec$table == "14.1.1"] <- "Demographics"   # group entries
assemble_from_spec(spec, "deliverable.rtf")
} # }
```

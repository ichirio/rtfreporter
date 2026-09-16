# Adding a table-object adapter

[In
Japanese](https://ichirio.github.io/rtfreporter/articles/extending-adapters-ja.md)

rtfreporter can read tables produced by other packages — gt, gtsummary,
rtables/tern, flextable, and huxtable — and turn them into RTF while
preserving their metadata (column labels, alignment, spanning headers,
titles, footnotes, per-cell styles). This article shows how to teach it
about **a new table-object type**, using the existing `gt_adapter.R`,
`rtables_adapter.R`, `flextable_adapter.R` and `huxtable_adapter.R` as
templates.

Read [Architecture &
internals](https://ichirio.github.io/rtfreporter/articles/architecture.md)
first for the big picture.

## The contract

An adapter has one job: convert your object into the common **kwargs
list** that
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
consumes. Every field except `data` is optional.

``` r
list(
  data            = <data.frame>,   # REQUIRED: the rendered body (character cells are fine)
  col_header      = <chr vector | stacked spanning rows>,
  col_spec        = list(list(col = 1L, align = "left"), ...),
  cell_styles     = <list, one element per data row, or NULL>,
  titles_block    = <chr vector | NULL>,   # becomes the page title
  footnotes_block = <chr vector | NULL>    # becomes the page footnote
)
```

[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
does everything else (pagination, per-page `cell_styles` slicing,
replicating shared metadata, attaching titles/footnotes). You do **not**
build the `rtftable` yourself.

## Step 1 — a detector

Add a cheap, dependency-light predicate. Keep it side-effect free.

``` r

# R/mypkg_adapter.R
.is_mypkg_tbl <- function(x) inherits(x, "mypkg_table")
# (for an S4 class use: isS4(x) && methods::is(x, "MyClass"))
```

## Step 2 — token resolution

Tokens let users opt in/out of metadata via `read =`. Mirror the gt /
rtables resolvers.

``` r

.MYPKG_TOKENS_ALL <- c("col_header", "alignment", "titles", "footnotes")

.resolve_mypkg_tokens <- function(read) {
  if (is.null(read) || isFALSE(read)) return(character(0))
  if (isTRUE(read))                    return(.MYPKG_TOKENS_ALL)
  if (!is.character(read))
    stop("`read` must be FALSE/TRUE or a character vector of tokens.",
         call. = FALSE)
  bad <- setdiff(read, .MYPKG_TOKENS_ALL)
  if (length(bad))
    stop("Unknown token(s): ", paste(bad, collapse = ", "), call. = FALSE)
  read
}
```

## Step 3 — the extractor

Return the kwargs list. Always require the heavy package lazily.

``` r

.mypkg_to_rtftable_kwargs <- function(x, tokens = .MYPKG_TOKENS_ALL) {
  if (!requireNamespace("mypkg", quietly = TRUE))
    stop("Reading a mypkg table requires the `mypkg` package.", call. = FALSE)

  out <- list()
  out$data <- mypkg::as_body_data_frame(x)          # however your package exposes the body

  if ("col_header" %in% tokens)
    out$col_header <- mypkg::column_labels(x)

  if ("alignment" %in% tokens)
    out$col_spec <- lapply(seq_len(ncol(out$data)), function(j)
      list(col = j, align = mypkg::col_align(x, j)))

  if ("titles" %in% tokens)
    out$titles_block <- mypkg::title_lines(x)

  if ("footnotes" %in% tokens)
    out$footnotes_block <- mypkg::footnote_lines(x)

  out
}
```

Tips drawn from the existing adapters:

- **Spanning headers** are a multi-row `col_header`: a list whose upper
  elements are `list(col_cell(pos = c(from, to), label = ...), ...)` and
  whose last element is the leaf-label character vector. See
  `.extract_spanners()` (gt) and `.rtables_header_segments()` (rtables).
- **Per-cell styles** go in `cell_styles`: a list with one element per
  data row, each either `NULL` or
  `list(bold=, italic=, underline=, indent_twips=)` where each is a
  vector of length `ncol` with `NA` meaning “inherit from `col_spec`”.
- **Footnote marks already in the text.** If your source renders marks
  inline (gt uses `<sup>N</sup>`, rtables uses `{N}`), convert them to
  rtfreporter’s `^{N}` superscript markup **without `gsub`
  backreferences** (some locale-broken R builds drop `\\1`) — use
  [`regmatches()`](https://rdrr.io/r/base/regmatches.html) as in
  `.convert_footnote_marks()` / `.convert_rtables_marks()`.

## Step 4 — wire it into `as_rtftables()`

Three small edits in `R/as_rtftables.R`:

``` r
# (a) the list-input guard must NOT treat your object as a list of items
if (is.list(x) && !is.data.frame(x) && !isS4(x) &&
    !.is_gt_tbl(x) && !.is_gtsummary_tbl(x) &&
    !.is_rtables_tbl(x) && !.is_mypkg_tbl(x)) { ... }

# (b) the resolve-body-+-metadata dispatch
} else if (.is_mypkg_tbl(x)) {
  tokens          <- .resolve_mypkg_tokens(read)
  kw              <- .mypkg_to_rtftable_kwargs(x, tokens = tokens)
  body            <- kw$data
  cell_styles     <- kw$cell_styles
  titles_block    <- kw$titles_block
  footnotes_block <- kw$footnotes_block
}
```

Optionally relax the guard in
[`as_rtftable()`](https://ichirio.github.io/rtfreporter/reference/as_rtftable.md)
(the singular wrapper) so it accepts your type too.

## Step 5 — declare the dependency

Add `mypkg` to `Suggests:` in `DESCRIPTION`. Never add it to `Imports:`
— the zero-hard-dependency rule is deliberate. The lazy
[`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) in Step 3
produces a friendly error when it is missing.

## Step 6 — tests and docs

- Add `tests/testthat/test-mypkg-adapter.R`, wrapping every test in
  `skip_if_not_installed("mypkg")`.
- Document the new `read` tokens (extend
  [`?as_rtftables`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)).
- Add a `NEWS.md` entry; for a major/minor change update `CHANGELOG.md`.
- Run `devtools::document()`, `devtools::test()`, `devtools::check()`.

## A worked reference

The rtables adapter (`R/rtables_adapter.R`) is a compact, complete
example that exercises every part of the contract — multi-level spanning
headers, per-row indentation as `cell_styles`, inline `{N}` -\> `^{N}`
mark conversion, and title/footer extraction — all from a single
[`formatters::matrix_form()`](https://rdrr.io/pkg/formatters/man/matrix_form.html)
call. It is a good model to copy.

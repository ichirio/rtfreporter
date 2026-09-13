# The default wrapping rule, as source to edit

The code
[`listing_wrap()`](https://ichirio.github.io/rtfreporter/reference/listing_wrap.md)
runs, written out ready to paste into a script and change. Use it when a
custom
[`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md)
`wrap` needs a *different* rule rather than an adjustment to this one –
when delegating to
[`listing_wrap()`](https://ichirio.github.io/rtfreporter/reference/listing_wrap.md)
and fixing up its result will not do.

## Usage

``` r
listing_wrap_code(name = "my_wrap")
```

## Arguments

- name:

  Name for the entry function – the one to pass as `wrap`. The helpers
  are prefixed with it.

## Value

A character vector of source lines, classed so that printing it writes
the code out.

## Details

What comes back is a verbatim copy of the shipped rule, comments and
all, regenerated from it and checked against it by the test suite. There
is no `rtfreporter:::` in it, and the entry function already matches the
`wrap` contract described in
[`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md).

What it hands out is the rule's POLICY – three functions. What they
measure with is exported instead:
[`listing_disp_width()`](https://ichirio.github.io/rtfreporter/reference/listing_measures.md),
[`listing_take()`](https://ichirio.github.io/rtfreporter/reference/listing_measures.md)
and
[`listing_split_after()`](https://ichirio.github.io/rtfreporter/reference/listing_measures.md)
are called, not copied, because every fork would keep them verbatim.
Edit the policy; leave the measurements shared.

The two helpers are named after `name`, so two edited rules can live in
one script.

## See also

[`listing_wrap()`](https://ichirio.github.io/rtfreporter/reference/listing_wrap.md)
to delegate to the rule instead of editing it,
[`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md)
for the contract an edited rule still has to keep, and
[`listing_code()`](https://ichirio.github.io/rtfreporter/reference/listing_code.md),
which writes a spec out the same way.

## Examples

``` r
# Print it, paste it, edit it.
listing_wrap_code("my_wrap")
#> # The "multiline" wrapping rule from rtfreporter 0.7.31, to edit.
#> #
#> #   listing_spec(cols, wrap = my_wrap)
#> #
#> # Keep the contract (see ?listing_spec): called positionally with
#> # (text, width, sep, layout); `text` is length 1; `width` may be NULL;
#> # `layout` is already "stack" or "flow"; return a non-empty character
#> # vector, one element per line.
#> #
#> # listing_disp_width(), listing_take() and listing_split_after() below are
#> # rtfreporter's own, shared rather than copied -- library(rtfreporter).
#> 
#> my_wrap_flow <- function(parts, width) {
#>   out <- character(0L)
#>   cur <- ""
#>   for (p in parts) {
#>     if (!nzchar(cur)) {
#>       cur <- p
#>     } else if (listing_disp_width(trimws(paste0(cur, p))) <= width) {
#>       cur <- paste0(cur, p)
#>     } else {
#>       out <- c(out, cur)
#>       cur <- p
#>     }
#>   }
#>   if (nzchar(cur)) out <- c(out, cur)
#>   out
#> }
#> 
#> my_wrap_words <- function(text, width) {
#>   words <- strsplit(text, "(?<=[ ,-])", perl = TRUE)[[1L]]
#>   if (!length(words)) words <- text
#>   out <- character(0L)
#>   cur <- ""
#>   for (w in words) {
#>     # A token wider than the column on its own.  Split it here, before the
#>     # running line is trimmed to measure it -- trimming would eat the trailing
#>     # space that separates this word from the next.
#>     if (listing_disp_width(trimws(w)) > width) {
#>       if (nzchar(trimws(cur))) out <- c(out, trimws(cur))
#>       tok <- sub("^\\s+", "", w)
#>       while (listing_disp_width(trimws(tok)) > width) {
#>         piece <- listing_take(tok, width)
#>         out   <- c(out, piece)
#>         tok   <- substring(tok, nchar(piece, type = "chars") + 1L)
#>       }
#>       cur <- tok                       # keeps any trailing separator
#>       next
#>     }
#>     if (!nzchar(cur)) {
#>       cur <- w
#>       # Measured WITHOUT trimming the running line, which is what the rule
#>       # this came from did.  Trimming would let a line end one character
#>       # wider (the trailing space is invisible) and would silently change
#>       # every existing listing's line counts; that is a separate decision,
#>       # not part of fixing #364.
#>     } else if (listing_disp_width(cur) + listing_disp_width(w) <= width) {
#>       cur <- paste0(cur, w)
#>     } else {
#>       out <- c(out, trimws(cur))
#>       cur <- w
#>     }
#>   }
#>   if (nzchar(trimws(cur))) out <- c(out, trimws(cur))
#>   out
#> }
#> 
#> my_wrap <- function(text, width, sep, layout = "stack") {
#>   if (is.null(text) || length(text) != 1L || is.na(text)) text <- ""
#>   text <- as.character(text)
#>   # A "\n" already in the data is a line break the author asked for; honour it
#>   # before any width is considered.
#>   chunks <- strsplit(text, "\n", fixed = TRUE)[[1L]]
#>   if (!length(chunks)) chunks <- ""
#>   if (is.null(width) || is.na(width)) {
#>     # No width, no layout: with nothing to lay the column out against, both
#>     # `layout`s return the text as it stands.  (ydisctools' rule breaks at
#>     # every separator here even with no width; that would silently make every
#>     # existing width-less multi-variable column taller, and nothing asks for
#>     # it -- `width` is what says how the column is laid out.)
#>     chunks <- trimws(chunks)
#>     return(if (all(!nzchar(chunks))) "" else chunks)
#>   }
#>   out <- character(0L)
#>   for (ch in chunks) {
#>     parts <- listing_split_after(ch, sep)
#>     # "flow": refill the pieces first, so a break survives only where the
#>     # line ran out of room.  "stack" keeps every separator break.
#>     if (identical(layout, "flow")) parts <- my_wrap_flow(parts, width)
#>     for (p in parts) {
#>       p <- trimws(p)
#>       if (!nzchar(p)) next
#>       if (listing_disp_width(p) <= width) {
#>         out <- c(out, p)
#>       } else {
#>         out <- c(out, my_wrap_words(p, width))
#>       }
#>     }
#>   }
#>   if (!length(out)) "" else out
#> }

# It runs as it stands: the emitted rule is the shipped one.
src <- listing_wrap_code("my_wrap")
env <- new.env(parent = globalenv())
eval(parse(text = src), envir = env)
identical(env$my_wrap("COMPLETED/BRCA1", 12, "/", "stack"),
          listing_wrap("COMPLETED/BRCA1", 12))
#> [1] TRUE
```

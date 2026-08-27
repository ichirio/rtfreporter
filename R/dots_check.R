# ---------------------------------------------------------------------------
# Argument validation for verbs whose `...` is a pure catch-all.
#
# Several style verbs take `...` only so that S3 dispatch stays open, not
# because they consume anything from it.  R's partial matching does not rescue
# a misspelled argument -- matching is case-sensitive, so `colS` is not a
# prefix of `cols` -- and the argument lands in `...` where it is silently
# dropped.  For set_decimal_split() that was actively harmful: the call then
# ran with `cols = NULL`, which is the documented way to CLEAR the setting, so
# a typo quietly removed the split instead of applying it (#301).
#
# These helpers make the mistake visible.  They warn rather than stop, so no
# working pipeline breaks; only a genuinely required argument errors.
# ---------------------------------------------------------------------------

# The argument names a method actually consumes: its formals minus the object
# and the catch-all itself.
.valid_args <- function(fun) {
  setdiff(names(formals(fun)), c("x", "object", "..."))
}

# The closest valid name to a mistyped one, or NULL when nothing is close.
# Case-insensitive first (which is the `colS` -> `cols` case), then edit
# distance for ordinary slips such as `col_rel_widht`.
.nearest_arg <- function(bad, valid) {
  if (length(valid) == 0L) return(NULL)
  ci <- valid[tolower(valid) == tolower(bad)]
  if (length(ci)) return(ci[1L])
  d <- utils::adist(bad, valid, ignore.case = TRUE)[1L, ]
  near <- valid[d <= max(2L, floor(nchar(bad) / 3))]
  if (length(near)) near[which.min(d[d <= max(2L, floor(nchar(bad) / 3))])]
  else NULL
}

# Warn about anything left in `...`, and return the recognised arguments.
#
# `dots`  the caller's list(...)
# `valid` the names the caller accepts
# `fn`    the verb name, for the message
#
# Returns `dots` with the unknown entries removed, so a `.list` dispatcher can
# validate once and forward only what the leaf method understands -- otherwise
# a typo would warn once per page.
#
# UNNAMED arguments are left strictly alone.  A `.list` dispatcher is declared
# `function(x, ...)`, so a perfectly ordinary positional call --
# `add_header_row(pages, c("A", "B"))` -- arrives here unnamed and must reach
# the leaf method to be matched positionally there.  Only a *named* argument
# that matches no formal is diagnosable as a typo.
.check_dots <- function(dots, valid, fn) {
  if (length(dots) == 0L) return(dots)

  nm <- names(dots)
  if (is.null(nm)) nm <- rep("", length(dots))

  unknown <- nzchar(nm) & !(nm %in% valid)
  if (!any(unknown)) return(dots)

  bad <- nm[unknown]
  parts <- vapply(bad, function(b) {
    hint <- .nearest_arg(b, valid)
    if (is.null(hint)) sprintf("`%s`", b)
    else sprintf("`%s` (did you mean `%s`?)", b, hint)
  }, character(1L))
  warning(sprintf(
    "`%s()`: unknown argument%s %s ignored.\n  Valid arguments: %s.",
    fn, if (length(bad) > 1L) "s" else "",
    paste(parts, collapse = ", "),
    paste(valid, collapse = ", ")),
    call. = FALSE)

  # Dropping a named element does not disturb the positional matching of the
  # unnamed ones, which keep their order.
  dots[!unknown]
}

# Convenience wrapper for a leaf method: validate its own `...` against its own
# formals.  `fun` is the method, `fn` the user-facing verb name.
.check_own_dots <- function(dots, fun, fn) {
  .check_dots(dots, .valid_args(fun), fn)
}

# The `.list` half of a style verb: validate the caller's `...` once against
# the leaf method's formals, then map the verb over the pages with only the
# recognised arguments.  `fixed` carries any argument the dispatcher itself
# names (style_body()'s `rows`, say).
.style_dispatch_pages <- function(x, generic, leaf, verb, dots,
                                  fixed = list()) {
  dots <- .check_dots(dots, .valid_args(leaf), verb)
  do.call(.style_map_pages,
          c(list(x, generic), fixed, dots, list(verb = verb)))
}

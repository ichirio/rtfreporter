# ============================================================================
#  EXPERIMENTAL -- cards/cardx ARD  ->  table data.frame            (issue #474)
# ----------------------------------------------------------------------------
#  Everything this feature owns lives in this one file.  TO REMOVE IT ENTIRELY:
#
#    1. delete  R/ard-experimental.R                 (this file)
#    2. delete  tests/testthat/test-ard-experimental.R
#    3. delete  man/ard_*.Rd  man/read_ard_spec.Rd  man/write_ard_spec.Rd
#               man/rtfreporter-ard.Rd
#    4. delete the block in NAMESPACE between the two
#               "# ---- experimental: ARD ----" marker comments
#    5. drop the "Experimental ARD helpers" section from NEWS.md
#    6. delete  inst/extdata/ard-spec/  and  data-raw/ard-spec-examples/
#               (the example definition workbooks and their builder)
#
#  No other source file in the package refers to any of these functions, and
#  nothing here is used by as_rtftables() or the renderer.
# ----------------------------------------------------------------------------
#  DESIGN RULE (from Discussion #473): the ARD's *object attributes* are never
#  read.  attr(ard, "args") is ordered differently per generator, mixes `by`
#  with `variables`, and is silently dropped for the second operand of
#  dplyr::bind_rows(); the ARD class survives bind_rows() of a different shape,
#  so S3 dispatch on it lies too.  Every structural fact -- which key goes
#  across, which goes down, what nests inside what -- is an explicit argument.
#  Only the tibble's own *rows* are inspected.
#
#  Base R only: no new Imports.  cards / readxl / writexl are optional and
#  reached through requireNamespace(), as everywhere else in this package.
# ============================================================================


# The experimental export set.  test-api-surface.R subtracts it from the
# reviewed API count, the same way it subtracts `.deprecated_exports`: neither
# is part of the surface a reader has to learn, and this family may be
# withdrawn outright.  Keep it in step with the NAMESPACE marker block.
.experimental_exports <- c(
  "ard_pull", "ard_keys", "ard_normalize", "ard_overall", "ard_spread", "ard_template",
  "ard_cells",
  "ard_spec", "ard_spec_template", "read_ard_spec", "write_ard_spec")


# ---------------------------------------------------------------- utilities

.ard_stop <- function(...) stop(..., call. = FALSE)

.ard_need <- function(pkg, what) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    .ard_stop(sprintf("%s needs the '%s' package (install it, or see ?%s).",
                      what, pkg, "rtfreporter-ard"))
  }
  invisible(TRUE)
}

# Column names an ARD uses structurally; these can always be referenced
# directly by `cols` / `rows` / `label`.
.ard_structural <- function() {
  c("variable", "variable_level", "context", "stat_name", "stat_label",
    "stat", "stat_fmt", ".kind", ".depth", ".label", ".overall",
    ".key_own")
}

# Flatten one list-column.  An element that is NULL, or longer than one, or a
# function/closure (cards stores fmt_fun there) becomes NA.
#
# A FACTOR element is converted to its label first.  cards stores the level of
# a factor variable as a one-element factor, and `unlist()` on those yields the
# integer codes -- so a demographics table came out with `1`, `2`, `3` in the
# label column where it should read `<65`, `65-74`, `>=75`.  The label is right
# there in the ARD; only the flattening lost it.  (cards' own
# `rename_ard_columns(fct_as_chr = TRUE)` makes the same conversion.)
.ard_unlist_col <- function(col) {
  if (!is.list(col)) return(col)
  n <- length(col)
  simple <- vapply(col, function(e) {
    length(e) == 1L && is.atomic(e) && !is.list(e)
  }, logical(1))
  out <- vector("list", n)
  for (i in seq_len(n)) {
    e <- if (simple[i]) col[[i]] else NA
    out[[i]] <- if (is.factor(e)) as.character(e) else e
  }
  vals <- unlist(out, use.names = FALSE)
  if (length(vals) != n) vals <- rep(NA, n)
  vals
}

# The level order a factor variable declared, read off the ARD's one-element
# factors before they are flattened.  An analyst who wrote `factor(levels = )`
# has already said how the rows should run; taking it saves them saying it
# again in `levels =`, and keeps the order right when a level is missing from
# one treatment group.  Returns a named list: variable -> levels.
.ard_factor_levels <- function(x) {
  if (!all(c("variable", "variable_level") %in% names(x))) return(NULL)
  lv <- x[["variable_level"]]
  if (!is.list(lv)) return(NULL)
  vars <- as.character(.ard_unlist_col(x[["variable"]]))
  out <- list()
  for (i in seq_along(lv)) {
    e <- lv[[i]]
    if (!is.factor(e) || is.na(vars[i]) || vars[i] %in% names(out)) next
    out[[vars[i]]] <- levels(e)
  }
  if (length(out)) out else NULL
}

# The same for the GROUPING variables: `groupN_level` holds the level of
# `groupN` as a one-element factor too, and that one element still carries
# the whole `levels()` -- unused levels included, and through bind_ard() /
# dplyr::bind_rows() -- so the order a treatment variable was declared in is
# in the ARD for the reading.  Returns a named list: variable -> levels.
.ard_group_factor_levels <- function(x) {
  out <- list()
  for (g in grep("^group[0-9]+$", names(x), value = TRUE)) {
    lv <- x[[paste0(g, "_level")]]
    if (!is.list(lv)) next
    vars <- as.character(.ard_unlist_col(x[[g]]))
    for (i in seq_along(lv)) {
      e <- lv[[i]]
      if (!is.factor(e) || is.na(vars[i]) || vars[i] %in% names(out)) next
      out[[vars[i]]] <- levels(e)
    }
  }
  if (length(out)) out else NULL
}

# Read that order back off the normalized frame's `.label_order` column: for
# each variable, its labels in the position the factor gave them.  A COLUMN,
# not an attribute, because the caller is invited to rework the frame between
# ard_normalize() and ard_spread() and `dplyr::mutate()` -- the natural verb
# for a one-pipe conversion -- rebuilds it and drops attributes.  Reading the
# label as it stands now is also what we want: a caller who indented a level
# to "  Mild" gets "  Mild" in the order "Mild" declared.
.ard_levels_from_order <- function(d) {
  need <- c(".label_order", ".label", "variable")
  if (!all(need %in% names(d))) return(NULL)
  ok <- !is.na(d$.label_order) & !is.na(d$.label)
  if (!any(ok)) return(NULL)
  v   <- as.character(d$variable)[ok]
  lab <- as.character(d$.label)[ok]
  ord <- as.integer(d$.label_order)[ok]
  out <- list()
  for (k in .ard_first_seen(v)) {
    hit <- v == k
    out[[k]] <- unique(lab[hit][order(ord[hit])])
  }
  if (length(out)) out else NULL
}

# TRUE when every non-NA element of a flattened column is numeric-like.
.ard_is_numericish <- function(x) {
  if (is.numeric(x) || is.logical(x)) return(TRUE)
  y <- suppressWarnings(as.numeric(as.character(x)))
  all(is.na(x) | !is.na(y))
}

# `labels` and `levels` are looked up BY NAME, so an unnamed element is not a
# no-op you would notice -- it is a label or an order that silently never
# applies.  The classic way to produce one is the two-parallel-vector idiom,
# `setNames(group_labels, group_vars)`: when `group_vars` is the shorter of the
# two, setNames() gives the extra elements an NA name rather than complaining,
# and that characteristic quietly keeps its raw variable name in the table.
# `labels` recodes a VALUE to the text it prints as.  One dictionary for
# the whole table is the usual thing and stays what it was.  But the same
# value can mean two things on the two axes -- a shift table's "0" is
# "Grade 0" down the side and "Baseline 0" across the top -- so an entry
# whose value is itself a NAMED vector is a dictionary for that COLUMN,
# matched on the output name then the source name:
#
#     labels = list(AGE    = "Age (years)",          # a value, anywhere
#                   BASEGR = c("0" = "Baseline 0"),  # only in BASEGR
#                   WORST  = c("0" = "Grade 0"))     # only in WORST
#
# The two can be mixed because they are told apart by shape, not by a
# switch: a value has text, a column has a dictionary.
.ard_labels_split <- function(labels) {
  if (is.null(labels)) return(NULL)
  if (!is.list(labels)) return(list(flat = labels, scopes = list()))
  scoped <- vapply(labels, function(z)
    !is.null(names(z)) && any(nzchar(names(z))), logical(1L))
  scoped <- scoped & names(labels) != ".default"
  flat <- labels[!scoped]
  dots <- names(flat) == ".default"
  flat <- c(unlist(unname(flat[dots])), unlist(flat[!dots]))
  list(flat = flat, scopes = labels[scoped])
}

.ard_labels_for <- function(labels, ref = NULL, out = NULL) {
  sp <- .ard_labels_split(labels)
  if (is.null(sp)) return(NULL)
  pick <- function(k) {
    if (is.null(k) || length(k) != 1L || is.na(k)) return(NULL)
    sp$scopes[[k]]
  }
  pick(out) %||% pick(ref) %||% sp$flat
}

# One view of every dictionary, for the places that only read the NAMES --
# the variable order `labels` also fixes.
.ard_labels_flat <- function(labels) {
  sp <- .ard_labels_split(labels)
  if (is.null(sp)) return(NULL)
  out <- c(sp$flat, unlist(unname(sp$scopes)))
  if (is.null(out)) NULL else out[!duplicated(names(out))]
}

.ard_check_named <- function(x, arg) {
  if (is.null(x) || !length(x)) return(invisible(TRUE))
  nms <- names(x)
  if (is.null(nms) || any(is.na(nms)) || !all(nzchar(nms))) {
    bad <- if (is.null(nms)) seq_along(x) else
      which(is.na(nms) | !nzchar(nms))
    .ard_stop(sprintf(
      paste0("`%s` must name every element; element(s) %s have no name.\n",
             "  A `%s` entry is matched by its name, so an unnamed one never ",
             "applies.\n",
             "  With `setNames(labels, vars)`, check that the two vectors are ",
             "the same length."),
      arg, paste(utils::head(bad, 5), collapse = ", "), arg))
  }
  dup <- unique(nms[duplicated(nms)])
  if (length(dup)) {
    .ard_stop(sprintf("`%s` names must be unique; %s is repeated.",
                      arg, paste(sQuote(dup), collapse = ", ")))
  }
  invisible(TRUE)
}

# stable order of first appearance
.ard_first_seen <- function(x) {
  u <- unique(as.character(x))
  u[!is.na(u)]
}

# Build a factor whose levels are `lv` (padding with anything unseen so no
# value is silently dropped).
#
# Plain, not ordered, by default.  `levels =` fixes the DISPLAY order, and
# that is all the caller told us: "N, Mean, SD, Median" is a row order, not a
# magnitude.  An ordered factor would assert `N < Mean`, which is false, and
# nothing downstream reads the ordered class anyway -- `order()` sorts on the
# level codes either way, so the RTF is byte-identical.  The column keys ask
# for `ordered = TRUE` explicitly, but only to sort the column names.
# The column keys (one string per row, the `cols` values pasted together) in
# the order the frame declares: a factor key's levels where there is one,
# first-seen otherwise.  With no factor key at all this IS first-seen, so a
# character key orders exactly as it always did.
.ard_key_order <- function(d, cols, key) {
  fac <- vapply(cols, function(k) is.factor(d[[k]]), logical(1))
  if (!any(fac)) return(.ard_first_seen(key))
  parts <- lapply(cols, function(k) {
    v <- d[[k]]
    if (is.factor(v)) v else factor(as.character(v), .ard_first_seen(v))
  })
  .ard_first_seen(key[do.call(order, c(parts, list(seq_along(key))))])
}

.ard_as_factor <- function(x, lv, ordered = FALSE) {
  x <- as.character(x)
  extra <- setdiff(.ard_first_seen(x), lv)
  factor(x, levels = c(lv, extra), ordered = ordered)
}



# ---------------------------------------------------------------------------
#  What was not used
# ---------------------------------------------------------------------------
#  Every stage here discards ARD rows: the `attributes` and `total_n` rows,
#  the key variables' own tabulations, rows whose column key is missing, and
#  -- the big one -- every statistic no template named.  All of it was silent,
#  and silence is how two real bugs stayed hidden: rows collapsing into each
#  other, and a whole overall block disappearing.  So the discards are tallied
#  and travel with the result on the "ard_ignored" attribute, and `notes`
#  prints a summary.

.ard_tally <- function(x, reason) {
  if (is.null(x) || !nrow(x)) return(NULL)
  v  <- as.character(.ard_unlist_col(x[["variable"]]))
  st <- as.character(.ard_unlist_col(x[["stat_name"]]))
  ct <- as.character(x[["context"]])
  key   <- paste(v, ct, st, sep = "\r")
  cnt   <- table(key)
  first <- !duplicated(key)
  data.frame(variable = v[first], context = ct[first], stat_name = st[first],
             rows = as.integer(cnt[key[first]]), reason = reason,
             stringsAsFactors = FALSE)
}

# The study total the ARD states outright, as ONE number, or NULL.
# Read from the rows ard_normalize() drops so that a header can still
# ask for it.  A total that is not one number is not a study total, and
# guessing which of several it meant is exactly what this refuses to do.
.ard_total_n_value <- function(x) {
  if (is.null(x) || !nrow(x)) return(NULL)
  v <- as.character(.ard_unlist_col(x[["variable"]]))
  st <- as.character(.ard_unlist_col(x[["stat_name"]]))
  keep <- !is.na(v) & v == "..ard_total_n.." &
    !is.na(st) & st == "N"
  if (!any(keep)) return(NULL)
  val <- suppressWarnings(as.numeric(as.character(
    .ard_unlist_col(x[["stat"]])[keep])))
  val <- unique(val[!is.na(val)])
  if (length(val) == 1L) val else NULL
}

.ard_ignored_bind <- function(...) {
  parts <- Filter(function(z) !is.null(z) && nrow(z), list(...))
  if (!length(parts)) return(NULL)
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out[order(out$reason, out$context, out$variable, out$stat_name), ,
      drop = FALSE]
}

# One compact line per (context, statistic, reason) -- the per-variable detail
# stays on the attribute, because an adverse-events table has hundreds of
# variables and three reasons.
# Which template produced each cell.  `notes` already says what was NOT
# read; once a chain can carry guards, the other half of the question --
# "of these three, which one did I get?" -- stops being answerable by
# looking at the finished cell, so it has to be reportable too.
.ard_applied_message <- function(long, what) {
  if (is.null(long) || !nrow(long) || !".tpl" %in% names(long)) {
    return(invisible(FALSE))
  }
  keep <- !is.na(long$.tpl)
  if (!any(keep)) return(invisible(FALSE))
  x <- long[keep, , drop = FALSE]
  key <- paste(x$.var, x$.lab, x$.tpl, x$.guard, sep = "")
  agg <- x[!duplicated(key), c(".var", ".lab", ".tpl", ".guard"), drop = FALSE]
  agg$cells <- as.integer(table(key)[unique(key)])
  agg <- agg[order(agg$.var, -agg$cells), , drop = FALSE]
  message(sprintf("%s: %d template%s produced %s cell%s.", what, nrow(agg),
                  if (nrow(agg) == 1L) "" else "s",
                  format(sum(agg$cells), big.mark = ","),
                  if (sum(agg$cells) == 1L) "" else "s"))
  show <- utils::head(agg, 12L)
  for (i in seq_len(nrow(show))) {
    lab <- if (is.na(show$.lab[i])) "<levels>" else show$.lab[i]
    message(sprintf("  %-12s %-12s %7s  %s%s",
                    show$.var[i], lab, format(show$cells[i], big.mark = ","),
                    show$.tpl[i],
                    if (is.na(show$.guard[i])) ""
                    else paste0("   <- when ", show$.guard[i])))
  }
  if (nrow(agg) > nrow(show)) {
    message(sprintf("  ... and %d more", nrow(agg) - nrow(show)))
  }
  invisible(TRUE)
}

.ard_notes_message <- function(ig, what) {
  if (is.null(ig) || !nrow(ig)) return(invisible(FALSE))
  key <- paste(ig$context, ig$stat_name, ig$reason, sep = "\r")
  agg <- ig[!duplicated(key), c("context", "stat_name", "reason"),
            drop = FALSE]
  agg$rows <- as.integer(vapply(split(ig$rows, key)[unique(key)], sum, 0))
  agg <- agg[order(-agg$rows), , drop = FALSE]
  message(sprintf("%s: %s ARD row%s not used.", what,
                  format(sum(ig$rows), big.mark = ","),
                  if (sum(ig$rows) == 1L) " was" else "s were"))
  show <- utils::head(agg, 8L)
  for (i in seq_len(nrow(show))) {
    message(sprintf("  %-14s %-10s %7s  %s",
                    show$context[i], show$stat_name[i],
                    format(show$rows[i], big.mark = ","), show$reason[i]))
  }
  if (nrow(agg) > nrow(show)) {
    message(sprintf("  ... and %d more; see attr(, \"ard_ignored\")",
                    nrow(agg) - nrow(show)))
  }
  message("  (notes = FALSE to silence; attr(, \"ard_ignored\") has the detail)")
  invisible(TRUE)
}

# ============================================================================
#  rounding
# ============================================================================

# The rounding family is the package's one rule (round_num(), option
# `rtfreporter.rounding`); an ARD table does not get a second one.

# Format to `d` significant digits with the caller's rounding family.
# The decimals a significant-digit request implies depend on the value,
# so each element is handled on its own; trailing zeros are dropped, as
# signif() + format() did, so only the HALF cases change.
.ard_signif_fmt <- function(x, d, round_type = NULL) {
  rnd <- .rounder(round_type)
  out <- character(length(x))
  for (i in seq_along(x)) {
    v <- x[[i]]
    if (is.na(v)) {
      out[i] <- NA_character_
      next
    }
    dec <- if (v == 0) max(0L, d - 1L) else
      max(0L, d - 1L - as.integer(floor(log10(abs(v)))))
    r <- rnd(v, dec)
    # rounding can carry into the next decade (99.95 -> 100.0)
    dec2 <- if (r == 0) max(0L, d - 1L) else
      max(0L, d - 1L - as.integer(floor(log10(abs(r)))))
    if (!identical(dec2, dec)) r <- rnd(v, dec2)
    out[i] <- format(r, trim = TRUE, scientific = FALSE)
  }
  out
}


# ============================================================================
#  the {token} mini-language
# ============================================================================
#
#   {mean}            the value cards itself formatted (its fmt_fun output)
#   {mean:.1f}        1 decimal, rounded with `rounding`
#   {p:.1f%}          multiply by 100 first, then 1 decimal
#   {mean:.3s}        3 significant digits
#   {n:d}             integer
#   {n:stat}          the `stat` column, as.character(), untouched
#   {n:stat_fmt}      the `stat_fmt` column, and an error if it is empty
#
# The two specs that name a column are spelled as the ARD spells them, so
# there is no mapping to learn.  A bare {n} is the forgiving one: it prefers
# stat_fmt and falls back to stat, because stat_fmt is optional and an ARD
# built without fmt_fun would otherwise produce nothing at all.
#
# A template that references a statistic the row group does not have yields NA
# for that row, which is what makes a fallback chain work.

.ard_tokens <- function(tpl) {
  m <- regmatches(tpl, gregexpr("[{][^{}]+[}]", tpl))[[1]]
  if (!length(m)) return(character(0))
  m
}

.ard_token_parts <- function(tok) {
  inner <- substr(tok, 2L, nchar(tok) - 1L)
  at <- regexpr(":", inner, fixed = TRUE)
  if (at < 0L) return(list(name = inner, spec = ""))
  list(name = substr(inner, 1L, at - 1L),
       spec = substr(inner, at + 1L, nchar(inner)))
}

.ard_format_value <- function(stat, stat_fmt, spec, round_type) {
  # `stat` takes the column as it stands, whether it holds a number or a
  # string: `method` and `alternative` reach a cell the way `n` does.
  if (identical(spec, "stat")) {
    if (is.na(stat)) return(NA_character_)
    return(as.character(stat))
  }
  # `stat_fmt` is a demand, so an empty one with a value beside it is an
  # error rather than a blank cell -- the caller asked for cards' formatting
  # and this ARD has none.
  if (identical(spec, "stat_fmt")) {
    if (is.na(stat_fmt) && !is.na(stat)) {
      .ard_stop(paste0(
        "`{x:stat_fmt}` was asked for, but this ARD has no `stat_fmt` value ",
        "here (`stat` is ", sQuote(as.character(stat)), ").
",
        "  cards writes `stat_fmt` from `fmt_fun`; an ARD built without one ",
        "has nothing to read.
",
        "  Use `{x}` to take whichever is there, or `{x:.1f}` to format ",
        "`stat` yourself."))
    }
    if (is.na(stat_fmt)) return(NA_character_)
    return(as.character(stat_fmt))
  }
  # A bare token prefers cards' formatted value and falls back to the raw
  # one, so a template keeps working against an ARD that carries no fmt_fun.
  if (!nzchar(spec)) {
    if (!is.na(stat_fmt)) return(as.character(stat_fmt))
    if (is.na(stat)) return(NA_character_)
    return(as.character(stat))
  }
  if (spec %in% c("raw", "fmt")) {
    .ard_stop(sprintf(paste0(
      "Format spec '%s' has been renamed: write '%s', which is the ARD ",
      "column it reads."), spec, if (spec == "raw") "stat" else "stat_fmt"))
  }
  pct <- grepl("%$", spec)
  spec <- sub("%$", "", spec)
  x <- suppressWarnings(as.numeric(stat))
  if (is.na(x)) return(NA_character_)
  if (pct) x <- x * 100
  if (grepl("^[.][0-9]+f$", spec)) {
    d <- as.integer(gsub("[.f]", "", spec))
    return(sprintf(paste0("%.", d, "f"), .rounder(round_type)(x, d)))
  }
  if (grepl("^[.][0-9]+s$", spec)) {
    d <- as.integer(gsub("[.s]", "", spec))
    # Significant digits are decimal places once you know the
    # magnitude, so the rounding FAMILY applies to them exactly as it
    # does to `.2f` -- which base signif() cannot honour, being
    # half-to-even always.  fmt_numeric() already works this way; this
    # is the same arithmetic, so the two agree on a half.
    return(.ard_signif_fmt(x, d, round_type))
  }
  if (identical(spec, "d")) {
    return(sprintf("%.0f", .rounder(round_type)(x, 0)))
  }
  .ard_stop(sprintf(paste0("Unknown format spec '%s'. Use .Nf, .Ns, d, ",
                           "stat, stat_fmt, or a %s suffix."), spec, "%"))
}

# Resolve one template against one group of ARD rows.  A token naming a
# statistic the group does not carry makes the whole template fail, which is
# what lets a chain of templates fall through to the next one.
.ard_fill <- function(tpl, stat_name, stat, stat_fmt, round_type) {
  toks <- .ard_tokens(tpl)
  if (!length(toks)) return(tpl)
  out <- tpl
  for (i in seq_along(toks)) {
    p <- .ard_token_parts(toks[i])
    spec <- p$spec
    j <- match(p$name, stat_name)
    if (is.na(j)) return(NA_character_)
    v <- .ard_format_value(stat[j], stat_fmt[j], spec, round_type)
    if (is.na(v)) return(NA_character_)
    out <- sub(toks[i], v, out, fixed = TRUE)
  }
  out
}

# One element of a fallback chain: a bare template, or `condition ~ template`.
# The condition is kept unevaluated, together with the formula's own
# environment, so a guard may name the caller's variables as well as the
# record's.
.ard_chain_el <- function(x, one_sided = FALSE) {
  if (inherits(x, "formula")) {
    if (length(x) != 3L && !one_sided) {
      .ard_stop(paste0("A `cells` guard needs both sides: ",
                       "`condition ~ template`. This one has only a right."))
    }
    e <- environment(x)
    if (is.null(e)) e <- baseenv()
    # `~ "..."` in a label is the unconditional element of the chain: there is
    # no bare-string spelling for it there, because a bare string is a column
    # name.
    if (length(x) == 2L) {
      tpl <- tryCatch(eval(x[[2L]], e), error = function(err) NULL)
      if (!is.character(tpl) || length(tpl) != 1L) {
        .ard_stop("A one-sided `~` must hold one template string.")
      }
      return(list(cond = NULL, env = e, tpl = tpl))
    }
    tpl <- tryCatch(eval(x[[3L]], e), error = function(err) NULL)
    if (!is.character(tpl) || length(tpl) != 1L) {
      .ard_stop(paste0("The right of a `cells` guard must be one template ",
                       "string, e.g. `n == 0 ~ \"0\"`."))
    }
    return(list(cond = x[[2L]], env = e, tpl = tpl))
  }
  list(cond = NULL, env = NULL, tpl = as.character(x))
}

# A chain is whatever c() or list() produced: strings, guards, or both.
.ard_chain <- function(x, one_sided = FALSE) {
  lapply(as.list(x), .ard_chain_el, one_sided = one_sided)
}

# What a guard sees: every statistic of the record by name, plus the record's
# own columns -- the keys, `.label`, `.depth`, `.kind`, `variable`.  A guard
# naming something absent, or erroring, is FALSE and the chain moves on, which
# is the same rule as "a token of this template has no value".  That is why
# guards need no new concept: they are one more way for an element not to
# apply.
.ard_guard_data <- function(s) {
  out <- as.list(stats::setNames(s$stat, s$stat_name))
  skip <- c("stat", "stat_name", "stat_label", "stat_fmt", "fmt_fun",
            "warning", "error")
  for (cn in setdiff(names(s), skip)) {
    v <- s[[cn]]
    if (is.list(v)) next
    out[[cn]] <- v[[1L]]
  }
  out
}

.ard_guard_ok <- function(el, gd) {
  if (is.null(el$cond)) return(TRUE)
  isTRUE(tryCatch(all(eval(el$cond, gd, el$env)), error = function(e) FALSE))
}

# Walk a chain: the first element whose guard holds and whose template
# resolves wins.  The winner's own text comes back with the value, because
# "which of these three did I get?" is the question a guarded chain invites
# and the finished cell cannot answer it.
.ard_chain_pick <- function(chain, s, round_type) {
  gd <- NULL
  for (el in chain) {
    if (!is.null(el$cond)) {
      if (is.null(gd)) gd <- .ard_guard_data(s)
      if (!.ard_guard_ok(el, gd)) next
    }
    v <- .ard_fill(el$tpl, s$stat_name, s$stat, s$stat_fmt, round_type)
    if (!is.na(v)) {
      return(list(v = v, tpl = el$tpl,
                  guard = if (is.null(el$cond)) NA_character_
                          else paste(deparse(el$cond), collapse = " ")))
    }
  }
  list(v = NA_character_, tpl = NA_character_, guard = NA_character_)
}

.ard_chain_value <- function(chain, s, round_type) {
  .ard_chain_pick(chain, s, round_type)$v
}

# The stub's own small language.  `cells` has had guarded templates since
# the overall-response table needed a cell to depend on a value; the stub had
# nothing, so "indent the severities under Any" was done by rewriting `.label`
# with paste0(), which is string surgery on a column that means something and
# which doubles its own indent if it runs twice.  A label template reads the
# record's columns, plus `.label` bound to the label this row would otherwise
# carry, so the rule is a declaration instead.
.ard_label_text <- function(chain, s, lab) {
  gd <- .ard_guard_data(s)
  gd[[".label"]] <- lab
  for (el in chain) {
    if (!is.null(el$cond)) {
      ok <- isTRUE(tryCatch(all(eval(el$cond, gd, el$env)),
                            error = function(e) FALSE))
      if (!ok) next
    }
    out <- el$tpl
    for (tk in .ard_tokens(el$tpl)) {
      nm <- gsub("^[{]|[}]$", "", tk)
      v <- gd[[nm]]
      if (is.null(v)) return(NA_character_)
      out <- sub(tk, as.character(v)[1L], out, fixed = TRUE)
    }
    return(out)
  }
  NA_character_
}

# A `cells` entry is one of
#   "n ({p})"                      one row, label taken from `label`
#   c("a", "b")                    one row, first template that resolves wins
#   c(n == 0 ~ "0", "{n} ({p})")   the same chain, its first element guarded
#   c("Mean (SD)" = "...", ...)    one row per element, label = the name
#   ard_cells("1" = c(...), ...)   the same, each element a chain of its own
# Returns list(labels = <chr|NULL>, chains = <list of chains>).
.ard_cell_entry <- function(entry) {
  if (is.null(entry)) return(NULL)
  if (inherits(entry, "ard_cells")) {
    return(list(labels = names(entry),
                chains = lapply(unclass(entry), .ard_chain)))
  }
  nms <- names(entry)
  if (is.null(nms) || !any(nzchar(nms))) {
    return(list(labels = NULL, chains = list(.ard_chain(entry))))
  }
  list(labels = nms,
       chains = lapply(seq_along(entry), function(i) .ard_chain(entry[[i]])))
}

# ---------------------------------------------------------------------------
#  Classifying a summary WITHOUT trusting the `context` string
# ---------------------------------------------------------------------------
#  `context` is a cards implementation detail and it moves: ard_continuous()
#  stamps "continuous" but the 0.9 rename, ard_summary(), stamps "summary";
#  ard_categorical() stamps "categorical" but ard_tabulate() stamps "tabulate".
#  Keying `cells` on it alone therefore ties a script to one cards generation,
#  and silently produces no cells at all against another.
#
#  So each summary is *also* classified from what its rows actually contain --
#  `.kind`, which is "categorical" when the summary has levels to enumerate
#  (any non-missing `variable_level`: a factor, a dichotomous value of
#  interest, a hierarchy term) and "continuous" when it does not (one row per
#  statistic of one numeric variable).  That reading is structural, so it holds
#  across every cards version, past and future.
#
#  A summary is one variable under one `context`, not the variable alone: the
#  same variable may be summarised AND tabulated in one ARD (a visit count,
#  a score), and asking "does the variable have levels" once would call its
#  mean and SD rows categorical as well.  The context string is used only to
#  tell the summaries apart -- never read for what it says.
#
#  `cells` is then matched in this order:
#     1. the analysis variable's own name
#     2. `context` -- with the known spellings treated as equivalent
#     3. `.kind`   -- likewise
#     4. "default"
#  Step 2 keeps a context-specific entry (cardx's "proportion_ci", "survival",
#  "stats_t_test", ...) winning where the caller wrote one; step 3 is what
#  makes `continuous` / `categorical` keep working when cards renames a verb
#  again.
.ard_kind <- function(d) {
  if (!nrow(d)) return(character(0))
  ctx <- if ("context" %in% names(d)) as.character(d$context)
         else rep(NA_character_, nrow(d))
  v <- paste(as.character(d$variable), ctx, sep = "\r")
  has_lv <- tapply(!is.na(d$variable_level), v, any)
  out <- ifelse(as.logical(has_lv[v]), "categorical", "continuous")
  out[is.na(out)] <- "continuous"
  unname(out)
}

# The spellings cards has used for the same idea, in both directions.  A name
# it has never used is returned unchanged.
.ard_context_aliases <- function(x) {
  switch(x,
         summary     = ,
         continuous  = c("continuous", "summary"),
         tabulate    = ,
         categorical = c("categorical", "tabulate"),
         x)
}

# Why no cell was produced.  The bare "no cell matched" that this replaces was
# useless: the two causes look identical from the outside, and the commonest
# one -- a `cells` list keyed on `continuous` / `categorical` against an ARD
# built with cards 0.9's ard_summary() / ard_tabulate() -- is invisible unless
# the message says which contexts the ARD actually carries.
.ard_no_cell_message <- function(d, cells, stats) {
  ctx  <- .ard_first_seen(d$context)
  vars <- .ard_first_seen(d$variable)
  keys <- if (is.character(cells) && is.null(names(cells))) character(0)
          else names(cells)
  msg <- c(
    "No cell was produced, so there is nothing to spread.",
    sprintf("  `cells` is keyed on : %s",
            if (!length(keys)) "(one template for everything)"
            else paste(sQuote(keys), collapse = ", ")),
    sprintf("  ARD contexts        : %s",
            if (!length(ctx)) "(none -- every row was filtered out)"
            else paste(sQuote(ctx), collapse = ", ")),
    sprintf("  ARD variables       : %s",
            paste(sQuote(utils::head(vars, 8)), collapse = ", ")),
    sprintf("  structural kinds    : %s",
            if (!".kind" %in% names(d)) "(not computed)"
            else paste(sQuote(.ard_first_seen(d$.kind)), collapse = ", ")))
  if (!nrow(d)) {
    msg <- c(msg,
      "Every row was dropped before the cells were built: check `cols` (a key",
      "whose value is missing on every row removes the row).")
  } else if (length(keys)) {
    kinds <- if (".kind" %in% names(d)) .ard_first_seen(d$.kind) else character(0)
    msg <- c(msg,
      "None of the `cells` names matched. A name is matched against the",
      "analysis variable, then the context, then the structural kind",
      "('continuous' / 'categorical', read from the rows rather than from the",
      "context string), then 'default'.  Use one of the names listed above,",
      sprintf("one of %s, or add a `default` entry.",
              if (!length(kinds)) "'continuous' / 'categorical'"
              else paste(sQuote(kinds), collapse = " / ")))
  }
  paste(msg, collapse = "\n")
}

# The label column's level order, assembled variable by variable: an explicit
# `levels[[<variable>]]` where the caller gave one, else the row names of that
# variable's `cells` entry (the `Mean (SD)` / `Min, Max` lines), else its labels
# as first seen.  Rows are sorted by the row keys before the label, so a
# variable's labels only ever compete with labels of the same variable and one
# concatenated order serves them all.
.ard_label_order <- function(d, cells, levels, labels) {
  vars <- .ard_first_seen(d$variable)
  if (!is.null(labels)) {              # `labels` also fixes the variable order
    named <- intersect(names(labels), vars)
    vars  <- c(named, setdiff(vars, named))
  }
  out <- character(0)
  for (v in vars) {
    lv <- if (is.null(levels)) NULL else levels[[v]]
    if (is.null(lv)) {
      sub   <- d[d$variable == v, , drop = FALSE]
      if (!nrow(sub)) next
      entry <- .ard_lookup_cells(
        cells, v, sub$context[1L],
        if (".kind" %in% names(sub)) sub$.kind[1L] else NA_character_)
      lv <- if (!is.null(entry) && !is.null(entry$labels)) entry$labels
            else .ard_first_seen(sub$.label)
    }
    out <- c(out, lv)
  }
  unique(as.character(out))
}

# A `cells` that is not a list is ONE entry, used for everything; its names, if
# any, are row labels.  A `cells` that IS a list is a map keyed by variable /
# context / kind / "default", and each of its elements is such an entry.  The
# two cannot be told apart by looking for names -- `c("Mean (SD)" = ...)` is a
# named vector meaning two rows -- so the container type decides.
# The message for a row identity that does not separate two summaries.  It has
# to name both source variables and both values, because the symptom the caller
# sees otherwise -- a table with a quarter of the expected rows, carrying the
# last variable's numbers -- says nothing about where the numbers went.
.ard_clash_message <- function(clash, long, base, ri, ci, id_cols) {
  k    <- clash$k
  prev <- clash$prev
  old  <- clash$old
  new  <- clash$new
  where <- paste(vapply(id_cols, function(cn)
    sprintf("%s = %s", cn, sQuote(as.character(base[[cn]][ri[k]]))), ""),
    collapse = ", ")
  vars <- unique(c(long$.var[prev], long$.var[k]))
  paste(c(
    "Two different values landed in the same cell, so these rows are not",
    "telling themselves apart.",
    sprintf("  cell        : %s, column %s", where, sQuote(long$.col[k])),
    sprintf("  first value : %s   (from %s)", sQuote(as.character(old)),
            sQuote(long$.var[prev])),
    sprintf("  second value: %s   (from %s)", sQuote(as.character(new)),
            sQuote(long$.var[k])),
    "A row is identified by the `rows` keys plus the label.  Here that is not",
    sprintf("enough: %s produce the same label.",
            paste(sQuote(vars), collapse = " and ")),
    "Add the key that separates them -- for a flat summary that is the analysis",
    "variable itself, `rows = c(group = \"variable\")`."),
    collapse = "\n")
}

.ard_lookup_cells <- function(cells, variable, context, kind = NA_character_) {
  # A list is a map only when its elements are NAMED: `c()` over guards
  # returns an unnamed list, and that is one chain, not a map.
  if (inherits(cells, "ard_cells")) return(.ard_cell_entry(cells))
  if (!is.list(cells) || !any(nzchar(names(cells) %||% ""))) {
    return(.ard_cell_entry(cells))
  }
  keys <- c(.ard_summary_key(variable, context), variable)
  if (!is.na(context)) keys <- c(keys, .ard_context_aliases(context))
  if (!is.na(kind))    keys <- c(keys, .ard_context_aliases(kind))
  for (k in c(unique(keys), "default")) {
    if (!is.na(k) && k %in% names(cells) && !is.null(cells[[k]])) {
      return(.ard_cell_entry(cells[[k]]))
    }
  }
  NULL
}

# The key of one summary -- a variable under one context -- in a cells map.
# Written only by the plan, when one variable is two summaries that need two
# recipes; the carriage return keeps it from ever meeting a name somebody
# typed.
.ard_summary_key <- function(variable, context) {
  if (is.na(variable) || is.na(context)) return(NA_character_)
  paste(variable, context, sep = "\r")
}


# ============================================================================
#  ard_keys()
# ============================================================================

#' What is actually inside an ARD
#'
#' Prints, and returns invisibly, the structural facts you need in order to
#' call [ard_normalize()] and [ard_spread()]: the grouping-variable names
#' that appear in the `group1..groupN` columns, the analysis variables, the
#' `context` values and the statistics each context carries.  All of it is read from the tibble's
#' rows -- never from the object's attributes.
#'
#' @param ard A cards/cardx ARD (any data frame with the ARD columns).
#'
#' @return Invisibly, a list with elements `keys`, `variables`, `contexts` and
#'   `stats` (a data frame of context / stat_name / stat_label).
#'
#' @section Lifecycle:
#' **Experimental.**  See [rtfreporter-ard].
#'
#' @seealso [ard_normalize()], [ard_spread()], [ard_template()]
#' @export
ard_keys <- function(ard) {
  d <- as.data.frame(ard, stringsAsFactors = FALSE)
  gcols <- grep("^group[0-9]+$", names(d), value = TRUE)
  keys <- unique(unlist(lapply(gcols, function(g) .ard_first_seen(d[[g]]))))
  vars <- .ard_first_seen(.ard_unlist_col(d$variable))
  ctx  <- .ard_first_seen(d$context)
  sn   <- .ard_unlist_col(d$stat_name)
  sl   <- .ard_unlist_col(d$stat_label)
  stats <- unique(data.frame(context = as.character(d$context),
                             stat_name = as.character(sn),
                             stat_label = as.character(sl),
                             stringsAsFactors = FALSE))
  stats <- stats[order(stats$context), , drop = FALSE]
  rownames(stats) <- NULL

  # The structural classification -- what `cells` should normally be keyed on,
  # because unlike `context` it does not move when cards renames a verb.
  norm  <- try(ard_normalize(ard, drop_key_variables = FALSE), silent = TRUE)
  kinds <- NULL
  if (!inherits(norm, "try-error")) {
    kinds <- unique(data.frame(variable = as.character(norm$variable),
                               kind     = as.character(norm$.kind),
                               context  = as.character(norm$context),
                               stringsAsFactors = FALSE))
    kinds <- kinds[!duplicated(kinds$variable), , drop = FALSE]
    rownames(kinds) <- NULL
  }

  cat("ARD keys (group1..groupN values) :", paste(keys, collapse = ", "), "\n")
  cat("Analysis variables               :", paste(vars, collapse = ", "), "\n")
  cat("Contexts (cards-version specific):", paste(ctx, collapse = ", "), "\n")
  if (!is.null(kinds)) {
    cat("Structural kinds (stable)        :",
        paste(.ard_first_seen(kinds$kind), collapse = ", "), "\n")
    cat("\nPer variable -- key `cells` on `kind` unless you need the context:\n")
    print(kinds, row.names = FALSE)
  }
  cat("\nStatistics per context:\n")
  print(stats, row.names = FALSE)
  invisible(list(keys = keys, variables = vars, contexts = ctx,
                 kinds = kinds, stats = stats))
}


# ============================================================================
#  ard_overall()
# ============================================================================

#' Where the table's overall row comes from
#'
#' An adverse-events table opens with a "subjects with at least one event"
#' row, and where that row lives in the ARD depends on how the ARD was built.
#' `cards::ard_stack_hierarchical(over_variables = TRUE)` writes it as rows
#' whose variable is the sentinel `..ard_hierarchical_overall..`.  Build the
#' same table by summarising each level separately and binding the results,
#' and there is no sentinel: the overall block counts the **treatment itself**,
#' so the arm sits in `variable` / `variable_level` with no grouping pair at
#' all.  The two are indistinguishable from the shape of the ARD, so which one
#' this is has to be said.
#'
#' @param label Text for the overall row, e.g. `"Any TEAE"`.  It is written
#'   into the first `hierarchy` column, so the row sits at the top level
#'   beside the other outermost rows.
#' @param from The analysis variable that block summarised -- the treatment
#'   variable, usually.  `NULL` (default) means the cards sentinel.
#'
#' @return An object of class `ard_overall`, for [ard_normalize()]'s
#'   `overall` argument.  That argument also takes a bare string, which is
#'   `ard_overall(label)`.
#'
#' @section Lifecycle:
#' **Experimental.**  See [rtfreporter-ard].
#'
#' @examples
#' ard_overall("Any TEAE")                      # the cards sentinel rows
#' ard_overall("Any TEAE", from = "TRT01P")     # a separately-built block
#' @seealso [ard_normalize()], [ard_spread()]
#' @export
ard_overall <- function(label, from = NULL) {
  if (missing(label) || !is.character(label) || length(label) != 1L) {
    .ard_stop("`label` is required: one string for the overall row.")
  }
  structure(list(label = label, from = as.character(from)),
            class = "ard_overall")
}

# `overall` as given -- NULL, a bare string, or ard_overall() -- as one list.
.ard_overall_spec <- function(x) {
  if (is.null(x)) return(list(label = NULL, from = character(0)))
  if (inherits(x, "ard_overall")) return(x)
  if (is.character(x) && length(x) == 1L) {
    return(list(label = x, from = character(0)))
  }
  .ard_stop(paste0("`overall` must be a single string or ard_overall(); see ",
                   "?ard_overall."))
}


# ============================================================================
#  ard_cells()
# ============================================================================

#' Several rows in one cell recipe, each with its own fallback chain
#'
#' `cells` already reads three containers: one template is one row, a *named*
#' character vector is one row per element, and a `list()` is a map looked up
#' by analysis variable.  The one shape those cannot spell is a **named row
#' whose value is itself a chain** -- `c("1" = c(a, b))` is flattened by `c()`
#' before `ard_spread()` ever sees it.  `ard_cells()` is that shape, and only
#' that shape.
#'
#' Each argument is one output row: its name is the row label, its value is a
#' template, a chain of templates, or a chain with guards.  Reading a recipe
#' then goes `list()` for *which variable*, `ard_cells()` for *which row*,
#' `c()` for *which template to try first*.
#'
#' @param ... One argument per output row.  Names become row labels; an
#'   unnamed argument takes its label from `label`, as a bare template does.
#'
#' @return An object of class `ard_cells`, for `cells` in [ard_spread()].
#'
#' @section Lifecycle:
#' **Experimental.**  See [rtfreporter-ard].
#'
#' @examples
#' # an estimate line and a confidence-interval line, the first guarded so a
#' # count of zero prints as "0" rather than "0 (0.0)"
#' ard_cells(
#'   "1" = c(n == 0 ~ "0", "{n:.0f} ({estimate:.1f%})"),
#'   "2" = "{conf.low:.1f%}, {conf.high:.1f%}")
#' @seealso [ard_spread()], [rtfreporter-ard]
#' @export
ard_cells <- function(...) {
  x <- list(...)
  if (!length(x)) .ard_stop("`ard_cells()` needs at least one row.")
  nms <- names(x)
  if (is.null(nms)) nms <- rep("", length(x))
  structure(stats::setNames(x, nms), class = "ard_cells")
}

#' @export
print.ard_cells <- function(x, ...) {
  cat("<ard_cells>", length(x), "row(s)
")
  nms <- names(x)
  for (i in seq_along(x)) {
    lab <- if (nzchar(nms[i])) nms[i] else "(from `label`)"
    tpl <- vapply(.ard_chain(x[[i]]), function(el)
      if (is.null(el$cond)) el$tpl
      else paste(deparse(el$cond), "~", encodeString(el$tpl, quote = "\"")),
      "")
    cat(sprintf("  %-14s %s
", lab, paste(tpl, collapse = "  |  ")))
  }
  invisible(x)
}


# ============================================================================
#  ard_pull()
# ============================================================================
#
#  Replaces ard_big_n(), which guessed.  "bigN" is tfrmt's word, not cards',
#  and the value it names has no fixed home in an ARD.  Measured across eight
#  ways of building the same table, the per-arm denominator (86 / 84 / 84 for
#  ADSL) turns up as, variously:
#
#     SEX    tabulate      N       the denominator on a categorical summary
#     AGE    summary       N       the denominator on a continuous summary
#     AESOC  hierarchical  N       the denominator on a hierarchical summary
#     TRT    tabulate      n       the by-variable's OWN count
#     AGE    summary       bigN    a statistic the author wrote themselves
#
#  and in the same ARD `stat_name == "N"` is the STUDY total on the
#  by-variable's own rows (254) while it is the per-arm denominator (86)
#  everywhere else.  A heuristic over `stat_name` was wrong in five of the
#  eight builds -- silently, with a plausible number.
#
#  So: name the statistic, and when the ARD offers more than one answer, say
#  which.  A wrong guess in a column header is a wrong number in a clinical
#  table, so this errors with the menu rather than picking.

#' Pull a statistic out of an ARD, keyed like the spread columns
#'
#' An ARD carries more than the table's body: the denominator behind every
#' percentage, the subject count per arm, a total the author computed
#' themselves.  Those belong in the **column header** (`Placebo\\nN = 86`) or in
#' an overall row, not in a body cell, so [ard_spread()] puts them nowhere.
#' `ard_pull()` reads one out, keyed exactly like the spread columns --
#' `"Placebo____F"` for a crossed header -- ready to paste into a `col_header`.
#'
#' Taking the number from the ARD rather than counting the data again is the
#' point: it is by construction the number the percentages used.
#'
#' @section Which N:
#' There is no one place an ARD keeps "the N".  `cards::ard_stack(.by = TRT)`
#' writes the per-arm count as `n` on the by-variable's own rows and the
#' **study** total as `N` on those same rows, while every summary row carries
#' its own denominator as `N`; `ard_total_n()` adds `..ard_total_n..`; and an
#' author may compute their own, as the `bigN` statistic in the solicited-AE
#' example on Discussion #473.  Guessing between them produces a plausible
#' wrong number in a column header, so this function does not guess:
#'
#' * `stat` names the statistic, and defaults to `"N"`.
#' * The **key variables' own tabulations are excluded** by default, because
#'   there `N` is the study total rather than the column's denominator.  Set
#'   `variable` to one of them to ask for those rows on purpose.
#' * If what is left still offers more than one answer, the call **fails with
#'   the candidates listed**, so you can pin it with `variable` / `context`.
#'
#' @param ard A cards/cardx ARD.
#' @param cols The column key(s), named as in [ard_spread()] -- the grouping
#'   variable's own name, not its `group*` position.
#' @param stat Statistic to read; `"N"` by default.
#' @param variable,context Restrict to this analysis variable and/or this
#'   `context`.  Use them when the error message says the choice is ambiguous,
#'   or to ask for the key variable's own rows.
#' @param levels Optional level order for the keys, so the result lines up
#'   with the table's columns.
#' @param sep Separator between multiple `cols` keys; match [ard_spread()].
#'
#' @return A named vector, one element per column key.
#'
#' @section Lifecycle:
#' **Experimental.**  See [rtfreporter-ard].
#'
#' @examples
#' if (requireNamespace("cards", quietly = TRUE)) {
#'   adsl <- cards::ADSL
#'   adsl$TRT <- as.character(adsl$ARM)
#'   ard <- cards::ard_stack(adsl, .by = TRT,
#'                           cards::ard_continuous(variables = AGE))
#'   n <- ard_pull(ard, cols = "TRT")            # the denominator, per arm
#'   paste0(names(n), "\\nN = ", n)
#' }
#' @seealso [ard_keys()], which lists every statistic an ARD carries;
#'   [ard_spread()]
#' @export
ard_pull <- function(ard, cols, stat = "N", variable = NULL, context = NULL,
                     levels = NULL, sep = "____") {
  raw   <- as.data.frame(ard, stringsAsFactors = FALSE)
  gcols <- grep("^group[0-9]+$", names(raw), value = TRUE)
  have  <- unique(c(unlist(lapply(gcols, function(g) .ard_first_seen(raw[[g]]))),
                    .ard_first_seen(.ard_unlist_col(raw[["variable"]]))))
  unknown <- setdiff(cols, have)
  if (length(unknown)) {
    .ard_stop(sprintf(
      "`cols`: no key %s in this ARD.  It has: %s.  ard_keys() lists them.",
      paste(sQuote(unknown), collapse = ", "),
      if (length(have)) paste(sQuote(utils::head(have, 12)), collapse = ", ")
      else "(none)"))
  }

  d <- ard_normalize(ard, keys = cols, drop_key_variables = FALSE,
                     drop_contexts = "attributes")
  key <- do.call(paste, c(lapply(cols, function(k) as.character(d[[k]])),
                          list(sep = sep)))
  ok  <- Reduce(`&`, lapply(cols, function(k) !is.na(d[[k]])))
  d   <- d[ok, , drop = FALSE]
  key <- key[ok]
  d$stat <- suppressWarnings(as.numeric(as.character(d$stat)))

  sel <- !is.na(d$stat_name) & d$stat_name == stat & !is.na(d$stat)
  if (!is.null(variable)) sel <- sel & d$variable %in% variable
  else sel <- sel & !d$variable %in% cols
  if (!is.null(context)) sel <- sel & d$context %in% context

  if (!any(sel)) .ard_stop(.ard_pull_menu(d, key, stat, cols, found = FALSE))

  s   <- d[sel, , drop = FALSE]
  k   <- key[sel]
  grp <- paste(s$variable, s$context, sep = "\r")
  cand <- lapply(split(seq_len(nrow(s)), grp), function(i)
    vapply(split(s$stat[i], k[i]), max, numeric(1)))
  if (length(cand) > 1L) {
    first <- cand[[1L]]
    same  <- vapply(cand[-1L], function(z)
      isTRUE(all.equal(z[names(first)], first)), logical(1))
    if (!all(same)) {
      .ard_stop(.ard_pull_menu(d, key, stat, cols, found = TRUE, cand = cand))
    }
  }
  out <- cand[[1L]]
  ord <- if (is.null(levels)) .ard_key_order(d, cols, key) else {
    lv <- levels[[cols[1L]]] %||% levels[[1L]]
    c(intersect(lv, names(out)), setdiff(names(out), lv))
  }
  out[intersect(ord, names(out))]
}

# The menu an ambiguous or empty pull prints: every (variable, context,
# stat_name) this ARD offers for these columns, with its values, so the caller
# can point at one instead of being handed a guess.
.ard_pull_menu <- function(d, key, stat, cols, found, cand = NULL) {
  agg <- unique(d[!is.na(d$stat), c("variable", "context", "stat_name")])
  agg <- agg[order(agg$variable, agg$context, agg$stat_name), , drop = FALSE]
  lines <- character(0)
  for (i in seq_len(nrow(agg))) {
    sel <- d$variable == agg$variable[i] & d$context == agg$context[i] &
      d$stat_name == agg$stat_name[i] & !is.na(d$stat)
    if (!any(sel)) next
    v <- vapply(split(d$stat[sel], key[sel]), max, numeric(1))
    lines <- c(lines, sprintf("    variable = %-14s context = %-14s stat = %-8s -> %s",
                              sQuote(agg$variable[i]), sQuote(agg$context[i]),
                              sQuote(agg$stat_name[i]),
                              paste(signif(unname(v), 6), collapse = " / ")))
  }
  head_txt <- if (!found) {
    c(sprintf("No %s found for these columns.", sQuote(stat)),
      sprintf("  (the key variables' own rows -- %s -- are excluded unless you",
              paste(sQuote(cols), collapse = ", ")),
      "   name one with `variable =`, because their `N` is the study total.)")
  } else {
    c(sprintf("This ARD offers more than one %s for these columns, and they",
              sQuote(stat)),
      "disagree.  Say which with `variable =` and/or `context =`.")
  }
  paste(c(head_txt, "", "  candidates, keyed by the spread columns:", lines),
        collapse = "\n")
}


# ============================================================================
#  ard_normalize()
# ============================================================================

#' Flatten an ARD into an explicitly keyed long table
#'
#' Step one of the ARD conversion: turn the `group1 / group1_level` *positional*
#' pairs into columns named after the grouping variables, flatten every
#' list-column, attach the formatted statistic (`stat_fmt`), and -- when the ARD
#' is hierarchical -- fold each level's own summary rows into the right key
#' column and record the nesting depth.
#'
#' The result is a plain data frame that you can keep manipulating with base R
#' or dplyr before handing it to [ard_spread()].  That is the intended route
#' for anything [ard_spread()] does not do by itself: marginal totals, derived
#' rows, custom sorting.
#'
#' @param ard A cards/cardx ARD.
#' @param keys Character vector of grouping-variable names to materialise as
#'   columns.  `NULL` (default) uses every name found in the `group*` columns.
#'   This reads the tibble's rows, not its attributes.
#' @param hierarchy Character vector naming a nested hierarchy, outermost
#'   first -- e.g. `c("AEBODSYS", "AEDECOD")`.  For these variables the rows
#'   where `variable == <name>` are that level's own summary rows, and are
#'   folded into the matching key column.
#' @param overall Label to give the `..ard_hierarchical_overall..` rows
#'   produced by `cards::ard_stack_hierarchical(over_variables = TRUE)`, e.g.
#'   `"Any TEAE"`.  They are placed on the first `hierarchy` column.  `NULL`
#'   (default) leaves them alone.
#' @param drop_contexts `context` values to discard.  The default drops the
#'   `attributes` rows (whose `stat` is a vector and cannot be flattened) and
#'   the `total_n` row.  Dropping the total does not lose the NUMBER: when
#'   `..ard_total_n..` states one, it is kept on the `"ard_total_n"`
#'   attribute, because it is a denominator a column header asks for even
#'   though it is not a table statistic.  Name only `"attributes"` to keep
#'   the row itself.
#' @param drop_key_variables The rows that describe a key variable itself --
#'   the `context == "tabulate"` counts of the by-variable -- are no table
#'   cell, but they are often the only place an ARD states each column's
#'   size.  `FALSE` (default) keeps them and marks them `.key_own = TRUE`, so
#'   [ard_spread()] leaves them out of the body while a column header can
#'   still read them.  `TRUE` removes them outright.
#'
#' @section Working on the result before [ard_spread()]:
#' The result is a plain data frame; reshaping it in between is the point of
#' the two-stage split.  **Everything [ard_spread()] reads is in the
#' columns**, so `dplyr::mutate()`, `filter()`, `arrange()`, `bind_rows()`,
#' `select()` and base `[` are all safe, and so is a one-pipe
#' `ard_normalize() |> ... |> ard_spread()`.  A manipulation that really does
#' break the rows is still caught -- two values arriving in one cell is an
#' error, not a silent overwrite.
#'
#' One attribute is left, `"ard_ignored"`, and nothing reads it to build the
#' table: it is a report about rows that are no longer in the frame, so there
#' is no column it could be.  Dropping it (`mutate()` and `select()`, base
#' `transform()` and `subset()` do) only makes `notes` report the discards
#' from [ard_spread()] alone.
#'
#' @section Factor levels:
#' \pkg{cards} stores the level of a factor variable as a one-element factor,
#' so the flattening has to take the label: a level comes back as `"<65"`, not
#' as the integer code `1`.  The order the factor declared is kept too, as the
#' `.label_order` column -- each row's position within its variable's declared
#' levels -- and [ard_spread()] rebuilds that variable's row order from it
#' unless `levels` says otherwise.  Relabelling a level keeps its position,
#' so indenting `"Mild"` to `"  Mild"` with `mutate()` still sorts where
#' `"Mild"` was declared.
#'
#' A **key** column holds one variable, so it can carry its order itself: a
#' key that was a factor in the data (a treatment variable declared
#' `factor(levels = c("Low", "Placebo", "High"))`) comes back a factor with
#' those levels, unused ones included, and [ard_spread()] lays the columns --
#' or the rows, for a row key -- out in that order however the frame was
#' reordered in between.  `levels` still overrides it.  A key that was
#' character stays character.
#'
#' @return A data frame with one row per ARD statistic: the key columns, the
#'   ARD's own `variable` / `variable_level` / `context` / `stat_name` /
#'   `stat_label` / `stat` / `stat_fmt`, the structural classification `.kind`
#'   (`"continuous"` / `"categorical"`, see [rtfreporter-ard]), `.label` (the
#'   deepest non-missing hierarchy value, or `variable_level`), `.label_order`
#'   (that label's position in the level order its factor declared, `NA` when
#'   it declared none), `.overall`, `.key_own` (`TRUE` on a key variable's
#'   own tabulation, see `drop_key_variables`), and `.depth` --- 1 =
#'   outermost within a
#'   `hierarchy`, 0 = a row of one that is not one of its levels, and `NA`
#'   throughout when no `hierarchy` was given, since depth only means
#'   something inside one.
#'
#' @section Lifecycle:
#' **Experimental.**  See [rtfreporter-ard].
#'
#' @seealso [ard_spread()], [ard_keys()]
#' @export
ard_normalize <- function(ard, keys = NULL, hierarchy = character(),
                          overall = NULL,
                          drop_contexts = c("attributes", "total_n"),
                          drop_key_variables = FALSE) {
  d <- as.data.frame(ard, stringsAsFactors = FALSE)
  if (!"context" %in% names(d) || !"stat_name" %in% names(d)) {
    .ard_stop("`ard` does not look like a cards ARD (no `context`/`stat_name`).")
  }
  ignored <- NULL
  total_n <- NULL
  if (length(drop_contexts)) {
    gone <- d[d$context %in% drop_contexts, , drop = FALSE]
    ignored <- .ard_ignored_bind(
      ignored, .ard_tally(gone, "not a table statistic"))
    # The row goes, the NUMBER stays: `..ard_total_n..` is not a table
    # statistic, but it is the denominator a column header asks for,
    # and dropping it is no reason to lose it.
    total_n <- .ard_total_n_value(gone)
    d <- d[!d$context %in% drop_contexts, , drop = FALSE]
  }

  # cards' own formatter, when available and not already applied
  if (!"stat_fmt" %in% names(d) && requireNamespace("cards", quietly = TRUE)) {
    fmt <- if ("apply_fmt_fun" %in% getNamespaceExports("cards")) {
      cards::apply_fmt_fun
    } else if ("apply_fmt_fn" %in% getNamespaceExports("cards")) {
      get("apply_fmt_fn", envir = asNamespace("cards"))
    } else NULL
    if (!is.null(fmt)) {
      got <- try(as.data.frame(fmt(ard), stringsAsFactors = FALSE), silent = TRUE)
      if (!inherits(got, "try-error") && "stat_fmt" %in% names(got) &&
          nrow(got) == nrow(as.data.frame(ard))) {
        got <- got[!got$context %in% drop_contexts, , drop = FALSE]
        if (nrow(got) == nrow(d)) d$stat_fmt <- got$stat_fmt
      }
    }
  }
  if (!"stat_fmt" %in% names(d)) d$stat_fmt <- NA

  # Fallback: cards' formatter needs cards' own class, so a plain data frame
  # (or an ARD that lost its class on the way here) gets its `fmt_fun` applied
  # directly.  cards stores either a function or a decimal count there.
  fmt_col <- intersect(c("fmt_fun", "fmt_fn"), names(d))[1L]
  if (all(is.na(d$stat_fmt)) && !is.na(fmt_col) && is.list(d[[fmt_col]])) {
    raw <- d$stat
    fmt <- d[[fmt_col]]
    d$stat_fmt <- vapply(seq_len(nrow(d)), function(i) {
      v <- if (is.list(raw)) raw[[i]] else raw[i]
      f <- fmt[[i]]
      if (is.null(v) || length(v) != 1L || is.na(v)) return(NA_character_)
      if (is.function(f)) {
        got <- try(f(v), silent = TRUE)
        return(if (inherits(got, "try-error") || length(got) != 1L)
          NA_character_ else as.character(got))
      }
      if (is.numeric(f) && length(f) == 1L && !is.na(f)) {
        return(sprintf(paste0("%.", as.integer(f), "f"), as.numeric(v)))
      }
      as.character(v)
    }, "")
  }

  fct_levels <- .ard_factor_levels(d)
  key_levels <- .ard_group_factor_levels(d) %||% list()

  for (nm in names(d)) {
    if (is.list(d[[nm]])) d[[nm]] <- .ard_unlist_col(d[[nm]])
  }
  if ("stat" %in% names(d) && .ard_is_numericish(d$stat)) {
    d$stat <- suppressWarnings(as.numeric(as.character(d$stat)))
  }
  d$stat_fmt <- as.character(d$stat_fmt)
  d$variable <- as.character(d$variable)
  if ("variable_level" %in% names(d)) {
    d$variable_level <- as.character(d$variable_level)
  } else {
    d$variable_level <- NA_character_
  }

  gcols <- grep("^group[0-9]+$", names(d), value = TRUE)
  if (is.null(keys)) {
    keys <- unique(unlist(lapply(gcols, function(g) .ard_first_seen(d[[g]]))))
  }
  keys <- setdiff(unique(c(keys, hierarchy)), .ard_structural())

  # A frame that has been through here already has flat `groupN_level`
  # strings; its declared order is on the key column itself.
  for (k in keys) {
    if (is.null(key_levels[[k]]) && is.factor(d[[k]])) {
      key_levels[[k]] <- base::levels(d[[k]])
    }
  }
  for (k in keys) d[[k]] <- NA_character_
  for (g in gcols) {
    lv <- paste0(g, "_level")
    if (!lv %in% names(d)) next
    gv <- as.character(d[[g]])
    for (k in keys) {
      hit <- !is.na(gv) & gv == k
      if (any(hit)) d[[k]][hit] <- as.character(d[[lv]][hit])
    }
  }

  # A key can reach the ARD two ways: as a `by` variable, in a group pair --
  # handled above -- or as the ANALYSED variable, when a block was built by
  # summarising it.  A hierarchy level's own summary rows are the familiar
  # case, but any key can arrive that way: bind three separately-built
  # summaries for an adverse-events table and the "subjects with at least one
  # TEAE" block counts the treatment itself, so the arm sits in `variable` /
  # `variable_level` with no group pair at all.  Fill the key column from
  # there too; `drop_key_variables` still decides whether the rows stay.
  for (k in unique(c(hierarchy, keys))) {
    hit <- !is.na(d$variable) & d$variable == k
    if (any(hit)) d[[k]][hit] <- d$variable_level[hit]
  }

  ovs <- .ard_overall_spec(overall)
  d$.overall <- FALSE
  ov <- !is.na(d$variable) & d$variable == "..ard_hierarchical_overall.."
  if (length(ovs$from)) {
    ov <- ov | (!is.na(d$variable) & d$variable %in% ovs$from)
  }
  if (!is.null(ovs$label) && any(ov) && length(hierarchy)) {
    d[[hierarchy[1L]]][ov] <- ovs$label
    d$.overall[ov] <- TRUE
  } else if (any(ov) && is.null(ovs$label)) {
    ignored <- .ard_ignored_bind(
      ignored, .ard_tally(d[ov, , drop = FALSE],
                          "an overall block, and no `overall =` was given"))
    d <- d[!ov, , drop = FALSE]
    ov <- rep(FALSE, nrow(d))
  }

  if (length(hierarchy)) {
    d$.depth <- NA_integer_
    d$.label <- NA_character_
    for (i in seq_along(hierarchy)) {
      hit <- !is.na(d[[hierarchy[i]]])
      d$.label[hit] <- d[[hierarchy[i]]][hit]
      d$.depth[hit] <- i
    }
    d$.depth[d$.overall] <- 1L
    # 0, not NA: inside a declared hierarchy every row has an answer, even
    # "not one of its levels".  That keeps "is there a hierarchy here?" a
    # question about this column, which survives any manipulation, rather
    # than about an attribute, which does not.
    d$.depth[is.na(d$.depth)] <- 0L
    # A depth-0 row is a variable the hierarchy does not cover -- `SEX` beside
    # a nested `ARACE`/`ASRACE`, which is what a demographics table looks like
    # once one characteristic gains a third level.  Its own level is still the
    # only label it has, so fall back to it rather than leaving the row
    # unlabelled and forcing the caller to normalize twice and rbind.
    flat <- d$.depth == 0L & is.na(d$.label)
    if (any(flat)) d$.label[flat] <- d$variable_level[flat]
  } else {
    # NA, not 1: depth only means something inside a hierarchy, and a column
    # that says "there is no hierarchy here" survives every manipulation the
    # caller may do between ard_normalize() and ard_spread(), where an
    # attribute does not.
    d$.depth <- NA_integer_
    d$.label <- d$variable_level
  }

  # A key variable's own tabulation is no table cell, but it is often the
  # only place the ARD states each column's size -- the per-arm `n` of
  # ard_stack(.by = ) -- so it is MARKED, not removed: ard_spread() leaves
  # it out of the body and a column header can still read it.  A column,
  # like every other mark, so it survives whatever happens in between.
  own <- d$variable %in% setdiff(keys, c(hierarchy, ovs$from))
  if (drop_key_variables) {
    ignored <- .ard_ignored_bind(
      ignored, .ard_tally(d[own, , drop = FALSE],
                          "a key variable's own tabulation"))
    d <- d[!own, , drop = FALSE]
    own <- rep(FALSE, nrow(d))
  }
  d$.key_own <- own

  # A key column holds ONE variable, so the order its factor declared can
  # ride on the column itself -- unlike `variable_level`, where every
  # variable's levels share a column and the order needs `.label_order`.
  # Done last, once every fill above has written its plain strings.
  for (k in keys) {
    lv <- key_levels[[k]] %||% fct_levels[[k]]
    if (is.null(lv)) next
    front <- if (!is.null(ovs$label) && length(hierarchy) &&
                 identical(k, hierarchy[1L])) ovs$label
    d[[k]] <- .ard_as_factor(d[[k]], unique(c(front, lv)))
  }

  d$.kind <- .ard_kind(d)

  # The order each factor variable declared, as a POSITION per row.  It rides
  # in a column so that every manipulation the caller may make in between
  # carries it; the levels themselves are rebuilt from it by
  # .ard_levels_from_order().
  d$.label_order <- NA_integer_
  if (length(fct_levels)) {
    lv <- as.character(d$variable)
    for (k in names(fct_levels)) {
      hit <- !is.na(lv) & lv == k
      if (any(hit)) {
        d$.label_order[hit] <- match(d$.label[hit], fct_levels[[k]])
      }
    }
  }

  rownames(d) <- NULL
  front <- c(keys, "variable", "variable_level", "context", "stat_name",
             "stat_label", "stat", "stat_fmt", ".kind", ".depth", ".label",
             ".label_order", ".overall", ".key_own")
  front <- intersect(front, names(d))
  out <- d[, c(front, setdiff(names(d), front)), drop = FALSE]
  # `ard_ignored` is the one attribute left, and nothing reads it to build the
  # table: it is a report about rows that are no longer here, so there is no
  # column it could be.  Losing it only shortens a message.
  attr(out, "ard_ignored") <- ignored
  attr(out, "ard_total_n") <- total_n
  # The class is a hint, not a requirement: ard_spread() accepts any data frame
  # of the right shape, because the whole point of the two-stage split is that
  # you may rebuild the middle however you like.  It is here so that passing a
  # raw ARD by mistake says so, rather than failing on a missing column.
  class(out) <- c("ard_long", "data.frame")
  out
}


# ============================================================================
#  ard_spread()
# ============================================================================

# Resolve a (possibly named) reference vector into a list of
# list(out = <output column name>, ref = <source column name>).
.ard_refs <- function(x, d, what) {
  if (is.null(x) || !length(x)) return(list())
  nms <- names(x)
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    ref <- as.character(x[[i]])
    # an unnamed internal reference (".label", ".depth") loses its dot, so the
    # default output column is `label`, not `.label`
    nm  <- if (!is.null(nms) && nzchar(nms[i])) nms[i] else sub("^[.]", "", ref)
    if (!ref %in% names(d)) {
      # the commonest slip: naming the analysed variable, whose levels
      # ard_normalize() puts in `.label` rather than in a column of its own.
      # `levels` does take that name, so the two arguments look inconsistent
      # unless the message says why.
      if ("variable" %in% names(d) && ref %in% d$variable) {
        .ard_stop(sprintf(paste0(
          "`%s`: '%s' is an analysis variable, not a key, so ard_normalize() ",
          "puts its levels in `.label`, not in a column named '%s'. Write ",
          "`.label` here. (`levels` does take '%s' -- it keys on the analysis ",
          "variable to order that variable's rows in the label column.)"),
          what, ref, ref, ref))
      }
      avail <- paste(setdiff(names(d), c(".overall", ".key_own")), collapse = ", ")
      # `.label` is ard_normalize()'s own column.  Asking a frame that never
      # went through it to produce one is a different mistake from naming a
      # column that is simply misspelt, and the fix is different too: say
      # which column carries the row identity.  Nothing can guess that.
      if (identical(ref, ".label") && !".label" %in% names(d)) {
        .ard_stop(paste0(
          "`", what, "`: this frame has no `.label`, which is the column ",
          "ard_normalize() adds.\n",
          "  A long frame of statistics built any other way has to say which ",
          "column\n  carries the row identity:\n",
          "    ", what, " = c(row = \"<column>\")\n",
          "  Available: ", avail))
      }
      .ard_stop(sprintf("`%s`: no column '%s' in the normalized ARD. Available: %s",
                        what, ref, avail))
    }
    out[[i]] <- list(out = nm, ref = ref)
  }
  .ard_warn_positional(vapply(out, function(z) z$ref, ""), d, what)
  out
}

# `group1_level` is a POSITION, not a variable.  cards fills the group columns
# in the order each summary was asked for, so once several summaries are
# stacked the same analysis variable can sit at `group1` in one block and
# `group2` in another.  Reading the position then splits one treatment arm
# across two table columns and invents columns for whatever else landed there.
#
# But reading a position is not wrong by itself, and a subgroup table shows
# why: its rows are "which subgroup variable" by "which level of it", so
# `rows = c(grp1 = "group2", grp2 = "group2_level")` is the *point* -- `group2`
# holding ten variables is the table, not a mistake.  What is dangerous is
# taking the LEVEL without the NAME, because then levels of different variables
# land in one key with nothing to tell them apart.
#
# So warn only when the level column is read and its name column is not, and
# only when that position really does hold more than one variable.
.ard_warn_positional <- function(refs, d, what) {
  fired <- FALSE
  for (ref in unique(refs)) {
    if (!grepl("^group[0-9]+_level$", ref)) next
    g <- sub("_level$", "", ref)
    if (!g %in% names(d) || g %in% refs) next
    vars <- .ard_first_seen(d[[g]])
    if (length(vars) < 2L) next
    warning(sprintf(
      paste0("`%s = \"%s\"` reads a POSITION: `%s` holds %d different ",
             "variables in this ARD (%s),\n",
             "so levels of different variables end up in the same key.\n",
             "  If that is deliberate -- a row group of \"which variable\" and ",
             "\"which level\" -- name the\n",
             "  variable column too, e.g. `%s = c(..., \"%s\", \"%s\")`.\n",
             "  Otherwise name the variable you mean; see ard_keys()."),
      what, ref, g, length(vars),
      paste(sQuote(utils::head(vars, 6)), collapse = ", "),
      what, g, ref), call. = FALSE)
    fired <- TRUE
  }
  invisible(fired)
}

#' Turn a normalized ARD into a wide table data.frame
#'
#' Step two of the ARD conversion.  `ard_spread()` takes the long table from
#' [ard_normalize()] (possibly after you have added rows of your own), builds
#' one character cell per template, and pivots the column keys across.
#'
#' One ARD record becomes one table row.  Layouts that put a single record on
#' two printed lines are deliberately out of scope -- do those afterwards, on
#' the returned data frame.
#'
#' @param x A data frame from [ard_normalize()].
#' @param cols Column keys, outermost first.  Multiple keys are pasted with
#'   `sep`, producing the `"Placebo____Day 1"` names that
#'   [rtftable()]'s `col_header` already reads as a spanning header.  May be
#'   named, in which case the name is ignored for the column text.
#' @param rows Row keys, in output order -- **any** column of the normalized
#'   frame, not a fixed one: the ARD's own `variable` when the row groups are
#'   the analysis variables (a demographics table), or a grouping variable's
#'   name when they are not (`"AEBODSYS"`).  A named vector renames them, and
#'   the name is only the output column's name: `rows = c(group = "variable")`
#'   and `rows = c(group1 = "AEBODSYS")` differ in *what* they group by, not in
#'   kind.
#'
#'   Left `NULL` on a flat ARD carrying more than one analysis variable, it
#'   defaults to `c(group = "variable")`, since that is the only thing left to
#'   group those rows by.  One analysis variable, or any `hierarchy`, leaves it
#'   empty.
#'
#'   A **formula** element is a template here too, which is how a constant
#'   row-group heading stops needing a `mutate()` of its own:
#'   `rows = c(param = "PARAM", grp = ~ "Worst Post-Baseline Values")`.
#'   A bare string is still a column name, so nothing already written
#'   changes meaning.
#' @param label Source of the row label, as a single (optionally named)
#'   reference.  Default `".label"`, which [ard_normalize()] sets to the
#'   deepest hierarchy value, or to `variable_level` when there is no
#'   hierarchy.  `NULL` drops the label column, which is what you want when
#'   every `cells` entry is named.
#'
#'   `NA` builds the column, uses it to tell the rows apart, and then
#'   **drops it**: a recipe whose names are a row index --- `"1"` for an
#'   estimate line and `"2"` for the confidence interval under it --- needs
#'   them to separate two rows of one record, and does not want a column of
#'   1s and 2s in the result.  `NULL` leaves the label out of the row
#'   identity altogether, so those two rows collide.
#'
#'   An element that is a **formula** is a template over the record rather
#'   than a column name, and the chain works as `cells` does: the first
#'   element whose guard holds wins, and `~ "..."` is the unguarded one.
#'   Inside a template `{column}` interpolates, and `{.label}` is the label
#'   this row would otherwise carry.  So indenting the severities under
#'   `Any` is a rule rather than a `paste0()` on `.label`:
#'   `label = c(.label %in% c("Mild", "Severe") ~ "  {.label}", ~ "{.label}")`.
#' @param cells The cell recipes.  A **character vector** is one recipe, used
#'   for every variable:
#'   * `"{n} ({p})"` -- one row, labelled from `label`;
#'   * `c("{n} ({p})", "{n}")` -- one row, the first template that resolves;
#'   * `c(n == 0 ~ "0", "{n} ({p})")` -- the same chain, its first element
#'     **guarded**: a `condition ~ template` element applies only when the
#'     condition holds, so a guard that is false and a template that has no
#'     value for one of its tokens fail the same way, and the chain moves on.
#'     The condition is ordinary R, evaluated with the record's statistics
#'     by name (`n`, `p`, `mean`, ...) plus its own columns (`variable`,
#'     `.label`, `.depth`, `.kind` and the keys), and it may name the
#'     caller's variables too;
#'   * `c("Mean (SD)" = "{mean} ({sd})", "Min, Max" = "{min}, {max}")` -- one
#'     row per element, the name being the row label.
#'
#'   A **list** is instead a map, looked up by analysis variable, then by
#'   `context`, then by the structural kind, then by `"default"`; each of its
#'   elements is a recipe of the three shapes above.  The container decides:
#'   `c("Mean (SD)" = ..., "Min, Max" = ...)` is a two-row recipe, while
#'   `list(continuous = ..., categorical = ...)` is a map.  A list with no
#'   names is a chain, which is what `c()` returns once a guard is in it.
#'   For a named row whose value is itself a chain, use [ard_cells()].
#'
#'   Statistics no template names are simply not read, which is how an ARD
#'   that also carries `method`, `alternative`, `conf.level` or a p-value
#'   converts without any filtering.
#'
#'   See [rtfreporter-ard] for the `{token:spec}` grammar.
#' @param stats `"cells"` (default) builds character cells from `cells`.
#'   `"rows"` ignores `cells` and gives every statistic its own row, labelled
#'   with `stat_label` -- the shape a PK concentration table wants.
#' @param value Which of the ARD's two values `stats = "rows"` puts in the
#'   cell: the raw numeric `"stat"` (the default, so the table can still be
#'   aligned with [set_decimal_split()]) or `"stat_fmt"`, the string
#'   \pkg{cards} already formatted.  Ignored when `stats = "cells"`, where the
#'   template says which it wants, token by token.
#' @param levels Named list of level orders, e.g.
#'   `list(TRT01P = c("Placebo", "Drug"), AGEGR = c("<65", ">=65"))`.  A name
#'   may be
#'   * a **column key** -- it then fixes the order of the spread columns, which
#'     is what keeps a hand-written `col_header` over the arm it names;
#'   * a **row key**, by either its source column or its renamed output column
#'     -- it becomes a factor and drives the row sort;
#'   * an **analysis variable** -- it orders that variable's rows in the label
#'     column, without your having to know what the label column is called.
#'     Variables you leave out keep their `cells` templates' order.
#' @param labels Named character vector recoding key *values* to display text,
#'   e.g. `c(AGE = "Age (years)", SEX = "Sex [n (\%)]")`.  When a column is
#'   recoded and has no explicit `levels`, the order of `labels` becomes its
#'   level order.  The two-parallel-vector spelling works just as well --
#'   `stats::setNames(group_labels, group_vars)` -- but note that `setNames()`
#'   gives an element an `NA` name rather than complaining when the two vectors
#'   are different lengths, so that entry would never apply; every element must
#'   be named, and an unnamed one is an error here rather than a silent
#'   omission.
#'
#'   One vector is one dictionary for the **whole table**, and the same
#'   value can mean two things on the two axes --- a shift table's `"0"`
#'   is `"Grade 0"` down the side and `"Baseline 0"` across the top.
#'   Scope it the way `levels` already is, with a **list keyed by
#'   column**, matched on the output name then the source name, with
#'   `.default` covering the rest:
#'
#'   ```r
#'   labels = list(BASEGR = c("0" = "Baseline 0"),
#'                 WORST  = c("0" = "Grade 0"))
#'   ```
#' @param sort `FALSE` (default) moves a row only where somebody
#'   **declared** an order.  A key is a factor exactly when `levels`,
#'   `labels` or the data itself made it one --- and making a column a
#'   factor is how a table says what its order is --- so a factor key is
#'   sorted on, and so is the label column when `levels` gave it an
#'   order.  Everything else stays where the data put it, and a declared
#'   order nested inside a plain key's block sorts within that block.
#'   Nothing is ever alphabetised behind your back.  (The cells are
#'   gathered by their key before this, so a block is whole either way;
#'   what is left is where the blocks sit.)
#'
#'   **Usually you write nothing here, or you name the keys.**  A
#'   **character vector** names them, in priority order, each optionally
#'   prefixed `-` for descending.  `TRUE` is the third, rarer answer: it
#'   **groups**, bringing a plain key's separate blocks together in the
#'   order they first appear, which no list of keys says without also
#'   choosing an order for them:
#'   \describe{
#'     \item{`".overall"`}{the hierarchical-overall rows (an `Any TEAE` block)
#'       first.}
#'     \item{`".depth"`}{a level's own summary row before the rows nested
#'       under it.}
#'     \item{a column}{any row key or the label column, by its output name.}
#'     \item{a statistic}{that statistic totalled across the spread columns --
#'       what a descending-frequency AE table sorts on.}
#'   }
#'   So `sort = c(".overall", "soc", ".depth", "-n", "term")` is the whole of
#'   an AE table's row order, and needs neither `sort_stat` nor an `arrange()`
#'   afterwards.
#' @param sep Separator pasted between multiple `cols` keys.
#' @param rounding Tie-breaking rule for `{x:.1f}`-style tokens: `"r"` or
#'   `"sas"`.  `NULL`, the default, reads `getOption("rtfreporter.rounding")`
#'   --- the package's one rule, base R's half-to-even unless the study set
#'   `"sas"` once.  It does **not** reach `{x}` or `{x:stat_fmt}`, which take
#'   a string \pkg{cards} already rounded (half away from zero, as it
#'   happens).  See [round_num()].
#' @param sort_stat Name of a statistic to total across the spread columns and
#'   attach as a numeric `.sort_stat` column -- what a descending-frequency AE
#'   table sorts on.  `NULL` (default) adds nothing.
#' @param na Value to put in a cell no template could fill.
#' @param notes Report what was **not** used.  Every stage discards ARD rows
#'   -- the `attributes` and `total_n` rows, the key variables' own
#'   tabulations, rows with no value for a `cols` key, and every statistic no
#'   template named -- and discarding them in silence is how a mis-typed
#'   `cells` looks exactly like a correct one.  `TRUE` (default) prints a
#'   summary, `FALSE` says nothing, and `"attr"` also attaches the per-variable
#'   detail as the `"ard_ignored"` attribute.  It is not attached by default
#'   because the result is a plain data frame that you will compare against
#'   whatever you built before, and an extra attribute makes `all.equal()`
#'   report a difference that is not in the table.
#'
#'   `"applied"` adds the other half: **which template produced each cell**,
#'   with the guard that let it through when it had one.  A bare template is
#'   its own explanation, but a guarded chain is not --- the finished cell
#'   cannot tell you which of its three candidates you got --- so this is the
#'   companion to `cells` guards rather than a general-purpose log.
#'
#' @return A data frame: the `rows` columns, the label column, then one column
#'   per column key.
#'
#' @section Lifecycle:
#' **Experimental.**  See [rtfreporter-ard].
#'
#' @seealso [ard_normalize()], [ard_template()]
#' @export
ard_spread <- function(x, cols, rows = NULL, label = ".label",
                       cells = "{n} ({p})", stats = c("cells", "rows"),
                       value = c("stat", "stat_fmt"),
                       levels = NULL, labels = NULL, sort = FALSE,
                       sep = "____",
                       rounding = NULL,
                       sort_stat = NULL, na = NA_character_, notes = TRUE) {
  if (missing(cols)) {
    .ard_stop("`cols` is required: name the column keys.")
  }
  stats <- match.arg(stats)
  value <- match.arg(value)
  rounding <- .rounding_type(rounding)
  # `stats = "rows"` lays the statistic out as a row and puts a value straight
  # in the cell, so which of the ARD's two values that is has to be sayable:
  # the raw numeric `stat` (the default -- a PK table is formatted later, by
  # set_decimal_split()) or cards' already-formatted `stat_fmt`.
  rows_numeric <- identical(stats, "rows") && identical(value, "stat")
  d <- as.data.frame(x, stringsAsFactors = FALSE)

  if (!".kind" %in% names(d) && all(c("variable", "variable_level") %in% names(d))) {
    d$.kind <- .ard_kind(d)
  }

  .ard_check_named(labels, "labels")
  if (is.list(labels)) {
    for (k in names(.ard_labels_split(labels)$scopes)) {
      .ard_check_named(labels[[k]], paste0("labels$", k))
    }
  }
  .ard_check_named(levels, "levels")

  if (!inherits(x, "ard_long") &&
      !any(c(".label", ".kind", "stat_name") %in% names(d))) {
    .ard_stop(paste0(
      "`x` does not look like an ard_normalize() result: it has none of\n",
      "  `.label`, `.kind`, `stat_name`.  Pass the ARD through\n",
      "  ard_normalize() first."))
  }

  # A long frame that nobody built with cards -- a statistician's own
  # summary, keyed how they liked -- carries `stat_name` and `stat` and
  # nothing else this function names.  Reading a column that is not there
  # gave an internal R error ("attempt to select less than one element")
  # rather than a message, because `d$variable` is NULL and `NULL[1L]` is
  # not a value the cell lookup can key on.  Supplying the optional ones as
  # NA makes that lookup fall through to the kind or default entry, which
  # is exactly what such a frame wants, and costs a cards ARD nothing.
  for (cn in c("variable", "context", "stat_fmt", "stat_label")) {
    if (!cn %in% names(d)) d[[cn]] <- NA_character_
  }

  # A key variable's own tabulation is kept by ard_normalize() for the
  # column header to read, and is no cell of the body.  Flip `.key_own` to
  # FALSE to have it spread after all.
  own_ignored <- NULL
  if (".key_own" %in% names(d)) {
    own <- d$.key_own %in% TRUE
    own_ignored <- .ard_tally(d[own, , drop = FALSE],
                              "a key variable's own tabulation")
    d <- d[!own, , drop = FALSE]
  }

  colrefs <- .ard_refs(cols, d, "cols")

  # With no `rows` and no hierarchy, the only thing left to group the rows by is
  # the analysis variable, and a table of several variables always wants that
  # column.  Fill it in rather than making every flat summary spell it out.
  # One variable needs no grouping column at all, so the default stays empty --
  # and neither does a hierarchy, where the hierarchy columns do the grouping.
  has_hier <- any(!is.na(d$.depth))
  if (is.null(rows) && !has_hier &&
      length(.ard_first_seen(d$variable)) > 1L) {
    rows <- c(group = "variable")
  }
  # A `rows` element that is a formula is a template over the record, not a
  # column name -- which is how a constant row-group heading ("Worst
  # Post-Baseline Values") stops needing a mutate() of its own.  A bare string
  # stays a column name, so nothing already written changes meaning.
  if (is.list(rows) || inherits(rows, "formula")) {
    parts <- as.list(rows)
    nms <- names(parts)
    if (is.null(nms)) nms <- rep("", length(parts))
    for (i in seq_along(parts)) {
      if (!inherits(parts[[i]], "formula")) next
      ch <- .ard_chain(parts[i], one_sided = TRUE)
      cn <- if (nzchar(nms[i])) nms[i] else paste0(".rowtpl", i)
      d[[cn]] <- vapply(seq_len(nrow(d)), function(j)
        .ard_label_text(ch, d[j, , drop = FALSE], d$.label[j]), "")
      parts[[i]] <- cn
      nms[i] <- cn
    }
    rows <- stats::setNames(as.character(unlist(parts)), nms)
  }
  rowrefs <- .ard_refs(rows, d, "rows")
  # `label = NA` means "these names separate the rows but are not printed".
  # A recipe whose names are a row INDEX -- "1" and "2" for the estimate line
  # and the confidence-interval line under it -- needs them to tell the two
  # rows apart, and does not want a column of 1s and 2s in the result.  The
  # column is built either way, because the row identity is made of it, and
  # dropped at the end.
  drop_label <- length(label) == 1L && !inherits(label, "formula") &&
    is.na(label[[1L]])
  if (drop_label) label <- ".label"
  # A `label` carrying a formula is a template over the record, not a column
  # name.  Strip the templated part out, keep any plain string as the column
  # the label still comes from, and apply the template to whatever label that
  # column (or the `cells` names) produced.
  lab_chain <- NULL
  if (is.list(label) || inherits(label, "formula")) {
    parts <- as.list(label)
    is_f <- vapply(parts, inherits, logical(1), "formula")
    if (any(is_f)) {
      lab_chain <- .ard_chain(parts, one_sided = TRUE)
      plain <- parts[!is_f]
      label <- if (length(plain)) stats::setNames(
        as.character(unlist(plain)),
        names(parts)[!is_f]) else ".label"
      if (is.null(names(label)) || !nzchar(names(label)[1L])) {
        names(label) <- "label"
      }
    }
  }
  labref  <- if (is.null(label)) list() else .ard_refs(label, d, "label")
  if (!length(colrefs)) .ard_stop("`cols` is required: name the key that goes across.")

  # ---- recode key values and build the ordering factors -------------------
  # An explicit `levels` entry first; otherwise the order the key's own
  # factor declared, which ard_normalize() carried over from the data.
  lev_for <- function(r) {
    lv <- if (is.null(levels)) NULL else levels[[r$out]] %||% levels[[r$ref]]
    if (is.null(lv) && is.factor(d[[r$ref]])) lv <- base::levels(d[[r$ref]])
    lv
  }
  recode <- function(v, ref = NULL, out = NULL) {
    lab <- .ard_labels_for(labels, ref, out)
    if (is.null(lab)) return(as.character(v))
    v <- as.character(v)
    hit <- !is.na(v) & v %in% names(lab)
    v[hit] <- unname(lab[v[hit]])
    v
  }

  # ---- the column key -----------------------------------------------------
  colparts <- lapply(colrefs, function(r) recode(d[[r$ref]], r$ref, r$out))
  ok <- Reduce(`&`, lapply(colparts, function(v) !is.na(v)))
  ignored <- .ard_ignored_bind(
    .ard_ignored_bind(attr(x, "ard_ignored", exact = TRUE), own_ignored),
    .ard_tally(d[!ok, , drop = FALSE], "no value for a `cols` key"))
  d <- d[ok, , drop = FALSE]
  colparts <- lapply(colparts, function(v) v[ok])
  colkey <- do.call(paste, c(colparts, list(sep = sep)))

  # column order: lexicographic on the ordered col keys
  ord_parts <- lapply(seq_along(colrefs), function(i) {
    lv <- lev_for(colrefs[[i]])
    lv <- if (is.null(lv)) .ard_first_seen(colparts[[i]]) else
      recode(lv, colrefs[[i]]$ref, colrefs[[i]]$out)
    .ard_as_factor(colparts[[i]], lv, ordered = TRUE)
  })
  colord <- unique(data.frame(key = colkey,
                              stringsAsFactors = FALSE))
  ord_idx <- do.call(order, ord_parts)
  col_levels <- unique(colkey[ord_idx])

  # ---- group the long rows into cells -------------------------------------
  grp_cols <- c(vapply(rowrefs, function(r) r$ref, ""), "variable", "context",
                ".kind")
  grp_cols <- unique(grp_cols[grp_cols %in% names(d)])
  gid <- do.call(paste, c(lapply(grp_cols, function(k) as.character(d[[k]])),
                          list(colkey), list(sep = "\r")))

  lab_src <- if (length(labref)) as.character(d[[labref[[1]]$ref]]) else
    rep(NA_character_, nrow(d))

  pieces <- list()
  named  <- list()
  idx <- split(seq_len(nrow(d)), factor(gid, levels = unique(gid)))
  for (ii in idx) {
    if (!length(ii)) next
    sub <- d[ii, , drop = FALSE]
    key_vals <- lapply(rowrefs, function(r) as.character(sub[[r$ref]][1L]))
    ckey <- colkey[ii][1L]

    if (identical(stats, "rows")) {
      labs <- if (length(labref) && !identical(labref[[1]]$ref, ".label"))
        lab_src[ii] else as.character(sub$stat_label)
      pieces[[length(pieces) + 1L]] <- data.frame(
        .lab  = labs,
        .col  = ckey,
        .valn = if (rows_numeric) suppressWarnings(as.numeric(sub$stat))
                else NA_real_,
        .valc = if (rows_numeric) NA_character_ else as.character(sub$stat_fmt),
        .tpl = NA_character_, .guard = NA_character_,
        .depth = if (".depth" %in% names(sub)) sub$.depth else 1L,
        .overall = if (".overall" %in% names(sub)) sub$.overall else FALSE,
        .var = sub$variable[1L],
        .keys = I(rep(list(key_vals), nrow(sub))),
        stringsAsFactors = FALSE)
      next
    }

    entry <- .ard_lookup_cells(
      cells, sub$variable[1L], sub$context[1L],
      if (".kind" %in% names(sub)) sub$.kind[1L] else NA_character_)
    if (is.null(entry)) next
    named[[paste(sub$variable[1L], sub$context[1L], sep = "\r")]] <-
      unique(c(named[[paste(sub$variable[1L], sub$context[1L], sep = "\r")]],
               unlist(lapply(entry$chains, function(ch)
                 unlist(lapply(ch, function(el)
                   vapply(.ard_tokens(el$tpl),
                          function(z) .ard_token_parts(z)$name, "")))))))

    if (!is.null(entry$labels)) {
      picks <- lapply(entry$chains, .ard_chain_pick, s = sub,
                      round_type = rounding)
      vals <- vapply(picks, function(z) z$v, "")
      labs_out <- entry$labels
      if (!is.null(lab_chain)) {
        labs_out <- vapply(labs_out, function(z)
          .ard_label_text(lab_chain, sub, z), "")
      }
      pieces[[length(pieces) + 1L]] <- data.frame(
        .lab = labs_out, .col = ckey, .valn = NA_real_, .valc = vals,
        .tpl = vapply(picks, function(z) z$tpl, ""),
        .guard = vapply(picks, function(z) z$guard, ""),
        .depth = if (".depth" %in% names(sub)) sub$.depth[1L] else 1L,
        .overall = if (".overall" %in% names(sub)) sub$.overall[1L] else FALSE,
        .var = sub$variable[1L],
        .keys = I(rep(list(key_vals), length(vals))),
        stringsAsFactors = FALSE)
    } else {
      labs <- lab_src[ii]
      for (lv in unique(labs)) {
        sel <- if (is.na(lv)) is.na(labs) else (!is.na(labs) & labs == lv)
        s2 <- sub[sel, , drop = FALSE]
        pk <- .ard_chain_pick(entry$chains[[1L]], s2, rounding)
        v <- pk$v
        if (!is.null(lab_chain)) lv <- .ard_label_text(lab_chain, s2, lv)
        pieces[[length(pieces) + 1L]] <- data.frame(
          .lab = lv, .col = ckey, .valn = NA_real_, .valc = v,
          .tpl = pk$tpl, .guard = pk$guard,
          .depth = if (".depth" %in% names(s2)) s2$.depth[1L] else 1L,
          .overall = if (".overall" %in% names(s2)) s2$.overall[1L] else FALSE,
          .var = s2$variable[1L],
          .keys = I(list(key_vals)), stringsAsFactors = FALSE)
      }
    }
  }
  if (identical(stats, "cells")) {
    vk   <- paste(d$variable, d$context, sep = "\r")
    keep <- rep(TRUE, nrow(d))
    for (k in unique(vk)) {
      if (is.null(named[[k]])) next
      keep[vk == k] <- d$stat_name[vk == k] %in% named[[k]]
    }
    ignored <- .ard_ignored_bind(
      ignored, .ard_tally(d[!keep, , drop = FALSE], "no template named it"))
  }

  if (!length(pieces)) .ard_stop(.ard_no_cell_message(d, cells, stats))
  long <- do.call(rbind, pieces)

  # ---- explode the stashed key values into columns ------------------------
  for (i in seq_along(rowrefs)) {
    long[[rowrefs[[i]]$out]] <- vapply(long$.keys, function(k) {
      v <- k[[i]]; if (is.null(v) || !length(v)) NA_character_ else as.character(v)
    }, "")
  }
  long$.keys <- NULL
  keep_val <- if (rows_numeric) !is.na(long$.valn) else !is.na(long$.valc)
  long <- long[keep_val | TRUE, , drop = FALSE]

  # ---- assemble the wide frame -------------------------------------------
  rowname_cols <- vapply(rowrefs, function(r) r$out, "")
  label_out <- if (length(labref)) labref[[1]]$out else NULL
  id_cols <- c(rowname_cols, if (!is.null(label_out)) label_out)
  if (!is.null(label_out)) long[[label_out]] <- long$.lab

  # recode + factorise the row keys
  for (r in rowrefs) {
    v <- recode(long[[r$out]], r$ref, r$out)
    lab <- .ard_labels_for(labels, r$ref, r$out)
    lv <- lev_for(r)
    if (!is.null(lv)) {
      long[[r$out]] <- .ard_as_factor(v, recode(lv, r$ref, r$out))
    } else if (!is.null(lab) && any(as.character(long[[r$out]]) != v)) {
      long[[r$out]] <- .ard_as_factor(v, unname(lab[names(lab) %in%
        .ard_first_seen(long[[r$out]])]))
    } else {
      long[[r$out]] <- v
    }
  }
  if (!is.null(label_out)) {
    lv <- if (is.null(levels)) NULL else
      (levels[[label_out]] %||% levels[[labref[[1]]$ref]])
    # `levels` may instead name the ANALYSIS VARIABLES -- the natural way to
    # write "AGEGR1 runs <65, 65-74, >=75" without knowing what the label
    # column ends up being called.  Assemble one order out of those, falling
    # back to whatever order each factor variable declared for itself.
    fl <- .ard_levels_from_order(d)
    if (is.null(lv) && (!is.null(fl) ||
        (!is.null(levels) && any(names(levels) %in% .ard_first_seen(d$variable))))) {
      lv <- .ard_label_order(d, cells, c(levels, fl[setdiff(names(fl),
                                                            names(levels))]),
                             .ard_labels_flat(labels))
    }
    if (!is.null(lv)) long[[label_out]] <- .ard_as_factor(long[[label_out]], lv)
  }

  rid <- do.call(paste, c(lapply(id_cols, function(k) as.character(long[[k]])),
                          list(sep = "\r")))
  row_first <- !duplicated(rid)
  base <- long[row_first, c(id_cols, ".depth", ".overall"), drop = FALSE]
  base$.rid <- rid[row_first]

  mat <- matrix(na, nrow = nrow(base), ncol = length(col_levels),
                dimnames = list(NULL, col_levels))
  if (rows_numeric) {
    matn <- matrix(NA_real_, nrow = nrow(base), ncol = length(col_levels),
                   dimnames = list(NULL, col_levels))
  }
  ri <- match(rid, base$.rid)
  ci <- match(long$.col, col_levels)
  # `src` remembers which long row wrote each cell, so a second, different
  # value arriving at the same cell can be reported against the first.  Two
  # summaries whose row identity is not unique -- four continuous variables all
  # producing a "Mean (SD)" line, with nothing but the label to tell them
  # apart -- would otherwise overwrite each other in silence, and the finished
  # table would carry the last variable's numbers under every label.
  src   <- matrix(NA_integer_, nrow = nrow(base), ncol = length(col_levels))
  clash <- NULL
  for (k in seq_len(nrow(long))) {
    if (is.na(ri[k]) || is.na(ci[k])) next
    new <- if (rows_numeric) long$.valn[k] else long$.valc[k]
    if (is.na(new)) next
    prev <- src[ri[k], ci[k]]
    if (!is.na(prev)) {
      old <- if (rows_numeric) matn[ri[k], ci[k]]
             else mat[ri[k], ci[k]]
      # unname() both sides: `mat` carries the column dimnames, so a cell read
      # back out of it has a `names` attribute that identical() would count as
      # a difference even when the two values are the same string.
      #
      # Keep the pair as it was at the moment of the clash -- by the end of the
      # loop the cell holds whatever was written last, which would make the
      # message describe the wrong two values.
      if (!identical(unname(old), unname(new)) && is.null(clash)) {
        clash <- list(k = k, prev = prev, old = old, new = new)
      }
    }
    src[ri[k], ci[k]] <- k
    if (rows_numeric) matn[ri[k], ci[k]] <- new
    else mat[ri[k], ci[k]] <- new
  }
  if (!is.null(clash)) {
    .ard_stop(.ard_clash_message(clash, long, base, ri, ci, id_cols))
  }

  out <- base[, id_cols, drop = FALSE]
  vals <- if (rows_numeric)
    as.data.frame(matn, stringsAsFactors = FALSE, check.names = FALSE)
  else as.data.frame(mat, stringsAsFactors = FALSE, check.names = FALSE)
  out <- cbind(out, vals, stringsAsFactors = FALSE)

  if (!is.null(sort_stat)) {
    tot <- tapply(suppressWarnings(as.numeric(
      d$stat[d$stat_name == sort_stat])), rid_for_stat(d, rowrefs, labref,
      sort_stat), sum, na.rm = TRUE)
    out$.sort_stat <- as.numeric(tot[match(base$.rid, names(tot))])
  }

  if (is.character(sort) && length(sort)) {
    out <- out[.ard_sort_order(sort, out, base, d, rowrefs, labref), ,
               drop = FALSE]
  } else if (length(rowname_cols) && nrow(out) > 1L) {
    # Only an order somebody DECLARED moves a row.  A key is a factor
    # exactly when `levels`, `labels` or the data itself gave it one --
    # and making a column a factor is how a table says what its order
    # is -- so a factor key is sorted on even with `sort = FALSE`.
    # Nothing is alphabetised behind the caller's back.
    #
    # A plain key is different under the two.  `FALSE` (the default)
    # leaves its blocks exactly where they are and lets a declared order
    # nested inside one sort WITHIN it; `TRUE` also GROUPS, clustering a
    # plain key's equal values wherever they are, first-seen first.
    keys <- lapply(rowname_cols, function(k) {
      v <- out[[k]]
      if (is.factor(v)) return(v)
      ch <- as.character(v)
      ch[is.na(ch)] <- ""
      if (isTRUE(sort)) factor(ch, levels = unique(ch))
      else cumsum(c(TRUE, ch[-1L] != ch[-length(ch)]))
    })
    if (!is.null(label_out) && is.factor(out[[label_out]])) {
      keys <- c(keys, list(out[[label_out]]))
    }
    # the row number breaks ties, so equal keys keep the order they
    # arrived in rather than whatever order() happens to produce
    out <- out[do.call(order, c(keys, list(seq_len(nrow(out))))), ,
               drop = FALSE]
  }
  if (drop_label && !is.null(label_out)) out[[label_out]] <- NULL
  rownames(out) <- NULL
  # The tally is NOT attached by default.  The result is a plain data frame
  # that the caller will compare against whatever they built before -- that
  # comparison is how anyone decides to adopt this -- and an extra attribute
  # makes all.equal() report a difference that is not in the table.
  if (identical(notes, "attr")) attr(out, "ard_ignored") <- ignored
  if (!isFALSE(notes)) .ard_notes_message(ignored, "ard_spread()")
  if (identical(notes, "applied")) .ard_applied_message(long, "ard_spread()")
  out
}

# Resolve an explicit `sort` spec into a row order.  Each element names one
# key, optionally prefixed with "-" for descending:
#
#   ".overall"  the hierarchical-overall rows (an "Any TEAE" block) first
#   ".depth"    a level's own summary row before the rows nested under it
#   <column>    any row key or the label column, by its output name
#   <statistic> a statistic's total across the spread columns -- what a
#               descending-frequency AE table sorts on
#
# A statistic is recognised last, so a column of that name wins; that is the
# safe way round, since the column is what the caller can see in the result.
.ard_sort_order <- function(spec, out, base, d, rowrefs, labref) {
  keys <- list()
  for (s in spec) {
    desc <- substr(s, 1L, 1L) == "-"
    nm   <- if (desc) substring(s, 2L) else s
    v <- if (nm %in% names(out)) out[[nm]]
         else if (nm %in% c(".overall", ".depth")) base[[nm]]
         else if (nm %in% d$stat_name) {
           tot <- tapply(suppressWarnings(as.numeric(d$stat[d$stat_name == nm])),
                         rid_for_stat(d, rowrefs, labref, nm), sum, na.rm = TRUE)
           as.numeric(tot[match(base$.rid, names(tot))])
         } else {
           .ard_stop(sprintf(
             paste0("`sort`: '%s' is neither a column of the result nor a ",
                    "statistic of this ARD.\n  Columns: %s\n  Also allowed: ",
                    "'.overall', '.depth', and a leading '-' for descending."),
             nm, paste(sQuote(names(out)), collapse = ", ")))
         }
    if (is.logical(v)) v <- !v            # TRUE first reads better than TRUE last
    keys[[length(keys) + 1L]] <- if (desc) {
      if (is.numeric(v)) -v else -xtfrm(v)
    } else v
  }
  do.call(order, keys)
}

# helper for sort_stat: rebuild the row identity on the long normalized frame
rid_for_stat <- function(d, rowrefs, labref, sort_stat) {
  sel <- d$stat_name == sort_stat
  parts <- lapply(rowrefs, function(r) as.character(d[[r$ref]][sel]))
  if (length(labref)) parts <- c(parts, list(as.character(d[[labref[[1]]$ref]][sel])))
  do.call(paste, c(parts, list(sep = "\r")))
}


# ============================================================================
#  the definition file
# ============================================================================
#
#  A definition file is a WORKBOOK with one sheet per grain, so that each
#  fact is written once, at the grain it belongs to:
#
#      study      one row per study fact      rounding (key / value)
#      tables     one row per report          cols, rows, label, sort ...
#      variables  one row per variable        display label, order, levels
#      cells      one row per line of a cell  template, guard, digits
#      layout     one row per report          pages, groups, blanks, stub
#      columns    one row per printed column  width, row title, decimals
#      style      one row per report          border, heights, font
#      col_header one row per header cell     line, columns, span, text
#
#  `study` holds what is ONE for the whole study by definition -- the
#  rounding family, so that no two tables of one study can disagree.  The
#  others start with `output_id` and follow ONE rule: a blank
#  `output_id` is a study-wide default, and a row naming the report
#  replaces the default row with the same key.  A sheet added later -- the
#  titles, the footnotes, the page header -- joins under the same rule, so
#  the names it will take are reserved now (`.ard_spec_reserved`).

.ard_spec_version <- 1L

# The display sheets carry values of more than one type, and a cell of a
# workbook is text.  Each column says what it holds, so a value is checked
# where it is written -- `pages_max_rows = twenty` names its sheet and row
# at read time, not as an error from as_rtftables() three stages later.
#   int   a whole number              list  `a | b | c`
#   num   a number                    ids   `a | b`, or `1 | 2` (positions)
#   bool  TRUE / FALSE (yes / no)     flex  TRUE / FALSE, or an `ids` list
#   text  as written; quote it ("...") to keep leading or trailing spaces
.ard_spec_types <- list(
  layout = c(
    pages_max_rows = "int", pages_split = "text", pages_by = "list",
    pages_min_group_rows = "int", pages_cont_label = "text",
    group_col = "text", group_mode = "text", group_collapse = "flex",
    group_page = "bool", group_show = "bool",
    blank_where = "text", blank_first = "bool", blank_last = "bool",
    blank_counted = "bool",
    stub_vars = "list", stub_into = "text", stub_indent = "int",
    stub_summary = "text", stub_before = "bool",
    colpages_every = "int", colpages_at = "ids", colpages_carry = "ids",
    colpages_order = "list"),
  style = c(
    border = "text", align_count_pct = "bool", auto_width = "bool",
    row_height_twips = "int", header_row_height_twips = "int",
    blank_row_height_twips = "int", font = "text",
    font_size_half_points = "int", table_align = "text",
    cell_valign = "text"),
  columns = c(
    column = "text", width = "num", row_title = "bool",
    decimal_split = "bool", hide = "bool"),
  col_header = c(
    line = "int", cols = "text", span = "text", text = "text",
    align = "text", bold = "bool", border_top = "text",
    border_bottom = "text"))

.ard_spec_unquote <- function(x) {
  q <- regmatches(x, regexec("^([\"'])(.*)\\1$", x))[[1L]]
  if (length(q)) q[3L] else x
}

# One cell to its value.  NA stays NULL: a blank cell says nothing, and
# nothing is what the verb it feeds then receives.
.ard_spec_value <- function(x, type, where) {
  if (is.null(x) || is.na(x)) return(NULL)
  bad <- function(what) {
    .ard_stop(sprintf("%s must be %s; got %s.", where, what, sQuote(x)))
  }
  bool <- function(v) {
    v <- toupper(trimws(v))
    if (v %in% c("TRUE", "YES", "Y", "1")) return(TRUE)
    if (v %in% c("FALSE", "NO", "N", "0")) return(FALSE)
    NA
  }
  ids <- function(v) {
    p <- .ard_spec_split(v)
    n <- suppressWarnings(as.integer(p))
    if (length(p) && !anyNA(n) && all(grepl("^[0-9]+$", p))) n else p
  }
  switch(type,
    int = {
      v <- suppressWarnings(as.numeric(x))
      if (is.na(v) || v != round(v)) bad("a whole number")
      as.integer(v)
    },
    num = {
      v <- suppressWarnings(as.numeric(x))
      if (is.na(v)) bad("a number")
      v
    },
    bool = {
      v <- bool(x)
      if (is.na(v)) bad("TRUE or FALSE")
      v
    },
    flex = {
      # a number here is a column position, never TRUE
      v <- if (grepl("^[0-9 |]+$", x)) NA else bool(x)
      if (!is.na(v)) v else ids(x)
    },
    list = .ard_spec_split(x),
    ids  = ids(x),
    text = gsub("\\n", "\n", gsub("\r\n", "\n",
                                   .ard_spec_unquote(x), fixed = TRUE),
                fixed = TRUE))
}

# A row of a display sheet as a named list of typed values (NULLs dropped).
.ard_spec_typed <- function(row, sheet) {
  ty <- .ard_spec_types[[sheet]]
  out <- list()
  for (cn in names(ty)) {
    v <- .ard_spec_value(row[[cn]], ty[[cn]],
                         sprintf("`%s$%s`", sheet, cn))
    if (!is.null(v)) out[[cn]] <- v
  }
  out
}


.ard_spec_schema <- function() {
  list(
    tables    = c("output_id", "cols", "rows", "label", "stats", "value",
                  "sep", "sort", "sort_stat", "na"),
    variables = c("output_id", "variable", "label", "order", "levels"),
    cells     = c("output_id", "variable", "context", "row", "when",
                  "template", "digits", "signif"),
    # the table half: what as_rtftables() / rtftable() are told, read by
    # rtf_plan(spec = ) and resolved like the plan's own verbs
    layout    = c("output_id", names(.ard_spec_types$layout)),
    columns   = c("output_id", names(.ard_spec_types$columns)),
    style     = c("output_id", names(.ard_spec_types$style)),
    col_header = c("output_id", names(.ard_spec_types$col_header)))
}

# The facts the `study` sheet may state, one value each for the whole
# study, and what each accepts.
.ard_spec_study_keys <- list(rounding = c("r", "sas"))

# What a report's own row replaces a default row by.  `tables` has one row
# per report, so its key is the report itself.
.ard_spec_keys <- list(tables    = character(),
                       variables = "variable",
                       cells     = c("variable", "context", "row"),
                       layout    = character(),
                       columns   = "column",
                       style     = character(),
                       # a header is one thing: a report's own cells replace
                       # the default header whole (see .ard_spec_scope)
                       col_header = NA_character_)

# Sheets a later version will read (the rest of the RTF deliverable).  A
# workbook that already carries one is told so, not refused: the file can be
# written ahead of the reader.
.ard_spec_reserved <- c("titles", "footnotes", "page", "header", "footer",
                        "cell_styles")

# A sheet in the shape the schema says: every column present, text trimmed,
# blank cells NA, wholly blank rows gone.  A column the sheet does not read is
# an error -- a typo in a header would otherwise be a setting that silently
# never applies -- except `note`, which is there for people.
.ard_spec_sheet <- function(d, sheet) {
  allowed <- .ard_spec_schema()[[sheet]]
  if (is.null(d)) d <- data.frame()
  d <- as.data.frame(d, stringsAsFactors = FALSE, check.names = FALSE)
  names(d) <- trimws(names(d))
  extra <- setdiff(names(d), c(allowed, "note"))
  if (length(extra)) {
    home <- vapply(extra, function(cn) {
      if (cn %in% names(.ard_spec_study_keys)) {
        return(" (one per study: a key of the `study` sheet)")
      }
      hit <- names(Filter(function(s) cn %in% s, .ard_spec_schema()))
      if (length(hit)) paste0(" (a `", hit[1L], "` column)") else ""
    }, "")
    .ard_stop(paste0(
      "The `", sheet, "` sheet has ",
      if (length(extra) == 1L) "a column" else "columns",
      " it does not read: ",
      paste0(sQuote(extra), home, collapse = ", "), ".\n",
      "  Its columns are: ", paste(allowed, collapse = ", "), ".\n",
      "  Free text goes in a `note` column."))
  }
  n <- nrow(d)
  out <- lapply(allowed, function(cn) {
    v <- if (cn %in% names(d)) d[[cn]] else rep(NA, n)
    if (identical(cn, "order")) return(suppressWarnings(as.numeric(v)))
    v <- trimws(as.character(v))
    v[!is.na(v) & !nzchar(v)] <- NA_character_
    v
  })
  out <- as.data.frame(stats::setNames(out, allowed),
                       stringsAsFactors = FALSE)
  if ("note" %in% names(d)) out$note <- as.character(d$note)
  body <- setdiff(allowed, "output_id")
  keep <- rowSums(!is.na(out[body])) > 0L
  out[keep, , drop = FALSE]
}

# One key per row, as a string, for the "same key" of the scoping rule.
.ard_spec_rowkey <- function(d, sheet) {
  cols <- .ard_spec_keys[[sheet]]
  if (!length(cols) || !nrow(d)) return(rep("", nrow(d)))
  do.call(paste, c(lapply(d[cols], function(v) ifelse(is.na(v), "", v)),
                   sep = "\r"))
}

# Two rows for one key in one scope.  In `cells` that is a chain (the rows
# are tried in sheet order), so only `tables` and `variables` can clash.
.ard_spec_dupes <- function(sp) {
  # one row per report on the sheets keyed by the report alone
  for (sh in c("tables", "layout", "style")) {
    t <- sp[[sh]]
    dup <- duplicated(t$output_id)
    if (any(dup)) {
      id <- t$output_id[dup][1L]
      .ard_stop(paste0(
        "The `", sh, "` sheet has two rows for ",
        if (is.na(id)) "the default (blank `output_id`)" else sQuote(id),
        ".\n  One row per report; merge them."))
    }
  }
  for (sh in c("variables", "columns")) {
    v <- sp[[sh]]
    key <- .ard_spec_keys[[sh]]
    k <- paste(v$output_id, v[[key]], sep = "\r")
    if (any(duplicated(k))) {
      i <- which(duplicated(k))[1L]
      .ard_stop(paste0(
        "The `", sh, "` sheet has two rows for ", sQuote(v[[key]][i]),
        if (!is.na(v$output_id[i])) paste0(" in ", sQuote(v$output_id[i]))
        else " among the defaults",
        ".\n  One row per ", key, "; merge them."))
    }
  }
  invisible(NULL)
}

# The `study` sheet: `key` / `value`, one row per fact.  A named vector or
# list is taken too, so `study = c(rounding = "sas")` reads as it looks.
.ard_spec_study <- function(d) {
  if (is.null(d)) d <- data.frame(key = character(), value = character())
  if (!is.data.frame(d)) {
    if (is.null(names(d)) || !all(nzchar(names(d)))) {
      .ard_stop("`study` must be a key / value frame or a named vector.")
    }
    d <- data.frame(key = names(d), value = as.character(unlist(d)),
                    stringsAsFactors = FALSE)
  }
  d <- as.data.frame(d, stringsAsFactors = FALSE, check.names = FALSE)
  names(d) <- trimws(names(d))
  extra <- setdiff(names(d), c("key", "value", "note"))
  if (length(extra) || !all(c("key", "value") %in% names(d))) {
    .ard_stop(paste0("The `study` sheet has the columns `key` and `value` ",
                     "(and `note`), one row per study fact."))
  }
  out <- data.frame(key = trimws(as.character(d$key)),
                    value = trimws(as.character(d$value)),
                    stringsAsFactors = FALSE)
  out$value[!is.na(out$value) & !nzchar(out$value)] <- NA_character_
  if ("note" %in% names(d)) out$note <- as.character(d$note)
  out <- out[!is.na(out$key) & nzchar(out$key), , drop = FALSE]
  bad <- setdiff(out$key, names(.ard_spec_study_keys))
  if (length(bad)) {
    .ard_stop(sprintf("The `study` sheet has %s %s; it reads: %s.",
                      if (length(bad) == 1L) "a key it does not read:" else
                        "keys it does not read:",
                      paste(sQuote(bad), collapse = ", "),
                      paste(names(.ard_spec_study_keys), collapse = ", ")))
  }
  if (any(duplicated(out$key))) {
    .ard_stop(sprintf(paste0("The `study` sheet states %s twice; a study ",
                             "has one."), sQuote(out$key[duplicated(out$key)][1L])))
  }
  for (k in out$key) {
    v <- out$value[out$key == k]
    ok <- .ard_spec_study_keys[[k]]
    if (!is.na(v) && !v %in% ok) {
      .ard_stop(sprintf("`study` %s must be %s; got %s.", sQuote(k),
                        paste(sQuote(ok), collapse = " or "), sQuote(v)))
    }
  }
  rownames(out) <- NULL
  out
}

.ard_spec_study_value <- function(sp, key) {
  st <- sp$study
  if (is.null(st) || !nrow(st)) return(NA_character_)
  v <- st$value[st$key == key]
  if (length(v)) v[1L] else NA_character_
}

# The layout before #474's rework was ONE sheet with every column on it.
# Say what moved where rather than listing columns it does not know.
.ard_spec_old_layout <- function(d) {
  .ard_stop(paste0(
    "This is the one-sheet layout, which is no longer read.  A definition ",
    "file now has\n  three sheets, one per grain:\n",
    "    tables     output_id, cols, rows, label, sort, ...\n",
    "    variables  output_id, variable, label, order, levels\n",
    "    cells      output_id, variable, context, row, when, template, ",
    "digits, signif\n",
    "  `label` / `order` / `levels` move to `variables` (once per variable), ",
    "`round` becomes\n  `rounding` on the `study` sheet (one per study), and the ",
    "rest stays on `cells`.  ",
    "ard_spec_template() writes the new layout."))
}

#' A workbook-shaped definition of how an ARD becomes a table
#'
#' @description
#' `ard_spec()` validates the definition that [ard_spread()] accepts as
#' `spec =`, and [read_ard_spec()] builds one from a workbook.  It holds
#' what would otherwise be repeated in every script --- which keys go
#' across and down, the display label and order of each variable, and the
#' template and digits of every row --- in **one sheet per grain**, so
#' that each fact is written once:
#'
#' | sheet | one row per | holds |
#' |---|---|---|
#' | `study` | study fact | the rounding family (`key` / `value`) |
#' | `tables` | report | the roles and the table-wide options |
#' | `variables` | variable | display label, order, level order |
#' | `cells` | line of a cell | template, guard, digits |
#' | `layout` | report | pages, groups, blank rows, stub |
#' | `columns` | printed column | width, row title, decimal split, hidden |
#' | `style` | report | border, row heights, font |
#' | `col_header` | header cell | line, columns, span, text, borders |
#'
#' The first four say how the ARD becomes a table data frame; the last
#' four how that becomes `rtftable` pages.  [rtf_plan()] reads them all
#' (`rtf_plan(data, spec = )`), as the first layers of a plan, so a verb
#' written after it still wins.
#'
#' @section `study`:
#' What is **one for the whole study** by definition, as `key` / `value`
#' rows.  Today that is `rounding` --- `r` (half to even) or `sas` (half
#' away from zero), see [round_num()] --- so that no two tables of one
#' study can round differently.  Blank leaves it to
#' `getOption("rtfreporter.rounding")`.
#'
#' @section One rule on the other sheets:
#' Every sheet but `study` starts with `output_id`.  **Blank means a study-wide
#' default; a row naming the report replaces the default row with the same
#' key** --- the whole `tables` row for that report, the `variables` row
#' for that variable, the `cells` rows for that variable / context / row.
#' So one workbook can hold a house style and every report's own changes to
#' it.  [read_ard_spec()] narrows it to one report with `output_id =`.
#'
#' Explicit [ard_spread()] arguments win over the spec, and the spec wins
#' over the defaults.
#'
#' Lists inside a cell are `|`-separated (`TR01AG1 | SEROSTAT`).  A column
#' called `note` is allowed on any sheet and never read; any other column a
#' sheet does not know is an error, so a mistyped header cannot become a
#' setting that silently never applies.
#'
#' @section `tables`:
#' \describe{
#'   \item{`cols`}{Column keys, outermost first: `TR01AG1 | SEROSTAT`.}
#'   \item{`rows`}{Row keys, in output order.  `name = column` renames
#'     (`group1 = AEBODSYS`); a quoted value is a constant heading
#'     (`group1 = "Worst Post-Baseline Values"`).}
#'   \item{`label`}{The row-label source, in the same notation
#'     (`label = AEDECOD`).  Blank keeps `.label`; `NA` builds it to tell
#'     rows apart and then drops it; `NULL` leaves it out.}
#'   \item{`stats`, `value`, `sep`, `sort_stat`, `na`}{As the
#'     [ard_spread()] arguments of the same name.}
#'   \item{`sort`}{`TRUE`, `FALSE`, or the keys in order:
#'     `.overall | group1 | .depth | -n | label`.}
#' }
#'
#' @section `variables`:
#' \describe{
#'   \item{`variable`}{An analysis variable, or any column key (`BASEGR`,
#'     `ATPT`, or the label column's own name).}
#'   \item{`label`}{Display text replacing the variable's name.}
#'   \item{`order`}{Number; the order the variables appear in.}
#'   \item{`levels`}{The order of its values: `Grade 0 | Grade 1 | Total`.}
#' }
#'
#' @section `cells`:
#' \describe{
#'   \item{`variable`, `context`}{What the row applies to.  Either may be
#'     blank; both blank is the default for everything.  `variable` may
#'     also be a context or kind (`continuous`, `categorical`), as a
#'     `cells` map key may.}
#'   \item{`row`}{The row label (`Mean (SD)`).  Blank means one row per
#'     level, labelled by the level.}
#'   \item{`when`}{Optional guard, ordinary R over the statistics:
#'     `n == 0`.  **Several rows with the same variable / context / row are
#'     one chain**, tried in sheet order; the first whose guard holds and
#'     whose template resolves wins.}
#'   \item{`template`}{The cell recipe: `{mean} ({sd})`.}
#'   \item{`digits`}{Decimal places for tokens that name none, per token
#'     when comma-separated: `1,2` for `{mean} ({sd})`.}
#'   \item{`signif`}{Significant digits; wins over `digits`.}
#' }
#' For a `stats = rows` table (one statistic per row, the raw value in the
#' cell) a row with **no template** is instead that statistic's display
#' format: `row` names the statistic as the label column prints it (`N`,
#' `Mean`) and `digits` / `signif` say how many, as [fmt_numeric()] takes
#' them (`fmt_numeric(by = <label column>, formats = )`).
#'
#' @section `layout`:
#' One row per report, each column one argument of the plan verb its
#' prefix names:
#' \describe{
#'   \item{`pages_*`}{[plan_paginate_rows()]: `max_rows`, `split`, `by`,
#'     `min_group_rows`, `cont_label`.}
#'   \item{`group_*`}{`col`, `mode` and `collapse` of [plan_row_group()];
#'     `group_page = TRUE` is [plan_paginate_group()], one page per value
#'     of `group_col`, and `group_show = FALSE` hides that column.}
#'   \item{`blank_*`}{[plan_blanks()]: `where`, `first`, `last`,
#'     `counted`.}
#'   \item{`stub_*`}{[plan_stub()]: `vars`, `into`, `indent`, `summary`,
#'     `before`.}
#'   \item{`colpages_*`}{[plan_paginate_cols()]: `every`, `at`, `carry`,
#'     `order`.}
#' }
#'
#' @section `columns`:
#' One row per printed column, by **name** --- the finished table's, so
#' a folded stub is the name given to `stub_into`.  `.values` stands for
#' every spread column, however many the data turned out to have.
#' \describe{
#'   \item{`width`}{Relative width.  Named columns win over `.values`;
#'     when widths are given, every printed column needs one.}
#'   \item{`row_title`}{`TRUE` for a row-heading column.}
#'   \item{`decimal_split`}{`TRUE` to line up the decimal points
#'     ([set_decimal_split()]).}
#'   \item{`hide`}{`TRUE` to use the column without printing it.}
#' }
#'
#' @section `style`:
#' One row per report: `border`, `align_count_pct`, `auto_width`,
#' `row_height_twips`, `header_row_height_twips`,
#' `blank_row_height_twips`, `font`, `font_size_half_points`,
#' `table_align`, `cell_valign`, as [rtftable()] / [as_rtftables()] take
#' them.
#'
#' Values are checked where they are written: a number, `TRUE` / `FALSE`
#' or a `|`-list, as the column needs.  Quote a text value (`" (Cont.)"`)
#' to keep its leading or trailing spaces.
#'
#' @section `col_header`:
#' **One row per header cell**; `line` 1 is the top row.  A report's own
#' cells replace the default header whole.
#' \describe{
#'   \item{`cols`}{The columns the cell sits over, `|`-separated: a
#'     column name, `.values` (every spread column), a position or range
#'     (`3`, `3:31`, `3:last`), or `KEY = value` --- the spread columns
#'     whose column key `KEY` has that value (`variable = n`).}
#'   \item{`span`}{Blank: one cell over all of `cols`.  `each`: one cell
#'     per column.  A column key (`TR01AG1`): one cell per value of that
#'     key, over its columns --- an arm's spanner, however many arms.}
#'   \item{`text`}{The label.  A line break is Alt+Enter or `\\n`.  The
#'     tokens of [plan_col_header()] work: `{col}` (the column's own
#'     value), `{col1}`, `{col2}` (its keys, outermost first), `{n}` (its
#'     denominator) and `{n:sum}` (the total over the cell's columns).
#'     `{n}` is read from the ARD whenever a text uses it.  Quote a text
#'     to keep leading spaces: `"  Category"`.}
#'   \item{`align`, `bold`, `border_top`, `border_bottom`}{As
#'     [col_cell()] / [rtf_border()] take them (`single`, `none`, ...).}
#' }
#'
#' @section Reserved for the rest of the report:
#' A later version will read the sheets `titles`, `footnotes`, `page`,
#' `header`, `footer` and `cell_styles` under the same `output_id` rule,
#' so the whole RTF deliverable can be defined in one workbook.  They are reported, not refused, when present today.  An
#' `about` sheet (`key` / `value`) may state `spec_version`; sheets whose
#' name starts with `_` are ignored.
#'
#' @param tables,variables,cells,layout,columns,style,col_header Data
#'   frames with the columns above; missing columns are added as `NA`.  `tables` may instead be a named list of the
#'   sheets, or an `ard_spec` (returned as it is).
#' @param study The `study` sheet: a `key` / `value` frame, or a named
#'   vector such as `c(rounding = "sas")`.
#'
#' @return An object of class `ard_spec`: a list of the sheets' data
#'   frames.
#'
#' @section Lifecycle:
#' **Experimental.**  See [rtfreporter-ard].
#'
#' @seealso [read_ard_spec()], [write_ard_spec()], [ard_spec_template()]
#' @export
ard_spec <- function(tables = NULL, variables = NULL, cells = NULL,
                     study = NULL, layout = NULL, columns = NULL,
                     style = NULL, col_header = NULL) {
  if (inherits(tables, "ard_spec")) return(tables)
  if (is.data.frame(tables) && "template" %in% names(tables) &&
      is.null(variables) && is.null(cells)) {
    .ard_spec_old_layout(tables)
  }
  if (is.list(tables) && !is.data.frame(tables)) {
    x <- tables
    bad <- setdiff(names(x), c("study", names(.ard_spec_schema())))
    if (length(bad) || is.null(names(x))) {
      .ard_stop(paste0("A spec list holds `study`, `tables`, `variables` ",
                       "and `cells`; it also had: ",
                       paste(sQuote(bad), collapse = ", ")))
    }
    tables <- x$tables; variables <- x$variables; cells <- x$cells
    study <- x$study; layout <- x$layout; columns <- x$columns
    style <- x$style; col_header <- x$col_header
  }
  sp <- list(study     = .ard_spec_study(study),
             tables    = .ard_spec_sheet(tables,    "tables"),
             variables = .ard_spec_sheet(variables, "variables"),
             cells     = .ard_spec_sheet(cells,     "cells"),
             layout    = .ard_spec_sheet(layout,    "layout"),
             columns   = .ard_spec_sheet(columns,   "columns"),
             style     = .ard_spec_sheet(style,     "style"),
             col_header = .ard_spec_sheet(col_header, "col_header"))
  t <- sp$tables
  chk <- function(v, ok, what) {
    bad <- !is.na(v) & !v %in% ok
    if (any(bad)) {
      .ard_stop(sprintf("`tables$%s` must be %s; got %s.", what,
                        paste(sQuote(ok), collapse = " or "),
                        sQuote(v[bad][1L])))
    }
  }
  chk(t$stats, c("cells", "rows"), "stats")
  chk(t$value, c("stat", "stat_fmt"), "value")
  if (any(is.na(sp$variables$variable))) {
    .ard_stop("Every `variables` row needs a `variable`.")
  }
  # a row with no template is a stats = rows display format, which needs
  # the format it is there to give
  nofmt <- is.na(sp$cells$template) & is.na(sp$cells$digits) &
    is.na(sp$cells$signif)
  if (any(nofmt)) {
    i <- which(nofmt)[1L]
    .ard_stop(sprintf(paste0(
      "`cells` row %d has no `template`, and no `digits` / `signif` ",
      "either.\n  A row is a template, or -- for a stats = rows table -- ",
      "the format of one statistic."), i))
  }
  if (any(is.na(sp$columns$column))) {
    .ard_stop("Every `columns` row needs a `column`.")
  }
  if (any(is.na(sp$col_header$line) | is.na(sp$col_header$cols))) {
    .ard_stop("Every `col_header` row needs a `line` and `cols`.")
  }
  for (sh in names(.ard_spec_types)) {
    for (i in seq_len(nrow(sp[[sh]]))) {
      .ard_spec_typed(sp[[sh]][i, , drop = FALSE], sh)
    }
  }
  .ard_spec_dupes(sp)
  class(sp) <- "ard_spec"
  sp
}

#' @export
print.ard_spec <- function(x, ...) {
  ids <- .ard_first_seen(stats::na.omit(unlist(lapply(x, `[[`, "output_id"))))
  cat("<ard_spec>",
      if (!is.null(attr(x, "output_id"))) paste0(" for ",
        sQuote(attr(x, "output_id")))
      else if (length(ids)) paste0(" for ", length(ids), " report",
                                   if (length(ids) > 1L) "s" else "",
                                   ": ", paste(ids, collapse = ", "))
      else "", "\n", sep = "")
  st <- x$study
  if (!is.null(st) && nrow(st)) {
    cat("  study      ", paste0(st$key, " = ",
                                ifelse(is.na(st$value), "(blank)", st$value),
                                collapse = ", "), "\n", sep = "")
  }
  for (s in names(.ard_spec_schema())) {
    cat(sprintf("  %-10s %3d row%s\n", s, nrow(x[[s]]),
                if (nrow(x[[s]]) == 1L) "" else "s"))
  }
  invisible(x)
}

# Narrow a workbook to one report, applying the one rule on every sheet.
# `NULL` is fine for a workbook that defines one report (or none, only
# defaults); for several it has to be said which.
.ard_spec_scope <- function(sp, output_id = NULL) {
  if (!is.null(attr(sp, "output_id"))) {
    if (is.null(output_id) || identical(output_id, attr(sp, "output_id"))) {
      return(sp)
    }
  }
  ids <- .ard_first_seen(stats::na.omit(unlist(
    lapply(sp[names(.ard_spec_schema())], `[[`, "output_id"))))
  if (is.null(output_id)) {
    if (length(ids) > 1L) {
      .ard_stop(paste0(
        "This spec defines ", length(ids), " reports (",
        paste(sQuote(ids), collapse = ", "), "); say which one:\n",
        "    read_ard_spec(path, output_id = ", dQuote(ids[1L], FALSE), ")"))
    }
    if (!length(ids)) return(sp)
    output_id <- ids
  }
  if (!length(ids)) return(sp)          # a file of defaults serves any report
  if (!is.character(output_id) || length(output_id) != 1L ||
      is.na(output_id)) {
    .ard_stop("`output_id` must be a single string.")
  }
  has_default <- any(vapply(sp[names(.ard_spec_schema())],
                            function(d) any(is.na(d$output_id)), NA))
  if (!output_id %in% ids) {
    if (!has_default) {
      .ard_stop(sprintf(paste0(
        "`output_id`: this spec has no row for %s and no default rows ",
        "either, so nothing would apply.\n  It defines: %s"),
        sQuote(output_id), paste(sQuote(ids), collapse = ", ")))
    }
    # Most reports in a shared file are covered by its defaults, so this is
    # normal; it is still worth saying, because a mistyped id looks exactly
    # the same from here.
    message(sprintf(paste0(
      "read_ard_spec(): no row names %s, so the default rows are used.",
      "\n  The file defines: %s"),
      sQuote(output_id), paste(sQuote(ids), collapse = ", ")))
  }
  for (s in names(.ard_spec_schema())) {
    d <- sp[[s]]
    d <- d[is.na(d$output_id) | d$output_id == output_id, , drop = FALSE]
    mine <- !is.na(d$output_id)
    if (!length(.ard_spec_keys[[s]])) {
      # one row: the report's own values over the defaults, column by column
      if (sum(mine) && sum(!mine)) {
        row <- d[!mine, , drop = FALSE]
        own <- d[mine, , drop = FALSE]
        for (cn in names(own)) if (!is.na(own[[cn]])) row[[cn]] <- own[[cn]]
        d <- row
      }
    } else if (identical(.ard_spec_keys[[s]], NA_character_)) {
      if (any(mine)) d <- d[mine, , drop = FALSE]
    } else {
      k <- .ard_spec_rowkey(d, s)
      d <- d[mine | !(k %in% k[mine]), , drop = FALSE]
    }
    d$output_id <- rep(output_id, nrow(d))
    rownames(d) <- NULL
    sp[[s]] <- d
  }
  attr(sp, "output_id") <- output_id
  sp
}

# `a | b | c` -> c("a", "b", "c")
.ard_spec_split <- function(x) {
  if (is.null(x) || is.na(x)) return(character())
  v <- trimws(strsplit(x, "|", fixed = TRUE)[[1L]])
  v[nzchar(v)]
}

# One reference, as written in a `tables` cell: `col`, `name = col`, or a
# quoted constant (`name = "Worst Post-Baseline Values"`), which is the
# formula-template form of the argument.
.ard_spec_ref <- function(x) {
  m <- regmatches(x, regexec("^([A-Za-z.][A-Za-z0-9._]*)\\s*=\\s*(.+)$", x))[[1L]]
  nm <- if (length(m)) m[2L] else ""
  v  <- trimws(if (length(m)) m[3L] else x)
  q <- regmatches(v, regexec("^([\"'])(.*)\\1$", v))[[1L]]
  val <- if (length(q)) eval(call("~", q[3L]), baseenv()) else v
  list(name = nm, value = val)
}

.ard_spec_refs <- function(x) {
  parts <- lapply(.ard_spec_split(x), .ard_spec_ref)
  if (!length(parts)) return(NULL)
  nms  <- vapply(parts, `[[`, "", "name")
  vals <- lapply(parts, `[[`, "value")
  if (any(vapply(vals, inherits, NA, "formula"))) {
    return(stats::setNames(vals, nms))
  }
  v <- unlist(vals)
  if (any(nzchar(nms))) names(v) <- nms
  v
}

# The table-wide arguments one `tables` row supplies, as ard_spread() takes
# them.  Only what the row says is returned: an argument it leaves blank
# keeps ard_spread()'s own default.
.ard_spec_table_args <- function(sp) {
  out <- list()
  r <- .ard_spec_study_value(sp, "rounding")
  if (!is.na(r)) out$rounding <- r
  t <- sp$tables
  if (!nrow(t)) return(out)
  t <- t[1L, , drop = FALSE]
  if (!is.na(t$cols)) out$cols <- .ard_spec_split(t$cols)
  if (!is.na(t$rows)) out$rows <- .ard_spec_refs(t$rows)
  if (!is.na(t$label)) {
    out["label"] <- list(switch(t$label, "NA" = NA, "NULL" = NULL,
                                .ard_spec_refs(t$label)))
  }
  for (a in c("stats", "value", "sep", "sort_stat", "na")) {
    if (!is.na(t[[a]])) out[[a]] <- t[[a]]
  }
  if (!is.na(t$sort)) {
    out$sort <- switch(toupper(t$sort), "TRUE" = TRUE, "FALSE" = FALSE,
                       .ard_spec_split(t$sort))
  }
  out
}

.ard_spec_variables <- function(sp) {
  v <- sp$variables
  if (any(!is.na(v$order))) v <- v[order(v$order, na.last = TRUE), , drop = FALSE]
  v
}

.ard_spec_labels <- function(sp) {
  v <- .ard_spec_variables(sp)
  v <- v[!is.na(v$label), , drop = FALSE]
  if (!nrow(v)) return(NULL)
  stats::setNames(v$label, v$variable)
}

.ard_spec_levels <- function(sp) {
  v <- sp$variables[!is.na(sp$variables$levels), , drop = FALSE]
  if (!nrow(v)) return(NULL)
  stats::setNames(lapply(v$levels, .ard_spec_split), v$variable)
}

# Rewrite "{mean} ({sd})" + digits "1,2" into "{mean:.1f} ({sd:.2f})", so what
# the spec asked for is visible in the template itself.  `signif` wins over
# `digits`; a token that already carries an inline spec is left alone.
.ard_apply_digits <- function(tpl, digits, sgnf) {
  toks <- .ard_tokens(tpl)
  if (!length(toks)) return(tpl)
  num <- function(v) {
    if (length(v) != 1L || is.na(v) || !nzchar(as.character(v)))
      return(integer(0))
    suppressWarnings(as.integer(trimws(strsplit(as.character(v), ",")[[1]])))
  }
  dg <- num(digits); sg <- num(sgnf)
  if (!length(dg) && !length(sg)) return(tpl)
  out <- tpl
  for (i in seq_along(toks)) {
    p <- .ard_token_parts(toks[i])
    if (nzchar(p$spec)) next
    pick <- function(v) if (!length(v)) NA_integer_ else v[min(i, length(v))]
    s <- pick(sg)
    new <- if (!is.na(s)) paste0(".", s, "s") else {
      dd <- pick(dg)
      if (is.na(dd)) NA_character_ else paste0(".", dd, "f")
    }
    if (is.na(new)) next
    out <- sub(toks[i], paste0("{", p$name, ":", new, "}"), out, fixed = TRUE)
  }
  out
}

# One `cells` row as one element of a chain: its template with the digits
# written in, guarded by `when` when there is one.  The guard is parsed
# here, so a malformed one names its row.
.ard_spec_chain_el <- function(r, i) {
  tpl <- .ard_apply_digits(r$template, r$digits, r$signif)
  if (is.na(r$when)) return(tpl)
  cond <- tryCatch(str2lang(r$when), error = function(e) {
    .ard_stop(sprintf("`cells` row %d: `when` is not valid R: %s\n  %s",
                      i, sQuote(r$when), conditionMessage(e)))
  })
  eval(call("~", cond, tpl), baseenv())
}

# The `cells` sheet as ard_spread()'s `cells` map.  Rows sharing variable /
# context / row are one chain, in sheet order; the map key is the variable,
# the context, both (a variable summarised two ways), or `default`.
.ard_spec_cells <- function(sp) {
  s <- sp$cells
  s <- s[!is.na(s$template), , drop = FALSE]    # the rest are rows formats
  if (!nrow(s)) return(NULL)
  ord <- .ard_spec_variables(sp)$variable
  key <- ifelse(!is.na(s$variable) & !is.na(s$context),
                paste(s$variable, s$context, sep = "\r"),
                ifelse(!is.na(s$variable), s$variable,
                       ifelse(!is.na(s$context), s$context, "default")))
  first <- .ard_first_seen(key)
  rank <- match(sub("\r.*$", "", first), ord)
  first <- first[order(is.na(rank), rank)]
  out <- list()
  for (k in first) {
    idx  <- which(key == k)
    rows <- s$row[idx]
    rows[is.na(rows)] <- ""
    labs <- .ard_first_seen(rows)
    chains <- lapply(labs, function(lb) {
      els <- lapply(idx[rows == lb], function(i)
        .ard_spec_chain_el(s[i, , drop = FALSE], i))
      if (all(vapply(els, is.character, NA))) unlist(els) else els
    })
    guarded <- any(vapply(chains, is.list, NA))
    out[[k]] <- if (identical(labs, "")) chains[[1L]]
                else if (guarded) do.call(ard_cells, stats::setNames(chains, labs))
                else stats::setNames(chains, labs)
  }
  out
}

# Workbook sheets -> the three frames.  Reserved sheets are reported, `_`
# sheets and empty ones ignored, `about` checked for the version; anything
# else is a sheet nobody reads, and says so.
.ard_spec_from_sheets <- function(sheets, where) {
  nm <- names(sheets)
  low <- tolower(nm)
  if (!any(low %in% c("study", names(.ard_spec_schema())))) {
    one <- sheets[[1L]]
    if (length(sheets) >= 1L && "template" %in% names(one)) {
      .ard_spec_old_layout(one)
    }
    .ard_stop(paste0(sQuote(where), " has none of the sheets `tables`, ",
                     "`variables`, `cells`."))
  }
  if ("about" %in% low) {
    a <- sheets[[which(low == "about")[1L]]]
    if (all(c("key", "value") %in% names(a))) {
      ver <- suppressWarnings(as.integer(a$value[a$key == "spec_version"]))
      if (length(ver) && !is.na(ver[1L]) && ver[1L] > .ard_spec_version) {
        .ard_stop(sprintf(paste0(
          "%s is spec_version %d; this rtfreporter reads up to %d.  ",
          "Update the package."), sQuote(where), ver[1L], .ard_spec_version))
      }
    }
  }
  empty <- vapply(sheets, function(d) !nrow(d), NA) & low != "study"
  skip <- startsWith(nm, "_") | low == "about" | empty
  res <- low %in% .ard_spec_reserved & !skip
  if (any(res)) {
    message(sprintf(paste0(
      "read_ard_spec(): %s %s reserved for a later version and not read yet."),
      paste(sQuote(nm[res]), collapse = ", "),
      if (sum(res) == 1L) "is" else "are"))
  }
  other <- !skip & !res & !low %in% c("study", names(.ard_spec_schema()))
  if (any(other)) {
    .ard_stop(paste0(
      sQuote(where), " has ",
      if (sum(other) == 1L) "a sheet" else "sheets", " nobody reads: ",
      paste(sQuote(nm[other]), collapse = ", "), ".\n",
      "  Rename it to start with `_` to keep it as notes."))
  }
  get <- function(s) {
    i <- which(low == s)
    if (length(i)) sheets[[i[1L]]] else NULL
  }
  ard_spec(get("tables"), get("variables"), get("cells"),
           study = get("study"), layout = get("layout"),
           columns = get("columns"), style = get("style"),
           col_header = get("col_header"))
}

# A definition is ONE file holding several sheets, which is what a workbook
# is; a CSV holds one table, so it cannot be one.  Say so by extension
# rather than letting readxl fail on a file it cannot open.
.ard_spec_xlsx_path <- function(path, fn) {
  if (!is.character(path) || length(path) != 1L ||
      !grepl("[.]xlsx$", path, ignore.case = TRUE)) {
    .ard_stop(paste0(
      "`", fn, "()` takes an .xlsx workbook: a definition is one file ",
      "with the sheets\n  `tables`, `variables` and `cells`, which a CSV ",
      "(one table per file) cannot hold."))
  }
  invisible(path)
}

#' Read an ARD table definition from a workbook
#'
#' @param path An `.xlsx` workbook (needs \pkg{readxl}) with the sheets
#'   of [ard_spec()] (`study`, `tables`, `variables`, `cells`, `layout`,
#'   `columns`, `style`, `col_header`).  Any of them may be absent.  A definition is one file with several sheets,
#'   so it is an Excel workbook and nothing else.
#' @param output_id The report to narrow the workbook to.  Rows with a blank
#'   `output_id` are the study's defaults and stay; a row naming this
#'   report replaces the default with the same key.  May be left `NULL`
#'   for a workbook that defines one report; for several it must be given.
#'
#' @return An [ard_spec()].
#'
#' @section Lifecycle:
#' **Experimental.**  See [rtfreporter-ard].
#'
#' @seealso [ard_spec()], [write_ard_spec()]
#' @export
read_ard_spec <- function(path, output_id = NULL) {
  .ard_spec_xlsx_path(path, "read_ard_spec")
  if (!file.exists(path)) .ard_stop(sprintf("No such file: %s", path))
  .ard_need("readxl", "read_ard_spec()")
  nms <- readxl::excel_sheets(path)
  sheets <- stats::setNames(lapply(nms, function(s) as.data.frame(
    readxl::read_excel(path, sheet = s, col_types = "text"),
    stringsAsFactors = FALSE)), nms)
  sp <- .ard_spec_from_sheets(sheets, basename(path))
  .ard_spec_scope(sp, output_id)
}

#' Write an ARD table definition to a workbook
#'
#' @param spec An [ard_spec()] (or what it accepts).
#' @param path Destination `.xlsx` (needs \pkg{writexl}).  The workbook
#'   gets every sheet of [ard_spec()], empty ones included so their columns
#'   are there to fill in, and an `about` sheet stating `spec_version`.
#'
#' @return `path`, invisibly.
#'
#' @section Lifecycle:
#' **Experimental.**  See [rtfreporter-ard].
#'
#' @seealso [ard_spec()], [read_ard_spec()]
#' @export
write_ard_spec <- function(spec, path) {
  sp <- ard_spec(spec)
  sheets <- lapply(names(.ard_spec_schema()), function(s) {
    d <- sp[[s]]
    rownames(d) <- NULL
    d
  })
  names(sheets) <- names(.ard_spec_schema())
  # the study sheet always shows its keys, blank or not, so the one place
  # the study's rounding is decided is visible in every workbook
  st <- sp$study
  for (k in setdiff(names(.ard_spec_study_keys), st$key)) {
    st[nrow(st) + 1L, c("key", "value")] <- list(k, NA_character_)
  }
  sheets <- c(list(study = st), sheets)
  .ard_spec_xlsx_path(path, "write_ard_spec")
  .ard_need("writexl", "write_ard_spec()")
  about <- data.frame(key = "spec_version",
                      value = as.character(.ard_spec_version),
                      stringsAsFactors = FALSE)
  writexl::write_xlsx(c(sheets, list(about = about)), path)
  invisible(path)
}

#' Scaffold a definition workbook from an ARD
#'
#' Walks the ARD and writes the three [ard_spec()] sheets: one `tables` row,
#' one `variables` row per analysis variable (its levels filled in for a
#' categorical one), and one `cells` row per row template --- a continuous
#' variable gets the templates its statistics can fill, a categorical one
#' `{n} ({p})`.  Edit the labels, templates and digits, and hand it back
#' through `spec =`.
#'
#' @param ard A cards/cardx ARD.
#' @param path Optional destination; when given the spec is also written there
#'   with [write_ard_spec()].
#' @param cols The column keys for the `tables` row, if known.
#' @param output_id The report the rows belong to; `NULL` writes them as
#'   defaults.
#'
#' @return An [ard_spec()], invisibly when `path` is given.
#'
#' @section Lifecycle:
#' **Experimental.**  See [rtfreporter-ard].
#'
#' @seealso [ard_spec()], [ard_template()]
#' @export
ard_spec_template <- function(ard, path = NULL, cols = NULL,
                              output_id = NULL) {
  d <- ard_normalize(ard, drop_key_variables = FALSE)
  if (".key_own" %in% names(d)) d <- d[!(d$.key_own %in% TRUE), , drop = FALSE]
  vars <- .ard_first_seen(d$variable)
  id <- if (is.null(output_id)) NA_character_ else output_id
  vrows <- list(); crows <- list()
  cell <- function(v, ctx, row, tpl) data.frame(
    output_id = id, variable = v, context = ctx, row = row,
    when = NA_character_, template = tpl, digits = NA_character_,
    signif = NA_character_, stringsAsFactors = FALSE)
  for (i in seq_along(vars)) {
    v <- vars[i]
    s <- d[d$variable == v, , drop = FALSE]
    ctx <- s$context[1L]
    lv <- NA_character_
    if (identical(ctx, "continuous") || identical(ctx, "summary")) {
      have <- .ard_first_seen(s$stat_name)
      cand <- list(c("n", "{N}"), c("Mean (SD)", "{mean} ({sd})"),
                   c("Median", "{median}"), c("Min, Max", "{min}, {max}"))
      for (cc in cand) {
        need <- gsub("[{}]", "", .ard_tokens(cc[2]))
        if (all(need %in% have)) {
          crows[[length(crows) + 1L]] <- cell(v, NA_character_, cc[1], cc[2])
        }
      }
    } else {
      l <- .ard_first_seen(stats::na.omit(s$variable_level))
      if (length(l)) lv <- paste(l, collapse = " | ")
      crows[[length(crows) + 1L]] <- cell(v, NA_character_, NA_character_,
                                          "{n} ({p})")
    }
    vrows[[i]] <- data.frame(output_id = id, variable = v, label = v,
                             order = i, levels = lv,
                             stringsAsFactors = FALSE)
  }
  tables <- data.frame(output_id = id,
                       cols = if (length(cols)) paste(cols, collapse = " | ")
                              else NA_character_,
                       stringsAsFactors = FALSE)
  sp <- ard_spec(tables,
                 if (length(vrows)) do.call(rbind, vrows) else NULL,
                 if (length(crows)) do.call(rbind, crows) else NULL)
  if (is.null(path)) return(sp)
  write_ard_spec(sp, path)
  invisible(sp)
}


# RStudio's own answer to "which pipe does this person write", read from the
# preference behind Ctrl+Shift+M: `insert_native_pipe_operator`, a boolean
# whose factory default is FALSE.  Asking the IDE beats guessing, and it is
# the same setting the author sees in Tools > Global Options > Code.
# Split in two so the mapping is testable without an RStudio to run in.
.ard_rstudio_pref <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) return(NULL)
  ok <- tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE)
  if (!isTRUE(ok)) return(NULL)
  tryCatch(
    rstudioapi::readRStudioPreference("insert_native_pipe_operator", NULL),
    error = function(e) NULL)
}

# NULL means "no answer" -- not RStudio, no rstudioapi, or the read failed --
# and the caller decides what to do about that.
.ard_pipe_rstudio <- function(pref = .ard_rstudio_pref()) {
  if (!is.logical(pref) || length(pref) != 1L || is.na(pref)) return(NULL)
  if (pref) "|>" else "%>%"
}

# The pipe operator the generated script is written with, resolved once:
# an explicit `pipe =`, then `getOption("rtfreporter.ard_pipe")`, then
# RStudio's own setting, then `%>%`.
#
# `%>%` is the floor rather than `|>` because the two are NOT
# interchangeable, and base R has not closed the gap: as of R 4.6 the
# placeholder `_` may still appear only once in a call and only as a named
# argument (or as the head of a `$`/`[`/`[[`/`@` chain), while magrittr's
# `.` is positional and may appear twice.  The generated pipeline itself
# uses no placeholder, so the choice only decides what the author may add
# at the seam -- which is why following the IDE is safe here, and why a
# study that wants one answer for everybody pins it ONCE --
#     options(rtfreporter.ard_pipe = "|>")
.ard_pipe_op <- function(x = NULL) {
  asked <- !is.null(x)
  if (is.null(x)) {
    x <- getOption("rtfreporter.ard_pipe")
    asked <- !is.null(x)
  }
  if (is.null(x)) x <- "rstudio"
  if (!is.character(x) || length(x) != 1L ||
        !x %in% c("%>%", "|>", "rstudio")) {
    .ard_stop(paste0(
      "`pipe` must be \"%>%\" (magrittr), \"|>\" (base R, needs no ",
      "package)\n  or \"rstudio\" (whichever RStudio's Insert Pipe ",
      "Operator inserts)."))
  }
  if (identical(x, "rstudio")) {
    got <- .ard_pipe_rstudio()
    if (is.null(got)) {
      # Silent when this was merely the default; a caller who NAMED
      # "rstudio" asked a question and is owed the answer.
      if (asked) {
        message("`pipe = \"rstudio\"`: no RStudio preference to read -- ",
                "not running in\n  RStudio, or rstudioapi is not ",
                "installed.  Writing \"%>%\".")
      }
      got <- "%>%"
    }
    x <- got
  }
  x
}

# What a template needs to know about an ARD, gathered once.  Two emitters
# read it -- ard_template() writes the verbs, plan_template() writes the
# plan -- and neither should be re-deriving the same facts from the same
# tibble in two places.
.ard_template_facts <- function(ard, cols = NULL, hierarchy = character()) {
  d  <- ard_normalize(ard, hierarchy = hierarchy, drop_key_variables = FALSE)
  ra <- as.data.frame(ard)
  gcols <- grep("^group[0-9]+$", names(ra), value = TRUE)
  keys  <- unique(unlist(lapply(gcols, function(g) .ard_first_seen(ra[[g]]))))
  guessed <- is.null(cols)
  if (guessed) cols <- utils::head(keys, 1L)
  rest <- setdiff(keys, c(cols, hierarchy))

  vars <- setdiff(.ard_first_seen(d$variable), c(keys, hierarchy))
  vars <- setdiff(vars, c("..ard_total_n..", "..ard_hierarchical_overall.."))
  kinds <- .ard_first_seen(d$.kind[d$variable %in% vars])
  if (!length(kinds)) kinds <- .ard_first_seen(d$.kind)
  # Ask the RAW ard: ard_normalize() drops the sentinel rows when no
  # `overall =` was given, so looking at `d` would never find them.
  overall <- any(as.character(unlist(ra$variable)) ==
                   "..ard_hierarchical_overall..", na.rm = TRUE)

  # The decimal places are IN the ARD: cards stores `fmt_fun` per statistic,
  # and a study that sets its own ("mean to 2, SD to 3") records exactly that.
  dig <- list()
  if ("fmt_fun" %in% names(ra)) {
    sn <- as.character(unlist(ra$stat_name))
    for (j in seq_len(nrow(ra))) {
      f <- ra$fmt_fun[[j]]
      if (is.null(dig[[sn[j]]]) && is.numeric(f) && length(f) == 1L) {
        dig[[sn[j]]] <- as.integer(f)
      }
    }
  }
  tok <- function(stat) {
    dd <- dig[[stat]]
    if (is.null(dd)) paste0("{", stat, "}") else paste0("{", stat, ":.", dd, "f}")
  }

  q <- function(v) paste0("\"", v, "\"")
  vecq <- function(v) {
    if (length(v) == 1L) q(v) else paste0("c(", paste(q(v), collapse = ", "), ")")
  }

  # `rows` is derived, not merely reported: a key left out of the call is a
  # column of the table that silently goes missing.
  row_parts <- character(0)
  if (length(rest)) row_parts <- paste0(rest, " = ", q(rest))
  if (length(hierarchy)) {
    row_parts <- c(row_parts, paste0("group1 = ", q(hierarchy[1L])))
  } else if (length(vars) > 1L) {
    row_parts <- c(row_parts, "group = \"variable\"")
  }
  stub <- c(sub(" =.*$", "", row_parts), "label")

  list(d = d, ra = ra, keys = keys, cols = cols, guessed = guessed,
       rest = rest, vars = vars, kinds = kinds, overall = overall,
       dig = dig, tok = tok, q = q, vecq = vecq,
       row_parts = row_parts, stub = stub)
}

# ============================================================================
#  ard_template()
# ============================================================================

# The second half of a generated script: the as_rtftables() call, plus the
# One banner line padded to a constant width, so the generated script reads
# as a script rather than as a dump.
.ard_bar <- function(text = "", char = "-", width = 70L) {
  if (!nzchar(text)) return(paste0("# ", strrep(char, width - 2L)))
  pad <- width - 5L - nchar(text)
  paste0("# ", strrep(char, 2L), " ", text, " ", strrep(char, max(pad, 3L)))
}

# `"name" = "value"` lines with the names padded to one width, so the reader
# sees the templates lined up and can delete whole rows cleanly.
.ard_aligned <- function(nms, vals, indent) {
  qn <- encodeString(nms, quote = "\"")
  paste0(indent, formatC(qn, width = max(nchar(qn)), flag = "-"),
         " = ", encodeString(vals, quote = "\""))
}

#' Write the conversion code for you
#'
#' Reads an ARD and prints the runnable conversion code, filled in with the
#' keys, hierarchy, contexts and statistics it actually found.  Because none
#' of the structure is guessed from the object's attributes, the generated
#' code is also a readable record of what the ARD contains.
#'
#' The emitted code is a starting point, not a finished table: in particular
#' it does **not** invent a row order, because the level order of a factor is
#' not recoverable from an ARD that was not built with `.attributes = TRUE`.
#'
#' @param ard A cards/cardx ARD.
#' @param cols Key(s) that go across.  `NULL` (default) uses the first key
#'   found and says so in a comment.
#' @param hierarchy Optional nested hierarchy, outermost first.
#' @section What it writes:
#' Always three blocks, so the author deletes rather than remembers:
#' the conversion written as the **pipe** --- `ard_normalize()`, a commented
#' `dplyr::mutate()` and `ard_spread()` --- with the seam left open, because
#' half the reports on Discussion #473 have to reach between the two steps
#' (to derive a key from a statistic, to add a constant column, to indent a
#' label); then the `col_header` (drafted from [ard_pull()] when one column
#' key makes that decidable, and skipped entirely when
#' several do, since `as_rtftables(header_sep = )` rebuilds the spanning
#' header from the `"____"` in the names), and the [as_rtftables()]
#' call with `stub_vars` derived from the row keys and the label column.
#'
#' @param pipe Which pipe to write the conversion with: `"%>%"` (magrittr),
#'   `"|>"` (base R, which needs no package), or `"rstudio"` --- whichever
#'   RStudio's own **Insert Pipe Operator** inserts, read from the
#'   `insert_native_pipe_operator` preference (Tools > Global Options >
#'   Code).  `NULL`, the default, reads `getOption("rtfreporter.ard_pipe")`,
#'   then asks RStudio, then falls back to `"%>%"`, so the generated script
#'   is written in the pipe you already write.  Pin it for everybody with
#'   `options(rtfreporter.ard_pipe = "|>")` --- worth doing if two people
#'   should get identical code from the same call, since the RStudio answer
#'   is per-installation.  Outside RStudio (`Rscript`, CI, Positron) there
#'   is nothing to read and the answer is `"%>%"`; naming `"rstudio"`
#'   explicitly says so in a message, while the default stays quiet.
#'
#'   The fallback is magrittr's rather than base R's because the two are not
#'   interchangeable in general: as of R 4.6 the placeholder `_` may still
#'   appear only once per call and only as a named argument (or as the head
#'   of a `$`/`[`/`[[`/`@` chain), while `%>%`'s `.` is positional and may
#'   appear twice.  The generated pipeline uses no placeholder, so the two
#'   are interchangeable *here* --- the choice decides what you may add at
#'   the seam.  A `"%>%"` script opens with `library(magrittr)`; a `"|>"`
#'   one needs no `library()` at all, because everything else is written
#'   `rtfreporter::`-qualified.
#' @param file Optional path to write the code to.
#'
#' @return The generated code, as a character vector, invisibly.
#'
#' @section Lifecycle:
#' **Experimental.**  See [rtfreporter-ard].
#'
#' @seealso [ard_normalize()], [ard_spread()], [ard_spec_template()]
#' @export
ard_template <- function(ard, cols = NULL, hierarchy = character(),
                         file = NULL, pipe = NULL) {
  op <- .ard_pipe_op(pipe)
  f <- .ard_template_facts(ard, cols, hierarchy)
  d <- f$d; ra <- f$ra; keys <- f$keys; cols <- f$cols
  guessed <- f$guessed; rest <- f$rest; vars <- f$vars
  kinds <- f$kinds; overall <- f$overall; dig <- f$dig
  tok <- f$tok; q <- f$q; vecq <- f$vecq
  row_parts <- f$row_parts; stub <- f$stub

  # -- banner -----------------------------------------------------------
  L <- c(
    .ard_bar("", "="),
    "#  generated by rtfreporter::ard_template()  --  EXPERIMENTAL",
    "#",
    paste0("#  keys       : ", paste(keys, collapse = ", ")),
    paste0("#  variables  : ",
           if (length(vars)) paste(utils::head(vars, 12), collapse = ", ")
           else "(none -- the rows come from `hierarchy`)"),
    paste0("#  kinds      : ", paste(kinds, collapse = ", ")),
    "#",
    "#  Three blocks follow.  Delete the ones this report does not need.",
    "#  Two things are NOT in the ARD and have to be added by hand:",
    "#    * row and column ORDER  ->  `levels = `",
    "#    * display labels        ->  `labels = `",
    if (guessed)
      "#  NOTE: `cols` was not given; the first key is used.  Check it."
    else NULL,
    if (length(rest))
      paste0("#  NOTE: keys outside `cols` became row keys: ",
             paste(rest, collapse = ", "))
    else NULL,
    if (overall)
      "#  NOTE: an overall sentinel is present; edit the `overall` label."
    else NULL,
    .ard_bar("", "="),
    "",
    # Everything else is `rtfreporter::`-qualified on purpose, so this is
    # the only line the script needs, and only for `%>%`.
    if (identical(op, "%>%"))
      c("library(magrittr)   # for %>% ; dplyr re-exports it too", "")
    else NULL)

  # -- 1. ARD -> table data.frame ---------------------------------------
  # Half the real reports need to reach between the two steps -- to derive a
  # key from a statistic, to add a constant column, to indent a label.
  # Showing the seam with an empty `mutate()` turns "you had to know to
  # split this" into "delete the line you do not need".
  norm_args <- c(
    if (length(hierarchy)) paste0("    hierarchy = ", vecq(hierarchy)) else NULL,
    if (overall) "    overall   = \"Any event\"        # <- label for the sentinel"
    else NULL)
  if (length(norm_args) > 1L) {
    norm_args[-length(norm_args)] <- paste0(norm_args[-length(norm_args)], ",")
  }
  head1 <- c(
    .ard_bar("1. ARD -> table data.frame"),
    paste0("tbl_df <- ard ", op),
    if (length(norm_args))
      c("  rtfreporter::ard_normalize(", norm_args, paste0("  ) ", op))
    else paste0("  rtfreporter::ard_normalize() ", op),
    # the trailing comment stays in one column whichever operator it is
    paste0("  # dplyr::mutate() ", op, strrep(" ", 34L - 20L - nchar(op)),
           "# <- a key derived from a statistic."),
    "  #                                  A constant heading or a label rule",
    "  #                                  goes in `rows` / `label` below.",
    "  rtfreporter::ard_spread(",
    paste0("    cols  = ", vecq(cols), ","),
    if (length(row_parts))
      paste0("    rows  = c(", paste(row_parts, collapse = ", "), "),")
    else NULL,
    if (length(hierarchy) > 1L)
      paste0("    label = c(label = ", q(utils::tail(hierarchy, 1L)), "),")
    else NULL)

  {
    cell_lines <- character(0)
    for (kd in kinds) {
      s <- d[!is.na(d$.kind) & d$.kind == kd, , drop = FALSE]
      have <- .ard_first_seen(s$stat_name)
      if (identical(kd, "continuous")) {
        cand <- c(
          "n"              = tok("N"),
          "Mean (SD)"      = paste0(tok("mean"), " (", tok("sd"), ")"),
          "Median"         = tok("median"),
          "Q1, Q3"         = paste0(tok("p25"), ", ", tok("p75")),
          "Min, Max"       = paste0(tok("min"), ", ", tok("max")),
          "CV (%)"         = tok("cv"),
          "Geometric Mean" = tok("geom_mean"),
          "95% CI"         = paste0(tok("conf.low"), ", ", tok("conf.high")))
        keepc <- vapply(cand, function(t)
          all(gsub("[{}]", "", gsub(":[^}]*", "", .ard_tokens(t))) %in% have),
          logical(1))
        cand <- cand[keepc]
        if (!length(cand)) next
        rows_txt <- .ard_aligned(names(cand), unname(cand), "        ")
        cell_lines <- c(cell_lines,
                        paste0("      ", kd, " = c("),
                        paste0(rows_txt, c(rep(",", length(rows_txt) - 1L), "")),
                        "      )")
      } else {
        tpl <- if (all(c("n", "p") %in% have)) paste0(tok("n"), " ({p:.1f%})")
               else if ("n" %in% have) tok("n") else paste0("{", have[1], "}")
        cell_lines <- c(cell_lines, paste0("      ", kd, " = ", q(tpl)))
      }
    }
    # a comma after every block but the last
    ends <- which(cell_lines == "      )" |
                    grepl("^      [a-z]+ = \"", cell_lines))
    if (length(ends) > 1L) {
      cell_lines[utils::head(ends, -1L)] <-
        paste0(cell_lines[utils::head(ends, -1L)], ",")
    }
    L <- c(L, head1, "    cells = list(", cell_lines, "    )", "  )")
  }

  # -- 2. column header --------------------------------------------------
  L <- c(L, "", .ard_bar("2. column header"))
  hdr_arg <- NULL
  if (length(cols) > 1L) {
    L <- c(L,
      "# Nothing to write here: as_rtftables(header_sep = ) rebuilds the",
      "# spanning header from the \"____\" in the column names.")
  } else {
    n <- tryCatch(ard_pull(ard, cols = cols), error = function(e) NULL)
    if (!is.null(n)) {
      L <- c(L,
        "# The denominator each percentage used, straight out of the ARD.",
        paste0("#   ard_pull() found  ",
               paste0(names(n), " = ", as.integer(n), collapse = ",  ")),
        paste0("arm_n      <- rtfreporter::ard_pull(ard, cols = ", vecq(cols), ")"),
        "col_header <- c(",
        "  \"Characteristic\",",
        paste0("  paste0(names(arm_n), \"\\nN = \", as.integer(arm_n))"),
        ")")
      hdr_arg <- "  col_header = col_header,"
    } else {
      L <- c(L,
        "# The header denominator is not decidable from this ARD alone.",
        paste0("# Run  rtfreporter::ard_pull(ard, cols = ", vecq(cols), ")"),
        "# read the list of candidates it prints, then name the one you want:",
        paste0("#   arm_n <- rtfreporter::ard_pull(ard, cols = ", vecq(cols),
               ", variable = \"<pick one>\")"))
    }
  }

  # -- 3. table data.frame -> RTF pages ----------------------------------
  L <- c(L, "",
    .ard_bar("3. table data.frame -> RTF pages"),
    "pages <- rtfreporter::as_rtftables(",
    "  tbl_df,",
    paste0("  stub_vars  = ", vecq(stub), ",   # row keys plus the label column"),
    hdr_arg,
    "  group_by   = \"indent\",",
    "  blank_rows = \"between_groups\",",
    "  split      = \"group_safe\",",
    "  max_rows   = 22,",
    "  border     = \"tfl\"",
    ")")

  code <- L[!vapply(L, is.null, logical(1))]
  cat(paste(code, collapse = "\n"), "\n")
  if (!is.null(file)) writeLines(code, file)
  invisible(code)
}


# ============================================================================
#  package-level help
# ============================================================================

#' Experimental: cards/cardx ARD to table data.frame
#'
#' @description
#' A small family that turns an **ARD** (Analysis Results Data, as produced by
#' \pkg{cards} and \pkg{cardx}) into the table `data.frame` that
#' [as_rtftables()] consumes.  The target is *one ARD record, one table row*.
#' Layouts that print one record over two lines, or that need derived rows such
#' as marginal totals, stay a human job -- do them on the returned data frame,
#' or on the long frame from [ard_normalize()].
#'
#' @section The functions:
#' \describe{
#'   \item{[ard_keys()]}{What keys, variables, contexts and statistics an ARD
#'     actually holds.}
#'   \item{[ard_normalize()]}{ARD to a flat, explicitly keyed long table.}
#'   \item{[ard_spread()]}{Long table to the wide table data.frame.}
#'   \item{[ard_template()]}{Emit runnable conversion code for a given ARD.}
#'   \item{[ard_overall()]}{Where the table's overall row comes from.}
#'   \item{[ard_pull()]}{A statistic keyed like the spread columns, for a
#'     column header or an overall row.}
#'   \item{[ard_spec()], [read_ard_spec()], [write_ard_spec()],
#'     [ard_spec_template()]}{The spreadsheet definition file.}
#' }
#'
#' @section Nothing is read from the object's attributes:
#' A cards ARD carries attributes as well as rows, but they are not a contract:
#' `attr(ard, "args")` lists `by` and `variables` in a different order per
#' generator and cannot tell them apart, it keeps only the **first** operand's
#' value after `dplyr::bind_rows()`, and it is not updated when the ARD is
#' filtered.  The ARD's class survives `bind_rows()` with a differently shaped
#' ARD, so dispatching on it is no safer.  Every structural decision here comes
#' from an explicit argument instead; only the tibble's rows are inspected.
#'
#' @section The cell template grammar:
#' A template is a string with `{...}` tokens naming statistics:
#' \tabular{ll}{
#'   `{mean}`      \tab the value \pkg{cards} itself formatted (`fmt_fun`) \cr
#'   `{mean:.1f}`  \tab 1 decimal place, rounded per `rounding` \cr
#'   `{p:.1f\%}`   \tab multiplied by 100 first, then 1 decimal \cr
#'   `{mean:.3s}`  \tab 3 significant digits \cr
#'   `{n:d}`       \tab integer \cr
#'   `{n:stat}`    \tab the `stat` column, `as.character()`, untouched \cr
#'   `{mean:stat_fmt}` \tab the `stat_fmt` column, demanded
#' }
#' An ARD carries two values per statistic and both are reachable: a token
#' the two specs that reach them are spelled as the ARD spells them, so there
#' is no mapping to learn.  `:stat_fmt` takes the string \pkg{cards} formatted
#' and **errors** when this ARD has none, because that is a demand; `:stat`
#' takes the value untouched, character statistics such as `method` included;
#' any other spec formats `stat` here.  A **bare** token is the forgiving one
#' --- it prefers `stat_fmt` and falls back to `stat` --- since `stat_fmt` is
#' optional and an ARD built without `fmt_fun` would otherwise produce
#' nothing.  The two differ for a proportion: \pkg{cards} writes `61.6` into
#' `stat_fmt` while `stat` holds `0.616`, so `{p}` and `{p:.1f\%}` agree and
#' `{p:.1f}` does not.  For `stats = "rows"`, where a value goes into the cell without
#' a template, `ard_spread(value = )` makes the same choice.
#' A template whose statistics are not all present yields `NA`, which is what
#' lets `c("{n} ({p})", "{n}")` act as a fallback chain.  A **named** vector of
#' templates produces one table row per element, the name being the row label:
#' that is how `Min` and `Max` become a single `Min, Max` line.
#'
#' @section How a `cells` entry is chosen (and why it survives a cards upgrade):
#' `context` is a \pkg{cards} implementation detail and it moves.
#' `ard_continuous()` stamps `"continuous"`, but its 0.9 rename
#' `ard_summary()` stamps `"summary"`; `ard_categorical()` stamps
#' `"categorical"`, but `ard_tabulate()` stamps `"tabulate"`.  Keying `cells`
#' on the context alone would tie your script to one \pkg{cards} generation and
#' produce **no cells at all** against another.
#'
#' So every summary -- a variable under one context -- is also classified
#' from what its rows actually contain, its **kind**:
#' \describe{
#'   \item{`"categorical"`}{the summary has levels to enumerate (some
#'     `variable_level` is present): a factor, a dichotomous value of interest,
#'     a hierarchy term.}
#'   \item{`"continuous"`}{it does not -- one row per statistic of one numeric
#'     variable.}
#' }
#' A variable both summarised and tabulated in one ARD is two summaries, and
#' gets one kind for each.
#' That reading is structural, so it is the same on every \pkg{cards} version,
#' past and future.  A `cells` entry is then matched in this order:
#' \enumerate{
#'   \item the analysis variable's own name;
#'   \item `context`, with the known spellings treated as equivalent;
#'   \item the kind, likewise;
#'   \item `"default"`.
#' }
#' Step 2 lets a context-specific entry win where you wrote one -- cardx's
#' `"proportion_ci"`, `"survival"`, `"stats_t_test"` and friends.  Step 3 is
#' what keeps `continuous` / `categorical` (or `summary` / `tabulate`, either
#' spelling) working when \pkg{cards} renames a verb again.
#'
#' [ard_keys()] prints both: the contexts, which are version-specific, and the
#' kinds, which are not.
#'
#' @section Where a value lives depends on how the ARD was built:
#' Two things a table needs have no fixed home in an ARD, because cards offers
#' more than one way to produce them.  Neither is guessed:
#' \describe{
#'   \item{the overall row}{`ard_stack_hierarchical(over_variables = TRUE)`
#'     writes an "any event" row as the sentinel variable
#'     `..ard_hierarchical_overall..`; summarise each level separately and bind
#'     the results and there is no sentinel -- that block counts the treatment
#'     itself, so the arm sits in `variable` / `variable_level`.
#'     [ard_overall()] says which.}
#'   \item{the denominator}{in one ARD `stat_name == "N"` is the per-arm
#'     denominator on the summary rows and the **study** total on the by
#'     variable's own rows, where the per-arm count is `n` instead; and an
#'     author may compute their own.  [ard_pull()] names the statistic and
#'     lists the candidates when the choice is ambiguous.}
#' }
#' Column headers are rtfreporter's own job -- `col_header` takes a plain
#' character vector -- so [ard_spread()] builds the body only, and
#' [ard_pull()] is there when the header needs a number that must agree with
#' the percentages.
#'
#' @section Lifecycle:
#' **Experimental.**  These functions are newer than the rest of the package
#' and are not covered by its stability expectations: names and arguments may
#' change, and the family may be withdrawn.  Nothing else in \pkg{rtfreporter}
#' depends on them.
#'
#' @name rtfreporter-ard
#' @aliases ard-experimental
#' @seealso [as_rtftables()], [stub_cols()]
NULL

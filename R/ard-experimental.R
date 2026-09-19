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
  "ard_pull", "ard_keys", "ard_normalize", "ard_overall", "ard_spread", "ard_table", "ard_template",
  "ard_cells",
  "ard_spec", "ard_spec_template", "read_ard_spec", "write_ard_spec",
  "ard_round")


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
    "stat", "stat_fmt", ".kind", ".depth", ".label", ".overall")
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
#  ard_round()
# ============================================================================

#' Round the way SAS rounds, or the way R rounds
#'
#' `ard_round()` exists because the two families disagree on a tie.  Base R
#' uses IEEE "round half to even" (`round(0.5)` is `0`, `round(2.5)` is `2`),
#' while SAS's `ROUND()` rounds a half **away from zero** (`0.5` becomes `1`,
#' `2.5` becomes `3`).  A clinical table that must match a SAS-produced one
#' needs the SAS rule, which is why it is the default here.
#'
#' @param x Numeric vector.
#' @param digits Number of decimal places.
#' @param type `"sas"` (half away from zero, the default) or `"r"` (base R's
#'   half-to-even).
#'
#' @return A numeric vector.
#'
#' @section Lifecycle:
#' **Experimental.**  See [rtfreporter-ard].
#'
#' @examples
#' ard_round(c(0.5, 1.5, 2.5, -0.5), 0)            # SAS: 1 2 3 -1
#' ard_round(c(0.5, 1.5, 2.5, -0.5), 0, type = "r") # R:   0 2 2  0
#' @export
ard_round <- function(x, digits = 0, type = c("sas", "r")) {
  type <- match.arg(type)
  x <- as.numeric(x)
  if (identical(type, "r")) return(round(x, digits))
  m <- 10^digits
  # scale, nudge off a representation error, then truncate towards zero
  z <- abs(x) * m
  z <- floor(z + 0.5 + sqrt(.Machine$double.eps) * abs(z))
  sign(x) * z / m
}


# ============================================================================
#  the {token} mini-language
# ============================================================================
#
#   {mean}            the value cards itself formatted (its fmt_fun output)
#   {mean:.1f}        1 decimal, rounded with `round`
#   {p:.1f%}          multiply by 100 first, then 1 decimal
#   {mean:.3s}        3 significant digits
#   {n:d}             integer
#   {n:raw}           the raw `stat`, as.character(), untouched
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
  if (identical(spec, "raw")) {
    if (is.na(stat)) return(NA_character_)
    return(as.character(stat))
  }
  # "" and "fmt" both mean cards' own formatted value; "fmt" is the spelling
  # that says so out loud, next to a "{x:raw}" that asks for the unformatted
  # `stat` and a "{x:.1f}" that formats `stat` here.
  if (!nzchar(spec) || identical(spec, "fmt")) {
    if (is.na(stat_fmt)) return(NA_character_)
    return(as.character(stat_fmt))
  }
  pct <- grepl("%$", spec)
  spec <- sub("%$", "", spec)
  x <- suppressWarnings(as.numeric(stat))
  if (is.na(x)) return(NA_character_)
  if (pct) x <- x * 100
  if (grepl("^[.][0-9]+f$", spec)) {
    d <- as.integer(gsub("[.f]", "", spec))
    return(sprintf(paste0("%.", d, "f"), ard_round(x, d, round_type)))
  }
  if (grepl("^[.][0-9]+s$", spec)) {
    d <- as.integer(gsub("[.s]", "", spec))
    return(format(signif(x, d), trim = TRUE, scientific = FALSE))
  }
  if (identical(spec, "d")) {
    return(sprintf("%.0f", ard_round(x, 0, round_type)))
  }
  .ard_stop(sprintf("Unknown format spec '%s'. Use .Nf, .Ns, d, raw, or a %s suffix.",
                    spec, "%"))
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
.ard_chain_el <- function(x) {
  if (inherits(x, "formula")) {
    if (length(x) != 3L) {
      .ard_stop(paste0("A `cells` guard needs both sides: ",
                       "`condition ~ template`. This one has only a right."))
    }
    e <- environment(x)
    if (is.null(e)) e <- baseenv()
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
.ard_chain <- function(x) lapply(as.list(x), .ard_chain_el)

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
# resolves wins.
.ard_chain_value <- function(chain, s, round_type) {
  gd <- NULL
  for (el in chain) {
    if (!is.null(el$cond)) {
      if (is.null(gd)) gd <- .ard_guard_data(s)
      if (!.ard_guard_ok(el, gd)) next
    }
    v <- .ard_fill(el$tpl, s$stat_name, s$stat, s$stat_fmt, round_type)
    if (!is.na(v)) return(v)
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
#  So each variable is *also* classified from what its rows actually contain --
#  `.kind`, which is "categorical" when the variable has levels to enumerate
#  (any non-missing `variable_level`: a factor, a dichotomous value of
#  interest, a hierarchy term) and "continuous" when it does not (one row per
#  statistic of one numeric variable).  That reading is structural, so it holds
#  across every cards version, past and future.
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
  v <- as.character(d$variable)
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
  keys <- variable
  if (!is.na(context)) keys <- c(keys, .ard_context_aliases(context))
  if (!is.na(kind))    keys <- c(keys, .ard_context_aliases(kind))
  for (k in c(unique(keys), "default")) {
    if (!is.na(k) && k %in% names(cells) && !is.null(cells[[k]])) {
      return(.ard_cell_entry(cells[[k]]))
    }
  }
  NULL
}


# ============================================================================
#  ard_keys()
# ============================================================================

#' What is actually inside an ARD
#'
#' Prints, and returns invisibly, the structural facts you need in order to
#' call [ard_table()]: the grouping-variable names that appear in the
#' `group1..groupN` columns, the analysis variables, the `context` values and
#' the statistics each context carries.  All of it is read from the tibble's
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
#' @seealso [ard_table()], [ard_template()]
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
#' @return An object of class `ard_overall`, for [ard_normalize()] and
#'   [ard_table()]'s `overall` argument.  That argument also takes a bare
#'   string, which is `ard_overall(label)`.
#'
#' @section Lifecycle:
#' **Experimental.**  See [rtfreporter-ard].
#'
#' @examples
#' ard_overall("Any TEAE")                      # the cards sentinel rows
#' ard_overall("Any TEAE", from = "TRT01P")     # a separately-built block
#' @seealso [ard_normalize()], [ard_table()]
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
#' @return An object of class `ard_cells`, for `cells` in [ard_spread()] and
#'   [ard_table()].
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
#' an overall row, not in a body cell, so [ard_table()] puts them nowhere.
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
#' @param cols The column key(s), named as in [ard_table()] -- the grouping
#'   variable's own name, not its `group*` position.
#' @param stat Statistic to read; `"N"` by default.
#' @param variable,context Restrict to this analysis variable and/or this
#'   `context`.  Use them when the error message says the choice is ambiguous,
#'   or to ask for the key variable's own rows.
#' @param levels Optional level order for the keys, so the result lines up
#'   with the table's columns.
#' @param sep Separator between multiple `cols` keys; match [ard_table()].
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
#'   [ard_table()]
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
  ord <- if (is.null(levels)) .ard_first_seen(key) else {
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
#' for anything [ard_table()] does not do by itself: marginal totals, derived
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
#'   the `total_n` row.
#' @param drop_key_variables When `TRUE` (default), drops the rows that merely
#'   describe a key variable itself -- the `context == "tabulate"` counts of the
#'   by-variable -- which no table cell uses.
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
#' @return A data frame with one row per ARD statistic: the key columns, the
#'   ARD's own `variable` / `variable_level` / `context` / `stat_name` /
#'   `stat_label` / `stat` / `stat_fmt`, the structural classification `.kind`
#'   (`"continuous"` / `"categorical"`, see [rtfreporter-ard]), `.label` (the
#'   deepest non-missing hierarchy value, or `variable_level`), `.label_order`
#'   (that label's position in the level order its factor declared, `NA` when
#'   it declared none), `.overall`, and `.depth` --- 1 = outermost within a
#'   `hierarchy`, 0 = a row of one that is not one of its levels, and `NA`
#'   throughout when no `hierarchy` was given, since depth only means
#'   something inside one.
#'
#' @section Lifecycle:
#' **Experimental.**  See [rtfreporter-ard].
#'
#' @seealso [ard_spread()], [ard_table()], [ard_keys()]
#' @export
ard_normalize <- function(ard, keys = NULL, hierarchy = character(),
                          overall = NULL,
                          drop_contexts = c("attributes", "total_n"),
                          drop_key_variables = TRUE) {
  d <- as.data.frame(ard, stringsAsFactors = FALSE)
  if (!"context" %in% names(d) || !"stat_name" %in% names(d)) {
    .ard_stop("`ard` does not look like a cards ARD (no `context`/`stat_name`).")
  }
  ignored <- NULL
  if (length(drop_contexts)) {
    gone <- d[d$context %in% drop_contexts, , drop = FALSE]
    ignored <- .ard_ignored_bind(
      ignored, .ard_tally(gone, "not a table statistic"))
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
  } else {
    # NA, not 1: depth only means something inside a hierarchy, and a column
    # that says "there is no hierarchy here" survives every manipulation the
    # caller may do between ard_normalize() and ard_spread(), where an
    # attribute does not.
    d$.depth <- NA_integer_
    d$.label <- d$variable_level
  }

  if (drop_key_variables) {
    kill <- setdiff(keys, c(hierarchy, ovs$from))
    ignored <- .ard_ignored_bind(
      ignored, .ard_tally(d[d$variable %in% kill, , drop = FALSE],
                          "a key variable's own tabulation"))
    d <- d[!(d$variable %in% kill), , drop = FALSE]
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
             ".label_order", ".overall")
  front <- intersect(front, names(d))
  out <- d[, c(front, setdiff(names(d), front)), drop = FALSE]
  # `ard_ignored` is the one attribute left, and nothing reads it to build the
  # table: it is a report about rows that are no longer here, so there is no
  # column it could be.  Losing it only shortens a message.
  attr(out, "ard_ignored") <- ignored
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
      .ard_stop(sprintf("`%s`: no column '%s' in the normalized ARD. Available: %s",
                        what, ref,
                        paste(setdiff(names(d), c(".overall")), collapse = ", ")))
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
#' @param label Source of the row label, as a single (optionally named)
#'   reference.  Default `".label"`, which [ard_normalize()] sets to the
#'   deepest hierarchy value, or to `variable_level` when there is no
#'   hierarchy.  `NULL` drops the label column, which is what you want when
#'   every `cells` entry is named.
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
#' @param sort `TRUE` (default) sorts by the row keys, using the label column
#'   too when `levels` gave it an explicit order; `FALSE` leaves the rows as
#'   they were built.  A **character vector** names the keys instead, in
#'   priority order, each optionally prefixed `-` for descending:
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
#' @param round Tie-breaking rule for `{x:.1f}`-style tokens, `"sas"` or
#'   `"r"`.  See [ard_round()].
#' @param spec An [ard_spec()] definition (or the path to one) supplying
#'   labels, level orders, row templates and digits.  Arguments given
#'   explicitly win over the spec.
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
#' @return A data frame: the `rows` columns, the label column, then one column
#'   per column key.
#'
#' @section Lifecycle:
#' **Experimental.**  See [rtfreporter-ard].
#'
#' @seealso [ard_normalize()], [ard_table()]
#' @export
ard_spread <- function(x, cols, rows = NULL, label = ".label",
                       cells = "{n} ({p})", stats = c("cells", "rows"),
                       value = c("stat", "stat_fmt"),
                       levels = NULL, labels = NULL, sort = TRUE,
                       sep = "____",
                       round = c("sas", "r"), spec = NULL,
                       sort_stat = NULL, na = NA_character_, notes = TRUE) {
  stats <- match.arg(stats)
  value <- match.arg(value)
  round <- match.arg(round)
  # `stats = "rows"` lays the statistic out as a row and puts a value straight
  # in the cell, so which of the ARD's two values that is has to be sayable:
  # the raw numeric `stat` (the default -- a PK table is formatted later, by
  # set_decimal_split()) or cards' already-formatted `stat_fmt`.
  rows_numeric <- identical(stats, "rows") && identical(value, "stat")
  d <- as.data.frame(x, stringsAsFactors = FALSE)

  sp <- NULL
  if (!is.null(spec)) {
    sp <- if (is.character(spec)) read_ard_spec(spec) else ard_spec(spec)
    if (is.null(labels)) labels <- .ard_spec_labels(sp)
    if (is.null(levels)) {
      levels <- .ard_spec_levels(sp)
      ro <- .ard_spec_row_order(sp)
      lo <- if (is.null(label)) NULL else
        (if (!is.null(names(label)) && nzchar(names(label)[1])) names(label)[1]
         else sub("^[.]", "", as.character(label)[1]))
      if (length(ro) && !is.null(lo)) levels[[lo]] <- ro
    }
    if (missing(cells))  cells  <- .ard_spec_cells(sp)
    if (missing(round) && !is.null(sp$round) && any(!is.na(sp$round))) {
      round <- as.character(stats::na.omit(sp$round))[1L]
    }
  }

  if (!".kind" %in% names(d) && all(c("variable", "variable_level") %in% names(d))) {
    d$.kind <- .ard_kind(d)
  }

  .ard_check_named(labels, "labels")
  .ard_check_named(levels, "levels")

  if (!inherits(x, "ard_long") &&
      !any(c(".label", ".kind", "stat_name") %in% names(d))) {
    .ard_stop(paste0(
      "`x` does not look like an ard_normalize() result: it has none of\n",
      "  `.label`, `.kind`, `stat_name`.  Pass the ARD through\n",
      "  ard_normalize() first, or use ard_table(), which does both."))
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
  rowrefs <- .ard_refs(rows, d, "rows")
  labref  <- if (is.null(label)) list() else .ard_refs(label, d, "label")
  if (!length(colrefs)) .ard_stop("`cols` is required: name the key that goes across.")

  # ---- recode key values and build the ordering factors -------------------
  lev_for <- function(r) {
    if (is.null(levels)) return(NULL)
    levels[[r$out]] %||% levels[[r$ref]]
  }
  recode <- function(v) {
    if (is.null(labels)) return(as.character(v))
    v <- as.character(v)
    hit <- !is.na(v) & v %in% names(labels)
    v[hit] <- unname(labels[v[hit]])
    v
  }

  # ---- the column key -----------------------------------------------------
  colparts <- lapply(colrefs, function(r) recode(d[[r$ref]]))
  ok <- Reduce(`&`, lapply(colparts, function(v) !is.na(v)))
  ignored <- .ard_ignored_bind(
    attr(x, "ard_ignored", exact = TRUE),
    .ard_tally(d[!ok, , drop = FALSE], "no value for a `cols` key"))
  d <- d[ok, , drop = FALSE]
  colparts <- lapply(colparts, function(v) v[ok])
  colkey <- do.call(paste, c(colparts, list(sep = sep)))

  # column order: lexicographic on the ordered col keys
  ord_parts <- lapply(seq_along(colrefs), function(i) {
    lv <- lev_for(colrefs[[i]])
    lv <- if (is.null(lv)) .ard_first_seen(colparts[[i]]) else recode(lv)
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
      vals <- vapply(entry$chains, .ard_chain_value, "", s = sub,
                     round_type = round)
      pieces[[length(pieces) + 1L]] <- data.frame(
        .lab = entry$labels, .col = ckey, .valn = NA_real_, .valc = vals,
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
        v <- .ard_chain_value(entry$chains[[1L]], s2, round)
        pieces[[length(pieces) + 1L]] <- data.frame(
          .lab = lv, .col = ckey, .valn = NA_real_, .valc = v,
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
    v <- recode(long[[r$out]])
    lv <- lev_for(r)
    if (!is.null(lv)) {
      long[[r$out]] <- .ard_as_factor(v, recode(lv))
    } else if (!is.null(labels) && any(as.character(long[[r$out]]) != v)) {
      long[[r$out]] <- .ard_as_factor(v, unname(labels[names(labels) %in%
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
                             labels)
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
  } else if (isTRUE(sort) && length(rowname_cols)) {
    keys <- lapply(rowname_cols, function(k) out[[k]])
    if (!is.null(label_out) && is.factor(out[[label_out]])) {
      keys <- c(keys, list(out[[label_out]]))
    }
    out <- out[do.call(order, keys), , drop = FALSE]
  }
  rownames(out) <- NULL
  # The tally is NOT attached by default.  The result is a plain data frame
  # that the caller will compare against whatever they built before -- that
  # comparison is how anyone decides to adopt this -- and an extra attribute
  # makes all.equal() report a difference that is not in the table.
  if (identical(notes, "attr")) attr(out, "ard_ignored") <- ignored
  if (!isFALSE(notes)) .ard_notes_message(ignored, "ard_table()")
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
#  ard_table()
# ============================================================================

#' Convert a cards/cardx ARD straight into a table data.frame
#'
#' The one-call form: [ard_normalize()] followed by [ard_spread()].  Use it
#' when the table is the ordinary "one ARD record, one table row" shape; when
#' it is not, call the two steps separately and do your own work in between.
#'
#' @inheritParams ard_normalize
#' @inheritParams ard_spread
#'
#' @return A data frame ready for [as_rtftables()].
#'
#' @section Lifecycle:
#' **Experimental.**  See [rtfreporter-ard].
#'
#' @examples
#' if (requireNamespace("cards", quietly = TRUE)) {
#'   ard <- cards::ard_stack(
#'     cards::ADSL, .by = ARM,
#'     cards::ard_continuous(variables = AGE),
#'     cards::ard_categorical(variables = SEX))
#'
#'   ard_table(
#'     ard,
#'     cols  = "ARM",
#'     rows  = c(group = "variable"),
#'     cells = list(
#'       continuous  = c("n"         = "{N:d}",
#'                       "Mean (SD)" = "{mean:.1f} ({sd:.2f})",
#'                       "Min, Max"  = "{min:.1f}, {max:.1f}"),
#'       categorical = "{n:d} ({p:.1f%})"))
#' }
#' @seealso [ard_normalize()], [ard_spread()], [ard_template()], [rtfreporter-ard]
#' @export
ard_table <- function(ard, cols, rows = NULL, label = ".label",
                      hierarchy = character(), overall = NULL, keys = NULL,
                      cells = "{n} ({p})", stats = c("cells", "rows"),
                      value = c("stat", "stat_fmt"),
                      levels = NULL, labels = NULL, sort = TRUE,
                      sep = "____", round = c("sas", "r"),
                      spec = NULL, sort_stat = NULL, na = NA_character_,
                      notes = TRUE,
                      drop_contexts = c("attributes", "total_n"),
                      drop_key_variables = TRUE) {
  x <- ard_normalize(ard, keys = keys, hierarchy = hierarchy,
                     overall = overall, drop_contexts = drop_contexts,
                     drop_key_variables = drop_key_variables)
  args <- list(x = x, cols = cols, rows = rows, label = label,
               stats = match.arg(stats), value = match.arg(value),
               levels = levels, labels = labels,
               sort = sort, sep = sep,
               round = match.arg(round), spec = spec, sort_stat = sort_stat,
               na = na, notes = notes)
  if (!missing(cells)) args$cells <- cells
  do.call(ard_spread, args)
}


# ============================================================================
#  the definition file
# ============================================================================

.ard_spec_cols <- function() {
  c("variable", "label", "order", "context", "row", "template", "levels",
    "round", "digits", "signif")
}

#' A spreadsheet-shaped definition of how an ARD becomes a table
#'
#' `ard_spec()` validates (and fills out) the definition table that
#' [ard_table()] and [ard_spread()] accept as `spec =`.  It carries the four
#' things that otherwise have to be repeated in every script: the display
#' label of each variable, the row templates, the rounding family, and the
#' number of decimal or significant digits.
#'
#' One row of the spec is one row of the finished table -- or, when `row` is
#' blank, one row per level of that variable.
#'
#' @section Columns:
#' \describe{
#'   \item{`variable`}{Analysis variable the row applies to.  `"default"`, or
#'     a `context` value such as `"categorical"`, acts as a fallback.}
#'   \item{`label`}{Display label for the variable (the row-group text).}
#'   \item{`order`}{Integer; the order variables appear in the table.}
#'   \item{`context`}{Optional; restrict the row to one ARD `context`.}
#'   \item{`row`}{Row label within the variable, e.g. `Mean (SD)` or
#'     `Min, Max`.  Blank means one row per `variable_level`.}
#'   \item{`template`}{The cell recipe, e.g. `{mean} ({sd})` or
#'     `{min}, {max}`.  Several templates separated by `|` form a fallback
#'     chain: the first that resolves wins.}
#'   \item{`levels`}{`|`-separated level order for that variable.}
#'   \item{`round`}{`sas` (half away from zero) or `r` (half to even).}
#'   \item{`digits`}{Decimal places used for tokens with no inline spec.  A
#'     comma-separated list applies per token, e.g. `1,2` for
#'     `{mean} ({sd})`.}
#'   \item{`signif`}{Significant digits; wins over `digits`.}
#' }
#'
#' @param x A data frame (or anything coercible) with the columns above.
#'   Missing optional columns are added as `NA`.
#'
#' @return A data frame of class `ard_spec`.
#'
#' @section Lifecycle:
#' **Experimental.**  See [rtfreporter-ard].
#'
#' @seealso [read_ard_spec()], [write_ard_spec()], [ard_spec_template()]
#' @export
ard_spec <- function(x) {
  d <- as.data.frame(x, stringsAsFactors = FALSE)
  if (!"variable" %in% names(d)) {
    .ard_stop("An ARD spec needs at least a `variable` column.")
  }
  for (cn in .ard_spec_cols()) if (!cn %in% names(d)) d[[cn]] <- NA
  d <- d[, c(.ard_spec_cols(), setdiff(names(d), .ard_spec_cols())),
         drop = FALSE]
  for (cn in c("variable", "label", "context", "row", "template", "levels",
               "round")) {
    d[[cn]] <- as.character(d[[cn]])
    d[[cn]][!is.na(d[[cn]]) & !nzchar(trimws(d[[cn]]))] <- NA
  }
  d$order <- suppressWarnings(as.numeric(d$order))
  bad <- !is.na(d$round) & !d$round %in% c("sas", "r")
  if (any(bad)) .ard_stop("`round` must be \"sas\" or \"r\".")
  class(d) <- c("ard_spec", "data.frame")
  d
}

.ard_spec_labels <- function(sp) {
  s <- sp[!is.na(sp$label) & !is.na(sp$variable), , drop = FALSE]
  s <- s[!duplicated(s$variable), , drop = FALSE]
  if (!nrow(s)) return(NULL)
  if (any(!is.na(s$order))) s <- s[order(s$order, na.last = TRUE), , drop = FALSE]
  stats::setNames(s$label, s$variable)
}

.ard_spec_levels <- function(sp) {
  s <- sp[!is.na(sp$levels) & !is.na(sp$variable), , drop = FALSE]
  s <- s[!duplicated(s$variable), , drop = FALSE]
  if (!nrow(s)) return(NULL)
  out <- lapply(s$levels, function(v) trimws(strsplit(v, "|", fixed = TRUE)[[1]]))
  stats::setNames(out, s$variable)
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

.ard_spec_cells <- function(sp) {
  s <- sp[!is.na(sp$template), , drop = FALSE]
  if (!nrow(s)) return("{n} ({p})")
  if (any(!is.na(s$order))) s <- s[order(s$order, na.last = TRUE), , drop = FALSE]
  out <- list()
  for (v in unique(s$variable)) {
    ss <- s[s$variable == v, , drop = FALSE]
    chains <- lapply(seq_len(nrow(ss)), function(i) {
      tpls <- trimws(strsplit(ss$template[i], "|", fixed = TRUE)[[1]])
      vapply(tpls, .ard_apply_digits, "", ss$digits[i], ss$signif[i],
             USE.NAMES = FALSE)
    })
    if (all(is.na(ss$row))) {
      out[[v]] <- chains[[1L]]
    } else {
      out[[v]] <- stats::setNames(chains, ifelse(is.na(ss$row), "", ss$row))
    }
  }
  out
}

# The order the finished table's label column should take: each variable's own
# row labels (or its declared levels), concatenated in spec order.  A label only
# ever competes with labels from the same variable, so one global order is
# enough to sort them all.
.ard_spec_row_order <- function(sp) {
  s <- sp
  if (any(!is.na(s$order))) s <- s[order(s$order, na.last = TRUE), , drop = FALSE]
  out <- character(0)
  for (v in unique(s$variable)) {
    ss <- s[s$variable == v, , drop = FALSE]
    out <- c(out, if (any(!is.na(ss$row))) stats::na.omit(ss$row)
             else if (!is.na(ss$levels[1L]))
               trimws(strsplit(ss$levels[1L], "|", fixed = TRUE)[[1]])
             else character(0))
  }
  unique(as.character(out))
}

#' Read an ARD table definition from a spreadsheet
#'
#' @param path Path to an `.xlsx` (needs \pkg{readxl}) or `.csv` file.
#' @param sheet Sheet name or index, for Excel input.
#'
#' @return An [ard_spec()] data frame.
#'
#' @section Lifecycle:
#' **Experimental.**  See [rtfreporter-ard].
#'
#' @seealso [ard_spec()], [write_ard_spec()]
#' @export
read_ard_spec <- function(path, sheet = 1) {
  if (grepl("[.]csv$", path, ignore.case = TRUE)) {
    return(ard_spec(utils::read.csv(path, stringsAsFactors = FALSE,
                                    check.names = FALSE)))
  }
  .ard_need("readxl", "read_ard_spec() on an Excel file")
  ard_spec(as.data.frame(readxl::read_excel(path, sheet = sheet),
                         stringsAsFactors = FALSE))
}

#' Write an ARD table definition to a spreadsheet
#'
#' @param spec An [ard_spec()].
#' @param path Destination `.xlsx` (needs \pkg{writexl}) or `.csv`.
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
  class(sp) <- "data.frame"
  if (grepl("[.]csv$", path, ignore.case = TRUE)) {
    utils::write.csv(sp, path, row.names = FALSE, na = "")
    return(invisible(path))
  }
  .ard_need("writexl", "write_ard_spec() to an Excel file")
  writexl::write_xlsx(list(ard_spec = sp), path)
  invisible(path)
}

#' Scaffold a definition file from an ARD
#'
#' Walks the ARD and emits one [ard_spec()] row per analysis variable (per row
#' template, for a continuous one), pre-filled with the statistics that
#' variable actually carries.  Edit the labels and templates, save it, and hand
#' it back through `spec =`.
#'
#' @param ard A cards/cardx ARD.
#' @param path Optional destination; when given the spec is also written there
#'   with [write_ard_spec()].
#'
#' @return An [ard_spec()] data frame, invisibly when `path` is given.
#'
#' @section Lifecycle:
#' **Experimental.**  See [rtfreporter-ard].
#'
#' @seealso [ard_spec()], [ard_template()]
#' @export
ard_spec_template <- function(ard, path = NULL) {
  d <- ard_normalize(ard, drop_key_variables = FALSE)
  vars <- .ard_first_seen(d$variable)
  rows <- list()
  i <- 0L
  for (v in vars) {
    s <- d[d$variable == v, , drop = FALSE]
    ctx <- s$context[1L]
    i <- i + 1L
    if (identical(ctx, "continuous")) {
      have <- .ard_first_seen(s$stat_name)
      cand <- list(c("n", "{N}"), c("Mean (SD)", "{mean} ({sd})"),
                   c("Median", "{median}"), c("Min, Max", "{min}, {max}"))
      for (cc in cand) {
        need <- gsub("[{}]", "", .ard_tokens(cc[2]))
        if (!all(need %in% have)) next
        rows[[length(rows) + 1L]] <- data.frame(
          variable = v, label = v, order = i, context = ctx,
          row = cc[1], template = cc[2], levels = NA_character_,
          round = "sas", digits = NA_character_, signif = NA_character_,
          stringsAsFactors = FALSE)
      }
    } else {
      lv <- .ard_first_seen(s$variable_level)
      rows[[length(rows) + 1L]] <- data.frame(
        variable = v, label = v, order = i, context = ctx,
        row = NA_character_, template = "{n} ({p})",
        levels = if (length(lv)) paste(lv, collapse = " | ") else NA_character_,
        round = "sas", digits = NA_character_, signif = NA_character_,
        stringsAsFactors = FALSE)
    }
  }
  sp <- ard_spec(do.call(rbind, rows))
  if (is.null(path)) return(sp)
  write_ard_spec(sp, path)
  invisible(sp)
}


# ============================================================================
#  ard_template()
# ============================================================================

#' Write the conversion code for you
#'
#' Reads an ARD and prints a runnable [ard_table()] call, filled in with the
#' keys, hierarchy, contexts and statistics it actually found.  Because none
#' of the structure is guessed from the object's attributes, the generated
#' call is also a readable record of what the ARD contains.
#'
#' The emitted code is a starting point, not a finished table: in particular
#' it does **not** invent a row order, because the level order of a factor is
#' not recoverable from an ARD that was not built with `.attributes = TRUE`.
#'
#' @param ard A cards/cardx ARD.
#' @param cols Key(s) that go across.  `NULL` (default) uses the first key
#'   found and says so in a comment.
#' @param hierarchy Optional nested hierarchy, outermost first.
#' @param spec When `TRUE`, the generated code reads a definition file made by
#'   [ard_spec_template()] instead of inlining the `cells` list.
#' @param file Optional path to write the code to.
#'
#' @return The generated code, as a character vector, invisibly.
#'
#' @section Lifecycle:
#' **Experimental.**  See [rtfreporter-ard].
#'
#' @seealso [ard_table()], [ard_spec_template()]
#' @export
ard_template <- function(ard, cols = NULL, hierarchy = character(),
                         spec = FALSE, file = NULL) {
  d <- ard_normalize(ard, hierarchy = hierarchy, drop_key_variables = FALSE)
  gcols <- grep("^group[0-9]+$", names(as.data.frame(ard)), value = TRUE)
  keys <- unique(unlist(lapply(gcols, function(g)
    .ard_first_seen(as.data.frame(ard)[[g]]))))
  guessed <- is.null(cols)
  if (guessed) cols <- utils::head(keys, 1L)
  rest <- setdiff(keys, c(cols, hierarchy))

  vars <- setdiff(.ard_first_seen(d$variable), c(keys, hierarchy))
  ctxs <- .ard_first_seen(d$context)

  q <- function(v) paste0("\"", v, "\"")
  vec <- function(v) if (length(v) == 1L) q(v) else
    paste0("c(", paste(q(v), collapse = ", "), ")")

  L <- c(
    "# ---------------------------------------------------------------",
    "# generated by rtfreporter::ard_template()  --  EXPERIMENTAL",
    paste0("# keys found      : ", paste(keys, collapse = ", ")),
    paste0("# variables       : ", paste(utils::head(vars, 12), collapse = ", "),
           if (length(vars) > 12) " ..." else ""),
    paste0("# contexts        : ", paste(ctxs, collapse = ", ")),
    if (guessed)
      "# NOTE: `cols` was not supplied; the first key found is used. Check it."
    else NULL,
    if (length(rest))
      paste0("# NOTE: unused keys: ", paste(rest, collapse = ", "),
             " -- add them to `rows =` or `keep`.")
    else NULL,
    "# NOTE: row/column ORDER is not derived from the ARD. Add `levels = `.",
    "# ---------------------------------------------------------------")

  if (isTRUE(spec)) {
    L <- c(L,
      "spec <- read_ard_spec(\"ard-spec.xlsx\")",
      "",
      "tbl_df <- rtfreporter::ard_table(",
      "  ard,",
      paste0("  cols  = ", vec(cols), ","),
      if (length(hierarchy)) paste0("  hierarchy = ", vec(hierarchy), ",") else
        "  rows  = c(group = \"variable\"),",
      "  spec  = spec)")
  } else {
    cell_lines <- character(0)
    for (ct in ctxs) {
      s <- d[d$context == ct, , drop = FALSE]
      have <- .ard_first_seen(s$stat_name)
      if (identical(ct, "continuous")) {
        cand <- c("n" = "{N:d}", "Mean (SD)" = "{mean:.1f} ({sd:.2f})",
                  "Median" = "{median:.1f}", "Min, Max" = "{min:.1f}, {max:.1f}")
        keepc <- vapply(cand, function(t)
          all(gsub("[{}]", "", gsub(":[^}]*", "", .ard_tokens(t))) %in% have),
          logical(1))
        cand <- cand[keepc]
        if (!length(cand)) next
        cell_lines <- c(cell_lines, paste0(
          "    ", ct, " = c(",
          paste0("\"", names(cand), "\" = \"", unname(cand), "\"",
                 collapse = ",\n                     "), ")"))
      } else {
        tpl <- if (all(c("n", "p") %in% have)) "{n:d} ({p:.1f%})" else
          if ("n" %in% have) "{n:d}" else paste0("{", have[1], "}")
        cell_lines <- c(cell_lines, paste0("    ", ct, " = \"", tpl, "\""))
      }
    }
    L <- c(L,
      "tbl_df <- rtfreporter::ard_table(",
      "  ard,",
      paste0("  cols  = ", vec(cols), ","),
      if (length(hierarchy))
        paste0("  hierarchy = c(group1 = ", q(hierarchy[1]), ", label = ",
               q(utils::tail(hierarchy, 1)), "),")
      else "  rows  = c(group = \"variable\"),",
      "  cells = list(",
      paste0(paste(cell_lines, collapse = ",\n")),
      "  ),",
      "  round = \"sas\")")
  }

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
#'   \item{[ard_table()]}{Both of the above in one call.}
#'   \item{[ard_template()]}{Emit runnable conversion code for a given ARD.}
#'   \item{[ard_overall()]}{Where the table's overall row comes from.}
#'   \item{[ard_pull()]}{A statistic keyed like the spread columns, for a
#'     column header or an overall row.}
#'   \item{[ard_spec()], [read_ard_spec()], [write_ard_spec()],
#'     [ard_spec_template()]}{The spreadsheet definition file.}
#'   \item{[ard_round()]}{SAS-style versus R-style rounding.}
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
#'   `{mean:.1f}`  \tab 1 decimal place, rounded per `round` \cr
#'   `{p:.1f\%}`   \tab multiplied by 100 first, then 1 decimal \cr
#'   `{mean:.3s}`  \tab 3 significant digits \cr
#'   `{n:d}`       \tab integer \cr
#'   `{n:raw}`     \tab the raw `stat`, `as.character()`, untouched \cr
#'   `{mean:fmt}`  \tab the same as `{mean}`, said out loud
#' }
#' An ARD carries two values per statistic and both are reachable: a token
#' with no format spec (or `:fmt`) takes `stat_fmt`, the string \pkg{cards}
#' formatted; any other spec formats the raw `stat` here, and `:raw` takes it
#' untouched.  For `stats = "rows"`, where a value goes into the cell without
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
#' So every variable is also classified from what its rows actually contain --
#' its **kind**:
#' \describe{
#'   \item{`"categorical"`}{the variable has levels to enumerate (some
#'     `variable_level` is present): a factor, a dichotomous value of interest,
#'     a hierarchy term.}
#'   \item{`"continuous"`}{it does not -- one row per statistic of one numeric
#'     variable.}
#' }
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
#' character vector -- so [ard_table()] builds the body only, and
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

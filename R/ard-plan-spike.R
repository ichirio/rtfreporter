# ============================================================================
#  SPIKE -- a deferred, last-wins plan for the ARD half (#474)
# ============================================================================
#
#  This file is a PROTOTYPE and is meant to be thrown away if the idea does
#  not earn its place.  It is one file, like R/ard-experimental.R, and nothing
#  else in the package refers to it: delete the file, its test file, the
#  `.ard_plan_exports` block in NAMESPACE and the _pkgdown entry, and the
#  package is exactly what it was.
#
#  ---------------------------------------------------------------------------
#  What it is
#  ---------------------------------------------------------------------------
#
#  `ard_normalize()` and `ard_spread()` do the work the moment they are
#  called.  This is the same conversion written as DECLARATIONS that are
#  resolved once at the end:
#
#      ard_plan(ard) |>
#        plan_spread(cols = "TRT01P", rows = c(group = "variable")) |>
#        plan_cells(continuous = c("n"         = "{N:d}",
#                                  "Mean (SD)" = "{mean} ({sd})"),
#                   categorical = "{n} ({p:%})") |>
#        plan_digits(2) |>                # everything to 2 dp ...
#        plan_digits(AGE = 0) |>          # ... except AGE          <- LAST WINS
#        apply_plan()
#
#  ---------------------------------------------------------------------------
#  Two rules, and no new vocabulary
#  ---------------------------------------------------------------------------
#
#  1. LAST WINS.  Every verb adds a layer; layers are merged in order and a
#     later one overwrites what an earlier one said about the same key.  This
#     is tfrmt's `frmt_structure` rule, and it is what makes "set everything,
#     then fix one variable" a two-line edit instead of a rewrite.
#
#     Note this is a DIFFERENT rule from `ard_spread(cells = )`, which picks
#     by SPECIFICITY (variable, then context, then kind, then default) and
#     ignores order.  Both are defensible; the point of the spike is to find
#     out which one is nicer to write.  They do not conflict, because the plan
#     resolves its layers into a `cells` map and then hands it to the existing
#     engine -- specificity still decides what a variable with no layer of its
#     own gets.
#
#  2. THE KEYS ARE THE ONES YOU ALREADY KNOW.  `plan_cells()` takes exactly
#     what `ard_spread(cells = )` takes, `plan_spread()` takes exactly
#     `ard_spread()`'s other arguments, `plan_normalize()` takes
#     `ard_normalize()`'s.  Nothing new is learned; the layering is the only
#     new idea.
#
#  ---------------------------------------------------------------------------
#  Why it resolves to arguments rather than re-implementing anything
#  ---------------------------------------------------------------------------
#
#  `apply_plan()` builds `ard_normalize()`'s and `ard_spread()`'s argument
#  lists and calls them.  Nothing about the conversion is duplicated, so the
#  plan cannot drift away from the immediate form -- and `apply_plan(stage =
#  "args")` can SHOW the call the plan amounts to, which is the honest answer
#  to "what is this thing going to do".
#
#  ---------------------------------------------------------------------------
#  Known gaps (a spike states them rather than hiding them)
#  ---------------------------------------------------------------------------
#
#  * A token that states its own digits (`{mean:.2f}`) keeps them;
#    `plan_digits()` only fills tokens that left the question open (`{mean}`,
#    or `{p:%}`, which is plan-only notation for "percent, digits from the
#    plan").  The alternative -- a later `plan_digits()` overriding an
#    explicit token -- would make house-library templates tunable per study,
#    and is the first thing to reconsider.
#  * Guards (`c(n == 0 ~ "0", "{n} ({p})")`) are carried through untouched:
#    `plan_digits()` does not reach inside a formula.
#  * No call site is recorded on a layer, so an error still points at the
#    resolver.  That is the main thing to fix before this could be real.
#  * `plan_*` collides with the verb names on design/plan-resolver.  That is
#    informative rather than accidental -- both spikes want the same namespace
#    -- but the two would have to be reconciled before either shipped.
#
#  Deleting this file removes:
.ard_plan_exports <- c(
  "ard_plan", "plan_normalize", "plan_spread", "plan_cells",
  "plan_digits", "plan_round",
  "plan_n", "plan_mutate", "plan_filter", "plan_derive", "plan_fmt",
  "plan_stub", "plan_styles",
  "plan_group", "plan_hide", "plan_sort", "plan_blanks", "plan_pages",
  "plan_style", "plan_header",
  "plan_after", "apply_plan", "plan_template")


# -- layer plumbing ----------------------------------------------------------

# A plan is the ARD, untouched, plus an ordered list of declarations.  The ARD
# is held rather than transformed, so a plan can be printed, inspected and
# re-pointed at a new data cut without anything having been computed yet.
.plan_layer <- function(plan, kind, fields) {
  if (!inherits(plan, "ard_plan")) {
    .ard_stop("Expected an ard_plan; pipe from ard_plan(ard).")
  }
  fields <- fields[!vapply(fields, is.null, logical(1L))]
  # Recorded even when empty: calling the verb is the declaration, and
  # `plan_rtf()` with nothing in it still means "make pages".
  plan$layers[[length(plan$layers) + 1L]] <-
    list(kind = kind, fields = fields)
  # A fresh cache per plan VALUE.  The environment is shared by reference
  # between copies, so a derived plan must not inherit its parent's --
  # that is how a stale column list would outlive the layer that changed
  # it.  Replacing it here makes staleness impossible by construction.
  plan$cache <- new.env(parent = emptyenv())
  plan
}

# The layers of one kind, in the order they were declared.
# How far the plan goes is a fact about what it DECLARES, not something
# the caller should have to repeat.  Anything that only makes sense once
# there are pages -- the as_rtftables() settings, the header, the cell
# styles, the steps after -- means the answer is pages; otherwise the
# plan stops at the table data.frame.
.plan_reach <- function(plan) {
  kinds <- vapply(plan$layers, `[[`, "", "kind")
  if (any(kinds %in% c("group", "hide", "sort", "blanks", "pages",
                       "style", "header", "styles", "after"))) "pages"
  else "table"
}

# `cols`, `rows` and `label` name columns of the NORMALIZED frame, and
# until now the only way to find out what those are was to run the plan.
# Normalising is the cheap half -- a flatten, not a spread -- so print()
# can afford to do it and say.  The answer is cached in an environment
# because a plan is copied by value and the cache must not be.
.plan_norm <- function(plan) {
  if (!is.null(plan$cache$norm)) return(plan$cache$norm)
  v <- tryCatch({
    n_args <- .plan_merge(.plan_of(plan, "normalize"))
    if (isTRUE(plan$normalized)) plan$ard
    else suppressMessages(
      do.call(ard_normalize, c(list(ard = plan$ard), n_args)))
  }, error = function(e) NULL)
  # plan_mutate() / plan_filter() are part of this stage, so what they
  # add has to be part of the answer
  if (!is.null(v)) {
    v <- tryCatch(.plan_reshape(plan, v, c("mutate", "filter")),
                  error = function(e) v)
  }
  if (!is.null(v)) plan$cache$norm <- v
  v
}

# What a stage left behind, remembered as it goes.  apply_plan() fills
# these in, so a print() after a run costs nothing and shows everything.
.plan_remember <- function(plan, what, x) {
  plan$cache[[what]] <- x
  invisible(NULL)
}

.plan_of <- function(plan, kind) {
  keep <- vapply(plan$layers, function(l) identical(l$kind, kind), logical(1L))
  lapply(plan$layers[keep], `[[`, "fields")
}

# Last wins, per field.  `levels` and `labels` merge one name at a time so a
# later layer can add one variable's order without restating the rest -- which
# is the whole point of the rule at the granularity that matters.
.plan_merge <- function(layers, deep = character()) {
  out <- list()
  for (fields in layers) {
    for (nm in names(fields)) {
      if (nm %in% deep && is.list(out[[nm]]) && is.list(fields[[nm]])) {
        for (k in names(fields[[nm]])) out[[nm]][[k]] <- fields[[nm]][[k]]
      } else {
        out[[nm]] <- fields[[nm]]
      }
    }
  }
  out
}


# -- digits ------------------------------------------------------------------

# Fill the tokens that left their format open.  `{mean}` becomes `{mean:.2f}`
# and `{p:%}` becomes `{p:.1f%}`; `{N:d}` and `{mean:.3f}` are left alone
# because they already answered the question.
.plan_fill_one <- function(tpl, digits) {
  if (!is.character(tpl) || !length(tpl)) return(tpl)
  for (i in seq_along(tpl)) {
    for (tok in unique(.ard_tokens(tpl[i]))) {
      pt <- .ard_token_parts(tok)
      if (identical(pt$spec, "") || identical(pt$spec, "%")) {
        new <- paste0("{", pt$name, ":.", as.integer(digits), "f",
                      if (identical(pt$spec, "%")) "%" else "", "}")
        tpl[i] <- gsub(tok, new, tpl[i], fixed = TRUE)
      }
    }
  }
  tpl
}

# A cells entry is a bare template, a named vector of row templates, a chain
# (an unnamed list, possibly holding formulas) or an ard_cells object.  Only
# the character parts can be rewritten; a formula carries its own environment
# and is carried through untouched.
.plan_fill_entry <- function(entry, digits) {
  if (is.null(entry) || is.null(digits) || is.na(digits)) return(entry)
  if (is.character(entry)) return(.plan_fill_one(entry, digits))
  if (is.list(entry)) {
    for (i in seq_along(entry)) {
      if (is.character(entry[[i]])) {
        entry[[i]] <- .plan_fill_one(entry[[i]], digits)
      }
    }
  }
  entry
}

# `{p:%}` is plan-only notation.  A template that reaches the engine still
# carrying it never had a `plan_digits()` to answer it, and the engine's own
# message ("Unknown format spec") would not say that, so this one does.
.plan_check_open <- function(entry, key) {
  tpls <- if (is.character(entry)) entry else
    unlist(entry[vapply(entry, is.character, logical(1L))], use.names = FALSE)
  for (tpl in tpls) {
    for (tok in .ard_tokens(tpl)) {
      if (identical(.ard_token_parts(tok)$spec, "%")) {
        .ard_stop(paste0(
          "`", tok, "` asks the plan for its digits, but nothing declared ",
          "them for ", sQuote(key), ".\n",
          "  Add `plan_digits(", key, " = <n>)`, or a plan-wide ",
          "`plan_digits(<n>)`,\n",
          "  or write the digits in the template: `{",
          .ard_token_parts(tok)$name, ":.1f%}`."))
      }
    }
  }
  invisible(NULL)
}


# -- what kind of thing is this? ---------------------------------------------

# Four answers, decided by the columns and nothing else -- no class, no
# attribute, because an ARD keeps its class through a bind_rows() with a
# frame of a different shape and so cannot be trusted to say what it is.
#
#   "ard"        a cards/cardx ARD: needs flattening first
#   "normalized" already through ard_normalize(): our own .label / .kind
#   "long"       somebody's OWN long summary: stat_name + stat and no more
#   "wide"       one row per printed row: the ARD half has nothing to do
#
# The "long" case is the point of the classifier.  A statistician who
# summarised with dplyr has a frame with keys, a statistic name and a
# value, which is everything `ard_spread()` reads -- so the cell template
# language, the row templates and the last-wins digits are all usable with
# no cards anywhere.
.plan_source_kind <- function(x) {
  if (!is.data.frame(x)) return("ard")
  nm <- names(x)
  if (any(c(".label", ".kind") %in% nm)) return("normalized")
  if (all(c("stat_name", "stat") %in% nm)) {
    # cards' own shape still needs flattening: it says so with `context`
    # or with the group pairs.  A frame carrying neither was built by hand.
    if ("context" %in% nm || any(grepl("^group[0-9]+$", nm))) return("ard")
    return("long")
  }
  "wide"
}


# -- the constructor ---------------------------------------------------------

#' A deferred, last-wins plan for an ARD (SPIKE)
#'
#' `ard_plan()` starts a plan.  The ARD is **held, not transformed**: every
#' `plan_*()` verb adds a declaration, and nothing runs until [apply_plan()].
#'
#' The rule is **last wins** --- a later layer overwrites what an earlier one
#' said about the same key --- so "set everything, then fix one variable" is a
#' two-line edit:
#'
#' ```r
#' plan_digits(2) |> plan_digits(AGE = 0)
#' ```
#'
#' This is tfrmt's `frmt_structure` rule.  It is deliberately **different**
#' from [ard_spread()]'s `cells`, which picks by specificity and ignores
#' order; the spike exists to find out which is nicer to write.
#'
#' @param ard What to build the table from.  Three things are accepted, and
#'   the plan works out which it is from the columns:
#'   * a **cards/cardx ARD**, which is flattened and spread;
#'   * a frame already through [ard_normalize()] --- how a report that has
#'     to reach in with dplyr gets back into a plan;
#'   * **any long frame of statistics**: keys, a `stat_name` and a `stat`,
#'     built with dplyr and no cards anywhere.  `plan_normalize()` is then
#'     skipped and `plan_cells()` does the work.
#'
#'   A frame that is already the table is refused, with a message saying to
#'   use [as_rtftables()] instead.
#'
#' @return An object of class `ard_plan`.
#'
#' @section Lifecycle:
#' **Spike.**  A prototype for #474, kept in one deletable file.  It may be
#' withdrawn wholesale; see the header of `R/ard-plan-spike.R`.
#'
#' @examples
#' if (requireNamespace("cards", quietly = TRUE)) {
#'   ard <- cards::ard_stack(
#'     cards::ADSL, .by = ARM,
#'     cards::ard_continuous(variables = c(AGE, BMIBL)),
#'     cards::ard_categorical(variables = SEX))
#'
#'   ard_plan(ard) |>
#'     plan_spread(cols = "ARM", rows = c(group = "variable")) |>
#'     plan_cells(continuous  = c("Mean (SD)" = "{mean} ({sd})"),
#'                categorical = "{n} ({p:%})") |>
#'     plan_digits(2) |>
#'     plan_digits(AGE = 0, SEX = 1) |>
#'     apply_plan()
#' }
#' @seealso [apply_plan()], [ard_normalize()], [ard_spread()]
#' @export
ard_plan <- function(ard) {
  if (is.null(ard)) .ard_stop("`ard` is required.")
  kind <- .plan_source_kind(ard)
  # A wide frame is refused HERE rather than three stages later, which is
  # the one thing deferring is supposed to buy.
  if (identical(kind, "wide")) {
    .ard_stop(paste0(
      "This looks like a table, not something to build one from: it has ",
      "no `stat_name`/`stat`\n  to read and none of the columns ",
      "ard_normalize() adds.\n",
      "  A plan converts an ARD, or any long frame of statistics, into a ",
      "table.\n",
      "  A frame that is already the table goes straight to ",
      "as_rtftables().\n",
      "  Columns seen: ", paste(utils::head(names(ard), 8L),
                                collapse = ", ")))
  }
  structure(list(ard = ard, kind = kind,
                 normalized = !identical(kind, "ard"), layers = list(),
                 cache = new.env(parent = emptyenv())),
            class = "ard_plan")
}

# A seam layer carries expressions, and "exprs, env" says nothing.  Show
# what it will do: the columns it writes, or the condition it keeps.
.plan_expr_text <- function(l) {
  ex <- l$fields$exprs
  if (!length(ex)) return("(nothing)")
  nms <- names(ex) %||% rep("", length(ex))
  txt <- vapply(seq_along(ex), function(i) {
    one <- paste(deparse(ex[[i]]), collapse = " ")
    if (nzchar(nms[i])) paste0(nms[i], " = ", one) else one
  }, "")
  txt <- ifelse(nchar(txt) > 46L, paste0(substr(txt, 1L, 43L), "..."), txt)
  paste(txt, collapse = "; ")
}

#' @export
print.ard_plan <- function(x, ...) {
  cat("<ard_plan>  ",
      switch(x$kind %||% "ard",
             ard        = "from an ARD, ",
             normalized = "from a normalized frame, ",
             long       = "from a long frame of statistics, ",
             ""),
      length(x$layers), " layer",
      if (length(x$layers) == 1L) "" else "s",
      "  ->  ",
      if (identical(.plan_reach(x), "pages")) "RTF pages"
      else "table data.frame",
      "\n", sep = "")
  if (!length(x$layers)) {
    cat("  (empty -- add plan_spread() / plan_cells() / plan_digits())\n")
    return(invisible(x))
  }
  # In declaration order, because that is the order that decides the result.
  for (i in seq_along(x$layers)) {
    l <- x$layers[[i]]
    what <- if (!is.null(l$fields$exprs)) .plan_expr_text(l) else
      paste(names(l$fields), collapse = ", ")
    cat(sprintf("  %2d. %-10s %s\n", i, l$kind, what))
  }
  # What `cols` / `rows` / `label` may name.  Normalising is the cheap
  # half, so this costs a flatten and answers the question that otherwise
  # needs a run.
  # The column names change three times -- ard_normalize() builds them,
  # ard_spread() replaces them, and folding the stub replaces them again
  # -- and different verbs name different ones.  Show every list that is
  # to hand.  Normalising is computed if it has not been; spreading is
  # not, because it is the expensive half.
  say <- function(lbl, d, note) {
    if (is.null(d)) return(invisible(NULL))
    cat("  ", lbl, "\n", sep = "")
    cat(strwrap(paste(setdiff(names(d), ".overall"), collapse = ", "),
                width = 72, prefix = "      "), sep = "\n")
    if (nzchar(note)) cat("      ", note, "\n", sep = "")
  }
  say("after normalize  -- for cols / rows / label / plan_mutate:",
      .plan_norm(x), "")
  say("after spread     -- for plan_stub / plan_group / plan_hide / sort:",
      x$cache$table, "")
  say("as printed       -- for plan_styles / plan_style(widths) / header:",
      x$cache$printed, "")
  if (is.null(x$cache$table)) {
    cat("  after spread     -- not computed yet; run it once and this",
        " print fills in\n", sep = "")
  }
  cat(if (identical(.plan_reach(x), "pages"))
        "  rtf_tables(doc, x) renders it"
      else "  apply_plan(x) returns it",
      ";  apply_plan(x, \"args\") shows the call\n", sep = "")
  invisible(x)
}


# -- the verbs ---------------------------------------------------------------

#' Declare the ARD conversion, one layer at a time (SPIKE)
#'
#' Each verb adds a layer to an [ard_plan()].  **A later layer wins.**  The
#' arguments are the ones [ard_normalize()] and [ard_spread()] already take,
#' so the layering is the only new idea.
#'
#' @param plan An [ard_plan()].
#' @param ... For `plan_cells()`, exactly what `ard_spread(cells = )` takes:
#'   one bare entry, or entries named by variable, `context`, kind
#'   (`continuous` / `categorical`) or `default`.  For `plan_digits()` and
#'   `plan_round()`, the same keys with a number (or `"r"` / `"sas"`) as the
#'   value; one unnamed value sets the plan-wide default.
#' @param vars,label,indent,group_summary For `plan_stub()`: the row keys to
#'   fold into one stub column and how, as [stub_cols()] takes them.
#' @param before For `plan_stub()`: `FALSE` (default) folds the stub inside
#'   [as_rtftables()], after grouping and pagination have had their say.
#'   `TRUE` folds it first, with [stub_cols()], which is what
#'   `plan_styles()` needs --- only then can a condition see the rows that
#'   will be printed.  The two do **not** always give the same table.
#' @param col,mode,collapse For `plan_group()`: `as_rtftables()`'s
#'   `group_col`, `group_by` and `collapse_repeats`.
#' @param desc For `plan_sort()`: `as_rtftables()`'s `sort_desc`.
#' @param where,first,last,counted For `plan_blanks()`: `as_rtftables()`'s
#'   `blank_rows`, `blank_row_first`, `blank_row_end` and
#'   `count_blank_rows`.
#' @param max_rows,split,break_before,by,min_group_rows,cont_label For
#'   `plan_pages()`: the row budget and what a page break may cut ---
#'   `as_rtftables()`'s `max_rows`, `split`, `split_rows`, `page_by`,
#'   `min_group_rows` and `cont_label`.
#' @param border,widths For `plan_style()`: the border set and the relative
#'   column widths (`col_rel_width`).  Anything else [rtftable()]
#'   understands goes through `...`.
#' @param header For `plan_header()`: what [set_col_header()] should be
#'   given --- an [rtf_col_header()] object, or a **function** of the
#'   `plan_n()` values, which is how a denominator reaches the header
#'   without being written down a second time.
#' @param values For `plan_header()`: passed to [set_col_header()] as
#'   `values =`, for a header whose cells carry `{token}` placeholders.
#' @inheritParams ard_normalize
#' @inheritParams ard_spread
#'
#' @return The plan, with one more layer.
#'
#' @section Lifecycle:
#' **Spike.**  See [ard_plan()].
#'
#' @name plan_verbs
#' @seealso [ard_plan()], [apply_plan()]
NULL

#' @rdname plan_verbs
#' @export
plan_normalize <- function(plan, keys = NULL, hierarchy = NULL,
                           overall = NULL, drop_contexts = NULL,
                           drop_key_variables = NULL) {
  .plan_layer(plan, "normalize",
              list(keys = keys, hierarchy = hierarchy, overall = overall,
                   drop_contexts = drop_contexts,
                   drop_key_variables = drop_key_variables))
}

#' @rdname plan_verbs
#' @export
plan_spread <- function(plan, cols = NULL, rows = NULL, label = NULL,
                        stats = NULL, value = NULL, levels = NULL,
                        labels = NULL, sort = NULL, sep = NULL,
                        sort_stat = NULL, na = NULL, notes = NULL,
                        spec = NULL) {
  .plan_layer(plan, "spread",
              list(cols = cols, rows = rows, label = label, stats = stats,
                   value = value, levels = levels, labels = labels,
                   sort = sort, sep = sep, sort_stat = sort_stat,
                   na = na, notes = notes, spec = spec))
}

# `...` is captured as-is: the values are templates, and a template may be a
# formula carrying its own environment, so nothing is evaluated or coerced.
.plan_keyed <- function(plan, kind, dots) {
  nms <- names(dots) %||% rep("", length(dots))
  if (any(!nzchar(nms))) {
    if (sum(!nzchar(nms)) > 1L) {
      .ard_stop(paste0("Give at most one unnamed value (the plan-wide ",
                       "default); name the rest."))
    }
    nms[!nzchar(nms)] <- "default"
  }
  names(dots) <- nms
  # Last wins ACROSS layers, which is the rule.  Twice in ONE call is not a
  # layering, it is a typo: `list(a = x, a = y)` silently keeps the first.
  if (anyDuplicated(nms)) {
    .ard_stop(paste0(
      "the same key is given twice in one call: ",
      paste(sQuote(unique(nms[duplicated(nms)])), collapse = ", "), ".\n",
      "  A later LAYER wins, so make it a second call if that is what you ",
      "meant."))
  }
  .plan_layer(plan, kind, dots)
}

#' @rdname plan_verbs
#' @export
plan_cells <- function(plan, ...) .plan_keyed(plan, "cells", list(...))

#' @rdname plan_verbs
#' @export
plan_digits <- function(plan, ...) .plan_keyed(plan, "digits", list(...))

#' @rdname plan_verbs
#' @export
plan_round <- function(plan, ...) .plan_keyed(plan, "round", list(...))


# -- the display half --------------------------------------------------------
#
#  Everything above turns an ARD into a table data.frame.  These five turn
#  that into RTF pages, and they are declarations for the same reason: the
#  column header has to agree with the columns, and the denominators in it
#  have to agree with the percentages underneath.  Resolving them together
#  is the only way that agreement is structural rather than remembered.
#
#  Each verb carries the arguments of the function it stands for, unchanged,
#  and `apply_plan(stage = "pages")` calls them in the order a report is
#  built:
#
#      plan_n()       numbers read out of the ARD, by name
#      plan_mutate()  a column derived on the LONG frame, before spreading
#      plan_filter()  rows dropped on the long frame
#      plan_derive()  a column derived FROM the finished table
#      plan_fmt()     fmt_numeric()      on the table data.frame
#      plan_stub()    stub_cols()        fold the row keys into one stub
#      plan_styles()  cell_styles        bold / colour / align, by condition
#      plan_group()   which column groups the rows, and how it shows
#      plan_hide()    columns that do their work without being printed
#      plan_sort()    the printed order
#      plan_blanks()  where the blank rows go
#      plan_pages()   the row budget and what a page break may cut
#      plan_style()   borders, widths, alignment
#      plan_header()  set_col_header()   with the plan_n() values in scope
#      plan_after()   set_decimal_split() / paginate_cols() / anything else

#' @rdname plan_verbs
#' @export
plan_n <- function(plan, ...) .plan_keyed(plan, "n", list(...))

# The two seams, written the way dplyr writes them.  `...` is captured
# UNEVALUATED and evaluated later against whichever frame the stage has,
# so `plan_mutate(variable = if_else(stat_name == "N", ...))` reads like
# the mutate() it replaces and nothing has to leave the plan to do it.
.plan_exprs <- function(plan, kind, dots, env) {
  .plan_layer(plan, kind, list(exprs = dots, env = env))
}

#' @rdname plan_verbs
#' @export
plan_mutate <- function(plan, ...) {
  .plan_exprs(plan, "mutate", as.list(substitute(list(...)))[-1L],
              parent.frame())
}

#' @rdname plan_verbs
#' @export
plan_filter <- function(plan, ...) {
  .plan_exprs(plan, "filter", as.list(substitute(list(...)))[-1L],
              parent.frame())
}

# Applied in DECLARATION order, so a filter and a mutate that depend on
# each other behave the way they were written.
.plan_reshape <- function(plan, d, kinds) {
  for (l in plan$layers) {
    if (!l$kind %in% kinds) next
    ex <- l$fields$exprs
    if (!length(ex)) next
    if (identical(l$kind, "filter")) {
      for (e in ex) {
        keep <- eval(e, d, l$fields$env)
        d <- d[!is.na(keep) & keep, , drop = FALSE]
      }
    } else {
      nms <- names(ex) %||% rep("", length(ex))
      for (i in seq_along(ex)) {
        if (!nzchar(nms[i])) {
          f <- tryCatch(eval(ex[[i]], l$fields$env),
                        error = function(e) NULL)
          if (!is.function(f)) {
            .ard_stop(paste0(
              "plan_", l$kind, "(): an unnamed argument has to be a ",
              "function of the frame.\n  Name it to make it a column: ",
              "plan_", l$kind, "(<name> = <expression>)."))
          }
          d <- f(d)
        } else {
          d[[nms[i]]] <- eval(ex[[i]], d, l$fields$env)
        }
      }
    }
  }
  d
}

# A column the table can only know about once it exists -- the page key in
# the solicited-AE report is `row_grp1 %in% <two categories>`, which nothing
# upstream can state.  Declared here, the seam between the table and the
# display stays open without the plan having to be broken in half.
#' @rdname plan_verbs
#' @export
plan_derive <- function(plan, ...) {
  .plan_exprs(plan, "derive", as.list(substitute(list(...)))[-1L],
              parent.frame())
}

#' @rdname plan_verbs
#' @export
plan_fmt <- function(plan, ...) .plan_layer(plan, "fmt", list(...))

# WHERE the stub is folded changes the answer, so it is a setting rather
# than a detail.  as_rtftables() folds it inside its own resolution, after
# grouping and pagination have had their say, and that is what a report
# grouped by a carrier column needs.  Folding it FIRST, with stub_cols(),
# is what plan_styles() needs, because only then can a condition see the
# rows that will be printed.
#' @rdname plan_verbs
#' @export
plan_stub <- function(plan, vars = NULL, label = NULL, indent = NULL,
                      group_summary = NULL, before = FALSE) {
  .plan_layer(plan, "stub",
              list(vars = vars, label = label, indent = indent,
                   group_summary = group_summary, before = before))
}

# SAS's `call define(_col_, 'style', ...)` inside a `compute` block: a cell
# looks at its own row and decides how it is printed.  The condition is a
# one-sided formula over the FINISHED table -- the same columns
# as_rtftables() is about to see -- and its value is used directly, so a
# logical attribute takes a logical vector and a valued one takes the
# value (or NA for "leave the column default alone").
#
#     plan_styles(bold  = ~ is.na(term),
#                 color = list(Placebo = ~ ifelse(n > 50, "#CC0000", NA)))
#
# A bare formula covers the whole row; a NAMED list scopes it to columns,
# by name rather than by position -- which is the difference between this
# and `col_spec`, whose numbers move when the stub does.
#' @rdname plan_verbs
#' @export
plan_styles <- function(plan, ...) .plan_keyed(plan, "styles", list(...))

# Build the `cell_styles` list rtftable() wants: one element per row, each
# NULL or a named list of per-column vectors.
# Folding the stub destroys the row keys, and a condition wants them: SAS's
# `compute` sees every variable in the report, including the ones it does
# not print.  `stub_cols()` leaves `rtf_stub_src` behind -- output row to
# source row, NA for a heading row -- so they can be put back, which also
# makes `is.na(<a row key>)` the test for "this is a heading row".
.plan_style_frame <- function(tbl, pre) {
  env <- as.list(tbl)
  if (is.null(pre) || identical(nrow(pre), nrow(tbl))) {
    for (nm in setdiff(names(pre), names(env))) env[[nm]] <- pre[[nm]]
    return(env)
  }
  src <- attr(tbl, "rtf_stub_src", exact = TRUE)
  if (is.null(src) || length(src) != nrow(tbl)) return(env)
  for (nm in setdiff(names(pre), names(env))) {
    env[[nm]] <- pre[[nm]][src]
  }
  env
}

.plan_cell_styles <- function(spec, tbl, pre = NULL) {
  n <- nrow(tbl); m <- ncol(tbl); nms <- names(tbl)
  frame <- .plan_style_frame(tbl, pre)
  out <- vector("list", n)
  for (att in names(spec)) {
    v <- spec[[att]]
    parts <- if (inherits(v, "formula")) list(v) else as.list(v)
    keys  <- if (inherits(v, "formula")) NA_character_ else
      (names(parts) %||% rep(NA_character_, length(parts)))
    for (i in seq_along(parts)) {
      f <- parts[[i]]
      if (!inherits(f, "formula") || length(f) != 2L) {
        .ard_stop(paste0(
          "plan_styles(", att, "): each entry is a one-sided formula ",
          "over the table's columns,\n  for example ",
          "`plan_styles(bold = ~ is.na(label))`."))
      }
      cols <- if (is.na(keys[i])) seq_len(m) else match(keys[i], nms)
      if (anyNA(cols)) {
        .ard_stop(paste0(
          "plan_styles(", att, "): no printed column ", sQuote(keys[i]),
          ".\n  Available: ", paste(nms, collapse = ", ")))
      }
      val <- eval(f[[2L]], frame, environment(f))
      if (length(val) == 1L) val <- rep(val, n)
      if (length(val) != n) {
        .ard_stop(paste0(
          "plan_styles(", att, "): the condition gave ", length(val),
          " values for ", n, " rows."))
      }
      for (r in seq_len(n)) {
        if (is.na(val[[r]])) next
        cur <- out[[r]]
        if (is.null(cur)) cur <- list()
        if (is.null(cur[[att]])) cur[[att]] <- rep(val[[r]][NA], m)
        cur[[att]][cols] <- val[[r]]
        out[[r]] <- cur
      }
    }
  }
  out
}

# as_rtftables() takes thirty-three arguments, and being able to pass all
# thirty-three through one verb is not a plan -- it is the same wall with
# a different door.  These six each own ONE concern, the way a ggplot
# layer does, and the resolver assembles the call from them.  The engine
# is still as_rtftables(); what changes is that nothing has to be read
# whole in order to write a little.
#
# The argument names are as_rtftables()'s own, so nothing new is learned.

#' @rdname plan_verbs
#' @export
plan_group <- function(plan, col = NULL, mode = NULL, collapse = NULL) {
  .plan_layer(plan, "group",
              list(group_col = col, group_by = mode,
                   collapse_repeats = collapse))
}

# A column can be needed and not wanted: a sort carrier, the key a page
# break reads.  Naming them here says which, instead of `drop_cols` being
# read as "columns I regret".
#' @rdname plan_verbs
#' @export
plan_hide <- function(plan, ...) {
  cols <- unlist(list(...), use.names = FALSE)
  .plan_layer(plan, "hide", list(drop_cols = cols))
}

#' @rdname plan_verbs
#' @export
plan_sort <- function(plan, ..., desc = NULL) {
  cols <- unlist(list(...), use.names = FALSE)
  .plan_layer(plan, "sort", list(sort_by = cols, sort_desc = desc))
}

#' @rdname plan_verbs
#' @export
plan_blanks <- function(plan, where = NULL, first = NULL, last = NULL,
                        counted = NULL) {
  .plan_layer(plan, "blanks",
              list(blank_rows = where, blank_row_first = first,
                   blank_row_end = last, count_blank_rows = counted))
}

#' @rdname plan_verbs
#' @export
plan_pages <- function(plan, max_rows = NULL, split = NULL,
                       break_before = NULL, by = NULL,
                       min_group_rows = NULL, cont_label = NULL) {
  .plan_layer(plan, "pages",
              list(max_rows = max_rows, split = split,
                   split_rows = break_before, page_by = by,
                   min_group_rows = min_group_rows,
                   cont_label = cont_label))
}

# `...` is open on purpose: everything rtftable() understands about how a
# table looks reaches it, without this verb having to list twenty-eight
# arguments in order to be complete.
#' @rdname plan_verbs
#' @export
plan_style <- function(plan, border = NULL, widths = NULL, ...) {
  .plan_layer(plan, "style",
              c(list(border = border, col_rel_width = widths),
                list(...)))
}

# The header is a VALUE, not a set of fields: `rtf_col_header()` builds a
# whole object and there is nothing useful to merge field-wise.  A function
# is accepted too, and is called with the resolved plan_n() list, which is
# how "(N=86)" reaches the header without being written down twice.
#' @rdname plan_verbs
#' @export
plan_header <- function(plan, header = NULL, values = NULL) {
  .plan_layer(plan, "header", list(header = header, values = values))
}

# Steps that run on the finished pages.  They are functions rather than
# fields because that is what they are -- set_decimal_split() and
# paginate_cols() take the object and give it back -- and a plan that
# pretended otherwise would need a field per argument of each.
#' @rdname plan_verbs
#' @export
plan_after <- function(plan, ...) {
  fs <- list(...)
  bad <- !vapply(fs, is.function, logical(1L))
  if (any(bad)) {
    .ard_stop(paste0(
      "plan_after() takes functions of the pages, one per step -- for ",
      "example\n    plan_after(\\(x) set_decimal_split(x, cols = 3:5))"))
  }
  .plan_layer(plan, "after", list(steps = fs))
}


# -- the resolver ------------------------------------------------------------

#' Run a plan, or look inside it (SPIKE)
#'
#' Resolves an [ard_plan()]'s layers and runs the conversion.  `stage` stops
#' it early, so the same one pass answers "what does this do" and "what did it
#' do" --- there is no second code path that could disagree with the first.
#'
#' @param plan An [ard_plan()].
#' @param stage How far to go.  `"auto"`, the default, is **as far as the
#'   plan declares**: a plan that says nothing about the display stops at
#'   the table `data.frame`; one that carries `plan_rtf()`, `plan_header()`,
#'   `plan_styles()` or `plan_after()` goes on to the RTF pages.  You rarely
#'   need this function at all --- [rtf_tables()] takes a plan directly ---
#'   and naming a stage is for looking inside: `"normalize"`, `"args"`,
#'   `"table"`, `"pages"`.
#'
#'   The named stages: `"table"` returns the table
#'   `data.frame`, the same object [ard_spread()] returns.  `"normalize"`
#'   returns the long frame [ard_normalize()] returns, which is where you
#'   reach in with dplyr if you have to.  `"args"` returns the resolved
#'   argument lists without running anything --- the call the plan amounts
#'   to.  `"pages"` goes all the way: [fmt_numeric()], [stub_cols()],
#'   [as_rtftables()], [set_col_header()] and whatever `plan_after()`
#'   declared, giving the RTF pages.
#'
#' @return A data frame; for `stage = "args"` a list of two argument lists;
#'   for `stage = "pages"` what [as_rtftables()] and the steps after it
#'   return.
#'
#' @section Lifecycle:
#' **Spike.**  See [ard_plan()].
#'
#' @examples
#' if (requireNamespace("cards", quietly = TRUE)) {
#'   p <- ard_plan(cards::ard_stack(
#'          cards::ADSL, .by = ARM,
#'          cards::ard_continuous(variables = AGE))) |>
#'     plan_spread(cols = "ARM", rows = c(group = "variable")) |>
#'     plan_cells(continuous = c("Mean (SD)" = "{mean} ({sd})")) |>
#'     plan_digits(2) |>
#'     plan_digits(AGE = 0)
#'
#'   str(apply_plan(p, "args")$spread$cells)   # AGE won
#'   apply_plan(p)
#' }
#' @seealso [ard_plan()], [plan_verbs]
#' @export
apply_plan <- function(plan, stage = c("auto", "normalize", "args",
                                       "table", "pages")) {
  if (!inherits(plan, "ard_plan")) {
    .ard_stop("Expected an ard_plan; start from ard_plan(ard).")
  }
  stage <- match.arg(stage)
  if (identical(stage, "auto")) stage <- .plan_reach(plan)

  # 1. the normalize half, last wins
  n_args <- .plan_merge(.plan_of(plan, "normalize"))
  if (isTRUE(plan$normalized)) {
    if (length(n_args)) {
      .ard_stop(paste0(
        "This plan started from a frame that does not need flattening (",
        plan$kind %||% "normalized", "), so plan_normalize()\n",
        "  has nothing to do.  Drop it, or start the plan from the ARD:\n    ",
        paste(names(n_args), collapse = ", ")))
    }
    x <- plan$ard
  } else {
    x <- do.call(ard_normalize, c(list(ard = plan$ard), n_args))
  }
  # the long-frame seam, inside the plan
  x <- .plan_reshape(plan, x, c("mutate", "filter"))
  .plan_remember(plan, "norm", x)
  if (identical(stage, "normalize")) return(x)

  # 2. the spread half, last wins; `levels` and `labels` merge per name so a
  #    later layer can add one variable without restating the others
  s_args <- .plan_merge(.plan_of(plan, "spread"), deep = c("levels", "labels"))

  # 3. the cells map, last wins over the keys the caller used
  cells <- .plan_merge(.plan_of(plan, "cells"))
  dig   <- .plan_merge(.plan_of(plan, "digits"))
  rnd   <- .plan_merge(.plan_of(plan, "round"))

  # 4. Expand to one entry per variable.  This is the step that NEEDS the ARD,
  #    and the reason the plan holds it: `plan_digits(AGE = 0)` cannot be
  #    turned into a template until we know which entry AGE would have got.
  if (length(cells)) {
    #  a) one entry per analysis variable, where a variable-specific layer
    #     can win.  A frame with no `variable` column -- somebody's own long
    #     summary -- has nothing to expand, and (b) still applies.
    vars <- if ("variable" %in% names(x)) .ard_first_seen(x$variable) else
      character(0)
    vars <- vars[!is.na(vars)]
    filled <- list()
    for (v in vars) {
      sel <- x$variable == v
      ctx <- .ard_first_seen(x$context[sel])[1L]
      knd <- .ard_first_seen(x$.kind[sel])[1L]
      entry <- .plan_lookup_raw(cells, v, ctx, knd)
      if (is.null(entry)) next
      entry <- .plan_fill_entry(entry, .plan_pick(dig, v, ctx, knd))
      .plan_check_open(entry, v)
      filled[[v]] <- entry
    }
    #  b) every key the caller actually wrote, filled with the digits THAT
    #     key resolves to.  Without this a plan-wide `plan_digits()` reached
    #     nothing at all when there were no variables to expand.
    for (k in names(cells)) {
      if (k %in% names(filled)) next
      cells[[k]] <- .plan_fill_entry(cells[[k]],
                                     .plan_pick(dig, k, NA_character_,
                                                NA_character_))
      # Only worth complaining about a key that can still be REACHED.  With
      # variables present every lookup goes through (a), so a `categorical`
      # entry that every variable has overridden is dead and its unanswered
      # tokens are nobody's problem.
      if (!length(vars)) .plan_check_open(cells[[k]], k)
    }
    for (k in names(filled)) cells[[k]] <- filled[[k]]
    s_args$cells <- cells

    #  c) a digits key that reached nothing is a typo, and silence is how a
    #     typo survives review.  "AST = 3" on a frame whose analysis
    #     variable column is called PARAM does nothing, and says so.
    reached <- c("default", names(cells), vars)
    miss <- setdiff(names(dig), reached)
    if (length(miss)) {
      .ard_stop(paste0(
        "plan_digits() names ", paste(sQuote(miss), collapse = ", "),
        ", which matched nothing.\n",
        "  A key is an analysis variable, a context, a kind ",
        "(continuous / categorical)\n  or \"default\".  ",
        "Variables here: ",
        if (length(vars)) paste(utils::head(vars, 8L), collapse = ", ")
        else "(none -- this frame has no `variable` column, so per-variable",
        if (length(vars)) "" else " keys cannot be used)"))
    }
  }

  # 5. one rounding family for the run; a per-variable one would have to reach
  #    into `ard_spread()`, which a spike does not do.  Named keys are read so
  #    the shape is there, and a disagreement is reported rather than guessed.
  if (length(rnd)) {
    u <- unique(unlist(rnd, use.names = FALSE))
    if (length(u) > 1L) {
      .ard_stop(paste0(
        "plan_round() was given more than one family (",
        paste(sQuote(u), collapse = ", "), ").\n",
        "  The spike resolves ONE rounding family per run; declare it once, ",
        "or use\n  a spec file, which carries `round` per variable."))
    }
    s_args$round <- u
  }

  if (identical(stage, "args")) {
    return(list(normalize = n_args, spread = s_args))
  }
  tbl <- do.call(ard_spread, c(list(x = x), s_args))
  # the table-side seam: a column the table can only know once it exists
  tbl <- .plan_reshape(plan, tbl, "derive")
  .plan_remember(plan, "table", tbl)
  if (identical(stage, "table")) return(tbl)

  .plan_to_pages(plan, tbl)
}


# The display half, in the order a report is built.  Each step calls the
# function it stands for with the arguments the caller declared, so nothing
# here reimplements as_rtftables() or anything around it.
# One call built from the layers.  Each verb owns its own arguments, so
# this is a merge, not a translation -- the names never change.
.plan_rtf_args <- function(plan) {
  out <- list()
  for (kind in c("group", "hide", "sort", "blanks", "pages", "style")) {
    for (nm in names(l <- .plan_merge(.plan_of(plan, kind)))) {
      out[[nm]] <- l[[nm]]
    }
  }
  # A table built from an ARD is a plain data.frame: there is no adapter
  # metadata to read, and every report was saying so by hand.
  if (is.null(out$read_meta)) out$read_meta <- FALSE
  out
}

.plan_to_pages <- function(plan, tbl) {
  #  the numbers the header needs, read out of the ARD the plan is holding.
  #  A function is called with the ARD; anything else is taken as it is.
  nvals <- lapply(.plan_merge(.plan_of(plan, "n")), function(v)
    if (is.function(v)) v(plan$ard) else v)

  fmt <- .plan_merge(.plan_of(plan, "fmt"))
  if (length(fmt)) tbl <- do.call(fmt_numeric, c(list(data = tbl), fmt))

  .plan_remember(plan, "table", tbl)

  stub <- .plan_merge(.plan_of(plan, "stub"))
  before <- isTRUE(stub$before)
  stub$before <- NULL
  pre <- tbl
  if (length(stub) && before) {
    # only what the caller named: stub_cols() has its own defaults, and
    # handing it NULL is not the same as leaving it alone
    a <- stub[!vapply(stub, is.null, logical(1L))]
    tbl <- do.call(stub_cols, c(list(data = tbl), a))
  }

  rtf <- .plan_rtf_args(plan)
  if (length(stub) && !before) {
    rtf$stub_vars         <- stub$vars
    rtf$stub_label        <- stub$label
    rtf$stub_indent       <- stub$indent
    rtf$stub_group_summary <- stub$group_summary
    rtf <- rtf[!vapply(rtf, is.null, logical(1L))]
  }
  st  <- .plan_merge(.plan_of(plan, "styles"))
  if (length(st)) {
    # The styles are built against the rows the plan can SEE.  Folding the
    # stub inside as_rtftables() adds heading rows the plan never saw, and
    # rtftable() would reject the length with nothing to say about why.
    if (!is.null(rtf$stub_vars)) {
      .ard_stop(paste0(
        "plan_styles() needs plan_stub(before = TRUE).\n",
        "  Folded inside as_rtftables(), the stub adds group heading rows ",
        "the conditions\n  never saw, so a style would land on the ",
        "wrong row."))
    }
    if (!is.null(rtf$cell_styles)) {
      .ard_stop("plan_styles() and a cell_styles = of your own: use one.")
    }
    rtf$cell_styles <- .plan_cell_styles(st, tbl, pre)
  }
  out <- do.call(as_rtftables, c(list(x = tbl), rtf))

  hdr <- .plan_merge(.plan_of(plan, "header"))
  if (!is.null(hdr$header)) {
    #  a function of the plan_n() values, so "(N=86)" is written once and
    #  the number comes from the ARD rather than from memory
    # The header may need the finished table -- "a spanner over columns 3 to
    # the last" is a fact about the table, not about the ARD -- so a function
    # of two arguments is given (values, table).
    h <- if (!is.function(hdr$header)) hdr$header
         else if (length(formals(hdr$header)) >= 2L) hdr$header(nvals, tbl)
         else hdr$header(nvals)
    args <- list(x = out, h)
    if (!is.null(hdr$values)) args$values <- hdr$values
    out <- do.call(set_col_header, args)
  }

  for (l in .plan_of(plan, "after")) {
    for (f in l$steps) out <- f(out)
  }
  # the names plan_styles() / plan_style(widths = ) / plan_header() use:
  # read off the finished page rather than predicted
  first <- if (inherits(out, "rtftable")) out else out[[1L]]
  if (!is.null(first$data)) .plan_remember(plan, "printed", first$data)
  out
}

# The cells map, looked up WITHOUT parsing: `.ard_lookup_cells()` returns the
# parsed entry, and a plan has to rewrite the template text before the engine
# ever sees it.
.plan_lookup_raw <- function(cells, variable, context, kind) {
  if (!is.list(cells) || !any(nzchar(names(cells) %||% ""))) return(NULL)
  .plan_pick(cells, variable, context, kind)
}

# Last-wins across layers has already happened; what is left is specificity,
# the same order `.ard_lookup_cells()` uses, so a key set for a variable beats
# one set for its kind.
.plan_pick <- function(map, variable, context, kind) {
  if (!length(map)) return(NULL)
  keys <- variable
  if (!is.null(context) && !is.na(context)) {
    keys <- c(keys, .ard_context_aliases(context))
  }
  if (!is.null(kind) && !is.na(kind)) {
    keys <- c(keys, .ard_context_aliases(kind))
  }
  for (k in c(unique(keys), "default")) {
    if (!is.na(k) && k %in% names(map)) return(map[[k]])
  }
  NULL
}


# ============================================================================
#  plan_template()
# ============================================================================

# One `verb(` line, its arguments indented under it, and the pipe on the
# close.  Written here rather than pasted six times below.
.plan_call <- function(verb, args, op, last = FALSE) {
  if (!length(args)) {
    return(paste0("  rtfreporter::", verb, "()", if (last) "" else
                  paste0(" ", op)))
  }
  args[-length(args)] <- paste0(args[-length(args)], ",")
  c(paste0("  rtfreporter::", verb, "("),
    paste0("    ", args),
    paste0("  )", if (last) "" else paste0(" ", op)))
}

#' Write the plan for you (SPIKE)
#'
#' The counterpart of [ard_template()] for the deferred form: reads an ARD
#' and prints a runnable [ard_plan()] pipeline, filled in with the keys,
#' hierarchy, contexts and statistics it actually found.
#'
#' It writes **both halves** --- the ARD to the table, and the table to the
#' RTF pages --- because a plan that stops at the table is a plan that made
#' you look up `stub_vars` and the header somewhere else.  The display half
#' is a starting point and is meant to be edited: only `plan_stub()` is
#' derivable from the ARD, and the rest are display decisions nothing can
#' guess.
#'
#' @inheritParams ard_template
#'
#' @return The generated code, as a character vector, invisibly.
#'
#' @section Lifecycle:
#' **Spike.**  See [ard_plan()].
#'
#' @examples
#' if (requireNamespace("cards", quietly = TRUE)) {
#'   ard <- cards::ard_stack(
#'     cards::ADSL, .by = ARM,
#'     cards::ard_continuous(variables = AGE),
#'     cards::ard_categorical(variables = SEX))
#'   plan_template(ard, cols = "ARM")
#' }
#' @seealso [ard_plan()], [apply_plan()], [ard_template()]
#' @export
plan_template <- function(ard, cols = NULL, hierarchy = character(),
                          spec = FALSE, file = NULL, pipe = NULL) {
  op <- .ard_pipe_op(pipe)
  f  <- .ard_template_facts(ard, cols, hierarchy)
  q <- f$q; vecq <- f$vecq; tok <- f$tok

  L <- c(
    .ard_bar("", "="),
    "#  generated by rtfreporter::plan_template()  --  SPIKE",
    "#",
    paste0("#  keys       : ", paste(f$keys, collapse = ", ")),
    paste0("#  variables  : ",
           if (length(f$vars)) paste(utils::head(f$vars, 12), collapse = ", ")
           else "(none -- the rows come from `hierarchy`)"),
    paste0("#  kinds      : ", paste(f$kinds, collapse = ", ")),
    "#",
    "#  Nothing runs until apply_plan(); print(p) says how far it will go.",
    "#  A later layer wins, so tune one variable by ADDING a line.",
    if (f$guessed)
      "#  NOTE: `cols` was not given; the first key is used.  Check it."
    else NULL,
    if (length(f$rest))
      paste0("#  NOTE: keys outside `cols` became row keys: ",
             paste(f$rest, collapse = ", "))
    else NULL,
    if (f$overall)
      "#  NOTE: an overall sentinel is present; edit the `overall` label."
    else NULL,
    .ard_bar("", "="),
    "",
    if (identical(op, "%>%"))
      c("library(magrittr)   # for %>% ; dplyr re-exports it too", "")
    else NULL)

  # -- 1. the ARD half ---------------------------------------------------
  norm <- c(
    if (length(hierarchy)) paste0("hierarchy = ", vecq(hierarchy)) else NULL,
    if (f$overall) "overall   = \"Any event\"" else NULL)

  spread <- c(
    paste0("cols = ", vecq(f$cols)),
    if (length(f$row_parts))
      paste0("rows = c(", paste(f$row_parts, collapse = ", "), ")")
    else NULL,
    if (length(hierarchy) > 1L)
      paste0("label = c(label = ", q(utils::tail(hierarchy, 1L)), ")")
    else NULL)

  L <- c(L,
         .ard_bar("1. the ARD half"),
         paste0("p <- rtfreporter::ard_plan(ard) ", op))
  if (length(norm)) L <- c(L, .plan_call("plan_normalize", norm, op))
  L <- c(L, .plan_call("plan_spread", spread, op))

  if (isTRUE(spec)) {
    L <- c(L, .plan_call("plan_spread",
                         "spec = rtfreporter::read_ard_spec(\"ard-spec.xlsx\")",
                         op))
  } else {
    # already indented and comma-ed: a `c(` entry spans several lines, so
    # it cannot go through .plan_call(), which commas every argument.
    L <- c(L, "  rtfreporter::plan_cells(", .plan_cell_lines(f),
           paste0("  ) ", op))
  }

  # -- 2. the display half -----------------------------------------------
  L <- c(L, "", .ard_bar("2. the display half -- edit this"))
  n_ok <- !is.null(tryCatch(ard_pull(ard, cols = f$cols),
                            error = function(e) NULL))
  if (n_ok && length(f$cols) == 1L) {
    L <- c(L, .plan_call(
      "plan_n",
      paste0("arm = function(ard) rtfreporter::ard_pull(ard, cols = ",
             vecq(f$cols), ")"), op))
  }
  # `plan_stub()` rather than plan_rtf(stub_vars = ): the plan then sees the
  # rows that will be printed, which plan_styles() needs.
  L <- c(L, .plan_call("plan_stub",
                       c(paste0("vars  = ", vecq(f$stub)),
                         "label = \"row_label\""), op))
  # One concern per line.  Delete the ones this report does not want;
  # none of them has to be read in order to change another.
  L <- c(L,
         paste0("  rtfreporter::plan_group(mode = \"indent\") ", op),
         paste0("  rtfreporter::plan_blanks(\"between_groups\") ", op),
         paste0("  rtfreporter::plan_pages(max_rows = 22, ",
                "split = \"group_safe\") ", op),
         paste0("  rtfreporter::plan_style(border = \"tfl\") ", op))
  if (n_ok && length(f$cols) == 1L) {
    L <- c(L, .plan_call(
      "plan_header",
      paste0("function(n) c(\"Characteristic\", ",
             "paste0(names(n$arm), \"\\nN = \", ",
             "as.integer(n$arm)))"), op, last = TRUE))
  } else {
    # fixed: the base pipe is " |>", and "|" is alternation in a regex
    L[length(L)] <- sub(paste0(" ", op), "", L[length(L)], fixed = TRUE)
    L <- c(L,
           "# Several column keys: as_rtftables(header_sep = ) rebuilds the",
           "# spanning header from the \"____\" in the names, so plan_header()",
           "# is only needed for text the data does not carry.")
  }

  L <- c(L, "",
         "# print(p) says whether this gives a table or RTF pages",
         "pages <- rtfreporter::apply_plan(p)",
         "#  ... or hand it straight to a document:",
         "#  doc <- rtf_document() |> rtf_section(...) |> rtf_tables(p)")

  code <- L[!vapply(L, is.null, logical(1))]
  cat(paste(code, collapse = "\n"), "\n")
  if (!is.null(file)) writeLines(code, file)
  invisible(code)
}

# The `cells` entries, one per kind, with the digits the ARD already knows.
.plan_cell_lines <- function(f) {
  blocks <- list()
  for (kd in f$kinds) {
    d <- f$d[!is.na(f$d$.kind) & f$d$.kind == kd, , drop = FALSE]
    have <- .ard_first_seen(d$stat_name)
    if (identical(kd, "continuous")) {
      cand <- c(
        "n"              = f$tok("N"),
        "Mean (SD)"      = paste0(f$tok("mean"), " (", f$tok("sd"), ")"),
        "Median"         = f$tok("median"),
        "Q1, Q3"         = paste0(f$tok("p25"), ", ", f$tok("p75")),
        "Min, Max"       = paste0(f$tok("min"), ", ", f$tok("max")),
        "CV (%)"         = f$tok("cv"),
        "Geometric Mean" = f$tok("geom_mean"),
        "95% CI"         = paste0(f$tok("conf.low"), ", ",
                                  f$tok("conf.high")))
      keep <- vapply(cand, function(t)
        all(gsub("[{}]", "", gsub(":[^}]*", "", .ard_tokens(t))) %in% have),
        logical(1))
      cand <- cand[keep]
      if (!length(cand)) next
      rows <- .ard_aligned(names(cand), unname(cand), "      ")
      rows[-length(rows)] <- paste0(rows[-length(rows)], ",")
      blocks[[length(blocks) + 1L]] <-
        c(paste0("    ", kd, " = c("), rows, "    )")
    } else {
      tpl <- if (all(c("n", "p") %in% have))
        paste0(f$tok("n"), " ({p:.1f%})")
      else if ("n" %in% have) f$tok("n") else paste0("{", have[1], "}")
      blocks[[length(blocks) + 1L]] <-
        paste0("    ", kd, " = ", encodeString(tpl, quote = "\""))
    }
  }
  # a comma after every block but the last
  for (i in seq_along(blocks)) {
    if (i < length(blocks)) {
      b <- blocks[[i]]
      b[length(b)] <- paste0(b[length(b)], ",")
      blocks[[i]] <- b
    }
  }
  unlist(blocks, use.names = FALSE)
}

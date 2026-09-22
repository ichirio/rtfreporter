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
  "plan_digits", "plan_round", "apply_plan")


# -- layer plumbing ----------------------------------------------------------

# A plan is the ARD, untouched, plus an ordered list of declarations.  The ARD
# is held rather than transformed, so a plan can be printed, inspected and
# re-pointed at a new data cut without anything having been computed yet.
.plan_layer <- function(plan, kind, fields) {
  if (!inherits(plan, "ard_plan")) {
    .ard_stop("Expected an ard_plan; pipe from ard_plan(ard).")
  }
  fields <- fields[!vapply(fields, is.null, logical(1L))]
  if (length(fields)) {
    plan$layers[[length(plan$layers) + 1L]] <-
      list(kind = kind, fields = fields)
  }
  plan
}

# The layers of one kind, in the order they were declared.
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
                 normalized = !identical(kind, "ard"), layers = list()),
            class = "ard_plan")
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
      ", nothing computed yet\n", sep = "")
  if (!length(x$layers)) {
    cat("  (empty -- add plan_spread() / plan_cells() / plan_digits())\n")
    return(invisible(x))
  }
  # In declaration order, because that is the order that decides the result.
  for (i in seq_along(x$layers)) {
    l <- x$layers[[i]]
    nms <- names(l$fields)
    cat(sprintf("  %2d. %-10s %s\n", i, l$kind,
                paste(nms, collapse = ", ")))
  }
  cat("  apply_plan(x) to run it; apply_plan(x, \"args\") to see the call\n")
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


# -- the resolver ------------------------------------------------------------

#' Run a plan, or look inside it (SPIKE)
#'
#' Resolves an [ard_plan()]'s layers and runs the conversion.  `stage` stops
#' it early, so the same one pass answers "what does this do" and "what did it
#' do" --- there is no second code path that could disagree with the first.
#'
#' @param plan An [ard_plan()].
#' @param stage How far to go.  `"table"` (default) returns the table
#'   `data.frame`, the same object [ard_spread()] returns.  `"normalize"`
#'   returns the long frame [ard_normalize()] returns, which is where you
#'   reach in with dplyr if you have to.  `"args"` returns the resolved
#'   argument lists without running anything --- the call the plan amounts to.
#'
#' @return A data frame, or for `stage = "args"` a list of two argument lists.
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
apply_plan <- function(plan, stage = c("table", "normalize", "args")) {
  if (!inherits(plan, "ard_plan")) {
    .ard_stop("Expected an ard_plan; start from ard_plan(ard).")
  }
  stage <- match.arg(stage)

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
  do.call(ard_spread, c(list(x = x), s_args))
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

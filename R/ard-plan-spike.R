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
#      ard |>
#        ard_normalize() |>                              # run, not declared
#        rtf_plan(cols = "TRT01P", rows = c(group = "variable")) |>
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
#  2. THE ROLES ARE SAID ONCE, WHERE THE DATA IS.  `rtf_plan()` takes the
#     NORMALIZED frame and, like `ggplot(data, aes(x, y))`, the columns
#     that play a part in the table: `cols` across, `rows` down, `label`
#     for the row identity.  Every other verb reads them from there, so
#     the stub, the group carrier and the header's denominator are not
#     restated and cannot disagree.
#
#     Normalising is NOT deferred.  Nothing in the plan feeds it, nothing
#     overrides it later, and deferring it meant `rtf_plan()` had to GUESS
#     whether what it was handed still needed flattening -- a guess that
#     silently skipped the step for every report once already.  Run it,
#     look at it, then name its columns.
#
#     Beyond that the keys are the ones you already know: `plan_cells()`
#     takes exactly what `ard_spread(cells = )` takes, and the display
#     verbs take `as_rtftables()`'s own arguments.
#
#  ---------------------------------------------------------------------------
#  Why it resolves to arguments rather than re-implementing anything
#  ---------------------------------------------------------------------------
#
#  `apply_plan()` builds `ard_spread()`'s argument list and calls it.  Nothing about the conversion is duplicated, so the
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
  "rtf_plan", "plan_cells", "plan_levels", "plan_labels",
  "plan_digits", "plan_fmt",
  "plan_stub", "plan_cell_style",
  "plan_paginate_group", "plan_row_group",
  "plan_hide", "plan_sort", "plan_blanks",
  "plan_paginate_rows",
  "plan_style", "plan_col_header", "plan_paginate_cols",
  "plan_titles", "plan_footnotes",
  "plan_listing",
  "plan_after", "apply_plan", "plan_template", "as_table_spec")


# -- layer plumbing ----------------------------------------------------------

# A plan is the ARD, untouched, plus an ordered list of declarations.  The ARD
# is held rather than transformed, so a plan can be printed, inspected and
# re-pointed at a new data cut without anything having been computed yet.
# The call the author actually wrote.  A plan is ONE statement, so when
# the resolver fails at the end there is no line number to go on -- and
# that was the one clear advantage the immediate form still had.  Walk out
# to the nearest frame whose function is a plan verb and keep its call.
.plan_site <- function() {
  for (i in seq_len(sys.nframe())) {
    cl <- sys.call(-i)
    if (is.null(cl) || !is.call(cl)) next
    f <- cl[[1L]]
    nm <- if (is.name(f)) as.character(f)
          else if (is.call(f) && identical(as.character(f[[1L]]), "::"))
            as.character(f[[3L]]) else ""
    if (startsWith(nm, "plan_") || identical(nm, "rtf_plan")) return(cl)
  }
  NULL
}

# Which statements built the call that has just failed.  With one layer
# per kind this is exact; with several it is the short list to look at.
.plan_blame <- function(plan, kinds) {
  cl <- Filter(Negate(is.null),
               lapply(plan$layers, function(l)
                 if (l$kind %in% kinds) l$site else NULL))
  if (!length(cl)) return("")
  # `|>` is syntax: sys.call() sees the desugared nesting, so the first
  # argument is the whole pipeline so far.  Drop it and show the verb with
  # its OWN arguments, which is what the author wrote on that line.
  txt <- vapply(cl, function(z) {
    if (length(z) > 1L) z <- z[-2L]
    paste(deparse(z), collapse = " ")
  }, "")
  txt <- ifelse(nchar(txt) > 64L, paste0(substr(txt, 1L, 61L), "..."), txt)
  paste0("\n  declared by:\n",
         paste0("    ", txt, collapse = "\n"))
}

# Run a stage and, if it fails, say which statements set it up.
.plan_stage <- function(expr, plan, kinds) {
  withCallingHandlers(expr, error = function(e) {
    .ard_stop(paste0(conditionMessage(e), .plan_blame(plan, kinds)))
  })
}

.plan_layer <- function(plan, kind, fields) {
  if (!inherits(plan, "rtf_plan")) {
    .ard_stop("Expected an rtf_plan; pipe from rtf_plan(ard).")
  }
  fields <- fields[!vapply(fields, is.null, logical(1L))]
  # Recorded even when empty: calling the verb is the declaration, and
  # `plan_rtf()` with nothing in it still means "make pages".
  plan$layers[[length(plan$layers) + 1L]] <-
    list(kind = kind, fields = fields, site = .plan_site())
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
  if (any(kinds %in% c("group", "hide", "blanks", "pages", "colpages",
                       "style", "header", "styles", "after",
                       "titles", "footnotes", "listing"))) "pages"
  else "table"
}

# The long frame the roles are named against: the data as handed in,
# with the ARD column names it was told about.  There is nothing to
# compute -- flattening and any dplyr happen before the plan -- so
# print() can always answer what the roles may name.
.plan_long <- function(plan) {
  if (!is.null(plan$cache$long)) return(plan$cache$long)
  if (is.null(plan$data)) return(NULL)
  v <- tryCatch(.plan_prepare(plan, plan$data),
                error = function(e) plan$data)
  if (!is.null(v)) plan$cache$long <- v
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
# How many digits a STATISTIC gets.  `cands` are the declarations that
# could apply, most specific first; the first that names the statistic
# wins, and a declaration that names nothing (one number for every
# token) wins at its own level.  So `plan_digits(c(mean = 2, sd = 3))`
# as the house rule and `plan_digits(AGE = c(mean = 1, sd = 2))` for
# one variable is two lines, and `plan_digits(AGE = c(mean = 1))`
# leaves that variable's `sd` to the house rule.
.plan_digits_for <- function(cands, stat) {
  for (d in cands) {
    if (is.null(d) || !length(d)) next
    nm <- names(d)
    if (is.null(nm)) return(d[[1L]])
    i <- match(stat, nm)
    if (!is.na(i)) return(d[[i]])
    j <- which(!nzchar(nm))
    if (length(j)) return(d[[j[1L]]])
  }
  NULL
}

.plan_fill_one <- function(tpl, cands) {
  if (!is.character(tpl) || !length(tpl)) return(tpl)
  for (i in seq_along(tpl)) {
    for (tok in unique(.ard_tokens(tpl[i]))) {
      pt <- .ard_token_parts(tok)
      if (identical(pt$spec, "") || identical(pt$spec, "%")) {
        d <- .plan_digits_for(cands, pt$name)
        if (is.null(d) || all(is.na(d))) next
        # a number is decimals; "4s" is significant digits, which is
        # the token grammar's own distinction (`.4f` / `.4s`) rather
        # than a second argument to learn
        sg <- is.character(d) && grepl("^[0-9]+s$", trimws(d))
        fmt <- if (sg) paste0(".", sub("s$", "", trimws(d)), "s")
               else paste0(".", as.integer(d), "f")
        new <- paste0("{", pt$name, ":", fmt,
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
.plan_fill_entry <- function(entry, cands) {
  if (is.null(entry) || !length(cands)) return(entry)
  if (is.character(entry)) return(.plan_fill_one(entry, cands))
  if (is.list(entry)) {
    for (i in seq_along(entry)) {
      if (is.character(entry[[i]])) {
        entry[[i]] <- .plan_fill_one(entry[[i]], cands)
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
.plan_source_kind <- function(x, roles = NULL) {
  if (!is.data.frame(x)) return("ard")
  nm <- names(x)
  # A frame that SAYS which of its columns are the statistic and its
  # value is a long frame, whatever those columns are called.  Judging
  # it by the cards names alone would call it a finished table.
  for (k in c("variable", "stat_name", "stat")) {
    src <- roles[[k]]
    if (!is.null(src) && as.character(src)[1L] %in% nm) nm <- c(nm, k)
  }
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

#' A deferred, last-wins plan for a table (SPIKE)
#'
#' `rtf_plan()` starts a plan, and takes the **roles**: which column goes
#' across the table, which go down it, which carries the row identity.
#' This is `ggplot(data, aes(x, y))` --- the names must be columns of the
#' data you hand it, so you can check them by looking.  Every `plan_*()`
#' verb after it adds a declaration, and nothing runs until [apply_plan()].
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
#' @param data What the table is built from.  A plan does **not** flatten:
#'   `cols` / `rows` / `label` name columns of what you hand it, so
#'   flatten first and look at the result.
#'   * a frame through [ard_normalize()] --- the ordinary case;
#'   * **any long frame of statistics**: keys, a `stat_name` and a `stat`,
#'     built with dplyr and no cards anywhere.  [plan_cells()] does the
#'     work;
#'   * a frame that is already the table, for the display half on its own;
#'   * a frame of subject records, with [plan_listing()].
#'
#'   A raw cards ARD is refused, with the line to write.
#' @param cols The key that goes **across** the table, as
#'   `ard_spread(cols = )` takes it: one or more columns, optionally
#'   renamed `c(new = old)`.
#' @param rows The keys that go **down** it, likewise.  Left out, the
#'   analysis variable is used (as `group`).
#' @param label The column carrying the **row identity** --- the text in
#'   the label column.  `".label"` by default (what [ard_normalize()]
#'   builds), `NA` for a table that has none, or a guarded template.
#' @param variable,stat_name,stat Which of **your** columns play the
#'   three parts [ard_spread()] reads by name: the analysis variable
#'   (what [plan_cells()] and [plan_digits()] key on), which statistic
#'   a row is, and what it is worth.  A frame from \pkg{cards} already
#'   calls them that and needs none of them; a summary somebody built
#'   with dplyr says so here, once, instead of being asked again by
#'   every verb.

#' @param stats `"cells"` (default) fills a template per cell; `"rows"`
#'   makes each statistic a row of its own.
#' @param sep Separator pasted between multiple `cols` keys.
#' @param value,na,notes The remaining [ard_spread()] options,
#'   unchanged: which of `stat` / `stat_fmt` a `{x}` reads, what fills a
#'   cell no template could, and whether to report what was not used.
#' @param spec An [table_spec()] definition, or the path to a workbook as
#'   [read_table_spec()] reads it (a workbook defining several reports must
#'   be narrowed first, with `read_table_spec(path, output_id = )`).  It is
#'   the plan's **first layers**: the roles from its `tables` sheet (a role
#'   given here wins), then everything else --- levels, labels, cells,
#'   rounding, and the `layout` / `columns` / `style` of the pages --- as
#'   if the matching verbs had been written first.  A verb written after
#'   `rtf_plan()` therefore still wins, which is how one report departs
#'   from the study's workbook in a line of code.
#'
#' @return An object of class `rtf_plan`.
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
#'   ard |>
#'     ard_normalize() |>
#'     rtf_plan(cols = "ARM", rows = c(group = "variable")) |>
#'     plan_cells(continuous  = c("Mean (SD)" = "{mean} ({sd})"),
#'                categorical = "{n} ({p:%})") |>
#'     plan_digits(2) |>
#'     plan_digits(AGE = 0, SEX = 1) |>
#'     apply_plan()
#' }
#' @seealso [apply_plan()], [ard_normalize()], [ard_spread()]
#' @export
rtf_plan <- function(data = NULL, cols = NULL, rows = NULL,
                     label = NULL,
                     variable = NULL, stat_name = NULL, stat = NULL,
                     stats = NULL, sep = NULL, value = NULL,
                     na = NULL, spec = NULL, notes = NULL) {
  roles <- list(cols = cols, rows = rows, label = label,
                variable = variable, stat_name = stat_name,
                stat = stat, stats = stats, sep = sep, value = value,
                na = na, spec = spec, notes = notes)
  roles <- roles[!vapply(roles, is.null, logical(1L))]
  # A definition workbook can say the roles too (its `tables` sheet).  Read
  # it HERE, so they are the plan's roles like any other -- checked against
  # the data below, printed, and read by every verb -- and a role written in
  # the call still wins.
  sp <- NULL
  if (!is.null(spec)) {
    sp <- .ard_spec_scope(if (is.character(spec)) read_table_spec(spec)
                          else table_spec(spec))
    roles$spec <- NULL
    sa <- .ard_spec_table_args(sp)
    for (r in c("cols", "rows", "label", "stats", "sep", "value", "na",
                "sort_stat")) {
      if (is.null(roles[[r]]) && !is.null(sa[[r]])) roles[[r]] <- sa[[r]]
    }
  }
  if (is.null(data)) {
    .ard_stop(paste0(
      "`data` is required.  The roles name its columns, and that ",
      "is what lets a typo\n  be caught here rather than three stages later.\n",
      "  A house style that serves every study is an ordinary ",
      "function:\n",
      "    my_dm <- function(d) rtf_plan(d, cols = ...) |> ",
      "plan_cells(...)"))
  }
  kind <- .plan_source_kind(data, roles)
  if (identical(kind, "ard")) {
    .ard_stop(paste0(
      "rtf_plan() takes the NORMALIZED frame, not a raw ARD, so that ",
      "`cols` / `rows` / `label`\n  name columns you can see.  ",
      "Flatten it first:\n",
      "    ard |>\n",
      "      ard_normalize() |>\n",
      "      rtf_plan(cols = ...)\n",
      "  ard_normalize() is what adds `.label`, `.kind` and `.depth`, ",
      "and names the\n  hierarchy levels -- which is what `rows` ",
      "and `label` then point at."))
  }
  .plan_check_roles(data, roles)
  p <- structure(list(data = data, kind = kind, roles = roles,
                      layers = list(),
                      cache = new.env(parent = emptyenv())),
                 class = "rtf_plan")
  if (!is.null(sp)) p <- .plan_from_spec(p, sp)
  p
}

# ---------------------------------------------------------------------------
#  A definition workbook as the plan's first layers
# ---------------------------------------------------------------------------
#
#  Every sheet becomes the layer its verb would have written, in the order a
#  report is built, so nothing downstream knows a workbook was involved:
#  the same resolver, the same checks, and a verb written afterwards wins.

.plan_from_spec <- function(p, sp) {
  sa <- .ard_spec_table_args(sp)
  if (!is.null(sa[["sort"]])) p <- .plan_layer(p, "sort", list(sort = sa[["sort"]]))
  if (!is.null(sa[["rounding"]])) {
    p <- .plan_layer(p, "round", list(rounding = sa[["rounding"]]))
  }
  lv <- .ard_spec_levels(sp)
  if (length(lv)) p <- plan_levels(p, lv)
  lb <- .ard_spec_labels(sp)
  if (length(lb)) p <- plan_labels(p, lb)
  cm <- .ard_spec_cells(sp)
  if (length(cm)) p <- do.call(plan_cells, c(list(p), cm))

  # rows with no template: the display format of one statistic, for a
  # table that lays the statistics out as rows
  f <- sp$cells[is.na(sp$cells$template), , drop = FALSE]
  if (nrow(f)) {
    if (!identical(p$roles$stats, "rows")) {
      .ard_stop(paste0(
        "The `cells` sheet has rows with no `template` -- a statistic's ",
        "display format --
  but the table is not `stats = rows`.  ",
        "Give those rows a template, or set
  `tables$stats` to `rows`."))
    }
    if (any(!is.na(f$variable) | !is.na(f$context))) {
      .ard_stop(paste0(
        "A `cells` row with no template formats one statistic across the ",
        "table;
  leave its `variable` and `context` blank."))
    }
    if (any(is.na(f$row))) {
      .ard_stop(paste0("A `cells` row with no template needs `row`: the ",
                       "statistic it formats, as the label column prints it."))
    }
    fm <- lapply(seq_len(nrow(f)), function(i) {
      if (!is.na(f$signif[i])) list(signif = as.integer(f$signif[i]))
      else list(digits = as.integer(f$digits[i]))
    })
    p <- plan_fmt(p, by = .plan_label_name(p),
                  formats = stats::setNames(fm, f$row))
  }

  lay <- if (nrow(sp$layout)) .ard_spec_typed(sp$layout[1L, ], "layout")
         else list()
  pick <- function(prefix, map) {
    out <- list()
    for (k in names(map)) {
      v <- lay[[paste0(prefix, k)]]
      if (!is.null(v)) out[[map[[k]]]] <- v
    }
    out
  }
  a <- pick("stub_", c(vars = "vars", into = "into", indent = "indent",
                       summary = "group_summary", before = "before"))
  if (length(a)) p <- do.call(plan_stub, c(list(p), a))
  if (isTRUE(lay[["group_page"]])) {
    p <- plan_paginate_group(p, col = lay[["group_col"]],
                             show = !identical(lay[["group_show"]], FALSE))
  }
  a <- pick("group_", c(mode = "mode", collapse = "collapse"))
  if (length(a) || (!is.null(lay[["group_col"]]) && !isTRUE(lay[["group_page"]]))) {
    a$col <- lay[["group_col"]]
    p <- do.call(plan_row_group, c(list(p), a))
  }
  a <- pick("blank_", c(where = "where", first = "first", last = "last",
                        counted = "counted"))
  if (length(a)) p <- do.call(plan_blanks, c(list(p), a))
  a <- pick("pages_", c(max_rows = "max_rows", split = "split", by = "by",
                        min_group_rows = "min_group_rows",
                        cont_label = "cont_label"))
  if (length(a)) p <- do.call(plan_paginate_rows, c(list(p), a))
  a <- pick("colpages_", c(every = "every", at = "at", carry = "carry",
                           order = "order"))
  if (length(a)) p <- do.call(plan_paginate_cols, c(list(p), a))

  st <- if (nrow(sp$style)) .ard_spec_typed(sp$style[1L, ], "style")
        else list()
  cl <- sp$columns
  ct <- lapply(seq_len(nrow(cl)), function(i)
    .ard_spec_typed(cl[i, , drop = FALSE], "columns"))
  flag <- function(k) vapply(ct, function(r) isTRUE(r[[k]]), NA)
  if (any(flag("row_title"))) st$row_title <- cl$column[flag("row_title")]
  if (length(st)) p <- do.call(plan_style, c(list(p), st))
  if (any(flag("hide"))) p <- plan_hide(p, cl$column[flag("hide")])
  hd <- sp$col_header
  if (nrow(hd)) {
    cells <- lapply(seq_len(nrow(hd)), function(i)
      .ard_spec_typed(hd[i, , drop = FALSE], "col_header"))
    txt <- vapply(cells, function(r) r[["text"]] %||% "", "")
    p <- plan_col_header(p, header = structure(
      list(cells = cells, n = any(grepl("{n", txt, fixed = TRUE))),
      class = "plan_spec_col_header"))
  }
  w <- vapply(ct, function(r) r[["width"]] %||% NA_real_, NA_real_)
  if (any(!is.na(w)) || any(flag("decimal_split"))) {
    p <- .plan_layer(p, "columns", list(
      widths  = stats::setNames(w[!is.na(w)], cl$column[!is.na(w)]),
      decimal = cl$column[flag("decimal_split")]))
  }
  p
}

# The `col_header` sheet, resolved against the page it heads: `cols` are
# names / `.values` / positions / `KEY = value`, `span` makes one cell,
# one per column, or one per value of a key.  What comes back is what a
# hand-written rtf_col_header() would have been, so the tokens and the
# rest of the header machinery see nothing new.
.plan_spec_col_header <- function(sh, page_names, spread, plan) {
  keys <- unname(as.character(unlist(plan$roles$cols)))
  sep <- plan$roles$sep %||% "____"
  # each spread column's key values: from the data where it can be
  # rebuilt, by splitting the name otherwise
  d <- plan$data
  kv <- list()
  if (length(keys) && all(keys %in% names(d))) {
    combo <- unique(as.data.frame(lapply(d[keys], as.character),
                                  stringsAsFactors = FALSE))
    combo <- combo[stats::complete.cases(combo), , drop = FALSE]
    nm <- do.call(paste, c(unname(as.list(combo)), sep = sep))
    for (i in seq_along(nm)) kv[[nm[i]]] <- unlist(combo[i, ], use.names = FALSE)
  }
  key_of <- function(col, key) {
    k <- match(key, keys)
    if (is.na(k)) {
      .ard_stop(sprintf(paste0("`col_header`: %s is not a column key; ",
                               "the keys are %s."), sQuote(key),
                        paste(sQuote(keys), collapse = ", ")))
    }
    v <- kv[[col]] %||% strsplit(col, sep, fixed = TRUE)[[1L]]
    if (k <= length(v)) v[[k]] else NA_character_
  }
  n <- length(page_names)
  sel <- function(x) {
    out <- integer()
    for (it in .ard_spec_split(x)) {
      m <- regmatches(it, regexec("^(.+?)\\s*=\\s*(.+)$", it))[[1L]]
      r <- regmatches(it, regexec("^([0-9]+)\\s*:\\s*([0-9]+|last)$", it))[[1L]]
      if (identical(it, ".values")) {
        out <- c(out, match(spread, page_names))
      } else if (length(r)) {
        to <- if (identical(r[3L], "last")) n else as.integer(r[3L])
        out <- c(out, seq(as.integer(r[2L]), to))
      } else if (grepl("^[0-9]+$", it)) {
        out <- c(out, as.integer(it))
      } else if (length(m) && !it %in% page_names) {
        hit <- spread[vapply(spread, function(cc)
          identical(key_of(cc, trimws(m[2L])), trimws(m[3L])), NA)]
        out <- c(out, match(hit, page_names))
      } else if (it %in% page_names) {
        out <- c(out, match(it, page_names))
      } else {
        .ard_stop(sprintf(paste0("`col_header`: no column %s on the page.",
                                 "\n  Columns: %s"), sQuote(it),
                          paste(page_names, collapse = ", ")))
      }
    }
    out <- sort(unique(out[!is.na(out)]))
    if (any(out < 1L | out > n)) {
      .ard_stop(sprintf("`col_header`: position outside 1..%d in %s.", n,
                        sQuote(x)))
    }
    out
  }
  units <- list()
  for (cl in sh$cells) {
    pos <- sel(cl[["cols"]])
    if (!length(pos)) next
    sp <- cl[["span"]]
    groups <- if (is.null(sp)) list(pos)
      else if (identical(sp, "each")) as.list(pos)
      else {
        v <- vapply(page_names[pos], function(cc) key_of(cc, sp), "")
        unname(split(pos, factor(v, levels = unique(v))))
      }
    for (g in groups) {
      units[[length(units) + 1L]] <- list(
        line = cl[["line"]], pos = range(g), text = cl[["text"]] %||% "",
        align = cl[["align"]], bold = cl[["bold"]],
        top = cl[["border_top"]], bottom = cl[["border_bottom"]])
    }
  }
  lines <- sort(unique(vapply(units, `[[`, 1L, "line")))
  rows <- lapply(lines, function(ln) {
    u <- Filter(function(x) identical(x$line, ln), units)
    u <- u[order(vapply(u, function(x) x$pos[1L], 1L))]
    plain <- all(vapply(u, function(x) x$pos[1L] == x$pos[2L] &&
      is.null(x$align) && is.null(x$bold) && is.null(x$top) &&
      is.null(x$bottom), NA))
    if (plain) {
      r <- rep("", n)
      for (x in u) r[x$pos[1L]] <- x$text
      return(r)
    }
    lapply(u, function(x) {
      a <- list(pos = if (x$pos[1L] == x$pos[2L]) x$pos[1L] else x$pos,
                label = x$text)
      if (!is.null(x$align)) a$align <- x$align
      if (!is.null(x$bold)) a$bold <- x$bold
      b <- list(top = x$top, bottom = x$bottom)
      b <- b[!vapply(b, is.null, NA)]
      if (length(b)) a$border <- do.call(rtf_border, b)
      do.call(col_cell, a)
    })
  })
  do.call(rtf_col_header, rows)
}

# `.values` is every spread column; a name is itself.  What a page prints
# is read off the page, so the answer holds however many columns the data
# turned out to have.
.plan_col_names <- function(want, page_names, spread) {
  out <- character()
  for (w in want) {
    out <- c(out, if (identical(w, ".values")) intersect(page_names, spread)
                  else intersect(w, page_names))
  }
  unique(out)
}

.plan_col_widths <- function(pages, widths, spread) {
  one <- function(tb) {
    nm <- names(tb$data)
    w <- rep(NA_real_, length(nm))
    if (".values" %in% names(widths)) w[nm %in% spread] <- widths[[".values"]]
    own <- intersect(names(widths), nm)
    w[match(own, nm)] <- widths[own]
    if (anyNA(w)) {
      .ard_stop(paste0(
        "The `columns` sheet gives widths, but not for: ",
        paste(sQuote(nm[is.na(w)]), collapse = ", "), ".
",
        "  Give every printed column a width (`.values` covers the spread ",
        "columns)."))
    }
    tb$col_rel_width <- w
    tb
  }
  if (inherits(pages, "rtftable")) return(one(pages))
  pages[] <- lapply(pages, one)
  pages
}

# The whole point of naming the roles beside the data is that the names can
# be CHECKED there, so a typo blames rtf_plan() rather than the resolver.
# Only plain strings are checked: a constant (`~ "Worst Post-Baseline"`) and
# a guarded template (`.label %in% x ~ "  {.label}"`) are not column names,
# and `label = NA` says there is no label column at all.
.plan_check_roles <- function(data, roles) {
  if (!is.data.frame(data)) return(invisible(TRUE))
  nm <- names(data)
  for (r in c("cols", "rows", "label", "variable", "stat_name", "stat")) {
    v <- roles[[r]]
    if (is.null(v) || inherits(v, "formula")) next
    v <- if (is.list(v)) v else as.list(v)
    v <- v[vapply(v, is.character, logical(1L))]
    v <- as.character(unlist(v, use.names = FALSE))
    v <- v[!is.na(v) & nzchar(v)]
    miss <- setdiff(v, nm)
    if (length(miss)) {
      .ard_stop(sprintf(paste0(
        "rtf_plan(%s = ): no column %s in the data.\n",
        "  Columns: %s%s\n",
        "  A column you derive has to exist first: dplyr::mutate() ",
        "it before rtf_plan()."),
        r, paste(sQuote(miss), collapse = ", "),
        paste(utils::head(nm, 12L), collapse = ", "),
        if (length(nm) > 12L) ", ..." else ""))
    }
  }
  invisible(TRUE)
}

#' @export
print.rtf_plan <- function(x, ...) {
  cat("<rtf_plan>  ",
      switch(x$kind %||% "normalized",
             normalized = "from a normalized frame, ",
             long       = "from a long frame of statistics, ",
             wide       = "from a table, ",
             ""),
      length(x$layers), " layer",
      if (length(x$layers) == 1L) "" else "s",
      "  ->  ",
      if (identical(.plan_reach(x), "pages")) "RTF pages"
      else "table data.frame",
      "\n", sep = "")
  # The roles first: every other verb is read against them.
  for (r in c("cols", "rows", "label")) {
    v <- x$roles[[r]]
    if (is.null(v)) next
    one <- function(i) {
      z <- v[[i]]
      txt <- if (inherits(z, "formula"))
               paste(deparse(z), collapse = " ")
             else shQuote(as.character(z), type = "cmd")
      nm <- names(v)[i] %||% ""
      if (length(nm) && !is.na(nm) && nzchar(nm))
        paste0(nm, " = ", txt) else txt
    }
    txt <- paste(vapply(seq_along(v), one, ""), collapse = ", ")
    if (nchar(txt) > 52L) txt <- paste0(substr(txt, 1L, 49L), "...")
    cat(sprintf("      %-6s %s\n", r, txt))
  }
  if (!length(x$layers)) {
    cat("  (no layers -- add plan_cells() / plan_digits())\n")
    return(invisible(x))
  }
  # In declaration order, because that is the order that decides the result.
  for (i in seq_along(x$layers)) {
    l <- x$layers[[i]]
    what <- paste(names(l$fields), collapse = ", ")
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
    cat(strwrap(paste(setdiff(names(d), c(".overall", ".key_own")), collapse = ", "),
                width = 72, prefix = "      "), sep = "\n")
    if (nzchar(note)) cat("      ", note, "\n", sep = "")
  }
  say("in                -- what cols / rows / label may name:",
      .plan_long(x), "")
  say("after spread      -- for plan_stub / plan_group / plan_hide:",
      x$cache$table, "")
  say("as printed        -- for plan_cell_style / plan_style / header:",
      x$cache$printed, "")
  if (is.null(x$cache$table)) {
    cat("  after spread      -- not computed yet; run it once and this",
        " print fills in\n", sep = "")
  }
  # What a header cell may say.  The values come from the ARD and the
  # columns from the spread, so neither is visible in the call.
  tk <- tryCatch(.plan_header_tokens(x), error = function(e) list())
  if (length(tk)) {
    cat("  header tokens     -- what a plan_col_header() cell may ",
        "carry:\n", sep = "")
    w <- max(nchar(names(tk)))
    for (k in names(tk)) {
      cat(sprintf("      %-*s  %s\n", w, k, tk[[k]]))
    }
  }
  cat(if (identical(.plan_reach(x), "pages"))
        "  rtf_tables(doc, x) renders it"
      else "  apply_plan(x) returns it",
      ";  apply_plan(x, \"args\") shows the call\n", sep = "")
  invisible(x)
}


# -- the verbs ---------------------------------------------------------------

#' Declare the table, one layer at a time (SPIKE)
#'
#' Each verb adds a layer to an [rtf_plan()].  **A later layer wins.**
#' The roles --- which column goes across, which go down, which carries
#' the row identity --- are said once, on [rtf_plan()]; these verbs are
#' the things a report really does declare twice.  Their arguments are
#' the ones [ard_spread()] and [as_rtftables()] already take, so the
#' layering is the only new idea.
#'
#' @param plan An [rtf_plan()].
#' @param ... For `plan_cells()`, exactly what `ard_spread(cells = )`
#'   takes: one bare entry, or entries named by variable, `context`,
#'   kind (`continuous` / `categorical`) or `default`.
#'
#'   `plan_digits()` takes the same keys, with the digits as the value.
#'   A value is **one number**, for every token in that entry, or a
#'   vector **named by statistic** --- a house rule is rarely one number.
#'   The two keys combine, which is the point of the verb:
#'
#'   ```r
#'   plan_digits(continuous  = c(mean = 2, sd = 3, median = 2),
#'               categorical = c(p = 1)) |>      # the house rule
#'     plan_digits(AGE = c(mean = 1, sd = 2))    # AGE only
#'   ```
#'
#'   A value is the **decimals**, or `"4s"` for **4 significant digits** ---
#'   the token grammar's own distinction (`{mean:.4f}` / `{mean:.4s}`), not a
#'   second argument to learn:
#'
#'   ```r
#'   plan_digits(continuous = c(mean = "4s", sd = "5s", n = 0))
#'   ```
#'
#'   A statistic the narrower entry says nothing about falls through to
#'   the wider one, so AGE's `median` stays at 2 above.  Digits only
#'   reach a token that left the question open (`{mean}`, or `{p:\%}`):
#'   one that answered it (`{mean:.2f}`) keeps its answer, so a template
#'   meant to be tuned is written open.
#'
#'   For `plan_levels()` and `plan_labels()`, one entry per column or
#'   analysis variable: an order (`AGEGR1 = c("<65", "65-74")`), or
#'   the text values are printed as (`AGE = "Age (years)"`).  Both
#'   merge one **key** at a time, so a later layer adds a variable
#'   without restating the rest --- which is the whole reason these
#'   two are layers and the roles are not.  A `plan_labels()` entry
#'   whose value is itself a named vector applies to that **column**
#'   only: a shift table's `"0"` is `"Grade 0"` down the side and
#'   `"Baseline 0"` across the top.
#' @param vars,into,indent,group_summary For `plan_stub()`: the row keys to
#'   fold into one stub column and how, as [stub_cols()] takes them.
#'   `into` is the NAME the folded column gets (`stub_cols(label = )`), which
#'   is a different thing from `rtf_plan(label = )` --- the column whose
#'   VALUES are the row text.  `vars` is derived when left out.
#' @param before For `plan_stub()`: `FALSE` (default) folds the stub inside
#'   [as_rtftables()], after grouping and pagination have had their say.
#'   `TRUE` folds it first, with [stub_cols()], which is what
#'   `plan_cell_style()` needs --- only then can a condition see the rows that
#'   will be printed.  The two do **not** always give the same table.
#' @param show `FALSE` also hides the column the verb names: the
#'   grouping carrier for `plan_row_group()`, the `by` key for `plan_paginate_rows()`, the
#'   sort keys for `plan_sort()`.  A column can be **needed and not
#'   wanted** --- a carrier that groups the rows, the key a page break
#'   reads --- and the verb that needs it is the one place that knows,
#'   so it says so there instead of the name being written again in a
#'   `plan_hide()`.  Names that are not columns (a statistic, `".depth"`)
#'   are ignored rather than refused.
#' @param mode,collapse For `plan_row_group()`: what a run of rows sharing a
#'   value is, and how the repeat shows.  `mode` is `as_rtftables(group_by = )`,
#'   which is how a group BOUNDARY is found --- `"value"` (each run of equal
#'   values), `"indent`" (a row starts a group when its cell is not indented),
#'   `"filled"` (when its cell is not empty), or `"auto"`.  `collapse` is
#'   `collapse_repeats`: a repeated value printed once and then blank.
#'
#'   `mode = "indent"` **reads** indentation to find the boundary;
#'   `plan_stub(indent = )` **writes** it.  They are not the same knob, and
#'   a stub written with `indent` is exactly what that mode then reads.
#' @param col For `plan_paginate_group()`: the column whose value starts a
#'   new page, `as_rtftables()`'s `group_col` with `split = "by_value"`.  Left
#'   out, it is the outermost row key.  The page is **named** after
#'   the value, which is the line `rtf_tables(auto_section = TRUE)`
#'   cuts a section on --- so this verb decides what a section is.
#'   `plan_row_group(col = )` may name it instead when there is no page
#'   break; naming different columns in the two is an error.
#' @param desc For `plan_sort()` over a table that is
#'   already built: `as_rtftables()`'s `sort_desc`.  A plan with an ARD
#'   half writes the direction into the keys instead (`-n`).
#' @param where,first,last,counted For `plan_blanks()`: `as_rtftables()`'s
#'   `blank_rows`, `blank_row_first`, `blank_row_end` and
#'   `count_blank_rows`.
#' @param max_rows,split,break_before,by,min_group_rows,cont_label For
#'   `plan_paginate_rows()`: the row budget and what a page break may cut ---
#'   `as_rtftables()`'s `max_rows`, `split`, `split_rows`, `page_by`,
#'   `min_group_rows` and `cont_label`.  This is the **row** axis; a
#'   value split (the **group** axis) is `plan_paginate_group()`,
#'   and the **column** axis is `plan_paginate_cols()`.
#' @param every For `plan_paginate_cols()`: cut a block every this many
#'   columns, counting only the ones a block does not carry.  This is
#'   `at` without writing down how many columns one study had --- the
#'   plan is deferred, so it counts them when the table exists.  Give one
#'   of `at`, `cols`, `by` or `every`.
#' @param at,carry,col_header,width,allow_span_break,order For
#'   `plan_paginate_cols()`: [paginate_cols()]'s own arguments --- where to
#'   cut (`at`, `cols` or `by`), which columns every block repeats (`carry`),
#'   what the header becomes, how the widths are rescaled, and `order` ---
#'   `paginate_cols(page_order = )`, the order the three axes nest in,
#'   outermost first: `"group"`, `"rows"`, `"cols"`, or the shorthands
#'   `"across"` and `"down"`.
#' @param border,widths For `plan_style()`: the border set and the relative
#'   column widths (`col_rel_width`).  Anything else [rtftable()]
#'   understands goes through `...`.
#' @param type,sep,spacer,spacer_rel_width,blank_row,blank_row_first,align,layout,wrap,record
#'   For `plan_listing()`: [listing_spec()]'s own arguments, unchanged.
#'   `...` there takes the [listing_col()]s.
#' @param pages For `plan_titles()` / `plan_footnotes()`: a list with one
#'   block per page, when the pages do not share a block.  `...` is the rows
#'   of a single block used on every page; give one or the other, never
#'   both, because a three-row title on a three-page table cannot be told
#'   apart from three one-row titles.
#' @param cols For `plan_paginate_cols()`: which columns each block keeps, when
#'   the cut is by name rather than by position.
#' @param header For `plan_col_header()`: the header, built with the same
#'   [rtf_col_header()] as everywhere else --- or a **function** of the
#'   resolved `n` (and, with two arguments, the finished table) when it
#'   has to be computed.  The plan adds two things to a header it is
#'   given, both of which used to need a function:
#'
#'   * `{n:sum}` is that number **totalled over the columns the cell
#'     covers**, so a spanner over one arm's two columns shows that
#'     arm's N and one over all of them shows the study total ---
#'     neither written down.  A cell outside the data (the stub)
#'     totals every column.  A spanner's `{col1}`, `{col2}`, ... are
#'     the levels its columns **agree** on, which is the arm name a
#'     spanning cell wants;
#'   * `{n:<column>}` names **one** of the values, for a cell that has
#'     to say a number belonging to a column it does not sit over ---
#'     `"A={n:Placebo} B={n:Xanomeline High Dose}"`;
#'   * **`print()` lists every token this plan offers, with its
#'     VALUES**, under `header tokens` --- the numbers a `{n}` holds and
#'     the text a `{col1}` prints as --- because they come from the ARD
#'     and from the spread, and neither is visible in the call.  The
#'     `{col...}` ones appear once the table has been built at least
#'     once;
#'   * its cells may carry `{col}` and `{n}` --- the column and its
#'     denominator (or the single one there is).  Several `cols` keys
#'     make a name like "Placebo____Negative", which nobody wants
#'     printed, so `{col}` is the **leaf** and `{col1}`, `{col2}`, ... are
#'     the levels in order; with one key the leaf is the whole name.
#'     The hierarchy itself needs no header --- [as_rtftables()] builds
#'     the spanning rows from the same separator, merging the cells.
#'     What each level READS is [plan_labels()]'s business, since it
#'     recodes the values the name is made of;
#'   * a row **shorter** than the table has its last cell repeated over
#'     the spread columns, whose names are not known until the table
#'     exists.
#'
#'   ```r
#'   plan_col_header(n = TRUE, rtf_col_header(
#'     c("",               "{col}"),
#'     c("Characteristic", "(N={n})")))
#'   ```
#'
#'   A row already the right length, and a cell with no token in it, are
#'   untouched --- so a spanner, a border or a cell that reads the
#'   finished table is written exactly as it always was.
#' @param n For `plan_col_header()`: the population each column describes
#'   --- its analysis set, the number a header prints as `(N=86)`.
#'   `TRUE` reads it from the data, keyed by the same `cols` / `levels`
#'   `rtf_plan()` was given, **at every depth of the keys**: with
#'   `cols = c("TRT", "SEX")` both the arm (`"Placebo"`) and the arm x sex
#'   cell (`"Placebo____F"`) are looked up.  Only a number the ARD
#'   **states as a population size** is read:
#'
#'   1. a **cards sentinel**'s `N` keyed by exactly those keys ---
#'      `..ard_hierarchical_overall..` from
#'      `cards::ard_stack_hierarchical(over_variables = TRUE)`, each arm's
#'      denominator; never its `n`, the subjects with an event that the
#'      "Any" row shows.  Taken only when there is one kind of sentinel;
#'   2. the column variable's **own tabulation** --- the per-arm `n` of
#'      `cards::ard_stack(.by = )`, which [ard_normalize()] keeps as
#'      `.key_own` rows --- where its counts add up to the `N` those rows
#'      state (the population split by arm, not the treatment counted as
#'      an event).  At depth *k* it is `cols[k]` tabulated within
#'      `cols[1..k-1]`: cards tabulates each `.by` variable on its own,
#'      so `ard_stack(.by = c(TRT, SEX))` states the arm but not the arm
#'      x sex cell --- bind `cards::ard_categorical(adsl, by = TRT,
#'      variables = SEX)` to state that;
#'   3. an analysis summary's `N` only where it is a denominator by
#'      construction (a hierarchical summary, a percentage of the
#'      `denominator =` data) or where **two or more different variables
#'      agree on it for every column**.  One variable's `N` is the count
#'      of its **non-missing** values --- the arm size only if nothing is
#'      missing, which the ARD cannot show --- and an `N` per visit or per
#'      parameter is not a column's at all.  [ard_pull()] is not asked.
#'
#'   The study total (`..ard_total_n..`, the one [ard_normalize()]
#'   remembers as it drops that row, or the `N` the column variable's
#'   own tabulation states) is **never put in every column**: it answers
#'   a one-column table and a cell over all the columns.
#'
#'   **Pages split by a group value** ([plan_paginate_group()]: a lab
#'   parameter, a visit) each read their numbers from **their own rows**
#'   of the ARD, then from the rows that belong to no page --- the
#'   subjects with an ALT result are not those with a haemoglobin one.
#'   Put that population in the ARD as the column variable tabulated
#'   within the page key: `cards::ard_categorical(adlb, by = PARAM,
#'   variables = BASEGR)` states each baseline column's N and the
#'   page's total.
#'
#'   What cannot be read is printed as **`NA`**, and one **warning** lists
#'   every such cell with the reason (`only AGE states an N (79)`,
#'   `the analysis variables state different N`, ...).  The table is still
#'   built; a guessed number would look exactly like a right one.
#'   `print()` shows the resolved values and what is not resolved.
#'
#'   In a header cell, `{n}` is the number of **what the cell stands
#'   for**: its column; over a spanner, the level its columns agree on
#'   (the arm over its F and M); over all columns, the total.
#'   `{n1}`, `{n2}`, ... name a depth from any cell --- `"{col2} {n2}"`
#'   under `"{col1} {n1}"`.
#'
#'   To **give the numbers yourself**, pass a vector named by column
#'   key, at any depth --- `c(Placebo = 86, "Placebo____F" = 53, ...)` ---
#'   or a function of the data returning one; a single unnamed number
#'   fills every cell.  After a workbook (`rtf_plan(spec = )`), a later
#'   `plan_col_header(n = ...)` supplies the numbers and keeps the
#'   workbook's header.

#'   A **function** of the data covers what neither can find, and a
#'   **named list** of either supplies several --- and then **each
#'   name is a token**, which is how one header says two numbers with
#'   no function at all: the study total in a spanner and each
#'   column's own underneath it.
#'
#'   ```r
#'   plan_col_header(
#'     n = list(n = TRUE, total = 254),
#'     rtf_col_header(
#'       list(col_cell(1, ""), col_cell(c(2, 4), "All (N={total})")),
#'       c("",               "{col}"),
#'       c("Characteristic", "(N={n})")))
#'   ```
#'
#'   An entry keyed by column fills each column with its own; a single
#'   number fills every cell.  `{n}` is the entry called `n`, or the
#'   only entry when there is one.  The resolved value is also what
#'   `header =` is called with when it is a function.
#' @param rounding For `plan_digits()`: the tie-breaking family for the
#'   run, as `ard_spread(rounding = )` takes it.  Last wins, like every
#'   other layer.
#' @param values For `plan_col_header()`: passed to [set_col_header()] as
#'   `values =`, for a header whose cells carry `{token}` placeholders.
#'
#' @return The plan, with one more layer.
#'
#' @section Lifecycle:
#' **Spike.**  See [rtf_plan()].
#'
#' @name plan_verbs
#' @seealso [rtf_plan()], [apply_plan()]
NULL

# The order values appear in, and the text they appear as.  These are the
# two declarations a report really does make twice -- one order for
# everything, then one variable's own -- so they are layers, merged one
# KEY at a time: a later plan_levels() adds a variable without restating
# the rest.  The roles are said once, on rtf_plan(); these are not roles.
#' @rdname plan_verbs
#' @export
plan_levels <- function(plan, ...) {
  v <- .plan_map(list(...), "plan_levels")
  .plan_layer(plan, "levels", list(levels = v))
}

#' @rdname plan_verbs
#' @export
plan_labels <- function(plan, ...) {
  v <- .plan_map(list(...), "plan_labels")
  .plan_layer(plan, "labels", list(labels = v))
}

# One entry per key -- written out, or handed over whole.  A study that
# keeps its labels in a named vector should not have to take it apart
# to declare it, so a single unnamed argument that is itself named IS
# the map.
.plan_map <- function(v, verb) {
  if (length(v) == 1L && is.null(names(v)) &&
      length(v[[1L]]) && !is.null(names(v[[1L]]))) {
    v <- as.list(v[[1L]])
  }
  .plan_named(v, verb)
  v
}

# Every entry is keyed, because an unnamed one could never be matched.
.plan_named <- function(v, verb) {
  if (!length(v)) return(invisible(TRUE))
  nm <- names(v)
  if (is.null(nm) || any(is.na(nm)) || !all(nzchar(nm))) {
    .ard_stop(paste0(
      verb, "(): every entry is named by the column or variable it ",
      "applies to,\n  and an unnamed one could never be matched: ",
      verb, "(AGEGR1 = c(\"<65\", \"65-74\"))."))
  }
  invisible(TRUE)
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


# `round` used to be a verb of its own, and it never earned one: the whole
# run takes ONE rounding family, so it is a setting of the digits rather
# than a layer beside them.
#' @rdname plan_verbs
#' @export
plan_digits <- function(plan, ..., rounding = NULL) {
  p <- .plan_keyed(plan, "digits", list(...))
  if (is.null(rounding)) p
  else .plan_layer(p, "round", list(rounding = rounding))
}


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
#      (dplyr)        a derived column or a filter is written before
#                     rtf_plan(), in the same sentence
#      plan_fmt()     fmt_numeric()      on the table data.frame
#      plan_stub()    stub_cols()        fold the row keys into one stub
#      plan_cell_style()  cell_styles    bold / colour / align, by condition
#      plan_paginate_group()  one page per value of a column, named
#                     after it -- what auto_section cuts a section on
#      plan_row_group()  how a repeated value looks down the body:
#                     a heading row, an indent, or printed once
#      plan_hide()    columns that do their work without being printed
#      plan_sort()    the printed order
#      plan_blanks()  where the blank rows go
#      plan_paginate_rows()  the row budget and what a break may cut
#      plan_paginate_cols()  the column blocks, and how the three
#                     page axes nest
#      plan_style()   borders, widths, alignment
#      plan_col_header()  set_col_header(), and the denominator `n` it needs
#      plan_titles()  the block ABOVE the table, on each page
#      plan_footnotes()  the block BELOW it
#      plan_after()   set_decimal_split() / paginate_cols() / anything else


#' @rdname plan_verbs
#' @export
plan_fmt <- function(plan, ...) .plan_layer(plan, "fmt", list(...))

# WHERE the stub is folded changes the answer, so it is a setting rather
# than a detail.  as_rtftables() folds it inside its own resolution, after
# grouping and pagination have had their say, and that is what a report
# grouped by a carrier column needs.  Folding it FIRST, with stub_cols(),
# is what plan_cell_style() needs, because only then can a condition see the
# rows that will be printed.
#' @rdname plan_verbs
#' @export
# `vars` is derivable and was being written twice: the row keys are the
# names of rtf_plan(rows = ), the label column is the name of its
# `label = `, and a grouping carrier is not part of the stub.  Left out,
# it is worked out from what has already been declared.
plan_stub <- function(plan, vars = NULL, into = NULL, indent = NULL,
                      group_summary = NULL, before = FALSE) {
  # `into` rather than `label`: rtf_plan(label = ) is the column whose
  # VALUES are the row text, and this is the NAME of the column the
  # row keys are folded into.  One letter of difference is not worth
  # the two of them being confusable.
  .plan_layer(plan, "stub",
              list(vars = vars, label = into, indent = indent,
                   group_summary = group_summary, before = before))
}

# SAS's `call define(_col_, 'style', ...)` inside a `compute` block: a cell
# looks at its own row and decides how it is printed.  The condition is a
# one-sided formula over the FINISHED table -- the same columns
# as_rtftables() is about to see -- and its value is used directly, so a
# logical attribute takes a logical vector and a valued one takes the
# value (or NA for "leave the column default alone").
#
#     plan_cell_style(bold  = ~ is.na(term),
#                 color = list(Placebo = ~ ifelse(n > 50, "#CC0000", NA)))
#
# A bare formula covers the whole row; a NAMED list scopes it to columns,
# by name rather than by position -- which is the difference between this
# and `col_spec`, whose numbers move when the stub does.
#' @rdname plan_verbs
#' @export
plan_cell_style <- function(plan, ...) .plan_keyed(plan, "styles", list(...))

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
          "plan_cell_style(", att, "): each entry is a one-sided formula ",
          "over the table's columns,\n  for example ",
          "`plan_cell_style(bold = ~ is.na(label))`."))
      }
      cols <- if (is.na(keys[i])) seq_len(m) else match(keys[i], nms)
      if (anyNA(cols)) {
        .ard_stop(paste0(
          "plan_cell_style(", att, "): no printed column ", sQuote(keys[i]),
          ".\n  Available: ", paste(nms, collapse = ", ")))
      }
      val <- eval(f[[2L]], frame, environment(f))
      if (length(val) == 1L) val <- rep(val, n)
      if (length(val) != n) {
        .ard_stop(paste0(
          "plan_cell_style(", att, "): the condition gave ", length(val),
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

# `show = FALSE` because the same column being BOTH the grouping carrier
# and one nobody wants printed is not two decisions -- it is the ordinary
# shape of a grouped table, and it was the only place in six reports where
# a column had to be named twice.
#' @rdname plan_verbs
#' @export
# The GROUP axis of pagination: each value of one column becomes its
# own page, and the page is NAMED after it, which is the line
# rtf_tables(auto_section = TRUE) cuts a section on.  It is the
# outermost division there is and has nothing to do with rows -- which
# is why it is not plan_row_group(), whose subject is how repeated
# values look down the body.
#
# `col` may be left out and read off rtf_plan(rows = ), so which column
# to hide is not known here.  Record the answer and let
# .plan_rtf_args() do it once the roles are in hand.
#' @rdname plan_verbs
#' @export
plan_paginate_group <- function(plan, col = NULL, show = TRUE) {
  .plan_layer(plan, "group",
              list(group_col = col, .show = show, .page = TRUE))
}

# What a row GROUP is inside the body -- where one run of equal values
# ends and the next begins -- and whether the repeat is printed.  It
# does NOT make the row headings; folding the keys into one heading
# column, indenting them and adding a summary row is plan_stub().
# In six reports this and the page group were never used together.
#' @rdname plan_verbs
#' @export
plan_row_group <- function(plan, mode = NULL, collapse = NULL,
                           col = NULL) {
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

# ONE sort, because there is only one question: what order are the
# rows in.  Which machinery answers it is not the author's problem --
# a plan with an ARD half sorts before the cells are filled, where the
# statistics are still there to sort ON (`-n`, `.overall`, `.depth`),
# and a plan over a finished table sorts the table.  Two verbs for
# that would be the same duplication the roles just lost.
#' @rdname plan_verbs
#' @export
plan_sort <- function(plan, ..., desc = NULL, show = TRUE) {
  v <- list(...)
  keys <- if (length(v) == 1L && is.logical(v[[1L]])) v[[1L]]
          else unlist(v, use.names = FALSE)
  .plan_layer(plan, "sort",
              list(sort = keys, sort_desc = desc, .show = show))
}

# Is there anything to spread?  A finished table and a listing have no
# statistics, so the ARD-side arguments have nowhere to go.
.plan_ard_half <- function(plan) {
  !identical(plan$kind, "wide") && !length(.plan_of(plan, "listing"))
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
# `show = FALSE` wherever a verb NAMES a column it needs: the page key,
# the grouping carrier, a sort carrier.  A column can be needed and not
# wanted, and the verb that needs it is the one place that knows -- so
# it says so there, rather than the name being written a second time in
# a plan_hide().
plan_paginate_rows <- function(plan, max_rows = NULL, split = NULL,
                               break_before = NULL, by = NULL,
                               min_group_rows = NULL,
                               cont_label = NULL, show = TRUE) {
  .plan_layer(plan, "pages",
              list(max_rows = max_rows, split = split,
                   split_rows = break_before, page_by = by,
                   min_group_rows = min_group_rows,
                   cont_label = cont_label, .show = show))
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
# is accepted too, and is called with the resolved `n`, which is
# how "(N=86)" reaches the header without being written down twice.
#' @rdname plan_verbs
#' @export
# `n` lives here because the header is the only thing that uses it, and
# because it already knows which columns there are: `TRUE` means "the
# denominator each percentage used", read with the SAME `cols` and
# `levels` rtf_plan() was given, so nothing is written twice.  A
# function of the ARD covers a denominator ard_pull() cannot find; a
# named list covers a header that needs more than one.
# There is ONE way to write a header: rtf_col_header(), the same
# constructor as everywhere else.  What the plan adds is that its
# cells may carry `{col}` (the column) and `{n}` (that column's
# denominator), and that a row SHORTER than the table has its last
# cell repeated over the spread columns -- which is the only thing a
# function was ever needed for, since the columns are not known until
# the table exists.  A row that is already the right length, and a
# cell with no token in it, are untouched; a short row is an error
# today, so nothing that works now changes meaning.
plan_col_header <- function(plan, header = NULL, n = NULL,
                            values = NULL) {
  .plan_layer(plan, "header",
              list(header = header, n = n, values = values))
}

# Titles and footnotes are NOT the section header and footer: those are
# RTF's own page furniture, one per section.  These are blocks in the
# BODY of each page -- the title above the table, the footnote a blank
# line below it, both rendered as tables the width of the content.
# rtfreporter already carries them page by page through the `rtf_titles`
# and `rtf_footnotes` attributes, which rtf_tables() reads, so the plan
# attaches them there and needs nothing new downstream.
#
# `...` is the rows of ONE block, used on every page.  `pages =` is a
# list of blocks, one per page, for a table whose pages differ.  Nothing
# is guessed from the shape: a three-row title and a three-page table
# would be ambiguous, and guessing there is how the wrong title ships.
.plan_block <- function(plan, kind, dots, pages) {
  if (length(dots) && !is.null(pages)) {
    .ard_stop(paste0(
      "plan_", kind, "(): give the rows of one block, or `pages = ` ",
      "with one block per page.\n  Not both."))
  }
  .plan_layer(plan, kind,
              list(block = if (length(dots)) dots else NULL,
                   pages = pages))
}

# A listing has no ARD anywhere near it: the source is SDTM or ADaM, an
# ordinary data frame of subject records, and nothing is summarised.  Its
# presence is what says the plan is building one -- a plain data frame
# cannot say so by its columns, and guessing would be the wrong kind of
# clever.  `...` takes listing_col()s; the rest are listing_spec()'s own
# arguments, unchanged.
#' @rdname plan_verbs
#' @export
plan_listing <- function(plan, ..., type = NULL, sep = NULL,
                         spacer = NULL, spacer_rel_width = NULL,
                         blank_row = NULL, blank_row_first = NULL,
                         align = NULL, layout = NULL, wrap = NULL,
                         record = NULL) {
  .plan_layer(plan, "listing",
              list(cols = list(...), type = type, sep = sep,
                   spacer = spacer, spacer_rel_width = spacer_rel_width,
                   blank_row = blank_row,
                   blank_row_first = blank_row_first, align = align,
                   layout = layout, wrap = wrap, record = record))
}

#' @rdname plan_verbs
#' @export
plan_titles <- function(plan, ..., pages = NULL) {
  .plan_block(plan, "titles", list(...), pages)
}

#' @rdname plan_verbs
#' @export
plan_footnotes <- function(plan, ..., pages = NULL) {
  .plan_block(plan, "footnotes", list(...), pages)
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
#' Resolves an [rtf_plan()]'s layers and runs the conversion.  `stage` stops
#' it early, so the same one pass answers "what does this do" and "what did it
#' do" --- there is no second code path that could disagree with the first.
#'
#' @param plan An [rtf_plan()].
#' @param stage How far to go.  `"auto"`, the default, is **as far as the
#'   plan declares**: a plan that says nothing about the display stops at
#'   the table `data.frame`; one that carries a display verb goes on
#'   to the RTF pages.  You rarely need this function at all ---
#'   [rtf_tables()] takes a plan directly --- and naming a stage is
#'   for looking inside: `"long"`, `"args"`, `"table"`, `"pages"`.
#'
#'   The named stages: `"table"` returns the table
#'   `data.frame`, the same object [ard_spread()] returns.  `"long"`
#'   returns the frame going in, with the ARD column names the roles
#'   renamed.  `"args"` returns the resolved argument lists
#'   without running anything --- the call the plan amounts to, as
#'   `$spread` ([ard_spread()]'s) and `$rtf` ([as_rtftables()]'s).  Both
#'   are resolved from layers and either can be the one that
#'   surprises: a page budget declared twice is last-wins, and the
#'   call you are editing may not be the one that decides, so
#'   `apply_plan(p, "args")$rtf$max_rows` is the way to ask.
#'   `"pages"` goes all the way: [fmt_numeric()], [stub_cols()],
#'   [as_rtftables()], [set_col_header()] and whatever `plan_after()`
#'   declared, giving the RTF pages.
#'
#' @return A data frame; for `stage = "args"` the resolved argument list;
#'   for `stage = "pages"` what [as_rtftables()] and the steps after it
#'   return.
#'
#' @section Lifecycle:
#' **Spike.**  See [rtf_plan()].
#'
#' @examples
#' if (requireNamespace("cards", quietly = TRUE)) {
#'   p <- cards::ard_stack(
#'          cards::ADSL, .by = ARM,
#'          cards::ard_continuous(variables = AGE)) |>
#'     ard_normalize() |>
#'     rtf_plan(cols = "ARM", rows = c(group = "variable")) |>
#'     plan_cells(continuous = c("Mean (SD)" = "{mean} ({sd})")) |>
#'     plan_digits(2) |>
#'     plan_digits(AGE = 0)
#'
#'   str(apply_plan(p, "args")$cells)   # AGE won
#'   apply_plan(p)
#' }
#' @seealso [rtf_plan()], [plan_verbs]
#' @export
apply_plan <- function(plan, stage = c("auto", "long", "args",
                                       "table", "pages")) {
  if (!inherits(plan, "rtf_plan")) {
    .ard_stop("Expected an rtf_plan; start from rtf_plan(ard).")
  }
  stage <- match.arg(stage)
  if (identical(stage, "auto")) stage <- .plan_reach(plan)

  # A LISTING never goes near an ARD.  Its source is SDTM or ADaM -- an
  # ordinary frame of subject records -- and plan_listing() is what says
  # so, because the columns cannot.  Nothing is normalised or spread; the
  # rows are the rows.
  if (length(.plan_of(plan, "listing"))) {
    d <- plan$data
    .plan_remember(plan, "table", d)
    if (stage %in% c("table", "long")) return(d)
    return(.plan_to_pages(plan, d))
  }
  # A table somebody already built -- with ard_spread(), with dplyr, with
  # anything -- is a legitimate source for the display half on its own.
  # Asking for the ARD half is what says otherwise: the roles and
  # plan_cells() need statistics, and a finished table has none.
  if (identical(plan$kind, "wide")) {
    ard_half <- vapply(plan$layers, function(l)
      l$kind %in% c("cells", "digits", "round", "levels", "labels"),
      TRUE)
    ard_half <- any(ard_half) || length(plan$roles)
    if (!length(plan$layers)) {
      .ard_stop(paste0(
        "This source has no statistics to read -- no `stat_name` / `stat`, ",
        "and none of\n  the columns ard_normalize() adds -- and the ",
        "plan declares nothing.\n",
        "  A table to lay out : keep the display verbs (plan_stub, ",
        "plan_pages, ...).\n",
        "  A listing of records: add plan_listing(listing_col(...), ...).\n",
        "  An ARD to convert  : name the roles on rtf_plan(), add ",
        "plan_cells().\n",
        "  Columns seen       : ", paste(utils::head(names(plan$data), 8L),
                                         collapse = ", ")))
    }
    if (!ard_half) {
      d <- plan$data
      .plan_remember(plan, "table", d)
      if (stage %in% c("table", "long")) return(d)
      return(.plan_to_pages(plan, d))
    }
    .ard_stop(paste0(
      "This source has no statistics to read -- no `stat_name` / `stat`, ",
      "and none of\n  the columns ard_normalize() adds -- but ",
      "rtf_plan(cols = ) / plan_cells() need them.\n",
      "  If it is already the table, drop those and keep the display ",
      "verbs.\n",
      "  If it is a listing of records, add plan_listing(...).\n",
      "  Columns seen : ", paste(utils::head(names(plan$data), 8L),
                                 collapse = ", ")))
  }

  # 1. the long-frame seam.  Nothing is flattened here: ard_normalize()
  #    ran before the plan, which is why the roles could be checked
  #    against real column names when they were declared.
  x <- .plan_prepare(plan, plan$data)
  .plan_remember(plan, "long", x)
  if (identical(stage, "long")) return(x)

  # 2. the spread arguments.  The roles were said once, on rtf_plan();
  #    `levels` and `labels` are layers and merge one KEY at a time, so a
  #    later one adds a variable without restating the rest.
  s_args <- .plan_spread_args(plan)

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
    # a key variable's own tabulation is no cell (ard_spread() leaves it
    # out), so it asks for no template either
    body <- if (".key_own" %in% names(x)) !(x$.key_own %in% TRUE) else
      rep(TRUE, nrow(x))
    vars <- if ("variable" %in% names(x)) .ard_first_seen(x$variable[body])
            else character(0)
    vars <- vars[!is.na(vars)]
    filled <- list()
    for (v in vars) {
      sel <- body & x$variable == v
      # One entry per SUMMARY -- a variable under one context.  A variable
      # both summarised and tabulated is two summaries, and pinning the
      # first one's template to the variable name would hand the mean
      # recipe to the counts.
      ctxs <- .ard_first_seen(x$context[sel])
      if (!length(ctxs)) ctxs <- NA_character_
      got <- list()
      for (ctx in ctxs) {
        s2  <- sel & (if (is.na(ctx)) is.na(x$context) else
                        (!is.na(x$context) & x$context == ctx))
        knd <- .ard_first_seen(x$.kind[s2])[1L]
        entry <- .plan_lookup_raw(cells, v, ctx, knd)
        if (is.null(entry)) next
        entry <- .plan_fill_entry(entry, .plan_pick(dig, v, ctx, knd))
        .plan_check_open(entry, v)
        got[[.ard_summary_key(v, ctx)]] <- entry
      }
      if (!length(got)) next
      # the usual case -- one summary, or several sharing one recipe --
      # stays keyed by the variable, exactly as before
      if (length(unique(got)) == 1L) filled[[v]] <- got[[1L]]
      else for (k in names(got)) filled[[k]] <- got[[k]]
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
  # One rounding family for the run, last wins like every other layer.
  if (length(rnd)) s_args$rounding <- rnd$rounding

  if (identical(stage, "args")) {
    # Both halves, because both are resolved from layers and either can
    # be the one that surprises you.  A page budget declared twice is the
    # obvious case: last wins, and the call you are editing may not be
    # the one that decides.
    return(list(spread = s_args, rtf = .plan_rtf_args(plan)))
  }
  tbl <- .plan_stage(do.call(ard_spread, c(list(x = x), s_args)),
                     plan, c("spread", "cells", "digits", "round", "levels", "labels"))
  # the table-side seam: a column the table can only know once it exists
  .plan_remember(plan, "table", tbl)
  if (identical(stage, "table")) return(tbl)

  .plan_to_pages(plan, tbl)
}


# A frame that did not come from cards has its own names for the three
# columns ard_spread() reads by name -- which statistic a row is, what
# it is worth, and which analysis variable it belongs to.  Saying so is
# a rename, in the `c(new = old)` vocabulary `rows` and `cols` already
# use, done once here rather than asked for again in every verb.
#
# The label column is the other thing such a frame does differently:
# tfrmt has the same problem, because a continuous row is labelled by
# its statistic and a categorical one by its level, and they are not
# the same column.  `label = c("CAT", "PARAM")` COALESCES -- first
# non-missing wins -- which is what ard_normalize() does for a cards
# ARD when it builds `.label`.
.plan_prepare <- function(plan, d) {
  if (!is.data.frame(d)) return(d)
  for (k in c("variable", "stat_name", "stat")) {
    src <- plan$roles[[k]]
    if (is.null(src) || identical(as.character(src), k)) next
    src <- as.character(src)[1L]
    if (!src %in% names(d)) {
      .ard_stop(sprintf(
        "rtf_plan(%s = %s): no such column.\n  Columns: %s",
        k, sQuote(src), paste(utils::head(names(d), 12L),
                              collapse = ", ")))
    }
    d[[k]] <- d[[src]]
  }
  lb <- .plan_label_spec(plan)
  if (!is.null(lb)) {
    v <- rep(NA_character_, nrow(d))
    for (cc in lb$from) {
      w <- as.character(d[[cc]])
      miss <- is.na(v)
      v[miss] <- w[miss]
    }
    d[[lb$name]] <- v
  }
  d
}

# `label` naming SEVERAL columns is a coalesce; one column is a column.
# Written `c(CAT, STAT)` the name goes on the whole thing, so the
# named form is a one-element LIST -- which is also how a guarded
# template arrives, and that carries formulas rather than columns.
.plan_label_spec <- function(plan) {
  lb <- plan$roles[["label"]]
  if (is.null(lb)) return(NULL)
  nm <- names(lb)
  if (is.list(lb)) {
    if (length(lb) != 1L || !is.character(lb[[1L]]) ||
        length(lb[[1L]]) < 2L) {
      return(NULL)
    }
    out <- if (is.null(nm) || !nzchar(nm[1L])) "label" else nm[1L]
    return(list(name = out, from = unname(lb[[1L]])))
  }
  if (!is.character(lb) || length(lb) < 2L) return(NULL)
  out <- if (is.null(nm) || !any(nzchar(nm))) "label" else
    nm[nzchar(nm)][1L]
  list(name = out, from = unname(lb))
}

# `ard_spread()`'s arguments, assembled from the two places they are
# declared: the roles, said once beside the data, and the keyed
# layers, which merge one key at a time.
.plan_spread_args <- function(plan) {
  out <- plan$roles
  srt <- .plan_merge(.plan_of(plan, "sort"))
  if (length(srt) && !is.null(srt$sort)) out$sort <- srt$sort
  # renames, not arguments: .plan_prepare() has already done them
  out[c("variable", "stat_name", "stat")] <- NULL
  lb <- .plan_label_spec(plan)
  if (!is.null(lb)) out$label <- stats::setNames(lb$name, lb$name)
  for (kind in c("levels", "labels")) {
    v <- .plan_merge(.plan_of(plan, kind), deep = kind)[[kind]]
    if (length(v)) out[[kind]] <- v
  }
  out
}

# The display half, in the order a report is built.  Each step calls the
# function it stands for with the arguments the caller declared, so nothing
# here reimplements as_rtftables() or anything around it.
# One call built from the layers.  Each verb owns its own arguments, so
# this is a merge, not a translation -- the names never change.
.plan_rtf_args <- function(plan, tbl = NULL) {
  out <- list()
  for (kind in c("group", "hide", "blanks", "pages", "style")) {
    for (nm in names(l <- .plan_merge(.plan_of(plan, kind)))) {
      # a dot-name is the plan's own bookkeeping, not an argument
      if (startsWith(nm, ".")) next
      out[[nm]] <- l[[nm]]
    }
  }
  # The row order goes to whichever half can do it: ard_spread() when
  # there are statistics to sort on, as_rtftables() when the source is
  # already the table.
  srt <- .plan_merge(.plan_of(plan, "sort"))
  if (length(srt) && !.plan_ard_half(plan)) {
    if (!is.null(srt$sort) && !is.logical(srt$sort)) {
      out$sort_by <- srt$sort
    }
    if (!is.null(srt$sort_desc)) out$sort_desc <- srt$sort_desc
  }
  # Grouping by the outermost row key is the ordinary case, so
  # plan_row_group() may leave `col` out and have it read off the roles.
  gcol <- .plan_group_col(plan)
  if (!is.null(gcol)) out$group_col <- gcol
  # plan_paginate_group() is as_rtftables(split = "by_value").  Two
  # verbs asking for different splits is a mistake, not something to
  # resolve by order.
  if (isTRUE(.plan_merge(.plan_of(plan, "group"))$.page)) {
    if (!is.null(out$split) && !identical(out$split, "by_value")) {
      .ard_stop(paste0(
        "plan_paginate_group() splits the pages by the group value, ",
        "and\n  plan_paginate_rows(split = ",
        sQuote(out$split), ") splits them another way.  Use one."))
    }
    out$split <- "by_value"
  }
  # `max_rows` is the row budget, and three of the splits do not have
  # one: a value split makes a page per value, "rows" cuts at the
  # positions given, "none" makes one page.  Declaring a budget that
  # is then dropped is how someone spends an afternoon raising it and
  # watching nothing change.
  if (!is.null(out$max_rows) &&
      !is.null(out$split) &&
      out$split %in% c("by_value", "rows", "none")) {
    .ard_stop(paste0(
      "plan_paginate_rows(max_rows = ", out$max_rows, ") has no ",
      "effect with split = ", sQuote(out$split), ".\n",
      if (identical(out$split, "by_value"))
        paste0("  A value split makes one page per group value, ",
               "however long it is --\n  it comes from ",
               "plan_paginate_group().  For a row budget as well, ",
               "page\n  the rows and hide the group key ",
               "instead:\n",
               "    plan_paginate_rows(max_rows = ", out$max_rows,
               ", split = \"group_safe\")\n",
               "  or drop the budget and keep the value split.")
      else paste0("  That split cuts where it is told, not by a ",
                  "count.  Drop `max_rows`,\n  or use ",
                  "split = \"group_safe\" / \"group_force\".")))
  }
  # ... and the two may not name different columns either.
  named <- unique(unlist(lapply(.plan_of(plan, "group"),
                                function(f) f$group_col),
                         use.names = FALSE))
  if (length(named) > 1L) {
    .ard_stop(paste0(
      "plan_paginate_group() and plan_row_group() name different ",
      "columns:\n  ", paste(sQuote(named), collapse = ", "),
      ".  There is one grouping column."))
  }
  # Hiding is the one thing that ADDS rather than replaces: two plan_hide()s
  # mean both columns go, and plan_row_group(show = FALSE) writes one of its own.
  # Last-wins there would silently un-hide whatever was named first.
  hid <- unique(unlist(lapply(.plan_of(plan, "hide"), `[[`, "drop_cols"),
                       use.names = FALSE))

  hid <- unique(c(hid, .plan_hidden(plan, tbl)))
  if (length(hid)) out$drop_cols <- hid
  # A table built from an ARD is a plain data.frame: there is no adapter
  # metadata to read, and every report was saying so by hand.
  if (is.null(out$read_meta)) out$read_meta <- FALSE
  out
}

# Which of these names are actually columns.  Without the table in
# hand nothing is dropped, which is the safe way round: the caller
# gets the column rather than an error about a column that is not
# there.
.plan_present <- function(x, tbl) {
  if (is.null(tbl)) return(character(0))
  intersect(as.character(x), names(tbl))
}

# The columns the plan needs but does not print, from the verbs that
# name them.  ONE answer, because two would drift: the stub asks it to
# know what not to fold in, and as_rtftables() asks it to know what to
# drop.  A derived name is intersected with the table, since a sort
# may also name a statistic or ".depth", which are not columns.
.plan_hidden <- function(plan, tbl = NULL) {
  out <- character(0)
  g <- .plan_merge(.plan_of(plan, "group"))
  if (length(g) && isFALSE(g$.show)) {
    # folded into the stub already?  then it is gone, not hidden.  With
    # no table to look at, keep it: the carrier is normally there.
    gc <- .plan_group_col(plan)
    out <- c(out, if (is.null(tbl)) gc else .plan_present(gc, tbl))
  }
  pg <- .plan_merge(.plan_of(plan, "pages"))
  if (length(pg) && isFALSE(pg$.show) && !is.null(pg$page_by)) {
    out <- c(out, .plan_present(pg$page_by, tbl))
  }
  srt <- .plan_merge(.plan_of(plan, "sort"))
  if (length(srt) && isFALSE(srt$.show) && !is.null(srt$sort) &&
      !is.logical(srt$sort)) {
    out <- c(out, .plan_present(sub("^-", "", srt$sort), tbl))
  }
  unique(out[!is.na(out)])
}

# Attach the title / footnote blocks to each page.  One block goes on
# every page; `pages =` is taken in order and must be as long as the
# pages there turned out to be -- a mismatch is a mistake, not something
# to recycle.
.plan_blocks <- function(plan, out) {
  one <- inherits(out, "rtftable")
  pg  <- if (one) list(out) else out
  for (kind in c("titles", "footnotes")) {
    l <- .plan_merge(.plan_of(plan, kind))
    if (!length(l)) next
    blocks <- if (!is.null(l$pages)) l$pages else
      rep(list(unlist(l$block, use.names = FALSE)), length(pg))
    if (length(blocks) != length(pg)) {
      .ard_stop(paste0(
        "plan_", kind, "(pages = ) has ", length(blocks), " block",
        if (length(blocks) == 1L) "" else "s", " for ", length(pg),
        " page", if (length(pg) == 1L) "" else "s", ".\n",
        "  The page count is decided by plan_paginate_rows(); print(x) after a ",
        "run shows it.", .plan_blame(plan, kind)))
    }
    at <- if (identical(kind, "titles")) "rtf_titles" else "rtf_footnotes"
    for (i in seq_along(pg)) attr(pg[[i]], at) <- blocks[[i]]
  }
  if (one) pg[[1L]] else pg
}

# The stub is the row keys plus the label column, minus any column that is
# only there to group by.  All of that was said on rtf_plan(), so saying it
# again is a place for the two to disagree.
# What the label column is called in the spread table.
.plan_label_name <- function(plan) {
  lb <- plan$roles[["label"]]
  if (is.null(lb)) return("label")
  if (length(lb) == 1L && !is.list(lb) && is.na(lb)) {
    return(character(0))
  }
  sp <- .plan_label_spec(plan)
  if (!is.null(sp)) return(sp$name)
  if (!is.null(names(lb)) && nzchar(names(lb)[1L])) names(lb)[1L]
  else "label"
}

.plan_stub_vars <- function(plan, tbl) {
  rn <- .plan_row_keys(plan)
  ln <- .plan_label_name(plan)
  # a column that is not printed is not part of the stub either: the
  # page key and the grouping carrier are row keys that do their work
  # without being read
  v <- setdiff(c(rn, ln), .plan_hidden(plan, tbl))
  v <- intersect(v, names(tbl))
  if (!length(v)) {
    .ard_stop(paste0(
      "plan_stub(): nothing to fold.  The row keys and label column are ",
      "worked out from\n  rtf_plan(rows = , label = ) less ",
      "whatever `show = FALSE` hides, and none\n  of them is in ",
      "the table.  ",
      "Name them with `vars = `.\n  Columns: ",
      paste(utils::head(names(tbl), 8L), collapse = ", ")))
  }
  v
}

# The names the row keys have in the spread table: what `rows` was called,
# or `group` when it was left to the analysis variable.
.plan_row_keys <- function(plan) {
  r <- plan$roles[["rows"]]
  if (is.null(r)) return("group")
  nm <- names(r)
  if (is.null(nm)) as.character(unlist(r, use.names = FALSE)) else nm
}

# Which column groups the rows.  Left out it usually stays out --
# as_rtftables() has its own answer, and two reports group without
# naming a column at all.  The exception is `show = FALSE`: a carrier
# that is not printed has to be NAMED to be hidden, and that name is
# always the outermost row key, which rtf_plan(rows = ) has already
# given.  Deriving it there and nowhere else is the difference between
# removing a duplicate and guessing.
.plan_group_col <- function(plan) {
  g <- .plan_of(plan, "group")
  if (!length(g)) return(NULL)
  m <- .plan_merge(g)
  if (!is.null(m$group_col)) return(m$group_col)
  if (!isFALSE(m$.show)) return(NULL)
  k <- .plan_row_keys(plan)
  if (length(k)) k[1L] else NULL
}

# A table splits on THREE axes and plan_paginate_rows() only ever covered one.
# The column blocks were reachable only through
# plan_after(paginate_cols(...)) -- a lambda around the very call the
# plan exists to take apart, and two of the six reports wrote one.
# This is that axis, with paginate_cols()'s own arguments, and
# `order` is where the three nest: "group" (a value split), "rows"
# (page_by and every row split) and "cols" (these blocks), outermost
# first, or the shorthands "across" / "down".
#' @rdname plan_verbs
#' @export
plan_paginate_cols <- function(plan, at = NULL, cols = NULL,
                               by = NULL, every = NULL, carry = NULL,
                               col_header = NULL, width = NULL,
                               allow_span_break = NULL,
                               order = NULL) {
  .plan_layer(plan, "colpages",
              list(at = at, cols = cols, by = by, every = every,
                   carry = carry,
                   col_header = col_header, width = width,
                   allow_span_break = allow_span_break,
                   page_order = order))
}

# `every` in the columns the table actually has.  A plan is deferred, so
# it can count them; `at = c(16, 29)` is that count written out for one
# study, and a study with a different number of timepoints needs it
# rewritten.  Carried headings belong to every block, so they are not
# counted.
.plan_colpages_every <- function(cp, out) {
  k <- as.integer(cp$every)
  cp$every <- NULL
  first <- if (inherits(out, "rtftable")) out else out[[1L]]
  nm <- names(first$data)
  car <- cp$carry %||% first$row_title %||% 1L
  ci <- if (is.character(car)) match(car, nm) else as.integer(car)
  rest <- setdiff(seq_along(nm), ci[!is.na(ci)])
  # A table narrower than one block is not an error and not a one-block
  # split: it is a table that needs no column pages at all.
  if (length(rest) <= k) return(NULL)
  cp$at <- rest[seq(k + 1L, length(rest), by = k)]
  cp
}

# ---------------------------------------------------------------------------
#  The header's N (#480, #482)
# ---------------------------------------------------------------------------
#  A header's number is the size of the population a column describes --
#  an arm's analysis set, an arm x sex cell.  An ARD states that number in
#  only a few places, and an analysis variable's `N` is NOT one of them: it
#  is the count of that variable's non-missing values, the arm size only
#  when nothing is missing, which the ARD cannot show.  Over 26 cards /
#  cardx builds (#482) reading it gave a plausible wrong number in six --
#  79 over 86 for an AGE with missing values, 151 for AE records counted
#  without a denominator, the study total in every column.  So a number is
#  taken only from:
#
#   1. a cards sentinel's `N` keyed by exactly these keys
#      (`..ard_hierarchical_overall..`: each arm's denominator);
#   2. the key variable's own tabulation (`.key_own`), when its `n` add up
#      to the `N` it states -- a partition of the population;
#   3. an analysis summary's `N` when it is a denominator by construction
#      (a hierarchical summary is a percentage of the denominator data), or
#      when two or more different variables state the same number.  Two
#      variables missing for exactly the same subjects is not what an
#      agreement usually means; one variable alone proves nothing.
#
#  It is read at EVERY depth of the column keys -- "Placebo" for the arm,
#  "Placebo____F" for the arm x sex cell -- because a spanner wants the
#  first and a leaf the second, and adding up the leaves is right only
#  when the lower key partitions the upper one (visits do not).  A key
#  with no stated number is left out, with the reason on attr "why"; the
#  header prints NA there and warns.  Nothing is guessed.

.plan_n_read <- function(plan, sp, d = plan$data) {
  cols <- as.character(unlist(sp$cols, use.names = FALSE))
  sep <- sp$sep %||% "____"
  total <- attr(d, "ard_total_n", exact = TRUE)
  out <- numeric(0)
  why <- character(0)
  done <- function() structure(out, total = total, why = why)
  if (!is.data.frame(d) || !length(cols) || !all(cols %in% names(d)) ||
      !all(c("variable", "stat_name", "stat") %in% names(d))) {
    why <- c(.all = paste0("the data has no `variable` / `stat_name` / ",
                           "`stat` for these columns"))
    return(done())
  }
  v   <- as.character(d$variable)
  sn  <- as.character(d$stat_name)
  st  <- suppressWarnings(as.numeric(as.character(d$stat)))
  ctx <- if ("context" %in% names(d)) as.character(d$context) else
    rep(NA_character_, nrow(d))
  own <- if (".key_own" %in% names(d)) d$.key_own %in% TRUE else
    rep(FALSE, nrow(d))
  sent <- !is.na(v) & startsWith(v, "..")
  has  <- lapply(cols, function(k) !is.na(d[[k]]))
  # a sentinel keyed by nothing is ONE number for the study
  # (..ard_total_n..): a total, never every column's number
  none <- Reduce(`&`, lapply(has, `!`))
  tt <- unique(st[sent & none & sn %in% "N" & !is.na(st)])
  if (length(tt) == 1L) total <- tt
  # two different sentinels are a choice, and choosing is a guess
  one_sent <- length(unique(v[sent & !none])) <= 1L
  for (k in seq_along(cols)) {
    at <- Reduce(`&`, has[seq_len(k)])
    if (k < length(cols)) {
      at <- at & Reduce(`&`, lapply(has[-seq_len(k)], `!`))
    }
    if (!any(at)) next
    key <- do.call(paste, c(lapply(cols[seq_len(k)], function(cc)
      as.character(d[[cc]])), list(sep = sep)))
    got <- numeric(0)
    add <- function(x) {
      x <- x[!names(x) %in% names(got)]
      got <<- c(got, x)
    }
    if (one_sent) add(.plan_n_unique(st, key, at & sent & sn %in% "N"))
    part <- .plan_n_partition(d, cols, k, key, at & own & v %in% cols[k],
                              sn, st, sep)
    # the key tabulated on its own states the table's population too:
    # its N is what the columns add up to (ard_stack(.by = ) says 254)
    if (k == 1L && is.null(total) && length(attr(part, "N")) == 1L) {
      total <- attr(part, "N")
    }
    attr(part, "N") <- NULL
    add(part)
    s <- .plan_n_summaries(v, ctx, sn, st, key, at & !own & !sent)
    add(s$n)
    miss <- setdiff(names(s$why), names(got))
    why <- c(why, s$why[miss])
    # the columns' order: declared levels first, then the frame's own
    ord <- .ard_key_order(d[at, , drop = FALSE], cols[seq_len(k)], key[at])
    lv <- sp$levels[[cols[1L]]]
    if (!is.null(lv)) {
      top <- vapply(strsplit(ord, sep, fixed = TRUE), `[`, "", 1L)
      ord <- ord[order(match(top, lv), seq_along(ord))]
    }
    got <- got[c(intersect(ord, names(got)), setdiff(names(got), ord))]
    out <- c(out, got)
  }
  done()
}

# One number per key from the selected rows, or nothing for that key.
.plan_n_unique <- function(st, key, sel) {
  sel <- sel & !is.na(st)
  if (!any(sel)) return(numeric(0))
  vals <- lapply(split(st[sel], key[sel]), unique)
  vals <- vals[lengths(vals) == 1L]
  unlist(vals)
}

# The key variable's own tabulation at depth `k`: `cols[k]` counted within
# each value of `cols[1..k-1]`.  Taken only where it is a partition -- its
# `n` add up to the one `N` those rows state -- because a key tabulated
# as an EVENT (the treatment of the subjects with an AE) is not the
# population split by arm.
.plan_n_partition <- function(d, cols, k, key, sel, sn, st, sep) {
  n_sel <- sel & sn %in% "n"
  if (!any(n_sel)) return(numeric(0))
  grp <- if (k == 1L) rep("", nrow(d)) else
    do.call(paste, c(lapply(cols[seq_len(k - 1L)], function(cc)
      as.character(d[[cc]])), list(sep = sep)))
  out <- numeric(0)
  bigs <- numeric(0)
  for (g in unique(grp[n_sel])) {
    i <- n_sel & grp == g
    n <- st[i]
    kk <- key[i]
    if (anyNA(n) || anyDuplicated(kk)) next
    big <- unique(st[sel & sn %in% "N" & grp == g])
    if (length(big) != 1L || is.na(big) || !isTRUE(all.equal(sum(n), big))) {
      next
    }
    out <- c(out, stats::setNames(n, kk))
    bigs <- c(bigs, big)
  }
  # the population the partition splits -- at depth 1, the whole table's
  structure(out, N = bigs)
}

# The analysis summaries' `N`, where it can be trusted to be a column's
# population, and why not where it cannot.
.plan_n_summaries <- function(v, ctx, sn, st, key, sel) {
  idx <- which(sel & sn %in% "N" & !is.na(st))
  none <- list(n = numeric(0), why = character(0))
  if (!length(idx)) return(none)
  who <- paste(v[idx], ctx[idx], sep = "\r")
  tabs <- list()
  split_up <- character(0)
  for (s in unique(who)) {
    j <- idx[who == s]
    vals <- lapply(split(st[j], key[j]), unique)
    # more than one N per column is not a column's number -- one per
    # visit, per parameter, per row
    if (any(lengths(vals) != 1L)) {
      split_up <- c(split_up, sub("\r.*$", "", s))
      next
    }
    tabs[[s]] <- unlist(vals)
  }
  out <- numeric(0)
  why <- character(0)
  # Variables that disagree for ANY column are counting their own
  # non-missing values, so where they happen to agree it is chance
  # (80 and 80 for one arm, 79 and 80 for the next), not a population.
  clash <- character(0)
  for (kk in unique(key[idx])) {
    vals <- unlist(lapply(tabs, function(t) if (kk %in% names(t)) t[[kk]]))
    if (length(unique(vals)) > 1L) {
      clash <- c(clash, paste0(kk, " -- ", paste0(
        sub("\r.*$", "", names(vals)), " ", vals, collapse = ", ")))
    }
  }
  for (kk in unique(key[idx])) {
    have <- Filter(function(t) kk %in% names(t), tabs)
    if (!length(have)) {
      why[[kk]] <- sprintf(paste0(
        "%s states an N per row (a visit, a parameter), not one per column"),
        paste(unique(split_up), collapse = ", "))
      next
    }
    vals <- vapply(have, function(t) t[[kk]], 0)
    vars <- sub("\r.*$", "", names(have))
    hier <- grepl("hierarch", sub("^[^\r]*\r", "", names(have)))
    if (length(clash)) {
      why[[kk]] <- sprintf(
        "the analysis variables state different N (%s)", clash[[1L]])
    } else if (any(hier) || length(unique(vars)) >= 2L) {
      out[[kk]] <- vals[[1L]]
    } else {
      why[[kk]] <- sprintf(paste0(
        "only %s states an N (%s) -- the count of its non-missing ",
        "values, not necessarily the analysis set"), vars[[1L]], vals[[1L]])
    }
  }
  list(n = out, why = why)
}

# Every token a header cell may carry, with what it resolves to.
# `print()` shows this because the answer is otherwise invisible:
# the values come from the ARD and the columns from the spread, and
# neither is written in the call.
.plan_header_tokens <- function(plan) {
  tbl <- plan$cache$table
  hdr <- .plan_merge(.plan_of(plan, "header"))
  why <- NULL
  nvals <- tryCatch(.plan_n_values(plan, hdr$n), error = function(e) {
    why <<- conditionMessage(e)
    NULL
  })
  toks <- if (is.list(nvals) && !is.null(names(nvals)) &&
              any(nzchar(names(nvals)))) nvals
          else if (is.null(nvals)) list() else list(n = nvals)
  cols <- if (is.null(tbl)) NULL else .plan_spread_cols(plan, tbl)
  sep <- plan$roles$sep %||% "____"
  out <- list()
  # A token that cannot be resolved is the one worth printing: saying
  # nothing is what sent you here.
  if (!is.null(why) && !is.null(hdr$n)) {
    out[["{n}"]] <- paste0("-- NOT resolved: ",
                           strsplit(why, "\n", fixed = TRUE)[[1L]][1L])
  }
  # Show the values, not a description of them: a denominator, and the
  # text a level prints as, are the things you came to check.
  show <- function(v) {
    if (length(v) == 1L && is.null(names(v))) {
      return(paste0("= ", format(v, trim = TRUE)))
    }
    nm <- names(v) %||% rep("", length(v))
    one <- paste0(ifelse(nzchar(nm), paste0(nm, " = "), ""),
                  vapply(v, function(z) format(z, trim = TRUE), ""))
    txt <- paste0("= c(", paste(one, collapse = ", "), ")")
    if (nchar(txt) > 96L) {
      keep <- 0L
      w <- 0L
      for (i in seq_along(one)) {
        w <- w + nchar(one[i]) + 2L
        if (w > 80L) break
        keep <- i
      }
      keep <- max(keep, 1L)
      txt <- paste0("= c(", paste(one[seq_len(keep)], collapse = ", "),
                    ", ... ", length(one) - keep, " more)")
    }
    txt
  }
  if (!is.null(cols) && length(cols)) {
    pp <- strsplit(cols, sep, fixed = TRUE)
    lv <- max(vapply(pp, length, 1L))
    lvl <- function(i) unique(vapply(pp, function(z)
      if (i <= length(z)) z[i] else NA_character_, ""))
    leaf <- unique(vapply(pp, function(z) z[length(z)], ""))
    out[["{col}"]] <- show(leaf[!is.na(leaf)])
    if (lv > 1L) {
      for (i in seq_len(lv)) {
        v <- lvl(i)
        out[[paste0("{col", i, "}")]] <- show(v[!is.na(v)])
      }
    }
  }
  for (nm in names(toks)) {
    v <- toks[[nm]]
    why_n <- attr(v, "why", exact = TRUE)
    if (!length(v)) {
      out[[paste0("{", nm, "}")]] <- paste0(
        "-- NOT resolved: ", if (length(why_n)) why_n[[1L]] else
          "the ARD states no population size for these columns")
      next
    }
    out[[paste0("{", nm, "}")]] <- show(v)
    if (length(why_n)) {
      out[[paste0("{", nm, "} NOT resolved")]] <- paste0(
        "-- ", paste0(ifelse(names(why_n) == ".all", "",
                             paste0(names(why_n), ": ")), why_n)[[1L]],
        if (length(why_n) > 1L) sprintf(" (+%d more)", length(why_n) - 1L))
    }
    # the leaves only: a vector keyed at several depths holds each
    # subject once per depth
    nmv <- names(v) %||% character(0)
    lv <- v
    if (length(nmv) && !is.null(cols)) {
      leafy <- intersect(cols, nmv)
      if (length(leafy)) lv <- v[leafy]
    } else if (length(nmv)) {
      dep <- lengths(strsplit(nmv, sep, fixed = TRUE))
      lv <- v[dep == max(dep)]
    }
    tot <- suppressWarnings(sum(as.numeric(unlist(lv)), na.rm = TRUE))
    out[[paste0("{", nm, ":sum}")]] <- paste0(
      "= ", format(tot, trim = TRUE),
      " over every column (less over a spanner: its own columns)")
    for (k in names(v) %||% character(0)) {
      out[[paste0("{", nm, ":", k, "}")]] <-
        paste0("= ", format(v[[k]], trim = TRUE))
    }
  }
  out
}

# The denominator, read once, with the keys rtf_plan() already has.
.plan_n_values <- function(plan, n, data = plan$data) {
  if (is.null(n)) return(NULL)
  sp <- .plan_spread_args(plan)
  one <- function(v) {
    if (isTRUE(v)) {
      if (is.null(sp$cols)) {
        .ard_stop(paste0(
          "plan_col_header(n = TRUE) reads the denominator with the ",
          "same `cols` rtf_plan()\n  was given, and this plan ",
          "has none.  Give a function of the data instead."))
      }
      # Only a number the ARD STATES as a population size, at every
      # depth of the keys; what it does not state is left out, and the
      # header prints NA there and says why (see .plan_n_read()).
      # ard_pull() is not asked: it reads an analysis variable's `N`,
      # the count of its non-missing values.
      return(.plan_n_read(plan, sp, data))
    }
    if (is.function(v)) v(data) else v
  }
  if (is.list(n) && !is.null(names(n)) && any(nzchar(names(n)))) {
    return(lapply(n, one))
  }
  one(n)
}

# The data column the pages are split on, and each page's value of it,
# when the plan splits pages by a group value.  A page whose name is not
# a value of that column (relabelled, or cut further) gets NULL and falls
# back to the whole table's numbers: a page never borrows another's.
.plan_page_group <- function(plan, page_names) {
  if (!isTRUE(.plan_merge(.plan_of(plan, "group"))$.page)) return(NULL)
  gcol <- .plan_group_col(plan)
  if (is.null(gcol) || is.null(page_names)) return(NULL)
  r <- plan$roles$rows
  src <- if (!is.null(names(r)) && gcol %in% names(r)) r[[gcol]] else gcol
  if (!is.character(src) || length(src) != 1L ||
      !src %in% names(plan$data)) {
    return(NULL)
  }
  raw <- .ard_first_seen(plan$data[[src]])
  value <- lapply(page_names, function(nm) {
    if (nm %in% raw) return(nm)
    hit <- raw[startsWith(nm, raw)]
    if (length(hit)) hit[which.max(nchar(hit))] else NULL
  })
  list(col = src, value = value)
}

# A page's numbers: its own rows first, then the rows that belong to no
# page (a study total, a tabulation made without the page key).
.plan_n_page <- function(plan, n, col, value) {
  d <- plan$data
  g <- as.character(d[[col]])
  mine <- d[!is.na(g) & g == value, , drop = FALSE]
  rest <- d[is.na(g), , drop = FALSE]
  attr(rest, "ard_total_n") <- attr(d, "ard_total_n", exact = TRUE)
  a <- .plan_n_values(plan, n, mine)
  b <- .plan_n_values(plan, n, rest)
  join <- function(x, y) {
    if (!is.numeric(x) || !is.numeric(y)) return(x)
    keep <- setdiff(names(y) %||% character(0), names(x))
    out <- c(x, y[keep])
    tx <- attr(x, "total", exact = TRUE)
    wx <- attr(x, "why", exact = TRUE)
    attr(out, "total") <- tx %||% attr(y, "total", exact = TRUE)
    attr(out, "why") <- wx[!names(wx) %in% names(out)]
    out
  }
  if (is.list(a) && !is.numeric(a)) return(Map(join, a, b))
  join(a, b)
}

.plan_to_pages <- function(plan, tbl) {
  hdr <- .plan_merge(.plan_of(plan, "header"))
  # a workbook's header asks for the ARD's N when its text says `{n}`; a
  # header written in code after it states its own
  n_req <- hdr$n
  if (is.null(n_req) && inherits(hdr$header, "plan_spec_col_header") &&
      isTRUE(hdr$header$n)) {
    n_req <- TRUE
  }
  nvals <- .plan_n_values(plan, n_req)

  fmt <- .plan_merge(.plan_of(plan, "fmt"))
  if (length(fmt)) {
    # `cols` left out means the VALUE cells -- the spread columns.  The
    # plan knows which they are, and writing `3:31` instead is a count
    # of the columns one particular study happened to have.
    if (is.null(fmt$cols)) fmt$cols <- .plan_spread_cols(plan, tbl)
    tbl <- do.call(fmt_numeric, c(list(data = tbl), fmt))
  }

  .plan_remember(plan, "table", tbl)

  stub <- .plan_merge(.plan_of(plan, "stub"))
  if (length(stub) && is.null(stub$vars)) {
    stub$vars <- .plan_stub_vars(plan, tbl)
  }
  before <- isTRUE(stub$before)
  stub$before <- NULL
  pre <- tbl
  if (length(stub) && before) {
    # only what the caller named: stub_cols() has its own defaults, and
    # handing it NULL is not the same as leaving it alone
    a <- stub[!vapply(stub, is.null, logical(1L))]
    tbl <- do.call(stub_cols, c(list(data = tbl), a))
  }

  lst <- .plan_merge(.plan_of(plan, "listing"))
  rtf <- .plan_rtf_args(plan, tbl)
  if (length(lst)) {
    cols <- lst$cols; lst$cols <- NULL
    lst <- lst[!vapply(lst, is.null, logical(1L))]
    rtf$listing <- do.call(listing_spec, c(list(cols = cols), lst))
  }
  if (length(stub) && !before) {
    rtf$stub_vars         <- stub$vars
    rtf$stub_label        <- stub$label
    rtf$stub_indent       <- stub$indent
    rtf$stub_group_summary <- stub$group_summary
    rtf <- rtf[!vapply(rtf, is.null, logical(1L))]
  }
  # done here, where the final column count is known
  if (!is.null(rtf$col_rel_width)) {
    w <- rtf$col_rel_width
    nfinal <- ncol(tbl) -
      (if (!is.null(rtf$stub_vars)) length(rtf$stub_vars) - 1L else 0L) -
      length(intersect(rtf$drop_cols %||% character(0), names(tbl)))
    if (length(w) >= 1L && length(w) < nfinal) {
      rtf$col_rel_width <- c(w, rep(w[length(w)], nfinal - length(w)))
    }
  }

  st  <- .plan_merge(.plan_of(plan, "styles"))
  if (length(st)) {
    # The styles are built against the rows the plan can SEE.  Folding the
    # stub inside as_rtftables() adds heading rows the plan never saw, and
    # rtftable() would reject the length with nothing to say about why.
    if (!is.null(rtf$stub_vars)) {
      .ard_stop(paste0(
        "plan_cell_style() needs plan_stub(before = TRUE).\n",
        "  Folded inside as_rtftables(), the stub adds group heading rows ",
        "the conditions\n  never saw, so a style would land on the ",
        "wrong row."))
    }
    if (!is.null(rtf$cell_styles)) {
      .ard_stop("plan_cell_style() and a cell_styles = of your own: use one.")
    }
    rtf$cell_styles <- .plan_cell_styles(st, tbl, pre)
  }
  out <- .plan_stage(
    do.call(as_rtftables, c(list(x = tbl), rtf)), plan,
    c("group", "hide", "sort", "blanks", "pages", "style", "stub",
      "styles"))

  # the `columns` sheet names the columns, so it is applied to the pages,
  # where the names are the ones printed (a folded stub, a hidden carrier)
  colset <- .plan_merge(.plan_of(plan, "columns"))
  spread <- .plan_spread_cols(plan, pre)
  if (length(colset$widths) && is.null(rtf$col_rel_width)) {
    out <- .plan_col_widths(out, colset$widths, spread)
  }
  # what as_table_spec() reads back: the columns and widths before the
  # column axis cuts them into blocks
  fp <- if (inherits(out, "rtftable")) out else out[[1L]]
  .plan_remember(plan, "pre_cols", list(
    names = names(fp$data), spread = intersect(spread, names(fp$data)),
    widths = fp$col_rel_width))

  if (!is.null(hdr$header)) {
    #  a function of the resolved `n`, so "(N=86)" is written once and
    #  the number comes from the ARD rather than from memory.  The
    #  header may need the finished table -- "a spanner over columns 3
    #  to the last" is a fact about the table, not about the ARD -- so
    #  a function of two arguments is given (values, table).
    #
    #  `cols` / `stub` are the same thing without the function, for
    #  the header that only repeats itself over the columns.
    build <- function(page_d, nv) {
      sc <- intersect(.plan_spread_cols(plan, pre), names(page_d))
      h <- if (inherits(hdr$header, "plan_spec_col_header"))
             .plan_spec_col_header(hdr$header, names(page_d), sc, plan)
           else if (!is.function(hdr$header)) hdr$header
           else if (length(formals(hdr$header)) >= 2L)
             hdr$header(nv, tbl)
           else hdr$header(nv)
      list(raw = h, filled = .plan_header_fill(
        h, nv, sc, length(page_d) - length(sc), plan$roles$sep))
    }
    first_d <- if (inherits(out, "rtftable")) out$data else out[[1L]]$data
    # Pages split by a group value (a lab parameter, a visit) each
    # describe their own population, so each page's `{n}` is read from
    # that page's rows of the ARD -- the subjects with an ALT result are
    # not the subjects with a haemoglobin one.
    pg <- if (is.null(n_req) || inherits(out, "rtftable")) NULL
          else .plan_page_group(plan, names(out))
    if (!is.null(pg)) {
      for (i in seq_along(out)) {
        nv <- if (is.null(pg$value[[i]])) nvals
              else .plan_n_page(plan, n_req, pg$col, pg$value[[i]])
        b <- build(out[[i]]$data, nv)
        if (i == 1L) .plan_remember(plan, "header_raw", b$raw)
        args <- list(x = out[[i]], b$filled)
        if (!is.null(hdr$values)) args$values <- hdr$values
        out[[i]] <- do.call(set_col_header, args)
      }
    } else {
      b <- build(first_d, nvals)
      .plan_remember(plan, "header_raw", b$raw)
      # the tokens and the short-row rule, on whatever came back
      args <- list(x = out, b$filled)
      if (!is.null(hdr$values)) args$values <- hdr$values
      out <- do.call(set_col_header, args)
    }
  }

  if (length(colset$decimal)) {
    first_d <- if (inherits(out, "rtftable")) out$data else out[[1L]]$data
    dc <- .plan_col_names(colset$decimal, names(first_d), spread)
    if (length(dc)) out <- set_decimal_split(out, cols = dc)
  }
  for (l in .plan_of(plan, "after")) {
    for (f in l$steps) out <- f(out)
  }
  # The column axis comes LAST: cutting the table into blocks renumbers
  # its columns, and everything that names a column by position -- a
  # plan_after() step like set_decimal_split(cols = 3:31) -- means the
  # table as it was written, not the first block of it.
  cp <- .plan_merge(.plan_of(plan, "colpages"))
  cp <- cp[!vapply(cp, is.null, logical(1L))]
  if (!is.null(cp$every)) cp <- .plan_colpages_every(cp, out) %||% list()
  if (length(cp)) {
    out <- .plan_stage(do.call(paginate_cols, c(list(x = out), cp)),
                       plan, "colpages")
  }
  # the blocks that sit above and below the table on each page
  out <- .plan_blocks(plan, out)

  # the names plan_cell_style() / plan_style(widths = ) / plan_col_header() use:
  # read off the finished page rather than predicted
  first <- if (inherits(out, "rtftable")) out else out[[1L]]
  if (!is.null(first$data)) .plan_remember(plan, "printed", first$data)
  out
}

# The columns the spread made: everything that is not a row key or the
# label.  Knowing them is what lets a header be written as ONE cell
# template instead of a function that reassembles the names.
.plan_spread_cols <- function(plan, tbl) {
  lb <- .plan_label_name(plan)
  setdiff(names(tbl), c(.plan_row_keys(plan), lb))
}

# "arm name over (N=86)" is the commonest header in clinical work, and
# writing it needed a function only because the arms are not known
# until the table exists.  The plan knows them, so a template per
# HEADER ROW is enough: `{col}` is the column, `{n}` its denominator.
# Fill `{col}` / `{n}` in a header, and repeat a short row's last
# cell over the spread columns.  `cols` are the spread columns and
# `n_lead` the ones to the left of them.
#
# Several `cols` keys make a name like "Placebo____Negative", which
# nobody wants printed.  `{col}` is therefore the LEAF -- the last
# level, which is what a single header row over those columns says --
# and `{col1}`, `{col2}`, ... are the levels in order.  With one key
# the leaf is the whole name, so nothing changes there.  The hierarchy
# itself needs no header at all: as_rtftables(header_sep = ) builds
# the spanning rows from the same separator.
.plan_header_fill <- function(h, nvals, cols, n_lead, sep = NULL) {
  if (is.null(h) || !length(cols)) return(h)
  sep <- sep %||% "____"
  parts <- function(col) {
    if (is.null(col)) character(0) else
      strsplit(col, sep, fixed = TRUE)[[1L]]
  }
  # `n` may be one value, a vector named by column, or a NAMED LIST of
  # either -- and then each name is its own token.  That is how a
  # header says both numbers at once: the study total in a spanner
  # (`{total}`) and each column's own underneath it (`{n}`), with no
  # function to carry them.  `{n}` is the entry called `n`, or the
  # only entry when there is one.
  toks <- if (is.list(nvals) && !is.null(names(nvals)) &&
              any(nzchar(names(nvals)))) nvals else list(n = nvals)
  if (is.null(toks[["n"]]) && length(toks) == 1L) names(toks) <- "n"
  # A NAMED vector is keyed at any depth of the column keys: "Placebo"
  # is the arm, "Placebo____F" the arm x sex cell.  An UNNAMED single
  # number is every cell's.  The study total (attr "total") answers
  # only for the whole table: a one-column table, or a cell over all
  # of it -- never each column, which is how 254 got into every arm.
  whole <- function(v) {
    t <- attr(v, "total", exact = TRUE)
    if (is.null(t)) NA else t
  }
  lookup <- function(v, key, all_cols = FALSE) {
    if (is.null(v)) return(NA)
    nm <- names(v)
    if (length(v) == 1L && (is.null(nm) || !any(nzchar(nm)))) {
      return(v[[1L]])
    }
    if (!is.null(key) && key %in% nm) return(v[[key]])
    if (all_cols || length(cols) == 1L) return(whole(v))
    NA
  }
  # Every token that came out NA, so ONE warning can say which and why
  # -- a header that quietly printed "" was how a missing N went unseen.
  missed <- character(0)
  miss <- function(tok, where) {
    missed[[length(missed) + 1L]] <<- sprintf("{%s} over %s", tok, where)
  }
  # `{n:sum}` is the total over the columns THIS CELL COVERS -- so a
  # spanner over an arm's two columns shows that arm's N, and one over
  # all of them shows the study total, without the header being told
  # either.  A cell covering one column sums to that column; a cell
  # outside the data (the stub) sums over every column.  Only the
  # columns themselves are added: a vector keyed at several depths
  # would otherwise count each subject twice.
  summed <- function(v, over) {
    if (is.null(v)) return(NA)
    nm <- names(v) %||% character(0)
    if (!length(nm)) return(if (length(v) == 1L) v[[1L]] else NA)
    if (!length(over)) {
      keep <- intersect(cols, nm)
      # a vector keyed by something else entirely: every entry
      if (!length(keep)) keep <- nm
      else if (length(keep) < length(cols)) return(NA)
    } else {
      keep <- intersect(over, nm)
      # a column with no number makes the sum a wrong number, not a
      # smaller one
      if (!length(keep) ||
          length(keep) < length(intersect(over, cols))) return(NA)
    }
    sum(suppressWarnings(as.numeric(unlist(v[keep]))), na.rm = TRUE)
  }
  one <- function(tpl, col, over = character(0)) {
    if (!is.character(tpl) || !length(tpl) ||
        !grepl("{", tpl, fixed = TRUE)) {
      return(tpl)
    }
    # A SPANNER has no single column, but it does have the levels its
    # columns agree on: over "Placebo____Negative" and
    # "Placebo____Positive", `{col1}` is "Placebo" and `{col2}` is empty.
    # That is the arm name a spanning cell wants, and it comes from the
    # columns rather than from the author.
    pp <- if (!is.null(col)) parts(col) else if (length(over)) {
      lv <- lapply(over, parts)
      k <- max(vapply(lv, length, 1L))
      vapply(seq_len(k), function(i) {
        v <- unique(vapply(lv, function(z)
          if (i <= length(z)) z[i] else NA_character_, ""))
        if (length(v) == 1L && !is.na(v)) v else ""
      }, "")
    } else character(0)
    # how many leading levels the cell stands for: all of them over one
    # column, the agreed ones over a spanner, none over everything
    depth <- if (!is.null(col)) length(pp) else {
      a <- which(!nzchar(pp))
      if (length(a)) a[1L] - 1L else length(pp)
    }
    all_cols <- is.null(col) && (!length(over) || setequal(over, cols))
    where <- if (!is.null(col)) sQuote(col) else if (depth > 0L)
      sQuote(paste(pp[seq_len(depth)], collapse = sep)) else if (all_cols)
      "all columns" else paste(sQuote(over), collapse = " + ")
    out <- tpl
    for (i in seq_along(pp)) {
      out <- gsub(paste0("{col", i, "}"), pp[i], out, fixed = TRUE)
    }
    leaf <- if (!is.null(col) && length(pp)) pp[length(pp)]
            else if (is.null(col) && length(pp)) pp[length(pp)]
            else (col %||% "")
    out <- gsub("{col}", leaf, out, fixed = TRUE)
    say <- function(v, tok) {
      if (is.null(v) || all(is.na(v))) {
        miss(tok, where)
        return("NA")
      }
      format(v, trim = TRUE)
    }
    for (nm in names(toks)) {
      v <- toks[[nm]]
      if (grepl(paste0("{", nm, ":sum}"), out, fixed = TRUE)) {
        out <- gsub(paste0("{", nm, ":sum}"),
                    say(summed(v, over), paste0(nm, ":sum")), out,
                    fixed = TRUE)
      }
      # `{n:<key>}` names ONE of the values, for a cell that has to say
      # a number belonging to a column it does not sit over.
      for (k in names(v) %||% character(0)) {
        tk <- paste0("{", nm, ":", k, "}")
        if (grepl(tk, out, fixed = TRUE)) {
          out <- gsub(tk, say(v[[k]], paste0(nm, ":", k)), out, fixed = TRUE)
        }
      }
      # `{n1}`, `{n2}`, ... the number at that depth of the keys: the
      # arm over an arm x sex column, whatever cell it is written in
      for (i in seq_along(pp)) {
        tk <- paste0("{", nm, i, "}")
        # a token of the user's own by that name is theirs
        if (paste0(nm, i) %in% names(toks) ||
            !grepl(tk, out, fixed = TRUE)) next
        val <- if (i <= depth) lookup(v, paste(pp[seq_len(i)],
                                               collapse = sep)) else NA
        out <- gsub(tk, say(val, paste0(nm, i)), out, fixed = TRUE)
      }
      # `{n}`: the number of what the cell stands for -- its column, or
      # the level a spanner's columns agree on, or the whole table
      tk <- paste0("{", nm, "}")
      if (grepl(tk, out, fixed = TRUE)) {
        key <- if (depth > 0L) paste(pp[seq_len(depth)], collapse = sep)
        out <- gsub(tk, say(lookup(v, key, all_cols), nm), out,
                    fixed = TRUE)
      }
    }
    out
  }
  # which spread column a cell sits over, when it sits over just one
  cell_col <- function(pos) {
    if (!is.numeric(pos) || length(pos) != 1L) return(NULL)
    i <- as.integer(pos) - n_lead
    if (i >= 1L && i <= length(cols)) cols[i] else NULL
  }
  # every spread column a cell covers, for `{n:sum}`
  cell_cols <- function(pos) {
    if (is.character(pos)) return(intersect(pos, cols))
    if (!is.numeric(pos) || !length(pos)) return(character(0))
    p <- as.integer(pos)
    rng <- if (length(p) >= 2L) seq(p[1L], p[2L]) else p[1L]
    i <- rng - n_lead
    cols[i[i >= 1L & i <= length(cols)]]
  }
  total <- n_lead + length(cols)
  fix <- function(r) {
    if (is.character(r)) {
      if (is.null(names(r)) && length(r) >= 1L && length(r) < total) {
        lead <- r[-length(r)]
        r <- c(lead, rep("", n_lead - length(lead)),
               rep(r[length(r)], length(cols)))
      }
      for (i in seq_along(r)) {
        r[i] <- one(r[i], if (i > n_lead) cols[i - n_lead] else NULL)
      }
      return(r)
    }
    if (is.list(r)) {
      return(lapply(r, function(cc) {
        if (inherits(cc, "rtf_col_cell")) {
          cc$label <- one(cc$label, cell_col(cc$pos),
                          cell_cols(cc$pos))
        }
        cc
      }))
    }
    r
  }
  out <- lapply(h, fix)
  if (inherits(h, "rtf_col_header")) class(out) <- class(h)
  if (length(missed)) .plan_n_warn(unique(missed), toks)
  out
}

# One warning for every header number that could not be had, with the
# reason the ARD gave and the way to supply it.  The header still gets
# built -- with NA where the number would be -- so the table can be
# looked at; a guessed number would look exactly like a right one.
.plan_n_warn <- function(missed, toks) {
  why <- unique(unlist(lapply(toks, function(v) {
    w <- attr(v, "why", exact = TRUE)
    if (length(w)) paste0(ifelse(names(w) == ".all", "", paste0(names(w), ": ")),
                          w)
  })))
  show <- utils::head(missed, 6L)
  more <- length(missed) - length(show)
  msg <- c(
    "Column header: a number could not be read from the ARD and is printed as NA.",
    paste0("  ", show),
    if (more > 0L) sprintf("  ... and %d more", more),
    if (length(why)) c("  Why:", paste0("    ", utils::head(why, 6L))),
    paste0("  An ARD states a column's population only as a sentinel's N ",
           "(ard_stack_hierarchical),"),
    paste0("  the column variable's own tabulation (ard_stack(.by = )), ",
           "or a denominator several"),
    "  variables agree on.  Otherwise give the numbers yourself:",
    paste0("    plan_col_header(n = c(\"Placebo\" = 86, ...))  -- names at any ",
           "level (\"Placebo\", \"Placebo____F\"),"),
    "    or n = function(data) ... , or n = ard_pull(ard, cols, variable = \"AGE\").")
  warning(paste(msg, collapse = "\n"), call. = FALSE)
}

# The cells map, looked up WITHOUT parsing: `.ard_lookup_cells()` returns the
# parsed entry, and a plan has to rewrite the template text before the engine
# ever sees it.
.plan_lookup_raw <- function(cells, variable, context, kind) {
  if (!is.list(cells) || !any(nzchar(names(cells) %||% ""))) return(NULL)
  # the cells map wants the FIRST match, not every candidate: a template
  # is one recipe, and specificity picks it outright
  hit <- .plan_pick(cells, variable, context, kind)
  if (length(hit)) hit[[1L]] else NULL
}

# Last-wins across layers has already happened; what is left is specificity,
# the same order `.ard_lookup_cells()` uses, so a key set for a variable beats
# one set for its kind.
# Every declaration that could apply, MOST SPECIFIC FIRST.  It used to
# return only the first, which was enough when a declaration was one
# number; now that it can name statistics, a narrower one may answer
# for `mean` and a wider one for `sd`.
.plan_pick <- function(map, variable, context, kind) {
  if (!length(map)) return(list())
  keys <- variable
  if (!is.null(context) && !is.na(context)) {
    keys <- c(keys, .ard_context_aliases(context))
  }
  if (!is.null(kind) && !is.na(kind)) {
    keys <- c(keys, .ard_context_aliases(kind))
  }
  out <- list()
  for (k in c(unique(keys), "default")) {
    if (!is.na(k) && k %in% names(map)) out[[length(out) + 1L]] <- map[[k]]
  }
  out
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
#' and prints a runnable [rtf_plan()] pipeline, filled in with the keys,
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
#' @param spec When `TRUE`, the generated plan reads its definition from a
#'   workbook (`rtf_plan(spec = read_table_spec(...))`) instead of inlining
#'   the cells.
#'
#' @return The generated code, as a character vector, invisibly.
#'
#' @section Lifecycle:
#' **Spike.**  See [rtf_plan()].
#'
#' @examples
#' if (requireNamespace("cards", quietly = TRUE)) {
#'   ard <- cards::ard_stack(
#'     cards::ADSL, .by = ARM,
#'     cards::ard_continuous(variables = AGE),
#'     cards::ard_categorical(variables = SEX))
#'   plan_template(ard, cols = "ARM")
#' }
#' @seealso [rtf_plan()], [apply_plan()], [ard_template()]
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
    "#  ard_normalize() runs; the plan does not, until it is asked.",
    "#  print(p) says how far it will go, and what its columns are called.",
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
    if (length(hierarchy)) paste0("hierarchy = ", vecq(hierarchy))
    else NULL,
    if (f$overall) "overall   = \"Any event\"" else NULL)

  spread <- c(
    paste0("cols = ", vecq(f$cols)),
    if (length(f$row_parts))
      paste0("rows = c(", paste(f$row_parts, collapse = ", "), ")")
    else NULL,
    if (length(hierarchy) > 1L)
      paste0("label = c(label = ", q(utils::tail(hierarchy, 1L)), ")")
    else NULL)

  # Flattening is RUN, not declared: the roles below name columns of
  # its result, so they can be looked at before they are named.
  L <- c(L,
         .ard_bar("1. the ARD half"),
         paste0("p <- ard ", op),
         if (length(norm)) .plan_call("ard_normalize", norm, op)
         else paste0("  rtfreporter::ard_normalize() ", op))
  if (isTRUE(spec)) {
    L <- c(L, .plan_call("rtf_plan",
                         c(spread,
                           paste0("spec = rtfreporter::read_table_spec(",
                                  "\"ard-spec.xlsx\")")),
                         op))
  } else {
    L <- c(L, .plan_call("rtf_plan", spread, op))
    # already indented and comma-ed: a `c(` entry spans several lines,
    # so it cannot go through .plan_call(), which commas every argument.
    L <- c(L, "  rtfreporter::plan_cells(", .plan_cell_lines(f),
           paste0("  ) ", op))
  }

  # -- 2. the display half -----------------------------------------------
  L <- c(L, "", .ard_bar("2. the display half -- edit this"))
  n_ok <- !is.null(tryCatch(ard_pull(ard, cols = f$cols),
                            error = function(e) NULL))
  # `plan_stub()` rather than plan_rtf(stub_vars = ): the plan then sees the
  # rows that will be printed, which plan_cell_style() needs.
  # `vars` is left out on purpose: plan_stub() works it out from
  # rtf_plan(rows = , label = ) less any plan_row_group(col = ).
  L <- c(L, .plan_call("plan_stub",
                       "into = \"row_label\"", op))
  # One concern per line.  Delete the ones this report does not want;
  # none of them has to be read in order to change another.
  L <- c(L,
         paste0("  rtfreporter::plan_row_group(mode = \"indent\") ", op),
         paste0("  rtfreporter::plan_blanks(\"between_groups\") ", op),
         paste0("  rtfreporter::plan_paginate_rows(max_rows = 22, ",
                "split = \"group_safe\") ", op),
         paste0("  rtfreporter::plan_style(border = \"tfl\") ", op))
  if (n_ok && length(f$cols) == 1L) {
    L <- c(L, .plan_call(
      "plan_col_header",
      c("n      = TRUE",
        paste0("header = function(n) c(\"Characteristic\", ",
               "paste0(names(n), \"\\nN = \", ",
               "as.integer(n)))")), op, last = TRUE))
  } else {
    # fixed: the base pipe is " |>", and "|" is alternation in a regex
    L[length(L)] <- sub(paste0(" ", op), "", L[length(L)], fixed = TRUE)
    L <- c(L,
           "# Several column keys: as_rtftables(header_sep = ) rebuilds the",
           "# spanning header from the \"____\" in the names, so plan_col_header()",
           "# is only needed for text the data does not carry.")
  }

  L <- c(L, "",
         "# rtf_tables() takes the plan directly -- apply_plan() is only for",
         "# looking inside:",
         "#",
         "#   doc <- rtf_document() |>",
         "#     rtf_section(page = 1, secinfo = <your header / footer>) |>",
         "#     rtf_tables(p)",
         "#",
         "#   print(p)        what it declares, and how far it goes",
         "#   apply_plan(p)   the object itself")

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

# ---------------------------------------------------------------------------
#  A plan written as code, back to a workbook
# ---------------------------------------------------------------------------
#
#  The inverse of .plan_from_spec(): every layer the workbook can say is
#  written to its sheet, read from what the plan RESOLVES to rather than
#  from how it was typed, so two plans that mean the same thing give the
#  same workbook.  What a sheet cannot say -- a function, a guarded label,
#  a positional list that no name reproduces -- is named, not dropped in
#  silence, and the workbook is run back through rtf_plan(spec = ) against
#  the plan's own data to say whether it gives the same pages.

#' Write a plan as a table definition workbook
#'
#' @description
#' `as_table_spec()` turns an [rtf_plan()] --- typically one a report
#' already has as code --- into a [table_spec()], the definition
#' [write_table_spec()] writes as an Excel workbook.  It is how an existing
#' report becomes the **template for a new study**: write the workbook,
#' edit its labels, levels and output ids, and read it back with
#' `rtf_plan(data, spec = read_table_spec(path, output_id = ))`.
#'
#' Everything is read from what the plan **resolves to**, against its own
#' data: the roles, levels, labels and cell templates (digits written in),
#' the pages, groups, blank rows and stub, the column widths **by name**
#' (`.values` when every value column shares one), and the column header
#' --- with a literal that is a column's own key value turned back into
#' `{col}` / `{col1}`, a repeated per-column cell into `span = each`, and
#' one spanner per arm into `span = <key>`, so the header keeps up with a
#' study that has a different number of arms or time points.
#'
#' What a workbook cannot say is **listed, not dropped**: a `plan_after()`
#' step (except `set_decimal_split()`, which becomes
#' `columns$decimal_split` on the value columns), a guarded label, a
#' column-scoped `labels` entry, `plan_cell_style()`, a literal `n`.  The
#' result is then run back through `rtf_plan(spec = )` on the plan's data,
#' and whether it gives **the same pages** is reported.
#'
#' @param x An [rtf_plan()], a **named list** of them (the names are the
#'   output ids; one workbook for the study), or anything [table_spec()]
#'   takes.
#' @param output_id The report the rows belong to.  `NULL` writes them as
#'   defaults (blank `output_id`).
#' @param check `TRUE` (default) rebuilds the pages from the workbook and
#'   compares them with the plan's.
#'
#' @return A [table_spec()], with attributes `"not_converted"` (what the
#'   workbook could not carry) and `"same_pages"` (`TRUE` / `FALSE`, or
#'   `NA` when not checked).
#'
#' @section Lifecycle:
#' **Spike.**  See [rtf_plan()].
#'
#' @examples
#' \dontrun{
#' p <- ard |> ard_normalize() |> rtf_plan(cols = "TRT01P") |> ...
#' as_table_spec(p, output_id = "T14-1-1") |> write_table_spec("study.xlsx")
#'
#' # a whole study at once
#' as_table_spec(list(DM = p_dm, AE = p_ae)) |> write_table_spec("study.xlsx")
#' }
#' @seealso [table_spec()], [write_table_spec()], [rtf_plan()]
#' @export
as_table_spec <- function(x, output_id = NULL, check = TRUE) {
  if (inherits(x, "table_spec")) return(x)
  if (is.list(x) && !inherits(x, "rtf_plan") && length(x) &&
      all(vapply(x, inherits, NA, "rtf_plan"))) {
    ids <- names(x)
    if (is.null(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
      .ard_stop(paste0("A list of plans needs unique names: they are the ",
                       "output ids of the workbook."))
    }
    parts <- lapply(ids, function(id) as_table_spec(x[[id]], id, check))
    rnd <- unique(stats::na.omit(vapply(parts, function(s)
      .ard_spec_study_value(s, "rounding"), "")))
    if (length(rnd) > 1L) {
      .ard_stop(paste0("The plans round differently (",
                       paste(rnd, collapse = " / "), "); a study has one ",
                       "rounding.  Make them agree first."))
    }
    sheets <- setdiff(names(.ard_spec_schema()), character())
    out <- lapply(sheets, function(s) do.call(rbind, lapply(parts, `[[`, s)))
    names(out) <- sheets
    out$study <- if (length(rnd)) c(rounding = rnd) else NULL
    sp <- table_spec(out)
    attr(sp, "not_converted") <- unlist(lapply(seq_along(parts), function(i)
      if (length(attr(parts[[i]], "not_converted")))
        paste0(ids[i], ": ", attr(parts[[i]], "not_converted"))))
    attr(sp, "same_pages") <- stats::setNames(
      vapply(parts, function(s) attr(s, "same_pages") %||% NA, NA), ids)
    return(sp)
  }
  if (!inherits(x, "rtf_plan")) return(table_spec(x))
  .plan_to_spec(x, output_id, check)
}

.plan_to_spec <- function(p, output_id, check) {
  id <- if (is.null(output_id)) NA_character_ else output_id
  lost <- character()
  miss <- function(...) lost <<- c(lost, sprintf(...))
  q <- function(x) if (grepl("^\\s|\\s$", x)) paste0("\"", x, "\"") else x
  bar <- function(x) paste(x, collapse = " | ")

  a <- suppressMessages(apply_plan(p, "args"))
  s <- a$spread
  pages <- suppressMessages(apply_plan(p, "pages"))
  seen <- p$cache[["pre_cols"]]
  first <- if (inherits(pages, "rtftable")) pages else pages[[1L]]
  pnames <- seen$names %||% names(first$data)
  spread <- seen$spread %||% intersect(.plan_spread_cols(p, first$data),
                                       pnames)
  seen$h <- p$cache[["header_raw"]]

  for (r in intersect(c("variable", "stat_name", "stat"), names(p$roles))) {
    miss("rtf_plan(%s = ): a column rename stays in code", r)
  }

  # -- tables ---------------------------------------------------------------
  ref <- function(v, nm) {
    if (inherits(v, "formula")) {
      if (length(v) == 3L) return(NA_character_)
      v <- paste0("\"", eval(v[[2L]], environment(v)), "\"")
    } else v <- as.character(v)
    if (nzchar(nm) && !identical(nm, v)) paste(nm, "=", v) else v
  }
  refs <- function(x, what) {
    if (is.null(x)) return(NA_character_)
    nms <- names(x) %||% rep("", length(x))
    out <- vapply(seq_along(x), function(i) ref(x[[i]], nms[i]), "")
    if (anyNA(out)) {
      miss("%s: a guarded (condition ~ template) element stays in code", what)
      out <- out[!is.na(out)]
    }
    if (length(out)) bar(out) else NA_character_
  }
  lab <- if (!"label" %in% names(s)) NA_character_
    else if (is.null(s$label)) "NULL"
    else if (length(s$label) == 1L && !is.list(s$label) && is.na(s$label)) "NA"
    else if (identical(unname(s$label), ".label") && is.null(names(s$label)))
      NA_character_
    else refs(s$label, "label")
  srt <- s$sort
  tables <- data.frame(
    output_id = id,
    cols = bar(unlist(s$cols)),
    rows = refs(s$rows, "rows"),
    label = lab,
    stats = s$stats %||% NA_character_,
    value = s$value %||% NA_character_,
    sep = s$sep %||% NA_character_,
    sort = if (is.null(srt)) NA_character_
           else if (is.logical(srt)) as.character(srt) else bar(srt),
    sort_stat = s$sort_stat %||% NA_character_,
    na = if (is.null(s$na) || is.na(s$na)) NA_character_ else q(s$na),
    stringsAsFactors = FALSE)

  # -- variables --------------------------------------------------------------
  lbl <- s$labels
  if (is.list(lbl)) {
    plain <- vapply(lbl, function(v) is.character(v) && length(v) == 1L &&
                      is.null(names(v)), NA)
    if (any(!plain)) {
      miss("labels: a column-scoped entry stays in code (plan_labels())")
    }
    lbl <- unlist(lbl[plain])
  }
  vars <- unique(c(names(lbl), names(s$levels)))
  variables <- data.frame(
    output_id = rep(id, length(vars)), variable = vars,
    label = unname(ifelse(vars %in% names(lbl), lbl[vars], NA_character_)),
    order = ifelse(vars %in% names(lbl), match(vars, names(lbl)), NA),
    levels = vapply(vars, function(v)
      if (is.null(s$levels[[v]])) NA_character_ else bar(s$levels[[v]]), ""),
    stringsAsFactors = FALSE)

  # -- cells ------------------------------------------------------------------
  crow <- function(var, ctx, row, when, tpl, digits = NA, signif = NA)
    data.frame(output_id = id, variable = var, context = ctx, row = row,
               when = when, template = tpl, digits = digits,
               signif = signif, stringsAsFactors = FALSE)
  cl <- list()
  cm <- s$cells
  if (!is.null(cm) && !identical(s$stats, "rows")) {
    is_map <- is.list(cm) && !inherits(cm, "ard_cells") &&
      any(nzchar(names(cm) %||% ""))
    if (!is_map) cm <- list(default = cm)
    for (k in names(cm)) {
      var <- if (identical(k, "default")) NA_character_ else
        sub("\r.*$", "", k)
      ctx <- if (grepl("\r", k, fixed = TRUE)) sub("^.*\r", "", k) else
        NA_character_
      e <- .ard_cell_entry(cm[[k]])
      for (i in seq_along(e$chains)) {
        rw <- if (is.null(e$labels) || !nzchar(e$labels[i])) NA_character_
              else e$labels[i]
        for (el in e$chains[[i]]) {
          wh <- if (is.null(el$cond)) NA_character_
                else paste(deparse(el$cond), collapse = " ")
          cl[[length(cl) + 1L]] <- crow(var, ctx, rw, wh, el$tpl)
        }
      }
    }
  }
  fm <- .plan_merge(.plan_of(p, "fmt"))
  if (length(fm)) {
    if (!identical(fm$by, .plan_label_name(p)) || !is.list(fm$formats)) {
      miss("plan_fmt(): only by = <label column> with formats = converts")
    } else {
      if (!is.null(fm$cols) && !identical(sort(as.character(
          if (is.numeric(fm$cols)) names(first$data)[fm$cols] else fm$cols)),
          sort(spread))) {
        miss("plan_fmt(cols = ): taken as the value columns")
      }
      for (st in names(fm$formats)) {
        f <- fm$formats[[st]]
        cl[[length(cl) + 1L]] <- crow(NA, NA, st, NA, NA,
          if (!is.null(f$digits)) as.character(f$digits) else NA,
          if (!is.null(f$signif)) as.character(f$signif) else NA)
      }
      for (o in setdiff(names(fm), c("by", "formats", "cols"))) {
        miss("plan_fmt(%s = ) stays in code", o)
      }
    }
  }
  cells <- if (length(cl)) do.call(rbind, cl) else NULL

  # -- layout -----------------------------------------------------------------
  lay <- list(output_id = id)
  put <- function(nm, v) {
    if (is.null(v)) return(invisible())
    lay[[nm]] <<- if (is.logical(v) && length(v) == 1L) as.character(v)
                  else if (is.character(v) && length(v) == 1L) q(v)
                  else bar(v)
  }
  st <- .plan_merge(.plan_of(p, "stub"))
  if (length(st)) {
    put("stub_vars", st$vars); put("stub_into", st$label)
    put("stub_indent", st$indent); put("stub_summary", st$group_summary)
    if (isTRUE(st$before)) put("stub_before", TRUE)
  }
  g <- .plan_merge(.plan_of(p, "group"))
  put("group_col", g$group_col); put("group_mode", g$group_by)
  put("group_collapse", g$collapse_repeats)
  if (isTRUE(g$.page)) put("group_page", TRUE)
  if (identical(g$.show, FALSE)) put("group_show", FALSE)
  b <- .plan_merge(.plan_of(p, "blanks"))
  if (!is.null(b$blank_rows) && !(is.character(b$blank_rows) &&
                                   length(b$blank_rows) == 1L)) {
    miss("plan_blanks(where = ): only a named rule (\"between_groups\") converts")
  } else put("blank_where", b$blank_rows)
  put("blank_first", b$blank_row_first); put("blank_last", b$blank_row_end)
  put("blank_counted", b$count_blank_rows)
  pg <- .plan_merge(.plan_of(p, "pages"))
  put("pages_max_rows", pg$max_rows); put("pages_split", pg$split)
  put("pages_by", pg$page_by); put("pages_min_group_rows", pg$min_group_rows)
  put("pages_cont_label", pg$cont_label)
  if (!is.null(pg$split_rows)) miss("plan_paginate_rows(break_before = ) stays in code")
  cp <- .plan_merge(.plan_of(p, "colpages"))
  put("colpages_every", cp$every); put("colpages_at", cp$at)
  put("colpages_carry", cp$carry); put("colpages_order", cp$page_order)
  for (o in intersect(c("cols", "by", "col_header", "width",
                        "allow_span_break"), names(cp))) {
    miss("plan_paginate_cols(%s = ) stays in code", o)
  }
  layout <- if (length(lay) > 1L) as.data.frame(lay, stringsAsFactors = FALSE)

  # -- style and columns ------------------------------------------------------
  sty <- .plan_merge(.plan_of(p, "style"))
  style <- list(output_id = id)
  for (nm in names(sty)) {
    v <- sty[[nm]]
    if (nm %in% c("col_rel_width", "row_title")) next
    if (nm %in% names(.ard_spec_types$style)) {
      style[[nm]] <- if (is.logical(v)) as.character(v) else as.character(v)
    } else {
      miss("plan_style(%s = ) stays in code", nm)
    }
  }
  if (length(.plan_of(p, "styles"))) miss("plan_cell_style() stays in code")
  style <- if (length(style) > 1L) as.data.frame(style, stringsAsFactors = FALSE)

  colw <- rep(NA_real_, length(pnames)); names(colw) <- pnames
  w <- seen$widths %||% first$col_rel_width
  if (!is.null(sty$col_rel_width) && length(w) == length(pnames)) colw[] <- w
  sc <- .plan_merge(.plan_of(p, "columns"))
  if (length(sc$widths)) {
    for (k in names(sc$widths)) {
      if (identical(k, ".values")) colw[spread] <- sc$widths[[k]]
      else if (k %in% pnames) colw[[k]] <- sc$widths[[k]]
    }
  }
  rt <- sty$row_title
  rt <- if (is.null(rt)) character() else if (is.numeric(rt)) pnames[rt] else rt
  hide <- setdiff(a$rtf$drop_cols %||% character(),
                  if (identical(g$.show, FALSE))
                    c(g$group_col, .plan_group_col(p)) else NULL)
  dec <- sc$decimal %||% character()
  for (l in .plan_of(p, "after")) {
    for (f in l$steps) {
      if (any(grepl("set_decimal_split", deparse(f), fixed = TRUE))) {
        dec <- c(dec, ".values")
        miss("plan_after(set_decimal_split()): taken as the value columns")
      } else {
        miss("a plan_after() step stays in code")
      }
    }
  }
  one_w <- length(spread) && all(!is.na(colw[spread])) &&
    length(unique(colw[spread])) == 1L
  crows <- list()
  cadd <- function(col, width = NA, title = NA, dsplit = NA, hid = NA)
    crows[[length(crows) + 1L]] <<- data.frame(
      output_id = id, column = col, width = as.character(width),
      row_title = title, decimal_split = dsplit, hide = hid,
      stringsAsFactors = FALSE)
  for (nm in setdiff(pnames, if (one_w) spread)) {
    if (is.na(colw[[nm]]) && !nm %in% c(rt, dec, hide)) next
    cadd(nm, colw[[nm]], if (nm %in% rt) "TRUE" else NA,
         if (nm %in% dec) "TRUE" else NA)
  }
  if (one_w || ".values" %in% dec) {
    cadd(".values", if (one_w) colw[[spread[1L]]] else NA, NA,
         if (".values" %in% dec) "TRUE" else NA)
  }
  for (nm in hide) cadd(nm, hid = "TRUE")
  columns <- if (length(crows)) do.call(rbind, crows)

  # -- col_header -------------------------------------------------------------
  hd <- .plan_merge(.plan_of(p, "header"))
  col_header <- NULL
  if (inherits(hd$header, "plan_spec_col_header")) {
    col_header <- do.call(rbind, lapply(hd$header$cells, function(cc)
      data.frame(output_id = id, line = as.character(cc$line),
                 cols = cc$cols, span = cc$span %||% NA,
                 text = if (is.null(cc$text)) NA else q(cc$text),
                 align = cc$align %||% NA,
                 bold = if (is.null(cc$bold)) NA else as.character(cc$bold),
                 border_top = cc$border_top %||% NA,
                 border_bottom = cc$border_bottom %||% NA,
                 stringsAsFactors = FALSE)))
  } else if (!is.null(seen$h)) {
    col_header <- .plan_header_rows(seen$h, pnames, spread, p, id, q)
    if (!is.null(hd$n) && !isTRUE(hd$n)) {
      miss("plan_col_header(n = ): a literal N stays in code (use {n})")
    }
  }

  study <- if (!is.null(s$rounding)) c(rounding = s$rounding)
  sp <- table_spec(tables, variables, cells, study = study, layout = layout,
                   columns = columns, style = style, col_header = col_header)

  same <- NA
  if (isTRUE(check)) {
    back <- tryCatch(suppressMessages(apply_plan(
      rtf_plan(p$data, spec = sp, notes = FALSE), "pages")),
      error = function(e) e)
    same <- !inherits(back, "error") && isTRUE(all.equal(back, pages))
    if (inherits(back, "error")) {
      miss("the workbook does not run: %s", conditionMessage(back))
    }
  }
  if (length(lost) || isFALSE(same)) {
    message(sprintf(
      "as_table_spec()%s: %s\n%s",
      if (is.na(id)) "" else paste0(" [", id, "]"),
      if (isTRUE(same)) "the workbook gives the same pages as the plan"
      else if (isFALSE(same)) "the workbook does NOT give the same pages"
      else "not checked",
      if (length(lost)) paste0("  not converted:\n",
                               paste0("    - ", unique(lost), collapse = "\n"))
      else ""))
  }
  attr(sp, "not_converted") <- unique(lost)
  attr(sp, "same_pages") <- same
  sp
}

# A resolved header, row by row, as `col_header` cells.  Literal text
# that is a column's own key value becomes the token; a cell repeated on
# every value column becomes `span = each`; a spanner per value of a key
# becomes `span = <key>`.
.plan_header_rows <- function(h, pnames, spread, p, id, q) {
  keys <- unname(as.character(unlist(p$roles$cols)))
  sep <- p$roles$sep %||% "____"
  kv <- list()
  d <- p$data
  if (length(keys) && all(keys %in% names(d))) {
    combo <- unique(as.data.frame(lapply(d[keys], as.character),
                                  stringsAsFactors = FALSE))
    combo <- combo[stats::complete.cases(combo), , drop = FALSE]
    nm <- do.call(paste, c(unname(as.list(combo)), sep = sep))
    for (i in seq_along(nm)) kv[[nm[i]]] <- unlist(combo[i, ], use.names = FALSE)
  }
  kvals <- function(col) kv[[col]] %||% strsplit(col, sep, fixed = TRUE)[[1L]]
  n <- length(pnames); lead <- n - length(spread)
  side <- function(b, s) if (is.null(b) || is.null(b[[s]])) NA else b[[s]]$style
  # the token a literal stands for, over these value columns
  tokenize <- function(txt, cols) {
    if (!length(cols) || grepl("{", txt, fixed = TRUE)) return(txt)
    kvs <- lapply(cols, kvals)
    if (length(cols) == 1L && identical(txt, cols)) return("{col}")
    k <- max(vapply(kvs, length, 1L))
    for (i in seq_len(k)) {
      v <- unique(vapply(kvs, function(z) if (i <= length(z)) z[i] else NA, ""))
      if (length(v) == 1L && identical(v, txt)) {
        return(if (length(cols) == 1L && i == length(kvs[[1L]])) "{col}"
               else paste0("{col", i, "}"))
      }
    }
    txt
  }
  out <- list()
  spanner <- NULL
  for (li in seq_along(h)) {
    r <- h[[li]]
    units <- list()
    if (is.character(r)) {
      if (length(r) < n) {
        ld <- r[-length(r)]
        r <- c(ld, rep("", lead - length(ld)), rep(r[length(r)], length(spread)))
      }
      for (i in seq_len(n)) units[[i]] <- list(from = i, to = i, label = r[i])
    } else {
      for (cc in r) {
        pos <- cc$pos
        if (is.character(pos)) pos <- match(pos, pnames)
        units[[length(units) + 1L]] <- list(
          from = min(pos), to = max(pos), label = cc$label %||% "",
          align = cc$align, bold = if (isTRUE(cc$bold)) TRUE else NULL,
          top = side(cc$border, "top"), bottom = side(cc$border, "bottom"))
      }
    }
    for (u in seq_along(units)) {
      cols <- intersect(pnames[units[[u]]$from:units[[u]]$to], spread)
      units[[u]]$label <- tokenize(units[[u]]$label, cols)
      units[[u]]$cols <- pnames[units[[u]]$from:units[[u]]$to]
    }
    sig <- function(u) paste(u$label, u$align %||% "", u$bold %||% "",
                             u$top %||% "", u$bottom %||% "", sep = "\r")
    done <- rep(FALSE, length(units))
    row <- function(cols, span, u) data.frame(
      output_id = id, line = as.character(li), cols = cols, span = span,
      text = if (nzchar(u$label)) q(u$label) else NA,
      align = u$align %||% NA, bold = if (is.null(u$bold)) NA else "TRUE",
      border_top = u$top %||% NA, border_bottom = u$bottom %||% NA,
      stringsAsFactors = FALSE)
    rows <- list()
    for (u in seq_along(units)) {
      if (done[u]) next
      same <- which(!done & vapply(units, sig, "") == sig(units[[u]]))
      on_values <- vapply(units[same], function(x)
        all(x$cols %in% spread), NA)
      grp <- same[on_values]
      if (u %in% grp && length(grp)) {
        covered <- unlist(lapply(units[grp], `[[`, "cols"))
        single <- all(vapply(units[grp], function(x) x$from == x$to, NA))
        if (setequal(covered, spread) && length(grp) == 1L && !single) {
          # one arm today is still an arm's spanner: a `{colK}` over a key
          # with a single value is `span = <key>`, so a study with three
          # arms gets three
          k <- suppressWarnings(as.integer(regmatches(units[[u]]$label,
                 regexec("[{]col([0-9]+)[}]", units[[u]]$label))[[1L]][2L]))
          sp_k <- if (!is.na(k) && k < length(keys) &&
                      length(unique(vapply(spread, function(cc)
                        kvals(cc)[k] %||% NA_character_, ""))) == 1L) keys[k]
                  else NA
          if (!is.na(sp_k)) spanner <- sp_k
          rows[[length(rows) + 1L]] <- row(".values", sp_k, units[[u]])
          done[grp] <- TRUE; next
        }
        if (single && setequal(covered, spread) && length(spread) > 1L) {
          rows[[length(rows) + 1L]] <- row(".values", "each", units[[u]])
          done[grp] <- TRUE; next
        }
        if (single && length(spread) == 1L && setequal(covered, spread)) {
          rows[[length(rows) + 1L]] <- row(".values", "each", units[[u]])
          done[grp] <- TRUE; next
        }
        if (setequal(covered, spread) && !single) {
          # one spanner per value of a key?
          for (ki in seq_along(keys)) {
            parts <- split(spread, vapply(spread, function(cc)
              kvals(cc)[ki] %||% NA_character_, ""))
            ranges <- lapply(units[grp], `[[`, "cols")
            if (length(parts) == length(ranges) &&
                all(vapply(ranges, function(rg) any(vapply(parts, function(pp)
                  setequal(pp, rg), NA)), NA))) {
              rows[[length(rows) + 1L]] <- row(".values", keys[ki], units[[u]])
              done[grp] <- TRUE
              break
            }
          }
          if (all(done[grp])) next
        }
        if (single) {
          # KEY = value: the columns sharing one key value
          hit <- NULL
          for (ki in seq_along(keys)) {
            vals <- unique(vapply(covered, function(cc) kvals(cc)[ki] %||% NA_character_, ""))
            if (length(vals) == 1L && !is.na(vals) &&
                setequal(covered, spread[vapply(spread, function(cc)
                  identical(kvals(cc)[ki], vals), NA)])) {
              hit <- paste(keys[ki], "=", vals); break
            }
          }
          if (!is.null(hit)) {
            rows[[length(rows) + 1L]] <- row(hit, "each", units[[u]])
            done[grp] <- TRUE; next
          }
        }
      }
      # otherwise the cell as it is, by name
      rows[[length(rows) + 1L]] <- row(paste(units[[u]]$cols, collapse = " | "),
                                       NA, units[[u]])
      done[u] <- TRUE
    }
    out <- c(out, rows)
  }
  res <- do.call(rbind, out)
  # a bordered blank under that spanner is the spanner's rule, per arm
  if (!is.null(res) && !is.null(spanner)) {
    rule <- res$cols == ".values" & is.na(res$span) & is.na(res$text) &
      (!is.na(res$border_top) | !is.na(res$border_bottom))
    res$span[rule] <- spanner
  }
  res
}

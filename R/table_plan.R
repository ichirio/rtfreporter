# ============================================================================
#  Tables from a cards / cardx ARD: the plan (deferred form)
# ----------------------------------------------------------------------------
#  Moved here from tflspec (plan E of the design discussion, #491).  See
#  R/ard_tables.R for the immediate form the plan resolves to.
# ============================================================================

# ============================================================================
#  A deferred, last-wins plan for the ARD half (#474)
# ============================================================================
#
#  The plan: the ARD conversion written as declarations that are resolved
#  once at the end.  Moved from rtfreporter (#474) with R/ard.R; the two
#  share internals and are one unit.
#
#
#  ---------------------------------------------------------------------------
#  What it is
#  ---------------------------------------------------------------------------
#
#  `normalize_ard()` and `widen_ard()` do the work the moment they are
#  called.  This is the same conversion written as DECLARATIONS that are
#  resolved once at the end:
#
#      ard |>
#        normalize_ard() |>                              # run, not declared
#        table_plan(cols = "TRT01P", rows = c(group = "variable")) |>
#        plan_cells(continuous = c("n"         = "{N:d}",
#                                  "Mean (SD)" = "{mean} ({sd})"),
#                   categorical = "{n} ({p:%})") |>
#        plan_digits(2) |>                # everything to 2 dp ...
#        plan_digits(AGE = 0) |>          # ... except AGE          <- LAST WINS
#        plan_apply()
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
#     Note this is a DIFFERENT rule from `widen_ard(cells = )`, which picks
#     by SPECIFICITY (variable, then context, then kind, then default) and
#     ignores order.  Both are defensible; the point of the spike is to find
#     out which one is nicer to write.  They do not conflict, because the plan
#     resolves its layers into a `cells` map and then hands it to the existing
#     engine -- specificity still decides what a variable with no layer of its
#     own gets.
#
#  2. THE ROLES ARE SAID ONCE, WHERE THE DATA IS.  `table_plan()` takes the
#     NORMALIZED frame and, like `ggplot(data, aes(x, y))`, the columns
#     that play a part in the table: `cols` across, `rows` down, `label`
#     for the row identity.  Every other verb reads them from there, so
#     the stub, the group carrier and the header's denominator are not
#     restated and cannot disagree.
#
#     Normalising is NOT deferred.  Nothing in the plan feeds it, nothing
#     overrides it later, and deferring it meant `table_plan()` had to GUESS
#     whether what it was handed still needed flattening -- a guess that
#     silently skipped the step for every report once already.  Run it,
#     look at it, then name its columns.
#
#     Beyond that the keys are the ones you already know: `plan_cells()`
#     takes exactly what `widen_ard(cells = )` takes, and the display
#     verbs take `as_rtftables()`'s own arguments.
#
#  ---------------------------------------------------------------------------
#  Why it resolves to arguments rather than re-implementing anything
#  ---------------------------------------------------------------------------
#
#  `plan_apply()` builds `widen_ard()`'s argument list and calls it.  Nothing about the conversion is duplicated, so the
#  plan cannot drift away from the immediate form -- and `plan_apply(stage =
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
    if (startsWith(nm, "plan_") || identical(nm, "table_plan")) return(cl)
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
    # A role nobody declared has no layer to blame: say where it goes.
    if (inherits(e, "rtfreporter_missing_role")) {
      .ard_stop(conditionMessage(e), "\n  Declare it with table_plan(",
                e$role, " = ).")
    }
    .ard_stop(paste0(conditionMessage(e), .plan_blame(plan, kinds)))
  })
}

.plan_layer <- function(plan, kind, fields) {
  if (!inherits(plan, "table_plan")) {
    .ard_stop("Expected a table_plan; pipe from table_plan(ard).")
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
                       "columns", "restyle",
                       "titles", "footnotes", "listing"))) "pages"
  # a finished table has no cells to make: plan_cells(na = ) on it is
  # what the pages print for a missing value
  else if (identical(plan$kind, "wide") && "cell_options" %in% kinds) "pages"
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

# What a stage left behind, remembered as it goes.  plan_apply() fills
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
# (an unnamed list, possibly holding formulas) or an cell_rows object.  Only
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
#   "normalized" already through normalize_ard(): our own .label / .kind
#   "long"       somebody's OWN long summary: stat_name + stat and no more
#   "wide"       one row per printed row: the ARD half has nothing to do
#
# The "long" case is the point of the classifier.  A statistician who
# summarised with dplyr has a frame with keys, a statistic name and a
# value, which is everything `widen_ard()` reads -- so the cell template
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

#' A deferred, last-wins plan for a table
#'
#' `table_plan()` starts a plan, and takes the **roles**: which column goes
#' across the table, which go down it, which carries the row identity.
#' This is `ggplot(data, aes(x, y))` --- the names must be columns of the
#' data you hand it, so you can check them by looking.  Every `plan_*()`
#' verb after it adds a declaration, and nothing runs until [plan_apply()].
#'
#' The rule is **last wins** --- a later layer overwrites what an earlier one
#' said about the same key --- so "set everything, then fix one variable" is a
#' two-line edit:
#'
#' ```r
#' plan_digits(2) |> plan_digits(AGE = 0)
#' ```
#'
#' This is tfrmt's `frmt_structure` rule.  Within one layer the keys are
#' picked by specificity, as [widen_ard()]'s `cells` are: an analysis
#' variable before a kind, a kind before the default.
#'
#' @param x What the table is built from.  A plan does **not** flatten:
#'   `cols` / `rows` / `label` name columns of what you hand it, so
#'   flatten first and look at the result.
#'   * a frame through [normalize_ard()] --- the ordinary case;
#'   * **any long frame of statistics**: keys, a `stat_name` and a `stat`,
#'     built with dplyr and no cards anywhere.  [plan_cells()] does the
#'     work;
#'   * a frame that is already the table, for the display half on its own;
#'   * a frame of subject records, with [plan_listing()].
#'
#'   A raw cards ARD is refused, with the line to write.
#' @param cols The key that goes **across** the table, as
#'   `widen_ard(cols = )` takes it: one or more columns, optionally
#'   renamed `c(new = old)`.
#' @param rows The keys that go **down** it, likewise.  Left out, the
#'   analysis variable is used (as `group`).
#' @param label The column carrying the **row identity** --- the text in
#'   the label column.  `".label"` by default (what [normalize_ard()]
#'   builds), `NA` for a table that has none, or a guarded template.
#' @param stat Which of **your** columns play the three parts [widen_ard()]
#'   reads by name, as a named vector: `variable` (the analysis variable,
#'   what [plan_cells()] and [plan_digits()] key on), `name` (which
#'   statistic a row is) and `value` (what it is worth) ---
#'   `stat = c(variable = "PARAMCD", name = "STAT", value = "AVAL")`.  A
#'   frame from \pkg{cards} already calls them `variable`, `stat_name` and
#'   `stat` and needs none of this; a summary somebody built with dplyr says
#'   so here, once, instead of being asked again by every verb.  Name only
#'   the parts your frame calls something else.
#'
#' @section Where the other settings went:
#' `table_plan()` takes the **roles** and nothing else.  Every other setting
#' belongs to the verb whose job it is:
#'
#' * a template per cell or a row per statistic, `stat` / `stat_fmt`, the empty-cell text: [plan_cells()] (`stats =`, `value =`, `na =`)
#' * the statistic a frequency order totals: [plan_sort()] (`stat =`)
#' * reporting what the templates did not use: [plan_cells()] (`notes =`)
#' * the separator several `cols` keys are joined with in the column names: [plan_columns()] (`sep =`)
#'
#' @return An object of class `table_plan`.
#'
#' @examples
#' if (requireNamespace("cards", quietly = TRUE)) {
#'   ard <- cards::ard_stack(
#'     cards::ADSL, .by = ARM,
#'     cards::ard_summary(variables = c(AGE, BMIBL)),
#'     cards::ard_tabulate(variables = SEX))
#'
#'   ard |>
#'     normalize_ard() |>
#'     table_plan(cols = "ARM", rows = c(group = "variable")) |>
#'     plan_cells(continuous  = c("Mean (SD)" = "{mean} ({sd})"),
#'                categorical = "{n} ({p:%})") |>
#'     plan_digits(2) |>
#'     plan_digits(AGE = 0, SEX = 1) |>
#'     plan_apply()
#' }
#' @seealso [plan_apply()], [normalize_ard()], [widen_ard()]
#' @export
table_plan <- function(x = NULL, cols = NULL, rows = NULL, label = NULL,
                       stat = NULL) {
  data <- x
  # Internally the three parts keep the names widen_ard() reads them by.
  parts <- .plan_stat_roles(stat)
  roles <- list(cols = cols, rows = rows, label = label,
                variable = parts[["variable"]], stat_name = parts[["name"]],
                stat = parts[["value"]])
  roles <- roles[!vapply(roles, is.null, logical(1L))]
  if (is.null(data)) {
    .ard_stop(paste0(
      "`data` is required.  The roles name its columns, and that ",
      "is what lets a typo\n  be caught here rather than three stages later.\n",
      "  A house style that serves every study is an ordinary ",
      "function:\n",
      "    my_dm <- function(d) table_plan(d, cols = ...) |> ",
      "plan_cells(...)"))
  }
  kind <- .plan_source_kind(data, roles)
  if (identical(kind, "ard")) {
    .ard_stop(paste0(
      "table_plan() takes the NORMALIZED frame, not a raw ARD, so that ",
      "`cols` / `rows` / `label`\n  name columns you can see.  ",
      "Flatten it first:\n",
      "    ard |>\n",
      "      normalize_ard() |>\n",
      "      table_plan(cols = ...)\n",
      "  normalize_ard() is what adds `.label`, `.kind` and `.depth`, ",
      "and names the\n  hierarchy levels -- which is what `rows` ",
      "and `label` then point at."))
  }
  .plan_check_roles(data, roles)
  p <- structure(list(data = data, kind = kind, roles = roles,
                      layers = list(),
                      cache = new.env(parent = emptyenv())),
                 class = "table_plan")
  p
}

# `stat = c(variable = , name = , value = )` -> its three parts, checked.
# One argument for the three because they are one question -- "what does
# your frame call the parts of a statistic" -- asked once.
.plan_stat_roles <- function(stat) {
  if (is.null(stat)) return(list())
  stat <- as.list(stat)
  nm <- names(stat)
  known <- c("variable", "name", "value")
  if (is.null(nm) || !all(nzchar(nm)) || !all(nm %in% known) ||
      anyDuplicated(nm)) {
    .ard_stop(paste0(
      "table_plan(stat = ) names the parts of a statistic your frame calls ",
      "something else:
",
      "    stat = c(variable = \"PARAMCD\", name = \"STAT\", value = \"AVAL\")
",
      "  Each part at most once, and only these three names."))
  }
  ok <- vapply(stat, function(v) is.character(v) && length(v) == 1L &&
                 !is.na(v) && nzchar(v), logical(1L))
  if (!all(ok)) {
    .ard_stop("table_plan(stat = ): each part is one column name.")
  }
  stat
}

# `a | b | c` -> c("a", "b", "c"): how a header cell lists its columns.
.plan_split_bar <- function(x) {
  if (is.null(x) || is.na(x)) return(character())
  v <- trimws(strsplit(x, "|", fixed = TRUE)[[1L]])
  v[nzchar(v)]
}

# One header cell, given as a row of a data frame (a workbook's row, or one
# written in R), to its typed values: `line` a whole number, `bold` TRUE /
# FALSE, the rest text (quoted to keep spaces; `\n` a line break).  NA says
# nothing, and nothing is what the cell then carries.
.plan_header_cell_types <- c(line = "int", cols = "text", span = "text",
                             text = "text", align = "text", bold = "bool",
                             border_top = "text", border_bottom = "text")

.plan_header_cell <- function(row) {
  out <- list()
  for (cn in names(.plan_header_cell_types)) {
    x <- if (cn %in% names(row)) row[[cn]] else NULL
    if (is.null(x) || is.na(x)) next
    bad <- function(what) .ard_stop(sprintf("`col_header$%s` must be %s; got %s.",
                                             cn, what, sQuote(x)))
    out[[cn]] <- switch(.plan_header_cell_types[[cn]],
      int = {
        v <- suppressWarnings(as.numeric(x))
        if (is.na(v) || v != round(v)) bad("a whole number")
        as.integer(v)
      },
      bool = if (is.logical(x)) x else {
        u <- toupper(trimws(x))
        if (u %in% c("TRUE", "YES", "Y", "1")) TRUE
        else if (u %in% c("FALSE", "NO", "N", "0")) FALSE
        else bad("TRUE or FALSE")
      },
      text = {
        x <- as.character(x)
        q <- regmatches(x, regexec("^([\"'])(.*)\\1$", x))[[1L]]
        if (length(q)) x <- q[3L]
        gsub("\\n", "\n", gsub("\r\n", "\n", x, fixed = TRUE), fixed = TRUE)
      })
  }
  out
}

# The header as cell rows, resolved against the page it heads: `cols` are
# names / `.values` / positions / `KEY = value`, `span` makes one cell,
# one per column, or one per value of a key.  What comes back is what a
# hand-written rtf_col_header() would have been, so the tokens and the
# rest of the header machinery see nothing new.
.plan_header_cells_resolve <- function(sh, page_names, spread, plan) {
  keys <- unname(as.character(unlist(plan$roles$cols)))
  sep <- .plan_sep(plan)
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
    for (it in .plan_split_bar(x)) {
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
        "  Give every printed column a width (`.values` covers the value ",
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
# be CHECKED there, so a typo blames table_plan() rather than the resolver.
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
      arg <- switch(r, variable = "stat = c(variable",
                    stat_name = "stat = c(name", stat = "stat = c(value", r)
      .ard_stop(sprintf(paste0(
        "table_plan(%s = ): no column %s in the data.\n",
        "  Columns: %s%s\n",
        "  A column you derive has to exist first: dplyr::mutate() ",
        "it before table_plan()."),
        arg, paste(sQuote(miss), collapse = ", "),
        paste(utils::head(nm, 12L), collapse = ", "),
        if (length(nm) > 12L) ", ..." else ""))
    }
  }
  invisible(TRUE)
}

#' @export
print.table_plan <- function(x, ...) {
  cat("<table_plan>  ",
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
  # The column names change three times -- normalize_ard() builds them,
  # widen_ard() replaces them, and folding the stub replaces them again
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
  say("after widen       -- for plan_stub / plan_paginate_group / plan_hide:",
      x$cache$table, "")
  say("as printed        -- for plan_cell_style / plan_style / header:",
      x$cache$printed, "")
  if (is.null(x$cache$table)) {
    cat("  after widen       -- not computed yet; run it once and this",
        " print fills in\n", sep = "")
  }
  # What a header cell may say.  The values come from the ARD and the
  # columns from the spread, so neither is visible in the call.
  tk <- tryCatch(.plan_header_tokens(x), error = function(e) NULL)
  if (!is.null(tk) && nrow(tk)) {
    cat("  header tokens     -- what a plan_col_header() cell may ",
        "carry:\n", sep = "")
    w <- max(nchar(tk$token))
    for (i in seq_len(nrow(tk))) {
      cat(sprintf("      %-*s  %s\n", w, tk$token[i], tk$text[i]))
    }
  }
  cat(if (identical(.plan_reach(x), "pages"))
        "  rtf_tables(doc, x) renders it"
      else "  plan_apply(x) returns it",
      ";  plan_apply(x, \"args\") shows the call\n", sep = "")
  invisible(x)
}


# -- the verbs ---------------------------------------------------------------

#' Declare the table, one layer at a time
#'
#' Each verb adds a layer to an [table_plan()].  **A later layer wins.**
#' The roles --- which column goes across, which go down, which carries
#' the row identity --- are said once, on [table_plan()]; these verbs are
#' the things a report really does declare twice.  Their arguments are
#' the ones [widen_ard()] and [as_rtftables()] already take, so the
#' layering is the only new idea.
#'
#' @param plan An [table_plan()].
#' @param ... For `plan_cells()`, exactly what `widen_ard(cells = )`
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
#'   A **finished table** has no templates to fill --- a source that is
#'   already the table, or statistics laid out as rows
#'   (`plan_cells(stats = "rows")`) --- so there `plan_digits()` formats
#'   the numbers themselves, with [fmt_numeric()]: a key that names a
#'   **column** formats that column, and `.rows` formats the value columns
#'   by the text of the label column:
#'
#'   ```r
#'   plan_digits(.rows = c(N = 0, Mean = 1, SD = "3s"))
#'   ```
#'
#'   A key that reaches neither a variable nor a column is an error, not
#'   silence.
#'
#'   For `plan_nest()`, one entry per nested variable: its name, and the
#'   variable and level its rows go under,
#'   `plan_nest(RACESUB = c(RACE = "Asian"))`.  The level is matched as the
#'   table shows it (the ARD's level: a code list's label).  The nested
#'   rows follow that level's row, one indent step deeper (the stub's
#'   `indent`, default 4), in the parent's group; their own heading is
#'   dropped, their order and text stay `plan_levels()`' and
#'   `plan_labels()`'.  The rows key that carries the variable
#'   (`table_plan(rows = c(group = "variable"))`) is what is read.
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
#' @param .drop_empty For `plan_levels()`: variables whose levels **no record
#'   has** are not shown -- a level whose `n` is 0 (or missing) in every
#'   column, as an ARD made with a code list's full set of levels has
#'   (a factor's unused level, counted 0).  `NULL` (default): every level
#'   the data has is shown.  A level that some column counts stays; a
#'   variable summarised without `n` is untouched.  Several calls add up.
#' @param stats,value,na,notes For `plan_cells()`: how a cell is made,
#'   as [widen_ard()] takes them --- `stats = "cells"` (default) fills a
#'   template per cell and `"rows"` makes each statistic a row of its own;
#'   `value` is which of `stat` / `stat_fmt` a `{x}` reads; `na` what fills
#'   a cell no template could; `notes = FALSE` stops the report of the
#'   statistics no template used.  They hold however the plan is run, by
#'   [plan_apply()] or by `rtf_tables(doc, plan)`.  On a plan of a table
#'   that is already built (no statistics), `na` alone is taken, and is
#'   [as_rtftables()]'s `na`: what a missing value prints as.
#' @param vars,name,indent,group_summary For `plan_stub()`: the row keys to
#'   fold into one stub column and how, as [stub_cols()] takes them.
#'   `name` is the NAME the folded column gets (`stub_cols(label = )`),
#'   which is a different thing from `table_plan(label = )` --- the column
#'   whose VALUES are the row text.  `vars` is derived when left out.
#' @param before For `plan_stub()`: `FALSE` (default) folds the stub inside
#'   [as_rtftables()], after grouping and pagination have had their say.
#'   `TRUE` folds it first, with [stub_cols()], which is what
#'   `plan_cell_style()` needs --- only then can a condition see the rows that
#'   will be printed.  The two do **not** always give the same table:
#'   folded first, the row keys are gone before a page split or a row
#'   group could read them, so `plan_paginate_group(col = )` naming one of
#'   them no longer finds it.  That is why this is a choice and not
#'   worked out for you.
#' @param keep `FALSE` also hides the column the verb names: the page key
#'   for `plan_paginate_group()`, the sort keys for `plan_sort()`.  A column
#'   can be **needed and not wanted** --- the key a page break reads, a sort
#'   carrier --- and the verb that needs it is the one place that knows, so
#'   it says so there instead of the name being written again in a
#'   `plan_hide()`.  Names that are not columns (a statistic, `".depth"`)
#'   are ignored rather than refused.
#' @param stat For `plan_sort()`: the statistic totalled into `.sort_stat`
#'   for a frequency order (`widen_ard(sort_stat = )`), e.g. `stat = "n"`.
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
#'
#'   The rows are grouped by the outermost row key (`table_plan(rows = )`);
#'   folded into a stub, by the stub's headings.
#' @param group_col For `plan_row_group()`, on a plan of a table that is
#'   already built: `as_rtftables(group_col = )`, the column whose runs are
#'   the groups (what `split = "group_safe"`, `mode` and the blank rows
#'   between groups read), with the pages still cut by rows.  An ARD plan
#'   names it as its outermost row key (`table_plan(rows = )`) and refuses
#'   this; a page per value is [plan_paginate_group()].
#' @param col For `plan_paginate_group()`: the column whose value starts a
#'   new page, `as_rtftables()`'s `group_col` with `split = "by_value"`.  Left
#'   out, it is the outermost row key.  The page is **named** after
#'   the value, which is the line `rtf_tables(auto_section = TRUE)`
#'   cuts a section on --- so this verb decides what a section is.
#'   It is the only verb that makes a page per value.
#' @param where For `plan_cell_style()`: a one-sided formula over the
#'   table's columns choosing the rows, e.g. `where = ~ is.na(label)`.  For
#'   `plan_blanks()`: `as_rtftables(blank_rows = )`, or `"records"` for a
#'   blank row after each record of a listing.
#' @param first,last,counted For `plan_blanks()`: `as_rtftables()`'s
#'   `blank_row_first`, `blank_row_end` and
#'   `count_blank_rows`.
#' @param max_rows,split,break_before,min_group_rows,cont_label For
#'   `plan_paginate_rows()`: the row budget and what a page break may cut ---
#'   `as_rtftables()`'s `max_rows`, `split`, `split_rows`, `min_group_rows`
#'   and `cont_label`.  This is the **row** axis; a page per value (the
#'   **group** axis) is `plan_paginate_group()`, and the **column** axis
#'   is `plan_paginate_cols()`.
#' @param page_by For `plan_paginate_rows()`: `as_rtftables()`'s `page_by`
#'   -- the column(s) whose value partitions the body **first**, each value
#'   a page named after it; the row settings then apply **within** one
#'   partition (a period, a cohort), so a BY page can still be cut by a row
#'   budget.  `plan_paginate_group()` is the other way to page by a value:
#'   one page per value however long it is, with no row budget.  The BY
#'   column is printed unless hidden ([plan_hide()]).
#' @param every For `plan_paginate_cols()`: cut a block every this many
#'   columns, counting only the ones a block does not keep.  This is
#'   `at` without writing down how many columns one study had --- the
#'   plan is deferred, so it counts them when the table exists.  Give one
#'   of `at`, `cut_by` or `every`.
#' @param at,cut_by,col_header,fit,allow_span_break,order For
#'   `plan_paginate_cols()`, in [paginate_cols()]'s terms: `at` the columns
#'   to cut before; `cut_by` a list of column blocks (`paginate_cols(cols =
#'   )`), or a separator found in the column names / one key per column
#'   (`paginate_cols(by = )`); `col_header` what the header becomes; `fit`
#'   `TRUE` (every block's widths on page 1's scale, `width = "fill"`) or
#'   `FALSE` (each column keeps its width, `"keep"`); `order`
#'   `paginate_cols(page_order = )`, the order the three axes nest in,
#'   outermost first: `"group"`, `"rows"`, `"cols"`, or the shorthands
#'   `"across"` and `"down"`.  For `plan_paginate_cols()` `keep` is the
#'   columns every block repeats (`paginate_cols(carry = )`).
#' @param border,align_count_pct,font,font_size_half_points,row_height_twips,row_height_exact,header_row_height_twips,blank_row_height_twips,cell_padding_left_twips,cell_padding_right_twips,cell_valign,table_align,markup,blank_row_normalize
#'   For `plan_style()`: the settings of the **whole table**, by
#'   [rtftable()]'s and [as_rtftables()]'s names.  What a table has per
#'   column or per cell is another verb's --- [plan_columns()],
#'   [plan_cell_style()], [plan_col_header()], [plan_blanks()] --- so this
#'   list is the whole of it.  For `plan_cell_style()`, `border` is a
#'   column's border ([rtf_border()]).
#' @param border_header,border_spanning,border_body,border_first_row,border_last_row
#'   For `plan_style()`: the rules of one kind of row, by the names
#'   [rtf_table_style()] gives them.  Say the table's rules one way:
#'   `border = "tfl"`, or these.
#' @param table_width_twips,table_width_pct,table_width_pct_of_writable For
#'   `plan_style()`: the table's width, by [rtftable()]'s names (and
#'   [as_rtftables()]'s `table_width_twips`).
#' @param header_align,header_bold,header_italic For `plan_style()`: the
#'   whole header's default look, [rtf_table_style()]'s fields; with the
#'   `border_*` zones and `align` ... `underline` they make the
#'   `as_rtftables(style = )` object.  One column or one cell is
#'   [plan_cell_style()].
#' @param underline For `plan_style()`: the body's default, an
#'   [rtf_table_style()] field like `align`, `bold` and `italic` (which
#'   are that too in `plan_style()`).  For `plan_cell_style()`: the cells'
#'   underline, as `bold`.
#' @param indent_twips For `plan_cell_style()`: the left indent of the
#'   cells' text, [style_cols()]'s `indent_twips` (not on the header).
#' @param cell_format,column_widths_twips For `plan_columns()`:
#'   [as_rtftables()]'s `cell_format` (a formatter, or a list of them one a
#'   column) and [rtftable()]'s `column_widths_twips`, as they are.
#' @param header_sep,col_header_align For `plan_col_header()`:
#'   [as_rtftables()]'s `header_sep` (the separator a plain table's column
#'   names are split on into spanning header rows) and [rtftable()]'s
#'   `col_header_align`.
#' @param lines For `plan_col_header()`: the header **a row at a time**,
#'   as it reads, instead of `header`: a list, one element a header row
#'   (the top first), each a named character vector -- a cell's name the
#'   columns it sits on (a column name, `.values` for every value column,
#'   a position or range such as `3:5`, or `KEY = value`), its value the
#'   text, with the same tokens (`{col}`, `{n}`, `{n:sum}` ...):
#'
#'   ```r
#'   plan_col_header(lines = list(
#'     c(row_label = "",               .values = "{col}"),
#'     c(row_label = "Characteristic", .values = "(N={n})")))
#'   ```
#'
#'   It is the data frame of cells written another way (one row a cell:
#'   `line`, `cols`, `text`, `span`), so it does all that does.
#' @param span For `plan_col_header(lines = )`: how a cell over several
#'   columns is made -- `"each"` (the default: a cell a column), `"one"`
#'   (one cell over them all) or a key's name (a cell per value of that
#'   key: a spanner).  One value for every cell, or a list as `lines` is,
#'   each element the spans of that row's cells, named as its cells are
#'   (a cell not named there is `"each"`).
#' @param widths For `plan_columns()`: the relative column widths,
#'   `rtftable(col_rel_width = )`.  **Named by column** (`c(row_label = 5,
#'   .values = 2)`, `.values` for every value column) a reordered table keeps
#'   them, and that is what the `columns` sheet's `width` is; unnamed, they
#'   are one a column in order, as `col_rel_width` itself.
#' @param row_title,auto_width For `plan_columns()`: the row-heading
#'   columns (`rtftable(row_title = )`) and whether each column is sized to
#'   its content (`as_rtftables(auto_width = )`).
#' @param sep For `plan_columns()`: the separator several `cols` keys are
#'   joined with in the value columns' names, `"____"` by default ---
#'   `"Placebo____F"`.  The spanning header is built by splitting on it, so
#'   a key value that contains it is refused.  For `plan_listing()`,
#'   [listing_spec()]'s `sep`.
#' @param decimal For `plan_columns()`: the columns whose numbers line
#'   up at the decimal point (`.values` for every value column) --- the
#'   `columns` sheet's `decimal_split`.
#' @param type,spacer,spacer_rel_width,layout,wrap
#'   For `plan_listing()`: [listing_spec()]'s own arguments, unchanged.
#'   `...` there takes the [listing_col()]s.  A blank row between records
#'   is `plan_blanks(where = "records")`, one at the top of each page
#'   `plan_blanks(first = TRUE)`, and a column's alignment is its own
#'   (`listing_col(align = )`).
#' @param pages For `plan_titles()` / `plan_footnotes()`: a list with one
#'   block per page, when the pages do not share a block.  `...` is the rows
#'   of a single block used on every page; give one or the other, never
#'   both, because a three-row title on a three-page table cannot be told
#'   apart from three one-row titles.
#' @param cols For `plan_cell_style()`: the columns styled, by name
#'   (`.values` for every value column); left out, every column.
#' @param bold,italic,align,color,background For `plan_cell_style()`: how
#'   the cells look.  A **value** (`bold = TRUE`, `color = "#CC0000"`)
#'   applies to the cells `cols` / `header` / `where` choose.  A
#'   **one-sided formula** computes the value row by row over the table's
#'   columns, `NA` leaving the column default alone --- `bold = ~
#'   is.na(label)`, `color = list(Placebo = ~ ifelse(n > 50, "#CC0000",
#'   NA))`, a named list scoping it to columns.  Formula styles see the
#'   printed rows only with `plan_stub(before = TRUE)`.  For `plan_style()`,
#'   `align`, `bold` and `italic` are the body's default look,
#'   [rtf_table_style()]'s fields.
#' @param header For `plan_cell_style()`: `TRUE` styles the column header
#'   ([style_header()]).  For `plan_col_header()`: the header, built with the same
#'   [rtf_col_header()] as everywhere else --- or a **function** of the
#'   resolved `n` (and, with two arguments, the finished table) when it
#'   has to be computed --- or a **data frame of cells**, one row a cell,
#'   with the `col_header` sheet's columns (`line`, `cols`, `span`, `text`,
#'   borders ...), placed on each page's columns when it is made; this is
#'   how `tflspec::tfl_table_code()` writes a workbook's header.  The plan adds two things to a header it is
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
#'   plan_col_header(values = list(n = TRUE), rtf_col_header(
#'     c("",               "{col}"),
#'     c("Characteristic", "(N={n})")))
#'   ```
#'
#'   A row already the right length, and a cell with no token in it, are
#'   untouched --- so a spanner, a border or a cell that reads the
#'   finished table is written exactly as it always was.
#' @param values For `plan_col_header()`: what the header's `{tokens}`
#'   take.  The **population** each column describes --- its analysis set,
#'   the number a header prints as `(N=86)` --- is `values = list(n = TRUE)`
#'   (or just `TRUE`), read from the data, keyed by the same `cols` /
#'   `levels` `table_plan()` was given, **at every depth of the keys**: with
#'   `cols = c("TRT", "SEX")` both the arm (`"Placebo"`) and the arm x sex
#'   cell (`"Placebo____F"`) are looked up.  Only a number the ARD
#'   **states as a population size** is read:
#'   1. a **cards sentinel**'s `N` keyed by exactly those keys ---
#'      `..ard_hierarchical_overall..` from
#'      `cards::ard_stack_hierarchical(over_variables = TRUE)`, each arm's
#'      denominator; never its `n`, the subjects with an event that the
#'      "Any" row shows.  Taken only when there is one kind of sentinel;
#'   2. the column variable's **own tabulation** --- the per-arm `n` of
#'      `cards::ard_stack(.by = )`, which [normalize_ard()] keeps as
#'      `.key_own` rows --- where its counts add up to the `N` those rows
#'      state (the population split by arm, not the treatment counted as
#'      an event).  At depth *k* it is `cols[k]` tabulated within
#'      `cols[1..k-1]`: cards tabulates each `.by` variable on its own,
#'      so `ard_stack(.by = c(TRT, SEX))` states the arm but not the arm
#'      x sex cell --- bind `cards::ard_tabulate(adsl, by = TRT,
#'      variables = SEX)` to state that;
#'   3. an analysis summary's `N` only where it is a denominator by
#'      construction (a hierarchical summary, a percentage of the
#'      `denominator =` data) or where **two or more different variables
#'      agree on it for every column**.  One variable's `N` is the count
#'      of its **non-missing** values --- the arm size only if nothing is
#'      missing, which the ARD cannot show --- and an `N` per visit or per
#'      parameter is not a column's at all.  [pull_ard()] is not asked.
#'
#'   The study total (`..ard_total_n..`, the one [normalize_ard()]
#'   remembers as it drops that row, or the `N` the column variable's
#'   own tabulation states) is **never put in every column**: it answers
#'   a one-column table and a cell over all the columns.
#'
#'   **Pages split by a group value** ([plan_paginate_group()]: a lab
#'   parameter, a visit) have **two populations**, and which one the
#'   header says is the author's choice:
#'
#'   * `values = list(n = "page")` --- each page's own, the subjects with that test:
#'     the ARD rows **carrying** the page key, e.g.
#'     `cards::ard_tabulate(adlb, by = PARAM, variables = BGRADE)`,
#'     which states each baseline column's N and the page's total;
#'   * `list(n = "table")` --- the analysis set: the ARD rows **without** the
#'     page key, e.g. `cards::ard_total_n(adsl)` or the treatment
#'     tabulated from ADSL;
#'   * `list(n = "page", N = "table")` --- both, as `{n}` and `{N}`.
#'
#'   `n = TRUE` reads the page's rows, then the table's for what they lack,
#'   and **warns** when the ARD states both and they differ.  Neither is
#'   filled in from outside the ARD: a population it does not state is
#'   `NA`.  Keep the header consistent with the body --- the percentages
#'   are over the denominator cards used, so a header saying the analysis
#'   set wants an ARD built with that `denominator =`.  The workbook says
#'   the same on `tables$header_n`.
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
#'   To **give the numbers yourself**, `n` is a vector named by column
#'   key, at any depth --- `c(Placebo = 86, "Placebo____F" = 53, ...)` ---
#'   or a function of the data returning one; a single unnamed number
#'   fills every cell.  After a workbook (`tflspec::tfl_table_plan()`), a later
#'   `plan_col_header(values = ...)` supplies the numbers and keeps the
#'   workbook's header.
#'
#'   A **data frame** of per-page values is [set_col_header()]'s own
#'   `values =`: one row per page key, a column per `{token}`.
#'#'   A **function** of the data covers what neither can find, and a
#'   **named list** of either supplies several --- and then **each
#'   name is a token**, which is how one header says two numbers with
#'   no function at all: the study total in a spanner and each
#'   column's own underneath it.
#'
#'   ```r
#'   plan_col_header(
#'     values = list(n = TRUE, total = 254),
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
#'   run, as `widen_ard(rounding = )` takes it.  Last wins, like every
#'   other layer.
#' @param label,position For `plan_total()`: the heading of the **Total
#'   column** (`"Total"`) and where it goes among the column key's values
#'   (`"last"`, `"first"`).  Its cells are cards' own overall rows --- the
#'   statistics with no value of the column key, from the same analysis
#'   without its `by` (`cards::ard_tabulate(adsl, variables = RACE)` bound
#'   under `cards::ard_tabulate(adsl, by = ARM, variables = RACE)`, or
#'   `cards::ard_stack(.overall = TRUE)`) --- so the ARD keeps no `ARM`
#'   value the data does not have.  Its `{n}` in the column header is the
#'   study total the ARD states (the column key tabulated on its own, or
#'   `cards::ard_total_n()`).  One column key only.
#'
#' @return The plan, with one more layer.
#'
#' @section Where each argument goes:
#' Each verb has **one job**, and its arguments keep the names of the
#' function that does it.  This is what each one hands on.
#'
#' * `plan_cells(..., stats, value, na, notes)`: how a cell is made. Goes to [widen_ard()]: `cells`, `stats`, `value`, `na`, `notes`; a finished table [as_rtftables()]: `na`.
#' * `plan_digits(..., rounding)`: the digits. Goes to the open tokens of the templates; on a finished table [fmt_numeric()].
#' * `plan_levels()`, `plan_labels()`: the order and text of values. Goes to [widen_ard()]: `levels`, `labels`.
#' * `plan_total(label, position)`: a Total column from the ARD's overall rows. Goes to [widen_ard()]: those rows as one more value of the column key, and its place in `levels`; the header's `{n}` there is the study total.
#'   `plan_levels(.drop_empty = )` leaves out the levels no record has, before the table is made.
#' * `plan_sort(..., stat, keep)`: the row order. Goes to [widen_ard()]: `sort`, `sort_stat`; a finished table [as_rtftables()]: `sort_by`, `sort_desc` from `-name`.
#' * `plan_stub(vars, name, indent, group_summary, before)`: the row headings. Goes to [stub_cols()]: `vars`, `label`, `indent`, `group_summary`.
#' * `plan_cell_style(cols, header, where, bold, italic, align, color, background, border, underline, indent_twips)`: how cells look. Goes to [style_header()], [style_cols()], or [rtftable()]'s `cell_styles` for a condition.
#' * `plan_paginate_group(col, keep)`: a page per value. Goes to [as_rtftables()]: `split = "by_value"`, `group_col`; `keep = FALSE` adds it to `drop_cols`.
#' * `plan_row_group(mode, collapse, group_col)`: groups down the body. Goes to [as_rtftables()]: `group_by`, `collapse_repeats`, `group_col` (a finished table).
#' * `plan_hide(...)`: columns not printed. Goes to [as_rtftables()]: `drop_cols`.
#' * `plan_blanks(where, first, last, counted)`: blank rows. Goes to [as_rtftables()]: `blank_rows`, `blank_row_first`, `blank_row_end`, `count_blank_rows`; a listing's `where = "records"` is [listing_spec()]'s `blank_row`.
#' * `plan_paginate_rows(max_rows, split, break_before, min_group_rows, cont_label, page_by)`: the row budget, inside the BY pages. Goes to [as_rtftables()]: `max_rows`, `split`, `split_rows`, `min_group_rows`, `cont_label`, `page_by`.
#' * `plan_paginate_cols(at, cut_by, every, keep, col_header, fit, allow_span_break, order)`: column blocks. Goes to [paginate_cols()]: `at`, `cols` / `by`, `carry`, `col_header`, `width`, `allow_span_break`, `page_order`.
#' * `plan_style(border, ..., border_header, ..., header_bold, ..., table_width_twips, ...)`: the whole table. Goes to [rtftable()] / [as_rtftables()] by the same names; `border_*` and the default look (`header_align`, `header_bold`, `header_italic`, `align`, `bold`, `italic`, `underline`) via [rtf_table_style()].
#' * `plan_columns(widths, decimal, row_title, auto_width, sep, cell_format, column_widths_twips)`: the columns. Goes to [rtftable()]: `col_rel_width`, `row_title`, `column_widths_twips`; [set_decimal_split()]: `cols`; [as_rtftables()]: `auto_width`, `cell_format`; [widen_ard()]: `sep`.
#' * `plan_col_header(header, values, header_sep, col_header_align, lines, span)`: the column header. Goes to [set_col_header()]: the header (or `lines`, the header a row at a time) and a data frame of `values`; a population fills its `{n}` tokens; [as_rtftables()]: `header_sep`; [rtftable()]: `col_header_align`.
#' * `plan_listing(..., type, sep, spacer, spacer_rel_width, layout, wrap)`: a listing. Goes to [listing_spec()], the same names.
#' * `plan_titles()`, `plan_footnotes()`: the blocks above and below. Goes to [rtf_titles()], [rtf_footnotes()].
#' * `plan_after(...)`: anything else. Goes to your functions of the pages.
#'
#' `plan_columns()` declares the columns **of this table** --- widths,
#' alignment, the key separator --- and is not [rtf_columns()], which
#' addresses the columns of finished pages by their printed names.
#'
#' @section plan_after() is the way out, not the way in:
#'
#' `plan_after()` runs functions of your own on the finished pages.  It
#' is for what the plan cannot **declare** --- a step with no verb --- and
#' it is the only verb whose content the plan cannot read: a workbook
#' (`tflspec::tfl_as_table_spec()`) cannot carry it, and the columns it names are
#' positions on the pages, which a reordered table does not keep.  The usual
#' reasons to reach for it each have a declaration, which names columns and
#' goes into a workbook:
#'
#' * `set_decimal_split(x, cols = 3:31)` is declared as `plan_columns(decimal = ".values")`
#' * `paginate_cols(x, ...)` is declared as `plan_paginate_cols(every = , at = , keep = )`
#' * `rtftable(col_rel_width = )` by position is declared as `plan_columns(widths = c(Analyte = 3, .values = 2))`
#' * `set_col_header()` / `rtf_col_header()` is declared as `plan_col_header()`
#' * `realign_count_pct()` is declared as `plan_style(align_count_pct = TRUE)`
#' * `fmt_numeric()` is declared as `plan_digits(<column> = 2)`, `plan_digits(.rows = c(Mean = 1))`
#' * `paginate()` is declared as `plan_paginate_rows()`
#' * `style_header(x, ...)` is declared as `plan_cell_style(header = TRUE, ...)`
#' * `style_cols(x, ...)` is declared as `plan_cell_style(cols = , ...)`
#' * bold / colour / alignment of body cells, by condition is declared as `plan_cell_style(where = ~ ..., ...)`
#' * `style_zone(x, ...)` is declared as `plan_style(border_header = , border_body = , ...)`
#'
#' The styles `plan_cell_style()` declares run on the pages in the order
#' written (after the header and the decimal alignment, before any
#' `plan_after()` step).  Its `cols` may be column names, and `.values`
#' stands for every value column, so a reordered table keeps them.
#'
#' @name plan_verbs
#' @seealso [table_plan()], [plan_apply()]
NULL

# The order values appear in, and the text they appear as.  These are the
# two declarations a report really does make twice -- one order for
# everything, then one variable's own -- so they are layers, merged one
# KEY at a time: a later plan_levels() adds a variable without restating
# the rest.  The roles are said once, on table_plan(); these are not roles.
#' @rdname plan_verbs
#' @export
plan_levels <- function(plan, ..., .drop_empty = NULL) {
  v <- if (...length()) .plan_map(list(...), "plan_levels")
  if (!is.null(.drop_empty) &&
      (!is.character(.drop_empty) || anyNA(.drop_empty))) {
    .ard_stop("plan_levels(.drop_empty = ) takes variable names.")
  }
  .plan_layer(plan, "levels",
              c(if (!is.null(v)) list(levels = v),
                if (!is.null(.drop_empty)) list(drop_empty = .drop_empty)))
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

# The widen_ard() settings that say how a cell is made -- a template per
# cell or a row per statistic, which column a `{x}` reads, what an unfilled
# cell prints, and whether the statistics no template used are reported --
# are the cells' own, so they are declared here and not on table_plan(),
# which takes the roles and nothing else.  Declared on the plan, they hold
# however the plan is run: plan_apply(), rtf_tables(doc, plan).
#' @rdname plan_verbs
#' @export
plan_cells <- function(plan, ..., stats = NULL, value = NULL, na = NULL,
                       notes = NULL) {
  # the templates are a layer only when there are some: `plan_cells(notes =
  # FALSE)` alone says nothing about them
  p <- if (...length()) .plan_keyed(plan, "cells", list(...)) else plan
  opts <- list(stats = stats, value = value, na = na, notes = notes)
  if (all(vapply(opts, is.null, logical(1L)))) return(p)
  if (!is.null(stats)) stats <- match.arg(stats, c("cells", "rows"))
  if (!is.null(value)) value <- match.arg(value, c("stat", "stat_fmt"))
  if (!is.null(notes) && !(is.logical(notes) && length(notes) == 1L && !is.na(notes))) {
    .ard_stop("plan_cells(notes = ) is TRUE (report the statistics no template used) or FALSE.")
  }
  .plan_layer(p, "cell_options",
              list(stats = stats, value = value, na = na, notes = notes))
}


# `round` used to be a verb of its own, and it never earned one: the whole
# run takes ONE rounding family, so it is a setting of the digits rather
# than a layer beside them.
#' @rdname plan_verbs
#' @export
plan_digits <- function(plan, ..., rounding = NULL) {
  d <- list(...)
  p <- if (length(d) || is.null(rounding)) .plan_keyed(plan, "digits", d)
       else plan
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
#  and `plan_apply(stage = "pages")` calls them in the order a report is
#  built:
#
#      (dplyr)        a derived column or a filter is written before
#                     table_plan(), in the same sentence
#      plan_digits()  fmt_numeric()      on a finished table: a column by
#                     name, or `.rows` by the value of the label column
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
#      plan_style()   the whole table: borders, row heights, padding
#      plan_col_header()  set_col_header(), and the populations its `{n}`
#                     tokens take (`values`)
#      plan_titles()  the block ABOVE the table, on each page
#      plan_footnotes()  the block BELOW it
#      plan_columns() widths by column name, decimal alignment
#      plan_cell_style()  style_header() / style_cols(), or cell_styles
#                     for a condition, as declared
#      plan_after()   the way out: a step no verb above declares


# WHERE the stub is folded changes the answer, so it is a setting rather
# than a detail.  as_rtftables() folds it inside its own resolution, after
# grouping and pagination have had their say, and that is what a report
# grouped by a carrier column needs.  Folding it FIRST, with stub_cols(),
# is what plan_cell_style() needs, because only then can a condition see the
# rows that will be printed.
#' @rdname plan_verbs
#' @export
# `vars` is derivable and was being written twice: the row keys are the
# names of table_plan(rows = ), the label column is the name of its
# `label = `, and a grouping carrier is not part of the stub.  Left out,
# it is worked out from what has already been declared.
plan_stub <- function(plan, vars = NULL, name = NULL, indent = NULL,
                      group_summary = NULL, before = FALSE) {
  # `name` rather than `label`: table_plan(label = ) is the column whose
  # VALUES are the row text, and this is the NAME of the column the row
  # keys are folded into.
  .plan_layer(plan, "stub",
              list(vars = vars, name = name, indent = indent,
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
#
# The same verb also says WHERE without a condition: `header = TRUE` is the
# column header (style_header()), `cols =` alone is whole columns
# (style_cols()), and `where =` is a condition on the rows, over `cols` or
# every column.  One verb for "which cells, and how they look", instead of
# one per place.
#' @rdname plan_verbs
#' @export
plan_cell_style <- function(plan, cols = NULL, header = FALSE, where = NULL,
                            bold = NULL, italic = NULL, align = NULL,
                            color = NULL, background = NULL, border = NULL,
                            underline = NULL, indent_twips = NULL) {
  attrs <- list(bold = bold, italic = italic, align = align, color = color,
                background = background, border = border,
                underline = underline, indent_twips = indent_twips)
  attrs <- attrs[!vapply(attrs, is.null, logical(1L))]
  if (!length(attrs)) {
    .ard_stop(paste0(
      "plan_cell_style(): nothing to style -- give bold, italic, ",
      "underline, align, indent_twips, color, background or border."))
  }
  if (!is.null(where) && (!inherits(where, "formula") || length(where) != 2L)) {
    .ard_stop(paste0(
      "plan_cell_style(where = ) is a one-sided formula over the table's ",
      "columns,
  for example `where = ~ is.na(label)`."))
  }
  # An attribute given as a formula computes its value row by row; the
  # rest are values.  One call is one of the two, so the rows it applies
  # to are decided in one place.
  computed <- vapply(attrs, .plan_is_style_formula, logical(1L))
  if (any(computed)) {
    if (!all(computed) || !is.null(where) || isTRUE(header)) {
      .ard_stop(paste0(
        "plan_cell_style(): an attribute written as a formula computes its ",
        "own value row by row
  (`bold = ~ is.na(label)`), so it takes ",
        "neither `where` nor `header`, and the
  other attributes of the ",
        "call are formulas too.  Make it two calls."))
    }
    spec <- if (is.null(cols)) attrs else lapply(attrs, function(f)
      if (inherits(f, "formula")) stats::setNames(rep(list(f), length(cols)), cols)
      else f)
    return(.plan_keyed(plan, "styles", spec))
  }
  if (isTRUE(header)) {
    if (!is.null(where)) {
      .ard_stop(paste0("plan_cell_style(header = TRUE) styles the column ",
                       "header, which has no rows for `where` to choose."))
    }
    if (!is.null(attrs$color) || !is.null(attrs$background) ||
        !is.null(attrs$indent_twips)) {
      .ard_stop(paste0("plan_cell_style(header = TRUE) takes bold, italic, ",
                       "underline, align and border."))
    }
    return(.plan_layer(plan, "restyle",
                       list(fun = "style_header", args = c(list(cols = cols), attrs))))
  }
  if (is.null(where)) {
    return(.plan_layer(plan, "restyle",
                       list(fun = "style_cols", args = c(list(cols = cols), attrs))))
  }
  # A condition: each attribute becomes the computed form -- its value
  # where the rows match, the column default (NA) elsewhere -- scoped to
  # `cols` when they are given.
  if (!is.null(attrs$border)) {
    .ard_stop(paste0("plan_cell_style(where = ) takes bold, italic, ",
                     "underline, align, indent_twips, color and background;",
                     " a border is set on whole columns."))
  }
  cond <- where[[2L]]
  spec <- lapply(attrs, function(value) {
    f <- stats::as.formula(call("~", call("ifelse", cond, value, NA)),
                           env = environment(where))
    if (is.null(cols)) f else stats::setNames(rep(list(f), length(cols)), cols)
  })
  .plan_keyed(plan, "styles", spec)
}

# A computed style: a one-sided formula, or a list of them named by column.
.plan_is_style_formula <- function(v) {
  inherits(v, "formula") ||
    (is.list(v) && length(v) &&
       all(vapply(v, inherits, logical(1L), "formula")))
}

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

# `keep = FALSE` because the same column being BOTH the page key and one
# nobody wants printed is not two decisions -- it is the ordinary shape of
# a table paged by a parameter, and it was the only place in six reports
# where a column had to be named twice.
# The GROUP axis of pagination: each value of one column becomes its
# own page, and the page is NAMED after it, which is the line
# rtf_tables(auto_section = TRUE) cuts a section on.  It is the
# outermost division there is and has nothing to do with rows -- which
# is why it is not plan_row_group(), whose subject is how repeated
# values look down the body.
#
# `col` may be left out and read off table_plan(rows = ), so which column
# to hide is not known here.  Record the answer and let
# .plan_rtf_args() do it once the roles are in hand.
#' @rdname plan_verbs
#' @export
plan_paginate_group <- function(plan, col = NULL, keep = TRUE) {
  .plan_layer(plan, "group",
              list(group_col = col, .keep = keep, .page = TRUE))
}

# What a row GROUP is inside the body -- where one run of equal values
# ends and the next begins -- and whether the repeat is printed.  It
# does NOT make the row headings; folding the keys into one heading
# column, indenting them and adding a summary row is plan_stub().
# In six reports this and the page group were never used together.
#' @rdname plan_verbs
#' @export
# The carrier is the outermost row key, which table_plan(rows = ) has
# already named; naming it again here was a second place for the two to
# disagree.
plan_row_group <- function(plan, mode = NULL, collapse = NULL,
                           group_col = NULL) {
  # An ARD plan has said which column groups the rows once already, as
  # its outermost row key; a second name for it is how the two drift.
  if (!is.null(group_col) && .plan_ard_half(plan)) {
    .ard_stop(paste0(
      "plan_row_group(group_col = ) names the grouping column of a table ",
      "that is already built.
  This plan's groups are its outermost row ",
      "key, table_plan(rows = ) -- name it there."))
  }
  if (!is.null(group_col) && (!is.character(group_col) ||
                              length(group_col) != 1L || is.na(group_col))) {
    .ard_stop("plan_row_group(group_col = ) is one column name.")
  }
  .plan_layer(plan, "group",
              list(group_by = mode, collapse_repeats = collapse,
                   group_col = group_col, .rows = TRUE))
}

# A column can be needed and not wanted: a sort carrier, the key a page
# break reads.  Naming them here says which, instead of `drop_cols` being
# read as "columns I regret".
# A variable whose rows belong under ONE level of another -- the
# sub-categories of a race under its "Asian" row.  Two analyses on the same
# data (a hierarchical tabulation would drop every level that has no
# sub-level), placed by the layout: the rows move, they are not counted
# again.
#' @rdname plan_verbs
#' @export
plan_nest <- function(plan, ...) {
  dots <- list(...)
  nm <- names(dots)
  if (!length(dots) || is.null(nm) || any(!nzchar(nm))) {
    .ard_stop("plan_nest(): name the nested variable, e.g. plan_nest(RACESUB = c(RACE = \"Asian\")).")
  }
  nest <- lapply(nm, function(v) {
    u <- dots[[v]]
    if (!is.character(u) || length(u) != 1L || is.null(names(u)) || !nzchar(names(u))) {
      .ard_stop(sprintf(paste0(
        "plan_nest(%s = ): one level of one variable, named by it: ",
        "c(RACE = \"Asian\")."), v))
    }
    list(parent = names(u), level = unname(u))
  })
  .plan_layer(plan, "nest", list(nest = stats::setNames(nest, nm)))
}

# A Total column read from cards' own overall rows -- the same analysis
# without its `by` (`ard_tabulate(adsl, variables = RACE)` beside
# `ard_tabulate(adsl, by = ARM, variables = RACE)`, ard_stack(.overall =
# TRUE)) -- which carry no group.  The ARD keeps no invented ARM value;
# the label is the table's.
#' @rdname plan_verbs
#' @export
plan_total <- function(plan, label = "Total", position = c("last", "first")) {
  if (!is.character(label) || length(label) != 1L || is.na(label) ||
      !nzchar(label)) {
    .ard_stop("plan_total(label = ) is one text, the column's heading: \"Total\".")
  }
  position <- match.arg(position)
  .plan_layer(plan, "total", list(label = label, position = position))
}

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
plan_sort <- function(plan, ..., stat = NULL, keep = TRUE) {
  v <- list(...)
  keys <- if (length(v) == 1L && is.logical(v[[1L]])) v[[1L]]
          else unlist(v, use.names = FALSE)
  .plan_layer(plan, "sort",
              list(sort = keys, sort_stat = stat, .keep = keep))
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
# The ROW axis.  A page per value of a column, with no row budget, is the
# group axis, plan_paginate_group().  `page_by` is the one thing here that
# is not a row cut: the BY column(s) that partition the body FIRST, each
# value a page named after it, the row settings above then applying
# within one partition (a period, a cohort) -- as_rtftables(page_by = ),
# where it is also documented with the row pagination it scopes.  It is
# here, not on plan_paginate_group(), because it is what lets a row budget
# and a value split live together: a value split alone makes one page per
# value however long it is.
plan_paginate_rows <- function(plan, max_rows = NULL, split = NULL,
                               break_before = NULL, min_group_rows = NULL,
                               cont_label = NULL, page_by = NULL) {
  .plan_layer(plan, "pages",
              list(max_rows = max_rows, split = split,
                   split_rows = break_before,
                   min_group_rows = min_group_rows,
                   cont_label = cont_label, page_by = page_by))
}

# The WHOLE-table settings, listed.  Everything a table has per column or
# per cell is another verb's (plan_columns(), plan_cell_style(),
# plan_col_header(), plan_blanks()), so what is left is short enough to
# name -- and a name that is written here is checked here.  The rules of
# one kind of row are `border_header` ... `border_last_row`, the names
# rtf_table_style() gives them.
#' @rdname plan_verbs
#' @export
plan_style <- function(plan, border = NULL, align_count_pct = NULL,
                       font = NULL, font_size_half_points = NULL,
                       row_height_twips = NULL, row_height_exact = NULL,
                       header_row_height_twips = NULL,
                       blank_row_height_twips = NULL,
                       cell_padding_left_twips = NULL,
                       cell_padding_right_twips = NULL, cell_valign = NULL,
                       table_align = NULL, markup = NULL,
                       blank_row_normalize = NULL,
                       border_header = NULL, border_spanning = NULL,
                       border_body = NULL, border_first_row = NULL,
                       border_last_row = NULL, header_align = NULL,
                       header_bold = NULL, header_italic = NULL,
                       align = NULL, bold = NULL, italic = NULL,
                       underline = NULL, table_width_twips = NULL,
                       table_width_pct = NULL,
                       table_width_pct_of_writable = NULL) {
  zones <- list(border_header = border_header,
                border_spanning = border_spanning, border_body = border_body,
                border_first_row = border_first_row,
                border_last_row = border_last_row)
  zones <- zones[!vapply(zones, is.null, logical(1L))]
  # the table's default look, which rtf_table_style() carries by these
  # names; .plan_rtf_args() folds them into it with the border zones
  look <- list(.style_header_align = header_align,
               .style_header_bold = header_bold,
               .style_header_italic = header_italic, .style_align = align,
               .style_bold = bold, .style_italic = italic,
               .style_underline = underline)
  look <- look[!vapply(look, is.null, logical(1L))]
  if (length(zones) && !is.null(border)) {
    .ard_stop(paste0(
      "plan_style(): `border` is the whole table's rules and `border_*` ",
      "those of one kind of row.\n  Say it one way: border = \"tfl\", or ",
      "border_header = , border_body = , ..."))
  }
  .plan_layer(plan, "style", c(
    list(border = border, align_count_pct = align_count_pct, font = font,
         font_size_half_points = font_size_half_points,
         row_height_twips = row_height_twips,
         row_height_exact = row_height_exact,
         header_row_height_twips = header_row_height_twips,
         blank_row_height_twips = blank_row_height_twips,
         cell_padding_left_twips = cell_padding_left_twips,
         cell_padding_right_twips = cell_padding_right_twips,
         cell_valign = cell_valign, table_align = table_align,
         markup = markup, blank_row_normalize = blank_row_normalize,
         table_width_twips = table_width_twips,
         table_width_pct = table_width_pct,
         table_width_pct_of_writable = table_width_pct_of_writable),
    zones, look))
}

# plan_col_header(lines = ): the header written a row at a time, a cell
# a name = its text, as one would read it --
#   list(c(row_label = "", .values = "{col}"),
#        c(row_label = "Characteristic", .values = "(N={n})"))
# -- turned into the cells of the data-frame form (the `col_header` sheet),
# so everything after it is the same.  `span`: "each" (a cell a column),
# "one" (one cell over the columns named) or a key's name (a cell per value
# of that key: a spanner); one for every cell, or a list as `lines` is, of
# a span a cell (named as the line's cells are).
.plan_header_lines <- function(lines, span = "each") {
  if (!is.list(lines) || !length(lines)) {
    .ard_stop("plan_col_header(lines = ) is a list, one element a header row: ",
              "list(c(row_label = \"\", .values = \"{col}\"), ...).")
  }
  if (is.list(span) && length(span) != length(lines)) {
    .ard_stop(sprintf(paste0(
      "plan_col_header(span = ) as a list has one element a header row; ",
      "`lines` has %d, `span` %d."), length(lines), length(span)))
  }
  rows <- list()
  for (i in seq_along(lines)) {
    ln <- lines[[i]]
    nm <- names(ln)
    if (!is.character(ln) || is.null(nm) || anyNA(nm) || !all(nzchar(nm))) {
      .ard_stop(sprintf(paste0(
        "plan_col_header(lines = ): row %d is a named character vector, a ",
        "cell a name (the columns it sits on: a name, .values, 3:5, ",
        "KEY = value) = its text."), i))
    }
    sp <- if (is.list(span)) span[[i]] else span
    for (j in seq_along(ln)) {
      s1 <- if (!is.null(names(sp)) && nm[j] %in% names(sp)) sp[[nm[j]]] else
        if (is.null(names(sp))) sp[[1L]] else "each"
      if (!is.character(s1) || length(s1) != 1L || is.na(s1)) {
        .ard_stop("plan_col_header(span = ) is \"each\", \"one\" or a key's name.")
      }
      rows[[length(rows) + 1L]] <- data.frame(
        line = i, cols = nm[j], text = if (is.na(ln[[j]])) "" else ln[[j]],
        span = if (identical(s1, "one")) NA_character_ else s1,
        stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
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
# `levels` table_plan() was given, so nothing is written twice.  A
# function of the ARD covers a denominator pull_ard() cannot find; a
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
# `values` is what the header's `{tokens}` take, in one argument:
# the populations the plan reads from the data (`list(n = TRUE)`,
# `list(n = "page", N = "table")`, numbers, a function), or a data frame
# of per-page values handed to set_col_header() as it is.
plan_col_header <- function(plan, header = NULL, values = NULL,
                            header_sep = NULL, col_header_align = NULL,
                            lines = NULL, span = "each") {
  if (!is.null(lines)) {
    if (!is.null(header)) {
      .ard_stop("plan_col_header(): give the header once, as `header` or as `lines`.")
    }
    header <- .plan_header_lines(lines, span)
  }
  if (is.data.frame(header)) {
    # the `col_header` sheet's cells (one row a cell: `row`, `cols`, `text`,
    # `span` ...), resolved against each page's columns when it is made
    cells <- lapply(seq_len(nrow(header)), function(i)
      .plan_header_cell(header[i, , drop = FALSE]))
    txt <- vapply(cells, function(r) r[["text"]] %||% "", "")
    header <- structure(list(cells = cells), class = "plan_header_cells")
    # a `{n` in a text is what asks for a population at all
    if (is.null(values) && any(grepl("{n", txt, fixed = TRUE))) {
      values <- list(n = TRUE)
    }
  }
  if (!is.null(values) && !is.data.frame(values) && !is.list(values) &&
      !isTRUE(values)) {
    values <- list(n = values)
  }
  if (isTRUE(values)) values <- list(n = TRUE)
  # not `header_sep`: `hdr$header` would match it by partial name
  .plan_layer(plan, "header", list(header = header, values = values,
                                   names_sep = header_sep,
                                   text_align = col_header_align))
}

# Widths by column NAME (a reordered table keeps them) and the columns
# whose numbers line up at the decimal point: the `columns` sheet.
#' @rdname plan_verbs
#' @export
plan_columns <- function(plan, widths = NULL, decimal = NULL,
                         row_title = NULL, auto_width = NULL, sep = NULL,
                         cell_format = NULL, column_widths_twips = NULL) {
  if (!is.null(sep) && (!is.character(sep) || length(sep) != 1L ||
                        is.na(sep) || !nzchar(sep))) {
    .ard_stop("plan_columns(sep = ) is one non-empty string, \"____\" by default.")
  }
  .plan_layer(plan, "columns",
              list(widths = widths, decimal = decimal, row_title = row_title,
                   auto_width = auto_width, sep = sep,
                   cell_format = cell_format,
                   column_widths_twips = column_widths_twips))
}

# The separator several `cols` keys are joined with in the value columns'
# names: "Placebo____F".  A column name is what it makes, so it is
# declared with the columns.
.plan_sep <- function(plan) {
  .plan_merge(.plan_of(plan, "columns"))$sep %||% "____"
}

# The styling verbs of rtfreporter, as declarations: each call is one
# style_header() / style_cols() / style_zone() on the finished pages, with
# that function's own arguments, run in the order written.  `cols` may
# name columns (and `.values` every value column), so it follows them.
.plan_restyle <- function(plan, fun, args) {
  args <- args[!vapply(args, is.null, logical(1L))]
  .plan_layer(plan, "restyle", list(fun = fun, args = args))
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
# Blank rows are plan_blanks()'s (`where = "records"` ends each record's
# block with one, `first = TRUE` starts each page with one), and an
# alignment is a column's own (listing_col(align = )), so this verb keeps
# what only a listing has.
plan_listing <- function(plan, ..., type = NULL, sep = NULL,
                         spacer = NULL, spacer_rel_width = NULL,
                         layout = NULL, wrap = NULL) {
  .plan_layer(plan, "listing",
              list(cols = list(...), type = type, sep = sep,
                   spacer = spacer, spacer_rel_width = spacer_rel_width,
                   layout = layout, wrap = wrap))
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

# Steps that run on the finished pages: the way out for what no verb
# declares.  They are functions, so the plan cannot read them (a workbook
# cannot carry one); what they were once used for most --
# set_decimal_split(), paginate_cols() -- has a declaration now
# (plan_columns(decimal = ), plan_paginate_cols()).
#' @rdname plan_verbs
#' @export
plan_after <- function(plan, ...) {
  fs <- list(...)
  bad <- !vapply(fs, is.function, logical(1L))
  if (any(bad)) {
    .ard_stop(paste0(
      "plan_after() takes functions of the pages, one per step -- for ",
      "example\n    plan_after(\\(x) style_header(x, bold = TRUE))\n",
      "  Decimal alignment and column pages are declarations: ",
      "plan_columns(decimal = ), plan_paginate_cols()."))
  }
  .plan_layer(plan, "after", list(steps = fs))
}


# -- the resolver ------------------------------------------------------------

#' Run a plan, or look inside it
#'
#' Resolves an [table_plan()]'s layers and runs the conversion.  `stage` stops
#' it early, so the same one pass answers "what does this do" and "what did it
#' do" --- there is no second code path that could disagree with the first.
#'
#' @param plan An [table_plan()].
#' @param stage How far to go.  `"auto"`, the default, is **as far as the
#'   plan declares**: a plan that says nothing about the display stops at
#'   the table `data.frame`; one that carries a display verb goes on
#'   to the RTF pages.  You rarely need this function at all ---
#'   [rtf_tables()] takes a plan directly --- and naming a stage is
#'   for looking inside: `"input"`, `"args"`, `"table"`, `"pages"`.
#'
#'   The named stages: `"table"` returns the table
#'   `data.frame`, the same object [widen_ard()] returns.  `"input"`
#'   returns the frame going in, with the ARD column names the roles
#'   renamed.  `"args"` returns the resolved argument lists
#'   without running anything --- the call the plan amounts to, as
#'   `$widen` ([widen_ard()]'s) and `$rtf` ([as_rtftables()]'s).  Both
#'   are resolved from layers and either can be the one that
#'   surprises: a page budget declared twice is last-wins, and the
#'   call you are editing may not be the one that decides, so
#'   `plan_apply(p, "args")$rtf$max_rows` is the way to ask.
#'   `"pages"` goes all the way: [fmt_numeric()], [stub_cols()],
#'   [as_rtftables()], [set_col_header()] and whatever `plan_after()`
#'   declared, giving the RTF pages.
#'
#' @return A data frame; for `stage = "args"` the resolved argument list;
#'   for `stage = "pages"` what [as_rtftables()] and the steps after it
#'   return.
#'
#' @examples
#' if (requireNamespace("cards", quietly = TRUE)) {
#'   p <- cards::ard_stack(
#'          cards::ADSL, .by = ARM,
#'          cards::ard_summary(variables = AGE)) |>
#'     normalize_ard() |>
#'     table_plan(cols = "ARM", rows = c(group = "variable")) |>
#'     plan_cells(continuous = c("Mean (SD)" = "{mean} ({sd})")) |>
#'     plan_digits(2) |>
#'     plan_digits(AGE = 0)
#'
#'   str(plan_apply(p, "args")$cells)   # AGE won
#'   plan_apply(p)
#' }
#' @seealso [table_plan()], [plan_verbs]
#' @export
plan_apply <- function(plan, stage = c("auto", "input", "args",
                                       "table", "pages")) {
  if (!inherits(plan, "table_plan")) {
    .ard_stop("Expected a table_plan; start from table_plan(ard).")
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
    if (stage %in% c("table", "input")) return(d)
    return(.plan_to_pages(plan, d))
  }
  # A table somebody already built -- with widen_ard(), with dplyr, with
  # anything -- is a legitimate source for the display half on its own.
  # Asking for the ARD half is what says otherwise: the roles and
  # plan_cells() need statistics, and a finished table has none.
  if (identical(plan$kind, "wide")) {
    # `plan_cells(na = )` alone is what an empty cell prints, which a
    # finished table has too
    ard_half <- vapply(plan$layers, function(l)
      l$kind %in% c("cells", "levels", "labels") ||
        (identical(l$kind, "cell_options") &&
           length(setdiff(names(Filter(Negate(is.null), l$fields)), "na"))),
      TRUE)
    ard_half <- any(ard_half) || length(plan$roles)
    if (!length(plan$layers)) {
      .ard_stop(paste0(
        "This source has no statistics to read -- no `stat_name` / `stat`, ",
        "and none of\n  the columns normalize_ard() adds -- and the ",
        "plan declares nothing.\n",
        "  A table to lay out : keep the display verbs (plan_stub, ",
        "plan_pages, ...).\n",
        "  A listing of records: add plan_listing(listing_col(...), ...).\n",
        "  An ARD to convert  : name the roles on table_plan(), add ",
        "plan_cells().\n",
        "  Columns seen       : ", paste(utils::head(names(plan$data), 8L),
                                         collapse = ", ")))
    }
    if (!ard_half) {
      d <- plan$data
      # every digits key names a column of this table (or is `.rows`)
      .plan_check_digit_cols(plan, d, setdiff(
        names(.plan_merge(.plan_of(plan, "digits"))), ".rows"))
      .plan_remember(plan, "table", d)
      if (stage %in% c("table", "input")) return(d)
      if (identical(stage, "args")) {
        return(list(widen = list(), rtf = .plan_rtf_args(plan, d)))
      }
      return(.plan_to_pages(plan, d))
    }
    .ard_stop(paste0(
      "This source has no statistics to read -- no `stat_name` / `stat`, ",
      "and none of\n  the columns normalize_ard() adds -- but ",
      "table_plan(cols = ) / plan_cells() need them.\n",
      "  If it is already the table, drop those and keep the display ",
      "verbs.\n",
      "  If it is a listing of records, add plan_listing(...).\n",
      "  Columns seen : ", paste(utils::head(names(plan$data), 8L),
                                 collapse = ", ")))
  }

  # 1. the long-frame seam.  Nothing is flattened here: normalize_ard()
  #    ran before the plan, which is why the roles could be checked
  #    against real column names when they were declared.
  x <- .plan_prepare(plan, plan$data)
  .plan_check_key_sep(plan, x)
  .plan_remember(plan, "long", x)
  if (identical(stage, "input")) return(x)

  # 2. the spread arguments.  The roles were said once, on table_plan();
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
    # a key variable's own tabulation is no cell (widen_ard() leaves it
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
    reached <- c("default", ".rows", names(cells), vars)
    plan$cache$digit_cols <- setdiff(names(dig), reached)
  }

  # 5. one rounding family for the run; a per-variable one would have to reach
  #    into `widen_ard()`, which a spike does not do.  Named keys are read so
  #    the shape is there, and a disagreement is reported rather than guessed.
  # One rounding family for the run, last wins like every other layer.
  if (length(rnd)) s_args$rounding <- rnd$rounding

  if (identical(stage, "args")) {
    # Both halves, because both are resolved from layers and either can
    # be the one that surprises you.  A page budget declared twice is the
    # obvious case: last wins, and the call you are editing may not be
    # the one that decides.
    return(list(widen = s_args, rtf = .plan_rtf_args(plan)))
  }
  tbl <- .plan_stage(do.call(widen_ard, c(list(x = x), s_args)),
                     plan, c("cells", "cell_options", "digits", "round",
                             "levels", "labels"))
  # a variable's rows under one level of another (plan_nest())
  nest <- .plan_merge(.plan_of(plan, "nest"), deep = "nest")$nest
  if (length(nest)) tbl <- .plan_nest_rows(plan, tbl, nest, s_args)
  .plan_check_digit_cols(plan, tbl, plan$cache$digit_cols, vars = TRUE)
  # the table-side seam: a column the table can only know once it exists
  .plan_remember(plan, "table", tbl)
  if (identical(stage, "table")) return(tbl)

  .plan_to_pages(plan, tbl)
}


# The rows of each nested variable moved under their level: found by the
# rows key that carries the variable (its value is the variable's label,
# as plan_labels() gave it, or its name), indented one stub step deeper.
.plan_nest_rows <- function(plan, tbl, nest, s_args) {
  rows <- plan$roles$rows
  key <- names(rows)[as.character(rows) == "variable"]
  key <- if (length(key) && nzchar(key[1L])) key[1L] else
    if ("variable" %in% names(tbl)) "variable" else character()
  if (!length(key) || !key %in% names(tbl)) {
    .ard_stop(paste0(
      "plan_nest() moves a variable's rows, and the rows carry no variable: ",
      "name it on table_plan(rows = ), e.g. rows = c(group = \"variable\")."))
  }
  # a variable's heading as plan_labels() gave it: under `variable`
  # (plan_labels(variable = c(RACE = "Race"))), its own key
  # (plan_labels(RACE = "Race")), or its own name among its levels'
  # (plan_labels(SEX = c(SEX = "Sex", F = "Female"))); else its name
  shown <- function(v) {
    lab <- s_args$labels$variable
    if (!is.null(lab) && v %in% names(lab)) return(lab[[v]])
    own <- s_args$labels[[v]]
    if (is.character(own) && length(own) == 1L && is.null(names(own))) return(own)
    if (is.character(own) && v %in% names(own)) return(own[[v]])
    v
  }
  lcol <- intersect(c(names(s_args$label %||% character()), "label", ".label"), names(tbl))[1L]
  if (is.na(lcol)) {
    .ard_stop("plan_nest(): the table has no row label column to match the level in.")
  }
  stub <- .plan_merge(.plan_of(plan, "stub"))
  pad <- strrep(.stub_nbsp(), stub$indent %||% 4L)
  for (child in names(nest)) {
    par <- nest[[child]]$parent
    lv <- nest[[child]]$level
    g <- as.character(tbl[[key]])
    kids <- which(g == shown(child))
    if (!length(kids)) {
      .ard_stop(sprintf("plan_nest(%s = ): the table has no rows of %s.  Its %s: %s.",
                        child, child, key, paste(unique(g), collapse = ", ")))
    }
    in_par <- g == shown(par)
    at <- which(in_par & as.character(tbl[[lcol]]) == lv)
    if (length(at) != 1L) {
      .ard_stop(sprintf(paste0(
        "plan_nest(%s = c(%s = \"%s\")): %s has no row \"%s\".  Its rows: %s."),
        child, par, lv, par, lv, paste(unique(tbl[[lcol]][in_par]), collapse = ", ")))
    }
    # (a factor column takes the new values: its levels in the new order)
    was <- vapply(tbl[c(key, lcol)], is.factor, NA)
    for (k in c(key, lcol)) tbl[[k]] <- as.character(tbl[[k]])
    tbl[[key]][kids] <- tbl[[key]][at]
    tbl[[lcol]][kids] <- paste0(pad, tbl[[lcol]][kids])
    rest <- setdiff(seq_len(nrow(tbl)), kids)
    tbl <- tbl[c(rest[rest <= at], kids, rest[rest > at]), , drop = FALSE]
    for (k in c(key, lcol)[was]) tbl[[k]] <- factor(tbl[[k]], levels = unique(tbl[[k]]))
    rownames(tbl) <- NULL
  }
  tbl
}

# A frame that did not come from cards has its own names for the three
# columns widen_ard() reads by name -- which statistic a row is, what
# it is worth, and which analysis variable it belongs to.  Saying so is
# a rename, in the `c(new = old)` vocabulary `rows` and `cols` already
# use, done once here rather than asked for again in every verb.
#
# The label column is the other thing such a frame does differently:
# tfrmt has the same problem, because a continuous row is labelled by
# its statistic and a categorical one by its level, and they are not
# the same column.  `label = c("CAT", "PARAM")` COALESCES -- first
# non-missing wins -- which is what normalize_ard() does for a cards
# ARD when it builds `.label`.
.plan_prepare <- function(plan, d) {
  if (!is.data.frame(d)) return(d)
  for (k in c("variable", "stat_name", "stat")) {
    src <- plan$roles[[k]]
    if (is.null(src) || identical(as.character(src), k)) next
    src <- as.character(src)[1L]
    if (!src %in% names(d)) {
      .ard_stop(sprintf(
        "table_plan(%s = %s): no such column.\n  Columns: %s",
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
  d <- .plan_total_rows(plan, d)
  .plan_drop_empty(plan, d)
}

# plan_total(): its label and position, or NULL
.plan_total_spec <- function(plan) {
  t <- .plan_merge(.plan_of(plan, "total"))
  if (length(t)) t
}

# The overall rows -- a statistic with no value of the column key, not the
# key's own tabulation nor the study total -- become the Total column.
.plan_total_rows <- function(plan, d) {
  tt <- .plan_total_spec(plan)
  if (is.null(tt) || !is.data.frame(d)) return(d)
  cols <- as.character(unlist(plan$roles$cols, use.names = FALSE))
  if (length(cols) != 1L) {
    .ard_stop(sprintf(paste0(
      "plan_total(): a Total column is read for one column key; this ",
      "table has %s."),
      if (length(cols)) paste0("cols = c(", paste(cols, collapse = ", "), ")")
      else "no cols"))
  }
  if (!cols %in% names(d)) return(d)
  k <- d[[cols]]
  v <- if ("variable" %in% names(d)) as.character(d$variable) else
    rep(NA_character_, nrow(d))
  own <- if (".key_own" %in% names(d)) d$.key_own %in% TRUE else
    rep(FALSE, nrow(d))
  at <- is.na(k) & !own & !(v %in% "..ard_total_n..")
  if (!any(at)) {
    .ard_stop(sprintf(paste0(
      "plan_total(): the ARD has no overall rows, rows without %s.
",
      "  Ask cards for them: the same analysis without `by` bound under it, ",
      "or ard_stack(.overall = TRUE)."), cols))
  }
  if (tt$label %in% as.character(k)) {
    .ard_stop(sprintf(paste0(
      "plan_total(label = \"%s\"): %s already has a value \"%s\" ",
      "(a Total made in the data?).  Keep one of the two."),
      tt$label, cols, tt$label))
  }
  x <- as.character(k)
  x[at] <- tt$label
  d[[cols]] <- if (is.factor(k)) {
    factor(x, levels = .plan_total_levels(levels(k), tt))
  } else x
  d
}

# the column key's order with the Total column in its place
.plan_total_levels <- function(lv, tt) {
  lv <- setdiff(lv, tt$label)
  if (identical(tt$position, "first")) c(tt$label, lv) else c(lv, tt$label)
}

# plan_levels(.drop_empty = ): a level of these variables that no record
# has -- its `n` is 0 in every column, as a code list applied before the
# ARD leaves it -- is not shown.  A level with no `n` (a summary) stays.
.plan_drop_empty <- function(plan, d) {
  vars <- unique(unlist(lapply(.plan_of(plan, "levels"), `[[`, "drop_empty")))
  if (!length(vars) || !all(c("variable", "stat_name", "stat") %in% names(d))) {
    return(d)
  }
  lvl <- if ("variable_level" %in% names(d)) "variable_level" else NULL
  if (is.null(lvl)) return(d)
  ctx <- if ("context" %in% names(d)) as.character(d$context) else ""
  key <- paste(d$variable, ctx, as.character(d[[lvl]]), sep = "\r")
  n <- d$variable %in% vars & d$stat_name %in% "n" & !is.na(d[[lvl]])
  if (!any(n)) return(d)
  st <- d$stat[n]
  st <- if (is.list(st)) vapply(st, function(x) suppressWarnings(as.numeric(x[1L])), 0) else suppressWarnings(as.numeric(st))
  empty <- tapply(is.na(st) | st == 0, key[n], all)
  drop <- names(empty)[empty]
  if (!length(drop)) return(d)
  d[!key %in% drop, , drop = FALSE]
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

# `widen_ard()`'s arguments, assembled from the two places they are
# declared: the roles, said once beside the data, and the keyed
# layers, which merge one key at a time.
.plan_spread_args <- function(plan) {
  out <- plan$roles
  opts <- .plan_merge(.plan_of(plan, "cell_options"))
  for (k in names(opts)) out[[k]] <- opts[[k]]
  sep <- .plan_merge(.plan_of(plan, "columns"))$sep
  if (!is.null(sep)) out$sep <- sep
  srt <- .plan_merge(.plan_of(plan, "sort"))
  if (length(srt) && !is.null(srt$sort)) out$sort <- srt$sort
  if (length(srt) && !is.null(srt$sort_stat)) out$sort_stat <- srt$sort_stat
  # renames, not arguments: .plan_prepare() has already done them
  out[c("variable", "stat_name", "stat")] <- NULL
  lb <- .plan_label_spec(plan)
  if (!is.null(lb)) out$label <- stats::setNames(lb$name, lb$name)
  for (kind in c("levels", "labels")) {
    v <- .plan_merge(.plan_of(plan, kind), deep = kind)[[kind]]
    if (length(v)) out[[kind]] <- v
  }
  # plan_total(): the Total column in the column key's order
  tt <- .plan_total_spec(plan)
  cols <- as.character(unlist(out$cols, use.names = FALSE))
  if (!is.null(tt) && length(cols) == 1L) {
    lv <- out$levels[[cols]]
    if (is.null(lv) && identical(tt$position, "first") &&
        is.data.frame(plan$data) && cols %in% names(plan$data)) {
      k <- plan$data[[cols]]
      lv <- if (is.factor(k)) levels(k) else .ard_first_seen(as.character(k[!is.na(k)]))
    }
    if (!is.null(lv)) {
      out$levels <- out$levels %||% list()
      out$levels[[cols]] <- .plan_total_levels(lv, tt)
    }
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
  # the rules of one kind of row are an rtf_table_style(), the object that
  # carries them by those names; so does the table's default look
  zn <- grep("^border_", names(out), value = TRUE)
  stl <- .plan_merge(.plan_of(plan, "style"))
  look <- stl[startsWith(names(stl) %||% character(0), ".style_")]
  names(look) <- sub(".style_", "", names(look), fixed = TRUE)
  if (length(zn) || length(look)) {
    out$style <- do.call(rtf_table_style, c(out[zn], look))
    out[zn] <- NULL
  }
  # plan_columns(widths = ) without names is col_rel_width itself, one a
  # column in order; named, it is applied to the pages by column name
  colset <- .plan_merge(.plan_of(plan, "columns"))
  cw <- colset$widths
  if (length(cw) && is.null(names(cw)) && is.null(out$col_rel_width)) {
    out$col_rel_width <- cw
  }
  if (!is.null(colset$row_title))  out$row_title  <- colset$row_title
  if (!is.null(colset$auto_width)) out$auto_width <- colset$auto_width
  if (!is.null(colset$cell_format)) out$cell_format <- colset$cell_format
  if (!is.null(colset$column_widths_twips)) {
    out$column_widths_twips <- colset$column_widths_twips
  }
  # how a plain table's names become a spanning header, and how the
  # header text sits: the header's own, so plan_col_header() says them
  hdr <- .plan_merge(.plan_of(plan, "header"))
  if (!is.null(hdr[["names_sep"]])) {
    out$header_sep <- hdr[["names_sep"]]
  } else if (.plan_ard_half(plan)) {
    # an ARD half widens its own column names on plan_columns(sep = ), so
    # that is the separator the header must be split on too -- unset, a
    # non-default sep would widen the table but never build its header
    out$header_sep <- .plan_sep(plan)
  }
  if (!is.null(hdr[["text_align"]])) {
    out$col_header_align <- hdr[["text_align"]]
  }
  # what an empty cell prints, on a table that is already built: there is
  # no widen_ard() to fill it, so as_rtftables() does
  if (!.plan_ard_half(plan)) {
    na <- .plan_merge(.plan_of(plan, "cell_options"))$na
    if (!is.null(na)) out$na <- na
  }
  # The row order goes to whichever half can do it: widen_ard() when
  # there are statistics to sort on, as_rtftables() when the source is
  # already the table.
  srt <- .plan_merge(.plan_of(plan, "sort"))
  if (length(srt) && !.plan_ard_half(plan) &&
      !is.null(srt$sort) && !is.logical(srt$sort)) {
    # `-name` is descending, as it is on the ARD half
    desc <- startsWith(srt$sort, "-")
    out$sort_by <- sub("^-", "", srt$sort)
    if (any(desc)) out$sort_desc <- desc
  }
  # The grouping carrier is the outermost row key, which the roles have
  # already named.
  gcol <- .plan_group_col(plan, tbl)
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
  # mean both columns go, and plan_paginate_group(keep = FALSE) writes one of
  # its own.
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
  if (length(g) && isFALSE(g$.keep)) {
    # folded into the stub already?  then it is gone, not hidden.  With
    # no table to look at, keep it: the carrier is normally there.
    gc <- .plan_group_col(plan)
    out <- c(out, if (is.null(tbl)) gc else .plan_present(gc, tbl))
  }
  srt <- .plan_merge(.plan_of(plan, "sort"))
  if (length(srt) && isFALSE(srt$.keep) && !is.null(srt$sort) &&
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
# only there to group by.  All of that was said on table_plan(), so saying it
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
      "worked out from\n  table_plan(rows = , label = ) less ",
      "whatever `keep = FALSE` hides, and none\n  of them is in ",
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

# Which column groups the rows.  Nobody names it twice: it is the page key
# plan_paginate_group(col = ) gave, or the outermost row key, which
# table_plan(rows = ) has already given.  It is derived when a page key is
# hidden (`keep = FALSE`: a column has to be named to be hidden) and when
# plan_row_group() groups by it; otherwise as_rtftables() has its own
# answer.
.plan_group_col <- function(plan, tbl = NULL) {
  g <- .plan_of(plan, "group")
  if (!length(g)) return(NULL)
  m <- .plan_merge(g)
  if (!is.null(m$group_col)) return(m$group_col)
  k <- .plan_row_keys(plan)
  if (!length(k)) return(NULL)
  # a hidden page key has to be named to be hidden
  if (isFALSE(m$.keep)) return(k[1L])
  # plan_row_group() groups by the outermost row key while it is still a
  # column of its own; folded into a stub, the groups are the stub's
  # headings, which is what as_rtftables() reads by default
  if (isTRUE(m$.rows) && !length(.plan_of(plan, "stub")) &&
      !is.null(tbl) && k[1L] %in% names(tbl)) {
    return(k[1L])
  }
  NULL
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
plan_paginate_cols <- function(plan, at = NULL, cut_by = NULL,
                               every = NULL, keep = NULL, col_header = NULL,
                               fit = NULL, allow_span_break = NULL,
                               order = NULL) {
  given <- !vapply(list(at, cut_by, every), is.null, logical(1L))
  if (sum(given) > 1L) {
    .ard_stop("plan_paginate_cols(): say where to cut one way -- `at`, `cut_by` or `every`.")
  }
  if (!is.null(fit) && !(is.logical(fit) && length(fit) == 1L && !is.na(fit))) {
    .ard_stop("plan_paginate_cols(fit = ) is TRUE (every block fills the sheet as page 1 does) or FALSE (each column keeps its width).")
  }
  .plan_layer(plan, "colpages",
              list(at = at, cut_by = cut_by, every = every, keep = keep,
                   col_header = col_header, fit = fit,
                   allow_span_break = allow_span_break, order = order))
}

# The plan's words for paginate_cols()'s arguments: `cut_by` is a list of
# column blocks (`cols`) or a separator / one key per column (`by`),
# `keep` the columns every block repeats (`carry`), `fit` the width rule.
.plan_colpages_args <- function(cp) {
  out <- list(at = cp$at, carry = cp$keep, col_header = cp$col_header,
              allow_span_break = cp$allow_span_break, page_order = cp$order)
  if (is.list(cp$cut_by)) out$cols <- cp$cut_by
  else if (!is.null(cp$cut_by)) out$by <- cp$cut_by
  if (!is.null(cp$fit)) out$width <- if (isTRUE(cp$fit)) "fill" else "keep"
  out[!vapply(out, is.null, logical(1L))]
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
  car <- cp$keep %||% first$row_title %||% 1L
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
    clash <- setdiff(attr(part, "conflict"), names(got))
    attr(part, "conflict") <- NULL
    if (length(clash)) {
      why <- c(why, stats::setNames(rep(paste0(
        "the ARD states two different counts for this column (two analyses ",
        "count the groups: keep one)"), length(clash)), clash))
    }
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
  # plan_total(): the Total column's population is the study total the
  # ARD states (the key's own tabulation's N, ..ard_total_n..) -- the one
  # column it is
  tt <- .plan_total_spec(plan)
  if (!is.null(tt) && length(cols) == 1L) {
    if (!is.null(total)) {
      out[[tt$label]] <- total
    } else {
      why[[tt$label]] <- paste0(
        "the ARD states no study total for the Total column (tabulate the ",
        "column key on its own, or add cards::ard_total_n())")
    }
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
  conflict <- character(0)
  for (g in unique(grp[n_sel])) {
    i <- n_sel & grp == g
    n <- st[i]
    kk <- key[i]
    if (anyNA(n)) next
    # the same count stated twice (the groups counted by two analyses: a
    # GROUPN row -- the subjects per group, which clinical reporting calls
    # big N -- and an ard_stack(.by_stats = TRUE)) is one count; two
    # different counts for one column are not a column's number
    if (anyDuplicated(kk)) {
      one <- vapply(split(n, kk), function(x) length(unique(x)) == 1L, NA)
      if (!all(one)) {
        conflict <- c(conflict, names(one)[!one])
        next
      }
      keep <- !duplicated(kk)
      n <- n[keep]
      kk <- kk[keep]
    }
    big <- unique(st[sel & sn %in% "N" & grp == g])
    if (length(big) != 1L || is.na(big) || !isTRUE(all.equal(sum(n), big))) {
      next
    }
    out <- c(out, stats::setNames(n, kk))
    bigs <- c(bigs, big)
  }
  # the population the partition splits -- at depth 1, the whole table's;
  # and the columns two different counts were stated for
  structure(out, N = bigs, conflict = unique(conflict))
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
# neither is written in the call.  One row a token: its values as they
# are (a list column) and as print() shows them (`text`).
.plan_header_tokens <- function(plan) {
  tbl <- plan$cache$table
  hdr <- .plan_merge(.plan_of(plan, "header"))
  hn <- .plan_header_n(hdr)
  why <- NULL
  nvals <- tryCatch(.plan_n_values(plan, hn), error = function(e) {
    why <<- conditionMessage(e)
    NULL
  })
  toks <- if (is.list(nvals) && !is.null(names(nvals)) &&
              any(nzchar(names(nvals)))) nvals
          else if (is.null(nvals)) list() else list(n = nvals)
  cols <- if (is.null(tbl)) NULL else .plan_spread_cols(plan, tbl)
  sep <- .plan_sep(plan)
  rows <- list()
  add <- function(token, kind, values, text, resolved = TRUE,
                  note = NA_character_) {
    rows[[length(rows) + 1L]] <<- list(token = token, kind = kind,
                                       values = list(values), text = text,
                                       resolved = resolved, note = note)
  }
  # A token that cannot be resolved is the one worth printing: saying
  # nothing is what sent you here.
  if (!is.null(why) && !is.null(hn)) {
    first <- strsplit(why, "\n", fixed = TRUE)[[1L]][1L]
    add("{n}", "population", NULL, paste0("-- NOT resolved: ", first),
        FALSE, first)
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
    leaf <- leaf[!is.na(leaf)]
    add("{col}", "column", leaf, show(leaf))
    if (lv > 1L) {
      for (i in seq_len(lv)) {
        v <- lvl(i)
        v <- v[!is.na(v)]
        add(paste0("{col", i, "}"), "column", v, show(v))
      }
    }
  }
  for (nm in names(toks)) {
    v <- toks[[nm]]
    why_n <- attr(v, "why", exact = TRUE)
    tok <- paste0("{", nm, "}")
    if (!length(v)) {
      note <- if (length(why_n)) why_n[[1L]] else
        "the ARD states no population size for these columns"
      add(tok, "population", NULL, paste0("-- NOT resolved: ", note), FALSE,
          note)
      next
    }
    add(tok, "population", unlist(v), show(v))
    if (length(why_n)) {
      note <- paste0(paste0(ifelse(names(why_n) == ".all", "",
                                   paste0(names(why_n), ": ")), why_n)[[1L]],
                     if (length(why_n) > 1L)
                       sprintf(" (+%d more)", length(why_n) - 1L))
      add(paste0(tok, " NOT resolved"), "population", NULL,
          paste0("-- ", note), FALSE, note)
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
    # a Total column (plan_total()) is the sum of the others, not one of
    # the parts
    tc <- .plan_total_spec(plan)$label
    if (!is.null(tc) && length(names(lv))) lv <- lv[names(lv) != tc]
    tot <- suppressWarnings(sum(as.numeric(unlist(lv)), na.rm = TRUE))
    add(paste0("{", nm, ":sum}"), "sum", tot, paste0(
      "= ", format(tot, trim = TRUE),
      " over every column (less over a spanner: its own columns)"))
    for (k in names(v) %||% character(0)) {
      add(paste0("{", nm, ":", k, "}"), "one", v[[k]],
          paste0("= ", format(v[[k]], trim = TRUE)))
    }
  }
  if (!length(rows)) {
    return(data.frame(token = character(), kind = character(),
                      values = I(list()), text = character(),
                      resolved = logical(), note = character(),
                      stringsAsFactors = FALSE))
  }
  out <- data.frame(
    token = vapply(rows, `[[`, "", "token"),
    kind = vapply(rows, `[[`, "", "kind"),
    text = vapply(rows, `[[`, "", "text"),
    resolved = vapply(rows, `[[`, NA, "resolved"),
    note = vapply(rows, function(r) as.character(r$note), ""),
    stringsAsFactors = FALSE)
  out$values <- lapply(rows, function(r) r$values[[1L]])
  out[c("token", "kind", "values", "text", "resolved", "note")]
}

#' The tokens a plan's column header may carry, with their values
#'
#' What `print(plan)` lists under "header tokens", as data: one row per
#' token a [plan_col_header()] cell may carry, with the values it resolves
#' to here.  A program that offers them (a GUI's "insert" menu that shows
#' each token's values) reads this instead of the printed text.
#'
#' * `kind = "column"`: `{col}` (the leaf of each spread column's name) and,
#'   with several `cols` keys, `{col1}`, `{col2}`, ... (each depth's
#'   values) -- known once the table has been made, which this does.
#' * `kind = "population"`: `{n}` (and the other names of
#'   `plan_col_header(values = )`), each column's population from the ARD;
#'   `resolved = FALSE` with the reason in `note` when the ARD does not state
#'   it.
#' * `kind = "sum"`: `{n:sum}`, the total over every column (over a spanner,
#'   over its own columns).
#' * `kind = "one"`: `{n:<column>}`, one column's value.
#'
#' @param plan A [table_plan()].
#' @return A data frame: `token`, `kind`, `values` (a list column: the
#'   values, named by column where they are), `text` (as `print()` shows
#'   them), `resolved`, `note`.
#' @seealso [plan_col_header()]
#' @examples
#' if (requireNamespace("cards", quietly = TRUE)) {
#'   ard <- normalize_ard(cards::ard_stack(cards::ADSL, .by = ARM,
#'     cards::ard_summary(variables = AGE)))
#'   p <- table_plan(ard, cols = "ARM") |>
#'     plan_col_header(values = list(n = TRUE))
#'   plan_header_tokens(p)[, c("token", "text")]
#' }
#' @export
plan_header_tokens <- function(plan) {
  if (!inherits(plan, "table_plan")) {
    .ard_stop("plan_header_tokens(): expected a table_plan.")
  }
  # the column tokens need the table: make it once (the cache keeps it)
  if (is.null(plan$cache$table)) {
    try(suppressMessages(suppressWarnings(plan_apply(plan, "table"))),
        silent = TRUE)
  }
  .plan_header_tokens(plan)
}

# The denominator, read once, with the keys table_plan() already has.
.plan_n_values <- function(plan, n, data = plan$data, page = NULL) {
  if (is.null(n)) return(NULL)
  sp <- .plan_spread_args(plan)
  # A page split by a group value (a lab parameter) has two populations:
  # its own -- the subjects with that test, the rows carrying the page
  # key -- and the table's -- the analysis set, rows without it.  Which
  # one a header says is the author's choice, `"page"` or `"table"`;
  # both must be in the ARD, and neither is filled in from elsewhere.
  if (!is.null(page)) {
    g <- as.character(data[[page$col]])
    mine <- data[!is.na(g) & g == page$value, , drop = FALSE]
    rest <- data[is.na(g), , drop = FALSE]
    attr(rest, "ard_total_n") <- attr(data, "ard_total_n", exact = TRUE)
  }
  read <- function(scope) {
    if (is.null(page)) return(.plan_n_read(plan, sp, data))
    tbl <- .plan_n_read(plan, sp, rest)
    if (identical(scope, "table")) return(tbl)
    own <- .plan_n_read(plan, sp, mine)
    out <- .plan_n_join(own, tbl)
    if (identical(scope, "auto") && .plan_n_differ(own, tbl)) {
      attr(out, "choice") <- TRUE
    }
    out
  }
  one <- function(v) {
    scope <- if (isTRUE(v)) "auto"
             else if (is.character(v) && length(v) == 1L &&
                      v %in% c("page", "table")) v
    if (is.character(v) && is.null(scope)) {
      .ard_stop(sprintf(paste0(
        "plan_col_header(values = list(n = %s)): a population is \"page\" (each ",
        "page's own) or \"table\"\n  (the analysis set); numbers are ",
        "given as numbers, or a function of the data."), sQuote(v[1L])))
    }
    if (!is.null(scope)) {
      if (is.null(sp$cols)) {
        .ard_stop(paste0(
          "plan_col_header(values = list(n = TRUE)) reads the denominator with the ",
          "same `cols` table_plan()\n  was given, and this plan ",
          "has none.  Give a function of the data instead."))
      }
      # Only a number the ARD STATES as a population size, at every
      # depth of the keys; what it does not state is left out, and the
      # header prints NA there and says why (see .plan_n_read()).
      # pull_ard() is not asked: it reads an analysis variable's `N`,
      # the count of its non-missing values.
      return(read(scope))
    }
    if (is.function(v)) {
      return(v(if (is.null(page)) data else rbind(mine, rest)))
    }
    v
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
  src <- .plan_page_col(plan)
  if (is.null(src) || is.null(page_names)) return(NULL)
  raw <- .ard_first_seen(plan$data[[src]])
  value <- lapply(page_names, function(nm) {
    if (nm %in% raw) return(nm)
    hit <- raw[startsWith(nm, raw)]
    if (length(hit)) hit[which.max(nchar(hit))] else NULL
  })
  list(col = src, value = value)
}

# The data's column that a page split by a group value is split on
# (plan_paginate_group()), or NULL when the pages are not split so.
.plan_page_col <- function(plan) {
  if (!isTRUE(.plan_merge(.plan_of(plan, "group"))$.page)) return(NULL)
  gcol <- .plan_group_col(plan)
  if (is.null(gcol)) return(NULL)
  r <- plan$roles$rows
  src <- if (!is.null(names(r)) && gcol %in% names(r)) r[[gcol]] else gcol
  if (!is.character(src) || length(src) != 1L ||
      !src %in% names(plan$data)) {
    return(NULL)
  }
  src
}

#' The populations a column header's `{n}` can say
#'
#' Which numbers the ARD states for `{n}`, before choosing one: a GUI that
#' asks "what does the header's `{n}` count?" shows each choice with its
#' values.  A table whose pages are split by a group value (a lab
#' parameter, with [plan_paginate_group()]) has two:
#'
#' * `scope = "page"`: each page's own -- the rows carrying the page's key
#'   (the subjects with that test), what `plan_col_header(values = list(n =
#'   "page"))` prints;
#' * `scope = "table"`: the table's -- the rows without the page key (the
#'   analysis set), the same on every page, what `n = "table"` prints.
#'
#' A table not split so has one, `scope = "all"`.  The numbers are the ones
#' [plan_col_header()] reads with `values = list(n = TRUE)` or a scope: only
#' a number the ARD states as a population size; a column it does not state
#' has no row.
#'
#' @param plan A [table_plan()] with `cols`.
#' @return A data frame: `scope` (`"all"`, `"page"` or `"table"`), `page`
#'   (the page's group value; `NA` but on `"page"` rows), `column` (the
#'   spread column's key, as `{n:<column>}` names it; `NA` for the
#'   population over all the columns, what a spanning cell's `{n}` reads),
#'   `value`.  Attribute `differ`: `TRUE` when a page's numbers and the
#'   table's are not the same, so which one `{n}` says is a choice (left
#'   unmade, the pages' are used, with a warning); `page_col`: the column
#'   the pages are split on (`NULL` when they are not).
#' @seealso [plan_col_header()], [plan_header_tokens()]
#' @examples
#' if (requireNamespace("cards", quietly = TRUE)) {
#'   ard <- normalize_ard(cards::ard_stack(cards::ADSL, .by = ARM,
#'     cards::ard_summary(variables = AGE)))
#'   plan_n_candidates(table_plan(ard, cols = "ARM"))
#' }
#' @export
plan_n_candidates <- function(plan) {
  if (!inherits(plan, "table_plan")) {
    .ard_stop("plan_n_candidates(): expected a table_plan.")
  }
  rows <- function(n, scope, page) {
    nm <- names(n) %||% rep("", length(n))
    tot <- attr(n, "total", exact = TRUE)
    data.frame(scope = scope, page = page,
               column = c(nm, if (!is.null(tot)) NA_character_),
               value = c(unname(as.numeric(n)), if (!is.null(tot)) as.numeric(tot)),
               stringsAsFactors = FALSE)
  }
  none <- rows(numeric(0), character(0), character(0))
  attr(none, "differ") <- FALSE
  sp <- .plan_spread_args(plan)
  if (is.null(sp$cols)) return(none)
  data <- plan$data
  src <- .plan_page_col(plan)
  if (is.null(src)) {
    out <- rows(.plan_n_read(plan, sp, data), "all", NA_character_)
    attr(out, "differ") <- FALSE
    return(out)
  }
  # as .plan_n_values() reads them: the table's from the rows without the
  # page key, a page's from its own rows and the table's for what it lacks
  g <- as.character(data[[src]])
  rest <- data[is.na(g), , drop = FALSE]
  attr(rest, "ard_total_n") <- attr(data, "ard_total_n", exact = TRUE)
  tbl <- .plan_n_read(plan, sp, rest)
  parts <- list()
  differ <- FALSE
  for (v in setdiff(.ard_first_seen(data[[src]]), NA)) {
    own <- .plan_n_read(plan, sp, data[!is.na(g) & g == v, , drop = FALSE])
    differ <- differ || .plan_n_differ(own, tbl)
    parts[[length(parts) + 1L]] <- rows(.plan_n_join(own, tbl), "page", as.character(v))
  }
  out <- do.call(rbind, c(parts, list(rows(tbl, "table", NA_character_))))
  rownames(out) <- NULL
  attr(out, "differ") <- differ
  attr(out, "page_col") <- src
  out
}

# A page's own numbers first, then the table's for what the page lacks.
.plan_n_join <- function(x, y) {
  keep <- setdiff(names(y) %||% character(0), names(x))
  out <- c(x, y[keep])
  wx <- attr(x, "why", exact = TRUE)
  wy <- attr(y, "why", exact = TRUE)
  attr(out, "total") <- attr(x, "total", exact = TRUE) %||%
    attr(y, "total", exact = TRUE)
  why <- c(wx, wy[!names(wy) %in% names(wx)])
  attr(out, "why") <- why[!names(why) %in% names(out)]
  out
}

# Do the page and the table state DIFFERENT numbers for the same thing?
# Then "which N" is a choice the author has to make.
.plan_n_differ <- function(own, tbl) {
  k <- intersect(names(own), names(tbl))
  if (length(k) && any(own[k] != tbl[k])) return(TRUE)
  a <- attr(own, "total", exact = TRUE)
  b <- attr(tbl, "total", exact = TRUE)
  !is.null(a) && !is.null(b) && !isTRUE(all.equal(a, b))
}

# One warning for the pages whose ARD states two populations and whose
# header did not say which it wants.
.plan_n_choice_warn <- function(pages) {
  warning(paste(c(
    sprintf(paste0("Column header: the ARD states two populations for ",
                   "page%s %s --"), if (length(pages) > 1L) "s" else "",
            paste(sQuote(utils::head(pages, 4L)), collapse = ", ")),
    "  the page's own (e.g. the subjects with that test) and the table's (the analysis set).",
    "  {n} used the page's.  Say which: plan_col_header(values = list(n = \"page\")) or \"table\"",
    "  (tables sheet: header_n = page | table), or both: values = list(n = \"page\", N = \"table\")."),
    collapse = "\n"), call. = FALSE)
}

# `n = "page"` / `"table"` / list of them, as the tables sheet writes it;
# NULL for anything else (numbers, a function, TRUE).
.plan_scope_text <- function(n) {
  sc <- function(v) is.character(v) && length(v) == 1L &&
    v %in% c("page", "table")
  if (sc(n)) return(n)
  if (is.list(n) && length(n) && !is.null(names(n)) &&
      all(vapply(n, sc, NA))) {
    return(paste(paste(names(n), "=", unlist(n)), collapse = " | "))
  }
  NULL
}

.plan_to_pages <- function(plan, tbl) {
  hdr <- .plan_merge(.plan_of(plan, "header"))
  n_req <- .plan_header_n(hdr)
  nvals <- .plan_n_values(plan, n_req)

  tbl <- .plan_digits_display(plan, tbl)

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
    a <- list(vars = stub$vars, label = stub$name, indent = stub$indent,
              group_summary = stub$group_summary)
    a <- a[!vapply(a, is.null, logical(1L))]
    tbl <- do.call(stub_cols, c(list(data = tbl), a))
  }

  lst <- .plan_merge(.plan_of(plan, "listing"))
  rtf <- .plan_rtf_args(plan, tbl)
  if (length(lst)) {
    cols <- lst$cols; lst$cols <- NULL
    lst <- lst[!vapply(lst, is.null, logical(1L))]
    # plan_blanks(where = "records") is a listing's record separator
    if (identical(rtf$blank_rows, "records")) {
      lst$blank_row <- TRUE
      rtf$blank_rows <- NULL
    }
    rtf$listing <- do.call(listing_spec, c(list(cols = cols), lst))
  } else if (identical(rtf$blank_rows, "records")) {
    .ard_stop(paste0(
      "plan_blanks(where = \"records\") separates the records of a listing, ",
      "and this plan\n  has no plan_listing().  Between the groups of a ",
      "table is where = \"between_groups\"."))
  }
  if (length(stub) && !before) {
    rtf$stub <- do.call(stub_spec, c(
      list(stub$vars),
      Filter(Negate(is.null), list(label = stub$name, indent = stub$indent,
                                   group_summary = stub$group_summary))))
    rtf <- rtf[!vapply(rtf, is.null, logical(1L))]
  }
  # done here, where the final column count is known
  if (!is.null(rtf$col_rel_width)) {
    w <- rtf$col_rel_width
    nfinal <- ncol(tbl) -
      (if (!is.null(rtf$stub)) length(rtf$stub$vars) - 1L else 0L) -
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
    if (!is.null(rtf$stub)) {
      .ard_stop(paste0(
        "plan_cell_style() needs plan_stub(before = TRUE).\n",
        "  Folded inside as_rtftables(), the stub adds group heading rows ",
        "the conditions\n  never saw, so a style would land on the ",
        "wrong row."))
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
  # what tfl_as_table_spec() reads back: the columns and widths before the
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
      h <- if (inherits(hdr$header, "plan_header_cells"))
             .plan_header_cells_resolve(hdr$header, names(page_d), sc, plan)
           else if (!is.function(hdr$header)) hdr$header
           else if (length(formals(hdr$header)) >= 2L)
             hdr$header(nv, tbl)
           else hdr$header(nv)
      list(raw = h, filled = .plan_header_fill(
        h, nv, sc, length(page_d) - length(sc), .plan_sep(plan),
        total_col = .plan_total_spec(plan)$label))
    }
    first_d <- if (inherits(out, "rtftable")) out$data else out[[1L]]$data
    # Pages split by a group value (a lab parameter, a visit) each
    # describe their own population, so each page's `{n}` is read from
    # that page's rows of the ARD -- the subjects with an ALT result are
    # not the subjects with a haemoglobin one.
    pg <- if (is.null(n_req) || inherits(out, "rtftable")) NULL
          else .plan_page_group(plan, names(out))
    if (!is.null(pg)) {
      choice <- character(0)
      said <- character(0)
      for (i in seq_along(out)) {
        nv <- if (is.null(pg$value[[i]])) nvals
              else .plan_n_values(plan, n_req, page = list(
                col = pg$col, value = pg$value[[i]]))
        chose <- if (is.list(nv) && !is.numeric(nv))
          any(vapply(nv, function(z) isTRUE(attr(z, "choice")), NA))
          else isTRUE(attr(nv, "choice"))
        if (chose) choice <- c(choice, names(out)[i])
        # the same missing number on every page is one warning, not one
        # per page
        b <- withCallingHandlers(build(out[[i]]$data, nv),
          warning = function(w) {
            said <<- unique(c(said, conditionMessage(w)))
            invokeRestart("muffleWarning")
          })
        if (i == 1L) .plan_remember(plan, "header_raw", b$raw)
        args <- list(x = out[[i]], b$filled)
        if (is.data.frame(hdr$values)) args$values <- hdr$values
        out[[i]] <- do.call(set_col_header, args)
      }
      for (w in said) warning(w, call. = FALSE)
      if (length(choice)) .plan_n_choice_warn(choice)
    } else {
      b <- build(first_d, nvals)
      .plan_remember(plan, "header_raw", b$raw)
      # the tokens and the short-row rule, on whatever came back
      args <- list(x = out, b$filled)
      if (is.data.frame(hdr$values)) args$values <- hdr$values
      out <- do.call(set_col_header, args)
    }
  }

  if (length(colset$decimal)) {
    first_d <- if (inherits(out, "rtftable")) out$data else out[[1L]]$data
    dc <- .plan_col_names(colset$decimal, names(first_d), spread)
    if (length(dc)) out <- set_decimal_split(out, cols = dc)
  }
  for (l in .plan_of(plan, "restyle")) {
    a <- l$args
    if (is.character(a$cols)) {
      first_d <- if (inherits(out, "rtftable")) out$data else out[[1L]]$data
      a$cols <- .plan_col_names(a$cols, names(first_d), spread)
    }
    f <- get(l$fun, envir = asNamespace("rtfreporter"), mode = "function")
    out <- do.call(f, c(list(out), a))
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
  cp <- .plan_colpages_args(cp)
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

# The populations a header's tokens ask the plan for: `values`, unless it
# is a table of per-page values (that one is set_col_header()'s own).  A
# single `n` entry is the value itself -- `list(n = TRUE)` is `TRUE` --
# so the one-token header resolves exactly as it always has.
.plan_header_n <- function(hdr) {
  v <- hdr$values
  if (is.null(v) || is.data.frame(v)) return(NULL)
  if (is.list(v) && identical(names(v), "n")) return(v[["n"]])
  v
}

# `plan_digits()` on a finished table: a key that names a column formats
# that column, and `.rows` formats the value columns by the value of the
# label column -- `.rows = c(Mean = 2, SD = "3s")`.  A number is the
# decimals and "3s" the significant digits, as everywhere else in the plan.
.plan_digits_display <- function(plan, tbl) {
  dig <- .plan_merge(.plan_of(plan, "digits"))
  cols <- if (identical(plan$kind, "wide")) setdiff(names(dig), ".rows")
          else plan$cache$digit_cols %||% character(0)
  if (!length(cols) && is.null(dig[[".rows"]])) return(tbl)
  rnd <- .plan_merge(.plan_of(plan, "round"))$rounding
  one <- function(d) {
    d <- d[[1L]]
    if (is.character(d) && grepl("^[0-9]+s$", trimws(d))) {
      list(signif = as.integer(sub("s$", "", trimws(d))))
    } else list(digits = as.integer(d))
  }
  for (k in cols) {
    tbl <- do.call(fmt_numeric, c(list(data = tbl, cols = k, rounding = rnd),
                                  one(dig[[k]])))
  }
  r <- dig[[".rows"]]
  if (!is.null(r)) {
    lb <- .plan_label_name(plan)
    if (!length(lb) || !lb %in% names(tbl)) {
      .ard_stop(paste0(
        "plan_digits(.rows = ) is keyed by the value of the label column, ",
        "and this table has none.\n  Columns: ",
        paste(utils::head(names(tbl), 8L), collapse = ", ")))
    }
    tbl <- fmt_numeric(tbl, cols = .plan_spread_cols(plan, tbl), by = lb,
                       formats = lapply(as.list(r), one), rounding = rnd)
  }
  tbl
}

# A digits key must reach something: a variable (checked before the
# table is made) or a column of the table.  Silence is how a typo
# survives review -- "AST = 3" on a frame whose variable column is PARAM
# does nothing -- so a key that reaches nothing is an error.
.plan_check_digit_cols <- function(plan, tbl, keys, vars = FALSE) {
  miss <- setdiff(keys, names(tbl))
  if (!length(miss)) return(invisible(TRUE))
  .ard_stop(paste0(
    "plan_digits() names ", paste(sQuote(miss), collapse = ", "),
    ", which matched nothing.\n",
    "  A key is ",
    if (vars) "an analysis variable, a context, a kind (continuous / categorical), " else "",
    "a column of the table, `.rows` or \"default\".\n  Columns: ",
    paste(utils::head(names(tbl), 8L), collapse = ", ")))
}

# The `cols` keys are joined with the separator into the value columns'
# names, and as_rtftables() splits them back on it to build the spanning
# header.  A key value that contains it could not be split back.
.plan_check_key_sep <- function(plan, x) {
  if (!is.data.frame(x) || is.null(plan$roles$cols)) return(invisible(TRUE))
  sep <- .plan_sep(plan)
  keys <- intersect(unname(as.character(unlist(plan$roles$cols))), names(x))
  for (k in keys) {
    v <- unique(as.character(x[[k]]))
    bad <- v[!is.na(v) & grepl(sep, v, fixed = TRUE)]
    if (length(bad)) {
      .ard_stop(sprintf(paste0(
        "%s has a value containing the key separator %s (%s).\n",
        "  The column keys are joined with it, so the name could not be ",
        "split back.\n  Recode the value, or choose another separator ",
        "with plan_columns(sep = )."), sQuote(k), dQuote(sep, FALSE),
        sQuote(bad[1L])))
    }
  }
  invisible(TRUE)
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
.plan_header_fill <- function(h, nvals, cols, n_lead, sep = NULL,
                              total_col = NULL) {
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
    # a Total column (plan_total()) is the sum of the others, not a part
    parts <- setdiff(cols, total_col)
    over <- setdiff(over, total_col)
    if (!length(over)) {
      keep <- intersect(parts, nm)
      # a vector keyed by something else entirely: every entry
      if (!length(keep)) keep <- setdiff(nm, total_col)
      else if (length(keep) < length(parts)) return(NA)
    } else {
      keep <- intersect(over, nm)
      # a column with no number makes the sum a wrong number, not a
      # smaller one
      if (!length(keep) ||
          length(keep) < length(intersect(over, parts))) return(NA)
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
    paste0("    plan_col_header(values = c(\"Placebo\" = 86, ...))  -- names at any ",
           "level (\"Placebo\", \"Placebo____F\"),"),
    "    or values = function(data) ... , or values = pull_ard(ard, cols, variable = \"AGE\").")
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

#' What a plan declares, and what it resolved to
#'
#' Reads a plan from the outside: its roles, the layers each verb declared
#' (merged per kind, a later layer winning, as the resolver merges them),
#' and --- because reading a plan means running it --- what its pages came
#' out as: the column names, the value columns, the widths, the column
#' header as cell rows, the pages themselves.  It is the one way another
#' package reads a plan: `tflspec::tfl_as_table_spec()` writes a workbook from
#' nothing but this and [plan_apply()].
#'
#' @param plan An [table_plan()].
#' @return A list: `data` (the plan's data); `roles`; `label` (the label
#'   column's name, or none); `group_col`; `declared` (the kinds of layer,
#'   in the order declared); `layers` (by kind, the merged fields; for
#'   `after` and `restyle` one entry per call); `cells` (the cell
#'   templates, each parsed into its label rows and template chains ---
#'   `NULL` when the statistics are rows); `columns` (`names`, `spread`,
#'   `page_names`, `widths`); `header` (`source`: `"cells"` for a header
#'   given as cell rows, `"resolved"` for one the plan built; `cells`: the
#'   header as cell rows, a data frame; `n_text`: the `{n}` scope as text,
#'   or `NULL`; `literal_n`: whether `n` was a number written in); and
#'   `pages`.
#'
#' @seealso [plan_apply()], `tflspec::tfl_as_table_spec()`
#' @export
plan_layers <- function(plan) {
  if (!inherits(plan, "table_plan")) .ard_stop("Expected a table_plan.")
  a <- plan_apply(plan, "args")
  pages <- plan_apply(plan, "pages")
  first <- if (inherits(pages, "rtftable")) pages else pages[[1L]]
  seen <- plan$cache[["pre_cols"]]
  page_names <- names(first$data)
  pnames <- seen$names %||% page_names
  spread <- seen$spread %||% intersect(.plan_spread_cols(plan, first$data),
                                       pnames)
  kinds <- vapply(plan$layers, `[[`, "", "kind")
  layers <- list()
  for (k in unique(kinds)) {
    layers[[k]] <- if (k %in% c("after", "restyle")) .plan_of(plan, k)
                   else if (k %in% c("levels", "labels", "nest"))
                     .plan_merge(.plan_of(plan, k), deep = k)
                   else .plan_merge(.plan_of(plan, k))
  }
  s <- a$widen
  cells <- NULL
  cm <- s$cells
  if (!is.null(cm) && !identical(s$stats, "rows")) {
    is_map <- is.list(cm) && !inherits(cm, "cell_rows") &&
      any(nzchar(names(cm) %||% ""))
    if (!is_map) cm <- list(default = cm)
    cells <- lapply(names(cm), function(k) list(
      variable = if (identical(k, "default")) NA_character_ else
        sub("\r.*$", "", k),
      context = if (grepl("\r", k, fixed = TRUE)) sub("^.*\r", "", k) else
        NA_character_,
      entry = .ard_cell_entry(cm[[k]])))
  }
  hd <- layers$header
  hn <- .plan_header_n(hd)
  header <- list(source = NULL, cells = NULL, n_text = .plan_scope_text(hn),
                 literal_n = !is.null(hn) && !isTRUE(hn) &&
                   is.null(.plan_scope_text(hn)))
  if (inherits(hd$header, "plan_header_cells")) {
    header$source <- "cells"
    header$cells <- do.call(rbind, lapply(hd$header$cells, function(cc)
      data.frame(line = as.character(cc$line), cols = cc$cols,
                 span = cc$span %||% NA,
                 text = if (is.null(cc$text)) NA else cc$text,
                 align = cc$align %||% NA,
                 bold = if (is.null(cc$bold)) NA else as.character(cc$bold),
                 border_top = cc$border_top %||% NA,
                 border_bottom = cc$border_bottom %||% NA,
                 stringsAsFactors = FALSE)))
  } else if (!is.null(plan$cache[["header_raw"]])) {
    header$source <- "resolved"
    header$cells <- .plan_header_rows(plan$cache[["header_raw"]], pnames,
                                      spread, plan)
  }
  list(data = plan$data, roles = plan$roles, label = .plan_label_name(plan),
       group_col = .plan_group_col(plan), declared = kinds, layers = layers,
       cells = cells,
       columns = list(names = pnames, spread = spread, page_names = page_names,
                      widths = seen$widths %||% first$col_rel_width),
       header = header, pages = pages)
}

#' Write the plan for you
#'
#' The counterpart of `plan_template(form = "widen")` for the deferred form: reads an ARD
#' and prints a runnable [table_plan()] pipeline, filled in with the keys,
#' hierarchy, contexts and statistics it actually found.
#'
#' It writes **both halves** --- the ARD to the table, and the table to the
#' RTF pages --- because a plan that stops at the table is a plan that made
#' you look up `stub_vars` and the header somewhere else.  The display half
#' is a starting point and is meant to be edited: only `plan_stub()` is
#' derivable from the ARD, and the rest are display decisions nothing can
#' guess.
#'
#' @param x A cards / cardx ARD (or anything [normalize_ard()] takes).
#' @param cols The column keys, as [table_plan()]'s `cols`; `NULL` takes the
#'   first key the ARD has.
#' @param hierarchy The hierarchy columns, as [normalize_ard()]'s.
#' @param form `"plan"` (default) writes the deferred form, a [table_plan()]
#'   pipeline; `"widen"` writes the immediate form, [normalize_ard()] into
#'   [widen_ard()] with the seam left open for a `dplyr::mutate()`.
#' @param file When given, the code is also written there.
#' @param pipe `"|>"` or `"%>%"`; `NULL` follows
#'   `getOption("rtfreporter.ard_pipe")`, then RStudio's own preference,
#'   then `%>%`.
#' @return The generated code, as a character vector, invisibly.
#'
#' @examples
#' if (requireNamespace("cards", quietly = TRUE)) {
#'   ard <- cards::ard_stack(
#'     cards::ADSL, .by = ARM,
#'     cards::ard_summary(variables = AGE),
#'     cards::ard_tabulate(variables = SEX))
#'   plan_template(ard, cols = "ARM")
#' }
#' @seealso [table_plan()], [plan_apply()]
#' @export
plan_template <- function(x, cols = NULL, hierarchy = character(),
                          form = c("plan", "widen"), file = NULL,
                          pipe = NULL) {
  ard <- x
  form <- match.arg(form)
  if (identical(form, "widen")) {
    return(.ard_template(ard, cols = cols, hierarchy = hierarchy, file = file,
                         pipe = pipe))
  }
  op <- .ard_pipe_op(pipe)
  f  <- .ard_template_facts(ard, cols, hierarchy)
  q <- f$q; vecq <- f$vecq; tok <- f$tok

  L <- c(
    .ard_bar("", "="),
    "#  generated by rtfreporter::plan_template()",
    "#",
    paste0("#  keys       : ", paste(f$keys, collapse = ", ")),
    paste0("#  variables  : ",
           if (length(f$vars)) paste(utils::head(f$vars, 12), collapse = ", ")
           else "(none -- the rows come from `hierarchy`)"),
    paste0("#  kinds      : ", paste(f$kinds, collapse = ", ")),
    "#",
    "#  normalize_ard() runs; the plan does not, until it is asked.",
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
         if (length(norm)) .plan_call("normalize_ard", norm, op)
         else paste0("  rtfreporter::normalize_ard() ", op))
  L <- c(L, .plan_call("table_plan", spread, op))
  # already indented and comma-ed: a `c(` entry spans several lines,
  # so it cannot go through .plan_call(), which commas every argument.
  L <- c(L, "  rtfreporter::plan_cells(", .plan_cell_lines(f),
         paste0("  ) ", op))

  # -- 2. the display half -----------------------------------------------
  L <- c(L, "", .ard_bar("2. the display half -- edit this"))
  n_ok <- !is.null(tryCatch(pull_ard(ard, cols = f$cols),
                            error = function(e) NULL))
  # `plan_stub()` rather than plan_rtf(stub_vars = ): the plan then sees the
  # rows that will be printed, which plan_cell_style() needs.
  # `vars` is left out on purpose: plan_stub() works it out from
  # table_plan(rows = , label = ) less any plan_row_group(col = ).
  L <- c(L, .plan_call("plan_stub",
                       "name = \"row_label\"", op))
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
      c("values = list(n = TRUE)",
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
         "# rtf_tables() takes the plan directly -- plan_apply() is only for",
         "# looking inside:",
         "#",
         "#   doc <- rtf_document() |>",
         "#     rtf_section(page = 1, secinfo = <your header / footer>) |>",
         "#     rtf_tables(p)",
         "#",
         "#   print(p)        what it declares, and how far it goes",
         "#   plan_apply(p)   the object itself")

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

# A resolved header, row by row, as header cells (what plan_layers()
# hands out, and tfl_as_table_spec() writes to the `col_header` sheet).  Literal text
# that is a column's own key value becomes the token; a cell repeated on
# every value column becomes `span = each`; a spanner per value of a key
# becomes `span = <key>`.
.plan_header_rows <- function(h, pnames, spread, p) {
  keys <- unname(as.character(unlist(p$roles$cols)))
  sep <- .plan_sep(p)
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
      line = as.character(li), cols = cols, span = span,
      text = if (nzchar(u$label)) u$label else NA,
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


# A plan is a table source like a gt table: rtf_tables(doc, plan) and
# as_rtftables(plan) both work through the generic.
#' @export
as_rtftables.table_plan <- function(x, ...) plan_apply(x, "pages")

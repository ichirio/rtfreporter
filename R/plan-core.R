# ============================================================================
#  SPIKE (design/plan-resolver) -- deferred layers, resolved once
# ============================================================================
#
#  NOT part of the public API.  Nothing here is exported yet and nothing else
#  in the package calls it.  Deleting these files removes the experiment
#  without trace.
#
#  ---------------------------------------------------------------------------
#  The question this spike exists to answer
#  ---------------------------------------------------------------------------
#
#  `as_rtftables()` passes grouping, blank rows and the page budget to separate
#  subsystems as separate arguments.  Two measured consequences:
#
#    * `group_col` reaches the paginator and the blank-row resolver by
#      different routes, so putting it inside `page_split_group_safe()` changes
#      where blanks land -- silently, with no error;
#    * `count_blank_rows` exists at all, which is pagination leaking into the
#      blank-row layer: someone has to tell the row budget what the blank layer
#      did.
#
#  Both are symptoms of the same thing: three things that depend on each other
#  are resolved independently.  The claim under test is that ONE resolver,
#  running the layers in dependency order over data it never rewrites, makes
#  both problems structurally impossible rather than merely fixed.
#
#  If this file cannot express that cleanly, option 1 does not stand up and the
#  branch is thrown away.
#
#  ---------------------------------------------------------------------------
#  Merge rule
#  ---------------------------------------------------------------------------
#
#  LAST WRITER WINS, PER FIELD -- the rule `style_verbs.R` already states for
#  the whole package, and what ggplot2 users expect of a re-declared setting.
#  Every layer argument therefore defaults to NULL meaning "not supplied": only
#  a field the caller actually passed overwrites the accumulated value, so
#
#      plan_pages(max_rows = 21) |> plan_pages(keep_groups = FALSE)
#
#  keeps `max_rows`.  Defaults are applied at RESOLVE time, never at
#  declaration time, which is what keeps "not supplied" distinguishable.

# ── the plan ───────────────────────────────────────────────────────────────

#' @keywords internal
rtf_plan <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  structure(list(data = data, layers = list()), class = "rtf_plan")
}

# Merge a layer's supplied fields onto whatever is already declared for that
# kind.  `fields` carries only what the caller passed (NULLs already dropped).
.plan_set <- function(plan, kind, fields) {
  if (!inherits(plan, "rtf_plan")) {
    stop("Expected an rtf_plan; pipe from rtf_plan(data).", call. = FALSE)
  }
  fields <- fields[!vapply(fields, is.null, logical(1L))]
  cur <- plan$layers[[kind]]
  if (is.null(cur)) cur <- list()
  for (nm in names(fields)) cur[[nm]] <- fields[[nm]]
  plan$layers[[kind]] <- cur
  plan
}

# Replace a layer wholesale.  The roles layer does its own per-column merge,
# so it hands the finished table down rather than field-merging here.
.plan_set_raw <- function(plan, kind, value) {
  if (!inherits(plan, "rtf_plan")) {
    stop("Expected an rtf_plan; pipe from rtf_plan(data).", call. = FALSE)
  }
  plan$layers[[kind]] <- value
  plan
}

.plan_get <- function(plan, kind) plan$layers[[kind]]

# ── layers ─────────────────────────────────────────────────────────────────

#' @keywords internal
plan_blanks <- function(plan, where = NULL, first = NULL, last = NULL) {
  .plan_set(plan, "blanks", list(where = where, first = first, last = last))
}

#' @keywords internal
# Explicit page breaks, the counterpart of a row budget.  An explicit break is
# not a suggestion, so it wins over `max_rows`.
#
# TWO spellings, because one of them is always the wrong guess and a silent
# off-by-one page break is a bad way to find out.  as_rtftables(split_rows = )
# means break_before; the plan's own `plan_blanks(where = )` means "after".
# Naming them removes the ambiguity instead of documenting it.
#
#   break_after  = 8        pages 1-8, 9-n
#   break_before = 8        pages 1-7, 8-n     (as_rtftables split_rows = 8)
#
# `groups` says what a page budget may do to a group -- one setting where
# as_rtftables() has three strategy names:
#
#   "keep"   never split a group          (split = "group_safe")
#   "prefer" fill the page but cut at a boundary when one is near
#                                          (split = "group_force")
#   "split"  cut wherever the budget runs out
#
# `per_group = TRUE` gives each group its own page, named after it
# (split = "by_value").  `split_fn` hands the whole decision to a function of
# (df, info, max_rows, cont_label, group_idx, min_group_rows), the same
# contract as_rtftables() accepts for a custom `split`.
plan_pages <- function(plan, max_rows = NULL, break_after = NULL,
                       break_before = NULL, groups = NULL, per_group = NULL,
                       cont_label = NULL, split_fn = NULL,
                       min_group_rows = NULL, count_blanks = NULL) {
  if (!is.null(break_after) && !is.null(break_before)) {
    stop("Give `break_after` or `break_before`, not both.", call. = FALSE)
  }
  if (!is.null(groups)) {
    groups <- match.arg(groups, c("keep", "prefer", "split"))
  }
  .plan_set(plan, "pages",
            list(max_rows = max_rows, break_after = break_after,
                 break_before = break_before, groups = groups,
                 per_group = per_group, cont_label = cont_label,
                 split_fn = split_fn,
                 min_group_rows = min_group_rows, count_blanks = count_blanks))
}

# ── the resolver ───────────────────────────────────────────────────────────
#
# One pass, in dependency order.  Each stage reads the resolved output of the
# stage before it -- never a raw argument that some other stage also received.

#' @keywords internal
resolve_plan <- function(plan) {
  if (!inherits(plan, "rtf_plan")) {
    stop("Expected an rtf_plan.", call. = FALSE)
  }
  d <- plan$data
  n <- nrow(d)

  # 1. columns -- every reference is a NAME until this point.  One projection
  #    of the final layout, one name -> position map, and nothing downstream
  #    ever re-indexes anything.
  columns <- .plan_resolve_columns(.plan_get(plan, "roles"), d)

  # 2. rows -- the stub's label rows enter here, and from this point every
  #    stage works in OUTPUT row coordinates.  One map, `rows$src`, is the
  #    only translation back to the source.
  rows <- .plan_resolve_rows(columns, d, .plan_get(plan, "style"))

  # 3. grouping -- resolved ONCE, on the output rows, from the column table.
  #    Everything downstream reads `groups`, so there is no second place for a
  #    group column to be declared and disagree.
  groups <- .plan_resolve_groups(columns, rows$body)

  # 4. blank rows -- may consult the grouping, and nothing else.
  blanks <- .plan_resolve_blanks(.plan_get(plan, "blanks"), rows$body, groups)

  # 5. pages -- consumes both.  The row budget can see the blanks because they
  #    are already resolved, so no argument has to describe them to it.
  pg <- .plan_resolve_pages(.plan_get(plan, "pages"), rows$n, groups, blanks,
                            rows$body)
  pages <- pg$pages

  # 6. style -- table-wide settings plus per-column options addressed BY NAME
  #    and placed through the column map, so col_spec never needs re-indexing.
  style <- .plan_resolve_style(.plan_get(plan, "style"),
                               .plan_get(plan, "roles"), columns,
                               plan$source$kw)

  # 7. the source's own metadata, placed through the same column map -- the
  #    adapter read it in SOURCE coordinates and never has to know what the
  #    stub merged or the hidden columns removed.
  header <- .plan_resolve_header(plan$source$kw %||% list(), columns)

  structure(list(columns = columns, rows = rows, groups = groups,
                 blanks = blanks, pages = pages,
                 page_data = pg$data, page_names = pg$names,
                 style = style, header = header, source = plan$source,
                 nrow = rows$n, nrow_source = n),
            class = "rtf_resolution")
}

# The grouping column is read out of the resolved column table -- never from a
# layer of its own, which is what made it possible to declare it twice.
.plan_resolve_groups <- function(columns, body) {
  if (is.null(columns$group)) return(NULL)
  # The column map already knows where the grouping column ended up -- if it
  # was folded into a merged stub, the map says so and the answer is the stub
  # column.  This is the payoff of resolving columns by name first: nothing
  # here has to know what the stub did.
  # BODY coordinates: a hidden carrier column may group the table without ever
  # being printed, which is why the projection keeps both views.
  idx <- unname(columns$body_map[[columns$group]])
  if (is.na(idx)) {
    stop("The grouping column \"", columns$group, "\" is not in the table.",
         call. = FALSE)
  }
  info <- .compute_group_info(body, idx, group_by = columns$mode %||% "auto")
  info$col  <- idx
  info$mode <- columns$mode %||% "auto"
  info
}

# Blank positions, as "insert a blank AFTER this body row".
#
# Delegated to .resolve_pagewise_blanks(), the resolver as_rtftables() already
# uses, so every spelling works here without a second implementation:
# positions, "between_groups", blank_rows_by_change(), blank_rows_by_rule(),
# and a list mixing them.  The plan supplies the resolved GROUPING rather than
# a column argument, which is the whole difference -- and the reason the two
# can no longer disagree about what a group is.
.plan_resolve_blanks <- function(spec, d, groups) {
  if (is.null(spec)) return(integer(0))
  n <- nrow(d)
  where <- spec$where
  pos <- integer(0)

  if (!is.null(where)) {
    needs_groups <- function(s) {
      if (is.character(s) && length(s) == 1L && s == "between_groups") return(TRUE)
      if (is.list(s) && !inherits(s, "rtf_blank_rows_by_change") &&
          !inherits(s, "rtf_blank_rows_by_rule")) {
        return(any(vapply(s, needs_groups, logical(1L))))
      }
      FALSE
    }
    if (needs_groups(where) && is.null(groups)) {
      stop("`plan_blanks(\"between_groups\")` needs a grouping; ",
           "declare plan_group() first.", call. = FALSE)
    }
    pos <- .resolve_pagewise_blanks(
      where, d,
      group_idx = if (is.null(groups)) NULL else groups$col,
      group_by  = groups$mode %||% "auto")
  }

  if (isTRUE(spec$first)) pos <- c(0L, pos)
  if (isTRUE(spec$last))  pos <- c(pos, n)
  sort(unique(as.integer(pos[pos >= 0L & pos <= n])))
}

# Cut the body into pages.
#
# The four named strategies are DELEGATED to the functions as_rtftables()
# already uses -- .split_by_rows(), .split_group_safe(), .split_group_force()
# and .split_by_value() -- so the plan cannot quietly paginate differently.
# They return data.frames, and `cont_label` writes into a chunk's group cell,
# which a list of row indices cannot express; so a delegated split keeps its
# chunks and records the row indices alongside for the row map.
#
# `blanks` is already resolved when this runs, which is what lets the budget
# charge for the blank lines a page will print without being told.
.plan_resolve_pages <- function(spec, n, groups, blanks, body = NULL) {
  none <- list(pages = list(seq_len(n)), data = NULL, names = NULL)
  if (n == 0L) return(list(pages = list(integer(0)), data = NULL, names = NULL))
  if (is.null(spec)) return(none)

  # Explicit breaks win: they say exactly where the pages end.
  brk <- if (!is.null(spec$break_after)) as.integer(spec$break_after)
         else if (!is.null(spec$break_before)) as.integer(spec$break_before) - 1L
         else NULL
  if (!is.null(brk)) {
    cuts <- sort(unique(brk))
    cuts <- cuts[cuts >= 1L & cuts < n]
    return(list(pages = Map(seq.int, c(1L, cuts + 1L), c(cuts, n)),
                data = NULL, names = NULL))
  }

  strat <- if (!is.null(spec$split_fn)) spec$split_fn
           else if (isTRUE(spec$per_group)) .split_by_value
           else if (is.null(spec$max_rows)) NULL
           else switch(spec$groups %||% "keep",
                       keep   = .split_group_safe,
                       prefer = .split_group_force,
                       split  = NULL)
  if (is.null(strat)) {
    if (is.null(spec$max_rows)) return(none)
    return(list(pages = .plan_greedy_pages(spec, n, groups, blanks),
                data = NULL, names = NULL))
  }

  # Delegate.  A row-index column rides along so the chunks can be mapped back
  # -- the same trick as_rtftables() uses to keep cell_styles aligned.
  idxc <- ".__plan_idx__"
  df <- body
  df[[idxc]] <- seq_len(n)
  info <- groups %||% list(id = rep(1L, n), label = rep("", n),
                           headers = c(TRUE, rep(FALSE, max(0L, n - 1L))))
  chunks <- strat(df, info, spec$max_rows, spec$cont_label %||% " (Cont.)",
                  if (is.null(groups)) NULL else groups$col,
                  as.integer(spec$min_group_rows %||% 2L))
  pages <- lapply(chunks, function(ch) as.integer(ch[[idxc]]))
  data  <- lapply(chunks, function(ch) {
    ch[[idxc]] <- NULL
    rownames(ch) <- NULL
    ch
  })
  nms <- NULL
  if (isTRUE(spec$per_group) && !is.null(groups)) {
    nms <- vapply(pages, function(ix) {
      lb <- groups$label[ix[[1L]]]
      if (is.na(lb)) "" else as.character(lb)
    }, character(1L))
  }
  list(pages = pages, data = data, names = nms)
}

# The plain budget: fill a page, charging for the blank lines it will print.
.plan_greedy_pages <- function(spec, n, groups, blanks) {
  max_rows <- spec$max_rows
  min_grp  <- as.integer(spec$min_group_rows %||% 2L)

  cost <- rep(1L, n)
  if (isTRUE(spec$count_blanks) && length(blanks)) {
    inner <- blanks[blanks >= 1L & blanks <= n]
    cost[inner] <- cost[inner] + 1L
  }
  gid <- if (!is.null(groups)) groups$id else seq_len(n)
  legal <- rep(TRUE, n)
  if (min_grp > 1L && !is.null(groups)) {
    for (i in seq_len(n - 1L)) {
      if (gid[i] != gid[i + 1L]) next
      before <- sum(gid[seq_len(i)] == gid[i])
      after  <- sum(gid == gid[i + 1L]) - sum(gid[seq_len(i)] == gid[i + 1L])
      if (before < min_grp || after < min_grp) legal[i] <- FALSE
    }
  }

  pages <- list()
  start <- 1L
  while (start <= n) {
    used <- 0L
    last_legal <- NA_integer_
    i <- start
    while (i <= n) {
      used <- used + cost[i]
      if (used > max_rows && i > start) break
      if (legal[i]) last_legal <- i
      i <- i + 1L
    }
    end <- if (i > n) n
           else if (!is.na(last_legal) && last_legal >= start) last_legal
           else i - 1L
    if (end < start) end <- start
    pages[[length(pages) + 1L]] <- start:end
    start <- end + 1L
  }
  pages
}

# ── inspection ─────────────────────────────────────────────────────────────

# Plain functions, not S3 print methods: an S3 method needs a line in the
# hand-managed NAMESPACE, and this branch is long-lived and expects to be
# rebased onto main repeatedly.  Every file the spike touches is a merge
# conflict waiting to happen, so it touches only its own.
#' @keywords internal
show_plan <- function(x, ...) {
  cat("<rtf_plan>", nrow(x$data), "rows x", ncol(x$data), "cols\n")
  if (length(x$layers) == 0L) {
    cat("  (no layers)\n")
  } else {
    for (k in names(x$layers)) {
      f <- x$layers[[k]]
      cat("  ", k, ": ",
          paste(names(f), vapply(f, function(v)
            paste(as.character(v), collapse = ","), character(1L)),
            sep = "=", collapse = "  "), "\n", sep = "")
    }
  }
  invisible(x)
}

#' @keywords internal
show_resolution <- function(x, ...) {
  cat("<rtf_resolution>", x$nrow, "printed rows from", x$nrow_source,
      "source rows\n")
  cat("  columns:", paste(x$columns$names, collapse = ", "), "\n")
  cat("  groups :", if (is.null(x$groups)) "none"
      else paste(length(unique(x$groups$id)), "groups"), "\n")
  cat("  blanks :", if (length(x$blanks)) paste(x$blanks, collapse = ",")
      else "none", "\n")
  cat("  pages  :", length(x$pages), "->",
      paste(vapply(x$pages, length, integer(1L)), collapse = ","), "\n")
  invisible(x)
}

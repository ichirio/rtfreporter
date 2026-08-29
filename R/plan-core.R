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
plan_pages <- function(plan, max_rows = NULL, keep_groups = NULL,
                       min_group_rows = NULL, count_blanks = NULL) {
  .plan_set(plan, "pages",
            list(max_rows = max_rows, keep_groups = keep_groups,
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
  rows <- .plan_resolve_rows(columns, d)

  # 3. grouping -- resolved ONCE, on the output rows, from the column table.
  #    Everything downstream reads `groups`, so there is no second place for a
  #    group column to be declared and disagree.
  groups <- .plan_resolve_groups(columns, rows$body)

  # 4. blank rows -- may consult the grouping, and nothing else.
  blanks <- .plan_resolve_blanks(.plan_get(plan, "blanks"), rows$body, groups)

  # 5. pages -- consumes both.  The row budget can see the blanks because they
  #    are already resolved, so no argument has to describe them to it.
  pages <- .plan_resolve_pages(.plan_get(plan, "pages"), rows$n, groups, blanks)

  # 6. style -- table-wide settings plus per-column options addressed BY NAME
  #    and placed through the column map, so col_spec never needs re-indexing.
  style <- .plan_resolve_style(.plan_get(plan, "style"),
                               .plan_get(plan, "roles"), columns)

  # 7. the source's own metadata, placed through the same column map -- the
  #    adapter read it in SOURCE coordinates and never has to know what the
  #    stub merged or the hidden columns removed.
  header <- .plan_resolve_header(plan$source$kw %||% list(), columns)

  structure(list(columns = columns, rows = rows, groups = groups,
                 blanks = blanks, pages = pages, style = style,
                 header = header, source = plan$source,
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
  info$col <- idx
  info
}

# Blank positions, as "insert a blank AFTER this body row".
.plan_resolve_blanks <- function(spec, d, groups) {
  if (is.null(spec)) return(integer(0))
  n <- nrow(d)
  where <- spec$where
  pos <- integer(0)

  if (is.character(where) && length(where) == 1L &&
      identical(where, "between_groups")) {
    if (is.null(groups)) {
      stop("`plan_blanks(\"between_groups\")` needs a grouping; ",
           "declare plan_group() first.", call. = FALSE)
    }
    id <- groups$id
    if (length(id) > 1L) {
      pos <- which(c(FALSE, id[-1L] != id[-length(id)])) - 1L
    }
  } else if (is.numeric(where)) {
    pos <- as.integer(where)
  } else if (!is.null(where)) {
    stop("`where` must be \"between_groups\" or row positions.", call. = FALSE)
  }

  if (isTRUE(spec$first)) pos <- c(0L, pos)
  if (isTRUE(spec$last))  pos <- c(pos, n)
  sort(unique(pos[pos >= 0L & pos <= n]))
}

# Cut the body into pages.  `blanks` is already resolved, so the budget can
# charge for the blank lines a page will actually print -- the job
# `count_blank_rows` used to do by hand, from outside.
.plan_resolve_pages <- function(spec, n, groups, blanks) {
  if (n == 0L) return(list(integer(0)))
  max_rows <- spec$max_rows
  if (is.null(spec) || is.null(max_rows)) return(list(seq_len(n)))

  keep_groups  <- spec$keep_groups %||% TRUE
  min_grp      <- as.integer(spec$min_group_rows %||% 2L)
  count_blanks <- isTRUE(spec$count_blanks)

  # Line cost of body row i: itself, plus a blank printed after it.
  cost <- rep(1L, n)
  if (count_blanks && length(blanks)) {
    inner <- blanks[blanks >= 1L & blanks <= n]
    cost[inner] <- cost[inner] + 1L
  }

  gid <- if (!is.null(groups)) groups$id else seq_len(n)
  # A cut is legal after row i when it does not fall inside a group, unless
  # groups are not being kept whole.
  legal <- if (keep_groups) c(gid[-n] != gid[-1L], TRUE) else rep(TRUE, n)
  # Orphan control: leaving fewer than `min_grp` rows of a group on either side
  # of a cut is what min_group_rows exists to prevent.  With whole groups kept
  # this never arises, so it only bites when keep_groups is FALSE.
  if (!keep_groups && min_grp > 1L && !is.null(groups)) {
    for (i in seq_len(n - 1L)) {
      if (!legal[i]) next
      before <- sum(gid[seq_len(i)] == gid[i])
      after  <- sum(gid == gid[i + 1L]) -
                sum(gid[seq_len(i)] == gid[i + 1L])
      if (gid[i] == gid[i + 1L] && (before < min_grp || after < min_grp)) {
        legal[i] <- FALSE
      }
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
           else i - 1L                      # a group larger than the budget
    if (end < start) end <- start           # never make no progress
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

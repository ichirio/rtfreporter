# ============================================================================
#  SPIKE (design/plan-resolver) -- column roles, resolved by name
# ============================================================================
#
#  Step 2 of the layered-plan design.  Nothing here is exported.
#
#  ---------------------------------------------------------------------------
#  The question this file exists to answer
#  ---------------------------------------------------------------------------
#
#  The package carries EIGHT separate implementations of the same idea --
#  "columns moved, so move every piece of metadata that referred to them":
#
#    .reindex_col_header   .reindex_header_row   .reindex_header_cell
#    .reindex_col_spec     .reindex_row_title    .rtftable_keep_cols
#    .stub_remap_styles    .prepend_stub_header
#
#  They exist because each stage rewrites the data.frame, so every later stage
#  inherits a different coordinate system and has to be told how to translate.
#  The claim under test: if every reference is held BY NAME and positions are
#  computed once, at the end, from a single projection of the final layout,
#  then there is nothing to re-index and the eight collapse to one.
#
#  ---------------------------------------------------------------------------
#  Why roles and not one layer per concern
#  ---------------------------------------------------------------------------
#
#  `plan_group()` began life as its own layer.  That is the mistake this design
#  set out to remove: a grouping column declared in two places is a grouping
#  column that can disagree with itself, which is precisely the as_rtftables()
#  defect.  So grouping is a ROLE a column plays, stored with every other role
#  in one table, and `plan_group()` is sugar that writes into it.  One storage
#  location, therefore no second opinion.

# ── role() ─────────────────────────────────────────────────────────────────

# `...` unnamed  -> role names ("group", "stub", "hide", "carry", ...)
# `...` named    -> options for those roles (order =, mode =, desc =)
#' @keywords internal
role <- function(...) {
  a <- list(...)
  nm <- names(a)
  if (is.null(nm)) nm <- rep("", length(a))
  roles <- unlist(a[!nzchar(nm)], use.names = FALSE)
  roles <- if (is.null(roles)) character(0) else as.character(roles)
  bad <- setdiff(roles, .PLAN_ROLES)
  if (length(bad)) {
    stop("Unknown role", if (length(bad) > 1L) "s" else "", ": ",
         paste0("\"", bad, "\"", collapse = ", "), ".\n  Valid roles: ",
         paste(.PLAN_ROLES, collapse = ", "), ".", call. = FALSE)
  }
  structure(list(roles = roles, opts = a[nzchar(nm)]), class = "rtf_role")
}

.PLAN_ROLES <- c("group", "stub", "hide", "carry", "collapse", "sort",
                 "display")

.as_role <- function(x) {
  if (inherits(x, "rtf_role")) return(x)
  role(x)
}

# ── the roles layer ────────────────────────────────────────────────────────

# Named arguments: column name = role spec.
#
# Merge rule, which the spike had to discover the hard way: a column plays a
# SET of roles, and carries FIELDS of options.  Replacing the whole entry --
# the obvious reading of "last writer wins" -- silently destroyed the stub role
# the moment `plan_group("SOC")` followed `plan_stub(c("SOC", "PT"))`, which is
# never what that pair of calls means.
#
# So: roles accumulate, options are last-writer-wins PER FIELD (the package's
# stated rule, applied where fields actually exist).  Contradictions between
# accumulated roles are caught when the columns are resolved, not silently
# resolved by declaration order.
#' @keywords internal
plan_roles <- function(plan, ...) {
  a <- list(...)
  nm <- names(a)
  if (length(a) && (is.null(nm) || !all(nzchar(nm)))) {
    stop("Every argument to `plan_roles()` must be named with a column name.",
         call. = FALSE)
  }
  cur <- .plan_get(plan, "roles")
  if (is.null(cur)) cur <- list()
  for (k in names(a)) {
    new_r <- .as_role(a[[k]])
    old_r <- cur[[k]]
    if (is.null(old_r)) {
      cur[[k]] <- new_r
    } else {
      opts <- old_r$opts
      for (o in names(new_r$opts)) opts[[o]] <- new_r$opts[[o]]
      cur[[k]] <- structure(
        list(roles = union(old_r$roles, new_r$roles), opts = opts),
        class = "rtf_role")
    }
  }
  .plan_set_raw(plan, "roles", cur)
}

# Drop a column's roles entirely -- the escape hatch for the accumulation
# above, so a declaration can still be taken back.
#' @keywords internal
plan_unset <- function(plan, cols) {
  cur <- .plan_get(plan, "roles")
  if (is.null(cur)) return(plan)
  for (k in as.character(cols)) cur[[k]] <- NULL
  .plan_set_raw(plan, "roles", cur)
}

# Sugar for the commonest declaration.  It writes into the SAME table as
# plan_roles(), so the grouping column has exactly one home.
#' @keywords internal
plan_group <- function(plan, cols = NULL, mode = NULL) {
  if (is.null(cols)) return(plan)
  if (!is.null(mode)) {
    mode <- match.arg(mode, c("auto", "value", "indent", "filled"))
  }
  if (length(cols) != 1L) {
    stop("The spike groups on a single column; got ", length(cols), ".",
         call. = FALSE)
  }
  args <- list(plan)
  args[[as.character(cols)]] <- if (is.null(mode)) role("group")
                                else role("group", mode = mode)
  do.call(plan_roles, args)
}

# Sugar for the stub, preserving declaration order as the hierarchy order.
#' @keywords internal
plan_stub <- function(plan, cols, layout = NULL) {
  cols <- as.character(cols)
  args <- list(plan)
  for (i in seq_along(cols)) {
    args[[cols[i]]] <- if (is.null(layout)) role("stub", order = i)
                       else role("stub", order = i, layout = layout)
  }
  do.call(plan_roles, args)
}

#' @keywords internal
plan_hide <- function(plan, cols) {
  args <- list(plan)
  for (k in as.character(cols)) args[[k]] <- role("hide")
  do.call(plan_roles, args)
}

# ── resolution ─────────────────────────────────────────────────────────────

# Project the FINAL column layout without touching the data, and return the
# one name -> final position map every consumer reads.
#
# Returns:
#   names   final column names, in order
#   map     named integer over the ORIGINAL names: final position, NA if gone
#   stub    stub columns in hierarchy order (original names)
#   group   grouping column (original name), or NULL
#   carry   final positions of the carry columns
#   hidden  original names removed from the printed table
.plan_resolve_columns <- function(roles, d) {
  orig <- names(d)
  if (is.null(roles)) roles <- list()

  unknown <- setdiff(names(roles), orig)
  if (length(unknown)) {
    stop("No such column", if (length(unknown) > 1L) "s" else "", ": ",
         paste0("\"", unknown, "\"", collapse = ", "), ".", call. = FALSE)
  }

  has <- function(r) {
    nms <- names(roles)[vapply(roles, function(e) r %in% e$roles,
                               logical(1L))]
    if (length(nms) == 0L) character(0) else nms
  }
  opt <- function(col, key) roles[[col]]$opts[[key]]

  stub_cols <- has("stub")
  if (length(stub_cols) > 1L) {
    ord <- vapply(stub_cols, function(c) as.numeric(opt(c, "order") %||% NA),
                  numeric(1L))
    if (!anyNA(ord)) stub_cols <- stub_cols[order(ord)]
  }
  stub_layout <- if (length(stub_cols)) {
    as.character(opt(stub_cols[[1L]], "layout") %||% "merged")
  } else NULL

  group_col <- has("group")
  if (length(group_col) > 1L) {
    stop("The spike groups on a single column; ", length(group_col),
         " are declared: ", paste(group_col, collapse = ", "), ".",
         call. = FALSE)
  }
  hidden <- has("hide")

  # A hidden column cannot also be printed as part of the stub, and a hidden
  # column cannot be carried onto every column page.  Catch it here rather
  # than letting declaration order decide.
  # NB grouping is deliberately absent here: a hidden carrier that groups the
  # table without being printed is the classic clinical idiom.
  clash <- intersect(hidden, c(stub_cols, has("carry")))
  if (length(clash)) {
    stop("Column", if (length(clash) > 1L) "s" else "", " ",
         paste0("\"", clash, "\"", collapse = ", "),
         " cannot be hidden and printed at the same time.", call. = FALSE)
  }

  # -- project the final layout ------------------------------------------
  # merged stub: the hierarchy columns are consumed and one stub column takes
  # position 1.  columns layout: nothing moves.  Then hidden columns go.
  final <- orig
  stub_name <- NULL
  if (length(stub_cols) >= 2L && identical(stub_layout, "merged")) {
    stub_name <- paste(stub_cols, collapse = " / ")
    final <- c(stub_name, setdiff(orig, stub_cols))
  }
  final <- setdiff(final, hidden)

  # -- two views of ONE projection ---------------------------------------
  #
  # A hidden column has to survive resolution -- the classic clinical idiom is
  # a carrier column that GROUPS the table without being printed -- but must
  # not survive into the output.  So the projection yields two positions per
  # column, both computed here and nowhere else:
  #
  #   body_map  position while resolving (stub applied, hidden still present)
  #   map       position in the printed table (hidden removed; NA if hidden)
  #
  # This is one projection with two views, not two coordinate systems: no other
  # stage computes a position, and each stage is told which view it works in.
  body <- if (is.null(stub_name)) orig else c(stub_name, setdiff(orig, stub_cols))

  body_map <- match(orig, body)
  names(body_map) <- orig
  map <- match(orig, final)
  names(map) <- orig
  if (!is.null(stub_name)) {
    # every merged hierarchy column now answers with the stub's position
    body_map[stub_cols] <- match(stub_name, body)
    map[stub_cols] <- match(stub_name, final)
  }

  list(names     = final,
       body_names = body,
       map       = map,
       body_map  = body_map,
       stub   = stub_cols,
       layout = stub_layout,
       group  = if (length(group_col)) group_col else NULL,
       mode   = if (length(group_col)) opt(group_col, "mode") else NULL,
       carry  = unname(map[has("carry")]),
       hidden = hidden)
}

# Where did this column end up?  The one question every consumer asks, with
# one implementation to answer it.
#' @keywords internal
plan_position <- function(res, cols) {
  m <- res$columns$map
  out <- m[as.character(cols)]
  names(out) <- as.character(cols)
  out
}

# ============================================================================
#  SPIKE (design/plan-resolver) -- listings, declared like everything else
# ============================================================================
#
#  Step 8.  Nothing exported.
#
#  ---------------------------------------------------------------------------
#  The question this file exists to answer
#  ---------------------------------------------------------------------------
#
#  `as_rtftables()` grew a `listing =` argument, and with it a second
#  vocabulary.  A listing declares, inside `listing_spec()` / `listing_col()`,
#  several things a TABLE declares elsewhere:
#
#    listing_spec(blank_row_first = )   a blank row atop every page
#    listing_col(align = )              a column's alignment
#    listing_col(collapse_repeats = )   suppress a repeated value
#    listing_spec(record = )            ... which silently becomes
#                                       group_col + group_by="value" +
#                                       split="group_safe" + drop_cols
#
#  Nothing there is wrong; it is simply a THIRD place to write settings the
#  package already has two spellings for, and the last line is the shape this
#  whole review is about -- one argument quietly setting four others.
#
#  The claim under test: once a plan can RESHAPE, a listing needs no vocabulary
#  of its own beyond the reshape itself.  What is genuinely listing-specific --
#  which source variables join into one printed column, how wide a cell may get
#  before it wraps, the gutters, how tall a record's block is -- stays here.
#  Everything else is declared with the SAME layers a table uses, addressed by
#  the printed column's name:
#
#    plan_blanks(first = TRUE)                    not blank_row_first =
#    plan_roles(AGE = role("display",             not listing_col(align = )
#                          align = "right"))
#    plan_roles(USUBJID = role("collapse"))       not collapse_repeats =
#    plan_hide() / plan_group() / plan_pages()    on the record column, which
#                                                 is an ordinary hidden
#                                                 grouping carrier here
#
#  So `plan_listing()` has 9 settings where `listing_spec()` has 11, and the
#  two that leave are the two a table already had a spelling for.  The record
#  column stops being magic: it is a hidden column that groups the body, which
#  is the clinical idiom the plan already supports for tables.
#
#  ---------------------------------------------------------------------------
#  Where the reshape happens
#  ---------------------------------------------------------------------------
#
#  Stage 0 of the resolver, before columns.  It has to precede them: the
#  printed columns do not exist in the source data, so there is nothing to
#  name until the reshape has run.  This is the same rule the stub follows one
#  stage later -- the USER's pipeline never rewrites the data, and the resolver
#  is the one place allowed to know every declaration at once.
#
#  `build_listing()` is CALLED, never copied.  The spike owns the plumbing, not
#  the layout algorithm.

# ── the layer ──────────────────────────────────────────────────────────────

# `...` takes listing_col() objects (or bare column names), in printed order.
#
# Deliberately ABSENT, compared with listing_spec():
#
#   blank_row_first   -> plan_blanks(first = TRUE).  It is page furniture, the
#                        same fact a table declares there; keeping it here as
#                        well would be a second declaration site for one thing.
#   align             -> plan_roles(<col> = role("display", align = )).  A
#                        listing column's alignment is a column style, and the
#                        plan already addresses column styles by name.
#
# Deliberately PRESENT:
#
#   blank_row         -> how tall a record's block is.  It is part of the
#                        reshape, not a separator between groups: build_listing()
#                        adds it to the record's line count, which is what
#                        `max_rows` pages on.  plan_blanks() means something
#                        else -- blanks between groups of the PRINTED body --
#                        so the two are different facts, not two spellings.
#' @keywords internal
plan_listing <- function(plan, ..., type = NULL, sep = NULL, spacer = NULL,
                         spacer_rel_width = NULL, blank_row = NULL,
                         layout = NULL, wrap = NULL, record = NULL) {
  if (!inherits(plan, "rtf_plan")) {
    stop("Expected an rtf_plan; pipe from rtf_plan(data).", call. = FALSE)
  }
  if (!is.null(attr(plan$data, "rtf_listing", exact = TRUE))) {
    stop("This plan started from a body that build_listing() had already ",
         "produced, and it carries its own spec.  Drop plan_listing(), or ",
         "start the plan from the unbuilt source data.", call. = FALSE)
  }
  cols <- list(...)
  # A single list of columns is accepted as well as columns spread over `...`,
  # because listing_spec() takes a list and copying an existing one across
  # should not require re-typing it.
  if (length(cols) == 1L && is.list(cols[[1L]]) &&
      !inherits(cols[[1L]], "rtf_listing_col")) {
    cols <- cols[[1L]]
  }
  .plan_set(plan, "listing",
            list(cols = if (length(cols)) cols else NULL,
                 type = type, sep = sep, spacer = spacer,
                 spacer_rel_width = spacer_rel_width, blank_row = blank_row,
                 layout = layout, wrap = wrap, record = record))
}

# The layer -> a real listing_spec().  Built through the public constructor so
# every validation, default and template lookup is the shipped one.
#
# `blank_row_first` and `align` are pinned to the values that mean "the plan
# decides": the spec must not also declare them.
.plan_listing_spec <- function(f) {
  if (is.null(f$cols)) {
    stop("`plan_listing()` needs at least one column: pass listing_col() ",
         "objects, or column names, in printed order.", call. = FALSE)
  }
  args <- list(cols = f$cols, blank_row_first = FALSE)
  for (nm in c("type", "sep", "spacer", "spacer_rel_width", "blank_row",
               "layout", "wrap")) {
    if (!is.null(f[[nm]])) args[[nm]] <- f[[nm]]
  }
  args$record <- if (is.null(f$record)) TRUE else f$record
  do.call(listing_spec, args)
}

# ── stage 0 of the resolver ────────────────────────────────────────────────

# Reshape, then hand back everything the ordinary table stages need:
#
#   data     the listing body -- an ordinary data.frame from here on
#   src      per printed row: the SOURCE record it belongs to.  The hidden
#            record column already holds exactly this, which is why the plan
#            needs no map of its own.
#   kw       header / relative widths / alignment, in the body's own column
#            names -- written where an adapter writes its metadata, so
#            .plan_resolve_header() and .plan_resolve_style() read it with no
#            listing-specific code at all.
#   roles    the roles the reshape IMPLIES, as defaults only
#
# Returning `roles` separately rather than writing them into the plan is what
# keeps "the user declared it" distinguishable from "the reshape implied it".
.plan_resolve_listing <- function(plan) {
  layer   <- .plan_get(plan, "listing")
  carried <- attr(plan$data, "rtf_listing", exact = TRUE)
  if (is.null(layer) && is.null(carried)) return(NULL)

  consumed <- character(0)
  if (!is.null(carried)) {
    # rtf_plan(build_listing(data, spec)) -- already reshaped.  Both spellings
    # exist for the same reason they do on main: one to look at (or patch) the
    # reshaped data, one to declare it inline.
    spec <- carried
    body <- plan$data
    attr(body, "rtf_listing") <- NULL
  } else {
    spec <- .plan_listing_spec(layer)
    # SORT runs on the SOURCE, before the reshape.  It has to: a record is
    # several physical rows once the cells are wrapped, and sorting the body
    # would interleave them.  The stub has the same property one stage later,
    # which is why the row stage sorts before it reshapes -- this is that rule
    # applied to the stage that comes first.
    src_data <- .plan_listing_sort(plan, plan$data)
    consumed <- attr(src_data, "rtf_plan_sorted", exact = TRUE) %||% character(0)
    attr(src_data, "rtf_plan_sorted") <- NULL
    body <- build_listing(src_data, spec)
    # build_listing() resolves each header against the data's own `label`
    # attributes and hands the resolved spec back on the body, so the labels
    # are derived once rather than here and again downstream.
    spec <- attr(body, "rtf_listing", exact = TRUE)
    attr(body, "rtf_listing") <- NULL
  }

  meta <- .listing_metadata(spec, body)
  kw   <- list(col_header      = meta$col_header,
               col_spec        = meta$col_spec,
               col_rel_width   = meta$col_rel_width)

  rec <- spec$record_col
  src <- if (!is.null(rec) && rec %in% names(body)) {
    as.integer(body[[rec]])
  } else {
    # No record column: the rows cannot be traced back, so per-source-row data
    # (cell_styles) has nothing to ride on.  Say so rather than mis-aligning it.
    rep(NA_integer_, nrow(body))
  }

  # The roles the reshape implies.  A record column is a hidden column that
  # groups the body -- the same idiom a table uses for a sort carrier -- so it
  # is expressed as roles rather than as four coupled arguments.
  roles <- list()
  if (!is.null(rec) && rec %in% names(body)) {
    roles[[rec]] <- role("group", "hide", mode = "value")
  }
  for (cl in spec$cols) {
    if (isTRUE(cl$collapse_repeats) && cl$name %in% names(body)) {
      roles[[cl$name]] <- role("collapse")
    }
  }

  list(data = body, src = src, kw = kw, roles = roles, spec = spec,
       record_col = rec, consumed = consumed)
}

# Apply the sort roles to the SOURCE data and say which columns did the work.
.plan_listing_sort <- function(plan, d) {
  roles <- .plan_get(plan, "roles") %||% list()
  if (!length(roles)) return(d)
  cols <- names(roles)[vapply(roles, function(e) "sort" %in% e$roles,
                              logical(1L))]
  cols <- intersect(cols, names(d))
  if (!length(cols)) return(d)
  ord <- vapply(cols, function(c)
    as.numeric(roles[[c]]$opts$order %||% NA), numeric(1L))
  if (!anyNA(ord)) cols <- cols[order(ord)]
  desc <- vapply(cols, function(c) isTRUE(roles[[c]]$opts$desc), logical(1L))
  o <- .resolve_sort_order(cols, unname(desc), d)
  if (!is.null(o)) {
    d <- d[o, , drop = FALSE]
    rownames(d) <- NULL
  }
  attr(d, "rtf_plan_sorted") <- cols
  d
}

# The reshape CONSUMES source columns.  A role still naming one of them is
# either finished with (a sort carrier, which has just done its work, or a
# hide, which the reshape performed by not printing it) or a mistake worth
# saying out loud -- an alignment declared on `HIST` when the printed column
# it ended up in is called something else.
.plan_drop_consumed_roles <- function(plan, consumed, body_names) {
  r <- .plan_get(plan, "roles")
  if (is.null(r) || !length(r)) return(plan)
  for (nm in names(r)) {
    e <- r[[nm]]
    if (nm %in% consumed) e$roles <- setdiff(e$roles, "sort")
    if (nm %in% body_names) {
      r[[nm]] <- if (length(e$roles)) e else NULL
      next
    }
    left <- setdiff(e$roles, c("hide", "sort"))
    if (length(left)) {
      stop("The listing consumed the source column \"", nm, "\", so ",
           if (length(left) > 1L) "the roles " else "the role ",
           paste0("\"", left, "\"", collapse = ", "),
           " cannot apply to it.  Name the printed column instead; this ",
           "listing prints: ",
           paste0("\"", setdiff(body_names, grep("^\\.", body_names,
                                                value = TRUE)), "\"",
                  collapse = ", "), ".", call. = FALSE)
    }
    r[[nm]] <- NULL
  }
  .plan_set_raw(plan, "roles", r)
}

# Merge the implied roles UNDER whatever the caller declared.
#
# Two rules, and they differ on purpose:
#
#   * `hide` and `collapse` are additive -- they say what the reshape did, and
#     a caller who also names the column is agreeing, not disagreeing.
#   * `group` is a DEFAULT.  A caller who declares a grouping of their own
#     (one page per treatment arm, say) means it, and two grouping columns is
#     an error the column resolver already raises.  So the record's grouping
#     is dropped the moment another column claims that role -- the record then
#     keeps its `hide`, and pagination behaves as it does for any table whose
#     grouping is not the record.
.plan_merge_listing_roles <- function(plan, implied) {
  if (!length(implied)) return(plan)
  declared <- .plan_get(plan, "roles") %||% list()
  user_groups <- names(declared)[vapply(declared, function(e)
    "group" %in% e$roles, logical(1L))]

  args <- list(plan)
  for (nm in names(implied)) {
    r <- implied[[nm]]
    if (length(user_groups) && "group" %in% r$roles &&
        !(nm %in% user_groups)) {
      keep <- setdiff(r$roles, "group")
      if (!length(keep)) next
      r <- do.call(role, c(as.list(keep), r$opts[setdiff(names(r$opts),
                                                          "mode")]))
    }
    args[[nm]] <- r
  }
  if (length(args) == 1L) return(plan)
  do.call(plan_roles, args)
}

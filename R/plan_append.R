# ---------------------------------------------------------------------------
#  plan_append(): another plan's rows under a table's
# ---------------------------------------------------------------------------
#  A table whose rows come from two sources -- a listing of the records
#  above, their summary below -- is two ordinary plans: the first carries
#  the column axis and the column header, and plan_append() lays the rows of
#  the second (the child) under the rows of the first (the parent).  Each
#  part is run to its table on its own (plan_apply(, "table")), its row
#  keys folded into one label column, and the tables are stacked, in the
#  order declared, BEFORE the pages are cut: so a page cut by a group value
#  (plan_paginate_rows(page_by = )) holds every part's rows of that
#  value, matched by value and never by order, and paginate_cols() then
#  cuts every page the same way.
#
#  What is said where (the design, _archive/plan-append-design-2026-10-28.md):
#    * a part's plan says how ITS rows are made: the roles, the cells,
#      digits, na, labels, the levels of its row keys, its stub fold, its
#      cell styles, its own first / last blank row;
#    * the whole table's settings -- the column order, the header, the
#      widths and decimal split, the pages, the blank rows between parts,
#      the style, titles, footnotes, the steps after -- are the parent's,
#      wherever in the parent they are written (before or after
#      plan_append()); a child carrying one stops, so that the child run on
#      its own and the child in the table never look different for a
#      reason nobody can see;
#    * what a child does not say it inherits from the parent, key by key
#      (cells, digits, na, labels, levels of row keys, the stub fold); its
#      own wins.

# The declarations only the whole table has: a child carrying one stops.
.plan_whole_kinds <- c("header", "columns", "pages", "colpages", "titles",
                       "footnotes", "after", "restyle", "listing", "part")
# ... and the declarations a child inherits from the parent (its own win)
.plan_inherit_kinds <- c("cells", "cell_options", "digits", "round", "labels",
                         "levels", "stub", "styles")

#' Lay another plan's rows under a table's
#'
#' `plan_append()` builds one table from two sources: the rows of
#' `.plan` (the **parent**: say, a listing of each subject's
#' concentrations) and under them the rows of `part` (the **child**: their
#' summary statistics), on the same columns.  Each is an ordinary plan,
#' run to its table on its own; the tables are stacked before the pages
#' are cut, so a page holds every part's rows of its group.  The whole
#' table's settings --- header, widths, pages, style, titles --- are the
#' parent's, written before or after `plan_append()`.
#'
#' @param .plan A [table_plan()]: the parent, whose column axis and header
#'   the table has.
#' @param part Another [table_plan()]: its rows go **under** the rows of
#'   `.plan`.  Several `plan_append()` calls stack their parts in the order
#'   written.  The part's plan says how its own rows are made and nothing
#'   about the whole table (see *Parts* below).
#' @param name The part's name, for its messages and for [plan_layers()]:
#'   `"part2"`, `"part3"`, ... by default (the parent is `"part1"`).  One
#'   non-empty string, not already taken.
#' @param absent What a page group (a value of `plan_paginate_rows(page_by
#'   = )`) the part has no rows for does: `"error"` (default: a report is
#'   not defined by a group that is missing), or `"skip"` (that page has no
#'   rows of this part).
#' @section Parts:
#' `plan_append(part)` lays another plan's rows under this plan's.  The
#' first plan (the **parent**) is an ordinary table with its column axis
#' and header; a **part** (the child) is an ordinary plan too, written as
#' any table is, and may carry only the verbs that make its rows:
#' [plan_cells()], [plan_digits()], [plan_labels()], [plan_levels()] of its
#' row keys, [plan_stub()], [plan_row_group()], [plan_filter()],
#' [plan_sort()], [plan_nest()], [plan_total()], [plan_compare()],
#' [plan_stat_groups()], [plan_hide()], [plan_empty()], [plan_cell_style()]
#' with a condition, and `plan_blanks(first =, last =)` (a blank row above
#' or below its own rows).  Its `cols` key is the parent's (or left out).
#'
#' The whole table's settings are the parent's, written anywhere in the
#' parent, before or after `plan_append()`, the later of two winning as
#' always: the column order ([plan_levels()] of the column key), the header
#' ([plan_col_header()]), the widths and decimal split ([plan_columns()]),
#' the pages ([plan_paginate_rows()], [plan_paginate_cols()]), the blank
#' rows between parts (`plan_blanks(where = "between_parts")`, and
#' `first` / `last` at the table's top and bottom), the style
#' ([plan_style()]), titles, footnotes and the steps after.  A part that
#' carries one of them stops.
#'
#' What a part does not say, it takes from the parent, key by key: the
#' cell templates, the digits, the empty-cell text, the labels, the levels
#' of its row keys, the stub fold; what it says itself wins.  Its row keys
#' and label are folded into one label column (the parent's
#' `plan_stub(name =, indent =)`), so every part ends as `[page keys]
#' [label] [value columns]`: a constant row key (`rows = c(block = ~
#' "Summary Statistics")`) is the heading row above its rows, as in any
#' table.  Columns a part does not have print its empty-cell text.
#'
#' The parts are stacked before the pages are cut: a page of
#' `page_by` value X holds the parent's rows of X, then each part's rows of
#' X, matched by **value**.  Every part's data must have the `page_by`
#' columns (it is not written to every page by default); a value the parent
#' has and a part has not stops, unless that part says `absent =
#' "skip"`; a value only a part has makes no page.
#'
#' A part run on its own (`plan_apply(part, "table")`) is the same rows
#' for every group at once, with only what it says itself: that is the
#' whole difference from the part in the table.
#'
#' @return The plan, with one more layer.  [plan_apply()] of it gives the
#'   stacked table, with a `.part` column naming each row's part.
#' @family plan verbs
#' @seealso [plan_verbs] for every verb.
#' @examples
#' # Invented concentrations: three subjects, three time points; S-02's
#' # 4 h sample is below the limit of quantitation (NA)
#' conc <- data.frame(
#'   USUBJID = rep(c("S-01", "S-02", "S-03"), each = 3),
#'   ATPT    = rep(c("1 h", "2 h", "4 h"), 3),
#'   AVAL    = c(12.1, 8.4, 3.2, 10.5, 7.9, NA, 14.0, 9.6, 4.1))
#' listing <- table_plan(conc, cols = "ATPT", rows = c(subject = ~ "{USUBJID}"),
#'                       value = "AVAL") |>
#'   plan_cells("{value:.1f}", na = "BLQ")
#'
#' # Their summary, one row a statistic, under one heading row
#' smry <- data.frame(
#'   ATPT      = rep(c("1 h", "2 h", "4 h"), each = 2),
#'   variable  = "AVAL", .label = NA, .kind = "continuous",
#'   stat_name = c("N", "mean"),
#'   stat      = c(3, 12.20, 3, 8.633, 2, 3.65))
#' summary <- table_plan(smry, cols = "ATPT", rows = c(block = ~ "Summary")) |>
#'   plan_cells(c(N = "{N:.0f}", Mean = "{mean:.2f}"))
#'
#' # Minimal: the summary's rows under the listing's
#' listing |> plan_append(summary) |> plan_apply("table")
#'
#' # The whole table's settings are the parent's: a blank row between the
#' # parts, and one header for both
#' pages <- listing |>
#'   plan_append(summary, name = "summary") |>
#'   plan_blanks(where = "between_parts") |>
#'   plan_col_header(rtf_col_header(c("Subject", "{col}"))) |>
#'   plan_apply()
#' pages[[1]]
#' @export
plan_append <- function(.plan, part, name = NULL, absent = c("error", "skip")) {
  if (!inherits(.plan, "table_plan")) {
    .ard_stop("Expected a table_plan; pipe from table_plan(x).")
  }
  if (!inherits(part, "table_plan")) {
    .ard_stop(paste0(
      "plan_append(part = ) is a table_plan: an ordinary plan whose rows go ",
      "under this\n  table's, e.g.\n",
      "    summary <- normalize_ard(ard) |> table_plan(cols = \"ATPT\", ",
      "rows = c(block = ~ \"Summary Statistics\")) |>\n",
      "      plan_cells(continuous = c(N = \"{N:.0f}\", Mean = \"{mean:.4s}\"))\n",
      "    listing |> plan_append(summary)"))
  }
  absent <- match.arg(absent)
  n <- length(.plan_of(.plan, "part")) + 2L
  if (is.null(name)) name <- paste0("part", n)
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
    .ard_stop("plan_append(name = ) is one non-empty string.")
  }
  taken <- c("part1", vapply(.plan_of(.plan, "part"), `[[`, "", "name"))
  if (name %in% taken) {
    .ard_stop(sprintf("plan_append(name = \"%s\"): a part of that name is already there.", name))
  }
  .plan_append_check_child(part, name, .plan)
  .plan_layer(.plan, "part", list(plan = part, name = name, absent = absent))
}

# A child may say how its own rows are made, and nothing about the whole
# table.  Said here, when the part is declared: the error names the verb
# and where it goes.
.plan_append_check_child <- function(child, name, parent) {
  kinds <- vapply(child$layers, `[[`, "", "kind")
  verb_of <- c(header = "plan_col_header()", columns = "plan_columns()",
               pages = "plan_paginate_rows() / plan_paginate_group()",
               colpages = "plan_paginate_cols()", titles = "plan_titles()",
               footnotes = "plan_footnotes()", after = "plan_after()",
               restyle = "plan_cell_style(header = TRUE) / plan_cell_style(cols = ) without a condition",
               listing = "plan_listing()", part = "plan_append()")
  bad <- unname(verb_of[intersect(kinds, .plan_whole_kinds)])
  if (length(.plan_of(child, "style"))) bad <- c(bad, "plan_style()")
  for (f in .plan_of(child, "blanks")) {
    if (!is.null(f$blank_rows) || !is.null(f$count_blank_rows)) {
      bad <- c(bad, "plan_blanks(where = ) / plan_blanks(counted = )")
    }
  }
  # the pages' group axis is a page setting too
  for (f in .plan_of(child, "group")) {
    if (isTRUE(f$.page)) bad <- c(bad, "plan_paginate_group()")
  }
  pcols <- .plan_cols_key(parent)
  for (f in .plan_of(child, "levels")) {
    lv <- f$levels %||% list()
    if (any(names(lv) %in% pcols)) {
      bad <- c(bad, sprintf("plan_levels(%s = ) (the column key's order)", pcols[1L]))
    }
  }
  if (length(bad)) {
    .ard_stop(paste0(
      "plan_append(\"", name, "\"): a part says how its own rows are made, and ",
      "the whole table's\n  settings are the first plan's, written before ",
      "or after plan_append().  Move to the first plan:\n    ",
      paste(unique(bad), collapse = "\n    "),
      "\n  (A part may carry plan_cells, plan_digits, plan_labels, ",
      "plan_levels of its row keys,\n  plan_stub, plan_row_group, ",
      "plan_filter, plan_sort, plan_nest, plan_total, plan_compare,\n  ",
      "plan_stat_groups, plan_hide, plan_empty, plan_cell_style(where = ), ",
      "plan_blanks(first = , last = ).)"))
  }
  ccols <- .plan_cols_key(child)
  if (length(ccols) && !identical(ccols, pcols)) {
    .ard_stop(sprintf(paste0(
      "plan_append(\"%s\"): the part's column key is %s and the table's is %s.\n",
      "  One column axis for the table: give the part table_plan(cols = %s), ",
      "or leave `cols` out."),
      name, paste(sQuote(ccols), collapse = ", "),
      paste(sQuote(pcols), collapse = ", "), deparse(pcols)))
  }
  invisible(TRUE)
}

# The column key's name(s), as the spread table calls them
.plan_cols_key <- function(plan) {
  cc <- plan$roles[["cols"]]
  if (is.null(cc)) return(character())
  nm <- names(cc)
  v <- as.character(unlist(cc, use.names = FALSE))
  if (is.null(nm)) v else ifelse(nzchar(nm), nm, v)
}

# ---- the run ----------------------------------------------------------------

# plan_apply() on a plan with parts: each part to its table, folded;
# stacked; then the parent's whole-table settings on the stack.
.plan_apply_parts <- function(plan, stage) {
  decls <- .plan_of(plan, "part")
  parent <- plan
  parent$layers <- Filter(function(l) !identical(l$kind, "part"), plan$layers)
  parent$cache <- new.env(parent = emptyenv())
  pg <- .plan_merge(.plan_of(parent, "pages"))$page_by
  pg <- if (is.null(pg)) character() else as.character(pg)
  stub <- .plan_merge(.plan_of(parent, "stub"))
  lbl <- stub$name %||% "label"
  indent <- stub$indent %||% 4L
  parts <- c(list(list(plan = parent, name = "part1", absent = "error", first = TRUE)),
              lapply(decls, function(d) c(d, list(first = FALSE))))
  parts <- lapply(parts, function(l) .plan_append_part(l, parent, pg, lbl, indent))
  if (identical(stage, "input")) return(parts[[1L]]$input)

  # the value columns: the column key's order when the parent says it,
  # else as first seen, part by part
  vcols <- unique(unlist(lapply(parts, `[[`, "vcols"), use.names = FALSE))
  key <- .plan_cols_key(parent)
  lv <- .plan_merge(.plan_of(parent, "levels"), deep = "levels")$levels
  if (length(key) == 1L && !is.null(lv[[key]])) {
    want <- as.character(unlist(lv[[key]], use.names = FALSE))
    if (!is.null(names(lv[[key]]))) want <- names(lv[[key]])
    vcols <- c(intersect(want, vcols), setdiff(vcols, want))
  }
  # every part as [page keys][label][value columns]: a column a part has
  # not prints its empty-cell text
  for (i in seq_along(parts)) {
    t <- parts[[i]]$tbl
    for (v in setdiff(vcols, names(t))) t[[v]] <- rep(parts[[i]]$na, nrow(t))
    t <- t[c(pg, lbl, vcols)]
    t$.part <- rep(parts[[i]]$name, nrow(t))
    parts[[i]]$tbl <- t
  }
  stacked <- .plan_append_stack(parts, pg, parent)
  stacked <- .plan_append_blanks(stacked, parts, pg, lbl, vcols, parent)
  shown <- stacked
  shown$.src <- NULL
  .plan_remember(plan, "table", shown)
  if (identical(stage, "table")) return(shown)

  whole <- .plan_append_whole(parent, stacked, pg, lbl, vcols, parts)
  body <- stacked
  body$.part <- NULL
  body$.src <- NULL
  if (identical(stage, "args")) {
    return(list(widen = list(), rtf = .plan_rtf_args(whole, body)))
  }
  out <- .plan_to_pages(whole, body)
  for (nm in c("printed", "pre_cols", "header_raw", "empty")) {
    if (!is.null(whole$cache[[nm]])) .plan_remember(plan, nm, whole$cache[[nm]])
  }
  out
}

# One part to its folded table.  The parent's rows are made with the
# parent's own declarations; a child's with the parent's inheritable
# declarations under its own.  The page keys are made row keys of every
# part, so the stack can be cut by their value.
.plan_append_part <- function(l, parent, pg, lbl, indent) {
  p <- l$plan
  name <- l$name
  if (!isTRUE(l$first)) {
    inh <- Filter(function(d) d$kind %in% .plan_inherit_kinds, parent$layers)
    # the empty-cell text is each part's own: the listing's "BLQ" is no
    # statistic's; so are its notes
    inh <- lapply(inh, function(d) {
      if (identical(d$kind, "cell_options")) {
        d$fields <- d$fields[setdiff(names(d$fields), c("na", "notes"))]
        if (!length(d$fields)) return(NULL)
      }
      d
    })
    inh <- Filter(Negate(is.null), inh)
    # the column key's order is the whole table's, said once on the stack
    key <- .plan_cols_key(parent)
    inh <- lapply(inh, function(d) {
      if (identical(d$kind, "levels") && !is.null(d$fields$levels)) {
        d$fields$levels <- d$fields$levels[setdiff(names(d$fields$levels), key)]
        if (!length(d$fields$levels)) return(NULL)
      }
      d
    })
    inh <- Filter(Negate(is.null), inh)
    p$layers <- c(inh, p$layers)
    p$cache <- new.env(parent = emptyenv())
    if (is.null(p$roles[["cols"]]) && !is.null(parent$roles[["cols"]])) {
      p$roles[["cols"]] <- parent$roles[["cols"]]
    }
  }
  if (length(pg)) {
    miss <- setdiff(pg, names(p$data))
    if (length(miss)) {
      .ard_stop(sprintf(paste0(
        "plan_append(\"%s\"): its data has no %s, the page_by column%s of ",
        "plan_paginate_rows().\n  Every part's data carries the page ",
        "keys (a part is not written to every page); rename or add the ",
        "column.\n  Columns: %s"),
        name, paste(sQuote(miss), collapse = ", "), if (length(miss) > 1L) "s" else "",
        paste(utils::head(names(p$data), 10L), collapse = ", ")))
    }
    rows <- p$roles[["rows"]]
    rows <- if (is.null(rows)) {
      # widen_ard()'s own default when several variables are in the data
      nv <- if ("variable" %in% names(p$data)) length(.ard_first_seen(p$data$variable)) else 0L
      if (nv > 1L || any(!is.na(p$data$.depth %||% NA))) list(group = "variable") else list()
    } else as.list(rows)
    nm <- names(rows) %||% rep("", length(rows))
    nm <- ifelse(nzchar(nm), nm, vapply(rows, function(r)
      if (inherits(r, "formula")) "" else as.character(r)[1L], ""))
    names(rows) <- nm
    add <- setdiff(pg, nm)
    p$roles[["rows"]] <- c(stats::setNames(as.list(add), add), rows)
  }
  input <- NULL
  tbl <- withCallingHandlers(
    {
      if (identical(name, "part1")) input <- plan_apply(p, "input")
      plan_apply(p, "table")
    },
    error = function(e) {
      .ard_stop(sprintf("plan_append(\"%s\"): %s", name, conditionMessage(e)))
    })
  tbl <- as.data.frame(tbl, stringsAsFactors = FALSE)
  # fold the row keys and the label into one label column
  own <- .plan_merge(.plan_of(l$plan, "stub"))
  keys <- own$vars %||% c(.plan_row_keys(p), .plan_label_name(p))
  keys <- setdiff(intersect(keys, names(tbl)), c(pg, .plan_hidden_by_verb(p), .plan_hidden(p, tbl)))
  gone <- setdiff(c(.plan_hidden_by_verb(p), .plan_hidden(p, tbl)), c(pg, keys))
  if (length(gone)) tbl <- tbl[setdiff(names(tbl), gone)]
  na <- .plan_merge(.plan_of(p, "cell_options"))$na %||% ""
  pre <- tbl
  # folded one page group at a time, so each group has its heading rows
  # (a constant row key folds into one heading above each group's rows)
  fold <- function(t, rows) {
    if (length(keys)) {
      a <- list(data = t, vars = keys, label = lbl, indent = indent)
      if (!is.null(own$group_summary)) a$group_summary <- own$group_summary
      f <- do.call(stub_cols, a)
    } else {
      f <- cbind(data.frame(.lbl. = rep("", nrow(t)), stringsAsFactors = FALSE), t)
      names(f)[1L] <- lbl
      attr(f, "rtf_stub_src") <- seq_len(nrow(t))
    }
    src <- rows[attr(f, "rtf_stub_src", exact = TRUE)]
    # the page keys, carried on every row (a heading row the fold added has none)
    for (k in pg) f[[k]] <- .carry_back(as.character(pre[[k]])[src])
    f$.src <- src
    f
  }
  gkey <- if (length(pg)) do.call(paste, c(lapply(pg, function(k) as.character(tbl[[k]])), sep = "")) else rep("", nrow(tbl))
  pieces <- lapply(.ard_first_seen(gkey), function(g) {
    rows <- which(gkey == g)
    fold(tbl[rows, , drop = FALSE], rows)
  })
  tbl <- do.call(rbind, pieces)
  rownames(tbl) <- NULL
  src <- tbl$.src
  tbl$.src <- NULL
  if (!lbl %in% names(tbl)) {
    .ard_stop(sprintf("plan_append(\"%s\"): no label column after the fold (%s).", name, lbl))
  }
  tbl[[lbl]] <- as.character(tbl[[lbl]])
  vcols <- setdiff(names(tbl), c(pg, lbl))
  # a cell with no value -- a heading row the fold added, a statistic the
  # ARD has not -- prints the part's empty-cell text
  for (v in vcols) {
    x <- as.character(tbl[[v]])
    x[is.na(x)] <- na
    tbl[[v]] <- x
  }
  edge <- .plan_merge(.plan_of(l$plan, "blanks"))
  list(name = name, absent = l$absent, tbl = tbl, pre = pre, src = src,
       vcols = vcols, na = na, input = input, plan = p,
       first = if (isTRUE(l$first)) NULL else edge$blank_row_first,
       last = if (isTRUE(l$first)) NULL else edge$blank_row_end)
}

# The stack: page group by page group, in the parent's order of the groups,
# each group's parts in the order declared.  A group a part lacks stops,
# unless the part says absent = "skip"; a group only a part has makes no
# rows.
.plan_append_stack <- function(parts, pg, parent) {
  key_of <- function(t) if (length(pg)) do.call(paste, c(lapply(pg, function(k) t[[k]]), sep = "\r")) else rep("", nrow(t))
  pk <- key_of(parts[[1L]]$tbl)
  groups <- .ard_first_seen(pk)
  # the parent's levels of the page keys order the pages
  lv <- .plan_merge(.plan_of(parent, "levels"), deep = "levels")$levels
  if (length(pg) == 1L && !is.null(lv[[pg]])) {
    want <- as.character(unlist(lv[[pg]], use.names = FALSE))
    if (!is.null(names(lv[[pg]]))) want <- names(lv[[pg]])
    groups <- c(intersect(want, groups), setdiff(groups, want))
  }
  for (i in seq_along(parts)[-1L]) {
    miss <- setdiff(groups, unique(key_of(parts[[i]]$tbl)))
    if (length(miss) && !identical(parts[[i]]$absent, "skip")) {
      .ard_stop(sprintf(paste0(
        "plan_append(\"%s\"): no rows for %s %s, which the first plan has.\n",
        "  A page holds every part's rows of one value of %s.  Give the part ",
        "the rows, or\n  plan_append(absent = \"skip\") to leave it off that page."),
        parts[[i]]$name, if (length(pg) > 1L) "the page groups" else sQuote(pg),
        paste(sQuote(gsub("\r", ", ", miss, fixed = TRUE)), collapse = ", "),
        paste(sQuote(pg), collapse = " / ")))
    }
  }
  out <- list()
  for (g in groups) {
    for (i in seq_along(parts)) {
      t <- parts[[i]]$tbl
      k <- key_of(t)
      sel <- t[k == g, , drop = FALSE]
      if (nrow(sel)) {
        sel$.src <- which(k == g)
        out[[length(out) + 1L]] <- sel
      }
    }
  }
  st <- do.call(rbind, out)
  rownames(st) <- NULL
  st
}

# The blank rows a part asks for -- plan_blanks(where = "between_parts")
# on the whole, first / last on a part -- as empty rows of the stack
# inside each page group, so the pages keep them.  (An empty row is what a
# blank row is: as_rtftables() prints it as a separator.)
.plan_append_blanks <- function(stacked, parts, pg, lbl, vcols, parent) {
  between <- identical(.plan_merge(.plan_of(parent, "blanks"))$blank_rows, "between_parts")
  first_of <- stats::setNames(lapply(parts, `[[`, "first"), vapply(parts, `[[`, "", "name"))
  last_of <- stats::setNames(lapply(parts, `[[`, "last"), vapply(parts, `[[`, "", "name"))
  if (!between && !any(vapply(c(first_of, last_of), isTRUE, NA))) return(stacked)
  key <- if (length(pg)) do.call(paste, c(lapply(pg, function(k) stacked[[k]]), sep = "\r")) else rep("", nrow(stacked))
  n <- nrow(stacked)
  blank_before <- logical(n)
  blank_after <- logical(n)
  for (i in seq_len(n)) {
    lay <- stacked$.part[i]
    starts <- i == 1L || key[i - 1L] != key[i] || stacked$.part[i - 1L] != lay
    ends <- i == n || key[i + 1L] != key[i] || stacked$.part[i + 1L] != lay
    first_in_group <- i == 1L || key[i - 1L] != key[i]
    last_in_group <- i == n || key[i + 1L] != key[i]
    if (starts && (isTRUE(first_of[[lay]]) || (between && !first_in_group))) blank_before[i] <- TRUE
    if (ends && isTRUE(last_of[[lay]]) && !(between && !last_in_group)) blank_after[i] <- TRUE
  }
  if (!any(blank_before) && !any(blank_after)) return(stacked)
  blank <- function(i) {
    b <- stacked[i, , drop = FALSE]
    b[[lbl]] <- ""
    for (v in vcols) b[[v]] <- ""
    b$.src <- NA_integer_
    b
  }
  out <- vector("list", n)
  for (i in seq_len(n)) {
    out[[i]] <- rbind(if (blank_before[i]) blank(i), stacked[i, , drop = FALSE],
                      if (blank_after[i]) blank(i))
  }
  st <- do.call(rbind, out)
  rownames(st) <- NULL
  st
}

# The whole: a plan of the stack (a finished table), carrying the parent's
# whole-table declarations, the page keys hidden, the parts' cell styles
# and edge borders read off the stack.
.plan_append_whole <- function(parent, stacked, pg, lbl, vcols, parts) {
  body <- stacked
  body$.part <- NULL
  body$.src <- NULL
  whole <- table_plan(body)
  whole$roles[["label"]] <- stats::setNames(lbl, lbl)
  keep <- Filter(function(d) d$kind %in% c("header", "columns", "pages", "colpages",
                                             "titles", "footnotes", "after", "restyle",
                                             "style", "blanks", "group", "hide",
                                             "cell_options"), parent$layers)
  keep <- lapply(keep, function(d) {
    if (identical(d$kind, "blanks") && identical(d$fields$blank_rows, "between_parts")) {
      d$fields$blank_rows <- NULL
    }
    if (identical(d$kind, "cell_options")) d$fields <- d$fields["na"]
    d
  })
  whole$layers <- keep
  if (length(pg)) whole <- do.call(plan_hide, c(list(whole), as.list(pg)))
  # the parts' cell styles (plan_cell_style(where = )), each on its own rows
  # of the stack: the condition sees the stack's columns and, through the
  # fold's source rows, the part's own
  styles <- NULL
  for (i in seq_along(parts)) {
    st <- .plan_merge(.plan_of(parts[[i]]$plan, "styles"))
    if (!length(st)) next
    rows <- which(stacked$.part == parts[[i]]$name)
    sub <- body[rows, , drop = FALSE]
    pre <- parts[[i]]$pre
    src <- parts[[i]]$src[stacked$.src[rows]]
    attr(sub, "rtf_stub_src") <- src
    cs <- .plan_cell_styles(st, sub, pre)
    if (is.null(styles)) styles <- vector("list", nrow(body))
    styles[rows] <- cs
  }
  if (!is.null(styles)) whole$cache$cell_styles <- styles
  whole$cache$n_plan <- parent
  whole$cache$parted <- TRUE
  whole
}

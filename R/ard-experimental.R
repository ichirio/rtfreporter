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
  "ard_keys", "ard_normalize", "ard_spread", "ard_table", "ard_template",
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
.ard_unlist_col <- function(col) {
  if (!is.list(col)) return(col)
  n <- length(col)
  simple <- vapply(col, function(e) {
    length(e) == 1L && is.atomic(e) && !is.list(e)
  }, logical(1))
  out <- vector("list", n)
  for (i in seq_len(n)) out[[i]] <- if (simple[i]) col[[i]] else NA
  vals <- unlist(out, use.names = FALSE)
  if (length(vals) != n) vals <- rep(NA, n)
  vals
}

# TRUE when every non-NA element of a flattened column is numeric-like.
.ard_is_numericish <- function(x) {
  if (is.numeric(x) || is.logical(x)) return(TRUE)
  y <- suppressWarnings(as.numeric(as.character(x)))
  all(is.na(x) | !is.na(y))
}

# stable order of first appearance
.ard_first_seen <- function(x) {
  u <- unique(as.character(x))
  u[!is.na(u)]
}

# Build a factor whose levels are `lv` (padding with anything unseen so no
# value is silently dropped).
.ard_as_factor <- function(x, lv, ordered = TRUE) {
  x <- as.character(x)
  extra <- setdiff(.ard_first_seen(x), lv)
  factor(x, levels = c(lv, extra), ordered = ordered)
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
  if (!nzchar(spec)) {
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

# A `cells` entry is one of
#   "n ({p})"                      one row, label taken from `label`
#   c("a", "b")                    one row, first template that resolves wins
#   c("Mean (SD)" = "...", ...)    one row per element, label = the name
#   list("Mean (SD)" = c(...))     same, each element a fallback chain
# Returns list(labels = <chr|NULL>, chains = <list of chr>).
.ard_cell_entry <- function(entry) {
  if (is.null(entry)) return(NULL)
  nms <- names(entry)
  if (is.null(nms) || !any(nzchar(nms))) {
    return(list(labels = NULL, chains = list(as.character(unlist(entry)))))
  }
  chains <- lapply(seq_along(entry), function(i) as.character(entry[[i]]))
  list(labels = nms, chains = chains)
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

.ard_lookup_cells <- function(cells, variable, context, kind = NA_character_) {
  if (is.character(cells) && is.null(names(cells))) {
    return(.ard_cell_entry(cells))
  }
  keys <- variable
  if (!is.na(context)) keys <- c(keys, .ard_context_aliases(context))
  if (!is.na(kind))    keys <- c(keys, .ard_context_aliases(kind))
  for (k in c(unique(keys), "default")) {
    if (!is.na(k) && !is.null(cells[[k]])) return(.ard_cell_entry(cells[[k]]))
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
#' @return A data frame with one row per ARD statistic: the key columns, the
#'   ARD's own `variable` / `variable_level` / `context` / `stat_name` /
#'   `stat_label` / `stat` / `stat_fmt`, the structural classification `.kind`
#'   (`"continuous"` / `"categorical"`, see [rtfreporter-ard]), and, when
#'   `hierarchy` is given, `.depth` (1 = outermost) and `.label` (the deepest
#'   non-missing hierarchy value).
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
  if (length(drop_contexts)) {
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

  # a hierarchy level's own summary rows carry it in variable/variable_level
  for (k in hierarchy) {
    hit <- !is.na(d$variable) & d$variable == k
    if (any(hit)) d[[k]][hit] <- d$variable_level[hit]
  }

  d$.overall <- FALSE
  ov <- !is.na(d$variable) & d$variable == "..ard_hierarchical_overall.."
  if (!is.null(overall) && any(ov) && length(hierarchy)) {
    d[[hierarchy[1L]]][ov] <- overall
    d$.overall[ov] <- TRUE
  } else if (any(ov) && is.null(overall)) {
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
  } else {
    d$.depth <- 1L
    d$.label <- d$variable_level
  }

  if (drop_key_variables) {
    kill <- setdiff(keys, hierarchy)
    d <- d[!(d$variable %in% kill), , drop = FALSE]
  }

  d$.kind <- .ard_kind(d)

  rownames(d) <- NULL
  front <- c(keys, "variable", "variable_level", "context", "stat_name",
             "stat_label", "stat", "stat_fmt", ".kind", ".depth", ".label",
             ".overall")
  front <- intersect(front, names(d))
  d[, c(front, setdiff(names(d), front)), drop = FALSE]
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
      .ard_stop(sprintf("`%s`: no column '%s' in the normalized ARD. Available: %s",
                        what, ref,
                        paste(setdiff(names(d), c(".overall")), collapse = ", ")))
    }
    out[[i]] <- list(out = nm, ref = ref)
  }
  out
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
#' @param rows Row keys, in output order.  A named vector renames them:
#'   `rows = c(group = "variable")` puts `variable` into a column called
#'   `group`.
#' @param label Source of the row label, as a single (optionally named)
#'   reference.  Default `".label"`, which [ard_normalize()] sets to the
#'   deepest hierarchy value, or to `variable_level` when there is no
#'   hierarchy.  `NULL` drops the label column, which is what you want when
#'   every `cells` entry is named.
#' @param cells The cell recipes.  Either one template string used everywhere,
#'   or a named list looked up by analysis variable, then by `context`, then
#'   by `"default"`.  Each entry is
#'   * `"{n} ({p})"` -- one row, labelled from `label`;
#'   * `c("{n} ({p})", "{n}")` -- one row, first template that resolves wins;
#'   * `c("Mean (SD)" = "{mean} ({sd})", "Min, Max" = "{min}, {max}")` -- one
#'     row per element, the name being the row label.
#'
#'   See [rtfreporter-ard] for the `{token:spec}` grammar.
#' @param stats `"cells"` (default) builds character cells from `cells`.
#'   `"rows"` ignores `cells` and gives every statistic its own row, labelled
#'   with `stat_label`, carrying the raw numeric `stat` -- the shape a PK
#'   concentration table wants.
#' @param levels Named list of level orders, e.g.
#'   `list(variable = c("AGE", "SEX"), TRT01P = c("Placebo", "Drug"))`.  Names
#'   may be either the source column or the renamed output column.  Row keys
#'   listed here become ordered factors and drive the row sort; column keys
#'   listed here drive the order of the spread columns.
#' @param labels Named character vector recoding key *values* to display text,
#'   e.g. `c(AGE = "Age (years)", SEX = "Sex [n (\%)]")`.  When a column is
#'   recoded and has no explicit `levels`, the order of `labels` becomes its
#'   level order.
#' @param sort Sort the output rows by the row keys.  The label column is used
#'   as a sort key only when `levels` gives it an explicit order.
#' @param ordered Make the factors built from `levels` / `labels` ordered.
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
                       levels = NULL, labels = NULL, sort = TRUE,
                       ordered = TRUE, sep = "____",
                       round = c("sas", "r"), spec = NULL,
                       sort_stat = NULL, na = NA_character_) {
  stats <- match.arg(stats)
  round <- match.arg(round)
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

  colrefs <- .ard_refs(cols, d, "cols")
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
        .valn = suppressWarnings(as.numeric(sub$stat)),
        .valc = NA_character_,
        .depth = if (".depth" %in% names(sub)) sub$.depth else 1L,
        .keys = I(rep(list(key_vals), nrow(sub))),
        stringsAsFactors = FALSE)
      next
    }

    entry <- .ard_lookup_cells(
      cells, sub$variable[1L], sub$context[1L],
      if (".kind" %in% names(sub)) sub$.kind[1L] else NA_character_)
    if (is.null(entry)) next

    if (!is.null(entry$labels)) {
      vals <- vapply(entry$chains, function(chain) {
        for (tpl in chain) {
          v <- .ard_fill(tpl, sub$stat_name, sub$stat, sub$stat_fmt, round)
          if (!is.na(v)) return(v)
        }
        NA_character_
      }, "")
      pieces[[length(pieces) + 1L]] <- data.frame(
        .lab = entry$labels, .col = ckey, .valn = NA_real_, .valc = vals,
        .depth = if (".depth" %in% names(sub)) sub$.depth[1L] else 1L,
        .keys = I(rep(list(key_vals), length(vals))),
        stringsAsFactors = FALSE)
    } else {
      labs <- lab_src[ii]
      for (lv in unique(labs)) {
        sel <- if (is.na(lv)) is.na(labs) else (!is.na(labs) & labs == lv)
        s2 <- sub[sel, , drop = FALSE]
        v <- NA_character_
        for (tpl in entry$chains[[1L]]) {
          v <- .ard_fill(tpl, s2$stat_name, s2$stat, s2$stat_fmt, round)
          if (!is.na(v)) break
        }
        pieces[[length(pieces) + 1L]] <- data.frame(
          .lab = lv, .col = ckey, .valn = NA_real_, .valc = v,
          .depth = if (".depth" %in% names(s2)) s2$.depth[1L] else 1L,
          .keys = I(list(key_vals)), stringsAsFactors = FALSE)
      }
    }
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
  keep_val <- if (identical(stats, "rows")) !is.na(long$.valn) else !is.na(long$.valc)
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
      long[[r$out]] <- .ard_as_factor(v, recode(lv), ordered = ordered)
    } else if (!is.null(labels) && any(as.character(long[[r$out]]) != v)) {
      long[[r$out]] <- .ard_as_factor(v, unname(labels[names(labels) %in%
        .ard_first_seen(long[[r$out]])]), ordered = ordered)
    } else {
      long[[r$out]] <- v
    }
  }
  if (!is.null(label_out)) {
    lv <- if (is.null(levels)) NULL else
      (levels[[label_out]] %||% levels[[labref[[1]]$ref]])
    if (!is.null(lv)) long[[label_out]] <- .ard_as_factor(long[[label_out]], lv, ordered)
  }

  rid <- do.call(paste, c(lapply(id_cols, function(k) as.character(long[[k]])),
                          list(sep = "\r")))
  row_first <- !duplicated(rid)
  base <- long[row_first, c(id_cols, ".depth"), drop = FALSE]
  base$.rid <- rid[row_first]

  mat <- matrix(na, nrow = nrow(base), ncol = length(col_levels),
                dimnames = list(NULL, col_levels))
  if (identical(stats, "rows")) {
    matn <- matrix(NA_real_, nrow = nrow(base), ncol = length(col_levels),
                   dimnames = list(NULL, col_levels))
  }
  ri <- match(rid, base$.rid)
  ci <- match(long$.col, col_levels)
  for (k in seq_len(nrow(long))) {
    if (is.na(ri[k]) || is.na(ci[k])) next
    if (identical(stats, "rows")) matn[ri[k], ci[k]] <- long$.valn[k]
    else mat[ri[k], ci[k]] <- long$.valc[k]
  }

  out <- base[, id_cols, drop = FALSE]
  vals <- if (identical(stats, "rows"))
    as.data.frame(matn, stringsAsFactors = FALSE, check.names = FALSE)
  else as.data.frame(mat, stringsAsFactors = FALSE, check.names = FALSE)
  out <- cbind(out, vals, stringsAsFactors = FALSE)

  if (!is.null(sort_stat)) {
    tot <- tapply(suppressWarnings(as.numeric(
      d$stat[d$stat_name == sort_stat])), rid_for_stat(d, rowrefs, labref,
      sort_stat), sum, na.rm = TRUE)
    out$.sort_stat <- as.numeric(tot[match(base$.rid, names(tot))])
  }

  if (isTRUE(sort) && length(rowname_cols)) {
    keys <- lapply(rowname_cols, function(k) out[[k]])
    if (!is.null(label_out) && is.factor(out[[label_out]])) {
      keys <- c(keys, list(out[[label_out]]))
    }
    out <- out[do.call(order, keys), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
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
                      levels = NULL, labels = NULL, sort = TRUE,
                      ordered = TRUE, sep = "____", round = c("sas", "r"),
                      spec = NULL, sort_stat = NULL, na = NA_character_,
                      drop_contexts = c("attributes", "total_n"),
                      drop_key_variables = TRUE) {
  x <- ard_normalize(ard, keys = keys, hierarchy = hierarchy,
                     overall = overall, drop_contexts = drop_contexts,
                     drop_key_variables = drop_key_variables)
  args <- list(x = x, cols = cols, rows = rows, label = label,
               stats = match.arg(stats), levels = levels, labels = labels,
               sort = sort, ordered = ordered, sep = sep,
               round = match.arg(round), spec = spec, sort_stat = sort_stat,
               na = na)
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
#'   `{n:raw}`     \tab the raw `stat`, `as.character()`, untouched
#' }
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

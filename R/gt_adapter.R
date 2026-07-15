# ============================================================================
#  gt_adapter -- read a gt_tbl (and, via as_gt(), a gtsummary table) into the
#  common rtftable "kwargs" list consumed by as_rtftables() / as_rtftable().
# ============================================================================
#
#  Design (since v0.0.46)
#  ----------------------
#  The table BODY is taken from `gt::extract_body(gt_obj, output = "html")` --
#  the same rendered-body route the (now deprecated) paginate() used.  This is
#  deliberately the cleanest source:
#
#    * only the VISIBLE columns appear (hidden / helper columns such as
#      tfrmt's `..tfrmt_row_grp_lbl` are dropped automatically);
#    * row-group / stub rows are already interleaved, with indentation baked
#      into the rendered label text;
#    * cells render exactly as gt would print them (no stray `<br />`
#      placeholders surviving as data).
#
#  On top of that clean body we read only the METADATA that the rtfreport
#  renderer can actually use: column labels, per-column alignment, spanning
#  headers, column widths, the title block, and the footnote / source-note
#  block (plus converting gt's in-cell footnote marks to `^{N}` superscript
#  markup).  Package-specific styling that RTF cannot reproduce -- per-cell
#  bold/italic from `tab_style()`, cell fills, markdown -- is intentionally
#  NOT read.
#
#  Everything is reduced to the same shape as the rtables adapter so
#  as_rtftables() consumes either source identically:
#    list(data, col_header, col_spec, col_rel_width / column_widths_twips,
#         titles_block, footnotes_block)


# Metadata tokens controllable through `read_meta = c(...)`.  `read_meta = TRUE`
# reads them all; `FALSE` reads none (the clean body is always produced).
.GT_META_TOKENS <- c("col_header", "alignment", "spanning", "widths",
                     "titles", "footnotes")


# ── Detection ────────────────────────────────────────────────────────────

# Is `x` a gt_tbl?  Cheap class check; does not import gt.
.is_gt_tbl <- function(x) inherits(x, "gt_tbl")

# Is `x` a gtsummary table?  (tbl_summary / tbl_regression / tbl_merge / …)
.is_gtsummary_tbl <- function(x) inherits(x, "gtsummary")

# Convert a gtsummary table to a gt_tbl via gtsummary::as_gt().
.gtsummary_to_gt <- function(x) {
  if (!requireNamespace("gtsummary", quietly = TRUE)) {
    stop("Reading from a gtsummary table requires the `gtsummary` package. ",
         "Install it with install.packages(\"gtsummary\").", call. = FALSE)
  }
  gt_obj <- gtsummary::as_gt(x)
  if (!.is_gt_tbl(gt_obj)) {
    stop("gtsummary::as_gt() did not return a gt_tbl.", call. = FALSE)
  }
  gt_obj
}

# Is `x` a gt_group -- gt's multi-table container from gt::gt_group() /
# gt::gt_split()?  tfrmt's print_to_gt() also returns one whenever the spec
# carries a page_plan.  Cheap class check; does not import gt.
.is_gt_group <- function(x) inherits(x, "gt_group")

# Expand a gt_group into the plain list of its member gt_tbl objects.
# Members are pulled through the exported gt::grp_pull() API; only the member
# COUNT comes from the object's own `gt_tbls` slot (list access, no `:::`),
# as gt exports no size accessor.
.gt_group_tables <- function(x) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("Reading from a gt_group requires the `gt` package.  Install it ",
         "with install.packages(\"gt\").", call. = FALSE)
  }
  n <- nrow(x[["gt_tbls"]])
  if (is.null(n) || n < 1L) return(list())
  lapply(seq_len(n), function(i) gt::grp_pull(x, which = i))
}

# Is `x` a gtsummary tbl_split -- the container returned by
# gtsummary::tbl_split_by_rows() / tbl_split_by_columns() (and the deprecated
# tbl_split())?  Cheap class check; does not import gtsummary.
.is_gtsummary_split <- function(x) inherits(x, "tbl_split")

# Expand a tbl_split into the plain list of its member gtsummary tables.
# Unlike a gt_group the object IS the member list, so dropping the container
# class is enough; centralized here so the handling mirrors
# .gt_group_tables() (and has one place to change if gtsummary ever adds
# non-table slots to the container).
.gtsummary_split_tables <- function(x) {
  unclass(x)
}


# ── Token resolution ─────────────────────────────────────────────────────

.resolve_gt_tokens <- function(read_meta) {
  .resolve_meta_tokens(read_meta, .GT_META_TOKENS, "gt")
}


# ── Small value helpers ──────────────────────────────────────────────────

# Convert a value that might be markdown_text, list-of-1, or NULL into a plain
# character string.  Returns NA_character_ when nothing usable.  Markdown
# bold markers (`**...**`) -- which gtsummary puts in `by`-column headers via
# modify_header() -- are stripped, since rtfreporter does not render Markdown
# and would otherwise show the literal asterisks.
.flatten_to_chr <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (length(x) == 0L) return(NA_character_)
  if (is.list(x))  x <- x[[1L]]
  if (is.null(x)) return(NA_character_)
  v <- as.character(x)[1L]
  if (is.na(v) || !nzchar(v)) return(NA_character_)
  v <- gsub("**", "", v, fixed = TRUE)        # strip markdown bold markers
  if (!nzchar(v)) return(NA_character_)
  v
}

# Title + subtitle (+ preheader) as a character vector for the page title.
.extract_titles <- function(gt_obj) {
  heading <- gt_obj[["_heading"]]
  if (is.null(heading)) return(NULL)
  rows <- c(.flatten_to_chr(heading$title),
            .flatten_to_chr(heading$subtitle),
            .flatten_to_chr(heading$preheader))
  rows <- rows[!is.na(rows)]
  if (!length(rows)) return(NULL)
  rows
}

# Source notes -> character vector for the page footnote block.
.extract_source_notes <- function(gt_obj) {
  notes <- gt_obj[["_source_notes"]]
  if (is.null(notes) || !length(notes)) return(NULL)
  out <- vapply(notes, .flatten_to_chr, character(1L))
  out <- out[!is.na(out)]
  if (!length(out)) return(NULL)
  out
}

# Table footnote texts -> character vector for the page footnote block.
.extract_footnote_texts <- function(gt_obj) {
  fn <- gt_obj[["_footnotes"]]
  if (is.null(fn) || !nrow(fn)) return(NULL)
  out <- vapply(fn$footnotes, function(ft) {
    if (is.null(ft) || !length(ft)) return(NA_character_)
    v <- vapply(seq_along(ft), function(i) .flatten_to_chr(ft[[i]]), character(1L))
    v <- v[!is.na(v)]
    if (!length(v)) NA_character_ else paste(v, collapse = " ")
  }, character(1L))
  out <- out[!is.na(out)]
  if (!length(out)) return(NULL)
  unname(out)
}

# Convert one gt column_width entry to list(kind, value).
.parse_one_width <- function(w) {
  while (is.list(w) && length(w)) w <- w[[1L]]
  if (is.null(w) || is.na(w) || !nzchar(as.character(w))) {
    return(list(kind = "missing", value = NA_real_))
  }
  s <- trimws(as.character(w))
  if (grepl("^[0-9.]+\\s*px$", s, ignore.case = TRUE)) {
    return(list(kind = "px",  value = as.numeric(sub("\\s*px\\s*$", "", s, ignore.case = TRUE))))
  }
  if (grepl("^[0-9.]+\\s*%$", s)) {
    return(list(kind = "pct", value = as.numeric(sub("\\s*%\\s*$", "", s))))
  }
  list(kind = "unknown", value = NA_real_)
}


# ── Body-cell HTML cleanup (extract_body(output = "html") returns HTML) ───

# Convert gt's in-cell footnote-mark HTML to rtfreporter `^{N}` superscript
# markup.  Regex-backreference-free (some locale-broken R builds drop `\\1`).
.convert_footnote_marks <- function(data) {
  span_pat <- "<span[^>]*gt_footnote_marks[^>]*>.*?</span>"
  sup_pat  <- "<sup>.*?</sup>"
  conv_one <- function(s) {
    if (is.na(s) || !grepl("gt_footnote_marks", s, fixed = TRUE)) return(s)
    m     <- gregexpr(span_pat, s, perl = TRUE)
    spans <- regmatches(s, m)[[1L]]
    if (!length(spans)) return(s)
    repl <- vapply(spans, function(sp) {
      sm   <- regmatches(sp, regexpr(sup_pat, sp, perl = TRUE))
      mark <- if (length(sm)) sub("</sup>$", "", sub("^<sup>", "", sm))
              else gsub("<[^>]+>", "", sp)
      mark <- trimws(gsub("[\u200b\u00a0]", "", mark))
      if (!nzchar(mark)) "" else paste0("^{", mark, "}")
    }, character(1L), USE.NAMES = FALSE)
    regmatches(s, m)[[1L]] <- repl
    s
  }
  for (j in seq_len(ncol(data))) {
    col <- data[[j]]
    if (!is.character(col)) next
    data[[j]] <- vapply(col, conv_one, character(1L), USE.NAMES = FALSE)
  }
  data
}

# Strip HTML tags from all character columns.  `<br>` becomes a newline
# (rendered as RTF \line).  A cell that is empty / whitespace / break-only
# AFTER stripping is normalised to "" -- this is what turns gt's `<br />`
# placeholder in tfrmt group-label rows into a genuinely empty cell instead
# of a stray newline.
.strip_html_from_df <- function(data) {
  for (j in seq_len(ncol(data))) {
    col <- data[[j]]
    if (!is.character(col)) next
    col <- gsub("<br\\s*/?>", "\n", col, ignore.case = TRUE, perl = TRUE)
    col <- gsub("<[^>]+>", "", col, perl = TRUE)
    # Decode the few HTML entities gt commonly emits.
    col <- gsub("&nbsp;", " ", col, fixed = TRUE)
    col <- gsub("&amp;",  "&", col, fixed = TRUE)
    col <- gsub("&lt;",   "<", col, fixed = TRUE)
    col <- gsub("&gt;",   ">", col, fixed = TRUE)
    # Whitespace/newline-only cells -> empty.
    blank <- !nzchar(gsub("[[:space:]\u200b\u00a0]", "", col))
    col[blank] <- ""
    data[[j]] <- col
  }
  data
}


# ── Spanning headers, mapped to the extract_body column order ────────────

# Build the spanner rows (a list of rows, each a list of col_cell()s) using
# `body_vars` -- the column ids returned by extract_body(), in render order --
# as the position reference.  Non-contiguous or fully-hidden spanners are
# skipped.  Returns list() when there are none.
.extract_spanners_body <- function(gt_obj, body_vars) {
  spans <- gt_obj[["_spanners"]]
  if (is.null(spans) || !nrow(spans)) return(list())
  levels_desc <- sort(unique(as.integer(spans$spanner_level)), decreasing = TRUE)
  rows <- list()
  for (lv in levels_desc) {
    spans_lv <- spans[as.integer(spans$spanner_level) == lv, , drop = FALSE]
    cells <- list()
    for (j in seq_len(nrow(spans_lv))) {
      vars <- intersect(as.character(spans_lv$vars[[j]]), body_vars)
      if (!length(vars)) next
      idx <- match(vars, body_vars)
      if (any(diff(sort(idx)) != 1L)) next            # skip non-contiguous
      pos <- if (length(idx) == 1L) idx else c(min(idx), max(idx))
      label <- .flatten_to_chr(spans_lv$spanner_label[[j]])
      if (is.na(label)) label <- ""
      cells[[length(cells) + 1L]] <- col_cell(pos = pos, label = label)
    }
    if (length(cells)) rows[[length(rows) + 1L]] <- cells
  }
  rows
}


# ── Body extraction (public extract_body, with a multi-stub fallback) ─────

# gt::extract_body() is the primary, PUBLIC route to the rendered body.  It
# assumes a SINGLE stub column, though: internally it does
# `rowname_col <- dt_boxhead_get_var_stub(data); if (is.na(rowname_col)) ...`,
# which errors `the condition has length > 1` when a table has more than one
# stub column.  That happens with tfrmt's `row_grp_plan(label_loc =
# element_row_grp_loc(location = "column" | "spanned"))`, which marks BOTH the
# group and the label columns as stub.  (gt's own renderer copes -- it takes
# `stub_var[length(stub_var)]` as the primary stub -- but extract_body does not.)
#
# For that one case we read the body from gt's own object slots `_data` +
# `_boxhead` -- list access only, the same coupling level the rest of this
# adapter already relies on, and deliberately NOT gt's unexported render
# internals.  tfrmt writes already-formatted strings into `_data`, so the body
# is display-ready.  The first stub column is renamed "::rowname::" so every
# metadata mapping below is identical to the extract_body() path.
.gt_extract_body_safe <- function(gt_obj) {
  boxh <- gt_obj[["_boxhead"]]
  multi_stub <- !is.null(boxh) &&
    sum(as.character(boxh$type) == "stub", na.rm = TRUE) > 1L
  if (!multi_stub) return(gt::extract_body(gt_obj, output = "html"))
  .gt_body_from_slots(gt_obj)
}

# Reconstruct the rendered body from `_data` + `_boxhead` for the multi-stub
# case.  Keeps visible columns (drops `type == "hidden"`) in boxhead (display)
# order, and names the first stub column "::rowname::" to match extract_body().
.gt_body_from_slots <- function(gt_obj) {
  boxh <- gt_obj[["_boxhead"]]
  dat  <- gt_obj[["_data"]]
  if (is.null(boxh) || is.null(dat)) {
    stop("Could not read the gt table body: `_boxhead` / `_data` missing.",
         call. = FALSE)
  }
  type <- as.character(boxh$type)
  vars <- as.character(boxh$var)
  vis  <- which(type %in% c("stub", "default") & vars %in% names(dat))
  if (!length(vis)) {
    stop("Could not read the gt table body: no visible columns found.",
         call. = FALSE)
  }
  out <- as.data.frame(dat[vars[vis]], stringsAsFactors = FALSE,
                       check.names = FALSE)
  st  <- which(type[vis] == "stub")
  if (length(st)) names(out)[st[1L]] <- "::rowname::"
  out
}


# ── Central mapping: gt_tbl + tokens -> rtftable kwargs ───────────────────

# Returns list(data, col_header, col_spec, column_widths_twips / col_rel_width,
# titles_block, footnotes_block) -- the same shape the rtables adapter yields.
.gt_to_rtftable_kwargs <- function(gt_obj, tokens = .GT_META_TOKENS) {
  if (!.is_gt_tbl(gt_obj)) stop("`gt_obj` must be a gt_tbl.", call. = FALSE)
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("Reading from a gt_tbl requires the `gt` package.  Install it with ",
         "install.packages(\"gt\").", call. = FALSE)
  }

  # ---- clean rendered body (visible columns only) ----------------------
  eb        <- .gt_extract_body_safe(gt_obj)
  body_vars <- names(eb)                                  # stub == "::rowname::"
  ncol_t    <- length(body_vars)

  df <- as.data.frame(eb, stringsAsFactors = FALSE, check.names = FALSE)
  df[] <- lapply(df, function(col) {
    col <- as.character(col)
    col[is.na(col)] <- ""
    col
  })
  # Safe, unique data.frame names (display labels come from col_header).
  raw <- ifelse(body_vars == "::rowname::", "rowname", body_vars)
  names(df) <- make.unique(make.names(raw))
  rownames(df) <- NULL

  # Clean the HTML extract_body emits: footnote marks -> ^{N}, then strip.
  df <- .convert_footnote_marks(df)
  df <- .strip_html_from_df(df)
  out <- list(data = df)

  boxh <- gt_obj[["_boxhead"]]
  # boxhead row index for each body column (stub -> the type == "stub" row).
  bx_idx <- vapply(body_vars, function(v) {
    if (identical(v, "::rowname::")) {
      w <- which(as.character(boxh$type) == "stub"); if (length(w)) w[1L] else NA_integer_
    } else {
      match(v, as.character(boxh$var))
    }
  }, integer(1L))

  # ---- col_header (labels) ---------------------------------------------
  if ("col_header" %in% tokens && !is.null(boxh)) {
    labs <- vapply(seq_len(ncol_t), function(j) {
      if (identical(body_vars[j], "::rowname::")) {
        sh <- gt_obj[["_stubhead"]]
        v  <- if (!is.null(sh)) .flatten_to_chr(sh$label) else NA_character_
      } else if (!is.na(bx_idx[j])) {
        v <- .flatten_to_chr(boxh$column_label[[bx_idx[j]]])
      } else v <- NA_character_
      if (is.na(v)) "" else v
    }, character(1L))
    out$col_header <- labs
  }

  # ---- alignment -> col_spec -------------------------------------------
  if ("alignment" %in% tokens && !is.null(boxh) &&
      "column_align" %in% names(boxh)) {
    out$col_spec <- lapply(seq_len(ncol_t), function(j) {
      a <- if (!is.na(bx_idx[j])) as.character(boxh$column_align[[bx_idx[j]]]) else NA
      if (is.na(a) || !a %in% c("left", "center", "right")) a <- "left"
      list(col = j, align = a)
    })
  }

  # ---- widths ----------------------------------------------------------
  if ("widths" %in% tokens && !is.null(boxh) &&
      "column_width" %in% names(boxh)) {
    parsed <- lapply(seq_len(ncol_t), function(j) {
      if (is.na(bx_idx[j])) list(kind = "missing", value = NA_real_)
      else .parse_one_width(boxh$column_width[[bx_idx[j]]])
    })
    kinds <- vapply(parsed, function(x) x$kind,  character(1L))
    vals  <- vapply(parsed, function(x) x$value, numeric(1L))
    if (all(kinds == "px")) {
      out$column_widths_twips <- as.integer(round(vals * 15))
    } else if (all(kinds == "pct")) {
      out$col_rel_width <- vals
    }
  }

  # ---- spanning -> stacked col_header ----------------------------------
  if ("spanning" %in% tokens) {
    span_rows <- .extract_spanners_body(gt_obj, body_vars)
    if (length(span_rows)) {
      bottom <- out$col_header %||% names(df)
      out$col_header <- c(span_rows, list(bottom))
    }
  }

  # ---- titles / footnotes (page-level blocks) --------------------------
  if ("titles" %in% tokens) {
    tt <- .extract_titles(gt_obj)
    if (!is.null(tt)) out$titles_block <- tt
  }
  if ("footnotes" %in% tokens) {
    fn <- c(.extract_footnote_texts(gt_obj), .extract_source_notes(gt_obj))
    fn <- fn[!is.na(fn) & nzchar(fn)]
    if (length(fn)) out$footnotes_block <- fn
  }

  out
}


# ── Used by rtf_tables(): user-supplied block wins over the gt-extracted one
.merge_gt_block <- function(user_block, gt_block) {
  if (!is.null(user_block)) return(user_block)
  gt_block
}

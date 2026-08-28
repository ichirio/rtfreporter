# ============================================================================
#  style_verbs -- post-hoc styling verbs for rtftable objects and page lists
# ============================================================================
#
#  Motivation (#218): a table read through as_rtftables(read_meta = TRUE)
#  already carries labels, alignment and spanning extracted from the source
#  object.  Fine-tuning one rule or one cell used to require manual list
#  surgery (tbl$col_header[[2]][[2]]$border <- ...).  These verbs make the
#  same edits first-class:
#
#    as_rtftables(gt_obj) |>
#      style_header(row = 2, cols = 2:4,
#                   border = rtf_border(top    = rtf_border_side("single"),
#                                       bottom = rtf_border_side("none"))) |>
#      style_body(rows = ~ label == "Mean", bold = TRUE)
#
#  Design (locked with the maintainer):
#    * a small VERB family, applied eagerly -- no lazy layer machinery;
#    * every verb is an S3 generic with an `rtftable` method and a `list`
#      method (the page list as_rtftables() returns), so one call styles a
#      single table or every page alike;
#    * merge semantics everywhere are the renderer's documented border rule
#      generalized: LAST WRITER WINS, per side / per field.

# ── shared helpers ─────────────────────────────────────────────────────────

# Map a verb over a page list, insisting every element is an rtftable.
.style_map_pages <- function(x, fun, ..., verb) {
  ok <- vapply(x, inherits, logical(1L), "rtftable")
  if (!all(ok)) {
    stop(sprintf(
      "`%s()` on a list expects every element to be an rtftable (as returned by as_rtftables()); element %d is '%s'.",
      verb, which(!ok)[1L], paste(class(x[[which(!ok)[1L]]]), collapse = "/")),
      call. = FALSE)
  }
  out <- lapply(x, fun, ...)
  names(out) <- names(x)
  out
}

# The data frame(s) behind a table's body, as a list (multi-DF tables have
# one element per constituent data.frame).
.style_body_frames <- function(x) {
  if (!is.null(x$data_list)) x$data_list else list(x$data)
}

# Reference data.frame used to resolve column names/positions.
.style_ref_df <- function(x) {
  if (!is.null(x$data)) x$data else x$data_list[[1L]]
}

# Resolve `cols` (names / positions / NULL = all) to integer indices.
.style_resolve_cols <- function(x, cols, verb) {
  ref <- .style_ref_df(x)
  if (is.null(cols)) return(seq_len(ncol(ref)))
  .resolve_col_indices(cols, ref, paste0(verb, "(cols)"))
}

# Resolve `rows` for style_body(): NULL = all body rows; an integer vector of
# row positions; a logical vector over all rows; a predicate function(data);
# or a one-sided formula evaluated inside the body data (base eval -- columns
# are visible as bare names, e.g. ~ label == "Mean").
.style_resolve_rows <- function(rows, frames, verb) {
  total <- sum(vapply(frames, nrow, integer(1L)))
  if (is.null(rows)) return(seq_len(total))

  per_frame <- function(f) {
    unlist(lapply(frames, function(df) {
      v <- f(df)
      if (!is.logical(v) || length(v) != nrow(df)) {
        stop(sprintf(
          "`%s(rows = )` predicate must return a logical vector of length nrow(data) (%d); got %s of length %d.",
          verb, nrow(df), typeof(v), length(v)), call. = FALSE)
      }
      v & !is.na(v)
    }), use.names = FALSE)
  }

  if (inherits(rows, "formula")) {
    if (length(rows) != 2L) {
      stop(sprintf("`%s(rows = )` formula must be one-sided (~ expr).", verb),
           call. = FALSE)
    }
    expr <- rows[[2L]]
    env  <- environment(rows)
    return(which(per_frame(function(df) {
      v <- eval(expr, df, env)
      if (is.logical(v) && length(v) == 1L) v <- rep(v, nrow(df))
      v
    })))
  }
  if (is.function(rows)) return(which(per_frame(rows)))
  if (is.logical(rows)) {
    if (length(rows) != total) {
      stop(sprintf(
        "`%s(rows = )` logical vector must have length %d (the number of body rows); got %d.",
        verb, total, length(rows)), call. = FALSE)
    }
    return(which(rows & !is.na(rows)))
  }
  if (is.numeric(rows)) {
    v <- as.integer(rows)
    if (anyNA(v) || any(v < 1L) || any(v > total)) {
      stop(sprintf("`%s(rows = )` positions must be in 1..%d.", verb, total),
           call. = FALSE)
    }
    return(v)
  }
  stop(sprintf(
    "`%s(rows = )` must be NULL, row positions, a logical vector, a predicate function(data), or a one-sided formula.",
    verb), call. = FALSE)
}

.style_check_align <- function(a, verb) {
  if (!is.character(a) || length(a) != 1L ||
      !a %in% c("left", "center", "right")) {
    stop(sprintf("`%s(align = )` must be \"left\", \"center\", or \"right\".",
                 verb), call. = FALSE)
  }
  a
}

.style_check_border <- function(b, verb) {
  if (!inherits(b, "rtf_border")) {
    stop(sprintf("`%s(border = )` must be an rtf_border() object.", verb),
         call. = FALSE)
  }
  b
}

.style_check_flag <- function(v, arg, verb) {
  if (!is.logical(v) || length(v) != 1L || is.na(v)) {
    stop(sprintf("`%s(%s = )` must be TRUE or FALSE.", verb, arg),
         call. = FALSE)
  }
  v
}

# Promote a character labels row to the equivalent list of single-column
# cells, copying each column's current header styling (header_align /
# header_bold / header_italic) onto the cell so the rendered output stays the
# same.  Needed when a per-cell property (border, underline) is requested on
# a labels row, which the labels renderer cannot express.  NB the promoted
# row is rendered through the spanning zone; that zone falls back to the
# header zone when unset, so appearance only changes if a *distinct*
# `spanning` zone border was configured.
.style_promote_labels_row <- function(labels, col_spec) {
  lapply(seq_along(col_spec), function(j) {
    spec <- col_spec[[j]]
    list(
      from   = j,
      to     = j,
      label  = if (j <= length(labels)) as.character(labels[[j]]) else "",
      align  = spec$header_align %||% "center",
      bold   = isTRUE(spec$header_bold),
      italic = isTRUE(spec$header_italic)
    )
  })
}

# Patch the cells of one cell-style header row.  A cell is targeted when its
# [from, to] span intersects `cols_idx`.  `label` is recycled over the
# targeted cells in order.
.style_patch_header_cells <- function(row, cols_idx, label, border, align,
                                      bold, italic, underline, verb) {
  targeted <- which(vapply(row, function(cell) {
    f <- as.integer(cell$from)
    t <- as.integer(cell$to)
    any(cols_idx >= f & cols_idx <= t)
  }, logical(1L)))
  if (length(targeted) == 0L) {
    warning(sprintf("`%s()`: no header cells intersect the requested columns; nothing changed.",
                    verb), call. = FALSE)
    return(row)
  }
  labels <- if (!is.null(label)) rep(as.character(label),
                                     length.out = length(targeted))
  for (k in seq_along(targeted)) {
    cell <- row[[targeted[k]]]
    if (!is.null(label))     cell$label     <- labels[[k]]
    if (!is.null(border))    cell$border    <- .merge_rtf_border(cell$border, border)
    if (!is.null(align))     cell$align     <- align
    if (!is.null(bold))      cell$bold      <- bold
    if (!is.null(italic))    cell$italic    <- italic
    if (!is.null(underline)) cell$underline <- underline
    row[[targeted[k]]] <- cell
  }
  row
}


# ── style_header() ─────────────────────────────────────────────────────────

#' Restyle an existing rtftable (post-hoc styling verbs)
#'
#' These verbs edit an already-built [rtftable()] -- typically one produced by
#' [as_rtftables()] with `read_meta = TRUE`, where labels, alignment and
#' spanning are already correct and only a detail needs to change.  Each verb
#' returns a modified copy (plain copy-on-modify S3) and is an S3 generic with
#' two methods: one for a single `rtftable`, one for a **list of pages** as
#' returned by [as_rtftables()] (the edit is applied to every page).  They are
#' designed to chain with the native pipe:
#'
#' ```
#' pages <- as_rtftables(gt_obj, read_meta = TRUE) |>
#'   style_header(row = 2, cols = 2:4,
#'                border = rtf_border(top    = rtf_border_side("single"),
#'                                    bottom = rtf_border_side("none"))) |>
#'   style_cols(cols = "AGE", align = "center") |>
#'   style_body(rows = ~ label == "Mean", bold = TRUE)
#' ```
#'
#' The merge rule everywhere is the border rule the renderer already
#' documents, generalized: **last writer wins, per side / per field**.  A
#' border passed to a verb is merged side-by-side onto whatever is already
#' there (`NULL` sides leave the existing side alone; use
#' `rtf_border_side("none")` for an explicit "no line").
#'
#' @section Body cells:
#' `style_body()` overrides, per cell, everything the data-row renderer
#' otherwise takes from `col_spec` -- `bold` / `italic` / `underline` /
#' `indent_twips` / `color` / `align` -- plus `border`, which merges on top
#' of the row's resolved zone border (`body` crossed with `first_row` /
#' `last_row`), per side.  So a rule under a summary row is
#' `style_body(rows = ~ Item == "Total",
#' border = rtf_border(bottom = rtf_border_side("single")))`, and an
#' `rtf_border_side("none")` side erases a zone rule on the selected cells.
#' Row height and cell padding are deliberately *not* per-row properties --
#' they stay uniform per table / document (see `rtf_document(default_format)`
#' and the `rtfreporter.*` options).
#'
#' @section Header rows and the two row kinds:
#' A column header holds two kinds of rows (see [rtf_col_header()]): **cell
#' rows** (lists of [col_cell()] spans -- spanning rows, or any row built from
#' cells) and one or more **character label rows**.  `style_header()` patches
#' cell rows directly.  On a label row, `bold` / `italic` / `align` are stored
#' in the per-column `col_spec` header styling (note: that styling is shared
#' by *all* label rows of the table), and a `border` or `underline` request
#' -- which the label-row renderer cannot express per cell -- first
#' **promotes** the row to an equivalent list of single-column cells (labels
#' and current header styling are copied over, so the rendered output is
#' unchanged; the promoted row renders through the `spanning` border zone,
#' which falls back to the `header` zone when unset).
#'
#' @param x An [rtftable()], or a list of them (pages from [as_rtftables()]).
#' @param row Integer header-row index(es), 1 = top row.  `NULL` (default)
#'   targets every header row.
#' @param cols Data-column selection: integer positions and/or column names of
#'   the table body (see the *What the columns are called* section of
#'   [as_rtftables()]).  `NULL` (default) = all columns.  Mix names and
#'   positions with a `list()` (`c()` would coerce the numbers to strings --
#'   same convention as `drop_cols` / `sort_by`).  A spanning cell is
#'   targeted when its span **intersects** `cols`.
#' @param label Optional replacement label(s), recycled over the targeted
#'   cells in order.
#' @param border An [rtf_border()] merged onto the targeted cells
#'   (side-by-side; existing sides survive where the new border leaves them
#'   `NULL`).  In `style_body()` the merge lands on top of the row's zone
#'   border; in `style_cols()` it becomes the column's header-cell border.
#' @param align `"left"`, `"center"`, or `"right"`.
#' @param bold,italic,underline `TRUE`/`FALSE`.  (On a character label row
#'   `underline` triggers the cell promotion described above.)
#' @param ... Passed between methods.
#'
#' @return An object of the same shape as `x` (rtftable, or list of pages),
#'   modified.
#'
#' @examples
#' df <- data.frame(Item = c("Age", "Sex"), A = 1:2, B = 3:4, C = 5:6)
#' hdr <- rtf_col_header(
#'   list(col_cell(1, ""), col_cell(c(2, 4), "Treatment")),
#'   list(col_cell(1, ""), col_cell(c(2, 4), "(N = 254)")),
#'   c("Item", "Placebo", "Drug A", "Drug B")
#' )
#' tbl <- rtftable(df, col_header = hdr, border = "tfl")
#'
#' # Solid rule above -- and no rule below -- the "(N = 254)" cell only:
#' tbl <- tbl |>
#'   style_header(row = 2, cols = 2:4,
#'                border = rtf_border(top    = rtf_border_side("single"),
#'                                    bottom = rtf_border_side("none")))
#'
#' # Centre the body of columns B and C, bold the "Age" row:
#' tbl <- tbl |>
#'   style_cols(cols = c("B", "C"), align = "center") |>
#'   style_body(rows = ~ Item == "Age", bold = TRUE)
#'
#' # Double rule under the whole table:
#' tbl <- tbl |>
#'   style_zone(last_row = rtf_border(bottom = rtf_border_side("double")))
#'
#' # Add a top header row after the fact:
#' tbl <- tbl |>
#'   add_header_row(list(col_cell(c(2, 4), "STUDY01")), .position = "top")
#' @export
style_header <- function(x, ...) UseMethod("style_header")

#' @rdname style_header
#' @export
style_header.rtftable <- function(x, row = NULL, cols = NULL, label = NULL,
                                  border = NULL, align = NULL, bold = NULL,
                                  italic = NULL, underline = NULL, ...) {
  .check_own_dots(list(...), style_header.rtftable, "style_header")
  if (is.null(x$col_header) || length(x$col_header) == 0L) {
    stop("`style_header()`: this rtftable has no column header.", call. = FALSE)
  }
  if (!is.null(border)) border <- .style_check_border(border, "style_header")
  if (!is.null(align))  align  <- .style_check_align(align, "style_header")
  if (!is.null(bold))      bold      <- .style_check_flag(bold, "bold", "style_header")
  if (!is.null(italic))    italic    <- .style_check_flag(italic, "italic", "style_header")
  if (!is.null(underline)) underline <- .style_check_flag(underline, "underline", "style_header")

  hdrs <- x$col_header
  idx  <- if (is.null(row)) seq_along(hdrs) else as.integer(row)
  if (anyNA(idx) || any(idx < 1L) || any(idx > length(hdrs))) {
    stop(sprintf("`style_header(row = )` must be in 1..%d (the header rows, top first).",
                 length(hdrs)), call. = FALSE)
  }
  cols_idx <- .style_resolve_cols(x, cols, "style_header")

  n_label_rows <- sum(vapply(hdrs, is.character, logical(1L)))
  for (ri in idx) {
    r <- hdrs[[ri]]
    if (is.character(r)) {
      if (!is.null(border) || !is.null(underline)) {
        # Per-cell request on a labels row: promote to cells, then patch.
        r <- .style_promote_labels_row(r, x$col_spec)
        hdrs[[ri]] <- .style_patch_header_cells(
          r, cols_idx, label, border, align, bold, italic, underline,
          "style_header")
      } else {
        if (!is.null(label)) {
          r[cols_idx] <- rep(as.character(label), length.out = length(cols_idx))
          hdrs[[ri]] <- r
        }
        if ((!is.null(bold) || !is.null(italic) || !is.null(align)) &&
            n_label_rows > 1L) {
          warning("`style_header()`: header text styling (bold/italic/align) is stored per column and shared by ALL label rows.",
                  call. = FALSE)
        }
        for (j in cols_idx) {
          if (!is.null(bold))   x$col_spec[[j]]$header_bold   <- bold
          if (!is.null(italic)) x$col_spec[[j]]$header_italic <- italic
          if (!is.null(align))  x$col_spec[[j]]$header_align  <- align
        }
      }
    } else {
      hdrs[[ri]] <- .style_patch_header_cells(
        r, cols_idx, label, border, align, bold, italic, underline,
        "style_header")
    }
  }
  x$col_header <- hdrs
  # Multi-DF tables render from col_header_list; keep it in step.
  if (!is.null(x$col_header_list)) {
    x$col_header_list <- lapply(x$col_header_list, function(h) {
      if (identical(h, x$col_header)) hdrs else h
    })
    x$col_header_list[[1L]] <- hdrs
  }
  x
}

#' @rdname style_header
#' @export
style_header.list <- function(x, ...) {
  .style_dispatch_pages(x, style_header, style_header.rtftable,
                        "style_header", list(...))
}


# ── style_cols() ───────────────────────────────────────────────────────────

#' @rdname style_header
#' @param indent_twips Integer left indent for the column's body cells.
#' @param color Body text colour, `"#RRGGBB"`.
#' @param header_align,header_bold,header_italic Header-label styling for the
#'   selected columns (what the character label rows render with).
#' @export
style_cols <- function(x, ...) UseMethod("style_cols")

#' @rdname style_header
#' @export
style_cols.rtftable <- function(x, cols = NULL, align = NULL, bold = NULL,
                                italic = NULL, underline = NULL,
                                indent_twips = NULL, color = NULL,
                                border = NULL, header_align = NULL,
                                header_bold = NULL, header_italic = NULL,
                                ...) {
  .check_own_dots(list(...), style_cols.rtftable, "style_cols")
  cols_idx <- .style_resolve_cols(x, cols, "style_cols")
  if (!is.null(align))        align        <- .style_check_align(align, "style_cols")
  if (!is.null(header_align)) header_align <- .style_check_align(header_align, "style_cols")
  if (!is.null(border))       border       <- .style_check_border(border, "style_cols")
  for (arg in c("bold", "italic", "underline", "header_bold", "header_italic")) {
    v <- get(arg)
    if (!is.null(v)) assign(arg, .style_check_flag(v, arg, "style_cols"))
  }
  for (j in cols_idx) {
    spec <- x$col_spec[[j]]
    if (!is.null(align))         spec$align         <- align
    if (!is.null(bold))          spec$bold          <- bold
    if (!is.null(italic))        spec$italic        <- italic
    if (!is.null(underline))     spec$underline     <- underline
    if (!is.null(indent_twips))  spec$indent_twips  <- as.integer(indent_twips)
    if (!is.null(color))         spec$color         <- as.character(color)
    if (!is.null(border))        spec$border        <- .merge_rtf_border(spec$border, border)
    if (!is.null(header_align))  spec$header_align  <- header_align
    if (!is.null(header_bold))   spec$header_bold   <- header_bold
    if (!is.null(header_italic)) spec$header_italic <- header_italic
    x$col_spec[[j]] <- spec
  }
  x
}

#' @rdname style_header
#' @export
style_cols.list <- function(x, ...) {
  .style_dispatch_pages(x, style_cols, style_cols.rtftable,
                        "style_cols", list(...))
}


# ── style_body() ───────────────────────────────────────────────────────────

#' @rdname style_header
#' @param rows Body-row selection: `NULL` (all rows), integer positions, a
#'   logical vector over all body rows, a predicate `function(data)`
#'   returning one logical per row, or a one-sided formula evaluated inside
#'   the body data (columns visible as bare names, e.g.
#'   `rows = ~ label == "Mean"`).  On a **page list**, integer and logical
#'   selections are rejected -- page-local row numbers are ambiguous across
#'   pages -- so use a predicate/formula (evaluated per page) or `NULL`.
#' @export
style_body <- function(x, ...) UseMethod("style_body")

#' @rdname style_header
#' @export
style_body.rtftable <- function(x, rows = NULL, cols = NULL, bold = NULL,
                                italic = NULL, underline = NULL,
                                indent_twips = NULL, color = NULL,
                                align = NULL, border = NULL, ...) {
  .check_own_dots(list(...), style_body.rtftable, "style_body")
  frames   <- .style_body_frames(x)
  rows_idx <- .style_resolve_rows(rows, frames, "style_body")
  cols_idx <- .style_resolve_cols(x, cols, "style_body")
  for (arg in c("bold", "italic", "underline")) {
    v <- get(arg)
    if (!is.null(v)) assign(arg, .style_check_flag(v, arg, "style_body"))
  }
  if (!is.null(align))  align  <- .style_check_align(align, "style_body")
  if (!is.null(border)) border <- .style_check_border(border, "style_body")
  if (length(rows_idx) == 0L) return(x)

  total <- sum(vapply(frames, nrow, integer(1L)))
  ncols <- ncol(.style_ref_df(x))
  cs_all <- x$cell_styles %||% rep(list(NULL), total)

  blank <- function(proto) rep(proto, ncols)
  for (r in rows_idx) {
    cs <- cs_all[[r]] %||% list()
    if (!is.null(bold)) {
      v <- cs$bold %||% blank(NA);            v[cols_idx] <- bold
      cs$bold <- v
    }
    if (!is.null(italic)) {
      v <- cs$italic %||% blank(NA);          v[cols_idx] <- italic
      cs$italic <- v
    }
    if (!is.null(underline)) {
      v <- cs$underline %||% blank(NA);       v[cols_idx] <- underline
      cs$underline <- v
    }
    if (!is.null(indent_twips)) {
      v <- cs$indent_twips %||% blank(NA_integer_)
      v[cols_idx] <- as.integer(indent_twips)
      cs$indent_twips <- v
    }
    if (!is.null(color)) {
      v <- cs$color %||% blank(NA_character_)
      v[cols_idx] <- as.character(color)
      cs$color <- v
    }
    if (!is.null(align)) {
      v <- cs$align %||% blank(NA_character_)
      v[cols_idx] <- align
      cs$align <- v
    }
    if (!is.null(border)) {
      v <- cs$border %||% vector("list", ncols)
      for (j in cols_idx) v[[j]] <- .merge_rtf_border(v[[j]], border)
      cs$border <- v
    }
    cs_all[[r]] <- cs
  }
  x$cell_styles <- cs_all
  x
}

#' @rdname style_header
#' @export
style_body.list <- function(x, rows = NULL, ...) {
  if (is.numeric(rows) || is.logical(rows)) {
    stop("`style_body()` on a page list cannot take integer/logical `rows` -- page-local row numbers are ambiguous across pages. Use a predicate function or one-sided formula (evaluated per page), or style a single page directly.",
         call. = FALSE)
  }
  .style_dispatch_pages(x, style_body, style_body.rtftable, "style_body",
                        list(...), fixed = list(rows = rows))
}


# ── style_zone() ───────────────────────────────────────────────────────────

#' @rdname style_header
#' @param header,spanning,body,first_row,last_row Per-zone [rtf_border()]
#'   overrides, merged side-by-side onto the table's current
#'   [rtf_table_border()] (see the *Borders* article for what each zone
#'   covers).
#' @export
style_zone <- function(x, ...) UseMethod("style_zone")

#' @rdname style_header
#' @export
style_zone.rtftable <- function(x, header = NULL, spanning = NULL,
                                body = NULL, first_row = NULL,
                                last_row = NULL, ...) {
  .check_own_dots(list(...), style_zone.rtftable, "style_zone")
  zones <- list(header = header, spanning = spanning, body = body,
                first_row = first_row, last_row = last_row)
  zones <- zones[!vapply(zones, is.null, logical(1L))]
  if (length(zones) == 0L) return(x)
  for (z in names(zones)) .style_check_border(zones[[z]], "style_zone")

  base <- x$border
  if (is.null(base)) base <- rtf_table_border()
  for (z in names(zones)) {
    base[[z]] <- .merge_rtf_border(base[[z]], zones[[z]])
  }
  x$border <- base
  x
}

#' @rdname style_header
#' @export
style_zone.list <- function(x, ...) {
  .style_dispatch_pages(x, style_zone, style_zone.rtftable,
                        "style_zone", list(...))
}


# ── add_header_row() ───────────────────────────────────────────────────────

#' @rdname style_header
#' @param .position `"top"` (default) prepends the new row above the existing
#'   header rows; `"bottom"` appends below.
#' @export
add_header_row <- function(x, ...) UseMethod("add_header_row")

#' @rdname style_header
#' @export
add_header_row.rtftable <- function(x, row, .position = c("top", "bottom"),
                                    ...) {
  .check_own_dots(list(...), add_header_row.rtftable, "add_header_row")
  .position <- match.arg(.position)
  ref       <- .style_ref_df(x)
  ncol_df   <- ncol(ref)
  new_row   <- .normalize_col_header_rows(list(row), ncol_df, names(ref))[[1L]]

  insert <- function(hdr) {
    hdr <- hdr %||% list()
    if (.position == "top") c(list(new_row), hdr) else c(hdr, list(new_row))
  }
  x$col_header <- insert(x$col_header)
  if (!is.null(x$col_header_list)) {
    x$col_header_list <- lapply(x$col_header_list, insert)
  }
  x
}

#' @rdname style_header
#' @export
add_header_row.list <- function(x, ...) {
  .style_dispatch_pages(x, add_header_row, add_header_row.rtftable,
                        "add_header_row", list(...))
}


# ── set_col_header() ───────────────────────────────────────────────────────

#' Set the whole column header of a finished table (final-table coordinates)
#'
#' `set_col_header()` replaces the column header of an already-built
#' [rtftable()] -- typically the output of [as_rtftables()] -- resolving every
#' cell against the **final, printed table**: the columns you actually see,
#' addressed by **name** or by **visible position** (`1` = first printed
#' column).  Because it runs on the finished table, there is no "intermediate"
#' column layout to reason about -- no hidden `drop_cols`, no `stub_vars`
#' bookkeeping, no position shifting.  This is the recommended way to attach a
#' multi-row / spanning header on top of an `as_rtftables()` pipeline; pair it
#' with [rtf_columns()] to see the exact column names first.
#'
#' Contrast with the `col_header =` argument of [rtftable()] / [as_rtftables()],
#' whose positions refer to the source body **before** `drop_cols` /
#' `stub_vars` are applied.  `set_col_header()` always speaks the final table's
#' coordinates.
#'
#' Like the other post-hoc verbs it is an S3 generic with an `rtftable` method
#' and a **list** method (every page of an [as_rtftables()] result), and it
#' chains with the native pipe.
#'
#' @param x An [rtftable()], or a list of them (pages from [as_rtftables()]).
#' @param ... The header rows, in render order (top first) -- each a character
#'   vector (a label row, optionally named to place labels by column name) or a
#'   list of [col_cell()] cells (a spanning / cell row, whose `pos` may be
#'   column names or final positions).  Alternatively a single pre-built
#'   [rtf_col_header()] object.  Passing nothing clears the header.
#' @param align Optional column-header text alignment for the final columns:
#'   `"left"`/`"center"`/`"right"` (applied to every column) or a character
#'   vector of length `ncol` (one per printed column).  `NULL` (default) leaves
#'   the current header alignment untouched.
#'
#' @return An object of the same shape as `x` (rtftable, or list of pages).
#'
#' @seealso [rtf_columns()] to list the final column names; [rtf_col_header()]
#'   / [col_cell()] to build header rows; [add_header_row()] to add a single
#'   row; [style_header()] to restyle existing header cells.
#'
#' @examples
#' df <- data.frame(row_label = c("A", "B"),
#'                  g1 = 1:2, g2 = 3:4, Total = 5:6)
#' tbl <- rtftable(df)
#' tbl <- set_col_header(
#'   tbl,
#'   list(col_cell("row_label", ""), col_cell(c("g1", "g2"), "Treatment")),
#'   c(row_label = "Category", g1 = "Low", g2 = "High", Total = "Total")
#' )
#' @export
set_col_header <- function(x, ...) UseMethod("set_col_header")

#' @rdname set_col_header
#' @export
set_col_header.rtftable <- function(x, ..., align = NULL) {
  rows <- list(...)
  # One argument is treated as a complete col_header spec in any shape
  # `.normalize_col_header_rows()` accepts (an rtf_col_header object, a
  # character label row, a single cell row, or a list of rows); several
  # arguments are wrapped as consecutive rows.
  header <-
    if (length(rows) == 0L) NULL
    else if (length(rows) == 1L) rows[[1L]]
    else do.call(rtf_col_header, rows)

  ref <- .style_ref_df(x)
  nc  <- ncol(ref)
  cn  <- names(ref)

  if (!is.null(x$data_list)) {
    x$col_header_list <-
      .normalize_multi_col_header(header, length(x$data_list), nc, cn)
    x$col_header <- x$col_header_list[[1L]]
  } else {
    x$col_header <- .normalize_col_header_rows(header, nc, cn)
  }

  if (!is.null(align)) {
    a <- if (length(align) == 1L) rep(as.character(align), nc) else as.character(align)
    if (length(a) != nc) {
      stop(sprintf("`set_col_header(align = )` must have length 1 or %d (one per printed column).",
                   nc), call. = FALSE)
    }
    if (!all(a %in% c("left", "center", "right"))) {
      stop("`set_col_header(align = )` values must be \"left\", \"center\", or \"right\".",
           call. = FALSE)
    }
    for (j in seq_len(nc)) x$col_spec[[j]]$header_align <- a[[j]]
  }
  x
}

#' @rdname set_col_header
#' @export
set_col_header.list <- function(x, ...) {
  .style_map_pages(x, set_col_header, ..., verb = "set_col_header")
}


# ── set_header_cell() ──────────────────────────────────────────────────────

# Convert a col_cell() to a header cell {from, to, <attrs>}, resolving its `pos`
# (name(s) or position(s)) against the final column names.
.header_cell_from_col_cell <- function(cc, cn, nc) {
  if (!inherits(cc, "rtf_col_cell")) {
    stop("`set_header_cell()` takes col_cell() objects in `...`.", call. = FALSE)
  }
  p    <- .resolve_cell_pos(cc$pos, cn)
  from <- as.integer(p[[1L]])
  to   <- if (length(p) == 1L) from else as.integer(p[[2L]])
  if (from > to) { tmp <- from; from <- to; to <- tmp }
  if (from < 1L || to > nc) {
    stop(sprintf("col_cell() covers columns %d-%d, outside 1..%d.", from, to, nc),
         call. = FALSE)
  }
  spec <- list(from = from, to = to, label = cc$label %||% "")
  for (k in c("align", "bold", "italic", "underline", "border")) {
    if (!is.null(cc[[k]])) spec[[k]] <- cc[[k]]
  }
  spec
}

# Splice `new_cells` into a header row `cur` (a list of {from,to,label,...}
# cells that cover columns 1..N with no gaps/overlaps).  Untargeted cells are
# preserved; every new span must align to existing cell boundaries (no partial
# split of an existing span); the new spans must not overlap each other.
.splice_header_cells <- function(cur, new_cells) {
  cells  <- cur[order(vapply(cur, function(c) as.integer(c$from), integer(1L)))]
  starts <- vapply(cells, function(c) as.integer(c$from), integer(1L))
  ends   <- vapply(cells, function(c) as.integer(c$to),   integer(1L))

  ord <- order(vapply(new_cells, function(c) c$from, integer(1L)))
  nc2 <- new_cells[ord]
  for (i in seq_along(nc2)) {
    a <- nc2[[i]]$from; b <- nc2[[i]]$to
    if (i > 1L && a <= nc2[[i - 1L]]$to) {
      stop(sprintf("Requested header cells overlap (columns %d-%d and %d-%d).",
                   nc2[[i - 1L]]$from, nc2[[i - 1L]]$to, a, b), call. = FALSE)
    }
    if (!(a %in% starts) || !(b %in% ends)) {
      stop(sprintf(
        "Target columns %d-%d do not align to existing header-cell boundaries in that row (would split a spanning cell); adjust the span.",
        a, b), call. = FALSE)
    }
  }

  covered <- function(c) any(vapply(new_cells, function(n)
    as.integer(c$from) >= n$from && as.integer(c$to) <= n$to, logical(1L)))
  out <- c(Filter(function(c) !covered(c), cells), new_cells)
  out[order(vapply(out, function(c) as.integer(c$from), integer(1L)))]
}

#' Set or merge individual column-header cells (spanning, borders, alignment)
#'
#' Edits **specific cells of one header row** of a finished [rtftable()] (or a
#' list of pages) without rebuilding the whole header: place one or more
#' [col_cell()]s -- by column **name or position**, spanning via `c(a, b)` --
#' into row `row`, keeping the other cells of that row intact.  Merging
#' currently-separate cells into one spanning cell is the core use.  Borders,
#' alignment and text decorations are carried natively by [col_cell()].
#'
#' A **label row** is promoted to cells first (as in [style_header()]), so the
#' row then renders through the `spanning` border zone (which falls back to the
#' `header` zone when unset).  Merging cells in one row replaces their
#' individual labels with the single span label -- to keep the sub-labels, put
#' the span on a separate upper row (e.g. via [add_header_row()]).
#'
#' @param x An [rtftable()], or a list of them (pages from [as_rtftables()]).
#' @param ... One or more [col_cell()] objects to place in `row`.  Their `pos`
#'   may be column names or positions (spanning via `c(a, b)`), resolved against
#'   the final columns; `align` / `border` / `bold` / `italic` / `underline`
#'   are applied to the cell.
#' @param row The header row to edit (1 = top).  Must be an existing row -- add
#'   a new row with [add_header_row()].
#'
#' @return An object of the same shape as `x`.
#'
#' @section Rules:
#' Each target span must align to existing cell boundaries in that row (it
#' cannot split an existing spanning cell -- an error is raised otherwise), and
#' the requested cells must not overlap one another.
#'
#' @seealso [set_col_header()] to set the whole header; [style_header()] to
#'   restyle existing cells; [add_header_row()] to add a row; [col_cell()].
#'
#' @examples
#' df  <- data.frame(Item = "x", g1 = 1, g2 = 2, g3 = 3, Total = 4)
#' tbl <- rtftable(df, col_header = c("Item", "N", "Mean", "SD", "Total"))
#' # Merge g1..g3 under one spanning "Statistics" cell on the top row:
#' tbl <- set_header_cell(tbl, col_cell(c("g1", "g3"), "Statistics"), row = 1)
#' @export
set_header_cell <- function(x, ...) UseMethod("set_header_cell")

#' @rdname set_header_cell
#' @export
set_header_cell.rtftable <- function(x, ..., row) {
  cells <- list(...)
  if (length(cells) == 0L) {
    stop("Provide at least one col_cell() to place.", call. = FALSE)
  }
  if (missing(row) || length(row) != 1L || is.na(suppressWarnings(as.integer(row)))) {
    stop("`row` (a single header-row index, 1 = top) is required.", call. = FALSE)
  }
  hdrs <- x$col_header
  if (is.null(hdrs) || length(hdrs) == 0L) {
    stop("This rtftable has no column header; use set_col_header() first.",
         call. = FALSE)
  }
  row <- as.integer(row)
  if (row < 1L || row > length(hdrs)) {
    stop(sprintf("`row` must be in 1..%d (the header rows, top first). To add a new row use add_header_row().",
                 length(hdrs)), call. = FALSE)
  }

  ref <- .style_ref_df(x)
  cn  <- names(ref)
  nc  <- ncol(ref)

  new_cells <- lapply(cells, .header_cell_from_col_cell, cn = cn, nc = nc)

  r   <- hdrs[[row]]
  cur <- if (is.character(r)) .style_promote_labels_row(r, x$col_spec) else r
  hdrs[[row]] <- .splice_header_cells(cur, new_cells)

  x$col_header <- hdrs
  if (!is.null(x$col_header_list)) x$col_header_list[[1L]] <- hdrs
  x
}

#' @rdname set_header_cell
#' @export
set_header_cell.list <- function(x, ...) {
  .style_map_pages(x, set_header_cell, ..., verb = "set_header_cell")
}


# ── set_decimal_split() ────────────────────────────────────────────────────

#' Line up the decimal points of a numeric column
#'
#' Marks one or more body columns to be rendered as **two adjacent RTF cells**:
#' the part before the first decimal separator (right-aligned) and the
#' separator plus everything after it (left-aligned).  The decimal points then
#' line up exactly, whatever the font, instead of merely right-aligning the
#' last character.
#'
#' This is an **RTF-output option**, not a data transformation.  The table's
#' data frame is never rewritten, the total table width is unchanged (only the
#' column's own width is divided), and the column-header block keeps rendering
#' over the original single column.  The console preview
#' (`print()` / `format()`) therefore does not show the split.
#'
#' @section Which cells are split:
#' A cell is split when it starts with an optional relational or sign prefix
#' (`<`, `>`, `>=`, `<=`, `~`, `+`, `-`) followed by a digit or the separator,
#' and contains at least one digit.  The prefix travels with the left half and
#' any suffix (`%`, a `^{a}` footnote marker, ...) with the right half, so both
#' hang outside the aligned point:
#'
#' \tabular{lll}{
#'   **cell** \tab **left** \tab **right** \cr
#'   `3.45`    \tab `3`   \tab `.45`  \cr
#'   `-0.7`    \tab `-0`  \tab `.7`   \cr
#'   `<0.001`  \tab `<0`  \tab `.001` \cr
#'   `45.6%`   \tab `45`  \tab `.6%`  \cr
#'   `100`     \tab `100` \tab (empty)
#' }
#'
#' Everything else -- free text such as `"n (%)"` or a group label -- is
#' rendered as **one cell across the pair**, exactly as it looks without the
#' option.  Leading and trailing spaces (non-breaking ones included, as left by
#' [fmt_right_align()]) are dropped from split cells, since the split supplies
#' the alignment they were emulating.
#'
#' Compound values such as `"12.3 (4.56)"` (mean (SD)) are excluded by default:
#' the trailing group would dominate the right half and drag the split point
#' far to the left.  Set `include_compound = TRUE` to split them anyway.
#'
#' A selected column in which no cell carries a separator is left untouched.
#' That decision is made per table, so across paginated pages a column may be
#' split on one page and not on another -- a PK visit column that is all `BLQ`
#' in one time band, for instance. Each page stays internally consistent, which
#' is what the geometry needs.
#'
#' @param x An [rtftable()], or a list of them (pages from [as_rtftables()]).
#' @param cols Columns to split: names or positions.  `NULL` clears the
#'   setting.
#' @param ratio `NULL` (the default) to size the two halves automatically from
#'   the widest left and right part actually present, or a number in `(0, 1)`
#'   giving the left half's share of the column width.  Pass an explicit value
#'   to keep the split point identical across pages, whose data differ.
#' @param decimal_mark The separator to split at.  Default `"."`.
#' @param pad_chars Breathing room added to each measured half before the ratio
#'   is taken, in character-width units, integer half first.  Default
#'   `c(0.5, 1)`.
#' @param min_chars Floor for each half, in the same units, applied after
#'   `pad_chars`: the effective width of a half is
#'   `max(measured + pad_chars, min_chars)`.  Default `c(3.5, 6)`, which holds a
#'   `3.5 : 6` baseline -- a three-digit integer part against a five-character
#'   decimal part -- for anything at or below that size, and scales from the
#'   measurement above it.  Without a floor a column of `0.0000` values measures
#'   1 against 5 and leaves the integer half cramped.  Pass
#'   `pad_chars = c(0, 0), min_chars = c(0, 0)` for the raw measured ratio.
#' @param include_compound Split values carrying a whitespace- or
#'   parenthesis-separated companion (`"12.3 (4.56)"`) too.  Default `FALSE`.
#' @param ... Unused.
#'
#' @return An object of the same shape as `x`.
#'
#' @seealso [style_cols()] for ordinary column alignment; [fmt_right_align()]
#'   and [fmt_count_paren()] for the text-padding alternatives.
#'
#' @examples
#' df  <- data.frame(
#'   Statistic = c("Mean", "SD", "p-value"),
#'   Value     = c("12.3", "1.05", "<0.001"),
#'   stringsAsFactors = FALSE
#' )
#' tbl <- rtftable(df) |> set_decimal_split(cols = "Value")
#' @export
set_decimal_split <- function(x, ...) UseMethod("set_decimal_split")

#' @rdname set_decimal_split
#' @export
set_decimal_split.rtftable <- function(x, cols = NULL, ratio = NULL,
                                       decimal_mark = ".",
                                       pad_chars = c(0.5, 1),
                                       min_chars = c(3.5, 6),
                                       include_compound = FALSE, ...) {
  .check_own_dots(list(...), set_decimal_split.rtftable, "set_decimal_split")
  # `cols` not supplied at all is a mistake -- most often a misspelling that
  # landed in `...` (#301).  Clearing the setting stays available, but only
  # through an explicit `cols = NULL`.
  if (missing(cols)) {
    stop("`set_decimal_split()`: `cols` is required. ",
         "To clear a previously set split, pass `cols = NULL` explicitly.",
         call. = FALSE)
  }
  if (is.null(cols)) {
    x$decimal_split <- NULL
    return(x)
  }
  cols_idx <- .style_resolve_cols(x, cols, "set_decimal_split")
  if (!is.character(decimal_mark) || length(decimal_mark) != 1L ||
      is.na(decimal_mark) || !nzchar(decimal_mark)) {
    stop("`decimal_mark` must be a single non-empty string.", call. = FALSE)
  }
  if (!is.null(ratio)) {
    ratio <- suppressWarnings(as.numeric(ratio))
    if (length(ratio) != 1L || is.na(ratio) || ratio <= 0 || ratio >= 1) {
      stop("`ratio` must be a single number strictly between 0 and 1.",
           call. = FALSE)
    }
  }
  x$decimal_split <- list(
    cols             = as.integer(cols_idx),
    ratio            = ratio,
    decimal_mark     = as.character(decimal_mark),
    pad_chars        = .check_char_widths(pad_chars, "pad_chars"),
    min_chars        = .check_char_widths(min_chars, "min_chars"),
    include_compound = isTRUE(include_compound)
  )
  x
}

#' @rdname set_decimal_split
#' @export
set_decimal_split.list <- function(x, ...) {
  # Validate once here, then forward only the recognised arguments, so a typo
  # warns once per call rather than once per page.
  dots <- .check_dots(list(...), .valid_args(set_decimal_split.rtftable),
                      "set_decimal_split")
  do.call(.style_map_pages,
          c(list(x, set_decimal_split), dots,
            list(verb = "set_decimal_split")))
}


# ── rtf_columns() ──────────────────────────────────────────────────────────

#' Column names of a finished table's body
#'
#' Returns the body column names of an [rtftable()] -- the **final, printed**
#' columns, in order.  Use it to see exactly which names (and positions)
#' [set_col_header()], [style_cols()], [style_header()] etc. address, before
#' writing a header.  On a list of pages (an [as_rtftables()] result) it
#' returns the first page's columns (pages share the same column structure).
#'
#' @param x An [rtftable()], or a list of them (pages from [as_rtftables()]).
#' @param ... Unused.
#'
#' @return A character vector of column names.
#'
#' @seealso [set_col_header()].
#'
#' @examples
#' tbl <- rtftable(data.frame(Item = "x", A = 1, B = 2))
#' rtf_columns(tbl)
#' @export
rtf_columns <- function(x, ...) UseMethod("rtf_columns")

#' @rdname rtf_columns
#' @export
rtf_columns.rtftable <- function(x, ...) names(.style_ref_df(x))

#' @rdname rtf_columns
#' @export
rtf_columns.list <- function(x, ...) {
  if (length(x) == 0L) return(character(0))
  if (!inherits(x[[1L]], "rtftable")) {
    stop("`rtf_columns()` on a list expects rtftable pages (as returned by as_rtftables()).",
         call. = FALSE)
  }
  names(.style_ref_df(x[[1L]]))
}

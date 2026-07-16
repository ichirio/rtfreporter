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
#'   `NULL`).
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
  .style_map_pages(x, style_header, ..., verb = "style_header")
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
  .style_map_pages(x, style_cols, ..., verb = "style_cols")
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
                                indent_twips = NULL, color = NULL, ...) {
  frames   <- .style_body_frames(x)
  rows_idx <- .style_resolve_rows(rows, frames, "style_body")
  cols_idx <- .style_resolve_cols(x, cols, "style_body")
  for (arg in c("bold", "italic", "underline")) {
    v <- get(arg)
    if (!is.null(v)) assign(arg, .style_check_flag(v, arg, "style_body"))
  }
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
  .style_map_pages(x, style_body, rows = rows, ..., verb = "style_body")
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
  .style_map_pages(x, style_zone, ..., verb = "style_zone")
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
  .position <- match.arg(.position)
  ncol_df   <- length(x$col_spec)
  new_row   <- .normalize_col_header_rows(list(row), ncol_df)[[1L]]

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
  .style_map_pages(x, add_header_row, ..., verb = "add_header_row")
}

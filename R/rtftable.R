# rtftable: standalone table object for use with rtfreport.
#
# Holds one or more data.frames plus all table-level formatting metadata.
# Pass an rtftable to rtf_tables() instead of a bare data.frame for richer
# control.

# A token argument (`markup`, `blank_row_normalize`) is a character vector of
# tokens.  Another type (a logical NA, a number) and an NA or "" element are
# errors (#846: they used to be dropped, so `markup = NA` was read as "none").
.check_token_vector <- function(x, arg, allowed) {
  if (!is.character(x) || anyNA(x) || !all(nzchar(x))) {
    got <- if (is.character(x)) {
      paste(ifelse(is.na(x), "NA", sprintf("\"%s\"", x)), collapse = ", ")
    } else {
      sprintf("a %s", class(x)[1L])
    }
    stop(sprintf("`%s` must be a subset of %s; got: %s", arg, allowed, got),
         call. = FALSE)
  }
  invisible(x)
}

# Resolve the `blank_row_normalize` argument to a clean character vector of the
# enabled tokens (a subset of c("detect", "collapse")).  Accepts the token
# vector, `NULL` / `"none"` / `character(0)` (all -> none enabled), and errors on
# any other token.
.resolve_blank_row_normalize <- function(x) {
  if (is.null(x)) return(character(0))
  .check_token_vector(x, "blank_row_normalize",
                      "c(\"detect\", \"collapse\") (or \"none\" / NULL)")
  if (length(x) == 0L || identical(x, "none")) return(character(0))
  allowed <- c("detect", "collapse")
  bad <- setdiff(x, allowed)
  if (length(bad)) {
    stop("`blank_row_normalize` must be a subset of ",
         "c(\"detect\", \"collapse\") (or \"none\" / NULL); got: ",
         paste(bad, collapse = ", "), call. = FALSE)
  }
  unique(x)
}

# Resolve a `markup` argument to the enabled tokens (a subset of
# c("script", "relational")).  Accepts the token vector, `"all"` (both),
# `"none"` / `character(0)` (neither), or `NULL` (returned as-is, meaning
# "inherit" for a per-table override).  Errors on any other token.
#   "script"     -- ^{...} -> \super, _{...} -> \sub
#   "relational" -- ">=" -> U+2265, "<=" -> U+2264
.resolve_markup <- function(x) {
  if (is.null(x)) return(NULL)
  .check_token_vector(x, "markup",
                      "c(\"script\", \"relational\") (or \"all\" / \"none\" / NULL)")
  if (length(x) == 0L || identical(x, "none")) return(character(0))
  if (identical(x, "all")) return(c("script", "relational"))
  allowed <- c("script", "relational")
  bad <- setdiff(x, allowed)
  if (length(bad)) {
    stop("`markup` must be a subset of c(\"script\", \"relational\") ",
         "(or \"all\" / \"none\" / NULL); got: ", paste(bad, collapse = ", "),
         call. = FALSE)
  }
  unique(x)
}

# Normalize a border specification to an rtf_table_border (or NULL).
# Accepts:
#   rtf_table_border          -> returned as-is
#   rtf_table_style (S3)      -> .style_to_table_border(style)
#   "tfl"                     -> the clinical TFL preset
#   "none" / NULL             -> NULL (no borders)
#   plain named list          -> .plain_list_to_table_border()
# anything else errors.
.normalize_table_border <- function(border) {
  if (is.null(border)) return(NULL)
  if (inherits(border, "rtf_table_border")) return(border)
  if (inherits(border, "rtf_table_style"))  return(.style_to_table_border(border))
  if (identical(border, "tfl")) return(.rtf_border_tfl())
  if (identical(border, "none")) return(NULL)   # "none" == no borders
  # A cell-level rtf_border() is a classed *named list*, so it would otherwise
  # reach the plain-list fallback below, which looks for zone names, finds none
  # and returns an empty table border -- every rule silently gone (#326).  The
  # opposite mix-up is already an error in col_cell(); this makes the pair
  # symmetric.
  # An rtf_border here selects the whole table: its four edges are the table's
  # outer frame and its inside_h / inside_v are the rules between rows and
  # between cells.  (Before 0.5.0 this was an error -- see #326 -- so nothing
  # that worked before changes meaning.)
  if (inherits(border, "rtf_border")) {
    return(.rtf_table_border(outer    = border,
                             inside_h = border$inside_h,
                             inside_v = border$inside_v))
  }
  if (is.list(border)) return(.plain_list_to_table_border(border))
  stop("`border` must be \"tfl\", \"none\", NULL, an rtf_table_border object, ",
       "an rtf_table_style object, or a named list.", call. = FALSE)
}

# Resolve a `row_title` argument (which columns are row-heading columns) into a
# sorted integer vector of valid column indices.
#
#   NULL           -> column 1 only (the default: first column is the row title)
#   integer vector -> those column indices
#   character      -> matched against `col_names`
#
# Out-of-range indices / unknown names raise an error.
.normalize_row_title <- function(row_title, ncol_df, col_names = NULL) {
  if (ncol_df < 1L) return(integer(0))
  if (is.null(row_title)) return(1L)
  if (is.character(row_title)) {
    idx <- match(row_title, col_names)
    if (anyNA(idx)) {
      stop(sprintf("`row_title` column(s) not found in data: %s",
                   paste(row_title[is.na(idx)], collapse = ", ")), call. = FALSE)
    }
  } else if (is.numeric(row_title)) {
    idx <- as.integer(row_title)
    if (anyNA(idx) || any(idx < 1L) || any(idx > ncol_df)) {
      stop(sprintf("`row_title` indices must be within 1..%d.", ncol_df),
           call. = FALSE)
    }
  } else {
    stop("`row_title` must be NULL, an integer vector, or column names.",
         call. = FALSE)
  }
  sort(unique(idx))
}

# Normalize col_spec: list of per-column lists -> internal indexed form.
#
# Each output element:
#   list(align, bold, italic, underline, indent_twips,
#        header_bold, header_align, header_italic)
#
# Default data alignment depends on `row_title` (the resolved integer vector of
# row-heading columns): row-title columns default to "left", all other columns
# default to "center".  An `rtf_table_style`'s `align`, when supplied, still
# governs (backward compatible); explicit col_spec align always wins.
#
# Header-align resolution precedence (highest first):
#   1. col_spec entry's header_align (per-column override from user)
#   2. col_header_align argument     (table-wide, scalar or length-ncol)
#   3. col_spec entry's align        (inherit data alignment)
#
# col_header_align: NULL | character(1) | character(ncol).
# row_title: resolved integer vector (see .normalize_row_title); NULL -> col 1.
#
.normalize_col_spec <- function(col_spec, ncol_df, col_names,
                                 col_header_align = NULL,
                                 style = NULL,
                                 row_title = NULL) {
  # Per-column defaults (potentially seeded from `style`).  `header_align`
  # is left NULL so the cascade can fill it in further down.
  # Data alignment default: a style's align wins if supplied; otherwise
  # row-title columns are left-aligned and the rest are centred.
  rt_idx             <- if (is.null(row_title)) 1L else as.integer(row_title)
  style_align        <- if (!is.null(style)) style$align else NULL
  base_bold          <- if (!is.null(style)) style$bold          else FALSE
  base_italic        <- if (!is.null(style)) style$italic        else FALSE
  base_underline     <- if (!is.null(style)) style$underline     else FALSE
  base_header_bold   <- if (!is.null(style)) style$header_bold   else FALSE
  base_header_italic <- if (!is.null(style)) style$header_italic else FALSE

  result <- lapply(seq_len(ncol_df), function(j) {
    list(
      align         = style_align %||% (if (j %in% rt_idx) "left" else "center"),
      bold          = base_bold,
      italic        = base_italic,
      underline     = base_underline,
      indent_twips  = 0L,
      header_bold   = base_header_bold,
      header_align  = NULL,            # resolved later (cascade)
      header_italic = base_header_italic,
      border        = NULL             # per-column border override (rtf_border)
    )
  })

  if (!is.null(col_spec)) {
    if (!is.list(col_spec)) stop("`col_spec` must be a list.", call. = FALSE)
    for (spec in col_spec) {
      if (!is.list(spec) || is.null(spec$col)) {
        stop("Each element of `col_spec` must be a list with a `col` key.",
             call. = FALSE)
      }
      col_ref <- spec$col
      if (is.character(col_ref)) {
        idx <- match(col_ref, col_names)
        if (is.na(idx)) stop(sprintf("col_spec col '%s' not found in data.",
                                       col_ref), call. = FALSE)
      } else {
        idx <- as.integer(col_ref)
        if (is.na(idx) || idx < 1L || idx > ncol_df) {
          stop(sprintf("col_spec col index %d out of range (1..%d).",
                       idx, ncol_df), call. = FALSE)
        }
      }
      for (key in setdiff(names(spec), "col")) {
        result[[idx]][[key]] <- spec[[key]]
      }
      # Validate per-column border (must be rtf_border S3 or NULL).
      if (!is.null(result[[idx]]$border) &&
          !inherits(result[[idx]]$border, "rtf_border")) {
        stop(sprintf("col_spec col %d border must be NULL or an rtf_border() object.",
                     idx), call. = FALSE)
      }
    }
  }

  # Expand col_header_align to a per-column vector (or NULL).
  cha <- col_header_align
  if (!is.null(cha)) {
    if (length(cha) == 1L) {
      cha <- rep(as.character(cha), ncol_df)
    } else if (length(cha) != ncol_df) {
      stop(sprintf("`col_header_align` must have length 1 or %d.", ncol_df),
           call. = FALSE)
    } else {
      cha <- as.character(cha)
    }
    if (!all(cha %in% c("left", "center", "right"))) {
      stop("`col_header_align` values must be \"left\", \"center\", or \"right\".",
           call. = FALSE)
    }
  }

  # Cascade fill: header_align <- col_header_align[j] <- align
  for (j in seq_len(ncol_df)) {
    if (is.null(result[[j]]$header_align)) {
      result[[j]]$header_align <- if (!is.null(cha)) cha[[j]] else result[[j]]$align
    }
  }

  result
}

# Normalize the col_header argument into a list whose elements are either:
#   * character vector -- a regular label row (one entry per data column)
#   * list of list(from, to, label, ...) -- a spanning row (renderer's form)
#
# Accepted inputs (in order of detection):
#   NULL                                        -> NULL (renderer uses names(df))
#   character(n)                                -> list of one label row
#   "A | B | C"   (single string)               -> split on '|', single row
#   rtf_col_header object                       -> unclass; list of rows
#   list, top-level all cell specs              -> wrap as a single row
#   list of mixed rows (label / spanning / pos) -> multi-row
#
# `ncol_df` is required when any row uses the new pos-style cell spec
# (it is needed to validate ranges and to fill gaps with empty cells).
# `col_names` (the data column names) enables the name-aware resolution of
# named label rows and of column-name `pos` in col_cell() cells.
.normalize_col_header_rows <- function(col_header, ncol_df = NULL,
                                       col_names = NULL) {
  if (is.null(col_header)) return(NULL)

  # rtf_col_header object -> list of rows (still tagged by class for outer
  # callers; here we strip it so the per-row loop below treats it as data).
  if (inherits(col_header, "rtf_col_header")) {
    col_header <- unclass(col_header)
  }

  # Pipe-delimited string shorthand.
  if (is.character(col_header) && length(col_header) == 1L &&
      grepl("|", col_header, fixed = TRUE)) {
    col_header <- trimws(strsplit(col_header, "|", fixed = TRUE)[[1L]])
  }
  if (is.character(col_header)) {
    .check_label_row_length(col_header, 1L, ncol_df, col_names)
    return(list(.resolve_named_label_row(col_header, col_names)))
  }
  if (!is.list(col_header)) {
    stop("`col_header` must be NULL, character, or a list.", call. = FALSE)
  }

  # Auto-detect: a bare list of cell specs at the top level means a single
  # row of cells, not multiple rows.  Wrap it so the row loop sees one row.
  if (length(col_header) > 0L &&
      all(vapply(col_header, .is_cell_spec, logical(1L)))) {
    col_header <- list(col_header)
  }

  lapply(seq_along(col_header), function(i) {
    row <- col_header[[i]]
    if (is.character(row)) {
      .check_label_row_length(row, i, ncol_df, col_names)
      return(.resolve_named_label_row(row, col_names))
    }
    if (!is.list(row) || length(row) == 0L) {
      stop("Each col_header row must be a character vector or a non-empty ",
           "list of cell specs.", call. = FALSE)
    }
    if (.is_cell_spec(row[[1L]])) {
      if (!is.null(row[[1L]]$pos)) {
        if (is.null(ncol_df)) {
          stop("Internal: ncol_df required to normalize pos-style rows.",
               call. = FALSE)
        }
        return(.pos_row_to_spans(row, ncol_df, col_names))
      }
      if (!is.null(row[[1L]]$from)) {
        return(row)   # legacy spanning row, leave as-is
      }
    }
    stop("Each col_header element must be a character vector, a list of ",
         "col_cell()/pos-spec cells, or a list of spanning specs ",
         "(list(from, to, label, ...)).", call. = FALSE)
  })
}

# A positional label row (no names) gives one label per data column.  One
# too few used to leave the last column's header empty, one too many was
# dropped, both without a word (#594 E4).  The rule is the one
# .check_col_header_width() applies to set_col_header(): a named row is
# matched by name (and checked there), and a single label is allowed.
.check_label_row_length <- function(row, i, ncol_df, col_names) {
  if (is.null(ncol_df)) return(invisible(NULL))
  nms <- names(row)
  if (!is.null(nms) && any(nzchar(nms))) return(invisible(NULL))
  if (length(row) > 1L && length(row) != ncol_df) {
    cols <- if (length(col_names)) {
      sprintf(" (%s)", paste(col_names, collapse = ", "))
    } else ""
    stop(sprintf(paste0("`col_header` row %d has %d label%s but the table ",
                        "has %d column%s%s.  Give one label per column, or ",
                        "name the ones to change: c(%s = \"...\")."),
                 i, length(row), if (length(row) == 1L) "" else "s",
                 ncol_df, if (ncol_df == 1L) "" else "s", cols,
                 if (length(col_names)) col_names[1L] else "col"),
         call. = FALSE)
  }
  invisible(NULL)
}

# Predicate: is `x` a canonical "header rows list" -- every element being
# either a character vector (label row) or a spanning / pos-cell row
# (list whose first element has $from or $pos)?
.is_header_rows_list <- function(x) {
  if (!is.list(x) || length(x) == 0L) return(FALSE)
  all(vapply(x, function(row) {
    is.character(row) ||
      (is.list(row) && length(row) > 0L &&
         is.list(row[[1L]]) &&
         (!is.null(row[[1L]]$from) || !is.null(row[[1L]]$pos)))
  }, logical(1L)))
}

# Normalize col_header for multi-DF mode.
# Returns a list of length n_dfs, each element is a normalized col_header
# (NULL or a list whose elements are label rows / spanning rows).
# `ncol_df` is required when any row uses pos-style cells.
.normalize_multi_col_header <- function(col_header, n_dfs, ncol_df = NULL,
                                        col_names = NULL) {
  if (is.null(col_header)) return(rep(list(NULL), n_dfs))

  # rtf_col_header instance is treated as a shared header for all DFs.
  if (inherits(col_header, "rtf_col_header")) {
    h <- .normalize_col_header_rows(col_header, ncol_df, col_names)
    return(rep(list(h), n_dfs))
  }

  # Plain character -> shared single label row for all DFs.
  if (is.character(col_header)) {
    h <- .normalize_col_header_rows(col_header, ncol_df, col_names)
    return(rep(list(h), n_dfs))
  }

  if (!is.list(col_header)) {
    stop("`col_header` must be NULL, a character vector, or a list.",
         call. = FALSE)
  }

  # A bare list of cell specs is a single row -- same handling as the
  # single-DF normalizer.
  if (length(col_header) > 0L &&
      all(vapply(col_header, .is_cell_spec, logical(1L)))) {
    h <- .normalize_col_header_rows(col_header, ncol_df, col_names)
    return(rep(list(h), n_dfs))
  }

  .is_per_df_spec <- function(x) {
    is.null(x) || inherits(x, "rtf_col_header") ||
      is.character(x) || .is_header_rows_list(x)
  }

  if (length(col_header) == n_dfs &&
      all(vapply(col_header, .is_per_df_spec, logical(1L)))) {
    return(lapply(col_header,
                   function(h) .normalize_col_header_rows(h, ncol_df, col_names)))
  }

  # Otherwise treat as shared multi-row header.
  if (!.is_header_rows_list(col_header)) {
    stop("Multi-row col_header must contain only character vectors and ",
         "cell-spec rows (col_cell()/pos-spec, or list(from, to, ...)).",
         call. = FALSE)
  }
  h <- .normalize_col_header_rows(col_header, ncol_df, col_names)
  rep(list(h), n_dfs)
}


# `table_width_pct` as the fraction of the writable width it stands for.
#
# The coercion is silenced rather than left to warn: a non-number is an error
# with a message that says what the setting takes, and R's "NAs introduced by
# coercion" arriving FIRST tells the user nothing and, under
# `options(warn = 2)`, replaces that message with itself (#388).  A string
# that is a number ("50") is still accepted, as it always was.
.table_width_pct_frac <- function(x, arg = "table_width_pct") {
  pct <- suppressWarnings(as.numeric(x))
  if (length(pct) != 1L || is.na(pct) || pct <= 0 || pct > 100) {
    stop(sprintf("`%s` must be a number in (0, 100].", arg), call. = FALSE)
  }
  pct / 100
}

#' Create an RTF table object
#'
#' Constructs a table object with full formatting control.
#' The result can be passed directly to `rtf_tables()` in a pipe chain.
#'
#' @param data A `data.frame`, or a `list` of `data.frame`s (multi-DF mode).
#'   Multi-DF mode renders each data.frame with its own column headers but
#'   shares column widths and border settings.
#' @param col_header The column header. One of:
#'   \describe{
#'     \item{`NULL`}{(default) use the column names of `data`.}
#'     \item{a character vector}{one label per column -- a single header row.
#'       By default labels are placed **by position**.  If the vector is
#'       **named**, placement becomes name-aware and order-independent (a
#'       safeguard against column reordering): a named element
#'       (`g1 = "G1"`) labels the column named `g1` and errors on an unknown
#'       or duplicate name; an unnamed element is matched as a column name if
#'       one exists, otherwise placed at its position; two elements resolving
#'       to the same column is an error.  A fully-unnamed vector keeps the
#'       exact legacy positional behaviour.}
#'     \item{a list of rows}{each row is either a character vector (a label row)
#'       or a spanning row (`list(list(from, to, label, underline), ...)`),
#'       rendered top to bottom.}
#'     \item{a list of per-DF specs}{in multi-DF mode, one of the above per
#'       `data.frame` (same length as `data`).}
#'   }
#'   Spanning cells built with [col_cell()] may also address columns by name
#'   (e.g. `col_cell(c("g1", "g3"), "Drug A")`).
#' @param col_header_align Column-header text alignment, applied across
#'   header rows. `NULL` (default) inherits each column's `align` value
#'   from `col_spec` (i.e. column headers follow the data alignment).
#'   `"center"` / `"left"` / `"right"` applies a single value to every
#'   column; a character vector of length `ncol` overrides per-column.
#' @param col_spec Per-column formatting, as a `list` of per-column specs. Each
#'   spec is a named list identifying its column with `col`, plus any of:
#'   \describe{
#'     \item{`col`}{the integer column index (or column name) the spec targets.}
#'     \item{`align`}{data alignment `"left"` / `"center"` / `"right"` (overrides
#'       the `row_title`-derived default below).}
#'     \item{`bold`, `italic`, `underline`}{logical text decorations.}
#'     \item{`indent_twips`}{integer left indent of the cell text.}
#'     \item{`color`}{a `"#RRGGBB"` hex string -- the column's **text colour**
#'       (added to the document colour table automatically).}
#'     \item{`header_align`, `header_bold`, `header_italic`}{the same, applied to
#'       this column's **header** cell.}
#'   }
#'   e.g. `list(list(col = 1, align = "left"), list(col = 2, bold = TRUE))`.
#' @param row_title Which columns are **row-heading** columns.  `NULL`
#'   (default) means the first column only; otherwise an integer vector of
#'   column indices or a character vector of column names (e.g.
#'   `row_title = c(1, 2)`).
#'   \describe{
#'     \item{Default alignment}{This sets the per-column default data
#'       alignment: row-heading columns default to `"left"` and every other
#'       column defaults to `"center"`.}
#'     \item{What overrides it}{Explicit `col_spec` alignment, an
#'       `rtf_table_style`, or alignment read from a gt/rtables source all
#'       still override this default; column headers follow the data
#'       alignment via the usual cascade.}
#'   }
#' @param border The table borders. One of:
#'   \describe{
#'     \item{`"tfl"`}{(default) the clinical TFL preset: a top and a bottom
#'       rule on the column-header block only (see [rtf_table_style_tfl()]);
#'       no rule under the last row.}
#'     \item{`"none"`}{no borders, the underline under a spanning header cell
#'       included (add one per cell with [col_cell()]`(border = )` or
#'       [style_header()]).}
#'     \item{an [rtf_border()] object}{its outer edges frame the whole table
#'       and its `inside_h` / `inside_v` rules fall between the rows / cells;
#'       [style_zone()] then sets one kind of row.  The blank rows of
#'       `blank_rows` are inside the frame: a blank row carries the left /
#'       right edges, `inside_v` and `inside_h`, and a leading (no header
#'       row) or trailing one the top or bottom edge.}
#'     \item{an [rtf_table_style()] object}{its border zones are used.}
#'   }
#' @param style Optional shared `rtf_table_style` (S3).  Provides default
#'   values for borders, alignment, cell padding, etc.; explicit arguments
#'   to `rtftable()` always override.  Snapshot semantics: each
#'   `rtftable()` call captures the style's current state at construction.
#' @param blank_rows Where to insert blank separator rows. One of -- or a `list`
#'   combining any of (positions are unioned):
#'   \describe{
#'     \item{an integer vector}{positions: `0` = before the first row, `k` =
#'       after data row `k`, `-1` = after the last row.  A position is a
#'       whole number in `-1..nrow`: a fraction, `NA`, or a position past
#'       the last row (or below `-1`) stops, naming the positions.}
#'     \item{a [blank_rows_by_change()] spec}{insert when a column value changes.}
#'     \item{a [blank_rows_by_rule()] spec}{insert before / after rows matching a
#'       regular expression.}
#'   }
#' @param read_attributes `TRUE` or `FALSE`. When `TRUE` (default), read recognised
#'   attributes off `data` for use as fallback defaults -- currently
#'   `attr(data, "rtf_blank_rows")` is folded into `blank_rows` when the
#'   argument is `NULL`. Set `FALSE` to ignore attributes.
#' @param col_rel_width Numeric vector of relative column widths (e.g.
#'   `c(2, 1, 1)` makes the first column twice as wide as the others), one
#'   positive finite number per column; another value is an error when the
#'   file is written.
#' @param column_widths_twips Integer vector of absolute column widths in
#'   twips, one positive whole number per column. Overrides `col_rel_width`.
#' @param table_width_twips Total table width in twips.  A value that is not
#'   one non-negative integer is an error.
#' @param table_width_pct_of_writable Table width as a fraction 0-1 of the
#'   writable page width.  A value that is not one number in (0, 1] is an
#'   error.
#' @param table_width_pct Table width as a percentage 0-100 of the writable
#'   page width (convenience alias for `table_width_pct_of_writable * 100`).
#'   A value that is not one number in (0, 100] is an error.
#' @param table_align Horizontal placement: `"left"` (default), `"center"`,
#'   or `"right"`.
#' @param row_height_twips Row height for data rows in twips. `NULL` (default)
#'   takes the `style`'s, else the document's ([rtf_style()]), else the
#'   `rtfreporter.row_height_twips` option, else the height the font size
#'   implies. A non-negative integer specifies an explicit value; anything
#'   else is an error, as it is for the other `*_twips` arguments.
#' @param row_height_exact `TRUE` = exact (clipped); `FALSE` = minimum; `NA`
#'   or another value is an error.
#' @param header_row_height_twips Row height for column-header rows.
#' @param font_size_half_points Font size for this table, in half-points.
#'   `NULL` (default) inherits the document size. Setting it also recomputes
#'   the row height from that size unless `row_height_twips` is given.
#' @param font Font family for this table, e.g. `"Arial"`. `NULL` (default)
#'   uses the document font. A family not already in the document's
#'   `font_table` is added to it automatically, the way a colour is.
#'   A named vector gives one family per script slot (`latin`,
#'   `east_asian`, `complex`, optional `hansi`), e.g. `c(latin =
#'   "Times New Roman", east_asian = "MS Mincho")`; see [rtf_document()].
#' @param blank_row_height_twips Row height for blank separator rows.
#' @param cell_padding_left_twips Left cell padding in twips.  `NULL`
#'   (default) takes the `style`'s, else the document's ([rtf_style()]), else
#'   the `rtfreporter.cell_padding_left_twips` option, else 0 (cell content
#'   sits flush against the cell border).
#' @param cell_padding_right_twips Right cell padding in twips, resolved as
#'   `cell_padding_left_twips` is.
#' @param cell_padding_top_twips,cell_padding_bottom_twips Cell padding above /
#'   below the text in twips, written into every header and data cell's
#'   definition (blank separator rows excepted).  `NULL` (default) takes the
#'   style's, else the document's ([rtf_style()]), else 0; 0
#'   writes nothing.
#' @param cell_valign Vertical alignment: `"bottom"` (default), `"top"`,
#'   or `"center"`.
#' @param cell_styles `NULL` (default), or a list of length `nrow(data)`.
#'   Each element is either `NULL` (no per-cell override for that row) or a
#'   named list with optional vectors of length `ncol(data)`:
#'   \describe{
#'     \item{`bold`}{logical -- overrides `col_spec[[j]]$bold` when non-`NA`.}
#'     \item{`italic`}{logical -- overrides `col_spec[[j]]$italic`.}
#'     \item{`underline`}{logical -- overrides `col_spec[[j]]$underline`.}
#'     \item{`indent_twips`}{integer -- overrides `col_spec[[j]]$indent_twips`
#'       (replaces, does not add to, the column default).}
#'     \item{`color`}{character `"#RRGGBB"` -- per-cell **text colour**,
#'       overriding `col_spec[[j]]$color`. `NA` means "use the column colour".}
#'     \item{`align`}{character `"left"` / `"center"` / `"right"` -- per-cell
#'       alignment, overriding `col_spec[[j]]$align`. `NA` means "use the
#'       column alignment".}
#'     \item{`border`}{a **list** of [rtf_border()] / `NULL` per column --
#'       per-cell border, merged side-by-side on top of the row's resolved
#'       zone border (`body` / `first_row` / `last_row`); an
#'       side of style `"none"` erases a zone rule.}
#'   }
#'   `NA` entries within a vector mean "no override; use the column default".
#'   This argument is populated automatically by [as_rtftable()] when reading
#'   from a `gt_tbl` or gtsummary table with `read = TRUE`.
#' @param blank_row_normalize How blank rows are normalised at render time. A
#'   character vector of zero or more of:
#'   \describe{
#'     \item{`"detect"`}{a **data** row whose every cell is `NA` / `""` (empty
#'       or ASCII-whitespace only) is treated as a blank row and rendered as a
#'       single full-width cell, like an explicit blank separator row, instead
#'       of one empty cell per column.}
#'     \item{`"collapse"`}{a run of two or more consecutive blank rows (separator
#'       rows and/or `"detect"`-detected empty data rows) is reduced to a single
#'       blank row.}
#'   }
#'   Default `c("detect", "collapse")` (both on). Pass `"none"`, `NULL`, or
#'   `character(0)` to disable. Both behaviours act per rendered table, so for a
#'   paginated table they apply per page (i.e. after the split).
#' @param markup Which cell-text markup is applied at render time, as a
#'   character vector of zero or more of:
#'   \describe{
#'     \item{`"script"`}{`^{...}` renders as superscript (`\\super`) and
#'       `_{...}` as subscript (`\\sub`).}
#'     \item{`"relational"`}{`">="` is converted to `U+2265` and `"<="` to
#'       `U+2264`.}
#'   }
#'   `"all"` enables both; `"none"` / `character(0)` enables neither. `NULL`
#'   (default) **inherits** the document default (`rtf_document(style =
#'   rtf_style(markup = ))` / the `rtfreporter.markup` option), which is `"script"` --
#'   so super/subscript (e.g. adapter footnote marks `^{N}`) work while the
#'   `>=` / `<=` symbol conversion is **opt-in**. Applies to all cell text: data
#'   cells, column / spanning headers, and title / footnote blocks.
#'   The braces after `^` / `_` are the markup's own, so a token written
#'   there is not filled: `^{STUDY}` raises the word STUDY.  To raise a
#'   token's value, put the token's braces inside, `^{{STUDY}}`, or write
#'   other text with it, `^{n={STUDY}}`.
#'
#' @return An `rtftable` (S3) object suitable for use in `rtf_tables()`.
#'
#' @examples
#' # 1. Simplest: a data.frame straight to a table (column names become the
#' #    header).
#' df <- data.frame(Subject = c("001", "002"), Age = c(34L, 45L))
#' tbl <- rtftable(df)
#'
#' # 2. A clinical-style table: a wide left-aligned row-label column, a spanning
#' #    "Treatment" header over the two arms, and the TFL border preset.
#' dm <- data.frame(
#'   Parameter = c("Age (years)", "  Mean (SD)", "  Median"),
#'   Placebo   = c("", "75.1 (8.2)", "76.0"),
#'   Active    = c("", "74.3 (7.9)", "75.0"),
#'   stringsAsFactors = FALSE
#' )
#' tbl <- rtftable(
#'   dm,
#'   col_header = list(
#'     list(list(from = 2, to = 3, label = "Treatment", underline = TRUE)),
#'     c("Parameter", "Placebo", "Active")
#'   ),
#'   col_spec      = list(list(col = 1, align = "left")),
#'   col_rel_width = c(2, 1, 1),
#'   border        = "tfl"
#' )
#'
#' doc <- rtf_document() |>
#'   rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
#'   rtf_tables(tbl, titles = list("Table 14.1.1"))
#' f <- tempfile(fileext = ".rtf")
#' generate_rtfreport(doc, f, overwrite = TRUE)
#' unlink(f)
#'
#' @export
rtftable <- function(
  data,
  col_header = NULL,
  col_header_align = NULL,
  col_spec = NULL,
  row_title = NULL,
  border = "tfl",
  blank_rows = NULL,
  read_attributes = TRUE,
  style = NULL,
  col_rel_width = NULL,
  column_widths_twips = NULL,
  table_width_twips = NULL,
  table_width_pct_of_writable = NULL,
  table_width_pct = NULL,
  table_align = "left",
  row_height_twips = NULL,
  font_size_half_points = NULL,
  font = NULL,
  row_height_exact = FALSE,
  header_row_height_twips = NULL,
  blank_row_height_twips = NULL,
  cell_padding_left_twips = NULL,
  cell_padding_right_twips = NULL,
  cell_padding_top_twips = NULL,
  cell_padding_bottom_twips = NULL,
  cell_valign = "bottom",
  cell_styles = NULL,
  blank_row_normalize = c("detect", "collapse"),
  markup = NULL
) {
  # The whole-table settings whose default is a value, not NULL: an
  # rtf_style() table section (#804) may set the ones left at it.
  style_defaulted <- c("table_align", "cell_valign", "row_height_exact")[
    c(missing(table_align), missing(cell_valign), missing(row_height_exact))]
  # -- Resolve defaults from a shared style template, when supplied ---
  # The style provides defaults; explicit arguments always override.
  if (!is.null(style)) {
    if (!inherits(style, "rtf_table_style")) {
      stop("`style` must be an rtf_table_style object.", call. = FALSE)
    }
    # `border` is "tfl" by default -- only adopt the style's borders
    # when the caller has not passed an rtf_table_border or another
    # explicit object.
    if (identical(border, "tfl")) border <- style
    if (is.null(col_header_align)) col_header_align <- style$header_align
    if (is.null(row_height_twips)) row_height_twips <- style$row_height_twips
    # A style seeds padding only when the caller left it unset (NULL), so an
    # explicit per-table padding always wins over the style.
    if (is.null(cell_padding_left_twips))
      cell_padding_left_twips <- style$cell_padding_left_twips
    if (is.null(cell_padding_right_twips))
      cell_padding_right_twips <- style$cell_padding_right_twips
    if (is.null(cell_padding_top_twips))
      cell_padding_top_twips <- style$cell_padding_top_twips
    if (is.null(cell_padding_bottom_twips))
      cell_padding_bottom_twips <- style$cell_padding_bottom_twips
  }


  data_single  <- NULL
  data_list    <- NULL
  col_hdr_one  <- NULL
  col_hdr_list <- NULL

  if (is.data.frame(data)) {
    # -- Single data.frame mode --------------------------------------------
    data_single <- data

    # Normalize col_header to a list whose elements are either:
    #   - character vector  (regular label row)
    #   - list of list(from, to, label, underline)  (spanning row)
    col_hdr_one <- .normalize_col_header_rows(col_header, ncol(data),
                                              names(data))

    row_title_idx <- .normalize_row_title(row_title, ncol(data), names(data))
    cs <- .normalize_col_spec(
      col_spec, ncol(data), names(data),
      col_header_align = col_header_align,
      style            = style,
      row_title        = row_title_idx)

    # Read recognised attributes off the data.frame as fallback defaults.
    if (isTRUE(read_attributes)) {
      attr_data <- .read_data_attributes(data)
      if (is.null(blank_rows) && !is.null(attr_data$blank_rows)) {
        blank_rows <- attr_data$blank_rows
      }
      # A spanned label row is carried as a per-row cell style, so it rides
      # the machinery that already slices styles per page and re-indexes them
      # through paginate_cols() -- no separate row-index bookkeeping (#312).
      if (!is.null(attr_data$label_rows)) {
        if (is.null(cell_styles)) {
          cell_styles <- vector("list", nrow(data))
        }
        for (r in attr_data$label_rows) {
          if (r >= 1L && r <= nrow(data)) {
            row_cs <- cell_styles[[r]]
            if (is.null(row_cs)) row_cs <- list()
            row_cs$span_row <- TRUE
            cell_styles[[r]] <- row_cs
          }
        }
      }
    }

  } else if (is.list(data) && length(data) > 0L) {
    # -- Multi data.frame mode ---------------------------------------------
    for (i in seq_along(data)) {
      if (!is.data.frame(data[[i]])) {
        stop(sprintf("`data[[%d]]` must be a data.frame.", i), call. = FALSE)
      }
    }
    ncols_all <- vapply(data, ncol, integer(1L))
    if (length(unique(ncols_all)) > 1L) {
      stop("All data.frames in `data` must have the same number of columns.",
           call. = FALSE)
    }
    data_list    <- data
    col_hdr_list <- .normalize_multi_col_header(col_header, length(data),
                                                  ncol_df = ncols_all[1L],
                                                  col_names = names(data[[1L]]))

    ref_ncol  <- ncols_all[1L]
    ref_names <- names(data[[1L]])
    row_title_idx <- .normalize_row_title(row_title, ref_ncol, ref_names)
    cs <- .normalize_col_spec(
      col_spec, ref_ncol, ref_names,
      col_header_align = col_header_align,
      style            = style,
      row_title        = row_title_idx)

  } else {
    stop("`data` must be a data.frame or a non-empty list of data.frames.",
         call. = FALSE)
  }

  # Border: normalize to rtf_table_border (or NULL for no borders), then expand
  # the whole-table shortcuts (outer / inside_h / inside_v) into the five zones.
  # Whether a column header exists decides where the table's top edge lands,
  # which is why the expansion happens here and not in the constructor.
  # col_header = NULL still writes a header row (the column names), so the
  # top edge is that row's; only a header of no rows (list()) has none.  It
  # used to read NULL as "no header": the top rule fell between the column
  # names and the first data row, and the inside_h seam under them was lost.
  first_hdr <- if (!is.null(data_single)) col_hdr_one else col_hdr_list[[1L]]
  border_resolved <- .expand_table_border(
    .normalize_table_border(border),
    has_header = is.null(first_hdr) || length(first_hdr) > 0L)

  # Resolve blank_rows spec into a sorted integer vector of positions.
  blank_rows_resolved <- NULL
  if (!is.null(blank_rows)) {
    if (is.data.frame(data)) {
      blank_rows_resolved <- .resolve_blank_rows(blank_rows, data)
    } else {
      # Multi-DF: only integer positions supported here (applied per-DF).
      if (!is.numeric(blank_rows) || is.list(blank_rows)) {
        stop("In multi-DF mode, `blank_rows` must be an integer vector ",
             "of positions; by_change / by_rule specs are not supported.",
             call. = FALSE)
      }
      v <- as.integer(blank_rows)
      if (any(is.na(v)) || any(v < -1L) ||
          any(blank_rows != trunc(blank_rows))) {
        stop("`blank_rows` integers must be -1, 0, or positive.", call. = FALSE)
      }
      blank_rows_resolved <- sort(unique(v))
    }
  }

  # table width %: a fraction in (0, 1]; 2 or "x" used to be kept as given
  twpw <- table_width_pct_of_writable
  if (!is.null(twpw)) {
    twpw <- suppressWarnings(as.numeric(twpw))
    if (length(twpw) != 1L || is.na(twpw) || twpw <= 0 || twpw > 1) {
      stop("`table_width_pct_of_writable` must be a number in (0, 1].",
           call. = FALSE)
    }
  }
  if (!is.null(table_width_pct)) {
    twpw <- .table_width_pct_frac(table_width_pct)
  }

  # one value: a vector of two, or none, used to stop with "the condition has
  # length > 1" / "argument is of length zero"; NA was taken for FALSE
  if (length(table_align) != 1L ||
      !table_align %in% c("left", "center", "right")) {
    stop("`table_align` must be 'left', 'center', or 'right'.", call. = FALSE)
  }
  if (!is.logical(row_height_exact) || length(row_height_exact) != 1L ||
      is.na(row_height_exact)) {
    stop("`row_height_exact` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(read_attributes) || length(read_attributes) != 1L ||
      is.na(read_attributes)) {
    stop("`read_attributes` must be TRUE or FALSE.", call. = FALSE)
  }
  if (length(cell_valign) != 1L ||
      !cell_valign %in% c("top", "center", "bottom")) {
    stop("`cell_valign` must be 'top', 'center', or 'bottom'.", call. = FALSE)
  }

  # Validate and normalise cell_styles.
  # Must be NULL or a list of length == total data rows.
  total_rows <- if (!is.null(data_single)) nrow(data_single)
                else sum(vapply(data_list, nrow, integer(1L)))
  cell_styles_resolved <- NULL
  if (!is.null(cell_styles)) {
    if (!is.list(cell_styles)) {
      stop("`cell_styles` must be a list (one element per data row) or NULL.",
           call. = FALSE)
    }
    if (length(cell_styles) != total_rows) {
      stop(sprintf(
        "`cell_styles` length (%d) must equal the number of data rows (%d).",
        length(cell_styles), total_rows), call. = FALSE)
    }
    cell_styles_resolved <- cell_styles
  }

  structure(
    list(
      data                        = data_single,
      data_list                   = data_list,
      col_header                  = col_hdr_one,
      col_header_list             = col_hdr_list,
      col_spec                    = cs,
      row_title                   = row_title_idx,
      border                      = border_resolved,
      blank_rows                  = blank_rows_resolved,
      col_rel_width               = col_rel_width,
      # kept as given unless whole numbers: the writer refuses the rest
      column_widths_twips         = if (is.numeric(column_widths_twips) &&
                                        all(is.finite(column_widths_twips)) &&
                                        all(column_widths_twips == round(column_widths_twips)))
                                      as.integer(column_widths_twips) else column_widths_twips,
      # the twips arguments are checked as the top / bottom padding is: a
      # non-number used to be written into the file as NA
      table_width_twips           = if (is.null(.check_row_height(table_width_twips, "table_width_twips"))) NULL else table_width_twips,
      table_width_pct_of_writable = twpw,
      table_align                 = table_align,
      row_height_twips            = .check_row_height(row_height_twips, "row_height_twips"),
      font_size_half_points       = .check_font_size(font_size_half_points, "font_size_half_points"),
      font                        = .check_font(font, "font"),
      row_height_exact            = row_height_exact,
      header_row_height_twips     = .check_row_height(header_row_height_twips, "header_row_height_twips"),
      blank_row_height_twips      = .check_row_height(blank_row_height_twips, "blank_row_height_twips"),
      cell_padding_left_twips     = .check_row_height(cell_padding_left_twips, "cell_padding_left_twips"),
      cell_padding_right_twips    = .check_row_height(cell_padding_right_twips, "cell_padding_right_twips"),
      cell_padding_top_twips      = .check_row_height(cell_padding_top_twips, "cell_padding_top_twips"),
      cell_padding_bottom_twips   = .check_row_height(cell_padding_bottom_twips, "cell_padding_bottom_twips"),
      cell_valign                 = cell_valign,
      cell_styles                 = cell_styles_resolved,
      blank_row_normalize         = .resolve_blank_row_normalize(blank_row_normalize),
      markup                      = .resolve_markup(markup),
      style_defaulted             = style_defaulted
    ),
    class = "rtftable"
  )
}


# Join column (leaf) labels into a single, newline-free line for printing,
# truncating with an ellipsis when it would be too wide for the console.
.fmt_label_line <- function(labs, width = 64L) {
  labs <- gsub("[\r\n]+", " ", labs)
  s <- paste(labs, collapse = " | ")
  # Truncate with a horizontal ellipsis (U+2026); built via intToUtf8() so the
  # source stays ASCII-only.
  if (nchar(s) > width) s <- paste0(substr(s, 1L, width - 1L), intToUtf8(8230L))
  s
}

# -- Console rendering of the table body ---------------------------------------
#
# The pieces below lay an rtftable out as a text grid -- column headers
# (including spanning headers), the rendered cells, per-column alignment, and
# horizontal rules drawn where the table's border zones are actually set -- so
# that print()ing an rtftable previews what the RTF will look like, much like
# printing a gt_tbl / rtables VTableTree / gtsummary object.  The rendering is
# deliberately faithful to the clinical (TFL) look: rules only where borders
# exist, columns separated by whitespace, no decorative grid.

# Display width of each string, accounting for wide (CJK) glyphs.  Falls back to
# the character count when the width is undeterminable (non-representable bytes).
.disp_width <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  w <- suppressWarnings(nchar(x, type = "width", allowNA = TRUE))
  bad <- is.na(w)
  if (any(bad)) w[bad] <- nchar(x[bad], type = "chars")
  as.integer(w)
}

# Pad a single-line string to `width` display columns under `align`.
.pad_cell <- function(text, width, align = "left") {
  text <- as.character(text)
  gap  <- width - .disp_width(text)
  if (length(gap) == 0L || is.na(gap) || gap <= 0L) return(text)
  switch(align,
    right  = paste0(strrep(" ", gap), text),
    center = {
      l <- gap %/% 2L
      paste0(strrep(" ", l), text, strrep(" ", gap - l))
    },
    paste0(text, strrep(" ", gap)))            # left / default
}

# Whether the console can render box-drawing characters.  ASCII fallback is used
# in a non-UTF-8 locale or when `options(rtfreporter.print_ascii = TRUE)`.
.print_uses_unicode <- function() {
  if (isTRUE(getOption("rtfreporter.print_ascii", FALSE))) return(FALSE)
  info <- tryCatch(l10n_info(), error = function(e) NULL)
  isTRUE(info[["UTF-8"]]) || identical(info[["codepage"]], 65001L)
}

# Horizontal rule glyph for one border side (an rtf_border_line, i.e. a list
# with `$style`), or NA when the side is absent / "none".
.rule_char <- function(side, unicode) {
  if (is.null(side)) return(NA_character_)
  style <- side$style %||% "single"
  if (identical(style, "none")) return(NA_character_)
  if (!unicode) return(if (style %in% c("double", "thick")) "=" else "-")
  # Box-drawing glyphs are written as \u escapes so the R source stays
  # ASCII-only (R CMD check "non-ASCII characters" WARNING otherwise):
  # 2500 single, 2550 double, 2501 thick, 2508 dot, 2504 dash -- the
  # names of .valid_border_styles (#550)
  switch(style,
    double = "\u2550", thick = "\u2501",
    dot = "\u2508", dash = "\u2504",
    "\u2500")                 # single / default
}

# Render the (first) table body of an rtftable as a character vector of lines.
.render_rtftable_console <- function(x, n = 10L) {
  unicode <- .print_uses_unicode()

  # A multi-DF rtftable carries data_list / col_header_list; preview the first.
  if (!is.null(x$data_list)) {
    body <- x$data_list[[1L]]
    hdr  <- (x$col_header_list %||% list())[[1L]] %||% x$col_header
  } else {
    body <- x$data
    hdr  <- x$col_header
  }
  if (is.null(body) || ncol(body) == 0L) return(character(0))

  nc <- ncol(body)
  nr <- nrow(body)
  cs <- x$col_spec
  sep  <- "  "
  sepw <- nchar(sep)

  # Leaf (per-column) labels used for widths; header rows are rendered below.
  leaf <- .flatten_col_header_labels(hdr, nc)
  if (is.null(leaf)) leaf <- names(body) %||% paste0("V", seq_len(nc))
  leaf <- as.character(leaf)

  # Rows to show, as a character matrix (NA -> "").  `n = Inf` shows them
  # all (as.integer(Inf) would be NA).
  n <- suppressWarnings(as.numeric(n)[1L])
  if (is.na(n)) n <- 10
  nshow <- as.integer(max(0, min(n, nr)))
  colcells <- lapply(seq_len(nc), function(j) {
    v <- if (nshow > 0L) as.character(body[[j]][seq_len(nshow)]) else character(0)
    v[is.na(v)] <- ""
    v
  })

  # Split each leaf label into physical lines (embedded "\n" -> stacked rows).
  leaf_lines <- lapply(seq_len(nc), function(j) {
    z <- strsplit(leaf[[j]] %||% "", "\n", fixed = TRUE)[[1L]]
    if (length(z) == 0L) "" else z
  })

  # Column widths: widest of the leaf-label lines and the shown data cells.
  width <- integer(nc)
  for (j in seq_len(nc)) {
    dl <- if (length(colcells[[j]])) max(.disp_width(colcells[[j]])) else 0L
    hl <- max(.disp_width(leaf_lines[[j]]))
    width[j] <- max(dl, hl, 1L)
  }

  # Border-side lookup for a zone ("header"/"spanning"/"body"/first/last row).
  bd   <- x$border
  side <- function(zone, s) {
    z <- if (is.null(bd)) NULL else bd[[zone]]
    if (is.null(z)) NULL else z[[s]]
  }

  # Widen columns so any spanning-header label fits across its span.
  span_rows <- if (is.list(hdr)) Filter(is.list, hdr) else list()
  for (row in span_rows) {
    for (cell in row) {
      from <- cell$from; to <- cell$to
      if (is.null(from) || is.null(to)) next
      cols  <- from:to
      need  <- max(.disp_width(strsplit(as.character(cell$label %||% ""),
                                        "\n", fixed = TRUE)[[1L]]), 0L)
      avail <- sum(width[cols]) + sepw * (length(cols) - 1L)
      if (need > avail) {
        deficit <- need - avail
        k   <- length(cols)
        add <- rep(deficit %/% k, k)
        rem <- deficit %% k
        if (rem > 0L) add[seq_len(rem)] <- add[seq_len(rem)] + 1L
        width[cols] <- width[cols] + add
      }
    }
  }
  total_w <- sum(width) + sepw * (nc - 1L)

  # Render a stack of per-column cells (each a character vector of physical
  # lines) into console lines, aligning within columns and v-aligning the
  # stack ("top" or "bottom").
  render_cells <- function(cells_lines, aligns, valign = "top") {
    h <- max(vapply(cells_lines, length, integer(1L)), 1L)
    vapply(seq_len(h), function(li) {
      parts <- vapply(seq_len(nc), function(j) {
        lj  <- cells_lines[[j]]
        kj  <- length(lj)
        idx <- if (identical(valign, "bottom")) li - (h - kj) else li
        txt <- if (idx >= 1L && idx <= kj) lj[[idx]] else ""
        .pad_cell(txt, width[j], aligns[[j]])
      }, character(1L))
      paste(parts, collapse = sep)
    }, character(1L))
  }

  # Render one spanning-header row -> its label line(s) plus an underline
  # line.  A label with embedded "\n" stacks its lines, bottom-aligned like
  # the leaf labels.  Contiguous columns not covered by a cell become blank
  # single-column units, so every unit is separated by exactly one column
  # boundary (one `sep`).
  render_span <- function(row) {
    by_from <- list()
    for (cell in row) by_from[[as.character(as.integer(cell$from))]] <- cell
    units <- list(); units_ul <- character(0)
    col <- 1L
    while (col <= nc) {
      cell <- by_from[[as.character(col)]]
      if (!is.null(cell)) {
        to  <- as.integer(cell$to)
        fw  <- sum(width[col:to]) + sepw * (to - col)
        lab <- as.character(cell$label %||% "")
        ln  <- strsplit(lab, "\n", fixed = TRUE)[[1L]]
        if (length(ln) == 0L) ln <- ""
        units[[length(units) + 1L]] <- list(
          lines = ln, width = fw, align = cell$align %||% "center")
        ul_on <- (isTRUE(cell$underline) || !is.null(cell$border) ||
                    !is.null(side("spanning", "bottom"))) && nzchar(lab)
        units_ul <- c(units_ul,
          if (ul_on) strrep(.rule_char(list(style = "single"), unicode), fw)
          else strrep(" ", fw))
        col <- to + 1L
      } else {
        units[[length(units) + 1L]] <- list(lines = "", width = width[col],
                                            align = "left")
        units_ul  <- c(units_ul,  strrep(" ", width[col]))
        col <- col + 1L
      }
    }
    h <- max(vapply(units, function(u) length(u$lines), integer(1L)))
    lab_lines <- vapply(seq_len(h), function(li) {
      paste(vapply(units, function(u) {
        idx <- li - (h - length(u$lines))
        txt <- if (idx >= 1L) u$lines[[idx]] else ""
        .pad_cell(txt, u$width, u$align)
      }, character(1L)), collapse = sep)
    }, character(1L))
    ul_line  <- paste(units_ul,  collapse = sep)
    list(label = lab_lines, underline = ul_line,
         has_underline = grepl("[^ ]", ul_line))
  }

  out  <- character(0)
  push <- function(...) out <<- c(out, ...)
  rule <- function(ch) if (!is.na(ch)) push(strrep(ch, total_w))

  # Attached page-level titles (centred) then the header top rule.
  ti <- attr(x, "rtf_titles", exact = TRUE)
  if (!is.null(ti)) for (t in ti) push(.pad_cell(as.character(t), total_w, "center"))

  rule(.rule_char(side("header", "top"), unicode))

  # Header rows, in order (spanning rows first, leaf label row last).
  hdr_rows <- if (is.null(hdr)) list(names(body) %||% leaf) else hdr
  for (row in hdr_rows) {
    if (is.character(row)) {
      lab <- as.character(row)
      if (length(lab) < nc) lab <- c(lab, rep("", nc - length(lab)))
      cells <- lapply(seq_len(nc), function(j)
        strsplit(lab[[j]] %||% "", "\n", fixed = TRUE)[[1L]] %||% "")
      cells   <- lapply(cells, function(z) if (length(z) == 0L) "" else z)
      h_align <- lapply(seq_len(nc), function(j)
        (cs[[j]]$header_align %||% cs[[j]]$align) %||% "center")
      push(render_cells(cells, h_align, valign = "bottom"))
    } else if (is.list(row)) {
      sr <- render_span(row)
      push(sr$label)
      if (sr$has_underline) push(sr$underline)
    }
  }

  # Rule between header and body.
  rule(.rule_char(side("header", "bottom"), unicode))

  # Body rows.  A first-row top override draws a rule above the first data row.
  rule(.rule_char(side("first_row", "top"), unicode))
  if (nshow > 0L) {
    d_align <- lapply(seq_len(nc), function(j) cs[[j]]$align %||% "left")
    for (i in seq_len(nshow)) {
      cells <- lapply(seq_len(nc), function(j)
        strsplit(colcells[[j]][[i]], "\n", fixed = TRUE)[[1L]] %||% "")
      cells <- lapply(cells, function(z) if (length(z) == 0L) "" else z)
      push(render_cells(cells, d_align, valign = x$cell_valign %||% "top"))
    }
  }
  if (nshow < nr) {
    more <- nr - nshow
    push(sprintf("%s (%d more row%s)",
                 if (unicode) "\u2026" else "...", more,
                 if (more == 1L) "" else "s"))
  }

  # Bottom rule of the last data row (last_row override, else body bottom).
  rule(.rule_char(side("last_row", "bottom") %||% side("body", "bottom"), unicode))

  # Attached footnotes, left-aligned below the table.
  fo <- attr(x, "rtf_footnotes", exact = TRUE)
  if (!is.null(fo)) for (f in fo) push(as.character(f))

  out
}

# Metadata block (the compact, labelled summary that used to be the whole of
# print()).  Returned as a character vector so print()/summary() can reuse it.
.rtftable_meta_lines <- function(x) {
  if (!is.null(x$data_list)) {
    ntab <- length(x$data_list)
    nr   <- sum(vapply(x$data_list, nrow, integer(1L)))
    body <- x$data_list[[1L]]
    nc   <- if (is.null(body)) 0L else ncol(body)
    hdr  <- (x$col_header_list %||% list())[[1L]] %||% x$col_header
    head_line <- sprintf("<rtftable> %d tables, %d body rows x %d columns",
                         ntab, nr, nc)
  } else {
    body <- x$data
    nr   <- if (is.null(body)) 0L else nrow(body)
    nc   <- if (is.null(body)) length(x$col_spec) else ncol(body)
    hdr  <- x$col_header
    head_line <- sprintf("<rtftable> %d row%s x %d columns",
                         nr, if (nr == 1L) "" else "s", nc)
  }

  L <- head_line
  add <- function(...) L <<- c(L, paste0(...))

  labs <- .flatten_col_header_labels(hdr, nc)
  if (!is.null(labs) && any(nzchar(labs)))
    add("  Columns:    ", .fmt_label_line(labs))
  nhdr <- if (is.null(hdr)) 0L else if (is.character(hdr)) 1L else length(hdr)
  if (nhdr > 1L) add(sprintf("  Header rows: %d (spanning)", nhdr))

  rt <- x$row_title %||% 1L
  add("  Row title:  col ", paste(rt, collapse = ", "))
  add("  Borders:    ", if (is.null(x$border)) "none" else "set")
  width_desc <-
    if (!is.null(x$col_rel_width))
      paste("relative", paste(x$col_rel_width, collapse = ":"))
    else if (!is.null(x$column_widths_twips))
      sprintf("fixed (%s twips)", paste(x$column_widths_twips, collapse = ", "))
    else "auto / inherited"
  add("  Widths:     ", width_desc)
  if (length(x$markup))
    add("  Markup:     ", paste(x$markup, collapse = ", "))
  if (!is.null(x$cell_styles))
    add(sprintf("  Cell styles: set (%d rows)", length(x$cell_styles)))

  ti <- attr(x, "rtf_titles",    exact = TRUE)
  fo <- attr(x, "rtf_footnotes", exact = TRUE)
  if (!is.null(ti)) add(sprintf("  Titles:     %d line(s)", length(ti)))
  if (!is.null(fo)) add(sprintf("  Footnotes:  %d line(s)", length(fo)))
  L
}

#' Render an rtftable body as console text
#'
#' Lays an [rtftable()] out as a text grid -- column headers (including spanning
#' headers), the rendered cells, per-column alignment, and horizontal rules
#' where the table's border zones are set -- so the result previews what the RTF
#' will look like.  Used by [print.rtftable()]; call it directly to capture the
#' rendered lines.
#'
#' @param x An `rtftable` object.
#' @param n Number of body rows to render (default `10`).
#' @param ... Additional arguments (unused).
#'
#' @return A character vector, one element per console line.
#'
#' @examples
#' dm <- data.frame(
#'   Characteristic = c("Age (years)", "  Mean (SD)", "  Median"),
#'   `Drug A`       = c("", "54.2 (11.3)", "55.0"),
#'   check.names = FALSE, stringsAsFactors = FALSE
#' )
#' tbl <- rtftable(dm, border = "tfl")
#'
#' # Capture the console rendering instead of printing it -- useful for
#' # snapshot tests, or to paste a preview into an issue.
#' lines <- format(tbl)
#' length(lines)
#' writeLines(head(lines, 4))
#'
#' # Show more body rows than print() does by default
#' invisible(format(tbl, n = 3))
#'
#' @seealso [print.rtftable()], [summary.rtftable()].
#' @export
format.rtftable <- function(x, n = 10L, ...) {
  .render_rtftable_console(x, n = n)
}

#' Print an rtftable object
#'
#' Prints a visual preview of an [rtftable()] -- the laid-out cells with column
#' (and spanning) headers, per-column alignment, and horizontal rules drawn
#' where the table's border zones are set -- followed by a compact metadata
#' block (dimensions, leaf labels, row-title columns, border/width mode, and any
#' attached title / footnote counts).  This mirrors printing a `gt_tbl`,
#' `rtables` `VTableTree`, or `gtsummary` object: the goal is to eyeball the
#' table, not just its metadata.  For an internal multi-table rtftable (one
#' carrying several data.frames), the first table is rendered.
#'
#' @param x An `rtftable` object.
#' @param n Number of body rows to render (default `10`).
#' @param ... Additional arguments (unused).
#'
#' @return `x`, invisibly. Called for the side effect of printing.
#'
#' @seealso [format.rtftable()] to capture the rendered lines,
#'   [summary.rtftable()] for the metadata block on its own.
#'
#' @examples
#' df <- data.frame(
#'   Characteristic = c("Age (years)", "  Mean (SD)", "Sex", "  Female"),
#'   `Drug A`       = c("", "75.1 (8.2)", "", "53 (54%)"),
#'   check.names    = FALSE
#' )
#' print(rtftable(df, col_header = c("Characteristic", "Drug A\nN = 98")))
#'
#' @export
print.rtftable <- function(x, n = 10L, ...) {
  body_lines <- .render_rtftable_console(x, n = n)
  if (length(body_lines)) cat(body_lines, sep = "\n")
  cat("\n")
  cat(.rtftable_meta_lines(x), sep = "\n")
  cat("\n")
  invisible(x)
}

#' Summarise an rtftable object
#'
#' Prints the compact metadata block for an [rtftable()] -- dimensions, leaf
#' labels, row-title columns, border and column-width mode, and any attached
#' title / footnote counts -- without the visual table body.
#'
#' @param object An `rtftable` object.
#' @param ... Additional arguments (unused).
#'
#' @return `object`, invisibly.
#'
#' @examples
#' dm <- data.frame(
#'   Characteristic = c("Age (years)", "  Mean (SD)", "Sex", "  Female, n (%)"),
#'   `Drug A`       = c("", "54.2 (11.3)", "", "31 (51.7%)"),
#'   `Drug B`       = c("", "56.8 (10.1)", "", "27 (46.6%)"),
#'   check.names = FALSE, stringsAsFactors = FALSE
#' )
#' tbl <- rtftable(dm, border = "tfl", col_rel_width = c(2, 1, 1))
#'
#' # The metadata block on its own -- dimensions, header rows, widths, borders
#' summary(tbl)
#'
#' @seealso [print.rtftable()] for the same block plus the table body,
#'   [format.rtftable()] to capture the rendered lines as a character vector.
#' @export
summary.rtftable <- function(object, ...) {
  cat(.rtftable_meta_lines(object), sep = "\n")
  cat("\n")
  invisible(object)
}

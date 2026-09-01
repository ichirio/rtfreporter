# ============================================================================
#  Unified column-header API (v0.0.21+)
# ============================================================================
#
#  Multi-row column headers were historically split across two arguments --
#  `col_header` (label rows + nested spanning rows) and `spanning_header`
#  (a single span row placed above col_header).  The split made it awkward
#  to span at the top-most level using `col_header` alone and forced two
#  different cell-spec shapes (`from = / to =` versus a plain character
#  vector).
#
#  This file adds a single unified specification, accepted by
#  `rtftable(col_header = ...)`, in which every cell is described by
#  `pos = ...`:
#
#    pos = 1            single-column cell at data column 1
#    pos = c(2, 5)      cell spanning data columns 2 through 5
#
#  Positions always refer to the underlying **data columns** -- not to the
#  positions of cells in the row above.  Cells in a row must not overlap;
#  gaps are filled internally with empty cells so the renderer always
#  sees a fully-covered row.
#
#  Three small public helpers make the API ergonomic:
#
#    col_cell(pos, label, ...)           -- one cell
#    rtf_col_header(...)                 -- collect rows top-to-bottom
#    add_col_header_row(hdr, row, ...)   -- append (or prepend) a row
#
#  Internally a pos-style row is converted to the existing (from, to)
#  spanning-row representation via `.pos_row_to_spans()`, so every
#  renderer path keeps working unchanged.
#
#  Backward compatibility
#  ----------------------
#  Every previously-accepted form continues to work:
#    NULL                              -> use names(data)
#    c("A", "B", "C")                  -> single label row
#    "A | B | C"                       -> single label row
#    list(c("A","B"), c("X","Y"))      -> multi-row labels
#    list(list(list(from=1,to=2,label="X")), c("A","B")) -> spanning + labels
#  The old `spanning_header =` argument is also unchanged.
#  ============================================================================


#' Column-header cell specification
#'
#' Convenience constructor for a single cell in a column-header row passed
#' to [rtftable()] (via `col_header =`) or [rtf_col_header()].
#'
#' Use `pos = 1` for a single-column cell and `pos = c(start, end)` for a
#' cell that spans several data columns.  Positions are always relative to
#' the underlying data columns, not to the previous header row.
#'
#' `pos` may instead be **column name(s)** (character), resolved against the
#' data columns when the header is attached to a table.  This makes a
#' spanning cell robust to column reordering and errors out on an unknown
#' name -- for example `col_cell(c("g1", "g3"), "Drug A")` spans from the
#' column named `g1` to the one named `g3`, and `col_cell("total", "Total")`
#' targets a single named column.
#'
#' @param pos Cell position, one of:
#'   * a numeric of length 1 (single column) or length 2 (`c(start, end)`,
#'     inclusive) -- `start <= end` required, values `>= 1`; or
#'   * a character of length 1 (single column name) or length 2
#'     (`c(start_name, end_name)`) referring to data columns by name.
#'   Name resolution (and the `start <= end` check for named ranges) happens
#'   when the header is attached to a table.
#' @param label Character scalar.  Cell text; may be `""`.
#' @param align Optional `"left"`, `"center"`, or `"right"`.  `NULL`
#'   (default) inherits the leftmost covered column's `header_align`.
#' @param bold,italic,underline Logical.  Default `FALSE`.
#' @param border Optional [rtf_border()] applied to **this header cell
#'   only**, overriding the zone border and the automatic group-underline.
#'   This is how you fine-tune individual rules in a multi-row header --
#'   for example removing the bottom line under one spanning cell with
#'   `border = rtf_border(bottom = "none")`, or adding a
#'   thicker rule under one column.  `NULL` (default) inherits the normal
#'   zone borders.
#'
#' @return A list of class `"rtf_col_cell"`.
#'
#' @examples
#' col_cell(1, "Item")
#' col_cell(c(2, 5), "Treatment", align = "center", underline = TRUE)
#'
#' # Remove the group underline under just this spanning cell:
#' col_cell(c(2, 3), "Drug A",
#'          border = rtf_border(bottom = "none"))
#'
#' @export
col_cell <- function(pos, label = "", align = NULL,
                     bold = FALSE, italic = FALSE, underline = FALSE,
                     border = NULL) {
  if (!length(pos) %in% c(1L, 2L) || any(is.na(pos))) {
    stop("`pos` must be a numeric or character of length 1 or 2.",
         call. = FALSE)
  }
  if (is.character(pos)) {
    # Column-name reference; resolved to positions when the header is
    # attached to a table (see `.pos_row_to_spans()`).
    if (!all(nzchar(pos))) {
      stop("`pos` column names must be non-empty strings.", call. = FALSE)
    }
  } else if (is.numeric(pos)) {
    pos <- as.integer(pos)
    if (any(pos < 1L)) stop("`pos` values must be >= 1.", call. = FALSE)
    if (length(pos) == 2L && pos[1L] > pos[2L]) {
      stop("`pos` start must be <= end.", call. = FALSE)
    }
  } else {
    stop("`pos` must be a numeric or character of length 1 or 2.",
         call. = FALSE)
  }
  if (!is.null(align) && !align %in% c("left", "center", "right")) {
    stop("`align` must be NULL, \"left\", \"center\", or \"right\".",
         call. = FALSE)
  }
  if (!is.null(border) && !inherits(border, "rtf_border")) {
    stop("`border` must be NULL or an rtf_border object.", call. = FALSE)
  }
  spec <- list(
    pos   = pos,
    label = if (is.null(label)) "" else as.character(label)[1L]
  )
  if (!is.null(align))   spec$align     <- align
  if (isTRUE(bold))      spec$bold      <- TRUE
  if (isTRUE(italic))    spec$italic    <- TRUE
  if (isTRUE(underline)) spec$underline <- TRUE
  if (!is.null(border))  spec$border    <- border
  structure(spec, class = "rtf_col_cell")
}

#' @export
print.rtf_col_cell <- function(x, ...) {
  fmt <- if (is.character(x$pos)) "%s" else "%d"
  pos_str <- if (length(x$pos) == 1L) sprintf(fmt, x$pos)
             else sprintf(paste0(fmt, "..", fmt), x$pos[1L], x$pos[2L])
  deco <- c(if (isTRUE(x$bold)) "b", if (isTRUE(x$italic)) "i",
             if (isTRUE(x$underline)) "u")
  align <- if (!is.null(x$align)) sprintf(", align=%s", x$align) else ""
  deco_str <- if (length(deco)) sprintf(" [%s]", paste(deco, collapse = "")) else ""
  cat(sprintf("<col_cell pos=%s label=%s%s%s>\n",
              pos_str, dQuote(x$label, q = FALSE), align, deco_str))
  invisible(x)
}

#' Build a multi-row column-header specification
#'
#' Collects column-header rows, top-to-bottom, into a single object that
#' can be passed to `rtftable(col_header = ...)`.  Each argument is one
#' row; a row may be either:
#'
#' \describe{
#'   \item{a character vector}{one label per data column (legacy form), or}
#'   \item{a list of [col_cell()] objects}{for a row with single and/or
#'     spanning cells.}
#' }
#'
#' @param ... Header rows in render order (top first).
#'
#' @return A list of class `"rtf_col_header"`.
#'
#' @examples
#' \dontrun{
#' rtf_col_header(
#'   list(col_cell(1, ""), col_cell(c(2, 5), "Treatment")),
#'   list(col_cell(1, ""),
#'        col_cell(c(2, 3), "Drug A"),
#'        col_cell(c(4, 5), "Drug B")),
#'   c("Item", "N", "Mean", "N", "Mean")
#' )
#' }
#'
#' @export
rtf_col_header <- function(...) {
  rows <- list(...)
  structure(rows, class = "rtf_col_header")
}

#' @export
print.rtf_col_header <- function(x, ...) {
  cat(sprintf("<rtf_col_header -- %d row%s>\n",
              length(x), if (length(x) == 1L) "" else "s"))
  for (i in seq_along(x)) {
    row <- x[[i]]
    if (is.character(row)) {
      cat(sprintf("  [%d] labels: %s\n", i,
                  paste(dQuote(row, q = FALSE), collapse = ", ")))
    } else if (is.list(row)) {
      cells <- vapply(row, function(c) {
        pos <- c$pos %||% c(c$from %||% NA, c$to %||% NA)
        fmt <- if (is.character(pos)) "%s" else "%d"
        if (length(pos) == 1L || (length(pos) == 2L && pos[1L] == pos[2L])) {
          sprintf(paste0("%s@", fmt), c$label %||% "", pos[1L])
        } else {
          sprintf(paste0("%s@", fmt, "-", fmt),
                  c$label %||% "", pos[1L], pos[2L])
        }
      }, character(1L))
      cat(sprintf("  [%d] cells: %s\n", i,
                  paste(cells, collapse = ", ")))
    }
  }
  invisible(x)
}

#' Append (or prepend) a row to an `rtf_col_header`
#'
#' @param hdr An [rtf_col_header()], or any value accepted by
#'   `rtftable(col_header = ...)`.  Non-`rtf_col_header` inputs are
#'   promoted automatically.
#' @param row One header row: a character vector or a list of cell specs.
#' @param .position `"bottom"` (default) appends below the existing rows;
#'   `"top"` prepends above.
#'
#' @return A new `rtf_col_header`.
#'
#' @examples
#' \dontrun{
#' hdr <- rtf_col_header(c("Item", "N", "Mean", "N", "Mean"))   # bottom row
#' hdr <- add_col_header_row(
#'   hdr,
#'   list(col_cell(1, ""),
#'        col_cell(c(2, 3), "Drug A"),
#'        col_cell(c(4, 5), "Drug B")),
#'   .position = "top"
#' )
#' }
#'
#' @export
add_col_header_row <- function(hdr, row,
                                .position = c("bottom", "top")) {
  .position <- match.arg(.position)
  if (!inherits(hdr, "rtf_col_header")) {
    hdr <- rtf_col_header(hdr)
  }
  current <- unclass(hdr)
  result  <- if (.position == "bottom") c(current, list(row))
             else c(list(row), current)
  structure(result, class = "rtf_col_header")
}


# -- Internal helpers --------------------------------------------------------

# Is `x` a cell spec -- either the new pos form or the legacy from/to form?
.is_cell_spec <- function(x) {
  is.list(x) && length(x) > 0L && (!is.null(x$pos) || !is.null(x$from))
}

# Resolve a single cell `pos` (numeric or character column name(s)) to an
# integer vector of length 1 or 2.  `col_names` is required only when `pos`
# is character.
.resolve_cell_pos <- function(pos, col_names) {
  if (is.null(pos)) stop("Cell spec missing `pos` field.", call. = FALSE)
  if (!is.character(pos)) return(as.integer(pos))
  if (is.null(col_names)) {
    stop("Column-name `pos` in col_cell() requires the header to be ",
         "attached to a table (data column names are unknown here).",
         call. = FALSE)
  }
  vapply(pos, function(nm) {
    j <- which(col_names == nm)
    if (length(j) == 0L) {
      stop(sprintf("`pos` column name \"%s\" not found in data columns. Available: %s.",
                   nm, paste(sprintf("\"%s\"", col_names), collapse = ", ")),
           call. = FALSE)
    }
    if (length(j) > 1L) {
      stop(sprintf("`pos` column name \"%s\" is ambiguous (matches %d columns).",
                   nm, length(j)), call. = FALSE)
    }
    as.integer(j)
  }, integer(1L), USE.NAMES = FALSE)
}

# Convert a pos-style row into the (from, to)-spanning representation the
# renderer expects.  Cells are sorted by start position; gaps in coverage
# are filled with empty cells so every data column is covered exactly once.
# `col_names` is used to resolve any column-name `pos` in the cells.
.pos_row_to_spans <- function(row, ncol_df, col_names = NULL) {
  if (length(row) == 0L) return(list())

  # Resolve any column-name positions to integers up front, so downstream
  # sorting / range logic is purely numeric.
  row <- lapply(row, function(c) {
    c$pos <- .resolve_cell_pos(c$pos, col_names)
    c
  })

  # Sort cells by start position.
  starts <- vapply(row, function(c) as.integer(c$pos[1L]), integer(1L))
  row <- row[order(starts)]

  result   <- list()
  next_col <- 1L
  for (cell in row) {
    p <- as.integer(cell$pos)
    if (length(p) > 2L) {
      stop("`pos` must be a scalar or a length-2 integer vector.",
           call. = FALSE)
    }
    from <- p[1L]
    to   <- if (length(p) == 1L) p[1L] else p[2L]
    if (from > to) {
      stop(sprintf("`pos` start (%d) must be <= end (%d).", from, to),
           call. = FALSE)
    }
    if (from < 1L || to > ncol_df) {
      stop(sprintf("`pos` %d-%d outside data column range 1..%d.",
                   from, to, ncol_df), call. = FALSE)
    }
    if (from < next_col) {
      stop(sprintf("`pos` %d-%d overlaps the previous cell (which ended at column %d).",
                   from, to, next_col - 1L), call. = FALSE)
    }
    # Fill gap with an empty cell.
    if (from > next_col) {
      result <- c(result, list(list(from = next_col, to = from - 1L,
                                     label = "")))
    }
    # Convert: drop pos, add from / to, keep other fields.
    spec <- list(from = from, to = to)
    for (k in setdiff(names(cell), "pos")) spec[[k]] <- cell[[k]]
    if (is.null(spec$label)) spec$label <- ""
    result   <- c(result, list(spec))
    next_col <- to + 1L
  }
  # Tail gap.
  if (next_col <= ncol_df) {
    result <- c(result, list(list(from = next_col, to = ncol_df, label = "")))
  }
  result
}

# Resolve a character label row into a full-length, position-indexed label
# vector using the data column names.
#
# The name-aware path is engaged *only* when at least one element carries a
# name; a fully-unnamed vector is returned unchanged (legacy positional
# behaviour).  Per-element resolution:
#
#   * named element (`g1 = "G1"`)   -> place on the column named `g1`;
#                                       unknown / duplicate name is an error.
#   * unnamed element (`"Total"`)   -> if the value is itself a column name,
#                                       place there (label == column name);
#                                       otherwise place positionally at this
#                                       element's index.
#
# Two elements resolving to the same column is a conflict (error).  Columns
# never targeted keep their own name as the label.
.resolve_named_label_row <- function(row, col_names) {
  nms <- names(row)
  # No names at all -> legacy positional row, untouched.
  if (is.null(nms) || !any(nzchar(nms))) return(row)

  if (is.null(col_names)) {
    stop("A named col_header label row needs data column names, which are ",
         "unknown here (attach the header to a table instead).",
         call. = FALSE)
  }
  n <- length(col_names)
  out     <- as.character(col_names)   # default: label = column name
  claimed <- rep(NA_character_, n)     # records how each column was claimed

  claim <- function(j, label, src) {
    if (!is.na(claimed[j])) {
      stop(sprintf(
        "col_header: column %d (\"%s\") is targeted twice -- %s and %s.",
        j, col_names[j], claimed[j], src), call. = FALSE)
    }
    out[j]     <<- label
    claimed[j] <<- src
  }

  for (i in seq_along(row)) {
    nm  <- nms[i]
    val <- as.character(row[[i]])
    if (nzchar(nm)) {
      # Named element: target the column named `nm`.
      j <- which(col_names == nm)
      if (length(j) == 0L) {
        stop(sprintf("col_header: unknown column name \"%s\". Available: %s.",
                     nm, paste(sprintf("\"%s\"", col_names), collapse = ", ")),
             call. = FALSE)
      }
      if (length(j) > 1L) {
        stop(sprintf("col_header: column name \"%s\" is ambiguous (matches %d columns).",
                     nm, length(j)), call. = FALSE)
      }
      claim(j, val, sprintf("name \"%s\"", nm))
    } else {
      # Unnamed element: try the value as a column name, else positional.
      j <- which(col_names == val)
      if (length(j) == 1L) {
        claim(j, val, sprintf("value \"%s\" (matched as a column name)", val))
      } else if (length(j) > 1L) {
        stop(sprintf("col_header: value \"%s\" matches %d columns; name it explicitly.",
                     val, length(j)), call. = FALSE)
      } else {
        if (i > n) {
          stop(sprintf(
            paste0("col_header: label #%d (\"%s\") has no matching column ",
                   "name and its position (%d) exceeds the number of data ",
                   "columns (%d)."),
            i, val, i, n), call. = FALSE)
        }
        claim(i, val, sprintf("position %d", i))
      }
    }
  }
  out
}


# ============================================================================
#  Spanning header from delimited column names
# ============================================================================

# Default separators understood when reconstructing a spanning column header
# from a plain data.frame's column names (see `.split_names_to_col_header()`):
#   "____"              -- ydisctools::pivot_stats_wider()
#   "___tlang_delim___" -- tfrmt's column delimiter
.default_header_seps <- function() c("____", "___tlang_delim___")

# Reconstruct a multi-row spanning column header by splitting delimited column
# names on `seps`.  A bare data.frame carries no spanning metadata, so the
# nesting is parsed out of the names: each name becomes a stack of segments,
# horizontally adjacent columns that share a label AND the same ancestor path
# are merged into a spanning cell, and columns with fewer segments (e.g. id
# columns with no separator) are bottom-aligned so their label sits on the leaf
# row with blank cells above.  A doubled "____" (i.e. "________") splits to an
# empty middle segment -> a blank cell at that header level.
#
# Returns the same shape the rtables adapter emits -- upper rows as lists of
# `col_cell()`, bottom row as a character vector -- so `drop_cols` reindexing
# and the rest of the pipeline treat it like any adapter spanning header.
# Returns NULL when no name splits into more than one segment (caller then
# falls back to the plain `names(data)` single-row header).
.split_names_to_col_header <- function(col_names, seps) {
  if (is.null(seps)) return(NULL)
  seps <- seps[!is.na(seps) & nzchar(seps)]
  if (length(seps) == 0L || length(col_names) == 0L) return(NULL)

  # Literal-quote each separator (PCRE \Q...\E) and try longest first, so a
  # longer separator wins over a shorter one that is its prefix.  \Q...\E avoids
  # gsub-style backreference escaping (unreliable in this R build).
  seps    <- seps[order(-nchar(seps))]
  pattern <- paste0("\\Q", seps, "\\E", collapse = "|")

  segs  <- strsplit(col_names, pattern, perl = TRUE)
  segs  <- lapply(segs, function(s) if (length(s) == 0L) "" else s)
  depth <- max(lengths(segs))
  if (depth <= 1L) return(NULL)

  ncol <- length(col_names)
  # Bottom-align each column's segments into a depth x ncol label matrix; cells
  # above a short column's top segment are NA ("no cell"), distinct from an
  # explicit empty-string segment ("" -> a blank labelled cell).
  M <- matrix(NA_character_, nrow = depth, ncol = ncol)
  for (j in seq_len(ncol)) {
    s <- segs[[j]]
    M[(depth - length(s) + 1L):depth, j] <- s
  }

  same_label <- function(a, b) {
    (is.na(a) && is.na(b)) || (!is.na(a) && !is.na(b) && a == b)
  }

  # Upper rows (1 .. depth-1): merge adjacent columns sharing this row's label
  # and the identical ancestor path (rows above).  Each run -> one col_cell.
  upper_rows <- vector("list", depth - 1L)
  for (r in seq_len(depth - 1L)) {
    cells <- list()
    j <- 1L
    while (j <= ncol) {
      k <- j
      while (k + 1L <= ncol &&
             same_label(M[r, k + 1L], M[r, j]) &&
             identical(M[seq_len(r - 1L), k + 1L], M[seq_len(r - 1L), j])) {
        k <- k + 1L
      }
      lab <- if (is.na(M[r, j])) "" else M[r, j]
      cells <- c(cells,
                 list(col_cell(pos = if (j == k) j else c(j, k), label = lab)))
      j <- k + 1L
    }
    upper_rows[[r]] <- cells
  }

  # Bottom (leaf) row: one label per column, never merged.
  bottom <- M[depth, ]
  bottom[is.na(bottom)] <- ""

  c(upper_rows, list(bottom))
}

#' Build a spanning column header from delimited column names
#'
#' Reconstructs a multi-row, spanning [rtf_col_header()] by parsing the nesting
#' encoded in delimited column names -- e.g. `"Drug A____N"`, `"Drug A____Mean"`,
#' `"Drug B____N"`, `"Drug B____Mean"` becomes a `Drug A` / `Drug B` spanning row
#' over an `N` / `Mean` leaf row.  Horizontally adjacent columns that share a
#' label **and** the same ancestor path are merged into one spanning cell;
#' columns with fewer segments (e.g. an id column with no separator) are
#' bottom-aligned so their label sits on the leaf row with blank cells above.
#'
#' This is the same reconstruction [as_rtftables()] applies automatically to a
#' plain data.frame; exposing it lets you build the header explicitly and pass
#' it to [set_col_header()] or `rtftable(col_header = )`.
#'
#' @param names A character vector of column names, or a `data.frame` (its
#'   `names()` are used).
#' @param sep Character vector of separator(s) to split names on; the longest
#'   matching separator wins.  Default recognises `"____"`
#'   (`ydisctools::pivot_stats_wider()`) and `"___tlang_delim___"` (tfrmt's
#'   column delimiter).  A doubled separator yields an empty (blank) cell at
#'   that level.
#'
#' @return An [rtf_col_header()].  When no name splits into more than one
#'   segment, a single flat label row of `names`.
#'
#' @seealso [set_col_header()] to apply it, [rtf_col_header()] / [col_cell()]
#'   for the pieces.
#'
#' @examples
#' col_header_from_names(
#'   c("Item", "Drug A____N", "Drug A____Mean", "Drug B____N", "Drug B____Mean")
#' )
#' @export
col_header_from_names <- function(names, sep = .default_header_seps()) {
  if (is.data.frame(names)) names <- names(names)
  if (!is.character(names)) names <- as.character(names)
  if (length(names) == 0L) {
    stop("`names` must be a non-empty character vector (or a data.frame).",
         call. = FALSE)
  }
  rows <- .split_names_to_col_header(names, sep)
  if (is.null(rows)) rows <- list(names)          # flat: one label row
  structure(rows, class = "rtf_col_header")
}

# ============================================================================
#  rtf_header_source() -- deparse a table's column header back to editable
#  rtf_col_header() source (#253)
# ============================================================================
#
#  Building a multi-level / spanning column header from scratch is fiddly.  It
#  is far easier to render the *current* header back as editable
#  rtf_col_header() source, tweak the labels / spans, and re-apply with
#  set_col_header().  This file provides that deparser.

# -- small deparse helpers ---------------------------------------------------

# A single string as an R literal (proper escaping of quotes / specials).
.hs_str <- function(s) encodeString(s, quote = "\"")

# A column name used as an R *name* (e.g. in a named vector): backtick-quote
# only when it is not a syntactic name.
.hs_name <- function(nm) {
  if (grepl("^[a-zA-Z.][a-zA-Z0-9._]*$", nm) && !grepl("^\\.[0-9]", nm)) nm
  else paste0("`", nm, "`")
}

# rtf_border_side(...) source.  `level`: "explicit" omits the default width
# (15); "default" / "all" show it.  A non-NULL colour is always shown.
.hs_side <- function(sd, level) {
  args <- .hs_str(sd$style)
  if (!is.null(sd$width) && (sd$width != 15L || level %in% c("default", "all"))) {
    args <- c(args, as.character(sd$width))
  }
  if (!is.null(sd$color)) args <- c(args, paste0("color = ", .hs_str(sd$color)))
  paste0("rtf_border_side(", paste(args, collapse = ", "), ")")
}

# rtf_border(...) source (only the sides that are set).
.hs_border <- function(b, level) {
  parts <- character(0)
  for (s in c("top", "bottom", "left", "right")) {
    if (!is.null(b[[s]])) {
      parts <- c(parts, paste0(s, " = ", .hs_side(b[[s]], level)))
    }
  }
  paste0("rtf_border(", paste(parts, collapse = ", "), ")")
}

# A cell `pos` expressed by column name(s).
.hs_pos <- function(from, to, cn) {
  if (from == to) .hs_str(cn[from])
  else paste0("c(", .hs_str(cn[from]), ", ", .hs_str(cn[to]), ")")
}

# col_cell(...) source for one spanning / cell-row cell.
.hs_cell <- function(cell, cn, col_spec, level) {
  args <- c(.hs_pos(cell$from, cell$to, cn), .hs_str(cell$label %||% ""))
  eff_align <- col_spec[[cell$from]]$header_align
  if (!is.null(cell$align)) {
    args <- c(args, paste0("align = ", .hs_str(cell$align)))
  } else if (level %in% c("default", "all") && !is.null(eff_align)) {
    args <- c(args, paste0("align = ", .hs_str(eff_align)))
  }
  for (f in c("bold", "italic", "underline")) {
    if (isTRUE(cell[[f]]))    args <- c(args, paste0(f, " = TRUE"))
    else if (level == "all")  args <- c(args, paste0(f, " = FALSE"))
  }
  if (!is.null(cell$border)) {
    args <- c(args, paste0("border = ", .hs_border(cell$border, level)))
  }
  paste0("col_cell(", paste(args, collapse = ", "), ")")
}

# A character label row as a *named* c(...) keyed by column name.
.hs_label_row <- function(row, cn) {
  n <- min(length(row), length(cn))
  items <- vapply(seq_len(n), function(i) {
    paste0(.hs_name(cn[i]), " = ", .hs_str(row[[i]]))
  }, character(1L))
  paste0("c(", paste(items, collapse = ", "), ")")
}

# Build a scaffold top row: `stub_idx` columns stay as single empty cells; the
# remaining columns are bundled under one empty spanning cell (a placeholder
# whose label the caller fills in).  Returned in the internal (from, to, label)
# cell form so it deparses like any other spanning row.
.hs_scaffold_row <- function(ncol, stub_idx) {
  ns    <- setdiff(seq_len(ncol), stub_idx)
  cells <- lapply(stub_idx, function(j) list(from = j, to = j, label = ""))
  if (length(ns)) {
    cells <- c(cells, list(list(from = min(ns), to = max(ns), label = "")))
  }
  cells[order(vapply(cells, function(c) c$from, integer(1L)))]
}

# rtf_col_header(...) source for a full list of header rows.
.hs_col_header <- function(rows, cn, col_spec, level, ind = "  ") {
  parts <- vapply(rows, function(row) {
    if (is.character(row)) return(.hs_label_row(row, cn))
    inner <- vapply(row, .hs_cell, character(1L),
                    cn = cn, col_spec = col_spec, level = level)
    paste0("list(", paste(inner, collapse = ", "), ")")
  }, character(1L))
  paste0("rtf_col_header(\n", ind, "  ",
         paste(parts, collapse = paste0(",\n", ind, "  ")),
         "\n", ind, ")")
}

# align = c(...) line for set_col_header(), from the per-column header_align.
# "explicit" level suppresses it when every column's header alignment already
# equals its data alignment (i.e. nothing was set explicitly).
.hs_align_line <- function(col_spec, level) {
  ha <- vapply(col_spec, function(s) s$header_align %||% "", character(1L))
  da <- vapply(col_spec, function(s) s$align %||% "", character(1L))
  if (level == "explicit" && all(ha == da)) return(NULL)
  paste0("align = c(", paste(vapply(ha, .hs_str, character(1L)),
                             collapse = ", "), ")")
}

# style_zone(...) line for the header-related zones only (header / spanning).
.hs_zone_line <- function(border, level) {
  if (is.null(border)) return(NULL)
  parts <- character(0)
  for (z in c("header", "spanning")) {
    if (!is.null(border[[z]])) {
      parts <- c(parts, paste0(z, " = ", .hs_border(border[[z]], level)))
    }
  }
  if (length(parts) == 0L) return(NULL)
  paste0("style_zone(", paste(parts, collapse = ", "), ")")
}


# -- public function ---------------------------------------------------------

#' Deparse a table's column header back to editable `rtf_col_header()` source
#'
#' Renders the **current** column header of a finished [rtftable()] (or the
#' first page of an [as_rtftables()] list) as `rtf_col_header(...)` source
#' text, addressed by **column name**, so you can copy it, edit the labels /
#' spans, and re-apply with [set_col_header()].  It is the inverse companion of
#' [set_col_header()] and pairs with [rtf_columns()] (which lists the final
#' column names).
#'
#' @details
#' The output is name-based (cell positions are written as column names, which
#' survive reordering), keeps the header's empty gap cells, and backtick-quotes
#' non-syntactic column names.  Per-cell text decorations and borders are
#' reproduced faithfully.
#'
#' `add_span_level = TRUE` previews **adding a second hierarchy level**: the
#' `stub` column(s) stay as single (empty) cells and every other column is
#' bundled under one empty spanning cell whose label you fill in -- a quick
#' scaffold for turning a one-row header into a grouped, two-row header.
#'
#' @param x An [rtftable()], or a list of them (pages from [as_rtftables()];
#'   the first page is used).
#' @param level Verbosity of the emitted cells: `"explicit"` (default -- only
#'   fields that differ from the defaults), `"default"` (also show each cell's
#'   effective `align` and default border widths, but not the `FALSE`
#'   decoration flags), or `"all"` (everything, including `bold = FALSE` etc.).
#' @param snippet Logical (default `TRUE`).  When `TRUE`, wrap the header in a
#'   pipeable `set_col_header(...)` call that also reproduces the header text
#'   alignment (from `col_spec`) and the header-related zone borders
#'   (`header` / `spanning`, via `style_zone()`).  There is no leading
#'   `tbl |>` -- the first argument is meant to arrive through the pipe (e.g.
#'   `my_tbl |> ` followed by the snippet).  When `FALSE`, return the bare
#'   `rtf_col_header(...)` value.
#' @param add_span_level Logical (default `FALSE`).  When `TRUE`, prepend a
#'   scaffold spanning row grouping the non-`stub` columns (see *Details*).
#' @param stub Column(s) to keep un-spanned when `add_span_level = TRUE`:
#'   integer position(s) and/or column name(s).  Default `1` (the first column,
#'   where `as_rtftables(stub_vars = )` places the stub).
#'
#' @return A single character string (the source).  Use `cat()` to print it
#'   with the line breaks rendered.
#'
#' @seealso [set_col_header()] to apply an edited header; [rtf_columns()] for
#'   the final column names; [rtf_col_header()] / [col_cell()] for the pieces.
#'
#' @examples
#' tbl <- rtftable(
#'   data.frame(row_label = "x", g1 = 1, g2 = 2, Total = 3),
#'   col_header = c("Category", "Low", "High", "Total")
#' )
#' cat(rtf_header_source(tbl, snippet = FALSE))
#'
#' # Preview adding a spanning level over the non-stub columns:
#' cat(rtf_header_source(tbl, snippet = FALSE, add_span_level = TRUE))
#' @export
rtf_header_source <- function(x,
                              level = c("explicit", "default", "all"),
                              snippet = TRUE,
                              add_span_level = FALSE,
                              stub = 1) {
  level <- match.arg(level)
  if (inherits(x, "rtftable")) {
    tbl <- x
  } else if (is.list(x) && length(x) > 0L && inherits(x[[1L]], "rtftable")) {
    tbl <- x[[1L]]
  } else {
    stop("`rtf_header_source()` expects an rtftable or a list of rtftable pages (as returned by as_rtftables()).",
         call. = FALSE)
  }

  ref <- if (!is.null(tbl$data)) tbl$data else tbl$data_list[[1L]]
  cn  <- names(ref)
  col_spec <- tbl$col_spec

  rows <- tbl$col_header
  if (is.null(rows) && !is.null(tbl$col_header_list)) rows <- tbl$col_header_list[[1L]]
  # No explicit header: the table renders names(data) as one label row.
  if (is.null(rows) || length(rows) == 0L) rows <- list(as.character(cn))

  if (isTRUE(add_span_level)) {
    stub_idx <- if (is.character(stub)) match(stub, cn) else as.integer(stub)
    if (anyNA(stub_idx) || any(stub_idx < 1L) || any(stub_idx > length(cn))) {
      stop("`stub` must be valid column name(s) or position(s) in 1..",
           length(cn), ".", call. = FALSE)
    }
    rows <- c(list(.hs_scaffold_row(length(cn), stub_idx)), rows)
  }

  hdr <- .hs_col_header(rows, cn, col_spec, level)
  if (!isTRUE(snippet)) return(hdr)

  al <- .hs_align_line(col_spec, level)
  sc <- paste0("set_col_header(\n  ", hdr,
               if (!is.null(al)) paste0(",\n  ", al), "\n)")
  zl <- .hs_zone_line(tbl$border, level)
  if (!is.null(zl)) sc <- paste0(sc, " |>\n  ", zl)
  sc
}

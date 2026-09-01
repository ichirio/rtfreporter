# rtf_border.R -- Border specifications (all S3)
#
# Three constructor functions build border specifications used in
# rtf_header(), rtf_footer(), and rtftable():
#
#   rtf_border_side   -- one edge (style, width, color)
#   rtf_border        -- four edges of a single cell/row
#   rtf_table_border  -- per-zone borders for a full table
#
# All three are plain S3 lists with a class attribute.  Reference semantics
# are intentionally absent: every value is a pure record that travels with
# copy semantics, so passing a border into multiple tables is always safe.


# -- Internal helpers -----------------------------------------------------------

.valid_border_styles <- c("single", "double", "thick", "dash", "dot")

# Deprecation warnings fire once per session: a table-building loop would
# otherwise repeat the same paragraph for every table.  The environment is
# package-local, so the state dies with the session.
.deprecation_state <- new.env(parent = emptyenv())

.deprecate_once <- function(key, msg) {
  if (isTRUE(.deprecation_state[[key]])) return(invisible(FALSE))
  .deprecation_state[[key]] <- TRUE
  warning(msg, call. = FALSE)
  invisible(TRUE)
}

# The pre-0.5 reading of an rtf_border was uniform: `left`/`right` reached every
# cell of a row and a multi-row zone's `top`/`bottom` reached every one of its
# rows.  Both now mean the selection's outer edge only.  The two readings are
# spelled identically, so the only honest signal is "edges set, matching
# inside_* never named" -- and it is worth raising only where the readings
# actually differ, which is why ncols / nrows are needed.
.warn_old_edge_reading <- function(tb, ncols, nrows) {
  if (is.null(tb)) return(invisible(FALSE))
  differs <- function(b, multi_row) {
    if (!inherits(b, "rtf_border")) return(FALSE)
    named <- attr(b, "inside_named") %||% c(h = FALSE, v = FALSE)
    v <- ncols > 1L && !isTRUE(named[["v"]]) &&
         (!is.null(b$left) || !is.null(b$right))
    h <- multi_row && nrows > 1L && !isTRUE(named[["h"]]) &&
         (!is.null(b$top) || !is.null(b$bottom))
    v || h
  }
  # `header` has always read top/bottom as the block's outer edges, so only its
  # vertical axis can have changed; `first_row` / `last_row` are single rows.
  # `outer` is deliberately not checked: rtftable(border = <rtf_border>) was an
  # error before 0.5.0 (#326), so no existing code can mean the old thing by it.
  hit <- differs(tb$body, TRUE) || differs(tb$header, FALSE) ||
         differs(tb$spanning, FALSE) || differs(tb$first_row, FALSE) ||
         differs(tb$last_row, FALSE)
  if (!hit) return(invisible(FALSE))
  .deprecate_once(
    "border_edge_reading",
    paste0(
      "The edges of `rtf_border()` now mean the *outer* edges of whatever you ",
      "attach it to.\n",
      "  Before 0.5.0 `left`/`right` were drawn on every cell of a row, and a ",
      "multi-row zone's\n  `top`/`bottom` on every one of its rows.  ",
      "To keep that look, name the interior rule:\n",
      "    rtf_border(left = s, right = s, inside_v = s)   # was: left/right ",
      "alone\n",
      "    rtf_border(bottom = s, inside_h = s)            # was: bottom alone ",
      "on a body zone\n",
      "  Naming `inside_h` / `inside_v` (with `rtf_border_side(\"none\")` for ",
      "\"no rule\") silences this.\n",
      "  See ?rtf_border, section \"Writing borders before and after 0.5.0\", ",
      "for the old and new spelling of each case."))
}

# The four physical edges an RTF cell can carry.
.border_edges <- c("top", "bottom", "left", "right")

# Plus the two "between" axes.  These are not edges: they say what the rules
# *between* a row's cells (inside_v) and *between* a zone's rows (inside_h)
# look like.  RTF has no such concept -- the renderer distributes them onto
# real edges of the right cells.
.border_slots <- c(.border_edges, "inside_h", "inside_v")

# The five row kinds an rtf_table_border addresses.
.table_border_zones <- c("header", "spanning", "body", "first_row", "last_row")

.check_border_side <- function(x, arg = deparse(substitute(x))) {
  if (!is.null(x) && !inherits(x, "rtf_border_side")) {
    stop(sprintf("`%s` must be NULL or an rtf_border_side object.", arg), call. = FALSE)
  }
}

.check_hex_color <- function(color) {
  if (is.null(color)) return(invisible(NULL))
  if (!is.character(color) || length(color) != 1L ||
      !grepl("^#[0-9A-Fa-f]{6}$", color)) {
    stop("`color` must be NULL or a 6-digit hex color string (e.g. \"#FF0000\").",
         call. = FALSE)
  }
}


# -- rtf_border_side ------------------------------------------------------------

#' Single-edge border specification
#'
#' Defines the line style, weight, and colour for one edge of a cell.
#' Use this as an argument to [rtf_border()].
#'
#' @param style Line style.  One of `"single"` (default), `"double"`,
#'   `"thick"`, `"dash"`, `"dot"`, or `"none"`.  Use `"none"` to build an
#'   *explicit no-line* side: unlike `NULL` (which simply leaves a side
#'   unset), a `"none"` side **overrides** any inherited border when it is
#'   merged on top of another spec.  This is how a per-cell border can
#'   remove an automatically-drawn rule -- e.g. suppressing the group
#'   underline under one spanning column-header cell.
#' @param width Line weight in twips.  Default `15` ≈ 0.5 pt.  Ignored when
#'   `style = "none"`.
#' @param color Line colour.  `NULL` (default) = black.  Or a 6-digit hex
#'   string such as `"#003366"`.
#'
#' @return A list of class `"rtf_border_side"`.
#'
#' @seealso [rtf_border()] to assemble sides into a cell border, and
#'   [rtf_table_border()] for whole-table border zones.
#'
#' @examples
#' rtf_border_side()                                   # thin black rule (~0.5 pt)
#' rtf_border_side(style = "double", width = 30L, color = "#003366")
#' rtf_border_side("none")   # explicit "no line" that removes an inherited rule
#' @export
rtf_border_side <- function(style = "single", width = 15L, color = NULL) {
  style <- match.arg(style, c(.valid_border_styles, "none"))
  width <- as.integer(width)
  if (!identical(style, "none") && width < 1L) {
    stop("`width` must be a positive integer (twips).", call. = FALSE)
  }
  .check_hex_color(color)
  structure(
    list(style = style, width = width, color = color),
    class = "rtf_border_side"
  )
}

#' @export
print.rtf_border_side <- function(x, ...) {
  col_str <- if (!is.null(x$color)) paste0(", color=", x$color) else ""
  cat(sprintf("<rtf_border_side: %s, %d twips%s>\n", x$style, x$width, col_str))
  invisible(x)
}


# -- rtf_border ----------------------------------------------------------------

#' Four-edge border specification for a cell or row
#'
#' Specifies borders for up to four sides (top, bottom, left, right).  Each
#' side is either `NULL` (no border) or an [rtf_border_side()] object.
#'
#' To derive a new border from an existing one, use [rtf_border_with()].
#'
#' @section What a border applies to:
#'
#' An `rtf_border` describes a **selection**, and *where* it applies is decided
#' by where you attach it -- the same way Word's border dialog acts on whatever
#' is selected:
#'
#' \tabular{ll}{
#'   `rtftable(border = )`                  \tab the whole table \cr
#'   `style_zone(header = , body = , ...)`  \tab that kind of row \cr
#'   `col_cell(border = )`, `cell_styles`   \tab one cell \cr
#'   `rtf_header(border = )` / `rtf_footer(border = )` \tab that block
#' }
#'
#' There is one rule, with no special cases:
#'
#' * `top` / `bottom` / `left` / `right` are the selection's **outer** edges.
#' * `inside_h` / `inside_v` are the rules **inside** it, and an absent one
#'   means no rule there.
#'
#' So an edge never means something different depending on which other
#' arguments are present.  On a row containing a spanning cell, `inside_v`
#' lands on **cell** boundaries rather than column boundaries: nothing is drawn
#' inside a merged cell.  A single cell has no inside, so both are ignored
#' there.
#'
#' @section Writing borders before and after 0.5.0:
#'
#' Two things changed at 0.5.0.  [rtf_table_border()] is deprecated, so a
#' border is aimed with `rtftable(border = )` or [style_zone()] instead; and an
#' edge now always means the **outer** edge of the selection, so the rules
#' *inside* it have to be asked for by name.  Each case below, `s` being an
#' [rtf_border_side()]:
#'
#' \preformatted{
#'   ## rules above and below the column header  (unchanged in meaning)
#'   was:  rtftable(df, border = rtf_table_border(
#'                    header = rtf_border(top = s, bottom = s)))
#'   now:  rtftable(df, border = "none") |>
#'           style_zone(header = rtf_border(top = s, bottom = s))
#'
#'   ## a rule under the last data row                      (unchanged)
#'   was:  rtf_table_border(last_row = rtf_border(bottom = s))
#'   now:  style_zone(last_row = rtf_border(bottom = s))
#'
#'   ## a rule under EVERY data row              (bottom -> inside_h)
#'   was:  rtf_table_border(body = rtf_border(bottom = s))
#'   now:  style_zone(body = rtf_border(inside_h = s))
#'
#'   ## a vertical rule at every column boundary  (left/right -> inside_v)
#'   was:  rtf_table_border(body = rtf_border(left = s, right = s))
#'   now:  style_zone(body = rtf_border(left = s, right = s, inside_v = s))
#'
#'   ## a grid around every data cell
#'   was:  rtf_table_border(body = rtf_border_box())
#'   now:  style_zone(body = rtf_border(top = s, bottom = s, left = s,
#'                                      right = s, inside_h = s, inside_v = s))
#'
#'   ## an outer frame only, no rules inside      (was not expressible)
#'   now:  rtftable(df, border = rtf_border(top = s, bottom = s,
#'                                          left = s, right = s))
#'
#'   ## frame plus a rule under every row -- the listing look
#'   was:  four style_header() / style_body() calls on the edge columns
#'   now:  rtftable(df, border = rtf_border(top = s, bottom = s, left = s,
#'                                          right = s, inside_h = s))
#' }
#'
#' `border = "tfl"` and [rtf_border_tfl()] are unaffected, as is any border on
#' a single cell ([col_cell()], `cell_styles`): a cell has no inside, so its
#' four edges never meant anything else.
#'
#' The old and new readings are spelled identically, so rtfreporter warns once
#' per session when it meets a border that would have rendered differently
#' before.  Naming `inside_h` / `inside_v` says which you mean and silences it
#' -- use `rtf_border_side("none")` for "no rule there".
#'
#' @param top,bottom,left,right `NULL` (no border on that side) or an
#'   [rtf_border_side()] object.
#' @param inside_h,inside_v `NULL` (default, meaning no rule) or an
#'   [rtf_border_side()]: the rule drawn *between* the selection's rows
#'   (`inside_h`) and *between* its cells (`inside_v`).
#'
#' @return A list of class `"rtf_border"`.
#'
#' @examples
#' rtf_border(top = rtf_border_side(), bottom = rtf_border_side())  # top + bottom
#' rtf_border(bottom = rtf_border_side(color = "#003366"))          # blue underline
#'
#' # A whole table: frame plus a rule under every row, no vertical rules.
#' s <- rtf_border_side()
#' rtf_border(top = s, bottom = s, left = s, right = s, inside_h = s)
#'
#' # A rule between the cells but no outer edges.
#' rtf_border(inside_v = s)
#' @export
rtf_border <- function(top = NULL, bottom = NULL, left = NULL, right = NULL,
                       inside_h = NULL, inside_v = NULL) {
  .check_border_side(top,      "top")
  .check_border_side(bottom,   "bottom")
  .check_border_side(left,     "left")
  .check_border_side(right,    "right")
  .check_border_side(inside_h, "inside_h")
  .check_border_side(inside_v, "inside_v")
  b <- structure(
    list(top = top, bottom = bottom, left = left, right = right,
         inside_h = inside_h, inside_v = inside_v),
    class = "rtf_border"
  )
  # Not part of the value, only of its provenance: it lets rtftable() tell a
  # border written for the pre-0.5 uniform reading from one written for the
  # current outer/inside reading, and warn about the first (see
  # .warn_old_edge_reading()).  Dropped by any merge, which is what we want --
  # a merged border is a new statement.
  attr(b, "inside_named") <- c(h = !missing(inside_h), v = !missing(inside_v))
  b
}

#' @export
print.rtf_border <- function(x, ...) {
  cat("<rtf_border>\n")
  for (s in .border_slots) {
    v <- x[[s]]
    # inside_h / inside_v are optional, so stay quiet about them when unset:
    # a plain four-edge border prints exactly as it always has.
    if (is.null(v)) {
      if (s %in% c("inside_h", "inside_v")) next
      cat(sprintf("  %-8s: none\n", s))
    } else {
      col_str <- if (!is.null(v$color)) paste0(", color=", v$color) else ""
      cat(sprintf("  %-8s: %s, %d twips%s\n", s, v$style, v$width, col_str))
    }
  }
  invisible(x)
}

#' Return a copy of an `rtf_border` with selected sides replaced
#'
#' Non-mutating: returns a new `rtf_border` with the supplied side(s) set on
#' top of `border`.  `NULL` arguments leave the corresponding side unchanged.
#'
#' @param border An [rtf_border()] object.  `NULL` is accepted and treated as
#'   an empty border.
#' @param top,bottom,left,right Replacement [rtf_border_side()] values, or
#'   `NULL` to leave a side unchanged.
#' @param inside_h,inside_v Replacement [rtf_border_side()] values for the
#'   between-rows and between-cells rules, or `NULL` to leave them unchanged.
#'
#' @return A new `rtf_border` object.
#'
#' @examples
#' b <- rtf_border(top = rtf_border_side(), bottom = rtf_border_side())
#' rtf_border_with(b, bottom = rtf_border_side(color = "#003366"))  # recolour bottom
#' rtf_border_with(b, top = rtf_border_side("none"))                # drop the top rule
#' @export
rtf_border_with <- function(border, top = NULL, bottom = NULL,
                            left = NULL, right = NULL,
                            inside_h = NULL, inside_v = NULL) {
  if (is.null(border)) border <- rtf_border()
  if (!inherits(border, "rtf_border")) {
    stop("`border` must be NULL or an rtf_border object.", call. = FALSE)
  }
  repl <- list(top = top, bottom = bottom, left = left, right = right,
               inside_h = inside_h, inside_v = inside_v)
  for (slot in .border_slots) {
    .check_border_side(repl[[slot]], slot)
    if (!is.null(repl[[slot]])) border[[slot]] <- repl[[slot]]
  }
  border
}


# -- Convenience rtf_border constructors ----------------------------------------

#' @describeIn rtf_border All sides `NULL` (explicit "no border").
#' @export
rtf_border_none <- function() rtf_border()

#' @describeIn rtf_border Top edge only.
#' @param style,width,color Passed to [rtf_border_side()].
#' @export
rtf_border_top <- function(style = "single", width = 15L, color = NULL) {
  rtf_border(top = rtf_border_side(style, width, color))
}

#' @describeIn rtf_border Bottom edge only.
#' @export
rtf_border_bottom <- function(style = "single", width = 15L, color = NULL) {
  rtf_border(bottom = rtf_border_side(style, width, color))
}

#' @describeIn rtf_border All four edges.
#' @export
rtf_border_box <- function(style = "single", width = 15L, color = NULL) {
  s <- rtf_border_side(style, width, color)
  rtf_border(top = s, bottom = s, left = s, right = s)
}


# -- rtf_table_border ----------------------------------------------------------

#' Per-zone border specification for a table
#'
#' Specifies borders for each logical zone of an [rtftable()].
#' Each zone is either `NULL` (no border) or an [rtf_border()] object.
#' `first_row` and `last_row` are *overrides* merged on top of the `body` spec.
#'
#' @section Deprecated since 0.5.0:
#'
#' A border is now written once with [rtf_border()], and *where* it applies is
#' decided by where you attach it:
#'
#' \preformatted{
#'   rtftable(border = rtf_border(...))                    # the whole table
#'   style_zone(header = rtf_border(...), body = ...)      # one kind of row
#'   col_cell(border = rtf_border(...))                    # one cell
#' }
#'
#' `outer =` becomes the four edges of that `rtf_border()`; `inside_h =` and
#' `inside_v =` keep their names.  This function still works and still returns
#' the same value, but warns once per session.
#'
#' @inheritSection rtf_border Writing borders before and after 0.5.0
#'
#' @param header    [rtf_border()] for column-header rows.  `NULL` = none.
#' @param spanning  [rtf_border()] for spanning-header rows.  `NULL` = none.
#' @param body      [rtf_border()] for data rows.  `NULL` = none.
#' @param first_row [rtf_border()] override for the first data row.
#' @param last_row  [rtf_border()] override for the last data row.
#' @param outer     [rtf_border()] for the table's four outermost edges, or
#'   `NULL` (default).  See the section above.
#' @param inside_h,inside_v [rtf_border_side()] for the rules between rows and
#'   between cells, or `NULL` (default).
#'
#' @return A list of class `"rtf_table_border"`.
#'
#' @seealso [rtf_border()] / [rtf_border_side()] for the pieces; pass the result
#'   as `rtftable(border = )`.
#'
#' @examples
#' s  <- rtf_border_side()
#' df <- data.frame(A = c("1", "2"), B = c("x", "y"))
#'
#' # The clinical TFL look, spelled out one row kind at a time.
#' rtftable(df, border = "none") |>
#'   style_zone(header   = rtf_border(top = s, bottom = s),
#'              last_row = rtf_border(bottom = s))
#'
#' # The listing look: outer frame plus a rule under every row, and no vertical
#' # rules between the columns.
#' rtftable(df, border = rtf_border(top = s, bottom = s, left = s, right = s,
#'                                  inside_h = s))
#'
#' # A full grid: add the rules between the cells.
#' rtftable(df, border = rtf_border(top = s, bottom = s, left = s, right = s,
#'                                  inside_h = s, inside_v = s))
#'
#' # Deprecated: the same thing written the old way.
#' \dontrun{
#' rtf_table_border(header = rtf_border(top = s, bottom = s))
#' }
#' @export
rtf_table_border <- function(header    = NULL,
                              spanning  = NULL,
                              body      = NULL,
                              first_row = NULL,
                              last_row  = NULL,
                              outer     = NULL,
                              inside_h  = NULL,
                              inside_v  = NULL) {
  .deprecate_once(
    "rtf_table_border",
    paste0(
      "`rtf_table_border()` is deprecated: a border is now written once with ",
      "`rtf_border()`, and *where* it applies is decided by where you attach ",
      "it.\n",
      "  whole table : rtftable(border = rtf_border(...))\n",
      "  one row kind: style_zone(header = rtf_border(...), body = ...)\n",
      "  one cell    : col_cell(border = rtf_border(...))\n",
      "  `outer =` becomes the four edges of that rtf_border(); `inside_h =` ",
      "and `inside_v =` keep their names.\n",
      "  See ?rtf_border, section \"Writing borders before and after 0.5.0\", ",
      "for the old and new spelling of each case."))
  .rtf_table_border(header = header, spanning = spanning, body = body,
                    first_row = first_row, last_row = last_row,
                    outer = outer, inside_h = inside_h, inside_v = inside_v)
}

# The five-zone map, unexported.  It stays the renderer's internal vocabulary;
# users address a row kind with style_zone() or rtf_table_style(border_* = ).
.rtf_table_border <- function(header    = NULL,
                              spanning  = NULL,
                              body      = NULL,
                              first_row = NULL,
                              last_row  = NULL,
                              outer     = NULL,
                              inside_h  = NULL,
                              inside_v  = NULL) {
  zones <- list(header = header, spanning = spanning, body = body,
                first_row = first_row, last_row = last_row)
  for (nm in names(zones)) {
    v <- zones[[nm]]
    if (!is.null(v) && !inherits(v, "rtf_border")) {
      stop(sprintf("`%s` must be NULL or an rtf_border object.", nm), call. = FALSE)
    }
  }
  if (!is.null(outer) && !inherits(outer, "rtf_border")) {
    stop("`outer` must be NULL or an rtf_border object (the table's four ",
         "outermost edges).", call. = FALSE)
  }
  .check_border_side(inside_h, "inside_h")
  .check_border_side(inside_v, "inside_v")
  structure(
    c(zones, list(outer = outer, inside_h = inside_h, inside_v = inside_v)),
    class = "rtf_table_border"
  )
}

# -- Whole-table shortcuts -> the five zones ------------------------------------
#
# `outer` / `inside_h` / `inside_v` are a "selection = the whole table" way of
# saying what the five zones say row-kind by row-kind.  They are expanded here,
# once, before the renderer ever sees them, so the renderer keeps its single
# vocabulary (zones carrying rtf_border objects).
#
# Expansion, with `has_header` telling us whether a column-header block exists:
#
#   outer$top    -> the top edge of the topmost row      (header, else first_row)
#   outer$bottom -> the bottom edge of the last data row (last_row)
#   outer$left / outer$right
#                -> every zone's left/right.  Those are per-cell in RTF, so the
#                   renderer needs to know they are OUTER edges: that is what
#                   setting `inside_v` on each zone does (see .cell_edge_border).
#                   With no inside_v asked for, an explicit "none" is planted so
#                   the interior boundaries stay clear.
#   inside_h     -> every row boundary inside the table: between header rows,
#                   between the header block and the body, and between data rows
#   inside_v     -> every cell boundary inside a row
#
# Explicit zone arguments are merged on top afterwards and therefore win, which
# keeps "whole table, except ..." expressible.
.expand_table_border <- function(tb, has_header = TRUE) {
  if (is.null(tb)) return(NULL)
  outer <- tb$outer
  ih    <- tb$inside_h
  iv    <- tb$inside_v
  if (is.null(outer) && is.null(ih) && is.null(iv)) return(tb)

  vertical <- function(b) {
    if (is.null(outer) && is.null(iv)) return(b)
    rtf_border_with(b %||% rtf_border(),
                    left = outer$left, right = outer$right, inside_v = iv)
  }

  base <- vector("list", length(.table_border_zones))
  names(base) <- .table_border_zones
  base$header    <- vertical(if (!is.null(ih)) rtf_border(inside_h = ih) else NULL)
  base$body      <- vertical(if (!is.null(ih)) rtf_border(inside_h = ih) else NULL)
  # `spanning` REPLACES `header` on spanning rows rather than layering over it,
  # so filling it in unconditionally would strip the frame off the topmost row.
  # Leave it NULL (the renderer then falls back to `header`) unless the caller
  # named it, in which case their value layers over the header expansion.
  base$spanning  <- if (is.null(tb$spanning)) NULL else base$header
  base$first_row <- NULL
  base$last_row  <- NULL

  # The table's own top and bottom edges.
  if (!is.null(outer$top)) {
    if (has_header) {
      base$header <- rtf_border_with(base$header %||% rtf_border(), top = outer$top)
    } else {
      base$first_row <- rtf_border_with(base$first_row %||% rtf_border(),
                                        top = outer$top)
    }
  }
  if (!is.null(outer$bottom)) {
    base$last_row <- rtf_border_with(base$last_row %||% rtf_border(),
                                     bottom = outer$bottom)
  }

  # The header/body seam is an *inside* horizontal rule, not an outer one.
  if (!is.null(ih) && has_header) {
    base$header <- rtf_border_with(base$header %||% rtf_border(), bottom = ih)
  }

  for (z in .table_border_zones) {
    tb[[z]] <- .merge_rtf_border(base[[z]], tb[[z]])
  }
  tb$outer    <- NULL
  tb$inside_h <- NULL
  tb$inside_v <- NULL
  tb
}

#' @export
print.rtf_table_border <- function(x, ...) {
  cat("<rtf_table_border>\n")
  for (zone in .table_border_zones) {
    if (!is.null(x[[zone]])) {
      sides <- vapply(.border_slots, function(s) {
        b <- x[[zone]][[s]]
        if (is.null(b)) "none"
        else paste0(b$style, "/", b$width, if (!is.null(b$color)) paste0("/", b$color) else "")
      }, character(1L))
      extra <- if (sides[5] != "none" || sides[6] != "none")
                 sprintf(" IH=%s IV=%s", sides[5], sides[6]) else ""
      cat(sprintf("  %-10s: T=%s B=%s L=%s R=%s%s\n",
                  zone, sides[1], sides[2], sides[3], sides[4], extra))
    } else {
      cat(sprintf("  %-10s: none\n", zone))
    }
  }
  if (!is.null(x$outer))    cat("  outer     : <rtf_border>\n")
  if (!is.null(x$inside_h)) cat(sprintf("  inside_h  : %s, %d twips\n",
                                      x$inside_h$style, x$inside_h$width))
  if (!is.null(x$inside_v)) cat(sprintf("  inside_v  : %s, %d twips\n",
                                      x$inside_v$style, x$inside_v$width))
  invisible(x)
}


# -- TFL preset ----------------------------------------------------------------

#' Clinical TFL-style table border preset
#'
#' Returns an [rtf_table_border()] matching the standard clinical TFL style:
#' **borders are applied to the column-header block only**, with no
#' borders in the data area by default.  Specifically:
#'
#' * `header$top`    -- top border on the topmost header row
#' * `header$bottom` -- bottom border on the bottommost header row
#' * A multi-column spanning cell additionally receives a bottom border
#'   (group underline) **only where the column grouping changes below it**
#'   -- that is, when the next header row subdivides the columns the span
#'   covers.  A span repeated unchanged on the following row is not
#'   underlined.  This is added automatically by the renderer.
#' * No vertical lines.
#' * **No borders on the data section** (`body` / `first_row` /
#'   `last_row` all `NULL`).  Callers who want a bottom rule under the
#'   last data row can set it explicitly:
#'   `rtf_table_border(last_row = rtf_border(bottom = rtf_border_side()))`.
#'
#' @inheritParams rtf_border_side
#' @return An `rtf_table_border` object.
#'
#' @examples
#' rtf_border_tfl()                         # the standard clinical TFL rules
#' rtf_border_tfl(width = 30L)              # heavier rules
#' rtftable(data.frame(A = 1:2), border = rtf_border_tfl())
#' @export
rtf_border_tfl <- function(style = "single", width = 15L, color = NULL) {
  s <- rtf_border_side(style, width, color)
  .rtf_table_border(
    header   = rtf_border(top = s, bottom = s),
    spanning = NULL,
    body     = NULL,
    first_row = NULL,
    last_row  = NULL
  )
}


# -- Internal conversion helpers -----------------------------------------------

# Convert old plain-list border spec (used by rtftable before the class redesign)
# to rtf_table_border.  The old spec had keys: header, spanning, body,
# first_row, last_row, each a list with top/bottom/left/right string + width.
.plain_list_to_table_border <- function(lst) {
  zones <- c("header", "spanning", "body", "first_row", "last_row")
  result <- vector("list", length(zones))
  names(result) <- zones

  for (zone in zones) {
    spec <- lst[[zone]]
    if (is.null(spec) || length(spec) == 0L) next
    width <- as.integer(spec$width %||% 15L)
    sides <- list()
    for (side in c("top", "bottom", "left", "right")) {
      st <- spec[[side]]
      if (!is.null(st) && !st %in% c("none", "")) {
        sides[[side]] <- rtf_border_side(st, width)
      }
    }
    if (length(sides) > 0L) {
      result[[zone]] <- do.call(rtf_border, sides)
    }
  }
  do.call(.rtf_table_border, result)
}

# Merge two rtf_border objects: override sides of `base` with non-NULL sides
# of `over`.  Both arguments are S3 lists; R's copy-on-modify semantics mean
# the caller's `base` is never mutated.
.merge_rtf_border <- function(base, over) {
  if (is.null(base)) return(over)
  if (is.null(over) || length(over) == 0L) return(base)
  for (side in .border_slots) {
    if (!is.null(over[[side]])) base[[side]] <- over[[side]]
  }
  base
}

# Collect all hex colors from an rtf_border (or NULL).
.collect_border_colors <- function(b) {
  if (is.null(b)) return(character(0))
  cols <- character(0)
  for (side in .border_slots) {
    c2 <- b[[side]]$color
    if (!is.null(c2)) cols <- c(cols, c2)
  }
  cols
}

# Collect all hex colors from an rtf_table_border (or NULL).
.collect_table_border_colors <- function(tb) {
  if (is.null(tb)) return(character(0))
  cols <- character(0)
  for (zone in .table_border_zones) {
    cols <- c(cols, .collect_border_colors(tb[[zone]]))
  }
  cols <- c(cols, .collect_border_colors(tb$outer))
  for (slot in c("inside_h", "inside_v")) {
    c2 <- tb[[slot]]$color
    if (!is.null(c2)) cols <- c(cols, c2)
  }
  cols
}

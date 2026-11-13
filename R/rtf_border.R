# rtf_border.R -- Border specifications (all S3)
#
# Three constructor functions build border specifications used in
# rtf_header(), rtf_footer(), and rtftable():
#
#   rtf_border_line   -- one edge (style, width, color)
#   rtf_border        -- four edges of a single cell/row
#   rtf_table_border  -- per-zone borders for a full table (internal:
#                        .rtf_table_border(); the exported constructor was
#                        removed in 0.9.0)
#
# All three are plain S3 lists with a class attribute.  Reference semantics
# are intentionally absent: every value is a pure record that travels with
# copy semantics, so passing a border into multiple tables is always safe.


# -- Internal helpers -----------------------------------------------------------

.valid_border_styles <- c("single", "double", "thick", "dash", "dot")

# Deprecation warnings fire once per session: a table-building loop would
# otherwise repeat the same paragraph for every table.  The environment is
# package-local, so the state dies with the session.
# 0.9.0 removed the deprecated exports (0.5.0 - 0.8.x, #594, #788, and
# rtf_default_format(), #804, #975): none is deprecated now.

.deprecation_state <- new.env(parent = emptyenv())

.deprecate_once <- function(key, msg) {
  if (isTRUE(.deprecation_state[[key]])) return(invisible(FALSE))
  .deprecation_state[[key]] <- TRUE
  warning(msg, call. = FALSE)
  invisible(TRUE)
}

# A warning that is not a deprecation, said once a session the same way
.warn_once <- function(key, msg) .deprecate_once(paste0("warn:", key), msg)

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
      "  Naming `inside_h` / `inside_v` (with `rtf_border_line(\"none\")` for ",
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
  if (!is.null(x) && !inherits(x, c("rtf_border_line", "rtf_border_side"))) {
    stop(sprintf("`%s` must be NULL or an rtf_border_line object.", arg), call. = FALSE)
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


# -- rtf_border_line ------------------------------------------------------------

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
#' @param width Line weight in twips.  Default `15` (about 0.5 pt).  Ignored when
#'   `style = "none"`.
#' @param color Line colour.  `NULL` (default) = black.  Or a 6-digit hex
#'   string such as `"#003366"`.
#'
#' `rtf_border_side()` was its old name, removed in 0.9.0.
#'
#' @return A list of class `"rtf_border_line"`.
#'
#' @seealso [rtf_border()] to assemble sides into a border, attached to
#'   `rtftable(border = )` for the whole table or to [style_zone()] for one
#'   kind of row.
#'
#' @examples
#' TRUE                                   # thin black rule (~0.5 pt)
#' rtf_border_line(style = "double", width = 30L, color = "#003366")
#' "none"   # explicit "no line" that removes an inherited rule
#' @export
rtf_border_line <- function(style = "single", width = 15L, color = NULL) {
  style <- match.arg(style, c(.valid_border_styles, "none"))
  width <- as.integer(width)
  if (!identical(style, "none") && width < 1L) {
    stop("`width` must be a positive integer (twips).", call. = FALSE)
  }
  .check_hex_color(color)
  structure(
    list(style = style, width = width, color = color),
    class = "rtf_border_line"
  )
}

# Resolve whatever a caller wrote for one side into an rtf_border_line.
#
#   NULL            -> unset (inherit)
#   TRUE            -> a default rule: single, 15 twips, black
#   FALSE           -> an explicit "no line", same as "none"
#   "double" etc.   -> that style, at the default weight and colour
#   rtf_border_line -> taken as-is, for weight and colour of its own
#
# The first four are shorthands for the fifth; a line's *type* is what gets
# named most often, so it should not need a constructor.
.as_border_side <- function(x, arg) {
  if (is.null(x)) return(NULL)
  # (an object saved before 0.8.2.9013 carries the old class name)
  if (inherits(x, c("rtf_border_line", "rtf_border_side"))) return(x)
  if (isTRUE(x))  return(rtf_border_line())
  if (isFALSE(x)) return(rtf_border_line("none"))
  if (is.character(x) && length(x) == 1L && !is.na(x)) {
    ok <- c(.valid_border_styles, "none")
    if (!x %in% ok) {
      stop(sprintf("`%s` must be one of %s, not \"%s\".",
                   arg, paste(dQuote(ok, q = FALSE), collapse = ", "), x),
           call. = FALSE)
    }
    return(rtf_border_line(x))
  }
  stop(sprintf(
    "`%s` must be TRUE, FALSE, a border style name, an rtf_border_line(), ",
    arg), "or NULL.", call. = FALSE)
}

#' @export
print.rtf_border_line <- function(x, ...) {
  col_str <- if (!is.null(x$color)) paste0(", color=", x$color) else ""
  cat(sprintf("<rtf_border_line: %s, %d twips%s>\n", x$style, x$width, col_str))
  invisible(x)
}


# -- rtf_border ----------------------------------------------------------------

#' Four-edge border specification for a cell or row
#'
#' Specifies borders for up to four sides (top, bottom, left, right).  Each
#' side is either `NULL` (no border) or a border side.
#'
#' To add to a border already in place, attach a second one where the first
#' is: the places it attaches to merge side by side (see *Building versus
#' layering*).
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
#' @section Writing a side:
#'
#' Every side takes a value from one of two families. Stay inside one family
#' within a call and it reads evenly:
#'
#' \preformatted{
#'   logical     rtf_border(top = TRUE,     bottom = FALSE)
#'   style name  rtf_border(top = "single", bottom = "none")
#' }
#'
#' The two agree: `TRUE` is `rtf_border_line()` with its defaults (a single
#' rule), and `FALSE` is `"none"` -- an explicit *no line*, which erases a rule the
#' selection would otherwise inherit. The style names are `"single"`,
#' `"double"`, `"thick"`, `"dash"`, `"dot"` and `"none"`.
#'
#' Both are shorthands for the third spelling, an [rtf_border_line()], which is
#' what a side actually holds. Reach for it when a line needs a weight or a
#' colour of its own -- and since each side carries its own, one call is always
#' enough:
#'
#' \preformatted{
#'   rtf_border(all = "double")                              # four edges alike
#'   rtf_border(top = rtf_border_line(color = "#C9372C"),    # ... or all four
#'              bottom = rtf_border_line("double", 30L))     #     different
#' }
#'
#' `FALSE` and `"none"` are always interchangeable.
#'
#' So an edge never means something different depending on which other
#' arguments are present.  On a row containing a spanning cell, `inside_v`
#' lands on **cell** boundaries rather than column boundaries: nothing is drawn
#' inside a merged cell.  A single cell has no inside, so both are ignored
#' there.
#'
#' @section Building versus layering:
#'
#' A side is in one of three states, and all three matter:
#'
#' \describe{
#'   \item{unset (`NULL`)}{nothing is said about it, so it inherits whatever
#'     the enclosing selection supplies.}
#'   \item{erased (`FALSE` / `"none"`)}{an explicit *no line*, which overrides
#'     an inherited rule.}
#'   \item{a rule (`TRUE` / a style name)}{an [rtf_border_line()]: its
#'     defaults for `TRUE`, that style for a name.}
#' }
#'
#' A call to `rtf_border()` says everything about the border it returns: every
#' side not named is unset. Adding to a border already in place is the job of
#' the place it is attached to -- [style_zone()], [style_header()] and
#' [style_body()] all merge **side by side**, so a second call adds to the first
#' rather than replacing it:
#'
#' \preformatted{
#'   tbl |>
#'     style_zone(header = rtf_border(top = rtf_border_line(color = "#C9372C"))) |>
#'     style_zone(header = rtf_border(bottom = TRUE))   # the top rule survives
#' }
#'
#' A later layer can change a side or erase it with `FALSE`; a side left `NULL`
#' says nothing and leaves what was there alone.
#'
#' One distinction worth keeping straight: `rtf_border(all = FALSE)` says
#' "explicitly no rule anywhere", which erases what a border would otherwise
#' inherit, while `rtf_border()` says nothing at all and inherits.
#'
#' @section Writing borders before and after 0.5.0:
#'
#' Two things changed at 0.5.0.  `rtf_table_border()` was deprecated (and
#' removed in 0.9.0), so a
#' border is aimed with `rtftable(border = )` or [style_zone()] instead; and an
#' edge now always means the **outer** edge of the selection, so the rules
#' *inside* it have to be asked for by name.  Each case below, `s` being an
#' a border side:
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
#'   was:  rtf_table_border(body = rtf_border(all = TRUE))
#'   now:  style_zone(body = rtf_border(top = s, bottom = s, left = s,
#'                                      right = s, inside_h = s, inside_v = s))
#'
#'   ## an outer frame only, no rules inside      (was not expressible)
#'   now:  rtftable(df, border = rtf_border(top = s, bottom = s,
#'                                          left = s, right = s))
#'
#'   ## frame plus a rule under every row -- the listing look
#'   was:  four style_header() / style_body() calls on the edge columns
#'   now:  rtftable(df, border = rtf_border(top = TRUE, bottom = TRUE,
#'                                          left = TRUE, right = TRUE,
#'                                          inside_h = TRUE))
#' }
#'
#' At 0.6.0 the remaining constructors folded in here too, so a side is written
#' as a value rather than built:
#'
#' \preformatted{
#'   rtf_border_none()                 ->  rtf_border()
#'   rtf_border_top()                  ->  rtf_border(top = TRUE)
#'   rtf_border_bottom()               ->  rtf_border(bottom = TRUE)
#'   rtf_border_box()                  ->  rtf_border(all = TRUE)
#'   rtf_border_with(b, bottom = x)    ->  layer at the attach point
#'   rtf_border_tfl()                  ->  border = "tfl", rtf_table_style_tfl()
#' }
#'
#' All of them were removed in 0.9.0.
#'
#' `border = "tfl"` is unaffected, as is any border on
#' a single cell ([col_cell()], `cell_styles`): a cell has no inside, so its
#' four edges never meant anything else.
#'
#' The old and new readings are spelled identically, so rtfreporter warns once
#' per session when it meets a border that would have rendered differently
#' before.  Naming `inside_h` / `inside_v` says which you mean and silences it
#' -- use `"none"` for "no rule there".
#'
#' @param all Shorthand for `top`, `bottom`, `left` and `right` at once. A side
#'   named explicitly wins over it.
#' @param top,bottom,left,right The selection's four outer edges. A side takes
#'   a value from either of two families -- `TRUE`/`FALSE`, or a style name --
#'   or `NULL` to leave it unset. See *Writing a side* below.
#' @param inside_h,inside_v The rules drawn *between* the selection's rows
#'   (`inside_h`) and *between* its cells (`inside_v`). Same values as the
#'   edges; `NULL` (default) means no rule there.
#'
#' @return A list of class `"rtf_border"`.
#'
#' @examples
#' rtf_border(top = TRUE, bottom = TRUE)  # top + bottom
#' rtf_border(bottom = rtf_border_line(color = "#003366"))          # blue underline
#'
#' # A whole table: frame plus a rule under every row, no vertical rules.
#' s <- TRUE
#' rtf_border(top = s, bottom = s, left = s, right = s, inside_h = s)
#'
#' # A rule between the cells but no outer edges.
#' rtf_border(inside_v = TRUE)
#'
#' # Either family, but one at a time: these two agree.
#' identical(rtf_border(top = TRUE,     bottom = FALSE),
#'           rtf_border(top = "single", bottom = "none"))
#'
#' # A weight or a colour of its own needs the side value.
#' rtf_border(top    = rtf_border_line("double", 30L, "#003366"),
#'            bottom = rtf_border_line(color = "#C9372C"))
#' @export
rtf_border <- function(all = NULL, top = NULL, bottom = NULL, left = NULL,
                       right = NULL, inside_h = NULL, inside_v = NULL) {
  # `all` is the four outer edges at once; a side named explicitly wins over it.
  if (!is.null(all)) {
    if (is.null(top))    top    <- all
    if (is.null(bottom)) bottom <- all
    if (is.null(left))   left   <- all
    if (is.null(right))  right  <- all
  }
  given <- list(top = top, bottom = bottom, left = left, right = right,
                inside_h = inside_h, inside_v = inside_v)
  sides <- lapply(.border_slots, function(sl) .as_border_side(given[[sl]], sl))
  names(sides) <- .border_slots

  b <- structure(sides, class = "rtf_border")
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



# -- rtf_table_border ----------------------------------------------------------

# The five-zone map, unexported.  It stays the renderer's internal vocabulary;
# users address a row kind with style_zone() or rtf_table_style(border_* = ).
.rtf_table_border <- function(header    = NULL,
                              spanning  = NULL,
                              body      = NULL,
                              first_row = NULL,
                              last_row  = NULL,
                              outer     = NULL,
                              inside_h  = NULL,
                              inside_v  = NULL,
                              group     = NULL) {
  zones <- list(header = header, spanning = spanning, body = body,
                first_row = first_row, last_row = last_row, group = group)
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
  # `group` is the sixth zone, the rule under the last row of each row
  # group.  The renderer does not draw it (it is not a row KIND of this
  # table): as_rtftables() turns it into per-cell borders.  It is carried
  # only when given, so a table without one is the object it always was.
  grp <- zones$group
  zones$group <- NULL
  out <- structure(
    c(zones, list(outer = outer, inside_h = inside_h, inside_v = inside_v)),
    class = "rtf_table_border"
  )
  if (!is.null(grp)) out$group <- grp
  out
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
    .merge_rtf_border(b %||% rtf_border(),
                      rtf_border(left = outer$left, right = outer$right,
                                 inside_v = iv))
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
      base$header <- .merge_rtf_border(base$header, rtf_border(top = outer$top))
    } else {
      base$first_row <- .merge_rtf_border(base$first_row,
                                          rtf_border(top = outer$top))
    }
  }
  if (!is.null(outer$bottom)) {
    base$last_row <- .merge_rtf_border(base$last_row,
                                       rtf_border(bottom = outer$bottom))
  }

  # The header/body seam is an *inside* horizontal rule, not an outer one.
  if (!is.null(ih) && has_header) {
    base$header <- .merge_rtf_border(base$header, rtf_border(bottom = ih))
  }

  for (z in .table_border_zones) {
    tb[[z]] <- .merge_rtf_border(base[[z]], tb[[z]])
  }
  # The frame's own top and bottom edges, kept for the renderer (#869): a
  # leading / trailing blank row is inside the frame, so the edge the zones
  # put on the first / last data row moves onto it.  Only the edge the frame
  # put there moves; one a zone set or erased on its own stays where it is.
  if (!is.null(outer$top) || !is.null(outer$bottom)) {
    tb$frame <- list(top = outer$top, bottom = outer$bottom)
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

# The preset itself, for `border = "tfl"` and rtf_table_style_tfl().
.rtf_border_tfl <- function(style = "single", width = 15L, color = NULL) {
  s <- rtf_border_line(style, width, color)
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
        sides[[side]] <- rtf_border_line(st, width)
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

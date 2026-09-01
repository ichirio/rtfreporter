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
# Exported functions that are deprecated and scheduled for bulk removal before
# the CRAN submission.  They still work, and each warns once per session.
#
# The API-review measure is deliberately the export count EXCLUDING these: a
# function you are being told to stop using is not part of what a reader has to
# learn.  test-api-surface.R asserts both numbers so the claim stays honest.
.deprecated_exports <- c(
  "rtf_border_top",    # -> rtf_border(top = TRUE)
  "rtf_border_bottom", # -> rtf_border(bottom = TRUE)
  "rtf_border_box",    # -> rtf_border(all = TRUE)
  "rtf_border_none",   # -> rtf_border()
  "rtf_border_with",   # -> layer at the attach point (style_zone() etc.)
  "rtf_border_tfl",    # -> border = "tfl" / rtf_table_style_tfl()
  "rtf_table_border"   # -> rtftable(border = ) / style_zone()
)

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
#' TRUE                                   # thin black rule (~0.5 pt)
#' rtf_border_side(style = "double", width = 30L, color = "#003366")
#' "none"   # explicit "no line" that removes an inherited rule
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

# Resolve whatever a caller wrote for one side into an rtf_border_side.
#
#   NULL            -> unset (inherit)
#   TRUE            -> a default rule: single, 15 twips, black
#   FALSE           -> an explicit "no line", same as "none"
#   "double" etc.   -> that style, at the default weight and colour
#   rtf_border_side -> taken as-is, for weight and colour of its own
#
# The first four are shorthands for the fifth; a line's *type* is what gets
# named most often, so it should not need a constructor.
.as_border_side <- function(x, arg) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "rtf_border_side")) return(x)
  if (isTRUE(x))  return(rtf_border_side())
  if (isFALSE(x)) return(rtf_border_side("none"))
  if (is.character(x) && length(x) == 1L && !is.na(x)) {
    ok <- c(.valid_border_styles, "none")
    if (!x %in% ok) {
      stop(sprintf("`%s` must be one of %s, not \"%s\".",
                   arg, paste(dQuote(ok, q = FALSE), collapse = ", "), x),
           call. = FALSE)
    }
    return(rtf_border_side(x))
  }
  stop(sprintf(
    "`%s` must be TRUE, FALSE, a border style name, an rtf_border_side(), ",
    arg), "or NULL.", call. = FALSE)
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
#' side is either `NULL` (no border) or a border side.
#'
#' To derive a new border from an existing one, pass it as `from`.
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
#' The two agree: `TRUE` is a rule in this call's `style` / `width` / `color`,
#' and `FALSE` is `"none"` -- an explicit *no line*, which erases a rule the
#' selection would otherwise inherit. The style names are `"single"`,
#' `"double"`, `"thick"`, `"dash"`, `"dot"` and `"none"`.
#'
#' Both are shorthands for the third spelling, an [rtf_border_side()], which is
#' what a side actually holds. Reach for it when a line needs a weight or a
#' colour of its own -- and since each side carries its own, one call is always
#' enough:
#'
#' \preformatted{
#'   rtf_border(all = "double")                              # four edges alike
#'   rtf_border(top = rtf_border_side(color = "#C9372C"),    # ... or all four
#'              bottom = rtf_border_side("double", 30L))     #     different
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
#'   \item{a rule (`TRUE` / a style name)}{drawn in this call's `style`,
#'     `width` and `color`.}
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
#'     style_zone(header = rtf_border(top = rtf_border_side(color = "#C9372C"))) |>
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
#' Two things changed at 0.5.0.  [rtf_table_border()] is deprecated, so a
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
#' All of them still work and warn once per session; they are scheduled for
#' removal before the CRAN submission.
#'
#' `border = "tfl"` and [rtf_border_tfl()] are unaffected, as is any border on
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
#' @param style,width,color The line the sides named in this call draw: one of
#'   `"single"` (default), `"double"`, `"thick"`, `"dash"`, `"dot"`; a weight in
#'   twips (default `15`, about 0.5 pt); and `NULL` for black or a 6-digit hex
#'   string. Naming a style on the side itself is shorthand for `style`.
#' @param from An existing `rtf_border` to layer onto: the sides named here
#'   replace its own, the rest survive. This is how one side gets a different
#'   weight or colour from the others.
#' @param inside_h,inside_v The rules drawn *between* the selection's rows
#'   (`inside_h`) and *between* its cells (`inside_v`). Same values as the
#'   edges; `NULL` (default) means no rule there.
#'
#' @return A list of class `"rtf_border"`.
#'
#' @examples
#' rtf_border(top = TRUE, bottom = TRUE)  # top + bottom
#' rtf_border(bottom = rtf_border_side(color = "#003366"))          # blue underline
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
#' rtf_border(top    = rtf_border_side("double", 30L, "#003366"),
#'            bottom = rtf_border_side(color = "#C9372C"))
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

#' Return a copy of an `rtf_border` with selected sides replaced
#'
#' Non-mutating: returns a new `rtf_border` with the supplied side(s) set on
#' top of `border`.  `NULL` arguments leave the corresponding side unchanged.
#'
#' @param border An [rtf_border()] object.  `NULL` is accepted and treated as
#'   an empty border.
#' @param top,bottom,left,right Replacement side values, or
#'   `NULL` to leave a side unchanged.
#' @param inside_h,inside_v Replacement side values for the
#'   between-rows and between-cells rules, or `NULL` to leave them unchanged.
#'
#' @return A new `rtf_border` object.
#'
#' @examples
#' b <- rtf_border(top = TRUE, bottom = TRUE)
#' rtf_border(top = b$top, bottom = rtf_border_side(color = "#003366"))
#' rtf_border(top = "none", bottom = b$bottom)   # an explicit no-line on top
#' @export
rtf_border_with <- function(border, top = NULL, bottom = NULL,
                            left = NULL, right = NULL,
                            inside_h = NULL, inside_v = NULL) {
  .deprecate_once(
    "rtf_border_with",
    paste0(
      "`rtf_border_with()` is deprecated. Layering happens where a border is ",
      "attached:\n",
      "    style_zone() / style_header() / style_body() merge side by side, ",
      "so a\n    second call adds to the first instead of replacing it.\n",
      "  A border that needs different weights or colours per side says so in ",
      "one\n  call: rtf_border(top = rtf_border_side(...), bottom = ",
      "rtf_border_side(...))."))
  if (is.null(border)) border <- rtf_border()
  if (!inherits(border, "rtf_border")) {
    stop("`border` must be NULL or an rtf_border object.", call. = FALSE)
  }
  args <- list()
  if (!missing(top))      args$top      <- top
  if (!missing(bottom))   args$bottom   <- bottom
  if (!missing(left))     args$left     <- left
  if (!missing(right))    args$right    <- right
  if (!missing(inside_h)) args$inside_h <- inside_h
  if (!missing(inside_v)) args$inside_v <- inside_v
  .merge_rtf_border(border, do.call(rtf_border, args))
}


# -- Convenience rtf_border constructors ----------------------------------------

.deprecate_sugar <- function(fn, replacement) {
  .deprecate_once(fn, sprintf(
    "`%s()` is deprecated: write `%s` instead.  See ?rtf_border.",
    fn, replacement))
}

#' @describeIn rtf_border Deprecated. Write `rtf_border()`.
#' @export
rtf_border_none <- function() {
  .deprecate_sugar("rtf_border_none", "rtf_border()")
  rtf_border()
}

#' @describeIn rtf_border Deprecated. Write `rtf_border(top = TRUE)`.
#' @param style,width,color Line style, weight in twips and colour, as in
#'   [rtf_border()].
#' @export
rtf_border_top <- function(style = "single", width = 15L, color = NULL) {
  .deprecate_sugar("rtf_border_top", "rtf_border(top = TRUE)")
  rtf_border(top = rtf_border_side(style, width, color))
}

#' @describeIn rtf_border Deprecated. Write `rtf_border(bottom = TRUE)`.
#' @export
rtf_border_bottom <- function(style = "single", width = 15L, color = NULL) {
  .deprecate_sugar("rtf_border_bottom", "rtf_border(bottom = TRUE)")
  rtf_border(bottom = rtf_border_side(style, width, color))
}

#' @describeIn rtf_border Deprecated. Write `rtf_border(all = TRUE)`.
#' @export
rtf_border_box <- function(style = "single", width = 15L, color = NULL) {
  .deprecate_sugar("rtf_border_box", "rtf_border(all = TRUE)")
  rtf_border(all = rtf_border_side(style, width, color))
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
#' @param inside_h,inside_v Side values for the rules between rows and
#'   between cells, or `NULL` (default).
#'
#' @return A list of class `"rtf_table_border"`.
#'
#' @seealso [rtf_border()] for the pieces; pass the result
#'   as `rtftable(border = )`.
#'
#' @examples
#' s  <- TRUE
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
#'   `rtf_table_border(last_row = rtf_border(bottom = TRUE))`.
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
  .deprecate_once(
    "rtf_border_tfl",
    paste0(
      "`rtf_border_tfl()` is deprecated: the clinical TFL rules are already ",
      "reachable as\n  `rtftable(border = \"tfl\")`, and as a reusable value ",
      "from `rtf_table_style_tfl()`."))
  .rtf_border_tfl(style = style, width = width, color = color)
}

# The preset itself, for `border = "tfl"` and rtf_table_style_tfl().
.rtf_border_tfl <- function(style = "single", width = 15L, color = NULL) {
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

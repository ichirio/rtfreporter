# ============================================================================
#  rtf_table_style -- shared style template (S3)
# ============================================================================
#
#  An `rtf_table_style` bundles table-wide formatting defaults (borders,
#  alignment, bold, cell padding, row height) into one record.  Build one
#  with [rtf_table_style()] and hand the same object to many [rtftable()]
#  calls; each call snapshots the values it needs at construction time.
#
#  Resolution precedence (used by rtftable()):
#
#      explicit argument > col_spec entry > style field > package default
#
#  so an explicit rtftable() argument always wins, and a style object
#  merely provides defaults.
#
#  Use [rtf_table_style_with()] to derive a new style from an existing one
#  with selected fields replaced.

#' Shared table style
#'
#' Bundles table-wide formatting defaults -- borders, alignment, bold, cell
#' padding, row height -- into a single record that can be passed as the
#' `style =` argument of [rtftable()].  Each `rtftable()` call snapshots the
#' style fields it needs at construction time, so the style object behaves
#' like an immutable template.
#'
#' Use [rtf_table_style_with()] (or simply construct a fresh style) to
#' derive a variant.
#'
#' @param border_header,border_spanning,border_body,border_first_row,border_last_row
#'   [rtf_border()] objects (or `NULL`) controlling each zone of the table.
#' @param border_group An [rtf_border()] (or `NULL`, the default: no rule)
#'   drawn under the **last row of each row group** -- the groups
#'   `as_rtftables(group_by = )` finds.  It is written by [as_rtftables()]
#'   (and so by [plan_style()]); [rtftable()] alone has no groups and
#'   ignores it.  See [plan_style()] for the last-row and page-break rules.
#' @param header_align,header_bold,header_italic Defaults for column-header
#'   row formatting.  `header_align = NULL` means "inherit `align`";
#'   otherwise `"left"`, `"center"` or `"right"`.
#' @param align,bold,italic,underline Defaults for data-row formatting.
#'   `align = NULL` (the default) leaves each column's own default alone ---
#'   row-title columns left, the others centred (see [rtftable()]'s
#'   `row_title`); `"left"` / `"center"` / `"right"` sets every column.  Any
#'   other `align` or `header_align` is an error; a flag other than `TRUE` is
#'   stored as `FALSE`.
#' @param cell_padding_left_twips,cell_padding_right_twips Cell padding
#'   (twips) used by both column-header and data cells; `NULL` leaves the
#'   document's padding.
#' @param cell_padding_top_twips,cell_padding_bottom_twips Cell padding above /
#'   below the text (twips) of column-header and data cells; `NULL` leaves the
#'   document's padding.
#' @param row_height_twips Row height (twips); `NULL` = font-aware default.
#'   This and the four padding arguments are each one non-negative integer
#'   or `NULL`; anything else is an error.
#'
#' @return A list of class `"rtf_table_style"`.
#'
#' @examples
#' tfl_style <- rtf_table_style(
#'   border_header   = rtf_border(top = TRUE, bottom = TRUE),
#'   border_last_row = rtf_border(bottom = TRUE),
#'   header_bold     = FALSE,
#'   header_align    = NULL    # inherit data alignment
#' )
#'
#' dfs <- list(data.frame(Parameter = "Age", Value = "75.1"),
#'             data.frame(Parameter = "Sex", Value = "53%"))
#' tbls <- lapply(dfs, function(df) rtftable(df, style = tfl_style))
#'
#' @export
rtf_table_style <- function(
  border_header    = NULL,
  border_spanning  = NULL,
  border_body      = NULL,
  border_first_row = NULL,
  border_last_row  = NULL,
  border_group     = NULL,
  header_align     = NULL,
  header_bold      = FALSE,
  header_italic    = FALSE,
  align            = NULL,
  bold             = FALSE,
  italic           = FALSE,
  underline        = FALSE,
  cell_padding_left_twips  = NULL,
  cell_padding_right_twips = NULL,
  row_height_twips         = NULL,
  cell_padding_top_twips    = NULL,
  cell_padding_bottom_twips = NULL
) {
  .check_border <- function(b, nm) {
    if (!is.null(b) && !inherits(b, "rtf_border")) {
      stop(sprintf("`%s` must be NULL or an rtf_border object.", nm),
           call. = FALSE)
    }
  }
  .check_border(border_header,    "border_header")
  .check_border(border_spanning,  "border_spanning")
  .check_border(border_body,      "border_body")
  .check_border(border_first_row, "border_first_row")
  .check_border(border_last_row,  "border_last_row")
  .check_border(border_group,     "border_group")

  structure(
    list(
      border_header    = border_header,
      border_spanning  = border_spanning,
      border_body      = border_body,
      border_first_row = border_first_row,
      border_last_row  = border_last_row,
      border_group     = border_group,

      header_align  = .check_align(header_align, "header_align"),
      header_bold   = isTRUE(header_bold),
      header_italic = isTRUE(header_italic),

      align     = .check_align(align, "align"),
      bold      = isTRUE(bold),
      italic    = isTRUE(italic),
      underline = isTRUE(underline),

      cell_padding_left_twips  =
        .check_row_height(cell_padding_left_twips, "cell_padding_left_twips"),
      cell_padding_right_twips =
        .check_row_height(cell_padding_right_twips, "cell_padding_right_twips"),
      row_height_twips         =
        .check_row_height(row_height_twips, "row_height_twips"),
      cell_padding_top_twips    =
        .check_row_height(cell_padding_top_twips, "cell_padding_top_twips"),
      cell_padding_bottom_twips =
        .check_row_height(cell_padding_bottom_twips, "cell_padding_bottom_twips")
    ),
    class = "rtf_table_style"
  )
}

#' Return a copy of an `rtf_table_style` with selected fields replaced
#'
#' Non-mutating derivation: returns a new `rtf_table_style` whose listed
#' fields are overridden.  Unknown field names raise an error.
#'
#' @param style An [rtf_table_style()] object.
#' @param ... Named field overrides.  Allowed names match the arguments of
#'   [rtf_table_style()]; each is checked as that function checks it. `NULL`
#'   resets a field to its [rtf_table_style()] default.
#'
#' @return A new `rtf_table_style` object.
#'
#' @examples
#' base <- rtf_table_style_tfl()
#' rtf_table_style_with(base, align = "center", row_height_twips = 280L)
#' @export
rtf_table_style_with <- function(style, ...) {
  if (!inherits(style, "rtf_table_style")) {
    stop("`style` must be an rtf_table_style object.", call. = FALSE)
  }
  overrides <- list(...)
  if (!length(overrides)) return(style)
  nms <- names(overrides)
  if (is.null(nms) || any(!nzchar(nms))) {
    stop("The fields given to `rtf_table_style_with()` must be named, ",
         "e.g. `align = \"center\"`.", call. = FALSE)
  }
  bad <- setdiff(nms, names(style))
  if (length(bad)) {
    stop(sprintf("Unknown style field: '%s'", bad[1L]), call. = FALSE)
  }
  # Rebuilt through rtf_table_style(): the values are checked there, and a NULL
  # (kept with `[<-`, not dropped as `[[<-` would) gives the field its default.
  fields <- unclass(style)
  fields[nms] <- overrides
  do.call(rtf_table_style, fields)
}

#' @export
print.rtf_table_style <- function(x, ...) {
  cat("<rtf_table_style>\n")
  cat("  borders:\n")
  for (z in c("header", "spanning", "body", "first_row", "last_row")) {
    v <- x[[paste0("border_", z)]]
    cat(sprintf("    %-10s: %s\n", z, if (is.null(v)) "none" else "<rtf_border>"))
  }
  # only when set, so a style without it prints as before
  if (!is.null(x$border_group)) cat("    group     : <rtf_border>\n")
  cat(sprintf("  header_align : %s\n",
              if (is.null(x$header_align)) "(inherit align)" else x$header_align))
  cat(sprintf("  header_bold  : %s\n", x$header_bold))
  cat(sprintf("  align        : %s\n",
              if (is.null(x$align)) "(per column)" else x$align))
  cat(sprintf("  bold         : %s\n", x$bold))
  cat(sprintf("  cell_padding : L=%s R=%s\n",
              x$cell_padding_left_twips  %||% "(default)",
              x$cell_padding_right_twips %||% "(default)"))
  # top / bottom only when set, so a style without them prints as before
  if (!is.null(x$cell_padding_top_twips) || !is.null(x$cell_padding_bottom_twips)) {
    cat(sprintf("  cell_padding : T=%s B=%s\n",
                x$cell_padding_top_twips    %||% "(default)",
                x$cell_padding_bottom_twips %||% "(default)"))
  }
  invisible(x)
}

# Internal: convert an rtf_table_style into the rtf_table_border S3 spec
# consumed by the renderer's border pipeline.
.style_to_table_border <- function(style) {
  .rtf_table_border(
    header    = style$border_header,
    spanning  = style$border_spanning,
    body      = style$border_body,
    first_row = style$border_first_row,
    last_row  = style$border_last_row,
    group     = style$border_group
  )
}

#' Clinical TFL preset (table style)
#'
#' Returns a freshly constructed `rtf_table_style` matching the standard
#' clinical TFL preset: borders are applied to the **column-header
#' block only** (top on the topmost header row, bottom on the
#' bottommost; multi-col spanning auto-underlines).  **The data section
#' carries no borders by default.**  No vertical lines.  No bold
#' headers.
#'
#' To override one or more fields, pipe through [rtf_table_style_with()]:
#'
#' \preformatted{
#'   heavy <- rtf_table_style_with(rtf_table_style_tfl(),
#'             header_bold = TRUE,
#'             border_last_row = rtf_border(bottom = TRUE))
#' }
#'
#' @return An `rtf_table_style` object.
#'
#' @examples
#' style <- rtf_table_style_tfl()
#' rtftable(data.frame(Parameter = "Age", Value = "75.1"), style = style)
#' @export
rtf_table_style_tfl <- function() {
  s <- rtf_border_line()
  rtf_table_style(
    border_header = rtf_border(top = s, bottom = s),
    header_bold   = FALSE,
    header_align  = NULL    # inherit data alignment
  )
}

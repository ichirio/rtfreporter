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
#' @param header_align,header_bold,header_italic Defaults for column-header
#'   row formatting.  `header_align = NULL` means "inherit `align`".
#' @param align,bold,italic,underline Defaults for data-row formatting.
#' @param cell_padding_left_twips,cell_padding_right_twips Cell padding
#'   (twips) used by both column-header and data cells.
#' @param row_height_twips Row height (twips); `NULL` = font-aware default.
#'
#' @return A list of class `"rtf_table_style"`.
#'
#' @examples
#' \dontrun{
#' tfl_style <- rtf_table_style(
#'   border_header   = rtf_border(top = TRUE, bottom = TRUE),
#'   border_last_row = rtf_border(bottom = TRUE),
#'   header_bold     = FALSE,
#'   header_align    = NULL    # inherit data alignment
#' )
#'
#' tbls <- lapply(dfs, function(df) rtftable(df, style = tfl_style))
#' }
#'
#' @export
rtf_table_style <- function(
  border_header    = NULL,
  border_spanning  = NULL,
  border_body      = NULL,
  border_first_row = NULL,
  border_last_row  = NULL,
  header_align     = NULL,
  header_bold      = FALSE,
  header_italic    = FALSE,
  align            = "left",
  bold             = FALSE,
  italic           = FALSE,
  underline        = FALSE,
  cell_padding_left_twips  = NULL,
  cell_padding_right_twips = NULL,
  row_height_twips         = NULL
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

  structure(
    list(
      border_header    = border_header,
      border_spanning  = border_spanning,
      border_body      = border_body,
      border_first_row = border_first_row,
      border_last_row  = border_last_row,

      header_align  = header_align,
      header_bold   = isTRUE(header_bold),
      header_italic = isTRUE(header_italic),

      align     = align,
      bold      = isTRUE(bold),
      italic    = isTRUE(italic),
      underline = isTRUE(underline),

      cell_padding_left_twips  = cell_padding_left_twips,
      cell_padding_right_twips = cell_padding_right_twips,
      row_height_twips         = row_height_twips
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
#'   [rtf_table_style()].
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
  allowed <- names(style)
  for (nm in names(overrides)) {
    if (!nm %in% allowed) {
      stop(sprintf("Unknown style field: '%s'", nm), call. = FALSE)
    }
    style[[nm]] <- overrides[[nm]]
  }
  style
}

#' @export
print.rtf_table_style <- function(x, ...) {
  cat("<rtf_table_style>\n")
  cat("  borders:\n")
  for (z in c("header", "spanning", "body", "first_row", "last_row")) {
    v <- x[[paste0("border_", z)]]
    cat(sprintf("    %-10s: %s\n", z, if (is.null(v)) "none" else "<rtf_border>"))
  }
  cat(sprintf("  header_align : %s\n",
              if (is.null(x$header_align)) "(inherit align)" else x$header_align))
  cat(sprintf("  header_bold  : %s\n", x$header_bold))
  cat(sprintf("  align        : %s\n", x$align))
  cat(sprintf("  bold         : %s\n", x$bold))
  cat(sprintf("  cell_padding : L=%s R=%s\n",
              x$cell_padding_left_twips  %||% "(default)",
              x$cell_padding_right_twips %||% "(default)"))
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
    last_row  = style$border_last_row
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
  s <- .rtf_border_side()
  rtf_table_style(
    border_header = rtf_border(top = s, bottom = s),
    header_bold   = FALSE,
    header_align  = NULL    # inherit data alignment
  )
}

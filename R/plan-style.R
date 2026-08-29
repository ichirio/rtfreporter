# ============================================================================
#  SPIKE (design/plan-resolver) -- the style layer
# ============================================================================
#
#  Step 5.  Nothing exported.
#
#  Until now `plan_tables(...)` passed styling straight through to rtftable(),
#  which meant table styling was the one thing NOT declared -- it had to be
#  supplied at build time, in final positions, exactly the coupling the design
#  set out to remove.
#
#  Two kinds of styling, and they belong in different places:
#
#    * TABLE-level (border, widths, font) is one setting for the whole table
#      and lives in its own layer, `plan_style()`;
#    * COLUMN-level (align, bold, indent, colour) is a property of a column and
#      so belongs on that column's ROLE, addressed BY NAME.
#
#  The second is the interesting one.  `col_spec` is position-indexed, which is
#  why `.reindex_col_spec()` exists.  Here the caller names a column, the column
#  map says where it ended up, and the col_spec is built at that position --
#  one more reindexer with nothing left to do.

# Table-wide styling.  LAST WRITER WINS, PER FIELD, as everywhere else.
#' @keywords internal
plan_style <- function(plan, border = NULL, widths = NULL, font = NULL,
                       font_size_half_points = NULL, row_height_twips = NULL,
                       table_align = NULL) {
  .plan_set(plan, "style",
            list(border = border, column_widths_twips = widths, font = font,
                 font_size_half_points = font_size_half_points,
                 row_height_twips = row_height_twips,
                 table_align = table_align))
}

# The rtftable() arguments a resolved plan implies.
#
# `columns$map` turns every by-name column style into a position, so nothing
# here -- and nothing downstream -- has to know what the stub merged or what
# the hidden columns removed.
.plan_resolve_style <- function(style, roles, columns, source_kw = NULL) {
  args <- if (is.null(style)) list() else style
  args <- args[!vapply(args, is.null, logical(1L))]

  if (length(columns$names) == 0L) return(args)

  keys <- c("align", "bold", "italic", "underline", "indent_twips", "color",
            "header_align", "header_bold", "header_italic")
  spec <- list()

  # The ADAPTER's own col_spec first -- gt's column alignment, for one, which
  # a spanning cell inherits.  Dropping it rendered a right-aligned spanner
  # centred; the RTF diff against as_rtftables() found it, neither source did.
  # Its `col` is a SOURCE position, so the map places it like everything else.
  adapter <- source_kw$col_spec
  if (is.list(adapter)) {
    n0 <- length(columns$map)
    for (e in adapter) {
      if (!is.list(e)) next
      src <- e$col
      if (is.character(src)) src <- match(src, names(columns$map))
      src <- suppressWarnings(as.integer(src))
      if (length(src) != 1L || is.na(src) || src < 1L || src > n0) next
      pos <- unname(columns$map[[src]])
      if (is.na(pos)) next
      prev <- spec[[as.character(pos)]]
      if (is.null(prev)) prev <- list(col = as.integer(pos))
      for (o in intersect(names(e), keys)) prev[[o]] <- e[[o]]
      spec[[as.character(pos)]] <- prev
    }
  }

  if (is.null(roles)) {
    if (length(spec)) {
      args$col_spec <- unname(spec[order(as.integer(names(spec)))])
    }
    return(args)
  }

  # Then the plan's own per-column options, which are explicit and so win.
  for (nm in names(roles)) {
    opts <- roles[[nm]]$opts[intersect(names(roles[[nm]]$opts), keys)]
    if (length(opts) == 0L) next
    pos <- unname(columns$map[[nm]])
    if (is.na(pos)) next                    # hidden: nothing to style
    prev <- spec[[as.character(pos)]]
    if (is.null(prev)) prev <- list(col = as.integer(pos))
    for (o in names(opts)) prev[[o]] <- opts[[o]]
    spec[[as.character(pos)]] <- prev
  }
  if (length(spec)) {
    # Several source columns can share one final position (a merged stub), so
    # the entries are collected by position and emitted once each.
    args$col_spec <- unname(spec[order(as.integer(names(spec)))])
  }
  args
}

# ============================================================================
#  Font table -- several fonts, and per-element selection
# ============================================================================
#
#  The document used to carry exactly one font: the resource template was fixed
#  at a single entry and the renderer only ever read `font_table[[1]]$name`, so
#  there was no `\f1` to select and per-element choice was impossible.
#
#  The table is now variable length, and each element may name a font. The
#  mechanism mirrors the colour table, which already resolves a requested
#  colour to an index and declares it on the way out:
#
#      .collect_fonts()      every family the document mentions, in order
#      .build_font_index_map()   family -> \fN index
#      .build_font_table_rtf()   the {\fonttbl ...} group
#      .f_cmd_for()          the \fN a given element needs, or "" for the default
#
#  Index 0 is the document default, so an element that asks for nothing emits
#  nothing and the output is unchanged.

# Normalise a font_table (a list of list(name=), or a plain character vector)
# to a character vector of family names, first = the document default.
.font_names <- function(font_table) {
  if (is.null(font_table)) return(character(0))
  if (is.character(font_table)) return(font_table[nzchar(font_table)])
  nm <- vapply(font_table, function(f) {
    if (is.character(f) && length(f) == 1L) f else as.character(f$name %||% "")
  }, character(1L))
  nm[nzchar(nm)]
}

.check_font <- function(x, arg) {
  if (is.null(x)) return(NULL)
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(sprintf("`%s` must be a single font family name.", arg), call. = FALSE)
  }
  x
}

# Every family the rendered document needs: the declared table first (its first
# entry is the default), then any family an element asked for that is not
# already declared.
.collect_fonts <- function(declared, requested) {
  declared <- .font_names(declared)
  if (length(declared) == 0L) declared <- .opt("rtfreporter.font")
  requested <- unlist(requested, use.names = FALSE)
  requested <- requested[!is.na(requested) & nzchar(requested)]
  unique(c(declared, requested))
}

# family -> \fN index, 0-based, matching the emitted table.
.build_font_index_map <- function(fonts) {
  if (length(fonts) == 0L) return(list())
  stats::setNames(as.list(seq_along(fonts) - 1L), fonts)
}

# The {\fonttbl ...} group for the collected families.
#   \fnil\fcharset0 -- unknown family, ANSI charset: deterministic glyph
#   mapping across viewers without claiming a specific family.
.build_font_table_rtf <- function(fonts) {
  if (length(fonts) == 0L) fonts <- .opt("rtfreporter.font")
  entries <- vapply(seq_along(fonts), function(i) {
    sprintf("{\\f%d\\fnil\\fcharset0 %s;}", i - 1L, .rtf_escape(fonts[[i]]))
  }, character(1L))
  paste0("{\\fonttbl", paste(entries, collapse = ""), "}")
}

# The \fN an element needs.  "" when it wants the document default (index 0),
# so an element that changes nothing emits nothing.
.f_cmd_for <- function(font, font_index_map) {
  if (is.null(font) || !length(font) || !nzchar(font)) return("")
  # `[[` on an absent name errors, and a renderer may legitimately be handed an
  # empty map (a direct .render_* call in a test), so look the name up by match
  i <- match(font, names(font_index_map))
  if (is.na(i)) return("")
  idx <- font_index_map[[i]]
  if (is.null(idx) || identical(as.integer(idx), 0L)) return("")
  paste0("\\f", as.integer(idx))
}

# Every font an assembled report asks for, so the table can declare them all
# before the body is written.
.collect_report_fonts <- function(report) {
  # A header/footer may still be the legacy named character vector rather than
  # an rtf_header() list, so only reach for `$font` where there is one to reach.
  fld <- function(x) if (is.list(x)) x$font else NULL
  doc <- report$document
  out <- c(fld(doc$title_style), fld(doc$footnote_style))
  for (p in report$pages) {
    ct <- p$content
    if (inherits(ct, "rtftable")) out <- c(out, fld(ct))
  }
  for (s in report$sections) {
    if (!is.list(s)) next
    out <- c(out, fld(s$header), fld(s$footer))
  }
  unlist(out, use.names = FALSE)
}

# Internal utility: inches to twips.
.in_to_twips <- function(x) {
  as.integer(round(x * 1440))
}

# NULL-coalescing operator.
`%||%` <- function(a, b) if (!is.null(a)) a else b

.load_rtf_commands <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) {
      return(cache)
    }

    resource_candidates <- c(
      file.path(getwd(), "inst", "resources", "rtf_commands.R"),
      file.path(getwd(), "r", "rtfreporter", "inst", "resources", "rtf_commands.R")
    )
    pkg_resource_dir <- system.file("resources", package = "rtfreporter")
    if (nzchar(pkg_resource_dir)) {
      # Append installed-package path AFTER local paths so that source-tree runs
      # always pick up the local (possibly newer) resource file first.
      resource_candidates <- c(resource_candidates, file.path(pkg_resource_dir, "rtf_commands.R"))
    }

    resource_path <- resource_candidates[file.exists(resource_candidates)][1]
    if (is.na(resource_path)) {
      stop("RTF command resource file not found.", call. = FALSE)
    }

    env <- new.env(parent = baseenv())
    sys.source(resource_path, envir = env)
    if (!exists("rtf_commands", envir = env, inherits = FALSE)) {
      stop("`rtf_commands` not defined in resource file.", call. = FALSE)
    }
    cache <<- get("rtf_commands", envir = env, inherits = FALSE)
    cache
  }
})

.load_rtfreporter_defaults <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) {
      return(cache)
    }

    resource_candidates <- c(
      file.path(getwd(), "inst", "resources", "rtfreporter_defaults.R"),
      file.path(getwd(), "r", "rtfreporter", "inst", "resources", "rtfreporter_defaults.R")
    )
    pkg_resource_dir <- system.file("resources", package = "rtfreporter")
    if (nzchar(pkg_resource_dir)) {
      resource_candidates <- c(file.path(pkg_resource_dir, "rtfreporter_defaults.R"), resource_candidates)
    }

    resource_path <- resource_candidates[file.exists(resource_candidates)][1]
    if (is.na(resource_path)) {
      # Built-in fallback: hard-coded approximation of the resource file.
      cache <<- list(
        default_row_height_twips_by_font_half_points = list("18" = 230L),
        default_row_height_twips_per_half_point      = 12.8,
        default_row_height_twips_min                 = 180L,
        default_cell_padding_left_twips              = 72L,
        default_cell_padding_right_twips             = 72L
      )
      return(cache)
    }

    env <- new.env(parent = baseenv())
    sys.source(resource_path, envir = env)
    if (!exists("rtfreporter_defaults", envir = env, inherits = FALSE)) {
      stop("`rtfreporter_defaults` not defined in resource file.", call. = FALSE)
    }
    cache <<- get("rtfreporter_defaults", envir = env, inherits = FALSE)
    cache
  }
})

# Resolve the default row (cell) height for a given font size.
#
# Looked up from `rtfreporter_defaults.R`:
#   1. If the font size (in half-points) appears in the lookup table, that
#      value is returned.
#   2. Otherwise a linear fallback `font_half_points * per_half_point` is
#      used.
#   3. The result is clamped to `default_row_height_twips_min`.
#
# Used by all table-shaped renderers (header / footer / table body /
# footnote) so a single resource-file entry controls the package-wide
# default visual density.
.default_row_height_twips <- function(font_half_points = 18L) {
  defaults <- .load_rtfreporter_defaults()
  fhp <- as.integer(font_half_points %||% 18L)

  tbl <- defaults$default_row_height_twips_by_font_half_points
  key <- as.character(fhp)
  h <- if (!is.null(tbl) && key %in% names(tbl)) {
    as.integer(tbl[[key]])
  } else {
    rate <- defaults$default_row_height_twips_per_half_point %||% 12.8
    as.integer(round(fhp * as.numeric(rate)))
  }

  lo <- as.integer(defaults$default_row_height_twips_min %||% 0L)
  if (h < lo) lo else h
}

.cmd_fmt <- function(template, values = list()) {
  out <- template
  if (length(values) == 0L) {
    return(out)
  }
  for (nm in names(values)) {
    out <- gsub(paste0("{", nm, "}"), as.character(values[[nm]]), out, fixed = TRUE)
  }
  out
}

# -- Text processing ------------------------------------------------------------

# Character-level RTF escape + Unicode conversion.
# Handles: \, {, }, newline -> \line , non-ASCII -> \uN?
# Does NOT handle markup tokens (^{}, _{}); those are handled by .format_cell_text().
.rtf_escape_unicode_raw <- function(x) {
  if (!nzchar(x)) return(x)
  chars <- strsplit(x, "", fixed = TRUE)[[1]]
  out <- vapply(chars, function(ch) {
    cp <- utf8ToInt(ch)
    if      (ch == "\\") return("\\\\")
    if      (ch == "{")  return("\\{")
    if      (ch == "}")  return("\\}")
    if      (ch == "\n") return("\\line ")
    if (cp > 127L)       return(sprintf("\\u%d?", cp))
    ch
  }, character(1L))
  paste0(out, collapse = "")
}

# Process a segment of text that may contain ^{} / _{} markup recursively,
# escaping plain-text parts along the way.
.process_markup <- function(x) {
  if (!nzchar(x)) return(x)
  n <- nchar(x)

  # Find the first ^{ or _{ occurrence.
  m_super <- regexpr("\\^\\{", x, perl = TRUE)
  m_sub   <- regexpr("_\\{",   x, perl = TRUE)

  # Pick the earliest match.
  pos  <- Inf
  kind <- ""
  if (m_super[1] > 0L && m_super[1] < pos) { pos <- m_super[1]; kind <- "super" }
  if (m_sub[1]   > 0L && m_sub[1]   < pos) { pos <- m_sub[1];   kind <- "sub"   }

  if (kind == "") {
    # No markup found - escape the whole segment.
    return(.rtf_escape_unicode_raw(x))
  }

  prefix_len <- 2L  # both ^{ and _{ are 2 chars
  before  <- substr(x, 1L, pos - 1L)
  rest    <- substr(x, pos + prefix_len, n)

  # Find the matching closing brace.
  depth     <- 1L
  end_pos   <- 0L
  rest_chars <- strsplit(rest, "", fixed = TRUE)[[1]]
  for (i in seq_along(rest_chars)) {
    ch <- rest_chars[i]
    if (ch == "{") depth <- depth + 1L
    if (ch == "}") { depth <- depth - 1L; if (depth == 0L) { end_pos <- i; break } }
  }

  if (end_pos == 0L) {
    # Unmatched brace - treat the whole thing as plain text.
    return(.rtf_escape_unicode_raw(x))
  }

  inner <- substr(rest, 1L, end_pos - 1L)
  after <- substr(rest, end_pos + 1L, nchar(rest))
  cmd   <- if (kind == "super") "\\super " else "\\sub "

  paste0(
    .process_markup(before),
    "{", cmd, .process_markup(inner), "}",
    .process_markup(after)
  )
}

# Full cell-text processing pipeline, gated by the enabled `markup` tokens:
#   "script"     -> ^{} / _{} super/subscript (via .process_markup)
#   "relational" -> >= / <= converted to U+2265 / U+2264
# RTF escaping + Unicode + newline handling always happen.  Default "script"
# matches the package default (relational conversion is opt-in).
.format_cell_text <- function(x, markup = "script") {
  if (is.null(x) || is.na(x)) return("")
  x <- as.character(x)
  relational <- "relational" %in% markup
  script     <- "script"     %in% markup

  # Relational symbols: stash as placeholders before escaping so the literal
  # ">=" / "<=" are not themselves escaped, then swap in the RTF Unicode escape.
  if (relational) {
    x <- gsub(">=", "\x01GE\x01", x, fixed = TRUE)
    x <- gsub("<=", "\x01LE\x01", x, fixed = TRUE)
  }

  # Script markup parses ^{}/_{} (and escapes plain parts); otherwise escape the
  # text verbatim so ^{ / _{ stay literal.
  result <- if (script) .process_markup(x) else .rtf_escape_unicode_raw(x)

  if (relational) {
    result <- gsub("\x01GE\x01", "\\u8805?", result, fixed = TRUE)
    result <- gsub("\x01LE\x01", "\\u8804?", result, fixed = TRUE)
  }

  result
}

# RTF-safe escape for header/footer text (no markup processing, supports tokens).
.rtf_escape <- function(x) {
  if (length(x) == 0L || is.na(x)) return("")
  .rtf_escape_unicode_raw(as.character(x))
}

# Replace every literal occurrence of `token` in `x` with `replacement`.
# A generic string substitution helper -- it does NOT escape, and knows nothing
# about page tokens (that semantics lives in .render_tokens() below).  A rare
# sentinel is appended before splitting so strsplit() does not silently drop a
# trailing empty match (R drops a trailing "" when the delimiter ends the string).
.replace_token <- function(x, token, replacement) {
  # Append a rare sentinel so strsplit never drops a trailing empty string
  # (R's strsplit silently drops trailing "" when the match is at end-of-string).
  sentinel <- "\x01RTFTOK_END\x01"
  parts <- strsplit(paste0(x, sentinel), token, fixed = TRUE)[[1L]]
  # Remove the sentinel from the last element.
  parts[length(parts)] <- sub(sentinel, "", parts[length(parts)], fixed = TRUE)
  paste(parts, collapse = replacement)
}

# Replace page tokens, then RTF-escape.
#
# Token reference (after escaping, braces appear as \{ and \}):
#   \{AUTO_PAGE\}                    -> \chpgn  (RTF DYNAMIC per-page number;
#                                        the viewer renders the actual page
#                                        the user is looking at)
#   \{AUTO_TOTAL_PAGES\}             -> RTF NUMPAGES field (DYNAMIC total,
#                                        recomputed across the document)
#   \{SECTION_PAGES\}                -> RTF SECTIONPAGES field (DYNAMIC,
#                                        current section's page count)
#   \{PAGE\}                         -> STATIC integer at render time
#                                        (the section's first-page number)
#   \{TOTAL_PAGES\}                  -> STATIC integer at render time
#                                        (document total page count)
#
# The static tokens freeze at render time and stay verbatim through
# downstream tooling (notably assemble_rtf, which never rewrites
# numbers in the source files).  The AUTO_* tokens are recomputed
# every time the RTF viewer opens the file, so they pick up the
# correct numbers even after `assemble_rtf()` has concatenated
# multiple deliverables.
.render_tokens <- function(x, current_page = NULL, total_pages = NULL) {
  if (is.null(x)) return("")
  # Escape first; after escaping, { becomes \{ and } becomes \}, so
  # the token {PAGE} appears as \{PAGE\} and can be substituted safely.
  out <- .rtf_escape(x)

  # Dynamic per-page page number: viewer-rendered.
  out <- gsub("\\{AUTO_PAGE\\}", "\\chpgn ", out, fixed = TRUE)

  # Dynamic total-pages: RTF NUMPAGES field with static fallback count.
  fallback <- if (!is.null(total_pages)) as.character(total_pages) else "?"
  cmds <- .load_rtf_commands()
  numpages_rtf <- .cmd_fmt(cmds$fields$auto_total_pages, list(total_pages = fallback))
  out <- gsub("\\{AUTO_TOTAL_PAGES\\}", numpages_rtf, out, fixed = TRUE)

  # Dynamic section-pages: RTF SECTIONPAGES field.
  out <- gsub("\\{SECTION_PAGES\\}", cmds$fields$section_pages, out, fixed = TRUE)

  # Static per-section page number: integer baked in at render time.
  # `current_page` is the first-page number of the section being rendered.
  if (!is.null(current_page)) {
    out <- gsub("\\{PAGE\\}", as.character(current_page), out, fixed = TRUE)
  }

  # Static total-pages: integer resolved at render time.
  if (!is.null(total_pages)) {
    out <- gsub("\\{TOTAL_PAGES\\}", as.character(total_pages), out, fixed = TRUE)
  }

  out
}

# -- Border helpers -------------------------------------------------------------

# Build RTF border commands for all four sides of a single cell.
# border_spec: rtf_border object (new style), OR
#              plain list with keys top/bottom/left/right (type string or "none"/NULL)
#              and optional width (integer twips, default 15) (old style).
# color_index_map: named list mapping "#RRGGBB" -> integer color-table index.
# Returns "" when border_spec is NULL (no borders).
.build_border_commands <- function(border_spec, color_index_map = NULL) {
  if (is.null(border_spec)) return("")
  cmds     <- .load_rtf_commands()
  prefixes <- cmds$border$side_prefix
  styles   <- cmds$border$style

  if (inherits(border_spec, "rtf_border")) {
    # New-style: rtf_border with rtf_border_side elements.
    .side <- function(side) {
      b <- border_spec[[side]]
      if (is.null(b)) return("")
      # An explicit "none" side draws no line (it exists only to override an
      # inherited border when merged on top of another spec).
      if (identical(b$style, "none")) return("")
      s <- styles[[b$style]]
      if (is.null(s)) stop(sprintf("Unknown border style: '%s'", b$style), call. = FALSE)
      color_cmd <- ""
      if (!is.null(b$color) && !is.null(color_index_map)) {
        idx <- color_index_map[[b$color]]
        if (!is.null(idx)) color_cmd <- paste0("\\brdrcf", idx)
      }
      paste0(prefixes[[side]], s, "\\brdrw", b$width, color_cmd)
    }
  } else {
    # Old-style: plain list with top/bottom/left/right as strings + width.
    width <- as.integer(border_spec$width %||% 15L)
    .side <- function(side) {
      type <- border_spec[[side]]
      if (is.null(type) || type %in% c("none", "")) return("")
      s <- styles[[type]]
      if (is.null(s)) stop(sprintf("Unknown border type: '%s'", type), call. = FALSE)
      paste0(prefixes[[side]], s, "\\brdrw", width)
    }
  }
  paste0(.side("top"), .side("bottom"), .side("left"), .side("right"))
}

# Merge an override border on top of a base border.
#
#   base = NULL, override = NULL -> NULL
#   base = X,    override = NULL -> X
#   base = NULL, override = Y    -> Y           (was: dropped -- bug)
#   base = X,    override = Y    -> X with non-NULL sides of Y replacing X
#
# Used by:
#   * the data-row loop (body x first_row / last_row),
#   * `.render_header_row()` (zone x col_spec per-cell),
#   * `.render_spanning_rows()` (zone x col_spec per-cell).
.effective_row_border <- function(base_border, override) {
  if (is.null(override) || length(override) == 0L) return(base_border)
  if (is.null(base_border)) return(override)
  if (inherits(base_border, "rtf_border")) {
    return(.merge_rtf_border(base_border, override))
  }
  .merge_list(base_border, override)
}

# -- Column width computation ---------------------------------------------------

# Compute cumulative cellx positions for a table.
# Priority: column_widths_twips > col_rel_width > equal distribution.
.compute_cellx <- function(ncols, writable_width_twips, tbl) {
  if (!is.null(tbl$column_widths_twips)) {
    widths <- as.integer(tbl$column_widths_twips)
    if (length(widths) != ncols) {
      stop(sprintf("`column_widths_twips` length (%d) must match ncol (%d).",
                   length(widths), ncols), call. = FALSE)
    }
    return(cumsum(widths))
  }

  # Determine total table width.
  total_w <- if (!is.null(tbl$table_width_twips)) {
    as.integer(tbl$table_width_twips)
  } else if (!is.null(tbl$table_width_pct_of_writable)) {
    as.integer(round(writable_width_twips * as.numeric(tbl$table_width_pct_of_writable)))
  } else {
    as.integer(writable_width_twips)
  }

  if (!is.null(tbl$col_rel_width)) {
    rel <- as.numeric(tbl$col_rel_width)
    if (length(rel) != ncols) {
      stop(sprintf("`col_rel_width` length (%d) must match ncol (%d).",
                   length(rel), ncols), call. = FALSE)
    }
    if (any(rel <= 0)) stop("`col_rel_width` values must be positive.", call. = FALSE)
    widths <- as.integer(round(total_w * rel / sum(rel)))
    widths[ncols] <- total_w - sum(widths[-ncols])  # absorb rounding drift
    return(cumsum(widths))
  }

  # Equal distribution.
  cell_w <- max(1L, floor(total_w / ncols))
  widths  <- rep(cell_w, ncols)
  widths[ncols] <- total_w - sum(widths[-ncols])
  cumsum(widths)
}

# Total rendered width (twips) of a page's content, so the title / footnote
# block can be drawn as a table of the SAME width as the content above/below it.
#   * rtftable -> the column total (last cumulative \cellx position).
#   * rtfplot  -> the figure display width (mirrors .render_rtfplot()).
#   * otherwise (no content) -> the writable page width.
.content_width_twips <- function(ct, writable_width_twips) {
  if (inherits(ct, "rtftable")) {
    ref_df <- if (!is.null(ct$data_list)) ct$data_list[[1L]] else ct$data
    ncols  <- ncol(ref_df)
    if (is.null(ncols) || ncols == 0L) return(as.integer(writable_width_twips))
    cellx <- .compute_cellx(ncols, writable_width_twips, ct)
    return(as.integer(cellx[ncols]))
  }
  if (inherits(ct, "rtfplot")) {
    return(as.integer(.rtfplot_display_twips(ct)$w))
  }
  as.integer(writable_width_twips)
}

# Horizontal placement of the title / footnote table: follow the content so the
# block sits directly above / below it (table_align for tables, align for plots).
.content_align <- function(ct) {
  if (inherits(ct, "rtftable")) return(ct$table_align %||% "left")
  if (inherits(ct, "rtfplot"))  return(ct$align %||% "center")
  "left"
}

# -- Cell content builder -------------------------------------------------------

# Build the content string for one table cell (no \trowd / \cellx, just text+\cell).
# align: "left"|"right"|"center"
# All decoration flags are logical scalars.
.build_cell_content <- function(text, align = "left", bold = FALSE, italic = FALSE,
                                 underline = FALSE, indent_twips = 0L,
                                 pad_l = 72L, pad_r = 72L, color_idx = NULL,
                                 fs_cmd = "") {
  align_cmd <- switch(align, left = "\\ql", right = "\\qr", center = "\\qc", "\\ql")
  li <- as.integer(pad_l) + as.integer(indent_twips)
  ri <- as.integer(pad_r)

  # Apply decorations inside-out (outermost last so they wrap correctly).
  if (underline) text <- paste0("\\ul ", text, "\\ulnone ")
  if (italic)    text <- paste0("\\i ",  text, "\\i0 ")
  if (bold)      text <- paste0("\\b ",  text, "\\b0 ")
  # Text colour outermost: \cf<idx> ... \cf1 (reset to the default black at
  # colour-table index 1).
  if (!is.null(color_idx)) {
    text <- paste0("\\cf", as.integer(color_idx), " ", text, "\\cf1 ")
  }

  paste0(align_cmd, "\\li", li, "\\ri", ri, fs_cmd, " ", text, "\\cell")
}

# -- Row renderers --------------------------------------------------------------

# Build one complete RTF table row string.
# cell_defs:    character vector - one border+valign+\cellx string per column.
# cell_contents: character vector - one \q..\li..\ri.. text\cell string per column.
# row_height_twips: integer or NULL.
.build_row <- function(cell_defs, cell_contents, row_height_twips = NULL, table_align = "left") {
  rh <- if (!is.null(row_height_twips) && as.integer(row_height_twips) != 0L)
    paste0("\\trrh", as.integer(row_height_twips)) else ""
  align_cmd <- switch(table_align, center = "\\trqc", right = "\\trqr", "")
  paste0("\\trowd", rh, align_cmd, paste(cell_defs, collapse = ""), paste(cell_contents, collapse = ""), "\\row")
}

# Build cell definition strings (border + valign + \cellx) for all columns.
.build_cell_defs <- function(cellx, border_spec, valign_cmd,
                             color_index_map = NULL) {
  border_cmds <- .build_border_commands(border_spec, color_index_map)
  vapply(cellx, function(cx) paste0(border_cmds, valign_cmd, "\\cellx", cx), character(1L))
}

# Outer-frame border for a header row at position `idx` of `n` total.
#
#   * top      -> present only on the FIRST row
#   * bottom   -> present only on the LAST row
#   * left/right -> preserved on every row (per-cell behaviour is
#                  controlled by the renderer; this just passes them
#                  through unchanged)
#
# Returns NULL when nothing would be emitted.
.header_outer_border <- function(idx, n, zone_border) {
  if (is.null(zone_border)) return(NULL)
  is_first <- idx == 1L
  is_last  <- idx == n
  top <- if (is_first) zone_border$top    else NULL
  bot <- if (is_last)  zone_border$bottom else NULL
  lft <- zone_border$left
  rgt <- zone_border$right
  if (is.null(top) && is.null(bot) && is.null(lft) && is.null(rgt)) return(NULL)
  rtf_border(top = top, bottom = bot, left = lft, right = rgt)
}

# Render spanning-header row(s).
#
# spanning_header: list of spec lists with the following fields:
#   from, to     -- (required) 1-based column range covered by the cell.
#   label        -- (required) cell text.
#   align        -- (optional) "left" / "center" / "right".
#                   Default: inherit from col_spec[[from]]$header_align
#                   (the leftmost covered column's header alignment),
#                   which itself cascades from col_spec$align.
#                   Final fallback: "center".
#   bold         -- (optional) TRUE / FALSE.  Default FALSE.
#   italic       -- (optional) TRUE / FALSE.  Default FALSE.
#   underline    -- (optional) TRUE / FALSE.  Default FALSE.
#
# `border_spec` is the row-level border (only top on first / bottom on
# last header row, per .header_outer_border()).  `group_bottom_side`,
# when non-NULL, adds a bottom border to every spanning cell that
# covers more than one column -- the typical "underline the group
# label" look.  Pass NULL to suppress (e.g. when this spanning row is
# itself the last header row and the row-level bottom already covers
# the whole span).
# Per-column "cell id" coverage for one header row, so adjacent columns that
# belong to the same cell share an id.  Used to decide where the column
# grouping changes between two header rows.
#   * labels row (character vector) -> every column is its own cell.
#   * spanning row (list of {from,to,...}) -> covered columns share an id;
#     any uncovered (gap) column becomes its own single-column cell.
.header_row_coverage <- function(row, kind, ncols) {
  if (!identical(kind, "spanning")) return(seq_len(ncols))
  cov <- integer(ncols)
  k   <- 0L
  for (cell in row) {
    f <- as.integer(cell$from); t <- as.integer(cell$to)
    if (is.na(f) || is.na(t)) next
    k <- k + 1L
    cov[f:t] <- k
  }
  for (j in seq_len(ncols)) if (cov[j] == 0L) { k <- k + 1L; cov[j] <- k }
  cov
}

# Internal cell-break positions of a header row: position b (1..ncols-1) is a
# boundary when columns b and b+1 belong to different cells.
.header_row_boundaries <- function(cov) {
  n <- length(cov)
  if (n < 2L) return(integer(0))
  which(cov[-n] != cov[-1L])
}

# `next_boundaries`: the cell-break positions of the FOLLOWING header row (see
# .header_row_boundaries).  A multi-column spanning cell receives the group
# underline only when its column range is broken below -- i.e. some boundary in
# the next row falls strictly inside the span.  NULL means "unknown" and keeps
# the legacy behaviour of underlining every multi-column cell.
.render_spanning_rows <- function(spanning_header, cellx, border_spec,
                                   row_height_twips, pad_l, pad_r, valign_cmd,
                                   col_spec = NULL,
                                   table_align = "left",
                                   group_bottom_side = NULL,
                                   next_boundaries = NULL,
                                   markup = "script",
                                   color_index_map = NULL, fs_cmd = "") {
  if (is.null(spanning_header) || length(spanning_header) == 0L) return(character())
  ncols <- length(cellx)

  # Determine span coverage per column (0 = not spanned, k = span group index).
  coverage <- integer(ncols)
  for (k in seq_along(spanning_header)) {
    sp <- spanning_header[[k]]
    for (j in as.integer(sp$from):as.integer(sp$to)) {
      coverage[j] <- k
    }
  }

  # -- Per-cell border resolution ------------------------------------------
  #   row's outer frame (border_spec)
  #   + multi-col group bottom (if applicable)
  #   + col_spec[[from]]$border override
  .cell_border <- function(k, single_col_idx = NULL) {
    eff <- border_spec
    if (!is.null(k) && k > 0L) {
      sp        <- spanning_header[[k]]
      from_idx  <- as.integer(sp$from)
      to_idx    <- as.integer(sp$to)
      multi_col <- to_idx > from_idx
      # Underline a multi-column cell only when the grouping changes below it:
      # the next header row must break the span somewhere inside [from, to-1].
      # `next_boundaries = NULL` keeps the legacy "always underline" behaviour.
      span_broken <- is.null(next_boundaries) ||
        any(next_boundaries >= from_idx & next_boundaries <= to_idx - 1L)
      if (multi_col && !is.null(group_bottom_side) && span_broken) {
        if (is.null(eff)) eff <- rtf_border()
        eff <- .merge_rtf_border(eff, rtf_border(bottom = group_bottom_side))
      }
      if (!is.null(col_spec) && from_idx >= 1L && from_idx <= length(col_spec)) {
        cb <- col_spec[[from_idx]]$border
        if (!is.null(cb)) {
          eff <- if (is.null(eff)) cb else .merge_rtf_border(eff, cb)
        }
      }
      # A per-cell border on the header cell itself (col_cell(border = ...))
      # takes precedence over everything above -- including the automatic
      # group-underline -- so an author can fine-tune or remove individual
      # rules (see col_cell()).
      if (!is.null(sp$border)) {
        eff <- if (is.null(eff)) sp$border else .merge_rtf_border(eff, sp$border)
      }
    } else if (!is.null(single_col_idx) && !is.null(col_spec) &&
                single_col_idx <= length(col_spec)) {
      cb <- col_spec[[single_col_idx]]$border
      if (!is.null(cb)) {
        eff <- if (is.null(eff)) cb else .merge_rtf_border(eff, cb)
      }
    }
    eff
  }

  # Build cell definitions: spanned columns use merged width.
  cell_defs <- character()
  j <- 1L
  while (j <= ncols) {
    k <- coverage[j]
    if (k > 0L) {
      to_idx <- as.integer(spanning_header[[k]]$to)
      bc     <- .build_border_commands(.cell_border(k), color_index_map)
      cell_defs <- c(cell_defs, paste0(bc, valign_cmd, "\\cellx", cellx[to_idx]))
      j <- to_idx + 1L
    } else {
      bc <- .build_border_commands(.cell_border(NULL, single_col_idx = j),
                                   color_index_map)
      cell_defs <- c(cell_defs, paste0(bc, valign_cmd, "\\cellx", cellx[j]))
      j <- j + 1L
    }
  }

  # Resolve the alignment for a spanning cell.
  .span_align <- function(sp) {
    if (!is.null(sp$align)) return(sp$align)
    from_idx <- as.integer(sp$from)
    if (!is.null(col_spec) && from_idx >= 1L && from_idx <= length(col_spec)) {
      below <- col_spec[[from_idx]]$header_align
      if (!is.null(below) && nzchar(below)) return(below)
    }
    "center"
  }

  # Build cell contents.
  #
  # Apply decorations inside-out so the outermost wrapper closes last.
  # All of bold / italic / underline default to FALSE -- spanning labels
  # are rendered in normal weight unless explicitly opted in.
  cell_contents <- character()
  j <- 1L
  while (j <= ncols) {
    k <- coverage[j]
    if (k > 0L) {
      sp        <- spanning_header[[k]]
      label     <- .format_cell_text(sp$label %||% "", markup)
      if (isTRUE(sp$underline)) label <- paste0("\\ul ", label, "\\ulnone ")
      if (isTRUE(sp$italic))    label <- paste0("\\i ",  label, "\\i0 ")
      if (isTRUE(sp$bold))      label <- paste0("\\b ",  label, "\\b0 ")
      al        <- .span_align(sp)
      align_cmd <- switch(al, left = "\\ql", right = "\\qr",
                              center = "\\qc", "\\qc")
      cell_contents <- c(cell_contents,
        paste0(align_cmd, "\\li", pad_l, "\\ri", pad_r, fs_cmd, " ", label, "\\cell"))
      j <- as.integer(sp$to) + 1L
    } else {
      cell_contents <- c(cell_contents,
        paste0("\\ql\\li", pad_l, "\\ri", pad_r, fs_cmd, " \\cell"))
      j <- j + 1L
    }
  }

  .build_row(cell_defs, cell_contents, row_height_twips, table_align)
}

# Render one column-header row.
#
# Per-cell border resolution:
#   effective_border[j] = merge(zone_header_border, col_spec[[j]]$border)
# where the col_spec entry's non-NULL sides override the zone border.
# This lets users put e.g. a thicker bottom line under a single column
# without affecting the rest of the header.
.render_header_row <- function(hdr_labels, cellx, border_spec, row_height_twips,
                                pad_l, pad_r, valign_cmd, col_spec,
                                table_align = "left", markup = "script",
                                fs_cmd = "",
                                color_index_map = NULL) {
  ncols <- length(cellx)

  # Build per-cell definitions: each cell may have its own border.
  cell_defs <- vapply(seq_len(ncols), function(j) {
    col_border <- col_spec[[j]]$border
    eff_border <- if (!is.null(col_border)) {
      .effective_row_border(border_spec, col_border)
    } else {
      border_spec
    }
    bc <- .build_border_commands(eff_border, color_index_map)
    paste0(bc, valign_cmd, "\\cellx", cellx[j])
  }, character(1L))

  cell_contents <- vapply(seq_len(ncols), function(j) {
    spec  <- col_spec[[j]]
    text  <- .format_cell_text(if (j <= length(hdr_labels)) hdr_labels[[j]] else "", markup)
    bold  <- isTRUE(spec$header_bold)
    itl   <- isTRUE(spec$header_italic)
    align <- spec$header_align %||% "center"
    .build_cell_content(text, align, bold, itl, FALSE, 0L, pad_l, pad_r,
                        fs_cmd = fs_cmd)
  }, character(1L))
  .build_row(cell_defs, cell_contents, row_height_twips, table_align)
}

# Resolve one body cell's text and styling into an RTF cell-content string.
#
# `spec` is the column's entry from col_spec; `j` indexes the per-column
# override vectors in `row_cell_styles` (non-NA entries win over col_spec).
# `force_align`, when non-NULL, overrides both -- the decimal-split halves use
# it so a per-cell align override cannot break the point alignment.
.data_cell_content <- function(spec, raw_val, j, row_cell_styles,
                               pad_l, pad_r, markup, color_index_map,
                               force_align = NULL, fs_cmd = "") {
  text        <- .format_cell_text(if (is.na(raw_val)) "" else as.character(raw_val), markup)
  align       <- spec$align       %||% "left"
  bold        <- isTRUE(spec$bold)
  itl         <- isTRUE(spec$italic)
  ul          <- isTRUE(spec$underline)
  indent      <- as.integer(spec$indent_twips %||% 0L)
  color_hex   <- spec$color       %||% NULL          # per-column text colour
  # Per-cell style overrides: non-NA entries win over col_spec defaults.
  if (!is.null(row_cell_styles)) {
    cs <- row_cell_styles
    if (!is.null(cs$align) && j <= length(cs$align) &&
        !is.na(cs$align[j]))
      align  <- as.character(cs$align[j])
    if (!is.null(cs$bold) && j <= length(cs$bold) &&
        !is.na(cs$bold[j]))
      bold   <- isTRUE(cs$bold[j])
    if (!is.null(cs$italic) && j <= length(cs$italic) &&
        !is.na(cs$italic[j]))
      itl    <- isTRUE(cs$italic[j])
    if (!is.null(cs$underline) && j <= length(cs$underline) &&
        !is.na(cs$underline[j]))
      ul     <- isTRUE(cs$underline[j])
    if (!is.null(cs$indent_twips) && j <= length(cs$indent_twips) &&
        !is.na(cs$indent_twips[j]))
      indent <- as.integer(cs$indent_twips[j])
    if (!is.null(cs$color) && j <= length(cs$color) && !is.na(cs$color[j]))
      color_hex <- cs$color[j]
  }
  if (!is.null(force_align)) align <- force_align
  # Resolve the colour hex to a colour-table index (NULL -> default black).
  color_idx <- if (!is.null(color_hex) && !is.null(color_index_map))
                 color_index_map[[color_hex]] else NULL
  .build_cell_content(text, align, bold, itl, ul, indent, pad_l, pad_r,
                      color_idx = color_idx, fs_cmd = fs_cmd)
}

# Render one data row.
# row_cell_styles: NULL, or a list with optional per-column vectors
#   $bold, $italic, $underline (logical), $indent_twips (integer),
#   $align, $color (character), and $border (a list of rtf_border / NULL) --
#   each of length ncols.  NA / NULL entries mean "no override; fall back to
#   col_spec (or, for $border, the row's zone border)".  Non-NA values win
#   over col_spec for that column.
#
# `dsplit_row`: NULL, or the output of .decimal_split_row() -- the row's cells
#   already expanded onto the decimal-split geometry.  When supplied, `vals`,
#   `cellx` and `col_spec` are the EXPANDED ones and rendering is delegated to
#   .render_data_row_split(); when NULL every code path below is the original,
#   untouched one.
.render_data_row <- function(vals, cellx, border_spec, row_height_twips,
                              pad_l, pad_r, valign_cmd, col_spec,
                              table_align = "left",
                              row_cell_styles = NULL,
                              color_index_map = NULL,
                              markup = "script",
                              dsplit_row = NULL, dsplit = NULL, fs_cmd = "") {
  if (!is.null(dsplit_row)) {
    return(.render_data_row_split(
      vals, cellx, border_spec, row_height_twips, pad_l, pad_r, valign_cmd,
      col_spec, table_align, row_cell_styles, color_index_map, markup,
      dsplit_row, dsplit))
  }
  ncols <- length(cellx)

  # Per-cell border overrides (cell_styles$border): each cell's rtf_border
  # merges ON TOP of the row's resolved zone border (body x first/last row),
  # per side -- the same last-writer-wins chain the header renderers use.
  # Rows without an override keep the shared fast path.
  cell_borders <- if (!is.null(row_cell_styles)) row_cell_styles$border
  if (is.list(cell_borders) &&
      !all(vapply(cell_borders, is.null, logical(1L)))) {
    cell_defs <- vapply(seq_len(ncols), function(j) {
      b   <- if (j <= length(cell_borders)) cell_borders[[j]]
      eff <- if (is.null(b)) border_spec
             else .effective_row_border(border_spec, b)
      paste0(.build_border_commands(eff, color_index_map), valign_cmd,
             "\\cellx", cellx[j])
    }, character(1L))
  } else {
    cell_defs <- .build_cell_defs(cellx, border_spec, valign_cmd,
                                  color_index_map)
  }
  cell_contents <- vapply(seq_len(ncols), function(j) {
    .data_cell_content(col_spec[[j]],
                       if (j <= length(vals)) vals[[j]] else NA,
                       j, row_cell_styles, pad_l, pad_r, markup,
                       color_index_map, fs_cmd = fs_cmd)
  }, character(1L))
  .build_row(cell_defs, cell_contents, row_height_twips, table_align)
}

# -- rtftable renderer ----------------------------------------------------------

# Render one data.frame section of an rtftable (headers + data rows).
# Used by both single-DF and multi-DF render paths.
# cell_styles: NULL, or a list of length nrow(df) where each element is
#   NULL (no override) or list($bold, $italic, $underline, $indent_twips).
.render_rtftable_section <- function(
    df, col_headers, cellx, border, col_spec,
    hdr_h, data_h, blank_h, blank_set,
    pad_l, pad_r, valign_cmd,
    spanning_header, table_align = "left",
    cell_styles = NULL, color_index_map = NULL,
    blank_row_normalize = character(0), markup = "script",
    dsplit = NULL, dsplit_offset = 0L, fs_cmd = "") {

  # The whole column-header block always renders over the ORIGINAL column
  # geometry -- only data rows use the decimal-split one (`dsplit$cellx`).
  ncols <- length(cellx)
  nrows <- nrow(df)
  lines <- character()

  # -- Column-header block -------------------------------------------------
  #
  # The whole header block (the legacy standalone `spanning_header`
  # argument plus every entry of `col_headers`) is treated as a single
  # outer-framed unit:
  #
  #   * `border$header$top`     is applied only to the topmost row.
  #   * `border$header$bottom`  is applied only to the bottommost row.
  #   * Intermediate rows carry no row-level top/bottom border by
  #     default (per-column overrides via `col_spec[[j]]$border`
  #     still apply, and so do explicit `border$header$left/right`).
  #
  # On any spanning row that is *not* the last header row, every cell
  # covering more than one column additionally receives a bottom border
  # -- the typical clinical TFL "underline the group label" look.
  #
  # `border$spanning`, when supplied, takes precedence over
  # `border$header` for spanning rows.
  hdr_border  <- border$header
  span_border <- border$spanning %||% border$header

  header_rows <- list()
  header_kind <- character()
  if (!is.null(spanning_header)) {
    header_rows[[length(header_rows) + 1L]] <- spanning_header
    header_kind <- c(header_kind, "spanning")
  }
  for (hdr_row in col_headers) {
    is_spanning <- is.list(hdr_row) && length(hdr_row) > 0L &&
                   is.list(hdr_row[[1L]]) && !is.null(hdr_row[[1L]]$from)
    header_rows[[length(header_rows) + 1L]] <- hdr_row
    header_kind <- c(header_kind, if (is_spanning) "spanning" else "labels")
  }
  n_hdr_rows <- length(header_rows)

  for (idx in seq_len(n_hdr_rows)) {
    hdr_row <- header_rows[[idx]]
    kind    <- header_kind[[idx]]
    zone    <- if (kind == "spanning") span_border else hdr_border
    row_b   <- .header_outer_border(idx, n_hdr_rows, zone)
    if (kind == "spanning") {
      # Boundaries of the next header row -- used to underline a spanning cell
      # only where the column grouping actually changes below it.
      nb <- if (idx < n_hdr_rows) {
        .header_row_boundaries(
          .header_row_coverage(header_rows[[idx + 1L]],
                               header_kind[[idx + 1L]], ncols))
      } else {
        integer(0)
      }
      lines <- c(lines, .render_spanning_rows(
        hdr_row, cellx, row_b, hdr_h, pad_l, pad_r, valign_cmd,
        col_spec = col_spec, table_align = table_align,
        group_bottom_side = if (idx < n_hdr_rows) {
          (hdr_border$bottom %||% span_border$bottom %||% rtf_border_side())
        } else {
          NULL   # last header row's outer frame already supplies the bottom
        },
        next_boundaries = nb, fs_cmd = fs_cmd,
        markup = markup,
        color_index_map = color_index_map
      ))
    } else {
      lines <- c(lines, .render_header_row(
        hdr_row, cellx, row_b, hdr_h, pad_l, pad_r, valign_cmd,
        col_spec, table_align, markup = markup, fs_cmd = fs_cmd,
        color_index_map = color_index_map
      ))
    }
  }

  align_cmd <- switch(table_align, center = "\\trqc", right = "\\trqr", "")
  .blank_row_rtf <- function() {
    total_w <- cellx[ncols]
    paste0("\\trowd\\trgaph0\\trleft0\\trrh", blank_h,
           align_cmd, "\\clvertalb\\cellx", total_w,
           " \\cell\\row")
  }

  # Data rows, with blank separator rows spliced in.
  #
  # blank_row_normalize controls two render-time behaviours:
  #   "detect"   -- a data row whose every cell is NA / "" (empty or ASCII
  #                 whitespace) is emitted as a single full-width blank row
  #                 (via .blank_row_rtf()) rather than one empty cell per column.
  #   "collapse" -- a run of >= 2 consecutive blank rows (separator rows and/or
  #                 detected empty data rows) is reduced to a single blank row.
  detect_blank   <- "detect"   %in% blank_row_normalize
  collapse_blank <- "collapse" %in% blank_row_normalize

  # A cell is "empty" when NA or, after trimming ASCII whitespace, "".  NBSP
  # (U+00A0) is NOT ASCII whitespace, so an indent-only cell is not empty.
  .row_all_empty <- function(row) {
    vals <- as.character(unlist(row, use.names = FALSE))
    all(is.na(vals) | !nzchar(trimws(vals)))
  }

  out        <- character()
  prev_blank <- FALSE
  add_blank  <- function() {
    if (collapse_blank && prev_blank) return(invisible())
    out        <<- c(out, .blank_row_rtf())
    prev_blank <<- TRUE
  }
  add_data <- function(txt) {
    out        <<- c(out, txt)
    prev_blank <<- FALSE
  }

  if (0L %in% blank_set) add_blank()

  for (i in seq_len(nrows)) {
    if (detect_blank && .row_all_empty(df[i, , drop = FALSE])) {
      add_blank()
    } else {
      # When nrows == 1 the only row is BOTH the first and the last row;
      # merge both overrides so e.g. the TFL "bottom on last row" still
      # applies to a single-row table.
      row_border <- border$body
      if (i == 1L && !is.null(border$first_row)) {
        row_border <- .effective_row_border(row_border, border$first_row)
      }
      if (i == nrows && !is.null(border$last_row)) {
        row_border <- .effective_row_border(row_border, border$last_row)
      }
      rcs <- if (!is.null(cell_styles) && i <= length(cell_styles))
               cell_styles[[i]] else NULL
      row_vals <- as.list(df[i, , drop = FALSE])
      if (is.null(dsplit)) {
        add_data(.render_data_row(
          row_vals,
          cellx, row_border, data_h, pad_l, pad_r, valign_cmd, col_spec, table_align,
          row_cell_styles = rcs,
          color_index_map = color_index_map,
          markup = markup, fs_cmd = fs_cmd
        ))
      } else {
        drow <- .decimal_split_row(dsplit, row_vals, dsplit_offset + i)
        add_data(.render_data_row(
          drow$vals,
          dsplit$cellx, row_border, data_h, pad_l, pad_r, valign_cmd,
          dsplit$col_spec, table_align,
          row_cell_styles = .decimal_split_cell_styles(dsplit, rcs),
          color_index_map = color_index_map,
          markup = markup,
          dsplit_row = drow, dsplit = dsplit, fs_cmd = fs_cmd
        ))
      }
    }
    if (i %in% blank_set) add_blank()
  }

  c(lines, out)
}

# Render an rtftable object to a character vector of RTF row strings.
# Handles both single-DF and multi-DF modes transparently.
# font_half_points drives the default row-height lookup when the table does
# not specify an explicit row_height_twips.
.render_rtftable <- function(tbl, writable_width_twips, font_half_points = 18L,
                             color_index_map = NULL,
                             doc_row_height = NULL, doc_pad_l = 0L, doc_pad_r = 0L,
                             doc_markup = "script") {
  border   <- tbl$border
  col_spec <- tbl$col_spec
  # Markup: the table's own setting wins; otherwise inherit the document default.
  eff_markup <- if (!is.null(tbl$markup)) tbl$markup else doc_markup
  # Cell padding: the table's own value wins; otherwise inherit the document
  # default (which already folds in the option / resource baseline).
  pad_l    <- tbl$cell_padding_left_twips  %||% doc_pad_l
  pad_r    <- tbl$cell_padding_right_twips %||% doc_pad_r

  cmds       <- .load_rtf_commands()
  valign_cmd <- cmds$cell_valign[[tbl$cell_valign]] %||% "\\clvertalb"

  # Determine column count from whichever mode is active.
  ref_df <- if (!is.null(tbl$data_list)) tbl$data_list[[1L]] else tbl$data
  ncols  <- ncol(ref_df)

  if (ncols == 0L) return(cmds$paragraph$empty_table)

  cellx <- .compute_cellx(ncols, writable_width_twips, tbl)

  # Decimal-point alignment (set_decimal_split()): a render-time expansion of
  # the BODY geometry only.  NULL whenever the feature is off or no selected
  # column actually carries a decimal separator -- then every renderer below
  # takes its original path.
  dsplit <- .decimal_split_plan(tbl, cellx, col_spec, eff_markup)

  # Resolve the effective body row height:
  #   * explicit positive integer  -> use as given
  #   * 0L                          -> legacy "automatic" (no \trrh emitted)
  #   * NULL                        -> apply the document-wide default
  # Font size and row height resolve together (#292): a table that sets its own
  # font size gets the height that size implies, rather than inheriting a
  # document height chosen for a different one.
  metrics   <- .resolve_element_metrics(tbl$font_size_half_points,
                                        tbl$row_height_twips,
                                        font_half_points, doc_row_height)
  effective <- metrics$rh
  fs_cmd    <- .fs_cmd_for(metrics$fs, font_half_points)

  # Apply row_height_exact flag: negate twips value to signal \trrh exact height.
  .apply_exact <- function(h) {
    if (isTRUE(tbl$row_height_exact) && !is.null(h) && as.integer(h) > 0L)
      -as.integer(h)
    else
      h
  }
  hdr_h   <- .apply_exact(tbl$header_row_height_twips %||% effective)
  data_h  <- .apply_exact(effective)
  blank_h <- .apply_exact(tbl$blank_row_height_twips %||% effective)

  blank_set <- if (!is.null(tbl$blank_rows)) tbl$blank_rows else integer(0)

  table_align <- tbl$table_align %||% "left"

  if (!is.null(tbl$data_list)) {
    # -- Multi-DF mode ----------------------------------------------------------
    lines  <- character()
    cs_all <- tbl$cell_styles   # flat list covering all rows across DFs
    row_offset <- 0L
    for (df_i in seq_along(tbl$data_list)) {
      df      <- tbl$data_list[[df_i]]
      # Per-DF header (NULL -> use column names of this DF).
      hdr_spec    <- tbl$col_header_list[[df_i]]
      col_headers <- hdr_spec %||% list(names(df))
      n_this <- nrow(df)

      # Slice the cell_styles window for this DF section.
      cs_section <- if (!is.null(cs_all) && row_offset < length(cs_all)) {
        cs_all[seq_len(n_this) + row_offset]
      } else NULL

      lines <- c(lines, .render_rtftable_section(
        df          = df,
        col_headers = col_headers,
        cellx       = cellx,
        border      = border,
        col_spec    = col_spec,
        hdr_h       = hdr_h,
        data_h      = data_h,
        blank_h     = blank_h,
        blank_set   = blank_set,
        pad_l       = pad_l,
        pad_r       = pad_r,
        valign_cmd  = valign_cmd,
        spanning_header = tbl$spanning_header,
        table_align     = table_align,
        cell_styles     = cs_section,
        color_index_map = color_index_map,
        blank_row_normalize = tbl$blank_row_normalize %||% character(0),
        markup = eff_markup, fs_cmd = fs_cmd,
        dsplit = dsplit, dsplit_offset = row_offset
      ))
      row_offset <- row_offset + n_this
    }
    return(lines)
  }

  # -- Single-DF mode ---------------------------------------------------------
  df          <- tbl$data
  col_headers <- tbl$col_header %||% list(names(df))

  .render_rtftable_section(
    df          = df,
    col_headers = col_headers,
    cellx       = cellx,
    border      = border,
    col_spec    = col_spec,
    hdr_h       = hdr_h,
    data_h      = data_h,
    blank_h     = blank_h,
    blank_set   = blank_set,
    pad_l       = pad_l,
    pad_r       = pad_r,
    valign_cmd  = valign_cmd,
    spanning_header = tbl$spanning_header,
    table_align     = table_align,
    cell_styles     = tbl$cell_styles,
    color_index_map = color_index_map,
    blank_row_normalize = tbl$blank_row_normalize %||% character(0),
    markup = eff_markup,
    dsplit = dsplit, dsplit_offset = 0L, fs_cmd = fs_cmd
  )
}

# -- rtfplot renderer -----------------------------------------------------------

.render_rtfplot <- function(plot_obj, writable_width_twips) {
  cmds <- .load_rtf_commands()

  # Display dimensions: native size at the image's DPI (100%) unless the user
  # gave an explicit width_twips / height_twips. See .rtfplot_display_twips().
  disp   <- .rtfplot_display_twips(plot_obj)
  disp_w <- disp$w
  disp_h <- disp$h

  # Read file and convert to uppercase hex.
  raw  <- readBin(plot_obj$path, "raw", n = file.info(plot_obj$path)$size)
  # Split into 126-char lines for readability (standard RTF practice).
  hex_chars <- toupper(paste(format(as.hexmode(as.integer(raw)), width = 2L), collapse = ""))
  hex_lines <- character()
  start <- 1L
  while (start <= nchar(hex_chars)) {
    hex_lines <- c(hex_lines, substr(hex_chars, start, start + 125L))
    start <- start + 126L
  }
  hex_block <- paste(hex_lines, collapse = "\n")

  align_cmd <- switch(plot_obj$align %||% "center",
    left   = "\\ql",
    right  = "\\qr",
    center = "\\qc",
    "\\qc"
  )

  template <- if (plot_obj$img_type == "png") cmds$picture$png_template else cmds$picture$jpeg_template
  .cmd_fmt(template, list(
    align    = align_cmd,
    picw     = plot_obj$img_width,
    pich     = plot_obj$img_height,
    picwgoal = disp_w,
    pichgoal = disp_h,
    hex      = hex_block
  ))
}

# -- Header / footer renderer (unchanged from original) ------------------------

# Does any text cell of a normalised header/footer contain `{PAGE}`?
# Used to decide whether each sub-page of an rtf_section needs its own
# RTF section break so the static `{PAGE}` token can bake the actual
# sub-page number (not the section's first-page number).
#
# NB: matches *only* the literal `{PAGE}` token, not `{AUTO_PAGE}` --
#   we treat them as separate regexes by using a word-ish boundary check.
.uses_static_page_token <- function(hf) {
  if (is.null(hf) || length(hf$rows) == 0L) return(FALSE)
  for (row in hf$rows) {
    txt <- if (is.list(row) && !is.null(row$columns)) row$columns else row
    txt <- as.character(txt)
    for (cell in txt) {
      if (is.na(cell)) next
      # `{PAGE}` only -- exclude {AUTO_PAGE} via a non-A character (or BOS) before P.
      if (grepl("(?:^|[^A-Z_])\\{PAGE\\}", cell, perl = TRUE)) return(TRUE)
    }
  }
  FALSE
}

# Normalize a section header/footer value to list(rows=list(...), width_twips=NULL).
# Accepts: NULL | plain named vector (single row) | list(rows=list(...)) | list(columns=c(...)) (legacy).
.normalize_hf <- function(hf) {
  if (is.null(hf)) return(NULL)
  # Already multi-row form
  if (is.list(hf) && !is.null(hf$rows)) return(hf)
  # Legacy single-row list(columns = c(...))
  if (is.list(hf) && !is.null(hf$columns)) return(list(rows = list(hf), width_twips = NULL))
  # Plain named vector -- single row
  list(rows = list(hf), width_twips = NULL)
}

.render_header_footer <- function(hf, writable_width_twips, is_footer = FALSE,
                                   current_page = NULL, total_pages = NULL,
                                   color_index_map = NULL,
                                   font_half_points = 18L,
                                   doc_row_height = NULL,
                                   doc_pad_l = NULL, doc_pad_r = NULL) {
  if (is.null(hf) || length(hf$rows) == 0L) {
    return(character())
  }

  rows        <- hf$rows
  hf_border   <- hf$border   # rtf_border or NULL
  # Width: the absolute `width_twips` wins (the legacy form), then the shared
  # `width` vocabulary, then the writable width.  A header/footer band has no
  # table body above it, so "content" resolves to the writable width too.
  width       <- if (!is.null(hf$width_twips)) {
    as.integer(hf$width_twips)
  } else if (!is.null(hf$width)) {
    .resolve_block_width(hf$width, writable_width_twips, writable_width_twips,
                         default = "page")
  } else {
    writable_width_twips
  }
  cmds <- .load_rtf_commands()
  table_cmd <- cmds$table
  align_cmd <- cmds$alignment

  # Row height: the band's own value wins; otherwise the document default;
  # otherwise the font-aware baseline.
  # Font size and row height resolve together (#292): a band that sets its own
  # size gets the height that size implies.
  hf_metrics <- .resolve_element_metrics(hf$font_size_half_points,
                                         hf$row_height_twips,
                                         font_half_points, doc_row_height)
  rh_full <- hf_metrics$rh
  hf_fs   <- .fs_cmd_for(hf_metrics$fs, font_half_points)
  rh_str  <- .cmd_fmt(table_cmd$row_height_template,
                       list(row_height_twips = rh_full))

  # Cell padding (left/right): the band's own value wins; otherwise inherit the
  # document default (which folds in the option / resource baseline).
  defaults <- .load_rtfreporter_defaults()
  pad_l <- as.integer(hf$cell_padding_left_twips  %||% doc_pad_l %||%
                       defaults$default_cell_padding_left_twips  %||% 0L)
  pad_r <- as.integer(hf$cell_padding_right_twips %||% doc_pad_r %||%
                       defaults$default_cell_padding_right_twips %||% 0L)

  out_rows <- character()
  for (row_idx in seq_along(rows)) {
    row_def  <- rows[[row_idx]]
    # Accept plain vector c(l=..., r=...) directly, or legacy list(columns=c(...)).
    cols_vec <- if (is.list(row_def) && !is.null(row_def$columns)) row_def$columns else row_def
    if (is.null(cols_vec) || length(cols_vec) == 0L) {
      cols_vec <- c("")
    }

    col_names <- names(cols_vec)
    if (is.null(col_names)) col_names <- rep("", length(cols_vec))

    has_l <- "l" %in% col_names
    has_r <- "r" %in% col_names
    has_c <- "c" %in% col_names

    # Column count rules:
    # - c present with l or r -> 3 columns (fill missing with "")
    # - l + r (no c)          -> 2 columns
    # - single key only       -> 1 column
    # - unnamed               -> count-based defaults
    if (has_c && (has_l || has_r)) {
      n_cols <- 3L
      aligns <- c("left", "center", "right")
      cols_display <- c(
        if (has_l) cols_vec[col_names == "l"][[1L]] else "",
        cols_vec[col_names == "c"][[1L]],
        if (has_r) cols_vec[col_names == "r"][[1L]] else ""
      )
    } else if (has_l && has_r) {
      n_cols <- 2L; aligns <- c("left", "right")
      cols_display <- c(cols_vec[col_names == "l"][[1L]], cols_vec[col_names == "r"][[1L]])
    } else if (has_c) {
      n_cols <- 1L; aligns <- "center"
      cols_display <- c(cols_vec[col_names == "c"][[1L]])
    } else if (has_l) {
      n_cols <- 1L; aligns <- "left"
      cols_display <- c(cols_vec[col_names == "l"][[1L]])
    } else if (has_r) {
      n_cols <- 1L; aligns <- "right"
      cols_display <- c(cols_vec[col_names == "r"][[1L]])
    } else {
      n_cols <- length(cols_vec)
      if (n_cols > 3L) stop("Header/footer supports up to 3 columns per row.", call. = FALSE)
      aligns <- if (n_cols == 1L) {
        "center"
      } else if (n_cols == 2L) c("left", "right") else c("left", "center", "right")
      cols_display <- cols_vec
    }

    cell_w <- floor(width / n_cols)
    cellx  <- cumsum(rep(cell_w, n_cols))
    # Border applies only to the first row of the header/footer block.
    row_border_cmds <- if (row_idx == 1L) .build_border_commands(hf_border, color_index_map) else ""

    row <- table_cmd$row_start
    row <- paste0(row, rh_str)
    for (cx in cellx) {
      if (nzchar(row_border_cmds)) {
        row <- paste0(row, row_border_cmds, "\\cellx", cx)
      } else {
        row <- paste0(row, .cmd_fmt(table_cmd$cell_boundary_template, list(cx = cx)))
      }
    }
    for (i in seq_len(n_cols)) {
      at <- switch(aligns[i], left = align_cmd$left, right = align_cmd$right,
                   center = align_cmd$center, align_cmd$default)
      txt <- .render_tokens(cols_display[i], current_page = current_page, total_pages = total_pages)
      row <- paste0(row,
                    at, "\\li", pad_l, "\\ri", pad_r, hf_fs, " ", txt, "\\cell")
    }
    row <- paste0(row, table_cmd$row_end)
    out_rows <- c(out_rows, row)
  }

  out_rows
}

# -- Color table helpers --------------------------------------------------------

# Collect all unique hex color strings used in border specs across the report.
# Returns a character vector of unique "#RRGGBB" strings (may be empty).
.collect_report_colors <- function(report) {
  cols <- character(0)

  .hf_colors <- function(hf) {
    if (is.null(hf)) return(character(0))
    b <- hf$border
    if (is.null(b) || !inherits(b, "rtf_border")) return(character(0))
    .collect_border_colors(b)
  }

  # Colours of the per-cell borders inside one col_header rows list.
  .header_border_colors <- function(rows) {
    if (is.null(rows) || !is.list(rows)) return(character(0))
    unlist(lapply(rows, function(row) {
      if (!is.list(row)) return(character(0))
      unlist(lapply(row, function(cell) {
        if (is.list(cell)) .collect_border_colors(cell$border) else character(0)
      }), use.names = FALSE)
    }), use.names = FALSE)
  }

  .tbl_colors <- function(tbl) {
    if (!inherits(tbl, "rtftable")) return(character(0))
    out <- character(0)
    tb <- tbl$border
    if (!is.null(tb) && inherits(tb, "rtf_table_border")) {
      out <- c(out, .collect_table_border_colors(tb))
    }
    # Per-column text colours and border colours (col_spec[[j]]).
    if (!is.null(tbl$col_spec)) {
      out <- c(out, unlist(lapply(tbl$col_spec, function(s) s$color),
                           use.names = FALSE))
      out <- c(out, unlist(lapply(tbl$col_spec, function(s)
        .collect_border_colors(s$border)), use.names = FALSE))
    }
    # Header-cell borders (col_cell(border = ) / adapter-read styles).
    out <- c(out, .header_border_colors(tbl$col_header))
    if (!is.null(tbl$col_header_list)) {
      out <- c(out, unlist(lapply(tbl$col_header_list, .header_border_colors),
                           use.names = FALSE))
    }
    if (!is.null(tbl$spanning_header)) {
      out <- c(out, .header_border_colors(list(tbl$spanning_header)))
    }
    # Per-cell text and border colours (cell_styles[[i]]).
    if (!is.null(tbl$cell_styles)) {
      out <- c(out, unlist(lapply(tbl$cell_styles, function(cs) {
        if (is.list(cs)) cs$color else NULL
      }), use.names = FALSE))
      out <- c(out, unlist(lapply(tbl$cell_styles, function(cs) {
        if (!is.list(cs) || !is.list(cs$border)) return(character(0))
        unlist(lapply(cs$border, .collect_border_colors), use.names = FALSE)
      }), use.names = FALSE))
    }
    out[!is.na(out)]
  }

  # Document-declared palette (rtf_document(color_table = ...)): make these
  # colours available by index even if no element references them yet.
  ct_doc <- report$document$color_table
  if (!is.null(ct_doc)) cols <- c(cols, as.character(ct_doc))

  for (sec in report$sections) {
    cols <- c(cols, .hf_colors(.normalize_hf(sec$header)))
    cols <- c(cols, .hf_colors(.normalize_hf(sec$footer)))
  }
  for (pg in report$pages) {
    ct <- pg$content
    if (inherits(ct, "rtftable")) cols <- c(cols, .tbl_colors(ct))
    cols <- c(cols, .text_block_colors(pg$title,    is_footer = FALSE))
    cols <- c(cols, .text_block_colors(pg$footnote, is_footer = TRUE))
  }
  cols <- cols[!is.na(cols) & nzchar(cols)]
  # Black / white already occupy the reserved colour-table slots (index 1 / 2),
  # so drop them: declaring them adds nothing, and it keeps the default
  # color_table = "#000000" a no-op (no redundant palette entry).
  cols <- cols[!toupper(cols) %in% c("#000000", "#FFFFFF")]
  unique(cols)
}

# Resolve sections to sorted list with from_page / to_page ranges.
# Returns a list of list(header, footer, from_page, to_page).
.resolve_sections <- function(report) {
  n_pages <- length(report$pages)
  if (n_pages == 0L) return(list())

  sections <- report$sections
  from_pages <- vapply(sections, function(s) {
    fp <- s$from_page
    if (is.null(fp) || is.na(fp)) NA_integer_ else as.integer(fp)
  }, integer(1L))

  # Single section with no from_page -> covers all pages
  if (length(sections) == 1L && is.na(from_pages[1L])) from_pages[1L] <- 1L

  # Sort by from_page (NAs last, assigned sequentially)
  ord        <- order(from_pages, na.last = TRUE)
  sections   <- sections[ord]
  from_pages <- from_pages[ord]
  from_pages[1L] <- 1L  # first section always starts at page 1

  n_sec    <- length(sections)
  to_pages <- c(if (n_sec > 1L) from_pages[-1L] - 1L else integer(0L), n_pages)

  lapply(seq_len(n_sec), function(i) {
    list(
      header    = sections[[i]]$header,
      footer    = sections[[i]]$footer,
      from_page = from_pages[i],
      to_page   = to_pages[i]
    )
  })
}

# Compute the rendered width of a content block in twips.
.compute_content_width <- function(content, writable_width) {
  if (is.null(content)) return(writable_width)
  if (inherits(content, "rtftable")) {
    if (!is.null(content$column_widths_twips)) return(sum(as.integer(content$column_widths_twips)))
    if (!is.null(content$table_width_twips))   return(as.integer(content$table_width_twips))
    if (!is.null(content$table_width_pct_of_writable)) {
      return(as.integer(round(writable_width * content$table_width_pct_of_writable)))
    }
  }
  if (inherits(content, "rtfplot")) {
    return(as.integer(.rtfplot_display_twips(content)$w))
  }
  writable_width
}

# -- Title / footnote: content-width tables ----------------------------------
#
# Both blocks render as a SINGLE-COLUMN table whose width equals the content's
# rendered width (see .content_width_twips()), so they line up with the content.
# Each input row becomes one table row.  A block is either:
#   * a character vector  -- each entry is a row with the block's default style, or
#   * a list of rows      -- each row a string (default style) or a named list
#     list(text=, align=, bold=, italic=, underline=, color=, border=).
# An empty string ("") yields a blank row. (Legacy "{...}" tokens are NOT
# special -- they render literally, matching the established behaviour.)
#
#   title:    default centre + bold, no border.  NULL -> one blank row (a small
#             gap between the page header and the content).  character(0) -> none.
#   footnote: default left, normal weight; the FIRST row carries a top rule
#             (separator) unless that row sets its own `border`.  NULL /
#             character(0) -> suppress entirely.

# Normalize a title/footnote block to a list of resolved row records:
#   list(text, align, bold, italic, underline, color, border, blank, half)
.normalize_text_block <- function(block, is_footer, align = NULL) {
  def_align <- align %||% (if (is_footer) "left" else "center")
  def_bold  <- !is_footer
  if (is.null(block)) {
    # Title: a single blank gap row. Footnote: nothing.
    if (is_footer) return(list())
    block <- ""
  }
  if (length(block) == 0L) return(list())
  rows <- if (is.list(block)) block else as.list(block)

  out <- lapply(rows, function(r) {
    if (is.null(r) || (is.character(r) && length(r) == 1L)) {
      txt <- if (is.null(r)) "" else r
      rec <- list(text = txt, align = def_align, bold = def_bold,
                  italic = FALSE, underline = FALSE, color = NULL, border = NULL)
    } else if (is.list(r)) {
      rec <- list(
        text      = r$text %||% "",
        align     = r$align %||% def_align,
        bold      = if (is.null(r$bold)) def_bold else isTRUE(r$bold),
        italic    = isTRUE(r$italic),
        underline = isTRUE(r$underline),
        color     = r$color,
        border    = r$border
      )
      if (!is.null(rec$border) && !inherits(rec$border, "rtf_border")) {
        stop("A title/footnote row `border` must be NULL or an rtf_border() object.",
             call. = FALSE)
      }
    } else {
      stop("Each title/footnote row must be a string or a named list.",
           call. = FALSE)
    }
    # An empty string is a blank row; everything else is literal text.
    rec$blank <- !nzchar(rec$text)
    rec
  })

  # Footnote separator: a top rule on the first row unless it sets its own border.
  if (is_footer && length(out) >= 1L && is.null(out[[1L]]$border)) {
    out[[1L]]$border <- rtf_border(top = rtf_border_side(style = "single", width = 15L))
  }
  out
}

# Hex colours referenced by a title/footnote block (row text + row borders),
# for the document colour table.
.text_block_colors <- function(block, is_footer) {
  rows <- tryCatch(.normalize_text_block(block, is_footer), error = function(e) list())
  cols <- character(0)
  for (rec in rows) {
    if (!is.null(rec$color)) cols <- c(cols, rec$color)
    if (!is.null(rec$border)) cols <- c(cols, .collect_border_colors(rec$border))
  }
  cols
}

# Render a title/footnote block as a single-column, content-width table.
.render_text_block_table <- function(block, total_width_twips, is_footer,
                                      font_half_points, pad_l, pad_r,
                                      valign_cmd, table_align,
                                      color_index_map = NULL,
                                      doc_row_height = NULL,
                                      markup = "script", style = NULL) {
  rows <- .normalize_text_block(block, is_footer, style$align)
  if (length(rows) == 0L) return(character())

  # Title / footnote rows inherit the document default row height, else the
  # font-aware baseline.
  m      <- .resolve_element_metrics(style$font_size_half_points,
                                     style$row_height_twips,
                                     font_half_points, doc_row_height)
  full_h <- m$rh
  fs_cmd <- .fs_cmd_for(m$fs, font_half_points)
  if (!is.null(style$markup)) markup <- style$markup
  cellx  <- as.integer(total_width_twips)

  vapply(rows, function(rec) {
    cell_def <- paste0(.build_border_commands(rec$border, color_index_map),
                       valign_cmd, "\\cellx", cellx)
    if (isTRUE(rec$blank)) {
      content <- .build_cell_content("", rec$align, FALSE, FALSE, FALSE,
                                     0L, pad_l, pad_r, fs_cmd = fs_cmd)
      rh <- full_h
    } else {
      color_idx <- if (!is.null(rec$color) && !is.null(color_index_map))
                     color_index_map[[rec$color]] else NULL
      txt <- .format_cell_text(rec$text, markup)
      content <- .build_cell_content(txt, rec$align, isTRUE(rec$bold),
                                     isTRUE(rec$italic), isTRUE(rec$underline),
                                     0L, pad_l, pad_r, color_idx,
                                     fs_cmd = fs_cmd)
      rh <- full_h
    }
    .build_row(cell_def, content, rh, table_align)
  }, character(1L))
}

# Render a title/footnote block as plain paragraphs (one per line) spanning the
# writable width, honouring per-row align / bold / italic / underline / colour.
# A row `border` does NOT apply in text mode (a paragraph carries no cell rule),
# so the footnote separator is only drawn in the table form.
.render_text_block_text <- function(block, is_footer, color_index_map = NULL,
                                    markup = "script", pad_l = 0L, pad_r = 0L,
                                    style = NULL, font_half_points = 18L) {
  rows <- .normalize_text_block(block, is_footer, style$align)
  if (length(rows) == 0L) return(character())
  # Honour the document-wide cell padding as left/right paragraph indent, so the
  # "every element inherits the document padding" contract still holds.
  if (!is.null(style$markup)) markup <- style$markup
  fs_cmd <- .fs_cmd_for(style$font_size_half_points %||% font_half_points,
                        font_half_points)
  indent <- paste0("\\li", as.integer(pad_l), "\\ri", as.integer(pad_r), fs_cmd)
  vapply(rows, function(rec) {
    align_cmd <- switch(rec$align, left = "\\ql", right = "\\qr",
                        center = "\\qc", "\\ql")
    if (isTRUE(rec$blank)) {
      return(paste0("\\pard", align_cmd, indent, "\\par"))
    }
    txt <- .format_cell_text(rec$text, markup)
    if (isTRUE(rec$underline)) txt <- paste0("\\ul ", txt, "\\ulnone ")
    if (isTRUE(rec$italic))    txt <- paste0("\\i ",  txt, "\\i0 ")
    if (isTRUE(rec$bold))      txt <- paste0("\\b ",  txt, "\\b0 ")
    if (!is.null(rec$color) && !is.null(color_index_map) &&
        !is.null(color_index_map[[rec$color]])) {
      txt <- paste0("\\cf", as.integer(color_index_map[[rec$color]]), " ",
                    txt, "\\cf1 ")
    }
    paste0("\\pard", align_cmd, indent, " ", txt, "\\par")
  }, character(1L))
}

# Validate a title/footnote format selector ("text" | "table"); NULL -> default.
.resolve_text_block_format <- function(x, default) {
  if (is.null(x)) return(default)
  x <- as.character(x)[1L]
  if (!x %in% c("text", "table")) {
    stop("`title_format` / `footnote_format` must be \"text\" or \"table\"; got '",
         x, "'.", call. = FALSE)
  }
  x
}

# Build the RTF color table string from a character vector of hex colors.
# Returns the RTF {\colortbl ...} string.
#
# Reserved color-table slots:
#   index 1 = black (default text color)
#   index 2 = white (used by assemble_rtf() to render the PDF-outline
#             label as white-on-white invisible text -- see
#             .insert_bookmark()).  Always present so assemble_rtf()
#             can safely emit `\cf2` regardless of the user's color use.
#
# User colors therefore start at index 3.
.build_color_table_rtf <- function(hex_colors) {
  base <- "{\\colortbl;\\red0\\green0\\blue0;\\red255\\green255\\blue255;"
  if (length(hex_colors) == 0L) return(paste0(base, "}"))
  entries <- vapply(hex_colors, function(h) {
    h <- sub("^#", "", h)
    r <- strtoi(substr(h, 1L, 2L), 16L)
    g <- strtoi(substr(h, 3L, 4L), 16L)
    b <- strtoi(substr(h, 5L, 6L), 16L)
    sprintf("\\red%d\\green%d\\blue%d;", r, g, b)
  }, character(1L))
  paste0(base, paste(entries, collapse = ""), "}")
}

# Build a named list mapping "#RRGGBB" -> integer color-table index (1-based).
# Index 1 = black, index 2 = white (reserved for invisible markers),
# so user colors start at index 3.
.build_color_index_map <- function(hex_colors) {
  if (length(hex_colors) == 0L) return(list())
  idx <- seq_along(hex_colors) + 2L   # +2 because index 1 = black, 2 = white
  stats::setNames(as.list(idx), hex_colors)
}



# ============================================================================
# Pipe API Adapter: Convert rtf_document (public S3) -> rtfreport (internal S3)
# ============================================================================

# -- auto_section helpers -------------------------------------------------------

# Build a per-section header by appending a label row to the base header
# from the "_default" section.
# default_sec : the "_default" entry from pipe_doc$sections (or NULL).
# label       : character label to append as a new row.
# label_align : "left" | "center" | "right"
.build_auto_section_header <- function(default_sec, label, label_align = "left") {
  base_hdr <- if (!is.null(default_sec)) default_sec$header else NULL
  norm     <- .normalize_hf(base_hdr)

  label_row <- switch(label_align,
    right  = c(r = label),
    center = c(c = label),
    c(l = label)   # default: left
  )

  if (is.null(norm)) {
    # No base header: create a single-row header with just the label.
    return(list(rows = list(label_row)))
  }

  # Append label row to the existing rows list.
  norm$rows <- c(norm$rows, list(label_row))
  norm
}

# Unwrap an rtf_auto_section_item to its underlying content object.
# By contract, item$content is always a single rtftable, rtfplot, or
# data.frame (validated and promoted in rtf_tables()).
.unwrap_auto_section_item <- function(item) {
  item$content
}

# Internal helper: pass through a single content item.
# By contract, rtf_tables() and rtf_figures() guarantee a single content
# object (rtftable, rtfplot, or data.frame) per element.
.normalise_content_item <- function(content_item) {
  content_item
}

# Internal helper: convert S3 rtf_document (pipe API) to internal S3 rtfreport.
.pipe_doc_to_rtfreport <- function(pipe_doc) {
  if (!inherits(pipe_doc, "rtf_document")) return(NULL)

  report <- .new_rtfreport()

  # Copy document-level settings.
  if (!is.null(pipe_doc$document$font_table)) {
    report <- .rtfreport_set_font_table(report, pipe_doc$document$font_table)
  }
  if (!is.null(pipe_doc$document$color_table)) {
    report <- .rtfreport_set_color_table(report, pipe_doc$document$color_table)
  }
  if (!is.null(pipe_doc$document$page)) {
    ps <- pipe_doc$document$page
    # Resolve paper size / orientation / dimensions consistently (see
    # .resolve_page_geometry(): explicit dimensions win and orientation is
    # inferred from them; otherwise a paper_size preset is oriented to match).
    op <- .resolve_page_geometry(ps)
    pg <- list(
      orientation         = op$orientation,
      width_twips         = op$width_twips,
      height_twips        = op$height_twips,
      margin_left_twips   = .in_to_twips(ps$margin_left_in  %||% .opt("rtfreporter.page.margin_left_in")),
      margin_right_twips  = .in_to_twips(ps$margin_right_in %||% .opt("rtfreporter.page.margin_right_in")),
      margin_top_twips    = .in_to_twips(ps$margin_top_in   %||% .opt("rtfreporter.page.margin_top_in")),
      margin_bottom_twips = .in_to_twips(ps$margin_bottom_in %||% .opt("rtfreporter.page.margin_bottom_in"))
    )
    # Explicit header/footer band distance (page key wins over option); left
    # unset here means "derive as half the margin" at the emission step below.
    hd <- ps$header_dist_in %||% getOption("rtfreporter.page.header_dist_in")
    if (!is.null(hd)) pg$header_dist_twips <- .in_to_twips(hd)
    fd <- ps$footer_dist_in %||% getOption("rtfreporter.page.footer_dist_in")
    if (!is.null(fd)) pg$footer_dist_twips <- .in_to_twips(fd)
    report <- .rtfreport_set_default_page(report, pg)
  }
  # Per-element style for the title / footnote blocks (#292) rides on the
  # document alongside the default format.
  report$document$title_style    <- pipe_doc$title_style
  report$document$footnote_style <- pipe_doc$footnote_style
  if (!is.null(pipe_doc$document$default_format)) {
    report <- .rtfreport_set_default_format(report, pipe_doc$document$default_format)
  }

  # -- Detect auto-section items ---------------------------------------------
  has_auto_section <- any(vapply(pipe_doc$contents,
    function(x) inherits(x, "rtf_auto_section_item"), logical(1L)))

  if (has_auto_section) {
    # -- Auto-section path --------------------------------------------------
    # "_default" section provides the base header/footer template.
    default_sec <- pipe_doc$sections[["_default"]]

    # Add any explicit page-number sections (not "_default") first.
    all_keys     <- names(pipe_doc$sections)
    explicit_keys <- sort(as.integer(all_keys[all_keys != "_default"]))
    for (key in explicit_keys) {
      si <- pipe_doc$sections[[as.character(key)]]
      report <- .rtfreport_add_section(report, header = si$header,
                                       footer = si$footer, from_page = key)
    }

    # Process contents one by one, creating one RTF section per named item.
    page_counter <- 0L
    for (ci in seq_along(pipe_doc$contents)) {
      content_item <- pipe_doc$contents[[ci]]
      page_counter <- page_counter + 1L

      if (inherits(content_item, "rtf_auto_section_item")) {
        label       <- content_item$label
        label_align <- content_item$label_align %||% "left"
        sec_header  <- .build_auto_section_header(default_sec, label, label_align)
        sec_footer  <- if (!is.null(default_sec)) default_sec$footer else NULL
        report <- .rtfreport_add_section(report, header = sec_header,
                                         footer = sec_footer,
                                         from_page = page_counter)
        ct <- .unwrap_auto_section_item(content_item)
      } else {
        ct <- .normalise_content_item(content_item)
      }
      title_i    <- if (ci <= length(pipe_doc$titles))    pipe_doc$titles[[ci]]    else NULL
      footnote_i <- if (ci <= length(pipe_doc$footnotes)) pipe_doc$footnotes[[ci]] else NULL
      report <- .rtfreport_add_page(report, content = ct, title = title_i,
                                    footnote = footnote_i)
    }

    # Guard: if no section was added at all (e.g., all items were non-auto),
    # fall back to the "_default" section or an empty one.
    if (length(report$sections) == 0L) {
      if (!is.null(default_sec)) {
        report <- .rtfreport_add_section(report, header = default_sec$header,
                                         footer = default_sec$footer)
      } else {
        report <- .rtfreport_add_section(report)
      }
    }

  } else {
    # -- Original path (no auto-section items) -----------------------------
    all_keys     <- names(pipe_doc$sections)
    non_def_keys <- all_keys[all_keys != "_default"]
    section_keys <- sort(as.integer(non_def_keys))

    if (length(section_keys) == 0L) {
      # No explicit page sections - use "_default" or an empty default.
      default_sec <- pipe_doc$sections[["_default"]]
      if (!is.null(default_sec)) {
        report <- .rtfreport_add_section(report, header = default_sec$header,
                                         footer = default_sec$footer)
      } else {
        report <- .rtfreport_add_section(report)  # default covering all pages
      }
    } else {
      for (key in section_keys) {
        si <- pipe_doc$sections[[as.character(key)]]
        report <- .rtfreport_add_section(report, header = si$header,
                                         footer = si$footer, from_page = key)
      }
    }

    # Add pages from pipe_doc$contents (each element = one page)
    for (ci in seq_along(pipe_doc$contents)) {
      ct         <- .normalise_content_item(pipe_doc$contents[[ci]])
      title_i    <- if (ci <= length(pipe_doc$titles))    pipe_doc$titles[[ci]]    else NULL
      footnote_i <- if (ci <= length(pipe_doc$footnotes)) pipe_doc$footnotes[[ci]] else NULL
      report <- .rtfreport_add_page(report, content = ct, title = title_i,
                                    footnote = footnote_i)
    }
  }

  report
}

#' Generate an RTF file from a report object
#'
#' Renders an `rtf_document` (from the pipe API) or internal `rtfreport`
#' object to an RTF file.
#'
#' @param report An `rtf_document` object (from [rtf_document()]) or an internal
#'   `rtfreport` object.
#' @param file_path Output RTF file path.
#' @param overwrite Logical; whether to overwrite an existing file.
#'   Default `FALSE`.
#'
#' @return Invisibly returns `file_path`.
#'
#' @seealso [rtf_document()] / [rtf_tables()] to compose the report, and
#'   [assemble_rtf()] to concatenate several rendered files into one deliverable.
#'
#' @examples
#' df  <- data.frame(Parameter = "Age, Mean (SD)", Value = "75.1 (8.2)")
#' doc <- rtf_document() |>
#'   rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
#'   rtf_tables(as_rtftables(df), titles = list("Table 14.1.1"))
#'
#' out <- tempfile(fileext = ".rtf")     # write to a temporary file
#' generate_rtfreport(doc, out, overwrite = TRUE)
#' file.exists(out)
#' @export
generate_rtfreport <- function(report, file_path, overwrite = FALSE) {
  if (inherits(report, "rtf_document")) {
    report <- .pipe_doc_to_rtfreport(report)
    if (is.null(report)) {
      stop("`report` must be an rtf_document or rtfreport object.", call. = FALSE)
    }
  } else if (!inherits(report, "rtfreport")) {
    stop("`report` must be an rtf_document (from rtf_document()) or rtfreport object.",
         call. = FALSE)
  }
  if (file.exists(file_path) && !isTRUE(overwrite)) {
    stop("`file_path` already exists. Set overwrite = TRUE.", call. = FALSE)
  }

  report <- .rtfreport_validate(report)

  doc           <- report$document
  page_defaults <- doc$default_page
  primary_font  <- doc$font_table[[1]]$name %||% "Courier"
  writable_w    <- page_defaults$width_twips -
                   page_defaults$margin_left_twips -
                   page_defaults$margin_right_twips

  cmds     <- .load_rtf_commands()
  doc_cmd  <- cmds$document

  # Header/footer band distance from the page edge. When set explicitly (page
  # key `header_dist_in`/`footer_dist_in` or the matching option) that value is
  # used; otherwise it is derived as the full top/bottom margin so the
  # header/footer band sits at the margin boundary, rather than the RTF/Word
  # built-in default (720 twips) which can land outside the body.
  header_dist_twips <- page_defaults$header_dist_twips %||%
    max(0L, as.integer(page_defaults$margin_top_twips    %||% 1440L))
  footer_dist_twips <- page_defaults$footer_dist_twips %||%
    max(0L, as.integer(page_defaults$margin_bottom_twips %||% 1440L))

  total_pages     <- length(report$pages)
  doc_colors      <- .collect_report_colors(report)
  color_table_str <- .build_color_table_rtf(doc_colors)
  color_index_map <- .build_color_index_map(doc_colors)

  lines <- c(
    doc_cmd$rtf_header_open,
    .cmd_fmt(doc_cmd$font_table_template, list(font_name = .rtf_escape(primary_font))),
    color_table_str,
    .cmd_fmt(doc_cmd$page_settings_template, list(
      width_twips           = page_defaults$width_twips,
      height_twips          = page_defaults$height_twips,
      # Document-level \landscape only when the page is landscape; otherwise
      # omit it so portrait pages are not forced wide (the section-level
      # \lndscpsxn is already orientation-aware).
      orientation_cmd       = if (isTRUE(page_defaults$orientation == "landscape"))
                                "\\landscape" else "",
      margin_left_twips     = page_defaults$margin_left_twips,
      margin_right_twips    = page_defaults$margin_right_twips,
      margin_top_twips      = page_defaults$margin_top_twips,
      margin_bottom_twips   = page_defaults$margin_bottom_twips,
      header_dist_twips     = header_dist_twips,
      footer_dist_twips     = footer_dist_twips,
      font_size_half_points = doc$default_format$font_size_half_points
    ))
  )

  # Font-size command used inside {\header} / {\footer} groups.
  # RTF header/footer groups are independent scopes and do NOT inherit the
  # document-level \fs, so the viewer falls back to its built-in default
  # (typically \fs24 = 12 pt).  Prepending the same \fs here keeps the
  # font size consistent with the body text.
  font_half_points <- as.integer(doc$default_format$font_size_half_points %||%
                                    .opt("rtfreporter.font_size_half_points"))
  fs_cmd           <- paste0("\\fs", font_half_points)

  # Document-wide style defaults (row height, cell padding): the layer between
  # an element's own setting and the font-aware / resource baseline.  Resolved
  # highest-wins as: per-module value (applied inside each renderer) >
  # document `default_format` > `rtfreporter.*` option > resource baseline.
  # `doc_row_height = NULL` means "keep the font-aware default".
  .res_defaults  <- .load_rtfreporter_defaults()
  doc_row_height <- doc$default_format$row_height_twips %||%
                      .opt("rtfreporter.row_height_twips")
  doc_pad_l <- doc$default_format$cell_padding_left_twips %||%
                 .opt("rtfreporter.cell_padding_left_twips") %||%
                 .res_defaults$default_cell_padding_left_twips %||% 0L
  doc_pad_r <- doc$default_format$cell_padding_right_twips %||%
                 .opt("rtfreporter.cell_padding_right_twips") %||%
                 .res_defaults$default_cell_padding_right_twips %||% 0L
  doc_row_height <- if (is.null(doc_row_height)) NULL else as.integer(doc_row_height)
  doc_pad_l      <- as.integer(doc_pad_l)
  doc_pad_r      <- as.integer(doc_pad_r)

  # Document-wide cell-text markup default (per-table `markup` overrides it).
  doc_markup <- .resolve_markup(doc$default_format$markup %||%
                                  .opt("rtfreporter.markup") %||% "script")

  # How the page title / footnote blocks render: "text" (plain paragraphs) or
  # "table" (content-width single-column table).  Title defaults to text;
  # footnote keeps the table form.
  doc_title_format <- .resolve_text_block_format(
    doc$default_format$title_format %||% .opt("rtfreporter.title_format"), "text")
  doc_footnote_format <- .resolve_text_block_format(
    doc$default_format$footnote_format %||% .opt("rtfreporter.footnote_format"),
    "table")

  # Per-element widths (#291).  NULL keeps each block's established default,
  # which .resolve_block_width() spells as "content".
  doc_title_width <- .check_block_width(
    doc$default_format$title_width %||% .opt("rtfreporter.title_width"),
    "title_width")
  doc_footnote_width <- .check_block_width(
    doc$default_format$footnote_width %||% .opt("rtfreporter.footnote_width"),
    "footnote_width")

  # Resolve sections (sorted, with from_page / to_page assigned).
  resolved_sections <- .resolve_sections(report)

  prev_header_hf <- NULL
  prev_footer_hf <- NULL

  for (rs_idx in seq_along(resolved_sections)) {
    rs      <- resolved_sections[[rs_idx]]
    pg_from <- rs$from_page
    pg_to   <- rs$to_page

    # Resolve header/footer with inheritance from previous section.
    cur_header_hf <- .normalize_hf(rs$header)
    if (is.null(cur_header_hf)) cur_header_hf <- prev_header_hf
    prev_header_hf <- cur_header_hf

    cur_footer_hf <- .normalize_hf(rs$footer)
    if (is.null(cur_footer_hf)) cur_footer_hf <- prev_footer_hf
    prev_footer_hf <- cur_footer_hf

    # Helper: emit `\sectd` + page settings + {\header} + {\footer} with
    # the page-number tokens resolved for `pg_for_hf`.  Used either:
    #   - once per rtf_section (header text is the same across sub-pages), or
    #   - once per sub-page when a STATIC `{PAGE}` token is used so that
    #     each rendered page can carry its own baked-in page number.
    emit_section_preamble <- function(pg_for_hf) {
      lines <<- c(lines, doc_cmd$section_defaults)

      # Re-emit section-level page properties after \sectd.
      #
      # \sectd resets all section formatting to RTF built-in defaults, so we must
      # explicitly re-specify:
      #   \sbkpage      - force each section to start on a new page (required for
      #                   per-section headers to work; without it sections are
      #                   "continuous" and share the first section's header).
      #   \lndscpsxn    - landscape orientation (document-level \landscape only
      #                   applies to the first section in many RTF viewers).
      #   \pgwsxn / \pghsxn         - section page dimensions.
      #   \marglsxn / \margrsxn ... - section margins.
      pg     <- page_defaults
      is_lnd <- isTRUE(pg$orientation == "landscape")
      lnd_cmd <- if (is_lnd) "\\lndscpsxn" else ""
      lines <<- c(lines, paste0(
        "\\sbkpage", lnd_cmd,
        "\\pgwsxn",   pg$width_twips,
        "\\pghsxn",   pg$height_twips,
        "\\marglsxn", pg$margin_left_twips,
        "\\margrsxn", pg$margin_right_twips,
        "\\margtsxn", pg$margin_top_twips,
        "\\margbsxn", pg$margin_bottom_twips,
        # Re-assert the header/footer band distance per section (\sectd reset
        # it); these have no ...sxn variant and apply to the current section.
        "\\headery",  header_dist_twips,
        "\\footery",  footer_dist_twips
      ))

      header_rtf <- .render_header_footer(cur_header_hf, writable_w, is_footer = FALSE,
                                          current_page = pg_for_hf, total_pages = total_pages,
                                          color_index_map = color_index_map,
                                          font_half_points = font_half_points,
                                          doc_row_height = doc_row_height,
                                          doc_pad_l = doc_pad_l, doc_pad_r = doc_pad_r)
      footer_rtf <- .render_header_footer(cur_footer_hf, writable_w, is_footer = TRUE,
                                          current_page = pg_for_hf, total_pages = total_pages,
                                          color_index_map = color_index_map,
                                          font_half_points = font_half_points,
                                          doc_row_height = doc_row_height,
                                          doc_pad_l = doc_pad_l, doc_pad_r = doc_pad_r)

      if (length(header_rtf) > 0L) {
        lines <<- c(lines, .cmd_fmt(doc_cmd$header_wrapper,
                                    list(content = paste0(fs_cmd,
                                          paste(header_rtf, collapse = "")))))
      }
      if (length(footer_rtf) > 0L) {
        lines <<- c(lines, .cmd_fmt(doc_cmd$footer_wrapper,
                                    list(content = paste0(fs_cmd,
                                          paste(footer_rtf, collapse = "")))))
      }
    }

    # Decide whether each sub-page needs its own RTF section.  The
    # static `{PAGE}` token is baked into the header text at render
    # time, so a single `\header` per RTF section cannot carry distinct
    # numbers across multiple sub-pages.  When `{PAGE}` is used (in
    # either header or footer), we promote every sub-page boundary
    # from a `\page` break to a `\sect` break and re-emit the
    # section preamble with the correct baked-in number.
    needs_per_page_section <- .uses_static_page_token(cur_header_hf) ||
                              .uses_static_page_token(cur_footer_hf)

    sec_pages <- seq(pg_from, pg_to)
    if (!needs_per_page_section) {
      emit_section_preamble(pg_from)
    }
    for (sp_idx in seq_along(sec_pages)) {
      p_idx <- sec_pages[sp_idx]
      page  <- report$pages[[p_idx]]

      if (needs_per_page_section) {
        emit_section_preamble(p_idx)
      }

      # -- Content (single rtftable or rtfplot) -----------------------------
      ct <- page$content

      # Title / footnote render as content-width tables (one row per line),
      # so they align with the content above/below (see .render_text_block_table).
      content_w     <- .content_width_twips(ct, writable_w)
      content_align <- .content_align(ct)
      # Per-element width (#291): "content" (the default -- follow the body),
      # "page", a fraction of the writable width, or twips.
      title_st    <- doc$title_style
      footnote_st <- doc$footnote_style
      title_w    <- .resolve_block_width(doc_title_width,    content_w, writable_w)
      footnote_w <- .resolve_block_width(doc_footnote_width, content_w, writable_w)
      # Title / footnote inherit the document-wide cell padding and row height.
      tf_pad_l      <- doc_pad_l
      tf_pad_r      <- doc_pad_r
      tf_valign     <- "\\clvertalt"

      # -- Title (default centre + bold; NULL -> one blank gap row) ----------
      # Default renders as plain paragraphs (full writable width); the legacy
      # content-width table form is opt-in via title_format = "table".
      lines <- c(lines, if (identical(doc_title_format, "text")) {
        .render_text_block_text(page$title, is_footer = FALSE, style = title_st,
                                font_half_points = font_half_points,
                                color_index_map, markup = doc_markup,
                                pad_l = tf_pad_l, pad_r = tf_pad_r)
      } else {
        .render_text_block_table(
          page$title, title_w, is_footer = FALSE, font_half_points,
          tf_pad_l, tf_pad_r, tf_valign, content_align, color_index_map,
          doc_row_height = doc_row_height, markup = doc_markup,
          style = title_st)
      })
      if (!is.null(ct)) {
        if (inherits(ct, "rtftable")) {
          lines <- c(lines, .render_rtftable(ct, writable_w, font_half_points,
                                              color_index_map,
                                              doc_row_height = doc_row_height,
                                              doc_pad_l = doc_pad_l,
                                              doc_pad_r = doc_pad_r,
                                              doc_markup = doc_markup))
        } else if (inherits(ct, "rtfplot")) {
          lines <- c(lines, .render_rtfplot(ct, writable_w))
        }
      }

      # -- Footnote (default left; first row carries the separator top rule) -
      # Default keeps the content-width table form (so the separator rule is
      # drawn); footnote_format = "text" switches to plain paragraphs.
      lines <- c(lines, if (identical(doc_footnote_format, "text")) {
        .render_text_block_text(page$footnote, is_footer = TRUE, style = footnote_st,
                                font_half_points = font_half_points,
                                color_index_map, markup = doc_markup,
                                pad_l = tf_pad_l, pad_r = tf_pad_r)
      } else {
        .render_text_block_table(
          page$footnote, footnote_w, is_footer = TRUE, font_half_points,
          tf_pad_l, tf_pad_r, tf_valign, content_align, color_index_map,
          doc_row_height = doc_row_height, markup = doc_markup,
          style = footnote_st)
      })

      # End-of-page break.  A table must be terminated before any paragraph-level
      # break or the document close, or strict readers absorb the break into the
      # table flow (#130).
      #   * Plain page break: `page_break` is the self-terminating wrapped form
      #     {\pard\fs2\par}\page{\pard\fs2\par} -- a bare \page is dropped by Word
      #     when not flanked by real paragraphs (#138), and its leading empty
      #     paragraph already closes the table, so no separate \pard is needed.
      #   * Section break (\sect + \sectd\sbkpage starts a new page on its own)
      #     and the document close still terminate the table with \pard.
      # When per-page sections are in effect, ALL sub-page boundaries become
      # section breaks so the next sub-page can re-emit its header with a fresh
      # `{PAGE}` value.
      is_last_in_section <- (sp_idx == length(sec_pages))
      is_last_section    <- (rs_idx == length(resolved_sections))
      if (!is_last_in_section) {
        if (needs_per_page_section) {
          lines <- c(lines, doc_cmd$table_end, doc_cmd$section_break)
        } else {
          lines <- c(lines, doc_cmd$page_break)
        }
      } else if (!is_last_section) {
        lines <- c(lines, doc_cmd$table_end, doc_cmd$section_break)
      } else {
        # Last page of the document: terminate the final table before close.
        lines <- c(lines, doc_cmd$table_end)
      }
    }
  }

  lines <- c(lines, doc_cmd$document_close)
  writeLines(lines, con = file_path, useBytes = TRUE)
  invisible(file_path)
}

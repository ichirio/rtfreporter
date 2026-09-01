# ============================================================================
#  decimal_split -- render-time column splitting for decimal-point alignment
# ============================================================================
#
#  A statistics column in a clinical TFL mixes integers and decimals of
#  differing widths ("12", "3.45", "100.0", "<0.001").  Right-alignment lines
#  up the last character, not the decimal point, so the units digit wanders.
#
#  This module splits such a column into TWO adjacent RTF cells at render
#  time: the part BEFORE the first decimal separator (right-aligned) and the
#  separator plus everything after it (left-aligned).  The decimal points then
#  line up exactly, independent of font metrics.
#
#  The split never touches the user's data:
#
#    * `rtftable$data` / `$data_list` are read, never rewritten;
#    * the cumulative \cellx positions still end at the same right edge, so
#      the table width, every other column, the title / footnote band
#      (.content_width_twips()) and pagination are unaffected;
#    * RTF allows a different cell count per row, so the whole column-header
#      block keeps rendering over the ORIGINAL column geometry -- only data
#      rows use the expanded one.  No header / spanning renderer changes.
#
#  Cells that are not split-eligible (free text such as "n (%)", and by
#  default compound values such as "12.3 (4.56)") are rendered as ONE cell
#  spanning the pair, exactly as they look today.

# Characters stripped from both ends before a cell is classified.  NBSP is
# included so a column already padded by fmt_right_align() still splits.
.DECIMAL_SPLIT_WS <- "[ \t\r\n\u00a0]"

# A cell may be split when it starts with an optional relational / sign prefix
# followed by a digit or the separator, and contains at least one digit.
.DECIMAL_SPLIT_GATE <- "^[<>=~+-]{0,2}[0-9.]"

# Strip leading / trailing whitespace, NBSP included.
.decimal_split_trim <- function(x) {
  gsub(paste0("^", .DECIMAL_SPLIT_WS, "+|", .DECIMAL_SPLIT_WS, "+$"), "", x)
}

# Classify and split one column's worth of cell text.
#
# Returns a list of three vectors, all the length of `x`:
#   $left   text for the left (integer-side) cell, right-aligned
#   $right  text for the right (decimal-side) cell, left-aligned
#   $split  TRUE  -> render as two cells
#           FALSE -> not eligible; `$left` holds the ORIGINAL text and the
#                    pair is rendered as one merged cell
#
# `include_compound = FALSE` (the default) excludes values carrying a
# whitespace- or parenthesis-separated companion -- "12.3 (4.56)" -- whose
# trailing group would otherwise dominate the right half and drag the split
# point far to the left.
.decimal_split_cells <- function(x, decimal_mark = ".",
                                 include_compound = FALSE) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  n <- length(x)
  s <- .decimal_split_trim(x)

  left  <- character(n)
  right <- character(n)

  empty <- !nzchar(s)
  ok    <- !empty &
           grepl(.DECIMAL_SPLIT_GATE, s) &
           grepl("[0-9]", s)
  if (!isTRUE(include_compound)) {
    ok <- ok & !grepl("[[:space:]()]", s)
  }

  pos <- rep(-1L, n)
  if (any(ok)) {
    pos[ok] <- as.integer(regexpr(decimal_mark, s[ok], fixed = TRUE))
  }
  has_pt <- ok & pos > 0L
  int_only <- ok & pos <= 0L

  left[has_pt]  <- substr(s[has_pt], 1L, pos[has_pt] - 1L)
  right[has_pt] <- substr(s[has_pt], pos[has_pt], nchar(s[has_pt]))
  left[int_only] <- s[int_only]

  split <- ok | empty
  # Non-eligible cells keep their original text verbatim (padding included).
  left[!split] <- x[!split]

  list(left = left, right = right, split = split)
}

# Display width (glyph count) of cell text AFTER markup substitution: ">=" is
# rendered as a single glyph, and the ^{ } / _{ } script markers do not print.
# Used only to derive the left/right width ratio.
.decimal_split_width <- function(x, markup = character(0)) {
  if (length(x) == 0L) return(integer(0))
  if ("relational" %in% markup) {
    x <- gsub(">=", "\u2265", x, fixed = TRUE)
    x <- gsub("<=", "\u2264", x, fixed = TRUE)
  }
  if ("script" %in% markup) {
    x <- gsub("\\^\\{|_\\{|\\}", "", x)
  }
  w <- suppressWarnings(nchar(x, type = "width", allowNA = TRUE))
  bad <- is.na(w)
  if (any(bad)) w[bad] <- nchar(x[bad], type = "chars")
  as.integer(w)
}

# Split ratio from the two measured half-widths (#304).
#
# The raw glyph counts alone leave the integer half cramped whenever the
# integers are short: a column of `0.0000` values measures wl = 1 against
# wr = 5 and takes one sixth of the column, so the digit sits hard against the
# cell edge.  Each half therefore gets a padding allowance and a floor, both in
# character-width units:
#
#     w_eff = max(w + pad, floor)
#
# One character of breathing room per half, and a floor corresponding to a
# 3-digit integer part and a 5-character decimal part plus that character: 4
# and 6.  Anything at or below that size holds a 4 : 6 baseline; anything
# larger scales from its own measurement.
#
# The allowance is dropped once the measured halves together exceed
# `max_chars`: a long column needs every twip it has, and reserving room it
# does not use would squeeze the digits it does.  Above the cap the raw
# measured proportions are used, however lopsided.
#
# `pad_chars = c(0, 0), min_chars = c(0, 0)` restores the pre-#304 behaviour;
# `max_chars = Inf` never drops the allowance.
.decimal_split_ratio <- function(wl, wr, pad_chars = NULL, min_chars = NULL,
                                 max_chars = NULL) {
  pad <- if (is.null(pad_chars)) c(1, 1) else as.numeric(pad_chars)
  flr <- if (is.null(min_chars)) c(4, 6) else as.numeric(min_chars)
  cap <- if (is.null(max_chars)) 10       else as.numeric(max_chars)
  if (wl + wr > cap) {
    le <- as.numeric(wl)
    re <- as.numeric(wr)
  } else {
    le <- max(wl + pad[1L], flr[1L])
    re <- max(wr + pad[2L], flr[2L])
  }
  if (le + re <= 0) return(0.5)
  le / (le + re)
}

# Validate a length-2 non-negative numeric vector of character-width units.
.check_char_widths <- function(v, arg) {
  if (is.null(v)) return(NULL)
  v <- suppressWarnings(as.numeric(v))
  if (length(v) != 2L || anyNA(v) || any(v < 0)) {
    stop(sprintf(
      "`%s` must be two non-negative numbers (integer half, decimal half).",
      arg), call. = FALSE)
  }
  unname(v)
}

# Validate the total-width cap: a single positive number, possibly Inf.
.check_char_cap <- function(v, arg) {
  if (is.null(v)) return(NULL)
  v <- suppressWarnings(as.numeric(v))
  if (length(v) != 1L || is.na(v) || v <= 0) {
    stop(sprintf("`%s` must be a single positive number (or Inf).", arg),
         call. = FALSE)
  }
  v
}

# Derive one half's border from the original column's border by suppressing
# the INTERIOR vertical edge.  `side = "left"` is the integer half (its right
# edge is interior); `side = "right"` is the decimal half (its left edge is).
# The outer edges keep whatever the original column asked for.
.decimal_split_border <- function(b, side) {
  none <- rtf_border_side("none")
  over <- if (identical(side, "left")) rtf_border(right = none)
          else                         rtf_border(left  = none)
  if (is.null(b)) return(over)
  if (!inherits(b, "rtf_border")) return(b)   # legacy plain list: leave as-is
  .merge_rtf_border(b, over)
}

# Normalize the stored `decimal_split` metadata.  Returns NULL when the
# feature is off.
.resolve_decimal_split <- function(spec) {
  if (is.null(spec)) return(NULL)
  cols <- as.integer(spec$cols)
  cols <- sort(unique(cols[!is.na(cols)]))
  if (length(cols) == 0L) return(NULL)
  list(
    cols             = cols,
    decimal_mark     = spec$decimal_mark %||% ".",
    ratio            = spec$ratio,
    pad_chars        = spec$pad_chars,
    min_chars        = spec$min_chars,
    max_chars        = spec$max_chars,
    include_compound = isTRUE(spec$include_compound)
  )
}

# Build the render-time expansion plan for a table, or NULL when nothing is
# split (feature off, no such column, or no column actually carries a
# separator -- an integer-only column is left alone).
#
# `cellx` / `col_spec` are the ORIGINAL (unexpanded) geometry.  The returned
# plan carries the expanded geometry for data rows only:
#
#   $cellx      integer(n1)  expanded cumulative cell edges
#   $col_spec   list(n1)     expanded per-column spec (halves realigned)
#   $left_of    integer(n0)  original column -> its first expanded index
#   $right_of   integer(n0)  original column -> its second index, NA if intact
#   $merge_to   integer(n1)  cell-start map when EVERY pair is merged back
#   $pad_l/$pad_r integer(n1) per-expanded-column padding (interior zeroed)
#   $interior   list(n1)     structural border override per half, NULL if intact
#   $parts      list(n0)     .decimal_split_cells() output per split column,
#                            computed over every body row of the table
.decimal_split_plan <- function(tbl, cellx, col_spec, markup = character(0)) {
  spec <- .resolve_decimal_split(tbl$decimal_split)
  if (is.null(spec)) return(NULL)

  n0 <- length(cellx)
  cols <- spec$cols[spec$cols >= 1L & spec$cols <= n0]
  if (length(cols) == 0L) return(NULL)

  frames <- if (!is.null(tbl$data_list)) tbl$data_list else list(tbl$data)

  parts    <- vector("list", n0)
  do_split <- logical(n0)
  for (j in cols) {
    vals <- unlist(lapply(frames, function(d) as.character(d[[j]])),
                   use.names = FALSE)
    p <- .decimal_split_cells(vals, spec$decimal_mark, spec$include_compound)
    # Nothing to align: no cell in this column carries a separator.
    if (!any(nzchar(p$right))) next
    parts[[j]]   <- p
    do_split[j]  <- TRUE
  }
  if (!any(do_split)) return(NULL)

  n1        <- n0 + sum(do_split)
  new_cellx <- integer(n1)
  new_spec  <- vector("list", n1)
  interior  <- vector("list", n1)
  pad_flag  <- character(n1)          # "" | "left" | "right"
  left_of   <- integer(n0)
  right_of  <- rep(NA_integer_, n0)

  k <- 0L
  for (j in seq_len(n0)) {
    if (!do_split[j]) {
      k <- k + 1L
      left_of[j]    <- k
      new_cellx[k]  <- cellx[j]
      new_spec[[k]] <- col_spec[[j]]
      next
    }

    p  <- parts[[j]]
    wl <- .decimal_split_width(p$left[p$split],  markup)
    wr <- .decimal_split_width(p$right[p$split], markup)
    wl <- max(c(1L, wl))
    wr <- max(c(1L, wr))
    r  <- if (!is.null(spec$ratio)) as.numeric(spec$ratio)
          else .decimal_split_ratio(wl, wr, spec$pad_chars, spec$min_chars,
                                    spec$max_chars)

    x0 <- if (j == 1L) 0L else as.integer(cellx[j - 1L])
    x1 <- as.integer(cellx[j])
    xs <- x0 + as.integer(round((x1 - x0) * r))
    xs <- max(x0 + 1L, min(x1 - 1L, xs))

    base <- col_spec[[j]]

    lspec <- base
    lspec$align  <- "right"
    lspec$border <- .decimal_split_border(base$border, "left")

    rspec <- base
    rspec$align        <- "left"
    rspec$indent_twips <- 0L          # a left indent would push the decimals
    rspec$border       <- .decimal_split_border(base$border, "right")

    k <- k + 1L
    left_of[j]    <- k
    new_cellx[k]  <- xs
    new_spec[[k]] <- lspec
    interior[[k]] <- .decimal_split_border(NULL, "left")
    pad_flag[k]   <- "left"

    k <- k + 1L
    right_of[j]   <- k
    new_cellx[k]  <- x1
    new_spec[[k]] <- rspec
    interior[[k]] <- .decimal_split_border(NULL, "right")
    pad_flag[k]   <- "right"
  }

  merge_to <- rep(NA_integer_, n1)
  for (j in seq_len(n0)) {
    merge_to[left_of[j]] <-
      if (is.na(right_of[j])) left_of[j] else right_of[j]
  }

  list(
    n0        = n0,
    n1        = n1,
    cellx     = new_cellx,
    col_spec  = new_spec,
    orig_spec = col_spec,
    left_of   = left_of,
    right_of  = right_of,
    do_split  = do_split,
    merge_to  = merge_to,
    pad_flag  = pad_flag,
    interior  = interior,
    parts     = parts
  )
}

# Expand one data row onto the plan's geometry.
#
# `row_index` is the row's position in the concatenated body (the same order
# .decimal_split_plan() scanned), so the pre-computed split decision is reused
# rather than recomputed per row.
#
# Returns list($vals, $merge_to, $merge_spec) where `merge_to[j]` is NA for a
# continuation cell, and `merge_spec[[j]]` is non-NULL only where a pair
# collapsed back into a single cell (then it is the ORIGINAL column's spec).
.decimal_split_row <- function(plan, vals, row_index) {
  n1        <- plan$n1
  out       <- character(n1)
  merge_to  <- rep(NA_integer_, n1)
  merge_spec <- vector("list", n1)

  for (j in seq_len(plan$n0)) {
    a <- plan$left_of[j]
    if (!plan$do_split[j]) {
      raw    <- if (j <= length(vals)) vals[[j]] else NA
      out[a] <- if (is.na(raw)) "" else as.character(raw)
      merge_to[a] <- a
      next
    }
    b <- plan$right_of[j]
    p <- plan$parts[[j]]
    if (isTRUE(p$split[row_index])) {
      out[a] <- p$left[row_index]
      out[b] <- p$right[row_index]
      merge_to[a] <- a
      merge_to[b] <- b
    } else {
      # Not split-eligible on this row: one cell across the pair, styled by
      # the original column.
      out[a] <- p$left[row_index]
      merge_to[a]   <- b
      merge_spec[[a]] <- plan$orig_spec[[j]]
    }
  }

  list(vals = out, merge_to = merge_to, merge_spec = merge_spec)
}

# Expand a per-row cell_styles entry (each element a length-n0 vector, or a
# length-n0 list for $border) onto the plan's geometry by duplicating each
# original column's value across its pair.
.decimal_split_cell_styles <- function(plan, rcs) {
  if (is.null(rcs)) return(NULL)
  idx <- integer(plan$n1)
  for (j in seq_len(plan$n0)) {
    idx[plan$left_of[j]] <- j
    if (!is.na(plan$right_of[j])) idx[plan$right_of[j]] <- j
  }
  lapply(rcs, function(v) {
    if (length(v) != plan$n0) return(v)
    v[idx]
  })
}

# Render one data row on the decimal-split geometry.
#
# `vals` / `cellx` / `col_spec` are the EXPANDED ones; `dsplit_row` is the
# output of .decimal_split_row() for this row and `dsplit` the table's plan.
# A pair whose cell was not split-eligible on this row collapses back into a
# single cell carrying the ORIGINAL column's spec, so such a row looks exactly
# as it does without the feature.
.render_data_row_split <- function(vals, cellx, border_spec, row_height_twips,
                                   pad_l, pad_r, valign_cmd, col_spec,
                                   table_align, row_cell_styles,
                                   color_index_map, markup,
                                   dsplit_row, dsplit) {
  merge_to   <- dsplit_row$merge_to
  merge_spec <- dsplit_row$merge_spec
  interior   <- dsplit$interior
  pad_flag   <- dsplit$pad_flag
  starts     <- which(!is.na(merge_to))

  # The pair's interior edge carries no padding, or the number would be split
  # by a gap; the outer edges keep the table's padding.
  pad_l_v <- ifelse(pad_flag == "right", 0L, as.integer(pad_l))
  pad_r_v <- ifelse(pad_flag == "left",  0L, as.integer(pad_r))

  cell_borders <- if (!is.null(row_cell_styles)) row_cell_styles$border

  n_cells <- length(starts)
  cell_defs <- vapply(seq_along(starts), function(ci) {
    j   <- starts[ci]
    to  <- merge_to[j]
    eff <- .cell_edge_border(border_spec, ci, n_cells)
    b   <- if (is.list(cell_borders) && j <= length(cell_borders))
             cell_borders[[j]] else NULL
    if (!is.null(b)) eff <- .effective_row_border(eff, b)
    # Suppress the interior vertical rule -- but only on a genuinely split
    # pair; a merged cell spans the whole original column and keeps both edges.
    if (to == j && !is.null(interior[[j]])) {
      eff <- .effective_row_border(eff, interior[[j]])
    }
    paste0(.build_border_commands(eff, color_index_map), valign_cmd,
           "\\cellx", cellx[to])
  }, character(1L))

  cell_contents <- vapply(starts, function(j) {
    to      <- merge_to[j]
    merged  <- to != j
    is_half <- !merged && !is.null(interior[[j]])
    spec    <- if (merged && !is.null(merge_spec[[j]])) merge_spec[[j]]
               else col_spec[[j]]
    .data_cell_content(spec, vals[[j]], j, row_cell_styles,
                       pad_l_v[j], pad_r_v[to], markup, color_index_map,
                       force_align = if (is_half) spec$align else NULL)
  }, character(1L))

  .build_row(cell_defs, cell_contents, row_height_twips, table_align)
}

# ============================================================================
#  paginate_cols() -- horizontal (column-wise) pagination
# ============================================================================
#
#  `paginate()` / `as_rtftables(split = )` cut ROWS.  A table wider than the
#  page needs the other axis: cut COLUMNS and continue on the next page,
#  repeating the row-heading column(s) so each page stays readable.
#
#  Page order
#  ----------
#  Row splitting happens first (as_rtftables), then this verb splits the
#  resulting pages by column.  `page_order` says which axis the PAGE NUMBER
#  advances along first:
#
#    "across" (the default)  ACROSS the columns -- the column block advances
#                            first, and the row pages run inside it;
#    "down"                  DOWN the rows -- the row page advances first, and
#                            its column blocks follow it.
#
#  Two row pages x three column blocks:
#
#      "across"   col1/row1  col1/row2  col2/row1  col2/row2  col3/row1 ...
#      "down"     row1/col1  row1/col2  row1/col3  row2/col1  row2/col2 ...
#
#  A GROUP is above both.  When the row pagination made a page per group value
#  (`as_rtftables(split = "by_value", group_col = )`, with `page_by` running
#  inside it), the column split happens WITHIN one group: a group's pages stay
#  together whatever `page_order` says, so with a group G, a `page_by` value P
#  and column blocks C the three axes come out as
#
#      "across"   G / C / P            "down"   G / P / C
#
#  Each page records its own group in `rtf_paginate_meta$page_group`, so this
#  never depends on reading a page NAME.
#
#  A page's NAME follows the row page it came from either way (that is the only
#  name there is), so under "across" pages that share a name are no longer
#  adjacent, which does change the sectioning:
#  `rtf_tables(auto_section = TRUE)` opens a section where the NAME CHANGES, so
#  "down" keeps a table's column pages in one section while "across"
#  interleaves them into one section per page.  An unnamed page list (what
#  as_rtftables() returns for a single table) opens none at all.
#
#  Why a post-hoc verb on BUILT tables
#  -----------------------------------
#  Applied after `drop_cols` / `stub_vars` / a user `col_header`, so the
#  column positions the author writes mean the FINAL printed columns -- the
#  same convention `set_col_header()` follows.  It also works on a table built
#  by hand with `rtftable()`, not just on an `as_rtftables()` result.
#
#  Column widths
#  -------------
#  `column_widths_twips` is absolute, so a subset already carries the right
#  widths and nothing is scaled.  Relative widths (and the default equal
#  distribution) need help: `.compute_cellx()` re-normalises whatever it is
#  given across the page's table width, so a bare subset would stretch the kept
#  columns to refill the sheet, making a ratio-1 column a different size on
#  every page.
#
#  So each page's table width is scaled by
#
#      share(page) / share(reference)
#
#  where a page's "share" is the sum of its kept relative widths (or simply how
#  many columns it keeps), and the reference is
#
#    width = "fill" (default)  PAGE 1's share -- this fixes the twips per ratio
#                              unit on the first page and reuses it everywhere,
#                              so page 1 (and any block with the same ratio
#                              total) fills the sheet while a ratio-1 column is
#                              the same width on every page;
#    width = "keep"            the WHOLE table's share -- a kept column then has
#                              exactly the width it had before the split, and a
#                              partial block yields a proportionally shorter
#                              page.
#
#  Under "fill" a LATER block totalling more than page 1 scales past 1 and would
#  run off the sheet; paginate_cols() warns, naming the pages.
#
#  The scale is expressed through `table_width_pct_of_writable` (or
#  `table_width_twips` when the table pins an absolute width) so the renderer
#  still resolves the real writable width and the page geometry stays
#  authoritative.  Rounding drift is bounded by a few twips on a page's last
#  column, where `.compute_cellx()` absorbs the remainder.

# Which pages belong to the same GROUP -- the outermost page axis when the row
# pagination made a page per group value (`split = "by_value"`, and the group
# side of `page_by`).  Each page records it in `rtf_paginate_meta$page_group`,
# so the column split never has to read a page NAME to find out.  A list with
# no group information at all is one run: there is nothing to keep together.
.page_group_runs <- function(x) {
  key <- vapply(x, function(t) {
    d <- if (!is.null(t$data)) t$data else t$data_list[[1L]]
    m <- attr(d, "rtf_paginate_meta", exact = TRUE)
    if (is.list(m) && length(m$page_group)) as.character(m$page_group)[1L]
    else NA_character_
  }, character(1L))
  if (all(is.na(key))) return(list(seq_along(x)))
  key[is.na(key)] <- ""
  rl     <- rle(key)
  ends   <- cumsum(rl$lengths)
  starts <- ends - rl$lengths + 1L
  lapply(seq_along(starts), function(i) seq.int(starts[i], ends[i]))
}

# Resolve the carry (row-heading) columns repeated on every column page.
.resolve_carry_cols <- function(carry, tbl, ref) {
  if (is.null(carry)) {
    rt <- tbl$row_title
    return(if (is.null(rt)) 1L else sort(unique(as.integer(rt))))
  }
  if (length(carry) == 0L) return(integer(0))
  sort(unique(.resolve_col_indices(carry, ref, "paginate_cols(carry)")))
}

# `by`: the block key per column, from a SEPARATOR in the column names (the
# part before it -- "Placebo____Day 1" -> "Placebo") or from a vector the
# caller supplies.  Carry columns get NA: they are on every page and belong to
# no block.
.resolve_by_key <- function(by, ref, carry) {
  nm <- names(ref)
  n  <- length(nm)
  key <- if (length(by) == n && n != 1L) {
    as.character(by)
  } else if (is.character(by) && length(by) == 1L) {
    vapply(strsplit(nm, by, fixed = TRUE), `[`, character(1L), 1L)
  } else {
    stop(sprintf(paste0("`by` must be a single separator found in the column ",
                        "names, or one key per column (%d)."), n), call. = FALSE)
  }
  key[carry] <- NA_character_
  key
}

# The blocks a key implies: each RUN of one key, in column order.
.blocks_from_key <- function(key) {
  keep <- which(!is.na(key) & nzchar(key))
  if (length(keep) == 0L) {
    stop("`by` left no columns to split: every column is a carry column.",
         call. = FALSE)
  }
  k   <- key[keep]
  brk <- c(TRUE, k[-1L] != k[-length(k)])
  split(keep, cumsum(brk))
}

# The two-level header `col_header = "names"` builds for ONE page: the key
# above (the group), the remainder below (the visit).  A carry column keeps
# its own name and sits under no spanning cell.
.two_level_header <- function(nm, sep, carry_pos) {
  parts <- strsplit(nm, sep, fixed = TRUE)
  top   <- vapply(parts, `[`, character(1L), 1L)
  bot   <- vapply(parts, function(u) u[length(u)], character(1L))
  bot[carry_pos] <- nm[carry_pos]          # the stub keeps its own label
  data_pos <- setdiff(seq_along(nm), carry_pos)
  cells <- list()
  if (length(data_pos)) {
    g   <- top[data_pos]
    brk <- c(TRUE, g[-1L] != g[-length(g)])
    for (run in split(data_pos, cumsum(brk))) {
      cells[[length(cells) + 1L]] <-
        col_cell(c(min(run), max(run)), top[run][1L])
    }
  }
  list(cells, bot)
}

# Resolve `at` (cut BEFORE these columns) / `cols` (explicit blocks) into a
# list of integer column vectors, each excluding the carry columns.
.resolve_col_blocks <- function(at, cols, ref, carry, by = NULL) {
  n0 <- ncol(ref)
  given <- c(at = !is.null(at), cols = !is.null(cols), by = !is.null(by))
  if (sum(given) > 1L) {
    stop(sprintf("Give one of `at`, `cols` or `by`, not %s.",
                 paste(names(given)[given], collapse = " and ")), call. = FALSE)
  }

  if (!is.null(by)) {
    blocks <- unname(.blocks_from_key(.resolve_by_key(by, ref, carry)))
  } else if (!is.null(cols)) {
    if (!is.list(cols)) {
      stop("`cols` must be a list of column blocks (names or positions).",
           call. = FALSE)
    }
    blocks <- lapply(cols, function(b)
      sort(unique(.resolve_col_indices(b, ref, "paginate_cols(cols)"))))
  } else {
    if (is.null(at) || length(at) == 0L) {
      stop("One of `at` (the columns to cut before), `cols` or `by` is required.",
           call. = FALSE)
    }
    idx <- sort(unique(.resolve_col_indices(at, ref, "paginate_cols(at)")))
    if (any(idx < 2L)) {
      stop("`at` must name columns 2..ncol -- there is nothing before column 1.",
           call. = FALSE)
    }
    bounds <- c(1L, idx, n0 + 1L)
    blocks <- lapply(seq_len(length(bounds) - 1L), function(i)
      seq.int(bounds[i], bounds[i + 1L] - 1L))
  }

  blocks <- lapply(blocks, function(b) setdiff(b, carry))
  blocks <- Filter(function(b) length(b) > 0L, blocks)
  if (length(blocks) == 0L) {
    stop("`paginate_cols()` produced no column blocks: every column is a ",
         "carry (row-heading) column.", call. = FALSE)
  }
  blocks
}

# The internal cut positions implied by a block list: a boundary `b` means the
# page break falls immediately before column `b`.
.col_block_boundaries <- function(blocks) {
  if (length(blocks) < 2L) return(integer(0))
  vapply(blocks[-1L], function(b) as.integer(min(b)), integer(1L))
}

# Error when a page break falls strictly inside a spanning header cell
# (allow_span_break = FALSE).
.check_span_breaks <- function(tbl, boundaries) {
  if (length(boundaries) == 0L) return(invisible(NULL))
  rows <- c(list(tbl$spanning_header), tbl$col_header %||% list())
  for (row in rows) {
    if (!is.list(row) || length(row) == 0L) next
    for (cell in row) {
      if (!is.list(cell)) next
      p <- if (!is.null(cell$pos)) cell$pos else c(cell$from, cell$to)
      p <- suppressWarnings(as.integer(p))
      p <- p[!is.na(p)]
      if (length(p) < 2L) next
      f <- min(p); t <- max(p)
      if (t <= f) next
      hit <- boundaries[boundaries > f & boundaries <= t]
      if (length(hit)) {
        stop(sprintf(
          paste0("`paginate_cols()` would break the spanning header cell ",
                 "'%s' (columns %d..%d) at column %d. Move the cut to a group ",
                 "boundary, or pass allow_span_break = TRUE."),
          cell$label %||% "", f, t, hit[1L]), call. = FALSE)
      }
    }
  }
  invisible(NULL)
}

# The "share" a set of kept columns represents: the sum of their relative
# widths, or simply how many there are when the table has none.  NULL when the
# table pins absolute per-column widths -- there is then nothing to scale.
.col_page_share <- function(tbl, keep, n0) {
  if (!is.null(tbl$column_widths_twips)) return(NULL)
  rel <- tbl$col_rel_width
  if (!is.null(rel) && length(rel) == n0) {
    tot <- sum(as.numeric(rel))
    if (tot > 0) return(sum(as.numeric(rel[keep])))
  }
  as.numeric(length(keep))
}

# The share the scale factors are measured against:
#   width = "fill" -> PAGE 1's share, so the ratio unit (twips per ratio 1) is
#                     fixed by the first page and reused on every other one;
#   width = "keep" -> the WHOLE table's share, so a kept column stays exactly
#                     as wide as it was before the split.
.col_page_reference <- function(tbl, keeps, n0, width) {
  if (!is.null(tbl$column_widths_twips)) return(NULL)
  if (identical(width, "fill")) return(.col_page_share(tbl, keeps[[1L]], n0))
  rel <- tbl$col_rel_width
  if (!is.null(rel) && length(rel) == n0) {
    tot <- sum(as.numeric(rel))
    if (tot > 0) return(tot)
  }
  as.numeric(n0)
}

# Subset a BUILT rtftable to `keep` columns, re-indexing every position-indexed
# field and rescaling the table width (see the note at the top of the file).
.rtftable_keep_cols <- function(tbl, keep, scale = NULL) {
  ref <- if (!is.null(tbl$data)) tbl$data else tbl$data_list[[1L]]
  n0  <- ncol(ref)
  keep <- sort(unique(as.integer(keep)))
  if (length(keep) == 0L) {
    stop("A column page must keep at least one column.", call. = FALSE)
  }

  # -- body (custom attributes do not survive a column subset) -------------
  # The blank-row positions and the page's own pagination meta -- which group /
  # BY value it came from, which `paginate_cols()` nests by and a caller can
  # read -- are re-attached; everything else about the subset is plain `[`.
  sub_df <- function(d) {
    ba <- attr(d, "rtf_blank_rows", exact = TRUE)
    pm <- attr(d, "rtf_paginate_meta", exact = TRUE)
    d2 <- d[keep]
    if (!is.null(ba)) attr(d2, "rtf_blank_rows")    <- ba
    if (!is.null(pm)) attr(d2, "rtf_paginate_meta") <- pm
    d2
  }
  if (!is.null(tbl$data))      tbl$data      <- sub_df(tbl$data)
  if (!is.null(tbl$data_list)) tbl$data_list <- lapply(tbl$data_list, sub_df)

  # -- header block (shares the drop_cols re-indexers; spans are clipped) --
  if (!is.null(tbl$col_header)) {
    tbl$col_header <- .reindex_col_header(tbl$col_header, keep, n0)
  }
  if (!is.null(tbl$col_header_list)) {
    tbl$col_header_list <- lapply(tbl$col_header_list, function(ch) {
      if (is.null(ch)) NULL else .reindex_col_header(ch, keep, n0)
    })
  }
  if (!is.null(tbl$spanning_header)) {
    cells <- lapply(tbl$spanning_header, .reindex_header_cell, keep = keep)
    cells <- Filter(Negate(is.null), cells)
    tbl$spanning_header <- if (length(cells)) cells else NULL
  }

  # -- per-column formatting ----------------------------------------------
  # On a BUILT table col_spec is already the normalised length-ncol list
  # (positional, no `col` keys), so a plain subset is the re-indexing.
  if (!is.null(tbl$col_spec)) tbl$col_spec <- tbl$col_spec[keep]

  for (k in c("col_rel_width", "column_widths_twips")) {
    v <- tbl[[k]]
    if (!is.null(v) && length(v) == n0) tbl[[k]] <- v[keep]
  }

  # -- resolved column-index fields ---------------------------------------
  rt <- match(as.integer(tbl$row_title), keep)
  rt <- rt[!is.na(rt)]
  tbl$row_title <- if (length(rt)) rt else 1L

  # set_decimal_split() metadata, when the table carries it.
  if (!is.null(tbl$decimal_split)) {
    dc <- match(as.integer(tbl$decimal_split$cols), keep)
    dc <- dc[!is.na(dc)]
    if (length(dc)) tbl$decimal_split$cols <- dc else tbl$decimal_split <- NULL
  }

  if (!is.null(tbl$cell_styles)) {
    tbl$cell_styles <- lapply(tbl$cell_styles, function(r) {
      if (is.null(r) || !is.list(r)) return(r)
      lapply(r, function(v) if (length(v) == n0) v[keep] else v)
    })
  }

  # -- width: keep each column exactly as wide as in the full table --------
  # A block that keeps every column needs no rescaling; leave the table's own
  # width settings untouched so such a page is identical to its input.
  if (!is.null(scale) && !isTRUE(all.equal(scale, 1))) {
    if (!is.null(tbl$table_width_twips)) {
      tbl$table_width_twips <-
        as.integer(round(as.numeric(tbl$table_width_twips) * scale))
    } else {
      pct <- tbl$table_width_pct_of_writable %||% 1
      tbl$table_width_pct_of_writable <- as.numeric(pct) * scale
    }
  }

  tbl
}


# Put the page's own header on it: the caller's full-table header sliced to the
# columns this page kept (the same reindexer the table's own header goes
# through), or the two-level header built from the names.  NULL leaves the
# table's header exactly as .rtftable_keep_cols() left it.
.page_with_header <- function(tbl, keep, ref, carry, col_header, hdr_names, by) {
  if (isTRUE(hdr_names)) {
    pos <- match(intersect(keep, carry), keep)
    hdr <- .two_level_header(names(ref)[keep], by, pos)
    return(set_col_header(tbl, hdr[[1L]], hdr[[2L]]))
  }
  if (is.null(col_header)) return(tbl)
  tbl$col_header <- .reindex_col_header(col_header, keep, ncol(ref))
  if (!is.null(tbl$col_header_list)) {
    tbl$col_header_list <- lapply(seq_along(tbl$col_header_list),
                                  function(i) tbl$col_header)
  }
  tbl
}

#' Paginate a table horizontally, by columns
#'
#' @description
#' Splits a table across pages **by column** -- the horizontal counterpart of
#' the row pagination [as_rtftables()] performs -- repeating the row-heading
#' column(s) on every page so each one can be read on its own.
#'
#' Row splitting happens first; this verb then splits the resulting pages, and
#' `page_order` says which axis the **page number advances along first** --
#' `"across"` the columns (the default: the column block advances, the row
#' pages running inside it) or `"down"` the rows (the row page advances, its
#' column blocks following it). Two row pages by three column blocks come out
#' as `col1/row1`, `col1/row2`, `col2/row1`, ... under `"across"` and
#' `row1/col1`, `row1/col2`, `row1/col3`, `row2/col1`, ... under `"down"`.
#' A **group** is above both: see *Page order*.
#'
#' ```r
#' as_rtftables(x, max_rows = 20) |> paginate_cols(at = c(4, 6))
#' ```
#'
#' Because it runs on **built** tables, the positions refer to the final
#' printed columns -- after `drop_cols`, `stub_vars` and any user
#' `col_header` -- the same convention [set_col_header()] uses.
#'
#' @section Column widths:
#' `column_widths_twips` is absolute, so a subset already carries the right
#' widths and `width` has no effect. Relative widths (and the default equal
#' distribution) need a rule, because `.compute_cellx()` re-normalises whatever
#' it is given across the page: a bare subset would stretch the kept columns to
#' refill the sheet, making a ratio-1 column a different size on every page.
#'
#' `width = "fill"` (the default) fixes the **twips per ratio unit on page 1**
#' and reuses it everywhere. Page 1 -- and any block with the same ratio total
#' -- fills the sheet, while a given ratio is the same width on every page.
#' With `rel = c(3, 1, 1, 1, 1, 1, 1, 1, 1)` on a 13680-twip page:
#'
#' | blocks  | unit   | page widths      |
#' |---------|--------|------------------|
#' | 4 + 4   | 1954.3 | 100% / 100%      |
#' | 4 + 3   | 1954.3 | 100% / 85.7%     |
#' | 2+2+2+2 | 2736.0 | 100% on all four |
#'
#' `width = "keep"` measures against the whole table instead, so a kept column
#' has exactly the width it had before the split and a partial block yields a
#' proportionally shorter page.
#'
#' A block totalling **more** ratio than page 1 scales past the sheet under
#' `"fill"`; a warning names the pages. Order the blocks so the widest comes
#' first, or use `"keep"`.
#'
#' @section Spanning headers:
#' Spanning cells are clipped to each page's columns. By default a cut may
#' fall inside a spanning group, and the group's label is repeated over its
#' remaining columns on each page; `allow_span_break = FALSE` rejects such a
#' cut instead.
#'
#' @param x An [rtftable()], or a list of them (pages from [as_rtftables()]).
#'   Every page in a list must have the same columns.
#' @param at Columns to cut **before**, as names or positions (symmetric with
#'   `split_rows`). `at = c(4, 6)` yields blocks `1:3`, `4:5`, `6:ncol`.
#' @param cols Explicit column blocks as a list, e.g. `list(2:3, 4:5)`. Give
#'   either `at` or `cols`.
#' @param by Where to cut, taken from the **columns themselves** instead of
#'   positions: either a **separator** found in the column names (a single
#'   string -- `"Placebo____Day 1"` with `by = "____"` keys on `"Placebo"`), or
#'   one **key per column**. Each run of one key becomes a block, so a wide
#'   table laid out as `<group>____<visit>` splits per group with no positions
#'   to count. Carry columns belong to no block. Give one of `at`, `cols` or
#'   `by`.
#' @param col_header The header each page should carry, written **once** for
#'   the whole table:
#'   \describe{
#'     \item{a header}{in the **full table's** coordinates -- a label row, or
#'       rows of [col_cell()] -- sliced to each page's columns, spanning cells
#'       clipped, exactly as a header already on the table is.}
#'     \item{`"names"`}{build the **two-level** header the column names
#'       already carry: the `by` key on top (the group), the rest of the name
#'       below (the visit). Needs `by` to be a separator. A carry column keeps
#'       its own label and sits under no spanning cell.}
#'     \item{`NULL`}{(default) leave the table's own header, which is sliced
#'       per page either way.}
#'   }
#'   This is the place to put it: a header written for the whole table does
#'   **not** fit a page that kept only some of its columns, so applying one
#'   after the split (`set_col_header()` on the page list,
#'   `rtf_tables(col_header = )`) is an error.
#' @param carry Row-heading columns repeated on every page. Defaults to the
#'   table's `row_title` (column 1 unless set). `carry = integer(0)` repeats
#'   nothing. Carry columns are removed from the blocks automatically, so they
#'   are never printed twice on a page.
#' @param allow_span_break Allow a cut inside a spanning header cell. Default
#'   `TRUE`.
#' @param width How relative widths are rescaled after the split. `"fill"`
#'   (default) fixes the twips-per-ratio unit on **page 1** and reuses it on
#'   every page, so page 1 fills the sheet and a given ratio is the same width
#'   throughout; `"keep"` gives each kept column exactly the width it had
#'   before the split. No effect under `column_widths_twips`. See
#'   *Column widths*.
#' @param page_order Which axis the page number advances along first when a
#'   table is split both ways. `"across"` (default) advances **across the
#'   columns** -- the column block first, the row pages running inside it;
#'   `"down"` advances **down the rows** -- the row page first, its column
#'   blocks following it. A group is above both and is never broken up (see
#'   *Page order*). The pages themselves are identical; only their order
#'   differs. A page's name follows its row page either way, so under
#'   `"across"` pages sharing a name are no longer adjacent -- which does not
#'   change the sectioning, see *Page names*.
#' @param ... Unused.
#'
#' @section Columns named `<group>__<item>`:
#' The common wide layout -- one column per treatment x visit -- splits per
#' group and wants a two-level header, group over visit. `by` and
#' `col_header = "names"` do both from the names:
#'
#' ```r
#' as_rtftables(df, split = "by_value", group_col = "period",
#'              stub_vars = c("row_grp1", "label")) |>
#'   paginate_cols(by = "____", carry = 1, col_header = "names")
#' ```
#'
#' ```
#' page 1                        page 2
#' |            | Placebo      | |            | HOGE-001     |
#' | Group      | D1 | D2 | D8 | | Group      | D1 | D2 | D8 |
#' ```
#'
#' @section Page order:
#' `page_order` orders the pages this verb returns. `"across"` (the default)
#' advances the **column block** first and runs the row pages inside it;
#' `"down"` advances the **row page** first and lets its column blocks follow.
#'
#' A **group** sits above both. When the row pagination made a page per group
#' value -- `as_rtftables(split = "by_value", group_col = )`, with `page_by`
#' running inside it -- the column split happens **within** one group, so a
#' group's pages always stay together. With a group `G`, a `page_by` value `P`
#' and column blocks `C`:
#'
#' | `page_order` | page sequence |
#' |---|---|
#' | `"across"` | `G` / `C` / `P` |
#' | `"down"`   | `G` / `P` / `C` |
#'
#' Each page records its own group in `rtf_paginate_meta$page_group` (and its
#' `page_by` value beside it), so none of this depends on reading a page name
#' -- which carries the group alone.
#'
#' @section Page names:
#' A column page inherits the name of the row page it was cut from, whatever
#' `page_order` is -- that is the only name there is. Under `"across"` pages
#' that share a name are therefore no longer adjacent.
#'
#' Pages sharing a heading are numbered with a `"...n"` tail
#' (`"Period 1...1"`, `"Period 1...2"`) so the returned list stays addressable
#' by name; the tail is stripped wherever the name is used as a heading.
#'
#' That is worth knowing because `rtf_tables(auto_section = TRUE)` opens a
#' section where the name **changes**: a run of pages sharing a name is one
#' section, and a page whose name is `""` joins the section before it. So
#' `"down"` keeps a table's column pages in one section, while `"across"`
#' interleaves the names and gives a section per page. An **unnamed** list --
#' what [as_rtftables()] returns for a single table, and what
#' `paginate_cols()` then passes through -- opens none at all, leaving every
#' page in the document's own section.
#'
#' To put pages with *different* names in one section, blank the ones that
#' should not open a new one ([combine_sections()] does that bookkeeping when
#' you are assembling whole tables).
#'
#' @return A list of [rtftable()] pages. Names are carried through unchanged --
#'   each column page keeps its row page's name (see *Page names*).
#'
#' @seealso [as_rtftables()] for row pagination; [set_col_header()] for the
#'   same final-column addressing; [combine_sections()] for the page names
#'   `rtf_tables(auto_section = TRUE)` reads.
#'
#' @examples
#' df <- data.frame(Parameter = c("Mean", "SD"),
#'                  A_n = c("86", "86"), A_mean = c("45.2", "12.3"),
#'                  B_n = c("84", "84"), B_mean = c("44.8", "11.9"),
#'                  stringsAsFactors = FALSE)
#' pages <- rtftable(df) |> paginate_cols(at = 4)
#' length(pages)                 # 2 column pages
#' names(pages[[1]]$data)        # Parameter repeated on both
#' @export
paginate_cols <- function(x, ...) UseMethod("paginate_cols")

#' @rdname paginate_cols
#' @export
paginate_cols.rtftable <- function(x, at = NULL, cols = NULL, by = NULL,
                                   carry = NULL, col_header = NULL,
                                   allow_span_break = TRUE,
                                   width = c("fill", "keep"),
                                   page_order = c("across", "down"), ...) {
  paginate_cols(list(x), at = at, cols = cols, by = by, carry = carry,
                col_header = col_header,
                allow_span_break = allow_span_break, width = width,
                page_order = page_order, ...)
}

#' @rdname paginate_cols
#' @export
paginate_cols.list <- function(x, at = NULL, cols = NULL, by = NULL,
                               carry = NULL, col_header = NULL,
                               allow_span_break = TRUE,
                               width = c("fill", "keep"),
                               page_order = c("across", "down"), ...) {
  .check_own_dots(list(...), paginate_cols.list, "paginate_cols")
  width      <- match.arg(width)
  page_order <- match.arg(page_order)
  if (length(x) == 0L) return(list())
  ok <- vapply(x, inherits, logical(1L), "rtftable")
  if (!all(ok)) {
    stop(sprintf(
      "`paginate_cols()` on a list expects every element to be an rtftable (as returned by as_rtftables()); element %d is '%s'.",
      which(!ok)[1L], paste(class(x[[which(!ok)[1L]]]), collapse = "/")),
      call. = FALSE)
  }

  ref_of <- function(t) if (!is.null(t$data)) t$data else t$data_list[[1L]]
  ref    <- ref_of(x[[1L]])
  ncols  <- vapply(x, function(t) ncol(ref_of(t)), integer(1L))
  if (any(ncols != ncols[1L])) {
    stop(sprintf(
      "`paginate_cols()` needs every page to have the same columns; page 1 has %d and page %d has %d.",
      ncols[1L], which(ncols != ncols[1L])[1L],
      ncols[which(ncols != ncols[1L])[1L]]), call. = FALSE)
  }

  carry_idx <- .resolve_carry_cols(carry, x[[1L]], ref)
  blocks    <- .resolve_col_blocks(at, cols, ref, carry_idx, by = by)

  # `col_header`: a header written for the WHOLE table, sliced per page -- or
  # "names", which builds the two-level header the column names already carry
  # (the `by` key above, the rest below).  Either way the caller writes the
  # header once, in the coordinates of the table they can see.
  hdr_names <- identical(col_header, "names")
  if (hdr_names) {
    if (!(is.character(by) && length(by) == 1L)) {
      stop("`col_header = \"names\"` needs `by` to be the separator in the ",
           "column names (e.g. by = \"____\").", call. = FALSE)
    }
  } else if (!is.null(col_header)) {
    .check_col_header_width(col_header, ncol(ref), "paginate_cols(col_header)")
    # Resolve it ONCE, against the whole table: a `col_cell()` carries a `pos`
    # (names or positions) that has to become the {from, to} the renderer
    # reads, and the full table is the coordinate space the caller wrote in.
    # Each page then takes its slice of the resolved header.
    col_header <- .normalize_col_header_rows(col_header, ncol(ref), names(ref))
  }

  if (!isTRUE(allow_span_break)) {
    bounds <- .col_block_boundaries(blocks)
    for (tbl in x) .check_span_breaks(tbl, bounds)
  }

  keeps <- lapply(blocks, function(b) sort(unique(c(carry_idx, b))))

  # Width rescaling: one factor per column block, measured against page 1's
  # share ("fill") or the whole table's ("keep").  NULL throughout when the
  # table pins absolute widths -- there is then nothing to scale.
  n0     <- ncol(ref)
  ref_sh <- .col_page_reference(x[[1L]], keeps, n0, width)
  scales <- if (is.null(ref_sh) || ref_sh <= 0) {
    vector("list", length(keeps))
  } else {
    lapply(keeps, function(k) .col_page_share(x[[1L]], k, n0) / ref_sh)
  }
  over <- which(vapply(scales, function(s)
    !is.null(s) && s > 1 + 1e-9, logical(1L)))
  if (length(over) && identical(width, "fill")) {
    warning(sprintf(
      paste0("`paginate_cols()`: column block%s %s total%s more than block 1, so ",
             "%s page%s wider than the sheet. Put the widest block first, or ",
             "use width = \"keep\"."),
      if (length(over) == 1L) "" else "s",
      paste(over, collapse = ", "),
      if (length(over) == 1L) "s" else "",
      if (length(over) == 1L) "its" else "their",
      if (length(over) == 1L) " is" else "s are"), call. = FALSE)
  }

  in_names <- names(x)
  out    <- vector("list", length(keeps) * length(x))
  onames <- character(length(out))
  k <- 0L
  # A group (`as_rtftables(split = "by_value", group_col = )`) is the OUTERMOST
  # page axis and is never broken up: the column split happens INSIDE one
  # group, so a group's pages stay together whatever `page_order` says.
  # Within a group, `page_order` decides which axis the page number advances
  # along first -- "across" the columns (the column block advances, then the
  # row pages inside it) or "down" the rows (the row page advances, and its
  # column blocks follow it).
  across <- identical(page_order, "across")
  for (run in .page_group_runs(x)) {
    if (across) {
      for (bi in seq_along(keeps)) for (i in run) {
        k <- k + 1L
        out[[k]]  <- .page_with_header(
          .rtftable_keep_cols(x[[i]], keeps[[bi]], scales[[bi]]),
          keeps[[bi]], ref, carry_idx, col_header, hdr_names, by)
        onames[k] <- if (!is.null(in_names)) in_names[i] else ""
      }
    } else {
      for (i in run) for (bi in seq_along(keeps)) {
        k <- k + 1L
        out[[k]]  <- .page_with_header(
          .rtftable_keep_cols(x[[i]], keeps[[bi]], scales[[bi]]),
          keeps[[bi]], ref, carry_idx, col_header, hdr_names, by)
        onames[k] <- if (!is.null(in_names)) in_names[i] else ""
      }
    }
  }
  if (!is.null(in_names)) names(out) <- .uniquify_page_names(onames)
  out
}

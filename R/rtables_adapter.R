# ============================================================================
#  rtables_adapter -- read configuration from an rtables / tern table object
# ============================================================================
#
#  Bridges the rtables ecosystem (rtables, and tern which builds on it) with
#  rtfreporter.  Any rtables table is a `VTableTree` (`TableTree` /
#  `ElementaryTable`); tern's analysis functions all return one, so supporting
#  `VTableTree` covers both packages.
#
#  Unlike gt -- which exposes its components through named list slots -- the
#  rtables family renders through a single canonical structure, the
#  `MatrixPrintForm` produced by `formatters::matrix_form()`.  That form is
#  exactly what every rtables back-end (txt, RTF, ...) consumes, so reading it
#  gives us the same content rtables itself would print:
#
#    * mf_strings()  -- character matrix of formatted cells.  The first
#                       `mf_nlheader()` rows are the column-header rows; column
#                       1 is the row-label (stub) column; the rest are data.
#    * mf_spans()    -- per-cell column span (for multi-level column headers).
#    * mf_aligns()   -- per-cell alignment ("left"/"center"/"right").
#    * mf_rinfo()    -- per-body-row metadata: `indent`, `node_class`
#                       ("LabelRow" group headers vs "DataRow"), `label`.
#    * main_title() / subtitles() / main_footer() / prov_footer() and the
#      referential footnotes (mf_rfnotes()) -- page-level title / footnote
#      material.
#
#  rtables embeds referential footnote marks directly in the cell text as
#  `{N}` (e.g. "37.7 {1}", "40.3 {1, 2}").  With the "footnote_marks" token we
#  rewrite these to rtfreporter superscript markup `^{N}`.
#
#  Tokens recognised (all on by default via read = TRUE):
#    "col_header"     -- leaf column labels (bottom header row)
#    "alignment"      -- per-column alignment
#    "spanning"       -- upper (spanner) header rows -> stacked col_header
#    "titles"         -- main title + subtitles -> page title block
#    "footnotes"      -- referential footnote texts + main/prov footer
#                        -> page footnote block
#    "footnote_marks" -- in-cell {N} -> ^{N} superscript markup
#  (Row-label indentation is rendered into the stub text by
#   matrix_form(indent_rownames = TRUE); it is not a separate token.)

.RTABLES_TOKENS_ALL <- c("col_header", "alignment", "spanning", "titles",
                         "footnotes", "footnote_marks")


# ── Detection ──────────────────────────────────────────────────────────────

# Is `x` an rtables/tern table (S4 VTableTree)?  Cheap, dependency-light.
.is_rtables_tbl <- function(x) {
  isS4(x) && methods::is(x, "VTableTree")
}


# ── Token resolution ─────────────────────────────────────────────────────────

.resolve_rtables_tokens <- function(read) {
  .resolve_meta_tokens(read, .RTABLES_TOKENS_ALL, "rtables")
}


# ── Helpers ──────────────────────────────────────────────────────────────────

# Convert rtables in-cell footnote marks {N} / {N, M} to rtfreporter ^{N}
# superscript markup, across every character column.  Backreference-free
# (some locale-broken R builds drop gsub \\1 replacements): each matched
# "{...}" run is prefixed with "^" via regmatches replacement.
.convert_rtables_marks <- function(data) {
  pat <- "\\{[0-9][0-9, ]*\\}"
  for (j in seq_len(ncol(data))) {
    col <- data[[j]]
    if (!is.character(col)) next
    m <- gregexpr(pat, col, perl = TRUE)
    regmatches(col, m) <- lapply(regmatches(col, m),
                                 function(v) if (length(v)) paste0("^", v) else v)
    data[[j]] <- col
  }
  data
}

# Walk one header row's span vector into contiguous (from, to, label) segments.
.rtables_header_segments <- function(labels, spans) {
  n <- length(labels)
  segs <- list()
  j <- 1L
  while (j <= n) {
    s <- as.integer(spans[j])
    if (is.na(s) || s < 1L) s <- 1L
    to <- min(j + s - 1L, n)
    segs[[length(segs) + 1L]] <- list(from = j, to = to,
                                       label = as.character(labels[j]))
    j <- to + 1L
  }
  segs
}


# ── Central mapping: MatrixPrintForm + tokens -> rtftable kwargs ─────────────

# The reader below is deliberately typed on the *MatrixPrintForm*, not on
# VTableTree: it never touches an rtables-specific slot, only the formatters
# accessors.  So every package that renders through `matrix_form()` can share
# it -- rtables/tern here, rlistings in R/rlistings_adapter.R.  Each adapter
# keeps its own detector, tokens and typed wrapper; only this body is common.
#
# Returns a list with the same shape the gt adapter produces, so as_rtftables()
# / as_rtftable() can consume either source identically:
#   data, col_header, col_spec, cell_styles, titles_block, footnotes_block.
.rtables_to_rtftable_kwargs <- function(x, tokens = .RTABLES_TOKENS_ALL) {
  if (!.is_rtables_tbl(x)) {
    stop("`x` must be an rtables/tern table (VTableTree).", call. = FALSE)
  }
  .mpf_to_rtftable_kwargs(x, tokens = tokens,
                          what = "an rtables/tern table")
}

.mpf_to_rtftable_kwargs <- function(x, tokens = .RTABLES_TOKENS_ALL,
                                    what = "a MatrixPrintForm table") {
  .need_pkg("formatters", paste("Reading", what))

  # `indent_rownames = TRUE` bakes the row-label indentation into the
  # rendered stub text (leading spaces), exactly as rtables itself prints it.
  # Without it the indentation is lost (mf_rinfo()$indent is 0 for many
  # layouts), which is why nested PT rows came out flush-left.
  mf      <- formatters::matrix_form(x, indent_rownames = TRUE)
  strings <- formatters::mf_strings(mf)
  nlh     <- formatters::mf_nlheader(mf)
  ncol_t  <- ncol(strings)
  nrow_t  <- nrow(strings)

  out <- list()

  # ---- body data.frame (always; header rows are structural, not metadata) --
  body_idx <- if (nlh < nrow_t) seq.int(nlh + 1L, nrow_t) else integer(0)
  body     <- strings[body_idx, , drop = FALSE]
  df <- as.data.frame(body, stringsAsFactors = FALSE, optional = TRUE)
  names(df) <- paste0("V", seq_len(ncol_t))
  rownames(df) <- NULL
  out$data <- df
  nbody <- nrow(df)

  # ---- "footnote_marks": rewrite {N} -> ^{N} in the body ------------------
  if ("footnote_marks" %in% tokens) {
    out$data <- .convert_rtables_marks(out$data)
  }

  # ---- column header rows (leaf labels + optional spanners) ---------------
  spans <- formatters::mf_spans(mf)
  want_header  <- "col_header" %in% tokens
  want_spanned <- "spanning"   %in% tokens

  bottom_row <- if (want_header && nlh >= 1L)
                  as.character(strings[nlh, ]) else NULL

  span_rows <- list()
  if (want_spanned && nlh >= 2L) {
    for (h in seq_len(nlh - 1L)) {
      segs <- .rtables_header_segments(strings[h, ], spans[h, ])
      cells <- lapply(segs, function(sg) {
        pos <- if (sg$from == sg$to) sg$from else c(sg$from, sg$to)
        col_cell(pos = pos, label = sg$label)
      })
      span_rows[[length(span_rows) + 1L]] <- cells
    }
  }

  if (length(span_rows)) {
    bottom <- bottom_row %||% names(out$data)
    out$col_header <- c(span_rows, list(bottom))
  } else if (!is.null(bottom_row)) {
    out$col_header <- bottom_row
  }

  # ---- "alignment": per-column align from the first body data row ---------
  if ("alignment" %in% tokens && nbody > 0L) {
    aligns <- formatters::mf_aligns(mf)
    arow   <- as.character(aligns[nlh + 1L, ])
    arow[!arow %in% c("left", "center", "right")] <- "left"
    out$col_spec <- lapply(seq_len(ncol_t), function(j) {
      list(col = j, align = arow[[j]])
    })
  }

  # NB: row-label indentation is rendered into the stub text by
  # matrix_form(indent_rownames = TRUE) above (leading spaces), so no
  # separate per-cell indent extraction is needed.

  # ---- "titles": main title + subtitles -----------------------------------
  if ("titles" %in% tokens) {
    out$titles_block <- .mpf_text_block(formatters::main_title(x),
                                        formatters::subtitles(x))
  }

  # ---- "footnotes": referential footnote texts + main/prov footer ---------
  if ("footnotes" %in% tokens) {
    refs <- tryCatch(formatters::mf_rfnotes(mf), error = function(e) character(0))
    out$footnotes_block <- .mpf_text_block(refs,
                                           formatters::main_footer(x),
                                           formatters::prov_footer(x))
  }

  out
}

# Collect title / footnote material into one character vector, or NULL when
# there is none.
#
# The pieces cannot be `c()`-ed straight together: the same accessor returns
# different types across the two sources -- `mf_rfnotes()` is `character(0)`
# for an rtables table but an empty `list()` for an rlistings listing -- and
# one list among the arguments turns the whole `c()` into a list, which is not
# what a title block is.  unlist() first, coerce, then drop the empties.
.mpf_text_block <- function(...) {
  v <- unlist(list(...), use.names = FALSE)
  if (!length(v)) return(NULL)
  v <- as.character(v)
  v <- v[!is.na(v) & nzchar(v)]
  if (length(v)) v else NULL
}

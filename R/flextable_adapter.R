# ============================================================================
#  flextable_adapter -- read configuration from a flextable object
# ============================================================================
#
#  Bridges the flextable package with rtfreporter.  A `flextable` is a list
#  (class "flextable") of header / body / footer parts.
#
#  The subtlety: a flextable's *displayed* cell text is NOT its `$body$dataset`.
#  Header labels set with `set_header_labels()` and numbers formatted with
#  `colformat_*()` live in a separate "chunk" layer; `$dataset` keeps the raw
#  input.  So we read the rendered text through flextable's EXPORTED
#  introspection helpers, keeping the adapter on the package's public API:
#
#    * information_data_chunk(x)     -- one row per text chunk, with columns
#                                       `.part`, `.row_id`, `.col_id`,
#                                       `.chunk_index`, `txt`.  Pasting the
#                                       chunks of a cell (in `.chunk_index`
#                                       order) yields the displayed text, with
#                                       header labels and colformat_*()
#                                       formatting already applied.
#    * information_data_paragraph(x) -- per-cell paragraph properties incl.
#                                       `text.align`, used for column alignment.
#    * x$header$spans$rows           -- per-header-row span widths (1 = cell
#                                       start, n = a span of n, 0 = covered),
#                                       used to rebuild spanning header rows.
#    * x$caption$value               -- the table title.
#
#  Tokens recognised (all on by default via read_meta = TRUE):
#    "col_header" -- leaf column labels (bottom header row)
#    "alignment"  -- per-column alignment
#    "spanning"   -- upper header rows -> stacked col_header
#    "titles"     -- caption -> page title block
#    "footnotes"  -- footer lines -> page footnote block
#
#  Not carried (consistent with the gt adapter): per-cell bold / italic /
#  colour / fill, explicit column widths, and `footnote()` reference marks.

.FLEXTABLE_TOKENS_ALL <- c("col_header", "alignment", "spanning",
                           "titles", "footnotes")


# -- Detection ---------------------------------------------------------------

# Is `x` a flextable?  Cheap, dependency-light (class string check only).
.is_flextable_tbl <- function(x) inherits(x, "flextable")


# -- Token resolution --------------------------------------------------------

.resolve_flextable_tokens <- function(read) {
  .resolve_meta_tokens(read, .FLEXTABLE_TOKENS_ALL, "flextable")
}


# -- Helpers -----------------------------------------------------------------

# Reshape information_data_chunk() output for one part ("header"/"body"/
# "footer") into a character matrix of (rows x col_keys).  A cell's text is its
# chunks pasted in `.chunk_index` order, so multi-run / formatted cells render
# exactly as flextable displays them.  Column order is `col_keys`; row order is
# the part's natural top-to-bottom order.
.flextable_part_matrix <- function(chunks, part, col_keys) {
  ncol_t <- length(col_keys)
  d <- chunks[chunks$.part == part, , drop = FALSE]
  if (!nrow(d)) {
    return(matrix(character(0), nrow = 0L, ncol = ncol_t,
                  dimnames = list(NULL, col_keys)))
  }
  d$txt[is.na(d$txt)] <- ""
  rids <- sort(unique(as.integer(d$.row_id)))
  m <- matrix("", nrow = length(rids), ncol = ncol_t,
              dimnames = list(NULL, col_keys))
  for (i in seq_along(rids)) {
    di <- d[as.integer(d$.row_id) == rids[i], , drop = FALSE]
    for (j in seq_len(ncol_t)) {
      cj <- di[di$.col_id == col_keys[j], , drop = FALSE]
      if (nrow(cj)) {
        cj <- cj[order(cj$.chunk_index), , drop = FALSE]
        m[i, j] <- paste0(cj$txt, collapse = "")
      }
    }
  }
  m
}


# -- Central mapping: flextable + tokens -> rtftable kwargs -------------------

# Returns a list with the same shape the gt / rtables adapters produce, so
# as_rtftables() / as_rtftable() can consume any source identically:
#   data, col_header, col_spec, titles_block, footnotes_block.
.flextable_to_rtftable_kwargs <- function(x, tokens = .FLEXTABLE_TOKENS_ALL) {
  if (!.is_flextable_tbl(x)) {
    stop("`x` must be a flextable object.", call. = FALSE)
  }
  .need_pkg("flextable", "Reading a flextable")

  col_keys <- x$col_keys
  ncol_t   <- length(col_keys)
  chunks   <- as.data.frame(flextable::information_data_chunk(x),
                            stringsAsFactors = FALSE)

  out <- list()

  # ---- body data.frame (header rows are structural, not data) -------------
  body_mat <- .flextable_part_matrix(chunks, "body", col_keys)
  df <- as.data.frame(body_mat, stringsAsFactors = FALSE, optional = TRUE)
  names(df)    <- paste0("V", seq_len(ncol_t))
  rownames(df) <- NULL
  out$data <- df

  # ---- column header rows (leaf labels + optional spanners) ---------------
  header_mat   <- .flextable_part_matrix(chunks, "header", col_keys)
  nlh          <- nrow(header_mat)
  want_header  <- "col_header" %in% tokens
  want_spanned <- "spanning"   %in% tokens

  bottom_row <- if (want_header && nlh >= 1L)
                  as.character(header_mat[nlh, ]) else NULL

  span_rows <- list()
  if (want_spanned && nlh >= 2L) {
    spans <- x$header$spans$rows                 # nlh x ncol matrix (or NULL)
    for (h in seq_len(nlh - 1L)) {
      span_vec <- if (!is.null(spans) && nrow(spans) >= h)
                    spans[h, ] else rep(1L, ncol_t)
      # `.rtables_header_segments()` is a generic span-vector -> segments
      # helper (shared with the rtables adapter): the flextable span encoding
      # (1 = start, n = span n, 0 = covered) is read identically.
      segs  <- .rtables_header_segments(header_mat[h, ], span_vec)
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

  # ---- "alignment": per-column align from the body paragraphs -------------
  if ("alignment" %in% tokens && nrow(df) > 0L) {
    pg <- as.data.frame(flextable::information_data_paragraph(x),
                        stringsAsFactors = FALSE)
    pb <- pg[pg$.part == "body", , drop = FALSE]
    out$col_spec <- lapply(seq_len(ncol_t), function(j) {
      av <- pb$text.align[pb$.col_id == col_keys[j]]
      a  <- if (length(av)) as.character(av[[1L]]) else "left"
      if (!a %in% c("left", "center", "right")) a <- "left"
      list(col = j, align = a)
    })
  }

  # ---- "titles": the caption ----------------------------------------------
  if ("titles" %in% tokens) {
    cap <- x$caption$value
    cap <- cap[!is.na(cap) & nzchar(cap)]
    if (length(cap)) out$titles_block <- as.character(cap)
  }

  # ---- "footnotes": footer lines ------------------------------------------
  # `add_footer_lines()` writes the same line into every column, so per footer
  # row we take its single distinct non-empty value.
  if ("footnotes" %in% tokens) {
    footer_mat <- .flextable_part_matrix(chunks, "footer", col_keys)
    if (nrow(footer_mat)) {
      fn <- apply(footer_mat, 1L, function(r) {
        u <- unique(r[nzchar(r)])
        if (length(u)) u[[1L]] else ""
      })
      fn <- fn[nzchar(fn)]
      if (length(fn)) out$footnotes_block <- as.character(fn)
    }
  }

  out
}

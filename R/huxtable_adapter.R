# ============================================================================
#  huxtable_adapter -- read configuration from a huxtable object
# ============================================================================
#
#  Bridges the huxtable package with rtfreporter.  A `huxtable` IS a
#  `data.frame` subclass that carries its layout as attributes, so the dispatch
#  in `as_rtftables()` must test for it BEFORE the plain-`data.frame` branch.
#
#  Content: a huxtable stores raw cell values plus a per-cell `number_format`.
#  The *displayed* text (formatting applied) comes from huxtable's own renderer;
#  we read it via `clean_contents(output_type = "screen")` (number formatting
#  applied, no LaTeX/HTML escaping), falling back to the raw values if that
#  internal helper is unavailable.  Read through huxtable's public accessors:
#
#    * header_rows(x) -- which rows are header rows; the LAST header row holds
#                        the leaf column labels, the rows above are spanners.
#    * colspan(x)     -- per-cell column span, used to rebuild spanning rows.
#    * align(x)       -- per-cell alignment.
#    * caption(x)     -- the table caption -> page title block.
#
#  Tokens recognised (all on by default via read_meta = TRUE):
#    "col_header" -- leaf column labels (bottom header row)
#    "alignment"  -- per-column alignment
#    "spanning"   -- upper header rows -> stacked col_header
#    "titles"     -- caption -> page title block
#
#  Not carried (consistent with the gt adapter): per-cell bold / italic /
#  colour / fill.  huxtable has no first-class footnote concept, so footnotes
#  are not extracted.

.HUXTABLE_TOKENS_ALL <- c("col_header", "alignment", "spanning", "titles")


# -- Detection ---------------------------------------------------------------

# Is `x` a huxtable?  Cheap, dependency-light (class string check only).
.is_huxtable_tbl <- function(x) inherits(x, "huxtable")


# -- Token resolution --------------------------------------------------------

.resolve_huxtable_tokens <- function(read) {
  .resolve_meta_tokens(read, .HUXTABLE_TOKENS_ALL, "huxtable")
}


# -- Helpers -----------------------------------------------------------------

# The displayed cell text as a character matrix (rows x cols).  Prefers
# huxtable's renderer so `number_format` is applied; falls back to the raw cell
# values if the internal `clean_contents` helper is not available.
.huxtable_display_matrix <- function(x) {
  # `clean_contents()` is internal to huxtable; reach it via `get()` (not `:::`)
  # so the package keeps a clean R CMD check, and fall back to the raw cell
  # values (number_format not applied) if it is unavailable.
  cc <- tryCatch({
    clean <- get("clean_contents", envir = asNamespace("huxtable"))
    clean(x, output_type = "screen")
  }, error = function(e) NULL)
  if (is.null(cc)) {
    cc <- vapply(seq_len(ncol(x)),
                 function(j) as.character(x[[j]]),
                 character(nrow(x)))
  }
  cc <- as.matrix(cc)
  storage.mode(cc) <- "character"
  cc[is.na(cc)] <- ""
  dimnames(cc) <- NULL
  cc
}


# -- Central mapping: huxtable + tokens -> rtftable kwargs --------------------

# Returns a list with the same shape the gt / rtables / flextable adapters
# produce, so as_rtftables() / as_rtftable() can consume any source identically:
#   data, col_header, col_spec, titles_block.
.huxtable_to_rtftable_kwargs <- function(x, tokens = .HUXTABLE_TOKENS_ALL) {
  if (!.is_huxtable_tbl(x)) {
    stop("`x` must be a huxtable object.", call. = FALSE)
  }
  .need_pkg("huxtable", "Reading a huxtable")

  nr  <- nrow(x)
  nc  <- ncol(x)
  cc  <- .huxtable_display_matrix(x)

  hdr_flag <- as.logical(huxtable::header_rows(x))
  hdr_idx  <- which(hdr_flag)
  body_idx <- setdiff(seq_len(nr), hdr_idx)

  out <- list()

  # ---- body data.frame (header rows are structural, not data) -------------
  body <- cc[body_idx, , drop = FALSE]
  df <- as.data.frame(body, stringsAsFactors = FALSE, optional = TRUE)
  names(df)    <- paste0("V", seq_len(nc))
  rownames(df) <- NULL
  out$data <- df

  # ---- column header rows (leaf labels + optional spanners) ---------------
  want_header  <- "col_header" %in% tokens
  want_spanned <- "spanning"   %in% tokens

  bottom_row <- if (want_header && length(hdr_idx) >= 1L)
                  as.character(cc[hdr_idx[length(hdr_idx)], ]) else NULL

  span_rows <- list()
  if (want_spanned && length(hdr_idx) >= 2L) {
    cspan <- huxtable::colspan(x)
    # Header rows above the leaf row become spanning rows.
    for (h in hdr_idx[-length(hdr_idx)]) {
      # `.rtables_header_segments()` is the shared span-vector -> segments
      # helper: huxtable's colspan (origin = span width, covered cells skipped
      # by the jump) is read the same way as the rtables/flextable encodings.
      segs  <- .rtables_header_segments(cc[h, ], as.integer(cspan[h, ]))
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

  # ---- "alignment": per-column align from the first body row --------------
  if ("alignment" %in% tokens && length(body_idx) > 0L) {
    al   <- huxtable::align(x)
    arow <- as.character(al[body_idx[1L], ])
    arow[arow == "."] <- "right"              # decimal alignment -> right
    arow[!arow %in% c("left", "center", "right")] <- "left"
    out$col_spec <- lapply(seq_len(nc), function(j) {
      list(col = j, align = arow[[j]])
    })
  }

  # ---- "titles": the caption ----------------------------------------------
  if ("titles" %in% tokens) {
    cap <- huxtable::caption(x)
    cap <- cap[!is.na(cap) & nzchar(cap)]
    if (length(cap)) out$titles_block <- as.character(cap)
  }

  out
}

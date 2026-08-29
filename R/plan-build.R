# ============================================================================
#  SPIKE (design/plan-resolver) -- materialise into the EXISTING rtftable
# ============================================================================
#
#  Step 4.  Nothing exported.
#
#  ---------------------------------------------------------------------------
#  Why there is no `rtftable2`
#  ---------------------------------------------------------------------------
#
#  A second table class would have to be understood by rtf_tables(),
#  paginate_cols(), every set_*() verb and the renderer -- doubling the type
#  surface of a package whose stated goal is to have FEWER moving parts.
#
#  It is also unnecessary.  The whole point of deferring is that resolution
#  produces exactly what `rtftable` already is: a body whose positions are
#  final, plus the metadata the renderer reads.  So the plan materialises INTO
#  `rtftable` and every existing consumer keeps working unchanged.
#
#  The new design therefore adds ONE class users touch -- `rtf_plan` -- and one
#  internal one, `rtf_resolution`.  That claim is what this file tests.

# Build one rtftable per page from a resolved plan.
#
# `...` is passed to rtftable(), so borders, widths and the rest keep their
# existing spelling until a style layer replaces them.
#' @keywords internal
plan_tables <- function(plan, ...) {
  res <- if (inherits(plan, "rtf_resolution")) plan else resolve_plan(plan)

  # Styling declared on the plan, resolved once.  Anything passed here still
  # wins, so the spike's existing call sites keep working while the style
  # layer takes over.
  style <- res$style %||% list()
  extra <- list(...)
  for (nm in names(extra)) style[[nm]] <- extra[[nm]]

  # auto_width sizes each column to its widest content so long labels do not
  # wrap.  It is computed ONCE, on the whole resolved body, and applied to
  # every page -- the same rule as_rtftables() follows, and the reason it
  # cannot simply be an rtftable() argument per page.
  if (isTRUE(style$auto_width) && is.null(style$column_widths_twips) &&
      is.null(style$col_rel_width)) {
    hdr <- .flatten_col_header_labels(res$header$col_header,
                                      length(res$columns$names))
    full <- res$rows$body
    if (length(res$columns$hidden)) {
      full <- full[, setdiff(names(full), res$columns$hidden), drop = FALSE]
    }
    tw <- style$table_width_twips
    if (is.null(tw)) {
      nat <- tryCatch(auto_col_widths(full, col_header = hdr),
                      error = function(e) NULL)
      if (!is.null(nat) && sum(nat) > .default_writable_twips()) {
        tw <- .default_writable_twips()
      }
    }
    aw <- tryCatch(auto_col_widths(full, col_header = hdr,
                                   table_width_twips = tw),
                   error = function(e) NULL)
    if (!is.null(aw)) style$column_widths_twips <- aw
  }
  # Settings that shaped the body upstream are not rtftable() arguments.
  style <- style[setdiff(names(style), .PLAN_STYLE_PRE)]

  out <- lapply(seq_along(res$pages), function(i) {
    idx  <- res$pages[[i]]
    # A delegated split may have rewritten a cell -- cont_label does -- so its
    # chunk is used verbatim where one exists.
    body <- if (!is.null(res$page_data)) res$page_data[[i]]
            else res$rows$body[idx, , drop = FALSE]
    # Hidden columns did their work during resolution -- a carrier column can
    # group the table without being printed -- and leave here.  This is the
    # single point where the body view becomes the printed view.
    if (length(res$columns$hidden)) {
      body <- body[, setdiff(names(body), res$columns$hidden), drop = FALSE]
    }
    rownames(body) <- NULL

    args <- c(list(data = body), style)

    # The source's own header, already placed at final positions by the column
    # map.  Nothing here knows what the adapter read or what the stub merged.
    if (!is.null(res$header$col_header) && is.null(args$col_header)) {
      args$col_header <- res$header$col_header
    }
    if (!is.null(res$header$col_header_align) &&
        is.null(args$col_header_align)) {
      args$col_header_align <- res$header$col_header_align
    }

    # Blank positions are output-row coordinates; a page needs its own.  This
    # is the only translation left in the design, and it is arithmetic on one
    # vector rather than a re-index of scattered metadata.
    #
    # A blank falling AFTER the page's last row would print at the foot of the
    # page, which is not a separator between anything.  as_rtftables() drops it
    # and so does this -- a behaviour the spike found by diffing the rendered
    # RTF, not by reading the code.
    if (length(res$blanks)) {
      first <- idx[[1L]]
      last  <- idx[[length(idx)]]
      local <- res$blanks[res$blanks >= first & res$blanks < last] -
        (first - 1L)
      if (length(local)) args$blank_rows <- as.integer(local)
    }

    # Carry columns are already final positions -- the column map resolved
    # them, so nothing here knows what the stub or the hidden columns did.
    if (length(res$columns$carry)) {
      args$row_title <- as.integer(res$columns$carry)
    }

    do.call(rtftable, args)
  })
  if (!is.null(res$page_names)) names(out) <- res$page_names
  out
}

# Per-source-row styles, sliced onto each page.  `plan_row_map()` does the
# insertion translation and the page index does the rest; there is no third
# coordinate system to reconcile.
#' @keywords internal
plan_cell_styles <- function(res, cell_styles) {
  if (is.null(cell_styles)) return(vector("list", length(res$pages)))
  mapped <- plan_row_map(res, cell_styles)
  lapply(res$pages, function(idx) mapped[idx])
}

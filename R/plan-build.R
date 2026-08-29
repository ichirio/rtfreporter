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

  lapply(seq_along(res$pages), function(i) {
    idx  <- res$pages[[i]]
    body <- res$rows$body[idx, , drop = FALSE]
    rownames(body) <- NULL

    args <- list(data = body, ...)

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

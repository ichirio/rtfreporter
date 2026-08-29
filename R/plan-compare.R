# ============================================================================
#  SPIKE (design/plan-resolver) -- the comparison harness
# ============================================================================
#
#  Step 8.  Nothing exported.
#
#  The acceptance criterion for this design is that a plan and the
#  as_rtftables() call it replaces produce the same report.  "The same" has two
#  useful meanings and this file checks both, because each catches what the
#  other misses:
#
#    OBJECT level  the rtftable fields -- body, header, col_spec, widths,
#                  blank rows, row_title.  Says WHERE a difference lives, in
#                  terms a maintainer can act on.
#    RTF level     the rendered command stream.  Says whether the difference
#                  MATTERS.  Twice already an object-level difference turned
#                  out to render identically (a normalised header, an extra
#                  rtf_paginate_meta attribute), and twice an object-level
#                  match hid a real rendering difference (header alignment,
#                  the adapter's col_spec).
#
#  Neither alone is sufficient.  Both together are the criterion.

# Fields worth comparing on an rtftable.  Deliberately not every slot: some
# are derived at render time and some are provenance.
.PLAN_CMP_FIELDS <- c("col_header", "col_header_align", "col_spec",
                      "column_widths_twips", "col_rel_width", "blank_rows",
                      "row_title", "border", "cell_styles", "decimal_split")

# Compare two lists of rtftable pages field by field.
#
# Returns a data.frame, one row per (page, field) that differs, with a short
# rendering of each side.  An empty result means the objects agree.
#' @keywords internal
plan_compare_objects <- function(a, b) {
  brief <- function(x) {
    if (is.null(x)) return("NULL")
    s <- paste(utils::capture.output(utils::str(x, max.level = 2L)),
               collapse = " ")
    substr(gsub("[[:space:]]+", " ", s), 1L, 120L)
  }
  out <- list()
  add <- function(page, field, x, y) {
    out[[length(out) + 1L]] <<-
      data.frame(page = page, field = field, a = brief(x), b = brief(y),
                 stringsAsFactors = FALSE)
  }

  if (length(a) != length(b)) {
    add(NA_integer_, "n_pages", length(a), length(b))
    return(do.call(rbind, out))
  }

  for (i in seq_along(a)) {
    pa <- a[[i]]
    pb <- b[[i]]

    # Body: values and names, ignoring attributes as_rtftables() attaches for
    # its own bookkeeping (rtf_paginate_meta) which never reach the RTF.
    if (!identical(names(pa$data), names(pb$data))) {
      add(i, "names(data)", names(pa$data), names(pb$data))
    } else if (!identical(unname(as.matrix(pa$data)),
                          unname(as.matrix(pb$data)))) {
      add(i, "data", dim(pa$data), dim(pb$data))
    }

    for (f in .PLAN_CMP_FIELDS) {
      if (!identical(pa[[f]], pb[[f]])) add(i, f, pa[[f]], pb[[f]])
    }
  }
  if (length(out) == 0L) {
    return(data.frame(page = integer(0), field = character(0),
                      a = character(0), b = character(0),
                      stringsAsFactors = FALSE))
  }
  do.call(rbind, out)
}

# Render a list of rtftable pages to RTF lines through the ordinary pipeline.
#' @keywords internal
plan_render_lines <- function(pages, page_opts = NULL, file = NULL) {
  doc <- rtf_document(page = page_opts %||% rtf_page(orientation = "landscape"))
  for (p in pages) doc <- rtf_tables(doc, p)
  f <- file %||% tempfile(fileext = ".rtf")
  if (is.null(file)) on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  readLines(f, warn = FALSE)
}

# Compare two RTF command streams.
#
# Returns a list: identical (logical), n_a / n_b (line counts), and a
# data.frame of the differing lines with both sides, truncated for reading.
#' @keywords internal
plan_compare_rtf <- function(la, lb, context = 90L) {
  n <- max(length(la), length(lb))
  pad <- function(x) c(x, rep(NA_character_, n - length(x)))
  xa <- pad(la)
  xb <- pad(lb)
  d <- which(!(!is.na(xa) & !is.na(xb) & xa == xb))
  list(
    identical = length(d) == 0L,
    n_a = length(la), n_b = length(lb),
    diffs = if (length(d) == 0L) {
      data.frame(line = integer(0), a = character(0), b = character(0),
                 stringsAsFactors = FALSE)
    } else {
      data.frame(line = d,
                 a = substr(ifelse(is.na(xa[d]), "<absent>", xa[d]), 1L, context),
                 b = substr(ifelse(is.na(xb[d]), "<absent>", xb[d]), 1L, context),
                 stringsAsFactors = FALSE)
    }
  )
}

# Both levels at once, for one case.
#
# `a` and `b` are lists of rtftable pages.  Returns a report; print it with
# show_comparison().
#' @keywords internal
plan_compare <- function(label, a, b, page_opts = NULL) {
  obj <- plan_compare_objects(a, b)
  rtf <- plan_compare_rtf(plan_render_lines(a, page_opts),
                          plan_render_lines(b, page_opts))
  structure(list(label = label, objects = obj, rtf = rtf,
                 n_pages_a = length(a), n_pages_b = length(b)),
            class = "rtf_plan_comparison")
}

#' @keywords internal
show_comparison <- function(x, max_rows = 6L) {
  ok_obj <- nrow(x$objects) == 0L
  cat(sprintf("%-46s objects: %-9s rtf: %s\n", x$label,
              if (ok_obj) "MATCH" else paste0(nrow(x$objects), " differ"),
              if (x$rtf$identical) "MATCH"
              else sprintf("%d/%d lines differ", nrow(x$rtf$diffs),
                           max(x$rtf$n_a, x$rtf$n_b))))
  if (!ok_obj) {
    for (i in seq_len(min(max_rows, nrow(x$objects)))) {
      r <- x$objects[i, ]
      cat(sprintf("    [obj] page %s  %s\n      as_rtftables: %s\n      plan        : %s\n",
                  r$page, r$field, r$a, r$b))
    }
  }
  if (!x$rtf$identical) {
    for (i in seq_len(min(max_rows, nrow(x$rtf$diffs)))) {
      r <- x$rtf$diffs[i, ]
      cat(sprintf("    [rtf] line %d\n      as_rtftables: %s\n      plan        : %s\n",
                  r$line, r$a, r$b))
    }
  }
  invisible(x)
}

# ============================================================================
#  S3 plot() methods -- visual preview of rtfreporter objects
# ============================================================================
#
#  These methods draw a quick wireframe of an rtfreporter object using base
#  graphics -- no extra dependency.  Their purpose is fast layout inspection
#  before the (more expensive) RTF render: you can eyeball whether your
#  column widths, headers, borders and page composition look right.
#
#  They are also one of the most natural showcases of S3 method dispatch
#  in the package: each method is just a function named plot.<class>().

#' Visualise an `rtf_border_side`
#'
#' Draws a 1cm-wide swatch of the line in the side's own style, width and
#' colour.  Useful for previewing the exact look of a border before it is
#' baked into an RTF document.
#'
#' @param x A border side object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#'
#' @examples
#' plot(rtf_border_side(style = "double", width = 30L, color = "#003366"))
#' @export
plot.rtf_border_side <- function(x, ...) {
  oldpar <- graphics::par(mar = c(2, 2, 2, 2), xpd = NA)
  on.exit(graphics::par(oldpar), add = TRUE)
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 10), ylim = c(0, 4),
                        xaxs = "i", yaxs = "i", asp = 1)
  graphics::title(main = sprintf("<rtf_border_side: %s, %d twips%s>",
                                  x$style, x$width,
                                  if (!is.null(x$color)) paste0(", ", x$color) else ""),
                  cex.main = 0.9)
  .draw_side(x, x0 = 1, x1 = 9, y = 2)
  invisible(x)
}

#' Visualise an `rtf_border`
#'
#' Draws a 1cm square showing which of the four edges (top, bottom, left,
#' right) carry a border, in each side's own style.
#'
#' @param x An [rtf_border()] object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#'
#' @examples
#' plot(rtf_border(top = TRUE, bottom = rtf_border_side(color = "#003366")))
#' @export
plot.rtf_border <- function(x, ...) {
  oldpar <- graphics::par(mar = c(2, 2, 3, 2), xpd = NA)
  on.exit(graphics::par(oldpar), add = TRUE)
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 10), ylim = c(0, 10),
                        xaxs = "i", yaxs = "i", asp = 1)
  graphics::title(main = "<rtf_border>", cex.main = 0.9)
  graphics::rect(2, 2, 8, 8, border = "gray80", lty = "dotted")
  .draw_side(x$top,    x0 = 2, x1 = 8, y = 8)
  .draw_side(x$bottom, x0 = 2, x1 = 8, y = 2)
  .draw_vside(x$left,  y0 = 2, y1 = 8, x = 2)
  .draw_vside(x$right, y0 = 2, y1 = 8, x = 8)
  invisible(x)
}

#' Visualise an `rtf_table_border`
#'
#' Draws a schematic table with the four zones (header, body, first_row,
#' last_row) coloured to show which one provides which border.
#'
#' @param x An [rtf_table_border()] object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#'
#' @examples
#' plot(rtf_border_tfl())          # preview the clinical TFL border zones
#' @export
plot.rtf_table_border <- function(x, ...) {
  oldpar <- graphics::par(mar = c(2, 2, 3, 2), xpd = NA)
  on.exit(graphics::par(oldpar), add = TRUE)
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 12), ylim = c(0, 10),
                        xaxs = "i", yaxs = "i")
  graphics::title(main = "<rtf_table_border>", cex.main = 0.9)

  # Schematic: header row (top), first data row, middle rows, last data row.
  zones <- list(
    list(label = "header",   y0 = 7.5, y1 = 9,   border = x$header),
    list(label = "first_row", y0 = 6,   y1 = 7.5, border = x$first_row %||% x$body),
    list(label = "body",     y0 = 3,   y1 = 6,   border = x$body),
    list(label = "last_row", y0 = 1.5, y1 = 3,   border = x$last_row %||% x$body)
  )
  for (z in zones) {
    graphics::rect(2, z$y0, 10, z$y1, border = "gray80", lty = "dotted")
    graphics::text(1.7, (z$y0 + z$y1) / 2, z$label, adj = c(1, 0.5), cex = 0.8)
    .draw_side(z$border$top,    x0 = 2, x1 = 10, y = z$y1)
    .draw_side(z$border$bottom, x0 = 2, x1 = 10, y = z$y0)
    .draw_vside(z$border$left,  y0 = z$y0, y1 = z$y1, x = 2)
    .draw_vside(z$border$right, y0 = z$y0, y1 = z$y1, x = 10)
  }
  invisible(x)
}

#' Visualise an `rtftable`
#'
#' Draws a wireframe of the table: column-header band, data band (with the
#' row count annotated), spanning headers if any, and column widths drawn
#' proportional to the table's effective layout.  Borders are sketched at
#' the table's outer frame and at zone boundaries (TFL preset is shown by
#' default).
#'
#' @param x An [rtftable()] object.
#' @param width Plot width in inches (the page's writable width is assumed
#'   to be 10 inches = landscape letter minus margins).  Default `8`.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#'
#' @examples
#' tbl <- rtftable(data.frame(Parameter = "Age", Value = "75.1"), border = "tfl")
#' \dontrun{
#' plot(tbl)        # a quick on-screen preview of the table layout
#' }
#' @export
plot.rtftable <- function(x, width = 8, ...) {
  oldpar <- graphics::par(mar = c(2, 2, 3, 2), xpd = NA)
  on.exit(graphics::par(oldpar), add = TRUE)

  # Column widths (relative).
  ref_df <- if (!is.null(x$data_list)) x$data_list[[1L]] else x$data
  ncols  <- if (is.null(ref_df)) 0L else ncol(ref_df)
  if (ncols == 0L) return(invisible(x))
  rel <- if (!is.null(x$column_widths_twips)) as.numeric(x$column_widths_twips)
         else if (!is.null(x$col_rel_width))  as.numeric(x$col_rel_width)
         else rep(1, ncols)
  xs <- c(0, cumsum(rel) / sum(rel)) * width

  # Header rows: count = length(col_header) + spanning row(s).
  n_header_rows <- length(x$col_header %||% list(names(ref_df)))
  if (!is.null(x$spanning_header) && length(x$spanning_header) > 0L) {
    n_header_rows <- n_header_rows + 1L
  }

  # Data row count summary across single-DF or multi-DF mode.
  n_data <- if (!is.null(x$data_list)) {
    sum(vapply(x$data_list, nrow, integer(1L)))
  } else {
    nrow(x$data %||% data.frame())
  }
  n_dfs <- if (!is.null(x$data_list)) length(x$data_list) else 1L

  # Plot canvas: 1 unit per row.
  total_rows <- n_header_rows + 2L              # header band + summary band
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, width),
                        ylim = c(total_rows, 0),
                        xaxs = "i", yaxs = "i")
  graphics::title(main = sprintf(
    "<rtftable> %d col x %d data rows%s",
    ncols, n_data,
    if (n_dfs > 1L) sprintf(" (%d data.frames)", n_dfs) else ""
  ), cex.main = 0.95)

  # Header band.
  graphics::rect(0, 0, width, n_header_rows,
                 col = "gray90", border = NA)
  graphics::text(width / 2, n_header_rows / 2,
                 sprintf("column header x %d", n_header_rows),
                 cex = 0.85, font = 2)

  # Data band.
  graphics::rect(0, n_header_rows, width, n_header_rows + 2L,
                 col = "white", border = NA)
  graphics::text(width / 2, n_header_rows + 1,
                 sprintf("data rows (%d)", n_data),
                 cex = 0.85)

  # Column separators (vertical guide lines).
  for (k in seq_along(xs)[-1L]) {
    graphics::segments(xs[k], 0, xs[k], total_rows,
                       col = "gray85", lty = "dotted")
  }

  # Outer + header borders, using the resolved rtf_table_border.
  b <- x$border
  if (!is.null(b)) {
    if (!is.null(b$header$top))    graphics::segments(0, 0, width, 0, lwd = 2)
    if (!is.null(b$header$bottom)) graphics::segments(0, n_header_rows,
                                                     width, n_header_rows, lwd = 2)
    if (!is.null(b$last_row$bottom)) graphics::segments(0, total_rows,
                                                       width, total_rows, lwd = 2)
  }

  invisible(x)
}

#' Visualise an `rtf_document`
#'
#' Draws a grid of page thumbnails.  Each thumbnail shows the title /
#' content / footnote regions, with header and footer bands sketched in
#' grey.
#'
#' @param x An [rtf_document()] object.
#' @param max_pages Maximum number of pages to draw (default `12`).  Larger
#'   documents are truncated with a note.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#'
#' @examples
#' doc <- rtf_document() |>
#'   rtf_tables(data.frame(Parameter = "Age", Value = "75.1"))
#' \dontrun{
#' plot(doc)        # preview the document's pages on screen
#' }
#' @export
plot.rtf_document <- function(x, max_pages = 12L, ...) {
  n_total <- length(x$contents)
  n_show  <- min(n_total, as.integer(max_pages))
  if (n_show == 0L) {
    graphics::plot.new()
    graphics::title(main = "<rtf_document -- no pages yet>")
    return(invisible(x))
  }

  ncol <- min(n_show, 4L)
  nrow <- ceiling(n_show / ncol)
  oldpar <- graphics::par(mfrow = c(nrow, ncol),
                           mar = c(1.2, 1.2, 2, 1.2), xpd = NA)
  on.exit(graphics::par(oldpar), add = TRUE)

  geo <- .resolve_page_geometry(x$document$page)
  pw  <- geo$width_twips  / 1440
  ph  <- geo$height_twips / 1440

  for (i in seq_len(n_show)) {
    graphics::plot.new()
    graphics::plot.window(xlim = c(0, pw), ylim = c(ph, 0),
                          xaxs = "i", yaxs = "i", asp = 1)
    graphics::title(main = sprintf("Page %d / %d", i, n_total),
                    cex.main = 0.85)
    graphics::rect(0, 0, pw, ph, border = "black")

    # Header / footer bands (sketched grey strips).
    graphics::rect(0.5, 0.4, pw - 0.5, 0.9, col = "gray92", border = NA)
    graphics::text(pw / 2, 0.65, "header", cex = 0.75, col = "gray40")
    graphics::rect(0.5, ph - 0.9, pw - 0.5, ph - 0.4,
                   col = "gray92", border = NA)
    graphics::text(pw / 2, ph - 0.65, "footer", cex = 0.75, col = "gray40")

    # Title / content / footnote zones.
    graphics::rect(0.6, 1.0, pw - 0.6, 1.5, col = "gray97", border = NA)
    graphics::text(pw / 2, 1.25, "title", cex = 0.75, col = "gray50")

    ct <- x$contents[[i]]
    label <- if (inherits(ct, "rtftable")) "table"
             else if (inherits(ct, "rtfplot")) "figure"
             else "content"
    graphics::rect(0.6, 1.7, pw - 0.6, ph - 1.4,
                   col = "white", border = "gray60")
    graphics::text(pw / 2, (1.7 + ph - 1.4) / 2, label, cex = 1.0, font = 2)

    graphics::rect(0.6, ph - 1.3, pw - 0.6, ph - 1.0,
                   col = "gray97", border = NA)
    graphics::text(pw / 2, ph - 1.15, "footnote", cex = 0.7, col = "gray50")
  }

  if (n_total > n_show) {
    graphics::mtext(sprintf("(%d more page(s) not shown -- pass max_pages = %d)",
                             n_total - n_show, n_total),
                    side = 1, line = -1, outer = TRUE, cex = 0.8, col = "gray50")
  }
  invisible(x)
}


# -- Internal helpers --------------------------------------------------------

# Map an rtf_border_side `style` to a base-R lty + lwd pair.
.side_lty_lwd <- function(side) {
  if (is.null(side)) return(list(lty = NA, lwd = NA, col = NA))
  lty <- switch(side$style,
    single = "solid", double = "solid", thick = "solid",
    dash = "dashed", dot = "dotted", "solid"
  )
  lwd <- if (identical(side$style, "thick")) 3
         else if (identical(side$style, "double")) 2
         else 1
  col <- if (!is.null(side$color)) side$color else "black"
  list(lty = lty, lwd = lwd, col = col)
}

# Draw a horizontal side at y between x0 and x1.
.draw_side <- function(side, x0, x1, y) {
  if (is.null(side)) return(invisible())
  spec <- .side_lty_lwd(side)
  graphics::segments(x0, y, x1, y,
                     lty = spec$lty, lwd = spec$lwd, col = spec$col)
  if (identical(side$style, "double")) {
    graphics::segments(x0, y + 0.15, x1, y + 0.15,
                       lty = spec$lty, lwd = spec$lwd, col = spec$col)
  }
}

# Draw a vertical side at x between y0 and y1.
.draw_vside <- function(side, y0, y1, x) {
  if (is.null(side)) return(invisible())
  spec <- .side_lty_lwd(side)
  graphics::segments(x, y0, x, y1,
                     lty = spec$lty, lwd = spec$lwd, col = spec$col)
  if (identical(side$style, "double")) {
    graphics::segments(x + 0.15, y0, x + 0.15, y1,
                       lty = spec$lty, lwd = spec$lwd, col = spec$col)
  }
}

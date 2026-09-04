# ============================================================================
#  fit_listing_widths() / listing_code() -- propose widths, then hand back
#  the code (#369)
# ============================================================================
#
#  Choosing every column's `width` by hand is the tedious part of writing a
#  listing: too narrow and a cell wraps into a tall ragged block, too wide and
#  the listing runs off the sheet.  And the answer is not a matter of taste --
#  it follows from the paper, the margins, the font and the data.
#
#  So compute it.  `fit_listing_widths()` measures what each column actually
#  demands, fits the demands into the width the PAGE leaves, and returns the
#  same `listing_spec` with the missing widths filled in.  `listing_code()`
#  then prints the spec as source you can paste into the program and tune --
#  because the measurement is a starting point, not a verdict: a column whose
#  header must not break, or one you want roomy, is yours to override, and a
#  `width` you set yourself is never touched.
#
#  Two decisions worth stating.  The demand is a QUANTILE of the column's cell
#  widths, not the maximum, so one unusually long value cannot dominate the
#  layout of every other column -- it wraps instead, which is what wrapping is
#  for.  And the budget is in CHARACTERS throughout: `width` is a display width
#  and doubles as the column's relative width, so a character is the natural
#  unit and the gutters can be counted in it too.


# ── The page's width, in characters ──────────────────────────────────────────

# Writable width of a page spec, in twips: the sheet minus its side margins.
.listing_writable_twips <- function(page) {
  page <- page %||% list()
  if (!is.list(page)) {
    stop("`page` must be an rtf_page() object or a list of page settings.",
         call. = FALSE)
  }
  geo <- .resolve_page_geometry(page)
  ml <- page$margin_left_in  %||% .opt("rtfreporter.page.margin_left_in")
  mr <- page$margin_right_in %||% .opt("rtfreporter.page.margin_right_in")
  w  <- geo$width_twips - (as.numeric(ml) + as.numeric(mr)) * 1440
  if (!is.finite(w) || w <= 0) {
    stop("The page's margins leave no writable width.", call. = FALSE)
  }
  as.integer(round(w))
}

# How many characters of the listing's font fit across that width.
.listing_total_width <- function(page, font, size_half_points) {
  char_in <- text_width_in("0", font = font,
                           size_half_points = size_half_points)
  if (!is.finite(char_in) || char_in <= 0) {
    stop("Could not measure the width of one character in font \"", font,
         "\".", call. = FALSE)
  }
  as.integer(floor(.listing_writable_twips(page) / (char_in * 1440)))
}


#' Propose the listing's column widths from the page and the data
#'
#' Fills in the `width` of every [listing_col()] that does not set one, by
#' measuring what the column demands and fitting the demands into the width the
#' page actually leaves.  The result is the same [listing_spec()], ready to
#' hand to [build_listing()] or `as_rtftables(listing = )` -- or to
#' [listing_code()], which prints it as source to paste and tune.
#'
#' @section How a width is chosen:
#'
#' The page decides the budget.  The sheet's writable width (paper and
#' orientation minus the side margins) is divided by the width of one character
#' in `font` at `size_half_points`, which gives the number of characters the
#' listing has to spend.  The gutter columns are subtracted first -- they are
#' printed too -- and so is every `width` you set yourself, because those are
#' decisions, not proposals.
#'
#' Each remaining column's **demand** is the `probs` quantile of the display
#' widths of its composed cells, floored by `min_width` and by the widest token
#' its header cannot break.  A quantile rather than the maximum: one unusually
#' long value should wrap, not push every other column narrow.  And the widest
#' unbreakable token rather than the header's length: a header wraps, so a long
#' label should not claim a column the data does not need.
#'
#' The demands are then scaled to the budget.  Where scaling would take a
#' column below the width its header needs, the column is raised back to it and
#' the characters are taken from the columns that have room to spare -- **a
#' header is never cut mid-word**.  Only if the headers alone cannot fit the
#' page does that guarantee give way.
#'
#' Where the data carries no `label` attributes, `labels` supplies the words,
#' and the measurement uses those.
#'
#' Widths are display widths, so a full-width (CJK) glyph counts as two
#' throughout.
#'
#' @param data The source data, as passed to [build_listing()].
#' @param spec A [listing_spec()].
#' @param page Page settings as an [rtf_page()] object (or a plain list of the
#'   same fields) -- the paper size, orientation and margins the listing will
#'   be rendered on.  `NULL` (default) uses rtfreporter's own page defaults.
#'   Ignored when `total_width` is given.
#' @param font,size_half_points The font the listing renders in, used to turn
#'   the page's width into a number of characters.  Defaults match
#'   [auto_col_widths()].
#' @param total_width Character budget for the whole listing, gutters included.
#'   `NULL` (default) computes it from `page`, `font` and `size_half_points`.
#'   Give it directly when you know the budget and would rather not describe
#'   the page.
#' @param labels Named character vector giving the words for the **source**
#'   variables, e.g. `c(USUBJID = "Unique Subject ID", AGE = "Age")`.  Used
#'   for data that carries no `label` attributes -- a CSV, a frame built in
#'   the program, a `subset()` that dropped them.  A column's header is still
#'   derived from these: the labels of its source variables are joined with
#'   its separator and wrapped to the width just fitted, so where the lines
#'   break is still worked out for you.  Precedence: `listing_col(label = )`,
#'   then `labels`, then the variable's `label` attribute, then its name.  A
#'   define/spec extract converts directly:
#'   `setNames(spec$label, spec$variable)`.
#' @param min_width Integer.  The narrowest a fitted column may be (default
#'   `6`).
#' @param probs Quantile of a column's cell widths taken as its demand
#'   (default `0.9`).
#'
#' @return The `listing_spec`, with a `width` on every column.  It carries the
#'   measurement as an attribute, which `print()` shows.
#'
#' @seealso [listing_code()], which turns the result into pasteable source;
#'   [listing_col()] for setting a width yourself; [auto_col_widths()], the
#'   same idea for a table's columns in twips.
#'
#' @examples
#' adsl <- data.frame(
#'   USUBJID = c("01-701-1015", "01-701-1023"),
#'   HIST    = c("ADENOCARCINOMA", "SQUAMOUS CELL CARCINOMA OF THE LUNG"),
#'   BRCA    = c("BRCA1", NA),
#'   STAGE   = c("IIIB", "IV"),
#'   stringsAsFactors = FALSE
#' )
#'
#' spec <- listing_spec(list(
#'   listing_col("USUBJID"),
#'   listing_col(c("HIST", "BRCA")),
#'   listing_col("STAGE")
#' ))
#'
#' # Landscape A4, half-inch margins, 8pt Courier.
#' fitted <- fit_listing_widths(
#'   adsl, spec,
#'   page = rtf_page(paper_size = "A4", orientation = "landscape",
#'                   margin_left_in = 0.5, margin_right_in = 0.5),
#'   size_half_points = 16L
#' )
#' fitted
#'
#' # A width you set yourself is kept, and the rest fit around it.
#' spec2 <- listing_spec(list(
#'   listing_col("USUBJID", width = 12),
#'   listing_col(c("HIST", "BRCA")),
#'   listing_col("STAGE")
#' ))
#' fit_listing_widths(adsl, spec2, total_width = 60)
#'
#' @export
fit_listing_widths <- function(data, spec,
                               page             = NULL,
                               font             = "courier_new",
                               size_half_points = 18L,
                               total_width      = NULL,
                               labels           = NULL,
                               min_width        = 6L,
                               probs            = 0.9) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame or tibble; got '",
         paste(class(data), collapse = "/"), "'.", call. = FALSE)
  }
  if (!inherits(spec, "rtf_listing_spec")) {
    stop("`spec` must be a listing_spec(); got '",
         paste(class(spec), collapse = "/"), "'.", call. = FALSE)
  }
  if (!is.null(attr(data, "rtf_listing", exact = TRUE))) {
    stop("`data` has already been through build_listing(); fit the widths on ",
         "the source data instead.", call. = FALSE)
  }
  if (!is.null(labels)) {
    labels <- unlist(labels, use.names = TRUE)
    if (!is.character(labels) || !length(labels) ||
        is.null(names(labels)) || anyNA(names(labels)) ||
        !all(nzchar(names(labels)))) {
      stop("`labels` must be a named character vector mapping SOURCE column ",
           "names to labels, e.g. c(USUBJID = \"Unique Subject ID\").",
           call. = FALSE)
    }
    labels[is.na(labels)] <- ""
  }
  min_width <- as.integer(min_width)
  if (length(min_width) != 1L || is.na(min_width) || min_width < 1L) {
    stop("`min_width` must be a single positive integer.", call. = FALSE)
  }
  if (length(probs) != 1L || !is.numeric(probs) || is.na(probs) ||
      probs < 0 || probs > 1) {
    stop("`probs` must be a single number in [0, 1].", call. = FALSE)
  }
  if (is.null(total_width)) {
    total_width <- .listing_total_width(page, font, size_half_points)
  }
  total_width <- as.integer(total_width)
  if (length(total_width) != 1L || is.na(total_width) || total_width < 1L) {
    stop("`total_width` must be a single positive number of characters.",
         call. = FALSE)
  }

  k     <- length(spec$cols)
  fixed <- vapply(spec$cols, function(cl) !is.null(cl$width), logical(1L))

  # The gutters print, so they come out of the budget before anything else.
  n_gut  <- if (isTRUE(spec$spacer)) max(k - 1L, 0L) else 0L
  gutter <- n_gut * as.numeric(spec$spacer_rel_width)
  budget <- total_width - gutter - sum(vapply(spec$cols[fixed],
                                              function(cl) as.numeric(cl$width),
                                              numeric(1L)))
  if (any(!fixed) && budget < sum(!fixed) * min_width) {
    stop(sprintf(paste0("A total width of %d character(s) leaves %g for the ",
                        "%d column(s) to be fitted, which cannot each be %d ",
                        "wide.  Widen the page, shrink the font, or set the ",
                        "widths yourself."),
                 total_width, budget, sum(!fixed), min_width), call. = FALSE)
  }

  # What each column asks for, whether or not it is being fitted -- the fixed
  # ones are reported too, so print() can show a width that is under-set.
  floor_hdr <- numeric(k)
  demand <- vapply(seq_len(k), function(j) {
    cl   <- spec$cols[[j]]
    sepj <- if (is.null(cl$sep)) spec$sep else cl$sep
    lay  <- if (is.null(cl$layout)) spec$layout else cl$layout
    cells <- if (nrow(data)) .listing_combine(data, cl, sepj) else character(0)
    cell_w <- if (length(cells)) {
      as.numeric(stats::quantile(.listing_disp_width(cells), probs = probs,
                                 names = FALSE, type = 7))
    } else 0
    # The header's floor is the widest token it cannot break, not its full
    # length: a header wraps, so a long label should not claim a column the
    # data does not need.
    lab   <- .listing_resolve_label(data, cl, sepj, lay, labels)
    hdr_w <- .listing_min_wrap_width(lab, sepj)
    floor_hdr[j] <<- hdr_w
    max(cell_w, hdr_w, min_width)
  }, numeric(1L))

  out <- vapply(seq_len(k), function(j) {
    if (fixed[j]) as.numeric(spec$cols[[j]]$width) else NA_real_
  }, numeric(1L))

  if (any(!fixed)) {
    free   <- demand[!fixed]
    scaled <- free * (budget / sum(free))
    fit    <- pmax(min_width, as.integer(round(scaled)))

    # A header must not be cut mid-word.  Scaling to the budget can take a
    # column below the widest token its header cannot break -- and then the
    # header is hard-split, which is never acceptable in a deliverable.  So
    # raise any column that fell under its floor, and pay for it out of the
    # columns that have slack above theirs, in proportion to that slack.
    floors <- pmax(min_width, floor_hdr[!fixed])
    if (sum(floors) <= budget) {
      fit  <- pmax(fit, floors)
      over <- sum(fit) - as.integer(round(budget))
      while (over > 0L) {
        slack <- fit - floors
        if (all(slack <= 0L)) break
        take <- pmin(slack, pmax(1L, as.integer(ceiling(
          over * slack / sum(slack)))))
        take[slack <= 0L] <- 0L
        if (sum(take) > over) {
          # Trim the last few characters one column at a time, widest first.
          idx <- order(slack, decreasing = TRUE)
          take[] <- 0L
          left <- over
          for (i in idx) {
            if (left <= 0L) break
            t <- min(left, slack[i])
            take[i] <- t
            left <- left - t
          }
        }
        fit  <- fit - take
        over <- sum(fit) - as.integer(round(budget))
      }
    }

    # Rounding drift lands on the widest fitted column, so the total is exact.
    drift <- as.integer(round(budget)) - sum(fit)
    if (drift != 0L) {
      j <- which.max(fit)
      fit[j] <- max(min_width, fit[j] + drift)
    }
    out[!fixed] <- fit
  }

  # Write the estimate DOWN, in full (#375).  A template you cannot see is a
  # template you cannot correct, so alongside the fitted `width` each column
  # gets its `rel_width` and its `label` -- resolved from the lookup or the
  # data and wrapped to the width just chosen, exactly as build_listing()
  # would have.  Only where the author has not set them: an explicit value is
  # still never touched.  Freezing the header here is a no-op for rendering
  # (build_listing() would wrap it to the same width); what it changes is that
  # the header becomes something you can edit.
  for (j in seq_len(k)) {
    cl <- spec$cols[[j]]
    cl$width <- as.integer(out[j])
    if (is.null(cl$rel_width)) cl$rel_width <- as.numeric(cl$width)
    if (is.null(cl$label)) {
      sepj <- if (is.null(cl$sep)) spec$sep else cl$sep
      lay  <- if (is.null(cl$layout)) spec$layout else cl$layout
      cl$label <- .listing_resolve_label(data, cl, sepj, lay, labels)
    }
    spec$cols[[j]] <- cl
  }

  attr(spec, "rtf_listing_fit") <- list(
    total_width = total_width,
    gutter      = gutter,
    demand      = stats::setNames(round(demand, 1), vapply(spec$cols,
                                                           function(cl) cl$name,
                                                           character(1L))),
    fixed       = fixed)
  spec
}


# ── Pasteable source ─────────────────────────────────────────────────────────

# One argument, rendered as `name = value` -- or NULL when it is the default
# and does not need saying.
.listing_arg <- function(name, value, default = NULL) {
  if (is.null(value)) return(NULL)
  if (!is.null(default) && identical(value, default)) return(NULL)
  rendered <- if (is.character(value)) {
    if (length(value) > 1L) {
      paste0("c(", paste(encodeString(value, quote = "\""), collapse = ", "),
             ")")
    } else {
      encodeString(value, quote = "\"")
    }
  } else if (is.logical(value)) {
    if (isTRUE(value)) "TRUE" else "FALSE"
  } else {
    format(value)
  }
  paste0(name, " = ", rendered)
}

#' Print a listing spec as the code that would build it
#'
#' Turns a [listing_spec()] into the source you would have written by hand, so
#' a spec that came out of [fit_listing_widths()] can be pasted into the
#' program and tuned there.  The measurement is a starting point; the code is
#' where the decisions get made and reviewed.
#'
#' Only what differs from the listing's own defaults is written out, so the
#' result reads like something a person wrote rather than a dump of every
#' setting.
#'
#' A spec straight from [fit_listing_widths()] therefore comes out in full --
#' `width`, `rel_width` and `label` on every column -- because the fit wrote
#' all three down.  That is the point: it is a template to edit.
#'
#' @param spec A [listing_spec()].
#' @param name Name to assign the spec to, e.g. `"listing"` produces
#'   `listing <- listing_spec(...)`.  `NULL` (default) writes the call alone.
#' @param indent Number of spaces the `listing_col()` calls are indented by
#'   (default `2`).
#'
#' @return A character vector of source lines, one per line of code, with class
#'   `rtf_listing_code` -- printing it shows the code ready to copy.
#'
#' @seealso [fit_listing_widths()], which proposes the widths this writes out.
#'
#' @examples
#' spec <- listing_spec(list(
#'   listing_col("USUBJID", width = 15, label = "Unique\nSubject ID"),
#'   listing_col(c("AGE", "SEX"), width = 12, layout = "flow"),
#'   listing_col("STAGE", width = 9)
#' ))
#'
#' listing_code(spec)
#' listing_code(spec, name = "listing")
#'
#' # The usual round trip: measure, print, paste, tune.
#' code <- listing_code(spec, name = "listing")
#' writeLines(code)
#'
#' @export
listing_code <- function(spec, name = NULL, indent = 2L) {
  if (!inherits(spec, "rtf_listing_spec")) {
    stop("`spec` must be a listing_spec(); got '",
         paste(class(spec), collapse = "/"), "'.", call. = FALSE)
  }
  if (!is.null(name) && (!is.character(name) || length(name) != 1L ||
                         is.na(name) || !nzchar(name))) {
    stop("`name` must be a single non-empty string, or NULL.", call. = FALSE)
  }
  indent <- as.integer(indent)
  if (length(indent) != 1L || is.na(indent) || indent < 0L) {
    stop("`indent` must be a single non-negative integer.", call. = FALSE)
  }
  pad  <- strrep(" ", indent)
  pad2 <- strrep(" ", indent * 2L)
  tpl  <- .listing_template(spec$type)

  col_lines <- unlist(lapply(seq_along(spec$cols), function(j) {
    cl   <- spec$cols[[j]]
    vars <- if (length(cl$vars) > 1L) {
      paste0("c(", paste(encodeString(cl$vars, quote = "\""),
                         collapse = ", "), ")")
    } else {
      encodeString(cl$vars, quote = "\"")
    }
    args <- c(
      .listing_arg("width", cl$width),
      .listing_arg("sep", cl$sep),
      .listing_arg("layout", cl$layout),
      # The output column's name is only worth writing when it is not the one
      # `vars` would have produced.
      if (!identical(cl$name, cl$vars[[1L]])) {
        .listing_arg("name", cl$name)
      },
      .listing_arg("rel_width", cl$rel_width),
      .listing_arg("align", cl$align),
      if (isTRUE(cl$collapse_repeats)) "collapse_repeats = TRUE",
      .listing_arg("label", cl$label))
    head_line <- paste0(pad, "listing_col(", vars)
    if (!length(args)) {
      return(paste0(head_line, ")", if (j < length(spec$cols)) "," else ""))
    }
    # The label is usually long; give it a line of its own so the call stays
    # readable at 80 columns.
    lab_last <- length(args) > 1L && startsWith(args[[length(args)]], "label = ")
    first <- if (lab_last) args[-length(args)] else args
    lines <- paste0(head_line, ", ", paste(first, collapse = ", "))
    if (lab_last) {
      lines <- c(paste0(lines, ","), paste0(pad2, args[[length(args)]]))
    }
    n <- length(lines)
    lines[n] <- paste0(lines[n], ")", if (j < length(spec$cols)) "," else "")
    lines
  }), use.names = FALSE)

  spec_args <- c(
    .listing_arg("type", spec$type, "multiline"),
    .listing_arg("sep", spec$sep, tpl$sep),
    .listing_arg("spacer", spec$spacer, tpl$spacer),
    .listing_arg("spacer_rel_width", spec$spacer_rel_width,
                 tpl$spacer_rel_width),
    .listing_arg("blank_row", spec$blank_row, tpl$blank_row),
    .listing_arg("blank_row_first", spec$blank_row_first, tpl$blank_row_first),
    .listing_arg("align", spec$align, tpl$align),
    .listing_arg("layout", spec$layout, tpl$layout),
    if (is.null(spec$record_col)) {
      "record = FALSE"
    } else if (!identical(spec$record_col, ".rtf_record")) {
      .listing_arg("record", spec$record_col)
    })

  head <- paste0(if (is.null(name)) "" else paste0(name, " <- "),
                 "listing_spec(")

  close <- if (length(spec_args)) {
    paste0("), ", paste(spec_args, collapse = ", "), ")")
  } else {
    "))"
  }
  structure(c(paste0(head, "list("), col_lines, close),
            class = "rtf_listing_code")
}

#' @export
print.rtf_listing_code <- function(x, ...) {
  cat(unclass(x), sep = "\n")
  invisible(x)
}

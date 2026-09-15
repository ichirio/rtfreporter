# ============================================================================
#  set_col_header(values = ) -- per-page values in a shared header
# ============================================================================
#
#  A column header is usually one shape with a few values that change page to
#  page -- "(N=120)" for one period, "(N=118)" for the next.  Writing it per
#  page means rebuilding the whole header in a loop and hoping the labels line
#  up with the columns.  Instead the header is written ONCE with `{token}`
#  where a value belongs, and the values arrive as a table keyed by the page:
#
#      hdr  : col_cell(c(2, 9), "Placebo\n(N={n_placebo})\nn(%)")
#      vals : group | n_placebo | n_trt | n_total      (one row per page key)
#
#  The key is the page's own -- its group, its `page_by` value, or its name --
#  never its position, so the table cannot be matched to the wrong page.
# ============================================================================

# Tokens the RENDERER fills, long after this: leave them alone.
.RENDER_TOKENS <- c("PAGE", "TOTAL_PAGES", "DATE", "BOOK_PAGE",
                    "AUTO_PAGE", "AUTO_TOTAL_PAGES", "SECTION_PAGES")

# `{{` / `}}` are literal braces.  They are parked on two control characters
# while the real tokens are replaced, then put back.
.BRACE_OPEN  <- "\u0001"
.BRACE_CLOSE <- "\u0002"

.TOKEN_RX <- "[{]([A-Za-z._][A-Za-z0-9._]*)[}]"

# The token names one string asks for.
.text_tokens <- function(txt) {
  if (!is.character(txt) || !length(txt)) return(character(0))
  s <- gsub("{{", .BRACE_OPEN, txt, fixed = TRUE)
  s <- gsub("}}", .BRACE_CLOSE, s, fixed = TRUE)
  m <- regmatches(s, gregexpr(.TOKEN_RX, s))
  unique(substr(unlist(m), 2L, nchar(unlist(m)) - 1L))
}

# Fill one string.  `vals` is a one-row list of values, `where` names the place
# for the error message.
.fill_text_tokens <- function(txt, vals, where) {
  if (!is.character(txt) || !length(txt)) return(txt)
  vapply(txt, function(s) {
    if (is.na(s) || !grepl("{", s, fixed = TRUE)) return(s)
    s <- gsub("{{", .BRACE_OPEN, s, fixed = TRUE)
    s <- gsub("}}", .BRACE_CLOSE, s, fixed = TRUE)
    for (nm in names(vals)) {
      s <- gsub(paste0("{", nm, "}"), as.character(vals[[nm]]), s, fixed = TRUE)
    }
    left <- .text_tokens(s)
    left <- setdiff(left, .RENDER_TOKENS)
    if (length(left)) {
      stop(sprintf(paste0(
        "%s: no value for %s.
  Supply it in `values`, or write `{{` for a ",
        "literal brace.
  values has: %s"),
        where, paste0("`{", left, "}`", collapse = ", "),
        if (length(vals)) paste(names(vals), collapse = ", ") else "(nothing)"),
        call. = FALSE)
    }
    s <- gsub(.BRACE_OPEN, "{", s, fixed = TRUE)
    gsub(.BRACE_CLOSE, "}", s, fixed = TRUE)
  }, character(1L), USE.NAMES = FALSE)
}

# Fill every label a header spec carries: a character row, a col_cell()'s
# label, an rtf_col_header's rows -- whatever shape the caller wrote it in.
.fill_header_tokens <- function(spec, vals, where) {
  if (is.null(spec)) return(spec)
  if (inherits(spec, "rtf_col_cell")) {
    spec$label <- .fill_text_tokens(spec$label %||% "", vals, where)
    return(spec)
  }
  if (is.character(spec)) return(.fill_text_tokens(spec, vals, where))
  if (is.list(spec)) {
    cls <- class(spec)
    out <- lapply(spec, .fill_header_tokens, vals = vals, where = where)
    class(out) <- cls
    return(out)
  }
  spec
}

# ---------------------------------------------------------------------------
#  Page keys: what a `values` row is matched against
# ---------------------------------------------------------------------------
#  "group"  the value a by_value split made the page for -- or, with no group
#           axis, the page's name without its "...n" tail (the heading);
#  "rows"   the `page_by` value;
#  "name"   the page's name, "...n" included, which is always unique.
.page_key <- function(page, nm, axis) {
  d    <- if (!is.null(page$data)) page$data else page$data_list[[1L]]
  meta <- attr(d, "rtf_paginate_meta", exact = TRUE)
  switch(axis,
    group = {
      g <- if (is.list(meta)) meta$page_group else NULL
      if (!is.null(g) && nzchar(g)) as.character(g)
      else .page_name_base(nm %||% NA_character_)
    },
    rows = {
      b <- if (is.list(meta)) meta$page_by else NULL
      if (is.null(b) || !nzchar(b)) NA_character_ else as.character(b)
    },
    name = nm %||% NA_character_,
    stop(sprintf("`by`: \"%s\" is not a page axis (group, rows, name).", axis),
         call. = FALSE))
}

# Did any page actually have a group of its own?  Used to tell the caller that
# `by = "group"` fell back to the page name.
.has_group_axis <- function(pages) {
  any(vapply(pages, function(p) {
    d <- if (!is.null(p$data)) p$data else p$data_list[[1L]]
    m <- attr(d, "rtf_paginate_meta", exact = TRUE)
    is.list(m) && length(m$page_group) > 0L && nzchar(m$page_group)
  }, logical(1L)))
}

# Match each page to one row of `values`.  Returns the row index per page.
.match_value_rows <- function(pages, values, by) {
  if (!is.data.frame(values)) {
    stop("`values` must be a data.frame: one row per page key, one column ",
         "per token.", call. = FALSE)
  }
  by <- if (is.null(by)) "group" else as.character(by)
  missing_cols <- setdiff(by, names(values))
  if (length(missing_cols)) {
    stop(sprintf(paste0("`values` has no key column %s.  Name the key column ",
                        "after the axis it matches: %s."),
                 paste0("`", missing_cols, "`", collapse = ", "),
                 paste0("`", c("group", "rows", "name"), "`", collapse = ", ")),
         call. = FALSE)
    }

  if (identical(by, "group") && !.has_group_axis(pages)) {
    message("set_col_header(values = ): no group axis, so `by = \"group\"` ",
            "matches on the page name instead.")
  }

  nms  <- names(pages) %||% rep(NA_character_, length(pages))
  keys <- vapply(seq_along(pages), function(i) {
    paste(vapply(by, function(a) .page_key(pages[[i]], nms[i], a),
                 character(1L)), collapse = "
")
  }, character(1L))

  want <- do.call(paste, c(lapply(by, function(a) as.character(values[[a]])),
                           list(sep = "
")))
  row <- match(keys, want)

  if (anyNA(row)) {
    i <- which(is.na(row))[1L]
    stop(sprintf(paste0(
      "`values` has no row for page %d (%s).
  looked for %s = \"%s\"
",
      "  values has: %s"),
      i, if (is.na(nms[i])) "unnamed" else paste0("\"", nms[i], "\""),
      paste(by, collapse = " + "), gsub("
", " + ", keys[i], fixed = TRUE),
      paste(unique(gsub("
", " + ", want, fixed = TRUE)), collapse = " | ")),
      call. = FALSE)
  }
  unused <- setdiff(seq_len(nrow(values)), row)
  if (length(unused)) {
    stop(sprintf(paste0(
      "`values` row%s %s matched no page: %s.
  Every row must be used -- a ",
      "row left over is usually a label that does not match the data."),
      if (length(unused) > 1L) "s" else "",
      paste(unused, collapse = ", "),
      paste(unique(gsub("
", " + ", want[unused], fixed = TRUE)),
            collapse = " | ")), call. = FALSE)
  }
  list(row = row, by = by)
}

# One page's values, as a plain named list (the key columns dropped).
.value_row <- function(values, i, by) {
  keep <- setdiff(names(values), by)
  as.list(values[i, keep, drop = FALSE])
}

#' Show the column header of every page, cell by cell
#'
#' @description
#' One row per header cell of every page: which page it is on, the keys that
#' page was cut at, which columns the cell covers, and the text it ends up
#' with. It is the check for a header built from a template and a `values`
#' table -- the mapping is visible at a glance, and assertable in a test.
#'
#' ```r
#' pages |> set_col_header(hdr, values = vals) |> header_map()
#' #>  page name              group   rows row cell from to text
#' #>  1    Period 1...1      Period 1  1    1    2    2    9 "Placebo\n(N=120)..."
#' ```
#'
#' @param x An [rtftable()] or a list of them (pages).
#'
#' @return A `data.frame` with one row per header cell: `page`, `name`,
#'   `group`, `rows`, `row` (which header row), `cell`, `from`, `to`, `text`.
#'   A page with no column header contributes nothing.
#'
#' @seealso [set_col_header()], whose `values` argument this checks.
#'
#' @examples
#' df <- data.frame(Parameter = c("Mean", "SD"), A = c("1", "2"),
#'                  B = c("3", "4"), stringsAsFactors = FALSE)
#' tbl <- rtftable(df) |> set_col_header(c("Parameter", "Drug", "Placebo"))
#' header_map(tbl)
#' @export
header_map <- function(x) {
  pages <- if (inherits(x, "rtftable")) list(x) else x
  nms   <- names(pages) %||% rep(NA_character_, length(pages))
  out   <- list()

  add <- function(i, r, k, from, to, text) {
    out[[length(out) + 1L]] <<- data.frame(
      page  = i,
      name  = nms[i],
      group = .page_key(pages[[i]], nms[i], "group"),
      rows  = .page_key(pages[[i]], nms[i], "rows"),
      row   = r, cell = k, from = from, to = to, text = text,
      stringsAsFactors = FALSE)
  }

  for (i in seq_along(pages)) {
    h <- pages[[i]]$col_header
    if (is.null(h)) next
    if (is.character(h)) h <- list(h)
    for (r in seq_along(h)) {
      row <- h[[r]]
      if (is.character(row)) {
        for (k in seq_along(row)) add(i, r, k, k, k, row[[k]])
      } else if (is.list(row)) {
        for (k in seq_along(row)) {
          cell <- row[[k]]
          add(i, r, k,
              as.integer(cell$from %||% NA_integer_),
              as.integer(cell$to   %||% NA_integer_),
              as.character(cell$label %||% ""))
        }
      }
    }
  }
  if (length(out) == 0L) {
    return(data.frame(page = integer(0), name = character(0),
                      group = character(0), rows = character(0),
                      row = integer(0), cell = integer(0),
                      from = integer(0), to = integer(0),
                      text = character(0), stringsAsFactors = FALSE))
  }
  do.call(rbind, out)
}

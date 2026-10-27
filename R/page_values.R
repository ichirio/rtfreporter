# ---------------------------------------------------------------------------
#  A page's values by column: the {<column>} tokens
# ---------------------------------------------------------------------------
#  A table cut into pages on a column -- as_rtftables(page_by = ), a value
#  split on a named group column (split = "by_value", group_col = ), a
#  plan's plan_paginate_group() -- knows, for each page, the column and the
#  value it was cut on.  The page's pagination metadata keeps them as
#  `page_values`, a named character vector (c(PARAM = "ALT", SEX = "F")),
#  beside `page_by`, the joined value that names the page.  Two sources say
#  it themselves: an rtables page from paginate_table() (its page_titles,
#  R/rtables_adapter.R) and a gt_group member a tfrmt page_plan wrote its
#  note on (R/gt_adapter.R).
#
#  A title, footnote, header or footer band then writes `{PARAM}` and gets
#  that page's value when the file is written, as it gets {PROGRAM}:
#
#    * a title or footnote takes the values of its own content, so each
#      table on a page of several names its own;
#    * a header / footer band is the page's: each column takes the first
#      table on the page that has a value for it (the rule rtf_layout()
#      uses for an auto_section heading); a later table with a different
#      value gets a warning.  A band that uses one is written for each page
#      (an RTF section a page), as a static {PAGE} is.
#
#  A `{NAME}` the page has no value for is left as written, as an unknown
#  token always has been; so is one that is rtfreporter's own ({PAGE},
#  {PROGRAM}, ...), and a token of one's own (rtf_document(tokens = )) of
#  the same name wins, being filled first.

# Two sets of page values as one, `a`'s value winning for a column in both.
# NULL when there are none.
.merge_page_values <- function(a, b) {
  a <- .as_page_values(a)
  b <- .as_page_values(b)
  out <- c(a, b[setdiff(names(b), names(a))])
  if (length(out)) out else NULL
}

.as_page_values <- function(x) {
  if (is.null(x) || !length(x) || is.null(names(x))) return(NULL)
  x <- stats::setNames(as.character(unlist(x, use.names = FALSE)), names(x))
  x <- x[!is.na(names(x)) & nzchar(names(x)) & !is.na(x)]
  if (length(x)) x else NULL
}

# The value a value split's page has in its group column `v`, named by the
# column: the page's label, when the column's first value on the page says
# the same (a group with no value of its own is named "group_<k>", no value).
.column_page_value <- function(v, col, label) {
  v <- as.character(v)
  v <- trimws(v[!is.na(v) & nzchar(trimws(v))])
  if (!length(v) || is.null(label) || is.na(label) || !identical(v[1L], label)) {
    return(NULL)
  }
  stats::setNames(label, col)
}

# A content's page values: those of an rtftable's pagination metadata
# (where as_rtftables() put them); none for anything else.
.content_page_values <- function(x) {
  if (!inherits(x, "rtftable")) return(NULL)
  d <- x$data %||% x$data_list[[1L]]
  m <- attr(d, "rtf_paginate_meta", exact = TRUE) %||%
    attr(x, "rtf_paginate_meta", exact = TRUE)
  if (!is.list(m)) return(NULL)
  .as_page_values(m[["page_values"]])
}

# A segment's values: those rtf_layout() gave it, else its content's (a
# report built by hand has no layout).
.segment_values <- function(s) {
  .as_page_values(s[["values"]]) %||% .content_page_values(s[["content"]])
}

# A page's values, from its segments: each column that of the first segment
# with a value for it.  `used`: the columns a band names; a later segment
# with another value for one of them is said (`page`: its number).
.page_values_of <- function(segments, used = character(0), page = NA_integer_) {
  out <- NULL
  for (s in segments) {
    v <- .segment_values(s)
    if (!length(v)) next
    clash <- intersect(intersect(names(v), names(out)), used)
    clash <- clash[v[clash] != out[clash]]
    for (nm in clash) {
      warning(sprintf(paste0("Page %d: {%s} is '%s' in its header / footer -- the ",
                             "first table on the page with a value for it; a later ",
                             "table there has '%s'."),
                      page, nm, out[[nm]], v[[nm]]), call. = FALSE)
    }
    out <- .merge_page_values(out, v)
  }
  out
}

# The columns of `cols` a header / footer band names as `{<column>}`.
.hf_column_tokens <- function(hf, cols) {
  cols <- setdiff(cols, .RENDER_TOKENS)
  if (is.null(hf) || !length(hf$rows) || !length(cols)) return(character(0))
  txt <- unlist(lapply(hf$rows, function(row) {
    as.character(unlist(if (is.list(row) && !is.null(row$columns)) row$columns else row))
  }), use.names = FALSE)
  txt <- txt[!is.na(txt)]
  cols[vapply(cols, function(nm) any(grepl(paste0("{", nm, "}"), txt, fixed = TRUE)),
              logical(1L))]
}

# Fill the {<column>} tokens in text that is already RTF-escaped (a token
# reads `\{PARAM\}`) from `values`; rtfreporter's own tokens are never
# columns' (their meaning does not change with a column's name).
.substitute_column_tokens <- function(out, values = .run_ctx$page_values) {
  for (nm in setdiff(names(values), .RENDER_TOKENS)) {
    tok <- paste0("\\{", .rtf_escape(nm), "\\}")
    if (grepl(tok, out, fixed = TRUE)) {
      out <- .replace_token(out, tok, .rtf_escape(values[[nm]]))
    }
  }
  out
}

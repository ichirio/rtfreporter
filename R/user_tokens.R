# ============================================================================
#  Tokens of one's own: {STUDY}, {CUTOFF} ...
# ----------------------------------------------------------------------------
#  A document names them (rtf_document(tokens = list(STUDY = "ABC-123"))),
#  a session too (options(rtfreporter.tokens = list(...))); the document's
#  value wins.  generate_rtfreport() fills them in headers, footers, titles
#  and footnotes, as it fills {PROGRAM} (a column header's values come from
#  set_col_header(values = )).  A name is upper
#  case -- a letter, then letters, digits or _ -- and never one of
#  rtfreporter's own tokens.
# ============================================================================

.USER_TOKEN_RX <- "^[A-Z][A-Z0-9_]*$"

# The tokens of one's own as a named character vector (NULL: none).
.check_user_tokens <- function(x, where = "`tokens`") {
  if (is.null(x) || !length(x)) return(NULL)
  if (!is.list(x) && !is.atomic(x)) {
    stop(where, " is a named list of values, e.g. list(STUDY = \"ABC-123\").",
         call. = FALSE)
  }
  nm <- names(x)
  if (is.null(nm) || any(is.na(nm) | !nzchar(nm))) {
    stop(where, ": every token has a name, e.g. list(STUDY = \"ABC-123\").",
         call. = FALSE)
  }
  bad <- nm[!grepl(.USER_TOKEN_RX, nm)]
  if (length(bad)) {
    stop(where, ": a token's name is upper case -- a letter, then letters, ",
         "digits or _ (STUDY, DATA_CUTOFF): not ",
         paste0("`", bad, "`", collapse = ", "), ".", call. = FALSE)
  }
  own <- intersect(nm, .RENDER_TOKENS)
  if (length(own)) {
    stop(where, ": ", paste0("{", own, "}", collapse = ", "),
         " is rtfreporter's own token; give yours another name.",
         call. = FALSE)
  }
  if (anyDuplicated(nm)) {
    stop(where, ": ", paste0("`", unique(nm[duplicated(nm)]), "`",
                             collapse = ", "), " is named twice.", call. = FALSE)
  }
  vals <- vapply(seq_along(x), function(i) {
    v <- x[[i]]
    if (length(v) != 1L || is.na(v) || !(is.character(v) || is.numeric(v) ||
                                           is.logical(v))) {
      stop(where, ": `", nm[i], "` is one value (a string or a number).",
           call. = FALSE)
    }
    as.character(v)
  }, "")
  stats::setNames(vals, nm)
}

# The session's tokens and the document's, the document's winning.
.user_tokens <- function(doc_tokens = NULL) {
  opt <- .check_user_tokens(getOption("rtfreporter.tokens"),
                            "options(rtfreporter.tokens)")
  doc <- .check_user_tokens(doc_tokens)
  out <- opt
  out[names(doc)] <- doc
  if (!length(out)) NULL else out
}

# Fill them in text that is already RTF-escaped (a token reads `\{STUDY\}`).
.substitute_user_tokens <- function(out, tokens = .run_ctx$tokens) {
  for (nm in names(tokens)) {
    tok <- paste0("\\{", nm, "\\}")
    if (grepl(tok, out, fixed = TRUE)) {
      out <- .replace_token(out, tok, .rtf_escape(tokens[[nm]]))
    }
  }
  out
}

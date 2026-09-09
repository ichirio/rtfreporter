# GENERATED FILE -- do not edit.
#
# The "multiline" wrapping rule, copied verbatim from R/listing.R by
# data-raw/gen_listing_wrap_template.R.  listing_wrap_code() renames these
# functions and hands them out; the suite checks this file still matches.

.listing_disp_width <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  w <- suppressWarnings(nchar(x, type = "width", allowNA = TRUE))
  bad <- is.na(w)
  if (any(bad)) w[bad] <- nchar(x[bad], type = "chars")
  as.integer(w)
}

.listing_take <- function(x, width) {
  n <- nchar(x, type = "chars")
  if (n == 0L) return("")
  best <- 1L
  for (i in seq_len(n)) {
    if (.listing_disp_width(substr(x, 1L, i)) <= width) best <- i else break
  }
  substr(x, 1L, best)
}

.listing_split_after <- function(text, sep) {
  if (is.null(sep) || !nzchar(sep)) return(text)
  # \Q...\E quotes the separator, so a "." or a "|" separator is a literal and
  # the lookbehind stays fixed-width.  (Written this way rather than with an
  # escaping gsub(): backreferences are unreliable in some R builds.)
  parts <- strsplit(text, paste0("(?<=\\Q", sep, "\\E)"), perl = TRUE)[[1L]]
  if (!length(parts)) text else parts
}

.listing_flow <- function(parts, width) {
  out <- character(0L)
  cur <- ""
  for (p in parts) {
    if (!nzchar(cur)) {
      cur <- p
    } else if (.listing_disp_width(trimws(paste0(cur, p))) <= width) {
      cur <- paste0(cur, p)
    } else {
      out <- c(out, cur)
      cur <- p
    }
  }
  if (nzchar(cur)) out <- c(out, cur)
  out
}

.listing_wrap_words <- function(text, width) {
  words <- strsplit(text, "(?<=[ ,-])", perl = TRUE)[[1L]]
  if (!length(words)) words <- text
  out <- character(0L)
  cur <- ""
  for (w in words) {
    # A token wider than the column on its own.  Split it here, before the
    # running line is trimmed to measure it -- trimming would eat the trailing
    # space that separates this word from the next.
    if (.listing_disp_width(trimws(w)) > width) {
      if (nzchar(trimws(cur))) out <- c(out, trimws(cur))
      tok <- sub("^\\s+", "", w)
      while (.listing_disp_width(trimws(tok)) > width) {
        piece <- .listing_take(tok, width)
        out   <- c(out, piece)
        tok   <- substring(tok, nchar(piece, type = "chars") + 1L)
      }
      cur <- tok                       # keeps any trailing separator
      next
    }
    if (!nzchar(cur)) {
      cur <- w
      # Measured WITHOUT trimming the running line, which is what the rule
      # this came from did.  Trimming would let a line end one character
      # wider (the trailing space is invisible) and would silently change
      # every existing listing's line counts; that is a separate decision,
      # not part of fixing #364.
    } else if (.listing_disp_width(cur) + .listing_disp_width(w) <= width) {
      cur <- paste0(cur, w)
    } else {
      out <- c(out, trimws(cur))
      cur <- w
    }
  }
  if (nzchar(trimws(cur))) out <- c(out, trimws(cur))
  out
}

.listing_wrap_sep_word <- function(text, width, sep, layout = "stack") {
  if (is.null(text) || length(text) != 1L || is.na(text)) text <- ""
  text <- as.character(text)
  # A "\n" already in the data is a line break the author asked for; honour it
  # before any width is considered.
  chunks <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  if (!length(chunks)) chunks <- ""
  if (is.null(width) || is.na(width)) {
    # No width, no layout: with nothing to lay the column out against, both
    # `layout`s return the text as it stands.  (ydisctools' rule breaks at
    # every separator here even with no width; that would silently make every
    # existing width-less multi-variable column taller, and nothing asks for
    # it -- `width` is what says how the column is laid out.)
    chunks <- trimws(chunks)
    return(if (all(!nzchar(chunks))) "" else chunks)
  }
  out <- character(0L)
  for (ch in chunks) {
    parts <- .listing_split_after(ch, sep)
    # "flow": refill the pieces first, so a break survives only where the
    # line ran out of room.  "stack" keeps every separator break.
    if (identical(layout, "flow")) parts <- .listing_flow(parts, width)
    for (p in parts) {
      p <- trimws(p)
      if (!nzchar(p)) next
      if (.listing_disp_width(p) <= width) {
        out <- c(out, p)
      } else {
        out <- c(out, .listing_wrap_words(p, width))
      }
    }
  }
  if (!length(out)) "" else out
}

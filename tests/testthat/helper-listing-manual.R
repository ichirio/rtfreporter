# A listing laid out by hand, the way a statistical programmer writes one
# without build_listing(): join several source columns into one printed
# column with "/", break a long cell first at the separators and then at word
# boundaries, give every column of a record as many lines as its tallest
# cell, follow each record with a blank line, and put a narrow empty gutter
# column between the printed ones.
#
# The data are an adverse-event listing made up for these tests.
# test-listing-vs-manual.R checks that build_listing() lays the same data out
# the same way, and pins the places where it deliberately does not.


# ── The hand-written pieces ──────────────────────────────────────────────────

# Join the values of several columns with `sep`, leaving out the missing and
# empty ones: "MILD/Y" when the middle one is missing, "" when all are.
.hand_join <- function(sep, cols) {
  vals <- do.call(cbind, lapply(cols, as.character))
  apply(vals, 1L, function(v) paste(v[!is.na(v) & nzchar(v)], collapse = sep))
}

# The lines of one cell, `width` characters at most: the cell cut after each
# "/" first; a piece still too wide is cut after a space, a comma or a hyphen
# and refilled, word by word, while the line has room.  Characters are
# counted with nchar(), and a word is never cut -- a line can be empty, or
# wider than `width`, where a word does not fit (test-listing-vs-manual.R
# shows both).
.hand_wrap <- function(x, width) {
  pieces <- trimws(regmatches(x, gregexpr("[^/]+/?|/", x))[[1L]])
  lines <- character(0L)
  for (piece in pieces) {
    if (nchar(piece) <= width) {
      lines <- c(lines, piece)
      next
    }
    words <- regmatches(piece, gregexpr("[^[:space:],-]*[[:space:],-]?", piece))[[1L]]
    words <- words[nzchar(words)]
    line <- ""
    i <- 1L
    while (i <= length(words)) {
      if (nchar(line) + nchar(words[i]) <= width) {
        line <- paste0(line, words[i])
      } else {
        lines <- c(lines, trimws(line))
        line <- words[i]
      }
      i <- i + 1L
    }
    if (nzchar(line)) lines <- c(lines, trimws(line))
  }
  lines
}

# The listing: `cols` names each printed column and the source columns it
# joins (in print order), `widths` the printed columns that wrap and their
# width.  A record is as many lines as its tallest wrapped cell, plus one
# blank line; a column that does not wrap prints its value on the first line.
# Gutters "S01", "S02", ... sit between the printed columns, empty (NA).
# `blank_first`: one blank line above the first record.
.manual_listing <- function(data, cols, widths, blank_first = TRUE) {
  printed <- names(cols)
  joined <- lapply(cols, function(src) .hand_join("/", data[src]))
  records <- lapply(seq_len(nrow(data)), function(i) {
    cells <- lapply(printed, function(k) {
      if (is.null(widths[[k]])) joined[[k]][i] else .hand_wrap(joined[[k]][i], widths[[k]])
    })
    tall <- max(c(0L, lengths(cells[printed %in% names(widths)])))
    block <- matrix("", nrow = tall + 1L, ncol = length(printed))
    for (j in seq_along(printed)) {
      v <- cells[[j]]
      if (length(v)) block[seq_along(v), j] <- v
    }
    block
  })
  body <- do.call(rbind, records)
  colnames(body) <- printed

  # the gutters between the printed columns
  out <- list()
  for (j in seq_along(printed)) {
    out[[printed[j]]] <- body[, j]
    if (j < length(printed)) {
      out[[sprintf("S%02d", j)]] <- rep(NA_character_, nrow(body))
    }
  }
  res <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
  if (isTRUE(blank_first)) {
    res <- rbind(res[NA_integer_, , drop = FALSE], res)
  }
  rownames(res) <- NULL
  res
}


# ── Test data ────────────────────────────────────────────────────────────────

# Six adverse events of a made-up study, chosen so that every branch of the
# layout is used: a subject id that wraps (at its hyphens), cells that wrap
# at the "/" only, a piece still too wide that wraps again at a word
# boundary, values missing in the middle and at the end of a joined column,
# and one record whose joined column is empty throughout.
#
# Left out on purpose: a single word wider than its column, and an embedded
# "\n" -- the hand-written rule and build_listing() differ there by design
# (test-listing-vs-manual.R), so the shared cases avoid them.
.listing_ae <- function() {
  data.frame(
    USUBJID  = c("XYZ-101-0001", "XYZ-101-0002", "XYZ-102-000115",
                 "XYZ-103-0004", "XYZ-103-0005", "XYZ-104-0006"),
    AEBODSYS = c("NERVOUS SYSTEM DISORDERS",
                 "GASTROINTESTINAL DISORDERS",
                 "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
                 "INFECTIONS AND INFESTATIONS",
                 "NERVOUS SYSTEM DISORDERS",
                 "GENERAL DISORDERS"),
    AEDECOD  = c("HEADACHE", "NAUSEA", "RASH MACULO-PAPULAR",
                 "UPPER RESPIRATORY TRACT INFECTION", "DIZZINESS", NA),
    ASTDT    = c("2025-03-04", "2025-03-11", "2025-04-02",
                 "2025-04-20", "2025-05-01", "2025-05-13"),
    ASTDY    = c("3", "10", "22", "40", "51", "63"),
    AESEV    = c("MILD", "MODERATE", "SEVERE", "MILD", "MODERATE", "MILD"),
    AESER    = c("N", NA, "Y", "N", "N", NA),
    AEREL    = c("NOT RELATED", "POSSIBLY RELATED", "RELATED",
                 "NOT RELATED", "UNLIKELY RELATED", NA),
    AEACN    = c("DOSE NOT CHANGED", "DOSE REDUCED", "DRUG WITHDRAWN",
                 "DOSE NOT CHANGED", "DOSE NOT CHANGED", NA),
    AEOUT    = c("RECOVERED/RESOLVED", "RECOVERING/RESOLVING",
                 "NOT RECOVERED/NOT RESOLVED", NA, "RECOVERED/RESOLVED", NA),
    AETOXGR  = c("1", "2", "3", "1", "2", NA),
    DOSE     = c(10.5, 20.25, 5, 10.5, 15.75, NA),
    DOSEPRV  = c(10.5, 30, 10, NA, 15.75, NA),
    TRTA     = c("DRUG A", "DRUG B", "DRUG A", "PLACEBO", "DRUG B", "PLACEBO"),
    AECONTRT = c("N", "Y", "Y", "N", "N", NA),
    stringsAsFactors = FALSE
  )
}

# Its ten printed columns: the source columns each joins, in print order ...
.listing_cols_ae <- function() {
  list(
    USUBJID = "USUBJID",
    COL01   = c("AEBODSYS", "AEDECOD"),
    COL02   = "ASTDT",
    COL03   = "ASTDY",
    COL04   = c("AESEV", "AESER", "AEREL"),
    COL05   = c("AEACN", "AEOUT"),
    COL06   = "AETOXGR",
    COL07   = c("DOSE", "DOSEPRV"),
    COL08   = "TRTA",
    COL09   = "AECONTRT"
  )
}

# ... and the widths of the ones that wrap.
.listing_widths_ae <- function() {
  list(USUBJID = 12, COL01 = 24, COL04 = 16, COL05 = 18, COL07 = 8)
}

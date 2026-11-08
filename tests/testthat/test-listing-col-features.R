# listing_col(): headers derived from the data, "flow" layout, and marking a
# key column for repeat suppression (#366).

.labelled_adsl <- function() {
  d <- data.frame(
    USUBJID = c("01-701-1015", "01-701-1015", "01-701-1023"),
    AGE     = c("40", "40", "63"),
    SEX     = c("F", "F", "M"),
    AEDECOD = c("HYPERGLYCAEMIA", "UPPER RESPIRATORY TRACT INFECTION",
                "SKIN ULCER"),
    VISIT   = c("SCREENING", "WEEK 4", "SCREENING"),
    stringsAsFactors = FALSE
  )
  attr(d$USUBJID, "label") <- "Unique Subject Identifier"
  attr(d$AGE,     "label") <- "Age"
  attr(d$SEX,     "label") <- "Sex"
  attr(d$AEDECOD,    "label") <- "Preferred Term"
  d
}

.labels_of <- function(body) {
  vapply(attr(body, "rtf_listing", exact = TRUE)$cols,
         function(cl) cl$label, character(1L))
}


# ── the header comes from the data ───────────────────────────────────────────

test_that("an omitted label is derived from the columns' label attributes", {
  body <- build_listing(.labelled_adsl(),
                        listing_spec(list(listing_col("AEDECOD", width = 20))))
  expect_identical(unname(.labels_of(body)), "Preferred Term")
})

test_that("a column with no label attribute falls back to its name", {
  body <- build_listing(.labelled_adsl(),
                        listing_spec(list(listing_col("VISIT", width = 20))))
  expect_identical(unname(.labels_of(body)), "VISIT")
})

test_that("several source columns give one label per line, separator kept", {
  body <- build_listing(.labelled_adsl(),
                        listing_spec(list(listing_col(c("AGE", "SEX"),
                                                      width = 20))))
  expect_identical(unname(.labels_of(body)), "Age/\nSex")
})

test_that("a derived header is wrapped to the column, so it cannot be wider", {
  body <- build_listing(.labelled_adsl(),
                        listing_spec(list(listing_col("USUBJID", width = 8))))
  lines <- strsplit(unname(.labels_of(body)), "\n", fixed = TRUE)[[1L]]
  expect_gt(length(lines), 1L)
  expect_true(all(listing_disp_width(lines) <= 8))
})

test_that("a label the author laid out is used exactly as written", {
  # A vector, or a string carrying its own breaks, says "I laid this out" --
  # even where a line is wider than the column (#380).
  hand <- c("A header the", "author wrote")
  body <- build_listing(.labelled_adsl(),
                        listing_spec(list(listing_col("AEDECOD", width = 6,
                                                      label = hand))))
  expect_identical(unname(.labels_of(body)), "A header the
author wrote")
})

test_that("label = \"\" asks for a deliberately empty header", {
  body <- build_listing(.labelled_adsl(),
                        listing_spec(list(listing_col("AEDECOD", label = ""))))
  expect_identical(unname(.labels_of(body)), "")
})

test_that("the derived header reaches the rendered table", {
  tbl <- as_rtftables(.labelled_adsl(),
                      listing = listing_spec(list(
                        listing_col("AEDECOD", width = 20),
                        listing_col("VISIT", width = 12))))[[1L]]
  expect_identical(unlist(tbl$col_header[[1L]])[c(1L, 3L)],
                   c("Preferred Term", "VISIT"))
})


# ── layout: stack / flow ─────────────────────────────────────────────────────

test_that("\"stack\" breaks after every separator, \"flow\" fills the line", {
  expect_identical(.listing_wrap_sep_word("40/F", 20, "/", "stack"),
                   c("40/", "F"))
  expect_identical(.listing_wrap_sep_word("40/F", 20, "/", "flow"), "40/F")
})

test_that("\"flow\" still breaks once the line is full", {
  expect_identical(
    .listing_wrap_sep_word("40/F/SCREENING/COMPLETED", 12, "/", "flow"),
    c("40/F/", "SCREENING/", "COMPLETED"))
})

test_that("with no width there is nothing to lay out, under either layout", {
  # `width` is what says how a column is laid out; without one both layouts
  # return the text as it stands, which is the behaviour that shipped.
  expect_identical(.listing_wrap_sep_word("40/F", NULL, "/", "stack"), "40/F")
  expect_identical(.listing_wrap_sep_word("40/F", NULL, "/", "flow"), "40/F")
})

test_that("layout is per column, and the listing's own is the default", {
  # Two variables joined in a cell: the separator is the join, so "stack"
  # puts each on its own line and "flow" lets them share one (#910).  (This
  # was one variable "40/F" until #910: its "/" is data, and a value that fits
  # no longer breaks after it.)
  d <- data.frame(AGE = "40", SEX = "F", stringsAsFactors = FALSE)
  spec <- listing_spec(list(listing_col(c("AGE", "SEX"), width = 20, name = "A"),
                            listing_col(c("AGE", "SEX"), width = 20,
                                        layout = "flow", name = "B")),
                       spacer = FALSE, blank_row = FALSE, record = FALSE)
  body <- build_listing(d, spec)
  expect_identical(body$A, c("40/", "F"))     # stacked: two rows
  expect_identical(body$B, c("40/F", ""))     # flowed: one, padded to match

  # ... and the listing-wide default can be flipped
  spec2 <- listing_spec(list(listing_col(c("AGE", "SEX"), width = 20)),
                        layout = "flow",
                        spacer = FALSE, blank_row = FALSE, record = FALSE)
  expect_identical(build_listing(d, spec2)[[1L]], "40/F")
})

test_that("layout is validated", {
  expect_error(listing_col("A", layout = "sideways"), "should be one of")
  expect_error(listing_spec("A", layout = "sideways"), "should be one of")
})


# ── collapse_repeats ─────────────────────────────────────────────────────────

test_that("a marked column is carried down its record's rows", {
  spec <- listing_spec(list(
    listing_col("USUBJID", width = 12, collapse_repeats = TRUE),
    listing_col("AEDECOD", width = 16)))
  body <- build_listing(.labelled_adsl(), spec)

  # record 2 wraps to three lines; the id is on all three, not just the first
  rows <- which(body$.rtf_record == 2L)
  content <- rows[seq_len(length(rows) - 1L)]      # its trailing blank apart
  expect_length(content, 3L)
  expect_true(all(body$USUBJID[content] == "01-701-1015"))

  # and on the blank row that closes the record (#742): the run goes on to
  # the next record of the same id, which as_rtftables() then blanks
  expect_identical(body$USUBJID[rows[length(rows)]], "01-701-1015")
})

test_that("records of the same value print it once, their blank rows between (#742)", {
  d <- data.frame(USUBJID = c("S1", "S1", "S2", "S2"),
                  AEDECOD = c("Cough", "Headache", "Nausea", "Rash"))
  spec <- listing_spec(list(listing_col("USUBJID", label = "Subject", collapse_repeats = TRUE),
                            listing_col("AEDECOD", label = "Term")))
  expect_true(isTRUE(spec$blank_row))
  tbl <- as_rtftables(d, listing = spec)
  if (!inherits(tbl, "rtftable")) tbl <- tbl[[1L]]
  ids <- tbl$data$USUBJID
  ids[is.na(ids)] <- ""
  expect_identical(ids[nzchar(ids)], c("S1", "S2"))
  # the other column as it was: each record's term, a blank row after it
  expect_identical(tbl$data$AEDECOD, c("Cough", "", "Headache", "", "Nausea", "", "Rash", ""))
})

test_that("an unmarked column is padded with blanks, as before", {
  spec <- listing_spec(list(listing_col("USUBJID", width = 12),
                            listing_col("AEDECOD", width = 16)))
  body <- build_listing(.labelled_adsl(), spec)
  rows <- which(body$.rtf_record == 2L)
  expect_identical(body$USUBJID[rows], c("01-701-1015", "", "", ""))
})

test_that("a cell that already wraps is not carried down", {
  # There is no single value to repeat, so it is padded as usual.
  d <- data.frame(A = "AAAA BBBB CCCC", B = "x\ny\nz", stringsAsFactors = FALSE)
  spec <- listing_spec(list(listing_col("A", width = 4,
                                        collapse_repeats = TRUE),
                            listing_col("B", width = 4)),
                       spacer = FALSE, blank_row = FALSE, record = FALSE)
  body <- build_listing(d, spec)
  expect_identical(body$A, c("AAAA", "BBBB", "CCCC"))
})

test_that("as_rtftables blanks the repeats the marked column carried", {
  spec <- listing_spec(list(
    listing_col("USUBJID", width = 12, collapse_repeats = TRUE),
    listing_col("AEDECOD", width = 16)))
  tbl <- as_rtftables(.labelled_adsl(), listing = spec)[[1L]]

  ids <- tbl$data[[1L]]
  ids[is.na(ids)] <- ""
  # the id prints once per run of its records, not on every row of them:
  # records 1 and 2 are the same subject, record 3 another (#742)
  expect_identical(ids[nzchar(ids)], c("01-701-1015", "01-701-1023"))
})

# A key column to the LEFT that wraps must not stop the one to its right from
# being blanked inside the record (#505): "Xanomeline" / "High Dose" are two
# lines of one record, not two values.
.wrap_keys <- function() {
  data.frame(TRT  = c("Placebo", "Xanomeline High Dose", "Xanomeline High Dose"),
             SUBJ = c("S-1", "S-2", "S-3"),
             TERM = c("A", "B", "C"), stringsAsFactors = FALSE)
}
.wrap_spec <- function(trt_width = 12, blank_row = TRUE) {
  listing_spec(list(
    listing_col("TRT", width = trt_width, collapse_repeats = TRUE),
    listing_col("SUBJ", width = 8, collapse_repeats = TRUE),
    listing_col("TERM", width = 8)), blank_row = blank_row)
}
.printed <- function(v) { v[is.na(v)] <- ""; v }

test_that("a key prints once per record when a key to its left wraps", {
  tbl <- as_rtftables(.wrap_keys(), listing = .wrap_spec())[[1L]]
  subj <- .printed(tbl$data$SUBJ)
  trt  <- .printed(tbl$data$TRT)
  # every record's subject once, on the record's first line
  expect_identical(subj[nzchar(subj)], c("S-1", "S-2", "S-3"))
  # the wrapped treatment keeps both of its lines, once: records 2 and 3
  # share it, so the second prints no line of it
  expect_identical(trt[nzchar(trt)],
                   c("Placebo", "Xanomeline", "High Dose"))
})

test_that("with no wrap the blanking is what it always was", {
  tbl <- as_rtftables(.wrap_keys(), listing = .wrap_spec(trt_width = 22))[[1L]]
  expect_identical(.printed(tbl$data$SUBJ)[nzchar(.printed(tbl$data$SUBJ))],
                   c("S-1", "S-2", "S-3"))
  # the same treatment on consecutive records is printed once: the blank
  # row between records carries it, so the run goes on (#742)
  expect_identical(sum(.printed(tbl$data$TRT) == "Xanomeline High Dose"), 1L)
})

test_that("without blank rows, a key still collapses across records", {
  d <- .wrap_keys()
  d$SUBJ[3L] <- "S-2"                 # records 2 and 3: the same subject
  tbl <- as_rtftables(d, listing = .wrap_spec(trt_width = 22,
                                              blank_row = FALSE))[[1L]]
  expect_identical(.printed(tbl$data$SUBJ), c("S-1", "S-2", ""))
})

test_that("a record continued on the next page shows its key again", {
  d <- data.frame(TRT = "Xanomeline High Dose", SUBJ = "S-9",
                  TERM = "a long term that wraps over several lines",
                  stringsAsFactors = FALSE)
  spec <- listing_spec(list(
    listing_col("TRT", width = 12, collapse_repeats = TRUE),
    listing_col("SUBJ", width = 8, collapse_repeats = TRUE),
    listing_col("TERM", width = 8)))
  pg <- as_rtftables(d, listing = spec, max_rows = 3, split = "rows",
                     split_rows = 3L)
  expect_gt(length(pg), 1L)
  for (p in pg) {
    s <- .printed(p$data$SUBJ)
    expect_identical(s[1L], "S-9")
    expect_identical(sum(nzchar(s)), 1L)
  }
})

test_that("a plan listing blanks the same way", {
  p <- table_plan(.wrap_keys()) |>
    plan_listing(listing_col("TRT", width = 12, collapse_repeats = TRUE),
                 listing_col("SUBJ", width = 8, collapse_repeats = TRUE),
                 listing_col("TERM", width = 8)) |>
    plan_blanks(where = "records")
  subj <- .printed(plan_apply(p)[[1L]]$data$SUBJ)
  expect_identical(subj[nzchar(subj)], c("S-1", "S-2", "S-3"))
})

test_that("FT-IX037 [FR-IX410]: a listing's pages by page_by start with their own first record, keys shown, no ghost row", {
  d <- data.frame(
    TRT  = c(rep("Placebo", 5), rep("Drug", 4)),
    SUBJ = c("S1", "S1", "S2", "S2", "S2", "S3", "S3", "S4", "S4"),
    SEV  = c("MILD", "MILD", "MILD", "MODERATE", "MODERATE", "MILD", "MODERATE",
             "MODERATE", "MODERATE"),
    TERM = paste("T", 1:9), DAY = as.character(1:9), stringsAsFactors = FALSE)
  pages_by <- function(by) {
    p <- table_plan(d) |>
      plan_listing(listing_col("TRT", width = 12, collapse_repeats = TRUE),
                   listing_col("SUBJ", width = 8, collapse_repeats = TRUE),
                   listing_col("SEV", width = 9),
                   listing_col("TERM", width = 10),
                   listing_col("DAY", width = 5)) |>
      plan_paginate_rows(page_by = by, heading_at = "title",
                         heading = if (length(by) == 1L) "{value}" else "{TRT} {SEV}")
    lapply(suppressMessages(plan_apply(p, "pages")), function(pg) {
      x <- as.data.frame(pg$data, stringsAsFactors = FALSE)
      x[] <- lapply(x, function(v) { v[is.na(v)] <- ""; v })
      x
    })
  }
  first_rec <- function(x) x[1L, c("TRT", "SUBJ", "TERM", "DAY")]
  # one BY variable: the MODERATE page opens with its own first record
  pg <- pages_by("SEV")
  expect_length(pg, 2L)
  expect_identical(unlist(first_rec(pg[[2L]]), use.names = FALSE),
                   c("Placebo", "S2", "T 4", "4"))
  expect_identical(unlist(first_rec(pg[[1L]]), use.names = FALSE),
                   c("Placebo", "S1", "T 1", "1"))
  # every record keeps its blank row on its own page: 4 + 5 records
  expect_identical(vapply(pg, nrow, 1L, USE.NAMES = FALSE), c(8L, 10L))
  # a record's key never stands alone on a row of its own
  for (x in c(pg, pages_by(c("TRT", "SEV")))) {
    alone <- nzchar(x$TRT) & !nzchar(x$TERM)
    expect_false(any(alone))
  }
  # two BY variables: a page per (TRT, SEV), each opening on its own record
  pg2 <- pages_by(c("TRT", "SEV"))
  expect_length(pg2, 4L)
  expect_identical(vapply(pg2, function(x) x$DAY[1L], "", USE.NAMES = FALSE), c("1", "4", "6", "7"))
  expect_identical(vapply(pg2, function(x) sum(nzchar(x$DAY)), 1L, USE.NAMES = FALSE),
                   c(3L, 2L, 1L, 3L))
  expect_true(all(vapply(pg2, function(x) all(nzchar(x$TRT[1L]), nzchar(x$SUBJ[1L])), NA, USE.NAMES = FALSE)))
})

test_that("FT-IX038 [FR-IX411]: a listing blanks a repeat of a value that wraps, as one that does not", {
  d <- data.frame(
    TERM = c(rep("Urinary tract infection", 3), "Rash", "Rash",
             "Urinary tract infection"),
    SUBJ = paste0("S", 1:6), stringsAsFactors = FALSE)
  pg <- table_plan(d) |>
    plan_listing(listing_col("TERM", width = 10, collapse_repeats = TRUE),
                 listing_col("SUBJ", width = 6)) |>
    plan_paginate_rows(max_rows = 40) |>
    plan_apply()
  x <- as.data.frame(pg[[1L]]$data, stringsAsFactors = FALSE)
  term <- x$TERM
  term[is.na(term)] <- ""
  # the record starts: wrapped lines of "Urinary tract infection" print for
  # record 1 only; "Rash" once; the wrapped term again after "Rash"
  starts <- which(nzchar(x$SUBJ))
  expect_length(starts, 6L)
  shown <- nzchar(term[starts])
  expect_identical(shown, c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE))
  # a blanked record has no line of the term left, wrapped lines included
  expect_identical(term[seq(starts[2L], starts[4L] - 1L)],
                   rep("", starts[4L] - starts[2L]))
  # the record that prints it prints all its lines
  expect_identical(term[starts[1L]:(starts[1L] + 2L)],
                   c("Urinary", "tract", "infection"))
})

test_that("FT-IX039 [FR-IX412]: a listing cut into blocks keeps a record on one page and heads a continued block once", {
  d <- data.frame(
    SEV  = rep(c("MILD", "SEVERE"), c(9, 4)),
    TERM = c(rep("Urinary tract infection", 3), "Rash", "Rash",
             rep("Urinary tract infection", 2), "Nausea", "Nausea",
             "Rash", "Rash", rep("Urinary tract infection", 2)),
    SUBJ = paste0("S", 1:13), stringsAsFactors = FALSE)
  pg <- table_plan(d) |>
    plan_listing(listing_col("SEV", width = 6, collapse_repeats = TRUE),
                 listing_col("TERM", width = 10),
                 listing_col("SUBJ", width = 6)) |>
    plan_paginate_rows(max_rows = 8) |>
    plan_row_group(mode = "value", group_col = "SEV") |>
    plan_apply()
  pages <- lapply(pg, function(p) {
    x <- as.data.frame(p$data, stringsAsFactors = FALSE)
    x[] <- lapply(x, function(v) { v[is.na(v)] <- ""; v })
    x
  })
  expect_gt(length(pages), 2L)
  # every record (its SUBJ) is on one page, with all its wrapped lines
  subj <- unlist(lapply(pages, function(x) x$SUBJ[nzchar(x$SUBJ)]))
  expect_identical(subj, paste0("S", 1:13))
  for (x in pages) {
    cont <- grepl("(Cont.)", x$SEV, fixed = TRUE)
    # the record's lines follow its first line on the same page
    i <- which(x$TERM == "Urinary")
    expect_identical(x$TERM[i + 1L], rep("tract", length(i)))
    expect_identical(x$TERM[i + 2L], rep("infection", length(i)))
    if (any(cont)) {
      # one heading, on the first row; the first record prints no severity
      expect_identical(which(cont), 1L)
      expect_identical(x$SEV[2L:nrow(x)], rep("", nrow(x) - 1L))
      # and the row under it is a record's first line, not an empty row
      expect_true(nzchar(x$SUBJ[2L]))
    }
  }
  expect_false(any(grepl("(Cont.)", pages[[1L]]$SEV, fixed = TRUE)))
})

test_that("FT-IX040 [FR-IX413]: a listing blanks a column whose record repeats the previous one's, a wrapped key to its left and records of different heights", {
  d <- data.frame(
    SOC  = rep(c("Infections and disorders", "Skin"), c(5, 2)),
    SEV  = c("MILD", "MILD", "MILD", "MILD", "SEVERE", "SEVERE", "SEVERE"),
    TERM = c("a b c d e f g h i j k", "a", "a b c d e f", "a", "a b c d e", "a",
             "a b c"),
    SUBJ = paste0("S", 1:7), stringsAsFactors = FALSE)
  pg <- table_plan(d) |>
    plan_listing(listing_col("SOC", width = 12, collapse_repeats = TRUE),
                 listing_col("SEV", width = 8, collapse_repeats = TRUE),
                 listing_col("TERM", width = 6),
                 listing_col("SUBJ", width = 4)) |>
    plan_paginate_rows(max_rows = 60) |>
    plan_apply()
  x <- as.data.frame(pg[[1L]]$data, stringsAsFactors = FALSE)
  x[] <- lapply(x, function(v) { v[is.na(v)] <- ""; v })
  starts <- which(nzchar(x$SUBJ))
  expect_length(starts, 7L)
  # SOC: records 1 and 6 (2 to 5 repeat 1; 7 repeats 6);
  # SEV: records 1, 5 and 6 (a change of SOC restarts it too)
  expect_identical(nzchar(x$SEV[starts]),
                   c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_identical(nzchar(x$SOC[starts]),
                   c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))
  # a record prints no line of a blanked key
  expect_identical(sum(nzchar(x$SEV)), 3L)
})

test_that("FT-IX041 [FR-IX414]: first_rows does not cut a listing record, and the record that opens the next page shows its keys", {
  d <- data.frame(SOC  = rep("Infections and disorders", 4),
                  TERM = c("a b c d e f g h", "a b c d e f g h", "a b c d", "a"),
                  SUBJ = paste0("S", 1:4), stringsAsFactors = FALSE)
  spec <- listing_spec(list(
    listing_col("SOC", width = 12, collapse_repeats = TRUE),
    listing_col("TERM", width = 4),
    listing_col("SUBJ", width = 4)))
  pg <- as_rtftables(d, listing = spec, max_rows = 40, first_rows = 7)
  expect_gt(length(pg), 1L)
  x <- lapply(pg, function(p) {
    v <- as.data.frame(p$data, stringsAsFactors = FALSE)
    v[] <- lapply(v, function(z) { z[is.na(z)] <- ""; z })
    v
  })
  # record 1 (3 lines and a blank row) fits in 7 rows, record 2 does not:
  # the first page ends after record 1, not inside record 2
  expect_identical(x[[1L]]$SUBJ[nzchar(x[[1L]]$SUBJ)], "S1")
  expect_identical(x[[2L]]$SUBJ[nzchar(x[[2L]]$SUBJ)][1L], "S2")
  expect_true(nzchar(x[[2L]]$SOC[1L]))             # a page top shows its key
  expect_true(nzchar(x[[2L]]$SUBJ[1L]))            # it opens on a first line
  # the key is printed with record 1 only: its lines end before the blank row
  blank <- which(!nzchar(x[[1L]]$TERM) & !nzchar(x[[1L]]$SUBJ))[1L]
  expect_false(any(nzchar(x[[1L]]$SOC[-seq_len(blank)])))
})

test_that("a caller's own collapse_repeats wins over the marked columns", {
  spec <- listing_spec(list(
    listing_col("USUBJID", width = 12, collapse_repeats = TRUE),
    listing_col("AEDECOD", width = 16)))
  tbl <- as_rtftables(.labelled_adsl(), listing = spec,
                      collapse_repeats = "AEDECOD")[[1L]]
  ids <- tbl$data[[1L]]
  ids[is.na(ids)] <- ""
  expect_gt(sum(nzchar(ids)), 3L)                 # the id was NOT suppressed
})

test_that("collapse_repeats is validated", {
  expect_error(listing_col("A", collapse_repeats = "yes"), "TRUE or FALSE")
  expect_error(listing_col("A", collapse_repeats = NA), "TRUE or FALSE")
})


# ── label: a vector is the lines, a string is words to lay out (#380) ────────

.lab_of <- function(spec, data = .labelled_adsl(), j = 1L) {
  attr(build_listing(data, spec), "rtf_listing", exact = TRUE)$cols[[j]]$label
}

test_that("a character vector is one line per element", {
  spec <- listing_spec(list(listing_col("AEDECOD", width = 15,
                                        label = c("Unique", "Subject ID"))))
  expect_identical(.lab_of(spec), "Unique\nSubject ID")
})

test_that("a vector is a layout, so it is not re-wrapped", {
  # the second line is wider than the column and stays whole
  spec <- listing_spec(list(listing_col("AEDECOD", width = 4,
                                        label = c("A", "BBBB CCCC"))))
  expect_identical(.lab_of(spec), "A\nBBBB CCCC")
})

test_that("a single string is laid out at the width", {
  spec <- listing_spec(list(listing_col("AEDECOD", width = 16,
                                        label = "Preferred term of the event")))
  expect_identical(.lab_of(spec), "Preferred term\nof the event")
})

test_that("a single string with no width is left as one line", {
  spec <- listing_spec(list(listing_col("AEDECOD",
                                        label = "Preferred term of the event")))
  expect_identical(.lab_of(spec), "Preferred term of the event")
})

test_that("a string carrying its own breaks is a layout too", {
  spec <- listing_spec(list(listing_col("AEDECOD", width = 8,
                                        label = "Preferred term\nof the event")))
  expect_identical(.lab_of(spec), "Preferred term\nof the event")
})

test_that("the layout rule is the cells' rule: separator, words, hard split", {
  spec <- listing_spec(list(listing_col("AEDECOD", width = 22,
                                        label = "COMPLETED/UPPER/HYPERGLYCAEMIA")))
  # One variable, so the "/" are words in one label, not joins (#910): the
  # label fills the line and breaks after a "/" only where it ran out.
  expect_identical(.lab_of(spec), "COMPLETED/UPPER/\nHYPERGLYCAEMIA")

  spec2 <- listing_spec(list(listing_col("AEDECOD", width = 8,
                                         label = "Immunohistochemistry")))
  lines <- strsplit(.lab_of(spec2), "\n", fixed = TRUE)[[1L]]
  expect_true(all(listing_disp_width(lines) <= 8))
})

test_that("label is validated", {
  expect_error(listing_col("A", label = 1:2), "character vector of header")
  expect_error(listing_col("A", label = character(0)),
               "character vector of header")
  expect_error(listing_col("A", label = c("a", NA)),
               "character vector of header")
})


# ── listing_wrap() ───────────────────────────────────────────────────────────

test_that("listing_wrap() applies the rule and returns the lines", {
  expect_identical(listing_wrap("COMPLETED/UPPER/HYPERGLYCAEMIA", 22),
                   c("COMPLETED/", "UPPER/", "HYPERGLYCAEMIA"))
  expect_identical(listing_wrap("40/F", 20, layout = "flow"), "40/F")
  expect_true(all(listing_disp_width(
    listing_wrap("Immunohistochemistry", 8)) <= 8))
})

test_that("listing_wrap() vectorises, and NULL width means no limit", {
  out <- listing_wrap(c("A/B", "C/D"), 20)
  expect_type(out, "list")
  expect_length(out, 2L)
  expect_identical(listing_wrap("Preferred term of the event", NULL),
                   "Preferred term of the event")
})

test_that("listing_wrap() composes with a vector label", {
  spec <- listing_spec(list(
    listing_col("AEDECOD", width = 16,
                label = listing_wrap("Preferred term of the event", 16))))
  expect_identical(.lab_of(spec), "Preferred term\nof the event")
})

test_that("listing_wrap() validates its arguments", {
  expect_error(listing_wrap("a", 10, sep = c("/", "-")), "single string")
  expect_error(listing_wrap("a", -1), "positive number")
})


# ── a derived header breaks where the data breaks (#384) ─────────────────────

.sexage <- function() {
  d <- data.frame(SEX = c("Female", "Male"), AGE = c("18", "72"),
                  stringsAsFactors = FALSE)
  attr(d$SEX, "label") <- "Sex"
  attr(d$AGE, "label") <- "Age"
  d
}

.pair <- function(width, layout) {
  spec <- listing_spec(list(listing_col(c("SEX", "AGE"), width = width,
                                        layout = layout)),
                       spacer = FALSE, blank_row = FALSE, record = FALSE)
  b <- build_listing(.sexage(), spec)
  list(cell = b[[1L]][1:2],
       head = strsplit(attr(b, "rtf_listing", exact = TRUE)$cols[[1L]]$label,
                       "\n", fixed = TRUE)[[1L]])
}

test_that("stacked: the header breaks at the separator, as the cell does", {
  r <- .pair(9, "stack")
  expect_identical(r$cell, c("Female/", "18"))
  expect_identical(r$head, c("Sex/", "Age"))
})

test_that("flowed: a cell on one line gets a header on one line", {
  r <- .pair(9, "flow")
  expect_identical(r$cell[[1L]], "Female/18")
  expect_identical(r$head, "Sex/Age")
})

test_that("a flowed header used to stay stacked -- two labels can share a line", {
  # Before #384 each label was wrapped separately, so `layout = "flow"` flowed
  # the cells and never the header.
  expect_length(.pair(20, "flow")$head, 1L)
  expect_length(.pair(20, "stack")$head, 2L)
})

test_that("stacked headers are unchanged by the join", {
  # Joining and wrapping once must give what wrapping each label gave, because
  # stack breaks at every separator anyway.
  d <- .labelled_adsl()
  spec <- listing_spec(list(listing_col(c("AGE", "SEX"), width = 4)))
  expect_identical(
    attr(build_listing(d, spec), "rtf_listing", exact = TRUE)$cols[[1L]]$label,
    "Age/\nSex")
})


# ── listing_spec(wrap = ): a listing's own splitter ──────────────────────────

test_that("a custom wrap lays out the cells and the headers alike", {
  by_pipe <- function(text, width, sep, layout) {
    unlist(strsplit(as.character(text)[[1L]], "|", fixed = TRUE))
  }
  spec <- listing_spec(list(listing_col("A", width = 20, label = "X|Y|Z")),
                       sep = "|", wrap = by_pipe, spacer = FALSE,
                       blank_row = FALSE, record = FALSE)
  b <- build_listing(data.frame(A = "a|b|c", stringsAsFactors = FALSE), spec)

  expect_identical(b$A, c("a", "b", "c"))
  expect_identical(attr(b, "rtf_listing", exact = TRUE)$cols[[1L]]$label,
                   "X\nY\nZ")
})

test_that("a custom wrap is used for a DERIVED header too", {
  by_pipe <- function(text, width, sep, layout) {
    unlist(strsplit(as.character(text)[[1L]], "|", fixed = TRUE))
  }
  d <- data.frame(A = "a", B = "b", stringsAsFactors = FALSE)
  attr(d$A, "label") <- "Alpha"
  attr(d$B, "label") <- "Beta"
  spec <- listing_spec(list(listing_col(c("A", "B"), width = 20)),
                       sep = "|", wrap = by_pipe, spacer = FALSE,
                       blank_row = FALSE, record = FALSE)
  expect_identical(
    attr(build_listing(d, spec), "rtf_listing", exact = TRUE)$cols[[1L]]$label,
    "Alpha\nBeta")
})

test_that("wrap is validated, and its contract enforced", {
  expect_error(listing_spec("A", wrap = "nope"), "must be a function")
  expect_error(listing_spec("A", wrap = function(x) x), "four arguments")

  bad <- function(text, width, sep, layout) 42
  spec <- listing_spec(list(listing_col("A", width = 5)), wrap = bad,
                       spacer = FALSE, blank_row = FALSE, record = FALSE)
  expect_error(build_listing(data.frame(A = "x", stringsAsFactors = FALSE),
                             spec),
               "non-empty character")
})

test_that("listing_code() says a custom wrap cannot be written out", {
  by_pipe <- function(text, width, sep, layout) {
    unlist(strsplit(as.character(text)[[1L]], "|", fixed = TRUE))
  }
  plain  <- listing_code(listing_spec(list(listing_col("A", width = 5))))
  custom <- listing_code(listing_spec(list(listing_col("A", width = 5)),
                                      wrap = by_pipe))
  expect_false(any(grepl("custom `wrap`", plain, fixed = TRUE)))
  expect_true(any(grepl("custom `wrap`", custom, fixed = TRUE)))
})

test_that("the cell path enforces the contract too, not just the header", {
  # A vector label is a layout, so it is NOT wrapped: the only call left is
  # the one on the cell, and it must be validated the same way (#386).
  bad  <- function(text, width, sep, layout) list("x")
  spec <- listing_spec(list(listing_col("A", width = 5,
                                        label = c("A", "B"))),
                       wrap = bad, spacer = FALSE, blank_row = FALSE,
                       record = FALSE)
  expect_error(build_listing(data.frame(A = "x", stringsAsFactors = FALSE),
                             spec),
               "non-empty character")
})

test_that("wrap = listing_wrap is a no-op, so it can be delegated to", {
  d <- data.frame(SEX = "Female", AGE = "18", stringsAsFactors = FALSE)
  attr(d$SEX, "label") <- "Sex"
  attr(d$AGE, "label") <- "Age"
  build <- function(...) {
    spec <- listing_spec(list(listing_col(c("SEX", "AGE"), width = 9)),
                         spacer = FALSE, blank_row = FALSE, record = FALSE,
                         ...)
    b <- build_listing(d, spec)
    list(cells = b[[1L]],
         label = attr(b, "rtf_listing", exact = TRUE)$cols[[1L]]$label)
  }
  expect_identical(build(wrap = listing_wrap), build())

  # ... and the documented way to write one: delegate, then adjust.
  shout <- function(text, width, sep, layout) {
    toupper(listing_wrap(text, width, sep, layout))
  }
  got <- build(wrap = shout)
  expect_identical(got$cells, toupper(build()$cells))
  expect_identical(got$label, toupper(build()$label))
})

# ── listing_wrap_code(): the rule as source ──────────────────────────────────

test_that("the emitted rule is self-contained and IS the shipped rule", {
  src <- listing_wrap_code("my_wrap")

  # Nothing in it reaches back into the package's INTERNALS; the measurements
  # it does call are exported, and shared on purpose (#392).
  expect_false(any(grepl(":::", src, fixed = TRUE)))
  expect_false(any(grepl(".listing_", src, fixed = TRUE)))
  expect_true(any(grepl("listing_disp_width(", src, fixed = TRUE)))

  env <- new.env(parent = globalenv())
  eval(parse(text = src), envir = env)
  expect_setequal(ls(env),
                  c("my_wrap", "my_wrap_flow", "my_wrap_words"))

  # And it cannot have drifted, because it is read off the live functions.
  # Widths, separators, layouts, CJK, an embedded newline, an empty cell.
  txt <- c("COMPLETED/UPPER/HYPERGLYCAEMIA",
           "UPPER RESPIRATORY TRACT INFECTION",
           "\u65e5\u672c\u8a9e\u306e\u9577\u3044\u8a18\u8ff0/A", "a\nb", "",
           "SUPERCALIFRAGILISTIC")
  for (w in list(NULL, 6, 12, 40)) {
    for (lay in c("stack", "flow")) {
      for (t in txt) {
        expect_identical(env$my_wrap(t, w, "/", lay),
                         listing_wrap(t, w, "/", lay),
                         info = paste(t, w, lay))
      }
    }
  }
})

test_that("what it emits can be handed straight back as `wrap`", {
  src <- listing_wrap_code("edited")
  env <- new.env(parent = globalenv())
  eval(parse(text = src), envir = env)

  d <- data.frame(AEDECOD = "ACUTE KIDNEY INJURY", stringsAsFactors = FALSE)
  plain <- listing_spec(list(listing_col("AEDECOD", width = 12, label = "H")),
                        spacer = FALSE, blank_row = FALSE, record = FALSE)
  edited <- listing_spec(list(listing_col("AEDECOD", width = 12, label = "H")),
                         wrap = env$edited, spacer = FALSE, blank_row = FALSE,
                         record = FALSE)
  expect_identical(build_listing(d, edited)$AEDECOD, build_listing(d, plain)$AEDECOD)
})

test_that("the source keeps its comments, which is the point of copying it", {
  src <- listing_wrap_code()
  expect_true(any(grepl("^[[:space:]]*#", src[-seq_len(9L)])))
  expect_true(any(grepl("#364", src, fixed = TRUE)))   # the rationale, kept
})

test_that("the header names the entry function and states the contract", {
  src <- listing_wrap_code("house_rule")
  expect_true(any(grepl("wrap = house_rule", src, fixed = TRUE)))
  expect_true(any(grepl("text, width, sep, layout", src, fixed = TRUE)))
  expect_true(any(grepl(as.character(utils::packageVersion("rtfreporter")),
                        src, fixed = TRUE)))
})

test_that("the helpers are named after `name`, so two rules can coexist", {
  a <- listing_wrap_code("alpha")
  b <- listing_wrap_code("beta")
  env <- new.env(parent = globalenv())
  eval(parse(text = a), envir = env)
  eval(parse(text = b), envir = env)
  expect_true(all(c("alpha", "alpha_words", "beta", "beta_words") %in% ls(env)))
  expect_identical(env$alpha("A/B", 3, "/", "stack"),
                   env$beta("A/B", 3, "/", "stack"))
})

test_that("listing_wrap_code() validates `name`", {
  expect_error(listing_wrap_code(c("a", "b")), "single non-empty string")
  expect_error(listing_wrap_code(""), "single non-empty string")
  expect_error(listing_wrap_code("2fast"), "syntactic R name")
})

test_that("the template has not drifted from the rule it was copied from", {
  # Regenerate with: Rscript data-raw/gen_listing_wrap_template.R
  #
  # Not under covr: it rewrites every function body to count what runs, so
  # the live deparse is instrumented code and would never match a source
  # file.  The comparison still runs in R CMD check on every platform.
  skip_on_covr()
  env <- new.env(parent = globalenv())
  eval(parse(text = .listing_wrap_template()), envir = env)
  for (nm in .listing_wrap_parts()) {
    expect_identical(deparse(get(nm, envir = env)),
                     deparse(get(nm, envir = asNamespace("rtfreporter"))),
                     info = nm)
  }
})

# ── the measurements the rule is built on ────────────────────────────────────

test_that("listing_disp_width() counts columns, not characters", {
  expect_identical(listing_disp_width(c("ABC", "\u3042\u3044\u3046")), c(3L, 6L))
  expect_identical(listing_disp_width(c(NA, "")), c(0L, 0L))
  expect_identical(listing_disp_width(42), 2L)
})

test_that("listing_take() gives the longest prefix that fits", {
  expect_identical(listing_take("HYPERGLYCAEMIA", 6), "HYPERG")
  expect_identical(listing_take("\u3042\u3044\u3046", 4), "\u3042\u3044")
  expect_identical(listing_take("", 5), "")
  # Never "" for non-empty text: a caller looping on the remainder must be
  # able to make progress even where one glyph is wider than the column.
  expect_identical(listing_take("\u3042", 1), "\u3042")
})

test_that("listing_split_after() keeps the separator on the line it closes", {
  expect_identical(listing_split_after("COMPLETED/UPPER", "/"),
                   c("COMPLETED/", "UPPER"))
  expect_identical(listing_split_after("a.b", "."), c("a.", "b"))   # literal
  expect_identical(listing_split_after("a/b", ""), "a/b")
  expect_identical(listing_split_after("a/b", NULL), "a/b")
})

test_that("the emitted rule shares them rather than carrying a copy", {
  # The fork calls the package's measurements, so fixing one fixes every fork.
  src <- listing_wrap_code("shared")
  expect_false(any(grepl("shared_disp_width", src, fixed = TRUE)))
  expect_true(any(grepl("listing_disp_width(", src, fixed = TRUE)))
  expect_true(any(grepl("listing_split_after(", src, fixed = TRUE)))
})

test_that("the measures and listing_wrap() refuse what is not one string or one width (#764)", {
  expect_error(listing_take(c("a", "b"), 3), "`x` must be one string")
  expect_error(listing_take(NA_character_, 3), "`x` must be one string")
  expect_error(listing_take("abc", NA), "`width` must be one number")
  expect_error(listing_take("abc", c(1, 2)), "`width` must be one number")
  expect_identical(listing_take("abc", 0), "a")   # never "" for non-empty text
  expect_error(listing_split_after(c("a/b", "c"), "/"), "`text` must be one string")
  expect_error(listing_split_after(NA_character_, "/"), "`text` must be one string")
  expect_error(listing_split_after("a/b", c("/", "-")), "`sep` must be one string")
  expect_identical(listing_split_after("a/b", NULL), "a/b")
  expect_error(listing_wrap("a b", NA), "`width` must be a single positive number")
  expect_error(listing_wrap("a b", NA_real_), "`width` must be a single positive number")
  # a wrap rule may take the four arguments through ...
  expect_s3_class(listing_spec("A", wrap = function(text, width, ...) text), "rtf_listing_spec")
  expect_error(listing_spec("A", wrap = function(text, width) text), "four arguments")
})

test_that("FT-IX042 [FR-IX415]: a listing record whose BY value is blank or NA is a page of its own", {
  pages_of <- function(sev, col = "SEV") {
    d <- data.frame(SUBJ = c("S1", "S1", "S2", "S3", "S4"), SEV = sev,
                    TERM = paste("T", 1:5), DAY = as.character(1:5),
                    stringsAsFactors = FALSE)
    p <- table_plan(d) |>
      plan_listing(listing_col("SUBJ", width = 8, collapse_repeats = TRUE),
                   listing_col("SEV", width = 9),
                   listing_col("TERM", width = 10),
                   listing_col("DAY", width = 5)) |>
      plan_paginate_rows(page_by = col, heading_at = "title",
                         heading = "Severity: {value}")
    suppressMessages(plan_apply(p, "pages"))
  }
  days <- function(pg) lapply(pg, function(x) {
    v <- as.character(as.data.frame(x$data, stringsAsFactors = FALSE)$DAY)
    v[!is.na(v) & nzchar(v)]
  })
  for (blank in list("", NA_character_)) {
    # first: the blank-severity records come first, and keep their own page
    pg <- pages_of(c(blank, blank, "MILD", "MILD", "SEVERE"))
    expect_identical(names(pg), c("Severity: ", "Severity: MILD", "Severity: SEVERE"))
    expect_identical(unname(days(pg)), list(c("1", "2"), c("3", "4"), "5"))
    # middle: not filled from the next record
    pg <- pages_of(c("MILD", "MILD", blank, "SEVERE", "SEVERE"))
    expect_identical(names(pg), c("Severity: MILD", "Severity: ", "Severity: SEVERE"))
    expect_identical(unname(days(pg)), list(c("1", "2"), "3", c("4", "5")))
  }
  # a record's wrapped line and blank row go with the record
  pg <- pages_of(c("", "", "MILD", "MILD", "SEVERE"))
  expect_identical(nrow(pg[[1L]]$data), 4L)
})

test_that("FT-IX043 [FR-IX415]: without a listing's records a blank BY value is still filled from its neighbour", {
  # a table's stub label row carries NA in the BY column and joins the run it
  # introduces; a blank value of a table row is a gap too: unchanged
  d <- data.frame(by = c("A", NA, "", "B", "B"), x = letters[1:5],
                  stringsAsFactors = FALSE)
  r <- .page_by_runs(d, 1L)
  expect_identical(lapply(r, `[[`, "rows"), list(1L, 2:5))
  expect_identical(vapply(r, `[[`, "", "label"), c("A", "B"))
  # with records, the line of a record (same record id) is filled from the
  # record's first line, and a line without a record from the next row
  rec <- c(1, 1, NA, 2, 2)
  r2 <- .page_by_runs(data.frame(by = c("A", "", NA, "B", ""), rec = rec,
                                 stringsAsFactors = FALSE), 1L, "rec")
  expect_identical(vapply(r2, `[[`, "", "label"), c("A", "B"))
  expect_identical(lapply(r2, `[[`, "rows"), list(1:2, 3:5))
})

test_that("FT-IX045 [FR-IX417]: a listing's BY value wider than its column is one page, named by the whole value", {
  d <- data.frame(SUBJ = c("S1", "S2", "S3", "S4"),
                  SEV  = c("LIFE-THREATENING", "MILD", "MILD", "LIFE-THREATENING"),
                  TERM = c("Anaphylaxis", "Nausea", "Rash", "Sepsis"),
                  DAY  = as.character(1:4), stringsAsFactors = FALSE)
  d <- d[order(d$SEV, d$SUBJ), ]
  plan_of <- function(col_sev, ...) {
    table_plan(d) |>
      plan_listing(listing_col("SUBJ", width = 8), col_sev,
                   listing_col("TERM", width = 12), listing_col("DAY", width = 5)) |>
      plan_paginate_rows(page_by = "SEV", heading_at = "title",
                         heading = "Severity: {value}", ...)
  }
  days <- function(pg) lapply(pg, function(x) {
    v <- as.character(as.data.frame(x$data, stringsAsFactors = FALSE)$DAY)
    v[!is.na(v) & nzchar(v)]
  })
  # 10 wide: the value prints over "LIFE-", "THREATENIN" and "G"
  p  <- plan_of(listing_col("SEV", width = 10))
  pg <- suppressMessages(plan_apply(p, "pages"))
  expect_identical(names(pg), c("Severity: LIFE-THREATENING", "Severity: MILD"))
  expect_identical(unname(days(pg)), list(c("1", "4"), c("2", "3")))
  # the same with 16 wide, where it does not wrap
  pg16 <- suppressMessages(plan_apply(plan_of(listing_col("SEV", width = 16)), "pages"))
  expect_identical(names(pg16), names(pg))
  expect_identical(unname(days(pg16)), unname(days(pg)))
  # a value that wraps at a space, and a page cut in rows as well
  pgm <- suppressMessages(plan_apply(plan_of(listing_col("SEV", width = 6), max_rows = 4), "pages"))
  expect_true(all(startsWith(names(pgm), "Severity: ")))
  expect_setequal(unique(sub("[.][.][.][0-9]+$", "", names(pgm))),
                  c("Severity: LIFE-THREATENING", "Severity: MILD"))
  # the unwrapped values travel on the body; without them the lines are joined
  b <- build_listing(d, listing_spec(list(listing_col("SEV", width = 10), listing_col("DAY", width = 5))))
  expect_identical(attr(b, "rtf_listing_values")$SEV, d$SEV)
  attr(b, "rtf_listing_values") <- NULL
  r <- .page_by_runs(b, 1L, ".rtf_record")
  expect_length(r, 2L)
  expect_identical(vapply(r, `[[`, "", "label"), c("LIFE- THREATENIN G", "MILD"))
})

test_that("FT-IX044 [FR-IX416]: plan_paginate_rows(na_label = ) names the blank BY group, the default leaves it empty", {
  d <- data.frame(SUBJ = c("S1", "S2", "S3", "S4"),
                  SEV = c("", "MILD", NA, "SEVERE"),
                  TERM = paste("T", 1:4), stringsAsFactors = FALSE)
  pg <- function(...) {
    p <- table_plan(d) |>
      plan_listing(listing_col("SUBJ", width = 8), listing_col("SEV", width = 9),
                   listing_col("TERM", width = 10)) |>
      plan_paginate_rows(page_by = "SEV", ...)
    suppressMessages(plan_apply(p, "pages"))
  }
  # unchanged by default
  expect_identical(names(pg(heading = "Severity: {value}", heading_at = "title")),
                   c("Severity: ", "Severity: MILD", "Severity: SEVERE"))
  # the text in the heading and, with no heading, in the page name
  expect_identical(
    names(pg(heading = "Severity: {value}", heading_at = "title", na_label = "Not coded")),
    c("Severity: Not coded", "Severity: MILD", "Severity: SEVERE"))
  expect_identical(names(pg(na_label = "Not coded")),
                   c("Not coded", "MILD", "SEVERE"))
  expect_identical(names(pg()), c("", "MILD", "SEVERE"))
  expect_error(plan_paginate_rows(table_plan(d), na_label = "x"), "page_by")
  expect_error(plan_paginate_rows(table_plan(d), page_by = "SEV", na_label = NA_character_), "one text")
})

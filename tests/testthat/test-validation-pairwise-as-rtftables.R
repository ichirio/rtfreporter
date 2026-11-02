# Validation tests: as_rtftables() -- how its pagination arguments act
# together: split x max_rows x stub x cell_format x first_rows x page_by x
# blank_rows -- a pairwise set checked by invariants on the pages and on the
# RTF written from them.  Helpers prefixed .vr_.  Fictional data only.
#
# ---------------------------------------------------------------------------
#  Design
# ---------------------------------------------------------------------------
#  The rules, as read from the help (as_rtftables()) and the code
#  (R/as_rtftables.R build_pages(), the by_value + stub path; R/paginate.R
#  .paginate_df(), .paginate_by_page(), .order_and_name_pages(),
#  .split_group_safe(), .split_group_force(), .split_by_value()):
#
#    order     page_by partitions first; "by_value" makes the group the outer
#              axis and page_by the inner one, a value's rows gathered in the
#              body's order; otherwise the body's order stands.
#    budget    max_rows counts the rows of a page (stub label rows and
#              continuation rows included, blank rows not); first_rows is the
#              first page's rows, the rest paginated as split says.
#    groups    a page that opens inside a group opens with "<group> (Cont.)";
#              no other page has one.  "group_safe" never cuts a group that
#              fits in max_rows.  "none" ignores max_rows.
#    stub      c("grp", "label") folds the two into one column: a label row
#              per group, the labels indented under it; every record sits
#              under its group's label row (or its continuation row).
#    blank     "between_groups" puts a blank row after the last row of a
#              group that another group follows on the same page.
#    format    cell_format = "right_align" pads the data columns body-wide,
#              so every page has the same widths.
#
#  Data: 18 records, grp A (3), B (4), C (2) in each of two periods, the
#  label "labN" and the value N unique per record.
#
#  (A) FT-TB779, pairwise.  Factors and levels:
#    split        none, group_safe, group_force, by_value
#    max_rows     none, 3, 5
#    stub         none, c("grp", "label")
#    cell_format  none, "right_align"
#    first_rows   none, at the end of group A (3 rows; 4 with the stub),
#                 inside group A (2 rows)
#    page_by      none, "period" (dropped from the print)
#    blank_rows   none, "between_groups"
#  Constraints: the group splits need max_rows; first_rows not with page_by
#  or by_value (FR-TB151).  272 valid combinations;
#  .allpairs() picks the rows covering every pair.  Oracle, by invariants:
#  every record once, in the expected order; each page one period
#  (page_by), one group (by_value); no page over max_rows (the first one
#  first_rows exactly); the continuation rows exactly where a page opens
#  inside a group (the page after a first_rows cut too, #856; not under
#  "none", where that page's stub and blank rows go unchecked);
#  group_safe keeps a group that fits; split = "none" gives
#  one page per partition (and the first_rows page); under the stub every
#  record below its group's label; the blank rows; the value widths; the RTF
#  holds each page's records in order and its continuation rows.
#  FR-TB121 FR-TB137 FR-TB138 FR-TB150 FR-TB596 FR-PL433 FR-PL434 FR-IX262
#  FR-TB608 FR-TB609 FR-TB610 FR-TB611.
#
#  (B) FT-TB780, decision table: split = "by_value" with max_rows on values
#  whose rows come back later in the body, with and without page_by.
#    page_by   max_rows   pages (group, period: records)
#    none      3          A: 1 2 3 | Cont 10 | Cont 11 12; B: 4 5 6 |
#                         Cont 7 13 | Cont 14 | Cont 15 16; C: 8 9 | Cont 17 18
#    period    2          A P1: 1 2 | Cont 3; A P2: 10 11 | Cont 12;
#                         B P1: 4 5 | Cont 6 | Cont 7; B P2: 13 14 | Cont 15 |
#                         Cont 16; C P1: 8 9; C P2: 17 18
#  (the first used to loop forever, the second to interleave a value's
#  periods).  FR-TB608 FR-TB609.
#
#  (C) FT-TB781, decision table: a first_rows cut inside a group (#856).
#  grp A (3), B (4); max_rows = 5, first_rows = 2.
#    split        stub   pages
#    group_force  no     1 2 | A (Cont.) 3 4 5 | B (Cont.) 6 7
#    group_force  yes    A 1 | A (Cont.) 2 3 | B 4 5 6 7
#    group_safe   no     1 2 | A (Cont.) 3 | 4 5 6 7
#    group_safe   yes    A 1 | A (Cont.) 2 3 | B 4 5 6 7
#  and no continuation row: a cut at a group's end (first_rows = 3),
#  split = "none", max_rows = 1.  FR-TB150 FR-TB610.
#
#  (D) FT-TB782, decision table: by_value + stub + max_rows (#857).
#  grp A (6), B (2); stub c("grp", "label").
#    max_rows  page_by  pages
#    none      none     A 1-6 | B 7 8
#    3         none     A 1 2 | A (Cont.) 3 4 | A (Cont.) 5 6 | B 7 8
#    3         period   A 1 | A (Cont.) 3 5 | A 2 | A (Cont.) 4 6 | B 7 | B 8
#    1         none     a row a page, no continuation row
#  (the stub's label rows count; the pages named by the value, "...n").
#  FR-TB596 FR-TB611.
#
#  (E) FT-TB783, pairwise: as_rtftables(listing = ) with max_rows,
#  first_rows and the blank rows.  Data: 8 records, subjects S01 (3), S02
#  (2), S03 (3), the record number "E<n>" printed in its own column, a
#  TERM column that wraps over 1 to 4 lines at width 12.  Factors:
#    split            default (group_safe from max_rows), group_force
#    max_rows         none, 4, 7
#    first_rows       none, at the end of record 2, on record 3's first line
#    count            count_blank_rows FALSE, TRUE
#    blank_rows       none, "between_groups" (the groups are the records)
#    blank_row        listing_spec(blank_row = ) FALSE, TRUE
#    blank_row_first  listing_spec(blank_row_first = ) FALSE, TRUE
#    wrap             TERM unwrapped, wrapped at 12
#    collapse         SUBJ listing_col(collapse_repeats = ) FALSE, TRUE
#  Constraint: group_force needs max_rows.  960 valid combinations (all
#  pass); .allpairs() picks the rows covering every pair.  Oracle, by
#  invariants: every record's rows once, in order (its TERM words, its
#  number); a page that opens inside a record (rows of the last record of
#  the page before still to come) opens with its continuation row -- empty,
#  the record column being hidden (FR-TB576) -- under a split by max_rows,
#  and any other page on a record's first row; no page over max_rows
#  (counting, under count_blank_rows, the blanks between its rows and the
#  top edge unless it merges with an empty first row); the first page
#  first_rows rows (the counted separators among them, the top edge on
#  top); the default split never cuts a record that fits a page alone (but
#  the one first_rows cuts); split none gives one page (and the first_rows
#  page); the subject prints on each record's first row, or with
#  collapse_repeats at the page's first row and where it changes;
#  blank_row_first opens every page; "between_groups" a blank row between
#  two records on a page; the RTF holds a page per page with its records'
#  numbers and TERM words in order.  A record cut just before its blank row
#  is not continued: the next page opens on the next record and that blank
#  row is dropped (#859).  FR-LS110 FR-LS156 FR-PL952 FR-TB150 FR-TB576
#  FR-TB610 FR-TB612 FR-TB613.
# ---------------------------------------------------------------------------

.vr_data <- function() {
  data.frame(grp = rep(rep(c("A", "B", "C"), c(3, 4, 2)), 2),
             label = paste0("lab", 1:18),
             period = rep(c("P1", "P2"), each = 9),
             val = as.character(1:18), stringsAsFactors = FALSE)
}

# One page read back: per row its record (NA for a label / continuation
# row), whether it is a continuation row, its group.
.vr_rows <- function(pg, df) {
  d <- pg$data
  txt <- apply(d, 1L, function(r) paste(ifelse(is.na(r), "", r), collapse = "|"))
  rec <- suppressWarnings(as.integer(sub(".*lab([0-9]+).*", "\\1", txt)))
  rec[!grepl("lab[0-9]+", txt)] <- NA_integer_
  cont <- grepl("(Cont.)", txt, fixed = TRUE)
  first <- trimws(sub("\\|.*", "", txt))
  grp <- ifelse(is.na(rec), sub(" [(]Cont[.][)]$", "", first), df$grp[rec])
  list(rec = rec, cont = cont, grp = grp, first = first)
}

.vr_check <- function(cs) {
  df <- .vr_data()
  stub <- cs$stub == "yes"
  args <- list(df, split = cs$split)
  if (cs$max_rows != "none") args$max_rows <- as.integer(cs$max_rows)
  if (stub) args$stub <- c("grp", "label")
  if (cs$cell_format == "yes") args$cell_format <- "right_align"
  fr <- if (cs$first_rows == "inside") 2L else if (stub) 4L else 3L
  if (cs$first_rows != "no") args$first_rows <- fr
  if (cs$page_by == "yes") {
    args$page_by <- "period"
    args$drop_cols <- "period"
  }
  if (cs$blank_rows == "yes") args$blank_rows <- "between_groups"
  pages <- do.call(as_rtftables, args)
  rows <- lapply(pages, .vr_rows, df = df)
  bad <- character(0)
  recs <- unname(unlist(lapply(rows, function(r) r$rec[!is.na(r$rec)])))
  want <- if (cs$split == "by_value") order(match(df$grp, unique(df$grp)), seq_len(18L)) else 1:18
  if (!identical(recs, want)) bad <- c(bad, "records")
  per <- lapply(rows, function(r) unique(df$period[r$rec[!is.na(r$rec)]]))
  if (cs$page_by == "yes" && any(lengths(per) != 1L)) bad <- c(bad, "one period")
  if (cs$split == "by_value" && any(vapply(rows, function(r) length(unique(r$grp)), 0L) != 1L)) {
    bad <- c(bad, "one group")
  }
  n <- vapply(pages, function(p) nrow(p$data), 0L)
  grouped <- cs$split != "none"
  if (cs$max_rows != "none" && grouped) {
    chk <- if (cs$first_rows != "no") n[-1L] else n
    if (any(chk > as.integer(cs$max_rows))) bad <- c(bad, "budget")
  }
  if (cs$first_rows != "no" && n[1L] != fr) bad <- c(bad, "first_rows")
  if (cs$split == "none") {
    np <- (if (cs$page_by == "yes") 2L else 1L) + (cs$first_rows != "no")
    if (length(pages) != np) bad <- c(bad, "pages of none")
  }
  # continuation rows: exactly where a page opens inside a group run
  for (i in seq_along(rows)) {
    r <- rows[[i]]
    inside <- FALSE
    if (i > 1L) {
      p <- rows[[i - 1L]]
      last <- utils::tail(p$rec[!is.na(p$rec)], 1L)
      nxt <- r$rec[!is.na(r$rec)][1L]
      inside <- df$grp[last] == df$grp[nxt] &&
        (df$period[last] == df$period[nxt] || (cs$split == "by_value" && cs$page_by == "no"))
    }
    # split = "none" writes no continuation row: a first_rows cut inside a
    # group leaves the next page's first rows under no stub label row, and
    # its blank rows are read on that page alone
    headless <- inside && cs$split == "none"
    if (headless) inside <- FALSE
    has <- r$cont[1L]
    if (!identical(has, inside)) bad <- c(bad, sprintf("cont page %d", i))
    if (sum(r$cont) > 1L || any(r$cont[-1L])) bad <- c(bad, sprintf("cont rows page %d", i))
    if (headless) next
    if (stub) {
      for (k in which(!is.na(r$rec))) {
        lab <- which(is.na(r$rec[seq_len(k)]))
        if (!length(lab) || r$grp[max(lab)] != df$grp[r$rec[k]]) bad <- c(bad, sprintf("stub page %d", i))
      }
    }
    # blank rows between the groups of the page
    want_b <- which(utils::head(r$grp, -1L) != r$grp[-1L])
    got_b <- as.integer(pages[[i]]$blank_rows)
    if (cs$blank_rows == "no") want_b <- integer(0)
    if (!identical(sort(got_b), want_b)) bad <- c(bad, sprintf("blanks page %d", i))
  }
  if (cs$split == "group_safe") {
    size <- if (stub) c(A = 4L, B = 5L, C = 3L) else c(A = 3L, B = 4L, C = 2L)
    pg_of <- rep(seq_along(rows), vapply(rows, function(r) sum(!is.na(r$rec)), 0L))
    run <- paste(df$grp, df$period)[recs]
    for (u in unique(run)) {
      g <- sub(" .*", "", u)
      if (size[[g]] <= as.integer(cs$max_rows) && length(unique(pg_of[run == u])) > 1L &&
          !(cs$first_rows != "no" && g == "A" && grepl("P1", u))) {
        bad <- c(bad, paste("group_safe cut", u))
      }
    }
  }
  vals <- unlist(lapply(unname(pages), function(p) {
    d <- p$data
    d[[ncol(d)]][grepl("lab[0-9]+", apply(d, 1L, paste, collapse = "|"))]
  }))
  w <- unique(nchar(vals))
  if (cs$cell_format == "yes" && length(w) != 1L) bad <- c(bad, "widths")
  if (cs$cell_format == "no" && length(w) == 1L) bad <- c(bad, "widths unformatted")
  # the RTF: each page's records in order, its continuation rows
  txt <- .render_to_string(rtf_tables(rtf_document(), pages))
  body <- sub("^.*?\\\\trowd", "\\\\trowd", txt)
  pg_txt <- strsplit(body, "\\\\sect(?![a-z])|\\\\page(?![a-z])", perl = TRUE)[[1L]]
  pg_txt <- pg_txt[grepl("lab[0-9]+", pg_txt)]
  if (length(pg_txt) != length(pages)) {
    bad <- c(bad, "rtf pages")
  } else {
    for (i in seq_along(pages)) {
      got <- as.integer(sub("lab", "", regmatches(pg_txt[i], gregexpr("lab[0-9]+", pg_txt[i]))[[1L]]))
      if (!identical(got, rows[[i]]$rec[!is.na(rows[[i]]$rec)])) bad <- c(bad, sprintf("rtf page %d", i))
      if (lengths(regmatches(pg_txt[i], gregexpr("(Cont.)", pg_txt[i], fixed = TRUE))) != sum(rows[[i]]$cont)) {
        bad <- c(bad, sprintf("rtf cont page %d", i))
      }
    }
  }
  bad
}

test_that("FT-TB779 [FR-TB121 FR-TB137 FR-TB138 FR-TB150 FR-TB596 FR-PL433 FR-PL434 FR-IX262 FR-TB608 FR-TB609 FR-TB610 FR-TB611]: pairwise -- split x max_rows x stub x cell_format x first_rows x page_by x blank_rows", {
  cand <- expand.grid(split = c("none", "group_safe", "group_force", "by_value"),
                      max_rows = c("none", "3", "5"), stub = c("no", "yes"),
                      cell_format = c("no", "yes"), first_rows = c("no", "end", "inside"),
                      page_by = c("no", "yes"), blank_rows = c("no", "yes"),
                      stringsAsFactors = FALSE)
  ok <- !(cand$split %in% c("group_safe", "group_force") & cand$max_rows == "none") &
    !(cand$first_rows != "no" & (cand$page_by == "yes" | cand$split == "by_value"))
  cand <- cand[ok, ]
  expect_identical(nrow(cand), 272L)
  ap <- .allpairs(cand)
  expect_setequal(ap$covered, ap$need)
  expect_lt(nrow(ap$rows), 30L)
  set <- ap$rows
  for (i in seq_len(nrow(set))) {
    cs <- as.list(set[i, ])
    expect_identical(.vr_check(cs), character(0), label = paste(unlist(cs), collapse = " / "))
  }
})

test_that("FT-TB780 [FR-TB608 FR-TB609]: decision table -- by_value with max_rows on values that come back later, with and without page_by", {
  df <- .vr_data()
  read <- function(p) vapply(p, function(pg) {
    r <- .vr_rows(pg, df)
    paste(c(if (r$cont[1L]) "Cont", r$rec[!is.na(r$rec)]), collapse = " ")
  }, "")
  p1 <- as_rtftables(df, split = "by_value", max_rows = 3)
  expect_identical(unname(read(p1)),
                   c("1 2 3", "Cont 10", "Cont 11 12", "4 5 6", "Cont 7 13", "Cont 14",
                     "Cont 15 16", "8 9", "Cont 17 18"))
  expect_identical(sub("[.]{3}[0-9]+$", "", names(p1)), rep(c("A", "B", "C"), c(3, 4, 2)))
  p2 <- as_rtftables(df, split = "by_value", max_rows = 2, page_by = "period", drop_cols = "period")
  expect_identical(unname(read(p2)),
                   c("1 2", "Cont 3", "10 11", "Cont 12", "4 5", "Cont 6", "Cont 7",
                     "13 14", "Cont 15", "Cont 16", "8 9", "17 18"))
  by <- vapply(p2, function(pg) attr(pg$data, "rtf_paginate_meta")$page_by %||% "", "")
  expect_identical(unname(by), c("P1", "P1", "P2", "P2", "P1", "P1", "P1", "P2", "P2", "P2", "P1", "P2"))
})

# A page read back as one string: per row its record ("labN" -> N), else
# the row's first cell (a stub label row, a continuation row).
.vr_read <- function(p) {
  unname(vapply(p, function(pg) {
    txt <- apply(pg$data, 1L, function(r) paste(ifelse(is.na(r), "", r), collapse = "|"))
    paste(ifelse(grepl("lab[0-9]+", txt), sub(".*lab([0-9]+).*", "\\1", txt),
                 trimws(sub("\\|.*", "", txt))), collapse = " ")
  }, ""))
}

test_that("FT-TB781 [FR-TB150 FR-TB610]: decision table -- a first_rows cut inside a group opens the next page with its continuation row", {
  df <- data.frame(grp = rep(c("A", "B"), c(3, 4)), label = paste0("lab", 1:7),
                   val = as.character(1:7), stringsAsFactors = FALSE)
  pg <- function(...) .vr_read(as_rtftables(df, max_rows = 5, first_rows = 2, ...))
  expect_identical(pg(split = "group_force"), c("1 2", "A (Cont.) 3 4 5", "B (Cont.) 6 7"))
  expect_identical(pg(split = "group_force", stub = c("grp", "label")),
                   c("A 1", "A (Cont.) 2 3", "B 4 5 6 7"))
  expect_identical(pg(split = "group_safe"), c("1 2", "A (Cont.) 3", "4 5 6 7"))
  expect_identical(pg(split = "group_safe", stub = c("grp", "label")),
                   c("A 1", "A (Cont.) 2 3", "B 4 5 6 7"))
  # no continuation row: a cut at a group's end, split = "none", max_rows = 1
  expect_identical(.vr_read(as_rtftables(df, split = "group_safe", max_rows = 5, first_rows = 3)),
                   c("1 2 3", "4 5 6 7"))
  expect_identical(.vr_read(as_rtftables(df, first_rows = 2)), c("1 2", "3 4 5 6 7"))
  p1 <- .vr_read(as_rtftables(df, split = "group_force", max_rows = 1, first_rows = 2))
  expect_identical(p1, c("1 2", "3", "4", "5", "6", "7"))
})

test_that("FT-TB782 [FR-TB596 FR-TB611]: decision table -- by_value with a stub cuts a value's page at max_rows", {
  df <- data.frame(grp = rep(c("A", "B"), c(6, 2)), label = paste0("lab", 1:8),
                   period = rep(c("P1", "P2"), 4), val = as.character(1:8),
                   stringsAsFactors = FALSE)
  bv <- function(...) as_rtftables(df, split = "by_value", stub = c("grp", "label"), ...)
  p0 <- bv(drop_cols = "period")
  expect_identical(.vr_read(p0), c("A 1 2 3 4 5 6", "B 7 8"))
  expect_identical(names(p0), c("A", "B"))
  p3 <- bv(max_rows = 3, drop_cols = "period")
  expect_identical(.vr_read(p3), c("A 1 2", "A (Cont.) 3 4", "A (Cont.) 5 6", "B 7 8"))
  expect_identical(names(p3), c("A...1", "A...2", "A...3", "B"))
  expect_true(all(vapply(p3, function(p) nrow(p$data), 0L) <= 3L))
  pb <- bv(max_rows = 3, page_by = "period", drop_cols = "period")
  expect_identical(.vr_read(pb), c("A 1", "A (Cont.) 3 5", "A 2", "A (Cont.) 4 6", "B 7", "B 8"))
  by <- vapply(pb, function(p) attr(p$data, "rtf_paginate_meta")$page_by %||% "", "")
  expect_identical(unname(by), c("P1", "P1", "P2", "P2", "P1", "P2"))
  p1 <- bv(max_rows = 1, drop_cols = "period")
  expect_identical(.vr_read(p1), c("A", as.character(1:6), "B", "7", "8"))
})

.vl_data <- function() {
  data.frame(SUBJ = rep(c("S01", "S02", "S03"), c(3, 2, 3)),
             SEQ = paste0("E", 1:8),
             TERM = c("Headache", "Nausea and vomiting at night", "Rash",
                      "Fatigue and long lasting weakness", "Cough",
                      "Pain in the lower back", "Fever", "Dry mouth"),
             stringsAsFactors = FALSE)
}

.vl_check <- function(cs) {
  src <- .vl_data()
  sp <- listing_spec(list(listing_col("SUBJ", collapse_repeats = cs$collapse == "yes"),
                          listing_col("SEQ"),
                          listing_col("TERM", width = if (cs$wrap == "yes") 12, layout = "flow")),
                     blank_row = cs$blank_row == "yes",
                     blank_row_first = cs$blank_row_first == "yes")
  rec_rows <- tabulate(build_listing(src, sp)$.rtf_record, 8L)
  args <- list(src, listing = sp)
  mr <- if (cs$max_rows == "none") NA_integer_ else as.integer(cs$max_rows)
  if (!is.na(mr)) args$max_rows <- mr
  if (cs$split == "group_force") args$split <- "group_force"
  count <- cs$count == "yes"
  if (count) args$count_blank_rows <- TRUE
  fr <- switch(cs$first_rows, none = NA_integer_,
               end = sum(rec_rows[1:2]), inside = sum(rec_rows[1:2]) + 1L)
  # the counted separators after records 1 and 2 are among the first rows
  if (!is.na(fr) && count && cs$blank_rows == "yes") fr <- fr + if (cs$first_rows == "end") 1L else 2L
  if (!is.na(fr)) args$first_rows <- fr
  if (cs$blank_rows == "yes") args$blank_rows <- "between_groups"
  pages <- do.call(as_rtftables, args)
  bad <- character(0)
  # each row read back: its record, carried down from the row that prints
  # the record's number
  cur <- 0L
  rows <- lapply(pages, function(pg) {
    d <- pg$data
    cell <- function(j) { v <- as.character(d[[j]]); v[is.na(v)] <- ""; v }
    seq <- cell("SEQ")
    rec <- integer(nrow(d))
    for (k in seq_len(nrow(d))) {
      if (nzchar(seq[k])) cur <<- as.integer(sub("E", "", seq[k]))
      rec[k] <- cur
    }
    list(rec = rec, subj = cell("SUBJ"), seq = seq, term = cell("TERM"))
  })
  # the continuation row: the first row of a page that opens inside a
  # record (text rows of the last record of the page before still to come),
  # under a split by max_rows; any other page opens on a record's first row.
  # A record whose page ends on its last text row is not continued, and its
  # blank row is dropped (#859), under a split by max_rows.
  seen <- integer(8L)
  rec_text <- rec_rows - (cs$blank_row == "yes" && !is.na(mr))
  want_rows <- rec_rows
  for (i in seq_along(rows)) {
    r <- rows[[i]]
    r$cont <- rep(FALSE, length(r$rec))
    empty <- !nzchar(r$subj) & !nzchar(r$seq) & !nzchar(r$term)
    last <- if (i > 1L) utils::tail(rows[[i - 1L]]$rec, 1L) else 0L
    inside <- last > 0L && seen[last] < rec_text[last]
    if (inside && !is.na(mr)) {
      if (!empty[1L]) bad <- c(bad, sprintf("no cont page %d", i)) else r$cont[1L] <- TRUE
    } else if (!inside && !nzchar(r$seq[1L])) {
      bad <- c(bad, sprintf("cont page %d", i))
    }
    seen <- seen + tabulate(r$rec[!r$cont], 8L)
    k <- utils::tail(r$rec, 1L)
    if (i < length(rows) && rec_text[k] < rec_rows[k] && seen[k] == rec_text[k]) {
      want_rows[k] <- rec_text[k]
    }
    rows[[i]] <- r
  }
  # every row of every record once, in order: its TERM words, its number
  if (!identical(seen, want_rows)) bad <- c(bad, "record rows")
  words <- function(x) unlist(strsplit(x[nzchar(x)], " +"))
  if (!identical(unlist(lapply(rows, function(r) words(r$term))), words(src$TERM))) {
    bad <- c(bad, "words")
  }
  if (!identical(unlist(lapply(rows, function(r) r$seq[nzchar(r$seq)])), src$SEQ)) {
    bad <- c(bad, "records")
  }
  # the rows a page counts: its rows and, when blank rows count, the blanks
  # between them and the top edge (unless it merges with an empty first
  # row); first_rows counts the rows, the top edge on top
  body_n <- vapply(pages, function(p) {
    nrow(p$data) + if (count) sum(p$blank_rows > 0L) else 0L
  }, 0L)
  n <- body_n + vapply(seq_along(pages), function(i) {
    r <- rows[[i]]
    top_empty <- !nzchar(r$subj[1L]) && !nzchar(r$seq[1L]) && !nzchar(r$term[1L])
    as.integer(count && 0L %in% pages[[i]]$blank_rows && !top_empty)
  }, 0L)
  if (!is.na(fr) && body_n[1L] != fr) bad <- c(bad, "first_rows")
  if (!is.na(mr)) {
    if (any((if (!is.na(fr)) n[-1L] else n) > mr)) bad <- c(bad, "budget")
  } else if (length(pages) != 1L + !is.na(fr)) {
    bad <- c(bad, "pages of none")
  }
  # the default split (group_safe) never cuts a record that fits a page
  # alone, but the one first_rows cuts
  if (!is.na(mr) && cs$split == "default") {
    edge <- if (count && cs$blank_row_first == "yes") 1L else 0L
    pg_of <- rep(seq_along(rows), vapply(rows, function(r) length(r$rec), 0L))
    rc <- unlist(lapply(rows, function(r) ifelse(r$cont, NA, r$rec)))
    for (k in seq_len(8L)) {
      cut <- length(unique(pg_of[!is.na(rc) & rc == k])) > 1L
      by_fr <- !is.na(fr) && k == 3L && cs$first_rows == "inside"
      if (cut && rec_rows[k] + edge <= mr && !by_fr) bad <- c(bad, paste("safe cut", k))
    }
  }
  for (i in seq_along(rows)) {
    r <- rows[[i]]
    # the subject: on each record's first row; with collapse_repeats at the
    # page's first record row and where it changes
    want <- nzchar(r$seq)
    if (cs$collapse == "yes") {
      s <- src$SUBJ[r$rec]
      k <- which(!r$cont)
      want <- rep(FALSE, length(s))
      want[k] <- c(TRUE, s[k][-1L] != utils::head(s[k], -1L))
    }
    if (!identical(nzchar(r$subj), want)) bad <- c(bad, sprintf("subj page %d", i))
    # blank rows: at the top (blank_row_first), between two records
    want_b <- c(if (cs$blank_row_first == "yes") 0L,
                if (cs$blank_rows == "yes") which(utils::head(r$rec, -1L) != r$rec[-1L]))
    if (!identical(sort(as.integer(pages[[i]]$blank_rows)), as.integer(want_b))) {
      bad <- c(bad, sprintf("blanks page %d", i))
    }
  }
  # the RTF: a page per page, each with its records' numbers and TERM
  # words in order
  txt <- .render_to_string(rtf_tables(rtf_document(), pages))
  body <- sub("^.*?\\\\trowd", "\\\\trowd", txt)
  pg_txt <- strsplit(body, "\\\\sect(?![a-z])|\\\\page(?![a-z])", perl = TRUE)[[1L]]
  pg_txt <- pg_txt[grepl("\\cell", pg_txt, fixed = TRUE)]
  if (length(pg_txt) != length(pages)) {
    bad <- c(bad, "rtf pages")
  } else {
    for (i in seq_along(pages)) {
      got <- regmatches(pg_txt[i], gregexpr("\\bE[0-9]\\b", pg_txt[i]))[[1L]]
      if (!identical(got, rows[[i]]$seq[nzchar(rows[[i]]$seq)])) bad <- c(bad, sprintf("rtf page %d", i))
      at <- 0L
      for (w in words(rows[[i]]$term)) {
        m <- regexpr(w, substring(pg_txt[i], at + 1L), fixed = TRUE)
        if (m < 0L) {
          bad <- c(bad, sprintf("rtf words page %d", i))
          break
        }
        at <- at + m + nchar(w) - 1L
      }
    }
  }
  bad
}

test_that("FT-TB783 [FR-LS110 FR-LS156 FR-PL952 FR-TB150 FR-TB576 FR-TB610 FR-TB612 FR-TB613]: pairwise -- listing x split x max_rows x first_rows x count_blank_rows x blank_rows x blank_row x blank_row_first x wrap x collapse_repeats", {
  cand <- expand.grid(split = c("default", "group_force"), max_rows = c("none", "4", "7"),
                      first_rows = c("none", "end", "inside"), count = c("no", "yes"),
                      blank_rows = c("no", "yes"), blank_row = c("no", "yes"),
                      blank_row_first = c("no", "yes"), wrap = c("no", "yes"),
                      collapse = c("no", "yes"), stringsAsFactors = FALSE)
  cand <- cand[!(cand$split == "group_force" & cand$max_rows == "none"), ]
  expect_identical(nrow(cand), 960L)
  ap <- .allpairs(cand)
  expect_setequal(ap$covered, ap$need)
  expect_lt(nrow(ap$rows), 20L)
  set <- ap$rows
  for (i in seq_len(nrow(set))) {
    cs <- as.list(set[i, ])
    expect_identical(.vl_check(cs), character(0), label = paste(unlist(cs), collapse = " / "))
  }
})

test_that("FT-TB972 [FR-TB613]: a cut just before a listing record's blank row drops that blank row, with no continuation row", {
  rd <- function(p) vapply(p, function(x) {
    d <- x$data
    paste(apply(d, 1L, function(r) {
      r[is.na(r)] <- ""
      if (all(!nzchar(r))) "_" else paste(r[nzchar(r)], collapse = " ")
    }), collapse = " | ")
  }, "")
  df <- data.frame(SUBJ = c("S01", "S02", "S03"), TERM = c("Headache", "Rash", "Cough"))
  sp <- listing_spec(c("SUBJ", "TERM"), spacer = FALSE, blank_row_first = FALSE)
  # first_rows cuts record 1 before its blank row (the issue's example)
  expect_identical(rd(as_rtftables(df, listing = sp, max_rows = 4, first_rows = 1)),
                   c("S01 Headache", "S02 Rash | _ | S03 Cough | _"))
  # split = "group_force" cuts record 2 before its blank row
  expect_identical(rd(as_rtftables(df, listing = sp, max_rows = 3, split = "group_force")),
                   c("S01 Headache | _ | S02 Rash", "S03 Cough | _"))
  # a record longer than a page under the default split
  long <- data.frame(SUBJ = c("S01", "S02"), TERM = c("a b c", "d"))
  spl <- listing_spec(list("SUBJ", listing_col("TERM", width = 1, layout = "flow")),
                      spacer = FALSE, blank_row_first = FALSE)
  expect_identical(rd(as_rtftables(long, listing = spl, max_rows = 3)),
                   c("S01 a | b | c", "S02 d | _"))
  # the key value carried into the blank row (collapse_repeats) still reads
  # as blank, and blank_row_first opens the next page alone
  df2 <- data.frame(SUBJ = c("S01", "S01", "S02"), TERM = c("Headache", "Rash", "Cough"))
  sp2 <- listing_spec(list(listing_col("SUBJ", collapse_repeats = TRUE), "TERM"),
                      spacer = FALSE, blank_row_first = TRUE)
  p <- as_rtftables(df2, listing = sp2, max_rows = 3, split = "group_force")
  expect_identical(rd(p), c("S01 Headache | _ | Rash", "S02 Cough | _"))
  expect_identical(as.integer(p[[2L]]$blank_rows), 0L)
  # a cut that leaves text rows of the record still continues it (the
  # continuation row is empty, the record column hidden)
  expect_identical(rd(as_rtftables(long, listing = spl, max_rows = 3, first_rows = 1)),
                   c("S01 a", "_ | b | c", "S02 d | _"))
})

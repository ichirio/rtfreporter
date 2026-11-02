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
#    first_rows   none, at the end of group A (3 rows; 4 with the stub)
#    page_by      none, "period" (dropped from the print)
#    blank_rows   none, "between_groups"
#  Constraints: the group splits need max_rows; first_rows not with page_by
#  or by_value (FR-TB151).  Left out: by_value + stub + max_rows (#857,
#  the pages are not cut) and first_rows inside a group (#856, no
#  continuation row on the next page).  200 valid combinations;
#  .allpairs() picks the rows covering every pair.  Oracle, by invariants:
#  every record once, in the expected order; each page one period
#  (page_by), one group (by_value); no page over max_rows (the first one
#  first_rows exactly); the continuation rows exactly where a page opens
#  inside a group; group_safe keeps a group that fits; split = "none" gives
#  one page per partition (and the first_rows page); under the stub every
#  record below its group's label; the blank rows; the value widths; the RTF
#  holds each page's records in order and its continuation rows.
#  FR-TB121 FR-TB137 FR-TB138 FR-TB150 FR-TB596 FR-PL433 FR-PL434 FR-IX262
#  FR-TB608 FR-TB609.
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
  fr <- if (stub) 4L else 3L
  if (cs$first_rows == "yes") args$first_rows <- fr
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
    chk <- if (cs$first_rows == "yes") n[-1L] else n
    if (any(chk > as.integer(cs$max_rows))) bad <- c(bad, "budget")
  }
  if (cs$first_rows == "yes" && n[1L] != fr) bad <- c(bad, "first_rows")
  if (cs$split == "none") {
    np <- (if (cs$page_by == "yes") 2L else 1L) + (cs$first_rows == "yes")
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
    has <- r$cont[1L]
    if (!identical(has, inside)) bad <- c(bad, sprintf("cont page %d", i))
    if (sum(r$cont) > 1L || any(r$cont[-1L])) bad <- c(bad, sprintf("cont rows page %d", i))
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
          !(cs$first_rows == "yes" && g == "A" && grepl("P1", u))) {
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

test_that("FT-TB779 [FR-TB121 FR-TB137 FR-TB138 FR-TB150 FR-TB596 FR-PL433 FR-PL434 FR-IX262 FR-TB608 FR-TB609]: pairwise -- split x max_rows x stub x cell_format x first_rows x page_by x blank_rows", {
  cand <- expand.grid(split = c("none", "group_safe", "group_force", "by_value"),
                      max_rows = c("none", "3", "5"), stub = c("no", "yes"),
                      cell_format = c("no", "yes"), first_rows = c("no", "yes"),
                      page_by = c("no", "yes"), blank_rows = c("no", "yes"),
                      stringsAsFactors = FALSE)
  ok <- !(cand$split %in% c("group_safe", "group_force") & cand$max_rows == "none") &
    !(cand$first_rows == "yes" & (cand$page_by == "yes" | cand$split == "by_value")) &
    !(cand$split == "by_value" & cand$stub == "yes" & cand$max_rows != "none")
  cand <- cand[ok, ]
  expect_identical(nrow(cand), 200L)
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

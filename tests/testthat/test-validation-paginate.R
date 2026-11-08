# Validation tests: pagination -- the column axis (paginate_cols(),
# plan_paginate_cols()), the row axis (plan_paginate_rows(), page_by) and the
# order the two give the pages in -- decision tables and pairwise sets, each
# checked on the pages and on the RTF written from them.  Helpers prefixed
# .vp_.  Fictional data only.
#
# ---------------------------------------------------------------------------
#  Design
# ---------------------------------------------------------------------------
#  The rules, as read from the code (R/paginate_cols.R paginate_cols.list(),
#  .resolve_col_blocks(), .resolve_carry_cols(), .check_span_breaks(),
#  .col_page_share(), .col_page_reference(), .resolve_page_axes();
#  R/paginate.R .split_group_safe(), .split_group_force(),
#  .compute_group_info(), .paginate_by_page(); R/table_plan.R
#  plan_paginate_rows(), plan_paginate_cols(), .every_at()):
#
#    blocks    `at` cuts before the columns named; `every = k` cuts every k
#              columns not kept; `cut_by` is a list of blocks or a separator
#              in the names (each run of one key a block).  The kept columns
#              (`keep`, by default the row title, column 1) are taken out of
#              every block and printed at their own place on every page.  A
#              table no wider than one `every` block is returned as it is.
#              Nothing is cut from the writable width: the caller says where.
#    widths    absolute column_widths_twips are carried as they are.  Relative
#              ones (given, or the default weights) are put on page 1's scale
#              (fit = TRUE: page 1 fills the writable width and a ratio is
#              the same width on every page; a block of more ratio warns) or
#              keep the width they had in the whole table (fit = FALSE).
#    spanners  a spanning header cell is clipped to each page's columns;
#              allow_span_break = FALSE stops when a cut falls strictly
#              inside one.
#    rows      max_rows is a budget of BODY rows (titles and footnotes take
#              none of it; the page's height is not measured).
#              "group_safe" packs whole groups and cuts a group longer than
#              the budget with a continuation row, packing what follows onto
#              its tail; "group_force" cuts every max_rows rows.  The
#              continuation row ("<group><cont_label>") is the first row of
#              the next page and counts in its budget.  min_group_rows
#              (default 2) moves a group whose first page would show fewer
#              than that many rows after its first one, and pulls a cut back
#              so the tail has that many.  page_by partitions first.
#    order     the pages sort on the axes outermost first: "across" =
#              group > cols > rows (the row pages of one column block come
#              one after the other), "down" = group > rows > cols.
#
#  Written RTF: the pages are split at \page and \sect; on each page the
#  cell texts (unique per row and column) are read back in order, so a page's
#  rows and columns are checked in the file, and the cell widths from the
#  \cellx of its last row.
#
#  (A) FT-TB701, pairwise, the column axis.  Factors: cut (fits: every = 8,
#  every = 3, at, a list, a separator) x keep (default, none, two columns) x
#  spanners (none; one labelled cell over each block; the same with the
#  first two merged, across the first cut) x allow_span_break (TRUE, FALSE)
#  x widths (default, relative, twips) x fit (TRUE, FALSE) x row pages (1, 2)
#  x order (across, down).  2016 valid combinations (a spanner across a cut
#  needs a cut); the greedy .allpairs() rows.  Oracle: the blocks from the
#  rules above; each page its kept columns and its block, its rows; the
#  widths; each spanner once on every page that has one of its columns; a
#  stop for a spanner across a cut under allow_span_break = FALSE; a warning
#  when fit = TRUE meets a block of more ratio than page 1.  FR-TB273
#  FR-TB274 FR-TB275 FR-TB276 FR-TB277 FR-TB278 FR-TB279 FR-TB281 FR-TB282
#  FR-TB289 FR-TB560.
#
#  (B) FT-PL926, decision table, the row split.  Groups A (3 rows), B (5),
#  C (2), D (4): split (group_safe, group_force) x budget (6: every group
#  fits; 4: B is longer than a page) x min_group_rows (0, 2), plus "none" and
#  "rows".  10 rows, the pages worked out by hand.  FR-PL433 FR-PL434
#  FR-PL435 FR-IX262 FR-TB121 FR-TB138 FR-PL920.
#
#  (C) FT-PL927, pairwise, the row axis.  Factors: split (group_safe,
#  group_force) x max_rows (3, 5, 8; the longest group is 5) x min_group_rows
#  (0, 2) x cont_label (default, " - continued") x blank rows (none, between
#  groups) x page_by (none, a period) x titles and footnotes (none, two of
#  each).  192 combinations.  Oracle, by invariants:
#  every record once, in order, each page inside one period; no page over
#  the budget; a page that opens inside a group
#  opens with its continuation row and no other page has one; group_safe
#  never cuts a group that fits; group_force with no widow control fills
#  every page but a period's last; the blank rows sit where the group
#  changes; titles and footnotes change no page.  FR-PL433 FR-PL435 FR-PL436
#  FR-PL437 FR-PL192 FR-TB137 FR-IX262 FR-PL919.
#
#  (D) FT-TB702, decision table, the three axes at once.  A value split
#  (GRP: G1, G2) inside page_by pages (PER: G1 in P1 and P2, G2 in P1), so
#  three row pages, x two column blocks, under order "across", "down", the
#  six orders of the three axes and "cols" alone (the rest appended): the
#  page sequence is the sort on the axes, outermost first, in the pages and
#  in the file; the pages are the same.  Then page_by pages alone (the rows
#  axis) and plan_paginate_rows() + plan_paginate_cols(order = ).  FR-TB279
#  FR-PL461 FR-PL467 FR-TB280.
#
#  (E) The cases #824 fixed, one named test each: FT-PL928 counted blank rows
#  with the group splits (the groups read as without them, with group_col
#  NULL too; a separator costs a row only between two groups on one page, so
#  a group that fits is not cut and no "A (Cont.)" opens B's page); FT-PL929
#  no between_groups blank after a continuation row; FT-TB703 a plain value
#  split is the group axis of order; FT-TB704 allow_span_break = FALSE skips
#  the empty filler cell; FT-TB705 "across" / "down" as the help now words
#  them; FT-PL930 min_group_rows counts the rows after a group's first.
# ---------------------------------------------------------------------------

# ---- helpers -----------------------------------------------------------------

.VP_W <- 13680L                                  # writable width, landscape Letter
# A record id ("r07") as a cell text, not inside a control word (\margr1080)
.VP_REC <- "(?<![A-Za-z0-9])r[0-9]{2}(?![0-9])"

# Render pages to RTF text, cut at the page and section breaks.
.vp_pages_rtf <- function(pages, ...) {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(rtf_tables(rtf_document(), pages, ...), f, overwrite = TRUE)
  x <- paste(readLines(f, warn = FALSE), collapse = "\n")
  strsplit(x, "\\\\page(?![a-zA-Z])|\\\\sect(?![a-zA-Z])", perl = TRUE)[[1L]]
}

.vp_tok <- function(x, rx) regmatches(x, gregexpr(rx, x, perl = TRUE))[[1L]]

# The cell widths of the last row of a page of RTF.
.vp_widths <- function(chunk) {
  rows <- .vp_tok(chunk, "\\\\trowd[^\n]*")
  cx <- as.integer(sub("\\\\cellx", "", .vp_tok(rows[length(rows)], "\\\\cellx[0-9]+")))
  diff(c(0L, cx))
}

# ---- (A) pairwise: the column axis ------------------------------------------

.VP_NM <- c("P", "Q", "A.1", "A.2", "B.1", "B.2", "C.1", "C.2")
.VP_REL <- c(2, 1, 1, 1, 2, 2, 1, 3)
.VP_TW <- c(1400L, 1000L, 900L, 900L, 1200L, 1200L, 1000L, 1600L)

.vp_wide <- function(rows) {
  d <- as.data.frame(lapply(.VP_NM, function(nm) paste0(nm, "_r", rows)), stringsAsFactors = FALSE)
  names(d) <- .VP_NM
  d
}

# The column blocks the rules give, kept columns taken out.
.vp_blocks <- function(cut, keep) {
  n <- length(.VP_NM)
  rest <- setdiff(seq_len(n), keep)
  b <- switch(cut,
    fits  = list(rest),
    every = split(rest, ceiling(seq_along(rest) / 3)),
    at    = list(1:4, 5:6, 7:8),
    list  = list(2:4, 5:8),
    sep   = {
      key <- sub("[.].*", "", .VP_NM)
      key[keep] <- NA
      k <- which(!is.na(key))
      split(k, cumsum(c(TRUE, key[k][-1L] != key[k][-length(k)])))
    })
  b <- lapply(unname(b), function(x) setdiff(x, keep))
  Filter(length, b)
}

.vp_cut_args <- function(cut) {
  switch(cut,
    fits  = list(every = 8),
    every = list(every = 3),
    at    = list(at = c("B.1", "C.1")),
    list  = list(cut_by = list(c("Q", "A.1", "A.2"), c("B.1", "B.2", "C.1", "C.2"))),
    sep   = list(cut_by = "."))
}

test_that("FT-TB701 [FR-TB273 FR-TB274 FR-TB275 FR-TB276 FR-TB277 FR-TB278 FR-TB279 FR-TB281 FR-TB282 FR-TB289 FR-TB560]: pairwise -- cut x keep x spanner x allow_span_break x widths x fit x row pages x order", {
  cand <- expand.grid(cut = c("fits", "every", "at", "list", "sep"), keep = c("default", "none", "two"),
                      span = c("none", "within", "across"), allow = c(TRUE, FALSE),
                      widths = c("default", "rel", "twips"), fit = c(TRUE, FALSE),
                      rowpages = c(1L, 2L), order = c("across", "down"), stringsAsFactors = FALSE)
  cand <- cand[!(cand$span == "across" & cand$cut == "fits"), ]
  ap <- .allpairs(cand)
  expect_setequal(ap$covered, ap$need)
  expect_lt(nrow(ap$rows), nrow(cand) / 50)
  set <- ap$rows
  for (i in seq_len(nrow(set))) {
    r <- set[i, ]
    label <- paste(r$cut, r$keep, r$span, r$allow, r$widths, r$fit, r$rowpages, r$order, sep = " / ")
    keep <- switch(r$keep, default = 1L, none = integer(0), two = 1:2)
    blocks <- .vp_blocks(r$cut, keep)
    # the spanners: one labelled cell over each block ("S1", "S2", ...); across
    # a cut, the first one over the first two blocks
    spans <- if (r$span == "none") list() else blocks
    if (r$span == "across") spans <- c(list(c(spans[[1L]], spans[[2L]])), spans[-(1:2)])
    hdr <- if (!length(spans)) NULL else
      list(lapply(seq_along(spans), function(s) col_cell(range(spans[[s]]), paste0("S", s))), .VP_NM)
    rows <- if (r$rowpages == 1L) list(1:2) else list(1:2, 3:4)
    mk <- function(rr) {
      a <- list(.vp_wide(rr), col_header = hdr)
      if (r$widths == "rel") a$col_rel_width <- .VP_REL
      if (r$widths == "twips") a$column_widths_twips <- .VP_TW
      do.call(rtftable, a)
    }
    x <- lapply(rows, mk)
    args <- c(list(x), .vp_cut_args(r$cut), list(allow_span_break = r$allow, fit = r$fit, order = r$order))
    if (r$keep != "default") args$keep <- keep
    if (r$span == "across" && !r$allow) {
      expect_error(do.call(paginate_cols, args), "would break the spanning header cell 'S1'", label = label)
      next
    }
    w <- if (r$widths == "default") .default_table_weights(x[[1L]]) else .VP_REL
    cols <- lapply(blocks, function(b) sort(c(keep, b)))
    over <- r$fit && r$cut != "fits" && r$widths != "twips" &&
      any(vapply(cols, function(k) sum(w[k]), 0) > sum(w[cols[[1L]]]) + 1e-9)
    warned <- character(0)
    pg <- withCallingHandlers(do.call(paginate_cols, args),
                              warning = function(e) {
                                warned <<- c(warned, conditionMessage(e))
                                invokeRestart("muffleWarning")
                              })
    expect_identical(any(grepl("more than block 1", warned)), over, label = label)
    # the page sequence: (row page, block) sorted by the axes
    seq_rb <- expand.grid(b = seq_along(cols), r = seq_along(rows))
    seq_rb <- if (r$order == "across") seq_rb[order(seq_rb$b, seq_rb$r), ] else seq_rb[order(seq_rb$r, seq_rb$b), ]
    expect_length(pg, nrow(seq_rb))
    chunks <- .vp_pages_rtf(pg)
    expect_length(chunks, nrow(seq_rb))
    for (k in seq_len(nrow(seq_rb))) {
      kc <- cols[[seq_rb$b[k]]]
      rr <- rows[[seq_rb$r[k]]]
      lab_k <- paste(label, "page", k)
      expect_identical(names(pg[[k]]$data), .VP_NM[kc], label = lab_k)
      want <- as.vector(t(outer(rr, kc, function(a, b) paste0(.VP_NM[b], "_r", a))))
      expect_identical(.vp_tok(chunks[k], "(?:P|Q|[ABC][.][12])_r[0-9]"), want, label = lab_k)
      # the widths in the file
      want_w <- if (r$widths == "twips") {
        .VP_TW[kc]
      } else if (r$cut == "fits" || !r$fit) {
        w[kc] / sum(w) * .VP_W
      } else {
        w[kc] * .VP_W / sum(w[cols[[1L]]])
      }
      expect_true(all(abs(.vp_widths(chunks[k]) - want_w) <= 3), label = paste(lab_k, "widths"))
      # the spanners: each once over the page's share of its columns
      on_page <- which(vapply(spans, function(sp) any(kc %in% sp), TRUE))
      expect_identical(.vp_tok(chunks[k], "\\bS[0-9]\\b"), sprintf("S%d", on_page),
                       label = paste(lab_k, "spanners"))
    }
  }
})

# ---- (B) decision table: the row split ---------------------------------------

.vp_recs <- function() {
  data.frame(G = rep(c("A", "B", "C", "D"), c(3, 5, 2, 4)), R = sprintf("r%02d", 1:14),
             stringsAsFactors = FALSE)
}

# A page as text: its records, a continuation row as "<group>+".
.vp_lay <- function(pg, cont = " (Cont.)") {
  paste(vapply(pg, function(p) {
    d <- p$data
    paste(ifelse(nzchar(d$R), d$R, paste0(sub(cont, "", d$G, fixed = TRUE), "+")), collapse = " ")
  }, ""), collapse = " | ")
}

test_that("FT-PL926 [FR-PL433 FR-PL434 FR-PL435 FR-IX262 FR-TB121 FR-TB138 FR-PL920]: decision table -- split x a group longer than the budget x min_group_rows", {
  dt <- data.frame(
    split = c("none", "rows", rep(c("group_safe", "group_force"), each = 4)),
    max   = c(NA, NA, rep(c(6, 6, 4, 4), 2)),
    mgr   = c(NA, NA, rep(c(0, 2), 4)),
    want  = c(
      "r01 r02 r03 r04 r05 r06 r07 r08 r09 r10 r11 r12 r13 r14",
      "r01 r02 r03 | r04 r05 r06 r07 r08 | r09 r10 r11 r12 r13 r14",
      # group_safe, 6: every group fits; C and D share a page
      "r01 r02 r03 | r04 r05 r06 r07 r08 | r09 r10 r11 r12 r13 r14",
      "r01 r02 r03 | r04 r05 r06 r07 r08 | r09 r10 r11 r12 r13 r14",
      # group_safe, 4: B is cut; C packs onto its tail unless the tail is
      # pulled back to two rows
      "r01 r02 r03 | r04 r05 r06 r07 | B+ r08 r09 r10 | r11 r12 r13 r14",
      "r01 r02 r03 | r04 r05 r06 | B+ r07 r08 | r09 r10 | r11 r12 r13 r14",
      # group_force, 6: a cut every 6 rows, the continuation row counted;
      # min_group_rows moves D (its first row alone) to the next page
      "r01 r02 r03 r04 r05 r06 | B+ r07 r08 r09 r10 r11 | D+ r12 r13 r14",
      "r01 r02 r03 r04 r05 r06 | B+ r07 r08 r09 r10 | r11 r12 r13 r14",
      # group_force, 4
      "r01 r02 r03 r04 | B+ r05 r06 r07 | B+ r08 r09 r10 | r11 r12 r13 r14",
      "r01 r02 r03 | r04 r05 r06 | B+ r07 r08 | r09 r10 | r11 r12 r13 r14"),
    stringsAsFactors = FALSE)
  for (i in seq_len(nrow(dt))) {
    r <- dt[i, ]
    label <- paste(r$split, r$max, r$mgr, sep = " / ")
    p <- table_plan(.vp_recs())
    p <- switch(r$split,
      none = plan_paginate_rows(p, split = "none"),
      rows = plan_paginate_rows(p, split = "rows", break_before = c(4, 9)),
      plan_paginate_rows(p, max_rows = r$max, split = r$split, min_group_rows = r$mgr))
    pg <- plan_apply(p)
    if (inherits(pg, "rtftable")) pg <- list(pg)
    expect_identical(.vp_lay(pg), r$want, label = label)
    chunks <- .vp_pages_rtf(pg)
    got <- vapply(chunks, function(ch) {
      paste(sub(" \\(Cont\\.\\)", "+", .vp_tok(ch, paste0(.VP_REC, "|[A-D] \\(Cont\\.\\)"))), collapse = " ")
    }, "", USE.NAMES = FALSE)
    expect_identical(paste(got, collapse = " | "), r$want, label = paste(label, "RTF"))
  }
})

# ---- (C) pairwise: the row axis ----------------------------------------------

test_that("FT-PL927 [FR-PL433 FR-PL435 FR-PL436 FR-PL437 FR-PL192 FR-TB137 FR-IX262 FR-PL919]: pairwise -- split x max_rows x min_group_rows x cont_label x blank rows x page_by x titles", {
  cand <- expand.grid(split = c("group_safe", "group_force"), max = c(3L, 5L, 8L), mgr = c(0L, 2L),
                      cont = c("default", "custom"), blanks = c("none", "between"),
                      by = c("none", "period"), titles = c("none", "set"), stringsAsFactors = FALSE)
  ap <- .allpairs(cand)
  expect_setequal(ap$covered, ap$need)
  expect_lt(nrow(ap$rows), nrow(cand) / 10)
  set <- ap$rows
  d0 <- .vp_recs()
  size <- table(d0$G)
  for (i in seq_len(nrow(set))) {
    r <- set[i, ]
    label <- paste(r$split, r$max, r$mgr, r$cont, r$blanks, r$by, r$titles, sep = " / ")
    cont <- if (r$cont == "default") " (Cont.)" else " - continued"
    d <- if (r$by == "period") cbind(PER = rep(c("P1", "P2"), c(8, 6)), d0) else d0
    build <- function(titles) {
      p <- table_plan(d) |> plan_row_group(group_col = "G")
      a <- list(p, max_rows = r$max, split = r$split, min_group_rows = r$mgr)
      if (r$cont == "custom") a$cont_label <- cont
      if (r$by == "period") a$page_by <- "PER"
      p <- do.call(plan_paginate_rows, a)
      if (r$blanks != "none") p <- plan_blanks(p, where = "between_groups")
      if (titles) {
        p <- p |> plan_titles(c("Example Pharma Table 1", "Safety Population")) |>
          plan_footnotes(c("Note 1: fictional data.", "Note 2: none."))
      }
      pg <- plan_apply(p)
      if (inherits(pg, "rtftable")) list(pg) else pg
    }
    pg <- build(r$titles == "set")
    grp <- function(p) sub(cont, "", p$data$G, fixed = TRUE)
    is_rec <- function(p) nzchar(p$data$R)
    # every record once, in order, each page inside one period
    expect_identical(unname(unlist(lapply(pg, function(p) p$data$R[is_rec(p)]))), d0$R, label = label)
    per <- vapply(pg, function(p) if (r$by == "period") paste(unique(p$data$PER[is_rec(p)]), collapse = ",") else "", "")
    expect_true(all(!grepl(",", per)), label = label)
    for (k in seq_along(pg)) {
      p <- pg[[k]]
      lk <- paste(label, "page", k)
      expect_lte(nrow(p$data), r$max, label = lk)
      # a continuation row opens a page that starts inside a group, and only there
      cont_rows <- which(!is_rec(p))
      first_g <- grp(p)[is_rec(p)][1L]
      inside <- k > 1L && per[k] == per[k - 1L] && {
        q <- pg[[k - 1L]]
        utils::tail(grp(q)[is_rec(q)], 1L) == first_g
      }
      if (inside) {
        expect_identical(cont_rows, 1L, label = lk)
        expect_identical(p$data$G[1L], paste0(first_g, cont), label = lk)
      } else {
        expect_identical(cont_rows, integer(0), label = lk)
      }
      # group_force without widow control fills every page but a period's last
      last_of_period <- k == length(pg) || per[k + 1L] != per[k]
      if (r$split == "group_force" && r$mgr == 0L && !last_of_period) {
        expect_identical(nrow(p$data), r$max, label = lk)
      }
      # the blank rows sit where the group changes, never after a
      # continuation row (#824)
      g <- grp(p)
      want_b <- if (r$blanks == "none") integer(0) else which(g[-1L] != g[-length(g)])
      got_b <- as.integer(p$blank_rows %||% integer(0))
      expect_identical(got_b, as.integer(want_b), label = paste(lk, "blanks"))
    }
    # group_safe never cuts a group that fits
    if (r$split == "group_safe") {
      for (g in names(size)[size <= r$max]) {
        on <- vapply(pg, function(p) any(p$data$R[is_rec(p)] %in% d0$R[d0$G == g]), TRUE)
        expect_identical(sum(on), 1L, label = paste(label, "group", g))
      }
    }
    # titles and footnotes take none of the budget
    if (r$titles == "set") {
      expect_identical(.vp_lay(pg, cont), .vp_lay(build(FALSE), cont), label = paste(label, "titles"))
    }
    # the written pages
    chunks <- .vp_pages_rtf(pg)
    expect_length(chunks, length(pg))
    rx <- paste0(.VP_REC, "|[A-D]", gsub("([().])", "\\\\\\1", cont))
    got <- vapply(chunks, function(ch) paste(sub(cont, "+", .vp_tok(ch, rx), fixed = TRUE), collapse = " "), "",
                  USE.NAMES = FALSE)
    expect_identical(paste(got, collapse = " | "), .vp_lay(pg, cont), label = paste(label, "RTF"))
    if (r$titles == "set") {
      expect_true(all(grepl("Example Pharma Table 1", chunks) & grepl("Note 2: none.", chunks, fixed = TRUE)),
                  label = paste(label, "titles on every page"))
    }
  }
})

# ---- (D) decision table: both axes, the page order ----------------------------

# Three axes: the group GRP (a value split), the period PER (page_by, the rows
# axis) and two column blocks (X, Y).
.vp_both <- function() {
  data.frame(PER = c("P1", "P1", "P2", "P1", "P1"), GRP = rep(c("G1", "G2"), c(3, 2)),
             R = sprintf("r%02d", 1:5), X1 = "x1", X2 = "x2", Y1 = "y1", Y2 = "y2",
             stringsAsFactors = FALSE)
}

# A page as "<first record>:<first column of its block>".
.vp_sig <- function(p) {
  d <- p$data
  paste0(d$R[1L], ":", names(d)[ncol(d) - 1L])
}

test_that("FT-TB702 [FR-TB279 FR-PL461 FR-PL467 FR-TB280]: decision table -- the page order of group, row and column pages under every order", {
  rp <- as_rtftables(.vp_both(), page_by = "PER", split = "by_value", group_col = "GRP")
  # G1 in P1 and P2, G2 in P1: the group outside the period
  expect_identical(vapply(rp, function(p) paste(p$data$R, collapse = " "), "", USE.NAMES = FALSE),
                   c("r01 r02", "r03", "r04 r05"))
  key <- data.frame(group = c(1L, 1L, 2L), rows = c(1L, 2L, 1L))
  orders <- list("across", "down", c("group", "rows", "cols"), c("group", "cols", "rows"),
                 c("rows", "group", "cols"), c("rows", "cols", "group"), c("cols", "group", "rows"),
                 c("cols", "rows", "group"), "cols")
  axes <- list(c("group", "cols", "rows"), c("group", "rows", "cols"),
               c("group", "rows", "cols"), c("group", "cols", "rows"), c("rows", "group", "cols"),
               c("rows", "cols", "group"), c("cols", "group", "rows"), c("cols", "rows", "group"),
               c("cols", "group", "rows"))
  ref <- NULL
  for (i in seq_along(orders)) {
    o <- orders[[i]]
    label <- paste(o, collapse = ",")
    pg <- paginate_cols(rp, every = 2, keep = 1:3, order = o)
    grid <- merge(key, data.frame(cols = 1:2), by = NULL)
    grid$i <- rep(1:3, 2)
    grid <- grid[do.call(order, unname(as.list(grid[axes[[i]]]))), ]
    want <- paste0(c("r01", "r03", "r04")[grid$i], ":", c("X1", "Y1")[grid$cols])
    expect_identical(unname(vapply(pg, .vp_sig, "")), want, label = label)
    # the pages themselves are the same whatever the order
    s <- unname(vapply(pg, function(p) paste(unlist(p$data), collapse = " "), ""))
    if (is.null(ref)) ref <- sort(s) else expect_identical(sort(s), ref, label = label)
    # each column page keeps its row page's name
    expect_identical(sub("[.]{3}[0-9]+$", "", names(pg)), sub("[.]{3}[0-9]+$", "", names(rp))[grid$i],
                     label = label)
    # the written order
    chunks <- .vp_pages_rtf(pg)
    got <- vapply(chunks, function(ch) paste0(.vp_tok(ch, .VP_REC)[1L], ":", .vp_tok(ch, "\\b[XY]1\\b")[1L]), "",
                  USE.NAMES = FALSE)
    expect_identical(got, want, label = paste(label, "RTF"))
  }
  # page_by pages alone are the rows axis: "across" runs them inside a block
  bp <- as_rtftables(.vp_both(), page_by = "PER")
  expect_identical(unname(vapply(paginate_cols(bp, every = 2, keep = 1:3, order = "across"), .vp_sig, "")),
                   c("r01:X1", "r03:X1", "r01:Y1", "r03:Y1"))
  expect_identical(unname(vapply(paginate_cols(bp, every = 2, keep = 1:3, order = "down"), .vp_sig, "")),
                   c("r01:X1", "r01:Y1", "r03:X1", "r03:Y1"))
  # the plan: the rows cut first, then the column blocks in the order asked
  d <- .vp_both()[-1L]
  rows_only <- table_plan(d) |> plan_paginate_rows(max_rows = 3, split = "group_safe") |> plan_apply()
  for (o in c("across", "down")) {
    pg <- table_plan(d) |>
      plan_paginate_rows(max_rows = 3, split = "group_safe") |>
      plan_paginate_cols(every = 2, keep = c("GRP", "R"), order = o) |>
      plan_apply()
    expect_identical(lapply(pg, `[[`, "data"),
                     lapply(paginate_cols(rows_only, every = 2, keep = 1:2, order = o), `[[`, "data"), label = o)
    expect_identical(unname(vapply(pg, .vp_sig, "")),
                     if (o == "across") c("r01:X1", "r04:X1", "r01:Y1", "r04:Y1") else c("r01:X1", "r01:Y1", "r04:X1", "r04:Y1"),
                     label = o)
  }
})

# ---- (E) the cases #824 fixed --------------------------------------------------

# A page as text with its blank rows: "_" after the row a blank follows.
.vp_layb <- function(pg) {
  paste(vapply(pg, function(p) {
    d <- p$data
    s <- ifelse(nzchar(d$R), d$R, paste0(sub(" (Cont.)", "", d$G, fixed = TRUE), "+"))
    b <- p$blank_rows %||% integer(0)
    b <- b[b >= 1L & b <= length(s)]
    s[b] <- paste0(s[b], "_")
    paste(s, collapse = " ")
  }, ""), collapse = " | ")
}

test_that("FT-PL928 [FR-PL433 FR-TB137 FR-TB138 FR-IX324]: counted blank rows with the group splits -- the groups are read as without them, a separator costs a row only between two groups on one page", {
  dt <- data.frame(
    split = rep(c("group_safe", "group_force"), each = 4),
    max   = rep(c(3, 3, 4, 4), 2),
    mgr   = rep(c(0, 2), 4),
    want  = c(
      # group_safe: A (3 rows) fits a page of 3 and is not cut; no "A (Cont.)"
      "r01 r02 r03 | r04 r05 r06 | B+ r07 r08 | r09 r10 | r11 r12 r13 | D+ r14",
      "r01 r02 r03 | r04 r05 r06 | B+ r07 r08 | r09 r10 | r11 r12 | D+ r13 r14",
      "r01 r02 r03 | r04 r05 r06 r07 | B+ r08 | r09 r10 | r11 r12 r13 r14",
      "r01 r02 r03 | r04 r05 r06 | B+ r07 r08 | r09 r10 | r11 r12 r13 r14",
      # group_force: a separator between two groups on a page is counted
      "r01 r02 r03 | r04 r05 r06 | B+ r07 r08 | r09 r10 | r11 r12 r13 | D+ r14",
      "r01 r02 r03 | r04 r05 r06 | B+ r07 r08 | r09 r10 | r11 r12 | D+ r13 r14",
      "r01 r02 r03 | r04 r05 r06 r07 | B+ r08_ r09 | C+ r10_ r11 | D+ r12 r13 r14",
      "r01 r02 r03 | r04 r05 r06 | B+ r07 r08 | r09 r10 | r11 r12 r13 r14"),
    stringsAsFactors = FALSE)
  for (i in seq_len(nrow(dt))) {
    r <- dt[i, ]
    label <- paste(r$split, r$max, r$mgr, sep = " / ")
    for (gc in list("G", NULL)) {
      lab <- paste(label, if (is.null(gc)) "group_col NULL" else "group_col G")
      pg <- as_rtftables(.vp_recs(), split = r$split, max_rows = r$max, group_col = gc,
                         blank_rows = "between_groups", count_blank_rows = TRUE, min_group_rows = r$mgr)
      expect_identical(.vp_layb(pg), r$want, label = lab)
      # every page prints no more than max_rows rows, blanks included
      printed <- vapply(pg, function(p) nrow(p$data) + length(p$blank_rows %||% integer(0)), 0)
      expect_true(all(printed <= r$max), label = lab)
    }
  }
})

test_that("FT-PL929 [FR-PL192 FR-TB137]: between_groups puts no blank row after a continuation row", {
  pg <- as_rtftables(.vp_recs(), split = "group_force", max_rows = 4, group_col = "G",
                     blank_rows = "between_groups", min_group_rows = 0)
  expect_identical(.vp_layb(pg), "r01 r02 r03_ r04 | B+ r05 r06 r07 | B+ r08_ r09 r10 | r11 r12 r13 r14")
  pg <- table_plan(.vp_recs()) |> plan_row_group(group_col = "G") |>
    plan_paginate_rows(max_rows = 4, split = "group_force", min_group_rows = 0) |>
    plan_blanks(where = "between_groups") |> plan_apply()
  expect_identical(.vp_layb(pg), "r01 r02 r03_ r04 | B+ r05 r06 r07 | B+ r08_ r09 r10 | r11 r12 r13 r14")
})

test_that("FT-TB703 [FR-TB279]: a plain value split is the group axis of paginate_cols(order = )", {
  d <- .vp_both()[-1L]
  rp <- as_rtftables(d, split = "by_value", group_col = "GRP", max_rows = 2)
  expect_identical(vapply(rp, function(p) attr(p$data, "rtf_paginate_meta")$page_group, ""),
                   c(G1...1 = "G1", G1...2 = "G1", G2 = "G2"))
  sig <- function(o) unname(vapply(paginate_cols(rp, every = 2, keep = 1:2, order = o), function(p) {
    paste0(p$data$GRP[1L], ":", names(p$data)[ncol(p$data) - 1L])
  }, ""))
  # the group outermost: G1's pages, then G2's, whatever the inner order
  expect_identical(sig("across"), c("G1:X1", "G1 (Cont.):X1", "G1:Y1", "G1 (Cont.):Y1", "G2:X1", "G2:Y1"))
  expect_identical(sig("down"), c("G1:X1", "G1:Y1", "G1 (Cont.):X1", "G1 (Cont.):Y1", "G2:X1", "G2:Y1"))
  expect_identical(sig("cols"), c("G1:X1", "G1 (Cont.):X1", "G2:X1", "G1:Y1", "G1 (Cont.):Y1", "G2:Y1"))
})

test_that("FT-TB704 [FR-TB282 FR-TB289]: allow_span_break = FALSE does not stop on the empty cell under the columns no spanner covers", {
  d <- data.frame(ROW = c("a", "b"), A1 = "1", A2 = "2", B1 = "3", B2 = "4", B3 = "5")
  t <- rtftable(d, col_header = list(list(col_cell(2:3, "Span A")), names(d)))
  pg <- paginate_cols(t, at = "B2", allow_span_break = FALSE)
  expect_identical(lapply(pg, function(p) names(p$data)), list(c("ROW", "A1", "A2", "B1"), c("ROW", "B2", "B3")))
  expect_error(paginate_cols(t, at = "A2", allow_span_break = FALSE),
               "would break the spanning header cell 'Span A'")
})

test_that("FT-TB705 [FR-TB279]: order = \"across\" takes one column block after another, \"down\" one row page after another", {
  # two row pages (page_by) x three column blocks, as the help draws them
  d <- data.frame(PER = rep(c("P1", "P2"), each = 2), R = sprintf("r%02d", 1:4),
                  X1 = "x", X2 = "x", Y1 = "y", Y2 = "y", Z1 = "z", Z2 = "z")
  rp <- as_rtftables(d, page_by = "PER")
  sig <- function(o) unname(vapply(paginate_cols(rp, every = 2, keep = 1:2, order = o), function(p) {
    paste0(substr(names(p$data)[3L], 1L, 1L), "/", p$data$PER[1L])
  }, ""))
  expect_identical(sig("across"), c("X/P1", "X/P2", "Y/P1", "Y/P2", "Z/P1", "Z/P2"))
  expect_identical(sig("down"), c("X/P1", "Y/P1", "Z/P1", "X/P2", "Y/P2", "Z/P2"))
})

test_that("FT-PL930 [FR-TB138 FR-PL435]: min_group_rows counts a group's rows after its first row", {
  # B (r04..r08) starts on a page of 5 with two rows, r04 and r05: one after its
  # first row, so min_group_rows = 2 moves it and 1 leaves it
  lay <- function(mgr) {
    .vp_lay(table_plan(.vp_recs()) |>
              plan_paginate_rows(max_rows = 5, split = "group_force", min_group_rows = mgr) |>
              plan_apply())
  }
  expect_identical(lay(2), "r01 r02 r03 | r04 r05 r06 r07 r08 | r09 r10 r11 r12 | D+ r13 r14")
  expect_identical(lay(1), "r01 r02 r03 r04 r05 | B+ r06 r07 r08 | r09 r10 r11 r12 r13 | D+ r14")
})

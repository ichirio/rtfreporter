# Validation tests: plan_append() with pagination and blank rows -- a
# pairwise set over the parts and the pages (plan_paginate_rows(),
# plan_blanks()), a decision table of a page break at a part's boundary, and
# a decision table of a listing (plan_listing()) at a page edge.  Every case
# is checked on the pages (plan_apply()) and on the RTF written.  Helpers
# prefixed .pa_.  Fictional data only.
#
# ---------------------------------------------------------------------------
#  Design
# ---------------------------------------------------------------------------
#  The rules, as read from the help (plan_append(), plan_paginate_rows(),
#  plan_blanks(), plan_listing(), as_rtftables(collapse_repeats = ), the
#  article "A listing and its summary in one table"):
#
#    stack     the parts' rows are stacked, the parts in the order declared,
#              BEFORE the pages are cut; a page_by value is a page of its
#              own and holds every part's rows of that value.
#    heading   a constant row key (rows = c(block = ~ "Summary")) is a
#              heading row above its part's rows, which are indented under
#              it; a heading row is empty in every value column, whatever
#              the part's na.  Without it the part's rows have no heading.
#    na        a cell the part has no value for -- a missing statistic or
#              value, a column the part does not have -- prints the part's
#              own na (default empty); the parent's na is not a child's.
#    pages     max_rows is a budget of body rows; "group_safe" keeps a group
#              (a heading row and its rows) on one page when it fits, and
#              cuts one longer than the page; "group_force" cuts where the
#              budget ends; min_group_rows (default 2) keeps a group from
#              leaving fewer rows than that on either side of a cut.  A
#              group cut is continued on the next page by its heading row
#              with cont_label ("Summary (Cont.)", no cells).
#    blanks    plan_blanks(where = "between_parts") puts a blank row between
#              the parts of each page group (never before the first part,
#              never between page groups).  They are rows of the stack, so
#              they take a place in max_rows.  Unset or where = "none", none.
#    listing   plan_listing(): a record wrapped over several rows is not cut
#              by a page break, it moves whole to the next page; the blank
#              row ends each record and the template puts one at the top of
#              each page; collapse_repeats blanks a key's repeats per page,
#              after the split, so a key shows again at the top of a page.
#
#  Data: records S-01.. (R-01.., Q-01..) of five subjects in group x and
#  four in group y, three time points TP = t1 t2 t3 across; the second
#  record's t2 value is missing; a statistic part has N/Mean/SD, Min/Max/
#  Median or Q1/Q3, the last statistic missing at t2.  Every value tells its
#  part, group, row and column (1000 part + 100 group + 10 row + column
#  + .5), so a cell out of place is found.
#
#  (A) FT-PL985, pairwise, plan_append() then pages.  Factors and levels:
#    nparts   2, 3
#    kinds    tt (table + table), tl (table + listing [+ table]), ls
#             (listing + summary [+ summary]); a "listing" is a records plan
#             (table_plan(value = ): a row a subject), a "table" an ARD of
#             statistics
#    heading  present (every part has a heading row), absent
#    na       default, "NC" (plan_cells(na = ) of every part)
#    columns  same, fewer (the second part has no t3)
#    paging   none, rows (max_rows = 3: a cut inside a part), page_by (a
#             page for each value of GRP, which every part has)
#    split    group_safe, group_force (with rows), - (without)
#    blanks   unset, none, between_parts
#  Constraint: split is given with paging rows and only then.  The product
#  is 576 rows; .allpairs() picks the rows covering every pair of levels
#  that can meet (the pairs of split with paging excepted).  Oracle: the
#  rows of the pages, but the continuation rows, are the expected stack's,
#  cell by cell; a page's first row is no blank row, and is a continuation
#  row exactly when the stack's row there is a row of a part with a heading;
#  a continuation row is its heading + " (Cont.)" with no cell; no page
#  holds more than max_rows rows but blank ones; a page holds one value of
#  GRP; a headed group that fits max_rows is whole on a page under
#  group_safe; no paging is one page a group.  The RTF has the pages and
#  their rows, the blank ones a separator.
#  FR-PL968 FR-PL903 FR-PL904 FR-PL905 FR-PL432 FR-PL433 FR-PL435 FR-PL436
#  FR-PL437.
#
#  (B) FT-PL986, decision table: a page break at a part's boundary, five
#  rows a page, five subjects in the first part (S-01 ..) and three rows of
#  statistics (N, Mean, SD) in the second:
#    n1 = 6  the break is one row before the boundary: the first part's last
#            row starts the page
#    n1 = 5  the break is on the boundary
#    n1 = 4  the break is one row after it: a heading row, or the first
#            statistic, would be last on its page
#    heading   blanks   the second part
#    present   -        has a heading row (a group of its three rows)
#    absent    -        has none
#    -         none     no blank row between the parts
#    -         between  a blank row (a row of the stack, of the first part's
#                       last record's group)
#  Expected, both splits (the cut is at the same place; a group is cut only
#  by group_force and only past what min_group_rows leaves):
#    the part's heading row never ends a page; a page never starts with a
#    blank row or an empty continuation row; the blank row between the parts
#    ends a page or sits among the rows, and moves with the last record when
#    that stays together; a continuation row is the heading + " (Cont.)".
#  FR-PL969 FR-PL905 FR-PL433 FR-PL435 FR-PL436.
#
#  (C) FT-LS162, decision table: plan_listing() at a page edge (a listing is
#  laid out by listing_spec(), so it is a plan of its own;
#  plan_append() refuses it, FT-PL987).  Six
#  records of three subjects, one wrapped over four lines (Nausea ..) and
#  one over three (Dizziness ..), the key column ID with collapse_repeats:
#    max_rows  blanks     key repeats  Expected
#    5         records    collapse     the wrapped record moves whole to the
#                                      next page; ID shows at the page top
#                                      and once for a run
#    5         none       collapse     no blank row; a record that fills the
#                                      page exactly ends it
#    5         records    off          ID on a record's first row only
#    6         records    collapse     a page ends where the next record
#                                      does not fit
#    7         records    collapse     a record that fits exactly ends a page
#    5         records    collapse     (first = FALSE) no blank row at the
#                                      top of a page
#  A page begins with one blank row (the template's) unless first = FALSE or
#  where = "none".  A record longer than a page is FT-LS163 below.
#  FR-LS254 FR-PL189 FR-PL196 FR-PL184 FR-PL432 FR-LS156.
# ---------------------------------------------------------------------------

.pa_TP <- c("t1", "t2", "t3")
.pa_KINDS <- list(tt = c("stats", "stats", "stats"),
                  tl = c("stats", "records", "stats"),
                  ls = c("records", "stats", "stats"))
.pa_STATS <- list(c(N = "n", Mean = "mean", SD = "sd"),
                  c(Min = "min", Max = "max", Median = "median"),
                  c(Q1 = "q1", Q3 = "q3"))
.pa_PREFIX <- c("S", "R", "Q")
.pa_NREC <- c(x = 5L, y = 4L)
.pa_MAXROWS <- 3L

# every pair of levels of two factors, as .allpairs() spells them
.pa_pairs <- function(levels) {
  pr <- utils::combn(names(levels), 2L, simplify = FALSE)
  unlist(lapply(pr, function(p) {
    g <- expand.grid(a = levels[[p[1L]]], b = levels[[p[2L]]],
                     stringsAsFactors = FALSE)
    paste(p[1L], g$a, p[2L], g$b, sep = "|")
  }))
}

.pa_heading_text <- function(i) paste("Block", LETTERS[i])

# a value that tells part, group, row and column
.pa_val <- function(i, gi, j, k) 1000 * i + 100 * gi + 10 * j + k + 0.5

# the data of part i: its records, or the long frame of its statistics
.pa_data <- function(i, kind, by, fewer) {
  ts <- if (fewer) .pa_TP[1:2] else .pa_TP
  gs <- if (by) c("x", "y") else "x"
  out <- lapply(seq_along(gs), function(gi) {
    if (kind == "records") {
      d <- expand.grid(r = seq_len(.pa_NREC[[gs[gi]]]), TP = ts,
                       stringsAsFactors = FALSE)
      d <- d[order(d$r, match(d$TP, .pa_TP)), ]
      d$ID <- sprintf("%s-%02d", .pa_PREFIX[i], d$r + 10L * (gi - 1L))
      d$V <- .pa_val(i, gi - 1L, d$r, match(d$TP, .pa_TP))
      d$V[d$r == 2L & d$TP == "t2"] <- NA_real_
      d$r <- NULL
    } else {
      st <- unname(.pa_STATS[[i]])
      d <- expand.grid(TP = ts, j = seq_along(st), stringsAsFactors = FALSE)
      d <- d[order(d$j, match(d$TP, .pa_TP)), ]
      d$variable <- "V"
      d$.label <- NA_character_
      d$stat_name <- st[d$j]
      d$stat <- .pa_val(i, gi - 1L, d$j, match(d$TP, .pa_TP))
      # the last statistic has no value at t2
      d <- d[!(d$j == length(st) & d$TP == "t2"), ]
      d$j <- NULL
    }
    d$GRP <- gs[gi]
    d
  })
  d <- do.call(rbind, out)
  if (!by) d$GRP <- NULL
  rownames(d) <- NULL
  d
}

# part i as a plan: how its own rows are made
.pa_plan <- function(i, kind, heading, na, by, fewer) {
  d <- .pa_data(i, kind, by, fewer)
  fb <- eval(bquote(~ .(.pa_heading_text(i))))
  if (kind == "records") {
    rows <- if (heading) list(block = fb, subject = ~ "{ID}") else "ID"
    p <- table_plan(d, cols = "TP", rows = rows, value = "V")
    if (is.null(na)) plan_cells(p, "{value:.1f}") else plan_cells(p, "{value:.1f}", na = na)
  } else {
    p <- if (heading) table_plan(d, cols = "TP", rows = c(block = fb)) else
      table_plan(d, cols = "TP")
    tpl <- stats::setNames(sprintf("{%s:.1f}", .pa_STATS[[i]]), names(.pa_STATS[[i]]))
    if (is.null(na)) plan_cells(p, V = tpl) else plan_cells(p, V = tpl, na = na)
  }
}

# the stack the help describes, a row each: group, part, type
# (heading / row / blank), label and the three value columns
.pa_expected <- function(kinds, heading, na, by, fewer_part, blanks) {
  nacell <- if (is.null(na)) "" else na
  fmt <- function(v) sprintf("%.1f", v)
  gs <- if (by) c("x", "y") else "x"
  rows <- list()
  add <- function(grp, part, type, label, cells) {
    rows[[length(rows) + 1L]] <<- data.frame(
      grp = grp, part = part, type = type, label = label, t1 = cells[1L],
      t2 = cells[2L], t3 = cells[3L], stringsAsFactors = FALSE)
  }
  for (gi in seq_along(gs)) {
    for (i in seq_along(kinds)) {
      if (i > 1L && blanks == "between_parts") add(gs[gi], i, "blank", "", rep("", 3L))
      ind <- if (heading) "    " else ""
      if (heading) add(gs[gi], i, "heading", .pa_heading_text(i), rep("", 3L))
      fewer <- identical(fewer_part, i)
      if (kinds[i] == "records") {
        for (r in seq_len(.pa_NREC[[gs[gi]]])) {
          cells <- vapply(1:3, function(k) {
            if ((fewer && k == 3L) || (r == 2L && k == 2L)) nacell else
              fmt(.pa_val(i, gi - 1L, r, k))
          }, "")
          add(gs[gi], i, "row", paste0(ind, sprintf("%s-%02d", .pa_PREFIX[i], r + 10L * (gi - 1L))), cells)
        }
      } else {
        nm <- names(.pa_STATS[[i]])
        for (j in seq_along(nm)) {
          cells <- vapply(1:3, function(k) {
            if ((fewer && k == 3L) || (j == length(nm) && k == 2L)) nacell else
              fmt(.pa_val(i, gi - 1L, j, k))
          }, "")
          add(gs[gi], i, "row", paste0(ind, nm[j]), cells)
        }
      }
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# ---- reading the RTF written ----------------------------------------------

# One RTF row as its cells' text: a separator row (one cell) as one "".
.pa_rtf_cells <- function(line) {
  body <- sub("^.*\\\\cellx[0-9]+", "", line)
  body <- sub("\\\\row\\s*$", "", body)
  cells <- strsplit(body, "\\\\cell(?!x)", perl = TRUE)[[1L]]
  cells <- sub("^\\\\q[lcr]\\\\li0\\\\ri0 ?", "", cells)
  cells <- gsub("\\\\u160\\?", " ", cells)
  sub(" +$", "", cells)
}

# The RTF's pages: a list, a page a list of rows (each its cells), the
# column header row dropped.
.pa_rtf_pages <- function(f) {
  x <- readLines(f, warn = FALSE)
  page <- cumsum(grepl("\\page", x, fixed = TRUE))
  is_row <- grepl("^\\\\trowd", x) & grepl("\\row", x, fixed = TRUE)
  rows <- lapply(x[is_row], .pa_rtf_cells)
  by_page <- split(rows, page[is_row])
  lapply(unname(by_page), function(r) r[-1L])
}

.pa_blank_row <- function(label, cells) !nzchar(label) && !any(nzchar(cells))

# the pages as one data frame, a row each, with its page number
.pa_pages <- function(p) {
  pg <- plan_apply(p, "pages")
  if (inherits(pg, "rtftable")) list(pg) else pg
}
.pa_stream <- function(pages) {
  do.call(rbind, lapply(seq_along(pages), function(k) {
    d <- pages[[k]]$data
    v <- lapply(.pa_TP, function(tp) if (tp %in% names(d)) as.character(d[[tp]]) else rep("", nrow(d)))
    data.frame(page = k, label = gsub(" ", " ", as.character(d$label)),
               t1 = v[[1L]], t2 = v[[2L]], t3 = v[[3L]], stringsAsFactors = FALSE)
  }))
}

# the RTF's rows equal the pages' rows, page by page
.pa_expect_rtf <- function(pages, rtf, info) {
  expect_identical(length(rtf), length(pages), info = info)
  for (k in seq_len(min(length(rtf), length(pages)))) {
    d <- pages[[k]]$data
    want <- lapply(seq_len(nrow(d)), function(r) {
      cells <- c(gsub(" ", " ", as.character(d$label[r])),
                 vapply(names(d)[-1L], function(nm) as.character(d[[nm]][r]), ""))
      cells <- unname(sub(" +$", "", cells))
      if (.pa_blank_row(cells[1L], cells[-1L])) "" else cells
    })
    expect_identical(rtf[[k]], want, info = paste(info, "RTF page", k))
  }
}

# One case of (A): the failures found.
.pa_case <- function(cs, dir) {
  by <- cs$paging == "page_by"
  nparts <- as.integer(cs$nparts)
  kinds <- .pa_KINDS[[cs$kinds]][seq_len(nparts)]
  heading <- cs$heading == "present"
  na <- if (cs$na == "NC") "NC"
  fewer_part <- if (cs$columns == "fewer") 2L
  plans <- lapply(seq_len(nparts), function(i) {
    .pa_plan(i, kinds[i], heading, na, by, identical(fewer_part, i))
  })
  p <- plan_levels(plans[[1L]], TP = .pa_TP)
  for (i in seq_len(nparts)[-1L]) p <- plan_append(p, plans[[i]])
  if (cs$paging == "rows") {
    p <- plan_paginate_rows(p, max_rows = .pa_MAXROWS, split = cs$split)
  }
  if (by) p <- plan_paginate_rows(p, page_by = "GRP")
  if (cs$blanks == "none") p <- plan_blanks(p, where = "none")
  if (cs$blanks == "between_parts") p <- plan_blanks(p, where = "between_parts")
  info <- paste(names(cs), unlist(cs), sep = "=", collapse = " ")
  pages <- .pa_pages(p)
  exp <- .pa_expected(kinds, heading, na, by, fewer_part, cs$blanks)
  act <- .pa_stream(pages)
  cont <- grepl(" \\(Cont\\.\\)$", act$label)

  # every row of every part once, in order, cell by cell
  body <- act[!cont, c("label", "t1", "t2", "t3")]
  rownames(body) <- NULL
  expect_identical(body, exp[c("label", "t1", "t2", "t3")], info = info)
  exp$page <- act$page[!cont]

  # a page holds one value of the page key; no paging, one page a value
  expect_true(all(tapply(exp$grp, exp$page, function(g) length(unique(g))) == 1L), info = info)
  if (cs$paging == "none") {
    expect_identical(length(pages), length(unique(exp$grp)), info = info)
  }
  if (by) expect_identical(length(unique(exp$grp)), 2L, info = info)

  # a page's first row: no blank row; a continuation row exactly when the
  # stack's row there is a row of a part with a heading, and then that
  # part's heading + " (Cont.)" with no cell
  for (k in seq_along(pages)) {
    rows_k <- which(act$page == k)
    first <- rows_k[1L]
    expect_false(.pa_blank_row(act$label[first], unlist(act[first, c("t1", "t2", "t3")])),
                 info = paste(info, "blank first row, page", k))
    e <- which(exp$page == k)[1L]
    want_cont <- heading && exp$type[e] == "row"
    expect_identical(cont[first], want_cont, info = paste(info, "continuation, page", k))
    if (want_cont) {
      expect_identical(act$label[first], paste0(.pa_heading_text(exp$part[e]), " (Cont.)"), info = info)
      expect_identical(unname(unlist(act[first, c("t1", "t2", "t3")])), rep("", 3L), info = info)
    }
    # only a page's first row is a continuation row
    expect_false(any(cont[rows_k[-1L]]), info = info)
  }

  # the row budget, but for blank rows
  if (cs$paging == "rows") {
    n <- tapply(seq_len(nrow(act)), act$page, function(ix) {
      sum(!mapply(.pa_blank_row, act$label[ix], asplit(as.matrix(act[ix, c("t1", "t2", "t3")]), 1L)))
    })
    expect_true(all(n <= .pa_MAXROWS), info = info)
  }
  # group_safe: a group (heading and rows) that fits the budget is not cut
  if (cs$split == "group_safe" && heading) {
    for (g in unique(exp$grp)) for (i in seq_len(nparts)) {
      ix <- which(exp$grp == g & exp$part == i & exp$type != "blank")
      if (length(ix) <= .pa_MAXROWS) {
        expect_identical(length(unique(exp$page[ix])), 1L,
                         info = paste(info, "group", g, "part", i))
      }
    }
  }

  # the RTF
  f <- file.path(dir, "a.rtf")
  generate_rtfreport(rtf_document() |> rtf_tables(p), f)
  .pa_expect_rtf(pages, .pa_rtf_pages(f), info)
  invisible(NULL)
}

test_that("FT-PL985 [FR-PL968 FR-PL903 FR-PL904 FR-PL905 FR-PL432 FR-PL433 FR-PL435 FR-PL436 FR-PL437]: pairwise -- plan_append() parts x heading x na x columns x paging x split x blank rows", {
  dir <- tempfile("pa_a")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  levels <- list(nparts = c("2", "3"), kinds = c("tt", "tl", "ls"),
                 heading = c("present", "absent"), na = c("default", "NC"),
                 columns = c("same", "fewer"),
                 paging = c("none", "rows", "page_by"),
                 split = c("group_safe", "group_force", "-"),
                 blanks = c("unset", "none", "between_parts"))
  cand <- do.call(expand.grid, c(levels, stringsAsFactors = FALSE))
  # a split is given with a row budget, and only then
  cand <- cand[(cand$paging == "rows") == (cand$split != "-"), ]
  rownames(cand) <- NULL
  expect_identical(nrow(cand), 576L)
  ap <- .allpairs(cand)
  # every pair of levels that can meet is in the candidates, and the rows
  # picked cover them all
  never <- c(paste("paging", c("none", "page_by"), "split", rep(c("group_safe", "group_force"), each = 2L), sep = "|"),
             "paging|rows|split|-")
  expect_setequal(ap$need, setdiff(.pa_pairs(levels), never))
  expect_setequal(ap$covered, ap$need)
  cases <- ap$rows
  for (i in seq_len(nrow(cases))) .pa_case(cases[i, ], dir)
})

# ---------------------------------------------------------------------------
#  (B) a page break at a part's boundary
# ---------------------------------------------------------------------------

# the pages as text: a space between rows, "_" a blank row, " | " a page;
# indentation dropped
.pa_text <- function(pages) {
  paste(vapply(pages, function(pg) {
    lab <- trimws(gsub(" ", " ", as.character(pg$data$label)))
    empty <- !nzchar(lab) & !nzchar(trimws(as.character(pg$data$t1))) &
      !nzchar(trimws(as.character(pg$data$t2)))
    lab[empty] <- "_"
    paste(lab, collapse = " ")
  }, ""), collapse = " | ")
}

.pa_plan_b <- function(n1, heading, blanks, split) {
  rec <- data.frame(ID = sprintf("S-%02d", seq_len(n1))[rep(seq_len(n1), each = 2L)],
                    TP = rep(c("t1", "t2"), n1), V = seq_len(2L * n1) + 0.5,
                    stringsAsFactors = FALSE)
  lng <- data.frame(TP = rep(c("t1", "t2"), 3L), variable = "V",
                    stat_name = rep(c("n", "mean", "sd"), each = 2L),
                    stat = seq_len(6L) + 0.5, .label = NA_character_,
                    stringsAsFactors = FALSE)
  child <- (if (heading) table_plan(lng, cols = "TP", rows = c(block = ~ "Summary")) else
    table_plan(lng, cols = "TP")) |>
    plan_cells(V = c(N = "{n:.0f}", Mean = "{mean:.1f}", SD = "{sd:.1f}"))
  p <- table_plan(rec, cols = "TP", rows = "ID", value = "V") |>
    plan_cells("{value:.1f}") |>
    plan_append(child) |>
    plan_paginate_rows(max_rows = 5L, split = split)
  if (blanks == "between") p <- plan_blanks(p, where = "between_parts")
  p
}

test_that("FT-PL986 [FR-PL969 FR-PL905 FR-PL433 FR-PL435 FR-PL436]: decision table -- a page break one row before, on and one row after a part's boundary", {
  dir <- tempfile("pa_b")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  s <- function(n) paste(sprintf("S-%02d", seq_len(n)), collapse = " ")
  cases <- data.frame(
    heading = rep(c("present", "absent"), each = 12L),
    blanks  = rep(rep(c("between", "none"), each = 6L), 2L),
    split   = rep(rep(c("group_safe", "group_force"), each = 3L), 4L),
    n1      = rep(c(4L, 5L, 6L), 8L),
    stringsAsFactors = FALSE)
  # present, between: the blank row is the first part's last record's, so
  # it goes with the record and ends a page, or sits among the rows
  want <- c(
    # present, between, group_safe
    paste0(s(4), " _ | Summary N Mean SD"),
    paste0(s(4), " | S-05 _ | Summary N Mean SD"),
    paste0(s(5), " | S-06 _ | Summary N Mean SD"),
    # present, between, group_force: the group is cut past what min_group_rows leaves
    paste0(s(4), " _ | Summary N Mean SD"),
    paste0(s(4), " | S-05 _ Summary N | Summary (Cont.) Mean SD"),
    paste0(s(5), " | S-06 _ Summary N | Summary (Cont.) Mean SD"),
    # present, none
    paste0(s(4), " | Summary N Mean SD"),
    paste0(s(5), " | Summary N Mean SD"),
    paste0(s(5), " | S-06 Summary N Mean SD"),
    paste0(s(4), " | Summary N Mean SD"),
    paste0(s(5), " | Summary N Mean SD"),
    paste0(s(5), " | S-06 Summary N Mean SD"),
    # absent, between: no heading, so a statistic row can stand anywhere
    paste0(s(4), " _ | N Mean SD"),
    paste0(s(4), " | S-05 _ N Mean SD"),
    paste0(s(5), " | S-06 _ N Mean SD"),
    paste0(s(4), " _ | N Mean SD"),
    paste0(s(4), " | S-05 _ N Mean SD"),
    paste0(s(5), " | S-06 _ N Mean SD"),
    # absent, none
    paste0(s(4), " N | Mean SD"),
    paste0(s(5), " | N Mean SD"),
    paste0(s(5), " | S-06 N Mean SD"),
    paste0(s(4), " N | Mean SD"),
    paste0(s(5), " | N Mean SD"),
    paste0(s(5), " | S-06 N Mean SD"))
  expect_identical(length(want), nrow(cases))
  for (i in seq_len(nrow(cases))) {
    cs <- cases[i, ]
    info <- paste(names(cs), unlist(cs), sep = "=", collapse = " ")
    p <- .pa_plan_b(cs$n1, cs$heading == "present", cs$blanks, cs$split)
    pages <- .pa_pages(p)
    expect_identical(.pa_text(pages), want[i], info = info)
    act <- .pa_stream(pages)
    # no page starts with a blank row or an empty continuation row
    first <- !duplicated(act$page)
    expect_true(all(nzchar(act$label[first])), info = info)
    # a heading row never ends a page
    last <- !duplicated(act$page, fromLast = TRUE)
    expect_false(any(trimws(act$label[last]) == "Summary"), info = info)
    # no row budget exceeded, but for the blank row
    n <- tapply(nzchar(act$label), act$page, sum)
    expect_true(all(n <= 5L), info = info)
    f <- file.path(dir, sprintf("b%02d.rtf", i))
    generate_rtfreport(rtf_document() |> rtf_tables(p), f)
    .pa_expect_rtf(pages, .pa_rtf_pages(f), info)
  }
})

# ---------------------------------------------------------------------------
#  (C) a listing at a page edge
# ---------------------------------------------------------------------------

.pa_listing_data <- function() {
  data.frame(ID = c("S-01", "S-01", "S-01", "S-02", "S-02", "S-03"),
             TERM = c("Headache", "Nausea and vomiting after the dose", "Rash",
                      "Fatigue", "Dizziness and light headedness", "Cough"),
             SEV = c("MILD", "MODERATE", "MILD", "MILD", "SEVERE", "MILD"),
             stringsAsFactors = FALSE)
}

.pa_listing_plan <- function(max_rows, blanks, collapse, first) {
  p <- table_plan(.pa_listing_data()) |>
    plan_listing(listing_col("ID", width = 6, collapse_repeats = collapse, label = "Subject"),
                 listing_col("TERM", width = 12, label = "Term"),
                 listing_col("SEV", width = 8, label = "Severity")) |>
    plan_paginate_rows(max_rows = max_rows)
  if (blanks == "none") p <- plan_blanks(p, where = "none", first = FALSE)
  if (!first && blanks != "none") p <- plan_blanks(p, first = FALSE)
  p
}

# a listing page as "ID|term|severity" rows; "_" the blank row, "-" a key
# the page blanks (NA)
.pa_listing_rows <- function(d, na = "-") {
  id <- ifelse(is.na(d$ID), na, as.character(d$ID))
  r <- paste(id, trimws(d$TERM), trimws(d$SEV), sep = "|")
  r[id %in% c(na, "") & !nzchar(trimws(d$TERM)) & !nzchar(trimws(d$SEV))] <- "_"
  r
}

test_that("FT-LS162 [FR-LS254 FR-PL189 FR-PL196 FR-PL184 FR-PL432 FR-LS156]: decision table -- plan_listing(): a wrapped record, the key and the blank row at a page edge", {
  dir <- tempfile("pa_c")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  P <- function(...) list(...)
  cases <- data.frame(
    max_rows = c(5L, 5L, 5L, 6L, 7L, 5L),
    blanks   = c("records", "none", "records", "records", "records", "records"),
    collapse = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE),
    first    = c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE)
  nausea <- c("S-01|Nausea and|MODERATE", "-|vomiting|", "-|after the|", "-|dose|")
  dizz <- c("S-02|Dizziness|SEVERE", "-|and light|", "-|headedness|")
  want <- list(
    # the wrapped record (4 lines and its blank row) moves whole to the next
    # page, the key shows again at each page's top, once for a run
    P(c("S-01|Headache|MILD", "_"), c(nausea, "_"),
      c("S-01|Rash|MILD", "_", "S-02|Fatigue|MILD", "_"),
      c(dizz, "_"), c("S-03|Cough|MILD", "_")),
    # no blank row: the page holds five rows exactly (a record and the
    # wrapped one, the key blanked in the run), the next begins with the key
    # again
    P(c("S-01|Headache|MILD", sub("^S-01", "-", nausea)),
      c("S-01|Rash|MILD", "S-02|Fatigue|MILD", sub("^S-02", "-", dizz)),
      "S-03|Cough|MILD"),
    # key off: it is on a record's first row only
    P(c("S-01|Headache|MILD", "_"), c(sub("^-", "", nausea), "_"),
      c("S-01|Rash|MILD", "_", "S-02|Fatigue|MILD", "_"),
      c(sub("^-", "", dizz), "_"), c("S-03|Cough|MILD", "_")),
    # a page ends where the next record does not fit
    P(c("S-01|Headache|MILD", "_"), c(nausea, "_"),
      c("S-01|Rash|MILD", "_", "S-02|Fatigue|MILD", "_"),
      c(dizz, "_", "S-03|Cough|MILD", "_")),
    # a record that fits exactly ends a page
    P(c("S-01|Headache|MILD", "_", sub("^S-01", "-", nausea), "_"),
      c("S-01|Rash|MILD", "_", "S-02|Fatigue|MILD", "_"),
      c(dizz, "_", "S-03|Cough|MILD", "_")),
    # no blank row at the top of a page
    P(c("S-01|Headache|MILD", "_"), c(nausea, "_"),
      c("S-01|Rash|MILD", "_", "S-02|Fatigue|MILD", "_"),
      c(dizz, "_"), c("S-03|Cough|MILD", "_")))
  expect_identical(length(want), nrow(cases))
  for (i in seq_len(nrow(cases))) {
    cs <- cases[i, ]
    info <- paste(names(cs), unlist(cs), sep = "=", collapse = " ")
    p <- .pa_listing_plan(cs$max_rows, cs$blanks, cs$collapse, cs$first)
    pages <- .pa_pages(p)
    got <- lapply(pages, function(pg) .pa_listing_rows(pg$data))
    # case 3 has an empty key where a repeat is blanked (not NA)
    if (!cs$collapse) got <- lapply(pages, function(pg) .pa_listing_rows(pg$data, na = ""))
    expect_identical(got, want[[i]], info = info)
    # the blank row the template puts at the top of a page, or none
    top <- cs$blanks == "records" && cs$first
    for (pg in pages) {
      expect_identical(!is.null(pg$blank_rows) && identical(as.integer(pg$blank_rows), 0L),
                       top, info = info)
    }
    # no page is taller than the budget, the top blank row apart
    expect_true(all(vapply(pages, function(pg) nrow(pg$data), 0L) <= cs$max_rows), info = info)
    # the RTF: the same rows, the top blank row (a separator) first
    f <- file.path(dir, sprintf("c%02d.rtf", i))
    generate_rtfreport(rtf_document() |> rtf_tables(p), f)
    rtf <- .pa_rtf_pages(f)
    expect_identical(length(rtf), length(pages), info = info)
    for (k in seq_along(pages)) {
      d <- pages[[k]]$data
      rows <- vapply(seq_len(nrow(d)), function(r) {
        id <- if (is.na(d$ID[r])) "" else as.character(d$ID[r])
        paste(id, trimws(d$TERM[r]), trimws(d$SEV[r]), sep = "|")
      }, "")
      rows[rows == "||"] <- "_"
      if (top) rows <- c("_", rows)
      got_rtf <- vapply(rtf[[k]], function(cells) {
        if (length(cells) == 1L) return("_")
        r <- paste(cells[1L], cells[3L], cells[5L], sep = "|")
        if (r == "||") "_" else r
      }, "")
      expect_identical(got_rtf, rows, info = paste(info, "RTF page", k))
    }
  }
})

test_that("FT-LS163 [FR-LS255 FR-LS254]: a plan_listing() record longer than a page continues on the next page with no empty row before it", {
  d <- data.frame(ID = c("S-01", "S-01"),
                  TERM = c("Headache", "Nausea and vomiting after the dose"),
                  SEV = c("MILD", "MODERATE"), stringsAsFactors = FALSE)
  for (collapse in c(TRUE, FALSE)) {
    p <- table_plan(d) |>
      plan_listing(listing_col("ID", width = 6, collapse_repeats = collapse),
                   listing_col("TERM", width = 12), listing_col("SEV", width = 8)) |>
      plan_paginate_rows(max_rows = 3)
    pages <- plan_apply(p)
    got <- lapply(pages, function(pg) .pa_listing_rows(pg$data, na = ""))
    info <- paste("collapse_repeats =", collapse)
    expect_length(pages, 3L)
    # the long record (four lines) is cut after three rows; its last line
    # opens the next page, and no page opens with an empty row
    expect_identical(got[[2L]], c("S-01|Nausea and|MODERATE", "|vomiting|", "|after the|"),
                     info = info)
    # (the key shows again at the page's top only when it is collapsed)
    expect_identical(sub("^[^|]*", "", got[[3L]][1L]), "|dose|", info = info)
    for (pg in pages[-1L]) {
      expect_true(nzchar(trimws(pg$data$TERM[1L])), info = info)
      # the page's template blank row, the only one at its top
      expect_identical(as.integer(pg$blank_rows), 0L, info = info)
    }
    # the budget holds: the rows of a page are the budget, none lost to a
    # continuation row
    expect_true(all(vapply(pages, function(pg) nrow(pg$data), 0L) <= 3L), info = info)
  }
})

test_that("FT-PL987 [FR-PL970]: plan_append() on a plan with plan_listing() stops and names the contents of a document", {
  rec <- data.frame(ID = c("S-01", "S-01", "S-02", "S-02"), TP = rep(c("t1", "t2"), 2),
                    V = c(1.5, 2.5, 3.5, 4.5))
  lng <- data.frame(TP = c("t1", "t2"), variable = "V", stat_name = "mean",
                    stat = c(2.5, 3.5), .label = NA_character_)
  summary_plan <- table_plan(lng, cols = "TP", rows = c(block = ~ "Summary")) |>
    plan_cells(V = c(Mean = "{mean:.1f}"))
  parent <- table_plan(rec, cols = "TP", rows = "ID", value = "V") |>
    plan_cells("{value:.1f}")
  listed <- plan_listing(parent, listing_col("ID"), listing_col("V"))
  advice <- "rtf_tables\\(doc, summary_plan, page_break = FALSE\\)"
  # the first plan is a listing: stops where the part is added ...
  expect_error(plan_append(listed, summary_plan),
               "first plan carries plan_listing\\(\\).*cannot be appended")
  expect_error(plan_append(listed, summary_plan), advice)
  expect_error(plan_append(listed, summary_plan), "Several contents on one page", fixed = TRUE)
  # ... and when the listing is written after plan_append()
  late <- plan_listing(plan_append(parent, summary_plan),
                       listing_col("ID"), listing_col("V"))
  expect_error(plan_apply(late, "table"), "first plan carries plan_listing\\(\\)")
  expect_error(plan_apply(late), advice)
  # a part with plan_listing(): the same advice, not "move to the first plan"
  err <- tryCatch(plan_append(parent, plan_listing(summary_plan, listing_col("TP"))),
                  error = function(e) conditionMessage(e))
  expect_match(err, "the part carries plan_listing\\(\\)")
  expect_match(err, advice)
  expect_false(grepl("Move to the first plan", err, fixed = TRUE))
  # without a listing it stacks as before
  expect_no_error(plan_apply(plan_append(parent, summary_plan), "table"))
})

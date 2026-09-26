# as_rtftables(page_by = ): the OUTER page partition -- a page per BY value,
# with the inner grouping still protected inside it (#423).

.lab <- function(periods = c("Period 1", "Period 2"),
                 params  = c("ALT", "Bilirubin", "Haemoglobin")) {
  do.call(rbind, lapply(periods, function(per) {
    do.call(rbind, lapply(params, function(p) {
      d <- data.frame(period    = per,
                      Parameter = c(p, "  n", "  Mean (SD)", "  Median"),
                      stringsAsFactors = FALSE)
      for (v in paste0("V", 1:4)) d[[v]] <- c("", "80", "50.0 (2.10)", "49.5")
      d
    }))
  }))
}

.names <- function(x) names(x)
.rows  <- function(x) vapply(x, function(p) nrow(p$data), integer(1L))
.first <- function(x) vapply(x, function(p) p$data[[1L]][1L], character(1L))

# ──────── the partition ────────────────────────────────────────────────────

test_that("page_by gives one page per BY value, named by it", {
  pg <- as_rtftables(.lab(), page_by = "period", drop_cols = "period")
  expect_length(pg, 2L)
  expect_identical(.names(pg), c("Period 1", "Period 2"))
  expect_identical(unname(.rows(pg)), c(12L, 12L))
})

test_that("the inner split runs INSIDE each partition, group protected", {
  # 3 blocks of 4 rows per period; max_rows = 8 fits two whole blocks, so the
  # third starts a new page instead of being cut.
  pg <- as_rtftables(.lab(), page_by = "period", split = "group_safe",
                     group_by = "indent", max_rows = 8, drop_cols = "period")
  # every page of one partition carries the same HEADING; the "...n" tail only
  # keeps the list addressable (`pages[["Period 1...2"]]`, View())
  expect_identical(.names(pg),
                   c("Period 1...1", "Period 1...2",
                     "Period 2...1", "Period 2...2"))
  expect_identical(rtfreporter:::.page_name_base(.names(pg)),
                   c("Period 1", "Period 1", "Period 2", "Period 2"))
  expect_identical(unname(.rows(pg)), c(8L, 4L, 8L, 4L))
  # every page starts on a block label, never mid-block
  expect_identical(unname(.first(pg)),
                   c("ALT", "Haemoglobin", "ALT", "Haemoglobin"))
})

test_that("by_value alone cuts inside the block -- what page_by is for", {
  pg <- as_rtftables(.lab(), split = "by_value", group_col = "period",
                     max_rows = 8, drop_cols = "period")
  # the period IS the group here, so the cut lands wherever max_rows falls
  # (and the continuation page carries the group's "(Cont.)" row)
  expect_identical(unname(.rows(pg)), c(8L, 5L, 8L, 5L))
  expect_false(identical(unname(.first(pg))[2L], "Haemoglobin"))
})

test_that("page_by accepts several columns and names the page by the pair", {
  d <- .lab(periods = c("Period 1", "Period 2"), params = "ALT")
  d$cohort <- rep(c("A", "B"), each = 2L, times = 2L)
  pg <- as_rtftables(d, page_by = c("period", "cohort"),
                     drop_cols = c("period", "cohort"))
  expect_identical(.names(pg), c("Period 1, A", "Period 1, B",
                                 "Period 2, A", "Period 2, B"))
})

test_that("a page GATHERS its rows, scattered or not, in the body's order", {
  # Needing the body sorted first is SAS's contract, not R's: R subsets
  # by value and leaves the order alone.  So Period 1 is ONE page holding
  # both of its blocks, and the rows inside it stay as they came.
  d  <- .lab(periods = "Period 1", params = "ALT")
  d2 <- rbind(d, .lab(periods = "Period 2", params = "ALT"), d)
  pg <- as_rtftables(d2, page_by = "period", drop_cols = "period")
  expect_identical(.names(pg), c("Period 1", "Period 2"))
  expect_identical(unname(.rows(pg)),
                   c(2L * nrow(d), nrow(d)))
})

test_that("a by_value group gathers its rows too", {
  # the two splits had disagreed: by_value + stub_vars has always
  # gathered, and this path cut the same table into one page per block
  d <- data.frame(period = c("P1", "P2", "P1", "P2"),
                  label  = c("a", "b", "c", "d"),
                  n      = 1:4, stringsAsFactors = FALSE)
  pg <- as_rtftables(d, split = "by_value", group_col = "period",
                     drop_cols = "period")
  expect_identical(.names(pg), c("P1", "P2"))
  expect_identical(pg[[1L]]$data[[1L]], c("a", "c"))
  expect_identical(pg[[2L]]$data[[1L]], c("b", "d"))
})

# ──────── naming ───────────────────────────────────────────────────────────

test_that("an inner by_value is the OUTER axis, and names the pages alone", {
  # The group a value-based split makes into a page owns the page; `page_by`
  # runs inside it.  So the pages of one group stay together AND share its
  # name -- the two values are never joined, because the name is the section
  # heading.  The BY value stays in the page meta.
  pg <- as_rtftables(.lab(), page_by = "period", split = "by_value",
                     group_by = "indent", drop_cols = "period")
  expect_identical(.names(pg),
                   c("ALT...1", "ALT...2", "Bilirubin...1", "Bilirubin...2",
                     "Haemoglobin...1", "Haemoglobin...2"))
  m <- attr(pg[[2L]]$data, "rtf_paginate_meta", exact = TRUE)
  expect_identical(m$page_group, "ALT")
  expect_identical(m$page_by,    "Period 2")
})

test_that("page_by = NULL changes nothing: by_value still names from group_col", {
  pg <- as_rtftables(.lab(), split = "by_value", group_col = "period",
                     drop_cols = "period")
  expect_identical(.names(pg), c("Period 1", "Period 2"))
})

# ──────── defaults and interactions ────────────────────────────────────────

test_that("group_col defaults to the first column outside page_by", {
  # With group_col left NULL the detection would otherwise read column 1 --
  # the BY column, constant within a partition and so structureless.
  pg <- as_rtftables(.lab(), page_by = "period", split = "group_safe",
                     group_by = "indent", max_rows = 8, drop_cols = "period")
  expect_identical(unname(.first(pg)),
                   c("ALT", "Haemoglobin", "ALT", "Haemoglobin"))
})

test_that("an explicit group_col still wins", {
  pg <- as_rtftables(.lab(), page_by = "period", group_col = "period",
                     split = "group_safe", max_rows = 8, drop_cols = "period")
  expect_length(pg, 4L)
})

test_that("blank_rows positions are resolved per partition", {
  pg <- as_rtftables(.lab(), page_by = "period", blank_rows = c(-1),
                     drop_cols = "period")
  expect_identical(attr(pg[[1L]]$data, "rtf_blank_rows", exact = TRUE),
                   attr(pg[[2L]]$data, "rtf_blank_rows", exact = TRUE))
})

test_that("page_by composes with paginate_cols() and page_order", {
  rows <- as_rtftables(.lab(), page_by = "period", split = "group_safe",
                       group_by = "indent", max_rows = 8, drop_cols = "period")
  expect_identical(.names(rows), c("Period 1...1", "Period 1...2",
                                   "Period 2...1", "Period 2...2"))
  blocks <- function(p) unname(vapply(p, function(q) names(q$data)[2L],
                                      character(1L)))
  # no group axis here (group_safe names no page), so the two orders are the
  # plain two-level ones: "across" advances the column block first, "down" the
  # row page.
  a <- paginate_cols(rows, at = 4)
  expect_length(a, 8L)                        # 4 row pages x 2 column blocks
  expect_identical(blocks(a), rep(c("V1", "V3"), each = 4L))
  expect_identical(rtfreporter:::.page_name_base(unname(.names(a))),
                   rep(c("Period 1", "Period 1", "Period 2", "Period 2"), 2L))
  expect_false(anyDuplicated(.names(a)) > 0L)   # still addressable by name
  d <- paginate_cols(rows, at = 4, page_order = "down")
  expect_identical(blocks(d), rep(c("V1", "V3"), 4L))
  expect_identical(rtfreporter:::.page_name_base(unname(.names(d))),
                   rep(c("Period 1", "Period 1", "Period 2", "Period 2"),
                       each = 2L))
})

test_that("an unknown page_by column is an error", {
  expect_error(as_rtftables(.lab(), page_by = "nope"), "page_by")
})

# ──────── page_by with a stub: label rows are continuations (#427) ──────────
#
# stub_cols() inserts a LABEL ROW per hierarchy level, carrying the stub text
# and NA everywhere else -- the BY column included.  Keyed literally, each of
# those rows became its own one-row partition holding just the label.

.ae <- function(periods = c("Period 1", "Period 2"), extra = NULL) {
  out <- list()
  for (per in periods) for (soc in c("CARDIAC", "GI")) {
    d <- data.frame(period = per, soc = soc,
                    pt  = c("Palpitations", "Tachycardia"),
                    n_A = c("5 (5.8)", "3 (3.5)"),
                    stringsAsFactors = FALSE)
    if (!is.null(extra)) d[[names(extra)]] <- extra[[1L]]
    out[[length(out) + 1L]] <- d
  }
  do.call(rbind, out)
}

test_that("a stub label row does not start a page of its own", {
  pg <- as_rtftables(.ae(), page_by = "period", stub_vars = c("soc", "pt"),
                     drop_cols = "period")
  expect_length(pg, 2L)                       # not 8 (a page per label row)
  expect_identical(names(pg), c("Period 1", "Period 2"))
  expect_identical(unname(.rows(pg)), c(6L, 6L))   # 2 labels + 4 PT rows each
  # the label row sits with the rows it introduces, at the top of its page
  expect_identical(as.character(pg[[1L]]$data[[1L]][1L]), "CARDIAC")
})

test_that("the filler keeps a trailing gap with the page above it", {
  v <- c("A", NA, "A", NA, "B", NA)
  expect_identical(rtfreporter:::.fill_page_by_gaps(v),
                   c("A", "A", "A", "B", "B", "B"))
  expect_identical(rtfreporter:::.fill_page_by_gaps(c(NA, "A", NA)),
                   c("A", "A", "A"))
  expect_identical(rtfreporter:::.fill_page_by_gaps(c(NA_character_, NA)),
                   c("", ""))
})

test_that("group_col is the OUTER level under split = by_value + a stub", {
  # That branch splits the body by group_col and builds the stub per page.
  # The group owns the page and `page_by` runs inside it, so a group's pages
  # stay together and the name reads "<group>.<BY value>" -- and the BY value
  # must survive the per-group naming, which once overwrote it.
  ae <- do.call(rbind, lapply(c("P1", "P2"), function(per)
    do.call(rbind, lapply(c("Cohort A", "Cohort B"), function(co) {
      d <- .ae(periods = per)
      d$cohort <- co
      d
    }))))
  pg <- as_rtftables(ae, page_by = "period", split = "by_value",
                     group_col = "cohort", stub_vars = c("soc", "pt"),
                     drop_cols = c("period", "cohort"))
  expect_identical(rtfreporter:::.page_name_base(names(pg)),
                   c("Cohort A", "Cohort A", "Cohort B", "Cohort B"))
  expect_identical(unname(.rows(pg)), rep(6L, 4L))
})

test_that("split = by_value + a stub is unchanged without page_by", {
  pg <- as_rtftables(.ae(), split = "by_value", group_col = "period",
                     stub_vars = c("soc", "pt"), drop_cols = "period")
  expect_identical(names(pg), c("Period 1", "Period 2"))
  expect_identical(unname(.rows(pg)), c(6L, 6L))
})

# ──────── three axes at once: group_col / page_by / column blocks ───────────

.gpc <- function() {
  rows <- list()
  for (g in c("G1", "G2")) for (p in c("P1", "P2"))
    rows[[length(rows) + 1L]] <- data.frame(
      cohort = g, period = p, lab = c("n", "Mean"),
      V1 = "1", V2 = "2", V3 = "3", V4 = "4", stringsAsFactors = FALSE)
  do.call(rbind, rows)
}

# "<group>|<BY value>/<first data column>" -- the two page coordinates come
# from the meta now that the NAME carries only the outer one.
.seq_of <- function(pages) {
  meta <- lapply(pages, function(p) attr(p$data, "rtf_paginate_meta",
                                         exact = TRUE))
  paste0(vapply(meta, function(m) m$page_group %||% "", character(1L)), "|",
         vapply(meta, function(m) m$page_by %||% "", character(1L)), "/",
         vapply(pages, function(p) names(p$data)[2L], character(1L)))
}

test_that("the group is the outer axis and page_by the inner one", {
  pg <- as_rtftables(.gpc(), split = "by_value", group_col = "cohort",
                     page_by = "period", drop_cols = c("cohort", "period"))
  expect_identical(rtfreporter:::.page_name_base(.names(pg)),
                   c("G1", "G1", "G2", "G2"))
  # each page records the two coordinates it was cut at
  m <- attr(pg[[2L]]$data, "rtf_paginate_meta", exact = TRUE)
  expect_identical(m$page_group, "G1")
  expect_identical(m$page_by,    "P2")
})

test_that("across = group / column block / page_by", {
  pg <- as_rtftables(.gpc(), split = "by_value", group_col = "cohort",
                     page_by = "period", drop_cols = c("cohort", "period")) |>
    paginate_cols(at = 4, carry = 1, width = "keep")
  expect_identical(unname(.seq_of(pg)),
                   c("G1|P1/V1", "G1|P2/V1", "G1|P1/V3", "G1|P2/V3",
                     "G2|P1/V1", "G2|P2/V1", "G2|P1/V3", "G2|P2/V3"))
})

test_that("down = group / page_by / column block", {
  pg <- as_rtftables(.gpc(), split = "by_value", group_col = "cohort",
                     page_by = "period", drop_cols = c("cohort", "period")) |>
    paginate_cols(at = 4, carry = 1, width = "keep", page_order = "down")
  expect_identical(unname(.seq_of(pg)),
                   c("G1|P1/V1", "G1|P1/V3", "G1|P2/V1", "G1|P2/V3",
                     "G2|P1/V1", "G2|P1/V3", "G2|P2/V1", "G2|P2/V3"))
})

test_that("a group is never broken up by the column split", {
  pg <- as_rtftables(.gpc(), split = "by_value", group_col = "cohort",
                     page_by = "period", drop_cols = c("cohort", "period"))
  for (po in c("across", "down")) {
    out <- paginate_cols(pg, at = 4, carry = 1, width = "keep",
                         page_order = po)
    grp <- rtfreporter:::.page_name_base(names(out))
    expect_identical(rle(grp)$values, c("G1", "G2"), info = po)
  }
})

# ──────── page names stay addressable (#440) ───────────────────────────────

test_that("a repeated heading is numbered so the list stays addressable", {
  pg <- as_rtftables(.lab(), page_by = "period", split = "group_safe",
                     group_by = "indent", max_rows = 8, drop_cols = "period")
  expect_identical(.names(pg), c("Period 1...1", "Period 1...2",
                                 "Period 2...1", "Period 2...2"))
  expect_false(anyDuplicated(.names(pg)) > 0L)
  # by name -- which R collapses onto the first match when names repeat
  expect_identical(pg[["Period 1...2"]]$data, pg[[2L]]$data)
})

test_that("a heading used once carries no tail", {
  pg <- as_rtftables(.lab(), page_by = "period", drop_cols = "period")
  expect_identical(.names(pg), c("Period 1", "Period 2"))
})

test_that("the tail is not part of the heading auto_section prints", {
  pg <- as_rtftables(.lab(), page_by = "period", split = "group_safe",
                     group_by = "indent", max_rows = 8, drop_cols = "period")
  doc <- rtf_document() |>
    rtf_section(secinfo = list(header = rtf_header(rows = list(c(l = "S"))))) |>
    rtf_tables(pg, auto_section = TRUE)
  rep0 <- rtfreporter:::.pipe_doc_to_rtfreport(doc)
  expect_equal(length(rep0$sections), 2L)          # one per heading, not per page
  labels <- vapply(rep0$sections, function(s) {
    rows <- s$header$rows
    paste(unlist(rows[[length(rows)]]), collapse = "")
  }, character(1L))
  expect_identical(unname(labels), c("Period 1", "Period 2"))
})

test_that("auto_title prints the heading without the tail", {
  pg <- as_rtftables(.lab(), page_by = "period", split = "group_safe",
                     group_by = "indent", max_rows = 8, drop_cols = "period")
  doc <- rtf_document() |>
    rtf_section(secinfo = list(header = NULL, footer = NULL)) |>
    rtf_tables(pg, auto_title = TRUE)
  ttl <- doc$titles[[2L]]
  expect_identical(ttl[[length(ttl)]]$text, "Period 1")
})

test_that("a heading that already ends in ...n is renumbered, not doubled", {
  expect_identical(rtfreporter:::.uniquify_page_names(c("A...1", "A...2", "B")),
                   c("A...1", "A...2", "B"))
  expect_identical(rtfreporter:::.page_name_base("A...12"), "A")
})

# ──────── page_order as an axis order (#444) ───────────────────────────────

.axis_seq <- function(pages) {
  m <- lapply(pages, function(p) attr(p$data, "rtf_paginate_meta", exact = TRUE))
  paste0(vapply(m, function(z) z$page_group %||% "-", character(1L)),
         vapply(m, function(z) z$page_by    %||% "-", character(1L)), "/",
         vapply(pages, function(p) names(p$data)[2L], character(1L)))
}

test_that("the shorthands are the two usual axis orders", {
  pg <- as_rtftables(.gpc(), split = "by_value", group_col = "cohort",
                     page_by = "period", drop_cols = c("cohort", "period"))
  cut <- function(po) .axis_seq(paginate_cols(pg, at = 4, carry = 1,
                                              width = "keep", page_order = po))
  expect_identical(cut("across"), cut(c("group", "cols", "rows")))
  expect_identical(cut("down"),   cut(c("group", "rows", "cols")))
})

test_that("every axis order is expressible", {
  pg <- as_rtftables(.gpc(), split = "by_value", group_col = "cohort",
                     page_by = "period", drop_cols = c("cohort", "period"))
  cut <- function(po) unname(.axis_seq(paginate_cols(pg, at = 4, carry = 1,
                                                     width = "keep",
                                                     page_order = po)))
  expect_identical(cut(c("group", "cols", "rows")),
                   c("G1P1/V1", "G1P2/V1", "G1P1/V3", "G1P2/V3",
                     "G2P1/V1", "G2P2/V1", "G2P1/V3", "G2P2/V3"))
  expect_identical(cut(c("group", "rows", "cols")),
                   c("G1P1/V1", "G1P1/V3", "G1P2/V1", "G1P2/V3",
                     "G2P1/V1", "G2P1/V3", "G2P2/V1", "G2P2/V3"))
  expect_identical(cut(c("cols", "group", "rows")),
                   c("G1P1/V1", "G1P2/V1", "G2P1/V1", "G2P2/V1",
                     "G1P1/V3", "G1P2/V3", "G2P1/V3", "G2P2/V3"))
  expect_identical(cut(c("rows", "cols", "group")),
                   c("G1P1/V1", "G2P1/V1", "G1P1/V3", "G2P1/V3",
                     "G1P2/V1", "G2P2/V1", "G1P2/V3", "G2P2/V3"))
  # the same pages, whatever the order
  for (po in list(c("group", "rows", "cols"), c("cols", "group", "rows"),
                  c("rows", "cols", "group"))) {
    expect_setequal(cut(po), cut(c("group", "cols", "rows")))
  }
})

test_that("an axis left out is appended in the default order", {
  pg <- as_rtftables(.gpc(), split = "by_value", group_col = "cohort",
                     page_by = "period", drop_cols = c("cohort", "period"))
  cut <- function(po) .axis_seq(paginate_cols(pg, at = 4, carry = 1,
                                              width = "keep", page_order = po))
  expect_identical(cut("cols"), cut(c("cols", "group", "rows")))
  expect_identical(cut("rows"), cut(c("rows", "group", "cols")))
})

test_that("`page_order` axes are validated", {
  pg <- as_rtftables(.gpc(), split = "by_value", group_col = "cohort",
                     page_by = "period", drop_cols = c("cohort", "period"))
  expect_error(paginate_cols(pg, at = 4, page_order = "sideways"),
               "is not an axis")
  expect_error(paginate_cols(pg, at = 4, page_order = c("cols", "cols")),
               "may appear once")
})

test_that(".resolve_page_axes fills and validates on its own", {
  f <- rtfreporter:::.resolve_page_axes
  expect_identical(f(NULL),      c("group", "cols", "rows"))   # the default
  expect_identical(f("across"),  c("group", "cols", "rows"))
  expect_identical(f("down"),    c("group", "rows", "cols"))
  expect_identical(f(c("rows")), c("rows", "group", "cols"))
  expect_identical(f(c("cols", "rows", "group")),
                   c("cols", "rows", "group"))
})

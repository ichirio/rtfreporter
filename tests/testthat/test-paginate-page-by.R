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
  expect_identical(.names(pg),
                   c("Period 1.1", "Period 1.2", "Period 2.1", "Period 2.2"))
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

test_that("page_by partitions on RUNS, like by_value", {
  d <- .lab(periods = "Period 1", params = "ALT")
  d2 <- rbind(d, .lab(periods = "Period 2", params = "ALT"), d)
  pg <- as_rtftables(d2, page_by = "period", drop_cols = "period")
  expect_identical(.names(pg), c("Period 1", "Period 2", "Period 1"))
})

# ──────── naming ───────────────────────────────────────────────────────────

test_that("an inner by_value composes its label onto the page name", {
  pg <- as_rtftables(.lab(), page_by = "period", split = "by_value",
                     group_by = "indent", drop_cols = "period")
  expect_identical(.names(pg),
                   c("Period 1.ALT", "Period 1.Bilirubin", "Period 1.Haemoglobin",
                     "Period 2.ALT", "Period 2.Bilirubin", "Period 2.Haemoglobin"))
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
  pg <- as_rtftables(.lab(), page_by = "period", split = "group_safe",
                     group_by = "indent", max_rows = 8, drop_cols = "period") |>
    paginate_cols(at = 4, page_order = "down")
  expect_length(pg, 8L)                       # 4 row pages x 2 column blocks
  expect_identical(unname(vapply(pg, function(p) names(p$data)[2L],
                                 character(1L))),
                   rep(c("V1", "V3"), each = 4L))
  expect_identical(unname(.names(pg)),
                   rep(c("Period 1.1", "Period 1.2",
                         "Period 2.1", "Period 2.2"), 2L))
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

test_that("page_by stays the OUTER level under split = by_value + a stub", {
  # That branch splits the body by group_col and builds the stub per page;
  # page_by must partition BEFORE it, or the two inverted and the page names
  # lost the BY value.
  ae <- do.call(rbind, lapply(c("P1", "P2"), function(per)
    do.call(rbind, lapply(c("Cohort A", "Cohort B"), function(co) {
      d <- .ae(periods = per)
      d$cohort <- co
      d
    }))))
  pg <- as_rtftables(ae, page_by = "period", split = "by_value",
                     group_col = "cohort", stub_vars = c("soc", "pt"),
                     drop_cols = c("period", "cohort"))
  expect_identical(names(pg), c("P1.Cohort A", "P1.Cohort B",
                                "P2.Cohort A", "P2.Cohort B"))
  expect_identical(unname(.rows(pg)), rep(6L, 4L))
})

test_that("split = by_value + a stub is unchanged without page_by", {
  pg <- as_rtftables(.ae(), split = "by_value", group_col = "period",
                     stub_vars = c("soc", "pt"), drop_cols = "period")
  expect_identical(names(pg), c("Period 1", "Period 2"))
  expect_identical(unname(.rows(pg)), c(6L, 6L))
})

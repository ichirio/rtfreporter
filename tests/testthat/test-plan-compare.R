# SPIKE (design/plan-resolver): the acceptance criterion, on real data.
#
# Every case below builds the same report twice -- once through
# as_rtftables(), once through a plan -- and requires them to agree at BOTH
# levels:
#
#   OBJECT level  the rtftable fields.  Says WHERE a difference lives.
#   RTF level     the rendered command stream.  Says whether it MATTERS.
#
# Neither alone is sufficient, and the spike has the scars to prove it: twice
# an object difference rendered identically (a normalised header, an extra
# provenance attribute), and twice an object match hid a real rendering
# difference (header alignment, the adapter's col_spec).

.demog <- function() {
  readRDS(file.path(system.file("extdata", package = "rtfreporter"),
                    "demog_p1.rds"))
}
.lab <- function() {
  readRDS(file.path(system.file("extdata", package = "rtfreporter"),
                    "lab_alt.rds"))
}

# The PK concentration shape, as data-raw/gen_pk_conc.R builds it: a two-level
# hierarchy (nominal time / statistic) with the visits across the columns.
.pk <- function() {
  vis   <- c("Day 1", "Day 7", "Day 14", "Day 28")
  times <- c(0.5, 1, 2, 4, 8, 12, 24)
  stats <- c("n", "Mean", "SD", "CV%", "Median", "Min, Max")
  set.seed(277)
  rows <- list()
  for (t in times) {
    rows[[length(rows) + 1L]] <- c(sprintf("%g h", t), "",
                                   rep("", length(vis)))
    for (s in stats) {
      rows[[length(rows) + 1L]] <- c(
        sprintf("%g h", t), s,
        vapply(seq_along(vis), function(v) sprintf("%.2f", runif(1, 1, 2000)),
               character(1L)))
    }
  }
  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(out) <- c("Time", "Statistic", vis)
  out
}

# Assert both levels agree, reporting whichever failed.
.expect_same <- function(a, b, label) {
  obj <- plan_compare_objects(a, b)
  expect_identical(nrow(obj), 0L,
                   info = paste0(label, ": objects differ -- ",
                                 paste(obj$field, collapse = ", ")))
  rtf <- plan_compare_rtf(plan_render_lines(a), plan_render_lines(b))
  expect_true(rtf$identical,
              info = paste0(label, ": RTF differs at line(s) ",
                            paste(utils::head(rtf$diffs$line, 5L),
                                  collapse = ", ")))
}

# ── the harness itself ─────────────────────────────────────────────────────

test_that("the object comparison reports no difference for equal input", {
  a <- rtf_plan(.demog()) |> plan_style(border = "tfl") |> plan_tables()
  expect_identical(nrow(plan_compare_objects(a, a)), 0L)
})

test_that("the object comparison names the field that differs", {
  a <- rtf_plan(.demog()) |> plan_style(border = "tfl") |> plan_tables()
  b <- rtf_plan(.demog()) |> plan_blanks(c(3L, 5L)) |>
    plan_style(border = "tfl") |> plan_tables()
  d <- plan_compare_objects(a, b)
  expect_true("blank_rows" %in% d$field)
})

test_that("a differing page count is reported before the fields", {
  a <- rtf_plan(.demog()) |> plan_tables()
  b <- rtf_plan(.demog()) |> plan_pages(break_after = 5L) |> plan_tables()
  d <- plan_compare_objects(a, b)
  expect_identical(d$field, "n_pages")
})

test_that("the RTF comparison finds a real difference", {
  a <- rtf_plan(.demog()) |> plan_style(border = "tfl") |> plan_tables()
  b <- rtf_plan(.demog()) |> plan_style(border = "none") |> plan_tables()
  expect_false(plan_compare_rtf(plan_render_lines(a),
                                plan_render_lines(b))$identical)
})

# ── explicit page breaks ───────────────────────────────────────────────────

test_that("break_before matches as_rtftables(split_rows = )", {
  for (sr in list(8L, c(6L, 12L, 18L), 5L)) {
    a <- as_rtftables(.demog(), split = "rows", split_rows = sr,
                      border = "tfl")
    b <- rtf_plan_from(.demog()) |> plan_pages(break_before = sr) |>
      plan_style(border = "tfl") |> plan_tables()
    .expect_same(a, b, paste("split_rows =", paste(sr, collapse = ",")))
  }
})

test_that("break_after is the other spelling, one row later", {
  a <- rtf_plan(.demog()) |> plan_pages(break_before = 8L) |> plan_tables()
  b <- rtf_plan(.demog()) |> plan_pages(break_after = 7L) |> plan_tables()
  expect_identical(nrow(plan_compare_objects(a, b)), 0L)
})

test_that("giving both spellings is refused", {
  expect_error(plan_pages(rtf_plan(.demog()), break_after = 3L,
                          break_before = 5L),
               "not both")
})

# ── the real cases ─────────────────────────────────────────────────────────

test_that("a plain data.frame renders identically", {
  .expect_same(as_rtftables(.demog(), border = "tfl"),
               rtf_plan_from(.demog()) |> plan_style(border = "tfl") |>
                 plan_tables(),
               "plain")
})

test_that("grouping plus group-safe pagination renders identically", {
  d <- .demog()
  .expect_same(
    as_rtftables(d, split = "group_safe", max_rows = 10L, group_col = 1L,
                 border = "tfl"),
    rtf_plan_from(d) |> plan_group(names(d)[1L]) |>
      plan_pages(max_rows = 10L) |> plan_style(border = "tfl") |> plan_tables(),
    "group_safe")
})

test_that("blank rows between groups render identically", {
  d <- .demog()
  .expect_same(
    as_rtftables(d, group_col = 1L, blank_rows = "between_groups",
                 border = "tfl"),
    rtf_plan_from(d) |> plan_group(names(d)[1L]) |>
      plan_blanks("between_groups") |> plan_style(border = "tfl") |>
      plan_tables(),
    "blanks")
})

test_that("the PK table -- two-level stub, indent grouping, pagination", {
  pk <- .pk()
  .expect_same(
    as_rtftables(pk, stub_vars = c("Time", "Statistic"),
                 split = "group_safe", max_rows = 21L, group_by = "indent",
                 border = "tfl"),
    rtf_plan_from(pk) |> plan_stub(c("Time", "Statistic")) |>
      plan_group("Time", mode = "indent") |> plan_pages(max_rows = 21L) |>
      plan_style(border = "tfl") |> plan_tables(),
    "pk_stub")
})

test_that("the PK table with blank rows between time points", {
  pk <- .pk()
  .expect_same(
    as_rtftables(pk, stub_vars = c("Time", "Statistic"),
                 split = "group_safe", max_rows = 21L, group_by = "indent",
                 blank_rows = "between_groups", border = "tfl"),
    rtf_plan_from(pk) |> plan_stub(c("Time", "Statistic")) |>
      plan_group("Time", mode = "indent") |> plan_blanks("between_groups") |>
      plan_pages(max_rows = 21L) |> plan_style(border = "tfl") |> plan_tables(),
    "pk_blanks")
})

test_that("a hidden carrier column that groups but is never printed", {
  d <- .lab()
  d$CARRIER <- rep(c("g1", "g2"), length.out = nrow(d))
  .expect_same(
    as_rtftables(d, group_col = "CARRIER", drop_cols = "CARRIER",
                 blank_rows = "between_groups", border = "tfl"),
    rtf_plan_from(d) |> plan_group("CARRIER") |> plan_hide("CARRIER") |>
      plan_blanks("between_groups") |> plan_style(border = "tfl") |>
      plan_tables(),
    "hidden_carrier")
})

test_that("a gt source with a spanner, merged into a stub", {
  skip_if_not_installed("gt")
  pk  <- .pk()
  vis <- setdiff(names(pk), c("Time", "Statistic"))
  # the spanner covers the VISIT columns only -- it does not overlap the
  # columns the stub merges, which is the ordinary clinical layout
  g <- gt::gt(pk) |> gt::tab_spanner("Visit", gt::all_of(vis))
  .expect_same(
    as_rtftables(g, read_meta = TRUE, stub_vars = c("Time", "Statistic"),
                 border = "tfl"),
    rtf_plan_from(g) |> plan_stub(c("Time", "Statistic")) |>
      plan_style(border = "tfl") |> plan_tables(),
    "gt_spanner")
})

test_that("a spanner OVER the merged columns is a deliberate difference", {
  skip_if_not_installed("gt")
  # The source says "All" spans every column, including the two the stub
  # merges.  After the merge it should still span everything.
  #
  #   plan          "All" over final 1-3   (the map sends 1,2,3,4 -> 1,1,2,3)
  #   as_rtftables  an empty cell at 1, "All" over 2-3
  #
  # as_rtftables() loses the stub from the span because .prepend_stub_header()
  # unconditionally inserts an empty leading cell and shifts the rest right --
  # mechanical, and correct only while no existing span covered the merged
  # columns.  The plan's answer is the faithful one, so this is recorded as a
  # deliberate divergence rather than chased to byte-identity.
  d <- data.frame(Time = c("1 h", "1 h", "2 h"), Stat = c("n", "Mean", "n"),
                  V1 = c("1", "2", "3"), V2 = c("4", "5", "6"),
                  stringsAsFactors = FALSE)
  g <- gt::gt(d) |> gt::tab_spanner("All", gt::everything())
  a <- as_rtftables(g, read_meta = TRUE, stub_vars = c("Time", "Stat"),
                    border = "tfl")[[1L]]
  p <- rtf_plan_from(g) |> plan_stub(c("Time", "Stat")) |>
    plan_style(border = "tfl") |> plan_tables()

  expect_length(a$col_header[[1L]], 2L)          # empty cell + "All" over 2-3
  expect_identical(a$col_header[[1L]][[2L]]$from, 2L)

  expect_length(p[[1L]]$col_header[[1L]], 1L)    # "All" over the whole width
  expect_identical(p[[1L]]$col_header[[1L]][[1L]]$from, 1L)
  expect_identical(p[[1L]]$col_header[[1L]][[1L]]$to, 3L)

  # the leaf row is the same either way
  expect_identical(a$col_header[[2L]], p[[1L]]$col_header[[2L]])
})

# ── recorded gap ───────────────────────────────────────────────────────────

test_that("group_force is a strategy the plan does not express yet", {
  # as_rtftables(split = "group_force") fills to the budget but prefers a
  # group boundary, splitting a group only when forced: 8,6,8,4 where a plain
  # greedy fill gives 8,8,7,2.  The plan has "never split" (groups = "keep",
  # which matches group_safe exactly) and "split freely" (FALSE); the middle
  # strategy is a gap, recorded here so it is not mistaken for a difference.
  d <- .demog()
  a <- as_rtftables(d, split = "group_force", max_rows = 8L, group_col = 1L,
                    border = "tfl")
  b <- rtf_plan_from(d) |> plan_group(names(d)[1L]) |>
    plan_pages(max_rows = 8L, groups = "split") |>
    plan_style(border = "tfl") |> plan_tables()
  expect_false(identical(vapply(a, function(x) nrow(x$data), integer(1L)),
                         vapply(b, function(x) nrow(x$data), integer(1L))))
})

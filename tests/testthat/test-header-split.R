# Reconstructing spanning column headers from delimited data.frame names
# (ydisctools "____" and tfrmt "___tlang_delim___"); see as_rtftables(header_sep=).

ydisc_df <- function() {
  data.frame(
    group1 = "Hoge", group2 = "Sex", label = c("Male", "Female"),
    `cohort1____trt1` = c("10", "9"),
    `cohort1____trt2` = c("11", "8"),
    `cohort2____trt1` = c("12", "7"),
    `cohort2____trt2` = c("13", "6"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

test_that(".split_names_to_col_header builds bottom-aligned spanning rows", {
  h <- .split_names_to_col_header(names(ydisc_df()), .default_header_seps())
  expect_length(h, 2L)               # 2 header rows (depth 2)

  # Top row: empty span over id cols, then one span per cohort.
  top <- h[[1L]]
  labs <- vapply(top, function(c) c$label, character(1))
  pos  <- lapply(top, function(c) c$pos)
  expect_equal(labs, c("", "cohort1", "cohort2"))
  expect_equal(pos, list(c(1L, 3L), c(4L, 5L), c(6L, 7L)))

  # Bottom row: a plain character vector of leaf labels, one per column.
  expect_equal(h[[2L]],
               c("group1", "group2", "label", "trt1", "trt2", "trt1", "trt2"))
})

test_that("default header_sep recognizes the tfrmt delimiter", {
  nm <- c("label",
          "cohort1___tlang_delim___trt1", "cohort1___tlang_delim___trt2")
  h <- .split_names_to_col_header(nm, .default_header_seps())
  expect_length(h, 2L)
  expect_equal(vapply(h[[1L]], function(c) c$label, character(1)),
               c("", "cohort1"))
  expect_equal(h[[2L]], c("label", "trt1", "trt2"))
})

test_that("names without a separator yield no spanning header (NULL)", {
  expect_null(.split_names_to_col_header(c("a", "b", "c"),
                                         .default_header_seps()))
})

test_that("header_sep = NULL or empty disables splitting", {
  expect_null(.split_names_to_col_header(names(ydisc_df()), NULL))
  expect_null(.split_names_to_col_header(names(ydisc_df()), character(0)))
  expect_null(.split_names_to_col_header(names(ydisc_df()), ""))
})

test_that("a custom separator overrides the defaults", {
  nm <- c("label", "A::x", "A::y", "B::x")
  h <- .split_names_to_col_header(nm, "::")
  expect_length(h, 2L)
  expect_equal(vapply(h[[1L]], function(c) c$label, character(1)),
               c("", "A", "B"))
  expect_equal(h[[2L]], c("label", "x", "y", "x"))
  # The default "____" would NOT split these names.
  expect_null(.split_names_to_col_header(nm, "____"))
})

test_that("doubled '____' inserts a blank middle cell (blank header row)", {
  nm <- c("label",
          "DrugA____Dose1____n", "DrugA____Dose2____n", "Overall________n")
  h <- .split_names_to_col_header(nm, "____")
  expect_length(h, 3L)                       # depth 3

  # Row 1: id blank, "DrugA" spanning the two Dose cols, "Overall".
  expect_equal(vapply(h[[1L]], function(c) c$label, character(1)),
               c("", "DrugA", "Overall"))
  expect_equal(lapply(h[[1L]], function(c) c$pos),
               list(1L, c(2L, 3L), 4L))

  # Row 2: id blank, Dose1, Dose2, and the empty middle segment for Overall.
  expect_equal(vapply(h[[2L]], function(c) c$label, character(1)),
               c("", "Dose1", "Dose2", ""))

  # Row 3 (leaf): every column is "n" except the id column label.
  expect_equal(h[[3L]], c("label", "n", "n", "n"))
})

test_that("as_rtftable() applies the reconstructed header end-to-end", {
  t <- as_rtftable(ydisc_df())
  # Header normalized to the internal (from, to) spanning form: 2 rows.
  expect_length(t$col_header, 2L)
  top <- t$col_header[[1L]]
  expect_equal(vapply(top, function(c) c$label, character(1)),
               c("", "cohort1", "cohort2"))
  expect_equal(top[[2L]]$from, 4L)
  expect_equal(top[[2L]]$to,   5L)
  expect_equal(t$col_header[[2L]],
               c("group1", "group2", "label", "trt1", "trt2", "trt1", "trt2"))
  # Body cells are untouched (7 columns, 2 rows).
  expect_equal(dim(t$data), c(2L, 7L))
})

test_that("an explicit col_header overrides the reconstructed one", {
  t <- as_rtftable(ydisc_df(),
                   col_header = c("a", "b", "c", "d", "e", "f", "g"))
  # A single-row header (no reconstructed cohort spanners).
  expect_length(t$col_header, 1L)
  expect_equal(t$col_header[[1L]],
               c("a", "b", "c", "d", "e", "f", "g"))
})

test_that("drop_cols reindexes the reconstructed spanning header", {
  # Drop the first id column; the cohort spans must shift left by one.
  pages <- as_rtftables(ydisc_df(), drop_cols = "group1")
  hdr <- pages[[1L]]$col_header
  top <- hdr[[1L]]
  # cohort1 now spans data columns 3..4, cohort2 5..6 (one fewer id column).
  spans <- lapply(Filter(function(c) nzchar(c$label), top),
                  function(c) c(c$from, c$to))
  expect_equal(spans, list(c(3L, 4L), c(5L, 6L)))
  expect_equal(hdr[[2L]],
               c("group2", "label", "trt1", "trt2", "trt1", "trt2"))
})

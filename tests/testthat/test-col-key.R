# col_key() / selector-function `pos` in col_cell()  (#451)

df <- data.frame(
  Severity           = c("Mild", "Severe"),
  `Placebo____Day 1` = c("1", "2"), `Placebo____Day 7` = c("3", "4"),
  `Drug A____Day 1`  = c("5", "6"), `Drug A____Day 7`  = c("7", "8"),
  `Total____Day 1`   = c("9", "0"), `Total____Day 7`   = c("1", "2"),
  check.names = FALSE, stringsAsFactors = FALSE
)
nm <- names(df)

# ──────── col_key() validation ────────────────────────────────────────────

test_that("col_key() returns a tagged selector function", {
  sel <- col_key("Placebo")
  expect_true(is.function(sel))
  expect_equal(attr(sel, "rtf_sel_label"), "col_key(\"Placebo\")")
  expect_equal(which(sel(nm)), c(2L, 3L))
})

test_that("col_key() rejects malformed arguments", {
  expect_error(col_key(character(0)), "non-empty strings")
  expect_error(col_key(""),           "non-empty strings")
  expect_error(col_key(NA),           "non-empty strings")
  expect_error(col_key("a", sep = c("_", "-")), "single non-empty string")
  expect_error(col_key("a", part = 0), "non-zero integer")
})

test_that("col_key() matches several keys and other segments", {
  expect_equal(which(col_key(c("Placebo", "Drug A"))(nm)), 2:5)
  expect_equal(which(col_key("Day 1", part = 2)(nm)), c(2L, 4L, 6L))
  expect_equal(which(col_key("Day 7", part = -1L)(nm)), c(3L, 5L, 7L))
})

test_that("col_key() ignores columns without the separator", {
  expect_false(col_key("Placebo")(nm)[1L])   # the "Severity" stub column
})

# ──────── resolution inside a header ──────────────────────────────────────

test_that("a col_key() cell spans the matched columns", {
  hdr <- list(list(col_cell(1L, ""),
                   col_cell(col_key("Placebo"), "Placebo"),
                   col_cell(col_key("Drug A"),  "Drug A"),
                   col_cell(col_key("Total"),   "Total")),
              nm)
  tbl  <- rtftable(df, col_header = hdr)
  span <- .pos_row_to_spans(hdr[[1L]], ncol(df), nm)
  fromto <- lapply(span, function(c) c(c$from, c$to, c$label))
  expect_equal(fromto[[1L]], c("1", "1", ""))
  expect_equal(fromto[[2L]], c("2", "3", "Placebo"))
  expect_equal(fromto[[3L]], c("4", "5", "Drug A"))
  expect_equal(fromto[[4L]], c("6", "7", "Total"))
  expect_s3_class(tbl, "rtftable")
})

test_that("a plain predicate function works as `pos`", {
  p <- .resolve_cell_pos(function(x) grepl("^Drug A____", x), nm)
  expect_equal(p, c(4L, 5L))
})

test_that("a selector may return positions or names", {
  expect_equal(.resolve_cell_pos(function(x) c(2L, 3L), nm), c(2L, 3L))
  expect_equal(.resolve_cell_pos(function(x) x[4:5], nm),    c(4L, 5L))
  expect_equal(.resolve_cell_pos(function(x) 6L, nm),        6L)
})

# ──────── errors ──────────────────────────────────────────────────────────

test_that("a selector that matches nothing errors and lists the keys", {
  expect_error(.resolve_cell_pos(col_key("Plcebo"), nm),
               "matched no data columns")
  expect_error(.resolve_cell_pos(col_key("Plcebo"), nm),
               "Available keys: \"Severity\", \"Placebo\", \"Drug A\", \"Total\"",
               fixed = TRUE)
  expect_error(.resolve_cell_pos(function(x) grepl("zzz", x), nm),
               "Available columns")
})

test_that("a selector matching non-adjacent columns errors", {
  expect_error(.resolve_cell_pos(col_key("Day 1", part = 2), nm),
               "non-adjacent data columns (2, 4, 6)", fixed = TRUE)
})

test_that("a selector needs the data column names", {
  expect_error(.resolve_cell_pos(col_key("Placebo"), NULL),
               "requires the header to be attached")
})

test_that("a selector must return a usable vector", {
  expect_error(.resolve_cell_pos(function(x) TRUE, nm),
               "returned 1 logical values for 7 data columns")
  expect_error(.resolve_cell_pos(function(x) "nope", nm),
               "unknown column name")
  expect_error(.resolve_cell_pos(function(x) list(1), nm),
               "must return a logical, integer or character vector")
  expect_error(.resolve_cell_pos(function(x) 99L, nm),
               "outside the data column range")
})

# ──────── construction / printing ─────────────────────────────────────────

test_that("col_cell() accepts a function without touching the names", {
  cc <- col_cell(col_key("Placebo"), "P")
  expect_s3_class(cc, "rtf_col_cell")
  expect_true(is.function(cc$pos))
  expect_output(print(cc), "col_key(\"Placebo\")", fixed = TRUE)
})

test_that("literal pos is still capped at length 2", {
  expect_error(col_cell(c(1, 2, 3)), "length 1 or 2")
  expect_error(col_cell(c(1, 2, 3)), "col_key()", fixed = TRUE)
})

test_that("rtf_col_header() prints a selector cell", {
  h <- rtf_col_header(list(col_cell(col_key("Placebo"), "P"), col_cell(2, "x")))
  expect_output(print(h), "col_key(\"Placebo\")", fixed = TRUE)
})

# ──────── interaction with the rest of the pipeline ───────────────────────

test_that("a selector cell is left alone by column-index remapping", {
  cc <- col_cell(col_key("Placebo"), "Placebo")
  expect_identical(.reindex_header_cell(cc, keep = c(1L, 4L, 5L)), cc)
})

test_that("a selector cell is not shifted when a stub column is prepended", {
  ch  <- list(list(col_cell(col_key("Placebo"), "P"), col_cell(2L, "x")),
              c("a", "b"))
  out <- .prepend_stub_header(ch, "Stub", 1L)
  expect_true(is.function(out[[1L]][[2L]]$pos))     # selector untouched
  expect_equal(out[[1L]][[3L]]$pos, 3L)             # literal shifted by one
})

test_that("paginate_cols() still refuses to cut inside a selector span", {
  tbl <- rtftable(df, col_header = list(
    list(col_cell(1L, ""), col_cell(col_key("Placebo"), "Placebo"),
         col_cell(col_key("Drug A"), "Drug A"),
         col_cell(col_key("Total"), "Total")),
    nm))
  expect_error(
    paginate_cols(tbl, at = 3L, carry = 1L, allow_span_break = FALSE),
    "spanning header cell")
  pages <- paginate_cols(tbl, at = c(4L, 6L), carry = 1L,
                         allow_span_break = FALSE)
  expect_length(pages, 3L)
})

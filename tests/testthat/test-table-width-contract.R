# The table-width vocabulary, and that as_rtftables() speaks it (#382).
#
#   column_widths_twips  per column, absolute -- wins outright
#   col_rel_width        per column, relative -- apportions the total
#   table_width_twips    the total, absolute
#   table_width_pct      the total, as a percentage of the writable width
#   nothing              the writable width, split equally
#
# `table_width_twips` and `table_width_pct` are two spellings of the same
# thing, so the absolute one wins; neither has anything to do with
# `auto_width`, which decides the columns rather than the total.

.wdf <- function() {
  data.frame(A = c("aaaa", "bb"), B = c("cccccccc", "d"),
             C = c("ee", "ffff"), stringsAsFactors = FALSE)
}

# Landscape Letter with the default 0.75in margins.
.WRITABLE <- as.integer((11 - 1.5) * 1440)

# The per-column widths of the widest table row in the rendered file.
.rendered_widths <- function(tbl) {
  doc <- rtf_document(page = list(orientation = "landscape"))
  doc <- rtf_section(doc, secinfo = list(header = rtf_header(list(c("T")))))
  doc <- rtf_tables(doc, list(tbl))
  path <- tempfile(fileext = ".rtf")
  on.exit(unlink(path), add = TRUE)
  generate_rtfreport(doc, path, overwrite = TRUE)
  txt  <- paste(readLines(path, warn = FALSE), collapse = "\n")
  rows <- strsplit(txt, "\\trowd", fixed = TRUE)[[1L]]
  cand <- lapply(rows, function(r) {
    as.integer(sub(".*cellx", "",
                   regmatches(r, gregexpr("\\\\cellx[0-9]+", r))[[1L]]))
  })
  n <- cand[[which.max(vapply(cand, length, integer(1L)))]]
  diff(c(0L, n))
}

.total <- function(tbl) sum(.rendered_widths(tbl))


# ── rtftable(): the renderer defines the contract ────────────────────────────

test_that("nothing given fills the writable width, split equally", {
  expect_identical(.rendered_widths(rtftable(.wdf())),
                   rep(.WRITABLE %/% 3L, 3L))
})

test_that("col_rel_width apportions the total", {
  expect_identical(.rendered_widths(rtftable(.wdf(),
                                             col_rel_width = c(3, 2, 1))),
                   c(6840L, 4560L, 2280L))
})

test_that("column_widths_twips is absolute and sets the total itself", {
  w <- c(1000L, 2000L, 3000L)
  expect_identical(.rendered_widths(rtftable(.wdf(),
                                             column_widths_twips = w)), w)
})

test_that("table_width_twips is the total, absolute", {
  expect_identical(.total(rtftable(.wdf(), table_width_twips = 6000L)), 6000L)
  expect_identical(
    .rendered_widths(rtftable(.wdf(), table_width_twips = 6000L,
                              col_rel_width = c(3, 2, 1))),
    c(3000L, 2000L, 1000L))
})

test_that("table_width_pct is the total, as a share of the writable width", {
  expect_identical(.total(rtftable(.wdf(), table_width_pct = 50)),
                   as.integer(.WRITABLE / 2))
  expect_identical(
    .rendered_widths(rtftable(.wdf(), table_width_pct = 50,
                              col_rel_width = c(3, 2, 1))),
    c(3420L, 2280L, 1140L))
})

test_that("the absolute total wins over the percentage", {
  expect_identical(
    .rendered_widths(rtftable(.wdf(), table_width_twips = 6000L,
                              table_width_pct = 50,
                              col_rel_width = c(3, 2, 1))),
    c(3000L, 2000L, 1000L))
})


# ── as_rtftables() must say the same thing ───────────────────────────────────

.one <- function(...) as_rtftables(.wdf(), ...)[[1L]]

test_that("as_rtftables() honours an absolute total width", {
  # It used to read `table_width_twips` only inside the auto_width branch, so
  # this asked for 6000 and rendered 13680 (#382).
  expect_identical(.total(.one(table_width_twips = 6000L)), 6000L)
  expect_identical(
    .rendered_widths(.one(table_width_twips = 6000L,
                          col_rel_width = c(3, 2, 1))),
    c(3000L, 2000L, 1000L))
})

test_that("as_rtftables() matches rtftable() on every spelling", {
  same <- function(...) {
    expect_identical(.rendered_widths(.one(...)),
                     .rendered_widths(rtftable(.wdf(), ...)))
  }
  same(col_rel_width = c(3, 2, 1))
  same(column_widths_twips = c(1000L, 2000L, 3000L))
  same(table_width_twips = 6000L)
  same(table_width_twips = 6000L, col_rel_width = c(3, 2, 1))
  same(table_width_pct = 50, col_rel_width = c(3, 2, 1))
  same(table_width_twips = 6000L, table_width_pct = 50,
       col_rel_width = c(3, 2, 1))
})

test_that("auto_width sizes the columns within the total it is given", {
  bare <- .total(.one(auto_width = TRUE))
  expect_lt(bare, .WRITABLE)                     # natural content width

  expect_identical(.total(.one(auto_width = TRUE,
                               table_width_twips = 6000L)), 6000L)

  # A percentage is a total too, and auto_width used to ignore it (#382).
  expect_identical(.total(.one(auto_width = TRUE, table_width_pct = 50)),
                   as.integer(.WRITABLE / 2))
  expect_identical(.total(.one(auto_width = TRUE,
                               table_width_pct_of_writable = 0.5)),
                   as.integer(.WRITABLE / 2))
})

test_that("an explicit per-column width still beats auto_width", {
  w <- c(1000L, 2000L, 3000L)
  expect_identical(.rendered_widths(.one(auto_width = TRUE,
                                         column_widths_twips = w)), w)
  expect_identical(.rendered_widths(.one(auto_width = TRUE,
                                         col_rel_width = c(3, 2, 1))),
                   c(6840L, 4560L, 2280L))
})

test_that("a listing reaches the same contract", {
  d <- data.frame(USUBJID = c("01-701-1015", "01-701-1023"),
                  HIST = c("ADENOCARCINOMA", "SMALL CELL"),
                  stringsAsFactors = FALSE)
  spec <- listing_spec(list(listing_col("USUBJID", width = 11),
                            listing_col("HIST", width = 16)),
                       spacer = FALSE)
  expect_identical(.total(as_rtftables(d, listing = spec,
                                       table_width_twips = 6000L)[[1L]]),
                   6000L)
})

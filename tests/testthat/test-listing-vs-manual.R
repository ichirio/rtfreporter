# build_listing() against the hand-written layout it replaces.
#
# `helper-listing-manual.R` lays a wide adverse-event listing out the way a
# statistical programmer writes it by hand: columns joined with "/", cells
# wrapped at the separators and then at word boundaries, every column of a
# record padded to its tallest cell, a blank line per record and the S01..S09
# gutter columns -- and holds the made-up data.
#
# The claim this feature makes is that build_listing() does the same
# reshaping automatically.  These tests check it cell by cell, and pin the four
# places where the behaviour is deliberately NOT the same.
#
# Scope: the listing's body, laid out -- cutting it into pages is
# as_rtftables()'s job (`split = "group_safe"`), covered in test-listing.R.

spec_ae <- function(...) {
  listing_spec(list(
    listing_col("USUBJID", width = 12),
    listing_col(c("AEBODSYS", "AEDECOD"),       width = 24, name = "COL01"),
    listing_col("ASTDT",                                    name = "COL02"),
    listing_col("ASTDY",                                    name = "COL03"),
    listing_col(c("AESEV", "AESER", "AEREL"),   width = 16, name = "COL04"),
    listing_col(c("AEACN", "AEOUT"),            width = 18, name = "COL05"),
    listing_col("AETOXGR",                                  name = "COL06"),
    listing_col(c("DOSE", "DOSEPRV"),           width = 8,  name = "COL07"),
    listing_col("TRTA",                                     name = "COL08"),
    listing_col("AECONTRT",                                 name = "COL09")
  ), ...)
}

# The hand-written layout carries its blank rows as NA and build_listing() as "",
# and both print as an empty cell.  Compare on the printed text.
as_text <- function(df) {
  out <- lapply(df, function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  })
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}


test_that("build_listing() reproduces the hand-written layout cell for cell", {
  adae <- .listing_ae()

  manual <- .manual_listing(adae, .listing_cols_ae(), .listing_widths_ae(),
                            blank_first = TRUE)
  auto   <- build_listing(adae, spec_ae())

  # The record column is bookkeeping for the page split, not a printed column.
  auto_printed <- auto[setdiff(names(auto), ".rtf_record")]

  # 10 printed columns with 9 gutters between them, in both.
  expect_identical(ncol(manual), 19L)
  expect_identical(ncol(auto_printed), 19L)

  # The hand-written layout opens with a blank row of its own;
  # build_listing() leaves that to as_rtftables(blank_row_first = ), which puts
  # one at the top of EVERY page rather than only the first.  Everything after
  # it must match, cell for cell.
  expect_true(all(unlist(as_text(manual)[1L, ]) == ""))
  expect_equal(as_text(auto_printed),
               `rownames<-`(as_text(manual)[-1L, ], NULL),
               ignore_attr = "names")
})

test_that("the two agree on how tall each record is", {
  adae   <- .listing_ae()
  manual <- .manual_listing(adae, .listing_cols_ae(), .listing_widths_ae(),
                            blank_first = FALSE)
  auto   <- build_listing(adae, spec_ae())

  expect_identical(nrow(auto), nrow(manual))

  # ... and build_listing()'s record column really does mark those blocks: one
  # id per source row, each block as long as the hand-written layout made it.
  expect_identical(length(unique(auto$.rtf_record)), nrow(adae))
  expect_identical(unname(table(auto$.rtf_record)[order(unique(auto$.rtf_record))]),
                   unname(table(auto$.rtf_record)))
})

test_that("the wrapping itself agrees, cell by cell, on every joined column", {
  adae <- .listing_ae()
  cols <- .listing_cols_ae()
  wrap <- .listing_widths_ae()

  for (k in names(cols)) {
    joined <- .hand_join("/", adae[cols[[k]]])
    for (i in seq_along(joined)) {
      w <- wrap[[k]]
      # An empty cell is the one per-cell difference: see the test below.
      if (!nzchar(joined[i])) next
      expected <- if (is.null(w)) joined[i] else .hand_wrap(joined[i], w)
      expect_identical(
        .listing_wrap_sep_word(joined[i], w, "/"),
        expected,
        info = sprintf("column %s, record %d: %s", k, i, joined[i]))
    }
  }
})

test_that("a missing value is skipped, not printed as a doubled separator", {
  adae <- .listing_ae()
  auto <- build_listing(adae, spec_ae())

  # Record 2 has no AESER: "MODERATE/" then the relationship, never "//".
  expect_false(any(grepl("//", auto$COL04, fixed = TRUE)))
  # Record 6 has neither AESER nor AEREL, so its joined cell is the severity alone.
  rows6 <- which(auto$.rtf_record == 6L)
  expect_identical(auto$COL04[rows6[1L]], "MILD")
})

test_that("the gutter columns are blank everywhere, in both", {
  adae   <- .listing_ae()
  manual <- as_text(.manual_listing(adae, .listing_cols_ae(),
                                    .listing_widths_ae(), blank_first = FALSE))
  auto   <- build_listing(adae, spec_ae())

  gutters_manual <- grep("^S[0-9]{2}$", names(manual), value = TRUE)
  gutters_auto   <- grep("^\\.sp[0-9]+$", names(auto), value = TRUE)
  expect_length(gutters_manual, 9L)
  expect_length(gutters_auto, 9L)
  expect_true(all(vapply(manual[gutters_manual],
                         function(x) all(x == ""), logical(1L))))
  expect_true(all(vapply(auto[gutters_auto],
                         function(x) all(x == ""), logical(1L))))
})


# ── Where the two deliberately differ ────────────────────────────────────────

test_that("a token wider than the column is hard-split, not left to overflow", {
  # The hand-written rule pushes the empty accumulator before starting on a
  # word that is by itself longer than the width, so the cell opens with a
  # blank line AND the token still overflows the column.  Both are wrong: the
  # blank line makes the record a row taller than it needs to be, and the
  # overflowing token makes Word add a row this package did not count (#364).
  expect_identical(.hand_wrap("ABCDEFGHIJKLMNOP", 8),
                   c("", "ABCDEFGHIJKLMNOP"))
  expect_identical(.listing_wrap_sep_word("ABCDEFGHIJKLMNOP", 8, "/"),
                   c("ABCDEFGH", "IJKLMNOP"))
})

test_that("the hand-written rule counts characters, not display width", {
  # It would let a Japanese cell ask for 8 columns and occupy 16.
  jp <- "肺腺癌ステージIIIB"
  manual <- .hand_wrap(jp, 8)
  expect_true(any(listing_disp_width(manual) > 8))    # a line 18 columns wide
  expect_true(all(listing_disp_width(
    .listing_wrap_sep_word(jp, 8, "/")) <= 8))         # every line fits
})

test_that("an empty cell still occupies its row, so the blank row survives", {
  # The hand-written rule gives "" NO lines at all, so a record whose every
  # wrapped column is empty is 0 + 1 = 1 row tall -- the values, and no
  # blank row after them.  That record then runs straight into the next one,
  # and (once this reaches as_rtftables()) there is no record boundary for the
  # page split to respect either.  An empty cell is one empty line here.
  expect_length(.hand_wrap("", 10), 0L)
  expect_identical(.listing_wrap_sep_word("", 10, "/"), "")

  d <- data.frame(A = c("x", NA), B = c("keep", "keep2"),
                  stringsAsFactors = FALSE)
  cols <- list(COLA = "A", COLB = "B")
  manual <- .manual_listing(d, cols, list(COLA = 10), blank_first = FALSE)
  auto   <- build_listing(d, listing_spec(list(
    listing_col("A", width = 10, name = "COLA"),
    listing_col("B", name = "COLB"))))

  expect_identical(nrow(manual), 3L)   # x, blank, keep2  -- no trailing blank
  expect_identical(nrow(auto), 4L)     # x, blank, keep2, blank
  expect_identical(auto$.rtf_record, c(1L, 1L, 2L, 2L))
})

test_that("a newline already in the data is honoured", {
  # The hand-written rule has no notion of one, so it stays inside the cell.
  expect_identical(.hand_wrap("one\ntwo", 20), "one\ntwo")
  expect_identical(.listing_wrap_sep_word("one\ntwo", 20, "/"), c("one", "two"))
})


# ── ... and the whole thing still renders ────────────────────────────────────

test_that("the adverse-event listing renders end to end, records kept whole", {
  adae <- .listing_ae()
  path <- tempfile(fileext = ".rtf")
  on.exit(unlink(path), add = TRUE)

  tbls <- as_rtftables(adae, listing = spec_ae(), max_rows = 12)
  expect_gt(length(tbls), 1L)
  expect_true(all(vapply(tbls, function(t) nrow(t$data), integer(1L)) <= 12L))
  expect_true(all(vapply(tbls, function(t) ncol(t$data), integer(1L)) == 19L))

  doc <- rtf_document(page = list(orientation = "landscape")) |>
    rtf_section(secinfo = list(
      header = rtf_header(list(c("Listing 16.2.7.1"),
                               c("Adverse Events"))))) |>
    rtf_tables(tbls)
  generate_rtfreport(doc, path, overwrite = TRUE)

  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_true(grepl("XYZ-101-0001", txt, fixed = TRUE))
})

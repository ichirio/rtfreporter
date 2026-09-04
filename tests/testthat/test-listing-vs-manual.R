# build_listing() against the hand-written pipeline it replaces.
#
# Discussion #356 is the code a statistical programmer writes today for a wide
# baseline-characteristics listing: catx() joins, split_string() wrapping,
# get_max_element_counts() + pad_list_elements() padding, the S00..S08 gutter
# columns, and a blank row per record.  `helper-listing-manual.R` holds that
# pipeline (the three functions verbatim, the dplyr/tidyr glue in base R) and
# the ADSL-shaped test data.
#
# The claim this feature makes is that build_listing() does the same
# reshaping automatically.  These tests check it cell by cell, and pin the three
# places where the behaviour is deliberately NOT the same.
#
# Scope: up to `result` in that pipeline -- the splitting after it is
# as_rtftables()'s job (`split = "group_safe"`), which is covered in
# test-listing.R.

spec_356 <- function(...) {
  listing_spec(list(
    listing_col("USUBJID", width = 15),
    listing_col(c("DISPTPD", "BRCA", "HIST"),      width = 22, name = "COL01"),
    listing_col("INIDGCAT",                                    name = "COL02"),
    listing_col("STAGE",                                       name = "COL03"),
    listing_col(c("HISTGRD", "PRRAD", "PRANTNM2"), width = 18, name = "COL04"),
    listing_col(c("CMBRFST", "CMBRLST"),           width = 20, name = "COL05"),
    listing_col("PRSRG",                                       name = "COL06"),
    listing_col(c("PPLATFI", "PLATFI"),            width = 18, name = "COL07"),
    listing_col("ECOGPS",                                      name = "COL08"),
    listing_col("FOLREVAL",                                    name = "COL09")
  ), ...)
}

# The manual `result` carries its blank rows as NA and build_listing() as "",
# and both print as an empty cell.  Compare on the printed text.
as_text <- function(df) {
  out <- lapply(df, function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  })
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}


test_that("build_listing() reproduces the hand-written pipeline cell for cell", {
  adsl <- .listing_adsl()

  manual <- .manual_listing(adsl, .listing_cols_356(), .listing_wrap_356(),
                            blank_first = TRUE)
  auto   <- build_listing(adsl, spec_356())

  # The record column is bookkeeping for the page split, not a printed column.
  auto_printed <- auto[setdiff(names(auto), ".rtf_record")]

  # 10 printed columns with 9 gutters between them, in both.
  expect_identical(ncol(manual), 19L)
  expect_identical(ncol(auto_printed), 19L)

  # `result` opens with the blank row bind_rows(df_blank, raw_df) adds;
  # build_listing() leaves that to as_rtftables(blank_row_first = ), which puts
  # one at the top of EVERY page rather than only the first.  Everything after
  # it must match, cell for cell.
  expect_true(all(unlist(as_text(manual)[1L, ]) == ""))
  expect_equal(as_text(auto_printed),
               `rownames<-`(as_text(manual)[-1L, ], NULL),
               ignore_attr = "names")
})

test_that("the two agree on how tall each record is", {
  adsl   <- .listing_adsl()
  manual <- .manual_listing(adsl, .listing_cols_356(), .listing_wrap_356(),
                            blank_first = FALSE)
  auto   <- build_listing(adsl, spec_356())

  expect_identical(nrow(auto), nrow(manual))

  # ... and build_listing()'s record column really does mark those blocks: one
  # id per source row, each block as long as the manual pipeline made it.
  expect_identical(length(unique(auto$.rtf_record)), nrow(adsl))
  expect_identical(unname(table(auto$.rtf_record)[order(unique(auto$.rtf_record))]),
                   unname(table(auto$.rtf_record)))
})

test_that("the wrapping itself agrees, cell by cell, on every joined column", {
  adsl <- .listing_adsl()
  cols <- .listing_cols_356()
  wrap <- .listing_wrap_356()

  for (k in names(cols)) {
    joined <- do.call(.ydisc_catx, c(list("/"), unname(as.list(adsl[cols[[k]]]))))
    for (i in seq_along(joined)) {
      w <- wrap[[k]]
      # An empty cell is the one per-cell difference: see the test below.
      if (!nzchar(joined[i])) next
      expected <- if (is.null(w)) list(joined[i]) else split_string(joined[i], w)
      expect_identical(
        .listing_wrap_sep_word(joined[i], w, "/"),
        as.character(unlist(expected)),
        info = sprintf("column %s, record %d: %s", k, i, joined[i]))
    }
  }
})

test_that("a missing value is skipped, not printed as a doubled separator", {
  adsl <- .listing_adsl()
  auto <- build_listing(adsl, spec_356())

  # Record 2 has no BRCA: "COMPLETED/" then the histology, never "//".
  expect_false(any(grepl("//", auto$COL01, fixed = TRUE)))
  # Record 6 has neither BRCA nor HIST, so its joined cell is the status alone.
  rows6 <- which(auto$.rtf_record == 6L)
  expect_identical(auto$COL01[rows6[1L]], "DISCONTINUED")
})

test_that("the gutter columns are blank everywhere, in both", {
  adsl   <- .listing_adsl()
  manual <- as_text(.manual_listing(adsl, .listing_cols_356(),
                                    .listing_wrap_356(), blank_first = FALSE))
  auto   <- build_listing(adsl, spec_356())

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

test_that("an unbreakable token gets its own line instead of an empty one", {
  # The hand-written rule pushes the empty accumulator before starting on a
  # word that is by itself longer than the width, so the cell opens with a
  # blank line and the record is a row taller than it needs to be.
  expect_identical(unlist(split_string("ABCDEFGHIJKLMNOP", 8)),
                   c("", "ABCDEFGHIJKLMNOP"))
  expect_identical(.listing_wrap_sep_word("ABCDEFGHIJKLMNOP", 8, "/"),
                   "ABCDEFGHIJKLMNOP")
})

test_that("an empty cell still occupies its row, so the blank row survives", {
  # split_string("") returns NO lines at all, so a record whose every wrapped
  # column is empty is padded to ROW_NUM + 1 = 1 row -- the values, and no
  # blank row after them.  That record then runs straight into the next one,
  # and (once this reaches as_rtftables()) there is no record boundary for the
  # page split to respect either.  An empty cell is one empty line here.
  expect_length(unlist(split_string("", 10)), 0L)
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
  expect_identical(unlist(split_string("one\ntwo", 20)), "one\ntwo")
  expect_identical(.listing_wrap_sep_word("one\ntwo", 20, "/"), c("one", "two"))
})


# ── ... and the whole thing still renders ────────────────────────────────────

test_that("the #356 listing renders end to end, records kept whole", {
  adsl <- .listing_adsl()
  path <- tempfile(fileext = ".rtf")
  on.exit(unlink(path), add = TRUE)

  tbls <- as_rtftables(adsl, listing = spec_356(), max_rows = 12)
  expect_gt(length(tbls), 1L)
  expect_true(all(vapply(tbls, function(t) nrow(t$data), integer(1L)) <= 12L))
  expect_true(all(vapply(tbls, function(t) ncol(t$data), integer(1L)) == 19L))

  doc <- rtf_document(page = list(orientation = "landscape")) |>
    rtf_section(secinfo = list(
      header = rtf_header(list(c("Listing 16.2.4.2.1.2"),
                               c("Baseline Characteristics"))))) |>
    rtf_tables(tbls)
  generate_rtfreport(doc, path, overwrite = TRUE)

  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_true(grepl("63016-204-1015", txt, fixed = TRUE))
})

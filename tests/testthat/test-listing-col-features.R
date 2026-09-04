# listing_col(): headers derived from the data, "flow" layout, and marking a
# key column for repeat suppression (#366).

.labelled_adsl <- function() {
  d <- data.frame(
    USUBJID = c("01-701-1015", "01-701-1015", "01-701-1023"),
    AGE     = c("40", "40", "63"),
    SEX     = c("F", "F", "M"),
    HIST    = c("ADENOCARCINOMA", "SQUAMOUS CELL CARCINOMA OF THE LUNG",
                "SMALL CELL"),
    VISIT   = c("SCREENING", "WEEK 4", "SCREENING"),
    stringsAsFactors = FALSE
  )
  attr(d$USUBJID, "label") <- "Unique Subject Identifier"
  attr(d$AGE,     "label") <- "Age"
  attr(d$SEX,     "label") <- "Sex"
  attr(d$HIST,    "label") <- "Histology"
  d
}

.labels_of <- function(body) {
  vapply(attr(body, "rtf_listing", exact = TRUE)$cols,
         function(cl) cl$label, character(1L))
}


# ── the header comes from the data ───────────────────────────────────────────

test_that("an omitted label is derived from the columns' label attributes", {
  body <- build_listing(.labelled_adsl(),
                        listing_spec(list(listing_col("HIST", width = 20))))
  expect_identical(unname(.labels_of(body)), "Histology")
})

test_that("a column with no label attribute falls back to its name", {
  body <- build_listing(.labelled_adsl(),
                        listing_spec(list(listing_col("VISIT", width = 20))))
  expect_identical(unname(.labels_of(body)), "VISIT")
})

test_that("several source columns give one label per line, separator kept", {
  body <- build_listing(.labelled_adsl(),
                        listing_spec(list(listing_col(c("AGE", "SEX"),
                                                      width = 20))))
  expect_identical(unname(.labels_of(body)), "Age/\nSex")
})

test_that("a derived header is wrapped to the column, so it cannot be wider", {
  body <- build_listing(.labelled_adsl(),
                        listing_spec(list(listing_col("USUBJID", width = 8))))
  lines <- strsplit(unname(.labels_of(body)), "\n", fixed = TRUE)[[1L]]
  expect_gt(length(lines), 1L)
  expect_true(all(.listing_disp_width(lines) <= 8))
})

test_that("a label written by hand is used exactly as written", {
  # Deliberately wider than the column: the author laid the lines out.
  hand <- "A header the author wrote"
  body <- build_listing(.labelled_adsl(),
                        listing_spec(list(listing_col("HIST", width = 6,
                                                      label = hand))))
  expect_identical(unname(.labels_of(body)), hand)
})

test_that("label = \"\" asks for a deliberately empty header", {
  body <- build_listing(.labelled_adsl(),
                        listing_spec(list(listing_col("HIST", label = ""))))
  expect_identical(unname(.labels_of(body)), "")
})

test_that("the derived header reaches the rendered table", {
  tbl <- as_rtftables(.labelled_adsl(),
                      listing = listing_spec(list(
                        listing_col("HIST", width = 20),
                        listing_col("VISIT", width = 12))))[[1L]]
  expect_identical(unlist(tbl$col_header[[1L]])[c(1L, 3L)],
                   c("Histology", "VISIT"))
})


# ── layout: stack / flow ─────────────────────────────────────────────────────

test_that("\"stack\" breaks after every separator, \"flow\" fills the line", {
  expect_identical(.listing_wrap_sep_word("40/F", 20, "/", "stack"),
                   c("40/", "F"))
  expect_identical(.listing_wrap_sep_word("40/F", 20, "/", "flow"), "40/F")
})

test_that("\"flow\" still breaks once the line is full", {
  expect_identical(
    .listing_wrap_sep_word("40/F/SCREENING/COMPLETED", 12, "/", "flow"),
    c("40/F/", "SCREENING/", "COMPLETED"))
})

test_that("with no width there is nothing to lay out, under either layout", {
  # `width` is what says how a column is laid out; without one both layouts
  # return the text as it stands, which is the behaviour that shipped.
  expect_identical(.listing_wrap_sep_word("40/F", NULL, "/", "stack"), "40/F")
  expect_identical(.listing_wrap_sep_word("40/F", NULL, "/", "flow"), "40/F")
})

test_that("layout is per column, and the listing's own is the default", {
  d <- data.frame(A = "40/F", B = "40/F", stringsAsFactors = FALSE)
  spec <- listing_spec(list(listing_col("A", width = 20),
                            listing_col("B", width = 20, layout = "flow")),
                       spacer = FALSE, blank_row = FALSE, record = FALSE)
  body <- build_listing(d, spec)
  expect_identical(body$A, c("40/", "F"))     # stacked: two rows
  expect_identical(body$B, c("40/F", ""))     # flowed: one, padded to match

  # ... and the listing-wide default can be flipped
  spec2 <- listing_spec(list(listing_col("A", width = 20)), layout = "flow",
                        spacer = FALSE, blank_row = FALSE, record = FALSE)
  expect_identical(build_listing(d, spec2)$A, "40/F")
})

test_that("layout is validated", {
  expect_error(listing_col("A", layout = "sideways"), "should be one of")
  expect_error(listing_spec("A", layout = "sideways"), "should be one of")
})


# ── collapse_repeats ─────────────────────────────────────────────────────────

test_that("a marked column is carried down its record's rows", {
  spec <- listing_spec(list(
    listing_col("USUBJID", width = 12, collapse_repeats = TRUE),
    listing_col("HIST", width = 16)))
  body <- build_listing(.labelled_adsl(), spec)

  # record 2 wraps to three lines; the id is on all three, not just the first
  rows <- which(body$.rtf_record == 2L)
  content <- rows[seq_len(length(rows) - 1L)]      # its trailing blank apart
  expect_length(content, 3L)
  expect_true(all(body$USUBJID[content] == "01-701-1015"))

  # the blank row that closes the record stays blank
  expect_identical(body$USUBJID[rows[length(rows)]], "")
})

test_that("an unmarked column is padded with blanks, as before", {
  spec <- listing_spec(list(listing_col("USUBJID", width = 12),
                            listing_col("HIST", width = 16)))
  body <- build_listing(.labelled_adsl(), spec)
  rows <- which(body$.rtf_record == 2L)
  expect_identical(body$USUBJID[rows], c("01-701-1015", "", "", ""))
})

test_that("a cell that already wraps is not carried down", {
  # There is no single value to repeat, so it is padded as usual.
  d <- data.frame(A = "AAAA BBBB CCCC", B = "x\ny\nz", stringsAsFactors = FALSE)
  spec <- listing_spec(list(listing_col("A", width = 4,
                                        collapse_repeats = TRUE),
                            listing_col("B", width = 4)),
                       spacer = FALSE, blank_row = FALSE, record = FALSE)
  body <- build_listing(d, spec)
  expect_identical(body$A, c("AAAA", "BBBB", "CCCC"))
})

test_that("as_rtftables blanks the repeats the marked column carried", {
  spec <- listing_spec(list(
    listing_col("USUBJID", width = 12, collapse_repeats = TRUE),
    listing_col("HIST", width = 16)))
  tbl <- as_rtftables(.labelled_adsl(), listing = spec)[[1L]]

  ids <- tbl$data[[1L]]
  ids[is.na(ids)] <- ""
  # the id prints once per record, not on every row of it
  expect_identical(sum(nzchar(ids)), 3L)          # 3 records, 3 printed ids
  expect_identical(ids[1L], "01-701-1015")
})

test_that("a caller's own collapse_repeats wins over the marked columns", {
  spec <- listing_spec(list(
    listing_col("USUBJID", width = 12, collapse_repeats = TRUE),
    listing_col("HIST", width = 16)))
  tbl <- as_rtftables(.labelled_adsl(), listing = spec,
                      collapse_repeats = "HIST")[[1L]]
  ids <- tbl$data[[1L]]
  ids[is.na(ids)] <- ""
  expect_gt(sum(nzchar(ids)), 3L)                 # the id was NOT suppressed
})

test_that("collapse_repeats is validated", {
  expect_error(listing_col("A", collapse_repeats = "yes"), "TRUE or FALSE")
  expect_error(listing_col("A", collapse_repeats = NA), "TRUE or FALSE")
})

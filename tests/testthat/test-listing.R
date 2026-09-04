# Tests for the listing preparation (#241): listing_col() / listing_spec() /
# build_listing(), and the as_rtftables(listing = ) hook.

adsl_demo <- function() {
  data.frame(
    USUBJID  = c("01-701-1015", "01-701-1023", "01-701-1028"),
    HIST     = c("ADENOCARCINOMA",
                 "SQUAMOUS CELL CARCINOMA OF THE LUNG",
                 "SMALL CELL"),
    BRCA     = c("BRCA1", NA, "BRCA2"),
    ARM      = c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"),
    stringsAsFactors = FALSE
  )
}

demo_spec <- function(...) {
  listing_spec(list(
    listing_col("USUBJID", width = 11, label = "Unique\nSubject ID"),
    listing_col(c("HIST", "BRCA"), width = 16, label = "Histology/\nMutation"),
    listing_col("ARM", width = 12, label = "Treatment Arm")
  ), ...)
}


# ── listing_col() ────────────────────────────────────────────────────────────

test_that("listing_col() validates and defaults its arguments", {
  cl <- listing_col("USUBJID")
  expect_s3_class(cl, "rtf_listing_col")
  expect_identical(cl$name, "USUBJID")     # first var names the output column
  expect_null(cl$width)                    # no wrapping unless asked

  expect_error(listing_col(character(0)), "one or more non-empty")
  expect_error(listing_col(1L), "one or more non-empty")
  expect_error(listing_col("A", width = 0), "positive number of characters")
  expect_error(listing_col("A", rel_width = -1), "positive number")
  expect_error(listing_col("A", label = c("a", "b")), "single string")
  expect_error(listing_col("A", align = "middle"), "should be one of")
})


# ── listing_spec() ───────────────────────────────────────────────────────────

test_that("listing_spec() resolves the template and lets arguments override it", {
  spec <- demo_spec()
  expect_s3_class(spec, "rtf_listing_spec")
  expect_identical(spec$sep, "/")
  expect_true(spec$spacer)
  expect_true(spec$blank_row)
  expect_identical(spec$align, "left")
  expect_identical(spec$record_col, ".rtf_record")

  over <- demo_spec(sep = " | ", spacer = FALSE, blank_row = FALSE,
                    align = "center", record = FALSE)
  expect_identical(over$sep, " | ")
  expect_false(over$spacer)
  expect_false(over$blank_row)
  expect_identical(over$align, "center")
  expect_null(over$record_col)

  expect_error(listing_spec(list(listing_col("A")), type = "nope"),
               "Unknown listing type")
  expect_error(listing_spec(list()), "non-empty list")
  expect_error(listing_spec(list(1L)), "must be a listing_col")
  expect_error(demo_spec(record = 1L), "TRUE, FALSE, or a single column name")
})

test_that("listing_spec() accepts bare column names and makes names unique", {
  spec <- listing_spec(c("USUBJID", "ARM"))
  expect_length(spec$cols, 2L)
  expect_identical(vapply(spec$cols, function(c1) c1$name, character(1L)),
                   c("USUBJID", "ARM"))

  # Two columns built from the same first variable must not collide.
  dup <- listing_spec(list(listing_col(c("A", "B")), listing_col(c("A", "C"))))
  expect_identical(vapply(dup$cols, function(c1) c1$name, character(1L)),
                   c("A", "A_1"))
})


# ── the wrapping rule ────────────────────────────────────────────────────────

test_that("a cell breaks after the separator first", {
  expect_identical(
    .listing_wrap_sep_word("STAGE II/GRADE 3", 12, "/"),
    c("STAGE II/", "GRADE 3"))
})

test_that("a piece still too long breaks at a word boundary", {
  expect_identical(
    .listing_wrap_sep_word("SQUAMOUS CELL CARCINOMA OF THE LUNG", 16, "/"),
    c("SQUAMOUS CELL", "CARCINOMA OF", "THE LUNG"))
})

test_that("a token wider than the column is hard-split, so every line fits", {
  # Not prettier, but honest (#364): `rel_width` follows `width`, so a token
  # wider than the column is wider than the RENDERED column, Word wraps it,
  # and the record would be a row taller than build_listing() counted.
  expect_identical(.listing_wrap_sep_word("ABCDEFGHIJKLMNOP", 8, "/"),
                   c("ABCDEFGH", "IJKLMNOP"))
  # every returned line fits the column it was measured against
  lines <- .listing_wrap_sep_word("63016-205-100028", 15, "/")
  expect_true(all(.listing_disp_width(lines) <= 15))
})

test_that("widths are display widths, so a full-width glyph counts as two", {
  jp <- "肺腺癌ステージIIIB"   # 11 chars, 18 columns
  expect_identical(nchar(jp), 11L)
  expect_identical(.listing_disp_width(jp), 18L)

  lines <- .listing_wrap_sep_word(jp, 8, "/")
  expect_gt(length(lines), 1L)                 # nchar() would have kept one
  expect_true(all(.listing_disp_width(lines) <= 8))
})

test_that("wrapping honours a newline already in the data, and NA is empty", {
  expect_identical(.listing_wrap_sep_word("one\ntwo", 20, "/"), c("one", "two"))
  expect_identical(.listing_wrap_sep_word(NA, 10, "/"), "")
  expect_identical(.listing_wrap_sep_word("", 10, "/"), "")
})

test_that("no width means no wrapping", {
  long <- "SQUAMOUS CELL CARCINOMA OF THE LUNG"
  expect_identical(.listing_wrap_sep_word(long, NULL, "/"), long)
})

test_that("a regex-special separator is treated literally", {
  expect_identical(.listing_wrap_sep_word("A.BBBBBBBB", 4, "."),
                   c("A.", "BBBB", "BBBB"))
})


# ── build_listing() ──────────────────────────────────────────────────────────

test_that("build_listing() joins, wraps, pads and separates records", {
  body <- build_listing(adsl_demo(), demo_spec())

  # 3 printed columns + 2 gutters + the record column
  expect_identical(names(body),
                   c("USUBJID", ".sp1", "HIST", ".sp2", "ARM", ".rtf_record"))

  # record 1 wraps to 2 lines, + 1 blank; record 2 to 3 lines, + 1; record 3
  # to 2 lines, + 1.
  expect_identical(nrow(body), 10L)
  expect_identical(body$.rtf_record, rep(1:3, times = c(3L, 4L, 3L)))

  # the joined column drops the missing BRCA of record 2, so no doubled "/"
  expect_identical(body$HIST[4:6],
                   c("SQUAMOUS CELL", "CARCINOMA OF", "THE LUNG"))
  expect_identical(body$HIST[1:2], c("ADENOCARCINOMA/", "BRCA1"))

  # every record ends with a blank row, and the gutters are blank throughout
  expect_identical(body$USUBJID[c(3L, 7L, 10L)], rep("", 3L))
  expect_true(all(body$.sp1 == "") && all(body$.sp2 == ""))
})

test_that("build_listing() honours spacer / blank_row / record switches", {
  body <- build_listing(adsl_demo(),
                        demo_spec(spacer = FALSE, blank_row = FALSE,
                                  record = FALSE))
  expect_identical(names(body), c("USUBJID", "HIST", "ARM"))
  expect_identical(nrow(body), 7L)          # 2 + 3 + 2 lines, no blank rows
})

test_that("build_listing() carries the spec and refuses to build twice", {
  spec <- demo_spec()
  body <- build_listing(adsl_demo(), spec)
  expect_identical(attr(body, "rtf_listing"), spec)
  expect_error(build_listing(body, spec), "already been through build_listing")
})

test_that("build_listing() reports a column that is not in the data", {
  expect_error(build_listing(adsl_demo(), listing_spec("NOPE")),
               "not in `data`")
})

test_that("build_listing() rejects a non-data.frame and a bad spec", {
  expect_error(build_listing(1:3, demo_spec()), "must be a data.frame")
  expect_error(build_listing(adsl_demo(), "spec"), "must be a listing_spec")
})

test_that("build_listing() handles zero rows", {
  empty <- adsl_demo()[0L, , drop = FALSE]
  body  <- build_listing(empty, demo_spec())
  expect_identical(nrow(body), 0L)
  expect_identical(names(body),
                   c("USUBJID", ".sp1", "HIST", ".sp2", "ARM", ".rtf_record"))
})

test_that("a factor source column is joined by its label, not its level code", {
  df <- data.frame(A = factor(c("high", "low")), stringsAsFactors = FALSE)
  body <- build_listing(df, listing_spec("A", blank_row = FALSE,
                                         record = FALSE))
  expect_identical(body$A, c("high", "low"))
})


# ── as_rtftables(listing = ) ─────────────────────────────────────────────────

test_that("the hook derives the header, the widths and left alignment", {
  tbls <- as_rtftables(adsl_demo(), listing = demo_spec())
  expect_length(tbls, 1L)
  tbl <- tbls[[1L]]

  # the record column is hidden, the gutters are not
  expect_identical(ncol(tbl$data), 5L)
  expect_identical(tbl$col_rel_width, c(11, 1, 16, 1, 12))
  expect_true(all(vapply(tbl$col_spec, function(s) s$align, character(1L)) ==
                    "left"))
})

test_that("the hook keeps a record whole across a page break", {
  # Records are 3, 4 and 3 rows; with room for 6 the second cannot join the
  # first, and each page carries whole records only.
  tbls <- as_rtftables(adsl_demo(), listing = demo_spec(), max_rows = 6)
  expect_length(tbls, 3L)
  expect_true(all(vapply(tbls, function(t) nrow(t$data), integer(1L)) <= 6L))

  # ... and none of them shows the record column that decided the breaks
  expect_true(all(vapply(tbls, function(t) ncol(t$data), integer(1L)) == 5L))
})

test_that("the hook never overrides an argument the caller passed", {
  spec <- demo_spec()
  tbl  <- as_rtftables(adsl_demo(), listing = spec,
                       col_rel_width = c(5, 1, 5, 1, 5))[[1L]]
  expect_identical(tbl$col_rel_width, c(5, 1, 5, 1, 5))

  # an explicit split is left alone (so no pagination happens without one)
  tbls <- as_rtftables(adsl_demo(), listing = spec, split = "none",
                       max_rows = 6)
  expect_length(tbls, 1L)
})

test_that("a body from build_listing() renders without repeating the spec", {
  body <- build_listing(adsl_demo(), demo_spec())
  tbl  <- as_rtftables(body)[[1L]]
  expect_identical(ncol(tbl$data), 5L)
  expect_identical(tbl$col_rel_width, c(11, 1, 16, 1, 12))

  expect_error(as_rtftables(body, listing = demo_spec()),
               "already built by build_listing")
})

test_that("as_rtftable() reaches the hook through its dots", {
  tbl <- as_rtftable(adsl_demo(), listing = demo_spec())
  expect_s3_class(tbl, "rtftable")
  expect_identical(ncol(tbl$data), 5L)
})

test_that("`listing` is refused where it cannot apply", {
  expect_error(as_rtftables(adsl_demo(), listing = "spec"),
               "must be a listing_spec")
})

test_that("a listing renders to RTF end to end", {
  path <- tempfile(fileext = ".rtf")
  on.exit(unlink(path), add = TRUE)

  tbls <- as_rtftables(adsl_demo(), listing = demo_spec(), max_rows = 6)
  doc  <- rtf_document() |>
    rtf_section(secinfo = list(header = rtf_header(list(c("Listing 16.2.4.1"))))) |>
    rtf_tables(tbls)
  generate_rtfreport(doc, path, overwrite = TRUE)

  expect_true(file.exists(path))
  txt <- readLines(path, warn = FALSE)
  expect_true(any(grepl("01-701-1015", txt, fixed = TRUE)))
})

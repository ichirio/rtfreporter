# fit_listing_widths() and listing_code() (#369): propose the widths from the
# page and the data, then hand back the source to paste and tune.

.fit_adsl <- function() {
  d <- data.frame(
    USUBJID  = sprintf("63016-20%d-10%02d", rep(4:6, each = 4), 1:12),
    DISPTPD  = rep(c("COMPLETED", "DISCONTINUED", "ONGOING"), 4),
    BRCA     = rep(c("BRCA1", NA, "BRCA2", NA), 3),
    HIST     = rep(c("ADENOCARCINOMA",
                     "SQUAMOUS CELL CARCINOMA OF THE LUNG"), 6),
    STAGE    = rep(c("IIIB", "IV", "IIIA", "IIB"), 3),
    stringsAsFactors = FALSE
  )
  attr(d$USUBJID, "label") <- "Unique Subject ID"
  attr(d$STAGE,   "label") <- "Stage at Initial Diagnosis"
  d
}

.plain_spec <- function(...) {
  listing_spec(list(
    listing_col("USUBJID"),
    listing_col(c("DISPTPD", "BRCA", "HIST")),
    listing_col("STAGE")
  ), ...)
}

.widths_of <- function(spec) {
  vapply(spec$cols, function(cl) as.integer(cl$width), integer(1L))
}


# ── the page decides the budget ──────────────────────────────────────────────

test_that("every column comes back with a width", {
  fitted <- fit_listing_widths(.fit_adsl(), .plain_spec(), total_width = 60)
  expect_s3_class(fitted, "rtf_listing_spec")
  expect_false(any(vapply(fitted$cols, function(cl) is.null(cl$width),
                          logical(1L))))
})

test_that("the widths and the gutters fill the budget exactly", {
  spec   <- .plain_spec()
  fitted <- fit_listing_widths(.fit_adsl(), spec, total_width = 60)
  gutters <- (length(spec$cols) - 1L) * spec$spacer_rel_width
  expect_identical(sum(.widths_of(fitted)) + gutters, 60)
})

test_that("the gutters are counted, and dropping them frees their width", {
  wide <- fit_listing_widths(.fit_adsl(), .plain_spec(spacer = FALSE),
                             total_width = 60)
  expect_identical(sum(.widths_of(wide)), 60L)
})

test_that("a wider page gives wider columns", {
  narrow <- fit_listing_widths(.fit_adsl(), .plain_spec(), total_width = 60)
  wide   <- fit_listing_widths(.fit_adsl(), .plain_spec(), total_width = 120)
  expect_true(all(.widths_of(wide) >= .widths_of(narrow)))
  expect_gt(sum(.widths_of(wide)), sum(.widths_of(narrow)))
})

test_that("the budget really does come from the paper, margins and font", {
  fit_on <- function(...) {
    attr(fit_listing_widths(.fit_adsl(), .plain_spec(),
                            page = rtf_page(...), size_half_points = 16L),
         "rtf_listing_fit", exact = TRUE)$total_width
  }
  a4_land <- fit_on(paper_size = "A4", orientation = "landscape",
                    margin_left_in = 0.5, margin_right_in = 0.5)
  a4_port <- fit_on(paper_size = "A4", orientation = "portrait",
                    margin_left_in = 0.5, margin_right_in = 0.5)
  wide_margins <- fit_on(paper_size = "A4", orientation = "landscape",
                         margin_left_in = 2, margin_right_in = 2)

  expect_gt(a4_land, a4_port)          # landscape has more room
  expect_gt(a4_land, wide_margins)     # so does a page with less margin

  # ... and a smaller font fits more characters across the same sheet
  small <- attr(fit_listing_widths(.fit_adsl(), .plain_spec(),
                                   page = rtf_page(paper_size = "A4"),
                                   size_half_points = 12L),
                "rtf_listing_fit", exact = TRUE)$total_width
  big <- attr(fit_listing_widths(.fit_adsl(), .plain_spec(),
                                 page = rtf_page(paper_size = "A4"),
                                 size_half_points = 24L),
              "rtf_listing_fit", exact = TRUE)$total_width
  expect_gt(small, big)
})


# ── what a column demands ────────────────────────────────────────────────────

test_that("a column with wider data gets a wider share", {
  fitted <- fit_listing_widths(.fit_adsl(), .plain_spec(), total_width = 120)
  w <- .widths_of(fitted)
  expect_gt(w[2L], w[3L])       # the joined diagnosis beats the stage code
})

test_that("a long header does not claim a column the data does not need", {
  # "Stage at Initial Diagnosis" is 26 characters but wraps; the floor is its
  # widest unbreakable token, not its length.
  fitted <- fit_listing_widths(.fit_adsl(), .plain_spec(), total_width = 120)
  demand <- attr(fitted, "rtf_listing_fit", exact = TRUE)$demand
  expect_lt(unname(demand[["STAGE"]]), 26)
  expect_gte(unname(demand[["STAGE"]]), 9)   # "Diagnosis" cannot break
})

test_that("one very long value does not dominate the layout", {
  d <- .fit_adsl()
  d$STAGE[1L] <- paste(rep("X", 200), collapse = "")
  demand <- attr(fit_listing_widths(d, .plain_spec(), total_width = 120),
                 "rtf_listing_fit", exact = TRUE)$demand
  expect_lt(unname(demand[["STAGE"]]), 200)  # the quantile, not the maximum

  # the maximum IS reachable, by asking for it
  demand_max <- attr(fit_listing_widths(d, .plain_spec(), total_width = 120,
                                        probs = 1), "rtf_listing_fit",
                     exact = TRUE)$demand
  expect_gte(unname(demand_max[["STAGE"]]), 200)
})

test_that("no column is fitted narrower than min_width", {
  fitted <- fit_listing_widths(.fit_adsl(), .plain_spec(), total_width = 30,
                               min_width = 8)
  expect_true(all(.widths_of(fitted) >= 8L))
})


# ── a width you set is a decision, not a proposal ────────────────────────────

test_that("an explicit width is kept, and the rest fit around it", {
  spec <- listing_spec(list(
    listing_col("USUBJID", width = 20),
    listing_col(c("DISPTPD", "BRCA", "HIST")),
    listing_col("STAGE")))
  fitted <- fit_listing_widths(.fit_adsl(), spec, total_width = 60)
  w <- .widths_of(fitted)
  expect_identical(w[1L], 20L)
  expect_identical(sum(w) + 2, 60)      # + two gutters
})

test_that("a spec whose widths are all set comes back unchanged", {
  spec <- listing_spec(list(listing_col("USUBJID", width = 11),
                            listing_col("STAGE", width = 8)))
  fitted <- fit_listing_widths(.fit_adsl(), spec, total_width = 60)
  expect_identical(.widths_of(fitted), c(11L, 8L))
})


# ── validation ───────────────────────────────────────────────────────────────

test_that("an impossible budget is refused with an explanation", {
  expect_error(
    fit_listing_widths(.fit_adsl(), .plain_spec(), total_width = 8),
    "cannot each be")
})

test_that("the arguments are validated", {
  spec <- .plain_spec()
  expect_error(fit_listing_widths(1:3, spec), "must be a data.frame")
  expect_error(fit_listing_widths(.fit_adsl(), "spec"), "must be a listing_spec")
  expect_error(fit_listing_widths(.fit_adsl(), spec, total_width = 60,
                                  min_width = 0), "positive integer")
  expect_error(fit_listing_widths(.fit_adsl(), spec, total_width = 60,
                                  probs = 2), "in \\[0, 1\\]")
  body <- build_listing(.fit_adsl(), fit_listing_widths(.fit_adsl(), spec,
                                                        total_width = 60))
  expect_error(fit_listing_widths(body, spec), "already been through")
})


# ── listing_code() ───────────────────────────────────────────────────────────

test_that("the code round-trips: parse it and get the same widths back", {
  fitted <- fit_listing_widths(.fit_adsl(), .plain_spec(), total_width = 90)
  code   <- listing_code(fitted)
  again  <- eval(parse(text = paste(code, collapse = "\n")))

  expect_s3_class(again, "rtf_listing_spec")
  expect_identical(.widths_of(again), .widths_of(fitted))
  expect_identical(vapply(again$cols, function(cl) cl$name, character(1L)),
                   vapply(fitted$cols, function(cl) cl$name, character(1L)))
})

test_that("the code assigns to a name when asked", {
  code <- listing_code(.plain_spec(), name = "listing")
  expect_true(startsWith(code[[1L]], "listing <- listing_spec(list("))
  expect_s3_class(code, "rtf_listing_code")
})

test_that("only what differs from the defaults is written out", {
  plain <- paste(listing_code(.plain_spec()), collapse = "\n")
  expect_false(grepl("type = ", plain, fixed = TRUE))
  expect_false(grepl("spacer = ", plain, fixed = TRUE))
  expect_false(grepl("blank_row = ", plain, fixed = TRUE))

  odd <- paste(listing_code(.plain_spec(spacer = FALSE, sep = " | ",
                                        record = FALSE)),
               collapse = "\n")
  expect_true(grepl("spacer = FALSE", odd, fixed = TRUE))
  expect_true(grepl('sep = " | "', odd, fixed = TRUE))
  expect_true(grepl("record = FALSE", odd, fixed = TRUE))
})

test_that("a multi-line label survives the round trip", {
  spec <- listing_spec(list(
    listing_col("USUBJID", width = 11, label = "Unique\nSubject ID")))
  code  <- paste(listing_code(spec), collapse = "\n")
  expect_true(grepl('label = "Unique\\\\nSubject ID"', code))
  again <- eval(parse(text = code))
  expect_identical(again$cols[[1L]]$label, "Unique\nSubject ID")
})

test_that("the per-column settings are written out", {
  spec <- listing_spec(list(
    listing_col("USUBJID", width = 11, collapse_repeats = TRUE),
    listing_col(c("AGE", "SEX"), width = 8, layout = "flow", name = "COL02",
                align = "center")))
  code <- paste(listing_code(spec), collapse = "\n")
  expect_true(grepl("collapse_repeats = TRUE", code, fixed = TRUE))
  expect_true(grepl('layout = "flow"', code, fixed = TRUE))
  expect_true(grepl('name = "COL02"', code, fixed = TRUE))
  expect_true(grepl('align = "center"', code, fixed = TRUE))
  expect_true(grepl('c("AGE", "SEX")', code, fixed = TRUE))
})

test_that("listing_code() validates its arguments", {
  expect_error(listing_code("spec"), "must be a listing_spec")
  expect_error(listing_code(.plain_spec(), name = ""), "non-empty string")
  expect_error(listing_code(.plain_spec(), indent = -1), "non-negative")
})


# ── end to end ───────────────────────────────────────────────────────────────

test_that("a fitted spec renders, and its widths reach the table", {
  fitted <- fit_listing_widths(
    .fit_adsl(), .plain_spec(),
    page = rtf_page(paper_size = "A4", orientation = "landscape",
                    margin_left_in = 0.5, margin_right_in = 0.5),
    size_half_points = 16L)

  tbl <- as_rtftables(.fit_adsl(), listing = fitted)[[1L]]
  expect_identical(ncol(tbl$data), 5L)          # 3 columns + 2 gutters
  expect_identical(tbl$col_rel_width,
                   as.numeric(c(rbind(.widths_of(fitted),
                                      c(1, 1, NA))))[1:5])
})


# ── listing_spec(labels = ): one table of labels for data that has none ──────

.unlabelled <- function() {
  data.frame(
    USUBJID = c("01-701-1015", "01-701-1023"),
    DISPTPD = c("COMPLETED", "ONGOING"),
    BRCA    = c("BRCA1", NA),
    HIST    = c("ADENOCARCINOMA", "SQUAMOUS CELL CARCINOMA OF THE LUNG"),
    stringsAsFactors = FALSE
  )
}

.spec_labels <- c(USUBJID = "Unique Subject ID",
                  DISPTPD = "Primary Diagnosis",
                  BRCA    = "Any (BRCA) Mutations",
                  HIST    = "Histology")

test_that("labels supply the header when the data carries none", {
  spec <- listing_spec(list(listing_col("USUBJID", width = 20)),
                       labels = .spec_labels)
  body <- build_listing(.unlabelled(), spec)
  expect_identical(attr(body, "rtf_listing", exact = TRUE)$cols[[1L]]$label,
                   "Unique Subject ID")
})

test_that("a joined column joins its labels, and the breaks stay automatic", {
  spec <- listing_spec(list(listing_col(c("DISPTPD", "BRCA", "HIST"),
                                        width = 22)),
                       labels = .spec_labels)
  body <- build_listing(.unlabelled(), spec)
  lab <- attr(body, "rtf_listing", exact = TRUE)$cols[[1L]]$label
  expect_identical(strsplit(lab, "\n", fixed = TRUE)[[1L]],
                   c("Primary Diagnosis/", "Any (BRCA) Mutations/", "Histology"))
})

test_that("precedence: listing_col(label) > labels > attribute > name", {
  d <- .unlabelled()
  attr(d$USUBJID, "label") <- "FROM THE DATA"

  by_col <- listing_spec(list(listing_col("USUBJID", width = 30,
                                          label = "FROM THE COLUMN")),
                         labels = .spec_labels)
  expect_identical(attr(build_listing(d, by_col), "rtf_listing")$cols[[1L]]$label,
                   "FROM THE COLUMN")

  by_lookup <- listing_spec(list(listing_col("USUBJID", width = 30)),
                            labels = .spec_labels)
  expect_identical(attr(build_listing(d, by_lookup), "rtf_listing")$cols[[1L]]$label,
                   "Unique Subject ID")

  by_attr <- listing_spec(list(listing_col("USUBJID", width = 30)))
  expect_identical(attr(build_listing(d, by_attr), "rtf_listing")$cols[[1L]]$label,
                   "FROM THE DATA")

  by_name <- listing_spec(list(listing_col("DISPTPD", width = 30)))
  expect_identical(attr(build_listing(d, by_name), "rtf_listing")$cols[[1L]]$label,
                   "DISPTPD")
})

test_that("a variable the lookup does not name still falls back", {
  spec <- listing_spec(list(listing_col("USUBJID", width = 20),
                            listing_col("DISPTPD", width = 20)),
                       labels = c(USUBJID = "Unique Subject ID"))
  cols <- attr(build_listing(.unlabelled(), spec), "rtf_listing")$cols
  expect_identical(cols[[1L]]$label, "Unique Subject ID")
  expect_identical(cols[[2L]]$label, "DISPTPD")
})

test_that("the width fit measures the supplied labels", {
  # "Any (BRCA) Mutations" cannot break below 9 ("Mutations"), so the column
  # cannot be fitted narrower than that.
  spec <- listing_spec(list(listing_col("BRCA"), listing_col("USUBJID")),
                       labels = .spec_labels, spacer = FALSE)
  fitted <- fit_listing_widths(.unlabelled(), spec, total_width = 30)
  expect_gte(fitted$cols[[1L]]$width, 9L)
})

test_that("labels are validated", {
  expect_error(listing_spec("USUBJID", labels = "no names"), "named character")
  expect_error(listing_spec("USUBJID", labels = c("Unnamed")), "named character")
  expect_error(listing_spec("USUBJID", labels = 1:2), "named character")
})

test_that("listing_code() writes the labels table out, and it round-trips", {
  spec <- listing_spec(list(listing_col("USUBJID", width = 11),
                            listing_col(c("DISPTPD", "BRCA"), width = 20)),
                       labels = .spec_labels)
  code <- listing_code(spec, name = "listing")

  expect_true(any(grepl("labels = c(", code, fixed = TRUE)))
  expect_true(any(grepl('USUBJID = "Unique Subject ID"', code, fixed = TRUE)))

  again <- eval(parse(text = paste(code, collapse = "\n")))
  expect_identical(again$labels, spec$labels)
  expect_identical(vapply(again$cols, function(cl) as.integer(cl$width),
                          integer(1L)),
                   c(11L, 20L))
})


# ── a header is never cut mid-word ───────────────────────────────────────────

test_that("scaling never takes a column below what its header needs", {
  # "Diagnosis" is nine characters and cannot break; a naive proportional
  # scaling gave this column eight and split the header as "Diagnosi" / "s".
  spec <- listing_spec(
    list(listing_col("USUBJID"),
         listing_col(c("DISPTPD", "BRCA", "HIST")),
         listing_col("STAGE")),
    labels = c(USUBJID = "Unique Subject ID",
               DISPTPD = "Primary Diagnosis",
               BRCA    = "Any (BRCA) Mutations",
               HIST    = "Histology",
               STAGE   = "Stage at Initial Diagnosis"))
  d <- .unlabelled()
  d$STAGE <- c("IIIB", "IV")

  fitted <- fit_listing_widths(d, spec, total_width = 60)
  expect_gte(fitted$cols[[3L]]$width, 9L)

  lab <- attr(build_listing(d, fitted), "rtf_listing")$cols[[3L]]$label
  expect_true(all(strsplit(lab, "\n", fixed = TRUE)[[1L]] %in%
                    c("Stage at", "Initial", "Diagnosis", "Stage",
                      "at Initial")))
  expect_false(any(grepl("Diagnosi$", strsplit(lab, "\n", fixed = TRUE)[[1L]])))
})

test_that("the budget is still respected exactly when a floor is raised", {
  spec <- listing_spec(
    list(listing_col("USUBJID"), listing_col("HIST"), listing_col("BRCA")),
    labels = .spec_labels)
  fitted <- fit_listing_widths(.unlabelled(), spec, total_width = 45)
  w <- vapply(fitted$cols, function(cl) as.integer(cl$width), integer(1L))
  expect_identical(sum(w) + 2, 45)
})

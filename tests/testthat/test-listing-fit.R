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


# ── fit_listing_widths(labels = ): the words for the source variables ────────
#
# The lookup is an INPUT TO THE ESTIMATION, not part of the listing's
# definition (#376): it is what a header is measured against, and once the fit
# runs the resolved header is written onto each column.

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

.label_of <- function(spec, j = 1L) spec$cols[[j]]$label

test_that("labels supply the header when the data carries none", {
  fitted <- fit_listing_widths(
    .unlabelled(), listing_spec(list(listing_col("USUBJID", width = 20))),
    total_width = 20, labels = .spec_labels)
  expect_identical(.label_of(fitted), "Unique Subject ID")
})

test_that("a joined column joins its labels, and the breaks stay automatic", {
  fitted <- fit_listing_widths(
    .unlabelled(),
    listing_spec(list(listing_col(c("DISPTPD", "BRCA", "HIST"), width = 22))),
    total_width = 22, labels = .spec_labels)
  expect_identical(strsplit(.label_of(fitted), "\n", fixed = TRUE)[[1L]],
                   c("Primary Diagnosis/", "Any (BRCA) Mutations/", "Histology"))
})

test_that("precedence: listing_col(label) > labels > attribute > name", {
  d <- .unlabelled()
  attr(d$USUBJID, "label") <- "FROM THE DATA"
  fit <- function(spec) {
    fit_listing_widths(d, spec, total_width = 30, labels = .spec_labels)
  }

  expect_identical(
    .label_of(fit(listing_spec(list(listing_col("USUBJID", width = 30,
                                                label = "FROM THE COLUMN"))))),
    "FROM THE COLUMN")

  expect_identical(
    .label_of(fit(listing_spec(list(listing_col("USUBJID", width = 30))))),
    "Unique Subject ID")

  # with no lookup, the data's attribute is next
  expect_identical(
    .label_of(fit_listing_widths(
      d, listing_spec(list(listing_col("USUBJID", width = 30))),
      total_width = 30)),
    "FROM THE DATA")

  # and with neither, the variable's name
  expect_identical(
    .label_of(fit_listing_widths(
      d, listing_spec(list(listing_col("DISPTPD", width = 30))),
      total_width = 30)),
    "DISPTPD")
})

test_that("a variable the lookup does not name still falls back", {
  fitted <- fit_listing_widths(
    .unlabelled(),
    listing_spec(list(listing_col("USUBJID", width = 20),
                      listing_col("DISPTPD", width = 20))),
    total_width = 41, labels = c(USUBJID = "Unique Subject ID"))
  expect_identical(.label_of(fitted, 1L), "Unique Subject ID")
  expect_identical(.label_of(fitted, 2L), "DISPTPD")
})

test_that("the width fit measures the supplied labels", {
  # "Any (BRCA) Mutations" cannot break below 9 ("Mutations"), so the column
  # cannot be fitted narrower than that.
  spec <- listing_spec(list(listing_col("BRCA"), listing_col("USUBJID")),
                       spacer = FALSE)
  fitted <- fit_listing_widths(.unlabelled(), spec, total_width = 30,
                               labels = .spec_labels)
  expect_gte(fitted$cols[[1L]]$width, 9L)
})

test_that("labels are validated", {
  spec <- listing_spec(list(listing_col("USUBJID")))
  expect_error(fit_listing_widths(.unlabelled(), spec, total_width = 20,
                                  labels = "no names"), "named character")
  expect_error(fit_listing_widths(.unlabelled(), spec, total_width = 20,
                                  labels = c("Unnamed")), "named character")
  expect_error(fit_listing_widths(.unlabelled(), spec, total_width = 20,
                                  labels = 1:2), "named character")
})

test_that("listing_spec() no longer takes a labels argument", {
  expect_error(listing_spec(list(listing_col("USUBJID")),
                            labels = .spec_labels), "unused argument")
})


# ── a header is never cut mid-word ───────────────────────────────────────────

test_that("scaling never takes a column below what its header needs", {
  # "Diagnosis" is nine characters and cannot break; a naive proportional
  # scaling gave this column eight and split the header as "Diagnosi" / "s".
  d <- .unlabelled()
  d$STAGE <- c("IIIB", "IV")
  spec <- listing_spec(list(listing_col("USUBJID"),
                            listing_col(c("DISPTPD", "BRCA", "HIST")),
                            listing_col("STAGE")))
  fitted <- fit_listing_widths(
    d, spec, total_width = 60,
    labels = c(.spec_labels, STAGE = "Stage at Initial Diagnosis"))

  expect_gte(fitted$cols[[3L]]$width, 9L)
  lines <- strsplit(.label_of(fitted, 3L), "\n", fixed = TRUE)[[1L]]
  expect_false(any(grepl("Diagnosi$", lines)))
})

test_that("the budget is still respected exactly when a floor is raised", {
  spec <- listing_spec(list(listing_col("USUBJID"), listing_col("HIST"),
                            listing_col("BRCA")))
  fitted <- fit_listing_widths(.unlabelled(), spec, total_width = 45,
                               labels = .spec_labels)
  w <- vapply(fitted$cols, function(cl) as.integer(cl$width), integer(1L))
  expect_identical(sum(w) + 2, 45)
})


# ── the fitted spec is a template: everything written down (#375) ────────────

test_that("fitting writes rel_width and the label down, not just the width", {
  fitted <- fit_listing_widths(
    .unlabelled(),
    listing_spec(list(listing_col("USUBJID"), listing_col("HIST"))),
    total_width = 40, labels = .spec_labels)

  for (cl in fitted$cols) {
    expect_false(is.null(cl$width))
    expect_false(is.null(cl$rel_width))
    expect_false(is.null(cl$label))
    expect_identical(as.numeric(cl$rel_width), as.numeric(cl$width))
  }
  # the header is resolved AND wrapped to the width just chosen
  lines <- strsplit(.label_of(fitted), "\n", fixed = TRUE)[[1L]]
  expect_true(all(.listing_disp_width(lines) <= fitted$cols[[1L]]$width))
})

test_that("a value the author set is still never touched", {
  spec <- listing_spec(list(
    listing_col("USUBJID", width = 12, rel_width = 40, label = "MINE"),
    listing_col("HIST")))
  fitted <- fit_listing_widths(.unlabelled(), spec, total_width = 60,
                               labels = .spec_labels)
  expect_identical(fitted$cols[[1L]]$width, 12L)
  expect_identical(fitted$cols[[1L]]$rel_width, 40)
  expect_identical(fitted$cols[[1L]]$label, "MINE")
})

test_that("the emitted template carries width, rel_width and label", {
  fitted <- fit_listing_widths(
    .unlabelled(),
    listing_spec(list(listing_col(c("DISPTPD", "BRCA", "HIST")))),
    total_width = 44, labels = .spec_labels)
  code <- paste(listing_code(fitted), collapse = "\n")

  expect_true(grepl("width = ",     code, fixed = TRUE))
  expect_true(grepl("rel_width = ", code, fixed = TRUE))
  expect_true(grepl("label = ",     code, fixed = TRUE))
})

test_that("the template round-trips to the same widths and headers", {
  fitted <- fit_listing_widths(
    .unlabelled(),
    listing_spec(list(listing_col("USUBJID"),
                      listing_col(c("DISPTPD", "BRCA", "HIST")))),
    total_width = 50, labels = .spec_labels)
  again <- eval(parse(text = paste(listing_code(fitted), collapse = "\n")))

  expect_identical(vapply(again$cols, function(cl) as.integer(cl$width),
                          integer(1L)),
                   vapply(fitted$cols, function(cl) as.integer(cl$width),
                          integer(1L)))
  expect_identical(vapply(again$cols, function(cl) cl$label, character(1L)),
                   vapply(fitted$cols, function(cl) cl$label, character(1L)))

  # and it renders identically to the spec it came from
  expect_identical(as_rtftables(.unlabelled(), listing = again),
                   as_rtftables(.unlabelled(), listing = fitted))
})


# ── header_lines: a long label buys width (#378) ─────────────────────────────

.long_header <- function() {
  d <- data.frame(SHORT = c("12.5", "36.2"), OTHER = c("aaaa", "bbbb"),
                  stringsAsFactors = FALSE)
  d
}

.long_labels <- c(
  SHORT = "Time since Initial Diagnosis to Date of First Dose of Study Drug (months)",
  OTHER = "Other")

.fit_hl <- function(hl) {
  spec <- listing_spec(list(listing_col("SHORT"), listing_col("OTHER")),
                       spacer = FALSE)
  fit_listing_widths(.long_header(), spec, total_width = 60,
                     labels = .long_labels, header_lines = hl)
}

.hdr_lines <- function(spec, j = 1L) {
  length(strsplit(spec$cols[[j]]$label, "\n", fixed = TRUE)[[1L]])
}

test_that("a long label over short data asks for more than its widest token", {
  # Data of four characters; the label is 73.  With the height ignored the
  # column gets almost nothing and the header becomes a tall block.
  tall <- .fit_hl(Inf)
  wide <- .fit_hl(4)
  expect_gt(wide$cols[[1L]]$width, tall$cols[[1L]]$width)
  expect_lt(.hdr_lines(wide), .hdr_lines(tall))
})

test_that("header_lines is a target the fit gets close to", {
  fitted <- .fit_hl(4)
  # not a guarantee -- the budget is shared -- but the block is bounded
  expect_lte(.hdr_lines(fitted), 6L)
  expect_gte(fitted$cols[[1L]]$width, 15L)
})

test_that("a lower header_lines buys more width, monotonically", {
  w <- vapply(c(Inf, 6, 4, 3), function(hl) .fit_hl(hl)$cols[[1L]]$width,
              integer(1L))
  expect_true(all(diff(w) >= 0L))
})

test_that("header_lines = Inf asks nothing on the header's behalf", {
  dem <- function(hl) {
    attr(.fit_hl(hl), "rtf_listing_fit", exact = TRUE)$demand[["SHORT"]]
  }
  # With Inf the demand is the data and the widest unbreakable token
  # ("Diagnosis", nine).  With a height target it is the label's width divided
  # by that height, which is more.
  expect_lte(unname(dem(Inf)), 10)
  expect_gt(unname(dem(4)), unname(dem(Inf)))
})

test_that("header_lines never overrides a width the author set", {
  spec <- listing_spec(list(listing_col("SHORT", width = 8),
                            listing_col("OTHER")), spacer = FALSE)
  fitted <- fit_listing_widths(.long_header(), spec, total_width = 60,
                               labels = .long_labels, header_lines = 3)
  expect_identical(fitted$cols[[1L]]$width, 8L)
})

test_that("header_lines is validated", {
  spec <- listing_spec(list(listing_col("SHORT")))
  expect_error(fit_listing_widths(.long_header(), spec, total_width = 20,
                                  header_lines = 0), "at least|>= 1")
  expect_error(fit_listing_widths(.long_header(), spec, total_width = 20,
                                  header_lines = "four"), "single number")
})

test_that("a fractional gutter is allowed, and frees width for the columns", {
  cols <- list(listing_col("SHORT"), listing_col("OTHER"))
  wide_gutter <- fit_listing_widths(
    .long_header(), listing_spec(cols, spacer_rel_width = 1),
    total_width = 40, labels = .long_labels)
  hair <- fit_listing_widths(
    .long_header(), listing_spec(cols, spacer_rel_width = 0.25),
    total_width = 40, labels = .long_labels)
  expect_gt(sum(vapply(hair$cols, function(cl) cl$width, integer(1L))),
            sum(vapply(wide_gutter$cols, function(cl) cl$width, integer(1L))))
})

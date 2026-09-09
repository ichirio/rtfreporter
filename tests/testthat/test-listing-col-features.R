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

test_that("a label the author laid out is used exactly as written", {
  # A vector, or a string carrying its own breaks, says "I laid this out" --
  # even where a line is wider than the column (#380).
  hand <- c("A header the", "author wrote")
  body <- build_listing(.labelled_adsl(),
                        listing_spec(list(listing_col("HIST", width = 6,
                                                      label = hand))))
  expect_identical(unname(.labels_of(body)), "A header the
author wrote")
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


# ── label: a vector is the lines, a string is words to lay out (#380) ────────

.lab_of <- function(spec, data = .labelled_adsl(), j = 1L) {
  attr(build_listing(data, spec), "rtf_listing", exact = TRUE)$cols[[j]]$label
}

test_that("a character vector is one line per element", {
  spec <- listing_spec(list(listing_col("HIST", width = 15,
                                        label = c("Unique", "Subject ID"))))
  expect_identical(.lab_of(spec), "Unique\nSubject ID")
})

test_that("a vector is a layout, so it is not re-wrapped", {
  # the second line is wider than the column and stays whole
  spec <- listing_spec(list(listing_col("HIST", width = 4,
                                        label = c("A", "BBBB CCCC"))))
  expect_identical(.lab_of(spec), "A\nBBBB CCCC")
})

test_that("a single string is laid out at the width", {
  spec <- listing_spec(list(listing_col("HIST", width = 16,
                                        label = "Histology of the tumour")))
  expect_identical(.lab_of(spec), "Histology of\nthe tumour")
})

test_that("a single string with no width is left as one line", {
  spec <- listing_spec(list(listing_col("HIST",
                                        label = "Histology of the tumour")))
  expect_identical(.lab_of(spec), "Histology of the tumour")
})

test_that("a string carrying its own breaks is a layout too", {
  spec <- listing_spec(list(listing_col("HIST", width = 8,
                                        label = "Histology\nof the tumour")))
  expect_identical(.lab_of(spec), "Histology\nof the tumour")
})

test_that("the layout rule is the cells' rule: separator, words, hard split", {
  spec <- listing_spec(list(listing_col("HIST", width = 22,
                                        label = "COMPLETED/BRCA1/ADENOCARCINOMA")))
  expect_identical(.lab_of(spec), "COMPLETED/\nBRCA1/\nADENOCARCINOMA")

  spec2 <- listing_spec(list(listing_col("HIST", width = 8,
                                         label = "Immunohistochemistry")))
  lines <- strsplit(.lab_of(spec2), "\n", fixed = TRUE)[[1L]]
  expect_true(all(.listing_disp_width(lines) <= 8))
})

test_that("label is validated", {
  expect_error(listing_col("A", label = 1:2), "character vector of header")
  expect_error(listing_col("A", label = character(0)),
               "character vector of header")
  expect_error(listing_col("A", label = c("a", NA)),
               "character vector of header")
})


# ── listing_wrap() ───────────────────────────────────────────────────────────

test_that("listing_wrap() applies the rule and returns the lines", {
  expect_identical(listing_wrap("COMPLETED/BRCA1/ADENOCARCINOMA", 22),
                   c("COMPLETED/", "BRCA1/", "ADENOCARCINOMA"))
  expect_identical(listing_wrap("40/F", 20, layout = "flow"), "40/F")
  expect_true(all(.listing_disp_width(
    listing_wrap("Immunohistochemistry", 8)) <= 8))
})

test_that("listing_wrap() vectorises, and NULL width means no limit", {
  out <- listing_wrap(c("A/B", "C/D"), 20)
  expect_type(out, "list")
  expect_length(out, 2L)
  expect_identical(listing_wrap("Histology of the tumour", NULL),
                   "Histology of the tumour")
})

test_that("listing_wrap() composes with a vector label", {
  spec <- listing_spec(list(
    listing_col("HIST", width = 16,
                label = listing_wrap("Histology of the tumour", 16))))
  expect_identical(.lab_of(spec), "Histology of\nthe tumour")
})

test_that("listing_wrap() validates its arguments", {
  expect_error(listing_wrap("a", 10, sep = c("/", "-")), "single string")
  expect_error(listing_wrap("a", -1), "positive number")
})


# ── a derived header breaks where the data breaks (#384) ─────────────────────

.sexage <- function() {
  d <- data.frame(SEX = c("Female", "Male"), AGE = c("18", "72"),
                  stringsAsFactors = FALSE)
  attr(d$SEX, "label") <- "Sex"
  attr(d$AGE, "label") <- "Age"
  d
}

.pair <- function(width, layout) {
  spec <- listing_spec(list(listing_col(c("SEX", "AGE"), width = width,
                                        layout = layout)),
                       spacer = FALSE, blank_row = FALSE, record = FALSE)
  b <- build_listing(.sexage(), spec)
  list(cell = b[[1L]][1:2],
       head = strsplit(attr(b, "rtf_listing", exact = TRUE)$cols[[1L]]$label,
                       "\n", fixed = TRUE)[[1L]])
}

test_that("stacked: the header breaks at the separator, as the cell does", {
  r <- .pair(9, "stack")
  expect_identical(r$cell, c("Female/", "18"))
  expect_identical(r$head, c("Sex/", "Age"))
})

test_that("flowed: a cell on one line gets a header on one line", {
  r <- .pair(9, "flow")
  expect_identical(r$cell[[1L]], "Female/18")
  expect_identical(r$head, "Sex/Age")
})

test_that("a flowed header used to stay stacked -- two labels can share a line", {
  # Before #384 each label was wrapped separately, so `layout = "flow"` flowed
  # the cells and never the header.
  expect_length(.pair(20, "flow")$head, 1L)
  expect_length(.pair(20, "stack")$head, 2L)
})

test_that("stacked headers are unchanged by the join", {
  # Joining and wrapping once must give what wrapping each label gave, because
  # stack breaks at every separator anyway.
  d <- .labelled_adsl()
  spec <- listing_spec(list(listing_col(c("AGE", "SEX"), width = 4)))
  expect_identical(
    attr(build_listing(d, spec), "rtf_listing", exact = TRUE)$cols[[1L]]$label,
    "Age/\nSex")
})


# ── listing_spec(wrap = ): a listing's own splitter ──────────────────────────

test_that("a custom wrap lays out the cells and the headers alike", {
  by_pipe <- function(text, width, sep, layout) {
    unlist(strsplit(as.character(text)[[1L]], "|", fixed = TRUE))
  }
  spec <- listing_spec(list(listing_col("A", width = 20, label = "X|Y|Z")),
                       sep = "|", wrap = by_pipe, spacer = FALSE,
                       blank_row = FALSE, record = FALSE)
  b <- build_listing(data.frame(A = "a|b|c", stringsAsFactors = FALSE), spec)

  expect_identical(b$A, c("a", "b", "c"))
  expect_identical(attr(b, "rtf_listing", exact = TRUE)$cols[[1L]]$label,
                   "X\nY\nZ")
})

test_that("a custom wrap is used for a DERIVED header too", {
  by_pipe <- function(text, width, sep, layout) {
    unlist(strsplit(as.character(text)[[1L]], "|", fixed = TRUE))
  }
  d <- data.frame(A = "a", B = "b", stringsAsFactors = FALSE)
  attr(d$A, "label") <- "Alpha"
  attr(d$B, "label") <- "Beta"
  spec <- listing_spec(list(listing_col(c("A", "B"), width = 20)),
                       sep = "|", wrap = by_pipe, spacer = FALSE,
                       blank_row = FALSE, record = FALSE)
  expect_identical(
    attr(build_listing(d, spec), "rtf_listing", exact = TRUE)$cols[[1L]]$label,
    "Alpha\nBeta")
})

test_that("wrap is validated, and its contract enforced", {
  expect_error(listing_spec("A", wrap = "nope"), "must be a function")
  expect_error(listing_spec("A", wrap = function(x) x), "four arguments")

  bad <- function(text, width, sep, layout) 42
  spec <- listing_spec(list(listing_col("A", width = 5)), wrap = bad,
                       spacer = FALSE, blank_row = FALSE, record = FALSE)
  expect_error(build_listing(data.frame(A = "x", stringsAsFactors = FALSE),
                             spec),
               "non-empty character")
})

test_that("listing_code() says a custom wrap cannot be written out", {
  by_pipe <- function(text, width, sep, layout) {
    unlist(strsplit(as.character(text)[[1L]], "|", fixed = TRUE))
  }
  plain  <- listing_code(listing_spec(list(listing_col("A", width = 5))))
  custom <- listing_code(listing_spec(list(listing_col("A", width = 5)),
                                      wrap = by_pipe))
  expect_false(any(grepl("custom `wrap`", plain, fixed = TRUE)))
  expect_true(any(grepl("custom `wrap`", custom, fixed = TRUE)))
})

test_that("the cell path enforces the contract too, not just the header", {
  # A vector label is a layout, so it is NOT wrapped: the only call left is
  # the one on the cell, and it must be validated the same way (#386).
  bad  <- function(text, width, sep, layout) list("x")
  spec <- listing_spec(list(listing_col("A", width = 5,
                                        label = c("A", "B"))),
                       wrap = bad, spacer = FALSE, blank_row = FALSE,
                       record = FALSE)
  expect_error(build_listing(data.frame(A = "x", stringsAsFactors = FALSE),
                             spec),
               "non-empty character")
})

test_that("wrap = listing_wrap is a no-op, so it can be delegated to", {
  d <- data.frame(SEX = "Female", AGE = "18", stringsAsFactors = FALSE)
  attr(d$SEX, "label") <- "Sex"
  attr(d$AGE, "label") <- "Age"
  build <- function(...) {
    spec <- listing_spec(list(listing_col(c("SEX", "AGE"), width = 9)),
                         spacer = FALSE, blank_row = FALSE, record = FALSE,
                         ...)
    b <- build_listing(d, spec)
    list(cells = b[[1L]],
         label = attr(b, "rtf_listing", exact = TRUE)$cols[[1L]]$label)
  }
  expect_identical(build(wrap = listing_wrap), build())

  # ... and the documented way to write one: delegate, then adjust.
  shout <- function(text, width, sep, layout) {
    toupper(listing_wrap(text, width, sep, layout))
  }
  got <- build(wrap = shout)
  expect_identical(got$cells, toupper(build()$cells))
  expect_identical(got$label, toupper(build()$label))
})

# ── listing_wrap_code(): the rule as source ──────────────────────────────────

test_that("the emitted rule is self-contained and IS the shipped rule", {
  src <- listing_wrap_code("my_wrap")

  # Self-contained: nothing in it reaches back into the package.
  expect_false(any(grepl(":::", src, fixed = TRUE)))
  expect_false(any(grepl(".listing_", src, fixed = TRUE)))
  expect_false(any(grepl("rtfreporter", src[-1L], fixed = TRUE)))

  env <- new.env(parent = globalenv())
  eval(parse(text = src), envir = env)
  expect_setequal(ls(env), c("my_wrap", "my_wrap_disp_width", "my_wrap_flow",
                             "my_wrap_split_after", "my_wrap_take",
                             "my_wrap_words"))

  # And it cannot have drifted, because it is read off the live functions.
  # Widths, separators, layouts, CJK, an embedded newline, an empty cell.
  txt <- c("COMPLETED/BRCA1/ADENOCARCINOMA",
           "SQUAMOUS CELL CARCINOMA OF THE LUNG",
           "\u65e5\u672c\u8a9e\u306e\u9577\u3044\u8a18\u8ff0/A", "a\nb", "",
           "SUPERCALIFRAGILISTIC")
  for (w in list(NULL, 6, 12, 40)) {
    for (lay in c("stack", "flow")) {
      for (t in txt) {
        expect_identical(env$my_wrap(t, w, "/", lay),
                         listing_wrap(t, w, "/", lay),
                         info = paste(t, w, lay))
      }
    }
  }
})

test_that("what it emits can be handed straight back as `wrap`", {
  src <- listing_wrap_code("edited")
  env <- new.env(parent = globalenv())
  eval(parse(text = src), envir = env)

  d <- data.frame(HIST = "SQUAMOUS CELL CARCINOMA", stringsAsFactors = FALSE)
  plain <- listing_spec(list(listing_col("HIST", width = 12, label = "H")),
                        spacer = FALSE, blank_row = FALSE, record = FALSE)
  edited <- listing_spec(list(listing_col("HIST", width = 12, label = "H")),
                         wrap = env$edited, spacer = FALSE, blank_row = FALSE,
                         record = FALSE)
  expect_identical(build_listing(d, edited)$HIST, build_listing(d, plain)$HIST)
})

test_that("the source keeps its comments, which is the point of copying it", {
  src <- listing_wrap_code()
  expect_true(any(grepl("^[[:space:]]*#", src[-seq_len(9L)])))
  expect_true(any(grepl("#364", src, fixed = TRUE)))   # the rationale, kept
})

test_that("the header names the entry function and states the contract", {
  src <- listing_wrap_code("house_rule")
  expect_true(any(grepl("wrap = house_rule", src, fixed = TRUE)))
  expect_true(any(grepl("text, width, sep, layout", src, fixed = TRUE)))
  expect_true(any(grepl(as.character(utils::packageVersion("rtfreporter")),
                        src, fixed = TRUE)))
})

test_that("the helpers are named after `name`, so two rules can coexist", {
  a <- listing_wrap_code("alpha")
  b <- listing_wrap_code("beta")
  env <- new.env(parent = globalenv())
  eval(parse(text = a), envir = env)
  eval(parse(text = b), envir = env)
  expect_true(all(c("alpha", "alpha_words", "beta", "beta_words") %in% ls(env)))
  expect_identical(env$alpha("A/B", 3, "/", "stack"),
                   env$beta("A/B", 3, "/", "stack"))
})

test_that("listing_wrap_code() validates `name`", {
  expect_error(listing_wrap_code(c("a", "b")), "single non-empty string")
  expect_error(listing_wrap_code(""), "single non-empty string")
  expect_error(listing_wrap_code("2fast"), "syntactic R name")
})

test_that("the template has not drifted from the rule it was copied from", {
  # Regenerate with: Rscript data-raw/gen_listing_wrap_template.R
  #
  # Not under covr: it rewrites every function body to count what runs, so
  # the live deparse is instrumented code and would never match a source
  # file.  The comparison still runs in R CMD check on every platform.
  skip_on_covr()
  env <- new.env(parent = globalenv())
  eval(parse(text = .listing_wrap_template()), envir = env)
  for (nm in .listing_wrap_parts()) {
    expect_identical(deparse(get(nm, envir = env)),
                     deparse(get(nm, envir = asNamespace("rtfreporter"))),
                     info = nm)
  }
})

# A border on a block whose rows are fixed -- a page header or footer band, a
# footnote block, a title block -- is the whole block's, read as a table
# zone's is: top above the first row, bottom below the last, inside_h between
# rows, left / right on every row's outer edges, inside_v between a row's
# cells.  A row's own `border` is merged side by side over it.

.bz_df <- function() data.frame(A = c("1", "2"), B = c("x", "y"),
                                stringsAsFactors = FALSE)

.bz_render <- function(doc) {
  d <- tempfile("bz-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- file.path(d, "out.rtf")
  generate_rtfreport(doc, f, overwrite = TRUE)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

.bz_doc <- function(header = NULL, footer = NULL) {
  rtf_document() |>
    rtf_section(page = 1, header = header, footer = footer) |>
    rtf_tables(list(.bz_df()))
}

# The rows of the {\header ...} (or {\footer ...}) group, each a list of its
# cell definitions (the border words before each \cellx).
.bz_band <- function(txt, which = "header") {
  g <- regmatches(txt, regexpr(sprintf("\\{\\\\%s [^\n]*", which), txt))
  rows <- strsplit(g, "\\row", fixed = TRUE)[[1L]]
  rows <- rows[grepl("\\trowd", rows, fixed = TRUE)]
  lapply(rows, function(r) {
    defs <- strsplit(sub("^.*?\\\\trowd(\\\\trrh[0-9]+)?", "", r, perl = TRUE),
                     "\\\\cellx[0-9]+")[[1L]]
    defs[seq_len(lengths(regmatches(r, gregexpr("\\\\cellx", r))))]
  })
}
.bz_has <- function(def, side) grepl(paste0("\\clbrdr", side), def, fixed = TRUE)

.bz_rows2 <- list(c(l = "Example Pharma", r = "Page {PAGE}"),
                  c(l = "Protocol: EXP-001", r = "ITT"))
.bz_rows3 <- c(.bz_rows2, list(c(c = "Draft")))

test_that("a 1-row band: bottom, left / right and inside_v are that row's", {
  b <- .bz_band(.bz_render(.bz_doc(rtf_header(
    list(c(l = "Example Pharma", r = "Page {PAGE}")),
    border = rtf_border(bottom = TRUE, left = TRUE, right = TRUE,
                        inside_v = "dot")))))
  expect_length(b, 1L)
  r <- b[[1L]]
  expect_true(all(.bz_has(r, "b")))
  expect_true(.bz_has(r[1L], "l\\brdrs"))
  expect_true(.bz_has(r[1L], "r\\brdrdot"))   # the inside rule, between cells
  expect_true(.bz_has(r[2L], "l\\brdrdot"))
  expect_true(.bz_has(r[2L], "r\\brdrs"))
})

test_that("a 2-row band: bottom is below the last row, not the first", {
  b <- .bz_band(.bz_render(.bz_doc(rtf_header(
    .bz_rows2, border = rtf_border(bottom = TRUE)))))
  expect_length(b, 2L)
  expect_false(any(.bz_has(b[[1L]], "b")))
  expect_true(all(.bz_has(b[[2L]], "b")))
})

test_that("a 3-row band: top above the first, inside_h between, bottom below", {
  b <- .bz_band(.bz_render(.bz_doc(rtf_header(
    .bz_rows3, border = rtf_border(top = TRUE, bottom = "double",
                                   inside_h = "dot")))))
  expect_length(b, 3L)
  expect_true(all(.bz_has(b[[1L]], "t")))
  expect_false(any(.bz_has(b[[2L]], "t")))
  expect_false(any(.bz_has(b[[3L]], "t")))
  expect_true(all(.bz_has(b[[1L]], "b\\brdrdot")))
  expect_true(all(.bz_has(b[[2L]], "b\\brdrdot")))
  expect_true(all(.bz_has(b[[3L]], "b\\brdrdb")))
})

test_that("left / right are on every row; inside_v between each row's cells", {
  b <- .bz_band(.bz_render(.bz_doc(rtf_header(
    .bz_rows3, border = rtf_border(left = TRUE, right = TRUE,
                                   inside_v = "dot")))))
  for (i in 1:2) {
    expect_true(.bz_has(b[[i]][1L], "l\\brdrs"))
    expect_true(.bz_has(b[[i]][1L], "r\\brdrdot"))
    expect_true(.bz_has(b[[i]][2L], "l\\brdrdot"))
    expect_true(.bz_has(b[[i]][2L], "r\\brdrs"))
  }
  # the one-cell third row: its outer edges, no inside rule
  expect_length(b[[3L]], 1L)
  expect_true(.bz_has(b[[3L]], "l\\brdrs"))
  expect_true(.bz_has(b[[3L]], "r\\brdrs"))
  expect_false(grepl("brdrdot", b[[3L]], fixed = TRUE))
})

test_that("a row's own border wins over the band's, side by side", {
  rows <- list(
    list(columns = c(l = "Example Pharma", r = "Page {PAGE}"),
         border = rtf_border(bottom = TRUE)),
    list(columns = c(l = "Protocol: EXP-001", r = "ITT"),
         border = rtf_border(bottom = "none", left = "double")))
  b <- .bz_band(.bz_render(.bz_doc(rtf_header(
    rows, border = rtf_border(bottom = "dot", left = TRUE)))))
  # row 1: its own bottom (single); the band's left stays
  expect_true(all(.bz_has(b[[1L]], "b\\brdrs")))
  expect_true(.bz_has(b[[1L]][1L], "l\\brdrs"))
  # row 2: "none" erases the band's bottom; its own left wins
  expect_false(any(.bz_has(b[[2L]], "b")))
  expect_true(.bz_has(b[[2L]][1L], "l\\brdrdb"))
})

test_that("a boundary both rows draw is drawn once, as the upper row's bottom", {
  rows <- list(c(l = "Example Pharma"),
               list(columns = c(l = "Protocol: EXP-001"),
                    border = rtf_border(top = "double")))
  # the zone's inside_h and the lower row's own top: the row's wins, above
  b <- .bz_band(.bz_render(.bz_doc(rtf_header(
    rows, border = rtf_border(inside_h = TRUE)))))
  expect_true(.bz_has(b[[1L]], "b\\brdrdb"))
  expect_false(.bz_has(b[[2L]], "t"))
  # a lower row's top with nothing above it stays the lower row's top
  b <- .bz_band(.bz_render(.bz_doc(rtf_header(rows))))
  expect_false(.bz_has(b[[1L]], "b"))
  expect_true(.bz_has(b[[2L]], "t\\brdrdb"))
})

test_that("a band row's border is checked, naming the row", {
  expect_error(
    rtf_header(list(c(l = "A"), list(columns = c(l = "B"), border = "bottom"))),
    "`rtf_header()` row 2's `border` must be NULL or an rtf_border() object.",
    fixed = TRUE)
  expect_error(
    rtf_footer(list(list(columns = c(l = "B"), border = TRUE))),
    "`rtf_footer()` row 1's `border` must be NULL or an rtf_border() object.",
    fixed = TRUE)
})

test_that("a row's border colour reaches the colour table", {
  txt <- .bz_render(.bz_doc(rtf_header(list(
    list(columns = c(l = "Example Pharma"),
         border = rtf_border(bottom = rtf_border_line(color = "#C00000")))))))
  expect_match(txt, "\\red192\\green0\\blue0", fixed = TRUE)
  expect_match(txt, "clbrdrb\\brdrs\\brdrw15\\brdrcf", fixed = TRUE)
})

test_that("drop_empty_rows: the zone is the rows written", {
  band <- rtf_header(list(c(l = "Example Pharma", r = "Page {PAGE}"),
                          c(c = "{POP}")),
                     border = rtf_border(bottom = TRUE), drop_empty_rows = TRUE)
  doc <- function(pop) rtf_document(tokens = list(POP = pop)) |>
    rtf_section(page = 1, header = band) |>
    rtf_tables(list(.bz_df()))
  kept <- .bz_band(.bz_render(doc("Safety Analysis Set")))
  expect_length(kept, 2L)
  expect_false(any(.bz_has(kept[[1L]], "b")))
  expect_true(.bz_has(kept[[2L]], "b"))
  # the empty last row is left out: the bottom rule is the first row's now
  gone <- .bz_band(.bz_render(doc("")))
  expect_length(gone, 1L)
  expect_true(all(.bz_has(gone[[1L]], "b")))
})

test_that("the footer default (a top rule) is the same as before: first row only", {
  b <- .bz_band(.bz_render(.bz_doc(footer = rtf_footer(.bz_rows3))), "footer")
  expect_length(b, 3L)
  expect_identical(unique(b[[1L]]), "\\clbrdrt\\brdrs\\brdrw15")
  expect_identical(unique(c(b[[2L]], b[[3L]])), "")
  # the words of the whole band, pinned
  txt <- .bz_render(.bz_doc(footer = rtf_footer(.bz_rows2)))
  g <- regmatches(txt, regexpr("\\{\\\\footer [^\n]*", txt))
  expect_identical(lengths(regmatches(g, gregexpr("clbrdr", g, fixed = TRUE))), 2L)
  expect_match(g, "\\trowd\\trrh230\\clbrdrt\\brdrs\\brdrw15\\cellx6840\\clbrdrt\\brdrs\\brdrw15\\cellx13680",
               fixed = TRUE)
  expect_match(g, "\\row\\trowd\\trrh230\\cellx6840\\cellx13680", fixed = TRUE)
})

test_that("rtf_style(header = list(border = )) still reaches a band left unset", {
  doc <- .bz_doc(rtf_header(.bz_rows2)) |>
    rtf_config(style = rtf_style(header = list(border = rtf_border(bottom = TRUE))))
  b <- .bz_band(.bz_render(doc))
  expect_false(any(.bz_has(b[[1L]], "b")))
  expect_true(all(.bz_has(b[[2L]], "b")))
  # a band that passes its own border keeps it
  doc <- .bz_doc(rtf_header(.bz_rows2, border = NULL)) |>
    rtf_config(style = rtf_style(header = list(border = rtf_border(bottom = TRUE))))
  b <- .bz_band(.bz_render(doc))
  expect_identical(unique(unlist(b)), "")
})

# ---- footnote blocks (the table format) --------------------------------------

.bz_fn_doc <- function(...) {
  rtf_document() |>
    rtf_section(page = 1, header = NULL, footer = NULL) |>
    rtf_tables(list(.bz_df())) |>
    rtf_footnotes(...)
}
# the footnote rows: those after the body table
.bz_fn_rows <- function(txt, pat = "Note") {
  l <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
  grep(pat, l, value = TRUE)
}

test_that("a 2-row footnote block: bottom is below the last row", {
  r <- .bz_fn_rows(.bz_render(.bz_fn_doc(list(c("Note 1", "Note 2")),
                                         border = rtf_border(bottom = TRUE))))
  expect_length(r, 2L)
  expect_false(grepl("clbrdrb", r[1L], fixed = TRUE))
  expect_true(grepl("clbrdrb", r[2L], fixed = TRUE))
})

test_that("the footnote separator: a top rule on the first row, as before", {
  r <- .bz_fn_rows(.bz_render(.bz_fn_doc(list(c("Note 1", "Note 2")),
                                         border = rtf_border(top = TRUE))))
  expect_true(grepl("\\trowd\\trrh230\\clbrdrt\\brdrs\\brdrw15\\cellx", r[1L],
                    fixed = TRUE))
  expect_false(grepl("clbrdr", r[2L], fixed = TRUE))
  # no border, no rule
  r <- .bz_fn_rows(.bz_render(.bz_fn_doc(list(c("Note 1", "Note 2")))))
  expect_false(any(grepl("clbrdr", r, fixed = TRUE)))
})

test_that("a footnote row's own border wins over the block's", {
  blk <- list(list(columns = c(l = "Note 1"), border = rtf_border(bottom = "double")),
              "Note 2")
  r <- .bz_fn_rows(.bz_render(.bz_fn_doc(list(blk),
                                         border = rtf_border(inside_h = TRUE,
                                                             bottom = TRUE))))
  expect_true(grepl("clbrdrb\\brdrdb", r[1L], fixed = TRUE))
  expect_true(grepl("clbrdrb\\brdrs", r[2L], fixed = TRUE))
  expect_error(.bz_render(.bz_fn_doc(list(list(list(columns = "x", border = 1))))),
               "`rtf_footnotes()` row 1's `border` must be NULL or an rtf_border() object.",
               fixed = TRUE)
})

# ---- title blocks ------------------------------------------------------------

.bz_ti_doc <- function(format, ...) {
  rtf_document() |>
    rtf_section(page = 1, header = NULL, footer = NULL) |>
    rtf_tables(list(.bz_df())) |>
    rtf_config(style = rtf_style(title_format = format)) |>
    rtf_titles(list(c("Table 14.1.1", "Demographics", "Safety Analysis Set")), ...)
}

test_that("rtf_titles(border = ) in the table format is the whole block's", {
  txt <- .bz_render(.bz_ti_doc("table",
                               border = rtf_border(top = TRUE, bottom = TRUE)))
  r <- .bz_fn_rows(txt, "Table 14\\.1\\.1|Demographics|Safety Analysis Set")
  expect_length(r, 3L)
  expect_true(grepl("clbrdrt", r[1L], fixed = TRUE))
  expect_false(grepl("clbrdr", r[2L], fixed = TRUE))
  expect_true(grepl("clbrdrb", r[3L], fixed = TRUE))
  expect_false(grepl("clbrdrt", r[3L], fixed = TRUE))
  # rtf_style(titles = list(border = )) reaches it the same way
  doc <- rtf_document() |>
    rtf_section(page = 1, header = NULL, footer = NULL) |>
    rtf_tables(list(.bz_df())) |>
    rtf_config(style = rtf_style(title_format = "table",
                                 titles = list(border = rtf_border(top = TRUE,
                                                                   bottom = TRUE)))) |>
    rtf_titles(list(c("Table 14.1.1", "Demographics", "Safety Analysis Set")))
  expect_identical(.bz_render(doc), txt)
  expect_error(rtf_titles(.bz_ti_doc("table"), list("T"), border = "top"),
               "`rtf_titles(border = )` must be NULL or an rtf_border() object.",
               fixed = TRUE)
})

test_that("rtf_titles(border = NULL), the default, writes what it did before", {
  plain <- .bz_render(.bz_ti_doc("table"))
  expect_false(grepl("clbrdr", paste(.bz_fn_rows(plain, "Demographics")), fixed = TRUE))
  expect_identical(.bz_render(.bz_ti_doc("table", border = NULL)), plain)
})

test_that("rtf_titles(border = ) in the text format warns once and draws nothing", {
  msg <- "drawn only when title_format / footnote_format = \"table\""
  expect_warning(txt_b <- .bz_render(.bz_ti_doc("text", border = rtf_border(bottom = TRUE))),
                 msg, fixed = TRUE)
  w <- character(0)
  withCallingHandlers(.bz_render(.bz_ti_doc("text", border = rtf_border(bottom = TRUE))),
                      warning = function(c) {
                        w <<- c(w, conditionMessage(c))
                        invokeRestart("muffleWarning")
                      })
  expect_length(w, 1L)
  expect_identical(txt_b, .bz_render(.bz_ti_doc("text")))
})

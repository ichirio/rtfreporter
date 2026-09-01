# The footnote block now uses the page footer's mechanism (#296).

.df  <- function() data.frame(a = c("x", "y"), b = c("1", "2"),
                              stringsAsFactors = FALSE)
.tbl <- function() rtftable(.df(), border = "tfl",
                            column_widths_twips = c(1500L, 1200L))

.run <- function(..., fmt = NULL) {
  d <- rtf_document(page = rtf_page(orientation = "landscape"),
                    default_format = fmt %||% rtf_default_format()) |>
    rtf_tables(.tbl()) |>
    rtf_footnotes(...)
  f <- tempfile(fileext = ".rtf"); on.exit(unlink(f), add = TRUE)
  generate_rtfreport(d, f, overwrite = TRUE)
  readLines(f, warn = FALSE)
}
.rows  <- function(l, pat = "FN") grep(pat, l, value = TRUE)
.cells <- function(l, pat = "FN")
  lengths(regmatches(.rows(l, pat), gregexpr("\\\\cellx", .rows(l, pat))))
.cellx <- function(l, pat = "FN")
  unique(unlist(regmatches(.rows(l, pat), gregexpr("cellx[0-9]+", .rows(l, pat)))))
.align <- function(l, pat = "FN")
  unique(unlist(regmatches(.rows(l, pat), gregexpr("\\\\q[lrc]", .rows(l, pat)))))
.rule  <- function(l, pat = "FN") any(grepl("clbrdrt", .rows(l, pat)))

# ──────── the row model ────────────────────────────────────────────────────

test_that("a bare string is one left-aligned cell", {
  l <- .run(list(c("FN one", "FN two")))
  expect_equal(.cells(l), c(1L, 1L))
  expect_equal(.align(l), "\\ql")
})

test_that("c(l = , r = ) gives a row of two positioned cells", {
  l <- .run(list(list(c(l = "FN left", r = "FN right"))))
  expect_equal(.cells(l), 2L)
  expect_length(.cellx(l), 2L)                # equal division
  expect_setequal(.align(l), c("\\ql", "\\qr"))
})

test_that("l / c / r gives three", {
  l <- .run(list(list(c(l = "FN a", c = "FN b", r = "FN c"))))
  expect_equal(.cells(l), 3L)
  expect_setequal(.align(l), c("\\ql", "\\qc", "\\qr"))
})

test_that("the block align picks the slot a bare string lands in", {
  expect_equal(.align(.run(list(c("FN one")), align = "left")),   "\\ql")
  expect_equal(.align(.run(list(c("FN one")), align = "center")), "\\qc")
  expect_equal(.align(.run(list(c("FN one")), align = "right")),  "\\qr")
})

test_that("a row's own slot beats the block align", {
  expect_equal(.align(.run(list(list(c(r = "FN one"))), align = "center")),
               "\\qr")
})

test_that("a blank string is still a row of its own", {
  l <- .run(list(c("FN one", "", "FN two")))
  # three rows: the middle one has no text to match on, so count \row instead
  i <- grep("FN one", l)
  expect_true(grepl("\\\\cellx[0-9]+\\\\ql\\\\li0\\\\ri0 \\\\cell", l[i + 1L]))
})

test_that("per-row style lists are refused, pointing at the block", {
  expect_error(.run(list(list(list(text = "x", bold = TRUE)))),
               "no longer supported")
})

test_that("only l / c / r may name a row", {
  expect_error(.run(list(list(c(x = "FN one")))), "l / c / r")
})

# ──────── borders ──────────────────────────────────────────────────────────

test_that("no rule by default -- unlike the page footer", {
  expect_false(.rule(.run(list(c("FN one")))))
})

test_that("rtf_footnotes(border = ) puts one back", {
  expect_true(.rule(.run(list(c("FN one")), border = rtf_border(top = TRUE))))
})

test_that("the rule lands on the first row only", {
  l <- .run(list(c("FN one", "FN two")), border = rtf_border(top = TRUE))
  expect_true(grepl("clbrdrt", .rows(l, "FN one")))
  expect_false(grepl("clbrdrt", .rows(l, "FN two")))
})

test_that("border is validated", {
  d <- rtf_document() |> rtf_tables(.tbl())
  expect_error(rtf_footnotes(d, list("x"), border = "top"), "rtf_border")
})

# ──────── an independent table ─────────────────────────────────────────────

test_that("a paragraph separates the footnote from the body table", {
  # without it RTF merges the two runs of \trowd into one table, and the
  # footnote could not carry a width of its own
  l <- .run(list(c("FN one")))
  i <- grep("FN one", l)
  expect_true(any(grepl("{\\pard\\fs2\\par}", l[seq_len(i)], fixed = TRUE)))
})

test_that("the block keeps its own width", {
  l <- .run(list(c("FN one")),
            fmt = rtf_default_format(footnote_width = "page"))
  expect_equal(.cellx(l), "cellx13680")
  l2 <- .run(list(c("FN one")))
  expect_equal(.cellx(l2), "cellx2700")       # the content width
})

# ──────── style, markup and tokens ─────────────────────────────────────────

test_that("the block takes a font size, and the height follows", {
  l <- .run(list(c("FN one")), font_size_half_points = 14L)
  expect_true(any(grepl("\\\\fs14", .rows(l))))
  expect_true(any(grepl(
    paste0("\\\\trrh", rtfreporter:::.default_row_height_twips(14L)),
    .rows(l))))
})

test_that("page-number tokens are expanded now", {
  l <- .run(list(c("FN Page {PAGE} of {TOTAL_PAGES}")))
  expect_true(any(grepl("FN Page 1 of 1", l, fixed = TRUE)))
})

test_that("markup applies to the block", {
  l <- .run(list(c("^{a} FN marker")))
  expect_true(any(grepl("\\\\super", .rows(l))))
})

test_that("markup can be switched off for the block alone", {
  l <- .run(list(c("^{a} FN marker")), markup = "none")
  expect_false(any(grepl("\\\\super", .rows(l))))
})

# ──────── the text (paragraph) form ────────────────────────────────────────

test_that("plain strings still work under footnote_format = 'text'", {
  l <- .run(list(c("FN one")),
            fmt = rtf_default_format(footnote_format = "text"))
  expect_true(any(grepl("\\\\pard", .rows(l))))
  expect_false(any(grepl("\\\\cellx", .rows(l))))
})

test_that("an l / c / r row under the text form is a clear error", {
  expect_error(
    .run(list(list(c(l = "a", r = "b"))),
         fmt = rtf_default_format(footnote_format = "text")),
    "table form")
})

# ──────── the band renderer now honours markup (#292 wiring gap) ───────────

test_that("rtf_header(markup = ) reaches the rendered band", {
  d <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = rtf_header(
      rows = list(c(l = "^{a} HDR")), markup = "script"))) |>
    rtf_tables(.tbl())
  f <- tempfile(fileext = ".rtf"); on.exit(unlink(f), add = TRUE)
  generate_rtfreport(d, f, overwrite = TRUE)
  expect_true(any(grepl("\\\\super", grep("HDR", readLines(f, warn = FALSE),
                                          value = TRUE))))
})

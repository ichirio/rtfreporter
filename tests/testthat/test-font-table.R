# Multi-entry font table and per-element font family (#293 / #44).

.df  <- function() data.frame(a = c("x", "y"), b = c("1", "2"),
                              stringsAsFactors = FALSE)
.run <- function(doc) {
  f <- tempfile(fileext = ".rtf"); on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  readLines(f, warn = FALSE)
}
.ftbl <- function(l) grep("fonttbl", l, value = TRUE)[1L]
.fcmd <- function(l, pat) {
  h <- grep(pat, l, value = TRUE)
  unique(unlist(regmatches(h, gregexpr("\\\\f[0-9]+(?![a-z0-9])", h, perl = TRUE))))
}

# ──────── the helpers ──────────────────────────────────────────────────────

test_that(".font_names accepts both shapes", {
  expect_equal(rtfreporter:::.font_names(list(list(name = "A"),
                                              list(name = "B"))), c("A", "B"))
  expect_equal(rtfreporter:::.font_names(c("A", "B")), c("A", "B"))
  expect_equal(rtfreporter:::.font_names(NULL), character(0))
})

test_that(".collect_fonts keeps the declared default first and de-duplicates", {
  f <- rtfreporter:::.collect_fonts(list(list(name = "Courier")),
                                    c("Arial", "Courier", "Arial"))
  expect_equal(f, c("Courier", "Arial"))
})

test_that(".build_font_index_map is 0-based", {
  m <- rtfreporter:::.build_font_index_map(c("Courier", "Arial"))
  expect_equal(m[["Courier"]], 0L)
  expect_equal(m[["Arial"]], 1L)
})

test_that(".f_cmd_for is silent for the default and for unknown names", {
  m <- rtfreporter:::.build_font_index_map(c("Courier", "Arial"))
  expect_equal(rtfreporter:::.f_cmd_for(NULL, m), "")
  expect_equal(rtfreporter:::.f_cmd_for("Courier", m), "")   # index 0
  expect_equal(rtfreporter:::.f_cmd_for("Arial", m), "\\f1")
  # an absent name must not error -- `[[` would
  expect_equal(rtfreporter:::.f_cmd_for("Nope", m), "")
  expect_equal(rtfreporter:::.f_cmd_for("Arial", list()), "")
})

test_that(".build_font_table_rtf declares every entry", {
  expect_equal(rtfreporter:::.build_font_table_rtf(c("Courier New", "Arial")),
    "{\\fonttbl{\\f0\\fnil\\fcharset0 Courier New;}{\\f1\\fnil\\fcharset0 Arial;}}")
})

# ──────── nothing asked for, nothing changes ───────────────────────────────

test_that("a single-font document is unchanged", {
  l <- .run(rtf_document() |> rtf_tables(rtftable(.df(), border = "tfl")))
  expect_equal(.ftbl(l), "{\\fonttbl{\\f0\\fnil\\fcharset0 Courier;}}")
  expect_length(.fcmd(l, "\\\\cell"), 0L)      # no \fN anywhere in the body
})

# ──────── per-element selection ────────────────────────────────────────────

test_that("a table font is declared and selected", {
  l <- .run(rtf_document() |>
              rtf_tables(rtftable(.df(), border = "tfl", font = "Arial")))
  expect_match(.ftbl(l), "\\\\f1\\\\fnil\\\\fcharset0 Arial;")
  expect_equal(.fcmd(l, "x\\\\cell"), "\\f1")
})

test_that("several elements each get their own index", {
  l <- .run(
    rtf_document(default_format = rtf_default_format(title_format = "table")) |>
      rtf_section(page = 1, secinfo = list(header = rtf_header(
        rows = list(c(l = "MYHDR")), font = "Times New Roman"))) |>
      rtf_tables(rtftable(.df(), border = "tfl", font = "Arial")) |>
      rtf_titles(list("MYTITLE"), font = "Calibri") |>
      rtf_footnotes(list("MYFOOT")))
  for (nm in c("Calibri", "Arial", "Times New Roman")) {
    expect_match(.ftbl(l), nm, fixed = TRUE)
  }
  expect_length(.fcmd(l, "MYHDR"), 1L)
  expect_length(.fcmd(l, "MYTITLE"), 1L)
  expect_length(.fcmd(l, "MYFOOT"), 0L)        # inherits the default
  expect_false(identical(.fcmd(l, "MYHDR"), .fcmd(l, "MYTITLE")))
})

test_that("a declared table is honoured, default first", {
  l <- .run(rtf_document(font_table = list(list(name = "Courier New"),
                                           list(name = "Arial"))) |>
              rtf_tables(rtftable(.df(), border = "tfl", font = "Arial")))
  expect_equal(.ftbl(l),
    "{\\fonttbl{\\f0\\fnil\\fcharset0 Courier New;}{\\f1\\fnil\\fcharset0 Arial;}}")
  expect_equal(.fcmd(l, "x\\\\cell"), "\\f1")
})

test_that("asking for the document default emits nothing", {
  l <- .run(rtf_document() |>
              rtf_tables(rtftable(.df(), border = "tfl", font = "Courier")))
  expect_equal(.ftbl(l), "{\\fonttbl{\\f0\\fnil\\fcharset0 Courier;}}")
  expect_length(.fcmd(l, "x\\\\cell"), 0L)
})

test_that("font and size travel together", {
  l <- .run(rtf_document() |>
              rtf_tables(rtftable(.df(), border = "tfl", font = "Arial",
                                  font_size_half_points = 24L)))
  h <- grep("x\\\\cell", l, value = TRUE)
  expect_match(h, "\\\\f1\\\\fs24")
})

test_that("rtf_footnotes(font = ) reaches the block", {
  l <- .run(rtf_document() |>
              rtf_tables(rtftable(.df(), border = "tfl")) |>
              rtf_footnotes(list("MYFOOT"), font = "Arial"))
  expect_match(.ftbl(l), "Arial", fixed = TRUE)
  expect_equal(.fcmd(l, "MYFOOT"), "\\f1")
})

# ──────── validation ───────────────────────────────────────────────────────

test_that("font must be a single family name", {
  expect_error(rtftable(.df(), font = 1), "font family")
  expect_error(rtftable(.df(), font = c("A", "B")), "font family")
  expect_error(rtf_header(list(c(l = "x")), font = ""), "font family")
})

test_that("a legacy character-vector header still renders", {
  # such a section is not a list, so font collection must skip it
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_tables(rtftable(.df(), border = "tfl"))
  expect_silent(.run(doc))
})

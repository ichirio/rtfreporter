# Command-template substitution (#306).
#
# `.cmd_fmt()` used to be best-effort: an unsupplied key was left verbatim in
# the output. That let `{orientation_cmd}`, `{header_dist_twips}` and
# `{footer_dist_twips}` ship inside six committed example files, where a reader
# renders the names as text on the page.

fmt   <- function(...) rtfreporter:::.cmd_fmt(...)
slots <- function(...) rtfreporter:::.cmd_fmt_slots(...)

# ── slot detection ─────────────────────────────────────────────────────────

test_that("slots are the bare identifiers in braces", {
  expect_identical(slots("\\cellx{cx}"), "cx")
  expect_identical(slots("\\paperw{width_twips}\\paperh{height_twips}"),
                   c("width_twips", "height_twips"))
  expect_identical(slots("no slots here"), character(0))
})

test_that("a repeated slot is reported once", {
  expect_identical(slots("{a} {b} {a}"), c("a", "b"))
})

test_that("an RTF group is not mistaken for a slot", {
  # RTF groups open with a control word, so the brace is followed by a
  # backslash and never by a bare identifier
  expect_identical(slots("{\\header \\fs18}"), character(0))
  expect_identical(slots("{\\fonttbl{\\f0\\fnil Courier;}}"), character(0))
  expect_identical(slots("\\{escaped\\}"), character(0))
})

test_that("a leading digit is not a slot", {
  expect_identical(slots("{1440}"), character(0))
})

# ── substitution ───────────────────────────────────────────────────────────

test_that("every supplied slot is filled", {
  expect_identical(fmt("\\cellx{cx}", list(cx = 1440L)), "\\cellx1440")
  expect_identical(
    fmt("\\paperw{w}\\paperh{h}", list(w = 15840L, h = 12240L)),
    "\\paperw15840\\paperh12240"
  )
})

test_that("a repeated slot is filled everywhere", {
  expect_identical(fmt("{a}-{a}", list(a = "x")), "x-x")
})

test_that("an empty value erases the slot", {
  # this is how a portrait page drops the document-level \landscape
  expect_identical(fmt("\\paperw1{orientation_cmd}\\margl2",
                       list(orientation_cmd = "")),
                   "\\paperw1\\margl2")
})

test_that("a template with no slots is returned unchanged", {
  expect_identical(fmt("\\sectd", list()), "\\sectd")
  expect_identical(fmt("\\sectd"), "\\sectd")
})

test_that("extra values that the template does not declare are harmless", {
  expect_identical(fmt("\\cellx{cx}", list(cx = 10L, unused = 3L)), "\\cellx10")
})

# ── the guard ──────────────────────────────────────────────────────────────

test_that("an unsupplied slot is an error, not silent output", {
  expect_error(fmt("\\paperw{w}\\paperh{h}", list(w = 1L)),
               "unsupplied placeholder")
  expect_error(fmt("\\paperw{w}\\paperh{h}", list(w = 1L)), "`\\{h\\}`")
})

test_that("the error names every missing slot and shows the template", {
  msg <- tryCatch(fmt("{a}{b}{c}", list(b = 1L)),
                  error = function(e) conditionMessage(e))
  expect_match(msg, "placeholders")
  expect_match(msg, "`\\{a\\}`")
  expect_match(msg, "`\\{c\\}`")
  expect_match(msg, "Template: ")
})

test_that("a template with slots and no values at all errors", {
  expect_error(fmt("\\cellx{cx}"), "unsupplied placeholder")
  expect_error(fmt("\\cellx{cx}", list()), "unsupplied placeholder")
})

# ── the regression the guard exists for ────────────────────────────────────

test_that("no rendered report can carry a placeholder", {
  df  <- data.frame(A = "1.5", B = "2", stringsAsFactors = FALSE)
  doc <- rtf_document(page = rtf_page(orientation = "landscape")) |>
    rtf_tables(rtftable(df, border = "tfl"))
  f <- withr::local_tempfile(fileext = ".rtf")
  generate_rtfreport(doc, f, overwrite = TRUE)
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_false(grepl(rtfreporter:::.CMD_FMT_SLOT, txt))
})

test_that("a portrait report is equally clean", {
  df  <- data.frame(A = "1.5", stringsAsFactors = FALSE)
  doc <- rtf_document(page = rtf_page(orientation = "portrait")) |>
    rtf_tables(rtftable(df, border = "tfl"))
  f <- withr::local_tempfile(fileext = ".rtf")
  generate_rtfreport(doc, f, overwrite = TRUE)
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_false(grepl(rtfreporter:::.CMD_FMT_SLOT, txt))
  expect_false(grepl("\\\\landscape", txt))
})

test_that("the committed examples carry no placeholder", {
  dir <- system.file("rtf-examples", package = "rtfreporter")
  skip_if(!nzchar(dir) || !dir.exists(dir), "rtf-examples not installed")
  files <- list.files(dir, pattern = "\\.rtf$", full.names = TRUE)
  skip_if(length(files) == 0L, "no example files")
  bad <- Filter(function(f) {
    any(grepl(rtfreporter:::.CMD_FMT_SLOT, readLines(f, warn = FALSE)))
  }, files)
  expect_identical(basename(bad), character(0))
})

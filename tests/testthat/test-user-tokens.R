# Tokens of one's own: rtf_document(tokens = ), options(rtfreporter.tokens).

own_doc <- function(tokens = NULL, footer = "S={STUDY}") {
  rtf_document(tokens = tokens) |>
    rtf_section(page = 1, secinfo = list(header = NULL,
      footer = rtf_footer(list(c(l = footer))))) |>
    rtf_tables(as_rtftables(data.frame(A = "a")),
               titles = list("Study {STUDY}"), footnotes = list("Cut-off {CUTOFF}"))
}
render_own <- function(doc) {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

test_that("a document's tokens fill the footer, titles and footnotes", {
  out <- render_own(own_doc(list(STUDY = "ABC-123", CUTOFF = "01JUN2026")))
  expect_match(out, "S=ABC-123", fixed = TRUE)
  expect_match(out, "Study ABC-123", fixed = TRUE)
  expect_match(out, "Cut-off 01JUN2026", fixed = TRUE)
  expect_false(grepl("STUDY", out, fixed = TRUE))
})

test_that("the option sets them for a session; the document's value wins", {
  old <- options(rtfreporter.tokens = list(STUDY = "OPT-1", CUTOFF = "OPTDATE"))
  on.exit(options(old), add = TRUE)
  out <- render_own(own_doc())
  expect_match(out, "S=OPT-1", fixed = TRUE)
  expect_match(out, "Cut-off OPTDATE", fixed = TRUE)
  out <- render_own(own_doc(list(STUDY = "DOC-2")))
  expect_match(out, "S=DOC-2", fixed = TRUE)
  expect_match(out, "Cut-off OPTDATE", fixed = TRUE)
})

test_that("a value is RTF-escaped, and a number is written as text", {
  out <- render_own(own_doc(list(STUDY = "A{B}\\C", CUTOFF = 12)))
  expect_match(out, "S=A\\{B\\}\\\\C", fixed = TRUE)
  expect_match(out, "Cut-off 12", fixed = TRUE)
})

test_that("names follow the rule, and never rtfreporter's own", {
  expect_error(rtf_document(tokens = list(study = "x")), "upper case")
  expect_error(rtf_document(tokens = list(`1ST` = "x")), "upper case")
  expect_error(rtf_document(tokens = list(PAGE = "x")), "rtfreporter's own")
  expect_error(rtf_document(tokens = list(PROGRAM_FULL = "x")), "rtfreporter's own")
  expect_error(rtf_document(tokens = list(DATETIME = "x")), "rtfreporter's own")
  expect_error(rtf_document(tokens = list("x")), "has a name")
  expect_error(rtf_document(tokens = list(STUDY = c("a", "b"))), "one value")
  expect_error(rtf_document(tokens = list(STUDY = NA)), "one value")
  expect_error(rtf_document(tokens = list(STUDY = "a", STUDY = "b")), "named twice")
  old <- options(rtfreporter.tokens = list(page = "x"))
  on.exit(options(old), add = TRUE)
  expect_error(render_own(own_doc()), "options\\(rtfreporter.tokens\\)")
})

test_that("a token not set is left as it is", {
  out <- render_own(own_doc(list(STUDY = "ABC"), footer = "{OTHER}"))
  expect_match(out, "\\{OTHER\\}", fixed = TRUE)
})

test_that("rtf_text_tokens() lists the session's and a document's own", {
  base <- rtf_text_tokens()
  expect_false("own" %in% base$kind)
  old <- options(rtfreporter.tokens = list(STUDY = "OPT"))
  on.exit(options(old), add = TRUE)
  tk <- rtf_text_tokens(rtf_document(tokens = list(STUDY = "DOC", CUTOFF = "D")))
  own <- tk[tk$kind == "own", ]
  expect_setequal(own$token, c("{STUDY}", "{CUTOFF}"))
  expect_identical(own$example[own$token == "{STUDY}"], "DOC")
  expect_identical(nrow(tk), nrow(base) + 2L)
  expect_error(rtf_text_tokens("x"), "rtf_document")
})

test_that("a column header is not filled: set_col_header() still asks for values", {
  # column headers take their values from set_col_header(values = ); a token
  # of one's own is not left there for the renderer, which would not fill it
  fill <- rtfreporter:::.fill_text_tokens
  old <- options(rtfreporter.tokens = list(STUDY = "ABC"))
  on.exit(options(old), add = TRUE)
  expect_error(fill("N={n} {STUDY}", list(n = 3), "header"), "no value for")
  expect_identical(unname(fill("N={n} {STUDY}", list(n = 3, STUDY = "ABC"), "header")),
                   "N=3 ABC")
})

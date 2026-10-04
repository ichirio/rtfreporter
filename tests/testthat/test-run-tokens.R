# Render-time tokens for the program and the run time (#478).

run_doc <- function(footer_rows) {
  rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = NULL,
                                         footer = rtf_footer(footer_rows))) |>
    rtf_tables(as_rtftables(data.frame(A = "a", B = "b")))
}
render <- function(doc, ...) {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE, ...)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

test_that("{PROGRAM}, {PROGRAM_NAME}, {PROGRAM_DIR} say which program wrote it", {
  out <- render(run_doc(list(c(l = "P={PROGRAM} N={PROGRAM_NAME} D={PROGRAM_DIR}"))),
                program = "work/tfl/t_dm.R")
  expect_match(out, "P=work/tfl/t_dm.R N=t_dm.R D=work/tfl", fixed = TRUE)
  # a Windows path keeps its backslashes (RTF-escaped as \\)
  out <- render(run_doc(list(c(l = "{PROGRAM}"))), program = "C:\\tfl\\t_dm.R")
  expect_match(out, "C:\\\\tfl\\\\t_dm.R", fixed = TRUE)
})

test_that("{PROGRAM_FULL} is the program's path made absolute; {PROGRAM} stays as given", {
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(tempdir())
  # the system's separator; in the RTF a backslash is escaped (\\)
  sep <- if (.Platform$OS.type == "windows") "\\" else "/"
  rtf <- function(p) gsub("\\", "\\\\", p, fixed = TRUE)
  here <- normalizePath(getwd(), winslash = "\\")
  out <- render(run_doc(list(c(l = "F=<{PROGRAM_FULL}> P=<{PROGRAM}>"))),
                program = "work/tfl/t_dm.R")
  want <- paste(here, "work", "tfl", "t_dm.R", sep = sep)
  expect_match(out, paste0("F=<", rtf(want), ">"), fixed = TRUE)
  expect_match(out, "P=<work/tfl/t_dm.R>", fixed = TRUE)
  # never read as {PROGRAM} followed by "_FULL}"
  expect_false(grepl("_FULL", out, fixed = TRUE))
  # an absolute path stays where it is, in the system's separator
  abs <- paste(here, "abs", "t_ae.R", sep = sep)
  expect_match(render(run_doc(list(c(l = "<{PROGRAM_FULL}>"))), program = abs),
               paste0("<", rtf(abs), ">"), fixed = TRUE)
})

test_that("on Windows {PROGRAM_FULL} uses \\ and the RTF escapes it", {
  skip_on_os(c("mac", "linux", "solaris"))
  out <- render(run_doc(list(c(l = "<{PROGRAM_FULL}>"))),
                program = "C:/tfl/t_dm.R")
  # C:\tfl\t_dm.R, each backslash written \\ in the RTF
  expect_match(out, "<C:\\\\tfl\\\\t_dm.R>", fixed = TRUE)
  expect_false(grepl("<C:/tfl", out, fixed = TRUE))
})

test_that("on Windows a short (8.3) folder name is written out long", {
  skip_on_os(c("mac", "linux", "solaris"))
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  # the working folder by its short name, as on a CI runner (RUNNER~1);
  # the program file does not exist (yet)
  setwd(utils::shortPathName(tempdir()))
  long <- normalizePath(tempdir(), winslash = "\\")
  got <- rtfreporter:::.full_path("work/tfl/t_dm.R")
  expect_identical(got, paste(long, "work", "tfl", "t_dm.R", sep = "\\"))
  expect_false(grepl("~", got, fixed = TRUE))
})

test_that("{PROGRAM_FULL} drops . and .. from a path that does not exist", {
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(tempdir())
  sep <- if (.Platform$OS.type == "windows") "\\" else "/"
  here <- normalizePath(getwd(), winslash = "\\")
  expect_identical(rtfreporter:::.full_path("./work/../work/t.R"),
                   paste(here, "work", "t.R", sep = sep))
})

test_that("{PROGRAM_FULL} with no program known is the same error as {PROGRAM}", {
  # outside generate_rtfreport() no program is set: the substitution itself
  # (so the test does not depend on running under Rscript)
  ctx <- rtfreporter:::.run_ctx
  old <- ctx$program
  on.exit(assign("program", old, envir = ctx), add = TRUE)
  ctx$program <- NULL
  for (tok in c("\\{PROGRAM\\}", "\\{PROGRAM_FULL\\}")) {
    expect_error(rtfreporter:::.substitute_run_tokens(paste0("x ", tok)),
                 "no program is known", label = tok)
  }
})

test_that("the program comes from the argument, else the option", {
  old <- options(rtfreporter.program = "from/option.R")
  on.exit(options(old), add = TRUE)
  expect_match(render(run_doc(list(c(l = "{PROGRAM_NAME}")))), "option.R",
               fixed = TRUE)
  expect_match(render(run_doc(list(c(l = "{PROGRAM_NAME}"))), program = "arg.R"),
               "arg.R", fixed = TRUE)
  options(rtfreporter.program = NULL)
  skip_if(any(grepl("^--file=", commandArgs(FALSE))), "running under Rscript")
  expect_error(render(run_doc(list(c(l = "{PROGRAM}")))), "no program is known")
})

test_that("a document can carry its program; the argument still wins", {
  doc <- rtf_document(program = "doc/own.R") |>
    rtf_section(page = 1, secinfo = list(header = NULL,
      footer = rtf_footer(list(c(l = "{PROGRAM_NAME}"))))) |>
    rtf_tables(as_rtftables(data.frame(A = "a")))
  expect_match(render(doc), "own.R", fixed = TRUE)
  expect_match(render(doc, program = "arg.R"), "arg.R", fixed = TRUE)
  expect_error(rtf_document(program = 1), "single string")
})

test_that("{DATETIME} is the time the file is written, in the C locale", {
  old <- options(rtfreporter.render_time = as.POSIXct("2026-09-25 10:05:00"))
  on.exit(options(old), add = TRUE)
  out <- render(run_doc(list(c(l = "Generated on: {DATETIME}"),
                             c(l = "ISO {DATETIME:%Y-%m-%dT%H:%M}"))))
  expect_match(out, "Generated on: 25Sep2026  10:05", fixed = TRUE)
  expect_match(out, "ISO 2026-09-25T10:05", fixed = TRUE)
  options(rtfreporter.datetime_format = "%d%b%Y")
  expect_match(render(run_doc(list(c(l = "{DATETIME}")))), "25Sep2026",
               fixed = TRUE)
  options(rtfreporter.datetime_format = NULL)
})

test_that("titles and footnotes take the run tokens too", {
  old <- options(rtfreporter.render_time = as.POSIXct("2026-09-25 10:05:00"))
  on.exit(options(old), add = TRUE)
  doc <- rtf_document() |>
    rtf_tables(as_rtftables(data.frame(A = "a")),
               titles = list("Table 1 ({PROGRAM_NAME})"),
               footnotes = list("Run {DATETIME:%Y}"))
  out <- render(doc, program = "t_x.R")
  expect_match(out, "Table 1 (t_x.R)", fixed = TRUE)
  expect_match(out, "Run 2026", fixed = TRUE)
})

test_that("a text without the tokens is untouched", {
  expect_identical(rtfreporter:::.substitute_run_tokens("plain \\{col\\} text"),
                   "plain \\{col\\} text")
})

test_that("rtf_text_tokens() lists exactly the tokens a page's text has filled", {
  tk <- rtf_text_tokens()
  expect_named(tk, c("token", "kind", "when", "description", "example"))
  expect_false(anyDuplicated(tk$token) > 0)
  # every token listed is replaced when the file is written: none is left
  # in the RTF as its own text
  old <- options(rtfreporter.render_time = as.POSIXct("2026-09-25 10:05:00"))
  on.exit(options(old), add = TRUE)
  out <- render(run_doc(lapply(tk$token, function(x) c(l = paste0("<", x, ">")))),
                program = "work/tfl/t_dm.R")
  for (x in tk$token) {
    esc <- gsub("}", "\\}", gsub("{", "\\{", x, fixed = TRUE), fixed = TRUE)
    expect_false(grepl(paste0("<", esc, ">"), out, fixed = TRUE), label = x)
  }
  # and the list is the renderer's own: the tokens set_col_header() leaves
  # for it, less {SECTION_PAGES} (removed in 0.7.31; kept there so that
  # writing it is an error that says what to use)
  own <- paste0("{", setdiff(rtfreporter:::.RENDER_TOKENS,
                             "SECTION_PAGES"), "}")
  expect_setequal(tk$token, own)
})

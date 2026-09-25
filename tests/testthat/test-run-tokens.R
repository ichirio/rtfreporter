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

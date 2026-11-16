# End-to-end RTF file generation through the pipe API.


test_that("generate_rtfreport() writes a valid non-empty RTF file", {
  df_safety <- data.frame(
    Event    = c("Headache", "Nausea", "Dizziness"),
    Mild     = c(5, 3, 2),
    Moderate = c(2, 1, 1),
    Severe   = c(0, 0, 1)
  )
  df_efficacy <- data.frame(
    Response = c("Complete", "Partial", "No Response"),
    Count    = c(25, 15, 10),
    Percent  = c("50%", "30%", "20%")
  )

  doc <- rtf_document()
  doc <- rtf_tables(doc, list(
    rtftable(df_safety, border = "tfl", row_height_twips = 280L),
    rtftable(df_efficacy, border = "tfl", row_height_twips = 280L)
  ))
  doc <- rtf_section(doc, page = 1, secinfo = list(
    header = rtf_header(rows = list(l = "Clinical Report", r = "Safety")),
    footer = rtf_footer(rows = list(c = "Page 1"))
  ))
  doc <- rtf_section(doc, page = 2, secinfo = list(
    header = rtf_header(rows = list(l = "Clinical Report", r = "Efficacy")),
    footer = rtf_footer(rows = list(c = "Page 2"))
  ))

  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)

  expect_invisible(generate_rtfreport(doc, f, overwrite = TRUE))
  expect_true(file.exists(f))
  expect_gt(file.info(f)$size, 0L)

  header_bytes <- readChar(f, nchars = 6L)
  expect_match(header_bytes, "^\\{\\\\rtf")
})

test_that("generate_rtfreport() refuses to overwrite without overwrite = TRUE", {
  df <- data.frame(A = 1L, B = "x", stringsAsFactors = FALSE)
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1,
                     secinfo = list(header = NULL, footer = NULL))
  doc <- rtf_tables(doc, list(df))

  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)

  generate_rtfreport(doc, f)
  # written over by default (#594): a report program is run again and again
  expect_no_error(generate_rtfreport(doc, f))
  expect_error(generate_rtfreport(doc, f, overwrite = FALSE), "already exists")
})

test_that("generate_rtfreport() rejects unsupported report types", {
  expect_error(generate_rtfreport(list(foo = 1), tempfile()),
               "rtf_document.*rtfreport")
})

test_that("FT-DC6802 [FR-DC386]: generate_rtfreport() reads its resource files from the installed package only, never from a folder's inst/resources", {
  skip_on_cran()
  rscript <- file.path(R.home("bin"),
                       if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  if (!file.exists(rscript)) skip("no Rscript to start a child R")
  pkg <- find.package("rtfreporter")
  if (!file.exists(file.path(pkg, "Meta", "package.rds"))) {
    skip("rtfreporter is not installed (tests run from source): nothing to give a child R")
  }
  tmp <- tempfile("ft-dc6802-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  df <- data.frame(Event = c("Headache", "Nausea"), N = c(5, 3))
  doc <- rtf_tables(rtf_document(), list(rtftable(df, border = "tfl")))
  ref <- file.path(tmp, "ref.rtf")
  generate_rtfreport(doc, ref, overwrite = TRUE)
  expect_gt(file.info(ref)$size, 0L)

  # a working folder with a file of each name, at both places the old lookup tried
  # (each would stop, or change the row height, if it were read)
  for (d in c(file.path("work", "inst", "resources"),
              file.path("work", "r", "rtfreporter", "inst", "resources"))) {
    dir.create(file.path(tmp, d), recursive = TRUE)
    writeLines("stop(\"read from the working folder\")",
               file.path(tmp, d, "rtf_commands.R"))
    writeLines("rtfreporter_defaults <- list(default_row_height_twips_min = 9999L)",
               file.path(tmp, d, "rtfreporter_defaults.R"))
  }

  # a fresh R, so the package has not read its files yet, started in that folder
  input <- file.path(tmp, "input.rds")
  out <- file.path(tmp, "out.rtf")
  script <- file.path(tmp, "child.R")
  saveRDS(doc, input)
  writeLines(c(
    "args <- commandArgs(trailingOnly = TRUE)",
    ".libPaths(strsplit(args[1], args[5], fixed = TRUE)[[1]])",
    "setwd(args[2])",
    "rtfreporter::generate_rtfreport(readRDS(args[3]), args[4], overwrite = TRUE)"),
    script)
  res <- suppressWarnings(system2(
    rscript, c(shQuote(script), shQuote(paste(.libPaths(), collapse = .Platform$path.sep)),
               shQuote(file.path(tmp, "work")),
               shQuote(input), shQuote(out), shQuote(.Platform$path.sep)),
    stdout = TRUE, stderr = TRUE))
  expect_null(attr(res, "status"))
  expect_true(file.exists(out))
  expect_identical(readBin(out, "raw", file.info(out)$size),
                   readBin(ref, "raw", file.info(ref)$size))
})

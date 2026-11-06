# Helpers around assemble_rtf(): assemble_folder() and the table of contents.

# A minimal rtfreporter RTF whose header carries a "Table N  <title>" line.

.make_tbl_rtf <- function(dir, file, table_no, title) {
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1, secinfo = list(
    header = rtf_header(rows = list(
      c(l = "Co", r = "Page {PAGE} of {TOTAL_PAGES}"),
      c(c = paste0("Table ", table_no)),
      c(c = title))),
    footer = rtf_footer(c(c = "Confidential"))))
  doc <- rtf_tables(doc, list(
    data.frame(A = 1:2, B = c("x", "y"), stringsAsFactors = FALSE)))
  f <- file.path(dir, file)
  generate_rtfreport(doc, f, overwrite = TRUE)
  f
}

.demo_dir <- function() {
  d <- tempfile("asm"); dir.create(d)
  .make_tbl_rtf(d, "t_14_3_1.rtf", "14.3.1", "Adverse Events")
  .make_tbl_rtf(d, "t_14_1_1.rtf", "14.1.1", "Demographics")
  d
}

test_that("assemble_folder lists rtf files in natural order", {
  d <- .demo_dir()
  files <- assemble_folder(d)$file
  expect_length(files, 2L)
  expect_true(all(grepl("[.]rtf$", files)))
})

test_that("assemble_folder reads table number + title from the header, ordered by table", {
  d <- .demo_dir()
  sp <- assemble_folder(d)
  expect_s3_class(sp, "data.frame")
  expect_setequal(c("order", "file", "table", "heading", "label", "level", "pages"),
                  names(sp))
  # default order is by table number: 14.1.1 before 14.3.1
  expect_equal(sp$table, c("14.1.1", "14.3.1"))
  expect_match(sp$label[1L], "Table 14.1.1  Demographics")
  expect_equal(sp$pages, c(1L, 1L))
})

test_that("assemble_folder headings become heading rows of the toc", {
  d <- .demo_dir()
  sp <- assemble_folder(d)
  sp$heading <- ifelse(grepl("^14[.]1", sp$table), "DEMOG", "SAFETY")
  toc <- rtfreporter:::.spec_to_toc(sp)
  kinds <- vapply(toc, function(e) class(e)[1L], character(1L))
  expect_identical(kinds,
                   c("rtf_toc_heading", "rtf_toc_entry",
                     "rtf_toc_heading", "rtf_toc_entry"))
})

test_that("assemble_rtf(toc = <table>) writes an assembled file with a TOC", {
  d   <- .demo_dir()
  sp  <- assemble_folder(d)
  out <- tempfile(fileext = ".rtf")
  assemble_rtf(toc = sp, output_file = out, overwrite = TRUE)
  expect_true(file.exists(out))
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(txt, "Table of Contents")
  expect_match(txt, "PAGEREF")
})

test_that("assemble_folder does the whole thing and can save a CSV spec", {
  d   <- .demo_dir()
  out <- tempfile(fileext = ".rtf")
  sf  <- tempfile(fileext = ".csv")
  res <- assemble_folder(d, out, spec_file = sf, overwrite = TRUE)
  expect_true(file.exists(out))
  expect_true(file.exists(sf))
  expect_s3_class(res$spec, "data.frame")
  expect_equal(nrow(res$spec), 2L)
  # without spec_file the spec stays in memory only
  out2 <- tempfile(fileext = ".rtf")
  res2 <- assemble_folder(d, out2, overwrite = TRUE)
  expect_true(file.exists(out2))
})

test_that("the table of contents reads back a title's non-ASCII characters", {
  d <- tempfile("asm-u"); dir.create(d)
  # written as \uN? escapes; read back as the characters (a no-break space as a space)
  .make_tbl_rtf(d, "t1.rtf", "14.1.1", "Age \u2265 65 years, \u00b5g/L,\u00a0Caf\u00e9")
  f <- file.path(d, "t1.rtf")
  expect_match(paste(readLines(f, warn = FALSE), collapse = ""), "\\u8805?", fixed = TRUE)
  lab <- rtfreporter:::.rtf_table_label(f)
  expect_identical(lab$title, "Age \u2265 65 years, \u00b5g/L, Caf\u00e9")
  # the number and the title on one line: the title is the rest of it
  doc <- rtf_document() |>
    rtf_section(header = rtf_header(list(c(c = "Table 14.2.1  Vital Signs")))) |>
    rtf_tables(data.frame(A = 1))
  f2 <- file.path(d, "t2.rtf")
  generate_rtfreport(doc, f2)
  lab2 <- rtfreporter:::.rtf_table_label(f2)
  expect_identical(lab2$table, "14.2.1")
  expect_identical(lab2$title, "Vital Signs")
  expect_identical(lab2$label, "Table 14.2.1  Vital Signs")
})

test_that("assemble_folder() without an output file gives the table of contents", {
  dir <- file.path(tempdir(), "toc-table")
  dir.create(dir, showWarnings = FALSE)
  for (t in c("14.2.1", "14.1.1")) {
    doc <- rtf_document() |>
      rtf_tables(data.frame(Parameter = "Age", Value = "75.1")) |>
      rtf_titles(list(c(paste("Table", t), "Safety Population")))
    generate_rtfreport(doc, file.path(dir, paste0("t", t, ".rtf")),
                       overwrite = TRUE)
  }
  spec <- assemble_folder(dir)
  expect_s3_class(spec, "data.frame")
  expect_identical(basename(spec$file), c("t14.1.1.rtf", "t14.2.1.rtf"))
  expect_true(all(c("file", "label", "heading", "level") %in% names(spec)))

  # the table is the toc, and its files the input files
  spec$heading <- c("DEMOGRAPHICS", "SAFETY")
  out1 <- tempfile(fileext = ".rtf")
  expect_silent(assemble_rtf(toc = spec, output_file = out1))
  txt <- paste(readLines(out1, warn = FALSE), collapse = "")
  expect_match(txt, "DEMOGRAPHICS")
  expect_match(txt, "SAFETY")

  # or the path of the table, in its `order`
  csv <- tempfile(fileext = ".csv")
  spec$order <- c(2L, 1L)
  utils::write.csv(spec, csv, row.names = FALSE)
  out3 <- tempfile(fileext = ".rtf")
  assemble_rtf(toc = csv, output_file = out3)
  txt3 <- paste(readLines(out3, warn = FALSE), collapse = "")
  expect_lt(regexpr("SAFETY", txt3), regexpr("DEMOGRAPHICS", txt3))

  expect_error(assemble_rtf(toc = data.frame(file = spec$file),
                            output_file = tempfile(fileext = ".rtf")),
               "needs the columns `file` and `label`")
})

test_that("FT-AS130 [FR-AS151 FR-AS148 FR-AS149]: assemble_folder() orders numbered files by number, then the others by file name, the same in every locale (#827)", {
  d <- tempfile("asm-ord"); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  mk_unnumbered <- function(file, title) {
    doc <- rtf_document()
    doc <- rtf_section(doc, page = 1, secinfo = list(
      header = rtf_header(rows = list(c(c = title)))))
    doc <- rtf_tables(doc, list(data.frame(A = "1")))
    generate_rtfreport(doc, file.path(d, file), overwrite = TRUE)
  }
  # numbered: file names in the opposite order to the numbers
  .make_tbl_rtf(d, "z_t14_1_2.rtf", "14.1.2", "Demographics")
  .make_tbl_rtf(d, "Y_t14_1_10.rtf", "14.1.10", "Disposition")
  .make_tbl_rtf(d, "a_t14_3_1.rtf", "14.3.1", "Adverse Events")
  # unnumbered (listings, figures): case, "-" vs "_", digits
  mk_unnumbered("l16_2_1.rtf", "Listing 16.2.1b")
  mk_unnumbered("l16-2-1.rtf", "Listing 16.2.1a")
  mk_unnumbered("L16_2_10.rtf", "Listing 16.2.10")
  mk_unnumbered("L16_2_9.rtf", "Listing 16.2.9")
  mk_unnumbered("0_figure.rtf", "Figure 14.2.1")
  want <- c("z_t14_1_2.rtf", "Y_t14_1_10.rtf", "a_t14_3_1.rtf",
            "0_figure.rtf", "L16_2_9.rtf", "L16_2_10.rtf",
            "l16-2-1.rtf", "l16_2_1.rtf")

  old <- Sys.getlocale("LC_COLLATE")
  on.exit(Sys.setlocale("LC_COLLATE", old), add = TRUE)
  locales <- unique(c("C", old, "English_United States.1252",
                      "Japanese_Japan.utf8", "en_US.UTF-8"))
  tried <- 0L
  for (loc in locales) {
    if (!nzchar(suppressWarnings(Sys.setlocale("LC_COLLATE", loc)))) next
    tried <- tried + 1L
    sp <- assemble_folder(d)
    expect_identical(basename(sp$file), want, label = loc)
    expect_identical(sp$table,
                     c("14.1.2", "14.1.10", "14.3.1", rep(NA, 5L)),
                     label = loc)
    expect_identical(sp$order, 1:8, label = loc)
  }
  expect_gte(tried, 2L)
})

test_that("natural order: a long run of digits sorts by its number, with no warning", {
  x <- c("t_20261106093001.rtf", "t_9.rtf", "t_20261106092959.rtf", "t_10.rtf",
         "t_020261106093000.rtf", "t_123456789012345678901.rtf", "a.rtf")
  expect_no_warning(o <- rtfreporter:::.natural_order(x))
  expect_identical(x[o], c("a.rtf", "t_9.rtf", "t_10.rtf", "t_20261106092959.rtf",
                           "t_020261106093000.rtf", "t_20261106093001.rtf",
                           "t_123456789012345678901.rtf"))
})

# test-cov-assemble.R -- edge paths of assemble_rtf() (R/assemble_rtf.R) and
# of the assembly-spec helpers (R/assemble_spec.R): an input with no body
# after its section preamble, a structured TOC list, a bad cover, and the
# internal guards of the spec reader / writer.  All data are fictional.

# Two minimal hand-written inputs: a section with its preamble and no body.
.cov_bare_inputs <- function(dir) {
  fs <- file.path(dir, c("bare1.rtf", "bare2.rtf"))
  for (f in fs) {
    writeLines(c("{\\rtf1\\ansi", "\\sectd", "\\sbkpage\\lndscpsxn", "}"), f)
  }
  fs
}

# An rtfreporter input with a title, made through the public API.
.cov_titled_input <- function(dir, name, title) {
  f <- file.path(dir, name)
  doc <- rtf_document()
  doc <- rtf_tables(doc, data.frame(A = 1L, B = "x"),
                    titles = list(c(title)))
  generate_rtfreport(doc, f, overwrite = TRUE)
  f
}

# ---- R/assemble_rtf.R ------------------------------------------------------

test_that("assemble_rtf() puts the bookmark after the preamble of a file with no body", {
  skip_on_cran()
  d <- tempfile("cova")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  fs  <- .cov_bare_inputs(d)
  out <- file.path(d, "out.rtf")
  assemble_rtf(fs, out, toc = c("One", "Two"))
  x <- readLines(out, warn = FALSE)
  for (b in c("tfl_bare1", "tfl_bare2")) {
    bk <- which(x == sprintf("{\\*\\bkmkstart %s}{\\*\\bkmkend %s}", b, b))
    expect_length(bk, 1L)
    # straight after the section's preamble line, nothing lost after it
    expect_identical(x[bk - 1L], "\\sbkpage\\lndscpsxn")
    expect_identical(x[bk - 2L], "\\sectd")
  }
  expect_true(any(grepl("outlinelevel0 One\\par}", x, fixed = TRUE)))
  expect_true(any(grepl("outlinelevel0 Two\\par}", x, fixed = TRUE)))
  expect_identical(x[length(x)], "}")
})

test_that("assemble_rtf() refuses a cover whose meta is not a character vector", {
  skip_on_cran()
  d <- tempfile("cova")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  fs <- .cov_bare_inputs(d)
  expect_error(
    assemble_rtf(fs, file.path(d, "out.rtf"),
                 cover = list(title = "Study X", meta = list("a", "b"))),
    "`cover\\$meta` must be a character vector")
})

# The structured TOC list (.toc_heading() / .toc_entry()) is the internal form
# a table of contents becomes; assemble_rtf() also takes it as `toc`.  A table
# always names its files, so the list is built here with the internal
# constructors to reach the entries with no file, a file index or a bad file.

test_that("a structured TOC: an entry with no file takes the next free one", {
  skip_on_cran()
  d <- tempfile("cova")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  fs  <- .cov_bare_inputs(d)
  out <- file.path(d, "out.rtf")
  te <- rtfreporter:::.toc_entry
  th <- rtfreporter:::.toc_heading
  assemble_rtf(fs, out, toc = list(th("Part A"), te("Second", file = 2L),
                                   te("First")))
  x <- readLines(out, warn = FALSE)
  links <- grep("HYPERLINK", x, value = TRUE)
  expect_length(links, 2L)
  expect_match(links[1L], "\"tfl_bare2\"}{\\fldrslt Second}", fixed = TRUE)
  expect_match(links[2L], "\"tfl_bare1\"}{\\fldrslt First}", fixed = TRUE)
  expect_true(any(grepl(" Part A\\par}", x, fixed = TRUE)))
})

test_that("a structured TOC with more entries than files stops: index out of range", {
  skip_on_cran()
  d <- tempfile("cova")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  fs <- .cov_bare_inputs(d)
  te <- rtfreporter:::.toc_entry
  expect_error(
    assemble_rtf(fs, file.path(d, "out.rtf"),
                 toc = list(te("A"), te("B"), te("C"))),
    "`toc`: file index out of range")
  expect_error(
    assemble_rtf(fs, file.path(d, "out.rtf"), toc = list(te("A", file = 5L))),
    "`toc`: file index out of range")
})

test_that("a structured TOC entry whose file is neither a path nor an index stops", {
  skip_on_cran()
  d <- tempfile("cova")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  fs <- .cov_bare_inputs(d)
  te <- rtfreporter:::.toc_entry
  expect_error(
    assemble_rtf(fs, file.path(d, "out.rtf"), toc = list(te("A", file = TRUE))),
    "`toc`: `file` must be a character path or integer index")
})

test_that(".toc_entry() refuses an NA label", {
  skip_on_cran()
  # Internal: a table row with no label reads the file's title, which is
  # never NA, so this guard is reached only directly.
  expect_error(rtfreporter:::.toc_entry(NA_character_, file = 1L),
               "A TOC entry's `label` must be a single string, not NA")
  expect_error(rtfreporter:::.toc_entry(c("a", "b"), file = 1L),
               "A TOC entry's `label` must be a single string, not NA")
})

test_that(".build_toc_section() writes page 1 when no start pages are known", {
  skip_on_cran()
  # Internal: assemble_rtf() always computes the start pages, so the
  # fallback is reached only directly.
  entries <- list(list(label = "Only entry", level = 1L, file_idx = 1L,
                       type = "entry"))
  x <- rtfreporter:::.build_toc_section(entries, bookmarks = "bm_1",
                                        toc_title = "Contents",
                                        entry_pages = NULL)
  ln <- grep("PAGEREF bm_1", x, value = TRUE, fixed = TRUE)
  expect_length(ln, 1L)
  expect_match(ln, "{\\fldrslt 1}}\\par}", fixed = TRUE)
  # an entry whose file has no start page is on page 1 as well
  x2 <- rtfreporter:::.build_toc_section(entries, bookmarks = "bm_1",
                                         toc_title = "Contents",
                                         entry_pages = NA_integer_)
  expect_match(grep("PAGEREF bm_1", x2, value = TRUE, fixed = TRUE),
               "{\\fldrslt 1}}\\par}", fixed = TRUE)
})

# ---- R/assemble_spec.R -----------------------------------------------------

test_that(".toc_label_from_title() reads the label from the file's title", {
  skip_on_cran()
  # Internal helper with no caller today (the readers call
  # .rtf_table_label() directly): unit-tested here.
  d <- tempfile("cova")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- .cov_titled_input(d, "t1.rtf", "Table 1 Demographics (\u00b5g)")
  expect_identical(rtfreporter:::.toc_label_from_title(f),
                   "Table 1 Demographics (\u00b5g)")
  b <- .cov_bare_inputs(d)[1L]
  expect_identical(rtfreporter:::.toc_label_from_title(b), "bare1")
})

test_that(".write_spec() refuses a path that is not .xlsx or .csv", {
  skip_on_cran()
  # Internal: assemble_folder() checks `spec_file` first, so this is reached
  # only directly.
  spec <- data.frame(file = "a.rtf", label = "A")
  p <- tempfile(fileext = ".txt")
  on.exit(unlink(p), add = TRUE)
  expect_error(rtfreporter:::.write_spec(spec, p),
               "`spec_file` must end in .xlsx or .csv")
  expect_false(file.exists(p))
})

test_that(".assemble_from_spec() refuses a spec with no file column", {
  skip_on_cran()
  # Internal: assemble_folder() always hands it a spec it built itself.
  expect_error(
    rtfreporter:::.assemble_from_spec(data.frame(label = "A"),
                                      tempfile(fileext = ".rtf")),
    "`spec` must be an assembly-spec data.frame")
  expect_error(
    rtfreporter:::.assemble_from_spec(list(file = "a.rtf"),
                                      tempfile(fileext = ".rtf")),
    "`spec` must be an assembly-spec data.frame")
})

test_that(".assemble_from_spec() names the files of the spec that are missing", {
  skip_on_cran()
  # Internal: assemble_folder() builds the spec from files it just listed.
  d <- tempfile("cova")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  spec <- data.frame(file = file.path(d, c("gone1.rtf", "gone2.rtf")),
                     order = 2:1, stringsAsFactors = FALSE)
  expect_error(
    rtfreporter:::.assemble_from_spec(spec, file.path(d, "out.rtf")),
    "Spec references missing file\\(s\\): .*gone2\\.rtf, .*gone1\\.rtf")
})

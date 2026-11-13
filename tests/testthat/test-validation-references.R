# The reference RTF files -- the stored outputs the end-to-end tests compare
# byte for byte, and the examples in inst/rtf-examples -- are regression
# references, not functional verification (Quality Plan, section 3a).  Each
# was read once against the code that made it, control word by control word,
# and the reading is recorded in inst/validation/reference_review.csv with the
# file's md5.  A reference written again must be read again: its md5 no
# longer matches, and this test fails until the reading is recorded.

.ref_review <- function() {
  p <- system.file("validation", "reference_review.csv", package = "rtfreporter")
  if (!nzchar(p)) p <- test_path("..", "..", "inst", "validation", "reference_review.csv")
  utils::read.csv(p, colClasses = "character", na.strings = "", stringsAsFactors = FALSE)
}

# md5 of the file's text with the line ends made LF, so a checkout that
# writes CRLF reads the same as one that writes LF
.ref_md5 <- function(f) {
  b <- readBin(f, "raw", file.size(f))
  b <- b[b != as.raw(13L)]
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)
  writeBin(b, tmp)
  unname(tools::md5sum(tmp))
}

.ref_files <- function() {
  fx <- test_path("fixtures")
  ex <- system.file("rtf-examples", package = "rtfreporter")
  if (!nzchar(ex)) ex <- test_path("..", "..", "inst", "rtf-examples")
  rel <- function(dir, prefix) {
    f <- list.files(dir, "[.]rtf$", recursive = TRUE)
    stats::setNames(file.path(dir, f), paste0(prefix, f))
  }
  c(rel(fx, "tests/testthat/fixtures/"), rel(ex, "inst/rtf-examples/"))
}

test_that("every reference RTF has been read: reference_review.csv holds its md5", {
  rv <- .ref_review()
  expect_named(rv, c("file", "md5", "made_by", "verdict", "checked", "problems", "issue", "read"))
  expect_true(all(rv$verdict %in% c("correct", "wrong", "pending-human")))
  # a wrong reference names the issue that fixes it
  expect(all(!is.na(rv$issue[rv$verdict == "wrong"])),
         "A reference read as wrong names its issue.")
  files <- .ref_files()
  skip_if(!length(files), "no reference files here")
  miss <- setdiff(names(files), rv$file)
  expect(!length(miss), paste0("Reference RTFs never read; read each against the code that ",
                               "made it and add a row to reference_review.csv: ",
                               paste(miss, collapse = " ")))
  have <- intersect(names(files), rv$file)
  now <- vapply(files[have], .ref_md5, "")
  changed <- have[now != rv$md5[match(have, rv$file)]]
  expect(!length(changed), paste0("Reference RTFs written again since they were read; read ",
                                  "them again and update their rows (md5 = the file with LF ",
                                  "line ends): ", paste(changed, collapse = " ")))
})

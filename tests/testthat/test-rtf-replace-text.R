# rtf_replace_text(): post-processing find/replace on a rendered RTF file.

# Helper: write a small text file and return its path.
.tmp_rtf <- function(text) {
  p <- tempfile(fileext = ".rtf")
  writeBin(charToRaw(text), p)
  p
}

.read_txt <- function(p) {
  rawToChar(readBin(p, what = "raw", n = file.info(p)$size))
}

test_that("fixed single replacement works in place and returns the path", {
  p <- .tmp_rtf("Hello DRAFT world")
  on.exit(unlink(c(p, paste0(p, ".bak"))), add = TRUE)
  out <- rtf_replace_text(p, "DRAFT", "FINAL")
  expect_identical(.read_txt(p), "Hello FINAL world")
  expect_identical(normalizePath(out), normalizePath(p))
})

test_that("in-place edit writes a .bak by default and can be disabled", {
  p <- .tmp_rtf("aaa")
  on.exit(unlink(c(p, paste0(p, ".bak"))), add = TRUE)
  rtf_replace_text(p, "aaa", "bbb")
  expect_true(file.exists(paste0(p, ".bak")))
  expect_identical(.read_txt(paste0(p, ".bak")), "aaa")

  p2 <- .tmp_rtf("ccc")
  on.exit(unlink(c(p2, paste0(p2, ".bak"))), add = TRUE)
  rtf_replace_text(p2, "ccc", "ddd", backup = FALSE)
  expect_false(file.exists(paste0(p2, ".bak")))
})

test_that("output_file leaves the input untouched and writes elsewhere", {
  p   <- .tmp_rtf("keep ME")
  out <- tempfile(fileext = ".rtf")
  on.exit(unlink(c(p, out)), add = TRUE)
  rtf_replace_text(p, "ME", "YOU", output_file = out)
  expect_identical(.read_txt(p),   "keep ME")   # unchanged
  expect_identical(.read_txt(out), "keep YOU")
  expect_false(file.exists(paste0(p, ".bak")))  # no backup when output given
})

test_that("vectorised targets with recycled replacement", {
  p <- .tmp_rtf("X and Y and Z")
  on.exit(unlink(c(p, paste0(p, ".bak"))), add = TRUE)
  rtf_replace_text(p, c("X", "Y", "Z"), "_")
  expect_identical(.read_txt(p), "_ and _ and _")
})

test_that("vectorised targets with parallel replacements applied in order", {
  p <- .tmp_rtf("DRAFT vX.Y")
  on.exit(unlink(c(p, paste0(p, ".bak"))), add = TRUE)
  rtf_replace_text(p, c("DRAFT", "vX.Y"), c("FINAL", "v1.0"))
  expect_identical(.read_txt(p), "FINAL v1.0")
})

test_that("fixed mode treats regex metacharacters literally", {
  p <- .tmp_rtf("a.b a.b axb")
  on.exit(unlink(c(p, paste0(p, ".bak"))), add = TRUE)
  rtf_replace_text(p, "a.b", "Z")          # fixed: only literal 'a.b'
  expect_identical(.read_txt(p), "Z Z axb")
})

test_that("use_regex = TRUE enables pattern matching", {
  p <- .tmp_rtf("a1b a2b a3b")
  on.exit(unlink(c(p, paste0(p, ".bak"))), add = TRUE)
  rtf_replace_text(p, "a[0-9]b", "Z", use_regex = TRUE)
  expect_identical(.read_txt(p), "Z Z Z")
})

test_that("case_insensitive matches regardless of case, escaping metachars", {
  p <- .tmp_rtf("Draft DRAFT draft a.b")
  on.exit(unlink(c(p, paste0(p, ".bak"))), add = TRUE)
  rtf_replace_text(p, "draft", "X", case_insensitive = TRUE)
  expect_identical(.read_txt(p), "X X X a.b")

  # fixed + case_insensitive must keep '.' literal
  p2 <- .tmp_rtf("A.B axb")
  on.exit(unlink(c(p2, paste0(p2, ".bak"))), add = TRUE)
  rtf_replace_text(p2, "a.b", "Z", case_insensitive = TRUE)
  expect_identical(.read_txt(p2), "Z axb")
})

test_that("FT-DC6803 [FR-DC387]: a backup that cannot be written stops before the file is overwritten", {
  p <- .tmp_rtf("aaa")
  bak <- paste0(p, ".bak")
  on.exit(unlink(c(p, bak), recursive = TRUE), add = TRUE)
  # the backup path is a folder
  dir.create(bak)
  expect_error(rtf_replace_text(p, "aaa", "bbb"),
               paste0("rtf_replace_text(): the backup path is a folder: ", bak,
                      "; the file is left as it was."), fixed = TRUE)
  expect_identical(.read_txt(p), "aaa")
  # backup = FALSE needs no backup
  rtf_replace_text(p, "aaa", "bbb", backup = FALSE)
  expect_identical(.read_txt(p), "bbb")
  unlink(bak, recursive = TRUE)
  # the copy fails
  local_mocked_bindings(.copy_backup = function(from, to) FALSE)
  expect_error(rtf_replace_text(p, "bbb", "ccc"),
               paste0("rtf_replace_text(): could not write the backup ", bak,
                      "; the file is left as it was."), fixed = TRUE)
  expect_identical(.read_txt(p), "bbb")
})

test_that("FT-DC6805 [FR-DC389]: rtf_replace_text(case_insensitive = TRUE) keeps a fixed replacement's backslashes; a regex replacement keeps its back-references", {
  p <- .tmp_rtf("DRAFT here")
  on.exit(unlink(c(p, paste0(p, ".bak"))), add = TRUE)
  rtf_replace_text(p, "draft", "FINAL\\line \\1 $1", case_insensitive = TRUE)
  expect_identical(.read_txt(p), "FINAL\\line \\1 $1 here")

  p2 <- .tmp_rtf("DRAFT here")
  on.exit(unlink(c(p2, paste0(p2, ".bak"))), add = TRUE)
  rtf_replace_text(p2, "(dr)aft", "\\1X", use_regex = TRUE,
                   case_insensitive = TRUE)
  expect_identical(.read_txt(p2), "DRX here")
})

test_that("argument validation errors are informative", {
  p <- .tmp_rtf("x")
  on.exit(unlink(p), add = TRUE)
  expect_error(rtf_replace_text("no-such-file.rtf", "a", "b"), "not found")
  expect_error(rtf_replace_text(p, character(0), "b"), "empty")
  expect_error(rtf_replace_text(p, c("a", "b"), c("1", "2", "3")),
               "same length")
  expect_error(rtf_replace_text(p, 1, "b"), "character vectors")
})

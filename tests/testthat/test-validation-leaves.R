# Every active leaf requirement is checked by a test, or is a deviation with
# its risk stated -- what validation/trace.R reports, made a test so a gap
# fails here and not only in the report.  A test names its FRs in
# traceability_matrix.csv or in its own name: "FT-PL049 [FR-PL122]: ...".

.leaf_read <- function(f) {
  p <- system.file("validation", f, package = "rtfreporter")
  if (!nzchar(p)) p <- test_path("..", "..", "inst", "validation", f)
  utils::read.csv(p, colClasses = "character", na.strings = "", stringsAsFactors = FALSE)
}

# The names of the test_that() blocks of every test file
.leaf_test_names <- function() {
  files <- list.files(test_path(), "^test-.*[.]R$", full.names = TRUE)
  out <- lapply(files, function(f) {
    ex <- parse(f, keep.source = FALSE, encoding = "UTF-8")
    nm <- lapply(ex, function(e) {
      if (is.call(e) && identical(e[[1L]], as.name("test_that")) && is.character(e[[2L]])) e[[2L]]
    })
    nm <- unlist(nm)
    if (length(nm)) data.frame(file = basename(f), test = nm, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

.leaf_head <- "^(FT-[A-Z]{2}[0-9]{3,4}(-[0-9]+)?) [[]([^]]*)[]]:"

test_that("every test traceability_matrix.csv names is in its file", {
  skip_on_cran()
  map <- .leaf_read("traceability_matrix.csv")
  tb <- .leaf_test_names()
  gone <- !paste(map$file, map$test, sep = "\r") %in% paste(tb$file, tb$test, sep = "\r")
  expect(!any(gone), paste0("traceability_matrix.csv names tests that are not there (renamed?):\n",
                            paste(map$file[gone], map$test[gone], sep = ": ", collapse = "\n")))
})

test_that("an FT id is given once: by a test's own name or by traceability_matrix.csv, not both", {
  skip_on_cran()
  map <- .leaf_read("traceability_matrix.csv")
  tb <- .leaf_test_names()
  named <- grep(.leaf_head, tb$test, value = TRUE)
  ft <- c(map$ft, sub(paste0(.leaf_head, ".*$"), "\\1", named))
  twice <- unique(ft[duplicated(ft)])
  expect(!length(twice), paste0("FT ids given twice (a test named \"FT-xx [FR-..]:\" ",
                                "needs no traceability_matrix.csv row): ", paste(twice, collapse = " ")))
})

test_that("a requirement id is given once in requirements.csv", {
  skip_on_cran()
  # two open PRs can each pick the next free id; the second one's CI runs on
  # the merge with the first, so a duplicate fails here before it is merged
  fr <- .leaf_read("requirements.csv")
  twice <- unique(fr$id[duplicated(fr$id)])
  expect(!length(twice), paste0("requirements.csv gives these ids twice (another PR took ",
                                "the id: pick a free one): ", paste(twice, collapse = " ")))
})

test_that("every FR a test names, in its own name or in traceability_matrix.csv, is in requirements.csv", {
  skip_on_cran()
  fr <- .leaf_read("requirements.csv")
  map <- .leaf_read("traceability_matrix.csv")
  tb <- .leaf_test_names()
  named <- grep(.leaf_head, tb$test, value = TRUE)
  frs <- c(unlist(strsplit(map$fr, " ", fixed = TRUE)),
           unlist(strsplit(sub(paste0(.leaf_head, ".*$"), "\\3", named), " ", fixed = TRUE)))
  unknown <- unique(setdiff(frs, fr$id))
  expect(!length(unknown), paste0("Tests name FR ids that are not in requirements.csv ",
                                  "(a typo, or the FR was renumbered): ",
                                  paste(unknown, collapse = " ")))
})

test_that("every active leaf FR has a test or a deviation with its risk", {
  skip_on_cran()
  fr <- .leaf_read("requirements.csv")
  map <- .leaf_read("traceability_matrix.csv")
  dev <- .leaf_read("deviations.csv")
  tb <- .leaf_test_names()
  leaf <- fr$id[!fr$id %in% fr$parent & fr$status == "active"]
  named <- grep(.leaf_head, tb$test, value = TRUE)
  tested <- c(unlist(strsplit(map$fr, " ", fixed = TRUE)),
              unlist(strsplit(sub(paste0(.leaf_head, ".*$"), "\\3", named), " ", fixed = TRUE)))
  assessed <- unlist(strsplit(dev$ids[!is.na(dev$risk) & nzchar(dev$risk)], " ", fixed = TRUE))
  miss <- setdiff(leaf, c(tested, assessed))
  expect(!length(miss), paste0("Active leaves with no test and no assessed deviation; ",
                               "add a test (traceability_matrix.csv or an \"FT-xx [FR-..]:\" name) or a ",
                               "deviations.csv row:\n", paste(miss, collapse = " ")))
  # a deviation names only leaves that still have no test
  stale <- intersect(unlist(strsplit(dev$ids, " ", fixed = TRUE)), tested)
  expect(!length(stale), paste0("These deviations now have a test; take them off ",
                                "deviations.csv: ", paste(stale, collapse = " ")))
})

test_that("every mapping of a test to an active FR is checked in traceability_review.csv", {
  skip_on_cran()
  # A mapping is checked when it is made: someone read the FR and the test and
  # recorded that the test's expectations assert what the FR states -- all of
  # it ("asserts") or a part of it ("part"), the FR's tests together asserting
  # all of it (fr_covered = "yes") -- with the expectation lines as evidence.
  fr <- .leaf_read("requirements.csv")
  map <- .leaf_read("traceability_matrix.csv")
  rv <- .leaf_read("traceability_review.csv")
  tb <- .leaf_test_names()
  expect_named(rv, c("ft", "fr", "verdict", "evidence", "fr_covered"))
  named <- grep(.leaf_head, tb$test, value = TRUE)
  ft <- c(map$ft, sub(paste0(.leaf_head, ".*$"), "\\1", named))
  frs <- c(map$fr, sub(paste0(.leaf_head, ".*$"), "\\3", named))
  pairs <- unlist(Map(function(f, r) paste(f, strsplit(r, " ", fixed = TRUE)[[1L]]), ft, frs),
                  use.names = FALSE)
  active <- fr$id[fr$status == "active"]
  pairs <- pairs[sub("^\\S+ ", "", pairs) %in% active]
  have <- paste(rv$ft, rv$fr)
  miss <- setdiff(pairs, have)
  expect(!length(miss), paste0(
    "Mappings with no check: read the FR and the test, and add a row to ",
    "inst/validation/traceability_review.csv (the expectation lines that assert it):\n",
    paste(utils::head(miss, 30), collapse = "\n")))
  stale <- setdiff(have, pairs)
  expect(!length(stale), paste0("traceability_review.csv rows for mappings that are gone: ",
                                paste(utils::head(stale, 30), collapse = " ")))
  expect(all(rv$verdict %in% c("asserts", "part")),
         "A verdict is \"asserts\" or \"part\"; a test that asserts none of an FR is not mapped to it.")
  expect(all(!is.na(rv$evidence) & nzchar(rv$evidence)), "Every check names its evidence.")
  expect(all(rv$fr_covered %in% "yes"), paste0(
    "An FR whose tests do not assert all of it together needs a test: ",
    paste(unique(rv$fr[!rv$fr_covered %in% "yes"]), collapse = " ")))
})

test_that("a test title that starts with FT- parses as an FT id with its FRs", {
  skip_on_cran()
  tb <- .leaf_test_names()
  ft_like <- tb[startsWith(tb$test, "FT-"), , drop = FALSE]
  bad <- ft_like[!grepl(.leaf_head, ft_like$test), , drop = FALSE]
  expect(!nrow(bad), paste0(
    "These test titles start with FT- but do not read as an FT id; the form is ",
    "\"FT-XX<nnn> [FR-XX<nnn> ...]: ...\" (two capital letters, three or four ",
    "digits, the FRs in square brackets, a colon):\n",
    paste(bad$file, bad$test, sep = ": ", collapse = "\n")))
})

test_that("an FT or FR id of four digits is read, as one of three", {
  skip_on_cran()
  for (n in c("999", "1000")) {
    title <- paste0("FT-TB", n, " [FR-TB", n, "]: x")
    expect_match(title, .leaf_head)
    expect_identical(sub(paste0(.leaf_head, ".*$"), "\\1", title), paste0("FT-TB", n))
    expect_identical(sub(paste0(.leaf_head, ".*$"), "\\3", title), paste0("FR-TB", n))
  }
  # a title that is not of the form is not taken as an FT
  expect_false(grepl(.leaf_head, "FT-TB10000 [FR-TB1]: x"))
  expect_false(grepl(.leaf_head, "FT-TB1000: no FR list"))
})

test_that("an FT id may carry an issue block suffix: FT-PL1137-2 [FR-PL005]", {
  skip_on_cran()
  title <- "FT-PL1137-2 [FR-PL005]: a new test takes its issue's number"
  expect_match(title, .leaf_head)
  expect_identical(sub(paste0(.leaf_head, ".*$"), "\\1", title), "FT-PL1137-2")
  expect_identical(sub(paste0(.leaf_head, ".*$"), "\\3", title), "FR-PL005")
  # an id already in use is read as before
  old <- "FT-PL049 [FR-PL122]: x"
  expect_match(old, .leaf_head)
  expect_identical(sub(paste0(.leaf_head, ".*$"), "\\1", old), "FT-PL049")
  # a dash with no number, or two suffixes, is not an id
  expect_false(grepl(.leaf_head, "FT-PL1137- [FR-PL005]: x"))
  expect_false(grepl(.leaf_head, "FT-PL1137-1-2 [FR-PL005]: x"))
})

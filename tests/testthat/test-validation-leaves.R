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

.leaf_head <- "^(FT-[A-Z]{2}[0-9]{3,4}) [[]([^]]*)[]]:"

test_that("every test traceability_matrix.csv names is in its file", {
  map <- .leaf_read("traceability_matrix.csv")
  tb <- .leaf_test_names()
  gone <- !paste(map$file, map$test, sep = "\r") %in% paste(tb$file, tb$test, sep = "\r")
  expect(!any(gone), paste0("traceability_matrix.csv names tests that are not there (renamed?):\n",
                            paste(map$file[gone], map$test[gone], sep = ": ", collapse = "\n")))
})

test_that("an FT id is given once: by a test's own name or by traceability_matrix.csv, not both", {
  map <- .leaf_read("traceability_matrix.csv")
  tb <- .leaf_test_names()
  named <- grep(.leaf_head, tb$test, value = TRUE)
  ft <- c(map$ft, sub(paste0(.leaf_head, ".*$"), "\\1", named))
  twice <- unique(ft[duplicated(ft)])
  expect(!length(twice), paste0("FT ids given twice (a test named \"FT-xx [FR-..]:\" ",
                                "needs no traceability_matrix.csv row): ", paste(twice, collapse = " ")))
})

test_that("every active leaf FR has a test or a deviation with its risk", {
  fr <- .leaf_read("requirements.csv")
  map <- .leaf_read("traceability_matrix.csv")
  dev <- .leaf_read("deviations.csv")
  tb <- .leaf_test_names()
  leaf <- fr$id[!fr$id %in% fr$parent & fr$status == "active"]
  named <- grep(.leaf_head, tb$test, value = TRUE)
  tested <- c(unlist(strsplit(map$fr, " ", fixed = TRUE)),
              unlist(strsplit(sub(paste0(.leaf_head, ".*$"), "\\2", named), " ", fixed = TRUE)))
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

test_that("a test title that starts with FT- parses as an FT id with its FRs", {
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
  for (n in c("999", "1000")) {
    title <- paste0("FT-TB", n, " [FR-TB", n, "]: x")
    expect_match(title, .leaf_head)
    expect_identical(sub(paste0(.leaf_head, ".*$"), "\\1", title), paste0("FT-TB", n))
    expect_identical(sub(paste0(.leaf_head, ".*$"), "\\2", title), paste0("FR-TB", n))
  }
  # a title that is not of the form is not taken as an FT
  expect_false(grepl(.leaf_head, "FT-TB10000 [FR-TB1]: x"))
  expect_false(grepl(.leaf_head, "FT-TB1000: no FR list"))
})

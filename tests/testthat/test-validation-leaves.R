# Every active leaf requirement is checked by a test, or is a deviation with
# its risk stated -- what validation/trace.R reports, made a test so a gap
# fails here and not only in the report.  A test names its FRs in
# test_map.csv or in its own name: "FT-PL049 [FR-PL122]: ...".

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

.leaf_head <- "^(FT-[A-Z]{2}[0-9]{3}) [[]([^]]*)[]]:"

test_that("every test test_map.csv names is in its file", {
  map <- .leaf_read("test_map.csv")
  tb <- .leaf_test_names()
  gone <- !paste(map$file, map$test, sep = "\r") %in% paste(tb$file, tb$test, sep = "\r")
  expect(!any(gone), paste0("test_map.csv names tests that are not there (renamed?):\n",
                            paste(map$file[gone], map$test[gone], sep = ": ", collapse = "\n")))
})

test_that("every active leaf FR has a test or a deviation with its risk", {
  fr <- .leaf_read("requirements.csv")
  map <- .leaf_read("test_map.csv")
  dev <- .leaf_read("deviations.csv")
  tb <- .leaf_test_names()
  leaf <- fr$id[!fr$id %in% fr$parent & fr$status == "active"]
  named <- grep(.leaf_head, tb$test, value = TRUE)
  tested <- c(unlist(strsplit(map$fr, " ", fixed = TRUE)),
              unlist(strsplit(sub(paste0(.leaf_head, ".*$"), "\\2", named), " ", fixed = TRUE)))
  assessed <- unlist(strsplit(dev$ids[!is.na(dev$risk) & nzchar(dev$risk)], " ", fixed = TRUE))
  miss <- setdiff(leaf, c(tested, assessed))
  expect(!length(miss), paste0("Active leaves with no test and no assessed deviation; ",
                               "add a test (test_map.csv or an \"FT-xx [FR-..]:\" name) or a ",
                               "deviations.csv row:\n", paste(miss, collapse = " ")))
  # a deviation names only leaves that still have no test
  stale <- intersect(unlist(strsplit(dev$ids, " ", fixed = TRUE)), tested)
  expect(!length(stale), paste0("These deviations now have a test; take them off ",
                                "deviations.csv: ", paste(stale, collapse = " ")))
})

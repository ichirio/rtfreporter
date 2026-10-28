# The validation kit's own consistency: the Functional Requirements (FR)
# and the map of the Functional Tests (FT) to them

.val_read <- function(f) {
  p <- system.file("validation", f, package = "rtfreporter")
  if (!nzchar(p)) p <- test_path("..", "..", "inst", "validation", f)
  utils::read.csv(p, colClasses = "character", na.strings = "", stringsAsFactors = FALSE)
}

test_that("the requirements: unique ids, their area, a statement, functions that exist", {
  fr <- .val_read("requirements.csv")
  expect_named(fr, c("id", "parent", "area", "kind", "statement", "functions", "since", "status",
                     "notes"))
  expect_false(anyDuplicated(fr$id) > 0L)
  expect_true(all(grepl("^FR-[A-Z]{2}[0-9]{3}$", fr$id)))
  # a hierarchy: every parent is an FR; an active leaf has a kind
  expect_true(all(is.na(fr$parent) | fr$parent %in% fr$id))
  leaf <- !fr$id %in% fr$parent & fr$status == "active"
  expect_true(all(fr$kind[leaf] %in% c("behaviour", "argument", "data", "guard", "interaction")))
  expect_identical(sub("^FR-([A-Z]+).*$", "\\1", fr$id), fr$area)
  expect_false(any(is.na(fr$statement) | !nzchar(fr$statement)))
  expect_true(all(fr$status %in% c("active", "draft", "superseded", "removed")))
  fns <- unique(stats::na.omit(unlist(strsplit(fr$functions, ";", fixed = TRUE))))
  expect_true(all(fns %in% getNamespaceExports("rtfreporter")), info = paste(
    setdiff(fns, getNamespaceExports("rtfreporter")), collapse = ", "))
})

test_that("every plan verb is in a requirement", {
  fr <- .val_read("requirements.csv")
  fns <- unique(stats::na.omit(unlist(strsplit(fr$functions[fr$status %in% c("active", "draft")],
                                                ";", fixed = TRUE))))
  plan <- grep("^(table_plan|plan_)", getNamespaceExports("rtfreporter"), value = TRUE)
  expect_identical(setdiff(plan, fns), character())
})

test_that("the map of the tests: unique FT ids, each to known requirements", {
  fr <- .val_read("requirements.csv")
  map <- .val_read("test_map.csv")
  expect_named(map, c("file", "test", "ft", "fr"))
  expect_false(anyDuplicated(map$ft) > 0L)
  expect_true(all(grepl("^FT-[A-Z]{2}[0-9]{3}$", map$ft)))
  ids <- unique(unlist(strsplit(map$fr, " ", fixed = TRUE)))
  expect_true(all(ids %in% fr$id))
  # to the leaves only
  expect_false(any(ids %in% fr$parent))
  # every mapped file is a test file there
  there <- list.files(test_path(), "^test-.*[.]R$")
  expect_true(all(map$file %in% there))
})

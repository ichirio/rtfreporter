# The user's validation kit: rtfreporter_validation_kit() and its templates

.kit_dir <- function() tempfile("vkit")

.kit_ids <- function(x) unique(trimws(unlist(strsplit(x[!is.na(x)], ";", fixed = TRUE))))

test_that("rtfreporter_validation_kit() copies the templates and the package's FRs", {
  d <- .kit_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  files <- rtfreporter_validation_kit(d)
  expect_true(all(file.exists(files)))
  expect_setequal(basename(files), c(
    "build_user_matrix.R", "uat_cases.csv", "uat_cases.xlsx", "urs.csv", "urs.md",
    "validation_plan.md", "validation_summary_report.md", "quality-plan.md",
    "requirements.csv"))
  expect_setequal(list.files(d), basename(files))
})

test_that("rtfreporter_validation_kit() refuses a folder with files unless overwrite = TRUE", {
  d <- .kit_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  dir.create(d)
  writeLines("mine", file.path(d, "notes.txt"))
  expect_error(rtfreporter_validation_kit(d), "is not empty")
  files <- rtfreporter_validation_kit(d, overwrite = TRUE)
  expect_true(all(file.exists(files)))
  expect_true(file.exists(file.path(d, "notes.txt")))
  expect_error(rtfreporter_validation_kit(c("a", "b")), "one folder path")
  expect_error(rtfreporter_validation_kit(d, overwrite = NA), "TRUE or FALSE")
})

test_that("every FR id the URS and the UAT cases name is in requirements.csv", {
  d <- .kit_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  rtfreporter_validation_kit(d)
  rd <- function(f) utils::read.csv(file.path(d, f), colClasses = "character",
                                    na.strings = "", stringsAsFactors = FALSE)
  fr <- rd("requirements.csv")
  urs <- rd("urs.csv")
  uat <- rd("uat_cases.csv")
  expect_named(urs, c("id", "statement", "priority", "fr_ids"))
  expect_named(uat, c("id", "ur_ids", "fr_ids", "steps", "expected", "actual", "result",
                      "tester", "date"))
  expect_true(all(grepl("^UR-[0-9]{3}$", urs$id)))
  expect_true(all(grepl("^UAT-[0-9]{3}$", uat$id)))
  expect_false(anyDuplicated(urs$id) > 0L)
  expect_false(anyDuplicated(uat$id) > 0L)
  ids <- c(.kit_ids(urs$fr_ids), .kit_ids(uat$fr_ids))
  expect_identical(setdiff(ids, fr$id), character())
  expect_identical(setdiff(.kit_ids(uat$ur_ids), urs$id), character())
  # every UR has a case; the results are left for the user
  expect_identical(setdiff(urs$id, .kit_ids(uat$ur_ids)), character())
  expect_true(all(is.na(uat$actual) & is.na(uat$result) & is.na(uat$tester) & is.na(uat$date)))
})

test_that("build_user_matrix.R writes the URS -> FR -> UAT matrix from the copied kit", {
  skip_if_not_installed("writexl")
  d <- .kit_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  rtfreporter_validation_kit(d)
  e <- new.env()
  e$kit_dir <- d
  suppressMessages(sys.source(file.path(d, "build_user_matrix.R"), envir = e))
  expect_true(file.exists(file.path(d, "user_traceability.csv")))
  expect_true(file.exists(file.path(d, "user_traceability.xlsx")))
  m <- utils::read.csv(file.path(d, "user_traceability.csv"), colClasses = "character",
                       na.strings = "", stringsAsFactors = FALSE)
  urs <- utils::read.csv(file.path(d, "urs.csv"), colClasses = "character",
                         stringsAsFactors = FALSE)
  expect_true(all(c("ur_id", "fr_id", "fr_statement", "uat_ids", "uat_results") %in% names(m)))
  expect_setequal(unique(m$ur_id), urs$id)
  expect_false(any(m$fr_status %in% "not in requirements.csv"))
  expect_true(all(e$by_ur$status == "not run"))
})

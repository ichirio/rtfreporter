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

test_that("requirements.csv and traceability_matrix.csv are strict CSV, every row whole", {
  # what any CSV reader takes them as (RFC 4180): a field quoted or not,
  # a quote inside a quoted field doubled -- a backslash does not escape
  # it, which R's reader forgives and Python's does not; every record as
  # many fields as the header; a status and a version where they belong
  field <- "(\"(?:[^\"]|\"\")*+\"|[^,\"]*+)"
  record <- paste0("^", field, "(?:,", field, ")*+$")
  for (f in c("requirements.csv", "traceability_matrix.csv")) {
    path <- system.file("validation", f, package = "rtfreporter")
    lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
    bad <- which(!grepl(record, lines, perl = TRUE))
    expect_identical(bad, integer(0), label = paste(f, "lines that are not strict CSV"))
    n <- utils::count.fields(path, sep = ",", quote = "\"")
    expect_true(all(n == n[1L]), label = paste(f, "has the same number of fields in every record"))
  }
  req <- utils::read.csv(system.file("validation", "requirements.csv", package = "rtfreporter"),
                         colClasses = "character")
  expect_true(all(req$status %in% c("active", "removed")))
  expect_true(all(grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", req$since)))
  expect_true(all(grepl("^FR-[A-Z]{2}[0-9]{3,4}$", req$id)))
})

test_that("every FR id the URS and the UAT cases name is an active FR of requirements.csv", {
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
  # a removed (or otherwise inactive) FR is no longer a requirement of the
  # package: the sample must name the active FR that now states it
  expect_identical(setdiff(ids, fr$id[fr$status == "active"]), character())
  # every UR names its FRs; a UAT case checks FRs of the URs it names
  expect_false(anyNA(urs$fr_ids))
  for (i in seq_len(nrow(uat))) {
    ur_fr <- .kit_ids(urs$fr_ids[urs$id %in% .kit_ids(uat$ur_ids[i])])
    expect_identical(setdiff(.kit_ids(uat$fr_ids[i]), ur_fr), character(),
                     label = paste(uat$id[i], "FRs outside its URs"))
  }
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
  expect_true(file.exists(file.path(d, "user_traceability_matrix.csv")))
  expect_true(file.exists(file.path(d, "user_traceability_matrix.xlsx")))
  m <- utils::read.csv(file.path(d, "user_traceability_matrix.csv"), colClasses = "character",
                       na.strings = "", stringsAsFactors = FALSE)
  urs <- utils::read.csv(file.path(d, "urs.csv"), colClasses = "character",
                         stringsAsFactors = FALSE)
  expect_true(all(c("ur_id", "fr_id", "fr_statement", "uat_ids", "uat_results") %in% names(m)))
  expect_setequal(unique(m$ur_id), urs$id)
  expect_true(all(m$fr_status == "active"))
  expect_true(all(e$by_ur$status == "not run"))
})

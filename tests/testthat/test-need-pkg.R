# Optional-package guards (#308).
#
# DESCRIPTION declares a minimum for most optional packages, but the guards
# used to ask `requireNamespace()` only, so an installation below the minimum
# sailed past and failed later inside the conversion.

mins <- function() rtfreporter:::.dep_min_versions()
need <- function(...) rtfreporter:::.need_pkg(...)

# ── reading the declared minimums ──────────────────────────────────────────

test_that("the minimums come from our own DESCRIPTION", {
  m <- mins()
  expect_true(is.character(m))
  expect_true(length(m) > 0L)
  expect_true(all(nzchar(names(m))))
})

test_that("a constrained dependency reports its version", {
  m <- mins()
  expect_true("gt" %in% names(m))
  expect_false(is.na(m[["gt"]]))
  expect_true(package_version(m[["gt"]]) >= package_version("0.9.0"))
})

test_that("an unconstrained dependency reports NA", {
  m <- mins()
  expect_true("tidyr" %in% names(m))
  expect_true(is.na(m[["tidyr"]]))
})

test_that("no name carries leftover version syntax", {
  m <- mins()
  expect_false(any(grepl("[()><=[:space:]]", names(m))))
})

test_that("every declared version parses", {
  m <- mins()
  v <- m[!is.na(m)]
  expect_silent(lapply(v, package_version))
})

# ── the guard ──────────────────────────────────────────────────────────────

test_that("a missing package is an error naming it and how to install it", {
  err <- tryCatch(need("definitelyNotAPackage", "Doing the thing"),
                  error = function(e) conditionMessage(e))
  expect_match(err, "Doing the thing requires the `definitelyNotAPackage`")
  expect_match(err, 'install.packages\\("definitelyNotAPackage"\\)')
})

test_that("an installed package at or above the minimum passes quietly", {
  skip_if_not_installed("gt")
  rtfreporter:::.need_pkg_reset()
  expect_silent(need("gt", "Reading from a gt_tbl"))
})

test_that("an unconstrained installed package passes quietly", {
  skip_if_not_installed("tidyr")
  rtfreporter:::.need_pkg_reset()
  expect_silent(need("tidyr", "Doing the thing"))
})

test_that("a package below the minimum warns but does not stop", {
  skip_if_not_installed("gt")
  rtfreporter:::.need_pkg_reset()
  # pretend the constraint is far above whatever is installed
  local_mocked_bindings(
    .dep_min_versions = function() c(gt = "99.0.0"),
    .package = "rtfreporter"
  )
  expect_warning(out <- need("gt", "Reading from a gt_tbl"),
                 "needs `gt` >= 99.0.0")
  expect_true(out)
})

test_that("the warning names the installed version and the upgrade command", {
  skip_if_not_installed("gt")
  rtfreporter:::.need_pkg_reset()
  local_mocked_bindings(
    .dep_min_versions = function() c(gt = "99.0.0"),
    .package = "rtfreporter"
  )
  msg <- tryCatch(need("gt", "Reading from a gt_tbl"),
                  warning = function(w) conditionMessage(w))
  expect_match(msg, as.character(utils::packageVersion("gt")), fixed = TRUE)
  expect_match(msg, 'install.packages\\("gt"\\)')
})

test_that("the warning fires once per package per session", {
  skip_if_not_installed("gt")
  rtfreporter:::.need_pkg_reset()
  local_mocked_bindings(
    .dep_min_versions = function() c(gt = "99.0.0"),
    .package = "rtfreporter"
  )
  n <- 0L
  withCallingHandlers(
    for (i in 1:5) need("gt", "Reading from a gt_tbl"),
    warning = function(w) { n <<- n + 1L; invokeRestart("muffleWarning") }
  )
  expect_identical(n, 1L)
})

test_that("the reset clears the once-per-session record", {
  skip_if_not_installed("gt")
  local_mocked_bindings(
    .dep_min_versions = function() c(gt = "99.0.0"),
    .package = "rtfreporter"
  )
  rtfreporter:::.need_pkg_reset()
  expect_warning(need("gt", "x"))
  expect_silent(need("gt", "x"))
  rtfreporter:::.need_pkg_reset()
  expect_warning(need("gt", "x"))
})

# ── wired into the adapters ────────────────────────────────────────────────

test_that("a real adapter call still works with a current package", {
  skip_if_not_installed("gt")
  rtfreporter:::.need_pkg_reset()
  df <- data.frame(a = 1:2, b = c("x", "y"), stringsAsFactors = FALSE)
  g  <- gt::gt(df)
  expect_silent(tbl <- as_rtftable(g))
  expect_s3_class(tbl, "rtftable")
})

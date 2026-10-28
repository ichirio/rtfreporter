# ---------------------------------------------------------------------------
#  The release's automatic validation report: the evidence, then the PDF
# ---------------------------------------------------------------------------
#  Run from the package root (CI: .github/workflows/validation-report.yaml,
#  on a published release or by hand; never on a push):
#    Rscript validation/report.R
#  It gathers, into validation/out/:
#    check.rds     R CMD check (rcmdcheck), unless RTFREPORTER_VAL_CHECK=false
#    tests.rds     every test's result (testthat)
#    coverage.rds  line coverage by file (covr), and the tests that run each
#                  exported function (covtracer)
#    trace: the FR -> FT matrix and summary (validation/trace.R)
#    session.txt   the environment
#  then renders validation/report.Rmd into validation/out/validation-report.pdf
#  (RTFREPORTER_VAL_FORMAT=html for an HTML one).  covr, covtracer,
#  rcmdcheck and rmarkdown are the CI's, not the package's dependencies.
# ---------------------------------------------------------------------------

root <- normalizePath(".", winslash = "/")
out <- file.path(root, "validation", "out")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
step <- function(what) message(sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"), what))

# ---- R CMD check ----------------------------------------------------------------
if (!identical(Sys.getenv("RTFREPORTER_VAL_CHECK"), "false")) {
  step("R CMD check")
  chk <- rcmdcheck::rcmdcheck(root, args = c("--no-manual", "--as-cran"), error_on = "never",
                              check_dir = file.path(out, "check"), quiet = TRUE)
  saveRDS(list(errors = chk$errors, warnings = chk$warnings, notes = chk$notes,
               status = chk$status, version = chk$version, platform = chk$platform),
          file.path(out, "check.rds"))
}

# ---- the tests' results -------------------------------------------------------------
step("tests")
# testthat's own output is the attachment (it ends in [ FAIL | WARN | SKIP | PASS ])
lst <- testthat::ListReporter$new()
log <- utils::capture.output(
  testthat::test_local(root, reporter = testthat::MultiReporter$new(list(
    lst, testthat::ProgressReporter$new(show_praise = FALSE))),
    stop_on_failure = FALSE, load_package = "source"),
  type = "output")
writeLines(log, file.path(out, "test-log-all.txt"), useBytes = TRUE)
d <- as.data.frame(lst$get_results())
tests <- data.frame(file = d$file, test = d$test, expectations = d$nb, failed = d$failed,
                    skipped = d$skipped, error = d$error, seconds = d$real,
                    stringsAsFactors = FALSE)
saveRDS(tests, file.path(out, "tests.rds"))

# ---- coverage, and the tests that run each exported function ----------------------
step("coverage (covr, tests recorded)")
options(covr.record_tests = TRUE)
cov <- covr::package_coverage(root, type = "tests", quiet = TRUE)
by_file <- covr::tally_coverage(cov, by = "line")
files <- aggregate(value ~ filename, data = by_file, function(v) c(lines = length(v),
                                                                     covered = sum(v > 0)))
files <- data.frame(file = files$filename, lines = files$value[, "lines"],
                    covered = files$value[, "covered"], stringsAsFactors = FALSE)
files$percent <- round(100 * files$covered / files$lines, 1)
step("trace (covtracer)")
# the tests that run each function: covtracer's trace of the recorded tests
tr <- tryCatch(covtracer::test_trace_df(cov), error = function(e) NULL)
exports <- sort(getNamespaceExports("rtfreporter"))
fn_tests <- if (!is.null(tr)) {
  x <- as.data.frame(tr)
  x <- x[!is.na(x$alias) & x$alias %in% exports, , drop = FALSE]
  if (!nrow(x)) data.frame(alias = character(), test_name = character(), direct = logical()) else
    data.frame(alias = x$alias, test_name = as.character(x$test_name),
               direct = if (!is.null(x$direct)) as.logical(x$direct) else NA,
               stringsAsFactors = FALSE)
} else NULL
saveRDS(list(total = covr::percent_coverage(cov), files = files, fn_tests = fn_tests,
             exports = exports), file.path(out, "coverage.rds"))

# ---- R-CMD-check on every CI environment, for this commit ----------------------------
# Not run again: the R-CMD-check workflow's run for this commit, read with gh
# (the job's name says its OS and R, its log the tests' count).  Outside CI,
# or with no run for the commit, nothing.
step("R-CMD-check of this commit, every environment (gh)")
gh_lines <- function(...) {
  x <- tryCatch(suppressWarnings(system2("gh", shQuote(c(...)), stdout = TRUE, stderr = FALSE)),
                error = function(e) character())
  if (!is.null(attr(x, "status"))) character() else x
}
repo <- Sys.getenv("GITHUB_REPOSITORY")
sha <- Sys.getenv("GITHUB_SHA")
envs <- NULL
if (nzchar(repo) && nzchar(sha)) {
  run <- gh_lines("api", sprintf("repos/%s/actions/runs?head_sha=%s&per_page=100", repo, sha),
                  "--jq", ".workflow_runs[] | select(.name == \"R-CMD-check\") | [.id, .html_url] | @tsv")
  if (length(run)) {
    run_id <- strsplit(run[1L], "\t", fixed = TRUE)[[1L]][1L]
    jobs <- gh_lines("api", sprintf("repos/%s/actions/runs/%s/jobs?per_page=100", repo, run_id),
                     "--jq", ".jobs[] | [.id, .name, (.conclusion // .status), .html_url] | @tsv")
    if (length(jobs)) {
      j <- do.call(rbind, strsplit(jobs, "\t", fixed = TRUE))
      tests <- vapply(j[, 1L], function(id) {
        log <- gh_lines("api", sprintf("repos/%s/actions/jobs/%s/logs", repo, id))
        hit <- regmatches(log, regexpr("\\[ FAIL [0-9]+ \\| WARN [0-9]+ \\| SKIP [0-9]+ \\| PASS [0-9]+ \\]", log))
        if (length(hit)) hit[length(hit)] else NA_character_
      }, "")
      envs <- data.frame(environment = j[, 2L], result = j[, 3L], tests = unname(tests),
                         url = j[, 4L], stringsAsFactors = FALSE)
    }
  }
}
saveRDS(envs, file.path(out, "environments.rds"))
# (local runs, by hand: a CSV of environment, result, tests, note -- e.g. R
# 4.4.2 and 4.6.1 on the maintainer's PC -- given by RTFREPORTER_VAL_LOCAL)
local_csv <- Sys.getenv("RTFREPORTER_VAL_LOCAL")
if (nzchar(local_csv) && file.exists(local_csv)) {
  file.copy(local_csv, file.path(out, "local-runs.csv"), overwrite = TRUE)
}

# ---- the FR -> FT matrix and the summary (after the environments: it cites them) --
step("FR -> FT")
sys.source(file.path(root, "validation", "trace.R"), envir = new.env())

# ---- the environment: sessionInfo and the versions of every dependency -------------
writeLines(utils::capture.output(utils::sessionInfo()), file.path(out, "session.txt"))
desc <- read.dcf(file.path(root, "DESCRIPTION"), fields = c("Imports", "Suggests"))
deps <- trimws(sub("[(].*$", "", unlist(strsplit(paste(stats::na.omit(desc), collapse = ","), ","))))
deps <- sort(unique(deps[nzchar(deps)]))
utils::write.csv(data.frame(package = deps, version = vapply(deps, function(p) {
  tryCatch(as.character(utils::packageVersion(p)), error = function(e) "not installed")
}, "")), file.path(out, "packages.csv"), row.names = FALSE)
step("report")
fmt <- if (identical(Sys.getenv("RTFREPORTER_VAL_FORMAT"), "html")) {
  rmarkdown::html_document(toc = TRUE)
} else rmarkdown::pdf_document(toc = TRUE)
rmarkdown::render(file.path(root, "validation", "report.Rmd"), output_format = fmt,
                  output_dir = out, output_file = "validation-report",
                  params = list(out = out), envir = new.env(), quiet = TRUE)
step("done")

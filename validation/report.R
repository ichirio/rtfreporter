# ---------------------------------------------------------------------------
#  The release's Development Summary Report: gathered, then the PDF
# ---------------------------------------------------------------------------
#  Run from the package root by the R-CMD-check workflow's last job (at a
#  published release, or by hand), after every environment's record
#  (validation/env.R) was downloaded into validation/env-out/:
#    Rscript validation/report.R
#  It gathers, into validation/out/:
#    environments.rds / environments.txt   every environment: CI's
#                  (validation/env-out/*/) and the maintainers' local runs
#                  (validation/env-local/*/), one row each; the text holds
#                  each one's sessionInfo and tests' log
#    coverage.rds  line coverage by file (covr): an aid, not evidence
#    the FR -> FT matrix and summary.md (validation/trace.R)
#  then renders validation/report.Rmd into validation/out/validation-report.pdf
#  (RTFREPORTER_VAL_FORMAT=html for an HTML one).  covr and rmarkdown are the
#  CI's, not the package's dependencies.
# ---------------------------------------------------------------------------

root <- normalizePath(".", winslash = "/")
out <- file.path(root, "validation", "out")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
step <- function(what) message(sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"), what))

# ---- every environment: CI's jobs and the local runs ---------------------------------
step("environments")
dirs <- c(list.dirs(file.path(root, "validation", "env-out"), recursive = FALSE),
          list.dirs(file.path(root, "validation", "env-local"), recursive = FALSE))
dirs <- dirs[file.exists(file.path(dirs, "env.csv"))]
envs <- if (length(dirs)) do.call(rbind, lapply(dirs, function(d) {
  utils::read.csv(file.path(d, "env.csv"), colClasses = "character", stringsAsFactors = FALSE)
})) else NULL
saveRDS(envs, file.path(out, "environments.rds"))
bundle <- unlist(lapply(dirs, function(d) {
  e <- utils::read.csv(file.path(d, "env.csv"), colClasses = "character")
  c(strrep("=", 78), sprintf("%s (%s)", e$environment, e$source), strrep("=", 78), "",
    "-- sessionInfo() --", readLines(file.path(d, "session.txt"), warn = FALSE), "",
    "-- the tests' log --", readLines(file.path(d, "test-log.txt"), warn = FALSE), "")
}))
writeLines(bundle, file.path(out, "environments.txt"), useBytes = TRUE)

# ---- line coverage: an aid -----------------------------------------------------------
if (!identical(Sys.getenv("RTFREPORTER_VAL_COVERAGE"), "false")) {
  step("coverage (covr)")
  cov <- covr::package_coverage(root, type = "tests", quiet = TRUE)
  by_line <- covr::tally_coverage(cov, by = "line")
  files <- stats::aggregate(value ~ filename, data = by_line,
                            function(v) c(lines = length(v), covered = sum(v > 0)))
  files <- data.frame(file = files$filename, lines = files$value[, "lines"],
                      covered = files$value[, "covered"], stringsAsFactors = FALSE)
  files$percent <- round(100 * files$covered / files$lines, 1)
  saveRDS(list(total = covr::percent_coverage(cov), files = files), file.path(out, "coverage.rds"))
}

# ---- the FR -> FT matrix and the summary (it cites the environments) -------------------
step("FR -> FT")
sys.source(file.path(root, "validation", "trace.R"), envir = new.env())

# ---- the PDF ----------------------------------------------------------------------------
step("report")
fmt <- if (identical(Sys.getenv("RTFREPORTER_VAL_FORMAT"), "html")) {
  rmarkdown::html_document(toc = TRUE)
} else rmarkdown::pdf_document(toc = TRUE)
rmarkdown::render(file.path(root, "validation", "report.Rmd"), output_format = fmt,
                  output_dir = out, output_file = "validation-report",
                  params = list(out = out), envir = new.env(), quiet = TRUE)
step("done")

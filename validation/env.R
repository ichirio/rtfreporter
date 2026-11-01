# ---------------------------------------------------------------------------
#  One environment's record, for the Quality Report
# ---------------------------------------------------------------------------
#  Writes validation/env-out/<slug>/:
#    env.csv        one row: the environment, CI or local, OS, R, the main
#                   dependencies' versions, R CMD check's result, the tests'
#                   summary line
#    session.txt    sessionInfo()
#    packages.csv   the versions of every Imports and Suggests
#    test-log.txt   testthat's output: from R CMD check's tests/testthat.Rout
#                   when a check folder is given, else the tests run here
#
#  In CI (each R-CMD-check job, at a release or by hand):
#    Rscript validation/env.R "<name>" ci <check outcome> <check folder>
#  By hand, on a maintainer's PC, from the package root (the tests run here):
#    Rscript validation/env.R "Windows 11, R 4.6.1" local
#  and copy validation/env-out/<slug>/ into validation/env-local/.
# ---------------------------------------------------------------------------

a <- commandArgs(trailingOnly = TRUE)
name <- a[1L]
source_ <- if (length(a) >= 2L) a[2L] else "local"
check <- if (length(a) >= 3L) a[3L] else NA_character_
check_dir <- if (length(a) >= 4L) a[4L] else NA_character_
root <- normalizePath(".", winslash = "/")
slug <- gsub("[^A-Za-z0-9.]+", "-", tolower(name))
out <- file.path(root, "validation", "env-out", slug)
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# the tests' log: R CMD check's, else run here
rout <- if (!is.na(check_dir)) {
  list.files(check_dir, "^testthat[.]Rout(.fail)?$", recursive = TRUE, full.names = TRUE)
} else character()
log <- if (length(rout)) {
  readLines(rout[1L], warn = FALSE)
} else {
  utils::capture.output(
    testthat::test_local(root, reporter = testthat::ProgressReporter$new(show_praise = FALSE),
                         stop_on_failure = FALSE),
    type = "output")
}
writeLines(log, file.path(out, "test-log.txt"), useBytes = TRUE)
summary_line <- grep("\\[ FAIL [0-9]+ \\| WARN [0-9]+ \\| SKIP [0-9]+ \\| PASS [0-9]+ \\]", log,
                     value = TRUE)
summary_line <- if (length(summary_line)) trimws(summary_line[length(summary_line)]) else NA_character_

# the versions: R, the OS, every dependency
writeLines(utils::capture.output(utils::sessionInfo()), file.path(out, "session.txt"))
desc <- read.dcf(file.path(root, "DESCRIPTION"), fields = c("Version", "Imports", "Suggests"))
deps <- trimws(sub("[(].*$", "", unlist(strsplit(paste(stats::na.omit(desc[, c("Imports", "Suggests")]),
                                                       collapse = ","), ","))))
deps <- sort(unique(deps[nzchar(deps)]))
ver <- function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
utils::write.csv(data.frame(package = deps, version = vapply(deps, ver, "")),
                 file.path(out, "packages.csv"), row.names = FALSE)
main <- c("cards", "ggplot2", "gt", "rtables", "testthat")
si <- utils::sessionInfo()
utils::write.csv(data.frame(
  environment = name, source = source_, os = si$running, r = R.version.string,
  rtfreporter = unname(desc[, "Version"]),
  main_deps = paste(sprintf("%s %s", main, vapply(main, ver, "")), collapse = "; "),
  check = check, tests = summary_line,
  date = format(Sys.time(), "%Y-%m-%d %H:%M %Z"),
  commit = Sys.getenv("GITHUB_SHA", tryCatch(system2("git", c("rev-parse", "--short", "HEAD"),
                                                       stdout = TRUE), error = function(e) NA)),
  stringsAsFactors = FALSE), file.path(out, "env.csv"), row.names = FALSE)
cat(sprintf("%s: %s\n", name, summary_line))

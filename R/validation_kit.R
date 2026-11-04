#' Copy the validation kit: templates to validate rtfreporter in your environment
#'
#' rtfreporter ships its own quality records -- the Functional Requirements
#' (FR), the tests mapped to them and, at each release, a Quality Report.
#' Your company still has to show that the package is fit for *its* use in
#' *its* environment.  This copies the templates for that into a folder, to
#' edit and fill:
#'
#' \describe{
#'   \item{`validation_plan.md`}{The Validation Plan: purpose, scope and
#'     intended use, roles, the risk-based approach (what the package's
#'     evidence covers, what you check), acceptance criteria, deviations.}
#'   \item{`urs.csv`, `urs.md`}{The User Requirement Specification: your
#'     needs (`UR-NNN`), each linked to the package's FR ids; `urs.md`
#'     explains the columns of this file and of the UAT cases.}
#'   \item{`uat_cases.csv`, `uat_cases.xlsx`}{User acceptance test cases
#'     (`UAT-NNN`) on dummy data, with blank `actual`, `result`, `tester` and
#'     `date` to fill.}
#'   \item{`build_user_matrix.R`}{Writes your Traceability Matrix (URS -> FR
#'     -> UAT): `user_traceability.csv`, and `user_traceability.xlsx` when
#'     writexl is installed.}
#'   \item{`validation_summary_report.md`}{The Validation Summary Report you
#'     fill: environment, UAT results, deviations, conclusion.}
#'   \item{`quality-plan.md`, `requirements.csv`}{The package's own Quality
#'     Plan and Functional Requirements, for reference.}
#' }
#'
#' The example content is fictional (a company "Example Pharma"); replace it
#' with your own.  The package's Quality Report and its attachments are on
#' each GitHub release of rtfreporter.
#'
#' @param dir The folder to write into.  Created if it does not exist.
#' @param overwrite `FALSE` (default) refuses a folder that already has files
#'   in it; `TRUE` writes into it, replacing the kit's files of the same name.
#'
#' @return The paths of the files written, invisibly.
#' @seealso The article "Validating rtfreporter in your environment",
#'   <https://ichirio.github.io/rtfreporter/articles/validation.html>.
#' @export
#' @examples
#' kit <- rtfreporter_validation_kit(file.path(tempdir(), "rtfreporter-validation"))
#' basename(kit)
#' unlink(dirname(kit[1]), recursive = TRUE)
rtfreporter_validation_kit <- function(dir, overwrite = FALSE) {
  if (!is.character(dir) || length(dir) != 1L || is.na(dir) || !nzchar(dir)) {
    stop("rtfreporter_validation_kit(dir = ) is one folder path.", call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("rtfreporter_validation_kit(overwrite = ) is TRUE or FALSE.", call. = FALSE)
  }
  vdir <- system.file("validation", package = "rtfreporter")
  tdir <- file.path(vdir, "templates")
  if (!nzchar(vdir) || !dir.exists(tdir)) {
    stop("The validation kit is not installed with this copy of rtfreporter.", call. = FALSE)
  }
  if (dir.exists(dir) && length(list.files(dir, all.files = TRUE, no.. = TRUE)) && !overwrite) {
    stop("rtfreporter_validation_kit(): '", dir, "' is not empty; ",
         "give an empty or new folder, or overwrite = TRUE.", call. = FALSE)
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  from <- c(list.files(tdir, full.names = TRUE),
            file.path(vdir, c("quality-plan.md", "requirements.csv")))
  ok <- file.copy(from, dir, overwrite = TRUE)
  if (!all(ok)) {
    stop("rtfreporter_validation_kit(): could not write ",
         paste(basename(from[!ok]), collapse = ", "), " to '", dir, "'.", call. = FALSE)
  }
  invisible(file.path(normalizePath(dir, winslash = "/"), basename(from)))
}

# Does a glob in the statements (cell_padding_*_twips, rtfreporter.page.*) match
# `x`?  A bare `rtfreporter.*` (every option) names none in particular.
#' @noRd
.fr_glob_names <- function(x, statements) {
  globs <- unique(unlist(regmatches(statements,
    gregexpr("[A-Za-z0-9_.]*\\*[A-Za-z0-9_.*]*", statements))))
  globs <- globs[grepl("[A-Za-z]", globs) & globs != "rtfreporter.*"]
  if (!length(globs)) return(FALSE)
  pats <- paste0("^", gsub("\\*", "[A-Za-z0-9_.]*",
                           gsub(".", "\\.", globs, fixed = TRUE)), "$")
  any(vapply(pats, grepl, logical(1), x = x))
}

# Does a requirement name argument `arg` of function `fn`?  `statements` are
# those of the active leaf FRs that list `fn` in `functions`.  An argument is
# named when a statement has `arg =` (whole word), or `arg` as a bare argument
# inside the parentheses of the call form `fn(...)`, or a glob that matches it.
# The word `arg` in plain prose does not name it.
#' @noRd
.fr_names_arg <- function(fn, arg, statements) {
  statements <- statements[!is.na(statements)]
  if (!length(statements)) return(FALSE)
  esc <- function(s) gsub(".", "\\.", s, fixed = TRUE)
  eq <- paste0("(^|[^A-Za-z0-9_.])", esc(arg), "\\s*=(?!=)")
  if (any(grepl(eq, statements, perl = TRUE))) return(TRUE)
  call_form <- paste0("(^|[^A-Za-z0-9_.])", esc(fn), "\\(")
  for (s in statements) {
    starts <- gregexpr(call_form, s, perl = TRUE)[[1L]]
    if (starts[1L] < 0L) next
    ends <- starts + attr(starts, "match.length") - 1L   # at the "("
    chars <- strsplit(s, "")[[1L]]
    for (open in ends) {
      depth <- 1L; piece <- ""; i <- open + 1L
      while (i <= length(chars) && depth > 0L) {
        ch <- chars[i]
        if (ch %in% c("(", "[", "{")) depth <- depth + 1L
        else if (ch %in% c(")", "]", "}")) depth <- depth - 1L
        if (depth == 0L || (depth == 1L && ch == ",")) {
          if (identical(trimws(piece), arg)) return(TRUE)
          piece <- ""
        } else piece <- paste0(piece, ch)
        i <- i + 1L
      }
    }
  }
  .fr_glob_names(arg, statements)
}

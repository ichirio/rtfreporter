# ---------------------------------------------------------------------------
#  Optional-package guards (#308)
# ---------------------------------------------------------------------------
#
#  Every adapter needs a package rtfreporter only Suggests.  The guards used to
#  ask `requireNamespace()` and nothing else, so an installation that satisfied
#  "is it there?" but not the minimum version in DESCRIPTION sailed past and
#  failed later inside the conversion, with a message that said nothing about
#  the version.
#
#  `.need_pkg()` is that guard. Missing is an error, as before; too old is a
#  warning and the call proceeds -- an older release often still handles the
#  simple cases, and turning a working pipeline into a hard failure over a
#  Suggests constraint would be the worse trade.
#
#  The minimum comes from rtfreporter's OWN DESCRIPTION at run time, so the
#  numbers live in exactly one place: adding `foo (>= 1.2.3)` to Suggests is
#  the whole change, and no version literal is ever repeated in R code.

# Warned-about packages, so a guard inside a loop does not flood the console.
# Cleared by .need_pkg_reset() in the tests.
.rtfreporter_warned <- new.env(parent = emptyenv())

.need_pkg_reset <- function() {
  rm(list = ls(.rtfreporter_warned, all.names = TRUE),
     envir = .rtfreporter_warned)
  invisible(NULL)
}

# Minimum versions declared in our own Imports / Suggests, as a named
# character vector; NA where a dependency carries no constraint.
.dep_min_versions <- function() {
  fields <- utils::packageDescription("rtfreporter",
                                      fields = c("Imports", "Suggests"))
  txt <- paste(unlist(fields[!is.na(fields)]), collapse = ",")
  if (!nzchar(txt)) return(character(0))

  parts <- trimws(strsplit(txt, ",", fixed = TRUE)[[1L]])
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) return(character(0))

  nm  <- sub("[[:space:]]*\\(.*$", "", parts)
  ver <- rep(NA_character_, length(parts))
  has <- grepl("\\([[:space:]]*>=[^)]+\\)", parts)
  ver[has] <- sub(".*\\([[:space:]]*>=[[:space:]]*([^)[:space:]]+).*", "\\1",
                  parts[has])
  names(ver) <- nm
  ver
}

# Guard one optional package.
#
# `pkg`  the package name
# `what` what the caller is doing, phrased to lead the sentence
#        ("Reading a flextable")
.need_pkg <- function(pkg, what) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf(
      "%s requires the `%s` package.  Install it with install.packages(\"%s\").",
      what, pkg, pkg), call. = FALSE)
  }

  want <- .dep_min_versions()[[pkg]]
  if (is.null(want) || is.na(want)) return(invisible(TRUE))

  have <- utils::packageVersion(pkg)
  if (have >= package_version(want)) return(invisible(TRUE))

  if (!is.null(.rtfreporter_warned[[pkg]])) return(invisible(TRUE))
  assign(pkg, TRUE, envir = .rtfreporter_warned)
  warning(sprintf(
    "%s needs `%s` >= %s, but %s is installed.  Results may be wrong or the call may fail.\n  Upgrade with install.packages(\"%s\").",
    what, pkg, want, have, pkg), call. = FALSE)
  invisible(TRUE)
}

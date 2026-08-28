# ---------------------------------------------------------------------------
#  Attach rtfreporter FROM THE SOURCE TREE for the data-raw generators.
# ---------------------------------------------------------------------------
#
#  The generators write files that are committed to the repository, so they
#  must render through the code they live beside.  `library(rtfreporter)` does
#  not: it attaches whatever build happens to be installed, which is a
#  different thing the moment the working tree is ahead of the last install.
#
#  That is not hypothetical.  Six committed examples shipped with literal
#  `{orientation_cmd}` / `{header_dist_twips}` / `{footer_dist_twips}` in
#  their preamble (#306) because a regeneration ran through an installed build
#  older than the template it was reading, and the same trap later hid a
#  reported bug behind a stale install.
#
#  Sourced by every generator:
#
#      source("data-raw/_load.R")     # from the package root
#
#  Falls back to library() when pkgload is unavailable, so the scripts still
#  run outside a development checkout.

local({
  root <- getwd()
  if (!file.exists(file.path(root, "DESCRIPTION"))) {
    stop("Run the data-raw generators from the package root.", call. = FALSE)
  }
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(root, quiet = TRUE, export_all = FALSE)
    message("data-raw: rtfreporter loaded from source (",
            read.dcf(file.path(root, "DESCRIPTION"), "Version")[[1L]], ")")
  } else {
    library(rtfreporter)
    warning("pkgload is not installed; the generators fell back to the ",
            "INSTALLED rtfreporter (", packageVersion("rtfreporter"),
            "), which may differ from this working tree.", call. = FALSE)
  }
})

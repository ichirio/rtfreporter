# ============================================================================
#  rtfreporter_ai_manual() -- hand the AI briefings to the user (#463)
# ============================================================================
#
#  The manuals are shipped in `inst/ai/` rather than only published on the
#  pkgdown site, and this is why: the site serves whatever is on `main`
#  (`development: mode: unreleased`), while a user is on whichever release
#  they installed.  A manual read against the wrong version is worse than no
#  manual, because an assistant quotes a function that will not resolve with
#  exactly the confidence it quotes one that will.  Shipping the file inside
#  the package makes it the same artefact as the code, so the two cannot
#  disagree; this accessor is how it comes back out.

#' The AI assistant manuals that ship with this package
#'
#' rtfreporter is too new to be in any chat model's training data: asked for
#' rtfreporter code, an assistant reaches for `r2rtf`'s verbs or invents
#' arguments, and flags neither as a guess.  The fix is to give it the facts
#' first, and these two files are those facts, each sized to sit in one chat
#' session.
#'
#' `rtfreporter_ai_manual()` returns the path to the copy **installed with
#' this package**, so the manual you attach always describes the version you
#' actually have.  The same files are published on the documentation site, but
#' that copy tracks the development version -- when the two differ, this one is
#' the one that matches your installation.
#'
#' @section Which manual:
#' \describe{
#'   \item{`"user"` (default)}{For *using* rtfreporter: the workflow, every
#'     [as_rtftables()] argument, the four clinical table shapes, headers and
#'     page tokens, listings, figures, borders, and the complete export list.}
#'   \item{`"dev"`}{For working *on* the package: the invariants, the S3 object
#'     model, the rendering pipeline, the adapter contract, and the test /
#'     docs / release conventions.}
#' }
#'
#' Attach **one** of them, never both: the user manual forbids touching
#' internals and the developer manual requires it, so an assistant given both
#' follows neither cleanly.
#'
#' @param which `"user"` (default) or `"dev"` -- see *Which manual*.
#' @param file Optional destination.  When given, the manual is copied there
#'   (ready to attach to a chat session) and the destination is returned
#'   invisibly.  A directory is accepted, and the file keeps its own name.
#' @param overwrite Overwrite `file` if it already exists.  Default `FALSE`.
#'
#' @return The path to the manual -- the installed file when `file` is `NULL`,
#'   otherwise the copy, returned invisibly.
#'
#' @examples
#' # where the manual lives
#' rtfreporter_ai_manual()
#'
#' # read it here, or copy it out to attach to a chat session
#' writeLines(head(readLines(rtfreporter_ai_manual()), 3))
#' rtfreporter_ai_manual("dev", file = tempfile(fileext = ".md"))
#' @export
rtfreporter_ai_manual <- function(which = c("user", "dev"), file = NULL,
                                  overwrite = FALSE) {
  which <- match.arg(which)
  name  <- sprintf("rtfreporter-ai-%s-manual.md", which)
  src   <- system.file("ai", name, package = "rtfreporter")

  if (!nzchar(src) || !file.exists(src)) {
    stop(sprintf(paste0(
      "`rtfreporter_ai_manual()`: %s is not in this installation.
  It ",
      "ships in inst/ai/; a package built before v0.7.56 does not have it.
  ",
      "Reinstall, or read the published copy at
  %s"),
      name, "https://ichirio.github.io/rtfreporter/ai/"), call. = FALSE)
  }
  if (is.null(file)) return(src)

  dest <- if (dir.exists(file)) file.path(file, name) else file
  if (file.exists(dest) && !isTRUE(overwrite)) {
    stop(sprintf(
      "`rtfreporter_ai_manual()`: '%s' already exists; pass `overwrite = TRUE` to replace it.",
      dest), call. = FALSE)
  }
  if (!file.copy(src, dest, overwrite = isTRUE(overwrite))) {
    stop(sprintf("`rtfreporter_ai_manual()`: could not write '%s'.", dest),
         call. = FALSE)
  }
  invisible(dest)
}

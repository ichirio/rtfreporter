# ============================================================================
#  The tokens a header, footer, title or footnote may carry
# ----------------------------------------------------------------------------
#  One list, so a program that offers them (a GUI's "insert" menu, a
#  preview that fills them) asks rtfreporter instead of keeping a copy that
#  drifts.  The substitution itself is .substitute_page_tokens() /
#  .substitute_run_tokens() in generate_rtfreport.R; the test checks that
#  every token listed here is replaced there.
# ============================================================================

#' The tokens a page's text may carry
#'
#' The `{TOKEN}`s that [generate_rtfreport()] fills in headers, footers,
#' titles and footnotes, with what each becomes and when.  A program that
#' offers them -- an "insert" menu, a preview -- reads them here.
#'
#' * `when = "render"`: filled when the file is written ([generate_rtfreport()]).
#' * `when = "viewer"`: an RTF field the word processor computes when the
#'   file is opened, so it stays right after [assemble_rtf()] joins files.
#' * `when = "assemble"`: a slot left empty until [assemble_rtf()] fills it.
#'
#' `{DATETIME}` also takes a format, `{DATETIME:%Y-%m-%d}`
#' ([base::strptime()] codes); `example` shows one.
#'
#' @return A data frame: `token` (as written, with its braces), `kind`
#'   (`"page"` or `"run"`), `when`, `description`, `example` (what it might
#'   print, for a preview).
#' @examples
#' rtf_text_tokens()[, c("token", "when", "description")]
#' @export
rtf_text_tokens <- function() {
  data.frame(
    token = c("{PAGE}", "{TOTAL_PAGES}", "{AUTO_PAGE}", "{AUTO_TOTAL_PAGES}",
              "{BOOK_PAGE}", "{PROGRAM}", "{PROGRAM_NAME}", "{PROGRAM_DIR}",
              "{DATETIME}"),
    kind = c("page", "page", "page", "page", "page", "run", "run", "run",
             "run"),
    when = c("render", "render", "viewer", "viewer", "assemble", "render",
             "render", "render", "render"),
    description = c(
      "Page number, written into the file (the first page of the section)",
      "Total pages of this file, written into the file",
      "Page number the word processor shows (right after assemble_rtf())",
      "Total pages the word processor counts (the whole document)",
      "Page number in the assembled book; empty until assemble_rtf(book_page =)",
      "Path of the program that wrote the file",
      "File name of that program",
      "Folder of that program",
      "Date and time the file was written; {DATETIME:<format>} for another format"),
    example = c("1", "3", "1", "3", "", "programs/t_14_1_1.R", "t_14_1_1.R",
                "programs", "04OCT2026  10:05"),
    stringsAsFactors = FALSE)
}

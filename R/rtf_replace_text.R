# Post-processing: literal text replacement inside a generated RTF file.
#
# This is a "last mile" helper for tweaking an already-rendered RTF deliverable
# (e.g. a one-off footnote wording fix) without re-running the whole pipeline.

#' Replace text inside a generated RTF file
#'
#' A post-processing helper that performs find-and-replace directly on a
#' rendered `.rtf` file's bytes.  It is meant for the "last mile" of TLG
#' production -- small textual fixes to a finished deliverable -- not as a
#' substitute for building the report correctly with the pipe API.
#'
#' One or more `target` strings are replaced by `replacement`.  Pass equal-length
#' vectors to apply several replacements, or a single `replacement` to use the
#' same value for every `target`.  Replacements are applied **sequentially**, so
#' an earlier replacement can affect a later one.
#'
#' @section Important -- this operates on the raw RTF bytes:
#' The file is treated as plain text, so a `target` only matches when it appears
#' **literally** in the file.  RTF stores formatting as control words and
#' escapes non-ASCII characters (`\\uN`, `\\'XX`), so:
#' \itemize{
#'   \item Plain ASCII runs and RTF control words match and replace fine.
#'   \item Text that is split across RTF control words, or non-ASCII characters
#'     that the renderer escaped, will **not** match their on-screen form.
#' }
#' For anything structural, change the inputs to the pipe API instead.
#'
#' @param input_file Path to the RTF file to read.
#' @param target Character vector of strings (or regex patterns when
#'   `use_regex = TRUE`) to search for.  Must be non-empty.
#' @param replacement Character vector of replacements.  Either the same length
#'   as `target`, or length 1 (recycled to every target).
#' @param output_file Path to write the result to.  `NULL` (default) overwrites
#'   `input_file` in place (see `backup`).
#' @param encoding Encoding of `input_file`, used when reading and writing.
#'   Default `"UTF-8"`.  rtfreporter writes ASCII-safe RTF, so the default is
#'   usually correct.
#' @param use_regex Logical.  `FALSE` (default) treats `target` as a fixed
#'   string; `TRUE` treats it as a Perl-compatible regular expression.
#' @param case_insensitive Logical.  `FALSE` (default) matches case-sensitively.
#'   When `TRUE`, matching ignores case (fixed targets are safely escaped before
#'   the case-insensitive match, so regex metacharacters stay literal).
#' @param backup Logical.  When overwriting in place (`output_file = NULL`),
#'   first copy the original to `paste0(input_file, ".bak")`.  Default `TRUE`.
#'   Ignored when `output_file` is given.
#'
#' @return The normalised path to the written file, invisibly.
#'
#' @examples
#' doc <- rtf_document() |>
#'   rtf_tables(data.frame(Parameter = "Age", Value = "75.1")) |>
#'   rtf_titles(list(c("Table 14.1.1 DRAFT vX.Y", "Saftey Population")))
#' f <- tempfile(fileext = ".rtf")
#' generate_rtfreport(doc, f, overwrite = TRUE)
#'
#' # Fix a footnote wording in place (keeps a .bak copy)
#' rtf_replace_text(f, "Saftey Population", "Safety Population")
#'
#' # Several replacements at once, writing to a new file
#' rtf_replace_text(
#'   f,
#'   target      = c("DRAFT", "vX.Y"),
#'   replacement = c("FINAL", "v1.0"),
#'   output_file = tempfile(fileext = ".rtf")
#' )
#'
#' @export
rtf_replace_text <- function(input_file,
                             target,
                             replacement,
                             output_file = NULL,
                             encoding = "UTF-8",
                             use_regex = FALSE,
                             case_insensitive = FALSE,
                             backup = TRUE) {
  # ---- argument checks --------------------------------------------------
  if (!is.character(input_file) || length(input_file) != 1L) {
    stop("`input_file` must be a single file path.", call. = FALSE)
  }
  if (!file.exists(input_file)) {
    stop("`input_file` not found: ", input_file, call. = FALSE)
  }
  if (missing(target) || missing(replacement)) {
    stop("Both `target` and `replacement` must be supplied.", call. = FALSE)
  }
  if (!is.character(target) || !is.character(replacement)) {
    stop("`target` and `replacement` must be character vectors.", call. = FALSE)
  }
  n_t <- length(target)
  n_r <- length(replacement)
  if (n_t == 0L) stop("`target` is empty.", call. = FALSE)
  if (!(n_t == n_r || n_r == 1L)) {
    stop("`replacement` must be the same length as `target`, or length 1.",
         call. = FALSE)
  }
  if (n_r == 1L && n_t > 1L) replacement <- rep(replacement, n_t)

  # ---- read (bytes -> string) -------------------------------------------
  raw  <- readBin(input_file, what = "raw", n = file.info(input_file)$size)
  text <- iconv(rawToChar(raw), from = encoding, to = "UTF-8", sub = "byte")

  # ---- prepare patterns -------------------------------------------------
  # A case-insensitive match needs a regex even for "fixed" targets, so we
  # escape regex metacharacters first and prepend the (?i) flag.
  patterns <- target
  if (!use_regex && !case_insensitive) {
    apply_once <- function(txt, pat, rep) gsub(pat, rep, txt, fixed = TRUE)
  } else {
    if (!use_regex) {
      patterns <- vapply(patterns, .escape_regex, character(1L))
    }
    if (case_insensitive) {
      patterns <- paste0("(?i)", patterns)
    }
    apply_once <- function(txt, pat, rep) gsub(pat, rep, txt, perl = TRUE)
  }

  # ---- apply replacements sequentially ----------------------------------
  for (i in seq_along(patterns)) {
    text <- apply_once(text, patterns[i], replacement[i])
  }

  # ---- write ------------------------------------------------------------
  if (is.null(output_file)) {
    output_file <- input_file
    if (isTRUE(backup)) {
      file.copy(input_file, paste0(input_file, ".bak"), overwrite = TRUE)
    }
  }
  writeBin(charToRaw(iconv(text, from = "UTF-8", to = encoding, sub = "byte")),
           output_file)

  invisible(normalizePath(output_file))
}

# Escape regex metacharacters so a fixed string can be embedded in a Perl
# regular expression (used for the case-insensitive fixed-string path).
.escape_regex <- function(x) {
  gsub("([][{}()^$.|*+?\\\\])", "\\\\\\1", x)
}

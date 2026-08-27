# ============================================================================
#  Package-wide configurable defaults
# ============================================================================
#
#  Defaults are exposed as ordinary R options under the `rtfreporter.*`
#  namespace.  The single source of truth is `.rtfreporter_factory_defaults()`;
#  `.onLoad()` (R/zzz.R) seeds any option not already set, so a site can set its
#  own values in `Rprofile.site` (or a project `.Rprofile`) and they take
#  precedence -- they are present before the package loads, so the seeding step
#  leaves them untouched.
#
#  Resolution precedence used throughout the package (highest wins):
#    1. an explicit function argument / `page` key,
#    2. the option value (`getOption("rtfreporter.<key>")`), and
#    3. the factory default.
#
#  Reproducibility note (clinical / CRAN): because option values can change the
#  output of identical code, a validated run should pin the configuration
#  explicitly in the script.  `rtfreporter_options()` returns a snapshot of the
#  resolved values for the audit trail, and `rtfreporter_reset_defaults()`
#  restores the factory baseline at any time.
# ----------------------------------------------------------------------------

# The factory baseline. The ONLY place package default values are defined.
# Header/footer band distances are deliberately absent: when unset they are
# derived as the full top / bottom margin (see the renderer). A site may still
# set `rtfreporter.page.header_dist_in` / `.footer_dist_in` to pin them.
#
# The margins are based on the FDA eCTD "Portable Document Format (PDF)
# Specifications" minimums (left/binding >= 0.75", other sides >= 0.375",
# landscape top >= 0.75"): a uniform 0.75" on all four sides (the bottom leaves
# room for an overall page-number footer). A wide TFL that does not fit can trade
# horizontal room by narrowing the left/right margins to eCTD's 0.5" (they still
# clear the 0.375" floor) -- either per document via `rtf_document(page = ...)` /
# `rtf_page()`, or site-wide via the `rtfreporter.page.margin_left_in` /
# `.margin_right_in` options. See the "Page setup" article.
#
# Document-wide style defaults (row height, cell padding) are seeded as `NULL`,
# meaning "inherit the font-aware / resource-file baseline". Set them (here, in
# Rprofile.site, or per document via `rtf_config(default_format = list(...))`) to
# change the package-wide default; a per-module value (rtftable / rtf_header /
# rtf_footer / rtf_table_style) always overrides it.
.rtfreporter_factory_defaults <- function() {
  list(
    rtfreporter.page.paper_size       = "letter",
    rtfreporter.page.orientation      = "landscape",
    rtfreporter.page.margin_top_in    = 0.75,
    rtfreporter.page.margin_bottom_in = 0.75,
    rtfreporter.page.margin_left_in   = 0.75,
    rtfreporter.page.margin_right_in  = 0.75,
    rtfreporter.font                  = "Courier",
    rtfreporter.font_size_half_points = 18L,
    # Document-wide style defaults. NULL = inherit the baseline:
    #   row height  -> font-aware default (resource table),
    #   cell padding -> resource default (0 twips = flush).
    rtfreporter.row_height_twips         = NULL,
    rtfreporter.cell_padding_left_twips  = NULL,
    rtfreporter.cell_padding_right_twips = NULL,
    # Cell-text markup applied at render time. "script" = ^{}/_{} super/subscript
    # only; the >=/<= -> symbol conversion ("relational") is opt-in.
    rtfreporter.markup                   = "script",
    # How the page title / footnote blocks are rendered: "text" = plain
    # paragraphs (full writable width), "table" = a content-width single-column
    # table.  Title defaults to text; footnote keeps the table form.
    rtfreporter.title_format             = "text",
    rtfreporter.footnote_format          = "table",
    rtfreporter.title_width              = NULL,
    rtfreporter.footnote_width           = NULL,
    # DPI assumed for an embedded figure (rtfplot) whose file carries no
    # resolution metadata. Figures default to their native size at this DPI.
    rtfreporter.figure.default_dpi       = 96L
  )
}

# Internal accessor: the resolved value of one default (option, else factory).
.opt <- function(name) {
  getOption(name, .rtfreporter_factory_defaults()[[name]])
}

#' Inspect the active rtfreporter defaults
#'
#' Returns a named list of the currently **resolved** `rtfreporter.*` default
#' values -- the option value where one is set (e.g. in `Rprofile.site`),
#' otherwise the factory baseline. Use it to record the configuration a report
#' was generated under (a useful audit trail for validated/reproducible runs).
#'
#' @return A named list of resolved default values.
#'
#' @seealso [rtfreporter_reset_defaults()] to restore the factory baseline.
#'
#' @examples
#' rtfreporter_options()
#'
#' @export
rtfreporter_options <- function() {
  nms <- names(.rtfreporter_factory_defaults())
  out <- lapply(nms, .opt)
  names(out) <- nms
  out
}

#' Restore the factory default options
#'
#' Re-applies the package's factory baseline to every `rtfreporter.*` option,
#' discarding any site or session overrides. Use this to recover a known state
#' if a configuration has been changed in error.
#'
#' @return Invisibly, the factory default list that was applied.
#'
#' @seealso [rtfreporter_options()] to inspect the active values.
#'
#' @examples
#' old <- options(rtfreporter.font = "Arial")
#' rtfreporter_reset_defaults()       # back to "Courier"
#' getOption("rtfreporter.font")
#' options(old)
#'
#' @export
rtfreporter_reset_defaults <- function() {
  fd <- .rtfreporter_factory_defaults()
  options(fd)
  invisible(fd)
}

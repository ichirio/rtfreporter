# The validation scenarios (FR-IX131 .. FR-IX134): data to an RTF file,
# compared byte for byte with a reference under fixtures/validation/.
# Each builder is the whole program of one scenario, on synthetic data, with
# nothing that varies from run to run (no {DATETIME}, no {PROGRAM}).
#
# Regenerate the references only when a deliberate change elsewhere alters
# the RTF (say so in the PR, as CONTRIBUTING's Regression asks):
#   pkgload::load_all(); testthat::source_test_helpers("tests/testthat")
#   .val_write_references()

.val_fixture <- function(...) testthat::test_path("fixtures", "validation", ...)

.val_section <- function(doc) {
  rtf_section(doc, page = 1, secinfo = list(
    header = rtf_header(rows = list(c(l = "STUDY-01", r = "Page {PAGE}"))),
    footer = rtf_footer(rows = list(c(l = "Listing 16.2.1 / Table 14.1.1")))))
}

# the records of a listing
.val_records <- function(n = 10L) {
  data.frame(USUBJID = sprintf("S-%03d", seq_len(n)),
             ARM = rep(c("Placebo", "Drug"), length.out = n),
             AGE = 30L + seq_len(n), SEX = rep(c("F", "M", "M"), length.out = n),
             stringsAsFactors = FALSE)
}

# a hand-built long frame: an ARD nobody needs cards to make
.val_long <- function() {
  data.frame(
    TRT       = rep(c("Placebo", "Drug"), each = 6),
    variable  = rep(rep(c("ALT", "AST"), each = 3), 2),
    stat_name = rep(c("n", "mean", "sd"), 4),
    stat      = c(20, 31.245, 4.1, 20, 28.7, 3.92,
                  18, 33.108, 5.3, 18, 30.2, 4.44),
    stringsAsFactors = FALSE)
}

.val_listing_plan <- function(d = .val_records(), max_rows = 4L) {
  p <- table_plan(d) |>
    plan_listing(listing_col("USUBJID", width = 12),
                 listing_col("ARM", width = 10),
                 listing_col("AGE", width = 6, align = "right"),
                 listing_col("SEX", width = 6, align = "center")) |>
    plan_style(border = "tfl") |>
    plan_titles("Listing 16.2.1", "Subjects by arm") |>
    plan_footnotes("Source: ADSL (synthetic)")
  if (!is.null(max_rows)) p <- plan_paginate_rows(p, max_rows = max_rows)
  p
}

.val_summary_plan <- function() {
  table_plan(.val_long(), cols = "TRT", rows = c(param = "variable"),
             label = c(row = "stat_name")) |>
    plan_cells(c("n" = "{n:.0f}", "Mean (SD)" = "{mean} ({sd})"), notes = FALSE) |>
    plan_digits(2) |>
    plan_digits(AST = 1) |>
    plan_style(border = "tfl")
}

# FR-IX132: a listing from records, paginated, with titles and footnotes
.val_listing_doc <- function() {
  rtf_document() |> .val_section() |> rtf_tables(.val_listing_plan())
}

# FR-IX133: a figure page from a PNG file (fixtures/validation/dot.png, a
# 1x1 PNG written by hand so no graphics device is needed)
.val_figure_doc <- function(png = .val_fixture("dot.png")) {
  rtf_document() |> .val_section() |>
    rtf_figures(list(rtfplot(png, width_twips = 4320L, height_twips = 2880L)),
                titles = c("Figure 14.2.1", "A dot"),
                footnotes = "Source: fixtures/validation/dot.png")
}

# FR-IX134: a summary and its listing on one page
.val_page_doc <- function() {
  rtf_document() |> .val_section() |>
    rtf_tables(.val_summary_plan(), titles = "Table 14.1.1 Summary") |>
    rtf_tables(.val_listing_plan(.val_records(6L), max_rows = NULL),
               titles = "Listing 16.2.1 Records", page_break = FALSE)
}

.val_scenarios <- function() {
  list(listing = .val_listing_doc, figure = .val_figure_doc, page = .val_page_doc)
}

# the lines the document writes, from a fresh file each time
.val_render <- function(doc) {
  d <- tempfile("validation-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- file.path(d, "out.rtf")
  suppressMessages(generate_rtfreport(doc, f, overwrite = TRUE))
  readLines(f, warn = FALSE)
}

.val_write_references <- function(dir = .val_fixture()) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for (nm in names(.val_scenarios())) {
    writeLines(.val_render(.val_scenarios()[[nm]]()),
               file.path(dir, paste0(nm, ".rtf")), useBytes = TRUE)
  }
  invisible(dir)
}

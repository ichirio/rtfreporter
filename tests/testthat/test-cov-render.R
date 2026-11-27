# Coverage of generate_rtfreport()'s render paths that the other tests leave
# out: the program found by a source() of a connection, legacy / gap-filled
# spanning header rows, multi data frame tables with cell styles, header and
# footer rows with no cells or two unnamed cells, and the title format option.

.cov_render <- function(doc) {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

.cov_doc <- function(tbl, header = NULL, footer = NULL) {
  rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = header, footer = footer)) |>
    rtf_tables(tbl)
}

test_that("generate_rtfreport() names the program from a sourced connection's srcfile", {
  skip_on_cran()
  d <- tempfile("covprog")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  old <- options(rtfreporter.program = NULL)
  on.exit(options(old), add = TRUE)
  out <- file.path(d, "t_con.rtf")
  code <- c(
    "doc <- rtf_document() |>",
    "  rtf_section(page = 1, secinfo = list(header = NULL,",
    "    footer = rtf_footer(list(c(l = 'N={PROGRAM_NAME}'))))) |>",
    "  rtf_tables(as_rtftables(data.frame(A = 'a')))",
    sprintf("generate_rtfreport(doc, '%s', overwrite = TRUE)",
            gsub("\\", "/", out, fixed = TRUE)))
  # a connection, not a file name: source() has no `ofile` string, only the
  # srcfile it builds from the connection's expression (keep.source = TRUE)
  prog_con <- textConnection(code)
  on.exit(close(prog_con), add = TRUE)
  expect_message(source(prog_con, local = new.env(), keep.source = TRUE),
                 "the file source() is running", fixed = TRUE)
  expect_match(paste(readLines(out, warn = FALSE), collapse = "\n"),
               "N=prog_con", fixed = TRUE)
})

test_that("a spanning row that leaves a column uncovered takes that column's border", {
  skip_on_cran()
  df <- data.frame(A = "a", B = "b", C = "c")
  # the legacy from/to row form is not gap-filled: column 1 has no span
  tbl <- rtftable(df,
    col_header = list(list(list(from = 2, to = 3, label = "Grp")),
                      c("A", "B", "C")),
    col_spec = list(list(col = 1,
                         border = rtf_border(left = rtf_border_line("double", 45L)))))
  out <- .cov_render(.cov_doc(tbl))
  expect_match(out, "Grp", fixed = TRUE)
  # the column's left rule is on the spanning row and on the label row
  expect_gte(lengths(regmatches(out, gregexpr("\\clbrdrl\\brdrdb\\brdrw45",
                                             out, fixed = TRUE))), 2L)
})

test_that("a spanning cell is centred when its first column has no header alignment", {
  skip_on_cran()
  df <- data.frame(A = "a", B = "b")
  tbl <- rtftable(df, col_header = list(list(list(from = 1, to = 2, label = "Both")),
                                        c("A", "B")))
  # every API-built col_spec has a header_align; an empty one falls back
  tbl$col_spec[[1L]]$header_align <- ""
  out <- .cov_render(.cov_doc(tbl))
  expect_match(out, "\\\\qc[^}]*Both", perl = TRUE)
})

test_that("a legacy spanning_header field renders above the header and adds its colours", {
  skip_on_cran()
  df <- data.frame(A = "a", B = "b")
  tbl <- rtftable(df, col_header = c("HdrA", "HdrB"))
  # the old `spanning_header =` argument is gone; a table carrying the field
  # (from before the change) still renders it as the top header row
  tbl$spanning_header <- list(list(
    from = 1, to = 2, label = "Legacy span",
    border = rtf_border(bottom = rtf_border_line("single", 15L, "#00FF00"))))
  out <- .cov_render(.cov_doc(tbl))
  expect_match(out, "Legacy span", fixed = TRUE)
  expect_match(out, "\\red0\\green255\\blue0;", fixed = TRUE)
  expect_lt(regexpr("Legacy span", out, fixed = TRUE),
            regexpr("HdrA", out, fixed = TRUE))
})

test_that("a multi data frame table slices cell_styles per data frame", {
  skip_on_cran()
  tbl <- rtftable(list(data.frame(A = c("a1", "a2"), B = 1:2),
                       data.frame(A = "z9", B = 3L)),
                  cell_styles = list(NULL, NULL, list(bold = c(TRUE, NA))))
  out <- .cov_render(.cov_doc(tbl))
  # the third style row lands on the second data frame's only row
  expect_match(out, "\\b z9", fixed = TRUE)
  expect_no_match(out, "\\b a1", fixed = TRUE)
  expect_no_match(out, "\\b a2", fixed = TRUE)
})

test_that("a figure whose alignment is not left / right / center is centred", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  p <- rtfplot(ggplot2::ggplot(data.frame(x = 1:2, y = 1:2), ggplot2::aes(x, y)) +
                 ggplot2::geom_point(), render_width = 2, render_height = 1.5)
  # rtfplot() refuses any other value; a hand-edited object falls back
  p$align <- "justify"
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_figures(p)
  out <- .cov_render(doc)
  expect_match(out, "\\qc{\\pict", fixed = TRUE)
})

test_that("a header row with no cells and an unnamed two-cell row render", {
  skip_on_cran()
  hdr <- rtf_header(list(character(0), c("Left text", "Right text")))
  out <- .cov_render(.cov_doc(as_rtftables(data.frame(A = 1)), header = hdr))
  # the empty row is one empty cell; the unnamed pair is left / right
  expect_match(out, "\\\\ql[^}]*Left text", perl = TRUE)
  expect_match(out, "\\\\qr[^}]*Right text", perl = TRUE)
})

test_that("generate_rtfreport() refuses an unknown title_format option", {
  skip_on_cran()
  old <- options(rtfreporter.title_format = "box")
  on.exit(options(old), add = TRUE)
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  doc <- .cov_doc(as_rtftables(data.frame(A = 1)))
  expect_error(generate_rtfreport(doc, f),
               "`title_format` / `footnote_format` must be \"text\" or \"table\"; got 'box'",
               fixed = TRUE)
})

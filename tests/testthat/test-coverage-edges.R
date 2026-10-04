# Tests that raise coverage before the first CRAN submission (#551).
# They pin argument validation, error / warning wording, print methods and
# empty / one-row / one-column edges that no other file exercised.  Nothing
# here reads the network or draws random numbers.

# ── page, default format, watermark: validation and print methods ──────────

test_that("rtf_page() rejects a malformed paper_size and prints its geometry", {
  expect_error(rtf_page(paper_size = c("A4", "letter")),
               "`paper_size` must be a single preset name")
  expect_error(rtf_page(paper_size = 4L), "`paper_size` must be a single preset")

  expect_output(print(rtf_page(width_in = 8.5, height_in = 11)),
                "<rtf_page> 8.5 x 11 in")
  expect_output(print(rtf_page(paper_size = "A4", orientation = "portrait")),
                "<rtf_page> A4 \\(portrait\\)")
  expect_output(print(rtf_page(width_in = 8.5)), "8.5 x \\? in")
  p <- rtf_page(margin_top_in = 1)
  expect_identical(withVisible(capture.output(print(p)))$visible, TRUE)
  expect_output(expect_identical(print(p), p), "<rtf_page>")
  out <- capture.output(print(p))
  expect_match(out[2], "margins \\(in\\): top 1,")
})

test_that("rtf_default_format() rejects negative twips and prints its settings", {
  for (nm in c("row_height_twips", "cell_padding_left_twips",
               "cell_padding_right_twips")) {
    args <- stats::setNames(list(-1), nm)
    expect_error(do.call(rtf_default_format, args),
                 sprintf("`%s` must be a single non-negative integer", nm),
                 info = nm)
    args <- stats::setNames(list(c(1, 2)), nm)
    expect_error(do.call(rtf_default_format, args), nm, info = nm)
    args <- stats::setNames(list(NA_real_), nm)
    expect_error(do.call(rtf_default_format, args), nm, info = nm)
  }
  expect_error(rtf_default_format(font_size_half_points = 0),
               "`font_size_half_points` must be a single positive integer")
  expect_error(rtf_default_format(font_size_half_points = NA_real_),
               "`font_size_half_points` must be a single positive integer")

  f <- rtf_default_format(font_size_half_points = 16L, row_height_twips = 300)
  out <- capture.output(res <- print(f))
  expect_identical(res, f)
  expect_match(out[1], "<rtf_default_format>")
  expect_match(out[2], "font 16 half-points; row height 300; padding L/R")
  expect_match(out[3], "markup \\[")
  # unset row height / padding are said as "auto" / 0
  out0 <- capture.output(print(rtf_default_format(row_height_twips = NULL,
                                                  cell_padding_left_twips = NULL,
                                                  cell_padding_right_twips = NULL)))
  expect_match(out0[2], "row height auto; padding L/R 0/0 twips")
})

test_that("rtf_watermark() validates every argument and prints a summary", {
  expect_error(rtf_watermark(), "`text` is required")
  expect_error(rtf_watermark(NULL), "`text` is required")
  expect_error(rtf_watermark(character(0)), "`text` is required")
  expect_error(rtf_watermark("X", font_size_half_points = 0),
               "`font_size_half_points` must be one positive number")
  expect_error(rtf_watermark("X", color = "red"), "must be one \"#RRGGBB\"")
  expect_error(rtf_watermark("X", color = NA_character_), "#RRGGBB")
  expect_error(rtf_watermark("X", angle = c(1, 2)), "`angle` must be one number")
  expect_error(rtf_watermark("X", width_in = 0), "`width_in` must be one positive")
  expect_error(rtf_watermark("X", height_in = -1), "`height_in` must be one positive")
  expect_error(rtf_watermark("X", font = c("A", "B")),
               "`font` must be NULL or one font-family name")

  # a missing text becomes the empty string rather than "NA"
  expect_identical(rtf_watermark(NA)$text, "")

  # print() dispatches to the method (registered since #546)
  out_print <- capture.output(print(rtf_watermark("DRAFT", font = "Arial")))
  expect_identical(out_print[1], "<rtf_watermark>")
  pw <- rtfreporter:::print.rtf_watermark
  w <- rtf_watermark("DRAFT", font = "Arial")
  out <- capture.output(res <- pw(w))
  expect_identical(res, w)
  expect_identical(out[1], "<rtf_watermark>")
  expect_true(any(grepl("^  text  : DRAFT$", out)))
  expect_true(any(grepl("^  font  : Arial$", out)))
  out_nofont <- capture.output(pw(rtf_watermark("DRAFT")))
  expect_false(any(grepl("font  :", out_nofont)))
})

test_that("print.rtf_stub_spec() shows the optional label and label_span lines", {
  s <- stub_spec(vars = c("SOC", "PT"), label = "Term", label_span = TRUE)
  out <- capture.output(res <- print(s))
  expect_identical(res, s)
  expect_true(any(grepl("^  label      : Term *$", out)))
  expect_true(any(grepl("^  label_span : TRUE *$", out)))
  plain <- capture.output(print(stub_spec(vars = "PT")))
  expect_false(any(grepl("label", plain)))
  expect_error(stub_spec(), "`vars` is required")
  expect_error(stub_spec(NULL), "`vars` is required")
})

test_that("footnote rows are strings or named vectors, never lists", {
  fr <- rtfreporter:::.footnote_rows
  expect_identical(fr(NULL), list())
  expect_identical(fr(character(0)), list())
  expect_error(fr(list(list(text = "a"))),
               "A footnote row is a string or a named vector")
  expect_error(fr(list(c("a", "b"))),
               "A footnote row must be one string, or a named vector")
  expect_identical(fr(list(NULL), format = "text"), list(""))
  one <- fr("note", align = "right")
  expect_identical(names(one[[1]]), "r")
})

test_that("rtfreporter_ai_manual() copies, refuses to clobber and names a missing file", {
  p <- rtfreporter_ai_manual()
  expect_true(file.exists(p))
  dest <- tempfile(fileext = ".md")
  on.exit(unlink(dest), add = TRUE)
  expect_identical(rtfreporter_ai_manual("user", file = dest), dest)
  expect_error(rtfreporter_ai_manual("user", file = dest),
               "already exists; pass `overwrite = TRUE`")
  expect_silent(rtfreporter_ai_manual("user", file = dest, overwrite = TRUE))
  # a directory gets the manual under its own name
  d <- tempfile("man"); dir.create(d); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  got <- rtfreporter_ai_manual("dev", file = d)
  expect_identical(basename(got), "rtfreporter-ai-dev-manual.md")
  expect_error(rtfreporter_ai_manual("other"), "should be one of")
})

# ── drop_cols: the reindexing helpers ──────────────────────────────────────

test_that(".resolve_drop_cols() returns nothing for NULL and refuses to drop every column", {
  df <- data.frame(a = 1, b = 2, c = 3)
  rd <- rtfreporter:::.resolve_drop_cols
  expect_identical(rd(NULL, df), integer(0))
  expect_identical(rd(character(0), df), integer(0))
  expect_identical(rd(c("c", "a", "c"), df), c(1L, 3L))
  expect_error(rd(c("a", "b", "c"), df), "must leave at least one column")
  expect_error(rd("zzz", df), "zzz")
})

test_that(".apply_col_drop() reindexes every positional argument", {
  df <- data.frame(a = 1:2, b = 3:4, c = 5:6)
  attr(df, "rtf_blank_rows") <- 1L
  ca <- list(
    data = df,
    col_rel_width = c(1, 2, 3), column_widths_twips = c(100, 200, 300),
    col_header_align = c("left", "center", "right"),
    col_header = list(c("A", "B", "C"),
                      list(list(pos = c(2, 3), label = "BC"),
                           list(from = 1, to = 1, label = "A"))),
    col_spec = list(list(col = 1, bold = TRUE), list(col = "b", italic = TRUE),
                    list(col = 3, align = "right"), list(bold = TRUE)),
    row_title = c(1, 3),
    cell_styles = list(NULL, list(bold = c(TRUE, FALSE, TRUE), other = "x"))
  )
  out <- rtfreporter:::.apply_col_drop(ca, 2L)
  expect_identical(names(out$data), c("a", "c"))
  expect_identical(attr(out$data, "rtf_blank_rows"), 1L)
  expect_identical(out$col_rel_width, c(1, 3))
  expect_identical(out$column_widths_twips, c(100, 300))
  expect_identical(out$col_header_align, c("left", "right"))
  expect_identical(out$col_header[[1]], c("A", "C"))
  # the spanning cell over columns 2-3 shrinks to the surviving column 3 -> 2
  expect_identical(out$col_header[[2]][[1]]$pos, 2L)
  expect_identical(out$col_header[[2]][[2]]$from, 1L)
  # integer `col` is remapped, the dropped name is gone, a col-less spec stays
  expect_identical(vapply(out$col_spec, function(s) s$col %||% NA, 1), c(1, 2, NA))
  expect_identical(out$row_title, c(1L, 2L))
  expect_identical(out$cell_styles[[2]]$bold, c(TRUE, TRUE))
  expect_identical(out$cell_styles[[2]]$other, "x")
  expect_null(out$cell_styles[[1]])

  # nothing to drop -> unchanged; every column -> an error
  expect_identical(rtfreporter:::.apply_col_drop(ca, integer(0)), ca)
  expect_identical(rtfreporter:::.apply_col_drop(ca, 99L), ca)
  expect_error(rtfreporter:::.apply_col_drop(ca, 1:3),
               "must leave at least one column")
})

test_that("the col_header / col_spec / row_title reindexers handle the odd shapes", {
  ri <- rtfreporter:::.reindex_col_header
  keep <- c(1L, 3L)
  expect_identical(ri(c("A", "B", "C"), keep, 3L), c("A", "C"))
  expect_identical(ri("A | B | C", keep, 3L), c("A", "C"))
  expect_identical(ri("one", keep, 3L), "one")          # no pipes: left alone
  expect_identical(ri("A | B", keep, 3L), "A | B")      # wrong width: left alone
  expect_identical(ri(c("A", "B"), keep, 3L), c("A", "B"))
  expect_null(ri(list(list(list(pos = 2L, label = "gone"))), keep, 3L))
  expect_identical(ri(list(c("A", "B")), keep, 3L), list(c("A", "B")))
  expect_identical(ri(1L, keep, 3L), 1L)                 # not a header at all

  rc <- rtfreporter:::.reindex_header_cell
  expect_identical(rc("plain", keep), "plain")
  sel <- list(pos = function(nm) nm == "b", label = "sel")
  expect_identical(rc(sel, keep), sel)                   # selectors resolve later
  expect_null(rc(list(from = 2L, to = 2L, label = "x"), keep))
  expect_identical(rc(list(label = "no position"), keep),
                   list(label = "no position"))

  rs <- rtfreporter:::.reindex_col_spec
  expect_null(rs(list(list(col = "b", bold = TRUE)), keep, "b"))
  expect_null(rs(list(list(col = 2, bold = TRUE)), keep, "b"))

  rt <- rtfreporter:::.reindex_row_title
  expect_null(rt(NULL, keep, "b"))
  expect_identical(rt(c("a", "b"), keep, "b"), "a")
  expect_null(rt("b", keep, "b"))
  expect_null(rt(2L, keep, "b"))
  expect_identical(rt(c(1L, 2L, 3L), keep, "b"), c(1L, 2L))
})

test_that("as_rtftables(drop_cols = ) hides a column and keeps the others", {
  d <- data.frame(g = c("a", "a", "b"), x = 1:3, y = 4:6, stringsAsFactors = FALSE)
  pages <- as_rtftables(d, drop_cols = "g")
  expect_identical(names(pages[[1]]$data), c("x", "y"))
  expect_error(as_rtftables(d, drop_cols = c("g", "x", "y")),
               "must leave at least one column")
})

# ── stub_cols() as_rtftables(stub = ) metadata, collapse_repeats(), fonts ──

test_that("the stub style helpers carry spans and skip inserted label rows", {
  rm <- rtfreporter:::.stub_remap_styles
  styles <- list(list(bold = c(TRUE, FALSE, TRUE)), NULL)
  expect_null(rm(NULL, 1:2, 3L, 2:3))
  expect_identical(rm(styles, NULL, 3L, 2:3), styles)
  out <- rm(styles, c(NA, 1L, 2L), 3L, 2:3)
  expect_null(out[[1]])                                  # inserted label row
  expect_identical(out[[2]]$bold, c(NA, FALSE, TRUE))  # leading NA for the stub
  expect_null(out[[3]])
  keep_cols <- rm(styles, c(1L, 2L), NULL, NULL)       # layout = "columns"
  expect_identical(keep_cols, styles)
  # a style that is not a per-column vector is left alone
  scalar <- rm(list(list(bold = TRUE)), 1L, 3L, 2:3)
  expect_identical(scalar[[1]]$bold, TRUE)

  ms <- rtfreporter:::.stub_mark_spans
  expect_identical(ms(NULL, integer(0), 3L), NULL)
  expect_identical(ms(styles, NULL, 2L), styles)
  marked <- ms(NULL, c(1L, 3L, 9L), 3L)
  expect_true(marked[[1]]$span_row)
  expect_null(marked[[2]])
  expect_true(marked[[3]]$span_row)
  expect_length(marked, 3L)
  both <- ms(list(NULL, list(bold = TRUE)), 2L, 2L)
  expect_true(both[[2]]$span_row)
  expect_true(both[[2]]$bold)
})

test_that(".prepend_stub_header() puts the stub label on the leaf row only", {
  ph <- rtfreporter:::.prepend_stub_header
  expect_null(ph(NULL, "Term", 2L))
  expect_identical(ph(c("A", "B"), "Term", 2L), c("Term", "A", "B"))
  two <- ph(list(list(list(pos = c(1L, 2L), label = "Both")), c("A", "B")),
            "Term", 2L)
  expect_identical(two[[2]], c("Term", "A", "B"))
  expect_identical(two[[1]][[1]], list(pos = 1L, label = ""))
  expect_identical(two[[1]][[2]]$pos, c(2L, 3L))
  legacy <- ph(list(list(list(from = 1L, to = 2L, label = "x")), c("A", "B")),
               "Term", 2L)
  expect_identical(legacy[[1]][[2]]$from, 2L)
  expect_identical(legacy[[1]][[2]]$to, 3L)
  sel <- ph(list(list(list(pos = function(nm) TRUE, label = "s")), c("A", "B")),
            "Term", 2L)
  expect_true(is.function(sel[[1]][[2]]$pos))
  expect_identical(ph(7L, "Term", 2L), 7L)
})

test_that("collapse_repeats() works on a multi-table rtftable and on one table", {
  d1 <- data.frame(g = c("a", "a", "b"), v = 1:3, stringsAsFactors = FALSE)
  d2 <- data.frame(g = c("c", "c"), v = 4:5, stringsAsFactors = FALSE)
  multi <- rtftable(list(d1, d2))
  out <- collapse_repeats(multi, cols = "g")
  expect_identical(out$data_list[[1]]$g, c("a", NA, "b"))
  expect_identical(out$data_list[[2]]$g, c("c", NA))
  single <- collapse_repeats(rtftable(d1), cols = "g")
  expect_identical(single$data$g, c("a", NA, "b"))
  expect_error(collapse_repeats(rtftable(d1), cols = "nope"))
})

test_that("font helpers cope with empty and odd font tables", {
  fn <- rtfreporter:::.font_names
  expect_identical(fn(NULL), character(0))
  expect_identical(fn(c("Arial", "", "Courier")), c("Arial", "Courier"))
  expect_identical(fn(list("Arial", list(name = "Cambria"), list(), list(name = ""))),
                   c("Arial", "Cambria"))

  cf <- rtfreporter:::.collect_fonts
  old <- options(rtfreporter.font = "Courier New")
  on.exit(options(old), add = TRUE)
  expect_identical(cf(NULL, list("Arial", NA, "")), c("Courier New", "Arial"))
  expect_identical(cf(c("Arial"), list("Arial", "Cambria")), c("Arial", "Cambria"))

  expect_identical(rtfreporter:::.build_font_index_map(character(0)), list())
  expect_match(rtfreporter:::.build_font_table_rtf(character(0)),
               "\\\\f0\\\\fnil\\\\fcharset0 Courier New;")
  expect_error(rtfreporter:::.check_font(c("A", "B"), "font"),
               "`font` must be a single font family name")
  expect_error(rtfreporter:::.check_font("", "font"), "single font family name")
  expect_null(rtfreporter:::.check_font(NULL, "font"))

  rep <- structure(list(document = list(title_style = list(font = "Arial")),
                        pages = list(), sections = list("not a list",
                                     list(header = c(l = "x"), footer = NULL))),
                   class = "rtfreport")
  expect_identical(rtfreporter:::.collect_report_fonts(rep), "Arial")
})

# ── table_plan(): the header as cell rows, and plan_layers() reading it ────

edge_ard <- function(by = "TRT") {
  adsl <- cards::ADSL
  adsl$SEX <- as.character(adsl$SEX)
  adsl$TRT <- as.character(adsl$ARM)
  suppressMessages(normalize_ard(cards::ard_stack(
    adsl, .by = all_of(by),
    cards::ard_continuous(
      variables = AGE,
      statistic = ~ cards::continuous_summary_fns(c("N", "mean", "sd"))),
    .total_n = TRUE)))
}

edge_plan <- function(by = "TRT") {
  table_plan(edge_ard(by), cols = by, rows = c(group = "variable")) |>
    plan_cells(notes = FALSE) |>
    plan_cells(continuous = c("n" = "{N:d}", "Mean (SD)" = "{mean} ({sd})"))
}

first_page <- function(x) if (inherits(x, "rtftable")) x else x[[1L]]

test_that("a header written as cell rows is read back by plan_layers()", {
  skip_if_not_installed("cards")
  hd <- data.frame(
    line = c(1, 1, 1, 2, 2, 2),
    cols = c("group", "label", ".values", "group", "label", ".values"),
    span = c(NA, NA, "TRT", NA, NA, "each"),
    text = c("Grp", "Term", "Treatment", "", "", "{col} (N={n})"),
    align = c(NA, NA, "center", NA, NA, NA),
    bold = c(NA, NA, "yes", NA, NA, NA),
    border_bottom = c(NA, NA, "single", NA, NA, NA),
    stringsAsFactors = FALSE)
  p <- plan_col_header(edge_plan(), hd)
  lay <- suppressMessages(plan_layers(p))
  expect_identical(lay$header$source, "cells")
  cells <- lay$header$cells
  expect_identical(names(cells), c("line", "cols", "span", "text", "align",
                                   "bold", "border_top", "border_bottom"))
  expect_identical(nrow(cells), 6L)
  expect_identical(cells$text[3], "Treatment")
  expect_identical(cells$bold[3], "TRUE")
  expect_identical(cells$border_bottom[3], "single")
  expect_true(is.na(cells$border_top[3]))
  # the header the plan builds from them: a spanner row over a leaf row
  hdr <- first_page(suppressMessages(plan_apply(p)))$col_header
  expect_length(hdr, 2L)
  expect_match(hdr[[2]][3], "^Placebo \\(N=")
})

test_that("a header sheet that names a missing column, key or position is refused", {
  skip_if_not_installed("cards")
  run <- function(...) suppressMessages(plan_apply(plan_col_header(
    edge_plan(), data.frame(line = 1, text = "x", stringsAsFactors = FALSE,
                            ...))))
  expect_error(run(cols = "nope"), "no column .nope. on the page")
  expect_error(run(cols = ".values", span = "BAD"),
               "BAD. is not a column key; the keys are .TRT.")
  expect_error(run(cols = "9"), "position outside 1..5")
  # ranges and `KEY = value` selectors resolve against the page
  got <- first_page(suppressMessages(plan_apply(plan_col_header(
    edge_plan(), data.frame(line = 1, cols = c("1:2", "3:last"),
                            text = c("left", "right"),
                            stringsAsFactors = FALSE)))))$col_header[[1]]
  expect_identical(vapply(got, function(z) z$label, ""), c("left", "right"))
  one <- first_page(suppressMessages(plan_apply(plan_col_header(
    edge_plan(), data.frame(line = 1, cols = c("group", "label", "TRT = Placebo"),
                            text = c("g", "l", "P"),
                            stringsAsFactors = FALSE)))))$col_header[[1]]
  expect_identical(one[1:3], c("g", "l", "P"))
})

test_that("a header cell's typed fields say what was wrong", {
  hc <- rtfreporter:::.plan_header_cell
  row <- function(...) data.frame(..., stringsAsFactors = FALSE)
  expect_error(hc(row(line = "x")), "`col_header\\$line` must be a whole number")
  expect_error(hc(row(line = 1.5)), "must be a whole number")
  expect_error(hc(row(bold = "maybe")), "`col_header\\$bold` must be TRUE or FALSE")
  expect_identical(hc(row(bold = "No"))$bold, FALSE)
  expect_identical(hc(row(bold = TRUE))$bold, TRUE)
  expect_identical(hc(row(text = "'quoted'"))$text, "quoted")
  expect_identical(hc(row(text = "a\\nb"))$text, "a\nb")
  expect_identical(hc(row(text = NA_character_, align = "left")), list(align = "left"))
})

test_that("plan_layers() describes the cells and the header a plan resolved", {
  skip_if_not_installed("cards")
  expect_error(plan_layers(data.frame()), "Expected a table_plan")

  none <- suppressMessages(plan_layers(edge_plan()))
  expect_null(none$header$source)
  expect_identical(none$declared, c("cell_options", "cells"))
  expect_identical(none$columns$names[1:2], c("group", "label"))
  expect_true(all(c("data", "roles", "layers", "pages") %in% names(none)))

  one_key <- plan_col_header(edge_plan(), rtf_col_header(
    list(col_cell(1L, ""), col_cell(2L, ""), col_cell(c(3L, 5L), "All arms")),
    c("", "", "{col}")))
  r1 <- suppressMessages(plan_layers(one_key))$header
  expect_identical(r1$source, "resolved")
  expect_identical(r1$cells$cols[r1$cells$text %in% "All arms"], ".values")
  expect_true("{col}" %in% r1$cells$text)

  # two keys: a spanner over each pair, its level named as {col1}
  p2 <- edge_plan(c("TRT", "SEX"))
  two <- plan_col_header(p2, rtf_col_header(
    list(col_cell(1L, ""), col_cell(2L, ""),
         col_cell(c(3L, 4L), "Placebo", align = "center", bold = TRUE,
                  border = rtf_border(bottom = TRUE)),
         col_cell(c(5L, 6L), "Xanomeline High Dose"),
         col_cell(c(7L, 8L), "Xanomeline Low Dose")),
    c("", "", "F", "M", "F", "M", "F", "M")))
  h2 <- suppressMessages(plan_layers(two))$header$cells
  sp <- h2[h2$line == "1" & grepl("Placebo", h2$cols, fixed = TRUE), ]
  expect_identical(sp$text, "{col1}")
  expect_identical(sp$align, "center")
  expect_identical(sp$bold, "TRUE")
  expect_identical(sp$border_bottom, "single")

  # one spanner over every value column: it names the first key
  all_cols <- plan_col_header(p2, rtf_col_header(
    list(col_cell(1L, ""), col_cell(2L, ""), col_cell(c(3L, 8L), "All")),
    rep(c("", "{col1}"), c(2, 6))))
  h3 <- suppressMessages(plan_layers(all_cols))$header$cells
  expect_true(all(c("All", "{col1}") %in% h3$text))

  # a short row repeats over the spread columns
  short <- plan_col_header(p2, rtf_col_header(c("A", "B", "{col2}")))
  h4 <- suppressMessages(plan_layers(short))$header$cells
  expect_identical(h4$text, c("A", "B", "{col2}"))
  expect_identical(h4$span[3], "each")
})

test_that("plan_layers() reads a plan whose cells are a named map", {
  skip_if_not_installed("cards")
  p <- edge_plan()
  lay <- suppressMessages(plan_layers(p))
  expect_true(is.list(lay$cells))
  expect_true(all(vapply(lay$cells, function(z)
    all(c("variable", "context", "entry") %in% names(z)), NA)))
})

# ── listings: validation, printing, empty / one-row data ───────────────────

test_that("listing_col() and listing_spec() refuse malformed arguments", {
  expect_error(listing_col("a", sep = c(",", ";")), "`sep` must be a single string")
  expect_error(listing_col("a", name = ""), "`name` must be a single non-empty string")
  expect_error(listing_col("a", name = c("x", "y")), "`name` must be a single")
  expect_error(listing_col(character(0)), "`vars` must be one or more")
  expect_error(listing_col(c("a", NA)), "`vars` must be one or more")
  expect_error(listing_col("a", width = 0), "`width` must be a single positive")
  expect_error(listing_col("a", rel_width = -1), "`rel_width` must be a single positive")
  expect_error(listing_col("a", label = NA_character_), "`label` must be a string")
  expect_error(listing_col("a", collapse_repeats = NA), "`collapse_repeats` must be TRUE or FALSE")
  expect_error(listing_col("a", align = "middle"), "should be one of")

  col <- listing_col("a")
  expect_error(listing_spec(), "`cols` is required")
  expect_error(listing_spec(NULL), "`cols` is required")
  expect_error(listing_spec(list()), "`cols` must be a non-empty list")
  expect_error(listing_spec(list(1)), "`cols\\[\\[1\\]\\]` must be a listing_col")
  expect_error(listing_spec(col, type = c("a", "b")), "`type` must be a single listing-template name")
  expect_error(listing_spec(col, spacer = "yes"), "`spacer` must be TRUE or FALSE")
  expect_error(listing_spec(col, blank_row = NA), "`blank_row` must be TRUE or FALSE")
  expect_error(listing_spec(col, spacer_rel_width = 0), "`spacer_rel_width` must be a single positive")
  expect_error(listing_spec(col, wrap = "not a function"), "`wrap` must be a function")
  expect_error(listing_spec(col, wrap = function(text, width) text), "must accept four arguments")
  expect_error(listing_spec(col, record = 3), "`record` must be TRUE, FALSE, or a single column name")
  # a single column object and bare names are both accepted
  expect_identical(length(listing_spec(col)$cols), 1L)
  expect_identical(vapply(listing_spec(c("a", "b"))$cols, function(z) z$name, ""),
                   c("a", "b"))
  # two columns built from the same variable get distinct names
  twin <- listing_spec(list(listing_col("a"), listing_col("a")))
  expect_identical(vapply(twin$cols, function(z) z$name, ""), c("a", "a_1"))
  expect_null(listing_spec(col, record = FALSE)$record_col)
  expect_identical(listing_spec(col, record = "rec")$record_col, "rec")
})

test_that("listing specs and columns print what they will do", {
  sp <- listing_spec(list(listing_col("USUBJID", label = c("Subject", "ID")),
                          listing_col(c("AGE", "TERM"), width = 20, sep = " / ")))
  out <- capture.output(res <- print(sp))
  expect_identical(res, sp)
  expect_identical(out[1], "<rtf_listing_spec>")
  expect_true(any(grepl("columns : 2 \\(\\+ gutters\\)", out)))
  expect_true(any(grepl("AGE  <- AGE / TERM  \\[wrap 20\\]", out)))
  expect_true(any(grepl("record  : .rtf_record", out)))
  nrec <- capture.output(print(listing_spec("a", record = FALSE)))
  expect_true(any(grepl("record  : \\(none\\)", nrec)))

  cl <- capture.output(print(sp$cols[[1]]))
  expect_identical(cl[1], "<rtf_listing_col>")
  expect_true(any(grepl("width : \\(no wrap\\)", cl)))
  expect_true(any(grepl("label : Subject / ID", cl)))
  expect_false(any(grepl("label", capture.output(print(listing_col("z"))))))

  d <- data.frame(USUBJID = c("S1", "S2"), AGE = c(30, NA),
                  TERM = c("a very long adverse event term that wraps", NA),
                  stringsAsFactors = FALSE)
  fitted <- fit_listing_widths(d, sp, total_width = 60)
  fo <- capture.output(print(fitted))
  expect_true(any(grepl("fitted  : \\d+ \\+ 1 gutter of 60 characters", fo)))
  expect_true(any(grepl("USUBJID +width = +\\d+ fit", fo)))
  expect_true(any(grepl("AGE +width = +20 set", fo)))
  expect_true(any(grepl("Paste listing_code\\(spec\\)", fo)))
})

test_that("build_listing() copes with empty, one-row and missing data and says what it refuses", {
  sp <- listing_spec(list(listing_col("USUBJID"),
                          listing_col(c("AGE", "TERM"), width = 20, sep = " / ")))
  d <- data.frame(USUBJID = c("S1", "S2"), AGE = c(30, NA),
                  TERM = c("a very long adverse event term that wraps", NA),
                  stringsAsFactors = FALSE)
  empty <- build_listing(d[0, ], sp)
  expect_identical(nrow(empty), 0L)
  expect_true(".rtf_record" %in% names(empty))
  expect_identical(names(empty), names(build_listing(d, sp)))

  one <- build_listing(d[1, ], sp)
  expect_true(all(one$.rtf_record == 1L))
  expect_true(nrow(one) > 1L)                       # the long term wrapped
  both <- build_listing(d, sp)
  expect_identical(sort(unique(both$.rtf_record)), 1:2)

  expect_error(build_listing(list(), sp), "`data` must be a data.frame or tibble")
  expect_error(build_listing(d, list()), "`spec` must be a listing_spec")
  expect_error(build_listing(both, sp), "already been through build_listing")
  clash <- listing_spec(list(listing_col("USUBJID")), record = "USUBJID")
  expect_error(build_listing(d, clash), "collides with a printed column")
})

# ── assembling a folder: what it refuses, what it writes ───────────────────

edge_tfl_dir <- function() {
  dir <- tempfile("tfl"); dir.create(dir)
  for (t in c("14.1.1", "14.2.1")) {
    doc <- rtf_document() |>
      rtf_tables(data.frame(Parameter = "Age", Value = "75.1")) |>
      rtf_titles(list(c(paste("Table", t), "Safety Population")))
    generate_rtfreport(doc, file.path(dir, paste0("t", gsub(".", "_", t, fixed = TRUE),
                                                   ".rtf")), overwrite = TRUE)
  }
  dir
}

test_that("assemble_folder() names a missing or empty folder and a bad spec file", {
  expect_error(assemble_folder(file.path(tempdir(), "no-such-dir")),
               "Directory not found")
  empty <- tempfile("empty"); dir.create(empty)
  on.exit(unlink(empty, recursive = TRUE), add = TRUE)
  expect_error(assemble_folder(empty), "No RTF files found")

  dir <- edge_tfl_dir(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  expect_error(assemble_folder(dir, spec_file = file.path(dir, "toc.txt")),
               "`spec_file` must end in .xlsx or .csv")
  csv <- file.path(dir, "toc.csv")
  toc <- assemble_folder(dir, spec_file = csv)
  expect_true(file.exists(csv))
  expect_identical(nrow(toc), 2L)
  expect_identical(utils::read.csv(csv)$file, unname(toc$file))
  # the folder scan ignores the .csv it just wrote and is in natural order
  expect_identical(basename(toc$file), c("t14_1_1.rtf", "t14_2_1.rtf"))
})

test_that("assemble_folder() writes an .xlsx table of contents when writexl is there", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  dir <- edge_tfl_dir(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  xlsx <- file.path(dir, "toc.xlsx")
  toc <- assemble_folder(dir, spec_file = xlsx)
  expect_true(file.exists(xlsx))
  expect_identical(readxl::read_xlsx(xlsx)$file, unname(toc$file))
})

test_that("assemble_rtf(toc = ) checks the table before it assembles", {
  dir <- edge_tfl_dir(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  toc <- assemble_folder(dir)
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)

  gone <- toc; gone$file[1] <- file.path(dir, "gone.rtf")
  expect_error(assemble_rtf(toc = gone, output_file = out),
               "names missing file\\(s\\):")
  expect_error(assemble_rtf(toc = toc[, c("file", "table")], output_file = out),
               "needs the columns `file` and `label`")

  got <- assemble_rtf(toc = toc, output_file = out)
  expect_identical(got, out)
  expect_true(file.exists(out))
  expect_error(assemble_rtf(toc = toc, output_file = out),
               "already exists. Set overwrite = TRUE")
  expect_identical(assemble_rtf(toc = toc, output_file = out, overwrite = TRUE), out)

  # the `order` column decides the sequence
  rev_toc <- toc; rev_toc$order <- c(2, 1)
  out2 <- tempfile(fileext = ".rtf"); on.exit(unlink(out2), add = TRUE)
  assemble_rtf(toc = rev_toc, output_file = out2)
  txt <- paste(readLines(out2, warn = FALSE), collapse = "\n")
  expect_true(regexpr("t14_2_1", txt) < regexpr("t14_1_1", txt))
})

# ── style verbs: row selection, header cells, error wording ────────────────

edge_tbl <- function() {
  d <- data.frame(a = c("x", "y", "z"), b = 1:3, c = c(1.5, 2.5, 3.5),
                  stringsAsFactors = FALSE)
  set_col_header(rtftable(d), c("A", "B", "C"))
}

test_that("style_body(rows = ) accepts every selector and names a bad one", {
  t1 <- edge_tbl()
  bold_rows <- function(x) which(vapply(x$cell_styles, function(r)
    !is.null(r$bold) && any(r$bold), NA))
  expect_identical(bold_rows(style_body(t1, rows = ~ b > 1, bold = TRUE)), 2:3)
  expect_identical(bold_rows(style_body(t1, rows = ~ TRUE, bold = TRUE)), 1:3)
  expect_identical(bold_rows(style_body(t1, rows = function(df) df$b == 2,
                                        bold = TRUE)), 2L)
  expect_identical(bold_rows(style_body(t1, rows = c(TRUE, FALSE, NA),
                                        bold = TRUE)), 1L)
  expect_identical(bold_rows(style_body(t1, rows = c(3, 1), bold = TRUE)), c(1L, 3L))

  expect_error(style_body(t1, rows = function(df) 1:2, bold = TRUE),
               "predicate must return a logical vector of length nrow\\(data\\) \\(3\\); got integer of length 2")
  expect_error(style_body(t1, rows = c(TRUE, FALSE), bold = TRUE),
               "logical vector must have length 3 .*got 2")
  expect_error(style_body(t1, rows = 9, bold = TRUE), "positions must be in 1..3")
  expect_error(style_body(t1, rows = 0, bold = TRUE), "positions must be in 1..3")
  expect_error(style_body(t1, rows = NA_integer_, bold = TRUE), "positions must be in 1..3")
  expect_error(style_body(t1, rows = "x", bold = TRUE),
               "must be NULL, row positions, a logical vector, a predicate function")
  expect_error(style_body(t1, rows = y ~ x, bold = TRUE), "formula must be one-sided")
})

test_that("style_body(rows = ) selects across the tables of a multi-table rtftable", {
  d1 <- data.frame(g = c("a", "b"), v = 1:2, stringsAsFactors = FALSE)
  d2 <- data.frame(g = c("c", "d", "e"), v = 3:5, stringsAsFactors = FALSE)
  m <- rtftable(list(d1, d2))
  out <- style_body(m, rows = ~ v %% 2 == 0, bold = TRUE)
  expect_true(inherits(out, "rtftable"))
  expect_error(style_body(m, rows = c(1, 6), bold = TRUE), "positions must be in 1..5")
})

test_that("style_header() promotes, patches and warns where it must", {
  t1 <- edge_tbl()
  no_head <- rtftable(data.frame(a = 1))
  expect_error(style_header(no_head, bold = TRUE), "this rtftable has no column header")
  expect_error(style_header(t1, row = 5, bold = TRUE),
               "`style_header\\(row = \\)` must be in 1..1")
  expect_error(style_header(t1, bold = "yes"), "bold")

  # a border on a labels row turns it into cells; the label is recycled
  b <- style_header(t1, cols = 2, border = rtf_border(bottom = TRUE), label = "Bee")
  cells <- b$col_header[[1]]
  expect_true(is.list(cells))
  expect_identical(vapply(cells, function(z) z$label, ""), c("A", "Bee", "C"))
  expect_false(is.null(cells[[2]]$border))

  # text styling on a labels row lives on the column, shared by all label rows
  two <- set_col_header(t1, rtf_col_header(c("a1", "b1", "c1"), c("a", "b", "c")))
  w <- testthat::capture_warnings(style_header(two, bold = TRUE))
  expect_true(length(w) >= 1L && all(grepl("shared by ALL label rows", w)))
  one <- style_header(t1, cols = 1, bold = TRUE, italic = TRUE, align = "left")
  expect_true(one$col_spec[[1]]$header_bold)
  expect_true(one$col_spec[[1]]$header_italic)
  expect_identical(one$col_spec[[1]]$header_align, "left")

  # a cell row: only intersecting cells change; none -> a warning, no change
  sp <- t1
  sp$col_header <- list(list(list(from = 1L, to = 1L, label = "One")))
  expect_warning(none <- style_header(sp, row = 1, cols = 3, bold = TRUE),
                 "no header cells intersect the requested columns")
  expect_identical(none$col_header, sp$col_header)
  hit <- style_header(sp, row = 1, cols = 1, label = "Uno", bold = TRUE,
                      italic = TRUE, underline = TRUE, align = "right",
                      border = rtf_border(top = TRUE))
  cell <- hit$col_header[[1]][[1]]
  expect_identical(cell$label, "Uno")
  expect_true(cell$bold && cell$italic && cell$underline)
  expect_identical(cell$align, "right")
})

test_that("style_cols() validates and records the column's look", {
  t1 <- edge_tbl()
  expect_error(style_cols(t1, cols = "a", header_align = "middle"),
               "must be \"left\", \"center\", or \"right\"")
  out <- style_cols(t1, cols = "b", header_align = "right", header_bold = TRUE,
                    header_italic = TRUE, border = rtf_border(bottom = TRUE),
                    underline = TRUE, indent_twips = 100)
  expect_identical(out$col_spec[[2]]$header_align, "right")
  expect_true(out$col_spec[[2]]$header_bold)
  expect_true(out$col_spec[[2]]$header_italic)
  expect_false(is.null(out$col_spec[[2]]$border))
  # the other columns keep their defaults
  expect_identical(out$col_spec[[1]]$header_align, t1$col_spec[[1]]$header_align)
  expect_error(style_cols(t1, cols = "nope", bold = TRUE), "nope")
})

test_that("set_col_header() and rtf_columns() say what is wrong with their input", {
  t1 <- edge_tbl()
  expect_error(set_col_header(t1, c("A", "B")),
               "the label row has 2 labels but the table has 3 printed columns")
  expect_error(set_col_header(t1, c("A", "B", "C"), align = c("left", "right")),
               "`set_col_header\\(align = \\)` must have length 1 or 3")
  expect_error(set_col_header(t1, c("A", "B", "C"), align = "mid"),
               "values must be \"left\", \"center\", or \"right\"")
  aligned <- set_col_header(t1, c("A", "B", "C"), align = c("left", "center", "right"))
  expect_identical(aligned$col_spec[[3]]$header_align, "right")
  added <- add_header_row(t1, c("X", "Y", "Z"))
  expect_identical(added$col_header[[1]], c("X", "Y", "Z"))
  expect_identical(added$col_header[[2]], c("A", "B", "C"))

  expect_identical(rtf_columns(t1), c("a", "b", "c"))
  expect_identical(rtf_columns(list()), character(0))
  expect_identical(rtf_columns(list(t1, t1)), c("a", "b", "c"))
  expect_error(rtf_columns(list("a")), "expects rtftable pages")
  expect_error(col_cell(c(3, 1), "x"), "`pos` start must be <= end")
})

# ── rtftable overrides (what rtf_tables() applies to a built table) ────────

test_that("the table overrides are stored, coerced and checked", {
  ov <- rtfreporter:::.override_rtftable_fields
  d <- data.frame(a = c("x", "y"), b = 1:2, c = c(1.5, 2.5), stringsAsFactors = FALSE)
  t1 <- rtftable(d)
  expect_identical(ov(t1, list()), t1)

  r <- ov(t1, list(table_width_twips = 9000L, table_width_pct_of_writable = 0.8,
                   table_align = "center", col_rel_width = c(1, 2, 3),
                   column_widths_twips = c(1000, 2000, 3000),
                   row_height_twips = 300, header_row_height_twips = 200,
                   blank_row_height_twips = 100, row_height_exact = TRUE,
                   cell_padding_left_twips = 50, cell_padding_right_twips = 60,
                   cell_valign = "bottom", font_size_half_points = 16L,
                   font = "Arial"))
  expect_identical(r$table_width_twips, 9000L)
  expect_identical(r$table_width_pct_of_writable, 0.8)
  expect_identical(r$table_align, "center")
  expect_identical(r$column_widths_twips, c(1000L, 2000L, 3000L))   # integers
  expect_identical(r$row_height_twips, 300L)
  expect_identical(r$header_row_height_twips, 200L)
  expect_identical(r$blank_row_height_twips, 100L)
  expect_true(r$row_height_exact)
  expect_identical(r$cell_padding_left_twips, 50L)
  expect_identical(r$cell_padding_right_twips, 60L)
  expect_identical(r$cell_valign, "bottom")
  expect_identical(r$font, "Arial")
  # NULL clears
  cleared <- ov(r, list(column_widths_twips = NULL, row_height_twips = NULL,
                        header_row_height_twips = NULL, blank_row_height_twips = NULL,
                        cell_padding_left_twips = NULL, cell_padding_right_twips = NULL))
  expect_null(cleared$column_widths_twips)
  expect_null(cleared$row_height_twips)
  expect_null(cleared$cell_padding_right_twips)
  expect_identical(ov(t1, list(table_width_pct = 80))$table_width_pct_of_writable, 0.8)

  expect_error(ov(t1, list(table_align = "up")), "`table_align` must be 'left', 'center', or 'right'")
  expect_error(ov(t1, list(row_height_exact = "yes")), "`row_height_exact` must be TRUE or FALSE")
  expect_error(ov(t1, list(row_height_exact = c(TRUE, FALSE))), "`row_height_exact` must be TRUE or FALSE")
  expect_error(ov(t1, list(cell_valign = "mid")), "`cell_valign` must be 'top', 'center', or 'bottom'")
})

test_that("overriding a header, blank rows, row titles and column specs", {
  ov <- rtfreporter:::.override_rtftable_fields
  d <- data.frame(a = c("x", "y"), b = 1:2, c = c(1.5, 2.5), stringsAsFactors = FALSE)
  t1 <- rtftable(d)
  m <- rtftable(list(d, d))

  # a border and a header in the same call: the border sees the header
  r <- ov(t1, list(border = "tfl", col_header = c("A", "B", "C")))
  expect_identical(r$col_header, list(c("A", "B", "C")))
  expect_false(is.null(r$border$header))

  expect_identical(ov(t1, list(blank_rows = 2L))$blank_rows, 2L)
  expect_null(ov(t1, list(blank_rows = NULL))$blank_rows)
  multi <- ov(m, list(col_header = c("A", "B", "C"), blank_rows = c(2, 1, 2)))
  expect_length(multi$col_header_list, 2L)
  expect_identical(multi$blank_rows, c(1L, 2L))               # sorted, unique
  expect_error(ov(m, list(col_header = list(1))),
               "Multi-row col_header must contain only character vectors and")
  # once a session: start from a clean slate, and put the state back
  st <- rtfreporter:::.deprecation_state
  saved <- as.list(st)
  rm(list = ls(st), envir = st)
  on.exit({ rm(list = ls(st), envir = st); list2env(saved, envir = st) }, add = TRUE)
  expect_warning(ov(t1, list(spanning_header = list(list(from = 1, to = 2, label = "S")))),
                 "`spanning_header =` is deprecated")

  # row titles re-seed only the columns still at their default alignment
  rt <- ov(t1, list(row_title = 2L))
  expect_identical(rt$row_title, 2L)
  expect_identical(vapply(rt$col_spec, function(s) s$align, ""),
                   c("center", "left", "center"))

  expect_error(ov(t1, list(col_spec = list(list(bold = TRUE)))),
               "Each element of `col_spec` must be a list with a `col` key")
  cs <- ov(t1, list(col_spec = list(list(col = "b", align = "right"),
                                    list(col = "nope", bold = TRUE),
                                    list(col = 9, bold = TRUE))))
  expect_identical(cs$col_spec[[2]]$align, "right")
  expect_identical(cs$col_spec[[2]]$header_align, "right")    # follows the data
  expect_identical(vapply(ov(t1, list(col_header_align = "left"))$col_spec,
                          function(s) s$header_align, ""), rep("left", 3))
  expect_identical(vapply(ov(t1, list(col_header_align = c("left", "center", "right")))$col_spec,
                          function(s) s$header_align, ""), c("left", "center", "right"))
})

# ── rendering: multi-table pages, section templates, widths, empty tables ──

edge_render <- function(doc, ...) {
  f <- tempfile(fileext = ".rtf"); on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE, ...)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

test_that("a multi-table rtftable renders every constituent table with its header", {
  d1 <- data.frame(a = c("x1", "x2"), b = 1:2, stringsAsFactors = FALSE)
  d2 <- data.frame(a = "y1", b = 3L, stringsAsFactors = FALSE)
  tbl <- rtftable(list(d1, d2), col_header = c("AAA", "BBB"))
  txt <- edge_render(rtf_document() |> rtf_tables(tbl))
  expect_true(all(vapply(c("x1", "x2", "y1", "AAA", "BBB"), grepl, NA, x = txt,
                         fixed = TRUE)))
  expect_gte(lengths(regmatches(txt, gregexpr("AAA", txt, fixed = TRUE))), 2L)
})

test_that("section templates, per-page sections and watermarks reach the file", {
  d <- data.frame(a = c("x", "y"), b = 1:2, stringsAsFactors = FALSE)
  hdr <- function(s) rtf_header(rows = list(c(l = s)))
  doc <- rtf_document() |>
    rtf_section(page = NULL, secinfo = list(header = hdr("StudyHdr"),
                                            watermark = rtf_watermark("DRAFTMARK"))) |>
    rtf_section(page = 2, secinfo = list(header = hdr("SecondHdr"),
                                         watermark = NULL)) |>
    rtf_tables(list("Table A" = d, "Table B" = d), auto_section = TRUE)
  txt <- edge_render(doc)
  expect_match(txt, "StudyHdr", fixed = TRUE)
  expect_match(txt, "SecondHdr", fixed = TRUE)
  expect_match(txt, "DRAFTMARK", fixed = TRUE)
  # the explicit page-2 section wins over the auto section of page 2 (#548):
  # two sections, one per page (before, a third repeated pages 1 and 2)
  expect_false(grepl("Table B", txt, fixed = TRUE))
  expect_identical(lengths(regmatches(txt, gregexpr("sectd", txt, fixed = TRUE))), 2L)

  # auto sections without any template, and a template with plain content
  expect_match(edge_render(rtf_document() |>
                             rtf_tables(list("OnlyOne" = d), auto_section = TRUE)),
               "OnlyOne", fixed = TRUE)
  plain <- rtf_document() |>
    rtf_section(page = NULL, secinfo = list(header = hdr("TemplateOnly"))) |>
    rtf_tables(list(d))
  expect_match(edge_render(plain), "TemplateOnly", fixed = TRUE)
})

test_that("a table's own width settings size the title block and the cells", {
  d <- data.frame(a = c("x", "y"), b = 1:2, stringsAsFactors = FALSE)
  doc <- function(tbl) rtf_document() |> rtf_tables(tbl) |>
    rtf_titles(list("A title")) |> rtf_footnotes(list("A footnote"))
  fixed <- edge_render(doc(rtftable(d, column_widths_twips = c(2000, 2000))))
  expect_match(fixed, "\\cellx2000", fixed = TRUE)
  expect_match(fixed, "\\cellx4000", fixed = TRUE)
  total <- edge_render(doc(rtftable(d, table_width_twips = 5000L)))
  expect_match(total, "\\cellx5000", fixed = TRUE)
  half <- edge_render(doc(rtftable(d, table_width_pct = 50)))
  expect_false(identical(half, total))
  expect_match(half, "A footnote", fixed = TRUE)
})

test_that("empty tables and tables with no columns still produce a file", {
  d <- data.frame(a = c("x", "y"), b = 1:2, stringsAsFactors = FALSE)
  no_rows <- edge_render(rtf_document() |> rtf_tables(rtftable(d[0, ])))
  expect_match(no_rows, "^\\{\\\\rtf1")
  no_cols <- edge_render(rtf_document() |> rtf_tables(rtftable(d[, 0])))
  expect_match(no_cols, "^\\{\\\\rtf1")
  one_row <- edge_render(rtf_document() |> rtf_tables(rtftable(d[1, ])))
  expect_match(one_row, "x", fixed = TRUE)
  one_col <- edge_render(rtf_document() |> rtf_tables(rtftable(d["a"])))
  expect_false(grepl("\\cellx3", one_col, fixed = TRUE))
})

test_that("`program` is one string, or it is refused", {
  d <- data.frame(a = "x", stringsAsFactors = FALSE)
  doc <- rtf_document() |> rtf_tables(list(d))
  expect_error(generate_rtfreport(doc, tempfile(fileext = ".rtf"),
                                  program = c("a", "b")),
               "`program` must be a single string")
  expect_error(generate_rtfreport(doc, tempfile(fileext = ".rtf"),
                                  program = NA_character_),
               "`program` must be a single string")
  expect_identical(rtfreporter:::.resolve_program("my_script.R"), "my_script.R")
})

test_that("print.rtf_document() summarises pages and sections", {
  d <- data.frame(a = "x", stringsAsFactors = FALSE)
  doc <- rtf_document() |> rtf_tables(list(d, d))
  out <- capture.output(res <- print(doc))
  expect_identical(res, doc)
  expect_identical(out[1:3], c("rtf_document object", "  Pages: 2 ",
                               "  Sections defined: 0 "))
  # the size line is there; its wording for a preset page is #547's business
  expect_match(out[4], "Document page size:")
  sized <- capture.output(print(rtf_document(page = rtf_page(width_in = 8.5,
                                                              height_in = 11))))
  expect_match(sized[4], "8.5 x 11 inches")
})

# ── figures: image headers read by hand, plot objects, refusals ───────────

# Bytes of a minimal JPEG: SOI, an optional JFIF APP0, an optional extra
# segment, SOF0 (height 7, width 9), EOI.
edge_jpeg <- function(units = NULL, xd = 300L, yd = 300L, extra = NULL,
                      rst = FALSE) {
  be16 <- function(v) as.raw(c(v %/% 256L, v %% 256L))
  app0 <- if (is.null(units)) raw(0) else
    c(as.raw(c(0xFF, 0xE0)), be16(16L), charToRaw("JFIF"), as.raw(0),
      as.raw(c(1, 1)), as.raw(units), be16(xd), be16(yd), as.raw(c(0, 0)))
  sof <- c(as.raw(c(0xFF, 0xC0)), be16(17L), as.raw(8), be16(7L), be16(9L),
           as.raw(c(3, 1, 0x22, 0, 2, 0x11, 1, 3, 0x11, 1)))
  c(as.raw(c(0xFF, 0xD8)), if (rst) as.raw(c(0xFF, 0xD3)) else raw(0), extra,
    app0, sof, as.raw(c(0xFF, 0xD9)))
}

edge_write <- function(bytes, ext) {
  f <- tempfile(fileext = ext)
  writeBin(bytes, f)
  f
}

test_that("JPEG density comes from the JFIF segment, in dpi or dots per cm", {
  rd <- rtfreporter:::.read_jpeg_density
  f1 <- edge_write(edge_jpeg(units = 1L), ".jpg")
  f2 <- edge_write(edge_jpeg(units = 2L, xd = 100L, yd = 200L), ".jpg")
  f0 <- edge_write(edge_jpeg(units = 0L), ".jpg")
  f_none <- edge_write(edge_jpeg(units = NULL), ".jpg")
  f_rst <- edge_write(edge_jpeg(units = 1L, rst = TRUE), ".jpg")
  on.exit(unlink(c(f1, f2, f0, f_none, f_rst)), add = TRUE)
  expect_identical(rd(f1), list(dpi_x = 300L, dpi_y = 300L))
  expect_equal(rd(f2), list(dpi_x = 254, dpi_y = 508))
  expect_null(rd(f0))                       # units = 0: an aspect ratio only
  expect_null(rd(f_none))                   # no JFIF segment at all
  expect_identical(rd(f_rst), list(dpi_x = 300L, dpi_y = 300L))
  # another APP segment before the JFIF one is skipped by its length
  exif <- c(as.raw(c(0xFF, 0xE1, 0x00, 0x06)), as.raw(1:4))
  f_exif <- edge_write(edge_jpeg(units = NULL, extra = exif), ".jpg")
  on.exit(unlink(f_exif), add = TRUE)
  expect_null(rd(f_exif))

  dims <- rtfreporter:::.read_jpeg_dims(f1)
  expect_identical(dims, list(width = 9L, height = 7L))
  bad <- edge_write(as.raw(c(0xFF, 0xD8, 0xFF, 0xD9, 1, 2, 3, 4, 5, 6)), ".jpg")
  on.exit(unlink(bad), add = TRUE)
  expect_error(rtfreporter:::.read_jpeg_dims(bad), "Could not locate SOF marker")
})

edge_png <- function(phys = NULL, truncated = FALSE) {
  be32 <- function(v) as.raw(c(v %/% 16777216, (v %/% 65536) %% 256,
                               (v %/% 256) %% 256, v %% 256))
  chunk <- function(type, data) c(be32(length(data)), charToRaw(type), data,
                                  as.raw(c(0, 0, 0, 0)))
  ihdr <- chunk("IHDR", c(be32(4L), be32(3L), as.raw(c(8, 2, 0, 0, 0))))
  p <- if (is.null(phys)) raw(0) else if (truncated) {
    c(be32(9L), charToRaw("pHYs"), be32(1L))             # chunk cut short
  } else chunk("pHYs", c(be32(phys[1]), be32(phys[2]), as.raw(phys[3])))
  c(as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A)), ihdr,
    chunk("tEXt", c(charToRaw("Comment"), as.raw(0), charToRaw("hi"))), p, chunk("IDAT", as.raw(0)),
    chunk("IEND", raw(0)))
}

test_that("PNG density comes from the pHYs chunk when it counts dots per metre", {
  rd <- rtfreporter:::.read_png_density
  per_inch <- edge_write(edge_png(c(11811L, 11811L, 1L)), ".png")
  unitless <- edge_write(edge_png(c(1L, 1L, 0L)), ".png")
  zero <- edge_write(edge_png(c(0L, 5L, 1L)), ".png")
  no_phys <- edge_write(edge_png(NULL), ".png")
  short <- edge_write(edge_png(1L, truncated = TRUE), ".png")
  on.exit(unlink(c(per_inch, unitless, zero, no_phys, short)), add = TRUE)
  expect_equal(rd(per_inch), list(dpi_x = 11811 * 0.0254, dpi_y = 11811 * 0.0254))
  expect_null(rd(unitless))
  expect_null(rd(zero))
  expect_null(rd(no_phys))
  expect_null(rd(short))
  expect_identical(rtfreporter:::.read_png_dims(per_inch), list(width = 4L, height = 3L))
})

test_that("plot objects are drawn into a PNG, and the options are checked", {
  skip_if_not(isTRUE(unname(capabilities("png"))))
  # a recorded base plot is replayed
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 200, height = 200)
  grDevices::dev.control("enable")          # a file device keeps no display list otherwise
  graphics::plot(1:3)
  rec <- grDevices::recordPlot()
  grDevices::dev.off()
  unlink(tmp)
  fig <- rtfplot(rec, render_width = 2, render_height = 2, render_dpi = 100)
  expect_s3_class(fig, "rtfplot")
  expect_identical(fig$dpi_x, 100)
  expect_identical(c(fig$img_width, fig$img_height), c(200L, 200L))
  # a function that draws is called
  fn <- rtfplot(function() { graphics::par(mar = c(0, 0, 0, 0)); graphics::plot(1) },
                render_width = 1, render_height = 1, render_dpi = 72)
  expect_identical(fn$dpi_x, 72)

  expect_error(rtfplot(function() NULL, render_width = 0), "`render_width` must be a single positive number")
  expect_error(rtfplot(function() NULL, render_dpi = NA), "`render_dpi` must be a single positive number")
  png <- edge_write(edge_png(), ".png")
  on.exit(unlink(png), add = TRUE)
  expect_error(rtfplot(png, render_dpi = 150),
               "say how to draw a plot object; a file already has a size")
  expect_error(rtfplot(png, align = "middle"), "`align` must be 'left', 'center', or 'right'")
  expect_error(rtfplot(file.path(tempdir(), "missing.png")), "Image file not found")
  gif <- edge_write(as.raw(1:10), ".gif")
  on.exit(unlink(gif), add = TRUE)
  expect_error(rtfplot(gif), "supports PNG and JPEG files only")
})

test_that("a plot object needs a PNG device", {
  testthat::local_mocked_bindings(capabilities = function(...) c(png = FALSE),
                                  .package = "base")
  expect_error(rtfplot(function() NULL), "This R has no PNG device")
})

# ── ARD templates: format specs, guards, cell_rows(), the pipe option ──────

test_that("a format spec reads stat, stat_fmt or formats the number itself", {
  fv <- rtfreporter:::.ard_format_value
  expect_true(is.na(fv(NA, NA, "stat", "r")))
  expect_identical(fv("a", NA, "stat", "r"), "a")
  expect_true(is.na(fv(NA, NA, "stat_fmt", "r")))
  expect_identical(fv(3, "3.0", "stat_fmt", "r"), "3.0")
  expect_error(fv(3, NA, "stat_fmt", "r"), "`\\{x:stat_fmt\\}` was asked for, but this ARD has no `stat_fmt`")
  # a bare token prefers the formatted value, then the raw one, then NA
  expect_identical(fv(3, "3.0", "", "r"), "3.0")
  expect_identical(fv(3, NA, "", "r"), "3")
  expect_true(is.na(fv(NA, NA, "", "r")))
  # the superseded spellings say what replaced them
  expect_error(fv(1, NA, "raw", "r"), "Format spec 'raw' has been renamed: write 'stat'")
  expect_error(fv(1, NA, "fmt", "r"), "'fmt' has been renamed: write 'stat_fmt'")
  # numbers: decimals, significant digits, integers, percentages
  expect_true(is.na(fv("abc", NA, ".1f", "r")))
  expect_identical(fv(0.256, NA, ".1f%", "r"), "25.6")
  expect_identical(fv(1234.5, NA, ".3s", "r"), "1234")
  expect_identical(fv(2.5, NA, "d", "r"), "2")        # R rounds half to even
  expect_identical(fv(2.5, NA, "d", "sas"), "3")      # SAS rounds half up
  expect_true(is.na(fv(NA_real_, NA, ".2s", "r")))
  expect_error(fv(1, NA, ".1x", "r"), "Unknown format spec '.1x'")
  expect_identical(rtfreporter:::.ard_signif_fmt(c(1234.5, NA, 0.012345), 3, "r"),
                   c("1234", NA, "0.0123"))
})

test_that("guards need both sides and a template string", {
  el <- rtfreporter:::.ard_chain_el
  expect_error(el(~ "x"), "A `cells` guard needs both sides")
  one_sided <- el(~ "x", one_sided = TRUE)
  expect_null(one_sided$cond)
  expect_identical(one_sided$tpl, "x")
  expect_error(el(n > 1 ~ 3), "The right of a `cells` guard must be one template string")
  expect_error(el(~ 3, one_sided = TRUE), "A one-sided `~` must hold one template string")
  guard <- el(n == 0 ~ "0")
  expect_identical(deparse(guard$cond), "n == 0")
  expect_identical(guard$tpl, "0")
  expect_identical(el("{n}")$tpl, "{n}")
})

test_that("cell_rows() needs a row and prints each chain", {
  expect_error(cell_rows(), "`cell_rows\\(\\)` needs at least one row")
  cr <- cell_rows("a" = "{n}", "b" = c(n == 0 ~ "0", "{n:.1f}"))
  expect_s3_class(cr, "cell_rows")
  out <- capture.output(res <- print(cr))
  expect_identical(res, cr)
  expect_identical(out[1], "<cell_rows> 2 row(s)")
  expect_match(out[2], "^  a +\\{n\\}$")
  expect_match(out[3], "^  b +n == 0 ~ \"0\"  \\|  \\{n:.1f\\}$")
  # an unnamed row takes its label from `label`
  un <- cell_rows("{x}", "lab" = "{n}")
  expect_identical(names(un), c("", "lab"))
  expect_match(capture.output(print(un))[2], "\\(from `label`\\) +\\{x\\}")
})

test_that("the generated code's pipe is chosen by argument or option", {
  op <- rtfreporter:::.ard_pipe_op
  expect_error(op("&&"), "`pipe` must be \"%>%\" \\(magrittr\\), \"\\|>\"")
  expect_error(op(c("%>%", "|>")), "`pipe` must be")
  expect_identical(op("|>"), "|>")
  expect_identical(op("%>%"), "%>%")
  old <- options(rtfreporter.ard_pipe = "|>"); on.exit(options(old), add = TRUE)
  expect_identical(op(), "|>")
  options(rtfreporter.ard_pipe = "bad")
  expect_error(op(), "`pipe` must be")
  options(rtfreporter.ard_pipe = NULL)
  # outside RStudio: silent as the default, said aloud when it was asked for
  skip_if(!is.null(rtfreporter:::.ard_pipe_rstudio()))
  expect_identical(op(), "%>%")
  expect_message(got <- op("rstudio"), "no RStudio preference to read")
  expect_identical(got, "%>%")
})

# ── deprecated arguments warn exactly once a session ───────────────────────

test_that("deprecated arguments and the border-reading change warn once, not every call", {
  st <- rtfreporter:::.deprecation_state
  saved <- as.list(st)
  reset <- function() rm(list = ls(st), envir = st)
  on.exit({ reset(); list2env(saved, envir = st) }, add = TRUE)   # leave it as found
  n_warnings <- function(expr) {
    n <- 0L
    withCallingHandlers(force(expr), warning = function(w) {
      n <<- n + 1L; invokeRestart("muffleWarning")
    })
    n
  }
  d <- data.frame(g = c("a", "a", "b"), v = c("x", "y", "z"), n = 1:3,
                  stringsAsFactors = FALSE)

  reset()
  expect_identical(n_warnings({
    add_cont_label(d, "G", chunk = d)
    add_cont_label(d, "G", chunk = d)
  }), 1L)
  reset()
  expect_warning(add_cont_label(d, "G", chunk = d), "`add_cont_label\\(chunk = \\)` is deprecated")

  reset()
  expect_identical(n_warnings({
    as_rtftables(d, stub_vars = c("g", "v"))
    as_rtftables(d, stub_vars = c("g", "v"))
  }), 1L)

  # the 0.5.0 reading of left/right: said once, and only where the readings differ
  local_deprecated("rtf_table_border")
  tb <- rtf_table_border(body = rtf_border(left = TRUE))
  reset(); local_deprecated("rtf_table_border")
  expect_identical(n_warnings({
    rtfreporter:::.warn_old_edge_reading(tb, 3L, 2L)
    rtfreporter:::.warn_old_edge_reading(tb, 3L, 2L)
  }), 1L)
  reset()
  expect_identical(n_warnings(rtfreporter:::.warn_old_edge_reading(tb, 1L, 2L)), 0L)
  expect_identical(n_warnings(rtfreporter:::.warn_old_edge_reading(NULL, 3L, 2L)), 0L)
  expect_false(rtfreporter:::.warn_old_edge_reading(NULL, 3L, 2L))
  # naming the interior rule silences it
  local_deprecated("rtf_table_border")
  quiet <- rtf_table_border(body = rtf_border(left = TRUE, inside_v = TRUE))
  expect_identical(n_warnings(rtfreporter:::.warn_old_edge_reading(quiet, 3L, 2L)), 0L)
})

# ── rtf_header_source(): the code that rebuilds a header ───────────────────

test_that("rtf_header_source() refuses what is not a table and a bad stub", {
  msg <- "`rtf_header_source\\(\\)` expects an rtftable or a list of rtftable pages"
  expect_error(rtf_header_source(1), msg)
  expect_error(rtf_header_source(list(1)), msg)
  expect_error(rtf_header_source(list()), msg)
  t1 <- rtftable(data.frame(a = 1, b = 2))
  expect_error(rtf_header_source(t1, add_span_level = TRUE, stub = 9),
               "`stub` must be valid column name\\(s\\) or position\\(s\\) in 1..2")
  expect_error(rtf_header_source(t1, add_span_level = TRUE, stub = "zz"),
               "`stub` must be valid column name")
})

test_that("rtf_header_source() writes cells, borders and levels of detail", {
  d <- data.frame(a = c("x", "y"), b = 1:2, c = c(1.5, 2.5), stringsAsFactors = FALSE)
  red <- rtf_border(bottom = rtf_border_line("double", 30L, color = "#FF0000"))
  t1 <- rtftable(d, col_header = rtf_col_header(
    list(col_cell(1, "One", align = "right", bold = TRUE, italic = TRUE,
                  underline = TRUE, border = red),
         col_cell(c(2, 3), "Two")),
    c("a", "b", "c"))) |> style_zone(header = rtf_border(bottom = TRUE))

  explicit <- rtf_header_source(t1)
  expect_match(explicit, "^set_col_header\\(")
  expect_match(explicit, "col_cell(\"a\", \"One\", align = \"right\", bold = TRUE, italic = TRUE, underline = TRUE",
               fixed = TRUE)
  expect_match(explicit, "rtf_border_line(\"double\", 30, color = \"#FF0000\")", fixed = TRUE)
  expect_match(explicit, "col_cell(c(\"b\", \"c\"), \"Two\")", fixed = TRUE)
  expect_match(explicit, "style_zone(header = rtf_border(", fixed = TRUE)

  all_lvl <- rtf_header_source(t1, level = "all")
  expect_match(all_lvl, "col_cell(c(\"b\", \"c\"), \"Two\", align = \"center\", bold = FALSE",
               fixed = TRUE)
  expect_match(all_lvl, "align = c(\"left\", \"center\", \"center\")", fixed = TRUE)
  expect_match(all_lvl, "rtf_border_line(\"single\", 15)", fixed = TRUE)

  bare <- rtf_header_source(t1, snippet = FALSE, level = "default")
  expect_match(bare, "^rtf_col_header\\(")
  expect_false(grepl("set_col_header", bare))
  expect_match(bare, "\"Two\", align = \"center\"", fixed = TRUE)

  scaffold <- rtf_header_source(t1, snippet = FALSE, add_span_level = TRUE, stub = "a")
  expect_match(scaffold, "col_cell(\"a\", \"\"), col_cell(c(\"b\", \"c\"), \"\")", fixed = TRUE)
  # the original rows follow the scaffold row
  expect_gt(regexpr("\"One\"", scaffold), regexpr("col_cell(c(\"b\", \"c\"), \"\")", scaffold, fixed = TRUE))
})

test_that("rtf_header_source() handles tables without an explicit header or with header_align", {
  d <- data.frame(a = c("x", "y"), b = 1:2, c = c(1.5, 2.5), stringsAsFactors = FALSE)
  # no header: the column names are the one label row
  none <- rtf_header_source(rtftable(d), snippet = FALSE)
  expect_match(none, "c(a = \"a\", b = \"b\", c = \"c\")", fixed = TRUE)
  # a multi-table rtftable shows its first table's header
  multi <- rtf_header_source(rtftable(list(d, d), col_header = c("A", "B", "C")),
                             snippet = FALSE)
  expect_match(multi, "c(a = \"A\", b = \"B\", c = \"C\")", fixed = TRUE)
  # a header alignment that differs from the data alignment is written out
  al <- rtf_header_source(rtftable(d, col_header = c("A", "B", "C"),
                                   col_header_align = "left"))
  expect_match(al, "align = c(\"left\", \"left\", \"left\")", fixed = TRUE)
  # a list of pages reads the first page
  pages <- as_rtftables(d)
  expect_match(rtf_header_source(pages, snippet = FALSE), "^rtf_col_header\\(")
})

# ── gt adapter helpers that need no gt ─────────────────────────────────────

test_that("gt cell text is flattened, markdown bold stripped, and empties are NA", {
  fl <- rtfreporter:::.flatten_to_chr
  expect_true(is.na(fl(NULL)))
  expect_true(is.na(fl(character(0))))
  expect_true(is.na(fl(list())))
  expect_true(is.na(fl(list(NULL))))
  expect_true(is.na(fl("")))
  expect_true(is.na(fl(NA_character_)))
  expect_true(is.na(fl("****")))
  expect_identical(fl("**Bold** text"), "Bold text")
  expect_identical(fl(list("first", "second")), "first")
  expect_identical(fl(12.5), "12.5")
})

test_that("gt colours become #RRGGBB, or NA when they cannot", {
  nc <- rtfreporter:::.gt_normalize_color
  expect_true(is.na(nc(NULL)))
  expect_true(is.na(nc(c("red", "blue"))))
  expect_true(is.na(nc(NA_character_)))
  expect_true(is.na(nc("")))
  expect_identical(nc("#ff0000"), "#FF0000")
  expect_identical(nc("#ff000080"), "#FF0000")        # alpha dropped
  expect_identical(nc("red"), "#FF0000")
  expect_true(is.na(nc("not-a-colour")))
})

test_that("footnote texts are read from the gt footnote slot", {
  ft <- rtfreporter:::.extract_footnote_texts
  expect_null(ft(list(`_footnotes` = NULL)))
  expect_null(ft(list(`_footnotes` = data.frame(footnotes = I(list())))))
  one <- list(`_footnotes` = data.frame(footnotes = I(list("a note", NULL, c("two", "**parts**")))))
  expect_identical(ft(one), c("a note", "two parts"))
  empty <- list(`_footnotes` = data.frame(footnotes = I(list(NULL, list()))))
  expect_null(ft(empty))
  expect_error(rtfreporter:::.gt_body_from_slots(list()),
               "Could not read the gt table body")
})

# ── print(rtftable): the console preview ───────────────────────────────────

test_that("print(rtftable) previews a spanning header, a split label and 'n' rows", {
  d <- data.frame(a = c("x", "y", "z"), b = 1:3, c = c(1.5, 2.5, 3.5),
                  stringsAsFactors = FALSE)
  t1 <- rtftable(d, col_header = rtf_col_header(
    list(col_cell(1, "One"), col_cell(c(2, 3), "Two and a very long spanner label")),
    c("a", "b", "c\nline2")))
  old <- options(rtfreporter.print_ascii = TRUE); on.exit(options(old), add = TRUE)

  out <- capture.output(res <- print(t1))
  expect_identical(res, t1)
  expect_match(out[1], "^-+$")                                  # ASCII rule
  expect_match(out[2], "One +Two and a very long spanner label")
  expect_match(out[3], "line2|c ")                              # the stacked label
  expect_true(any(grepl("^x +1 +1.5", out)))
  expect_true(any(grepl("<rtftable> 3 rows x 3 columns", out)))
  expect_true(any(grepl("Header rows: 2 \\(spanning\\)", out)))

  few <- capture.output(print(t1, n = 1))
  expect_true(any(grepl("(2 more rows)", few, fixed = TRUE)))
  expect_false(any(grepl("^z +3", few)))
  all_rows <- capture.output(print(t1, n = Inf))
  expect_true(any(grepl("^z +3", all_rows)))
  expect_false(any(grepl("more rows", all_rows)))
  expect_identical(capture.output(print(t1, n = NA)), all_rows)      # NA -> default 10
  none <- capture.output(print(t1, n = 0))
  expect_true(any(grepl("(3 more rows)", none, fixed = TRUE)))
})

test_that("the console preview draws the rule styles, and nothing for no columns", {
  d <- data.frame(a = c("x", "y"), b = 1:2, stringsAsFactors = FALSE)
  rule <- function(side) {
    t <- rtftable(d, col_header = c("A", "B")) |>
      style_zone(header = rtf_border(bottom = side))
    capture.output(print(t))[1:3]
  }
  old <- options(rtfreporter.print_ascii = TRUE); on.exit(options(old), add = TRUE)
  expect_match(rule(rtf_border_line("double"))[3], "^=+$")
  expect_match(rule(rtf_border_line("thick"))[3], "^=+$")
  expect_match(rule(rtf_border_line("single"))[3], "^-+$")
  expect_match(rule(rtf_border_line("none"))[3], "^x")           # no rule at all

  options(rtfreporter.print_ascii = FALSE)
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]))
  expect_match(rule(rtf_border_line("double"))[3], "^═+$")
  expect_match(rule(rtf_border_line("thick"))[3], "^━+$")
  expect_match(rule(rtf_border_line("single"))[3], "^─+$")

  rc <- rtfreporter:::.rule_char
  expect_true(is.na(rc(NULL, TRUE)))
  expect_true(is.na(rc(list(style = "none"), FALSE)))
  expect_identical(rc(list(style = "double"), FALSE), "=")
  expect_identical(rc(list(), FALSE), "-")

  expect_identical(rtfreporter:::.render_rtftable_console(rtftable(d[, 0])), character(0))
  zero_rows <- capture.output(print(rtftable(d[0, ])))
  expect_true(any(grepl("0 rows x 2 columns", zero_rows)))
  multi <- capture.output(print(rtftable(list(d, d[1, ]), col_header = c("A", "B"))))
  expect_true(any(grepl("2 tables", multi)))
})

# ── plan verbs: what they refuse, in the words they use ────────────────────

test_that("plan_cell_style() explains a where that cannot work", {
  skip_if_not_installed("cards")
  p <- edge_plan()
  expect_error(plan_cell_style(p, where = "label", bold = TRUE),
               "plan_cell_style\\(where = \\) is a one-sided formula over the table's columns")
  expect_error(plan_cell_style(p, header = TRUE, where = ~ label == "n", bold = TRUE),
               "plan_cell_style\\(header = TRUE\\) styles the column header, which has no rows")
  expect_error(plan_cell_style(p, where = ~ label == "n", border = rtf_border(bottom = TRUE)),
               "takes bold, italic, underline, align, indent_twips, color and background; a border is set on whole columns")
})

test_that("plan_columns(widths = ) must cover every printed column", {
  skip_if_not_installed("cards")
  p <- edge_plan()
  expect_error(suppressMessages(plan_apply(plan_columns(p, widths = c(group = 1, label = 2)))),
               "The `columns` sheet gives widths, but not for: .Placebo., .Xanomeline High Dose., .Xanomeline Low Dose.")
  ok <- suppressMessages(plan_apply(plan_columns(p, widths = c(group = 1, label = 2, .values = 3))))
  expect_identical(first_page(ok)$col_rel_width, c(1, 2, 3, 3, 3))
})

test_that("plan_col_header(values = ) accepts a scope, a number or a function, and says why not", {
  skip_if_not_installed("cards")
  p <- edge_plan()
  hdr <- rtf_col_header(c("a", "b", "{col} ({n})"))
  expect_error(suppressMessages(plan_apply(plan_col_header(p, hdr, values = list(n = "bogus")))),
               "a population is \"page\" \\(each page's own\\) or \"table\"")
  lay <- function(...) suppressMessages(plan_layers(plan_col_header(p, hdr, ...)))$header
  expect_identical(lay(values = "page")$n_text, "page")
  expect_identical(lay(values = list(n = "page", N = "table"))$n_text, "n = page | N = table")
  lit <- lay(values = c(Placebo = 1, "Xanomeline High Dose" = 2, "Xanomeline Low Dose" = 3))
  expect_null(lit$n_text)
  expect_true(lit$literal_n)
  # a header sheet whose text asks for {n} reads the population without being told
  sheet <- data.frame(line = 1, cols = c("group", "label", ".values"),
                      text = c("G", "L", "{col} (N={n})"),
                      span = c(NA, NA, "each"), stringsAsFactors = FALSE)
  got <- first_page(suppressMessages(plan_apply(plan_col_header(p, sheet))))$col_header[[1]]
  expect_match(got[3], "^Placebo \\(N=\\d+\\)$")
})

# ── pagination and column pages: refusals and edges ────────────────────────

test_that("as_rtftables() names a split or collapse column that is not there", {
  d <- data.frame(g = c("a", "a", "b", "b", "c"), v = 1:5, stringsAsFactors = FALSE)
  expect_error(as_rtftables(d, split = "rows"), "`split_rows` is required when split = \"rows\"")
  expect_error(as_rtftables(d, split = "group_safe"), "`max_rows` is required when split = \"group_safe\"")
  expect_error(as_rtftables(d, split = "by_value", group_col = 9),
               "`group_col` index 9 out of range \\(1..2\\)")
  expect_error(as_rtftables(d, split = "by_value", group_col = "nope"), "nope")
  expect_error(as_rtftables(d, collapse_repeats = 9),
               "`collapse_repeats` index 9 out of range \\(1..2\\)")
  expect_error(as_rtftables(d, collapse_repeats = "nope"),
               "`collapse_repeats` column 'nope' not found in the table")
  expect_error(as_rtftables(5), "`as_rtftables\\(\\)` supports gt_tbl, gt_group")
})

test_that("as_rtftables() makes one page of an empty, one-row or one-column data frame", {
  d <- data.frame(g = c("a", "a", "b"), v = 1:3, stringsAsFactors = FALSE)
  for (x in list(d[0, ], d[1, ], d["g"])) {
    pages <- as_rtftables(x)
    expect_length(pages, 1L)
    expect_s3_class(pages[[1]], "rtftable")
    expect_identical(nrow(pages[[1]]$data), nrow(x))
  }
  # a one-row table still renders
  txt <- edge_render(rtf_document() |> rtf_tables(as_rtftables(d[1, ])))
  expect_match(txt, "a", fixed = TRUE)
  # a long text cell goes into the file whole
  long <- data.frame(t = strrep("long words ", 60), stringsAsFactors = FALSE)
  expect_match(edge_render(rtf_document() |> rtf_tables(as_rtftables(long))),
               strrep("long words ", 60), fixed = TRUE)
})

test_that("paginate_cols() explains a bad `by`, an unknown `at` and an empty cut", {
  d <- data.frame(id = c("s1", "s2"), `P____D1` = 1:2, `P____D2` = 3:4,
                  `Q____D1` = 5:6, `Q____D2` = 7:8, check.names = FALSE,
                  stringsAsFactors = FALSE)
  t1 <- rtftable(d, col_header = c("ID", "D1", "D2", "D1", "D2"))
  expect_error(paginate_cols(t1, by = c("a", "b")),
               "`by` must be a single separator found in the column names, or one key per column \\(5\\)")
  expect_error(paginate_cols(t1, by = "____", carry = 1:5),
               "`by` left no columns to split: every column is a carry column")
  expect_error(paginate_cols(t1, at = 9), "`paginate_cols\\(at\\)` index 9 out of range \\(1..5\\)")
  expect_error(paginate_cols(t1), "One of `at` \\(the columns to cut before\\), `cols` or `by` is required")
  # one key per column is accepted, and the carry column is on every page
  pages <- suppressWarnings(paginate_cols(t1, by = c(NA, "P", "P", "Q", "Q")))
  expect_length(pages, 2L)
  expect_identical(names(pages[[1]]$data), c("id", "P____D1", "P____D2"))
  expect_identical(names(pages[[2]]$data), c("id", "Q____D1", "Q____D2"))
  # absolute column widths follow their columns onto each page
  fixed <- rtftable(d, column_widths_twips = rep(1000, 5))
  cut <- paginate_cols(fixed, at = 3)
  expect_identical(cut[[2]]$column_widths_twips, rep(1000L, 4))
  # a wider second block is said, not hidden
  expect_warning(paginate_cols(t1, at = 3), "column block 2 totals more than block 1")
  expect_error(rtfreporter:::.rtftable_keep_cols(t1, integer(0)),
               "A column page must keep at least one column")
})

# ── column headers: names and positions that cannot be resolved ────────────

test_that("a named header row says which column it could not place", {
  r <- rtfreporter:::.resolve_named_label_row
  expect_error(r(c(a = "A"), NULL), "A named col_header label row needs data column names")
  expect_identical(r(c("A", "B"), NULL), c("A", "B"))             # positional: untouched
  expect_error(r(c(a = "A", "x"), c("a", "x", "x")),
               "value \"x\" matches 2 columns; name it explicitly")
  expect_error(r(c(c1 = "A", "B", "C", "D"), c("c1", "b", "c")),
               "label #4 \\(\"D\"\\) has no matching column name and its position \\(4\\) exceeds the number of data columns \\(3\\)")
  expect_identical(r(c(a = "A", "x"), c("a", "x", "y")), c("A", "x", "y"))

  cp <- rtfreporter:::.resolve_cell_pos
  expect_error(cp(NULL, "a"), "Cell spec missing `pos` field")
  expect_error(cp("a", NULL), "Column-name `pos` in col_cell\\(\\) requires the header to be attached")
  expect_error(cp("zzz", c("a", "b")), "`pos` column name \"zzz\" not found in data columns. Available: \"a\", \"b\"")
  expect_error(cp("x", c("x", "x")), "`pos` column name \"x\" is ambiguous \\(matches 2 columns\\)")
  expect_identical(cp(c("a", "b"), c("a", "b")), c(1L, 2L))
  expect_identical(cp(3, NULL), 3L)
  expect_identical(cp(function(nm) nm == "b", c("a", "b")), 2L)
  expect_error(col_cell(NA, "x"), "`pos` must be a number or a column name of length 1 or 2")
  expect_error(col_cell(1:3, "x"), "`pos` must be a number or a column name of length 1 or 2")
  expect_identical(rtfreporter:::.int_runs_text(c(2L, 4L, 5L, 6L, 9L)), "2, 4-6, 9")

  d <- data.frame(a = 1:2, b = 3:4, b2 = 5:6)
  expect_error(rtftable(d, col_header = c(a = "A", zzz = "Z")),
               "col_header: unknown column name \"zzz\". Available: \"a\", \"b\", \"b2\"")
  expect_error(rtftable(d, col_header = c(b = "B", "b")),
               "col_header: column 2 \\(\"b\"\\) is targeted twice")
})

# ── listing widths and the code that reproduces a listing ──────────────────

test_that("fit_listing_widths() refuses a bad page, width or font and writes pasteable code", {
  d <- data.frame(USUBJID = c("S1", "S2"), AGE = c(30, NA),
                  TERM = c("a very long adverse event term that wraps again", "short"),
                  stringsAsFactors = FALSE)
  sp <- listing_spec(list(listing_col("USUBJID"), listing_col(c("AGE", "TERM"), sep = " / "),
                          listing_col("AGE", label = c("A", "B"), width = 6, rel_width = 2,
                                      align = "right", layout = "flow",
                                      collapse_repeats = TRUE)),
                     sep = ", ", spacer = FALSE, record = "REC",
                     wrap = function(text, width, sep, layout) text)
  expect_error(fit_listing_widths(d, sp, page = 3),
               "`page` must be an rtf_page\\(\\) object or a list of page settings")
  expect_error(fit_listing_widths(d, sp, total_width = -1),
               "`total_width` must be a single positive number of characters")
  expect_error(fit_listing_widths(d, sp, total_width = c(1, 2)),
               "`total_width` must be a single positive number of characters")
  expect_error(fit_listing_widths(d, sp, page = rtf_page(margin_left_in = 5, margin_right_in = 5)),
               "leaves 7 for the 2 column\\(s\\) to be fitted, which cannot each be 6 wide")

  fitted <- fit_listing_widths(d, sp, total_width = 40)
  code <- listing_code(fitted)
  expect_s3_class(code, "rtf_listing_code")
  txt <- paste(unclass(code), collapse = "\n")
  expect_match(txt, "custom `wrap` function, which cannot be", fixed = TRUE)
  expect_match(txt, "listing_col(\"USUBJID\", width = ", fixed = TRUE)
  expect_match(txt, "label = \"A\\nB\"", fixed = TRUE)
  expect_match(txt, "collapse_repeats = TRUE", fixed = TRUE)
  expect_match(txt, "record = \"REC\"", fixed = TRUE)
  expect_match(txt, "spacer = FALSE", fixed = TRUE)
  out <- capture.output(res <- print(code))
  expect_identical(res, code)
  expect_true(any(grepl("^listing_spec\\(list\\($", out)))
  # the code of an unfitted spec leaves the widths alone
  plain <- paste(unclass(listing_code(sp)), collapse = "\n")
  expect_false(grepl("rel_width = 7", plain, fixed = TRUE))
  expect_error(listing_code(list()), "`spec` must be a listing_spec\\(\\); got 'list'")
})

test_that("set_col_header(values = ) matches pages to rows and says when it falls back", {
  d <- data.frame(a = "x", b = 1L, stringsAsFactors = FALSE)
  pages <- as_rtftables(list(A = d, B = d))
  hdr <- c("Item", "N={n}")
  by_name <- set_col_header(pages, hdr, by = "name",
                            values = data.frame(name = c("A", "B"), n = c(10, 20),
                                                stringsAsFactors = FALSE))
  expect_identical(by_name[[1]]$col_header[[1]], c("Item", "N=10"))
  expect_identical(by_name[[2]]$col_header[[1]], c("Item", "N=20"))

  expect_error(set_col_header(pages, hdr, values = list(n = 1)),
               "`values` must be a data.frame: one row per page key, one column per token")
  expect_error(set_col_header(pages, hdr, by = "bogus", values = data.frame(n = 1)),
               "`values` has no key column `bogus`.  Name the key column after the axis it matches: `group`, `rows`, `name`")
  expect_error(set_col_header(pages, hdr, by = "bogus",
                              values = data.frame(bogus = c("A", "B"), n = c(1, 2))),
               "`by`: \"bogus\" is not a page axis \\(group, rows, name\\)")
  # pages with no group of their own match "group" on the page name, and say so
  expect_message(
    got <- set_col_header(pages, hdr, values = data.frame(group = c("A", "B"), n = c(1, 2),
                                                          stringsAsFactors = FALSE)),
    "no group axis, so `by = \"group\"` matches on the page name instead")
  expect_identical(got[[2]]$col_header[[1]], c("Item", "N=2"))
})

# ── plot() helpers: the line styles and the vertical rules ────────────────

test_that("border sides map to line types, weights and colours", {
  sl <- rtfreporter:::.side_lty_lwd
  expect_identical(sl(NULL), list(lty = NA, lwd = NA, col = NA))
  expect_identical(sl(rtf_border_line("dash")), list(lty = "dashed", lwd = 1, col = "black"))
  expect_identical(sl(rtf_border_line("dot", color = "#FF0000")),
                   list(lty = "dotted", lwd = 1, col = "#FF0000"))
  expect_identical(sl(rtf_border_line("thick"))$lwd, 3)
  expect_identical(sl(rtf_border_line("double"))$lwd, 2)
  expect_identical(sl(list(style = "other"))$lty, "solid")
})

test_that("plot(rtftable) draws a header with spanning cells and a last-row rule", {
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  d <- data.frame(a = c("x", "y"), b = 1:2, c = c(1.5, 2.5), stringsAsFactors = FALSE)
  tbl <- rtftable(d, col_header = rtf_col_header(
    list(col_cell(1, "One"), col_cell(c(2, 3), "Two")), c("a", "b", "c"))) |>
    style_zone(header = rtf_border(top = TRUE, bottom = TRUE),
               last_row = rtf_border(bottom = TRUE))
  expect_identical(plot(tbl), tbl)
  # vertical rules, plain and double, are drawn without error
  expect_silent(rtfreporter:::.draw_vside(rtf_border_line("double"), 0, 1, 0.5))
  expect_silent(rtfreporter:::.draw_vside(rtf_border_line("single"), 0, 1, 0.5))
  expect_silent(rtfreporter:::.draw_vside(NULL, 0, 1, 0.5))
  graphics::plot.new()
  graphics::plot.window(c(0, 1), c(0, 1))
  expect_silent(rtfreporter:::.draw_side(rtf_border_line("double"), 0, 1, 0.5))
  expect_null(rtfreporter:::.draw_side(NULL, 0, 1, 0.5))
})


# -- #547-#550: the behaviour found while raising coverage, now fixed ---------

test_that("print(rtf_document()) names a preset page by its size and orientation (#547)", {
  out <- capture.output(print(rtf_document()))
  expect_true(any(grepl("Document page size: letter \\(landscape\\)", out)))
  out2 <- capture.output(print(rtf_document(page = rtf_page(width_in = 8.5,
                                                            height_in = 11))))
  expect_true(any(grepl("Document page size: 8.5 x 11 inches", out2)))
})

test_that("an explicit rtf_section(page = 1) wins over the first auto section (#548)", {
  d <- data.frame(a = c("x", "y"), b = 1:2)
  doc <- rtf_document() |>
    rtf_section(page = NULL,
                secinfo = list(header = rtf_header(rows = list(c(l = "Study"))))) |>
    rtf_section(page = 1,
                secinfo = list(header = rtf_header(rows = list(c(l = "First"))))) |>
    rtf_tables(list("T1" = d, "T2" = d), auto_section = TRUE)
  f <- withr::local_tempfile(fileext = ".rtf")
  expect_no_error(generate_rtfreport(doc, f, overwrite = TRUE))
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(txt, "First", fixed = TRUE)   # page 1: the explicit section
  expect_match(txt, "T2", fixed = TRUE)      # page 2: its auto section
})

test_that("a row of cell_rows() may be one bare guard (#549)", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  ard <- cards::ard_stack(adsl, .by = TRT,
                          cards::ard_tabulate(variables = SEX,
                                              statistic = ~ c("n", "p")),
                          .total_n = TRUE)
  w <- widen_ard(normalize_ard(ard), cols = "TRT", rows = c(group = "variable"),
                 cells = cell_rows("1" = n > 0 ~ "has"), notes = FALSE)
  expect_true(all(unlist(w[, -(1:2)]) == "has"))
  # the same as the guard inside c()
  w2 <- widen_ard(normalize_ard(ard), cols = "TRT", rows = c(group = "variable"),
                  cells = cell_rows("1" = c(n > 0 ~ "has")), notes = FALSE)
  expect_identical(w, w2)
  out <- capture.output(print(cell_rows("1" = n == 0 ~ "0")))
  expect_match(out[2], 'n == 0 ~ "0"', fixed = TRUE)
})

test_that("the console preview draws dash and dot rules as such (#550)", {
  rc <- rtfreporter:::.rule_char
  expect_identical(rc(list(style = "dash"), TRUE), "\u2504")
  expect_identical(rc(list(style = "dot"), TRUE), "\u2508")
  expect_identical(rc(list(style = "single"), TRUE), "\u2500")
  expect_true(all(rtfreporter:::.valid_border_styles %in%
                    c("single", "double", "thick", "dash", "dot")))
})

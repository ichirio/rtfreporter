# Tests added to close the gaps found by the Traceability Matrix check
# (2026-11-12): each asserts what its requirement states.
# Group g3: areas IX, LS and PL.  Fictional data only.

# ---------------------------------------------------------------- helpers

.g3_render <- function(doc, ...) {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE, ...)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

# A small normalized ARD: AGE summarised (N, mean, sd) in arms A and B,
# with a formatted statistic column.
.g3_ard <- function(fmt = TRUE) {
  d <- data.frame(
    group1 = "TRT", group1_level = rep(c("A", "B"), each = 3L),
    variable = "AGE", variable_level = NA_character_, context = "continuous",
    stat_name = rep(c("N", "mean", "sd"), 2L),
    stat_label = rep(c("N", "Mean", "SD"), 2L),
    stat = c(10, 12.34, 1.2, 12, 15.67, 2.2), stringsAsFactors = FALSE)
  if (fmt) d$stat_fmt <- c("10", "12.3", "1.2", "12", "15.7", "2.2")
  normalize_ard(d)
}

# Two column keys (TRT, SEX): one mean per cell.
.g3_ard2 <- function() {
  g <- expand.grid(sx = c("F", "M"), trt = c("Placebo", "Drug"),
                   stringsAsFactors = FALSE)
  normalize_ard(data.frame(
    group1 = "TRT", group1_level = g$trt, group2 = "SEX", group2_level = g$sx,
    variable = "AGE", variable_level = NA_character_, context = "continuous",
    stat_name = "mean", stat_label = "Mean",
    stat = c(2.5, 4.5, 6.5, 8.5), stringsAsFactors = FALSE))
}

# ------------------------------------------------------------------- IX

test_that("FT-IX3000 [FR-IX123]: a footer in its own font starts in the document's font", {
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(
      header = NULL, footer = rtf_footer(c(l = "FOOTX"), font = "Arial"))) |>
    rtf_tables(data.frame(A = c("1", "2"), B = c("x", "y")))
  out <- .g3_render(doc)
  expect_match(out, "{\\fonttbl{\\f0\\fnil\\fcharset0 Courier New;}{\\f1\\fnil\\fcharset0 Arial;}}",
               fixed = TRUE)
  expect_match(out, "{\\footer \\f0\\fs18", fixed = TRUE)
  expect_match(out, "\\f1 FOOTX\\cell", fixed = TRUE)
})

test_that("FT-IX3001 [FR-IX201]: with templates an open {x} reads stat_fmt, else stat", {
  with_fmt <- plan_apply(
    table_plan(.g3_ard(), cols = "TRT") |> plan_cells("{mean}", notes = FALSE),
    "table")
  expect_identical(with_fmt$A, "12.3")
  expect_identical(with_fmt$B, "15.7")
  no_fmt <- plan_apply(
    table_plan(.g3_ard(fmt = FALSE), cols = "TRT") |>
      plan_cells("{mean}", notes = FALSE), "table")
  expect_identical(no_fmt$A, "12.34")
  expect_identical(no_fmt$B, "15.67")
  # value = changes nothing with templates
  forced <- plan_apply(
    table_plan(.g3_ard(), cols = "TRT") |>
      plan_cells("{mean}", value = "stat", notes = FALSE), "table")
  expect_identical(forced$A, "12.3")
})

test_that("FT-IX3002 [FR-IX251]: plan_col_header(header_sep) wins over plan_columns(sep)", {
  p <- table_plan(.g3_ard2(), cols = c("TRT", "SEX")) |>
    plan_cells("{mean}", notes = FALSE) |>
    plan_columns(sep = "--")
  # sep alone: the header is split on "--" (a spanner per arm)
  pg <- plan_apply(p, "pages")
  expect_identical(pg[[1L]]$col_header[[2L]],
                   c("label", "F", "M", "F", "M"))
  expect_identical(vapply(pg[[1L]]$col_header[[1L]], function(x) x$label, ""),
                   c("", "Placebo", "Drug"))
  # header_sep = "|": the names hold no "|", so no spanning row is made
  p2 <- plan_col_header(p, header_sep = "|")
  expect_identical(plan_apply(p2, "args")$rtf$header_sep, "|")
  pg2 <- plan_apply(p2, "pages")
  expect_null(pg2[[1L]]$col_header)
  expect_identical(names(pg2[[1L]]$data),
                   c("label", "Placebo--F", "Placebo--M", "Drug--F", "Drug--M"))
})

test_that("FT-IX3003 [FR-IX301]: option beats a program found, a document's beats the option, the call's fallback beats the document's", {
  fb <- function(...) {
    rtf_document(...) |>
      rtf_section(page = 1, secinfo = list(header = NULL,
        footer = rtf_footer(list(c(l = "P={PROGRAM}"))))) |>
      rtf_tables(as_rtftables(data.frame(A = "a")))
  }
  old <- options(rtfreporter.program = "opt.R")
  on.exit(options(old), add = TRUE)
  # a program found (mocked) is not used while the option is set; no message
  local_mocked_bindings(.find_program = function()
    list(path = "found/f.R", how = "the script Rscript runs"))
  expect_no_message(out <- .g3_render(fb()))
  expect_match(out, "P=opt.R", fixed = TRUE)
  expect_false(grepl("found/f.R", out, fixed = TRUE))
  # rtf_document(program = ) beats the option
  expect_no_message(out <- .g3_render(fb(program = "doc.R")))
  expect_match(out, "P=doc.R", fixed = TRUE)
  # nothing said, nothing found: the call's program_fallback beats the document's
  options(rtfreporter.program = NULL)
  local_mocked_bindings(.find_program = function() NULL)
  out <- suppressMessages(.g3_render(fb(program_fallback = "b"),
                                     program_fallback = "a"))
  expect_match(out, "P=a.R", fixed = TRUE)
  expect_false(grepl("P=b.R", out, fixed = TRUE))
})

test_that("FT-IX3004 [FR-IX303]: a default argument is taken from the option, an explicit one beats it", {
  old <- options(rtfreporter.font_lang = c(east_asian = "ja"),
                 rtfreporter.page.paper_size = NULL)
  on.exit(options(old), add = TRUE)
  expect_identical(rtf_document()$document$font_lang, c(east_asian = "ja"))
  options(rtfreporter.font_lang = NULL, rtfreporter.page.paper_size = "A4")
  expect_identical(rtf_document()$document$page$paper_size, "A4")
  expect_identical(
    rtf_document(page = rtf_page(paper_size = "Letter"))$document$page$paper_size,
    "Letter")
})

test_that("FT-IX3005 [FR-IX305]: rtf_config() replaces watermark and font_lang whole", {
  d <- rtf_document(watermark = rtf_watermark("A", color = "#112233")) |>
    rtf_config(watermark = "B")
  expect_identical(d$document$watermark$text, "B")
  expect_identical(d$document$watermark$color, "#C8C8C8")
  d2 <- rtf_document(font_lang = c(latin = "en", east_asian = "ja")) |>
    rtf_config(font_lang = c(latin = "ko"))
  expect_identical(d2$document$font_lang, c(latin = "ko"))
})

# A small listing source: three subjects with two rows each, a spec of three columns.
.g3_lst_df <- function() {
  data.frame(S = rep(c("S1", "S2", "S3"), each = 2L),
             A = c("a1", "a2", "a3", "a4", "a5", "a6"),
             B = letters[1:6], stringsAsFactors = FALSE)
}
.g3_lst_spec <- function() {
  listing_spec(list(listing_col("S", width = 6), listing_col("A", width = 6),
                    listing_col("B", width = 3)))
}

test_that("FT-IX3006 [FR-IX317]: an explicit col_header wins over the rebuilt, the listing's and the labelled header", {
  df <- data.frame(label = c("x", "y"), G_a = 1:2, G_b = 3:4)
  rebuilt <- as_rtftables(df, header_sep = "_")[[1L]]$col_header
  expect_length(rebuilt, 2L)                       # the spanning row is rebuilt
  expect_identical(rebuilt[[1L]][[2L]]$label, "G")
  given <- as_rtftables(df, header_sep = "_", col_header = c("L", "P", "Q"))[[1L]]
  expect_identical(given$col_header, list(c("L", "P", "Q")))
  # the listing supplies its own header ("S", "", "A", "", "B"); the caller's wins
  lst <- as_rtftables(.g3_lst_df(), listing = .g3_lst_spec(),
                      col_header = list(c("X", "Y", "Z", "V", "W")))[[1L]]
  expect_identical(lst$col_header, list(c("X", "Y", "Z", "V", "W")))
  own <- as_rtftables(.g3_lst_df(), listing = .g3_lst_spec())[[1L]]
  expect_identical(own$col_header, list(c("S", "", "A", "", "B")))
  # a label attribute read from the source is beaten as well
  lab <- data.frame(a = 1:2, b = 3:4)
  attr(lab$a, "label") <- "Labelled A"
  expect_identical(as_rtftables(lab)[[1L]]$col_header[[1L]], c("Labelled A", "b"))
  expect_identical(as_rtftables(lab, col_header = c("P", "Q"))[[1L]]$col_header,
                   list(c("P", "Q")))
})

test_that("FT-IX3007 [FR-IX318 FR-IX351]: a listing supplies defaults only; the caller's arguments win", {
  df <- .g3_lst_df()
  spec <- .g3_lst_spec()
  meta <- function(p) attr(p$data, "rtf_paginate_meta", exact = TRUE)
  # defaults from the listing: the record column groups, blank row first
  own <- as_rtftables(df, listing = spec, max_rows = 20)[[1L]]
  expect_identical(meta(own)$group_col, ".rtf_record")
  expect_identical(attr(own$data, "rtf_blank_rows", exact = TRUE), 0L)
  expect_identical(names(own$data), c("S", ".sp1", "A", ".sp2", "B"))
  # the caller's group_col / group_by / drop_cols / blank_row_first
  mine <- as_rtftables(df, listing = spec, max_rows = 20, group_col = "S",
                       group_by = "indent", drop_cols = "A",
                       blank_row_first = FALSE)[[1L]]
  expect_identical(meta(mine)$group_col, "S")
  expect_null(attr(mine$data, "rtf_blank_rows", exact = TRUE))
  expect_identical(names(mine$data), c("S", ".sp1", ".sp2", "B"))
  # no split given but max_rows set: the listing gives "group_safe"
  expect_identical(meta(own)$strategy, "group_safe")
  paged <- as_rtftables(df, listing = spec, max_rows = 4)
  expect_length(paged, 3L)
  expect_identical(meta(paged[[1L]])$strategy, "group_safe")
  # an explicit split = "none" is left alone: no pagination
  expect_length(as_rtftables(df, listing = spec, max_rows = 4, split = "none"), 1L)
  # the caller's header and alignment beat the ones derived from the listing
  tb <- as_rtftables(df, listing = spec,
                     col_header = list(c("X", "Y", "Z", "V", "W")),
                     col_spec = list(list(col = 1L, align = "right")))[[1L]]
  expect_identical(tb$col_header, list(c("X", "Y", "Z", "V", "W")))
  expect_identical(tb$col_spec[[1L]]$align, "right")
  expect_identical(tb$col_spec[[3L]]$align, "left")
  # col_rel_width given wins over the derived 6, 1, 6, 1, 3
  expect_identical(as_rtftables(df, listing = spec,
                                col_rel_width = c(5, 1, 5, 1, 5))[[1L]]$col_rel_width,
                   c(5, 1, 5, 1, 5))
})

test_that("FT-IX3008 [FR-IX319]: column_widths_twips wins outright over table_width_twips and col_rel_width", {
  d3 <- data.frame(a = "1", b = "2", c = "3")
  tbl <- rtftable(d3, column_widths_twips = c(3000, 2000, 1000),
                  table_width_twips = 9000, col_rel_width = c(1, 1, 1))
  out <- .g3_render(rtf_document() |> rtf_tables(tbl))
  cellx <- regmatches(out, gregexpr("\\\\cellx[0-9]+", out))[[1L]]
  # every row has the three edges 3000, 5000, 6000: the sum, 6000, is the width
  expect_true(length(cellx) >= 3L)
  expect_identical(unique(cellx), c("\\cellx3000", "\\cellx5000", "\\cellx6000"))
})

test_that("FT-IX3009 [FR-IX320]: without by_value the stub is built first and the arguments name the reshaped columns", {
  ae <- data.frame(SOC = c("Cardiac", "Cardiac", "GI"),
                   PT = c("Arrhythmia", "Tachycardia", "Nausea"),
                   Placebo = c("3", "2", "5"), Active = c("1", "0", "4"),
                   stringsAsFactors = FALSE)
  st <- stub_spec(c("SOC", "PT"), label = "SOC / PT")
  # drop_cols: the stub column exists after the reshape, PT no longer does
  dropped <- as_rtftables(ae, stub = st, split = "none",
                          drop_cols = "SOC / PT")[[1L]]
  expect_identical(names(dropped$data), c("Placebo", "Active"))
  expect_error(as_rtftables(ae, stub = st, split = "none", drop_cols = "PT"),
               "`drop_cols` column 'PT' not found in the table.", fixed = TRUE)
  # group_col: the stub column, not PT
  expect_error(as_rtftables(ae, stub = st, split = "none", group_col = "PT",
                            group_by = "indent"),
               "`group_col` 'PT' not found in the table.", fixed = TRUE)
  g <- as_rtftables(ae, stub = st, split = "none", group_col = "SOC / PT",
                    group_by = "indent")[[1L]]
  expect_identical(names(g$data), c("SOC / PT", "Placebo", "Active"))
  # sort_by names a reshaped column: the stub's label rows (no Placebo) go last
  s <- as_rtftables(ae, stub = st, split = "none", sort_by = "Placebo",
                    sort_desc = TRUE)[[1L]]
  expect_identical(gsub(intToUtf8(160L), "", s$data[["SOC / PT"]], fixed = TRUE),
                   c("Nausea", "Arrhythmia", "Tachycardia", "Cardiac", "GI"))
  # with by_value the group_col is the pre-stub column, and the stub column is not one
  expect_error(as_rtftables(ae, stub = st, split = "by_value",
                            group_col = "SOC / PT"),
               "`group_col` column 'SOC / PT' not found in the table.",
               fixed = TRUE)
  expect_identical(names(as_rtftables(ae, stub = st, split = "by_value",
                                      group_col = "SOC")),
                   c("Cardiac", "GI"))
})

test_that("FT-IX3010 [FR-IX322]: drop_cols: row_title and col_rel_width pre-drop, paginate_cols final", {
  df <- data.frame(A = c("a1", "a2"), B = c("b1", "b2"), C = c("c1", "c2"),
                   D = c("d1", "d2"), E = c("e1", "e2"),
                   stringsAsFactors = FALSE)
  t1 <- as_rtftables(df, drop_cols = "B", row_title = 3L,
                     col_rel_width = c(1, 9, 2, 3, 4))[[1L]]
  expect_identical(names(t1$data), c("A", "C", "D", "E"))
  expect_identical(t1$row_title, 2L)                # input column 3 (C) is now 2
  expect_identical(t1$col_rel_width, c(1, 2, 3, 4)) # the dropped B's 9 is gone
  # paginate_cols positions are final: at = 3 starts the second block at D
  pgs <- suppressWarnings(paginate_cols(as_rtftables(df, drop_cols = "B"), at = 3))
  expect_identical(lapply(pgs, function(p) names(p$data)),
                   list(c("A", "C"), c("A", "D", "E")))
})

test_that("FT-IX3011 [FR-IX323]: sort_by is applied first: group detection, continuation labels and blank rows see the sorted order", {
  df <- data.frame(G = c("b", "a", "b", "a", "a", "a"),
                   V = c("v1", "v2", "v3", "v4", "v5", "v6"),
                   stringsAsFactors = FALSE)
  bl <- as_rtftables(df, sort_by = "G", group_col = "G",
                     blank_rows = "between_groups")[[1L]]
  expect_identical(bl$data$V, c("v2", "v4", "v5", "v6", "v1", "v3"))
  expect_identical(bl$blank_rows, 4L)         # one change, after the four a rows
  unsorted <- as_rtftables(df, group_col = "G", blank_rows = "between_groups")[[1L]]
  expect_identical(unsorted$blank_rows, 1L:3L)
  pg <- as_rtftables(df, sort_by = "G", group_col = "G", max_rows = 3,
                     split = "group_force", cont_label = "(cont.)")
  expect_length(pg, 3L)
  expect_identical(pg[[1L]]$data$V, c("v2", "v4"))
  expect_identical(pg[[2L]]$data$G[1L], "a(cont.)")
  expect_identical(pg[[2L]]$data$V, c("", "v5", "v6"))
  expect_identical(pg[[3L]]$data$G, c("b", "b"))
})

test_that("FT-IX3012 [FR-IX326]: paginate_cols(order) decides how many sections auto_section opens", {
  d <- data.frame(a = letters[1:6], b = 1:6, c = LETTERS[1:6], d = 6:1,
                  stringsAsFactors = FALSE)
  pg <- list(g1 = rtftable(d[1:3, ]), g2 = rtftable(d[4:6, ]))
  n_sections <- function(order) {
    x <- paginate_cols(pg, at = 3, order = order, fit = FALSE)
    doc <- rtf_document() |> rtf_tables(x, auto_section = TRUE)
    length(rtfreporter:::.pipe_doc_to_rtfreport(doc)$sections)
  }
  expect_identical(n_sections("down"), 2L)      # a table's column pages together
  expect_identical(n_sections("across"), 4L)    # the names interleave: a section per page
})

test_that("FT-IX3013 [FR-IX329]: set_blank_rows(group_col, group_by) leave by_change, by_rule and integer positions alone", {
  df <- data.frame(G = c("A", "  a1", "  a2", "B", "  b1", "  b2"),
                   L = c("x", "x", "y", "y", "y", "z"), stringsAsFactors = FALSE)
  rules <- list(
    blank_rows_by_change("L", include_before_first = FALSE,
                         include_after_last = FALSE),
    blank_rows_by_rule(col = "G", pattern = "^B", where = "before"),
    2L)
  pos <- function(...) attr(set_blank_rows(df, blank_rows = rules, ...),
                            "rtf_blank_rows", exact = TRUE)
  # by_change: after rows 2 and 5; by_rule: before row 4 = after row 3; integer: 2
  expect_identical(pos(), c(2L, 3L, 5L))
  expect_identical(pos(group_col = "G", group_by = "indent"), c(2L, 3L, 5L))
  expect_identical(pos(group_col = "G", group_by = "value"), c(2L, 3L, 5L))
  # between_groups does read them
  bg <- function(by) attr(set_blank_rows(df, blank_rows = "between_groups",
                                         group_col = "G", group_by = by),
                          "rtf_blank_rows", exact = TRUE)
  expect_identical(bg("indent"), 3L)
  expect_identical(bg("value"), 1L:5L)
})

test_that("FT-IX3014 [FR-IX336]: a look field resolves explicit argument, col_spec, style, default", {
  d <- data.frame(a = "x", b = "y", c = "z")
  aligns <- function(t) vapply(t$col_spec, function(s) s$align, "")
  # no style: the package default (row title left, the others centred)
  expect_identical(aligns(rtftable(d)), c("left", "center", "center"))
  # a style that says align beats the default
  sty <- rtf_table_style(align = "left")
  expect_identical(aligns(rtftable(d, style = sty)), c("left", "left", "left"))
  # a col_spec entry beats the style's field
  cs <- list(list(col = 2L, align = "right"))
  expect_identical(aligns(rtftable(d, col_spec = cs, style = sty)),
                   c("left", "right", "left"))
  # an explicit argument beats the style's field
  both <- rtf_table_style(row_height_twips = 300L, cell_padding_left_twips = 100L)
  expect_identical(rtftable(d, style = both)$row_height_twips, 300L)
  expect_identical(rtftable(d, style = both)$cell_padding_left_twips, 100L)
  mine <- rtftable(d, style = both, row_height_twips = 400L,
                   cell_padding_left_twips = 50L)
  expect_identical(mine$row_height_twips, 400L)
  expect_identical(mine$cell_padding_left_twips, 50L)
  expect_null(rtftable(d)$row_height_twips)
  # the header's alignment: the table-wide argument beats the style's field
  hs <- rtf_table_style(header_align = "left")
  ha <- function(t) vapply(t$col_spec, function(s) s$header_align, "")
  expect_identical(ha(rtftable(d, style = hs)), c("left", "left", "left"))
  expect_identical(ha(rtftable(d, style = hs, col_header_align = "center")),
                   c("center", "center", "center"))
})

test_that("FT-IX3015 [FR-IX338]: a col_cell() border is the four outer edges of that one cell", {
  d <- data.frame(a = "x", b = "y", c = "z")
  tbl <- rtftable(d, border = NULL, col_header = list(
    list(col_cell(1L, ""), col_cell(2:3, "SPAN", border = rtf_border(all = TRUE))),
    c("a", "b", "c")))
  out <- .g3_render(rtf_document() |> rtf_tables(tbl))
  rows <- strsplit(out, "\n", fixed = TRUE)[[1L]]
  span <- grep("SPAN", rows, value = TRUE)
  edges <- paste0("\\clbrdrt\\brdrs\\brdrw15\\clbrdrb\\brdrs\\brdrw15",
                  "\\clbrdrl\\brdrs\\brdrw15\\clbrdrr\\brdrs\\brdrw15",
                  "\\clvertalb\\cellx13680")
  expect_length(span, 1L)
  expect_true(grepl(edges, span, fixed = TRUE))
  # only the spanning cell has them: the first cell and the other rows have none
  expect_identical(lengths(regmatches(span, gregexpr("\\clbrdrt", span, fixed = TRUE))), 1L)
  expect_false(any(grepl("\\clbrdr", grep("\\cell", rows[!grepl("SPAN", rows)],
                                           value = TRUE), fixed = TRUE)))
})

test_that("FT-IX3016 [FR-IX342]: a column-name col_cell stays over its column when a stub is prepended or columns are dropped", {
  d <- data.frame(SOC = c("X", "X"), PT = c("p", "q"), Active = c("1", "2"),
                  Placebo = c("3", "4"), stringsAsFactors = FALSE)
  named <- rtf_col_header(list(col_cell("Active", "SPAN")))
  span_of <- function(t) {
    cells <- Filter(function(z) identical(z$label, "SPAN"), t$col_header[[1L]])
    c(cells[[1L]]$from, cells[[1L]]$to)
  }
  stubbed <- as_rtftables(d, stub = stub_spec(c("SOC", "PT"), label = "Stub"),
                          col_header = named)[[1L]]
  expect_identical(names(stubbed$data), c("Stub", "Active", "Placebo"))
  expect_identical(span_of(stubbed), c(2L, 2L))        # over Active
  dropped <- as_rtftables(d, drop_cols = "SOC", col_header = named)[[1L]]
  expect_identical(names(dropped$data), c("PT", "Active", "Placebo"))
  expect_identical(span_of(dropped), c(2L, 2L))        # still over Active
  # the same header with Active's position at the input (3) would sit over Placebo
  numeric <- as_rtftables(d, drop_cols = "SOC",
                          col_header = rtf_col_header(list(col_cell(3L, "SPAN"))))[[1L]]
  expect_identical(span_of(numeric), c(3L, 3L))
})

test_that("FT-IX3017 [FR-IX344]: layout = \"flow\" with a width breaks after the separator once the line is full", {
  d <- data.frame(AEDECOD = "40/F/SCREENING/COMPLETED", stringsAsFactors = FALSE)
  body <- build_listing(d, listing_spec(list(
    listing_col("AEDECOD", layout = "flow", width = 12, sep = "/"))))
  expect_identical(body$AEDECOD[1:3], c("40/F/", "SCREENING/", "COMPLETED"))
  # with no width the text comes back as it stands
  nowidth <- build_listing(d, listing_spec(list(
    listing_col("AEDECOD", layout = "flow", sep = "/"))))
  expect_identical(nowidth$AEDECOD[1L], "40/F/SCREENING/COMPLETED")
})

test_that("FT-IX3018 [FR-IX347]: a column's sep, align and layout win over the listing's; NULL takes the listing's, then the type's", {
  d <- data.frame(A = "x", B = "y", stringsAsFactors = FALSE)
  sp <- listing_spec(list(
    listing_col(c("A", "B"), name = "AB", sep = " | ", align = "right",
                layout = "flow", width = 20),
    listing_col("A", name = "A2", width = 20)),
    sep = "/", align = "left", layout = "stack")
  lay <- rtfreporter:::.listing_layout(attr(build_listing(d, sp), "rtf_listing"))
  expect_identical(lay[[1L]]$label, "A | B")             # the column's sep
  expect_identical(lay[[1L]]$align, "right")             # the column's align
  expect_identical(lay[[3L]]$align, "left")              # NULL: the listing's
  expect_identical(build_listing(d, sp)$AB[1L], "x | y") # flow: one line
  # layout: the column's "flow" over the listing's "stack", and the reverse
  two <- function(col_layout, spec_layout) {
    s <- listing_spec(list(listing_col(c("A", "B"), width = 20, layout = col_layout)),
                      layout = spec_layout)
    build_listing(d, s)[[1L]][1:2]
  }
  expect_identical(two("flow", "stack"), c("x/y", ""))
  expect_identical(two("stack", "flow"), c("x/", "y"))
  expect_identical(two(NULL, "flow"), c("x/y", ""))      # NULL: the listing's
  expect_identical(two(NULL, "stack"), c("x/", "y"))
  # the listing's NULL takes the type's template
  tpl <- listing_spec(list(listing_col("A")))
  expect_identical(c(tpl$sep, tpl$align, tpl$layout), c("/", "left", "stack"))
  # a column sep beats the listing's in the derived header too
  b2 <- build_listing(d, listing_spec(list(
    listing_col(c("A", "B"), name = "P", sep = "-"),
    listing_col(c("A", "B"), name = "Q")), sep = "|"))
  expect_identical(c(b2$P[1L], b2$Q[1L]), c("x-y", "x|y"))
})

test_that("FT-IX3019 [FR-IX348]: an explicit listing_spec() argument overrides the multiline template's", {
  base <- listing_spec(list(listing_col("A")))
  expect_identical(c(base$spacer_rel_width, base$blank_row_first), c(1, TRUE))
  expect_identical(base$layout, "stack")
  expect_false(base$wrap_custom)
  over <- listing_spec(list(listing_col("A")), type = "multiline",
                       spacer_rel_width = 3, blank_row_first = FALSE,
                       layout = "flow",
                       wrap = function(text, width, sep, layout) text)
  expect_identical(over$spacer_rel_width, 3)
  expect_false(over$blank_row_first)
  expect_identical(over$layout, "flow")
  expect_true(over$wrap_custom)
  expect_identical(over$wrap("t", 5, "/", "stack"), "t")
})

test_that("FT-IX3020 [FR-IX350 FR-LS153]: rel_width wins over width, then the longest label line, then 10", {
  d <- data.frame(A = "a", B = "b", C = "c", D = "d", stringsAsFactors = FALSE)
  spec <- listing_spec(list(
    listing_col("A", width = 11, rel_width = 20),
    listing_col("B", width = 11),
    listing_col("C", label = "Unique\nSubject ID"),
    listing_col("D", label = "")),
    spacer = FALSE)
  expect_null(spec$cols[[2L]]$rel_width)
  tbl <- as_rtftables(d, listing = spec)[[1L]]
  expect_identical(tbl$col_rel_width, c(20, 11, 10, 10))
  # an explicit rel_width of 12 wins as well
  s12 <- listing_spec(list(listing_col("A", width = 11, rel_width = 12)),
                      spacer = FALSE)
  expect_identical(as_rtftables(d, listing = s12)[[1L]]$col_rel_width[1L], 12)
})

test_that("FT-IX3021 [FR-IX361]: fmt_numeric(): by takes formats and refuses signif / digits; without by, formats is not read", {
  df <- data.frame(stat = c("a", "b", "a"), v = c(1.23456, 123.456, 0.0123456),
                   stringsAsFactors = FALSE)
  fm <- list(a = list(signif = 3), b = list(digits = 1))
  both <- "Give either `by` + `formats`, or `signif` / `digits` -- not both."
  expect_error(fmt_numeric(df, cols = "v", by = "stat", formats = fm, signif = 4),
               both, fixed = TRUE)
  expect_error(fmt_numeric(df, cols = "v", by = "stat", formats = fm, digits = 2),
               both, fixed = TRUE)
  # without by, a given formats is not read at all
  out <- fmt_numeric(df, cols = "v", signif = 4, formats = list("garbage"))
  expect_identical(out$v, c("1.235", "123.5", "0.01235"))
  # with by and formats, the rule per key applies
  ruled <- fmt_numeric(df, cols = "v", by = "stat", formats = fm)
  expect_identical(ruled$v, c("1.23", "123.5", "0.0123"))
})

test_that("FT-IX3022 [FR-IX364]: pct_unit says what pct means; pct_sign widens by one, the parenthesis stays in the same column", {
  nb <- intToUtf8(160L)
  want <- paste0(nb, nb, "1", nb, "(50.0)")
  expect_identical(fmt_count_pct(1L, 0.5), want)
  expect_identical(fmt_count_pct(1L, 50, pct_unit = "percent"), want)
  signed <- paste0(nb, nb, "1", nb, "(50.0%)")
  expect_identical(fmt_count_pct(1L, 0.5, pct_sign = TRUE), signed)
  expect_identical(fmt_count_pct(1L, 50, pct_unit = "percent", pct_sign = TRUE),
                   signed)
  # the closing parenthesis is one column further right with the sign
  expect_identical(regexpr(")", signed, fixed = TRUE)[[1L]],
                   regexpr(")", want, fixed = TRUE)[[1L]] + 1L)
  # a column of values lines up: the parenthesis is in one column
  col <- fmt_count_pct(c(1L, 108L), c(0.5, 0.353), pct_sign = TRUE)
  expect_identical(col, c(paste0(nb, nb, "1", nb, "(50.0%)"),
                          paste0("108", nb, "(35.3%)")))
})

test_that("FT-IX3023 [FR-IX365]: the na cell is padded with nbsp like the others, or with plain spaces when nbsp = \" \"", {
  nb <- intToUtf8(160L)
  x <- c("1 (1.2%)", NA, "108 (35.3%)")
  # fmt_count_paren: the token's right edge is under the ones digit (column 3)
  cp <- fmt_count_paren(x, na = "-")
  expect_identical(cp[2L], paste0(nb, nb, "-", strrep(nb, 8L)))
  expect_identical(fmt_count_paren(x, na = "-", nbsp = " ")[2L],
                   paste0("  -", strrep(" ", 8L)))
  expect_identical(cp[1L], paste0(nb, nb, "1", nb, "(", nb, "1.2%)"))
  # fmt_count_pct
  pc <- fmt_count_pct(c(1L, 108L, NA), c(0.5, 0.353, NA), na = "-")
  expect_identical(pc[3L], paste0(nb, nb, "-", strrep(nb, 7L)))
  expect_identical(fmt_count_pct(c(1L, 108L, NA), c(0.5, 0.353, NA), na = "-",
                                 nbsp = " ")[3L],
                   paste0("  -", strrep(" ", 7L)))
  # fmt_value_paren: the token ends the value field
  vp <- fmt_value_paren(c("1.5 (0.2)", "10.25 (3.1)", NA), na = "-")
  expect_identical(vp[3L], paste0(strrep(nb, 4L), "-", strrep(nb, 6L)))
  expect_identical(fmt_value_paren(c("1.5 (0.2)", "10.25 (3.1)", NA), na = "-",
                                   nbsp = " ")[3L],
                   paste0("    -", strrep(" ", 6L)))
  # fmt_right_align
  expect_identical(fmt_right_align(c("1", "108", NA), na = "-")[3L],
                   paste0(nb, nb, "-"))
  expect_identical(fmt_right_align(c("1", "108", NA), na = "-", nbsp = " ")[3L],
                   "  -")
})

test_that("FT-IX3024 [FR-IX366]: fmt_count_paren(): a missing cell is aligned like a bare count; a wide token widens the field", {
  nb <- intToUtf8(160L)
  unbsp <- function(z) gsub(nb, " ", z, fixed = TRUE)
  x <- c("1 (1.2%)", NA, "9 (3.6%)")
  expect_identical(unbsp(fmt_count_paren(x, na = "-", bare = TRUE)),
                   c("1 (1.2%)", "-       ", "9 (3.6%)"))
  expect_identical(unbsp(fmt_count_paren(x, na = "-", bare = FALSE)),
                   c("1 (1.2%)", "-       ", "9 (3.6%)"))
  # a token wider than the counts widens the count field, with bare = TRUE too
  expect_identical(unbsp(fmt_count_paren(x, na = "n/a", bare = TRUE)),
                   c("  1 (1.2%)", "n/a       ", "  9 (3.6%)"))
  expect_identical(unbsp(fmt_count_paren(x, na = "n/a", bare = FALSE)),
                   c("  1 (1.2%)", "n/a       ", "  9 (3.6%)"))
})

test_that("FT-IX3025 [FR-IX368]: widen_ard(value = ) is ignored with stats = \"cells\"; each token says which value it takes", {
  ard <- .g3_ard()
  w <- function(...) widen_ard(ard, cols = "TRT", notes = FALSE, ...)
  expect_identical(w(cells = "{mean}", value = "stat"),
                   w(cells = "{mean}"))
  expect_identical(w(cells = "{mean}", value = "stat_fmt"),
                   w(cells = "{mean}"))
  expect_identical(w(cells = "{mean}")$A, "12.3")             # stat_fmt then stat
  expect_identical(w(cells = "{mean:stat}")$A, "12.34")        # stat
  expect_identical(w(cells = "{mean:stat}")$B, "15.67")
  expect_identical(w(cells = "{mean:stat_fmt}")$A, "12.3")     # stat_fmt
  bare <- widen_ard(.g3_ard(fmt = FALSE), cols = "TRT", cells = "{mean}",
                    notes = FALSE)
  expect_identical(bare$A, "12.34")                            # no stat_fmt: stat
})

test_that("FT-IX3026 [FR-IX369]: widen_ard(stats = \"rows\") ignores cells and reads the default label as stat_label", {
  ard <- .g3_ard()
  rows <- widen_ard(ard, cols = "TRT", stats = "rows", notes = FALSE)
  expect_identical(names(rows), c("label", "A", "B"))
  expect_identical(rows$label, c("N", "Mean", "SD"))           # stat_label text
  expect_identical(rows$A, c(10, 12.34, 1.2))
  with_cells <- widen_ard(ard, cols = "TRT", stats = "rows", cells = "{n}",
                          notes = FALSE)
  expect_identical(with_cells, rows)                           # cells ignored
  # a label naming another column is that column
  by_name <- widen_ard(ard, cols = "TRT", stats = "rows", label = "stat_name",
                       notes = FALSE)
  expect_identical(names(by_name), c("stat_name", "A", "B"))
  expect_identical(by_name$stat_name, c("N", "mean", "sd"))
})

.g3_two_vars <- function() {
  normalize_ard(rbind(
    data.frame(group1 = "TRT", group1_level = rep(c("A", "B"), each = 4L),
               variable = "SEX", variable_level = rep(c("F", "F", "M", "M"), 2L),
               context = "categorical", stat_name = rep(c("n", "p"), 4L),
               stat_label = rep(c("n", "%"), 4L),
               stat = c(4, 0.4, 6, 0.6, 5, 0.5, 5, 0.5), stringsAsFactors = FALSE),
    data.frame(group1 = "TRT", group1_level = rep(c("A", "B"), each = 2L),
               variable = "AGE", variable_level = NA_character_,
               context = "continuous", stat_name = rep(c("N", "mean"), 2L),
               stat_label = rep(c("N", "Mean"), 2L),
               stat = c(10, 12.34, 12, 15.67), stringsAsFactors = FALSE)))
}
.g3_two_cells <- function() {
  list(continuous = c(n = "{N:.0f}"), categorical = "{n:.0f}")
}

test_that("FT-IX3027 [FR-IX371]: labels never set an order; a level is ordered by its own name or by the text labels gives it", {
  ard <- .g3_two_vars()
  w <- function(...) widen_ard(ard, cols = "TRT", rows = c(group = "variable"),
                               cells = .g3_two_cells(), notes = FALSE, ...)
  labs <- c(SEX = "Sex", AGE = "Age")
  # the data's order (SEX first), whatever order the labels are written in
  expect_identical(as.character(w(labels = labs)$group), c("Sex", "Sex", "Age"))
  expect_identical(as.character(w(labels = c(AGE = "Age", SEX = "Sex"))$group),
                   c("Sex", "Sex", "Age"))
  # levels in the text labels gives, or in the values' own names: Age first
  expect_identical(
    as.character(w(labels = labs, levels = list(variable = c("Age", "Sex")))$group),
    c("Age", "Sex", "Sex"))
  expect_identical(
    as.character(w(labels = labs, levels = list(variable = c("AGE", "SEX")))$group),
    c("Age", "Sex", "Sex"))
})

test_that("FT-IX3028 [FR-IX372]: a labels scope matches the output name, then the source name, then the plain entries; an analysis variable's scope recodes its levels", {
  ard <- .g3_two_vars()
  w <- function(labs) widen_ard(ard, cols = "TRT", rows = c(group = "variable"),
                                cells = .g3_two_cells(), notes = FALSE, labels = labs)
  # the output name (group) is read first; the source name (variable) is not
  one <- w(list(group = c(SEX = "S-out"),
                variable = c(SEX = "S-src", AGE = "A-src"),
                .default = c(SEX = "S-def", AGE = "A-def")))
  expect_identical(as.character(one$group), c("S-out", "S-out", "AGE"))
  # without the output name: the source name
  two <- w(list(variable = c(SEX = "S-src", AGE = "A-src"),
                .default = c(SEX = "S-def", AGE = "A-def")))
  expect_identical(as.character(two$group), c("S-src", "S-src", "A-src"))
  # with neither: the plain entries
  three <- w(list(.default = c(SEX = "S-def", AGE = "A-def")))
  expect_identical(as.character(three$group), c("S-def", "S-def", "A-def"))
  # a scope named after an analysis variable recodes its levels in the label column
  rec <- w(list(AGE = c(n = "Count"), SEX = c(F = "Female", M = "Male")))
  expect_identical(as.character(rec$label), c("Female", "Male", "Count"))
  expect_identical(as.character(rec$group), c("SEX", "SEX", "AGE"))
})

test_that("FT-IX3029 [FR-IX373]: widen_ard(sort = FALSE) still orders on a factor key", {
  d <- data.frame(PARAM = factor(c("B", "B", "A", "A"), levels = c("A", "B")),
                  TRT = c("x", "y", "x", "y"), variable = "V",
                  variable_level = "L", context = "categorical",
                  stat_name = "n", stat_label = "n", stat = c(1, 2, 3, 4),
                  .label = "L", .kind = "categorical", stringsAsFactors = FALSE)
  run <- function(p) widen_ard(p, cols = "TRT", rows = c(PARAM = "PARAM"),
                               label = c(label = ".label"), cells = "{n:.0f}",
                               sort = FALSE, notes = FALSE)
  out <- run(d)
  expect_identical(as.character(out$PARAM), c("A", "B"))       # the factor's levels
  expect_identical(out$x, c("3", "1"))
  # the same data with a plain character key stays where the data put it
  d$PARAM <- as.character(d$PARAM)
  expect_identical(as.character(run(d)$PARAM), c("B", "A"))
})

test_that("FT-IX3030 [FR-IX374]: an explicit rows wins over the default; a hierarchy turns the default off", {
  ard <- .g3_two_vars()
  def <- widen_ard(ard, cols = "TRT", cells = .g3_two_cells(), notes = FALSE)
  expect_identical(names(def)[1L], "group")                    # c(group = "variable")
  expect_identical(as.character(def$group), c("SEX", "SEX", "AGE"))
  own <- widen_ard(ard, cols = "TRT", rows = c(characteristic = "variable"),
                   cells = .g3_two_cells(), notes = FALSE)
  expect_identical(names(own)[1L], "characteristic")
  # a hierarchy: no default group column, the first column is the label
  hier <- normalize_ard(data.frame(
    group1 = "TRT", group1_level = c("A", "B", "A", "B", "A", "B"),
    group2 = c(NA, NA, NA, NA, "AESOC", "AESOC"),
    group2_level = c(NA, NA, NA, NA, "Cardiac", "Cardiac"),
    variable = c(rep("..ard_hierarchical_overall..", 2L), "AESOC", "AESOC",
                 "AETERM", "AETERM"),
    variable_level = c(NA, NA, "Cardiac", "Cardiac", "Palpitations", "Palpitations"),
    context = "hierarchical", stat_name = "n", stat_label = "n",
    stat = c(5, 7, 3, 4, 2, 1), stringsAsFactors = FALSE),
    hierarchy = c("AESOC", "AETERM"), overall = "Any TEAE")
  expect_true(all(hier$.depth >= 1L))
  w <- widen_ard(hier, cols = "TRT", cells = "{n}", notes = FALSE)
  expect_identical(names(w), c("label", "A", "B"))
  expect_identical(w$label, c("Any TEAE", "Cardiac", "Palpitations"))
})

test_that("FT-IX3031 [FR-IX376]: overall_row(from = ) rows are not marked .key_own and stay as the overall row", {
  ard <- rbind(
    data.frame(group1 = NA_character_, group1_level = NA_character_,
               variable = "SEX", variable_level = c("F", "M"),
               context = "categorical", stat_name = "n", stat_label = "n",
               stat = c(7, 9), stringsAsFactors = FALSE),
    data.frame(group1 = "SEX", group1_level = c("F", "M"), variable = "AESOC",
               variable_level = "Cardiac", context = "categorical",
               stat_name = "n", stat_label = "n", stat = c(3, 4),
               stringsAsFactors = FALSE))
  unnamed <- normalize_ard(ard)
  expect_identical(unnamed$.key_own[unnamed$variable == "SEX"], c(TRUE, TRUE))
  named <- normalize_ard(ard, hierarchy = "AESOC",
                         overall = overall_row("Any TEAE", from = "SEX"))
  expect_identical(named$.key_own[named$variable == "SEX"], c(FALSE, FALSE))
  expect_identical(named$.overall[named$variable == "SEX"], c(TRUE, TRUE))
  expect_identical(named$.label[named$variable == "SEX"], c("Any TEAE", "Any TEAE"))
  body <- widen_ard(named, cols = "SEX", cells = "{n}", notes = FALSE)
  expect_identical(body$label, c("Any TEAE", "Cardiac"))
  expect_identical(body$F, c("7", "3"))
})

test_that("FT-IX3032 [FR-IX378]: normalize_ard(drop_contexts = \"attributes\") keeps the total_n row and states no attribute", {
  ard <- rbind(
    data.frame(group1 = NA_character_, group1_level = NA_character_,
               variable = "..ard_total_n..", variable_level = NA_character_,
               context = "total_n", stat_name = "N", stat_label = "N", stat = 20,
               stringsAsFactors = FALSE),
    data.frame(group1 = "SEX", group1_level = c("F", "M"), variable = "AESOC",
               variable_level = "Cardiac", context = "categorical",
               stat_name = "n", stat_label = "n", stat = c(3, 4),
               stringsAsFactors = FALSE))
  dflt <- normalize_ard(ard)
  expect_identical(unique(dflt$variable), "AESOC")             # the row goes ...
  expect_identical(attr(dflt, "ard_total_n"), 20)              # ... its number is kept
  kept <- normalize_ard(ard, drop_contexts = "attributes")
  expect_identical(unique(kept$variable), c("..ard_total_n..", "AESOC"))
  expect_null(attr(kept, "ard_total_n"))
})

test_that("FT-IX3033 [FR-IX380 FR-IX382]: assemble_rtf(toc = ) takes the table's files; assemble_folder() never reads its spec file back", {
  mk <- function(dir, file, no, title, mark) {
    doc <- rtf_document() |>
      rtf_section(page = 1, secinfo = list(
        header = rtf_header(rows = list(c(c = paste0("Table ", no)), c(c = title))),
        footer = NULL)) |>
      rtf_tables(data.frame(A = mark))
    generate_rtfreport(doc, file.path(dir, file), overwrite = TRUE)
    file.path(dir, file)
  }
  d <- tempfile("asm"); dir.create(d)
  od <- tempfile("asmout"); dir.create(od)
  on.exit(unlink(c(d, od), recursive = TRUE), add = TRUE)
  a <- mk(d, "a.rtf", "14.1.1", "Demog", "MARKAAA")
  b <- mk(d, "b.rtf", "14.3.1", "AEs", "MARKBBB")
  mk(d, "c.rtf", "14.2.1", "Vitals", "MARKCCC")
  marks <- function(f) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    regmatches(txt, gregexpr("MARK[ABC]{3}", txt))[[1L]]
  }
  sp <- assemble_folder(d)
  expect_identical(basename(sp$file), c("a.rtf", "c.rtf", "b.rtf"))
  # a toc file that is not in input_files stops, as a table and as a path
  gone <- paste0("`toc`: file '", sp$file[2L], "' is not in `input_files`.")
  expect_error(assemble_rtf(toc = sp, input_files = c(a, b),
                            output_file = file.path(od, "x.rtf")),
               gone, fixed = TRUE)
  csv <- file.path(od, "toc.csv")
  utils::write.csv(sp, csv, row.names = FALSE)
  expect_error(assemble_rtf(toc = csv, input_files = c(a, b),
                            output_file = file.path(od, "x.rtf")),
               gone, fixed = TRUE)
  # as a path with input_files left out: the file column, in the table's order
  edited <- sp[3:1, ]
  edited$order <- 1:3
  utils::write.csv(edited, csv, row.names = FALSE)
  o1 <- file.path(od, "o1.rtf")
  assemble_rtf(toc = csv, output_file = o1)
  expect_identical(marks(o1), c("MARKBBB", "MARKCCC", "MARKAAA"))
  # assemble_folder() writes its spec file, never reads it: a file already
  # there is overwritten, and the output is the scanned order
  sf <- file.path(od, "spec.csv")
  writeLines("garbage,spec", sf)
  o2 <- file.path(od, "o2.rtf")
  assemble_folder(d, o2, spec_file = sf, overwrite = TRUE)
  expect_identical(read.csv(sf)$table, c("14.1.1", "14.2.1", "14.3.1"))
  expect_identical(marks(o2), c("MARKAAA", "MARKCCC", "MARKBBB"))
  # an edit of the written file (b, c, a) has no effect on the next assemble_folder()
  utils::write.csv(edited, sf, row.names = FALSE)
  o3 <- file.path(od, "o3.rtf")
  assemble_folder(d, o3, spec_file = sf, overwrite = TRUE)
  expect_identical(read.csv(sf)$table, c("14.1.1", "14.2.1", "14.3.1"))
  expect_identical(marks(o3), c("MARKAAA", "MARKCCC", "MARKBBB"))
})

test_that("FT-IX3034 [FR-IX386]: rtf_figures(width_twips) sets the page size, whatever render_width", {
  skip_if_not(isTRUE(unname(capabilities("png"))), "no PNG device")
  old <- options(rtfreporter.png_device = "png")
  on.exit(options(old), add = TRUE)
  draw <- function() plot(1:10)
  goal <- function(rw) {
    doc <- rtf_document() |>
      rtf_figures(draw, width_twips = 4000L, render_width = rw,
                  render_height = 2, render_dpi = 100)
    rtf <- .g3_render(doc)
    regmatches(rtf, regexpr("\\\\picwgoal[0-9]+\\\\pichgoal[0-9]{1,5}", rtf))
  }
  # the width is 4000 in both; the height follows the drawn aspect (3x2, 8x2)
  expect_identical(goal(3), "\\picwgoal4000\\pichgoal2667")
  expect_identical(goal(8), "\\picwgoal4000\\pichgoal1000")
})

test_that("FT-IX3035 [FR-IX389]: rtfplot(png_device) given wins over the option, which wins over \"auto\"", {
  dev <- rtfreporter:::.rtfplot_png_device
  old <- options(rtfreporter.png_device = "ragg")
  on.exit(options(old), add = TRUE)
  # given "png": the option "ragg" is not even checked (ragg may be absent)
  expect_identical(dev("png"), "png")
  options(rtfreporter.png_device = NULL)
  has_ragg <- requireNamespace("ragg", quietly = TRUE)
  expect_identical(dev(), if (has_ragg) "ragg" else "png")      # the default "auto"
  expect_identical(dev("auto"), dev())
  options(rtfreporter.png_device = "png")
  expect_identical(dev(), "png")                                # the option
  expect_identical(dev("auto"), if (has_ragg) "ragg" else "png")  # given "auto" wins
})

test_that("FT-IX3036 [FR-IX392]: rtfreporter_ai_manual(which = \"dev\") ignores ard, shell, size and values", {
  plain <- tempfile("kit"); dir.create(plain)
  extra <- tempfile("kit"); dir.create(extra)
  on.exit(unlink(c(plain, extra), recursive = TRUE), add = TRUE)
  o1 <- rtfreporter_ai_manual(plain, which = "dev")
  o2 <- rtfreporter_ai_manual(extra, which = "dev", ard = TRUE, shell = "bash",
                              size = "short", values = TRUE)
  expect_identical(basename(o2), "rtfreporter-ai-dev-manual.md")
  expect_identical(list.files(extra), "rtfreporter-ai-dev-manual.md")   # no kit, no PASTE.md
  expect_identical(list.files(plain), list.files(extra))
  expect_identical(readLines(o1, warn = FALSE), readLines(o2, warn = FALSE))
})

test_that("FT-IX3037 [FR-IX404]: fmt_numeric(): a rule's own small, an all-missing row needs no key, no numeric column returns the data whole", {
  d <- data.frame(stat = c("a", "a", NA), v = c(1.23456, 0.0123456, NA),
                  w = c("x", "y", "z"), stringsAsFactors = FALSE)
  # the call's small = "signif" is beaten by the rule's own small = "fixed"
  fixed <- fmt_numeric(d, "v", by = "stat",
                       formats = list(a = list(signif = 3, small = "fixed")),
                       small = "signif")
  expect_identical(fixed$v, c("1.23", "0.01", ""))
  # and the call's small applies where the rule says none
  sig <- fmt_numeric(d, "v", by = "stat", formats = list(a = list(signif = 3)),
                     small = "signif")
  expect_identical(sig$v, c("1.23", "0.0123", ""))
  # the row whose selected cell is missing has no key and needs none
  expect_identical(fixed$stat, d$stat)
  # a row with a number but no key does need one
  miss <- data.frame(stat = c("a", NA), v = c(1.5, 2.5))
  expect_error(fmt_numeric(miss, "v", by = "stat",
                           formats = list(a = list(digits = 1))),
               "no format for ''", fixed = TRUE)
  # no numeric column selected: the data comes back whole, before the mode is
  # checked (neither signif nor digits given)
  expect_identical(fmt_numeric(d, "w"), d)
  expect_identical(fmt_numeric(d, "w", by = "stat"), d)
})

# ------------------------------------------------------------------- LS

.g3_ls_df <- function() {
  d <- data.frame(S = c("S1", "S1", "S2"), A = c("a1", "a2", "a3"),
                  B = c("b1", "b2", "b3"), stringsAsFactors = FALSE)
  attr(d$A, "label") <- "Lab A"
  d
}
.g3_ls_cols <- function() {
  list(listing_col("S", width = 6), listing_col("A", width = 6),
       listing_col("B", width = 6))
}

# A wrap function that records how it was called, one entry per call.
.g3_recorder <- function() {
  seen <- list()
  fn <- function(a, b, c, d) {
    seen[[length(seen) + 1L]] <<- list(text = a, width = b, sep = c, layout = d)
    a
  }
  list(fn = fn, seen = function() seen)
}

test_that("FT-LS3000 [FR-LS101 FR-LS105 FR-LS106]: build_listing() columns, gutters named .sp1, .sp2 and the record column", {
  d <- .g3_ls_df()
  b <- build_listing(d, listing_spec(.g3_ls_cols()))
  expect_s3_class(b, "data.frame")
  expect_identical(names(b), c("S", ".sp1", "A", ".sp2", "B", ".rtf_record"))
  expect_identical(names(b)[ncol(b)], ".rtf_record")           # the record last
  expect_identical(b$.sp1, rep("", 6L))                       # blank on every row
  expect_identical(b$.sp2, rep("", 6L))
  expect_s3_class(attr(b, "rtf_listing"), "rtf_listing_spec")
  # record = FALSE: no record column; spacer = FALSE: no gutters
  expect_identical(names(build_listing(d, listing_spec(.g3_ls_cols(), record = FALSE))),
                   c("S", ".sp1", "A", ".sp2", "B"))
  expect_identical(names(build_listing(d, listing_spec(.g3_ls_cols(), spacer = FALSE))),
                   c("S", "A", "B", ".rtf_record"))
  # the record column under the name the spec gives it, and no .rtf_record
  named <- build_listing(d, listing_spec(.g3_ls_cols(), record = "REC"))
  expect_identical(names(named), c("S", ".sp1", "A", ".sp2", "B", "REC"))
  expect_false(".rtf_record" %in% names(named))
})

test_that("FT-LS3001 [FR-LS107]: the omitted label is resolved against the data and handed back on the body", {
  d <- .g3_ls_df()
  b <- build_listing(d, listing_spec(.g3_ls_cols()))
  labels <- function(x) vapply(attr(x, "rtf_listing")$cols, function(cl) cl$label, "")
  expect_identical(labels(b), c("S", "Lab A", "B"))   # attribute, else the name
  # the source's label changes: the body keeps the label it resolved
  attr(d$A, "label") <- "Changed"
  expect_identical(as_rtftables(b)[[1L]]$col_header, list(c("S", "", "Lab A", "", "B")))
  expect_identical(labels(b), c("S", "Lab A", "B"))
  # a second build from the new data derives it again (wrapped to its width)
  again <- build_listing(d, listing_spec(.g3_ls_cols()))
  expect_identical(labels(again), c("S", "Change\nd", "B"))
})

test_that("FT-LS3002 [FR-LS110]: build_listing() |> as_rtftables() renders the same listing as as_rtftables(listing = )", {
  d <- .g3_ls_df()
  spec <- listing_spec(.g3_ls_cols())
  via_body <- as_rtftables(build_listing(d, spec), max_rows = 4)
  direct <- as_rtftables(d, listing = spec, max_rows = 4)
  expect_identical(via_body, direct)
  # two pages: records 1 and 2 (two rows each), then record 3
  expect_length(direct, 2L)
  expect_identical(vapply(direct, function(p) nrow(p$data), 1L), c(4L, 2L))
  expect_identical(direct[[1L]]$data$S, c("S1", "", "S1", ""))
  # the record column decided the breaks and is not printed
  expect_identical(names(direct[[1L]]$data), c("S", ".sp1", "A", ".sp2", "B"))
})

test_that("FT-LS3003 [FR-LS115]: several missing variables stop with the plural message", {
  d <- data.frame(S = c("S1", "S2"), stringsAsFactors = FALSE)
  expect_error(build_listing(d, listing_spec(list(listing_col(c("A", "B"))))),
               "Columns \"A\", \"B\" not in `data` (listing column \"A\").",
               fixed = TRUE)
  expect_error(build_listing(d, listing_spec(list(listing_col("NOPE")))),
               "Column \"NOPE\" not in `data` (listing column \"NOPE\").",
               fixed = TRUE)
})

test_that("FT-LS3004 [FR-LS117]: a wrap returning a bad value stops, for a header and for a cell, with the full message", {
  d <- data.frame(S = c("S1", "S2"), stringsAsFactors = FALSE)
  msg <- "The listing's `wrap` function must return a non-empty character vector of lines."
  sp <- function(w) listing_spec(list(listing_col("S", width = 10)), wrap = w)
  # bad for the derived header text only ("S"); the cells are fine
  expect_error(build_listing(d, sp(function(text, width, sep, layout)
    if (identical(text, "S")) 42 else text)), msg, fixed = TRUE)
  # bad for one cell only
  expect_error(build_listing(d, sp(function(text, width, sep, layout)
    if (identical(text, "S1")) 42 else text)), msg, fixed = TRUE)
  expect_error(build_listing(d, sp(function(text, width, sep, layout) character(0))),
               msg, fixed = TRUE)
})

test_that("FT-LS3005 [FR-LS121 FR-LS124]: the multiline defaults and the default wrapping rule; spacer_rel_width sets the gutters", {
  sp <- listing_spec(list(listing_col("A")))
  expect_identical(sp$spacer_rel_width, 1)
  expect_identical(c(sp$sep, sp$align, sp$layout), c("/", "left", "stack"))
  expect_true(sp$blank_row_first)
  expect_true(sp$blank_row)
  expect_true(sp$spacer)
  # the default rule: each variable on its own line, then a word boundary,
  # then a hard split
  dd <- data.frame(A = "Alpha", B = "one two three four", C = "abcdefghijklmnopq",
                   stringsAsFactors = FALSE)
  b <- build_listing(dd, listing_spec(list(
    listing_col(c("A", "B"), width = 8, name = "AB"),
    listing_col("C", width = 6))))
  expect_identical(b$AB[1:4], c("Alpha/", "one two", "three", "four"))
  expect_identical(b$C[1:3], c("abcdef", "ghijkl", "mnopq"))
  # the gutter widths: 0.25 is kept as a fraction; NULL takes the template's 1
  d2 <- data.frame(A = "a", B = "b")
  cols <- list(listing_col("A", width = 3), listing_col("B", width = 3))
  sp25 <- listing_spec(cols, spacer_rel_width = 0.25)
  expect_identical(as_rtftables(d2, listing = sp25)[[1L]]$col_rel_width,
                   c(3, 0.25, 3))
  expect_identical(as_rtftables(d2, listing = listing_spec(cols))[[1L]]$col_rel_width,
                   c(3, 1, 3))
})

test_that("FT-LS3006 [FR-LS129 FR-LS130]: a custom wrap is called positionally; per variable under stack, whole under flow or with no width", {
  dd <- data.frame(A = "x", B = "y", stringsAsFactors = FALSE)
  rec <- function(...) {
    r <- .g3_recorder()
    build_listing(dd, listing_spec(list(
      listing_col(c("A", "B"), name = "AB", label = "LAB", ...)), wrap = r$fn))
    r$seen()
  }
  texts <- function(s) vapply(s, function(z) z$text, "")
  # stack: the header, then each variable's value, the separator kept at the end
  # of every part but the last
  st <- rec(width = 10, layout = "stack")
  expect_identical(texts(st), c("LAB", "x/", "y"))
  # flow, or no width: the whole composed cell
  expect_identical(texts(rec(width = 10, layout = "flow")), c("LAB", "x/y"))
  expect_identical(texts(rec(layout = "stack")), c("LAB", "x/y"))
  expect_identical(texts(rec(width = 10, sep = " | ")), c("LAB", "x | ", "y"))
  # the arguments: length-1 text, a positive width or NULL, the effective sep,
  # a resolved layout (the function names its formals a, b, c, d: positional)
  for (z in st) {
    expect_type(z$text, "character")
    expect_length(z$text, 1L)
    expect_identical(z$width, 10L)
    expect_identical(z$sep, "/")
    expect_true(z$layout %in% c("stack", "flow"))
  }
  expect_identical(st[[1L]]$layout, "stack")                   # the header's
  expect_identical(st[[2L]]$layout, "flow")                    # a variable's
  none <- rec(layout = "flow")
  expect_null(none[[2L]]$width)
  # wrap = NULL uses the type's own rule
  dflt <- listing_spec(list(listing_col("A", width = 4)), wrap = NULL)
  expect_false(dflt$wrap_custom)
  expect_identical(build_listing(data.frame(A = "abcdefg"), dflt)$A[1:2],
                   c("abcd", "efg"))
})

test_that("FT-LS3007 [FR-LS142]: listing_col(\"USUBJID\") takes the listing's own sep, align, layout and a derived label", {
  cl <- listing_col("USUBJID")
  expect_s3_class(cl, "rtf_listing_col")
  expect_identical(cl$name, "USUBJID")
  expect_null(cl$width)
  expect_null(cl$label)
  expect_null(cl$sep)
  expect_null(cl$align)
  expect_null(cl$layout)
})

test_that("FT-LS3008 [FR-LS145]: width is an integer in characters, a full-width glyph counts as two, NULL never wraps", {
  expect_identical(listing_col("A", width = 15)$width, 15L)
  d <- data.frame(A = "ああいい", B = strrep("word ", 30),
                  stringsAsFactors = FALSE)
  b <- build_listing(d, listing_spec(list(listing_col("A", width = 4)),
                                     spacer = FALSE))
  expect_identical(b$A[1:2], c("ああ", "いい"))   # two glyphs a line
  b3 <- build_listing(d, listing_spec(list(listing_col("A", width = 3)),
                                      spacer = FALSE))
  expect_identical(b3$A[1:4], c("あ", "あ", "い", "い"))
  nowrap <- build_listing(d, listing_spec(list(listing_col("B")), spacer = FALSE))
  expect_identical(nowrap$B[1L], trimws(strrep("word ", 30)))
  expect_length(nowrap$B, 2L)                                  # the text and the blank row
  # width does not set the table width when rel_width differs
  wide <- as_rtftables(d, listing = listing_spec(list(
    listing_col("A", width = 4, rel_width = 9)), spacer = FALSE))[[1L]]
  expect_identical(wide$col_rel_width, 9)
})

test_that("FT-LS3009 [FR-LS149]: fit_listing_widths(labels = ) supplies the derived header", {
  d <- data.frame(USUBJID = c("A1", "A2"), stringsAsFactors = FALSE)
  sp <- listing_spec(list(listing_col("USUBJID")))
  fitted <- fit_listing_widths(d, sp, labels = c(USUBJID = "Unique Subject ID"),
                               total_width = 60)
  expect_identical(fitted$cols[[1L]]$label, "Unique Subject ID")
  expect_identical(fit_listing_widths(d, sp, total_width = 60)$cols[[1L]]$label,
                   "USUBJID")
})

test_that("FT-LS3010 [FR-LS171 FR-LS173 FR-LS178]: listing_wrap(): stack breaks after every separator, word boundaries, no limit", {
  expect_identical(listing_wrap("a/b/c", 20, "/", layout = "stack"), c("a/", "b/", "c"))
  expect_identical(listing_wrap("a/b/c", 20, "/", layout = "flow"), "a/b/c")
  # a listing calls it on one variable's value as "flow"
  r <- .g3_recorder()
  build_listing(data.frame(A = "a/b/c"), listing_spec(list(
    listing_col("A", width = 20, label = "H")), wrap = r$fn))
  expect_identical(vapply(r$seen(), function(z) z$layout, "")[2L], "flow")
  # after a comma and after a hyphen, as far as width allows
  expect_identical(listing_wrap("one,two three-four", 8, ""),
                   c("one,two", "three-", "four"))
  # no limit: the text as it stands, trimmed, whatever the layout
  expect_identical(listing_wrap("  padded text  ", NULL), "padded text")
  expect_identical(listing_wrap("  padded text  ", NULL, layout = "flow"), "padded text")
  expect_identical(listing_wrap("  padded text  ", NULL, layout = "stack"), "padded text")
})

test_that("FT-LS3011 [FR-LS182 FR-LS183]: listing_wrap_code() is rtf_listing_code, prints its lines, and calls the exported helpers", {
  code <- listing_wrap_code("my_wrap")
  expect_s3_class(code, "rtf_listing_code")
  lines <- as.character(unclass(code))
  expect_identical(utils::capture.output(print(code)), lines)
  expect_true(any(grepl("listing_take(", lines, fixed = TRUE)))
  expect_true(any(grepl("listing_disp_width(", lines, fixed = TRUE)))
  expect_true(any(grepl("listing_split_after(", lines, fixed = TRUE)))
  expect_false(any(grepl(":::", lines, fixed = TRUE)))
  env <- new.env()
  eval(parse(text = lines), env)
  expect_setequal(ls(env), c("my_wrap", "my_wrap_flow", "my_wrap_words"))
  expect_identical(env$my_wrap("a/b/c", 20, "/", "stack"),
                   listing_wrap("a/b/c", 20, "/", "stack"))
})

test_that("FT-LS3012 [FR-LS191 FR-LS193]: listing_code() writes each column's own settings; NULL name writes the call alone", {
  sp <- listing_spec(list(
    listing_col(c("A", "B"), width = 12, sep = " | ", rel_width = 7, name = "X"),
    listing_col("C", collapse_repeats = TRUE, label = "Line one\nLine two")))
  code <- listing_code(sp)
  txt <- paste(code, collapse = "\n")
  expect_match(txt, "width = 12", fixed = TRUE)
  expect_match(txt, "sep = \" | \"", fixed = TRUE)
  expect_match(txt, "rel_width = 7", fixed = TRUE)
  expect_match(txt, "name = \"X\"", fixed = TRUE)
  expect_match(txt, "collapse_repeats = TRUE", fixed = TRUE)
  # the column named after its first var does not write name =
  cols <- strsplit(txt, "\n  listing_col", fixed = TRUE)[[1L]][-1L]
  expect_length(cols, 2L)
  expect_false(grepl("name = ", cols[[2L]], fixed = TRUE))
  # the multi-line label is kept through the round trip
  back <- eval(parse(text = code))
  expect_identical(back$cols[[2L]]$label, "Line one\nLine two")
  expect_true(back$cols[[2L]]$collapse_repeats)
  # name = NULL (the default): the call alone, no assignment
  expect_identical(code[[1L]], "listing_spec(list(")
  expect_false(any(grepl("<-", code, fixed = TRUE)))
  expect_identical(listing_code(sp, name = "listing")[[1L]], "listing <- listing_spec(list(")
})

test_that("FT-LS3013 [FR-LS192]: a fitted spec writes width, rel_width and label on every column", {
  d <- data.frame(A = c("aaaa", "bbbbbbbbbb"), C = c("c1", "c2"),
                  stringsAsFactors = FALSE)
  fitted <- fit_listing_widths(d, listing_spec(list(listing_col("A"), listing_col("C"))),
                               total_width = 40)
  txt <- paste(listing_code(fitted), collapse = "\n")
  cols <- strsplit(txt, "\n  listing_col", fixed = TRUE)[[1L]][-1L]
  expect_length(cols, 2L)
  for (cl in cols) {
    expect_match(cl, "[^_]width = [0-9]+", perl = TRUE)
    expect_match(cl, "rel_width = [0-9]+")
    expect_match(cl, "label = ")
  }
})

.g3_fit_df <- function() {
  data.frame(ID = c(rep("S1", 9), "S-LONG-IDENTIFIER-ABCDEFGHIJ"),
             NARROW = rep("n", 10), MID = rep("middle text", 10),
             stringsAsFactors = FALSE)
}
.g3_fit_spec <- function() {
  listing_spec(list(listing_col("ID"), listing_col("NARROW"), listing_col("MID")),
               spacer = FALSE)
}
.g3_widths <- function(spec) vapply(spec$cols, function(cl) as.integer(cl$width), 1L)

test_that("FT-LS3014 [FR-LS209]: a column's demand is the largest of the quantile, the header, the unbreakable token and min_width, scaled to the budget", {
  d <- .g3_fit_df()
  sp <- .g3_fit_spec()
  # probs: the median ignores the one long cell, 1 asks for all of it
  half <- fit_listing_widths(d, sp, total_width = 60, probs = 0.5)
  full <- fit_listing_widths(d, sp, total_width = 60, probs = 1)
  expect_gt(.g3_widths(full)[[1L]], .g3_widths(half)[[1L]])    # the long ID column
  # min_width raises a narrow column's demand before scaling
  demand <- function(...) {
    attr(fit_listing_widths(d, sp, total_width = 60, ...), "rtf_listing_fit",
         exact = TRUE)$demand
  }
  expect_identical(unname(demand(min_width = 6L)), c(6, 6, 11))
  expect_identical(unname(demand(min_width = 12L)), c(12, 12, 12))
  # the scaled widths fill the budget
  expect_identical(sum(.g3_widths(half)), 60L)
  expect_identical(sum(.g3_widths(full)), 60L)
  expect_identical(sum(.g3_widths(fit_listing_widths(d, sp, total_width = 60,
                                                     min_width = 12L))), 60L)
  # the header's height: one line asks for more than three
  lng <- data.frame(SHORT = c("12.5", "36.2"), OTHER = c("aaaa", "bbbb"),
                    stringsAsFactors = FALSE)
  labs <- c(SHORT = "Time since Initial Diagnosis to Date of First Dose of Study Drug (months)",
            OTHER = "Other")
  lsp <- listing_spec(list(listing_col("SHORT"), listing_col("OTHER")), spacer = FALSE)
  hl <- function(n) fit_listing_widths(lng, lsp, total_width = 60, labels = labs,
                                       header_lines = n)$cols[[1L]]$width
  expect_gt(hl(1), hl(3))
})

test_that("FT-LS3015 [FR-LS211]: the fit writes rel_width and a frozen label only where the author set none, and renders the same table", {
  d <- data.frame(A = c("aaa", "bbb"), B = c("cc", "dd"), C = c("e", "f"),
                  stringsAsFactors = FALSE)
  attr(d$B, "label") <- "B label long words here"
  sp <- listing_spec(list(listing_col("A", rel_width = 40, label = "MINE"),
                          listing_col("B"), listing_col("C")))
  fit <- fit_listing_widths(d, sp, total_width = 50)
  w <- .g3_widths(fit)
  # the author's rel_width stays; the others get theirs (the fitted width)
  expect_identical(vapply(fit$cols, function(cl) cl$rel_width, 1),
                   c(40, as.numeric(w[2:3])))
  expect_identical(vapply(fit$cols, function(cl) cl$label, ""),
                   c("MINE", "B label long words here", "C"))
  # the same table as the spec with those widths written by hand
  by_hand <- listing_spec(list(listing_col("A", width = w[[1L]], rel_width = 40, label = "MINE"),
                               listing_col("B", width = w[[2L]]),
                               listing_col("C", width = w[[3L]])))
  expect_identical(as_rtftables(d, listing = fit), as_rtftables(d, listing = by_hand))
})

test_that("FT-LS3016 [FR-LS212 FR-LS214]: page, total_width and font_size_half_points", {
  d <- .g3_fit_df()
  sp <- .g3_fit_spec()
  as_list <- fit_listing_widths(d, sp, page = list(orientation = "portrait", paper_size = "A4"))
  as_page <- fit_listing_widths(d, sp, page = rtf_page(orientation = "portrait", paper_size = "A4"))
  expect_identical(as_list, as_page)
  expect_identical(fit_listing_widths(d, sp, page = NULL), fit_listing_widths(d, sp))
  # the portrait page is narrower than the default landscape one
  expect_lt(sum(.g3_widths(as_page)), sum(.g3_widths(fit_listing_widths(d, sp))))
  # total_width given: the page is ignored
  expect_identical(fit_listing_widths(d, sp, total_width = 50,
                                      page = rtf_page(orientation = "portrait")),
                   fit_listing_widths(d, sp, total_width = 50))
  # the default font size is 18 (9 pt); a smaller font fits more characters across
  expect_identical(fit_listing_widths(d, sp),
                   fit_listing_widths(d, sp, font_size_half_points = 18L))
  expect_gt(sum(.g3_widths(fit_listing_widths(d, sp, font_size_half_points = 16L))),
            sum(.g3_widths(fit_listing_widths(d, sp))))
})

test_that("FT-LS3017 [FR-LS218]: header_lines defaults to 4", {
  expect_identical(formals(fit_listing_widths)$header_lines, 4L)
  lng <- data.frame(SHORT = c("12.5", "36.2"), OTHER = c("aaaa", "bbbb"),
                    stringsAsFactors = FALSE)
  labs <- c(SHORT = "Time since Initial Diagnosis to Date of First Dose of Study Drug (months)",
            OTHER = "Other")
  lsp <- listing_spec(list(listing_col("SHORT"), listing_col("OTHER")), spacer = FALSE)
  omitted <- fit_listing_widths(lng, lsp, total_width = 60, labels = labs)
  four <- fit_listing_widths(lng, lsp, total_width = 60, labels = labs, header_lines = 4)
  expect_identical(.g3_widths(omitted), .g3_widths(four))
  expect_identical(omitted, four)
  expect_false(identical(.g3_widths(omitted),
                         .g3_widths(fit_listing_widths(lng, lsp, total_width = 60,
                                                       labels = labs, header_lines = 1))))
})

test_that("FT-LS3018 [FR-LS247]: catx() stops on an NA sep", {
  msg <- "`sep` must be a single string."
  expect_error(catx(NA, "a"), msg, fixed = TRUE)
  expect_error(catx(NA_character_, "a"), msg, fixed = TRUE)
  expect_identical(catx("-", "a", "b"), "a-b")
})

test_that("FT-LS3019 [FR-LS252]: a font given as a named vector is measured by its latin font", {
  d <- .g3_fit_df()
  sp <- .g3_fit_spec()
  named <- fit_listing_widths(d, sp, font = c(latin = "Arial", east_asian = "SimSun"))
  expect_identical(named, fit_listing_widths(d, sp, font = "Arial"))
  expect_false(identical(.g3_widths(named),
                         .g3_widths(fit_listing_widths(d, sp, font = "Courier New"))))
})

# ------------------------------------------------------------------- PL

# A hierarchical normalized ARD: the overall row, three SOCs with their terms.
# p per arm (A, B): S1 own .50/.50, P1 .10/.10, P2 .10/.05; S2 own .20/.20,
# P3 .35/.10; S3 own .60/.60, P4 .40/.40.
.g3_hier <- function() {
  ents <- list(
    list("..ard_hierarchical_overall..", NA, NA, c(0.9, 0.9)),
    list("AESOC", "S1", "S1", c(0.5, 0.5)), list("AETERM", "S1", "P1", c(0.1, 0.1)),
    list("AETERM", "S1", "P2", c(0.1, 0.05)),
    list("AESOC", "S2", "S2", c(0.2, 0.2)), list("AETERM", "S2", "P3", c(0.35, 0.1)),
    list("AESOC", "S3", "S3", c(0.6, 0.6)), list("AETERM", "S3", "P4", c(0.4, 0.4)))
  out <- list()
  for (e in ents) for (k in 1:2) for (sn in c("n", "p")) {
    term <- identical(e[[1L]], "AETERM")
    out[[length(out) + 1L]] <- data.frame(
      group1 = "TRT", group1_level = c("A", "B")[k],
      group2 = if (term) "AESOC" else NA_character_,
      group2_level = if (term) e[[2L]] else NA_character_,
      variable = e[[1L]], variable_level = if (is.na(e[[3L]])) NA_character_ else e[[3L]],
      context = "hierarchical", stat_name = sn, stat_label = sn,
      stat = if (sn == "n") round(e[[4L]][k] * 20) else e[[4L]][k],
      stringsAsFactors = FALSE)
  }
  normalize_ard(do.call(rbind, out), hierarchy = c("AESOC", "AETERM"),
                overall = "Any TEAE")
}
.g3_hier_plan <- function(...) {
  table_plan(.g3_hier(), cols = "TRT", rows = c(soc = "AESOC", pt = "AETERM")) |>
    plan_cells("{n} ({p:.1f%})", notes = FALSE)
}
.g3_tab <- function(p) as.data.frame(plan_apply(p, "table"))

test_that("FT-PL3000 [FR-PL101]: table_plan() holds the data and its roles with no layers, and computes nothing", {
  d <- .g3_ard()
  p <- table_plan(d, cols = "TRT", rows = c(group = "variable"))
  expect_s3_class(p, "table_plan")
  expect_identical(p$data, d)
  expect_identical(p$roles, list(cols = "TRT", rows = c(group = "variable")))
  expect_length(p$layers, 0L)
  expect_length(suppressMessages(plan_layers(p))$declared, 0L)
  # two different values for one cell would fail at plan_apply(), not here
  clash <- rbind(d, transform(d, stat = stat + 1))
  p2 <- table_plan(clash, cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(stats = "rows", notes = FALSE)
  expect_s3_class(p2, "table_plan")
  expect_error(suppressMessages(plan_apply(p2, "table")),
               "Two different values landed in the same cell")
})

test_that("FT-PL3001 [FR-PL106 FR-PL168]: cols = c(K1, K2) spreads one column per combination, named by the values and the key separator", {
  p <- table_plan(.g3_ard2(), cols = c("TRT", "SEX")) |> plan_cells("{mean}", notes = FALSE)
  expect_identical(names(plan_apply(p, "table")),
                   c("label", "Placebo____F", "Placebo____M", "Drug____F", "Drug____M"))
  q <- plan_columns(p, sep = "--")
  expect_identical(names(plan_apply(q, "table")),
                   c("label", "Placebo--F", "Placebo--M", "Drug--F", "Drug--M"))
  # plan_columns(sep = ) is also the header_sep; plan_col_header(header_sep = ) wins
  expect_identical(plan_apply(q, "args")$rtf$header_sep, "--")
  expect_identical(plan_apply(plan_col_header(q, header_sep = "~"), "args")$rtf$header_sep, "~")
})

test_that("FT-PL3002 [FR-PL109 FR-PL113]: rows left out is the analysis variable in `group`; a guarded label chain", {
  m <- normalize_ard(rbind(
    data.frame(group1 = "TRT", group1_level = rep(c("A", "B"), each = 2L), variable = "AGE",
               variable_level = NA_character_, context = "continuous",
               stat_name = rep(c("N", "mean"), 2L), stat_label = rep(c("N", "Mean"), 2L),
               stat = c(10, 12.34, 12, 15.67), stringsAsFactors = FALSE),
    data.frame(group1 = "TRT", group1_level = rep(c("A", "B"), each = 2L), variable = "HGT",
               variable_level = NA_character_, context = "continuous",
               stat_name = rep(c("N", "mean"), 2L), stat_label = rep(c("N", "Mean"), 2L),
               stat = c(10, 150.1, 12, 160.2), stringsAsFactors = FALSE)))
  tab <- .g3_tab(table_plan(m, cols = "TRT") |> plan_cells(continuous = "{mean}", notes = FALSE))
  expect_true("group" %in% names(tab))
  expect_identical(tab$group, c("AGE", "HGT"))
  # label = c(guard ~ text, ~ template): a missing .label prints "Total"
  dn <- normalize_ard(data.frame(
    group1 = "TRT", group1_level = rep(c("A", "B"), each = 2L), variable = "SEX",
    variable_level = c("F", NA), context = "categorical", stat_name = "n",
    stat_label = "n", stat = 1:4, stringsAsFactors = FALSE))
  lab <- .g3_tab(table_plan(dn, cols = "TRT",
                            label = c(is.na(.label) ~ "Total", ~ "{.label}")) |>
                   plan_cells("{n}", notes = FALSE))
  expect_identical(lab$label, c("F", "Total"))
  expect_identical(lab$A, c("1", "2"))
})

test_that("FT-PL3003 [FR-PL117 FR-PL126 FR-PL128]: an edited normalized frame is the input unchanged; bad input and bad stat stop", {
  d <- .g3_ard()
  f <- d
  f$stat[2L] <- 99
  f$stat_fmt[2L] <- "edited"
  inp <- plan_apply(table_plan(f, cols = "TRT"), "input")
  expect_identical(inp, f)
  expect_identical(inp$stat_fmt[2L], "edited")
  expect_false(identical(inp, plan_apply(table_plan(d, cols = "TRT"), "input")))
  # not a data frame
  expect_error(table_plan(1:3, cols = "TRT"), "NORMALIZED frame", fixed = TRUE)
  expect_error(table_plan(list(a = 1), cols = "TRT"), "NORMALIZED frame", fixed = TRUE)
  expect_error(table_plan("x", cols = "TRT"), "NORMALIZED frame", fixed = TRUE)
  # stat = repeated, unnamed or unknown parts
  full <- "table_plan(stat = ) names the parts of a statistic your frame calls something else"
  expect_error(table_plan(d, cols = "TRT", stat = c(value = "x", value = "y")), full, fixed = TRUE)
  expect_error(table_plan(d, cols = "TRT", stat = c("x", "y")), full, fixed = TRUE)
  expect_error(table_plan(d, cols = "TRT", stat = c(bogus = "x")), full, fixed = TRUE)
})

test_that("FT-PL3004 [FR-PL121]: a hierarchy with an overall row: the overall row first, one row per SOC and per term", {
  tab <- .g3_tab(.g3_hier_plan())
  n_soc <- 3L
  n_pt <- 4L                                       # (SOC, term) pairs
  expect_identical(nrow(tab), 1L + n_soc + n_pt)
  expect_identical(tab$label[1L], "Any TEAE")
  expect_identical(tab$soc[1L], "Any TEAE")
  expect_identical(sum(is.na(tab$pt) & tab$soc != "Any TEAE"), n_soc)
  expect_identical(sum(!is.na(tab$pt)), n_pt)
  expect_identical(tab$label, c("Any TEAE", "S1", "P1", "P2", "S2", "P3", "S3", "P4"))
})

test_that("FT-PL3005 [FR-PL144 FR-PL145 FR-PL149]: plan_filter() parents keep / drop, and the unnamed-condition stop", {
  base <- .g3_hier_plan()
  keep <- .g3_tab(plan_filter(base, p >= 0.30))
  # a SOC stays exactly when a child is kept: S2 (own .20, P3 .35) and S3
  expect_identical(keep$label, c("Any TEAE", "S2", "P3", "S3", "P4"))
  expect_false("S1" %in% keep$label)        # own .50 passes, no child does
  expect_identical(.g3_tab(plan_filter(base, p >= 0.30, parents = "keep")), keep)
  # drop: a SOC is judged by its own counts: S1 stays, S2 is gone, its P3 stays
  drop <- .g3_tab(plan_filter(base, p >= 0.30, parents = "drop"))
  expect_identical(drop$label, c("Any TEAE", "S1", "P3", "S3", "P4"))
  # conditions are unnamed
  msg <- "plan_filter() takes conditions, unnamed: `plan_filter(p >= 0.05)`."
  expect_error(plan_filter(base), msg, fixed = TRUE)
  expect_error(plan_filter(base, keep = p > 0), msg, fixed = TRUE)
})

test_that("FT-PL3006 [FR-PL201]: plan_apply(stage = \"input\") on a finished table or a listing is the data itself", {
  wide <- data.frame(label = c("a", "b"), v = c("1.5", "2.5"), stringsAsFactors = FALSE)
  expect_identical(plan_apply(table_plan(wide) |> plan_columns(), "input"), wide)
  expect_identical(plan_apply(table_plan(wide) |> plan_listing(listing_col("label", width = 5),
                                                                listing_col("v", width = 5)),
                              "input"), wide)
})

test_that("FT-PL3007 [FR-PL206]: an error inside a stage names the verbs that declared it with their own arguments", {
  two <- normalize_ard(data.frame(
    group1 = "TRT", group1_level = rep(c("A", "B"), each = 4L), variable = "SEX",
    variable_level = rep(c("F", "F", "M", "M"), 2L), context = "categorical",
    stat_name = rep(c("n", "p"), 4L), stat_label = rep(c("n", "%"), 4L),
    stat = c(4, 0.4, 6, 0.6, 5, 0.5, 5, 0.5), stringsAsFactors = FALSE))
  p <- table_plan(two, cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(categorical = c(a = "{n:.0f}"), notes = FALSE) |>
    plan_labels(SEX = "Sex")
  e <- tryCatch(plan_apply(p, "table"), error = function(e) e)
  expect_s3_class(e, "error")
  msg <- conditionMessage(e)
  expect_match(msg, paste0("\n  declared by:\n    ",
                           "plan_cells(categorical = c(a = \"{n:.0f}\"), notes = FALSE)"),
               fixed = TRUE)
  expect_match(msg, "\n    plan_labels(SEX = \"Sex\")", fixed = TRUE)
  # the verbs' own arguments, not the whole pipeline
  expect_no_match(msg, "table_plan(", fixed = TRUE)
})

test_that("FT-PL3008 [FR-PL211]: plan_layers() declared order; after and restyle one entry per call; a later plan_cells(default =) wins", {
  p <- table_plan(.g3_ard(), cols = "TRT") |>
    plan_cells("{mean}", notes = FALSE) |>
    plan_after(function(x) x) |> plan_after(function(x) x) |>
    plan_cell_style(cols = "A", bold = TRUE) |> plan_cell_style(cols = "B", italic = TRUE) |>
    plan_cells(default = "{sd}", notes = FALSE)
  lay <- plan_layers(p)
  expect_identical(lay$declared, c("cells", "cell_options", "after", "after", "restyle",
                                   "restyle", "cells", "cell_options"))
  expect_length(lay$layers$after, 2L)
  expect_length(lay$layers$restyle, 2L)
  expect_identical(lay$layers$cells, list(default = "{sd}"))
})

test_that("FT-PL3009 [FR-PL213 FR-PL214]: plan_layers()$cells parses the templates; header n_text and cell rows", {
  d <- .g3_ard()
  lay <- plan_layers(table_plan(d, cols = "TRT") |>
                       plan_cells("AGE\rcontinuous" = c(Mean = "{mean:.1f}", n = "{N}"),
                                  notes = FALSE))
  expect_length(lay$cells, 1L)
  expect_identical(lay$cells[[1L]]$variable, "AGE")
  expect_identical(lay$cells[[1L]]$context, "continuous")
  expect_identical(lay$cells[[1L]]$entry$labels, c("Mean", "n"))
  expect_identical(lay$cells[[1L]]$entry$chains[[1L]][[1L]]$tpl, "{mean:.1f}")
  expect_identical(lay$cells[[1L]]$entry$chains[[2L]][[1L]]$tpl, "{N}")
  # NULL when the statistics are rows or no template is declared
  expect_null(plan_layers(table_plan(d, cols = "TRT") |>
                            plan_cells(stats = "rows", notes = FALSE))$cells)
  expect_null(suppressMessages(plan_layers(table_plan(d, cols = "TRT") |> plan_columns()))$cells)
  # the header: {n} scope as text, or NULL
  hdr <- function(v) plan_layers(table_plan(d, cols = "TRT") |>
                                   plan_cells("{mean}", notes = FALSE) |>
                                   plan_col_header(values = v))$header
  expect_identical(hdr(list(n = "page"))$n_text, "page")
  expect_identical(hdr(list(n = "table"))$n_text, "table")
  expect_null(hdr(c(A = 86, B = 84))$n_text)
  built <- plan_layers(table_plan(d, cols = "TRT") |> plan_cells("{mean}", notes = FALSE) |>
                         plan_col_header(header = rtf_col_header("{col} (N={n})"),
                                         values = c(A = 86, B = 84)))$header
  expect_identical(built$source, "resolved")
  expect_true(built$literal_n)
  expect_identical(built$cells$cols, c("label", ".values"))
  expect_identical(built$cells$span, c(NA, "each"))
  expect_identical(built$cells$text, c(NA, "{col} (N={n})"))
})

test_that("FT-PL3010 [FR-PL227]: variable, context, kind and default entries are picked by specificity", {
  b <- function(v, x, ctx) data.frame(
    group1 = "TRT", group1_level = rep(c("A", "B"), each = 2L), variable = v,
    variable_level = NA_character_, context = ctx, stat_name = rep(c("N", "mean"), 2L),
    stat_label = rep(c("N", "Mean"), 2L), stat = x, stringsAsFactors = FALSE)
  m <- normalize_ard(rbind(
    b("AGE", c(10, 12.34, 12, 15.67), "mysummary"),
    b("HGT", c(10, 150.123, 12, 160.456), "mysummary"),
    b("WGT", c(10, 70.1, 12, 71.2), "other"),
    data.frame(group1 = "TRT", group1_level = rep(c("A", "B"), each = 2L), variable = "SEX",
               variable_level = "F", context = "categorical", stat_name = rep(c("n", "p"), 2L),
               stat_label = rep(c("n", "%"), 2L), stat = c(4, 0.4, 5, 0.5),
               stringsAsFactors = FALSE)))
  tab <- .g3_tab(table_plan(m, cols = "TRT") |>
                   plan_cells("{n}", continuous = "{mean:.1f}", mysummary = "{N}",
                              HGT = "{mean:.2f}", notes = FALSE))
  expect_identical(tab$group, c("AGE", "HGT", "WGT", "SEX"))
  expect_identical(tab$A, c("10", "150.12", "70.1", "4"))   # context, variable, kind, default
  expect_identical(tab$B, c("12", "160.46", "71.2", "5"))
})

test_that("FT-PL3011 [FR-PL232 FR-PL233]: notes = TRUE says which statistics no template used, also from rtf_tables(); an option-free plan_cells() adds no template layer", {
  d <- .g3_ard()
  loud <- table_plan(d, cols = "TRT") |> plan_cells("{mean}")
  said <- character()
  withCallingHandlers(rtf_tables(rtf_document(), loud), message = function(m) {
    said <<- c(said, conditionMessage(m))
    invokeRestart("muffleMessage")
  })
  expect_match(paste(said, collapse = ""), "no template named it", fixed = TRUE)
  quiet <- table_plan(d, cols = "TRT") |> plan_cells("{mean}", notes = FALSE)
  expect_no_message(rtf_tables(rtf_document(), quiet))
  # stats = / na = with no template: the resolved cells are the earlier call's
  first <- plan_layers(table_plan(d, cols = "TRT") |> plan_cells("{n}", notes = FALSE))
  later <- plan_layers(table_plan(d, cols = "TRT") |> plan_cells("{n}", notes = FALSE) |>
                         plan_cells(stats = "cells", na = "-"))
  expect_identical(later$layers$cells, first$layers$cells)
  expect_identical(later$layers$cells, list(default = "{n}"))
  again <- plan_layers(table_plan(d, cols = "TRT") |> plan_cells("{N}", notes = FALSE) |>
                         plan_cells(notes = FALSE))
  expect_identical(again$layers$cells, list(default = "{N}"))
})

.g3_lab_plan <- function() {
  tt <- normalize_ard(rbind(
    data.frame(group1 = "TRT", group1_level = rep(c("A", "B"), each = 2L), variable = "LB",
               variable_level = NA_character_, context = "continuous",
               stat_name = rep(c("N", "mean"), 2L), stat_label = rep(c("N", "Mean"), 2L),
               stat = c(10, 1.5, 12, 2.5), PARAM = "AST", stringsAsFactors = FALSE),
    data.frame(group1 = "TRT", group1_level = rep(c("A", "B"), each = 2L), variable = "LB",
               variable_level = NA_character_, context = "continuous",
               stat_name = rep(c("N", "mean"), 2L), stat_label = rep(c("N", "Mean"), 2L),
               stat = c(10, 3.5, 12, 4.5), PARAM = "ALT", stringsAsFactors = FALSE)))
  table_plan(tt, cols = "TRT", rows = c(param = "PARAM")) |> plan_cells("{mean}", notes = FALSE)
}

test_that("FT-PL3012 [FR-PL254]: later plan_levels() add a key; the same key is replaced; a map handed whole is its entries", {
  p <- .g3_lab_plan() |> plan_levels(TRT = c("B", "A")) |>
    plan_levels(list(PARAM = c("AST", "ALT")))
  expect_identical(plan_layers(p)$layers$levels$levels,
                   list(TRT = c("B", "A"), PARAM = c("AST", "ALT")))
  expect_identical(names(.g3_tab(p)), c("param", "label", "B", "A"))
  q <- plan_levels(p, TRT = c("A", "B"))
  expect_identical(plan_layers(q)$layers$levels$levels,
                   list(TRT = c("A", "B"), PARAM = c("AST", "ALT")))
  expect_identical(names(.g3_tab(q)), c("param", "label", "A", "B"))
  whole <- plan_levels(p, list(TRT = c("A", "B"), PARAM = c("ALT", "AST")))
  expect_identical(plan_layers(whole)$layers$levels$levels,
                   list(TRT = c("A", "B"), PARAM = c("ALT", "AST")))
  expect_identical(as.character(.g3_tab(whole)$param), c("ALT", "AST"))
  expect_identical(plan_layers(plan_levels(.g3_lab_plan(), c(TRT = "B")))$layers$levels$levels,
                   list(TRT = "B"))
})

test_that("FT-PL3013 [FR-PL257]: plan_labels() prints the text without ordering; later calls add or replace a key's entry", {
  p <- .g3_lab_plan()
  one <- .g3_tab(plan_labels(p, TRT = c(B = "Drug")))
  expect_identical(names(one), c("param", "label", "A", "Drug"))   # order unchanged
  expect_identical(one$Drug, c("2.5", "4.5"))
  both <- plan_labels(p, TRT = c(B = "Drug")) |> plan_labels(PARAM = c(AST = "Aspartate"))
  expect_identical(names(.g3_tab(both)), c("param", "label", "A", "Drug"))
  expect_identical(as.character(.g3_tab(both)$param), c("Aspartate", "ALT"))
  expect_identical(names(plan_layers(both)$layers$labels$labels), c("TRT", "PARAM"))
  repl <- plan_labels(p, TRT = c(B = "Drug")) |> plan_labels(TRT = c(A = "Active"))
  expect_identical(names(.g3_tab(repl)), c("param", "label", "Active", "B"))
  expect_identical(plan_layers(repl)$layers$labels$labels, list(TRT = c(A = "Active")))
})

test_that("FT-PL3014 [FR-PL268]: plan_total() makes no Total column for records with no outer key", {
  rec <- function(g1, l1, g2, l2, x) data.frame(
    group1 = g1, group1_level = l1, group2 = g2, group2_level = l2, variable = "AGE",
    variable_level = NA_character_, context = "continuous", stat_name = "mean",
    stat_label = "Mean", stat = x, stringsAsFactors = FALSE)
  na <- NA_character_
  t2 <- normalize_ard(rbind(
    rec("TRT", rep(c("A", "B"), each = 2L), "SEX", rep(c("F", "M"), 2L), c(1, 2, 3, 4)),
    rec("TRT", c("A", "B"), na, na, c(10, 20)),
    rec("SEX", c("F", "M"), na, na, c(30, 40)),          # SEX set, no TRT
    rec(na, na, na, na, 99)))
  expect_true(anyNA(t2$TRT) && !all(is.na(t2$SEX[is.na(t2$TRT)])))
  p <- table_plan(t2, cols = c("TRT", "SEX")) |> plan_cells("{mean}", notes = FALSE) |>
    plan_total()
  tab <- .g3_tab(p)
  expect_identical(names(tab), c("label", "A____F", "A____M", "A____Total",
                                 "B____F", "B____M", "B____Total"))
  expect_false("NA____Total" %in% names(tab))
  expect_identical(tab$A____Total, "10")
})

test_that("FT-PL3015 [FR-PL305]: a row key's empty value (a group's own row) comes first in its group, ascending or descending", {
  for (key in c("pt", "-pt")) {
    tab <- .g3_tab(.g3_hier_plan() |> plan_sort(key))
    for (s in c("S1", "S2", "S3")) {
      expect_true(is.na(tab$pt[tab$soc == s][1L]), info = paste(key, s))
    }
  }
  asc <- .g3_tab(.g3_hier_plan() |> plan_sort("pt"))
  desc <- .g3_tab(.g3_hier_plan() |> plan_sort("-pt"))
  expect_identical(asc$pt[asc$soc == "S1"], c(NA, "P1", "P2"))
  expect_identical(desc$pt[desc$soc == "S1"], c(NA, "P2", "P1"))
})

test_that("FT-PL3016 [FR-PL307]: .overall puts the overall rows first", {
  p <- .g3_hier_plan()
  asc <- .g3_tab(p |> plan_sort(".overall", "soc"))
  expect_identical(asc$soc[1L], "Any TEAE")
  expect_identical(asc$soc[2:8], c("S1", "S1", "S1", "S2", "S2", "S3", "S3"))
  desc <- .g3_tab(p |> plan_sort(".overall", "-soc"))
  expect_identical(desc$soc[1L], "Any TEAE")
  expect_identical(desc$soc[2L], "S3")
})

test_that("FT-PL3017 [FR-PL331]: plan_empty() twice merges field by field", {
  d <- .g3_hier()
  p <- table_plan(d, cols = "TRT", rows = c(soc = "AESOC")) |>
    plan_cells("{n}", notes = FALSE)
  empty <- function(p) plan_layers(p)$layers$empty
  one <- p |> plan_empty(footnote = "x") |> plan_empty(text = "a")
  expect_identical(empty(one)$footnote, "x")     # a field left out keeps the earlier
  expect_identical(empty(one)$text, "a")         # the later text wins
  two <- one |> plan_empty(footnote = "y")
  expect_identical(empty(two)$footnote, "y")     # the later footnote replaces
  expect_identical(empty(two)$text, "a")
  three <- p |> plan_empty(text = "a", when = "no_rows") |> plan_empty(position = "first")
  expect_identical(empty(three)$when, "no_rows")
  expect_identical(empty(three)$position, "first")
})

.g3_template <- function(...) {
  code <- NULL
  utils::capture.output(code <- plan_template(...))
  code
}

test_that("FT-PL3018 [FR-PL350]: plan_template() prints and returns the pipeline code, which runs to RTF pages", {
  ard <- .g3_ard()
  out <- utils::capture.output(res <- withVisible(plan_template(ard, cols = "TRT")))
  expect_false(res$visible)
  expect_type(res$value, "character")
  expect_true(length(out) > 5L)                       # it also prints the code
  code <- res$value
  txt <- paste(code, collapse = "
")
  has <- function(x) grepl(x, txt, fixed = TRUE)
  expect_true(has("rtfreporter::normalize_ard()"))
  expect_true(has("rtfreporter::table_plan("))
  expect_true(has("cols = \"TRT\""))
  expect_true(has("rtfreporter::plan_cells("))
  expect_true(has("\"Mean (SD)\" = \"{mean} ({sd})\""))
  expect_true(has("plan_stub(\n    name = \"row_label\""))
  expect_true(has("plan_row_group(mode = \"indent\")"))
  expect_true(has("plan_blanks(\"between_groups\")"))
  expect_true(has("plan_paginate_rows(max_rows = 22, split = \"group_safe\")"))
  expect_true(has("plan_style(border = \"tfl\")"))
  # the code, read back, is a table_plan that runs to RTF pages
  skip_if_not_installed("magrittr")
  env <- list2env(list(ard = ard), parent = globalenv())
  p <- eval(parse(text = code), envir = env)
  expect_s3_class(p, "table_plan")
  pages <- plan_apply(p, "pages")
  expect_true(length(pages) >= 1L)
  expect_s3_class(pages[[1L]], "rtftable")
  out <- .g3_render(rtf_document() |> rtf_tables(p))
  expect_true(grepl("^\\{\\\\rtf1", out))
})

test_that("FT-PL3019 [FR-PL352]: the column header is written only for one cols key with one denominator per column; otherwise a comment explains", {
  ard <- .g3_ard()
  one <- .g3_template(ard, cols = "TRT")
  expect_true(any(grepl("rtfreporter::plan_col_header(", one, fixed = TRUE)))
  # two cols keys: no header verb, an explaining comment, and the code still parses
  two <- .g3_template(.g3_ard2(), cols = c("TRT", "SEX"))
  expect_false(any(grepl("rtfreporter::plan_col_header(", two, fixed = TRUE)))
  expect_true(any(grepl("^# Several column keys", two)))
  expect_true(any(grepl("header_sep", two, fixed = TRUE)))
  expect_no_error(parse(text = two))
  # no denominator the ARD carries: the header verb is left out, the pipeline parses
  nod <- ard[!ard$stat_name %in% c("N"), ]
  none <- .g3_template(nod, cols = "TRT")
  expect_false(any(grepl("rtfreporter::plan_col_header(", none, fixed = TRUE)))
  expect_true(any(grepl("^#", none)))
  expect_no_error(parse(text = none))
  # the trailing pipe is removed: the last verb of the pipeline ends its line
  last <- grep("plan_style(border = \"tfl\")", two, fixed = TRUE, value = TRUE)
  expect_false(grepl("%>%\\s*$", last))
})

test_that("FT-PL3020 [FR-PL355]: plan_template(form = \"widen\") writes normalize_ard() into widen_ard() into as_rtftables()", {
  ard <- .g3_ard()
  code <- .g3_template(ard, cols = "TRT", form = "widen")
  expect_true(any(grepl("generated by rtfreporter::plan_template(form = \"widen\")",
                        code, fixed = TRUE)))
  txt <- paste(code, collapse = "\n")
  at <- function(x) regexpr(x, txt, fixed = TRUE)[[1L]]
  expect_true(at("normalize_ard(") > 0L)
  expect_true(at("normalize_ard(") < at("widen_ard("))
  expect_true(at("widen_ard(") < at("as_rtftables("))
  expect_false(grepl("table_plan(", txt, fixed = TRUE))
  expect_true(grepl("cols  = \"TRT\"", txt, fixed = TRUE))
  # the keys outside cols are the rows
  two <- paste(.g3_template(.g3_ard2(), cols = "TRT", form = "widen"), collapse = "\n")
  expect_true(grepl("SEX", two, fixed = TRUE))
})

test_that("FT-PL3021 [FR-PL358]: the pipe follows the option, then RStudio's preference, then %>%", {
  ard <- .g3_ard()
  old <- options(rtfreporter.ard_pipe = NULL)
  on.exit(options(old), add = TRUE)
  pipe_in <- function(code) {
    line <- grep("^p <- ard ", code, value = TRUE)[1L]
    sub("^.* ([^ ]+)$", "\\1", line)
  }
  testthat::local_mocked_bindings(.ard_pipe_rstudio = function() "|>",
                                  .package = "rtfreporter")
  expect_identical(rtfreporter:::.ard_pipe_op(), "|>")
  expect_identical(pipe_in(.g3_template(ard, cols = "TRT", pipe = NULL)), "|>")
  # the option comes before RStudio's preference
  options(rtfreporter.ard_pipe = "%>%")
  expect_identical(rtfreporter:::.ard_pipe_op(), "%>%")
  expect_identical(pipe_in(.g3_template(ard, cols = "TRT", pipe = NULL)), "%>%")
  options(rtfreporter.ard_pipe = NULL)
  testthat::local_mocked_bindings(.ard_pipe_rstudio = function() "%>%",
                                  .package = "rtfreporter")
  expect_identical(rtfreporter:::.ard_pipe_op(), "%>%")
  testthat::local_mocked_bindings(.ard_pipe_rstudio = function() NULL,
                                  .package = "rtfreporter")
  expect_identical(rtfreporter:::.ard_pipe_op(), "%>%")
  expect_error(plan_template(ard, cols = "TRT", pipe = "&&"),
               "`pipe` must be \"%>%\" \\(magrittr\\), \"\\|>\"")
})

# A stub cell without its indent (spaces or no-break spaces).
.g3_plain <- function(x) sub("^[^[:alnum:]]+", "", as.character(x))

# Three row keys (a constant site, the SOC, the PT) on the hierarchy ARD.
.g3_site_plan <- function() {
  d <- .g3_hier()
  d$site <- "Site 1"
  table_plan(d, cols = "TRT", rows = c(site = "site", soc = "AESOC", pt = "AETERM")) |>
    plan_cells("{n}", notes = FALSE)
}

test_that("FT-PL3022 [FR-PL361 FR-PL476]: the derived stub leaves out the page key and the hidden columns; its headings are the outermost printed key", {
  p <- .g3_site_plan()
  expect_identical(names(.g3_tab(p)), c("site", "soc", "pt", "label", "A", "B"))
  # no page key, nothing hidden: every row key and the label are folded
  all <- plan_apply(p |> plan_stub(), "pages")
  expect_identical(names(all[[1L]]$data)[1L], "site / soc / pt / label")
  # the page key (site) and the hidden column (pt) are not in the stub
  q <- p |> plan_paginate_group(col = "site", keep = FALSE) |> plan_hide("pt") |>
    plan_stub()
  pg <- plan_apply(q, "pages")
  expect_identical(names(pg), "Site 1")
  expect_identical(names(pg[[1L]]$data), c("soc / label", "A", "B"))
  # the headings are the SOC values (the outermost printed key), not the page key's
  stub <- as.character(pg[[1L]]$data[[1L]])
  expect_identical(grepl("^[^[:alnum:]]", stub),
                   c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE))
  stub <- .g3_plain(stub)
  expect_identical(stub, c("Any TEAE", "S1", "P1", "P2", "S2", "P3", "S3", "P4"))
  expect_false("Site 1" %in% stub)
  # a different page key: the stub is left with the other keys
  r <- p |> plan_paginate_group(col = "soc", keep = FALSE) |> plan_stub()
  one <- plan_apply(r, "pages")[["S1"]]
  expect_identical(names(one$data)[1L], "site / pt / label")
})

test_that("FT-PL3023 [FR-PL455]: with plan_stub() the pages are cut by value, then each page's stub is folded", {
  p <- .g3_site_plan()
  pg <- plan_apply(p |> plan_paginate_group(col = "soc", keep = FALSE) |> plan_stub(),
                   "pages")
  expect_identical(names(pg), c("Any TEAE", "S1", "S2", "S3"))
  wide <- .g3_tab(p)
  ref <- as_rtftables(wide, stub = stub_spec(c("site", "pt", "label")),
                      split = "by_value", group_col = "soc")
  expect_identical(names(ref), names(pg))
  for (nm in names(pg)) {
    got <- as.data.frame(pg[[nm]]$data)
    want <- as.data.frame(ref[[nm]]$data)
    want <- want[setdiff(names(want), "soc")]
    expect_identical(names(got), names(want))
    expect_identical(lapply(got, as.vector), lapply(want, as.vector))
  }
  # the stub is folded per page: the S1 page holds the heading and its two PTs only
  expect_identical(.g3_plain(pg[["S1"]]$data[[1L]]), c("Site 1", "S1", "P1", "P2"))
  expect_identical(.g3_plain(pg[["S2"]]$data[[1L]]), c("Site 1", "S2", "P3"))
})

test_that("FT-PL3024 [FR-PL373]: a header set with rtf_col_header() is on every page, and a full-length row stays positional", {
  p <- .g3_site_plan() |> plan_paginate_group(col = "soc", keep = FALSE) |> plan_stub()
  hdr <- rtf_col_header(c("Term", "Group A", "Group B"))
  pg <- plan_apply(p |> plan_col_header(header = hdr), "pages")
  expect_length(pg, 4L)
  for (page in pg) {
    expect_identical(unname(unlist(page$col_header)), c("Term", "Group A", "Group B"))
  }
  # the same as set_col_header() on each page of the plan without the header
  bare <- plan_apply(p, "pages")
  for (i in seq_along(pg)) {
    want <- set_col_header(bare[[i]], hdr)
    expect_identical(pg[[i]]$col_header, want$col_header)
  }
  # one per-column text stays in its column
  expect_identical(vapply(pg, function(x) x$col_header[[1L]][[3L]], ""),
                   setNames(rep("Group B", 4L), names(pg)))
})

test_that("FT-PL3025 [FR-PL374 FR-PL384]: {col} is the column's name and {n} its own number, from a vector named by column", {
  p <- table_plan(.g3_ard(), cols = "TRT") |> plan_cells("{mean}", notes = FALSE)
  hdr <- rtf_col_header(c("Statistic", "{col} (N={n})"))
  pg <- plan_apply(p |> plan_col_header(values = c(A = 86, B = 84), hdr), "pages")
  expect_identical(unname(pg[[1L]]$col_header[[1L]]),
                   c("Statistic", "A (N=86)", "B (N=84)"))
  # the order of the vector does not matter: each column takes its own number
  rev <- plan_apply(p |> plan_col_header(values = c(B = 84, A = 86), hdr), "pages")
  expect_identical(unname(rev[[1L]]$col_header[[1L]]),
                   c("Statistic", "A (N=86)", "B (N=84)"))
  # one unnamed number fills every cell
  one <- plan_apply(p |> plan_col_header(values = 99, hdr), "pages")
  expect_identical(unname(one[[1L]]$col_header[[1L]]),
                   c("Statistic", "A (N=99)", "B (N=99)"))
})

test_that("FT-PL3026 [FR-PL375]: a header row shorter than the table repeats its last cell over the value columns", {
  p <- table_plan(.g3_ard2(), cols = c("TRT", "SEX"), rows = c(group = "variable")) |>
    plan_cells("{mean}", notes = FALSE)
  expect_identical(names(.g3_tab(p)),
                   c("group", "label", "Placebo____F", "Placebo____M", "Drug____F",
                     "Drug____M"))
  pg <- plan_apply(p |> plan_col_header(rtf_col_header(c("Grp", "Stat", "X"))), "pages")
  hdr <- unname(pg[[1L]]$col_header[[1L]])
  expect_length(hdr, 6L)
  expect_identical(hdr, c("Grp", "Stat", "X", "X", "X", "X"))
})

test_that("FT-PL3027 [FR-PL376]: with several cols keys {col} is the leaf and {col1}, {col2} the levels; the joined name is never printed", {
  p <- table_plan(.g3_ard2(), cols = c("TRT", "SEX"), rows = c(group = "variable")) |>
    plan_cells("{mean}", notes = FALSE)
  pg <- plan_apply(p |> plan_col_header(rtf_col_header(
    c("G", "S", "{col1}"), c("G", "S", "{col2}"), c("G", "S", "{col}"))), "pages")
  rows <- lapply(pg[[1L]]$col_header, unname)
  expect_identical(rows[[1L]], c("G", "S", "Placebo", "Placebo", "Drug", "Drug"))
  expect_identical(rows[[2L]], c("G", "S", "F", "M", "F", "M"))
  expect_identical(rows[[3L]], rows[[2L]])             # {col} is the last key's value
  expect_false(any(grepl("____", unlist(rows), fixed = TRUE)))
  # all three in one cell
  one <- plan_apply(p |> plan_col_header(rtf_col_header(c("G", "S", "{col1}|{col2}|{col}"))),
                    "pages")
  expect_identical(unname(one[[1L]]$col_header[[1L]])[3:4], c("Placebo|F|F", "Placebo|M|M"))
})

test_that("FT-PL3028 [FR-PL377]: {n:sum} over columns that include a Total column leaves the Total out", {
  rec <- function(g1, l1, x) data.frame(
    group1 = g1, group1_level = l1, variable = "AGE", variable_level = NA_character_,
    context = "continuous", stat_name = "mean", stat_label = "mean", stat = x,
    stringsAsFactors = FALSE)
  na <- NA_character_
  d <- normalize_ard(rbind(rec("TRT", c("A", "B"), c(1, 2)), rec(na, na, 1.5)))
  p <- table_plan(d, cols = "TRT") |> plan_cells("{mean}", notes = FALSE) |> plan_total()
  expect_identical(names(.g3_tab(p)), c("label", "A", "B", "Total"))
  hdr <- rtf_col_header(
    list(col_cell(1L, "S"), col_cell(c(2L, 4L), "ALL {n:sum}")),
    c("", "{col} {n}", "{col} {n}", "{col} {n}"))
  pg <- plan_apply(p |> plan_col_header(values = c(A = 10, B = 12, Total = 22), hdr),
                   "pages")
  top <- pg[[1L]]$col_header[[1L]]
  expect_identical(top[[2L]]$label, "ALL 22")          # 10 + 12, not 44
  expect_identical(unname(pg[[1L]]$col_header[[2L]]),
                   c("", "A 10", "B 12", "Total 22"))
})

test_that("FT-PL3029 [FR-PL381]: a header sheet's cols by position, span \"each\" and a key's name, resolved on the page and read back as cells", {
  p <- table_plan(.g3_ard2(), cols = c("TRT", "SEX"), rows = c(group = "variable")) |>
    plan_cells("{mean}", notes = FALSE)
  hd <- data.frame(
    line = c(1, 1, 1, 2, 2, 2), cols = c("1", "2", ".values", "1", "2", ".values"),
    span = c(NA, NA, "TRT", NA, NA, "each"),
    text = c("G", "S", "{col1}", "", "", "{col}"), stringsAsFactors = FALSE)
  q <- plan_col_header(p, hd)
  cells <- plan_layers(q)$header$cells
  expect_identical(plan_layers(q)$header$source, "cells")
  expect_identical(cells$cols, c("1", "2", ".values", "1", "2", ".values"))
  expect_identical(cells$span, c(NA, NA, "TRT", NA, NA, "each"))
  expect_identical(cells$text, c("G", "S", "{col1}", "", "", "{col}"))
  pg <- plan_apply(q, "pages")
  top <- pg[[1L]]$col_header[[1L]]
  # a position is a cell over that column; a key's name makes a cell per value
  expect_identical(vapply(top, function(z) z$label, ""), c("G", "S", "Placebo", "Drug"))
  expect_identical(vapply(top, function(z) z$from, 1L), c(1L, 2L, 3L, 5L))
  expect_identical(vapply(top, function(z) z$to, 1L), c(1L, 2L, 4L, 6L))
  # "each": a cell a column
  expect_identical(unname(pg[[1L]]$col_header[[2L]]), c("", "", "F", "M", "F", "M"))
  # a range ends at "last"; NA is one cell over all of it
  one <- plan_apply(plan_col_header(p, data.frame(
    line = 1, cols = c("1", "3:last"), text = c("G", "ALL"), stringsAsFactors = FALSE)),
    "pages")
  cell <- one[[1L]]$col_header[[1L]]
  expect_identical(vapply(cell, function(z) z$label, ""), c("G", "", "ALL"))
  expect_identical(c(cell[[3L]]$from, cell[[3L]]$to), c(3L, 6L))
})

test_that("FT-PL3030 [FR-PL385]: values = list(n = \"page\", N = \"table\") takes the page's population for {n} and the whole table's for {N}", {
  skip_if_not_installed("cards")
  adsl <- data.frame(USUBJID = sprintf("S%02d", 1:20), stringsAsFactors = FALSE)
  lb <- expand.grid(USUBJID = adsl$USUBJID, PARAM = c("ALT", "HGB"),
                    stringsAsFactors = FALSE)
  lb$BGRADE <- rep(c("G0", "G1"), length.out = nrow(lb))
  lb$WGRADE <- rep(c("G0", "G1", "G2"), length.out = nrow(lb))
  lb <- lb[!(lb$PARAM == "HGB" & seq_len(nrow(lb)) %% 4 == 0), ]
  expect_identical(as.integer(table(lb$PARAM)), c(20L, 15L))
  d <- normalize_ard(cards::bind_ard(
    cards::ard_tabulate(lb, by = c(PARAM, BGRADE), variables = WGRADE),
    cards::ard_tabulate(lb, by = PARAM, variables = BGRADE),
    cards::ard_total_n(adsl)), drop_contexts = "attributes")
  p <- table_plan(d, cols = "BGRADE", rows = c(PARAM = "PARAM"),
                  label = c(label = ".label")) |>
    plan_cells("{n}", notes = FALSE) |> plan_paginate_group(keep = FALSE) |>
    plan_col_header(values = list(n = "page", N = "table"), rtf_col_header(
      list(col_cell(1, ""), col_cell(c(2, 3), "Page {n} of {N}")), c("", "{col}")))
  pg <- plan_apply(p, "pages")
  expect_identical(names(pg), c("ALT", "HGB"))
  spanner <- vapply(pg, function(x) x$col_header[[1L]][[2L]]$label, "")
  expect_identical(unname(spanner), c("Page 20 of 20", "Page 15 of 20"))
})

test_that("FT-PL3031 [FR-PL387]: header_sep given to plan_col_header() wins over plan_columns(sep = ) and is what the names are split on", {
  p <- table_plan(.g3_ard2(), cols = c("TRT", "SEX"), rows = c(group = "variable")) |>
    plan_cells("{mean}", notes = FALSE)
  both <- p |> plan_columns(sep = "__") |> plan_col_header(header_sep = "--")
  expect_identical(plan_apply(both, "args")$rtf$header_sep, "--")
  # the header is split on the separator given: names joined with "--" are
  split <- p |> plan_columns(sep = "--") |> plan_col_header(header_sep = "--")
  expect_identical(plan_apply(split, "args")$rtf$header_sep, "--")
  pg <- plan_apply(split, "pages")
  expect_length(pg[[1L]]$col_header, 2L)
  expect_identical(vapply(pg[[1L]]$col_header[[1L]], function(z) z$label, ""),
                   c("", "Placebo", "Drug"))
  # a separator that is not the one the names carry splits nothing
  other <- p |> plan_columns(sep = "--") |> plan_col_header(header_sep = "__")
  expect_identical(plan_apply(other, "args")$rtf$header_sep, "__")
  expect_null(plan_apply(other, "pages")[[1L]]$col_header)
  # the columns' sep alone is what the header is split on
  alone <- p |> plan_columns(sep = "--")
  expect_length(plan_apply(alone, "pages")[[1L]]$col_header, 2L)
})

# A plain 12-row table of fictional values (a parameter, two arms).
.g3_df12 <- function() {
  data.frame(Parameter = sprintf("Param %02d", 1:12),
             Placebo = sprintf("%.1f", seq(10, 21, by = 1) + 0.5),
             Active = sprintf("%.1f", seq(30, 41, by = 1) + 0.25),
             stringsAsFactors = FALSE)
}
.g3_pages <- function(p) suppressMessages(plan_apply(p, "pages"))
.g3_direct <- function(d, ...) {
  out <- suppressMessages(as_rtftables(d, read_meta = FALSE, ...))
  if (inherits(out, "rtftable")) list(out) else out
}

test_that("FT-PL3032 [FR-PL409]: plan_cell_style() without where styles whole columns: .values, a column left out is every column", {
  d <- .g3_df12()
  p <- table_plan(d) |>
    plan_columns(row_title = 1L)
  # cols left out: every column
  all <- .g3_pages(p |> plan_cell_style(bold = TRUE))[[1L]]
  expect_length(all$col_spec, 3L)
  expect_true(all(vapply(all$col_spec, function(s) isTRUE(s$bold), NA)))
  # cols named: those columns only, with colour, underline and indent
  two <- .g3_pages(p |> plan_cell_style(cols = c("Placebo", "Active"), color = "#CC0000",
                                        underline = TRUE, indent_twips = 200))[[1L]]
  expect_false(isTRUE(two$col_spec[[1L]]$underline))
  for (i in 2:3) {
    expect_true(isTRUE(two$col_spec[[i]]$underline))
    expect_identical(two$col_spec[[i]]$color, "#CC0000")
    expect_identical(two$col_spec[[i]]$indent_twips, 200L)
  }
  expect_null(two$col_spec[[1L]]$color)
})

test_that("FT-PL3033 [FR-PL409]: .values styles every value column of an ARD plan and not the stub columns", {
  p <- table_plan(.g3_ard2(), cols = c("TRT", "SEX"), rows = c(group = "variable")) |>
    plan_cells("{mean}", notes = FALSE)
  pg <- .g3_pages(p |> plan_cell_style(cols = ".values", color = "#CC0000",
                                       underline = TRUE, indent_twips = 200))[[1L]]
  expect_length(pg$col_spec, 6L)
  col <- vapply(pg$col_spec, function(s) s$color %||% "", "")
  expect_identical(col, c("", "", rep("#CC0000", 4L)))
  expect_identical(vapply(pg$col_spec, function(s) isTRUE(s$underline), NA),
                   c(FALSE, FALSE, rep(TRUE, 4L)))
  expect_identical(vapply(pg$col_spec, function(s) s$indent_twips %||% 0L, 1L),
                   c(0L, 0L, rep(200L, 4L)))
})

test_that("FT-PL3034 [FR-PL429]: plan_style(border_<zone> = ) and border = give one table in either order and either call layout", {
  d <- .g3_df12()
  dbl <- rtf_border(bottom = "double")
  one <- .g3_pages(table_plan(d) |> plan_style(border = "tfl", border_header = dbl))
  rev <- .g3_pages(table_plan(d) |> plan_style(border_header = dbl) |>
                     plan_style(border = "tfl"))
  fwd <- .g3_pages(table_plan(d) |> plan_style(border = "tfl") |>
                     plan_style(border_header = dbl))
  expect_equal(rev, one)
  expect_equal(fwd, one)
  direct <- .g3_direct(d, border = "tfl") |>
    style_zone(header = dbl)
  expect_equal(one, direct)
  # the zone is merged onto the preset's: its bottom rule is the zone's, its top is kept
  expect_identical(one[[1L]]$border$header$bottom$style, "double")
  expect_false(is.null(one[[1L]]$border$header$top))
})

test_that("FT-PL3035 [FR-PL429]: a whole-table rtf_border() with a zone is rtftable(border = ) then style_zone()", {
  d <- .g3_df12()
  whole <- rtf_border(all = TRUE, inside_h = TRUE)
  dbl <- rtf_border(bottom = "double")
  got <- .g3_pages(table_plan(d) |> plan_style(border = whole, border_header = dbl))
  flip <- .g3_pages(table_plan(d) |> plan_style(border_header = dbl) |>
                      plan_style(border = whole))
  want <- .g3_direct(d, border = whole) |>
    style_zone(header = dbl)
  expect_equal(got, want)
  expect_equal(flip, want)
  # the whole-table rule was expanded to zones; the header's bottom is the zone's
  expect_identical(got[[1L]]$border$header$bottom$style, "double")
})

test_that("FT-PL3036 [FR-PL440]: plan_paginate_rows() twice keeps the fields the later call leaves out", {
  p <- table_plan(.g3_df12()) |>
    plan_paginate_rows(max_rows = 30, split = "group_safe") |>
    plan_paginate_rows(max_rows = 40)
  rtf <- plan_apply(p, "args")$rtf
  expect_identical(rtf$max_rows, 40)
  expect_identical(rtf$split, "group_safe")
  # and the other way: a later split with the earlier row budget kept
  q <- table_plan(.g3_df12()) |> plan_paginate_rows(max_rows = 30) |>
    plan_paginate_rows(split = "group_force")
  expect_identical(plan_apply(q, "args")$rtf$max_rows, 30)
  expect_identical(plan_apply(q, "args")$rtf$split, "group_force")
})

test_that("FT-PL3037 [FR-PL487]: plan_titles(pages = ) and plan_footnotes(pages = ) give each page its own block, in page order", {
  p <- table_plan(.g3_df12()) |> plan_paginate_rows(max_rows = 4)
  n <- length(.g3_pages(p))
  expect_identical(n, 3L)
  pg <- .g3_pages(p |> plan_footnotes(pages = list("F1", "F2", "F3")) |>
                    plan_titles(pages = list("T1", "T2", "T3")))
  expect_identical(vapply(pg, function(x) attr(x, "rtf_footnotes"), ""),
                   setNames(c("F1", "F2", "F3"), names(pg)))
  expect_identical(vapply(pg, function(x) attr(x, "rtf_titles"), ""),
                   setNames(c("T1", "T2", "T3"), names(pg)))
  expect_identical(attr(pg[[2L]], "rtf_footnotes"), "F2")
  expect_identical(attr(pg[[2L]], "rtf_titles"), "T2")
})

test_that("FT-PL3038 [FR-PL490]: a count of blocks other than the pages' stops at plan_apply(), naming plan_paginate_rows()", {
  p <- table_plan(.g3_df12()) |> plan_paginate_rows(max_rows = 6)
  expect_length(.g3_pages(p), 2L)
  expect_error(plan_apply(p |> plan_footnotes(pages = list("a"))),
               "plan_footnotes(pages = ) has 1 block for 2 pages", fixed = TRUE)
  expect_error(plan_apply(p |> plan_titles(pages = list("a"))),
               "plan_titles(pages = ) has 1 block for 2 pages", fixed = TRUE)
  expect_error(plan_apply(p |> plan_footnotes(pages = list("a"))),
               "plan_paginate_rows()", fixed = TRUE)
  expect_error(plan_apply(p |> plan_titles(pages = list("a", "b", "c"))),
               "has 3 blocks for 2 pages")
})

test_that("FT-PL3039 [FR-PL493]: plan_after(f, g) runs on the finished pages, g gets f's result", {
  seen <- list()
  f <- function(pages) {
    seen$f <<- pages
    lapply(pages, function(x) {
      attr(x, "g3_marker") <- "set by f"
      x
    })
  }
  g <- function(pages) {
    seen$g <<- vapply(pages, function(x) attr(x, "g3_marker") %||% "none", "")
    pages
  }
  p <- table_plan(.g3_df12()) |> plan_paginate_rows(max_rows = 6) |>
    plan_col_header(rtf_col_header(c("Parameter", "Placebo", "Active"))) |>
    plan_columns(decimal = c("Placebo", "Active")) |>
    plan_cell_style(cols = "Parameter", bold = TRUE, where = ~ Parameter == "Param 01") |>
    plan_after(f, g)
  pg <- .g3_pages(p)
  expect_identical(unname(seen$g), c("set by f", "set by f"))   # g saw f's result
  expect_identical(attr(pg[[1L]], "g3_marker"), "set by f")
  # what f was given is the finished page: header, decimal alignment, cell styles
  first <- seen$f[[1L]]
  expect_identical(unname(unlist(first$col_header)), c("Parameter", "Placebo", "Active"))
  expect_false(is.null(first$decimal_split))
  expect_true(isTRUE(first$cell_styles[[1L]]$bold[[1L]]))
})

# A records table (one value a subject and time) and a long summary of it.
.g3_rec <- function() {
  data.frame(ID = rep(c("A", "B"), each = 3), T = rep(c("t1", "t2", "t3"), 2),
             V = c(1.234, 2.5, NA, 3, 4.25, 5), stringsAsFactors = FALSE)
}
.g3_long <- function(stats = "mean") {
  do.call(rbind, lapply(stats, function(s) data.frame(
    T = c("t1", "t2", "t3"), variable = "V", stat_name = s, stat = c(1.5, 2.5, 3.5),
    .label = NA_character_, stringsAsFactors = FALSE)))
}
.g3_parent <- function() {
  table_plan(.g3_rec(), cols = "T", rows = "ID", value = "V") |>
    plan_cells("{value:.1f}", na = "BLQ")
}
.g3_child <- function(stats, block, ...) {
  table_plan(.g3_long(stats), cols = "T", rows = c(block = block)) |>
    plan_cells(V = c(Mean = "{mean:.1f}", SD = "{sd:.2f}"), ...)
}

test_that("FT-PL3040 [FR-PL903]: unnamed parts are part2, part3; a name twice stops; a statistic a part lacks prints that part's empty text", {
  one <- .g3_child(c("mean", "sd"), ~ "Summary")
  two <- .g3_child("mean", ~ "Other")
  t3 <- as.data.frame(plan_apply(.g3_parent() |> plan_append(one) |> plan_append(two),
                                 "table"))
  expect_identical(unique(t3$.part), c("part1", "part2", "part3"))   # in the order declared
  expect_identical(.g3_plain(t3$label),
                   c("A", "B", "Summary", "Mean", "SD", "Other", "Mean", "SD"))
  # the SD the second part's ARD has not: that part's empty text, not the parent's "BLQ"
  expect_identical(unlist(t3[8L, c("t1", "t2", "t3")], use.names = FALSE), c("", "", ""))
  expect_identical(unlist(t3[5L, c("t1", "t2", "t3")], use.names = FALSE),
                   c("1.50", "2.50", "3.50"))
  own <- as.data.frame(plan_apply(.g3_parent() |> plan_append(one) |>
                                    plan_append(.g3_child("mean", ~ "Other", na = "NC")),
                                  "table"))
  expect_identical(unlist(own[8L, c("t1", "t2", "t3")], use.names = FALSE),
                   c("NC", "NC", "NC"))
  # a name given twice stops
  expect_error(.g3_parent() |> plan_append(one, name = "s") |> plan_append(two, name = "s"),
               "a part of that name is already there")
})

test_that("FT-PL3041 [FR-PL906]: a part carrying plan_footnotes(), plan_titles(), plan_after() or plan_paginate_group() stops, naming the verb", {
  one <- .g3_child("mean", ~ "Summary")
  bad <- function(child, what) {
    expect_error(.g3_parent() |> plan_append(child), what, fixed = TRUE)
  }
  bad(one |> plan_footnotes("F"), "plan_footnotes()")
  bad(one |> plan_titles("T"), "plan_titles()")
  bad(one |> plan_after(function(x) x), "plan_after()")
  bad(one |> plan_paginate_group(col = "T"), "plan_paginate_group()")
  # the message says where the verb goes
  expect_error(.g3_parent() |> plan_append(one |> plan_footnotes("F")),
               "settings are the first plan's")
  # something that is not a table_plan stops
  expect_error(.g3_parent() |> plan_append(.g3_long()), "is a table_plan")
})

test_that("FT-PL3042 [FR-PL907]: plan_columns(decimal = ) applies to the stacked table's value columns", {
  p <- .g3_parent() |> plan_append(.g3_child("mean", ~ "Summary"))
  bare <- plan_apply(p, "pages")[[1L]]
  expect_null(bare$decimal_split)
  pg <- plan_apply(p |> plan_columns(decimal = ".values"), "pages")[[1L]]
  expect_identical(names(pg$data), c("label", "t1", "t2", "t3"))
  expect_identical(pg$decimal_split$cols, 2:4)
})

test_that("FT-PL3043 [FR-PL944]: plan_paginate_rows(max_rows = ) on a listing plan pages as split = \"group_safe\"; a split with no row budget stops", {
  d <- data.frame(USUBJID = sprintf("S-%03d", 1:12), ARM = rep(c("A", "B"), 6),
                  stringsAsFactors = FALSE)
  cols <- function() list(listing_col("USUBJID", width = 12), listing_col("ARM", width = 10))
  lp <- table_plan(d) |> plan_listing(cols()[[1L]], cols()[[2L]]) |>
    plan_paginate_rows(max_rows = 4)
  got <- .g3_pages(lp)
  want <- .g3_direct(d, listing = listing_spec(cols()), split = "group_safe", max_rows = 4)
  expect_length(got, 6L)
  expect_equal(got, want)
  # a split that takes no row budget, given one, stops
  for (sp in c("none", "rows", "by_value")) {
    q <- table_plan(d) |> plan_listing(cols()[[1L]]) |>
      plan_paginate_rows(max_rows = 4, split = sp)
    expect_error(plan_apply(q, "pages"), "has no effect with split", info = sp)
  }
})

test_that("FT-PL3044 [FR-PL981]: with no last-row zone a body border stays on each page's last row", {
  side <- rtf_border(bottom = "single")
  p <- table_plan(.g3_df12()) |> plan_paginate_rows(max_rows = 5) |>
    plan_cell_style(where = ~ TRUE, border = side)
  pg <- .g3_pages(p)
  expect_length(pg, 3L)
  for (page in pg) {
    n <- length(page$cell_styles)
    expect_identical(page$cell_styles[[n]]$border[[1L]]$bottom$style, "single")
    expect_identical(page$cell_styles[[1L]]$border[[1L]]$bottom$style, "single")
  }
  # a last-row zone wins on the last row only
  dbl <- rtf_border(bottom = "double")
  z <- .g3_pages(p |> plan_style(border_last_row = dbl))
  for (page in z) {
    n <- length(page$cell_styles)
    expect_identical(page$cell_styles[[n]]$border[[1L]]$bottom$style, "double")
    expect_identical(page$cell_styles[[1L]]$border[[1L]]$bottom$style, "single")
  }
})

.g3_blocks_doc <- function(pages, ...) {
  rtf_document() |>
    rtf_section(page = 1, header = NULL, footer = NULL) |>
    rtf_tables(pages) |>
    rtf_config(style = rtf_style(title_format = "table", footnote_format = "table", ...))
}
.g3_block_lines <- function(doc, pat) {
  grep(pat, strsplit(.g3_render(doc), "\n", fixed = TRUE)[[1L]], value = TRUE)
}
.g3_block_pages <- function(tb = NULL, fb = NULL) {
  table_plan(.g3_df12()[1:2, ]) |>
    plan_titles("Table 14.1.1", "Demographics", border = tb) |>
    plan_footnotes("Note: Example Pharma data.", "Source: ADSL.", border = fb) |>
    plan_apply()
}

test_that("FT-PL3045 [FR-PL985]: plan_titles(border = ) draws top, a rule between the rows, and left and right on every row", {
  b <- rtf_border(top = TRUE, inside_h = TRUE, left = TRUE, right = TRUE)
  pages <- .g3_block_pages(tb = b)
  expect_s3_class(attr(pages[[1L]], "rtf_titles_border"), "rtf_border")
  r <- .g3_block_lines(.g3_blocks_doc(pages), "Table 14\\.1\\.1|Demographics")
  expect_length(r, 2L)
  has <- function(x, w) grepl(w, x, fixed = TRUE)
  expect_true(has(r[1L], "\\clbrdrt"))                 # top above the first row
  expect_false(has(r[2L], "\\clbrdrt") && !has(r[1L], "\\clbrdrb"))
  expect_true(has(r[1L], "\\clbrdrb") || has(r[2L], "\\clbrdrt"))   # between the rows
  expect_true(all(has(r, "\\clbrdrl")))                # left on every row
  expect_true(all(has(r, "\\clbrdrr")))                # right on every row
  # no border given: none of it
  plain <- .g3_block_lines(.g3_blocks_doc(.g3_block_pages()), "Table 14\\.1\\.1|Demographics")
  expect_false(any(has(plain, "\\clbrdrl")))
})

test_that("FT-PL3046 [FR-PL986]: plan_footnotes(border = rtf_border(top = TRUE)) is the separator rule above the block only", {
  pages <- .g3_block_pages(fb = rtf_border(top = TRUE))
  expect_s3_class(attr(pages[[1L]], "rtf_footnotes_border"), "rtf_border")
  r <- .g3_block_lines(.g3_blocks_doc(pages), "Note: Example|Source: ADSL")
  expect_length(r, 2L)
  expect_true(grepl("\\clbrdrt", r[1L], fixed = TRUE))
  expect_false(any(grepl("\\clbrdrb", r, fixed = TRUE)))
  expect_false(grepl("\\clbrdrt", r[2L], fixed = TRUE))
})

test_that("FT-PL3047 [FR-PL987]: a content without a footnote border takes the document's; its own wins", {
  fdoc <- rtf_border(bottom = "double")
  # rtf_style(footnotes = list(border = )) reaches a plan that has none
  r <- .g3_block_lines(.g3_blocks_doc(.g3_block_pages(), footnotes = list(border = fdoc)),
                       "Note: Example|Source: ADSL")
  expect_false(any(grepl("\\clbrdrt", r, fixed = TRUE)))
  expect_true(grepl("\\clbrdrb\\brdrdb", r[2L], fixed = TRUE))
  # rtf_footnotes(border = ) the same
  doc <- .g3_blocks_doc(.g3_block_pages()) |>
    rtf_footnotes(list(c("Note: Example Pharma data.", "Source: ADSL.")), border = fdoc)
  r2 <- .g3_block_lines(doc, "Note: Example|Source: ADSL")
  expect_true(any(grepl("\\clbrdrb\\brdrdb", r2, fixed = TRUE)))
  # the plan's own border wins over the document's
  own <- .g3_block_lines(.g3_blocks_doc(.g3_block_pages(fb = rtf_border(top = "double")),
                                        footnotes = list(border = fdoc)),
                         "Note: Example|Source: ADSL")
  expect_true(grepl("\\clbrdrt\\brdrdb", own[1L], fixed = TRUE))
  expect_false(any(grepl("\\clbrdrb", own, fixed = TRUE)))
})

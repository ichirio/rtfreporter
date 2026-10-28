# Validation tests: interactions (FR-IX257 .. FR-IX266) -- precedence between
# verbs and arguments, an argument that changes what another one sees, and
# combinations that once had a bug.  Hand-built frames (no cards); helpers
# prefixed vi_.

vi_long <- function() {
  data.frame(
    TRT       = rep(c("A", "B"), each = 6),
    variable  = rep(rep(c("ALT", "AST"), each = 3), 2),
    stat_name = rep(c("n", "mean", "sd"), 4),
    stat      = c(20, 31.245, 4.1, 20, 28.7, 3.92,
                  18, 33.108, 5.3, 18, 30.2, 4.44),
    stringsAsFactors = FALSE)
}

vi_base <- function() {
  table_plan(vi_long(), cols = "TRT", rows = c(group = "variable"),
             label = c(row = "stat_name")) |>
    plan_cells(c("Mean (SD)" = "{mean} ({sd})"), notes = FALSE)
}

vi_counts <- function() {
  data.frame(
    TRT       = rep(c("A", "B"), each = 4),
    PARAM     = rep(c("ALT", "ALT", "AST", "AST"), 2),
    VISIT     = rep(c("W1", "W2"), 4),
    stat_name = "n",
    stat      = c(5, 12, 3, 4, 6, 2, 8, 1),
    stringsAsFactors = FALSE)
}

vi_wide <- function() {
  data.frame(
    grp = c("A", "A", "B", "B", "C", "C"),
    label = c("n", "Mean", "n", "Mean", "n", "Mean"),
    X = c("1", "2.5", "3", "4.5", "5", "6.5"),
    Y = c("7", "8.5", "9", "10.5", "11", "12.5"),
    stringsAsFactors = FALSE)
}

vi_tab <- function(p) as.data.frame(suppressMessages(plan_apply(p, "table")))
vi_first <- function(x) if (inherits(x, "rtftable")) x else x[[1L]]
vi_pages <- function(p) suppressMessages(plan_apply(p, "pages"))
vi_render <- function(doc) {
  d <- tempfile("vi-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- file.path(d, "out.rtf")
  suppressMessages(generate_rtfreport(doc, f, overwrite = TRUE))
  readLines(f, warn = FALSE)
}
vi_titles <- function(ct) {
  unname(vapply(ct$title, function(r) if (is.list(r)) r$text else as.character(r), ""))
}


# ---- precedence ------------------------------------------------------------

test_that("FT-IX010 [FR-IX257]: plan_digits() plan-wide and per variable give the same table in either order", {
  wide_first <- vi_tab(vi_base() |> plan_digits(1) |> plan_digits(AST = 3))
  var_first <- vi_tab(vi_base() |> plan_digits(AST = 3) |> plan_digits(1))
  expect_identical(wide_first, var_first)
  # the variable's key formats its rows, the plan-wide default the rest
  expect_identical(wide_first$A, c("31.2 (4.1)", "28.700 (3.920)"))
  expect_identical(wide_first$B, c("33.1 (5.3)", "30.200 (4.440)"))
  # and nothing but digits moved: the rows and columns are the plan's
  expect_identical(names(wide_first), c("group", "row", "A", "B"))
})

test_that("FT-IX012 [FR-IX258]: plan_levels() and plan_sort(\"levels(<key>)\") order the rows the same whichever is called first", {
  p <- table_plan(vi_counts(), cols = "TRT", rows = c(param = "PARAM"),
                  label = c(visit = "VISIT")) |>
    plan_cells("{n:.0f}", notes = FALSE)
  levels_first <- vi_tab(p |> plan_levels(PARAM = "AST") |> plan_sort("levels(param)"))
  sort_first <- vi_tab(p |> plan_sort("levels(param)") |> plan_levels(PARAM = "AST"))
  expect_identical(levels_first, sort_first)
  expect_identical(as.character(levels_first$param), c("AST", "AST", "ALT", "ALT"))
  # the data's order without the levels: nothing else moved the rows
  expect_identical(as.character(vi_tab(p)$param), c("ALT", "ALT", "AST", "AST"))
})

test_that("FT-IX013 [FR-IX259]: plan_paginate_rows() written twice: the later budget cuts the pages", {
  d <- vi_wide()
  two_then_four <- vi_pages(table_plan(d) |>
                              plan_paginate_rows(max_rows = 2, split = "group_safe") |>
                              plan_paginate_rows(max_rows = 4))
  four_then_two <- vi_pages(table_plan(d) |>
                              plan_paginate_rows(max_rows = 4, split = "group_safe") |>
                              plan_paginate_rows(max_rows = 2))
  expect_equal(two_then_four, as_rtftables(d, max_rows = 4, split = "group_safe", read_meta = FALSE))
  expect_equal(four_then_two, as_rtftables(d, max_rows = 2, split = "group_safe", read_meta = FALSE))
  expect_length(two_then_four, 2L)
  expect_length(four_then_two, 3L)
})

test_that("FT-IX014 [FR-IX260]: rtf_tables(titles = , footnotes = ) win over a plan's plan_titles() / plan_footnotes(); left out, the plan's blocks are the page's", {
  p <- table_plan(vi_wide()) |> plan_titles("Plan title") |>
    plan_footnotes("Plan footnote") |> plan_style()
  given <- rtf_document() |> rtf_tables(p, titles = "Call title", footnotes = "Call footnote")
  left <- rtf_document() |> rtf_tables(p)
  expect_identical(vi_titles(given$contents[[1L]]), "Call title")
  expect_identical(vi_titles(left$contents[[1L]]), "Plan title")
  out_given <- paste(vi_render(given), collapse = "\n")
  out_left <- paste(vi_render(left), collapse = "\n")
  expect_match(out_given, "Call title", fixed = TRUE)
  expect_match(out_given, "Call footnote", fixed = TRUE)
  expect_false(grepl("Plan title", out_given, fixed = TRUE))
  expect_false(grepl("Plan footnote", out_given, fixed = TRUE))
  expect_match(out_left, "Plan title", fixed = TRUE)
  expect_match(out_left, "Plan footnote", fixed = TRUE)
})

test_that("FT-IX015 [FR-IX261]: the page name auto_title adds is the last row of the title block, after plan_titles()' rows", {
  p <- table_plan(vi_wide()) |>
    plan_paginate_group(col = "grp", heading = "Group {value}", heading_at = "title") |>
    plan_titles("Table 1", "By group")
  doc <- suppressMessages(rtf_document() |> rtf_tables(p))
  expect_length(doc$contents, 3L)
  expect_identical(vi_titles(doc$contents[[1L]]), c("Table 1", "By group", "Group A"))
  expect_identical(vi_titles(doc$contents[[3L]]), c("Table 1", "By group", "Group C"))
  # the same block when rtf_tables() says it
  pages <- vi_pages(table_plan(vi_wide()) |>
                      plan_paginate_group(col = "grp", heading = "Group {value}") |>
                      plan_titles("Table 1", "By group"))
  doc2 <- rtf_document() |> rtf_tables(pages, auto_title = TRUE)
  expect_identical(vi_titles(doc2$contents[[1L]]), c("Table 1", "By group", "Group A"))
  # auto_title off: the plan's rows alone
  doc3 <- rtf_document() |> rtf_tables(pages, auto_title = FALSE)
  expect_identical(vi_titles(doc3$contents[[1L]]), c("Table 1", "By group"))
})


# ---- an argument that changes what another one sees ---------------------------

test_that("FT-IX016 [FR-IX263]: plan_stub(before = TRUE) hands plan_paginate_rows() the folded rows, grouped under the stub's headings: the same pages as folding inside", {
  d <- vi_wide()
  before <- vi_pages(table_plan(d) |>
                       plan_stub(vars = c("grp", "label"), name = "rl", before = TRUE) |>
                       plan_paginate_rows(max_rows = 4, split = "group_safe"))
  inside <- vi_pages(table_plan(d) |>
                       plan_stub(vars = c("grp", "label"), name = "rl") |>
                       plan_paginate_rows(max_rows = 4, split = "group_safe"))
  # (before = TRUE also leaves the stub's source map on the data, for plan_cell_style())
  bare <- function(pg) lapply(pg, function(z) { d <- z$data; attr(d, "rtf_stub_src") <- NULL; d })
  expect_identical(bare(before), bare(inside))
  # a heading (3 rows with its leaves) never crosses the budget: one group a page,
  # each page opening with its heading row
  expect_length(before, 3L)
  expect_identical(vapply(before, function(z) z$data$rl[1L], ""), c("A", "B", "C"))
  expect_true(all(vapply(before, function(z) nrow(z$data), 0L) == 3L))
})

test_that("FT-IX017 [FR-IX253]: with plan_stub(before = TRUE) a bare plan_cell_style() formula sees the folded keys and styles the rows it chooses", {
  d <- vi_wide()
  pg <- vi_pages(table_plan(d) |>
                   plan_stub(vars = c("grp", "label"), name = "rl", before = TRUE) |>
                   plan_cell_style(bold = ~ !is.na(grp) & grp == "B") |>
                   plan_style(border = "tfl")) |> vi_first()
  cs <- pg$cell_styles
  on <- vapply(cs, function(z) !is.null(z) && isTRUE(z$bold[[1L]]), NA)
  # the folded key is read through the stub's source map: B's rows only
  # (the leaves indented; the indent's own characters are stub_cols()' business)
  expect_identical(sub("^[^[:alnum:]]+", "", as.vector(pg$data$rl)),
                   c("A", "n", "Mean", "B", "n", "Mean", "C", "n", "Mean"))
  expect_identical(which(on), 5:6)
  expect_true(all(vapply(cs[on], function(z) all(z$bold), NA)))
})

test_that("FT-IX018 [FR-IX253]: with plan_stub(before = TRUE) a heading row has no source row, so a condition on a key reads NA there", {
  d <- vi_wide()
  pg <- vi_pages(table_plan(d) |>
                   plan_stub(vars = c("grp", "label"), name = "rl", before = TRUE) |>
                   plan_cell_style(italic = ~ is.na(grp)) |>
                   plan_style(border = "tfl")) |> vi_first()
  on <- vapply(pg$cell_styles, function(z) !is.null(z) && isTRUE(z$italic[[1L]]), NA)
  expect_identical(pg$data$rl[on], c("A", "B", "C"))
})


# ---- combinations that once had a bug ----------------------------------------

test_that("FT-IX019 [FR-IX264]: auto_section = TRUE: a value's pages cut by the row budget keep their carried titles, whether the page opens the section or continues it (#728)", {
  g <- data.frame(G = c(rep("Cohort A", 5), "Cohort B"),
                  S = c("n", "Mean", "SD", "Min", "Max", "n"),
                  V = as.character(1:6), stringsAsFactors = FALSE)
  pages <- as_rtftables(g, split = "by_value", group_col = "G", max_rows = 3, border = "tfl")
  expect_identical(names(pages), c("Cohort A...1", "Cohort A...2", "Cohort B"))
  for (i in seq_along(pages)) {
    attr(pages[[i]], "rtf_titles") <- paste("Title of page", i)
    attr(pages[[i]], "rtf_footnotes") <- paste("Footnote of page", i)
  }
  doc <- rtf_document() |> rtf_tables(pages, auto_section = TRUE)
  # two sections (the cohorts), three pages
  lay <- rtf_layout(doc)
  expect_length(lay$pages, 3L)
  expect_identical(vapply(lay$pages, function(p) p$section_start, NA), c(TRUE, FALSE, TRUE))
  expect_identical(vapply(lay$pages, function(p) p$section, 0L), c(1L, 1L, 2L))
  out <- vi_render(doc)
  for (i in 1:3) {
    expect_identical(sum(grepl(paste("Title of page", i), out, fixed = TRUE)), 1L)
    expect_identical(sum(grepl(paste("Footnote of page", i), out, fixed = TRUE)), 1L)
  }
  # the section's heading once (its header), then the cells and the continuation row
  expect_identical(sum(grepl("Cohort A", out, fixed = TRUE)), 1L + 5L + 1L)
  expect_identical(sum(grepl("Cohort B", out, fixed = TRUE)), 1L + 1L)
})

test_that("FT-IX020 [FR-IX265]: a later content's font and colour are added after the document's own, so the first content keeps its indices (#730)", {
  df <- function(n) data.frame(Item = sprintf("Row %02d", seq_len(n)), N = seq_len(n))
  bs <- intToUtf8(92)
  first <- rtf_document() |>
    rtf_tables(list(rtftable(df(2), font = "Times New Roman",
                             col_spec = list(list(col = 2, color = "#0000FF")))), titles = "T1")
  both <- first |>
    rtf_tables(list(rtftable(df(2), font = "Arial",
                             col_spec = list(list(col = 2, color = "#FF0000")))),
               titles = "T2", page_break = FALSE)
  l1 <- vi_render(first)
  l2 <- vi_render(both)
  tbl <- function(l, what) grep(what, l, value = TRUE, fixed = TRUE)[1L]
  # the first document's tables are a prefix of the second's
  f1 <- tbl(l1, "fonttbl"); f2 <- tbl(l2, "fonttbl")
  c1 <- tbl(l1, "colortbl"); c2 <- tbl(l2, "colortbl")
  expect_identical(substr(f2, 1L, nchar(f1) - 1L), substr(f1, 1L, nchar(f1) - 1L))
  expect_identical(substr(c2, 1L, nchar(c1) - 1L), substr(c1, 1L, nchar(c1) - 1L))
  expect_true(grepl("Arial", f2, fixed = TRUE) && !grepl("Arial", f1, fixed = TRUE))
  expect_true(grepl(paste0(bs, "red255", bs, "green0", bs, "blue0;"), c2, fixed = TRUE))
  # the first content's rows are written the same in both documents
  rows1 <- grep("Row 01", l1, value = TRUE, fixed = TRUE)
  rows2 <- grep("Row 01", l2, value = TRUE, fixed = TRUE)
  expect_identical(rows2[1L], rows1[1L])
})

test_that("FT-IX021 [FR-IX266]: a footer, like a header, starts in the document's font -- \\f0, or the slots' default command -- then its own (#732)", {
  bs <- intToUtf8(92)
  df <- data.frame(a = c("x", "y"), b = c("1", "2"), stringsAsFactors = FALSE)
  doc <- function() {
    rtf_document() |>
      rtf_section(page = 1, secinfo = list(
        header = rtf_header(rows = list(c(l = "MYHDR"))),
        footer = rtf_footer(rows = list(c(l = "MYFTR"))))) |>
      rtf_tables(rtftable(df, border = "tfl"))
  }
  band <- function(l, what) grep(what, l, value = TRUE, fixed = TRUE)[1L]
  l <- vi_render(doc())
  expect_true(startsWith(band(l, "MYFTR"), paste0("{", bs, "footer ", bs, "f0", bs, "fs")))
  expect_true(startsWith(band(l, "MYHDR"), paste0("{", bs, "header ", bs, "f0", bs, "fs")))
  # a band with its own font: the document's first, then its own on the cells
  own <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(
      footer = rtf_footer(rows = list(c(l = "MYFTR")), font = "Arial"))) |>
    rtf_tables(rtftable(df, border = "tfl"))
  lo <- vi_render(own)
  ftr <- band(lo, "MYFTR")
  expect_true(startsWith(ftr, paste0("{", bs, "footer ", bs, "f0", bs, "fs")))
  expect_match(ftr, paste0(bs, "f1 MYFTR"), fixed = TRUE)
  # a document whose default font has slots: the slots' default command
  old <- options(rtfreporter.font = c(latin = "Arial", east_asian = "MS Mincho"))
  on.exit(options(old), add = TRUE)
  ls <- vi_render(doc())
  for (what in c("MYFTR", "MYHDR")) {
    b <- band(ls, what)
    expect_true(startsWith(b, paste0("{", bs, if (what == "MYFTR") "footer " else "header ",
                                     bs, "rtlch", bs, "af0", bs, "ltrch", bs, "hich", bs, "af0",
                                     bs, "dbch", bs, "af1", bs, "loch", bs, "f0", bs, "fs")))
  }
})

# Thickening of the functional tests, batch 12: the part-only FRs left in the
# areas IX, PL, AS, DC and AR.  Each block asserts what its FR states, the
# figures worked out in a comment.  FT ids from 6201 up.

.t12_render <- function(doc, ...) {
  d <- tempfile("t12-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- file.path(d, "out.rtf")
  generate_rtfreport(doc, f, overwrite = TRUE, ...)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}
.t12_lines <- function(txt, pat) {
  grep(pat, strsplit(txt, "\n", fixed = TRUE)[[1L]], value = TRUE)
}
.t12_has <- function(x, w) grepl(w, x, fixed = TRUE)

# A normalized ARD: AGE (N, mean, sd) in arms A and B.
.t12_ard <- function() {
  normalize_ard(data.frame(
    group1 = "TRT", group1_level = rep(c("A", "B"), each = 3L),
    variable = "AGE", variable_level = NA_character_, context = "continuous",
    stat_name = rep(c("N", "mean", "sd"), 2L),
    stat_label = rep(c("N", "Mean", "SD"), 2L),
    stat = c(10, 12.34, 1.2, 12, 15.67, 2.2), stringsAsFactors = FALSE))
}

# Six one-line rows: with max_rows = 3 two pages.
.t12_df6 <- function() {
  data.frame(Parameter = sprintf("Param %02d", 1:6),
             Placebo = sprintf("%.1f", 1:6), stringsAsFactors = FALSE)
}

# A document that prints titles and footnotes as tables, so each row is one
# line of RTF with its cell borders.
.t12_blocks_doc <- function(pages, ...) {
  rtf_document() |>
    rtf_section(page = 1, header = NULL, footer = NULL) |>
    rtf_tables(pages) |>
    rtf_config(style = rtf_style(title_format = "table", footnote_format = "table", ...))
}

# ------------------------------------------------------------------- PL

test_that("FT-PL6201 [FR-PL103]: print() of a table_plan names the source kind, the columns the roles may name, the layers in order, and table data.frame or RTF pages", {
  d <- .t12_ard()
  p0 <- table_plan(d, cols = "TRT", rows = c(group = "variable"))
  out0 <- utils::capture.output(print(p0))
  # source kind, no layers, the data.frame it gives
  expect_identical(out0[1L],
                   "<table_plan>  from a normalized frame, 0 layers  ->  table data.frame")
  expect_true(any(grepl("(no layers", out0, fixed = TRUE)))
  # two layers, declared cells then digits: listed in that order
  p1 <- p0 |> plan_cells(continuous = c(N = "{N}", Mean = "{mean}")) |>
    plan_digits(AGE = 1)
  out1 <- utils::capture.output(print(p1))
  expect_identical(out1[1L],
                   "<table_plan>  from a normalized frame, 2 layers  ->  table data.frame")
  i_cells <- grep("^\\s+1\\. cells ", out1)
  i_digits <- grep("^\\s+2\\. digits ", out1)
  expect_length(i_cells, 1L)
  expect_length(i_digits, 1L)
  expect_lt(i_cells, i_digits)
  # the columns the roles may name: the frame's own, TRT among them
  i_in <- grep("what cols / rows / label may name", out1)
  expect_length(i_in, 1L)
  expect_true(any(grepl("TRT, variable, variable_level", out1[(i_in + 1L):(i_in + 2L)])))
  # a style verb makes it RTF pages; the order of the layers follows the calls
  p2 <- plan_style(p1, border = "tfl")
  out2 <- utils::capture.output(print(p2))
  expect_identical(out2[1L],
                   "<table_plan>  from a normalized frame, 3 layers  ->  RTF pages")
  expect_lt(grep("^\\s+2\\. digits ", out2), grep("^\\s+3\\. style ", out2))
  # the source kinds: a finished table, and a long frame of statistics
  expect_identical(utils::capture.output(print(table_plan(data.frame(a = 1:2, b = c("x", "y")))))[1L],
                   "<table_plan>  from a table, 0 layers  ->  table data.frame")
  long <- data.frame(variable = "v", stat_name = "n", stat = 1, g = "a")
  expect_identical(utils::capture.output(print(table_plan(long, cols = "g")))[1L],
                   "<table_plan>  from a long frame of statistics, 0 layers  ->  table data.frame")
})

test_that("FT-PL6202 [FR-PL206]: an error inside a stage names the verbs that declared it with their own arguments; a role nobody declared is blamed on no verb", {
  two <- normalize_ard(data.frame(
    group1 = "TRT", group1_level = rep(c("A", "B"), each = 4L), variable = "SEX",
    variable_level = rep(c("F", "F", "M", "M"), 2L), context = "categorical",
    stat_name = rep(c("n", "p"), 4L), stat_label = rep(c("n", "%"), 4L),
    stat = c(4, 0.4, 6, 0.6, 5, 0.5, 5, 0.5), stringsAsFactors = FALSE))
  # a stage that fails on what two verbs declared: both are named, with their own call
  p <- table_plan(two, cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(categorical = c(a = "{n:.0f}"), notes = FALSE) |>
    plan_labels(SEX = "Sex")
  e <- tryCatch(plan_apply(p, "table"), error = function(e) e)
  expect_s3_class(e, "error")
  msg <- conditionMessage(e)
  expect_true(.t12_has(msg, paste0("\n  declared by:\n    ",
                                   "plan_cells(categorical = c(a = \"{n:.0f}\"), notes = FALSE)")))
  expect_true(.t12_has(msg, "\n    plan_labels(SEX = \"Sex\")"))
  expect_false(.t12_has(msg, "table_plan("))
  # a role nobody declared (no cols): blamed on no verb, whatever else is declared
  q <- table_plan(two, rows = c(group = "variable")) |> plan_cells(notes = FALSE)
  e2 <- tryCatch(suppressMessages(plan_apply(q)), error = function(e) e)
  expect_s3_class(e2, "error")
  msg2 <- conditionMessage(e2)
  expect_true(.t12_has(msg2, "`cols` is required"))
  expect_true(.t12_has(msg2, "table_plan(cols = )"))
  expect_false(.t12_has(msg2, "declared by"))
  expect_false(.t12_has(msg2, "plan_cells"))
})

test_that("FT-PL6203 [FR-PL985]: plan_titles(border = ) is the title block's border on every page: top above the first row, bottom below the last, inside_h between, left and right on every row, its colour in the colour table; NULL leaves the RTF as before", {
  b <- rtf_border(top = TRUE, bottom = "double", inside_h = TRUE, left = TRUE,
                  right = rtf_border_line(color = "#C00000"))
  mk <- function(border) {
    table_plan(.t12_df6()) |>
      plan_titles("Table 14.1.1", "Demographics", "Third", border = border) |>
      plan_paginate_rows(max_rows = 3) |> plan_apply()
  }
  pages <- mk(b)
  # six rows, three a page: two pages, each carrying the border
  expect_length(pages, 2L)
  for (pg in pages) expect_s3_class(attr(pg, "rtf_titles_border"), "rtf_border")
  txt <- .t12_render(.t12_blocks_doc(pages))
  r <- .t12_lines(txt, "Table 14\\.1\\.1|Demographics|Third")
  expect_length(r, 6L)       # three title rows on each of the two pages
  for (page in list(1:3, 4:6)) {
    one <- r[page]
    expect_true(.t12_has(one[1L], "\\clbrdrt\\brdrs"))        # top above the first row
    expect_false(any(.t12_has(one[2:3], "\\clbrdrt")))
    expect_true(.t12_has(one[1L], "\\clbrdrb\\brdrs"))        # inside_h between rows 1 and 2
    expect_true(.t12_has(one[2L], "\\clbrdrb\\brdrs"))        # and rows 2 and 3
    expect_true(.t12_has(one[3L], "\\clbrdrb\\brdrdb"))       # bottom below the last row
    expect_true(all(.t12_has(one, "\\clbrdrl\\brdrs")))       # left on every row
    expect_true(all(.t12_has(one, "\\clbrdrr\\brdrs")))       # right on every row
  }
  # the right side's colour: a brdrcf, and the colour in the table (192, 0, 0)
  expect_true(all(.t12_has(r, "\\brdrcf")))
  expect_true(.t12_has(txt, "\\red192\\green0\\blue0"))
  # NULL (the default): no attribute, and the RTF is the plan without the argument
  plain <- table_plan(.t12_df6()) |>
    plan_titles("Table 14.1.1", "Demographics", "Third") |>
    plan_paginate_rows(max_rows = 3) |> plan_apply()
  expect_null(attr(plain[[1L]], "rtf_titles_border"))
  expect_identical(.t12_render(.t12_blocks_doc(mk(NULL))), .t12_render(.t12_blocks_doc(plain)))
  expect_false(any(.t12_has(.t12_lines(.t12_render(.t12_blocks_doc(plain)), "Demographics"),
                            "\\clbrdr")))
})

test_that("FT-PL6204 [FR-PL986]: plan_footnotes(border = ) is the footnote block's border on every page; rtf_border(top = TRUE) is the rule above the block; NULL leaves the RTF as before", {
  mk <- function(border) {
    table_plan(.t12_df6()) |>
      plan_footnotes("Note: One.", "Source: ADSL.", border = border) |>
      plan_paginate_rows(max_rows = 3) |> plan_apply()
  }
  # the separator rule only: top above the block, nothing below, on both pages
  pages <- mk(rtf_border(top = TRUE))
  expect_length(pages, 2L)
  for (pg in pages) expect_s3_class(attr(pg, "rtf_footnotes_border"), "rtf_border")
  r <- .t12_lines(.t12_render(.t12_blocks_doc(pages)), "Note: One|Source: ADSL")
  expect_length(r, 4L)
  for (page in list(1:2, 3:4)) {
    expect_true(.t12_has(r[page[1L]], "\\clbrdrt\\brdrs"))
    expect_false(.t12_has(r[page[2L]], "\\clbrdrt"))
    expect_false(any(.t12_has(r[page], "\\clbrdrb")))
  }
  # a block border as rtf_footnotes() reads it: top above, bottom below, left and right on each row
  full <- mk(rtf_border(top = TRUE, bottom = "double", left = TRUE, right = TRUE))
  r2 <- .t12_lines(.t12_render(.t12_blocks_doc(full)), "Note: One|Source: ADSL")
  expect_true(.t12_has(r2[1L], "\\clbrdrt\\brdrs"))
  expect_true(.t12_has(r2[2L], "\\clbrdrb\\brdrdb"))
  expect_false(.t12_has(r2[1L], "\\clbrdrb"))
  expect_true(all(.t12_has(r2, "\\clbrdrl\\brdrs")))
  expect_true(all(.t12_has(r2, "\\clbrdrr\\brdrs")))
  # NULL: no attribute and the same RTF as a plan that never said it
  plain <- table_plan(.t12_df6()) |>
    plan_footnotes("Note: One.", "Source: ADSL.") |>
    plan_paginate_rows(max_rows = 3) |> plan_apply()
  expect_null(attr(plain[[1L]], "rtf_footnotes_border"))
  expect_identical(.t12_render(.t12_blocks_doc(mk(NULL))), .t12_render(.t12_blocks_doc(plain)))
})

test_that("FT-PL6205 [FR-PL987]: a content's own title or footnote border wins over rtf_style() and rtf_titles() / rtf_footnotes(); one without takes the document's; a later call without border keeps it", {
  df <- .t12_df6()[1:2, ]
  tdoc <- rtf_border(bottom = TRUE)
  fdoc <- rtf_border(top = TRUE)
  town <- rtf_border(bottom = "double")
  fown <- rtf_border(top = "double")
  pat_t <- "Table 14\\.1\\.1|Demographics"
  pat_f <- "Note: One|Source: ADSL"
  with_own <- table_plan(df) |>
    plan_titles("Table 14.1.1", "Demographics", border = town) |>
    plan_footnotes("Note: One.", "Source: ADSL.", border = fown) |> plan_apply()
  # rtf_style(titles = , footnotes = ): the content's own wins on both blocks
  txt <- .t12_render(.t12_blocks_doc(with_own, titles = list(border = tdoc),
                                     footnotes = list(border = fdoc)))
  ti <- .t12_lines(txt, pat_t)
  fn <- .t12_lines(txt, pat_f)
  expect_true(.t12_has(ti[2L], "\\clbrdrb\\brdrdb"))
  expect_false(any(.t12_has(ti, "\\brdrs")))
  expect_true(.t12_has(fn[1L], "\\clbrdrt\\brdrdb"))
  expect_false(any(.t12_has(fn, "\\brdrs")))
  # rtf_titles(border = ) / rtf_footnotes(border = ) on the document: the content's own still wins
  doc2 <- .t12_blocks_doc(with_own) |>
    rtf_titles(list(c("Table 14.1.1", "Demographics")), border = tdoc) |>
    rtf_footnotes(list(c("Note: One.", "Source: ADSL.")), border = fdoc)
  txt2 <- .t12_render(doc2)
  expect_true(.t12_has(.t12_lines(txt2, "Demographics")[1L], "\\clbrdrb\\brdrdb"))
  expect_true(.t12_has(.t12_lines(txt2, "Note: One")[1L], "\\clbrdrt\\brdrdb"))
  # a content without one takes the document's
  without <- table_plan(df) |>
    plan_titles("Table 14.1.1", "Demographics") |>
    plan_footnotes("Note: One.", "Source: ADSL.") |> plan_apply()
  txt3 <- .t12_render(.t12_blocks_doc(without, titles = list(border = tdoc),
                                      footnotes = list(border = fdoc)))
  ti3 <- .t12_lines(txt3, pat_t)
  fn3 <- .t12_lines(txt3, pat_f)
  expect_true(.t12_has(ti3[2L], "\\clbrdrb\\brdrs"))
  expect_false(any(.t12_has(ti3, "\\clbrdrt")))
  expect_true(.t12_has(fn3[1L], "\\clbrdrt\\brdrs"))
  expect_false(any(.t12_has(fn3, "\\clbrdrb")))
  # a later plan_titles() / plan_footnotes() call without border keeps the earlier call's
  later <- table_plan(df) |>
    plan_titles("Table 14.1.1", "Demographics", border = town) |>
    plan_titles("Table 14.1.1", "Demographics") |>
    plan_footnotes("Note: One.", "Source: ADSL.", border = fown) |>
    plan_footnotes("Note: One.", "Source: ADSL.") |> plan_apply()
  expect_s3_class(attr(later[[1L]], "rtf_titles_border"), "rtf_border")
  expect_s3_class(attr(later[[1L]], "rtf_footnotes_border"), "rtf_border")
  txt4 <- .t12_render(.t12_blocks_doc(later))
  expect_true(.t12_has(.t12_lines(txt4, "Demographics")[1L], "\\clbrdrb\\brdrdb"))
  expect_true(.t12_has(.t12_lines(txt4, "Note: One")[1L], "\\clbrdrt\\brdrdb"))
})

# A four-row table, three columns, as the plan tests use.
.t12_tbl <- function() {
  data.frame(Parameter = c("Subjects", "Age, mean (SD)", "Female, n (%)", "Male, n (%)"),
             Placebo = c("86", "75.2 (8.59)", "53 (61.6)", "33 (38.4)"),
             Active = c("84", "75.7 (8.29)", "50 (59.5)", "34 (40.5)"),
             stringsAsFactors = FALSE)
}
# A table keeps a `col_header = NULL` entry or leaves it out depending on how it
# was built; it is the same table, so the entry is dropped before comparing.
.t12_strip <- function(pages) {
  lapply(pages, function(pg) {
    if ("col_header" %in% names(pg) && is.null(pg$col_header)) pg["col_header"] <- NULL
    pg
  })
}
.t12_pages <- function(p) .t12_strip(suppressMessages(plan_apply(p, "pages")))
.t12_direct <- function(d, ...) {
  out <- suppressMessages(as_rtftables(d, read_meta = FALSE, ...))
  .t12_strip(if (inherits(out, "rtftable")) list(out) else out)
}
.t12_pages_rtf <- function(pages) .t12_render(rtf_document() |> rtf_tables(pages))
.t12_rows <- function(txt) unlist(strsplit(txt, "\\trowd", fixed = TRUE))[-1L]
.t12_n <- function(x, pat) lengths(regmatches(x, gregexpr(pat, x, fixed = TRUE)))

# A warning that says itself once a session: let it fire again in this block.
.t12_reset_once <- function(key) {
  st <- rtfreporter:::.deprecation_state
  old <- if (exists(key, envir = st, inherits = FALSE)) get(key, envir = st)
  if (exists(key, envir = st, inherits = FALSE)) rm(list = key, envir = st)
  old
}
.t12_restore_once <- function(key, old) {
  st <- rtfreporter:::.deprecation_state
  if (is.null(old)) {
    if (exists(key, envir = st, inherits = FALSE)) rm(list = key, envir = st)
  } else assign(key, old, envir = st)
}

test_that("FT-PL6206 [FR-PL409]: plan_cell_style(cols = , <attributes>) without where is style_cols() on those columns, every column when cols is left out, .values the value columns; a border there is the header cells', said once, pointing to where = ~ TRUE", {
  d <- .t12_tbl()
  base <- .t12_direct(d)[[1L]]
  attrs <- list(list(bold = TRUE), list(italic = TRUE), list(underline = TRUE),
                list(color = "#CC0000"), list(indent_twips = 200L),
                list(bold = TRUE, italic = TRUE, underline = TRUE, color = "#CC0000",
                     indent_twips = 200L))
  for (a in attrs) {
    named <- .t12_pages(do.call(plan_cell_style, c(list(table_plan(d), cols = c("Placebo", "Active")), a)))
    want <- do.call(style_cols, c(list(base, cols = c("Placebo", "Active")), a))
    expect_equal(named[[1L]], want, info = paste(names(a), collapse = "+"))
    # the stub column is not touched; the two named are
    for (k in names(a)) {
      expect_false(identical(named[[1L]]$col_spec[[1L]][[k]], a[[k]]), info = k)
      expect_identical(named[[1L]]$col_spec[[2L]][[k]], a[[k]], info = k)
      expect_identical(named[[1L]]$col_spec[[3L]][[k]], a[[k]], info = k)
    }
    # cols left out: every column
    every <- .t12_pages(do.call(plan_cell_style, c(list(table_plan(d)), a)))
    expect_equal(every[[1L]], do.call(style_cols, c(list(base), a)), info = paste(names(a), collapse = "+"))
    for (k in names(a)) {
      for (i in 1:3) expect_identical(every[[1L]]$col_spec[[i]][[k]], a[[k]], info = k)
    }
    # no body cell is styled one by one
    expect_null(named[[1L]]$cell_styles)
  }
  # .values: the value columns of an ARD plan (the stub columns group and label are not)
  p <- table_plan(.t12_ard(), cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(continuous = c(N = "{N}", Mean = "{mean}"), notes = FALSE)
  v <- .t12_pages(plan_cell_style(p, cols = ".values", bold = TRUE, color = "#CC0000"))[[1L]]
  expect_identical(names(v$data), c("group", "label", "A", "B"))
  expect_identical(vapply(v$col_spec, function(s) isTRUE(s$bold), NA), c(FALSE, FALSE, TRUE, TRUE))
  expect_identical(vapply(v$col_spec, function(s) s$color %||% "", ""),
                   c("", "", "#CC0000", "#CC0000"))
  # a border on whole columns is style_cols()'s: the header cells; it warns once a session
  key <- "warn:plan_cell_style_cols_border"
  old <- .t12_reset_once(key)
  on.exit(.t12_restore_once(key, old), add = TRUE)
  rule <- rtf_border(bottom = "double")
  expect_warning(
    q <- plan_cell_style(table_plan(d), cols = "Active", border = rule),
    "draws the border on the columns' header cells only", fixed = TRUE)
  expect_no_warning(plan_cell_style(table_plan(d), cols = "Active", border = rule))
  .t12_reset_once(key)
  w <- tryCatch(plan_cell_style(table_plan(d), cols = "Active", border = rule),
                warning = function(w) conditionMessage(w))
  expect_true(.t12_has(w, "write where = ~ TRUE"))
  expect_true(.t12_has(w, "plan_cell_style(cols = , where = ~ TRUE, border = )"))
  got <- .t12_pages(q)
  expect_equal(got[[1L]], style_cols(base, cols = "Active", border = rule))
  expect_null(got[[1L]]$cell_styles)
  # in the RTF the double rule is in the header row's third cell only
  rows <- .t12_rows(.t12_pages_rtf(got))
  expect_identical(.t12_n(rows[1L], "\\clbrdrb\\brdrdb"), 1L)
  expect_identical(sum(.t12_n(rows[-1L], "\\brdrdb")), 0L)
  # where = ~ TRUE is the body cells' border
  b <- .t12_pages(plan_cell_style(table_plan(d), cols = "Active", where = ~ TRUE, border = rule))
  expect_equal(b[[1L]], style_body(base, cols = "Active", border = rule))
})

test_that("FT-PL6207 [FR-PL428]: plan_style() called twice merges field by field, later wins; not called, no style or width argument reaches as_rtftables()", {
  d <- .t12_tbl()
  dbl <- rtf_border(bottom = "double")
  # twice: font later wins, border and align_count_pct from the calls kept
  two <- plan_apply(table_plan(d) |> plan_style(border = "tfl", font = "Arial") |>
                      plan_style(font = "Courier New", align_count_pct = TRUE), "args")$rtf
  expect_identical(two[c("border", "font", "align_count_pct")],
                   list(border = "tfl", font = "Courier New", align_count_pct = TRUE))
  # three calls: each adds a field, none is lost, the last value of a field stands
  three <- plan_apply(table_plan(d) |>
                        plan_style(font = "Arial", row_height_twips = 300) |>
                        plan_style(font = "Courier New") |>
                        plan_style(table_width_twips = 6000), "args")$rtf
  expect_identical(three[c("font", "row_height_twips", "table_width_twips")],
                   list(font = "Courier New", row_height_twips = 300, table_width_twips = 6000))
  # the pages are the direct call with the merged fields
  expect_equal(.t12_pages(table_plan(d) |> plan_style(font = "Arial", row_height_twips = 300) |>
                            plan_style(font = "Courier New")),
               .t12_direct(d, font = "Courier New", row_height_twips = 300))
  # the rules are said one way: border and a zone across calls is the preset with the zone merged
  expect_equal(.t12_pages(table_plan(d) |> plan_style(border = "tfl") |>
                            plan_style(border_header = dbl)),
               .t12_direct(d, border = "tfl") |> style_zone(header = dbl))
  # a later border replaces an earlier one
  expect_equal(.t12_pages(table_plan(d) |> plan_style(border = "tfl") |> plan_style(border = "none")),
               .t12_direct(d, border = "none"))
  # not called: none of the style or width arguments is passed on, and the pages are the default
  a <- plan_apply(table_plan(d) |> plan_paginate_rows(max_rows = 10), "args")
  expect_false(any(c("na", "cell_format", "header_sep", "col_header_align", "column_widths_twips",
                     "table_width_twips", "table_width_pct", "style", "border", "font",
                     "align_count_pct", "row_height_twips") %in% names(a$rtf)))
  expect_equal(.t12_pages(table_plan(d) |> plan_style()), .t12_direct(d))
})

test_that("FT-PL6208 [FR-PL429]: plan_style(border = , border_<zone> = ) in one call or across calls in either order is the preset with each zone merged side by side; a whole-table rtf_border() is expanded first; border_group and the look fields keep the preset's rules", {
  d <- .t12_tbl()
  dbl <- rtf_border(bottom = "double")
  zones <- c("header", "spanning", "body", "first_row", "last_row")
  # every zone, preset "tfl": one call, zone first, border first, as border = "tfl" |> style_zone()
  for (z in zones) {
    zone_args <- stats::setNames(list(dbl), paste0("border_", z))
    zone_call <- stats::setNames(list(dbl), z)
    want <- suppressWarnings(do.call(style_zone, c(list(.t12_direct(d, border = "tfl")), zone_call)))
    one <- suppressWarnings(.t12_pages(do.call(plan_style, c(list(table_plan(d), border = "tfl"), zone_args))))
    fwd <- suppressWarnings(.t12_pages(do.call(plan_style, c(list(plan_style(table_plan(d), border = "tfl")), zone_args))))
    rev <- suppressWarnings(.t12_pages(plan_style(do.call(plan_style, c(list(table_plan(d)), zone_args)),
                                                  border = "tfl")))
    expect_equal(one, want, info = z)
    expect_equal(fwd, want, info = z)
    expect_equal(rev, want, info = z)
  }
  # the merge is side by side: the header's bottom is the zone's, its top is the preset's
  hz <- .t12_pages(plan_style(table_plan(d), border = "tfl", border_header = dbl))[[1L]]$border$header
  expect_identical(hz$bottom$style, "double")
  expect_false(is.null(hz$top))
  # a whole-table rtf_border() is expanded by rtftable(), the zone merged onto that
  whole <- rtf_border(all = TRUE, inside_h = TRUE)
  expect_equal(.t12_pages(plan_style(table_plan(d), border = whole, border_header = dbl)),
               .t12_direct(d, border = whole) |> style_zone(header = dbl))
  expect_equal(.t12_pages(plan_style(plan_style(table_plan(d), border_header = dbl), border = whole)),
               .t12_direct(d, border = whole) |> style_zone(header = dbl))
  # the look fields given with them keep the preset's rules (the zone merged on)
  look <- .t12_pages(plan_style(table_plan(d), border = "tfl", border_header = dbl, header_bold = TRUE))
  expect_equal(look, .t12_direct(d, border = "tfl") |> style_zone(header = dbl) |>
                 style_cols(header_bold = TRUE))
  expect_identical(.t12_n(.t12_rows(.t12_pages_rtf(look))[1L], "\\clbrdrt\\brdrs"), 3L)
  # border_group stays with the style: the rule under each row group, the preset's rules kept
  df <- data.frame(Item = c("Age", " Mean", " SD", "Sex", " Female", " Male"),
                   Value = as.character(1:6), stringsAsFactors = FALSE)
  rule <- rtf_border(bottom = TRUE)
  tfl <- rtfreporter:::.normalize_table_border("tfl")
  got <- .t12_pages(table_plan(df) |> plan_row_group(mode = "indent") |>
                      plan_style(border = "tfl", border_header = dbl, border_group = rule))
  want <- .t12_direct(df, group_by = "indent", border = tfl,
                      style = rtf_table_style(border_group = rule)) |> style_zone(header = dbl)
  expect_equal(got, want)
  expect_identical(.t12_pages_rtf(got), .t12_pages_rtf(want))
  expect_identical(.t12_n(.t12_rows(.t12_pages_rtf(got))[1L], "\\clbrdrt\\brdrs"), 2L)
})

test_that("FT-PL6209 [FR-PL973]: plan_cell_style(background = ) fills whole columns as style_cols(), the rows a condition chooses only, and the header's fill with header = TRUE", {
  d <- .t12_tbl()
  base <- .t12_direct(d)[[1L]]
  fill <- "#EEEEEE"
  # whole columns: as style_cols(), the other column unfilled
  c1 <- .t12_pages(plan_cell_style(table_plan(d), cols = "Placebo", background = fill))[[1L]]
  expect_equal(c1, style_cols(base, cols = "Placebo", background = fill))
  expect_identical(c1$col_spec[[2L]]$background, fill)
  expect_null(c1$col_spec[[3L]]$background)
  # a condition: the rows it chooses only (row 1 of four), NA for the other columns, no style on the other rows
  w <- .t12_pages(plan_cell_style(table_plan(d), where = ~ Parameter == "Subjects",
                                  cols = "Placebo", background = fill))[[1L]]
  expect_identical(w$cell_styles[[1L]]$background, c(NA, fill, NA))   # row 1: column Placebo
  expect_null(w$cell_styles[[2L]])
  expect_null(w$col_spec[[2L]]$background)
  # with header = TRUE: the column header's fill (style_cols(header_background = ))
  h <- .t12_pages(plan_cell_style(table_plan(d), header = TRUE, cols = "Placebo", background = fill))[[1L]]
  expect_equal(h, style_cols(base, cols = "Placebo", header_background = fill))
  expect_identical(h$col_spec[[2L]]$header_background, fill)
  expect_null(h$col_spec[[2L]]$background)
  expect_null(h$col_spec[[3L]]$header_background)
})

test_that("FT-PL6210 [FR-PL978]: plan_cell_style(header = TRUE, row = ) styles that header row only as style_header(row = ), every header row when left out; row without header, with color or background, or outside the header stops", {
  d <- .t12_tbl()
  hdr <- rtf_col_header(list(col_cell(1, ""), col_cell(c(2, 3), "Treatment")),
                        c("Parameter", "Placebo", "Active"))
  base <- table_plan(d) |> plan_col_header(hdr)
  dir <- function() .t12_direct(d, col_header = hdr)
  # row 1, the spanning row
  got <- .t12_pages(base |> plan_cell_style(header = TRUE, row = 1, italic = TRUE))
  expect_equal(got, dir() |> style_header(row = 1, italic = TRUE))
  expect_true(isTRUE(got[[1L]]$col_header[[1L]][[2L]]$italic))
  expect_false(any(vapply(got[[1L]]$col_spec, function(s) isTRUE(s$header_italic), NA)))
  # row 2, the label row, one column's border: the spanning row is left alone
  rule <- rtf_border(bottom = "double")
  got2 <- .t12_pages(base |> plan_cell_style(header = TRUE, row = 2, cols = "Active", border = rule))
  expect_equal(got2, dir() |> style_header(row = 2, cols = 3, border = rule))
  expect_null(got2[[1L]]$col_header[[1L]][[2L]]$border)
  # left out: every header row
  expect_equal(.t12_pages(base |> plan_cell_style(header = TRUE, bold = TRUE)),
               dir() |> style_header(bold = TRUE))
  # the stops
  expect_error(plan_cell_style(base, row = 1, bold = TRUE),
               "plan_cell_style(row = ) picks a column-header row, so it is given with header = TRUE.",
               fixed = TRUE)
  expect_error(plan_cell_style(base, header = TRUE, row = 1, color = "#CC0000"),
               "color and background\n  are the columns' header look", fixed = TRUE)
  expect_error(plan_cell_style(base, header = TRUE, row = 2, background = "#EEEEEE", bold = TRUE),
               "Make them a call without `row`.", fixed = TRUE)
  expect_error(.t12_pages(plan_cell_style(base, header = TRUE, row = 3, bold = TRUE)),
               "`style_header(row = )` must be in 1..2", fixed = TRUE)
})

test_that("FT-PL6211 [FR-PL981]: on each page's last row the last_row zone wins side by side over a body border from plan_cell_style(where =); the sides it leaves NULL keep the cell's; no zone keeps the body border; no body border is written as before", {
  d <- .t12_tbl()
  dbl <- rtf_border(bottom = "double")
  side <- rtf_border(bottom = TRUE, left = TRUE)
  body <- function(p) plan_cell_style(p, where = ~ TRUE, border = side)
  # border_last_row: the last row's bottom is the zone's, its left kept from the cell's
  got <- .t12_pages(body(table_plan(d) |> plan_style(border_last_row = dbl)))[[1L]]$cell_styles
  last <- got[[4L]]$border[[1L]]
  expect_identical(last$bottom$style, "double")
  expect_false(is.null(last$left))
  expect_identical(got[[3L]]$border[[1L]]$bottom$style, "single")   # the other rows keep theirs
  # in the RTF: the double rule on the three cells of the last row only
  rows <- .t12_rows(.t12_pages_rtf(.t12_pages(body(table_plan(d) |> plan_style(border_last_row = dbl)))))
  expect_identical(.t12_n(rows[length(rows)], "\\clbrdrb\\brdrdb"), 3L)
  expect_identical(sum(.t12_n(rows[-length(rows)], "\\brdrdb")), 0L)
  # that zone of the border preset
  got2 <- .t12_pages(body(table_plan(d) |> plan_style(border = "tfl", border_last_row = dbl)))
  expect_identical(got2[[1L]]$cell_styles[[4L]]$border[[2L]]$bottom$style, "double")
  # no last-row zone: the body border stays on the last row
  got3 <- .t12_pages(body(table_plan(d) |> plan_style(border = "tfl")))
  expect_identical(got3[[1L]]$cell_styles[[4L]]$border[[1L]]$bottom$style, "single")
  # paged: every page's last row (two rows a page)
  pg <- .t12_pages(body(table_plan(d) |> plan_style(border_last_row = dbl) |>
                          plan_paginate_rows(max_rows = 2)))
  expect_length(pg, 2L)
  for (page in pg) {
    expect_identical(page$cell_styles[[2L]]$border[[1L]]$bottom$style, "double")
    expect_identical(page$cell_styles[[1L]]$border[[1L]]$bottom$style, "single")
  }
  # no body border: written as before, the zone and a bold row
  p <- table_plan(d) |> plan_style(border_last_row = dbl) |>
    plan_cell_style(where = ~ Parameter == "Subjects", bold = TRUE)
  want <- .t12_direct(d, style = rtf_table_style(border_last_row = dbl)) |>
    style_body(rows = ~ Parameter == "Subjects", bold = TRUE)
  expect_identical(.t12_pages_rtf(.t12_pages(p)), .t12_pages_rtf(want))
})

test_that("FT-PL6212 [FR-PL990]: plan_style(border = \"tfl\" or no border) with a look field keeps the preset's rules, header_row over and under; border_group adds to them; \"none\" stays without rules; zones without border stay the only rules; no look field is as before", {
  d <- .t12_tbl()
  dir <- function(...) .t12_direct(d, ...)
  tfl <- rtfreporter:::.normalize_table_border("tfl")
  # tfl with each look field: the pages are the preset with style_cols()
  looks <- list(list(header_bold = TRUE), list(header_italic = TRUE), list(header_align = "left"),
                list(align = "right"), list(bold = TRUE), list(italic = TRUE), list(underline = TRUE))
  for (lk in looks) {
    got <- .t12_pages(do.call(plan_style, c(list(table_plan(d), border = "tfl"), lk)))
    hdr <- .t12_rows(.t12_pages_rtf(got))[1L]
    # three header cells, a rule over and under each
    expect_identical(.t12_n(hdr, "\\clbrdrt\\brdrs"), 3L, info = names(lk))
    expect_identical(.t12_n(hdr, "\\clbrdrb\\brdrs"), 3L, info = names(lk))
    # no border said: the same
    nob <- .t12_pages(do.call(plan_style, c(list(table_plan(d)), lk)))
    expect_identical(.t12_pages_rtf(nob), .t12_pages_rtf(got), info = names(lk))
    # "none": no rules at all
    expect_false(.t12_has(.t12_pages_rtf(.t12_pages(
      do.call(plan_style, c(list(table_plan(d), border = "none"), lk)))), "clbrdr"), info = names(lk))
  }
  expect_equal(.t12_pages(plan_style(table_plan(d), border = "tfl", header_bold = TRUE)),
               dir(border = "tfl") |> style_cols(header_bold = TRUE))
  expect_equal(.t12_pages(plan_style(table_plan(d), border = "tfl", align = "right", italic = TRUE)),
               dir(border = "tfl") |> style_cols(align = "right", italic = TRUE, header_align = "right"))
  expect_equal(.t12_pages(plan_style(table_plan(d), border = "none", header_bold = TRUE)),
               dir(border = "none") |> style_cols(header_bold = TRUE))
  # zones without a border are the table's only rules
  z <- rtf_border(bottom = TRUE)
  expect_equal(.t12_pages(plan_style(table_plan(d), border_last_row = z, header_bold = TRUE)),
               dir(style = rtf_table_style(border_last_row = z, header_bold = TRUE)))
  # border_group: the preset's rules and a rule under each group; with "none", the group rules only
  df <- data.frame(Item = c("Age", " Mean", " SD", "Sex", " Female", " Male"),
                   Value = as.character(1:6), stringsAsFactors = FALSE)
  rule <- rtf_border(bottom = TRUE)
  grp <- function(...) table_plan(df) |> plan_row_group(mode = "indent") |> plan_style(border_group = rule, ...)
  got <- .t12_pages(grp())
  expect_equal(got, .t12_direct(df, group_by = "indent", border = tfl,
                                style = rtf_table_style(border_group = rule)))
  expect_identical(.t12_n(.t12_rows(.t12_pages_rtf(got))[1L], "\\clbrdrt\\brdrs"), 2L)
  expect_identical(.t12_pages_rtf(got), .t12_pages_rtf(.t12_pages(grp(border = "tfl"))))
  expect_false(.t12_has(.t12_rows(.t12_pages_rtf(.t12_pages(grp(border = "none"))))[1L], "clbrdr"))
  # no look field: written as before, the preset alone and with a font
  expect_equal(.t12_pages(plan_style(table_plan(d), border = "tfl")), dir(border = "tfl"))
  expect_equal(.t12_pages(plan_style(table_plan(d), border = "tfl", font = "Arial")),
               dir(border = "tfl", font = "Arial"))
})

# Records (A: 1.234, 2.5, NA; B: 3, 4.25, 5 at t1..t3) and a long frame of
# statistics (t1..t3) for the part rows.
.t12_rec <- function() {
  data.frame(ID = rep(c("A", "B"), each = 3), T = rep(c("t1", "t2", "t3"), 2),
             V = c(1.234, 2.5, NA, 3, 4.25, 5), stringsAsFactors = FALSE)
}
.t12_long <- function(stats = "mean", times = c("t1", "t2", "t3")) {
  do.call(rbind, lapply(stats, function(s) data.frame(
    T = times, variable = "V", stat_name = s, stat = seq_along(times) + 0.5,
    .label = NA_character_, stringsAsFactors = FALSE)))
}
.t12_parent <- function() {
  table_plan(.t12_rec(), cols = "T", rows = "ID", value = "V") |>
    plan_cells("{value:.1f}", na = "BLQ")
}
.t12_child <- function(stats, block, times = c("t1", "t2", "t3"), tmpl = NULL, ...) {
  table_plan(.t12_long(stats, times), cols = "T", rows = c(block = block)) |>
    plan_cells(V = tmpl %||% c(Mean = "{mean}", SD = "{sd}"), ...)
}
.t12_plain <- function(x) sub("^[^[:alnum:]]+", "", as.character(x))
.t12_cells <- function(t, i) unlist(t[i, c("t1", "t2", "t3")], use.names = FALSE)

test_that("FT-PL6213 [FR-PL903]: plan_append(part = ) puts a part's rows under the parent's in the order declared, each under its name; a missing column or statistic prints the part's own empty text, a heading row stays empty; digits inherited key by key, the part's own win; a name twice stops", {
  one <- .t12_child(c("mean", "sd"), ~ "Summary")
  two <- .t12_child("mean", ~ "Other")
  # the rows: the parent's, then each part's heading and statistics, in the order declared
  t3 <- as.data.frame(plan_apply(.t12_parent() |> plan_append(one) |> plan_append(two), "table"))
  expect_identical(names(t3), c("label", "t1", "t2", "t3", ".part"))
  expect_identical(.t12_plain(t3$label), c("A", "B", "Summary", "Mean", "SD", "Other", "Mean", "SD"))
  expect_identical(t3$.part, c("part1", "part1", "part2", "part2", "part2", "part3", "part3", "part3"))
  expect_identical(.t12_cells(t3, 4L), c("1.5", "2.5", "3.5"))        # Mean of part2, the parent's digits
  # the SD the part's ARD has not: the part's empty text, not the parent's "BLQ"
  expect_identical(.t12_cells(t3, 8L), c("", "", ""))
  # ... which is the part's own: na = "NC" fills the missing statistic but never a heading row
  own <- as.data.frame(plan_apply(.t12_parent() |> plan_append(one) |>
                                    plan_append(.t12_child("mean", ~ "Other", na = "NC")), "table"))
  expect_identical(.t12_cells(own, 8L), c("NC", "NC", "NC"))
  expect_identical(.t12_cells(own, 6L), c("", "", ""))               # the heading row "Other"
  expect_identical(.t12_cells(own, 3L), c("", "", ""))               # the heading row "Summary"
  # a column the part has not (t3): the part's empty text there, the parent's BLQ is the parent's
  fewer <- as.data.frame(plan_apply(.t12_parent() |>
                                      plan_append(.t12_child("mean", ~ "Summary", times = c("t1", "t2"),
                                                             tmpl = c(Mean = "{mean}"))), "table"))
  expect_identical(fewer$t3, c("BLQ", "5.0", "", ""))
  expect_identical(fewer$t1[4L], "1.5")
  fewer_na <- as.data.frame(plan_apply(.t12_parent() |>
                                         plan_append(.t12_child("mean", ~ "Summary", times = c("t1", "t2"),
                                                                tmpl = c(Mean = "{mean}"), na = "NC")), "table"))
  expect_identical(fewer_na$t3, c("BLQ", "5.0", "", "NC"))
  # digits: the parent's by key, and the part's own win
  d3 <- as.data.frame(plan_apply(.t12_parent() |> plan_digits(3) |> plan_append(one), "table"))
  expect_identical(d3$t1[4L], "1.500")
  d1 <- as.data.frame(plan_apply(.t12_parent() |> plan_digits(3) |> plan_append(one |> plan_digits(1)), "table"))
  expect_identical(d1$t1[4L], "1.5")
  # parts under their own names; the parts in the order declared
  named <- as.data.frame(plan_apply(.t12_parent() |> plan_append(one, name = "stats") |>
                                      plan_append(two, name = "again") |> plan_append(two), "table"))
  expect_identical(unique(named$.part), c("part1", "stats", "again", "part4"))
  # a name given twice stops (the parent is part1)
  expect_error(.t12_parent() |> plan_append(one, name = "s") |> plan_append(two, name = "s"),
               "a part of that name is already there")
  expect_error(.t12_parent() |> plan_append(one, name = "part1"), "already there")
})

test_that("FT-PL6214 [FR-PL907]: the whole table's settings read the stack: the header's {col}, the named widths and the decimal split apply to the stacked table's columns", {
  p <- .t12_parent() |> plan_append(.t12_child("mean", ~ "Summary", tmpl = c(Mean = "{mean}"))) |>
    plan_col_header(rtf_col_header(c("Subject", "{col}"))) |>
    plan_columns(widths = c(label = 2, .values = 1), decimal = ".values") |>
    plan_style(border = "tfl")
  pg <- plan_apply(p, "pages")[[1L]]
  # the stacked table's columns: the label, then the three time columns of the stack
  expect_identical(names(pg$data), c("label", "t1", "t2", "t3"))
  expect_identical(pg$col_rel_width, c(2, 1, 1, 1))          # label = 2, every value column 1
  expect_identical(pg$decimal_split$cols, 2:4)              # the value columns only
  # {col} is each stacked column's name: the header row reads Subject, t1, t2, t3
  expect_identical(pg$col_header[[1L]], c("Subject", "t1", "t2", "t3"))
  # without the settings: no widths of the stack, no split
  bare <- plan_apply(.t12_parent() |> plan_append(.t12_child("mean", ~ "Summary", tmpl = c(Mean = "{mean}"))),
                     "pages")[[1L]]
  expect_null(bare$decimal_split)
  expect_false(identical(bare$col_rel_width, c(2, 1, 1, 1)))
})

.t12_nd_wide <- function() {
  data.frame(grp = c("A", "A", "B", "B"), label = c("n", "Mean", "n", "Mean"),
             X = c("1", "2.5", "3", "4.5"), Y = c("7", "8.5", "9", "10.5"),
             ord = c(2, 2, 1, 1), stringsAsFactors = FALSE)
}
.t12_nd_ard <- function() {
  data.frame(ARM = rep(c("Placebo", "Active"), each = 3),
             variable = rep(c("AGE", "SEX", "SEX"), 2), .label = rep(c(NA, "F", "M"), 2),
             .kind = rep(c("continuous", "categorical", "categorical"), 2),
             stat_name = rep(c("mean", "n", "n"), 2), stat = c(61.2, 12, 8, 63.5, 9, 11),
             stringsAsFactors = FALSE)
}
# Arms A and B with an overall block (TRT missing): n and mean by parameter.
.t12_nd_total <- function() {
  d <- data.frame(TRT = rep(c("A", "B"), each = 2), PARAM = "ALT",
                  stat_name = rep(c("n", "mean"), 2), stat = c(20, 31.2, 18, 33.1),
                  stringsAsFactors = FALSE)
  o <- d[d$TRT == "A", ]
  o$TRT <- NA
  o$stat <- o$stat * 10
  table_plan(rbind(d, o), cols = "TRT", rows = c(param = "PARAM"),
             label = c(row = "stat_name")) |>
    plan_cells(c(n = "{n:.0f}", Mean = "{mean:.1f}"), notes = FALSE)
}
# An AE table with nothing in it but its overall row (0 in each arm).
.t12_nd_empty <- function() {
  d <- data.frame(TRT = c("A", "B"), soc = NA, .label = "Any TEAE", stat_name = "n",
                  stat = 0, .overall = TRUE, .kind = "categorical", stringsAsFactors = FALSE)
  table_plan(d, cols = "TRT", rows = c(soc = "soc")) |> plan_cells("{n:.0f}", notes = FALSE)
}
.t12_tab <- function(p) as.data.frame(suppressMessages(plan_apply(p, "table")))
.t12_first <- function(x) if (inherits(x, "rtftable")) x else x[[1L]]

test_that("FT-PL6215 [FR-PL984]: a plan verb records only the arguments its call gives: a later call of the same verb keeps an earlier call's value, the documented default applies when no call gives one, a single call gives the pages it gave before", {
  # plan_stub(before): TRUE survives a later call without it; FALSE is the default; an explicit FALSE resets
  p <- table_plan(.t12_nd_ard(), cols = "ARM", rows = c(group = "variable")) |>
    plan_cells(continuous = c(Mean = "{mean:.1f}"), categorical = "{n}") |>
    plan_labels(AGE = "Age (years)", SEX = "Sex")
  pg <- function(x) suppressMessages(plan_apply(x, "pages"))
  one <- p |> plan_stub(name = "C", before = TRUE) |> plan_style()
  two <- p |> plan_stub(before = TRUE) |> plan_stub(name = "C") |> plan_style()
  expect_identical(pg(two), pg(one))
  expect_identical(names(.t12_first(pg(two))$data)[1L], "C")
  l <- (p |> plan_stub(name = "C"))$layers
  expect_identical(l[[length(l)]]$fields, list(name = "C"))         # the layer says what the call said
  expect_identical(pg(p |> plan_stub(before = TRUE) |> plan_stub(name = "C", before = FALSE) |> plan_style()),
                   pg(p |> plan_stub(name = "C") |> plan_style()))  # the default is before = FALSE
  expect_identical(plan_layers(p |> plan_stub(name = "C") |> plan_style())$layers$stub,
                   list(name = "C", before = FALSE))
  # plan_total(label, position, direction)
  tp <- .t12_nd_total()
  expect_identical(names(.t12_tab(tp |> plan_total(label = "All") |> plan_total(position = "first"))),
                   c("param", "row", "All", "A", "B"))
  expect_identical(names(.t12_tab(tp |> plan_total(position = "first") |> plan_total(label = "All"))),
                   c("param", "row", "All", "A", "B"))
  expect_identical(.plan_resolve(tp |> plan_total(direction = "rows") |> plan_total(label = "All"),
                                 "total")$direction, "rows")
  expect_identical(names(.t12_tab(tp |> plan_total())), c("param", "row", "A", "B", "Total"))
  expect_identical(.t12_tab(tp |> plan_total()),
                   .t12_tab(tp |> plan_total(label = "Total", position = "last", direction = "cols")))
  expect_identical(plan_layers(tp |> plan_total())$layers$total, list(label = "Total", position = "last"))
  # plan_empty(text, when, position)
  ep <- .t12_nd_empty()
  expect_identical(.t12_tab(ep |> plan_empty(text = "a") |> plan_empty(position = "first"))$label[1L], "a")
  expect_identical(.t12_tab(ep |> plan_empty(position = "replace") |> plan_empty(text = "None."))$label,
                   "None.")
  expect_identical(.plan_resolve(ep |> plan_empty(when = "no_rows") |> plan_empty(text = "a"),
                                 "empty")$when, "no_rows")
  expect_identical(.t12_tab(ep |> plan_empty())$label, c("Any TEAE", "No subjects met the criteria."))
  expect_identical(.plan_resolve(ep |> plan_empty(), "empty"),
                   list(text = "No subjects met the criteria.", when = "auto",
                        position = "last", .no_text = FALSE))
  # plan_sort(keep): FALSE survives a later call, TRUE is the default
  d <- .t12_nd_wide()
  sp <- table_plan(d) |> plan_style()
  expect_identical(pg(sp |> plan_sort("grp", keep = FALSE) |> plan_sort("ord")),
                   pg(sp |> plan_sort("ord", keep = FALSE)))
  expect_false("ord" %in% names(.t12_first(pg(sp |> plan_sort("grp", keep = FALSE) |> plan_sort("ord")))$data))
  expect_true("ord" %in% names(.t12_first(pg(sp |> plan_sort("ord")))$data))
  expect_identical(plan_layers(sp |> plan_sort("ord"))$layers$sort, list(sort = "ord", .keep = TRUE))
  # plan_paginate_group(keep)
  one <- pg(sp |> plan_paginate_group(col = "grp", keep = FALSE, heading_at = "section"))
  two <- pg(sp |> plan_paginate_group(col = "grp", keep = FALSE) |> plan_paginate_group(heading_at = "section"))
  expect_identical(two, one)
  expect_false("grp" %in% names(.t12_first(two)$data))
  expect_true("grp" %in% names(.t12_first(pg(sp |> plan_paginate_group(col = "grp")))$data))
  expect_identical(plan_layers(sp |> plan_paginate_group(col = "grp"))$layers$group,
                   list(group_col = "grp", .keep = TRUE, .page = TRUE))
  # a single call gives the pages it gave before: the same as the direct call
  expect_identical(pg(sp |> plan_sort("ord")),
                   pg(sp |> plan_sort("ord", keep = TRUE)))
})

# ------------------------------------------------------------------- AS

test_that("FT-AS6201 [FR-AS219]: rtfreporter_ai_manual(ard = ) adds 05_ard.csv (the normalized ARD, every row up to 2,000, else each analysis' first 30) and 06_ard_shape.md (keys, which come together, each variable's statistics, a starting table_plan()); without it 00_START_HERE.md says how to add them", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  ard <- cards::bind_ard(
    cards::ard_continuous(adsl, by = ARM, variables = AGE,
                          statistic = ~ cards::continuous_summary_fns(c("mean", "sd"))),
    cards::ard_tabulate(adsl, by = ARM, variables = SEX, statistic = ~ c("n", "p")))
  flat <- normalize_ard(ard)
  d <- tempfile("as6201-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  out <- rtfreporter_ai_manual(d, ard = ard)
  expect_true(all(c("05_ard.csv", "06_ard_shape.md") %in% basename(out)))
  # every row of the normalized ARD, as it is flattened (a few hundred rows, under 2,000)
  csv <- utils::read.csv(file.path(d, "05_ard.csv"), stringsAsFactors = FALSE)
  expect_lt(nrow(flat), 2000L)
  expect_identical(nrow(csv), nrow(flat))
  expect_true(all(c("ARM", "variable", "stat_name", "stat") %in% names(csv)))
  expect_identical(csv$stat_name, as.character(flat$stat_name))
  expect_identical(sort(unique(csv$variable)), c("AGE", "SEX"))
  # the shape: the key and its values, the variables with their statistics, a starting plan
  shape <- readLines(file.path(d, "06_ard_shape.md"), warn = FALSE)
  expect_true(any(grepl("| `ARM` |", shape, fixed = TRUE)))
  expect_true(any(grepl("Which keys come together", shape, fixed = TRUE)))
  # AGE: mean and sd in 3 arms = 6 rows; SEX: n and p for 2 levels in 3 arms = 12 rows
  expect_true("| `AGE` | continuous | mean, sd | 6 |" %in% shape)
  expect_true("| `SEX` | tabulate | n, p | 12 |" %in% shape)
  expect_true("| ARM | 18 |" %in% shape)                                # the one key, on every row
  expect_true(any(grepl("table_plan(cols = \"ARM\"", shape, fixed = TRUE)))
  # without the ARD: no such files, and 00 says how to add them
  d0 <- tempfile("as6201b-")
  dir.create(d0)
  on.exit(unlink(d0, recursive = TRUE), add = TRUE)
  out0 <- rtfreporter_ai_manual(d0)
  expect_false(any(c("05_ard.csv", "06_ard_shape.md") %in% basename(out0)))
  expect_false(any(file.exists(file.path(d0, c("05_ard.csv", "06_ard_shape.md")))))
  start <- readLines(file.path(d0, "00_START_HERE.md"), warn = FALSE)
  expect_true(any(grepl("rtfreporter_ai_manual(dir, ard = ard)", start, fixed = TRUE)))
  expect_true(any(grepl("No ARD came with this kit", start, fixed = TRUE)))
  # above 2,000 rows: each analysis' first 30 rows (1001 groups x 2 statistics = 2002 rows a variable)
  df <- data.frame(g = sprintf("G%04d", 1:1001), AGE = seq(30, 60, length.out = 1001),
                   BMI = seq(20, 30, length.out = 1001), stringsAsFactors = FALSE)
  big <- cards::ard_continuous(df, by = g, variables = c(AGE, BMI),
                               statistic = ~ cards::continuous_summary_fns(c("mean", "sd")))
  d2 <- tempfile("as6201c-")
  dir.create(d2)
  on.exit(unlink(d2, recursive = TRUE), add = TRUE)
  rtfreporter_ai_manual(d2, ard = big)
  csv2 <- utils::read.csv(file.path(d2, "05_ard.csv"), stringsAsFactors = FALSE)
  expect_identical(nrow(csv2), 60L)                                    # 2 variables x first 30 rows
  expect_identical(as.integer(table(csv2$variable)[c("AGE", "BMI")]), c(30L, 30L))
  expect_identical(csv2$g[csv2$variable == "AGE"], rep(sprintf("G%04d", 1:15), each = 2L))
})

test_that("FT-AS6202 [FR-AS228]: the kit's text files are UTF-8 whatever the locale, the Japanese labels of the ARD kept, and nothing is sent anywhere", {
  skip_if_not_installed("cards")
  df <- data.frame(g = c("A", "B"), AGE = c(30, 40), stringsAsFactors = FALSE)
  ard <- cards::ard_continuous(df, by = g, variables = AGE,
                               statistic = ~ cards::continuous_summary_fns(c("mean")))
  ard$group1_level <- list("プラセボ", "実薬")     # the arm labels in Japanese
  d <- tempfile("as6202-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  old <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", old), add = TRUE)
  invisible(Sys.setlocale("LC_CTYPE", "C"))                           # a locale that is not UTF-8
  called <- character(0)
  trap <- function(what) function(...) {
    called <<- c(called, what)
    stop("network used: ", what)
  }
  local({
    local_mocked_bindings(url = trap("url"), socketConnection = trap("socketConnection"),
                          curlGetHeaders = trap("curlGetHeaders"), .package = "base")
    local_mocked_bindings(download.file = trap("utils::download.file"), .package = "utils")
    rtfreporter_ai_manual(d, ard = ard, shell = c("Table 1", "年齢  n (%)"))
  })
  expect_identical(called, character(0))                              # nothing is sent anywhere
  for (f in list.files(d, full.names = TRUE)) {
    bytes <- readBin(f, "raw", file.size(f))
    expect_true(validUTF8(rawToChar(bytes)), info = basename(f))
    # no byte-order mark, no stray locale encoding
    expect_false(identical(bytes[1:3], as.raw(c(0xef, 0xbb, 0xbf))), info = basename(f))
  }
  jp <- charToRaw("プラセボ")                           # the arm label as UTF-8 bytes
  has <- function(f) {
    b <- readBin(file.path(d, f), "raw", file.size(file.path(d, f)))
    any(vapply(seq_len(length(b) - length(jp) + 1L),
               function(i) identical(b[i:(i + length(jp) - 1L)], jp), NA))
  }
  expect_true(has("05_ard.csv"))
  expect_true(has("06_ard_shape.md"))
  expect_true(file.exists(file.path(d, "PASTE.md")))
})

# One heading and three entries on three levels, for the TOC paragraphs.
.t12_hang_toc <- function(f) {
  data.frame(file = f, heading = c("Efficacy", NA, NA), label = c("One", "Two", "Three"),
             level = c(1L, 2L, 3L))
}

test_that("FT-AS6203 [FR-AS260]: toc_style sets size and boldness of the title, heading, entry and the blank line, merged key by key over 14 pt bold / 11 pt bold / 10 pt / 9 pt; a half point; the argument wins over the option; the height estimate and the font follow", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B"), font = "Arial")
  tab <- data.frame(file = f, label = c("a", "b"), heading = c("H", NA))
  # the built-in values: title 14 pt bold (\fs28), gap 9 pt (\fs18), heading 11 pt bold, entry 10 pt
  p <- .tt_paras(.tt_asm(f, toc = tab))
  expect_identical(p[1L], "{\\pard\\qc\\b\\fs28 Table of Contents\\par}")
  expect_identical(p[2L], "{\\pard\\fs18\\par}")
  expect_identical(p[3L], "{\\pard\\b\\fs22 H\\par}")
  expect_match(p[4L], "^\\{\\\\pard\\\\li600\\\\fs20\\\\tqr\\\\tldot\\\\tx14112 ")
  # every item given: points are doubled to half points, bold FALSE drops \b
  st <- list(title = list(size = 16, bold = FALSE), heading = list(size = 12, bold = FALSE),
             entry = list(size = 9, bold = TRUE), gap = list(size = 6))
  q <- .tt_paras(.tt_asm(f, toc = tab, toc_style = st))
  expect_identical(q[1L], "{\\pard\\qc\\fs32 Table of Contents\\par}")
  expect_identical(q[2L], "{\\pard\\fs12\\par}")
  expect_identical(q[3L], "{\\pard\\fs24 H\\par}")
  expect_match(q[4L], "^\\{\\\\pard\\\\li600\\\\b\\\\fs18\\\\tqr\\\\tldot\\\\tx14112 ")
  # a half point: 10.5 pt is \fs21
  expect_match(.tt_entries(.tt_asm(f, toc = "auto", toc_style = list(entry = list(size = 10.5))))[1L],
               "^\\{\\\\pard\\\\fs21\\\\tqr")
  # a partial style is merged key by key: bold added with the size kept, size changed with bold kept
  p1 <- .tt_paras(.tt_asm(f, toc = tab, toc_style = list(entry = list(bold = TRUE))))
  expect_match(p1[4L], "^\\{\\\\pard\\\\li600\\\\b\\\\fs20\\\\tqr")
  expect_identical(p1[1L], p[1L])
  p2 <- .tt_paras(.tt_asm(f, toc = tab, toc_style = list(heading = list(size = 13))))
  expect_identical(p2[3L], "{\\pard\\b\\fs26 H\\par}")
  expect_identical(.tt_paras(.tt_asm(f, toc = tab, toc_style = list())), p)
  # the option, and the argument wins over it
  old <- options(rtfreporter.toc_style = list(title = list(size = 20)))
  on.exit(options(old), add = TRUE)
  expect_identical(.tt_paras(.tt_asm(f, toc = "auto"))[1L], "{\\pard\\qc\\b\\fs40 Table of Contents\\par}")
  expect_identical(.tt_paras(.tt_asm(f, toc = "auto", toc_style = list(title = list(size = 8))))[1L],
                   "{\\pard\\qc\\b\\fs16 Table of Contents\\par}")
  options(old)
  expect_true("rtfreporter.toc_style" %in% names(rtfreporter_options()))
  expect_null(getOption("rtfreporter.toc_style"))
  # the font is the document's: Arial from the input, none written by the TOC
  x <- .tt_asm(f, toc = tab)
  expect_match(x[2L], "Arial")
  expect_false(any(grepl("\\\\f[0-9]", .tt_paras(x))))
  # the height estimate uses the sizes: 30 entries fit on one page at 10 pt, not at 20 pt
  toc <- data.frame(file = f[rep_len(1:2, 30L)], label = sprintf("Entry %02d", 1:30))
  small <- .tt_asm(f, toc = toc)
  big <- .tt_asm(f, toc = toc, toc_style = list(entry = list(size = 20)))
  expect_length(grep("\\\\pagebb", small), 0L)
  expect_gt(length(grep("\\\\pagebb", big)), 0L)
})

test_that("FT-AS6204 [FR-AS263]: a TOC too long for a page is split where the estimate says, \\pagebb before the first entry of each further page, a heading that would close a page goes on with its entry; the cached page numbers of the body count the TOC pages; the TOC pages carry on the one sequence: no page-number command", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B"), pages = c(2L, 1L))
  # 34 entries, then a heading, then 6 entries: the heading would be the 35th line (the last of page 1)
  n <- 40L
  toc <- data.frame(file = f[rep_len(1:2, n)], label = sprintf("Entry %02d", seq_len(n)),
                    heading = c(rep(NA, 34L), "Safety", rep(NA, n - 35L)))
  x <- .tt_asm(f, toc = toc)
  p <- .tt_paras(x)
  pb <- grep("\\\\pagebb", p)
  expect_length(pb, 1L)                                          # two TOC pages: one break
  expect_identical(p[pb], "{\\pard\\pagebb\\b\\fs22 Safety\\par}")  # the heading goes to page 2 with its entry
  expect_match(p[pb + 1L], "Entry 35")
  expect_identical(sum(grepl("PAGEREF", p[seq_len(pb - 1L)])), 34L)  # page 1 holds the 34 entries
  # without a heading near the end: the break is before the 36th entry (35 lines a page)
  plain <- .tt_paras(.tt_asm(f, toc = toc[, c("file", "label")]))
  pb <- grep("\\\\pagebb", plain)
  expect_length(pb, 1L)
  expect_match(plain[pb], "^\\{\\\\pard\\\\pagebb")
  expect_match(plain[pb], "Entry 36")
  # the TOC section writes no numbering control word, nor does the body
  expect_true(any(x == "\\sectd\\sbkpage\\lndscpsxn"))
  expect_false(any(grepl("pgnlcrm|pgnrestart|pgndec", x)))
  # the cached numbers: 2 TOC pages, then the first file (2 pages) and the second
  expect_identical(.tt_pages(x)[1:2], c(3L, 5L))
})

test_that("FT-AS6205 [FR-AS265]: a label too long for a line stays in its one paragraph with its level's indent, the right tab and the leader, no hanging indent by default; the estimate counts the lines it wraps to at Courier New's width beside the indent and the page number", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  x <- readLines(.tt_case_longlabel(d), warn = FALSE)
  e <- .tt_entries(x)
  lab <- .tt_long_label()
  expect_length(e, 3L)
  expect_true(grepl(lab, e[2L], fixed = TRUE))                    # the whole label in the one paragraph
  expect_match(e[2L], "^\\{\\\\pard\\\\li600\\\\fs20\\\\tqr\\\\tldot\\\\tx14112 ")
  expect_match(e[3L], "^\\{\\\\pard\\\\li1200\\\\fs20\\\\tqr\\\\tldot\\\\tx14112 ")
  expect_false(any(grepl("\\\\fi[0-9-]", e)))
  expect_identical(sum(grepl("\\\\par\\}$", e)), 3L)              # three entries, three paragraphs
  # The estimate: a Courier New 10 pt character is 120 twips.  Level 2 has the width
  # 14112 - 600 (indent) - 720 (page number) = 12792 = 106.6 characters: 106 fit a line, 107 wrap.
  # A page holds 35 lines, so 34 one-line entries and one label of 106 characters fit one page,
  # and the same with a label of 107 characters (two lines, 36 lines) take two.
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B"))
  pages_of <- function(len, level) {
    toc <- data.frame(file = f[rep_len(1:2, 35L)], label = c(rep("x", 34L), strrep("y", len)),
                      level = c(rep(2L, 34L), level))
    sum(grepl("\\\\pagebb", .tt_paras(.tt_asm(f, toc = toc)))) + 1L
  }
  expect_identical(pages_of(106L, 2L), 1L)
  expect_identical(pages_of(107L, 2L), 2L)
  # Level 3: 14112 - 1200 - 720 = 12192 = 101.6 characters: 101 fit, 102 wrap
  expect_identical(pages_of(101L, 3L), 1L)
  expect_identical(pages_of(102L, 3L), 2L)
})

test_that("FT-AS6206 [FR-AS269]: toc_style entry hanging is FALSE by default (no \\fi, as before), TRUE writes \\li<indent + 720>\\fi-720, a whole number h writes \\li<indent + h>\\fi-h, headings are not affected, the estimate counts the wrapped lines with h less width, anything else or another key stops naming it; the option works and the argument wins", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B", t_14_3_1 = "C"))
  toc <- .t12_hang_toc(f)
  x0 <- .tt_asm(f, toc = toc)
  e0 <- .tt_entries(x0)
  # default: no \fi, and the explicit FALSE writes the same bytes
  expect_length(e0, 3L)
  expect_false(any(grepl("\\\\fi[0-9-]", e0)))
  expect_match(e0[2L], "^\\{\\\\pard\\\\li600\\\\fs20\\\\tqr")
  expect_identical(.tt_asm(f, toc = toc, toc_style = list(entry = list(hanging = FALSE))), x0)
  # TRUE: 720 on every level (indent 0, 600, 1200 -> 720, 1320, 1920), first line where it was
  x1 <- .tt_asm(f, toc = toc, toc_style = list(entry = list(hanging = TRUE)))
  e1 <- .tt_entries(x1)
  expect_match(e1[1L], "^\\{\\\\pard\\\\li720\\\\fi-720\\\\fs20\\\\tqr\\\\tldot\\\\tx14112 ")
  expect_match(e1[2L], "^\\{\\\\pard\\\\li1320\\\\fi-720\\\\fs20\\\\tqr\\\\tldot\\\\tx14112 ")
  expect_match(e1[3L], "^\\{\\\\pard\\\\li1920\\\\fi-720\\\\fs20\\\\tqr\\\\tldot\\\\tx14112 ")
  # a whole number of twips: 360 -> 360, 960, 1560
  e2 <- .tt_entries(.tt_asm(f, toc = toc, toc_style = list(entry = list(hanging = 360))))
  expect_match(e2[1L], "^\\{\\\\pard\\\\li360\\\\fi-360\\\\fs20\\\\tqr")
  expect_match(e2[2L], "^\\{\\\\pard\\\\li960\\\\fi-360\\\\fs20\\\\tqr")
  expect_match(e2[3L], "^\\{\\\\pard\\\\li1560\\\\fi-360\\\\fs20\\\\tqr")
  # headings and the title are as they were
  noent <- function(l) .tt_paras(l)[!grepl("PAGEREF", .tt_paras(l))]
  expect_identical(noent(x1), noent(x0))
  expect_false(any(grepl("\\\\fi[0-9-]", noent(x1))))
  # the estimate: 33 one-line entries and one of 210 characters at level 2 (106 on the first line)
  # are 33 + 2 = 35 lines without a hanging indent; with 720 the wrapped line holds
  # floor((12792 - 720) / 120) = 100, so 210 - 106 = 104 characters need two: 33 + 3 = 36 lines
  g <- .tt_inputs(d, c(t_14_4_1 = "D", t_14_5_1 = "E"))
  big <- data.frame(file = g[rep_len(1:2, 34L)], label = c(rep("x", 33L), strrep("y", 210L)), level = 2L)
  npb <- function(style) {
    sum(grepl("\\\\pagebb", .tt_paras(.tt_asm(g, toc = big, toc_style = style))))
  }
  expect_identical(npb(NULL), 0L)
  expect_identical(npb(list(entry = list(hanging = TRUE))), 1L)
  expect_identical(npb(list(entry = list(hanging = 720))), 1L)
  # the option sets it, and the argument wins over it
  old <- options(rtfreporter.toc_style = list(entry = list(hanging = 360)))
  on.exit(options(old), add = TRUE)
  expect_match(.tt_entries(.tt_asm(f, toc = toc))[2L], "^\\{\\\\pard\\\\li960\\\\fi-360\\\\fs20\\\\tqr")
  expect_match(.tt_entries(.tt_asm(f, toc = toc, toc_style = list(entry = list(hanging = TRUE))))[2L],
               "^\\{\\\\pard\\\\li1320\\\\fi-720\\\\fs20\\\\tqr")
  expect_false(any(grepl("\\\\fi[0-9-]", .tt_entries(
    .tt_asm(f, toc = toc, toc_style = list(entry = list(hanging = FALSE)))))))
  options(old)
  # what is not TRUE, FALSE or one positive whole number stops naming it; so does another key
  out <- file.path(d, "bad.rtf")
  bad <- function(h) assemble_rtf(f, out, toc = "auto", toc_style = list(entry = list(hanging = h)))
  msg <- "`toc_style$entry$hanging` must be TRUE, FALSE or a positive whole number of twips"
  for (h in list("yes", -1, 0, 1.5, c(1, 2), NA, NA_real_, c(TRUE, FALSE))) {
    expect_error(bad(h), msg, fixed = TRUE)
  }
  expect_error(assemble_rtf(f, out, toc = "auto", toc_style = list(heading = list(hanging = TRUE))),
               "`toc_style$heading` has unknown key(s) 'hanging'; it takes size, bold", fixed = TRUE)
  expect_error(assemble_rtf(f, out, toc = "auto", toc_style = list(title = list(hanging = 360))),
               "`toc_style$title` has unknown key(s) 'hanging'", fixed = TRUE)
  expect_error(assemble_rtf(f, out, toc = "auto", toc_style = list(gap = list(hanging = TRUE))),
               "`toc_style$gap` has unknown key(s) 'hanging'", fixed = TRUE)
})

test_that("FT-AS6207 [FR-AS250]: a contents entry read from a title is the title as the file shows it: own and run tokens and static {PAGE} / {TOTAL_PAGES} filled, non-ASCII decoded, dynamic page tokens and the book slot left out, markup keeping its text; the outline entry is the same; for toc = \"auto\" and assemble_folder()", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  mk <- function(name, title, tokens = NULL) {
    f <- file.path(d, name)
    doc <- rtf_document(tokens = tokens, program = "prog_x.R") |>
      rtf_tables(data.frame(A = 1:2, B = "x"), titles = list(c(title)))
    generate_rtfreport(doc, f, overwrite = TRUE)
    f
  }
  f1 <- mk("t_14_1_1.rtf", paste("Table 14.1.1 {STUDY} {PROGRAM} p{PAGE}/{TOTAL_PAGES}",
                                 "[{AUTO_PAGE}/{AUTO_TOTAL_PAGES}] {BOOK_PAGE} H_{2}O µg"),
           tokens = list(STUDY = "XY-1"))
  f2 <- mk("t_14_2_1.rtf", "Table 14.2.1 Plain")
  # the input shows the dynamic tokens as fields, the book slot as a slot, the markup as a group
  src <- paste(readLines(f1, warn = FALSE), collapse = "\n")
  expect_true(grepl("\\chpgn", src, fixed = TRUE))
  expect_true(grepl("NUMPAGES", src, fixed = TRUE))
  expect_true(grepl("{\\*\\rtfreporterbookpage}", src, fixed = TRUE))
  expect_true(grepl("H{\\sub 2}O", src, fixed = TRUE))
  # toc = "auto": own token XY-1, run token prog_x.R, static page 1 of 1, µ decoded; the
  # dynamic page tokens and the slot gone ("[ / ]" is what is left of "[{AUTO_PAGE}/{AUTO_TOTAL_PAGES}]")
  out <- file.path(d, "out.rtf")
  assemble_rtf(c(f1, f2), out, toc = "auto")
  l <- readLines(out, warn = FALSE)
  ent <- grep("HYPERLINK", l, value = TRUE)
  want <- "Table 14.1.1 XY-1 prog_x.R p1/1 [ / ] H2O \\u181?g"
  expect_true(grepl(paste0("{\\fldrslt ", want, "}}"), ent[1L], fixed = TRUE))
  expect_true(grepl("{\\fldrslt Table 14.2.1 Plain}}", ent[2L], fixed = TRUE))
  expect_false(any(grepl("\\\\chpgn|NUMPAGES\\}|rtfreporterbookpage|\\\\sub", ent[1L])))
  # the outline entry is the same text
  ol <- grep("outlinelevel", l, value = TRUE)
  expect_identical(ol[1L], paste0("{\\pard\\plain\\cf2\\fs2\\sl1\\slmult0\\sa0\\sb0\\outlinelevel0 ", want, "\\par}"))
  # assemble_folder(): the same label, in the table it returns
  sp <- assemble_folder(d)
  lab <- sp$label[grepl("t_14_1_1", sp$file, fixed = TRUE)][1L]
  expect_true(grepl("XY-1 prog_x.R p1/1 [ / ] H2O µg", lab, fixed = TRUE))
  expect_false(grepl("{", lab, fixed = TRUE))
})

# ------------------------------------------------------------------- DC

# The cell definitions (the border words before each \cellx) of the row that
# holds `key`, in a rendered document; a header or footer band has all its rows
# on one line.
.t12_rowdefs <- function(txt, key) {
  rows <- regmatches(txt, gregexpr("\\\\trowd.*?\\\\row(?![a-z])", txt, perl = TRUE))[[1L]]
  r <- rows[grepl(key, rows, fixed = TRUE)][1L]
  if (is.na(r)) stop("no row holds ", key)
  defs <- strsplit(sub("^.*?\\\\trowd(\\\\trrh[0-9]+)?(\\\\trq[cr])?", "", r, perl = TRUE),
                   "\\\\cellx[0-9]+")[[1L]]
  defs <- defs[seq_len(lengths(regmatches(r, gregexpr("\\\\cellx", r))))]
  if (!length(defs)) stop("no cells in the row of ", key)
  defs
}
.t12_side <- function(def, side) grepl(paste0("\\clbrdr", side), def, fixed = TRUE)

# A document that carries rows `rows` as header, footer and footnote block, with
# the block border `border` on each (keys are in the rows: H, F, N).
.t12_band_doc <- function(rows_for, border = NULL) {
  rtf_document() |>
    rtf_section(page = 1, secinfo = list(
      header = rtf_header(rows_for("H"), border = border),
      footer = rtf_footer(rows_for("F"), border = border))) |>
    rtf_tables(list(data.frame(A = "1", B = "x", stringsAsFactors = FALSE))) |>
    rtf_footnotes(list(rows_for("N")), border = border)
}

test_that("FT-DC6201 [FR-DC359]: a token's value is written as it is whatever it holds: a value of one's own (the document, the option), a page's column value, the program's path; {PAGE}, {AUTO_PAGE}, {PROGRAM}, {DATETIME} and another own token print with their braces in a header, footer, title and footnote", {
  held <- list(OWN1 = "{PAGE}", OWN2 = "{AUTO_PAGE}", OWN3 = "{PROGRAM}", OWN4 = "{DATETIME}",
               OWN5 = "{OWN1}")
  text <- "{OWN1}|{OWN2}|{OWN3}|{OWN4}|{OWN5}"
  want <- "\\{PAGE\\}|\\{AUTO_PAGE\\}|\\{PROGRAM\\}|\\{DATETIME\\}|\\{OWN1\\}"
  build <- function(tokens = NULL) {
    rtf_document(tokens = tokens, program = "prog/t_14.R") |>
      rtf_section(page = 1, secinfo = list(
        header = rtf_header(c(l = paste("h[", text, "]"))),
        footer = rtf_footer(c(l = paste("f[", text, "]"))))) |>
      rtf_tables(data.frame(A = "1", B = "x"), titles = list(paste("t[", text, "]")),
                 footnotes = list(list(paste("n[", text, "]"))))
  }
  check <- function(txt, info) {
    for (k in c("h", "f", "t", "n")) {
      expect_true(grepl(paste0(k, "[ ", want, " ]"), txt, fixed = TRUE), info = paste(info, k))
    }
    # none was filled again by the tokens substituted after it
    expect_false(grepl("\\chpgn", txt, fixed = TRUE), info = info)
    expect_false(grepl("prog/t_14.R", txt, fixed = TRUE), info = info)
  }
  # the document's own tokens
  check(.render_to_string(build(held)), "document")
  # the option rtfreporter.tokens
  old <- options(rtfreporter.tokens = held)
  on.exit(options(old), add = TRUE)
  check(.render_to_string(build()), "option")
  options(old)
  # a page's value by column: the column holds {PAGE} and {AUTO_PAGE}
  pages <- as_rtftables(data.frame(G = c("{PAGE}", "{AUTO_PAGE}"), V = 1:2),
                        split = "by_value", group_col = "G")
  doc2 <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = NULL,
                                         footer = rtf_footer(list(c(l = "g[{G}]"))))) |>
    rtf_tables(pages)
  txt2 <- .render_to_string(doc2)
  expect_true(grepl("g[\\{PAGE\\}]", txt2, fixed = TRUE))
  expect_true(grepl("g[\\{AUTO_PAGE\\}]", txt2, fixed = TRUE))
  expect_false(grepl("\\chpgn", txt2, fixed = TRUE))
  # the program's path that holds a token: kept as written, its directory read from it
  doc3 <- rtf_document(program = "a/{PAGE}.R") |>
    rtf_section(page = 1, secinfo = list(header = NULL,
      footer = rtf_footer(list(c(l = "p[{PROGRAM}] [{PROGRAM_DIR}]"))))) |>
    rtf_tables(data.frame(A = "1"))
  expect_true(grepl("p[a/\\{PAGE\\}.R] [a]", .render_to_string(doc3), fixed = TRUE))
})

test_that("FT-DC6202 [FR-DC361]: in a header, footer, title or footnote {{ is { and }} is }, a token in doubled braces is not filled while a single-brace one beside it is, an unknown token and a single brace print as written, ^{{STUDY}} raises the token's value; a body cell prints doubled braces as written", {
  cases <- data.frame(
    text = c("a {{ b }} c", "{{PAGE}}", "{{STUDY}} {STUDY}", "{{PAGE}} {PAGE}", "{{x}}",
             "{UNKNOWN}", "a { b", "a } b", "{{{{ and }}}}"),
    want = c("a \\{ b \\} c", "\\{PAGE\\}", "\\{STUDY\\} EX-1", "\\{PAGE\\} 1", "\\{x\\}",
             "\\{UNKNOWN\\}", "a \\{ b", "a \\} b", "\\{\\{ and \\}\\}"),
    stringsAsFactors = FALSE)
  mk <- function(text, where, markup = "none") {
    mark <- paste("Mk", text, "kM")
    doc <- rtf_document(tokens = list(STUDY = "EX-1")) |>
      rtf_tables(list(rtftable(data.frame(A = if (where == "cell") mark else "x"), markup = markup)))
    if (where == "title") doc <- rtf_titles(doc, list(mark))
    if (where == "footnote") doc <- rtf_footnotes(doc, list(mark))
    if (where == "header") doc <- rtf_section(doc, secinfo = list(header = rtf_header(c(l = mark), markup = markup)))
    if (where == "footer") doc <- rtf_section(doc, secinfo = list(footer = rtf_footer(c(l = mark), markup = markup)))
    .render_to_string(doc)
  }
  mk_got <- function(txt) sub("^.*?(Mk .*? kM).*$", "\\1", grep("Mk ", strsplit(txt, "\n", fixed = TRUE)[[1L]], value = TRUE)[1L])
  for (where in c("header", "footer", "title", "footnote")) {
    for (i in seq_len(nrow(cases))) {
      expect_identical(mk_got(mk(cases$text[i], where)), paste("Mk", cases$want[i], "kM"),
                       info = paste(where, cases$text[i]))
    }
    # with markup = "script": the markup's braces are read first, then the token
    expect_true(grepl("Mk {\\super EX-1} kM", mk_got(mk("^{{STUDY}}", where, "script")), fixed = TRUE),
                info = where)
  }
  # a table's body cell prints doubled braces as written, token-free
  expect_identical(mk_got(mk("{{PAGE}}", "cell")), "Mk \\{\\{PAGE\\}\\} kM")
})

test_that("FT-DC6203 [FR-DC362]: rtf_header(border = ) / rtf_footer(border = ) is the band's border as a whole: top above the first row, bottom below the last, inside_h between, left / right on the outer edges of every row, inside_v between cells, all the four outer edges; after drop_empty_rows; a one-row band and a top rule alone are as before", {
  rows3 <- list(c(l = "Ra", r = "Rb"), c(l = "Sa", c = "Sb", r = "Sc"), c(c = "Tc"))
  for (kind in c("header", "footer")) {
    mkdoc <- function(rows, border, ...) {
      band <- if (kind == "header") rtf_header(rows, border = border, ...) else rtf_footer(rows, border = border, ...)
      rtf_document() |>
        rtf_section(page = 1, secinfo = stats::setNames(list(band), kind)) |>
        rtf_tables(list(data.frame(A = "1", B = "x", stringsAsFactors = FALSE)))
    }
    defs <- function(txt, key) .t12_rowdefs(txt, key)
    # a three-row band: top on the first row's cells, inside_h between, bottom (double) below the last
    b <- .render_to_string(mkdoc(rows3, rtf_border(top = TRUE, bottom = "double", inside_h = "dot")))
    expect_true(all(.t12_side(defs(b, "Ra"), "t")), info = kind)
    expect_false(any(.t12_side(defs(b, "Sa"), "t")), info = kind)
    expect_false(any(.t12_side(defs(b, "Tc"), "t")), info = kind)
    expect_true(all(.t12_has(defs(b, "Ra"), "\\clbrdrb\\brdrdot")), info = kind)   # inside_h under row 1
    expect_true(all(.t12_has(defs(b, "Sa"), "\\clbrdrb\\brdrdot")), info = kind)   # and under row 2
    expect_true(all(.t12_has(defs(b, "Tc"), "\\clbrdrb\\brdrdb")), info = kind)    # bottom under row 3
    # left / right on every row's outer edges, inside_v between the cells of a row
    v <- .render_to_string(mkdoc(rows3, rtf_border(left = TRUE, right = TRUE, inside_v = "dot")))
    for (key in c("Ra", "Sa")) {
      d <- defs(v, key)
      expect_true(.t12_has(d[1L], "\\clbrdrl\\brdrs"), info = paste(kind, key))
      expect_true(.t12_has(d[1L], "\\clbrdrr\\brdrdot"), info = paste(kind, key))
      expect_true(.t12_has(d[length(d)], "\\clbrdrr\\brdrs"), info = paste(kind, key))
      expect_true(.t12_has(d[length(d)], "\\clbrdrl\\brdrdot"), info = paste(kind, key))
    }
    one <- defs(v, "Tc")                                           # a one-cell row: no inside rule
    expect_length(one, 1L)
    expect_true(.t12_has(one, "\\clbrdrl\\brdrs") && .t12_has(one, "\\clbrdrr\\brdrs"))
    expect_false(.t12_has(one, "brdrdot"))
    # all = TRUE: the four outer edges
    a <- .render_to_string(mkdoc(rows3[1:2], rtf_border(all = TRUE)))
    expect_true(all(.t12_side(defs(a, "Ra"), "t")))
    expect_false(any(.t12_side(defs(a, "Ra"), "b")))
    expect_true(all(.t12_side(defs(a, "Sa"), "b")))
    expect_false(any(.t12_side(defs(a, "Sa"), "t")))
    expect_true(.t12_side(defs(a, "Ra")[1L], "l") && .t12_side(defs(a, "Sa")[1L], "l"))
    # a bottom on a two-row band is below the last row (not the first, as it was written before 0.9.0)
    two <- .render_to_string(mkdoc(rows3[1:2], rtf_border(bottom = TRUE)))
    expect_false(any(.t12_side(defs(two, "Ra"), "b")))
    expect_true(all(.t12_side(defs(two, "Sa"), "b")))
    # a one-row band: bottom, left / right, inside_v are that row's
    o <- defs(.render_to_string(mkdoc(rows3[1L], rtf_border(bottom = TRUE, left = TRUE, right = TRUE,
                                                              inside_v = "dot"))), "Ra")
    expect_true(all(.t12_side(o, "b")))
    expect_true(.t12_has(o[1L], "\\clbrdrl\\brdrs") && .t12_has(o[1L], "\\clbrdrr\\brdrdot"))
    expect_true(.t12_has(o[2L], "\\clbrdrl\\brdrdot") && .t12_has(o[2L], "\\clbrdrr\\brdrs"))
    # drop_empty_rows: the zone is the rows written; the empty last row is left out, bottom under the first
    tok <- function(pop) {
      band <- if (kind == "header") rtf_header(list(c(l = "Ra", r = "Rb"), c(c = "{POP}")),
                                               border = rtf_border(bottom = TRUE), drop_empty_rows = TRUE)
      else rtf_footer(list(c(l = "Ra", r = "Rb"), c(c = "{POP}")), border = rtf_border(bottom = TRUE),
                      drop_empty_rows = TRUE)
      .render_to_string(rtf_document(tokens = list(POP = pop)) |>
        rtf_section(page = 1, secinfo = stats::setNames(list(band), kind)) |>
        rtf_tables(list(data.frame(A = "1"))))
    }
    kept <- tok("Safety")
    expect_false(any(.t12_side(defs(kept, "Ra"), "b")))
    expect_true(all(.t12_side(defs(kept, "Safety"), "b")))
    gone <- tok("")
    expect_true(all(.t12_side(defs(gone, "Ra"), "b")))
  }
  # the footer's default (a top rule alone): the first row only, as before
  f <- .render_to_string(rtf_document() |>
    rtf_section(page = 1, secinfo = list(footer = rtf_footer(rows3))) |>
    rtf_tables(list(data.frame(A = "1"))))
  expect_identical(unique(.t12_rowdefs(f, "Ra")), "\\clbrdrt\\brdrs\\brdrw15")
  expect_identical(unique(c(.t12_rowdefs(f, "Sa"), .t12_rowdefs(f, "Tc"))), "")
})

test_that("FT-DC6204 [FR-DC363]: a header, footer or footnote row given as a list takes border = merged side by side over the block's (\"none\" erases), on its outer edges and inside_v; a line both adjacent rows draw is drawn once as the upper row's bottom; a lower row's top with nothing above stays; its colour is in the colour table", {
  own <- function(cols, border) list(columns = cols, border = border)
  # (1) the row's sides win over the block's, side by side; "none" erases one
  s1 <- function(k) list(own(c(l = paste0(k, "1a"), r = paste0(k, "1b")), rtf_border(bottom = TRUE)),
                         own(c(l = paste0(k, "2a"), r = paste0(k, "2b")),
                             rtf_border(bottom = "none", left = "double")))
  t1 <- .render_to_string(.t12_band_doc(s1, rtf_border(bottom = "dot", left = TRUE)))
  for (k in c("H", "F", "N")) {
    r1 <- .t12_rowdefs(t1, paste0(k, "1a"))
    r2 <- .t12_rowdefs(t1, paste0(k, "2a"))
    expect_true(all(.t12_has(r1, "\\clbrdrb\\brdrs")), info = k)       # its own bottom over the block's dot
    expect_true(.t12_has(r1[1L], "\\clbrdrl\\brdrs"), info = k)         # the block's left kept
    expect_false(any(.t12_side(r2, "b")), info = k)                     # "none" erases the block's bottom
    expect_true(.t12_has(r2[1L], "\\clbrdrl\\brdrdb"), info = k)        # its own left wins
  }
  # (2) a boundary both rows draw is drawn once, as the upper row's bottom
  s2 <- function(k) list(c(l = paste0(k, "1")), own(c(l = paste0(k, "2")), rtf_border(top = "double")))
  t2 <- .render_to_string(.t12_band_doc(s2, rtf_border(inside_h = TRUE)))   # inside_h under row 1 only
  t3 <- .render_to_string(.t12_band_doc(s2, NULL))                          # nothing above the lower row's top
  s4 <- function(k) list(own(c(l = paste0(k, "1")), rtf_border(bottom = TRUE)),
                         own(c(l = paste0(k, "2")), rtf_border(top = "double")))
  t4 <- .render_to_string(.t12_band_doc(s4, NULL))                          # both draw it: the upper's bottom
  for (k in c("H", "F", "N")) {
    expect_true(.t12_has(.t12_rowdefs(t2, paste0(k, "1")), "\\clbrdrb\\brdrdb"), info = k)   # the lower's top replaces
    expect_false(.t12_side(.t12_rowdefs(t2, paste0(k, "2")), "t"), info = k)
    expect_false(.t12_side(.t12_rowdefs(t3, paste0(k, "1")), "b"), info = k)
    expect_true(.t12_has(.t12_rowdefs(t3, paste0(k, "2")), "\\clbrdrt\\brdrdb"), info = k)   # stays its top
    expect_true(.t12_has(.t12_rowdefs(t4, paste0(k, "1")), "\\clbrdrb\\brdrs"), info = k)    # the upper's bottom
    expect_false(.t12_side(.t12_rowdefs(t4, paste0(k, "2")), "t"), info = k)
  }
  # (3) a row's inside_v: between that row's cells only
  s5 <- function(k) list(own(c(l = paste0(k, "1a"), r = paste0(k, "1b")),
                             rtf_border(left = TRUE, right = TRUE, inside_v = "dot")),
                         c(l = paste0(k, "2a"), r = paste0(k, "2b")))
  t5 <- .render_to_string(.t12_band_doc(s5, NULL))
  for (k in c("H", "F", "N")) {
    d <- .t12_rowdefs(t5, paste0(k, "1a"))
    expect_true(.t12_has(d[1L], "\\clbrdrl\\brdrs") && .t12_has(d[1L], "\\clbrdrr\\brdrdot"), info = k)
    expect_true(.t12_has(d[2L], "\\clbrdrl\\brdrdot") && .t12_has(d[2L], "\\clbrdrr\\brdrs"), info = k)
    expect_false(any(.t12_side(.t12_rowdefs(t5, paste0(k, "2a")), "r")), info = k)
  }
  # (4) the colour: in the colour table, and the border words carry it
  s6 <- function(k) list(own(c(l = paste0(k, "1")), rtf_border(bottom = rtf_border_line(color = "#C00000"))))
  t6 <- .render_to_string(.t12_band_doc(s6, NULL))
  expect_true(.t12_has(t6, "\\red192\\green0\\blue0"))
  for (k in c("H", "F", "N")) {
    expect_true(.t12_has(.t12_rowdefs(t6, paste0(k, "1")), "\\clbrdrb\\brdrs\\brdrw15\\brdrcf"), info = k)
  }
})

test_that("FT-DC6205 [FR-DC383]: a footnote block in its table form is placed as its content is: \\trqc under a centred figure or table, \\trqr under a right one, neither under a left one; as wide as its content by default, footnote_width = \"page\" the writable width placed the same; a page header and footer not placed; the text form has no table placement", {
  png <- test_path("fixtures", "validation", "dot.png")
  fig <- function(align, style = NULL) {
    rtf_document(style = style) |>
      rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
      rtf_figures(list(rtfplot(png, width_twips = 4320L, height_twips = 2880L, align = align)),
                  titles = "Figure 14.2.1", footnotes = c("Note A", "Note B"))
  }
  tbl <- function(align, style = NULL, width = 6000L) {
    rtf_document(style = style) |>
      rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
      rtf_tables(list(rtftable(data.frame(A = 1L, B = "x", stringsAsFactors = FALSE),
                               table_align = align, table_width_twips = width)),
                 footnotes = list(c("Note A", "Note B")))
  }
  rows <- function(txt, text) {
    r <- regmatches(txt, gregexpr("\\\\trowd[^\n]*", txt))[[1L]]
    r[grepl(text, r, fixed = TRUE)]
  }
  last_cellx <- function(r) vapply(r, function(x) {
    m <- regmatches(x, gregexpr("cellx[0-9]+", x))[[1L]]
    as.integer(sub("cellx", "", m[length(m)], fixed = TRUE))
  }, 0L, USE.NAMES = FALSE)
  word <- c(center = "\\trqc", right = "\\trqr", left = "")
  for (al in names(word)) {
    # the figure (the picture 4320 twips wide) and the table (6000 twips wide): the width of the content
    for (doc in list(fig(al), tbl(al))) {
      fn <- rows(.render_to_string(doc), "Note ")
      expect_length(fn, 2L)
      if (nzchar(word[[al]])) {
        expect_true(all(grepl(word[[al]], fn, fixed = TRUE)), info = al)
      } else {
        expect_false(any(grepl("\\\\trq[cr]", fn)), info = al)
      }
    }
    expect_identical(last_cellx(rows(.render_to_string(fig(al)), "Note ")), c(4320L, 4320L), info = al)
    expect_identical(last_cellx(rows(.render_to_string(tbl(al)), "Note ")), c(6000L, 6000L), info = al)
    # footnote_width = "page": the writable width (13680), placed as the content is
    pg <- rows(.render_to_string(fig(al, rtf_style(footnote_width = "page"))), "Note ")
    expect_identical(last_cellx(pg), c(13680L, 13680L), info = al)
    if (nzchar(word[[al]])) expect_true(all(grepl(word[[al]], pg, fixed = TRUE)), info = al)
    else expect_false(any(grepl("\\\\trq[cr]", pg)), info = al)
    pt <- rows(.render_to_string(tbl(al, rtf_style(footnote_width = "page"))), "Note ")
    expect_identical(last_cellx(pt), c(13680L, 13680L), info = al)
  }
  # a page header and footer are not placed: they stay at the margin, the footnotes still centred
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = rtf_header(c(l = "HDR")),
                                         footer = rtf_footer(c(l = "FTR")))) |>
    rtf_figures(list(rtfplot(png, width_twips = 4320L, height_twips = 2880L)), footnotes = "Note A")
  txt <- .render_to_string(doc)
  expect_true(all(grepl("\\trqc", rows(txt, "Note A"), fixed = TRUE)))
  expect_false(any(grepl("\\\\trq[cr]", rows(txt, "HDR"))))
  expect_false(any(grepl("\\\\trq[cr]", rows(txt, "FTR"))))
  # the text form: plain paragraphs, no table placement
  tx <- .render_to_string(fig("center", rtf_style(footnote_format = "text")))
  expect_false(any(grepl("\\\\trq[cr]", rows(tx, "Note A"))))
  expect_true(grepl("Note A", tx, fixed = TRUE))
})

# ------------------------------------------------------------------- IX

# Three groups A, B, C of an n row and a Mean row.
.t12_wide <- function() {
  data.frame(grp = c("A", "A", "B", "B", "C", "C"),
             label = c("n", "Mean", "n", "Mean", "n", "Mean"),
             X = c("1", "2.5", "3", "4.5", "5", "6.5"),
             Y = c("7", "8.5", "9", "10.5", "11", "12.5"), stringsAsFactors = FALSE)
}

test_that("FT-IX6201 [FR-IX253]: with plan_stub(before = TRUE) a plan_cell_style() condition sees the folded rows (the heading rows, key NA there); with before = FALSE and a cell style the plan is refused", {
  d <- .t12_wide()
  folded <- table_plan(d) |> plan_stub(vars = c("grp", "label"), name = "rl", before = TRUE)
  # a condition on a key: only the rows of group B (rows 5 and 6 of the folded table: A, n, Mean, B, n, Mean, ...)
  pg <- .t12_pages(folded |> plan_cell_style(bold = ~ !is.na(grp) & grp == "B") |>
                     plan_style(border = "tfl"))[[1L]]
  expect_identical(sub("^[^[:alnum:]]+", "", as.vector(pg$data$rl)),
                   c("A", "n", "Mean", "B", "n", "Mean", "C", "n", "Mean"))   # 3 headings + 6 rows
  on <- vapply(pg$cell_styles, function(z) !is.null(z) && isTRUE(z$bold[[1L]]), NA)
  expect_identical(which(on), 5:6)               # B's n and Mean, not B's heading row (row 4)
  # a heading row has no source row: a condition on a key reads NA there
  pg2 <- .t12_pages(folded |> plan_cell_style(italic = ~ is.na(grp)) |> plan_style(border = "tfl"))[[1L]]
  on2 <- vapply(pg2$cell_styles, function(z) !is.null(z) && isTRUE(z$italic[[1L]]), NA)
  expect_identical(pg2$data$rl[on2], c("A", "B", "C"))
  # before = FALSE (the default) folds inside as_rtftables(): refused with a reason when a cell style is there
  p <- table_plan(d) |> plan_cell_style(bold = ~ TRUE) |> plan_stub(vars = c("grp", "label"))
  expect_error(plan_apply(p, "pages"), "before = TRUE")
  expect_error(plan_apply(p, "pages"), "wrong row")
  p2 <- table_plan(d) |> plan_cell_style(bold = ~ TRUE) |>
    plan_stub(vars = c("grp", "label"), before = FALSE)
  expect_error(plan_apply(p2, "pages"), "before = TRUE")
  # without a cell style, before = FALSE is fine
  expect_s3_class(.t12_pages(table_plan(d) |> plan_stub(vars = c("grp", "label")))[[1L]], "rtftable")
})

test_that("FT-IX6202 [FR-IX262]: plan_paginate_rows(split = ): group_safe moves a group that would cross the budget whole to the next page, group_force cuts it at the budget with the continuation label, rows and none refuse max_rows", {
  d <- data.frame(grp = rep(c("A", "B", "C"), each = 2L),
                  label = c("a1", "a2", "b1", "b2", "c1", "c2"), X = as.character(1:6),
                  stringsAsFactors = FALSE)
  # groups of 2 rows, a budget of 3: the next group would cross it
  safe <- .t12_pages(table_plan(d) |> plan_paginate_rows(max_rows = 3, split = "group_safe",
                                                          min_group_rows = 0))
  expect_identical(lapply(safe, function(p) p$data$grp), list(c("A", "A"), c("B", "B"), c("C", "C")))
  # group_force: a page of 3 rows (the continuation row counted), the group cut, "(cont)" opening the next page
  force <- .t12_pages(table_plan(d) |> plan_paginate_rows(max_rows = 3, split = "group_force",
                                                           min_group_rows = 0, cont_label = "(cont)"))
  expect_identical(lapply(force, function(p) p$data$grp),
                   list(c("A", "A", "B"), c("B(cont)", "B", "C"), c("C(cont)", "C")))
  expect_identical(lapply(force, function(p) p$data$label),
                   list(c("a1", "a2", "b1"), c("", "b2", "c1"), c("", "c2")))
  expect_true(all(vapply(force, function(p) nrow(p$data) <= 3L, NA)))
  expect_equal(force, .t12_direct(d, max_rows = 3, split = "group_force", min_group_rows = 0,
                                  cont_label = "(cont)"))
  # split = "rows" / "none": no budget by count, max_rows is refused
  p <- table_plan(d)
  expect_error(plan_apply(plan_paginate_rows(p, max_rows = 4, split = "rows", break_before = 3), "args"),
               "plan_paginate_rows(max_rows = 4) has no effect with split = 'rows'.", fixed = TRUE)
  expect_error(plan_apply(plan_paginate_rows(p, max_rows = 4, split = "none"), "args"),
               "plan_paginate_rows(max_rows = 4) has no effect with split = 'none'.", fixed = TRUE)
  expect_error(plan_apply(plan_paginate_rows(p, max_rows = 4, split = "none"), "args"),
               "Drop `max_rows`", fixed = TRUE)
})

test_that("FT-IX6203 [FR-IX327]: rtftable(blank_rows = ) given explicitly wins over the positions set_blank_rows() stored (attr rtf_blank_rows); left NULL with read_attributes = TRUE the attribute is used, with FALSE it is ignored", {
  df <- data.frame(Grp = c("A", "A", "B", "B", "C"), Val = 1:5, stringsAsFactors = FALSE)
  attr(df, "rtf_blank_rows") <- c(0L, -1L)          # 0 = above the first row, -1 = after the last (5)
  expect_identical(rtftable(df, blank_rows = 2L)$blank_rows, 2L)          # explicit wins
  expect_identical(rtftable(df, blank_rows = 2L, read_attributes = FALSE)$blank_rows, 2L)
  expect_identical(rtftable(df)$blank_rows, c(0L, 5L))                    # the attribute, by default
  expect_identical(rtftable(df, read_attributes = TRUE)$blank_rows, c(0L, 5L))
  expect_null(rtftable(df, read_attributes = FALSE)$blank_rows)           # ignored
  # the attribute set_blank_rows() writes
  out <- set_blank_rows(df, blank_rows = "between_groups", group_col = "Grp")
  expect_identical(attr(out, "rtf_blank_rows"), c(2L, 4L))
  expect_identical(rtftable(out)$blank_rows, c(2L, 4L))
  expect_identical(rtftable(out, blank_rows = 3L)$blank_rows, 3L)
  expect_null(rtftable(out, read_attributes = FALSE)$blank_rows)
})

test_that("FT-IX6204 [FR-IX328]: blank_rows given as a list of positions, \"between_groups\", blank_rows_by_change() and blank_rows_by_rule() puts a blank wherever any of them says, once: the entries are unioned, duplicates dropped; blank_row_first and blank_row_end add 0 and nrow(data)", {
  df <- data.frame(Grp = c("A", "A", "B", "B", "C"), Val = 1:5, stringsAsFactors = FALSE)
  by_change <- blank_rows_by_change("Grp", include_before_first = FALSE, include_after_last = FALSE)  # 2, 4
  by_rule <- blank_rows_by_rule("Grp", "^C", "before")                                               # 4
  # rtftable(): positions (-1 = after the last row, 5), by_change (2, 4), by_rule (4): the union 2, 4, 5
  expect_identical(rtftable(df, blank_rows = list(-1, by_change, by_rule))$blank_rows, c(2L, 4L, 5L))
  # a position every entry also says is kept once: 2 is said by the list and by_change, 4 by by_change and by_rule
  expect_identical(rtftable(df, blank_rows = list(2L, 2L, by_change, by_rule))$blank_rows, c(2L, 4L))
  # set_blank_rows(): the list, "between_groups" (2, 4), blank_row_first (0) and blank_row_end (5)
  out <- set_blank_rows(df, blank_rows = list(1L, 2L, "between_groups", by_change, by_rule),
                        group_col = "Grp", blank_row_first = TRUE, blank_row_end = TRUE)
  expect_identical(attr(out, "rtf_blank_rows"), c(0L, 1L, 2L, 4L, 5L))
  # without the first / end: 1, 2, 4
  out2 <- set_blank_rows(df, blank_rows = list(1L, 2L, "between_groups", by_change, by_rule),
                         group_col = "Grp")
  expect_identical(attr(out2, "rtf_blank_rows"), c(1L, 2L, 4L))
  # the same set reaches the pages: as_rtftables() with the same arguments
  tb <- as_rtftables(df, blank_rows = list(1L, 2L, by_change, by_rule), blank_row_first = TRUE,
                     blank_row_end = TRUE, read_meta = FALSE)
  tb <- if (inherits(tb, "rtftable")) tb else tb[[1L]]
  expect_identical(tb$blank_rows, c(0L, 1L, 2L, 4L, 5L))
})

# The word of a cell definition's border on `side` ("b", "t", "l", "r"): "s" single, "th" thick, ...; "" none.
.t12_bw <- function(def, side) {
  vapply(def, function(x) {
    m <- regmatches(x, regexpr(paste0("\\\\clbrdr", side, "\\\\brdr[a-z]+"), x))
    if (length(m)) sub("^.*\\\\brdr", "", m) else ""
  }, "", USE.NAMES = FALSE)
}
.t12_3x3 <- function() {
  data.frame(A = c("a1", "a2", "a3"), B = c("b1", "b2", "b3"), C = c("c1", "c2", "c3"),
             stringsAsFactors = FALSE)
}
.t12_tbl_rtf <- function(tbl) .t12_render(rtf_document() |> rtf_tables(list(tbl)))

test_that("FT-IX6205 [FR-IX331]: style_body(border = ) is merged per side on top of the row's zone border: a side it names wins, one it leaves NULL keeps the zone's, \"none\" erases the zone's rule on the selected cells only, repeated calls layer per side", {
  thick <- rtf_border(bottom = "thick")
  t0 <- rtftable(.t12_3x3(), border = "none") |> style_zone(last_row = thick)
  r3 <- function(tbl) .t12_rowdefs(.t12_tbl_rtf(tbl), "a3")
  # the zone alone: every cell of the last row has the thick bottom rule
  expect_identical(.t12_bw(r3(t0), "b"), c("th", "th", "th"))
  # "none" erases the zone's rule on the selected cell (row 3, column 2) only
  er <- t0 |> style_body(rows = 3, cols = 2, border = rtf_border(bottom = "none"))
  expect_identical(.t12_bw(r3(er), "b"), c("th", "", "th"))
  # a side it names wins over the zone's (double over thick); the others keep the zone's
  wins <- t0 |> style_body(rows = 3, cols = 1, border = rtf_border(bottom = "double"))
  expect_identical(.t12_bw(r3(wins), "b"), c("db", "th", "th"))
  # a side it leaves NULL keeps the zone's: a top rule added, the zone's bottom stays
  keep <- t0 |> style_body(rows = 3, cols = 3, border = rtf_border(top = "dot"))
  expect_identical(.t12_bw(r3(keep), "b"), c("th", "th", "th"))
  expect_identical(.t12_bw(r3(keep), "t"), c("", "", "dot"))
  # the selected cells only: the other rows are untouched by the erasing
  rows <- function(tbl, key) .t12_rowdefs(.t12_tbl_rtf(tbl), key)
  expect_identical(.t12_bw(rows(er, "a1"), "b"), c("", "", ""))
  expect_identical(.t12_bw(rows(er, "a2"), "b"), c("", "", ""))
  # body crossed with first_row and last_row: the body's inside_h and the last row's bottom
  both <- rtftable(.t12_3x3(), border = "none") |>
    style_zone(body = rtf_border(inside_h = "dot"), last_row = thick) |>
    style_body(rows = 2:3, cols = 1, border = rtf_border(bottom = "none"))
  expect_identical(.t12_bw(rows(both, "a1"), "b"), c("dot", "dot", "dot"))
  expect_identical(.t12_bw(rows(both, "a2"), "b"), c("", "dot", "dot"))
  expect_identical(.t12_bw(rows(both, "a3"), "b"), c("", "th", "th"))
  # repeated calls layer per side: the earlier bottom survives the later top
  lay <- rtftable(.t12_3x3(), border = "none") |>
    style_body(rows = 1, cols = 2, border = rtf_border(bottom = "single")) |>
    style_body(rows = 1, cols = 2, border = rtf_border(top = "double"))
  expect_identical(.t12_bw(rows(lay, "a1"), "b"), c("", "s", ""))
  expect_identical(.t12_bw(rows(lay, "a1"), "t"), c("", "db", ""))
  # ... and a later call naming the same side gives its final value
  lay2 <- lay |> style_body(rows = 1, cols = 2, border = rtf_border(bottom = "dot"))
  expect_identical(.t12_bw(rows(lay2, "a1"), "b"), c("", "dot", ""))
  expect_identical(.t12_bw(rows(lay2, "a1"), "t"), c("", "db", ""))
})

test_that("FT-IX6206 [FR-IX334]: style_body(background = ) overrides the column's fill from style_cols(background = ) on the selected cells; style_cols(header_background = ) fills only the header, so a shaded header does not shade the body", {
  fill <- function(def) vapply(def, function(x) {
    m <- regmatches(x, regexpr("\\\\clcbpat[0-9]+", x))
    if (length(m)) sub("\\clcbpat", "", m, fixed = TRUE) else ""
  }, "", USE.NAMES = FALSE)
  rows <- function(tbl, key) .t12_rowdefs(.t12_tbl_rtf(tbl), key)
  # the header's fill: the header cell of column 2 only (B), no body cell
  h <- rtftable(.t12_3x3()) |> style_cols(cols = 2, header_background = "#D6E4F0")
  expect_identical(nzchar(fill(rows(h, "A\\cell"))), c(FALSE, TRUE, FALSE))
  for (k in c("a1", "a2", "a3")) expect_identical(fill(rows(h, k)), c("", "", ""), info = k)
  # the column's fill: the three body cells of column 3, not its header
  c3 <- rtftable(.t12_3x3()) |> style_cols(cols = 3, background = "#EFEFEF")
  expect_identical(nzchar(fill(rows(c3, "A\\cell"))), c(FALSE, FALSE, FALSE))
  same <- vapply(c("a1", "a2", "a3"), function(k) fill(rows(c3, k))[3L], "")
  expect_true(all(nzchar(same)) && length(unique(same)) == 1L)
  # style_body(background = ) on row 2, column 3: that cell takes the other colour, the other cells keep the column's
  o <- c3 |> style_body(rows = 2, cols = 3, background = "#F8D7DA")
  got <- vapply(c("a1", "a2", "a3"), function(k) fill(rows(o, k))[3L], "")
  expect_true(all(nzchar(got)))
  expect_identical(got[["a1"]], got[["a3"]])
  expect_identical(got[["a1"]], same[["a1"]])           # the column's fill kept
  expect_false(identical(got[["a2"]], got[["a1"]]))     # the selected cell overridden
  expect_identical(nzchar(fill(rows(o, "A\\cell"))), c(FALSE, FALSE, FALSE))   # the header is not shaded
  # both colours are in the colour table
  txt <- .t12_tbl_rtf(o)
  expect_true(.t12_has(txt, "\\red239\\green239\\blue239"))
  expect_true(.t12_has(txt, "\\red248\\green215\\blue218"))
})

test_that("FT-IX6207 [FR-IX301]: the program for the run tokens is, in order, generate_rtfreport(program = ), rtf_document(program = ), options(rtfreporter.program), the program found (source(), Rscript, knitr, RStudio in that order), then program_fallback of the call, then the document's; a program said is not announced, one found or fallen back to is said in a message", {
  fb <- function(...) {
    rtf_document(...) |>
      rtf_section(page = 1, secinfo = list(header = NULL,
        footer = rtf_footer(list(c(l = "P={PROGRAM}"))))) |>
      rtf_tables(as_rtftables(data.frame(A = "a")))
  }
  old <- options(rtfreporter.program = "opt.R")
  on.exit(options(old), add = TRUE)
  # the four finders, each with its own file
  local_mocked_bindings(.program_from_source = function() "found/s.R",
                        .program_from_rscript = function() "found/r.R",
                        .program_from_knitr = function() "found/k.Rmd",
                        .program_from_rstudio = function() "found/e.R")
  # everything set: the call's program wins, nothing is said
  expect_no_message(out <- .t12_render(fb(program = "doc.R", program_fallback = "fb_doc"),
                                       program = "call.R", program_fallback = "fb_call"))
  expect_true(.t12_has(out, "P=call.R"))
  # no call program: the document's, nothing said
  expect_no_message(out <- .t12_render(fb(program = "doc.R", program_fallback = "fb_doc")))
  expect_true(.t12_has(out, "P=doc.R"))
  # no document program: the option, nothing said; a program found is not used while it is set
  expect_no_message(out <- .t12_render(fb(program_fallback = "fb_doc")))
  expect_true(.t12_has(out, "P=opt.R"))
  expect_false(.t12_has(out, "found/"))
  # no option: the program found, said in a message; the order source(), Rscript, knitr, RStudio
  options(rtfreporter.program = NULL)
  expect_message(out <- .t12_render(fb(program_fallback = "fb_doc")), "the file source() is running", fixed = TRUE)
  expect_true(.t12_has(out, "P=found/s.R"))
  local({
    local_mocked_bindings(.program_from_source = function() NULL)
    expect_message(out <- .t12_render(fb()), "the script Rscript runs", fixed = TRUE)
    expect_true(.t12_has(out, "P=found/r.R"))
  })
  local({
    local_mocked_bindings(.program_from_source = function() NULL, .program_from_rscript = function() NULL)
    expect_message(out <- .t12_render(fb()), "the document knitr is knitting", fixed = TRUE)
    expect_true(.t12_has(out, "P=found/k.Rmd"))
  })
  local({
    local_mocked_bindings(.program_from_source = function() NULL, .program_from_rscript = function() NULL,
                          .program_from_knitr = function() NULL)
    expect_message(out <- .t12_render(fb()), "the file open in RStudio's editor", fixed = TRUE)
    expect_true(.t12_has(out, "P=found/e.R"))
  })
  # nothing found: the call's program_fallback, then the document's; said in a message
  local({
    local_mocked_bindings(.find_program = function() NULL)
    expect_message(out <- .t12_render(fb(program_fallback = "doc_fb"), program_fallback = "call_fb.R"),
                   "no file name was found: program_fallback", fixed = TRUE)
    expect_true(.t12_has(out, "P=call_fb.R"))
    expect_false(.t12_has(out, "doc_fb"))
    expect_message(out <- .t12_render(fb(program_fallback = "doc_fb.R")),
                   "no file name was found: program_fallback", fixed = TRUE)
    expect_true(.t12_has(out, "P=doc_fb.R"))
    # a program said: the fallback is not used, nothing is said
    expect_no_message(out <- .t12_render(fb(program = "said.R", program_fallback = "doc_fb.R")))
    expect_true(.t12_has(out, "P=said.R"))
    # no fallback and nothing found: an error
    expect_error(.t12_render(fb()), "no program is known")
  })
})

test_that("FT-IX6208 [FR-IX313]: the list from combine_sections() given to rtf_tables(auto_section = TRUE) makes one section per argument; a section opens where the page name changes, an empty name continues the open section, and a name that comes back after another opens a new one", {
  one <- function(v) as_rtftable(data.frame(a = v, stringsAsFactors = FALSE))
  pages <- function(k, tag = "p") lapply(seq_len(k), function(i) one(paste0(tag, i)))
  n_sect <- function(tabs) {
    doc <- rtf_document() |>
      rtf_section(secinfo = list(header = rtf_header(rows = list(c(l = "Study"))))) |>
      rtf_tables(tabs, auto_section = TRUE)
    txt <- .t12_render(doc)
    lengths(regmatches(txt, gregexpr("\\sectd", txt, fixed = TRUE)))
  }
  # one section per argument: 3 + 2 pages are 2 sections, not 5
  s <- combine_sections(Demographics = pages(3L, "d"), `Adverse Events` = pages(2L, "e"))
  expect_identical(names(s), c("Demographics", "", "", "Adverse Events", ""))
  expect_identical(n_sect(s), 2L)
  # the pages of an argument stay together: the section break falls between d3 and e1
  txt <- .t12_render(rtf_document() |>
    rtf_section(secinfo = list(header = rtf_header(rows = list(c(l = "Study"))))) |>
    rtf_tables(s, auto_section = TRUE))
  at <- function(x) regexpr(x, txt, fixed = TRUE)[[1L]]
  brk <- gregexpr("\\sectd", txt, fixed = TRUE)[[1L]]
  expect_true(at("d1") > 0 && at("d3") > 0 && at("e1") > 0)
  expect_false(any(brk > at("d1") & brk < at("d3")))           # no section opens inside the first argument
  expect_true(any(brk > at("d3") & brk < at("e1")))            # one opens before the second
  # the names of a plain list: a run of equal names is one section
  p4 <- pages(4L)
  names(p4) <- c("A", "A", "B", "B")
  expect_identical(n_sect(p4), 2L)
  # a name that comes back after another opens a new one
  names(p4) <- c("A", "B", "A", "B")
  expect_identical(n_sect(p4), 4L)
  # an empty name continues the open section
  names(p4) <- c("A", "", "B", "")
  expect_identical(n_sect(p4), 2L)
  # ... even between equals
  p3 <- pages(3L)
  names(p3) <- c("A", "", "A")
  expect_identical(n_sect(p3), 1L)
})

test_that("FT-IX6209 [FR-IX343]: listing_col(label = ) and its width: a single string with no breaks is wrapped to the column's width, a character vector or a string with its own breaks is left as written, and with no width a single string stays one line", {
  d <- data.frame(AEDECOD = c("Headache", "Nausea"), stringsAsFactors = FALSE)
  lab <- function(...) {
    spec <- listing_spec(list(listing_col("AEDECOD", ...)))
    attr(build_listing(d, spec), "rtf_listing", exact = TRUE)$cols[[1L]]$label
  }
  text <- "Preferred term of the event"
  # a single string, a width of 16: wrapped to it ("Preferred term" is 14 characters, one more word is 17)
  expect_identical(lab(width = 16, label = text), "Preferred term\nof the event")
  # the same string with no width: one line
  expect_identical(lab(label = text), "Preferred term of the event")
  # a string with its own breaks is a layout: left as written even at a narrower width
  expect_identical(lab(width = 8, label = "Preferred term\nof the event"), "Preferred term\nof the event")
  # a vector is a layout, not re-wrapped: the second line is wider than the column and stays whole
  expect_identical(lab(width = 4, label = c("A", "BBBB CCCC")), "A\nBBBB CCCC")
  expect_identical(lab(width = 15, label = c("Unique", "Subject ID")), "Unique\nSubject ID")
})

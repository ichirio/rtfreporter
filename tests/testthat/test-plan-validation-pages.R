# Validation tests: the pages-side plan verbs -- plan_template(), plan_stub(),
# plan_col_header(), plan_hide(), plan_cell_style(), plan_style(),
# plan_paginate_rows(), plan_paginate_group(), plan_paginate_cols(),
# plan_row_group(), plan_listing(), plan_titles() / plan_footnotes(),
# plan_after().  Where an existing test already checks a leaf it is mapped in
# inst/validation/test_map.csv; these are the leaves none did.

# A finished (wide) table: three groups of two rows, two value columns.
vp_wide <- function() {
  data.frame(
    grp = c("A", "A", "B", "B", "C", "C"),
    label = c("n", "Mean", "n", "Mean", "n", "Mean"),
    X = c("1", "2.5", "3", "4.5", "5", "6.5"),
    Y = c("7", "8.5", "9", "10.5", "11", "12.5"),
    stringsAsFactors = FALSE)
}

vp_first <- function(x) if (inherits(x, "rtftable")) x else x[[1L]]

vp_pages <- function(p) suppressMessages(plan_apply(p, "pages"))

# the direct call the plan stands for
vp_direct <- function(d = vp_wide(), ...) as_rtftables(d, read_meta = FALSE, ...)

# A small cards ARD by arm, with the arm's own tabulation bound in.
vp_ard <- function() {
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  cards::bind_ard(
    cards::ard_tabulate(adsl, variables = TRT),
    cards::ard_stack(adsl, .by = TRT,
      cards::ard_summary(variables = AGE,
        statistic = ~ cards::continuous_summary_fns(c("N", "mean")))))
}

vp_ard_plan <- function() {
  table_plan(suppressMessages(normalize_ard(vp_ard())), cols = "TRT",
             rows = c(group = "variable")) |>
    plan_cells(notes = FALSE) |>
    plan_cells(continuous = c(n = "{N:d}", Mean = "{mean:.1f}"))
}

vp_rtf_lines <- function(pages) {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  suppressMessages(generate_rtfreport(rtf_tables(rtf_document(), pages), f,
                                      overwrite = TRUE))
  readLines(f, warn = FALSE)
}


# ---- plan_template() -------------------------------------------------------

test_that("FT-PL270 [FR-PL353 FR-PL356]: plan_template() guesses cols with a NOTE, names the keys left over, and writes the file", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adsl$SEX <- as.character(adsl$SEX)
  ard <- cards::ard_stack(adsl, .by = c(TRT, SEX),
                          cards::ard_summary(variables = AGE))
  code <- utils::capture.output(invisible(plan_template(ard, pipe = "|>")))
  expect_true(any(grepl("NOTE: `cols` was not given; the first key is used.", code,
                        fixed = TRUE)))
  expect_true(any(grepl("NOTE: keys outside `cols` became row keys: SEX", code,
                        fixed = TRUE)))
  expect_true(any(grepl('cols = "TRT",', code, fixed = TRUE)))
  expect_true(any(grepl('rows = c(SEX = "SEX")', code, fixed = TRUE)))
  # file = : the same lines on disk
  dir <- tempfile("vp-tpl-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  f <- file.path(dir, "plan.R")
  out <- utils::capture.output(code2 <- plan_template(ard, cols = "TRT", pipe = "|>",
                                                      file = f))
  expect_true(file.exists(f))
  expect_identical(readLines(f), code2)
  expect_identical(out[nzchar(out)][1:3], code2[1:3])
})

test_that("FT-PL271 [FR-PL354]: plan_template(hierarchy = ) writes the hierarchy, the overall label and the label role", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adae <- merge(cards::ADAE[, c("USUBJID", "AESOC", "AEDECOD")],
                adsl[, c("USUBJID", "TRT")], by = "USUBJID")
  ard <- cards::ard_stack_hierarchical(
    adae, variables = c(AESOC, AEDECOD), by = TRT, denominator = adsl,
    id = USUBJID, over_variables = TRUE)
  code <- utils::capture.output(invisible(
    plan_template(ard, cols = "TRT", hierarchy = c("AESOC", "AEDECOD"), pipe = "|>")))
  expect_true(any(grepl('hierarchy = c("AESOC", "AEDECOD")', code, fixed = TRUE)))
  expect_true(any(grepl('overall   = "Any event"', code, fixed = TRUE)))
  expect_true(any(grepl("NOTE: an overall sentinel is present", code, fixed = TRUE)))
  expect_true(any(grepl('label = c(label = "AEDECOD")', code, fixed = TRUE)))
  expect_silent(parse(text = code))
})

test_that("FT-PL272 [FR-PL359]: plan_template(form = ) takes plan or widen only", {
  expect_error(plan_template(NULL, form = "spread"), "should be one of")
  expect_error(plan_template(NULL, form = "spread"), "\"plan\", \"widen\"")
})


# ---- plan_stub() -----------------------------------------------------------

test_that("FT-PL273 [FR-PL360 FR-PL362 FR-PL363 FR-PL364 FR-PL365 FR-PL366]: plan_stub() is stub_spec() inside as_rtftables(), or stub_cols() first", {
  d <- vp_wide()
  # inside: as_rtftables(stub = stub_spec())
  a <- vp_pages(table_plan(d) |> plan_style() |>
                  plan_stub(vars = c("grp", "label"), name = "rl", indent = 3L))
  expect_equal(a, vp_direct(d, stub = stub_spec(c("grp", "label"), label = "rl",
                                                indent = 3L)))
  expect_identical(names(vp_first(a)$data)[1L], "rl")
  expect_identical(nrow(vp_first(a)$data), 9L)          # three heading rows added
  expect_identical(vp_first(a)$data$rl[1:2], c("A", paste0(strrep(" ", 3L), "n")))
  # before = TRUE: stub_cols() first, the same table as_rtftables() then sees
  b <- vp_pages(table_plan(d) |> plan_style() |>
                  plan_stub(vars = c("grp", "label"), name = "rl", indent = 3L,
                            before = TRUE))
  expect_equal(b, vp_direct(stub_cols(d, vars = c("grp", "label"), label = "rl",
                                      indent = 3L)))
  expect_identical(vp_first(b)$data$rl, vp_first(a)$data$rl)
  # group_summary: an NA leaf folds onto the heading row
  d2 <- d
  d2$label[d2$label == "n"] <- NA
  g <- vp_pages(table_plan(d2) |> plan_style() |>
                  plan_stub(vars = c("grp", "label"), name = "rl", group_summary = "all"))
  expect_equal(g, vp_direct(d2, stub = stub_spec(c("grp", "label"), label = "rl",
                                                 group_summary = "all")))
  expect_identical(nrow(vp_first(g)$data), 6L)          # no separate heading rows
  expect_identical(vp_first(g)$data$X[1L], "1")         # the n sits on A's row
  n <- vp_pages(table_plan(d2) |> plan_style() |>
                  plan_stub(vars = c("grp", "label"), name = "rl", group_summary = "none"))
  expect_identical(nrow(vp_first(n)$data), 9L)
})

test_that("FT-PL274 [FR-PL361 FR-PL367 FR-PL369]: plan_stub() derives its columns; one column is renamed; alone it stops at the table", {
  d <- vp_wide()
  # no roles: the row key would be `group` (absent), the label column `label`
  s <- vp_pages(table_plan(d) |> plan_style() |> plan_stub(name = "row_label"))
  expect_identical(names(vp_first(s)$data), c("row_label", "grp", "X", "Y"))
  expect_identical(vp_first(s)$data$row_label, d$label)   # nothing folded
  expect_identical(nrow(vp_first(s)$data), 6L)
  # with roles on a long frame the row keys are the names table_plan(rows = ) gave
  long <- data.frame(TRT = rep(c("A", "B"), each = 2), PARAM = c("ALT", "AST"),
                     stat_name = "n", stat = c(1, 2, 3, 4), stringsAsFactors = FALSE)
  p <- table_plan(long, cols = "TRT", rows = c(param = "PARAM"),
                  label = c(row = "stat_name")) |>
    plan_cells("{n:.0f}", notes = FALSE) |> plan_style() |> plan_stub(name = "rl")
  expect_identical(names(vp_first(vp_pages(p))$data), c("rl", "A", "B"))
  # alone, the plan still gives the table
  alone <- suppressMessages(plan_apply(table_plan(d) |> plan_stub(vars = c("grp", "label"))))
  expect_true(is.data.frame(alone))
  expect_identical(names(alone), names(d))
})

test_that("FT-PL275 [FR-PL368]: plan_stub() called twice merges field by field", {
  d <- vp_wide()
  a <- vp_pages(table_plan(d) |> plan_style() |>
                  plan_stub(vars = c("grp", "label"), indent = 3L) |>
                  plan_stub(name = "rl"))
  b <- vp_pages(table_plan(d) |> plan_style() |>
                  plan_stub(vars = c("grp", "label"), name = "rl", indent = 3L))
  expect_identical(a, b)
  c3 <- vp_pages(table_plan(d) |> plan_style() |>
                   plan_stub(vars = c("grp", "label"), indent = 3L) |>
                   plan_stub(indent = 1L))
  expect_identical(vp_first(c3)$data[[1L]][2L], " n")
})

test_that("FT-PL276 [FR-PL370]: plan_stub() with nothing to fold says so", {
  d <- vp_wide()
  names(d)[1:2] <- c("a", "b")
  p <- table_plan(d) |> plan_style() |> plan_stub(name = "row_label")
  expect_error(vp_pages(p), "plan_stub(): nothing to fold.", fixed = TRUE)
  expect_error(vp_pages(p), "Name them with `vars = `", fixed = TRUE)
  expect_error(vp_pages(p), "Columns: a, b, X, Y", fixed = TRUE)
})


# ---- plan_col_header() -----------------------------------------------------

test_that("FT-PL277 [FR-PL386]: plan_col_header(values = <data frame>) fills each page's tokens from its row", {
  d <- vp_wide()
  pg <- vp_pages(table_plan(d) |> plan_paginate_rows(page_by = "grp") |>
                   plan_col_header(rtf_col_header(c("G", "L", "X {k}", "Y {k}")),
                                   values = data.frame(group = c("A", "B", "C"),
                                                       k = c(10, 20, 30))))
  expect_identical(names(pg), c("A", "B", "C"))
  expect_identical(unlist(pg[["A"]]$col_header), c("G", "L", "X 10", "Y 10"))
  expect_identical(unlist(pg[["C"]]$col_header), c("G", "L", "X 30", "Y 30"))
})

test_that("FT-PL278 [FR-PL390]: plan_col_header() makes pages, and a later header replaces the earlier", {
  d <- vp_wide()
  expect_identical(rtfreporter:::.plan_reach(table_plan(d) |> plan_col_header(header_sep = "__")),
                   "pages")
  pg <- vp_pages(table_plan(d) |> plan_col_header(header_sep = "__"))
  expect_s3_class(vp_first(pg), "rtftable")
  two <- vp_pages(table_plan(d) |>
                    plan_col_header(rtf_col_header(c("G", "L", "X", "Y"))) |>
                    plan_col_header(rtf_col_header(c("g", "l", "x", "y"))))
  expect_identical(unlist(vp_first(two)$col_header), c("g", "l", "x", "y"))
  # the other fields are kept
  kept <- plan_apply(table_plan(d) |> plan_col_header(col_header_align = "left") |>
                       plan_col_header(header_sep = "__"), "args")$rtf
  expect_identical(kept$col_header_align, "left")
  expect_identical(kept$header_sep, "__")
})

test_that("FT-PL279 [FR-PL392]: plan_col_header(lines = ) is a list and span = a string", {
  p <- table_plan(vp_wide())
  expect_error(plan_col_header(p, lines = c(a = "x")),
               "plan_col_header(lines = ) is a list, one element a header row", fixed = TRUE)
  expect_error(plan_col_header(p, lines = list(c(a = "x")), span = 1),
               "plan_col_header(span = ) is \"each\", \"one\" or a key's name.", fixed = TRUE)
})


# ---- plan_hide() -----------------------------------------------------------

test_that("FT-PL280 [FR-PL399]: plan_hide() takes several names or one vector, and the column is not printed", {
  d <- vp_wide()
  a <- plan_apply(table_plan(d) |> plan_hide("X", "Y"), "args")$rtf$drop_cols
  b <- plan_apply(table_plan(d) |> plan_hide(c("X", "Y")), "args")$rtf$drop_cols
  expect_identical(a, c("X", "Y"))
  expect_identical(b, a)
  pg <- vp_pages(table_plan(d) |> plan_hide("X"))
  expect_identical(names(vp_first(pg)$data), c("grp", "label", "Y"))
  expect_equal(pg, vp_direct(d, drop_cols = "X"))
})

test_that("FT-PL281 [FR-PL402]: plan_hide() of a column that is not there is named, with the verb", {
  p <- table_plan(vp_wide()) |> plan_hide("nope")
  expect_error(vp_pages(p), "`drop_cols` column 'nope' not found in the table.", fixed = TRUE)
  expect_error(vp_pages(p), "plan_hide(\"nope\")", fixed = TRUE)
})


# ---- plan_cell_style() -----------------------------------------------------

test_that("FT-PL282 [FR-PL405 FR-PL409 FR-PL410]: plan_cell_style() by column name, .values, and a condition over named columns", {
  skip_if_not_installed("cards")
  d <- vp_wide()
  # a formula scoped to one column by name, on a finished table
  one <- vp_first(vp_pages(table_plan(d) |> plan_cell_style(bold = list(X = ~ label == "n"))))
  cs <- one$cell_styles
  expect_identical(cs[[1L]]$bold, c(NA, NA, TRUE, NA))
  expect_identical(cs[[2L]]$bold, c(NA, NA, FALSE, NA))
  # a value where a condition holds, over the columns named; NA elsewhere
  w <- vp_first(vp_pages(table_plan(d) |>
                           plan_cell_style(where = ~ label == "n", cols = "X",
                                           color = "#CC0000", bold = TRUE)))
  expect_identical(w$cell_styles[[1L]]$bold, c(NA, NA, TRUE, NA))
  expect_identical(w$cell_styles[[1L]]$color, c(NA, NA, "#CC0000", NA))
  expect_null(w$cell_styles[[2L]])
  # whole columns by name
  cols <- vp_first(vp_pages(table_plan(d) |> plan_cell_style(cols = c("X", "Y"), italic = TRUE)))
  expect_identical(vapply(cols$col_spec, function(s) isTRUE(s$italic), NA),
                   c(FALSE, FALSE, TRUE, TRUE))
  # .values is every value column of an ARD plan
  v <- vp_first(vp_pages(vp_ard_plan() |> plan_cell_style(cols = ".values", bold = TRUE)))
  expect_identical(names(v$data)[1:2], c("group", "label"))
  expect_identical(vapply(v$col_spec, function(s) isTRUE(s$bold), NA),
                   c(FALSE, FALSE, TRUE, TRUE, TRUE))
})


# ---- plan_style() ----------------------------------------------------------

test_that("FT-PL283 [FR-PL422 FR-PL427 FR-PL428]: plan_style()'s whole-table settings reach as_rtftables() by name", {
  d <- vp_wide()
  expect_identical(rtfreporter:::.plan_reach(table_plan(d) |> plan_style()), "pages")
  expect_s3_class(vp_first(vp_pages(table_plan(d) |> plan_style())), "rtftable")
  args <- plan_apply(table_plan(d) |> plan_style(
    align_count_pct = TRUE, font = "Arial", font_size_half_points = 18,
    row_height_twips = 300, row_height_exact = TRUE, header_row_height_twips = 400,
    blank_row_height_twips = 100, cell_padding_left_twips = 50,
    cell_padding_right_twips = 40, cell_valign = "center", table_align = "left",
    markup = TRUE, blank_row_normalize = TRUE), "args")$rtf
  expect_identical(args[c("align_count_pct", "font", "font_size_half_points",
                          "row_height_twips", "row_height_exact",
                          "header_row_height_twips", "blank_row_height_twips",
                          "cell_padding_left_twips", "cell_padding_right_twips",
                          "cell_valign", "table_align", "markup", "blank_row_normalize")],
                   list(align_count_pct = TRUE, font = "Arial", font_size_half_points = 18,
                        row_height_twips = 300, row_height_exact = TRUE,
                        header_row_height_twips = 400, blank_row_height_twips = 100,
                        cell_padding_left_twips = 50, cell_padding_right_twips = 40,
                        cell_valign = "center", table_align = "left", markup = TRUE,
                        blank_row_normalize = TRUE))
  expect_equal(
    vp_pages(table_plan(d) |> plan_style(font_size_half_points = 18, row_height_twips = 300,
                                         cell_padding_left_twips = 50, font = "Arial")),
    vp_direct(d, font_size_half_points = 18, row_height_twips = 300,
              cell_padding_left_twips = 50, font = "Arial"))
  # twice: later wins per field, the rest kept
  two <- plan_apply(table_plan(d) |> plan_style(border = "tfl", font = "Arial") |>
                      plan_style(font = "Courier New", align_count_pct = TRUE), "args")$rtf
  expect_identical(two[c("border", "font", "align_count_pct")],
                   list(border = "tfl", font = "Courier New", align_count_pct = TRUE))
})


# ---- plan_paginate_rows() --------------------------------------------------

test_that("FT-PL284 [FR-PL433 FR-PL434 FR-PL435 FR-PL436 FR-IX262]: every split, with its own arguments, is the direct call's", {
  d <- vp_wide()
  same <- function(p, ...) expect_equal(vp_pages(p), vp_direct(d, ...))
  same(table_plan(d) |> plan_paginate_rows(max_rows = 4, split = "group_safe"),
       max_rows = 4, split = "group_safe")
  same(table_plan(d) |> plan_paginate_rows(max_rows = 4, split = "group_force",
                                           min_group_rows = 2, cont_label = "(cont)"),
       max_rows = 4, split = "group_force", min_group_rows = 2, cont_label = "(cont)")
  same(table_plan(d) |> plan_paginate_rows(split = "rows", break_before = c(3, 5)),
       split = "rows", split_rows = c(3, 5))
  same(table_plan(d) |> plan_paginate_rows(split = "none"), split = "none")
  # what each does
  safe <- vp_pages(table_plan(d) |> plan_paginate_rows(max_rows = 4, split = "group_safe"))
  expect_identical(lapply(safe, function(z) z$data$grp), list(c("A", "A", "B", "B"), c("C", "C")))
  rows <- vp_pages(table_plan(d) |> plan_paginate_rows(split = "rows", break_before = c(3, 5)))
  expect_identical(vapply(rows, function(z) nrow(z$data), 0L), c(2L, 2L, 2L))
  none <- vp_pages(table_plan(d) |> plan_paginate_rows(split = "none"))
  expect_length(none, 1L)
  # a group longer than the budget is cut, the continuation labelled
  d3 <- rbind(d[1:2, ], d[1:2, ], d[3:6, ])
  d3$label[3:4] <- c("SD", "Median")
  force <- vp_pages(table_plan(d3) |> plan_paginate_rows(max_rows = 3, split = "group_force",
                                                         cont_label = "(cont)"))
  expect_equal(force, vp_direct(d3, max_rows = 3, split = "group_force", cont_label = "(cont)"))
  expect_gt(length(force), 1L)
  expect_true(any(grepl("(cont)", unlist(lapply(force, function(z) z$data)), fixed = TRUE)))
})

test_that("FT-PL285 [FR-PL438 FR-PL439 FR-PL441]: heading = names the BY pages, {n} the data does not state is NA with one warning", {
  d <- vp_wide()
  expect_warning(
    pg <- vp_pages(table_plan(d) |>
                     plan_paginate_rows(page_by = "grp", heading = "Group {value} (N={n})",
                                        heading_at = "title")),
    "states no population for 'A', 'B', 'C'")
  expect_identical(names(pg), c("Group A (N=NA)", "Group B (N=NA)", "Group C (N=NA)"))
  expect_true(all(vapply(pg, attr, "", "rtf_heading_at") == "title"))
  plain <- vp_pages(table_plan(d) |> plan_paginate_rows(page_by = "grp", heading = "Group {value}"))
  expect_identical(names(plain), c("Group A", "Group B", "Group C"))
  expect_null(attr(plain[[1L]], "rtf_heading_at"))
})

test_that("FT-PL286 [FR-PL444 FR-PL445 FR-PL446 FR-PL447 FR-IX262]: plan_paginate_rows() refuses a bad heading, heading_at, a budget the split ignores, an unknown page_by", {
  p <- table_plan(vp_wide())
  expect_error(plan_paginate_rows(p, page_by = "grp", heading = c("a", "b")),
               "plan_paginate_rows(heading = ) is one text with `{value}` and `{n}`", fixed = TRUE)
  expect_error(plan_paginate_rows(p, page_by = "grp", heading = ""), "is one text")
  expect_error(plan_paginate_rows(p, page_by = "grp", heading_at = "header"),
               'plan_paginate_rows(heading_at = ) is one of "section", "title", "none".', fixed = TRUE)
  expect_error(plan_apply(plan_paginate_rows(p, max_rows = 4, split = "rows", break_before = 3), "args"),
               "plan_paginate_rows(max_rows = 4) has no effect with split = 'rows'.", fixed = TRUE)
  expect_error(plan_apply(plan_paginate_rows(p, max_rows = 4, split = "none"), "args"),
               "has no effect with split = 'none'", fixed = TRUE)
  expect_error(plan_apply(plan_paginate_rows(p, max_rows = 4, split = "none"), "args"),
               "Drop `max_rows`", fixed = TRUE)
  expect_error(vp_pages(plan_paginate_rows(p, page_by = "nope")),
               "`page_by` column 'nope' not found in the table.", fixed = TRUE)
  expect_error(vp_pages(plan_paginate_rows(p, page_by = "nope")),
               "plan_paginate_rows(page_by = \"nope\")", fixed = TRUE)
})


# ---- plan_paginate_group() -------------------------------------------------

test_that("FT-PL287 [FR-PL451 FR-PL452 FR-PL453 FR-PL454 FR-PL456]: plan_paginate_group() on a finished table: the default key, keep, heading, heading_at, two calls", {
  d <- vp_wide()
  names(d)[1L] <- "group"            # the default row key's name
  a <- plan_apply(table_plan(d) |> plan_paginate_group(), "args")$rtf
  expect_identical(a$page_by, "group")
  expect_null(a$split)
  expect_null(a$drop_cols)
  pg <- vp_pages(table_plan(d) |> plan_paginate_group())
  expect_identical(names(pg), c("A", "B", "C"))
  expect_identical(names(pg[[1L]]$data), c("group", "label", "X", "Y"))   # keep = TRUE
  hid <- vp_pages(table_plan(d) |> plan_paginate_group(keep = FALSE))
  expect_identical(names(hid), c("A", "B", "C"))
  expect_identical(names(hid[[1L]]$data), c("label", "X", "Y"))
  expect_warning(
    h <- vp_pages(table_plan(d) |> plan_paginate_group(col = "group", heading = "G: {value} (N={n})")),
    "plan_paginate_group(heading = ): the ARD states no population", fixed = TRUE)
  expect_identical(names(h), c("G: A (N=NA)", "G: B (N=NA)", "G: C (N=NA)"))
  at <- vp_pages(table_plan(d) |> plan_paginate_group(col = "group", heading_at = "section"))
  expect_true(all(vapply(at, attr, "", "rtf_heading_at") == "section"))
  expect_null(attr(pg[[1L]], "rtf_heading_at"))
  # two calls merge: the column from the first, keep from the second
  two <- plan_apply(table_plan(d) |> plan_paginate_group(col = "group") |>
                      plan_paginate_group(keep = FALSE), "args")$rtf
  expect_identical(two$page_by, "group")
  expect_identical(two$drop_cols, "group")
})


# ---- plan_paginate_cols() --------------------------------------------------

test_that("FT-PL288 [FR-PL461 FR-PL462 FR-PL463 FR-PL465 FR-PL466 FR-PL467]: plan_paginate_cols() is paginate_cols() with its own arguments", {
  d <- vp_wide()
  base <- vp_direct(d)
  # at, keep, fit = FALSE
  a <- vp_pages(table_plan(d) |> plan_paginate_cols(at = 4L, keep = 1:2, fit = FALSE))
  expect_equal(a, paginate_cols(base, at = 4L, carry = 1:2, width = "keep"))
  expect_length(a, 2L)
  expect_identical(names(a[[1L]]$data), c("grp", "label", "X"))
  expect_identical(names(a[[2L]]$data), c("grp", "label", "Y"))
  # at by name; fit = TRUE is "fill"
  expect_equal(vp_pages(table_plan(d) |> plan_paginate_cols(at = "Y", keep = c("grp", "label"), fit = TRUE)),
               paginate_cols(base, at = "Y", carry = c("grp", "label"), width = "fill"))
  # cut_by: a list of blocks, or a separator (with col_header = "names")
  expect_equal(vp_pages(table_plan(d) |> plan_paginate_cols(cut_by = list(3L, 4L), keep = 1:2)),
               paginate_cols(base, cols = list(3L, 4L), carry = 1:2))
  d2 <- d
  names(d2)[3:4] <- c("A__X", "B__Y")
  expect_equal(vp_pages(table_plan(d2) |> plan_paginate_cols(cut_by = "__", keep = 1:2,
                                                              col_header = "names")),
               paginate_cols(vp_direct(d2), by = "__", carry = 1:2, col_header = "names"))
  # order: the axes nest as paginate_cols(page_order = )
  rows <- vp_direct(d, max_rows = 4, split = "group_force")
  down <- vp_pages(table_plan(d) |> plan_paginate_rows(max_rows = 4, split = "group_force") |>
                     plan_paginate_cols(at = 4L, keep = 1:2, order = "down"))
  expect_equal(down, paginate_cols(rows, at = 4L, carry = 1:2, page_order = "down"))
  across <- vp_pages(table_plan(d) |> plan_paginate_rows(max_rows = 4, split = "group_force") |>
                       plan_paginate_cols(at = 4L, keep = 1:2, order = "across"))
  expect_equal(across, paginate_cols(rows, at = 4L, carry = 1:2, page_order = "across"))
  expect_identical(vapply(down, function(z) names(z$data)[3L], ""), c("X", "Y", "X", "Y"))
  expect_identical(vapply(across, function(z) names(z$data)[3L], ""), c("X", "X", "Y", "Y"))
  # allow_span_break passes
  expect_equal(vp_pages(table_plan(d) |> plan_paginate_cols(at = 4L, keep = 1:2, allow_span_break = FALSE)),
               paginate_cols(base, at = 4L, carry = 1:2, allow_span_break = FALSE))
})

test_that("FT-PL289 [FR-PL468 FR-IX254 FR-PL471]: the column cut runs after plan_after(), and every = must be positive", {
  d <- vp_wide()
  pg <- vp_pages(table_plan(d) |>
                   plan_after(function(x) set_decimal_split(x, cols = 4L)) |>
                   plan_paginate_cols(at = 4L, keep = 1:2))
  expect_length(pg, 2L)
  expect_null(pg[[1L]]$decimal_split)
  expect_identical(names(pg[[2L]]$data)[3L], "Y")
  expect_identical(pg[[2L]]$decimal_split$cols, 3L)      # column 4 of the table, 3 of the block
  expect_error(vp_pages(table_plan(d) |> plan_paginate_cols(every = 0L)),
               "`every` is a positive number of columns.", fixed = TRUE)
})


# ---- plan_row_group() ------------------------------------------------------

test_that("FT-PL290 [FR-PL473 FR-PL474]: plan_row_group(mode, collapse) are as_rtftables(group_by, collapse_repeats)", {
  d <- vp_wide()
  expect_equal(
    vp_pages(table_plan(d) |> plan_row_group(mode = "value", collapse = 1L) |>
               plan_paginate_rows(max_rows = 4, split = "group_safe")),
    vp_direct(d, group_by = "value", collapse_repeats = 1L, max_rows = 4, split = "group_safe"))
  for (m in c("indent", "filled", "auto")) {
    expect_identical(plan_apply(table_plan(d) |> plan_row_group(mode = m), "args")$rtf$group_by, m)
  }
  by_name <- vp_pages(table_plan(d) |> plan_row_group(collapse = "grp"))
  expect_equal(by_name, vp_direct(d, collapse_repeats = 1L))
  expect_identical(vp_first(by_name)$data$grp, c("A", NA, "B", NA, "C", NA))
})


# ---- plan_listing() --------------------------------------------------------

test_that("FT-PL291 [FR-PL483 FR-PL484]: plan_listing()'s layout arguments are listing_spec()'s, and zero records make an empty page", {
  d <- data.frame(ID = sprintf("S%02d", 1:5), ARM = c("A", "B", "A", "B", "A"),
                  TXT = paste("word", 1:5, "and more words here"), stringsAsFactors = FALSE)
  cols <- list(listing_col("ID", width = 6), listing_col("ARM", width = 6),
               listing_col("TXT", width = 12))
  same <- function(...) {
    expect_equal(vp_pages(table_plan(d) |> plan_listing(cols[[1L]], cols[[2L]], cols[[3L]], ...)),
                 as_rtftables(d, listing = listing_spec(cols, ...)))
  }
  same(type = "multiline")
  same(sep = " / ")
  same(spacer = TRUE, spacer_rel_width = 0.5)
  same(layout = "flow")
  wr <- function(text, width, sep, layout) toupper(text)
  same(wrap = wr)
  up <- vp_first(vp_pages(table_plan(d) |> plan_listing(cols[[1L]], cols[[3L]], wrap = wr)))
  expect_true(any(grepl("WORD 1", unlist(up$data), fixed = TRUE)))
  empty <- vp_pages(table_plan(d[0, , drop = FALSE]) |> plan_listing(cols[[1L]]))
  expect_s3_class(vp_first(empty), "rtftable")
  expect_identical(nrow(vp_first(empty)$data), 0L)
})


# ---- plan_titles() / plan_footnotes() -------------------------------------

test_that("FT-PL292 [FR-PL486 FR-PL488 FR-PL489]: the blocks sit on a single page too, reach the RTF, keep Japanese, and the later call wins", {
  d <- vp_wide()
  pg <- vp_pages(table_plan(d) |> plan_titles("T1", "表 2") |> plan_footnotes("F1"))
  expect_length(pg, 1L)
  expect_identical(attr(pg[[1L]], "rtf_titles"), c("T1", "表 2"))
  expect_identical(attr(pg[[1L]], "rtf_footnotes"), "F1")
  lines <- vp_rtf_lines(pg)
  expect_true(any(grepl("\\b T1\\b0", lines, fixed = TRUE)))
  expect_true(any(grepl("\\u34920? 2", lines, fixed = TRUE)))      # U+8868 as RTF writes it
  expect_true(any(grepl("F1\\cell", lines, fixed = TRUE)))
  two <- vp_pages(table_plan(d) |> plan_titles("T1") |> plan_titles("T2"))
  expect_identical(attr(two[[1L]], "rtf_titles"), "T2")
  twof <- vp_pages(table_plan(d) |> plan_footnotes("F1") |> plan_footnotes(pages = list("F2")))
  expect_identical(attr(twof[[1L]], "rtf_footnotes"), "F2")
})


# ---- plan_after() ----------------------------------------------------------

test_that("FT-PL293 [FR-PL494]: two plan_after() calls both run, in order", {
  seen <- character(0)
  vp_pages(table_plan(vp_wide()) |>
             plan_after(function(x) { seen <<- c(seen, "one"); x }) |>
             plan_after(function(x) { seen <<- c(seen, "two"); x }))
  expect_identical(seen, c("one", "two"))
  expect_identical(rtfreporter:::.plan_reach(table_plan(vp_wide()) |> plan_after(identity)), "pages")
})


# ---- every verb: not a plan --------------------------------------------------

test_that("FT-PL294 [FR-PL372 FR-PL398 FR-PL403 FR-PL421 FR-PL431 FR-PL448 FR-PL460 FR-PL472 FR-PL479 FR-PL485 FR-PL492 FR-PL496]: the pages-side verbs refuse anything that is not a plan", {
  msg <- "Expected a table_plan; pipe from table_plan(ard)."
  not <- data.frame(a = 1)
  expect_error(plan_stub(not, vars = "a"), msg, fixed = TRUE)
  expect_error(plan_col_header(not, header_sep = "_"), msg, fixed = TRUE)
  expect_error(plan_hide(not, "a"), msg, fixed = TRUE)
  expect_error(plan_cell_style(not, bold = TRUE), msg, fixed = TRUE)
  expect_error(plan_style(not, border = "tfl"), msg, fixed = TRUE)
  expect_error(plan_paginate_rows(not, max_rows = 10), msg, fixed = TRUE)
  expect_error(plan_paginate_group(not, col = "a"), msg, fixed = TRUE)
  expect_error(plan_paginate_cols(not, at = 2L), msg, fixed = TRUE)
  expect_error(plan_row_group(not, mode = "value"), msg, fixed = TRUE)
  expect_error(plan_listing(not, listing_col("a")), msg, fixed = TRUE)
  expect_error(plan_titles(not, "t"), msg, fixed = TRUE)
  expect_error(plan_footnotes(not, "f"), msg, fixed = TRUE)
  expect_error(plan_after(not, identity), msg, fixed = TRUE)
})

# plan_append(): another plan's rows under a table's (issue #737).  The
# three PK tables are compared byte for byte with the references built by
# hand (fixtures/plan-append/make_reference.R); the rest are unit tests on
# small frames.

pl_fx <- function(...) test_path("fixtures", "plan-append", ...)
pl_skip <- function() {
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
}

# the document the references use: no run date, the program as given
pl_doc <- function(p, titles, program) {
  rtf_document(tokens = list(STUDY = "DEMO-02"), program = program) |>
    rtf_section(
      header = rtf_header(list(c(l = "Protocol: {STUDY} (synthetic data)",
                                 r = "Page {PAGE} of {TOTAL_PAGES}"))),
      footer = rtf_footer(list(c(l = "Synthetic data made for this sample.",
                                 r = "Program: {PROGRAM}")))) |>
    rtf_tables(p, titles = titles)
}
pl_same_bytes <- function(doc, ref) {
  out <- tempfile(fileext = ".rtf")
  on.exit(unlink(out))
  generate_rtfreport(doc, out, overwrite = TRUE)
  expect_identical(readLines(out, warn = FALSE), readLines(ref, warn = FALSE))
}
pl_stats <- c(N = "{N:.0f}", Mean = "{mean:.4s}", SD = "{sd:.5s}", SE = "{se:.5s}",
              "CV(%)" = "{cv:.1f}", Median = "{median:.4s}", Min = "{min:.3s}",
              Max = "{max:.3s}")
# (a time point with no value -- every sample BLQ after Cmax -- leaves the
# statistics empty, N 0)
pl_fns <- ~ list(N = length, mean = function(x) if (length(x)) mean(x) else NA_real_,
                 sd = stats::sd, median = stats::median,
                 min = function(x) if (length(x)) min(x) else NA_real_,
                 max = function(x) if (length(x)) max(x) else NA_real_,
                 se = function(x) stats::sd(x) / sqrt(length(x)),
                 cv = function(x) if (!length(x) || mean(x) == 0) NA_real_
                                  else 100 * stats::sd(x) / mean(x))
pl_by_group <- function(d, cols, levels) {
  d$ANALYTE <- factor(d$ANALYTE, levels = c("Parent", "Metabolite M1"))
  d$RENALGR <- factor(d$RENALGR, levels = levels)
  d[[cols]] <- factor(d[[cols]], levels = unique(d[[cols]]))
  d
}

test_that("FT-PL901 [FR-PL901]: a listing above its summary, the time points across two column blocks: the same bytes as by hand", {
  pl_skip()
  adpc <- read.csv(pl_fx("adpc.csv"))
  adpc <- adpc[adpc$RENALGR %in% c("Normal", "Moderate"), ]
  times <- c("Pre-Dose", paste(c(0.25, 0.5, 1, 1.5, 2, 3, 4, 6, 8, 12, 24, 36, 48), "hr"))
  ard <- cards::ard_summary(adpc[!is.na(adpc$AVAL), ], by = c(ANALYTE, RENALGR, ATPT),
                            variables = AVAL, statistic = pl_fns)
  rec <- adpc
  rec$AVAL[rec$BLQFL == "Y"] <- NA                      # a BLQ sample prints "BLQ"
  listing <- rec |>
    table_plan(cols = "ATPT", rows = c(subject = ~ "{USUBJID}/{AGE}/{SEX}"), value = "AVAL") |>
    plan_cells("{value:.3s}", na = "BLQ")
  summary <- normalize_ard(ard) |>
    table_plan(cols = "ATPT", rows = c(block = ~ "Summary Statistics")) |>
    plan_cells(continuous = pl_stats)
  p <- listing |>
    plan_append(summary) |>
    plan_levels(ATPT = times) |>
    plan_blanks(first = TRUE, last = TRUE, where = "between_parts") |>
    plan_col_header(rtf_col_header(
      list(col_cell(1, ""), col_cell(c(2, length(times) + 1L), "Time (hr) Relative to Dose")),
      c("Subject ID/Age (years)/Gender", "{col}"))) |>
    plan_columns(widths = c(label = 2.4, .values = 1.2), decimal = ".values") |>
    plan_paginate_rows(page_by = c("ANALYTE", "RENALGR")) |>
    plan_paginate_cols(at = "4 hr", order = "down") |>
    plan_style(border = "tfl") |>
    plan_after(function(x) lapply(x, style_zone, last_row = rtf_border(bottom = TRUE)))
  # the stack: [page keys][label][values], the parts in order, a group's
  # rows together
  tb <- plan_apply(p, "table")
  expect_identical(names(tb)[1:3], c("ANALYTE", "RENALGR", "label"))
  expect_identical(unique(tb$.part), c("part1", "part2"))
  expect_true(all(times %in% names(tb)))
  titles <- list(list("Table 14.4.3", "Drug X Plasma Concentrations (ng/mL) by Subject",
                      "PK Population",
                      list(text = "Analyte: {ANALYTE}", align = "left"),
                      list(text = "Renal Function Group: {RENALGR}", align = "left")))
  pl_same_bytes(pl_doc(p, titles, "plan-append/conc_cols_time.R"), pl_fx("ref_conc_cols_time.rtf"))
  # the whole table's verbs before plan_append() or after: the same bytes
  p2 <- listing |>
    plan_levels(ATPT = times) |>
    plan_blanks(first = TRUE, last = TRUE, where = "between_parts") |>
    plan_col_header(rtf_col_header(
      list(col_cell(1, ""), col_cell(c(2, length(times) + 1L), "Time (hr) Relative to Dose")),
      c("Subject ID/Age (years)/Gender", "{col}"))) |>
    plan_columns(widths = c(label = 2.4, .values = 1.2), decimal = ".values") |>
    plan_paginate_rows(page_by = c("ANALYTE", "RENALGR")) |>
    plan_paginate_cols(at = "4 hr", order = "down") |>
    plan_style(border = "tfl") |>
    plan_after(function(x) lapply(x, style_zone, last_row = rtf_border(bottom = TRUE))) |>
    plan_append(summary)
  pl_same_bytes(pl_doc(p2, titles, "plan-append/conc_cols_time.R"), pl_fx("ref_conc_cols_time.rtf"))
  # ... and the later of two wins, as always
  p3 <- p2 |> plan_paginate_cols(at = "2 hr", order = "down") |> plan_paginate_cols(at = "4 hr", order = "down")
  pl_same_bytes(pl_doc(p3, titles, "plan-append/conc_cols_time.R"), pl_fx("ref_conc_cols_time.rtf"))
})

test_that("FT-PL902 [FR-PL901]: the PK parameters across, a page an analyte and group: the same bytes as by hand", {
  pl_skip()
  adpp <- read.csv(pl_fx("adpp.csv"))
  params <- c("C_{max} (ng/mL)", "T_{max} (hr)", "AUC_{last} (ng·hr/mL)",
              "AUC_{inf} (ng·hr/mL)", "t1/2 (hr)")
  ard <- cards::ard_summary(adpp, by = c(ANALYTE, RENALGR, PARAM), variables = AVAL,
                            statistic = pl_fns)
  listing <- adpp |>
    table_plan(cols = "PARAM", rows = c(subject = ~ "{USUBJID}/{AGE}/{SEX}"), value = "AVAL") |>
    plan_cells("{value:.3s}")
  summary <- normalize_ard(ard) |>
    table_plan(cols = "PARAM", rows = c(block = ~ "Summary Statistics")) |>
    plan_cells(continuous = pl_stats)
  p <- listing |>
    plan_append(summary) |>
    plan_levels(PARAM = params, ANALYTE = c("Parent", "Metabolite M1"),
                RENALGR = c("Normal", "Mild", "Moderate")) |>
    plan_blanks(first = TRUE, last = TRUE, where = "between_parts") |>
    plan_col_header(rtf_col_header(
      list(col_cell(1, ""), col_cell(c(2, length(params) + 1L), "Parameter")),
      c("Subject ID/Age (years)/Gender", "{col}"))) |>
    plan_columns(widths = c(label = 2.4, .values = 1.2), decimal = ".values") |>
    plan_paginate_rows(page_by = c("ANALYTE", "RENALGR")) |>
    plan_style(border = "tfl") |>
    plan_after(function(x) lapply(x, style_zone, last_row = rtf_border(bottom = TRUE)))
  titles <- list(list("Table 14.4.1", "Drug X Pharmacokinetic Parameters by Subject",
                      "PK Population",
                      list(text = "Analyte: {ANALYTE}", align = "left"),
                      list(text = "Renal Function Group: {RENALGR}", align = "left")))
  pl_same_bytes(pl_doc(p, titles, "plan-append/param_by_group.R"), pl_fx("ref_param_by_group.rtf"))
})

test_that("FT-PL903 [FR-PL901]: the analytes across, a page a group and time point: the same bytes as by hand", {
  pl_skip()
  adpc <- read.csv(pl_fx("adpc.csv"))
  adpc <- adpc[adpc$ATPT %in% c("1 hr", "12 hr"), ]
  ard <- cards::ard_summary(adpc[!is.na(adpc$AVAL), ], by = c(RENALGR, ATPT, ANALYTE),
                            variables = AVAL, statistic = pl_fns)
  rec <- adpc
  rec$AVAL[rec$BLQFL == "Y"] <- NA
  listing <- rec |>
    table_plan(cols = "ANALYTE", rows = c(subject = ~ "{USUBJID}/{AGE}/{SEX}"), value = "AVAL") |>
    plan_cells("{value:.3s}", na = "BLQ")
  summary <- normalize_ard(ard) |>
    table_plan(cols = "ANALYTE", rows = c(block = ~ "Summary Statistics")) |>
    plan_cells(continuous = pl_stats)
  p <- listing |>
    plan_append(summary) |>
    plan_levels(ANALYTE = c("Parent", "Metabolite M1"), ATPT = c("1 hr", "12 hr"),
                RENALGR = c("Normal", "Mild", "Moderate")) |>
    plan_blanks(first = TRUE, last = TRUE, where = "between_parts") |>
    plan_col_header(rtf_col_header(
      list(col_cell(1, ""), col_cell(c(2, 3), "Analyte")),
      c("Subject ID/Age (years)/Gender", "{col}"))) |>
    plan_columns(widths = c(label = 2.4, .values = 1.2), decimal = ".values") |>
    plan_paginate_rows(page_by = c("RENALGR", "ATPT")) |>
    plan_style(border = "tfl") |>
    plan_after(function(x) lapply(x, style_zone, last_row = rtf_border(bottom = TRUE)))
  titles <- list(list("Table 14.4.2", "Drug X Plasma Concentrations (ng/mL) by Subject",
                      "PK Population",
                      list(text = "Time: {ATPT}", align = "left"),
                      list(text = "Renal Function Group: {RENALGR}", align = "left")))
  pl_same_bytes(pl_doc(p, titles, "plan-append/conc_cols_analyte.R"), pl_fx("ref_conc_cols_analyte.rtf"))
})

# ---- unit tests on small frames ------------------------------------------------

pl_rec <- function() data.frame(
  ID = rep(c("A", "B"), each = 3), GRP = rep(c("x", "y"), each = 3),
  T = rep(c("t1", "t2", "t3"), 2), V = c(1.234, 2.5, NA, 3, 4.25, 5), stringsAsFactors = FALSE)
pl_long <- function() data.frame(
  GRP = rep(c("x", "y"), each = 3), T = rep(c("t1", "t2", "t3"), 2),
  variable = "V", stat_name = "mean", stat = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5),
  .label = NA_character_, stringsAsFactors = FALSE)

test_that("FT-PL904 [FR-PL902]: a records plan (value = ): one row a key, the values across, one template", {
  d <- pl_rec()
  p <- table_plan(d, cols = "T", rows = "ID", value = "V") |> plan_cells("{value:.2f}", na = "-")
  t <- as.data.frame(plan_apply(p, "table"))
  expect_identical(names(t), c("ID", "t1", "t2", "t3"))
  expect_identical(t$ID, c("A", "B"))
  expect_identical(t$t3[1], "-")
  expect_identical(t$t1, c("1.23", "3.00"))
  # a template over the record as the row's text
  q <- table_plan(d, cols = "T", rows = c(subject = ~ "{ID} ({GRP})"), value = "V") |>
    plan_cells("{value:.1f}")
  expect_identical(as.data.frame(plan_apply(q, "table"))$subject, c("A (x)", "B (y)"))
  # a records plan needs its rows; no `stat`; the column must be there
  expect_error(table_plan(d, cols = "T", value = "V"), "needs `rows`")
  expect_error(table_plan(d, cols = "T", rows = "ID", value = "nope"), "no such column")
  expect_error(table_plan(d, cols = "T", rows = "ID", value = "V", stat = c(name = "x")), "takes no `stat`")
})

test_that("FT-PL905 [FR-PL903]: a part's rows under the parent's; a column a part lacks prints its na; the part's own na", {
  rec <- pl_rec()
  rec$GRP <- NULL
  parent <- table_plan(rec, cols = "T", rows = "ID", value = "V") |> plan_cells("{value:.1f}", na = "BLQ")
  lng <- pl_long()
  lng <- lng[lng$T != "t3", ]
  child <- table_plan(lng, cols = "T", rows = c(block = ~ "Summary")) |>
    plan_cells(V = c(Mean = "{mean:.2f}"))
  t <- as.data.frame(plan_apply(parent |> plan_append(child), "table"))
  expect_identical(names(t), c("label", "t1", "t2", "t3", ".part"))
  expect_identical(t$label, c("A", "B", "Summary", paste0(strrep(" ", 4L), "Mean")))
  expect_identical(t$.part, c("part1", "part1", "part2", "part2"))
  # the parent's na, not the child's: t3 is empty on the child's rows
  expect_identical(t$t3, c("BLQ", "5.0", "", ""))
  expect_identical(t$t1[4], "1.50")
  # the child inherits the parent's digits by key, and its own win
  child2 <- table_plan(lng, cols = "T", rows = c(block = ~ "Summary")) |>
    plan_cells(V = c(Mean = "{mean}"))
  t2 <- as.data.frame(plan_apply(parent |> plan_digits(3) |> plan_append(child2), "table"))
  expect_identical(t2$t1[4], "1.500")
  t3 <- as.data.frame(plan_apply(parent |> plan_digits(3) |> plan_append(child2 |> plan_digits(1)), "table"))
  expect_identical(t3$t1[4], "1.5")
  # the parts in the order declared; a name of their own
  t4 <- as.data.frame(plan_apply(parent |> plan_append(child, name = "stats") |>
                                   plan_append(child2, name = "again"), "table"))
  expect_identical(unique(t4$.part), c("part1", "stats", "again"))
  expect_error(parent |> plan_append(child, name = "part1"), "already there")
})

test_that("FT-PL906 [FR-PL904]: page groups are matched by value; a group a part lacks stops, or is skipped", {
  rec <- pl_rec()
  parent <- table_plan(rec, cols = "T", rows = "ID", value = "V") |> plan_cells("{value:.1f}") |>
    plan_paginate_rows(page_by = "GRP")
  child <- table_plan(pl_long(), cols = "T", rows = c(block = ~ "Summary")) |>
    plan_cells(V = c(Mean = "{mean:.1f}"))
  t <- as.data.frame(plan_apply(parent |> plan_append(child), "table"))
  expect_identical(t$GRP, c("x", "x", "x", "y", "y", "y"))
  expect_identical(t$.part, rep(c("part1", "part2", "part2"), 2L))
  pg <- plan_apply(parent |> plan_append(child), "pages")
  expect_length(pg, 2L)
  expect_identical(names(pg[[1L]]$data), c("label", "t1", "t2", "t3"))
  expect_identical(pg[[2L]]$data$label, c("B", "Summary", paste0(strrep(" ", 4L), "Mean")))
  # a group the part has not: an error, or skipped
  half <- pl_long()[pl_long()$GRP == "x", ]
  child_x <- table_plan(half, cols = "T", rows = c(block = ~ "Summary")) |>
    plan_cells(V = c(Mean = "{mean:.1f}"))
  expect_error(plan_apply(parent |> plan_append(child_x), "table"), "no rows for .GRP. .y.")
  t2 <- as.data.frame(plan_apply(parent |> plan_append(child_x, absent = "skip"), "table"))
  expect_identical(t2$.part, c("part1", "part2", "part2", "part1"))
  # a group only the part has makes no rows
  more <- rbind(pl_long(), data.frame(GRP = "z", T = "t1", variable = "V", stat_name = "mean",
                                      stat = 9, .label = NA_character_))
  child_z <- table_plan(more, cols = "T", rows = c(block = ~ "Summary")) |>
    plan_cells(V = c(Mean = "{mean:.1f}"))
  expect_false("z" %in% as.data.frame(plan_apply(parent |> plan_append(child_z), "table"))$GRP)
  # the part's data must carry the page key
  nogrp <- pl_long()
  nogrp$GRP <- NULL
  child_n <- table_plan(nogrp, cols = "T", rows = c(block = ~ "Summary")) |>
    plan_cells(V = c(Mean = "{mean:.1f}"))
  expect_error(plan_apply(parent |> plan_append(child_n), "table"), "has no .GRP., the page_by column")
})

test_that("FT-PL907 [FR-PL905]: blank rows between the parts, and a part's own first / last", {
  rec <- pl_rec()
  parent <- table_plan(rec, cols = "T", rows = "ID", value = "V") |> plan_cells("{value:.1f}") |>
    plan_paginate_rows(page_by = "GRP")
  child <- table_plan(pl_long(), cols = "T", rows = c(block = ~ "Summary")) |>
    plan_cells(V = c(Mean = "{mean:.1f}"))
  t <- as.data.frame(plan_apply(parent |> plan_append(child) |> plan_blanks(where = "between_parts"), "table"))
  expect_identical(t$label[1:4], c("A", "", "Summary", paste0(strrep(" ", 4L), "Mean")))
  expect_identical(t$GRP[2], "x")
  expect_identical(nrow(t), 8L)
  t2 <- as.data.frame(plan_apply(parent |> plan_append(child |> plan_blanks(first = TRUE, last = TRUE)), "table"))
  expect_identical(t2$label[1:6], c("A", "", "Summary", paste0(strrep(" ", 4L), "Mean"), "", "B"))
  # a blank row is a row of the pages (an empty row prints as a separator)
  pg <- plan_apply(parent |> plan_append(child) |> plan_blanks(where = "between_parts"), "pages")
  expect_identical(nrow(pg[[1L]]$data), 4L)
  # between_parts without parts
  expect_error(plan_apply(table_plan(rec, cols = "T", rows = "ID", value = "V") |>
                            plan_cells("{value:.1f}") |> plan_blanks(where = "between_parts"), "pages"),
               "has no plan_append")
})

test_that("FT-PL908 [FR-PL906]: a part may not carry the whole table's settings; its column key is the table's", {
  rec <- pl_rec()
  parent <- table_plan(rec, cols = "T", rows = "ID", value = "V") |> plan_cells("{value:.1f}")
  child <- table_plan(pl_long(), cols = "T", rows = c(block = ~ "Summary")) |>
    plan_cells(V = c(Mean = "{mean:.1f}"))
  bad <- function(c2, what) expect_error(parent |> plan_append(c2), what, fixed = TRUE)
  bad(child |> plan_col_header(rtf_col_header(c("a", "b", "c", "d"))), "plan_col_header()")
  bad(child |> plan_columns(widths = c(1, 1, 1, 1)), "plan_columns()")
  bad(child |> plan_paginate_rows(max_rows = 5), "plan_paginate_rows()")
  bad(child |> plan_paginate_cols(at = "t2"), "plan_paginate_cols()")
  bad(child |> plan_blanks(where = "between_groups"), "plan_blanks(where = )")
  bad(child |> plan_style(border = "tfl"), "plan_style()")
  bad(child |> plan_titles("T"), "plan_titles()")
  bad(child |> plan_levels(T = c("t3", "t2", "t1")), "plan_levels(T = )")
  bad(child |> plan_append(child), "plan_append()")
  expect_error(parent |> plan_append(table_plan(pl_long(), cols = "GRP", rows = c(block = ~ "S")) |>
                                      plan_cells(V = c(Mean = "{mean:.1f}"))),
               "column key is 'GRP' and the table's is 'T'")
  expect_error(parent |> plan_append(pl_long()), "is a table_plan")
  # a part may say how its own rows are made
  ok <- child |> plan_digits(2) |> plan_labels(V = "Value") |> plan_stub(indent = 2L) |>
    plan_cell_style(where = ~ label == "Summary", bold = TRUE) |> plan_blanks(first = TRUE)
  expect_s3_class(parent |> plan_append(ok), "table_plan")
  pg <- plan_apply(parent |> plan_append(ok) |> plan_style(border = "tfl"), "pages")[[1L]]
  # (its own blank row first: the heading is the stack's 4th row)
  expect_null(pg$cell_styles[[3L]])
  expect_true(isTRUE(pg$cell_styles[[4L]]$bold[1L]))
  expect_identical(pg$data$label[3:4], c("", "Summary"))
  # ... and runs on its own, with only what it says
  expect_identical(as.data.frame(plan_apply(child, "table"))$block[1], "Summary")
})

test_that("FT-PL909 [FR-PL907]: the header's {n} and {col}, the widths and the decimal split read the stack", {
  rec <- pl_rec()
  parent <- table_plan(rec, cols = "T", rows = "ID", value = "V") |> plan_cells("{value:.1f}")
  child <- table_plan(pl_long(), cols = "T", rows = c(block = ~ "Summary")) |>
    plan_cells(V = c(Mean = "{mean:.1f}"))
  p <- parent |> plan_append(child) |>
    plan_col_header(rtf_col_header(c("Subject", "{col}"))) |>
    plan_columns(widths = c(label = 2, .values = 1), decimal = ".values") |>
    plan_style(border = "tfl")
  pg <- plan_apply(p, "pages")[[1L]]
  expect_identical(pg$col_rel_width, c(2, 1, 1, 1))
  expect_identical(names(pg$data), c("label", "t1", "t2", "t3"))
  expect_true(any(grepl("t1", unlist(pg$col_header), fixed = TRUE)))
  # print() says the parts
  expect_output(print(p), "part")
})

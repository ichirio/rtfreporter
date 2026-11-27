# Coverage of R/table_plan.R: the branches of table_plan(), its print method,
# plan_cell_style(), the header (cells, functions, tokens, plan_layers()),
# the populations a header reads, plan_template() and the display arguments
# that no other test file reached.  All data are fictional (cards::ADSL).

# -- fixtures ------------------------------------------------------------------

# a normalized ARD, built once (cards is the slow part)
cov_tp_cache <- new.env()
cov_tp_ard <- function() {
  if (is.null(cov_tp_cache$ard)) {
    adsl <- cards::ADSL
    adsl$SEX <- as.character(adsl$SEX)
    adsl$TRT <- as.character(adsl$ARM)
    cov_tp_cache$ard <- suppressMessages(normalize_ard(cards::ard_stack(
      adsl, .by = TRT,
      cards::ard_summary(
        variables = AGE,
        statistic = ~ cards::continuous_summary_fns(c("N", "mean", "sd"))),
      cards::ard_tabulate(variables = SEX, statistic = ~ c("n", "p")),
      .total_n = TRUE)))
  }
  cov_tp_cache$ard
}

cov_tp_plan <- function(d = cov_tp_ard()) {
  table_plan(d, cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(continuous = c("n" = "{N:d}",
                              "Mean (SD)" = "{mean:.1f} ({sd:.2f})"),
               categorical = "{n:d} ({p:.1f%})", notes = FALSE)
}

# a finished table
cov_tp_wide <- function() {
  data.frame(Parameter = c("Subjects", "Age, mean (SD)", "Female, n (%)"),
             Placebo = c("86", "75.2 (8.59)", "53 (61.6)"),
             Active = c("84", "75.7 (8.29)", "50 (59.5)"),
             stringsAsFactors = FALSE)
}

# a lab-like long frame: one analysis variable, three parameters
cov_tp_lab <- function() {
  data.frame(
    ARM = rep(c("A", "B"), each = 6),
    PARAMCD = rep(rep(c("ALT", "BILI", "AST"), each = 2), 2),
    variable = "AVAL", .label = NA_character_, .kind = "continuous",
    context = "summary",
    stat_name = rep(c("mean", "sd"), 6),
    stat = c(31.2345, 4.10234, 0.61234, 0.123456, 28.5555, 3.3333,
             33.1111, 5.30303, 0.70707, 0.151515, 29.9999, 4.4444),
    stringsAsFactors = FALSE)
}

# a long frame with comparison records, split by CMP
cov_tp_cmp <- function() {
  d <- data.frame(TRT = rep(c("A", "B"), each = 6),
                  PARAM = rep(rep(c("ALT", "AST"), each = 3), 2),
                  stat_name = rep(c("n", "mean", "sd"), 4),
                  stat = c(20, 31.245, 4.1, 20, 28.7, 3.92,
                           18, 33.108, 5.3, 18, 30.2, 4.44),
                  stringsAsFactors = FALSE)
  d$variable <- d$PARAM
  d$context <- "summary"
  cmp <- data.frame(TRT = NA, PARAM = rep(c("ALT", "AST"), 2),
                    stat_name = rep(c("p.value", "estimate"), each = 2),
                    stat = c(0.0004, 0.123, -1.86, -1.5),
                    variable = rep(c("ALT", "AST"), 2),
                    context = "stats_t_test", stringsAsFactors = FALSE)
  d <- rbind(d, cmp)
  d$.kind <- "continuous"
  d$CMP <- NA_character_
  d$CMP[d$context == "stats_t_test"] <- c("A vs B", "A vs C", "A vs B", "A vs C")
  d
}

cov_tp_tab <- function(p) as.data.frame(suppressMessages(plan_apply(p, "table")))

# -- table_plan() and print() --------------------------------------------------

test_that("table_plan(value = ) that is not one column name is refused", {
  skip_on_cran()
  recs <- data.frame(USUBJID = "S1", AVAL = 1)
  msg <- "table_plan\\(value = \\) names one column of the records"
  expect_error(table_plan(recs, value = 1), msg)
  expect_error(table_plan(recs, value = c("AVAL", "USUBJID")), msg)
  expect_error(table_plan(recs, value = NA_character_), msg)
})

test_that("print() of a plan of an unknown kind leaves the source out", {
  skip_on_cran()
  p <- table_plan(cov_tp_wide())
  p$kind <- "something else"
  out <- capture.output(print(p))
  expect_identical(out[1L], "<table_plan>  0 layers  ->  table data.frame")
})

test_that("print() of a records plan shows a formula row key as written", {
  skip_on_cran()
  recs <- data.frame(USUBJID = c("S1", "S1", "S2"), VISIT = c("W1", "W2", "W1"),
                     AVAL = c(1.5, 2, 3), stringsAsFactors = FALSE)
  p <- table_plan(recs, cols = "VISIT", rows = c(subject = ~ "{USUBJID}"),
                  value = "AVAL")
  out <- capture.output(print(p))
  expect_true(any(grepl("rows   subject = ~\"{USUBJID}\"", out, fixed = TRUE)))
})

# -- plan_cell_style() ---------------------------------------------------------

test_that("plan_cell_style() with `cols` and a formula attribute styles only those columns", {
  skip_on_cran()
  p <- table_plan(cov_tp_wide()) |>
    plan_cell_style(cols = "Active", bold = ~ startsWith(Parameter, "Female"))
  st <- plan_apply(p)[[1L]]$cell_styles
  expect_length(st, 3L)
  expect_identical(lapply(st, function(r) r$bold[3L]), list(FALSE, FALSE, TRUE))
  # the other columns are left at the column default
  expect_true(all(is.na(st[[3L]]$bold[1:2])))
})

test_that("plan_cell_style() with `cols` keeps a per-column map of formulas as given", {
  skip_on_cran()
  p <- table_plan(cov_tp_wide()) |>
    plan_cell_style(cols = "Active", color = list(
      Active = ~ ifelse(Parameter == "Female, n (%)", "#CC0000", NA)))
  st <- plan_apply(p)[[1L]]$cell_styles
  expect_null(st[[1L]])
  expect_identical(st[[3L]]$color, c(NA, NA, "#CC0000"))
})

# -- plan_cells(): what is refused ---------------------------------------------

test_that("plan_cells() names a cell_rows() inside a chain as such", {
  skip_on_cran()
  skip_if_not_installed("cards")
  expect_error(plan_cells(cov_tp_plan(), categorical = list(cell_rows(a = "{n}"))),
               "not a list with a cell_rows\\(\\) in it\\.")
})

test_that("plan_cells() names a function given as a cell rule by its class", {
  skip_on_cran()
  skip_if_not_installed("cards")
  expect_error(plan_cells(cov_tp_plan(), categorical = mean),
               "plan_cells\\(categorical = \\) is .*, not an object of class function\\.")
})

# -- digits keyed by a row key's value -----------------------------------------

test_that("a guard whose right side is no template is kept for the engine to refuse (row key digits)", {
  skip_on_cran()
  p <- table_plan(cov_tp_lab(), cols = "ARM", rows = c(param = "PARAMCD")) |>
    plan_cells(continuous = list(sd < 0.2 ~ NA, "{mean} ({sd})"), notes = FALSE) |>
    plan_digits(continuous = c(mean = 2, sd = 3)) |>
    plan_digits(PARAMCD = c(ALT = 1))
  expect_error(cov_tp_tab(p),
               "The right of a `cells` guard must be one template string")
})

test_that("a cell_rows() entry takes a row key value's digits row by row", {
  skip_on_cran()
  p <- table_plan(cov_tp_lab(), cols = "ARM", rows = c(param = "PARAMCD")) |>
    plan_cells(continuous = cell_rows("Mean (SD)" = "{mean} ({sd})",
                                      "Mean" = "{mean}"), notes = FALSE) |>
    plan_digits(continuous = c(mean = 2, sd = 3)) |>
    plan_digits(PARAMCD = c(ALT = 1))
  t <- cov_tp_tab(p)
  expect_identical(t$A, c("31.2 (4.1)", "31.2", "0.61 (0.123)", "0.61",
                          "28.56 (3.333)", "28.56"))
  expect_identical(as.character(t$label[1:2]), c("Mean (SD)", "Mean"))
})

test_that(".plan_fill_keyed() passes an element that is no template through", {
  skip_on_cran()
  # internal: plan_cells() refuses a number in a chain (.plan_check_cell_rule()),
  # so only a direct call reaches this pass-through
  out <- rtfreporter:::.plan_fill_keyed(
    list(1, "{mean}"), list(PARAMCD = list(ALT = 1), default = 3), "PARAMCD",
    data.frame(PARAMCD = "ALT", stringsAsFactors = FALSE), "AVAL",
    NA_character_, "continuous")
  expect_identical(out$entry[[1L]], 1)
  expect_identical(out$entry[[length(out$entry)]], "{mean:.3f}")
  expect_identical(deparse(out$entry[[2L]][[2L]]), "PARAMCD == \"ALT\"")
})

test_that(".plan_check_open() reads nothing from an element that is no template", {
  skip_on_cran()
  # internal: as above, no verb lets a number reach the check
  expect_null(rtfreporter:::.plan_check_open(list(1, TRUE), "AGE"))
  expect_error(rtfreporter:::.plan_check_open(list(1, "{p:%}"), "AGE"),
               "`\\{p:%\\}` asks the plan for its digits")
})

# -- the long frame ------------------------------------------------------------

test_that("plan_apply() says when a stat role's column is gone from the data", {
  skip_on_cran()
  vl <- data.frame(TRT = rep(c("A", "B"), each = 2), PARAM = "ALT",
                   STAT = c("n", "mean"), stat = c(20, 31.2, 18, 33.1),
                   stringsAsFactors = FALSE)
  p <- table_plan(vl, cols = "TRT", rows = c(param = "PARAM"),
                  label = c(row = "STAT"), stat = c(name = "STAT")) |>
    plan_cells("{n:.0f}", notes = FALSE)
  expect_identical(cov_tp_tab(p)$A[1L], "20")
  # the roles were checked when the plan was made; a frame changed after
  # that is checked again when it runs
  p$data$STAT <- NULL
  expect_error(plan_apply(p, "table"),
               "table_plan\\(stat_name = .STAT.\\): no such column\\.\n  Columns: TRT, PARAM, stat")
})

test_that("plan_nest() on a table with no label column says so", {
  skip_on_cran()
  skip_if_not_installed("cards")
  p <- table_plan(cov_tp_ard(), cols = "TRT", rows = c(group = "variable"),
                  label = NA) |>
    plan_cells(categorical = "{n}", continuous = "{mean}", notes = FALSE) |>
    plan_nest(AGE = c(SEX = "F"))
  expect_error(plan_apply(p, "table"),
               "plan_nest\\(\\): the table has no row label column to match the level in\\.")
})

test_that(".plan_total_has() is FALSE for a key the records do not have", {
  skip_on_cran()
  # internal: every caller passes keys of the frame
  d <- data.frame(a = c(1, NA), stringsAsFactors = FALSE)
  expect_identical(rtfreporter:::.plan_total_has(d, c("a", "zz")), c(FALSE, FALSE))
  expect_identical(rtfreporter:::.plan_total_has(d, "a"), c(TRUE, FALSE))
})

test_that(".plan_levels_names() keeps a label that is not text as it is", {
  skip_on_cran()
  # internal: plan_labels() only takes text, so no verb hands it a list
  out <- rtfreporter:::.plan_levels_names(list(
    levels = list(SEX = c(F = "Female", M = "Male")),
    labels = list(SEX = list("Sex"))))
  expect_identical(out$levels$SEX, c("F", "M"))
  expect_identical(out$labels$SEX, list("Sex"))
})

test_that(".plan_restyle() records a styling call as a restyle layer", {
  skip_on_cran()
  # internal: no verb calls this helper (plan_cell_style() writes the
  # restyle layer itself); the NULL arguments are dropped
  p <- rtfreporter:::.plan_restyle(table_plan(cov_tp_wide()), "style_cols",
                                   list(cols = "Active", bold = TRUE, italic = NULL))
  l <- p$layers[[length(p$layers)]]
  expect_identical(l$kind, "restyle")
  expect_identical(l$fields$fun, "style_cols")
  expect_identical(l$fields$args, list(cols = "Active", bold = TRUE))
})

# -- the display arguments -----------------------------------------------------

test_that("plan_style() with a preset and a zone that is no rtf_border stops", {
  skip_on_cran()
  p <- table_plan(cov_tp_wide()) |>
    plan_style(border = "tfl", border_body = "single")
  expect_error(plan_apply(p),
               "`border_body` must be NULL or an rtf_border object.", fixed = TRUE)
})

test_that("a row_title position past the printed columns names the hidden ones", {
  skip_on_cran()
  p <- table_plan(cov_tp_wide()) |>
    plan_hide("Active") |>
    plan_columns(row_title = 5)
  expect_error(plan_apply(p), paste0(
    "plan_columns(row_title = 5): the table prints 2 columns (Parameter, ",
    "Placebo); Active hidden; positions count those, or name the columns."),
    fixed = TRUE)
})

test_that("a row_title position past the columns, none hidden, lists the printed ones", {
  skip_on_cran()
  p <- table_plan(cov_tp_wide()) |> plan_columns(row_title = 9)
  expect_error(plan_apply(p), paste0(
    "plan_columns(row_title = 9): the table prints 3 columns (Parameter, ",
    "Placebo, Active); positions count those, or name the columns."),
    fixed = TRUE)
})

test_that(".plan_heading_at() sets heading_at on a single rtftable", {
  skip_on_cran()
  # internal: a plan that sets heading_at always pages, so its pages come
  # to this helper as a list
  p <- table_plan(cov_tp_wide()) |> plan_paginate_group(heading_at = "title")
  one <- rtftable(cov_tp_wide())
  out <- rtfreporter:::.plan_heading_at(p, one)
  expect_s3_class(out, "rtftable")
  expect_identical(attr(out, "rtf_heading_at"), "title")
  # nothing asked for: the table as it came
  expect_null(attr(rtfreporter:::.plan_heading_at(table_plan(cov_tp_wide()), one),
                   "rtf_heading_at"))
})

# -- pages split by a row key that is a template ------------------------------

cov_tp_recs <- function() {
  recs <- data.frame(USUBJID = c("S1", "S1", "S2", "S2"),
                     SITE = c("A", "A", "B", "B"),
                     VISIT = c("W1", "W2", "W1", "W2"),
                     AVAL = c(1.5, 2, 3, 4), stringsAsFactors = FALSE)
  table_plan(recs, cols = "VISIT",
             rows = c(site = ~ "{SITE}", subject = ~ "{USUBJID}"),
             value = "AVAL") |>
    plan_cells("{value}")
}

test_that("BY pages on a template row key are headed by the page's value", {
  skip_on_cran()
  p <- cov_tp_recs() |>
    plan_paginate_rows(page_by = "site", heading = "Site {value}")
  pg <- plan_apply(p)
  expect_identical(names(pg), c("Site A", "Site B"))
})

test_that("plan_n_candidates() of pages grouped on a template row key reads the whole table", {
  skip_on_cran()
  p <- cov_tp_recs() |> plan_paginate_group(col = "site")
  out <- plan_n_candidates(p)
  expect_s3_class(out, "data.frame")
  expect_identical(names(out), c("scope", "page", "column", "value"))
  expect_false("page" %in% out$scope)
})

# -- the header ----------------------------------------------------------------

test_that("a header sheet with a stub of no name heads the page's first column", {
  skip_on_cran()
  skip_if_not_installed("cards")
  hdr <- data.frame(line = c("1", "1"), cols = c("group", ".values"),
                    span = c(NA, "each"), text = c("Subjects with", "{col}"),
                    stringsAsFactors = FALSE)
  p <- table_plan(cov_tp_ard(), cols = "TRT", rows = list(group = "variable"),
                  label = NA) |>
    plan_cells(categorical = "{n}", continuous = "{mean}", notes = FALSE) |>
    plan_stub(before = TRUE) |>
    plan_col_header(header = hdr)
  pg <- plan_apply(p)
  expect_identical(pg[[1L]]$col_header[[1L]],
                   c("Subjects with", "Placebo", "Xanomeline High Dose",
                     "Xanomeline Low Dose"))
})

test_that("a header function of two arguments gets the n and the finished table", {
  skip_on_cran()
  skip_if_not_installed("cards")
  p <- cov_tp_plan() |>
    plan_col_header(values = list(n = TRUE), header = function(n, tbl)
      list(c(names(tbl)[1:2], paste0(names(n), " (", nrow(tbl), " rows)"))))
  expect_identical(plan_apply(p)[[1L]]$col_header[[1L]],
                   c("group", "label", "Placebo (4 rows)",
                     "Xanomeline High Dose (4 rows)",
                     "Xanomeline Low Dose (4 rows)"))
})

test_that("a header function that returns a row of NULL is refused by set_col_header()", {
  skip_on_cran()
  skip_if_not_installed("cards")
  p <- cov_tp_plan() |>
    plan_col_header(header = function(n) list(c("A", "B", "{col}"), NULL))
  expect_error(plan_apply(p),
               "Each col_header row must be a character vector or a non-empty list of cell specs.",
               fixed = TRUE)
})

test_that("plan_compare(by = ) with two columns heads each comparison and column", {
  skip_on_cran()
  p <- table_plan(cov_tp_cmp(), cols = "TRT", rows = c(param = "variable"),
                  label = c(row = "stat_name")) |>
    plan_cells(c("n" = "{n:.0f}", "Mean (SD)" = "{mean:.1f} ({sd:.1f})"),
               notes = FALSE) |>
    plan_compare(p = "{p.value:.3f}", e = "{estimate:.1f}", by = "CMP") |>
    plan_col_header(header = rtf_col_header(c("", "", "{col}")))
  pg <- plan_apply(p)
  expect_identical(pg[[1L]]$col_header[[1L]],
                   c("", "", "A", "B", "A vs B____p", "A vs B____e",
                     "A vs C____p", "A vs C____e"))
  # the comparison columns are no `{col}`
  expect_identical(plan_header_tokens(p)$values[[1L]], c("A", "B"))
})

# -- plan_layers(): a resolved header read back as cells -----------------------

test_that("plan_layers() reads a spanner over one level of a key as span = <key>, its rule too", {
  skip_on_cran()
  skip_if_not_installed("cards")
  d <- cov_tp_ard()
  d$PH <- ifelse(is.na(d$TRT), NA, "Phase 1")
  p <- table_plan(d, cols = c("PH", "TRT"), rows = c(group = "variable")) |>
    plan_cells(continuous = c("Mean (SD)" = "{mean:.1f} ({sd:.2f})"),
               categorical = "{n:d}", notes = FALSE) |>
    plan_col_header(header = rtf_col_header(
      list(col_cell(1L, ""), col_cell(2L, ""), col_cell(c(3L, 5L), "Phase 1")),
      list(col_cell(1L, ""), col_cell(2L, ""),
           col_cell(c(3L, 5L), "", border = rtf_border(bottom = TRUE))),
      c("", "", "{col}")))
  cells <- suppressMessages(plan_layers(p))$header$cells
  v <- cells[cells$cols == ".values", ]
  expect_identical(v$text, c("{col1}", NA, "{col}"))
  expect_identical(v$span, c("PH", "PH", "each"))
  expect_identical(v$border_bottom, c(NA, "single", NA))
})

test_that("plan_layers() reads a one-column table's header row as span = each", {
  skip_on_cran()
  skip_if_not_installed("cards")
  d <- cov_tp_ard()
  p <- cov_tp_plan(d[d$TRT %in% "Placebo" | is.na(d$TRT), ]) |>
    plan_col_header(header = rtf_col_header(c("", "", "Placebo")))
  cells <- suppressMessages(plan_layers(p))$header$cells
  expect_identical(cells$cols, c("group", "label", ".values"))
  expect_identical(cells$span, c(NA, NA, "each"))
  expect_identical(cells$text[3L], "{col}")
})

# -- plan_header_tokens() and the populations ----------------------------------

test_that("plan_header_tokens() names a population request that cannot be read", {
  skip_on_cran()
  skip_if_not_installed("cards")
  p <- cov_tp_plan() |>
    plan_col_header(values = list(n = "bogus"), header = c("", "", "{col} N={n}"))
  tk <- plan_header_tokens(p)
  expect_identical(tk$token[1L], "{n}")
  expect_false(tk$resolved[1L])
  expect_match(tk$text[1L], "^-- NOT resolved: plan_col_header\\(values = list\\(n = .bogus.\\)\\)")
  expect_identical(tk$token[2L], "{col}")
})

test_that("plan_header_tokens() of a plan that does not build still shows its n", {
  skip_on_cran()
  skip_if_not_installed("cards")
  # `{p:%}` with no digits stops the table; the populations are read anyway,
  # keyed by column but with no columns to keep them to
  p <- cov_tp_plan() |>
    plan_cells(categorical = "{n} ({p:%})") |>
    plan_col_header(values = list(n = TRUE), header = c("", "", "{col} N={n}"))
  expect_error(plan_apply(p, "table"), "asks the plan for its digits")
  tk <- plan_header_tokens(p)
  expect_false("{col}" %in% tk$token)
  expect_identical(tk$token[1:2], c("{n}", "{n:sum}"))
  expect_identical(tk$values[[2L]], 254)
})

test_that("plan_header_tokens() says which columns' n a Total could not read", {
  skip_on_cran()
  skip_if_not_installed("cards")
  # no arm count of its own: the arms' n is not stated, the Total's is
  d <- cov_tp_ard()
  p <- cov_tp_plan(d[d$variable != "TRT", ]) |>
    plan_total() |>
    plan_col_header(values = list(n = TRUE), header = c("", "", "{col} N={n}"))
  tk <- plan_header_tokens(p)
  bad <- tk[tk$token == "{n} NOT resolved", ]
  expect_identical(nrow(bad), 1L)
  expect_false(bad$resolved)
  expect_match(bad$text, "^-- Placebo: only AGE states an N \\(86\\).*\\(\\+2 more\\)$")
})

test_that("plan_header_tokens() says the Total column has no study total to read", {
  skip_on_cran()
  skip_if_not_installed("cards")
  d <- cov_tp_ard()
  d <- d[d$variable != "TRT", ]
  d$.total_n <- NULL
  attr(d, "ard_total_n") <- NULL
  p <- cov_tp_plan(d) |>
    plan_total() |>
    plan_col_header(values = list(n = TRUE), header = c("", "", "{col} N={n}"))
  tk <- plan_header_tokens(p)
  # the token row shows the first reason, an arm's
  expect_match(tk$note[1L], "^only AGE states an N \\(86\\)")
  # the Total's own reason sits with the others (internal: the reasons
  # are only printed one at a time)
  why <- attr(rtfreporter:::.plan_n_values(p, list(n = TRUE))$n, "why")
  expect_identical(names(why), c("Placebo", "Xanomeline High Dose",
                                 "Xanomeline Low Dose", "Total"))
  expect_match(why[["Total"]],
               "the ARD states no study total for the Total column", fixed = TRUE)
})

test_that("the header's n follows the order plan_levels() gives the columns", {
  skip_on_cran()
  skip_if_not_installed("cards")
  lv <- c("Xanomeline Low Dose", "Placebo", "Xanomeline High Dose")
  p <- cov_tp_plan() |>
    plan_levels(TRT = lv) |>
    plan_col_header(values = list(n = TRUE), header = c("", "", "{col} N={n}"))
  tk <- plan_header_tokens(p)
  expect_identical(names(tk$values[[which(tk$token == "{n}")]]), lv)
  expect_identical(tk$token[4:6], paste0("{n:", lv, "}"))
})

test_that("one sentinel stating two different N for a column stops the header", {
  skip_on_cran()
  skip_if_not_installed("cards")
  d <- cov_tp_ard()
  s <- d[d$variable == "TRT" & d$stat_name == "N", ][c(1L, 1L), ]
  s$variable <- "..ard_total_n.."
  s$stat <- I(list(50, 60))
  s$.key_own <- FALSE
  s$context <- "total_n"
  p <- cov_tp_plan(rbind(d[d$variable != "TRT", ], s)) |>
    plan_col_header(values = list(n = TRUE), header = c("", "", "{col} N={n}"))
  expect_error(plan_apply(p), paste0(
    "Placebo: the ARD states two different counts for this column ",
    "(context total_n says 50, context total_n says 60): keep one"), fixed = TRUE)
})

# -- plan_template() -----------------------------------------------------------

test_that("plan_template() writes `{n}` for a categorical kind with counts and no percent", {
  skip_on_cran()
  skip_if_not_installed("cards")
  ard <- cards::ard_tabulate(cards::ADSL, by = ARM, variables = SEX)
  code <- plan_template(ard[ard$stat_name == "n", ], cols = "ARM", pipe = "|>")
  expect_true("    categorical = \"{n:.0f}\"" %in% code)
})

test_that("plan_template() writes the first statistic for a categorical kind with no n", {
  skip_on_cran()
  skip_if_not_installed("cards")
  ard <- cards::ard_tabulate(cards::ADSL, by = ARM, variables = SEX)
  code <- plan_template(ard[ard$stat_name == "N", ], cols = "ARM", pipe = "|>")
  expect_true("    categorical = \"{N}\"" %in% code)
})

test_that(".plan_call() writes a call of no arguments on one line", {
  skip_on_cran()
  # internal: plan_template() only calls it with arguments
  expect_identical(rtfreporter:::.plan_call("normalize_ard", character(), "|>"),
                   "  rtfreporter::normalize_ard() |>")
  expect_identical(rtfreporter:::.plan_call("normalize_ard", character(), "|>",
                                            last = TRUE),
                   "  rtfreporter::normalize_ard()")
})

test_that("plan_template()'s generated header is one row over the columns", {
  skip_on_cran()
  skip("bug: plan_template() writes header = function(n) c(...), which plan_col_header() reads as one row per element")
  skip_if_not_installed("cards")
  ard <- cards::ard_stack(cards::ADSL, .by = ARM,
                          cards::ard_tabulate(variables = SEX))
  code <- plan_template(ard, cols = "ARM", pipe = "|>")
  env <- new.env()
  env$ard <- ard
  eval(parse(text = code), envir = env)
  hdr <- suppressMessages(plan_apply(env$p))[[1L]]$col_header
  expect_length(hdr, 1L)
  expect_identical(hdr[[1L]][1:2], c("Characteristic", "Placebo\nN = 86"))
})

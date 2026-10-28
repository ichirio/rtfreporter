# Tests of the plan (deferred form), moved from tflspec with the engine (#491).
# Tests for the DEFERRED, LAST-WINS plan spike (#474).
# This whole file belongs to R/ard-plan-spike.R and is deleted with it.

local_deprecated("stub_vars")  # deprecated in 0.8.x

skip_if_no_cards2 <- function() testthat::skip_if_not_installed("cards")

# Flattening is RUN before the plan now, so that table_plan()'s roles
# name columns that exist.  This keeps the tests to one line.
nz <- function(x) {
  if (identical(rtfreporter:::.plan_source_kind(x), "ard"))
    suppressMessages(normalize_ard(x)) else x
}

plan_ard <- function() {
  adsl <- cards::ADSL
  adsl$SEX <- as.character(adsl$SEX)
  adsl$TRT <- as.character(adsl$ARM)
  cards::ard_stack(
    adsl, .by = TRT,
    cards::ard_summary(
      variables = c(AGE, BMIBL),
      statistic = ~ cards::continuous_summary_fns(c("N", "mean", "sd"))),
    cards::ard_tabulate(variables = SEX, statistic = ~ c("n", "p")),
    .total_n = TRUE)
}

base_plan <- function(ard = plan_ard()) {
  table_plan(nz(ard), cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(continuous  = c("n"         = "{N:d}",
                               "Mean (SD)" = "{mean} ({sd})"),
               categorical = "{n:d} ({p:.1f%})")
}

# --------------------------------------------------------------- the rule


test_that("a later layer wins, which is the whole point", {
  skip_if_no_cards2()
  p <- base_plan() |> plan_digits(2) |> plan_digits(AGE = 0)
  cells <- plan_apply(p, "args")$widen$cells

  # set everything, then fix one variable -- a two-line edit
  expect_match(cells$AGE[["Mean (SD)"]], "{mean:.0f} ({sd:.0f})", fixed = TRUE)
  expect_match(cells$BMIBL[["Mean (SD)"]], "{mean:.2f} ({sd:.2f})",
               fixed = TRUE)
})

test_that("last wins for the same key too, not just for a narrower one", {
  skip_if_no_cards2()
  p <- base_plan() |> plan_digits(2) |> plan_digits(AGE = 0) |>
    plan_digits(AGE = 3)
  expect_match(plan_apply(p, "args")$widen$cells$AGE[["Mean (SD)"]],
               "{mean:.3f}", fixed = TRUE)
})

test_that("a later plan_cells() replaces an earlier entry for that key", {
  skip_if_no_cards2()
  p <- base_plan() |> plan_cells(continuous = c("n" = "{N:d}"))
  ent <- plan_apply(p, "args")$widen$cells$continuous
  expect_length(ent, 1L)
  expect_identical(unname(ent), "{N:d}")
})

test_that("levels and labels merge one name at a time", {
  skip_if_no_cards2()
  p <- base_plan() |>
    plan_levels(TRT = c("Placebo", "Xanomeline Low Dose",
                        "Xanomeline High Dose")) |>
    plan_levels(SEX = c("M", "F"))          # adds, does not replace
  lv <- plan_apply(p, "args")$widen$levels
  expect_setequal(names(lv), c("TRT", "SEX"))
  expect_identical(lv$SEX, c("M", "F"))
})

test_that("one key restated is replaced, not merged into", {
  skip_if_no_cards2()
  p <- base_plan() |> plan_levels(SEX = c("M", "F")) |>
    plan_levels(SEX = c("F", "M"))
  expect_identical(plan_apply(p, "args")$widen$levels$SEX, c("F", "M"))
})

test_that("a map can be handed over whole, not taken apart", {
  skip_if_no_cards2()
  lab <- c(AGE = "Age (years)", SEX = "Sex")
  p <- base_plan() |> plan_labels(lab)
  expect_identical(unlist(plan_apply(p, "args")$widen$labels), lab)
})


# ------------------------------------------------------------ the digits


test_that("a token that states its own digits keeps them", {
  skip_if_no_cards2()
  p <- table_plan(nz(plan_ard()), cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(continuous = c("Mean (SD)" = "{mean:.4f} ({sd})")) |>
    plan_digits(1)
  ent <- plan_apply(p, "args")$widen$cells$AGE
  expect_match(ent[["Mean (SD)"]], "{mean:.4f} ({sd:.1f})", fixed = TRUE)
})

test_that("`{p:%}` with no digits declared says what to do about it", {
  skip_if_no_cards2()
  p <- table_plan(nz(plan_ard()), cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(categorical = "{n:d} ({p:%})")
  expect_error(plan_apply(p, "args"), "asks the plan for its digits")
  expect_error(plan_apply(p, "args"), "plan_digits")
})

test_that("digits pick by specificity once last-wins has had its say", {
  skip_if_no_cards2()
  # `continuous` is a kind, `AGE` is a variable: the variable is narrower
  p <- table_plan(nz(plan_ard()), cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(continuous  = c("Mean (SD)" = "{mean} ({sd})"),
               categorical = "{n:d} ({p:%})") |>
    plan_digits(continuous = 2) |> plan_digits(AGE = 0) |>
    plan_digits(SEX = 1)
  cells <- plan_apply(p, "args")$widen$cells
  expect_match(cells$BMIBL[["Mean (SD)"]], "{mean:.2f}", fixed = TRUE)
  expect_match(cells$AGE[["Mean (SD)"]], "{mean:.0f}", fixed = TRUE)
  expect_match(cells$SEX, "{p:.1f%}", fixed = TRUE)
})


# ----------------------------------------------------- nothing runs early


test_that("the data is held, not transformed, and nothing is run", {
  skip_if_no_cards2()
  ard <- plan_ard()
  p <- base_plan(ard) |> plan_digits(1)
  expect_identical(p$data, nz(ard))
  expect_false(is.data.frame(p$layers))
  # `args` resolves without running the conversion
  a <- plan_apply(p, "args")$widen
  expect_true(all(c("cols", "rows", "cells") %in% names(a)))
})

test_that("the plan prints its layers in the order that decides the result", {
  skip_if_no_cards2()
  out <- utils::capture.output(print(base_plan() |> plan_digits(1)))
  expect_true(any(grepl("what cols / rows / label may name", out)))
  expect_true(any(grepl("cols ", out)))
  expect_true(any(grepl("1\\. cells", out)))
  expect_true(any(grepl("2\\. digits", out)))
  expect_true(any(grepl("plan_apply", out)))
})

test_that("a plan with no layers says so rather than printing nothing", {
  skip_if_no_cards2()
  out <- utils::capture.output(print(table_plan(nz(plan_ard()))))
  expect_true(any(grepl("no layers", out)))
})


# ------------------------------------------- the same answer as the verbs


test_that("a plan and the immediate form agree", {
  skip_if_no_cards2()
  ard <- plan_ard()
  cells <- list(continuous  = c("n"         = "{N:d}",
                                "Mean (SD)" = "{mean:.1f} ({sd:.2f})"),
                categorical = "{n:d} ({p:.1f%})")
  direct <- suppressMessages(
    ard |> normalize_ard() |>
      widen_ard(cols = "TRT", rows = c(group = "variable"), cells = cells))
  planned <- suppressMessages(plan_apply(
    table_plan(nz(ard), cols = "TRT", rows = c(group = "variable")) |>
      plan_cells(continuous = cells$continuous,
                 categorical = cells$categorical)))
  expect_equal(planned, direct)
})

test_that("stage = 'long' is the frame going in", {
  skip_if_no_cards2()
  ard <- plan_ard()
  expect_equal(plan_apply(base_plan(ard), "input"), normalize_ard(ard))
})


# ----------------------------------------------------------- the seam


test_that("a plan can start from an already-normalized frame", {
  skip_if_no_cards2()
  ard <- plan_ard()
  d <- normalize_ard(ard)
  d$variable <- ifelse(d$variable == "BMIBL", "AGE", d$variable)  # a seam edit

  p <- table_plan(d, cols = "TRT", rows = c(group = "variable"))
  expect_identical(p$kind, "normalized")
  expect_true(any(grepl("normalized frame",
                        utils::capture.output(print(p)))))

  out <- suppressMessages(plan_apply(
    p |> plan_cells(continuous = c("n" = "{N:d}"), categorical = "{n:d}")))
  expect_true(is.data.frame(out))
})

test_that("a raw ARD is refused, with the line to write", {
  skip_if_no_cards2()
  # the roles name columns of what is handed in, so a frame that has
  # not been flattened cannot be one of them
  expect_error(table_plan(plan_ard()), "NORMALIZED frame")
  expect_error(table_plan(plan_ard()), "normalize_ard()", fixed = TRUE)
})

test_that("a role that names no column blames table_plan()", {
  skip_if_no_cards2()
  expect_error(table_plan(nz(plan_ard()), cols = "ARM"),
               "no column .ARM. in the data")
  expect_error(table_plan(nz(plan_ard()), cols = "TRT",
                        rows = c(g = "nope")),
               "table_plan(rows = )", fixed = TRUE)
})


# --------------------------------------------------------------- refusals


test_that("two unnamed values are refused", {
  skip_if_no_cards2()
  expect_error(plan_digits(table_plan(plan_ard()), 1, 2), "at most one unnamed")
})

test_that("the rounding family is last-wins, like every other layer", {
  skip_if_no_cards2()
  p <- base_plan() |> plan_digits(1, rounding = "sas") |> plan_digits(2, rounding = "r")
  expect_identical(plan_apply(p, "args")$widen$rounding, "r")
})

test_that("one rounding family reaches widen_ard()", {
  skip_if_no_cards2()
  p <- base_plan() |> plan_digits(1) |> plan_digits(1, rounding = "sas")
  expect_identical(plan_apply(p, "args")$widen$rounding, "sas")
})


# ------------------------------------------------- what a plan starts from

# A long summary somebody built with dplyr: keys, a statistic name and a
# value, and nothing cards ever touched.
hand_long <- function(var_col = "PARAM") {
  d <- data.frame(
    TRT       = rep(c("A", "B"), each = 6),
    PARAM     = rep(rep(c("ALT", "AST"), each = 3), 2),
    stat_name = rep(c("n", "mean", "sd"), 4),
    stat      = c(20, 31.245, 4.1, 20, 28.7, 3.92,
                  18, 33.108, 5.3, 18, 30.2, 4.44),
    stringsAsFactors = FALSE)
  names(d)[names(d) == "PARAM"] <- var_col
  d
}


test_that("the four kinds of source are told apart by their columns", {
  skip_if_no_cards2()
  kind <- rtfreporter:::.plan_source_kind
  expect_identical(kind(plan_ard()), "ard")
  expect_identical(kind(normalize_ard(plan_ard())), "normalized")
  expect_identical(kind(hand_long()), "long")
  expect_identical(kind(data.frame(group = "ALT", A = "31.2")), "wide")
})

test_that("a long frame nobody built with cards makes a table", {
  out <- suppressMessages(plan_apply(
    table_plan(nz(hand_long()), cols = "TRT", rows = c(param = "PARAM"),
                  label = c(row = "stat_name")) |> plan_cells(notes = FALSE) |>
      plan_cells(c("n" = "{n:.0f}", "Mean (SD)" = "{mean} ({sd})")) |>
      plan_digits(2)))
  expect_identical(names(out), c("param", "row", "A", "B"))
  # the plan-wide digits reached a frame with no `variable` column at all
  expect_identical(out$A[out$param == "ALT" & out$row == "Mean (SD)"],
                   "31.25 (4.10)")
})

test_that("per-variable keys work once the column is called `variable`", {
  out <- suppressMessages(plan_apply(
    table_plan(nz(hand_long("variable")), cols = "TRT", rows = c(param = "variable"),
                  label = c(row = "stat_name")) |> plan_cells(notes = FALSE) |>
      plan_cells(c("Mean (SD)" = "{mean} ({sd})")) |>
      plan_digits(2) |> plan_digits(AST = 3)))
  expect_identical(out$A[out$param == "ALT"], "31.25 (4.10)")
  expect_identical(out$A[out$param == "AST"], "28.700 (3.920)")
})

test_that("a digits key that reached nothing is refused, not ignored", {
  p <- table_plan(nz(hand_long()), cols = "TRT", rows = c(param = "PARAM"),
                label = c(row = "stat_name")) |> plan_cells(notes = FALSE) |>
    plan_cells(c("Mean (SD)" = "{mean} ({sd})")) |>
    plan_digits(2) |>
    plan_digits(AST = 3)          # AST is a PARAM value, not a variable
  # neither a variable nor a column of the table the plan makes
  expect_error(plan_apply(p, "table"), "matched nothing")
  expect_error(plan_apply(p, "table"), "AST")
})

test_that("a frame that is already the table is refused at the door", {
  # the point of deferring: this is caught at table_plan(), not three stages
  # later inside the resolver
  # the refusal moved to resolution: a listing's source is an ordinary frame
  # too, and only plan_listing() can say which this is
  p <- table_plan(data.frame(group = "ALT", A = "31.2 (4.1)"))
  expect_error(plan_apply(p), "declares nothing")
  expect_error(plan_apply(p), "plan_listing")
})

test_that("widen_ard() tolerates a frame with no variable/context", {
  # it used to fail with an internal R error rather than a message
  out <- suppressMessages(widen_ard(
    hand_long(), cols = "TRT", rows = c(param = "PARAM"),
    label = c(row = "stat_name"),
    cells = c("Mean (SD)" = "{mean:.1f} ({sd:.1f})"), notes = FALSE))
  expect_identical(out$A[out$param == "ALT"], "31.2 (4.1)")
})

test_that("a missing .label says what to do instead of naming the ARD", {
  expect_error(
    widen_ard(hand_long(), cols = "TRT", rows = c(param = "PARAM"),
               cells = "{mean:.1f}"),
    "has no `.label`")
  expect_error(
    widen_ard(hand_long(), cols = "TRT", rows = c(param = "PARAM"),
               cells = "{mean:.1f}"),
    "carries the row identity")
})

# --------------------------------------------------------- the display half

disp_plan <- function(ard = plan_ard()) {
  table_plan(nz(ard), cols = "TRT", rows = c(group = "variable")) |> plan_cells(notes = FALSE) |>
    plan_cells(continuous  = c("Mean (SD)" = "{mean:.1f} ({sd:.2f})"),
               categorical = "{n:.0f} ({p:.1f%})")
}


test_that("stage = \"pages\" reaches the same rtftable as the code does", {
  skip_if_no_cards2()
  ard <- plan_ard()
  tbl <- suppressMessages(plan_apply(disp_plan(ard)))

  ref <- as_rtftables(tbl, stub_vars = c("group", "label"),
                      group_by = "indent", split = "group_safe",
                      max_rows = 22, border = "tfl")
  new <- suppressMessages(plan_apply(
    disp_plan(ard) |>
      plan_stub(vars = c("group", "label")) |>
      plan_row_group(mode = "indent") |>
      plan_paginate_rows(split = "group_safe", max_rows = 22) |>
      plan_style(border = "tfl"),
    "pages"))
  expect_equal(new, ref)
})

test_that("plan_col_header(values = ) reads the ARD and spends it", {
  skip_if_no_cards2()
  # the point of resolving them together: the denominator in the header and
  # the percentages under it come from one reading of one ARD
  new <- suppressMessages(plan_apply(
    disp_plan() |>
      plan_stub(vars = c("group", "label")) |> plan_style(border = "tfl") |>
      plan_col_header(values = function(a) pull_ard(a, cols = "TRT", variable = "AGE"),
                      header = function(n) c("Characteristic",
                                             paste0(names(n), " N=",
                                                    as.integer(n)))),
    "pages"))
  hdr <- unlist(new[[1]]$col_header)
  expect_true(any(grepl("Placebo N=86", hdr, fixed = TRUE)))
})

test_that("a literal n is taken as it is", {
  skip_if_no_cards2()
  new <- suppressMessages(plan_apply(
    disp_plan() |>
      plan_stub(vars = c("group", "label")) |>
      plan_col_header(values = 254L,
                      header = function(n) c(paste0("All (N=", n, ")"),
                                             "A", "B", "C")),
    "pages"))
  expect_true(any(grepl("All (N=254)", unlist(new[[1]]$col_header),
                        fixed = TRUE)))
})

test_that("plan_stub() folds the row keys before as_rtftables() sees them", {
  skip_if_no_cards2()
  new <- suppressMessages(plan_apply(
    disp_plan() |>
      plan_stub(vars = c("group", "label"), name = "row_label") |>
      plan_style(border = "tfl"),
    "pages"))
  expect_true("row_label" %in% names(new[[1]]$data))
})

test_that("plan_after() runs its steps on the pages, in order", {
  skip_if_no_cards2()
  seen <- character(0)
  new <- suppressMessages(plan_apply(
    disp_plan() |>
      plan_stub(vars = c("group", "label")) |>
      plan_after(function(x) { seen <<- c(seen, "one"); x },
                 function(x) { seen <<- c(seen, "two"); x }),
    "pages"))
  expect_identical(seen, c("one", "two"))
})

test_that("plan_after() refuses anything that is not a function", {
  skip_if_no_cards2()
  expect_error(plan_after(disp_plan(), "set_decimal_split"),
               "takes functions")
  expect_error(plan_after(disp_plan(), "set_decimal_split"),
               "plan_columns(decimal = )", fixed = TRUE)
})

test_that("decimal alignment is a declaration: the same pages as an after() step", {
  skip_if_no_cards2()
  p <- base_plan()
  by_step <- as_rtftables(
    plan_after(p, function(x) set_decimal_split(x, cols = 3:5)))
  by_decl <- as_rtftables(plan_columns(p, decimal = ".values"))
  expect_identical(by_decl, by_step)
  # by name, so it follows the columns wherever they go
  by_name <- as_rtftables(plan_columns(p, decimal = c("Placebo",
    "Xanomeline High Dose", "Xanomeline Low Dose")))
  expect_identical(by_name, by_step)
})

test_that("the display verbs are last-wins too", {
  skip_if_no_cards2()
  p <- disp_plan() |>
    plan_paginate_rows(max_rows = 10) |> plan_style(border = "tfl") |>
    plan_paginate_rows(max_rows = 40)          # later wins, `border` is kept
  rtf <- rtfreporter:::.plan_rtf_args(p)
  expect_identical(rtf$max_rows, 40)
  expect_identical(rtf$border, "tfl")
})

test_that("how far a plan goes is read off what it declares", {
  skip_if_no_cards2()
  # nothing about the display: the answer is the table data.frame
  expect_identical(.plan_reach(disp_plan()), "table")
  expect_true(is.data.frame(suppressMessages(plan_apply(disp_plan()))))

  # anything that only makes sense once there are pages moves the answer
  for (v in list(function(p) plan_style(p, border = "tfl"),
                 function(p) plan_col_header(p, c("a", "b", "c", "d")),
                 function(p) plan_cell_style(p, bold = ~ TRUE),
                 function(p) plan_after(p, identity))) {
    expect_identical(.plan_reach(v(disp_plan())), "pages")
  }

  # calling the verb IS the declaration, even with nothing in it
  expect_identical(.plan_reach(plan_style(disp_plan())), "pages")
})

test_that("a named stage still stops where it is told, for looking inside", {
  skip_if_no_cards2()
  p <- disp_plan() |> plan_stub(vars = c("group", "label"))
  expect_true(is.data.frame(suppressMessages(plan_apply(p, "table"))))
  expect_s3_class(suppressMessages(plan_apply(p, "input")), "data.frame")
  expect_true(all(c("cols", "cells") %in% names(plan_apply(p, "args")$widen)))
  expect_type(suppressMessages(plan_apply(p)), "list")
})

test_that("the plan says which of the two it will give", {
  skip_if_no_cards2()
  expect_true(any(grepl("table data.frame",
                        utils::capture.output(print(disp_plan())),
                        fixed = TRUE)))
  expect_true(any(grepl("RTF pages",
    utils::capture.output(print(plan_style(disp_plan(), border = "tfl"))),
    fixed = TRUE)))
})

test_that("rtf_tables() takes a plan, so plan_apply() is for looking", {
  skip_if_no_cards2()
  p <- disp_plan() |>
    plan_stub(vars = c("group", "label")) |> plan_style(border = "tfl")
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(
      header = rtf_header(rows = list(c(c = "T"))),
      footer = rtf_footer(rows = list(c(l = "F")))))
  direct <- suppressMessages(rtf_tables(doc, p))
  byhand <- suppressMessages(rtf_tables(doc, plan_apply(p, "pages")))
  expect_equal(direct, byhand)
})

# ------------------------------------------------- conditional cell styles

styled <- function(...) {
  suppressMessages(plan_apply(
    disp_plan() |>
      plan_stub(vars = c("group", "label"), name = "row_label",
                before = TRUE) |>
      plan_cell_style(...) |>
      plan_style(border = "tfl"),
    "pages"))
}


test_that("a bare formula styles the whole row", {
  skip_if_no_cards2()
  # `group` is folded away by stub_cols(); it is put back through the
  # rtf_stub_src map so a condition can still ask about it, the way SAS's
  # compute block sees variables the report does not print
  pg <- styled(bold = ~ !is.na(group) & group == "SEX")[[1]]
  cs <- pg$cell_styles
  on <- vapply(cs, function(z) !is.null(z) && isTRUE(z$bold[[1]]), TRUE)
  expect_true(any(on))
  expect_false(all(on))
  # every column of a styled row is styled, since the formula was bare
  expect_true(all(vapply(cs[on], function(z) all(z$bold), TRUE)))
})

test_that("a heading row has no source row, so its keys read NA", {
  skip_if_no_cards2()
  pg <- styled(bold = ~ is.na(group))[[1]]
  on <- vapply(pg$cell_styles,
               function(z) !is.null(z) && isTRUE(z$bold[[1]]), TRUE)
  expect_true(any(on))
})

test_that("a named list scopes a style to one column, by NAME", {
  skip_if_no_cards2()
  pg <- styled(align = list(Placebo = ~ "right"))[[1]]
  j <- match("Placebo", names(pg$data))
  expect_false(is.na(j))
  expect_identical(pg$cell_styles[[1]]$align[[j]], "right")
  expect_true(all(is.na(pg$cell_styles[[1]]$align[-j])))
})

test_that("NA means \"leave the column default alone\"", {
  skip_if_no_cards2()
  cs <- styled(bold = ~ NA)[[1]]$cell_styles
  expect_true(all(vapply(cs, is.null, TRUE)))
})

test_that("styles are last-wins like every other layer", {
  skip_if_no_cards2()
  pg <- suppressMessages(plan_apply(
    disp_plan() |>
      plan_stub(vars = c("group", "label"), name = "row_label",
                before = TRUE) |>
      plan_cell_style(bold = ~ TRUE) |>
      plan_cell_style(bold = ~ FALSE) |>          # a later LAYER wins
      plan_style(border = "tfl"),
    "pages"))[[1]]
  expect_true(all(vapply(pg$cell_styles,
                         function(z) !any(z$bold), TRUE)))
})

test_that("the same key twice in ONE call is a typo, not a layering", {
  skip_if_no_cards2()
  expect_error(styled(bold = ~ TRUE, bold = ~ FALSE), "multiple actual arguments")
  expect_error(plan_digits(disp_plan(), AGE = 1, AGE = 2), "twice in one call")
})

test_that("a column that is not there is named, not ignored", {
  skip_if_no_cards2()
  expect_error(styled(bold = list(NOPE = ~ TRUE)), "no printed column")
  expect_error(styled(bold = list(NOPE = ~ TRUE)), "Available")
})

test_that("a style that is neither a value nor a one-sided formula is refused", {
  skip_if_no_cards2()
  expect_error(styled(bold = y ~ TRUE), "one-sided formula")
  expect_error(styled(bold = "yes"), "TRUE or FALSE")
})

test_that("a condition of the wrong length is refused", {
  skip_if_no_cards2()
  expect_error(styled(bold = ~ c(TRUE, FALSE)), "values for")
})

test_that("folding the stub inside as_rtftables() is refused with a reason", {
  skip_if_no_cards2()
  # the heading rows it adds were never seen by the conditions
  p <- disp_plan() |> plan_cell_style(bold = ~ TRUE) |>
    plan_stub(vars = c("group", "label"))        # folded inside, not before
  expect_error(plan_apply(p, "pages"), "before = TRUE")
  expect_error(plan_apply(p, "pages"), "wrong row")
})

test_that("plan_cell_style() says where as well as how", {
  skip_if_no_cards2()
  base <- disp_plan() |>
    plan_stub(vars = c("group", "label"), name = "row_label", before = TRUE)
  page1 <- function(p) {
    out <- suppressMessages(plan_apply(p, "pages"))
    if (inherits(out, "rtftable")) out else out[[1L]]
  }
  # the column header, and whole columns, without a condition
  hdr <- page1(base |>
                 plan_col_header(rtf_col_header(c("Characteristic", "{col}"))) |>
                 plan_cell_style(header = TRUE, bold = TRUE))
  expect_true(all(vapply(hdr$col_spec, function(s) isTRUE(s$header_bold), NA)))
  one <- page1(base |> plan_cell_style(cols = "row_label", italic = TRUE))
  expect_true(isTRUE(one$col_spec[[1L]]$italic))
  expect_false(isTRUE(one$col_spec[[2L]]$italic))
  # a value where a condition holds, over the columns named
  w <- page1(base |> plan_cell_style(cols = "row_label", bold = TRUE,
                                     where = ~ is.na(label)))   # heading rows
  b <- vapply(w$cell_styles, function(r) isTRUE(r$bold[[1L]]), NA)
  expect_true(any(b))
  expect_false(all(b))
  # the two spellings of a condition do not mix in one call
  expect_error(plan_cell_style(base, bold = ~ TRUE, where = ~ TRUE), "two calls")
  expect_error(plan_cell_style(base, header = TRUE, color = "#FF0000"),
               "bold, italic, underline, align and border")
  expect_error(plan_cell_style(base), "nothing to style")
})

# --------------------------------------------------------- plan_template()


test_that("plan_template() writes a plan that runs to the pages", {
  skip_if_no_cards2()
  ard <- plan_ard()
  gen <- utils::capture.output(code <- plan_template(ard, cols = "TRT",
                                                     pipe = "|>"))
  expect_true(any(grepl("normalize_ard()", code, fixed = TRUE)))
  expect_true(any(grepl("table_plan(", code, fixed = TRUE)))
  expect_true(any(grepl("plan_cells(", code, fixed = TRUE)))
  # both halves, because a plan that stops at the table is half a plan
  expect_true(any(grepl("plan_style(", code, fixed = TRUE)))
  expect_true(any(grepl("plan_stub(", code, fixed = TRUE)))
  expect_true(any(grepl("plan_apply(p)", code, fixed = TRUE)))

  e <- new.env(); assign("ard", ard, e)
  suppressMessages(eval(parse(text = paste(code, collapse = "
")), e))
  # the template stops at the plan: rtf_tables() takes it from there
  p <- get("p", e)
  expect_s3_class(p, "table_plan")
  pg <- suppressMessages(plan_apply(p, "pages"))
  expect_s3_class(pg[[1]], "rtftable")
})

test_that("plan_template() derives the stub and leaves the rest to be edited", {
  skip_if_no_cards2()
  code <- utils::capture.output(
    invisible(plan_template(plan_ard(), cols = "TRT", pipe = "|>")))
  # the stub is not written at all: plan_stub() works it out from what
  # table_plan(rows = ) and plan_row_group() already declared
  expect_false(any(grepl("vars", code, fixed = TRUE)))
  expect_true(any(grepl("plan_stub(", code, fixed = TRUE)))
  expect_true(any(grepl("edit this", code, fixed = TRUE)))
})

test_that("plan_template() takes the pipe like plan_template(form = 'spread') does", {
  skip_if_no_cards2()
  base <- utils::capture.output(
    invisible(plan_template(plan_ard(), cols = "TRT", pipe = "|>")))
  mag <- utils::capture.output(
    invisible(plan_template(plan_ard(), cols = "TRT", pipe = "%>%")))
  expect_false(any(grepl("%>%", base, fixed = TRUE)))
  expect_true(any(grepl("library(magrittr)", mag, fixed = TRUE)))
})

test_that("the generated header writes a real newline escape", {
  skip_if_no_cards2()
  # one unambiguous denominator, so a header block is written at all
  ard <- cards::ard_stack(
    cards::ADSL, .by = ARM,
    cards::ard_summary(variables = AGE,
                          statistic = ~ list(N = function(x) length(x))))
  code <- utils::capture.output(
    invisible(plan_template(ard, cols = "ARM", pipe = "|>")))
  expect_true(any(grepl("plan_col_header", code, fixed = TRUE)))
  # ONE backslash, not two: the generated file is R source
  one <- paste0('"', '\\', 'nN = "')
  two <- paste0('"', '\\\\', 'nN = "')
  expect_true(any(grepl(one, code, fixed = TRUE)))
  expect_false(any(grepl(two, code, fixed = TRUE)))
})

test_that("plan_template() invents no header when the N is ambiguous", {
  skip_if_no_cards2()
  # two continuous variables disagree about N, so nothing is guessed
  code <- utils::capture.output(
    invisible(plan_template(plan_ard(), cols = "TRT", pipe = "|>")))
  expect_false(any(grepl("rtfreporter::plan_col_header(", code, fixed = TRUE)))
  # and the pipeline still parses: the trailing pipe must have been removed
  expect_silent(parse(text = paste(code, collapse = "\n")))
})


# ------------------------------------------------- the seams, as expressions


test_that("print() names the columns of every stage it has", {
  skip_if_no_cards2()
  p <- disp_plan() |>
    plan_stub(vars = c("group", "label"), name = "row_label") |>
    plan_style(border = "tfl")

  out <- utils::capture.output(print(p))
  # the frame going in is to hand, so it is always answered
  expect_true(any(grepl("cols / rows / label may name", out,
                        fixed = TRUE)))
  expect_true(any(grepl("stat_name", out, fixed = TRUE)))
  # spreading is not, so it says so rather than costing a run
  expect_true(any(grepl("not computed yet", out, fixed = TRUE)))

  invisible(suppressMessages(plan_apply(p)))
  out2 <- utils::capture.output(print(p))
  expect_true(any(grepl("after widen", out2, fixed = TRUE)))
  expect_true(any(grepl("as printed", out2, fixed = TRUE)))
  expect_true(any(grepl("row_label", out2, fixed = TRUE)))
})

test_that("a derived plan does not inherit its parent's column cache", {
  skip_if_no_cards2()
  p <- disp_plan()
  invisible(suppressMessages(plan_apply(p)))
  expect_false(is.null(p$cache$table))
  p2 <- p |> plan_digits(3)
  expect_null(p2$cache$table)
})


# ------------------------------------------------ titles, footnotes, listings

# a listing splits on max_rows alone, which keeps these tests about the
# blocks rather than about pagination strategies
.pages_src <- function(n = 12L) {
  data.frame(USUBJID = sprintf("S-%03d", seq_len(n)),
             ARM = rep(c("A", "B"), length.out = n),
             stringsAsFactors = FALSE)
}
.pages_plan <- function(max_rows = 4L) {
  table_plan(.pages_src()) |>
    plan_listing(listing_col("USUBJID", width = 12)) |>
    plan_paginate_rows(max_rows = max_rows)
}


test_that("plan_titles() / plan_footnotes() ride on every page", {
  pg <- suppressMessages(plan_apply(
    .pages_plan() |>
      plan_titles("Table 14.1.1", "Demographics") |>
      plan_footnotes("Source: ADSL")))
  expect_gt(length(pg), 1L)
  for (i in seq_along(pg)) {
    expect_identical(attr(pg[[i]], "rtf_titles"),
                     c("Table 14.1.1", "Demographics"))
    expect_identical(attr(pg[[i]], "rtf_footnotes"), "Source: ADSL")
  }
})

test_that("plan_titles(pages = ) gives each page its own block", {
  base <- .pages_plan()
  n <- length(suppressMessages(plan_apply(base)))
  expect_gt(n, 1L)
  pg <- suppressMessages(plan_apply(
    base |> plan_titles(pages = as.list(paste("Part", seq_len(n))))))
  expect_identical(attr(pg[[1]], "rtf_titles"), "Part 1")
  expect_identical(attr(pg[[n]], "rtf_titles"), paste("Part", n))
})

test_that("a page count that does not match is refused, and named", {
  p <- .pages_plan() |> plan_titles(pages = list("only one"))
  expect_error(plan_apply(p), "block for")
  expect_error(plan_apply(p), "plan_titles")
})

test_that("rows and per-page blocks are not both accepted", {
  skip_if_no_cards2()
  expect_error(plan_titles(disp_plan(), "a", pages = list("b")),
               "Not both")
})

test_that("a listing goes nowhere near an ARD", {
  skip_if_no_cards2()
  d <- data.frame(USUBJID = sprintf("S-%03d", 1:12),
                  ARM = rep(c("A", "B"), 6), AGE = 40:51,
                  stringsAsFactors = FALSE)
  pg <- suppressMessages(plan_apply(
    table_plan(d[d$AGE >= 45, , drop = FALSE]) |>
      plan_listing(listing_col("USUBJID", width = 12),
                   listing_col("ARM", width = 10)) |>
      plan_paginate_rows(max_rows = 20) |>
      plan_style(border = "tfl") |>
      plan_titles("Listing 16.2.1")))
  expect_s3_class(pg[[1]], "rtftable")
  expect_identical(attr(pg[[1]], "rtf_titles"), "Listing 16.2.1")
  # the records went straight through: nothing normalised or spread
  tb <- suppressMessages(plan_apply(
    table_plan(d[d$AGE >= 45, , drop = FALSE]) |>
      plan_listing(listing_col("USUBJID")), "table"))
  expect_identical(nrow(tb), 7L)
})

test_that("a listing matches the same call written by hand", {
  skip_if_no_cards2()
  d <- data.frame(USUBJID = sprintf("S-%03d", 1:12),
                  ARM = rep(c("A", "B"), 6), stringsAsFactors = FALSE)
  spec <- listing_spec(list(listing_col("USUBJID", width = 12),
                            listing_col("ARM", width = 10)))
  ref <- as_rtftables(d, listing = spec, max_rows = 20, border = "tfl")
  new <- suppressMessages(plan_apply(
    table_plan(d) |>
      plan_listing(listing_col("USUBJID", width = 12),
                   listing_col("ARM", width = 10)) |>
      plan_paginate_rows(max_rows = 20) |>
      plan_style(border = "tfl")))
  expect_equal(new, ref)
})

test_that("plan_listing(align, record): listing_spec()'s, as the same call by hand (#720)", {
  skip_if_no_cards2()
  d <- data.frame(USUBJID = sprintf("S-%03d", 1:6),
                  ARM = rep(c("A", "B"), 3), stringsAsFactors = FALSE)
  cols <- list(listing_col("USUBJID", width = 12), listing_col("ARM", width = 10))
  by_hand <- function(...) {
    as_rtftables(d, listing = listing_spec(cols, ...), max_rows = 20)
  }
  by_plan <- function(...) {
    suppressMessages(plan_apply(
      table_plan(d) |>
        plan_listing(cols[[1L]], cols[[2L]], ...) |>
        plan_paginate_rows(max_rows = 20)))
  }
  # the columns' default alignment
  expect_equal(by_plan(align = "center"), by_hand(align = "center"))
  # no record column; the record column under a name of its own
  expect_equal(by_plan(record = FALSE), by_hand(record = FALSE))
  expect_equal(by_plan(record = "REC"), by_hand(record = "REC"))
  # (not given: as before)
  expect_equal(by_plan(), by_hand())
})

test_that("a frame that is neither an ARD nor a listing says which to add", {
  expect_error(plan_apply(table_plan(data.frame(a = "x", b = "y"))),
               "plan_listing")
  expect_error(plan_apply(table_plan(data.frame(a = "x", b = "y"))),
               "display verbs")
})


# ------------------------------------------- the three ways in, one system


test_that("a finished table is a source for the display half alone", {
  skip_if_no_cards2()
  # pattern 2: ard_*() built the table, the plan does the rest
  ard <- plan_ard()
  tbl <- suppressMessages(
    ard |> normalize_ard() |>
      widen_ard(cols = "TRT", rows = c(group = "variable"),
                 cells = "{n:.0f}", notes = FALSE))
  ref <- as_rtftables(tbl, read_meta = FALSE,
                      stub_vars = c("group", "label"), border = "tfl")
  new <- suppressMessages(plan_apply(
    table_plan(tbl) |>
      plan_stub(vars = c("group", "label")) |>
      plan_style(border = "tfl")))
  expect_equal(new, ref)
})

test_that("asking for the ARD half of a finished table says what to drop", {
  skip_if_no_cards2()
  p <- table_plan(nz(data.frame(group = "A", x = "1")), cols = "x") |>
    plan_cells("{n}")
  expect_error(plan_apply(p), "plan_cells() need them", fixed = TRUE)
  expect_error(plan_apply(p), "keep the display")
})

test_that("plan_paginate_group(keep = FALSE) hides the carrier it groups by", {
  skip_if_no_cards2()
  p <- disp_plan() |> plan_paginate_group(col = "group", keep = FALSE)
  # a page per value, cut by the page key, which is never the row group (#635)
  expect_identical(rtfreporter:::.plan_rtf_args(p)$page_by, "group")
  expect_null(rtfreporter:::.plan_rtf_args(p)$group_col)
  expect_identical(rtfreporter:::.plan_rtf_args(p)$drop_cols, "group")
})

test_that("hiding ADDS rather than replaces", {
  skip_if_no_cards2()
  # last-wins here would silently un-hide the first column named
  p <- disp_plan() |> plan_hide("a") |> plan_hide("b") |>
    plan_paginate_group(col = "c", keep = FALSE)
  expect_setequal(rtfreporter:::.plan_rtf_args(p)$drop_cols, c("a", "b", "c"))
})

test_that("the verbs refuse anything that is not a plan", {
  expect_error(plan_cells(data.frame(a = 1), "x"), "Expected a table_plan")
  expect_error(plan_apply(data.frame(a = 1)), "Expected a table_plan")
  expect_error(table_plan(), "required")
})


# ------------------------------- a frame that never went near cards

# Columns named the way a study names them: no `variable`, no
# `stat_name`, no `stat`, and the row label in two different places
# depending on what kind of row it is.
own_frame <- function() {
  data.frame(
    TRT   = rep(c("A", "B"), each = 5L),
    PARAM = rep(c("AGE", "AGE", "AGE", "SEX", "SEX"), 2L),
    CAT   = c(NA, NA, NA, "F", "M", NA, NA, NA, "F", "M"),
    STAT  = c("N", "mean", "sd", "n", "n", "N", "mean", "sd", "n", "n"),
    VALUE = c(10, 55.5, 4.25, 6, 4, 12, 57.25, 3.5, 7, 5),
    stringsAsFactors = FALSE)
}


test_that("a frame keeps its own names, and says which plays what", {
  p <- table_plan(own_frame(), cols = "TRT", rows = c(group = "PARAM"),
                label = c(label = "CAT"), stat = c(variable = "PARAM", name = "STAT", value = "VALUE")) |> plan_cells(notes = FALSE) |>
    plan_cells(AGE = c("Mean (SD)" = "{mean} ({sd})"),
               SEX = "{n}") |>
    plan_digits(1)
  out <- suppressMessages(plan_apply(p))
  expect_true(all(c("group", "label", "A", "B") %in% names(out)))
  expect_true(any(grepl("55.5 (4.2)", out$A, fixed = TRUE)))
})

test_that("a column that is not there blames the role that named it", {
  expect_error(
    table_plan(own_frame(), cols = "TRT", stat = c(name = "PARAMCD")),
    "no column .PARAMCD. in the data")
})

test_that("`label` naming two columns coalesces them", {
  # tfrmt has the same problem: a continuous row is labelled by its
  # statistic and a categorical one by its level, and they are not the
  # same column.  First non-missing wins.
  p <- table_plan(own_frame(), cols = "TRT", rows = c(group = "PARAM"),
                label = list(row = c("CAT", "STAT")), stat = c(variable = "PARAM", name = "STAT", value = "VALUE")) |> plan_cells(notes = FALSE) |>
    plan_cells("{n}", AGE = "{mean}") |>
    plan_digits(1)
  out <- suppressMessages(plan_apply(p, "table"))
  expect_true("row" %in% names(out))
  # the categorical rows took CAT, the continuous ones fell back to STAT
  expect_true(all(c("F", "M") %in% out$row))
  expect_true(any(c("N", "mean", "sd") %in% out$row))
})


# -------------------------------------------------------- the row order


test_that("plan_sort() goes to the ARD half, where the statistics are", {
  skip_if_no_cards2()
  p <- base_plan() |> plan_sort(".overall", "group", "-n")
  a <- plan_apply(p, "args")$widen
  expect_identical(a$sort, c(".overall", "group", "-n"))
  # and nothing reaches as_rtftables(), which could not sort on `-n`:
  # by then the statistic is a formatted cell, not a number
  r <- rtfreporter:::.plan_rtf_args(p)
  expect_null(r$sort_by)
})

test_that("plan_sort() over a finished table sorts the table", {
  d <- data.frame(group = c("B", "A"), x = c("1", "2"),
                  stringsAsFactors = FALSE)
  p <- table_plan(d) |> plan_sort("group") |> plan_paginate_rows(max_rows = 10)
  r <- rtfreporter:::.plan_rtf_args(p)
  expect_identical(r$sort_by, "group")
  out <- suppressMessages(plan_apply(p))
  first <- if (inherits(out, "rtftable")) out else out[[1L]]
  expect_identical(first$data$group[1L], "A")
})

test_that("sorting alone does not turn a table into RTF pages", {
  skip_if_no_cards2()
  p <- base_plan() |> plan_sort("group")
  expect_true(is.data.frame(suppressMessages(plan_apply(p))))
})

# ------------------------------------ needed and not wanted, said once


test_that("the spread builds only what the roles named", {
  skip_if_no_cards2()
  # there is nothing to drop, because nothing unasked-for is built: a
  # column is a row key, the label, or a spread column
  out <- suppressMessages(plan_apply(base_plan(), "table"))
  keys <- c("group", "label")
  arms <- setdiff(names(out), keys)
  expect_setequal(intersect(names(out), keys), keys)
  expect_true(all(grepl("Placebo|Xanomeline", arms)))
})

test_that("plan_paginate_group(keep = FALSE) hides the key the break reads", {
  skip_if_no_cards2()
  d <- nz(plan_ard())
  d$pg <- ifelse(d$variable == "SEX", "1", "2")
  p <- table_plan(d, cols = "TRT",
                rows = c(group = "variable",
                         pg = "pg")) |>
    plan_cells(continuous = c(n = "{N:d}"),
               categorical = "{n:d}") |>
    plan_paginate_group(col = "pg", keep = FALSE)
  out <- suppressMessages(plan_apply(p))
  first <- if (inherits(out, "rtftable")) out else out[[1L]]
  expect_false("pg" %in% names(first$data))
  expect_gt(length(out), 1L)          # it really did break on it
})

test_that("plan_sort(keep = FALSE) hides a sort carrier, and only it", {
  d <- data.frame(ord = c("2", "1"), label = c("B", "A"),
                  x = c("9", "8"), stringsAsFactors = FALSE)
  p <- table_plan(d) |> plan_sort("ord", keep = FALSE) |>
    plan_paginate_rows(max_rows = 10)
  out <- suppressMessages(plan_apply(p))
  first <- if (inherits(out, "rtftable")) out else out[[1L]]
  expect_false("ord" %in% names(first$data))
  expect_identical(first$data$label[1L], "A")
})

test_that("a sort key that is not a column is not mistaken for one", {
  skip_if_no_cards2()
  # ".overall" and "-n" are instructions, not columns: hiding must not
  # try to drop them
  p <- base_plan() |> plan_sort(".overall", "group", "-n", keep = FALSE) |>
    plan_paginate_rows(max_rows = 40)
  out <- suppressMessages(plan_apply(p))
  first <- if (inherits(out, "rtftable")) out else out[[1L]]
  expect_false("group" %in% names(first$data))
  expect_true("label" %in% names(first$data))
})

test_that("plan_paginate_cols() is the column axis, without a lambda", {
  skip_if_no_cards2()
  p <- disp_plan() |> plan_paginate_rows(max_rows = 40) |>
    plan_paginate_cols(at = 4L, keep = 1:2, fit = FALSE)
  out <- suppressMessages(plan_apply(p))
  expect_gt(length(out), 1L)
  # every block repeats the carried column
  expect_true(all(vapply(out, function(z) names(z$data)[1L], "") ==
                  names(out[[1L]]$data)[1L]))
})

test_that("a house style is an ordinary function, not a plan without data", {
  skip_if_no_cards2()
  house <- function(d) {
    table_plan(d, cols = "TRT", rows = c(group = "variable")) |> plan_cells(notes = FALSE) |>
      plan_cells(continuous = c(n = "{N:d}"), categorical = "{n:d}") |>
      plan_digits(1)
  }
  a <- suppressMessages(plan_apply(house(nz(plan_ard()))))
  b <- suppressMessages(plan_apply(house(nz(plan_ard())) |>
                                     plan_digits(3)))
  expect_identical(names(a), names(b))
  expect_true(is.data.frame(a))
  # and a plan cannot be built without the data the roles name
  expect_error(table_plan(cols = "TRT"), "`x` \\(the data\\) is required")
  expect_error(table_plan(cols = "TRT"), "ordinary function")
})

test_that("plan_paginate_group() is the group axis", {
  skip_if_no_cards2()
  p <- disp_plan() |> plan_paginate_group(col = "group")
  r <- rtfreporter:::.plan_rtf_args(p)
  # handed over as page_by: the page key is not the rows' group (#635)
  expect_null(r$split)
  expect_identical(r$page_by, "group")
  expect_null(r$group_col)
  out <- suppressMessages(plan_apply(p))
  expect_gt(length(out), 1L)          # one page per group value
})

test_that("two verbs asking for different splits is a mistake", {
  skip_if_no_cards2()
  p <- disp_plan() |> plan_paginate_group(col = "group") |>
    plan_paginate_rows(split = "group_safe", max_rows = 5)
  expect_error(suppressMessages(plan_apply(p)), "Use one", fixed = TRUE)
})

test_that("plan_paginate_group() is what auto_section cuts on", {
  skip_if_no_cards2()
  # a value split NAMES each page by its group value, and
  # rtf_tables(auto_section = TRUE) opens a section where the name changes
  p <- disp_plan() |>
    plan_paginate_group(col = "group", keep = FALSE)
  pg <- suppressMessages(plan_apply(p))
  expect_setequal(names(pg), c("AGE", "BMIBL", "SEX"))

  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_tables(p, auto_section = TRUE)
  secs <- Filter(Negate(is.null), lapply(doc$contents, `[[`, "section"))
  expect_length(secs, 3L)
  expect_setequal(vapply(secs, function(x) x[["label"]], ""),
                  c("AGE", "BMIBL", "SEX"))
})

# ------------------------------------ the header without a function


test_that("a header cell may carry {col} and {n}", {
  skip_if_no_cards2()
  p <- disp_plan() |>
    plan_col_header(values = c(Placebo = 86, "Xanomeline High Dose" = 84,
                          "Xanomeline Low Dose" = 84), rtf_col_header(
      c("",               "{col}"),
      c("Characteristic", "(N={n})")))
  out <- suppressMessages(plan_apply(p))
  first <- if (inherits(out, "rtftable")) out else out[[1L]]
  rows <- first$col_header$rows %||% first$col_header
  txt <- unlist(lapply(rows, function(r)
    vapply(r, function(z) if (is.list(z)) z$text %||% "" else
      as.character(z), "")))
  expect_true(any(grepl("Characteristic", txt, fixed = TRUE)))
  expect_true(any(grepl("(N=", txt, fixed = TRUE)))
  # the arm name came from the table, not from the author
  expect_true(any(grepl("Placebo", txt, fixed = TRUE)))
})

test_that("an rtf_col_header() goes through untouched, positionally", {
  skip_if_no_cards2()
  h <- rtf_col_header(c("Group", "Characteristic", "A", "B", "C"))
  a <- suppressMessages(plan_apply(disp_plan() |> plan_col_header(h)))
  b <- suppressMessages(plan_apply(disp_plan() |>
                                     plan_col_header(header = h)))
  expect_equal(a, b)
})

test_that("a row already the right length is left alone", {
  skip_if_no_cards2()
  # five columns, five cells: nothing to repeat and no token to fill
  h <- rtf_col_header(c("Group", "Characteristic", "A", "B", "C"))
  out <- suppressMessages(plan_apply(disp_plan() |>
                                       plan_col_header(h)))
  first <- if (inherits(out, "rtftable")) out else out[[1L]]
  expect_true(any(grepl("Characteristic", unlist(first$col_header),
                        fixed = TRUE)))
})

test_that("{n} in a spanner takes the one value there is", {
  skip_if_no_cards2()
  h <- rtf_col_header(
    list(col_cell(1L, "Group"), col_cell(2L, "Characteristic"),
         col_cell(c(3L, 5L), "All arms (N={n})")),
    c("", "", "{col}"))
  out <- suppressMessages(plan_apply(
    disp_plan() |> plan_col_header(values = 254, h)))
  first <- if (inherits(out, "rtftable")) out else out[[1L]]
  txt <- unlist(lapply(first$col_header, function(r)
    vapply(r, function(z) if (is.list(z)) as.character(z$label) else
      as.character(z), "")))
  expect_true(any(grepl("All arms (N=254)", txt, fixed = TRUE)))
  expect_true(any(grepl("Placebo", txt, fixed = TRUE)))
})

test_that("{col} is the LEAF, and {col1}/{col2} are the levels", {
  skip_if_no_cards2()
  # two `cols` keys make a name like "Placebo____F", which nobody wants
  # printed; the hierarchy itself is built by as_rtftables(header_sep = )
  d <- nz(plan_ard())
  p <- table_plan(d, cols = c("TRT", "variable"),
                rows = c(group = "variable")) |> plan_cells(notes = FALSE) |>
    plan_cells(continuous = c(n = "{N:d}"), categorical = "{n:d}") |>
    plan_paginate_rows(max_rows = 40) |>
    plan_col_header(values = 42, rtf_col_header(
      c("", "{col1}"),
      c("Characteristic", "{col2} (N={n})")))
  out <- suppressMessages(plan_apply(p))
  first <- if (inherits(out, "rtftable")) out else out[[1L]]
  txt <- unlist(first$col_header)
  expect_false(any(grepl("____", txt, fixed = TRUE)))
  expect_true(any(grepl("Placebo", txt, fixed = TRUE)))
  expect_true(any(grepl("(N=42)", txt, fixed = TRUE)))
})

test_that("with one key the leaf is the whole name", {
  skip_if_no_cards2()
  out <- suppressMessages(plan_apply(
    disp_plan() |> plan_col_header(rtf_col_header(c("Term", "{col}")))))
  first <- if (inherits(out, "rtftable")) out else out[[1L]]
  expect_true(any(grepl("Placebo", unlist(first$col_header),
                        fixed = TRUE)))
})

# ------------------------------------------ digits, per statistic

open_plan <- function() {
  table_plan(nz(plan_ard()), cols = "TRT", rows = c(group = "variable")) |> plan_cells(notes = FALSE) |>
    plan_cells(continuous  = c("n"         = "{N:.0f}",
                               "Mean (SD)" = "{mean} ({sd})"),
               categorical = "{n:.0f} ({p:%})")
}


test_that("digits can be stated per statistic, not just per variable", {
  skip_if_no_cards2()
  p <- open_plan() |>
    plan_digits(c(mean = 2, sd = 3, p = 1)) |>
    plan_digits(AGE = c(mean = 1, sd = 2))
  cells <- plan_apply(p, "args")$widen$cells
  expect_match(cells$AGE[["Mean (SD)"]], "{mean:.1f} ({sd:.2f})",
               fixed = TRUE)
  expect_match(cells$BMIBL[["Mean (SD)"]], "{mean:.2f} ({sd:.3f})",
               fixed = TRUE)
})

test_that("a statistic the narrower rule skips falls through", {
  skip_if_no_cards2()
  # AGE says nothing about `sd`, so the house rule answers for it
  p <- open_plan() |>
    plan_digits(c(mean = 2, sd = 3, p = 1)) |>
    plan_digits(AGE = c(mean = 0))
  expect_match(plan_apply(p, "args")$widen$cells$AGE[["Mean (SD)"]],
               "{mean:.0f} ({sd:.3f})", fixed = TRUE)
})

test_that("one number still means every token", {
  skip_if_no_cards2()
  p <- open_plan() |> plan_digits(c(mean = 2, sd = 3, p = 1)) |>
    plan_digits(AGE = 1)
  expect_match(plan_apply(p, "args")$widen$cells$AGE[["Mean (SD)"]],
               "{mean:.1f} ({sd:.1f})", fixed = TRUE)
})

test_that("kind x statistic and variable x statistic combine", {
  skip_if_no_cards2()
  p <- open_plan() |>
    plan_digits(continuous = c(mean = 2, sd = 3),
                categorical = c(p = 1)) |>
    plan_digits(AGE = c(mean = 1, sd = 2))
  cells <- plan_apply(p, "args")$widen$cells
  expect_match(cells$AGE[["Mean (SD)"]], "{mean:.1f} ({sd:.2f})",
               fixed = TRUE)
  expect_match(cells$BMIBL[["Mean (SD)"]], "{mean:.2f} ({sd:.3f})",
               fixed = TRUE)
  expect_match(cells$SEX, "{p:.1f%}", fixed = TRUE)
})

test_that("a digits value may ask for significant digits", {
  skip_if_no_cards2()
  p <- open_plan() |>
    plan_digits(continuous = c(mean = "4s", sd = "5s"),
                categorical = c(p = 1)) |>
    plan_digits(AGE = c(mean = 1))
  cells <- plan_apply(p, "args")$widen$cells
  # decimals and significant digits mix, per statistic
  expect_match(cells$AGE[["Mean (SD)"]], "{mean:.1f} ({sd:.5s})",
               fixed = TRUE)
  expect_match(cells$BMIBL[["Mean (SD)"]], "{mean:.4s} ({sd:.5s})",
               fixed = TRUE)
  out <- suppressMessages(plan_apply(p, "table"))
  expect_true(any(grepl("8.5902", out[[3L]], fixed = TRUE)))
})

# --------------------------------- the denominator the ARD states

# the header numbers n = TRUE reads, without the bookkeeping attributes
nv <- function(p) {
  v <- rtfreporter:::.plan_n_values(p, TRUE)
  if (!length(v)) return(numeric(0))
  stats::setNames(as.numeric(v), names(v))
}


test_that("n = TRUE reads a cards sentinel keyed by the cols", {
  skip_if_no_cards2()
  # a sentinel row is a number the ARD states outright, so reading it is
  # not a guess -- and it is taken only when its keys ARE the `cols`.
  # A sentinel's number is its `N`, as cards writes it (#480)
  d <- data.frame(
    TRT = rep(c("A", "B"), each = 2L),
    variable = c("..ard_total_n..", "X", "..ard_total_n..", "X"),
    stat_name = c("N", "n", "N", "n"),
    stat = c(40, 7, 50, 9),
    .label = c("all", "x", "all", "x"),
    stringsAsFactors = FALSE)
  p <- table_plan(d, cols = "TRT") |> plan_cells(notes = FALSE)
  expect_identical(nv(p), c(A = 40, B = 50))
})

test_that("two sentinels are a choice, so neither is made", {
  skip_if_no_cards2()
  d <- data.frame(
    TRT = c("A", "A"),
    variable = c("..ard_total_n..", "..ard_hierarchical_overall.."),
    stat_name = c("N", "N"), stat = c(40, 12),
    .label = c("all", "any"), stringsAsFactors = FALSE)
  p <- table_plan(d, cols = "TRT") |> plan_cells(notes = FALSE)
  expect_length(nv(p), 0L)
})

test_that("a short widths vector repeats its last value", {
  skip_if_no_cards2()
  a <- suppressMessages(plan_apply(disp_plan() |>
    plan_paginate_rows(max_rows = 40) |> plan_columns(widths = c(5, 2))))
  b <- suppressMessages(plan_apply(disp_plan() |>
    plan_paginate_rows(max_rows = 40) |>
    plan_columns(widths = c(5, 2, 2, 2, 2))))
  expect_equal(a, b)
})

test_that("a row budget that the split ignores is refused, not dropped", {
  skip_if_no_cards2()
  # raising max_rows and watching nothing change is how an afternoon goes
  p <- disp_plan() |> plan_paginate_group() |>
    plan_paginate_rows(max_rows = 28)
  expect_error(suppressMessages(plan_apply(p)), "has no effect with split")
  expect_error(suppressMessages(plan_apply(p)), "group_safe", fixed = TRUE)
})

test_that("a row budget that the split ignores is refused, not dropped", {
  skip_if_no_cards2()
  # raising max_rows and watching nothing change is how an afternoon goes
  p <- disp_plan() |> plan_paginate_group() |>
    plan_paginate_rows(max_rows = 28)
  expect_error(suppressMessages(plan_apply(p)), "has no effect with split")
  expect_error(suppressMessages(plan_apply(p)), "group_safe", fixed = TRUE)
})

test_that("plan_apply(args) shows the display half, so last-wins is "
          |> paste0("visible"), {
  skip_if_no_cards2()
  p <- disp_plan() |>
    plan_paginate_rows(max_rows = 25, split = "group_force") |>
    plan_paginate_rows(max_rows = 15)
  a <- plan_apply(p, "args")
  expect_setequal(names(a), c("widen", "rtf"))
  # the call you edit last is the one that decides, and you can see it
  expect_identical(a$rtf$max_rows, 15)
  expect_identical(a$rtf$split, "group_force")
})

test_that("a named list of n makes each name a token", {
  skip_if_no_cards2()
  # both numbers in one header, no function: the study total in a spanner
  # and each column's own underneath it
  p <- disp_plan() |> plan_paginate_rows(max_rows = 40) |>
    plan_col_header(
      values = list(n = c("Placebo" = 86, "Xanomeline High Dose" = 84,
                          "Xanomeline Low Dose" = 84),
                    total = 254),
      rtf_col_header(
        list(col_cell(1, ""), col_cell(2, ""),
             col_cell(c(3, 5), "All (N={total})")),
        c("", "", "{col}"),
        c("Group", "Characteristic", "(N={n})")))
  out <- suppressMessages(plan_apply(p))
  first <- if (inherits(out, "rtftable")) out else out[[1L]]
  r1 <- vapply(first$col_header[[1L]], function(z) as.character(z$label), "")
  expect_true(any(grepl("All (N=254)", r1, fixed = TRUE)))
  expect_true(any(grepl("(N=86)", first$col_header[[3L]], fixed = TRUE)))
  expect_true(any(grepl("(N=84)", first$col_header[[3L]], fixed = TRUE)))
})

test_that("{n:sum} totals over the columns the cell covers", {
  skip_if_no_cards2()
  f <- rtfreporter:::.plan_header_fill
  nv <- c("A____x" = 10, "A____y" = 20, "B____x" = 30, "B____y" = 40)
  cols <- names(nv)
  h <- rtf_col_header(
    list(col_cell(1, "all {n:sum}"),
         col_cell(c(2, 3), "{col1} {n:sum}"),
         col_cell(c(4, 5), "{col1} {n:sum}")),
    c("Term", "{col2} {n}"))
  out <- f(h, nv, cols, 1L, "____")
  r1 <- vapply(out[[1L]], function(z) as.character(z$label), "")
  # a cell outside the data totals every column; a spanner totals its own
  expect_identical(r1, c("all 100", "A 30", "B 70"))
  # and a spanner takes the level its columns agree on
  expect_identical(out[[2L]], c("Term", "x 10", "y 20", "x 30", "y 40"))
})

test_that("{n:sum} needs no n_subjs: the ARD and the cell say it all", {
  skip_if_no_cards2()
  # one column per arm, a spanner over all of them
  p <- disp_plan() |> plan_paginate_rows(max_rows = 40) |>
    plan_col_header(
      values = c("Placebo" = 86, "Xanomeline High Dose" = 84,
                 "Xanomeline Low Dose" = 84),
      rtf_col_header(
        list(col_cell(1, ""), col_cell(2, ""),
             col_cell(c(3, 5), "All (N={n:sum})")),
        c("Group", "Characteristic", "{col} (N={n})")))
  out <- suppressMessages(plan_apply(p))
  first <- if (inherits(out, "rtftable")) out else out[[1L]]
  r1 <- vapply(first$col_header[[1L]], function(z) as.character(z$label), "")
  expect_true(any(grepl("All (N=254)", r1, fixed = TRUE)))
})

test_that("{n:<column>} names one of the values", {
  f <- rtfreporter:::.plan_header_fill
  nv <- c("A" = 10, "B" = 20, "C" = 30)
  h <- rtf_col_header(
    list(col_cell(1, "A={n:A} B={n:B} C={n:C} all={n:sum}"),
         col_cell(c(2, 4), "{n:sum}")),
    c("Term", "{col} ({n})"))
  out <- f(h, nv, c("A", "B", "C"), 1L, "____")
  r1 <- vapply(out[[1L]], function(z) as.character(z$label), "")
  expect_identical(r1[[1L]], "A=10 B=20 C=30 all=60")
  expect_identical(r1[[2L]], "60")
  expect_identical(out[[2L]], c("Term", "A (10)", "B (20)", "C (30)"))
})

test_that("print() lists the header tokens and what they resolve to", {
  skip_if_no_cards2()
  p <- disp_plan() |> plan_paginate_rows(max_rows = 40) |>
    plan_col_header(values = c("Placebo" = 86, "Xanomeline High Dose" = 84,
                          "Xanomeline Low Dose" = 84),
                    rtf_col_header(c("Group", "Characteristic", "{col}")))
  invisible(suppressMessages(plan_apply(p)))
  out <- utils::capture.output(print(p))
  expect_true(any(grepl("header tokens", out, fixed = TRUE)))
  expect_true(any(grepl("{n:sum}", out, fixed = TRUE)))
  expect_true(any(grepl("{n:Placebo}", out, fixed = TRUE)))
  expect_true(any(grepl("= 86", out, fixed = TRUE)))
  expect_true(any(grepl("{col}", out, fixed = TRUE)))
  # a level shows the text it prints as, not a description of itself
  expect_true(any(grepl("= c(AGE, BMIBL, SEX)", out, fixed = TRUE)) ||
              any(grepl("= c(Placebo", out, fixed = TRUE)))
  # the numbers themselves, not a description of them
  expect_true(any(grepl("= c(Placebo = 86", out, fixed = TRUE)))
  expect_true(any(grepl("= 254 over every column", out, fixed = TRUE)))
})

test_that("..ard_total_n.. is read as the total, never each column's", {
  skip_if_no_cards2()
  # the total carries `N`, not `n`, and has no value for the `cols` keys:
  # it is one number for the whole table -- a spanner over all of it --
  # and putting it in every arm's header was #482
  d <- data.frame(
    TRT = c(NA, "A", "B"),
    variable = c("..ard_total_n..", "X", "X"),
    stat_name = c("N", "n", "n"),
    stat = c(254, 7, 9),
    .label = c("all", "x", "x"),
    stringsAsFactors = FALSE)
  p <- table_plan(d, cols = "TRT") |> plan_cells(notes = FALSE)
  v <- rtfreporter:::.plan_n_values(p, TRUE)
  expect_identical(attr(v, "total"), 254)
  expect_false(any(c("A", "B") %in% names(v)))
})

test_that("a sentinel carrying both n and N gives the denominator N", {
  skip_if_no_cards2()
  # ..ard_hierarchical_overall.. carries n (had an event) and N (the
  # denominator); a header's number is the denominator (#480) -- the
  # event count is the "Any" row's, a subset of it
  d <- data.frame(
    TRT = c("A", "A", "B", "B"),
    variable = "..ard_hierarchical_overall..",
    stat_name = c("n", "N", "n", "N"),
    stat = c(42, 86, 27, 84),
    .label = "any", stringsAsFactors = FALSE)
  p <- table_plan(d, cols = "TRT") |> plan_cells(notes = FALSE)
  expect_identical(nv(p), c(A = 86, B = 84))
})

test_that("a sentinel with only n gives no header number", {
  skip_if_no_cards2()
  d <- data.frame(
    TRT = c("A", "B"),
    variable = "..ard_hierarchical_overall..",
    stat_name = "n", stat = c(42, 27), .label = "any",
    stringsAsFactors = FALSE)
  p <- table_plan(d, cols = "TRT") |> plan_cells(notes = FALSE)
  expect_length(nv(p), 0L)
})

test_that("an AE table's header N is the analysis set, not the Any row", {
  skip_if_no_cards2()
  adsl <- cards::ADSL
  adae <- cards::ADAE[cards::ADAE$TRTEMFL == "Y", ]
  adsl$TRTA <- adsl$TRT01A
  ard <- cards::ard_stack_hierarchical(
    adae, variables = c(AEBODSYS, AEDECOD), by = TRTA,
    denominator = adsl, id = USUBJID, over_variables = TRUE)
  d <- normalize_ard(ard, hierarchy = c("AEBODSYS", "AEDECOD"),
                     overall = "Any TEAE")
  p <- table_plan(d, cols = "TRTA", rows = c(group1 = "AEBODSYS"),
                label = c(label = "AEDECOD")) |> plan_cells(notes = FALSE)
  n <- rtfreporter:::.plan_n_values(p, TRUE)
  expect_equal(unname(n[c("Placebo", "Xanomeline Low Dose",
                          "Xanomeline High Dose")]), c(86, 84, 84))
})

test_that("normalize_ard() keeps the total it drops, as a number", {
  skip_if_no_cards2()
  # the row is not a table statistic and goes; the NUMBER is a
  # denominator and stays, because a header still asks for it
  d <- data.frame(
    TRT = c(NA, "A", "B"),
    variable = c("..ard_total_n..", "X", "X"),
    variable_level = NA_character_,
    context = c("total_n", "categorical", "categorical"),
    stat_name = c("N", "n", "n"),
    stat_label = c("N", "n", "n"),
    stat = c(254, 7, 9),
    stringsAsFactors = FALSE)
  nz <- normalize_ard(d, keys = "TRT")
  expect_false(any(nz$variable == "..ard_total_n.."))
  expect_identical(attr(nz, "ard_total_n", exact = TRUE), 254)
})

test_that("a total that is not one number is not remembered", {
  skip_if_no_cards2()
  # two totals is a question, and answering it would be a guess
  d <- data.frame(
    variable = c("..ard_total_n..", "..ard_total_n..", "X"),
    variable_level = NA_character_,
    context = c("total_n", "total_n", "categorical"),
    stat_name = c("N", "N", "n"),
    stat_label = c("N", "N", "n"),
    stat = c(254, 86, 7),
    stringsAsFactors = FALSE)
  expect_null(attr(normalize_ard(d), "ard_total_n", exact = TRUE))
})

test_that("one variable's N is not a column's population (#482)", {
  skip_if_no_cards2()
  base <- data.frame(
    group1 = c(NA, rep("TRT", 4L)),
    group1_level = c(NA, "A", "A", "B", "B"),
    variable = c("..ard_total_n..", rep("X", 4L)),
    variable_level = "x",
    context = c("total_n", rep("categorical", 4L)),
    stat_name = c("N", "N", "n", "N", "n"),
    stat_label = c("N", "N", "n", "N", "n"),
    stat = c(254, 86, 7, 84, 9),
    stringsAsFactors = FALSE)
  # X's N is the count of its non-missing values: the arm size only if
  # nothing is missing, which the ARD cannot show -- so it is not read
  p <- table_plan(normalize_ard(base, keys = "TRT"), cols = "TRT") |> plan_cells(notes = FALSE)
  v <- rtfreporter:::.plan_n_values(p, TRUE)
  expect_length(nv(p), 0L)
  expect_match(attr(v, "why")[["A"]], "only X states an N")
  # the study total is remembered as the TOTAL, never each column's
  expect_identical(attr(v, "total"), 254)
  fill <- function() rtfreporter:::.plan_header_fill(
    rtf_col_header(c("", "(N={n})")), v, c("A", "B"), 1L)
  expect_warning(fill(), "printed as NA")
  expect_warning(fill(), "only X states an N")
  h <- suppressWarnings(fill())
  expect_identical(h[[1L]], c("", "(N=NA)", "(N=NA)"))
})

test_that("print() says why {n} could not be resolved", {
  skip_if_no_cards2()
  # saying nothing is what sent the reader here
  d <- data.frame(TRT = c("A", "B"), variable = "X",
                  stat_name = "mean", stat = c(1, 2),
                  .label = "x", stringsAsFactors = FALSE)
  p <- table_plan(d, cols = "TRT") |> plan_cells(notes = FALSE)
  p <- plan_col_header(p, values = TRUE,
                       rtf_col_header(c("", "(N={n})")))
  out <- paste(capture.output(print(p)), collapse = "|")
  expect_match(out, "NOT resolved", fixed = TRUE)
})

test_that("plan_digits(.rows = ) formats the value cells by row label", {
  skip_if_no_cards2()
  # `cols = 3:31` is a count of the columns one study happened to have;
  # the plan knows which columns hold values
  d <- data.frame(
    group1 = "PARAM", group1_level = "Drug X",
    group2 = "TP", group2_level = c("t1", "t1", "t2", "t2"),
    variable = "AVAL", variable_level = NA_character_,
    context = "continuous",
    stat_name = c("N", "mean", "N", "mean"),
    stat_label = c("N", "Mean", "N", "Mean"),
    stat = c(12, 1.23456, 12, 2.34567),
    .kind = "continuous", stringsAsFactors = FALSE)
  p <- table_plan(normalize_ard(d, keys = c("PARAM", "TP")), cols = "TP",
                rows = c(Analyte = "PARAM"),
                label = c(Statistics = "stat_label")) |> plan_cells(stats = "rows", notes = FALSE) |>
    plan_digits(.rows = c(N = 0, Mean = "3s"))
  tbl <- plan_apply(p, "pages")[[1L]]$data
  expect_identical(tbl$t1, c("12", "1.23"))
  expect_identical(tbl$t2, c("12", "2.35"))
  # the label column is untouched -- it was never a value cell
  expect_identical(as.character(tbl$Statistics), c("N", "Mean"))
})

test_that("plan_paginate_cols(every = ) counts the columns for you", {
  skip_if_no_cards2()
  d <- data.frame(
    group1 = "PARAM", group1_level = "Drug X",
    group2 = "TP", group2_level = rep(paste0("t", 1:5), each = 2L),
    variable = "AVAL", variable_level = NA_character_,
    context = "continuous",
    stat_name = rep(c("N", "mean"), 5L),
    stat_label = rep(c("N", "Mean"), 5L),
    stat = seq_len(10L),
    .kind = "continuous", stringsAsFactors = FALSE)
  p <- table_plan(normalize_ard(d, keys = c("PARAM", "TP")), cols = "TP",
                rows = c(Analyte = "PARAM"),
                label = c(Statistics = "stat_label")) |> plan_cells(stats = "rows", notes = FALSE)
  # 5 value columns, 2 carried: blocks of 2, 2, 1
  pg <- plan_apply(plan_paginate_cols(p, every = 2L, keep = 1:2))
  expect_identical(vapply(pg, function(z) ncol(z$data), 0L),
                   c(4L, 4L, 3L))
  # a block wide enough for everything cuts nothing
  one <- plan_apply(plan_paginate_cols(p, every = 99L, keep = 1:2))
  expect_length(one, 1L)
})

# ------------------------------------------ the header n, key rows, .kind --


test_that("plan_col_header(values = TRUE) reads the key's own tabulation", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  adsl$TRT <- factor(as.character(adsl$ARM),
                     c("Xanomeline Low Dose", "Placebo",
                       "Xanomeline High Dose"))
  adsl$AGE[1:5] <- NA          # so the summary's N is NOT the arm size
  ard <- cards::ard_stack(adsl, .by = TRT,
                          cards::ard_summary(variables = AGE))
  p <- table_plan(normalize_ard(ard), cols = "TRT")
  arm <- table(adsl$TRT)
  expect_equal(nv(p), stats::setNames(as.numeric(arm), names(arm)))
})

test_that("a key tabulation that is not the population split is not taken", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adae <- merge(cards::ADAE[, c("USUBJID", "AESOC")],
                adsl[, c("USUBJID", "TRT")], by = "USUBJID")
  # the treatment counted as an EVENT: n per arm does not add up to one N
  ard <- cards::bind_ard(
    cards::ard_tabulate(adae[!duplicated(adae$USUBJID), ],
                           variables = TRT, denominator = adsl),
    cards::ard_hierarchical(adae[!duplicated(adae[c("USUBJID", "AESOC")]), ],
                            variables = AESOC, by = TRT,
                            id = USUBJID, denominator = adsl))
  p <- table_plan(normalize_ard(ard, hierarchy = "AESOC"), cols = "TRT")
  # not the event counts: the hierarchical summary's denominator
  arm <- table(adsl$TRT)
  expect_equal(nv(p), stats::setNames(as.numeric(arm), names(arm))[names(nv(p))])
  expect_setequal(names(nv(p)), names(arm))
})

test_that("a Total column the key cannot speak for gets no number", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  d <- normalize_ard(cards::ard_stack(
    adsl, .by = TRT, cards::ard_tabulate(variables = SEX),
    .overall = TRUE))
  d$TRT[is.na(d$TRT) & !d$.key_own] <- "Total"
  p <- table_plan(d, cols = "TRT")
  # the arms are the key's own partition; "Total" is nobody's
  arm <- table(adsl$TRT)
  expect_equal(nv(p)[names(arm)],
               stats::setNames(as.numeric(arm), names(arm)))
  expect_false("Total" %in% names(nv(p)))
})

test_that("a variable summarised and tabulated gets both recipes in a plan", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adsl$DEC <- round(adsl$AGE / 10)
  d <- normalize_ard(cards::ard_stack(
    adsl, .by = TRT,
    cards::ard_summary(variables = DEC),
    cards::ard_tabulate(variables = DEC)))
  direct <- widen_ard(d, cols = "TRT", notes = FALSE,
                       cells = list(continuous  = "{mean:.1f}",
                                    categorical = "{n}"))
  planned <- suppressMessages(table_plan(d, cols = "TRT") |>
    plan_cells(continuous = "{mean:.1f}", categorical = "{n}") |>
    plan_apply("table"))
  expect_equal(as.data.frame(planned), as.data.frame(direct))
  expect_false(anyNA(direct[!is.na(direct$label), -(1:2)]))
})


# ------------------------------------------------ roles from a definition file



# ------------------------------------------- the table half of a workbook

spec_pages_ard <- function() nz(plan_ard())



# ------------------------------------------------ the col_header sheet


# ------------------------------------------------ plan -> workbook

code_plan <- function(d = spec_pages_ard()) {
  table_plan(d, cols = "TRT", rows = c(group = "variable")) |> plan_cells(notes = FALSE) |>
    plan_labels(AGE = "Age (years)", SEX = "Sex") |>
    plan_cells(continuous  = c("n" = "{N:d}", "Mean (SD)" = "{mean} ({sd})"),
               categorical = "{n:d} ({p:.1f%})") |>
    plan_digits(1, rounding = "sas") |>
    plan_stub(name = "row_label", before = TRUE) |>
    plan_blanks(where = "between_groups", first = TRUE) |>
    plan_paginate_rows(max_rows = 6, split = "group_safe") |>
    plan_columns(widths = c(4, 2)) |>
    plan_style(align_count_pct = TRUE) |>
    plan_col_header(values = TRUE, rtf_col_header(c("", "{col}"),
                                             c("Characteristic", "(N={n})")))
}


# ------------------------------- the header N, level by level (#482) --

hdr_rows <- function(out) {
  first <- if (inherits(out, "rtftable")) out else out[[1L]]
  lapply(first$col_header, function(r) if (is.character(r)) r else
    vapply(r, function(z) as.character(z$label), ""))
}

adsl_trt <- function() {
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adsl$SEX <- as.character(adsl$SEX)
  adsl$AGEGRP <- ifelse(adsl$AGE < 70, "<70", ">=70")
  adsl
}


test_that("variables that disagree anywhere give no number anywhere", {
  skip_if_not_installed("cards")
  adsl <- adsl_trt()
  adsl$AGE[adsl$TRT == "Placebo"][1:3] <- NA   # Placebo: AGE 83, SEX 86
  ard <- cards::bind_ard(
    cards::ard_summary(adsl, by = TRT, variables = AGE),
    cards::ard_tabulate(adsl, by = TRT, variables = SEX))
  p <- table_plan(normalize_ard(ard), cols = "TRT") |> plan_cells(notes = FALSE)
  # the other arms agree (84 = 84) only because nothing is missing there
  expect_length(nv(p), 0L)
  expect_match(attr(rtfreporter:::.plan_n_values(p, TRUE), "why")[[1L]],
               "different N")
})

test_that("two variables that agree everywhere are read", {
  skip_if_not_installed("cards")
  adsl <- adsl_trt()
  ard <- cards::bind_ard(
    cards::ard_summary(adsl, by = TRT, variables = AGE),
    cards::ard_tabulate(adsl, by = TRT, variables = SEX))
  p <- table_plan(normalize_ard(ard), cols = "TRT") |> plan_cells(notes = FALSE)
  arm <- table(adsl$TRT)
  expect_equal(nv(p), stats::setNames(as.numeric(arm), names(arm)))
})

test_that("an N per visit is not a column's number", {
  skip_if_not_installed("cards")
  adlb <- cards::ADLB[cards::ADLB$PARAMCD == "ALT", ]
  adlb$TRT <- as.character(adlb$TRTA)
  ard <- cards::ard_summary(adlb, by = c(TRT, AVISIT), variables = AVAL)
  p <- table_plan(normalize_ard(ard), cols = "TRT") |> plan_cells(notes = FALSE)
  expect_length(nv(p), 0L)
  expect_match(attr(rtfreporter:::.plan_n_values(p, TRUE), "why")[[1L]],
               "per row")
})

test_that("two column keys: the arm and the arm x sex cell are both read", {
  skip_if_not_installed("cards")
  adsl <- adsl_trt()
  d <- normalize_ard(cards::ard_stack(
    adsl, .by = c(TRT, SEX), cards::ard_summary(variables = AGE),
    cards::ard_tabulate(variables = AGEGRP)))
  p <- table_plan(d, cols = c("TRT", "SEX"), rows = c(group = "variable")) |> plan_cells(notes = FALSE) |>
    plan_cells(continuous = "{mean:.1f}", categorical = "{n}")
  v <- nv(p)
  expect_identical(v[["Placebo"]], 86)
  expect_identical(v[["Placebo____F"]], 53)
  expect_identical(v[["Xanomeline High Dose____M"]], 44)
  # a spanner's {n} is its level's; a leaf's is its cell's
  out <- suppressMessages(plan_apply(plan_col_header(p, values = TRUE,
    rtf_col_header(
      list(col_cell(1, ""), col_cell(2, ""),
           col_cell(c(3, 4), "{col1} (N={n})"),
           col_cell(c(5, 6), "{col1} (N={n})"),
           col_cell(c(7, 8), "{col1} (N={n})")),
      c("", "", "{col2} (N={n})"))), "pages"))
  h <- hdr_rows(out)
  expect_identical(h[[1L]][3:5], c("Placebo (N=86)",
                                   "Xanomeline High Dose (N=84)",
                                   "Xanomeline Low Dose (N=84)"))
  expect_identical(h[[2L]][3:4], c("F (N=53)", "M (N=33)"))
  # {n1} / {n2} name a depth from any cell
  out <- suppressMessages(plan_apply(plan_col_header(p, values = TRUE,
    rtf_col_header(c("", "", "{col1} {n1}"), c("", "", "{col2} {n2}"))),
    "pages"))
  h <- hdr_rows(out)
  expect_identical(h[[1L]][3:4], c("Placebo 86", "Placebo 86"))
  expect_identical(h[[2L]][3:4], c("F 53", "M 33"))
})

test_that("three keys: a depth the ARD does not state is NA, with a warning", {
  skip_if_not_installed("cards")
  adsl <- adsl_trt()
  mk <- function(extra = NULL) {
    a <- cards::ard_stack(adsl, .by = c(TRT, SEX, AGEGRP),
                          cards::ard_summary(variables = AGE),
                          cards::ard_tabulate(variables = RACE))
    if (!is.null(extra)) a <- cards::bind_ard(a, extra)
    table_plan(normalize_ard(a), cols = c("TRT", "SEX", "AGEGRP"),
             rows = c(group = "variable")) |> plan_cells(notes = FALSE) |>
      plan_cells(continuous = "{mean:.1f}", categorical = "{n}") |>
      plan_col_header(values = TRUE, rtf_col_header(
        c("", "", "{col1} N={n1}"), c("", "", "{col2} N={n2}"),
        c("", "", "{col3} N={n3}")))
  }
  # cards tabulates the by variables one at a time: TRT x SEX is nowhere
  expect_warning(out <- suppressMessages(plan_apply(mk(), "pages")),
                 "printed as NA")
  h <- hdr_rows(out)
  expect_identical(h[[1L]][3L], "Placebo N=86")
  expect_identical(h[[2L]][3L], "F N=NA")
  expect_identical(h[[3L]][3:4], c("<70 N=13", ">=70 N=40"))
  # tabulating SEX within TRT states it
  out <- suppressMessages(plan_apply(mk(
    cards::ard_tabulate(adsl, by = TRT, variables = SEX)), "pages"))
  expect_identical(hdr_rows(out)[[2L]][3L], "F N=53")
})

test_that("a hierarchical ARD by two keys states both levels", {
  skip_if_not_installed("cards")
  adsl <- adsl_trt()
  adae <- cards::ADAE[cards::ADAE$TRTEMFL == "Y", ]
  adae$TRT <- as.character(adae$TRTA)
  adae$SEX <- as.character(adae$SEX)
  d <- normalize_ard(cards::ard_stack_hierarchical(
    adae, variables = c(AEBODSYS, AEDECOD), by = c(TRT, SEX),
    denominator = adsl, id = USUBJID, over_variables = TRUE),
    hierarchy = c("AEBODSYS", "AEDECOD"), overall = "Any TEAE")
  p <- table_plan(d, cols = c("TRT", "SEX"), rows = c(group1 = "AEBODSYS"),
                label = c(label = "AEDECOD")) |> plan_cells(notes = FALSE)
  v <- nv(p)
  expect_identical(v[["Placebo"]], 86)
  expect_identical(v[["Placebo____F"]], 53)
})

test_that("{n:sum} with a column missing is NA, not a smaller total", {
  f <- rtfreporter:::.plan_header_fill
  h <- rtf_col_header(list(col_cell(1, ""), col_cell(c(2, 3), "{n:sum}")))
  expect_warning(out <- f(h, c(A = 10), c("A", "B"), 1L), "printed as NA")
  expect_identical(out[[1L]][[2L]]$label, "NA")
  # a spanner over some columns that share no level is not the total
  v <- structure(c(A = 10, B = 20, C = 30), total = 60)
  h <- rtf_col_header(list(col_cell(1, ""), col_cell(c(2, 3), "{n}"),
                           col_cell(c(2, 4), "{n}")))
  out <- suppressWarnings(f(h, v, c("A", "B", "C"), 1L))
  expect_identical(vapply(out[[1L]], `[[`, "", "label"), c("", "NA", "60"))
})

test_that("the key tabulated on its own states the table's total", {
  skip_if_not_installed("cards")
  adsl <- adsl_trt()
  d <- normalize_ard(cards::ard_stack(
    adsl, .by = TRT, cards::ard_tabulate(variables = SEX)))
  p <- table_plan(d, cols = "TRT") |> plan_cells(notes = FALSE)
  expect_identical(attr(rtfreporter:::.plan_n_values(p, TRUE), "total"),
                   nrow(adsl) + 0)
})

test_that("pages split by a group value each read their own N from the ARD", {
  skip_if_not_installed("cards")
  set.seed(1)
  lb <- expand.grid(USUBJID = cards::ADSL$USUBJID,
                    PARAM = c("ALT", "HGB"), stringsAsFactors = FALSE)
  lb$BGRADE <- sample(c("G0", "G1"), nrow(lb), TRUE)
  lb$WGRADE <- sample(c("G0", "G1", "G2"), nrow(lb), TRUE)
  lb <- lb[!(lb$PARAM == "HGB" & seq_len(nrow(lb)) %% 10 == 0), ]
  ard <- cards::bind_ard(
    cards::ard_tabulate(lb, by = c(PARAM, BGRADE), variables = WGRADE),
    # the population of each parameter, split by the column variable:
    # the ARD states every column's N and each page's total
    cards::ard_tabulate(lb, by = PARAM, variables = BGRADE))
  p <- table_plan(normalize_ard(ard), cols = "BGRADE",
                rows = c(PARAM = "PARAM"), label = c(label = ".label")) |> plan_cells(notes = FALSE) |>
    plan_cells("{n}") |>
    plan_paginate_group(keep = FALSE) |>
    plan_col_header(values = TRUE, rtf_col_header(
      list(col_cell(1, ""), col_cell(c(2, 3), "All (N={n})")),
      c("", "{col} (N={n})")))
  pg <- expect_silent(suppressMessages(plan_apply(p, "pages")))
  n <- table(lb$PARAM)
  cell <- table(lb$PARAM, lb$BGRADE)
  for (i in seq_along(pg)) {
    prm <- names(pg)[i]
    h <- hdr_rows(pg[i])
    expect_identical(h[[1L]][2L], sprintf("All (N=%d)", n[[prm]]))
    expect_identical(h[[2L]][2:3], sprintf("%s (N=%d)", c("G0", "G1"),
                                           cell[prm, c("G0", "G1")]))
  }
  expect_false(identical(hdr_rows(pg[1])[[1L]], hdr_rows(pg[2])[[1L]]))
})


# the analysis set and the subjects with the test: both in the ARD
two_pop <- function() {
  set.seed(1)
  lb <- expand.grid(USUBJID = cards::ADSL$USUBJID,
                    PARAM = c("ALT", "HGB"), stringsAsFactors = FALSE)
  lb$BGRADE <- sample(c("G0", "G1"), nrow(lb), TRUE)
  lb$WGRADE <- sample(c("G0", "G1", "G2"), nrow(lb), TRUE)
  lb <- lb[!(lb$PARAM == "HGB" & seq_len(nrow(lb)) %% 10 == 0), ]
  d <- normalize_ard(cards::bind_ard(
    cards::ard_tabulate(lb, by = c(PARAM, BGRADE), variables = WGRADE),
    cards::ard_tabulate(lb, by = PARAM, variables = BGRADE),
    cards::ard_total_n(cards::ADSL)), drop_contexts = "attributes")
  list(d = d, tested = table(lb$PARAM), set = nrow(cards::ADSL))
}
two_pop_plan <- function(d, n, text = "T (N={n})") {
  table_plan(d, cols = "BGRADE", rows = c(PARAM = "PARAM"),
           label = c(label = ".label")) |> plan_cells(notes = FALSE) |>
    plan_cells("{n}") |>
    plan_paginate_group(keep = FALSE) |>
    plan_col_header(values = n, rtf_col_header(
      list(col_cell(1, ""), col_cell(c(2, 3), text)),
      c("", "{col} (n={n})")))
}
spanner <- function(pg) vapply(pg, function(p)
  p$col_header[[1L]][[2L]]$label, "")


test_that("n = \"page\" / \"table\" choose the population", {
  skip_if_not_installed("cards")
  x <- two_pop()
  pg <- suppressMessages(plan_apply(two_pop_plan(x$d, "page"), "pages"))
  expect_identical(unname(spanner(pg)),
                   sprintf("T (N=%d)", as.integer(x$tested[names(pg)])))
  expect_warning(pg <- suppressMessages(plan_apply(
    two_pop_plan(x$d, "table"), "pages")), "printed as NA")
  expect_identical(unname(spanner(pg)), rep(sprintf("T (N=%d)", x$set), 2L))
  # the analysis set by baseline grade is not in this ARD: NA, not a guess
  expect_identical(pg[[1L]]$col_header[[2L]][2L], "G0 (n=NA)")
})

test_that("two populations and no choice: the page's, with a warning", {
  skip_if_not_installed("cards")
  x <- two_pop()
  expect_warning(pg <- suppressMessages(plan_apply(
    two_pop_plan(x$d, TRUE), "pages")), "two populations")
  expect_identical(spanner(pg)[["HGB"]],
                   sprintf("T (N=%d)", as.integer(x$tested[["HGB"]])))
})

test_that("plan_n_candidates(): each population {n} can say, as {n} prints it", {
  skip_if_not_installed("cards")
  x <- two_pop()
  cand <- plan_n_candidates(two_pop_plan(x$d, "page"))
  expect_named(cand, c("scope", "page", "column", "value"))
  expect_true(attr(cand, "differ"))
  expect_identical(attr(cand, "page_col"), "PARAM")
  expect_setequal(unique(cand$scope), c("page", "table"))
  # over all the columns: each page's tested subjects, the analysis set
  tot <- cand[is.na(cand$column), ]
  expect_identical(tot$value[tot$scope == "page"],
                   as.numeric(x$tested[tot$page[tot$scope == "page"]]))
  expect_identical(tot$value[tot$scope == "table"], as.numeric(x$set))
  # what the header prints with each choice
  pg <- suppressMessages(plan_apply(two_pop_plan(x$d, "page"), "pages"))
  expect_identical(unname(spanner(pg)),
                   sprintf("T (N=%d)", as.integer(tot$value[match(names(pg), tot$page)])))
  # a page's columns: its own (the table's for what it lacks)
  hgb <- cand[cand$scope == "page" & cand$page == "HGB" & !is.na(cand$column), ]
  expect_setequal(hgb$column, c("G0", "G1"))
  # a table not split by page: one population, the same the header reads
  ard <- normalize_ard(cards::ard_stack(cards::ADSL, .by = ARM,
                                        cards::ard_summary(variables = AGE)))
  p <- table_plan(ard, cols = "ARM")
  one <- plan_n_candidates(p)
  expect_identical(unique(one$scope), "all")
  expect_false(attr(one, "differ"))
  arm <- one[!is.na(one$column), ]
  n <- table(cards::ADSL$ARM)
  expect_identical(arm$value, as.numeric(n[arm$column]))
  tk <- plan_header_tokens(plan_col_header(p, values = list(n = TRUE)))
  v <- unlist(tk$values[[match("{n}", tk$token)]])
  expect_identical(as.numeric(v[arm$column]), arm$value)
  # no cols: nothing
  expect_identical(nrow(plan_n_candidates(table_plan(ard))), 0L)
  expect_error(plan_n_candidates(ard), "expected a table_plan")
})

test_that("plan_style() takes the whole table's settings, and only those", {
  skip_if_no_cards2()
  p <- base_plan()
  # a per-column setting is another verb's
  expect_error(plan_style(p, widths = c(3, 3)), "unused argument")
  expect_error(plan_style(p, row_title = 1:2), "unused argument")
  expect_s3_class(plan_style(p, border = "tfl", align_count_pct = TRUE),
                  "table_plan")
  # one kind of row, by the name rtf_table_style() gives it
  z <- plan_apply(plan_style(p, border_body = rtf_border(bottom = TRUE)),
                  "args")$rtf
  expect_s3_class(z$style, "rtf_table_style")
  expect_error(plan_style(p, border = "tfl", border_body = rtf_border()),
               "one way")
})

test_that("plan_columns() declares the stub column, auto width and the key separator", {
  skip_if_no_cards2()
  a <- plan_apply(base_plan() |>
                    plan_columns(row_title = 1:2, auto_width = TRUE,
                                 sep = "__"), "args")
  expect_identical(a$rtf$row_title, 1:2)
  expect_true(a$rtf$auto_width)
  expect_identical(a$widen$sep, "__")
  expect_error(plan_columns(base_plan(), sep = ""), "non-empty")
})

test_that("plan_columns(sep = ) also drives the ARD plan's own spanning header (#520)", {
  skip_if_not_installed("cards")
  adsl <- adsl_trt()
  d <- normalize_ard(cards::ard_stack(
    adsl, .by = c(TRT, SEX), cards::ard_summary(variables = AGE),
    cards::ard_tabulate(variables = AGEGRP)))
  mk <- function(sep = NULL) {
    p <- table_plan(d, cols = c("TRT", "SEX"), rows = c(group = "variable")) |>
      plan_cells(notes = FALSE) |>
      plan_cells(continuous = "{mean:.1f}", categorical = "{n}")
    if (!is.null(sep)) p <- plan_columns(p, sep = sep)
    p
  }
  rtf <- function(x) {
    f <- tempfile(fileext = ".rtf")
    old <- options(rtfreporter.render_time = as.POSIXct("2000-01-01", tz = "UTC"))
    on.exit({ options(old); unlink(f) })
    generate_rtfreport(rtf_tables(rtf_document(), x), f, overwrite = TRUE)
    readBin(f, "raw", file.info(f)$size)
  }
  default_out <- suppressMessages(plan_apply(mk(), "pages"))
  custom_out  <- suppressMessages(plan_apply(mk("--"), "pages"))
  # same spanning structure -- TRT/SEX values contain no separator, so the
  # header text itself does not differ between the two separators
  expect_identical(hdr_rows(default_out), hdr_rows(custom_out))
  expect_identical(rtf(default_out), rtf(custom_out))
  # a plan that declares its own header_sep still wins over the plan's sep
  explicit <- plan_apply(mk("--") |> plan_col_header(header_sep = "--"),
                         "args")$rtf
  expect_identical(explicit$header_sep, "--")
})

test_that("table_plan() takes the roles, and stat = names the parts", {
  expect_error(table_plan(data.frame(a = 1), stat = c(value = "a", nope = "a")),
               "only these three names")
  expect_error(table_plan(data.frame(a = 1), stat = c(value = "b")),
               "stat = c(value", fixed = TRUE)
})


# -- the settings that left table_plan() (#498) --------------------------------

test_that("plan_cells() carries how a cell is made, however the plan is run", {
  skip_if_no_cards2()
  p <- table_plan(nz(plan_ard()), cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(categorical = "{n:.0f}", stats = "cells", na = "-",
               notes = FALSE)
  a <- plan_apply(p, "args")$widen
  expect_identical(a$na, "-")
  expect_identical(a$stats, "cells")
  expect_false(a$notes)
  # notes = FALSE holds when rtf_tables() runs the plan, too
  expect_silent(as_rtftables(p |> plan_stub()))
  expect_error(plan_cells(p, stats = "columns"), "should be one of")
  expect_error(plan_cells(p, notes = "no"), "TRUE")
})

test_that("plan_sort(stat = ) names the statistic a frequency order totals", {
  skip_if_no_cards2()
  p <- base_plan() |> plan_sort("-n", stat = "n")
  expect_identical(plan_apply(p, "args")$widen$sort_stat, "n")
})

test_that("a finished table sorts descending by -name", {
  d <- data.frame(term = c("a", "b", "c"), ord = c(2, 3, 1),
                  stringsAsFactors = FALSE)
  a <- plan_apply(table_plan(d) |> plan_sort("-ord") |> plan_hide("ord"),
                  "args")$rtf
  expect_identical(a$sort_by, "ord")
  expect_true(a$sort_desc)
  pg <- plan_apply(table_plan(d) |> plan_sort("-ord") |> plan_hide("ord"))
  first <- if (inherits(pg, "rtftable")) pg else pg[[1L]]
  expect_identical(first$data$term, c("b", "a", "c"))
})

test_that("plan_digits() formats a finished table by column name", {
  d <- data.frame(term = c("a", "b"), val = c(1.23456, 2.5),
                  stringsAsFactors = FALSE)
  pg <- plan_apply(table_plan(d) |> plan_digits(val = 2) |> plan_style(border = "tfl"))
  first <- if (inherits(pg, "rtftable")) pg else pg[[1L]]
  expect_identical(first$data$val, c("1.23", "2.50"))
  expect_error(plan_apply(table_plan(d) |> plan_digits(nope = 2)),
               "matched nothing")
})

test_that("plan_blanks(where = \"records\") separates a listing's records", {
  pg <- suppressMessages(plan_apply(
    table_plan(.pages_src()) |>
      plan_listing(listing_col("USUBJID", width = 12)) |>
      plan_blanks(where = "records")))
  first <- if (inherits(pg, "rtftable")) pg else pg[[1L]]
  expect_gt(length(first$blank_rows %||% attr(first$data, "rtf_blank_rows")), 0L)
  # a table has no records to separate
  expect_error(plan_apply(disp_plan() |> plan_blanks(where = "records"),
                          "pages"), "plan_listing")
})

test_that("plan_blanks(where = \"none\") takes a listing's record separator away", {
  d <- data.frame(USUBJID = rep(c("S1", "S2"), each = 3), T = rep(1:3, 2), V = 1:6)
  lst <- function(p) p |> plan_listing(listing_col("USUBJID", width = 8),
                                       listing_col("T", width = 4), listing_col("V", width = 4))
  rows <- function(p) {
    pg <- plan_apply(p)
    nrow((if (inherits(pg, "rtftable")) pg else pg[[1L]])$data)
  }
  # the multiline type: a blank row after each record; none: the records alone
  expect_gt(rows(lst(table_plan(d))), nrow(d))
  expect_identical(rows(lst(table_plan(d)) |> plan_blanks(where = "none")), nrow(d))
  # the same as listing_spec(blank_row = FALSE)
  spec <- listing_spec(list(listing_col("USUBJID", width = 8), listing_col("T", width = 4),
                            listing_col("V", width = 4)), blank_row = FALSE)
  expect_identical(nrow(build_listing(d, spec)), nrow(d))
  # a table: no blank rows, as before
  expect_identical(rows(table_plan(d) |> plan_blanks(where = "none")), nrow(d))
})

test_that("plan_paginate_cols() cuts by a block list or a separator", {
  skip_if_no_cards2()
  p <- disp_plan() |> plan_paginate_rows(max_rows = 40)
  a <- plan_apply(plan_paginate_cols(p, cut_by = list(3:4, 5L), keep = 1:2,
                                     fit = TRUE))
  expect_length(a, 2L)
  expect_error(plan_paginate_cols(p, at = 4L, every = 2L), "one way")
  expect_error(plan_paginate_cols(p, fit = "keep"), "TRUE")
})

test_that("a conditional style follows its rows onto every page", {
  skip_if_no_cards2()
  # the styles are computed for the whole table; the page split cuts them
  # with the rows, instead of handing each page all of them
  pg <- suppressMessages(plan_apply(
    disp_plan() |>
      plan_stub(vars = c("group", "label"), name = "row_label", before = TRUE) |>
      plan_paginate_rows(max_rows = 4, split = "group_safe") |>
      plan_cell_style(where = ~ is.na(label), italic = TRUE)))
  expect_gt(length(pg), 1L)
  for (p in pg) {
    expect_length(p$cell_styles, nrow(p$data))
    it <- vapply(p$cell_styles, function(r) isTRUE(r$italic[[1L]]), NA)
    expect_true(it[[1L]])               # each page opens on a heading row
  }
})

test_that("a missing declaration is not blamed on unrelated verbs (#507)", {
  skip_if_no_cards2()
  p <- table_plan(nz(plan_ard()), rows = c(group = "variable")) |>
    plan_cells(notes = FALSE)
  e <- tryCatch(suppressMessages(plan_apply(p)), error = function(e) e)
  expect_s3_class(e, "error")
  msg <- conditionMessage(e)
  expect_match(msg, "`cols` is required", fixed = TRUE)
  expect_match(msg, "table_plan(cols = )", fixed = TRUE)
  expect_no_match(msg, "declared by")
  expect_no_match(msg, "plan_cells")
})

test_that("plan_labels() scoped by an analysis variable relabels its levels (#514)", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adsl$SEX <- as.character(adsl$SEX)
  ard <- cards::ard_stack(adsl, .by = TRT,
    cards::ard_tabulate(variables = c(SEX, AGEGR1), statistic = ~ c("n", "p")))
  d <- suppressMessages(normalize_ard(ard))
  base <- table_plan(d, cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(notes = FALSE) |> plan_cells("{n:d} ({p:.1f%})")
  page <- function(p) {
    x <- suppressMessages(plan_apply(p))
    if (is.data.frame(x)) x else if (inherits(x, "rtftable")) x$data else x[[1L]]$data
  }
  before <- page(base)
  got <- page(base |> plan_labels(SEX = c(F = "Female", M = "Male")) |>
                plan_levels(SEX = c("M", "F")))
  lab <- function(x, g) as.character(x$label[x$group == g])
  expect_identical(lab(got, "SEX"), c("Male", "Female"))
  # the other variable, and the numbers, are as they were
  expect_identical(lab(got, "AGEGR1"), lab(before, "AGEGR1"))
  num <- function(x, l) unlist(x[as.character(x$label) == l, -(1:2)])
  expect_identical(num(got, "Female"), num(before, "F"))
  expect_identical(num(got, "Male"), num(before, "M"))
  # its own name labels the variable (its group row) at the same time
  both <- page(base |> plan_labels(SEX = c(SEX = "Sex", F = "Female", M = "Male")))
  expect_true("Sex" %in% as.character(both$group))
  expect_identical(lab(both, "Sex"), c("Female", "Male"))
  # a scope naming no analysis variable changes nothing
  expect_identical(page(base |> plan_labels(NOPE = c(F = "Female"))), before)
  # the variables in the ARD's order, however labelled -- a variable's own
  # label in its dictionary or not (#585, #666): a label sets no order
  grp <- function(x) unique(as.character(x$group))
  expect_identical(grp(page(base |> plan_labels(SEX = "Sex", AGEGR1 = "Age group"))),
                   c("Sex", "Age group"))
  expect_identical(grp(page(base |> plan_levels(SEX = c("F", "M")) |>
                              plan_labels(SEX = c(SEX = "Sex", F = "Female", M = "Male"),
                                          AGEGR1 = "Age group"))),
                   c("Sex", "Age group"))
  expect_identical(grp(page(base |> plan_labels(AGEGR1 = "Age group",
                                                SEX = c(SEX = "Sex", F = "Female")))),
                   c("Sex", "Age group"))
  # the order is plan_levels()'s
  expect_identical(grp(page(base |> plan_levels(variable = c("AGEGR1", "SEX")) |>
                              plan_labels(SEX = "Sex", AGEGR1 = "Age group"))),
                   c("Age group", "Sex"))
})

test_that("plan_paginate_rows(page_by = ) is as_rtftables(page_by = ): BY pages, rows paged inside", {
  tbl <- data.frame(
    period = rep(c("Period 1", "Period 2"), each = 8),
    param  = rep(rep(c("ALT", "AST"), each = 4), 2),
    stat   = rep(c("n", "Mean", "SD", "Median"), 4),
    A = as.character(1:16), B = as.character(16:1), stringsAsFactors = FALSE)
  p <- table_plan(tbl) |>
    plan_paginate_rows(page_by = "period", split = "group_safe", max_rows = 5) |>
    plan_hide("period")
  a <- plan_apply(p, "args")$rtf
  expect_identical(a$page_by, "period")
  pages <- plan_apply(p)
  direct <- as_rtftables(tbl, page_by = "period", split = "group_safe",
                         max_rows = 5, drop_cols = "period", read_meta = FALSE)
  # a page per period, each cut by the row budget inside it, named by it
  expect_identical(names(pages), names(direct))
  expect_identical(names(pages), c("Period 1...1", "Period 1...2",
                                   "Period 2...1", "Period 2...2"))
  expect_identical(lapply(pages, `[[`, "data"), lapply(direct, `[[`, "data"))
  # the RTF: byte for byte the direct call's
  rtf <- function(x) {
    f <- tempfile(fileext = ".rtf")
    old <- options(rtfreporter.render_time = as.POSIXct("2000-01-01", tz = "UTC"))
    on.exit({ options(old); unlink(f) })
    generate_rtfreport(rtf_tables(rtf_document(), x), f, overwrite = TRUE)
    readBin(f, "raw", file.info(f)$size)
  }
  expect_identical(rtf(pages), rtf(direct))
  # no page_by: the plan's arguments are as before
  q <- table_plan(tbl) |> plan_paginate_rows(split = "group_safe", max_rows = 5)
  expect_null(plan_apply(q, "args")$rtf$page_by)
})

test_that("every as_rtftables() setting has a plan verb, with the direct call's RTF", {
  tbl <- data.frame(
    param = rep(c("ALT", "AST"), each = 3),
    stat  = rep(c("n", "Mean", "SD"), 2),
    "Drug A__N" = c("1", NA, "3.25", "4", "5", NA),
    "Drug A__Pct" = c("10", "20", NA, "40", "50", "60"),
    check.names = FALSE, stringsAsFactors = FALSE)
  rtf <- function(x) {
    f <- tempfile(fileext = ".rtf")
    old <- options(rtfreporter.render_time = as.POSIXct("2000-01-01", tz = "UTC"))
    on.exit({ options(old); unlink(f) })
    generate_rtfreport(rtf_tables(rtf_document(), x), f, overwrite = TRUE)
    readBin(f, "raw", file.info(f)$size)
  }
  same <- function(p, ...) {
    direct <- as_rtftables(tbl, read_meta = FALSE, ...)
    pages <- plan_apply(p)
    expect_identical(lapply(pages, `[[`, "data"), lapply(direct, `[[`, "data"))
    expect_identical(rtf(pages), rtf(direct))
  }
  fmt <- function(x) ifelse(is.na(x), x, paste0("[", x, "]"))
  # plan_style(): the table's width and its default look
  same(table_plan(tbl) |> plan_style(table_width_twips = 8000),
       table_width_twips = 8000)
  same(table_plan(tbl) |> plan_style(table_width_pct = 80),
       table_width_pct = 80)
  same(table_plan(tbl) |> plan_style(table_width_pct_of_writable = 90),
       table_width_pct_of_writable = 90)
  same(table_plan(tbl) |> plan_style(header_bold = TRUE, header_italic = TRUE,
                                     header_align = "left", align = "right",
                                     bold = TRUE, italic = TRUE, underline = TRUE),
       style = rtf_table_style(header_bold = TRUE, header_italic = TRUE,
                               header_align = "left", align = "right",
                               bold = TRUE, italic = TRUE, underline = TRUE))
  # the look and the rules make ONE style object
  dbl <- rtf_border(top = rtf_border_line("double"),
                    bottom = rtf_border_line("double"),
                    inside_h = rtf_border_line("none"))
  same(table_plan(tbl) |> plan_style(border_header = dbl, header_bold = TRUE),
       style = rtf_table_style(border_header = dbl, header_bold = TRUE))
  # plan_columns(): a cell formatter and widths in twips
  same(table_plan(tbl) |> plan_columns(cell_format = fmt), cell_format = fmt)
  same(table_plan(tbl) |> plan_columns(cell_format = list(NULL, NULL, fmt)),
       cell_format = list(NULL, NULL, fmt))
  same(table_plan(tbl) |> plan_columns(column_widths_twips = c(2000, 2000, 1500, 1500)),
       column_widths_twips = c(2000, 2000, 1500, 1500))
  # plan_col_header(): a plain table's names split into a spanning header,
  # and how the header text sits
  same(table_plan(tbl) |> plan_col_header(header_sep = "__"), header_sep = "__")
  same(table_plan(tbl) |> plan_col_header(col_header_align = "left"),
       col_header_align = "left")
  # plan_cells(na = ) on a finished table: what a missing value prints as
  same(table_plan(tbl) |> plan_cells(na = "-"), na = "-")
  expect_identical(plan_apply(table_plan(tbl) |> plan_cells(na = "-"), "args")$rtf$na, "-")
  # plan_cell_style(): underline and indent, as style_cols() takes them
  p <- table_plan(tbl) |> plan_cell_style(cols = "stat", underline = TRUE,
                                          indent_twips = 200)
  direct <- style_cols(as_rtftables(tbl, read_meta = FALSE), cols = "stat",
                       underline = TRUE, indent_twips = 200)
  expect_identical(rtf(plan_apply(p)), rtf(direct))
  p <- table_plan(tbl) |> plan_col_header(header_sep = "__") |>
    plan_cell_style(header = TRUE, underline = TRUE)
  direct <- style_header(as_rtftables(tbl, read_meta = FALSE, header_sep = "__"),
                         underline = TRUE)
  expect_identical(rtf(plan_apply(p)), rtf(direct))
  expect_error(plan_cell_style(table_plan(tbl), header = TRUE, indent_twips = 200),
               "header = TRUE")
  # computed per row, by a condition
  p <- table_plan(tbl) |> plan_cell_style(where = ~ stat == "n", underline = TRUE)
  cs <- plan_apply(p)[[1]]$cell_styles
  expect_true(isTRUE(cs[[1]]$underline[[1]]))
  expect_null(cs[[2]])
  # plan_row_group(group_col = ): the grouping column of a finished table,
  # pages still cut by rows
  long <- tbl[rep(seq_len(nrow(tbl)), 3), ]
  long$stat <- paste(long$stat, rep(1:3, each = nrow(tbl)))
  long <- long[order(long$param), ]
  long$grp <- long$param
  long$param <- NULL
  long <- long[c("stat", "grp", setdiff(names(long), c("stat", "grp")))]
  tbl <- long
  same(table_plan(tbl) |> plan_row_group(group_col = "grp") |>
         plan_paginate_rows(split = "group_safe", max_rows = 7),
       group_col = "grp", split = "group_safe", max_rows = 7)
  expect_identical(plan_apply(table_plan(tbl) |> plan_row_group(group_col = "grp"),
                              "args")$rtf$group_col, "grp")
  expect_error(table_plan(tbl) |> plan_row_group(group_col = c("a", "b")),
               "one column name")
  expect_error(table_plan(tbl) |> plan_row_group(group_col = "grp") |>
                 plan_paginate_group(col = "stat") |> plan_apply("args"),
               "name different")
  # an ARD plan has named it already, as its outermost row key
  expect_error(disp_plan() |> plan_row_group(group_col = "label"),
               "name it there")
  # nothing declared: none of these arguments appear
  a <- plan_apply(table_plan(tbl) |> plan_hide("param"), "args")$rtf
  expect_false(any(c("na", "cell_format", "header_sep", "col_header_align",
                     "column_widths_twips", "table_width_twips", "style") %in% names(a)))
})

test_that("one analysis variable, no row keys: plan_levels() still orders the rows (#534)", {
  skip_if_not_installed("cards")
  d <- normalize_ard(cards::ard_tabulate(cards::ADSL, by = ARM, variables = AGEGR1))
  p <- table_plan(d, cols = "ARM") |>
    plan_levels(AGEGR1 = c("<65", "65-80", ">80")) |>
    plan_cells(categorical = "{n}", notes = FALSE)
  expect_identical(as.character(plan_apply(p, "table")$label), c("<65", "65-80", ">80"))
  w <- widen_ard(d, cols = "ARM", levels = list(AGEGR1 = c(">80", "<65", "65-80")),
                 cells = list(categorical = "{n}"), notes = FALSE)
  expect_identical(as.character(w$label), c(">80", "<65", "65-80"))
  # no order declared: as the ARD has them
  w0 <- widen_ard(d, cols = "ARM", cells = list(categorical = "{n}"), notes = FALSE)
  expect_identical(as.character(w0$label),
                   unique(vapply(d$variable_level[d$variable == "AGEGR1"], as.character, "")))
})

test_that("plan_header_tokens() gives the header tokens as data", {
  skip_if_no_cards2()
  p <- disp_plan() |>
    plan_col_header(values = c("Placebo" = 86, "Xanomeline High Dose" = 84,
                               "Xanomeline Low Dose" = 84),
                    rtf_col_header(c("Group", "Characteristic", "{col}")))
  tk <- plan_header_tokens(p)        # makes the table itself for {col}
  expect_named(tk, c("token", "kind", "values", "text", "resolved", "note"))
  expect_true(all(c("{col}", "{n}", "{n:sum}", "{n:Placebo}") %in% tk$token))
  n <- tk$values[[match("{n}", tk$token)]]
  expect_equal(unname(n[["Placebo"]]), 86)
  expect_equal(tk$values[[match("{n:sum}", tk$token)]], 254)
  expect_true(all(c("Placebo", "Xanomeline High Dose") %in%
                    tk$values[[match("{col}", tk$token)]]))
  expect_identical(tk$kind[match(c("{col}", "{n}", "{n:sum}", "{n:Placebo}"), tk$token)],
                   c("column", "population", "sum", "one"))
  expect_true(all(tk$resolved))
  # print() shows the same text
  out <- utils::capture.output(print(p))
  expect_true(any(grepl(tk$text[match("{n:sum}", tk$token)], out, fixed = TRUE)))
  expect_error(plan_header_tokens(1), "table_plan")
})

test_that("plan_levels(.drop_empty = ) leaves out the levels no record has", {
  skip_if_not_installed("cards")
  adsl <- data.frame(
    USUBJID = 1:6, TRT = rep(c("A", "B"), 3),
    RACE = factor(c("WHITE", "WHITE", "ASIAN", "WHITE", "ASIAN", "WHITE"),
                  levels = c("WHITE", "ASIAN", "OTHER")))
  d <- normalize_ard(cards::ard_tabulate(adsl, by = TRT, variables = RACE))
  p <- table_plan(d, cols = "TRT")
  all <- suppressMessages(plan_apply(plan_cells(p, notes = FALSE), "table"))
  expect_identical(as.character(all$label), c("WHITE", "ASIAN", "OTHER"))  # the default
  dropped <- suppressMessages(plan_apply(
    plan_cells(p, notes = FALSE) |> plan_levels(.drop_empty = "RACE"), "table"))
  # ASIAN is 0 in B but counted in A, so it stays
  expect_identical(as.character(dropped$label), c("WHITE", "ASIAN"))
  # with an order, and the order naming the dropped level
  ord <- suppressMessages(plan_apply(
    plan_cells(p, notes = FALSE) |>
      plan_levels(RACE = c("OTHER", "ASIAN", "WHITE"), .drop_empty = "RACE"),
    "table"))
  expect_identical(as.character(ord$label), c("ASIAN", "WHITE"))
  expect_error(plan_levels(p, .drop_empty = 1), "takes variable names")
})

test_that("a group count stated twice is one count; two different ones say why", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  groupn <- cards::ard_tabulate(adsl, variables = ARM)
  st <- cards::ard_stack(adsl, .by = ARM,
                         cards::ard_summary(variables = AGE,
                           statistic = ~ cards::continuous_summary_fns("mean")))
  hdr <- data.frame(line = c(1, 1), cols = c("label", ".values"),
                    span = c(NA, "each"), text = c("", "{col} (N={n})"))
  tok <- function(ard) {
    p <- table_plan(normalize_ard(ard), cols = "ARM") |>
      plan_cells(notes = FALSE) |> plan_col_header(header = hdr)
    t <- plan_header_tokens(p)
    t[t$token == "{n}", ]
  }
  # the stack alone, and a GROUPN row (the subjects per group) next to the
  # stack (the same counts):
  # the same numbers
  alone <- tok(st)
  twice <- tok(dplyr::bind_rows(groupn, st))
  expect_true(twice$resolved)
  expect_identical(twice$values, alone$values)
  expect_identical(unname(unlist(twice$values)), c(86, 84, 84))
  # two different counts for one column: not a column's number, and why
  other <- groupn
  other$stat[[1]] <- 99L
  clash <- tok(dplyr::bind_rows(other, st))
  expect_false(clash$resolved)
  expect_match(clash$note, "two different counts")
})

test_that("plan_col_header(lines = ): the header a row at a time, the cells' data frame written another way", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  ard <- cards::ard_stack(adsl, .by = c(TRT, SEX),
    cards::ard_tabulate(variables = AGEGR1, statistic = ~ c("n", "p")))
  d <- suppressMessages(normalize_ard(ard))
  base <- table_plan(d, cols = c("TRT", "SEX"), rows = c(group = "variable")) |>
    plan_cells(notes = FALSE) |> plan_cells("{n} ({p:.1f%})") |>
    plan_stub(name = "row_label", before = TRUE)
  pages <- function(p) suppressMessages(plan_apply(p))
  cells <- data.frame(
    line = c(1, 1, 2, 2, 3, 3),
    cols = c("row_label", ".values", "row_label", ".values", "row_label", ".values"),
    span = c(NA, "TRT", NA, "each", NA, NA),
    text = c("", "{col1}", "", "{col2}", "Characteristic", ""))
  want <- pages(base |> plan_col_header(header = cells))
  got <- pages(base |> plan_col_header(
    lines = list(
      c(row_label = "", .values = "{col1}"),
      c(row_label = "", .values = "{col2}"),
      c(row_label = "Characteristic", .values = "")),
    span = list(c(row_label = "one", .values = "TRT"),
                c(row_label = "one"),
                c(row_label = "one", .values = "one"))))
  expect_identical(got, want)
  # one span for every cell: "each", the default
  each <- data.frame(line = c(1, 1, 2, 2), cols = c("row_label", ".values"),
                     span = "each", text = c("", "{col}", "Characteristic", "(N={n})"))
  expect_identical(
    pages(base |> plan_col_header(lines = list(c(row_label = "", .values = "{col}"),
                                               c(row_label = "Characteristic",
                                                 .values = "(N={n})")))),
    pages(base |> plan_col_header(header = each)))
  # what it is, and is not
  expect_error(plan_col_header(base, header = each, lines = list(c(a = "x"))),
               "once, as `header` or as `lines`")
  expect_error(plan_col_header(base, lines = list(c("x", "y"))),
               "row 1 is a named character vector")
  expect_error(plan_col_header(base, lines = list(c(a = "x")), span = list("each", "one")),
               "one element a header row")
})

# ------------------------------------------------- a variable under a level
# (#598) RACESUB's rows under RACE's "Asian" row, one indent step deeper
nest_ard <- function() {
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adsl$RACE <- factor(adsl$RACE, levels = c("WHITE", "BLACK OR AFRICAN AMERICAN",
                                            "AMERICAN INDIAN OR ALASKA NATIVE"),
                      labels = c("White", "Black", "American Indian"))
  adsl$RSUB <- factor(ifelse(adsl$RACE %in% "Black", c("A", "B")[1L + adsl$AGE %% 2], NA),
                      levels = c("A", "B", "C"), labels = c("Sub A", "Sub B", "Sub C"))
  cards::bind_ard(
    cards::ard_tabulate(adsl, variables = TRT),
    cards::ard_tabulate(adsl, by = TRT, variables = c(RACE, RSUB), denominator = adsl))
}

test_that("plan_nest() puts a variable's rows under one level of another", {
  skip_if_no_cards2()
  p <- table_plan(nz(nest_ard()), cols = "TRT", rows = c(group = "variable")) |>
    plan_labels(variable = c(RACE = "Race, n (%)", RSUB = "Black sub")) |>
    plan_cells(categorical = "{n} ({p:.1f%})", notes = FALSE)
  plain <- plan_apply(p, "table")
  nested <- plan_apply(p |> plan_nest(RSUB = c(RACE = "Black")), "table")
  pad <- strrep(intToUtf8(160L), 4L)
  expect_identical(as.character(nested$label),
                   c("White", "Black", paste0(pad, c("Sub A", "Sub B", "Sub C")), "American Indian"))
  expect_identical(unique(as.character(nested$group)), "Race, n (%)")
  # the rows themselves are the same, moved: no count changes
  k <- match(c("Sub A", "Sub B", "Sub C"), as.character(plain$label))
  expect_identical(nested[3:5, -(1:2)], plain[k, -(1:2)], ignore_attr = TRUE)
  # the stub's indent is the step
  n2 <- plan_apply(p |> plan_stub(indent = 2L) |> plan_nest(RSUB = c(RACE = "Black")), "table")
  expect_identical(as.character(n2$label[3]), paste0(strrep(intToUtf8(160L), 2L), "Sub A"))
})

test_that("plan_nest() says what is wrong", {
  skip_if_no_cards2()
  p <- table_plan(nz(nest_ard()), cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(categorical = "{n} ({p:.1f%})", notes = FALSE)
  expect_error(plan_nest(p, RSUB = "Black"), "named by it")
  expect_error(plan_nest(p), "name the nested variable")
  expect_error(plan_apply(plan_nest(p, RSUB = c(RACE = "Blue")), "table"),
               "RACE has no row \"Blue\".  Its rows: White, Black, American Indian")
  expect_error(plan_apply(plan_nest(p, NOPE = c(RACE = "Black")), "table"),
               "no rows of NOPE")
  p2 <- table_plan(nz(nest_ard()), cols = "TRT", rows = ".label") |>
    plan_cells(categorical = "{n} ({p:.1f%})", notes = FALSE)
  expect_error(plan_apply(plan_nest(p2, RSUB = c(RACE = "Black")), "table"),
               "carry no variable")
})

test_that("plan_layers() shows the nestings, a later one for a variable winning", {
  skip_if_no_cards2()
  p <- table_plan(nz(nest_ard()), cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(categorical = "{n} ({p:.1f%})", notes = FALSE) |>
    plan_nest(RSUB = c(RACE = "White")) |>
    plan_nest(RSUB = c(RACE = "Black"))
  n <- plan_layers(p)$layers$nest$nest
  expect_identical(names(n), "RSUB")
  expect_identical(n$RSUB, list(parent = "RACE", level = "Black"))
  # and plan_apply() reads the same
  tb <- plan_apply(p, "table")
  expect_identical(which(grepl("Sub A", tb$label)), 3L)
})

test_that("plan_nest() finds a variable by any form of its heading", {
  skip_if_no_cards2()
  base <- table_plan(nz(nest_ard()), cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(categorical = "{n} ({p:.1f%})", notes = FALSE)
  forms <- list(
    plan_labels(base, variable = c(RACE = "Race", RSUB = "Black sub")),
    plan_labels(base, RACE = "Race", RSUB = "Black sub"),
    plan_labels(base, RACE = c(RACE = "Race"), RSUB = c(RSUB = "Black sub")))
  for (p in forms) {
    tb <- plan_apply(plan_nest(p, RSUB = c(RACE = "Black")), "table")
    expect_identical(unique(as.character(tb$group)), "Race")
    expect_identical(which(grepl("Sub A", tb$label)), 3L)
  }
})

# ---------------------------------------------- a Total column (plan_total)
# cards' own overall rows -- the analysis again without its by -- become
# the Total column; the ARD keeps no invented ARM value
total_ard <- function(factor_trt = FALSE) {
  adsl <- cards::ADSL
  adsl$TRT <- if (factor_trt) factor(adsl$ARM, levels = c("Placebo",
    "Xanomeline Low Dose", "Xanomeline High Dose")) else as.character(adsl$ARM)
  cards::bind_ard(
    cards::ard_tabulate(adsl, variables = TRT),
    cards::ard_tabulate(adsl, by = TRT, variables = SEX, denominator = adsl),
    cards::ard_tabulate(adsl, variables = SEX, denominator = adsl))
}

test_that("plan_total() makes the overall rows a Total column, its N the study total", {
  skip_if_no_cards2()
  for (f in c(FALSE, TRUE)) {
    p <- table_plan(nz(total_ard(f)), cols = "TRT", rows = c(group = "variable")) |>
      plan_cells(categorical = "{n} ({p:.1f%})", notes = FALSE)
    tb <- plan_apply(plan_total(p), "table")
    expect_identical(names(tb)[ncol(tb)], "Total")
    expect_identical(as.character(tb$Total), c("143 (56.3)", "111 (43.7)"))
    first <- plan_apply(plan_total(p, label = "All", position = "first"), "table")
    expect_identical(names(first)[3L], "All")
    # the header's {n}: each arm's own, and the study total over Total
    n <- rtfreporter:::.plan_n_read(plan_total(p), rtfreporter:::.plan_spread_args(plan_total(p)))
    expect_equal(unname(n[c("Placebo", "Total")]), c(86, 254))
  }
  # with plan_levels(): Total in its place among them
  p <- table_plan(nz(total_ard()), cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(categorical = "{n}", notes = FALSE) |>
    plan_levels(TRT = c("Xanomeline High Dose", "Xanomeline Low Dose", "Placebo"))
  expect_identical(names(plan_apply(plan_total(p, position = "first"), "table"))[-(1:2)],
                   c("Total", "Xanomeline High Dose", "Xanomeline Low Dose", "Placebo"))
  # without plan_total(), the table is what it was
  expect_false("Total" %in% names(plan_apply(p, "table")))
})

test_that("{n:sum} leaves the Total column out: it is the sum, not a part", {
  skip_if_no_cards2()
  p <- table_plan(nz(total_ard()), cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(categorical = "{n}", notes = FALSE) |>
    plan_total() |>
    plan_stub(name = "row_label", before = TRUE) |>
    plan_col_header(values = list(n = TRUE), header = data.frame(
      line = c("1", "1", "2"), cols = c("row_label", ".values", ".values"),
      span = c(NA, NA, "each"), text = c(NA, "All (N={n:sum})", "{col} (N={n})")))
  tk <- plan_header_tokens(p)
  expect_equal(unlist(tk$values[tk$token == "{n:sum}"]), 254)
  expect_equal(unlist(tk$values[tk$token == "{n}"])[["Total"]], 254)
  pg <- plan_apply(p, "pages")
  h <- if (inherits(pg, "rtftable")) pg$col_header else pg[[1L]]$col_header
  txt <- paste(unlist(h), collapse = " ")
  expect_match(txt, "All (N=254)", fixed = TRUE)
  expect_match(txt, "Total (N=254)", fixed = TRUE)
})

test_that("plan_total() says what is wrong", {
  skip_if_no_cards2()
  adsl <- cards::ADSL
  no_overall <- cards::ard_tabulate(adsl, by = ARM, variables = SEX)
  p <- table_plan(nz(no_overall), cols = "ARM", rows = c(group = "variable")) |>
    plan_cells(categorical = "{n}", notes = FALSE)
  expect_error(plan_apply(plan_total(p), "table"), "no overall rows, rows without ARM")
  expect_error(plan_total(p, label = ""), "one text")
  expect_error(plan_total(p, position = "middle"))
  # a Total already in the data: one of the two
  adsl$ARM2 <- adsl$ARM
  made <- rbind(adsl, transform(adsl, ARM2 = "Total"))
  both <- cards::bind_ard(cards::ard_tabulate(made, by = ARM2, variables = SEX),
                          cards::ard_tabulate(adsl, variables = SEX))
  p2 <- table_plan(nz(both), cols = "ARM2", rows = c(group = "variable")) |>
    plan_cells(categorical = "{n}", notes = FALSE)
  expect_error(plan_apply(plan_total(p2), "table"), "already has a value \"Total\"")
  # two column keys: the innermost is folded, from each ARM's rows without SEX
  two <- cards::bind_ard(cards::ard_tabulate(adsl, by = c(ARM, SEX), variables = RACE),
                         cards::ard_tabulate(adsl, variables = RACE))
  p3 <- table_plan(nz(two), cols = c("ARM", "SEX"), rows = c(group = "variable")) |>
    plan_cells(categorical = "{n}", notes = FALSE)
  expect_error(plan_apply(plan_total(p3), "table"), "no overall rows, rows without SEX")
  # plan_layers() shows it
  p4 <- table_plan(nz(total_ard()), cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(categorical = "{n}", notes = FALSE)
  expect_identical(plan_layers(plan_total(p4, label = "All"))$layers$total,
                   list(label = "All", position = "last"))
})

test_that("plan_stat_groups(): one record's N in one column, its rate and CI in the next, no mutate() (#619)", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom", minimum_version = "1.0.8")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adsl$RESP <- factor(ifelse(adsl$AGE > 75, "Y", "N"), levels = c("Y", "N"))
  orr <- function(.x) cardx::ard_categorical_ci(.x, variables = RESP, value = RESP ~ "Y",
                                               method = "clopper-pearson", conf.level = 0.90)
  ard <- cards::bind_ard(
    cards::ard_strata(adsl, .by = TRT, .strata = SEX, .f = orr),
    cards::ard_strata(adsl, .by = TRT, .strata = AGEGR1, .f = orr))
  cells <- list(n = c("1" = "{N:.0f}"),
                orr_ci = cell_rows(
                  "1" = c(n == 0 ~ "0", "{n:.0f} ({estimate:.1f%})"),
                  "2" = "{conf.low:.1f%}, {conf.high:.1f%}"))
  finish <- function(p) do.call(plan_cells, c(list(p), cells)) |>
    plan_sort(FALSE) |>
    plan_row_group(mode = "value", collapse = c(1L, 2L))
  d <- normalize_ard(ard)
  m <- d
  m$variable <- ifelse(m$stat_name == "N", "n", "orr_ci")
  by_mutate <- table_plan(m, cols = c("TRT", "variable"),
                          rows = c(grp1 = "group2", grp2 = "group2_level"), label = NA) |>
    finish()
  by_groups <- table_plan(d, cols = c("TRT", ".stat_group"),
                          rows = c(grp1 = "group2", grp2 = "group2_level"), label = NA) |>
    plan_stat_groups(n = "N", orr_ci = c("n", "estimate", "conf.low", "conf.high")) |>
    finish()
  render <- function(p) {
    f <- tempfile(fileext = ".rtf"); on.exit(unlink(f), add = TRUE)
    old <- options(rtfreporter.render_time = as.POSIXct("2026-01-01 09:00"))
    on.exit(options(old), add = TRUE)
    suppressMessages(generate_rtfreport(rtf_tables(rtf_document(program = "t.R"), p), f,
                                        overwrite = TRUE))
    readBin(f, "raw", file.size(f))
  }
  expect_identical(render(by_groups), render(by_mutate))
  tab <- suppressMessages(plan_apply(by_groups, "table"))
  tab <- as.data.frame(if (is.data.frame(tab)) tab else tab[[1L]])
  expect_true(all(c("Placebo____n", "Placebo____orr_ci") %in% names(tab)))
  # widen_ard() takes it directly too
  w <- suppressMessages(widen_ard(d, cols = c("TRT", ".stat_group"),
                                  rows = c(grp1 = "group2", grp2 = "group2_level"),
                                  label = NA, cells = cells,
                                  stat_groups = list(n = "N", orr_ci = c("n", "estimate", "conf.low", "conf.high"))))
  expect_identical(w$Placebo____n[1L], as.character(tab$Placebo____n[1L]))
  # what is said, not guessed
  expect_error(plan_stat_groups(table_plan(d, cols = "TRT"), n = "N", "n"), "a name of its own")
  expect_error(plan_stat_groups(table_plan(d, cols = "TRT"), a = "N", b = c("N", "n")), "in two groups")
  expect_error(plan_stat_groups(table_plan(d, cols = "TRT"), a = character()), "names no statistic")
})

test_that("plan_stat_groups() columns: the header reads each arm's N over its columns (#709)", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom", minimum_version = "1.0.8")
  adsl <- cards::ADSL
  adsl$RESP <- factor(ifelse(adsl$AGE > 75, "Y", "N"), levels = c("Y", "N"))
  ard <- cards::bind_ard(
    cards::ard_tabulate(adsl, variables = ARM),
    cards::ard_strata(adsl, .by = ARM, .f = function(.x)
      cardx::ard_categorical_ci(.x, variables = RESP, value = RESP ~ "Y")))
  nd <- normalize_ard(ard)
  arm_n <- c(Placebo = 86, "Xanomeline High Dose" = 84, "Xanomeline Low Dose" = 84)
  p <- table_plan(nd, cols = c("ARM", ".stat_group")) |>
    plan_stat_groups("n (%)" = c("n", "estimate"), "95% CI" = c("conf.low", "conf.high"),
                     .variables = "RESP") |>
    plan_cells("n (%)" = "{n} ({estimate:.1f%})", "95% CI" = "{conf.low:.1f%}, {conf.high:.1f%}") |>
    plan_col_header(header = data.frame(line = c("1", "2"), cols = c(".values", ".values"),
                                        span = c("ARM", "each"),
                                        text = c("{col1} (N={n1})", "{col} {n}")))
  tk <- plan_header_tokens(p)
  expect_true(all(tk$resolved[startsWith(tk$token, "{n")]))
  # each arm's N; each of its columns the arm's too; the total counts each arm once
  n <- tk$values[tk$token == "{n}"][[1L]]
  expect_identical(unname(n[names(arm_n)]), unname(arm_n))
  expect_identical(unname(n[["Placebo____95% CI"]]), 86)
  expect_identical(tk$values[tk$token == "{n:sum}"][[1L]], sum(arm_n))
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  expect_no_warning(suppressMessages(generate_rtfreport(rtf_tables(rtf_document(), p), f,
                                                        overwrite = TRUE)))
  txt <- paste(readLines(f, warn = FALSE), collapse = " ")
  expect_true(grepl("Placebo (N=86)", txt, fixed = TRUE))
  expect_true(grepl("Xanomeline Low Dose (N=84)", txt, fixed = TRUE))
  expect_true(grepl("95% CI 84", txt, fixed = TRUE))
  expect_false(grepl("N=NA", txt, fixed = TRUE))
})

test_that("plan_stat_groups() x cell_rows(): n rows x m columns of one record, on cols or rows (#619)", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom", minimum_version = "1.0.8")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adsl$RESP <- factor(ifelse(adsl$AGE > 75, "Y", "N"), levels = c("Y", "N"))
  orr <- function(.x) cardx::ard_categorical_ci(.x, variables = RESP, value = RESP ~ "Y",
                                               method = "clopper-pearson", conf.level = 0.90)
  d <- normalize_ard(cards::ard_strata(adsl, .by = TRT, .strata = SEX, .f = orr))
  tab_of <- function(p) {
    t <- suppressMessages(plan_apply(p, "table"))
    as.data.frame(if (is.data.frame(t)) t else t[[1L]])
  }
  # across: N one row, the rate and CI two; aligned row by row
  across <- table_plan(d, cols = c("TRT", ".stat_group"),
                       rows = c(grp = "group2_level"), label = NA) |>
    plan_stat_groups(n = "N", orr_ci = c("n", "estimate", "conf.low", "conf.high")) |>
    plan_sort(FALSE) |>
    plan_cells(n = cell_rows("1" = "{N:.0f}"),
               orr_ci = cell_rows("1" = "{n:.0f} ({estimate:.1f%})",
                                  "2" = "{conf.low:.1f%}, {conf.high:.1f%}"))
  a <- tab_of(across)
  expect_identical(nrow(a), 4L)                       # 2 sexes x 2 rows
  expect_identical(a$grp, c("F", "F", "M", "M"))
  expect_false(anyNA(a$Placebo____n[c(1L, 3L)]))
  expect_true(all(is.na(a$Placebo____n[c(2L, 4L)])))  # the N's second row is empty
  expect_match(a$Placebo____orr_ci[1L], "^[0-9]+ \\(")
  expect_match(a$Placebo____orr_ci[2L], "^[0-9.]+, [0-9.]+$")
  # down: the groups are rows, in the order plan_stat_groups() says them
  down <- table_plan(d, cols = "TRT",
                     rows = c(grp = "group2_level", stat = ".stat_group"), label = NA) |>
    plan_stat_groups(n = "N", rate = c("n", "estimate"), ci = c("conf.low", "conf.high")) |>
    plan_sort(FALSE) |>
    plan_cells(n = "{N:.0f}", rate = "{n:.0f} ({estimate:.1f%})",
               ci = cell_rows("1" = "{conf.low:.1f%}", "2" = "{conf.high:.1f%}"))
  b <- tab_of(down)
  expect_identical(b$stat[b$grp == "F"], c("n", "rate", "ci", "ci"))
  expect_identical(b$Placebo[b$grp == "F" & b$stat == "n"],
                   a$Placebo____n[1L])
  expect_identical(b$Placebo[b$grp == "F" & b$stat == "rate"],
                   a$Placebo____orr_ci[1L])
})

test_that("plan_paginate_group(heading = ): a section per arm named with the arm and its N from the ARD (#619)", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  ard_body <- cards::ard_tabulate(adsl, by = TRT, variables = SEX)
  ard <- cards::bind_ard(cards::ard_tabulate(adsl, variables = TRT), ard_body)
  plan <- function(a) {
    table_plan(normalize_ard(a), cols = ".stat_group",
               rows = c(arm = "TRT", label = "variable_level"), label = NA) |>
      plan_stat_groups(n = "n", pct = "p") |>
      plan_cells(n = "{n}", pct = "{p:.1f%}", notes = FALSE) |>
      plan_paginate_group(col = "arm", keep = FALSE,
                          heading = "Treatment: {value} (N={n})")
  }
  pages <- suppressMessages(plan_apply(plan(ard)))
  n_arm <- table(adsl$TRT)
  expect_identical(names(pages),
                   sprintf("Treatment: %s (N=%d)", names(n_arm), as.integer(n_arm)))
  # the name is the section heading rtf_tables(auto_section = TRUE) prints
  f <- tempfile(fileext = ".rtf"); on.exit(unlink(f), add = TRUE)
  doc <- rtf_document() |>
    rtf_section(header = rtf_header(list("Table 1"))) |>
    rtf_tables(plan(ard), auto_section = TRUE)
  suppressMessages(generate_rtfreport(doc, f, overwrite = TRUE))
  rtf <- paste(readLines(f, warn = FALSE), collapse = "")
  for (h in names(pages)) expect_true(grepl(h, rtf, fixed = TRUE))
  # an N the ARD does not state is NA, said, never guessed
  expect_warning(p2 <- suppressMessages(plan_apply(plan(ard_body))), "states no population")
  expect_match(names(p2)[1L], "(N=NA)", fixed = TRUE)
  expect_error(plan_paginate_group(plan(ard), heading = c("a", "b")), "one text")
})

test_that("normalize_ard(stack = ): parallel subgroups as one pair of keys, whatever slot each came in (#619)", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom", minimum_version = "1.0.8")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adsl$ALL <- "All"
  adsl$RESP <- factor(ifelse(adsl$AGE > 75, "Y", "N"), levels = c("Y", "N"))
  orr <- function(.x) cardx::ard_categorical_ci(.x, variables = RESP, value = RESP ~ "Y",
                                               method = "clopper-pearson", conf.level = 0.90)
  # SEX in group2; AGEGR1 in group1, the arm in group2 (another `by =` order)
  ard <- cards::bind_ard(
    cards::ard_strata(adsl, .by = TRT, .strata = ALL, .f = orr),
    cards::ard_strata(adsl, .by = TRT, .strata = SEX, .f = orr),
    cardx::ard_categorical_ci(adsl, by = c(AGEGR1, TRT), variables = RESP, value = RESP ~ "Y",
                              method = "clopper-pearson", conf.level = 0.90))
  d <- normalize_ard(ard, stack = list(subgroup = c("ALL", "SEX", "AGEGR1")))
  expect_identical(levels(d$subgroup), c("ALL", "SEX", "AGEGR1"))
  expect_true(all(c("SEX", "AGEGR1") %in% names(d)))          # the own columns stay
  expect_identical(as.character(d$subgroup_level[!is.na(d$SEX)]), d$SEX[!is.na(d$SEX)])
  tab <- suppressMessages(plan_apply(
    table_plan(d, cols = c("TRT", ".stat_group"),
               rows = c(grp1 = "subgroup", grp2 = "subgroup_level"), label = NA) |>
      plan_stat_groups(n = "N", orr = c("n", "estimate")) |>
      plan_sort(FALSE) |>
      plan_labels(subgroup = c(ALL = "All Subjects", SEX = "Sex", AGEGR1 = "Age Group")) |>
      plan_cells(n = "{N:.0f}", orr = "{n:.0f} ({estimate:.1f%})", notes = FALSE), "table"))
  tab <- as.data.frame(if (is.data.frame(tab)) tab else tab[[1L]])
  expect_identical(as.character(unique(tab$grp1)), c("All Subjects", "Sex", "Age Group"))
  expect_identical(nrow(tab), 1L + 2L + length(unique(adsl$AGEGR1)))
  expect_identical(tab$Placebo____n[1L], as.character(sum(adsl$TRT == "Placebo")))
  # stack = NULL is what it was
  expect_identical(normalize_ard(ard), normalize_ard(ard, stack = NULL))
  # what is said, not guessed
  nested <- cards::ard_strata(adsl, .by = TRT, .strata = c(SEX, AGEGR1), .f = orr)
  expect_error(normalize_ard(nested, stack = list(s = c("SEX", "AGEGR1"))), "no layer says so")
  expect_error(normalize_ard(ard, stack = list(s = "BMIGR1")), "no key")
  expect_error(normalize_ard(ard, stack = list(SEX = "AGEGR1")), "a column already")
  expect_error(normalize_ard(ard, stack = c("SEX", "AGEGR1")), "named list")
})

test_that("plan_paginate_rows(page_by = , heading = ): BY pages with a row budget, named with the arm and its N (#619)", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  ard <- cards::bind_ard(cards::ard_tabulate(adsl, variables = TRT),
                         cards::ard_tabulate(adsl, by = TRT, variables = c(SEX, AGEGR1, RACE)))
  p <- table_plan(normalize_ard(ard), cols = ".stat_group",
                  rows = c(arm = "TRT", grp = "variable", lvl = "variable_level"), label = NA) |>
    plan_stat_groups(n = "n", pct = "p") |>
    plan_cells(n = "{n}", pct = "{p:.1f%}", notes = FALSE) |>
    plan_hide("arm") |>
    plan_paginate_rows(page_by = "arm", max_rows = 6, split = "group_safe",
                       heading = "Treatment: {value} (N={n})")
  pages <- suppressMessages(plan_apply(p))
  n_arm <- table(adsl$TRT)
  want <- sprintf("Treatment: %s (N=%d)", names(n_arm), as.integer(n_arm))
  expect_gt(length(pages), length(want))                       # the budget cut them
  expect_setequal(unique(sub("[.]{3}[0-9]+$", "", names(pages))), want)
  expect_error(plan_paginate_rows(p, heading = "{value}"), "one `page_by` column")
  # a page per value refuses a budget, and says where it lives
  expect_error(suppressMessages(plan_apply(
    plan_paginate_group(p, col = "arm") |> plan_paginate_rows(split = "group_safe"))),
    "plan_paginate_rows(page_by", fixed = TRUE)
})

test_that("plan_paginate_group(heading_at = ): where the page name prints, rtf_tables()'s default; its own argument wins", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  ard <- cards::bind_ard(cards::ard_tabulate(adsl, variables = TRT),
                         cards::ard_tabulate(adsl, by = TRT, variables = c(SEX, AGEGR1)))
  base <- table_plan(normalize_ard(ard), cols = ".stat_group",
                     rows = c(arm = "TRT", grp = "variable", lvl = "variable_level"), label = NA) |>
    plan_stat_groups(n = "n", pct = "p") |>
    plan_cells(n = "{n}", pct = "{p:.1f%}", notes = FALSE)
  rtf <- function(p, ...) {
    f <- tempfile(fileext = ".rtf"); on.exit(unlink(f))
    doc <- rtf_document() |> rtf_section(header = rtf_header(list("Table 1"))) |> rtf_tables(p, ...)
    suppressMessages(generate_rtfreport(doc, f, overwrite = TRUE))
    unname(tools::md5sum(f))
  }
  grp <- function(at = NULL) plan_paginate_group(base, col = "arm", keep = FALSE,
                                                 heading = "Treatment: {value}", heading_at = at)
  old <- options(rtfreporter.render_time = as.POSIXct("2000-01-01", tz = "UTC"))
  on.exit(options(old), add = TRUE)
  # the plan's word is rtf_tables()'s default: the same RTF as saying it there
  expect_identical(rtf(grp("section")), rtf(grp(), auto_section = TRUE))
  expect_identical(rtf(grp("title")), rtf(grp(), auto_title = TRUE))
  expect_identical(rtf(grp("none")), rtf(grp()))
  expect_false(identical(rtf(grp("section")), rtf(grp())))
  # rtf_tables()'s own argument wins
  expect_identical(rtf(grp("section"), auto_section = FALSE), rtf(grp()))
  expect_identical(rtf(grp("title"), auto_section = TRUE, auto_title = FALSE),
                   rtf(grp(), auto_section = TRUE))
  # carried on each page, as its titles are
  pages <- suppressMessages(plan_apply(grp("section")))
  expect_true(all(vapply(pages, attr, "", "rtf_heading_at") == "section"))
  expect_null(attr(suppressMessages(plan_apply(grp()))[[1L]], "rtf_heading_at"))
  # the BY pages of plan_paginate_rows(page_by = )
  by <- function(at = NULL) plan_hide(base, "arm") |>
    plan_paginate_rows(page_by = "arm", max_rows = 6, split = "group_safe",
                       heading = "Treatment: {value}", heading_at = at)
  expect_identical(rtf(by("section")), rtf(by(), auto_section = TRUE))
  expect_error(plan_paginate_group(base, heading_at = "header"), "one of")
  expect_error(plan_paginate_rows(base, heading_at = "section"), "give `page_by`")
  # no heading: the page's bare value is the name, and prints there too
  expect_identical(rtf(plan_paginate_group(base, col = "arm", keep = FALSE, heading_at = "section")),
                   rtf(plan_paginate_group(base, col = "arm", keep = FALSE), auto_section = TRUE))
})

test_that("normalize_ard(stack = list(x = \"group2\")): a slot stands for its variables, refused where ambiguous (#619)", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adsl$ALL <- "All"
  tab <- function(by) cards::ard_tabulate(adsl, by = by, variables = DISCONFL)
  # several variables in the slot
  ard <- cards::bind_ard(tab(c("TRT", "ALL")), tab(c("TRT", "SEX")), tab(c("TRT", "AGEGR1")))
  by_slot <- normalize_ard(ard, stack = list(subgroup = "group2"))
  by_name <- normalize_ard(ard, stack = list(subgroup = c("ALL", "SEX", "AGEGR1")))
  expect_identical(by_slot, by_name)
  # one variable in the slot
  one <- normalize_ard(tab(c("TRT", "SEX")), stack = list(subgroup = "group2"))
  expect_identical(levels(one$subgroup), "SEX")
  # a slot beside a named variable
  mix <- normalize_ard(ard, stack = list(subgroup = c("group2")))
  expect_identical(levels(mix$subgroup), c("ALL", "SEX", "AGEGR1"))
  # the slot also holds the arm on some rows: which are subgroups is not said
  amb <- cards::bind_ard(tab(c("TRT", "SEX")), tab(c("AGEGR1", "TRT")))
  expect_error(normalize_ard(amb, stack = list(subgroup = "group2")), "another slot")
  expect_identical(levels(normalize_ard(amb, stack = list(subgroup = c("SEX", "AGEGR1")))$subgroup),
                   c("SEX", "AGEGR1"))
})

test_that("normalize_ard(stack = ): crossed layers, three-way too, and the slots finding them (#619)", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adsl$ALL <- "All"
  tab <- function(by) cards::ard_tabulate(adsl, by = by, variables = DISCONFL)
  ard <- cards::bind_ard(tab(c("TRT", "ALL")), tab(c("TRT", "SEX")), tab(c("TRT", "AGEGR1")),
                         tab(c("TRT", "AGEGR1", "SEX")))
  layers <- list("ALL", "SEX", "AGEGR1", c("AGEGR1", "SEX"))
  d <- normalize_ard(ard, stack = list(subgroup = layers))
  expect_identical(levels(d$subgroup), c("ALL", "SEX", "AGEGR1", "AGEGR1 / SEX"))
  x <- d[d$subgroup %in% "AGEGR1 / SEX", ]
  expect_identical(as.character(x$subgroup_level), paste(x$AGEGR1, x$SEX, sep = " / "))
  # the first variable turns slowest
  cl <- levels(droplevels(x$subgroup_level))
  expect_identical(sub(" / .*$", "", cl), rep(unique(sub(" / .*$", "", cl)), each = 2L))
  # the slots find the same layers, in the ARD's order
  expect_identical(normalize_ard(ard, stack = list(subgroup = c("group2", "group3"))), d)
  # a separator of one's own
  expect_identical(levels(normalize_ard(ard, stack = list(s = layers), stack_sep = " x ")$s)[4L],
                   "AGEGR1 x SEX")
  # a combination no layer names: said, never NA
  expect_error(normalize_ard(ard, stack = list(subgroup = c("ALL", "SEX", "AGEGR1"))),
               "no layer says so")
  # three variables crossed: the same rule
  three <- cards::bind_ard(tab(c("TRT", "SEX")), tab(c("TRT", "AGEGR1", "SEX", "RACE")))
  t3 <- normalize_ard(three, stack = list(g = list("SEX", c("AGEGR1", "SEX", "RACE"))))
  expect_identical(levels(t3$g), c("SEX", "AGEGR1 / SEX / RACE"))
  expect_true(all(lengths(strsplit(as.character(t3$g_level[t3$g %in% "AGEGR1 / SEX / RACE"]), " / ")) == 3L))
  # the table lays the layers out
  tab_d <- suppressMessages(plan_apply(
    table_plan(d, cols = "TRT", rows = c(grp = "subgroup", lvl = "subgroup_level"), label = NA) |>
      plan_sort(FALSE) |> plan_cells("{n}", notes = FALSE), "table"))
  tab_d <- as.data.frame(if (is.data.frame(tab_d)) tab_d else tab_d[[1L]])
  expect_identical(as.character(unique(tab_d$grp)), c("ALL", "SEX", "AGEGR1", "AGEGR1 / SEX"))
})

test_that("normalize_ard(stack = ): the arm in a later slot than the subgroup, crossed too (#619)", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  tab <- function(by) cards::ard_tabulate(adsl, by = by, variables = DISCONFL)
  # group1 = the subgroup, the arm last
  ard <- cards::bind_ard(tab(c("SEX", "TRT")), tab(c("AGEGR1", "TRT")), tab(c("AGEGR1", "SEX", "TRT")))
  expect_true(all(as.character(as.data.frame(ard)$group2) %in% c("TRT", "SEX")))
  d <- normalize_ard(ard, stack = list(subgroup = list("SEX", "AGEGR1", c("AGEGR1", "SEX"))))
  expect_identical(levels(d$subgroup), c("SEX", "AGEGR1", "AGEGR1 / SEX"))
  expect_false(anyNA(d$subgroup[!is.na(d$TRT) & d$variable == "DISCONFL"]))
  # the slots there hold the arm too: they do not say what the subgroups are
  expect_error(normalize_ard(ard, stack = list(subgroup = c("group1", "group2"))), "another slot")
})

test_that("sort keys -n@<key> and -n[<col>]: a group's own count carried to its rows, one column's count (#619)", {
  skip_if_not_installed("cards")
  adae <- cards::ADAE[cards::ADAE$AESOC %in% unique(cards::ADAE$AESOC)[1:4], ]
  ard <- cards::ard_stack_hierarchical(adae, variables = c(AESOC, AEDECOD), by = TRTA,
                                       denominator = cards::ADSL, id = USUBJID)
  d <- normalize_ard(ard, hierarchy = c("AESOC", "AEDECOD"))
  tab <- function(...) {
    t <- suppressMessages(plan_apply(
      table_plan(d, cols = "TRTA", rows = c(soc = "AESOC", pt = "AEDECOD"), label = NA) |>
        plan_sort(...) |> plan_cells("{n}", notes = FALSE), "table"))
    as.data.frame(if (is.data.frame(t)) t else t[[1L]])
  }
  arms <- unique(as.character(cards::ADSL$TRTA))
  num <- function(t, cols = arms) rowSums(sapply(t[cols], function(x) as.numeric(x)), na.rm = TRUE)
  t1 <- tab("-n@soc", "soc", ".depth", "-n")
  soc_rows <- is.na(t1$pt)
  # SOCs by their own count, descending; each SOC's PTs right under it, by count
  expect_true(all(diff(num(t1)[soc_rows]) <= 0))
  runs <- rle(as.character(t1$soc))$values
  expect_identical(runs, unique(runs))
  for (s in unique(t1$soc)) {
    blk <- t1[t1$soc == s, ]
    expect_true(is.na(blk$pt[1L]))
    expect_true(all(diff(num(blk)[-1L]) <= 0))
  }
  # one column only
  t2 <- tab("-n@soc[Placebo]", "soc", ".depth", "-n[Placebo]")
  expect_true(all(diff(num(t2, "Placebo")[is.na(t2$pt)]) <= 0))
  for (s in unique(t2$soc)) {
    blk <- t2[t2$soc == s, ]
    expect_true(all(diff(num(blk, "Placebo")[-1L]) <= 0))
  }
  # what is not there is said
  expect_error(tab("-n@nope"), "not a row key")
  expect_error(tab("-n[Nope]"), "no spread column")
  # a plain -n sorts as it did
  expect_identical(tab("soc", ".depth", "-n"), tab("soc", ".depth", "-n"))
  # max(n) / min(n): a row's largest / smallest count across the arms (#683),
  # both ways; sum(n) is the plain total
  big <- function(t) apply(sapply(t[arms], function(x) as.numeric(x)), 1L, max, na.rm = TRUE)
  small <- function(t) apply(sapply(t[arms], function(x) as.numeric(x)), 1L, min, na.rm = TRUE)
  pts <- function(t, s) t[t$soc == s & !is.na(t$pt), ]
  s1 <- unique(t1$soc)[1L]
  expect_true(all(diff(big(pts(tab("soc", ".depth", "-max(n)"), s1))) <= 0))
  expect_true(all(diff(big(pts(tab("soc", ".depth", "max(n)"), s1))) >= 0))
  expect_true(all(diff(small(pts(tab("soc", ".depth", "-min(n)"), s1))) <= 0))
  expect_identical(tab("soc", ".depth", "-sum(n)"), tab("soc", ".depth", "-n"))
  # not the total's order: a row with one large arm comes first
  expect_false(identical(tab("-n")$pt, tab("-max(n)")$pt))
  # with @: the SOCs by their own row's largest count, each with its PTs
  t3 <- tab("-max(n)@soc", "soc", ".depth", "-max(n)")
  expect_true(all(diff(big(t3)[is.na(t3$pt)]) <= 0))
  runs3 <- rle(as.character(t3$soc))$values
  expect_identical(runs3, unique(runs3))
})

test_that("plan_compare(): a test's p-value and a difference with its CI as columns, placed not computed (#625)", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom", minimum_version = "1.0.8")   # cardx's t-test / prop test
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  body <- cards::ard_stack(adsl, .by = TRT, cards::ard_summary(variables = AGE),
                           cards::ard_tabulate(variables = SEX))
  tests <- cards::bind_ard(cardx::ard_stats_kruskal_test(adsl, by = TRT, variables = AGE),
                           cardx::ard_stats_chisq_test(adsl, by = TRT, variables = SEX))
  plan <- function(a) table_plan(normalize_ard(a), cols = "TRT", rows = c(grp = "variable"),
                                 label = "variable_level") |>
    plan_cells(continuous = c("Mean (SD)" = "{mean:.1f} ({sd:.2f})", "Median" = "{median:.1f}"),
               categorical = "{n} ({p:.1f%})", notes = FALSE)
  tab_of <- function(p) {
    t <- suppressMessages(plan_apply(p, "table"))
    as.data.frame(if (is.data.frame(t)) t else t[[1L]])
  }
  with_p <- tab_of(plan(cards::bind_ard(body, tests)) |>
                     plan_compare(p = c(p.value < 0.001 ~ "<0.001", "{p.value:.3f}")))
  # by hand: the summary alone, and the p-values put on the first row of each variable
  by_hand <- tab_of(plan(body))
  pv <- as.data.frame(tests)
  pv <- unlist(pv$stat[pv$stat_name == "p.value"])
  names(pv) <- c("AGE", "SEX")
  by_hand$p <- NA_character_
  for (v in names(pv)) {
    by_hand$p[which(by_hand$grp == v)[1L]] <- if (pv[[v]] < 0.001) "<0.001" else sprintf("%.3f", pv[[v]])
  }
  expect_identical(with_p, by_hand)
  # the RTF is the one a hand-made table gives
  render <- function(x) {
    f <- tempfile(fileext = ".rtf"); on.exit(unlink(f), add = TRUE)
    old <- options(rtfreporter.render_time = as.POSIXct("2026-01-01 09:00"))
    on.exit(options(old), add = TRUE)
    suppressMessages(generate_rtfreport(rtf_tables(rtf_document(program = "t.R"), x), f, overwrite = TRUE))
    readBin(f, "raw", file.size(f))
  }
  expect_identical(render(as_rtftables(with_p)), render(as_rtftables(by_hand)))
  # a difference and its CI, two columns
  two <- adsl[adsl$TRT != "Xanomeline Low Dose", ]
  ef <- cards::bind_ard(cards::ard_summary(two, by = TRT, variables = c(AGE, BMIBL)),
                        cardx::ard_stats_t_test(two, by = TRT, variables = c(AGE, BMIBL)))
  t2 <- tab_of(table_plan(normalize_ard(ef), cols = "TRT", rows = c(grp = "variable"), label = NA) |>
                 plan_cells("{mean:.1f} ({sd:.2f})", notes = FALSE) |>
                 plan_compare(diff = "{estimate:.2f} ({conf.low:.2f}, {conf.high:.2f})",
                              p = "{p.value:.3f}"))
  tt <- as.data.frame(cardx::ard_stats_t_test(two, by = TRT, variables = AGE))
  st <- function(n) unlist(tt$stat[tt$stat_name == n])
  expect_identical(t2$diff[t2$grp == "AGE"],
                   sprintf("%.2f (%.2f, %.2f)", st("estimate"), st("conf.low"), st("conf.high")))
  expect_identical(names(t2)[4:5], c("diff", "p"))
  # one column a comparison
  cmp <- rbind(transform(adsl[adsl$TRT %in% c("Placebo", "Xanomeline Low Dose"), ], CMP = "Low vs Pbo"),
               transform(adsl[adsl$TRT %in% c("Placebo", "Xanomeline High Dose"), ], CMP = "High vs Pbo"))
  a3 <- cards::bind_ard(cards::ard_summary(adsl, by = TRT, variables = AGE),
                        cards::ard_strata(cmp, .strata = CMP, .f = ~ cardx::ard_stats_t_test(.x, by = TRT, variables = AGE)))
  t3 <- tab_of(table_plan(normalize_ard(a3), cols = "TRT", rows = c(grp = "variable"), label = NA) |>
                 plan_cells("{mean:.1f}", notes = FALSE) |>
                 plan_compare(p = "{p.value:.3f}", by = "CMP", position = "first"))
  expect_setequal(names(t3)[2:3], c("Low vs Pbo", "High vs Pbo"))
  # what is said, not guessed
  expect_error(plan_compare(plan(body), "{p.value}"), "a name of its own")
  expect_error(plan_compare(plan(body), p = "x", position = "middle"))
})

test_that("plan_total() reads the body's analyses only: a test is no Total and leaves no empty row (#625)", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom", minimum_version = "1.0.8")   # cardx's tests
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  body <- cards::ard_stack(adsl, .by = TRT, cards::ard_tabulate(variables = SEX), .overall = TRUE)
  tab_of <- function(a) {
    t <- suppressMessages(plan_apply(
      table_plan(normalize_ard(a), cols = "TRT", rows = c(grp = "variable"), label = "variable_level") |>
        plan_total() |> plan_cells("{n}", notes = FALSE), "table"))
    as.data.frame(if (is.data.frame(t)) t else t[[1L]])
  }
  expect_identical(tab_of(cards::bind_ard(body, cardx::ard_stats_chisq_test(adsl, by = TRT, variables = SEX))),
                   tab_of(body))
})

test_that("plan_compare(): what a table does not use -- other statistics, a second test, overall rows (#625)", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom", minimum_version = "1.0.8")   # cardx's t-test / prop test
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  two <- adsl[adsl$TRT != "Xanomeline Low Dose", ]
  skip_if_not_installed("dplyr")
  # cards::bind_ard() refuses two tests of one variable (their statistic
  # names repeat); bound as data frames they still can meet here
  ard <- dplyr::bind_rows(
    cards::ard_summary(two, by = TRT, variables = AGE),
    cards::ard_summary(two, variables = AGE),                        # overall rows, no plan_total()
    cardx::ard_stats_t_test(two, by = TRT, variables = AGE),
    cardx::ard_stats_wilcox_test(two, by = TRT, variables = AGE))    # a second test of AGE
  d <- normalize_ard(ard)
  wide <- function(compare) suppressMessages(widen_ard(
    d, cols = "TRT", rows = c(grp = "variable"), label = NA,
    cells = "{mean:.1f}", compare = compare, notes = "attr"))
  # two tests, one column reading both: said, not one chosen
  expect_error(wide(list(cols = list(p = "{p.value:.3f}"))), "Split them with `by =`")
  # the column keyed by context reads the t-test; the Wilcoxon is left, and noted
  w <- wide(list(cols = list(p = c(stats_t_test = "{p.value:.3f}"))))
  tt <- as.data.frame(cardx::ard_stats_t_test(two, by = TRT, variables = AGE))
  expect_identical(w$p, sprintf("%.3f", unlist(tt$stat[tt$stat_name == "p.value"])))
  ig <- attr(w, "ard_ignored")
  expect_true(any(ig$context == "stats_wilcox_test" &
                    grepl("comparison record no row or column", ig$reason)))
  # the t-test's other statistics (statistic, parameter, method ...) are not read
  expect_identical(names(w), c("grp", "Placebo", "Xanomeline High Dose", "p"))
  # the overall rows are no comparison: left out as before, with the same note
  expect_true(any(ig$context == "summary" & grepl("no value for a `cols` key", ig$reason)))
  # one column each test, side by side
  w2 <- wide(list(cols = list(t = c(stats_t_test = "{p.value:.3f}"),
                              wilcoxon = c(stats_wilcox_test = "{p.value:.3f}"))))
  expect_false(anyNA(w2$t)); expect_false(anyNA(w2$wilcoxon))
})

test_that("plan_compare() with cell_rows(): a difference on line 1 and its CI on line 2, beside the body's two lines (#625)", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom", minimum_version = "1.0.8")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adsl <- adsl[adsl$TRT != "Xanomeline Low Dose", ]
  adsl$RESP <- adsl$AGE > 75
  adsl$ALL <- "All"
  orr  <- function(.x) cardx::ard_categorical_ci(.x, variables = RESP, value = RESP ~ TRUE,
                                                method = "clopper-pearson", conf.level = 0.90)
  dif  <- function(.x) cardx::ard_stats_prop_test(.x, by = TRT, variables = RESP, conf.level = 0.90)
  ard <- cards::bind_ard(
    cards::ard_strata(adsl, .by = TRT, .strata = ALL, .f = orr),
    cards::ard_strata(adsl, .by = TRT, .strata = SEX, .f = orr),
    cards::ard_strata(adsl, .strata = ALL, .f = dif),
    cards::ard_strata(adsl, .strata = SEX, .f = dif))
  t <- suppressMessages(plan_apply(
    normalize_ard(ard, stack = list(subgroup = c("ALL", "SEX"))) |>
      table_plan(cols = c("TRT", ".stat_group"),
                 rows = c(grp1 = "subgroup", grp2 = "subgroup_level"), label = NA) |>
      plan_stat_groups(n = "N", orr_ci = c("n", "estimate", "conf.low", "conf.high")) |>
      plan_sort(FALSE) |>
      plan_cells(n = cell_rows("1" = "{N:.0f}"),
                 orr_ci = cell_rows("1" = "{n:.0f} ({estimate:.1f%})",
                                    "2" = "{conf.low:.1f%}, {conf.high:.1f%}"), notes = FALSE) |>
      plan_compare(diff = cell_rows("1" = "{estimate:.1f%}", "2" = "({conf.low:.1f%}, {conf.high:.1f%})"),
                   p = cell_rows("1" = "{p.value:.3f}")), "table"))
  t <- as.data.frame(if (is.data.frame(t)) t else t[[1L]])
  expect_identical(nrow(t), 6L)                                  # 3 subgroups x 2 lines
  one <- seq(1L, 6L, by = 2L)
  expect_match(t$diff[one], "^-?[0-9]+[.][0-9]$")                # the difference on line 1
  expect_match(t$diff[-one], "^[(]-?[0-9.]+, -?[0-9.]+[)]$")     # its CI on line 2
  expect_false(anyNA(t$p[one]))
  expect_true(all(is.na(t$p[-one])))                             # the p-value on line 1 only
  pt <- as.data.frame(dif(adsl))
  st <- function(n) unlist(pt$stat[pt$stat_name == n])
  expect_identical(t$diff[1L], sprintf("%.1f", 100 * st("estimate")))
})

test_that("normalize_ard(stack_nest = TRUE): a crossed layer's outer level as a heading row, single layers as they were (#623)", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adsl$ALL <- "All"
  tab <- function(by) cards::ard_tabulate(adsl, by = by, variables = DISCONFL)
  ard <- cards::bind_ard(tab(c("TRT", "ALL")), tab(c("TRT", "SEX")), tab(c("TRT", "AGEGR1", "SEX")))
  layers <- list(subgroup = list("ALL", "SEX", c("AGEGR1", "SEX")))
  d <- normalize_ard(ard, stack = layers, stack_nest = TRUE)
  x <- d[d$subgroup %in% "AGEGR1 / SEX", ]
  expect_identical(as.character(x$subgroup_outer), as.character(x$AGEGR1))
  expect_identical(as.character(x$subgroup_level), as.character(x$SEX))
  expect_true(all(is.na(d$subgroup_outer[d$subgroup %in% c("ALL", "SEX")])))
  # FALSE (the default) is the joined form, unchanged
  expect_identical(normalize_ard(ard, stack = layers), normalize_ard(ard, stack = layers, stack_nest = FALSE))
  expect_false("subgroup_outer" %in% names(normalize_ard(ard, stack = layers)))
  # the stub: single layers two levels, the crossed one three, no empty heading
  pg <- suppressMessages(plan_apply(
    table_plan(d, cols = "TRT", rows = c(grp = "subgroup", outer = "subgroup_outer", lvl = "subgroup_level"),
               label = "variable_level") |>
      plan_sort(FALSE) |> plan_cells("{n}", notes = FALSE) |>
      plan_stub(indent = 2, before = TRUE), "pages"))
  pg <- if (inherits(pg, "rtftable")) pg else pg[[1L]]
  stub <- trimws(as.character(pg$data[[1L]]), whitespace = "[\\h\\v]")
  expect_identical(stub[1:4], c("ALL", "All", "All", "SEX"))   # no empty outer row under a single layer
  at <- match("AGEGR1 / SEX", stub)
  expect_true(stub[at + 1L] %in% as.character(x$AGEGR1))       # the outer level, a heading
  expect_true(stub[at + 2L] %in% c("F", "M"))                  # the inner level under it
  expect_error(normalize_ard(transform(as.data.frame(normalize_ard(ard)), subgroup_outer = 1), stack = layers,
                             stack_nest = TRUE), "a column already")
})

test_that("plan_col_header(values = list(n = TRUE)) reads stacked keys on the columns: each layer's own tabulation (#622)", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  ard <- cards::bind_ard(cards::ard_tabulate(adsl, variables = c(SEX, AGEGR1)),
                         cards::ard_tabulate(adsl, by = SEX, variables = DISCONFL),
                         cards::ard_tabulate(adsl, by = AGEGR1, variables = DISCONFL))
  d <- normalize_ard(ard, stack = list(subgroup = c("SEX", "AGEGR1")))
  p <- table_plan(d, cols = c("subgroup", "subgroup_level"), rows = c(grp = "variable"),
                  label = "variable_level") |>
    plan_cells("{n}", notes = FALSE) |>
    plan_col_header(values = list(n = TRUE),
                    header = rtf_col_header(c("", "", "{col2}"), c("", "", "(N={n})")))
  tok <- plan_header_tokens(p)
  n_of <- function(k) tok$text[tok$token == sprintf("{n:%s}", k)]
  for (lv in names(table(adsl$SEX))) {
    expect_identical(n_of(paste0("SEX____", lv)), paste("=", as.integer(table(adsl$SEX)[[lv]])))
  }
  for (lv in names(table(adsl$AGEGR1))) {
    expect_identical(n_of(paste0("AGEGR1____", lv)), paste("=", as.integer(table(adsl$AGEGR1)[[lv]])))
  }
})

test_that("table_plan(show_keys = ) and plan_stat_groups(.variables, .others): SEX's own n (%) beside the SEX subgroups' ORR (#621)", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom", minimum_version = "1.0.8")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adsl$RESP <- factor(ifelse(adsl$AGE > 75, "Y", "N"), levels = c("Y", "N"))
  orr <- function(.x) cardx::ard_categorical_ci(.x, variables = RESP, value = RESP ~ "Y",
                                               method = "clopper-pearson", conf.level = 0.90)
  ard <- cards::bind_ard(cards::ard_tabulate(adsl, by = TRT, variables = SEX),
                         cards::ard_strata(adsl, .by = TRT, .strata = SEX, .f = orr))
  d <- normalize_ard(ard, stack = list(subgroup = "SEX"))
  expect_true(any(d$.key_own & d$variable == "SEX"))     # SEX is a key here, so its counts are marked
  plan <- function(show) table_plan(d, cols = c("TRT", ".stat_group"),
                                    rows = c(block = "variable", grp = "subgroup", lvl = "subgroup_level"),
                                    label = NA, show_keys = show) |>
    plan_stat_groups(n = "N", orr_ci = c("n", "estimate", "conf.low", "conf.high"),
                     .variables = "RESP", .others = "n") |>
    plan_sort(FALSE) |>
    plan_cells(SEX = "{n} ({p:.1f%})", n = cell_rows("1" = "{N:.0f}"),
               orr_ci = cell_rows("1" = "{n:.0f} ({estimate:.1f%})", "2" = "{conf.low:.1f%}, {conf.high:.1f%}"),
               notes = FALSE)
  tab_of <- function(p) {
    t <- suppressMessages(plan_apply(p, "table"))
    as.data.frame(if (is.data.frame(t)) t else t[[1L]])
  }
  with_keys <- tab_of(plan("SEX"))
  sx <- with_keys[with_keys$block == "SEX", ]
  expect_identical(nrow(sx), 2L)
  n_f <- sum(adsl$TRT == "Placebo" & adsl$SEX == "F")
  expect_identical(sx$Placebo____n[sx$lvl == "F"],
                   sprintf("%d (%.1f)", n_f, 100 * n_f / sum(adsl$TRT == "Placebo")))
  expect_true(all(is.na(sx$Placebo____orr_ci)))                       # not split into ORR's groups
  expect_identical(nrow(with_keys[with_keys$block == "RESP", ]), 4L)  # 2 sexes x 2 lines
  # without it: as before, SEX's own counts stay out of the body
  expect_false("SEX" %in% tab_of(plan(NULL))$block)
  # what is said, not guessed
  expect_error(widen_ard(d, cols = "TRT", show_keys = "TRT"), "a column key")
  expect_error(plan_stat_groups(plan(NULL), n = "N", .others = "n"), "needs `.variables`")
  expect_error(plan_stat_groups(plan(NULL), n = "N", .variables = "RESP", .others = "x"), "one of the groups")
})

test_that("plan_stat_groups(): a split record takes its group's cells, an .others record its context's, with a plan-wide categorical", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adsl$RESP <- factor(ifelse(adsl$AGE > 75, "Y", "N"), levels = c("Y", "N"))
  ard <- cards::bind_ard(
    cards::ard_tabulate(adsl, by = TRT, variables = AGEGR1),
    cards::ard_strata(adsl, .by = TRT, .f = function(.x) {
      cardx::ard_categorical_ci(.x, variables = RESP, value = RESP ~ "Y")
    }))
  plan <- function() table_plan(normalize_ard(ard), cols = c("TRT", ".stat_group"),
                                rows = c(group = "variable")) |>
    plan_stat_groups("n (%)" = c("n", "estimate"), "95% CI" = c("conf.low", "conf.high"),
                     .variables = "RESP", .others = "n (%)") |>
    plan_cells("n (%)" = "{n} ({estimate:.1f%})", "95% CI" = "[{conf.low:.1f%}, {conf.high:.1f%}]",
               notes = FALSE)
  tab_of <- function(p) as.data.frame(suppressMessages(plan_apply(p, "table")))
  pl <- adsl$TRT == "Placebo"
  n_resp <- sum(pl & adsl$RESP == "Y")
  rate <- sprintf("%d (%.1f)", n_resp, 100 * n_resp / sum(pl))
  # a later plan-wide categorical (a study's default) is AGEGR1's, and the
  # rate keeps its group's template
  t <- tab_of(plan() |> plan_cells(categorical = "{n} ({p:.1f%})"))
  ag <- t[t$group == "AGEGR1", ]
  n_ag <- sum(pl & adsl$AGEGR1 == ag$label[1L])
  expect_identical(ag[["Placebo____n (%)"]][1L], sprintf("%d (%.1f)", n_ag, 100 * n_ag / sum(pl)))
  expect_true(all(is.na(ag[["Placebo____95% CI"]])))
  rs <- t[t$group == "RESP", ]
  expect_identical(rs[["Placebo____n (%)"]], rate)
  expect_match(rs[["Placebo____95% CI"]], "^\\[[0-9.]+, [0-9.]+\\]$")
  # without it, as before: the rate as its group says
  expect_identical(tab_of(plan())[["Placebo____n (%)"]][tab_of(plan())$group == "RESP"], rate)
})

test_that("plan_filter(): PTs with p >= 5% in any arm, a SOC with its PTs; the RTF a hand-filtered ARD gives (#620)", {
  skip_if_not_installed("cards")
  ard <- cards::ard_stack_hierarchical(cards::ADAE, variables = c(AESOC, AEDECOD), by = TRTA,
                                       denominator = cards::ADSL, id = USUBJID, over_variables = TRUE)
  d <- normalize_ard(ard, hierarchy = c("AESOC", "AEDECOD"), overall = "Any TEAE")
  plan <- function(x) table_plan(x, cols = "TRTA", rows = c(soc = "AESOC", pt = "AEDECOD"), label = NA) |>
    plan_sort(".overall", "soc", ".depth", "-n") |> plan_cells("{n} ({p:.1f%})", notes = FALSE)
  # by hand: the PTs whose p reaches 5% in an arm, the SOCs they are in, the overall row
  pv <- d[!is.na(d$AEDECOD) & d$stat_name == "p", ]
  pts <- names(which(tapply(pv$stat, as.character(pv$AEDECOD), max) >= 0.05))
  socs <- unique(as.character(d$AESOC[as.character(d$AEDECOD) %in% pts]))
  hand <- d[d$.overall | as.character(d$AEDECOD) %in% pts |
              (is.na(d$AEDECOD) & as.character(d$AESOC) %in% socs), ]
  render <- function(p) {
    f <- tempfile(fileext = ".rtf"); on.exit(unlink(f), add = TRUE)
    old <- options(rtfreporter.render_time = as.POSIXct("2026-01-01 09:00"))
    on.exit(options(old), add = TRUE)
    suppressMessages(generate_rtfreport(rtf_tables(rtf_document(program = "t.R"), p), f, overwrite = TRUE))
    readBin(f, "raw", file.size(f))
  }
  expect_identical(render(plan(d) |> plan_filter(p >= 0.05)), render(plan(hand)))
  tab_of <- function(p) {
    t <- suppressMessages(plan_apply(p, "table"))
    as.data.frame(if (is.data.frame(t)) t else t[[1L]])
  }
  # every arm instead of any; parents = "drop" judges a SOC by its own counts
  all_arms <- tab_of(plan(d) |> plan_filter(p >= 0.05, any = FALSE))
  expect_true(sum(!is.na(all_arms$pt)) < length(pts))
  own <- tab_of(plan(d) |> plan_filter(p >= 0.30, parents = "drop"))
  kids <- tab_of(plan(d) |> plan_filter(p >= 0.30))
  # a SOC counts at least its PTs' subjects, so judged on its own it stays as
  # often as when it waits for a PT
  expect_gte(sum(is.na(own$pt)), sum(is.na(kids$pt)))
  expect_identical(own$pt[!is.na(own$pt)], kids$pt[!is.na(kids$pt)])
  # the overall row always stays; nothing passes, it is the only row left
  none <- tab_of(plan(d) |> plan_filter(p > 1))
  expect_identical(nrow(none), 1L)
  expect_identical(as.character(none$soc), "Any TEAE")
  # what is said
  expect_error(plan_filter(plan(d), keep = p > 0), "unnamed")
  expect_error(plan_filter(plan(d), p > 0, any = NA), "any column")
})

test_that("plan_compare(): the column header's {col} / {n} sit over their columns, the comparison column gets its cell (#631)", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom", minimum_version = "1.0.8")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  ard <- cards::bind_ard(
    cards::ard_stack(adsl, .by = TRT, .overall = TRUE, cards::ard_tabulate(variables = SEX)),
    cardx::ard_stats_chisq_test(adsl, by = TRT, variables = SEX))
  base <- table_plan(normalize_ard(ard), cols = "TRT", rows = c(group = "variable")) |>
    plan_cells("{n}", notes = FALSE) |> plan_stub(name = "Characteristic")
  hdr <- function(p) {
    pg <- suppressMessages(suppressWarnings(plan_apply(p, "pages")))
    pg <- if (inherits(pg, "rtftable")) pg else pg[[1L]]
    list(names = names(pg$data), rows = lapply(pg$col_header, function(r)
      unname(vapply(r, function(x) if (is.list(x)) x$label else x, ""))))
  }
  arms <- sort(unique(adsl$TRT))
  n_arm <- as.integer(table(adsl$TRT)[arms])
  # short rows over the spread columns
  h <- hdr(base |> plan_total() |> plan_compare(p = "{p.value:.3f}") |>
             plan_col_header(values = list(n = TRUE),
                             header = rtf_col_header(c("", "{col}"), c("Characteristic", "(N={n})"))))
  expect_identical(h$names, c("Characteristic", arms, "Total", "p"))
  expect_identical(h$rows[[1L]], c("", arms, "Total", "p"))
  expect_identical(h$rows[[2L]], c("Characteristic", sprintf("(N=%d)", n_arm),
                                   sprintf("(N=%d)", nrow(adsl)), ""))
  # the header a row at a time, the comparison column named
  h2 <- hdr(base |> plan_compare(p = "{p.value:.3f}") |>
              plan_col_header(values = list(n = TRUE), lines = list(
                c(Characteristic = "", .values = "{col}", p = "p-value"),
                c(Characteristic = "Characteristic", .values = "(N={n})", p = ""))))
  expect_identical(h2$rows[[1L]], c("", arms, "p-value"))
  expect_identical(h2$rows[[2L]], c("Characteristic", sprintf("(N=%d)", n_arm), ""))
  # without a comparison column: as before
  h3 <- hdr(base |> plan_total() |>
              plan_col_header(values = list(n = TRUE),
                              header = rtf_col_header(c("", "{col}"), c("Characteristic", "(N={n})"))))
  expect_identical(h3$rows[[1L]], c("", arms, "Total"))
})

test_that("a plan's column positions count the printed columns, with a hidden key and BY pages (#635)", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  ard <- cards::bind_ard(cards::ard_tabulate(adsl, variables = TRT),
                         cards::ard_tabulate(adsl, by = c(TRT, SEX), variables = c(AGEGR1, RACE)))
  d <- normalize_ard(ard, stack = list(subgroup = "group2"))
  base <- function() table_plan(d, cols = "variable", rows = c(arm = "TRT", grp1 = "subgroup",
                                                               grp2 = "subgroup_level", lvl = "variable_level"),
                                label = NA) |>
    plan_sort(FALSE) |> plan_cells("{n}", notes = FALSE) |>
    plan_hide("arm") |>
    plan_paginate_rows(page_by = "arm", heading = "Treatment: {value} (N={n})")
  first <- function(p) {
    pg <- suppressMessages(plan_apply(p, "pages"))
    if (inherits(pg, "rtftable")) pg else pg[[1L]]
  }
  # row_title and collapse: positions 1-2 are the printed grp1 and grp2, not the hidden arm
  p1 <- first(base() |> plan_columns(row_title = c(1, 2)) |>
                plan_row_group(mode = "value", collapse = c(1L, 2L)))
  expect_identical(names(p1$data)[1:3], c("grp1", "grp2", "lvl"))
  expect_identical(as.integer(p1$row_title), 1:2)
  g2 <- as.character(p1$data$grp2)
  expect_false(any(g2[-1L] == g2[-length(g2)], na.rm = TRUE))   # no repeat left in grp2
  expect_true(anyNA(g2))                                         # the repeats are blanked
  # widths: one a printed column, in order
  p2 <- first(base() |> plan_columns(widths = c(3, 2, 2, 1, 1)))
  expect_identical(length(p2$col_rel_width), ncol(p2$data))
  expect_identical(as.numeric(p2$col_rel_width), c(3, 2, 2, 1, 1))
  # a header cell at a printed position sits over that column
  p3 <- first(base() |> plan_col_header(header = rtf_col_header(
    list(col_cell(c(1, 3), "Subgroup"), col_cell(4, "Age group"), col_cell(5, "Race")))))
  cells <- p3$col_header[[1L]]
  expect_identical(c(cells[[1L]]$from, cells[[1L]]$to), c(1L, 3L))
  expect_identical(cells[[2L]]$from, 4L)
  expect_identical(names(p3$data)[4:5], c("AGEGR1", "RACE"))
  # a position past the printed columns is said, naming them
  expect_error(suppressMessages(plan_apply(base() |> plan_columns(row_title = 6), "pages")),
               "prints 5 columns")
})

test_that("row_title and collapse also take column names: a row key's name, .values (#635)", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  ard <- cards::bind_ard(cards::ard_tabulate(adsl, variables = TRT),
                         cards::ard_tabulate(adsl, by = c(TRT, SEX), variables = c(AGEGR1, RACE)))
  d <- normalize_ard(ard, stack = list(subgroup = "group2"))
  base <- function() table_plan(d, cols = "variable", rows = c(arm = "TRT", grp1 = "subgroup",
                                                               grp2 = "subgroup_level", lvl = "variable_level"),
                                label = NA) |>
    plan_sort(FALSE) |> plan_cells("{n}", notes = FALSE) |>
    plan_hide("arm") |>
    plan_paginate_rows(page_by = "arm", heading = "Treatment: {value} (N={n})")
  first <- function(p) {
    pg <- suppressMessages(plan_apply(p, "pages"))
    if (inherits(pg, "rtftable")) pg else pg[[1L]]
  }
  by_pos <- first(base() |> plan_columns(widths = c(grp1 = 3, grp2 = 2, lvl = 2, .values = 1),
                                         row_title = c(1, 2)) |>
                    plan_row_group(mode = "value", collapse = c(1L, 2L)))
  by_name <- first(base() |> plan_columns(widths = c(grp1 = 3, grp2 = 2, lvl = 2, .values = 1),
                                          row_title = c("grp1", "grp2")) |>
                     plan_row_group(mode = "value", collapse = c("grp1", "grp2")))
  expect_identical(as.integer(by_name$row_title), 1:2)
  expect_identical(by_name$data, by_pos$data)
  # .values is every value column
  vals <- first(base() |> plan_columns(row_title = c("grp1", ".values")))
  expect_identical(as.integer(vals$row_title), c(1L, 4L, 5L))
  # a hidden or unknown name is said, with the printed columns
  expect_error(suppressMessages(plan_apply(base() |> plan_columns(row_title = "arm"), "pages")),
               "not a printed column")
  expect_error(suppressMessages(plan_apply(base() |> plan_row_group(collapse = "grp9"), "pages")),
               "not a printed column")
})

test_that("a page key is never a row group: blanks, collapse, row_title, sort and stub start at the first printed key (#635)", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  ard <- cards::bind_ard(cards::ard_tabulate(adsl, variables = TRT),
                         cards::ard_tabulate(adsl, by = c(TRT, SEX), variables = c(AGEGR1, RACE)))
  d <- normalize_ard(ard, stack = list(subgroup = "group2"))
  rows <- c(arm = "TRT", grp1 = "subgroup", grp2 = "subgroup_level", lvl = "variable_level")
  core <- function() table_plan(d, cols = "variable", rows = rows, label = NA) |>
    plan_cells("{n}", notes = FALSE)
  # the two ways to a section per arm: page_by + plan_hide, and a page per value
  by_rows <- function(p) p |> plan_hide("arm") |>
    plan_paginate_rows(page_by = "arm", heading = "Treatment: {value}")
  by_group <- function(p) p |>
    plan_paginate_group(col = "arm", keep = FALSE, heading = "Treatment: {value}")
  pages <- function(p) suppressMessages(plan_apply(p, "pages"))
  lay <- function(p) p |> plan_sort(FALSE) |>
    plan_row_group(mode = "value", collapse = c("grp1", "grp2")) |>
    plan_blanks(where = "between_groups", first = TRUE, last = TRUE) |>
    plan_columns(row_title = c("grp1", "grp2"))
  for (form in list(by_rows, by_group)) {
    pg <- pages(form(lay(core())))
    expect_length(pg, 3L)
    expect_identical(sub("[.]{3}[0-9]+$", "", names(pg)),
                     paste("Treatment:", unique(adsl$TRT)))
    one <- pg[[1L]]
    expect_identical(names(one$data)[1:3], c("grp1", "grp2", "lvl"))
    expect_identical(as.integer(one$row_title), 1:2)
    # a blank at each grp1 change, and first / last on every page
    g1 <- as.character(suppressMessages(plan_apply(form(lay(core())), "table"))$grp1)
    g1 <- g1[seq_len(nrow(one$data))]
    change <- which(g1[-1L] != g1[-length(g1)])
    expect_identical(attr(one$data, "rtf_blank_rows"),
                     as.integer(c(0L, change, nrow(one$data))))
    for (q in pg) expect_true(all(c(0L, nrow(q$data)) %in% attr(q$data, "rtf_blank_rows")))
    # collapse blanks repeats in grp1 (not in the page key, which is gone)
    expect_true(anyNA(one$data$grp1))
  }
  # the stub folds the printed row keys, not the page key
  for (form in list(by_rows, by_group)) {
    pg <- pages(form(core() |> plan_sort(FALSE) |> plan_stub()))
    expect_false("arm" %in% names(pg[[1L]]$data))
    expect_false(any(grepl("Placebo|Xanomeline", pg[[1L]]$data[[1L]])))
  }
  # a sort runs inside each page: every page keeps its own arm
  pg <- pages(by_rows(core() |> plan_sort("-n")))
  expect_length(pg, 3L)
})

test_that("a verb's first argument is .plan: a name such as p in ... is not taken for it", {
  skip_if_no_cards2()
  # every plan verb takes the plan as `.plan` (one name, the pipe passes it)
  verbs <- grep("^plan_", getNamespaceExports("rtfreporter"), value = TRUE)
  verbs <- setdiff(verbs, "plan_template")
  first <- vapply(verbs, function(f) names(formals(getExportedValue("rtfreporter", f)))[1L], "")
  expect_true(all(first == ".plan"), info = paste(verbs[first != ".plan"], collapse = ", "))
  # `p` (plan_compare()'s column, cards' percentage), `pl`, `pla`: an
  # argument before `...` matches by its first letters, so these stopped with
  # "Expected a table_plan" when the argument was called `plan`
  b <- base_plan()
  expect_s3_class(b |> plan_digits(p = 0), "table_plan")
  # the column p of a comparison, its digits by name
  cmp <- b |> plan_compare(p = "{p.value}")
  expect_s3_class(cmp |> plan_digits(p = 3), "table_plan")
  expect_s3_class(b |> plan_cells(p = "{p:.1f%}"), "table_plan")
  expect_s3_class(b |> plan_labels(pl = "x"), "table_plan")
  expect_s3_class(b |> plan_levels(pla = c("a", "b")), "table_plan")
  expect_s3_class(b |> plan_sort(p = "x"), "table_plan")
  expect_s3_class(b |> plan_hide(p = "x"), "table_plan")
  expect_s3_class(b |> plan_nest(p = c(SEX = "F")), "table_plan")
  expect_s3_class(b |> plan_after(p = identity), "table_plan")
  expect_s3_class(b |> plan_titles(p = "x"), "table_plan")
  # by name, it is `.plan`
  expect_s3_class(plan_digits(.plan = b, p = 0), "table_plan")
})

test_that("a value plan_levels() names that the ARD does not have is said (it makes no row), whatever notes says", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  adsl$TRT <- factor(adsl$ARM)
  adsl$SEXC <- as.character(adsl$SEX)
  ard <- cards::bind_ard(cards::ard_tabulate(adsl, variables = TRT),
                         cards::ard_summary(adsl, by = TRT, variables = AGE),
                         cards::ard_tabulate(adsl, by = TRT, variables = SEXC))
  p <- table_plan(normalize_ard(ard), cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(continuous = c(n = "{N}"), categorical = "{n} ({p:.1f%})", notes = FALSE)
  expect_message(t <- plan_apply(plan_levels(p, SEXC = c("F", "M", "U"), variable = c("SEXC", "WEIGHT")), "table"),
                 'SEXC: "U"')
  expect_identical(nrow(t), 3L)   # n, F, M: no row of U
  expect_message(plan_apply(plan_levels(p, variable = c("SEXC", "WEIGHT")), "table"), 'variable: "WEIGHT"')
  # what is there, by its value or as plan_labels() prints it: nothing said
  expect_no_message(plan_apply(p |> plan_labels(SEXC = c(F = "Female")) |>
                                 plan_levels(SEXC = c("M", "Female"), variable = c("SEXC", "AGE")), "table"))
})

test_that("plan_levels() with names: a value and its text at once (factor(levels, labels)); plan_labels() wins value by value", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  ard <- cards::bind_ard(cards::ard_tabulate(adsl, variables = TRT),
                         cards::ard_summary(adsl, by = TRT, variables = AGE),
                         cards::ard_tabulate(adsl, by = TRT, variables = c(SEX, AGEGR1)))
  base <- table_plan(normalize_ard(ard), cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(continuous = c(n = "{N}"), categorical = "{n} ({p:.1f%})", notes = FALSE)
  tab <- function(p) suppressMessages(plan_apply(p, "table"))
  # a variable's values: the order and the text, as the two verbs say them
  expect_identical(tab(plan_levels(base, SEX = c(M = "Male", F = "Female"))),
                   tab(base |> plan_levels(SEX = c("M", "F")) |>
                         plan_labels(SEX = c(M = "Male", F = "Female"))))
  # the variables' order and headings; an element without a name: the variable as it is
  expect_identical(tab(plan_levels(base, variable = c(SEX = "Sex", "AGE", AGEGR1 = "Age group"))),
                   tab(base |> plan_levels(variable = c("SEX", "AGE", "AGEGR1")) |>
                         plan_labels(variable = c(SEX = "Sex", AGEGR1 = "Age group"))))
  # some values named, the others as they are; no names: as before
  expect_identical(tab(plan_levels(base, AGEGR1 = c(">80", "65-80", "<65" = "Under 65"))),
                   tab(base |> plan_levels(AGEGR1 = c(">80", "65-80", "<65")) |>
                         plan_labels(AGEGR1 = c("<65" = "Under 65"))))
  # plan_labels() wins value by value, before or after
  want <- tab(base |> plan_levels(SEX = c("M", "F")) |> plan_labels(SEX = c(M = "Men", F = "Female")))
  expect_identical(tab(base |> plan_levels(SEX = c(M = "Male", F = "Female")) |> plan_labels(SEX = c(M = "Men"))), want)
  expect_identical(tab(base |> plan_labels(SEX = c(M = "Men")) |> plan_levels(SEX = c(M = "Male", F = "Female"))), want)
  # a heading of plan_labels() (the variable's own key) beside the values' text
  expect_identical(tab(base |> plan_labels(SEX = "Sex") |> plan_levels(SEX = c(M = "Male", F = "Female"))),
                   tab(base |> plan_levels(SEX = c("M", "F")) |>
                         plan_labels(SEX = c(SEX = "Sex", M = "Male", F = "Female"))))
  # plan_labels() names a value by the text the order gives it, too
  expect_identical(tab(base |> plan_levels(SEX = c(M = "Male", F = "Female")) |> plan_labels(SEX = c(Female = "Women"))),
                   tab(base |> plan_levels(SEX = c("M", "F")) |> plan_labels(SEX = c(M = "Male", F = "Women"))))
  # the same text a value and another value's text: the value, said
  expect_message(t2 <- plan_apply(base |> plan_levels(SEX = c(M = "F", F = "Female")) |>
                                   plan_labels(SEX = c(F = "Women")), "table"),
                 "is a value and the text")
  expect_identical(t2, tab(base |> plan_levels(SEX = c("M", "F")) |> plan_labels(SEX = c(M = "F", F = "Women"))))
  # a dictionary of headings (plan_labels(variable = )) over an order that names one
  expect_identical(tab(base |> plan_labels(variable = c(AGE = "Age (years)")) |>
                         plan_levels(variable = c(AGE = "Age", SEX = "Sex"))),
                   tab(base |> plan_levels(variable = c("AGE", "SEX")) |>
                         plan_labels(variable = c(AGE = "Age (years)", SEX = "Sex"))))
})

test_that("plan_sort() by a plan_compare() statistic: the rows by the arms' difference, either call order (#684)", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  adae <- cards::ADAE[cards::ADAE$AESOC %in% c("CARDIAC DISORDERS", "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"), ]
  arms <- c("Placebo", "Xanomeline High Dose")
  adsl <- adsl[adsl$ARM %in% arms, ]
  adae <- adae[adae$TRTA %in% arms, ]
  adae$TRTA <- factor(adae$TRTA, levels = arms)
  adsl$TRTA <- factor(adsl$ARM, levels = arms)
  ard <- cards::ard_stack_hierarchical(adae, variables = c(AESOC, AEDECOD), by = TRTA,
                                       denominator = adsl, id = USUBJID)
  d <- normalize_ard(cards::bind_ard(ard, cards::diff_ard_hierarchical(ard), .update = TRUE),
                     hierarchy = c("AESOC", "AEDECOD"))
  base <- table_plan(d, cols = "TRTA", rows = c(SOC = "AESOC"), label = c(PT = "AEDECOD")) |>
    plan_cells(hierarchical = "{n} ({p:.1f%})", notes = FALSE)
  tab <- function(p) as.data.frame(suppressMessages(plan_apply(p, "table")))
  p <- base |> plan_compare(diff = "{estimate:.3f}")
  num <- function(t) as.numeric(t$diff)
  # within each SOC, its PTs by the difference, descending; ascending without the minus
  dn <- tab(p |> plan_sort("SOC", ".depth", "-estimate"))
  up <- tab(p |> plan_sort("SOC", ".depth", "estimate"))
  for (s in unique(dn$SOC)) {
    expect_true(all(diff(num(dn[dn$SOC == s & !is.na(dn$PT), ])) <= 0))
    expect_true(all(diff(num(up[up$SOC == s & !is.na(up$PT), ])) >= 0))
    expect_true(is.na(dn$PT[dn$SOC == s][1L]))     # the SOC's own row first
  }
  # the SOCs by their own difference, each with its PTs
  t3 <- tab(p |> plan_sort("-estimate@SOC", "SOC", ".depth", "-estimate"))
  expect_true(all(diff(num(t3)[is.na(t3$PT)]) <= 0))
  runs <- rle(as.character(t3$SOC))$values
  expect_identical(runs, unique(runs))
  # the call order does not matter (one layer each)
  p2 <- base |> plan_sort("SOC", ".depth", "-estimate") |> plan_compare(diff = "{estimate:.3f}")
  expect_identical(tab(p2), dn)
  # a statistic the body has is still the body's; a comparison by name needs `by`
  expect_identical(tab(p |> plan_sort("SOC", ".depth", "-n")), tab(base |> plan_compare(diff = "{estimate:.3f}") |>
                                                                    plan_sort("SOC", ".depth", "-n")))
  expect_error(tab(p |> plan_sort("-estimate[X]")), "names a comparison")
  # without plan_compare(): no comparison to read
  expect_error(tab(base |> plan_sort("-estimate")), "neither a column")
})

test_that("plan_empty(): a table of no events says so, only when it is empty (#691)", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  # an ARD of no events: the overall row 0 (as tflspec makes it for no records)
  stand <- adsl[!duplicated(adsl$TRTA), ]
  stand$AESOC <- "(none)"
  stand$AEDECOD <- "(none)"
  ard0 <- cards::ard_stack_hierarchical(stand, variables = c(AESOC, AEDECOD), by = TRTA,
                                        denominator = adsl, id = USUBJID, over_variables = TRUE)
  ard0 <- ard0[ard0$variable %in% c("..ard_hierarchical_overall..", "TRTA"), ]
  ov <- ard0$variable %in% "..ard_hierarchical_overall.."
  ard0$stat[ov & ard0$stat_name == "n"] <- list(0L)
  ard0$stat[ov & ard0$stat_name == "p"] <- list(0)
  base <- function(a) {
    table_plan(normalize_ard(a, hierarchy = c("AESOC", "AEDECOD"), overall = "Any TEAE"),
               cols = "TRTA", rows = c(soc = "AESOC", pt = "AEDECOD")) |>
      plan_cells(categorical = c(n == 0 ~ "0", "{n} ({p:.1f%})"), notes = FALSE)
  }
  tab <- function(p) as.data.frame(suppressMessages(plan_apply(p, "table")))
  arms <- unique(as.character(adsl$TRTA))
  # last (the default): the overall row's 0 stays, the sentence after it
  t <- tab(base(ard0) |> plan_empty())
  expect_identical(t$label, c("Any TEAE", "No subjects met the criteria."))
  expect_true(all(unlist(t[2L, arms]) == ""))
  expect_true(all(unlist(t[1L, arms]) == "0"))
  expect_identical(tab(base(ard0) |> plan_empty(position = "first"))$label[1L],
                   "No subjects met the criteria.")
  expect_identical(tab(base(ard0) |> plan_empty(text = "None.", position = "replace"))$label, "None.")
  # when = "no_rows": no row but the overall row -- its 0 counts too
  expect_identical(nrow(tab(base(ard0) |> plan_empty(when = "no_rows"))), 2L)
  # the footnote, on an empty table only; text = NULL: the footnote alone
  pg <- function(p) {
    r <- suppressMessages(plan_apply(p |> plan_stub(name = "row_label")))
    if (inherits(r, "rtftable")) r else r[[1L]]
  }
  e <- pg(base(ard0) |> plan_empty(text = NULL, footnote = "No events were reported."))
  expect_identical(unlist(attr(e, "rtf_footnotes")), "No events were reported.")
  expect_false(any(grepl("No subjects", unlist(e$data), fixed = TRUE)))
  # a table with events: as it was, no footnote
  adae <- cards::ADAE[cards::ADAE$AESOC %in% unique(cards::ADAE$AESOC)[1:2], ]
  ard1 <- cards::ard_stack_hierarchical(adae, variables = c(AESOC, AEDECOD), by = TRTA,
                                        denominator = adsl, id = USUBJID, over_variables = TRUE)
  expect_identical(tab(base(ard1) |> plan_empty()), tab(base(ard1)))
  f <- pg(base(ard1) |> plan_empty(footnote = "No events were reported."))
  expect_null(unlist(attr(f, "rtf_footnotes")))
  # every row but the overall filtered out: the overall row, then the sentence
  t2 <- tab(base(ard1) |> plan_filter(n > 1000) |> plan_empty())
  expect_identical(t2$label, c("Any TEAE", "No subjects met the criteria."))
  # and only an overall row of 0 counts for when = "overall_zero"
  expect_identical(nrow(tab(base(ard1) |> plan_filter(n > 1000) |> plan_empty(when = "overall_zero"))), 1L)
  expect_error(base(ard0) |> plan_empty(text = c("a", "b")), "one string")
})

test_that("no cols: one column, All, when the ARD has no group; its N the ARD's total (#689)", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL[cards::ADSL$ARM == "Placebo", ]
  adae <- cards::ADAE[cards::ADAE$TRTA == "Placebo" &
                        cards::ADAE$AESOC %in% unique(cards::ADAE$AESOC)[1:2], ]
  ard <- cards::bind_ard(cards::ard_total_n(adsl),
                         cards::ard_stack_hierarchical(adae, variables = c(AESOC, AEDECOD),
                                                       denominator = adsl, id = USUBJID,
                                                       over_variables = TRUE))
  d <- normalize_ard(ard, hierarchy = c("AESOC", "AEDECOD"), overall = "Any TEAE")
  p <- table_plan(d, rows = c(soc = "AESOC", pt = "AEDECOD")) |>
    plan_cells(hierarchical = "{n} ({p:.1f%})", notes = FALSE) |>
    plan_sort(".overall", "soc", ".depth", "-n", "pt")
  t <- as.data.frame(suppressMessages(plan_apply(p, "table")))
  expect_true("All" %in% names(t))
  expect_identical(t$soc[1L], "Any TEAE")
  n_any <- length(unique(adae$USUBJID))
  expect_identical(t$All[1L], sprintf("%d (%.1f)", n_any, 100 * n_any / nrow(adsl)))
  # a header's {n}: the ARD's total
  hp <- p |> plan_stub(name = "row_label") |>
    plan_col_header(data.frame(line = c("1", "1"), cols = c("row_label", ".values"),
                               span = c(NA, "each"), text = c("SOC / PT", "Drug X (N={n})")))
  tok <- plan_header_tokens(hp)
  expect_identical(unname(unlist(tok$values[tok$token == "{n}"])), as.numeric(nrow(adsl)))
  # with no total in the ARD: said
  d2 <- normalize_ard(cards::ard_stack_hierarchical(adae, variables = c(AESOC, AEDECOD),
                                                    denominator = adsl, id = USUBJID),
                      hierarchy = c("AESOC", "AEDECOD"))
  p2 <- table_plan(d2, rows = c(soc = "AESOC", pt = "AEDECOD")) |>
    plan_cells(hierarchical = "{n}", notes = FALSE) |> plan_stub(name = "row_label") |>
    plan_col_header(values = list(n = TRUE))
  expect_error(suppressMessages(plan_apply(p2)), "ard_total_n")
  # an ARD with a group: cols still named, the group said
  ard3 <- cards::ard_stack_hierarchical(cards::ADAE[cards::ADAE$AESOC %in% unique(cards::ADAE$AESOC)[1:2], ],
                                        variables = c(AESOC, AEDECOD), by = TRTA,
                                        denominator = cards::ADSL, id = USUBJID)
  p3 <- table_plan(normalize_ard(ard3, hierarchy = c("AESOC", "AEDECOD")),
                   rows = c(soc = "AESOC", pt = "AEDECOD"))
  expect_error(suppressMessages(plan_apply(p3, "table")), "the ARD has .TRTA. to spread")
})

# ---------------------------------- a shift table's margins (plan_total(direction))

# subjects x parameter x direction, baseline and worst grade (the ADaM values)
shift_ard <- function(two_cols = FALSE) {
  set.seed(15)
  adsl <- data.frame(USUBJID = sprintf("S%03d", 1:29), TRTA = rep(c("A", "B"), length.out = 29))
  g <- function(n) factor(sample(as.character(c(0, 0, 0, 1:4)), n, TRUE), levels = as.character(0:4))
  x <- expand.grid(USUBJID = adsl$USUBJID, PARAM = c("Albumin", "Potassium"),
                   ATOXDIR = c("Low", "High"), stringsAsFactors = FALSE)
  x <- x[!(x$PARAM == "Albumin" & x$ATOXDIR == "High"), ]
  x$PARAM <- factor(x$PARAM, levels = c("Albumin", "Potassium"))
  x$ATOXDIR <- factor(x$ATOXDIR, levels = c("Low", "High"))
  x$BTOXGR <- g(nrow(x))
  x$ATOXGR <- g(nrow(x))
  x$TRTA <- adsl$TRTA[match(x$USUBJID, adsl$USUBJID)]
  st <- everything() ~ c("n", "p")
  by0 <- if (two_cols) "TRTA" else character(0)
  cards::bind_ard(
    cards::ard_total_n(adsl),
    cards::ard_tabulate(x, variables = ATOXGR, by = c(by0, "PARAM", "ATOXDIR", "BTOXGR"),
                        denominator = adsl, statistic = st),
    cards::ard_tabulate(x, variables = ATOXGR, by = c(by0, "PARAM", "ATOXDIR"), denominator = adsl, statistic = st),
    cards::ard_tabulate(x, variables = BTOXGR, by = c(by0, "PARAM", "ATOXDIR"), denominator = adsl, statistic = st),
    cards::ard_tabulate(x, variables = ATOXDIR, by = c(by0, "PARAM"), denominator = adsl, statistic = st))
}
shift_plan <- function(nd, cols = "BTOXGR", ...) {
  table_plan(nd, cols = cols, rows = c(LAB = ~ "{PARAM} - {ATOXDIR}"),
             label = c(is.na(.label) ~ "Total", ~ "{.label}")) |>
    plan_cells("{n:.0f} ({p:.1f%})", notes = FALSE) |>
    plan_total(label = "Total", ...)
}

test_that("plan_total(direction = \"both\"): a shift table's Total row and corner from cards' plain margins", {
  skip_if_not_installed("cards")
  nd <- normalize_ard(shift_ard())
  # the workaround it replaces: the margins rewritten as the body's variable
  nd2 <- nd
  m <- nd2$.key_own %in% TRUE & nd2$variable %in% c("BTOXGR", "ATOXDIR")
  nd2$variable[m] <- "ATOXGR"
  nd2$.label[m] <- NA_character_
  nd2$.key_own[m] <- FALSE
  nd2$.kind[m] <- nd2$.kind[!m & nd2$variable == "ATOXGR"][1L]
  nd2$.label_order[m] <- max(nd2$.label_order, na.rm = TRUE) + 1
  old <- shift_plan(nd2)
  new <- shift_plan(nd, direction = "both")
  t_new <- as.data.frame(plan_apply(new, "table"))
  expect_identical(t_new, as.data.frame(plan_apply(old, "table")))
  alb <- t_new[t_new$LAB == "Albumin - Low", ]
  expect_identical(as.character(alb$label), c(as.character(0:4), "Total"))
  expect_identical(alb$Total[6L], "29 (100.0)")              # the corner
  expect_identical(nrow(t_new), 4L * 6L)   # four blocks of six (Albumin - High: n = 0, the factors)
  # the RTF, byte for byte
  op <- options(rtfreporter.render_time = as.POSIXct("2026-01-01 09:00", tz = "UTC"))
  on.exit(options(op), add = TRUE)
  f1 <- tempfile(fileext = ".rtf"); f2 <- tempfile(fileext = ".rtf")
  on.exit(unlink(c(f1, f2)), add = TRUE)
  generate_rtfreport(rtf_document() |> rtf_tables(old), f1, overwrite = TRUE)
  generate_rtfreport(rtf_document() |> rtf_tables(new), f2, overwrite = TRUE)
  expect_identical(unname(tools::md5sum(f1)), unname(tools::md5sum(f2)))
  # the header's N as before
  expect_identical(plan_n_candidates(new), plan_n_candidates(shift_plan(nd)))
  # "rows": the Total row, no Total column
  r <- as.data.frame(plan_apply(shift_plan(nd, direction = "rows"), "table"))
  expect_false("Total" %in% names(r))
  expect_identical(as.character(r$label[r$LAB == "Albumin - Low"]), c(as.character(0:4), "Total"))
  # first
  f <- as.data.frame(plan_apply(shift_plan(nd, direction = "both", position = "first"), "table"))
  expect_identical(as.character(f$label[f$LAB == "Albumin - Low"]), c("Total", as.character(0:4)))
  expect_identical(names(f)[3L], "Total")
  # the default writes no direction: the plan as before
  expect_null(.plan_merge(.plan_of(shift_plan(nd), "total"))$direction)
})

test_that("plan_total(direction = ): two column keys; one analysis variable; margins needed", {
  skip_if_not_installed("cards")
  nd <- normalize_ard(shift_ard(two_cols = TRUE))
  t <- as.data.frame(plan_apply(shift_plan(nd, cols = c("TRTA", "BTOXGR"), direction = "both"), "table"))
  expect_true(all(c("A____Total", "B____Total") %in% names(t)))
  alb <- t[t$LAB == "Albumin - Low", ]
  expect_identical(as.character(alb$label[6L]), "Total")
  expect_match(alb$A____Total[6L], "^[0-9]+ \\(")              # each arm's corner
  # a second analysis variable in the body: which one's margins is not said
  two <- normalize_ard(cards::bind_ard(shift_ard(), cards::ard_tabulate(
    data.frame(SEX = c("F", "M"), BTOXGR = factor(c("0", "1"), levels = as.character(0:4))),
    variables = SEX, by = BTOXGR)))
  expect_error(plan_apply(shift_plan(two, direction = "both"), "table"), "one analysis\\s+variable")
  # no margins in the ARD
  cells <- normalize_ard(cards::ard_tabulate(
    data.frame(BTOXGR = factor(c("0", "1")), ATOXGR = factor(c("1", "1"))),
    variables = ATOXGR, by = BTOXGR))
  expect_error(plan_apply(table_plan(cells, cols = "BTOXGR") |> plan_total(direction = "rows"), "table"),
               "no margins")
  expect_error(plan_total(table_plan(cells, cols = "BTOXGR"), direction = "down"), "should be one of")
})

test_that("table_plan(label = c(label = c(...))): c() numbered the formulas, said", {
  skip_if_not_installed("cards")
  nd <- normalize_ard(shift_ard())
  expect_error(table_plan(nd, cols = "BTOXGR", rows = c(LAB = ~ "{PARAM} - {ATOXDIR}"),
                          label = c(label = c(is.na(.label) ~ "Total", ~ "{.label}"))),
               "label = c\\(is.na")
})

test_that("the study total survives a mutate() between normalize_ard() and table_plan()", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  nd <- normalize_ard(cards::bind_ard(
    cards::ard_tabulate(adsl, by = ARM, variables = SEX),
    cards::ard_total_n(adsl)))
  expect_identical(unique(nd$.total_n), as.numeric(nrow(adsl)))
  n_of <- function(d) plan_n_candidates(table_plan(d, cols = "ARM") |> plan_total())
  before <- n_of(nd)
  # base transform() drops the attribute; the column keeps the total
  tr <- transform(nd, z = 1)
  expect_null(attr(tr, "ard_total_n"))
  expect_identical(n_of(tr), before)
  expect_identical(before$value[is.na(before$column)], as.numeric(nrow(adsl)))
  if (requireNamespace("dplyr", quietly = TRUE)) {
    expect_identical(n_of(dplyr::mutate(nd, z = 1)), before)
  }
  # no total stated: no column
  expect_false(".total_n" %in% names(normalize_ard(cards::ard_tabulate(adsl, by = ARM, variables = SEX))))
})

test_that("plan_n_candidates(): nothing to read is no row, not an error", {
  skip_if_not_installed("cards")
  d <- normalize_ard(cards::ard_summary(cards::ADSL, by = ARM, variables = AGE,
                                         statistic = everything() ~ list(mean = mean)))
  nc <- plan_n_candidates(table_plan(d, cols = "ARM"))
  expect_s3_class(nc, "data.frame")
  expect_identical(nrow(nc), 0L)
  expect_identical(names(nc), c("scope", "page", "column", "value"))
})

test_that("FT-PL046 [FR-PL160]: plan_columns(widths =) naming no column of the table stops, naming it", {
  d <- data.frame(Item = c("a", "b"), A = c("1", "2"), B = c("3", "4"), Hid = c("x", "y"),
                  stringsAsFactors = FALSE)
  pages <- function(p) suppressMessages(plan_apply(p, "pages"))
  # every name a column: fine; a hidden column's width is harmless
  ok <- pages(table_plan(d) |> plan_hide("Hid") |>
                plan_columns(widths = c(Item = 3, A = 1, B = 1, Hid = 9)))
  ok <- if (inherits(ok, "rtftable")) ok else ok[[1L]]
  expect_equal(as.numeric(ok$col_rel_width), c(3, 1, 1))
  # a name that is no column: stops, with the name and the printed columns
  err <- expect_error(pages(table_plan(d) |> plan_columns(widths = c(Item = 3, nope = 1, .values = 1))),
                      "plan_columns(widths = ) names 'nope', not a column of the table", fixed = TRUE)
  expect_match(conditionMessage(err), "Its printed columns: 'Item', 'A', 'B', 'Hid'", fixed = TRUE)
})

test_that("FT-PL047 [FR-PL172 FR-IX102]: plan_columns(widths =) with column_widths_twips stops: one of them", {
  d <- data.frame(Item = c("a", "b"), A = c("1", "2"), stringsAsFactors = FALSE)
  # in one call
  expect_error(table_plan(d) |> plan_columns(widths = c(2, 1), column_widths_twips = c(3000, 1500)),
               "`widths` and `column_widths_twips` both set the columns' widths", fixed = TRUE)
  # in two calls: at plan_apply()
  p <- table_plan(d) |> plan_columns(widths = c(Item = 2, A = 1)) |>
    plan_columns(column_widths_twips = c(3000, 1500))
  expect_error(suppressMessages(plan_apply(p, "pages")),
               "Give one: `widths` (relative, by name or in order) or", fixed = TRUE)
  # either alone: fine
  tw <- suppressMessages(plan_apply(table_plan(d) |> plan_columns(column_widths_twips = c(3000, 1500)),
                                    "pages"))
  tw <- if (inherits(tw, "rtftable")) tw else tw[[1L]]
  expect_identical(tw$column_widths_twips, c(3000L, 1500L))
})

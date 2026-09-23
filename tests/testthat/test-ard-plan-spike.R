# Tests for the DEFERRED, LAST-WINS plan spike (#474).
# This whole file belongs to R/ard-plan-spike.R and is deleted with it.

skip_if_no_cards2 <- function() testthat::skip_if_not_installed("cards")

# Flattening is RUN before the plan now, so that rtf_plan()'s roles
# name columns that exist.  This keeps the tests to one line.
nz <- function(x) {
  if (identical(rtfreporter:::.plan_source_kind(x), "ard"))
    suppressMessages(ard_normalize(x)) else x
}

plan_ard <- function() {
  adsl <- cards::ADSL
  adsl$SEX <- as.character(adsl$SEX)
  adsl$TRT <- as.character(adsl$ARM)
  cards::ard_stack(
    adsl, .by = TRT,
    cards::ard_continuous(
      variables = c(AGE, BMIBL),
      statistic = ~ cards::continuous_summary_fns(c("N", "mean", "sd"))),
    cards::ard_categorical(variables = SEX, statistic = ~ c("n", "p")),
    .total_n = TRUE)
}

base_plan <- function(ard = plan_ard()) {
  rtf_plan(nz(ard), cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(continuous  = c("n"         = "{N:d}",
                               "Mean (SD)" = "{mean} ({sd})"),
               categorical = "{n:d} ({p:.1f%})")
}

# --------------------------------------------------------------- the rule

test_that("a later layer wins, which is the whole point", {
  skip_if_no_cards2()
  p <- base_plan() |> plan_digits(2) |> plan_digits(AGE = 0)
  cells <- apply_plan(p, "args")$cells

  # set everything, then fix one variable -- a two-line edit
  expect_match(cells$AGE[["Mean (SD)"]], "{mean:.0f} ({sd:.0f})", fixed = TRUE)
  expect_match(cells$BMIBL[["Mean (SD)"]], "{mean:.2f} ({sd:.2f})",
               fixed = TRUE)
})

test_that("last wins for the same key too, not just for a narrower one", {
  skip_if_no_cards2()
  p <- base_plan() |> plan_digits(2) |> plan_digits(AGE = 0) |>
    plan_digits(AGE = 3)
  expect_match(apply_plan(p, "args")$cells$AGE[["Mean (SD)"]],
               "{mean:.3f}", fixed = TRUE)
})

test_that("a later plan_cells() replaces an earlier entry for that key", {
  skip_if_no_cards2()
  p <- base_plan() |> plan_cells(continuous = c("n" = "{N:d}"))
  ent <- apply_plan(p, "args")$cells$continuous
  expect_length(ent, 1L)
  expect_identical(unname(ent), "{N:d}")
})

test_that("levels and labels merge one name at a time", {
  skip_if_no_cards2()
  p <- base_plan() |>
    plan_levels(TRT = c("Placebo", "Xanomeline Low Dose",
                        "Xanomeline High Dose")) |>
    plan_levels(SEX = c("M", "F"))          # adds, does not replace
  lv <- apply_plan(p, "args")$levels
  expect_setequal(names(lv), c("TRT", "SEX"))
  expect_identical(lv$SEX, c("M", "F"))
})

test_that("one key restated is replaced, not merged into", {
  skip_if_no_cards2()
  p <- base_plan() |> plan_levels(SEX = c("M", "F")) |>
    plan_levels(SEX = c("F", "M"))
  expect_identical(apply_plan(p, "args")$levels$SEX, c("F", "M"))
})

test_that("a map can be handed over whole, not taken apart", {
  skip_if_no_cards2()
  lab <- c(AGE = "Age (years)", SEX = "Sex")
  p <- base_plan() |> plan_labels(lab)
  expect_identical(unlist(apply_plan(p, "args")$labels), lab)
})

# ------------------------------------------------------------ the digits

test_that("a token that states its own digits keeps them", {
  skip_if_no_cards2()
  p <- rtf_plan(nz(plan_ard()), cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(continuous = c("Mean (SD)" = "{mean:.4f} ({sd})")) |>
    plan_digits(1)
  ent <- apply_plan(p, "args")$cells$AGE
  expect_match(ent[["Mean (SD)"]], "{mean:.4f} ({sd:.1f})", fixed = TRUE)
})

test_that("`{p:%}` with no digits declared says what to do about it", {
  skip_if_no_cards2()
  p <- rtf_plan(nz(plan_ard()), cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(categorical = "{n:d} ({p:%})")
  expect_error(apply_plan(p, "args"), "asks the plan for its digits")
  expect_error(apply_plan(p, "args"), "plan_digits")
})

test_that("digits pick by specificity once last-wins has had its say", {
  skip_if_no_cards2()
  # `continuous` is a kind, `AGE` is a variable: the variable is narrower
  p <- rtf_plan(nz(plan_ard()), cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(continuous  = c("Mean (SD)" = "{mean} ({sd})"),
               categorical = "{n:d} ({p:%})") |>
    plan_digits(continuous = 2) |> plan_digits(AGE = 0) |>
    plan_digits(SEX = 1)
  cells <- apply_plan(p, "args")$cells
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
  a <- apply_plan(p, "args")
  expect_true(all(c("cols", "rows", "cells") %in% names(a)))
})

test_that("the plan prints its layers in the order that decides the result", {
  skip_if_no_cards2()
  out <- utils::capture.output(print(base_plan() |> plan_digits(1)))
  expect_true(any(grepl("what cols / rows / label may name", out)))
  expect_true(any(grepl("cols ", out)))
  expect_true(any(grepl("1\\. cells", out)))
  expect_true(any(grepl("2\\. digits", out)))
  expect_true(any(grepl("apply_plan", out)))
})

test_that("a plan with no layers says so rather than printing nothing", {
  skip_if_no_cards2()
  out <- utils::capture.output(print(rtf_plan(nz(plan_ard()))))
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
    ard |> ard_normalize() |>
      ard_spread(cols = "TRT", rows = c(group = "variable"), cells = cells))
  planned <- suppressMessages(apply_plan(
    rtf_plan(nz(ard), cols = "TRT", rows = c(group = "variable")) |>
      plan_cells(continuous = cells$continuous,
                 categorical = cells$categorical)))
  expect_equal(planned, direct)
})

test_that("stage = 'long' is the frame going in", {
  skip_if_no_cards2()
  ard <- plan_ard()
  expect_equal(apply_plan(base_plan(ard), "long"), ard_normalize(ard))
})

# ----------------------------------------------------------- the seam

test_that("a plan can start from an already-normalized frame", {
  skip_if_no_cards2()
  ard <- plan_ard()
  d <- ard_normalize(ard)
  d$variable <- ifelse(d$variable == "BMIBL", "AGE", d$variable)  # a seam edit

  p <- rtf_plan(d, cols = "TRT", rows = c(group = "variable"))
  expect_identical(p$kind, "normalized")
  expect_true(any(grepl("normalized frame",
                        utils::capture.output(print(p)))))

  out <- suppressMessages(apply_plan(
    p |> plan_cells(continuous = c("n" = "{N:d}"), categorical = "{n:d}")))
  expect_true(is.data.frame(out))
})

test_that("a raw ARD is refused, with the line to write", {
  skip_if_no_cards2()
  # the roles name columns of what is handed in, so a frame that has
  # not been flattened cannot be one of them
  expect_error(rtf_plan(plan_ard()), "NORMALIZED frame")
  expect_error(rtf_plan(plan_ard()), "ard_normalize()", fixed = TRUE)
})

test_that("a role that names no column blames rtf_plan()", {
  skip_if_no_cards2()
  expect_error(rtf_plan(nz(plan_ard()), cols = "ARM"),
               "no column .ARM. in the data")
  expect_error(rtf_plan(nz(plan_ard()), cols = "TRT",
                        rows = c(g = "nope")),
               "rtf_plan(rows = )", fixed = TRUE)
})

test_that("a plan with no data is a template until it is given one", {
  skip_if_no_cards2()
  p <- rtf_plan(cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(continuous = c("n" = "{N:d}"), categorical = "{n:d}")
  expect_null(p$data)
  expect_error(apply_plan(p), "TEMPLATE")
  out <- suppressMessages(apply_plan(plan_data(p, nz(plan_ard()))))
  expect_true(is.data.frame(out))
})

# --------------------------------------------------------------- refusals

test_that("two unnamed values are refused", {
  skip_if_no_cards2()
  expect_error(plan_digits(rtf_plan(plan_ard()), 1, 2), "at most one unnamed")
})

test_that("the rounding family is last-wins, like every other layer", {
  skip_if_no_cards2()
  p <- base_plan() |> plan_digits(1, round = "sas") |> plan_digits(2, round = "r")
  expect_identical(apply_plan(p, "args")$round, "r")
})

test_that("one rounding family reaches ard_spread()", {
  skip_if_no_cards2()
  p <- base_plan() |> plan_digits(1) |> plan_digits(1, round = "sas")
  expect_identical(apply_plan(p, "args")$round, "sas")
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
  expect_identical(kind(ard_normalize(plan_ard())), "normalized")
  expect_identical(kind(hand_long()), "long")
  expect_identical(kind(data.frame(group = "ALT", A = "31.2")), "wide")
})

test_that("a long frame nobody built with cards makes a table", {
  out <- suppressMessages(apply_plan(
    rtf_plan(nz(hand_long()), cols = "TRT", rows = c(param = "PARAM"),
                  label = c(row = "stat_name"), notes = FALSE) |>
      plan_cells(c("n" = "{n:.0f}", "Mean (SD)" = "{mean} ({sd})")) |>
      plan_digits(2)))
  expect_identical(names(out), c("param", "row", "A", "B"))
  # the plan-wide digits reached a frame with no `variable` column at all
  expect_identical(out$A[out$param == "ALT" & out$row == "Mean (SD)"],
                   "31.25 (4.10)")
})

test_that("per-variable keys work once the column is called `variable`", {
  out <- suppressMessages(apply_plan(
    rtf_plan(nz(hand_long("variable")), cols = "TRT", rows = c(param = "variable"),
                  label = c(row = "stat_name"), notes = FALSE) |>
      plan_cells(c("Mean (SD)" = "{mean} ({sd})")) |>
      plan_digits(2) |> plan_digits(AST = 3)))
  expect_identical(out$A[out$param == "ALT"], "31.25 (4.10)")
  expect_identical(out$A[out$param == "AST"], "28.700 (3.920)")
})

test_that("a digits key that reached nothing is refused, not ignored", {
  p <- rtf_plan(nz(hand_long()), cols = "TRT", rows = c(param = "PARAM"),
                label = c(row = "stat_name"), notes = FALSE) |>
    plan_cells(c("Mean (SD)" = "{mean} ({sd})")) |>
    plan_digits(2) |>
    plan_digits(AST = 3)          # AST is a PARAM value, not a variable
  expect_error(apply_plan(p, "args"), "matched nothing")
  expect_error(apply_plan(p, "args"), "AST")
})

test_that("a frame that is already the table is refused at the door", {
  # the point of deferring: this is caught at rtf_plan(), not three stages
  # later inside the resolver
  # the refusal moved to resolution: a listing's source is an ordinary frame
  # too, and only plan_listing() can say which this is
  p <- rtf_plan(data.frame(group = "ALT", A = "31.2 (4.1)"))
  expect_error(apply_plan(p), "declares nothing")
  expect_error(apply_plan(p), "plan_listing")
})

test_that("ard_spread() tolerates a frame with no variable/context", {
  # it used to fail with an internal R error rather than a message
  out <- suppressMessages(ard_spread(
    hand_long(), cols = "TRT", rows = c(param = "PARAM"),
    label = c(row = "stat_name"),
    cells = c("Mean (SD)" = "{mean:.1f} ({sd:.1f})"), notes = FALSE))
  expect_identical(out$A[out$param == "ALT"], "31.2 (4.1)")
})

test_that("a missing .label says what to do instead of naming the ARD", {
  expect_error(
    ard_spread(hand_long(), cols = "TRT", rows = c(param = "PARAM"),
               cells = "{mean:.1f}"),
    "has no `.label`")
  expect_error(
    ard_spread(hand_long(), cols = "TRT", rows = c(param = "PARAM"),
               cells = "{mean:.1f}"),
    "carries the row identity")
})
# --------------------------------------------------------- the display half

disp_plan <- function(ard = plan_ard()) {
  rtf_plan(nz(ard), cols = "TRT", rows = c(group = "variable"),
                notes = FALSE) |>
    plan_cells(continuous  = c("Mean (SD)" = "{mean:.1f} ({sd:.2f})"),
               categorical = "{n:.0f} ({p:.1f%})")
}

test_that("stage = \"pages\" reaches the same rtftable as the code does", {
  skip_if_no_cards2()
  ard <- plan_ard()
  tbl <- suppressMessages(apply_plan(disp_plan(ard)))

  ref <- as_rtftables(tbl, stub_vars = c("group", "label"),
                      group_by = "indent", split = "group_safe",
                      max_rows = 22, border = "tfl")
  new <- suppressMessages(apply_plan(
    disp_plan(ard) |>
      plan_stub(vars = c("group", "label")) |>
      plan_group(mode = "indent") |>
      plan_pages(split = "group_safe", max_rows = 22) |>
      plan_style(border = "tfl"),
    "pages"))
  expect_equal(new, ref)
})

test_that("plan_col_header(n = ) reads the ARD and spends it", {
  skip_if_no_cards2()
  # the point of resolving them together: the denominator in the header and
  # the percentages under it come from one reading of one ARD
  new <- suppressMessages(apply_plan(
    disp_plan() |>
      plan_stub(vars = c("group", "label")) |> plan_style(border = "tfl") |>
      plan_col_header(n = function(a) ard_pull(a, cols = "TRT", variable = "AGE"),
                      header = function(n) c("Characteristic",
                                             paste0(names(n), " N=",
                                                    as.integer(n)))),
    "pages"))
  hdr <- unlist(new[[1]]$col_header)
  expect_true(any(grepl("Placebo N=86", hdr, fixed = TRUE)))
})

test_that("a literal n is taken as it is", {
  skip_if_no_cards2()
  new <- suppressMessages(apply_plan(
    disp_plan() |>
      plan_stub(vars = c("group", "label")) |>
      plan_col_header(n = 254L,
                      header = function(n) c(paste0("All (N=", n, ")"),
                                             "A", "B", "C")),
    "pages"))
  expect_true(any(grepl("All (N=254)", unlist(new[[1]]$col_header),
                        fixed = TRUE)))
})

test_that("plan_stub() folds the row keys before as_rtftables() sees them", {
  skip_if_no_cards2()
  new <- suppressMessages(apply_plan(
    disp_plan() |>
      plan_stub(vars = c("group", "label"), into = "row_label") |>
      plan_style(border = "tfl"),
    "pages"))
  expect_true("row_label" %in% names(new[[1]]$data))
})

test_that("plan_after() runs its steps on the pages, in order", {
  skip_if_no_cards2()
  seen <- character(0)
  new <- suppressMessages(apply_plan(
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
})

test_that("the display verbs are last-wins too", {
  skip_if_no_cards2()
  p <- disp_plan() |>
    plan_pages(max_rows = 10) |> plan_style(border = "tfl") |>
    plan_pages(max_rows = 40)          # later wins, `border` is kept
  rtf <- rtfreporter:::.plan_rtf_args(p)
  expect_identical(rtf$max_rows, 40)
  expect_identical(rtf$border, "tfl")
})

test_that("how far a plan goes is read off what it declares", {
  skip_if_no_cards2()
  # nothing about the display: the answer is the table data.frame
  expect_identical(.plan_reach(disp_plan()), "table")
  expect_true(is.data.frame(suppressMessages(apply_plan(disp_plan()))))

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
  expect_true(is.data.frame(suppressMessages(apply_plan(p, "table"))))
  expect_s3_class(suppressMessages(apply_plan(p, "long")), "data.frame")
  expect_true(all(c("cols", "cells") %in% names(apply_plan(p, "args"))))
  expect_type(suppressMessages(apply_plan(p)), "list")
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

test_that("rtf_tables() takes a plan, so apply_plan() is for looking", {
  skip_if_no_cards2()
  p <- disp_plan() |>
    plan_stub(vars = c("group", "label")) |> plan_style(border = "tfl")
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(
      header = rtf_header(rows = list(c(c = "T"))),
      footer = rtf_footer(rows = list(c(l = "F")))))
  direct <- suppressMessages(rtf_tables(doc, p))
  byhand <- suppressMessages(rtf_tables(doc, apply_plan(p, "pages")))
  expect_equal(direct, byhand)
})
# ------------------------------------------------- conditional cell styles

styled <- function(...) {
  suppressMessages(apply_plan(
    disp_plan() |>
      plan_stub(vars = c("group", "label"), into = "row_label",
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
  pg <- suppressMessages(apply_plan(
    disp_plan() |>
      plan_stub(vars = c("group", "label"), into = "row_label",
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
  expect_error(styled(bold = ~ TRUE, bold = ~ FALSE), "twice in one call")
  expect_error(plan_digits(disp_plan(), AGE = 1, AGE = 2), "twice in one call")
})

test_that("a column that is not there is named, not ignored", {
  skip_if_no_cards2()
  expect_error(styled(bold = list(NOPE = ~ TRUE)), "no printed column")
  expect_error(styled(bold = list(NOPE = ~ TRUE)), "Available")
})

test_that("a style that is not a one-sided formula is refused", {
  skip_if_no_cards2()
  expect_error(styled(bold = "yes"), "one-sided formula")
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
  expect_error(apply_plan(p, "pages"), "before = TRUE")
  expect_error(apply_plan(p, "pages"), "wrong row")
})

test_that("plan_cell_style() and plan_style(cell_styles=) do not both apply", {
  skip_if_no_cards2()
  p <- disp_plan() |>
    plan_stub(vars = c("group", "label"), into = "row_label",
              before = TRUE) |>
    plan_cell_style(bold = ~ TRUE) |>
    plan_style(cell_styles = list(NULL))
  expect_error(apply_plan(p, "pages"), "use one")
})
# --------------------------------------------------------- plan_template()

test_that("plan_template() writes a plan that runs to the pages", {
  skip_if_no_cards2()
  ard <- plan_ard()
  gen <- utils::capture.output(code <- plan_template(ard, cols = "TRT",
                                                     pipe = "|>"))
  expect_true(any(grepl("ard_normalize()", code, fixed = TRUE)))
  expect_true(any(grepl("rtf_plan(", code, fixed = TRUE)))
  expect_true(any(grepl("plan_cells(", code, fixed = TRUE)))
  # both halves, because a plan that stops at the table is half a plan
  expect_true(any(grepl("plan_style(", code, fixed = TRUE)))
  expect_true(any(grepl("plan_stub(", code, fixed = TRUE)))
  expect_true(any(grepl("apply_plan(p)", code, fixed = TRUE)))

  e <- new.env(); assign("ard", ard, e)
  suppressMessages(eval(parse(text = paste(code, collapse = "
")), e))
  # the template stops at the plan: rtf_tables() takes it from there
  p <- get("p", e)
  expect_s3_class(p, "rtf_plan")
  pg <- suppressMessages(apply_plan(p, "pages"))
  expect_s3_class(pg[[1]], "rtftable")
})

test_that("plan_template() derives the stub and leaves the rest to be edited", {
  skip_if_no_cards2()
  code <- utils::capture.output(
    invisible(plan_template(plan_ard(), cols = "TRT", pipe = "|>")))
  # the stub is not written at all: plan_stub() works it out from what
  # rtf_plan(rows = ) and plan_group() already declared
  expect_false(any(grepl("vars", code, fixed = TRUE)))
  expect_true(any(grepl("plan_stub(", code, fixed = TRUE)))
  expect_true(any(grepl("edit this", code, fixed = TRUE)))
})

test_that("plan_template() takes the pipe like ard_template() does", {
  skip_if_no_cards2()
  base <- utils::capture.output(
    invisible(plan_template(plan_ard(), cols = "TRT", pipe = "|>")))
  mag <- utils::capture.output(
    invisible(plan_template(plan_ard(), cols = "TRT", pipe = "%>%")))
  expect_false(any(grepl("%>%", base, fixed = TRUE)))
  expect_true(any(grepl("library(magrittr)", mag, fixed = TRUE)))
})

test_that("plan_template(spec = ) reads the definition file instead", {
  skip_if_no_cards2()
  code <- utils::capture.output(
    invisible(plan_template(plan_ard(), cols = "TRT", spec = TRUE,
                            pipe = "|>")))
  expect_true(any(grepl("read_ard_spec", code, fixed = TRUE)))
  expect_false(any(grepl("plan_cells(", code, fixed = TRUE)))
})

test_that("the generated header writes a real newline escape", {
  skip_if_no_cards2()
  # one unambiguous denominator, so a header block is written at all
  ard <- cards::ard_stack(
    cards::ADSL, .by = ARM,
    cards::ard_continuous(variables = AGE,
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

test_that("plan_mutate() and plan_filter() work on the long frame", {
  skip_if_no_cards2()
  p <- rtf_plan(nz(plan_ard()), cols = "TRT",
                rows = c(group = "variable"), notes = FALSE) |>
    plan_filter(stat_name != "sd") |>
    plan_mutate(variable = toupper(variable)) |>
    plan_cells(continuous = c("Mean" = "{mean:.1f}"), categorical = "{n:.0f}")
  d <- suppressMessages(apply_plan(p, "long"))
  expect_false(any(d$stat_name == "sd"))
  expect_true(all(d$variable == toupper(d$variable)))
  # and the whole thing is still one sentence: nothing left the plan
  expect_true(is.data.frame(suppressMessages(apply_plan(p))))
})

test_that("the seams run in the order they were declared", {
  skip_if_no_cards2()
  # mutate-then-filter keeps what the mutate made; the other order does not
  keep <- suppressMessages(apply_plan(
    rtf_plan(nz(plan_ard())) |>
      plan_mutate(.tag = "z") |> plan_filter(.tag == "z"),
    "long"))
  drop <- suppressMessages(apply_plan(
    rtf_plan(nz(plan_ard())) |>
      plan_mutate(.tag = "z") |> plan_filter(stat_name == "sd") |>
      plan_mutate(.tag2 = "y"),
    "long"))
  expect_gt(nrow(keep), 0L)
  expect_true(all(drop$stat_name == "sd"))
  expect_true(".tag2" %in% names(drop))
})

test_that("plan_derive() takes an expression, like mutate() does", {
  skip_if_no_cards2()
  # `group` is a column of the TABLE, which is what plan_derive() sees
  out <- suppressMessages(apply_plan(
    disp_plan() |> plan_derive(flag = ifelse(group == "SEX", "y", "n"))))
  expect_true("flag" %in% names(out))
  expect_setequal(unique(out$flag), c("y", "n"))
})

test_that("a seam still takes a function for what an expression cannot", {
  skip_if_no_cards2()
  out <- suppressMessages(apply_plan(
    disp_plan() |> plan_derive(function(d) d[order(d$label), , drop = FALSE])))
  expect_true(is.data.frame(out))
})

test_that("an unnamed argument that is not a function is refused", {
  skip_if_no_cards2()
  expect_error(
    suppressMessages(apply_plan(disp_plan() |> plan_mutate(group))),
    "has to be a function")
})

test_that("print() names the columns of every stage it has", {
  skip_if_no_cards2()
  p <- disp_plan() |>
    plan_stub(vars = c("group", "label"), into = "row_label") |>
    plan_style(border = "tfl")

  out <- utils::capture.output(print(p))
  # the frame going in is to hand, so it is always answered
  expect_true(any(grepl("cols / rows / label may name", out,
                        fixed = TRUE)))
  expect_true(any(grepl("stat_name", out, fixed = TRUE)))
  # spreading is not, so it says so rather than costing a run
  expect_true(any(grepl("not computed yet", out, fixed = TRUE)))

  invisible(suppressMessages(apply_plan(p)))
  out2 <- utils::capture.output(print(p))
  expect_true(any(grepl("after spread", out2, fixed = TRUE)))
  expect_true(any(grepl("as printed", out2, fixed = TRUE)))
  expect_true(any(grepl("row_label", out2, fixed = TRUE)))
})

test_that("a derived plan does not inherit its parent's column cache", {
  skip_if_no_cards2()
  p <- disp_plan()
  invisible(suppressMessages(apply_plan(p)))
  expect_false(is.null(p$cache$table))
  p2 <- p |> plan_mutate(extra = "x")
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
  rtf_plan(.pages_src()) |>
    plan_listing(listing_col("USUBJID", width = 12)) |>
    plan_pages(max_rows = max_rows)
}

test_that("plan_titles() / plan_footnotes() ride on every page", {
  pg <- suppressMessages(apply_plan(
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
  n <- length(suppressMessages(apply_plan(base)))
  expect_gt(n, 1L)
  pg <- suppressMessages(apply_plan(
    base |> plan_titles(pages = as.list(paste("Part", seq_len(n))))))
  expect_identical(attr(pg[[1]], "rtf_titles"), "Part 1")
  expect_identical(attr(pg[[n]], "rtf_titles"), paste("Part", n))
})

test_that("a page count that does not match is refused, and named", {
  p <- .pages_plan() |> plan_titles(pages = list("only one"))
  expect_error(apply_plan(p), "block for")
  expect_error(apply_plan(p), "plan_titles")
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
  pg <- suppressMessages(apply_plan(
    rtf_plan(d) |>
      plan_filter(AGE >= 45) |>
      plan_listing(listing_col("USUBJID", width = 12),
                   listing_col("ARM", width = 10)) |>
      plan_pages(max_rows = 20) |>
      plan_style(border = "tfl") |>
      plan_titles("Listing 16.2.1")))
  expect_s3_class(pg[[1]], "rtftable")
  expect_identical(attr(pg[[1]], "rtf_titles"), "Listing 16.2.1")
  # the filter reached the records, and nothing was normalised or spread
  tb <- suppressMessages(apply_plan(
    rtf_plan(d) |> plan_filter(AGE >= 45) |>
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
  new <- suppressMessages(apply_plan(
    rtf_plan(d) |>
      plan_listing(listing_col("USUBJID", width = 12),
                   listing_col("ARM", width = 10)) |>
      plan_pages(max_rows = 20) |>
      plan_style(border = "tfl")))
  expect_equal(new, ref)
})

test_that("a frame that is neither an ARD nor a listing says which to add", {
  expect_error(apply_plan(rtf_plan(data.frame(a = "x", b = "y"))),
               "plan_listing")
  expect_error(apply_plan(rtf_plan(data.frame(a = "x", b = "y"))),
               "display verbs")
})

# ------------------------------------------- the three ways in, one system

test_that("a finished table is a source for the display half alone", {
  skip_if_no_cards2()
  # pattern 2: ard_*() built the table, the plan does the rest
  ard <- plan_ard()
  tbl <- suppressMessages(
    ard |> ard_normalize() |>
      ard_spread(cols = "TRT", rows = c(group = "variable"),
                 cells = "{n:.0f}", notes = FALSE))
  ref <- as_rtftables(tbl, read_meta = FALSE,
                      stub_vars = c("group", "label"), border = "tfl")
  new <- suppressMessages(apply_plan(
    rtf_plan(tbl) |>
      plan_stub(vars = c("group", "label")) |>
      plan_style(border = "tfl")))
  expect_equal(new, ref)
})

test_that("asking for the ARD half of a finished table says what to drop", {
  skip_if_no_cards2()
  p <- rtf_plan(nz(data.frame(group = "A", x = "1")), cols = "x") |>
    plan_cells("{n}")
  expect_error(apply_plan(p), "plan_cells() need them", fixed = TRUE)
  expect_error(apply_plan(p), "keep the display")
})

test_that("plan_group(show = FALSE) hides the carrier it groups by", {
  skip_if_no_cards2()
  p <- disp_plan() |> plan_group(col = "group", show = FALSE)
  expect_identical(rtfreporter:::.plan_rtf_args(p)$group_col, "group")
  expect_identical(rtfreporter:::.plan_rtf_args(p)$drop_cols, "group")
})

test_that("hiding ADDS rather than replaces", {
  skip_if_no_cards2()
  # last-wins here would silently un-hide the first column named
  p <- disp_plan() |> plan_hide("a") |> plan_hide("b") |>
    plan_group(col = "c", show = FALSE)
  expect_setequal(rtfreporter:::.plan_rtf_args(p)$drop_cols, c("a", "b", "c"))
})

test_that("the verbs refuse anything that is not a plan", {
  expect_error(plan_cells(data.frame(a = 1), "x"), "Expected an rtf_plan")
  expect_error(apply_plan(data.frame(a = 1)), "Expected an rtf_plan")
  expect_error(plan_data(data.frame(a = 1), data.frame(b = 2)),
               "Expected an rtf_plan")
  expect_error(plan_data(rtf_plan(), NULL), "required")
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
  p <- rtf_plan(own_frame(), cols = "TRT", rows = c(group = "PARAM"),
                label = c(label = "CAT"),
                variable = "PARAM", stat_name = "STAT", stat = "VALUE",
                notes = FALSE) |>
    plan_cells(AGE = c("Mean (SD)" = "{mean} ({sd})"),
               SEX = "{n}") |>
    plan_digits(1)
  out <- suppressMessages(apply_plan(p))
  expect_true(all(c("group", "label", "A", "B") %in% names(out)))
  expect_true(any(grepl("55.5 (4.2)", out$A, fixed = TRUE)))
})

test_that("a column that is not there blames the role that named it", {
  expect_error(
    rtf_plan(own_frame(), cols = "TRT", stat_name = "PARAMCD"),
    "no column .PARAMCD. in the data")
})

test_that("`label` naming two columns coalesces them", {
  # tfrmt has the same problem: a continuous row is labelled by its
  # statistic and a categorical one by its level, and they are not the
  # same column.  First non-missing wins.
  p <- rtf_plan(own_frame(), cols = "TRT", rows = c(group = "PARAM"),
                label = list(row = c("CAT", "STAT")),
                variable = "PARAM", stat_name = "STAT", stat = "VALUE",
                notes = FALSE) |>
    plan_cells("{n}", AGE = "{mean}") |>
    plan_digits(1)
  out <- suppressMessages(apply_plan(p, "table"))
  expect_true("row" %in% names(out))
  # the categorical rows took CAT, the continuous ones fell back to STAT
  expect_true(all(c("F", "M") %in% out$row))
  expect_true(any(c("N", "mean", "sd") %in% out$row))
})

test_that("the same plan serves a second study, data and all", {
  tmpl <- rtf_plan(cols = "TRT", rows = c(group = "PARAM"),
                   label = c(label = "CAT"),
                   variable = "PARAM", stat_name = "STAT",
                   stat = "VALUE", notes = FALSE) |>
    plan_cells(AGE = c("Mean" = "{mean}"), SEX = "{n}") |>
    plan_digits(1)

  d2 <- own_frame()
  d2$VALUE <- d2$VALUE + 1
  a <- suppressMessages(apply_plan(plan_data(tmpl, own_frame())))
  b <- suppressMessages(apply_plan(plan_data(tmpl, d2)))
  expect_identical(names(a), names(b))
  expect_false(identical(a$A, b$A))
})


# -------------------------------------------------------- the row order

test_that("plan_sort() goes to the ARD half, where the statistics are", {
  skip_if_no_cards2()
  p <- base_plan() |> plan_sort(".overall", "group", "-n")
  a <- apply_plan(p, "args")
  expect_identical(a$sort, c(".overall", "group", "-n"))
  # and nothing reaches as_rtftables(), which could not sort on `-n`:
  # by then the statistic is a formatted cell, not a number
  r <- rtfreporter:::.plan_rtf_args(p)
  expect_null(r$sort_by)
})

test_that("plan_sort() over a finished table sorts the table", {
  d <- data.frame(group = c("B", "A"), x = c("1", "2"),
                  stringsAsFactors = FALSE)
  p <- rtf_plan(d) |> plan_sort("group") |> plan_pages(max_rows = 10)
  r <- rtfreporter:::.plan_rtf_args(p)
  expect_identical(r$sort_by, "group")
  out <- suppressMessages(apply_plan(p))
  first <- if (inherits(out, "rtftable")) out else out[[1L]]
  expect_identical(first$data$group[1L], "A")
})

test_that("sorting alone does not turn a table into RTF pages", {
  skip_if_no_cards2()
  p <- base_plan() |> plan_sort("group")
  expect_true(is.data.frame(suppressMessages(apply_plan(p))))
})


# ------------------------------------ needed and not wanted, said once

test_that("the spread builds only what the roles named", {
  skip_if_no_cards2()
  # there is nothing to drop, because nothing unasked-for is built: a
  # column is a row key, the label, or a spread column
  out <- suppressMessages(apply_plan(base_plan(), "table"))
  keys <- c("group", "label")
  arms <- setdiff(names(out), keys)
  expect_setequal(intersect(names(out), keys), keys)
  expect_true(all(grepl("Placebo|Xanomeline", arms)))
})

test_that("plan_pages(show = FALSE) hides the key the break reads", {
  skip_if_no_cards2()
  p <- disp_plan() |>
    plan_derive(pg = ifelse(group == "SEX", "1", "2")) |>
    plan_pages(split = "by_value", by = "pg", show = FALSE)
  out <- suppressMessages(apply_plan(p))
  first <- if (inherits(out, "rtftable")) out else out[[1L]]
  expect_false("pg" %in% names(first$data))
  expect_gt(length(out), 1L)          # it really did break on it
})

test_that("plan_sort(show = FALSE) hides a sort carrier, and only it", {
  d <- data.frame(ord = c("2", "1"), label = c("B", "A"),
                  x = c("9", "8"), stringsAsFactors = FALSE)
  p <- rtf_plan(d) |> plan_sort("ord", show = FALSE) |>
    plan_pages(max_rows = 10)
  out <- suppressMessages(apply_plan(p))
  first <- if (inherits(out, "rtftable")) out else out[[1L]]
  expect_false("ord" %in% names(first$data))
  expect_identical(first$data$label[1L], "A")
})

test_that("a sort key that is not a column is not mistaken for one", {
  skip_if_no_cards2()
  # ".overall" and "-n" are instructions, not columns: hiding must not
  # try to drop them
  p <- base_plan() |> plan_sort(".overall", "group", "-n", show = FALSE) |>
    plan_pages(max_rows = 40)
  out <- suppressMessages(apply_plan(p))
  first <- if (inherits(out, "rtftable")) out else out[[1L]]
  expect_false("group" %in% names(first$data))
  expect_true("label" %in% names(first$data))
})


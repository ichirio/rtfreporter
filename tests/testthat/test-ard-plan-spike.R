# Tests for the DEFERRED, LAST-WINS plan spike (#474).
# This whole file belongs to R/ard-plan-spike.R and is deleted with it.

skip_if_no_cards2 <- function() testthat::skip_if_not_installed("cards")

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
  ard_plan(ard) |>
    plan_spread(cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(continuous  = c("n"         = "{N:d}",
                               "Mean (SD)" = "{mean} ({sd})"),
               categorical = "{n:d} ({p:.1f%})")
}

# --------------------------------------------------------------- the rule

test_that("a later layer wins, which is the whole point", {
  skip_if_no_cards2()
  p <- base_plan() |> plan_digits(2) |> plan_digits(AGE = 0)
  cells <- apply_plan(p, "args")$spread$cells

  # set everything, then fix one variable -- a two-line edit
  expect_match(cells$AGE[["Mean (SD)"]], "{mean:.0f} ({sd:.0f})", fixed = TRUE)
  expect_match(cells$BMIBL[["Mean (SD)"]], "{mean:.2f} ({sd:.2f})",
               fixed = TRUE)
})

test_that("last wins for the same key too, not just for a narrower one", {
  skip_if_no_cards2()
  p <- base_plan() |> plan_digits(2) |> plan_digits(AGE = 0) |>
    plan_digits(AGE = 3)
  expect_match(apply_plan(p, "args")$spread$cells$AGE[["Mean (SD)"]],
               "{mean:.3f}", fixed = TRUE)
})

test_that("a later plan_cells() replaces an earlier entry for that key", {
  skip_if_no_cards2()
  p <- base_plan() |> plan_cells(continuous = c("n" = "{N:d}"))
  ent <- apply_plan(p, "args")$spread$cells$continuous
  expect_length(ent, 1L)
  expect_identical(unname(ent), "{N:d}")
})

test_that("levels and labels merge one name at a time", {
  skip_if_no_cards2()
  p <- base_plan() |>
    plan_spread(levels = list(TRT = c("Placebo", "Xanomeline Low Dose",
                                      "Xanomeline High Dose"))) |>
    plan_spread(levels = list(SEX = c("M", "F")))     # adds, does not replace
  lv <- apply_plan(p, "args")$spread$levels
  expect_setequal(names(lv), c("TRT", "SEX"))
  expect_identical(lv$SEX, c("M", "F"))
})

test_that("a scalar field is replaced wholesale", {
  skip_if_no_cards2()
  p <- base_plan() |> plan_spread(sep = "__") |> plan_spread(sep = "____")
  expect_identical(apply_plan(p, "args")$spread$sep, "____")
})

# ------------------------------------------------------------ the digits

test_that("a token that states its own digits keeps them", {
  skip_if_no_cards2()
  p <- ard_plan(plan_ard()) |>
    plan_spread(cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(continuous = c("Mean (SD)" = "{mean:.4f} ({sd})")) |>
    plan_digits(1)
  ent <- apply_plan(p, "args")$spread$cells$AGE
  expect_match(ent[["Mean (SD)"]], "{mean:.4f} ({sd:.1f})", fixed = TRUE)
})

test_that("`{p:%}` with no digits declared says what to do about it", {
  skip_if_no_cards2()
  p <- ard_plan(plan_ard()) |>
    plan_spread(cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(categorical = "{n:d} ({p:%})")
  expect_error(apply_plan(p, "args"), "asks the plan for its digits")
  expect_error(apply_plan(p, "args"), "plan_digits")
})

test_that("digits pick by specificity once last-wins has had its say", {
  skip_if_no_cards2()
  # `continuous` is a kind, `AGE` is a variable: the variable is narrower
  p <- ard_plan(plan_ard()) |>
    plan_spread(cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(continuous  = c("Mean (SD)" = "{mean} ({sd})"),
               categorical = "{n:d} ({p:%})") |>
    plan_digits(continuous = 2) |> plan_digits(AGE = 0) |>
    plan_digits(SEX = 1)
  cells <- apply_plan(p, "args")$spread$cells
  expect_match(cells$BMIBL[["Mean (SD)"]], "{mean:.2f}", fixed = TRUE)
  expect_match(cells$AGE[["Mean (SD)"]], "{mean:.0f}", fixed = TRUE)
  expect_match(cells$SEX, "{p:.1f%}", fixed = TRUE)
})

# ----------------------------------------------------- nothing runs early

test_that("the ARD is held, not transformed, and nothing is computed", {
  skip_if_no_cards2()
  ard <- plan_ard()
  p <- base_plan(ard) |> plan_digits(1)
  expect_identical(p$ard, ard)
  expect_false(is.data.frame(p$layers))
  # `args` resolves without running the conversion
  a <- apply_plan(p, "args")
  expect_setequal(names(a), c("normalize", "spread"))
})

test_that("the plan prints its layers in the order that decides the result", {
  skip_if_no_cards2()
  out <- utils::capture.output(print(base_plan() |> plan_digits(1)))
  expect_true(any(grepl("nothing computed yet", out)))
  expect_true(any(grepl("1\\. spread", out)))
  expect_true(any(grepl("3\\. digits", out)))
  expect_true(any(grepl("apply_plan", out)))
})

test_that("an empty plan says so rather than printing nothing", {
  skip_if_no_cards2()
  out <- utils::capture.output(print(ard_plan(plan_ard())))
  expect_true(any(grepl("empty", out)))
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
    ard_plan(ard) |>
      plan_spread(cols = "TRT", rows = c(group = "variable")) |>
      plan_cells(continuous = cells$continuous,
                 categorical = cells$categorical)))
  expect_equal(planned, direct)
})

test_that("stage = 'normalize' is what ard_normalize() returns", {
  skip_if_no_cards2()
  ard <- plan_ard()
  expect_equal(apply_plan(base_plan(ard), "normalize"), ard_normalize(ard))
})

# ----------------------------------------------------------- the seam

test_that("a plan can start from an already-normalized frame", {
  skip_if_no_cards2()
  ard <- plan_ard()
  d <- ard_normalize(ard)
  d$variable <- ifelse(d$variable == "BMIBL", "AGE", d$variable)  # a seam edit

  p <- ard_plan(d)
  expect_true(p$normalized)
  expect_true(any(grepl("normalized frame",
                        utils::capture.output(print(p)))))

  out <- suppressMessages(apply_plan(
    p |> plan_spread(cols = "TRT", rows = c(group = "variable")) |>
      plan_cells(continuous = c("n" = "{N:d}"), categorical = "{n:d}")))
  expect_true(is.data.frame(out))
})

test_that("a raw ARD is NOT mistaken for a normalized one", {
  skip_if_no_cards2()
  # `stat_name` is in a raw cards ARD too; only ard_normalize()'s own
  # columns may be used to tell them apart
  expect_false(isTRUE(ard_plan(plan_ard())$normalized))
})

test_that("plan_normalize() on a normalized frame is refused, not ignored", {
  skip_if_no_cards2()
  p <- ard_plan(ard_normalize(plan_ard())) |>
    plan_normalize(hierarchy = "SEX") |>
    plan_spread(cols = "TRT")
  expect_error(apply_plan(p), "does not need flattening")
  expect_error(apply_plan(p), "hierarchy")
})

# --------------------------------------------------------------- refusals

test_that("two unnamed values are refused", {
  skip_if_no_cards2()
  expect_error(plan_digits(ard_plan(plan_ard()), 1, 2), "at most one unnamed")
})

test_that("two rounding families are refused rather than guessed", {
  skip_if_no_cards2()
  p <- base_plan() |> plan_digits(1) |>
    plan_round("sas") |> plan_round(AGE = "r")
  expect_error(apply_plan(p, "args"), "more than one family")
})

test_that("one rounding family reaches ard_spread()", {
  skip_if_no_cards2()
  p <- base_plan() |> plan_digits(1) |> plan_round("sas")
  expect_identical(apply_plan(p, "args")$spread$round, "sas")
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
    ard_plan(hand_long()) |>
      plan_spread(cols = "TRT", rows = c(param = "PARAM"),
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
    ard_plan(hand_long("variable")) |>
      plan_spread(cols = "TRT", rows = c(param = "variable"),
                  label = c(row = "stat_name"), notes = FALSE) |>
      plan_cells(c("Mean (SD)" = "{mean} ({sd})")) |>
      plan_digits(2) |> plan_digits(AST = 3)))
  expect_identical(out$A[out$param == "ALT"], "31.25 (4.10)")
  expect_identical(out$A[out$param == "AST"], "28.700 (3.920)")
})

test_that("a digits key that reached nothing is refused, not ignored", {
  p <- ard_plan(hand_long()) |>
    plan_spread(cols = "TRT", rows = c(param = "PARAM"),
                label = c(row = "stat_name"), notes = FALSE) |>
    plan_cells(c("Mean (SD)" = "{mean} ({sd})")) |>
    plan_digits(2) |>
    plan_digits(AST = 3)          # AST is a PARAM value, not a variable
  expect_error(apply_plan(p, "args"), "matched nothing")
  expect_error(apply_plan(p, "args"), "AST")
})

test_that("a frame that is already the table is refused at the door", {
  # the point of deferring: this is caught at ard_plan(), not three stages
  # later inside the resolver
  expect_error(ard_plan(data.frame(group = "ALT", A = "31.2 (4.1)")),
               "already the table")
  expect_error(ard_plan(data.frame(group = "ALT", A = "31.2 (4.1)")),
               "as_rtftables")
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
  ard_plan(ard) |>
    plan_spread(cols = "TRT", rows = c(group = "variable"),
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
      plan_rtf(stub_vars = c("group", "label"), group_by = "indent",
               split = "group_safe", max_rows = 22, border = "tfl"),
    "pages"))
  expect_equal(new, ref)
})

test_that("plan_n() reads the ARD and plan_header() spends it", {
  skip_if_no_cards2()
  # the point of resolving them together: the denominator in the header and
  # the percentages under it come from one reading of one ARD
  new <- suppressMessages(apply_plan(
    disp_plan() |>
      plan_n(arm = function(a) ard_pull(a, cols = "TRT", variable = "AGE")) |>
      plan_rtf(stub_vars = c("group", "label"), border = "tfl") |>
      plan_header(function(n) c("Characteristic",
                                paste0(names(n$arm), " N=",
                                       as.integer(n$arm)))),
    "pages"))
  hdr <- unlist(new[[1]]$col_header)
  expect_true(any(grepl("Placebo N=86", hdr, fixed = TRUE)))
})

test_that("a literal plan_n() value is taken as it is", {
  skip_if_no_cards2()
  new <- suppressMessages(apply_plan(
    disp_plan() |>
      plan_n(total = 254L) |>
      plan_rtf(stub_vars = c("group", "label")) |>
      plan_header(function(n) c(paste0("All (N=", n$total, ")"),
                                "A", "B", "C")),
    "pages"))
  expect_true(any(grepl("All (N=254)", unlist(new[[1]]$col_header),
                        fixed = TRUE)))
})

test_that("plan_stub() folds the row keys before as_rtftables() sees them", {
  skip_if_no_cards2()
  new <- suppressMessages(apply_plan(
    disp_plan() |>
      plan_stub(vars = c("group", "label"), label = "row_label") |>
      plan_rtf(border = "tfl"),
    "pages"))
  expect_true("row_label" %in% names(new[[1]]$data))
})

test_that("plan_after() runs its steps on the pages, in order", {
  skip_if_no_cards2()
  seen <- character(0)
  new <- suppressMessages(apply_plan(
    disp_plan() |>
      plan_rtf(stub_vars = c("group", "label")) |>
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
    plan_rtf(max_rows = 10, border = "tfl") |>
    plan_rtf(max_rows = 40)          # later wins, `border` is kept
  rtf <- rtfreporter:::.plan_merge(rtfreporter:::.plan_of(p, "rtf"))
  expect_identical(rtf$max_rows, 40)
  expect_identical(rtf$border, "tfl")
})

test_that("how far a plan goes is read off what it declares", {
  skip_if_no_cards2()
  # nothing about the display: the answer is the table data.frame
  expect_identical(.plan_reach(disp_plan()), "table")
  expect_true(is.data.frame(suppressMessages(apply_plan(disp_plan()))))

  # anything that only makes sense once there are pages moves the answer
  for (v in list(function(p) plan_rtf(p, border = "tfl"),
                 function(p) plan_header(p, c("a", "b", "c", "d")),
                 function(p) plan_styles(p, bold = ~ TRUE),
                 function(p) plan_after(p, identity))) {
    expect_identical(.plan_reach(v(disp_plan())), "pages")
  }

  # calling the verb IS the declaration, even with nothing in it
  expect_identical(.plan_reach(plan_rtf(disp_plan())), "pages")
})

test_that("a named stage still stops where it is told, for looking inside", {
  skip_if_no_cards2()
  p <- disp_plan() |> plan_rtf(stub_vars = c("group", "label"))
  expect_true(is.data.frame(suppressMessages(apply_plan(p, "table"))))
  expect_s3_class(suppressMessages(apply_plan(p, "normalize")), "data.frame")
  expect_setequal(names(apply_plan(p, "args")), c("normalize", "spread"))
  expect_type(suppressMessages(apply_plan(p)), "list")
})

test_that("the plan says which of the two it will give", {
  skip_if_no_cards2()
  expect_true(any(grepl("table data.frame",
                        utils::capture.output(print(disp_plan())),
                        fixed = TRUE)))
  expect_true(any(grepl("RTF pages",
    utils::capture.output(print(plan_rtf(disp_plan(), border = "tfl"))),
    fixed = TRUE)))
})

test_that("rtf_tables() takes a plan, so apply_plan() is for looking", {
  skip_if_no_cards2()
  p <- disp_plan() |>
    plan_rtf(stub_vars = c("group", "label"), border = "tfl")
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
      plan_stub(vars = c("group", "label"), label = "row_label") |>
      plan_styles(...) |>
      plan_rtf(border = "tfl"),
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
      plan_stub(vars = c("group", "label"), label = "row_label") |>
      plan_styles(bold = ~ TRUE) |>
      plan_styles(bold = ~ FALSE) |>          # a later LAYER wins
      plan_rtf(border = "tfl"),
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
  p <- disp_plan() |> plan_styles(bold = ~ TRUE) |>
    plan_rtf(stub_vars = c("group", "label"))
  expect_error(apply_plan(p, "pages"), "plan_stub\\(\\)")
  expect_error(apply_plan(p, "pages"), "wrong row")
})

test_that("plan_styles() and plan_rtf(cell_styles=) do not both apply", {
  skip_if_no_cards2()
  p <- disp_plan() |>
    plan_stub(vars = c("group", "label"), label = "row_label") |>
    plan_styles(bold = ~ TRUE) |>
    plan_rtf(cell_styles = list(NULL))
  expect_error(apply_plan(p, "pages"), "both set the same")
})
# --------------------------------------------------------- plan_template()

test_that("plan_template() writes a plan that runs to the pages", {
  skip_if_no_cards2()
  ard <- plan_ard()
  gen <- utils::capture.output(code <- plan_template(ard, cols = "TRT",
                                                     pipe = "|>"))
  expect_true(any(grepl("ard_plan(ard)", code, fixed = TRUE)))
  expect_true(any(grepl("plan_spread(", code, fixed = TRUE)))
  expect_true(any(grepl("plan_cells(", code, fixed = TRUE)))
  # both halves, because a plan that stops at the table is half a plan
  expect_true(any(grepl("plan_rtf(", code, fixed = TRUE)))
  expect_true(any(grepl("plan_stub(", code, fixed = TRUE)))
  expect_true(any(grepl("apply_plan(p)", code, fixed = TRUE)))

  e <- new.env(); assign("ard", ard, e)
  suppressMessages(eval(parse(text = paste(code, collapse = "
")), e))
  pg <- get("pages", e)
  expect_type(pg, "list")
  expect_s3_class(pg[[1]], "rtftable")
})

test_that("plan_template() derives the stub and leaves the rest to be edited", {
  skip_if_no_cards2()
  code <- utils::capture.output(
    invisible(plan_template(plan_ard(), cols = "TRT", pipe = "|>")))
  # stub_vars is the one as_rtftables() setting the ARD can answer
  expect_true(any(grepl('vars  = c("group", "label")', code, fixed = TRUE)))
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
  expect_true(any(grepl("plan_header", code, fixed = TRUE)))
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
  expect_false(any(grepl("rtfreporter::plan_header(", code, fixed = TRUE)))
  # and the pipeline still parses: the trailing pipe must have been removed
  expect_silent(parse(text = paste(code, collapse = "\n")))
})

test_that("the verbs refuse anything that is not a plan", {
  expect_error(plan_cells(data.frame(a = 1), "x"), "Expected an ard_plan")
  expect_error(apply_plan(data.frame(a = 1)), "Expected an ard_plan")
  expect_error(ard_plan(NULL), "required")
})

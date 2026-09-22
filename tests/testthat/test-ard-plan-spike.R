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
test_that("the verbs refuse anything that is not a plan", {
  expect_error(plan_cells(data.frame(a = 1), "x"), "Expected an ard_plan")
  expect_error(apply_plan(data.frame(a = 1)), "Expected an ard_plan")
  expect_error(ard_plan(NULL), "required")
})

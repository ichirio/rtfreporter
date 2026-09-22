# ============================================================================
#  SPIKE #474 -- the same table, written twice
# ============================================================================
#
#  Run from the repository root:
#
#      Rscript data-raw/ard-plan-spike/try-it.R
#
#  Left:  ard_normalize() |> ard_spread()   -- runs as it is called
#  Right: ard_plan() |> plan_*() |> apply_plan()  -- declarations, LAST WINS
#
#  The point of the spike is the third block: changing the decimals for ONE
#  variable.  With the immediate form the templates are the only place digits
#  live, so a per-variable change means writing that variable's whole `cells`
#  entry.  With a plan it is one more line that wins over the earlier one.
# ============================================================================

# The spike lives on this branch, so load the SOURCE rather than whatever
# version happens to be installed.
suppressMessages({
  if (requireNamespace("pkgload", quietly = TRUE) &&
      file.exists("R/ard-plan-spike.R")) {
    pkgload::load_all(".", quiet = TRUE)
  } else {
    library(rtfreporter)
  }
  library(cards)
})

data(ADSL, package = "cards")
adsl <- ADSL
adsl$TRT01P <- as.character(adsl$ARM)
adsl$SEX    <- as.character(adsl$SEX)

ard <- ard_stack(
  adsl, .by = TRT01P,
  ard_continuous(variables = c(AGE, BMIBL),
                 statistic = ~ continuous_summary_fns(c("N", "mean", "sd"))),
  ard_categorical(variables = SEX, statistic = ~ c("n", "p")),
  .total_n = TRUE)

bar <- function(s) cat("\n", strrep("=", 72), "\n ", s, "\n",
                       strrep("=", 72), "\n", sep = "")

# -- 1. the immediate form ---------------------------------------------------
bar("1. ard_normalize() |> ard_spread()   (runs as it is called)")

direct <- ard |>
  ard_normalize() |>
  ard_spread(
    cols  = "TRT01P",
    rows  = c(group = "variable"),
    cells = list(
      continuous  = c("n"         = "{N:d}",
                      "Mean (SD)" = "{mean:.2f} ({sd:.2f})"),
      categorical = "{n:d} ({p:.1f%})"),
    notes = FALSE)
print(as.data.frame(direct))

# -- 2. the same thing as a plan ---------------------------------------------
bar("2. ard_plan() |> plan_*() |> apply_plan()   (nothing runs until the end)")

p <- ard_plan(ard) |>
  plan_spread(cols = "TRT01P", rows = c(group = "variable"), notes = FALSE) |>
  plan_cells(continuous  = c("n"         = "{N:d}",
                             "Mean (SD)" = "{mean} ({sd})"),
             categorical = "{n:d} ({p:%})") |>   # `{p:%}`: digits from the plan
  plan_digits(2) |>
  plan_digits(SEX = 1)          # same declarations as block 1

print(p)                                   # a plan is inspectable before it runs
planned <- apply_plan(p)
print(as.data.frame(planned))

cat("\nsame answer as the immediate form: ",
    isTRUE(all.equal(as.data.frame(direct), as.data.frame(planned))), "\n",
    sep = "")

# -- 3. the reason to want this: change ONE variable --------------------------
bar("3. now put AGE on 0 dp -- ONE more line, and it wins")

tuned <- p |>
  plan_digits(AGE = 0)

# what the layers resolved to, before anything is computed
cat("\nresolved `cells` (this is the argument ard_spread() will be given):\n")
str(apply_plan(tuned, "args")$spread$cells, max.level = 1, give.attr = FALSE)

cat("\n")
print(as.data.frame(apply_plan(tuned)))

# -- 4. the seam still works -------------------------------------------------
bar("4. reaching in with dplyr, then starting a plan from the result")

d <- ard_normalize(ard)
d <- d[d$variable != "BMIBL", ]            # the kind of edit no plan can declare

reentered <- ard_plan(d) |>                # a normalized frame is accepted
  plan_spread(cols = "TRT01P", rows = c(group = "variable"), notes = FALSE) |>
  plan_cells(continuous = c("n" = "{N:d}", "Mean (SD)" = "{mean} ({sd})"),
             categorical = "{n:d} ({p:%})") |>
  plan_digits(1) |>
  apply_plan()
print(as.data.frame(reentered))

bar("done")

# ============================================================================
#  SPIKE #474 -- the same table, written twice
# ============================================================================
#
#  Run from the repository root:
#
#      Rscript data-raw/ard-plan-spike/try-it.R
#
#  Left:  ard_normalize() |> ard_spread()          -- runs as it is called
#  Right: rtf_plan() |> plan_*() |> apply_plan()   -- declarations, LAST WINS
#
#  Flattening is NOT deferred either way.  rtf_plan() takes the normalized
#  frame and the ROLES -- which column goes across, which go down, which
#  carries the row identity -- so the names it is given are names you can
#  see, the way ggplot(data, aes(x, y)) works.
#
#  The point of the spike is block 3: changing the decimals for ONE
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
bar("2. rtf_plan() |> plan_*() |> apply_plan()   (the roles, said once)")

nz <- ard_normalize(ard)        # run it, and look at it if you like

p <- nz |>
  rtf_plan(cols = "TRT01P", rows = c(group = "variable"), notes = FALSE) |>
  plan_cells(continuous  = c("n"         = "{N:d}",
                             "Mean (SD)" = "{mean} ({sd})"),
             categorical = "{n:d} ({p:%})") |>   # `{p:%}`: digits from the plan
  plan_digits(2) |>
  plan_digits(SEX = 1)          # same declarations as block 1

print(p)                            # the roles, the layers, and the columns
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
str(apply_plan(tuned, "args")$cells, max.level = 1, give.attr = FALSE)

cat("\n")
print(as.data.frame(apply_plan(tuned)))

# -- 4. dplyr, wherever it is needed -----------------------------------------
bar("4. reaching in with dplyr -- before the plan, or inside it")

d <- nz[nz$variable != "BMIBL", ]      # the kind of edit no plan can declare

reentered <- d |>
  rtf_plan(cols = "TRT01P", rows = c(group = "variable"), notes = FALSE) |>
  plan_cells(continuous = c("n" = "{N:d}", "Mean (SD)" = "{mean} ({sd})"),
             categorical = "{n:d} ({p:%})") |>
  plan_digits(1) |>
  apply_plan()
print(as.data.frame(reentered))

# -- 5. a frame that never went near cards -----------------------------------
bar("5. no cards at all -- a study's own summary, with its own names")

# Nothing here is called `variable`, `stat_name` or `stat`, and the row label
# lives in two different columns depending on the kind of row.  Say so, once.
own <- data.frame(
  TRT    = rep(c("A", "B"), each = 5),
  PARAM  = rep(c("ALT", "ALT", "ALT", "GRADE", "GRADE"), 2),
  CAT    = c(NA, NA, NA, "Grade 1", "Grade 2",
             NA, NA, NA, "Grade 1", "Grade 2"),
  STAT   = c("n", "mean", "sd", "n", "n"),
  VALUE  = c(20, 31.245, 4.1, 6, 4, 18, 33.108, 5.3, 7, 5),
  stringsAsFactors = FALSE)

print(as.data.frame(apply_plan(
  own |>
    rtf_plan(cols = "TRT", rows = c(param = "PARAM"),
             # every statistic is a row of its own, and WHICH COLUMN names
             # the row depends on the kind of row: a level for the
             # categorical ones, the statistic for the continuous ones.
             # Coalesce them -- first non-missing wins.
             stats    = "rows",
             label    = list(row = c("CAT", "STAT")),
             variable = "PARAM", stat_name = "STAT", stat = "VALUE",
             notes    = FALSE))))

# -- 6. a plan with no data is a template ------------------------------------
bar("6. the roles without the data: one house style, every study")

house <- rtf_plan(cols = "TRT01P", rows = c(group = "variable"),
                  notes = FALSE) |>
  plan_cells(continuous  = c("n" = "{N:d}", "Mean (SD)" = "{mean} ({sd})"),
             categorical = "{n:d} ({p:%})") |>
  plan_digits(1)

print(house)                            # it prints, it just cannot run
print(as.data.frame(apply_plan(plan_data(house, nz))))

bar("done")

# A small fictional ADLB (Example Pharma) for lab shift tables by direction
# (#1081): Albumin has a grade toward low only, Potassium toward low and
# toward high.  The grades are ADaM's: BTOXGRL / ATOXGRL (Low) and
# BTOXGRH / ATOXGRH (High), factors with every grade 0 to 4.
.shift_data <- function() {
  set.seed(1081)
  ids <- sprintf("EX-%03d", 1:12)
  draw <- function(n) as.character(sample(0:3, n, replace = TRUE,
                                          prob = c(0.5, 0.25, 0.15, 0.1)))
  alb <- data.frame(USUBJID = ids[1:10], PARAM = "Albumin (g/L)",
                    BTOXGRL = draw(10), ATOXGRL = draw(10),
                    BTOXGRH = NA_character_, ATOXGRH = NA_character_)
  pot <- data.frame(USUBJID = ids[1:11], PARAM = "Potassium (mmol/L)",
                    BTOXGRL = draw(11), ATOXGRL = draw(11),
                    BTOXGRH = draw(11), ATOXGRH = draw(11))
  pot$ATOXGRL[3] <- NA               # no post-baseline grade toward low
  adlb <- rbind(alb, pot)
  adlb$PARAM <- factor(adlb$PARAM)
  for (v in c("BTOXGRL", "ATOXGRL", "BTOXGRH", "ATOXGRH")) {
    adlb[[v]] <- factor(adlb[[v]], levels = as.character(0:4))
  }
  list(adsl = data.frame(USUBJID = ids), adlb = adlb)
}

# The ARD with ADaM's names: per direction the records are filtered and
# only a direction column, tox_dir, is added; four tabulations each (the
# cells, the Total column, the Total row, the corner).
.shift_ard <- function(x = .shift_data()) {
  adsl <- x$adsl
  adlb <- x$adlb
  lo <- adlb[!is.na(adlb$BTOXGRL) & !is.na(adlb$ATOXGRL), ]
  lo$tox_dir <- "Low"
  hi <- adlb[!is.na(adlb$BTOXGRH) & !is.na(adlb$ATOXGRH), ]
  hi$tox_dir <- "High"
  cards::bind_ard(
    cards::ard_total_n(adsl),
    cards::ard_tabulate(lo, variables = ATOXGRL, by = c(PARAM, tox_dir, BTOXGRL), denominator = adsl),
    cards::ard_tabulate(lo, variables = ATOXGRL, by = c(PARAM, tox_dir), denominator = adsl),
    cards::ard_tabulate(lo, variables = BTOXGRL, by = c(PARAM, tox_dir), denominator = adsl),
    cards::ard_tabulate(lo, variables = tox_dir, by = PARAM, denominator = adsl),
    cards::ard_tabulate(hi, variables = ATOXGRH, by = c(PARAM, tox_dir, BTOXGRH), denominator = adsl),
    cards::ard_tabulate(hi, variables = ATOXGRH, by = c(PARAM, tox_dir), denominator = adsl),
    cards::ard_tabulate(hi, variables = BTOXGRH, by = c(PARAM, tox_dir), denominator = adsl),
    cards::ard_tabulate(hi, variables = tox_dir, by = PARAM, denominator = adsl))
}

# The reference route: one variable per role made by stacking the two
# directions' records (tox_dir, base_gr, worst_gr), then tabulated.
.shift_ref_ard <- function(x = .shift_data()) {
  adsl <- x$adsl
  adlb <- x$adlb
  one <- function(dir, b, a) data.frame(USUBJID = adlb$USUBJID, PARAM = adlb$PARAM,
                                        tox_dir = dir, base_gr = adlb[[b]],
                                        worst_gr = adlb[[a]])
  df <- rbind(one("Low", "BTOXGRL", "ATOXGRL"), one("High", "BTOXGRH", "ATOXGRH"))
  df <- df[!is.na(df$base_gr) & !is.na(df$worst_gr), ]
  df$tox_dir <- factor(df$tox_dir, levels = c("Low", "High"))
  cards::bind_ard(
    cards::ard_total_n(adsl),
    cards::ard_tabulate(df, variables = worst_gr, by = c(PARAM, tox_dir, base_gr), denominator = adsl),
    cards::ard_tabulate(df, variables = worst_gr, by = c(PARAM, tox_dir), denominator = adsl),
    cards::ard_tabulate(df, variables = base_gr, by = c(PARAM, tox_dir), denominator = adsl),
    cards::ard_tabulate(df, variables = tox_dir, by = PARAM, denominator = adsl))
}

.shift_combine <- list(worst_gr = c("ATOXGRL", "ATOXGRH"),
                       base_gr  = c("BTOXGRL", "BTOXGRH"))

# The plan of the shift table (the same for both routes)
.shift_plan <- function(nd) {
  grades <- c("0" = "Grade 0", "1" = "Grade 1", "2" = "Grade 2",
              "3" = "Grade 3", "4" = "Grade 4")
  table_plan(nd, cols = "base_gr", rows = c(PARAM = "PARAM", tox_dir = "tox_dir"),
             label = c(is.na(.label) ~ "Total", ~ "{.label}")) |>
    plan_levels(tox_dir = c("Low", "High")) |>
    plan_cells("{n:.0f} ({p:.1f%})", notes = FALSE) |>
    plan_total(label = "Total", direction = "both") |>
    plan_labels(base_gr = grades, worst_gr = grades)
}

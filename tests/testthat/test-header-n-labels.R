# Header N when plan_labels relabels the column values

test_that("FT-IX6806 [FR-IX419]: plan_labels on the column variable: the header N is still each arm's count", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  ard <- cards::bind_ard(
    cards::ard_tabulate(adsl, variables = ARM),
    cards::ard_summary(adsl, by = ARM, variables = AGE,
                       statistic = ~ cards::continuous_summary_fns(c("N", "mean"))))
  arms <- unique(as.character(adsl$ARM))
  lab <- stats::setNames(paste("Arm", seq_along(arms)), arms)
  p <- normalize_ard(ard) |>
    table_plan(cols = "ARM") |>
    plan_labels(ARM = lab) |>
    plan_cells(continuous = list(n = "{N:.0f}", Mean = "{mean:.1f}"), notes = FALSE) |>
    plan_col_header(header = data.frame(line = "1", cols = ".values", span = "each",
                                        text = "{col} (N={n})"))
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f))
  w <- character()
  withCallingHandlers(
    suppressMessages(generate_rtfreport(rtf_tables(rtf_document(), plan_apply(p)),
                                        f, overwrite = TRUE)),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    })
  r <- paste(readLines(f), collapse = "\n")
  n <- c(table(adsl$ARM))
  for (a in names(n)) {
    expect_match(r, sprintf("%s (N=%d)", lab[[a]], n[[a]]), fixed = TRUE, info = a)
  }
  expect_false(grepl("N=NA", r, fixed = TRUE))
  expect_false(any(grepl("N=NA|header N", w)))
})

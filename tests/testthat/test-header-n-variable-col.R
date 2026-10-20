# `variable` as a column key: each arm's N over its variables' columns

test_that("cols = c(arm, variable): the arm's N from its own count", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  ard <- cards::bind_ard(
    cards::ard_tabulate(adsl, variables = ARM),
    cards::ard_summary(adsl, by = c(ARM, SEX), variables = c(AGE, HEIGHTBL),
                       statistic = ~ cards::continuous_summary_fns(c("N", "mean"))))
  hdr <- data.frame(line = c("1", "2"), cols = ".values", span = c("ARM", "each"),
                    text = c("{col1} (N={n})", "{col}"))
  p <- normalize_ard(ard) |>
    table_plan(cols = c("ARM", "variable"), rows = list(group = "SEX")) |>
    plan_cells(continuous = list(n = "{N:.0f}", Mean = "{mean:.1f}"), notes = FALSE) |>
    plan_col_header(header = hdr)
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f))
  suppressWarnings(suppressMessages(
    generate_rtfreport(rtf_tables(rtf_document(), plan_apply(p)), f, overwrite = TRUE)))
  r <- paste(readLines(f), collapse = "\n")
  n <- c(table(adsl$ARM))
  for (a in names(n)) {
    expect_match(r, sprintf("%s (N=%d)", a, n[[a]]), fixed = TRUE, info = a)
  }
  expect_false(grepl("N=NA", r, fixed = TRUE))
})

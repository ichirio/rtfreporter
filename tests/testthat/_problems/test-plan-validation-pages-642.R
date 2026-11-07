# Extracted from test-plan-validation-pages.R:642

# prequel ----------------------------------------------------------------------
vp_wide <- function() {
  data.frame(
    grp = c("A", "A", "B", "B", "C", "C"),
    label = c("n", "Mean", "n", "Mean", "n", "Mean"),
    X = c("1", "2.5", "3", "4.5", "5", "6.5"),
    Y = c("7", "8.5", "9", "10.5", "11", "12.5"),
    stringsAsFactors = FALSE)
}
vp_first <- function(x) if (inherits(x, "rtftable")) x else x[[1L]]
vp_pages <- function(p) suppressMessages(plan_apply(p, "pages"))
vp_direct <- function(d = vp_wide(), ...) as_rtftables(d, read_meta = FALSE, ...)
vp_ard <- function() {
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  cards::bind_ard(
    cards::ard_tabulate(adsl, variables = TRT),
    cards::ard_stack(adsl, .by = TRT,
      cards::ard_summary(variables = AGE,
        statistic = ~ cards::continuous_summary_fns(c("N", "mean")))))
}
vp_ard_plan <- function() {
  table_plan(suppressMessages(normalize_ard(vp_ard())), cols = "TRT",
             rows = c(group = "variable")) |>
    plan_cells(notes = FALSE) |>
    plan_cells(continuous = c(n = "{N:d}", Mean = "{mean:.1f}"))
}
vp_rtf_lines <- function(pages) {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  suppressMessages(generate_rtfreport(rtf_tables(rtf_document(), pages), f,
                                      overwrite = TRUE))
  readLines(f, warn = FALSE)
}

# test -------------------------------------------------------------------------
mk <- function(visits, paged = TRUE) {
    st <- expand.grid(VISIT = visits, PARAM = c("ALT", "HGB"), ARM = "Placebo", stringsAsFactors = FALSE)
    st$variable <- "AVAL"; st$stat_name <- "mean"; st$stat <- seq_len(nrow(st)) * 1.5
    st$.label <- NA_character_; st$.kind <- "continuous"
    p <- table_plan(st, cols = "ARM", rows = c(group1 = "PARAM", group2 = "VISIT", group3 = "variable")) |>
      plan_cells(continuous = c(Mean = "{mean:.1f}")) |>
      plan_stub(name = "row_label")
    p <- if (paged) plan_paginate_rows(p, max_rows = 30L, split = "group_safe", page_by = "group1")
         else plan_paginate_rows(p, max_rows = 30L, split = "group_safe")
    lapply(vp_pages(p), function(z) z$data$row_label)
  }
nb <- intToUtf8(160L)
one <- mk("WEEK 12")
expect_length(one, 2L)
for (pg in one) expect_identical(pg[1:2], c("WEEK 12", paste0(nb, nb, nb, nb, "AVAL")))
two <- mk(c("WEEK 4", "WEEK 12"))
expect_length(two, 2L)
for (pg in two) expect_identical(pg[c(1L, 4L)], c("WEEK 4", "WEEK 12"))
whole <- mk("WEEK 12", paged = FALSE)
expect_identical(sum(unlist(whole) == "WEEK 12"), 1L)

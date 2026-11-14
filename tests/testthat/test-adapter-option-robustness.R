## tests/testthat/test-adapter-option-robustness.R
##
## Regression guard for rtfreporter's headline promise: reading ANY table
## object directly.  These tests run the three priority object types -- gt,
## gtsummary (-> gt), and rtables/tern (VTableTree) -- through as_rtftable()
## under a wide range of *option-triggered* layouts that have historically
## stressed the adapters (multi-stub gt, row groups, summary rows, merged
## columns, multi-level column splits, hierarchical / strata tables, ...), and
## assert each is read without error into a sane body.  Version-robust: they
## check structure (no error, column count, header stacking, titles), not
## framework-specific cell strings.  All skip when their package is absent.

library(testthat)

# Small synthetic analysis dataset (USUBJID so tern occurrence functions run).
.opt_df <- function() {
  set.seed(1L)
  n <- 40L
  data.frame(
    USUBJID = sprintf("S%03d", seq_len(n)),
    ARM  = factor(sample(c("A", "B"), n, TRUE), levels = c("A", "B")),
    SEX  = factor(sample(c("M", "F"), n, TRUE)),
    AGE  = round(stats::rnorm(n, 55, 10)),
    RESP = factor(sample(c("CR", "PD"), n, TRUE)),
    stringsAsFactors = FALSE
  )
}

# Build the table lazily, read it, and assert a sane rtftable body.
.expect_reads <- function(make, min_cols = 1L) {
  tbl <- suppressWarnings(suppressMessages(make()))
  rt  <- expect_no_error(as_rtftable(tbl, read_meta = TRUE))
  expect_s3_class(rt, "rtftable")
  expect_true(is.data.frame(rt$data))
  expect_gte(ncol(rt$data), min_cols)
  invisible(rt)
}


# ── gt option matrix ──────────────────────────────────────────────────────────

test_that("gt: a range of option-driven layouts read without error", {
  skip_if_not_installed("gt")
  df <- .opt_df()[1:9, ]
  cases <- list(
    groupname_col       = function() gt::gt(df, rowname_col = "SEX",
                                             groupname_col = "ARM"),
    row_group_as_column = function() gt::gt(df, rowname_col = "SEX",
                                            groupname_col = "ARM",
                                            row_group_as_column = TRUE),
    tab_row_group       = function() gt::tab_row_group(
                            gt::gt(df[, c("SEX", "AGE")]), label = "Grp",
                            rows = 1:3),
    grand_summary_rows  = function() gt::grand_summary_rows(
                            gt::gt(df[, "AGE", drop = FALSE]), columns = "AGE",
                            fns = list(Mean ~ mean(.))),
    cols_merge_range    = function() gt::cols_merge_range(
                            gt::gt(data.frame(a = 1:3, lo = 4:6, hi = 7:9)),
                            col_begin = lo, col_end = hi),
    nested_spanners     = function() gt::tab_spanner(
                            gt::tab_spanner(gt::gt(data.frame(a = 1, b1 = 2,
                              b2 = 3)), "S", c(b1, b2)), "TOP", c(a, b1, b2)),
    sub_missing         = function() gt::sub_missing(
                            gt::gt(data.frame(a = c(1, NA))), columns = a),
    zero_row            = function() gt::gt(data.frame(a = character(0),
                                                       b = numeric(0))),
    single_column       = function() gt::gt(data.frame(only = c("a", "b"))),
    markdown            = function() gt::fmt_markdown(
                            gt::gt(data.frame(a = c("**x**", "_y_"))), a)
  )
  for (nm in names(cases)) {
    info <- paste0("gt case: ", nm)
    expect_no_error(as_rtftable(suppressWarnings(cases[[nm]]()),
                                read_meta = TRUE))
  }
})

test_that("gt: nested spanners become a stacked multi-row col_header", {
  skip_if_not_installed("gt")
  g <- gt::gt(data.frame(a = 1, b1 = 2, b2 = 3))
  g <- gt::tab_spanner(g, "S",   c(b1, b2))
  g <- gt::tab_spanner(g, "TOP", c(a, b1, b2))
  kw <- rtfreporter:::.gt_to_rtftable_kwargs(g)
  expect_true(is.list(kw$col_header))          # stacked, not a flat vector
  expect_gte(length(kw$col_header), 2L)
})


# ── gtsummary option matrix ───────────────────────────────────────────────────

test_that("gtsummary: a range of tbl_* layouts read without error", {
  skip_on_cran()                  # one of the slowest blocks (~4 s); CI runs it
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("gt")
  df <- .opt_df()
  .expect_reads(function() gtsummary::tbl_summary(df[, c("ARM", "AGE", "SEX")],
                                                  by = ARM), min_cols = 2L)
  .expect_reads(function() gtsummary::add_overall(
    gtsummary::tbl_summary(df[, c("ARM", "AGE")], by = ARM)))
  .expect_reads(function() gtsummary::add_p(
    gtsummary::tbl_summary(df[, c("ARM", "AGE", "SEX")], by = ARM)))
  .expect_reads(function() gtsummary::tbl_continuous(df, variable = AGE,
                                                     by = ARM, include = SEX))
  # (tbl_regression is covered by test-gtsummary-adapter.R; its tidy backend is
  # an environment-fragile dependency we don't re-exercise here.)
  .expect_reads(function() gtsummary::tbl_stack(list(
    gtsummary::tbl_summary(df[, "AGE", drop = FALSE]),
    gtsummary::tbl_summary(df[, "SEX", drop = FALSE]))))
  .expect_reads(function() gtsummary::bold_labels(
    gtsummary::tbl_summary(df[, c("ARM", "AGE")], by = ARM)))
})

test_that("gtsummary: tbl_merge spanners survive as a stacked col_header", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("gt")
  df <- .opt_df()
  tm <- suppressMessages(gtsummary::tbl_merge(
    list(gtsummary::tbl_summary(df[, c("ARM", "AGE")], by = ARM),
         gtsummary::tbl_summary(df[, c("ARM", "SEX")], by = ARM)),
    tab_spanner = c("**G1**", "**G2**")))
  kw <- rtfreporter:::.gt_to_rtftable_kwargs(gtsummary::as_gt(tm))
  expect_true(is.list(kw$col_header))
  expect_gte(length(kw$col_header), 2L)
})

test_that("gtsummary: hierarchical (AE-shape) tables read without error", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("cards")
  skip_if_not_installed("gt")
  df <- .opt_df()
  .expect_reads(function() gtsummary::tbl_hierarchical(
    df, variables = c(RESP), by = ARM, denominator = df, id = USUBJID,
    overall_row = TRUE), min_cols = 2L)
})


# ── rtables / tern option matrix ──────────────────────────────────────────────

test_that("rtables/tern: a range of layouts read without error", {
  skip_if_not_installed("rtables")
  skip_if_not_installed("formatters")
  df <- .opt_df()
  bt <- rtables::basic_table
  .expect_reads(function()
    rtables::build_table(rtables::analyze(rtables::split_cols_by(bt(), "ARM"),
                         "AGE", afun = mean), df), min_cols = 2L)
  .expect_reads(function()                                   # 2-level col split
    rtables::build_table(rtables::analyze(rtables::split_cols_by(
      rtables::split_cols_by(bt(), "ARM"), "SEX"), "AGE", afun = mean), df),
    min_cols = 3L)
  .expect_reads(function()                                   # col counts
    rtables::build_table(rtables::analyze(rtables::split_cols_by(
      bt(show_colcounts = TRUE), "ARM"), "AGE", afun = mean), df))
  .expect_reads(function()                                   # nested row split
    rtables::build_table(rtables::analyze(rtables::split_rows_by(
      rtables::split_cols_by(bt(), "ARM"), "SEX"), "AGE", afun = mean), df))
  .expect_reads(function()                                   # content rows
    rtables::build_table(rtables::analyze(rtables::summarize_row_groups(
      rtables::split_rows_by(rtables::split_cols_by(bt(), "ARM"), "SEX")),
      "AGE", afun = mean), df))
})

test_that("rtables/tern: tern analysis functions read without error", {
  skip_if_not_installed("tern")
  skip_if_not_installed("rtables")
  df <- .opt_df()
  bt <- rtables::basic_table
  .expect_reads(function()
    rtables::build_table(tern::analyze_vars(rtables::split_cols_by(bt(), "ARM"),
                         "AGE"), df), min_cols = 2L)
  .expect_reads(function()
    rtables::build_table(tern::count_occurrences(
      rtables::split_cols_by(bt(), "ARM"), vars = "RESP"), df), min_cols = 2L)
})

test_that("rtables: multi-level column split stacks the col_header", {
  skip_if_not_installed("rtables")
  skip_if_not_installed("formatters")
  df <- .opt_df()
  t2 <- rtables::build_table(
    rtables::analyze(rtables::split_cols_by(
      rtables::split_cols_by(rtables::basic_table(), "ARM"), "SEX"),
      "AGE", afun = mean), df)
  kw <- rtfreporter:::.rtables_to_rtftable_kwargs(t2)
  expect_true(is.list(kw$col_header))
  expect_gte(length(kw$col_header), 2L)
})

test_that("rtables: titles and footers are read into page blocks", {
  skip_if_not_installed("rtables")
  skip_if_not_installed("formatters")
  df <- .opt_df()
  t <- rtables::build_table(
    rtables::analyze(rtables::split_cols_by(rtables::basic_table(), "ARM"),
                     "AGE", afun = mean), df)
  rtables::main_title(t)  <- "My Title"
  rtables::main_footer(t) <- "My Footer"
  kw <- rtfreporter:::.rtables_to_rtftable_kwargs(t)
  expect_true("My Title" %in% kw$titles_block)
  expect_true("My Footer" %in% kw$footnotes_block)
})

test_that("rtables: a table with no data rows reads to a zero-row body", {
  skip_if_not_installed("rtables")
  skip_if_not_installed("formatters")
  df <- .opt_df()
  # label rows only (split with no analyze) -> structural rows, no data rows
  t <- rtables::build_table(
    rtables::split_rows_by(rtables::split_cols_by(rtables::basic_table(),
                                                  "ARM"), "SEX"), df)
  rt <- expect_no_error(as_rtftable(t, read_meta = TRUE))
  expect_equal(nrow(rt$data), 0L)
})

test_that("gt: several group columns give one group row per combination, formats kept (#193)", {
  skip_if_not_installed("gt")
  d <- data.frame(a = c("x", "y", "x", "y"), b = c("p", "p", "q", "p"),
                  v = c(1.234, 2, 3, 4))
  g <- gt::fmt_number(gt::gt(d, groupname_col = c("a", "b")), v, decimals = 1)
  rt <- expect_no_error(as_rtftable(g, read_meta = TRUE))
  # rows under a group are indented (non-breaking spaces)
  cells <- trimws(unlist(rt$data, use.names = FALSE), whitespace = "[\\h\\v]")
  expect_true(all(c("x - p", "y - p", "x - q") %in% cells))
  expect_true(all(c("1.2", "2.0", "4.0", "3.0") %in% cells))
  expect_lt(match("y - p", cells), match("x - q", cells))
})

test_that("gt: a table with no column stops with a clear message (#193)", {
  skip_if_not_installed("gt")
  expect_error(as_rtftable(gt::gt(data.frame())), "no column to read")
})

test_that("gt: an html() label is read as its text, <br> a line break (#193)", {
  skip_if_not_installed("gt")
  g <- gt::cols_label(gt::gt(data.frame(a = 1, b = 2, c = 3)),
                      a = gt::html("W<sub>t</sub>"), b = gt::html("A<br>B"),
                      c = gt::md("**Age**"))
  rt <- as_rtftable(g, read_meta = TRUE)
  expect_identical(unlist(rt$col_header), c("Wt", "A\nB", "Age"))
})


# ── flextable option matrix ───────────────────────────────────────────────────
#
# Draft battery (#781): option-built flextables that read as displayed today.
# Cases whose RTF differs from what flextable displays (merged cells, a
# paragraph caption, multi-cell footer rows, a deleted header, line breaks,
# footnote() marks) are left out on purpose and tracked in the #781 report.

# Body cells as a plain character matrix, non-breaking spaces trimmed.
.opt_cells <- function(rt) {
  m <- as.matrix(rt$data)
  m[] <- trimws(m, whitespace = "[\\h\\v]")
  unname(m)
}

.opt_ft_df <- function() {
  data.frame(Param = c("Age", "Weight", "Height"),
             Mean = c(55.1234, 70.5, 170.25),
             SD = c(10.5, 12.25, 8), N = c(30L, 30L, 29L),
             stringsAsFactors = FALSE)
}

test_that("flextable: option-built tables read to the displayed body and header", {
  skip_if_not_installed("flextable")
  ft <- flextable::flextable
  d <- .opt_ft_df()
  dn <- data.frame(n = c(1234.5678, NA, 0.5), i = c(1L, NA, 30L),
                   ch = c("a", NA, "c"), l = c(TRUE, NA, FALSE),
                   dt = as.Date(c("2026-01-05", NA, "2026-03-09")),
                   stringsAsFactors = FALSE)

  # labels set by set_header_labels() are the leaf header
  rt <- as_rtftable(flextable::set_header_labels(
    ft(d), Param = "Parameter", Mean = "Mean (kg)"))
  expect_identical(dim(rt$data), c(3L, 4L))
  expect_identical(unlist(rt$col_header),
                   c("Parameter", "Mean (kg)", "SD", "N"))

  # two- and three-level headers stack as spanning rows above the leaf row
  h2 <- flextable::add_header_row(ft(d), values = c("", "Stats", "N"),
                                  colwidths = c(1, 2, 1))
  rt <- as_rtftable(h2)
  expect_length(rt$col_header, 2L)
  expect_identical(dim(rt$data), c(3L, 4L))
  seg <- rt$col_header[[1L]]
  expect_identical(vapply(seg, function(s) s$label, ""), c("", "Stats", "N"))
  expect_identical(c(seg[[2L]]$from, seg[[2L]]$to), c(2L, 3L))
  h3 <- flextable::add_header_row(h2, values = "Top", colwidths = 4)
  expect_length(as_rtftable(h3)$col_header, 3L)

  # colformat_*() text and na_str are read as displayed
  num <- as_rtftable(flextable::colformat_num(
    ft(dn), j = "n", big.mark = ",", na_str = "--"))
  expect_identical(.opt_cells(num)[2L, 1L], "--")
  expect_match(.opt_cells(num)[1L, 1L], "^1,234[.]5")
  dbl <- as_rtftable(flextable::colformat_double(
    ft(dn), j = "n", digits = 2, na_str = "n/a"))
  expect_identical(.opt_cells(dbl)[, 1L], c("1,234.57", "n/a", "0.50"))
  int <- as_rtftable(flextable::colformat_int(ft(dn), j = "i",
                                              na_str = "missing"))
  expect_identical(.opt_cells(int)[, 2L], c("1", "missing", "30"))
  chr <- as_rtftable(flextable::colformat_char(ft(dn), j = "ch",
                                               na_str = "(none)"))
  expect_identical(.opt_cells(chr)[, 3L], c("a", "(none)", "c"))
  lgl <- as_rtftable(flextable::colformat_lgl(
    ft(dn), j = "l", true = "Yes", false = "No", na_str = "?"))
  expect_identical(.opt_cells(lgl)[, 4L], c("Yes", "?", "No"))
  dte <- as_rtftable(flextable::colformat_date(
    ft(dn), j = "dt", fmt_date = "%d/%m/%Y", na_str = "UNK"))
  expect_identical(.opt_cells(dte)[, 5L], c("05/01/2026", "UNK", "09/03/2026"))

  # default NA cells read as empty, never as the text "NA"
  na <- as_rtftable(ft(data.frame(a = c("x", NA), b = c(NA, 2.5),
                                  stringsAsFactors = FALSE)))
  expect_identical(.opt_cells(na), cbind(c("x", ""), c("", "2.5")))

  # compose() / as_paragraph() / mk_par(): the composed text is the cell text
  cp <- as_rtftable(flextable::compose(
    ft(d), j = "Mean",
    value = flextable::as_paragraph(flextable::as_b(sprintf("%.1f", Mean)),
                                    " kg")))
  expect_identical(.opt_cells(cp)[, 2L], c("55.1 kg", "70.5 kg", "170.2 kg"))
  mp <- as_rtftable(flextable::mk_par(
    ft(d), j = "N", value = flextable::as_paragraph(N, " pts")))
  expect_identical(.opt_cells(mp)[, 4L], c("30 pts", "30 pts", "29 pts"))
})

test_that("flextable: caption, footer lines, and no-op styling calls", {
  skip_if_not_installed("flextable")
  ft <- flextable::flextable
  d <- .opt_ft_df()

  rt <- as_rtftable(flextable::set_caption(ft(d), "Table 1: Demog"))
  expect_identical(attr(rt, "rtf_titles"), "Table 1: Demog")

  rt <- as_rtftable(flextable::add_footer_lines(
    ft(d), c("Note 1: abc", "Note 2: def")))
  expect_identical(attr(rt, "rtf_footnotes"), c("Note 1: abc", "Note 2: def"))

  # drawing and styling calls do not change the text that is read
  base <- .opt_cells(as_rtftable(ft(d)))
  styled <- flextable::bg(
    flextable::bold(flextable::theme_vanilla(ft(d)), part = "header"),
    bg = "grey", part = "header")
  expect_identical(.opt_cells(as_rtftable(styled)), base)
  hl <- flextable::hline(flextable::hline_top(
    ft(d), border = flextable::fp_border_default()), border = flextable::fp_border_default(width = 2))
  expect_identical(.opt_cells(as_rtftable(hl)), base)
  fx <- flextable::set_table_properties(ft(d), layout = "fixed", width = 0.8)
  expect_identical(.opt_cells(as_rtftable(fx)), base)
  af <- flextable::autofit(ft(d))
  expect_identical(.opt_cells(as_rtftable(af)), base)
  expect_identical(.opt_cells(as_rtftable(flextable::width(af, j = 1, width = 2))),
                   base)
  # a delete_part() of the footer leaves no footnotes
  nf <- as_rtftable(flextable::delete_part(
    flextable::add_footer_lines(ft(d), "z"), part = "footer"))
  expect_null(attr(nf, "rtf_footnotes"))
})

test_that("flextable: shape edge cases read to the right number of rows and columns", {
  skip_if_not_installed("flextable")
  ft <- flextable::flextable
  d <- .opt_ft_df()

  zr <- as_rtftable(ft(d[0, ]))                         # zero rows, header kept
  expect_identical(dim(zr$data), c(0L, 4L))
  expect_identical(unlist(zr$col_header), names(d))

  oc <- as_rtftable(ft(d["Param"]))                     # one column
  expect_identical(dim(oc$data), c(3L, 1L))
  expect_identical(unlist(oc$col_header), "Param")
  expect_identical(dim(as_rtftable(ft(data.frame(a = "z")))$data), c(1L, 1L))

  ks <- as_rtftable(ft(d, col_keys = c("Param", "N")))  # col_keys subset
  expect_identical(dim(ks$data), c(3L, 2L))
  expect_identical(unlist(ks$col_header), c("Param", "N"))
  bl <- as_rtftable(ft(d, col_keys = c("Param", "blank", "Mean")))
  expect_identical(dim(bl$data), c(3L, 3L))
  expect_identical(unlist(bl$col_header), c("Param", "", "Mean"))

  wide <- as_rtftable(ft(as.data.frame(matrix(1:40, 2, 20))))
  expect_identical(dim(wide$data), c(2L, 20L))

  # non-ASCII text is kept intact, in the body and in a relabelled header
  na <- data.frame(a = c("été αβ", "日本語",
                         "≤ 5 ± 2 µg"),
                   b = 1:3, stringsAsFactors = FALSE)
  rt <- as_rtftable(flextable::set_header_labels(ft(na), a = "α Über"))
  expect_identical(.opt_cells(rt)[, 1L], na$a)
  expect_identical(unlist(rt$col_header), c("α Über", "b"))

  # text that only looks like markup is data and stays as typed
  mk <- as_rtftable(ft(data.frame(a = c("<b>x</b>", "a & b", "1 < 2 > 0"),
                                  stringsAsFactors = FALSE)))
  expect_identical(.opt_cells(mk)[, 1L], c("<b>x</b>", "a & b", "1 < 2 > 0"))

  # alignment: strings left, numbers right, set alignment follows the column
  al <- as_rtftable(flextable::align(ft(d), j = 1, align = "center",
                                     part = "body"))
  expect_identical(vapply(al$col_spec, function(s) s$align, ""),
                   c("center", "right", "right", "right"))

  # grouped data: one group row per group, then its rows
  gd <- data.frame(g = c("A", "A", "B", "B"), x = c("p", "p", "q", "r"),
                   v = 1:4, stringsAsFactors = FALSE)
  grp <- as_rtftable(flextable::as_flextable(
    flextable::as_grouped_data(gd, groups = "g")))
  expect_identical(dim(grp$data), c(6L, 2L))
  expect_identical(.opt_cells(grp)[c(1L, 4L), 1L], c("g: A", "g: B"))
})

test_that("flextable: an option-built table renders to RTF with header, caption and footer", {
  skip_if_not_installed("flextable")
  d <- .opt_ft_df()
  x <- flextable::add_footer_lines(
    flextable::set_caption(
      flextable::set_header_labels(flextable::flextable(d), Param = "Parameter"),
      "Table 9: Invented"), "Source: invented data.")
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_tables(as_rtftables(x))
  out <- tempfile(fileext = ".rtf")
  on.exit(unlink(out), add = TRUE)
  expect_no_error(generate_rtfreport(doc, out, overwrite = TRUE))
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  for (s in c("Table 9: Invented", "Parameter", "Weight", "Source: invented data."))
    expect_true(grepl(s, txt, fixed = TRUE), info = s)
})


# ── huxtable option matrix ────────────────────────────────────────────────────
#
# Draft battery (#781).  Left out and tracked in the #781 report: body merges
# (colspan / rowspan repeat the text), add_footnote() (a repeated body row),
# header-less tables (V1.. names), markdown cells, and a non-leading
# header_rows() set.

.opt_hx <- function(add_colnames = TRUE) {
  huxtable::as_hux(.opt_ft_df(), add_colnames = add_colnames)
}

test_that("huxtable: number_format variants are read as displayed", {
  skip_if_not_installed("huxtable")
  hx <- .opt_hx()
  hx1 <- hx; huxtable::number_format(hx1) <- 1
  expect_identical(.opt_cells(as_rtftable(hx1))[, 2L],
                   c("55.1", "70.5", "170.2"))
  hx2 <- hx; huxtable::number_format(hx2)[, 2:3] <- 2
  expect_identical(.opt_cells(as_rtftable(hx2))[, 2:3],
                   cbind(c("55.12", "70.50", "170.25"),
                         c("10.50", "12.25", "8.00")))
  hx3 <- hx; huxtable::number_format(hx3)[, 2] <- "%.0f%%"
  expect_identical(.opt_cells(as_rtftable(hx3))[, 2L], c("55%", "70%", "170%"))
  hx4 <- hx; huxtable::number_format(hx4)[, 2] <-
    list(function(v) paste0("[", round(v), "]"))
  expect_identical(.opt_cells(as_rtftable(hx4))[, 2L],
                   c("[55]", "[70]", "[170]"))
  hx5 <- huxtable::set_number_format(hx, 2:4, 2:3, 3)
  expect_identical(.opt_cells(as_rtftable(hx5))[, 2L],
                   c("55.123", "70.500", "170.250"))
  # the header row is not touched by number_format
  expect_identical(unlist(as_rtftable(hx1)$col_header),
                   c("Param", "Mean", "SD", "N"))
})

test_that("huxtable: caption forms and header-row layouts", {
  skip_if_not_installed("huxtable")
  hx <- .opt_hx()
  expect_identical(attr(as_rtftable(huxtable::set_caption(hx, "Table 1: Demog")),
                        "rtf_titles"), "Table 1: Demog")
  h2 <- hx; huxtable::caption(h2) <- "Table 2: Other"
  expect_identical(attr(as_rtftable(h2), "rtf_titles"), "Table 2: Other")

  # a spanner row above the column names via insert_row() + colspan()
  sp <- huxtable::insert_row(hx, "", "Stats", "", "", after = 0)
  huxtable::colspan(sp)[1, 2] <- 3
  huxtable::header_rows(sp)[1:2] <- TRUE
  rt <- as_rtftable(sp)
  expect_identical(dim(rt$data), c(3L, 4L))
  expect_length(rt$col_header, 2L)
  expect_identical(vapply(rt$col_header[[1L]], function(s) s$label, ""),
                   c("", "Stats"))
  span <- rt$col_header[[1L]][[2L]]
  expect_identical(c(span$from, span$to), c(2L, 4L))
  expect_identical(unlist(rt$col_header[[2L]]), c("Param", "Mean", "SD", "N"))

  # rows added to the body keep their position
  top <- as_rtftable(huxtable::insert_row(hx, "X", "Y", "Z", "W", after = 2))
  expect_identical(dim(top$data), c(4L, 4L))
  expect_identical(.opt_cells(top)[1L, ], c("Age", "55.1", "10.5", "30"))
  expect_identical(.opt_cells(top)[2L, ], c("X", "Y", "Z", "W"))
  end <- as_rtftable(huxtable::insert_row(hx, "X", "Y", "Z", "W", after = 4))
  expect_identical(.opt_cells(end)[4L, ], c("X", "Y", "Z", "W"))
  ic <- as_rtftable(huxtable::insert_column(hx, c("a", "b", "c"), after = 1,
                                            colnames = "Newc"))
  expect_identical(dim(ic$data), c(3L, 5L))

  # a table built without column names has no header rows: a blank header
  nh <- as_rtftable(.opt_hx(add_colnames = FALSE))
  expect_identical(dim(nh$data), c(3L, 4L))
  expect_identical(unlist(nh$col_header), rep("", 4L))
})

test_that("huxtable: NA, na_string, and cell text that only looks like markup", {
  skip_if_not_installed("huxtable")
  df <- data.frame(a = c("x", NA), b = c(NA, 2.5), stringsAsFactors = FALSE)
  hx <- huxtable::huxtable(df, add_colnames = FALSE)
  expect_identical(.opt_cells(as_rtftable(hx)), cbind(c("x", ""), c("", "2.5")))
  huxtable::na_string(hx) <- "--"
  expect_identical(.opt_cells(as_rtftable(hx)), cbind(c("x", "--"), c("--", "2.5")))
  nn <- huxtable::set_na_string(
    huxtable::huxtable(b = c(1.5, NA, 3), add_colnames = FALSE), "n/a")
  expect_identical(.opt_cells(as_rtftable(nn))[, 1L], c("1.5", "n/a", "3"))

  # text with < > & stays as typed, whichever escape_contents is set
  txt <- c("<b>x</b>", "a & b", "1 < 2")
  for (esc in c(TRUE, FALSE)) {
    h <- huxtable::set_escape_contents(
      huxtable::huxtable(a = txt, add_colnames = FALSE), esc)
    expect_identical(.opt_cells(as_rtftable(h))[, 1L], txt, info = esc)
  }
  # a number_format function is applied, the result is not re-read as markup
  hf <- .opt_hx()
  huxtable::number_format(hf)[, 2] <- list(function(v) paste0("<", round(v), ">"))
  expect_identical(.opt_cells(as_rtftable(hf))[, 2L], c("<55>", "<70>", "<170>"))
})

test_that("huxtable: shape edge cases read to the right number of rows and columns", {
  skip_if_not_installed("huxtable")
  skip_if_not_installed("tibble")
  hx <- .opt_hx()

  zr <- as_rtftable(huxtable::as_hux(.opt_ft_df()[0, ]))   # columns, no rows
  expect_identical(dim(zr$data), c(0L, 4L))
  expect_identical(unlist(zr$col_header), c("Param", "Mean", "SD", "N"))
  zc <- as_rtftable(huxtable::huxtable(a = character(0), b = numeric(0)))
  expect_identical(dim(zc$data), c(0L, 2L))
  expect_identical(unlist(zc$col_header), c("a", "b"))

  oc <- as_rtftable(huxtable::as_hux(.opt_ft_df()["Param"]))
  expect_identical(dim(oc$data), c(3L, 1L))
  expect_identical(unlist(oc$col_header), "Param")
  one <- as_rtftable(huxtable::huxtable(a = "z", add_colnames = FALSE))
  expect_identical(dim(one$data), c(1L, 1L))

  wide <- as_rtftable(huxtable::as_hux(as.data.frame(matrix(1:40, 2, 20))))
  expect_identical(dim(wide$data), c(2L, 20L))

  # a row-subset huxtable keeps its header and the rows chosen
  sub <- as_rtftable(hx[c(1, 3, 2), ])
  expect_identical(.opt_cells(sub)[, 1L], c("Weight", "Age"))

  # tibble input, factor / Date / logical columns
  tb <- as_rtftable(huxtable::as_hux(tibble::tibble(a = c(1, 2), b = c("x", "y"))))
  expect_identical(dim(tb$data), c(2L, 2L))
  mixed <- as_rtftable(huxtable::as_hux(data.frame(
    f = factor(c("u", "v")), d = as.Date(c("2026-01-05", NA)),
    l = c(TRUE, NA))))
  expect_identical(.opt_cells(mixed)[1L, ], c("u", "2026-01-05", "TRUE"))
  expect_identical(.opt_cells(mixed)[2L, ], c("v", "", ""))

  na <- data.frame(a = c("été αβ", "日本語",
                         "≤ 5 ± 2 µg"),
                   b = 1:3, stringsAsFactors = FALSE)
  nr <- as_rtftable(huxtable::as_hux(na))
  expect_identical(.opt_cells(nr)[, 1L], na$a)

  # alignment: strings left, numbers (decimal "." alignment) right
  expect_identical(vapply(as_rtftable(hx)$col_spec, function(s) s$align, ""),
                   c("left", "right", "right", "right"))
})

test_that("huxtable: an option-built table renders to RTF with header and caption", {
  skip_if_not_installed("huxtable")
  hx <- huxtable::set_caption(.opt_hx(), "Table 9: Invented")
  huxtable::number_format(hx)[, 2] <- 1
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_tables(as_rtftables(hx))
  out <- tempfile(fileext = ".rtf")
  on.exit(unlink(out), add = TRUE)
  expect_no_error(generate_rtfreport(doc, out, overwrite = TRUE))
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  for (s in c("Table 9: Invented", "Param", "Weight", "70.5"))
    expect_true(grepl(s, txt, fixed = TRUE), info = s)
})


# ── flextable / huxtable batteries, read to the RTF (#781) ────────────────────
#
# Each case is built, read with as_rtftables(), written with
# generate_rtfreport(), and the RTF is checked for the value expected, for
# markup left in the text and for a literal NA.

# The pages as_rtftables() gives for `x`, and the RTF text written from them.
.opt_render <- function(x, ...) {
  pages <- expect_no_error(as_rtftables(x, ...))
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_tables(pages)
  out <- tempfile(fileext = ".rtf")
  on.exit(unlink(out), add = TRUE)
  expect_no_error(generate_rtfreport(doc, out, overwrite = TRUE))
  list(pages = pages,
       txt = paste(readLines(out, warn = FALSE), collapse = "\n"))
}

# No markup the adapters read is left in the RTF text, and no cell says NA.
.opt_expect_clean <- function(txt) {
  for (m in c("<br>", "<tab>", "&amp;", "&lt;", "&nbsp;", "**"))
    expect_false(grepl(m, txt, fixed = TRUE), info = m)
  expect_false(grepl("(^|[ }])NA(\\\\cell|\\\\line|\\\\par| |$)", txt),
               info = "literal NA")
  expect_false(grepl("V1", txt, fixed = TRUE), info = "V1 header")
}

.opt_in_rtf <- function(txt, ...) {
  for (s in c(...)) expect_true(grepl(s, txt, fixed = TRUE), info = s)
}

.opt_n_in_rtf <- function(txt, s) {
  m <- gregexpr(s, txt, fixed = TRUE)[[1L]]
  if (m[1L] < 0L) 0L else length(m)
}

test_that("FT-TB993 [FR-TB632]: flextable header labels reach the RTF as text, special characters escaped", {
  skip_if_not_installed("flextable")
  ft <- flextable::flextable
  d <- .opt_ft_df()
  x <- flextable::set_header_labels(
    ft(d), Param = "P & Q <1>", Mean = "Mean {kg} \\ 50%",
    SD = "α Ü", N = NA)
  r <- .opt_render(x)
  expect_identical(unlist(r$pages[[1L]]$col_header),
                   c("P & Q <1>", "Mean {kg} \\ 50%", "α Ü", ""))
  .opt_expect_clean(r$txt)
  .opt_in_rtf(r$txt, "P & Q <1>", "Mean \\{kg\\} \\\\ 50%", "\\u945?",
              "\\u220?", "Weight", "170.25")

  # separate_header(): the part of a column name before the separator is a
  # spanner over the columns that share it
  sh <- flextable::separate_header(
    ft(data.frame(a_x = 1, a_y = 2, b_x = 3)), split = "_")
  r <- .opt_render(sh)
  hd <- r$pages[[1L]]$col_header
  expect_length(hd, 2L)
  expect_identical(vapply(hd[[1L]], function(s) s$label, ""), c("a", "b"))
  expect_identical(unlist(hd[[2L]]), c("x", "y", "x"))
  .opt_expect_clean(r$txt)

  # add_header_lines(): one cell across the table above the labels
  hl <- flextable::add_header_lines(ft(d), "Line one")
  r <- .opt_render(hl)
  expect_length(r$pages[[1L]]$col_header, 2L)
  .opt_in_rtf(r$txt, "Line one", "Param")
  .opt_expect_clean(r$txt)
})

test_that("FT-TB994 [FR-TB632]: merged flextable cells reach the RTF once, with the values around them", {
  skip_if_not_installed("flextable")
  ft <- flextable::flextable
  # merge_h(): equal neighbours in a row
  h <- flextable::merge_h(ft(data.frame(a = c("x", "y"), b = c("x", "z"),
                                        c = c("x", "z"))))
  r <- .opt_render(h)
  expect_identical(.opt_cells(r$pages[[1L]]),
                   rbind(c("x", "", ""), c("y", "z", "")))
  .opt_expect_clean(r$txt)
  expect_identical(.opt_n_in_rtf(r$txt, "x\\cell"), 1L)

  # merge_v(): equal cells down a column; merge_none() undoes it
  v <- flextable::merge_v(ft(data.frame(a = c("x", "x", "y"), b = c(1, 2, 3))),
                          j = "a")
  r <- .opt_render(v)
  expect_identical(.opt_cells(r$pages[[1L]])[, 1L], c("x", "", "y"))
  expect_identical(.opt_n_in_rtf(r$txt, "x\\cell"), 1L)
  n0 <- .opt_render(flextable::merge_none(v))
  expect_identical(.opt_cells(n0$pages[[1L]])[, 1L], c("x", "x", "y"))

  # merge_at(): a block; the text read is the block's first cell
  d <- .opt_ft_df()
  b <- flextable::merge_at(ft(d), i = 1:2, j = 2:3)
  r <- .opt_render(b)
  m <- .opt_cells(r$pages[[1L]])
  expect_identical(m[1:2, 2:3], matrix(c("55.1234", "", "", ""), 2L, 2L))
  expect_identical(m[3L, ], c("Height", "170.2500", "8.00", "29"))
  .opt_expect_clean(r$txt)
  .opt_in_rtf(r$txt, "55.1234", "170.2500")

  # a merge in the header and in the footer
  hh <- flextable::merge_at(ft(d), i = 1, j = 1:2, part = "header")
  r <- .opt_render(hh)
  expect_identical(unlist(r$pages[[1L]]$col_header),
                   c("Param", "", "SD", "N"))
  fm <- flextable::merge_at(flextable::add_footer_lines(ft(d), "Footer text"),
                            i = 1, j = 1:4, part = "footer")
  r <- .opt_render(fm)
  expect_identical(attr(r$pages[[1L]], "rtf_footnotes"), "Footer text")
  expect_identical(.opt_n_in_rtf(r$txt, "Footer text"), 1L)
  .opt_expect_clean(r$txt)
})

test_that("FT-TB995 [FR-TB632]: flextable colformat_*() text and na_str reach the RTF as displayed", {
  skip_if_not_installed("flextable")
  ft <- flextable::flextable
  dn <- data.frame(n = c(1234.5678, NA, -0.5), i = c(1L, NA, 30L),
                   ch = c("a", NA, "c"), l = c(TRUE, NA, FALSE),
                   dt = as.Date(c("2026-01-05", NA, "2026-03-09")),
                   stringsAsFactors = FALSE)
  x <- ft(dn)
  x <- flextable::colformat_num(x, j = "n", big.mark = ",", prefix = "$",
                                suffix = " kg", na_str = "--")
  x <- flextable::colformat_int(x, j = "i", na_str = "missing")
  x <- flextable::colformat_char(x, j = "ch", prefix = ">", suffix = "<",
                                 na_str = "(none)")
  x <- flextable::colformat_lgl(x, j = "l", true = "Yes", false = "No",
                                na_str = "?")
  x <- flextable::colformat_date(x, j = "dt", fmt_date = "%d/%m/%Y",
                                 na_str = "UNK")
  r <- .opt_render(x)
  expect_identical(.opt_cells(r$pages[[1L]]),
                   cbind(c("$1,234.568 kg", "--", "$-0.500 kg"),
                         c("1", "missing", "30"),
                         c(">a<", "(none)", ">c<"),
                         c("Yes", "?", "No"),
                         c("05/01/2026", "UNK", "09/03/2026")))
  .opt_expect_clean(r$txt)
  .opt_in_rtf(r$txt, "$1,234.568 kg\\cell", "missing\\cell", "(none)\\cell",
              "Yes\\cell", "09/03/2026\\cell")

  # the default na_str is empty: a missing value prints nothing, not NA
  r <- .opt_render(ft(dn))
  expect_identical(.opt_cells(r$pages[[1L]])[2L, ], c("", "", "", "", ""))
  .opt_expect_clean(r$txt)

  # decimal.mark and big.mark, and a formatter set by set_formatter()
  dm <- flextable::colformat_double(ft(dn), j = "n", digits = 1,
                                    decimal.mark = ",", big.mark = " ")
  r <- .opt_render(dm)
  expect_identical(.opt_cells(r$pages[[1L]])[, 1L], c("1 234,6", "", "-0,5"))
  sf <- flextable::set_formatter(ft(.opt_ft_df()),
                                 Mean = function(x) sprintf("%.1f%%", x))
  r <- .opt_render(sf)
  expect_identical(.opt_cells(r$pages[[1L]])[, 2L],
                   c("55.1%", "70.5%", "170.2%"))
  .opt_in_rtf(r$txt, "55.1%\\cell")
})

test_that("FT-TB996 [FR-TB632]: flextable footers reach the RTF as footnote lines, a footnote() mark as text", {
  skip_if_not_installed("flextable")
  ft <- flextable::flextable
  d <- .opt_ft_df()

  # footer lines: one line each, an empty one dropped, markup-like text kept
  x <- flextable::add_footer_lines(ft(d), c("N1: abc", "", "N2: a & b <i>"))
  r <- .opt_render(x)
  expect_identical(attr(r$pages[[1L]], "rtf_footnotes"),
                   c("N1: abc", "N2: a & b <i>"))
  .opt_in_rtf(r$txt, "N1: abc", "N2: a & b <i>")
  expect_identical(.opt_n_in_rtf(r$txt, "N1: abc"), 1L)

  # a footer row of values and lines together, in flextable's order
  y <- flextable::add_footer_lines(
    flextable::add_footer_row(ft(d), values = c("L", "M", "R"),
                              colwidths = c(1, 2, 1)), "Last line")
  r <- .opt_render(y)
  expect_identical(attr(r$pages[[1L]], "rtf_footnotes"),
                   c("L M R", "Last line"))
  .opt_expect_clean(r$txt)

  # footnote(): the mark follows the cell text and leads the footer line,
  # both as plain text
  fn <- flextable::footnote(ft(d), i = 1, j = 2, ref_symbols = "a",
                            value = flextable::as_paragraph("Mean of two"),
                            part = "body")
  r <- .opt_render(fn)
  expect_identical(.opt_cells(r$pages[[1L]])[1L, 2L], "55.1234a")
  expect_identical(attr(r$pages[[1L]], "rtf_footnotes"), "aMean of two")
  .opt_in_rtf(r$txt, "55.1234a\\cell", "aMean of two")
  fh <- flextable::footnote(ft(d), i = 1, j = 2, ref_symbols = "*",
                            value = flextable::as_paragraph("Header note"),
                            part = "header")
  r <- .opt_render(fh)
  expect_identical(unlist(r$pages[[1L]]$col_header),
                   c("Param", "Mean*", "SD", "N"))
  .opt_expect_clean(r$txt)

  # deleting the footer leaves no footnote; read_meta without "footnotes" too
  nf <- flextable::delete_part(flextable::add_footer_lines(ft(d), "z"),
                               part = "footer")
  expect_null(attr(.opt_render(nf)$pages[[1L]], "rtf_footnotes"))
  kept <- flextable::add_footer_lines(ft(d), "z")
  expect_null(attr(as_rtftables(kept, read_meta = c("col_header"))[[1L]],
                   "rtf_footnotes"))
})

test_that("FT-TB997 [FR-TB632 FR-TB629]: a flextable tab is a tab in the cell and the caption, not <tab>", {
  skip_if_not_installed("flextable")
  ft <- flextable::flextable
  d <- .opt_ft_df()
  x <- flextable::compose(
    ft(d), i = 1, j = 1,
    value = flextable::as_paragraph("a\tb", "\n", "c"))
  x <- flextable::set_caption(
    x, flextable::as_paragraph("Cap\tone", "\n", "two"))
  r <- .opt_render(x)
  expect_identical(r$pages[[1L]]$data$V1[1L], "a\tb\nc")
  expect_identical(attr(r$pages[[1L]], "rtf_titles"), "Cap\tone\ntwo")
  .opt_expect_clean(r$txt)
  expect_false(grepl("<tab>", r$txt, fixed = TRUE))
})

test_that("FT-TB998 [FR-TB632]: a flextable with no rows is written as RTF with its header, caption and footer", {
  skip_if_not_installed("flextable")
  ft <- flextable::flextable
  d <- .opt_ft_df()
  x <- flextable::add_footer_lines(
    flextable::set_caption(
      flextable::set_header_labels(ft(d[0, ]), Param = "Parameter"),
      "Table 0: Empty"), "Note z")
  r <- .opt_render(x)
  expect_identical(nrow(r$pages[[1L]]$data), 0L)
  expect_identical(unlist(r$pages[[1L]]$col_header),
                   c("Parameter", "Mean", "SD", "N"))
  .opt_in_rtf(r$txt, "Parameter", "Table 0: Empty", "Note z")
  .opt_expect_clean(r$txt)

  # a spanning header over no rows, and no header at all
  sp <- flextable::add_header_row(ft(d[0, ]), values = c("", "S", "N"),
                                  colwidths = c(1, 2, 1))
  r <- .opt_render(sp)
  expect_length(r$pages[[1L]]$col_header, 2L)
  .opt_in_rtf(r$txt, "S")
  nh <- .opt_render(flextable::delete_part(ft(d[0, ]), part = "header"))
  expect_identical(unlist(nh$pages[[1L]]$col_header), rep("", 4L))
})

test_that("FT-TB999 [FR-TB633]: huxtable number_format reaches the RTF as displayed, a missing value empty", {
  skip_if_not_installed("huxtable")
  hx <- .opt_hx()
  h1 <- hx
  huxtable::number_format(h1)[, 2] <- "%.1f%%"
  r <- .opt_render(h1)
  expect_identical(.opt_cells(r$pages[[1L]])[, 2L],
                   c("55.1%", "70.5%", "170.2%"))
  .opt_in_rtf(r$txt, "55.1%", "170.2%")
  .opt_expect_clean(r$txt)

  h2 <- hx
  huxtable::number_format(h2)[, 2] <-
    list(function(v) format(v, big.mark = ",", nsmall = 1))
  expect_identical(.opt_cells(.opt_render(h2)$pages[[1L]])[, 2L],
                   c("55.1234", "70.5", "170.25"))

  hs <- huxtable::huxtable(
    a = c(1234567.891, -0.00001234, NA, 0), add_colnames = FALSE)
  huxtable::number_format(hs) <- "%.2e"
  r <- .opt_render(hs)
  expect_identical(.opt_cells(r$pages[[1L]])[, 1L],
                   c("1.23e+06", "-1.23e-05", "", "0.00e+00"))
  .opt_expect_clean(r$txt)

  # NA as the format leaves the number as it is; a character column that
  # looks like a number is not formatted
  hn <- huxtable::huxtable(a = c(1234567.891, 12.5, NA), add_colnames = FALSE)
  huxtable::number_format(hn) <- NA
  r <- .opt_render(hn)
  expect_identical(.opt_cells(r$pages[[1L]])[, 1L], c("1234567.891", "12.5", ""))
  .opt_expect_clean(r$txt)
  hc <- huxtable::huxtable(b = c("1.2345", NA), add_colnames = FALSE)
  r <- .opt_render(hc)
  expect_identical(.opt_cells(r$pages[[1L]])[, 1L], c("1.2345", ""))

  # the integer column and the header are as huxtable prints them
  h3 <- hx
  huxtable::number_format(h3) <- 2
  r <- .opt_render(h3)
  expect_identical(.opt_cells(r$pages[[1L]])[, 4L], c("30.00", "30.00", "29.00"))
  expect_identical(unlist(r$pages[[1L]]$col_header),
                   c("Param", "Mean", "SD", "N"))
})

test_that("FT-TB1000 [FR-TB633]: huxtable captions reach the RTF as the title, special characters escaped", {
  skip_if_not_installed("huxtable")
  hx <- .opt_hx()
  h <- huxtable::set_caption(hx, "Table 1: A & B <c> {d}")
  r <- .opt_render(h)
  expect_identical(attr(r$pages[[1L]], "rtf_titles"), "Table 1: A & B <c> {d}")
  .opt_in_rtf(r$txt, "Table 1: A & B <c> \\{d\\}")
  .opt_expect_clean(r$txt)

  # a caption of two lines is one title with a line break
  h2 <- huxtable::set_caption(hx, "Line1\nLine2")
  r <- .opt_render(h2)
  expect_identical(attr(r$pages[[1L]], "rtf_titles"), "Line1\nLine2")
  .opt_in_rtf(r$txt, "Line1")
  .opt_in_rtf(r$txt, "Line2")

  # no caption, an empty one and NA give no title
  for (cp in list(NA_character_, "")) {
    r <- .opt_render(huxtable::set_caption(hx, cp))
    expect_null(attr(r$pages[[1L]], "rtf_titles"))
    .opt_expect_clean(r$txt)
  }
  expect_null(attr(.opt_render(hx)$pages[[1L]], "rtf_titles"))

  # a caption at the bottom or with a label is still the title
  hb <- huxtable::set_label(huxtable::set_caption_pos(
    huxtable::set_caption(hx, "Cap"), "bottom"), "tab:x")
  expect_identical(attr(.opt_render(hb)$pages[[1L]], "rtf_titles"), "Cap")
})

test_that("FT-TB1001 [FR-TB633]: merged huxtable cells reach the RTF once, with the values around them", {
  skip_if_not_installed("huxtable")
  hx <- .opt_hx()
  # colspan() in a body row
  c1 <- hx
  huxtable::colspan(c1)[2, 1] <- 2
  r <- .opt_render(c1)
  expect_identical(.opt_cells(r$pages[[1L]])[1L, ], c("Age", "", "10.5", "30"))
  .opt_expect_clean(r$txt)
  # rowspan() down a column
  c2 <- hx
  huxtable::rowspan(c2)[2, 1] <- 2
  r <- .opt_render(c2)
  expect_identical(.opt_cells(r$pages[[1L]])[, 1L], c("Age", "", "Height"))
  expect_identical(.opt_n_in_rtf(r$txt, "Age\\cell"), 1L)
  # a block of both, and the merge_*() helpers
  c3 <- huxtable::merge_cells(hx, 2:3, 1:2)
  r <- .opt_render(c3)
  m <- .opt_cells(r$pages[[1L]])
  expect_identical(m[1:2, 1:2], matrix(c("Age", "", "", ""), 2L, 2L))
  expect_identical(m[3L, 1L], "Height")
  c4 <- huxtable::merge_across(hx, 2, 1:3)
  expect_identical(.opt_cells(.opt_render(c4)$pages[[1L]])[1L, ],
                   c("Age", "", "", "30"))
  c5 <- huxtable::merge_down(hx, 2:4, 1)
  expect_identical(.opt_cells(.opt_render(c5)$pages[[1L]])[, 1L],
                   c("Age", "", ""))

  # a spanner in the header keeps its text once; a leaf colspan blanks the rest
  c6 <- hx
  huxtable::colspan(c6)[1, 2] <- 3
  r <- .opt_render(c6)
  expect_identical(unlist(r$pages[[1L]]$col_header),
                   c("Param", "Mean", "", ""))
  .opt_in_rtf(r$txt, "Mean\\cell")
  expect_identical(.opt_n_in_rtf(r$txt, "SD"), 0L)
  .opt_expect_clean(r$txt)
})

test_that("FT-TB1002 [FR-TB633 FR-TB631]: only the header rows from the first row on are the huxtable column header", {
  skip_if_not_installed("huxtable")
  hx <- .opt_hx()
  # a body row flagged as a header row stays in the body
  h <- hx
  huxtable::header_rows(h)[3] <- TRUE
  r <- .opt_render(h)
  expect_identical(unlist(r$pages[[1L]]$col_header),
                   c("Param", "Mean", "SD", "N"))
  expect_identical(.opt_cells(r$pages[[1L]])[, 1L],
                   c("Age", "Weight", "Height"))
  .opt_expect_clean(r$txt)

  # no row flagged from the first row on: no header, the rows all body
  h0 <- hx
  huxtable::header_rows(h0) <- FALSE
  r <- .opt_render(h0)
  expect_identical(unlist(r$pages[[1L]]$col_header), rep("", 4L))
  expect_identical(.opt_cells(r$pages[[1L]])[, 1L],
                   c("Param", "Age", "Weight", "Height"))
  h1 <- hx
  huxtable::header_rows(h1) <- FALSE
  huxtable::header_rows(h1)[2] <- TRUE
  expect_identical(unlist(.opt_render(h1)$pages[[1L]]$col_header), rep("", 4L))

  # two flagged rows from the first row on: a spanner and the labels
  sp <- huxtable::insert_row(hx, "", "Stats", "", "", after = 0)
  huxtable::colspan(sp)[1, 2] <- 3
  huxtable::header_rows(sp)[1:2] <- TRUE
  r <- .opt_render(sp)
  expect_length(r$pages[[1L]]$col_header, 2L)
  expect_identical(nrow(r$pages[[1L]]$data), 3L)
})

test_that("FT-TB1003 [FR-TB633]: a huxtable with no rows is written as RTF with its header, caption and footnote", {
  skip_if_not_installed("huxtable")
  d <- .opt_ft_df()
  z <- huxtable::set_caption(huxtable::as_hux(d[0, ]), "Table 0: Empty")
  r <- .opt_render(z)
  expect_identical(dim(r$pages[[1L]]$data), c(0L, 4L))
  expect_identical(unlist(r$pages[[1L]]$col_header),
                   c("Param", "Mean", "SD", "N"))
  .opt_in_rtf(r$txt, "Table 0: Empty", "Param")
  .opt_expect_clean(r$txt)

  # no rows and no header: a blank header
  nh <- .opt_render(huxtable::as_hux(d[0, ], add_colnames = FALSE))
  expect_identical(unlist(nh$pages[[1L]]$col_header), rep("", 4L))

  # no rows and an add_footnote(): the footnote is the footnote, the body empty
  zf <- huxtable::add_footnote(huxtable::as_hux(d[0, ]), "Note z")
  r <- .opt_render(zf)
  expect_identical(nrow(r$pages[[1L]]$data), 0L)
  expect_identical(attr(r$pages[[1L]], "rtf_footnotes"), "Note z")
  expect_identical(.opt_n_in_rtf(r$txt, "Note z"), 1L)
  .opt_expect_clean(r$txt)
})

test_that("FT-TB1004 [FR-TB630]: huxtable add_footnote() rows are the page's footnote lines, not body rows", {
  skip_if_not_installed("huxtable")
  hx <- .opt_hx()
  one <- huxtable::add_footnote(hx, "Source: invented & <ok>.")
  r <- .opt_render(one)
  expect_identical(nrow(r$pages[[1L]]$data), 3L)
  expect_identical(attr(r$pages[[1L]], "rtf_footnotes"),
                   "Source: invented & <ok>.")
  expect_identical(.opt_n_in_rtf(r$txt, "Source: invented & <ok>."), 1L)
  .opt_expect_clean(r$txt)

  # several, in the order added; border = NULL and a caption change nothing
  two <- huxtable::add_footnote(
    huxtable::add_footnote(huxtable::set_caption(hx, "Cap"), "N1"),
    "N2", border = NULL)
  r <- .opt_render(two)
  expect_identical(nrow(r$pages[[1L]]$data), 3L)
  expect_identical(attr(r$pages[[1L]], "rtf_footnotes"), c("N1", "N2"))
  expect_identical(attr(r$pages[[1L]], "rtf_titles"), "Cap")

  # the token: left out, the row stays in the body, as with read_meta = FALSE
  tk <- as_rtftables(one, read_meta = c("col_header", "titles"))[[1L]]
  expect_identical(nrow(tk$data), 4L)
  expect_null(attr(tk, "rtf_footnotes"))
  expect_identical(nrow(as_rtftables(one, read_meta = FALSE)[[1L]]$data), 4L)
  expect_identical(
    attr(as_rtftables(one, read_meta = "footnotes")[[1L]], "rtf_footnotes"),
    "Source: invented & <ok>.")

  # a last row that is not a footnote stays in the body: values beyond the
  # first cell, a first cell that does not span the table, an empty text
  tot <- huxtable::add_rows(hx, c("Total", "1", "2", "3"), after = 4)
  expect_identical(nrow(as_rtftables(tot)[[1L]]$data), 4L)
  expect_null(attr(as_rtftables(tot)[[1L]], "rtf_footnotes"))
  sp2 <- huxtable::add_rows(hx, c("Total", "", "", ""), after = 4)
  huxtable::colspan(sp2)[5, 1] <- 2
  expect_identical(nrow(as_rtftables(sp2)[[1L]]$data), 4L)
  blank <- huxtable::add_footnote(hx, "")
  expect_identical(nrow(as_rtftables(blank)[[1L]]$data), 4L)
  # one column: the shape cannot tell a footnote from a row, so it is a row
  oc <- huxtable::add_footnote(huxtable::as_hux(.opt_ft_df()["Param"]), "Note")
  expect_identical(nrow(as_rtftables(oc)[[1L]]$data), 4L)
  # a spanning body row in the middle is a body row
  mid <- huxtable::add_rows(hx, c("Group", "", "", ""), after = 1)
  huxtable::header_rows(mid)[2] <- FALSE
  huxtable::colspan(mid)[2, 1] <- 4
  expect_identical(nrow(as_rtftables(mid)[[1L]]$data), 4L)
  expect_null(attr(as_rtftables(mid)[[1L]], "rtf_footnotes"))
})

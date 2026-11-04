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
  skip_if_not_installed("officer")
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
    ft(d), border = officer::fp_border()), border = officer::fp_border(width = 2))
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

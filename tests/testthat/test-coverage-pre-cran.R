# Tests that raise coverage before the CRAN submission (#699).  Each block
# takes one of the exported functions that covr reported as least covered and
# pins the branches no other file reached: empty input, NA, a table of several
# data frames, a page vector, the arguments refused.  No network, no random
# numbers, no LibreOffice.

# ── borders: the deprecated rtf_border_with() and rtf_table_border() ───────

test_that("rtf_border_with() replaces only the sides it is given", {
  silence_deprecation("rtf_border_with")
  b <- rtf_border(top = TRUE, bottom = TRUE)
  got <- rtf_border_with(b, bottom = rtf_border_line(color = "#003366"))
  expect_s3_class(got, "rtf_border")
  expect_identical(got$top, b$top)
  expect_identical(got$bottom$color, "#003366")

  # every side at once, on a NULL border (an empty one)
  all6 <- rtf_border_with(NULL, top = TRUE, bottom = TRUE, left = TRUE,
                          right = TRUE, inside_h = TRUE, inside_v = TRUE)
  for (s in c("top", "bottom", "left", "right", "inside_h", "inside_v")) {
    expect_s3_class(all6[[s]], "rtf_border_line")
  }
  expect_error(rtf_border_with("single", top = TRUE),
               "`border` must be NULL or an rtf_border object")
})

test_that("print.rtf_table_border() shows the outer and inside rules", {
  silence_deprecation("rtf_table_border")
  tb <- rtf_table_border(header = rtf_border(top = TRUE),
                         outer = rtf_border(top = TRUE),
                         inside_h = rtf_border_line(),
                         inside_v = rtf_border_line(width = 30L))
  out <- capture.output(res <- print(tb))
  expect_identical(res, tb)
  expect_identical(out[1], "<rtf_table_border>")
  expect_true(any(grepl("^  header    : T=single/15 B=none", out)))
  expect_true(any(grepl("^  body      : none$", out)))
  expect_true(any(out == "  outer     : <rtf_border>"))
  expect_true(any(out == "  inside_h  : single, 15 twips"))
  expect_true(any(out == "  inside_v  : single, 30 twips"))
})

# ── cell helpers: cell_rows(), fmt_right_align(), fmt_count_pct() ──────────

test_that("cell_rows() keeps unnamed rows and refuses an empty call", {
  r <- cell_rows("{n}", "{p}")
  expect_s3_class(r, "cell_rows")
  expect_identical(names(r), c("", ""))
  expect_identical(names(cell_rows(a = "{n}", "{p}")), c("a", ""))
  expect_error(cell_rows(), "needs at least one row")
})

test_that("fmt_right_align() returns empty and all-blank input unchanged", {
  expect_identical(fmt_right_align(character(0)), character(0))
  # NA becomes `na`; nothing left to pad, so no padding is added
  expect_identical(fmt_right_align(c("", " ", NA)), c("", " ", ""))
  expect_identical(fmt_right_align(c(NA, NA), na = ""), c("", ""))
  # one value: nothing to align against
  expect_identical(fmt_right_align("12"), "12")
  expect_identical(fmt_right_align(c("5", "120"), nbsp = " "), c("  5", "120"))
})

test_that("fmt_count_pct() warns once when a fraction is above 1", {
  st <- rtfreporter:::.deprecation_state
  key <- "warn:fmt_count_pct_fraction"
  old <- st[[key]]
  on.exit(assign(key, old, envir = st), add = TRUE)
  if (exists(key, envir = st, inherits = FALSE)) rm(list = key, envir = st)

  expect_warning(got <- fmt_count_pct(3, 5), "values above 1 but `pct_unit`")
  expect_match(got, "3.*\\(500\\)")
  # said once a session
  expect_no_warning(fmt_count_pct(3, 5))
  # NA / Inf above 1 do not count as a mistaken fraction
  rm(list = key, envir = st)
  expect_no_warning(fmt_count_pct(c(1, 2), c(NA, 0.5)))
})

# ── listing_disp_width() ────────────────────────────────────────────────────

test_that("listing_disp_width() takes NA as empty and non-character input", {
  got <- listing_disp_width(c("ab", NA, ""))
  expect_type(got, "integer")
  expect_identical(got, c(2L, 0L, 0L))
  expect_identical(listing_disp_width(c(12, NA)), c(2L, 0L))
  expect_identical(listing_disp_width(factor("abc")), 3L)
  expect_identical(listing_disp_width(character(0)), integer(0))
})

# ── style verbs on a table of several data frames ──────────────────────────

pc_multi <- function() {
  set_col_header(
    rtftable(list(data.frame(a = "x", b = "1"), data.frame(a = "y", b = "2"))),
    rtf_col_header(list(col_cell(1L, "A"), col_cell(2L, "B"))))
}

test_that("style_header() restyles the header of every data frame (#699)", {
  m <- pc_multi()
  expect_length(m$col_header_list, 2L)
  s <- style_header(m, label = "Bee", cols = 2)
  lab <- function(h) vapply(h[[1L]], function(z) z$label, "")
  expect_identical(lab(s$col_header), c("A", "Bee"))
  # the second data frame's header had kept "B" before the fix
  expect_identical(lab(s$col_header_list[[1L]]), c("A", "Bee"))
  expect_identical(lab(s$col_header_list[[2L]]), c("A", "Bee"))
  rtf <- .render_to_string(.test_doc_with(s))
  expect_identical(lengths(regmatches(rtf, gregexpr("Bee", rtf))), 2L)

  # a frame given its own header keeps it
  own <- m
  own$col_header_list[[2L]] <- list(c("C", "D"))
  s2 <- style_header(own, label = "Bee", cols = 2)
  expect_identical(s2$col_header_list[[2L]], list(c("C", "D")))
})

test_that("set_header_cell() needs a header and keeps the first frame's in step", {
  silence_deprecation("set_header_cell")
  expect_error(set_header_cell(rtftable(data.frame(a = 1)), col_cell(1, "x"),
                               row = 1),
               "has no column header; use set_col_header\\(\\) first")
  h <- set_header_cell(pc_multi(), col_cell(1:2, "Span"), row = 1)
  expect_identical(h$col_header_list[[1L]], h$col_header)
  expect_identical(h$col_header[[1L]][[1L]]$label, "Span")
})

test_that("set_col_header(values = ) fills a single table's tokens", {
  t1 <- rtftable(data.frame(a = c("x", "y"), b = c("1", "2")))
  expect_message(
    got <- set_col_header(t1, rtf_col_header(c("A", "N={n}")),
                          values = data.frame(group = NA, n = 5)),
    "no group axis")
  expect_identical(got$col_header[[1L]], c("A", "N=5"))
  expect_error(set_col_header(t1, c("A", "B"), values = list(n = 5)),
               "`values` must be a data.frame")
})

test_that("collapse_repeats() blanks repeats within each data frame", {
  m <- rtftable(list(data.frame(a = c("x", "x"), b = c("1", "2")),
                     data.frame(a = c("y", "y"), b = c("3", "4"))))
  got <- collapse_repeats(m, "a")
  expect_identical(got$data_list[[1L]]$a, c("x", NA))
  expect_identical(got$data_list[[2L]]$a, c("y", NA))
  # the second frame starts afresh: its first row is shown
  expect_null(got$data)
})

# ── rtf_section(): no section, a page vector, refused shapes ───────────────

test_that("rtf_section() stores an empty section, or one for several pages", {
  d <- rtf_document()
  one <- rtf_section(d, page = 1)
  expect_identical(one$sections[["1"]], list(header = NULL, footer = NULL))

  both <- rtf_section(d, page = c(1, 2),
                      secinfo = list(header = "H", footer = "F"))
  expect_identical(names(both$sections), c("1", "2"))
  expect_identical(both$sections[["2"]], list(header = "H", footer = "F"))

  expect_error(rtf_section(d, page = 1:2, secinfo = "x"),
               "secinfo must be a list of section objects")
  expect_error(rtf_section(d, page = 1:2,
                           secinfo = list(list(header = "a"),
                                          list(header = "b"), list())),
               "Length of page \\(2\\) must match length of secinfo \\(3\\)")
  expect_error(rtf_section(d, page = 1, header = "H",
                           secinfo = list(header = "H")),
               "not both")
})

# ── assemble_toc() (deprecated) ─────────────────────────────────────────────

test_that("assemble_toc() needs files or a spec", {
  silence_deprecation("assemble_toc")
  expect_error(assemble_toc(), "Supply `files` or `spec`")
})

# ── table_plan verbs: plan_cells(), plan_empty(), plan_layers() ────────────

# One plan, built once from a small ADSL (eight subjects an arm), keeps these
# tests fast.  With cardx (and broom) a Kruskal-Wallis p-value is added for
# plan_compare().
pc_plan_cache <- new.env(parent = emptyenv())
pc_has_test <- function() {
  requireNamespace("cardx", quietly = TRUE) &&
    requireNamespace("broom", quietly = TRUE) &&
    utils::packageVersion("broom") >= "1.0.8"
}
pc_plan <- function() {
  if (is.null(pc_plan_cache$p)) {
    adsl <- cards::ADSL
    adsl$TRT <- as.character(adsl$ARM)
    adsl <- do.call(rbind, lapply(split(adsl, adsl$TRT), utils::head, 8L))
    ard <- cards::ard_stack(
      adsl, .by = TRT,
      cards::ard_summary(
        variables = AGE,
        statistic = ~ cards::continuous_summary_fns(c("N", "mean", "sd"))))
    if (pc_has_test()) {
      ard <- cards::bind_ard(
        ard, cardx::ard_stats_kruskal_test(adsl, by = TRT, variables = AGE))
    }
    pc_plan_cache$p <- table_plan(suppressMessages(normalize_ard(ard)),
                                  cols = "TRT", rows = c(group = "variable"))
  }
  pc_plan_cache$p
}

test_that("plan_cells() and plan_empty() check their options", {
  skip_if_not_installed("cards")
  p <- pc_plan()
  expect_identical(plan_cells(p), p)        # nothing declared, nothing added
  expect_s3_class(plan_cells(p, value = "stat"), "table_plan")
  expect_error(plan_cells(p, value = "bad"), "should be one of")
  expect_error(plan_cells(p, notes = NA), "notes")
  expect_error(plan_empty(p, footnote = 1),
               "plan_empty\\(footnote = \\) is the footnote's text, or NULL")
  expect_error(plan_empty(p, footnote = character(0)), "footnote")
  expect_error(plan_empty(p, text = c("a", "b")), "is one string, or NULL")
})

test_that("plan_layers() reads a single template and the compare columns", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom", minimum_version = "1.0.8")
  p <- pc_plan() |>
    plan_cells("{mean:.1f}", notes = FALSE) |>
    plan_compare(p = "{p.value:.3f}", diff = c(AGE = "{p.value:.2f}"))
  lay <- suppressMessages(plan_layers(p))
  cl <- lay$cells
  expect_true(length(cl) >= 1L)
  expect_true(is.na(cl[[1L]]$variable))
  expect_true(is.na(cl[[1L]]$context))

  cmp <- lay$compare
  expect_identical(vapply(cmp$cols, `[[`, "", "column"), c("p", "diff"))
  expect_identical(vapply(cmp$cols, `[[`, "", "context"), c(NA, "AGE"))
  expect_identical(cmp$position, "last")
  expect_null(cmp$by)
})

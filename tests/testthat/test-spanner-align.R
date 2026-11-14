# A spanner read by as_rtftables() is aligned as its source prints it
# (#1023): gt / gtsummary centre it unless tab_style() sets it, rtables
# centres it, flextable and huxtable take their own cell alignment.

# Write the pages and return the RTF text.
.t74_rtf <- function(pages) {
  d <- tempfile("t74-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- file.path(d, "sp.rtf")
  generate_rtfreport(rtf_tables(rtf_document(), pages), f, overwrite = TRUE)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

# The \q[lcr] written before a header cell's text: "l", "c" or "r".
.t74_q <- function(rtf, label) {
  pat <- sprintf("\\\\q([lcr])\\\\li[0-9]+\\\\ri[0-9]+ %s\\\\cell", label)
  m <- regmatches(rtf, regexpr(pat, rtf))
  if (!length(m)) return(NA_character_)
  sub("^\\\\q([lcr]).*$", "\\1", m)
}

.t74_df <- data.frame(subj = c("S01", "S02"), age = c(34, 45), wt = c(70.1, 80.2))

test_that("FT-TB7401 [FR-TB635]: a gt spanner over numeric (right-aligned) columns is centred, as gt prints it", {
  skip_if_not_installed("gt")
  g <- gt::gt(.t74_df) |> gt::tab_spanner("Demography", columns = c("age", "wt"))
  p <- as_rtftables(g)
  sp <- Filter(function(cl) identical(cl$label, "Demography"), p[[1L]]$col_header[[1L]])
  expect_identical(sp[[1L]]$align, "center")
  rtf <- .t74_rtf(p)
  expect_identical(.t74_q(rtf, "Demography"), "c")
  # the columns under it stay right-aligned
  expect_identical(.t74_q(rtf, "age"), "r")
  expect_identical(.t74_q(rtf, "wt"), "r")
})

test_that("FT-TB7402 [FR-TB635 FR-TB578]: a tab_style() alignment on a gt spanner is followed; without one, or without the \"styles\" token, it is centred", {
  skip_if_not_installed("gt")
  base <- gt::gt(.t74_df) |>
    gt::tab_spanner("Left L", columns = "subj", id = "l") |>
    gt::tab_spanner("Right R", columns = "age", id = "r") |>
    gt::tab_spanner("Plain P", columns = "wt", id = "p")
  g <- base |>
    gt::tab_style(gt::cell_text(align = "left"),
                  locations = gt::cells_column_spanners("l")) |>
    gt::tab_style(gt::cell_text(align = "right"),
                  locations = gt::cells_column_spanners("r")) |>
    gt::tab_style(gt::cell_text(weight = "bold"),
                  locations = gt::cells_column_spanners("p"))
  rtf <- .t74_rtf(as_rtftables(g))
  # over a left-aligned column, set right; over a right-aligned one, set left
  expect_identical(.t74_q(rtf, "Left L"), "l")
  expect_identical(.t74_q(rtf, "Right R"), "r")
  # a style with no alignment: centred (over a right-aligned column)
  expect_identical(.t74_q(rtf, "\\\\b Plain P\\\\b0 "), "c")
  # without the "styles" token nothing is read from tab_style(): all centred
  q <- .t74_rtf(as_rtftables(g, read_meta = c("col_header", "alignment", "spanning")))
  expect_identical(vapply(c("Left L", "Right R", "Plain P"), function(l) .t74_q(q, l), ""),
                   c("Left L" = "c", "Right R" = "c", "Plain P" = "c"))
  # gt itself: the spanners are centred unless set
  html <- gt::as_raw_html(base, inline_css = TRUE)
  expect_match(html, "id=\"p\"[^>]*text-align: center|text-align: center[^>]*id=\"p\"")
})

test_that("FT-TB7403 [FR-TB635]: a gtsummary spanning header is centred, over right-aligned columns too", {
  skip_if_not_installed("gt")
  skip_if_not_installed("gtsummary")
  d <- data.frame(trt = rep(c("A", "B"), 5), age = c(41, 52, 38, 60, 47, 55, 33, 49, 58, 44))
  ts <- gtsummary::tbl_summary(d, by = "trt", include = "age") |>
    gtsummary::modify_spanning_header(gtsummary::all_stat_cols() ~ "Arm X") |>
    gtsummary::modify_column_alignment(gtsummary::all_stat_cols(), "right")
  p <- as_rtftables(ts)
  rtf <- .t74_rtf(p)
  expect_identical(.t74_q(rtf, "Arm X"), "c")
  expect_identical(p[[1L]]$col_spec[[2L]]$align, "right")
})

test_that("FT-TB7404 [FR-TB635]: a tfrmt spanner is centred, with or without the \"styles\" token", {
  skip_if_not_installed("gt")
  skip_if_not_installed("tfrmt")
  dd <- data.frame(group = "G", label = rep(c("n", "Mean"), 2), span = "Arms Q",
                   column = rep(c("A", "B"), each = 2),
                   param = rep(c("n", "mean"), 2), value = c(10, 2.5, 12, 3.1))
  tf <- tfrmt::tfrmt(group = group, label = label, column = c(span, column),
                     param = param, value = value,
                     body_plan = tfrmt::body_plan(tfrmt::frmt_structure(
                       group_val = ".default", label_val = ".default", tfrmt::frmt("xx.x"))))
  g <- tfrmt::print_to_gt(tf, dd)
  expect_identical(.t74_q(.t74_rtf(as_rtftables(g)), "Arms Q"), "c")
  q <- as_rtftables(g, read_meta = c("col_header", "alignment", "spanning"))
  expect_identical(.t74_q(.t74_rtf(q), "Arms Q"), "c")
})

test_that("FT-TB7405 [FR-TB636]: an rtables spanner is centred, as rtables prints it, over right-aligned columns too", {
  skip_if_not_installed("rtables")
  d <- data.frame(grp = rep(c("X", "Y"), each = 4), trt = rep(c("A", "B"), 4),
                  age = c(41, 52, 38, 60, 47, 55, 33, 49))
  lyt <- rtables::basic_table() |> rtables::split_cols_by("grp") |>
    rtables::split_cols_by("trt") |>
    rtables::analyze("age", afun = function(x) {
      rtables::in_rows("Mean" = rtables::rcell(mean(x), format = "xx.x", align = "right"))
    })
  rt <- rtables::build_table(lyt, d)
  mf <- formatters::matrix_form(rt)
  expect_identical(unname(formatters::mf_aligns(mf)[1L, 2L]), "center")
  p <- as_rtftables(rt)
  expect_identical(p[[1L]]$col_spec[[2L]]$align, "right")
  rtf <- .t74_rtf(p)
  expect_identical(.t74_q(rtf, "X"), "c")
  expect_identical(.t74_q(rtf, "Y"), "c")
})

test_that("FT-TB7406 [FR-TB637]: a flextable spanner takes its first header cell's alignment", {
  skip_if_not_installed("flextable")
  ft <- flextable::flextable(.t74_df) |>
    flextable::add_header_row(values = c("", "Demo D"), colwidths = c(1, 2))
  # flextable's own: right over numeric columns
  expect_identical(.t74_q(.t74_rtf(as_rtftables(ft)), "Demo D"), "r")
  ctr <- flextable::align(ft, i = 1, j = 2:3, align = "center", part = "header")
  expect_identical(.t74_q(.t74_rtf(as_rtftables(ctr)), "Demo D"), "c")
  lft <- flextable::align(ft, i = 1, j = 2:3, align = "left", part = "header")
  rtf <- .t74_rtf(as_rtftables(lft))
  expect_identical(.t74_q(rtf, "Demo D"), "l")
  # the leaf labels keep their own alignment
  expect_identical(.t74_q(rtf, "age"), "r")
})

test_that("FT-TB7407 [FR-TB638]: a huxtable spanner takes its first cell's align(), \".\" as right", {
  skip_if_not_installed("huxtable")
  hx <- huxtable::hux(.t74_df, add_colnames = TRUE)
  hx <- huxtable::insert_row(hx, "", "Demo H", "", after = 0)
  huxtable::colspan(hx)[1, 2] <- 2
  huxtable::header_rows(hx)[1:2] <- TRUE
  # huxtable's own: left (its CSS and LaTeX print the cell left)
  expect_identical(as.character(huxtable::align(hx)[1, 2]), "left")
  rtf <- .t74_rtf(as_rtftables(hx))
  expect_identical(.t74_q(rtf, "Demo H"), "l")
  expect_identical(.t74_q(rtf, "age"), "r")
  huxtable::align(hx)[1, 2] <- "center"
  expect_identical(.t74_q(.t74_rtf(as_rtftables(hx)), "Demo H"), "c")
  huxtable::align(hx)[1, 2] <- "."
  expect_identical(.t74_q(.t74_rtf(as_rtftables(hx)), "Demo H"), "r")
})

test_that("FT-TB7408 [FR-TB527]: a spanner written with col_cell() still takes its leftmost column's header alignment", {
  tbl <- rtftable(.t74_df, col_header = list(
    list(col_cell(1, ""), col_cell(c(2, 3), "Own O")),
    c("subj", "age", "wt")),
    col_spec = list(list(col = 2, align = "right"), list(col = 3, align = "right")))
  rtf <- .t74_rtf(list(tbl))
  expect_identical(.t74_q(rtf, "Own O"), "r")
  tbl2 <- rtftable(.t74_df, col_header = list(
    list(col_cell(1, ""), col_cell(c(2, 3), "Own O", align = "center")),
    c("subj", "age", "wt")),
    col_spec = list(list(col = 2, align = "right"), list(col = 3, align = "right")))
  expect_identical(.t74_q(.t74_rtf(list(tbl2)), "Own O"), "c")
})

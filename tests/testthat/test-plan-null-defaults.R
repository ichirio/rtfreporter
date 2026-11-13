# A verb records only the arguments its call gives.  An argument left out
# is NULL and is not written into the layer, so a later call of the same
# verb keeps what an earlier one set; the documented default applies where
# the layers are resolved, when no call gave it.  Small fictional frames.

nd_wide <- function() {
  data.frame(
    grp = c("A", "A", "B", "B"),
    label = c("n", "Mean", "n", "Mean"),
    X = c("1", "2.5", "3", "4.5"),
    Y = c("7", "8.5", "9", "10.5"),
    stringsAsFactors = FALSE)
}

nd_ard <- function() {
  data.frame(
    ARM = rep(c("Placebo", "Active"), each = 3),
    variable = rep(c("AGE", "SEX", "SEX"), 2),
    .label = rep(c(NA, "F", "M"), 2),
    .kind = rep(c("continuous", "categorical", "categorical"), 2),
    stat_name = rep(c("mean", "n", "n"), 2),
    stat = c(61.2, 12, 8, 63.5, 9, 11),
    stringsAsFactors = FALSE)
}

nd_plan <- function() {
  table_plan(nd_ard(), cols = "ARM", rows = c(group = "variable")) |>
    plan_cells(continuous = c(Mean = "{mean:.1f}"), categorical = "{n}") |>
    plan_labels(AGE = "Age (years)", SEX = "Sex")
}

# the long frame with overall rows (no TRT)
nd_total <- function() {
  d <- data.frame(
    TRT = rep(c("A", "B"), each = 2), PARAM = "ALT",
    stat_name = rep(c("n", "mean"), 2), stat = c(20, 31.2, 18, 33.1),
    stringsAsFactors = FALSE)
  o <- d[d$TRT == "A", ]
  o$TRT <- NA
  o$stat <- o$stat * 10
  table_plan(rbind(d, o), cols = "TRT", rows = c(param = "PARAM"),
             label = c(row = "stat_name")) |>
    plan_cells(c(n = "{n:.0f}", Mean = "{mean:.1f}"), notes = FALSE)
}

# an AE table with nothing in it but its overall row (0 in each arm)
nd_empty <- function() {
  d <- data.frame(TRT = c("A", "B"), soc = NA, .label = "Any TEAE",
                  stat_name = "n", stat = 0, .overall = TRUE,
                  .kind = "categorical", stringsAsFactors = FALSE)
  table_plan(d, cols = "TRT", rows = c(soc = "soc")) |>
    plan_cells("{n:.0f}", notes = FALSE)
}

nd_tab <- function(p) as.data.frame(suppressMessages(plan_apply(p, "table")))
nd_pages <- function(p) suppressMessages(plan_apply(p, "pages"))
nd_first <- function(x) if (inherits(x, "rtftable")) x else x[[1L]]

# the RTF one plan writes, as text
nd_rtf <- function(p) {
  dir <- tempfile("nulldef")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  f <- file.path(dir, "t.rtf")
  pages <- nd_pages(p)
  if (inherits(pages, "rtftable")) pages <- list(pages)
  generate_rtfreport(rtf_tables(rtf_document(), pages), f, overwrite = TRUE)
  readLines(f, warn = FALSE)
}

test_that("plan_stub(before = TRUE) survives a later plan_stub() that leaves it out", {
  p <- nd_plan()
  one <- p |> plan_stub(name = "C", before = TRUE) |> plan_style()
  two <- p |> plan_stub(before = TRUE) |> plan_stub(name = "C") |> plan_style()
  expect_identical(nd_pages(two), nd_pages(one))
  # folded first: the stub is a column of the table before the pages
  expect_identical(names(nd_first(nd_pages(two))$data)[1L], "C")
  # the layer records only what the call says
  l <- (p |> plan_stub(name = "C"))$layers
  expect_identical(l[[length(l)]]$fields, list(name = "C"))
  # an explicit FALSE still resets an earlier TRUE
  back <- p |> plan_stub(before = TRUE) |> plan_stub(name = "C", before = FALSE) |>
    plan_style()
  expect_identical(nd_pages(back), nd_pages(p |> plan_stub(name = "C") |> plan_style()))
})

test_that("plan_stub() left out reads before = FALSE; one call writes what it did", {
  p <- nd_plan()
  expect_identical(nd_pages(p |> plan_stub(name = "C") |> plan_style()),
                   nd_pages(p |> plan_stub(name = "C", before = FALSE) |> plan_style()))
  expect_identical(nd_rtf(p |> plan_stub() |> plan_style()),
                   nd_rtf(p |> plan_stub(before = FALSE) |> plan_style()))
  # plan_stub() with nothing in it still folds the stub
  expect_identical(nd_pages(p |> plan_stub() |> plan_style()),
                   nd_pages(p |> plan_stub(before = FALSE) |> plan_style()))
  expect_identical(plan_layers(p |> plan_stub(name = "C") |> plan_style())$layers$stub,
                   list(name = "C", before = FALSE))
  expect_error(plan_stub(p, before = NA), "plan_stub(before = ) is TRUE", fixed = TRUE)
  expect_error(plan_stub(p, before = "yes"), "plan_stub(before = ) is TRUE", fixed = TRUE)
})

test_that("plan_total()'s label, position and direction survive a later call that leaves them out", {
  p <- nd_total()
  expect_identical(names(nd_tab(p |> plan_total(label = "All") |> plan_total(position = "first"))),
                   c("param", "row", "All", "A", "B"))
  expect_identical(names(nd_tab(p |> plan_total(position = "first") |> plan_total(label = "All"))),
                   c("param", "row", "All", "A", "B"))
  # an explicit value still wins, the default's own value included
  expect_identical(names(nd_tab(p |> plan_total(position = "first") |>
                                  plan_total(position = "last"))),
                   c("param", "row", "A", "B", "Total"))
  expect_identical(.plan_resolve(p |> plan_total(direction = "rows") |>
                                   plan_total(label = "All"), "total")$direction, "rows")
  expect_identical(.plan_resolve(p |> plan_total(direction = "rows") |>
                                   plan_total(direction = "cols"), "total")$direction, "cols")
})

test_that("plan_total() left out is label \"Total\", position \"last\", direction \"cols\"", {
  p <- nd_total()
  expect_identical(nd_tab(p |> plan_total()),
                   nd_tab(p |> plan_total(label = "Total", position = "last", direction = "cols")))
  expect_identical(names(nd_tab(p |> plan_total())), c("param", "row", "A", "B", "Total"))
  expect_identical(plan_layers(p |> plan_total())$layers$total,
                   list(label = "Total", position = "last"))
  expect_error(plan_total(p, label = ""), "plan_total(label = ) is one text", fixed = TRUE)
  expect_error(plan_total(p, position = "x"), "'arg' should be one of", fixed = TRUE)
  expect_error(plan_total(p, direction = "x"), "'arg' should be one of", fixed = TRUE)
})

test_that("plan_empty()'s text, when and position survive a later call that leaves them out", {
  p <- nd_empty()
  expect_identical(nd_tab(p |> plan_empty(text = "a") |> plan_empty(position = "first"))$label[1L],
                   "a")
  expect_identical(nd_tab(p |> plan_empty(position = "replace") |> plan_empty(text = "None."))$label,
                   "None.")
  # when = "no_rows" kept by a later call that leaves it out
  expect_identical(.plan_resolve(p |> plan_empty(when = "no_rows") |>
                                   plan_empty(text = "a"), "empty")$when, "no_rows")
  # text = NULL (no row) survives too, and a later text brings it back
  expect_identical(nrow(nd_tab(p |> plan_empty(text = NULL) |> plan_empty(position = "first"))), 1L)
  expect_identical(nd_tab(p |> plan_empty(text = NULL) |> plan_empty(text = "b"))$label[2L], "b")
})

test_that("plan_empty() left out is the sentence, when \"auto\", position \"last\"", {
  p <- nd_empty()
  expect_identical(nd_tab(p |> plan_empty()),
                   nd_tab(p |> plan_empty(text = "No subjects met the criteria.",
                                          when = "auto", position = "last")))
  expect_identical(nd_tab(p |> plan_empty())$label,
                   c("Any TEAE", "No subjects met the criteria."))
  expect_identical(.plan_resolve(p |> plan_empty(), "empty"),
                   list(text = "No subjects met the criteria.", when = "auto",
                        position = "last", .no_text = FALSE))
  expect_identical(.plan_resolve(p |> plan_empty(footnote = "F"), "empty"),
                   list(text = "No subjects met the criteria.", when = "auto",
                        position = "last", footnote = "F", .no_text = FALSE))
  expect_error(plan_empty(p, when = "x"), "'arg' should be one of \"auto\", \"no_rows\", \"overall_zero\"", fixed = TRUE)
  expect_error(plan_empty(p, position = "x"), "'arg' should be one of \"last\", \"first\", \"replace\"", fixed = TRUE)
})

test_that("plan_sort(keep = FALSE) survives a later plan_sort() that leaves it out", {
  d <- nd_wide()
  d$ord <- c(2, 2, 1, 1)
  p <- table_plan(d) |> plan_style()
  hid <- nd_pages(p |> plan_sort("ord", keep = FALSE))
  two <- nd_pages(p |> plan_sort("grp", keep = FALSE) |> plan_sort("ord"))
  expect_identical(two, hid)
  expect_false("ord" %in% names(nd_first(two)$data))
  # left out everywhere: kept
  expect_identical(nd_pages(p |> plan_sort("ord")), nd_pages(p |> plan_sort("ord", keep = TRUE)))
  expect_true("ord" %in% names(nd_first(nd_pages(p |> plan_sort("ord")))$data))
  expect_identical(plan_layers(p |> plan_sort("ord"))$layers$sort,
                   list(sort = "ord", .keep = TRUE))
  expect_error(plan_sort(p, "ord", keep = NA), "plan_sort(keep = ) is TRUE", fixed = TRUE)
})

test_that("plan_paginate_group(keep = FALSE) survives a later call that leaves it out", {
  d <- nd_wide()
  p <- table_plan(d) |> plan_style()
  one <- nd_pages(p |> plan_paginate_group(col = "grp", keep = FALSE, heading_at = "section"))
  two <- nd_pages(p |> plan_paginate_group(col = "grp", keep = FALSE) |>
                    plan_paginate_group(heading_at = "section"))
  expect_identical(two, one)
  expect_identical(names(nd_first(two)$data), c("label", "X", "Y"))
  # left out everywhere: the key is printed
  expect_identical(nd_pages(p |> plan_paginate_group(col = "grp")),
                   nd_pages(p |> plan_paginate_group(col = "grp", keep = TRUE)))
  expect_identical(plan_layers(p |> plan_paginate_group(col = "grp"))$layers$group,
                   list(group_col = "grp", .keep = TRUE, .page = TRUE))
  # plan_row_group() alone shares the kind and has no keep
  expect_null(.plan_resolve(table_plan(d) |> plan_row_group(mode = "indent"), "group")$.keep)
})

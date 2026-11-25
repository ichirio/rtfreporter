# plan_total() with two column keys (#1103, #1109): the Total column's
# place follows `position` in the folded key, whatever order the ARD's
# records come in; with no overall rows for the inner key, the outer key is
# folded and the inner key kept under its Total.  Fictional cards-shaped
# ARDs (Example Pharma), built by hand so no package is needed.

.t2c_rec <- function(g1, l1, g2, l2, var, vl, ctx, sn, x) {
  data.frame(group1 = g1, group1_level = l1, group2 = g2, group2_level = l2,
             variable = var, variable_level = vl, context = ctx,
             stat_name = sn, stat_label = sn, stat = x, stringsAsFactors = FALSE)
}

# A shift table: columns the arm (TRT01A), then the post-baseline category
# (ANRIND) under it; rows the baseline category (BNRIND); n and N.  Each
# arm's overall rows (no ANRIND) are bound FIRST, as a template may bind
# them, so the data's own order would put the Total column first.
.t2c_shift <- function(margins = FALSE) {
  na <- NA_character_
  lv <- c("L", "N", "H")
  n <- list(A = rbind(L = c(2, 1, 0), N = c(1, 5, 1), H = c(0, 1, 2)),
            B = rbind(L = c(1, 1, 0), N = c(2, 4, 1), H = c(0, 2, 1)))
  cells <- overall <- marg <- NULL
  for (a in names(n)) {
    m <- n[[a]]                        # rows ANRIND, columns BNRIND
    big <- sum(m)
    for (i in seq_along(lv)) {
      cells <- rbind(cells,
        .t2c_rec("TRT01A", a, "ANRIND", lv[i], "BNRIND", lv, "tabulate", "n", m[i, ]),
        .t2c_rec("TRT01A", a, "ANRIND", lv[i], "BNRIND", lv, "tabulate", "N", sum(m[i, ])))
    }
    overall <- rbind(overall,
      .t2c_rec("TRT01A", a, na, na, "BNRIND", lv, "tabulate", "n", colSums(m)),
      .t2c_rec("TRT01A", a, na, na, "BNRIND", lv, "tabulate", "N", big))
    marg <- rbind(marg,
      .t2c_rec("TRT01A", a, na, na, "ANRIND", lv, "tabulate", "n", rowSums(m)),
      .t2c_rec("TRT01A", a, na, na, "ANRIND", lv, "tabulate", "N", big))
  }
  suppressMessages(normalize_ard(rbind(overall, cells, if (margins) marg)))
}

.t2c_tab <- function(p) as.data.frame(plan_apply(p, "table"))

.t2c_plan <- function(nd, ...) {
  table_plan(nd, cols = c("TRT01A", "ANRIND")) |>
    plan_cells("{n}/{N}", notes = FALSE) |>
    plan_total(...)
}

.t2c_vals <- function(tab) grep("____", names(tab), value = TRUE)

test_that("FT-PL6804 [FR-PL994]: two column keys: the Total column is last or first under each outer value, as position says", {
  nd <- .t2c_shift()
  last <- c("A____L", "A____N", "A____H", "A____Total",
            "B____L", "B____N", "B____H", "B____Total")
  first <- c("A____Total", "A____L", "A____N", "A____H",
             "B____Total", "B____L", "B____N", "B____H")
  expect_identical(.t2c_vals(.t2c_tab(.t2c_plan(nd, direction = "cols"))), last)
  expect_identical(.t2c_vals(.t2c_tab(.t2c_plan(nd, direction = "cols", position = "last"))), last)
  expect_identical(.t2c_vals(.t2c_tab(.t2c_plan(nd, direction = "cols", position = "first"))), first)
  # direction = "both": the same columns, a Total row from the margins
  nb <- .t2c_shift(margins = TRUE)
  tb <- .t2c_tab(.t2c_plan(nb, direction = "both", position = "last"))
  expect_identical(.t2c_vals(tb), last)
  expect_identical(.t2c_vals(.t2c_tab(.t2c_plan(nb, direction = "both", position = "first"))),
                   first)
  expect_true("Total" %in% tb$label)
  # within the order plan_levels() gives the inner key
  lp <- .t2c_plan(nd, direction = "cols", position = "first") |>
    plan_levels(ANRIND = c("H", "N", "L"))
  expect_identical(.t2c_vals(.t2c_tab(lp))[1:4],
                   c("A____Total", "A____H", "A____N", "A____L"))
  lp2 <- .t2c_plan(nd, direction = "cols") |> plan_levels(ANRIND = c("H", "N", "L"))
  expect_identical(.t2c_vals(.t2c_tab(lp2))[1:4],
                   c("A____H", "A____N", "A____L", "A____Total"))
  # direction = "rows" folds no column: the columns are the data's
  tr <- .t2c_tab(.t2c_plan(nb, direction = "rows"))
  expect_false(any(grepl("Total", .t2c_vals(tr))))
})

test_that("FT-PL6805 [FR-PL994]: each arm's Total column is its overall rows: the inner categories summed", {
  nd <- .t2c_shift()
  for (pos in c("first", "last")) {
    tab <- .t2c_tab(.t2c_plan(nd, direction = "cols", position = pos))
    num <- function(x) as.numeric(sub("/.*$", "", x))
    den <- function(x) as.numeric(sub("^.*/", "", x))
    for (a in c("A", "B")) {
      inner <- num(tab[[paste0(a, "____L")]]) + num(tab[[paste0(a, "____N")]]) +
        num(tab[[paste0(a, "____H")]])
      expect_identical(num(tab[[paste0(a, "____Total")]]), inner)
    }
    expect_identical(tab$A____Total, c("3/13", "7/13", "3/13"))
    expect_identical(unique(den(tab$B____Total)), 12)
  }
})

# Lab values by visit: columns the arm, then value / change (the analysis
# variable) under it; the overall rows carry the variable but no arm.
.t2c_by_visit_raw <- function() {
  na <- NA_character_
  vis <- c("WEEK 2", "WEEK 4")
  mean <- list(A = c(AVAL = 10, CHG = 1), B = c(AVAL = 12, CHG = 2),
               All = c(AVAL = 11, CHG = 3))
  out <- NULL
  for (v in vis) for (var in c("AVAL", "CHG")) {
    out <- rbind(out, .t2c_rec(na, na, "AVISIT", v, var, na, "continuous", "mean",
                               mean$All[[var]]))
    for (a in c("A", "B")) {
      out <- rbind(out, .t2c_rec("TRT01A", a, "AVISIT", v, var, na, "continuous", "mean",
                                 mean[[a]][[var]]))
    }
  }
  # cards puts a lone `by` in group1: the overall rows' AVISIT
  ov <- is.na(out$group1)
  out$group1[ov] <- "AVISIT"
  out$group1_level[ov] <- out$group2_level[ov]
  out$group2[ov] <- na
  out$group2_level[ov] <- na
  out
}
.t2c_by_visit <- function() suppressMessages(normalize_ard(.t2c_by_visit_raw()))

test_that("FT-PL6806 [FR-PL995]: no overall rows for the inner key: the outer key is folded, the inner kept under its Total", {
  nd <- .t2c_by_visit()
  p <- table_plan(nd, cols = c("TRT01A", "variable"), rows = "AVISIT") |>
    plan_cells("{mean}", notes = FALSE)
  tab <- .t2c_tab(plan_total(p, direction = "cols"))
  expect_identical(.t2c_vals(tab), c("A____AVAL", "A____CHG", "B____AVAL", "B____CHG",
                                     "Total____AVAL", "Total____CHG"))
  expect_identical(tab$Total____AVAL, c("11", "11"))
  expect_identical(tab$Total____CHG, c("3", "3"))
  tf <- .t2c_tab(plan_total(p, direction = "cols", position = "first"))
  expect_identical(.t2c_vals(tf), c("Total____AVAL", "Total____CHG", "A____AVAL", "A____CHG",
                                    "B____AVAL", "B____CHG"))
  # no overall rows at all still stops, naming the inner key
  bare <- nd[!is.na(nd$TRT01A), , drop = FALSE]
  expect_error(
    .t2c_tab(plan_total(table_plan(bare, cols = c("TRT01A", "variable"), rows = "AVISIT") |>
                          plan_cells("{mean}", notes = FALSE))),
    "the ARD has no overall rows, rows without variable.", fixed = TRUE)
})

# A shift table as a template binds it (#1103): pages by PARAM (a plain
# text key), columns the arm then the post-baseline category, rows the
# baseline category; the arms' N (BIGN) tabulated on its own, no PARAM;
# the Total column's records and the Total row's margins; no factor, so no
# order for the categories but the data's.
.t2c_rec3 <- function(g1, l1, g2, l2, g3, l3, var, vl, sn, x) {
  data.frame(group1 = g1, group1_level = l1, group2 = g2, group2_level = l2,
             group3 = g3, group3_level = l3, variable = var, variable_level = vl,
             context = "tabulate", stat_name = sn, stat_label = sn, stat = x,
             stringsAsFactors = FALSE)
}
.t2c_shift_bign <- function() {
  na <- NA_character_
  lv <- c("L", "N", "H")
  n <- list(ALT = list(A = rbind(c(2, 1, 0), c(1, 5, 1), c(0, 1, 2)),
                       B = rbind(c(1, 1, 0), c(2, 4, 1), c(0, 2, 1))),
            HGB = list(A = rbind(c(3, 0, 0), c(1, 4, 0), c(0, 2, 3)),
                       B = rbind(c(0, 2, 1), c(1, 3, 0), c(1, 1, 2))))
  cells <- overall <- marg <- NULL
  for (pm in names(n)) for (a in names(n[[pm]])) {
    m <- n[[pm]][[a]]                  # rows ANRIND, columns BNRIND
    for (i in seq_along(lv)) {
      cells <- rbind(cells, .t2c_rec3("PARAM", pm, "TRT01A", a, "ANRIND", lv[i],
                                      "BNRIND", lv, "n", m[i, ]))
    }
    overall <- rbind(overall, .t2c_rec3("PARAM", pm, "TRT01A", a, na, na,
                                        "BNRIND", lv, "n", colSums(m)))
    marg <- rbind(marg, .t2c_rec3("PARAM", pm, "TRT01A", a, na, na,
                                  "ANRIND", lv, "n", rowSums(m)))
  }
  bign <- .t2c_rec3(na, na, na, na, na, na, "TRT01A", c("A", "B"), "n", c(30, 28))
  suppressMessages(normalize_ard(rbind(bign, overall, cells, marg)))
}

.t2c_shift_plan <- function(nd, ...) {
  table_plan(nd, cols = c("TRT01A", "ANRIND"), rows = c("PARAM", group = "variable")) |>
    plan_cells("{n}", notes = FALSE) |>
    plan_total(...)
}

test_that("FT-PL1103-1 [FR-PL1000]: (#1103) a shift table's BIGN is no Total: no row without PARAM, each arm's Total its categories summed", {
  nd <- .t2c_shift_bign()
  last <- c("A____L", "A____N", "A____H", "A____Total",
            "B____L", "B____N", "B____H", "B____Total")
  first <- c("A____Total", "A____L", "A____N", "A____H",
             "B____Total", "B____L", "B____N", "B____H")
  for (dir in c("cols", "both")) for (pos in c("last", "first")) {
    tab <- .t2c_tab(.t2c_shift_plan(nd, direction = dir, position = pos))
    expect_false(anyNA(tab$PARAM))
    expect_identical(unique(tab$PARAM), c("ALT", "HGB"))
    expect_identical(.t2c_vals(tab), if (pos == "last") last else first)
    body <- tab[tab$label != "Total", ]
    expect_identical(nrow(body), 6L)
    for (a in c("A", "B")) {
      inner <- rowSums(sapply(paste0(a, "____", c("L", "N", "H")),
                              function(k) as.numeric(body[[k]])))
      expect_identical(as.numeric(body[[paste0(a, "____Total")]]), unname(inner))
    }
    # the arms' N is the header's only
    expect_false(any(c("30", "28") %in% unlist(body[, .t2c_vals(body)])))
  }
  # the pages: one per PARAM, no "group_0"
  pg <- plan_apply(.t2c_shift_plan(nd, direction = "both") |> plan_paginate_group("PARAM"))
  expect_false(any(grepl("group_0", names(pg), fixed = TRUE)))
})

test_that("FT-PL1103-2 [FR-PL1001]: (#1103) the Total row is in its block, last or first by position, with a plain row key and no factor", {
  nd <- .t2c_shift_bign()
  for (dir in c("rows", "both")) {
    tl <- .t2c_tab(.t2c_shift_plan(nd, direction = dir, position = "last"))
    tf <- .t2c_tab(.t2c_shift_plan(nd, direction = dir, position = "first"))
    expect_identical(tl$PARAM, rep(c("ALT", "HGB"), each = 4L))
    expect_identical(tf$PARAM, rep(c("ALT", "HGB"), each = 4L))
    expect_identical(as.character(tl$label), rep(c("L", "N", "H", "Total"), 2L))
    expect_identical(as.character(tf$label), rep(c("Total", "L", "N", "H"), 2L))
    # the margin: ALT, arm A, ANRIND = L is 2 + 1 + 0
    expect_identical(tl$A____L[tl$PARAM == "ALT" & tl$label == "Total"], "3")
  }
})

test_that("FT-PL1109-1 [FR-PL1002]: (#1109) two column keys by visit with the arms' N and a pooled record: the arms totalled, value / change kept under it, by position", {
  # the arms' N, and an overall mean over all visits (no arm, no AVISIT):
  # each leaves a key the cells carry empty, so neither is a Total
  bign <- .t2c_rec(NA, NA, NA, NA, "TRT01A", c("A", "B"), "tabulate", "n", c(30, 28))
  pooled <- .t2c_rec(NA, NA, NA, NA, c("AVAL", "CHG"), NA, "continuous", "mean", c(99, 98))
  nd <- suppressMessages(normalize_ard(rbind(bign, .t2c_by_visit_raw(), pooled)))
  p <- table_plan(nd, cols = c("TRT01A", "variable"), rows = "AVISIT") |>
    plan_cells("{mean}", notes = FALSE)
  tl <- .t2c_tab(plan_total(p, direction = "cols", position = "last"))
  tf <- .t2c_tab(plan_total(p, direction = "cols", position = "first"))
  expect_identical(.t2c_vals(tl), c("A____AVAL", "A____CHG", "B____AVAL", "B____CHG",
                                    "Total____AVAL", "Total____CHG"))
  expect_identical(.t2c_vals(tf), c("Total____AVAL", "Total____CHG", "A____AVAL", "A____CHG",
                                    "B____AVAL", "B____CHG"))
  expect_identical(tl$AVISIT, c("WEEK 2", "WEEK 4"))
  expect_identical(tf$AVISIT, c("WEEK 2", "WEEK 4"))
  expect_false(any(c("99", "98", "30", "28") %in% unlist(tl[, .t2c_vals(tl)])))
  expect_identical(tl$Total____AVAL, c("11", "11"))
  expect_identical(tf$Total____CHG, c("3", "3"))
  # direction = "both": value / change are two analysis variables, so no
  # Total row is said -- the stop names that, not missing overall rows
  expect_error(.t2c_tab(plan_total(p, direction = "both")), "no margins|one analysis")
})

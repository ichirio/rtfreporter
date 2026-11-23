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
.t2c_by_visit <- function() {
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
  suppressMessages(normalize_ard(out))
}

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

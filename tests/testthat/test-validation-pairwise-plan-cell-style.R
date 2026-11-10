# Validation tests: plan_cell_style() -- which cells a call styles (whole
# columns, the header, the rows a condition chooses, or a formula) against
# which attributes it may carry and what a later call does: a decision table
# of the attributes the places take, a pairwise set of the places, columns,
# attributes and a later call, and a table of what cannot be mixed in one
# call.  Helpers prefixed .cs_.  Fictional data only.
#
# ---------------------------------------------------------------------------
#  Design
# ---------------------------------------------------------------------------
#  The rules, as read from the help (plan_cell_style()) and FR-PL020 FR-PL404
#  .. FR-PL420 FR-PL972 FR-PL973:
#
#    place    `cols` alone (no `where`, no `header`): whole columns, every
#             column when `cols` is left out; header = TRUE: the column
#             header cells of `cols`; `where` = a one-sided formula: the rows
#             it holds for, over `cols` or every column, the other rows keep
#             the column's style; an attribute that is itself a one-sided
#             formula (or a list of them named by column): computed row by
#             row, NA leaving the column default alone, for every column or
#             the named ones.
#    attrs    bold, italic, underline, align, color, background,
#             indent_twips and border.  The header takes bold, italic,
#             underline, align, border, color and background (the columns'
#             header_color / header_background; indent_twips stops,
#             FR-PL416); `where` takes all but border (FR-PL417); whole
#             columns take all; a formula carries a value of an attribute
#             that is a value (not a border).
#    later    value styles run in the order written, a formula style is
#             last-wins per attribute (FR-PL407): a later call on the same
#             place that names an attribute gives the final value of it, and
#             the attributes it does not name keep the first call's.
#
#  Data (.cs_tbl): rows n, Mean, SD; columns lab, A, B.  The condition is
#  lab == "n", the first row.  An attribute's value is read where the page
#  keeps it: col_spec for whole columns, the header cell of col_header, and
#  cell_styles by row for a condition or a formula.
#
#  (A) FT-PL993, decision table, place x attribute (4 x 8 = 32 cells; a
#  formula has no border, so 4 x 8 less 1): "ok" (the value is on the page, at
#  the places the rule names and nowhere else), or the error of the rule.
#
#  (B) FT-PL994, pairwise.  Factors and levels:
#    place    cols, header, where, formula, formula list
#    cols     left out, "A", c("A", "B")  (the list names them: not left out)
#    attrs    bold | align | bold, italic, underline | color, background |
#             align, indent_twips  (the header takes all but the last)
#    later    none | a second call on the same place with align "center"
#  Of 5 x 3 x 5 x 2 = 150 rows, the invalid ones (the header with an indent:
#  6; a formula list with no columns: 10) leave 134; .allpairs()
#  picks the rows covering every pair of levels.  Oracle: the rules above; the
#  page's styles must differ from a plan with no cell style at exactly the
#  places the rule names.
#
#  (C) FT-PL995, decision table, what one call may not mix: a formula attribute
#  with `where`, with `header`, with a value attribute; header with where.
# ---------------------------------------------------------------------------

.cs_tbl <- function() {
  data.frame(lab = c("n", "Mean", "SD"), A = c("1", "2", "3"), B = c("4", "5", "6"),
             stringsAsFactors = FALSE)
}

.cs_pg <- function(p) suppressMessages(plan_apply(p, "pages"))[[1L]]

.cs_values <- list(bold = TRUE, italic = TRUE, underline = TRUE, align = "right",
                   color = "#CC0000", background = "#EEEEEE", indent_twips = 200)
.cs_attrs <- names(.cs_values)

# the header's value of an attribute over column j: in the cell of a promoted
# header, else in the column's header_* fields
.cs_head <- function(pg, j, a) {
  row1 <- unclass(pg$col_header)[[1L]]
  if (is.list(row1)) {
    cell <- Filter(function(z) z$from == j, row1)[[1L]]
    if (!is.null(cell[[a]])) return(cell[[a]])
  }
  pg$col_spec[[j]][[paste0("header_", a)]]
}

# every place a style can be read, with its value ("-" when there is none)
.cs_snapshot <- function(pg) {
  cols <- names(pg$data)
  one <- function(v) if (is.null(v) || is.na(v)) "-" else as.character(v)
  out <- list()
  for (j in seq_along(cols)) for (a in .cs_attrs) {
    out[[paste("body", cols[j], a)]] <- one(pg$col_spec[[j]][[a]])
    out[[paste("head", cols[j], a)]] <- one(.cs_head(pg, j, a))
    for (i in seq_len(nrow(pg$data))) {
      v <- pg$cell_styles[[i]][[a]]
      out[[paste("cell", i, cols[j], a)]] <- if (is.null(v)) "-" else one(v[j])
    }
  }
  out
}

# where each place's value is expected: the keys a call touches and the value
.cs_expect <- function(place, cols, attrs, cond_rows = 1L) {
  allcols <- c("lab", "A", "B")
  cs <- if (is.null(cols)) allcols else cols
  out <- list()
  for (a in attrs) {
    v <- as.character(.cs_values[[a]])
    for (cl in cs) {
      if (place == "cols") out[[paste("body", cl, a)]] <- v
      if (place == "header") out[[paste("head", cl, a)]] <- v
      if (place %in% c("where", "formula", "flist")) {
        for (i in cond_rows) out[[paste("cell", i, cl, a)]] <- v
      }
    }
  }
  out
}

# the plan_cell_style() call a place, columns and attributes stand for
.cs_call <- function(p, place, cols, attrs, values = .cs_values) {
  args <- list(p)
  if (!is.null(cols) && place != "flist") args$cols <- cols
  cond <- function(v) as.formula(paste0("~ ifelse(lab == 'n', ",
                                        if (is.character(v)) paste0("'", v, "'") else v, ", NA)"))
  for (a in attrs) {
    v <- values[[a]]
    args[[a]] <- switch(place,
      cols = v, header = v, where = v,
      formula = cond(v),
      flist = stats::setNames(rep(list(cond(v)), length(cols)), cols))
  }
  if (place == "header") args$header <- TRUE
  if (place == "where") args$where <- ~ lab == "n"
  do.call(plan_cell_style, args)
}

test_that("FT-PL993 [FR-PL404 FR-PL408 FR-PL409 FR-PL410 FR-PL416 FR-PL417 FR-PL972 FR-PL973 FR-IX408]: plan_cell_style(): which attributes each place takes, the decision table", {
  d <- .cs_tbl()
  hdr_msg <- "plan_cell_style(header = TRUE) takes bold, italic, underline, align, border, color and background."
  where_msg <- "a border is set on whole columns."
  base <- .cs_snapshot(.cs_pg(table_plan(d) |> plan_style()))
  bord <- rtf_border(bottom = TRUE)
  places <- c("cols", "header", "where", "formula")
  for (pl in places) for (a in c(.cs_attrs, "border")) {
    label <- paste(pl, a)
    if (pl == "formula" && a == "border") next
    args <- list(table_plan(d))
    v <- if (a == "border") bord else .cs_values[[a]]
    if (pl == "formula") {
      args[[a]] <- as.formula(paste0("~ ifelse(lab == 'n', ",
                                     if (is.character(v)) paste0("'", v, "'") else v, ", NA)"))
    } else {
      args[[a]] <- v
    }
    if (pl != "formula") args$cols <- "A"
    if (pl == "header") args$header <- TRUE
    if (pl == "where") args$where <- ~ lab == "n"
    if (pl == "header" && a == "indent_twips") {
      expect_error(do.call(plan_cell_style, args), hdr_msg, fixed = TRUE, info = label)
    } else if (pl == "where" && a == "border") {
      expect_error(do.call(plan_cell_style, args), where_msg, fixed = TRUE, info = label)
    } else {
      pg <- .cs_pg(do.call(plan_cell_style, args))
      if (a == "border") {
        # whole columns and the header: the border is on column A / its header cell
        expect_false(is.null(if (pl == "cols") pg$col_spec[[2L]]$border else
          .cs_head(pg, 2L, "border")), info = label)
        expect_null(pg$col_spec[[3L]]$border, info = label)
      } else {
        now <- .cs_snapshot(pg)
        cols_in <- if (pl == "formula") c("lab", "A", "B") else "A"
        want <- .cs_expect(pl, cols_in, a)
        changed <- names(now)[unlist(now) != unlist(base[names(now)])]
        for (k in names(want)) expect_equal(now[[k]], want[[k]], info = paste(label, k))
        expect_true(all(changed %in% names(want)), info = paste(label, "elsewhere:",
                                                                  paste(setdiff(changed, names(want)), collapse = ", ")))
      }
    }
  }
})

test_that("FT-PL994 [FR-PL404 FR-PL405 FR-PL407 FR-PL408 FR-PL409 FR-PL410 FR-PL972 FR-PL973 FR-IX408]: plan_cell_style(): place x columns x attributes x a later call, pairwise", {
  attr_sets <- list(bold = "bold", align = "align", biu = c("bold", "italic", "underline"),
                    cb = c("color", "background"), ai = c("align", "indent_twips"))
  cand <- expand.grid(place = c("cols", "header", "where", "formula", "flist"),
                      cols = c("all", "A", "AB"), attrs = names(attr_sets),
                      later = c("none", "align"), stringsAsFactors = FALSE)
  cand <- cand[!(cand$place == "header" & cand$attrs == "ai") &
                 !(cand$place == "flist" & cand$cols == "all"), ]
  expect_identical(nrow(cand), 134L)
  ap <- .allpairs(cand)
  expect_setequal(ap$covered, ap$need)
  expect_lt(nrow(ap$rows), 40L)
  d <- .cs_tbl()
  base <- .cs_snapshot(.cs_pg(table_plan(d) |> plan_style()))
  for (i in seq_len(nrow(ap$rows))) {
    r <- ap$rows[i, ]
    cols <- switch(r$cols, all = NULL, A = "A", AB = c("A", "B"))
    attrs <- attr_sets[[r$attrs]]
    p <- .cs_call(table_plan(d), r$place, cols, attrs)
    want <- .cs_expect(r$place, cols, attrs)
    if (r$later == "align") {
      p <- .cs_call(p, r$place, cols, "align", values = list(align = "center"))
      for (cl in (if (is.null(cols)) c("lab", "A", "B") else cols)) {
        for (k in grep(paste0(" ", cl, " align$|^cell [0-9] ", cl, " align$"), names(want), value = TRUE)) want[[k]] <- NULL
      }
      want <- c(want, .cs_expect(r$place, cols, "align") |> lapply(function(x) "center"))
    }
    now <- .cs_snapshot(.cs_pg(p))
    label <- paste(unlist(r), collapse = "/")
    changed <- names(now)[unlist(now) != unlist(base[names(now)])]
    for (k in names(want)) expect_equal(now[[k]], want[[k]], info = paste(label, k))
    expect_true(all(changed %in% names(want)),
                info = paste(label, "elsewhere:", paste(setdiff(changed, names(want)), collapse = ", ")))
  }
})

test_that("FT-PL995 [FR-PL412 FR-PL413 FR-PL414 FR-PL415 FR-IX408]: plan_cell_style(): what one call may not mix, the decision table", {
  d <- .cs_tbl()
  p <- table_plan(d)
  f <- ~ lab == "n"
  mix <- "so it takes neither `where` nor `header`"
  expect_error(plan_cell_style(p, bold = f, where = ~ lab == "SD"), mix, fixed = TRUE)
  expect_error(plan_cell_style(p, bold = f, header = TRUE), mix, fixed = TRUE)
  expect_error(plan_cell_style(p, bold = f, italic = TRUE), mix, fixed = TRUE)
  expect_error(plan_cell_style(p, color = list(A = ~ ifelse(lab == "n", "#CC0000", NA)), italic = TRUE),
               mix, fixed = TRUE)
  expect_s3_class(plan_cell_style(p, bold = f, italic = f), "table_plan")
  expect_error(plan_cell_style(p, header = TRUE, where = f, bold = TRUE),
               "styles the column header, which has no rows for `where` to choose", fixed = TRUE)
  expect_error(plan_cell_style(p, cols = "A"), "nothing to style", fixed = TRUE)
  expect_error(plan_cell_style(p, where = "lab == 'n'", bold = TRUE), "is a one-sided formula", fixed = TRUE)
  expect_error(plan_cell_style(p, where = lab ~ x, bold = TRUE), "is a one-sided formula", fixed = TRUE)
})

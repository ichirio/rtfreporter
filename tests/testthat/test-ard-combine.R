# normalize_ard(combine = ) (#1081): ADaM's Low / High variables read as one
# table dimension.  The data is the fictional ADLB of helper-lab-shift.R.

test_that("FT-AR8001 [FR-AR255]: normalize_ard(combine = ) renames the variables and folds their key columns into one at the first one's place, its levels and the bookkeeping kept", {
  skip_on_cran()
  skip_if_not_installed("cards")
  ard <- .shift_ard()
  nd0 <- normalize_ard(ard)
  nd <- normalize_ard(ard, combine = .shift_combine)
  # the key columns: one, where BTOXGRL was
  expect_identical(names(nd)[1:4], c("PARAM", "tox_dir", "base_gr", "variable"))
  expect_false(any(c("BTOXGRL", "BTOXGRH", "ATOXGRL", "ATOXGRH") %in% names(nd)))
  expect_identical(levels(nd$base_gr), as.character(0:4))
  b0 <- ifelse(is.na(nd0$BTOXGRL), as.character(nd0$BTOXGRH), as.character(nd0$BTOXGRL))
  expect_identical(as.character(nd$base_gr), b0)
  # the variable column: an old name read as the new one
  new <- c(ATOXGRL = "worst_gr", ATOXGRH = "worst_gr", BTOXGRL = "base_gr",
           BTOXGRH = "base_gr", tox_dir = "tox_dir")
  expect_identical(nd$variable, unname(new[nd0$variable]))
  # nothing else moves: the rows, the statistics, the marks
  expect_identical(nd$stat, nd0$stat)
  expect_identical(nd$.label, nd0$.label)
  expect_identical(nd$.key_own, nd0$.key_own)
  expect_true(all(nd$.key_own[nd$variable == "base_gr"]))
  expect_identical(nd$.kind, nd0$.kind)
  w <- nd$variable == "worst_gr"
  expect_identical(nd$.label_order[w], match(nd$.label[w], as.character(0:4)))
  expect_equal(attr(nd, "ard_total_n"), 12)
  # the discards report follows the new name
  ig <- attr(normalize_ard(ard, combine = .shift_combine, drop_key_variables = TRUE),
             "ard_ignored")
  expect_identical(ig$rows[ig$variable == "base_gr"], c(20L, 20L, 20L))
  expect_false(any(grepl("TOXGR", ig$variable)))
  # NULL (the default) changes nothing
  expect_identical(normalize_ard(ard, combine = NULL), nd0)
  # character keys stay character, their values the union first seen
  x <- .shift_data()
  for (v in c("BTOXGRL", "ATOXGRL", "BTOXGRH", "ATOXGRH")) x$adlb[[v]] <- as.character(x$adlb[[v]])
  ndc <- normalize_ard(.shift_ard(x), combine = .shift_combine)
  expect_type(ndc$base_gr, "character")
  expect_identical(sort(unique(ndc$base_gr[!is.na(ndc$base_gr)])), as.character(0:3))
})

test_that("FT-AR8002 [FR-AR255]: a lab shift table by direction from ADaM's names and combine, with its Total row, Total column and corner, is the reference table and RTF", {
  skip_on_cran()
  skip_if_not_installed("cards")
  x <- .shift_data()
  nd <- normalize_ard(.shift_ard(x), combine = .shift_combine)
  ref <- normalize_ard(.shift_ref_ard(x))
  tab <- as.data.frame(plan_apply(.shift_plan(nd), "table"))
  expect_identical(names(tab), c("PARAM", "tox_dir", "label", paste("Grade", 0:4), "Total"))
  expect_identical(as.character(tab$label[1:6]), c(paste("Grade", 0:4), "Total"))
  k <- tab[tab$PARAM == "Potassium (mmol/L)", ]
  expect_identical(as.character(unique(k$tox_dir)), c("Low", "High"))
  hi <- k[k$tox_dir == "High", ]
  expect_identical(hi$Total[hi$label == "Total"], "11 (91.7)")      # the corner
  expect_identical(hi$`Grade 0`[hi$label == "Total"], "6 (50.0)")   # the Total row
  expect_identical(hi$Total[hi$label == "Grade 0"], "9 (75.0)")     # the Total column
  # the reference: one variable per role made by stacking the records
  expect_identical(tab, as.data.frame(plan_apply(.shift_plan(ref), "table")))
  old <- options(rtfreporter.render_time = as.POSIXct("2026-01-01 09:00", tz = "UTC"))
  on.exit(options(old), add = TRUE)
  rtf <- function(p) {
    f <- tempfile(fileext = ".rtf")
    on.exit(unlink(f))
    generate_rtfreport(rtf_document() |> rtf_tables(p), f, overwrite = TRUE)
    readBin(f, "raw", file.size(f))
  }
  expect_identical(rtf(.shift_plan(nd)), rtf(.shift_plan(ref)))
})

test_that("FT-AR8003 [FR-AR255 FR-AR124]: a stacked key may name a combined one", {
  skip_on_cran()
  skip_if_not_installed("cards")
  nd <- normalize_ard(.shift_ard(), combine = .shift_combine,
                      stack = list(s = list("tox_dir", c("tox_dir", "base_gr"))))
  expect_identical(levels(nd$s), c("tox_dir", "tox_dir / base_gr"))
  g <- !is.na(nd$base_gr)
  expect_true(all(nd$s[g] == "tox_dir / base_gr"))
  expect_identical(as.character(nd$s_level[g]),
                   paste(nd$tox_dir[g], nd$base_gr[g], sep = " / "))
})

test_that("FT-AR8004 [FR-AR256 FR-AR257 FR-AR258 FR-AR259 FR-AR260 FR-AR261 FR-AR262]: normalize_ard(combine = ) stops on what it cannot read", {
  skip_on_cran()
  skip_if_not_installed("cards")
  ard <- .shift_ard()
  # what it refuses
  shape <- "`combine` is a named list: a new name and the two or more variables"
  expect_error(normalize_ard(ard, combine = list(c("ATOXGRL", "ATOXGRH"))), shape, fixed = TRUE)
  expect_error(normalize_ard(ard, combine = list(a = "ATOXGRL")), shape, fixed = TRUE)
  expect_error(normalize_ard(ard, combine = list(a = 1:2)), shape, fixed = TRUE)
  expect_error(normalize_ard(ard, combine = c(a = "ATOXGRL", b = "ATOXGRH")), shape, fixed = TRUE)
  blank <- list(c("ATOXGRL", "ATOXGRH"))
  names(blank) <- ""
  expect_error(normalize_ard(ard, combine = blank), shape, fixed = TRUE)
  expect_error(normalize_ard(ard, combine = list(a = c("ATOXGRL", "ATOXGRL"))), shape, fixed = TRUE)
  expect_error(normalize_ard(ard, combine = list(a = c("ATOXGRL", "ATOXGRH"),
                                                 a = c("BTOXGRL", "BTOXGRH"))),
               "`combine`: the new name 'a' is given twice.", fixed = TRUE)
  expect_error(normalize_ard(ard, combine = list(a = c("ATOXGRL", "ATOXGRH"),
                                                 b = c("ATOXGRL", "BTOXGRL"))),
               "`combine`: 'ATOXGRL' is in two elements; a variable is read under one name.",
               fixed = TRUE)
  expect_error(normalize_ard(ard, combine = list(a = c("ATOXGRL", "ATOXGRZ"))),
               "`combine`: no variable 'ATOXGRZ' in this ARD.", fixed = TRUE)
  expect_error(normalize_ard(ard, combine = list(PARAM = c("ATOXGRL", "ATOXGRH"))),
               "`combine`: 'PARAM' is a column, a variable or a stacked key already", fixed = TRUE)
  expect_error(normalize_ard(ard, combine = list(a = c("ATOXGRL", "ATOXGRH")),
                             stack = list(a = "tox_dir")),
               "`combine`: 'a' is a column, a variable or a stacked key already", fixed = TRUE)
  expect_error(normalize_ard(ard, hierarchy = "BTOXGRL",
                             combine = list(a = c("BTOXGRL", "BTOXGRH"))),
               "`combine`: 'BTOXGRL' is in `hierarchy`.", fixed = TRUE)
  # the cells hold the worst grade and the baseline grade: never one dimension
  expect_error(normalize_ard(ard, combine = list(a = c("ATOXGRL", "BTOXGRL"))),
               "rows have values of 'ATOXGRL' and 'BTOXGRL' together", fixed = TRUE)
})

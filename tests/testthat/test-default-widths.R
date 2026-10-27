# The column widths of a table that gives none: default_col_widths()

.dm <- function(label = "Age (years)") {
  data.frame(Characteristic = c(label, "  Mean (SD)", "  Median"),
             Placebo = c("", "75.2 (8.59)", "76.0"),
             Active  = c("", "74.4 (7.89)", "75.0"),
             stringsAsFactors = FALSE)
}

test_that("the data columns share the rest equally; the row headings by content", {
  w <- default_col_widths(.dm())
  expect_named(w, c("Characteristic", "Placebo", "Active"))
  expect_identical(unname(w[2:3]), c(1, 1))
  # a longer label, a wider row-heading column -- up to its cap
  long <- default_col_widths(.dm(strrep("x", 40)))
  expect_gt(long[["Characteristic"]], w[["Characteristic"]])
  huge <- default_col_widths(.dm(strrep("x", 200)), digits = 6L)
  expect_lte(huge[["Characteristic"]] / sum(huge), 0.45 + 1e-6)
})

test_that("many data columns lower the row headings' cap; indentation raises it", {
  wide <- cbind(.dm(strrep("x", 200)),
                as.data.frame(matrix("1", 3, 10, dimnames = list(NULL, paste0("V", 1:10)))))
  w <- default_col_widths(wide, digits = 6L)
  expect_lte(w[["Characteristic"]] / sum(w), 0.30 + 1e-6)
  flat <- .dm(strrep("x", 200))
  flat$Characteristic <- trimws(flat$Characteristic)
  wf <- default_col_widths(flat, digits = 6L)
  wi <- default_col_widths(.dm(strrep("x", 200)), digits = 6L)
  expect_gt(wi[["Characteristic"]] / sum(wi), wf[["Characteristic"]] / sum(wf))
})

test_that("a data column whose cells do not fit is widened from the others", {
  d <- data.frame(Subject = c("S-001", "S-002"),
                  Term = c(strrep("long term ", 8), "x"),
                  Grade = c("1", "2"), Serious = c("N", "Y"),
                  stringsAsFactors = FALSE)
  w <- default_col_widths(d, digits = 6L)
  expect_gt(w[["Term"]], w[["Grade"]])
  expect_equal(w[["Grade"]], w[["Serious"]])
})

test_that("an rtftable with no widths renders them; one with widths is unchanged", {
  out <- function(tbl) {
    d <- tempfile("dw-")
    dir.create(d)
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    f <- file.path(d, "t.rtf")
    generate_rtfreport(rtf_tables(rtf_document(), list(tbl)), f, overwrite = TRUE)
    l <- readLines(f, warn = FALSE)
    row <- grep("Mean (SD)", l, value = TRUE, fixed = TRUE)[1L]
    diff(c(0L, as.integer(sub(".*cellx", "",
                             regmatches(row, gregexpr("\\\\cellx[0-9]+", row))[[1L]]))))
  }
  w <- out(rtftable(.dm()))
  rel <- default_col_widths(.dm(), digits = 6L)
  expect_true(all(abs(w - sum(w) * rel / sum(rel)) <= 2))
  # given widths: as given
  g <- out(rtftable(.dm(), col_rel_width = c(2, 1, 1)))
  expect_true(all(abs(g - sum(g) * c(2, 1, 1) / 4) <= 2))
})

test_that("a table split into column pages keeps each column's default width", {
  d <- cbind(.dm(), as.data.frame(matrix("1", 3, 4, dimnames = list(NULL, paste0("V", 1:4)))))
  tbl <- rtftable(d)
  pages <- paginate_cols(tbl, at = 5, width = "keep")
  expect_equal(pages[[1L]]$col_rel_width[1L], pages[[2L]]$col_rel_width[1L])
})

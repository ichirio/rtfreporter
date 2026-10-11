# Early, clear errors for common first-session mistakes (#594 E1-E11).
# Each of these used to be accepted and fail later (or never), so the
# valid spellings next to them must keep working unchanged.

test_that("rtf_document(page = ) / rtf_config(page = ) take an rtf_page or a named list (E1)", {
  expect_error(rtf_document(page = "a4"),
               "`page` must be an rtf_page\\(\\) object.*rtf_page\\(paper_size = \"a4\"\\)")
  expect_error(rtf_document(page = list("A4")), "an unnamed list")
  expect_error(rtf_document(page = 1), "not a numeric")
  doc <- rtf_document()
  expect_error(rtf_config(doc, page = "a4"), "`page` must be an rtf_page")

  expect_s3_class(rtf_document(page = rtf_page(paper_size = "A4")), "rtf_document")
  expect_s3_class(rtf_document(page = list(paper_size = "A4")), "rtf_document")
  expect_s3_class(rtf_document(page = list()), "rtf_document")
  expect_s3_class(rtf_config(doc, page = list(width_in = 8.27)), "rtf_document")
})

test_that("rtf_header() / rtf_footer() refuse cell names other than l / c / r (E2)", {
  expect_error(rtf_header(c(left = "x")),
               "`rtf_header\\(\\)` row 1: a cell may only be named l, c or r.*'left'")
  expect_error(rtf_footer(list(c(l = "a"), c(l = "b", right = "c"))),
               "`rtf_footer\\(\\)` row 2.*'right'")
  expect_error(rtf_header(list(list(columns = c(centre = "x")))), "'centre'")

  # valid rows are unchanged: named l / c / r, unnamed, empty, legacy list
  expect_silent(rtf_header(c(l = "a", c = "b", r = "c")))
  expect_silent(rtf_header(c("a", "b")))
  expect_silent(rtf_footer(list(c(l = "a"), "", c(r = "Page {PAGE}"))))
  expect_silent(rtf_header(list(list(columns = c(l = "x")))))
})

test_that("a positional col_header row gives one label per column (E4)", {
  df <- data.frame(a = 1, b = 2, c = 3)
  expect_error(rtftable(df, col_header = c("A", "B")),
               "row 1 has 2 labels but the table has 3 columns \\(a, b, c\\)")
  expect_error(rtftable(df, col_header = list(c("A", "B", "C"), c("x", "y", "z", "w"))),
               "row 2 has 4 labels")
  expect_error(rtf_tables(rtf_document(), df, col_header = c("A", "B")),
               "has 2 labels but the table has 3 columns")
  expect_error(rtftable(df, col_header = "A | B"), "row 1 has 2 labels")

  # named rows still change only the columns they name
  expect_s3_class(rtftable(df, col_header = c(b = "Bee")), "rtftable")
  expect_s3_class(rtftable(df, col_header = c("A", "B", "C")), "rtftable")
  expect_s3_class(rtftable(df, col_header = "A | B | C"), "rtftable")
  # a single label is allowed, as set_col_header() allows it
  expect_s3_class(rtftable(df, col_header = "Only"), "rtftable")
})

test_that("rtf_col_header() label rows have the same length (E5)", {
  expect_error(rtf_col_header(c("a", "b"), c("A", "B", "C")),
               "rows 1, 2 have 2, 3 labels")
  expect_s3_class(rtf_col_header(list(col_cell(c(2, 3), "Arm")),
                                 c("a", "b", "c")),
                  "rtf_col_header")
  expect_s3_class(rtf_col_header(c("a", "b"), c(a = "A")), "rtf_col_header")
  expect_s3_class(rtf_col_header("Top", c("a", "b", "c")), "rtf_col_header")
})

test_that("rtf_tables() names what it got and where other tables go (E6)", {
  expect_error(rtf_tables(rtf_document(), list("notatable")),
               "Item 1 is a 'character'.*one page.*as_rtftables\\(\\)")
})

test_that("error texts name the real argument (E7, E8) and verb (E9)", {
  expect_error(table_plan(), "`x` \\(the data\\) is required")
  expect_error(normalize_ard(data.frame(a = 1)),
               "`x` does not look like a cards ARD")
  expect_error(plan_apply(table_plan(data.frame(a = 1))),
               "plan_paginate_rows")
})

test_that("titles / footnotes length and order messages say what to do (E10, E11)", {
  doc <- rtf_document()
  expect_error(rtf_titles(doc, list("T")),
               "Call rtf_titles\\(\\) after rtf_tables\\(\\)")
  expect_error(rtf_footnotes(doc, list("F")),
               "Call rtf_footnotes\\(\\) after rtf_tables\\(\\)")

  df <- data.frame(a = 1)
  expect_error(rtf_tables(doc, df, titles = list("a", "b")),
               "`titles` has 2 blocks but there is 1 page")
  d2 <- rtf_tables(doc, list(df, df, df))
  expect_error(rtf_titles(d2, list("a", "b")),
               "`titles` has 2 blocks but the document has 3 pages")
  expect_error(rtf_footnotes(d2, list("a", "b")),
               "`footnotes` has 2 blocks but the document has 3 pages")
})

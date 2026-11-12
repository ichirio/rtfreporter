# FR-IX336: the order in which a column's look is resolved (user-approved
# wording, 2026-11-13): a col_spec entry (one column) > a table-wide rtftable()
# argument > the style's field > the package default.

test_that("FT-IX6501 [FR-IX336]: col_spec beats the table-wide argument, which beats the style, which beats the default", {
  d  <- data.frame(A = "a", B = "b", stringsAsFactors = FALSE)
  st <- rtf_table_style(header_align = "left", align = "right", bold = TRUE,
                        cell_padding_left_twips = 300L, row_height_twips = 400L)
  cs <- function(t, j, k) t$col_spec[[j]][[k]]
  # header_align, the one field both col_spec and an argument set:
  # col_spec "right" > col_header_align "center" > style "left" > align
  t <- rtftable(d, style = st, col_header_align = "center",
                col_spec = list(list(col = 1, header_align = "right")))
  expect_identical(c(cs(t, 1, "header_align"), cs(t, 2, "header_align")),
                   c("right", "center"))
  expect_identical(cs(rtftable(d, style = st), 2, "header_align"), "left")
  t <- rtftable(d)
  expect_identical(c(cs(t, 1, "header_align"), cs(t, 2, "header_align")),
                   c("left", "center"))
  # align and bold have no table-wide argument: col_spec > style > default
  t <- rtftable(d, style = st,
                col_spec = list(list(col = 1, align = "center", bold = FALSE)))
  expect_identical(c(cs(t, 1, "align"), cs(t, 2, "align")), c("center", "right"))
  expect_identical(c(cs(t, 1, "bold"), cs(t, 2, "bold")), c(FALSE, TRUE))
  # padding and row height have no col_spec entry: argument > style > default
  t <- rtftable(d, style = st, cell_padding_left_twips = 50L, row_height_twips = 250L)
  expect_identical(c(t$cell_padding_left_twips, t$row_height_twips), c(50L, 250L))
  t <- rtftable(d, style = st)
  expect_identical(c(t$cell_padding_left_twips, t$row_height_twips), c(300L, 400L))
  # a style that does not say align: row title left, the others centred
  t <- rtftable(d, style = rtf_table_style(bold = TRUE))
  expect_identical(c(cs(t, 1, "align"), cs(t, 2, "align")), c("left", "center"))
})

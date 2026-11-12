## tests/testthat/test-api-surface.R
##
## #346: the API-review measure -- the export count, kept honest.  0.9.0
## removed every function and argument the 0.5.0 - 0.8.x reviews had
## deprecated (#788); the tests below keep them gone.

library(testthat)

.exports <- function() {
  ns <- asNamespace("rtfreporter")
  sort(getNamespaceExports(ns))
}

.reset_deprecation <- function() {
  st <- rtfreporter:::.deprecation_state
  rm(list = ls(st), envir = st)
}

test_that("the functions deprecated before 0.9.0 are gone (#788)", {
  gone <- c("toc_heading", "toc_entry", "assemble_files", "assemble_spec",
            "assemble_toc", "assemble_from_spec", "fmt_count_paren_bare",
            "add_col_header_row", "col_header_from_names", "realign_count_pct",
            "format_count_pct", "paginate", "rtf_border_side", "rtf_border_with",
            "rtf_border_none", "rtf_border_top", "rtf_border_bottom",
            "rtf_border_box", "rtf_table_border", "rtf_border_tfl",
            "update_header_row", "update_footer_row", "set_header_cell")
  expect_identical(intersect(gone, .exports()), character(0))
  # rtf_default_format() gave way to rtf_style() (#804), and went in 0.9 (#975)
  expect_false("rtf_default_format" %in% .exports())
  expect_false("default_format" %in% names(formals(rtf_document)))
  expect_false("default_format" %in% names(formals(rtf_config)))
  # a call that still passes it is R's own "unused argument" error
  expect_error(rtf_document(default_format = list(markup = "all")), "unused argument")
  expect_error(rtf_config(rtf_document(), default_format = list(markup = "all")),
               "unused argument")
})

test_that("one border constructor is left, and one line", {
  border_api <- grep("^rtf_border|^rtf_table_border$", .exports(), value = TRUE)
  # Two, doing different jobs: which edges, and what the line is.
  expect_setequal(border_api, c("rtf_border", "rtf_border_line"))
})

test_that("the effective export count is the reviewed number", {
  # (91 since #463 added rtfreporter_ai_manual(); 92 since #476 added
  # round_num(); 126 since #491 brought the ARD table engine --
  # 34 functions -- over from tflspec; 122 since #498 folded plan_fmt()
  # into plan_digits() and the three style verbs into plan_cell_style();
  # 123 since #529 added rtf_text_tokens(); 124 since #536 added
  # plan_header_tokens(); 125 since the pre-CRAN API review, iteration 1,
  # renamed rtf_border_side() to rtf_border_line() and deprecated the old
  # name with six others; 105 since six assembly helpers gave way to
  # assemble_folder() and assemble_rtf(toc = <a table>); 126 / 106 since
  # #598 added plan_nest(), a variable's rows under a level of another;
  # 127 / 107 since tflplanner #288 needed plan_n_candidates(), the
  # populations a header's {n} can say;
  # 128 / 108 since plan_total(), a Total column from the overall rows;
  # 129 / 106 since the review's iteration 0 (#594) renamed
  # format_count_pct() to fmt_count_pct() and deprecated the old name with
  # fmt_count_paren_bare() and realign_count_pct(); 130 / 107 since #619
  # added plan_stat_groups(), a record's statistics in named groups; 131 /
  # 108 since #625 added plan_compare(), the comparison columns; 132 / 109
  # since #620 added plan_filter(), rows dropped by a condition; 133 / 110
  # since #691 added plan_empty(), the row of a table of no events; 134 /
  # 111 since rtf_layout(), a document's contents as printed pages; 135 /
  # 112 since default_col_widths(), the widths of a table that gives none;
  # 136 / 113 since #737 added plan_append(), another plan's rows under a
  # table's; 137 / 114 since rtfreporter_validation_kit(), the user's
  # validation templates; 114 since 0.9.0 removed the 23 deprecated ones,
  # #788; 115 since #804 added rtf_style(), one style object for every
  # module; 114 since #975 removed rtf_default_format().)
  expect_length(.exports(), 114L)
})

test_that("the argument names removed in 0.9.0 are gone (#788)", {
  has <- function(f, a) a %in% names(formals(f))
  expect_false(has(as_rtftable, "gt_obj"))
  expect_false(any(has(as_rtftables, c("stub_vars", "stub_label", "stub_indent",
                                       "stub_group_summary"))))
  expect_false(has(add_cont_label, "chunk"))
  expect_false(has(set_blank_rows, "df"))
  expect_false(has(rtftable, "spanning_header"))
  expect_false(has(rtf_tables, "spanning_header"))
  expect_false(any(has(rtfreporter:::paginate_cols.list,
                       c("cols", "by", "carry", "width", "page_order"))))
  expect_false(any(has(rtfreporter:::paginate_cols.rtftable,
                       c("cols", "by", "carry", "width", "page_order"))))
  for (f in list(text_width_in, auto_col_widths, fit_listing_widths)) {
    expect_false(has(f, "size_half_points"))
  }
  expect_false(has(auto_col_widths, "df"))
  # the old names now reach `...` and say they are not arguments
  d <- data.frame(a = 1, b = 2)
  expect_warning(paginate_cols(rtftable(d), at = 2, carry = 1), "unknown argument `carry`")
  # a spanning row is the first row of col_header
  sp <- list(list(from = 1, to = 2, label = "AB"))
  t2 <- rtftable(d, col_header = list(sp, c("a", "b")))
  expect_length(t2$col_header, 2L)
})

test_that("fmt_count_paren(bare = TRUE) is the built-in \"count_paren_bare\", byte for byte (#594)", {
  xs <- list(c("1 (1.2%)", "0", "11 (3.6%)", "108 (35.3%)"),
             c("5 (33.3)", "0", "12 (100.0)", NA, ""),
             c("86", "5 (5.8)", "10 (11.6)", "0"),
             c("Mild", "2 (1.0%)", "0"))
  for (x in xs) for (nb in c(" ", "\u00a0")) for (na in c("", "-")) {
    expect_identical(fmt_count_paren(x, nbsp = nb, na = na, bare = TRUE),
                     rtfreporter:::.fmt_count_paren_bare(x, nbsp = nb, na = na))
  }
  # bare = FALSE is fmt_count_paren() as it was
  expect_identical(fmt_count_paren(xs[[1]]), fmt_count_paren(xs[[1]], bare = FALSE))
})

test_that("generate_rtfreport() writes over a file by default (#594)", {
  expect_true(formals(generate_rtfreport)$overwrite)
})

test_that("the cell-format name \"realign_count_pct\" is gone; \"count_pct\" is the one (#788)", {
  x <- c("5 (33.3)", "12 (100.0)")
  expect_error(rtfreporter:::.builtin_cell_format("realign_count_pct"),
               "not a built-in cell format")
  f <- rtfreporter:::.builtin_cell_format("count_pct")
  expect_identical(f(x), rtfreporter:::.realign_count_pct(x))
  # "count_paren_bare" stays: fmt_count_paren(bare = TRUE)
  expect_silent(rtfreporter:::.builtin_cell_format("count_paren_bare"))
  # the align_count_pct switch says nothing
  expect_silent(rtfreporter:::.resolve_align_count_pct(TRUE))
})

test_that("a border line saved under the old class name still reads", {
  old <- structure(list(style = "single", width = 15L, color = NULL),
                   class = "rtf_border_side")
  expect_silent(rtf_border(top = old))
})

test_that("the first argument is data", {
  expect_identical(names(formals(set_blank_rows))[1], "data")
  expect_identical(names(formals(add_cont_label))[1], "data")
  expect_identical(names(formals(auto_col_widths))[1], "data")
})

test_that("rtf_section(header = , footer = ) is the section; secinfo is checked (#594 1.5, E3)", {
  h <- rtf_header(c(l = "H"))
  f <- rtf_footer(c(l = "F"))
  d <- rtf_tables(rtf_document(), data.frame(a = 1))
  expect_identical(rtf_section(d, page = 1, header = h, footer = f)$sections,
                   rtf_section(d, page = 1, secinfo = list(header = h, footer = f))$sections)
  expect_identical(rtf_section(d, header = h)$sections,
                   rtf_section(d, secinfo = list(header = h, footer = NULL))$sections)
  expect_error(rtf_section(d, page = 1, header = h, secinfo = list(header = h)), "not both")
  expect_error(rtf_section(d, page = 1, secinfo = list(hdr = h)), "`hdr`")
  expect_error(rtf_section(d, page = 1, secinfo = list(header = 1)), "rtf_header")
  # the text, the rows, a watermark: as before
  expect_no_error(rtf_section(d, page = 1, secinfo = list(header = "x")))
  expect_no_error(rtf_section(d, page = 1, secinfo = list(header = h, watermark = "DRAFT")))
  # several sections, the older list rows form, a wrapped section: as before
  expect_no_error(rtf_section(d, page = c(1, 2),
                              secinfo = list(list(header = h), list(header = h, footer = f))))
  expect_no_error(rtf_section(d, page = 1, secinfo = list(header = list(l = "x"))))
  expect_no_error(rtf_section(d, page = 1, secinfo = list(list(header = h))))
})

test_that("a character vector of titles or footnotes is one block for every page (#594)", {
  d <- rtf_tables(rtf_document(), list(data.frame(a = 1), data.frame(a = 2)))
  expect_identical(.doc_titles(rtf_titles(d, c("T", "U"))), list(c("T", "U"), c("T", "U")))
  expect_identical(.doc_footnotes(rtf_footnotes(d, "F")), list("F", "F"))
  d2 <- rtf_tables(rtf_document(), list(data.frame(a = 1), data.frame(a = 2)),
                   titles = c("T", "U"))
  expect_identical(.doc_titles(d2), list(c("T", "U"), c("T", "U")))
  expect_error(rtf_titles(d, 1), "character vector")
})

test_that("paginate_cols() takes the plan's names (#594 1.4)", {
  df <- data.frame(id = 1:2, a = 1, b = 2, c = 3, d = 4, e = 5)
  t <- rtftable(df)
  cols_of <- function(pages) lapply(pages, function(p) names(p$data))
  expect_identical(cols_of(paginate_cols(t, cut_by = list(2:4, 5:6))),
                   list(c("id", "a", "b", "c"), c("id", "d", "e")))
  expect_identical(cols_of(paginate_cols(t, at = 5, keep = 1)),
                   list(c("id", "a", "b", "c"), c("id", "d", "e")))
  expect_identical(cols_of(paginate_cols(t, at = 5, keep = integer(0))),
                   list(c("id", "a", "b", "c"), c("d", "e")))
  # fit = FALSE: a column keeps its width, so block 2 is narrower
  expect_lt(sum(paginate_cols(t, at = 5, fit = FALSE)[[2]]$col_rel_width),
            sum(paginate_cols(t, at = 5, fit = TRUE)[[2]]$col_rel_width) + 1e-9)
  expect_length(paginate_cols(t, at = 5, order = "down"), 2L)
  # every: a block every 2 of the columns not kept
  expect_identical(cols_of(paginate_cols(t, every = 2)),
                   list(c("id", "a", "b"), c("id", "c", "d"), c("id", "e")))
  # no wider than one block: the table as it is
  expect_length(paginate_cols(t, every = 10), 1L)
  expect_error(paginate_cols(t, at = 3, cut_by = list(4:5)), "not both")
  expect_error(paginate_cols(t, at = 3, every = 2), "one way")
})

test_that("the width helpers: font = NULL is the package's font (#594 1.4)", {
  expect_identical(text_width_in("abc"), text_width_in("abc", font = "courier_new"))
  expect_identical(text_width_in("abc", font = "Courier New"),
                   text_width_in("abc", font = "courier_new"))
  expect_gt(text_width_in("abc", font_size_half_points = 24), text_width_in("abc"))
  old <- options(rtfreporter.font = "Arial")
  on.exit(options(old), add = TRUE)
  expect_identical(text_width_in("abc"), text_width_in("abc", font = "arial"))
  options(old)
  df <- data.frame(USUBJID = c("S-001", "S-002"), AGE = c(45L, 62L))
  expect_identical(auto_col_widths(df), auto_col_widths(data = df))
})

test_that("generate_rtfreport(doc = ); report = still works, by name or with the file by position (#594 1.4)", {
  d <- rtf_tables(rtf_document(), data.frame(a = 1))
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  expect_no_error(generate_rtfreport(doc = d, file_path = f))
  expect_no_error(generate_rtfreport(report = d, file_path = f))
  expect_no_error(generate_rtfreport(report = d, f))
  expect_true(file.exists(f))
})

test_that("rtf_document(file = ): generate_rtfreport(doc) writes there; a file_path given wins", {
  d <- tempfile("doc-file")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  own <- file.path(d, "own.rtf")
  doc <- rtf_tables(rtf_document(file = own), data.frame(a = 1))
  expect_identical(doc$document$file, own)
  expect_identical(generate_rtfreport(doc), own)
  expect_true(file.exists(own))
  other <- file.path(d, "other.rtf")
  expect_identical(generate_rtfreport(doc, other), other)
  expect_identical(generate_rtfreport(doc, file_path = other), other)
  expect_identical(readLines(other), readLines(own))
  # the old forms: report = by name, the file by position
  unlink(own)
  expect_no_error(generate_rtfreport(report = doc))
  expect_true(file.exists(own))
  expect_no_error(generate_rtfreport(report = doc, other))
  # overwrite = FALSE still stops on the document's file
  expect_error(generate_rtfreport(doc, overwrite = FALSE), "already exists")
  # no file anywhere
  expect_error(generate_rtfreport(rtf_tables(rtf_document(), data.frame(a = 1))),
               "rtf_document(file = )", fixed = TRUE)
  expect_error(rtf_document(file = c("a.rtf", "b.rtf")), "single string")
  expect_error(rtf_document(file = ""), "single string")
  expect_error(rtf_document(file = 1), "single string")
  expect_error(generate_rtfreport(doc, file_path = 1), "single string")
  expect_null(rtf_document()$document$file)
  expect_identical(names(formals(rtf_document))[length(formals(rtf_document))], "file")
})

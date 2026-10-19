## tests/testthat/test-api-surface.R
##
## #346: the API-review measure. Deprecated functions still work, so they still
## appear in NAMESPACE, but they are not part of what a reader has to learn --
## they are scheduled for bulk removal before the CRAN submission. The number
## that matters is therefore the export count MINUS the deprecated set, and
## these tests keep both honest.

library(testthat)

.dep <- rtfreporter:::.deprecated_exports

.exports <- function() {
  ns <- asNamespace("rtfreporter")
  sort(getNamespaceExports(ns))
}

# A folder of two small deliverables, for the assembly helpers.
.toc_dir <- function() {
  dir <- file.path(tempdir(), "api-surface-toc")
  if (!dir.exists(dir)) {
    dir.create(dir)
    for (t in c("14.1.1", "14.2.1")) {
      doc <- rtf_document() |>
        rtf_tables(data.frame(Parameter = "Age", Value = "75.1")) |>
        rtf_titles(list(c(paste("Table", t), "Safety Population")))
      generate_rtfreport(doc, file.path(dir, paste0("t", t, ".rtf")))
    }
  }
  dir
}

.reset_deprecation <- function() {
  st <- rtfreporter:::.deprecation_state
  rm(list = ls(st), envir = st)
  # paginate() keeps its own once-a-session flag
  rm(list = ls(rtfreporter:::.paginate_depr_env),
     envir = rtfreporter:::.paginate_depr_env)
}

test_that("every name on the deprecated list is actually exported", {
  expect_true(all(.dep %in% .exports()),
              info = paste(setdiff(.dep, .exports()), collapse = ", "))
})

test_that("every deprecated function still works and warns once", {
  b <- rtf_border(top = TRUE)
  calls <- list(
    rtf_border_top    = function() rtf_border_top(),
    rtf_border_bottom = function() rtf_border_bottom(),
    rtf_border_box    = function() rtf_border_box(),
    rtf_border_none   = function() rtf_border_none(),
    rtf_border_with   = function() rtf_border_with(b, bottom = TRUE),
    rtf_border_tfl    = function() rtf_border_tfl(),
    rtf_table_border  = function() rtf_table_border(header = b),
    # 0.8.2.9013, the pre-CRAN API review (iteration 1)
    rtf_border_side   = function() rtf_border_side(),
    add_col_header_row = function()
      add_col_header_row(rtf_col_header(c("a", "b")), c("A", "B")),
    col_header_from_names = function() col_header_from_names(c("a", "x__b")),
    set_header_cell   = function()
      set_header_cell(rtftable(data.frame(a = 1, b = 2), col_header = c("A", "B")),
                      col_cell(c(1, 2), "AB"), row = 1),
    update_header_row = function()
      update_header_row(rtf_header(c(l = "x")), 2, c(l = "y")),
    update_footer_row = function()
      update_footer_row(rtf_footer(c(l = "x")), 2, c(l = "y")),
    paginate          = function() paginate(data.frame(a = 1)),
    # 0.8.2.9014: the assembly helpers
    assemble_files    = function() assemble_files(.toc_dir()),
    assemble_spec     = function() assemble_spec(.toc_dir()),
    assemble_toc      = function() assemble_toc(spec = assemble_folder(.toc_dir())),
    assemble_from_spec = function()
      assemble_from_spec(assemble_folder(.toc_dir()), tempfile(fileext = ".rtf")),
    toc_heading       = function() toc_heading("A"),
    toc_entry         = function() toc_entry("A", file = "a.rtf"),
    # 0.8.2.9039: the pre-CRAN API review, iteration 0 (#594 O2)
    fmt_count_paren_bare = function() fmt_count_paren_bare(c("1 (5.0%)", "0")),
    realign_count_pct = function() realign_count_pct(c("5 (33.3)", "12 (100.0)")),
    format_count_pct  = function() format_count_pct(5L, 0.05)
  )
  expect_setequal(names(calls), .dep)

  for (nm in names(calls)) {
    .reset_deprecation()
    expect_warning(value <- calls[[nm]](), "deprecated", info = nm)
    expect_false(is.null(value), info = nm)          # still does its job
    expect_silent(calls[[nm]]())                     # ... and only warns once
  }
  .reset_deprecation()
})

test_that("the deprecated spellings still produce the new values", {
  suppressWarnings({
    expect_identical(rtf_border_none(),  rtf_border())
    expect_identical(rtf_border_top(),   rtf_border(top = TRUE))
    expect_identical(rtf_border_bottom(), rtf_border(bottom = TRUE))
    expect_identical(rtf_border_box(),   rtf_border(all = TRUE))
    b <- rtf_border(top = TRUE)
    expect_identical(rtf_border_with(b, bottom = TRUE),
                     rtf_border(top = TRUE, bottom = TRUE))
  })
})

test_that("one border constructor is left once the deprecated ones are set aside", {
  border_api <- grep("^rtf_border|^rtf_table_border$", .exports(), value = TRUE)
  expect_length(border_api, 10L)                      # what NAMESPACE still holds
  # Two, doing different jobs: which edges, and what the line is.
  expect_setequal(setdiff(border_api, .dep),
                  c("rtf_border", "rtf_border_line"))
})

test_that("the effective export count is the reviewed number", {
  # 128 exports, 20 of them deprecated and slated for removal before CRAN.
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
  # since #691 added plan_empty(), the row of a table of no events.)
  expect_length(.exports(), 133L)
  expect_length(setdiff(.exports(), .dep), 110L)
})

test_that("the old argument names still work, with a warning", {
  .reset_deprecation()
  d <- data.frame(a = 1, b = 2)
  expect_warning(t1 <- as_rtftable(gt_obj = d), "deprecated")
  expect_identical(t1, as_rtftable(d))
  .reset_deprecation()
  sp <- list(list(from = 1, to = 2, label = "AB"))
  expect_warning(t2 <- rtftable(d, spanning_header = sp), "deprecated")
  expect_identical(t2$spanning_header, sp)
  expect_silent(rtftable(d, spanning_header = sp))     # once a session
  .reset_deprecation()
})

test_that("rtf_tables()'s table-format arguments warn once each, and still apply (#594)", {
  .reset_deprecation()
  d <- data.frame(a = 1, b = 2)
  expect_warning(doc <- rtf_tables(rtf_document(), d, col_rel_width = c(2, 1)),
                 "rtf_tables(col_rel_width = )` is deprecated", fixed = TRUE)
  expect_identical(doc$contents[[1]]$col_rel_width, c(2, 1))   # as before
  expect_silent(rtf_tables(rtf_document(), d, col_rel_width = c(2, 1)))
  # each its own once
  expect_warning(rtf_tables(rtf_document(), d, border = "tfl"), "border")
  expect_warning(rtf_tables(rtf_document(), d, font = "Arial"), "font")
  expect_warning(rtf_tables(rtf_document(), d, read_gt = TRUE), "as_rtftables")
  # the same settings on the table: no warning, the same page
  expect_silent(doc2 <- rtf_tables(rtf_document(), rtftable(d, col_rel_width = c(2, 1))))
  expect_identical(doc2$contents[[1]]$col_rel_width, c(2, 1))
  # the arguments that are not table-format copies say nothing
  expect_silent(rtf_tables(rtf_document(), d, titles = list("T"), auto_title = FALSE))
  expect_length(rtfreporter:::.rtf_tables_fmt_args, 27L)
  .reset_deprecation()
})

test_that("fmt_count_paren(bare = TRUE) is fmt_count_paren_bare(), byte for byte (#594)", {
  xs <- list(c("1 (1.2%)", "0", "11 (3.6%)", "108 (35.3%)"),
             c("5 (33.3)", "0", "12 (100.0)", NA, ""),
             c("86", "5 (5.8)", "10 (11.6)", "0"),
             c("Mild", "2 (1.0%)", "0"))
  for (x in xs) for (nb in c(" ", "\u00a0")) for (na in c("", "-")) {
    expect_identical(fmt_count_paren(x, nbsp = nb, na = na, bare = TRUE),
                     suppressWarnings(fmt_count_paren_bare(x, nbsp = nb, na = na)))
  }
  # bare = FALSE is fmt_count_paren() as it was
  expect_identical(fmt_count_paren(xs[[1]]), fmt_count_paren(xs[[1]], bare = FALSE))
})

test_that("generate_rtfreport() writes over a file by default (#594)", {
  expect_true(formals(generate_rtfreport)$overwrite)
})

test_that("a deprecated cell-format name still resolves, and says so (#594)", {
  .reset_deprecation()
  x <- c("5 (33.3)", "12 (100.0)")
  expect_warning(f <- rtfreporter:::.builtin_cell_format("realign_count_pct"), "count_pct")
  expect_identical(f(x), rtfreporter:::.realign_count_pct(x))
  expect_silent(rtfreporter:::.builtin_cell_format("count_pct"))
  # "count_paren_bare" stays: fmt_count_paren(bare = TRUE)
  expect_silent(rtfreporter:::.builtin_cell_format("count_paren_bare"))
  expect_identical(suppressWarnings(format_count_pct(5L, 0.05)), fmt_count_pct(5L, 0.05))
  # the align_count_pct switch says nothing
  expect_silent(rtfreporter:::.resolve_align_count_pct(TRUE))
  .reset_deprecation()
})

test_that("a border line saved under the old class name still reads", {
  old <- structure(list(style = "single", width = 15L, color = NULL),
                   class = "rtf_border_side")
  expect_silent(rtf_border(top = old))
})

test_that("the first argument is data; the old names still work, with a warning", {
  d <- data.frame(g = c("A", "A", "B"), v = 1:3)
  expect_identical(names(formals(set_blank_rows))[1], "data")
  expect_identical(names(formals(add_cont_label))[1], "data")
  .reset_deprecation()
  expect_warning(a <- set_blank_rows(df = d, blank_rows = 1L), "deprecated")
  expect_identical(a, set_blank_rows(d, blank_rows = 1L))
  expect_warning(b <- add_cont_label(chunk = d, label = "A"), "deprecated")
  expect_identical(b, add_cont_label(data = d, label = "A"))
  .reset_deprecation()
})

test_that("the NEWS expression finds every deprecated export", {
  news <- system.file("NEWS.md", package = "rtfreporter")
  skip_if(!nzchar(news), "NEWS.md is not installed")
  txt <- readLines(news, warn = FALSE)
  i <- grep("^\\\\b\\(rtf_border_", txt)
  skip_if(!length(i), "the expression is not in this NEWS")
  re <- txt[i[1]]
  for (f in rtfreporter:::.deprecated_exports) {
    expect_true(grepl(re, paste0("x <- ", f, "(1)"), perl = TRUE), info = f)
  }
  expect_false(grepl(re, "paginate_cols(x)", perl = TRUE))
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
  expect_identical(rtf_titles(d, c("T", "U"))$titles, list(c("T", "U"), c("T", "U")))
  expect_identical(rtf_footnotes(d, "F")$footnotes, list("F", "F"))
  d2 <- rtf_tables(rtf_document(), list(data.frame(a = 1), data.frame(a = 2)),
                   titles = c("T", "U"))
  expect_identical(d2$titles, list(c("T", "U"), c("T", "U")))
  expect_error(rtf_titles(d, 1), "character vector")
})

test_that("paginate_cols() takes the plan's names; the old ones still work (#594 1.4)", {
  df <- data.frame(id = 1:2, a = 1, b = 2, c = 3, d = 4, e = 5)
  t <- rtftable(df)
  cols_of <- function(pages) lapply(pages, function(p) names(p$data))
  expect_identical(cols_of(paginate_cols(t, cut_by = list(2:4, 5:6))),
                   cols_of(paginate_cols(t, cols = list(2:4, 5:6))))
  expect_identical(cols_of(paginate_cols(t, at = 5, keep = 1)),
                   cols_of(paginate_cols(t, at = 5, carry = 1)))
  expect_identical(cols_of(paginate_cols(t, at = 5, fit = FALSE)),
                   cols_of(paginate_cols(t, at = 5, width = "keep")))
  expect_identical(paginate_cols(t, at = 5, fit = FALSE)[[2]]$col_rel_width,
                   paginate_cols(t, at = 5, width = "keep")[[2]]$col_rel_width)
  expect_identical(cols_of(paginate_cols(t, at = 5, order = "down")),
                   cols_of(paginate_cols(t, at = 5, page_order = "down")))
  # every: a block every 2 of the columns not kept
  expect_identical(cols_of(paginate_cols(t, every = 2)),
                   list(c("id", "a", "b"), c("id", "c", "d"), c("id", "e")))
  # no wider than one block: the table as it is
  expect_length(paginate_cols(t, every = 10), 1L)
  expect_error(paginate_cols(t, cut_by = list(2:3), cols = list(2:3)), "not both")
  expect_error(paginate_cols(t, at = 3, every = 2), "one way")
})

test_that("the width helpers: font = NULL is the package's font; the older names work (#594 1.4)", {
  expect_identical(text_width_in("abc"), text_width_in("abc", font = "courier_new"))
  expect_identical(text_width_in("abc", font = "Courier New"),
                   text_width_in("abc", font = "courier_new"))
  expect_identical(text_width_in("abc", size_half_points = 24),
                   text_width_in("abc", font_size_half_points = 24))
  old <- options(rtfreporter.font = "Arial")
  on.exit(options(old), add = TRUE)
  expect_identical(text_width_in("abc"), text_width_in("abc", font = "arial"))
  options(old)
  df <- data.frame(USUBJID = c("S-001", "S-002"), AGE = c(45L, 62L))
  expect_identical(auto_col_widths(df = df), auto_col_widths(df))
  expect_identical(auto_col_widths(df, size_half_points = 20),
                   auto_col_widths(data = df, font_size_half_points = 20))
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

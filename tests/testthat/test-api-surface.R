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
  # fmt_count_paren_bare() and realign_count_pct().)
  expect_length(.exports(), 129L)
  expect_length(setdiff(.exports(), .dep), 106L)
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

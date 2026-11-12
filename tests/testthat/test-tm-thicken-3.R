# Thickening of the functional tests, batch 3: the guards, arguments and behaviour of
# the table builders (area TB).  Each block asserts what its FR states: the error or
# warning in full, the control words or values the statement names.  FT ids from 5201 up.

.t3_h <- function() {
  data.frame(SOC = c("Gastro", "Gastro", "Nerv"), PT = c("Nausea", "Vomit", "Headache"),
             N = c("1", "2", "3"), stringsAsFactors = FALSE)
}

# The file a document makes, as lines.
.t3_lines <- function(doc) {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  readLines(f, warn = FALSE)
}

test_that("FT-TB5201 [FR-TB136]: sort_by and sort_desc misuse stops with each message", {
  d <- data.frame(grp = c("a", "a", "b", "b"), ord = 1:4, v = c("1", "2", "3", "4"),
                  stringsAsFactors = FALSE)
  expect_error(as_rtftables(d, sort_by = "zz"),
               "`sort_by` column 'zz' not found in the table.", fixed = TRUE)
  # the body has 3 columns, so position 99 is out of range
  expect_error(as_rtftables(d, sort_by = 99),
               "`sort_by` index 99 out of range (1..3).", fixed = TRUE)
  # two sort columns, three flags
  expect_error(as_rtftables(d, sort_by = c("grp", "ord"), sort_desc = c(TRUE, FALSE, TRUE)),
               "`sort_desc` must have length 1 or 2 (one per `sort_by`).", fixed = TRUE)
  expect_error(as_rtftables(d, sort_by = c("grp", "ord"), sort_desc = NA),
               "`sort_desc` must be TRUE / FALSE.", fixed = TRUE)
  expect_error(as_rtftables(d, sort_by = c("grp", "ord"), sort_desc = c(TRUE, NA)),
               "`sort_desc` must be TRUE / FALSE.", fixed = TRUE)
})

test_that("FT-TB5202 [FR-TB154]: drop_cols naming everything, or an unknown column, stops", {
  d <- data.frame(grp = c("a", "b"), ord = 1:2, v = c("1", "2"), stringsAsFactors = FALSE)
  expect_error(as_rtftables(d, drop_cols = 1:3),
               "`drop_cols` must leave at least one column to display.", fixed = TRUE)
  expect_error(as_rtftables(d, drop_cols = c("grp", "ord", "v")),
               "`drop_cols` must leave at least one column to display.", fixed = TRUE)
  expect_error(as_rtftables(d, drop_cols = "nope"),
               "`drop_cols` column 'nope' not found in the table.", fixed = TRUE)
  expect_error(as_rtftables(d, drop_cols = 99),
               "`drop_cols` index 99 out of range (1..3).", fixed = TRUE)
})

test_that("FT-TB5203 [FR-TB157]: listing on a source or value it cannot take stops with each message", {
  d <- data.frame(USUBJID = c("01", "02"), AETERM = c("Headache", "Nausea"),
                  stringsAsFactors = FALSE)
  ls <- listing_spec(list(listing_col("USUBJID", label = "Subject")))
  # not a listing_spec(): the class is named
  expect_error(as_rtftables(d, listing = "x"),
               "`listing` must be a listing_spec(); got 'character'.", fixed = TRUE)
  expect_error(as_rtftables(d, listing = list()),
               "`listing` must be a listing_spec(); got 'list'.", fixed = TRUE)
  # a body already built by build_listing()
  expect_error(as_rtftables(build_listing(d, ls), listing = ls),
               paste0("`data` was already built by build_listing(), and carries its own listing ",
                      "spec.  Drop the `listing` argument, or pass the unbuilt source data."),
               fixed = TRUE)
  # a source that is not a data frame
  expect_error(as_rtftables(42, listing = ls),
               "`listing` applies to a data.frame or tibble source; got 'numeric'.", fixed = TRUE)
  skip_if_not_installed("rlistings")
  lst <- rlistings::as_listing(data.frame(a = c("x", "y"), b = c("p", "q"),
                                          stringsAsFactors = FALSE), key_cols = "a")
  expect_error(as_rtftables(lst, listing = ls),
               paste0("`listing` does not apply to an rlistings listing (`listing_df`): rlistings ",
                      "has already laid it out.  Drop `listing` and pass it to `as_rtftables()` as ",
                      "it is."), fixed = TRUE)
})

test_that("FT-TB5204 [FR-TB204]: stub_cols() stops on a bad label, indent, label_span or group_summary", {
  h <- .t3_h()
  v <- c("SOC", "PT")
  expect_error(stub_cols(h, vars = v, label = 1),
               "`label` must be NULL or a single string.", fixed = TRUE)
  expect_error(stub_cols(h, vars = v, label = c("a", "b")),
               "`label` must be NULL or a single string.", fixed = TRUE)
  expect_error(stub_cols(h, vars = v, indent = -1),
               "`indent` must be a single non-negative integer.", fixed = TRUE)
  expect_error(stub_cols(h, vars = v, indent = c(1, 2)),
               "`indent` must be a single non-negative integer.", fixed = TRUE)
  expect_error(stub_cols(h, vars = v, label_span = NA),
               "`label_span` must be TRUE or FALSE.", fixed = TRUE)
  expect_error(stub_cols(h, vars = v, label_span = "yes"),
               "`label_span` must be TRUE or FALSE.", fixed = TRUE)
  head <- "`group_summary` must be a subset of c(\"empty\", \"parent\") (or \"all\" / \"none\" / NULL); got: "
  expect_error(stub_cols(h, vars = v, group_summary = "bogus"), paste0(head, "bogus"),
               fixed = TRUE)
  # one good token does not save a bad one
  expect_error(stub_cols(h, vars = v, group_summary = c("empty", "bogus")), paste0(head, "bogus"),
               fixed = TRUE)
})

test_that("FT-TB5205 [FR-TB205]: layout = columns refuses label_span = TRUE and a label; an unknown layout stops", {
  h <- .t3_h()
  v <- c("SOC", "PT")
  expect_error(stub_cols(h, vars = v, layout = "columns", label_span = TRUE),
               paste0("`label_span` applies to layout = \"merged\" only; with layout = ",
                      "\"columns\" the group value stays in its own column."), fixed = TRUE)
  expect_error(stub_cols(h, vars = v, layout = "columns", label = "L"),
               paste0("`label` names the merged stub column, which layout = \"columns\" ",
                      "does not create."), fixed = TRUE)
  expect_error(stub_cols(h, vars = v, layout = "nope"),
               "'arg' should be one of .merged., .columns.")
})

.t3_pages <- function() {
  d <- do.call(rbind, lapply(c("Per 1", "Per 2"), function(per)
    do.call(rbind, lapply(c("1", "2"), function(pg) {
      x <- data.frame(period = per, page = pg, row_grp1 = "SOC",
                      label = c("Mild", "Severe"), stringsAsFactors = FALSE)
      for (a in c("Placebo", "TAK-003")) for (v in paste("Day", 1:2)) x[[paste0(a, "____", v)]] <- "1"
      x
    }))))
  as_rtftables(d, split = "by_value", group_col = "period", page_by = "page",
               drop_cols = c("page", "period"),
               stub = stub_spec(c("row_grp1", "label"), label = "Sev"))
}
.t3_hdr <- function(second = "Placebo\n(N={n_pbo})") {
  rtf_col_header(list(col_cell(1L, ""), col_cell(c(2L, 3L), second),
                      col_cell(c(4L, 5L), "TAK-003\n(N={n_trt})")),
                 c("Sev", rep(paste("Day", 1:2), 2L)))
}
.t3_vals <- function() {
  data.frame(group = c("Per 1", "Per 2"), n_pbo = c(120, 118), n_trt = c(115, 112),
             stringsAsFactors = FALSE)
}

test_that("FT-TB5206 [FR-TB226]: set_col_header(values = ) that does not match the pages stops", {
  # four pages: Per 1 p1, Per 1 p2, Per 2 p1, Per 2 p2.  Only the Per 1 row is given, so the
  # first page without a row is page 3
  e1 <- tryCatch(set_col_header(.t3_pages(), .t3_hdr(), values = .t3_vals()[1L, , drop = FALSE]),
                 error = function(e) conditionMessage(e))
  expect_match(e1, "`values` has no row for page 3 (", fixed = TRUE)
  expect_match(e1, "\n  looked for group = \"Per 2\"\n  values has: Per 1", fixed = TRUE)
  # a third row that no page uses
  e2 <- tryCatch(set_col_header(.t3_pages(), .t3_hdr(),
                                values = rbind(.t3_vals(), data.frame(group = "Per 3", n_pbo = 1,
                                                                      n_trt = 2))),
                 error = function(e) conditionMessage(e))
  expect_identical(e2, paste0("`values` row 3 matched no page: Per 3.\n  Every row must be used -- ",
                              "a row left over is usually a label that does not match the data."))
  # a token {nope} with no value in `values`
  e3 <- tryCatch(set_col_header(.t3_pages(), .t3_hdr("{nope}"), values = .t3_vals()),
                 error = function(e) conditionMessage(e))
  expect_identical(e3, paste0("set_col_header(values = ): no value for `{nope}`.\n  Supply it in ",
                              "`values`, or write `{{` for a literal brace.\n  values has: n_pbo, n_trt"))
})

test_that("FT-TB5207 [FR-TB424]: style_cols() on a bad page list stops, an unknown argument warns", {
  t1 <- rtftable(data.frame(a = "x", b = "y", stringsAsFactors = FALSE))
  expect_error(style_cols(list(t1, data.frame(a = 1)), cols = 1, bold = TRUE),
               paste0("`style_cols()` on a list expects every element to be an rtftable (as ",
                      "returned by as_rtftables()); element 2 is 'data.frame'."), fixed = TRUE)
  w <- testthat::capture_warnings(style_cols(t1, cols = 1, alignn = "right"))
  expect_length(w, 1L)
  expect_match(w, "`style_cols()`: unknown argument `alignn` (did you mean `align`?) ignored.",
               fixed = TRUE)
})

test_that("FT-TB5208 [FR-TB524]: a column-name pos that is empty, unknown or ambiguous stops", {
  expect_error(col_cell(""), "`pos` column names must be non-empty strings.", fixed = TRUE)
  expect_error(col_cell(c("a", "")), "`pos` column names must be non-empty strings.", fixed = TRUE)
  d <- data.frame(a = "x", b = "y", stringsAsFactors = FALSE)
  expect_error(rtftable(d, col_header = rtf_col_header(list(col_cell("zzz", "Z")))),
               "`pos` column name \"zzz\" not found in data columns. Available: \"a\", \"b\".",
               fixed = TRUE)
  # two columns both named x
  dd <- data.frame(x = 1, x = 2, check.names = FALSE)
  expect_error(rtftable(dd, col_header = rtf_col_header(list(col_cell("x", "Z")))),
               "`pos` column name \"x\" is ambiguous (matches 2 columns).", fixed = TRUE)
})

test_that("FT-TB5209 [FR-TB603]: as_rtftables(first_rows = ) that is not one whole number >= 1 stops", {
  d <- data.frame(Item = c("a", "b"), V = c("1", "2"), stringsAsFactors = FALSE)
  msg <- "as_rtftables(): `first_rows` is a whole number of rows, 1 or more."
  for (bad in list(0, -1, 1.5, NA, Inf, "2", TRUE, c(1, 2), numeric(0))) {
    expect_error(as_rtftables(d, first_rows = bad), msg, fixed = TRUE)
  }
  # rtf_tables() stops alike
  expect_error(rtf_tables(rtf_document(), d, page_break = FALSE, first_rows = Inf),
               "rtf_tables(): `first_rows` is a whole number of rows, 1 or more.", fixed = TRUE)
  # 1 is the smallest value allowed
  expect_s3_class(as_rtftables(d, first_rows = 1)[[1L]], "rtftable")
})

test_that("FT-TB5210 [FR-TB605]: indent and group_summary of a folding stub stop in every entry point", {
  h <- .t3_h()
  v <- c("SOC", "PT")
  ie <- "`indent` must be a single non-negative integer."
  head <- "`group_summary` must be a subset of c(\"empty\", \"parent\") (or \"all\" / \"none\" / NULL); got: "
  st <- data.frame(ARM = rep(c("Placebo", "Active"), each = 3),
                   variable = rep(c("AGE", "SEX", "SEX"), 2),
                   .label = rep(c(NA, "F", "M"), 2),
                   .kind = rep(c("continuous", "categorical", "categorical"), 2),
                   stat_name = rep(c("mean", "n", "n"), 2), stat = c(61.246, 12, 8, 63.51, 9, 11))
  pl <- table_plan(st, cols = "ARM", rows = c(group = "variable")) |>
    plan_cells(continuous = c(Mean = "{mean}"), categorical = "{n}") |>
    plan_labels(AGE = "Age", SEX = "Sex")
  for (bad in list(1.5, "2", TRUE)) {
    expect_error(stub_cols(h, vars = v, indent = bad), ie, fixed = TRUE)
    expect_error(as_rtftables(h, stub = stub_spec(v, indent = bad)), ie, fixed = TRUE)
    expect_error(plan_apply(plan_stub(pl, name = "C", indent = bad), "pages"), ie, fixed = TRUE)
  }
  # NA, "" and a non-character vector were dropped, turning the folding off: now they stop
  for (case in list(list(NA, "a logical"), list("", "\"\""), list(1, "a numeric"),
                    list(list("empty"), "a list"))) {
    expect_error(stub_cols(h, vars = v, group_summary = case[[1L]]), paste0(head, case[[2L]]),
                 fixed = TRUE)
    expect_error(as_rtftables(h, stub = stub_spec(v, group_summary = case[[1L]])),
                 paste0(head, case[[2L]]), fixed = TRUE)
    expect_error(plan_apply(plan_stub(pl, name = "C", group_summary = case[[1L]]), "pages"),
                 paste0(head, case[[2L]]), fixed = TRUE)
  }
  # a whole number is accepted
  expect_s3_class(stub_cols(h, vars = v, indent = 2), "data.frame")
})

test_that("FT-TB5211 [FR-TB606]: a column position that is TRUE, a fraction or a factor stops; NA is out of range", {
  h <- .t3_h()
  wide <- data.frame(P = "ALT", D1 = "1", D8 = "2", D15 = "3", stringsAsFactors = FALSE)
  num <- data.frame(g = c("a", "b"), v = c(1.234, 2.5), stringsAsFactors = FALSE)
  t1 <- rtftable(h)
  one <- function(arg, shown, n) {
    paste0("`", arg, "` must be a column name or a whole position (1..", n, "), not ", shown, ".")
  }
  # the six as_rtftables() arguments and stub_cols(vars); a table of 3 columns
  calls <- list(
    list("stub(vars)", function(x) as_rtftables(h, stub = x)),
    list("group_col", function(x) as_rtftables(h, split = "group_safe", group_col = x, max_rows = 2)),
    list("collapse_repeats", function(x) as_rtftables(h, collapse_repeats = x)),
    list("sort_by", function(x) as_rtftables(h, sort_by = x)),
    list("drop_cols", function(x) as_rtftables(h, drop_cols = x)),
    list("page_by", function(x) as_rtftables(h, split = "by_value", page_by = x)),
    list("vars", function(x) stub_cols(h, vars = x)))
  for (cl in calls) {
    expect_error(cl[[2L]](TRUE), one(cl[[1L]], "TRUE", 3L), fixed = TRUE)
    expect_error(cl[[2L]](1.5), one(cl[[1L]], "1.5", 3L), fixed = TRUE)
    fmsg <- tryCatch(cl[[2L]](factor("SOC")), error = function(e) conditionMessage(e))
    expect_true(startsWith(fmsg, paste0("`", cl[[1L]], "` must be ")))
    expect_match(fmsg, "column names? or (a )?whole positions? \\(1\\.\\.3\\)")
    expect_error(cl[[2L]](NA), paste0("`", cl[[1L]], "` index NA out of range (1..3)."), fixed = TRUE)
  }
  # paginate_cols(at, carry, cut_by), fmt_numeric(cols, by), the style verbs' cols
  pages <- as_rtftables(wide)
  expect_error(paginate_cols(pages, at = TRUE), one("paginate_cols(at)", "TRUE", 4L), fixed = TRUE)
  expect_error(paginate_cols(pages, at = 2.7), one("paginate_cols(at)", "2.7", 4L), fixed = TRUE)
  expect_error(paginate_cols(pages, at = 3, keep = 1.5), one("paginate_cols(carry)", "1.5", 4L),
               fixed = TRUE)
  expect_error(paginate_cols(pages, at = 3, keep = TRUE), one("paginate_cols(carry)", "TRUE", 4L),
               fixed = TRUE)
  expect_error(paginate_cols(pages, cut_by = list(1.5)), one("paginate_cols(cut_by)", "1.5", 4L),
               fixed = TRUE)
  expect_error(paginate_cols(pages, cut_by = list(TRUE)), one("paginate_cols(cut_by)", "TRUE", 4L),
               fixed = TRUE)
  expect_error(fmt_numeric(num, cols = 1.5), one("fmt_numeric(cols)", "1.5", 2L), fixed = TRUE)
  expect_error(fmt_numeric(num, cols = TRUE), one("fmt_numeric(cols)", "TRUE", 2L), fixed = TRUE)
  expect_error(fmt_numeric(num, cols = 2, by = TRUE, formats = list(.default = 1)),
               one("fmt_numeric(by)", "TRUE", 2L), fixed = TRUE)
  expect_error(fmt_numeric(num, cols = 2, by = 1.5, formats = list(.default = 1)),
               one("fmt_numeric(by)", "1.5", 2L), fixed = TRUE)
  for (verb in c("style_cols", "style_body", "style_header", "set_decimal_split")) {
    f <- get(verb)
    arg <- paste0(verb, "(cols)")
    expect_error(f(t1, cols = TRUE), one(arg, "TRUE", 3L), fixed = TRUE)
    expect_error(f(t1, cols = 1.5), one(arg, "1.5", 3L), fixed = TRUE)
    expect_error(f(t1, cols = NA), paste0("`", arg, "` index NA out of range (1..3)."),
                 fixed = TRUE)
  }
  # whole positions, integer or double, are accepted
  expect_identical(as_rtftables(h, stub = c(1, 2))[[1L]]$data,
                   as_rtftables(h, stub = 1:2)[[1L]]$data)
})

test_that("FT-TB5212 [FR-TB108]: as_rtftable(...) passes arguments on to rtftable() and as_rtftables()", {
  d <- data.frame(g = c("b", "a", "c"), v = c("1", "2", "3"), stringsAsFactors = FALSE)
  # on to as_rtftables(): the rows are sorted by g, and a missing value is printed as "-"
  t1 <- as_rtftable(d, sort_by = "g")
  expect_identical(t1$data$g, c("a", "b", "c"))
  expect_identical(t1$data$v, c("2", "1", "3"))
  t2 <- as_rtftable(data.frame(g = c("a", NA), v = c("1", "2"), stringsAsFactors = FALSE), na = "-")
  expect_identical(t2$data$g, c("a", "-"))
  # on to rtftable(): the relative widths
  t3 <- as_rtftable(d, col_rel_width = c(2, 1))
  expect_identical(t3$col_rel_width, c(2, 1))
  # an explicit value wins over the one the source gives: the label attribute of g is read as
  # the header unless col_header is given
  lab <- d
  attr(lab$g, "label") <- "Group"
  expect_identical(as_rtftable(lab)$col_header, list(c("Group", "v")))
  expect_identical(as_rtftable(lab, col_header = c("P", "Q"))$col_header, list(c("P", "Q")))
})

test_that("FT-TB5213 [FR-TB134]: sort_by orders the rows by the keys in the input body's columns, stably, NA last", {
  d <- data.frame(g = c("b", "a", NA, "a", "b"), n = c(2, 1, 9, 3, 1), s = c("x", "y", "z", "w", "v"),
                  stringsAsFactors = FALSE)
  body <- function(...) as_rtftables(d, ...)[[1L]]$data
  # NULL (the default): input order kept
  expect_identical(body()$s, c("x", "y", "z", "w", "v"))
  expect_identical(body(sort_by = NULL)$s, c("x", "y", "z", "w", "v"))
  # by g: a (y, w), b (x, v) -- ties keep the input order -- and the NA last
  expect_identical(body(sort_by = "g")$s, c("y", "w", "x", "v", "z"))
  # by position 2 (n): 1 (y, v), 2 (x), 3 (w), 9 (z): the two 1s keep the input order
  expect_identical(body(sort_by = 2)$s, c("y", "v", "x", "w", "z"))
  # a list mixing a name and a position: g, then n -> a1 a3 b1 b2 NA
  expect_identical(body(sort_by = list("g", 2))$s, c("y", "w", "v", "x", "z"))
  expect_identical(body(sort_by = c("g", "n"))$s, c("y", "w", "v", "x", "z"))
  # the sort is on the input body's coordinates: with g dropped, 3 still means s
  r <- body(sort_by = 3, drop_cols = 1)
  expect_identical(names(r), c("n", "s"))
  expect_identical(r$s, c("v", "w", "x", "y", "z"))
  # the order is made before the pagination: sorted by n the rows are y v | x w z
  p <- as_rtftables(d, sort_by = "n", split = "rows", split_rows = 3)
  expect_identical(p[[1L]]$data$s, c("y", "v"))
  expect_identical(p[[2L]]$data$s, c("x", "w", "z"))
})

test_that("FT-TB5214 [FR-TB135]: sort_desc is ascending by default, one TRUE reverses all keys, a vector one per key", {
  d <- data.frame(g = c("b", "a", NA, "a", "b"), n = c(2, 1, 9, 3, 1), s = c("x", "y", "z", "w", "v"),
                  stringsAsFactors = FALSE)
  body <- function(...) as_rtftables(d, ...)[[1L]]$data
  asc <- c("y", "w", "x", "v", "z")
  expect_identical(body(sort_by = "g")$s, asc)
  expect_identical(body(sort_by = "g", sort_desc = NULL)$s, asc)
  expect_identical(body(sort_by = "g", sort_desc = FALSE)$s, asc)
  # one TRUE: b (x, v, input order), a (y, w), the NA still last
  expect_identical(body(sort_by = "g", sort_desc = TRUE)$s, c("x", "v", "y", "w", "z"))
  # all keys descending: g descending, n descending inside: b2 b1 a3 a1 NA
  expect_identical(body(sort_by = c("g", "n"), sort_desc = TRUE)$s, c("x", "v", "w", "y", "z"))
  # one direction per key: g ascending, n descending: a3 a1 b2 b1 NA
  expect_identical(body(sort_by = c("g", "n"), sort_desc = c(FALSE, TRUE))$s,
                   c("w", "y", "x", "v", "z"))
  expect_identical(body(sort_by = c("g", "n"), sort_desc = c(TRUE, FALSE))$s,
                   c("v", "x", "y", "w", "z"))
  # ignored when sort_by is NULL
  expect_identical(body(sort_desc = TRUE)$s, c("x", "y", "z", "w", "v"))
})

test_that("FT-TB5215 [FR-TB139]: blank_rows takes positions, between_groups, a spec object or a list of them", {
  d <- data.frame(g = c("a", "a", "b", "b", "c"), v = as.character(1:5), stringsAsFactors = FALSE)
  br <- function(...) as_rtftables(d, ...)[[1L]]$blank_rows
  expect_null(br())
  expect_null(br(blank_rows = NULL))
  # a blank after each given row; 0 before the first row; -1 after the last row (row 5)
  expect_identical(br(blank_rows = c(2, 4)), c(2L, 4L))
  expect_identical(br(blank_rows = 0), 0L)
  expect_identical(br(blank_rows = -1), 5L)
  # a blank at each group transition of g: after rows 2 (a to b) and 4 (b to c)
  expect_identical(br(blank_rows = "between_groups", group_col = "g"), c(2L, 4L))
  expect_identical(br(blank_rows = blank_rows_by_change("g")), c(2L, 4L))
  # the rule spec, and a list combining positions and a spec: the union, sorted, no duplicates
  rule <- blank_rows_by_rule("v", "^[35]$")
  expect_identical(br(blank_rows = list(1, rule)), sort(union(1L, br(blank_rows = rule))))
  expect_identical(br(blank_rows = list(2, blank_rows_by_change("g"))), c(2L, 4L))
  # an rtf_blank_rows attribute already on the input is still honoured
  x <- d
  attr(x, "rtf_blank_rows") <- 3L
  expect_identical(as_rtftables(x)[[1L]]$blank_rows, 3L)
})

test_that("FT-TB5216 [FR-TB143]: align_count_pct is off by default, TRUE is the count_pct format, a name or function acts as cell_format", {
  cn <- data.frame(l = c("a", "b"), v = c("5 (10.0)", "12 (50.0)"), stringsAsFactors = FALSE)
  v <- function(...) as_rtftables(cn, ...)[[1L]]$data$v
  expect_identical(v(), c("5 (10.0)", "12 (50.0)"))
  expect_identical(v(align_count_pct = FALSE), c("5 (10.0)", "12 (50.0)"))
  # TRUE: the built-in count_pct format, the cells padded to one width (10 characters here)
  aligned <- v(align_count_pct = TRUE)
  expect_identical(aligned, v(cell_format = "count_pct"))
  expect_identical(nchar(aligned), c(10L, 10L))
  # the name of a built-in behaves as cell_format
  expect_identical(v(align_count_pct = "count_pct"), aligned)
  expect_identical(v(align_count_pct = "right_align"), v(cell_format = "right_align"))
  # a function behaves as cell_format
  f <- function(x) paste0("<", x, ">")
  expect_identical(v(align_count_pct = f), c("<5 (10.0)>", "<12 (50.0)>"))
  expect_identical(v(align_count_pct = f), v(cell_format = f))
  # ignored when cell_format is given
  expect_identical(v(align_count_pct = TRUE, cell_format = f), c("<5 (10.0)>", "<12 (50.0)>"))
})

test_that("FT-TB5217 [FR-TB145]: cell_format re-formats the data columns, or each column of a list, before pagination", {
  c2 <- data.frame(l = c("a", "bb"), x = c("5", "10"), y = c("1 (2)", "33 (4)"),
                   stringsAsFactors = FALSE)
  nb <- " "
  body <- function(...) as_rtftables(c2, ...)[[1L]]$data
  expect_identical(body()$x, c("5", "10"))
  f <- function(x) paste0("<", x, ">")
  # one formatter: every data column, the row-label column 1 left alone
  r <- body(cell_format = f)
  expect_identical(r$l, c("a", "bb"))
  expect_identical(r$x, c("<5>", "<10>"))
  expect_identical(r$y, c("<1 (2)>", "<33 (4)>"))
  # a list is taken positionally: NULL leaves column 1, f column 2, a built-in name column 3
  r <- body(cell_format = list(NULL, f, "right_align"))
  expect_identical(r$l, c("a", "bb"))
  expect_identical(r$x, c("<5>", "<10>"))
  expect_identical(r$y, c(paste0(nb, "1", nb, "(2)"), paste0("33", nb, "(4)")))
  # the list may format column 1 too
  expect_identical(body(cell_format = list(toupper))$l, c("A", "BB"))
  # the built-in names, the fmt_ prefix optional; right_align pads 5 to the width of 10
  expect_identical(body(cell_format = "right_align")$x, c(paste0(nb, "5"), "10"))
  expect_identical(body(cell_format = "fmt_right_align"), body(cell_format = "right_align"))
  expect_identical(body(cell_format = "count_paren")$x, c("5", "10"))
  expect_identical(body(cell_format = "count_paren")$y, c(paste0(nb, "1", nb, "(2)"), paste0("33", nb, "(4)")))
  expect_identical(body(cell_format = "count_paren_bare")$x, c(paste0(nb, "5"), "10"))
  expect_identical(body(cell_format = "value_paren"), body(cell_format = fmt_value_paren))
  expect_identical(body(cell_format = "count_pct"), body(cell_format = "fmt_count_pct"))
  expect_true(all(nchar(body(cell_format = "count_pct")$y) == nchar(body(cell_format = "count_pct")$y[1L])))
  # before pagination: the pad is the width of the widest cell of the whole column
  p <- as_rtftables(c2, cell_format = "right_align", split = "rows", split_rows = 2)
  expect_identical(p[[1L]]$data$x, paste0(nb, "5"))
  expect_identical(p[[2L]]$data$x, "10")
})

test_that("FT-TB5218 [FR-TB147]: na is the text printed for a missing value in every column; empty leaves the cell empty", {
  n <- data.frame(l = c("a", NA), v = c("1 (2.0)", NA), w = c(NA, "22 (3.0)"), stringsAsFactors = FALSE)
  nb <- " "
  # a missing cell stays NA when na is not given
  expect_true(is.na(as_rtftables(n)[[1L]]$data$l[2L]))
  r <- as_rtftables(n, na = "n/a")[[1L]]$data
  expect_identical(r$l, c("a", "n/a"))
  expect_identical(r$v, c("1 (2.0)", "n/a"))
  expect_identical(r$w, c("n/a", "22 (3.0)"))
  # align_count_pct or a cell_format only decide whether the token is aligned with the counts
  a <- as_rtftables(n, na = "n/a", align_count_pct = TRUE)[[1L]]$data
  expect_identical(a$l, c("a", "n/a"))
  expect_true(grepl("n/a", a$v[2L], fixed = TRUE))
  expect_true(grepl("n/a", a$w[1L], fixed = TRUE))
  expect_identical(nchar(a$v[1L]), nchar(a$v[2L]))
  ra <- as_rtftables(n, na = "n/a", cell_format = "right_align")[[1L]]$data
  expect_identical(ra$l, c("a", "n/a"))
  expect_identical(trimws(gsub(nb, " ", ra$v, fixed = TRUE)), c("1 (2.0)", "n/a"))
  # "" (the default) leaves the cell empty: the cell stays NA in the data and the file
  # prints nothing for it -- no "NA" text and no "n/a" -- in the label and data columns
  e <- as_rtftables(n, na = "")
  expect_true(is.na(e[[1L]]$data$l[2L]) && is.na(e[[1L]]$data$v[2L]) && is.na(e[[1L]]$data$w[1L]))
  empty <- .t3_lines(rtf_document() |> rtf_tables(e))
  expect_false(any(grepl("NA|n/a", empty)))
  expect_true(any(grepl("22 (3.0)", empty, fixed = TRUE)))
  shown <- .t3_lines(rtf_document() |> rtf_tables(as_rtftables(n, na = "n/a")))
  # three cells hold the token (the label and one data cell of row 2, one data cell of row 1)
  expect_identical(sum(lengths(regmatches(shown, gregexpr("n/a", shown, fixed = TRUE)))), 3L)
})

test_that("FT-TB5219 [FR-TB152]: collapse_repeats blanks consecutive repeats per page, later columns on the combination", {
  d <- data.frame(g = c("a", "a", "a", "b", "b"), h = c("x", "x", "y", "y", "y"), v = as.character(1:5),
                  stringsAsFactors = FALSE)
  cr <- function(...) as_rtftables(d, ...)[[1L]]$data
  expect_identical(cr()$g, d$g)
  # g alone: the first value of a run is kept, the rest are NA
  expect_identical(cr(collapse_repeats = "g")$g, c("a", NA, NA, "b", NA))
  expect_identical(cr(collapse_repeats = "g")$h, d$h)
  # g then h: h is collapsed on the pair (g, h): (a,x) (a,x) (a,y) (b,y) (b,y) -> x NA y y NA
  r <- cr(collapse_repeats = c("g", "h"))
  expect_identical(r$g, c("a", NA, NA, "b", NA))
  expect_identical(r$h, c("x", NA, "y", "y", NA))
  # priority order: h first, then g on the pair (h, g): h = x NA y NA NA; g = a NA a b NA
  r <- cr(collapse_repeats = c("h", "g"))
  expect_identical(r$h, c("x", NA, "y", NA, NA))
  expect_identical(r$g, c("a", NA, "a", "b", NA))
  # a position names the column too
  expect_identical(cr(collapse_repeats = 2)$h, c("x", NA, "y", NA, NA))
  # per page, after the split: the run a, a, a is cut after row 2, so row 3 opens page 2 with "a"
  p <- as_rtftables(d, collapse_repeats = "g", split = "rows", split_rows = 3)
  expect_identical(p[[1L]]$data$g, c("a", NA))
  expect_identical(p[[2L]]$data$g, c("a", "b", NA))
})

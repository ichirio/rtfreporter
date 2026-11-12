# Thickening of the functional tests, batch 9: area TB, FR-TB102 to FR-TB370.
# Each block asserts what its FR states, the figures worked out in a comment.
# FT ids TB5901 up.  Fictional data only.

# The first cell of every body row of a rendered table, "<blank>" for a blank
# row (one empty cell across the table); the header row is dropped.
.t9_rows <- function(tb, width = 9360L) {
  r <- .render_rtftable(tb, width)[-1L]
  vapply(r, function(l) {
    n <- lengths(regmatches(l, gregexpr("\\cellx", l, fixed = TRUE)))
    if (n == 1L && grepl(" \\cell\\row", l, fixed = TRUE)) return("<blank>")
    sub("^.*?\\\\ri[0-9]+ ([^\\\\]*)\\\\cell.*$", "\\1", l, perl = TRUE)
  }, character(1L), USE.NAMES = FALSE)
}

.t9_blank_df <- function() {
  data.frame(g = c("A", "A", "B", "B", "C"), v = 1:5, stringsAsFactors = FALSE)
}

# ---- blank rows ----------------------------------------------------------------

test_that("FT-TB5901 [FR-TB301]: set_blank_rows() stores sorted unique integer positions (0 before the first row, k after row k) and leaves the data", {
  df <- .t9_blank_df()
  # c(4, 2, 2) and 0 and the change spec (changes after rows 2 and 4, no edges)
  # are unioned: 0, 2, 4
  spec <- blank_rows_by_change("g", include_before_first = FALSE, include_after_last = FALSE)
  out <- set_blank_rows(df, blank_rows = list(c(4, 2, 2), 0, spec))
  pos <- attr(out, "rtf_blank_rows")
  expect_identical(pos, c(0L, 2L, 4L))
  expect_type(pos, "integer")
  expect_false(is.unsorted(pos))
  expect_identical(anyDuplicated(pos), 0L)
  # doubles come back as integers, sorted
  expect_identical(attr(set_blank_rows(df, blank_rows = c(5, 3)), "rtf_blank_rows"), c(3L, 5L))
  # the data itself is not changed
  plain <- out
  attr(plain, "rtf_blank_rows") <- NULL
  expect_identical(plain, df)
  expect_identical(nrow(out), 5L)
  # 0 is a blank row before the first row, k one after row k
  expect_identical(.t9_rows(rtftable(out)),
                   c("<blank>", "A", "A", "<blank>", "B", "B", "<blank>", "C"))
  expect_identical(rtftable(out)$blank_rows, c(0L, 2L, 4L))
})

test_that("FT-TB5902 [FR-TB306]: set_blank_rows() drops NA and positions above nrow() without a warning; -1 is after the last row, as in rtftable()", {
  df <- .t9_blank_df()
  expect_no_warning(out <- set_blank_rows(df, blank_rows = c(NA, 99, -1, 2)))
  expect_identical(attr(out, "rtf_blank_rows"), c(2L, 5L))     # 99 and NA gone, -1 = 5
  expect_identical(attr(out, "rtf_blank_rows"), rtftable(df, blank_rows = c(-1, 2))$blank_rows)
  expect_identical(.t9_rows(rtftable(out)), c("A", "A", "<blank>", "B", "B", "C", "<blank>"))
  # a position above nrow() alone leaves the attribute absent
  expect_no_warning(only <- set_blank_rows(df, blank_rows = c(6, NA)))
  expect_null(attr(only, "rtf_blank_rows"))
  # in a list as well
  expect_no_warning(l <- set_blank_rows(df, blank_rows = list(c(NA, 7), -1)))
  expect_identical(attr(l, "rtf_blank_rows"), 5L)
})

test_that("FT-TB5903 [FR-TB308]: set_blank_rows(blank_rows = ) resolves a by_change and a by_rule spec on this data frame, each page of as_rtftables() on its own rows", {
  d <- data.frame(lab = c("a", "Total", "b", "c", "Total", "d"), v = 1:6,
                  stringsAsFactors = FALSE)
  # "^Total" is row 2 and row 5: a blank before each is after rows 1 and 4
  expect_identical(attr(set_blank_rows(d, blank_rows = blank_rows_by_rule("lab", "^Total")),
                        "rtf_blank_rows"), c(1L, 4L))
  expect_identical(attr(set_blank_rows(d, blank_rows = blank_rows_by_rule("lab", "^Total", "after")),
                        "rtf_blank_rows"), c(2L, 5L))
  # by_change on this frame: a, Total, b, c, Total, d all differ -> after rows 1..5
  expect_identical(attr(set_blank_rows(d, blank_rows = blank_rows_by_change("lab")),
                        "rtf_blank_rows"), 1:5)
  # two pages of 4 and 2 rows: the rule is resolved on each page's own rows
  # page 1 (a, Total, b, c): "Total" is row 2 -> 1; page 2 (Total, d): row 1 -> 0 is
  # the page edge and a rule is a separator, so nothing
  pg <- as_rtftables(d, blank_rows = blank_rows_by_rule("lab", "^Total"),
                     split = "rows", split_rows = 5)
  expect_length(pg, 2L)
  expect_identical(pg[[1L]]$data$lab, c("a", "Total", "b", "c"))
  expect_identical(pg[[1L]]$blank_rows, 1L)
  expect_identical(pg[[2L]]$data$lab, c("Total", "d"))
  expect_length(pg[[2L]]$blank_rows, 0L)
  # after: page 1 -> 2, page 2 (Total is row 1) -> 1
  pa <- as_rtftables(d, blank_rows = blank_rows_by_rule("lab", "^Total", "after"),
                     split = "rows", split_rows = 5)
  expect_identical(pa[[1L]]$blank_rows, 2L)
  expect_identical(pa[[2L]]$blank_rows, 1L)
  # by_change per page (split_rows = 4 starts page 2 at row 4): A A B | B C:
  # page 1 changes after its row 2, page 2 after its row 1
  df <- .t9_blank_df()
  pc <- as_rtftables(df, blank_rows = blank_rows_by_change("g"), split = "rows", split_rows = 4)
  expect_identical(pc[[1L]]$data$g, c("A", "A", "B"))
  expect_identical(pc[[1L]]$blank_rows, 2L)
  expect_identical(pc[[2L]]$data$g, c("B", "C"))
  expect_identical(pc[[2L]]$blank_rows, 1L)
})

test_that("FT-TB5904 [FR-TB309]: set_blank_rows(blank_rows = list(...)) unions the positions of every form and drops duplicates", {
  df <- .t9_blank_df()
  one <- function(s) attr(set_blank_rows(df, blank_rows = s), "rtf_blank_rows")
  pos <- c(0, 1, 2)                                   # positions
  grp <- "between_groups"                             # column 1 changes after rows 2, 4
  chg <- blank_rows_by_change("g")                    # 2, 4 (edges are not kept on a page)
  rul <- blank_rows_by_rule("g", "^C")                # a blank before row 5: after row 4
  expect_identical(one(pos), 0:2)
  expect_identical(one(grp), c(2L, 4L))
  expect_identical(one(chg), c(2L, 4L))
  expect_identical(one(rul), 4L)
  # each pair, then all of them: the union, every position once, sorted
  expect_identical(one(list(pos, grp)), c(0L, 1L, 2L, 4L))
  expect_identical(one(list(grp, chg)), c(2L, 4L))                     # duplicates dropped
  expect_identical(one(list(chg, rul)), c(2L, 4L))
  expect_identical(one(list(pos, grp, chg, rul)), c(0L, 1L, 2L, 4L))
  expect_identical(one(list(rul, chg, grp, pos, c(2, 4))), c(0L, 1L, 2L, 4L))
  expect_identical(one(list(5, 5, 5)), 5L)
  # the same union on each page of as_rtftables()
  # (pages A A B | B C): page 1 gets 1 and the change after row 2; page 2 gets 1
  # and the change after its row 1, which is also where "^C" puts its blank: once
  pg <- as_rtftables(df, blank_rows = list(1, grp, rul), split = "rows", split_rows = 4)
  expect_identical(pg[[1L]]$blank_rows, c(1L, 2L))
  expect_identical(pg[[2L]]$blank_rows, 1L)
})

test_that("FT-TB5905 [FR-TB315]: set_blank_rows(group_by = ) is auto by default, or indent, value, filled, as the pagination splits detect groups; any other value is refused", {
  expect_identical(eval(formals(set_blank_rows)$group_by),
                   c("auto", "indent", "value", "filled"))
  d4 <- data.frame(g = c("A", "", "", "B", "", "C"), v = 1:6, stringsAsFactors = FALSE)
  at4 <- function(...) attr(set_blank_rows(d4, "between_groups", ...), "rtf_blank_rows")
  # filled: a non-empty cell starts a group (rows 1, 4, 6): blanks after rows 3, 5
  expect_identical(at4(group_by = "filled"), c(3L, 5L))
  # value: every change of value (A "" "" B "" C): after rows 1, 3, 4, 5
  expect_identical(at4(group_by = "value"), c(1L, 3L, 4L, 5L))
  expect_identical(at4(), at4(group_by = "filled"))             # auto picks filled here
  expect_identical(at4(group_by = "auto"), c(3L, 5L))
  d5 <- data.frame(g = c("P", "  a", "  b", "Q", "  c"), v = 1:5, stringsAsFactors = FALSE)
  at5 <- function(...) attr(set_blank_rows(d5, "between_groups", ...), "rtf_blank_rows")
  expect_identical(at5(group_by = "indent"), 3L)               # P a b | Q c
  expect_identical(at5(), 3L)                                  # auto picks indent here
  expect_identical(at5(group_by = "value"), 1:4)
  expect_error(at5(group_by = "bogus"), "should be one of .auto., .indent., .value., .filled.")
  expect_error(at5(group_by = c("indent", "value")), "must be of length 1")
  # the pagination splits cut at the same group starts: indent, 3 rows a page
  pg <- as_rtftables(d5, split = "group_safe", max_rows = 3, group_by = "indent")
  expect_identical(lengths(lapply(pg, function(p) p$data$g)), c(3L, 2L))   # P a b | Q c
  expect_identical(cumsum(lengths(lapply(pg, function(p) p$data$g)))[1L], at5())
  # filled, 4 rows a page: groups A "" "" | B "" | C -> pages of 3 and 3
  pf <- as_rtftables(d4, split = "group_safe", max_rows = 4, group_by = "filled")
  expect_identical(lengths(lapply(pf, function(p) p$data$g)), c(3L, 3L))
  expect_identical(cumsum(lengths(lapply(pf, function(p) p$data$g)))[1L], at4(group_by = "filled")[1L])
})

test_that("FT-TB5906 [FR-TB317]: blank_rows_by_change() holds cols, group_by, include_before_first and include_after_last and inserts nothing; the table that receives it does", {
  spec <- blank_rows_by_change(c("g", "v"))
  expect_s3_class(spec, "rtf_blank_rows_by_change")
  expect_identical(names(unclass(spec)),
                   c("cols", "group_by", "include_before_first", "include_after_last"))
  expect_identical(spec$cols, c("g", "v"))
  expect_identical(spec$group_by, "value")                    # the defaults
  expect_true(spec$include_before_first)
  expect_true(spec$include_after_last)
  s2 <- blank_rows_by_change("g", "indent", FALSE, FALSE)
  expect_identical(unclass(s2), list(cols = "g", group_by = "indent",
                                     include_before_first = FALSE, include_after_last = FALSE))
  expect_error(blank_rows_by_change("g", group_by = "bogus"), "should be one of")
  expect_error(blank_rows_by_change(1), "`cols` must be a non-empty character vector.", fixed = TRUE)
  # holding it inserts nothing: the table it is given to does
  df <- .t9_blank_df()
  expect_null(attr(df, "rtf_blank_rows"))
  g <- blank_rows_by_change("g")
  expect_identical(rtftable(df, blank_rows = g)$blank_rows, c(0L, 2L, 4L, 5L))
  expect_identical(.t9_rows(rtftable(df, blank_rows = g)),
                   c("<blank>", "A", "A", "<blank>", "B", "B", "<blank>", "C", "<blank>"))
  expect_identical(as_rtftables(df, blank_rows = g)[[1L]]$blank_rows, c(2L, 4L))
  expect_identical(attr(set_blank_rows(df, blank_rows = g), "rtf_blank_rows"), c(2L, 4L))
})

test_that("FT-TB5907 [FR-TB323]: blank_rows_by_change(group_by = 'auto') picks value, indent or filled from each column's content", {
  a <- data.frame(val = c("A", "A", "B", "B"), ind = c("P", "  a", "Q", "  b"),
                  fil = c("A", "", "B", ""), stringsAsFactors = FALSE)
  at <- function(col, m) {
    spec <- blank_rows_by_change(col, group_by = m, include_before_first = FALSE,
                                 include_after_last = FALSE)
    rtftable(a, blank_rows = spec)$blank_rows
  }
  # plain values: a run of equal values is a group, as "value" (after row 2), not as
  # "indent" or "filled" (every row is a group start)
  expect_identical(at("val", "auto"), at("val", "value"))
  expect_identical(at("val", "auto"), 2L)
  expect_identical(at("val", "indent"), 1:3)
  # indented labels: as "indent" (P a | Q b: after row 2), not as "value" (1 2 3)
  expect_identical(at("ind", "auto"), at("ind", "indent"))
  expect_identical(at("ind", "auto"), 2L)
  expect_identical(at("ind", "value"), 1:3)
  # empty cells under a value: as "filled" (A "" | B "": after row 2), not as "value"
  expect_identical(at("fil", "auto"), at("fil", "filled"))
  expect_identical(at("fil", "auto"), 2L)
  expect_identical(at("fil", "value"), 1:3)
  # the spec's default is "value", not "auto"
  expect_identical(blank_rows_by_change("ind")$group_by, "value")
  expect_identical(blank_rows_by_change("ind", group_by = "auto")$group_by, "auto")
})

test_that("FT-TB5908 [FR-TB328]: blank_rows_by_change() on no rows gives no blank row; on one row only the before-first and after-last rows", {
  spec <- blank_rows_by_change("g")
  df <- .t9_blank_df()
  expect_identical(.resolve_blank_rows(spec, df[0L, ]), integer(0))
  expect_null(attr(set_blank_rows(df[0L, ], blank_rows = spec), "rtf_blank_rows"))
  expect_identical(.resolve_blank_rows(blank_rows_by_change("g", include_before_first = FALSE,
                                                              include_after_last = FALSE),
                                       df[0L, ]), integer(0))
  one <- df[1L, ]
  expect_identical(.resolve_blank_rows(spec, one), c(0L, 1L))
  expect_identical(rtftable(one, blank_rows = spec)$blank_rows, c(0L, 1L))
  expect_identical(.t9_rows(rtftable(one, blank_rows = spec)), c("<blank>", "A", "<blank>"))
  expect_identical(.resolve_blank_rows(blank_rows_by_change("g", include_after_last = FALSE), one), 0L)
  expect_identical(.resolve_blank_rows(blank_rows_by_change("g", include_before_first = FALSE), one), 1L)
  expect_identical(.resolve_blank_rows(blank_rows_by_change("g", group_by = "indent"), one), c(0L, 1L))
})

test_that("FT-TB5909 [FR-TB329]: a by_change spec resolved on a page never falls on its first or last row, whatever the include_* flags say", {
  df <- .t9_blank_df()                       # A A B B C: changes after rows 2 and 4
  flags <- expand.grid(b = c(TRUE, FALSE), a = c(TRUE, FALSE))
  for (i in seq_len(nrow(flags))) {
    spec <- blank_rows_by_change("g", include_before_first = flags$b[i],
                                 include_after_last = flags$a[i])
    expect_identical(attr(set_blank_rows(df, blank_rows = spec), "rtf_blank_rows"),
                     c(2L, 4L), info = i)
    expect_identical(as_rtftables(df, blank_rows = spec)[[1L]]$blank_rows, c(2L, 4L), info = i)
  }
  # pages of 2 rows (A A | B B | C): the group changes fall on the page edges, and
  # no page gets a blank row at its first or last row
  spec <- blank_rows_by_change("g")
  pg <- as_rtftables(df, blank_rows = spec, split = "rows", split_rows = c(3, 5))
  expect_identical(lengths(lapply(pg, function(p) p$data$g)), c(2L, 2L, 1L))
  for (p in pg) expect_length(p$blank_rows, 0L)
  # a list holding the spec is trimmed the same way; an explicit position is not:
  # page 1 is A A B B, the spec gives 2 (its change after row 4 is the page's last
  # row), the explicit 4 stays
  l <- as_rtftables(df, blank_rows = list(spec, 4), split = "rows", split_rows = 5)
  expect_identical(l[[1L]]$blank_rows, c(2L, 4L))
  # while rtftable() on the one table keeps both edges
  expect_identical(rtftable(df, blank_rows = spec)$blank_rows, c(0L, 2L, 4L, 5L))
})

test_that("FT-TB5910 [FR-TB330]: blank_rows_by_rule() holds col, pattern and where; the table that receives it inserts the rows", {
  r <- blank_rows_by_rule("g", "^B", "after")
  expect_s3_class(r, "rtf_blank_rows_by_rule")
  expect_identical(unclass(r), list(col = "g", pattern = "^B", where = "after"))
  expect_identical(blank_rows_by_rule("g", "^B")$where, "before")
  expect_error(blank_rows_by_rule("g", "^B", "middle"), "should be one of")
  expect_error(blank_rows_by_rule(c("g", "v"), "x"), "`col` must be a single column name.", fixed = TRUE)
  expect_error(blank_rows_by_rule("g", 1), "`pattern` must be a single regular expression.", fixed = TRUE)
  df <- .t9_blank_df()                                      # rows 3 and 4 match "^B"
  expect_null(attr(df, "rtf_blank_rows"))                   # holding it inserts nothing
  expect_identical(rtftable(df, blank_rows = r)$blank_rows, c(3L, 4L))
  expect_identical(.t9_rows(rtftable(df, blank_rows = r)),
                   c("A", "A", "B", "<blank>", "B", "<blank>", "C"))
  expect_identical(rtftable(df, blank_rows = blank_rows_by_rule("g", "^B"))$blank_rows, c(2L, 3L))  # before rows 3 and 4
  expect_identical(attr(set_blank_rows(df, blank_rows = r), "rtf_blank_rows"), c(3L, 4L))
  expect_identical(as_rtftables(df, blank_rows = r)[[1L]]$blank_rows, c(3L, 4L))
})

test_that("FT-TB5911 [FR-TB333]: blank_rows_by_rule(pattern = ) is matched with grepl() against the column as character; the pattern found anywhere in the value matches, an NA never", {
  at <- function(d, pat, where = "after") {
    rtftable(d, blank_rows = blank_rows_by_rule(colnames(d)[1L], pat, where))$blank_rows
  }
  d <- data.frame(lab = c("Mean", "Mean (SD)", NA, "Median", "n"), stringsAsFactors = FALSE)
  expect_identical(at(d, "Mea"), c(1L, 2L))               # found inside the value, not only at the start
  expect_identical(at(d, "^Mean$"), 1L)                   # anchors are honoured
  expect_identical(at(d, "an|n$"), c(1L, 2L, 4L, 5L))     # a regular expression, with an alternation
  expect_identical(at(d, "."), c(1L, 2L, 4L, 5L))         # the NA (row 3) never matches
  expect_identical(at(d, "NA"), integer(0))               # not the text "NA"
  expect_identical(at(d, "^\\(SD"), integer(0))
  # the column is taken as character: a number, a factor
  num <- data.frame(n = c(1, 10, 12, 2, NA))
  expect_identical(at(num, "^1"), c(1L, 2L, 3L))
  expect_identical(at(num, "0$"), 2L)
  expect_identical(at(data.frame(f = factor(c("ab", "cd", "ab"))), "b$"), c(1L, 3L))
  # the result is grepl()'s on the character values
  expect_identical(at(d, "e"), which(grepl("e", as.character(d$lab))))
  expect_identical(at(d, "Mean", "before"), c(0L, 1L))
})

test_that("FT-TB5912 [FR-TB339]: blank_rows_by_rule() whose pattern matches no row, or applied to no rows, adds no blank row", {
  df <- .t9_blank_df()
  none <- blank_rows_by_rule("g", "^Z")
  expect_length(rtftable(df, blank_rows = none)$blank_rows, 0L)
  expect_identical(.t9_rows(rtftable(df, blank_rows = none)), c("A", "A", "B", "B", "C"))
  expect_identical(.resolve_blank_rows(none, df), integer(0))
  expect_null(attr(set_blank_rows(df, blank_rows = none), "rtf_blank_rows"))
  expect_length(as_rtftables(df, blank_rows = none)[[1L]]$blank_rows, 0L)
  # an NA-only column matches nothing either
  na <- data.frame(g = c(NA_character_, NA_character_))
  expect_identical(.resolve_blank_rows(blank_rows_by_rule("g", "."), na), integer(0))
  # no rows at all
  zero <- df[0L, ]
  for (w in c("before", "after")) {
    expect_identical(.resolve_blank_rows(blank_rows_by_rule("g", ".", w), zero), integer(0))
    expect_identical(.resolve_blank_rows(blank_rows_by_rule("g", "", w), zero), integer(0))
  }
  expect_null(attr(set_blank_rows(zero, blank_rows = blank_rows_by_rule("g", ".")), "rtf_blank_rows"))
  # a pattern that matches does add them, so the above is not just a dead spec
  expect_length(rtftable(df, blank_rows = blank_rows_by_rule("g", "^C"))$blank_rows, 1L)
})

# ---- set_decimal_split() ---------------------------------------------------------

# the \cellx edges, and the texts, of one rendered row
.t9_cx <- function(l) {
  as.integer(sub("cellx", "", regmatches(l, gregexpr("cellx[0-9]+", l))[[1L]], fixed = TRUE))
}
# the column names of a page (of one data frame, or of the first of several)
.t9_names <- function(p) names(if (is.null(p$data)) p$data_list[[1L]] else p$data)
.t9_tx <- function(l) {
  m <- regmatches(l, gregexpr("\\\\ri[0-9]+ [^\\\\]*\\\\cell", l))[[1L]]
  sub("\\cell", "", sub("^\\\\ri[0-9]+ ", "", m), fixed = TRUE)
}

test_that("FT-TB5913 [FR-TB341]: set_decimal_split() stores the split as x$decimal_split and changes only the RTF output; a table without the option renders as before", {
  d <- data.frame(Item = c("a", "b"), Mean = c("12.5", "3.25"), SD = c("1.0", "0.5"),
                  stringsAsFactors = FALSE)
  tb <- rtftable(d, col_rel_width = c(1, 1, 1))
  sp <- set_decimal_split(tb, cols = 2)
  expect_s3_class(sp, "rtftable")
  expect_identical(sp$decimal_split$cols, 2L)            # stored on the table
  expect_null(tb$decimal_split)
  # everything but the stored split is the table it came from: the data frame is
  # never rewritten
  expect_identical(sp$data, tb$data)
  expect_identical(sp$data$Mean, c("12.5", "3.25"))
  keep <- setdiff(names(sp), "decimal_split")
  expect_identical(unclass(sp)[keep], unclass(tb)[keep])
  # the console preview does not show the split
  expect_identical(capture.output(print(sp)), capture.output(print(tb)))
  # the RTF output changes: the header keeps its three cells, a data row has four
  # (width 9360, thirds of 3120; "12"|".5" and "3"|".25" measure 2 and 3 characters,
  # ratio max(2 + 1, 4) : max(3 + 1, 6) = 0.4, so the split is at 3120 + round(3120 * 0.4) = 4368)
  r0 <- .render_rtftable(tb, 9360L)
  r1 <- .render_rtftable(sp, 9360L)
  expect_identical(r1[1L], r0[1L])                        # the header row is as before
  expect_length(.t9_cx(r0[2L]), 3L)
  expect_identical(.t9_cx(r1[2L]), c(3120L, 4368L, 6240L, 9360L))
  expect_identical(.t9_tx(r1[2L]), c("a", "12", ".5", "1.0"))
  expect_identical(.t9_tx(r1[3L]), c("b", "3", ".25", "0.5"))
  # a table without the option, one cleared of it, and one whose selected column has
  # no separator render byte for byte alike
  expect_identical(.render_rtftable(set_decimal_split(sp, cols = NULL), 9360L), r0)
  expect_null(set_decimal_split(sp, cols = NULL)$decimal_split)
  ints <- data.frame(Item = c("a", "b"), N = c("12", "345"), stringsAsFactors = FALSE)
  expect_identical(.render_rtftable(set_decimal_split(rtftable(ints), cols = 2), 9360L),
                   .render_rtftable(rtftable(ints), 9360L))
})

test_that("FT-TB5914 [FR-TB349]: set_decimal_split(ratio = NULL) sizes the halves from the widest left and right part present; a ratio strictly between 0 and 1 is the left share and keeps the split point across pages", {
  mk <- function(v) rtftable(data.frame(a = c("x", "y"), v = v, stringsAsFactors = FALSE),
                             col_rel_width = c(1, 1))
  edge <- function(tb) .t9_cx(.render_rtftable(tb, 9360L)[2L])[2L]
  expect_null(formals(set_decimal_split.rtftable)$ratio)
  # column 2 starts at 4680.  "1.5" "2.25": left 1, right ".25" 3 -> max(1 + 1, 4) :
  # max(3 + 1, 6) = 4 : 6, 4680 + round(4680 * 0.4) = 6552
  expect_identical(edge(set_decimal_split(mk(c("1.5", "2.25")), cols = 2)), 6552L)
  # "1000.125" "20.5": left 4, right 4 -> 5 : 6, 4680 + round(4680 * 5 / 11) = 6807
  expect_identical(edge(set_decimal_split(mk(c("1000.125", "20.5")), cols = 2)), 6807L)
  # the widest left (1000) and the widest right (.125) can sit in different rows
  expect_identical(edge(set_decimal_split(mk(c("1000.5", "2.125")), cols = 2)), 6807L)
  expect_null(set_decimal_split(mk("1.5"), cols = 2)$decimal_split$ratio)
  # a number is the left half's share: 4680 + round(4680 * r), whatever the data
  for (v in list(c("1.5", "2.25"), c("1000.125", "20.5"))) {
    expect_identical(edge(set_decimal_split(mk(v), cols = 2, ratio = 0.3)), 6084L)
    expect_identical(edge(set_decimal_split(mk(v), cols = 2, ratio = 0.75)), 8190L)
  }
  expect_identical(set_decimal_split(mk("1.5"), cols = 2, ratio = 0.3)$decimal_split$ratio, 0.3)
  # a ratio of 0, 1, outside, NA, text or two numbers is refused
  for (bad in list(0, 1, -0.2, 1.5, NA, "x", c(0.2, 0.3))) {
    expect_error(set_decimal_split(mk("1.5"), cols = 2, ratio = bad),
                 "`ratio` must be a single number strictly between 0 and 1.", fixed = TRUE)
  }
  expect_no_error(set_decimal_split(mk("1.5"), cols = 2, ratio = 0.001))
  expect_no_error(set_decimal_split(mk("1.5"), cols = 2, ratio = 0.999))
})

test_that("FT-TB5915 [FR-TB351]: set_decimal_split(decimal_mark = ) is the separator the cells are split at, the first one in the cell; another single string splits there instead", {
  expect_identical(formals(set_decimal_split.rtftable)$decimal_mark, ".")
  d <- data.frame(a = c("1,5", "12,25", "1,2,3"), stringsAsFactors = FALSE)
  expect_identical(set_decimal_split(rtftable(d), cols = 1)$decimal_split$decimal_mark, ".")
  # the default "." finds none of these cells' separator: the column is left as it is
  expect_identical(.render_rtftable(set_decimal_split(rtftable(d), cols = 1), 9360L),
                   .render_rtftable(rtftable(d), 9360L))
  # "," splits there, at the first one: "1,2,3" is "1" and ",2,3"
  sp <- set_decimal_split(rtftable(d), cols = 1, decimal_mark = ",")
  expect_identical(sp$decimal_split$decimal_mark, ",")
  r <- .render_rtftable(sp, 9360L)
  expect_identical(.t9_tx(r[2L]), c("1", ",5"))
  expect_identical(.t9_tx(r[3L]), c("12", ",25"))
  expect_identical(.t9_tx(r[4L]), c("1", ",2,3"))
  # "12,25": 2 and 3 characters -> 4 : 6, 9360 * 0.4 = 3744
  expect_identical(.t9_cx(r[3L]), c(3744L, 9360L))
  # the default splits "1.2.3" at the first "."
  rd <- .render_rtftable(set_decimal_split(rtftable(data.frame(a = "1.2.3")), cols = 1), 9360L)
  expect_identical(.t9_tx(rd[2L]), c("1", ".2.3"))
  # a mark of several characters, and one that is a regular-expression character
  rm <- .render_rtftable(set_decimal_split(rtftable(data.frame(a = "4::5")), cols = 1,
                                           decimal_mark = "::"), 9360L)
  expect_identical(.t9_tx(rm[2L]), c("4", "::5"))
  rp <- .render_rtftable(set_decimal_split(rtftable(data.frame(a = c("4|5", "n/a"))), cols = 1,
                                           decimal_mark = "|"), 9360L)
  expect_identical(.t9_tx(rp[2L]), c("4", "|5"))
  expect_length(.t9_cx(rp[3L]), 1L)                       # "n/a" has no digit: free text
  # the mark must be one non-empty string
  for (bad in list("", NA_character_, c(".", ","), 1)) {
    expect_error(set_decimal_split(rtftable(d), cols = 1, decimal_mark = bad),
                 "`decimal_mark` must be a single non-empty string.", fixed = TRUE)
  }
})

test_that("FT-TB5916 [FR-TB355]: set_decimal_split(max_chars = 10) is the total measured width above which the allowance is dropped; the boundary is exclusive and Inf never drops it", {
  expect_identical(formals(set_decimal_split.rtftable)$max_chars, 10)
  one <- function(v, ...) {
    tb <- set_decimal_split(rtftable(data.frame(a = v, stringsAsFactors = FALSE)), cols = 1, ...)
    .t9_cx(.render_rtftable(tb, 9360L)[2L])[1L]
  }
  # "123.123456": left 3, right ".123456" 7, total 10: not above 10, the allowance
  # stays: max(3 + 1, 4) : max(7 + 1, 6) = 4 : 8, 9360 * 4 / 12 = 3120
  expect_identical(one("123.123456"), 3120L)
  expect_identical(one("123.123456", max_chars = 10), 3120L)
  # a cap of 9 puts the total 10 above it: the raw 3 : 7, 9360 * 3 / 10 = 2808
  expect_identical(one("123.123456", max_chars = 9), 2808L)
  # "12345.123456": total 12 > 10: raw 5 : 7, 9360 * 5 / 12 = 3900; Inf keeps the
  # allowance: 6 : 8, 9360 * 6 / 14 = 4011.4 -> 4011; a cap of 12 keeps it too (not above)
  expect_identical(one("12345.123456"), 3900L)
  expect_identical(one("12345.123456", max_chars = Inf), 4011L)
  expect_identical(one("12345.123456", max_chars = 12), 4011L)
  expect_identical(one("12345.123456", max_chars = 11.9), 3900L)
  # the function that takes the ratio, at the boundary
  expect_equal(rtfreporter:::.decimal_split_ratio(3L, 7L, max_chars = 10), 4 / 12)
  expect_equal(rtfreporter:::.decimal_split_ratio(3L, 8L, max_chars = 10), 3 / 11)
  expect_equal(rtfreporter:::.decimal_split_ratio(3L, 8L, max_chars = 11), 4 / 13)
  expect_equal(rtfreporter:::.decimal_split_ratio(30L, 40L, max_chars = Inf), 31 / 72)
  expect_equal(rtfreporter:::.decimal_split_ratio(30L, 40L), 30 / 70)
  expect_identical(set_decimal_split(rtftable(data.frame(a = "1.5")), cols = 1)$decimal_split$max_chars, 10)
  expect_identical(set_decimal_split(rtftable(data.frame(a = "1.5")), cols = 1,
                                     max_chars = Inf)$decimal_split$max_chars, Inf)
  for (bad in list(0, -1, NA, c(1, 2))) {
    expect_error(set_decimal_split(rtftable(data.frame(a = "1.5")), cols = 1, max_chars = bad),
                 "`max_chars` must be a single positive number (or Inf).", fixed = TRUE)
  }
})

test_that("FT-TB5917 [FR-TB359]: a cell is split when it starts with an optional prefix then a digit or the separator and holds a digit; the prefix goes left, a suffix right", {
  x <- c("<0.001", ">=1.5", "<=2.5", ">3.5", "~4.5", "+5.5", "-6.5", ".5", "7.5^{a}", "12.0*",
         "5.2%", "100", "1.5.2",
         "<.", ".", "a1.5", "p<0.05", "n", "-", "3 (4)", "")
  p <- rtfreporter:::.decimal_split_cells(x)
  split <- c(rep(TRUE, 13L), rep(FALSE, 7L), TRUE)    # the empty cell is two empty halves
  expect_identical(p$split, split)
  expect_identical(p$left[1:13],
                   c("<0", ">=1", "<=2", ">3", "~4", "+5", "-6", "", "7", "12", "5", "100", "1"))
  expect_identical(p$right[1:13],
                   c(".001", ".5", ".5", ".5", ".5", ".5", ".5", ".5", ".5^{a}", ".0*",
                     ".2%", "", ".5.2"))
  # a cell that fails the test keeps its original text and is not split
  expect_identical(p$left[14:20], x[14:20])
  expect_identical(p$right[14:20], rep("", 7L))
  expect_identical(c(p$left[21L], p$right[21L]), c("", ""))
  # in the rendered table: the prefix is in the left cell, the suffix in the right one
  d <- data.frame(a = c("<0.001", "5.2%", "100", "x"), stringsAsFactors = FALSE)
  r <- .render_rtftable(set_decimal_split(rtftable(d), cols = 1), 9360L)
  expect_identical(.t9_tx(r[2L]), c("<0", ".001"))
  expect_identical(.t9_tx(r[3L]), c("5", ".2%"))
  expect_identical(.t9_tx(r[4L]), c("100", ""))
  expect_identical(.t9_tx(r[5L]), "x")
})

test_that("FT-TB5918 [FR-TB360]: free text is not split and is written as one cell across the pair as without the option; empty and NA cells give two empty halves", {
  d <- data.frame(a = c("p", "q", "r", "s", "t"), v = c("3.45", "n (%)", "Placebo", "", NA),
                  stringsAsFactors = FALSE)
  tb <- rtftable(d, col_rel_width = c(1, 1))
  plain <- .render_rtftable(tb, 9360L)
  sp <- .render_rtftable(set_decimal_split(tb, cols = 2), 9360L)
  # the decimal row: "3" | ".45" (1 and 3 characters -> 4 : 6, 4680 + round(1872) = 6552)
  expect_identical(.t9_cx(sp[2L]), c(4680L, 6552L, 9360L))
  expect_identical(.t9_tx(sp[2L]), c("p", "3", ".45"))
  # free text: one cell across the pair, the very bytes of the unsplit table
  for (i in 3:4) {
    expect_length(.t9_cx(sp[i]), 2L)
    expect_identical(sp[i], plain[i])
  }
  expect_identical(.t9_tx(sp[3L]), c("q", "n (%)"))
  expect_identical(.t9_tx(sp[4L]), c("r", "Placebo"))
  # empty and NA: two empty halves, i.e. two cells with nothing in them
  for (i in 5:6) {
    expect_identical(.t9_cx(sp[i]), c(4680L, 6552L, 9360L))
    expect_identical(.t9_tx(sp[i])[-1L], c("", ""))
  }
  # a column of free text and empty cells only has no separator: untouched
  f <- data.frame(a = c("p", "q", "r"), v = c("n (%)", "", NA), stringsAsFactors = FALSE)
  expect_identical(.render_rtftable(set_decimal_split(rtftable(f), cols = 2), 9360L),
                   .render_rtftable(rtftable(f), 9360L))
})

test_that("FT-TB5919 [FR-TB362]: a selected column with no separator is left untouched, decided per table: across paginated pages it may be split on one page and not on another", {
  d <- data.frame(g = c("a", "b", "c", "d"), v = c("1.5", "2.25", "10", "20"),
                  w = c("5", "6", "7.5", "8"), stringsAsFactors = FALSE)
  pages <- as_rtftables(d, split = "rows", split_rows = 3)        # rows 1-2 | 3-4
  expect_length(pages, 2L)
  sp <- set_decimal_split(pages, cols = list("v", "w"))
  r1 <- .render_rtftable(sp[[1L]], 9360L)
  r2 <- .render_rtftable(sp[[2L]], 9360L)
  # page 1: v has separators and is split, w has none and is not: 3 + 1 cells
  expect_length(.t9_cx(r1[2L]), 4L)
  expect_identical(.t9_tx(r1[2L]), c("a", "1", ".5", "5"))
  expect_identical(.t9_tx(r1[3L]), c("b", "2", ".25", "6"))
  # page 2: v has none and is not, w has one and is: 3 + 1 cells
  expect_length(.t9_cx(r2[2L]), 4L)
  expect_identical(.t9_tx(r2[2L]), c("c", "10", "7", ".5"))
  expect_identical(.t9_tx(r2[3L]), c("d", "20", "8", ""))
  # every page carries the same setting, the decision is made when it renders
  expect_identical(sp[[1L]]$decimal_split$cols, 2:3)
  expect_identical(sp[[2L]]$decimal_split$cols, 2:3)
  # a page with no separator in any selected column is the table without the option
  none <- set_decimal_split(as_rtftables(d[3:4, c("g", "v")]), cols = "v")
  expect_identical(.render_rtftable(none[[1L]], 9360L),
                   .render_rtftable(as_rtftables(d[3:4, c("g", "v")])[[1L]], 9360L))
})

test_that("FT-TB5920 [FR-TB365]: a table of several data frames has every one scanned for the separator, and the setting survives paginate_cols()", {
  d1 <- data.frame(a = "x", v = "1", stringsAsFactors = FALSE)
  d2 <- data.frame(a = "y", v = "2.5", stringsAsFactors = FALSE)
  # the first frame has no separator, the second has: the column is split in both
  tb <- set_decimal_split(rtftable(list(d1, d2)), cols = "v")
  r <- .render_rtftable(tb, 9360L)
  rows <- r[vapply(r, function(l) length(.t9_cx(l)) == 3L, logical(1L))]
  expect_length(rows, 2L)
  expect_identical(.t9_tx(rows[1L]), c("x", "1", ""))
  expect_identical(.t9_tx(rows[2L]), c("y", "2", ".5"))
  # no frame with a separator: left alone
  none <- rtftable(list(d1, data.frame(a = "z", v = "3", stringsAsFactors = FALSE)))
  expect_identical(.render_rtftable(set_decimal_split(none, cols = "v"), 9360L),
                   .render_rtftable(none, 9360L))
  # paginate_cols() on a single frame and on several: each page keeps the setting,
  # its columns renumbered to the page, and renders the split
  x <- data.frame(P = "a", Q = "b", V1 = c("1", "2.5"), V2 = c("3.1", "4"), stringsAsFactors = FALSE)
  for (t0 in list(rtftable(x), rtftable(list(x, x)))) {
    pages <- paginate_cols(set_decimal_split(t0, cols = 3:4, ratio = 0.3), at = 4L, keep = 1:2)
    expect_length(pages, 2L)
    expect_identical(.t9_names(pages[[1L]]), c("P", "Q", "V1"))
    expect_identical(.t9_names(pages[[2L]]), c("P", "Q", "V2"))
    for (p in pages) {
      expect_identical(p$decimal_split$cols, 3L)
      expect_identical(p$decimal_split$ratio, 0.3)
    }
    body4 <- function(p) {
      rr <- .render_rtftable(p, 9360L)
      rr[vapply(rr, function(l) length(.t9_cx(l)) == 4L, logical(1L))]
    }
    r1 <- body4(pages[[1L]])
    expect_identical(.t9_tx(r1[1L]), c("a", "b", "1", ""))
    expect_identical(.t9_tx(r1[2L]), c("a", "b", "2", ".5"))
    r2 <- body4(pages[[2L]])
    expect_identical(.t9_tx(r2[1L]), c("a", "b", "3", ".1"))
    expect_identical(.t9_tx(r2[2L]), c("a", "b", "4", ""))
  }
})

# ---- auto_col_widths() -----------------------------------------------------------

test_that("FT-TB5921 [FR-TB370]: auto_col_widths(font, font_size_half_points = 18L) measures every character at one average width of the font: Courier's for Courier and unknown fonts, a flat one for Arial; NULL is the rtfreporter.font option", {
  expect_identical(formals(auto_col_widths)$font_size_half_points, 18L)
  expect_null(formals(auto_col_widths)$font)
  d <- data.frame(a = strrep("i", 20L), b = strrep("W", 20L), stringsAsFactors = FALSE)
  # 20 characters at 9 pt (18 half-points) plus 288 twips of padding:
  #   Courier  7.22 * 9 / 12 = 5.415 pt a character -> 108.3 pt = 2166 twips -> 2454
  #   Arial    6.00 * 9 / 12 = 4.5 pt a character   -> 90 pt   = 1800 twips -> 2088
  expect_identical(auto_col_widths(d, font = "Courier New"), c(2454L, 2454L))
  expect_identical(auto_col_widths(d, font = "Courier"), c(2454L, 2454L))
  expect_identical(auto_col_widths(d, font = "Not A Font"), c(2454L, 2454L))
  expect_identical(auto_col_widths(d, font = "Times New Roman"), c(2454L, 2454L))   # not known: Courier's
  expect_identical(auto_col_widths(d, font = "Arial"), c(2088L, 2088L))
  # the same for every character: a column of i and a column of W are as wide
  expect_identical(auto_col_widths(d, font = "arial"), c(2088L, 2088L))
  # ... which text_width_in() does not say of Arial: its measure is per character
  expect_lt(text_width_in(strrep("i", 20L), font = "Arial"), text_width_in(strrep("W", 20L), font = "Arial"))
  expect_gt(text_width_in(strrep("W", 20L), font = "Arial") * 1440 + 288, 2088)
  expect_lt(text_width_in(strrep("i", 20L), font = "Arial") * 1440 + 288, 2088)
  # a half-point size: 12 (6 pt) halves a Courier character, 36 (18 pt) doubles it
  #   6 pt: 20 * 7.22 * 6 / 12 = 72.2 pt = 1444 twips -> 1732; 18 pt: 4332 + 288 = 4620
  expect_identical(auto_col_widths(d, font_size_half_points = 12L), c(1732L, 1732L))
  expect_identical(auto_col_widths(d, font_size_half_points = 18L), c(2454L, 2454L))
  expect_identical(auto_col_widths(d, font_size_half_points = 36L), c(4620L, 4620L))
  expect_identical(auto_col_widths(d, font = "Arial", font_size_half_points = 36L), c(3888L, 3888L))
  # NULL is getOption("rtfreporter.font")
  old <- options(rtfreporter.font = "Arial")
  on.exit(options(old), add = TRUE)
  expect_identical(auto_col_widths(d), c(2088L, 2088L))
  expect_identical(auto_col_widths(d, font = NULL), c(2088L, 2088L))
  expect_identical(auto_col_widths(d, font = "Courier New"), c(2454L, 2454L))   # an argument wins
  options(rtfreporter.font = "Courier New")
  expect_identical(auto_col_widths(d), c(2454L, 2454L))
})

# ---- header_map(), add_cont_label() ----------------------------------------------

test_that("FT-TB5922 [FR-TB252]: header_map(x = ) is an rtftable or a list of them", {
  d <- data.frame(p = c("u", "v"), k = 1:2, stringsAsFactors = FALSE)
  t1 <- rtftable(d, col_header = list(list(col_cell(1:2, "Top")), c("p", "k")))
  t2 <- rtftable(d, col_header = c("P", "K"))
  # one table: page 1, a spanning cell and two labels
  one <- header_map(t1)
  expect_s3_class(one, "data.frame")
  expect_identical(one$page, c(1L, 1L, 1L))
  expect_identical(one$row, c(1L, 2L, 2L))
  expect_identical(one$cell, c(1L, 1L, 2L))
  expect_identical(one$from, c(1L, 1L, 2L))
  expect_identical(one$to, c(2L, 1L, 2L))
  expect_identical(one$text, c("Top", "p", "k"))
  expect_true(all(is.na(one$name)))                     # a table on its own has no page name
  # the same table as a list of one: the same map
  expect_identical(header_map(list(t1)), one)
  # a list of tables, as as_rtftables() gives: every page, in order
  both <- header_map(list(A = t1, B = t2))
  expect_identical(both$page, c(1L, 1L, 1L, 2L, 2L))
  expect_identical(both$name, c("A", "A", "A", "B", "B"))
  expect_identical(both$text, c("Top", "p", "k", "P", "K"))
  expect_identical(both$row, c(1L, 2L, 2L, 1L, 1L))
  pages <- as_rtftables(data.frame(g = c("a", "b"), v = 1:2, stringsAsFactors = FALSE),
                        split = "rows", split_rows = 2, col_header = c("G", "V"))
  hp <- header_map(pages)
  expect_identical(hp$page, c(1L, 1L, 2L, 2L))
  expect_identical(hp$text, c("G", "V", "G", "V"))
})

test_that("FT-TB5923 [FR-TB260]: add_cont_label() prepends one row: paste0(label, cont_label) in column col, every other cell empty (\"\" for character, NA otherwise); the row names are reset", {
  df <- data.frame(g = c("B", "B"), n = c(3, 4), i = 1:2, l = c(TRUE, FALSE), s = c("x", "y"),
                   stringsAsFactors = FALSE)
  rownames(df) <- c("r7", "r9")
  out <- add_cont_label(df, "Group B")
  expect_identical(nrow(out), 3L)
  expect_identical(names(out), names(df))
  expect_identical(out$g, c("Group B (Cont.)", "B", "B"))
  expect_identical(out$s, c("", "x", "y"))                       # character: ""
  expect_identical(out$n, c(NA, 3, 4))                           # anything else: NA
  expect_identical(out$i, c(NA, 1L, 2L))
  expect_identical(out$l, c(NA, TRUE, FALSE))
  expect_identical(attr(out, "row.names"), 1:3)                  # reset
  # the rows that were there follow, as they were
  expect_identical(out[-1L, "n"], df$n)
  # another suffix, and another column by index or by name
  expect_identical(add_cont_label(df, "B", cont_label = " (cont'd)")$g[1L], "B (cont'd)")
  expect_identical(add_cont_label(df, "B", cont_label = "")$g[1L], "B")
  by_idx <- add_cont_label(df, "Lbl", col = 5L)
  expect_identical(by_idx$s, c("Lbl (Cont.)", "x", "y"))
  expect_identical(by_idx$g, c("", "B", "B"))
  expect_identical(add_cont_label(df, "Lbl", col = "s"), by_idx)
  # a factor label column is made character so that it can hold the suffix
  f <- data.frame(g = factor(c("B", "B")), n = c(3, 4))
  fo <- add_cont_label(f, "B")
  expect_identical(fo$g, c("B (Cont.)", "B", "B"))
  expect_identical(fo$n, c(NA, 3, 4))
  # a wrong call stops
  expect_error(add_cont_label(df, "B", col = "nope"), "`col` 'nope' not found in `data`.", fixed = TRUE)
  expect_error(add_cont_label(df, "B", col = 9L), "`col` index 9 out of range (1..5).", fixed = TRUE)
  expect_error(add_cont_label(list(1), "B"), "`data` must be a data.frame.", fixed = TRUE)
  expect_error(add_cont_label(df, c("A", "B")), "`label` must be a single string.", fixed = TRUE)
})

# ---- set_col_header(), add_header_row() ------------------------------------------

test_that("FT-TB5924 [FR-TB212]: set_col_header(x, ...) replaces the whole column header of a finished rtftable and returns the table; cells resolve against the final printed columns, by name or visible position", {
  d <- data.frame(x = c("q", "q"), soc = c("S1", "S1"), pt = c("a", "b"), n = c("1", "2"),
                  m = c("3", "4"), stringsAsFactors = FALSE)
  pg <- as_rtftables(d, drop_cols = "x", stub = stub_spec(c("soc", "pt")))[[1L]]
  expect_identical(names(pg$data), c("soc / pt", "n", "m"))           # the final printed columns
  old <- set_col_header(pg, list(col_cell(2:3, "Old")), c("L", "N", "M"))
  expect_length(old$col_header, 2L)
  # a second call replaces the whole header: nothing of the first is left
  new <- set_col_header(old, c("Lab", "N2", "M2"))
  expect_s3_class(new, "rtftable")
  expect_identical(new$col_header, list(c("Lab", "N2", "M2")))
  expect_identical(new$data, pg$data)                                  # the table is otherwise the same
  keep <- setdiff(names(new), "col_header")
  expect_identical(unclass(new)[keep], unclass(old)[keep])
  # the header in the output: one row of the new labels, then the data
  r <- .render_rtftable(new, 9360L)
  expect_identical(.t9_tx(r[1L]), c("Lab", "N2", "M2"))
  expect_identical(.t9_tx(r[3L]), c("1", "3"))   # r[2] is the stub label row "S1"; the indented stub cell holds escapes and is not read
  # by visible position (1 is the stub, not the dropped "x") and by name: the same cells
  by_pos <- set_col_header(pg, list(col_cell(c(2L, 3L), "S")), c("Lab", "N", "M"))
  by_name <- set_col_header(pg, list(col_cell(c("n", "m"), "S")), c("Lab", "N", "M"))
  expect_identical(by_name$col_header, by_pos$col_header)
  expect_identical(by_pos$col_header[[1L]][[2L]], list(from = 2L, to = 3L, label = "S"))
  first <- set_col_header(pg, list(col_cell("soc / pt", "First")))
  expect_identical(first$col_header[[1L]][[1L]], list(from = 1L, to = 1L, label = "First"))
  expect_identical(set_col_header(pg, list(col_cell(1L, "First")))$col_header, first$col_header)
  # the columns drop_cols and the stub took out are not addressable
  expect_error(set_col_header(pg, list(col_cell("x", "X"))),
               "`pos` column name \"x\" not found in data columns.", fixed = TRUE)
  expect_error(set_col_header(pg, list(col_cell("soc", "S"))),
               "`pos` column name \"soc\" not found in data columns.", fixed = TRUE)
  expect_error(set_col_header(pg, list(col_cell(4L, "X"))),
               "`pos` 4-4 outside data column range 1..3.", fixed = TRUE)
})

test_that("FT-TB5925 [FR-TB213]: set_col_header(...) are the header rows, top first: a character vector, a list of col_cell() cells or one prebuilt rtf_col_header(); one argument is a whole header, several are consecutive rows", {
  t3 <- rtftable(data.frame(A = "1", B = "2", C = "3", stringsAsFactors = FALSE))
  span <- list(from = 1L, to = 1L, label = "")
  # a character vector: a label row
  expect_identical(set_col_header(t3, c("a", "b", "c"))$col_header, list(c("a", "b", "c")))
  # a list of col_cell(): a spanning row, as the whole header (the unspanned column gets an empty cell)
  sp <- set_col_header(t3, list(col_cell(2:3, "Top")))
  expect_identical(sp$col_header, list(list(span, list(from = 2L, to = 3L, label = "Top"))))
  # one prebuilt rtf_col_header(): the spanning row above the label row
  pre <- set_col_header(t3, rtf_col_header(list(col_cell(c(1, 3), "All")), c("a", "b", "c")))
  expect_identical(pre$col_header, list(list(list(from = 1L, to = 3L, label = "All")),
                                        c("a", "b", "c")))
  # several: consecutive rows, top first, whatever their shape
  three <- set_col_header(t3, list(col_cell(c(1, 3), "Study")), list(col_cell(2:3, "Arm")), c("a", "b", "c"))
  expect_length(three$col_header, 3L)
  expect_identical(three$col_header[[1L]][[1L]]$label, "Study")
  expect_identical(three$col_header[[2L]][[2L]]$label, "Arm")
  expect_identical(three$col_header[[3L]], c("a", "b", "c"))
  two <- set_col_header(t3, c("t1", "t2", "t3"), c("b1", "b2", "b3"))
  expect_identical(two$col_header, list(c("t1", "t2", "t3"), c("b1", "b2", "b3")))
  # in the output the first argument is the top row
  r <- .render_rtftable(two, 9360L)
  expect_identical(.t9_tx(r[1L]), c("t1", "t2", "t3"))
  expect_identical(.t9_tx(r[2L]), c("b1", "b2", "b3"))
  expect_identical(.t9_tx(r[3L]), c("1", "2", "3"))
  r3 <- .render_rtftable(three, 9360L)
  expect_identical(.t9_tx(r3[1L]), "Study")
  expect_identical(.t9_tx(r3[2L]), c("", "Arm"))
  expect_identical(.t9_tx(r3[3L]), c("a", "b", "c"))
})

test_that("FT-TB5926 [FR-TB216]: a col_cell() pos may be column names, final positions, a range c(a, b) or a selector such as col_key(); a selector that matches no column or non-adjacent columns is an error", {
  d <- data.frame(lab = "x", T1__n = "1", T1__p = "2", T2__n = "3", T2__p = "4",
                  check.names = FALSE, stringsAsFactors = FALSE)
  tb <- rtftable(d)
  lab <- c("l", "n", "p", "n", "p")
  cells <- function(pos) set_col_header(tb, list(col_cell(pos, "S")), lab)$col_header[[1L]]
  # a name, and a position: one column
  expect_identical(cells("T2__n")[[2L]], list(from = 4L, to = 4L, label = "S"))
  expect_identical(cells(4L)[[2L]], list(from = 4L, to = 4L, label = "S"))
  # a range of two names or of two positions covers the columns between them
  expect_identical(cells(c("T1__n", "T2__n"))[[2L]], list(from = 2L, to = 4L, label = "S"))
  expect_identical(cells(c(2L, 4L))[[2L]], list(from = 2L, to = 4L, label = "S"))
  expect_identical(cells(c(2, 3))[[2L]], list(from = 2L, to = 3L, label = "S"))
  # a selector: a column-name segment, or any function of the names
  expect_identical(cells(col_key("T1", sep = "__", part = 1))[[2L]],
                   list(from = 2L, to = 3L, label = "S"))
  expect_identical(cells(function(nm) grepl("^T2", nm))[[2L]],
                   list(from = 4L, to = 5L, label = "S"))
  expect_identical(cells(function(nm) nm %in% c("T1__p", "T2__n"))[[2L]],
                   list(from = 3L, to = 4L, label = "S"))
  # no column, non-adjacent columns
  expect_error(set_col_header(tb, list(col_cell(col_key("zzz", sep = "__", part = 1), "S")), lab),
               "col_key(\"zzz\") matched no data columns.", fixed = TRUE)
  expect_error(set_col_header(tb, list(col_cell(col_key("n", sep = "__", part = 2), "S")), lab),
               "col_key(\"n\") matched non-adjacent data columns (2, 4); a header cell can only span a contiguous range.",
               fixed = TRUE)
  expect_error(set_col_header(tb, list(col_cell(function(nm) nm %in% c("lab", "T2__p"), "S")), lab),
               "matched non-adjacent data columns (1, 5); a header cell can only span a contiguous range.",
               fixed = TRUE)
  expect_error(set_col_header(tb, list(col_cell(function(nm) rep(FALSE, length(nm)), "S")), lab),
               "matched no data columns.", fixed = TRUE)
})

test_that("FT-TB5927 [FR-TB222]: set_col_header(values = ) fills {name} from the row each page's key matches; {{ is a literal brace; render-time tokens are left for the renderer", {
  d <- data.frame(period = c("P1", "P1", "P2", "P2"), lab = c("x", "y", "z", "w"),
                  v1 = "1", v2 = "2", stringsAsFactors = FALSE)
  pg <- as_rtftables(d, split = "by_value", group_col = "period", drop_cols = "period")
  expect_identical(names(pg), c("P1", "P2"))
  hh <- rtf_col_header(list(col_cell(2:3, "Grp (N={n}) {{lit}}")), c("Lab", "a {x}", "b"))
  # one row per page key, here in the other order: the key decides, not the position
  vals <- data.frame(group = c("P2", "P1"), n = c(20, 10), x = c("X2", "X1"), stringsAsFactors = FALSE)
  out <- set_col_header(pg, hh, values = vals)
  expect_identical(names(out), c("P1", "P2"))
  expect_identical(out$P1$col_header[[1L]][[2L]]$label, "Grp (N=10) {lit}")
  expect_identical(out$P2$col_header[[1L]][[2L]]$label, "Grp (N=20) {lit}")
  expect_identical(out$P1$col_header[[2L]], c("Lab", "a X1", "b"))
  expect_identical(out$P2$col_header[[2L]], c("Lab", "a X2", "b"))
  # the header is written once: the same spec filled for each page
  expect_identical(out$P1$col_header[[1L]][[2L]][c("from", "to")], list(from = 2L, to = 3L))
  # the render-time tokens stay as they are
  tok <- c("PAGE", "TOTAL_PAGES", "BOOK_PAGE", "AUTO_PAGE", "AUTO_TOTAL_PAGES", "PROGRAM",
           "PROGRAM_FULL", "DATETIME")
  hh2 <- rtf_col_header(list(col_cell(2:3, paste0("N={n} ", paste0("{", tok, "}", collapse = " ")))),
                        c("Lab", "a", "b"))
  o2 <- set_col_header(pg, hh2, values = vals[, c("group", "n")])
  expect_identical(o2$P1$col_header[[1L]][[2L]]$label,
                   paste0("N=10 ", paste0("{", tok, "}", collapse = " ")))
  # a token with no column in values stops, naming it; a row that matches no page; a page without a row
  expect_error(set_col_header(pg, hh, values = vals[, c("group", "n")]), "no value for `{x}`", fixed = TRUE)
  expect_error(set_col_header(pg, hh, values = rbind(vals, data.frame(group = "P3", n = 1, x = "z"))),
               "`values` row 3 matched no page: P3.", fixed = TRUE)
  expect_error(set_col_header(pg, hh, values = vals[2L, ]),
               "`values` has no row for page 2 (\"P2\").", fixed = TRUE)
  expect_error(set_col_header(pg, hh, values = list(group = "P1")),
               "`values` must be a data.frame", fixed = TRUE)
})

test_that("FT-TB5928 [FR-TB223]: set_col_header(by = ) matches the key columns of values on group (default), rows or name; several match on the combination", {
  d <- data.frame(period = c("P1", "P1", "P2", "P2"), pg = c("1", "2", "1", "2"),
                  lab = c("x", "y", "z", "w"), v1 = "1", v2 = "2", stringsAsFactors = FALSE)
  pg <- as_rtftables(d, split = "by_value", group_col = "period", page_by = "pg",
                     drop_cols = c("pg", "period"))
  expect_identical(names(pg), c("P1...1", "P1...2", "P2...1", "P2...2"))
  hh <- rtf_col_header(list(col_cell(2:3, "N={n}")), c("Lab", "a", "b"))
  lab <- function(o) unname(vapply(o, function(p) p$col_header[[1L]][[2L]]$label, ""))
  # group (the default): the value the by_value split cut the page for
  g <- data.frame(group = c("P2", "P1"), n = c(20, 10))
  expect_identical(lab(set_col_header(pg, hh, values = g)), c("N=10", "N=10", "N=20", "N=20"))
  expect_identical(lab(set_col_header(pg, hh, values = g, by = "group")), c("N=10", "N=10", "N=20", "N=20"))
  # rows: the page_by value
  r <- data.frame(rows = c("2", "1"), n = c(2, 1))
  expect_identical(lab(set_col_header(pg, hh, values = r, by = "rows")), c("N=1", "N=2", "N=1", "N=2"))
  # name: the page's name, "...n" included, always unique
  nm <- data.frame(name = rev(names(pg)), n = 1:4, stringsAsFactors = FALSE)
  expect_identical(lab(set_col_header(pg, hh, values = nm, by = "name")), c("N=4", "N=3", "N=2", "N=1"))
  # several: the combination
  gr <- expand.grid(group = c("P1", "P2"), rows = c("1", "2"), stringsAsFactors = FALSE)
  gr$n <- 1:4                                       # (P1,1) 1  (P2,1) 2  (P1,2) 3  (P2,2) 4
  expect_identical(lab(set_col_header(pg, hh, values = gr, by = c("group", "rows"))),
                   c("N=1", "N=3", "N=2", "N=4"))
  expect_identical(lab(set_col_header(pg, hh, values = gr[, c("rows", "group", "n")],
                                      by = c("rows", "group"))), c("N=1", "N=3", "N=2", "N=4"))
  # with no group axis the group is the page name without its "...n" tail
  named <- as_rtftables(list(A = d[1:3, c("lab", "v1")]), split = "rows", split_rows = c(2, 3))
  expect_identical(names(named), c("A...1", "A...2", "A...3"))
  h2 <- rtf_col_header(list(col_cell(1:2, "N={n}")), c("lab", "v1"))
  expect_message(o <- set_col_header(named, h2, values = data.frame(group = "A", n = 5)),
                 "no group axis", fixed = TRUE)
  expect_identical(unname(vapply(o, function(p) p$col_header[[1L]][[1L]]$label, "")), rep("N=5", 3L))
  # a key column that is not there, a key that is not an axis
  expect_error(set_col_header(pg, hh, values = g, by = "rows"),
               "`values` has no key column `rows`.", fixed = TRUE)
  expect_error(set_col_header(pg, hh, values = data.frame(zzz = "a", n = 1), by = "zzz"),
               "`by`: \"zzz\" is not a page axis (group, rows, name).", fixed = TRUE)
})

test_that("FT-TB5929 [FR-TB227]: set_col_header() on a list maps over every page of an as_rtftables() list and returns a list of the same names; a table of several data frames takes one header for each", {
  d <- data.frame(g = c("a", "a", "b"), v = c("1", "2", "3"), stringsAsFactors = FALSE)
  pg <- as_rtftables(d, split = "by_value", group_col = "g")
  expect_identical(names(pg), c("a", "b"))
  out <- set_col_header(pg, c("G", "V"))
  expect_type(out, "list")
  expect_identical(names(out), c("a", "b"))
  expect_length(out, 2L)
  for (p in out) {
    expect_s3_class(p, "rtftable")
    expect_identical(p$col_header, list(c("G", "V")))
  }
  expect_identical(out$a$data$v, c("1", "2"))            # the pages are otherwise as they were
  expect_identical(out$b$data$v, "3")
  # several rows go to every page
  two <- set_col_header(pg, list(col_cell(1:2, "Top")), c("G", "V"))
  expect_identical(names(two), c("a", "b"))
  expect_identical(two$a$col_header, two$b$col_header)
  expect_length(two$a$col_header, 2L)
  # an unnamed list comes back unnamed
  un <- set_col_header(list(rtftable(d), rtftable(d)), c("G", "V"))
  expect_null(names(un))
  expect_length(un, 2L)
  # a table of several data frames: one header for each frame
  d1 <- data.frame(Item = "Age", A = "1", B = "3", stringsAsFactors = FALSE)
  d2 <- data.frame(Item = "Sex", A = "2", B = "4", stringsAsFactors = FALSE)
  m <- set_col_header(rtftable(list(d1, d2)), c("i1", "p1", "d1"), c("i2", "p2", "d2"))
  expect_length(m$col_header_list, 2L)
  for (h in m$col_header_list) expect_identical(h, list(c("i1", "p1", "d1"), c("i2", "p2", "d2")))
  r <- .render_rtftable(m, 9360L)
  texts <- lapply(r, .t9_tx)
  expect_identical(sum(vapply(texts, identical, logical(1L), c("i1", "p1", "d1"))), 2L)   # one per frame
  expect_identical(sum(vapply(texts, identical, logical(1L), c("i2", "p2", "d2"))), 2L)
})

test_that("FT-TB5930 [FR-TB232 FR-TB233]: add_header_row(row) takes a label row (named to place labels by column) or a list of col_cell() cells; .position is top (the default) or bottom", {
  t3 <- rtftable(data.frame(a = "1", b = "2", c = "3", stringsAsFactors = FALSE))
  # the table has no header of its own: its column names are the header the row goes next to
  top <- add_header_row(t3, c("A", "B", "C"))
  expect_identical(top$col_header, list(c("A", "B", "C"), c("a", "b", "c")))
  expect_identical(add_header_row(t3, c("A", "B", "C"), .position = "top")$col_header, top$col_header)
  bot <- add_header_row(t3, c("A", "B", "C"), .position = "bottom")
  expect_identical(bot$col_header, list(c("a", "b", "c"), c("A", "B", "C")))
  # a row named by column places its labels by name; the columns not named keep their names
  named <- add_header_row(t3, c(c = "C2", a = "A2"))
  expect_identical(named$col_header[[1L]], c("A2", "b", "C2"))
  # a list of col_cell(): a spanning row
  sp <- add_header_row(t3, list(col_cell(2:3, "Span")))
  expect_identical(sp$col_header[[1L]], list(list(from = 1L, to = 1L, label = ""),
                                             list(from = 2L, to = 3L, label = "Span")))
  expect_identical(sp$col_header[[2L]], c("a", "b", "c"))
  # rows added one after another: top ones stack above, bottom ones below
  both <- add_header_row(add_header_row(t3, c("T1", "T2", "T3")), c("B1", "B2", "B3"), .position = "bottom")
  expect_identical(both$col_header, list(c("T1", "T2", "T3"), c("a", "b", "c"), c("B1", "B2", "B3")))
  r <- .render_rtftable(both, 9360L)
  expect_identical(.t9_tx(r[1L]), c("T1", "T2", "T3"))
  expect_identical(.t9_tx(r[2L]), c("a", "b", "c"))
  expect_identical(.t9_tx(r[3L]), c("B1", "B2", "B3"))
  expect_identical(.t9_tx(r[4L]), c("1", "2", "3"))
  # a header of its own: the row goes above it, or below it
  own <- set_col_header(t3, c("x", "y", "z"))
  expect_identical(add_header_row(own, c("A", "B", "C"))$col_header, list(c("A", "B", "C"), c("x", "y", "z")))
  expect_identical(add_header_row(own, c("A", "B", "C"), .position = "bottom")$col_header,
                   list(c("x", "y", "z"), c("A", "B", "C")))
  # .position is one of the two
  expect_error(add_header_row(t3, c("A", "B", "C"), .position = "middle"), "should be one of")
  # a row that does not fit the table is refused
  expect_error(add_header_row(t3, c("A", "B")), "has 2 labels but the table has 3 columns", fixed = TRUE)
  expect_error(add_header_row(t3, c(zz = "A")), "unknown column name \"zz\"", fixed = TRUE)
})

# ---- stub_cols(), stub_spec() ----------------------------------------------------

test_that("FT-TB5931 [FR-TB195]: a label row is emitted at every run of a parent value, a change in a higher-level column starts a new run in every column below it; each level indents one step further", {
  nb <- intToUtf8(160L)
  p <- function(k) strrep(nb, 4L * k)
  d <- data.frame(a = c("A", "A", "A", "B", "B", "A"), b = c("b1", "b1", "b2", "b2", "b2", "b2"),
                  leaf = c("x", "y", "z", "w", "v", "u"), n = 1:6, stringsAsFactors = FALSE)
  out <- stub_cols(d, vars = c("a", "b", "leaf"))
  # rows 1-2 share the run A / b1: one label row each; row 3 starts b2 under A;
  # row 4 (B, b2) starts B and, though b did not change, b2 again under it; rows 5
  # follows; row 6 (A, b2) is a new run of A -- A has a label row a second time
  expect_identical(out[[1L]],
                   c("A", paste0(p(1), "b1"), paste0(p(2), "x"), paste0(p(2), "y"),
                     paste0(p(1), "b2"), paste0(p(2), "z"),
                     "B", paste0(p(1), "b2"), paste0(p(2), "w"), paste0(p(2), "v"),
                     "A", paste0(p(1), "b2"), paste0(p(2), "u")))
  expect_identical(out$n, c(NA, NA, 1L, 2L, NA, 3L, NA, NA, 4L, 5L, NA, NA, 6L))
  expect_identical(attr(out, "rtf_stub_src"), c(NA, NA, 1L, 2L, NA, 3L, NA, NA, 4L, 5L, NA, NA, 6L))
  lab <- sub(paste0("^", nb, "+"), "", out[[1L]])
  expect_identical(sum(lab == "A"), 2L)                 # a run each, not once per distinct value
  expect_identical(sum(lab == "b2"), 3L)
  expect_identical(sum(lab == "B"), 1L)
  # with more than two vars each level indents one step further (indent = 2): 0, 2, 4, 6
  d4 <- data.frame(a = "A", b = "B", c = "C", leaf = "x", n = "1", stringsAsFactors = FALSE)
  o4 <- stub_cols(d4, vars = c("a", "b", "c", "leaf"), indent = 2)
  expect_identical(o4[[1L]], c("A", paste0(strrep(nb, 2L), "B"), paste0(strrep(nb, 4L), "C"),
                               paste0(strrep(nb, 6L), "x")))
  expect_identical(o4$n, c(NA, NA, NA, "1"))
  # a higher-level change with the lower value unchanged still opens the lower label again
  d2 <- data.frame(a = c("A", "A", "B"), b = c("b1", "b1", "b1"), leaf = c("x", "y", "z"),
                   stringsAsFactors = FALSE)
  o2 <- stub_cols(d2, vars = c("a", "b", "leaf"))
  expect_identical(sub(paste0("^", nb, "+"), "", o2[[1L]]), c("A", "b1", "x", "y", "B", "b1", "z"))
})

test_that("FT-TB5932 [FR-TB199]: group_summary names which leaf values mark a row as its group's summary: empty (NA or \"\"), parent (the deepest non-empty parent), all, none or NULL; its statistics go on the group's label row", {
  nb <- intToUtf8(160L)
  p <- function(k) strrep(nb, 4L * k)
  d <- data.frame(a = c("A", "A", "A", "B"), b = c("b1", "b1", "", "b2"),
                  leaf = c("x", "b1", "A", NA), n = as.character(1:4), stringsAsFactors = FALSE)
  vars <- c("a", "b", "leaf")
  # row 2: leaf "b1" equals its deepest non-empty parent b1 -> summary of b1 (parent);
  # row 3: b is empty, the deepest non-empty parent is a = "A", the leaf "A" equals it -> summary of A;
  # row 4: leaf NA -> summary of b2 (empty)
  par <- stub_cols(d, vars = vars, group_summary = "parent")
  expect_identical(par[[1L]], c("A", paste0(p(1), "b1"), paste0(p(2), "x"),
                                "B", paste0(p(1), "b2"), paste0(p(2), "")))
  expect_identical(par$n, c("3", "2", "1", NA, NA, "4"))
  emp <- stub_cols(d, vars = vars, group_summary = "empty")
  # "empty" folds only row 4 (leaf NA) onto the b2 label row; the leaves "b1" and "A"
  # keep rows of their own
  expect_identical(emp[[1L]], c("A", paste0(p(1), "b1"), paste0(p(2), "x"), paste0(p(2), "b1"),
                                paste0(p(1), "A"), "B", paste0(p(1), "b2")))
  expect_identical(emp$n, c(NA, NA, "1", "2", "3", NA, "4"))
  all <- stub_cols(d, vars = vars, group_summary = "all")
  expect_identical(all[[1L]], c("A", paste0(p(1), "b1"), paste0(p(2), "x"), "B", paste0(p(1), "b2")))
  # every one of the summaries is on its group's label row, not on a row of its own
  expect_identical(all$n, c("3", "2", "1", NA, "4"))
  expect_identical(attr(all, "rtf_stub_src"), c(3L, 2L, 1L, NA, 4L))
  expect_identical(stub_cols(d, vars = vars, group_summary = c("empty", "parent")), all)
  expect_identical(stub_cols(d, vars = vars, group_summary = c("parent", "empty")), all)
  # none, NULL and character(0) disable it: every row has its own leaf row
  none <- stub_cols(d, vars = vars, group_summary = "none")
  expect_identical(stub_cols(d, vars = vars, group_summary = NULL), none)
  expect_identical(stub_cols(d, vars = vars, group_summary = character(0)), none)
  expect_identical(nrow(none), 8L)                                    # 4 labels + 4 leaves
  expect_identical(none$n, c(NA, NA, "1", "2", "3", NA, NA, "4"))
  # a token that is none of them stops
  expect_error(stub_cols(d, vars = vars, group_summary = "bogus"),
               "`group_summary` must be a subset of c(\"empty\", \"parent\") (or \"all\" / \"none\" / NULL); got: bogus",
               fixed = TRUE)
})

test_that("FT-TB5933 [FR-TB200]: stub_cols(layout = ) merged (the default) replaces the hierarchy columns by one indented stub column; columns keeps them in their original positions with their own headers, a group value on its own row, blank on the member rows, the leaf indented in the leaf column", {
  nb <- intToUtf8(160L)
  d <- data.frame(v = c("1", "2", "3"), group = c("A", "A", "B"), label = c("x", "y", "z"),
                  stringsAsFactors = FALSE)
  expect_identical(eval(formals(stub_cols)$layout), c("merged", "columns"))
  mer <- stub_cols(d, vars = c("group", "label"))
  expect_identical(names(mer), c("group / label", "v"))               # one column, in place of both
  expect_identical(mer[[1L]], c("A", paste0(strrep(nb, 4L), "x"), paste0(strrep(nb, 4L), "y"),
                                "B", paste0(strrep(nb, 4L), "z")))
  expect_identical(stub_cols(d, vars = c("group", "label"), layout = "merged"), mer)
  col <- stub_cols(d, vars = c("group", "label"), layout = "columns")
  # the columns keep their places and names (the headers)
  expect_identical(names(col), c("v", "group", "label"))
  expect_identical(nrow(col), 5L)
  # the group value on a row of its own, blank (NA) on the member rows
  expect_identical(col$group, c("A", NA, NA, "B", NA))
  # the leaf, indented one step, in the leaf column
  expect_identical(col$label, c(NA, paste0(strrep(nb, 4L), "x"), paste0(strrep(nb, 4L), "y"),
                                NA, paste0(strrep(nb, 4L), "z")))
  expect_identical(col$v, c(NA, "1", "2", NA, "3"))
  # the stub starts at the first column when the hierarchy does
  d2 <- data.frame(group = c("A", "A"), label = c("x", "y"), v = c("1", "2"), stringsAsFactors = FALSE)
  expect_identical(names(stub_cols(d2, c("group", "label"), layout = "columns")), c("group", "label", "v"))
  # the indent is the one asked for, and own labels are the headers as_rtftables() reads
  attr(d2$group, "label") <- "Group"
  c2 <- stub_cols(d2, c("group", "label"), layout = "columns", indent = 2L)
  expect_identical(c2$label[2L], paste0(strrep(nb, 2L), "x"))
  expect_identical(attr(c2$group, "label"), "Group")
  expect_identical(as_rtftables(d2, stub = stub_spec(c("group", "label"), layout = "columns"))[[1L]]$col_header,
                   list(c("Group", "label", "v")))
  # in columns, `label` and `label_span` are contradictions, refused
  expect_error(stub_cols(d2, c("group", "label"), layout = "columns", label_span = TRUE),
               "`label_span` applies to layout = \"merged\" only", fixed = TRUE)
  expect_error(stub_cols(d2, c("group", "label"), layout = "columns", label = "L"),
               "`label` names the merged stub column", fixed = TRUE)
})

test_that("FT-TB5934 [FR-TB201]: stub_cols(label_span = TRUE) renders each group label row as one cell spanning the table, recorded as attr(out, \"rtf_label_rows\") that rtftable(read_attributes = TRUE) consumes; a summary row folded onto a label row is never spanned", {
  d <- data.frame(g = c("G1", "G1", "G2"), l = c("G1", "p", "q"), n = c("1", "2", "3"),
                  stringsAsFactors = FALSE)
  expect_false(eval(formals(stub_cols)$label_span))
  # FALSE (the default): no record
  expect_null(attr(stub_cols(d, c("g", "l")), "rtf_label_rows"))
  out <- stub_cols(d, c("g", "l"), label_span = TRUE)
  # rows: G1 (its summary "G1" is folded onto it), p, G2, q: only the label row G2 (3) is spanned
  expect_identical(attr(out, "rtf_stub_src"), c(1L, 2L, NA, 3L))
  expect_identical(attr(out, "rtf_label_rows"), 3L)
  # rtftable(read_attributes = TRUE) consumes it: that row is one cell, the others two
  r <- .render_rtftable(rtftable(out), 9360L)
  expect_identical(vapply(r, function(l) length(.t9_cx(l)), integer(1L), USE.NAMES = FALSE),
                   c(2L, 2L, 2L, 1L, 2L))                    # header, G1 (folded), p, G2, q
  expect_identical(.t9_cx(r[4L]), 9360L)
  expect_identical(.t9_tx(r[4L]), "G2")
  # the folded summary row has statistics in the other column, so it is a row of cells
  expect_length(.t9_cx(r[2L]), 2L)
  # not read when read_attributes = FALSE
  r0 <- .render_rtftable(rtftable(out, read_attributes = FALSE), 9360L)
  expect_identical(vapply(r0, function(l) length(.t9_cx(l)), integer(1L), USE.NAMES = FALSE), rep(2L, 5L))
  # as_rtftables(stub = ) does the same through stub_spec()
  pg <- as_rtftables(d, stub = stub_spec(c("g", "l"), label_span = TRUE))[[1L]]
  rp <- .render_rtftable(pg, 9360L)
  expect_identical(vapply(rp, function(l) length(.t9_cx(l)), integer(1L), USE.NAMES = FALSE),
                   c(2L, 2L, 2L, 1L, 2L))
  # without the folding every label row is spanned
  nf <- stub_cols(d, c("g", "l"), label_span = TRUE, group_summary = "none")
  expect_identical(attr(nf, "rtf_label_rows"), c(1L, 4L))     # G1 (1), its leaf G1 (2), p (3), G2 (4), q (5)
  expect_error(stub_cols(d, c("g", "l"), label_span = NA), "`label_span` must be TRUE or FALSE.", fixed = TRUE)
})

test_that("FT-TB5935 [FR-TB202]: stub_cols() keeps the label attributes of the columns that remain, gives a zero-row stub for no rows, and records the source row of each output row (NA for a label row) as rtf_stub_src", {
  d <- data.frame(g = c("G1", "G1"), l = c("p", "q"), n = c("1", "2"), stringsAsFactors = FALSE)
  attr(d$n, "label") <- "N label"
  attr(d$g, "label") <- "Group"
  for (lay in c("merged", "columns")) {
    out <- stub_cols(d, c("g", "l"), layout = lay)
    expect_identical(attr(out$n, "label"), "N label")
    expect_identical(attr(out, "rtf_stub_src"), c(NA, 1L, 2L))        # a label row, then rows 1 and 2
  }
  expect_identical(names(stub_cols(d, c("g", "l"))), c("Group / l", "n"))
  # as_rtftables() picks the label up as the header
  expect_identical(as_rtftables(d, stub = stub_spec(c("g", "l")))[[1L]]$col_header,
                   list(c("Group / l", "N label")))
  # no rows: a zero-row stub layout
  z <- stub_cols(d[0L, ], c("g", "l"))
  expect_identical(dim(z), c(0L, 2L))
  expect_identical(names(z), c("g / l", "n"))     # (a subset of rows keeps no label attributes)
  expect_identical(attr(z, "rtf_stub_src"), integer(0))
  zc <- stub_cols(d[0L, ], c("g", "l"), layout = "columns")
  expect_identical(dim(zc), c(0L, 3L))
  expect_identical(names(zc), c("g", "l", "n"))
  expect_identical(attr(zc, "rtf_stub_src"), integer(0))
  # a single var: every row is its own, the source rows 1..n
  one <- stub_cols(d, "l")
  expect_identical(attr(one, "rtf_stub_src"), 1:2)
})

test_that("FT-TB5936 [FR-TB207]: stub_spec(vars, label, indent, group_summary, layout, label_span) returns an rtf_stub_spec carrying every stub_cols() setting, for as_rtftables(stub = )", {
  sp <- stub_spec(c("a", "b"), label = "Lab", indent = 2, group_summary = "all",
                  layout = "merged", label_span = TRUE)
  expect_s3_class(sp, "rtf_stub_spec")
  expect_identical(names(unclass(sp)), c("vars", "label", "indent", "group_summary", "layout", "label_span"))
  expect_identical(sp$vars, c("a", "b"))
  expect_identical(sp$label, "Lab")
  expect_identical(sp$indent, 2)
  expect_identical(sp$group_summary, "all")
  expect_identical(sp$layout, "merged")
  expect_true(sp$label_span)
  expect_identical(names(formals(stub_spec)), c("vars", "label", "indent", "group_summary", "layout", "label_span"))
  # as_rtftables(stub = ) builds the stub with every one of them
  d <- data.frame(a = c("A", "A", "B"), b = c("A", "x", "y"), n = c("1", "2", "3"), stringsAsFactors = FALSE)
  pg <- as_rtftables(d, stub = sp)[[1L]]
  ref <- stub_cols(d, vars = c("a", "b"), label = "Lab", indent = 2, group_summary = "all",
                   layout = "merged", label_span = TRUE)
  expect_identical(names(pg$data), c("Lab", "n"))
  expect_identical(pg$data[[1L]], ref[[1L]])
  expect_identical(pg$data$n, ref$n)
  expect_identical(pg$data[[1L]][2L], paste0(strrep(intToUtf8(160L), 2L), "x"))     # indent 2
  expect_identical(nrow(pg$data), 4L)                                               # A's summary folded
  rr <- .render_rtftable(pg, 9360L)
  expect_identical(vapply(rr, function(l) length(.t9_cx(l)), integer(1L), USE.NAMES = FALSE), c(2L, 2L, 2L, 1L, 2L))
  # the other layout, as a column-layout spec
  sc <- stub_spec(c("a", "b"), layout = "columns", indent = 1, group_summary = "none")
  pc <- as_rtftables(d, stub = sc)[[1L]]
  expect_identical(names(pc$data), c("a", "b", "n"))
  expect_identical(pc$data$b[2L], paste0(intToUtf8(160L), "A"))
  expect_identical(nrow(pc$data), 5L)                      # A, A (leaf), x, B, y: nothing folded
})

test_that("FT-TB5937 [FR-TB211]: print() of an rtf_stub_spec shows the vars, layout and indent, and the label and label_span lines when set", {
  plain <- capture.output(print(stub_spec(c("SOC", "PT"))))
  expect_identical(plain[1L], "<rtf_stub_spec>")
  expect_match(plain[2L], "vars +: SOC, PT")
  expect_match(plain[3L], "layout +: merged")
  expect_match(plain[4L], "indent +: 4")
  expect_length(plain, 4L)                                     # no label, no label_span line
  full <- capture.output(print(stub_spec(c("SOC", "PT"), label = "Term", indent = 2,
                                         layout = "merged", label_span = TRUE)))
  expect_match(full[2L], "vars +: SOC, PT")
  expect_match(full[3L], "layout +: merged")
  expect_match(full[4L], "indent +: 2")
  expect_match(full[5L], "label +: Term")
  expect_match(full[6L], "label_span +: TRUE")
  expect_length(full, 6L)
  only_label <- capture.output(print(stub_spec(c("SOC", "PT"), label = "Term")))
  expect_length(only_label, 5L)
  expect_match(only_label[5L], "label +: Term")
  expect_false(any(grepl("label_span", only_label)))
  cols <- capture.output(print(stub_spec("SOC", layout = "columns", indent = 0)))
  expect_match(cols[2L], "vars +: SOC")
  expect_match(cols[3L], "layout +: columns")
  expect_match(cols[4L], "indent +: 0")
  expect_length(cols, 4L)
})

# ---- paginate_cols() -------------------------------------------------------------

.t9_pc_df <- function() {
  data.frame(P = c("a", "b"), A_n = c("1", "2"), A_m = c("3", "4"), B_n = c("5", "6"),
             B_m = c("7", "8"), stringsAsFactors = FALSE)
}
.t9_pc_hdr <- function() {
  list(list(col_cell(c(2, 3), "Alpha"), col_cell(c(4, 5), "Beta")),
       c("P", "n", "m", "n", "m"))
}
.t9_pc_names <- function(pages) unname(lapply(pages, function(p) names(p$data)))

test_that("FT-TB5938 [FR-TB271]: paginate_cols(x, ...) cuts built pages into column blocks and returns a list of rtftable pages, the row-heading columns repeated on every block and the header, widths, col_spec, cell_styles, row_title and blank rows following the kept columns", {
  tb <- rtftable(.t9_pc_df(), col_header = .t9_pc_hdr(),
                 column_widths_twips = c(1000L, 1100L, 1200L, 1300L, 1400L),
                 col_spec = list(list(col = 1, bold = TRUE), list(col = 3, align = "right"),
                                 list(col = 5, italic = TRUE)),
                 cell_styles = list(list(bold = c(NA, TRUE, NA, NA, TRUE)), NULL),
                 blank_rows = 1L)
  pg <- paginate_cols(tb, at = 4, fit = FALSE)
  expect_type(pg, "list")
  expect_length(pg, 2L)
  for (p in pg) expect_s3_class(p, "rtftable")
  # the row-heading column P is on both blocks; columns 2-3 and 4-5 are the blocks
  expect_identical(.t9_pc_names(pg), list(c("P", "A_n", "A_m"), c("P", "B_n", "B_m")))
  expect_identical(pg[[2L]]$data$P, c("a", "b"))
  # the header follows the kept columns, the spanning cells with them
  expect_identical(pg[[1L]]$col_header,
                   list(list(list(from = 1L, to = 1L, label = ""), list(from = 2L, to = 3L, label = "Alpha")),
                        c("P", "n", "m")))
  expect_identical(pg[[2L]]$col_header,
                   list(list(list(from = 1L, to = 1L, label = ""), list(from = 2L, to = 3L, label = "Beta")),
                        c("P", "n", "m")))
  # the widths: each column keeps its own
  expect_identical(pg[[1L]]$column_widths_twips, c(1000L, 1100L, 1200L))
  expect_identical(pg[[2L]]$column_widths_twips, c(1000L, 1300L, 1400L))
  # col_spec: P bold; column 3 right-aligned and column 5 italic go with their columns
  spec <- function(p, f) vapply(p$col_spec, function(s) s[[f]], if (f == "align") "" else NA)
  expect_identical(spec(pg[[1L]], "align"), c("left", "center", "right"))
  expect_identical(spec(pg[[2L]], "align"), c("left", "center", "center"))
  expect_identical(spec(pg[[1L]], "bold"), c(TRUE, FALSE, FALSE))
  expect_identical(spec(pg[[2L]], "bold"), c(TRUE, FALSE, FALSE))
  expect_identical(spec(pg[[1L]], "italic"), c(FALSE, FALSE, FALSE))
  expect_identical(spec(pg[[2L]], "italic"), c(FALSE, FALSE, TRUE))
  # cell_styles: row 1 bold in columns 2 and 5: kept columns 1,2,3 -> NA TRUE NA, 1,4,5 -> NA NA TRUE
  expect_identical(pg[[1L]]$cell_styles[[1L]]$bold, c(NA, TRUE, NA))
  expect_identical(pg[[2L]]$cell_styles[[1L]]$bold, c(NA, NA, TRUE))
  expect_null(pg[[1L]]$cell_styles[[2L]])
  expect_null(pg[[2L]]$cell_styles[[2L]])
  # row_title and blank rows: as before
  expect_identical(pg[[1L]]$row_title, 1L)
  expect_identical(pg[[2L]]$row_title, 1L)
  expect_identical(pg[[1L]]$blank_rows, 1L)
  expect_identical(pg[[2L]]$blank_rows, 1L)
  # two row-heading columns are both repeated, and stay the row_title
  t2 <- rtftable(.t9_pc_df(), row_title = c(1, 2))
  p2 <- paginate_cols(t2, at = 4, fit = FALSE)
  expect_identical(.t9_pc_names(p2), list(c("P", "A_n", "A_m"), c("P", "A_n", "B_n", "B_m")))
  expect_identical(p2[[2L]]$row_title, 1:2)
  # keep = ... overrides which columns repeat
  p3 <- paginate_cols(t2, at = 4, fit = FALSE, keep = 1L)
  expect_identical(.t9_pc_names(p3), list(c("P", "A_n", "A_m"), c("P", "B_n", "B_m")))
  # the rendered page of a block has only its columns: the widths of the kept and the block's
  r <- .render_rtftable(pg[[2L]], 3700L)
  expect_identical(.t9_cx(r[length(r)]), c(1000L, 2300L, 3700L))
})

test_that("FT-TB5939 [FR-TB274]: paginate_cols(cut_by = ) takes a list of column blocks, a separator found in the column names, or one key per column, each run of one key being a block", {
  d <- .t9_pc_df()
  # a list of column blocks, by position or by name; the kept column is added to each
  expect_identical(.t9_pc_names(paginate_cols(rtftable(d), cut_by = list(2:3, 4:5), fit = FALSE)),
                   list(c("P", "A_n", "A_m"), c("P", "B_n", "B_m")))
  expect_identical(.t9_pc_names(paginate_cols(rtftable(d), cut_by = list(c("A_n", "A_m"), "B_n"), fit = FALSE)),
                   list(c("P", "A_n", "A_m"), c("P", "B_n")))
  # a separator: the part of the name before it is the key; a run of one key is a block
  expect_identical(.t9_pc_names(paginate_cols(rtftable(d), cut_by = "_", fit = FALSE)),
                   list(c("P", "A_n", "A_m"), c("P", "B_n", "B_m")))
  # the same key in runs that are not adjacent: a block for each run
  d2 <- d[c("P", "A_n", "B_n", "A_m", "B_m")]
  expect_identical(.t9_pc_names(paginate_cols(rtftable(d2), cut_by = "_", fit = FALSE)),
                   list(c("P", "A_n"), c("P", "B_n"), c("P", "A_m"), c("P", "B_m")))
  # one key per column (as many as there are columns); the kept column's key is not used
  expect_identical(.t9_pc_names(paginate_cols(rtftable(d), cut_by = c("k", "a", "a", "b", "b"), fit = FALSE)),
                   list(c("P", "A_n", "A_m"), c("P", "B_n", "B_m")))
  expect_identical(.t9_pc_names(paginate_cols(rtftable(d), cut_by = c("k", "a", "b", "b", "a"), fit = FALSE)),
                   list(c("P", "A_n"), c("P", "A_m", "B_n"), c("P", "B_m")))
  expect_identical(.t9_pc_names(paginate_cols(rtftable(d), cut_by = c("a", "a", "a", "b", "b"), fit = FALSE)),
                   list(c("P", "A_n", "A_m"), c("P", "B_n", "B_m")))
  # nothing kept: the first column belongs to a block of its own
  expect_identical(.t9_pc_names(paginate_cols(rtftable(d), cut_by = "_", keep = integer(0), fit = FALSE)),
                   list("P", c("A_n", "A_m"), c("B_n", "B_m")))
  # none of the three: stops
  expect_error(paginate_cols(rtftable(d), cut_by = c("a", "b")),
               "`cut_by` must be a list of column blocks, a single separator found in the column names, or one key per column (5).",
               fixed = TRUE)
  # on every page of a list
  out <- paginate_cols(list(a = rtftable(d), b = rtftable(d)), cut_by = "_", fit = FALSE)
  expect_identical(names(out), c("a...1", "b...1", "a...2", "b...2"))
})

test_that("FT-TB5940 [FR-TB281]: paginate_cols(col_header = ) is the header each page carries, written once in the full table's coordinates and sliced to the page's columns with spanning cells clipped; 'names' builds the two-level header the names carry; NULL slices the table's own header", {
  d <- .t9_pc_df()
  hdr <- function(p) p$col_header
  # a label row, in the full table's coordinates (5 labels), sliced to the page's columns
  lab <- paginate_cols(rtftable(d), at = 4, fit = FALSE, col_header = c("Lbl", "a", "b", "c", "d"))
  expect_identical(hdr(lab[[1L]]), list(c("Lbl", "a", "b")))
  expect_identical(hdr(lab[[2L]]), list(c("Lbl", "c", "d")))
  # rows of col_cell(): a cell over columns 2-4 is clipped to the columns each page holds
  span <- list(list(col_cell(c(2, 4), "Wide")), c("Lbl", "a", "b", "c", "d"))
  sp <- paginate_cols(rtftable(d), at = 4, fit = FALSE, col_header = span)
  expect_identical(hdr(sp[[1L]]),
                   list(list(list(from = 1L, to = 1L, label = ""), list(from = 2L, to = 3L, label = "Wide")),
                        c("Lbl", "a", "b")))
  # page 2 holds P and B_n (full column 4) and B_m: the cell covers its column 2 only
  expect_identical(hdr(sp[[2L]]),
                   list(list(list(from = 1L, to = 1L, label = ""), list(from = 2L, to = 2L, label = "Wide"),
                             list(from = 3L, to = 3L, label = "")),
                        c("Lbl", "c", "d")))
  # NULL (the default) slices the table's own header
  own <- paginate_cols(rtftable(d, col_header = .t9_pc_hdr()), at = 4, fit = FALSE)
  expect_identical(hdr(own[[1L]]),
                   list(list(list(from = 1L, to = 1L, label = ""), list(from = 2L, to = 3L, label = "Alpha")),
                        c("P", "n", "m")))
  expect_identical(hdr(own[[2L]]),
                   list(list(list(from = 1L, to = 1L, label = ""), list(from = 2L, to = 3L, label = "Beta")),
                        c("P", "n", "m")))
  # the header given replaces the table's own
  rep <- paginate_cols(rtftable(d, col_header = .t9_pc_hdr()), at = 4, fit = FALSE,
                       col_header = c("Lbl", "a", "b", "c", "d"))
  expect_identical(hdr(rep[[1L]]), list(c("Lbl", "a", "b")))
  # "names": the two-level header the column names carry; the kept column keeps its name
  # and sits under no spanning cell (an empty cell over column 1)
  w <- data.frame(P = "x", A____n = "1", A____m = "2", B____n = "3", B____m = "4",
                  check.names = FALSE, stringsAsFactors = FALSE)
  nm <- paginate_cols(rtftable(w), cut_by = "____", col_header = "names", fit = FALSE)
  expect_length(nm, 2L)
  expect_identical(hdr(nm[[1L]]),
                   list(list(list(from = 1L, to = 1L, label = ""), list(from = 2L, to = 3L, label = "A")),
                        c("P", "n", "m")))
  expect_identical(hdr(nm[[2L]]),
                   list(list(list(from = 1L, to = 1L, label = ""), list(from = 2L, to = 3L, label = "B")),
                        c("P", "n", "m")))
  # without it the table's own header (none here) is sliced: none
  expect_null(hdr(paginate_cols(rtftable(w), cut_by = "____", fit = FALSE)[[1L]]))
  # "names" needs the separator form of cut_by
  expect_error(paginate_cols(rtftable(w), at = 4, col_header = "names"),
               "`col_header = \"names\"` needs `cut_by` to be the separator in the column names (e.g. cut_by = \"____\").",
               fixed = TRUE)
})

test_that("FT-TB5941 [FR-TB251]: header_map(x) has one row per header cell of every page, with the columns page, name, group, rows, row, cell, from, to and text; a label-row cell covers one column, a spanning cell from..to", {
  d <- data.frame(period = c("P1", "P1", "P2"), pg = c("1", "2", "1"), lab = c("x", "y", "z"),
                  v1 = "1", v2 = "2", stringsAsFactors = FALSE)
  pages <- as_rtftables(d, split = "by_value", group_col = "period", page_by = "pg",
                        drop_cols = c("pg", "period"))
  expect_identical(names(pages), c("P1...1", "P1...2", "P2"))
  pages <- set_col_header(pages, list(col_cell(2:3, "Grp")), c("Lab", "a", "b"))
  hm <- header_map(pages)
  expect_s3_class(hm, "data.frame")
  expect_identical(names(hm), c("page", "name", "group", "rows", "row", "cell", "from", "to", "text"))
  # per page: the spanning row has 2 cells (the empty one over column 1, and "Grp") and the
  # label row 3: 5 header cells a page, 3 pages
  expect_identical(nrow(hm), 15L)
  first <- hm[hm$page == 1L, ]
  expect_identical(first$name, rep("P1...1", 5L))   # P2 is one page, so it has no tail
  expect_identical(first$group, rep("P1", 5L))
  expect_identical(first$rows, rep("1", 5L))
  expect_identical(first$row, c(1L, 1L, 2L, 2L, 2L))
  expect_identical(first$cell, c(1L, 2L, 1L, 2L, 3L))
  expect_identical(first$from, c(1L, 2L, 1L, 2L, 3L))
  expect_identical(first$to, c(1L, 3L, 1L, 2L, 3L))          # the spanning cell 2..3; each label one column
  expect_identical(first$text, c("", "Grp", "Lab", "a", "b"))
  expect_identical(unique(hm$page), 1:3)
  expect_identical(unique(hm$name), c("P1...1", "P1...2", "P2"))
  expect_identical(hm$text[hm$page == 3L], first$text)
  # a label-row cell is from == to everywhere; only the spanning cell is wider
  expect_identical(hm$from[hm$row == 2L], hm$to[hm$row == 2L])
  expect_true(all(hm$to[hm$row == 1L & hm$text == "Grp"] > hm$from[hm$row == 1L & hm$text == "Grp"]))
})

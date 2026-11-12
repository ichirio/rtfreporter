# Thickening of the functional tests, batch 8: the listing tools (area LS) and
# the assembly tools (area AS).  Each block asserts what its FR states, the
# figures worked out in a comment.  FT ids FT-LS5801 and FT-AS5801 up.

# ------------------------------------------------------------------- LS

.t8_df <- function() {
  data.frame(A = c("Alpha", "Beta"), B = c("one two three", "x"),
             C = c("c1", "c2"), stringsAsFactors = FALSE)
}

test_that("FT-LS5801 [FR-LS118]: listing_spec() holds the columns and every setting resolved against the template; printing shows them", {
  sp <- listing_spec(list(listing_col(c("A", "B"), width = 8, name = "AB"), listing_col("C")))
  expect_s3_class(sp, "rtf_listing_spec")
  expect_identical(vapply(sp$cols, function(cl) cl$name, ""), c("AB", "C"))
  # every setting of the "multiline" template is resolved, none left NULL
  expect_identical(sp$type, "multiline")
  expect_identical(sp$sep, "/")
  expect_true(sp$spacer)
  expect_identical(sp$spacer_rel_width, 1)
  expect_true(sp$blank_row)
  expect_true(sp$blank_row_first)
  expect_identical(sp$align, "left")
  expect_identical(sp$layout, "stack")
  expect_true(is.function(sp$wrap))
  expect_identical(sp$record_col, ".rtf_record")
  # a setting given overrides the template's
  over <- listing_spec(list(listing_col("C")), sep = "-", spacer = FALSE, align = "right")
  expect_identical(c(over$sep, over$align), c("-", "right"))
  expect_false(over$spacer)
  # printing shows the columns and the settings it will use
  out <- utils::capture.output(print(sp))
  expect_identical(out[1L], "<rtf_listing_spec>")
  expect_identical(out[2L], "  type    : multiline")
  expect_identical(out[3L], "  columns : 2 (+ gutters)")
  expect_identical(out[4L], "    - AB  <- A / B  [wrap 8]")
  expect_identical(out[5L], "    - C  <- C")
  expect_identical(out[6L], "  record  : .rtf_record")
  expect_identical(utils::capture.output(print(over))[3L], "  columns : 1")
})

test_that("FT-LS5802 [FR-LS129]: wrap = f lays out cells and derived headers alike, one variable at a time under stack; NULL and listing_wrap change nothing", {
  d <- .t8_df()
  rec <- new.env()
  rec$calls <- list()
  spy <- function(text, width, sep, layout) {
    rec$calls[[length(rec$calls) + 1L]] <- list(text = text, width = width, sep = sep, layout = layout)
    paste0("<", text, ">")
  }
  texts <- function() vapply(rec$calls, function(z) z$text, "")
  sp <- listing_spec(list(listing_col(c("A", "C"), width = 12, name = "AC")), wrap = spy,
                     spacer = FALSE, blank_row = FALSE)
  b <- build_listing(d, sp)
  # the cells come back through the function, one call per variable's value,
  # the separator kept at the end of every part but the last
  expect_identical(b$AC, c("<Alpha/>", "<c1>", "<Beta/>", "<c2>"))
  expect_true(all(c("Alpha/", "c1", "Beta/", "c2") %in% texts()))
  # the derived header (the names A and C) goes through it too
  expect_identical(attr(b, "rtf_listing", exact = TRUE)$cols[[1L]]$label, "<A/>
<C>")
  expect_true(all(c("A/", "C") %in% texts()))
  # under flow, or with no width, the whole composed cell is one call
  rec$calls <- list()
  build_listing(d, listing_spec(list(listing_col(c("A", "C"), width = 12, layout = "flow")), wrap = spy))
  expect_true("Alpha/c1" %in% texts())
  expect_false("Alpha/" %in% texts())
  rec$calls <- list()
  build_listing(d, listing_spec(list(listing_col(c("A", "C"))), wrap = spy))
  expect_true("Alpha/c1" %in% texts())
  expect_null(rec$calls[[1L]]$width)
  # NULL (the default) uses the type's own rule, and so does wrap = listing_wrap
  cols <- list(listing_col(c("A", "B"), width = 8, name = "AB"), listing_col("C", width = 3))
  expect_false(listing_spec(cols)$wrap_custom)
  expect_true(listing_spec(cols, wrap = spy)$wrap_custom)
  expect_identical(build_listing(d, listing_spec(cols, wrap = listing_wrap))$AB,
                   build_listing(d, listing_spec(cols))$AB)
  expect_identical(build_listing(d, listing_spec(cols, wrap = listing_wrap))$AB[1:5],
                   c("Alpha/", "one two", "three", "", "Beta/"))
  expect_identical(build_listing(d, listing_spec(cols, wrap = NULL)), build_listing(d, listing_spec(cols)))
})

test_that("FT-LS5803 [FR-LS130]: a custom wrap is called positionally with (text, width, sep, layout) and returns one element per line", {
  d <- data.frame(A = c("x1", "p1"), B = c("y", "q"), stringsAsFactors = FALSE)
  seen <- list()
  # the formals are named a, b, c, d: only a positional call can reach them
  fn <- function(a, b, c, d) {
    seen[[length(seen) + 1L]] <<- list(text = a, width = b, sep = c, layout = d)
    c(a, "second")
  }
  sp <- listing_spec(list(listing_col(c("A", "B"), width = 10, name = "AB", sep = "+")),
                     wrap = fn, spacer = FALSE, blank_row = FALSE)
  b <- build_listing(d, sp)
  # the function's lines are the physical rows: header label "A+"/"B", cells
  # "x1+", "second", "y", "second" for record 1 (each variable gets its two lines)
  expect_identical(b$AB, c("x1+", "second", "y", "second", "p1+", "second", "q", "second"))
  # text: a length-1 character; width: a positive number; sep: the column's own
  # separator; layout: resolved ("flow" for a variable's value under stack)
  expect_true(all(vapply(seen, function(z) is.character(z$text) && length(z$text) == 1L, TRUE)))
  expect_true(all(vapply(seen, function(z) is.numeric(z$width) && z$width > 0, TRUE)))
  expect_true(all(vapply(seen, function(z) identical(z$sep, "+"), TRUE)))
  expect_true(all(vapply(seen, function(z) z$layout %in% c("stack", "flow"), TRUE)))
  expect_identical(vapply(seen, function(z) z$layout, "")[1:3], c("flow", "flow", "flow"))
  # a column with no width gets NULL; the spec's layout and sep fill in what the column leaves out
  seen <- list()
  build_listing(d, listing_spec(list(listing_col("A")), wrap = fn, layout = "flow", sep = "|"))
  expect_null(seen[[1L]]$width)
  expect_identical(seen[[1L]]$layout, "flow")
  expect_identical(seen[[1L]]$sep, "|")
  # a column's own layout beats the spec's: with no width the cell is one call, "stack" as resolved
  seen <- list()
  build_listing(d, listing_spec(list(listing_col("A", layout = "stack")), wrap = fn, layout = "flow"))
  expect_identical(seen[[1L]]$layout, "stack")
})

test_that("FT-LS5804 [FR-LS131]: record = TRUE / FALSE / a name sets the record column; as_rtftables(listing = ) keeps a record on one page and does not print it", {
  # three records of three physical rows (two lines and the blank row) each
  d <- data.frame(A = c("x1", "p1", "r1"), B = c("y", "q", "s"), stringsAsFactors = FALSE)
  cols <- list(listing_col(c("A", "B"), width = 5, name = "AB"))
  sp <- listing_spec(cols, spacer = FALSE, blank_row_first = FALSE)
  expect_identical(sp$record_col, ".rtf_record")
  expect_identical(names(build_listing(d, sp)), c("AB", ".rtf_record"))
  expect_identical(names(build_listing(d, listing_spec(cols, spacer = FALSE, record = "REC"))),
                   c("AB", "REC"))
  expect_identical(names(build_listing(d, listing_spec(cols, spacer = FALSE, record = FALSE))),
                   c("AB"))
  expect_null(listing_spec(cols, record = FALSE)$record_col)
  # group_safe on the record: max_rows = 7 takes two whole records (6 rows) on
  # the first page; the third record (3 rows) would make 9 > 7 and starts the second
  for (rc in list(TRUE, "REC")) {
    pages <- as_rtftables(d, listing = listing_spec(cols, spacer = FALSE, blank_row_first = FALSE,
                                                    record = rc), max_rows = 7)
    expect_identical(vapply(pages, function(p) nrow(p$data), 1L), c(6L, 3L), info = as.character(rc))
    # the record column is not printed
    expect_identical(names(pages[[1L]]$data), "AB", info = as.character(rc))
    expect_identical(pages[[2L]]$data$AB, c("r1/", "s", ""), info = as.character(rc))
  }
})

test_that("FT-LS5805 [FR-LS142]: listing_col(\"USUBJID\") takes the first var as the name, no width, no label, and the listing's own sep, align and layout; printing shows it", {
  cl <- listing_col("USUBJID")
  expect_s3_class(cl, "rtf_listing_col")
  expect_identical(cl$vars, "USUBJID")
  expect_identical(cl$name, "USUBJID")
  expect_null(cl$width)
  expect_null(cl$label)
  expect_null(cl$sep)
  expect_null(cl$align)
  expect_null(cl$layout)
  expect_identical(utils::capture.output(print(cl)),
                   c("<rtf_listing_col>", "  name  : USUBJID", "  vars  : USUBJID", "  width : (no wrap)"))
  # the listing's own sep, align and layout are what the column then uses
  d <- .t8_df()
  sp <- listing_spec(list(listing_col(c("A", "C"), name = "AC")), sep = "-", align = "right",
                     layout = "flow", spacer = FALSE)
  b <- build_listing(d, sp)
  expect_identical(b$AC[1L], "Alpha-c1")                      # sep "-" of the listing, one line (flow)
  tb <- as_rtftables(d, listing = sp)[[1L]]
  expect_identical(tb$col_spec[[1L]]$align, "right")
  # no width: the cell never wraps, however long
  long <- data.frame(A = strrep("word ", 30), stringsAsFactors = FALSE)
  expect_identical(build_listing(long, listing_spec(list(listing_col("A")), spacer = FALSE))$A[1L],
                   trimws(strrep("word ", 30)))
})

test_that("FT-LS5806 [FR-LS148]: a label that is one string with no breaks is wrapped to width by the cells' rule, or left on one line with no width", {
  d <- data.frame(PT = "x", AGESEX = "y", stringsAsFactors = FALSE)
  lab <- function(...) {
    sp <- listing_spec(list(listing_col(...)), spacer = FALSE)
    attr(build_listing(d, sp), "rtf_listing", exact = TRUE)$cols[[1L]]$label
  }
  # "Preferred term of the event" in 16: "Preferred term of" is 17 wide, so the
  # line ends after "term"
  expect_identical(lab("PT", label = "Preferred term of the event", width = 16),
                   "Preferred term\nof the event")
  # no width: one line
  expect_identical(lab("PT", label = "Preferred term of the event"), "Preferred term of the event")
  # a "/" in the label of a one-variable column is words: it breaks after the "/" only when too wide
  expect_identical(lab("AGESEX", label = "Age/Sex", width = 10), "Age/Sex")
  expect_identical(lab("AGESEX", label = "Age/Sex", width = 5), "Age/\nSex")
  # the header the table prints is that wrapped label
  sp <- listing_spec(list(listing_col("PT", label = "Preferred term of the event", width = 16)),
                     spacer = FALSE)
  expect_identical(as_rtftables(d, listing = sp)[[1L]]$col_header[[1L]][[1L]], "Preferred term\nof the event")
})

test_that("FT-LS5807 [FR-LS149]: label = NULL derives the header from the data's label attributes, else the names, joined with sep and wrapped like the cells, or from fit_listing_widths(labels = )", {
  d <- data.frame(A = "Alpha", B = "x", stringsAsFactors = FALSE)
  attr(d$A, "label") <- "Subject ID"                       # B has none: its name is used
  lab <- function(width = NULL, ...) {
    sp <- listing_spec(list(listing_col(c("A", "B"), width = width, name = "AB", ...)), spacer = FALSE)
    attr(build_listing(d, sp), "rtf_listing", exact = TRUE)$cols[[1L]]$label
  }
  # no width: the labels joined with sep, on one line
  expect_identical(lab(), "Subject ID/B")
  expect_identical(lab(sep = " | "), "Subject ID | B")
  # width 8: each variable's label on its own line, wrapped as the cells are
  # ("Subject ID/" is 11 wide: it breaks at the space, the separator staying with "ID")
  expect_identical(lab(8L), "Subject\nID/\nB")
  # a single variable: the attribute, else the name
  one <- function(v) attr(build_listing(d, listing_spec(list(listing_col(v)), spacer = FALSE)),
                          "rtf_listing", exact = TRUE)$cols[[1L]]$label
  expect_identical(one("A"), "Subject ID")
  expect_identical(one("B"), "B")
  # the header the table prints
  tb <- as_rtftables(d, listing = listing_spec(list(listing_col(c("A", "B"), width = 8, name = "AB")),
                                               spacer = FALSE))[[1L]]
  expect_identical(tb$col_header[[1L]][[1L]], "Subject\nID/\nB")
  # from fit_listing_widths(labels = ) when the data carries no labels
  plain <- data.frame(USUBJID = c("A1", "A2"), stringsAsFactors = FALSE)
  sp <- listing_spec(list(listing_col("USUBJID")))
  expect_identical(fit_listing_widths(plain, sp, labels = c(USUBJID = "Unique Subject ID"),
                                      total_width = 60)$cols[[1L]]$label, "Unique Subject ID")
  expect_identical(fit_listing_widths(plain, sp, total_width = 60)$cols[[1L]]$label, "USUBJID")
})

test_that("FT-LS5808 [FR-LS152]: listing_col(name = ) is the output column name; NULL uses the first var", {
  d <- .t8_df()
  nm <- function(cl) names(build_listing(d, listing_spec(list(cl), spacer = FALSE, record = FALSE)))
  expect_identical(nm(listing_col("A", name = "COL01")), "COL01")
  expect_identical(nm(listing_col(c("B", "A"), name = "COL01")), "COL01")
  expect_identical(nm(listing_col("A")), "A")
  expect_identical(nm(listing_col(c("B", "A"))), "B")
  expect_identical(listing_col(c("B", "A"), name = "COL01")$name, "COL01")
  expect_identical(listing_col(c("B", "A"))$name, "B")
  # the name is what the body is addressed by, and the cells are the joined values
  expect_identical(build_listing(d, listing_spec(list(listing_col(c("A", "C"), name = "COL01")),
                                                 spacer = FALSE))$COL01[1L], "Alpha/c1")
  # two columns on the same first var are kept apart by the names given
  sp <- listing_spec(list(listing_col("A", name = "X"), listing_col("A", name = "Y")), spacer = FALSE,
                     record = FALSE)
  expect_identical(names(build_listing(d, sp)), c("X", "Y"))
})

test_that("FT-LS5809 [FR-LS153]: rel_width is the relative column width; NULL uses width, else the longest label line, else 10", {
  d <- data.frame(A = "a", stringsAsFactors = FALSE)
  rw <- function(cl) as_rtftables(d, listing = listing_spec(list(cl), spacer = FALSE))[[1L]]$col_rel_width
  expect_identical(rw(listing_col("A", rel_width = 12, width = 5)), 12)       # given: wins over width
  expect_identical(rw(listing_col("A", rel_width = 0.5)), 0.5)               # a fraction
  expect_identical(rw(listing_col("A", width = 5, label = "a long label")), 5)  # NULL: the width
  expect_identical(rw(listing_col("A", label = c("ab", "abcdef"))), 6)       # no width: the longest label line
  expect_identical(rw(listing_col("A", label = "abc")), 3)
  expect_identical(rw(listing_col("A", label = "")), 10)                     # nothing to go by: 10
  # the gutters between columns take spacer_rel_width
  two <- listing_spec(list(listing_col("A", rel_width = 7), listing_col("A", name = "A2", width = 4)))
  expect_identical(as_rtftables(d, listing = two)[[1L]]$col_rel_width, c(7, 1, 4))
})

test_that("FT-LS5810 [FR-LS154]: listing_col(align = ) is the column's alignment; NULL takes the listing's own", {
  d <- .t8_df()
  al <- function(spec) vapply(as_rtftables(d, listing = spec)[[1L]]$col_spec, function(s) s$align, "")
  cols <- list(listing_col("A", align = "right"), listing_col("B", align = "center"), listing_col("C"))
  # gutters between: .sp1, .sp2 take the listing's align
  expect_identical(al(listing_spec(cols)), c("right", "left", "center", "left", "left"))
  # the listing's own align fills the NULL
  expect_identical(al(listing_spec(cols, align = "center")), c("right", "center", "center", "center", "center"))
  expect_identical(listing_col("A", align = "left")$align, "left")
  expect_null(listing_col("A")$align)
  expect_identical(listing_spec(list(listing_col("A")))$align, "left")       # "left" under multiline
})

test_that("FT-LS5811 [FR-LS171]: listing_wrap() is the multiline rule; on one string stack breaks after every separator, a listing calls it per value as flow", {
  d <- data.frame(A = c("Alpha", "Beta"), B = c("one two three", "x"), C = c("a/b/c", "d"),
                  stringsAsFactors = FALSE)
  cols <- function() list(listing_col(c("A", "B"), width = 8, name = "AB"), listing_col("C", width = 20))
  as_chr <- function(x) lapply(x, as.character)
  expect_identical(as_chr(build_listing(d, listing_spec(cols(), wrap = listing_wrap))),
                   as_chr(build_listing(d, listing_spec(cols()))))
  expect_identical(as_chr(build_listing(d, listing_spec(cols(), wrap = function(...) listing_wrap(...)))),
                   as_chr(build_listing(d, listing_spec(cols()))))
  # on one string: stack breaks after every separator, flow keeps what fits
  expect_identical(listing_wrap("a/b/c", 20, "/"), c("a/", "b/", "c"))
  expect_identical(listing_wrap("a/b/c", 20, "/", layout = "stack"), c("a/", "b/", "c"))
  expect_identical(listing_wrap("a/b/c", 20, "/", layout = "flow"), "a/b/c")
  # in a listing the column C = "a/b/c" is one value laid out as flow: one line, not three
  expect_identical(build_listing(d, listing_spec(cols(), spacer = FALSE))$C[1:2], c("a/b/c", ""))
  seen <- character(0)
  spy <- function(text, width, sep, layout) {
    if (identical(text, "a/b/c")) seen <<- c(seen, layout)
    listing_wrap(text, width, sep, layout)
  }
  build_listing(d, listing_spec(cols(), wrap = spy))
  expect_identical(seen, "flow")
})

test_that("FT-LS5812 [FR-LS172]: listing_wrap() breaks first after the separator, which stays on the line it closes; sep = \"\" or NULL skips it; \".\" is literal", {
  # "Nausea/and vomiting" in 8: after the "/" (kept on the first line), then at the word
  expect_identical(listing_wrap("Nausea/and vomiting", 8, "/"), c("Nausea/", "and", "vomiting"))
  expect_identical(listing_wrap("Nausea/and vomiting", 8), c("Nausea/", "and", "vomiting"))   # "/" is the default
  # no separator step: hard split at 8 characters, then the word break
  expect_identical(listing_wrap("Nausea/and vomiting", 8, ""), c("Nausea/a", "nd", "vomiting"))
  expect_identical(listing_wrap("Nausea/and vomiting", 8, NULL), c("Nausea/a", "nd", "vomiting"))
  # a regex-special separator is literal: "." does not match any character
  expect_identical(listing_wrap("A.BBBBBBBB", 4, "."), c("A.", "BBBB", "BBBB"))
  expect_identical(listing_wrap("AxBBBBBBBB", 4, "."), c("AxBB", "BBBB", "BB"))
  # another separator
  expect_identical(listing_wrap("A|B|C", 3, "|"), c("A|", "B|", "C"))
})

test_that("FT-LS5813 [FR-LS173]: listing_wrap() breaks a piece still too wide after a space, a comma or a hyphen, filling each line as far as width allows", {
  # after a space: "aa " + "bb " = 6 fits in 6, "cc " does not; "cc dd" is 5
  expect_identical(listing_wrap("aa bb cc dd", 6, ""), c("aa bb", "cc dd"))
  # after a comma
  expect_identical(listing_wrap("ab,cd,ef,gh", 6, ""), c("ab,cd,", "ef,gh"))
  # after a hyphen
  expect_identical(listing_wrap("ab-cd-ef-gh", 6, ""), c("ab-cd-", "ef-gh"))
  # a piece that fits is one line; every line fits the width
  expect_identical(listing_wrap("aa bb", 6, ""), "aa bb")
  lines <- listing_wrap("UPPER RESPIRATORY TRACT INFECTION", 16, "/")
  expect_identical(lines, c("UPPER", "RESPIRATORY", "TRACT INFECTION"))
  expect_true(all(listing_disp_width(lines) <= 16))
  # the word step is what a listing cell uses too
  b <- build_listing(data.frame(A = "aa bb cc dd", stringsAsFactors = FALSE),
                     listing_spec(list(listing_col("A", width = 6)), spacer = FALSE, record = FALSE,
                                  blank_row = FALSE))
  expect_identical(b$A, c("aa bb", "cc dd"))
})

test_that("FT-LS5814 [FR-LS185]: the emitted wrap source begins with a header naming the version, the entry function and the contract, and keeps the rule's comments", {
  src <- as.character(unclass(listing_wrap_code("house_rule")))
  # the header: eleven comment lines and a blank line
  head11 <- src[1:12]
  expect_identical(head11[12L], "")
  expect_true(all(startsWith(head11[1:11], "#")))
  expect_identical(head11[1L], sprintf("# The \"multiline\" wrapping rule from rtfreporter %s, to edit.",
                                       utils::packageVersion("rtfreporter")))
  expect_identical(head11[3L], "#   listing_spec(cols, wrap = house_rule)")
  # the contract: positional (text, width, sep, layout), length-1 text, NULL width, resolved layout, lines back
  expect_match(head11[5L], "called positionally with", fixed = TRUE)
  expect_match(head11[6L], "(text, width, sep, layout); `text` is length 1; `width` may be NULL;", fixed = TRUE)
  expect_match(head11[7L], "`layout` is already \"stack\" or \"flow\"; return a non-empty character", fixed = TRUE)
  expect_match(head11[8L], "vector, one element per line.", fixed = TRUE)
  # another name is the one named
  expect_identical(listing_wrap_code("other_rule")[3L], "#   listing_spec(cols, wrap = other_rule)")
  # the shipped rule's comments are kept: one comment line for each the template carries after its own header
  tpl <- system.file("templates", "listing_wrap.R", package = "rtfreporter")
  skip_if(!nzchar(tpl), "the template is not installed")
  tsrc <- readLines(tpl, warn = FALSE)
  tsrc <- tsrc[cumsum(!grepl("^#", tsrc) & nzchar(tsrc)) > 0L]
  expect_identical(sum(grepl("^[[:space:]]*#", src[-seq_len(12L)])), sum(grepl("^[[:space:]]*#", tsrc)))
  expect_gt(sum(grepl("^[[:space:]]*#", tsrc)), 0L)
  expect_true(any(grepl("# A token wider than the column on its own.", src, fixed = TRUE)))
})

test_that("FT-LS5815 [FR-LS189]: listing_code() returns one element per line, of class rtf_listing_code, printing the code, and parsing it gives back the spec", {
  d <- data.frame(USUBJID = c("S-001", "S-002"), AGE = c("30", "41"), AETERM = c("Headache", "Nausea and vomiting"),
                  stringsAsFactors = FALSE)
  sp <- listing_spec(list(listing_col(c("USUBJID", "AGE")), listing_col("AETERM")))
  fitted <- fit_listing_widths(d, sp, total_width = 50,
                               labels = c(USUBJID = "Unique Subject ID", AGE = "Age", AETERM = "Preferred Term"))
  code <- listing_code(fitted)
  expect_s3_class(code, "rtf_listing_code")
  expect_type(unclass(code), "character")
  # one element per line: none holds a newline except inside a quoted multi-line label (written as \n)
  expect_false(any(grepl("\n", code, fixed = TRUE)))
  expect_identical(code[1L], "listing_spec(list(")
  expect_identical(code[length(code)], "))")
  # printing writes the code out, ready to copy
  expect_identical(utils::capture.output(print(code)), as.character(unclass(code)))
  # parsing it gives back the spec: the same widths and headers
  again <- eval(parse(text = code))
  expect_s3_class(again, "rtf_listing_spec")
  expect_identical(vapply(again$cols, function(cl) cl$width, 1L), vapply(fitted$cols, function(cl) cl$width, 1L))
  expect_identical(vapply(again$cols, function(cl) cl$label, ""), vapply(fitted$cols, function(cl) cl$label, ""))
  expect_identical(vapply(again$cols, function(cl) cl$name, ""), vapply(fitted$cols, function(cl) cl$name, ""))
  expect_identical(as_rtftables(d, listing = again), as_rtftables(d, listing = fitted))
  # the unfitted spec round-trips too
  back <- eval(parse(text = listing_code(sp)))
  expect_identical(lapply(back$cols, unclass), lapply(sp$cols, unclass))
})

test_that("FT-LS5816 [FR-LS191]: listing_code() writes each column's own settings and a multi-line label that survives the round trip", {
  sp <- listing_spec(list(
    listing_col(c("A", "B"), width = 12, sep = " | ", layout = "flow", name = "X", rel_width = 7,
                align = "center", collapse_repeats = TRUE, label = "L1\nL2"),
    listing_col("C")))
  code <- as.character(unclass(listing_code(sp)))
  expect_identical(code, c(
    "listing_spec(list(",
    "  listing_col(c(\"A\", \"B\"), width = 12, sep = \" | \", layout = \"flow\", name = \"X\", rel_width = 7, align = \"center\", collapse_repeats = TRUE,",
    "    label = \"L1\\nL2\"),",
    "  listing_col(\"C\")",
    "))"))
  back <- eval(parse(text = code))
  b1 <- back$cols[[1L]]
  expect_identical(b1$vars, c("A", "B"))
  expect_identical(b1$width, 12L)
  expect_identical(b1$sep, " | ")
  expect_identical(b1$layout, "flow")
  expect_identical(b1$name, "X")
  expect_identical(b1$rel_width, 7)
  expect_identical(b1$align, "center")
  expect_true(b1$collapse_repeats)
  expect_identical(b1$label, "L1\nL2")
  # a column with nothing set writes its vars alone: no name (it is the first var), no collapse_repeats = FALSE
  expect_identical(code[4L], "  listing_col(\"C\")")
})

test_that("FT-LS5817 [FR-LS247]: catx() with sep missing, not a character, of length other than one or NA stops with the message", {
  msg <- "`sep` must be a single string."
  expect_error(catx(), msg, fixed = TRUE)
  expect_error(catx(1, "a"), msg, fixed = TRUE)
  expect_error(catx(TRUE, "a"), msg, fixed = TRUE)
  expect_error(catx(c("/", "-"), "a"), msg, fixed = TRUE)
  expect_error(catx(character(0), "a"), msg, fixed = TRUE)
  expect_error(catx(NA, "a"), msg, fixed = TRUE)
  expect_error(catx(NA_character_, "a"), msg, fixed = TRUE)
  # a single string is accepted, "" included
  expect_identical(catx("-", "a", "b"), "a-b")
  expect_identical(catx("", "a", "b"), "ab")
})

test_that("FT-LS5818 [FR-LS251]: a fractional or infinite width, an infinite rel_width or spacer_rel_width stops with the messages of FR-LS160, FR-LS161 and FR-LS138, without a warning", {
  warned <- character(0)
  msg_of <- function(expr) {
    withCallingHandlers(tryCatch({ expr; NA_character_ }, error = function(e) conditionMessage(e)),
                        warning = function(w) { warned <<- c(warned, conditionMessage(w)); invokeRestart("muffleWarning") })
  }
  wd <- "`width` must be a single positive number of characters, or NULL."
  expect_identical(msg_of(listing_col("A", width = 1.5)), wd)
  expect_identical(msg_of(listing_col("A", width = Inf)), wd)
  expect_identical(msg_of(listing_col("A", rel_width = Inf)),
                   "`rel_width` must be a single positive number, or NULL.")
  expect_identical(msg_of(listing_spec(list(listing_col("A")), spacer_rel_width = Inf)),
                   "`spacer_rel_width` must be a single positive number, or NULL.")
  # no coercion warning on the way (Inf used to give NA with one)
  expect_length(warned, 0L)
  # whole widths and a finite fraction of rel_width are fine
  expect_identical(listing_col("A", width = 2)$width, 2L)
  expect_identical(listing_col("A", rel_width = 1.5)$rel_width, 1.5)
})

test_that("FT-LS5819 [FR-LS252]: total_width, min_width, font_size_half_points and font refuse what is not whole or not a name, with the messages; a named font is measured by its latin one", {
  d <- data.frame(A = c("aaaa", "bb"), B = c("c", "dddddd"), stringsAsFactors = FALSE)
  sp <- listing_spec(list(listing_col("A"), listing_col("B")))
  tw <- "`total_width` must be a single positive number of characters."
  for (bad in list(100.7, "100", TRUE)) {
    expect_error(fit_listing_widths(d, sp, total_width = bad), tw, fixed = TRUE, info = format(bad))
  }
  mw <- "`min_width` must be a single positive integer."
  for (bad in list(6.5, "6", TRUE)) {
    expect_error(fit_listing_widths(d, sp, total_width = 60, min_width = bad), mw, fixed = TRUE, info = format(bad))
  }
  fs <- "`font_size_half_points` must be a single positive integer (half-points)."
  for (bad in list(17.5, "18", TRUE, c(16, 18))) {
    expect_error(fit_listing_widths(d, sp, font_size_half_points = bad), fs, fixed = TRUE, info = format(bad)[1L])
  }
  expect_error(fit_listing_widths(d, sp, font_size_half_points = NULL), fs, fixed = TRUE)
  # 0, a negative, NA and Inf still stop as FR-LS232 says
  for (bad in list(0, -1, NA, Inf)) {
    expect_error(fit_listing_widths(d, sp, font_size_half_points = bad),
                 "Could not measure the width of one character", fixed = TRUE, info = format(bad))
  }
  # font: not one family name or a named vector per script slot
  fm <- "`font` must be a single font family name, or a named vector of them per script slot, "
  for (bad in list("", NA_character_, 12, c("Arial", "Courier New"))) {
    expect_error(fit_listing_widths(d, sp, font = bad), fm, fixed = TRUE, info = format(bad)[1L])
  }
  # a named vector is measured by its latin font, whatever the other slots say
  by_latin <- fit_listing_widths(d, sp, font = "Arial")
  expect_identical(fit_listing_widths(d, sp, font = c(latin = "Arial", east_asian = "SimSun")), by_latin)
  expect_identical(fit_listing_widths(d, sp, font = c(east_asian = "SimSun", latin = "Arial")), by_latin)
})

.t8_fit_df <- function() {
  data.frame(ID = c("S1", "S2", "S3"), AGE = c("30", "41", "5"),
             TERM = c("Headache", "Nausea and vomiting", "Rash"), stringsAsFactors = FALSE)
}
.t8_fit_spec <- function(...) {
  listing_spec(list(listing_col("ID"), listing_col("AGE"), listing_col("TERM")), ...)
}
.t8_w <- function(spec) vapply(spec$cols, function(cl) cl$width, 1L)

test_that("FT-LS5820 [FR-LS205]: fit_listing_widths() returns the same spec with a width on every column and attaches the measurement, which print() shows", {
  d <- .t8_fit_df()
  sp <- .t8_fit_spec()
  f <- fit_listing_widths(d, sp, total_width = 60)
  expect_s3_class(f, "rtf_listing_spec")
  # the same spec: only the columns' widths (and what FR-LS211 writes) differ
  expect_identical(vapply(f$cols, function(cl) cl$name, ""), c("ID", "AGE", "TERM"))
  keep <- setdiff(names(sp), "cols")
  expect_identical(unclass(f)[keep], unclass(sp)[keep], ignore_function_env = TRUE, ignore_srcref = TRUE)
  # a width on every column, where the spec had none
  expect_true(all(vapply(sp$cols, function(cl) is.null(cl$width), TRUE)))
  expect_true(all(vapply(f$cols, function(cl) is.integer(cl$width) && cl$width > 0L, TRUE)))
  # the measurement: total width, gutter (2 gutters of 1), each column's demand, which were fixed
  fit <- attr(f, "rtf_listing_fit", exact = TRUE)
  expect_identical(fit$total_width, 60L)
  expect_identical(fit$gutter, 2)
  # demands: ID and AGE ask for min_width 6; TERM's cells are 8, 19 and 4 wide, the 0.9
  # quantile is 8 + 0.8 * (19 - 8) = 16.8 (type 7: h = 0.9 * (3 - 1) + 1 = 2.8)
  expect_identical(fit$demand, c(ID = 6, AGE = 6, TERM = 16.8))
  expect_identical(fit$fixed, c(FALSE, FALSE, FALSE))
  # scaled to the budget 60 - 2 = 58: 58 / 28.8 = 2.0139 -> 12.08, 12.08, 33.8 -> 12, 12, 34
  expect_identical(.t8_w(f), c(12L, 12L, 34L))
  # print() shows it
  out <- utils::capture.output(print(f))
  expect_true("  fitted  : 58 + 2 gutter of 60 characters" %in% out)
  expect_true("    ID    width =  12 fit (demand 6.0)" %in% out)
  expect_true("    TERM  width =  34 fit (demand 16.8)" %in% out)
})

test_that("FT-LS5821 [FR-LS207]: the gutters come out of the budget first, spacer_rel_width characters each (a fraction allowed), none when spacer = FALSE", {
  d <- .t8_fit_df()
  cols <- listing_spec(list(listing_col("ID"), listing_col("AGE"), listing_col("TERM")))$cols
  fit_w <- function(...) {
    f <- fit_listing_widths(d, listing_spec(cols, ...), total_width = 60)
    list(w = .t8_w(f), gutter = attr(f, "rtf_listing_fit", exact = TRUE)$gutter)
  }
  # three columns, two gutters: widths + gutters = 60
  a <- fit_w()                                     # 2 * 1
  expect_identical(a$gutter, 2)
  expect_identical(sum(a$w), 58L)
  b <- fit_w(spacer_rel_width = 0.5)               # 2 * 0.5 = 1: a fraction
  expect_identical(b$gutter, 1)
  expect_identical(sum(b$w), 59L)
  cc <- fit_w(spacer_rel_width = 2)                # 2 * 2 = 4
  expect_identical(cc$gutter, 4)
  expect_identical(sum(cc$w), 56L)
  # no gutter columns: the whole budget
  n <- fit_w(spacer = FALSE)
  expect_identical(n$gutter, 0)
  expect_identical(sum(n$w), 60L)
  # one column has no gutter either way
  one <- fit_listing_widths(d, listing_spec(list(listing_col("TERM"))), total_width = 60)
  expect_identical(.t8_w(one), 60L)
})

test_that("FT-LS5822 [FR-LS208]: widths the author set are kept, taken out of the budget, and the others fit around them; all set comes back unchanged", {
  d <- .t8_fit_df()
  sp <- listing_spec(list(listing_col("ID", width = 10), listing_col("AGE"), listing_col("TERM")))
  f <- fit_listing_widths(d, sp, total_width = 60)
  # ID stays 10; the budget left is 60 - 2 gutters - 10 = 48 for AGE (demand 6) and TERM (16.8):
  # 48 / 22.8 = 2.105 -> 12.6 -> 13 and 35.4 -> 35
  expect_identical(.t8_w(f), c(10L, 13L, 35L))
  expect_identical(sum(.t8_w(f)) + 2L, 60L)
  expect_identical(attr(f, "rtf_listing_fit", exact = TRUE)$fixed, c(TRUE, FALSE, FALSE))
  # a width the author set is not raised, even when the column's demand is larger
  small <- fit_listing_widths(d, listing_spec(list(listing_col("ID"), listing_col("AGE"),
                                                   listing_col("TERM", width = 3))), total_width = 60)
  expect_identical(.t8_w(small)[[3L]], 3L)
  # all widths set: they come back unchanged, whatever the budget
  all_set <- listing_spec(list(listing_col("ID", width = 10), listing_col("AGE", width = 4),
                               listing_col("TERM", width = 30)))
  for (tw in c(60L, 20L)) {
    g <- fit_listing_widths(d, all_set, total_width = tw)
    expect_identical(.t8_w(g), c(10L, 4L, 30L), info = as.character(tw))
    expect_identical(attr(g, "rtf_listing_fit", exact = TRUE)$fixed, c(TRUE, TRUE, TRUE), info = as.character(tw))
  }
})

test_that("FT-LS5823 [FR-LS209]: a column's demand is the largest of the probs quantile of its cells, the header's height need, its widest unbreakable token and min_width, then scaled to the budget", {
  # Q: one cell of 20 characters among three of 1; H: a header of 19 characters that can break;
  # T: a header with one 12-character token; M: nothing but min_width
  d <- data.frame(Q = c("a", "a", "a", strrep("q", 20)), H = "x", T = "y", M = "z", stringsAsFactors = FALSE)
  labs <- c(Q = "Q", H = "aaaa bbbb cccc dddd", T = "abcdefghijkl mn", M = "M")
  sp <- listing_spec(list(listing_col("Q"), listing_col("H"), listing_col("T"), listing_col("M")),
                     spacer = FALSE)
  dem <- function(tw = 45, ...) {
    attr(fit_listing_widths(d, sp, total_width = tw, labels = labs, ...), "rtf_listing_fit",
         exact = TRUE)$demand
  }
  # probs = 1 is the longest cell; header_lines = 2; min_width = 3
  #   Q: max(20, token 1, ceiling(1 / 2) = 1, 3) = 20
  #   H: max(1, token 4, ceiling(19 / 2) = 10, 3) = 10
  #   T: max(1, token 12, ceiling(15 / 2) = 8, 3) = 12     (the token beats the height)
  #   M: max(1, 1, 1, 3) = 3
  expect_identical(dem(probs = 1, header_lines = 2, min_width = 3L), c(Q = 20, H = 10, T = 12, M = 3))
  # probs = 0 is the shortest cell: Q falls to min_width
  expect_identical(dem(probs = 0, header_lines = 2, min_width = 3L), c(Q = 3, H = 10, T = 12, M = 3))
  # header_lines = 4: H needs ceiling(19 / 4) = 5; header_lines = Inf: only its token, 4
  expect_identical(dem(probs = 0, header_lines = 4, min_width = 3L), c(Q = 3, H = 5, T = 12, M = 3))
  expect_identical(dem(probs = 0, header_lines = Inf, min_width = 3L), c(Q = 3, H = 4, T = 12, M = 3))
  # min_width raises every demand under it
  expect_identical(dem(probs = 0, header_lines = Inf, min_width = 8L), c(Q = 8, H = 8, T = 12, M = 8))
  # the demands are scaled to the budget: 45 is their sum (20 + 10 + 12 + 3), so they come out as they are;
  # twice the budget doubles them
  w45 <- fit_listing_widths(d, sp, total_width = 45, labels = labs, probs = 1, header_lines = 2, min_width = 3L)
  expect_identical(.t8_w(w45), c(20L, 10L, 12L, 3L))
  w90 <- fit_listing_widths(d, sp, total_width = 90, labels = labs, probs = 1, header_lines = 2, min_width = 3L)
  expect_identical(.t8_w(w90), c(40L, 20L, 24L, 6L))
})

test_that("FT-LS5824 [FR-LS211]: the fit writes rel_width and a frozen label only where the author set none, with only the label's structural breaks, and renders the same table", {
  d <- data.frame(A = c("aaa", "bbb"), B = c("cc", "dd"), C = c("e", "f"), stringsAsFactors = FALSE)
  attr(d$A, "label") <- "Alpha label words"
  attr(d$B, "label") <- "Beta\nline2"
  sp <- listing_spec(list(listing_col(c("A", "B"), name = "AB"), listing_col("C", rel_width = 40, label = "MINE")))
  f <- fit_listing_widths(d, sp, total_width = 50)
  w <- .t8_w(f)
  # AB: the author set neither: rel_width is the fitted width, the label is frozen as the
  # header prints ("Alpha label words" is not cut to the width): a variable label with a
  # break of its own ("Beta\nline2") is stacked under the other, as the derived header
  # stacks it (#997), not joined on one line
  expect_identical(f$cols[[1L]]$rel_width, as.numeric(w[[1L]]))
  expect_identical(f$cols[[1L]]$label, "Alpha label words/\nBeta\nline2")
  # C: what the author set stays
  expect_identical(f$cols[[2L]]$rel_width, 40)
  expect_identical(f$cols[[2L]]$label, "MINE")
  # the table is the one the same widths written by hand give (labels without a break of their own)
  attr(d$B, "label") <- "Beta"
  f2 <- fit_listing_widths(d, sp, total_width = 50)
  expect_identical(f2$cols[[1L]]$label, "Alpha label words/Beta")
  w2 <- .t8_w(f2)
  by_hand <- listing_spec(list(listing_col(c("A", "B"), name = "AB", width = w2[[1L]]),
                               listing_col("C", width = w2[[2L]], rel_width = 40, label = "MINE")))
  expect_identical(as_rtftables(d, listing = f2), as_rtftables(d, listing = by_hand))
})

test_that("FT-LS5825 [FR-LS212]: page takes the paper size, orientation and margins the listing is rendered on (an rtf_page() or a list of its fields); NULL is the defaults; total_width wins", {
  d <- .t8_fit_df()
  sp <- .t8_fit_spec(spacer = FALSE)
  total <- function(...) sum(.t8_w(fit_listing_widths(d, sp, ...)))
  land <- total(page = rtf_page(orientation = "landscape", paper_size = "A4"))
  port <- total(page = rtf_page(orientation = "portrait", paper_size = "A4"))
  expect_gt(land, port)                                           # orientation
  # paper size: A4 landscape (11.69 in) is wider than letter landscape (11 in)
  letter <- total(page = rtf_page(orientation = "landscape", paper_size = "letter"))
  expect_gt(land, letter)
  # margins: 1 inch more on each side takes 2 inches (a sixth) off the line
  margins <- total(page = rtf_page(orientation = "landscape", paper_size = "A4",
                                   margin_left_in = 2, margin_right_in = 2))
  expect_lt(margins, land)
  # a list of the fields is the same as the object
  expect_identical(total(page = list(orientation = "portrait", paper_size = "A4")), port)
  # NULL (the default) is the package's page defaults
  expect_identical(total(page = NULL), total())
  expect_identical(total(page = rtf_page()), total())
  # ignored when total_width is given
  expect_identical(fit_listing_widths(d, sp, total_width = 50, page = rtf_page(orientation = "portrait")),
                   fit_listing_widths(d, sp, total_width = 50))
  expect_identical(total(total_width = 50, page = rtf_page(margin_left_in = 4)), 50L)
})

test_that("FT-LS5826 [FR-LS217]: labels gives the words for variables without label attributes; the header is derived from them, joined with sep and wrapped to the width fitted, and the width measures them", {
  d <- data.frame(USUBJID = c("S-001", "S-002"), AGE = c("30", "41"), stringsAsFactors = FALSE)
  sp <- listing_spec(list(listing_col(c("USUBJID", "AGE"), name = "UA"), listing_col("AGE", name = "A2")), spacer = FALSE)
  labs <- c(USUBJID = "Unique Subject Identifier", AGE = "Age")
  f <- fit_listing_widths(d, sp, total_width = 60, labels = labs)
  # joined with the separator, in the order of vars
  expect_identical(f$cols[[1L]]$label, "Unique Subject Identifier/Age")
  expect_identical(f$cols[[2L]]$label, "Age")
  # wrapped to the width just fitted, as the cells are: no line is wider than the column
  b <- build_listing(d, f)
  hdr <- strsplit(attr(b, "rtf_listing", exact = TRUE)$cols[[1L]]$label, "\n", fixed = TRUE)[[1L]]
  expect_identical(hdr[length(hdr)], "Age")
  expect_true(all(listing_disp_width(hdr) <= f$cols[[1L]]$width))
  # the width fit measures the words: "Identifier" (10) cannot break, so the column is no narrower;
  # without the words the header is the names, whose longest token USUBJID is 7 wide
  with_labs <- attr(f, "rtf_listing_fit", exact = TRUE)$demand
  no_labs <- attr(fit_listing_widths(d, sp, total_width = 60), "rtf_listing_fit", exact = TRUE)$demand
  expect_gte(unname(with_labs[[1L]]), 10)
  expect_lt(unname(no_labs[[1L]]), unname(with_labs[[1L]]))
  # data that carries a label attribute needs no labels =; a name not in the data is not used
  attr(d$AGE, "label") <- "Age at baseline"
  g <- fit_listing_widths(d, sp, total_width = 60, labels = c(USUBJID = "Subject"))
  expect_identical(g$cols[[1L]]$label, "Subject/Age at baseline")
})

test_that("FT-LS5827 [FR-LS218]: header_lines is how tall a header may be (default 4): a lower value buys width, Inf asks only for the unbreakable token, and a width the author set stays", {
  expect_identical(formals(fit_listing_widths)$header_lines, 4L)
  d <- data.frame(H = "x", F = "y", stringsAsFactors = FALSE)
  labs <- c(H = "aaaa bbbb cccc dddd", F = "f")            # H's header is 19 wide, token 4
  sp <- listing_spec(list(listing_col("H"), listing_col("F")), spacer = FALSE)
  dem <- function(...) {
    unname(attr(fit_listing_widths(d, sp, total_width = 60, labels = labs, min_width = 1L, ...),
                "rtf_listing_fit", exact = TRUE)$demand[[1L]])
  }
  # H's demand is ceiling(19 / header_lines) when that beats its token (4)
  expect_identical(dem(header_lines = 1), 19)
  expect_identical(dem(header_lines = 2), 10)
  expect_identical(dem(header_lines = 4), 5)
  expect_identical(dem(), 5)                                # the default is 4
  expect_identical(dem(header_lines = 10), 4)               # 2 lines of 10 would do; the token floors it at 4
  expect_identical(dem(header_lines = Inf), 4)              # nothing beyond the token
  # a lower value buys a wider column
  w <- function(h) .t8_w(fit_listing_widths(d, sp, total_width = 60, labels = labs, min_width = 1L,
                                            header_lines = h))[[1L]]
  expect_gt(w(1), w(4))
  # it never overrides a width the author set
  fixed <- listing_spec(list(listing_col("H", width = 8), listing_col("F")), spacer = FALSE)
  expect_identical(.t8_w(fit_listing_widths(d, fixed, total_width = 60, labels = labs, header_lines = 1))[[1L]], 8L)
})

test_that("FT-LS5828 [FR-LS236]: collapse_repeats() on a list of pages collapses each page independently and gives what as_rtftables(collapse_repeats = ) gives", {
  df <- data.frame(g = c("A", "A", "A", "A", "B"), v = 1:5, stringsAsFactors = FALSE)
  # split_rows are the positions cut before: pages of rows 1-2, 3-4 and 5
  pages <- as_rtftables(df, split = "rows", split_rows = c(3, 5))
  expect_identical(vapply(pages, function(p) nrow(p$data), 1L), c(2L, 2L, 1L))
  out <- collapse_repeats(pages, cols = "g")
  expect_length(out, 3L)
  # a run restarts at every page break: the first row of each page shows its value again
  expect_identical(as.character(out[[1L]]$data$g), c("A", NA))
  expect_identical(as.character(out[[2L]]$data$g), c("A", NA))
  expect_identical(as.character(out[[3L]]$data$g), "B")
  # a whole-frame collapse would have blanked the second page's first row
  whole <- collapse_repeats(rtftable(df), cols = "g")
  expect_identical(as.character(whole$data$g), c("A", NA, NA, NA, "B"))
  # the same as the per-page argument of as_rtftables()
  via_arg <- as_rtftables(df, split = "rows", split_rows = c(3, 5), collapse_repeats = "g")
  expect_identical(lapply(out, function(p) as.character(p$data$g)),
                   lapply(via_arg, function(p) as.character(p$data$g)))
  expect_identical(lapply(out, function(p) p$data$v), lapply(via_arg, function(p) p$data$v))
  # the pages are still rtftables, in the same number
  expect_true(all(vapply(out, inherits, TRUE, what = "rtftable")))
})

# ------------------------------------------------------------------- AS

.t8_dir <- function(env = parent.frame()) {
  d <- tempfile("t8")
  dir.create(d)
  do.call(on.exit, list(substitute(unlink(d, recursive = TRUE)), add = TRUE), envir = env)
  d
}

test_that("FT-AS5801 [FR-AS252]: rtfplot() of a plot object refuses a render_format that is not png or emf, emf without devEMF, emf with render_dpi / png_device, and a file with render_format", {
  draw <- function() plot(1)
  fmt <- "`render_format` must be \"png\" or \"emf\"."
  for (bad in list("svg", c("png", "emf"), NA_character_, "PNG", 1)) {
    expect_error(rtfplot(draw, render_format = bad), fmt, fixed = TRUE, info = format(bad)[1L])
  }
  # no devEMF (mocked: it may well be installed): the message says how to get it
  local_mocked_bindings(.rtfplot_has_devemf = function() FALSE)
  expect_error(rtfplot(draw, render_format = "emf"),
               "`render_format = \"emf\"` needs the devEMF package: install.packages(\"devEMF\"), or draw as \"png\".",
               fixed = TRUE)
  # with devEMF (mocked present), a dpi or a PNG device are refused before anything is drawn
  local_mocked_bindings(.rtfplot_has_devemf = function() TRUE)
  dp <- "`render_dpi` and `png_device` are for `render_format = \"png\"` only; an EMF is a vector picture with no resolution."
  expect_error(rtfplot(draw, render_format = "emf", render_dpi = 300), dp, fixed = TRUE)
  expect_error(rtfplot(draw, render_format = "emf", png_device = "png"), dp, fixed = TRUE)
  expect_error(rtfplot(draw, render_format = "emf", render_dpi = 300, png_device = "png"), dp, fixed = TRUE)
  # a file path with render_format given stops as FR-AS209 says
  f <- test_path("fixtures", "figure.emf")
  fm <- "`render_width`, `render_height`, `render_dpi`, `png_device` and `render_format` say how to draw a plot object; a file already has a size and a resolution."
  expect_error(rtfplot(f, render_format = "emf"), fm, fixed = TRUE)
  expect_error(rtfplot(f, render_format = "png"), fm, fixed = TRUE)
  # their defaults, not given, are silent: png and emf with only width and height draw
  skip_if_not_installed("devEMF")
  local_mocked_bindings(.rtfplot_has_devemf = function() TRUE)
  expect_no_error(rtfplot(draw, render_format = "emf", render_width = 4, render_height = 3))
  expect_no_warning(rtfplot(draw, render_format = "emf"))
})

test_that("FT-AS5802 [FR-AS220 FR-AS221]: shell = is the shell as text or a UTF-8 file (BOM, any line end, trailing blanks dropped), written to 07_shell.md and last in PASTE.md; text that is not text or a file in another encoding stops", {
  d <- .t8_dir()
  sh <- c("Table 14.1.1  Demographics", "    Placebo   Drug X", "性別  n (%)")
  # NULL (the default) writes none
  suppressMessages(rtfreporter_ai_manual(d, overwrite = TRUE))
  expect_false(file.exists(file.path(d, "07_shell.md")))
  # text: written to 07_shell.md and put last in PASTE.md
  suppressMessages(rtfreporter_ai_manual(d, shell = sh, overwrite = TRUE))
  expect_identical(readLines(file.path(d, "07_shell.md"), encoding = "UTF-8"), sh)
  heads <- sub("^##### ", "", grep("^##### ", readLines(file.path(d, "PASTE.md"), encoding = "UTF-8"), value = TRUE))
  expect_identical(heads[length(heads)], "07_shell.md")
  # a file: CRLF, a BOM, trailing tabs and spaces dropped
  bytes <- charToRaw(enc2utf8("Table 1\t\t\r\n  Mean (SD)\txx.x   \r\n性別\t\r\n"))
  f <- file.path(d, "shell.md")
  for (b in list(bytes, c(as.raw(c(0xef, 0xbb, 0xbf)), bytes))) {
    writeBin(b, f)
    suppressMessages(rtfreporter_ai_manual(d, shell = f, overwrite = TRUE))
    expect_identical(readLines(file.path(d, "07_shell.md"), encoding = "UTF-8"),
                     c("Table 1", "  Mean (SD)\txx.x", "性別"))
  }
  # a file that is not text: the guard's message
  msg <- "`rtfreporter_ai_manual(shell = )` is the shell's text, or a .txt / .md file."
  for (bad in list(1, NA_character_, character(0), TRUE)) {
    expect_error(rtfreporter_ai_manual(d, shell = bad, overwrite = TRUE), msg, fixed = TRUE, info = format(bad)[1L])
  }
  # UTF-16: its name in the message, and how to save it as UTF-8
  txt <- "Table 14.1.1  Demographics (背景因子)\r\n"
  utf16 <- c(as.raw(c(0xff, 0xfe)), iconv(enc2utf8(txt), "UTF-8", "UTF-16LE", toRaw = TRUE)[[1L]])
  writeBin(utf16, f)
  expect_error(rtfreporter_ai_manual(d, shell = f, overwrite = TRUE),
               "`rtfreporter_ai_manual(shell = )`: 'shell.md' is UTF-16 (\"Unicode\"), not UTF-8.\n  Open it in Notepad, File > Save As, Encoding: UTF-8, and pass that file (or the text itself).",
               fixed = TRUE)
  # another encoding (Shift-JIS)
  sjis <- iconv(enc2utf8(txt), "UTF-8", "CP932", toRaw = TRUE)[[1L]]
  skip_if(is.null(sjis), "no CP932 converter")
  writeBin(sjis, f)
  expect_error(rtfreporter_ai_manual(d, shell = f, overwrite = TRUE),
               "`rtfreporter_ai_manual(shell = )`: 'shell.md' is not UTF-8 (saved as Shift-JIS / ANSI?).\n  Open it in Notepad, File > Save As, Encoding: UTF-8, and pass that file (or the text itself).",
               fixed = TRUE)
})

test_that("FT-AS5803 [FR-AS253]: rtf_figures() hands render_format, render_width, render_height, render_dpi and png_device to each plot object as rtfplot() takes them, never to a file, and an rtfplot() keeps its format", {
  skip_if_not_installed("devEMF")
  skip_if_not(isTRUE(unname(capabilities("png"))), "no PNG device")
  d <- .t8_dir()
  fn <- function() plot(1)
  blk <- function(doc) lapply(doc$contents, function(b) b$content)
  same <- function(a, b) {
    all(vapply(c("img_type", "img_width", "img_height"), function(k) identical(a[[k]], b[[k]]), TRUE))
  }
  # NULL (the default) is rtfplot()'s own "png"
  expect_identical(blk(rtf_figures(rtf_document(), list(fn, fn)))[[1L]]$img_type, "png")
  # every argument reaches each plot object: the same figure as rtfplot() with them
  args <- list(render_width = 4, render_height = 3, render_dpi = 100, png_device = "png")
  b <- blk(rtf_figures(rtf_document(), list(fn, fn), render_width = 4, render_height = 3,
                       render_dpi = 100, png_device = "png"))
  ref <- do.call(rtfplot, c(list(fn), args))
  expect_true(same(b[[1L]], ref))
  expect_true(same(b[[2L]], ref))
  # 4 x 3 inches at 100 dpi
  expect_identical(c(ref$img_width, ref$img_height), c(400L, 300L))
  # a different size is a different figure: the arguments are not dropped
  other <- blk(rtf_figures(rtf_document(), fn, render_width = 6, render_height = 2, render_dpi = 100))
  expect_identical(c(other[[1L]]$img_width, other[[1L]]$img_height), c(600L, 200L))
  # render_format = "emf" reaches the plot objects, and so does the refusal of render_dpi with it
  em <- blk(rtf_figures(rtf_document(), list(fn, fn), render_format = "emf"))
  expect_identical(vapply(em, function(x) x$img_type, ""), c("emf", "emf"))
  expect_error(rtf_figures(rtf_document(), fn, render_format = "emf", render_dpi = 300),
               "are for `render_format = \"png\"` only", fixed = TRUE)
  # a file path is never handed them (a file with render_format would stop); an rtfplot() keeps its format
  png <- file.path(d, "p.png")
  grDevices::png(png, width = 600, height = 400)
  plot(1)
  grDevices::dev.off()
  emf <- test_path("fixtures", "figure.emf")
  made_png <- rtfplot(fn)
  made_emf <- rtfplot(fn, render_format = "emf")
  fs <- blk(rtf_figures(rtf_document(), list(png, emf, made_png, made_emf), render_format = "emf",
                        render_width = 5))
  expect_identical(vapply(fs, function(x) x$img_type, ""), c("png", "emf", "png", "emf"))
  fs2 <- blk(rtf_figures(rtf_document(), list(png, emf, made_png, made_emf), render_format = "png"))
  expect_identical(vapply(fs2, function(x) x$img_type, ""), c("png", "emf", "png", "emf"))
})

test_that("FT-AS5804 [FR-AS258]: a table with no label column, or a row whose label is NA, takes the label from its file's title as toc = auto does; a label given wins; the outline entry is the same", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "Table 14.1.1 Demographics", t_14_2_1 = "Table 14.2.1 Adverse Events"))
  f <- c(f, .tt_input(d, "untitled_listing.rtf", title = NULL))
  auto <- .tt_asm(f, toc = "auto")
  # no label column: every label from the titles, a file with no title by its name without ".rtf"
  none <- .tt_asm(f, toc = data.frame(file = f))
  expect_identical(.tt_labels(none), c("Table 14.1.1 Demographics", "Table 14.2.1 Adverse Events", "untitled_listing"))
  expect_identical(.tt_labels(none), .tt_labels(auto))
  # an NA label: that row only is read from its title; the labels given win
  some <- .tt_asm(f, toc = data.frame(file = f, label = c(NA, "Safety AEs", NA)))
  expect_identical(.tt_labels(some), c("Table 14.1.1 Demographics", "Safety AEs", "untitled_listing"))
  # a label given for every row wins over the titles
  all_given <- .tt_asm(f, toc = data.frame(file = f, label = c("a", "b", "c")))
  expect_identical(.tt_labels(all_given), c("a", "b", "c"))
  # the outline entry (the PDF bookmark) is the same label
  expect_true(any(grepl("outlinelevel0 Table 14\\.1\\.1 Demographics\\\\par", some)))
  expect_true(any(grepl("outlinelevel0 Safety AEs\\\\par", some)))
  expect_true(any(grepl("outlinelevel0 untitled_listing\\\\par", some)))
  expect_true(any(grepl("outlinelevel0 Table 14\\.1\\.1 Demographics\\\\par", none)))
  expect_false(any(grepl("outlinelevel0 Table 14\\.1\\.1", all_given)))
})

test_that("FT-AS5805 [FR-AS267]: a heading is printed once, above the entry of the row where its value changes; NA or empty prints none, a value that comes back prints again; no heading column, no heading", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B", t_14_3_1 = "C", t_14_4_1 = "D", t_14_5_1 = "E", t_14_6_1 = "F"))
  # X X (NA) "" Y X: X once for rows 1-2 (the NA and the empty heading print none and do not
  # end its run? they print nothing), Y above row 5, X again above row 6
  x <- .tt_asm(f, toc = data.frame(file = f, label = LETTERS[1:6],
                                   heading = c("X", "X", NA, "", "Y", "X")))
  expect_identical(.tt_headings(x), c("X", "Y", "X"))
  p <- .tt_paras(x)
  # the title, the blank line, then: X, a, b, c, d, Y, e, X, f
  expect_match(p[3L], "X\\par}", fixed = TRUE)
  expect_match(p[4L], "fldrslt A}", fixed = TRUE)
  expect_match(p[5L], "fldrslt B}", fixed = TRUE)
  expect_match(p[6L], "fldrslt C}", fixed = TRUE)
  expect_match(p[7L], "fldrslt D}", fixed = TRUE)
  expect_match(p[8L], "Y\\par}", fixed = TRUE)
  expect_match(p[9L], "fldrslt E}", fixed = TRUE)
  expect_match(p[10L], "X\\par}", fixed = TRUE)
  expect_match(p[11L], "fldrslt F}", fixed = TRUE)
  expect_length(p, 11L)
  # the same heading on every row: once
  expect_identical(.tt_headings(.tt_asm(f, toc = data.frame(file = f, label = LETTERS[1:6], heading = "Same"))), "Same")
  # NA and empty only: none; no heading column: none
  expect_length(.tt_headings(.tt_asm(f, toc = data.frame(file = f, label = LETTERS[1:6],
                                                         heading = c(NA, "", NA, "", NA, "")))), 0L)
  expect_length(.tt_headings(.tt_asm(f, toc = data.frame(file = f, label = LETTERS[1:6]))), 0L)
})

test_that("FT-AS5806 [FR-AS255]: toc_title fills the session's tokens (braces of a value escaped), {DATETIME}, {PROGRAM} and the page tokens; an unknown token stays as text; a document's own token is not known", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- c(.tt_input(d, "t_14_1_1.rtf", "Table 14.1.1 {STUDY} Demographics", tokens = list(STUDY = "DOC-7")),
         .tt_input(d, "t_14_2_1.rtf", "Table 14.2.1 B"))
  old <- options(rtfreporter.tokens = list(STUDY_ID = "XYZ-101", ODD = "a{b}c"),
                 rtfreporter.render_time = as.POSIXct("2026-11-12 09:30:00", tz = "UTC"),
                 # R CMD check runs the tests with no program to detect: name one
                 rtfreporter.program = "work/tfl/t_toc.R")
  on.exit(options(old), add = TRUE)
  title <- function(t) .tt_paras(.tt_asm(f, toc = "auto", toc_title = t))[1L]
  # the session's tokens, a value written as it is (its braces escaped)
  expect_identical(title("Study {STUDY_ID}"), "{\\pard\\qc\\b\\fs28 Study XYZ-101\\par}")
  expect_identical(title("{ODD}"), "{\\pard\\qc\\b\\fs28 a\\{b\\}c\\par}")
  # {DATETIME}, {PROGRAM} (whatever program the session runs: it is filled, not left as text)
  # and a viewer page token
  prog <- suppressMessages(title("On {DATETIME:%Y-%m-%d} by {PROGRAM}"))
  expect_match(prog, "^\\{\\\\pard\\\\qc\\\\b\\\\fs28 On 2026-11-12 by [^{}]+\\\\par\\}$")
  expect_false(grepl("PROGRAM", prog, fixed = TRUE))
  # the session's options(rtfreporter.program) is the one named
  expect_match(prog, "t_toc.R", fixed = TRUE)
  expect_match(title("Page {AUTO_PAGE}"), "Page \\\\chpgn \\\\par\\}$")
  # a token that is none of them stays as text, without a warning or an error
  x <- expect_no_warning(.tt_asm(f, toc = "auto", toc_title = "Contents {NO_SUCH_TOKEN}"))
  expect_identical(.tt_paras(x)[1L], "{\\pard\\qc\\b\\fs28 Contents \\{NO_SUCH_TOKEN\\}\\par}")
  # the document's own token (rtf_document(tokens = )) is not known at assembly: it stays as text in
  # toc_title, but is filled in the title of an input file read by toc = "auto"
  expect_identical(title("Contents {STUDY}"), "{\\pard\\qc\\b\\fs28 Contents \\{STUDY\\}\\par}")
  expect_identical(.tt_labels(.tt_asm(f, toc = "auto"))[1L], "Table 14.1.1 DOC-7 Demographics")
})

test_that("FT-AS5807 [FR-AS256]: toc_title named l, c or r is aligned left, centre or right, unnamed centred; a title that is not one string or a name that is not l, c or r stops", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B"))
  title <- function(t) .tt_paras(.tt_asm(f, toc = "auto", toc_title = t))[1L]
  expect_identical(title(c(l = "Contents")), "{\\pard\\ql\\b\\fs28 Contents\\par}")
  expect_identical(title(c(c = "Contents")), "{\\pard\\qc\\b\\fs28 Contents\\par}")
  expect_identical(title(c(r = "Contents")), "{\\pard\\qr\\b\\fs28 Contents\\par}")
  expect_identical(title("Contents"), "{\\pard\\qc\\b\\fs28 Contents\\par}")
  out <- file.path(d, "assembled.rtf")
  msg <- "`toc_title` must be a single string."
  for (bad in list(c("A", "B"), c(l = "A", r = "B"), NA_character_, 1, character(0))) {
    expect_error(assemble_rtf(f, out, toc = "auto", toc_title = bad, overwrite = TRUE), msg,
                 fixed = TRUE, info = format(bad)[1L])
  }
  expect_error(assemble_rtf(f, out, toc = "auto", toc_title = c(x = "Contents"), overwrite = TRUE),
               "`assemble_rtf()` row 1: a cell may only be named l, c or r", fixed = TRUE)
})

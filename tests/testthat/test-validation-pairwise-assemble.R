# Validation tests: assembling a folder of reports -- assemble_folder() and
# assemble_rtf() with a cover, the shapes and levels of a table of contents,
# files with and without a table number, the number read from the header or
# from the rtf_titles() title block, files of several sections, an output
# file and a spec file -- a pairwise set and a decision table, every case
# checked on the files written.  Helpers prefixed .va_.  Fictional data only.
#
# ---------------------------------------------------------------------------
#  Design
# ---------------------------------------------------------------------------
#  The rules, as read from the help (assemble_folder(), assemble_rtf()) and
#  the code (R/assemble_spec.R .assemble_spec(), .write_spec(),
#  .assemble_from_spec(); R/assemble_rtf.R assemble_rtf(),
#  .build_toc_section()):
#
#    number    a file's table number and title come from its running header
#              when it has a "Table N" line, else from the centred bold lines
#              of its rtf_titles() block (#843, #845); a header line wins over
#              the title block; a file with neither (a listing) is labelled
#              by its file name.  The label is "Table N  <title>".
#    order     the numbered files by number in natural order (14.1.2 before
#              14.1.10), then the others by file name, whatever the names.
#    sections  a file of several sections is one row and one entry, read
#              from its first section; its pages counted; every section's
#              header kept in the book.
#    output    no output_file: the table is returned and nothing assembled;
#              output_file: list(output, spec), invisibly; spec_file: the
#              table written there as well (.csv), the same rows.
#    contents  toc = NULL: no contents, no bookmark, no outline paragraph;
#              "auto": a level-1 entry per file, the first centred bold title
#              cell, else the file's name; a character vector: level-1
#              entries in that order; a table: its rows (order column, when
#              given), level 1 / 2 / 3 indented 0 / 600 / 1200 twips, a bold
#              heading line whenever the heading changes; the path of such a
#              table (.csv) the same.  Each entry links to a bookmark at its
#              file's first section; the body is in the entries' order.
#    cover     a cover page (its title \fs44) before the contents; none
#              without cover.
#    numbering toc_page_numbering "none" no \pgnrestart, "roman" two,
#              "decimal" one (the numbers themselves: FT-AS126, FT-AS127).
#
#  (A) FT-AS138, pairwise, assemble_folder().  Three numbered files (Table
#  14.2.1, 14.1.10, 14.1.2) and maybe a listing.  Factors and levels:
#    src       where the number is: header, titles, both (header 14.x,
#              titles "Table 9.9.k": the header wins)
#    listing   no, yes (a file with "Listing 16.2.1" titles, no table number)
#    sections  1, 2 (a second section with a header of its own)
#    names     agree (the file names in number order), reversed
#    cover     no, yes
#    numbering none, roman, decimal
#    output    out, out + spec_file, spec_file only, neither
#    toc_title default, "Contents of the Example Pharma tables"
#  1152 combinations; .allpairs() picks the rows covering every pair.
#  Oracle: the table (files in order, table, label, level 2, pages); with an
#  output, the entries' labels and bookmarks in that order, the bodies in
#  that order, every section header, the cover, the contents title, the
#  \pgnrestart count; the spec file's rows.  FR-AS148 FR-AS149 FR-AS150
#  FR-AS151 FR-AS152 FR-AS153 FR-AS157 FR-AS158 FR-AS242 FR-AS243.
#
#  (B) FT-AS139, decision table, assemble_rtf().  Inputs: t1 (titles "Table
#  14.1.1" / "Demographics"), t2 ("Table 14.2.1" in the header, no titles),
#  t3 (two sections, no titles).
#    toc                       entries (label, level)            body order
#    NULL                      none, no bookmark                 t1 t2 t3
#    "auto"                    Table 14.1.1 1, t2 1, t3 1        t1 t2 t3
#    c("L1", "L2", "L3")       L1 1, L2 1, L3 1                  t1 t2 t3
#    table, levels 1 2 3,      H1; L1 1, L2 2; H2; L3 3          t1 t2 t3
#      headings H1 H1 H2
#    its .csv path             as the table                      t1 t2 t3
#    table, order 3 1 2,       L2 2, L3 3, L1 1 (in that order)  t2 t3 t1
#      no input_files
#  x cover (no, yes) x toc_leader (dot, none: \tldot or not).  24 rows.
#  FR-AS101 FR-AS109 FR-AS111 FR-AS112 FR-AS113 FR-AS115 FR-AS116 FR-AS117
#  FR-AS125 FR-AS128 FR-AS135.
# ---------------------------------------------------------------------------

.VA_TABLES <- data.frame(id = c("A", "B", "C"), num = c("14.2.1", "14.1.10", "14.1.2"),
                         title = c("Adverse Events", "Vital Signs", "Demographics"),
                         stringsAsFactors = FALSE)
.va_read <- function(f) paste(readLines(f, warn = FALSE), collapse = "\n")
.va_tok <- function(x, rx) regmatches(x, gregexpr(rx, x, perl = TRUE))[[1L]]

# One report: `num` NA for a listing; `src` where its number is written.
.va_report <- function(f, id, num, title, src, sections) {
  tb <- rtftable(data.frame(Parameter = paste("body", id), Value = "x"))
  doc <- rtf_tables(rtf_document(), rep(list(tb), sections))
  hdr <- if (!is.na(num) && src %in% c("header", "both")) {
    rtf_header(list(c(c = paste("Table", num)), c(c = title)))
  } else {
    rtf_header(c(l = "Example Pharma STUDY-01"))
  }
  doc <- rtf_section(doc, page = 1, header = hdr)
  if (sections > 1L) doc <- rtf_section(doc, page = 2, header = rtf_header(c(l = paste("Second section", id))))
  if (is.na(num)) {
    doc <- rtf_titles(doc, c("Listing 16.2.1", title))
  } else if (src == "titles") {
    doc <- rtf_titles(doc, c(paste("Table", num), title))
  } else if (src == "both") {
    doc <- rtf_titles(doc, c(paste0("Table 9.9.", match(id, .VA_TABLES$id)), "Other title"))
  }
  generate_rtfreport(doc, f, overwrite = TRUE)
  f
}

# The folder of one case; returns the expected table (in order).
.va_folder <- function(d, cs) {
  sec <- as.integer(cs$sections)
  ord <- c("C", "B", "A")                     # the number order
  nm <- if (cs$names == "agree") ord else rev(ord)
  files <- stats::setNames(sprintf("t-%02d.rtf", seq_along(nm)), nm)
  for (id in ord) {
    k <- match(id, .VA_TABLES$id)
    .va_report(file.path(d, files[[id]]), id, .VA_TABLES$num[k], .VA_TABLES$title[k], cs$src, sec)
  }
  want <- data.frame(file = unname(files[ord]), id = ord,
                     table = .VA_TABLES$num[match(ord, .VA_TABLES$id)],
                     label = paste0("Table ", .VA_TABLES$num[match(ord, .VA_TABLES$id)], "  ",
                                    .VA_TABLES$title[match(ord, .VA_TABLES$id)]),
                     stringsAsFactors = FALSE)
  if (cs$listing == "yes") {
    .va_report(file.path(d, "l-01.rtf"), "L", NA, "Listing of Subjects", cs$src, sec)
    want <- rbind(want, data.frame(file = "l-01.rtf", id = "L", table = NA_character_,
                                   label = "l-01", stringsAsFactors = FALSE))
  }
  want
}

.va_entries <- function(x) {
  sub(".*fldrslt ", "", .va_tok(x, "HYPERLINK [^}]*\\}\\{\\\\fldrslt [^}]*"))
}

.va_check_folder <- function(cs, root) {
  d <- file.path(root, paste0("in-", cs$i))
  dir.create(d)
  want <- .va_folder(d, cs)
  sec <- as.integer(cs$sections)
  out <- if (cs$output %in% c("out", "both")) file.path(root, paste0("book-", cs$i, ".rtf"))
  sf <- if (cs$output %in% c("spec", "both")) file.path(root, paste0("spec-", cs$i, ".csv"))
  args <- list(d, output_file = out, spec_file = sf, toc_page_numbering = cs$numbering)
  if (cs$cover == "yes") args$cover <- list(title = "Example Pharma STUDY-01 Tables")
  if (cs$toc_title == "custom") args$toc_title <- "Contents of the Example Pharma tables"
  res <- do.call(assemble_folder, args)
  bad <- character(0)
  spec <- if (is.null(out)) res else res$spec
  if (!is.null(out) && !identical(names(res), c("output", "spec"))) bad <- c(bad, "return")
  if (is.null(out) && !is.data.frame(res)) bad <- c(bad, "return table")
  if (!identical(basename(spec$file), want$file)) bad <- c(bad, "order")
  if (!identical(as.character(spec$table), want$table)) bad <- c(bad, "table")
  if (!identical(spec$label, want$label)) bad <- c(bad, "label")
  if (!all(spec$level == 2L)) bad <- c(bad, "level")
  if (!all(spec$pages == sec)) bad <- c(bad, "pages")
  if (!is.null(sf)) {
    s2 <- utils::read.csv(sf, stringsAsFactors = FALSE)
    if (!identical(basename(s2$file), want$file) || !identical(s2$label, want$label)) bad <- c(bad, "spec file")
  }
  if (is.null(out)) {
    if (length(list.files(root, pattern = paste0("^book-", cs$i, "[.]")))) bad <- c(bad, "wrote a book")
    return(bad)
  }
  x <- .va_read(out)
  if (!identical(.va_entries(x), want$label)) bad <- c(bad, "entries")
  bk <- sub("\\\\bkmkstart ", "", .va_tok(x, "\\\\bkmkstart [^}]*"))
  # the bookmark: the prefix and the file name, "-" made "_" (FR-AS133)
  if (!identical(bk, paste0("tfl_", gsub("-", "_", sub("[.]rtf$", "", want$file))))) bad <- c(bad, "bookmarks")
  body <- sub("body ", "", .va_tok(x, "body [A-Z]"))
  if (!identical(body, rep(want$id, each = sec))) bad <- c(bad, "body order")
  if (sec > 1L && !identical(sub("Second section ", "", .va_tok(x, "Second section [A-Z]")), want$id)) {
    bad <- c(bad, "second headers")
  }
  if (grepl("\\fs44 Example Pharma STUDY-01 Tables", x, fixed = TRUE) != (cs$cover == "yes")) bad <- c(bad, "cover")
  tt <- if (cs$toc_title == "custom") "Contents of the Example Pharma tables" else "Table of Contents"
  if (!grepl(paste0("\\b\\fs28 ", tt, "\\par"), x, fixed = TRUE)) bad <- c(bad, "toc title")
  n_pgn <- c(none = 0L, roman = 2L, decimal = 1L)[[cs$numbering]]
  if (length(.va_tok(x, "\\\\pgnrestart")) != n_pgn) bad <- c(bad, "numbering")
  bad
}

test_that("FT-AS138 [FR-AS148 FR-AS149 FR-AS150 FR-AS151 FR-AS152 FR-AS153 FR-AS157 FR-AS158 FR-AS242 FR-AS243]: pairwise -- assemble_folder(): number source x listing x sections x names x cover x numbering x output x toc_title", {
  cand <- expand.grid(src = c("header", "titles", "both"), listing = c("no", "yes"),
                      sections = c("1", "2"), names = c("agree", "reversed"),
                      cover = c("no", "yes"), numbering = c("none", "roman", "decimal"),
                      output = c("out", "both", "spec", "none"),
                      toc_title = c("default", "custom"), stringsAsFactors = FALSE)
  expect_identical(nrow(cand), 1152L)
  ap <- .allpairs(cand)
  expect_setequal(ap$covered, ap$need)
  expect_lt(nrow(ap$rows), 20L)
  root <- tempfile("va-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  set <- ap$rows
  for (i in seq_len(nrow(set))) {
    cs <- c(as.list(set[i, ]), i = i)
    expect_identical(.va_check_folder(cs, root), character(0),
                     label = paste(unlist(cs), collapse = " / "))
  }
})

test_that("FT-AS139 [FR-AS101 FR-AS109 FR-AS111 FR-AS112 FR-AS113 FR-AS115 FR-AS116 FR-AS117 FR-AS125 FR-AS128 FR-AS135]: decision table -- assemble_rtf(): the contents' shape and levels x cover x leader", {
  d <- tempfile("va-rtf-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  tb <- function(id) rtftable(data.frame(Parameter = paste("body", id), Value = "x"))
  f1 <- file.path(d, "t1.rtf")
  f2 <- file.path(d, "t2.rtf")
  f3 <- file.path(d, "t3.rtf")
  generate_rtfreport(rtf_document() |> rtf_tables(list(tb("P"))) |>
                       rtf_titles(c("Table 14.1.1", "Demographics")), f1, overwrite = TRUE)
  generate_rtfreport(rtf_document() |> rtf_tables(list(tb("Q"))) |>
                       rtf_section(header = rtf_header(list(c(c = "Table 14.2.1"), c(c = "Adverse Events")))),
                     f2, overwrite = TRUE)
  generate_rtfreport(rtf_document() |> rtf_tables(list(tb("R"), tb("R"))) |>
                       rtf_section(page = 1, header = rtf_header("First band R")) |>
                       rtf_section(page = 2, header = rtf_header("Second band R")), f3, overwrite = TRUE)
  tbl <- data.frame(file = c(f1, f2, f3), label = c("L1", "L2", "L3"),
                    heading = c("H1", "H1", "H2"), level = c(1L, 2L, 3L), stringsAsFactors = FALSE)
  csv <- file.path(d, "toc.csv")
  utils::write.csv(tbl, csv, row.names = FALSE)
  reord <- tbl
  reord$order <- c(3L, 1L, 2L)
  kinds <- list(
    null   = list(toc = NULL, files = TRUE, labels = character(0), li = integer(0), heads = character(0), body = c("P", "Q", "R")),
    auto   = list(toc = "auto", files = TRUE, labels = c("Table 14.1.1", "t2", "t3"), li = c(0L, 0L, 0L), heads = character(0), body = c("P", "Q", "R")),
    labels = list(toc = c("L1", "L2", "L3"), files = TRUE, labels = c("L1", "L2", "L3"), li = c(0L, 0L, 0L), heads = character(0), body = c("P", "Q", "R")),
    table  = list(toc = tbl, files = FALSE, labels = c("L1", "L2", "L3"), li = c(0L, 600L, 1200L), heads = c("H1", "H2"), body = c("P", "Q", "R")),
    csv    = list(toc = csv, files = FALSE, labels = c("L1", "L2", "L3"), li = c(0L, 600L, 1200L), heads = c("H1", "H2"), body = c("P", "Q", "R")),
    order  = list(toc = reord, files = FALSE, labels = c("L2", "L3", "L1"), li = c(600L, 1200L, 0L), heads = c("H1", "H2", "H1"), body = c("Q", "R", "P")))
  dt <- expand.grid(kind = names(kinds), cover = c(FALSE, TRUE), leader = c("dot", "none"),
                    stringsAsFactors = FALSE)
  expect_identical(nrow(dt), 24L)
  for (i in seq_len(nrow(dt))) {
    k <- kinds[[dt$kind[i]]]
    lab <- paste(dt$kind[i], dt$cover[i], dt$leader[i], sep = " / ")
    out <- file.path(d, sprintf("book-%02d.rtf", i))
    args <- list(output_file = out, toc_leader = dt$leader[i])
    if (k$files) args$input_files <- c(f1, f2, f3)
    if (!is.null(k$toc)) args$toc <- k$toc
    if (dt$cover[i]) args$cover <- list(title = "Example Pharma STUDY-01 Tables", meta = "Draft")
    do.call(assemble_rtf, args)
    x <- .va_read(out)
    expect_identical(.va_entries(x), k$labels, label = lab)
    ent <- .va_tok(x, "\\{\\\\pard[^\n{]*\\{\\\\field\\{\\\\\\*\\\\fldinst HYPERLINK")
    li <- vapply(ent, function(e) {
      m <- regmatches(e, regexpr("\\\\li[0-9]+", e))
      if (length(m)) as.integer(sub("\\\\li", "", m)) else 0L
    }, 0L, USE.NAMES = FALSE)
    expect_identical(li, k$li, label = lab)
    heads <- sub(".*\\\\fs22 ", "", sub("\\\\par\\}$", "", .va_tok(x, "\\{\\\\pard\\\\b\\\\fs22 [^\\\\]*\\\\par\\}")))
    expect_identical(heads, k$heads, label = lab)
    expect_identical(sub("body ", "", unique(.va_tok(x, "body [PQR]"))), k$body, label = lab)
    toc <- length(k$labels) > 0L
    expect_identical(length(.va_tok(x, "\\\\bkmkstart ")), if (toc) 3L else 0L, label = lab)
    expect_identical(length(.va_tok(x, "\\\\outlinelevel")) > 0L, toc, label = lab)
    expect_identical(grepl("Table of Contents", x, fixed = TRUE), toc, label = lab)
    expect_identical(grepl("\\tldot", x, fixed = TRUE), toc && dt$leader[i] == "dot", label = lab)
    expect_identical(grepl("\\fs44 Example Pharma STUDY-01 Tables", x, fixed = TRUE), dt$cover[i], label = lab)
    # both bands of the file of two sections are kept
    expect_true(grepl("First band R", x, fixed = TRUE) && grepl("Second band R", x, fixed = TRUE), label = lab)
  }
})

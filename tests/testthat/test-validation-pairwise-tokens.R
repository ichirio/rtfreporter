# Validation tests: the {TOKEN}s of a header, footer, title, footnote and
# table cell, and the bands they live in -- a pairwise set over the token,
# where it is, its cell and row in the band, the document and the text
# around it; a pairwise set over the page tokens through assemble_rtf() /
# assemble_folder(); decision tables of where a token's value comes from,
# of what an unknown token does, and of the page tokens after assembly.
# Every case is checked on the RTF written.  Helpers prefixed .tk_.
# Fictional data only.
#
# ---------------------------------------------------------------------------
#  Design
# ---------------------------------------------------------------------------
#  The rules, as read from the help (rtf_text_tokens(), rtf_header(),
#  rtf_document(tokens = ), rtf_tables(titles = ), assemble_rtf()) and the
#  code (R/generate_rtfreport.R .substitute_page_tokens(),
#  .substitute_run_tokens(), .hf_row_empty(); R/user_tokens.R;
#  R/assemble_rtf.R .retotal_numpages(), .fill_book_page_slots(),
#  .extract_first_title(); R/assemble_spec.R .rtf_table_label()):
#
#    tokens   the package's own are those of rtf_text_tokens(): {PAGE} and
#             {TOTAL_PAGES} (numbers written into the file), {AUTO_PAGE}
#             (\chpgn) and {AUTO_TOTAL_PAGES} (a NUMPAGES field, its cached
#             result the file's total), {BOOK_PAGE} (an empty slot,
#             {\*\rtfreporterbookpage}), {PROGRAM} / {PROGRAM_FULL} (the
#             program, as given / absolute), {DATETIME} (the time written).
#             There is no {NUMPAGES} or {SECTIONPAGES} token ({SECTION_PAGES}
#             was removed and stops) and no {OUTPUT_*} token: the tests use
#             {OUTPUT_LABEL} as a token of the document's own
#             (rtf_document(tokens = )), {OUTPUT_POPULATION} as one of the
#             session (options(rtfreporter.tokens = )) and {OUTPUT_TITLE} as
#             an unknown token, printed as written.
#    where    headers, footers, titles and footnotes are filled (FR-DC282
#             FR-DC283 FR-DC285 FR-TB176); a table's cell is not: it prints
#             every token as written (FR-DC360).
#    pages    a static {PAGE} in a header or footer writes the band once a
#             page, each with its own number (FR-IX396); a title or
#             footnote is written once a page anyway, so it has the page's
#             number; any other token's band is written once a section.
#    rows     rtf_header(drop_empty_rows = TRUE) leaves out a row whose own
#             tokens are all empty ("<{POP}>"), only in a header or footer
#             (FR-DC331); a title, footnote or cell keeps it ("<>").
#    markup   the band's markup formats the text around a token ("H_{2}O"
#             is H{\sub 2}O with "script", H_\{2\}O with "none"), and the
#             token is filled all the same (FR-IX401).
#    text     non-ASCII text around a token is \uN? (FR-DC116).
#
#  (A) FT-DC293, pairwise, generate_rtfreport().  Factors and levels:
#    token    page, total, auto_page, auto_total, book, program,
#             program_full, datetime, doc ({OUTPUT_LABEL}), option
#             ({OUTPUT_POPULATION}), unknown ({OUTPUT_TITLE})
#    where    header, footer, title, footnote, cell
#    slot     l, c, r (the band's left / centre / right cell; a title row's
#             align; a cell's column)
#    rows     one, three (the token's row between two others), drop (a row
#             "<{POP}>" with POP = "" first, drop_empty_rows = TRUE)
#    doc      one (one page), pages (three pages, one section), sections
#             (three pages, sections from pages 1 and 2)
#    markup   none, script
#    text     ascii ("a" / "z"), nonascii (U+00E9 / U+65E5)
#  No constraints.  The product is 5940 rows; the candidates are every 23rd
#  of them (expand.grid order, 259 rows), checked to hold every pair of
#  levels, and .allpairs() picks the rows covering every pair (56).  Oracle:
#  the text between the markers "Mk" and "kM", for every time the band is
#  written, in order; the cell the token is in; the other rows.
#  FR-DC282 FR-DC283 FR-DC285 FR-DC281 FR-DC111 FR-DC115 FR-DC116 FR-TB176
#  FR-IX304 FR-IX396 FR-DC331 FR-DC360 FR-IX401.
#
#  (B) FT-AS147, pairwise, the page tokens through assembly.  Two input
#  files (Table 14.1.1 and 14.1.2, two pages each).  Factors and levels:
#    token     auto_page, auto_total, page, total, book, doc
#    where     header, title, footnote
#    sections  1, 2 (an input file of two sections)
#    numbering none, roman, decimal (toc_page_numbering)
#    cover     no, yes
#    assemble  rtf (assemble_rtf(), no contents), auto (assemble_rtf(toc =
#              "auto")), folder (assemble_folder())
#    book      none, "Bk {AUTO_PAGE} of {AUTO_TOTAL_PAGES}" (book_page)
#  The product is 1296 rows; .allpairs() picks the rows covering every
#  pair (22).  Oracle: the "Mk ... kM" texts of the book are those of the inputs,
#  in order, each NUMPAGES field's cached result the book's page count (4,
#  one more for a cover, one more for contents) and each slot filled with
#  book_page (else left empty); \chpgn as often as in the inputs (and the
#  slots); \pgnrestart 0 / 2 / 1 times; a contents entry's label is the
#  title as the file shows it, a dynamic page token and the slot left out.
#  FR-AS138 FR-AS140 FR-AS141 FR-IX111 FR-IX384 FR-AS250.
#
#  (C) FT-IX035, decision table: where a token of one's own comes from.
#    option            document          writes / does
#    OUTPUT_POPULATION -                 the option's value      FR-IX304
#    -                 OUTPUT_LABEL      the document's value    FR-DC115
#    STUDY = Opt       STUDY = Doc       Doc                     FR-IX304
#    STUDY = Opt       STUDY = ""        "", a drop row dropped  FR-IX402
#    STUDY = ""        -                 "", a drop row dropped  FR-DC331
#    STUDY = ""        STUDY = Doc       Doc, the row kept       FR-IX402
#    -                 PAGE = x          stops, rtf_document()   FR-DC118
#    DATETIME = x      -                 stops, generate         FR-IX402
#    -                 A = "{PAGE} {AUTO_PAGE} {OUTPUT_POPULATION}
#                          {PROGRAM}"    written as it is        FR-DC359
#    PAGE_NOTE = "{DATETIME}"  -         written as it is        FR-DC359
#    STUDY = Opt       STUDY = Doc       rtf_text_tokens(doc): once, Doc
#                                                                FR-IX314
#
#  (D) FT-DC294, decision table: an unknown token.
#    where                text                   writes / does
#    header               {OUTPUT_TITLE}         as written     FR-DC283
#    footer               {NUMPAGES}             as written, no field
#    title                {SECTIONPAGES}         as written, no field
#    footnote             {study} (STUDY set)    as written
#    header, drop rows    <{OUTPUT_TITLE}>       the row kept   FR-DC331
#    cell                 {OUTPUT_TITLE}         as written     FR-DC360
#    column header        {OUTPUT_TITLE}         as written     FR-DC284
#    footnote             {SECTION_PAGES}        stops          FR-DC286
#
#  (F) FT-DC295, decision table: doubled braces in a band.  {{ and }} print
#  a literal { and }, in a header, footer, title and footnote as in a column
#  header (#876); a table's cell prints them as written.
#    text                  band prints        FR-DC361
#    {{PAGE}}              {PAGE}, unfilled
#    {{STUDY}} {STUDY}     {STUDY} EX-1
#    a {{x}} b             a {x} b
#    {{{{ and }}}}         {{ and }}
#    {{PAGE}} {PAGE}       {PAGE} 1
#    ^{{STUDY}} (script)   the value EX-1 raised: the markup's braces first
#
#  (G) FT-IX036, decision table: a token and the braces of ^ / _ markup.
#  The braces after ^ or _ are the markup's own (#877): Ref^{STUDY} raises
#  the word STUDY; Ref^{{STUDY}} and p_{{PAGE}} raise the token's value,
#  Ref^{n={STUDY}} the text with it; with markup "none", ^ is a character.
#  FR-IX403 FR-IX401.
#
#  (E) FT-AS148, decision table: the page tokens after assembly, two input
#  files whose header has "{AUTO_PAGE} {AUTO_TOTAL_PAGES} {PAGE}
#  {TOTAL_PAGES} [{BOOK_PAGE}]" and whose footnote has {AUTO_TOTAL_PAGES}.
#    toc     numbering cover book_page          total  slot
#    -       -         no    NULL               4      empty
#    -       -         yes   NULL               5      empty
#    auto    none      no    "Bk {AUTO_PAGE}"   5      Bk \chpgn
#    auto    roman     yes   "{AUTO_TOTAL_PAGES}" 6    a NUMPAGES field, 6
#    folder  decimal   no    "{OUTPUT_TITLE}"   5      as written
#    auto    none      no    "{PAGE}"           stops  (FR-AS139)
#    auto    none      no    "p {TOTAL_PAGES}"  stops  (FR-AS139)
#  Every case: \chpgn stays (as often as in the inputs, and the slots'),
#  every NUMPAGES field stays a field with the book's total cached, the
#  static numbers stay those of the input ("1 2", "2 2"), \pgnrestart 0 /
#  1 / 2.  FR-AS138 FR-AS139 FR-AS140 FR-AS141 FR-IX111 FR-IX384.
# ---------------------------------------------------------------------------

.tk_time <- as.POSIXct("2026-01-02 03:04:00")

# every pair of levels of two factors, as .allpairs() spells them
.tk_pairs <- function(levels) {
  pr <- utils::combn(names(levels), 2L, simplify = FALSE)
  unlist(lapply(pr, function(p) {
    g <- expand.grid(a = levels[[p[1L]]], b = levels[[p[2L]]],
                     stringsAsFactors = FALSE)
    paste(p[1L], g$a, p[2L], g$b, sep = "|")
  }))
}

.tk_read <- function(f) paste(readLines(f, warn = FALSE), collapse = "\n")

# the texts between the markers, in order
.tk_marked <- function(txt) {
  regmatches(txt, gregexpr("Mk .*? kM", txt))[[1L]]
}

.tk_count <- function(txt, pattern, fixed = TRUE) {
  m <- gregexpr(pattern, txt, fixed = fixed)[[1L]]
  if (m[1L] == -1L) 0L else length(m)
}

.tk_numpages <- function(n) {
  sprintf("{\\field{\\*\\fldinst NUMPAGES}{\\fldrslt %d}}", as.integer(n))
}

.tk_names <- c(page = "PAGE", total = "TOTAL_PAGES", auto_page = "AUTO_PAGE",
               auto_total = "AUTO_TOTAL_PAGES", book = "BOOK_PAGE",
               program = "PROGRAM", program_full = "PROGRAM_FULL",
               datetime = "DATETIME", doc = "OUTPUT_LABEL",
               option = "OUTPUT_POPULATION", unknown = "OUTPUT_TITLE")

# what a filled token writes on page k of n
.tk_value <- function(tok, k, n, prog) {
  esc <- function(s) gsub("\\", "\\\\", s, fixed = TRUE)
  switch(tok,
         page = as.character(k), total = as.character(n),
         auto_page = "\\chpgn ", auto_total = .tk_numpages(n),
         book = "{\\*\\rtfreporterbookpage}",
         program = esc(prog),
         program_full = esc(normalizePath(prog, winslash = "\\",
                                          mustWork = FALSE)),
         datetime = "02Jan2026  03:04", doc = "Lbl-1", option = "Safety",
         unknown = "\\{OUTPUT_TITLE\\}")
}

# One case of (A): the document written, the failures found.
.tk_case_a <- function(cs, dir, prog) {
  tok <- cs$token
  pre <- if (cs$text == "ascii") "a" else "é"
  post <- if (cs$text == "ascii") "z" else "日"
  txt <- paste0("Mk ", pre, " {", .tk_names[[tok]], "} ", post, " H_{2}O kM")
  n <- if (cs$doc == "one") 1L else 3L
  slots <- c(l = "Fl", c = "Fc", r = "Fr")
  slots[[cs$slot]] <- txt
  first <- switch(cs$rows, one = NULL, three = "Rowfirst", drop = "<{POP}>")
  last <- if (cs$rows != "one") "Rowlast"
  band <- function(footer = FALSE) {
    rows <- c(if (!is.null(first)) list(c(l = first)), list(slots),
              if (!is.null(last)) list(c(r = last)))
    f <- if (footer) rtf_footer else rtf_header
    f(rows, markup = cs$markup, drop_empty_rows = cs$rows == "drop")
  }
  df <- data.frame(A = "Fl", B = "Fc", C = "Fr")
  df[[match(cs$slot, c("l", "c", "r"))]] <- if (cs$where == "cell") txt else "Fx"
  if (cs$where == "cell" && cs$rows != "one") {
    df <- rbind(data.frame(A = first, B = "", C = ""), df,
                data.frame(A = "", B = "", C = last))
  }
  tbls <- lapply(seq_len(n), function(i) rtftable(df, markup = cs$markup))
  doc <- rtf_document(program = prog,
                      tokens = list(OUTPUT_LABEL = "Lbl-1", POP = "")) |>
    rtf_tables(tbls)
  if (cs$where == "title") {
    align <- c(l = "left", c = "center", r = "right")[[cs$slot]]
    blk <- c(if (!is.null(first)) list(first), list(list(text = txt, align = align)),
             if (!is.null(last)) list(last))
    doc <- rtf_titles(doc, list(blk), markup = cs$markup)
  }
  if (cs$where == "footnote") {
    blk <- c(if (!is.null(first)) list(first), list(slots),
             if (!is.null(last)) list(last))
    doc <- rtf_footnotes(doc, list(blk), markup = cs$markup)
  }
  hf <- cs$where %in% c("header", "footer")
  sec <- function(i) {
    if (hf) {
      if (cs$where == "header") list(header = band()) else list(footer = band(TRUE))
    } else {
      list(header = rtf_header(c(l = paste0("Sec", i))))
    }
  }
  doc <- if (cs$doc == "sections") {
    rtf_section(doc, page = c(1, 2), secinfo = list(sec(1L), sec(2L)))
  } else {
    rtf_section(doc, secinfo = sec(1L))
  }
  f <- file.path(dir, sprintf("a%03d.rtf", cs$id))
  generate_rtfreport(doc, f)
  out <- .tk_read(f)
  bad <- character(0)
  # the marked texts
  times <- if (hf && tok != "page") (if (cs$doc == "sections") 2L else 1L) else n
  pre_rtf <- if (cs$text == "ascii") "a" else "\\u233?"
  post_rtf <- if (cs$text == "ascii") "z" else "\\u26085?"
  h2o <- if (cs$markup == "script") "H{\\sub 2}O" else "H_\\{2\\}O"
  want <- vapply(seq_len(times), function(k) {
    v <- if (cs$where == "cell") paste0("\\{", .tk_names[[tok]], "\\}") else
      .tk_value(tok, k, n, prog)
    paste0("Mk ", pre_rtf, " ", v, " ", post_rtf, " ", h2o, " kM")
  }, "")
  got <- .tk_marked(out)
  if (!identical(got, want)) bad <- c(bad, "text")
  # the cell the token is in
  k <- match(cs$slot, c("l", "c", "r"))
  if (cs$where == "title") {
    q <- regmatches(out, gregexpr("\\\\q[lcr]\\\\li0\\\\ri0 \\\\b Mk", out))[[1L]]
    if (!length(q) || any(substr(q, 3L, 3L) != cs$slot)) bad <- c(bad, "align")
  } else {
    chunks <- strsplit(out, "\\row", fixed = TRUE)[[1L]]
    chunks <- chunks[grepl("Mk ", chunks, fixed = TRUE)]
    at <- vapply(chunks, function(ch) {
      # a cell ends at \cell (not \cellx, the cell's right edge)
      cells <- strsplit(ch, "\\\\cell(?!x)", perl = TRUE)[[1L]]
      which(grepl("Mk ", cells, fixed = TRUE))[1L]
    }, 0L, USE.NAMES = FALSE)
    if (!length(at) || any(at != k)) bad <- c(bad, "cell")
  }
  # the other rows
  if (cs$rows == "three" &&
      (.tk_count(out, "Rowfirst") != times || .tk_count(out, "Rowlast") != times)) {
    bad <- c(bad, "rows")
  }
  if (cs$rows == "drop") {
    gone <- .tk_count(out, "<>") + .tk_count(out, "<\\{POP\\}>")
    kept <- switch(cs$where, header = , footer = 0L, title = , footnote = ,
                   cell = n)
    if (gone != kept || .tk_count(out, "Rowlast") != times) bad <- c(bad, "drop")
    if (cs$where == "cell" && .tk_count(out, "<\\{POP\\}>") != n) bad <- c(bad, "drop cell")
  }
  bad
}

test_that("FT-DC293 [FR-DC282 FR-DC283 FR-DC285 FR-DC281 FR-DC111 FR-DC115 FR-DC116 FR-TB176 FR-IX304 FR-IX396 FR-DC331 FR-DC360 FR-IX401]: pairwise -- token x where x cell x rows x document x markup x text", {
  old <- options(rtfreporter.render_time = .tk_time,
                 rtfreporter.tokens = list(OUTPUT_POPULATION = "Safety"))
  on.exit(options(old), add = TRUE)
  dir <- tempfile("tk_a")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  prog <- file.path(dir, "t_tok.R")
  writeLines("# fictional program", prog)
  levels <- list(token = names(.tk_names),
                 where = c("header", "footer", "title", "footnote", "cell"),
                 slot = c("l", "c", "r"), rows = c("one", "three", "drop"),
                 doc = c("one", "pages", "sections"),
                 markup = c("none", "script"), text = c("ascii", "nonascii"))
  cand <- do.call(expand.grid, c(levels, stringsAsFactors = FALSE))
  expect_identical(nrow(cand), 5940L)
  cand <- cand[seq(1L, nrow(cand), by = 23L), ]
  rownames(cand) <- NULL
  ap <- .allpairs(cand)
  # the candidates hold every pair, and the rows picked cover them all
  expect_setequal(ap$need, .tk_pairs(levels))
  expect_setequal(ap$covered, ap$need)
  cases <- ap$rows
  cases$id <- seq_len(nrow(cases))
  for (i in seq_len(nrow(cases))) {
    cs <- cases[i, ]
    bad <- .tk_case_a(cs, dir, prog)
    expect_identical(bad, character(0),
                     info = paste(names(cs), unlist(cs), sep = "=", collapse = " "))
  }
})

# ---------------------------------------------------------------------------
#  (B) the page tokens through assembly
# ---------------------------------------------------------------------------

.tk_names_b <- c(auto_page = "AUTO_PAGE", auto_total = "AUTO_TOTAL_PAGES",
                 page = "PAGE", total = "TOTAL_PAGES", book = "BOOK_PAGE",
                 doc = "OUTPUT_LABEL")

# An input file: Table 14.1.<k>, two pages, the token in `where`.
.tk_input <- function(path, k, tok, where, sections) {
  mark <- paste0("Mk {", .tk_names_b[[tok]], "} kM")
  hdr <- rtf_header(c(l = "Example Pharma", r = if (where == "header") mark else "Fr"))
  ttl <- paste0("Table 14.1.", k, " Ages", if (where == "title") paste0(" ", mark))
  df <- data.frame(Param = "Age", Value = "51")
  doc <- rtf_document(tokens = list(OUTPUT_LABEL = "Lbl-1")) |>
    rtf_tables(list(df, df)) |>
    rtf_titles(list(ttl))
  if (where == "footnote") doc <- rtf_footnotes(doc, list(mark))
  doc <- if (sections == "2") {
    rtf_section(doc, page = c(1, 2), secinfo = list(list(header = hdr), list(header = hdr)))
  } else {
    rtf_section(doc, header = hdr)
  }
  generate_rtfreport(doc, path)
  path
}

# a title's text as a contents label: a dynamic page token and the slot out
.tk_label <- function(tok, where, k) {
  base <- paste0("Table 14.1.", k, " Ages")
  if (where != "title") return(base)
  v <- switch(tok, page = " 1", total = " 2", doc = " Lbl-1", "")
  paste0(base, " Mk", v, " kM")
}

test_that("FT-AS147 [FR-AS138 FR-AS140 FR-AS141 FR-IX111 FR-IX384 FR-AS250]: pairwise -- page token x where x sections x numbering x cover x assembler x book_page", {
  dir <- tempfile("tk_b")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  levels <- list(token = names(.tk_names_b),
                 where = c("header", "title", "footnote"),
                 sections = c("1", "2"), numbering = c("none", "roman", "decimal"),
                 cover = c("no", "yes"), assemble = c("rtf", "auto", "folder"),
                 book = c("none", "given"))
  cand <- do.call(expand.grid, c(levels, stringsAsFactors = FALSE))
  expect_identical(nrow(cand), 1296L)
  ap <- .allpairs(cand)
  expect_setequal(ap$need, .tk_pairs(levels))
  expect_setequal(ap$covered, ap$need)
  cases <- ap$rows
  for (i in seq_len(nrow(cases))) {
    cs <- cases[i, ]
    info <- paste(names(cs), unlist(cs), sep = "=", collapse = " ")
    src <- file.path(dir, paste0("in", i))
    dir.create(src)
    f <- vapply(1:2, function(k) {
      .tk_input(file.path(src, sprintf("t_14_1_%d.rtf", k)), k, cs$token,
                cs$where, cs$sections)
    }, "")
    out <- file.path(dir, sprintf("book%02d.rtf", i))
    cover <- if (cs$cover == "yes") list(title = "Example Pharma Study EX-1")
    book <- if (cs$book == "given") "Bk {AUTO_PAGE} of {AUTO_TOTAL_PAGES}"
    if (cs$assemble == "folder") {
      assemble_folder(src, out, toc_page_numbering = cs$numbering,
                      cover = cover, book_page = book)
    } else {
      assemble_rtf(f, out, cover = cover,
                   toc = if (cs$assemble == "auto") "auto",
                   toc_page_numbering = cs$numbering, book_page = book)
    }
    ins <- vapply(f, .tk_read, "")
    txt <- .tk_read(out)
    # the book's pages: the contents entries and outline lines left out
    lines <- readLines(out, warn = FALSE)
    body <- paste(lines[!grepl("HYPERLINK|outlinelevel", lines)], collapse = "\n")
    has_toc <- cs$assemble != "rtf"
    total <- 4L + (cs$cover == "yes") + has_toc
    # the inputs' texts, each cache the book's total, each slot filled
    want <- unlist(lapply(ins, .tk_marked), use.names = FALSE)
    want <- gsub("\\{\\\\fldrslt [0-9]+\\}", sprintf("{\\\\fldrslt %d}", total), want)
    slot <- "{\\*\\rtfreporterbookpage}"
    fill <- if (!is.null(book)) paste0("Bk \\chpgn  of ", .tk_numpages(total)) else slot
    want <- gsub(slot, fill, want, fixed = TRUE)
    expect_identical(.tk_marked(body), want, info = info)
    # \\chpgn as often as in the inputs, and the slots filled
    n_slot <- sum(vapply(ins, .tk_count, 0L, pattern = slot))
    n_chpgn <- sum(vapply(ins, .tk_count, 0L, pattern = "\\chpgn")) +
      if (!is.null(book)) n_slot else 0L
    expect_identical(.tk_count(txt, "\\chpgn"), n_chpgn, info = info)
    # every NUMPAGES field a field, with the book's total
    caches <- regmatches(txt, gregexpr("\\\\fldinst NUMPAGES\\}\\{\\\\fldrslt [0-9]+\\}", txt))[[1L]]
    expect_true(all(grepl(sprintf("fldrslt %d}", total), caches, fixed = TRUE)), info = info)
    # the numbering
    restarts <- if (!has_toc) 0L else switch(cs$numbering, none = 0L, roman = 2L, decimal = 1L)
    expect_identical(.tk_count(txt, "\\pgnrestart"), restarts, info = info)
    # the contents entries
    if (has_toc) {
      labels <- regmatches(txt, gregexpr("HYPERLINK \\\\\\\\l \"[^\"]*\"\\}\\{\\\\fldrslt [^}]*\\}", txt))[[1L]]
      labels <- sub("^.*\\\\fldrslt ", "", sub("\\}$", "", labels))
      want_lab <- vapply(1:2, function(k) .tk_label(cs$token, cs$where, k), "")
      if (cs$assemble == "folder") want_lab <- sub(" Ages", "  Ages", want_lab, fixed = TRUE)
      expect_identical(labels, want_lab, info = info)
      expect_false(grepl("\\\\chpgn|\\\\field|rtfreporterbookpage", paste(labels, collapse = " ")))
    }
  }
})

# ---------------------------------------------------------------------------
#  (C) where a token of one's own comes from
# ---------------------------------------------------------------------------

# "NAME=value;NAME=value" (or "-") as a named list
.tk_kv <- function(x) {
  if (identical(x, "-")) return(NULL)
  kv <- strsplit(strsplit(x, ";", fixed = TRUE)[[1L]], "=", fixed = TRUE)
  stats::setNames(lapply(kv, function(p) if (length(p) > 1L) p[2L] else ""),
                  vapply(kv, `[`, "", 1L))
}

test_that("FT-IX035 [FR-IX304 FR-DC115 FR-DC331 FR-DC118 FR-DC359 FR-IX314 FR-IX402]: decision table -- a token of one's own: the document, the option and rtfreporter's own", {
  dir <- tempfile("tk_c")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  old <- options(rtfreporter.render_time = .tk_time, rtfreporter.tokens = NULL)
  on.exit(options(old), add = TRUE)
  cases <- data.frame(
    opt  = c("OUTPUT_POPULATION=Safety", "-", "STUDY=Opt", "STUDY=Opt",
             "STUDY=", "STUDY=", "-", "DATETIME=x", "-",
             "PAGE_NOTE={DATETIME}"),
    doc  = c("-", "OUTPUT_LABEL=Lbl-1", "STUDY=Doc", "STUDY=", "-",
             "STUDY=Doc", "PAGE=x", "-",
             "A={PAGE} {AUTO_PAGE} {OUTPUT_POPULATION} {PROGRAM} {A}", "-"),
    text = c("{OUTPUT_POPULATION}", "{OUTPUT_LABEL}", "{STUDY}", "<{STUDY}>",
             "<{STUDY}>", "<{STUDY}>", "{PAGE}", "{DATETIME}", "{A}",
             "{PAGE_NOTE}"),
    want = c("Safety", "Lbl-1", "Doc", "<dropped>", "<dropped>", "<Doc>",
             "<error:rtfreporter's own token>",
             "<error:options\\(rtfreporter.tokens\\): \\{DATETIME\\} is rtfreporter's own>",
             "\\{PAGE\\} \\{AUTO_PAGE\\} \\{OUTPUT_POPULATION\\} \\{PROGRAM\\} \\{A\\}",
             "\\{DATETIME\\}"),
    stringsAsFactors = FALSE)
  for (i in seq_len(nrow(cases))) {
    cs <- cases[i, ]
    info <- paste(names(cs), unlist(cs), sep = "=", collapse = " ")
    options(rtfreporter.tokens = .tk_kv(cs$opt))
    err <- if (startsWith(cs$want, "<error:")) sub("^<error:(.*)>$", "\\1", cs$want)
    build <- function() {
      rtf_document(program = "prog/t_own.R", tokens = .tk_kv(cs$doc)) |>
        rtf_tables(list(data.frame(A = "x"))) |>
        rtf_titles(list(paste("Mk", cs$text, "kM"))) |>
        rtf_section(header = rtf_header(list(c(l = cs$text), c(l = "Rowlast")),
                                        drop_empty_rows = TRUE))
    }
    f <- file.path(dir, sprintf("c%02d.rtf", i))
    if (!is.null(err) && grepl("own token", err) && cs$doc != "-") {
      expect_error(build(), err, info = info)
      next
    }
    if (!is.null(err)) {
      expect_error(generate_rtfreport(build(), f), err, info = info)
      next
    }
    generate_rtfreport(build(), f)
    out <- .tk_read(f)
    hdr <- regmatches(out, regexpr("\\{\\\\header[^\n]*", out))
    if (cs$want == "<dropped>") {
      # the title keeps "<>", the header leaves the row out
      expect_identical(.tk_marked(out), "Mk <> kM", info = info)
      expect_false(grepl("<", hdr, fixed = TRUE), info = info)
      expect_match(hdr, "Rowlast", fixed = TRUE, info = info)
    } else {
      expect_identical(.tk_marked(out), paste("Mk", cs$want, "kM"), info = info)
      expect_match(hdr, paste0("\\ql\\li0\\ri0 ", cs$want, "\\cell"), fixed = TRUE,
                   info = info)
    }
  }
  # rtf_text_tokens(doc) lists a name both set once, with the document's value
  options(rtfreporter.tokens = list(STUDY = "Opt"))
  tt <- rtf_text_tokens(rtf_document(tokens = list(STUDY = "Doc")))
  expect_identical(tt$example[tt$token == "{STUDY}"], "Doc")
})

# ---------------------------------------------------------------------------
#  (D) an unknown token
# ---------------------------------------------------------------------------

test_that("FT-DC294 [FR-DC283 FR-DC331 FR-DC360 FR-DC284 FR-DC286]: decision table -- an unknown token is printed as written; {SECTION_PAGES} stops", {
  dir <- tempfile("tk_d")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  cases <- data.frame(
    where = c("header", "footer", "title", "footnote", "drop", "cell",
              "colhdr", "footnote"),
    text  = c("{OUTPUT_TITLE}", "{NUMPAGES}", "{SECTIONPAGES}", "{study}",
              "<{OUTPUT_TITLE}>", "{OUTPUT_TITLE}", "{OUTPUT_TITLE}",
              "{SECTION_PAGES}"),
    want  = c("\\{OUTPUT_TITLE\\}", "\\{NUMPAGES\\}", "\\{SECTIONPAGES\\}",
              "\\{study\\}", "<\\{OUTPUT_TITLE\\}>", "\\{OUTPUT_TITLE\\}",
              "\\{OUTPUT_TITLE\\}", "<error>"),
    stringsAsFactors = FALSE)
  for (i in seq_len(nrow(cases))) {
    cs <- cases[i, ]
    info <- paste(names(cs), unlist(cs), sep = "=", collapse = " ")
    mark <- paste("Mk", cs$text, "kM")
    df <- data.frame(A = if (cs$where == "cell") mark else "x")
    tbl <- if (cs$where == "colhdr") rtftable(df, col_header = list(mark)) else rtftable(df)
    doc <- rtf_document(tokens = list(STUDY = "EX-1", POP = "")) |>
      rtf_tables(list(tbl))
    if (cs$where == "title") doc <- rtf_titles(doc, list(mark))
    if (cs$where == "footnote") doc <- rtf_footnotes(doc, list(mark))
    band <- switch(cs$where,
                   header = rtf_header(c(l = mark)),
                   drop = rtf_header(list(c(l = "<{POP}>"), c(l = mark)),
                                     drop_empty_rows = TRUE))
    if (cs$where == "footer") band <- list(footer = rtf_footer(c(l = mark)))
    if (cs$where %in% c("header", "drop")) band <- list(header = band)
    if (!is.null(band)) doc <- rtf_section(doc, secinfo = band)
    f <- file.path(dir, sprintf("d%02d.rtf", i))
    if (cs$want == "<error>") {
      expect_error(generate_rtfreport(doc, f), "`\\{SECTION_PAGES\\}` was removed",
                   info = info)
      next
    }
    generate_rtfreport(doc, f)
    out <- .tk_read(f)
    expect_identical(.tk_marked(out), paste("Mk", cs$want, "kM"), info = info)
    expect_identical(.tk_count(out, "\\fldinst SECTIONPAGES"), 0L, info = info)
    expect_identical(.tk_count(out, "\\fldinst NUMPAGES"), 0L, info = info)
    if (cs$where == "drop") expect_false(grepl("<>", out, fixed = TRUE), info = info)
  }
})

test_that("FT-DC295 [FR-DC361 FR-DC283 FR-DC360 FR-IX401]: decision table -- {{ and }} are literal braces in a band, as in a column header", {
  dir <- tempfile("tk_br")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  cases <- data.frame(
    text   = c("{{PAGE}}", "{{STUDY}} {STUDY}", "a {{x}} b", "{{{{ and }}}}",
               "{{PAGE}} {PAGE}", "^{{STUDY}}", "{{OUTPUT_TITLE}}"),
    markup = c("none", "none", "none", "none", "none", "script", "none"),
    want   = c("\\{PAGE\\}", "\\{STUDY\\} EX-1", "a \\{x\\} b",
               "\\{\\{ and \\}\\}", "\\{PAGE\\} 1", "{\\super EX-1}",
               "\\{OUTPUT_TITLE\\}"),
    stringsAsFactors = FALSE)
  k <- 0L
  for (where in c("header", "footer", "title", "footnote", "cell")) {
    for (i in seq_len(nrow(cases))) {
      cs <- cases[i, ]
      info <- paste(where, paste(names(cs), unlist(cs), sep = "=", collapse = " "))
      mark <- paste("Mk", cs$text, "kM")
      tbl <- rtftable(data.frame(A = if (where == "cell") mark else "x"),
                      markup = cs$markup)
      doc <- rtf_document(tokens = list(STUDY = "EX-1")) |> rtf_tables(list(tbl))
      if (where == "title") doc <- rtf_titles(doc, list(mark))
      if (where == "footnote") doc <- rtf_footnotes(doc, list(mark))
      if (where == "header") {
        doc <- rtf_section(doc, secinfo = list(
          header = rtf_header(c(l = mark), markup = cs$markup)))
      }
      if (where == "footer") {
        doc <- rtf_section(doc, secinfo = list(
          footer = rtf_footer(c(l = mark), markup = cs$markup)))
      }
      k <- k + 1L
      f <- file.path(dir, sprintf("b%03d.rtf", k))
      generate_rtfreport(doc, f)
      got <- .tk_marked(.tk_read(f))
      if (where == "cell") {
        # a cell prints the text as written (FR-DC360)
        want <- paste("Mk", gsub("([{}])", "\\\\\\1", cs$text), "kM")
        if (cs$markup == "script") next
      } else {
        want <- paste("Mk", cs$want, "kM")
      }
      expect_identical(got, want, info = info)
    }
  }
})

test_that("FT-IX036 [FR-IX403 FR-IX401 FR-DC361]: decision table -- a token and the braces of ^ / _ markup", {
  dir <- tempfile("tk_sc")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  cases <- data.frame(
    text   = c("Ref^{STUDY}", "Ref^{{STUDY}}", "p_{{PAGE}}", "Ref^{n={STUDY}}",
               "H_{2}O {STUDY}", "Ref^{{STUDY}}", "Ref^{STUDY}"),
    markup = c("script", "script", "script", "script", "script", "none", "none"),
    want   = c("Ref{\\super STUDY}", "Ref{\\super EX-1}", "p{\\sub 1}",
               "Ref{\\super n=EX-1}", "H{\\sub 2}O EX-1", "Ref^\\{STUDY\\}",
               "Ref^EX-1"),
    stringsAsFactors = FALSE)
  k <- 0L
  for (where in c("header", "footer", "title", "footnote")) {
    for (i in seq_len(nrow(cases))) {
      cs <- cases[i, ]
      info <- paste(where, paste(names(cs), unlist(cs), sep = "=", collapse = " "))
      mark <- paste("Mk", cs$text, "kM")
      doc <- rtf_document(tokens = list(STUDY = "EX-1")) |>
        rtf_tables(list(data.frame(A = "x")))
      if (where == "title") doc <- rtf_titles(doc, list(mark), markup = cs$markup)
      if (where == "footnote") doc <- rtf_footnotes(doc, list(mark), markup = cs$markup)
      if (where == "header") {
        doc <- rtf_section(doc, secinfo = list(
          header = rtf_header(c(l = mark), markup = cs$markup)))
      }
      if (where == "footer") {
        doc <- rtf_section(doc, secinfo = list(
          footer = rtf_footer(c(l = mark), markup = cs$markup)))
      }
      k <- k + 1L
      f <- file.path(dir, sprintf("s%03d.rtf", k))
      generate_rtfreport(doc, f)
      expect_identical(.tk_marked(.tk_read(f)), paste("Mk", cs$want, "kM"),
                       info = info)
    }
  }
})

# ---------------------------------------------------------------------------
#  (E) the page tokens after assembly
# ---------------------------------------------------------------------------

test_that("FT-AS148 [FR-AS138 FR-AS139 FR-AS140 FR-AS141 FR-IX111 FR-IX384]: decision table -- page tokens after assembly: the cached numbers, and fields stay fields", {
  dir <- tempfile("tk_e")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  src <- file.path(dir, "in")
  dir.create(src)
  mk <- function(k) {
    df <- data.frame(Param = "Age", Value = "51")
    doc <- rtf_document() |>
      rtf_tables(list(df, df)) |>
      rtf_titles(list(paste0("Table 14.1.", k, " Ages"))) |>
      rtf_footnotes(list("Fn {AUTO_TOTAL_PAGES}")) |>
      rtf_section(header = rtf_header(c(
        l = "Example Pharma",
        r = "Mk {AUTO_PAGE} {AUTO_TOTAL_PAGES} {PAGE} {TOTAL_PAGES} [{BOOK_PAGE}] kM")))
    f <- file.path(src, sprintf("t_14_1_%d.rtf", k))
    generate_rtfreport(doc, f)
    f
  }
  f <- c(mk(1L), mk(2L))
  ins <- vapply(f, .tk_read, "")
  n_chpgn <- sum(vapply(ins, .tk_count, 0L, pattern = "\\chpgn"))
  n_fields <- sum(vapply(ins, .tk_count, 0L, pattern = "\\fldinst NUMPAGES"))
  cases <- data.frame(
    toc   = c("-", "-", "auto", "auto", "folder", "auto", "auto"),
    num   = c("none", "none", "none", "roman", "decimal", "none", "none"),
    cover = c("no", "yes", "no", "yes", "no", "no", "no"),
    book  = c("-", "-", "Bk {AUTO_PAGE}", "{AUTO_TOTAL_PAGES}", "{OUTPUT_TITLE}",
              "{PAGE}", "p {TOTAL_PAGES}"),
    total = c(4L, 5L, 5L, 6L, 5L, NA, NA),
    slot  = c("{\\*\\rtfreporterbookpage}", "{\\*\\rtfreporterbookpage}",
              "Bk \\chpgn ", .tk_numpages(6L), "\\{OUTPUT_TITLE\\}", "<error>",
              "<error>"),
    restarts = c(0L, 0L, 0L, 2L, 1L, NA, NA),
    stringsAsFactors = FALSE)
  for (i in seq_len(nrow(cases))) {
    cs <- cases[i, ]
    info <- paste(names(cs), unlist(cs), sep = "=", collapse = " ")
    out <- file.path(dir, sprintf("e%02d.rtf", i))
    cover <- if (cs$cover == "yes") list(title = "Example Pharma Study EX-1")
    book <- if (cs$book != "-") cs$book
    run <- function() {
      if (cs$toc == "folder") {
        assemble_folder(src, out, toc_page_numbering = cs$num, cover = cover,
                        book_page = book)
      } else {
        assemble_rtf(f, out, cover = cover, toc = if (cs$toc == "auto") "auto",
                     toc_page_numbering = cs$num, book_page = book)
      }
    }
    if (cs$slot == "<error>") {
      expect_error(run(), "cannot use the static", info = info)
      next
    }
    run()
    txt <- .tk_read(out)
    # the static numbers stay the input's; the dynamic ones stay fields
    hdr <- sprintf("Mk \\chpgn  %s %d 2 [%s] kM", .tk_numpages(cs$total), 1:2, cs$slot)
    expect_identical(.tk_marked(txt), rep(hdr, 2L), info = info)
    expect_match(txt, paste("Fn", .tk_numpages(cs$total)), fixed = TRUE, info = info)
    slot_fields <- if (grepl("NUMPAGES", cs$slot)) 4L else 0L
    slot_chpgn <- if (grepl("chpgn", cs$slot)) 4L else 0L
    expect_identical(.tk_count(txt, "\\chpgn"), n_chpgn + slot_chpgn, info = info)
    expect_identical(.tk_count(txt, "\\fldinst NUMPAGES"), n_fields + slot_fields,
                     info = info)
    caches <- regmatches(txt, gregexpr("fldinst NUMPAGES\\}\\{\\\\fldrslt [0-9]+", txt))[[1L]]
    expect_true(all(endsWith(caches, paste0(" ", cs$total))), info = info)
    expect_identical(.tk_count(txt, "\\pgnrestart"), cs$restarts, info = info)
  }
})

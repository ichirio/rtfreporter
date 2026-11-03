# Validation tests: how a style value is resolved (#804) -- decision tables and
# a pairwise set.  Helpers prefixed .sp_.  Fictional data only.
#
# ---------------------------------------------------------------------------
#  Design
# ---------------------------------------------------------------------------
#  Subject: the value a module ends up with for a style key.  The sources,
#  highest first, as read from the code (R/rtf_style.R .doc_effective_style(),
#  .style_fill(), .apply_style_to_report(); R/generate_rtfreport.R doc_* chain):
#
#    S1 arg       the module's own argument: rtftable(), rtf_titles(),
#                 rtf_footnotes(), rtf_header(), rtf_footer()
#    S2 section   rtf_style(<module> = list(<key> = ))
#    S3 shared    rtf_style(<key> = )
#    S4 option    options(rtfreporter.<key> = )
#    S5 baseline  the resource baseline (always there)
#
#  Factors and levels
#    key     font_size_half_points (a size), cell_padding_left_twips (a
#            padding), markup, title_format (a shared-only key)
#    module  table, titles, footnotes, header, footer -- where the key applies:
#            the size and markup all five; the padding table, header, footer
#            (titles / footnotes have no padding argument or section key);
#            title_format the titles, from S3, S4 only
#    sources which of S1..S4 are set (S5 always)
#    route   the style given to rtf_document() or to
#            rtf_config() (pairwise set only)
#
#  Values: each source has its own value, so the output names the winner.
#    size     S1 30, S2 28, S3 26, S4 22, S5 18      (\fsN)
#    padding  S1 55, S2 66, S3 44, S4 77, S5 0       (\liN)
#    markup   the winner "relational", every other source "none", S5 "script"
#    title_format  the winner "table", every other source "text", S5 "text"
#  The output read is the line holding the module's text: the last \fs (else
#  the document's \fs) and the last \li before it, how ">= 1^{2}" is written
#  after it, and whether the title is a table row.
#
#  Decision table (A), per key x module, run by the loop in FT-DC248:
#    row  set                          winner     FR
#    R01  none                         S5         FR-DC315
#    R02  S1                           S1         FR-DC315
#    R03  S2                           S2         FR-DC315 FR-DC312..314
#    R04  S3                           S3         FR-DC315 FR-DC309
#    R05  S4                           S4         FR-DC315
#    R06  S1 S2                        S1         FR-DC315
#    R07  S2 S3                        S2         FR-DC315
#    R08  S3 S4                        S3         FR-DC315
#    R09  S1 S2 S3 S4                  S1         FR-DC315
#  title_format (titles; S3 S4): none -> S5; S3, S4 alone; S3 S4 -> S3
#  (FR-DC311).
#  Rows: 9 x (5 size + 3 padding + 5 markup) + 5 = 122.
#
#  Pairwise set (B), FT-DC249: key (3) x module (where it applies) x the two
#  sources set (any 2 of S1..S4, 6) x route (2) -- every pair of levels of
#  every two factors at least once, picked from the valid full product by the
#  greedy all-pairs helper .allpairs() (helper-allpairs.R); winner the higher source.
#  FR-DC315 FR-DC318.
#
#  Decision table (C), FT-DC250: rtf_config(style = ) over
#  rtf_document(style = ), the content table's size and padding.
#    doc style              config style(s)            size  pad   FR
#    shared 26              shared 20                  20    0     FR-DC318
#    table 28               table 21                   21    0     FR-DC318
#    table 28               shared 20                  28    0     FR-DC337
#    shared 26              table 21                   21    0     FR-DC337
#    shared 26, table 28    shared pad 40              28    40    FR-DC318
#    shared 26              NULL                       26    0     FR-DC318
#    shared 26              shared 20, then shared 24  24    0     FR-DC318
#
#  Decision table (D), FT-DC251: the size without a height (.style_fill), for
#  the table and the header band.  Size from S1 / S2 / S3 / none x height
#  (row_height_twips) from S1 410 / S2 420 / S3 430 / S4 450 / none.
#    height S1                                -> 410            FR-DC316
#    size S1 or S2, height S3 / S4 / none -> the height the size implies
#                                                (as with no height at all)
#                                                FR-DC316 FR-DC336
#    size S1, height S2                       -> the size's height FR-DC316
#    size S2, height S2                       -> 420            FR-DC316
#    size S3 or none, height S2..S4           -> that height    FR-DC315
#    size S3 or none, no height               -> the size's height
#  Rows: 4 x 5 x 2 = 40.
#
#  Decision table (E), FT-DC252: a band's default border counts as unset.
#    band      its border             section border   written   FR
#    header    not passed             bottom           bottom    FR-DC316
#    header    NULL passed            bottom           none      FR-DC316
#    header    top passed             bottom           top       FR-DC316
#    header    a bare named vector    bottom           bottom    FR-DC314
#    header    not passed             none             none      FR-DC326
#    footer    not passed             bottom           bottom    FR-DC316
#    footer    top passed (default)   bottom           top       FR-DC316
#    footer    NULL passed            bottom           none      FR-DC316
#    footer    not passed             none             top       FR-DC326
#    footnotes NULL (its default)     bottom           bottom    FR-DC316 FR-DC313
#    footnotes top passed             bottom           top       FR-DC313
#
#  (F), FT-DC253: a section key reaches its own module only -- each section of
#  each key set alone moves that module and leaves the other four at the
#  baseline (13 rows).  FR-DC312 FR-DC313 FR-DC314 FR-DC335.
#
#  (G), FT-DC254: the colour reaches the header and footer only.
#    shared             header, footer #7F7F7F; table, titles, footnotes none
#    shared + header    header the section's, footer the shared
#    + rtf_header(color = )  header its own
#    footer section     footer only
#    none               no colour anywhere
#  FR-DC310 FR-DC314 FR-DC315.
#
# ---------------------------------------------------------------------------

.SP_SOURCES <- c("arg", "section", "shared", "option")
.SP_MARK <- c(table = "TBL", titles = "TTL", footnotes = "FNT", header = "HDR",
              footer = "FTR")
.SP_MODULES <- list(
  font_size_half_points   = names(.SP_MARK),
  cell_padding_left_twips = c("table", "header", "footer"),
  markup                  = names(.SP_MARK),
  title_format            = "titles")
.SP_KEY_SOURCES <- list(
  font_size_half_points = .SP_SOURCES, cell_padding_left_twips = .SP_SOURCES,
  markup = .SP_SOURCES, title_format = c("shared", "option"))
.SP_NUM <- list(
  font_size_half_points   = c(arg = 30L, section = 28L, shared = 26L,
                              option = 22L, baseline = 18L),
  cell_padding_left_twips = c(arg = 55L, section = 66L, shared = 44L,
                              option = 77L, baseline = 0L))
.SP_CAT <- list(markup = c(win = "relational", lose = "none", baseline = "script"),
                title_format = c(win = "table", lose = "text", baseline = "text"))

.sp_or <- function(x, y) if (is.null(x)) y else x
.sp_text <- function(m) paste(.SP_MARK[[m]], ">= 1^{2}")
.sp_data <- function() data.frame(A = c(.sp_text("table"), "a2"), B = c("b1", "b2"))

# The value source `s` holds for `key` when `winner` is the highest set.
.sp_value <- function(key, s, winner) {
  if (!is.null(.SP_NUM[[key]])) return(.SP_NUM[[key]][[s]])
  cv <- .SP_CAT[[key]]
  if (identical(s, "baseline")) cv[["baseline"]] else if (identical(s, winner)) cv[["win"]] else cv[["lose"]]
}

# A document with all five modules.  `own`: per module, a list of its own
# arguments; `bands`: header / footer built by the caller instead.
.sp_doc <- function(own = list(), style = NULL, route = "document",
                    bands = list()) {
  o <- function(m) .sp_or(own[[m]], list())
  tbl <- do.call(rtftable, c(list(.sp_data()), o("table")))
  hd <- .sp_or(bands$header, do.call(rtf_header, c(list(c(l = .sp_text("header"))), o("header"))))
  ft <- .sp_or(bands$footer, do.call(rtf_footer, c(list(c(l = .sp_text("footer"))), o("footer"))))
  doc <- if (identical(route, "document")) {
    rtf_document(style = style)
  } else {
    rtf_config(rtf_document(), style = style)
  }
  doc <- doc |>
    rtf_section(page = 1, secinfo = list(header = hd, footer = ft)) |>
    rtf_tables(list(tbl))
  doc <- do.call(rtf_titles, c(list(doc, .sp_text("titles")), o("titles")))
  do.call(rtf_footnotes, c(list(doc, .sp_text("footnotes")), o("footnotes")))
}

# The line of the RTF that holds module m's text, split at the text.
.sp_line <- function(txt, m) {
  ln <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
  l <- ln[grepl(paste0(.SP_MARK[[m]], " "), ln, fixed = TRUE)][1L]
  at <- regexpr(paste0(.SP_MARK[[m]], " "), l, fixed = TRUE)
  list(line = l, before = substr(l, 1L, at - 1L), after = substr(l, at, nchar(l)))
}

.sp_last <- function(x, word) {
  m <- regmatches(x, gregexpr(paste0("\\\\", word, "[0-9]+"), x))[[1L]]
  if (!length(m)) return(NA_integer_)
  as.integer(sub(paste0("\\\\", word), "", m[length(m)]))
}

# What module m shows for key in the rendered text.
.sp_observe <- function(txt, key, m) {
  p <- .sp_line(txt, m)
  switch(key,
    font_size_half_points = {
      fs <- .sp_last(p$before, "fs")
      if (is.na(fs)) .sp_last(regmatches(txt, regexpr("\\\\widowctrl\\\\fs[0-9]+", txt)), "fs") else fs
    },
    cell_padding_left_twips = .sp_last(p$before, "li"),
    row_height_twips = .sp_last(p$before, "trrh"),
    markup = {
      rel <- grepl("\\u8805?", p$after, fixed = TRUE)
      scr <- grepl("{\\super 2}", p$after, fixed = TRUE)
      c("none", "script", "relational", "all")[1L + scr + 2L * rel]
    },
    title_format = if (startsWith(p$line, "\\trowd")) "table" else "text",
    border = {
      t <- grepl("\\clbrdrt", p$before, fixed = TRUE)
      b <- grepl("\\clbrdrb", p$before, fixed = TRUE)
      c("none", "top", "bottom", "both")[1L + t + 2L * b]
    },
    color = {
      cf <- .sp_last(p$before, "cf")
      if (is.na(cf)) return("none")
      tbl <- regmatches(txt, regexpr("\\{\\\\colortbl;[^}]*\\}", txt))
      ent <- strsplit(sub("^\\{\\\\colortbl;", "", tbl), ";", fixed = TRUE)[[1L]]
      rgb <- as.integer(regmatches(ent[cf], gregexpr("[0-9]+", ent[cf]))[[1L]])
      sprintf("#%02X%02X%02X", rgb[1L], rgb[2L], rgb[3L])
    })
}

# Build, render and read one case: `set` the sources set, `winner` the highest.
.sp_run <- function(key, module, set, route = "document") {
  winner <- if (length(set)) set[1L] else "baseline"
  v <- function(s) .sp_value(key, s, winner)
  opts <- list()
  if ("option" %in% set) opts[[paste0("rtfreporter.", key)]] <- v("option")
  old <- options(opts)
  on.exit(options(old), add = TRUE)
  own <- list()
  sty <- list()
  if ("arg" %in% set) own[[module]] <- stats::setNames(list(v("arg")), key)
  if ("section" %in% set) sty[[module]] <- stats::setNames(list(v("section")), key)
  if ("shared" %in% set) sty[[key]] <- v("shared")
  style <- if (length(sty)) do.call(rtf_style, sty) else NULL
  doc <- .sp_doc(own, style, route)
  list(got = .sp_observe(.render_to_string(doc), key, module),
       want = .sp_value(key, winner, winner))
}

# The decision-table rows over the ordered sources: none, each alone, every
# adjacent pair, all.
.sp_rows <- function(src) {
  c(list(character(0)), lapply(src, function(s) s),
    lapply(seq_len(length(src) - 1L), function(i) src[i:(i + 1L)]), list(src))
}

.sp_cases_a <- function() {
  out <- list()
  for (key in names(.SP_MODULES)) {
    rows <- .sp_rows(.SP_KEY_SOURCES[[key]])
    for (m in .SP_MODULES[[key]]) {
      for (r in seq_along(rows)) {
        out[[length(out) + 1L]] <- data.frame(key = key, module = m, row = r,
                                              set = paste(rows[[r]], collapse = " "))
      }
    }
  }
  do.call(rbind, out)
}

test_that("FT-DC248 [FR-DC311 FR-DC315]: decision table -- argument > section > shared > option > baseline, for each key and module", {
  cases <- .sp_cases_a()
  expect_identical(nrow(cases), 122L)
  for (i in seq_len(nrow(cases))) {
    set <- strsplit(cases$set[i], " ", fixed = TRUE)[[1L]]
    r <- .sp_run(cases$key[i], cases$module[i], set)
    expect_identical(r$got, r$want,
                     label = sprintf("%s / %s / set {%s}", cases$key[i], cases$module[i],
                                     cases$set[i]))
  }
})

test_that("FT-DC249 [FR-DC315 FR-DC318]: pairwise -- key x module x the two sources set x route; the higher source wins", {
  pairs <- utils::combn(.SP_SOURCES, 2L, FUN = paste, collapse = " ")
  keys <- c("font_size_half_points", "cell_padding_left_twips", "markup")
  cand <- do.call(rbind, lapply(keys, function(k) {
    expand.grid(key = k, module = .SP_MODULES[[k]], pair = pairs,
                route = c("document", "config"), stringsAsFactors = FALSE)
  }))
  ap <- .allpairs(cand)
  # every valid pair of levels is covered, with far fewer rows than the product
  expect_setequal(ap$covered, ap$need)
  expect_lt(nrow(ap$rows), nrow(cand) / 3)
  set <- ap$rows
  for (i in seq_len(nrow(set))) {
    src <- strsplit(set$pair[i], " ", fixed = TRUE)[[1L]]
    r <- .sp_run(set$key[i], set$module[i], src, route = set$route[i])
    expect_identical(r$got, r$want,
                     label = sprintf("%s / %s / {%s} / %s", set$key[i], set$module[i],
                                     set$pair[i], set$route[i]))
  }
})

test_that("FT-DC250 [FR-DC318 FR-DC337]: rtf_config(style = ) merges key by key over rtf_document(style = ); a section still beats a later shared key", {
  cases <- list(
    list(doc = rtf_style(font_size_half_points = 26L),
         cfg = list(rtf_style(font_size_half_points = 20L)), fs = 20L, li = 0L),
    list(doc = rtf_style(table = list(font_size_half_points = 28L)),
         cfg = list(rtf_style(table = list(font_size_half_points = 21L))), fs = 21L, li = 0L),
    list(doc = rtf_style(table = list(font_size_half_points = 28L)),
         cfg = list(rtf_style(font_size_half_points = 20L)), fs = 28L, li = 0L),
    list(doc = rtf_style(font_size_half_points = 26L),
         cfg = list(rtf_style(table = list(font_size_half_points = 21L))), fs = 21L, li = 0L),
    list(doc = rtf_style(font_size_half_points = 26L, table = list(font_size_half_points = 28L)),
         cfg = list(rtf_style(cell_padding_left_twips = 40L)), fs = 28L, li = 40L),
    list(doc = rtf_style(font_size_half_points = 26L), cfg = list(NULL), fs = 26L, li = 0L),
    list(doc = rtf_style(font_size_half_points = 26L),
         cfg = list(rtf_style(font_size_half_points = 20L), rtf_style(font_size_half_points = 24L)),
         fs = 24L, li = 0L))
  for (i in seq_along(cases)) {
    cs <- cases[[i]]
    doc <- rtf_document(style = cs$doc)
    for (s in cs$cfg) doc <- rtf_config(doc, style = s)
    doc <- doc |> rtf_tables(list(rtftable(.sp_data())))
    txt <- .render_to_string(doc)
    expect_identical(.sp_observe(txt, "font_size_half_points", "table"), cs$fs,
                     label = sprintf("case %d size", i))
    expect_identical(.sp_observe(txt, "cell_padding_left_twips", "table"), cs$li,
                     label = sprintf("case %d padding", i))
  }
})

test_that("FT-DC251 [FR-DC315 FR-DC316 FR-DC336]: a size set without a height keeps the height its size implies; a section's size too", {
  sz <- c(arg = 30L, section = 28L, shared = 26L)
  hv <- c(arg = 410L, section = 420L, shared = 430L, option = 450L)
  run <- function(m, size_src, h_src) {
    opts <- if (identical(h_src, "option")) list(rtfreporter.row_height_twips = hv[["option"]]) else list()
    old <- options(opts)
    on.exit(options(old), add = TRUE)
    own <- list()
    sec <- list()
    sh <- list()
    if (identical(size_src, "arg")) own$font_size_half_points <- sz[["arg"]]
    if (identical(size_src, "section")) sec$font_size_half_points <- sz[["section"]]
    if (identical(size_src, "shared")) sh$font_size_half_points <- sz[["shared"]]
    if (identical(h_src, "arg")) own$row_height_twips <- hv[["arg"]]
    if (identical(h_src, "section")) sec$row_height_twips <- hv[["section"]]
    if (identical(h_src, "shared")) sh$row_height_twips <- hv[["shared"]]
    sty <- sh
    if (length(sec)) sty[[m]] <- sec
    style <- if (length(sty)) do.call(rtf_style, sty) else NULL
    doc <- .sp_doc(stats::setNames(list(own), m), style)
    .sp_observe(.render_to_string(doc), "row_height_twips", m)
  }
  for (m in c("table", "header")) {
    for (s in c("arg", "section", "shared", "none")) {
      implied <- run(m, s, "none")          # the height the size implies, alone
      for (h in c(names(hv), "none")) {
        want <- if (h == "arg") {
          hv[["arg"]]
        } else if (h == "none") {
          implied
        } else if (s %in% c("arg", "section") && !(s == "section" && h == "section")) {
          implied
        } else {
          hv[[h]]
        }
        expect_identical(run(m, s, h), want,
                         label = sprintf("%s / size from %s / height from %s", m, s, h))
      }
    }
    # the size really moves the height: 30 and 28 imply heights of their own
    expect_false(identical(run(m, "arg", "none"), run(m, "none", "none")))
  }
})

test_that("FT-DC252 [FR-DC313 FR-DC314 FR-DC316 FR-DC326]: a band's default border counts as unset; a border passed is the band's own", {
  b <- rtf_border(bottom = TRUE)
  tp <- rtf_border(top = TRUE)
  hdr <- function(...) rtf_header(c(l = .sp_text("header")), ...)
  ftr <- function(...) rtf_footer(c(l = .sp_text("footer")), ...)
  cases <- list(
    list("header", list(header = hdr()), list(header = list(border = b)), "bottom"),
    list("header", list(header = hdr(border = NULL)), list(header = list(border = b)), "none"),
    list("header", list(header = hdr(border = tp)), list(header = list(border = b)), "top"),
    list("header", list(header = c(l = .sp_text("header"))), list(header = list(border = b)), "bottom"),
    list("header", list(header = hdr()), list(), "none"),
    list("footer", list(footer = ftr()), list(footer = list(border = b)), "bottom"),
    list("footer", list(footer = ftr(border = tp)), list(footer = list(border = b)), "top"),
    list("footer", list(footer = ftr(border = NULL)), list(footer = list(border = b)), "none"),
    list("footer", list(footer = ftr()), list(), "top"),
    list("footnotes", list(), list(footnotes = list(border = b)), "bottom"),
    list("footnotes", list(own = list(footnotes = list(border = tp))),
         list(footnotes = list(border = b)), "top"))
  for (i in seq_along(cases)) {
    cs <- cases[[i]]
    own <- .sp_or(cs[[2L]]$own, list())
    bands <- cs[[2L]][setdiff(names(cs[[2L]]), "own")]
    style <- if (length(cs[[3L]])) do.call(rtf_style, cs[[3L]]) else NULL
    txt <- .render_to_string(.sp_doc(own, style, bands = bands))
    expect_identical(.sp_observe(txt, "border", cs[[1L]]), cs[[4L]],
                     label = sprintf("case %d (%s)", i, cs[[1L]]))
  }
})

test_that("FT-DC253 [FR-DC312 FR-DC313 FR-DC314 FR-DC335]: a section key reaches its own module only", {
  for (key in c("font_size_half_points", "cell_padding_left_twips", "markup")) {
    base <- .sp_value(key, "baseline", "section")
    for (s in .SP_MODULES[[key]]) {
      style <- do.call(rtf_style, stats::setNames(
        list(stats::setNames(list(.sp_value(key, "section", "section")), key)), s))
      txt <- .render_to_string(.sp_doc(style = style))
      for (m in .SP_MODULES[[key]]) {
        want <- if (m == s) .sp_value(key, "section", "section") else base
        expect_identical(.sp_observe(txt, key, m), want,
                         label = sprintf("%s: section %s, module %s", key, s, m))
      }
    }
  }
})

test_that("FT-DC254 [FR-DC310 FR-DC314 FR-DC315]: the colour reaches the header and footer only; the band's own > its section > the shared colour", {
  grey <- "#7F7F7F"
  red <- "#C00000"
  blue <- "#0000FF"
  cases <- list(
    list(style = rtf_style(color = tolower(grey)), bands = list(),
         want = c(table = "none", titles = "none", footnotes = "none", header = grey, footer = grey)),
    list(style = rtf_style(color = grey, header = list(color = red)), bands = list(),
         want = c(table = "none", titles = "none", footnotes = "none", header = red, footer = grey)),
    list(style = rtf_style(color = grey, header = list(color = red)),
         bands = list(header = rtf_header(c(l = .sp_text("header")), color = blue)),
         want = c(table = "none", titles = "none", footnotes = "none", header = blue, footer = grey)),
    list(style = rtf_style(footer = list(color = red)), bands = list(),
         want = c(table = "none", titles = "none", footnotes = "none", header = "none", footer = red)),
    list(style = NULL, bands = list(),
         want = c(table = "none", titles = "none", footnotes = "none", header = "none", footer = "none")))
  for (i in seq_along(cases)) {
    cs <- cases[[i]]
    txt <- .render_to_string(.sp_doc(style = cs$style, bands = cs$bands))
    got <- vapply(names(.SP_MARK), function(m) .sp_observe(txt, "color", m), "")
    expect_identical(got, cs$want, label = sprintf("colour case %d", i))
  }
})

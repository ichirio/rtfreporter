# ============================================================================
#  A table with a DRAFT watermark and non-ASCII text, to judge in Word
# ============================================================================
#
#  The PK concentration table of data-raw/gen_pk_conc.R (same data and
#  code: that script is run as far as the document is built, with its own
#  output file left alone) on a document with rtf_document(watermark = ).  A
#  person can see on a real, full page whether the watermark is diagonal,
#  light, behind the table on every page, and the table readable over it.
#
#  The page also carries the characters a report uses beyond ASCII, for the
#  same look in Word: lambda, mu, the middle dot, the plus-minus, less-or-equal
#  and greater-or-equal signs, the degree sign, the vulgar one half, sub- and
#  superscripts next to Greek letters (the package's markup: _{ } and ^{ }),
#  and one Japanese line in a Japanese font (the east_asian font slot).  They
#  are written as \u escapes so this script stays ASCII.
#
#  Writes inst/rtf-examples/watermark-example.rtf.  Run from the package root:
#      Rscript data-raw/gen_watermark_example.R

source("data-raw/_load.R")

outfile <- "inst/rtf-examples/watermark-example.rtf"

# gen_pk_conc.R up to the line that builds the document: the data, the pages,
# pk_header and the footnotes.  Its spanning header gets lambda_z and the half-
# life in it (replaced as text, fixed: the replacement holds \u escapes that
# the parse turns into the characters).
src  <- readLines("data-raw/gen_pk_conc.R", warn = FALSE)
stop_at <- grep("^doc <- rtf_document", src)[1L]
stopifnot(!is.na(stop_at))
src <- src[seq_len(stop_at - 1L)]
old_span <- "Plasma Concentration (ng/mL)"
stopifnot(any(grepl(old_span, src, fixed = TRUE)))
src <- gsub(old_span,
            paste0(old_span, ";  \\u03bb_{z} = 0.18 h^{-1},  t\\u00bd = 3.8 h"),
            src, fixed = TRUE)
env <- new.env(parent = globalenv())
eval(parse(text = src, keep.source = FALSE), envir = env)

# the footnotes: the PK table's own, with the characters to look at
fn_chars <- c(
  paste0("C_{max} = maximum concentration (\u03bcg/mL = 1000 ng/mL); ",
         "AUC in ng\u00b7h/mL; \u03bb_{z} = terminal rate constant; ",
         "t\u00bd = half-life."),
  paste0("Mean \u00b1 SD.  BLQ = below the limit of quantitation ",
         "(\u2264 0.500 ng/mL); subjects with compliance \u2265 80%.  ",
         "Samples assayed at 25 \u00b0C."),
  paste0("\u6ce8: \u6570\u5024\u306f\u67b6\u7a7a\u306e\u30c7\u30fc\u30bf",
         "\u3067\u3059\u3002 (Japanese note)")
)
footnotes <- lapply(env$pages, function(p) fn_chars)

# the document font: Courier New for Latin text, MS Gothic for Japanese
doc <- rtf_document(
    page = rtf_page(orientation = "landscape"),
    font_table = list(list(name = c(latin = "Courier New", east_asian = "MS Gothic"))),
    font_lang = c(east_asian = "ja"),
    watermark = rtf_watermark("DRAFT")) |>
  rtf_section(page = 1, secinfo = list(header = env$pk_header))
for (p in env$pages) doc <- rtf_tables(doc, p)
doc <- rtf_footnotes(doc, footnotes)

generate_rtfreport(doc, outfile, overwrite = TRUE)
message(sprintf("%s: %d pages, %.0f KB", outfile, length(env$pages),
                file.size(outfile) / 1024))

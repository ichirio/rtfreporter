# The small documents a person opened in Word to check the fonts, built
# through the public functions only.  The fixtures under fixtures/fonts/ are
# what they write; a test rebuilds each and compares the bytes, so a change
# that alters a checked file fails.
#
#   .wck_font_doc("游明朝")        fonts/font-ja-yumincho-jpname.rtf
#   .wck_font_doc("ＭＳ 明朝") fonts/font-ja-msmincho-jpname.rtf
#       (#1047: a font family named in Japanese; checked in Word 2026-11-14)
#   .wck_slot_doc()                            fonts/font-slots-ja-latin.rtf
#       (the font slots of inst/rtf-examples/watermark-example.rtf on one
#       small page: Latin text in Courier New, Japanese text in MS Gothic)

.wck_font_data <- function() {
  data.frame(Item = c("日本語の文字", "ASCII text 0123",
                      "治験薬 10 mg"),
             Value = c("あいうえお", "ABCDE",
                       "≤ 5 µg/mL"),
             stringsAsFactors = FALSE)
}

# one family for the whole document, named as the user wrote it
.wck_font_doc <- function(font) {
  rtf_document(font_table = list(list(name = font))) |>
    rtf_tables(list(.wck_font_data()),
               titles = list(c(paste0("Font check: ", font),
                               "フォントの確認")))
}

# a Latin family and a Japanese family in their slots, the slot tagged ja
.wck_slot_doc <- function() {
  df <- data.frame(Item = c("Latin text 0123", "日本語の行"),
                   Value = c("ABCDE", "あいうえお"),
                   stringsAsFactors = FALSE)
  rtf_document(font_table = list(list(name = c(latin = "Courier New",
                                               east_asian = "MS Gothic"))),
               font_lang = c(east_asian = "ja")) |>
    rtf_tables(list(df),
               titles = list(c("Slot check: Courier New / MS Gothic",
                               "フォントの確認")))
}

.wck_write <- function(doc, dir, name) {
  f <- file.path(dir, name)
  generate_rtfreport(doc, f, overwrite = TRUE)
  f
}

# the bytes with the CRs removed, so a CRLF checkout reads as an LF one
.wck_bytes <- function(path) {
  x <- readBin(path, "raw", file.size(path))
  x[x != as.raw(13L)]
}

# ---- LibreOffice: what it reads from the font table ---------------------------

# Converts the RTF files to flat ODF (.fodt) with soffice from PATH and a
# throwaway profile; returns the .fodt texts, named by file, or skips.
.wck_lo_fodt <- function(files) {
  skip_on_cran()
  soffice <- Sys.which("soffice")
  skip_if(!nzchar(soffice), "LibreOffice absent")
  d <- tempfile("wck-lo-")
  dir.create(file.path(d, "profile"), recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  file.copy(files, d)
  prof <- normalizePath(file.path(d, "profile"), "/")
  prof <- paste0("file://", if (!startsWith(prof, "/")) "/", prof)
  # R's own LD_LIBRARY_PATH can hide LibreOffice's libraries from it
  suppressWarnings(system2(soffice, c(paste0("-env:UserInstallation=", prof),
                                      "--headless", "--convert-to", "fodt",
                                      "--outdir", shQuote(d),
                                      shQuote(file.path(d, basename(files)))),
                           stdout = FALSE, stderr = FALSE,
                           env = "LD_LIBRARY_PATH=", timeout = 180))
  out <- file.path(d, sub("[.]rtf$", ".fodt", basename(files)))
  skip_if(!all(file.exists(out)), "LibreOffice did not convert")
  stats::setNames(
    vapply(out, function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"),
                                  collapse = "\n"), ""),
    basename(files))
}

# The font family LibreOffice gives the run holding `text`: the text style of
# its <text:span>, that style's `attr` (style:font-name for Latin text,
# style:font-name-asian for East Asian text), resolved through the
# <style:font-face> declarations to the family name.  NA when any step is missing.
.wck_run_font <- function(x, text, attr = "style:font-name") {
  at <- regexpr(text, x, fixed = TRUE)
  if (at < 0L) return(NA_character_)
  before <- substr(x, 1L, at - 1L)
  sp <- regmatches(before, gregexpr("<text:span text:style-name=\"[^\"]+\"", before))[[1L]]
  if (!length(sp)) return(NA_character_)
  st <- sub(".*=\"([^\"]+)\"$", "\\1", sp[length(sp)])
  def <- regmatches(x, regexpr(paste0("<style:style style:name=\"", st,
                                      "\" style:family=\"text\">\\s*<style:text-properties[^>]*>"),
                               x, perl = TRUE))
  if (!length(def)) return(NA_character_)
  face <- regmatches(def, regexpr(paste0(" ", attr, "=\"[^\"]+\""), def))
  if (!length(face)) return(NA_character_)
  .wck_face_family(x, sub(".*=\"([^\"]+)\"$", "\\1", face))
}

# the family a <style:font-face style:name = > declares
.wck_face_family <- function(x, name) {
  ff <- regmatches(x, gregexpr("<style:font-face [^>]*>", x))[[1L]]
  nm <- sub(".* style:name=\"([^\"]+)\".*", "\\1", ff)
  fam <- sub(".* svg:font-family=\"([^\"]+)\".*", "\\1", ff)
  fam <- gsub("&apos;", "", fam, fixed = TRUE)
  fam <- gsub("&amp;", "&", fam, fixed = TRUE)
  if (!name %in% nm) return(NA_character_)
  fam[match(name, nm)]
}

# every family the document's <style:font-face> declarations name
.wck_families <- function(x) {
  ff <- regmatches(x, gregexpr("<style:font-face [^>]*>", x))[[1L]]
  fam <- sub(".* svg:font-family=\"([^\"]+)\".*", "\\1", ff)
  gsub("&amp;", "&", gsub("&apos;", "", fam, fixed = TRUE), fixed = TRUE)
}

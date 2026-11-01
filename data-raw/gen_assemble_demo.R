## ============================================================================
##  Realistic multi-page TFL deliverable demos using assemble_rtf().
##
##  Two header styles x two cover modes => four assembled outputs:
##
##    AUTO   set, no  cover  (TOC only)  <- DEFAULT real-world layout
##    AUTO   set, with cover (full)      <- showcase of all features
##    STATIC set, no  cover  (TOC only)  <- DEFAULT real-world layout
##    STATIC set, with cover (full)      <- showcase of all features
##
##  Header tokens:
##    AUTO    : {AUTO_PAGE} / {AUTO_TOTAL_PAGES}
##              -> dynamic fields; numbers RECOMPUTE across the assembled
##                 document when opened in Word / Reader.
##    STATIC  : {PAGE} / {TOTAL_PAGES}
##              -> integer literals baked in at render time; numbers
##                 reflect only the source file's own page count.
##
##  PDF conversion via headless LibreOffice is run on each assembled
##  RTF when soffice.exe is on the standard install path.
## ============================================================================

suppressMessages(suppressWarnings(devtools::load_all(quiet = TRUE)))

out_dir <- normalizePath("output/demo", mustWork = FALSE)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

## ──────────────────────────────────────────────────────────────────────────
##  Shared building blocks
## ──────────────────────────────────────────────────────────────────────────

ftr <- rtf_footer(c(c = "Source: ADaM ADSL"))

mk_demog <- function(label) {
  data.frame(
    Characteristic = c(
      label,
      "  Age, mean (SD)", "  Age, median (Q1, Q3)", "  Age, min - max",
      "  Sex, n (%)", "    Female", "    Male",
      "  Race, n (%)", "    White", "    Black or African American",
      "    Asian", "    Other",
      "  BMI, mean (SD)"
    ),
    Placebo = c("", "52.3 (12.1)", "51 (44, 60)",   "21 - 79",
                "", "16 (53.3)", "14 (46.7)",
                "", "24 (80.0)", "3 (10.0)", "2 ( 6.7)", "1 ( 3.3)",
                "27.4 (4.1)"),
    Active  = c("", "54.1 (11.7)", "53 (45, 62)",   "24 - 78",
                "", "14 (46.7)", "16 (53.3)",
                "", "23 (76.7)", "4 (13.3)", "2 ( 6.7)", "1 ( 3.3)",
                "27.8 (3.9)"),
    Total   = c("", "53.2 (11.9)", "52 (45, 61)",   "21 - 79",
                "", "30 (50.0)", "30 (50.0)",
                "", "47 (78.3)", "7 (11.7)", "4 ( 6.7)", "2 ( 3.3)",
                "27.6 (4.0)"),
    stringsAsFactors = FALSE
  )
}
mk_ae <- function(rows) {
  data.frame(
    SOC_PT    = rows$pt,
    Placebo_n = rows$pbo,
    Active_n  = rows$act,
    Total_n   = rows$tot,
    stringsAsFactors = FALSE
  )
}
mk_listing <- function(idx_start, n = 12) {
  ids <- sprintf("XYZ-%03d", seq(idx_start, idx_start + n - 1L))
  data.frame(
    USUBJID = ids,
    Status  = sample(c("Completed", "Withdrawn", "Discontinued"),
                      n, replace = TRUE, prob = c(0.7, 0.2, 0.1)),
    Reason  = sample(c("", "Adverse event", "Lost to follow-up",
                       "Withdrew consent", "Protocol deviation"),
                      n, replace = TRUE,
                      prob = c(0.6, 0.1, 0.1, 0.1, 0.1)),
    EndDate = format(seq.Date(as.Date("2026-01-15"), by = "day",
                                length.out = n), "%Y-%m-%d"),
    stringsAsFactors = FALSE
  )
}

demo_pages <- list(
  mk_demog("Overall Population"),
  mk_demog("By Age <= 65 years"),
  mk_demog("By Age >  65 years")
)
titles_demog <- list(
  c("Table 14.1.1", "Demographics and Baseline Characteristics",
    "Safety Population - All Subjects"),
  c("Table 14.1.1 (continued)", "Demographics and Baseline Characteristics",
    "Safety Population - Age <= 65"),
  c("Table 14.1.1 (continued)", "Demographics and Baseline Characteristics",
    "Safety Population - Age >  65")
)
fnotes_demog <- list(
  "Percentages based on column N.  BMI = Body Mass Index.",
  "Subjects aged 65 or younger at randomisation.",
  "Subjects aged over 65 at randomisation."
)

ae_pages <- list(
  mk_ae(list(
    pt  = c("ALL ADVERSE EVENTS",
            "  Subjects with at least one AE",
            "  Subjects with at least one Serious AE",
            "  Subjects discontinued due to AE"),
    pbo = c("",          "18 (60.0)", " 2 ( 6.7)", " 1 ( 3.3)"),
    act = c("",          "22 (73.3)", " 4 (13.3)", " 2 ( 6.7)"),
    tot = c("",          "40 (66.7)", " 6 (10.0)", " 3 ( 5.0)"))),
  mk_ae(list(
    pt  = c("Nervous system disorders",
            "  Headache", "  Dizziness", "  Insomnia"),
    pbo = c("", "10 (33.3)", " 2 ( 6.7)", " 3 (10.0)"),
    act = c("", "13 (43.3)", " 2 ( 6.7)", " 4 (13.3)"),
    tot = c("", "23 (38.3)", " 4 ( 6.7)", " 7 (11.7)"))),
  mk_ae(list(
    pt  = c("Gastrointestinal disorders",
            "  Nausea", "  Diarrhoea", "  Abdominal pain"),
    pbo = c("", " 6 (20.0)", " 4 (13.3)", " 2 ( 6.7)"),
    act = c("", " 7 (23.3)", " 5 (16.7)", " 3 (10.0)"),
    tot = c("", "13 (21.7)", " 9 (15.0)", " 5 ( 8.3)"))),
  mk_ae(list(
    pt  = c("Investigations",
            "  Blood pressure increased", "  ALT increased",
            "  Weight decreased"),
    pbo = c("", " 1 ( 3.3)", " 2 ( 6.7)", " 0       "),
    act = c("", " 2 ( 6.7)", " 3 (10.0)", " 1 ( 3.3)"),
    tot = c("", " 3 ( 5.0)", " 5 ( 8.3)", " 1 ( 1.7)")))
)
titles_ae <- list(
  c("Table 14.2.1", "Adverse Events Summary", "Safety Population"),
  c("Table 14.2.2", "Adverse Events by SOC and PT (Nervous System)",
    "Safety Population"),
  c("Table 14.2.3", "Adverse Events by SOC and PT (GI)",
    "Safety Population"),
  c("Table 14.2.4", "Adverse Events by SOC and PT (Investigations)",
    "Safety Population")
)

set.seed(42)
listing_pages <- list(mk_listing(1L), mk_listing(13L), mk_listing(25L))
titles_listing <- list(
  c("Listing 16.1", "Subject Disposition", "All Randomized - Page 1 of 3"),
  c("Listing 16.1 (continued)", "Subject Disposition",
    "All Randomized - Page 2 of 3"),
  c("Listing 16.1 (continued)", "Subject Disposition",
    "All Randomized - Page 3 of 3")
)

## ──────────────────────────────────────────────────────────────────────────
##  Build a single deliverable given a header style + filename
## ──────────────────────────────────────────────────────────────────────────
build_deliverable <- function(header, pages, titles, footnotes = NULL,
                              path) {
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = header, footer = ftr)) |>
    rtf_tables(pages, titles = titles, footnotes = footnotes)
  generate_rtfreport(doc, path, overwrite = TRUE)
  path
}

convert_to_pdf <- function(rtf) {
  soffice <- Filter(file.exists, c(
    "C:/Program Files/LibreOffice/program/soffice.exe",
    "C:/Program Files (x86)/LibreOffice/program/soffice.exe"
  ))[1L]
  if (is.na(soffice) || !length(soffice)) {
    cat("  (LibreOffice not found; PDF skipped)\n")
    return(invisible(NA_character_))
  }
  pdf_path <- sub("\\.rtf$", ".pdf", rtf)
  if (file.exists(pdf_path)) unlink(pdf_path)
  system2(soffice,
          args = c("--headless", "--convert-to",
                   "pdf:writer_pdf_Export",
                   "--outdir", shQuote(dirname(rtf)), shQuote(rtf)),
          stdout = FALSE, stderr = FALSE)
  if (file.exists(pdf_path)) {
    cat(sprintf("  -> %s  (%d KB)\n", basename(pdf_path),
                round(file.info(pdf_path)$size / 1024)))
    pdf_path
  } else {
    cat("  (PDF conversion failed)\n"); invisible(NA_character_)
  }
}

# Build the multi-level TOC for a given set of 3 input files.
build_toc_for <- function(f1, f2, f3) {
  data.frame(
    file    = c(f1, f2, f3),
    label   = c("Table 14.1.1 Demographics and Baseline Characteristics",
                "Table 14.2.1 Adverse Events Summary",
                "Listing 16.1 Subject Disposition"),
    heading = c("EFFICACY ANALYSES", "SAFETY ANALYSES", "LISTINGS"),
    level   = 2L
  )
}

# English-locale date string ("29 May 2026"), independent of Sys.getlocale().
# `format(Sys.Date(), "%B")` uses the system locale and produces e.g. "5月"
# on a Japanese Windows -- mixing locales on a cover page looks odd.
.fmt_date_en <- function(d = Sys.Date()) {
  sprintf("%02d %s %d",
          as.integer(format(d, "%d")),
          month.name[as.integer(format(d, "%m"))],   # always English
          as.integer(format(d, "%Y")))
}

# Standard cover-page spec used for the two showcase outputs.
mk_cover <- function(label) {
  list(
    title    = "Study XYZ-001",
    subtitle = sprintf("Final Statistical Report - %s Page Numbers", label),
    date     = .fmt_date_en(),
    version  = sprintf("v1.0 (%s demo)", label),
    meta     = c("Confidential - For Sponsor Use Only",
                 if (label == "AUTO")
                   "Page numbers recompute across the document"
                 else
                   "Page numbers reflect each source file only")
  )
}

report_size <- function(path) {
  cat(sprintf("RTF: %s  (%d bytes)\n", basename(path),
              as.integer(file.info(path)$size)))
}

## ──────────────────────────────────────────────────────────────────────────
##  RUN 1 — AUTO sources (dynamic page numbers)
## ──────────────────────────────────────────────────────────────────────────

cat("\n=== RUN 1: AUTO source files (dynamic page numbers) ===\n\n")

hdr_auto <- rtf_header(rows = list(
  c(l = "Protocol XYZ-001", r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}"),
  c(l = "Confidential",     r = "ACME Pharma")
))

f1_auto <- build_deliverable(hdr_auto, demo_pages, titles_demog, fnotes_demog,
                              file.path(out_dir, "auto_t14_1_1_demographics.rtf"))
f2_auto <- build_deliverable(hdr_auto, ae_pages,    titles_ae,
                              path = file.path(out_dir, "auto_t14_2_x_ae.rtf"))
f3_auto <- build_deliverable(hdr_auto, listing_pages, titles_listing,
                              path = file.path(out_dir, "auto_l16_1_disposition.rtf"))

## (1a) Default real-world layout: TOC only, NO cover
cat("[1a] AUTO, TOC only, no cover (DEFAULT layout)\n")
final_auto_default <- file.path(out_dir, "auto_assembled_default.rtf")
assemble_rtf(
  input_files = c(f1_auto, f2_auto, f3_auto),
  output_file = final_auto_default,
  toc                = build_toc_for(f1_auto, f2_auto, f3_auto),
  toc_title          = "Table of Contents",
  toc_leader         = "dot",
  toc_page_numbering = "roman",
  overwrite          = TRUE
)
report_size(final_auto_default); convert_to_pdf(final_auto_default)

## (1b) Full showcase: with cover
cat("\n[1b] AUTO, TOC + cover (full showcase)\n")
final_auto_full <- file.path(out_dir, "auto_assembled_full.rtf")
assemble_rtf(
  input_files = c(f1_auto, f2_auto, f3_auto),
  output_file = final_auto_full,
  cover              = mk_cover("AUTO"),
  toc                = build_toc_for(f1_auto, f2_auto, f3_auto),
  toc_title          = "Table of Contents",
  toc_leader         = "dot",
  toc_page_numbering = "roman",
  overwrite          = TRUE
)
report_size(final_auto_full); convert_to_pdf(final_auto_full)

## ──────────────────────────────────────────────────────────────────────────
##  RUN 2 — STATIC sources (frozen integer page numbers)
## ──────────────────────────────────────────────────────────────────────────

cat("\n=== RUN 2: STATIC source files (frozen integer page numbers) ===\n\n")

hdr_static <- rtf_header(rows = list(
  c(l = "Protocol XYZ-001", r = "Page {PAGE} of {TOTAL_PAGES}"),
  c(l = "Confidential",     r = "ACME Pharma")
))

f1_static <- build_deliverable(hdr_static, demo_pages, titles_demog, fnotes_demog,
                                file.path(out_dir, "static_t14_1_1_demographics.rtf"))
f2_static <- build_deliverable(hdr_static, ae_pages, titles_ae,
                                path = file.path(out_dir, "static_t14_2_x_ae.rtf"))
f3_static <- build_deliverable(hdr_static, listing_pages, titles_listing,
                                path = file.path(out_dir, "static_l16_1_disposition.rtf"))

## (2a) Default real-world layout: TOC only, NO cover
cat("[2a] STATIC, TOC only, no cover (DEFAULT layout)\n")
final_static_default <- file.path(out_dir, "static_assembled_default.rtf")
assemble_rtf(
  input_files = c(f1_static, f2_static, f3_static),
  output_file = final_static_default,
  toc                = build_toc_for(f1_static, f2_static, f3_static),
  toc_title          = "Table of Contents",
  toc_leader         = "dot",
  toc_page_numbering = "roman",
  overwrite          = TRUE
)
report_size(final_static_default); convert_to_pdf(final_static_default)

## (2b) Full showcase: with cover
cat("\n[2b] STATIC, TOC + cover (full showcase)\n")
final_static_full <- file.path(out_dir, "static_assembled_full.rtf")
assemble_rtf(
  input_files = c(f1_static, f2_static, f3_static),
  output_file = final_static_full,
  cover              = mk_cover("STATIC"),
  toc                = build_toc_for(f1_static, f2_static, f3_static),
  toc_title          = "Table of Contents",
  toc_leader         = "dot",
  toc_page_numbering = "roman",
  overwrite          = TRUE
)
report_size(final_static_full); convert_to_pdf(final_static_full)

cat("\n===\nFolder:", out_dir, "\n")

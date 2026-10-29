# The reference RTF files of the plan_append() tests, built BY HAND -- the
# listing and the summary each made on their own, joined row by row, paged
# with as_rtftables() -- the way these tables were written before
# plan_append() (dev-rep samples/multi-object/listing-summary-one-table).
# A plan_append() version of each table has to give the same bytes.
#
# The data are synthetic (adpc.csv, adpp.csv: 15 subjects, three renal
# function groups, two analytes).  Run from this folder:
#   Rscript make_reference.R
library(rtfreporter)

sig <- function(x, d) fmt_signif(signif(x, d), digits = d)

digits <- c(value = 3, mean = 4, sd = 5, se = 5, cv = 1, median = 4,
            min = 3, max = 3)

# A page a group: the group's listing (one row a subject) above, the
# summary of the same group below.  `d`: the records with the page
# variables `by`, the subject's `label`, the column `col`, `BLQ` and `AVAL`.
listing_summary_pages <- function(d, by, col, cols, col_label, stub_header,
                                  col_cut = NULL) {
  d$AVALC <- ifelse(d$BLQ, "BLQ", sig(d$AVAL, digits[["value"]]))
  wide <- stats::reshape(d[, c(by, "label", col, "AVALC")],
                         idvar = c(by, "label"), timevar = col,
                         direction = "wide")
  names(wide) <- sub("^AVALC[.]", "", names(wide))
  wide$part <- 1L
  wide$ord <- 0L
  ard <- cards::ard_summary(
    d[!is.na(d$AVAL), ],
    by = tidyselect::all_of(c(by, col)), variables = AVAL,
    statistic = ~ list(
      N = length,
      mean = function(x) if (length(x)) mean(x) else NA_real_,
      sd = stats::sd,
      se = function(x) stats::sd(x) / sqrt(length(x)),
      cv = function(x) if (!length(x) || mean(x) == 0) NA_real_
                       else 100 * stats::sd(x) / mean(x),
      median = stats::median,
      min = function(x) if (length(x)) min(x) else NA_real_,
      max = function(x) if (length(x)) max(x) else NA_real_))
  p <- ard |>
    normalize_ard() |>
    table_plan(cols = col, rows = c(stats::setNames(by, by), group = "variable")) |>
    plan_cells(continuous = c(
      "N" = "{N:.0f}", "Mean" = sprintf("{mean:.%ds}", digits[["mean"]]),
      "SD" = sprintf("{sd:.%ds}", digits[["sd"]]),
      "SE" = sprintf("{se:.%ds}", digits[["se"]]),
      "CV(%)" = sprintf("{cv:.%df}", digits[["cv"]]),
      "Median" = sprintf("{median:.%ds}", digits[["median"]]),
      "Min" = sprintf("{min:.%ds}", digits[["min"]]),
      "Max" = sprintf("{max:.%ds}", digits[["max"]])), notes = FALSE)
  p <- do.call(plan_levels, c(list(p), stats::setNames(list(cols), col)))
  stats_tab <- as.data.frame(plan_apply(p, "table"))
  stats_tab$label <- paste0(strrep(" ", 4L), stats_tab$label)   # indented, as a stub is
  stats_tab$part <- 3L
  stats_tab$ord <- seq_len(nrow(stats_tab))
  heads <- unique(stats_tab[by])
  heads$label <- "Summary Statistics"
  heads[cols] <- ""
  heads$part <- 2L
  heads$ord <- 0L
  keep <- c(by, "part", "ord", "label", cols)
  body <- rbind(wide[keep], heads[keep], stats_tab[keep])
  body[cols] <- lapply(body[cols], function(v) ifelse(is.na(v), "", v))
  key <- lapply(by, function(b) match(body[[b]], levels(factor(d[[b]]))))
  body <- body[do.call(order, c(key, list(body$part, body$ord, body$label))), ]
  pages <- as_rtftables(
    body, page_by = by, drop_cols = c(by, "part", "ord"),
    blank_row_first = TRUE, blank_row_end = TRUE,
    blank_rows = blank_rows_by_rule("label", "^Summary Statistics$", where = "before"),
    col_header = rtf_col_header(
      list(col_cell(1, ""), col_cell(c(2, length(cols) + 1L), col_label)),
      c(stub_header, cols)),
    col_rel_width = c(2.4, rep(1.2, length(cols))),
    border = "tfl")
  if (!is.null(col_cut)) {
    pages <- paginate_cols(pages, at = match(col_cut, cols) + 1L,
                           order = c("group", "rows", "cols"))
  }
  lapply(pages, function(pg) {
    pg |>
      set_decimal_split(cols = seq_len(ncol(pg$data))[-1]) |>
      style_zone(last_row = rtf_border(bottom = TRUE))
  })
}

# the document: no run date, so the bytes are the same run after run
write_report <- function(pages, titles, file, program) {
  doc <- rtf_document(tokens = list(STUDY = "DEMO-02"), program = program) |>
    rtf_section(
      header = rtf_header(list(c(l = "Protocol: {STUDY} (synthetic data)",
                                 r = "Page {PAGE} of {TOTAL_PAGES}"))),
      footer = rtf_footer(list(c(l = "Synthetic data made for this sample.",
                                 r = "Program: {PROGRAM}")))) |>
    rtf_tables(pages, titles = titles)
  generate_rtfreport(doc, file, overwrite = TRUE)
}

records <- function(d) {
  d$label <- sprintf("%s/%d/%s", d$USUBJID, d$AGE, d$SEX)
  d$BLQ <- d$BLQFL %in% "Y"
  d
}

# ---- 1. concentrations, the time points across (two column blocks) ----------
adpc <- records(read.csv("adpc.csv"))
adpc <- adpc[adpc$RENALGR %in% c("Normal", "Moderate"), ]
adpc$RENALGR <- factor(adpc$RENALGR, levels = c("Normal", "Moderate"))
adpc$ANALYTE <- factor(adpc$ANALYTE, levels = c("Parent", "Metabolite M1"))
times <- c("Pre-Dose", paste(c(0.25, 0.5, 1, 1.5, 2, 3, 4, 6, 8, 12, 24, 36, 48), "hr"))
adpc$ATPT <- factor(adpc$ATPT, levels = times)
write_report(
  listing_summary_pages(adpc, by = c("ANALYTE", "RENALGR"), col = "ATPT", cols = times,
                        col_label = "Time (hr) Relative to Dose",
                        stub_header = "Subject ID/Age (years)/Gender", col_cut = "4 hr"),
  titles = list(list("Table 14.4.3", "Drug X Plasma Concentrations (ng/mL) by Subject",
                     "PK Population",
                     list(text = "Analyte: {ANALYTE}", align = "left"),
                     list(text = "Renal Function Group: {RENALGR}", align = "left"))),
  file = "ref_conc_cols_time.rtf", program = "plan-append/conc_cols_time.R")

# ---- 2. the PK parameters across ------------------------------------------------
adpp <- read.csv("adpp.csv")
adpp$label <- sprintf("%s/%d/%s", adpp$USUBJID, adpp$AGE, adpp$SEX)
adpp$BLQ <- FALSE
adpp$RENALGR <- factor(adpp$RENALGR, levels = c("Normal", "Mild", "Moderate"))
adpp$ANALYTE <- factor(adpp$ANALYTE, levels = c("Parent", "Metabolite M1"))
params <- c("C_{max} (ng/mL)", "T_{max} (hr)", "AUC_{last} (ng·hr/mL)",
            "AUC_{inf} (ng·hr/mL)", "t1/2 (hr)")
adpp$PARAM <- factor(adpp$PARAM, levels = params)
write_report(
  listing_summary_pages(adpp, by = c("ANALYTE", "RENALGR"), col = "PARAM", cols = params,
                        col_label = "Parameter",
                        stub_header = "Subject ID/Age (years)/Gender"),
  titles = list(list("Table 14.4.1", "Drug X Pharmacokinetic Parameters by Subject",
                     "PK Population",
                     list(text = "Analyte: {ANALYTE}", align = "left"),
                     list(text = "Renal Function Group: {RENALGR}", align = "left"))),
  file = "ref_param_by_group.rtf", program = "plan-append/param_by_group.R")

# ---- 3. the analytes across, a page a group and time point ---------------------
adpc2 <- records(read.csv("adpc.csv"))
adpc2 <- adpc2[adpc2$ATPT %in% c("1 hr", "12 hr"), ]
adpc2$RENALGR <- factor(adpc2$RENALGR, levels = c("Normal", "Mild", "Moderate"))
adpc2$ATPT <- factor(adpc2$ATPT, levels = c("1 hr", "12 hr"))
adpc2$ANALYTE <- factor(adpc2$ANALYTE, levels = c("Parent", "Metabolite M1"))
write_report(
  listing_summary_pages(adpc2, by = c("RENALGR", "ATPT"), col = "ANALYTE",
                        cols = c("Parent", "Metabolite M1"), col_label = "Analyte",
                        stub_header = "Subject ID/Age (years)/Gender"),
  titles = list(list("Table 14.4.2", "Drug X Plasma Concentrations (ng/mL) by Subject",
                     "PK Population",
                     list(text = "Time: {ATPT}", align = "left"),
                     list(text = "Renal Function Group: {RENALGR}", align = "left"))),
  file = "ref_conc_cols_analyte.rtf", program = "plan-append/conc_cols_analyte.R")

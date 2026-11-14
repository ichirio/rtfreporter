# helper-toc-books.R -- the two table-of-contents books a person checked in
# Word (#1056, #1068), built through the public functions only.  This is the
# one copy of the code: test-assemble-toc-books.R rebuilds the books and
# compares their bytes with the fixtures, and data-raw/gen_toc_word_check.R
# sources this file to write them for a person to open.
#
#   .tb_build_books(dir)   writes dir/inputs/*.rtf (36 reports) and
#                          dir/toc-book-default.rtf, dir/toc-book-hanging.rtf;
#                          returns the books' paths, named default and hanging
#
# About 40 real reports -- one-page and multi-page tables, figures, short
# listings and one long listing -- assembled into two books with a cover and
# a table of contents of more than one page:
#
#   toc-book-default.rtf   the default TOC style: every heading and entry,
#                          levels 2 and 3 alike, at the same left position
#                          (fixtures/toc-word-check-default.rtf)
#   toc-book-hanging.rtf   toc_style = list(entry = list(hanging = TRUE))
#                          (fixtures/toc-word-check-hanging.rtf)
#
# The book runs past 100 pages, so the TOC shows page numbers of 1, 2 and 3
# digits; several titles are long enough to wrap in the TOC.  Every report
# page shows two page numbers:
#   - header, right: the report's own page, "Page {PAGE} of {TOTAL_PAGES}",
#     written when the report is made (static), so it stays the report's
#     number in the book ("Page 3 of 50" on the third page of the listing);
#   - footer, centre: the book's page, a {BOOK_PAGE} slot in its own footer
#     row under the "Source: ..." row, filled by assemble_rtf(book_page =
#     "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}") ("Page 74 of 121").
# The cover and the TOC pages have no page number.  The data are fictional
# (set.seed(1056); the caller's random stream is restored on exit).  The
# figures are ggplot2 plots when ggplot2 is installed, else base graphics;
# either way they are PNGs of 960 x 504 pixels, whose bytes depend on the
# device and its version.

.tb_build_books <- function(dir) {
  in_dir <- file.path(dir, "inputs")
  dir.create(in_dir, recursive = TRUE, showWarnings = FALSE)

  seed <- ".Random.seed"
  had_seed <- exists(seed, envir = globalenv(), inherits = FALSE)
  if (had_seed) old_seed <- get(seed, envir = globalenv())
  on.exit({
    if (had_seed) {
      assign(seed, old_seed, envir = globalenv())
    } else if (exists(seed, envir = globalenv(), inherits = FALSE)) {
      rm(list = seed, envir = globalenv())
    }
  }, add = TRUE)

  set.seed(1056)
  arms <- c("Placebo (N=120)", "Drug 10 mg (N=118)", "Drug 20 mg (N=121)")
  n_arm <- c(120L, 118L, 121L)

  # -- the page set-up every report shares ------------------------------------
  # (the header's page is the report's own; the footer's {BOOK_PAGE} row is
  # empty in a report and gets the book's page when the book is assembled.
  # The "Source:" text names the generator the books were first written by;
  # it is part of the checked bytes.)
  hdr <- rtf_header(rows = list(
    c(l = "Example Pharma", r = "Page {PAGE} of {TOTAL_PAGES}"),
    c(l = "Study EX-2026-01 (fictional data)", r = "Final, 13NOV2026")
  ))
  ftr <- rtf_footer(rows = list(
    c(l = "Source: data-raw/gen_toc_word_check.R (fictional data)"),
    c(c = "{BOOK_PAGE}")
  ))

  write_report <- function(name, fill, ...) {
    path <- file.path(in_dir, paste0(name, ".rtf"))
    doc <- rtf_document() |>
      rtf_section(page = 1, secinfo = list(header = hdr, footer = ftr)) |>
      fill(...)
    generate_rtfreport(doc, path, overwrite = TRUE)
    path
  }

  # -- fictional content --------------------------------------------------------
  pct <- function(n, N) sprintf("%3d (%4.1f)", n, 100 * n / N)

  # A count table: `labels` rows (an indented row when it starts with spaces),
  # cut into pages of `per_page` rows
  count_pages <- function(labels, per_page = 16L) {
    rows <- lapply(labels, function(l) {
      if (!grepl("^ ", l)) return(c(l, "", "", ""))
      k <- vapply(n_arm, function(N) as.integer(stats::rbinom(1L, N, stats::runif(1L, 0.02, 0.4))),
                  integer(1L))
      c(l, pct(k[1L], n_arm[1L]), pct(k[2L], n_arm[2L]), pct(k[3L], n_arm[3L]))
    })
    m <- do.call(rbind, rows)
    df <- data.frame(m, stringsAsFactors = FALSE)
    names(df) <- c("Parameter", arms)
    split(df, ceiling(seq_len(nrow(df)) / per_page))
  }

  # A summary table of a measure by visit: n, mean (SD), median, min - max
  visit_pages <- function(params, visits, per_page = 15L) {
    rows <- list()
    for (p in params) {
      for (v in visits) {
        mu <- stats::runif(1L, 5, 150)
        rows[[length(rows) + 1L]] <- c(paste0(p, " - ", v), "", "", "")
        for (s in c("n", "Mean (SD)", "Median", "Min - Max")) {
          cells <- vapply(seq_along(n_arm), function(j) {
            x <- stats::rnorm(40L, mu, mu / 6)
            switch(s,
                   n = as.character(n_arm[j] - sample(0:9, 1L)),
                   "Mean (SD)" = sprintf("%.1f (%.2f)", mean(x), stats::sd(x)),
                   Median = sprintf("%.1f", stats::median(x)),
                   sprintf("%.1f - %.1f", min(x), max(x)))
          }, character(1L))
          rows[[length(rows) + 1L]] <- c(paste0("  ", s), cells)
        }
      }
    }
    df <- data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    names(df) <- c("Parameter / Visit", arms)
    split(df, ceiling(seq_len(nrow(df)) / per_page))
  }

  socs <- c("Gastrointestinal disorders", "Nervous system disorders",
            "Infections and infestations", "Skin and subcutaneous tissue disorders",
            "Musculoskeletal and connective tissue disorders", "Investigations",
            "General disorders and administration site conditions",
            "Respiratory, thoracic and mediastinal disorders", "Psychiatric disorders",
            "Metabolism and nutrition disorders", "Vascular disorders", "Cardiac disorders")
  pts <- list(
    c("Nausea", "Diarrhoea", "Vomiting", "Constipation", "Abdominal pain", "Dyspepsia"),
    c("Headache", "Dizziness", "Somnolence", "Paraesthesia", "Dysgeusia"),
    c("Nasopharyngitis", "Upper respiratory tract infection", "Urinary tract infection",
      "Influenza", "Bronchitis"),
    c("Rash", "Pruritus", "Dry skin", "Erythema"),
    c("Back pain", "Arthralgia", "Myalgia", "Muscle spasms", "Pain in extremity"),
    c("Alanine aminotransferase increased", "Weight decreased", "Blood creatine phosphokinase increased"),
    c("Fatigue", "Pyrexia", "Oedema peripheral", "Injection site reaction"),
    c("Cough", "Oropharyngeal pain", "Dyspnoea"),
    c("Insomnia", "Anxiety", "Depressed mood"),
    c("Decreased appetite", "Hyperglycaemia", "Hypokalaemia"),
    c("Hypertension", "Hot flush"),
    c("Palpitations", "Tachycardia"))
  soc_pt <- unlist(lapply(seq_along(socs), function(i) c(socs[i], paste0("  ", pts[[i]]))))

  # A listing of `n` adverse-event records, `per_page` a page
  ae_listing <- function(n, per_page = 15L) {
    soc_i <- sample(seq_along(socs), n, replace = TRUE)
    df <- data.frame(
      `Subject` = sprintf("EX-%03d-%04d", sample(101:118, n, TRUE), sample(1:400, n, TRUE)),
      `Arm` = sample(c("Placebo", "Drug 10 mg", "Drug 20 mg"), n, TRUE),
      `System organ class` = socs[soc_i],
      `Preferred term` = vapply(soc_i, function(i) sample(pts[[i]], 1L), character(1L)),
      `Start day` = sample(1:180, n, TRUE),
      `Severity` = sample(c("Mild", "Moderate", "Severe"), n, TRUE, prob = c(0.6, 0.3, 0.1)),
      `Serious` = sample(c("N", "Y"), n, TRUE, prob = c(0.93, 0.07)),
      `Related` = sample(c("Not related", "Possibly", "Probably"), n, TRUE),
      check.names = FALSE, stringsAsFactors = FALSE)
    df <- df[order(df$Subject, df$`Start day`), ]
    rownames(df) <- NULL
    split(df, ceiling(seq_len(nrow(df)) / per_page))
  }

  # A short listing of subjects with a reason
  subject_listing <- function(n, reasons, per_page = 18L) {
    df <- data.frame(
      Subject = sprintf("EX-%03d-%04d", sample(101:118, n, TRUE), sample(1:400, n, TRUE)),
      Arm = sample(c("Placebo", "Drug 10 mg", "Drug 20 mg"), n, TRUE),
      Age = sample(19:79, n, TRUE),
      Sex = sample(c("F", "M"), n, TRUE),
      Day = sample(1:180, n, TRUE),
      Reason = sample(reasons, n, TRUE),
      stringsAsFactors = FALSE)
    df <- df[order(df$Subject), ]
    rownames(df) <- NULL
    split(df, ceiling(seq_len(nrow(df)) / per_page))
  }

  table_report <- function(name, number, title, pop, pages, foot = NULL) {
    write_report(name, rtf_tables, pages,
                 titles = list(c(number, title, pop)),
                 footnotes = list(c(if (is.null(foot)) "Percentages are based on the number of subjects in each arm." else foot,
                                    "Fictional data, for a table of contents check.")))
  }

  figure_report <- function(name, number, title, pop, plot) {
    write_report(name, rtf_figures, list(if (is.character(plot)) rtfplot(plot) else plot),
                 titles = list(c(number, title, pop)),
                 footnotes = list("Fictional data, for a table of contents check."),
                 render_width = 8, render_height = 4.2, render_dpi = 120)
  }

  # -- figures: ggplot2 when it is installed, else base graphics ---------------
  weeks <- c(0, 2, 4, 8, 12, 16, 24)
  fig_data <- data.frame(
    week = rep(weeks, 3L),
    arm  = rep(c("Placebo", "Drug 10 mg", "Drug 20 mg"), each = length(weeks)),
    est  = c(-1.5 * (1 - exp(-weeks / 6)), -8 * (1 - exp(-weeks / 7)), -11 * (1 - exp(-weeks / 7))))
  fig_data$est <- fig_data$est + stats::rnorm(nrow(fig_data), 0, 0.3)
  fig_data$se  <- stats::runif(nrow(fig_data), 0.8, 1.6)

  km_data <- do.call(rbind, lapply(c("Placebo", "Drug 10 mg", "Drug 20 mg"), function(a) {
    rate <- switch(a, Placebo = 1 / 30, "Drug 10 mg" = 1 / 45, 1 / 60)
    t <- sort(stats::rexp(110L, rate))
    data.frame(arm = a, time = c(0, t), surv = c(1, 1 - seq_along(t) / 120))
  }))

  lab_data <- data.frame(
    visit = factor(rep(rep(c("Baseline", "Week 4", "Week 12", "Week 24"), each = 60L), 3L),
                   levels = c("Baseline", "Week 4", "Week 12", "Week 24")),
    arm = rep(c("Placebo", "Drug 10 mg", "Drug 20 mg"), each = 240L),
    alt = stats::rlnorm(720L, log(24), 0.35))

  # (the base-graphics calls are not written graphics::, which DESCRIPTION
  # does not list; graphics is attached in every default R session)
  have_gg <- requireNamespace("ggplot2", quietly = TRUE)
  plot_or_file <- function(gg, base) {
    if (have_gg) return(gg)
    f <- tempfile(fileext = ".png")
    grDevices::png(f, width = 960, height = 504, res = 120)
    base()
    grDevices::dev.off()
    f
  }
  fig_change <- plot_or_file(
    if (have_gg) {
      ggplot2::ggplot(fig_data, ggplot2::aes(week, est, linetype = arm, shape = arm)) +
        ggplot2::geom_line() + ggplot2::geom_point(size = 2) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = est - se, ymax = est + se), width = 0.6) +
        ggplot2::labs(x = "Week", y = "Mean change from baseline (mmHg)", linetype = NULL, shape = NULL) +
        ggplot2::theme_bw()
    },
    function() matplot(weeks, matrix(fig_data$est, ncol = 3L), type = "b",
                       xlab = "Week", ylab = "Mean change from baseline (mmHg)"))
  fig_km <- plot_or_file(
    if (have_gg) {
      ggplot2::ggplot(km_data, ggplot2::aes(time, surv, linetype = arm)) +
        ggplot2::geom_step() +
        ggplot2::labs(x = "Days from randomisation", y = "Proportion without event", linetype = NULL) +
        ggplot2::theme_bw()
    },
    function() plot(km_data$time, km_data$surv, type = "s",
                    xlab = "Days from randomisation", ylab = "Proportion without event"))
  fig_lab <- plot_or_file(
    if (have_gg) {
      ggplot2::ggplot(lab_data, ggplot2::aes(visit, alt, fill = arm)) +
        ggplot2::geom_boxplot(outlier.size = 0.6) +
        ggplot2::scale_fill_grey(start = 0.5, end = 0.95) +
        ggplot2::labs(x = NULL, y = "ALT (U/L)", fill = NULL) +
        ggplot2::theme_bw()
    },
    function() boxplot(alt ~ visit, data = lab_data, ylab = "ALT (U/L)"))

  # -- the reports ----------------------------------------------------------------
  saf <- "Safety Population"
  fas <- "Full Analysis Set"
  enr <- "All Enrolled Subjects"
  long_ae <- paste("Treatment-Emergent Adverse Events by System Organ Class, Preferred Term",
                   "and Maximum Severity, Sorted by Descending Frequency in the Total Column")

  R <- list()
  add <- function(file, heading, level = 2L) {
    R[[length(R) + 1L]] <<- data.frame(file = file, heading = heading, level = level)
  }

  # DEMOGRAPHIC
  h <- "DEMOGRAPHIC"
  add(table_report("t_14_1_1", "Table 14.1.1", "Subject Disposition", enr,
                   count_pages(c("Subjects", "  Randomised", "  Treated", "  Completed the study",
                                 "  Discontinued the study", "Reason for discontinuation",
                                 "  Adverse event", "  Lack of efficacy", "  Withdrawal by subject",
                                 "  Lost to follow-up", "  Physician decision", "  Other"))), h)
  add(table_report("t_14_1_2", "Table 14.1.2", "Major Protocol Deviations", enr,
                   count_pages(c("Any major deviation", "  Eligibility criteria not met",
                                 "  Prohibited medication", "  Study drug non-compliance",
                                 "  Visit out of window", "  Informed consent"))), h)
  add(table_report("t_14_1_3", "Table 14.1.3", "Demographic and Baseline Characteristics", saf,
                   count_pages(c("Sex", "  Female", "  Male", "Age group", "  < 40 years",
                                 "  40 - 64 years", "  >= 65 years", "Race", "  White",
                                 "  Black or African American", "  Asian", "  American Indian or Alaska Native",
                                 "  Multiple", "Ethnicity", "  Hispanic or Latino",
                                 "  Not Hispanic or Latino", "Region", "  North America", "  Europe",
                                 "  Asia Pacific", "  Latin America", "BMI group", "  < 25 kg/m2",
                                 "  25 - < 30 kg/m2", "  >= 30 kg/m2", "Smoking", "  Never",
                                 "  Former", "  Current", "Baseline disease severity", "  Mild",
                                 "  Moderate", "  Severe", "Prior therapy", "  None", "  One line",
                                 "  Two or more lines"))), h)
  add(table_report("t_14_1_4", "Table 14.1.4",
                   "Medical History by System Organ Class and Preferred Term, Including Conditions Ongoing at Screening and Resolved Before the First Dose of Study Drug",
                   saf, count_pages(soc_pt)), h)
  add(table_report("t_14_1_5", "Table 14.1.5", "Prior and Concomitant Medications by ATC Class and Preferred Name", saf,
                   count_pages(unlist(lapply(c("Analgesics", "Antibacterials for systemic use",
                                               "Antihistamines", "Lipid modifying agents",
                                               "Agents acting on the renin-angiotensin system",
                                               "Drugs for acid related disorders",
                                               "Psycholeptics", "Corticosteroids for systemic use",
                                               "Vitamins", "Antithrombotic agents"),
                                             function(a) c(a, paste0("  ", a, " ", 1:6)))))), h)
  add(table_report("t_14_1_6", "Table 14.1.6", "Extent of Exposure to Study Drug", saf,
                   count_pages(c("Duration of exposure", "  < 4 weeks", "  4 - < 12 weeks",
                                 "  12 - < 24 weeks", "  >= 24 weeks", "Dose interruptions",
                                 "  Any", "  Due to an adverse event"))), h)

  # EFFICACY
  h <- "EFFICACY"
  add(table_report("t_14_2_1", "Table 14.2.1",
                   "Change from Baseline in Systolic Blood Pressure at Week 24: Analysis of Covariance with Treatment, Region and Baseline Value as Covariates (Primary Endpoint)",
                   fas, count_pages(c("Week 24", "  Responders", "  Non-responders", "  Missing"))), h)
  add(table_report("t_14_2_1_1", "Table 14.2.1.1", "Primary Endpoint by Age Group", fas,
                   count_pages(c("< 65 years", "  Responders", "  Non-responders", ">= 65 years",
                                 "  Responders", "  Non-responders"))), h, 3L)
  add(table_report("t_14_2_1_2", "Table 14.2.1.2", "Primary Endpoint by Sex", fas,
                   count_pages(c("Female", "  Responders", "  Non-responders", "Male",
                                 "  Responders", "  Non-responders"))), h, 3L)
  add(table_report("t_14_2_1_3", "Table 14.2.1.3",
                   "Primary Endpoint by Geographic Region (North America, Europe, Asia Pacific and Latin America) and by Baseline Disease Severity",
                   fas, count_pages(c("North America", "  Responders", "Europe", "  Responders",
                                      "Asia Pacific", "  Responders", "Latin America", "  Responders"))), h, 3L)
  add(table_report("t_14_2_2", "Table 14.2.2", "Change from Baseline in Diastolic Blood Pressure at Week 24", fas,
                   count_pages(c("Week 24", "  Responders", "  Non-responders"))), h)
  add(table_report("t_14_2_3", "Table 14.2.3", "Blood Pressure Control (< 140/90 mmHg) at Each Visit", fas,
                   count_pages(c("Week 4", "  Controlled", "Week 12", "  Controlled", "Week 24",
                                 "  Controlled"))), h)
  add(table_report("t_14_2_4", "Table 14.2.4", "Time to First Rescue Medication: Kaplan-Meier Estimates", fas,
                   count_pages(c("Subjects with an event", "  Yes", "  No (censored)",
                                 "Kaplan-Meier estimate", "  Day 30", "  Day 90", "  Day 180"))), h)
  add(figure_report("f_14_2_5", "Figure 14.2.5", "Kaplan-Meier Plot of Time to First Rescue Medication", fas, fig_km), h)
  add(figure_report("f_14_2_6", "Figure 14.2.6", "Mean (SE) Change from Baseline in Systolic Blood Pressure by Visit", fas, fig_change), h)
  add(table_report("t_14_2_7", "Table 14.2.7", "Blood Pressure by Visit: Summary Statistics", fas,
                   visit_pages(c("Systolic BP (mmHg)", "Diastolic BP (mmHg)"),
                               c("Baseline", "Week 4", "Week 12", "Week 24"))), h)
  add(table_report("t_14_2_8", "Table 14.2.8", "Sensitivity Analysis of the Primary Endpoint: Per-Protocol Set", "Per-Protocol Set",
                   count_pages(c("Week 24", "  Responders", "  Non-responders"))), h)

  # SAFETY
  h <- "SAFETY"
  add(table_report("t_14_3_1", "Table 14.3.1", "Overview of Treatment-Emergent Adverse Events", saf,
                   count_pages(c("Subjects with", "  Any TEAE", "  Any related TEAE", "  Any serious TEAE",
                                 "  Any severe TEAE", "  TEAE leading to discontinuation",
                                 "  TEAE leading to death"))), h)
  add(table_report("t_14_3_2", "Table 14.3.2", "Treatment-Emergent Adverse Events by System Organ Class and Preferred Term", saf,
                   count_pages(soc_pt)), h)
  add(table_report("t_14_3_3", "Table 14.3.3", long_ae, saf,
                   count_pages(c(soc_pt, paste0(soc_pt, " ")))), h)
  add(table_report("t_14_3_4", "Table 14.3.4", "Treatment-Related Treatment-Emergent Adverse Events by System Organ Class and Preferred Term", saf,
                   count_pages(soc_pt[1:40])), h)
  add(table_report("t_14_3_5", "Table 14.3.5", "Serious Treatment-Emergent Adverse Events", saf,
                   count_pages(soc_pt[1:12])), h)
  add(table_report("t_14_3_6", "Table 14.3.6", "Treatment-Emergent Adverse Events Leading to Discontinuation of Study Drug", saf,
                   count_pages(soc_pt[13:26])), h)
  add(table_report("t_14_3_7", "Table 14.3.7", "Haematology: Summary of Observed Values and Change from Baseline by Visit, Including Unscheduled Visits Mapped to the Nearest Scheduled Visit", saf,
                   visit_pages(c("Haemoglobin (g/L)", "Platelets (10^9/L)", "Leukocytes (10^9/L)"),
                               c("Baseline", "Week 12", "Week 24"))), h)
  add(table_report("t_14_3_8", "Table 14.3.8", "Clinical Chemistry: Summary of Observed Values and Change from Baseline by Visit, Including Unscheduled Visits Mapped to the Nearest Scheduled Visit", saf,
                   visit_pages(c("ALT (U/L)", "AST (U/L)", "Creatinine (umol/L)", "Glucose (mmol/L)"),
                               c("Baseline", "Week 12", "Week 24"))), h)
  add(table_report("t_14_3_9", "Table 14.3.9", "Shift from Baseline to Worst Post-Baseline Grade in Laboratory Parameters", saf,
                   count_pages(unlist(lapply(c("ALT", "AST", "Bilirubin", "Creatinine", "Potassium", "Sodium"),
                                             function(p) c(p, paste0("  Grade ", 0:3)))))), h)
  add(table_report("t_14_3_10", "Table 14.3.10", "Vital Signs: Summary of Values and Change from Baseline by Visit", saf,
                   visit_pages(c("Pulse rate (beats/min)", "Body temperature (C)", "Weight (kg)"),
                               c("Baseline", "Week 12", "Week 24"))), h)
  add(table_report("t_14_3_11", "Table 14.3.11", "12-Lead ECG: QTcF Categories", saf,
                   count_pages(c("Maximum QTcF", "  <= 450 ms", "  > 450 - 480 ms", "  > 480 - 500 ms",
                                 "  > 500 ms", "Maximum change", "  <= 30 ms", "  > 30 - 60 ms",
                                 "  > 60 ms", "Any visit", "  Abnormal, clinically significant",
                                 "  Abnormal, not clinically significant", "  Normal",
                                 "Last visit", "  Abnormal, clinically significant",
                                 "  Abnormal, not clinically significant", "  Normal",
                                 "  Not done"))), h)
  add(figure_report("f_14_3_12", "Figure 14.3.12", "Box Plot of Alanine Aminotransferase by Visit", saf, fig_lab), h)
  add(table_report("t_14_3_13", "Table 14.3.13",
                   "Subjects with Potentially Clinically Significant Laboratory Values at Any Post-Baseline Visit, by Parameter and Criterion",
                   saf, count_pages(c("ALT > 3 x ULN", "  Any visit", "AST > 3 x ULN", "  Any visit",
                                      "Bilirubin > 2 x ULN", "  Any visit"))), h)

  # LISTINGS
  h <- "LISTINGS"
  add(write_report("l_16_2_1", rtf_tables, subject_listing(14L, c("Adverse event", "Withdrawal by subject", "Lost to follow-up")),
                   titles = list(c("Listing 16.2.1", "Subjects Who Discontinued the Study", enr))), h)
  add(write_report("l_16_2_2", rtf_tables, subject_listing(30L, c("Visit out of window", "Prohibited medication", "Non-compliance")),
                   titles = list(c("Listing 16.2.2", "Major Protocol Deviations", enr))), h)
  add(write_report("l_16_2_3", rtf_tables, subject_listing(9L, c("No post-baseline efficacy data", "Not treated")),
                   titles = list(c("Listing 16.2.3", "Subjects Excluded from the Analysis Sets", enr))), h)
  add(write_report("l_16_2_7", rtf_tables, ae_listing(15L * 50L),
                   titles = list(c("Listing 16.2.7", "All Adverse Events, by Subject and Start Day, Including Events Before the First Dose of Study Drug and Events After the Last Dose", saf))), h)
  add(write_report("l_16_2_8", rtf_tables, subject_listing(11L, c("Hospitalisation", "Medically important event")),
                   titles = list(c("Listing 16.2.8", "Serious Adverse Events", saf))), h)
  add(write_report("l_16_2_9", rtf_tables, subject_listing(2L, c("Myocardial infarction", "Road traffic accident")),
                   titles = list(c("Listing 16.2.9", "Deaths", saf))), h)

  toc <- do.call(rbind, R)

  cover <- list(title = "Study EX-2026-01", subtitle = "Tables, Figures and Listings",
                date = "13 November 2026", version = "Table of contents check book (fictional data)",
                meta = c("Example Pharma", "Confidential"))

  books <- c(default = "toc-book-default.rtf", hanging = "toc-book-hanging.rtf")
  out <- vapply(names(books), function(b) {
    f <- file.path(dir, books[[b]])
    assemble_rtf(toc = toc, output_file = f, cover = cover, overwrite = TRUE,
                 book_page = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}",
                 toc_style = if (b == "hanging") list(entry = list(hanging = TRUE)))
    f
  }, "")
  out
}

# The bytes of an RTF with the CRs removed (a CRLF checkout reads as an LF
# one) and every picture's data -- the hex between a \pict header line and
# its closing brace -- replaced by "<picture>", so a PNG that another device
# or device version draws differently still compares equal.
.tb_bytes <- function(path) {
  x <- readBin(path, "raw", file.size(path))
  x <- rawToChar(x[x != as.raw(13L)])
  charToRaw(gsub("(\\\\pict[^\n]*\n)[0-9A-Fa-f\n]+\\}", "\\1<picture>}", x,
                 useBytes = TRUE))
}

# Each picture of an RTF: its header (\pict\pngblip\picw..\pich..\picwgoal..
# \pichgoal..), the format and the width and height in pixels its PNG's
# IHDR chunk gives, and the number of bytes of the PNG
.tb_pictures <- function(path) {
  x <- readBin(path, "raw", file.size(path))
  x <- rawToChar(x[x != as.raw(13L)])
  m <- regmatches(x, gregexpr("\\\\pict[^\n]*\n[0-9A-Fa-f\n]+\\}", x, useBytes = TRUE))[[1L]]
  do.call(rbind, lapply(m, function(p) {
    hdr <- sub("\n.*$", "", p)
    hex <- gsub("[^0-9A-Fa-f]", "", sub("^[^\n]*\n", "", p))
    ihdr <- regmatches(hex, regexpr("49484452[0-9A-Fa-f]{16}", hex))
    data.frame(header = hdr,
               png = startsWith(toupper(hex), "89504E470D0A1A0A"),
               width = strtoi(substr(ihdr, 9L, 16L), 16L),
               height = strtoi(substr(ihdr, 17L, 24L), 16L),
               bytes = nchar(hex) / 2L,
               stringsAsFactors = FALSE)
  }))
}

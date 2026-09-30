## ---------------------------------------------------------------------------
##  Screenshots for the plan articles (vignettes/articles/tables-from-ard.Rmd,
##  plan-verbs.Rmd, plan-listings.Rmd, plan-and-as-rtftables.Rmd).
##
##  Each article writes its RTF into the folder named by the environment
##  variable RTFREPORTER_ARTICLE_OUT (a scratch folder when it is unset).  This
##  script renders the articles with that folder set, then has LibreOffice
##  turn the first page of every RTF into a PNG under
##  inst/rtf-examples/plan/, which the articles copy in and include.
##
##  Run from the package root, after a change to an article's tables:
##    Rscript data-raw/gen_plan_articles.R
## ---------------------------------------------------------------------------
pkgload::load_all(".", quiet = TRUE)

articles <- c("tables-from-ard", "plan-verbs", "plan-listings",
              "plan-and-as-rtftables")
out <- file.path(tempdir(), "plan-articles")
dir.create(out, showWarnings = FALSE)
Sys.setenv(RTFREPORTER_ARTICLE_OUT = normalizePath(out))

for (a in articles) {
  src <- file.path("vignettes", "articles", paste0(a, ".Rmd"))
  if (!file.exists(src)) next
  rmarkdown::render(src, output_format = rmarkdown::md_document(),
                    output_dir = tempdir(), quiet = TRUE)
}

soffice <- Filter(file.exists, c(
  "C:/Program Files/LibreOffice/program/soffice.exe",
  "C:/Program Files (x86)/LibreOffice/program/soffice.exe",
  Sys.which("soffice")))[1]
if (is.na(soffice)) stop("LibreOffice (soffice) is needed for the screenshots.")

figs <- file.path("inst", "rtf-examples", "plan")
dir.create(figs, showWarnings = FALSE, recursive = TRUE)
for (f in list.files(out, pattern = "[.]rtf$", full.names = TRUE)) {
  system2(soffice, c("--headless", "--convert-to", "png", "--outdir",
                     shQuote(normalizePath(figs)), shQuote(normalizePath(f))),
          stdout = FALSE, stderr = FALSE)
}
message("screenshots: ", paste(list.files(figs, pattern = "[.]png$"),
                               collapse = ", "))

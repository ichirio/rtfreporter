## ---------------------------------------------------------------------------
##  Pictures of the tables in the guide pages (headers and footers,
##  pagination, borders, figures, listings, importing tables, page setup,
##  sections, adding content, the plan and as_rtftables(), output, the
##  feature tour, from gt and gtsummary, from rtables and tern).
##
##  Each article keeps its code as a reader writes it; a hidden `.keep()`
##  after a table writes that table's RTF into the folder named by the
##  environment variable RTFREPORTER_ARTICLE_OUT (nothing, when it is unset),
##  and `.shot()` includes the picture made from it.  This script renders the
##  articles with that folder set (their working folder too, so a chunk that
##  writes a fixed file name leaves nothing in the repository), then turns the
##  first page of every RTF into a PNG under inst/rtf-examples/articles/
##  (cropped, as the gallery's: data-raw/rtf_png.R).
##
##  Run from the package root, after a change to an article's tables:
##    Rscript data-raw/gen_article_shots.R                    # every page
##    Rscript data-raw/gen_article_shots.R articles/figures   # some
## ---------------------------------------------------------------------------
pkgload::load_all(".", quiet = TRUE)

# the pages, under vignettes/ (the articles in articles/); the plan
# articles' pictures are data-raw/gen_plan_articles.R's
articles <- c(file.path("articles", c(
                "headers-footers", "pagination", "borders", "figures", "listings",
                "importing-tables", "page-setup", "section-splitting",
                "multistub-section-split", "adding-content", "plan-and-as-rtftables",
                "output")),
              "rtfreporter-quickstart", "from-gt", "from-rtables")
# `Rscript data-raw/gen_article_shots.R articles/figures`: those pages only
only <- commandArgs(trailingOnly = TRUE)
if (length(only)) articles <- intersect(articles, only)
out <- file.path(tempdir(), "article-shots")
dir.create(out, showWarnings = FALSE)
out <- normalizePath(out)
Sys.setenv(RTFREPORTER_ARTICLE_OUT = out)

root <- getwd()
for (a in articles) {
  src <- normalizePath(file.path(root, "vignettes", paste0(a, ".Rmd")))
  if (!file.exists(src)) next
  rmarkdown::render(src, output_format = rmarkdown::md_document(),
                    output_dir = out, knit_root_dir = out, quiet = TRUE,
                    envir = new.env(parent = globalenv()))
  setwd(root)   # (a page's chunk may move it)
}

source(file.path(root, "data-raw", "rtf_png.R"))   # soffice, rtf_to_png()

figs <- file.path(root, "inst", "rtf-examples", "articles")
dir.create(figs, showWarnings = FALSE, recursive = TRUE)
# only the pictures the articles ask for (a chunk's own fixed-name file is not one)
want <- unlist(lapply(articles, function(a) {
  x <- readLines(file.path(root, "vignettes", paste0(a, ".Rmd")))
  # the pictures a page shows (.shot("<name>.png")): a .keep() call over
  # several lines names its file on its last line, which a pattern of the
  # call's first line missed
  sub('^\\.shot\\("([^"]+)\\.png"\\)$', "\\1", grep('^\\.shot\\("', trimws(x), value = TRUE))
}))
for (w in want) {
  f <- file.path(out, paste0(w, ".rtf"))
  if (!file.exists(f)) {
    warning("no RTF for ", w, " (its chunk did not run?)", call. = FALSE)
    next
  }
  rtf_to_png(f, file.path(figs, paste0(w, ".png")))
}
message("pictures: ", paste(list.files(figs, pattern = "[.]png$"), collapse = ", "))

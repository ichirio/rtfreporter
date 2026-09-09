# Regenerates inst/templates/listing_wrap.R -- the source `listing_wrap_code()`
# hands out -- from the functions in R/listing.R themselves.
#
# The template is a FILE rather than something read off the installed package
# because keeping srcrefs through installation (`KeepSource: yes`) nearly
# doubles the installed R/ directory, for one feature.  A file costs 5 kB, and
# a reviewer can see it change in a diff.
#
# It cannot silently drift: test-listing-col-features.R parses this file,
# evaluates it and compares each function's deparse with the live one, so a
# change to the rule that is not regenerated here fails the suite.  Run:
#
#   Rscript data-raw/gen_listing_wrap_template.R

pkgload::load_all(".", quiet = TRUE)   # load_all keeps srcrefs, so comments survive

parts <- rtfreporter:::.listing_wrap_parts()
out <- c(
  "# GENERATED FILE -- do not edit.",
  "#",
  "# The \"multiline\" wrapping rule -- its policy half, copied verbatim from",
  "# R/listing.R by data-raw/gen_listing_wrap_template.R.  What it measures with",
  "# (listing_disp_width(), listing_take(), listing_split_after()) is exported,",
  "# so a fork shares those rather than carrying a copy.",
  "#",
  "# listing_wrap_code() renames these and hands them out; the suite checks",
  "# this file still matches.",
  "")
for (nm in parts) {
  fn <- get(nm, envir = asNamespace("rtfreporter"))
  src <- as.character(utils::getSrcref(fn))
  if (!length(src)) stop("no srcref for ", nm, " -- load with keep.source")
  out <- c(out, paste0(nm, " <- ", src[[1L]]), src[-1L], "")
}
writeLines(utils::head(out, -1L), "inst/templates/listing_wrap.R")
cat("wrote inst/templates/listing_wrap.R (", length(out) - 1L, "lines )\n")

# ---------------------------------------------------------------------------
#  The user's Traceability Matrix: URS -> FR -> UAT
# ---------------------------------------------------------------------------
#  Reads, from the kit folder:
#    urs.csv           the user requirements (UR-NNN) and the FRs each relies on
#    uat_cases.csv     the UAT cases (UAT-NNN), the URs and FRs each checks,
#                      and their results once run
#    requirements.csv  rtfreporter's Functional Requirements (optional: when
#                      present, each FR's statement and status are added)
#  Writes, to the same folder:
#    user_traceability.csv    one row a (UR, FR) pair: the UAT cases that
#                             check both, and their results
#    user_traceability.xlsx   that table, and one row a UR (its FRs, its UAT
#                             cases, whether every case passed)
#  Needs writexl for the xlsx (the csv is written without it).
#
#  Run:  Rscript build_user_matrix.R <kit folder>     (default: the current one)
#  or, in R, with `kit_dir` set:  kit_dir <- "my-kit"; source("build_user_matrix.R")
# ---------------------------------------------------------------------------

if (!exists("kit_dir", inherits = FALSE)) {
  args <- commandArgs(trailingOnly = TRUE)
  kit_dir <- if (length(args)) args[[1L]] else "."
}

read_kit <- function(f) {
  utils::read.csv(file.path(kit_dir, f), colClasses = "character", na.strings = "",
                  stringsAsFactors = FALSE, encoding = "UTF-8")
}
split_ids <- function(x) {
  x <- ifelse(is.na(x), "", x)
  lapply(strsplit(x, ";", fixed = TRUE), trimws)
}

urs <- read_kit("urs.csv")
uat <- read_kit("uat_cases.csv")
fr <- if (file.exists(file.path(kit_dir, "requirements.csv"))) read_kit("requirements.csv")

uat_ur <- split_ids(uat$ur_ids)
uat_fr <- split_ids(uat$fr_ids)
uat$result[is.na(uat$result)] <- ""

# one row a (UR, FR) pair
ur_fr <- split_ids(urs$fr_ids)
pairs <- data.frame(ur_id = rep(urs$id, lengths(ur_fr)), fr_id = unlist(ur_fr),
                    stringsAsFactors = FALSE)
pairs <- pairs[nzchar(pairs$fr_id), , drop = FALSE]
m <- merge(pairs, urs[c("id", "statement", "priority")], by.x = "ur_id", by.y = "id",
           all.x = TRUE, sort = FALSE)
names(m)[names(m) == "statement"] <- "ur_statement"
if (!is.null(fr)) {
  m$fr_statement <- fr$statement[match(m$fr_id, fr$id)]
  m$fr_status <- fr$status[match(m$fr_id, fr$id)]
  m$fr_status[is.na(m$fr_status)] <- "not in requirements.csv"
}
hits <- lapply(seq_len(nrow(m)), function(i) {
  which(vapply(uat_ur, function(u) m$ur_id[i] %in% u, logical(1L)) &
          vapply(uat_fr, function(f) m$fr_id[i] %in% f, logical(1L)))
})
m$uat_ids <- vapply(hits, function(h) paste(uat$id[h], collapse = ";"), "")
m$uat_results <- vapply(hits, function(h) {
  paste(ifelse(nzchar(uat$result[h]), uat$result[h], "not run"), collapse = ";")
}, "")
m <- m[order(m$ur_id, m$fr_id), , drop = FALSE]
lead <- c("ur_id", "priority", "ur_statement", "fr_id")
m <- m[c(lead, setdiff(names(m), lead))]
rownames(m) <- NULL

# one row a UR
by_ur <- data.frame(
  ur_id = urs$id, priority = urs$priority, statement = urs$statement,
  fr_ids = vapply(ur_fr, paste, "", collapse = ";"),
  uat_ids = vapply(urs$id, function(u) {
    paste(uat$id[vapply(uat_ur, function(x) u %in% x, logical(1L))], collapse = ";")
  }, ""),
  stringsAsFactors = FALSE)
by_ur$status <- vapply(urs$id, function(u) {
  r <- uat$result[vapply(uat_ur, function(x) u %in% x, logical(1L))]
  if (!length(r)) "no UAT case"
  else if (any(!nzchar(r))) "not run"
  else if (all(tolower(r) == "pass")) "pass"
  else "fail or deviation"
}, "")

utils::write.csv(m, file.path(kit_dir, "user_traceability.csv"), row.names = FALSE,
                 na = "", fileEncoding = "UTF-8")
out <- file.path(kit_dir, "user_traceability.csv")
if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(list(urs_fr_uat = m, by_requirement = by_ur),
                      file.path(kit_dir, "user_traceability.xlsx"))
  out <- c(out, file.path(kit_dir, "user_traceability.xlsx"))
} else {
  message("writexl is not installed: only user_traceability.csv was written.")
}
message("Written: ", paste(out, collapse = ", "))
if (any(by_ur$status == "no UAT case")) {
  message("URs with no UAT case: ",
          paste(by_ur$ur_id[by_ur$status == "no UAT case"], collapse = ", "))
}

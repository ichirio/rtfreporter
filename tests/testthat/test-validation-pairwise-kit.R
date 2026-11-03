# Validation tests: the validation kit -- rtfreporter_validation_kit() on a
# folder in each state, and the copied build_user_matrix.R on URS / UAT /
# requirements CSVs the user has edited -- a decision table and a pairwise
# set, checked on the files written and the objects and messages the script
# leaves.  Helpers prefixed .vk_.  Fictional data only.
#
# ---------------------------------------------------------------------------
#  Design
# ---------------------------------------------------------------------------
#  The rules, as read from the help (rtfreporter_validation_kit()) and the
#  code (R/validation_kit.R; inst/validation/templates/build_user_matrix.R):
#
#    folder    the kit is the templates with quality-plan.md and
#              requirements.csv; a missing or empty folder is written; one
#              with files in it is refused unless overwrite = TRUE, which
#              replaces the kit's files and keeps the others.  The full
#              paths are returned, invisibly.
#    matrix    build_user_matrix.R reads urs.csv, uat_cases.csv and (when
#              present) requirements.csv from kit_dir and leaves
#              m (one row a (UR, FR) pair, in user_traceability.csv) and
#              by_ur (one row a UR); a pair's uat_ids are the cases that
#              name both, its uat_results their results ("not run" when
#              blank); with requirements.csv each pair has fr_statement and
#              fr_status (the FR's status, "not in requirements.csv" for an
#              id it does not hold); a UR's status is "no UAT case", "not
#              run" (a blank result), "pass" (all Pass, any case) or "fail
#              or deviation"; messages name the FRs that are not active and
#              the URs with no UAT case.  A column the user adds is carried
#              by nothing and breaks nothing.
#
#  (A) FT-AS145, decision table: the folder.
#    folder before                  overwrite  result
#    missing                        FALSE      the kit written
#    missing                        TRUE       the kit written
#    empty                          FALSE      the kit written
#    empty                          TRUE       the kit written
#    a file of the user's           FALSE      "is not empty"; folder as it was
#    a file of the user's           TRUE       the kit written; the file kept
#    an earlier kit, urs.csv edited FALSE      "is not empty"; the edit kept
#    an earlier kit, urs.csv edited TRUE       urs.csv the shipped one again;
#                                              the user's matrix file kept
#  FR-AS229 FR-AS230 FR-AS232.
#
#  (B) FT-AS146, pairwise.  Factors and levels:
#    rows      shipped, added (UR-901 and its case UAT-901), nocase (UR-902,
#              no case names it)
#    removed   none, urs (the first FR of UR-001 deleted from
#              requirements.csv)
#    inactive  none, removed (the first FR of UR-002 given status "removed")
#    extra     none, urs, uat, requirements (a column "owner" added)
#    results   blank, pass (every case "Pass"), fail (UAT-002 "Fail")
#    reqfile   present, absent (requirements.csv deleted)
#  Constraint: removed, inactive and an extra requirements column need
#  requirements.csv.  The valid rows are the candidates (checked to hold
#  every valid pair) and .allpairs() picks the rows covering every pair.
#  Oracle: the (UR, FR) pairs of m against urs.csv, fr_status per FR,
#  uat_ids and uat_results of the added URs, every UR's status, the two
#  messages, the columns of m, user_traceability.csv as m.
#  FR-AS229 FR-AS249.
# ---------------------------------------------------------------------------

.vk_read <- function(d, f) {
  utils::read.csv(file.path(d, f), colClasses = "character", na.strings = "",
                  stringsAsFactors = FALSE, encoding = "UTF-8")
}
.vk_write <- function(x, d, f) {
  utils::write.csv(x, file.path(d, f), row.names = FALSE, na = "", fileEncoding = "UTF-8")
}
.vk_ids <- function(x) {
  x[is.na(x)] <- ""
  lapply(strsplit(x, ";", fixed = TRUE), trimws)
}

# Run the copied script in its folder: the objects it leaves and its messages.
.vk_run <- function(d) {
  e <- new.env(parent = globalenv())
  e$kit_dir <- d
  msgs <- character()
  withCallingHandlers(
    sys.source(file.path(d, "build_user_matrix.R"), envir = e),
    message = function(m) {
      msgs <<- c(msgs, trimws(conditionMessage(m)))
      invokeRestart("muffleMessage")
    })
  list(env = e, msgs = msgs)
}

test_that("FT-AS145 [FR-AS229 FR-AS230 FR-AS232]: validation kit decision table -- the folder before against overwrite", {
  kit <- c("build_user_matrix.R", "uat_cases.csv", "uat_cases.xlsx", "urs.csv", "urs.md",
           "validation_plan.md", "validation_summary_report.md", "quality-plan.md",
           "requirements.csv")
  shipped_urs <- readBin(system.file("validation", "templates", "urs.csv", package = "rtfreporter"),
                         "raw", 1e6)
  dt <- expand.grid(overwrite = c(FALSE, TRUE),
                    state = c("missing", "empty", "user_file", "earlier_kit"),
                    stringsAsFactors = FALSE)
  for (r in seq_len(nrow(dt))) {
    cs <- dt[r, ]
    lab <- paste(cs$state, cs$overwrite)
    d <- tempfile("vk")
    if (cs$state != "missing") dir.create(d)
    if (cs$state == "user_file") writeLines("mine", file.path(d, "notes.txt"))
    if (cs$state == "earlier_kit") {
      rtfreporter_validation_kit(d)
      writeLines("\"id\",\"statement\",\"priority\",\"fr_ids\"", file.path(d, "urs.csv"))
      writeLines("mine", file.path(d, "user_traceability.csv"))
    }
    before <- sort(list.files(d))
    refused <- cs$state %in% c("user_file", "earlier_kit") && !cs$overwrite
    if (refused) {
      expect_error(rtfreporter_validation_kit(d, overwrite = cs$overwrite),
                   "is not empty; give an empty or new folder, or overwrite = TRUE",
                   fixed = TRUE, label = lab)
      expect_identical(sort(list.files(d)), before, label = lab)
      if (cs$state == "earlier_kit") {
        expect_identical(readLines(file.path(d, "urs.csv")),
                         "\"id\",\"statement\",\"priority\",\"fr_ids\"", label = lab)
      }
    } else {
      vis <- withVisible(rtfreporter_validation_kit(d, overwrite = cs$overwrite))
      expect_false(vis$visible, label = lab)
      expect_setequal(basename(vis$value), kit)
      expect_identical(vis$value, file.path(normalizePath(d, winslash = "/"), basename(vis$value)),
                       label = lab)
      keep <- switch(cs$state, user_file = "notes.txt", earlier_kit = "user_traceability.csv",
                     character())
      expect_setequal(list.files(d), c(kit, keep))
      expect_identical(readBin(file.path(d, "urs.csv"), "raw", 1e6), shipped_urs, label = lab)
      if (length(keep)) expect_identical(readLines(file.path(d, keep)), "mine", label = lab)
    }
    unlink(d, recursive = TRUE)
  }
})

.vk_design <- function() {
  lv <- list(rows = c("shipped", "added", "nocase"),
             removed = c("none", "urs"),
             inactive = c("none", "removed"),
             extra = c("none", "urs", "uat", "requirements"),
             results = c("blank", "pass", "fail"),
             reqfile = c("present", "absent"))
  full <- expand.grid(lv, stringsAsFactors = FALSE)
  ok <- full$reqfile == "present" |
    (full$removed == "none" & full$inactive == "none" & full$extra != "requirements")
  valid <- full[ok, , drop = FALSE]
  rownames(valid) <- NULL
  every <- unique(unlist(lapply(utils::combn(names(lv), 2L, simplify = FALSE), function(q) {
    paste(q[1L], valid[[q[1L]]], q[2L], valid[[q[2L]]], sep = "|")
  })))
  list(pw = .allpairs(valid), every = every)
}

test_that("FT-AS146 [FR-AS249]: validation kit pairwise -- build_user_matrix.R on edited URS, UAT and requirements CSVs", {
  skip_if_not_installed("writexl")
  des <- .vk_design()
  expect_setequal(des$pw$need, des$every)
  expect_setequal(des$pw$covered, des$every)
  cases <- des$pw$rows
  expect_lte(nrow(cases), 20L)

  for (r in seq_len(nrow(cases))) {
    cs <- as.list(cases[r, , drop = FALSE])
    lab <- paste0("case ", r, " (", paste(names(cs), unlist(cs), sep = "=", collapse = ", "), ")")
    d <- tempfile("vk")
    rtfreporter_validation_kit(d)
    urs <- .vk_read(d, "urs.csv")
    uat <- .vk_read(d, "uat_cases.csv")
    req <- .vk_read(d, "requirements.csv")

    # -- the user's edits ----------------------------------------------------
    if (cs$rows == "added") {
      urs[nrow(urs) + 1L, ] <- c("UR-901", "Fictional need: a validation kit.", "Low",
                                 "FR-AS229;FR-AS230")
      uat[nrow(uat) + 1L, ] <- c("UAT-901", "UR-901", "FR-AS229", "1) Copy the kit.",
                                 "Nine files.", NA, NA, NA, NA)
    } else if (cs$rows == "nocase") {
      urs[nrow(urs) + 1L, ] <- c("UR-902", "Fictional need: no case yet.", "Low", "FR-AS231")
    }
    gone <- .vk_ids(urs$fr_ids[urs$id == "UR-001"])[[1L]][1L]
    off <- .vk_ids(urs$fr_ids[urs$id == "UR-002"])[[1L]][1L]
    if (cs$removed == "urs") req <- req[req$id != gone, , drop = FALSE]
    if (cs$inactive == "removed") req$status[req$id == off] <- "removed"
    uat$result <- switch(cs$results, blank = NA_character_, "Pass")
    if (cs$results == "fail") uat$result[uat$id == "UAT-002"] <- "Fail"
    if (cs$extra == "urs") urs$owner <- "QA"
    if (cs$extra == "uat") uat$owner <- "QA"
    if (cs$extra == "requirements") req$owner <- "QA"
    .vk_write(urs, d, "urs.csv")
    .vk_write(uat, d, "uat_cases.csv")
    if (cs$reqfile == "present") .vk_write(req, d, "requirements.csv")
    else unlink(file.path(d, "requirements.csv"))

    # -- the matrix ----------------------------------------------------------
    run <- .vk_run(d)
    m <- run$env$m
    by_ur <- run$env$by_ur
    ur_fr <- .vk_ids(urs$fr_ids)
    pairs <- unique(paste(rep(urs$id, lengths(ur_fr)), unlist(ur_fr)))
    expect_setequal(paste(m$ur_id, m$fr_id), pairs)
    expect_identical(nrow(m), length(pairs), label = lab)
    lead <- c("ur_id", "priority", "ur_statement", "fr_id")
    expect_identical(names(m), c(lead, if (cs$reqfile == "present") c("fr_statement", "fr_status"),
                                 "uat_ids", "uat_results"), label = lab)
    if (cs$reqfile == "present") {
      want <- ifelse(m$fr_id %in% req$id, req$status[match(m$fr_id, req$id)],
                     "not in requirements.csv")
      expect_identical(m$fr_status, want, label = lab)
      expect_identical(is.na(m$fr_statement), !m$fr_id %in% req$id, label = lab)
      if (cs$removed == "urs") expect_true(all(m$fr_status[m$fr_id == gone] == "not in requirements.csv"))
      if (cs$inactive == "removed") expect_true(all(m$fr_status[m$fr_id == off] == "removed"))
    }
    bad <- if (cs$reqfile == "present") unique(m$fr_id[m$fr_status != "active"]) else character()
    expect_identical(length(bad), (cs$removed == "urs") + (cs$inactive == "removed"), label = lab)
    nm <- grep("FRs that are not active", run$msgs, value = TRUE, fixed = TRUE)
    if (length(bad)) {
      expect_true(length(nm) == 1L && grepl(paste(bad, collapse = ", "), nm, fixed = TRUE),
                  label = lab)
    } else {
      expect_length(nm, 0L)
    }
    nc <- grep("URs with no UAT case", run$msgs, value = TRUE, fixed = TRUE)
    expect_identical(nc, if (cs$rows == "nocase") "URs with no UAT case: UR-902" else character(),
                     label = lab)

    # each UR's status, and the cases of the added URs
    has <- function(u) vapply(.vk_ids(uat$ur_ids), function(x) u %in% x, NA)
    st <- vapply(urs$id, function(u) {
      res <- uat$result[has(u)]
      if (!length(res)) "no UAT case" else if (anyNA(res)) "not run"
      else if (all(res == "Pass")) "pass" else "fail or deviation"
    }, "", USE.NAMES = FALSE)
    expect_identical(by_ur$status, st, label = lab)
    expect_identical(by_ur$ur_id, urs$id, label = lab)
    if (cs$rows == "added") {
      i <- which(m$ur_id == "UR-901")
      expect_identical(m$uat_ids[i], c("UAT-901", ""), label = lab)
      expect_identical(m$uat_results[i],
                       c(if (cs$results == "blank") "not run" else "Pass", ""), label = lab)
    }
    if (cs$rows == "nocase") {
      expect_identical(m$uat_ids[m$ur_id == "UR-902"], "", label = lab)
    }
    if (cs$results == "fail") {
      i <- grepl("UAT-002", m$uat_ids, fixed = TRUE)
      expect_true(all(grepl("Fail", m$uat_results[i], fixed = TRUE)), label = lab)
    }

    # the files written
    csv <- .vk_read(d, "user_traceability.csv")
    mm <- m
    mm[] <- lapply(mm, function(v) ifelse(is.na(v) | v == "", NA_character_, v))
    expect_identical(csv, mm, label = lab)
    expect_true(file.exists(file.path(d, "user_traceability.xlsx")), label = lab)
    unlink(d, recursive = TRUE)
  }
})

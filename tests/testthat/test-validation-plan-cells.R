# Validation tests: how plan_cells(), plan_digits() and plan_stub() combine --
# decision tables and pairwise sets.  Helpers prefixed .pc_.  Fictional data
# only.
#
# ---------------------------------------------------------------------------
#  Design
# ---------------------------------------------------------------------------
#  Subject: the text of a cell, and the stub, of the rtftable a plan gives.
#  The rules, as read from the code (R/table_plan.R plan_apply() step 4,
#  .plan_pick(), .plan_lookup_raw(), .plan_digits_for(), .plan_fill_one(),
#  .plan_merge(); R/ard_tables.R .ard_lookup_cells()):
#
#    template  the first key set, in the order variable > context > kind >
#              default (an unnamed entry is `default`).  With
#              plan_stat_groups() a split record reads variable > group >
#              context > kind > default, a record that went whole to
#              `.others` variable > context > kind > group > default.
#    digits    resolved apart from the template, per statistic: the first
#              key in the same order whose value names the statistic or is
#              one number; a statistic a narrower value does not name falls
#              through to the wider one.  A token that states its spec
#              ({mean:.4f}) is left alone.  "3s" is significant digits.
#    layers    .plan_merge() is last wins per KEY: a later layer replaces the
#              whole entry of a key it names (not statistic by statistic) and
#              keeps every other key.
#    stat_fmt  an open {x} reads stat_fmt when the frame has it; once the
#              digits fill it ({x:.nf}) it reads stat.  value = is for
#              stats = "rows" only.
#
#  Fixture (.pc_ard): two arms A / B; AGE (context ctxc, kind continuous;
#  mean, sd), WT (ctxw, continuous), SEX (ctxt, categorical; levels F / M;
#  n, p).  The contexts are not cards' summary / tabulate, which are aliases
#  of the kinds, so the context and the kind are two keys.  The cell read is
#  arm A of AGE's row "Stat" or SEX's row "F" on the rtftable's data.
#
#  (A) FT-PL916, decision table, which entry gives the template.  For AGE
#  and for SEX: every subset of {variable, context, kind, default} set, each
#  key its own marker (V, C, K, D); the default spelled unnamed on odd rows,
#  `default =` on even ones.  Winner the first set; none set: no row.
#  2 x 16 = 32 rows.  FR-PL226 FR-PL227 FR-AR163.
#
#  (B) FT-PL917, decision table, stat groups (.variables = "ALT", .others =
#  "N").  Record ALT (split) or AST (whole) x every subset of {variable,
#  group, context, kind}, the default always set.  Split: V > G > C > K > D;
#  whole: V > C > K > G > D.  2 x 16 = 32 rows.  FR-PL239.
#
#  (C) FT-PL918, decision table, which key gives the digits.  The templates
#  keyed by context and kind (so those digits keys reach); digits variable 3,
#  context 2, kind 1, default 0; every subset for AGE ("{mean} ({sd})") and
#  SEX ("{n} ({p:%})").  Winner the first set; none set: AGE prints the raw
#  numbers, SEX stops (FR-PL236).  2 x 16 = 32 rows.  FR-PL240 FR-PL242
#  FR-PL915.
#
#  (D) FT-PL919, decision table, fall-through by statistic.  AGE = narrow x
#  default = wide, each absent / one number / c(mean) / c(sd) / c(mean, sd).
#  Per statistic: the narrow value naming it or being one number, else the
#  wide one, else the raw number.  5 x 5 = 25 rows.  FR-PL241 FR-PL242.
#
#  (E) FT-PL920, decision table, a later layer wins key by key: 10 rows of
#  plan_digits() layers and 6 of plan_cells() layers (below in the test).
#  FR-PL229 FR-PL242 FR-IX257 FR-PL916.
#
#  (F) FT-PL921, pairwise.  Factors: variable (AGE, SEX) x template key
#  (variable, context, kind, unnamed, default) x digits key (variable, the
#  template's key, unnamed) x digits form (one number, named by statistic --
#  naming one statistic only --, "3s", mixed named "3s" and number) x token
#  (all open, the first stating its spec) x stat_fmt in the frame (no, yes) x
#  value (unset, "stat", "stat_fmt").  1440 combinations, the rows the
#  greedy .allpairs() (helper-allpairs.R) picks; the expected text from a
#  small formatter.  FR-PL227 FR-PL240 FR-PL241 FR-IX202 FR-PL915 FR-PL917.
#
#  (G) FT-PL922, pairwise, the stub.  Factors: layout (templates, stats =
#  "rows") x indent (unset = 4, 2, 0) x name (rl, row_label) x heading
#  (unset, "", "Parameter") x before (FALSE, TRUE) x digits (none; plan_digits(1)
#  for templates, plan_digits(.rows = ) for rows).  The stub column: a heading
#  row per PARAM and a leaf per row, indented by that many non-breaking
#  spaces; its name `name`, its header text `heading`, else `name`.
#  FR-PL244 FR-PL360 FR-PL362 FR-PL363 FR-PL365 FR-PL366 FR-PL912 FR-PL918.
#
#  (H) FT-PL923, decision table, rounding (r, sas) x where the digits come
#  from (a variable key, the default, a token's own .2f): 2.125 to 2 dp is
#  2.12 / 2.13 whichever.  6 rows.  FR-PL243 FR-AR178.
#
#  (I) FT-PL924, decision table, a digits key of a context or a kind
#  reaches the cells of that context or kind whatever key their template
#  has (#821).  AGE and SEX x template key (variable, context, kind,
#  default) x digits key (context, kind).  2 x 4 x 2 = 16 rows.  FR-PL242
#  FR-PL915.
#
#  (J) FT-PL925, decision table, the digits with stat groups (#821).  Record
#  ALT (split) or AST (whole) x template key (variable, group, default) x
#  every subset of digits keys {variable, group, context, kind}, the default
#  always set.  The digits read the order of the template: split V > G > C >
#  K > D, whole V > C > K > G > D.  2 x 3 x 16 = 96 rows.  FR-PL239
#  FR-PL242 FR-PL915.
# ---------------------------------------------------------------------------

.PC_KEYS <- list(AGE = c(variable = "AGE", context = "ctxc", kind = "continuous"),
                 SEX = c(variable = "SEX", context = "ctxt", kind = "categorical"))
.PC_ORDER <- c("variable", "context", "kind", "default")
.PC_MARK <- c(variable = "V", context = "C", kind = "K", default = "D", group = "G")
.PC_NBSP <- intToUtf8(160L)

.pc_ard <- function(fmt = FALSE) {
  one <- function(trt, k) {
    rbind(
      data.frame(TRT = trt, variable = "AGE", context = "ctxc", .kind = "continuous", .label = NA,
                 stat_name = c("mean", "sd"), stat = c(31.2468, 4.1357) + k),
      data.frame(TRT = trt, variable = "WT", context = "ctxw", .kind = "continuous", .label = NA,
                 stat_name = c("mean", "sd"), stat = c(70.4512, 9.8762) + k),
      data.frame(TRT = trt, variable = "SEX", context = "ctxt", .kind = "categorical",
                 .label = rep(c("F", "M"), each = 2), stat_name = c("n", "p", "n", "p"),
                 stat = c(12, 0.6 + k / 10, 8, 0.4 - k / 10)))
  }
  d <- rbind(one("A", 0), one("B", 1))
  if (fmt) d$stat_fmt <- paste0("<", d$stat, ">")
  d
}

.pc_base <- function(d = .pc_ard()) table_plan(d, cols = "TRT", rows = c(group = "variable"))

# The rtftable the plan gives, its rows folded into a stub `rl`.
.pc_page <- function(p) {
  pg <- suppressMessages(plan_apply(p |> plan_style() |> plan_stub(name = "rl"), "pages"))
  if (inherits(pg, "rtftable")) pg else pg[[1L]]
}

# Arm A's text on the leaf `leaf` under the heading `var`; NULL when the
# variable has no row.
.pc_cell <- function(pg, var, leaf, col = "A") {
  rl <- gsub(.PC_NBSP, "", pg$data$rl, fixed = TRUE)
  h <- which(rl == var)[1L]
  if (is.na(h)) return(NULL)
  i <- h + match(leaf, rl[-seq_len(h)])
  as.character(pg$data[[col]][i])
}

.pc_leaf <- c(AGE = "Stat", SEX = "F")

# A template for a variable: a named row for AGE, one row a level for SEX.
.pc_tpl <- function(var, body) if (var == "AGE") c(Stat = body) else body

# The cells entries of the variables other than `var`, by their own key and
# with every spec stated, so that no key under test reaches them.
.pc_others <- function(var) {
  o <- list(AGE = c(Stat = "{mean:.1f}"), WT = c(Stat = "{mean:.1f}"), SEX = "{n:.0f}")
  o[setdiff(names(o), var)]
}

.pc_key_name <- function(var, k) if (k == "default") "default" else .PC_KEYS[[var]][[k]]

# Every subset of `keys`, in a fixed order (the empty set first).
.pc_subsets <- function(keys) {
  g <- expand.grid(rep(list(c(FALSE, TRUE)), length(keys)))
  lapply(seq_len(nrow(g)), function(i) keys[unlist(g[i, ])])
}

# plan_cells() / plan_digits() with these entries; an entry named "" is
# passed unnamed.
.pc_call <- function(verb, p, entries, ...) {
  do.call(verb, c(list(p), entries, list(...)))
}

# The expected text of one token: `x` the number, `spec` the token's own
# spec ("" or "%" when open), `d` the digits the plan gives it (NULL none),
# `fmt` the stat_fmt the frame carries for it (NULL none).
.pc_fmt <- function(x, spec, d, fmt = NULL) {
  pct <- endsWith(spec, "%")
  open <- spec %in% c("", "%")
  if (open && is.null(d)) return(if (is.null(fmt)) as.character(x) else fmt)
  if (!open) d <- as.integer(sub("^\\.([0-9]+)[fd]?%?$", "\\1", sub("^d$", ".0", spec)))
  v <- if (pct) x * 100 else x
  if (is.character(d) && grepl("s$", d)) {
    n <- as.integer(sub("s$", "", d))
    return(formatC(signif(v, n), digits = n, format = "fg", flag = "#"))
  }
  sprintf("%.*f", as.integer(d), v)
}

# The digits a statistic takes from one value (a number, "3s", or a vector
# named by statistic); NULL when the value says nothing about it.
.pc_dig <- function(val, stat) {
  if (is.null(val)) return(NULL)
  if (is.null(names(val))) return(val[[1L]])
  if (stat %in% names(val)) val[[stat]] else NULL
}


# ---- (A) the template's key ------------------------------------------------

test_that("FT-PL916 [FR-PL226 FR-PL227 FR-AR163]: decision table -- the template is the first key set of variable > context > kind > default", {
  n <- 0L
  for (var in names(.PC_KEYS)) {
    body <- if (var == "AGE") "{mean:.1f}" else "{n:.0f}"
    plain <- if (var == "AGE") "31.2" else "12"
    sets <- .pc_subsets(.PC_ORDER)
    for (r in seq_along(sets)) {
      set <- sets[[r]]
      ent <- list()
      for (k in set) {
        nm <- if (k == "default" && r %% 2L == 1L) "" else .pc_key_name(var, k)
        ent[[length(ent) + 1L]] <- .pc_tpl(var, paste0(.PC_MARK[[k]], body))
        names(ent)[length(ent)] <- nm
      }
      ent <- c(ent, .pc_others(var))
      pg <- .pc_page(.pc_call(plan_cells, .pc_base(), ent, notes = FALSE))
      want <- if (length(set)) paste0(.PC_MARK[[set[1L]]], plain) else NULL
      expect_identical(.pc_cell(pg, var, .pc_leaf[[var]]), want,
                       label = sprintf("%s / keys {%s}", var, paste(set, collapse = " ")))
      n <- n + 1L
    }
  }
  expect_identical(n, 32L)
})


# ---- (B) stat groups --------------------------------------------------------

.pc_sg <- function() {
  data.frame(TRT = rep(c("A", "B"), each = 6),
             variable = rep(rep(c("ALT", "AST"), each = 3), 2),
             context = "ctxc", .kind = "continuous", LBL = "",
             stat_name = rep(c("n", "mean", "sd"), 4),
             stat = c(20, 31.2468, 4.1357, 19, 28.7123, 3.9246, 18, 33.1, 5.3, 17, 30.2, 4.4))
}

test_that("FT-PL917 [FR-PL239]: decision table -- a split record reads variable > group > context > kind, a whole one variable > context > kind > group", {
  base <- table_plan(.pc_sg(), cols = c("TRT", ".stat_group"), rows = c(group = "variable"),
                     label = c(row = "LBL")) |>
    plan_stat_groups(N = "n", msd = c("mean", "sd"), .variables = "ALT", .others = "N")
  order <- list(ALT = c("variable", "group", "context", "kind"),
                AST = c("variable", "context", "kind", "group"))
  name <- c(group = "N", context = "ctxc", kind = "continuous")
  n <- 0L
  for (rec in names(order)) {
    for (set in .pc_subsets(c("variable", "group", "context", "kind"))) {
      ent <- list(msd = "M{mean:.1f}", default = "D{n:.0f}")
      for (k in set) {
        ent[[if (k == "variable") rec else name[[k]]]] <- paste0(.PC_MARK[[k]], "{n:.0f}")
      }
      pg <- suppressMessages(plan_apply(.pc_call(plan_cells, base, ent, notes = FALSE) |>
                                          plan_style(), "pages"))
      pg <- if (inherits(pg, "rtftable")) pg else pg[[1L]]
      win <- intersect(order[[rec]], set)
      want <- paste0(if (length(win)) .PC_MARK[[win[1L]]] else "D",
                     if (rec == "ALT") "20" else "19")
      expect_identical(as.character(pg$data$A____N[pg$data$group == rec]), want,
                       label = sprintf("%s / keys {%s}", rec, paste(set, collapse = " ")))
      n <- n + 1L
    }
  }
  expect_identical(n, 32L)
})


# ---- (C) the digits' key ----------------------------------------------------

test_that("FT-PL918 [FR-PL236 FR-PL240 FR-PL242 FR-PL915]: decision table -- the digits are the first key set of variable > context > kind > default, apart from the template's", {
  tpl <- list(AGE = c(Stat = "{mean} ({sd})"), SEX = "{n} ({p:%})")
  cells <- list(ctxc = tpl$AGE, continuous = tpl$AGE, ctxt = tpl$SEX, categorical = tpl$SEX,
                WT = c(Stat = "{mean:.1f}"))
  base <- .pc_call(plan_cells, .pc_base(), cells, notes = FALSE)
  dval <- c(variable = 3L, context = 2L, kind = 1L, default = 0L)
  n <- 0L
  for (var in names(.PC_KEYS)) {
    for (set in .pc_subsets(.PC_ORDER)) {
      dig <- list()
      for (k in set) dig[[.pc_key_name(var, k)]] <- dval[[k]]
      # the other variable's {p:%} needs digits of its own
      if (var == "AGE") dig$SEX <- 0L
      p <- if (length(dig)) .pc_call(plan_digits, base, dig) else base
      label <- sprintf("%s / keys {%s}", var, paste(set, collapse = " "))
      n <- n + 1L
      if (var == "SEX" && !length(set)) {
        expect_error(.pc_page(p), "asks the plan for its digits", fixed = TRUE, label = label)
        next
      }
      d <- if (length(set)) dval[[set[1L]]] else NULL
      want <- if (var == "AGE") {
        paste0(.pc_fmt(31.2468, "", d), " (", .pc_fmt(4.1357, "", d), ")")
      } else {
        paste0(.pc_fmt(12, "", d), " (", .pc_fmt(0.6, "%", d), ")")
      }
      expect_identical(.pc_cell(.pc_page(p), var, .pc_leaf[[var]]), want, label = label)
    }
  }
  expect_identical(n, 32L)
})


# ---- (D) a statistic falls through -----------------------------------------

test_that("FT-PL919 [FR-PL241 FR-PL242]: decision table -- a statistic the narrower value does not name falls through to the wider one", {
  narrow <- list(none = NULL, one = 3L, mean = c(mean = 3L), sd = c(sd = 2L),
                 both = c(mean = 3L, sd = 2L))
  wide <- list(none = NULL, one = 1L, mean = c(mean = 1L), sd = c(sd = 0L),
               both = c(mean = 1L, sd = 0L))
  base <- plan_cells(.pc_base(), c(Stat = "{mean} ({sd})"), SEX = "{n:.0f}", notes = FALSE)
  n <- 0L
  for (a in names(narrow)) {
    for (w in names(wide)) {
      dig <- list(AGE = narrow[[a]], default = wide[[w]])
      dig <- dig[!vapply(dig, is.null, NA)]
      p <- if (length(dig)) .pc_call(plan_digits, base, dig) else base
      pick <- function(s) .pc_dig(narrow[[a]], s) %||% .pc_dig(wide[[w]], s)
      want <- paste0(.pc_fmt(31.2468, "", pick("mean")), " (", .pc_fmt(4.1357, "", pick("sd")), ")")
      expect_identical(.pc_cell(.pc_page(p), "AGE", "Stat"), want,
                       label = sprintf("AGE = %s, default = %s", a, w))
      n <- n + 1L
    }
  }
  expect_identical(n, 25L)
})


# ---- (E) layers ---------------------------------------------------------------

test_that("FT-PL920 [FR-PL229 FR-PL242 FR-IX257 FR-PL916]: decision table -- a later layer replaces the whole entry of the keys it names and keeps the rest", {
  base <- plan_cells(.pc_base(), continuous = c(Stat = "{mean} ({sd})"), SEX = "{n:.0f}",
                     notes = FALSE)
  # layers of plan_digits(); the digits of mean and sd for AGE and for WT
  dt <- list(
    list(list(default = 1L), list(default = 2L), age = c(2, 2), wt = c(2, 2)),
    list(list(default = 1L), list(AGE = 2L), age = c(2, 2), wt = c(1, 1)),
    list(list(AGE = 2L), list(default = 1L), age = c(2, 2), wt = c(1, 1)),
    list(list(default = 0L, AGE = c(mean = 1L, sd = 2L)), list(AGE = c(mean = 3L)),
         age = c(3, 0), wt = c(0, 0)),
    list(list(default = 0L, AGE = c(mean = 3L)), list(AGE = c(sd = 2L)), age = c(0, 2), wt = c(0, 0)),
    list(list(default = 0L, AGE = 3L), list(AGE = c(mean = 1L)), age = c(1, 0), wt = c(0, 0)),
    list(list(continuous = c(mean = 1L, sd = 2L)), list(continuous = 3L), age = c(3, 3), wt = c(3, 3)),
    list(list(default = 1L), list(AGE = 2L), list(default = 3L), age = c(2, 2), wt = c(3, 3)),
    list(list(continuous = 2L), list(default = 0L), age = c(2, 2), wt = c(2, 2)),
    list(list(default = 0L, AGE = c(mean = 2L)), list(continuous = c(sd = 3L)),
         age = c(2, 3), wt = c(0, 3)))
  val <- list(AGE = c(31.2468, 4.1357), WT = c(70.4512, 9.8762))
  for (i in seq_along(dt)) {
    row <- dt[[i]]
    p <- base
    for (l in row[!names(row) %in% c("age", "wt")]) p <- .pc_call(plan_digits, p, l)
    pg <- .pc_page(p)
    for (v in c("AGE", "WT")) {
      d <- row[[tolower(v)]]
      want <- sprintf("%.*f (%.*f)", as.integer(d[1L]), val[[v]][1L], as.integer(d[2L]), val[[v]][2L])
      expect_identical(.pc_cell(pg, v, "Stat"), want, label = sprintf("digits row %d, %s", i, v))
    }
  }
  # layers of plan_cells(); the marker each variable's cell starts with
  t <- function(m) c(Stat = paste0(m, "{mean:.1f}"))
  ct <- list(
    list(list(default = t("D")), list(default = t("E")), age = "E", wt = "E"),
    list(list(AGE = t("V")), list(default = t("D")), age = "V", wt = "D"),
    list(list(default = t("D")), list(AGE = t("V")), age = "V", wt = "D"),
    list(list(continuous = t("K")), list(default = t("D")), age = "K", wt = "K"),
    list(list(default = t("D"), AGE = t("V")), list(AGE = t("X")), age = "X", wt = "D"),
    list(list(default = t("D"), AGE = c(Stat = "V{mean:.1f}", Other = "W{sd:.1f}")),
         list(AGE = t("X")), age = "X", wt = "D"))
  for (i in seq_along(ct)) {
    row <- ct[[i]]
    p <- .pc_base()
    for (l in row[!names(row) %in% c("age", "wt")]) p <- .pc_call(plan_cells, p, l)
    pg <- .pc_page(plan_cells(p, SEX = "{n:.0f}", notes = FALSE))
    expect_identical(.pc_cell(pg, "AGE", "Stat"), paste0(row$age, "31.2"), label = sprintf("cells row %d, AGE", i))
    expect_identical(.pc_cell(pg, "WT", "Stat"), paste0(row$wt, "70.5"), label = sprintf("cells row %d, WT", i))
    # an entry replaced whole: the earlier layer's other row is gone
    expect_false("Other" %in% gsub(.PC_NBSP, "", pg$data$rl, fixed = TRUE), label = sprintf("cells row %d", i))
  }
})


# ---- (F) pairwise: template x digits ------------------------------------------

test_that("FT-PL921 [FR-PL227 FR-PL240 FR-PL241 FR-IX202 FR-PL915 FR-PL917]: pairwise -- template key x digits key x digits form x token x stat_fmt x value", {
  cand <- expand.grid(var = c("AGE", "SEX"),
                      tkey = c("variable", "context", "kind", "unnamed", "default"),
                      dkey = c("variable", "template", "unnamed"),
                      form = c("one", "by_stat", "sig", "mixed"),
                      token = c("open", "stated"), fmt = c("no", "yes"),
                      value = c("unset", "stat", "stat_fmt"), stringsAsFactors = FALSE)
  ap <- .allpairs(cand)
  expect_setequal(ap$covered, ap$need)
  expect_lt(nrow(ap$rows), nrow(cand) / 20)
  # the template, its tokens (statistic, spec) and the digits forms, per variable
  tpl <- list(AGE = c(open = "{mean} ({sd})", stated = "{mean:.4f} ({sd})"),
              SEX = c(open = "{n} ({p:%})", stated = "{n:.0f} ({p:%})"))
  tok <- list(AGE = list(open = c(mean = "", sd = ""), stated = c(mean = ".4f", sd = "")),
              SEX = list(open = c(n = "", p = "%"), stated = c(n = ".0f", p = "%")))
  x <- list(AGE = c(mean = 31.2468, sd = 4.1357), SEX = c(n = 12, p = 0.6))
  forms <- list(AGE = list(one = 2L, by_stat = c(mean = 2L), sig = "3s", mixed = c(mean = "3s", sd = 1)),
                SEX = list(one = 2L, by_stat = c(p = 2L), sig = "3s", mixed = c(n = 1, p = "3s")))
  set <- ap$rows
  for (i in seq_len(nrow(set))) {
    r <- set[i, ]
    v <- r$var
    key <- function(k) switch(k, unnamed = "", default = "default",
                              .PC_KEYS[[v]][[k]])
    tk <- key(r$tkey)
    dk <- if (r$dkey == "template") tk else key(r$dkey)
    cells <- c(stats::setNames(list(.pc_tpl(v, tpl[[v]][[r$token]])), tk), .pc_others(v))
    args <- list(notes = FALSE)
    if (r$value != "unset") args$value <- r$value
    p <- .pc_call(plan_cells, .pc_base(.pc_ard(r$fmt == "yes")), c(cells, args))
    p <- .pc_call(plan_digits, p, stats::setNames(list(forms[[v]][[r$form]]), dk))
    tk_spec <- tok[[v]][[r$token]]
    got_parts <- vapply(names(tk_spec), function(s) {
      f <- if (r$fmt == "yes") paste0("<", x[[v]][[s]], ">")
      .pc_fmt(x[[v]][[s]], tk_spec[[s]], .pc_dig(forms[[v]][[r$form]], s), f)
    }, "")
    want <- paste0(got_parts[1L], " (", got_parts[2L], ")")
    expect_identical(.pc_cell(.pc_page(p), v, .pc_leaf[[v]]), want,
                     label = paste(v, r$tkey, r$dkey, r$form, r$token, r$fmt, r$value, sep = " / "))
  }
})


# ---- (G) pairwise: the stub ---------------------------------------------------

.pc_lab <- function() {
  data.frame(TRT = rep(c("A", "B"), each = 6),
             PARAM = rep(rep(c("ALT", "AST"), each = 3), 2),
             stat_name = rep(c("n", "mean", "sd"), 4),
             stat = c(20, 31.2468, 4.1357, 19, 28.7123, 3.9246, 18, 33.1, 5.3, 17, 30.2, 4.4))
}

test_that("FT-PL922 [FR-PL244 FR-PL360 FR-PL362 FR-PL363 FR-PL365 FR-PL366 FR-PL912 FR-PL918]: pairwise -- the stub: layout x indent x name x heading x before x digits", {
  cand <- expand.grid(layout = c("cells", "rows"), indent = c("unset", "2", "0"),
                      name = c("rl", "row_label"), heading = c("unset", "blank", "Parameter"),
                      before = c(FALSE, TRUE), digits = c("none", "set"), stringsAsFactors = FALSE)
  ap <- .allpairs(cand)
  expect_setequal(ap$covered, ap$need)
  expect_lt(nrow(ap$rows), nrow(cand) / 5)
  set <- ap$rows
  for (i in seq_len(nrow(set))) {
    r <- set[i, ]
    p <- table_plan(.pc_lab(), cols = "TRT", rows = c(param = "PARAM"), label = c(row = "stat_name"))
    if (r$layout == "cells") {
      p <- plan_cells(p, c(n = "{n:.0f}", "Mean (SD)" = "{mean} ({sd})"), notes = FALSE)
      if (r$digits == "set") p <- plan_digits(p, 1)
      leaves <- c("n", "Mean (SD)")
      want <- if (r$digits == "set") "31.2 (4.1)" else "31.2468 (4.1357)"
    } else {
      p <- plan_cells(p, stats = "rows", notes = FALSE)
      if (r$digits == "set") p <- plan_digits(p, .rows = c(n = 0, mean = 1, sd = 2))
      leaves <- c("n", "mean", "sd")
      want <- if (r$digits == "set") "31.2" else "31.2468"
    }
    sa <- list(name = r$name, before = r$before)
    if (r$indent != "unset") sa$indent <- as.integer(r$indent)
    if (r$heading != "unset") sa$heading <- if (r$heading == "blank") "" else r$heading
    pg <- suppressMessages(plan_apply(do.call(plan_stub, c(list(plan_style(p)), sa)), "pages"))
    pg <- if (inherits(pg, "rtftable")) pg else pg[[1L]]
    label <- paste(r$layout, r$indent, r$name, r$heading, r$before, r$digits, sep = " / ")
    ind <- strrep(.PC_NBSP, if (r$indent == "unset") 4L else as.integer(r$indent))
    stub <- unlist(lapply(c("ALT", "AST"), function(h) c(h, paste0(ind, leaves))))
    expect_identical(names(pg$data)[1L], r$name, label = label)
    expect_identical(as.character(pg$data[[1L]]), stub, label = label)
    head <- unlist(pg$col_header %||% names(pg$data))
    expect_identical(head[1L], switch(r$heading, unset = r$name, blank = "", r$heading), label = label)
    leaf <- if (r$layout == "cells") "Mean (SD)" else "mean"
    expect_identical(as.character(pg$data$A[match(paste0(ind, leaf), pg$data[[1L]])]), want,
                     label = label)
  }
})


# ---- (H) rounding ----------------------------------------------------------------

test_that("FT-PL923 [FR-PL243 FR-AR178]: decision table -- the rounding family reaches the digits from any key and a token's own", {
  d <- .pc_ard()
  d$stat[d$variable == "AGE" & d$stat_name == "mean"] <- 2.125
  base <- function(t) plan_cells(.pc_base(d), AGE = c(Stat = t), WT = c(Stat = "{mean:.1f}"),
                                 SEX = "{n:.0f}", notes = FALSE)
  src <- list(variable = function(p) plan_digits(p, AGE = 2L),
              default = function(p) plan_digits(p, 2L),
              token = function(p) p)
  want <- c(r = "2.12", sas = "2.13")
  n <- 0L
  for (rd in names(want)) {
    for (s in names(src)) {
      p <- src[[s]](base(if (s == "token") "{mean:.2f}" else "{mean}"))
      p <- plan_digits(p, rounding = rd)
      expect_identical(.pc_cell(.pc_page(p), "AGE", "Stat"), want[[rd]],
                       label = sprintf("rounding %s / digits from %s", rd, s))
      n <- n + 1L
    }
  }
  expect_identical(n, 6L)
})


# ---- (I) a context or kind digits key, any template key ------------------------

test_that("FT-PL924 [FR-PL242 FR-PL915]: decision table -- a digits key of a context or a kind reaches the cells whatever key their template has", {
  tpl <- list(AGE = "{mean} ({sd})", SEX = "{n} ({p:%})")
  x <- list(AGE = c(31.2468, 4.1357), SEX = c(12, 0.6))
  spec <- list(AGE = c("", ""), SEX = c("", "%"))
  n <- 0L
  for (var in names(.PC_KEYS)) {
    for (tk in .PC_ORDER) {
      for (dk in c("context", "kind")) {
        cells <- c(stats::setNames(list(.pc_tpl(var, tpl[[var]])), .pc_key_name(var, tk)),
                   .pc_others(var))
        p <- .pc_call(plan_cells, .pc_base(), cells, notes = FALSE)
        p <- .pc_call(plan_digits, p, stats::setNames(list(2L), .pc_key_name(var, dk)))
        want <- paste0(.pc_fmt(x[[var]][1L], spec[[var]][1L], 2L), " (",
                       .pc_fmt(x[[var]][2L], spec[[var]][2L], 2L), ")")
        expect_identical(.pc_cell(.pc_page(p), var, .pc_leaf[[var]]), want,
                         label = sprintf("%s / template %s / digits %s", var, tk, dk))
        n <- n + 1L
      }
    }
  }
  expect_identical(n, 16L)
})


# ---- (J) the digits with stat groups ---------------------------------------------

test_that("FT-PL925 [FR-PL239 FR-PL242 FR-PL915]: decision table -- with stat groups the digits reach split and whole records in the order of their template", {
  base <- table_plan(.pc_sg(), cols = c("TRT", ".stat_group"), rows = c(group = "variable"),
                     label = c(row = "LBL")) |>
    plan_stat_groups(N = "n", msd = c("mean", "sd"), .variables = "ALT", .others = "N")
  order <- list(ALT = c("variable", "group", "context", "kind"),
                AST = c("variable", "context", "kind", "group"))
  name <- c(group = "N", context = "ctxc", kind = "continuous")
  dval <- c(variable = 3L, group = 2L, context = 1L, kind = 4L)
  nval <- c(ALT = 20, AST = 19)
  n <- 0L
  for (rec in names(order)) {
    other <- setdiff(names(order), rec)
    for (tk in c("variable", "group", "default")) {
      tkey <- switch(tk, variable = rec, group = "N", default = "default")
      cells <- stats::setNames(list("{n}", "M{mean:.1f}", "{n:.0f}"), c(tkey, "msd", other))
      for (set in .pc_subsets(names(dval))) {
        dig <- list(default = 0L)
        for (k in set) dig[[if (k == "variable") rec else name[[k]]]] <- dval[[k]]
        p <- .pc_call(plan_digits, .pc_call(plan_cells, base, cells, notes = FALSE), dig)
        tb <- suppressMessages(plan_apply(p, "table"))
        win <- intersect(order[[rec]], set)
        d <- if (length(win)) dval[[win[1L]]] else 0L
        expect_identical(as.character(tb$A____N[tb$group == rec]), sprintf("%.*f", d, nval[[rec]]),
                         label = sprintf("%s / template %s / digits {%s}", rec, tk,
                                         paste(set, collapse = " ")))
        n <- n + 1L
      }
    }
  }
  expect_identical(n, 96L)
})

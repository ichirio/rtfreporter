setwd("C:/Yrepo/rtfreporter")
suppressMessages(devtools::load_all(".", quiet = TRUE))

df <- data.frame(A = c("a", "b"), B = c("1", "2"), stringsAsFactors = FALSE)
tb <- rtftable(df, border = "tfl")

bd  <- rtf_border(top = rtf_border_side("single"))
tbd <- rtf_table_border(body = bd)
sty <- rtf_table_style(border_body = bd)
cc  <- col_cell(1L, "X")
ch  <- rtf_col_header(c("A", "B"))
brc <- blank_rows_by_change("A")

# Does FUN accept VALUE in argument ARG?  A wrong TYPE errors; a missing
# ARGUMENT also errors.  Either way the answer is "no".
accepts <- function(fun, arg, value, ...) {
  f <- tryCatch(match.fun(fun), error = function(e) NULL)
  if (is.null(f)) return(FALSE)
  # An S3 generic is function(x, ...): the real arguments live on the method,
  # so look there when the generic itself does not declare `arg`.
  fm <- names(formals(f))
  if (!(arg %in% fm)) {
    meth <- tryCatch(get(paste0(fun, ".rtftable"),
                         envir = asNamespace("rtfreporter")),
                     error = function(e) NULL)
    if (!is.null(meth)) fm <- names(formals(meth))
  }
  if (!(arg %in% fm)) return(FALSE)
  a <- c(list(...), stats::setNames(list(value), arg))
  isTRUE(tryCatch({ do.call(f, a); TRUE },
                  error = function(e) FALSE, warning = function(w) TRUE))
}
m <- function(x) if (isTRUE(x)) "o" else "-"

cat("値のコンストラクタは、どこで使えるか\n")
cat("（o = その関数のその引数に渡せる / - = 渡せない、または引数が無い）\n\n")
cat(sprintf("%-22s %-9s %-11s %-9s %-11s %-9s %s\n",
            "コンストラクタ", "rtftable", "plan_style", "style_*",
            "rtf_default_", "rtf_tables", "その他"))

row <- function(nm, a, b, c, d, e, extra) {
  cat(sprintf("%-22s %-9s %-11s %-9s %-11s %-9s %s\n",
              nm, m(a), m(b), m(c), m(d), m(e), extra))
}

row("rtf_border()",
    accepts("rtftable", "border", bd, data = df),
    accepts("plan_style", "border", bd, plan = rtf_plan(df)),
    accepts("style_cols", "border", bd, x = tb, cols = 1L),
    accepts("rtf_default_format", "border", bd),
    accepts("rtf_tables", "border", bd, doc = rtf_document(), tables = tb),
    paste0("col_cell:", m(accepts("col_cell", "border", bd, pos = 1L,
                                  label = "X")),
           " rtf_table_border:", m(accepts("rtf_table_border", "body", bd))))

row("rtf_table_border()",
    accepts("rtftable", "border", tbd, data = df),
    accepts("plan_style", "border", tbd, plan = rtf_plan(df)),
    accepts("style_body", "border", bd, x = tb, rows = 1L),
    accepts("rtf_default_format", "border", tbd),
    accepts("rtf_tables", "border", tbd, doc = rtf_document(), tables = tb),
    "")

row("rtf_table_style()",
    accepts("rtftable", "style", sty, data = df),
    accepts("plan_style", "style", sty, plan = rtf_plan(df)),
    FALSE,
    accepts("rtf_default_format", "style", sty),
    accepts("rtf_tables", "style", sty, doc = rtf_document(), tables = tb),
    "")

row("rtf_col_header()",
    accepts("rtftable", "col_header", ch, data = df),
    FALSE,
    accepts("set_col_header", "x", tb),
    FALSE,
    accepts("rtf_tables", "col_header", ch, doc = rtf_document(), tables = tb),
    paste0("plan_header:",
           m(isTRUE(tryCatch({ plan_header(rtf_plan(df), ch); TRUE },
                             error = function(e) FALSE)))))

row("col_cell()",
    accepts("rtftable", "col_header", list(list(cc), c("A", "B")), data = df),
    FALSE,
    FALSE, FALSE,
    accepts("rtf_tables", "col_header", list(list(cc), c("A", "B")),
            doc = rtf_document(), tables = tb),
    paste0("set_header_cell:o  rtf_col_header:",
           m(isTRUE(tryCatch({ rtf_col_header(list(cc), c("A", "B")); TRUE },
                             error = function(e) FALSE)))))

row("blank_rows_by_change()",
    accepts("rtftable", "blank_rows", brc, data = df),
    FALSE, FALSE, FALSE,
    accepts("rtf_tables", "blank_rows", brc, doc = rtf_document(), tables = tb),
    paste0("plan_blanks:",
           m(isTRUE(tryCatch({ plan_blanks(rtf_plan(df), brc); TRUE },
                             error = function(e) FALSE))),
           "  as_rtftables:", m(accepts("as_rtftables", "blank_rows", brc,
                                        x = df))))

row("rtf_page()",
    FALSE, FALSE, FALSE, FALSE, FALSE,
    paste0("rtf_document:", m(accepts("rtf_document", "page", rtf_page())),
           "  rtf_config:", m(accepts("rtf_config", "page", rtf_page(),
                                      doc = rtf_document()))))

row("rtf_default_format()",
    FALSE, FALSE, FALSE, FALSE, FALSE,
    paste0("rtf_document:",
           m(accepts("rtf_document", "default_format", rtf_default_format())),
           "  rtf_config:",
           m(accepts("rtf_config", "default_format", rtf_default_format(),
                     doc = rtf_document()))))

cat("\n=== 同じ設定が2階層で指定でき、table 側が document 側を上書きするか ===\n")
for (a in c("font_size_half_points", "row_height_twips")) {
  cat(sprintf("  %-24s rtf_default_format:%s  rtftable:%s  plan_style:%s  rtf_tables:%s\n",
      a,
      m(a %in% names(formals(rtf_default_format))),
      m(a %in% names(formals(rtftable))),
      m(a %in% names(formals(plan_style))),
      m(a %in% names(formals(rtf_tables)))))
}

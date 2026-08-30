setwd("C:/Yrepo/rtfreporter")

# Read the CURRENT reference index straight out of _pkgdown.yml, so the draft
# below is a diff against what is actually published rather than my memory.
y <- readLines("_pkgdown.yml", warn = FALSE)
start <- grep("^reference:", y)
end   <- grep("^articles:", y)
ref   <- y[start:(end - 1L)]

secs <- list(); cur <- NULL
for (l in ref) {
  if (grepl("^  - title:", l)) {
    if (!is.null(cur)) secs[[length(secs) + 1L]] <- cur
    cur <- list(title = trimws(sub("^  - title:", "", l)), items = character(0))
  } else if (grepl("^      - ", l) && !is.null(cur)) {
    cur$items <- c(cur$items, trimws(sub("^      - ", "", l)))
  }
}
if (!is.null(cur)) secs[[length(secs) + 1L]] <- cur

RETIRE <- c("as_rtftables", "as_rtftable", "stub_spec", "page_split",
            "page_split_none", "page_split_rows", "page_split_group_safe",
            "page_split_group_force", "page_split_by_value")

PLAN <- c("rtf_plan_from", "plan_roles", "role", "plan_group", "plan_stub",
          "plan_hide", "plan_unset", "plan_blanks", "plan_pages",
          "plan_style", "plan_header", "plan_tables")

out <- c(
"# Reference 索引の草案 — plan 系を採用した場合",
"",
"現行の `_pkgdown.yml` から機械的に生成した**変更後の姿**です。",
"公開中の [Reference](https://ichirio.github.io/rtfreporter/reference/) と",
"見比べてください。",
"",
"凡例  **NEW** = 新設 / ~~取消線~~ = Deprecated へ移動 / 無印 = 変更なし",
"", "---", "")

for (s in secs) {
  items <- s$items
  if (length(items) == 0L) next
  ttl <- s$title
  if (grepl("^Importing tables", ttl)) {
    out <- c(out, sprintf("### %s", ttl),
             "",
             "> `as_rtftables()` / `as_rtftable()` / `stub_spec()` は plan 系に置き換わるため Deprecated へ移動。",
             "> `combine_sections()` と `stub_cols()` は残ります（`stub_cols()` は plan が内部で使用）。",
             "")
  } else if (grepl("^Pagination strategies", ttl)) {
    out <- c(out, sprintf("### %s", ttl),
             "",
             "> `page_split_*()` 5関数は `plan_pages()` に置き換わるため Deprecated へ移動。",
             "> `paginate_cols()` と `add_cont_label()` は残ります（plan 未対応のため必須）。",
             "")
  } else {
    out <- c(out, sprintf("### %s", ttl), "")
  }
  for (i in items) {
    mark <- if (i %in% RETIRE) sprintf("~~`%s()`~~", i) else sprintf("`%s()`", i)
    out <- c(out, paste0("- ", mark))
  }
  out <- c(out, "")

  # the new section slots in right after page content
  if (grepl("^Page content", ttl)) {
    out <- c(out,
      "### Building a table plan  **NEW**", "",
      "> 何を作るかを宣言し、解決は最後に1回だけ行う。",
      "> 列は**名前**で指定し、位置は解決時に1つの写像が決めます。",
      "",
      paste0("- `", PLAN, "()`  **NEW**"), "")
  }
}

# Deprecated section, rebuilt
out <- c(out, "---", "",
  "### Deprecated  （変更後）", "",
  "> いずれも動作は変わりません。ドキュメント上で後継を案内するだけです。",
  "",
  "| 関数 | 後継 |",
  "|---|---|",
  "| `as_rtftables()` | `rtf_plan_from()` |",
  "| `as_rtftable()` | `rtf_plan_from()`（単数形は `plan_tables(p)[[1]]`） |",
  "| `stub_spec()` | `plan_stub()` |",
  "| `page_split_none()` | `plan_pages()` |",
  "| `page_split_rows()` | `plan_pages(break_before =)` |",
  "| `page_split_group_safe()` | `plan_pages(groups = \"keep\")` |",
  "| `page_split_group_force()` | `plan_pages(groups = \"prefer\")` |",
  "| `page_split_by_value()` | `plan_pages(per_group = TRUE)` |",
  "| `paginate()` | `plan_pages()`（既に Deprecated） |",
  "",
  "**9関数のみ**が Deprecated になります。",
  "`set_col_header()` / `set_header_cell()` / `set_blank_rows()` /",
  "`collapse_repeats()` は plan でも宣言できますが、**完成テーブルへの",
  "後付け調整**という別の役割があるため、すべて残します。",
  "")

writeLines(out, "data-raw/review-samples/REFERENCE_DRAFT.md")
cat("wrote data-raw/review-samples/REFERENCE_DRAFT.md  (",
    length(out), "lines )\n")
cat("sections:", length(secs), "  retired marked:", length(RETIRE), "\n")

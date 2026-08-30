# Reference 索引の草案 — plan 系を採用した場合

現行の `_pkgdown.yml` から機械的に生成した**変更後の姿**です。
公開中の [Reference](https://ichirio.github.io/rtfreporter/reference/) と
見比べてください。

凡例  **NEW** = 新設 / ~~取消線~~ = Deprecated へ移動 / 無印 = 変更なし

---

### Document & rendering

- `rtf_document()`
- `rtf_config()`
- `rtf_page()`
- `rtf_default_format()`
- `generate_rtfreport()`
- `print.rtf_document()`
- `print.rtfreport()`

### Package defaults

- `rtfreporter_options()`
- `rtfreporter_reset_defaults()`

### Sections — headers & footers

- `rtf_section()`
- `rtf_header()`
- `rtf_footer()`
- `update_header_row()`
- `update_footer_row()`

### Page content — tables & figures

- `rtf_tables()`
- `rtf_figures()`
- `rtf_titles()`
- `rtf_footnotes()`
- `rtftable()`
- `rtfplot()`
- `print.rtftable()`
- `format.rtftable()`
- `summary.rtftable()`
- `print.rtfplot()`

### Building a table plan  **NEW**

> 何を作るかを宣言し、解決は最後に1回だけ行う。
> 列は**名前**で指定し、位置は解決時に1つの写像が決めます。

- `rtf_plan_from()`  **NEW**
- `plan_roles()`  **NEW**
- `role()`  **NEW**
- `plan_group()`  **NEW**
- `plan_stub()`  **NEW**
- `plan_hide()`  **NEW**
- `plan_unset()`  **NEW**
- `plan_blanks()`  **NEW**
- `plan_pages()`  **NEW**
- `plan_style()`  **NEW**
- `plan_header()`  **NEW**
- `plan_tables()`  **NEW**

### Importing tables (gt / gtsummary / rtables → rtftable)

> `as_rtftables()` / `as_rtftable()` / `stub_spec()` は plan 系に置き換わるため Deprecated へ移動。
> `combine_sections()` と `stub_cols()` は残ります（`stub_cols()` は plan が内部で使用）。

- ~~`as_rtftables()`~~
- ~~`as_rtftable()`~~
- `combine_sections()`
- `stub_cols()`
- ~~`stub_spec()`~~

### Column headers

- `col_cell()`
- `rtf_col_header()`
- `col_header_from_names()`
- `add_col_header_row()`
- `set_col_header()`
- `set_header_cell()`
- `rtf_columns()`
- `rtf_header_source()`

### Post-hoc styling verbs

- `style_header()`
- `collapse_repeats()`
- `set_decimal_split()`

### Built-in cell-format functions

- `format_count_pct()`
- `realign_count_pct()`
- `fmt_count_paren()`
- `fmt_count_paren_bare()`
- `fmt_right_align()`

### Numeric display formatters

- `fmt_signif()`
- `fmt_round()`
- `fmt_numeric()`

### Blank rows

- `set_blank_rows()`
- `blank_rows_by_change()`
- `blank_rows_by_rule()`

### Pagination strategies & helpers

> `page_split_*()` 5関数は `plan_pages()` に置き換わるため Deprecated へ移動。
> `paginate_cols()` と `add_cont_label()` は残ります（plan 未対応のため必須）。

- ~~`page_split()`~~
- `paginate_cols()`
- `add_cont_label()`

### Borders

- `rtf_border_side()`
- `rtf_border()`
- `rtf_border_with()`
- `rtf_border_none()`
- `rtf_border_top()`
- `rtf_border_bottom()`
- `rtf_border_box()`
- `rtf_table_border()`
- `rtf_border_tfl()`

### Shared table styles (S3, snapshot)

- `rtf_table_style()`
- `rtf_table_style_with()`
- `rtf_table_style_tfl()`

### Column-width utilities

- `text_width_in()`
- `auto_col_widths()`

### Visual preview (S3 plot methods)

- `plot.rtf_border()`
- `plot.rtf_border_side()`
- `plot.rtf_table_border()`
- `plot.rtftable()`
- `plot.rtf_document()`

### Assembling multiple RTF files

- `assemble_rtf()`
- `assemble_files()`
- `assemble_folder()`
- `assemble_spec()`
- `assemble_from_spec()`
- `assemble_toc()`
- `toc_heading()`
- `toc_entry()`

### Post-processing

- `rtf_replace_text()`

### Deprecated

- `paginate()`

---

### Deprecated  （変更後）

> いずれも動作は変わりません。ドキュメント上で後継を案内するだけです。

| 関数 | 後継 |
|---|---|
| `as_rtftables()` | `rtf_plan_from()` |
| `as_rtftable()` | `rtf_plan_from()`（単数形は `plan_tables(p)[[1]]`） |
| `stub_spec()` | `plan_stub()` |
| `page_split_none()` | `plan_pages()` |
| `page_split_rows()` | `plan_pages(break_before =)` |
| `page_split_group_safe()` | `plan_pages(groups = "keep")` |
| `page_split_group_force()` | `plan_pages(groups = "prefer")` |
| `page_split_by_value()` | `plan_pages(per_group = TRUE)` |
| `paginate()` | `plan_pages()`（既に Deprecated） |

**9関数のみ**が Deprecated になります。
`set_col_header()` / `set_header_cell()` / `set_blank_rows()` /
`collapse_repeats()` は plan でも宣言できますが、**完成テーブルへの
後付け調整**という別の役割があるため、すべて残します。


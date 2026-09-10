# Reference 索引の草案 — plan 系を採用した場合

現行の `_pkgdown.yml` から機械的に生成した**変更後の姿**です。
公開中の [Reference](https://ichirio.github.io/rtfreporter/reference/) と
見比べてください。

凡例  **NEW** = 新設 / ~~取消線~~ = Deprecated へ移動 / 無印 = 変更なし

---

### Start here — four complete recipes

- `rtfreporter-recipes()`

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

- `rtf_plan()`  **NEW**
- `plan_roles()`  **NEW**
- `role()`  **NEW**
- `plan_group()`  **NEW**
- `plan_stub()`  **NEW**
- `plan_hide()`  **NEW**
- `plan_unset()`  **NEW**
- `plan_listing()`  **NEW**
- `plan_blanks()`  **NEW**
- `plan_pages()`  **NEW**
- `plan_style()`  **NEW**
- `plan_header()`  **NEW**
- `plan_tables()`  **NEW**
- `rtf_pages()`  **NEW**

### Importing tables (gt / gtsummary / rtables / rlistings → rtftable)

> `as_rtftables()` / `as_rtftable()` / `stub_spec()` は plan 系に置き換わるため Deprecated へ移動。
> `combine_sections()` と `stub_cols()` は残ります（`stub_cols()` は plan が内部で使用）。

- ~~`as_rtftables()`~~
- ~~`as_rtftable()`~~
- `combine_sections()`
- `stub_cols()`
- ~~`stub_spec()`~~

### Listings (source data → listing body)

> **listing 系はそのまま残ります。** `listing_col()` / `listing_spec()` /
> `build_listing()` は「並べ方」を決める関数で、plan が置き換えるのは
> 入口の `as_rtftables(listing = )` だけです（→ `plan_listing()`）。
> `plan_listing()` は `listing_spec()` の11設定のうち8つだけを持ちます。
> `blank_row_first` は `plan_blanks(first = )`、`align` は
> `plan_roles(role(align = ))` に寄せたためです。

- `listing_col()`
- `listing_spec()`
- `build_listing()`
- `fit_listing_widths()`
- `listing_code()`
- `listing_wrap()`
- `listing_wrap_code()`
- `listing_measures()`
- `catx()`

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

> `page_split_*()` は #334 で廃止済み。plan 系では `plan_pages()` が
> その役目を持ちます。`paginate_cols()` と `add_cont_label()` は残ります
> （plan 未対応のため必須）。

- `paginate_cols()`
- `add_cont_label()`

### Borders

- `rtf_border_side()`
- `rtf_border()`

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

### Superseded

- `paginate()`

### Deprecated -- scheduled for removal

- `rtf_border_with()`
- `rtf_table_border()`
- `rtf_border_tfl()`

---

### Deprecated  （変更後）

> いずれも動作は変わりません。ドキュメント上で後継を案内するだけです。

| 関数 | 後継 |
|---|---|
| `as_rtftables()` | `rtf_plan()` + レイヤー |
| `as_rtftable()` | `rtf_plan()`（単数形は `rtf_pages(p)[[1]]`） |
| `stub_spec()` | `plan_stub()` |
| `paginate()` | `plan_pages()`（既に Deprecated） |

**3関数のみ**が Deprecated になります（`page_split_*()` の5つは #334 で
廃止済みなので、この表には残っていません）。
listing 系は1つも Deprecated になりません: `as_rtftables(listing = )` という
**引数**が `plan_listing()` に置き換わるだけです。
`set_col_header()` / `set_header_cell()` / `set_blank_rows()` /
`collapse_repeats()` は plan でも宣言できますが、**完成テーブルへの
後付け調整**という別の役割があるため、すべて残します。


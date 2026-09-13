# Example: Lab Toxicity Grade Shift Table (Table 14.3.x)

## 1 概要

**入力**: 成型済みシフトテーブル data.frame（各検査項目 1 つ） **出力**:
RTF ファイル（3 ページ、3
セクション、セクションごとに検査項目名をヘッダーに表示）

### 1.1 表の構造

                     | ─── Drug A (N=30) ─────────────────── | ─── Drug B (N=30) ─── | ─── Total (N=60) ─ |
                     | Worst Post-Baseline Toxicity Grade     |                       |                    |
    BL Grade         |  0   |  1   |  2   |  3   |  4   | Tot|  0  | ... | Tot | ...| Tot                |

- **列（Level 1, スパニング）**: Drug A / Drug B / Total
- **列（Level 2, 通常ヘッダー）**: Worst Post-BL Grade 0–4 + Total（各群
  6 列 × 3 = 18 列）
- **行**: Baseline Grade 0–4 + Total（6 行）
- **ページ / セクション**: 検査項目（HGB / ALT / CREAT）ごとに 1 ページ

## 2 データ読み込み

コードを表示

``` r

library(rtfreporter)

data_dir <- system.file("extdata", package = "rtfreporter")
if (!nzchar(data_dir)) data_dir <- file.path("..", "..", "inst", "extdata")

lab_hgb   <- readRDS(file.path(data_dir, "lab_hgb.rds"))
lab_alt   <- readRDS(file.path(data_dir, "lab_alt.rds"))
lab_creat <- readRDS(file.path(data_dir, "lab_creat.rds"))

# 検査項目の定義（ページ順に並べる）
lab_params <- list(
  list(paramcd = "HGB",   param = "Hemoglobin (g/dL)",       data = lab_hgb),
  list(paramcd = "ALT",   param = "ALT (U/L)",                data = lab_alt),
  list(paramcd = "CREAT", param = "Creatinine (umol/L)",      data = lab_creat)
)

cat(sprintf("Shift table: %d rows x %d cols\n", nrow(lab_hgb), ncol(lab_hgb)))
```

    Shift table: 6 rows x 19 cols

コードを表示

``` r

print(lab_hgb)
```

      BL_Grade    DrugA_G0   DrugA_G1   DrugA_G2  DrugA_G3 DrugA_G4    DrugA_Tot
    1        0 15\n(50.0%) 7\n(23.3%)  2\n(6.7%) 1\n(3.3%)        0  25\n(83.3%)
    2        1   1\n(3.3%)  2\n(6.7%)  1\n(3.3%) 1\n(3.3%)        0   5\n(16.7%)
    3        2           0          0          0         0        0            0
    4        3           0          0          0         0        0            0
    5        4           0          0          0         0        0            0
    6    Total 16\n(53.3%) 9\n(30.0%) 3\n(10.0%) 2\n(6.7%)        0 30\n(100.0%)
         DrugB_G0   DrugB_G1  DrugB_G2  DrugB_G3 DrugB_G4    DrugB_Tot    Total_G0
    1 18\n(60.0%) 5\n(16.7%) 2\n(6.7%) 1\n(3.3%)        0  26\n(86.7%) 33\n(55.0%)
    2   1\n(3.3%)  2\n(6.7%)         0 1\n(3.3%)        0   4\n(13.3%)   2\n(3.3%)
    3           0          0         0         0        0            0           0
    4           0          0         0         0        0            0           0
    5           0          0         0         0        0            0           0
    6 19\n(63.3%) 7\n(23.3%) 2\n(6.7%) 2\n(6.7%)        0 30\n(100.0%) 35\n(58.3%)
         Total_G1  Total_G2  Total_G3 Total_G4    Total_Tot
    1 12\n(20.0%) 4\n(6.7%) 2\n(3.3%)        0  51\n(85.0%)
    2   4\n(6.7%) 1\n(1.7%) 2\n(3.3%)        0   9\n(15.0%)
    3           0         0         0        0            0
    4           0         0         0        0            0
    5           0         0         0        0            0
    6 16\n(26.7%) 5\n(8.3%) 4\n(6.7%)        0 60\n(100.0%)

## 3 スパニングヘッダー定義

コードを表示

``` r

# 列構成（BL_Grade 含め 19 列）:
#   col 1         : BL_Grade
#   col 2 – 7    : Drug A  (G0, G1, G2, G3, G4, Total)
#   col 8 – 13   : Drug B  (G0, G1, G2, G3, G4, Total)
#   col 14 – 19  : Total   (G0, G1, G2, G3, G4, Total)

spanning_hdr <- list(
  list(from = 2L,  to = 7L,  label = "Drug A\n(N=30)",  underline = TRUE),
  list(from = 8L,  to = 13L, label = "Drug B\n(N=30)",  underline = TRUE),
  list(from = 14L, to = 19L, label = "Total\n(N=60)",   underline = TRUE)
)

# Level 2 列ヘッダー（繰り返し）
grade_labels  <- c("0", "1", "2", "3", "4", "Total")
col_hdr_row   <- c("Baseline\nGrade", rep(grade_labels, 3L))

cat("Column header (Level 2):\n")
```

    Column header (Level 2):

コードを表示

``` r

print(col_hdr_row)
```

     [1] "Baseline\nGrade" "0"               "1"               "2"
     [5] "3"               "4"               "Total"           "0"
     [9] "1"               "2"               "3"               "4"
    [13] "Total"           "0"               "1"               "2"
    [17] "3"               "4"               "Total"          

## 4 列幅設定

コードを表示

``` r

# ランドスケープ letter: 書き込み幅 10 inch = 14400 twips
# BL_Grade: 1200 twips (≈ 0.83 in)
# 18 grade cols: (14400 - 1200) / 18 = 733 twips each
col_w_bl    <- 1200L
col_w_grade <- 733L
col_widths  <- c(col_w_bl, rep(col_w_grade, 18L))
cat(sprintf("Total width: %d twips = %.2f inch\n",
            sum(col_widths), sum(col_widths) / 1440))
```

    Total width: 14394 twips = 10.00 inch

## 5 rtftable 生成関数

コードを表示

``` r

make_shift_tbl <- function(df) {
  rtftable(
    data                = df,
    col_header          = col_hdr_row,
    spanning_header     = spanning_hdr,
    column_widths_twips = col_widths,
    col_spec            = c(
      list(list(col = 1L, align = "left")),
      lapply(2:19, \(j) list(col = j, align = "center"))
    ),
    row_height_twips    = 360L,   # 2 行テキスト対応（n\n(x.x%)）
    border              = "tfl",
    table_align         = "left"
  )
}

# 全検査項目の rtftable を生成
shift_tbls <- lapply(lab_params, \(p) make_shift_tbl(p$data))
names(shift_tbls) <- sapply(lab_params, \(p) p$paramcd)
```

## 6 ヘッダー定義（共通 + 検査項目別）

コードを表示

``` r

# ── 共通ヘッダー2行 ────────────────────────────────────────────────────────────
common_hdr_rows <- list(
  c(l = "Protocol: STUDY001",
    r = "Drug Co., Ltd."),
  c(l = "Table 14.3.x  Toxicity Grade Shift Table",
    r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}")
)

# ── 検査項目別ヘッダー生成 ─────────────────────────────────────────────────────
# update_header_row() で共通ヘッダーの3行目に検査名を追加
make_lab_hdr <- function(param_label) {
  hdr <- rtf_header(rows = common_hdr_rows)
  update_header_row(hdr, row = 3L, content = c(c = param_label))
}

lab_hdrs <- lapply(lab_params, \(p) make_lab_hdr(p$param))
names(lab_hdrs) <- sapply(lab_params, \(p) p$paramcd)

# 確認: HGB ヘッダーの行構成
cat("HGB header rows:\n")
```

    HGB header rows:

コードを表示

``` r

for (i in seq_along(lab_hdrs$HGB$rows)) {
  cat(sprintf("  [%d] %s\n", i,
              paste(names(lab_hdrs$HGB$rows[[i]]),
                    lab_hdrs$HGB$rows[[i]], sep = "=", collapse = "  |  ")))
}
```

      [1] l=Protocol: STUDY001  |  r=Drug Co., Ltd.
      [2] l=Table 14.3.x  Toxicity Grade Shift Table  |  r=Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}
      [3] c=Hemoglobin (g/dL)

コードを表示

``` r

# ── 共通フッター ──────────────────────────────────────────────────────────────
common_ftr <- rtf_footer(rows = list(
  c(l = paste0("Run date: ", Sys.Date()),
    c = "CONFIDENTIAL",
    r = "DRAFT")
))
```

## 7 RTF 出力

コードを表示

``` r

# 3 ページ × 3 セクション（各セクションに検査項目名入りヘッダー）
page_cfg <- list(orientation = "landscape",
                 width_in = 11, height_in = 8.5,
                 margin_top_in = 0.75, margin_bottom_in = 0.75,
                 margin_left_in = 0.5, margin_right_in = 0.5)

doc <- rtf_document() |> rtf_config(page = page_cfg)

for (i in seq_along(lab_params)) {
  pcd <- lab_params[[i]]$paramcd
  doc <- doc |>
    rtf_tables(list(shift_tbls[[pcd]])) |>
    rtf_section(page    = i,
                secinfo = list(header = lab_hdrs[[pcd]],
                               footer = common_ftr))
}

out_file <- tempfile(fileext = ".rtf")
generate_rtfreport(doc, out_file, overwrite = TRUE)
cat("RTF generated:", out_file, "\n")
```

> `eval=FALSE` のため上記チャンクはデフォルトで実行されません。

## 8 HTML プレビュー

コードを表示

``` r

library(knitr)
library(kableExtra)

preview_shift <- function(df, param_label, N_A = 30L, N_B = 30L, N_tot = 60L) {
  arm_hdr <- c(
    sprintf("Drug A (N=%d)", N_A),
    sprintf("Drug B (N=%d)", N_B),
    sprintf("Total (N=%d)", N_tot)
  )
  gr_hdr <- c("0", "1", "2", "3", "4", "Total")

  col_names <- c("BL Grade", paste0(rep(arm_hdr, each = 6L), "\nGr.", gr_hdr))

  kable(df, caption = param_label, col.names = col_names,
        align = c("l", rep("c", 18L))) |>
    kable_styling(bootstrap_options = c("striped", "condensed"), font_size = 9) |>
    add_header_above(c(
      " "          = 1L,
      setNames(6L, arm_hdr[1L]),
      setNames(6L, arm_hdr[2L]),
      setNames(6L, arm_hdr[3L])
    )) |>
    row_spec(nrow(df), bold = TRUE, background = "#d0e8d0")  # Total行
}
```

コードを表示

``` r

preview_shift(lab_hgb, "Hemoglobin (g/dL)")
```

[TABLE]

Hemoglobin (g/dL) {.table .table .table-striped .table-condensed
.caption-top
style="font-size: 9px; margin-left: auto; margin-right: auto;"}

コードを表示

``` r

preview_shift(lab_alt, "ALT (U/L)")
```

[TABLE]

ALT (U/L) {.table .table .table-striped .table-condensed .caption-top
style="font-size: 9px; margin-left: auto; margin-right: auto;"}

コードを表示

``` r

preview_shift(lab_creat, "Creatinine (umol/L)")
```

[TABLE]

Creatinine (umol/L) {.table .table .table-striped .table-condensed
.caption-top
style="font-size: 9px; margin-left: auto; margin-right: auto;"}

## 9 設計のポイント

### 9.1 検査項目別セクションヘッダー

[`update_header_row()`](https://ichirio.github.io/rtfreporter/reference/update_header_row.md)
を使い、共通 2 行に検査名を 3 行目として追加します。

``` r

make_lab_hdr <- function(param_label) {
  hdr <- rtf_header(rows = list(
    c(l = "Protocol: STUDY001",       r = "Company"),
    c(l = "Table 14.3.x Shift Table", r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}")
  ))
  update_header_row(hdr, row = 3L, content = c(c = param_label))
}
```

RTF では `\sect` でセクションが切り替わるたびに `{\header}`
が出力されるため、 各ページのヘッダー 3
行目が自動的に検査項目名に切り替わります。

### 9.2 2 階層ヘッダー（スパニング + 列ヘッダー）

``` r

# Level 1 (spanning): Drug A / Drug B / Total
spanning_header = list(
  list(from = 2L, to = 7L,  label = "Drug A\n(N=30)", underline = TRUE),
  list(from = 8L, to = 13L, label = "Drug B\n(N=30)", underline = TRUE),
  list(from = 14L, to = 19L, label = "Total\n(N=60)", underline = TRUE)
)

# Level 2 (col_header): Baseline Grade | 0 1 2 3 4 Total × 3
col_header = c("Baseline\nGrade", rep(c("0","1","2","3","4","Total"), 3L))
```

### 9.3 セル内 2 行テキスト（`\n`）

シフトテーブルのセル値は `"12\n(40.0%)"` 形式で、 rtfreporter がセル内の
`\n` を RTF の `\line` に変換します。 `row_height_twips = 360L` で 2
行分の高さを確保しています。

| 設定                     | 値                                 |
|--------------------------|------------------------------------|
| `row_height_twips`       | `360` twips（≈ 0.25 inch、2 行分） |
| `col_widths[BL_Grade]`   | `1200` twips（≈ 0.83 inch）        |
| `col_widths[grade_cols]` | `733` twips × 18（≈ 0.51 inch 各） |
| 合計幅                   | `14394` twips（≈ 10.0 inch）       |

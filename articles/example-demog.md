# Example: Demographics Table (Table 14.1.1)

## 1 概要

**入力**: 統計処理済みの data.frame（`demog_p1`, `demog_p2`） **出力**:
RTF ファイル（1 セクション、共通ヘッダー/フッター、2 ページ）

ページ分割はカテゴリ単位で行い、最大 26 行を超えないようにあらかじめ
`demog_p1`（25 行）と `demog_p2`（19 行）に分割済みです。

## 2 データ読み込み

コードを表示

``` r

library(rtfreporter)

data_dir <- system.file("extdata", package = "rtfreporter")
if (!nzchar(data_dir)) data_dir <- file.path("..", "..", "inst", "extdata")

demog_p1 <- readRDS(file.path(data_dir, "demog_p1.rds"))
demog_p2 <- readRDS(file.path(data_dir, "demog_p2.rds"))

cat(sprintf("Page 1: %d rows x %d cols\n", nrow(demog_p1), ncol(demog_p1)))
```

    Page 1: 25 rows x 4 cols

コードを表示

``` r

cat(sprintf("Page 2: %d rows x %d cols\n", nrow(demog_p2), ncol(demog_p2)))
```

    Page 2: 19 rows x 4 cols

コードを表示

``` r

# データ構造の確認（先頭 8 行）
head(demog_p1, 8)
```

                 Label      Drug A      Drug B       Total
    1 Age Group, n (%)
    2              <65  10 (33.3%)  12 (40.0%)  22 (36.7%)
    3           65-<75  14 (46.7%)  12 (40.0%)  26 (43.3%)
    4             >=75   6 (20.0%)   6 (20.0%)  12 (20.0%)
    5      Age (years)
    6                N          30          30          60
    7        Mean (SD) 63.2 (9.14) 67.0 (8.31) 65.1 (8.85)
    8           Median        64.0        67.5        66.0

## 3 ヘッダー / フッター定義

コードを表示

``` r

# ── 共通ヘッダー（2 行） ───────────────────────────────────────────────────────
hdr <- rtf_header(rows = list(
  c(l = "Protocol: STUDY001",
    r = "Drug Co., Ltd."),
  c(l = "Table 14.1.1  Summary of Demographic and Baseline Characteristics",
    r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}")
))

# ── 共通フッター（1 行、上辺罫線） ────────────────────────────────────────────
ftr <- rtf_footer(rows = list(
  c(l = paste0("Run date: ", Sys.Date()),
    c = "CONFIDENTIAL",
    r = "DRAFT")
))
```

## 4 rtftable 定義

コードを表示

``` r

# ランドスケープ letter: 書き込み幅 ≈ 10 inch = 14400 twips
# Label: 4320 (3.0 in), 各群: 3360 (2.33 in) × 3 = 14400
col_widths <- c(4320L, 3360L, 3360L, 3360L)

col_hdr <- c(
  "Characteristic",
  "Drug A\n(N=30)",
  "Drug B\n(N=30)",
  "Total\n(N=60)"
)

make_demog_tbl <- function(df) {
  rtftable(
    data                = df,
    col_header          = col_hdr,
    column_widths_twips = col_widths,
    col_spec            = list(
      list(col = 1L, align = "left"),
      list(col = 2L, align = "center"),
      list(col = 3L, align = "center"),
      list(col = 4L, align = "center")
    ),
    row_height_twips    = 240L,
    border              = "tfl"
  )
}

tbl_p1 <- make_demog_tbl(demog_p1)
tbl_p2 <- make_demog_tbl(demog_p2)
```

## 5 RTF 出力

コードを表示

``` r

# ── 1 セクション、2 ページ ────────────────────────────────────────────────────
page_cfg <- list(orientation = "landscape",
                 width_in = 11, height_in = 8.5,
                 margin_top_in = 0.75, margin_bottom_in = 0.75,
                 margin_left_in = 0.5, margin_right_in = 0.5)

doc <- rtf_document() |>
  rtf_config(page = page_cfg) |>
  rtf_tables(list(tbl_p1)) |>            # page 1
  rtf_tables(list(tbl_p2)) |>            # page 2
  rtf_section(page   = 1:2,
              secinfo = list(header = hdr, footer = ftr))

out_file <- tempfile(fileext = ".rtf")
generate_rtfreport(doc, out_file, overwrite = TRUE)
cat("RTF generated:", out_file, "\n")
```

> `eval=FALSE` のため上記チャンクはデフォルトで実行されません。
> 実行するには `eval=TRUE`
> に変更するか、コンソールで直接実行してください。

## 6 HTML プレビュー

コードを表示

``` r

library(knitr)
library(kableExtra)

fmt_tbl <- function(df, caption) {
  kable(df, caption = caption,
        col.names = c("Characteristic", "Drug A (N=30)", "Drug B (N=30)", "Total (N=60)"),
        align = c("l", "c", "c", "c")) |>
    kable_styling(bootstrap_options = c("striped", "condensed"), font_size = 12) |>
    row_spec(
      which(df$`Drug A` == ""),  # ヘッダー行を強調
      bold = TRUE, background = "#e8e8e8"
    )
}

fmt_tbl(demog_p1, "Page 1: Age Group ~ Weight Group")
```

| Characteristic            | Drug A (N=30) | Drug B (N=30) | Total (N=60) |
|:--------------------------|:-------------:|:-------------:|:------------:|
| Age Group, n (%)          |               |               |              |
| \<65                      |  10 (33.3%)   |  12 (40.0%)   |  22 (36.7%)  |
| 65-\<75                   |  14 (46.7%)   |  12 (40.0%)   |  26 (43.3%)  |
| \>=75                     |   6 (20.0%)   |   6 (20.0%)   |  12 (20.0%)  |
| Age (years)               |               |               |              |
| N                         |      30       |      30       |      60      |
| Mean (SD)                 |  63.2 (9.14)  |  67.0 (8.31)  | 65.1 (8.85)  |
| Median                    |     64.0      |     67.5      |     66.0     |
| Q1, Q3                    |  57.0, 70.0   |  61.5, 73.5   |  59.5, 71.5  |
| Min, Max                  |    43, 79     |    48, 85     |    43, 85    |
| Sex, n (%)                |               |               |              |
| Male                      |  17 (56.7%)   |  16 (53.3%)   |  33 (55.0%)  |
| Female                    |  13 (43.3%)   |  14 (46.7%)   |  27 (45.0%)  |
| Race, n (%)               |               |               |              |
| White                     |  19 (63.3%)   |  20 (66.7%)   |  39 (65.0%)  |
| Black or African American |   5 (16.7%)   |   4 (13.3%)   |  9 (15.0%)   |
| Asian                     |   4 (13.3%)   |   5 (16.7%)   |  9 (15.0%)   |
| Other                     |   2 (6.7%)    |   1 (3.3%)    |   3 (5.0%)   |
| Ethnicity, n (%)          |               |               |              |
| Hispanic or Latino        |   6 (20.0%)   |   6 (20.0%)   |  12 (20.0%)  |
| Not Hispanic or Latino    |  24 (80.0%)   |  24 (80.0%)   |  48 (80.0%)  |
| Weight Group (kg), n (%)  |               |               |              |
| \<60 kg                   |   4 (13.3%)   |   3 (10.0%)   |  7 (11.7%)   |
| 60-\<80 kg                |  17 (56.7%)   |  18 (60.0%)   |  35 (58.3%)  |
| \>=80 kg                  |   9 (30.0%)   |   9 (30.0%)   |  18 (30.0%)  |

Page 1: Age Group ~ Weight Group {.table .table .table-striped
.table-condensed .caption-top
style="font-size: 12px; margin-left: auto; margin-right: auto;"}

コードを表示

``` r

fmt_tbl(demog_p2, "Page 2: Weight ~ Prior Treatment")
```

| Characteristic              | Drug A (N=30) | Drug B (N=30) | Total (N=60) |
|:----------------------------|:-------------:|:-------------:|:------------:|
| Weight (kg)                 |               |               |              |
| N                           |      30       |      30       |      60      |
| Mean (SD)                   | 74.8 (12.08)  | 75.2 (11.83)  | 75.0 (11.93) |
| Median                      |     74.5      |     75.0      |     74.8     |
| Q1, Q3                      |  66.0, 83.0   |  67.0, 83.5   |  66.5, 83.0  |
| Min, Max                    |  51.2, 98.7   |  52.3, 97.1   |  51.2, 98.7  |
| BMI Group (kg/m^{2}), n (%) |               |               |              |
| \<25                        |   8 (26.7%)   |   9 (30.0%)   |  17 (28.3%)  |
| 25-\<30                     |  13 (43.3%)   |  12 (40.0%)   |  25 (41.7%)  |
| \>=30                       |   9 (30.0%)   |   9 (30.0%)   |  18 (30.0%)  |
| BMI (kg/m^{2})              |               |               |              |
| N                           |      30       |      30       |      60      |
| Mean (SD)                   |  26.5 (4.21)  |  26.7 (3.97)  | 26.6 (4.08)  |
| Median                      |     26.1      |     26.4      |     26.2     |
| Q1, Q3                      |  23.1, 29.5   |  23.5, 29.8   |  23.3, 29.6  |
| Min, Max                    |  19.2, 36.8   |  19.8, 37.2   |  19.2, 37.2  |
| Prior Treatment, n (%)      |               |               |              |
| Yes                         |  12 (40.0%)   |  12 (40.0%)   |  24 (40.0%)  |
| No                          |  18 (60.0%)   |  18 (60.0%)   |  36 (60.0%)  |

Page 2: Weight ~ Prior Treatment {.table .table .table-striped
.table-condensed .caption-top
style="font-size: 12px; margin-left: auto; margin-right: auto;"}

## 7 設計のポイント

### 7.1 1 セクション = 共通ヘッダー/フッター

``` r

rtf_section(pages = 1:2, header = hdr, footer = ftr)
```

全 2 ページに同一のヘッダー/フッターが適用されます。 `{AUTO_PAGE}` /
`{AUTO_TOTAL_PAGES}` はビューワーが動的に解決します。

### 7.2 ページ分割の考え方

ページ分割機能（カテゴリ途中での分割禁止）は現時点で未実装です。
代わりに **カテゴリ単位であらかじめ data.frame を分割** し、
ページごとに
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
で追加することで同等の結果を得ています。

|  | Page 1 | Page 2 |
|----|----|----|
| 含むカテゴリ | Age Group, Age, Sex, Race, Ethnicity, Weight Group | Weight, BMI Group, BMI, Prior Treatment |
| 行数 | 25 行 | 19 行 |
| 上限 | 26 行 | 26 行 |

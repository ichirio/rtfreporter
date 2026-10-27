# 1 ページに複数のコンテンツを置く

[In
English](https://ichirio.github.io/rtfreporter/articles/several-contents.md)

帳票では、1 ページに 2
つのコンテンツを置くことがよくあります。集計表とその元のリスティング、
表とその図、短い表 2 つなどです。rtfreporter では
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
や
[`rtf_figures()`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.md)
を 1 回呼ぶごとにコンテンツが 1 つ足され、`page_break = FALSE`
を付けると、新しいページではなく
前のコンテンツの下に続きます。この記事では、そうした帳票を 4
つ作り、その決まりを説明します。

``` r

library(rtfreporter)
```

## 引数

| 引数 | 関数 | 働き |
|----|----|----|
| `page_break` | [`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md), [`rtf_figures()`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.md) | `TRUE`（既定）: コンテンツは新しいページから始まる。`FALSE`: 前のコンテンツの下に続く。 |
| `space_before` | 両方 | `page_break = FALSE` の時、2 つの間の空行の数。 |
| `titles`, `footnotes` | 両方 | そのコンテンツのタイトル（上）と脚注（下）。 |
| `first_rows` | [`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md) | `page_break = FALSE` の時、共有するページに載せる表の行数。残りは表のページ分け（plan の [`plan_paginate_rows()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)）のとおりに分かれ、各ページにタイトル・列見出し・脚注が付く。 |
| `cont_title` | [`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md) | `first_rows` の時、2 ページ目からのタイトルの下に足す 1 行（`"(continued)"` など）。 |

ページのヘッダーとフッター（`rtf_section(header = , footer = )`）は、コンテンツではなくページの物です。
何が載っていても、どのページにも出ます。

**1 ページに収めるのはプログラムの役目です。** rtfreporter
はページの高さを測りません。
入りきらないコンテンツは、読む側（Word、LibreOffice）が次のページへ送ります。そのページには
タイトルも列見出しも脚注も付かず、ファイルに書いたページ番号（`{PAGE}` /
`{TOTAL_PAGES}`）も ずれます。入る量を数えてください。1
つ目のコンテンツを短くする、図に高さを与える、長い表には `first_rows`
を付ける、などです。この記事の後半の
[`rtf_layout()`](https://ichirio.github.io/rtfreporter/reference/rtf_layout.md)
で、rtfreporter が書き出す ページを確かめられます。

## (a) 表の下に表

同じ集団の 2 つの集計です。それぞれ自分の ARD と plan
から作ります。人口統計と、被験者の内訳（中止理由など）です。

Show code

``` r

adsl <- cards::ADSL
adsl$ARM <- factor(adsl$ARM, levels = c("Placebo", "Xanomeline Low Dose",
                                        "Xanomeline High Dose"))
ard_dm <- cards::ard_stack(
  adsl, .by = ARM,
  cards::ard_summary(variables = AGE),
  cards::ard_tabulate(variables = c(AGEGR1, SEX)))
ard_ds <- cards::ard_stack(
  adsl, .by = ARM,
  cards::ard_tabulate(variables = DCDECOD))

# the column header both tables share: the arm and its N
arm_header <- rtf_col_header(c("", "{col}"), c("", "(N={n})"))
dm <- ard_dm |>
  normalize_ard() |>
  table_plan(cols = "ARM", rows = c(group = "variable")) |>
  plan_cells(continuous = c("n" = "{N:.0f}", "Mean (SD)" = "{mean:.1f} ({sd:.2f})",
                            "Min, Max" = "{min:.0f}, {max:.0f}"),
             categorical = "{n:.0f} ({p:.1f%})", notes = FALSE) |>
  plan_labels(c(AGE = "Age (years)", AGEGR1 = "Age group, n (%)", SEX = "Sex, n (%)")) |>
  plan_levels(AGEGR1 = c("<65", "65-80", ">80"), SEX = c("F", "M")) |>
  plan_col_header(values = list(n = TRUE), arm_header) |>
  plan_stub(name = "row_label") |>
  plan_columns(widths = c(4, 2, 2, 2)) |>
  plan_style(border = "tfl", align_count_pct = TRUE)
ds <- ard_ds |>
  normalize_ard() |>
  table_plan(cols = "ARM", rows = c(group = "variable")) |>
  plan_cells(categorical = "{n:.0f} ({p:.1f%})", notes = FALSE) |>
  plan_labels(c(DCDECOD = "Disposition, n (%)")) |>
  plan_col_header(values = list(n = TRUE), arm_header) |>
  plan_stub(name = "row_label") |>
  plan_columns(widths = c(4, 2, 2, 2)) |>
  plan_style(border = "tfl", align_count_pct = TRUE)
```

1 つ目の
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
がページを始め、`page_break = FALSE` の 2 つ目はその下に、 空行 1
つをはさんで続きます:

Show code

``` r

header <- rtf_header(list(c(l = "Protocol: CDISCPILOT01", r = "Page {PAGE} of {TOTAL_PAGES}")))
doc_a <- rtf_document() |>
  rtf_section(header = header) |>
  rtf_tables(dm, titles = list(c("Table 14.1.1",
                                 "Demographics and Disposition",
                                 "Intent-to-Treat Population")),
             footnotes = list("Percentages are of the subjects in the arm.")) |>
  rtf_tables(ds, page_break = FALSE, space_before = 1L,
             titles = list("Disposition"),
             footnotes = list("Source: ADSL."))
generate_rtfreport(doc_a, "a_table_table.rtf", overwrite = TRUE)
```

![1
ページに人口統計の表と、その下に内訳の表](img/several-contents/a_table_table.png)

1 ページに人口統計の表と、その下に内訳の表

どちらの表も自分のタイトルと脚注を持ちます。出力全体のタイトル（表番号、表題、集団）は
1
つ目のコンテンツのタイトルに書きます。出力全体の脚注は最後のコンテンツの脚注に書きます。
どのページにも出したい時は、ページのフッターに書きます。

## (b) 表の下に図

[`rtf_figures()`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.md)
も同じ `page_break` と `space_before`
を取ります。図の高さは、ページの残りに合わせて決めます:

Show code

``` r

library(ggplot2)
age_plot <- ggplot(adsl, aes(ARM, AGE)) +
  geom_boxplot() +
  labs(x = NULL, y = "Age (years)") +
  theme_bw(base_size = 9)
doc_b <- rtf_document() |>
  rtf_section(header = header) |>
  rtf_tables(dm, titles = list(c("Table 14.1.2", "Age by Arm",
                                 "Intent-to-Treat Population"))) |>
  rtf_figures(age_plot, page_break = FALSE, space_before = 1L,
              render_width = 6, render_height = 2.6,
              titles = list("Figure: age by arm"),
              footnotes = list("Box: quartiles; whiskers: 1.5 IQR."))
generate_rtfreport(doc_b, "b_table_figure.rtf", overwrite = TRUE)
```

![人口統計の表と、その下に群ごとの年齢の箱ひげ図](img/several-contents/b_table_figure.png)

人口統計の表と、その下に群ごとの年齢の箱ひげ図

## (c) PK 濃度の集計表とリスティング

時点ごとの濃度の集計表と、その下に同じ濃度のリスティングを置きます。どちらも時点を横に並べ、
集計表の行は統計量、リスティングの行は被験者です。データはここで作る合成の物です。24
人、8 時点、 1-コンパートメントの曲線に誤差を足し、2 ng/mL 未満は BLQ
です。この例ではそのうち 5 人を使います。
時点を横に並べるので、集計表とリスティングが 1 ページに収まります。(d)
では 24 人全員を使います。

Show code

``` r

set.seed(20261028)
n_subj <- 24
tpt <- data.frame(ATPTN = c(0, 0.5, 1, 2, 4, 8, 12, 24),
                  ATPT  = c("Pre-dose", "0.5 h", "1 h", "2 h", "4 h", "8 h", "12 h", "24 h"))
subj <- data.frame(USUBJID = sprintf("DEMO-01-%03d", seq_len(n_subj)),
                   ka   = stats::rlnorm(n_subj, log(1.2), 0.25),
                   ke   = stats::rlnorm(n_subj, log(0.12), 0.25),
                   dose = stats::rlnorm(n_subj, log(400), 0.20))
adpc <- merge(subj, tpt, by = NULL)
adpc <- adpc[order(adpc$USUBJID, adpc$ATPTN), ]
conc <- with(adpc, dose * ka / (ka - ke) * (exp(-ke * ATPTN) - exp(-ka * ATPTN)))
conc <- conc * stats::rlnorm(nrow(adpc), 0, 0.12)
adpc$ARELTM <- round(pmax(0, adpc$ATPTN +
  ifelse(adpc$ATPTN == 0, -0.1, stats::runif(nrow(adpc), -0.08, 0.12))), 2)
blq <- adpc$ATPTN == 0 | conc < 2
adpc$AVAL  <- ifelse(blq, NA_real_, round(conc, 1))
adpc$AVALC <- ifelse(blq, "BLQ", format(round(conc, 1), nsmall = 1, trim = TRUE))
adpc$ATPT  <- factor(adpc$ATPT, levels = tpt$ATPT)
```

集計表は時点ごとの ARD から作り、時点を横に並べます。BLQ
の検体は除き、投与前（全員 BLQ）の列は ありません。plan は 1
つの関数で作り、(d) では 24 人全員に使います:

Show code

``` r

cv <- function(x) 100 * stats::sd(x) / mean(x)
gm <- function(x) exp(mean(log(x)))
pk_summary_plan <- function(data, cells) {
  ard <- cards::ard_summary(
    droplevels(data[!is.na(data$AVAL), ]), by = ATPT, variables = AVAL,
    statistic = ~ list(N = length, mean = mean, sd = stats::sd, cv = cv,
                       geo_mean = gm, median = stats::median, min = min, max = max))
  ard |>
    normalize_ard() |>
    table_plan(cols = "ATPT", rows = c(group = "variable")) |>
    plan_cells(continuous = cells, notes = FALSE) |>
    plan_labels(c(AVAL = "Concentration (ng/mL)")) |>
    plan_levels(ATPT = tpt$ATPT[-1]) |>
    plan_col_header(rtf_col_header(c("Drug X (ng/mL)", "{col}"))) |>
    plan_stub(name = "row_label") |>
    plan_columns(widths = c(3, rep(1, nrow(tpt) - 1))) |>
    plan_style(border = "tfl")
}
pk_cells <- c("n" = "{N:.0f}", "Mean" = "{mean:.1f}", "SD" = "{sd:.2f}",
              "CV%" = "{cv:.1f}", "Geometric mean" = "{geo_mean:.1f}",
              "Median" = "{median:.1f}", "Min" = "{min:.1f}", "Max" = "{max:.1f}")
pk5 <- adpc[adpc$USUBJID %in% unique(adpc$USUBJID)[1:5], ]
summary5 <- pk_summary_plan(pk5, pk_cells)
```

リスティングは集計表と同じ形にします。行が被験者、列が時点、セルが濃度です（下限未満は
`BLQ`）。
ふつうのデータフレームで、列名が見出しになります。列幅を集計表と同じにして、2
つの列をそろえます:

Show code

``` r

post <- pk5[pk5$ATPT != "Pre-dose", ]
listing5 <- data.frame(Subject = unique(post$USUBJID))
for (t in tpt$ATPT[-1]) {
  at <- post[post$ATPT == t, ]
  listing5[[t]] <- at$AVALC[match(listing5$Subject, at$USUBJID)]
}
listing5_plan <- table_plan(listing5) |>
  plan_columns(widths = c(3, rep(1, nrow(tpt) - 1))) |>
  plan_style(border = "tfl")
listing5
```

    #>       Subject 0.5 h   1 h   2 h   4 h   8 h  12 h 24 h
    #> 1 DEMO-01-001 159.4 196.4 269.6 309.8 221.9 169.1 55.6
    #> 2 DEMO-01-002 129.4 195.0 318.7 267.4 186.7 116.7 29.2
    #> 3 DEMO-01-003 135.9 248.6 332.5 222.1 151.5 111.8 28.5
    #> 4 DEMO-01-004 107.8 185.4 283.5 266.9 147.9 133.0 34.6
    #> 5 DEMO-01-005 187.8 242.3 292.8 334.4 179.0 117.8 26.5

集計表がページを始めて出力のタイトルを持ち、リスティングはその下に続きます:

Show code

``` r

pk_header <- rtf_header(list(
  c(l = "Protocol: {STUDY} (synthetic data)", r = "Page {PAGE} of {TOTAL_PAGES}")))
pk_footer <- rtf_footer(list(
  c(l = "BLQ: below the lower limit of quantification (2 ng/mL); left out of the summary."),
  c(l = "Pre-dose: BLQ for every subject. Synthetic data made for this page.")))
doc_c <- rtf_document(tokens = list(STUDY = "DEMO-01")) |>
  rtf_section(header = pk_header, footer = pk_footer) |>
  rtf_tables(summary5,
             titles = list(c("Table 14.4.1",
                             "Drug X Plasma Concentrations: Summary and Listing",
                             "Safety Population"))) |>
  rtf_tables(listing5_plan, page_break = FALSE, space_before = 1L,
             titles = list("Listing of concentrations (ng/mL)"))
generate_rtfreport(doc_c, "c_pk.rtf", overwrite = TRUE)
```

![5 人の PK
濃度の集計表と、その下に時点ごとの濃度のリスティング](img/several-contents/c_pk.png)

5 人の PK 濃度の集計表と、その下に時点ごとの濃度のリスティング

## (d) 長いリスティングの続き

24 人全員では、リスティングは集計表の下の残りより長くなります。ここでは
1 行が 1 検体のふつうの表として
書きます。列は印刷する形にし、列名を見出しにします。[`plan_row_group()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
は被験者番号を最初の行だけに出し
（ページの頭では出し直す）、[`plan_blanks()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
は被験者の間に空行を入れ、[`plan_paginate_rows()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
は被験者の 途中で切らずに 1 ページ 24 行（3 人分）に分けます:

Show code

``` r

pk_summary <- pk_summary_plan(adpc, pk_cells)
lst <- data.frame(Subject = adpc$USUBJID,
                  `Nominal time` = as.character(adpc$ATPT),
                  `Actual time (h)` = format(adpc$ARELTM, nsmall = 2),
                  `Concentration (ng/mL)` = adpc$AVALC,
                  check.names = FALSE)
pk_listing <- table_plan(lst) |>
  plan_row_group(mode = "value", collapse = "Subject", group_col = "Subject") |>
  plan_blanks(where = "between_groups") |>
  plan_paginate_rows(max_rows = 24, split = "group_safe") |>
  plan_columns(widths = c(2, 1.5, 1.5, 2)) |>
  plan_style(border = "tfl")
```

（[`plan_listing()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
と
[`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md)
は、有害事象のように 1 件が何行にも折り返すリスティング向けで、 1
件ごとに空行が入ります。[Listings with a
plan](https://ichirio.github.io/rtfreporter/articles/plan-listings.md)
を見てください。）

`first_rows`
で、共有するページに載せるリスティングの行数を書きます（データの行数で、被験者の間の空行は
数えません）。残りは plan
のページ分けのとおりに自分のページへ続き、各ページにリスティングのタイトル、
`cont_title` の行、列見出し、脚注が付きます:

Show code

``` r

doc_pk <- rtf_document(tokens = list(STUDY = "DEMO-01")) |>
  rtf_section(header = pk_header, footer = pk_footer) |>
  rtf_tables(pk_summary,
             titles = list(c("Table 14.4.2",
                             "Drug X Plasma Concentrations: Summary and Listing",
                             "Safety Population"))) |>
  rtf_tables(pk_listing, page_break = FALSE, space_before = 1L,
             first_rows = 16L, cont_title = "(continued)",
             titles = list("Listing of concentrations"))
generate_rtfreport(doc_pk, "d_pk.rtf", overwrite = TRUE)
```

1 ページ目: 集計表と、リスティングの最初の 16 行:

![1 ページ目: PK
濃度の集計表とリスティングの始め](img/several-contents/d_pk_page1.png)

1 ページ目: PK 濃度の集計表とリスティングの始め

2 ページ目: リスティングの続き:

![2 ページ目: (continued)
の付いたリスティングの続き](img/several-contents/d_pk_page2.png)

2 ページ目: (continued) の付いたリスティングの続き

この例の脚注はページのフッターに入れたので、どのページにも出ます。`rtf_tables(footnotes = )`
に
書いた脚注はそのコンテンツの物で、そのコンテンツの各ページで、その下に出ます。

## ページを確かめる: `rtf_layout()`

[`rtf_layout()`](https://ichirio.github.io/rtfreporter/reference/rtf_layout.md)
は、[`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md)
が書き出すとおりにページを組みます（ファイルは書きません）。
各ページのコンテンツを 1 行ずつ、表の行数と図の大きさとともに示します。1
ページに載る量を数え、 コンテンツがどこで切れるかを確かめるのに使います:

``` r

rtf_layout(doc_pk)
#> rtf_pages: 10 blocks on 9 pages (letter landscape, 9 pt)
#>  page 1  [sec 1]  rtftable 9 rows         title 3
#>       +           rtftable 16 rows (1/9)  title 1  space 1
#>  page 2           rtftable 24 rows (2/9)  title 2
#>  page 3           rtftable 24 rows (3/9)  title 2
#>  page 4           rtftable 24 rows (4/9)  title 2
#>  page 5           rtftable 24 rows (5/9)  title 2
#>  page 6           rtftable 24 rows (6/9)  title 2
#>  page 7           rtftable 24 rows (7/9)  title 2
#>  page 8           rtftable 24 rows (8/9)  title 2
#>  page 9           rtftable 8 rows (9/9)   title 2
```

結果はリストでもあります。`rtf_layout(doc)$pages` は印刷するページごとに
1 つで、それぞれの `segments` がそのページのコンテンツ（上から順）です。

## 複数のコンテンツがあるページのセクション

`rtf_tables(auto_section = TRUE)`
は名前の付いたコンテンツごとにセクションを開き、ヘッダーとフッターの値を
変えられるようにします。ページのヘッダーとフッターは 1
つなので、複数のコンテンツがあるページのセクションも 1 つです:

- ページのセクションは、**そのページで最初にセクションを持つコンテンツ**の物です。
- 同じページの後のコンテンツが違うセクションを持つと warning
  が出て、そのセクションは使われません。そのために改ページは足しません。
- どのコンテンツもセクションを持たないページは、前のページのセクションを続けます。

Show code

``` r

df <- function(n) data.frame(Item = sprintf("Row %02d", seq_len(n)), N = seq_len(n))
doc_s <- rtf_document() |>
  rtf_tables(list(Overview = df(3)), titles = "Table 1") |>
  rtf_tables(list("Part B" = df(4)), titles = "Table 2", auto_section = TRUE,
             page_break = FALSE)
rtf_layout(doc_s)
```

    #> rtf_pages: 2 blocks on 1 page (letter landscape, 9 pt)
    #>  page 1  [sec 1 "Part B"]  Overview  rtftable 3 rows  title 1
    #>       +                    Part B    rtftable 4 rows  title 1

1 つ目の表はセクションを持たないので、ページは 2 つ目の “Part B”
になります。改ページは `page_break`
と表自身のページ分けだけで決まります。

## タイトルやヘッダーにページの値を入れる: `{列名}`

列の値でページを分けた表（`as_rtftables(page_by = )`、plan の
[`plan_paginate_group()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)）は、
各ページのその列の値を知っています。タイトル・脚注・ヘッダー・フッターに
`{列名}` と書くと、その値が入ります:

Show code

``` r

lab <- data.frame(PARAM = rep(c("ALT (U/L)", "AST (U/L)"), each = 3),
                  Visit = rep(c("Baseline", "Week 2", "Week 4"), 2),
                  Mean  = c(21.3, 22.0, 21.8, 24.1, 24.9, 25.2))
pages <- as_rtftables(lab, page_by = "PARAM", drop_cols = "PARAM")
doc_t <- rtf_document() |>
  rtf_section(header = rtf_header(list(c(l = "Parameter: {PARAM}",
                                         r = "Page {PAGE} of {TOTAL_PAGES}")))) |>
  rtf_tables(pages, titles = list(c("Table 14.3.1", "Mean of {PARAM} by Visit")))
rtf_layout(doc_t)
```

    #> rtf_pages: 2 blocks on 2 pages (letter landscape, 9 pt)
    #>  page 1  [sec 1]  ALT (U/L)  rtftable 3 rows  title 2
    #>  page 2           AST (U/L)  rtftable 3 rows  title 2
    rtf_layout(doc_t)$pages[[2]]$values     # the values of page 2
    #>       PARAM 
    #> "AST (U/L)"
    generate_rtfreport(doc_t, "t_tokens.rtf", overwrite = TRUE)

![検査値の表の 2 ページ目: ヘッダーとタイトルに
AST](img/several-contents/t_tokens_page2.png)

検査値の表の 2 ページ目: ヘッダーとタイトルに AST

複数のコンテンツがあるページでは、タイトルと脚注は自分の表の値を取るので、それぞれが自分の値を書きます。
ヘッダーとフッターはページの物で、列ごとに、そのページで最初に値を持つ表の値を取ります。後の表が違う値を
持つと warning が出ます（上のセクションと同じ決まり）。ページに値の無い
`{名前}` は書いたまま出ます。 rtfreporter の
token（`{PAGE}`、`{PROGRAM}` など）と `rtf_document(tokens = )` で渡した
token は、 同じ名前の列より優先します。

## 次に読む

- [Titles, footnotes and several
  tables](https://ichirio.github.io/rtfreporter/articles/adding-content.md)
  — タイトルと脚注の書き方と体裁
- [Headers, footers and
  tokens](https://ichirio.github.io/rtfreporter/articles/headers-footers.md)
  — ページのヘッダー・フッターと token
- [Splitting a report into
  sections](https://ichirio.github.io/rtfreporter/articles/section-splitting.md)
  — セクションと `auto_section`
- [Listings with a
  plan](https://ichirio.github.io/rtfreporter/articles/plan-listings.md)
  — 折り返すリスティング

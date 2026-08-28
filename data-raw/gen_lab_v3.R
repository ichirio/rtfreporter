# Render through the source tree, never a stale install (#306).
source("data-raw/_load.R")

data_dir <- system.file("extdata", package = "rtfreporter")
lab_rbc <- readRDS(file.path(data_dir, "lab_rbc.rds"))
lab_wbc <- readRDS(file.path(data_dir, "lab_wbc.rds"))
lab_hgb <- readRDS(file.path(data_dir, "lab_hgb.rds"))

# ── スパニングヘッダー / 列ヘッダー ──────────────────────────────────────────
spanning_hdr <- list(
  list(from = 2L,  to = 7L,  label = "Drug A (N=30)", underline = TRUE),
  list(from = 8L,  to = 13L, label = "Drug B (N=30)", underline = TRUE),
  list(from = 14L, to = 19L, label = "Total (N=60)",  underline = TRUE)
)
col_hdr_row <- c("Baseline\nGrade", rep(c("0","1","2","3","4","Total"), 3L))
col_widths  <- c(1200L, rep(733L, 18L))

make_shift_tbl <- function(df) rtftable(
  df, col_header = col_hdr_row, spanning_header = spanning_hdr,
  column_widths_twips = col_widths,
  col_spec = c(list(list(col=1, align="left")),
               lapply(2:19, function(j) list(col=j, align="center"))),
  row_height_twips = 360L, border = "tfl", table_align = "left"
)

# ── 共通ヘッダー（5行）─────────────────────────────────────────────────────
#   行6はauto_sectionが自動で追加する（検査項目名・左アライン）
common_hdr <- rtf_header(rows = list(
  c(l = "Protocol: STUDY001",
    r = "DRAFT"),
  c(l = "Drug Co., Ltd.",
    r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}"),
  c(c = "Table 14.x.x.x  Shift table of Toxicity Grade"),
  c(c = "<Safety Analysis Set>"),
  c(c = "")   # blank row
))

ftr_note <- paste0(
  "Note: Percentages are based on the number of subjects in the Safety Analysis Set (N) ",
  "for each treatment group. ",
  "A subject is counted once at the worst post-baseline toxicity grade observed. ",
  "This document contains confidential information intended solely for the use of the ",
  "named recipient. Unauthorised review, use, disclosure or distribution is prohibited. ",
  "Run date: ", Sys.Date()
)
common_ftr <- rtf_footer(rows = list(c(l = ftr_note)))

# ── レポート構築（新API: auto_section） ────────────────────────────────────
#   rtf_section()でpage=を省略 → 共通ヘッダー/フッターを"_default"として登録
#   rtf_tables()でauto_section=TRUEを指定 → 名前付きリストの各名前を
#   ヘッダーの最終行（左アライン）として自動追加し、セクションを自動分割
doc <- rtf_document() |>
  rtf_config(page = list(orientation = "landscape", width_in = 11, height_in = 8.5,
                         margin_top_in = 0.75, margin_bottom_in = 0.75,
                         margin_left_in = 0.5, margin_right_in = 0.5)) |>
  rtf_section(secinfo = list(header = common_hdr, footer = common_ftr)) |>
  rtf_tables(
    list(
      "Red Blood Cell Count (10^6/uL)"   = make_shift_tbl(lab_rbc),
      "White Blood Cell Count (10^3/uL)" = make_shift_tbl(lab_wbc),
      "Hemoglobin (g/dL)"                = make_shift_tbl(lab_hgb)
    ),
    auto_section = TRUE
  )

out <- "output/table14_3_x_lab_shift_v3.rtf"
generate_rtfreport(doc, out, overwrite = TRUE)
cat("Generated:", normalizePath(out), "\n")
cat("Size:", file.info(out)[["size"]], "bytes\n")

# Render through the source tree, never a stale install (#306).
source("data-raw/_load.R")

data_dir <- system.file("extdata", package = "rtfreporter")
lab_rbc <- readRDS(file.path(data_dir, "lab_rbc.rds"))
lab_wbc <- readRDS(file.path(data_dir, "lab_wbc.rds"))
lab_hgb <- readRDS(file.path(data_dir, "lab_hgb.rds"))

lab_params <- list(
  list(param = "Red Blood Cell Count (10^6/uL)",   data = lab_rbc),
  list(param = "White Blood Cell Count (10^3/uL)", data = lab_wbc),
  list(param = "Hemoglobin (g/dL)",                data = lab_hgb)
)

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

# ── 6行ヘッダー構成 ───────────────────────────────────────────────────────────
#   Row 1: Protocol (left)  |  DRAFT (right)
#   Row 2: Company (left)   |  Page x of y (right)
#   Row 3: centered テーブルタイトル（全ページ共通）
#   Row 4: centered 解析対象集団（全ページ共通）
#   Row 5: blank（全ページ共通）
#   Row 6: left 検査項目名（ページ毎に変わる）
common_hdr_rows <- list(
  c(l = "Protocol: STUDY001",
    r = "DRAFT"),
  c(l = "Drug Co., Ltd.",
    r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}"),
  c(c = "Table 14.x.x.x  Shift table of Toxicity Grade"),
  c(c = "<Safety Analysis Set>"),
  c(c = "")  # blank row
)

make_lab_hdr <- function(param_label) {
  update_header_row(
    rtf_header(rows = common_hdr_rows),
    row = 6L, content = c(l = param_label)   # left-aligned
  )
}

ftr_note <- paste0(
  "Note: Percentages are based on the number of subjects in the Safety Analysis Set (N) ",
  "for each treatment group. ",
  "A subject is counted once at the worst post-baseline toxicity grade observed. ",
  "This document contains confidential information intended solely for the use of the ",
  "named recipient. Unauthorised review, use, disclosure or distribution is prohibited. ",
  "Run date: ", Sys.Date()
)
common_ftr <- rtf_footer(rows = list(
  c(l = ftr_note)   # 1列・左アライン
))

# ── レポート構築（3ページ × 3セクション） ────────────────────────────────────
doc <- rtf_document() |>
  rtf_config(page = list(orientation = "landscape", width_in = 11, height_in = 8.5,
                         margin_top_in = 0.75, margin_bottom_in = 0.75,
                         margin_left_in = 0.5, margin_right_in = 0.5))

for (i in seq_along(lab_params)) {
  doc <- doc |>
    rtf_tables(list(make_shift_tbl(lab_params[[i]]$data))) |>
    rtf_section(page    = i,
                secinfo = list(header = make_lab_hdr(lab_params[[i]]$param),
                               footer = common_ftr))
}

out <- "output/table14_3_x_lab_shift_v2.rtf"
generate_rtfreport(doc, out, overwrite = TRUE)
cat("Generated:", normalizePath(out), "\n")
cat("Size:", file.info(out)[["size"]], "bytes\n")

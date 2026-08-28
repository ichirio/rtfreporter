# Render through the source tree, never a stale install (#306).
source("data-raw/_load.R")

data_dir <- system.file("extdata", package = "rtfreporter")
lab_hgb   <- readRDS(file.path(data_dir, "lab_hgb.rds"))
lab_alt   <- readRDS(file.path(data_dir, "lab_alt.rds"))
lab_creat <- readRDS(file.path(data_dir, "lab_creat.rds"))

lab_params <- list(
  list(param = "Hemoglobin (g/dL)",   data = lab_hgb),
  list(param = "ALT (U/L)",            data = lab_alt),
  list(param = "Creatinine (umol/L)",  data = lab_creat)
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

# ── ヘッダー（共通2行 + 検査項目名3行目） ─────────────────────────────────────
common_hdr_rows <- list(
  c(l = "Protocol: STUDY001", r = "Drug Co., Ltd."),
  c(l = "Table 14.3.x  Toxicity Grade Shift Table",
    r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}")
)
make_lab_hdr <- function(param_label) {
  update_header_row(
    rtf_header(rows = common_hdr_rows),
    row = 3L, content = c(c = param_label)
  )
}
common_ftr <- rtf_footer(rows = list(
  c(l = paste0("Run date: ", Sys.Date()), c = "CONFIDENTIAL", r = "DRAFT")
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

out <- "output/table14_3_x_lab_shift.rtf"
generate_rtfreport(doc, out, overwrite = TRUE)
cat("Generated:", normalizePath(out), "\n")
cat("Size:", file.info(out)[["size"]], "bytes\n")

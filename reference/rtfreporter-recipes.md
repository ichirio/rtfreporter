# Four complete recipes: DM, AE, PK and LB

Copy-and-run examples for the four table shapes a clinical report is
mostly made of. Each builds its data, its table and its document, and
ends in a rendered RTF. They are deliberately shown together because
**their arguments barely overlap** – what a demographics table needs, an
adverse events table does not, and vice versa.

## Which arguments each shape actually uses

|  |  |  |  |  |
|----|----|----|----|----|
| **Setting** | **DM** | **AE** | **PK** | **LB** |
| `stub_vars` (hierarchy) | – | yes | yes | – |
| `group_col` / `group_by` | yes | yes | yes | yes |
| `blank_rows` | – | yes | yes | yes |
| `split` / `max_rows` | yes | yes | yes | – |
| `drop_cols` (hidden carrier) | – | – | – | yes |
| [`set_decimal_split()`](https://ichirio.github.io/rtfreporter/reference/set_decimal_split.md) | – | – | yes | – |
| [`paginate_cols()`](https://ichirio.github.io/rtfreporter/reference/paginate_cols.md) (too wide) | – | – | yes | – |
| `fmt_*()` before building | – | yes | yes | yes |

The single most common mistake is reaching for `stub_vars` on a table
that has no hierarchy (DM), or omitting it on one that does (AE, PK).

## See also

[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
for the conversion,
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
for placement,
[`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md)
to render. The
[`vignette("rtfreporter-quickstart")`](https://ichirio.github.io/rtfreporter/articles/rtfreporter-quickstart.md)
walks through one report slowly.

## Examples

``` r
# ==========================================================================
# 1. DM -- demographics.  Flat table, grouped by characteristic.
# ==========================================================================
dm <- data.frame(
  Characteristic = c("Age (years)", "Age (years)", "Age (years)",
                     "Sex", "Sex"),
  Statistic      = c("n", "Mean (SD)", "Median", "Male, n (%)",
                     "Female, n (%)"),
  `Drug A`       = c("60", "54.2 (11.3)", "55.0", "31 (51.7%)",
                     "29 (48.3%)"),
  `Drug B`       = c("58", "56.8 (10.1)", "57.5", "27 (46.6%)",
                     "31 (53.4%)"),
  check.names = FALSE, stringsAsFactors = FALSE
)

dm_doc <- rtf_document(page = rtf_page(orientation = "landscape")) |>
  rtf_tables(
    as_rtftables(dm,
                 group_col = "Characteristic",   # keep a characteristic whole
                 split     = "group_safe",
                 max_rows  = 20,
                 border    = "tfl"),
    titles = list(c("Table 14.1.1",
                    "Demographic and Baseline Characteristics",
                    "<Safety Analysis Set>"))
  )
f <- tempfile(fileext = ".rtf")
generate_rtfreport(dm_doc, f, overwrite = TRUE)

# ==========================================================================
# 2. AE -- adverse events.  SOC / PT hierarchy folded into one stub column.
# ==========================================================================
ae <- data.frame(
  SOC = c(rep("Cardiac disorders", 2), rep("Gastrointestinal disorders", 3)),
  PT  = c("Atrial fibrillation", "Bradycardia",
          "Nausea", "Vomiting", "Diarrhoea"),
  `Drug A` = c("3 (5.0%)", "1 (1.7%)", "8 (13.3%)", "4 (6.7%)", "2 (3.3%)"),
  `Drug B` = c("2 (3.4%)", "0", "6 (10.3%)", "3 (5.2%)", "5 (8.6%)"),
  check.names = FALSE, stringsAsFactors = FALSE
)

ae_doc <- rtf_document(page = rtf_page(orientation = "landscape")) |>
  rtf_tables(
    as_rtftables(ae,
                 stub_vars  = c("SOC", "PT"),   # the hierarchy DM does not have
                 group_by   = "indent",         # groups are found by indentation
                 blank_rows = "between_groups",
                 split      = "group_safe",
                 max_rows   = 20,
                 border     = "tfl"),
    titles = list(c("Table 14.3.1",
                    "Adverse Events by System Organ Class and Preferred Term",
                    "<Safety Analysis Set>")),
    footnotes = list("Percentages use the number of treated subjects.")
  )
generate_rtfreport(ae_doc, f, overwrite = TRUE)

# ==========================================================================
# 3. PK -- concentrations.  Decimal alignment and a table wider than a page.
# ==========================================================================
pk <- data.frame(
  Time      = c(rep("1 h", 3), rep("2 h", 3)),
  Statistic = rep(c("n", "Mean", "SD"), 2),
  `Day 1`   = c("24", "1104.5", "233.41"),
  `Day 7`   = c("24", "88.012", "19.223"),
  `Day 14`  = c("24", "9.0125", "2.1044"),
  `Day 28`  = c("24", "1234.5", "301.22"),
  check.names = FALSE, stringsAsFactors = FALSE
)

pk_pages <- as_rtftables(
  pk,
  stub_vars  = c("Time", "Statistic"),
  group_by   = "indent",
  blank_rows = "between_groups",
  # ABSOLUTE widths: relative ones are normalised to the page, so the table
  # could never be too wide and paginate_cols() would have nothing to do
  column_widths_twips = c(2000L, rep(1800L, 4)),
  border     = "tfl"
)

pk_pages <- pk_pages |>
  set_decimal_split(cols = 2:5) |>   # line the decimal points up
  paginate_cols(at = 4L, carry = 1L) # split by COLUMN, repeat the stub

pk_doc <- rtf_document(page = rtf_page(orientation = "landscape"))
for (p in pk_pages) pk_doc <- rtf_tables(pk_doc, p)
generate_rtfreport(pk_doc, f, overwrite = TRUE)

# ==========================================================================
# 4. LB -- laboratory shift, grouped by a column that is never printed.
# ==========================================================================
lb <- data.frame(
  PARAMCD  = c(rep("ALT", 3), rep("AST", 3)),   # carrier: groups, not printed
  Baseline = rep(c("Normal", "Grade 1", "Grade 2"), 2),
  Normal   = c("40", "5", "1", "38", "6", "2"),
  `Grade 1`= c("8", "12", "3", "9", "11", "4"),
  `Grade 2`= c("1", "4", "7", "2", "3", "6"),
  check.names = FALSE, stringsAsFactors = FALSE
)

lb_doc <- rtf_document(page = rtf_page(orientation = "landscape")) |>
  rtf_tables(
    as_rtftables(lb,
                 group_col  = "PARAMCD",   # group by it ...
                 drop_cols  = "PARAMCD",   # ... but never print it
                 blank_rows = "between_groups",
                 border     = "tfl"),
    titles = list(c("Table 14.4.1",
                    "Shift from Baseline in Laboratory Grade",
                    "<Safety Analysis Set>"))
  )
generate_rtfreport(lb_doc, f, overwrite = TRUE)

unlink(f)
```

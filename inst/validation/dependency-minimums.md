# rtfreporter: the minimum versions of its dependencies

Each minimum in `DESCRIPTION` is the first version of that package that has
every function, argument and data set rtfreporter uses: in `R/`, in the
tests, and in the vignettes built with the package.  For each package the
table names what was found, the version that first has it, and where that
is recorded.  The minimum is the latest of those versions.  The Quality Plan
(`quality-plan.md`, *The environment strategy*) says how the minimums are
tested: the R-CMD-check job *dependency minimums* installs each of them
exactly, on R 4.4.  It does not run on R's own minimum (4.2), because
`tern`'s dependencies (through `car`) cannot be installed on R 4.2; R 4.2 is
tested by the R 4.2 job of the matrix, with the newest packages that R
allows.

The rule for what sets a minimum: a use in `R/`, in a test that always runs,
or in a vignette built with the package does.  These do not: a use only in a
test that skips when the version is older (`skip_if_not_installed(pkg,
version)` or a check that the function exists), code that checks for the
function before it calls it, and the articles of the web site, which are not
part of the package.  Such a use is named in the table, with the version it
needs.  A version found in a package's `NAMESPACE` was read from the CRAN
release of that version (the read-only mirror `github.com/cran/<package>`,
one tag per release).

| Package | Minimum | What needs it (function or argument -> version) | Source |
|---|---|---|---|
| R (`Depends`) | 4.2 | The native pipe `\|>` in the tests and examples -> 4.1.0. The minimum of gtsummary (2.3.0) itself depends on R (>= 4.2), and 4.2 is the oldest R the CI tests (the R 4.2 job of the matrix). | R NEWS 4.1.0; gtsummary 2.3.0 `DESCRIPTION` |
| grDevices, grid, methods, stats, utils (`Imports`) | none | They ship with R, so their version is R's. | -- |
| cards | 0.7.0 | `ard_tabulate()`, `ard_summary()`, `ard_tabulate_value()` (the names the package and its tests use) -> 0.7.0; the `ADLB` data -> 0.7.0; `ard_stack_hierarchical()`, `ard_strata()` -> 0.3.0; `apply_fmt_fun()` -> 0.6.1 (the code also takes the older `apply_fmt_fn()`); `bind_ard()`, `ard_stack()`, `ard_hierarchical()`, `ADSL`, `ADAE`, `ADTTE` -> 0.1.0. Not counted: `diff_ard_hierarchical()` -> 0.9.0, used by one test that skips without it and by an article. | cards NEWS 0.7.0 (*Lifecycle Changes*: the renames; `ADLB`); `NAMESPACE` of each release |
| cardx | 0.2.0 | `ard_stats_t_test()`, `ard_stats_chisq_test()`, `ard_stats_kruskal_test()`, `ard_stats_wilcox_test()`, `ard_stats_prop_test()` (their names since the renames) and `ard_categorical_ci(value = )` -> 0.2.0. | cardx NEWS 0.2.0; `NAMESPACE` 0.2.0 |
| broom | 1.0.8 | cardx's tests and confidence intervals call broom; cardx asks for broom (>= 1.0.8), and the tests that use them skip below 1.0.8. | cardx `DESCRIPTION` (Suggests: broom (>= 1.0.8)); `tests/testthat/test-table-plan.R` |
| gt | 0.11.1 | `extract_body()` -> 0.10.0, `extract_body(output = )` and its column arguments -> 0.11.0, and its leaving out hidden columns by default (the row-group column the adapter hides) -> 0.11.1; `gt_split()`, `gt_group()`, `grp_pull()` -> 0.9.0; `sub_missing()` -> 0.6.0; the other functions -> 0.2.0.5, the first CRAN release. Not counted: `rows_hide()` -> 1.4.0, used by one test that skips below 1.4.0 (the adapter reads `_rows_hidden` only when it is there). | gt NEWS 0.11.1, 0.11.0, 0.10.0, 0.9.0, 0.6.0; `NAMESPACE` of each release |
| gtsummary | 2.3.0 | `tbl_split_by_rows()` (a `tbl_split`, which `as_rtftable()` and `as_rtftables()` read: FR-TB103, FR-TB115) -> 2.3.0; `modify_source_note()` -> 2.0.4; `tbl_hierarchical()` -> 2.0.3; `tbl_ard_summary()` -> 2.0.0; `modify_column_alignment()` -> 1.5.1; `tbl_continuous()` -> 1.5.0; `as_flex_table()`, `as_hux_table()` -> 1.3.3; `modify_spanning_header()`, `modify_footnote()` -> 1.3.1. The minimum was 1.7.0, which has no `tbl_split`. | gtsummary NEWS 2.3.0 ("Added `tbl_split_by_rows()`"); `NAMESPACE` of each release |
| formatters | 0.5.2 | `mf_strings()`, `mf_spans()`, `mf_aligns()`, `mf_nlheader()`, `mf_rfnotes()`, `mf_rinfo()` -> 0.4.0; `matrix_form()`, `main_title()`, `subtitles()`, `main_footer()`, `prov_footer()`, `page_titles()` -> 0.2.0. These alone allow 0.4.0 (the minimum was 0.5.0). Raised to 0.5.2 so that it installs together with tern 0.9.0: tern 0.9.0 depends on rtables (>= 0.6.3), and rtables 0.6.3 depends on formatters (>= 0.5.2). | `NAMESPACE` of each release; rtables 0.6.3 `DESCRIPTION` |
| rtables | 0.6.3 | `main_title()`, `main_footer()` -> 0.5.0; `basic_table()`, `split_cols_by()`, `split_rows_by()`, `analyze()`, `build_table()`, `in_rows()`, `rcell()`, `summarize_row_groups()`, `prune_table()`, `keep_split_levels()`, `paginate_table()` -> 0.3.6, the first CRAN release. These alone allow 0.5.0 (the minimum was 0.6.0). Raised to 0.6.3 so that it installs together with tern 0.9.0, which depends on rtables (>= 0.6.3). | `NAMESPACE` of each release; tern 0.9.0 `DESCRIPTION` |
| rlistings | 0.2.1 | `as_listing()`, `listing_dispcols()`, and `matrix_form()` of a listing -> 0.2.1, the first CRAN release. The minimum was 0.2.0, which was never on CRAN, so it could not be installed. | CRAN archive of rlistings; `NAMESPACE` 0.2.1 |
| tern | 0.9.0 | `analyze_vars()` -> 0.9.0 (before it, `summarize_vars()`); `count_occurrences()` -> 0.8.3, the first CRAN release. tern needs car, whose dependencies cannot be installed on R 4.2; this is why the job *dependency minimums* runs on R 4.4. | tern NEWS 0.9.0; `NAMESPACE` of each release |
| tfrmt | 0.1.0 | `page_plan()`, `page_structure()` (a page_plan render is a `gt_group` that `as_rtftable()` and `as_rtftables()` read: FR-TB103, FR-TB115) -> 0.1.0; `tfrmt()`, `frmt()`, `frmt_structure()`, `body_plan()`, `row_grp_plan()`, `element_row_grp_loc()`, `print_to_gt()` -> 0.0.1. | tfrmt NEWS 0.1.0 ("Add `page_plan`"); `NAMESPACE` of each release |
| flextable | 0.9.5 | `information_data_chunk()`, `information_data_paragraph()` (the flextable adapter reads the cells and their alignment with them) -> 0.9.5; `separate_header()` -> 0.7.1; `nrow_part()` -> 0.6.4; `fp_border_default()` -> 0.6.2; `colformat_double()`, `colformat_date()` -> 0.6.0; the others -> 0.5.10 or earlier. The minimum was 0.9.0, which has no `information_data_*()`. | `NAMESPACE` of 0.9.4 and 0.9.5 (the NEWS does not name them) |
| huxtable | 5.0.0 | `header_rows()`, `set_header_rows()`, `markdown()`, `set_markdown()`, `merge_across()`, `merge_down()` -> 5.0.0; `merge_cells()` -> 4.2.0; `add_rows()` -> 4.1.0; the others -> 0.3.1 or earlier. | huxtable NEWS 5.0.0; `NAMESPACE` of each release |
| testthat | 3.1.7 | `local_mocked_bindings()` -> 3.1.7; `expect_no_error()`, `expect_no_warning()`, `expect_no_message()` -> 3.1.5; `expect_no_match()` -> 3.0.3; the third edition (`Config/testthat/edition: 3`) -> 3.0.0. | testthat NEWS 3.1.7, 3.1.5, 3.0.3, 3.0.0 |
| rstudioapi | 0.11 | `readRStudioPreference()` (which pipe the user prefers, for written code) -> 0.11; `getSourceEditorContext()` -> 0.6; `isAvailable()` -> 0.1. | `NAMESPACE` of each release |
| knitr | none | `opts_chunk`, `knit_hooks`, `opts_knit` -> 0.1; `include_graphics()` -> 1.12; `current_input()`, `asis_output()` -> 1.6. The vignettes are built by rmarkdown, which needs a later knitr than any of these, and `current_input()` in the package is called inside `tryCatch()`. | `NAMESPACE` of each release; rmarkdown `DESCRIPTION` |
| rmarkdown | none | `html_vignette()` -> 0.3.3, the first CRAN release. | `NAMESPACE` 0.3.3 |
| ggplot2 | none | `ggplot()`, `aes()`, `geom_point()`, `ggsave()` -> 0.9.0, the first release with a `NAMESPACE`. | `NAMESPACE` 0.9.0 |
| magrittr | none | `%>%`, in the code the package writes for the user (`library(magrittr)`) -> every release. | -- |
| ragg | none | `agg_png(filename, width, height, units, res, background)` -> 0.1.0, the first CRAN release. | `NAMESPACE` 0.1.0 |
| devEMF | none | `emf(file, width, height, emfPlus)` -> every release since 0.9-4. | `NAMESPACE` 0.9-4 |
| tibble | none | `tibble()` -> 1.0, `as_tibble()` -> 1.1. Every dplyr the minimum cards (0.7.0) can be installed with (dplyr >= 1.1.2) needs tibble (>= 3.2.0). | `NAMESPACE` of each release; cards 0.7.0 `DESCRIPTION` |
| dplyr | none | `across()`, `all_of()`, `slice_max()`, `rename_with()` -> 1.0.0; the others -> 0.4.0 or earlier. The minimum cards (0.7.0) needs dplyr (>= 1.1.2). | `NAMESPACE` of each release; cards 0.7.0 `DESCRIPTION` |
| tidyr | none | `pivot_wider()`, `pivot_longer()` -> 1.0.0; `complete()`, `nesting()` -> 0.4.0 or earlier. The minimum cards (0.7.0) needs tidyr (>= 1.3.0). | `NAMESPACE` of each release; cards 0.7.0 `DESCRIPTION` |
| forcats | none | Only an article of the web site uses it (`library(forcats)`); no function of it is named in the package or its tests. | -- |
| labelled | none | `set_variable_labels()` -> 0.2.3; only an article of the web site uses it. | `NAMESPACE` 0.2.3 |
| pharmaverseadam | none | The data `adsl`, `adae`, `advs`, `adrs_onco` -> 0.1.1, the first CRAN release. | `data/` of 0.1.1 |
| writexl | none | `write_xlsx()` -> 0.1, the first CRAN release. | `NAMESPACE` 0.1 |
| readxl | 1.0.0 | `read_xlsx()` (the package reads a spec workbook with it) -> 1.0.0. | `NAMESPACE` 0.1.1 and 1.0.0 |
| gtable | none | `gtable_col()` -> 0.1.1, the first CRAN release. | `NAMESPACE` 0.1.1 |
| lattice | none | `xyplot()`; lattice ships with R (a recommended package). | -- |
| patchwork | none | `wrap_plots()` -> 1.0.0, the first CRAN release. | `NAMESPACE` 1.0.0 |

## Changes

The minimums before this table was made (#1032), and why each changed:

| Package | Was | Now | Why |
|---|---|---|---|
| testthat | 3.0.0 | 3.1.7 | The tests use `local_mocked_bindings()` (3.1.7). |
| gtsummary | 1.7.0 | 2.3.0 | A `tbl_split` (`tbl_split_by_rows()`, 2.3.0) is an input the requirements name. |
| formatters | 0.5.0 | 0.5.2 | To install together with tern 0.9.0 (through rtables 0.6.3). |
| rtables | 0.6.0 | 0.6.3 | To install together with tern 0.9.0, which depends on rtables (>= 0.6.3). |
| rlistings | 0.2.0 | 0.2.1 | 0.2.0 was never released on CRAN; 0.2.1 is the first. |
| tern | none | 0.9.0 | The tests use `analyze_vars()` (0.9.0). |
| tfrmt | none | 0.1.0 | A `page_plan()` render (0.1.0) is an input the requirements name. |
| cardx | none | 0.2.0 | The `ard_stats_*()` names and `ard_categorical_ci(value = )` (0.2.0). |
| flextable | 0.9.0 | 0.9.5 | The adapter reads a flextable with `information_data_chunk()` and `information_data_paragraph()` (0.9.5). |
| rstudioapi | none | 0.11 | `readRStudioPreference()` (0.11). |
| readxl | none | 1.0.0 | `read_xlsx()` (1.0.0). |
| random.cdisc.data | none | removed | Nothing in the package, its tests or its articles used it; it is no longer in `Suggests`. |

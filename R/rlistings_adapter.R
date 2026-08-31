# ============================================================================
#  rlistings_adapter -- read configuration from an rlistings listing object
# ============================================================================
#
#  rlistings (pharmaverse) produces clinical data listings.  Its table object,
#  `listing_df`, is declared as
#
#      setOldClass(c("listing_df", "tbl_df", "tbl", "data.frame"))
#
#  -- an S3 tibble, *not* an S4 `VTableTree`.  That matters twice over:
#
#    1. `.is_rtables_tbl()` (which tests `isS4() && is(x, "VTableTree")`) does
#       not recognise it, so without this adapter it never reaches the rtables
#       path; and
#    2. because it genuinely *is* a `data.frame`, it would otherwise fall
#       through to the plain-data.frame branch of `as_rtftables()` and render
#       with no error at all -- silently discarding the listing's own
#       decisions: the columns `disp_cols` excluded would be printed, the key
#       columns' repeat suppression would be lost, and the titles and footers
#       would be dropped (#322).
#
#  So the detector below must be consulted **before** the data.frame branch.
#  Order is the whole fix; the extraction was already possible.
#
#  Extraction itself needs no new code.  rlistings renders through exactly the
#  canonical structure the rtables adapter already reads -- the
#  `MatrixPrintForm` returned by `formatters::matrix_form()` -- and implements
#  every accessor that reader uses: mf_strings(), mf_nlheader(), mf_spans(),
#  mf_aligns(), mf_rfnotes(), main_title(), subtitles(), main_footer() and
#  prov_footer().  This file therefore contributes a detector, a token set and
#  a typed wrapper, and delegates to `.mpf_to_rtftable_kwargs()`.
#
#  What comes across:
#    * only the display columns -- matrix_form() honours listing_dispcols(),
#      so a column left out of `disp_cols` cannot leak into the output;
#    * key-column repeat suppression, already applied to the cell strings;
#    * column headers, including spanning rows from spanning_col_label_df();
#    * per-column alignment;
#    * main_title() + subtitles() -> page title block;
#    * main_footer() + prov_footer() + referential footnotes -> footnote block.
#
#  What does not, by design:
#    * `paginate_listing()`.  rtfreporter paginates itself, and matrix_form()
#      hands us an unpaginated table, which is what `split` / `max_rows` want.
#    * Repeat suppression is *baked into* the strings by rlistings, so unlike
#      `as_rtftables(collapse_repeats = )` a suppressed key does not reprint at
#      the top of a new page.  Noted in ?as_rtftables.

# ── Detection ────────────────────────────────────────────────────────────────

# Is `x` an rlistings listing?  Cheap, dependency-light -- no rlistings needed.
.is_rlistings_tbl <- function(x) {
  inherits(x, "listing_df")
}


# ── Token resolution ─────────────────────────────────────────────────────────

# The same tokens as the rtables adapter, because the same reader consumes
# them.  "footnote_marks" is kept for symmetry: a listing carrying referential
# footnote marks in its cells is rewritten the same way a table's are.
#
# Spelled out rather than aliased to `.RTABLES_TOKENS_ALL`: R sources this file
# before rtables_adapter.R (alphabetical order), so the alias would not resolve
# at load time.  test-rlistings-adapter.R asserts the two stay identical.
.RLISTINGS_TOKENS_ALL <- c("col_header", "alignment", "spanning", "titles",
                           "footnotes", "footnote_marks")

.resolve_rlistings_tokens <- function(read) {
  .resolve_meta_tokens(read, .RLISTINGS_TOKENS_ALL, "rlistings")
}


# ── Central mapping: listing_df + tokens -> rtftable kwargs ──────────────────

.rlistings_to_rtftable_kwargs <- function(x, tokens = .RLISTINGS_TOKENS_ALL) {
  if (!.is_rlistings_tbl(x)) {
    stop("`x` must be an rlistings listing (listing_df).", call. = FALSE)
  }
  .mpf_to_rtftable_kwargs(x, tokens = tokens,
                          what = "an rlistings listing")
}

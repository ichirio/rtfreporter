# ============================================================================
#  rtfreporter defaults — admin-tunable
# ============================================================================
#
#  This file defines default values used by the RTF renderer when callers
#  do not supply an explicit value.  Edit the numbers below to change the
#  package-wide visual style of generated reports.
#
#  All values are in **twips** (1 twip = 1/1440 inch = 1/20 point).
#
# ----------------------------------------------------------------------------

rtfreporter_defaults <- list(

  # ── Default row (cell) height for every table-shaped element ────────────
  #
  # Applied uniformly to:
  #   • RTF page header / footer rows  (rtf_header / rtf_footer)
  #   • Table column-header rows       (rtftable: header_row_height_twips)
  #   • Table data rows                (rtftable: row_height_twips)
  #   • Blank separator rows           (rtftable: blank_row_height_twips)
  #   • Title / footnote block rows    (single-column content-width tables)
  #
  # Looked up by the document's font size, expressed in HALF-POINTS
  # (the unit used by the RTF `\fs` command).
  #   16 = 8pt, 18 = 9pt, 20 = 10pt, 22 = 11pt, 24 = 12pt, ...
  #
  # The package default font size is 18 (= 9pt); admins changing the font
  # size should keep the corresponding entry below in sync.
  default_row_height_twips_by_font_half_points = list(
    "16" = 210L,   #  8pt
    "18" = 230L,   #  9pt  ← package default
    "20" = 250L,   # 10pt
    "22" = 270L,   # 11pt
    "24" = 290L,   # 12pt
    "26" = 310L,   # 13pt
    "28" = 330L    # 14pt
  ),

  # Fallback formula when the font size is not listed above.
  #   row_height_twips ≈ font_half_points * twips_per_half_point
  # For 18 half-points (9pt) this yields ~230 twips, matching the table.
  default_row_height_twips_per_half_point = 12.8,

  # Lower bound enforced after the lookup / formula above.
  default_row_height_twips_min = 180L,

  # ── Default left / right cell padding ───────────────────────────────────
  # Applied to all table-shaped cells (page header / page footer / content
  # table) when the caller does not supply an explicit value.  Default 0L:
  # cell content sits flush against the cell border, matching the typical
  # clinical TFL look.  Set to e.g. 72L (= 0.05 inch ≈ 1 mm) for a small
  # internal padding.
  default_cell_padding_left_twips  = 0L,
  default_cell_padding_right_twips = 0L,

  # Default top / bottom cell padding (#125), written into each cell
  # definition. 0L writes nothing, so rows keep their height as before.
  default_cell_padding_top_twips    = 0L,
  default_cell_padding_bottom_twips = 0L
)

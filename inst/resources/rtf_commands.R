rtf_commands <- list(
  document = list(
    # Document header. Hardened for portable, renderer-independent output:
    #   \ansicpg1252  declare the ANSI code page (correct 8-bit fallback chars)
    #   \deflang1033  default language (US English)
    #   \uc1          every \uN unicode escape is followed by exactly ONE
    #                 fallback char -- matches the "\uN?" form this package
    #                 emits, so readers consume the right number of bytes.
    rtf_header_open = "{\\rtf1\\ansi\\ansicpg1252\\deff0\\deflang1033\\uc1",
    # \fnil\fcharset0: unknown font family, ANSI charset. Declaring the charset
    # makes glyph mapping deterministic across viewers without assuming a
    # specific family (mono vs proportional).
    font_table_template = "{\\fonttbl{\\f0\\fnil\\fcharset0 {font_name};}}",
    color_table_default = "{\\colortbl;\\red0\\green0\\blue0;}",
    # Page settings.
    #   \headery / \footery  distance (twips) of the header/footer band from
    #                        the page edge, coordinated with the top/bottom
    #                        margins so the band sits inside the margin.
    #   \widowctrl           widow/orphan control (standard for body text).
    page_settings_template = "\\paperw{width_twips}\\paperh{height_twips}{orientation_cmd}\\margl{margin_left_twips}\\margr{margin_right_twips}\\margt{margin_top_twips}\\margb{margin_bottom_twips}\\headery{header_dist_twips}\\footery{footer_dist_twips}\\widowctrl\\fs{font_size_half_points}",
    section_defaults = "\\sectd",
    # Table terminator emitted after a page's content (and its title / footnote
    # tables) before a section break or the document close.  A table must be
    # closed with \pard before paragraph-level content, or strict RTF readers
    # absorb the break into the table flow (#130).
    table_end = "\\pard",
    # Manual page break, as ONE group holding two empty 1pt paragraphs around
    # the \page:
    #
    #   * the leading \pard\fs2\par terminates the preceding table.  It must be
    #     a *terminated* paragraph: a bare \pard with no \par leaves Word with
    #     nothing to flank the break and Word drops it (#138).
    #   * \page forces the break.
    #   * the trailing \pard\fs2\par anchors it so the next table starts on the
    #     new page.
    #   * the single enclosing group scopes \fs2, so the 1pt size cannot leak
    #     into the content that follows.
    #
    # Grouping the two paragraphs SEPARATELY -- r2rtf's as_rtf_new_page() and
    # reporter both do -- satisfies Word but makes LibreOffice's RTF importer
    # drop the break outright when it sits between two tables (#408), which
    # silently ruins any RTF -> PDF batch conversion that goes through
    # LibreOffice.  One group around the whole break satisfies both.
    page_break = "{\\pard\\fs2\\par\\page\\pard\\fs2\\par}",
    section_break = "\\sect",
    header_wrapper = "{\\header {content}}",
    footer_wrapper = "{\\footer {content}}",
    document_close = "}"
  ),
  paragraph = list(
    center_bold_template = "\\pard\\qc\\b {text}\\b0\\par",
    left_template = "\\pard\\ql {text}\\par",
    figure_placeholder_center_template = "\\pard\\qc [Figure: {filename}]\\par",
    empty_table = "\\par [empty table]\\par"
  ),
  table = list(
    row_start = "\\trowd",
    row_end = "\\row",
    row_height_template = "\\trrh{row_height_twips}",
    # Legacy cell templates used by .render_header_footer().
    cell_boundary_template = "\\cellx{cx}",
    cell_boundary_top_border_template = "\\clbrdrt\\brdrs\\brdrw10\\cellx{cx}",
    header_cell_text_template = "\\b {text}\\b0\\cell",
    cell_text_template = "{text}\\cell",
    cell_text_aligned_template = "{align} {text}\\cell"
  ),
  alignment = list(
    left    = "\\ql",
    right   = "\\qr",
    center  = "\\qc",
    default = "\\ql"
  ),
  # RTF cell-border side prefixes and style codes.
  border = list(
    side_prefix = list(
      top    = "\\clbrdrt",
      bottom = "\\clbrdrb",
      left   = "\\clbrdrl",
      right  = "\\clbrdrr"
    ),
    style = list(
      single = "\\brdrs",
      double = "\\brdrdb",
      thick  = "\\brdrth",
      dash   = "\\brdrdash",
      dot    = "\\brdrdot"
    )
  ),
  # Cell vertical alignment commands.
  cell_valign = list(
    top    = "\\clvertalt",
    center = "\\clvertalc",
    bottom = "\\clvertalb"
  ),
  # Inline text decoration on/off pairs.
  text_decor = list(
    bold      = list(on = "\\b ",      off = "\\b0 "),
    italic    = list(on = "\\i ",      off = "\\i0 "),
    underline = list(on = "\\ul ",     off = "\\ulnone "),
    super     = list(on = "\\super ",  off = "\\nosupersub "),
    sub       = list(on = "\\sub ",    off = "\\nosupersub ")
  ),
  # RTF picture templates for PNG and JPEG embedding.
  picture = list(
    png_template  = "\\pard{align}{\\pict\\pngblip\\picw{picw}\\pich{pich}\\picwgoal{picwgoal}\\pichgoal{pichgoal}\n{hex}\n}\\par",
    jpeg_template = "\\pard{align}{\\pict\\jpegblip\\picw{picw}\\pich{pich}\\picwgoal{picwgoal}\\pichgoal{pichgoal}\n{hex}\n}\\par"
  ),
  # RTF dynamic field templates.
  fields = list(
    # Dynamic page number — updated per page by the RTF viewer (\chpgn).
    auto_page        = "\\chpgn ",
    # Dynamic total-pages field — updated by the RTF viewer (NUMPAGES).
    # {total_pages} is substituted with the static fallback count.
    auto_total_pages  = "{\\field{\\*\\fldinst NUMPAGES}{\\fldrslt {total_pages}}}",
    # Dynamic section-pages field — number of pages in the current section.
    section_pages     = "{\\field{\\*\\fldinst SECTIONPAGES}{\\fldrslt 1}}"
  ),
  # Package-wide configurable defaults.
  # Adjust these values to tune the visual appearance without touching renderer code.
  defaults = list(
    # Row height for header/footer table rows, in twips (1 inch = 1440 twips).
    # 288 twips ≈ 0.2 inch (suitable for 9 pt font).
    header_footer_row_height_twips = 288L
  )
)

"""
The sample PDF and the bookmarks picture for the Assembling article
(vignettes/articles/assembling.Rmd).

    python data-raw/gen_assemble_pdf.py <assembled.rtf> <work-dir> [qpdf]

<assembled.rtf> is the Gallery's four outputs assembled with the call the
article shows (data-raw/gen_gallery.R, section 5, makes the same file).
qpdf (optional) is the path of the qpdf program, else the QPDF environment
variable, else qpdf on the PATH; without it the PDF is not linearized.

  1. LibreOffice (headless, a private profile in <work-dir>) turns the RTF into
     a PDF with the export options that matter for an FDA submission:
     bookmarks on, PDF 1.7, initial view "Bookmarks Panel and Page" (PageMode
     /UseOutlines), default page layout and magnification, no notes or form
     fields, lossless images.
  2. pypdf sets the document information: a title, and a neutral author
     ("rtfreporter example") -- never a person's name.
  3. qpdf --linearize makes it "Fast Web View" (LibreOffice cannot), and
     qpdf --check / --check-linearization confirm it.
  4. The PDF is copied to pkgdown/assets/ (the site serves it; pkgdown/ is
     .Rbuildignore'd, so it never ships in the package).
  5. A picture of a viewer with the bookmarks pane open is drawn from the PDF's
     real outline (inst/rtf-examples/assemble/bookmarks.png, also
     .Rbuildignore'd).  It is an illustration, not a screenshot of Acrobat.

qpdf, LibreOffice, pypdf, PyMuPDF (fitz) and Pillow are tools for making this
article only; rtfreporter does not use them.
"""
import os
import shutil
import subprocess
import sys

import fitz
import pypdf
from pypdf.generic import BooleanObject, DictionaryObject, NameObject
from PIL import Image, ImageDraw, ImageFont

SOFFICE = next(p for p in (
    r"C:\Program Files\LibreOffice\program\soffice.exe",
    r"C:\Program Files (x86)\LibreOffice\program\soffice.exe",
    shutil.which("soffice") or "") if p and os.path.exists(p))

OPTIONS = (
    '{"ExportBookmarks":{"type":"boolean","value":"true"},'
    '"OpenBookmarkLevels":{"type":"long","value":"-1"},'
    '"SelectPdfVersion":{"type":"long","value":"17"},'
    '"InitialView":{"type":"long","value":"1"},'
    '"PageLayout":{"type":"long","value":"0"},'
    '"Magnification":{"type":"long","value":"0"},'
    '"InitialPage":{"type":"long","value":"1"},'
    '"ExportNotes":{"type":"boolean","value":"false"},'
    '"ExportNotesPages":{"type":"boolean","value":"false"},'
    '"ExportFormFields":{"type":"boolean","value":"false"},'
    '"UseTaggedPDF":{"type":"boolean","value":"false"},'
    '"UseLosslessCompression":{"type":"boolean","value":"true"},'
    '"ReduceImageResolution":{"type":"boolean","value":"false"}}')

SAMPLE = "xyz-001-tables-sample.pdf"
TITLE = "Study XYZ-001 Tables and Listings (rtfreporter sample)"
AUTHOR = "rtfreporter example"
SUBJECT = "Fictional data. Assembled with rtfreporter::assemble_rtf(), converted with LibreOffice."


def to_pdf(rtf, outdir):
    os.makedirs(outdir, exist_ok=True)
    profile = "file:///" + os.path.abspath(os.path.join(outdir, "profile")).replace("\\", "/")
    subprocess.run([SOFFICE, "-env:UserInstallation=" + profile, "--headless",
                    "--convert-to", "pdf:writer_pdf_Export:" + OPTIONS,
                    "--outdir", outdir, rtf], check=True, capture_output=True,
                   timeout=300)
    return os.path.join(outdir, os.path.splitext(os.path.basename(rtf))[0] + ".pdf")


def set_info(pdf):
    """Title, a neutral author, open on the bookmarks; no XMP left to disagree."""
    out = pdf[:-4] + "-info.pdf"
    w = pypdf.PdfWriter(clone_from=pdf)
    w.pdf_header = pypdf.PdfReader(pdf).pdf_header   # keep PDF 1.7
    w.add_metadata({"/Title": TITLE, "/Author": AUTHOR, "/Subject": SUBJECT})
    root = w._root_object
    root[NameObject("/PageMode")] = NameObject("/UseOutlines")
    root[NameObject("/ViewerPreferences")] = DictionaryObject(
        {NameObject("/DisplayDocTitle"): BooleanObject(True)})
    if "/Metadata" in root:   # LibreOffice's XMP has no title; drop it
        del root["/Metadata"]
    with open(out, "wb") as f:
        w.write(f)
    return out


def linearize(pdf, qpdf):
    out = pdf[:-4] + "-lin.pdf"
    subprocess.run([qpdf, "--linearize", pdf, out], check=True)
    for flag in ("--check", "--check-linearization"):
        subprocess.run([qpdf, flag, out], check=True, capture_output=True)
    return out


def font(size, bold=False):
    name = "arialbd.ttf" if bold else "arial.ttf"
    for d in (r"C:\Windows\Fonts", "/usr/share/fonts/truetype/dejavu"):
        p = os.path.join(d, name if os.path.exists(os.path.join(d, name)) else "DejaVuSans.ttf")
        if os.path.exists(p):
            return ImageFont.truetype(p, size)
    return ImageFont.load_default()


def bookmarks_picture(pdf, png, page_no, dpi=80):
    doc = fitz.open(pdf)
    toc = doc.get_toc()   # [level, title, page]
    pix = doc[page_no - 1].get_pixmap(dpi=dpi)
    page = Image.frombytes("RGB", (pix.width, pix.height), pix.samples)
    pane_w, bar_h, row_h = 380, 34, 26
    w = pane_w + page.width + 24
    h = max(page.height + 24, 40 + row_h * (len(toc) + 2))
    img = Image.new("RGB", (w, h + bar_h), "#d9dde3")
    dr = ImageDraw.Draw(img)
    dr.rectangle([0, 0, w, bar_h], fill="#2b3a4a")
    dr.text((12, 8), "%s  -  page %d of %d" % (SAMPLE, page_no, len(doc)),
            fill="white", font=font(15))
    dr.rectangle([0, bar_h, pane_w, h + bar_h], fill="#f4f5f7", outline="#b8bec6")
    dr.text((12, bar_h + 10), "Bookmarks", fill="#333333", font=font(15, True))
    y = bar_h + 42
    for lvl, title, pg in toc:
        x = 14 + (lvl - 1) * 18
        here = pg == page_no
        if here:
            dr.rectangle([4, y - 4, pane_w - 4, y + row_h - 6], fill="#cfe0f5")
        label = title if len(title) <= 44 else title[:43] + "\u2026"
        dr.text((x, y), label, fill="#1a1a1a", font=font(13, here))
        y += row_h
    img.paste(page, (pane_w + 12, bar_h + 12))
    img = img.quantize(colors=128, method=Image.Quantize.MEDIANCUT)
    img.save(png, optimize=True)


if __name__ == "__main__":
    rtf, work = sys.argv[1], sys.argv[2]
    root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    pdf = set_info(to_pdf(os.path.abspath(rtf), work))
    qpdf = (sys.argv[3] if len(sys.argv) > 3 else os.environ.get("QPDF")
            or shutil.which("qpdf"))
    if qpdf:
        pdf = linearize(pdf, qpdf)
    dest = os.path.join(root, "pkgdown", "assets", SAMPLE)
    os.makedirs(os.path.dirname(dest), exist_ok=True)
    shutil.copy(pdf, dest)
    doc = fitz.open(pdf)
    target = next((i + 1 for i, p in enumerate(doc) if i > 1 and
                   "Treatment-Emergent Adverse Events by SOC and PT" in p.get_text()), 3)
    out = os.path.join(root, "inst", "rtf-examples", "assemble", "bookmarks.png")
    os.makedirs(os.path.dirname(out), exist_ok=True)
    bookmarks_picture(pdf, out, target)
    print(dest, out)

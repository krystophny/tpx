"""Browser upload/edit/download coverage for editable SVG and TikZ sources."""
from pathlib import Path
import math
import re
import xml.etree.ElementTree as ET

from scenarios import box, diff, park

ROOT = Path(__file__).resolve().parents[2]
SVG_FIXTURE = ROOT / "tests/acceptance/fixtures/svg-workflow.svg"
TIKZ_FIXTURE = ROOT / "tests/core/fixtures/tikz/independent-scene.tex"


def open_source(app, path, related=None):
    app.menu("File", "Open")
    dialog = app.page.get_by_role("dialog")
    dialog.get_by_label("Choose file").set_input_files(path)
    if related is not None:
        dialog.get_by_label("Related files (optional)", exact=True).set_input_files(related)
    dialog.get_by_role("button", name="OK", exact=True).click()
    app.page.wait_for_timeout(300)


def save_source(app, name, format_part):
    app.menu("File", "Save as...")
    dialog = app.page.get_by_role("dialog")
    file_type = dialog.get_by_label("File type")
    options = file_type.locator("option").all()
    matches = [option for option in options
               if format_part.casefold() in (option.text_content() or "").casefold()]
    if not matches:
        labels = [option.text_content() for option in options]
        raise AssertionError(f"Save As has no {format_part!r} source format: {labels}")
    file_type.select_option(matches[0].get_attribute("value"))
    dialog.get_by_label("File name").fill(name)
    with app.page.expect_download() as download_info:
        dialog.get_by_role("button", name="OK", exact=True).click()
    download = download_info.value
    path = app.output / download.suggested_filename
    download.save_as(path)
    return path


def draw_line_and_check_change(app):
    viewport, cx, cy = box(app)
    area = [viewport["x"], viewport["y"], viewport["w"], viewport["h"]]
    before = app.pixels(area)
    app.menu("Insert", "Insert line")
    app.drag(cx - 70, cy - 30, cx + 80, cy + 50)
    app.key("Escape")
    park(app, viewport)
    app.page.wait_for_timeout(200)
    after = app.pixels(area)
    changed = diff(before, after)
    if changed < 80:
        raise AssertionError(f"graphical edit changed only {changed} paper pixels")
    return area, after


def reopen_and_compare(app, path, related, area, expected_pixels):
    open_source(app, path, related)
    actual_pixels = app.pixels(area)
    changed = diff(expected_pixels, actual_pixels)
    if changed > 25:
        raise AssertionError(f"downloaded source reopens with {changed} changed paper pixels")


def s_svg_source_workflow(app):
    upload = app.output / "numeric-reference SVG.svg"
    source_bytes = SVG_FIXTURE.read_bytes()
    if "Label β".encode("utf-8") not in source_bytes:
        raise AssertionError("independent SVG Unicode label changed")
    source_bytes = source_bytes.replace(
        "Label β".encode("utf-8"), b"Label &#946; &#x1F600;")
    upload.write_bytes(source_bytes)
    open_source(app, upload)
    area, edited_pixels = draw_line_and_check_change(app)
    downloaded = save_source(app, "browser-svg-edit", "SVG")
    root = ET.parse(downloaded).getroot()
    if not root.tag.endswith("svg"):
        raise AssertionError("SVG download is not an SVG document")
    if root.get("width") != "24mm" or root.get("height") != "18mm":
        raise AssertionError("SVG download lost its authored physical canvas")
    labels = [element.text for element in root.iter() if element.tag.endswith("text")]
    if "Label β 😀" not in labels:
        raise AssertionError(f"SVG download lost its Unicode text: {labels}")
    shapes = [element for element in root.iter()
              if element.tag.rsplit("}", 1)[-1] in
              {"line", "polyline", "polygon", "path", "rect", "circle", "ellipse", "text"}]
    if len(shapes) < 6:
        raise AssertionError(f"SVG download lost editable geometry: {len(shapes)} shapes")
    reopen_and_compare(app, downloaded, None, area, edited_pixels)
    return f"uploaded strict SVG, edited and downloaded {len(shapes)} rendered shapes"


def s_tikz_source_workflow(app):
    original = TIKZ_FIXTURE.read_bytes()
    linked_image = (b"\\node at (12,11) [anchor=south west] "
                    b"{\\includegraphics[width=4mm,height=3mm]"
                    b"{acceptance-image.png}};\n")
    if linked_image not in original:
        raise AssertionError("canonical TikZ linked-image statement changed")
    # The browser has no native filesystem for resolving linked assets.
    # Keep this scenario focused on its supported source-edit workflow.
    original = original.replace(linked_image, b"")
    upload = app.output / "browser source β.tex"
    upload.write_bytes(original)
    open_source(app, upload)
    area, edited_pixels = draw_line_and_check_change(app)
    downloaded = save_source(app, "browser-tikz-edit.tex", "TikZ")
    saved = downloaded.read_bytes()
    prefix, _, _ = original.partition(b"\\begin{tikzpicture}")
    _, _, suffix = original.partition(b"\\end{tikzpicture}")
    saved_prefix, _, _ = saved.partition(b"\\begin{tikzpicture}")
    _, _, saved_suffix = saved.partition(b"\\end{tikzpicture}")
    if not prefix or not suffix or saved_prefix != prefix or saved_suffix != suffix:
        raise AssertionError("TikZ edit changed bytes outside the selected picture")
    number = rb"[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][-+]?\d+)?"
    line = re.search(
        rb"\\(?:draw|path)\[[^\]]*draw=black[^\]]*\]\s*"
        rb"\(\s*(" + number + rb")mm\s*,\s*(" + number + rb")mm\s*\)"
        rb"\s*--\s*\(\s*(" + number + rb")mm\s*,\s*(" + number + rb")mm\s*\)",
        saved)
    if line is None:
        raise AssertionError("TikZ download lost the inserted source-coordinate line")
    x0, y0, x1, y1 = map(float, line.groups())
    if not all(0 <= x <= 20 and 0 <= y <= 18 for x, y in ((x0, y0), (x1, y1))):
        raise AssertionError(f"downloaded line escaped the fixture page: {(x0, y0, x1, y1)}")
    length = math.hypot(x1 - x0, y1 - y0)
    if not 2.0 <= length <= 6.0:
        raise AssertionError(f"downloaded line has implausible physical length: {length} mm")
    reopen_and_compare(app, downloaded, None, area, edited_pixels)
    return (f"uploaded TeX/TikZ; parsed edited line length {length:.3f} mm, "
            f"reopened and matched rendered pixels in {downloaded.suffix} source")


SOURCE_FORMAT_SCENARIOS = [
    ("svg-source-workflow", s_svg_source_workflow),
    ("tikz-source-workflow", s_tikz_source_workflow),
]

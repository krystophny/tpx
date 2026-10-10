#!/usr/bin/env python3
"""Independent render oracle for a hand-authored TpX scene export."""
import json
import math
import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile
import unittest
import xml.etree.ElementTree as ET

BINARY = Path(os.environ["TPX_BINARY"]).resolve()
RUNTIME_BINARY = Path(os.environ["RUNTIME_BINARY"]).resolve()
FIXTURES = Path(__file__).parent / "acceptance" / "fixtures"
TIKZ_FIXTURES = Path(__file__).parent / "core" / "fixtures" / "tikz"
SVG_NS = "http://www.w3.org/2000/svg"
XLINK_NS = "http://www.w3.org/1999/xlink"


def require_tools(*names):
    missing = [name for name in names if not shutil.which(name)]
    if missing:
        raise RuntimeError("Required source-acceptance tools are missing: "
                           + ", ".join(missing))


def ppm_pixels(path):
    data = Path(path).read_bytes()
    tokens = []
    offset = 0
    while len(tokens) < 4:
        while data[offset:offset + 1].isspace():
            offset += 1
        if data[offset:offset + 1] == b"#":
            offset = data.index(b"\n", offset) + 1
            continue
        end = offset
        while data[end:end + 1] and not data[end:end + 1].isspace():
            end += 1
        tokens.append(data[offset:end])
        offset = end
    if tokens[0] != b"P6" or tokens[3] != b"255":
        raise AssertionError("renderer did not produce an 8-bit RGB PPM")
    if data[offset:offset + 2] == b"\r\n":
        offset += 2
    else:
        offset += 1
    width, height = map(int, tokens[1:3])
    pixels = data[offset:]
    if len(pixels) != width * height * 3:
        raise AssertionError("renderer PPM has an incomplete pixel buffer")
    return width, height, pixels


def points(value):
    numbers = [float(item) for item in re.findall(r"[-+]?(?:\d+\.?\d*|\.\d+)", value)]
    if len(numbers) % 2:
        raise AssertionError("SVG point list has an odd number of coordinates")
    return list(zip(numbers[::2], numbers[1::2]))


class SourceAcceptance(unittest.TestCase):
    def setUp(self):
        require_tools("rsvg-convert", "gs", "pdfinfo", "pdftotext",
                      "pdftocairo", "pdflatex")
        self.directory = tempfile.TemporaryDirectory(prefix="tpx-source-acceptance-")
        self.root = Path(self.directory.name)
        self.source_dir = self.root / "source"
        self.output_dir = self.root / "output"
        self.source_dir.mkdir()
        self.output_dir.mkdir()
        for name in ("independent-scene.tpx", "independent-scene.svg",
                     "svg-workflow.svg"):
            shutil.copy2(FIXTURES / name, self.source_dir / name)
        shutil.copy2(TIKZ_FIXTURES / "acceptance-image.png", self.source_dir)
        self.expected = json.loads((FIXTURES / "scene-expected.json").read_text())

    def tearDown(self):
        self.directory.cleanup()

    def command(self, arguments, cwd=None):
        result = subprocess.run(list(map(str, arguments)), cwd=cwd or self.root,
                                capture_output=True, text=True, timeout=40)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        return result.stdout

    def run_lcl_source_workflow(self, format_id):
        source_dir = self.root / ("lcl source " + format_id + " Grüß")
        source_dir.mkdir()
        if format_id == "tikz":
            source = source_dir / "nested source β.tex"
            shutil.copy2(TIKZ_FIXTURES / "independent-scene.tex", source)
            shutil.copy2(TIKZ_FIXTURES / "acceptance-image.png", source_dir)
            extension = ".tex"
        else:
            source = source_dir / "strict source β.svg"
            shutil.copy2(FIXTURES / "svg-workflow.svg", source)
            extension = ".svg"
        edited = source_dir / ("after graphic edit" + extension)
        structural = source_dir / ("after delete and append" + extension)
        env = os.environ.copy()
        env.update(TPX_SOURCE_FILE=str(source),
                   TPX_SOURCE_AFTER_EDIT=str(edited),
                   TPX_SOURCE_AFTER_STRUCTURAL=str(structural))
        result = subprocess.run([str(RUNTIME_BINARY), "source-format-" + format_id],
                                cwd=source_dir, env=env, capture_output=True,
                                text=True, timeout=60)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("PASS source-format-" + format_id, result.stdout,
                      result.stdout + result.stderr)
        self.assertTrue(edited.is_file(), "LCL workflow did not save its edited snapshot")
        return source_dir, source, edited, structural

    @staticmethod
    def tikz_scope_point(x, y):
        inner = math.radians(-17.5)
        outer = math.radians(30)
        ix = 1 + math.cos(inner) * (4 * x) - math.sin(inner) * y
        iy = math.sin(inner) * (4 * x) + math.cos(inner) * y
        return (3 + math.cos(outer) * ix - math.sin(outer) * iy,
                1 + math.sin(outer) * ix + math.cos(outer) * iy)

    def compile_tex_snapshot(self, source_dir, source):
        self.command(["pdflatex", "-interaction=batchmode", "-halt-on-error",
                      "-no-shell-escape", source.name], cwd=source_dir)
        pdf = source.with_suffix(".pdf")
        svg = source.with_name(source.stem + "-render.svg")
        self.command(["pdftocairo", "-svg", pdf, svg], cwd=source_dir)
        return pdf, svg

    def test_native_export_matches_independent_scene_and_renders(self):
        source_image = self.source_dir / "acceptance-image.png"
        self.command([BINARY, "-f", self.source_dir / "independent-scene.tpx",
                      "-x", "svg", "-o", self.output_dir / "native-export"])
        exported = self.output_dir / "native-export.svg"
        root = ET.parse(exported).getroot()
        children = [node for node in root if node.tag.startswith("{" + SVG_NS + "}")]
        self.assertEqual([node.tag.rsplit("}", 1)[-1] for node in children],
                         ["defs", "polyline", "path", "polygon", "text", "text", "image"])

        expected = self.expected["tpx"]
        line, cubic, rectangle, first_text, second_text, image = children[1:]
        line_oracle, cubic_oracle, rect_oracle, text_oracle, text2_oracle, image_oracle = expected
        self.assertEqual(points(line.get("points")), [
            (line_oracle["start"][0], -line_oracle["start"][1]),
            (line_oracle["end"][0], -line_oracle["end"][1]),
        ])
        self.assertEqual(line.get("stroke"), line_oracle["stroke"])
        self.assertAlmostEqual(float(line.get("stroke-width")),
                               line_oracle["stroke_width"], places=5)
        dash = self.expected["svg"]["objects"][0]["dash_pattern"]
        self.assertEqual(tuple(map(float, line.get("stroke-dasharray").split(","))),
                         tuple(dash))

        cubic_values = [float(item) for item in re.findall(
            r"[-+]?(?:\d+\.?\d*|\.\d+)", cubic.get("d"))]
        expected_cubic = [coordinate for x, y in cubic_oracle["points"]
                          for coordinate in (x, -y)]
        self.assertEqual(cubic_values, expected_cubic)
        self.assertEqual(cubic.get("stroke"), cubic_oracle["stroke"])
        self.assertAlmostEqual(float(cubic.get("stroke-width")),
                               cubic_oracle["stroke_width"], places=5)

        ox, oy = rect_oracle["origin"]
        width, height = rect_oracle["size"]
        angle = math.radians(rect_oracle["rotation_degrees"])
        expected_corners = []
        for x, y in ((0, 0), (width, 0), (width, height), (0, height)):
            expected_corners.append((
                round(ox + x * math.cos(angle) - y * math.sin(angle), 2),
                round(-(oy + x * math.sin(angle) + y * math.cos(angle)), 2)))
        actual_corners = points(rectangle.get("points"))
        self.assertEqual(len(actual_corners), 4)
        for actual, expected in zip(actual_corners, expected_corners):
            self.assertAlmostEqual(actual[0], expected[0], delta=0.01)
            self.assertAlmostEqual(actual[1], expected[1], delta=0.01)
        self.assertEqual(rectangle.get("fill"), rect_oracle["fill"])
        self.assertEqual(rectangle.get("stroke"), rect_oracle["stroke"])
        self.assertAlmostEqual(float(rectangle.get("stroke-width")),
                               rect_oracle["stroke_width"], places=5)

        self.assertEqual((first_text.get("x"), first_text.get("y"),
                          first_text.get("text-anchor"), first_text.text),
                         (f"{text_oracle['position'][0]:.2f}",
                          f"-{text_oracle['position'][1]:.2f}", "start",
                          text_oracle["plain_text"]))
        self.assertEqual((second_text.get("x"), second_text.get("y"),
                          second_text.get("text-anchor"), second_text.text),
                         (f"{text2_oracle['position'][0]:.2f}",
                          f"-{text2_oracle['position'][1]:.2f}", "end",
                          text2_oracle["plain_text"]))
        self.assertEqual((image.get("x"), image.get("y"), image.get("width"),
                          image.get("height"), image.get("{" + XLINK_NS + "}href")),
                         (f"{image_oracle['position'][0]:.2f}",
                          f"-{image_oracle['position'][1] + image_oracle['size'][1]:.2f}",
                          f"{image_oracle['size'][0]:.2f}",
                          f"{image_oracle['size'][1]:.2f}",
                          image_oracle["reference"]))
        self.assertEqual((self.output_dir / "acceptance-image.png").read_bytes(),
                         (TIKZ_FIXTURES / "acceptance-image.png").read_bytes())
        self.assertEqual(source_image.read_bytes(),
                         (TIKZ_FIXTURES / "acceptance-image.png").read_bytes())

        pdf = self.root / "rendered.pdf"
        ppm = self.root / "rendered.ppm"
        self.command(["rsvg-convert", "--format", "pdf", "--output", pdf, exported])
        page = self.command(["pdfinfo", pdf])
        match = re.search(r"Page size:\s+([\d.]+) x ([\d.]+) pts", page)
        self.assertIsNotNone(match, page)
        width_mm, height_mm = (float(value) * 25.4 / 72 for value in match.groups())
        expected_bounds = self.expected["tpx_physical_bounds_mm"]
        self.assertAlmostEqual(width_mm, expected_bounds[0], delta=0.02)
        self.assertAlmostEqual(height_mm, expected_bounds[1], delta=0.02)
        self.assertAlmostEqual(float(root.get("width")[:-2]), expected_bounds[0], delta=0.02)
        self.assertAlmostEqual(float(root.get("height")[:-2]), expected_bounds[1], delta=0.02)
        text = self.command(["pdftotext", pdf, "-"])
        self.assertIn("Label β", text)
        self.assertIn("x squared", text)
        self.command(["gs", "-q", "-dSAFER", "-dBATCH", "-dNOPAUSE",
                      "-sDEVICE=ppmraw", "-r144", "-sOutputFile=" + str(ppm), pdf])
        _, _, pixels = ppm_pixels(ppm)
        color_counts = {
            "magenta": 0,
            "cyan": 0,
            "yellow": 0,
        }
        for offset in range(0, len(pixels), 3):
            red, green, blue = pixels[offset:offset + 3]
            if red > 180 and green < 80 and blue > 180:
                color_counts["magenta"] += 1
            if red < 80 and green > 180 and blue > 180:
                color_counts["cyan"] += 1
            if red > 180 and green > 180 and blue < 80:
                color_counts["yellow"] += 1
        for color, count in color_counts.items():
            self.assertGreater(count, 20, f"independently rendered image lost {color}: {color_counts}")

    def test_trusted_tikz_fixture_has_expected_rendered_content_and_bounds(self):
        source_dir = self.root / "trusted-tex"
        source_dir.mkdir()
        for name in ("independent-scene.tex", "acceptance-image.png"):
            shutil.copy2(TIKZ_FIXTURES / name, source_dir / name)
        self.command(["pdflatex", "-interaction=batchmode", "-halt-on-error",
                      "-no-shell-escape", "independent-scene.tex"], cwd=source_dir)

        pdf = source_dir / "independent-scene.pdf"
        rendered_svg = source_dir / "trusted-tex.svg"
        self.command(["pdftocairo", "-svg", pdf, rendered_svg])
        vector_paths = [element for element in ET.parse(rendered_svg).getroot().iter()
                        if element.tag.endswith("path") and element.get("stroke")]
        line_paths = [element for element in vector_paths
                      if element.get("stroke-dasharray")]
        cubic_paths = [element for element in vector_paths
                       if re.search(r"\bC\b", element.get("d", ""))]
        self.assertGreaterEqual(len(line_paths), 1, "TeX renderer lost the colored line")
        self.assertGreaterEqual(len(cubic_paths), 1, "TeX renderer lost the cubic path")

        def rendered_mm(element):
            values = [float(value) * 25.4 / 72 for value in re.findall(
                r"[-+]?(?:\d+\.?\d*|\.\d+)", element.get("d", ""))]
            self.assertEqual(len(values) % 2, 0,
                             "renderer emitted an incomplete path point")
            return list(zip(values[::2], values[1::2]))

        expected_paths = self.expected["tikz"]["objects"]
        actual_line = rendered_mm(line_paths[0])
        self.assertEqual(len(actual_line), 2)
        for actual, expected in zip(actual_line,
                                    (expected_paths[0]["start"], expected_paths[0]["end"])):
            self.assertAlmostEqual(actual[0], expected[0], delta=0.002)
            self.assertAlmostEqual(actual[1], expected[1], delta=0.002)
        actual_cubic = rendered_mm(cubic_paths[0])
        self.assertEqual(len(actual_cubic), 4)
        for actual, expected in zip(actual_cubic, expected_paths[1]["points"]):
            self.assertAlmostEqual(actual[0], expected[0], delta=0.002)
            self.assertAlmostEqual(actual[1], expected[1], delta=0.002)

        page = self.command(["pdfinfo", pdf])
        match = re.search(r"Page size:\s+([\d.]+) x ([\d.]+) pts", page)
        self.assertIsNotNone(match, page)
        width_mm, height_mm = (float(value) * 25.4 / 72 for value in match.groups())
        # The fixture's geometry spans roughly 17.5 mm by 15 mm. These bounds
        # catch unit-scale errors while tolerating small TeX font differences.
        min_width, max_width, min_height, max_height = self.expected["tikz"][
            "physical_bounds_range_mm"]
        self.assertGreater(width_mm, min_width)
        self.assertLess(width_mm, max_width)
        self.assertGreater(height_mm, min_height)
        self.assertLess(height_mm, max_height)

        text = self.command(["pdftotext", pdf, "-"])
        self.assertIn("Label β", text)
        self.assertIn("x2", text)
        self.assertNotIn("bytes before", text)
        self.assertNotIn("bytes after", text)
        ppm = source_dir / "trusted-tex.ppm"
        self.command(["gs", "-q", "-dSAFER", "-dBATCH", "-dNOPAUSE",
                      "-sDEVICE=ppmraw", "-r144", "-sOutputFile=" + str(ppm), pdf])
        _, _, pixels = ppm_pixels(ppm)
        for color in ((255, 0, 255), (0, 200, 255), (255, 220, 0),
                      (51, 102, 153), (102, 51, 153), (221, 238, 255)):
            count = sum(pixels[offset:offset + 3] == bytes(color)
                        for offset in range(0, len(pixels), 3))
            self.assertGreater(count, 20,
                               f"trusted TeX render lost RGB {color}: {count}")

    def test_independent_svg_fixture_preserves_physical_bounds_and_content(self):
        source_dir = self.root / "trusted-svg"
        source_dir.mkdir()
        shutil.copy2(FIXTURES / "independent-scene.svg", source_dir)
        shutil.copy2(TIKZ_FIXTURES / "acceptance-image.png", source_dir)
        pdf = source_dir / "independent-scene.pdf"
        self.command(["rsvg-convert", "--format", "pdf", "--output", pdf,
                      source_dir / "independent-scene.svg"])
        page = self.command(["pdfinfo", pdf])
        match = re.search(r"Page size:\s+([\d.]+) x ([\d.]+) pts", page)
        self.assertIsNotNone(match, page)
        width_mm, height_mm = (float(value) * 25.4 / 72 for value in match.groups())
        expected_bounds = self.expected["svg"]["physical_bounds_mm"]
        self.assertAlmostEqual(width_mm, expected_bounds[0], delta=0.02)
        self.assertAlmostEqual(height_mm, expected_bounds[1], delta=0.02)
        svg_children = list(ET.parse(source_dir / "independent-scene.svg").getroot())
        svg_texts = [node.text for node in svg_children if node.tag.endswith("}text")]
        self.assertEqual(svg_texts, [item["plain_text"]
                                     for item in self.expected["svg"]["objects"]
                                     if item["kind"] == "text"])

        ppm = source_dir / "independent-svg.ppm"
        self.command(["gs", "-q", "-dSAFER", "-dBATCH", "-dNOPAUSE",
                      "-sDEVICE=ppmraw", "-r144", "-sOutputFile=" + str(ppm), pdf])
        pixel_width, pixel_height, pixels = ppm_pixels(ppm)
        dark_counts = []
        # Text nodes use the only near-black paint in this fixture. These
        # expected-position windows make the render check sensitive to omitted
        # labels while the exact SVG strings above remain the text oracle.
        for bounds_mm in ((0.0, 2.0, 10.5, 8.0), (8.0, 0.0, 15.0, 4.0)):
            x0, y0, x1, y1 = bounds_mm
            physical_width, physical_height = expected_bounds
            box = (int(x0 * pixel_width / physical_width),
                   int(y0 * pixel_height / physical_height),
                   int(x1 * pixel_width / physical_width),
                   int(y1 * pixel_height / physical_height))
            dark = 0
            for y in range(box[1], box[3]):
                for x in range(box[0], box[2]):
                    pixel = pixels[3 * (y * pixel_width + x):3 * (y * pixel_width + x) + 3]
                    if max(pixel) < 80:
                        dark += 1
            dark_counts.append(dark)
        for label, count in zip(("first text", "second text"), dark_counts):
            self.assertGreater(count, 40,
                               f"independent SVG render lost {label}: {dark_counts}")
        for color in ((255, 0, 255), (0, 200, 255), (255, 220, 0),
                      (51, 102, 153), (102, 51, 153), (221, 238, 255)):
            count = sum(pixels[offset:offset + 3] == bytes(color)
                        for offset in range(0, len(pixels), 3))
            self.assertGreater(count, 20,
                               f"independent SVG render lost RGB {color}: {count}")

    def test_lcl_tikz_edit_rebinds_and_matches_independent_tex_render(self):
        source_dir, source, edited, structural = self.run_lcl_source_workflow("tikz")
        edited_text = edited.read_text(encoding="utf-8")
        self.assertNotIn(r"\includegraphics", structural.read_text(encoding="utf-8"))
        self.assertGreaterEqual(structural.read_text(encoding="utf-8").count(r"\path["), 4)
        local_end = re.search(
            r"\(1,0\)\s*--\s*\(\s*([-+0-9.eE]+)\s*,\s*([-+0-9.eE]+)\s*\)",
            edited_text)
        self.assertIsNotNone(local_end, "saved TikZ line lost its local coordinate")
        source_xy = tuple(map(float, local_end.groups()))
        self.assertNotEqual(source_xy, (2.0, 0.0), "graphic edit did not reach source bytes")

        render_dir = self.root / "independent edited TikZ render"
        render_dir.mkdir()
        rendered_source = render_dir / "edited source.tex"
        shutil.copy2(edited, rendered_source)
        shutil.copy2(TIKZ_FIXTURES / "acceptance-image.png", render_dir)
        pdf, svg = self.compile_tex_snapshot(render_dir, rendered_source)
        rendered_paths = [element for element in ET.parse(svg).getroot().iter()
                          if element.tag.endswith("path") and element.get("stroke")]
        line_paths = [element for element in rendered_paths
                      if element.get("stroke-dasharray")]
        cubic_paths = [element for element in rendered_paths
                       if re.search(r"\bC\b", element.get("d", ""))]
        self.assertTrue(line_paths, "TeX renderer lost the edited dashed line")
        self.assertTrue(cubic_paths, "TeX renderer lost the unchanged cubic")

        def rendered_mm(element):
            values = [float(value) * 25.4 / 72 for value in re.findall(
                r"[-+]?(?:\d+\.?\d*|\.\d+)", element.get("d", ""))]
            self.assertEqual(len(values) % 2, 0,
                             "TeX renderer emitted an incomplete path point")
            return list(zip(values[::2], values[1::2]))

        actual_line = rendered_mm(line_paths[0])
        self.assertEqual(len(actual_line), 2)
        expected_line = (self.tikz_scope_point(1, 0),
                         self.tikz_scope_point(*source_xy))
        for actual, expected in zip(actual_line, expected_line):
            self.assertAlmostEqual(actual[0], expected[0], delta=0.002)
            self.assertAlmostEqual(actual[1], expected[1], delta=0.002)
        self.assertGreater(math.dist(actual_line[1], (11.6763934607, 3.2315169115)),
                           0.05, "TeX output did not reflect the edited endpoint")
        expected_cubic = self.expected["tikz"]["objects"][1]["points"]
        actual_cubic = rendered_mm(cubic_paths[0])
        self.assertEqual(len(actual_cubic), 4)
        for actual, expected in zip(actual_cubic, expected_cubic):
            self.assertAlmostEqual(actual[0], expected[0], delta=0.002)
            self.assertAlmostEqual(actual[1], expected[1], delta=0.002)
        self.assertIn("Label β", self.command(["pdftotext", pdf, "-"]))

        final_pdf, _ = self.compile_tex_snapshot(source_dir, structural)
        self.assertIn("Label β", self.command(["pdftotext", final_pdf, "-"]))

    def test_lcl_svg_edit_save_renders_and_reloads(self):
        _, source, edited, _ = self.run_lcl_source_workflow("svg")
        fixture = FIXTURES / "svg-workflow.svg"
        original = ET.parse(fixture).getroot()
        saved = ET.parse(edited).getroot()
        self.assertAlmostEqual(float(saved.get("width")[:-2]), 24.0, delta=0.001)
        self.assertAlmostEqual(float(saved.get("height")[:-2]), 18.0, delta=0.001)
        self.assertIn("Label β", "".join(node.text or "" for node in saved.iter()))
        self.assertEqual(source.read_bytes(), edited.read_bytes(),
                         "SVG reload changed the accepted saved bytes")
        self.assertNotEqual(fixture.read_bytes(), edited.read_bytes(),
                            "the native SVG point edit did not change saved source")
        self.assertEqual(original.get("width"), "24mm")
        self.assertEqual(original.get("height"), "18mm")

        rendered = self.root / "saved svg render.pdf"
        original_pdf = self.root / "original svg render.pdf"
        self.command(["rsvg-convert", "--format", "pdf", "--output", rendered, edited])
        self.command(["rsvg-convert", "--format", "pdf", "--output", original_pdf, fixture])
        for pdf in (original_pdf, rendered):
            page = self.command(["pdfinfo", pdf])
            match = re.search(r"Page size:\s+([\d.]+) x ([\d.]+) pts", page)
            self.assertIsNotNone(match, page)
            physical = tuple(float(value) * 25.4 / 72 for value in match.groups())
            self.assertAlmostEqual(physical[0], 24, delta=0.02)
            self.assertAlmostEqual(physical[1], 18, delta=0.02)
        rendered_ppm = self.root / "saved-svg.ppm"
        original_ppm = self.root / "original-svg.ppm"
        for pdf, ppm in ((rendered, rendered_ppm), (original_pdf, original_ppm)):
            self.command(["gs", "-q", "-dSAFER", "-dBATCH", "-dNOPAUSE",
                          "-sDEVICE=ppmraw", "-r144", "-sOutputFile=" + str(ppm), pdf])
        width0, height0, pixels0 = ppm_pixels(original_ppm)
        width1, height1, pixels1 = ppm_pixels(rendered_ppm)
        self.assertEqual((width1, height1), (width0, height0))
        changed = sum(1 for i in range(0, len(pixels0), 3)
                      if pixels0[i:i + 3] != pixels1[i:i + 3])
        self.assertGreater(changed, 20,
                           "independent SVG render did not show the moved endpoint")


if __name__ == "__main__":
    unittest.main(verbosity=2)

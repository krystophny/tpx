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
FIXTURES = Path(__file__).parent / "acceptance" / "fixtures"
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
        require_tools("rsvg-convert", "gs", "pdfinfo", "pdftotext", "pdflatex")
        self.directory = tempfile.TemporaryDirectory(prefix="tpx-source-acceptance-")
        self.root = Path(self.directory.name)
        self.source_dir = self.root / "source"
        self.output_dir = self.root / "output"
        self.source_dir.mkdir()
        self.output_dir.mkdir()
        for name in ("independent-scene.tpx", "acceptance-image.png"):
            shutil.copy2(FIXTURES / name, self.source_dir / name)
        self.expected = json.loads((FIXTURES / "scene-expected.json").read_text())

    def tearDown(self):
        self.directory.cleanup()

    def command(self, arguments, cwd=None):
        result = subprocess.run(list(map(str, arguments)), cwd=cwd or self.root,
                                capture_output=True, text=True, timeout=40)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        return result.stdout

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
                         (FIXTURES / "acceptance-image.png").read_bytes())
        self.assertEqual(source_image.read_bytes(),
                         (FIXTURES / "acceptance-image.png").read_bytes())

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
            shutil.copy2(FIXTURES / name, source_dir / name)
        self.command(["pdflatex", "-interaction=batchmode", "-halt-on-error",
                      "-no-shell-escape", "independent-scene.tex"], cwd=source_dir)

        pdf = source_dir / "independent-scene.pdf"
        page = self.command(["pdfinfo", pdf])
        match = re.search(r"Page size:\s+([\d.]+) x ([\d.]+) pts", page)
        self.assertIsNotNone(match, page)
        width_mm, height_mm = (float(value) * 25.4 / 72 for value in match.groups())
        # The fixture's geometry spans roughly 19 mm by 18 mm. These bounds
        # catch unit-scale errors while tolerating small TeX font differences.
        min_width, max_width, min_height, max_height = self.expected["tikz"][
            "physical_bounds_range_mm"]
        self.assertGreater(width_mm, min_width)
        self.assertLess(width_mm, max_width)
        self.assertGreater(height_mm, min_height)
        self.assertLess(height_mm, max_height)

        text = self.command(["pdftotext", pdf, "-"])
        self.assertIn("Label beta", text)
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
        for name in ("independent-scene.svg", "acceptance-image.png"):
            shutil.copy2(FIXTURES / name, source_dir / name)
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


if __name__ == "__main__":
    unittest.main(verbosity=2)

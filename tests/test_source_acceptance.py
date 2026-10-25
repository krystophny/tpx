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
import traceback
import unittest
import xml.etree.ElementTree as ET

import headless_display

headless_display.ensure_private_display()

ROOT = Path(__file__).resolve().parents[1]
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
        self.home = self.root / "home"
        self.config_home = self.root / "config"
        self.windows_profile = self.root / "windows-profile"
        self.home.mkdir()
        self.config_home.mkdir()
        self.windows_profile.mkdir()
        self.env = os.environ.copy()
        self.env.update(HOME=str(self.home),
                        XDG_CONFIG_HOME=str(self.config_home),
                        APPDATA=str(self.config_home),
                        USERPROFILE=str(self.windows_profile),
                        TPX_TEST_HOME_ROOT=str(self.home),
                        TPX_TEST_CONFIG_ROOT=str(self.config_home))
        self.source_dir = self.root / "source"
        self.output_dir = self.root / "output"
        self.source_dir.mkdir()
        self.output_dir.mkdir()
        for name in ("independent-scene.tpx", "independent-scene.svg",
                     "svg-workflow.svg"):
            shutil.copy2(FIXTURES / name, self.source_dir / name)
        shutil.copy2(TIKZ_FIXTURES / "acceptance-image.png", self.source_dir)
        self.expected = json.loads((FIXTURES / "scene-expected.json").read_text())

    def command(self, arguments, cwd=None):
        result = subprocess.run(list(map(str, arguments)), cwd=cwd or self.root,
                                env=self.env, capture_output=True, text=True,
                                timeout=40)
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
        env = self.env.copy()
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

    def run_lcl_source_refresh(self, format_id):
        source_dir = self.root / ("external refresh " + format_id + " β")
        source_dir.mkdir()
        if format_id == "tikz":
            source = source_dir / "refreshed source β.tex"
            shutil.copy2(TIKZ_FIXTURES / "independent-scene.tex", source)
            shutil.copy2(TIKZ_FIXTURES / "acceptance-image.png", source_dir)
        else:
            source = source_dir / "refreshed source β.svg"
            shutil.copy2(FIXTURES / "svg-workflow.svg", source)
        env = self.env.copy()
        env["TPX_SOURCE_FILE"] = str(source)
        case = "source-format-refresh-" + format_id
        result = subprocess.run([str(RUNTIME_BINARY), case], cwd=source_dir,
                                env=env, capture_output=True, text=True,
                                timeout=75)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("PASS " + case, result.stdout, result.stdout + result.stderr)
        return source_dir, source

    def run_lcl_binding_burst(self, format_id):
        burst_root = self.root / ("source binding burst " + format_id + " β")
        burst_root.mkdir()
        extension = "svg" if format_id == "svg" else "tex"
        for index in range(1, 9):
            cycle = burst_root / f"cycle-{index:02d}"
            cycle.mkdir()
            if format_id == "svg":
                shutil.copy2(FIXTURES / "svg-workflow.svg",
                             cycle / f"source.{extension}")
            else:
                shutil.copy2(TIKZ_FIXTURES / "independent-scene.tex",
                             cycle / f"source.{extension}")
                shutil.copy2(TIKZ_FIXTURES / "acceptance-image.png", cycle)
        env = self.env.copy()
        env["TPX_SOURCE_BURST_ROOT"] = str(burst_root)
        case = "source-format-lifecycle-" + format_id
        result = subprocess.run([str(RUNTIME_BINARY), case], cwd=burst_root,
                                env=env, capture_output=True, text=True,
                                timeout=90)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("PASS " + case, result.stdout, result.stdout + result.stderr)
        for index in range(1, 9):
            cycle = burst_root / f"cycle-{index:02d}"
            source = cycle / f"source.{extension}"
            saved = cycle / f"saved.{extension}"
            self.assertTrue(saved.is_file(), "Save As target is missing")
            source_bytes = source.read_bytes()
            saved_bytes = saved.read_bytes()
            if format_id == "svg":
                self.assertIn(b'x2="9"', source_bytes,
                              "external SVG update did not reach the old source")
                saved_root = ET.fromstring(saved_bytes)
                saved_line = next(node for node in saved_root.iter()
                                  if node.tag.endswith(("line", "polyline")))
                saved_points = ([(float(saved_line.get("x1")),
                                  float(saved_line.get("y1"))),
                                 (float(saved_line.get("x2")),
                                  float(saved_line.get("y2")))]
                                if saved_line.tag.rsplit("}", 1)[-1] == "line" else
                                points(saved_line.get("points", "")))
                self.assertEqual(len(saved_points), 2)
                for actual, expected in zip(saved_points, ((1.0, 16.0),
                                                           (7.0, 13.0))):
                    self.assertAlmostEqual(actual[0], expected[0], delta=0.001,
                                           msg="stale SVG callback changed saved x")
                    self.assertAlmostEqual(actual[1], expected[1], delta=0.001,
                                           msg="stale SVG callback changed saved y")
                expected_files = {source.name, saved.name}
            else:
                self.assertIn(b"(2.25,0);", source_bytes,
                              "external TikZ update did not reach the old source")
                self.assertIn(b"(2,0);", saved_bytes,
                              "stale TikZ callback overwrote the Save As snapshot")
                expected_files = {source.name, saved.name, "acceptance-image.png"}
            self.assertEqual({path.name for path in cycle.iterdir()}, expected_files,
                             "repeated source rebinding left a staged resource")

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
                         ["defs", "polyline", "polygon", "path", "polygon",
                          "text", "text", "image"])

        expected = self.expected["tpx"]
        line, arrow, cubic, rectangle, first_text, second_text, image = children[1:]
        line_oracle, cubic_oracle, rect_oracle, text_oracle, text2_oracle, image_oracle = expected
        self.assertEqual(points(line.get("points")), [
            (line_oracle["start"][0], -line_oracle["start"][1]),
            (line_oracle["end"][0], -line_oracle["end"][1]),
        ])
        self.assertEqual(line.get("stroke"), line_oracle["stroke"])
        self.assertAlmostEqual(float(line.get("stroke-width")),
                               line_oracle["stroke_width"], places=5)
        self.assertEqual(arrow.get("stroke"), line_oracle["stroke"])
        self.assertEqual(arrow.get("fill"), line_oracle["stroke"])
        start, tip = line_oracle["start"], line_oracle["end"]
        dx, dy = start[0] - tip[0], start[1] - tip[1]
        length = math.hypot(dx, dy)
        vx, vy = dx / length, dy / length
        expected_arrow = []
        for local_x, local_y in line_oracle["arrow_local_points"]:
            x = tip[0] + line_oracle["arrow_size_mm"] * (
                local_x * vx - local_y * vy)
            y = tip[1] + line_oracle["arrow_size_mm"] * (
                local_x * vy + local_y * vx)
            expected_arrow.append((x, -y))
        actual_arrow = points(arrow.get("points"))
        self.assertEqual(len(actual_arrow), len(expected_arrow))
        for actual, wanted in zip(actual_arrow, expected_arrow):
            self.assertAlmostEqual(actual[0], wanted[0], delta=0.0002)
            self.assertAlmostEqual(actual[1], wanted[1], delta=0.0002)
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
        self.assertEqual(rectangle.get("stroke"), rect_oracle["stroke"])
        self.assertAlmostEqual(float(rectangle.get("stroke-width")),
                               rect_oracle["stroke_width"], places=5)
        self.assertTrue(rectangle.get("fill", "").startswith("url(#"),
                        "diagonal-cross hatch was not applied to the rectangle")
        pattern_id = rectangle.get("fill")[5:-1]
        pattern = next((item for item in children[0]
                        if item.get("id") == pattern_id), None)
        self.assertIsNotNone(pattern, "hatching definition was not emitted")
        pattern_paths = [item for item in pattern if item.tag.endswith("path")]
        self.assertEqual(len(pattern_paths), 2)
        self.assertEqual(pattern_paths[0].get("fill"), rect_oracle["fill"])
        hatch = pattern_paths[1]
        self.assertEqual((hatch.get("stroke"), hatch.get("fill")),
                         (rect_oracle["hatch_color"], "none"))
        self.assertAlmostEqual(float(hatch.get("stroke-width")), 0.5, places=5)
        hatch_numbers = [float(value) for value in re.findall(
            r"[-+]?(?:\d+\.?\d*|\.\d+)", hatch.get("d", ""))]
        diagonal = rect_oracle["hatching_step_mm"] * math.sqrt(2)
        for actual, wanted in zip(hatch_numbers,
                                  (0, 0, diagonal, diagonal,
                                   diagonal, 0, 0, diagonal)):
            self.assertAlmostEqual(actual, wanted, delta=0.0002)

        for element, oracle, anchor in (
                (first_text, text_oracle, "start"),
                (second_text, text2_oracle, "end")):
            self.assertAlmostEqual(float(element.get("x")),
                                   oracle["position"][0], places=5)
            self.assertAlmostEqual(float(element.get("y")),
                                   -oracle["position"][1], places=5)
            self.assertEqual((element.get("text-anchor"), element.text),
                             (anchor, oracle["plain_text"]))
        self.assertEqual(image.get("{" + XLINK_NS + "}href"),
                         image_oracle["reference"])
        for key, expected_value in (
                ("x", image_oracle["position"][0]),
                ("y", -(image_oracle["position"][1] + image_oracle["size"][1])),
                ("width", image_oracle["size"][0]),
                ("height", image_oracle["size"][1])):
            self.assertAlmostEqual(float(image.get(key)), expected_value, places=5)
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
        raster_width, raster_height, pixels = ppm_pixels(ppm)
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

        view_left, view_top, view_width, view_height = map(
            float, root.get("viewBox").split())

        def colored_pixels_in_svg_bounds(bounds, predicate):
            x0, y0, x1, y1 = bounds
            px0 = max(0, math.floor((x0 - view_left) / view_width * raster_width))
            px1 = min(raster_width, math.ceil((x1 - view_left) / view_width * raster_width))
            py0 = max(0, math.floor((y0 - view_top) / view_height * raster_height))
            py1 = min(raster_height, math.ceil((y1 - view_top) / view_height * raster_height))
            count = 0
            for py in range(py0, py1):
                for px in range(px0, px1):
                    offset = (py * raster_width + px) * 3
                    if predicate(*pixels[offset:offset + 3]):
                        count += 1
            return count

        arrow_x = [point[0] for point in expected_arrow]
        arrow_y = [point[1] for point in expected_arrow]
        arrow_bounds = (min(arrow_x) - 0.2, min(arrow_y) - 0.2,
                        max(arrow_x) + 0.2, max(arrow_y) + 0.2)
        blue_head_pixels = colored_pixels_in_svg_bounds(
            arrow_bounds, lambda r, g, b: r < 100 and 35 < g < 150 and b > 100)
        self.assertGreater(blue_head_pixels, 12,
                           "independent raster lost the arrow head near its expected tip")

        hatch_x = [point[0] for point in expected_corners]
        hatch_y = [point[1] for point in expected_corners]
        hatch_bounds = (min(hatch_x), min(hatch_y), max(hatch_x), max(hatch_y))
        red_hatch_pixels = colored_pixels_in_svg_bounds(
            hatch_bounds, lambda r, g, b: r > 110 and g < 100 and b < 80)
        self.assertGreater(red_hatch_pixels, 20,
                           "independent raster lost the diagonal-cross hatch")

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

        structural_text = structural.read_text(encoding="utf-8")
        structural_local = re.search(
            r"\(1,0\)\s*--\s*\(\s*([-+0-9.eE]+)\s*,\s*([-+0-9.eE]+)\s*\)",
            structural_text)
        self.assertIsNotNone(structural_local,
                             "100-cycle TikZ source lost its edited coordinate")
        structural_xy = tuple(map(float, structural_local.groups()))
        initial_world = self.tikz_scope_point(*source_xy)
        structural_world = self.tikz_scope_point(*structural_xy)
        expected_world = (initial_world[0] + 100 * 0.0131,
                          initial_world[1] - 100 * 0.0073)
        for actual, expected in zip(structural_world, expected_world):
            self.assertAlmostEqual(actual, expected, delta=0.0005,
                                   msg="100 Single-precision cycles drifted")
        final_pdf, final_svg = self.compile_tex_snapshot(source_dir, structural)
        self.assertIn("Label β", self.command(["pdftotext", final_pdf, "-"]))
        final_line = next((element for element in ET.parse(final_svg).getroot().iter()
                           if element.tag.endswith("path") and
                           element.get("stroke-dasharray")), None)
        self.assertIsNotNone(final_line, "final TeX render lost the dashed line")
        final_points = rendered_mm(final_line)
        self.assertEqual(len(final_points), 2)
        for actual, expected in zip(final_points[1], structural_world):
            self.assertAlmostEqual(actual, expected, delta=0.002,
                                   msg="final TeX render disagrees with source oracle")

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

    def test_lcl_svg_external_refresh_then_graphic_save(self):
        _, source = self.run_lcl_source_refresh("svg")
        saved = ET.parse(source).getroot()
        line = next(node for node in saved.iter()
                    if node.tag.endswith(("line", "polyline")))
        if line.tag.rsplit("}", 1)[-1] == "line":
            endpoint = (float(line.get("x2")), float(line.get("y2")))
        else:
            endpoint = points(line.get("points"))[-1]
        self.assertGreater(math.dist(endpoint, (9.0, 13.0)), 0.05,
                           "graphic edit after refresh was not saved")
        self.assertIn("Label β", "".join(node.text or "" for node in saved.iter()))

        external = self.root / "external SVG baseline.svg"
        external.write_bytes((FIXTURES / "svg-workflow.svg").read_bytes().replace(
            b'x2="7"', b'x2="9"'))
        images = []
        for index, path in enumerate((external, source)):
            pdf = self.root / f"refresh-svg-{index}.pdf"
            ppm = self.root / f"refresh-svg-{index}.ppm"
            self.command(["rsvg-convert", "--format", "pdf", "--output", pdf, path])
            self.command(["gs", "-q", "-dSAFER", "-dBATCH", "-dNOPAUSE",
                          "-sDEVICE=ppmraw", "-r144", "-sOutputFile=" + str(ppm), pdf])
            images.append(ppm_pixels(ppm))
        self.assertEqual(images[0][:2], images[1][:2])
        changed = sum(images[0][2][i:i + 3] != images[1][2][i:i + 3]
                      for i in range(0, len(images[0][2]), 3))
        self.assertGreater(changed, 20,
                           "independent SVG render lost the post-refresh graphic edit")

    def test_lcl_tikz_external_refresh_then_graphic_save(self):
        source_dir, source = self.run_lcl_source_refresh("tikz")
        saved_text = source.read_text(encoding="utf-8")
        local_end = re.search(
            r"\(1,0\)\s*--\s*\(\s*([-+0-9.eE]+)\s*,\s*([-+0-9.eE]+)\s*\)",
            saved_text)
        self.assertIsNotNone(local_end, "refreshed TikZ line lost its source coordinates")
        source_xy = tuple(map(float, local_end.groups()))
        self.assertNotEqual(source_xy, (2.25, 0.0),
                            "graphic edit after refresh was not saved")

        render_dir = self.root / "independent refreshed TikZ render"
        render_dir.mkdir()
        rendered_source = render_dir / "refreshed source.tex"
        shutil.copy2(source, rendered_source)
        shutil.copy2(TIKZ_FIXTURES / "acceptance-image.png", render_dir)
        pdf, svg = self.compile_tex_snapshot(render_dir, rendered_source)
        rendered_line = next((element for element in ET.parse(svg).getroot().iter()
                              if element.tag.endswith("path") and
                              element.get("stroke-dasharray")), None)
        self.assertIsNotNone(rendered_line, "trusted TeX render lost refreshed line")
        values = [float(value) * 25.4 / 72 for value in re.findall(
            r"[-+]?(?:\d+\.?\d*|\.\d+)", rendered_line.get("d", ""))]
        self.assertEqual(len(values), 4)
        actual = list(zip(values[::2], values[1::2]))
        expected = (self.tikz_scope_point(1, 0), self.tikz_scope_point(*source_xy))
        for point, oracle in zip(actual, expected):
            self.assertAlmostEqual(point[0], oracle[0], delta=0.002)
            self.assertAlmostEqual(point[1], oracle[1], delta=0.002)
        self.assertGreater(math.dist(expected[1], self.tikz_scope_point(2.25, 0)),
                           0.05, "TeX renderer did not reflect the post-refresh edit")
        self.assertIn("Label β", self.command(["pdftotext", pdf, "-"]))

    def test_lcl_svg_source_binding_bursts_ignore_stale_callbacks(self):
        self.run_lcl_binding_burst("svg")

    def test_lcl_tikz_source_binding_bursts_ignore_stale_callbacks(self):
        self.run_lcl_binding_burst("tikz")

    def test_lcl_document_write_failures_preserve_source_and_session(self):
        source_dir = self.root / "write failure source β"
        source_dir.mkdir()
        source = source_dir / "nested source.tex"
        shutil.copy2(TIKZ_FIXTURES / "independent-scene.tex", source)
        shutil.copy2(TIKZ_FIXTURES / "acceptance-image.png", source_dir)
        fault_root = self.root / "write failure destinations"
        fault_root.mkdir()
        env = self.env.copy()
        env.update(TPX_SOURCE_FILE=str(source),
                   TPX_SOURCE_FAULT_ROOT=str(fault_root))
        result = subprocess.run([str(RUNTIME_BINARY),
                                 "source-format-write-failures"],
                                cwd=source_dir, env=env, capture_output=True,
                                text=True, timeout=60)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("PASS source-format-write-failures", result.stdout,
                      result.stdout + result.stderr)


if __name__ == "__main__":
    class AcceptanceResult(unittest.TextTestResult):
        def __init__(self, *args, **kwargs):
            super().__init__(*args, **kwargs)
            self.case_results = []

        @staticmethod
        def finish_case(test, failed):
            directory = getattr(test, "directory", None)
            if directory is None:
                return ""
            retained_path = ""
            if failed:
                retained_root = Path(os.environ.get(
                    "TPX_ACCEPTANCE_ARTIFACT_ROOT",
                    ROOT / "obj" / "source-acceptance" / "retained"))
                case_name = re.sub(r"[^A-Za-z0-9_.-]+", "_", test.id())
                retained = retained_root / case_name
                retained.parent.mkdir(parents=True, exist_ok=True)
                shutil.copytree(test.root, retained, dirs_exist_ok=True)
                retained_path = str(retained)
            directory.cleanup()
            return retained_path

        def addSuccess(self, test):
            self.finish_case(test, False)
            super().addSuccess(test)
            self.case_results.append({"case": test.id(), "status": "passed"})

        def addFailure(self, test, err):
            artifact = self.finish_case(test, True)
            super().addFailure(test, err)
            self.case_results.append({"case": test.id(), "status": "failed",
                                      "detail": "".join(traceback.format_exception(*err)),
                                      "artifact": artifact})

        def addError(self, test, err):
            artifact = self.finish_case(test, True)
            super().addError(test, err)
            self.case_results.append({"case": test.id(), "status": "error",
                                      "detail": "".join(traceback.format_exception(*err)),
                                      "artifact": artifact})

        def addSkip(self, test, reason):
            self.finish_case(test, False)
            super().addSkip(test, reason)
            self.case_results.append({"case": test.id(), "status": "skipped",
                                      "detail": reason})

    suite = unittest.defaultTestLoader.loadTestsFromTestCase(SourceAcceptance)
    discovered = suite.countTestCases()
    if discovered <= 0:
        raise SystemExit("source acceptance discovered no test cases")
    runner = unittest.TextTestRunner(verbosity=2, resultclass=AcceptanceResult)
    result = runner.run(suite)
    report = {
        "suite": "source-acceptance",
        "discovered": discovered,
        "selected": discovered,
        "executed": result.testsRun,
        "passed": len(result.successes) if hasattr(result, "successes") else
                  sum(case["status"] == "passed" for case in result.case_results),
        "failed": len(result.failures),
        "errors": len(result.errors),
        "skipped": len(result.skipped),
        "cases": result.case_results,
    }
    report_path = os.environ.get("TPX_ACCEPTANCE_REPORT")
    if report_path:
        report_file = Path(report_path)
        report_file.parent.mkdir(parents=True, exist_ok=True)
        report_file.write_text(json.dumps(report, indent=2) + "\n",
                               encoding="utf-8")
    print("SOURCE_ACCEPTANCE_REPORT=" + json.dumps(report, sort_keys=True))
    raise SystemExit(0 if result.wasSuccessful() and
                     result.testsRun == discovered else 1)

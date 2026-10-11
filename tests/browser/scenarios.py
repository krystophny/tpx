"""Behavioral scenarios for the TpX browser playtest.

Every scenario acts through real Chromium input events and asserts on rendered
canvas pixels, the frame counter or the window title. Coordinates come from the
harness measurement of the live layout (the form is resized to the browser
surface, so LFM numbers are shifted); the tool pitch of 22 px is verified by a
responsive-button sweep in tests/browser/../web/discover-tools.py.
"""


def diff(a, b):
    return sum(1 for i in range(0, len(a), 4)
               if abs(a[i] - b[i]) > 8 or abs(a[i+1] - b[i+1]) > 8 or abs(a[i+2] - b[i+2]) > 8)


# Left tool window. Coordinates include Panel31's form offset; app.at converts
# them through the live canvas size before dispatching browser input.
TOOL_X = 13
TOOL_PITCH = 22
# ToolBar2 starts at y=71 in Panel31; its first button center is y=83.
TOOL_FIRST = 83          # InsertLine


def tool(name):
    """Y of a tool button in the left tool window, from the LFM order."""
    order = ['line', 'rect', 'circle', 'ellipse', 'arc', 'sector', 'segment',
             'polyline', 'polygon', 'curve', 'closedcurve', 'bezier',
             'closedbezier', 'text', 'star', 'symbol']
    return TOOL_FIRST + order.index(name) * TOOL_PITCH


def box(app):
    v = app.viewport()
    return v, v['x'] + v['w'] // 2, v['y'] + v['h'] // 2


def region(cx, cy, w=320, h=240):
    return [cx - w // 2, cy - h // 2, w, h]


def park(app, v):
    """Take the pointer off the canvas so the crosshair is erased.

    The viewport paints full-width crosshair lines under the pointer, which
    would add width+height leftover pixels to every undo comparison. Leaving
    the canvas fires the leave notification, the viewport erases its lines and
    the frame is clean again; parked anywhere on the paper they stay visible.
    """
    b = app.box()
    app.page.mouse.move(b['x'] + b['w'] / 2, max(2.0, b['y'] - 20))
    app.page.wait_for_timeout(350)


def focus_document(app, v):
    """Click an empty corner of the paper so the document owns the keyboard.

    At startup focus sits on a properties combo box, exactly as on the desktop;
    a real user clicks the drawing before expecting shortcuts to act on it.
    Clicking a toolbar button must not take that focus away (fixed in the WASI
    CustomDrawn backend), which is what makes Ctrl+Z and Escape work here.
    """
    app.click(v['x'] + v['w'] - 60, v['y'] + 40)
    app.page.wait_for_timeout(200)


def s_default_view(app):
    v, cx, cy = box(app)
    reg = region(cx, cy, 300, 220)
    frame = app.pixels(reg)
    n = len(frame) // 4
    white = sum(1 for i in range(0, len(frame), 4)
                if frame[i] == 255 and frame[i+1] == 255 and frame[i+2] == 255)
    if white < n * 0.9:
        raise AssertionError(f'paper not painted: {white}/{n} white pixels')
    if app.frames() < 1:
        raise AssertionError('nothing was presented')
    return f'{v["w"]}x{v["h"]} canvas, {white*100//n}% white paper, {app.frames()} frames'


def s_viewport_fit(app):
    v, cx, cy = box(app)
    if app.frames() < 1:
        raise AssertionError('no frame presented')
    if v['w'] > app.box()['w'] + 2 or v['h'] > app.box()['h'] + 2:
        raise AssertionError(f'paper {v["w"]}x{v["h"]} exceeds client {app.box()["w"]}x{app.box()["h"]}')
    return f'paper {v["w"]}x{v["h"]} inside client'


def s_resize_follows(app):
    if app.frames() < 1:
        raise AssertionError('no frame presented')
    before = app.box()['nw']
    app.resize(1000, 700)
    after = app.box()['nw']
    if after >= before:
        raise AssertionError(f'canvas did not follow viewport: {before} -> {after}')
    return f'canvas {before} -> {after}'


def s_draw_drag(app):
    """A drag with the line tool armed leaves persistent ink."""
    v, cx, cy = box(app)
    reg = region(cx, cy)
    app.menu('Insert', 'Insert line')
    clean = app.pixels(reg)
    app.drag(cx - 90, cy - 60, cx + 90, cy + 60)
    after = app.pixels(reg)
    ink = diff(clean, after)
    if ink < 150:
        raise AssertionError(f'line tool drew only {ink} pixels')
    return f'{ink} ink pixels'


def s_draw_click(app):
    """The rectangle tool stays armed, so a second object appears after one."""
    v, cx, cy = box(app)
    reg = region(cx, cy)
    app.menu('Insert', 'Insert rectangle')
    clean = app.pixels(reg)
    app.click(cx - 40, cy - 30)
    app.click(cx + 60, cy + 40)
    app.key('Escape')
    park(app, v)
    two = app.pixels(reg)
    if diff(clean, two) < 80:
        raise AssertionError(f'two-click drawing produced only {diff(clean, two)} pixels')
    objects = drawing_xml(app.save('two-click.tpx'))
    if len(objects) != 1 or float(objects[0].get('w', '0')) <= 0 or float(objects[0].get('h', '0')) <= 0:
        raise AssertionError('two clicks did not save a rectangle with positive dimensions')
    return 'two-click rectangle renders and survives save'


def s_path_completion(app):
    """Double-click commits each variable-length path through native LCL input."""
    v, cx, cy = box(app)
    paths = [('Insert polyline', 'polyline', False),
             ('Insert polygon', 'polygon', False),
             ('Insert curve', 'curve', False),
             ('Insert closed curve', 'curve', True),
             ('Insert Bezier path', 'bezier', False),
             ('Insert closed Bezier path', 'bezier', True)]
    for i, (caption, _, _) in enumerate(paths):
        x, y = cx - 240 + i % 3 * 170, cy - 140 + i // 3 * 190
        app.menu('Insert', caption)
        for dx, dy in [(0, 0), (50, -40), (110, 10)]:
            app.click(x + dx, y + dy)
        px, py = app.at(x + 70, y + 80)
        app.page.mouse.dblclick(px, py, delay=80)
        app.key('Escape')
    objects = drawing_xml(app.save('completed-paths.tpx'))
    if len(objects) != len(paths):
        raise AssertionError(f'double-click committed {len(objects)} of {len(paths)} paths')
    for obj, (caption, tag, closed) in zip(objects, paths):
        # The loader accepts these historical aliases for the same path types.
        actual = {'path': 'polyline', 'smooth': 'curve'}.get(obj.tag, obj.tag)
        points = (obj.text or '').split()
        if actual != tag or (obj.get('closed', '0') != '0') != closed or len(set(points)) < 3:
            raise AssertionError(f'{caption} saved invalid geometry: {obj.tag} {obj.attrib} {obj.text}')
    return 'double-click commits all six open/closed path tools with saved geometry'


def s_draw_cancel(app):
    """Escape during a drag cancels the object instead of inserting it."""
    v, cx, cy = box(app)
    reg = [cx - 140, cy - 110, 300, 240]
    focus_document(app, v)
    park(app, v)
    base = app.pixels(reg)
    app.menu('Insert', 'Insert rectangle')
    park(app, v)
    if diff(base, app.pixels(reg)) > 40:
        raise AssertionError('arming the tool already painted on the paper')
    app.move(cx - 90, cy - 60)
    app.page.wait_for_timeout(200)
    app.press()
    for i in range(6):
        app.move(cx - 90 + i * 24, cy - 60 + i * 16)
    app.page.wait_for_timeout(250)
    live = diff(base, app.pixels(reg))
    if live < 200:
        raise AssertionError(f'drag preview showed only {live} pixels')
    app.key('Escape')
    app.page.wait_for_timeout(250)
    app.release()
    park(app, v)
    left = diff(base, app.pixels(reg))
    if left > 40:
        raise AssertionError(f'Escape left {left} of {live} pixels')
    # control: the same gesture without Escape inserts an object that stays, so
    # the cancel assertion cannot pass by accident.
    app.menu('Insert', 'Insert rectangle')
    app.move(cx - 90, cy - 60)
    app.press()
    for i in range(6):
        app.move(cx - 90 + i * 24, cy - 60 + i * 16)
    app.release()
    park(app, v)
    inserted = diff(base, app.pixels(reg))
    if inserted < 200:
        raise AssertionError(f'control insert showed only {inserted} pixels')
    return f'preview {live} px, cancelled to {left} px, control insert {inserted} px'


def s_crosshair(app):
    """Pointer motion must update the coordinate readout in the status bar.

    The browser paints the whole window surface, so hovering does not dirty the
    paper; the observable effect of a move is the status-bar position readout,
    which is the same feedback a native build gives.
    """
    v, cx, cy = box(app)
    b = app.box()
    status = [0, int(b['nh']) - 24, int(b['nw']), 22]
    base = app.pixels(status)
    moved = 0
    for dx in (0, 40, -60, 120):
        app.move(cx + dx, cy)
        moved = max(moved, diff(base, app.pixels(status)))
    if moved < 40:
        raise AssertionError(f'pointer move changed the status bar by only {moved} pixels')
    return f'status readout updated {moved} px over 4 moves'


def s_wheel_zoom(app):
    v, cx, cy = box(app)
    reg = region(cx, cy, 360, 260)
    base = app.pixels(reg)
    app.wheel(cx, cy, -240)
    zoom_in = app.pixels(reg)
    app.wheel(cx, cy, 240)
    zoom_out = app.pixels(reg)
    if diff(base, zoom_in) < 500:
        raise AssertionError(f'wheel forward changed only {diff(base, zoom_in)} pixels')
    if diff(base, zoom_out) > 1000:
        raise AssertionError(f'wheel back did not restore the view ({diff(base, zoom_out)} px differ)')
    return f'zoom in {diff(base, zoom_in)} px, restored to {diff(base, zoom_out)} px'


def s_keyboard_shortcut(app):
    """Ctrl+Z then Ctrl+Shift+Z undo and redo a real drawn object."""
    v, cx, cy = box(app)
    reg = region(cx, cy)
    focus_document(app, v)
    app.menu('Insert', 'Insert line')
    clean = app.pixels(reg)
    app.drag(cx - 100, cy - 50, cx + 100, cy + 50)
    park(app, v)
    drawn = app.pixels(reg)
    ink = diff(clean, drawn)
    if ink < 150:
        raise AssertionError(f'setup drag drew only {ink} pixels')
    app.key('Control+z')
    park(app, v)
    left = diff(clean, app.pixels(reg))
    if left > 120:
        raise AssertionError(f'Ctrl+Z left {left} of {ink} pixels')
    app.key('Control+Shift+z')
    app.move(v['x'] + 4, v['y'] + 4)
    app.page.wait_for_timeout(200)
    redone = diff(clean, app.pixels(reg))
    if redone < 150:
        raise AssertionError(f'Ctrl+Shift+Z did not restore the object ({redone} px)')
    return f'drew {ink} px, undone to {left} px, redone {redone} px'


def s_text_input(app):
    """Create, save, reopen and edit Unicode text without a modal dialog."""
    import re

    v, cx, cy = box(app)
    text = 'WASM Grüße λ 漢字 🐈 & &#946;'
    browser_dialogs = []

    def dismiss_and_record(dialog):
        browser_dialogs.append(dialog.type)
        dialog.dismiss()

    app.page.on('dialog', dismiss_and_record)

    def rendered_text_point():
        paper = app.viewport()
        pixels = app.pixels([paper['x'], paper['y'], paper['w'], paper['h']])
        width, height = paper['w'], paper['h']
        dark = bytearray(width * height)
        for pixel in range(0, len(pixels), 4):
            if (pixels[pixel] < 96 and pixels[pixel + 1] < 96 and
                    pixels[pixel + 2] < 96):
                dark[pixel // 4] = 1
        seen = bytearray(len(dark))
        largest = []
        for first, value in enumerate(dark):
            if not value or seen[first]:
                continue
            component = [first]
            seen[first] = 1
            for current in component:
                x, y = current % width, current // width
                for next_y in range(max(0, y - 1), min(height, y + 2)):
                    for next_x in range(max(0, x - 1), min(width, x + 2)):
                        neighbor = next_y * width + next_x
                        if dark[neighbor] and not seen[neighbor]:
                            seen[neighbor] = 1
                            component.append(neighbor)
            if len(component) > len(largest):
                largest = component
        if len(largest) < 20:
            raise AssertionError('reopened text has no glyph-sized rendered component')
        center_x = sum(index % width for index in largest) // len(largest)
        center_y = sum(index // width for index in largest) // len(largest)
        return paper['x'] + center_x, paper['y'] + center_y

    def reopen_existing_text():
        app.page.mouse.dblclick(*app.at(*rendered_text_point()), delay=80)

    before_prompt_controls = app.page.locator(
        '#lcl-dom input, #lcl-dom textarea').count()
    app.menu('Insert', 'Insert text')
    after_prompt_controls = app.page.locator(
        '#lcl-dom input, #lcl-dom textarea').count()
    if (browser_dialogs or app.page.get_by_role('dialog').count() or
            app.page.locator('dialog[open]').count() or
            after_prompt_controls > before_prompt_controls):
        raise AssertionError('Insert text opened a modal dialog or prompt control')
    app.click(cx-60, cy-20)
    editor = app.page.get_by_role('textbox', name='Text (canvas)', exact=True)
    editor.wait_for()
    if app.page.locator('dialog[open]').count():
        raise AssertionError('creating text opened a modal dialog')
    editor.click()
    shortcut_letters = 'lrceapgtb'
    typed = ''
    for letter in shortcut_letters:
        app.page.keyboard.type(letter)
        typed += letter
        if editor.input_value() != typed:
            raise AssertionError(f'canvas shortcut intercepted typed {letter!r}: '
                                 f'{editor.input_value()!r}')
    editor.fill('')
    app.page.keyboard.type(text)
    if editor.input_value() != text:
        raise AssertionError('Unicode browser input changed before commit: '
                             f'{editor.input_value()!r}')
    app.page.get_by_role('button', name='Done', exact=True).click()
    app.page.wait_for_timeout(150)
    if editor.count():
        raise AssertionError(f'Done did not settle the inline text session: {app.errors!r}')
    if app.page.locator('dialog[open]').count():
        raise AssertionError('committing inline text opened a modal dialog')

    saved = app.save('text-roundtrip.tpx')
    # The generated TikZ block can contain local-codepage bytes even though
    # the TpX XML text attributes are ASCII numeric references. Preserve the
    # downloaded bytes while inspecting or cloning the fixture.
    raw = saved.read_bytes().decode('latin-1')
    tag = re.search(r'<text\b[^>]*>', raw)
    if tag is None or 't=' not in tag.group(0):
        raise AssertionError('saved drawing has no text object')

    saved_objects = drawing_xml(saved)
    saved_texts = [obj for obj in saved_objects if obj.tag == 'text']
    if (len(saved_texts) != 1 or
            decode_tpx_text_once(saved_texts[0].attrib.get('t', '')) != text):
        raise AssertionError('saved TpX Text value changed before reload')

    app.menu('File', 'Open')
    dialog = app.page.get_by_role('dialog')
    dialog.get_by_label('Choose file').set_input_files(saved)
    dialog.get_by_role('button', name='OK', exact=True).click()
    app.page.wait_for_timeout(300)
    reopen_existing_text()
    editor = app.page.get_by_role('textbox', name='Text (canvas)', exact=True)
    editor.wait_for()
    if editor.input_value() != text:
        raise AssertionError(f'save/reopen changed Unicode text: {editor.input_value()!r}')
    app.page.keyboard.type('!')
    if editor.input_value() != text + '!':
        raise AssertionError('Unicode text caret was not at the end after reopening')
    app.page.keyboard.press('Backspace')
    if editor.input_value() != text:
        raise AssertionError('native text editing changed Unicode caret content')

    app.page.get_by_role('button', name='Done', exact=True).click()
    app.click(TOOL_X, tool('text'))
    app.click(*rendered_text_point())
    editor = app.page.get_by_role('textbox', name='Text (canvas)', exact=True)
    editor.wait_for()
    if editor.input_value() != text:
        raise AssertionError('Text tool click did not edit the existing label')

    # A deliberate final line break is user content and must survive the
    # actual save/reopen path.
    multiline = 'first line\nsecond line\n'
    editor.fill(multiline)
    app.page.get_by_role('button', name='Done', exact=True).click()
    multiline_saved = app.save('text-trailing-newline.tpx')
    multiline_xml = multiline_saved.read_bytes().decode('latin-1')
    multiline_tag = re.search(r'<text\b[^>]*>', multiline_xml)
    if len(re.findall(r'<text\b', multiline_xml)) != 1 or not multiline_tag:
        raise AssertionError('Text-tool editing created a duplicate label')
    initial_attrs = dict(re.findall(r'([A-Za-z][A-Za-z0-9]*)="([^"]*)"',
                                   tag.group(0)))
    multiline_attrs = dict(re.findall(
        r'([A-Za-z][A-Za-z0-9]*)="([^"]*)"', multiline_tag.group(0)))
    for name in ('x', 'y', 'h', 'halign', 'rotdeg'):
        if multiline_attrs.get(name) != initial_attrs.get(name):
            raise AssertionError(f'text edit changed saved {name}: '
                                 f'{initial_attrs.get(name)!r} -> '
                                 f'{multiline_attrs.get(name)!r}')
    app.menu('File', 'Open')
    dialog = app.page.get_by_role('dialog')
    dialog.get_by_label('Choose file').set_input_files(multiline_saved)
    dialog.get_by_role('button', name='OK', exact=True).click()
    app.page.wait_for_timeout(300)
    reopen_existing_text()
    editor = app.page.get_by_role('textbox', name='Text (canvas)', exact=True)
    editor.wait_for()
    if editor.input_value() != multiline:
        raise AssertionError(f'save/reopen changed an intentional trailing newline: '
                             f'{editor.input_value()!r}')

    # Add a TeX source to the saved fixture, edit only that field, and compare
    # the serialized plain-text attribute before and after the source edit.
    tex_tag = tag.group(0)
    insert_at = -2 if tex_tag.endswith('/>') else -1
    tex_fixture = app.output / 'text-with-tex-source.tpx'
    tex_fixture.write_bytes(raw.replace(tex_tag,
        tex_tag[:insert_at] + ' tex="\\alpha"' + tex_tag[insert_at:], 1)
        .encode('latin-1'))
    app.page.get_by_role('button', name='Cancel', exact=True).click()
    app.menu('File', 'Open')
    dialog = app.page.get_by_role('dialog')
    dialog.get_by_label('Choose file').set_input_files(tex_fixture)
    dialog.get_by_role('button', name='OK', exact=True).click()
    app.page.wait_for_timeout(300)
    reopen_existing_text()
    tex_editor = app.page.get_by_role('textbox', name='TeX source (canvas)', exact=True)
    tex_editor.wait_for()
    if tex_editor.input_value() != r'\alpha':
        raise AssertionError('editing did not select the independent TeX source')
    tex_editor.fill(r'\beta')
    app.page.get_by_role('button', name='Done', exact=True).click()
    tex_saved = app.save('text-tex-source.tpx').read_bytes().decode('latin-1')
    final_tag = re.search(r'<text\b[^>]*>', tex_saved)
    before_attrs = dict(re.findall(r'([A-Za-z][A-Za-z0-9]*)="([^"]*)"',
                                   tag.group(0)))
    after_attrs = dict(re.findall(r'([A-Za-z][A-Za-z0-9]*)="([^"]*)"',
                                  final_tag.group(0))) if final_tag else {}
    if not final_tag or after_attrs.get('t') != before_attrs.get('t') or after_attrs.get('tex') != r'\beta':
        raise AssertionError('editing TeX source changed plain Text or failed to save')
    for name in ('x', 'y', 'h', 'halign', 'rotdeg'):
        if after_attrs.get(name) != before_attrs.get(name):
            raise AssertionError(f'TeX edit changed saved {name}: '
                                 f'{before_attrs.get(name)!r} -> '
                                 f'{after_attrs.get(name)!r}')

    # Escape cancels a newly placed provisional label without leaving a blank
    # object in the document.
    app.menu('Insert', 'Insert text')
    app.click(cx+100, cy+70)
    app.page.get_by_role('textbox', name='Text (canvas)', exact=True).wait_for()
    app.page.keyboard.press('Escape')
    app.page.wait_for_timeout(150)
    cancelled = re.findall(r'<text\b',
                           app.save('text-cancel.tpx').read_bytes().decode('latin-1'))
    if len(cancelled) != 1:
        raise AssertionError(f'cancel left a phantom text object: {len(cancelled)}')
    if app.page.locator('dialog[open]').count():
        raise AssertionError('inline editing displayed an unexpected dialog')
    return 'Unicode Text and independent TeX source survive edit/save/reopen; Escape leaves no phantom label'


def s_idle_stops_repainting(app):
    if app.frames() < 1:
        raise AssertionError('no frames presented')
    f1 = app.idle_frames(3.0)
    if f1 > 2:
        raise AssertionError(f'{f1} frames presented during 3 s of idle time')
    return f'{f1} frames in 3 s idle'


def s_title_tracks_document(app):
    v, cx, cy = box(app)
    app.menu('Insert', 'Insert line')
    app.drag(cx-100, cy-40, cx+100, cy+40)
    app.key('Escape')
    app.save('browser-title.tpx')
    if 'browser-title.tpx' not in app.title():
        raise AssertionError(f'saved document title is missing: {app.title()}')
    return app.title()


def decode_tpx_text_once(value):
    """Decode TpX character references after the XML parser's one pass."""
    import re

    def decode(match):
        codepoint = int(match.group(1))
        return chr(codepoint) if codepoint <= 0x10FFFF else match.group(0)

    decoded = re.sub(r'&#([0-9]+);', decode, value)
    scalars = []
    index = 0
    while index < len(decoded):
        codepoint = ord(decoded[index])
        if (0xD800 <= codepoint <= 0xDBFF and index + 1 < len(decoded) and
                0xDC00 <= ord(decoded[index + 1]) <= 0xDFFF):
            low = ord(decoded[index + 1])
            scalars.append(chr(0x10000 + ((codepoint - 0xD800) << 10) +
                               (low - 0xDC00)))
            index += 2
        else:
            scalars.append(decoded[index])
            index += 1
    return ''.join(scalars)


def drawing_xml(path):
    import xml.etree.ElementTree as ET
    xml = []
    for line in path.read_bytes().splitlines():
        if line.startswith(b'%'):
            xml.append(line[1:])
            if line.startswith(b'%</TpX>') or (
                    line.startswith(b'%<TpX ') and line.endswith(b'/>')):
                return ET.fromstring(b'\n'.join(xml))
    raise AssertionError('saved file has no TpX drawing')


def same_drawing_xml(left, right):
    import math

    if len(left) != len(right):
        return False
    for original, reopened in zip(left, right):
        if original.tag != reopened.tag:
            return False
        if (original.text or '').strip() != (reopened.text or '').strip():
            return False
        names = original.attrib.keys() | reopened.attrib.keys()
        for name in names:
            value = original.get(name, '1' if name == 'lw' else None)
            actual = reopened.get(name, '1' if name == 'lw' else None)
            if value is None or actual is None:
                return False
            try:
                left_value, right_value = float(value), float(actual)
                if original.tag == 'arc' and name in ('a1', 'a2'):
                    equal = math.isclose(math.remainder(left_value - right_value, math.tau),
                                         0.0, rel_tol=1e-9, abs_tol=1e-5)
                else:
                    equal = math.isclose(left_value, right_value, rel_tol=1e-9, abs_tol=1e-9)
            except ValueError:
                equal = value == actual
            if not equal:
                return False
        if original.tag == 'arc':
            left_sweep = float(original.get('a2')) - float(original.get('a1'))
            right_sweep = float(reopened.get('a2')) - float(reopened.get('a1'))
            if not math.isclose(left_sweep, right_sweep, rel_tol=1e-9, abs_tol=1e-9):
                return False
    return True


def _ink(pixels, w):
    """Centroid and count of non-white pixels in an RGBA row-major region."""
    sx = sy = n = 0
    for i in range(0, len(pixels), 4):
        if pixels[i] < 200 and pixels[i+1] < 200 and pixels[i+2] < 200:
            px = (i // 4) % w
            py = (i // 4) // w
            sx += px
            sy += py
            n += 1
    if n == 0:
        return None, None, 0
    return sx / n, sy / n, n


SELECT_Y = 104          # select / pick tool row, measured


def s_edit_move_delete(app):
    v, cx, cy = box(app)
    app.menu('Insert', 'Insert rectangle')
    app.drag(cx-90, cy-60, cx+60, cy+40)
    app.key('Escape')
    before = drawing_xml(app.save('before-move.tpx'))
    if len(before) != 1:
        raise AssertionError(f'expected one drawn object, got {len(before)}')
    app.menu('Edit', 'Select all')
    app.drag(cx-90, cy-10, cx-20, cy+30)
    after = drawing_xml(app.save('after-move.tpx'))
    if len(after) != 1:
        raise AssertionError('move changed the object count')
    dx = float(after[0].get('x'))-float(before[0].get('x'))
    dy = float(after[0].get('y'))-float(before[0].get('y'))
    if abs(dx) < 1 or abs(dy) < 1:
        raise AssertionError(f'saved object did not move: {dx}, {dy}')
    app.key('Delete')
    deleted = drawing_xml(app.save('after-delete.tpx'))
    if len(deleted):
        raise AssertionError('Delete left objects in the saved drawing')
    return f'moved by {dx:g}, {dy:g} drawing units; Delete removed the object'


def s_area_select(app):
    """Dragging blank paper selects enclosed objects and changes the cursor."""
    v, cx, cy = box(app)
    app.menu('Insert', 'Insert rectangle')
    app.drag(cx-30, cy-20, cx+30, cy+20)
    app.key('Escape')
    app.menu('Insert', 'Insert circle')
    app.drag(cx+220, cy, cx+250, cy)
    app.key('Escape')
    app.move(cx-100, cy-100)
    app.press()
    app.move(cx-75, cy-75)
    app.page.wait_for_function('getComputedStyle(document.querySelector("#lcl")).cursor === "crosshair"')
    app.move(cx+100, cy+100)
    app.release()
    app.page.wait_for_function('getComputedStyle(document.querySelector("#lcl")).cursor === "default"')
    app.key('Delete')
    objects = drawing_xml(app.save('area-selection.tpx'))
    if len(objects) != 1 or objects[0].tag != 'circle':
        raise AssertionError('area selection did not delete only the enclosed rectangle')
    return 'blank-paper drag uses crosshair, restores cursor and selects only enclosed objects'


def s_open_roundtrip(app):
    v, cx, cy = box(app)
    app.menu('Insert', 'Insert rectangle')
    app.drag(cx-90, cy-60, cx+60, cy+40)
    app.key('Escape')
    saved = app.save('open-roundtrip.tpx')
    before = drawing_xml(saved)
    if len(before) != 1:
        raise AssertionError('initial Save As did not retain the drawing')

    def ordinary_save(filename, expected_count):
        with app.page.expect_download(timeout=5000) as saved_download:
            app.menu('File', 'Save')
        path = app.output / filename
        saved_download.value.save_as(path)
        objects = drawing_xml(path)
        if len(objects) != expected_count:
            raise AssertionError(
                f'ordinary save produced {len(objects)} objects; '
                f'expected {expected_count}')
        return path, objects

    for offset in (0, 70):
        app.menu('Insert', 'Insert rectangle')
        app.drag(cx-30+offset, cy-20, cx+30+offset, cy+40)
        app.key('Escape')
        expected_count = len(before) + 1
        saved, before = ordinary_save(
            f'open-roundtrip-save-{expected_count}.tpx', expected_count)
    app.menu('File', 'New')
    app.menu('File', 'Open')
    dialog = app.page.get_by_role('dialog')
    dialog.get_by_label('Choose file').set_input_files(saved)
    dialog.get_by_role('button', name='OK', exact=True).click()
    app.page.wait_for_timeout(300)
    after = drawing_xml(app.save('reopened.tpx'))
    if len(after) != len(before) or [c.attrib for c in after] != [c.attrib for c in before]:
        raise AssertionError('open/save changed the drawing geometry')

    invalid = app.output / 'open-invalid.tpx'
    invalid.write_text('%<TpX v="5">\n%<line x1="0"')
    app.menu('File', 'Open')
    dialog = app.page.get_by_role('dialog')
    dialog.get_by_label('Choose file').set_input_files(invalid)
    dialog.get_by_role('button', name='OK', exact=True).click()
    error_dialog = app.page.locator('dialog').filter(has_text='Can not open')
    error_dialog.wait_for()
    error_dialog.get_by_role('button', name='OK', exact=True).click()
    after_failure = drawing_xml(app.save('open-invalid-preserved.tpx'))
    if [c.attrib for c in after_failure] != [c.attrib for c in before]:
        raise AssertionError('failed document open changed the current drawing')
    return ('ordinary saves after Save As stay conflict-free; uploaded drawings '
            'reopen unchanged and malformed uploads preserve the accepted scene')


def s_open_examples(app):
    examples = (
        ('Quadratic function', 'quadratic-function.TpX', 16, '$f(x)=x^2-1$'),
        ('Vector addition', 'vector-addition.TpX', 14, r'$\vec{A}$'),
        ('Double-slit diagram', 'double-slit.TpX', 27, r'$\Delta r=0$'),
    )
    blank = app.pixels()
    for title, filename, expected_count, formula in examples:
        link = app.page.get_by_role('link', name=title, exact=True)
        with app.page.expect_download(timeout=5000) as downloaded:
            link.click()
        source = app.output / filename
        downloaded.value.save_as(source)
        document = drawing_xml(source)
        if len(document) != expected_count:
            raise AssertionError(f'{filename} has {len(document)} scene objects')
        if not any(item.tag == 'text' and item.get('tex') == formula for item in document):
            raise AssertionError(f'{filename} lost its math source label')
        if any(item.tag == 'bitmap' for item in document):
            raise AssertionError(f'{filename} depends on a bitmap')

        app.menu('File', 'Open')
        dialog = app.page.get_by_role('dialog')
        dialog.get_by_label('Choose file').set_input_files(source)
        dialog.get_by_role('button', name='OK', exact=True).click()
        app.page.wait_for_timeout(300)
        after = app.pixels()
        if diff(blank, after) < 100:
            raise AssertionError(f'{filename} did not render a scene on the canvas')
        fallback = after
        app.menu('View', 'Trust TeX preview for this document')
        app.page.wait_for_function(
            "performance.getEntriesByType('resource').some(e=>e.name.endsWith('/tex-svg.bundle.js'))",
            timeout=10000)
        app.page.wait_for_timeout(500)
        if diff(fallback, app.pixels()) < 10:
            raise AssertionError(f'{filename} trusted TeX preview did not replace fallback labels')
        saved = drawing_xml(app.save('opened-' + filename))
        if len(saved) != expected_count:
            raise AssertionError(f'{filename} changed during browser open/save')
        if not same_drawing_xml(document, saved):
            raise AssertionError(f'{filename} geometry changed during browser open/save')
        blank = after
    return 'all three downloads open through File → Open, render, and save with their scene geometry and TeX labels intact'


def s_export_formats(app):
    v, cx, cy = box(app)
    app.menu('Insert', 'Insert rectangle')
    app.drag(cx-70, cy-50, cx+70, cy+50)
    app.key('Escape')
    app.page.evaluate('''() => {
      window.tpxExportBlobTypes = [];
      const create = URL.createObjectURL.bind(URL);
      URL.createObjectURL = blob => {
        window.tpxExportBlobTypes.push(blob.type);
        return create(blob);
      };
    }''')
    formats = [('SVG', b'<svg'), ('EPS', b'%!PS'), ('PDF', b'%PDF'), ('.mp', b'beginfig')]
    for name, signature in formats:
        app.menu('File', 'Save as...')
        dialog = app.page.get_by_role('dialog')
        option = dialog.get_by_label('File type').locator('option').filter(has_text=name).first
        dialog.get_by_label('File type').select_option(option.get_attribute('value'))
        dialog.get_by_label('File name').fill('browser-export')
        with app.page.expect_download() as saved:
            dialog.get_by_role('button', name='OK', exact=True).click()
        download = saved.value
        path = app.output / download.suggested_filename
        download.save_as(path)
        if signature not in path.read_bytes()[:1000]:
            raise AssertionError(f'{name} export has the wrong content')
    if app.page.evaluate('window.tpxExportBlobTypes') != [
            'image/svg+xml', 'application/postscript', 'application/pdf', 'text/plain']:
        raise AssertionError('export download MIME type did not match its selected format')
    return 'SVG, EPS, PDF and MetaPost downloads contain the selected format'


def s_download_status_and_unload(app):
    status = app.page.locator('#document-status')
    if 'original file' not in status.inner_text().lower():
        raise AssertionError('the initial status does not explain browser persistence')

    def unload_is_blocked():
        return app.page.evaluate('''() => {
          const event = new Event('beforeunload', {cancelable: true});
          window.dispatchEvent(event);
          return event.defaultPrevented;
        }''')

    if unload_is_blocked():
        raise AssertionError('a clean document installed an unload warning')
    v, cx, cy = box(app)
    app.menu('Insert', 'Insert rectangle')
    app.drag(cx-60, cy-40, cx+60, cy+40)
    app.key('Escape')
    phrase = 'Browser Ω 🐈'
    app.menu('Insert', 'Insert text')
    app.click(cx+90, cy-30)
    editor = app.page.get_by_role('textbox', name='Text (canvas)', exact=True)
    editor.wait_for()
    editor.fill(phrase)
    app.page.get_by_role('button', name='Done', exact=True).click()
    app.page.wait_for_timeout(150)
    if editor.count():
        raise AssertionError('Done did not settle the inline text session')
    if app.page.get_by_role('dialog').count():
        raise AssertionError('creating or committing text opened a dialog')
    status.wait_for(state='visible')
    app.page.wait_for_function('document.querySelector("#document-status").dataset.state === "unsaved"')
    if not unload_is_blocked():
        raise AssertionError('real unsaved geometry did not guard beforeunload')

    app.page.evaluate('''() => {
      window.tpxDownloadBlobs = [];
      window.tpxRevokedBlobs = [];
      const create = URL.createObjectURL.bind(URL);
      const revoke = URL.revokeObjectURL.bind(URL);
      URL.createObjectURL = blob => {
        window.tpxDownloadBlobs.push(blob.type);
        return create(blob);
      };
      URL.revokeObjectURL = url => {
        window.tpxRevokedBlobs.push(url);
        return revoke(url);
      };
    }''')
    saved = app.save('browser-copy.tpx')
    if saved.name != 'browser-copy.tpx':
        raise AssertionError(f'download used an unexpected filename: {saved.name}')
    objects = drawing_xml(saved)
    if (len(objects) != 2 or objects[0].tag != 'rect' or
            not any(obj.tag == 'text' and
                    phrase in decode_tpx_text_once(obj.attrib.get('t', ''))
                    for obj in objects)):
        raise AssertionError('download did not retain the geometry and Unicode text')
    app.page.wait_for_function('document.querySelector("#document-status").dataset.state === "unconfirmed"')
    text = status.inner_text().lower()
    if 'started' not in text and 'requested' not in text:
        raise AssertionError(f'download status overstates or omits initiation: {text}')
    if 'original file' not in text or 'cannot confirm it was saved' not in text:
        raise AssertionError(f'download status does not distinguish the original file: {text}')
    if 'suppress' not in text:
        raise AssertionError('status does not explain that beforeunload is browser-controlled')
    if not unload_is_blocked():
        raise AssertionError('an unconfirmed browser download did not retain the unload guard')
    if app.page.evaluate('window.tpxDownloadBlobs') != ['application/xml']:
        raise AssertionError('TpX download used the wrong MIME type')
    app.page.wait_for_function('window.tpxRevokedBlobs.length === 1', timeout=4000)
    if app.page.locator('a[download][href^="blob:"]').count():
        raise AssertionError('temporary download anchor was left in the page')

    app.menu('File', 'New')
    if unload_is_blocked():
        raise AssertionError('starting a new document kept the prior download guard')
    app.menu('File', 'Open')
    dialog = app.page.get_by_role('dialog')
    dialog.get_by_label('Choose file').set_input_files(saved)
    dialog.get_by_role('button', name='OK', exact=True).click()
    reopened = drawing_xml(app.save('browser-reimported.tpx'))
    if ([obj.tag for obj in reopened] != [obj.tag for obj in objects] or
            [obj.attrib for obj in reopened] != [obj.attrib for obj in objects]):
        raise AssertionError('downloaded text and geometry changed after reimport')
    app.page.wait_for_function('window.tpxDownloadBlobs.length === 2')
    app.page.wait_for_function('window.tpxRevokedBlobs.length === 2', timeout=4000)

    downloads = []
    app.page.on('download', lambda item: downloads.append(item.suggested_filename))
    app.page.evaluate('''() => {
      window.tpxOriginalAnchorClick = HTMLAnchorElement.prototype.click;
      HTMLAnchorElement.prototype.click = () => {};
    }''')
    app.menu('Insert', 'Insert line')
    app.drag(cx-45, cy-35, cx+45, cy+35)
    app.key('Escape')
    app.menu('File', 'Save')
    app.page.wait_for_function('document.querySelector("#document-status").dataset.state === "unconfirmed"')
    if downloads:
        raise AssertionError('a browser-suppressed anchor click produced a download event')
    if not unload_is_blocked() or 'cannot confirm' not in status.inner_text().lower():
        raise AssertionError('a silently blocked request implied durable persistence')

    app.menu('Insert', 'Insert line')
    app.drag(cx-30, cy-25, cx+30, cy+25)
    app.key('Escape')
    app.page.evaluate('''() => {
      HTMLAnchorElement.prototype.click = () => { throw new Error('injected download failure'); };
    }''')
    app.menu('File', 'Save')
    app.page.wait_for_function('''() => {
      const status = document.querySelector('#document-status');
      return status.dataset.state === 'unsaved' && status.textContent.includes('could not start');
    }''')
    if app.page.get_by_role('dialog').count():
        raise AssertionError('a failed browser download opened a modal dialog')
    if not unload_is_blocked():
        raise AssertionError('failed download incorrectly marked the drawing clean')
    app.page.evaluate('''() => {
      HTMLAnchorElement.prototype.click = window.tpxOriginalAnchorClick;
    }''')
    with app.page.expect_download() as retried:
        app.menu('File', 'Save')
    retry_path = app.output / retried.value.suggested_filename
    retried.value.save_as(retry_path)
    if len(drawing_xml(retry_path)) != 4:
        raise AssertionError('retry after download failure lost text or geometry')
    if not unload_is_blocked():
        raise AssertionError('a successful retry cleared the unconfirmed-download guard')
    return ('dirty and unconfirmed-request unload guard; silent and thrown download '
            'failures stay honest; retry preserves text and geometry')


def s_bitmap_roundtrip(app):
    import struct
    import zlib

    def chunk(kind, data):
        return (struct.pack('>I', len(data)) + kind + data +
                struct.pack('>I', zlib.crc32(kind + data)))

    fixture = app.output / 'bitmap-fixture.png'
    fixture.write_bytes(b'\x89PNG\r\n\x1a\n' +
        chunk(b'IHDR', struct.pack('>IIBBBBB', 32, 32, 8, 2, 0, 0, 0)) +
        chunk(b'IDAT', zlib.compress((b'\x00' + b'\xff\x00\xff' * 32) * 32)) +
        chunk(b'IEND', b''))
    v, cx, cy = box(app)
    app.menu('Insert', 'Insert bitmap')
    dialog = app.page.get_by_role('dialog')
    dialog.get_by_label('Choose file', exact=True).set_input_files(fixture)
    before = app.frames()
    dialog.get_by_role('button', name='OK', exact=True).click()
    app.painted(before)  # Wait for Pascal to arm the bitmap tool after the file dialog.
    app.drag(cx-70, cy-50, cx+70, cy+50)
    app.key('Escape')
    related_downloads = []
    app.page.on('download', lambda download: related_downloads.append(download))
    saved = app.save('bitmap-roundtrip.tpx')
    app.page.wait_for_timeout(100)
    objects = drawing_xml(saved)
    link = objects[0].get('link', '') if len(objects) == 1 else ''
    if link != fixture.name:
        raise AssertionError('saved drawing did not use a portable relative bitmap link')
    if app.page.get_by_role('dialog').count():
        raise AssertionError('saving a bitmap displayed an unexpected dialog')
    asset_download = next((item for item in related_downloads
                           if item.suggested_filename == fixture.name), None)
    if asset_download is None:
        raise AssertionError('TpX download omitted its linked bitmap asset')
    downloaded_asset = app.output / asset_download.suggested_filename
    asset_download.save_as(downloaded_asset)
    if downloaded_asset.read_bytes() != fixture.read_bytes():
        raise AssertionError('linked bitmap download changed the asset bytes')

    app.menu('File', 'Save as...')
    dialog = app.page.get_by_role('dialog')
    option = dialog.get_by_label('File type').locator('option').filter(has_text='EPS').first
    dialog.get_by_label('File type').select_option(option.get_attribute('value'))
    dialog.get_by_label('File name').fill('bitmap-export.eps')
    with app.page.expect_download() as exported:
        dialog.get_by_role('button', name='OK', exact=True).click()
    eps = app.output / exported.value.suggested_filename
    exported.value.save_as(eps)
    if b'colorimage' not in eps.read_bytes():
        raise AssertionError('EPS export omitted the bitmap image data')

    def accept_unload_prompt(dialog):
        if dialog.type == 'beforeunload':
            dialog.accept()
        else:
            dialog.dismiss()
    app.page.on('dialog', accept_unload_prompt)
    app.page.reload()
    app.page.remove_listener('dialog', accept_unload_prompt)
    app.page.wait_for_selector('#status[data-state=ready]')
    app.menu('File', 'Open')
    dialog = app.page.get_by_role('dialog')
    dialog.get_by_label('Choose file', exact=True).set_input_files(saved)
    dialog.get_by_label('Related files (optional)', exact=True).set_input_files(fixture)
    dialog.get_by_role('button', name='OK', exact=True).click()
    app.page.wait_for_function('''() => {
        const c = document.querySelector('#lcl');
        const p = c.getContext('2d').getImageData(0, 0, c.width, c.height).data;
        let n = 0;
        for (let i = 0; i < p.length; i += 4)
            if (p[i] === 255 && p[i+1] === 0 && p[i+2] === 255) n++;
        return n > 1000;
    }''')
    return 'bitmap save has no converter dialog; EPS contains pixels; related PNG reopens'


def s_object_inspector(app):
    import re
    from playwright.sync_api import expect

    v, cx, cy = box(app)
    app.menu('Insert', 'Insert rectangle')
    app.drag(cx-150, cy-55, cx-35, cy+45)
    app.key('Escape')
    app.menu('Insert', 'Insert rectangle')
    app.drag(cx+35, cy-45, cx+145, cy+55)
    app.key('Escape')
    app.menu('Edit', 'Select all')
    app.menu('Edit', 'Object properties')
    if app.page.get_by_role('dialog').count():
        raise AssertionError('opening the inspector created a modal dialog')
    rounded = app.page.get_by_role('button').filter(has_text='Rounded corners')
    width = app.page.get_by_role('textbox', name='Width multiplier', exact=True)
    arrow_size = app.page.get_by_role(
        'textbox', name='Arrow size multiplier', exact=True)

    def open_combo_from_width(rows_away):
        bounds = width.bounding_box()
        size_bounds = arrow_size.bounding_box()
        if bounds is None or size_bounds is None:
            raise AssertionError('inspector anchor fields have no browser bounds')
        width_center = bounds['y'] + bounds['height'] / 2
        size_center = size_bounds['y'] + size_bounds['height'] / 2
        # The Line section's owner-drawn combos share rows with the two
        # accessible edit controls; interpolation keeps the hit test scaled.
        row_pitch = (size_center - width_center) / 3
        click_x = bounds['x'] + bounds['width'] - 16
        click_y = width_center + rows_away * row_pitch
        app.page.mouse.click(click_x, click_y)
        dialog = app.page.get_by_role('dialog', name='Choose item', exact=True)
        try:
            dialog.wait_for(state='visible', timeout=4000)
        except Exception as exc:
            raise AssertionError(
                'Choose item did not open for the inspector row click '
                f'at ({click_x:.1f}, {click_y:.1f}); field bounds={bounds}') from exc
        return dialog

    def select_combo_option(rows_away, option):
        dialog = open_combo_from_width(rows_away)
        dialog.locator('select').select_option(label=option)
        dialog.get_by_role('button', name='OK', exact=True).click()

    width.fill('2.5')
    width.press('Tab')
    if width.input_value() != '2.5' or not width.is_enabled():
        focus = app.page.evaluate('''() => document.activeElement?.tagName || 'none' ''')
        raise AssertionError(
            f'Tab lost the selected property or its committed value: '
            f'{width.input_value()!r}, focus={focus}')
    if app.page.get_by_role('dialog').count():
        raise AssertionError('routine inspector edits opened a modal dialog')
    width.fill('4.5')
    style_dialog = open_combo_from_width(-2)
    style_dialog.get_by_role('button', name='Cancel', exact=True).click()
    width.press('Escape')
    restored_width = width.input_value()
    if restored_width != '2.5':
        raise AssertionError(
            'opening a property dropdown committed the other field draft: '
            f'width is {restored_width!r}')
    select_combo_option(-1, 'red')

    width.fill('3.5')
    # Leaving the field for another canvas selection commits the pending edit.
    app.click(cx+210, cy+160)
    if app.page.get_by_role('dialog').count():
        raise AssertionError('committing the inspector edit opened a modal dialog')
    app.menu('Edit', 'Select all')
    app.menu('Edit', 'Object properties')
    if width.input_value() != '3.5':
        raise AssertionError('selection refresh lost the last committed width')

    def section_button(title):
        return app.page.get_by_role('button').filter(has_text=re.compile(
            rf'^(?:(?:▾|▸)\s*)?{re.escape(title)}$'))

    for title in ('Line', 'Fill and hatching', 'Text'):
        button = section_button(title)
        if button.count() != 1 or not button.is_visible():
            raise AssertionError(f'inspector section button is missing: {title}')
        button.click()
    if width.is_visible():
        raise AssertionError('collapsing Line left its width control visible')

    rounded = section_button('Rounded corners')
    if rounded.count() != 1 or not rounded.is_visible():
        raise AssertionError('Rounded corners section is not visible')
    rounded.click()

    def assert_editor_in_viewport(editor):
        bounds = editor.bounding_box()
        canvas = app.page.locator('#lcl').bounding_box()
        viewport = app.page.evaluate('({width: innerWidth, height: innerHeight})')
        if bounds is None or canvas is None or not (
                bounds['x'] >= canvas['x'] and bounds['y'] >= canvas['y'] and
                bounds['x'] + bounds['width'] <= canvas['x'] + canvas['width'] and
                bounds['y'] + bounds['height'] <= canvas['y'] + canvas['height'] and
                bounds['x'] >= 0 and bounds['y'] >= 0 and
                bounds['x'] + bounds['width'] <= viewport['width'] and
                bounds['y'] + bounds['height'] <= viewport['height']):
            raise AssertionError(
                f'inspector editor is outside the visible canvas: {bounds}')

    horizontal = app.page.get_by_role(
        'textbox', name='Horizontal radius (mm)', exact=True)
    assert_editor_in_viewport(horizontal)
    horizontal.fill('2.5')
    vertical = app.page.get_by_role('textbox', name='Vertical radius (mm)', exact=True)
    assert_editor_in_viewport(vertical)
    vertical.fill('3.5')
    vertical.press('Enter')
    if vertical.input_value() != '3.5':
        raise AssertionError(
            f'vertical radius did not remain after Enter: {vertical.input_value()!r}')
    vertical.press('Tab')
    expect(section_button('Bitmap')).to_be_focused()
    focus_after_tab = app.page.evaluate('''() => {
        const e = document.activeElement;
        return e ? {tag: e.tagName, text: (e.textContent || '').trim()} : null;
    }''')
    if (not vertical.is_enabled() or vertical.input_value() != '3.5' or
            focus_after_tab is None or focus_after_tab['tag'] != 'BUTTON' or
            'Bitmap' not in focus_after_tab['text']):
        raise AssertionError(
            'Tab from Vertical radius did not preserve selection and move to '
            f'the next Inspector control (Bitmap section): {focus_after_tab}')
    app.page.keyboard.type('9')
    if vertical.input_value() != '3.5':
        raise AssertionError(
            'typing after Tab changed the previously committed radius: '
            f'{vertical.input_value()!r}')
    app.page.keyboard.press('Shift+Tab')
    expect(vertical).to_be_focused()
    focus_after_shift_tab = app.page.evaluate('''() => {
        const e = document.activeElement;
        return e ? `${e.tagName}[${e.getAttribute('aria-label') || ''}]` : 'none';
    }''')
    if (focus_after_shift_tab != 'INPUT[Vertical radius (mm)]' or
            not vertical.is_enabled() or vertical.input_value() != '3.5'):
        raise AssertionError(
            'Shift+Tab did not return to the committed Inspector field: '
            f'{focus_after_shift_tab}')
    if app.capture_requested:
        app.shot('wide-open')
    if vertical.input_value() != '3.5':
        raise AssertionError(
            f'vertical radius was not committed before selection refresh: {vertical.input_value()!r}')
    before = app.viewport()
    inspector_menu_item = re.compile(r'^(?:✓ )?Object inspector$')
    app.menu('View', inspector_menu_item)
    hidden = app.viewport()
    if hidden['w'] <= before['w']:
        raise AssertionError('collapsing the inspector did not restore canvas width')
    if app.capture_requested:
        app.shot('wide-collapsed')
    app.menu('View', inspector_menu_item)
    if vertical.input_value() != '3.5':
        raise AssertionError('reopening the inspector lost the rounded-corner value')
    app.page.set_viewport_size({'width': 760, 'height': 900})
    app.page.wait_for_timeout(250)
    radius = app.page.get_by_role('textbox', name='Horizontal radius (mm)', exact=True)
    if not radius.is_visible():
        raise AssertionError('Rounded corners editor left the visible inspector on narrow resize')
    assert_editor_in_viewport(radius)
    radius.fill('-1')
    radius.press('Enter')
    # Validation messages are painted into the WASI canvas, not exposed as DOM
    # text; the native inspector scenario asserts the actual error label.
    if radius.input_value() != '-1':
        raise AssertionError('invalid inspector input was cleared instead of remaining editable')
    if app.capture_requested:
        app.shot('narrow-inline-error-advanced')
    radius.fill('2.5')
    radius.press('Enter')
    objects = drawing_xml(app.save('properties.tpx'))
    if (len(objects) != 2 or any(
            float(obj.get('lw', '0')) != 3.5 or
            float(obj.get('rx', '0')) != 2.5 or
            float(obj.get('ry', '0')) != 3.5 or
            obj.get('lc', '').lower() != 'red' for obj in objects)):
        raise AssertionError(
            'inspector edits did not survive save: '
            f'RY editor={vertical.input_value()!r}, {[o.attrib for o in objects]}')
    app.menu('Help', 'About')
    app.page.get_by_role('button', name='Acknowledgements').click()
    memo = app.page.locator('textarea:visible')
    if memo.count() != 1 or not memo.input_value():
        raise AssertionError('acknowledgements memo is empty or hidden')
    app.page.get_by_role('button', name='Acknowledgements').focus()
    app.key('Escape')
    app.page.wait_for_function('document.querySelector("#lcl").width > 500')
    return 'two-object property edits, inline validation, collapse and save work without a modal'


def s_clipboard_roundtrip(app):
    v, cx, cy = box(app)
    app.menu('Insert', 'Insert rectangle')
    app.drag(cx-90, cy-60, cx+60, cy+40)
    app.key('Escape')
    app.menu('Edit', 'Select all')
    app.menu('Edit', 'Copy')
    app.menu('Edit', 'Paste')
    pasted = drawing_xml(app.save('clipboard-paste.tpx'))
    if len(pasted) != 2:
        raise AssertionError(f'copy/paste produced {len(pasted)} objects instead of 2')
    app.menu('Edit', 'Select all')
    app.menu('Edit', 'Cut')
    if len(drawing_xml(app.save('clipboard-cut.tpx'))):
        raise AssertionError('cut left objects in the drawing')
    app.menu('Edit', 'Paste')
    restored = drawing_xml(app.save('clipboard-restored.tpx'))
    if len(restored) != 2:
        raise AssertionError('paste did not restore the cut objects')
    return 'copy, paste, cut and restore preserve drawing objects'


def s_copy_as_tikz(app):
    v, cx, cy = box(app)
    app.menu('Insert', 'Insert rectangle')
    app.drag(cx-90, cy-60, cx+60, cy+40)
    app.key('Escape')
    app.page.evaluate('''() => Object.defineProperty(navigator, 'clipboard', {
      configurable: true, value: {writeText: text => {
        window.tpxCopiedTikZ = text; return Promise.resolve();
      }}
    })''')
    app.menu('Edit', 'Copy as TikZ')
    app.page.wait_for_function('typeof window.tpxCopiedTikZ === "string"')
    copied = app.page.evaluate('window.tpxCopiedTikZ')
    if not copied.startswith('% Generated from the current TpX drawing'):
        raise AssertionError('clipboard API did not receive generated TikZ source')
    if r'\begin{tikzpicture}' not in copied or r'\end{tikzpicture}' not in copied:
        raise AssertionError('browser clipboard received an incomplete TikZ picture')
    if app.page.locator('#tpx-copy-fallback').count():
        raise AssertionError('successful clipboard write showed the fallback')

    app.page.evaluate('''() => Object.defineProperty(navigator, 'clipboard', {
      configurable: true, value: {writeText: () => Promise.reject(new Error('denied'))}
    })''')
    app.menu('Edit', 'Copy as TikZ')
    fallback = app.page.locator('#tpx-copy-fallback:not([hidden])')
    fallback.wait_for()
    source = fallback.locator('textarea')
    displayed = source.input_value()
    selection = source.evaluate('''element => ({active: document.activeElement === element,
      start: element.selectionStart, end: element.selectionEnd, length: element.value.length})''')
    if displayed != copied or selection != {
            'active': True, 'start': 0, 'end': len(displayed), 'length': len(displayed)}:
        raise AssertionError('rejected clipboard write did not expose selectable complete source')
    if app.page.get_by_role('dialog').count():
        raise AssertionError('clipboard rejection opened a modal dialog')
    return 'browser menu copy writes a complete TikZ picture; rejected writes show selectable source'


def s_nested_coordinates(app):
    v, cx, cy = box(app)
    app.menu('Insert', 'Insert line')
    app.drag(cx-70, cy-50, cx+70, cy+50)
    app.key('Escape')
    app.menu('Edit', 'Select all')
    app.menu('Edit', 'Object properties')
    app.page.get_by_role('button', name='Point coordinates…', exact=True).click()
    app.page.wait_for_function('document.querySelector("#lcl").width === 530')
    app.click(155, 55)  # First x-coordinate cell in the fixed-size Table LFM
    app.key('F2')
    editor = app.page.get_by_role('textbox').last
    editor.fill('125.5')
    app.key('Enter')
    app.page.get_by_role('button', name='OK', exact=True).click()
    app.page.wait_for_function('document.querySelector("#lcl").width > 600')
    objects = drawing_xml(app.save('coordinates.tpx'))
    if len(objects) != 1 or float(objects[0].get('x1', '0')) != 125.5:
        raise AssertionError('coordinate editor did not update the saved line')
    return 'nested coordinate editor commits a cell; both Pascal modal forms close'


def s_unsaved_confirmation(app):
    v, cx, cy = box(app)
    app.menu('Insert', 'Insert rectangle')
    app.drag(cx-50, cy-40, cx+50, cy+40)
    app.key('Escape')
    app.menu('File', 'New')
    dialog = app.page.get_by_role('dialog')
    dialog.get_by_role('button', name='Cancel', exact=True).click()
    if len(drawing_xml(app.save('cancel-preserved.tpx'))) != 1:
        raise AssertionError('Cancel discarded the drawing')
    app.menu('Insert', 'Insert line')
    app.drag(cx-50, cy-40, cx+50, cy+40)
    app.key('Escape')
    app.menu('File', 'New')
    app.page.get_by_role('dialog').get_by_role('button', name='No', exact=True).click()
    if len(drawing_xml(app.save('discarded.tpx'))) != 0:
        raise AssertionError('No did not start an empty drawing')
    return 'confirmation labels and Cancel/No results preserve or discard the drawing'


def s_tex_preview(app):
    import re
    import xml.etree.ElementTree as ET

    preview = re.compile(r'(?:✓ )?Live LaTeX Preview')
    formula = r'Energy $E=mc^2+\frac{1}{2}$'
    root = ET.Element('TpX', v='5', TeXFormat='none', PdfTeXFormat='none')
    ET.SubElement(root, 'text', x='20', y='30', h='7', t='Fallback', tex=formula)
    ET.SubElement(root, 'text', x='20', y='50', h='7', t='Ordinary text')

    def open_drawing(name):
        path = app.output / name
        ET.indent(root)
        path.write_text('\n'.join('%' + line for line in ET.tostring(root, encoding='unicode').splitlines()))
        app.menu('File', 'Open')
        dialog = app.page.get_by_role('dialog')
        dialog.get_by_label('Choose file').set_input_files(path)
        dialog.get_by_role('button', name='OK', exact=True).click()
        app.page.wait_for_timeout(250)

    if app.page.evaluate("performance.getEntriesByType('resource').some(e=>e.name.endsWith('/tex-svg.bundle.js'))"):
        raise AssertionError('MathJax loaded for a drawing without TeX')
    open_drawing('tex-preview-input.tpx')
    if app.page.evaluate("performance.getEntriesByType('resource').some(e=>e.name.endsWith('/tex-svg.bundle.js'))"):
        raise AssertionError('Imported TeX was previewed before document trust')
    snapshot_status = app.page.locator('#source-snapshot-status')
    snapshot_status.wait_for(state='visible')
    snapshot_message = snapshot_status.inner_text()
    if 'auto-refresh is unavailable' not in snapshot_message or 'File → Open again' not in snapshot_message:
        raise AssertionError('Browser source snapshot notice does not explain how to refresh it')
    app.menu('View', 'Trust TeX preview for this document')
    if not snapshot_status.is_visible():
        raise AssertionError('Approving TeX preview hid the browser source snapshot notice')
    app.page.wait_for_function("performance.getEntriesByType('resource').some(e=>e.name.endsWith('/tex-svg.bundle.js'))")
    v = app.viewport()
    area = [v['x'], v['y'], v['w'], v['h']]

    def capture(name):
        app.page.evaluate('''([name, r]) => {
          window.tpxTexTest ||= {};
          window.tpxTexTest[name] = document.querySelector('#lcl').getContext('2d')
            .getImageData(...r).data;
        }''', [name, area])

    def difference(name):
        return app.page.evaluate('''([name, r]) => {
          const a = window.tpxTexTest[name];
          const b = document.querySelector('#lcl').getContext('2d').getImageData(...r).data;
          let changed = 0;
          for (let i = 0; i < a.length; i += 4)
            if (Math.abs(a[i]-b[i]) > 8 || Math.abs(a[i+1]-b[i+1]) > 8 || Math.abs(a[i+2]-b[i+2]) > 8) changed++;
          return changed;
        }''', [name, area])

    app.menu('View', preview)
    park(app, v)
    capture('plain')
    app.menu('View', preview)
    app.page.wait_for_function('''r => {
      const a = window.tpxTexTest.plain;
      const b = document.querySelector('#lcl').getContext('2d').getImageData(...r).data;
      let changed = 0;
      for (let i = 0; i < a.length; i += 4)
        if (Math.abs(a[i]-b[i]) > 8 || Math.abs(a[i+1]-b[i+1]) > 8 || Math.abs(a[i+2]-b[i+2]) > 8)
          if (++changed > 80) return true;
      return false;
    }''', arg=area, polling=100, timeout=10000)
    park(app, v)
    changed = difference('plain')
    if changed < 80:
        raise AssertionError(f'TeX preview differs from fallback by only {changed} pixels')
    capture('rendered')
    app.menu('View', preview)
    app.menu('View', preview)
    park(app, v)
    if difference('rendered') > 20:
        raise AssertionError('cached TeX preview did not return after toggling')
    if app.idle_frames(1.0):
        raise AssertionError('TeX preview keeps repainting while idle')
    saved = drawing_xml(app.save('tex-preview-saved.tpx'))
    if saved[0].get('tex') != formula or saved[0].get('t') != 'Fallback':
        raise AssertionError('preview changed the saved text sources')
    root[0].set('tex', r'$\undefinedPreviewCommand$')
    open_drawing('tex-preview-invalid.tpx')
    app.menu('View', 'Trust TeX preview for this document')
    app.page.locator('#notice:not([hidden])').wait_for()
    park(app, v)
    capture('invalid')
    app.menu('View', preview)
    park(app, v)
    if difference('invalid') > 20:
        raise AssertionError('new invalid TeX object retained another object\'s cached preview')
    app.menu('File', 'New')
    app.page.wait_for_timeout(100)
    if snapshot_status.is_visible():
        raise AssertionError('Starting a new drawing kept the prior source snapshot notice')
    return f'lazy MathJax changed {changed} pixels; toggle/cache/save/fallback and idle verified'



def s_nonmodal_editing_workflow(app):
    """Keep canvas geometry and both text sources through an integrated edit."""
    import copy
    import math
    import re
    from playwright.sync_api import expect

    native_dialogs = []
    popups = []

    def dismiss_and_record(dialog):
        native_dialogs.append(dialog.type)
        dialog.dismiss()

    app.page.on('dialog', dismiss_and_record)
    app.page.on('popup', lambda page: popups.append(page.url))

    # End with the inspector visible: if the first toggle hid it, paper grows
    # and the second toggle restores it. This scenario checks coexistence.
    previous_width = app.viewport()['w']
    app.menu('View', 'Object inspector')
    if app.viewport()['w'] > previous_width:
        app.menu('View', 'Object inspector')
    # Recompute canvas coordinates after the side panel changes its layout.
    v, cx, cy = box(app)
    app.menu('Insert', 'Insert line')
    app.drag(cx - 130, cy - 70, cx + 90, cy - 30)
    app.key('Escape')
    app.menu('Insert', 'Insert rectangle')
    app.drag(cx + 25, cy + 15, cx + 125, cy + 85)
    app.key('Escape')

    # Change a real line through the same inspector used by the label edit.
    # These values also make the later undo/redo and reopen oracle non-default.
    app.click(cx - 20, cy - 50)
    app.menu('Edit', 'Object properties')
    line_width = app.page.get_by_role('textbox', name='Width multiplier', exact=True)
    line_width.fill('2.25')
    line_width.press('Enter')

    def open_painted_picker(anchor, lower_anchor, fraction, value):
        first = anchor.bounding_box()
        last = lower_anchor.bounding_box()
        if first is None or last is None:
            raise AssertionError(f'inspector controls for {value!r} are not rendered')
        first_center = first['y'] + first['height'] / 2
        last_center = last['y'] + last['height'] / 2
        y = first_center + (last_center - first_center) * fraction
        # WASI paints LCL combo boxes into the canvas; the host exposes the
        # opened list as a small accessible picker dialog.
        app.page.mouse.click(first['x'] + first['width'] - 8, y)
        picker = app.page.get_by_role('dialog', name='Choose item', exact=True)
        picker.wait_for()
        return picker

    def choose_painted_item(anchor, lower_anchor, fraction, value):
        picker = open_painted_picker(anchor, lower_anchor, fraction, value)
        picker.locator('select').select_option(label=value)
        picker.get_by_role('button', name='OK', exact=True).click()

    arrow_size = app.page.get_by_role('textbox', name='Arrow size multiplier', exact=True)
    # End arrow sits two rows after Width and one before Arrow size.
    choose_painted_item(line_width, arrow_size, 2 / 3, 'h40')

    app.menu('Insert', 'Insert text')
    if app.page.get_by_role('dialog').count():
        raise AssertionError('Insert text opened a dialog')
    app.click(cx - 80, cy - 40)
    text_editor = app.page.get_by_role('textbox', name='Text (canvas)', exact=True)
    expect(text_editor).to_be_focused(timeout=3000)
    app.type('Fallback λ')
    if text_editor.input_value() != 'Fallback λ':
        raise AssertionError('canvas keyboard input did not reach the new label')
    app.page.get_by_role('button', name='Done', exact=True).click()

    # Select the newly committed label through the canvas before opening its
    # properties, so the text-specific inspector fields are exercised.
    app.click(cx - 80, cy - 40)
    app.menu('Edit', 'Object properties')
    if app.page.get_by_role('dialog').count():
        raise AssertionError('Object properties opened a dialog')
    tex = app.page.get_by_role('textbox', name='TeX source', exact=True)
    height = app.page.get_by_role('textbox', name='Text height (mm)', exact=True)
    font_family = app.page.get_by_role('textbox', name='Font family', exact=True)
    if tex.input_value() != '':
        raise AssertionError(f'new label inspector started with unexpected TeX: {tex.input_value()}')
    if not tex.is_enabled() or not height.is_enabled():
        raise AssertionError('text properties were unavailable after selecting the new label')
    tex.fill(r'$\beta_{i}$')
    tex.press('Control+Enter')
    if tex.input_value() != r'$\beta_{i}$' or not height.is_enabled():
        raise AssertionError('committing TeX changed the selected label or its Inspector fields')
    font_family.fill('Arial')
    font_family.press('Enter')
    if font_family.input_value() != 'Arial':
        raise AssertionError('committing the font family changed its inspector value')
    rotation = app.page.get_by_role('textbox', name='Text angle (degrees)', exact=True)
    # Keep the canvas viewport fixed. The Inspector owns its own scrollbox, so
    # collapse the unrelated sections before using the painted alignment combo.
    app.page.get_by_role('button', name='▾ Line', exact=True).click()
    app.page.get_by_role('button', name='▾ Fill and hatching', exact=True).click()
    if not height.is_enabled():
        raise AssertionError('collapsing unrelated Inspector sections changed the selected label')
    height.fill('2.75')
    if height.input_value() != '2.75' or not tex.is_enabled():
        raise AssertionError('starting the height edit changed the selected label or text')
    height.press('Tab')
    expect(font_family).to_be_focused(timeout=3000)
    if not height.is_enabled() or height.input_value() != '2.75':
        raise AssertionError('Tab did not commit height and focus the next Inspector field')
    app.page.keyboard.press('Shift+Tab')
    expect(height).to_be_focused(timeout=3000)
    if height.input_value() != '2.75':
        raise AssertionError('Shift+Tab did not return to the committed height field')

    # Continue through the painted style and alignment controls using the
    # actual keyboard route. The chooser's checked option independently
    # confirms each owner-drawn control received ArrowDown.
    height.press('Tab')
    expect(font_family).to_be_focused(timeout=3000)
    font_family.press('Tab')
    app.page.keyboard.press('ArrowDown')
    style_picker = open_painted_picker(font_family, rotation, 1 / 3,
                                       'Font style')
    style_selection = style_picker.locator('select').locator(
        'option:checked').inner_text()
    if style_selection != 'Bold':
        style_picker.get_by_role('button', name='Cancel', exact=True).click()
        raise AssertionError(
            f'Tab/ArrowDown left Font style at {style_selection!r}')
    style_picker.get_by_role('button', name='OK', exact=True).click()

    app.page.keyboard.press('Tab')
    app.page.keyboard.press('ArrowDown')
    alignment_picker = open_painted_picker(height, rotation, 3 / 4,
                                           'Horizontal alignment')
    alignment_selection = alignment_picker.locator('select').locator(
        'option:checked').inner_text()
    if alignment_selection != 'Center':
        alignment_picker.get_by_role('button', name='Cancel', exact=True).click()
        raise AssertionError(
            f'Tab/ArrowDown left Horizontal alignment at {alignment_selection!r}, expected Center')
    alignment_picker.get_by_role('button', name='OK', exact=True).click()

    app.page.keyboard.press('Tab')
    expect(rotation).to_be_focused(timeout=3000)
    rotation_value = rotation.input_value()
    rotation.press('Shift+Tab')
    app.page.keyboard.press('ArrowDown')
    alignment_picker = open_painted_picker(height, rotation, 3 / 4,
                                           'Horizontal alignment')
    alignment_selection = alignment_picker.locator('select').locator(
        'option:checked').inner_text()
    if alignment_selection != 'Right' or rotation.input_value() != rotation_value:
        alignment_picker.get_by_role('button', name='Cancel', exact=True).click()
        raise AssertionError(
            'Shift+Tab did not return to alignment without changing Text angle')
    alignment_picker.locator('select').select_option(label='Center')
    alignment_picker.get_by_role('button', name='OK', exact=True).click()
    app.page.keyboard.press('Tab')
    expect(rotation).to_be_focused(timeout=3000)
    if app.capture_requested:
        app.shot('inspector-text-angle-focused')
    rotation.press('Tab')
    stars_section = app.page.get_by_role('button').filter(has_text=re.compile(
        r'^(?:(?:▾|▸)\s*)?Stars and symbols$'))
    expect(stars_section).to_be_focused(timeout=3000)
    active_field = app.page.evaluate(
        '() => document.activeElement?.getAttribute("aria-label") || ""')
    if active_field in {
            'Text', 'TeX source', 'Text height (mm)', 'Font family',
            'Font style', 'Horizontal alignment', 'Text angle (degrees)'}:
        raise AssertionError('Tab from the final Inspector field wrapped inside the Inspector')
    app.page.keyboard.press('Shift+Tab')
    expect(rotation).to_be_focused(timeout=3000)
    if rotation.input_value() != rotation_value:
        raise AssertionError('Shift+Tab changed Text angle while returning from Stars and symbols')

    # Finish the numeric editor explicitly before opening the canvas editor.
    # This keeps the cross-view check independent of blur-commit behavior.
    px, py = app.at(cx - 80, cy - 40)
    app.page.mouse.dblclick(px, py, delay=80)
    handoff_editor = app.page.get_by_role('textbox', name='TeX source (canvas)', exact=True)
    handoff_editor.wait_for()
    if (handoff_editor.input_value() != r'$\beta_{i}$' or
            height.input_value() != '2.75'):
        raise AssertionError(
            'moving from inspector to canvas changed committed values: '
            f'TeX={handoff_editor.input_value()!r}, height={height.input_value()!r}')
    app.page.get_by_role('button', name='Done', exact=True).click()
    app.page.wait_for_timeout(150)
    if handoff_editor.count():
        raise AssertionError('the inspector-to-canvas handoff left a stale editor visible')

    def no_edit_dialogs():
        if native_dialogs:
            raise AssertionError(f'ordinary editing opened browser dialogs: {native_dialogs}')
        if popups:
            raise AssertionError(f'ordinary editing opened new pages: {popups}')
        if app.page.get_by_role('dialog').count():
            raise AssertionError('ordinary editing left a modal dialog open')

    no_edit_dialogs()
    baseline_path = app.save('nonmodal-initial.tpx')
    baseline = drawing_xml(baseline_path)
    if [obj.tag for obj in baseline] != ['line', 'rect', 'text']:
        raise AssertionError(f'workflow saved the wrong scene: {[obj.tag for obj in baseline]}')
    baseline_attrs = [copy.deepcopy(obj.attrib) for obj in baseline]
    if (baseline[0].get('x1') == baseline[0].get('x2') and
            baseline[0].get('y1') == baseline[0].get('y2')):
        raise AssertionError(f'line tool saved a zero-length line: {baseline[0].attrib}')
    if (float(baseline[1].get('w', '0')) <= 0 or
            float(baseline[1].get('h', '0')) <= 0):
        raise AssertionError(f'rectangle tool saved invalid geometry: {baseline[1].attrib}')
    label = baseline[2]
    font = label.find('font')
    if (font is None or font.get('face') != 'Arial' or font.get('bf') != '1'):
        raise AssertionError(f'keyboard Font-style selection was not saved as bold: {label.attrib}')
    baseline_font = copy.deepcopy(font.attrib)
    if (decode_tpx_text_once(label.get('t', '')) != 'Fallback λ' or
            label.get('tex') != r'$\beta_{i}$' or
            float(label.get('h', '0')) != 2.75):
        raise AssertionError(f'inspector values were not saved independently: {label.attrib}')
    if (label.get('halign', 'l') != 'c' or
            any(not math.isfinite(float(label.get(field, 'nan')))
                for field in ('x', 'y'))):
        raise AssertionError(f'canvas label lost centered alignment or finite anchor: {label.attrib}')
    if float(baseline[0].get('lw', '0')) != 2.25 or baseline[0].get('arr2') != 'h40':
        raise AssertionError(f'inspector stroke or arrow values were not saved: {baseline[0].attrib}')

    def snapshot(name):
        objects = drawing_xml(app.save(name))
        attrs = [obj.attrib for obj in objects]
        if [obj.tag for obj in objects] != ['line', 'rect', 'text']:
            raise AssertionError(f'{name} changed the object count or types')
        if attrs[0] != baseline_attrs[0] or attrs[1] != baseline_attrs[1]:
            raise AssertionError(f'{name} changed line or rectangle geometry/style')
        saved_font = objects[2].find('font')
        if saved_font is None or saved_font.attrib != baseline_font:
            raise AssertionError(f'{name} changed the committed text font: {saved_font}')
        return attrs[2]

    # An over-limit live draft must keep its buffer, caret and editor when an
    # Inspector handoff is refused; Cancel then restores the committed source.
    px, py = app.at(cx - 80, cy - 40)
    app.page.mouse.dblclick(px, py, delay=80)
    blocked_editor = app.page.get_by_role(
        'textbox', name='TeX source (canvas)', exact=True)
    blocked_editor.wait_for()
    oversized = 'x' * 65537
    blocked_editor.fill(oversized)
    overflow_selection = blocked_editor.evaluate(
        '(e) => [e.selectionStart, e.selectionEnd]')
    height.click()
    expect(blocked_editor).to_be_focused(timeout=3000)
    if (blocked_editor.input_value() != oversized or
            blocked_editor.evaluate('(e) => [e.selectionStart, e.selectionEnd]') !=
            overflow_selection or height.input_value() != '2.75'):
        raise AssertionError(
            'refused Inspector handoff changed buffer/caret or committed height: '
            f'buffer length={len(blocked_editor.input_value())}, '
            f'selection={blocked_editor.evaluate("(e) => [e.selectionStart, e.selectionEnd]")}, '
            f'height={height.input_value()!r}')
    app.page.get_by_role('button', name='Cancel', exact=True).click()
    blocked_editor.wait_for(state='hidden')
    if tex.input_value() != r'$\beta_{i}$' or height.input_value() != '2.75':
        raise AssertionError(
            'canceling the refused draft changed its committed source or height: '
            f'TeX={tex.input_value()!r}, height={height.input_value()!r}')
    if snapshot('nonmodal-overflow-cancelled.tpx') != baseline_attrs[2]:
        raise AssertionError('canceling the refused raw draft changed the saved scene')

    v = app.viewport()
    px, py = app.at(cx - 80, cy - 40)
    zoom_region = region(cx - 160, cy - 110, 320, 220)
    before_zoom = app.pixels(zoom_region)

    def zoom_at_label(wheel_steps):
        app.page.keyboard.down('Control')
        try:
            delta = 120 if wheel_steps > 0 else -120
            for _ in range(abs(wheel_steps)):
                app.wheel(cx - 80, cy - 40, delta)
        finally:
            app.page.keyboard.up('Control')
        app.page.wait_for_timeout(120)

    # WASI normalizes every browser wheel event to one LCL tick; each Ctrl
    # tick scales by 2**(1/8), independent of the browser delta magnitude.
    # Five ticks reach about 154%; three more reach 200%; eight back restore 100%.
    zoom_at_label(5)
    at_150 = app.pixels(zoom_region)
    if diff(before_zoom, at_150) < 40:
        raise AssertionError('about 150% canvas zoom did not change the edited scene')
    if snapshot('nonmodal-zoom-150.tpx') != baseline_attrs[2]:
        raise AssertionError('150% canvas zoom changed the label or its world anchor')
    zoom_at_label(3)
    at_200 = app.pixels(zoom_region)
    if diff(at_150, at_200) < 40:
        raise AssertionError('200% canvas zoom did not change the edited scene again')
    if snapshot('nonmodal-zoom-200.tpx') != baseline_attrs[2]:
        raise AssertionError('200% canvas zoom changed the label or its world anchor')
    zoom_at_label(-8)
    restored_zoom = app.pixels(zoom_region)
    if diff(before_zoom, restored_zoom) > 1000:
        raise AssertionError('returning to 100% did not restore the starting view')
    if snapshot('nonmodal-zoom-100.tpx') != baseline_attrs[2]:
        raise AssertionError('returning to 100% changed the label or its world anchor')

    # The cursor anchor remains usable after zoom changes.
    px, py = app.at(cx - 80, cy - 40)
    app.page.mouse.dblclick(px, py, delay=80)
    editor = app.page.get_by_role('textbox', name='TeX source (canvas)', exact=True)
    editor.wait_for()
    if not editor.is_visible() or not tex.is_visible():
        raise AssertionError('the inline editor and inspector did not remain visible together')
    editor.fill(r'$\gamma_{j}$')
    app.menu('View', 'Object inspector')
    app.page.wait_for_timeout(200)
    if not editor.is_visible() or editor.input_value() != r'$\gamma_{j}$':
        raise AssertionError('collapsing the inspector lost or hid the active text buffer')
    app.menu('View', 'Object inspector')
    app.page.wait_for_timeout(200)
    if (not editor.is_visible() or not tex.is_visible() or
            editor.input_value() != r'$\gamma_{j}$'):
        raise AssertionError('reopening the inspector lost or hid the active text buffer')
    browser_size = app.page.viewport_size
    app.page.set_viewport_size({'width': 900, 'height': 720})
    app.page.wait_for_timeout(200)
    editor_box = editor.bounding_box()
    if (not editor.is_visible() or editor.input_value() != r'$\gamma_{j}$' or
            editor_box is None or editor_box['x'] < 0 or editor_box['y'] < 0 or
            editor_box['x'] + editor_box['width'] > 900 or
            editor_box['y'] + editor_box['height'] > 720):
        raise AssertionError('resizing the workspace hid or moved the active text buffer off-screen')
    app.page.set_viewport_size(browser_size)
    app.page.wait_for_timeout(200)
    v = app.viewport()
    if not editor.is_visible() or editor.input_value() != r'$\gamma_{j}$':
        raise AssertionError('restoring the workspace size lost the active text buffer')
    # A deliberate click in another Inspector field settles the valid canvas
    # draft first, then gives keyboard focus to the new field before its first
    # character is typed. This catches a commit callback that refocuses canvas.
    height.click()
    if not height.evaluate('(e) => document.activeElement === e'):
        raise AssertionError('Inspector field did not receive focus after inline settlement')
    height.press('Control+A')
    height.type('3.125')
    if height.input_value() != '3.125':
        raise AssertionError(
            f'keyboard input after inline settlement was lost or changed: {height.input_value()!r}')
    height.press('Enter')
    app.page.wait_for_timeout(150)
    if editor.count() or height.input_value() != '3.125':
        raise AssertionError(
            'switching to the Inspector failed to settle inline text and commit '
            f'height: inline editors={editor.count()}, height={height.input_value()!r}')
    no_edit_dialogs()
    edited = snapshot('nonmodal-cross-view-edited.tpx')
    expected_edited = copy.deepcopy(baseline_attrs[2])
    expected_edited['tex'] = r'$\gamma_{j}$'
    expected_edited['h'] = '3.125'
    if edited != expected_edited:
        raise AssertionError(f'cross-view edits changed source, anchor, style or unrelated fields: {edited}')

    focus_document(app, v)
    app.key('Control+z')
    undone_height = snapshot('nonmodal-height-undone.tpx')
    expected_gamma = copy.deepcopy(baseline_attrs[2])
    expected_gamma['tex'] = r'$\gamma_{j}$'
    if undone_height != expected_gamma:
        raise AssertionError(f'height undo crossed the inline text edit: {undone_height}')
    focus_document(app, v)
    app.key('Control+Shift+z')
    redone_height = snapshot('nonmodal-height-redone.tpx')
    if redone_height != expected_edited:
        raise AssertionError(f'height redo did not restore its own committed edit: {redone_height}')
    focus_document(app, v)
    app.key('Control+z')
    undone_height_again = snapshot('nonmodal-height-undone-again.tpx')
    if undone_height_again != expected_gamma:
        raise AssertionError(f'second height undo crossed the inline text edit: {undone_height_again}')
    focus_document(app, v)
    app.key('Control+z')
    undone_text = snapshot('nonmodal-text-undone.tpx')
    if undone_text != baseline_attrs[2]:
        raise AssertionError(f'text undo did not restore the complete prior object: {undone_text}')
    focus_document(app, v)
    app.key('Control+Shift+z')
    redone_text = snapshot('nonmodal-text-redone.tpx')
    if redone_text != expected_gamma:
        raise AssertionError(f'text redo changed the inspector edit or other fields: {redone_text}')
    focus_document(app, v)
    app.key('Control+Shift+z')
    redone_height = snapshot('nonmodal-height-redone-again.tpx')
    if redone_height != expected_edited:
        raise AssertionError(f'height redo changed the inline text edit: {redone_height}')
    px, py = app.at(cx - 80, cy - 40)
    app.page.mouse.dblclick(px, py, delay=80)
    save_editor = app.page.get_by_role('textbox', name='TeX source (canvas)', exact=True)
    save_editor.wait_for()
    save_editor.fill(r'$\delta_{k}$')
    with app.page.expect_download(timeout=5000) as saved_download:
        app.menu('File', 'Download a copy')
    final_path = app.output / 'nonmodal-active-draft-save.tpx'
    saved_download.value.save_as(final_path)
    if save_editor.count():
        raise AssertionError('File → Save did not settle and close the valid inline draft')
    no_edit_dialogs()
    final = drawing_xml(final_path)
    expected_saved = copy.deepcopy(expected_edited)
    expected_saved['tex'] = r'$\delta_{k}$'
    final_font = final[2].find('font')
    if ([obj.tag for obj in final] != ['line', 'rect', 'text'] or
            [obj.attrib for obj in final[:2]] != baseline_attrs[:2] or
            final[2].attrib != expected_saved or
            final_font is None or final_font.attrib != baseline_font or
            decode_tpx_text_once(final[2].get('t', '')) != 'Fallback λ'):
        raise AssertionError(f'File → Save lost the live draft or changed unrelated scene data: {[obj.attrib for obj in final]}')

    app.menu('File', 'New')
    app.menu('File', 'Open')
    dialog = app.page.get_by_role('dialog')
    dialog.get_by_label('Choose file').set_input_files(final_path)
    dialog.get_by_role('button', name='OK', exact=True).click()
    app.page.wait_for_timeout(250)
    reopened = drawing_xml(app.save('nonmodal-reopened.tpx'))
    reopened_font = reopened[2].find('font')
    if ([obj.attrib for obj in reopened] != [obj.attrib for obj in final] or
            reopened_font is None or reopened_font.attrib != baseline_font or
            decode_tpx_text_once(reopened[2].get('t', '')) != 'Fallback λ'):
        raise AssertionError('save/reopen changed the accepted scene or either text source')
    no_edit_dialogs()
    return 'canvas label, cross-view TeX/height edits, undo/redo, active-draft Save and reopen preserve sources, anchor and unrelated geometry'

SCENARIOS = [
    ('default-view', s_default_view),
    ('viewport-fit', s_viewport_fit),
    ('resize-follows', s_resize_follows),
    ('draw-drag', s_draw_drag),
    ('draw-click', s_draw_click),
    ('path-completion', s_path_completion),
    ('draw-cancel', s_draw_cancel),
    ('crosshair', s_crosshair),
    ('wheel-zoom', s_wheel_zoom),
    ('keyboard-shortcut', s_keyboard_shortcut),
    ('text-input', s_text_input),
    ('nonmodal-editing-workflow', s_nonmodal_editing_workflow),
    ('idle-stops-repainting', s_idle_stops_repainting),
    ('title-tracks-document', s_title_tracks_document),
    ('edit-move-delete', s_edit_move_delete),
    ('area-select', s_area_select),
    ('open-roundtrip', s_open_roundtrip),
    ('open-examples', s_open_examples),
    ('clipboard-roundtrip', s_clipboard_roundtrip),
    ('copy-as-tikz', s_copy_as_tikz),
    ('export-formats', s_export_formats),
    ('download-status-unload', s_download_status_and_unload),
    ('bitmap-roundtrip', s_bitmap_roundtrip),
    ('object-inspector', s_object_inspector),
    ('nested-coordinates', s_nested_coordinates),
    ('unsaved-confirmation', s_unsaved_confirmation),
    ('tex-preview', s_tex_preview),
]

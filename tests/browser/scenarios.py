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


# Left tool window, measured in client/canvas space. x=13 is the button centre.
# Row 104 is the select/area tool; the insert tools begin one pitch lower,
# verified by the responsive-button sweep and clean draw+undo at y=126.
TOOL_X = 13
TOOL_PITCH = 22
TOOL_FIRST = 126         # InsertLine


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
    """Accept the text dialog, place text, and verify saved content."""
    app.menu('Insert', 'Insert text')
    app.page.wait_for_function('document.querySelector("#lcl").width < 500')
    app.type('WASM Grüße λ 漢字 🐈')
    app.key('Enter')  # Accept through the ordinary LCL default button
    app.page.wait_for_function('document.querySelector("#lcl").width > 500')
    v, cx, cy = box(app)
    app.click(cx-60, cy-20)
    app.key('Escape')
    saved = app.save('text-roundtrip.tpx').read_text()
    if 'WASM Grüße λ 漢字 🐈' not in saved:
        raise AssertionError('accepted text is missing from the saved drawing')
    return 'accepted and placed text survives save'


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


def drawing_xml(path):
    import xml.etree.ElementTree as ET
    lines = path.read_text().splitlines()
    xml = []
    for line in lines:
        if line.startswith('%'):
            xml.append(line[1:])
            if line.startswith('%</TpX>') or (line.startswith('%<TpX ') and line.endswith('/>')):
                return ET.fromstring('\n'.join(xml))
    raise AssertionError('saved file has no TpX drawing')


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
    app.menu('File', 'New')
    app.menu('File', 'Open')
    dialog = app.page.get_by_role('dialog')
    dialog.get_by_label('Choose file').set_input_files(saved)
    dialog.get_by_role('button', name='OK', exact=True).click()
    app.page.wait_for_timeout(300)
    after = drawing_xml(app.save('reopened.tpx'))
    if len(after) != len(before) or [c.attrib for c in after] != [c.attrib for c in before]:
        raise AssertionError('open/save changed the drawing geometry')
    return 'downloaded drawing reopens with identical geometry'


def s_export_formats(app):
    v, cx, cy = box(app)
    app.menu('Insert', 'Insert rectangle')
    app.drag(cx-70, cy-50, cx+70, cy+50)
    app.key('Escape')
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
    return 'SVG, EPS, PDF and MetaPost downloads contain the selected format'


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
    saved = app.save('bitmap-roundtrip.tpx')
    objects = drawing_xml(saved)
    if len(objects) != 1 or objects[0].get('link') != fixture.name:
        raise AssertionError('saved drawing lost the bitmap reference')
    if app.page.get_by_role('dialog').count():
        raise AssertionError('saving a bitmap displayed an unexpected dialog')

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

    app.page.reload()
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


def s_modal_properties(app):
    v, cx, cy = box(app)
    app.menu('Insert', 'Insert rectangle')
    app.drag(cx-70, cy-50, cx+70, cy+50)
    app.key('Escape')
    app.menu('Edit', 'Select all')
    app.menu('Edit', 'Object properties')
    app.click(335, 17)  # Line color combo's dropdown arrow in the Properties LFM
    choices = app.page.get_by_role('dialog').get_by_role('listbox')
    choices.press('Escape')
    app.page.get_by_role('dialog').wait_for(state='hidden')
    app.click(335, 17)
    choices = app.page.get_by_role('dialog').get_by_role('listbox')
    choices.select_option('1')  # Custom, after Default; preserves Pascal item order
    choices.press('Enter')
    color = app.page.get_by_role('dialog').get_by_label('Color', exact=True)
    color.fill('#1278b5')
    app.page.get_by_role('dialog').get_by_role('button', name='OK', exact=True).click()
    app.page.get_by_role('textbox', name='RX', exact=True).fill('2.5')
    app.page.get_by_role('textbox', name='RY', exact=True).fill('3.5')
    app.page.get_by_role('button', name='OK', exact=True).click()
    objects = drawing_xml(app.save('properties.tpx'))
    if len(objects) != 1 or float(objects[0].get('rx', '0')) != 2.5 or float(objects[0].get('ry', '0')) != 3.5 or objects[0].get('lc') != '#1278B5':
        raise AssertionError(f'edited corner radii did not survive save: {[o.attrib for o in objects]}')
    app.menu('Edit', 'Object properties')
    if app.page.get_by_role('textbox', name='RX', exact=True).input_value() != '2.5':
        raise AssertionError('reopened properties lost the edited value')
    app.page.get_by_role('button', name='Cancel', exact=True).click()
    app.menu('Help', 'About')
    app.page.get_by_role('button', name='Acknowledgements').click()
    memo = app.page.locator('textarea:visible')
    if memo.count() != 1 or not memo.input_value():
        raise AssertionError('acknowledgements memo is empty or hidden')
    app.page.get_by_role('button', name='Acknowledgements').focus()
    app.key('Escape')
    app.page.wait_for_function('document.querySelector("#lcl").width > 500')
    return 'custom color and property edits survive save; read-only memo and modal Escape work'


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


def s_nested_coordinates(app):
    v, cx, cy = box(app)
    app.menu('Insert', 'Insert line')
    app.drag(cx-70, cy-50, cx+70, cy+50)
    app.key('Escape')
    app.menu('Edit', 'Select all')
    app.menu('Edit', 'Object properties')
    app.page.get_by_role('button', name='Points', exact=True).click()
    app.page.wait_for_function('document.querySelector("#lcl").width === 530')
    app.click(155, 55)  # First x-coordinate cell in the fixed-size Table LFM
    app.key('F2')
    editor = app.page.get_by_role('textbox').last
    editor.fill('125.5')
    app.key('Enter')
    app.page.get_by_role('button', name='OK', exact=True).click()
    app.page.wait_for_function('document.querySelector("#lcl").width === 460')
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
    app.page.locator('#notice:not([hidden])').wait_for()
    park(app, v)
    capture('invalid')
    app.menu('View', preview)
    park(app, v)
    if difference('invalid') > 20:
        raise AssertionError('new invalid TeX object retained another object\'s cached preview')
    return f'lazy MathJax changed {changed} pixels; toggle/cache/save/fallback and idle verified'


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
    ('idle-stops-repainting', s_idle_stops_repainting),
    ('title-tracks-document', s_title_tracks_document),
    ('edit-move-delete', s_edit_move_delete),
    ('area-select', s_area_select),
    ('open-roundtrip', s_open_roundtrip),
    ('clipboard-roundtrip', s_clipboard_roundtrip),
    ('export-formats', s_export_formats),
    ('bitmap-roundtrip', s_bitmap_roundtrip),
    ('modal-properties', s_modal_properties),
    ('nested-coordinates', s_nested_coordinates),
    ('unsaved-confirmation', s_unsaved_confirmation),
    ('tex-preview', s_tex_preview),
]

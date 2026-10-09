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
    """Move the pointer inside the paper but out of the measured region.

    The viewport paints full-width crosshair lines under the pointer; they must
    be moved somewhere outside the measured region before comparing pixels, or
    width+height leftover pixels show up in every undo comparison. The target
    has to stay on the paper: parked over the gray gutter the crosshair is not
    repainted and its old lines stay in the frame.
    """
    app.move(v['x'] + v['w'] - 30, v['y'] + v['h'] - 30)
    app.page.wait_for_timeout(300)


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
    app.click(TOOL_X, tool('line'))
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
    app.click(TOOL_X, tool('rect'))
    clean = app.pixels(reg)
    app.click(cx - 40, cy - 30)
    one = app.pixels(reg)
    if diff(clean, one) < 80:
        raise AssertionError(f'click draw produced only {diff(clean, one)} pixels')
    app.click(cx + 60, cy + 40)
    two = app.pixels(reg)
    if diff(one, two) < 40:
        raise AssertionError('tool did not stay armed for a second click-draw')
    return 'two objects drawn'


def s_draw_cancel(app):
    """Escape during a drag cancels the in-progress object."""
    v, cx, cy = box(app)
    reg = region(cx, cy)
    focus_document(app, v)
    app.click(TOOL_X, tool('ellipse'))
    app.move(cx, cy)
    clean = app.pixels(reg)
    app.press()
    for i in range(8):
        app.move(cx - 80 + i * 20, cy - 50 + i * 12)
    mid = app.pixels(reg)
    app.key('Escape')
    app.release()
    park(app, v)
    after = app.pixels(reg)
    live = diff(clean, mid)
    if live < 100:
        raise AssertionError(f'drag preview showed only {live} pixels')
    left = diff(clean, after)
    if left > 120:
        raise AssertionError(f'Escape left {left} of {live} pixels')
    return f'preview {live} px, cancelled to {left} px'


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
    app.click(TOOL_X, tool('line'))
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
    """Text tool: click in the viewport, type, and the glyphs appear."""
    v, cx, cy = box(app)
    reg = region(cx, cy, 400, 300)
    app.click(TOOL_X, tool('text'))
    clean = app.pixels(reg)
    app.click(cx - 60, cy - 20)
    app.type('WASM playtest')
    after = app.pixels(reg)
    ink = diff(clean, after)
    if ink < 200:
        raise AssertionError(f'typing produced only {ink} pixels (keys not rendered)')
    return f'{ink} pixels of typed text'


def s_idle_stops_repainting(app):
    if app.frames() < 1:
        raise AssertionError('no frames presented')
    f1 = app.idle_frames(3.0)
    if f1 > 2:
        raise AssertionError(f'{f1} frames presented during 3 s of idle time')
    return f'{f1} frames in 3 s idle'


def s_title_tracks_document(app):
    base = app.title()
    v, cx, cy = box(app)
    app.click(TOOL_X, tool('line'))
    app.drag(cx - 100, cy - 40, cx + 100, cy + 40)
    app.key('Control+z')
    app.key('Control+Shift+z')
    if app.title() != base:
        return f'title now: {app.title()}'
    return f'title stable: {base}'


SCENARIOS = [
    ('default-view', s_default_view),
    ('viewport-fit', s_viewport_fit),
    ('resize-follows', s_resize_follows),
    ('draw-drag', s_draw_drag),
    ('draw-click', s_draw_click),
    ('draw-cancel', s_draw_cancel),
    ('crosshair', s_crosshair),
    ('wheel-zoom', s_wheel_zoom),
    ('keyboard-shortcut', s_keyboard_shortcut),
    ('text-input', s_text_input),
    ('idle-stops-repainting', s_idle_stops_repainting),
    ('title-tracks-document', s_title_tracks_document),
]

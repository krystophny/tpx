#!/usr/bin/env python3
"""Behavioral scenarios for the browser playtest.

Each scenario performs real actions and asserts on the rendered result.
Failures are expected to become the defect catalog; they are reported with the
screenshot and the observed numbers.
"""

CANVAS = (0, 0)  # resolved at run time to the canvas client box


def _center(app):
    b = app.box()
    return int(b['nw'] * 0.45), int(b['nh'] * 0.55)


def _region(app, pad=140):
    """A patch in the middle of the drawing area, away from toolbars."""
    cx, cy = _center(app)
    return (cx - pad, cy - pad, pad * 2, pad * 2)


def s_default_view(app):
    app.shot('default-view')
    b = app.box()
    if b['nw'] < 200 or b['nh'] < 200:
        return f'canvas too small {b["nw"]}x{b["nh"]}'
    if app.frames() < 1:
        return 'no frame presented'
    px = app.pixels((0, 0, b['nw'], 40))
    if all(p > 245 for p in px[::4]):
        return 'top toolbar strip is blank'
    return None


def s_viewport_fit(app):
    """The UI must fit the browser viewport, not the desktop it was saved on."""
    b = app.box()
    vp = app.page.viewport_size
    app.shot('viewport-fit')
    if b['w'] > vp['width'] + 2 or b['h'] > vp['height'] + 2:
        return f'canvas {int(b["w"])}x{int(b["h"])} overflows viewport {vp["width"]}x{vp["height"]}'
    return None


def s_resize_follows(app):
    b0 = app.box()
    app.page.set_viewport_size({'width': 1000, 'height': 700})
    app.page.wait_for_timeout(600)
    b1 = app.box()
    app.shot('resize-1000x700')
    app.page.set_viewport_size({'width': 1280, 'height': 900})
    app.page.wait_for_timeout(600)
    if b1['w'] >= b0['w']:
        return f'canvas did not shrink with the window: {int(b0["w"])}x{int(b0["h"])} -> {int(b1["w"])}x{int(b1["h"])}'
    return None


def s_draw_drag(app):
    r = _region(app)
    before = app.pixels(r)
    app.drag(r[0] + 20, r[1] + 20, r[0] + r[2] - 20, r[1] + r[3] - 20, 'draw-drag')
    diff, _ = app.changed(before, r)
    if diff < 200:
        return f'drag produced only {diff} changed pixels'
    return None


def s_draw_click(app):
    cx, cy = _center(app)
    r = _region(app, 60)
    before = app.pixels(r)
    app.click(cx, cy, name='draw-click')
    diff, _ = app.changed(before, r)
    if diff < 20:
        return f'single click produced only {diff} changed pixels'
    return None


def s_draw_cancel(app):
    """Escape during a drag must not leave a shape behind."""
    r = _region(app)
    app.drag(r[0] + 30, r[1] + 30, r[0] + r[2] - 30, r[1] + 30, 'draw-then-escape-start')
    after_shape = app.pixels(r)
    app.key('Escape', 'draw-escape')
    diff, _ = app.changed(after_shape, r)
    if diff < 50:
        return f'Escape did not cancel the drawing (only {diff} pixels changed)'
    return None


def s_crosshair(app):
    """Moving the pointer repaints the crosshair on the canvas."""
    cx, cy = _center(app)
    r = _region(app, 200)
    before = app.pixels(r)
    app.move(cx - 100, cy - 100)
    app.painted(app.frames())
    app.move(cx + 100, cy + 100)
    app.painted(app.frames())
    diff, _ = app.changed(before, r)
    if diff < 20:
        return f'pointer move repainted only {diff} pixels'
    return None


def s_wheel_zoom(app):
    cx, cy = _center(app)
    r = _region(app)
    app.drag(r[0] + 20, r[1] + 20, r[0] + r[2] - 20, r[1] + r[3] - 20)
    before = app.pixels(r)
    app.wheel(cx, cy, -240)
    app.page.wait_for_timeout(500)
    diff, _ = app.changed(before, r)
    app.shot('wheel-zoom')
    if diff < 200:
        return f'mouse wheel changed only {diff} pixels (no zoom)'
    return None


def s_keyboard_shortcut(app):
    """Undo via the keyboard after a drawing operation."""
    r = _region(app)
    app.drag(r[0] + 40, r[1] + 60, r[0] + r[2] - 40, r[1] + r[3] - 60, 'kbd-draw')
    before = app.pixels(r)
    app.key('Control+z')
    app.painted(app.frames(), timeout=4000)
    diff, _ = app.changed(before, r)
    app.shot('kbd-undo')
    if diff < 200:
        return f'Ctrl+Z repainted only {diff} pixels (shortcut not delivered)'
    return None


def s_text_input(app):
    """Typing must reach a focused text control (property panel)."""
    app.shot('text-input-before')
    b = app.box()
    # Property panel sits on the right side of the main form.
    app.click(int(b['nw'] * 0.90), 120, name='text-input-focus')
    before = app.pixels((int(b['nw'] * 0.85), 100, int(b['nw'] * 0.14), 60))
    app.type('123')
    app.page.wait_for_timeout(500)
    diff, _ = app.changed(before, (int(b['nw'] * 0.85), 100, int(b['nw'] * 0.14), 60))
    app.shot('text-input-after')
    if diff < 20:
        return f'typing changed only {diff} pixels (keys not delivered to LCL)'
    return None


def s_idle_stops_repainting(app):
    """Performance oracle: no work and no new frames while nothing changes."""
    f0 = app.frames()
    app.page.wait_for_timeout(3000)
    f1 = app.frames()
    if f1 != f0:
        return f'{f1 - f0} frames presented during 3 s of idle time'
    return None


def s_title_tracks_document(app):
    t = app.title()
    if 'TpX' not in t and 'drawing' not in t.lower():
        return f'window title does not come from the application: {t!r}'
    return None


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

# SPDX-License-Identifier: MIT
"""Regression probe for the CustomDrawn WASI widgetset, run against `make web-probe`.

The probe form (demomain.pas) exercises memo text fidelity, IME composition,
focus traversal, scrolling, downloads and JOB object lifetime through the
upstream Lazarus example page. Observable DOM and canvas state is the oracle.
"""
import argparse
import json
import os
import re
from functools import partial
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from threading import Thread
from playwright.sync_api import expect, sync_playwright, TimeoutError as PlaywrightTimeoutError

ROOT = Path(__file__).resolve().parents[3]
parser = argparse.ArgumentParser()
parser.add_argument('dist', nargs='?', default=str(ROOT / 'web/build/lcl-probe'))
parser.add_argument('--chromium', default=os.environ.get('CHROMIUM', '/usr/bin/chromium'))
args = parser.parse_args()


class QuietHandler(SimpleHTTPRequestHandler):
    def log_message(self, *args):
        pass


srv = ThreadingHTTPServer(('127.0.0.1', 0), partial(QuietHandler, directory=args.dist))
Thread(target=srv.serve_forever, daemon=True).start()
args.url = f'http://127.0.0.1:{srv.server_port}/'
with sync_playwright() as p:
    browser = p.chromium.launch(headless=True, executable_path=args.chromium,
                                args=['--no-sandbox'])
    page = browser.new_page(viewport={'width': 1100, 'height': 900})
    errors = []
    page.on('pageerror', lambda error: errors.append(str(error)))
    page.goto(args.url)
    page.wait_for_selector('#status[data-state=ready]')
    page.wait_for_function('Number(document.querySelector("#lcl").dataset.frames)>0')

    # A real LCL memo is backed by a DOM textarea. Composition is held outside
    # the Pascal value until compositionend, then the committed Unicode value is
    # delivered before Ctrl+Enter is routed through the LCL key callback.
    memo = page.locator('#lcl-dom textarea')
    expect(memo).to_have_value('seed\n')
    composition = page.get_by_label('Composition status')
    value = page.get_by_label('Composition value')
    saved_text = page.get_by_label('Saved text')
    change_count = page.get_by_label('Memo change count')
    expect(composition).to_have_value('Composition: idle')
    expect(saved_text).to_have_value('Saved: none')
    expect(change_count).to_have_value('Changes: 0')

    # Focus handlers can queue async application work. It must run before the
    # browser focus callback returns, without a later key or input event.
    focus_result = page.evaluate('''() => {
        document.querySelector('#lcl-dom textarea').focus();
        document.querySelector('#lcl-dom input[aria-label="Composition status"]').focus();
        return document.querySelector('#lcl-dom input[aria-label="Saved text"]').value;
    }''')
    assert focus_result == 'Focus callback completed', focus_result

    # LCL Tab traversal must move browser focus too. If it leaves DOM focus on
    # the textarea, typing after the move still changes the memo invisibly.
    memo.focus()
    page.keyboard.press('Tab')
    destroy_on_change = page.get_by_role('button', name='Destroy on change', exact=True)
    expect(destroy_on_change).to_be_focused()
    page.keyboard.press('x')
    expect(memo).to_have_value('seed\n')
    page.keyboard.press('Shift+Tab')
    expect(memo).to_be_focused()

    memo.evaluate('''el => {
        el.focus();
        el.dispatchEvent(new CompositionEvent('compositionstart', {bubbles:true}));
        el.value = 'かな😀';
        el.dispatchEvent(new InputEvent('input', {bubbles:true, inputType:'insertCompositionText',
            data:'かな😀', isComposing:true}));
    }''')
    expect(composition).to_have_value('Composition: active')
    expect(value).to_have_value('Value: seed<LF>')
    page.get_by_role('button', name='Save text', exact=True).click()
    expect(saved_text).to_have_value('Save pending')
    memo.evaluate("el => el.dispatchEvent(new CompositionEvent('compositionend', {bubbles:true, data:'かな😀'}))")
    expect(composition).to_have_value('Composition: idle')
    expect(value).to_have_value('Value: かな😀')
    expect(saved_text).to_have_value('Saved: かな😀')
    expect(change_count).to_have_value('Changes: 1')
    memo.focus()
    page.keyboard.press('Control+Enter')
    expect(composition).to_have_value('Ctrl+Enter routed')
    page.get_by_role('button', name='Set memo text', exact=True).click()
    expect(memo).to_have_value('setter')
    expect(value).to_have_value('Value: setter')
    expect(change_count).to_have_value('Changes: 1')
    page.get_by_role('button', name='Save text', exact=True).click()
    expect(saved_text).to_have_value('Saved: setter')
    page.get_by_role('button', name='Set memo text', exact=True).click()
    expect(memo).to_have_value('setter\n')
    expect(value).to_have_value('Value: setter<LF>')
    expect(change_count).to_have_value('Changes: 1')
    old_memo = memo.element_handle()
    page.get_by_role('button', name='Recreate memo', exact=True).click()
    expect(memo).to_have_value('setter\n')
    expect(value).to_have_value('Value: setter<LF>')
    expect(change_count).to_have_value('Changes: 1')
    page.get_by_role('button', name='Save text', exact=True).click()
    expect(saved_text).to_have_value('Saved: setter<LF>')
    old_memo.evaluate('''el => {
        el.dispatchEvent(new CompositionEvent('compositionstart', {bubbles:true}));
        el.dispatchEvent(new CompositionEvent('compositionend', {bubbles:true}));
    }''')
    expect(composition).to_have_value('Ctrl+Enter routed')
    expect(memo).to_have_value('setter\n')
    expect(change_count).to_have_value('Changes: 1')
    for change_number, memo_text in enumerate(
            ['', 'alpha', 'alpha\nbeta', 'alpha\n', 'alpha\nbeta\n'], 2):
        memo.evaluate('''(el, value) => {
            el.value = value;
            el.dispatchEvent(new Event('input', {bubbles:true}));
        }''', memo_text)
        expect(memo).to_have_value(memo_text)
        value_text = memo_text.replace('\r', '<CR>').replace('\n', '<LF>')
        expect(value).to_have_value('Value: ' + value_text)
        page.get_by_role('button', name='Save text', exact=True).click()
        expect(saved_text).to_have_value('Saved: ' + value_text)
        expect(change_count).to_have_value('Changes: ' + str(change_number))

    # Leave the memo cached as the last DOM focus while the painted panel takes
    # LCL focus; then its double-click handler returns focus to the memo.
    canvas = page.locator('#lcl')
    memo.focus()
    frames = int(canvas.get_attribute('data-frames'))
    canvas.click(position={'x': 434, 'y': 169})
    expect(canvas).to_be_focused()
    page.wait_for_function(
        'frames => Number(document.querySelector("#lcl").dataset.frames) > frames',
        arg=frames)
    canvas.dblclick(position={'x': 434, 'y': 169})
    expect(memo).to_be_focused()
    memo.evaluate('el => el.setSelectionRange(el.value.length, el.value.length)')
    page.keyboard.type('!')
    expect(memo).to_have_value('alpha\nbeta\n!')
    expect(memo).to_be_focused()
    expect(change_count).to_have_value('Changes: 7')

    # The existing LCL memo keeps native textarea wheel scrolling.
    memo.evaluate('''el => {
        el.value = Array(80).fill('wheel line').join(String.fromCharCode(10));
        el.scrollTop = 0;
    }''')
    assert memo.evaluate('(el) => el.scrollHeight > el.clientHeight')
    textarea_box = memo.bounding_box()
    assert textarea_box is not None
    outer_scroll = page.locator('.scroller')
    textarea_page_scroll = outer_scroll.evaluate('(el) => el.scrollTop')
    textarea_canvas_top = canvas.bounding_box()['y']
    page.mouse.move(textarea_box['x'] + textarea_box['width'] / 2,
                    textarea_box['y'] + textarea_box['height'] / 2)
    page.mouse.wheel(0, 120)
    page.wait_for_function('(el) => el.scrollTop > 0', arg=memo.element_handle())
    assert outer_scroll.evaluate('(el) => el.scrollTop') == textarea_page_scroll
    assert abs(canvas.bounding_box()['y'] - textarea_canvas_top) < 1

    page.get_by_role('button', name='Destroy on change', exact=True).click()
    old_memo = memo.element_handle()
    old_memo.evaluate('''el => {
        el.dispatchEvent(new CompositionEvent('compositionstart', {bubbles:true}));
        el.value = 'destroy';
        el.dispatchEvent(new CompositionEvent('compositionend', {bubbles:true, data:'destroy'}));
    }''')
    expect(value).to_have_value('Memo destroyed')
    expect(change_count).to_have_value('Changes: 8')
    old_memo.evaluate('''el => {
        el.dispatchEvent(new CompositionEvent('compositionstart', {bubbles:true}));
        el.dispatchEvent(new CompositionEvent('compositionend', {bubbles:true}));
    }''')
    expect(composition).to_have_value('Composition: idle')
    expect(change_count).to_have_value('Changes: 8')

    # A painted combo has no overlay element of its own. Leaving the DOM edit
    # focused would make the host discard ArrowDown before it reaches LCL.
    focus_source = page.get_by_label('Composition value')
    focus_source.focus()
    expect(focus_source).to_be_focused()
    page.keyboard.press('Tab')
    expect(saved_text).to_have_value('Painted combo focused')
    expect(focus_source).not_to_be_focused()
    page.keyboard.press('ArrowDown')
    expect(saved_text).to_have_value('Painted combo: Bold')
    expect(focus_source).to_have_value('Memo destroyed')

    def open_painted_combo_picker():
        canvas = page.locator('#lcl')
        canvas_box = canvas.bounding_box()
        canvas_width, canvas_height = canvas.evaluate('(el) => [el.width, el.height]')
        page.mouse.click(canvas_box['x'] + 686 * canvas_box['width'] / canvas_width,
                         canvas_box['y'] + 139 * canvas_box['height'] / canvas_height)
        picker = page.get_by_role('dialog', name='Choose item')
        expect(picker).to_be_visible()
        return picker

    picker = open_painted_combo_picker()
    expect(picker.locator('option:checked')).to_have_text('Bold')
    picker.get_by_role('button', name='Cancel').click()
    expect(saved_text).to_have_value('Painted combo: Bold')

    picker = open_painted_combo_picker()
    expect(picker.locator('option:checked')).to_have_text('Bold')
    picker.get_by_role('button', name='OK').click()
    expect(saved_text).to_have_value('Painted combo: Bold')
    picker = open_painted_combo_picker()
    expect(picker.locator('option:checked')).to_have_text('Bold')
    picker.get_by_role('button', name='Cancel').click()

    # The DOM proxy for the painted combo is only the canvas. Tab changes LCL
    # focus synchronously, while its normal DOM synchronization is deferred to
    # the next animation frame. A following key in that same frame must still
    # reach the newly focused painted control.
    page.keyboard.press('ArrowUp')
    expect(saved_text).to_have_value('Painted combo: Regular')
    focus_source.focus()
    expect(focus_source).to_be_focused()
    page.evaluate('''() => new Promise(resolve =>
        requestAnimationFrame(() => requestAnimationFrame(resolve)))''')
    page.evaluate('''() => {
        const nativeRequestAnimationFrame = window.requestAnimationFrame;
        const pendingFrames = [];
        window.requestAnimationFrame = callback => {
            pendingFrames.push(callback);
            return pendingFrames.length;
        };
        window.releaseHeldFrames = () => {
            window.requestAnimationFrame = nativeRequestAnimationFrame;
            for (const callback of pendingFrames.splice(0)) callback(performance.now());
            delete window.releaseHeldFrames;
        };
    }''')
    page.keyboard.press('Tab')
    dom_focus_after_tab = page.evaluate('''() => {
        const active = document.activeElement;
        return [active.tagName, active.id, active.getAttribute('aria-label')];
    }''')
    page.keyboard.press('ArrowDown')
    page.keyboard.press('Shift+Tab')
    assert dom_focus_after_tab[:2] == ['CANVAS', 'lcl'], (
        f'Tab did not move DOM focus to the canvas before the next frame: {dom_focus_after_tab}')
    assert focus_source.evaluate('(el) => el === document.activeElement'), (
        'Shift+Tab did not synchronously return DOM focus to Composition value')
    page.evaluate('window.releaseHeldFrames()')
    actual_saved_text = saved_text.input_value()
    assert actual_saved_text == 'Painted combo: Bold', (
        'ArrowDown in the same frame as Tab did not reach the painted combo; '
        f'DOM focus after Tab was {dom_focus_after_tab}, saved text was {actual_saved_text!r}')

    def frame():
        return page.locator('#lcl').get_attribute('data-frames')

    def painted_after(old):
        page.wait_for_function('(old) => document.querySelector("#lcl").dataset.frames !== old', arg=old)

    def pixel(x, y):
        return page.evaluate('([x,y]) => Array.from(document.querySelector("#lcl").getContext("2d").getImageData(x,y,1,1).data)', [x, y])

    def wait_pixel(x, y, color):
        page.wait_for_function('''([x,y,color]) => {
            const p = document.querySelector('#lcl').getContext('2d').getImageData(x,y,1,1).data;
            return color.every((v,i) => p[i] === v);
        }''', arg=[x, y, color])

    # A scroll box reveals both DOM controls and painted children. The lower
    # button has no OnEnter handler, so reaching it exercises the backend's
    # focus-to-scroll handoff rather than an application workaround.
    canvas_box = canvas.bounding_box()
    canvas_top = canvas_box['y']
    scroll_source = page.get_by_label('Scroll source')
    top_scroll_button = page.get_by_role('button', name='Top scroll button', exact=True)
    scroll_button = page.get_by_role('button', name=re.compile(r'^Scroll button'))
    scroll_target = page.get_by_label('Scroll target')
    wait_pixel(636, 455, [255, 0, 0])
    wait_pixel(526, 455, [0, 0, 255])
    assert pixel(636, 465) != [255, 0, 0, 255], 'Paint escaped the initial scroll viewport'
    assert pixel(526, 465) != [0, 0, 255, 255], 'Nested paint escaped the initial scroll viewport'
    scroll_source.focus()
    page.keyboard.press('Tab')
    expect(top_scroll_button).to_be_focused()
    page.keyboard.press('Tab')
    expect(scroll_button).to_be_focused()

    def app_rect(locator):
        return locator.evaluate('''(el) => {
            const rect = el.getBoundingClientRect();
            const canvas = document.querySelector('#lcl');
            const surface = canvas.getBoundingClientRect();
            return {
                x: (rect.left - surface.left) * canvas.width / surface.width,
                y: (rect.top - surface.top) * canvas.height / surface.height,
                width: rect.width * canvas.width / surface.width,
                height: rect.height * canvas.height / surface.height
            };
        }''')

    def inside_scrollbox(rect):
        return (rect['x'] >= 344 and rect['x'] + rect['width'] <= 696
                and rect['y'] >= 280 and rect['y'] + rect['height'] <= 460)

    def click_app(x, y):
        box = canvas.bounding_box()
        size = canvas.evaluate('(el) => ({width: el.width, height: el.height})')
        page.mouse.click(box['x'] + x * box['width'] / size['width'],
                         box['y'] + y * box['height'] / size['height'])

    button_rect = app_rect(scroll_button)
    assert inside_scrollbox(button_rect), button_rect
    assert abs(canvas.bounding_box()['y'] - canvas_top) < 1, 'Focusing a scrolled control moved the page'
    scroll_button.click()
    expect(scroll_button).to_have_text('Scroll button clicked')
    page.keyboard.press('Tab')
    expect(scroll_target).to_be_focused()
    target_rect = app_rect(scroll_target)
    assert inside_scrollbox(target_rect), target_rect

    # The painted child moves with the same scroll offset and remains a real
    # pointer target after the geometry update.
    button_top = scroll_button.evaluate('(el) => parseInt(el.style.top, 10)')
    scroll_offset = 500 - button_top
    red_x, red_y = 636, 280 + 170 - scroll_offset + 12
    blue_x, blue_y = 526, 280 + 135 + 35 - scroll_offset + 12
    wait_pixel(red_x, red_y, [255, 0, 0])
    wait_pixel(blue_x, blue_y, [0, 0, 255])
    assert pixel(636, 465) != [255, 0, 0, 255], 'Paint escaped the scrolled viewport'
    assert pixel(526, 465) != [0, 0, 255, 255], 'Nested paint escaped the scrolled viewport'
    click_app(red_x, red_y)
    expect(saved_text).to_have_value('Scroll click 1: ScrollPaint')
    click_app(blue_x, blue_y)
    expect(saved_text).to_have_value('Scroll click 2: ScrollNestedPaint')

    # Moving focus back to the top returns the scroll position once. The lower
    # button remains in the canvas surface but is clipped by the scroll box.
    scroll_source.focus()
    page.keyboard.press('Tab')
    expect(top_scroll_button).to_be_focused()
    page.keyboard.press('Tab')
    expect(scroll_button).to_be_focused()
    page.keyboard.press('Tab')
    expect(scroll_target).to_be_focused()
    assert inside_scrollbox(app_rect(scroll_target)), app_rect(scroll_target)
    click_app(690, 290)
    page.wait_for_function(
        '(el) => parseInt(el.style.top, 10) === 500',
        arg=scroll_button.element_handle())
    expect(saved_text).to_have_value('Scroll click 2: ScrollNestedPaint')
    lower_button_hit = scroll_button.evaluate('''el => {
        const r = el.getBoundingClientRect();
        return document.elementFromPoint(r.left + r.width / 2, r.top + r.height / 2) === el;
    }''')
    assert not lower_button_hit, 'An off-viewport scroll-box button received a DOM pointer hit'
    assert abs(canvas.bounding_box()['y'] - canvas_top) < 1, 'Scrolling the LCL viewport moved the page'
    wait_pixel(636, 455, [255, 0, 0])
    wait_pixel(526, 455, [0, 0, 255])
    assert pixel(636, 465) != [255, 0, 0, 255], 'Paint escaped after scrolling back'
    assert pixel(526, 465) != [0, 0, 255, 255], 'Nested paint escaped after scrolling back'
    click_app(636, 465)
    expect(saved_text).to_have_value('Scroll click 2: ScrollNestedPaint')
    click_app(526, 465)
    expect(saved_text).to_have_value('Scroll click 2: ScrollNestedPaint')

    count = page.get_by_role('button', name=re.compile(r'^Count: \d+$'))
    count.click()
    expect(count).to_have_text('Count: 1')
    count.focus()
    page.keyboard.press('Enter')
    expect(count).to_have_text('Count: 2')
    assert count.evaluate('(el) => el === document.activeElement'), 'Repaint stole DOM focus'

    # The LFM/Pascal specifies light blue RGB(175,207,234), not a host-generated shape.
    assert pixel(158, 263) == [175, 207, 234, 255], pixel(158, 263)
    checkbox = page.get_by_role('checkbox', name='Filled shape')
    old = frame()
    checkbox.focus()
    page.keyboard.press('Space')
    painted_after(old)
    wait_pixel(158, 263, [255, 255, 255, 255])
    assert not checkbox.is_checked()
    assert pixel(158, 263) == [255, 255, 255, 255]
    old = frame()
    checkbox.click()
    painted_after(old)
    wait_pixel(158, 263, [175, 207, 234, 255])
    assert checkbox.is_checked()
    assert pixel(158, 263) == [175, 207, 234, 255]

    box = page.locator('#lcl').bounding_box()
    old = frame()
    page.mouse.click(box['x']+464, box['y']+264)
    painted_after(old)
    assert pixel(464, 264) == [38, 122, 56, 255], pixel(464, 264)
    old = frame()
    page.get_by_role('button', name='Reset', exact=True).click()
    painted_after(old)
    assert count.inner_text() == 'Count: 0'
    assert pixel(464, 264) == [255, 255, 255, 255]

    before = page.evaluate('({live:jobHost.live(),slots:jobHost.slots()})')
    count.focus()
    for _ in range(100):
        page.keyboard.press('Enter')
    expect(count).to_have_text('Count: 100')
    after = page.evaluate('({live:jobHost.live(),slots:jobHost.slots()})')
    assert after['live'] == before['live'], (before, after)
    assert after['slots'] <= before['slots']+2, (before, after)

    # Exercise the platform resize boundary; the LFM's Anchors must grow the paintbox.
    old = frame()
    page.evaluate('lclDemo.lcl_resize(800,520)')
    page.wait_for_function('document.querySelector("#lcl").width === 800 && document.querySelector("#lcl").height === 520')
    painted_after(old)
    assert page.locator('#lcl').get_attribute('width') == '800'
    assert pixel(760, 490) == [255, 255, 255, 255], pixel(760, 490)

    save_button = page.get_by_role('button', name='Save file', exact=True)
    save_again = page.get_by_role('button', name='Save again', exact=True)
    atomic = page.get_by_role('checkbox', name='Atomic rename', exact=True)
    fail_rename = page.get_by_role('checkbox', name='Fail rename', exact=True)
    payload = b'WASI browser save payload\n'
    for name, expected_name, use_atomic in [
            ('direct', 'direct.txt', False), ('atomic.txt', 'atomic.txt', True)]:
        atomic.set_checked(use_atomic)
        save_button.click()
        dialog = page.get_by_role('dialog')
        dialog.get_by_label('File name').fill(name)
        with page.expect_download() as pending:
            dialog.get_by_role('button', name='OK', exact=True).click()
        saved = pending.value
        assert saved.suggested_filename == expected_name, saved.suggested_filename
        assert Path(saved.path()).read_bytes() == payload
        with page.expect_download() as repeated:
            save_again.click()
        repeated_save = repeated.value
        assert repeated_save.suggested_filename == expected_name, repeated_save.suggested_filename
        assert Path(repeated_save.path()).read_bytes() == payload
        assert page.get_by_role('dialog').count() == 0, 'Save again unexpectedly opened a dialog'

    atomic.check()
    save_button.click()
    dialog = page.get_by_role('dialog')
    dialog.get_by_label('File name').fill('explicit.tex')
    with page.expect_download() as alternate_extension:
        dialog.get_by_role('button', name='OK', exact=True).click()
    explicit_save = alternate_extension.value
    assert explicit_save.suggested_filename == 'explicit.tex', explicit_save.suggested_filename
    assert Path(explicit_save.path()).read_bytes() == payload

    fail_rename.check()
    save_button.click()
    dialog = page.get_by_role('dialog')
    dialog.get_by_label('File name').fill('rename-failure.txt')
    try:
        with page.expect_download(timeout=750):
            dialog.get_by_role('button', name='OK', exact=True).click()
    except PlaywrightTimeoutError:
        pass
    else:
        raise AssertionError('failed staged rename produced a download')
    # Wheel input over a DOM overlay must scroll the LCL viewport instead of
    # the outer page. The overlay button remains a real browser hit target.
    outer_scroll = page.locator('.scroller')
    outer_scroll_top = outer_scroll.evaluate('(el) => el.scrollTop')
    top_button_rect = top_scroll_button.bounding_box()
    page.mouse.move(top_button_rect['x'] + top_button_rect['width'] / 2,
                    top_button_rect['y'] + top_button_rect['height'] / 2)
    page.mouse.wheel(0, 120)
    page.wait_for_function(
        '(el) => parseInt(el.style.top, 10) < 500',
        arg=scroll_button.element_handle())
    assert outer_scroll.evaluate('(el) => el.scrollTop') == outer_scroll_top
    assert abs(canvas.bounding_box()['y'] - canvas_top) < 1
    page.mouse.wheel(0, -120)
    page.wait_for_function(
        '(el) => parseInt(el.style.top, 10) === 500',
        arg=scroll_button.element_handle())
    assert outer_scroll.evaluate('(el) => el.scrollTop') == outer_scroll_top

    # Wheel input over a painted canvas child follows the same ScrollBox path.
    painted_wheel_top = scroll_button.evaluate('(el) => parseInt(el.style.top, 10)')
    assert painted_wheel_top == 500
    canvas_box = canvas.bounding_box()
    page.mouse.move(canvas_box['x'] + 636, canvas_box['y'] + 455)
    page.mouse.wheel(0, 120)
    page.wait_for_function(
        '(el) => parseInt(el.style.top, 10) < 500',
        arg=scroll_button.element_handle())
    assert outer_scroll.evaluate('(el) => el.scrollTop') == outer_scroll_top
    page.mouse.wheel(0, -120)
    page.wait_for_function(
        '(el) => parseInt(el.style.top, 10) === 500',
        arg=scroll_button.element_handle())

    assert page.locator('#status').get_attribute('data-state') == 'ready'
    assert not errors, errors
    print(json.dumps({'dom_mouse_and_keyboard': True, 'pascal_canvas_colors': True,
                      'dom_textarea_composition_unicode_and_ctrl_enter': True,
                      'checkbox_changes_lcl_paint': True, 'canvas_pointer_and_reset': True,
                      'anchored_resize': True, 'callback_cycles': 100,
                      'direct_download': True, 'default_extension': True,
                      'explicit_alternate_extension': True, 'atomic_rename_download': True,
                      'direct_repeat_download': True, 'atomic_repeat_download': True,
                      'failed_rename_no_download': True,
                      'job_before': before, 'job_after': after}))
    browser.close()

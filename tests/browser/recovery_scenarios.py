"""Browser-local recovery behavior in real browser storage and WASM sessions."""

from harness import App
from scenarios import box, diff, drawing_xml, park, region


DB_NAME = 'tpx-browser-local-recovery'


def wait_ready(page):
    page.wait_for_selector('#status[data-state=ready]', timeout=30000)
    page.wait_for_function('Number(document.querySelector("#lcl").dataset.frames)>0')


def decode_tpx_text_once(value):
    """Decode TpX text references after XML parsing, preserving literal entities."""
    import re

    def decode(match):
        codepoint = int(match.group(1))
        return chr(codepoint) if codepoint <= 0x10FFFF else match.group(0)

    return re.sub(r'&#([0-9]+);', decode, value)


def opt_in(page):
    page.locator('#tpx-recovery-enabled').check()
    page.wait_for_function('document.querySelector("#tpx-recovery-enabled").checked')


def records(page):
    return page.evaluate('''async (name) => {
      const request = indexedDB.open(name);
      const db = await new Promise((resolve, reject) => {
        request.onsuccess = () => resolve(request.result);
        request.onerror = () => reject(request.error);
      });
      try {
        const tx = db.transaction('drafts', 'readonly');
        const rows = await new Promise((resolve, reject) => {
          const get = tx.objectStore('drafts').getAll();
          get.onsuccess = () => resolve(get.result || []);
          get.onerror = () => reject(get.error);
        });
        return Promise.all(rows.map(async row => ({id: row.id, byteLength: row.byteLength,
          updatedAt: row.updatedAt, sequence: row.sequence, title: row.metadata?.title || '',
          payloadHash: Array.from(new Uint8Array(await crypto.subtle.digest(
            'SHA-256', row.payload))).map(value => value.toString(16).padStart(2, '0')).join('')})));
      } finally { db.close(); }
    }''', DB_NAME)


def stored_record_details(page, record_id):
    return page.evaluate('''async ({name, id}) => {
      const open = indexedDB.open(name);
      const db = await new Promise((resolve, reject) => {
        open.onsuccess = () => resolve(open.result);
        open.onerror = () => reject(open.error);
      });
      try {
        const tx = db.transaction('drafts', 'readonly');
        const row = await new Promise((resolve, reject) => {
          const get = tx.objectStore('drafts').get(id);
          get.onsuccess = () => resolve(get.result);
          get.onerror = () => reject(get.error);
        });
        if (!row) return null;
        const bytes = new Uint8Array(row.payload), view = new DataView(row.payload);
        let at = 8;
        const u32 = () => { const value = view.getUint32(at, true); at += 4; return value; };
        const u64 = () => { const value = Number(view.getBigUint64(at, true)); at += 8; return value; };
        const str = () => { const size = view.getInt32(at, true); at += 4;
          const value = new TextDecoder().decode(bytes.subarray(at, at + size)); at += size; return value; };
        const magic = new TextDecoder().decode(bytes.subarray(0, 8));
        if (magic !== 'TPXREC03') throw new Error('recovery record signature changed');
        const kind = bytes[at++];
        const storedId = str(), sourcePath = str(), sourceFormat = str(), format = str();
        const digest = str(); view.getBigInt64(at, true); at += 8;
        view.getBigInt64(at, true); at += 8; str(); at++;
        view.getBigUint64(at, true); at += 8;
        const sourceLength = u64(), source = bytes.subarray(at, at + sourceLength); at += sourceLength;
        const payloadLength = u64(), payload = bytes.subarray(at, at + payloadLength);
        const hash = Array.from(new Uint8Array(await crypto.subtle.digest('SHA-256', source)))
          .map(value => value.toString(16).padStart(2, '0')).join('');
        const payloadMagic = new TextDecoder().decode(payload.subarray(0, 8));
        const payloadView = new DataView(payload.buffer, payload.byteOffset, payload.byteLength);
        return {kind, storedId, sourcePath, sourceFormat, format, digest, sourceHash: hash,
          payloadMagic, unresolved: payloadView.getUint32(12, true),
          assets: payloadView.getUint32(16, true)};
      } finally { db.close(); }
    }''', {'name': DB_NAME, 'id': record_id})


def add_records(page, rows):
    page.evaluate('''async ({name, rows}) => {
      const request = indexedDB.open(name);
      const db = await new Promise((resolve, reject) => {
        request.onsuccess = () => resolve(request.result);
        request.onerror = () => reject(request.error);
      });
      try {
        const tx = db.transaction('drafts', 'readwrite');
        for (const row of rows) {
          row.payload = new Uint8Array(row.payload).buffer;
          tx.objectStore('drafts').put(row);
        }
        await new Promise((resolve, reject) => {
          tx.oncomplete = resolve; tx.onabort = tx.onerror = () => reject(tx.error);
        });
      } finally { db.close(); }
    }''', {'name': DB_NAME, 'rows': rows})


def draw_line(app):
    _, cx, cy = box(app)
    area = region(cx, cy, 320, 240)
    app.menu('Insert', 'Insert line')
    clean = app.pixels(area)
    app.drag(cx - 90, cy - 60, cx + 90, cy + 60)
    park(app, box(app)[0])
    return area, clean


def wait_saved(page):
    try:
        page.wait_for_function('document.querySelector("#tpx-recovery-state").textContent.includes("Draft saved in this browser")',
                               timeout=10000)
    except Exception as exc:
        state = page.locator('#tpx-recovery-state').inner_text()
        document = page.locator('#document-status').inner_text()
        raise AssertionError(f'checkpoint did not finish: recovery={state!r}; document={document!r}') from exc


def s_recovery_disabled_by_default(app):
    if app.page.locator('#tpx-recovery-enabled').is_checked():
        raise AssertionError('browser-local recovery unexpectedly started opted in')
    draw_line(app)
    app.page.wait_for_timeout(1100)
    if records(app.page):
        raise AssertionError('a drawing was stored before explicit opt-in')
    return 'recovery remains off and stores no drawing until opted in'


def s_recovery_survives_application_restart(app):
    opt_in(app.page)
    area, clean = draw_line(app)
    wait_saved(app.page)
    before = records(app.page)
    if len(before) != 1 or before[0]['byteLength'] <= 0:
        raise AssertionError(f'expected one durable draft, got {before}')

    app.restart()
    wait_ready(app.page)
    app.page.wait_for_function('document.querySelector("#tpx-recovery-enabled").checked')
    button = app.page.get_by_role('button', name='Recover as new drawing', exact=True)
    button.wait_for()
    blank = app.pixels(area)
    button.click()
    app.page.wait_for_function('document.querySelector("#tpx-recovery-state").textContent.includes("Draft recovered as a new TpX drawing")',
                               timeout=10000)
    status = app.page.locator('#document-status')
    if (status.get_attribute('data-state') != 'unsaved' or
            'original file is unchanged' not in status.inner_text()):
        raise AssertionError('recovered geometry was represented as synchronized with an original file')
    app.page.wait_for_timeout(200)
    ink = diff(blank, app.pixels(area))
    if ink < 100:
        raise AssertionError(f'recovered geometry painted only {ink} changed pixels')
    after = records(app.page)
    if before[0]['id'] not in {row['id'] for row in after} or len(after) > 2:
        raise AssertionError('recovery silently removed its durable source draft or created excess records')
    retained = next(row for row in after if row['id'] == before[0]['id'])
    if retained['payloadHash'] != before[0]['payloadHash']:
        raise AssertionError('recovery rewrote the original durable source draft')
    if app.page.locator('#tpx-recovery-drafts [data-restore]').first.is_enabled():
        raise AssertionError('a dirty recovered drawing still allowed another restore')
    return f'IndexedDB draft survived app reload and restored {ink} geometry pixels as a new drawing'


def hold_recovery_checkpoint_timer(page, record_id):
    page.evaluate('''(recordId) => {
      const originalSetInterval = window.setInterval.bind(window);
      window.tpxRecoveryTimerCallback = null;
      window.tpxRecoveryTimerCreated = 0;
      window.setInterval = (callback, delay, ...args) => {
        if (delay === 750) {
          window.tpxRecoveryTimerCallback = callback;
          window.tpxRecoveryTimerCreated++;
          return originalSetInterval(() => {}, delay, ...args);
        }
        return originalSetInterval(callback, delay, ...args);
      };
      const originalPut = IDBObjectStore.prototype.put;
      window.tpxRecoveryCheckpointCommitted = false;
      window.tpxRecoveryCheckpointSequence = 0;
      IDBObjectStore.prototype.put = function(value, ...args) {
        if (this.name === 'drafts' && value?.id === recordId) {
          const sequence = value.sequence;
          this.transaction.addEventListener('complete', () => {
            window.tpxRecoveryCheckpointCommitted = true;
            window.tpxRecoveryCheckpointSequence = sequence;
          }, {once: true});
        }
        return originalPut.call(this, value, ...args);
      };
    }''', record_id)


def s_recovery_noop_download_keeps_previous_draft(app):
    opt_in(app.page)
    draw_line(app)
    wait_saved(app.page)
    before = records(app.page)
    if len(before) != 1:
        raise AssertionError(f'expected one existing draft before download, got {before}')

    hold_recovery_checkpoint_timer(app.page, before[0]['id'])
    draw_line(app)
    app.page.wait_for_function('window.tpxRecoveryTimerCallback !== null')
    app.page.evaluate('''() => {
      window.tpxNoopDownloadClicks = 0;
      HTMLAnchorElement.prototype.click = function() { window.tpxNoopDownloadClicks++; };
    }''')
    app.menu('File', 'Save as...')
    dialog = app.page.get_by_role('dialog')
    dialog.get_by_label('File name').fill('browser-noop-download.tpx')
    dialog.get_by_role('button', name='OK', exact=True).click()
    app.page.wait_for_function('window.tpxNoopDownloadClicks === 1')
    app.page.wait_for_function('document.querySelector("#document-status").dataset.state === "unconfirmed"')
    app.page.wait_for_function('window.tpxRecoveryCheckpointCommitted === true')
    status = app.page.locator('#document-status').inner_text()
    if 'cannot confirm' not in status:
        raise AssertionError(f'no-op anchor was represented as a durable copy: {status}')
    after_request = records(app.page)
    if (len(after_request) != 1 or after_request[0]['id'] != before[0]['id'] or
            after_request[0]['payloadHash'] == before[0]['payloadHash'] or
            app.page.evaluate('window.tpxRecoveryCheckpointSequence') <= before[0]['sequence']):
        raise AssertionError('the pre-download edit was not checkpointed under the prior recovery identity')

    draw_line(app)
    app.page.wait_for_function('window.tpxRecoveryTimerCallback !== null')
    app.page.evaluate('window.tpxRecoveryTimerCallback()')
    app.page.wait_for_function('''async (oldId) => {
      const request = indexedDB.open('tpx-browser-local-recovery');
      const db = await new Promise((resolve, reject) => {
        request.onsuccess = () => resolve(request.result);
        request.onerror = () => reject(request.error);
      });
      try {
        const tx = db.transaction('drafts', 'readonly');
        const rows = await new Promise((resolve, reject) => {
          const get = tx.objectStore('drafts').getAll();
          get.onsuccess = () => resolve(get.result || []);
          get.onerror = () => reject(get.error);
        });
        return rows.length === 2 && rows.some(row => row.id !== oldId);
      } finally { db.close(); }
    }''', arg=before[0]['id'], timeout=10000)
    after_edit = records(app.page)
    if before[0]['id'] not in {row['id'] for row in after_edit} or len(after_edit) != 2:
        raise AssertionError(f'new edit did not rotate recovery identity while retaining prior draft: {after_edit}')
    retained = next(row for row in after_edit if row['id'] == before[0]['id'])
    if retained['payloadHash'] != after_request[0]['payloadHash']:
        raise AssertionError('the older recovery identity changed after rotation')
    return 'an unconfirmed no-op download checkpoints the latest scene under the old ID, then later edits use a new ID'


def s_recovery_restores_raw_inline_label_after_restart(app):
    opt_in(app.page)
    _, cx, cy = box(app)
    raw_label = 'Uncommitted λ label: \\input{must-not-run}; literal &#955;'
    app.menu('Insert', 'Insert text')
    app.click(cx - 60, cy - 20)
    editor = app.page.get_by_role('textbox', name='Text (canvas)', exact=True)
    editor.wait_for()
    editor.fill(raw_label)
    wait_saved(app.page)

    page = app.restart()
    wait_ready(page)
    page.get_by_role('button', name='Recover as new drawing', exact=True).click()
    page.wait_for_function('document.querySelector("#tpx-recovery-state").textContent.includes("Draft recovered as a new TpX drawing")',
                           timeout=10000)
    editor = page.get_by_role('textbox', name='Text (canvas)', exact=True)
    editor.wait_for(timeout=10000)
    if editor.input_value() != raw_label:
        raise AssertionError(f'recovered inline editor lost raw label text: {editor.input_value()!r}')
    page.get_by_role('button', name='Done', exact=True).click()
    saved = drawing_xml(app.save('recovered-raw-label.tpx'))
    labels = [decode_tpx_text_once(node.attrib['t']) for node in saved.iter('text')
              if 't' in node.attrib]
    if raw_label not in labels:
        raise AssertionError('committed recovered label is missing from the downloaded TpX source')
    return 'a browser process restart restores the exact uncommitted Unicode label and its later TpX source'


def s_recovery_skips_active_composition_and_restores_commit(app):
    opt_in(app.page)
    _, cx, cy = box(app)
    app.menu('Insert', 'Insert text')
    app.click(cx - 60, cy - 20)
    editor = app.page.locator('#lcl-dom textarea')
    editor.wait_for()
    stable = 'Committed before composition'
    editor.fill(stable)
    wait_saved(app.page)
    before = records(app.page)
    if len(before) != 1:
        raise AssertionError(f'expected one baseline draft, got {before}')
    hold_recovery_checkpoint_timer(app.page, before[0]['id'])

    def start_composition(text):
        app.page.evaluate('''({text}) => {
          const el = document.querySelector('#lcl-dom textarea');
          el.focus();
          el.dispatchEvent(new CompositionEvent('compositionstart', {bubbles:true}));
          el.value = text;
          el.dispatchEvent(new InputEvent('input', {bubbles:true,
            inputType:'insertCompositionText', data:text, isComposing:true}));
        }''', {'text': text})

    def force_composition_checkpoint():
        generation = app.page.evaluate('window.tpxRecoveryTimerCreated')
        app.page.wait_for_function('(old) => window.tpxRecoveryTimerCreated > old', arg=generation,
                                   timeout=10000)
        app.page.evaluate('window.tpxRecoveryCheckpointCommitted = false')
        app.page.evaluate('window.tpxRecoveryTimerCallback()')
        app.page.wait_for_function('document.querySelector("#tpx-recovery-state").textContent.includes("Text composition is still active")',
                                   timeout=10000)
        rows = records(app.page)
        if len(rows) != 1 or rows[0]['payloadHash'] != before[0]['payloadHash']:
            raise AssertionError('an active composition replaced the last safe recovery payload')

    start_composition('Uncommitted preedit')
    force_composition_checkpoint()

    app.page.evaluate('''(text) => {
      const el = document.querySelector('#lcl-dom textarea');
      el.value = text;
      el.dispatchEvent(new CompositionEvent('compositionend', {bubbles:true, data:''}));
    }''', stable)
    app.page.wait_for_function('document.querySelector("#tpx-recovery-state").textContent.includes("New changes are not yet saved")',
                               timeout=10000)
    app.page.evaluate('window.tpxRecoveryCheckpointCommitted = false')
    app.page.evaluate('window.tpxRecoveryTimerCallback()')
    app.page.wait_for_function('window.tpxRecoveryCheckpointCommitted === true', timeout=10000)
    after_cancel = records(app.page)
    if (len(after_cancel) != 1 or
            after_cancel[0]['payloadHash'] != before[0]['payloadHash'] or
            after_cancel[0]['sequence'] <= before[0]['sequence']):
        raise AssertionError('cancelled composition changed or failed to checkpoint the stable draft')

    start_composition('Uncommitted λ label: \\input{pending}')
    force_composition_checkpoint()
    committed = 'Committed λ label: \\input{pending}'
    app.page.evaluate('''(text) => {
      const el = document.querySelector('#lcl-dom textarea');
      el.value = text;
      el.dispatchEvent(new CompositionEvent('compositionend', {bubbles:true, data:text}));
    }''', committed)
    app.page.wait_for_function('document.querySelector("#tpx-recovery-state").textContent.includes("New changes are not yet saved")',
                               timeout=10000)
    app.page.evaluate('window.tpxRecoveryCheckpointCommitted = false')
    app.page.evaluate('window.tpxRecoveryTimerCallback()')
    app.page.wait_for_function('window.tpxRecoveryCheckpointCommitted === true', timeout=10000)
    after_commit = records(app.page)
    if (len(after_commit) != 1 or
            after_commit[0]['payloadHash'] == after_cancel[0]['payloadHash']):
        raise AssertionError('committed composition was not checkpointed as a new raw draft')

    page = app.restart()
    wait_ready(page)
    page.get_by_role('button', name='Recover as new drawing', exact=True).click()
    page.wait_for_function('document.querySelector("#tpx-recovery-state").textContent.includes("Draft recovered as a new TpX drawing")',
                           timeout=10000)
    recovered = page.locator('#lcl-dom textarea')
    recovered.wait_for(timeout=10000)
    if recovered.input_value() != committed:
        raise AssertionError(f'restarted recovery lost committed composition text: {recovered.input_value()!r}')
    return 'active preedit preserved the last safe payload; cancellation retained it and committed composition survived restart'


def s_recovery_retains_source_and_linked_asset(app):
    import hashlib
    import re
    import struct
    import zlib

    def chunk(kind, data):
        return (struct.pack('>I', len(data)) + kind + data +
                struct.pack('>I', zlib.crc32(kind + data)))

    asset = app.output / 'recovery-linked-image.png'
    asset.write_bytes(b'\x89PNG\r\n\x1a\n' +
        chunk(b'IHDR', struct.pack('>IIBBBBB', 32, 32, 8, 2, 0, 0, 0)) +
        chunk(b'IDAT', zlib.compress((b'\x00' + b'\xff\x00\xff' * 32) * 32)) +
        chunk(b'IEND', b''))
    _, cx, cy = box(app)
    app.menu('Insert', 'Insert bitmap')
    dialog = app.page.get_by_role('dialog')
    dialog.get_by_label('Choose file', exact=True).set_input_files(asset)
    before = app.frames()
    dialog.get_by_role('button', name='OK', exact=True).click()
    app.painted(before)
    app.drag(cx - 70, cy - 50, cx + 70, cy + 50)
    app.key('Escape')
    app.menu('Insert', 'Insert text')
    text_prompt = app.page.locator('#lcl-dom input[aria-label="Edit text"]:focus:visible')
    if text_prompt.count():
        text_prompt.fill('Uploaded source label')
        text_prompt.press('Enter')
        app.click(cx - 40, cy + 65)
    else:
        app.click(cx - 40, cy + 65)
        label = app.page.get_by_role('textbox', name='Text (canvas)', exact=True)
        label.wait_for()
        label.fill('Uploaded source label')
        app.page.get_by_role('button', name='Done', exact=True).click()

    related_downloads = []
    app.page.on('download', lambda download: related_downloads.append(download))
    source = app.save('recovery-source.tpx')
    app.page.wait_for_timeout(150)
    asset_download = next((item for item in related_downloads
                           if item.suggested_filename == asset.name), None)
    if asset_download is None:
        raise AssertionError('source fixture download omitted the linked image')
    asset_copy = app.output / asset_download.suggested_filename
    asset_download.save_as(asset_copy)

    source_text = source.read_text()
    match = re.search(r'<text\b[^>]*>', source_text)
    if not match or not match.group(0).endswith('/>'):
        raise AssertionError('source fixture did not contain a self-closing text element')
    tag = match.group(0)
    updated = tag[:-2] + ' tex="\\input{browser-recovery-must-not-compile}"/>'
    source_text = source_text[:match.start()] + updated + source_text[match.end():]
    source.write_text(source_text)
    original_source = source.read_bytes()
    original_asset = asset_copy.read_bytes()

    app.restart()
    wait_ready(app.page)
    opt_in(app.page)
    app.menu('File', 'Open')
    dialog = app.page.get_by_role('dialog')
    dialog.get_by_label('Choose file', exact=True).set_input_files(source)
    related = dialog.get_by_label('Related files (optional)', exact=True)
    related.set_input_files(asset_copy)
    dialog.get_by_role('button', name='OK', exact=True).click()
    app.page.wait_for_function('document.querySelector("#source-snapshot-status") && !document.querySelector("#source-snapshot-status").hidden')
    draw_line(app)
    wait_saved(app.page)
    saved = records(app.page)
    if len(saved) != 1:
        raise AssertionError(f'expected one source-backed browser draft, got {saved}')
    details = stored_record_details(app.page, saved[0]['id'])
    expected_hash = hashlib.sha256(original_source).hexdigest()
    if (details is None or details['sourceHash'] != expected_hash or
            details['sourceFormat'] != 'tpx' or details['format'] != 'tpx-assets' or
            details['payloadMagic'] != 'TPXRA001' or details['unresolved'] != 0 or
            details['assets'] < 1):
        raise AssertionError(f'recovery did not retain the exact source and linked asset: {details}')

    app.restart()
    wait_ready(app.page)
    app.page.get_by_role('button', name='Recover as new drawing', exact=True).click()
    app.page.wait_for_function('document.querySelector("#tpx-recovery-state").textContent.includes("Draft recovered as a new TpX drawing")',
                               timeout=10000)
    status = app.page.locator('#document-status')
    if (status.get_attribute('data-state') != 'unsaved' or
            'original file is unchanged' not in status.inner_text()):
        raise AssertionError('recovered source snapshot was represented as synchronized with its original file')
    app.page.wait_for_function('''() => {
      const c = document.querySelector('#lcl');
      const pixels = c.getContext('2d').getImageData(0, 0, c.width, c.height).data;
      let magenta = 0;
      for (let i = 0; i < pixels.length; i += 4)
        if (pixels[i] > 245 && pixels[i+1] < 10 && pixels[i+2] > 245) magenta++;
      return magenta > 500;
    }''', timeout=10000)
    if app.page.evaluate("performance.getEntriesByType('resource').some(e=>e.name.endsWith('/tex-svg.bundle.js'))"):
        raise AssertionError('recovery granted preview trust to the uploaded TeX source')
    recovered_copy = app.save('recovered-source-copy.tpx')
    if source.read_bytes() != original_source or asset_copy.read_bytes() != original_asset:
        raise AssertionError('recovery or Download changed the uploaded source or linked asset')
    if recovered_copy.read_bytes() == original_source:
        raise AssertionError('Download did not produce a distinct recovered copy')
    return 'restart recovery retained exact source bytes and linked-image pixels; preview stayed untrusted and Download left uploaded files unchanged'


def s_recovery_quota_failure_keeps_previous_record(app):
    opt_in(app.page)
    add_records(app.page, [{'id': 'quota-baseline', 'metadata': {'title': 'kept'},
                            'payload': [1, 2, 3], 'byteLength': 3,
                            'updatedAt': 1, 'sequence': 1}])
    app.page.evaluate('''() => {
      const put = IDBObjectStore.prototype.put;
      IDBObjectStore.prototype.put = function(value, key) {
        if (this.name === 'drafts') throw new DOMException('Injected quota failure', 'QuotaExceededError');
        return put.call(this, value, key);
      };
    }''')
    draw_line(app)
    app.page.wait_for_function('document.querySelector("#tpx-recovery-state").textContent.includes("quota prevented")',
                               timeout=10000)
    saved = records(app.page)
    if [row['id'] for row in saved] != ['quota-baseline']:
        raise AssertionError(f'quota failure replaced or discarded an existing record: {saved}')
    return 'quota failure is reported and leaves the prior durable record unchanged'


def s_recovery_capacity_does_not_evict(app):
    opt_in(app.page)
    rows = [{'id': f'retained-{i}', 'metadata': {'title': f'Existing {i}', 'format': 'TpX'},
             'payload': [i], 'byteLength': 1, 'updatedAt': i, 'sequence': 1}
            for i in range(5)]
    add_records(app.page, rows)
    app.page.reload()
    wait_ready(app.page)
    app.page.wait_for_function('document.querySelectorAll("#tpx-recovery-drafts [data-restore]").length === 5')
    draw_line(app)
    app.page.wait_for_function('document.querySelector("#tpx-recovery-state").textContent.includes("storage is full")',
                               timeout=10000)
    saved = records(app.page)
    if {row['id'] for row in saved} != {f'retained-{i}' for i in range(5)}:
        raise AssertionError(f'capacity failure evicted or replaced an existing draft: {saved}')
    return 'the sixth draft is rejected with a full-store status and all five prior records remain'


def s_recovery_corrupt_record_is_retained_until_deleted(app):
    opt_in(app.page)
    add_records(app.page, [{'id': 'corrupt-test', 'metadata': {'title': 'Corrupt fixture', 'format': 'TpX'},
                            'payload': [88, 88, 88], 'byteLength': 3,
                            'updatedAt': 1, 'sequence': 1}])
    app.page.reload()
    wait_ready(app.page)
    app.page.get_by_role('button', name='Recover as new drawing', exact=True).click()
    app.page.wait_for_function('document.querySelector("#tpx-recovery-state").textContent.includes("corrupt")',
                               timeout=10000)
    if len(records(app.page)) != 1:
        raise AssertionError('a corrupt draft was deleted without user action')
    app.page.get_by_role('button', name='Delete draft', exact=True).click()
    app.page.get_by_role('button', name='Confirm delete', exact=True).click()
    app.page.wait_for_function('document.querySelectorAll("#tpx-recovery-drafts .draft").length === 0')
    if records(app.page):
        raise AssertionError('explicitly confirmed corrupt-draft deletion did not commit')
    return 'a corrupt record is rejected, retained for review, and deleted only after confirmation'


def s_recovery_opt_in_is_origin_scoped(app):
    opt_in(app.page)
    alternate = app.page.context.new_page()
    try:
        alternate.goto(app.page.url.replace('127.0.0.1', 'localhost'))
        wait_ready(alternate)
        if alternate.locator('#tpx-recovery-enabled').is_checked():
            raise AssertionError('opt-in from 127.0.0.1 leaked to the localhost origin')
    finally:
        alternate.close()
    return 'consent is isolated by browser origin'



def s_recovery_opt_out_keeps_drafts_and_ignores_stale_reads(app):
    opt_in(app.page)
    draw_line(app)
    wait_saved(app.page)
    before = records(app.page)
    if len(before) != 1:
        raise AssertionError(f'expected one draft before opting out, got {before}')
    app.page.wait_for_function('document.querySelectorAll("#tpx-recovery-drafts .draft").length === 1')

    app.page.evaluate('''() => {
      const originalGetAll = IDBObjectStore.prototype.getAll;
      window.tpxHeldDraftReads = [];
      window.tpxHoldDraftReads = true;
      window.tpxOriginalDraftGetAll = originalGetAll;
      IDBObjectStore.prototype.getAll = function(...args) {
        const request = originalGetAll.apply(this, args);
        if (this.name === 'drafts' && window.tpxHoldDraftReads) {
          request.addEventListener('success', event => {
            if (!window.tpxHoldDraftReads) return;
            event.stopImmediatePropagation();
            window.tpxHeldDraftReads.push(() => {
              if (typeof request.onsuccess === 'function') request.onsuccess.call(request, event);
            });
          }, {once: true});
        }
        return request;
      };
      const change = new BroadcastChannel('tpx-browser-local-recovery');
      change.postMessage({type: 'draft-change'});
      change.close();
    }''')
    app.page.wait_for_function('window.tpxHeldDraftReads.length === 1')
    app.page.locator('#tpx-recovery-enabled').uncheck()
    app.page.wait_for_function('!document.querySelector("#tpx-recovery-enabled").checked && document.querySelector("#tpx-recovery-state").textContent.startsWith("Off.") && document.querySelector("#tpx-recovery-drafts").children.length === 0')
    app.page.evaluate('''async () => {
      window.tpxHoldDraftReads = false;
      IDBObjectStore.prototype.getAll = window.tpxOriginalDraftGetAll;
      for (const resume of window.tpxHeldDraftReads) resume();
      await Promise.resolve();
      await Promise.resolve();
    }''')
    if app.page.locator('#tpx-recovery-drafts').locator('.draft').count() != 0:
        raise AssertionError('a read started before opting out repopulated the recovery list')
    after = records(app.page)
    if len(after) != 1 or after[0]['id'] != before[0]['id']:
        raise AssertionError('opting out deleted an existing browser recovery draft')

    opt_in(app.page)
    app.page.wait_for_function('document.querySelectorAll("#tpx-recovery-drafts .draft").length === 1')
    return 'opting out keeps the IndexedDB draft and a delayed pre-opt-out read cannot repopulate the panel'


def s_recovery_tabs_get_distinct_records(app):
    second = app.page.context.new_page()
    try:
        second.goto(app.page.url)
        wait_ready(second)
        opt_in(app.page)
        second.wait_for_function('document.querySelector("#tpx-recovery-enabled").checked')
        draw_line(app)
        wait_saved(app.page)
        second_app = App(second, 'two-tab', app.output)
        draw_line(second_app)
        second.wait_for_function('document.querySelector("#tpx-recovery-state").textContent.includes("Draft saved in this browser")',
                                 timeout=10000)
        saved = records(app.page)
        ids = [row['id'] for row in saved]
        if len(ids) != 2 or len(set(ids)) != 2:
            raise AssertionError(f'two tabs did not keep separate draft identities: {ids}')
        if second_app.errors:
            raise AssertionError(f'second tab reported a browser error: {second_app.errors[0]}')
    finally:
        second.close()
    return 'simultaneous tabs retain two independent records without stale overwrites'


SCENARIOS = [
    ('recovery-disabled', s_recovery_disabled_by_default),
    ('recovery-restart-geometry', s_recovery_survives_application_restart),
    ('recovery-noop-download', s_recovery_noop_download_keeps_previous_draft),
    ('recovery-restart-raw-label', s_recovery_restores_raw_inline_label_after_restart),
    ('recovery-restart-composition', s_recovery_skips_active_composition_and_restores_commit),
    ('recovery-source-assets', s_recovery_retains_source_and_linked_asset),
    ('recovery-quota', s_recovery_quota_failure_keeps_previous_record),
    ('recovery-capacity', s_recovery_capacity_does_not_evict),
    ('recovery-corrupt', s_recovery_corrupt_record_is_retained_until_deleted),
    ('recovery-origin', s_recovery_opt_in_is_origin_scoped),
    ('recovery-optout', s_recovery_opt_out_keeps_drafts_and_ignores_stale_reads),
    ('recovery-two-tabs', s_recovery_tabs_get_distinct_records),
]

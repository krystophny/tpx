// SPDX-License-Identifier: MIT

const DB_NAME = 'tpx-browser-local-recovery';
const DB_VERSION = 1;
const MAX_RECORDS = 5;
const MAX_RECORD_BYTES = 24 * 1024 * 1024;
const MAX_TOTAL_BYTES = 64 * 1024 * 1024;
const MAX_METADATA_BYTES = 4096;
const encoder = new TextEncoder();
const decoder = new TextDecoder();

const statusText = new Map([
  [1, 'Draft saved in this browser. Keep using Download for a separate copy.'],
  [2, 'Browser storage quota prevented this draft from being saved. Existing drafts were kept.'],
  [3, 'Browser draft storage is full. Delete a draft after reviewing it, then try again.'],
  [4, 'This draft is too large for browser-local recovery. Use Download to keep a copy.'],
  [5, 'The draft could not be stored. Existing drafts were kept.'],
  [6, 'This recovery record is corrupt and was left in storage. You can delete it below.'],
  [7, 'Start a clean, untitled drawing before recovery. The saved draft and original source were left unchanged.'],
  [8, 'Draft recovered as a new TpX drawing. The original source and downloads are unchanged.'],
  [9, 'Browser-local recovery is unavailable in this browser context. Use Download to keep a copy.'],
  [10, 'Text composition is still active. The previous saved draft was kept; finish composing to save an update.'],
  [11, 'Start a new drawing before recovering. The open source is left unchanged.'],
  [12, 'Start a new drawing before recovering. The current changes remain available.'],
  [13, 'A newer recovery record already exists for this session. It was kept.'],
  [14, 'This update was not saved because a linked image could not be captured. Earlier drafts remain available; use Download to keep a separate copy.'],
  [15, 'The drawing was recovered, but its pending editor could not be reopened. The saved draft remains available; start a new drawing before retrying.'],
  [16, 'New changes are not yet saved as a browser recovery draft. The previous draft remains available.'],
  [17, 'There are no unsaved changes. Any earlier browser recovery draft remains available.'],
]);

function openDatabase() {
  if (!('indexedDB' in globalThis)) return Promise.reject(new Error('IndexedDB is unavailable'));
  return new Promise((resolve, reject) => {
    let request;
    try { request = indexedDB.open(DB_NAME, DB_VERSION); }
    catch (error) { reject(error); return; }
    request.onupgradeneeded = () => {
      const db = request.result;
      if (!db.objectStoreNames.contains('drafts')) db.createObjectStore('drafts', {keyPath: 'id'});
      if (!db.objectStoreNames.contains('settings')) db.createObjectStore('settings', {keyPath: 'key'});
    };
    request.onsuccess = () => resolve(request.result);
    request.onerror = () => reject(request.error || new Error('Could not open browser storage'));
    request.onblocked = () => reject(new Error('Browser storage upgrade is blocked by another tab'));
  });
}

function storedBytes(record) {
  try {
    if (!record || !(record.payload instanceof ArrayBuffer) ||
        typeof record.id !== 'string' || typeof record.metadata !== 'object' ||
        record.metadata === null) return -1;
    return record.payload.byteLength +
      encoder.encode(JSON.stringify(record.metadata)).byteLength +
      encoder.encode(record.id).byteLength + 64;
  } catch (_) { return -1; }
}

function validStoredRecord(record) {
  const size = storedBytes(record);
  if (size <= 0 || size > MAX_RECORD_BYTES ||
      !/^[A-Za-z0-9_-]{1,80}$/.test(record.id) ||
      record.byteLength !== record.payload.byteLength || record.payload.byteLength <= 0 ||
      !Number.isSafeInteger(record.sequence) || record.sequence <= 0)
    return false;
  try { return encoder.encode(JSON.stringify(record.metadata)).byteLength <= MAX_METADATA_BYTES; }
  catch (_) { return false; }
}

function requestResult(request) {
  return new Promise((resolve, reject) => {
    request.onsuccess = () => resolve(request.result);
    request.onerror = () => reject(request.error || new Error('Browser storage request failed'));
  });
}

function transactionDone(transaction) {
  return new Promise((resolve, reject) => {
    transaction.oncomplete = () => resolve();
    transaction.onabort = transaction.onerror = () => reject(
      transaction.error || new Error('Browser storage transaction failed'));
  });
}

function makePanel() {
  let panel = document.getElementById('tpx-local-recovery');
  if (panel) return panel;
  const style = document.createElement('style');
  style.textContent = `
    #tpx-local-recovery{margin:12px 0;padding:10px 12px;border:1px solid #b9c9d9;border-radius:6px;background:#fff;color:#263a50;font:13px/1.45 system-ui,sans-serif}
    #tpx-local-recovery .head{display:flex;flex-wrap:wrap;justify-content:space-between;align-items:center;gap:8px 12px}
    #tpx-local-recovery .head strong{font-size:14px}#tpx-local-recovery label{white-space:nowrap}
    #tpx-local-recovery p{margin:5px 0;color:#4c5c6d}#tpx-local-recovery .state{font-weight:600}
    #tpx-local-recovery .draft{display:flex;align-items:center;justify-content:space-between;gap:12px;padding:6px 0;border-top:1px solid #e2e8ee}
    #tpx-local-recovery .actions{display:flex;gap:6px;flex-shrink:0}#tpx-local-recovery button{border:1px solid #8fa6bb;background:#f8fbff;border-radius:4px;padding:3px 8px;color:#203e69;cursor:pointer}
    #tpx-local-recovery button:disabled{cursor:not-allowed;opacity:.55}#tpx-local-recovery .muted{color:#657485}
  `;
  document.head.append(style);
  panel = document.createElement('section');
  panel.id = 'tpx-local-recovery';
  panel.setAttribute('aria-label', 'Browser-local draft recovery');
  panel.innerHTML = `<div class="head"><strong>Browser-local draft recovery</strong><label><input id="tpx-recovery-enabled" type="checkbox"> Enable</label></div>
    <p>When enabled, drafts stay in this browser profile for this site. Anyone using the profile may see them. Browser storage can be cleared or evicted, so keep using Download for a separate copy. Recovery creates a new TpX drawing; it does not update the original file or trust TeX.</p>
    <p id="tpx-recovery-state" class="state" role="status">Off. No draft is stored until you enable recovery.</p><div id="tpx-recovery-drafts"></div>`;
  const anchor = document.querySelector('main') || document.body;
  anchor.append(panel);
  return panel;
}

export function localRecoveryImports(runtime) {
  const panel = makePanel();
  const enabledControl = panel.querySelector('#tpx-recovery-enabled');
  const state = panel.querySelector('#tpx-recovery-state');
  const list = panel.querySelector('#tpx-recovery-drafts');
  let enabled = false;
  let wasmReady = false;
  let sourceOpen = false;
  let dirty = false;
  let pending = null;
  let refreshGeneration = 0;
  const sequences = new Map();
  const channel = typeof BroadcastChannel === 'function' ?
    new BroadcastChannel('tpx-browser-local-recovery') : null;

  if (channel) channel.onmessage = event => {
    if (event.data?.type === 'consent') {
      enabled = event.data.enabled === true;
      enabledControl.checked = enabled;
      notifyEnabled();
      if (enabled) refresh();
      else { refresh(); show('Off. Existing drafts were kept; enable recovery to review or delete them.'); }
      updateRestoreButtons();
    } else if (event.data?.type === 'draft-change' && enabled) refresh();
  };

  function call(name, ...args) {
    try { runtime.call(name, ...args); } catch (_) { /* wasm is not ready yet */ }
  }

  function notifyEnabled() {
    if (wasmReady) call('tpx_browser_recovery_enabled', Number(enabled));
  }

  function show(text) { state.textContent = text; }

  function updateRestoreButtons() {
    const blocked = sourceOpen || dirty || !wasmReady || pending !== null;
    for (const button of list.querySelectorAll('[data-restore]'))
      button.disabled = blocked || button.dataset.corrupt === 'true';
  }

  function bytesFromWasm(pointer, length) {
    if (!Number.isSafeInteger(length) || length < 0 || length > MAX_RECORD_BYTES)
      throw new Error('Invalid browser recovery buffer');
    return new Uint8Array(runtime.memory().buffer, pointer >>> 0, length).slice();
  }

  async function getAllDrafts() {
    const db = await openDatabase();
    try {
      const tx = db.transaction('drafts', 'readonly');
      return await requestResult(tx.objectStore('drafts').getAll());
    } finally { db.close(); }
  }

  function addDraft(record) {
    const row = document.createElement('div');
    row.className = 'draft';
    const summary = document.createElement('span');
    const title = String(record.metadata?.title || 'Untitled drawing').slice(0, 200);
    const format = String(record.metadata?.format || 'TpX').slice(0, 24);
    summary.textContent = `${title} · ${format} · ${new Date(record.updatedAt).toLocaleString()}`;
    const actions = document.createElement('span'); actions.className = 'actions';
    const restore = document.createElement('button'); restore.textContent = 'Recover as new drawing';
    restore.dataset.restore = record.id;
    restore.addEventListener('click', () => requestRecovery(record.id, restore));
    const remove = document.createElement('button'); remove.textContent = 'Delete draft';
    remove.addEventListener('click', () => {
      if (remove.dataset.confirm !== 'yes') {
        remove.dataset.confirm = 'yes'; remove.textContent = 'Confirm delete'; return;
      }
      remove.disabled = true;
      deleteDraft(record.id).then(() => {
        if (channel) channel.postMessage({type: 'draft-change'});
        show('Draft deleted from this browser.'); refresh();
      }).catch(() => { remove.disabled = false; show(statusText.get(5)); });
    });
    actions.append(restore, remove); row.append(summary, actions); list.append(row);
  }

  async function refresh() {
    const generation = ++refreshGeneration;
    list.replaceChildren();
    if (!enabled) return;
    try {
      const rows = await getAllDrafts();
      if (!enabled || generation !== refreshGeneration) return;
      rows.sort((a, b) => (b.updatedAt || 0) - (a.updatedAt || 0));
      for (const record of rows) {
        if (!validStoredRecord(record)) {
          if (record && typeof record.id === 'string' && record.id) {
            addDraft({id: record.id, updatedAt: record.updatedAt || Date.now(),
              metadata: {title: 'Unreadable recovery record', format: 'corrupt'}});
            const button = list.lastElementChild.querySelector('[data-restore]');
            button.dataset.corrupt = 'true';
          } else {
            const warning = document.createElement('p');
            warning.className = 'muted';
            warning.textContent = 'An unreadable record has no safe delete key. It was left untouched.';
            list.append(warning);
          }
          show(statusText.get(6));
          continue;
        }
        addDraft(record);
      }
      if (rows.length === 0) show('Enabled. No recoverable drafts are stored for this site.');
      updateRestoreButtons();
    } catch (_) { show(statusText.get(9)); }
  }

  async function requestRecovery(id, button) {
    if (!enabled || sourceOpen || dirty || pending) { show(statusText.get(7)); return; }
    button.disabled = true;
    try {
      const db = await openDatabase();
      let record;
      try {
        const tx = db.transaction('drafts', 'readonly');
        record = await requestResult(tx.objectStore('drafts').get(id));
      } finally { db.close(); }
      if (!validStoredRecord(record) || record.id !== id) {
        show(statusText.get(6)); return;
      }
      const metadata = encoder.encode(id);
      if (metadata.byteLength > MAX_METADATA_BYTES) { show(statusText.get(6)); return; }
      pending = {id, payload: new Uint8Array(record.payload), metadata};
      show('Checking the selected draft before recovery…');
      call('tpx_browser_recovery_size_ready', pending.payload.byteLength, metadata.byteLength);
    } catch (_) { show(statusText.get(5)); }
    finally { updateRestoreButtons(); }
  }

  async function deleteDraft(id) {
    const db = await openDatabase();
    try {
      const tx = db.transaction('drafts', 'readwrite');
      tx.objectStore('drafts').delete(id);
      await transactionDone(tx);
    } finally { db.close(); }
  }

  async function setConsent(value) {
    enabledControl.disabled = true;
    try {
      const db = await openDatabase();
      try {
        const tx = db.transaction('settings', 'readwrite');
        tx.objectStore('settings').put({key: 'recovery-opt-in', enabled: value});
        await transactionDone(tx);
      } finally { db.close(); }
      enabled = value;
      enabledControl.checked = enabled;
      if (channel) channel.postMessage({type: 'consent', enabled});
      show(enabled ? 'Enabled. Browser storage may still be cleared or evicted.' :
        'Off. Existing drafts were kept; enable recovery to review or delete them.');
      notifyEnabled();
      await refresh();
    } catch (_) {
      enabledControl.checked = enabled;
      show(statusText.get(9));
      notifyEnabled();
    } finally { enabledControl.disabled = false; updateRestoreButtons(); }
  }

  enabledControl.addEventListener('change', () => setConsent(enabledControl.checked));

  async function initialize() {
    enabledControl.disabled = true;
    try {
      const db = await openDatabase();
      let setting;
      try {
        const tx = db.transaction('settings', 'readonly');
        setting = await requestResult(tx.objectStore('settings').get('recovery-opt-in'));
      } finally { db.close(); }
      enabled = setting?.enabled === true;
      enabledControl.checked = enabled;
      show(enabled ? 'Enabled. Browser storage may still be cleared or evicted.' :
        'Off. No draft is stored until you enable recovery.');
      if (enabled) await refresh();
      enabledControl.disabled = false;
      notifyEnabled();
    } catch (_) { enabledControl.disabled = true; show(statusText.get(9)); notifyEnabled(); }
  }

  function put(idPtr, idLength, metadataPtr, metadataLength, payloadPtr, payloadLength) {
    if (!enabled) return 0;
    try {
      if (idLength <= 0 || idLength > 80 || metadataLength <= 0 ||
          metadataLength > MAX_METADATA_BYTES) return 0;
      const memory = runtime.memory().buffer;
      const id = decoder.decode(new Uint8Array(memory, idPtr >>> 0, idLength));
      const metadata = JSON.parse(decoder.decode(new Uint8Array(memory, metadataPtr >>> 0, metadataLength)));
      const payload = bytesFromWasm(payloadPtr, payloadLength);
      const metadataBytes = encoder.encode(JSON.stringify(metadata));
      if (!/^[A-Za-z0-9_-]{1,80}$/.test(id) || payload.byteLength <= 0 ||
          metadataBytes.byteLength > MAX_METADATA_BYTES ||
          payload.byteLength + metadataBytes.byteLength + encoder.encode(id).byteLength + 64 > MAX_RECORD_BYTES) {
        show(statusText.get(4)); return 0;
      }
      const sequence = (sequences.get(id) || 0) + 1;
      sequences.set(id, sequence);
      const record = {id, metadata, payload: payload.buffer, byteLength: payload.byteLength,
        updatedAt: Date.now(), sequence};
      writeDraft(record).then(written => {
        if (written) {
          show(statusText.get(1));
          if (channel) channel.postMessage({type: 'draft-change'});
        } else show(statusText.get(13));
        refresh();
      }).catch(error => {
        show(error?.code === 'corrupt' ? statusText.get(6) :
          (error?.code === 'capacity' ? statusText.get(3) :
          (error?.name === 'QuotaExceededError' ? statusText.get(2) : statusText.get(5))));
      });
      return 1;
    } catch (_) { show(statusText.get(5)); return 0; }
  }

  function writeDraft(record) {
    return openDatabase().then(db => new Promise((resolve, reject) => {
      let capacityError = false, corruptError = false, stale = false, writeError = null;
      const tx = db.transaction('drafts', 'readwrite');
      const store = tx.objectStore('drafts');
      const request = store.getAll();
      request.onerror = () => { try { tx.abort(); } catch (_) {} };
      request.onsuccess = () => {
        const rows = request.result || [];
        if (rows.some(row => !validStoredRecord(row))) {
          corruptError = true; try { tx.abort(); } catch (_) {} return;
        }
        const old = rows.find(row => row.id === record.id);
        if (old && (old.sequence || 0) >= record.sequence) { stale = true; return; }
        if (old && !validStoredRecord(old)) { corruptError = true; try { tx.abort(); } catch (_) {} return; }
        const count = rows.length + (old ? 0 : 1);
        const total = rows.reduce((sum, row) => sum + Math.max(0, storedBytes(row)), 0)
          - Math.max(0, storedBytes(old)) + storedBytes(record);
        if (!validStoredRecord(record) || count > MAX_RECORDS || total > MAX_TOTAL_BYTES) {
          capacityError = true; try { tx.abort(); } catch (_) {} return;
        }
        try { store.put(record); }
        catch (error) { writeError = error; try { tx.abort(); } catch (_) {} }
      };
      tx.oncomplete = () => { db.close(); resolve(!stale); };
      tx.onabort = tx.onerror = () => {
        db.close();
        const error = corruptError ? Object.assign(new Error('Existing recovery record is corrupt'), {code: 'corrupt'}) :
          (capacityError ? Object.assign(new Error('Draft storage is full'), {code: 'capacity'}) :
          writeError || tx.error || new Error('Could not store recovery draft'));
        reject(error);
      };
    }));
  }

  function copy(payloadPointer, payloadCapacity, metadataPointer, metadataCapacity) {
    if (!pending || payloadCapacity < pending.payload.byteLength ||
        metadataCapacity < pending.metadata.byteLength) return 0;
    try {
      const memory = new Uint8Array(runtime.memory().buffer);
      memory.set(pending.payload, payloadPointer >>> 0);
      memory.set(pending.metadata, metadataPointer >>> 0);
      return 1;
    } catch (_) { return 0; }
  }

  function newId(destinationPointer, capacity) {
    if (!globalThis.crypto?.getRandomValues || capacity < 36) return 0;
    const random = new Uint8Array(16); globalThis.crypto.getRandomValues(random);
    random[6] = (random[6] & 0x0f) | 0x40; random[8] = (random[8] & 0x3f) | 0x80;
    const hex = [...random].map(value => value.toString(16).padStart(2, '0')).join('');
    const id = `${hex.slice(0,8)}-${hex.slice(8,12)}-${hex.slice(12,16)}-${hex.slice(16,20)}-${hex.slice(20)}`;
    new Uint8Array(runtime.memory().buffer).set(encoder.encode(id), destinationPointer >>> 0);
    return 36;
  }

  function showStatus(code) { show(statusText.get(code) || statusText.get(5)); }

  initialize();
  return {tpx: {
    recovery_initialized: () => { wasmReady = true; notifyEnabled(); },
    recovery_new_id: newId,
    recovery_put: put,
    recovery_copy: copy,
    recovery_refresh: refresh,
    recovery_document_state: (source, isDirty) => {
      sourceOpen = Boolean(source); dirty = Boolean(isDirty); updateRestoreButtons();
    },
    recovery_show_status: showStatus,
    recovery_restored: (success, code) => {
      pending = null;
      show(success ? statusText.get(8) : (statusText.get(code) || statusText.get(5)));
      refresh();
    }
  }};
}

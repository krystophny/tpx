// SPDX-License-Identifier: MIT
import { WASI, File, OpenFile, ConsoleStdout, PreopenDirectory } from '@bjorn3/browser_wasi_shim';

const canvas = document.querySelector('#lcl');
const context = canvas.getContext('2d', {alpha: false});
const status = document.querySelector('#status');
const textCanvas = document.createElement('canvas');
const textContext = textCanvas.getContext('2d', {willReadFrequently: true});
const decoder = new TextDecoder();
let instance, ready = false, scheduled = false, image = null, pendingMove = null;
const text = (pointer, length) => decoder.decode(new Uint8Array(instance.exports.memory.buffer, pointer, length));
function fail(error) {
  ready = false;
  // Name the innermost Pascal routines so a report locates the failure.
  const frames = String(error.stack || '').match(/at [A-Z0-9_$]+\$\$?_?[A-Z0-9_$]*/g) || [];
  const where = frames.slice(0, 6).map(frame => frame.slice(3)).join(' ← ');
  status.textContent = `Application error: ${error.message || error}${where ? ` in ${where}` : ''}`;
  status.dataset.state = 'error';
  console.error(error);
}
function guarded(callback) {
  try { callback(); } catch (error) { fail(error); }
}
function invalidate() {
  if (scheduled) return;
  scheduled = true;
  requestAnimationFrame(() => {
    scheduled = false;
    flushMove();
    if (ready) guarded(() => instance.exports.lcl_render());
  });
}
const imports = {
  invalidate,
  title: (pointer, length) => { document.title = text(pointer, length); },
  timer: (handle, interval) => setInterval(() => {
    if (ready) guarded(() => instance.exports.lcl_timer(handle));
  }, interval),
  clear_timer: handle => clearInterval(handle),
  measure: (pointer, length, size) => {
    textContext.font = `${size}px sans-serif`;
    return Math.ceil(textContext.measureText(text(pointer, length)).width);
  },
  text: (pointer, width, height, x, y, string, length, size, color, left, top, right, bottom) => {
    textCanvas.width = width;
    textCanvas.height = height;
    textContext.font = `${size}px sans-serif`;
    textContext.textBaseline = 'top';
    textContext.fillStyle = `#${(color >>> 0).toString(16).padStart(6, '0')}`;
    textContext.beginPath();
    textContext.rect(left, top, right-left, bottom-top);
    textContext.clip();
    textContext.fillText(text(string, length), x, y);
    const layer = textContext.getImageData(0, 0, width, height).data;
    const pixels = new Uint8Array(instance.exports.memory.buffer, pointer, width*height*4);
    for (let i=0; i<layer.length; i+=4) {
      const alpha = layer[i+3] / 255;
      if (!alpha) continue;
      pixels[i] = 255; // LazCanvas clfARGB32 stores bytes A, R, G, B.
      pixels[i+1] = Math.round(layer[i]*alpha + pixels[i+1]*(1-alpha));
      pixels[i+2] = Math.round(layer[i+1]*alpha + pixels[i+2]*(1-alpha));
      pixels[i+3] = Math.round(layer[i+2]*alpha + pixels[i+3]*(1-alpha));
    }
  },
  present: (pointer, width, height) => {
    if (canvas.width !== width) canvas.width = width;
    if (canvas.height !== height) canvas.height = height;
    // The surface is sized by CSS; the canvas keeps the form's pixel size and
    // scales down to fit, so the UI never overflows the browser window.
    // LazCanvas clfARGB32 bytes A, R, G, B become canvas bytes R, G, B, A:
    // one shift per little-endian word into a reused buffer.
    if (!image || image.width !== width || image.height !== height) image = context.createImageData(width, height);
    const source = new Uint32Array(instance.exports.memory.buffer, pointer, width*height);
    const target = new Uint32Array(image.data.buffer);
    for (let i=0; i<source.length; i++) target[i] = (source[i] >>> 8) | 0xFF000000;
    context.putImageData(image, 0, 0);
    canvas.dataset.frames = String(Number(canvas.dataset.frames || 0)+1);
  }
};
function pointer(kind, x, y, button, modifiers) {
  if (ready) guarded(() => instance.exports.lcl_pointer(kind, x, y, button, modifiers));
}
// Moves are coalesced to one per frame; a pending move is delivered first so
// presses and releases keep their order.
function flushMove() {
  if (!pendingMove) return;
  const move = pendingMove;
  pendingMove = null;
  pointer(2, ...move);
}
for (const [name, kind] of [['pointerdown', 0], ['pointerup', 1], ['pointermove', 2]]) {
  canvas.addEventListener(name, event => {
    if (!ready) return;
    event.preventDefault();
    if (kind === 0) { canvas.focus({preventScroll: true}); canvas.setPointerCapture(event.pointerId); }
    const bounds = canvas.getBoundingClientRect();
    const x = Math.round((event.clientX-bounds.left)*canvas.width/bounds.width);
    const y = Math.round((event.clientY-bounds.top)*canvas.height/bounds.height);
    const modifiers = Number(event.shiftKey) | Number(event.ctrlKey)<<1 | Number(event.altKey)<<2 | Number(event.buttons&1)<<3;
    if (kind === 2) {
      pendingMove = [x, y, event.button, modifiers];
      invalidate();
      return;
    }
    flushMove();
    pointer(kind, x, y, event.button, modifiers);
    if (kind === 1 && canvas.hasPointerCapture(event.pointerId)) canvas.releasePointerCapture(event.pointerId);
  });
}
canvas.addEventListener('contextmenu', event => event.preventDefault());

// Keys: Windows virtual key codes for the keys LCL routes, Unicode scalar for
// text input. Modifiers follow the pointer encoding: shift 1, ctrl 2, alt 4.
const KEYS = {Backspace: 8, Tab: 9, Enter: 13, Shift: 16, Control: 17, Alt: 18,
  Escape: 27, PageUp: 33, PageDown: 34, End: 35, Home: 36, Insert: 45, Delete: 46,
  ArrowLeft: 37, ArrowUp: 38, ArrowRight: 39, ArrowDown: 40, Meta: 91};
const PREVENT = new Set(['Tab', 'Escape', 'Enter', 'Backspace', 'Delete', 'ArrowLeft',
  'ArrowUp', 'ArrowRight', 'ArrowDown', 'PageUp', 'PageDown', 'F1', 'F2', 'F3', 'F4',
  'F5', 'F6', 'F7', 'F8', 'F9', 'F10', 'F11', 'F12']);
function vk(key) {
  if (key in KEYS) return KEYS[key];
  if (key.length === 1) return key.toUpperCase().charCodeAt(0);
  if (/^F([1-9]|1[0-2])$/.test(key)) return 111 + Number(key.slice(1));
  return 0;
}
const modifiers = event => Number(event.shiftKey) | Number(event.ctrlKey) << 1 |
  Number(event.altKey) << 2 | Number(event.buttons & 1) << 3;
function key(kind, event) {
  if (!ready) return;
  if (PREVENT.has(event.key)) event.preventDefault();
  const code = vk(event.key);
  guarded(() => kind === 2
    ? instance.exports.lcl_key(2, code, event.key.codePointAt(0), modifiers(event))
    : instance.exports.lcl_key(kind, code, 0, modifiers(event)));
}
document.addEventListener('keydown', event => { key(0, event); if (event.key.length === 1 && !event.ctrlKey && !event.altKey) key(2, event); });
document.addEventListener('keyup', event => key(1, event));
canvas.addEventListener('wheel', event => {
  if (!ready) return;
  event.preventDefault();
  const b = canvas.getBoundingClientRect();
  const x = Math.round((event.clientX-b.left)*canvas.width/b.width);
  const y = Math.round((event.clientY-b.top)*canvas.height/b.height);
  // LCL wants Windows wheel ticks (120 per notch, positive scrolls up).
  const delta = Math.sign(-event.deltaY) * 120;
  guarded(() => instance.exports.lcl_wheel(x, y, delta, modifiers(event)));
}, {passive: false});

// The form follows the window instead of the desktop size it was saved with.
function surfaceBox() {
  const surface = document.querySelector('#lcl-surface');
  const left = surface.getBoundingClientRect().left;
  return [Math.max(320, Math.floor(document.documentElement.clientWidth - left*2)),
          Math.max(240, Math.floor(window.innerHeight - surface.getBoundingClientRect().top - 16))];
}
function resize() {
  if (!ready) return;
  const [width, height] = surfaceBox();
  guarded(() => instance.exports.lcl_resize(width, height));
}
// Native widgetsets receive WM_MOUSELEAVE; the browser must say so as well, or
// a crosshair painted on the paper is never erased.
canvas.addEventListener('pointerleave', () => {
  if (ready) guarded(() => instance.exports.lcl_leave());
});
canvas.addEventListener('pointercancel', () => {
  if (ready) guarded(() => instance.exports.lcl_pointer(1, 0, 0, 0, 0));
});

window.addEventListener('resize', () => requestAnimationFrame(resize));
async function start() {
  const wasi = new WASI(['tpx'], [], [
    new OpenFile(new File([])),
    ConsoleStdout.lineBuffered(line => console.log(`[Pascal] ${line}`)),
    ConsoleStdout.lineBuffered(line => console.error(`[Pascal] ${line}`)),
    new PreopenDirectory('/', [])
  ]);
  // FPC's generic WASI RTL imports these services even though this demo does not use them.
  const wasiImports = {...wasi.wasiImport};
  wasiImports.random_get = (pointer, length) => {
    const bytes = new Uint8Array(instance.exports.memory.buffer, pointer, length);
    for (let offset=0; offset<length; offset+=65536) crypto.getRandomValues(bytes.subarray(offset, offset+65536));
    return 0;
  };
  wasiImports.clock_time_get = (id, precision, pointer) => {
    const nanoseconds = BigInt(Math.floor((id === 0 ? Date.now() : performance.now())*1e6));
    new DataView(instance.exports.memory.buffer).setBigUint64(pointer, nanoseconds, true);
    return 0;
  };
  const response = await fetch('./tpx.wasm');
  if (!response.ok) throw new Error(`WASM download failed (${response.status})`);
  const module = await WebAssembly.compile(await response.arrayBuffer());
  const missing = WebAssembly.Module.imports(module).filter(i => i.module === 'wasi_snapshot_preview1' && !(i.name in wasiImports));
  if (missing.length) throw new Error(`Missing WASI services: ${missing.map(i=>i.name).join(', ')}`);
  instance = await WebAssembly.instantiate(module, {wasi_snapshot_preview1: wasiImports, job: jobHost.imports, lcl: imports});
  jobHost.connect(instance);
  wasi.initialize(instance);
  ready = true;
  window.lclDemo = instance.exports;
  status.textContent = 'Running · Pascal + LCL in WebAssembly';
  status.dataset.state = 'ready';
  resize();
  invalidate();
}
start().catch(fail);

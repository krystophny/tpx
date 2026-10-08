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
    document.querySelector("#lcl-surface").style.width = `${width}px`;
    document.querySelector("#lcl-surface").style.height = `${height}px`;
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
  invalidate();
}
start().catch(fail);

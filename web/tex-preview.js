// SPDX-License-Identifier: MIT
const decoder = new TextDecoder();

export function texPreviewImports(runtime) {
  const sources = new Map(), rasters = new Map(), objects = new Map();
  const pending = new Set();
  const layer = document.createElement('canvas');
  const context = layer.getContext('2d', {willReadFrequently: true});
  let loader, timer, enabled = true, running = false, rasterBytes = 0, sourceBytes = 0;
  const touch = (map, key, value) => { map.delete(key); map.set(key, value); return value; };

  function trimRasters(keep, needed = 0) {
    // Object references must not retain images after cache eviction.
    const countLimit = Math.max(256, objects.size * 2);
    if (rasterBytes + needed <= 16 * 1024 * 1024 && rasters.size <= countLimit) return;
    const current = new Set([...objects.values()].map(object => object.request));
    for (const [key, old] of rasters) {
      if (rasterBytes + needed <= 16 * 1024 * 1024 && rasters.size <= countLimit) break;
      // Evicting a visible formula would schedule it again on the completion
      // repaint, creating an endless typeset/repaint loop in large drawings.
      if (old === keep || old.loading || current.has(old)) continue;
      rasters.delete(key); pending.delete(old);
      if (old.image) {
        rasterBytes -= old.width * old.height * 4;
        old.image = null;
        for (const object of objects.values()) if (object.valid === old) object.valid = null;
      }
    }
  }

  function notice(error) {
    let element = document.querySelector('#notice');
    if (!element) {
      element = document.createElement('aside'); element.id = 'notice';
      element.setAttribute('role', 'status');
      const message = document.createElement('span');
      const close = document.createElement('button'); close.textContent = 'Dismiss';
      close.onclick = () => { element.hidden = true; };
      element.append(message, close); document.body.append(element);
    }
    element.firstElementChild.textContent = `TeX preview: ${error.message || error}. Showing the last valid preview or text.`;
    element.hidden = false;
  }

  async function rasterize(entry) {
    let svg = sources.get(entry.source);
    if (!svg) {
      loader ||= import('./tex-svg.bundle.js');
      svg = (await loader).texSVG(entry.source);
      sourceBytes += svg.xml.length * 2;
      sources.set(entry.source, svg);
      while (sourceBytes > 8 * 1024 * 1024 || sources.size > 128) {
        const [key, old] = sources.entries().next().value;
        sourceBytes -= old.xml.length * 2; sources.delete(key);
      }
    } else touch(sources, entry.source, svg);
    const width = Math.ceil(svg.width * entry.size), height = Math.ceil(svg.height * entry.size);
    if (width > 4096 || height > 4096 || width * height > 4 * 1024 * 1024)
      throw new Error('TeX text is too large at this zoom');
    trimRasters(entry, width * height * 4);
    if (rasterBytes + width * height * 4 > 16 * 1024 * 1024)
      throw Object.assign(new Error('TeX preview cache is full'), {capacity: true});
    const node = new DOMParser().parseFromString(svg.xml, 'image/svg+xml').documentElement;
    node.setAttribute('width', width); node.setAttribute('height', height);
    node.style.color = `#${entry.color.toString(16).padStart(6, '0')}`;
    const url = URL.createObjectURL(new Blob([new XMLSerializer().serializeToString(node)], {type: 'image/svg+xml'}));
    const image = new Image();
    try {
      await new Promise((resolve, reject) => {
        image.onload = resolve; image.onerror = () => reject(new Error('Cannot render TeX image'));
        image.src = url;
      });
    } finally { URL.revokeObjectURL(url); }
    // Retain pixels, not an SVG image tree that can be rasterized again on
    // every pointer frame. The cache budget now describes its backing stores.
    const raster = document.createElement('canvas');
    raster.width = width; raster.height = height;
    raster.getContext('2d').drawImage(image, 0, 0, width, height);
    entry.image = raster; entry.width = width; entry.height = height;
    entry.baseline = svg.baseline * entry.size;
    rasterBytes += width * height * 4;
    trimRasters(entry);
  }

  async function processQueue() {
    timer = null;
    if (running || !enabled) return;
    running = true;
    let changed = false;
    try {
      while (pending.size && enabled) {
        const entry = pending.values().next().value; pending.delete(entry);
        // Editing may have replaced all users of this source during debounce.
        if (![...objects.values()].some(object => object.request === entry)) continue;
        entry.loading = true;
        try { await rasterize(entry); }
        catch (error) {
          entry.error = true; entry.capacityBlocked = error.capacity;
          if (enabled) notice(error);
        }
        finally { entry.loading = false; }
        changed = true;
      }
    } finally {
      running = false;
      if (changed && enabled) runtime.call('tpx_tex_ready');
    }
  }

  function draw(pointer, width, height, key, x, y, size, rotation, sourcePtr, sourceLen,
                horizontal, vertical, color, left, top, right, bottom) {
    if (!enabled || size <= 0) return 0;
    let object = objects.get(key);
    const source = decoder.decode(new Uint8Array(runtime.memory().buffer, sourcePtr, sourceLen));
    size = Math.max(1, Math.round(size * 4) / 4);
    const cacheKey = JSON.stringify([source, size, color]);
    let entry = rasters.get(cacheKey);
    if (!entry) {
      entry = {source, size, color}; rasters.set(cacheKey, entry);
    }
    touch(rasters, cacheKey, entry);
    if (!object) object = {};
    object.request = entry;
    if (entry.image) object.valid = entry;
    touch(objects, key, object);
    trimRasters(entry);
    if (!entry.image && !entry.error && !entry.loading && !pending.has(entry)) {
      pending.add(entry);
      if (!timer && !running) timer = setTimeout(processQueue, 120);
    }
    entry = entry.image ? entry : object.valid;
    if (!entry) return 0;
    const scale = size / entry.size;
    const w = entry.width * scale, h = entry.height * scale;
    const dx = horizontal === 1 ? -w/2 : horizontal === 2 ? -w : 0;
    const dy = vertical === 0 ? -entry.baseline * scale : vertical === 1 ? -h : vertical === 2 ? -h/2 : 0;
    const c = Math.cos(rotation), s = -Math.sin(rotation);
    const corners = [[dx,dy],[dx+w,dy],[dx,dy+h],[dx+w,dy+h]]
      .map(([px,py]) => [x+px*c-py*s, y+px*s+py*c]);
    const x0 = Math.max(0, left, Math.floor(Math.min(...corners.map(p=>p[0])))-1);
    const y0 = Math.max(0, top, Math.floor(Math.min(...corners.map(p=>p[1])))-1);
    const x1 = Math.min(width, right, Math.ceil(Math.max(...corners.map(p=>p[0])))+1);
    const y1 = Math.min(height, bottom, Math.ceil(Math.max(...corners.map(p=>p[1])))+1);
    if (x1 <= x0 || y1 <= y0) return 1;
    const lw = x1-x0, lh = y1-y0;
    if (layer.width !== lw) layer.width = lw;
    if (layer.height !== lh) layer.height = lh;
    context.clearRect(0, 0, lw, lh);
    context.save(); context.translate(x-x0, y-y0); context.rotate(-rotation);
    context.drawImage(entry.image, dx, dy, w, h); context.restore();
    const pixels = context.getImageData(0, 0, lw, lh).data;
    const target = new Uint8Array(runtime.memory().buffer, pointer, width*height*4);
    for (let row=0; row<lh; row++) for (let column=0; column<lw; column++) {
      const i = (row*lw+column)*4, alpha = pixels[i+3]/255;
      if (!alpha) continue;
      const dest = ((row+y0)*width+column+x0)*4;
      target[dest] = 255;
      for (let component=0; component<3; component++)
        target[dest+component+1] = Math.round(pixels[i+component]*alpha+target[dest+component+1]*(1-alpha));
    }
    return 1;
  }

  return {tpx: {
    tex_draw: draw,
    tex_forget: key => {
      objects.delete(key); trimRasters();
      for (const entry of rasters.values()) if (entry.capacityBlocked) entry.error = false;
    },
    tex_enable: value => {
      enabled = Boolean(value);
      if (!enabled) { clearTimeout(timer); timer = null; pending.clear(); }
    }
  }};
}

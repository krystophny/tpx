// Browser copies live only in the virtual filesystem until the user downloads them.
// A TpX save downloads the drawing together with its linked assets. Pascal
// reads each bundle member from the virtual filesystem; the downloads start
// only after every member arrived, so a failed save downloads nothing.
const DOWNLOAD_TYPES = {tpx: 'application/xml', xml: 'application/xml', svg: 'image/svg+xml',
  eps: 'application/postscript', ps: 'application/postscript', pdf: 'application/pdf',
  png: 'image/png', jpg: 'image/jpeg', jpeg: 'image/jpeg', gif: 'image/gif', bmp: 'image/bmp',
  tif: 'image/tiff', tiff: 'image/tiff', webp: 'image/webp', tex: 'text/plain',
  mp: 'text/plain', tikz: 'text/plain'};
let downloadBundle = null;
// The widgetset host downloads files written to names chosen in a save
// dialog. While a bundle is open, TpX starts those downloads itself, so the
// anchors the host creates meanwhile only release their blob when clicked.
const createElement = document.createElement;
document.createElement = function (tagName, ...options) {
  const element = createElement.call(this, tagName, ...options);
  if (downloadBundle && element instanceof HTMLAnchorElement)
    Object.defineProperty(element, 'click', {configurable: true, value() {
      if (this.href.startsWith('blob:')) URL.revokeObjectURL(this.href);
    }});
  return element;
};
const bundleName = name => name.replace(/^\/+/, '');
const validBundleName = name => name !== '' && !name.includes('/') && !name.includes('\\');

function downloadFile(name, bytes) {
  let url = '', link;
  try {
    const type = DOWNLOAD_TYPES[name.split('.').pop().toLowerCase()] || 'application/octet-stream';
    url = URL.createObjectURL(new Blob([bytes], {type}));
    link = document.createElement('a');
    link.href = url; link.download = name;
    document.body.append(link);
    link.click();
  } finally {
    link?.remove();
    if (url) setTimeout(() => URL.revokeObjectURL(url), 1000);
  }
}

function beginBundle(name) {
  name = bundleName(name);
  if (downloadBundle || !validBundleName(name)) return false;
  downloadBundle = {names: [name], files: new Map()};
  return true;
}

function addBundlePath(name) {
  name = bundleName(name);
  if (!downloadBundle || !validBundleName(name)) return false;
  if (!downloadBundle.names.includes(name)) downloadBundle.names.push(name);
  return true;
}

function finishBundle(success) {
  const bundle = downloadBundle;
  downloadBundle = null;
  if (!bundle || !success || bundle.names.some(name => !bundle.files.has(name))) return false;
  try {
    for (const name of bundle.names) downloadFile(name, bundle.files.get(name));
    return true;
  } catch (_) { return false; }
}

export function documentStatusImports(runtime) {
  const status = document.querySelector('#document-status');
  const decoder = new TextDecoder();
  let hasSource = false, dirty = false, downloadUnconfirmed = false;
  let downloadState = '', errorMessage = '';
  const read = (pointer, length) => decoder.decode(
    new Uint8Array(runtime.memory().buffer, pointer, length));

  function refresh() {
    status.dataset.state = dirty ? 'unsaved' : downloadUnconfirmed ? 'unconfirmed' :
      downloadState === 'failed' ? 'error' : 'clean';
    if (dirty) {
      status.textContent = errorMessage
        ? `${errorMessage} Your changes remain unsaved. The browser may suppress leave-page warnings.`
        : downloadState === 'failed'
        ? 'The download could not be started. Your changes remain unsaved; try Download a copy again. The browser may suppress leave-page warnings.'
        : downloadState === 'started'
        ? 'A copy download was requested, but this browser document still has unsaved changes. The browser may suppress leave-page warnings.'
        : 'Unsaved changes in this browser document. Download a copy to keep them; the original file is unchanged. The browser may suppress leave-page warnings.';
    } else if (downloadUnconfirmed && errorMessage) {
      status.textContent = `${errorMessage} An earlier download request is unconfirmed; the browser cannot confirm it was saved.`;
    } else if (downloadUnconfirmed && downloadState === 'failed') {
      status.textContent = 'A later download could not be started. An earlier copy request is still unconfirmed; the browser cannot confirm it was saved, and the original file is unchanged.';
    } else if (downloadUnconfirmed) {
      status.textContent = 'Download requested. The browser cannot confirm it was saved, and the original file is unchanged. The browser may suppress leave-page warnings.';
    } else if (downloadState === 'failed') {
      status.textContent = errorMessage || 'The download could not be started. Your browser document remains available; try Download a copy again.';
    } else if (hasSource) {
      status.textContent = 'This browser copy is not synchronized with the original file. Download a copy to keep it outside this browser.';
    } else {
      status.textContent = 'This browser drawing is not saved to the original file. Download a copy to keep it outside this browser.';
    }
  }

  window.addEventListener('beforeunload', event => {
    if (!dirty && !downloadUnconfirmed) return;
    event.preventDefault();
    event.returnValue = '';
  });

  return {tpx: {
    browser_document_dirty: value => {
      dirty = Boolean(value);
      if (dirty) errorMessage = '';
      refresh();
    },
    browser_document_source: value => {
      hasSource = Boolean(value);
      refresh();
    },
    browser_document_reset: () => {
      downloadUnconfirmed = false;
      errorMessage = '';
      downloadState = '';
      refresh();
    },
    browser_document_error: (pointer, length) => {
      errorMessage = read(pointer, length);
      refresh();
    },
    browser_download_begin: (pointer, length) => {
      errorMessage = '';
      downloadState = '';
      return Number(beginBundle(read(pointer, length)));
    },
    browser_download_file: (namePointer, nameLength, dataPointer, dataLength) => {
      const name = bundleName(read(namePointer, nameLength));
      if (!downloadBundle || !downloadBundle.names.includes(name)) return 0;
      downloadBundle.files.set(name, new Uint8Array(runtime.memory().buffer,
        dataPointer, dataLength).slice());
      return 1;
    },
    browser_download_add: (pointer, length) =>
      Number(addBundlePath(read(pointer, length))),
    browser_download_finish: success => {
      const started = finishBundle(Boolean(success));
      downloadState = started ? 'started' : 'failed';
      if (started) downloadUnconfirmed = true;
      errorMessage = '';
      refresh();
      return Number(started);
    }
  }};
}

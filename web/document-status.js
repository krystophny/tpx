// Browser copies live only in the virtual filesystem until the user downloads them.
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
      return Number(runtime.downloads.begin(read(pointer, length)));
    },
    browser_download_add: (pointer, length) =>
      Number(runtime.downloads.add(read(pointer, length))),
    browser_download_finish: success => {
      const started = runtime.downloads.finish(Boolean(success));
      downloadState = started ? 'started' : 'failed';
      if (started) downloadUnconfirmed = true;
      errorMessage = '';
      refresh();
      return Number(started);
    }
  }};
}

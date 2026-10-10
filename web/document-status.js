// Browser copies live only in the virtual filesystem until the user downloads them.
export function documentStatusImports(runtime) {
  const status = document.querySelector('#document-status');
  const decoder = new TextDecoder();
  let hasSource = false, dirty = false, downloadState = '', errorMessage = '';
  const read = (pointer, length) => decoder.decode(
    new Uint8Array(runtime.memory().buffer, pointer, length));

  function refresh() {
    status.dataset.state = dirty ? 'unsaved' : downloadState === 'failed' ? 'error' : 'clean';
    if (dirty) {
      status.textContent = errorMessage
        ? `${errorMessage} Your changes remain unsaved.`
        : downloadState === 'failed'
        ? 'The download could not be started. Your changes remain unsaved; try Download a copy again.'
        : downloadState === 'started'
        ? 'A copy download was requested, but this browser document still has unsaved changes.'
        : 'Unsaved changes in this browser document. Download a copy to keep them; the original file is unchanged.';
    } else if (downloadState === 'failed') {
      status.textContent = errorMessage || 'The download could not be started. Your browser document remains available; try Download a copy again.';
    } else if (downloadState === 'started') {
      status.textContent = 'Download requested. The browser cannot confirm it was saved, and the original file is unchanged.';
    } else if (hasSource) {
      status.textContent = 'This browser copy is not synchronized with the original file. Download a copy to keep it outside this browser.';
    } else {
      status.textContent = 'This browser drawing is not saved to the original file. Download a copy to keep it outside this browser.';
    }
  }

  window.addEventListener('beforeunload', event => {
    if (!dirty) return;
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
      errorMessage = '';
      refresh();
      return Number(started);
    }
  }};
}

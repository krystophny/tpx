// SPDX-License-Identifier: MIT
import {startLCL} from '@lcl/browser-host';
import {texPreviewImports} from './tex-preview.js';
import {documentStatusImports} from './document-status.js';
import {localRecoveryImports} from './local-recovery.js';

function mergeImports(...groups) {
  const result = {};
  for (const group of groups) for (const [module, imports] of Object.entries(group))
    result[module] = {...result[module], ...imports};
  return result;
}

// Modal LCL loops need WebAssembly JSPI; say so instead of failing mid-start.
if (typeof WebAssembly.Suspending !== 'function') {
  const status = document.querySelector('#status');
  status.dataset.state = 'error';
  status.textContent = 'This browser lacks WebAssembly JSPI; use a current version of Chrome, Edge, Firefox or Safari.';
} else startLCL({moduleURL: './tpx.wasm', argv: ['tpx'], createImports: runtime =>
  mergeImports(texPreviewImports(runtime), documentStatusImports(runtime),
    localRecoveryImports(runtime))});

// SPDX-License-Identifier: MIT
import {startLCL} from '@lcl/browser-host';
import {texPreviewImports} from './tex-preview.js';
import {documentStatusImports} from './document-status.js';

function mergeImports(...groups) {
  const result = {};
  for (const group of groups) for (const [module, imports] of Object.entries(group))
    result[module] = {...result[module], ...imports};
  return result;
}

startLCL({moduleURL: './tpx.wasm', argv: ['tpx'], createImports: runtime =>
  mergeImports(texPreviewImports(runtime), documentStatusImports(runtime))});

// SPDX-License-Identifier: MIT
import {startLCL} from '@lcl/browser-host';
import {texPreviewImports} from './tex-preview.js';
startLCL({moduleURL: './tpx.wasm', argv: ['tpx'], createImports: texPreviewImports});

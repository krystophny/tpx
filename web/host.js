// SPDX-License-Identifier: MIT
import {startLCL} from '@lcl/browser-host';
startLCL({moduleURL: './tpx.wasm', argv: ['tpx']});

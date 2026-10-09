// SPDX-License-Identifier: MIT
// Loaded only when a drawing actually contains TeX text. The original TeX
// font has no dynamic font downloads, so previews also work offline.
import {mathjax} from '@mathjax/src/js/mathjax.js';
import {TeX} from '@mathjax/src/js/input/tex.js';
import {SVG} from '@mathjax/src/js/output/svg.js';
import {liteAdaptor} from '@mathjax/src/js/adaptors/liteAdaptor.js';
import {RegisterHTMLHandler} from '@mathjax/src/js/handlers/html.js';
import {MathJaxTexFont} from '@mathjax/mathjax-tex-font/js/svg.js';
import '@mathjax/src/js/input/tex/base/BaseConfiguration.js';
import '@mathjax/src/js/input/tex/ams/AmsConfiguration.js';
import '@mathjax/src/js/input/tex/textmacros/TextMacrosConfiguration.js';

const adaptor = liteAdaptor();
RegisterHTMLHandler(adaptor);
const document = mathjax.document('', {
  InputJax: new TeX({packages: ['base', 'ams', 'textmacros'], maxBuffer: 10000,
    formatError: (_jax, error) => { throw error; }}),
  OutputJax: new SVG({fontCache: 'none', fontData: MathJaxTexFont}),
});

export function texSVG(source) {
  if (source.length > 5000) throw new Error('TeX text exceeds the preview limit');
  // TpX TeXText is a text-mode LaTeX fragment, including embedded $...$ math.
  // textmacros preserves this distinction instead of turning words into math.
  const node = document.convert(`\\text{${source}}`, {display: false});
  const svg = adaptor.firstChild(node);
  const [x, y, width, height] = adaptor.getAttribute(svg, 'viewBox').split(/\s+/).map(Number);
  if (!(width > 0 && height > 0)) throw new Error('Empty TeX preview');
  adaptor.setAttribute(svg, 'xmlns', 'http://www.w3.org/2000/svg');
  // SVG paths are measured in thousandths of an em; baseline is y=0.
  return {xml: adaptor.outerHTML(svg), width: width / 1000,
    height: height / 1000, baseline: -y / 1000};
}

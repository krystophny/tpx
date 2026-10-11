# Scientific figure examples

These three drawings are small, self-contained TpX documents. They use only
native vector objects and TeX labels; none refers to an external image. Open
them from **File → Open Example** in the desktop application. In a browser,
download one of the linked `.TpX` files below, then choose **File → Open**.

- [quadratic-function.TpX](quadratic-function.TpX) combines axes, a smooth curve, and math labels.
- [vector-addition.TpX](vector-addition.TpX) uses directed paths and construction lines to show
  vector addition.
- [double-slit.TpX](double-slit.TpX) combines wavefronts, path arrows, a screen, and math
  annotations in a two-source interference diagram.

## Five steps to a first figure

1. Start a blank drawing with **File → New**. Use **Insert → Insert line** for
   axes and **Insert → Insert curve** for a plotted trace; click to place points
   and finish the curve with a double-click.
2. Choose **Insert → Insert text** and place a label. Select it to edit its
   caption and **TeX source**; for example, enter `$f(x)=x^2-1$` as the TeX
   source. The caption remains editable on the canvas.
3. Select an object to inspect its style and text properties in the
   **Inspector**. Choose **Point coordinates…** for point geometry;
   **Object properties** focuses the Inspector on the selection.
4. Choose **File → Save as…** (**File → Download a copy as…** in the browser),
   select **TikZ source**, and save as
   `figure.tikz`. Keep a `.TpX` copy as the editable native document; TikZ
   source preserves supported edits and reports unsupported changes instead
   of silently dropping them.
5. Include the snippet in a LaTeX document:

   ```tex
   \usepackage{tikz}
   ...
   \input{figure.tikz}
   ```

   Compile with `pdflatex`. On Debian or Ubuntu, `texlive-latex-base` supplies
   the engine and `texlive-pictures` supplies PGF/TikZ. TikZ output includes a
   bounding rectangle from the drawing extent and removes node padding.

## TeX previews and source limits

On desktop, **View → Trust TeX preview for this document** enables live
rendering for imported or refreshed TeX source. Review a file before trusting
it: native preview passes its TeX source to the local LaTeX and `dvipng` tools.
Install `dvisvgm` as well for reusable SVG previews. In the browser, trusted
math text is rendered by the bundled MathJax subset; it does not run a local
TeX engine or external programs.

TikZ import supports a documented subset of source commands. Its lexer limits
one source to 4 MiB, 500,000 tokens, and 256 nested groups. Unsupported source
is reported by the importer; unchanged source is preserved when supported
edits are saved back.

The `.TpX` files are the editable originals. The desktop **File → Open
Example** menu opens one as a new unsaved drawing, so **Save** asks where to
write your copy and cannot replace the bundled file. The browser's virtual file
system lasts only for the current tab, and **File → Download a copy** downloads
a copy to your computer.

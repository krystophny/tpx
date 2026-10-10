# TikZ source support profile

This document describes syntax recognized by the non-GUI TikZ front end. The
profile version is **1**. The lexer and syntax layer retain source bytes and
identify a drawing region; a later semantic evaluator decides whether that
region can become an editable TpX scene. Parsing never invokes TeX or reads
external files.

## Source and span rules

Input is a byte array, limited by default to 4 MiB, 500,000 tokens, and 256
nested groups. Callers may set tighter limits and supply a cancellation
callback. Tokens retain source order, including whitespace, comments, unknown
bytes, and delimiters. A leading UTF-8 BOM is retained and treated as trivia;
other bytes are never decoded or rewritten. Line and column positions count
bytes, start at one, and treat CRLF as one line break. Byte spans use zero-based
`[StartByte, EndByte)` offsets.

Brace, bracket, and parenthesis groups link their opening and closing token
indices. Comments and escaped control symbols do not alter group structure.
Semicolons in groups or comments do not terminate a root TikZ statement.
TeX control words are matched with their exact spelling; case variants are
distinct commands and are diagnosed as unsupported when not in the profile.
`\verb` payloads are kept opaque for delimiter scanning and diagnosed as
unsupported TeX in the selected picture. Parsing an oversized source returns a
limit diagnostic without copying that source into the result; other invalid or
unsupported inputs within the limit remain available through `CopySource`.
Recognized TeX dimensions include `mm`, `cm`, `pt`, `bp`, `in`, `ex`, `em`,
`sp`, `dd`, `pc`, and `px`; the syntax layer does not convert their values.

## Support tiers

### Tier A: current TpX writer envelope and syntax

The inventory below is derived from `src/DevTikZ.pas`:

- `\begingroup` / `\endgroup`, an optional `\beginpgfgraphicnamed{...}` /
  `\endpgfgraphicnamed` pair, and one `tikzpicture` environment.
- Picture options for the writer's `x` and `y` millimetre scales, zero inner
  and outer separations, and optional miter limit.
- `\providecommand` defaults for `\tpxLineWidth`, `\tpxTextSize`,
  `\tpxDashSize`, and `\tpxDotSize`, with the writer's millimetre or point
  dimensions; literal RGB `\definecolor` declarations with simple color names
  (the writer uses `L`, `F`, and `T`).
- Root `\path`, `\draw`, and `\node` statements. The writer emits coordinate
  pairs, `--`, cubic `.. controls ... and ... ..`, `cycle`, `circle`, `ellipse`,
  legacy `arc (start:end:radius)` syntax, `rectangle + (width,height)`, and
  per-path `rotate around={angle:(point)}`. Styles use line width, draw/fill
  colors, and the generated dash/dot macros.
- Generated text nodes with `anchor`, optional rotation, nested text groups,
  `\pgfmathsetlengthmacro` font-size/baseline expressions, `\fontsize`,
  `\selectfont`, and text from `Get_TeXText`.
- Image nodes at a coordinate with a south-west anchor and an
  `\includegraphics` wrapper.

The syntax front end recognizes wrapper balance and statement boundaries. It
does not itself evaluate coordinates, style values, color definitions, font
expressions, text macros, or image paths.

### Tier B: hand-written declarative input

The front end accepts one raw `.tikz` fragment or exactly one uniquely selected
`tikzpicture` in a `.tex` source. The `.tex` wrapper may contain a known plain
document class (`article`, `report`, `book`, `standalone`, or `minimal`),
`tikz`, `pgf`, `xcolor`, or `graphicx` package declarations without package
options, and the `document`, `center`, or `figure` environments. The `preview`
package is allowed only with literal `active,tightpage` options (in either
order), together with `\PreviewEnvironment{tikzpicture}` and a literal
point-valued `\setlength\PreviewBorder{...}`. Literal RGB color declarations
with simple names are allowed in the preamble. A literal
`\DeclareUnicodeCharacter{...}{\ensuremath{\beta}}` declaration is accepted
for the generated beta text mapping. A picture may be empty. Declarative path
statements and node text can contain nested groups,
fractions, escaped percent/braces, comments, Unicode bytes, scientific
notation, and semicolons that are not statement terminators.

Nested `scope` environments are structurally supported with literal
`shift={(x,y)}`, `rotate`, `scale`, `xscale`, and `yscale` options. Shift
coordinates may use numeric or supported dimensional values; scale and angle
values are numeric. Other scope options, macro-valued transforms, and matrix or
canvas transforms are diagnosed as unsupported.

This is a syntax eligibility profile, not a promise that every syntactically
accepted construct has a scene representation. Scene conversion must reject
constructs outside its own supported semantic subset.

## Editable scene profile

The semantic evaluator converts a supported picture into an ordered list of
existing TpX drawing primitives. A `.tex` document must contain one supported
picture; a `.tikz` file may contain a bare fragment. Import rejects a picture
with no visible editable objects, any unsupported visible construct, and any
materialization failure. It never accepts a partial scene. A failed open leaves
the current drawing unchanged.

Coordinates without units use the picture's `x` and `y` basis, which defaults
to one centimetre per coordinate unit. Picture bases and explicit coordinate,
size, and stroke dimensions accept `mm`, `cm`, TeX `pt`, PostScript `bp`, `in`,
and CSS `px`. A coordinate pair must use either two unitless values or two
dimensioned values. `+` is relative to the previous path point without moving
the relative base; `++` also updates that base. Supported picture scales and
numeric scope transforms compose in TikZ source order. Numeric nested scope
transforms support `shift`, `rotate`, `scale`, `xscale`, and `yscale`; path
`rotate around` is also supported. Geometry that would require a sheared
circle, ellipse, or circular arc is rejected rather than approximated.

Editable geometry includes lines, polylines, closed paths, cubic Beziers,
rectangles, circles, ellipses, circular arcs, sectors, and circular segments.
The generated zero-width rectangle used only to declare bounds is retained as
source metadata and does not become a visible object. Text nodes retain their
complete TeX body, anchor, rotation, font height, baseline, and node-local
`text=<color>` glyph color. Plain labels
use native TpX text; imported TeX labels do not enable or invoke live preview.
Generated hatching or arrow artwork that the writer emits as ordinary path
geometry is imported in source order as those paths.

Inline styles support literal RGB/HTML/gray color definitions, the named
standard colors recognized by the evaluator, path `draw`/`fill` enablement,
node `text=<color>`, literal line widths, and solid, dashed, and dotted strokes.
Colored node frames and fills are rejected because native text has no matching
frame or fill primitive. Generated TpX width, text,
dash, and dot defaults are evaluated and retained as source dependencies.
Dash patterns are accepted only when they map exactly to the native solid,
dashed, or dotted line styles. Unsupported option keys, unresolved macros,
library styles, opacity, clipping, gradients, and pattern fills produce a
source-located import error. Hand-written TikZ arrow-head options such as
`->`, `<-`, `arrows=...`, or `>=Stealth` are unsupported; TpX-generated arrow
artwork is accepted only when it is emitted as ordinary supported geometry.

Images must be existing local PNG, JPEG, or BMP files named by a basename in
the document's directory. Nested paths, absolute paths, remote references,
other formats, and missing files are rejected with a source location. Import
does not run TeX, converters, or network requests to resolve an image.

### Tier C: unsupported or invalid input

The parser reports source-located diagnostics for unknown root commands,
unknown commands in option/coordinate contexts, arbitrary text or unattached
groups outside the selected picture, unsupported environments, multiple
picture candidates, malformed wrappers, mismatched or unterminated groups and
environments, and drawing statements without a terminating semicolon.

Dynamic or side-effecting commands such as `\write`, `\input`, `\include`,
catcode changes, macro definitions outside the generated defaults, loops,
Lua, `pgfplots`, and library/style mutation are diagnosed as unsupported,
except when their bytes occur inside a TikZ node text group. Node text is
retained opaquely; import never expands or runs it. TeX preview for an opened
source stays blocked until the user explicitly trusts that document. The
parser does not expand macros, execute control flow, run external tools, access
the network, or resolve image files. In a `.tex` source, non-command content
outside the selected picture is unsupported; unknown preamble code is never
assumed harmless.

## API outline

`TikZLexer.LexTikZ` exposes tokens, linked groups, limits, and lexical
diagnostics. `TikZSyntax.ParseTikZ` returns an owned `TTikZSyntaxResult` with an
outcome, picture region, token/group accessors, diagnostics, a copied source,
and copied source slices. The region records its full wrapper span, header
span, content span, and token boundaries. Indices are zero-based; `-1` means
that a token or group link is absent. The result preserves unknown syntax for
diagnosis but does not imply that such source is editable.

# SVG document support

TpX uses its existing SVG importer and exporter. An SVG file is editable and may
be saved back to `.svg` only when the complete source passes the validated
profile below. Save back writes normalized SVG from the TpX model; it does not
preserve source bytes or formatting. If any feature is outside the profile,
TpX opens the representable geometry as import-only and retains the original
source snapshot for the document session. SVGZ is always import-only.
For editable SVG, same-format save keeps the source canvas's physical size and
complete viewBox, including its origin, even when geometry occupies only part
of the page. Output is normalized XML; original markup and formatting are not
retained.

| SVG construct | Editable profile | Import-only or rejected |
| --- | --- | --- |
| Root and coordinates | SVG root with physical `width`/`height`, `viewBox`, and default `xMidYMid meet`; dimensions must have a uniform physical scale | Missing dimensions/viewBox, percentages, non-default aspect handling, nested SVG viewports |
| Geometry | `line`, `rect`, `circle`, `ellipse`, `polygon`, `polyline`; paths using `M/L/H/V/C/S/Q/T/Z` | Zero-sized shapes and elliptic path arcs are imported but import-only; malformed paths are rejected |
| Transforms | `translate`, uniform non-singular `scale`, `rotate`, and orthogonal uniform `matrix` transforms without reflection | Skew, non-uniform or singular transforms, and reflected matrices are import-only |
| Paint and line style | Solid colors, `none`, positive stroke width, default/nonzero fill rule, default miter limit, and a shared two-to-one `stroke-dasharray` whose effective physical dash length is uniform after absolute units and supported transforms | Gradients, patterns/hatching, URL paints, opacity, zero stroke width, dotted or other custom/mixed dash arrays, other fill rules and color inheritance |
| Text | UTF-8 plain `<text>` with ASCII font family/size, normal or bold weight, normal or italic style, and start/middle/end anchoring; preserved whitespace is retained with `xml:space="preserve"` | Non-ASCII attribute values (including `font-family`), `tspan`, `textPath`, CSS/font fallback lists, unpreserved boundary whitespace and unsupported font properties |
| Groups and metadata | Groups; ASCII plain-text `title` and `desc` | Non-ASCII `title`/`desc`, element IDs and references (`use`, `href`, `xlink:href`), linked/image content, CSS classes/styles, clipping, masks, filters, markers, and other unsupported elements |
| Active/external content | None | Scripts and event attributes are rejected. DOCTYPE/entity declarations are rejected; processing instructions and linked assets are import-only and never fetched. No external entities, network requests, or scripts are executed. |

SVG/CSS pixels use 96 dpi (`1 px = 25.4/96 mm`). TpX stores coordinates in
viewBox user units and uses `PicScale` to map them to physical millimetres;
SVG's y-down coordinates are mapped to TpX's y-up model coordinates. The strict
profile requires the physical root size and viewBox to share one scale. The
parser bounds SVG input to 4 MiB, nesting to 64, and elements/path command
groups to 100,000. SVGZ input is bounded to 4 MiB compressed and 16 MiB
expanded.

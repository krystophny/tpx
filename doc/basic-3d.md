# Basic 3D objects

TpX draws simple 3D objects in its usual line style: white faces, thin grey
edges and hidden faces removed. A 3D object is one drawing object.

- **Insert → Box, Cylinder, Sphere, Cone, Torus** (also on the tool palette):
  drag a box; the object is fitted into it.
- **Insert → Torus segment** works like Insert → Arc: press at the centre,
  drag (or click) to the outer rim at the start angle, then move round
  counterclockwise and click at the end angle. The torus lies in the base
  plane of the 3D view and follows the pointer; Shift snaps both angles to
  15°. The open ends are flat circular caps. The Inspector edits the angle
  and start angle of any torus (360° is a full torus).
- **Modify → Extrude / Revolve** turn the selected closed shapes into one 3D
  object. For Revolve, a selected straight line is the axis (otherwise the
  left edge of the shapes). Revolve leaves a 90° cut-away toward the viewer;
  cut faces use the object's hatching.
- **Transform → 3D view** sets Isometric, Top, Front or Side, or toggles
  Perspective. **Start rotate** (Ctrl+R) on 3D objects orbits the camera:
  drag sideways for azimuth, up/down for elevation, Shift snaps to 15°.
- **Group** on 3D objects joins them into one scene with a shared camera and
  hidden-face removal across bodies. **Ungroup** splits the scene again;
  ungrouping a single extrusion or revolution returns its profiles as 2D
  shapes for editing.

Line style and colour apply to edges, fill colour to faces, hatching to cut
faces.

## File formats

A 3D object is stored with its parameters and, beside them, as the plain
polygons it renders to, so every reader shows the picture.

**TpX.** A `<group>` whose first child is `<solid>`, followed by the rendered
faces as ordinary `<polygon>` elements. TpX versions without 3D support show
the polygons; current TpX reads the solid and ignores them.

```xml
<group>
  <solid x1=".." y1=".." x2=".." y2=".." az="-60" el="25" [persp="1"]
         [hidden="wire"] lw=".." lc=".." fill=".." [ha=".."]>
    <body kind="box" w=".." d=".." h=".."/>
    <body kind="cylinder|cone" r=".." h=".." segments=".."/>
    <body kind="sphere" r=".." segments=".." rings=".."/>
    <body kind="torus" r1=".." r2=".." [angle=".."] [start=".."]
          segments=".." rings=".."/>
    <body kind="extrude" depth="..">  2D profile elements  </body>
    <body kind="revolve" angle=".." start=".." segments="..">  profiles  </body>
  </solid>
  <polygon ..>..</polygon> ...
</group>
```

`x1,y1`–`x2,y2` is the placement: origin, scale (length) and in-plane angle
of the projected model. `az`/`el` are the camera azimuth and elevation in
degrees. A torus segment has `angle` (swept degrees, below 360) and `start`
(degrees from the body's x axis); `angle` is left out for a full torus and
`start` when it is 0, so existing files are written unchanged. A body may
carry `m="12 numbers"`, its row-major 3×4 placement in the scene. The TeX part
of the file contains plain TikZ/PGF paths.

**SVG.** The faces are a plain `<g>` of polygons (hatching as lines), led by an
invisible two-point polyline carrying the placement and followed by
`<tpx:solid xmlns:tpx="https://github.com/krystophny/tpx/ns/solid"
digest="md5">` holding the `<solid>` element in CDATA. Renderers ignore the
record. TpX restores the object when the digest of the face geometry still
matches; edited faces stay plain editable paths.

**TikZ.** The paths sit between two comments:

```tex
% tpx-solid <solid ...>...</solid>
\draw[...] (..) -- (..) -- cycle;
...
% tpx-solid-end <md5 of the path lines>
```

The block compiles with plain TikZ. On reopening, TpX turns a block whose
paths still match the digest back into the 3D object and rewrites only that
block when the object changes; an edited block is kept as plain paths.

Export-only formats (PDF, EPS, PNG, EMF, MetaPost, PSTricks, ...) receive the
rendered faces.

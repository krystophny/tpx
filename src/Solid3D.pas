unit Solid3D;

// Basic 3D objects: primitives, extruded and revolved 2D profiles.
// A TSolid3D is a scene of bodies seen through one orthographic camera.
// It renders to ordinary closed polygon pieces sorted back to front, so
// every device and exporter draws it without knowing about 3D.

interface

uses SysUtils, Classes, Contnrs, Math, Graphics,
  Geometry, Devices, Pieces, GObjBase, GObjects;

type
  TSolidKind = (skBox, skCylinder, skSphere, skCone, skTorus,
    skExtrude, skRevolve);
  TSolidHidden = (shRemoved, shWire);
  TSolidView = (svCustom, svIsometric, svTop, svFront, svSide);

  TVec3 = record
    X, Y, Z: Double;
  end;

  // Row-major 3x4 affine map from body to scene coordinates.
  TMat34 = array[0..11] of Double;

  TSolidBody = class
  public
    Kind: TSolidKind;
    // box: width, depth, height; cylinder and cone: radius, height;
    // sphere: radius; torus: major and minor radius;
    // extrude: depth; revolve: angle and start angle in degrees.
    Size1, Size2, Size3: Double;
    SegU, SegV: Integer;
    M: TMat34;
    // Closed 2D shapes in model units (x along X or R, y along Z).
    Profiles: TGraphicObjList;
    constructor Create(AKind: TSolidKind);
    destructor Destroy; override;
    procedure Assign(Source: TSolidBody);
    procedure SaveToStream(const Stream: TStream);
    procedure LoadFromStream(const Stream: TStream);
    function IsIdentity: Boolean;
  end;

  TSolid3D = class(TPrimitive2D)
  private
    fBodies: TObjectList;
    fAzimuth, fElevation: Double;
    fPerspective: Boolean;
    fHidden: TSolidHidden;
    fPlaced: Boolean;
    // Faces and hatch lines exactly as read from a file; used while the
    // parameters stay unchanged so re-saving reproduces the same numbers.
    fFrozen: TObjectList;
    fFrozenHatch: TPointsSet2D;
    fFrozenKey: string;
    // Eye distance for perspective, set once per rendering (0 = unknown).
    fEyeDistance: Double;
    // While inserting, the two points span the box the object is fitted
    // into; this placement replaces them for projection.
    fFit: Boolean;
    fFitOrigin: TPoint2D;
    fFitScale: Double;
    function GeometryKey: string;
    procedure BoxPlacement(const A, B: TPoint2D; out Origin: TPoint2D;
      out Scale: Double);
    function GetBody(I: Integer): TSolidBody;
    function GetBodyCount: Integer;
  protected
    procedure ProjectBasis(out R, U, C: TVec3);
  public
    class function GetName: string; override;
    constructor Create(ID: Integer); override;
    constructor CreateFromStream(const Stream: TStream); override;
    destructor Destroy; override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure FillPieces; override;
    procedure FinishFirstDraw; override;
    function AddBody(AKind: TSolidKind): TSolidBody;
    procedure AdoptBody(Body: TSolidBody);
    function ExtractBody(I: Integer): TSolidBody;
    procedure SetPlacement(const Origin: TPoint2D; const Scale,
      Angle: TRealType);
    procedure SetPlacementPoints(const P0, P1: TPoint2D);
    // Takes over Faces (one point set per rendered piece) and Hatch when
    // they match the current rendering within Tolerance.
    function FreezeRendering(const Faces: TObjectList; const Hatch: TPointsSet2D;
      const Tolerance: TRealType): Boolean;
    // Hatch segments of the frozen rendering, or nil.
    function FrozenHatch: TPointsSet2D;
    procedure GetPlacement(out Origin: TPoint2D; out Scale,
      Angle: Double);
    procedure SetView(const Azimuth, Elevation: Double);
    procedure ApplyView(const View: TSolidView);
    function ProjectScene(const P: TVec3): TPoint2D;
    // Model-space vector that projects to the 2D drawing offset D.
    function UnprojectOffset(const D: TPoint2D): TVec3;
    procedure SceneBounds(out Lo, Hi: TVec3);
    property Bodies[I: Integer]: TSolidBody read GetBody;
    property BodyCount: Integer read GetBodyCount;
    property Azimuth: Double read fAzimuth write fAzimuth;
    property Elevation: Double read fElevation write fElevation;
    property Perspective: Boolean read fPerspective write fPerspective;
    property Hidden: TSolidHidden read fHidden write fHidden;
    property Placed: Boolean read fPlaced write fPlaced;
  end;

const
  SolidKindIDs: array[TSolidKind] of string =
    ('box', 'cylinder', 'sphere', 'cone', 'torus', 'extrude', 'revolve');
  SolidHiddenIDs: array[TSolidHidden] of string = ('removed', 'wire');
  SolidKindNames: array[TSolidKind] of string =
    ('Box', 'Cylinder', 'Sphere', 'Cone', 'Torus', 'Extrusion',
    'Revolution');
  Solid_DefaultAzimuth = -60;
  Solid_DefaultElevation = 25;
  // Clean line style for new solids: white faces, thin grey edges.
  Solid_DefaultLineColor = $8C8C8C;
  Solid_DefaultLineWidth = 0.5;

function Vec3(const X, Y, Z: Double): TVec3;
function IdentityMat34: TMat34;
function MulMat34(const A, B: TMat34): TMat34;
function ApplyMat34(const M: TMat34; const P: TVec3): TVec3;
function SolidKindFromID(const ID: string; out Kind: TSolidKind): Boolean;
function IsClosedProfile(const Obj: TGraphicObject): Boolean;
// Copies of the profile loops (linearized, closed, model units).
procedure ProfileLoops(const Profiles: TGraphicObjList;
  const Loops: TObjectList);
// Builds a solid from closed 2D shapes; the shapes are copied.
// For a revolution the selected line supplies the radial/axial frame.
function CreateSolidFromProfiles(const Shapes: TGraphicObjList;
  const Kind: TSolidKind; const AxisStart, AxisEnd: TPoint2D): TSolid3D;

const
  SvgSolidNamespace = 'https://github.com/krystophny/tpx/ns/solid';

// MD5 over the rendered geometry strings, whitespace-normalized, used to
// detect faces that were edited outside TpX.
function SolidGeometryDigest(const Geometry: TStrings): string;
// Moves and scales Solid so its rendered extent matches Box.
procedure FitSolidToBox(const Solid: TSolid3D; const Box: TRect2D);
// Moves and scales Solid so its first rendered face lands on Face, the
// same face as read back from another format. Returns False on mismatch.
function FitSolidToFace(const Solid: TSolid3D; const Face: TPointsSet2D): Boolean;
// Hatch line segments (point pairs) filling polygon PP, lines anchored at
// multiples of Step so neighbouring faces line up.
procedure HatchPolygon(const PP: TPointsSet2D; const Hatching: THatching;
  const Step: TRealType; const Segments: TPointsSet2D);

implementation

uses md5;

function NormalizeGeometry(const S: string): string;
var
  I: Integer;
  Space: Boolean;
begin
  Result := '';
  Space := False;
  for I := 1 to Length(S) do
    if S[I] in [#9, #10, #13, ' '] then Space := Result <> ''
    else
    begin
      if Space then Result := Result + ' ';
      Space := False;
      Result := Result + S[I];
    end;
end;

function SolidGeometryDigest(const Geometry: TStrings): string;
var
  I: Integer;
  All: string;
begin
  All := '';
  if Geometry <> nil then
    for I := 0 to Geometry.Count - 1 do
      All := All + NormalizeGeometry(Geometry[I]) + #10;
  Result := MD5Print(MD5String(All));
end;

procedure HatchPolygon(const PP: TPointsSet2D; const Hatching: THatching;
  const Step: TRealType; const Segments: TPointsSet2D);
  procedure Lines(const Angle: Double);
  var
    DX, DY, NX, NY, S, SMin, SMax, PA, PB, T: Double;
    I, J, K: Integer;
    U: array of Double;
    Swap: Double;
  begin
    DX := Cos(Angle);
    DY := Sin(Angle);
    NX := -DY;
    NY := DX;
    SMin := MaxDouble;
    SMax := -MaxDouble;
    for I := 0 to PP.Count - 1 do
    begin
      S := PP[I].X * NX + PP[I].Y * NY;
      SMin := Min(SMin, S);
      SMax := Max(SMax, S);
    end;
    S := Ceil(SMin / Step) * Step;
    while S <= SMax do
    begin
      SetLength(U, 0);
      for I := 0 to PP.Count - 1 do
      begin
        J := (I + 1) mod PP.Count;
        PA := PP[I].X * NX + PP[I].Y * NY - S;
        PB := PP[J].X * NX + PP[J].Y * NY - S;
        if (PA < 0) <> (PB < 0) then
        begin
          T := PA / (PA - PB);
          SetLength(U, Length(U) + 1);
          U[High(U)] := (PP[I].X + T * (PP[J].X - PP[I].X)) * DX +
            (PP[I].Y + T * (PP[J].Y - PP[I].Y)) * DY;
        end;
      end;
      for I := 1 to High(U) do
        for K := I downto 1 do
          if U[K] < U[K - 1] then
          begin
            Swap := U[K];
            U[K] := U[K - 1];
            U[K - 1] := Swap;
          end;
      I := 0;
      while I + 1 <= High(U) do
      begin
        Segments.Add(Point2D(S * NX + U[I] * DX, S * NY + U[I] * DY));
        Segments.Add(Point2D(S * NX + U[I + 1] * DX, S * NY + U[I + 1] * DY));
        Inc(I, 2);
      end;
      S := S + Step;
    end;
  end;
begin
  if (PP.Count < 3) or (Step <= 0) then Exit;
  case Hatching of
    haHorizontal: Lines(0);
    haVertical: Lines(Pi / 2);
    haFDiagonal: Lines(Pi / 4);
    haBDiagonal: Lines(3 * Pi / 4);
    haCross: begin Lines(0); Lines(Pi / 2); end;
    haDiagCross: begin Lines(Pi / 4); Lines(3 * Pi / 4); end;
  end;
end;

function FitSolidToFace(const Solid: TSolid3D; const Face: TPointsSet2D): Boolean;
var
  P: TPiece;
  I, Far: Integer;
  D, Best, S, Scale, Angle, Size: Double;
  Origin: TPoint2D;
begin
  Result := False;
  Solid.UpdateExtension(Solid);
  if (Solid.Pieces.Count = 0) or (Face = nil) then Exit;
  P := Solid.Pieces[0];
  if (P = nil) or (P.Count <> Face.Count) or (P.Count < 2) then Exit;
  Far := 1;
  Best := -1;
  for I := 1 to P.Count - 1 do
  begin
    D := Sqr(P[I].X - P[0].X) + Sqr(P[I].Y - P[0].Y);
    if D > Best then
    begin
      Best := D;
      Far := I;
    end;
  end;
  if Best <= 0 then Exit;
  Size := Max(1, Solid.BoundingBox.Right - Solid.BoundingBox.Left);
  Result := True;
  if (Abs(P[0].X - Face[0].X) < 1E-4 * Size) and
    (Abs(P[0].Y - Face[0].Y) < 1E-4 * Size) and
    (Abs(P[Far].X - Face[Far].X) < 1E-4 * Size) and
    (Abs(P[Far].Y - Face[Far].Y) < 1E-4 * Size) then Exit;
  S := Sqrt((Sqr(Face[Far].X - Face[0].X) + Sqr(Face[Far].Y - Face[0].Y)) / Best);
  Solid.GetPlacement(Origin, Scale, Angle);
  Solid.SetPlacement(Point2D(Face[0].X + S * (Origin.X - P[0].X),
    Face[0].Y + S * (Origin.Y - P[0].Y)), Scale * S, Angle);
end;

procedure FitSolidToBox(const Solid: TSolid3D; const Box: TRect2D);
var
  Own: TRect2D;
  Origin: TPoint2D;
  Scale, Angle, K: Double;
begin
  Solid.UpdateExtension(Solid);
  Own := Solid.BoundingBox;
  if (Own.Right - Own.Left) <= 0 then Exit;
  K := (Box.Right - Box.Left) / (Own.Right - Own.Left);
  Solid.GetPlacement(Origin, Scale, Angle);
  Solid.SetPlacement(Point2D(Box.Left + (Origin.X - Own.Left) * K,
    Box.Bottom + (Origin.Y - Own.Bottom) * K), Scale * K, Angle);
end;

type
  TFace = class
    P: array of TVec3;     // scene coordinates
    Loops: TObjectList;    // cap loops (owned TPointsSet2D in 2D image)
    IsCap: Boolean;
    Depth: Double;
    Holes: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  TCapLoop = class
    P: array of TVec3;
  end;

constructor TFace.Create;
begin
  inherited Create;
  Loops := nil;
end;

destructor TFace.Destroy;
begin
  Loops.Free;
  inherited Destroy;
end;

function Vec3(const X, Y, Z: Double): TVec3;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function VSub(const A, B: TVec3): TVec3;
begin
  Result := Vec3(A.X - B.X, A.Y - B.Y, A.Z - B.Z);
end;

function VAdd(const A, B: TVec3): TVec3;
begin
  Result := Vec3(A.X + B.X, A.Y + B.Y, A.Z + B.Z);
end;

function VScale(const A: TVec3; const S: Double): TVec3;
begin
  Result := Vec3(A.X * S, A.Y * S, A.Z * S);
end;

function VDot(const A, B: TVec3): Double;
begin
  Result := A.X * B.X + A.Y * B.Y + A.Z * B.Z;
end;

function VCross(const A, B: TVec3): TVec3;
begin
  Result := Vec3(A.Y * B.Z - A.Z * B.Y, A.Z * B.X - A.X * B.Z,
    A.X * B.Y - A.Y * B.X);
end;

function VLen(const A: TVec3): Double;
begin
  Result := Sqrt(VDot(A, A));
end;

function IdentityMat34: TMat34;
var
  I: Integer;
begin
  for I := 0 to 11 do Result[I] := 0;
  Result[0] := 1;
  Result[5] := 1;
  Result[10] := 1;
end;

function MulMat34(const A, B: TMat34): TMat34;
var
  R, C: Integer;
begin
  for R := 0 to 2 do
  begin
    for C := 0 to 2 do
      Result[R * 4 + C] := A[R * 4] * B[C] + A[R * 4 + 1] * B[4 + C] +
        A[R * 4 + 2] * B[8 + C];
    Result[R * 4 + 3] := A[R * 4] * B[3] + A[R * 4 + 1] * B[7] +
      A[R * 4 + 2] * B[11] + A[R * 4 + 3];
  end;
end;

function ApplyMat34(const M: TMat34; const P: TVec3): TVec3;
begin
  Result.X := M[0] * P.X + M[1] * P.Y + M[2] * P.Z + M[3];
  Result.Y := M[4] * P.X + M[5] * P.Y + M[6] * P.Z + M[7];
  Result.Z := M[8] * P.X + M[9] * P.Y + M[10] * P.Z + M[11];
end;

function ApplyLinear(const M: TMat34; const P: TVec3): TVec3;
begin
  Result.X := M[0] * P.X + M[1] * P.Y + M[2] * P.Z;
  Result.Y := M[4] * P.X + M[5] * P.Y + M[6] * P.Z;
  Result.Z := M[8] * P.X + M[9] * P.Y + M[10] * P.Z;
end;

function SolidKindFromID(const ID: string; out Kind: TSolidKind): Boolean;
var
  K: TSolidKind;
begin
  for K := Low(TSolidKind) to High(TSolidKind) do
    if SameText(SolidKindIDs[K], ID) then
    begin
      Kind := K;
      Result := True;
      Exit;
    end;
  Kind := skBox;
  Result := False;
end;

function IsClosedProfile(const Obj: TGraphicObject): Boolean;
var
  Prim: TPrimitive2D;
  I: Integer;
begin
  Result := False;
  if not (Obj is TPrimitive2D) or (Obj is TSolid3D) or (Obj is TText2D)
    or (Obj is TBitmap2D) or (Obj is TSymbol2D) then Exit;
  Prim := TPrimitive2D(Obj);
  if Prim.Pieces.Count = 0 then Exit;
  for I := 0 to Prim.Pieces.Count - 1 do
    if (Prim.Pieces[I] = nil) or not Prim.Pieces[I].Closed or
      (Prim.Pieces[I] is TGenPathPiece) or
      (Prim.Pieces[I] is TTextPiece) or (Prim.Pieces[I] is TBitmapPiece) then
      Exit;
  Result := True;
end;

function SignedArea(const PP: TPointsSet2D): Double;
var
  I, J: Integer;
begin
  Result := 0;
  for I := 0 to PP.Count - 1 do
  begin
    J := (I + 1) mod PP.Count;
    Result := Result + PP[I].X * PP[J].Y - PP[J].X * PP[I].Y;
  end;
  Result := Result / 2;
end;

function PointInLoop(const P: TPoint2D; const PP: TPointsSet2D): Boolean;
var
  I, J: Integer;
begin
  Result := False;
  J := PP.Count - 1;
  for I := 0 to PP.Count - 1 do
  begin
    if ((PP[I].Y > P.Y) <> (PP[J].Y > P.Y)) and
      (P.X < (PP[J].X - PP[I].X) * (P.Y - PP[I].Y) /
      (PP[J].Y - PP[I].Y) + PP[I].X) then
      Result := not Result;
    J := I;
  end;
end;

procedure ProfileLoops(const Profiles: TGraphicObjList;
  const Loops: TObjectList);
var
  Obj: TGraphicObject;
  Prim: TPrimitive2D;
  I, J, Depth: Integer;
  PP, Q: TPointsSet2D;
  R: TRect2D;
  Precision: TRealType;
begin
  Loops.Clear;
  if Profiles = nil then Exit;
  Obj := Profiles.FirstObj;
  while Obj <> nil do
  begin
    if Obj is TPrimitive2D then
    begin
      Prim := TPrimitive2D(Obj);
      Prim.UpdateExtension(nil);
      R := Prim.Pieces.GetBoundingBox;
      Precision := Max(R.Right - R.Left, R.Top - R.Bottom) / 200;
      if Precision <= 0 then Precision := 0.01;
      for I := 0 to Prim.Pieces.Count - 1 do
        if (Prim.Pieces[I] <> nil) and Prim.Pieces[I].Closed then
        begin
          PP := TPointsSet2D.Create(0);
          Prim.Pieces[I].Linearize(PP, Precision);
          // Drop a repeated closing point.
          if (PP.Count > 1) and IsSamePoint2D(PP[0], PP[PP.Count - 1]) then
            PP.Delete(PP.Count - 1);
          if PP.Count >= 3 then Loops.Add(PP) else PP.Free;
        end;
    end;
    Obj := Profiles.NextObj;
  end;
  // Outer loops counter-clockwise, holes clockwise (even-odd nesting).
  for I := 0 to Loops.Count - 1 do
  begin
    PP := TPointsSet2D(Loops[I]);
    Depth := 0;
    for J := 0 to Loops.Count - 1 do
      if J <> I then
      begin
        Q := TPointsSet2D(Loops[J]);
        if PointInLoop(PP[0], Q) then Inc(Depth);
      end;
    if (Odd(Depth) and (SignedArea(PP) > 0)) or
      (not Odd(Depth) and (SignedArea(PP) < 0)) then
      PP.ReversePoints;
  end;
end;

function LoopIsHole(const PP: TPointsSet2D): Boolean;
begin
  Result := SignedArea(PP) < 0;
end;

{ TSolidBody }

constructor TSolidBody.Create(AKind: TSolidKind);
begin
  inherited Create;
  Kind := AKind;
  M := IdentityMat34;
  Profiles := TGraphicObjList.Create;
  Profiles.FreeOnDelete := True;
  case Kind of
    skBox:
      begin Size1 := 1; Size2 := 1; Size3 := 1; SegU := 1; SegV := 1; end;
    skCylinder:
      begin Size1 := 0.5; Size2 := 1; Size3 := 0; SegU := 24; SegV := 1; end;
    skSphere:
      begin Size1 := 0.5; Size2 := 0; Size3 := 0; SegU := 24; SegV := 12; end;
    skCone:
      begin Size1 := 0.5; Size2 := 1; Size3 := 0; SegU := 24; SegV := 1; end;
    skTorus:
      begin Size1 := 0.6; Size2 := 0.25; Size3 := 0; SegU := 36; SegV := 16; end;
    skExtrude:
      begin Size1 := 10; Size2 := 0; Size3 := 0; SegU := 1; SegV := 1; end;
    skRevolve:
      begin Size1 := 360; Size2 := 0; Size3 := 0; SegU := 36; SegV := 1; end;
  end;
end;

destructor TSolidBody.Destroy;
begin
  Profiles.Free;
  inherited Destroy;
end;

procedure TSolidBody.Assign(Source: TSolidBody);
var
  Obj: TGraphicObject;
  Copy: TGraphicObject;
begin
  Kind := Source.Kind;
  Size1 := Source.Size1;
  Size2 := Source.Size2;
  Size3 := Source.Size3;
  SegU := Source.SegU;
  SegV := Source.SegV;
  M := Source.M;
  Profiles.Clear;
  Obj := Source.Profiles.FirstObj;
  while Obj <> nil do
  begin
    Copy := TGraphicObjectClass(Obj.ClassType).Create(Obj.ID);
    Copy.Assign(Obj);
    Profiles.Add(Copy);
    Obj := Source.Profiles.NextObj;
  end;
end;

function TSolidBody.IsIdentity: Boolean;
var
  I: TMat34;
  K: Integer;
begin
  I := IdentityMat34;
  Result := True;
  for K := 0 to 11 do
    if Abs(M[K] - I[K]) > 1E-12 then
    begin
      Result := False;
      Exit;
    end;
end;

procedure TSolidBody.SaveToStream(const Stream: TStream);
var
  B: Byte;
  N: Longint;
  W: Word;
  Obj: TGraphicObject;
begin
  B := Ord(Kind);
  Stream.Write(B, SizeOf(B));
  Stream.Write(Size1, SizeOf(Size1));
  Stream.Write(Size2, SizeOf(Size2));
  Stream.Write(Size3, SizeOf(Size3));
  Stream.Write(SegU, SizeOf(SegU));
  Stream.Write(SegV, SizeOf(SegV));
  Stream.Write(M, SizeOf(M));
  N := Profiles.Count;
  Stream.Write(N, SizeOf(N));
  Obj := Profiles.FirstObj;
  while Obj <> nil do
  begin
    W := TpXFindClassIndex(Obj.ClassName);
    Stream.Write(W, SizeOf(W));
    Obj.SaveToStream(Stream);
    Obj := Profiles.NextObj;
  end;
end;

procedure TSolidBody.LoadFromStream(const Stream: TStream);
var
  B: Byte;
  N, I: Longint;
  W: Word;
begin
  Stream.ReadBuffer(B, SizeOf(B));
  if B > Ord(High(TSolidKind)) then
    raise EReadError.Create('Invalid 3D body kind');
  Kind := TSolidKind(B);
  Stream.ReadBuffer(Size1, SizeOf(Size1));
  Stream.ReadBuffer(Size2, SizeOf(Size2));
  Stream.ReadBuffer(Size3, SizeOf(Size3));
  Stream.ReadBuffer(SegU, SizeOf(SegU));
  Stream.ReadBuffer(SegV, SizeOf(SegV));
  Stream.ReadBuffer(M, SizeOf(M));
  Stream.ReadBuffer(N, SizeOf(N));
  if (N < 0) or (N > 10000) then
    raise EReadError.Create('Invalid 3D profile count');
  Profiles.Clear;
  for I := 0 to N - 1 do
  begin
    Stream.ReadBuffer(W, SizeOf(W));
    Profiles.Add(TpXFindClassByIndex(W).CreateFromStream(Stream));
  end;
end;

{ Tessellation }

procedure PrimitiveLoop(const Body: TSolidBody; const Loops: TObjectList);
var
  PP: TPointsSet2D;
  I, N: Integer;
  T, R, H: Double;
begin
  PP := TPointsSet2D.Create(0);
  Loops.Add(PP);
  case Body.Kind of
    skBox:
      begin
        PP.Add(Point2D(-Body.Size1 / 2, -Body.Size3 / 2));
        PP.Add(Point2D(Body.Size1 / 2, -Body.Size3 / 2));
        PP.Add(Point2D(Body.Size1 / 2, Body.Size3 / 2));
        PP.Add(Point2D(-Body.Size1 / 2, Body.Size3 / 2));
      end;
    skCylinder:
      begin
        R := Body.Size1;
        H := Body.Size2;
        PP.Add(Point2D(0, -H / 2));
        PP.Add(Point2D(R, -H / 2));
        PP.Add(Point2D(R, H / 2));
        PP.Add(Point2D(0, H / 2));
      end;
    skCone:
      begin
        PP.Add(Point2D(0, -Body.Size2 / 2));
        PP.Add(Point2D(Body.Size1, -Body.Size2 / 2));
        PP.Add(Point2D(0, Body.Size2 / 2));
      end;
    skSphere:
      begin
        N := Max(Body.SegV, 3);
        for I := 0 to N do
        begin
          T := Pi * I / N;
          PP.Add(Point2D(Body.Size1 * Sin(T), -Body.Size1 * Cos(T)));
        end;
      end;
    skTorus:
      begin
        N := Max(Body.SegV, 3);
        for I := 0 to N - 1 do
        begin
          T := 2 * Pi * I / N;
          PP.Add(Point2D(Body.Size1 + Body.Size2 * Cos(T),
            Body.Size2 * Sin(T)));
        end;
      end;
  end;
end;

procedure AddFace(const Faces: TObjectList; const Pts: array of TVec3;
  const Outward: TVec3);
var
  F: TFace;
  N: TVec3;
  I, K: Integer;
begin
  K := Length(Pts);
  if K < 3 then Exit;
  N := Vec3(0, 0, 0);
  for I := 0 to K - 1 do
    N := VAdd(N, VCross(Pts[I], Pts[(I + 1) mod K]));
  if VLen(N) < 1E-12 then Exit;
  F := TFace.Create;
  SetLength(F.P, K);
  if VDot(N, Outward) >= 0 then
    for I := 0 to K - 1 do F.P[I] := Pts[I]
  else
    for I := 0 to K - 1 do F.P[I] := Pts[K - 1 - I];
  Faces.Add(F);
end;

procedure AddCap(const Faces: TObjectList; const Loops: TObjectList;
  const Map: TMat34; const Normal: TVec3);
var
  F: TFace;
  I, J: Integer;
  PP: TPointsSet2D;
  L: TCapLoop;
begin
  F := TFace.Create;
  F.IsCap := True;
  F.Loops := TObjectList.Create(True);
  for I := 0 to Loops.Count - 1 do
  begin
    PP := TPointsSet2D(Loops[I]);
    L := TCapLoop.Create;
    SetLength(L.P, PP.Count);
    for J := 0 to PP.Count - 1 do
      L.P[J] := ApplyMat34(Map, Vec3(PP[J].X, PP[J].Y, 0));
    F.Loops.Add(L);
    if LoopIsHole(PP) then F.Holes := True;
  end;
  // Remember the outward normal in P[0] for back face culling.
  SetLength(F.P, 1);
  F.P[0] := Normal;
  Faces.Add(F);
end;

procedure TessellateBody(const Body: TSolidBody; const Faces: TObjectList);
var
  Loops: TObjectList;
  PP: TPointsSet2D;
  I, J, K, N, NU: Integer;
  A, B: TPoint2D;
  Phi0, Phi1, Start, Angle, Depth, Y0: Double;
  Q: array[0..3] of TVec3;
  Outward, Mid: TVec3;
  Full: Boolean;
  CapMap: TMat34;
  Local: TObjectList;
  Disk: array of TVec3;
  function Rev(const P: TPoint2D; const Phi: Double): TVec3;
  begin
    Result := Vec3(P.X * Cos(Phi), P.X * Sin(Phi), P.Y);
  end;
  procedure Emit(const Pts: array of TVec3; const Out: TVec3);
  var
    T: array of TVec3;
    Z: Integer;
  begin
    SetLength(T, Length(Pts));
    for Z := 0 to High(Pts) do T[Z] := ApplyMat34(Body.M, Pts[Z]);
    AddFace(Faces, T, ApplyLinear(Body.M, Out));
  end;
begin
  Loops := TObjectList.Create(True);
  try
    if Body.Kind in [skExtrude, skRevolve] then
      ProfileLoops(Body.Profiles, Loops)
    else
      PrimitiveLoop(Body, Loops);
    if Body.Kind in [skBox, skExtrude] then
    begin
      // Profile in the XZ plane, extruded along +Y.
      if Body.Kind = skBox then
      begin
        Depth := Body.Size2;
        Y0 := -Depth / 2;
      end
      else
      begin
        Depth := Body.Size1;
        Y0 := 0;
      end;
      for K := 0 to Loops.Count - 1 do
      begin
        PP := TPointsSet2D(Loops[K]);
        N := PP.Count;
        for I := 0 to N - 1 do
        begin
          A := PP[I];
          B := PP[(I + 1) mod N];
          Q[0] := Vec3(A.X, Y0, A.Y);
          Q[1] := Vec3(B.X, Y0, B.Y);
          Q[2] := Vec3(B.X, Y0 + Depth, B.Y);
          Q[3] := Vec3(A.X, Y0 + Depth, A.Y);
          Emit(Q, Vec3(B.Y - A.Y, 0, -(B.X - A.X)));
        end;
      end;
      // Caps: (x, y) -> (x, Y, y)
      CapMap := IdentityMat34;
      CapMap[0] := 1; CapMap[1] := 0; CapMap[2] := 0; CapMap[3] := 0;
      CapMap[4] := 0; CapMap[5] := 0; CapMap[6] := 0; CapMap[7] := Y0;
      CapMap[8] := 0; CapMap[9] := 1; CapMap[10] := 0; CapMap[11] := 0;
      AddCap(Faces, Loops, MulMat34(Body.M, CapMap),
        ApplyLinear(Body.M, Vec3(0, -1, 0)));
      CapMap[7] := Y0 + Depth;
      AddCap(Faces, Loops, MulMat34(Body.M, CapMap),
        ApplyLinear(Body.M, Vec3(0, 1, 0)));
    end
    else
    begin
      if Body.Kind = skRevolve then
      begin
        Angle := Body.Size1;
        Start := Body.Size2;
      end
      else
      begin
        Angle := 360;
        Start := 0;
      end;
      if Angle > 360 then Angle := 360;
      if Angle < 1 then Angle := 1;
      Full := Angle >= 360 - 1E-9;
      NU := Max(3, Round(Max(Body.SegU, 3) * Angle / 360));
      for K := 0 to Loops.Count - 1 do
      begin
        PP := TPointsSet2D(Loops[K]);
        N := PP.Count;
        // A flat edge ending on the axis becomes one disk (or sector)
        // instead of a fan of thin triangles.
        for I := 0 to N - 1 do
        begin
          A := PP[I];
          B := PP[(I + 1) mod N];
          if ((A.X <= 1E-9) <> (B.X <= 1E-9)) and
            (Abs(A.Y - B.Y) <= 1E-9 * Max(1, Abs(A.X) + Abs(B.X))) then
          begin
            // Outward normal of a counter-clockwise loop edge is (dy, -dx).
            Mid := Vec3(0, 0, -(B.X - A.X));
            if A.X > B.X then B := A;
            SetLength(Disk, 0);
            if not Full then
            begin
              SetLength(Disk, 1);
              Disk[0] := Vec3(0, 0, B.Y);
            end;
            for J := 0 to NU - Ord(Full) do
            begin
              SetLength(Disk, Length(Disk) + 1);
              Disk[High(Disk)] := Rev(B, DegToRad(Start + Angle * J / NU));
            end;
            Emit(Disk, Mid);
          end;
        end;
        for J := 0 to NU - 1 do
        begin
          Phi0 := DegToRad(Start + Angle * J / NU);
          Phi1 := DegToRad(Start + Angle * (J + 1) / NU);
          for I := 0 to N - 1 do
          begin
            A := PP[I];
            B := PP[(I + 1) mod N];
            if (A.X <= 1E-9) and (B.X <= 1E-9) then Continue;
            if ((A.X <= 1E-9) <> (B.X <= 1E-9)) and
              (Abs(A.Y - B.Y) <= 1E-9 * Max(1, Abs(A.X) + Abs(B.X))) then
              Continue;
            Q[0] := Rev(A, Phi0);
            Q[1] := Rev(B, Phi0);
            Q[2] := Rev(B, Phi1);
            Q[3] := Rev(A, Phi1);
            Mid := Vec3(B.Y - A.Y, 0, -(B.X - A.X));
            Outward := Vec3(Mid.X * Cos((Phi0 + Phi1) / 2),
              Mid.X * Sin((Phi0 + Phi1) / 2), Mid.Z);
            if (A.X <= 1E-9) then
              Emit([Q[0], Q[1], Q[2]], Outward)
            else if (B.X <= 1E-9) then
              Emit([Q[0], Q[1], Q[3]], Outward)
            else
              Emit(Q, Outward);
          end;
        end;
      end;
      if not Full then
      begin
        // Caps lie in the profile plane rotated to the start and end angle.
        Local := Loops;
        Phi0 := DegToRad(Start);
        CapMap := IdentityMat34;
        CapMap[0] := Cos(Phi0); CapMap[1] := 0; CapMap[2] := 0;
        CapMap[4] := Sin(Phi0); CapMap[5] := 0; CapMap[6] := 0;
        CapMap[8] := 0; CapMap[9] := 1; CapMap[10] := 0;
        AddCap(Faces, Local, MulMat34(Body.M, CapMap),
          ApplyLinear(Body.M, Vec3(Sin(Phi0), -Cos(Phi0), 0)));
        Phi1 := DegToRad(Start + Angle);
        CapMap[0] := Cos(Phi1);
        CapMap[4] := Sin(Phi1);
        AddCap(Faces, Local, MulMat34(Body.M, CapMap),
          ApplyLinear(Body.M, Vec3(-Sin(Phi1), Cos(Phi1), 0)));
      end;
    end;
  finally
    Loops.Free;
  end;
end;

{ TSolid3D }

class function TSolid3D.GetName: string;
begin
  Result := '3D object';
end;

constructor TSolid3D.Create(ID: Integer);
begin
  inherited CreateSpec(ID, 2, 0);
  fBodies := TObjectList.Create(True);
  fAzimuth := Solid_DefaultAzimuth;
  fElevation := Solid_DefaultElevation;
  fPerspective := False;
  fHidden := shRemoved;
  fPlaced := False;
  Points.Clear;
  Points.Add(Point2D(0, 0));
  Points.Add(Point2D(1, 0));
  fCanDeletePoints := False;
end;

destructor TSolid3D.Destroy;
begin
  fFrozen.Free;
  fFrozenHatch.Free;
  fBodies.Free;
  inherited Destroy;
end;

function TSolid3D.GetBody(I: Integer): TSolidBody;
begin
  Result := TSolidBody(fBodies[I]);
end;

function TSolid3D.GetBodyCount: Integer;
begin
  Result := fBodies.Count;
end;

function TSolid3D.AddBody(AKind: TSolidKind): TSolidBody;
begin
  Result := TSolidBody.Create(AKind);
  fBodies.Add(Result);
end;

procedure TSolid3D.AdoptBody(Body: TSolidBody);
begin
  fBodies.Add(Body);
end;

function TSolid3D.ExtractBody(I: Integer): TSolidBody;
begin
  Result := TSolidBody(fBodies.Extract(fBodies[I]));
end;

const
  SolidStreamVersion: Byte = 2;

// Point sets as count + points; used for the frozen rendering.
procedure WritePointSet(const Stream: TStream; const PP: TPointsSet2D);
var
  N, I: Longint;
  P: TPoint2D;
begin
  N := PP.Count;
  Stream.Write(N, SizeOf(N));
  for I := 0 to N - 1 do
  begin
    P := PP[I];
    Stream.Write(P, SizeOf(P));
  end;
end;

procedure ReadPointSet(const Stream: TStream; const PP: TPointsSet2D);
var
  N, I: Longint;
  P: TPoint2D;
begin
  Stream.ReadBuffer(N, SizeOf(N));
  if (N < 0) or (Int64(N) * SizeOf(P) > Stream.Size - Stream.Position) then
    raise EReadError.Create('Invalid 3D rendering data');
  for I := 0 to N - 1 do
  begin
    Stream.ReadBuffer(P, SizeOf(P));
    PP.Add(P);
  end;
end;

procedure TSolid3D.SaveToStream(const Stream: TStream);
var
  N: Longint;
  I: Integer;
  B: Byte;
begin
  inherited SaveToStream(Stream);
  Stream.Write(SolidStreamVersion, SizeOf(SolidStreamVersion));
  Stream.Write(fAzimuth, SizeOf(fAzimuth));
  Stream.Write(fElevation, SizeOf(fElevation));
  Stream.Write(fPerspective, SizeOf(fPerspective));
  B := Ord(fHidden);
  Stream.Write(B, SizeOf(B));
  Stream.Write(fPlaced, SizeOf(fPlaced));
  N := fBodies.Count;
  Stream.Write(N, SizeOf(N));
  for I := 0 to fBodies.Count - 1 do Bodies[I].SaveToStream(Stream);
  // The frozen rendering survives undo snapshots and the clipboard.
  if (fFrozen <> nil) and (fFrozenKey = GeometryKey) then N := fFrozen.Count
  else N := -1;
  Stream.Write(N, SizeOf(N));
  if N >= 0 then
  begin
    for I := 0 to N - 1 do
    begin
      B := Ord(TPiece(fFrozen[I]).Closed) or (Ord(TPiece(fFrozen[I]).Line) shl 1) or
        (Ord(TPiece(fFrozen[I]).Fill) shl 4) or (Ord(TPiece(fFrozen[I]).Hatch) shl 7);
      Stream.Write(B, SizeOf(B));
      WritePointSet(Stream, TPiece(fFrozen[I]));
    end;
    WritePointSet(Stream, fFrozenHatch);
  end;
end;

constructor TSolid3D.CreateFromStream(const Stream: TStream);
var
  N, I: Longint;
  B, Version: Byte;
  Body: TSolidBody;
  Piece: TPolyLinPiece;
begin
  inherited CreateFromStream(Stream);
  fBodies := TObjectList.Create(True);
  Stream.ReadBuffer(Version, SizeOf(Version));
  if (Version < 1) or (Version > SolidStreamVersion) then
    raise EReadError.Create('Unsupported 3D object stream version');
  Stream.ReadBuffer(fAzimuth, SizeOf(fAzimuth));
  Stream.ReadBuffer(fElevation, SizeOf(fElevation));
  Stream.ReadBuffer(fPerspective, SizeOf(fPerspective));
  Stream.ReadBuffer(B, SizeOf(B));
  if B > Ord(High(TSolidHidden)) then B := 0;
  fHidden := TSolidHidden(B);
  Stream.ReadBuffer(fPlaced, SizeOf(fPlaced));
  Stream.ReadBuffer(N, SizeOf(N));
  if (N < 0) or (N > 10000) then
    raise EReadError.Create('Invalid 3D body count');
  for I := 0 to N - 1 do
  begin
    Body := TSolidBody.Create(skBox);
    try
      Body.LoadFromStream(Stream);
    except
      Body.Free;
      raise;
    end;
    fBodies.Add(Body);
  end;
  if Version >= 2 then
  begin
    Stream.ReadBuffer(N, SizeOf(N));
    if N > 1000000 then raise EReadError.Create('Invalid 3D rendering data');
    if N >= 0 then
    begin
      fFrozen := TObjectList.Create(True);
      for I := 0 to N - 1 do
      begin
        Piece := TPolyLinPiece.Create(0);
        fFrozen.Add(Piece);
        Stream.ReadBuffer(B, SizeOf(B));
        Piece.Closed := B and 1 <> 0;
        Piece.Line := TPieceLine((B shr 1) and 7);
        Piece.Fill := TPieceFill((B shr 4) and 7);
        Piece.Hatch := TPieceHatch((B shr 7) and 1);
        ReadPointSet(Stream, Piece);
      end;
      fFrozenHatch := TPointsSet2D.Create(0);
      ReadPointSet(Stream, fFrozenHatch);
      fFrozenKey := GeometryKey;
    end;
  end;
  UpdateExtension(Self);
end;

procedure TSolid3D.Assign(const Obj: TGraphicObject);
var
  Src: TSolid3D;
  I: Integer;
  Body: TSolidBody;
  Piece: TPolyLinPiece;
begin
  if Obj = Self then Exit;
  inherited Assign(Obj);
  if Obj is TSolid3D then
  begin
    Src := TSolid3D(Obj);
    Points.Clear;
    Points.Copy(Src.Points, 0, Src.Points.Count - 1);
    fAzimuth := Src.fAzimuth;
    fElevation := Src.fElevation;
    fPerspective := Src.fPerspective;
    fHidden := Src.fHidden;
    fPlaced := Src.fPlaced;
    fBodies.Clear;
    for I := 0 to Src.BodyCount - 1 do
    begin
      Body := TSolidBody.Create(Src.Bodies[I].Kind);
      Body.Assign(Src.Bodies[I]);
      fBodies.Add(Body);
    end;
    FreeAndNil(fFrozen);
    FreeAndNil(fFrozenHatch);
    if (Src.fFrozen <> nil) and (Src.fFrozenKey = Src.GeometryKey) then
    begin
      fFrozen := TObjectList.Create(True);
      for I := 0 to Src.fFrozen.Count - 1 do
      begin
        Piece := TPolyLinPiece.Create(0);
        Piece.Assign(TPiece(Src.fFrozen[I]));
        Piece.Copy(TPiece(Src.fFrozen[I]), 0, TPiece(Src.fFrozen[I]).Count - 1);
        fFrozen.Add(Piece);
      end;
      fFrozenHatch := TPointsSet2D.Create(0);
      if Src.fFrozenHatch.Count > 0 then
        fFrozenHatch.Copy(Src.fFrozenHatch, 0, Src.fFrozenHatch.Count - 1);
      fFrozenKey := GeometryKey;
    end;
  end;
  UpdateExtension(Self);
end;

procedure TSolid3D.ProjectBasis(out R, U, C: TVec3);
var
  Az, El: Double;
begin
  Az := DegToRad(fAzimuth);
  El := DegToRad(fElevation);
  C := Vec3(Cos(El) * Cos(Az), Cos(El) * Sin(Az), Sin(El));
  R := Vec3(-Sin(Az), Cos(Az), 0);
  U := Vec3(-Sin(El) * Cos(Az), -Sin(El) * Sin(Az), Cos(El));
end;

procedure TSolid3D.SceneBounds(out Lo, Hi: TVec3);
var
  Faces: TObjectList;
  I, J, K: Integer;
  F: TFace;
  procedure Grow(const P: TVec3);
  begin
    Lo := Vec3(Min(Lo.X, P.X), Min(Lo.Y, P.Y), Min(Lo.Z, P.Z));
    Hi := Vec3(Max(Hi.X, P.X), Max(Hi.Y, P.Y), Max(Hi.Z, P.Z));
  end;
begin
  Lo := Vec3(MaxDouble, MaxDouble, MaxDouble);
  Hi := Vec3(-MaxDouble, -MaxDouble, -MaxDouble);
  Faces := TObjectList.Create(True);
  try
    for I := 0 to BodyCount - 1 do TessellateBody(Bodies[I], Faces);
    for I := 0 to Faces.Count - 1 do
    begin
      F := TFace(Faces[I]);
      if F.IsCap then
      begin
        for J := 0 to F.Loops.Count - 1 do
          for K := 0 to High(TCapLoop(F.Loops[J]).P) do
            Grow(TCapLoop(F.Loops[J]).P[K]);
      end
      else
        for J := 0 to High(F.P) do Grow(F.P[J]);
    end;
  finally
    Faces.Free;
  end;
  if Lo.X > Hi.X then
  begin
    Lo := Vec3(0, 0, 0);
    Hi := Vec3(0, 0, 0);
  end;
end;

procedure TSolid3D.GetPlacement(out Origin: TPoint2D; out Scale,
  Angle: Double);
var
  DX, DY: Double;
begin
  Origin := Points[0];
  DX := Points[1].X - Points[0].X;
  DY := Points[1].Y - Points[0].Y;
  Scale := Sqrt(DX * DX + DY * DY);
  if Scale <= 0 then Scale := 1;
  Angle := ArcTan2(DY, DX);
end;

procedure TSolid3D.SetPlacement(const Origin: TPoint2D; const Scale,
  Angle: TRealType);
begin
  Points.DisableEvents := True;
  Points[0] := Origin;
  Points[1] := Point2D(Origin.X + Scale * Cos(Angle),
    Origin.Y + Scale * Sin(Angle));
  Points.DisableEvents := False;
  fPlaced := True;
  UpdateExtension(Self);
end;

function TSolid3D.GeometryKey: string;
var
  Stream: TMemoryStream;
  B: Byte;
  P: TPoint2D;
  I: Integer;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.Write(fAzimuth, SizeOf(fAzimuth));
    Stream.Write(fElevation, SizeOf(fElevation));
    Stream.Write(fPerspective, SizeOf(fPerspective));
    B := Ord(fHidden);
    Stream.Write(B, SizeOf(B));
    P := Points[0];
    Stream.Write(P, SizeOf(P));
    P := Points[1];
    Stream.Write(P, SizeOf(P));
    Stream.Write(fHatching, SizeOf(fHatching));
    for I := 0 to BodyCount - 1 do Bodies[I].SaveToStream(Stream);
    Result := MD5Print(MD5Buffer(Stream.Memory^, Stream.Size));
  finally
    Stream.Free;
  end;
end;

function TSolid3D.FreezeRendering(const Faces: TObjectList;
  const Hatch: TPointsSet2D; const Tolerance: TRealType): Boolean;
var
  I, J: Integer;
  F: TPointsSet2D;
begin
  Result := False;
  FreeAndNil(fFrozen);
  FreeAndNil(fFrozenHatch);
  UpdateExtension(Self);
  if Faces.Count <> Pieces.Count then Exit;
  for I := 0 to Pieces.Count - 1 do
  begin
    F := TPointsSet2D(Faces[I]);
    if F.Count <> Pieces[I].Count then Exit;
    for J := 0 to F.Count - 1 do
      if (Abs(F[J].X - Pieces[I][J].X) > Tolerance) or
        (Abs(F[J].Y - Pieces[I][J].Y) > Tolerance) then Exit;
  end;
  fFrozen := TObjectList.Create(True);
  for I := 0 to Pieces.Count - 1 do
  begin
    F := TPolyLinPiece.Create(Pieces[I].Count);
    TPolyLinPiece(F).Assign(Pieces[I]);
    F.Copy(TPointsSet2D(Faces[I]), 0, TPointsSet2D(Faces[I]).Count - 1);
    fFrozen.Add(F);
  end;
  fFrozenHatch := TPointsSet2D.Create(Hatch.Count);
  if Hatch.Count > 0 then fFrozenHatch.Copy(Hatch, 0, Hatch.Count - 1);
  fFrozenKey := GeometryKey;
  UpdateExtension(Self);
  Result := True;
end;

function TSolid3D.FrozenHatch: TPointsSet2D;
begin
  Result := nil;
  if (fFrozen <> nil) and (fFrozenKey = GeometryKey) then Result := fFrozenHatch;
end;

procedure TSolid3D.SetPlacementPoints(const P0, P1: TPoint2D);
begin
  Points.DisableEvents := True;
  Points[0] := P0;
  Points[1] := P1;
  Points.DisableEvents := False;
  fPlaced := True;
  UpdateExtension(Self);
end;

function TSolid3D.ProjectScene(const P: TVec3): TPoint2D;
var
  R, U, C: TVec3;
  X, Y, F, Scale, Angle: Double;
  Origin: TPoint2D;
  Lo, Hi: TVec3;
begin
  ProjectBasis(R, U, C);
  X := VDot(P, R);
  Y := VDot(P, U);
  if fPerspective then
  begin
    F := fEyeDistance;
    if F <= 0 then
    begin
      SceneBounds(Lo, Hi);
      F := 3 * Max(VLen(VSub(Hi, Lo)), 1E-9);
    end;
    F := F / Max(F - VDot(P, C), 1E-9 * F);
    X := X * F;
    Y := Y * F;
  end;
  if fFit then
  begin
    Origin := fFitOrigin;
    Scale := fFitScale;
    Angle := 0;
  end
  else
    GetPlacement(Origin, Scale, Angle);
  Result := Point2D(Origin.X + Scale * (X * Cos(Angle) - Y * Sin(Angle)),
    Origin.Y + Scale * (X * Sin(Angle) + Y * Cos(Angle)));
end;

function TSolid3D.UnprojectOffset(const D: TPoint2D): TVec3;
var
  R, U, C: TVec3;
  Scale, Angle, X, Y: Double;
  Origin: TPoint2D;
begin
  ProjectBasis(R, U, C);
  GetPlacement(Origin, Scale, Angle);
  X := (D.X * Cos(Angle) + D.Y * Sin(Angle)) / Scale;
  Y := (-D.X * Sin(Angle) + D.Y * Cos(Angle)) / Scale;
  Result := VAdd(VScale(R, X), VScale(U, Y));
end;

procedure TSolid3D.SetView(const Azimuth, Elevation: Double);
begin
  fAzimuth := Azimuth - 360 * Floor((Azimuth + 180) / 360);
  fElevation := Max(-90, Min(90, Elevation));
  UpdateExtension(Self);
end;

procedure TSolid3D.ApplyView(const View: TSolidView);
begin
  case View of
    svIsometric: SetView(Solid_DefaultAzimuth, Solid_DefaultElevation);
    svTop: SetView(-90, 90);
    svFront: SetView(-90, 0);
    svSide: SetView(0, 0);
  end;
end;

procedure TSolid3D.BoxPlacement(const A, B: TPoint2D; out Origin: TPoint2D;
  out Scale: Double);
var
  Faces: TObjectList;
  I, J, K: Integer;
  F: TFace;
  Lo, Hi: TVec3;
  MinU, MaxU, MinV, MaxV, W, H: Double;
  procedure Grow(const P: TVec3);
  var
    Q: TPoint2D;
  begin
    Q := ProjectScene(P);
    MinU := Min(MinU, Q.X);
    MaxU := Max(MaxU, Q.X);
    MinV := Min(MinV, Q.Y);
    MaxV := Max(MaxV, Q.Y);
  end;
begin
  // Project every vertex with unit placement; the drawn image is the
  // outline of these points, so its extent is what must fit the box.
  MinU := MaxDouble; MaxU := -MaxDouble;
  MinV := MaxDouble; MaxV := -MaxDouble;
  fFit := True;
  fFitOrigin := Point2D(0, 0);
  fFitScale := 1;
  Faces := TObjectList.Create(True);
  try
    for I := 0 to BodyCount - 1 do TessellateBody(Bodies[I], Faces);
    if fPerspective then
    begin
      SceneBounds(Lo, Hi);
      fEyeDistance := 3 * Max(VLen(VSub(Hi, Lo)), 1E-9);
    end;
    for I := 0 to Faces.Count - 1 do
    begin
      F := TFace(Faces[I]);
      if F.IsCap then
      begin
        for J := 0 to F.Loops.Count - 1 do
          for K := 0 to High(TCapLoop(F.Loops[J]).P) do
            Grow(TCapLoop(F.Loops[J]).P[K]);
      end
      else
        for J := 0 to High(F.P) do Grow(F.P[J]);
    end;
  finally
    fEyeDistance := 0;
    fFit := False;
    Faces.Free;
  end;
  W := Abs(B.X - A.X);
  H := Abs(B.Y - A.Y);
  if (W < 1E-6) and (H < 1E-6) then
  begin
    W := 30;
    H := 30;
  end;
  Scale := Min(IfThen(MaxU > MinU, W / (MaxU - MinU), MaxDouble),
    IfThen(MaxV > MinV, H / (MaxV - MinV), MaxDouble));
  if (Scale <= 0) or (Scale >= MaxDouble / 2) then
  begin
    Scale := 20;
    MinU := 0; MaxU := 0;
    MinV := 0; MaxV := 0;
  end;
  Origin := Point2D((A.X + B.X) / 2 - Scale * (MinU + MaxU) / 2,
    (A.Y + B.Y) / 2 - Scale * (MinV + MaxV) / 2);
end;

procedure TSolid3D.FinishFirstDraw;
var
  Origin: TPoint2D;
  S: Double;
begin
  inherited FinishFirstDraw;
  if fPlaced then Exit;
  // During insertion the two points span the box the object is fitted into.
  BoxPlacement(Points[0], Points[1], Origin, S);
  SetPlacement(Origin, S, 0);
end;

type
  TProjected = class
    Face: TFace;
    Depth: Double;
  end;

function CompareDepth(Item1, Item2: Pointer): Integer;
var
  D: Double;
begin
  D := TProjected(Item1).Depth - TProjected(Item2).Depth;
  if D < 0 then Result := -1
  else if D > 0 then Result := 1
  else Result := 0;
end;

procedure TSolid3D.FillPieces;
var
  Faces, Order, Bridged: TObjectList;
  I, J, K: Integer;
  F: TFace;
  Item: TProjected;
  R, U, C, Sum, Lo, Hi: TVec3;
  Piece: TPolyLinPiece;
  L: TCapLoop;
  Count: Integer;
  FitOrigin: TPoint2D;
  FitScale: Double;
  procedure AddPoly(const Pts: array of TVec3; const Closed: Boolean;
    const Line: TPieceLine; const Fill: TPieceFill; const Hatch: TPieceHatch);
  var
    Z: Integer;
  begin
    Piece := TPolyLinPiece.Create(Length(Pts));
    for Z := 0 to High(Pts) do Piece.Add(ProjectScene(Pts[Z]));
    Piece.Closed := Closed;
    Piece.Line := Line;
    Piece.Fill := Fill;
    Piece.Hatch := Hatch;
    Pieces.Add(Piece);
  end;
  procedure AddCapPieces(const F: TFace);
  var
    Outer, Merged: array of TVec3;
    Z, Y2, Best, BestK, LoopIndex: Integer;
    Loop: TCapLoop;
    D, BestD: Double;
    FillKind: TPieceFill;
  begin
    if fHidden = shWire then FillKind := pfiNone else FillKind := pfiDefault;
    // Holes are bridged into the outer loop, so devices without even-odd
    // paths fill the cap correctly; the outlines are drawn separately.
    begin
      if not F.Holes then
      begin
        for Z := 0 to F.Loops.Count - 1 do
        begin
          Loop := TCapLoop(F.Loops[Z]);
          AddPoly(Loop.P, True, pliDefault, FillKind, phaDefault);
        end;
        Exit;
      end;
      // Merge every loop into the first (outer) loop through bridges.
      Loop := TCapLoop(F.Loops[0]);
      SetLength(Merged, Length(Loop.P));
      for Z := 0 to High(Loop.P) do Merged[Z] := Loop.P[Z];
      for LoopIndex := 1 to F.Loops.Count - 1 do
      begin
        Loop := TCapLoop(F.Loops[LoopIndex]);
        BestD := MaxDouble;
        Best := 0;
        BestK := 0;
        for Z := 0 to High(Merged) do
          for Y2 := 0 to High(Loop.P) do
          begin
            D := VLen(VSub(Merged[Z], Loop.P[Y2]));
            if D < BestD then
            begin
              BestD := D;
              Best := Z;
              BestK := Y2;
            end;
          end;
        SetLength(Outer, 0);
        for Z := 0 to Best do
        begin
          SetLength(Outer, Length(Outer) + 1);
          Outer[High(Outer)] := Merged[Z];
        end;
        for Z := 0 to High(Loop.P) + 1 do
        begin
          SetLength(Outer, Length(Outer) + 1);
          Outer[High(Outer)] := Loop.P[(BestK + Z) mod Length(Loop.P)];
        end;
        for Z := Best to High(Merged) do
        begin
          SetLength(Outer, Length(Outer) + 1);
          Outer[High(Outer)] := Merged[Z];
        end;
        Merged := Outer;
      end;
      AddPoly(Merged, True, pliNone, FillKind, phaDefault);
      for Z := 0 to F.Loops.Count - 1 do
        AddPoly(TCapLoop(F.Loops[Z]).P, True, pliDefault, pfiNone, phaNone);
    end;
  end;
begin
  if fBodies = nil then Exit;
  if fFrozen <> nil then
  begin
    if fFrozenKey = GeometryKey then
    begin
      for I := 0 to fFrozen.Count - 1 do
      begin
        Piece := TPolyLinPiece.Create(TPiece(fFrozen[I]).Count);
        Piece.Assign(TPiece(fFrozen[I]));
        Piece.Copy(TPiece(fFrozen[I]), 0, TPiece(fFrozen[I]).Count - 1);
        Pieces.Add(Piece);
      end;
      Exit;
    end;
    FreeAndNil(fFrozen);
    FreeAndNil(fFrozenHatch);
  end;
  // The insertion preview shows the object fitted into the dragged box,
  // exactly as FinishFirstDraw will place it.
  if not fPlaced and (BodyCount > 0) and (Points.Count >= 2) then
  begin
    BoxPlacement(Points[0], Points[1], FitOrigin, FitScale);
    fFitOrigin := FitOrigin;
    fFitScale := FitScale;
    fFit := True;
  end;
  Faces := TObjectList.Create(True);
  Order := TObjectList.Create(True);
  try
    for I := 0 to BodyCount - 1 do TessellateBody(Bodies[I], Faces);
    if fPerspective then
    begin
      SceneBounds(Lo, Hi);
      fEyeDistance := 3 * Max(VLen(VSub(Hi, Lo)), 1E-9);
    end;
    ProjectBasis(R, U, C);
    for I := 0 to Faces.Count - 1 do
    begin
      F := TFace(Faces[I]);
      Sum := Vec3(0, 0, 0);
      Count := 0;
      if F.IsCap then
      begin
        // Back face culling with the stored outward normal.
        if (fHidden = shRemoved) and (VDot(F.P[0], C) < -1E-9) then Continue;
        for J := 0 to F.Loops.Count - 1 do
        begin
          L := TCapLoop(F.Loops[J]);
          for K := 0 to High(L.P) do
          begin
            Sum := VAdd(Sum, L.P[K]);
            Inc(Count);
          end;
        end;
      end
      else
      begin
        if (fHidden = shRemoved) and
          (VDot(VCross(VSub(F.P[1], F.P[0]), VSub(F.P[High(F.P)], F.P[0])),
          C) < -1E-12) then Continue;
        for J := 0 to High(F.P) do Sum := VAdd(Sum, F.P[J]);
        Count := Length(F.P);
      end;
      if Count = 0 then Continue;
      Item := TProjected.Create;
      Item.Face := F;
      Item.Depth := VDot(VScale(Sum, 1 / Count), C);
      Order.Add(Item);
    end;
    Order.Sort(CompareDepth);
    for I := 0 to Order.Count - 1 do
    begin
      F := TProjected(Order[I]).Face;
      if F.IsCap then AddCapPieces(F)
      else if fHidden = shWire then
        AddPoly(F.P, True, pliDefault, pfiNone, phaNone)
      else
        AddPoly(F.P, True, pliDefault, pfiDefault, phaNone);
    end;
  finally
    fEyeDistance := 0;
    fFit := False;
    Order.Free;
    Faces.Free;
  end;
end;

function CreateSolidFromProfiles(const Shapes: TGraphicObjList;
  const Kind: TSolidKind; const AxisStart, AxisEnd: TPoint2D): TSolid3D;
var
  Obj: TGraphicObject;
  Copy: TPrimitive2D;
  Body: TSolidBody;
  R: TRect2D;
  First: Boolean;
  CY, CX, DX, DY, Length, Along, FrameAngle: TRealType;
  T: TTransf2D;
  Lo, Hi: TVec3;
  J: Integer;
  P: TPoint2D;
  function NativePoint(const Point: TPoint2D): TPoint2D;
  begin
    // TpX stores coordinates with six significant digits. Render from
    // those same values so reopening does not reorder nearly coplanar faces.
    Result := Point;
    Result.X := StrToFloat(Format('%.6g', [Point.X]));
    Result.Y := StrToFloat(Format('%.6g', [Point.Y]));
  end;
begin
  Result := TSolid3D.Create(-1);
  Body := Result.AddBody(Kind);
  First := True;
  R := Rect2D(0, 0, 0, 0);
  Obj := Shapes.FirstObj;
  while Obj <> nil do
  begin
    if IsClosedProfile(Obj) then
    begin
      Copy := TPrimitive2D(TGraphicObjectClass(Obj.ClassType).Create(-1));
      Copy.Assign(Obj);
      Body.Profiles.Add(Copy);
      if First then R := TPrimitive2D(Obj).BoundingBox
      else R := BoxOutBox2D(R, TPrimitive2D(Obj).BoundingBox);
      First := False;
      if Obj is TPrimitive2D then
      begin
        Result.LineColor := TPrimitive2D(Obj).LineColor;
        Result.LineStyle := TPrimitive2D(Obj).LineStyle;
        Result.LineWidth := TPrimitive2D(Obj).LineWidth;
        Result.FillColor := TPrimitive2D(Obj).FillColor;
        Result.Hatching := TPrimitive2D(Obj).Hatching;
        Result.HatchColor := TPrimitive2D(Obj).HatchColor;
      end;
    end;
    Obj := Shapes.NextObj;
  end;
  CX := (R.Left + R.Right) / 2;
  CY := (R.Bottom + R.Top) / 2;
  FrameAngle := 0;
  if Kind = skRevolve then
  begin
    DX := AxisEnd.X - AxisStart.X;
    DY := AxisEnd.Y - AxisStart.Y;
    Length := Hypot(DX, DY);
    DX := DX / Length;
    DY := DY / Length;
    // Orient the radial direction toward the profile, independent of the
    // order of the selected axis endpoints.
    if (CX - AxisStart.X) * DY - (CY - AxisStart.Y) * DX < 0 then
    begin
      DX := -DX;
      DY := -DY;
    end;
    Along := (CX - AxisStart.X) * DX + (CY - AxisStart.Y) * DY;
    CX := AxisStart.X + Along * DX;
    CY := AxisStart.Y + Along * DY;
    FrameAngle := ArcTan2(DX, DY);
  end;
  // Profiles move to the axis frame (radial X, axial Y). Placement carries
  // the inverse rotation so Ungroup returns the original editable shapes.
  T := MultiplyTransform2D(Translate2D(-CX, -CY), Rotate2D(FrameAngle));
  Obj := Body.Profiles.FirstObj;
  while Obj <> nil do
  begin
    TPrimitive2D(Obj).Transform(T);
    for J := 0 to TPrimitive2D(Obj).Points.Count - 1 do
    begin
      P := NativePoint(TPrimitive2D(Obj).Points[J]);
      TPrimitive2D(Obj).Points[J] := P;
    end;
    Obj := Body.Profiles.NextObj;
  end;
  if Kind = skExtrude then
    Body.Size1 := Max(R.Right - R.Left, R.Top - R.Bottom) / 2;
  Result.SetPlacement(Point2D(CX, CY), 1, -FrameAngle);
  Result.SetPlacementPoints(NativePoint(Result.Points[0]),
    NativePoint(Result.Points[1]));
  // Keep the visible picture where the profiles were.
  Result.SceneBounds(Lo, Hi);
end;

end.

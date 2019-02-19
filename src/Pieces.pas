unit Pieces;

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface

uses SysUtils, Classes, Contnrs, Graphics, Geometry, Devices, Math;

type

  TPieceLine = (pliDefault, pliNone, pliSolidDefault,
    pliFillAsDefault, pliFill);
  TPieceFill = (pfiDefault, pfiNone, pfiLineAsDefault, pfiLine);
  TPieceHatch = (phaDefault, phaNone);

  { Base class for elementary shapes from which compound
  graphical objects are made. An extension of TPointsSet2D}

  TPiece = class(TPointsSet2D)
  public
    Closed: Boolean;
    Line: TPieceLine;
    Fill: TPieceFill;
    Hatch: TPieceHatch;
    constructor Create(const _Capacity: Integer);
    constructor Assign(Source: TPiece); virtual;
    function GetLineStyle(Obj: TObject): TLineStyle;
    function GetLineWidth(Obj: TObject): TRealType;
    function GetLineColor(Obj: TObject): TColor;
    function GetHatchColor(Obj: TObject): TColor;
    function GetHatching(Obj: TObject): THatching;
    function GetFillColor(Obj: TObject): TColor;
    procedure Linearize(const PP: TPointsSet2D;
      const Precision: TRealType); virtual;
    procedure BezierPoints(const PP: TPointsSet2D); virtual;
    function IsPointOnStroke(
      const P: TPoint2D; const Precision: TRealType;
      var Distance: TRealType;
      out Pos: Integer): Boolean; virtual;
  end;

  // polyline/polygon
  TPolyLinPiece = class(TPiece)
  end;

  // polybezier
  TBezierPiece = class(TPiece)
  public
    procedure Linearize(const PP: TPointsSet2D;
      const Precision: TRealType); override;
    procedure BezierPoints(const PP: TPointsSet2D); override;
    procedure GetBoundingBox0(var Rect: TRect2D); override;
    function IsPointOnStroke(
      const P: TPoint2D; const Precision: TRealType;
      var Distance: TRealType;
      out Pos: Integer): Boolean; override;
    function IsPointInside(const P: TPoint2D;
      const Precision: TRealType): Boolean; override;
  end;

  TFixedPiece = class(TPiece)
    procedure GetBoundingBox0(var Rect: TRect2D); override;
  end;

  // Circle
  TCirclePiece = class(TFixedPiece)
  public
    R: TRealType;
    constructor Create(const _Capacity: Integer);
    procedure TransformPoints(const T: TTransf2D); override;
    procedure Linearize(const PP: TPointsSet2D;
      const Precision: TRealType); override;
    procedure BezierPoints(const PP: TPointsSet2D); override;
    function GetBoundingBox: TRect2D; override;
    function IsPointOnStroke(
      const P: TPoint2D; const Precision: TRealType;
      var Distance: TRealType;
      out Pos: Integer): Boolean; override;
    function IsPointInside(const P: TPoint2D;
      const Precision: TRealType): Boolean; override;
  end;

  // Arc, Sector, Segment
  TCircularPiece = class(TFixedPiece)
  public
    R, SA, EA: TRealType;
    Kind: TCircularKind;
    procedure Linearize(const PP: TPointsSet2D;
      const Precision: TRealType); override;
    procedure BezierPoints(const PP: TPointsSet2D); override;
    function GetBoundingBox: TRect2D; override;
    function IsPointOnStroke(
      const P: TPoint2D; const Precision: TRealType;
      var Distance: TRealType;
      out Pos: Integer): Boolean; override;
    function IsPointInside(const P: TPoint2D;
      const Precision: TRealType): Boolean; override;
  end;

  // Rectangle
  TRectanglePiece = class(TFixedPiece)
  public
    W, H, ARot: TRealType;
    procedure Linearize(const PP: TPointsSet2D;
      const Precision: TRealType); override;
    function GetBoundingBox: TRect2D; override;
    function IsPointOnStroke(
      const P: TPoint2D; const Precision: TRealType;
      var Distance: TRealType;
      out Pos: Integer): Boolean; override;
    function IsPointInside(const P: TPoint2D;
      const Precision: TRealType): Boolean; override;
  end;

  // Ellipse
  TEllipsePiece = class(TFixedPiece)
  public
    RX, RY, ARot: TRealType;
    Kind: TCircularKind;
    procedure Linearize(const PP: TPointsSet2D;
      const Precision: TRealType); override;
    procedure BezierPoints(const PP: TPointsSet2D); override;
    function GetBoundingBox: TRect2D; override;
    function IsPointOnStroke(
      const P: TPoint2D; const Precision: TRealType;
      var Distance: TRealType;
      out Pos: Integer): Boolean; override;
    function IsPointInside(const P: TPoint2D;
      const Precision: TRealType): Boolean; override;
  end;

  // Text label
  TTextPiece = class(TFixedPiece)
  public
    ARot: TRealType;
    WideText: WideString;
    TeXText: AnsiString;
    Height: TRealType;
    HAlignment: THAlignment;  
    VAlignment: TVAlignment;
    FaceName: AnsiString;
    Style: TFontStyles;
    Charset: TFontCharSet;
    procedure Linearize(const PP: TPointsSet2D;
      const Precision: TRealType); override;
    procedure MeasureTextRectangle(
      var P: TPoint2D; out Size: TVector2D);
    function GetBoundingBox: TRect2D; override;
    function IsPointOnStroke(
      const P: TPoint2D; const Precision: TRealType;
      var Distance: TRealType;
      out Pos: Integer): Boolean; override;
    function IsPointInside(const P: TPoint2D;
      const Precision: TRealType): Boolean; override;
  end;

  // Bitmap
  TBitmapPiece = class(TFixedPiece)
  public
    BitmapEntry: TObject;
    Width, Height: TRealType;
    ImageLink: string;
    KeepAspectRatio: Boolean;
    constructor Create(const _Capacity: Integer);
    procedure Linearize(const PP: TPointsSet2D;
      const Precision: TRealType); override;
    function GetBoundingBox: TRect2D; override;
    function IsPointOnStroke(
      const P: TPoint2D; const Precision: TRealType;
      var Distance: TRealType;
      out Pos: Integer): Boolean; override;
    function IsPointInside(const P: TPoint2D;
      const Precision: TRealType): Boolean; override;
  end;

// Generic path

  TGenPathPiece = class(TPiece)
  protected
    PGenPath: TGenericPath;
  public
    constructor Create(const _Path: TGenericPath);
    destructor Destroy; override;
    procedure GetBoundingBox0(var Rect: TRect2D); override;
    function IsPointOnStroke(
      const P: TPoint2D; const Precision: TRealType;
      var Distance: TRealType;
      out Pos: Integer): Boolean; override;
    function IsPointInside(const P: TPoint2D;
      const Precision: TRealType): Boolean; override;
    property Path: TGenericPath read PGenPath;
  end;

  TPiece_CreateFromSvg = class(TPointsSet2D)
  private
    fIsBezier: Boolean;
    fPiece: TPiece;
    procedure PathOnClose(const X, Y: TRealType);
    procedure PathOnMoveTo(const X, Y: TRealType);
    procedure PathOnLineTo(const X, Y: TRealType);
    procedure PathOnBezierTo(const X1, Y1, X2, Y2, X3, Y3:
      TRealType);
  public
    function CreateFromSvgPath(const SvgPath: string): TPiece;
  end;

  TArrayOfPiece = class(TObjectList)
  protected
    function GetItem(I: Integer): TPiece;
  public
    constructor Create(const Capacity: Integer);
    function AddFromSvgPath(const SvgPath: string): TPiece;
    function AddFromSvgPathT(const SvgPath: string;
      const T: TTransf2D): TPiece;
    procedure GetBoundingBox0(var R: TRect2D);
    function GetBoundingBox: TRect2D;
    procedure TransForm(const T: TTransf2D);
    property Item[I: Integer]: TPiece read GetItem; default;
    function IsPointOnStroke(Obj: TObject;
      const P: TPoint2D; const Precision: TRealType;
      var Distance: TRealType;
      out Pos: Integer): Boolean; virtual;
    function IsPointInside(
      Obj: TObject; const P: TPoint2D;
      const Precision: TRealType): Boolean; virtual;
  end;

implementation

uses GObjects, Drawings, Bitmaps, SysBasic;

// =====================================================================
// TPiece
// =====================================================================

constructor TPiece.Create(const _Capacity: Integer);
begin
  inherited Create(_Capacity);
  Closed := False;
  Line := pliDefault;
  Fill := pfiDefault;
  Hatch := phaDefault;
end;

constructor TPiece.Assign(Source: TPiece);
begin
  Closed := Source.Closed;
  Line := Source.Line;
  Fill := Source.Fill;
  Hatch := Source.Hatch;
end;

function TPiece.GetLineStyle(Obj: TObject): TLineStyle;
begin
  case Line of
    pliDefault: Result := (Obj as TPrimitive2D).LineStyle;
    pliNone: Result := liNone;
    pliSolidDefault:
      if (Obj as TPrimitive2D).LineStyle in [liNone] then
        Result := liNone
      else
        Result := liSolid;
    pliFillAsDefault:
      Result := liSolid;
    pliFill:
      if (Obj as TPrimitive2D).FillColor = clDefault then
        Result := liNone
      else
        Result := liSolid;
  end;
end;

function TPiece.GetLineWidth(Obj: TObject): TRealType;
begin
  Result := (Obj as TPrimitive2D).LineWidth;
end;

function TPiece.GetLineColor(Obj: TObject): TColor;
begin
  case Line of
    pliDefault, pliSolidDefault:
      Result := (Obj as TPrimitive2D).LineColor;
    pliNone: Result := clRed;
    pliFillAsDefault:
      if (Obj as TPrimitive2D).LineColor <> clDefault then
        Result := (Obj as TPrimitive2D).LineColor
      else if (Obj as TPrimitive2D).FillColor <> clDefault then
        Result := (Obj as TPrimitive2D).FillColor
      else
        Result := clBlack;
    pliFill: Result := (Obj as TPrimitive2D).FillColor;
  end;
end;

function TPiece.GetHatchColor(Obj: TObject): TColor;
begin
  Result := (Obj as TPrimitive2D).HatchColor;
end;

function TPiece.GetHatching(Obj: TObject): THatching;
begin
  case Hatch of
    phaDefault: Result := (Obj as TPrimitive2D).Hatching;
    phaNone: Result := haNone;
  end;
end;

function TPiece.GetFillColor(Obj: TObject): TColor;
begin
  case Fill of
    pfiDefault: Result := (Obj as TPrimitive2D).FillColor;
    pfiNone: Result := clDefault;
    pfiLine:
      if (Obj as TPrimitive2D).LineColor <> clDefault then
        Result := (Obj as TPrimitive2D).LineColor
      else
        Result := clBlack;
    pfiLineAsDefault:
      if (Obj as TPrimitive2D).FillColor <> clDefault then
        Result := (Obj as TPrimitive2D).FillColor
      else if (Obj as TPrimitive2D).LineColor <> clDefault then
        Result := (Obj as TPrimitive2D).LineColor
      else
        Result := clBlack;
  end;
end;

procedure TPiece.Linearize(const PP: TPointsSet2D;
  const Precision: TRealType);
begin
  PP.Clear;
  PP.Copy(Self, 0, Count - 1);
end;

procedure TPiece.BezierPoints(const PP: TPointsSet2D);
var
  LinPP: TPointsSet2D;
begin
  LinPP := TPointsSet2D.Create(0);
  try
    Linearize(LinPP, 0);
    LinPolyToBezier(LinPP, Closed, PP);
  finally
    LinPP.Free;
  end;
end;

function TPiece.IsPointOnStroke(
  const P: TPoint2D; const Precision: TRealType;
  var Distance: TRealType;
  out Pos: Integer): Boolean;
begin
  Result :=
    IsPointOnPolylineStroke(P, Closed, Distance, Pos);
end;

// =====================================================================
// TBezierPiece
// =====================================================================

procedure TBezierPiece.Linearize(const PP: TPointsSet2D;
  const Precision: TRealType);
begin
  LinearizeBezier(Self, Precision, Closed, PP);
end;

procedure TBezierPiece.BezierPoints(const PP: TPointsSet2D);
begin
  PP.Clear;
  PP.Copy(Self, 0, Count - 1);
end;

procedure TBezierPiece.GetBoundingBox0(var Rect: TRect2D);
begin
  BezierPathBoundingBox(Self, Closed, Rect);
end;

function TBezierPiece.IsPointOnStroke(
  const P: TPoint2D; const Precision: TRealType;
  var Distance: TRealType;
  out Pos: Integer): Boolean;
begin
  Result := IsPointOnBezierStroke(
    Self, P, Closed, Precision, Distance, Pos);
end;

function TBezierPiece.IsPointInside(const P: TPoint2D;
  const Precision: TRealType): Boolean;
begin
  Result := IsPointInBezierShape(
    Self, P, Closed, IdentityTransf2D, wn_NonZero,
    Precision);
end;

// =====================================================================
// TFixedPiece
// =====================================================================

procedure TFixedPiece.GetBoundingBox0(var Rect: TRect2D);
begin
  Rect := BoxOutBox2D0(Rect, GetBoundingBox);
end;

// =====================================================================
// TCirclePiece
// =====================================================================

constructor TCirclePiece.Create(const _Capacity: Integer);
begin
  inherited Create(_Capacity);
  Closed := True;
end;

procedure TCirclePiece.TransformPoints(const T: TTransf2D);
var
  V: TVector2D;
  P: TPoint2D;
begin
  V.X := R;
  V.Y := 0;
  P := TransformPoint2D(ShiftPoint(Points[0], V), T);
  Points[0] := TransformPoint2D(Points[0], T);
  R := PointDistance2D(Points[0], P);
end;

procedure TCirclePiece.Linearize(const PP: TPointsSet2D;
  const Precision: TRealType);
begin
  LinearizeCircle(PP, Points[0], R, Precision);
end;

procedure TCirclePiece.BezierPoints(const PP: TPointsSet2D);
begin
  EllipseBezierPoints8(Points[0], R, R, 0, PP);
end;

function TCirclePiece.GetBoundingBox: TRect2D;
begin
  Result := CirceBoundingBox(Points[0], R);
end;

function TCirclePiece.IsPointOnStroke(
  const P: TPoint2D; const Precision: TRealType;
  var Distance: TRealType;
  out Pos: Integer): Boolean;
var
  D: TRealType;
begin
  D := Abs(PointDistance2D(P, Points[0]) - R);
  Result := D <= Distance;
  if Result then Distance := D;
end;

function TCirclePiece.IsPointInside(const P: TPoint2D;
  const Precision: TRealType): Boolean;
begin
  Result := PointDistance2D(P, Points[0]) <= R;
end;

// =====================================================================
// TCircularPiece
// =====================================================================

procedure TCircularPiece.Linearize(const PP: TPointsSet2D;
  const Precision: TRealType);
begin
  LinearizeCircular(PP, Points[0], R, SA, EA, Kind, Precision);
end;

procedure TCircularPiece.BezierPoints(const PP: TPointsSet2D);
begin
  CircularBezierPoints(Points[0], R, SA, EA, PP, Kind);
end;

function TCircularPiece.GetBoundingBox: TRect2D;
begin
  Result := CircularBoundingBox(Points[0], R, SA, EA, Kind);
end;

function TCircularPiece.IsPointOnStroke(
  const P: TPoint2D; const Precision: TRealType;
  var Distance: TRealType;
  out Pos: Integer): Boolean;
begin
  Result := IsPointOnCircular(
    P, Points[0], R, SA, EA, Kind, Precision, Distance);
end;

function TCircularPiece.IsPointInside(const P: TPoint2D;
  const Precision: TRealType): Boolean;
begin
  Result := IsPointInCircular(
    P, Points[0], R, SA, EA, Kind);
end;

// =====================================================================
// TRectanglePiece
// =====================================================================

procedure TRectanglePiece.Linearize(const PP: TPointsSet2D;
  const Precision: TRealType);
begin
  LinearizeRectangle(PP, Points[0], W, H, ARot);
end;

function TRectanglePiece.GetBoundingBox: TRect2D;
begin
  Result := RectangleBoundingBox(Points[0], W, H, ARot);
end;

function TRectanglePiece.IsPointOnStroke(
  const P: TPoint2D; const Precision: TRealType;
  var Distance: TRealType;
  out Pos: Integer): Boolean;
var
  V: TVector2D;
  CP: TPoint2D;
  MinDist, D: TRealType;
begin
  V := TransformVector2D(Vector2D(Points[0], P),
    Rotate2D(-ARot));
  CP := Point2D(V.X, V.Y);
  MinDist := PointSegmentDistance2D(
    CP, Point2D(0, 0), Point2D(0, H));
  D := PointSegmentDistance2D(
    CP, Point2D(0, H), Point2D(W, H));
  if D < MinDist then MinDist := D;
  D := PointSegmentDistance2D(
    CP, Point2D(W, H), Point2D(W, 0));
  if D < MinDist then MinDist := D;
  D := PointSegmentDistance2D(
    CP, Point2D(W, 0), Point2D(0, 0));
  if D < MinDist then MinDist := D;
  Result := MinDist < Distance;
  if Result then Distance := MinDist;
end;

function TRectanglePiece.IsPointInside(const P: TPoint2D;
  const Precision: TRealType): Boolean;
var
  V: TVector2D;
begin
  V := TransformVector2D(Vector2D(Points[0], P),
    Rotate2D(-ARot));
  Result := (V.X >= 0) and (V.Y >= 0)
    and (V.X <= W) and (V.Y <= H);
end;

// =====================================================================
// TEllipsePiece
// =====================================================================

procedure TEllipsePiece.Linearize(const PP: TPointsSet2D;
  const Precision: TRealType);
begin
  LinearizeEllipse(PP, Points[0], RX, RY, ARot, Precision);
end;

procedure TEllipsePiece.BezierPoints(const PP: TPointsSet2D);
begin
  EllipseBezierPoints8(Points[0], RX, RY, ARot, PP);
end;

function TEllipsePiece.GetBoundingBox: TRect2D;
begin
  Result := EllipseBoundingBox(Points[0], RX, RY, ARot);
end;

function TEllipsePiece.IsPointOnStroke(
  const P: TPoint2D; const Precision: TRealType;
  var Distance: TRealType;
  out Pos: Integer): Boolean;
begin
  Result := IsPointOnEllipse(
    P, Points[0], RX, RY, ARot, Distance);
end;

function TEllipsePiece.IsPointInside(const P: TPoint2D;
  const Precision: TRealType): Boolean;
begin
  Result := IsPointInEllipse(P, Points[0], RX, RY, ARot);
end;

// =====================================================================
// TTextPiece
// =====================================================================

procedure TTextPiece.Linearize(const PP: TPointsSet2D;
  const Precision: TRealType);
var
  Rect: TRect2D;
  P: TPoint2D;
  Size: TVector2D;
begin
  MeasureTextRectangle(P, Size);
  LinearizeRectangle(PP, P, Size.X, Size.Y, ARot);
end;

procedure TTextPiece.MeasureTextRectangle(
  var P: TPoint2D; out Size: TVector2D);
var
  V: TVector2D;
begin
  GetTextDimension(WideText, FaceName, Style, Charset,
    Size.X, V.Y);
  Size.X := Size.X * Height;
  Size.Y := Height;
  case HAlignment of
    ahLeft: V.X := 0;
    ahCenter: V.X := -Size.X / 2;
    ahRight: V.X := -Size.X;
  end;
//    jvBaseline: V.Y := Text_Metric.tmDescent / TmpH;
  V.Y := -V.Y * Height;
  P := ShiftPoint(Points[0],
    TransformVector2D(V, Rotate2D(ARot)));
end;

function TTextPiece.GetBoundingBox: TRect2D;
var
  P: TPoint2D;
  Size: TVector2D;
begin
  MeasureTextRectangle(P, Size);
  Result := RectangleBoundingBox(P, Size.X, Size.Y, ARot);
end;

function TTextPiece.IsPointOnStroke(
  const P: TPoint2D; const Precision: TRealType;
  var Distance: TRealType;
  out Pos: Integer): Boolean;
begin
  Result := False;
end;

function TTextPiece.IsPointInside(const P: TPoint2D;
  const Precision: TRealType): Boolean;
var
  P0: TPoint2D;
  Size: TVector2D;
  V: TVector2D;
begin
  MeasureTextRectangle(P0, Size);
  V := TransformVector2D(Vector2D(P0, P),
    Rotate2D(-ARot));
  Result := (V.X >= 0) and (V.Y >= 0)
    and (V.X <= Size.X) and (V.Y <= Size.Y);
end;

// =====================================================================
// TBitmapPiece
// =====================================================================

constructor TBitmapPiece.Create(const _Capacity: Integer);
begin
  inherited Create(_Capacity);
  BitmapEntry := nil;
end;

procedure TBitmapPiece.Linearize(const PP: TPointsSet2D;
  const Precision: TRealType);
begin
  LinearizeRectangle(PP, Points[0], Width, Height, 0);
end;

function TBitmapPiece.GetBoundingBox: TRect2D;
begin
  Result := RectangleBoundingBox(Points[0], Width, Height, 0);
end;

function TBitmapPiece.IsPointOnStroke(
  const P: TPoint2D; const Precision: TRealType;
  var Distance: TRealType;
  out Pos: Integer): Boolean;
begin
  Result := False;
end;

function TBitmapPiece.IsPointInside(const P: TPoint2D;
  const Precision: TRealType): Boolean;
var
  V: TVector2D;
begin
  V := Vector2D(Points[0], P);
  Result := (V.X >= 0) and (V.Y >= 0)
    and (V.X <= Width) and (V.Y <= Height);
end;

// =====================================================================
// TGenPathPiece
// =====================================================================

constructor TGenPathPiece.Create(const _Path: TGenericPath);
begin
  inherited Create(0);
  PGenPath := _Path;
end;

destructor TGenPathPiece.Destroy;
begin
  inherited Destroy;
end;

procedure TGenPathPiece.GetBoundingBox0(var Rect: TRect2D);
begin
  PGenPath.GetBoundingBox0(Rect);
end;

function TGenPathPiece.IsPointOnStroke(
  const P: TPoint2D; const Precision: TRealType;
  var Distance: TRealType;
  out Pos: Integer): Boolean;
begin
  Result := PGenPath.IsPointOnStroke(P, Precision, Distance, Pos);
end;

function TGenPathPiece.IsPointInside(const P: TPoint2D;
  const Precision: TRealType): Boolean;
begin
  Result := PGenPath.IsPointInside(P, wn_NonZero, Precision);
end;

// =====================================================================
// TPiece_CreateFromSvg
// =====================================================================

procedure TPiece_CreateFromSvg.PathOnClose(const X, Y: TRealType);
begin
  //fPiece.Add(Point2D(X, Y));
  //fPiece.Fill :=
  fPiece.Closed := True;
  if fPiece is TPolyLinPiece then
  else
  begin
//    fPiece.Add(Point2D(X, Y));
//    fPiece.Add(Point2D(X, Y));
  end;
end;

procedure TPiece_CreateFromSvg.PathOnMoveTo(const X, Y: TRealType);
begin
  fPiece.Add(Point2D(X, Y));
end;

procedure TPiece_CreateFromSvg.PathOnLineTo(const X, Y: TRealType);
begin
  if fPiece is TPolyLinPiece then
    fPiece.Add(Point2D(X, Y))
  else
  begin
    fPiece.Add(
      MixPoint(fPiece[fPiece.Count - 1], Point2D(X, Y), 0.25));
    fPiece.Add(
      MixPoint(fPiece[fPiece.Count - 2], Point2D(X, Y), 0.75));
    fPiece.Add(Point2D(X, Y));
  end;
end;

procedure TPiece_CreateFromSvg.PathOnBezierTo(
  const X1, Y1, X2, Y2, X3, Y3: TRealType);
begin
  fPiece.Add(Point2D(X1, Y1));
  fPiece.Add(Point2D(X2, Y2));
  fPiece.Add(Point2D(X3, Y3));
end;

function TPiece_CreateFromSvg.CreateFromSvgPath(const SvgPath:
  string): TPiece;
var
  PathParser: T_SVG_Path_Parser;
begin
  fIsBezier := SVG_Path_HasCurves(SvgPath);
  if fIsBezier then
    Result := TBezierPiece.Create(0)
  else
    Result := TPolyLinPiece.Create(0);
  fPiece := Result;
  PathParser := T_SVG_Path_Parser.Create(
    PathOnClose, PathOnMoveTo, PathOnLineTo, PathOnBezierTo);
  try
    //?PathParser.AllAsBezier := fIsBezier;
    PathParser.Parse(SvgPath);
  finally
    PathParser.Free;
  end;
end;

// =====================================================================
// TArrayOfPiece
// =====================================================================

constructor TArrayOfPiece.Create(const Capacity: Integer);
begin
  inherited Create;
  Self.Capacity := Capacity;
end;

function TArrayOfPiece.AddFromSvgPath(const SvgPath: string):
  TPiece;
var
  FromSvg: TPiece_CreateFromSvg;
begin
  FromSvg := TPiece_CreateFromSvg.Create(20);
  try
    Result := FromSvg.CreateFromSvgPath(SvgPath);
    Add(Result);
  finally
    FromSvg.Free;
  end;
end;

function TArrayOfPiece.AddFromSvgPathT(const SvgPath: string;
  const T: TTransf2D): TPiece;
begin
  Result := AddFromSvgPath(SvgPath);
  Result.TransformPoints(T);
end;

function TArrayOfPiece.GetItem(I: Integer): TPiece;
begin
  Result := inherited GetItem(I) as TPiece;
end;

procedure TArrayOfPiece.TransForm(const T: TTransf2D);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    GetItem(I).TransformPoints(T);
end;

procedure TArrayOfPiece.GetBoundingBox0(var R: TRect2D);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    GetItem(I).GetBoundingBox0(R);
end;

function TArrayOfPiece.GetBoundingBox: TRect2D;
begin
  Result := Rect2D(
    MaxRealType, MaxRealType, -MaxRealType, -MaxRealType);
  GetBoundingBox0(Result);
end;

function TArrayOfPiece.IsPointOnStroke(
  Obj: TObject;
  const P: TPoint2D; const Precision: TRealType;
  var Distance: TRealType;
  out Pos: Integer): Boolean;
var
  I: Integer;
  D: TRealType;
begin
  D := Distance;
  Result := False;
  for I := 0 to Count - 1 do
    //if GetItem(I).GetLineStyle(Obj) <> liNone then
    Result := Result or
      GetItem(I).IsPointOnStroke(P, Precision, D, Pos);
  if D < Distance then Distance := D;
end;

function TArrayOfPiece.IsPointInside(
  Obj: TObject; const P: TPoint2D;
  const Precision: TRealType): Boolean;
var
  I: Integer;
  Piece: TPiece;
begin
  Result := True;
  for I := 0 to Count - 1 do
  begin
    Piece := GetItem(I);
    if (Piece.GetFillColor(Obj) <> clDefault)
      or (Piece.GetHatching(Obj) <> haNone)
      or (Piece is TTextPiece) or (Piece is TBitmapPiece) then
      if Piece.IsPointInside(P, Precision) then Exit;
  end;
  Result := False;
end;

end.


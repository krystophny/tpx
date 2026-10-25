unit TikZImport;

{$mode objfpc}{$H+}

interface

uses SysUtils, Classes, Math, TikZLexer, TikZSyntax, TikZFading;

type
  TTikZSemanticOutcome = (tsoAccepted, tsoUnsupported, tsoInvalid,
    tsoCancelled);
  TTikZSceneObjectKind = (tsoPath, tsoCircle, tsoEllipse, tsoRectangle,
    tsoArc, tsoSector, tsoSegment, tsoText, tsoImage);
  TTikZPathCommandKind = (tpcMove, tpcLine, tpcCubic, tpcClose);
  TTikZLineKind = (tlSolid, tlDashed, tlDotted);
  TTikZGradientKind = (tgNone, tgLinear, tgRadial);
  TTikZDependencyKind = (tdLineWidth, tdTextSize, tdTextBaseline, tdDashSize,
    tdDotSize, tdStrokeColor, tdFillColor, tdTextColor, tdTextBody,
    tdCoordinateX, tdCoordinateY, tdWidth, tdHeight, tdRadius,
    tdRotation, tdStartAngle, tdEndAngle, tdTextAnchor, tdTextRotation,
    tdArrows, tdStrokeOpacity, tdFillOpacity, tdOpacity, tdFillPaint);
  TTikZDependencySource = (tdepLocalLiteral, tdepSharedDefaultMacro,
    tdepLocalStyle, tdepNamedColor, tdepNamedCoordinate, tdepUnsupported);

  TTikZPoint = record
    X, Y: Double;
  end;
  TTikZAffineTransform = record
    XX, XY, YX, YY, TX, TY: Double;
  end;
  TTikZPathCommand = record
    Kind: TTikZPathCommandKind;
    P1, P2, P3: TTikZPoint;
    P1Span, P2Span, P3Span: TTikZSourceSpan;
    P1XSpan, P1YSpan, P2XSpan, P2YSpan, P3XSpan, P3YSpan:
      TTikZSourceSpan;
  end;
  TTikZPathCommands = array of TTikZPathCommand;

  TTikZPropertyDependency = record
    Kind: TTikZDependencyKind;
    DependencySource: TTikZDependencySource;
    DependencyName: string;
    ValueSpan: TTikZSourceSpan;
    PropertyKeySpan: TTikZSourceSpan;
    OptionListSpan: TTikZSourceSpan;
    DefinitionSpan: TTikZSourceSpan;
    HasLocalOverride: Boolean;
    CommandIndex, PointSlot: SizeInt;
    ValueScaleToMM: Double;
    ValueUnit: string;
    IsRelative, UpdatesRelativeBase: Boolean;
    ScopeTransform: TTikZAffineTransform;
  end;
  TTikZPropertyDependencies = array of TTikZPropertyDependency;

  TTikZSceneObject = class
  public
    Kind: TTikZSceneObjectKind;
    SourceSpan: TTikZSourceSpan;
    Commands: TTikZPathCommands;
    Center: TTikZPoint;
    Origin, RotationCenter: TTikZPoint;
    SourceFirst, SourceSecond: TTikZPoint;
    SourceRotationCenter: TTikZPoint;
    SourceRotation: Double;
    ScopeTransform: TTikZAffineTransform;
    RadiusX, RadiusY: Double;
    WidthMM, HeightMM: Double;
    StartAngle, EndAngle, Rotation: Double;
    StrokeEnabled, FillEnabled: Boolean;
    StrokeRGB, FillRGB: LongWord;
    { 'draw opacity', 'fill opacity' and 'opacity'; 1 when absent. }
    StrokeOpacity, FillOpacity: Double;
    { Axis or radial shading from FillRGB to GradientRGB; GradientAngle is
      the direction from start to end in degrees (TPaintStyle layout). }
    Gradient: TTikZGradientKind;
    GradientRGB: LongWord;
    GradientAngle: Double;
    { End opacity of the gradient: FillOpacity unless a TpX fading gives
      the two ends different opacities. }
    GradientOpacity: Double;
    { Source names of the start (fill) and end colours, '' when the fill
      colour comes from the command alone. }
    FillColorName, GradientColorName: string;
    { 'path fading' naming a TpX fading declaration (see
      TTikZFadingDeclaration); FillOpacity and GradientOpacity then come
      from that declaration. A linear fading carries its literal
      'fading transform' (shift in mm, rotation in degrees, scales of the
      50bp ramp), which materialization checks against the gradient box. }
    FadingName: string;
    FadingDeclaration: SizeInt;
    FadingShift: TTikZPoint;
    FadingRotation, FadingXScale, FadingYScale: Double;
    { Scene index of the stroke-only statement that TpX writes right after
      a faded fill, because the fading would also fade the stroke; the two
      statements form one native object. StrokeOf is the reverse link. }
    StrokeCompanion, StrokeOf: SizeInt;
    { Colour names defined between a faded fill and its stroke companion
      (set on the companion). }
    ColorsSinceFill: string;
    LineWidthMM, DashOnMM, DashOffMM: Double;
    LineKind: TTikZLineKind;
    TextBody, TextContent, ImageReference: RawByteString;
    TextPayloadSpan: TTikZSourceSpan;
    TextHeightMM, TextBaselineMM: Double;
    TextAnchor: string;
    ImageKeepAspectRatio: Boolean;
    { Normalized arrow tips ('', 'to', 'latex' or 'stealth'). }
    BeginArrow, EndArrow: string;
    Dependencies: TTikZPropertyDependencies;
    constructor Create;
  end;

  TTikZMacroDefinition = record
    Name: string;
    ValueMM: Double;
    SourceSpan: TTikZSourceSpan;
    ExpressionSpan: TTikZSourceSpan;
  end;
  TTikZMacroDefinitions = array of TTikZMacroDefinition;

  { A fading declaration in the form TpX writes:
    \pgfinterruptpicture<shading><fading>\endpgfinterruptpicture. Span
    covers the declaration with its trailing comment and line break. }
  TTikZFadingDeclaration = record
    Name: string;
    Radial: Boolean;
    StartOpacity, EndOpacity: Double;
    Span: TTikZSourceSpan;
  end;
  TTikZFadingDeclarations = array of TTikZFadingDeclaration;

  TTikZSourceBinding = record
    ObjectIndex: SizeInt;
    NativeObjectID: Integer;
    StatementSpan: TTikZSourceSpan;
    Dependencies: TTikZPropertyDependencies;
  end;
  TTikZSourceBindings = array of TTikZSourceBinding;

  TTikZScene = class
  private
    FObjects: TList;
    FBindings: TTikZSourceBindings;
  public
    Source: TBytes;
    PictureScaleX, PictureScaleY: Double;
    PictureShearX, PictureShearY: Double;
    MiterLimit: Double;
    LineWidthBaseMM, TextSizeMM, DashSizeMM, DotSizeMM: Double;
    HasOwnedBounds: Boolean;
    OwnedBoundsSpan: TTikZSourceSpan;
    OwnedBoundsOrigin: TTikZPoint;
    OwnedBoundsWidthMM, OwnedBoundsHeightMM: Double;
    MacroDefinitions: TTikZMacroDefinitions;
    Fadings: TTikZFadingDeclarations;
    constructor Create;
    function FindFading(const Name: string): SizeInt;
    destructor Destroy; override;
    function ObjectCount: SizeInt;
    function ObjectAt(Index: SizeInt): TTikZSceneObject;
    procedure AddObject(Obj: TTikZSceneObject);
    procedure AddBinding(const Binding: TTikZSourceBinding);
    function BindingCount: SizeInt;
    function BindingAt(Index: SizeInt): TTikZSourceBinding;
  end;

  // A '% tpx-solid' block whose statements render one TpX 3D object.
  TTikZSolidRegion = record
    Span: TTikZSourceSpan;
    NativeObjectID: Integer;
    FirstBinding, LastBinding: SizeInt;
  end;

  TTikZImportContext = class
  private
    FDocument: TTikZSyntaxResult;
    FScene: TTikZScene;
    FSource: TBytes;
    FSolidRegions: array of TTikZSolidRegion;
  public
    procedure AddSolidRegion(const Region: TTikZSolidRegion);
    function SolidRegionCount: SizeInt;
    function SolidRegionAt(Index: SizeInt): TTikZSolidRegion;
    procedure SetSolidRegionObject(Index: SizeInt; ObjectID: Integer);
    function FindSolidRegionForObjectID(ObjectID: Integer): SizeInt;
    function SolidRegionOfBinding(BindingIndex: SizeInt): SizeInt;
    constructor Create(Document: TTikZSyntaxResult; Scene: TTikZScene);
    destructor Destroy; override;
    function BindingCount: SizeInt;
    function BindingAt(Index: SizeInt): TTikZSourceBinding;
    function FindBindingForObjectID(ObjectID: Integer): SizeInt;
    procedure BindNativeObject(ObjectIndex: SizeInt; ObjectID: Integer);
    procedure UnbindNativeObject(ObjectID: Integer);
    function CopySource: TBytes;
    property Scene: TTikZScene read FScene;
    property Document: TTikZSyntaxResult read FDocument;
  end;

  TTikZImportDiagnostic = record
    Code: TTikZDiagnosticCode;
    Severity: TTikZSeverity;
    Span: TTikZSourceSpan;
    Message: string;
  end;
  TTikZImportDiagnostics = array of TTikZImportDiagnostic;

  TTikZImportCancelCheck = function(Context: Pointer): Boolean;
  TTikZImportOptions = record
    MaxObjects, MaxPathCommands, MaxEvaluationSteps: SizeInt;
    CancelCheck: TTikZImportCancelCheck;
    CancelContext: Pointer;
  end;

  TTikZSemanticResult = class
  public
    Outcome: TTikZSemanticOutcome;
    Scene: TTikZScene;
    Diagnostics: TTikZImportDiagnostics;
    destructor Destroy; override;
    function DetachScene: TTikZScene;
  end;

{ Fading transform of a linear TpX fading for a gradient from P1 to P2 (mm)
  on a box with diagonal D (mm): its 50bp ramp along P1-P2, scaled to the
  gradient length and to D across. }
procedure TikZLinearFadingTransform(const P1, P2: TTikZPoint; D: Double;
  out Shift: TTikZPoint; out Rotation, XScale, YScale: Double);
{ True when a fading transform matches another within the precision TpX
  writes (coordinates to 0.01 picture units, six significant digits). }
function SameTikZFadingTransform(const ShiftA: TTikZPoint;
  RotationA, XScaleA, YScaleA: Double; const ShiftB: TTikZPoint;
  RotationB, XScaleB, YScaleB, UnitMM: Double): Boolean;

function DefaultTikZImportOptions: TTikZImportOptions;
function EvaluateTikZ(const Syntax: TTikZSyntaxResult;
  const Options: TTikZImportOptions): TTikZSemanticResult;

implementation

type
  TTokenIndexArray = array of SizeInt;
  TTikZTransformStack = array of TTikZAffineTransform;
  TTikZOptionValue = record
    Key, Value: string;
    KeySpan, ValueSpan, OptionSpan: TTikZSourceSpan;
    HasValue: Boolean;
  end;
  TTikZOptionValues = array of TTikZOptionValue;
  { The opacity of the enclosing scopes, 1 without any, and the innermost
    scope option that set it. }
  TTikZScopeOpacity = record
    Opacity: Double;
    Option: TTikZOptionValue;
  end;
  TTikZScopeOpacityStack = array of TTikZScopeOpacity;

  TTikZEvaluator = class
  private
    FDocument: TTikZSyntaxResult;
    FOptions: TTikZImportOptions;
    FResult: TTikZSemanticResult;
    FSource: TBytes;
    FOpenGroups: TTokenIndexArray;
    FColors: TStringList;
    FColorSpans: TStringList;
    FCurrentTransform: TTikZAffineTransform;
    FScopeOpacity: TTikZScopeOpacity;
    FEvaluationSteps, FPathCommandCount: SizeInt;
    function TokenText(Index: SizeInt): string;
    function SourceText(const Span: TTikZSourceSpan): RawByteString;
    function NextSignificant(Index, Limit: SizeInt): SizeInt;
    function GroupAtOpen(Index: SizeInt): SizeInt;
    function TokenSpan(Index: SizeInt): TTikZSourceSpan;
    function SpanBetween(First, PastLast: SizeInt): TTikZSourceSpan;
    function Tick(const Span: TTikZSourceSpan): Boolean;
    procedure AddDiagnostic(Code: TTikZDiagnosticCode;
      Severity: TTikZSeverity; const Span: TTikZSourceSpan;
      const Message: string);
    procedure Unsupported(const Span: TTikZSourceSpan;
      const Message: string);
    procedure Invalid(const Span: TTikZSourceSpan;
      const Message: string);
    procedure EvaluatePreamble;
    procedure EvaluatePictureHeader;
    procedure EvaluatePicture;
    function TryReadBraceGroup(var Index: SizeInt; Limit: SizeInt;
      out GroupIndex: SizeInt; out Content: RawByteString;
      out ContentSpan: TTikZSourceSpan): Boolean;
    procedure ParseColorDefinition(var Index: SizeInt; Limit: SizeInt);
    procedure AddDependency(Obj: TTikZSceneObject; Kind: TTikZDependencyKind;
      DependencySource: TTikZDependencySource; const DependencyName: string;
      const ValueSpan, KeySpan, OptionSpan, DefinitionSpan: TTikZSourceSpan;
      HasLocalOverride: Boolean; CommandIndex, PointSlot: SizeInt;
      ValueScaleToMM: Double; const ValueUnit: string; IsRelative: Boolean);
    procedure AddSemanticObject(Obj: TTikZSceneObject;
      const StatementSpan: TTikZSourceSpan);
    function ReadGroup(var Index: SizeInt; Kind: TTikZGroupKind;
      out GroupIndex: SizeInt): Boolean;
    function TryReadMeasure(var Index: SizeInt; out Value: Double;
      out MeasureUnit: string; out ValueSpan: TTikZSourceSpan): Boolean;
    function TryReadCoordinate(var Index: SizeInt; Limit: SizeInt;
      const RelativeBase: TTikZPoint; out Point: TTikZPoint;
      out UpdateRelativeBase: Boolean; out PointSpan, XSpan,
      YSpan: TTikZSourceSpan; out IsRelative: Boolean;
      out XScaleToMM, YScaleToMM: Double; out XUnit, YUnit: string): Boolean;
    function ParseOptionGroup(GroupIndex: SizeInt): TTikZOptionValues;
    function ParseFadingDeclaration(Index, Limit: SizeInt): SizeInt;
    function ParseFadingTransform(const Text: string; Obj: TTikZSceneObject): Boolean;
    procedure LinkStrokeCompanion(FillIndex: SizeInt; const DefinedColors: string);
    function FindOption(const Values: TTikZOptionValues;
      const Key: string): SizeInt;
    function ParseRGB(const Name: string; out RGB: LongWord): Boolean;
    function ParseLengthExpression(const Text: string;
      out ValueMM: Double; out Dependency: string): Boolean;
    procedure ParseStatement(CommandIndex, Limit: SizeInt;
      const Command: string);
  public
    constructor Create(Document: TTikZSyntaxResult;
      const Options: TTikZImportOptions);
    destructor Destroy; override;
    procedure Run;
    property ImportResult: TTikZSemanticResult read FResult;
  end;

function ParseInvariantFloat(const Text: string; out Value: Double): Boolean; forward;
function InvariantFloatText(Value: Double): string; forward;
function TrimASCII(const Value: string): string; forward;
function LowerASCII(const Value: string): string; forward;

function BytesToRawString(const Bytes: TBytes): RawByteString;
begin
  Result := '';
  if Length(Bytes) > 0 then
    SetString(Result, PAnsiChar(@Bytes[0]), Length(Bytes));
end;

function SpanAtZero: TTikZSourceSpan;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

function IdentityTikZTransform: TTikZAffineTransform;
begin
  Result.XX := 1;
  Result.XY := 0;
  Result.YX := 0;
  Result.YY := 1;
  Result.TX := 0;
  Result.TY := 0;
end;

function ComposeTikZTransform(const A, B: TTikZAffineTransform):
  TTikZAffineTransform;
begin
  Result.XX := A.XX * B.XX + A.XY * B.YX;
  Result.XY := A.XX * B.XY + A.XY * B.YY;
  Result.YX := A.YX * B.XX + A.YY * B.YX;
  Result.YY := A.YX * B.XY + A.YY * B.YY;
  Result.TX := A.XX * B.TX + A.XY * B.TY + A.TX;
  Result.TY := A.YX * B.TX + A.YY * B.TY + A.TY;
end;

function TransformTikZPoint(const T: TTikZAffineTransform;
  const P: TTikZPoint): TTikZPoint;
begin
  Result.X := T.XX * P.X + T.XY * P.Y + T.TX;
  Result.Y := T.YX * P.X + T.YY * P.Y + T.TY;
end;

function TransformTikZVector(const T: TTikZAffineTransform;
  const P: TTikZPoint): TTikZPoint;
begin
  Result.X := T.XX * P.X + T.XY * P.Y;
  Result.Y := T.YX * P.X + T.YY * P.Y;
end;

function VectorLength(const P: TTikZPoint): Double;
begin
  Result := Sqrt(Sqr(P.X) + Sqr(P.Y));
end;

function AreOrthogonal(const A, B: TTikZPoint): Boolean;
var
  Scale: Double;
begin
  Scale := VectorLength(A) * VectorLength(B);
  Result := Abs(A.X * B.X + A.Y * B.Y) <= 1E-10 * Max(1, Scale);
end;

constructor TTikZScene.Create;
begin
  inherited Create;
  FObjects := TList.Create;
  PictureScaleX := 10;
  PictureScaleY := 10;
  PictureShearX := 0;
  PictureShearY := 0;
  MiterLimit := 10;
  LineWidthBaseMM := 0.4 * 25.4 / 72.27;
  TextSizeMM := 10 * 25.4 / 72.27;
  DashSizeMM := 3 * 25.4 / 72.27;
  DotSizeMM := 0.4 * 25.4 / 72.27;
end;

destructor TTikZScene.Destroy;
var
  I: SizeInt;
begin
  for I := 0 to FObjects.Count - 1 do TObject(FObjects[I]).Free;
  FObjects.Free;
  inherited Destroy;
end;

constructor TTikZSceneObject.Create;
begin
  inherited Create;
  StrokeOpacity := 1;
  FillOpacity := 1;
  GradientOpacity := 1;
  FadingDeclaration := -1;
  StrokeCompanion := -1;
  StrokeOf := -1;
end;

function TTikZScene.FindFading(const Name: string): SizeInt;
var
  I: SizeInt;
begin
  for I := 0 to High(Fadings) do
    if Fadings[I].Name = Name then Exit(I);
  Result := -1;
end;

procedure TikZLinearFadingTransform(const P1, P2: TTikZPoint; D: Double;
  out Shift: TTikZPoint; out Rotation, XScale, YScale: Double);
const
  BPPerMM = 72 / 25.4;
begin
  Shift.X := (P1.X + P2.X) / 2;
  Shift.Y := (P1.Y + P2.Y) / 2;
  Rotation := ArcTan2(P2.Y - P1.Y, P2.X - P1.X) * 180 / Pi;
  XScale := Sqrt(Sqr(P2.X - P1.X) + Sqr(P2.Y - P1.Y)) * BPPerMM / 50;
  YScale := D * BPPerMM / 50;
end;

function SameTikZFadingTransform(const ShiftA: TTikZPoint;
  RotationA, XScaleA, YScaleA: Double; const ShiftB: TTikZPoint;
  RotationB, XScaleB, YScaleB, UnitMM: Double): Boolean;
var
  Tolerance, ScaleTolerance, Turn: Double;
begin
  { Coordinates are written to 0.01 picture units, so the box behind the
    shift and the scales may move by that much. }
  Tolerance := 0.011 * Abs(UnitMM) + 1E-6;
  ScaleTolerance := 2 * Tolerance * 72 / 25.4 / 50;
  Turn := RotationA - RotationB;
  Turn := Turn - 360 * Round(Turn / 360);
  Result := (Abs(ShiftA.X - ShiftB.X) <= Tolerance) and
    (Abs(ShiftA.Y - ShiftB.Y) <= Tolerance) and (Abs(Turn) <= 0.01) and
    (Abs(XScaleA - XScaleB) <= ScaleTolerance + 1E-5 * Abs(XScaleB)) and
    (Abs(YScaleA - YScaleB) <= ScaleTolerance + 1E-5 * Abs(YScaleB));
end;

function TTikZScene.ObjectCount: SizeInt;
begin
  Result := FObjects.Count;
end;

function TTikZScene.ObjectAt(Index: SizeInt): TTikZSceneObject;
begin
  Result := TTikZSceneObject(FObjects[Index]);
end;

procedure TTikZScene.AddObject(Obj: TTikZSceneObject);
begin
  FObjects.Add(Obj);
end;

procedure TTikZScene.AddBinding(const Binding: TTikZSourceBinding);
var
  N: SizeInt;
begin
  N := Length(FBindings);
  SetLength(FBindings, N + 1);
  FBindings[N] := Binding;
end;

function TTikZScene.BindingCount: SizeInt;
begin
  Result := Length(FBindings);
end;

function TTikZScene.BindingAt(Index: SizeInt): TTikZSourceBinding;
begin
  Result := FBindings[Index];
end;

destructor TTikZSemanticResult.Destroy;
begin
  Scene.Free;
  inherited Destroy;
end;

function TTikZSemanticResult.DetachScene: TTikZScene;
begin
  Result := Scene;
  Scene := nil;
end;

constructor TTikZImportContext.Create(Document: TTikZSyntaxResult;
  Scene: TTikZScene);
begin
  inherited Create;
  FDocument := Document;
  FScene := Scene;
  if Document <> nil then FSource := Document.CopySource;
end;

destructor TTikZImportContext.Destroy;
begin
  FScene.Free;
  FDocument.Free;
  inherited Destroy;
end;

function TTikZImportContext.BindingCount: SizeInt;
begin
  Result := FScene.BindingCount;
end;

function TTikZImportContext.BindingAt(Index: SizeInt): TTikZSourceBinding;
begin
  Result := FScene.BindingAt(Index);
end;

procedure TTikZImportContext.AddSolidRegion(const Region: TTikZSolidRegion);
begin
  SetLength(FSolidRegions, Length(FSolidRegions) + 1);
  FSolidRegions[High(FSolidRegions)] := Region;
end;

function TTikZImportContext.SolidRegionCount: SizeInt;
begin
  Result := Length(FSolidRegions);
end;

function TTikZImportContext.SolidRegionAt(Index: SizeInt): TTikZSolidRegion;
begin
  if (Index < 0) or (Index > High(FSolidRegions)) then
    raise ERangeError.Create('TikZ solid region index out of range');
  Result := FSolidRegions[Index];
end;

procedure TTikZImportContext.SetSolidRegionObject(Index: SizeInt;
  ObjectID: Integer);
begin
  if (Index < 0) or (Index > High(FSolidRegions)) then
    raise ERangeError.Create('TikZ solid region index out of range');
  FSolidRegions[Index].NativeObjectID := ObjectID;
end;

function TTikZImportContext.FindSolidRegionForObjectID(ObjectID: Integer): SizeInt;
var
  I: SizeInt;
begin
  Result := -1;
  for I := 0 to High(FSolidRegions) do
    if FSolidRegions[I].NativeObjectID = ObjectID then Exit(I);
end;

function TTikZImportContext.SolidRegionOfBinding(BindingIndex: SizeInt): SizeInt;
var
  I: SizeInt;
begin
  Result := -1;
  for I := 0 to High(FSolidRegions) do
    if (BindingIndex >= FSolidRegions[I].FirstBinding) and
      (BindingIndex <= FSolidRegions[I].LastBinding) then Exit(I);
end;

function TTikZImportContext.FindBindingForObjectID(ObjectID: Integer): SizeInt;
var
  I: SizeInt;
  Binding: TTikZSourceBinding;
begin
  Result := -1;
  for I := 0 to BindingCount - 1 do
  begin
    Binding := BindingAt(I);
    if Binding.NativeObjectID = ObjectID then Exit(I);
  end;
end;

procedure TTikZImportContext.BindNativeObject(ObjectIndex: SizeInt;
  ObjectID: Integer);
var
  I: SizeInt;
  Binding: TTikZSourceBinding;
begin
  for I := 0 to BindingCount - 1 do
  begin
    Binding := BindingAt(I);
    if Binding.ObjectIndex = ObjectIndex then
    begin
      Binding.NativeObjectID := ObjectID;
      FScene.FBindings[I] := Binding;
      Exit;
    end;
  end;
end;

procedure TTikZImportContext.UnbindNativeObject(ObjectID: Integer);
var
  I: SizeInt;
  Binding: TTikZSourceBinding;
begin
  for I := 0 to BindingCount - 1 do
  begin
    Binding := BindingAt(I);
    if Binding.NativeObjectID = ObjectID then
    begin
      Binding.NativeObjectID := -1;
      FScene.FBindings[I] := Binding;
    end;
  end;
end;

function TTikZImportContext.CopySource: TBytes;
var
  I: SizeInt;
begin
  Result := nil;
  SetLength(Result, Length(FSource));
  for I := 0 to High(FSource) do Result[I] := FSource[I];
end;

function DefaultTikZImportOptions: TTikZImportOptions;
begin
  Result.MaxObjects := 100000;
  Result.MaxPathCommands := 1000000;
  Result.MaxEvaluationSteps := 4000000;
  Result.CancelCheck := nil;
  Result.CancelContext := nil;
end;

constructor TTikZEvaluator.Create(Document: TTikZSyntaxResult;
  const Options: TTikZImportOptions);
var
  I: SizeInt;
  Group: TTikZGroup;
begin
  inherited Create;
  FDocument := Document;
  FOptions := Options;
  FResult := TTikZSemanticResult.Create;
  FResult.Outcome := tsoInvalid;
  FResult.Scene := TTikZScene.Create;
  FSource := Document.CopySource;
  FColors := TStringList.Create;
  FColors.CaseSensitive := True;
  FColors.NameValueSeparator := '=';
  FColorSpans := TStringList.Create;
  FColorSpans.CaseSensitive := True;
  FColorSpans.NameValueSeparator := '=';
  if Document <> nil then
  begin
    SetLength(FOpenGroups, Document.TokenCount);
    for I := 0 to Document.TokenCount - 1 do FOpenGroups[I] := -1;
    for I := 0 to Document.GroupCount - 1 do
    begin
      Group := Document.GroupAt(I);
      if (Group.OpenToken >= 0) and (Group.OpenToken < Length(FOpenGroups)) then
        FOpenGroups[Group.OpenToken] := I;
    end;
  end;
end;

destructor TTikZEvaluator.Destroy;
begin
  FColorSpans.Free;
  FColors.Free;
  FResult.Free;
  inherited Destroy;
end;

function TTikZEvaluator.TokenText(Index: SizeInt): string;
begin
  if (Index < 0) or (Index >= FDocument.TokenCount) then Exit('');
  Result := string(SourceText(TokenSpan(Index)));
end;

function TTikZEvaluator.SourceText(const Span: TTikZSourceSpan): RawByteString;
begin
  Result := BytesToRawString(FDocument.SourceSlice(Span));
end;

function TTikZEvaluator.NextSignificant(Index, Limit: SizeInt): SizeInt;
var
  Tok: TTikZToken;
begin
  Result := Index;
  while Result < Limit do
  begin
    Tok := FDocument.TokenAt(Result);
    if not (Tok.Kind in [ttkWhitespace, ttkComment]) then Break;
    Inc(Result);
  end;
end;

function TTikZEvaluator.GroupAtOpen(Index: SizeInt): SizeInt;
var
  I: SizeInt;
  Group: TTikZGroup;
begin
  Result := -1;
  if FDocument.TokenAt(Index).MatchingToken < 0 then Exit;
  for I := 0 to FDocument.GroupCount - 1 do
  begin
    Group := FDocument.GroupAt(I);
    if Group.OpenToken = Index then Exit(I);
  end;
end;

function TTikZEvaluator.TokenSpan(Index: SizeInt): TTikZSourceSpan;
begin
  if (Index < 0) or (Index >= FDocument.TokenCount) then
    Result := SpanAtZero
  else
    Result := FDocument.TokenAt(Index).Span;
end;

function TTikZEvaluator.SpanBetween(First, PastLast: SizeInt): TTikZSourceSpan;
begin
  if PastLast <= First then Exit(SpanAtZero);
  Result := TokenSpan(First);
  Result.EndByte := TokenSpan(PastLast - 1).EndByte;
  Result.EndLine := TokenSpan(PastLast - 1).EndLine;
  Result.EndColumn := TokenSpan(PastLast - 1).EndColumn;
end;

function TTikZEvaluator.Tick(const Span: TTikZSourceSpan): Boolean;
begin
  Inc(FEvaluationSteps);
  if (FOptions.MaxEvaluationSteps > 0) and
    (FEvaluationSteps > FOptions.MaxEvaluationSteps) then
  begin
    Invalid(Span, 'TikZ semantic evaluation exceeds the step limit');
    Exit(False);
  end;
  Result := True;
  if (FEvaluationSteps mod 128 = 0) and
    (FOptions.CancelCheck <> nil) and
    FOptions.CancelCheck(FOptions.CancelContext) then
  begin
    FResult.Outcome := tsoCancelled;
    AddDiagnostic(tdcCancelled, tsError, Span, 'TikZ import was cancelled');
    Result := False;
  end;
end;

procedure TTikZEvaluator.AddDiagnostic(Code: TTikZDiagnosticCode;
  Severity: TTikZSeverity; const Span: TTikZSourceSpan;
  const Message: string);
var
  N: SizeInt;
begin
  N := Length(FResult.Diagnostics);
  SetLength(FResult.Diagnostics, N + 1);
  FResult.Diagnostics[N].Code := Code;
  FResult.Diagnostics[N].Severity := Severity;
  FResult.Diagnostics[N].Span := Span;
  FResult.Diagnostics[N].Message := Message;
end;

procedure TTikZEvaluator.Unsupported(const Span: TTikZSourceSpan;
  const Message: string);
begin
  if FResult.Outcome = tsoAccepted then FResult.Outcome := tsoUnsupported;
  AddDiagnostic(tdcUnsupportedCommand, tsError, Span, Message);
end;

procedure TTikZEvaluator.Invalid(const Span: TTikZSourceSpan;
  const Message: string);
begin
  if FResult.Outcome <> tsoCancelled then FResult.Outcome := tsoInvalid;
  AddDiagnostic(tdcMalformedWrapper, tsError, Span, Message);
end;

function TTikZEvaluator.ReadGroup(var Index: SizeInt; Kind: TTikZGroupKind;
  out GroupIndex: SizeInt): Boolean;
var
  Group: TTikZGroup;
begin
  Result := False;
  GroupIndex := -1;
  Index := NextSignificant(Index, FDocument.TokenCount);
  if Index >= FDocument.TokenCount then Exit;
  if FOpenGroups[Index] < 0 then Exit;
  GroupIndex := FOpenGroups[Index];
  Group := FDocument.GroupAt(GroupIndex);
  if Group.Kind <> Kind then Exit;
  Index := Group.CloseToken + 1;
  Result := True;
end;

function TTikZEvaluator.TryReadBraceGroup(var Index: SizeInt; Limit: SizeInt;
  out GroupIndex: SizeInt; out Content: RawByteString;
  out ContentSpan: TTikZSourceSpan): Boolean;
var
  Group: TTikZGroup;
begin
  Result := False;
  GroupIndex := -1;
  Content := '';
  ContentSpan := SpanAtZero;
  Index := NextSignificant(Index, Limit);
  if Index >= Limit then Exit;
  GroupIndex := FOpenGroups[Index];
  if GroupIndex < 0 then Exit;
  Group := FDocument.GroupAt(GroupIndex);
  if Group.Kind <> tgBrace then Exit;
  if Group.CloseToken > Group.OpenToken + 1 then
  begin
    ContentSpan := SpanBetween(Group.OpenToken + 1, Group.CloseToken);
    Content := SourceText(ContentSpan);
  end
  else
  begin
    ContentSpan := TokenSpan(Group.OpenToken);
    ContentSpan.StartByte := ContentSpan.EndByte;
    ContentSpan.StartColumn := ContentSpan.EndColumn;
  end;
  Index := Group.CloseToken + 1;
  Result := True;
end;

procedure TTikZEvaluator.AddDependency(Obj: TTikZSceneObject;
  Kind: TTikZDependencyKind; DependencySource: TTikZDependencySource;
  const DependencyName: string; const ValueSpan, KeySpan, OptionSpan,
  DefinitionSpan: TTikZSourceSpan; HasLocalOverride: Boolean;
  CommandIndex, PointSlot: SizeInt; ValueScaleToMM: Double;
  const ValueUnit: string; IsRelative: Boolean);
var
  N: SizeInt;
begin
  N := Length(Obj.Dependencies);
  SetLength(Obj.Dependencies, N + 1);
  Obj.Dependencies[N].Kind := Kind;
  Obj.Dependencies[N].DependencySource := DependencySource;
  Obj.Dependencies[N].DependencyName := DependencyName;
  Obj.Dependencies[N].ValueSpan := ValueSpan;
  Obj.Dependencies[N].PropertyKeySpan := KeySpan;
  Obj.Dependencies[N].OptionListSpan := OptionSpan;
  Obj.Dependencies[N].DefinitionSpan := DefinitionSpan;
  Obj.Dependencies[N].HasLocalOverride := HasLocalOverride;
  Obj.Dependencies[N].CommandIndex := CommandIndex;
  Obj.Dependencies[N].PointSlot := PointSlot;
  Obj.Dependencies[N].ValueScaleToMM := ValueScaleToMM;
  Obj.Dependencies[N].ValueUnit := ValueUnit;
  Obj.Dependencies[N].IsRelative := IsRelative;
  Obj.Dependencies[N].UpdatesRelativeBase := False;
end;

function HasOpacityDependency(Obj: TTikZSceneObject): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to High(Obj.Dependencies) do
    if Obj.Dependencies[I].Kind in [tdStrokeOpacity, tdFillOpacity,
      tdOpacity] then Exit(True);
  Result := False;
end;

procedure TTikZEvaluator.AddSemanticObject(Obj: TTikZSceneObject;
  const StatementSpan: TTikZSourceSpan);
var
  Binding: TTikZSourceBinding;
  I, N: SizeInt;
  P0, U, V, Center: TTikZPoint;
  Angle, LengthU, LengthV, Determinant: Double;
  TempCommands: TTikZPathCommands;
  Dep: TTikZPropertyDependency;
begin
  if (FOptions.MaxObjects > 0) and
    (FResult.Scene.ObjectCount >= FOptions.MaxObjects) then
  begin
    Obj.Free;
    Invalid(StatementSpan, 'TikZ import exceeds the object limit');
    Exit;
  end;
  if FScopeOpacity.Opacity < 1 then
  begin
    { TpX exports a translucent primitive with arrowheads as an opacity
      group that holds paths without opacities of their own. Its paths
      reopen with the group opacity, bound to the scope option, so that
      an opacity edit is refused instead of being written into the
      statement and multiplied with the group. }
    if (Obj.Kind in [tsoText, tsoImage]) or HasOpacityDependency(Obj) then
    begin
      Obj.Free;
      Unsupported(StatementSpan,
        'An opacity inside a translucent TikZ scope is not native');
      Exit;
    end;
    if Obj.StrokeEnabled then Obj.StrokeOpacity := FScopeOpacity.Opacity;
    if Obj.FillEnabled then
    begin
      Obj.FillOpacity := FScopeOpacity.Opacity;
      Obj.GradientOpacity := FScopeOpacity.Opacity;
    end;
    AddDependency(Obj, tdOpacity, tdepUnsupported, '',
      FScopeOpacity.Option.ValueSpan, FScopeOpacity.Option.KeySpan,
      FScopeOpacity.Option.OptionSpan, SpanAtZero, False, -1, -1, 1, '',
      False);
  end;
  Obj.ScopeTransform := FCurrentTransform;
  for I := 0 to High(Obj.Dependencies) do
  begin
    Dep := Obj.Dependencies[I];
    Dep.ScopeTransform := FCurrentTransform;
    Obj.Dependencies[I] := Dep;
  end;
  Determinant := FCurrentTransform.XX * FCurrentTransform.YY -
    FCurrentTransform.XY * FCurrentTransform.YX;
  case Obj.Kind of
    tsoPath:
      for I := 0 to High(Obj.Commands) do
        case Obj.Commands[I].Kind of
          tpcMove, tpcLine:
            Obj.Commands[I].P1 := TransformTikZPoint(FCurrentTransform,
              Obj.Commands[I].P1);
          tpcCubic:
            begin
              Obj.Commands[I].P1 := TransformTikZPoint(FCurrentTransform,
                Obj.Commands[I].P1);
              Obj.Commands[I].P2 := TransformTikZPoint(FCurrentTransform,
                Obj.Commands[I].P2);
              Obj.Commands[I].P3 := TransformTikZPoint(FCurrentTransform,
                Obj.Commands[I].P3);
            end;
        end;
    tsoRectangle:
      begin
        Angle := Obj.Rotation;
        P0 := Obj.Origin;
        if Angle <> 0 then
        begin
          U.X := P0.X - Obj.RotationCenter.X;
          U.Y := P0.Y - Obj.RotationCenter.Y;
          P0.X := Obj.RotationCenter.X + Cos(Angle) * U.X - Sin(Angle) * U.Y;
          P0.Y := Obj.RotationCenter.Y + Sin(Angle) * U.X + Cos(Angle) * U.Y;
        end;
        U.X := Obj.WidthMM * Cos(Angle);
        U.Y := Obj.WidthMM * Sin(Angle);
        V.X := -Obj.HeightMM * Sin(Angle);
        V.Y := Obj.HeightMM * Cos(Angle);
        P0 := TransformTikZPoint(FCurrentTransform, P0);
        U := TransformTikZVector(FCurrentTransform, U);
        V := TransformTikZVector(FCurrentTransform, V);
        LengthU := VectorLength(U);
        LengthV := VectorLength(V);
        if AreOrthogonal(U, V) then
        begin
          Obj.Origin := P0;
          Obj.Center := TransformTikZPoint(FCurrentTransform, Obj.Center);
          Obj.WidthMM := LengthU;
          Obj.HeightMM := LengthV;
          Obj.Rotation := ArcTan2(U.Y, U.X);
          Obj.RotationCenter := P0;
        end
        else
        begin
          SetLength(TempCommands, 5);
          FillChar(TempCommands[0], Length(TempCommands) *
            SizeOf(TTikZPathCommand), 0);
          TempCommands[0].Kind := tpcMove;
          TempCommands[0].P1 := P0;
          TempCommands[1].Kind := tpcLine;
          TempCommands[1].P1.X := P0.X + U.X;
          TempCommands[1].P1.Y := P0.Y + U.Y;
          TempCommands[2].Kind := tpcLine;
          TempCommands[2].P1.X := P0.X + U.X + V.X;
          TempCommands[2].P1.Y := P0.Y + U.Y + V.Y;
          TempCommands[3].Kind := tpcLine;
          TempCommands[3].P1.X := P0.X + V.X;
          TempCommands[3].P1.Y := P0.Y + V.Y;
          TempCommands[4].Kind := tpcClose;
          Obj.Commands := TempCommands;
          Obj.Kind := tsoPath;
        end;
      end;
    tsoCircle, tsoEllipse:
      begin
        Center := Obj.Center;
        Angle := Obj.Rotation;
        if (Obj.Kind = tsoEllipse) and (Angle <> 0) then
        begin
          U.X := Center.X - Obj.RotationCenter.X;
          U.Y := Center.Y - Obj.RotationCenter.Y;
          Center.X := Obj.RotationCenter.X + Cos(Angle) * U.X - Sin(Angle) * U.Y;
          Center.Y := Obj.RotationCenter.Y + Sin(Angle) * U.X + Cos(Angle) * U.Y;
        end;
        U.X := Obj.RadiusX * Cos(Angle);
        U.Y := Obj.RadiusX * Sin(Angle);
        V.X := -Obj.RadiusY * Sin(Angle);
        V.Y := Obj.RadiusY * Cos(Angle);
        Center := TransformTikZPoint(FCurrentTransform, Center);
        U := TransformTikZVector(FCurrentTransform, U);
        V := TransformTikZVector(FCurrentTransform, V);
        if not AreOrthogonal(U, V) then
        begin
          Obj.Free;
          Unsupported(StatementSpan,
            'Scope transform shears a circle or ellipse beyond native representation');
          Exit;
        end;
        Obj.Center := Center;
        Obj.RadiusX := VectorLength(U);
        Obj.RadiusY := VectorLength(V);
        Obj.Rotation := ArcTan2(U.Y, U.X);
        Obj.RotationCenter := Center;
        if (Obj.Kind = tsoCircle) and
          (Abs(Obj.RadiusX - Obj.RadiusY) <= 1E-10 * Max(1, Obj.RadiusX)) then
        begin
          Obj.RadiusX := (Obj.RadiusX + Obj.RadiusY) / 2;
          Obj.RadiusY := Obj.RadiusX;
        end
        else if Obj.Kind = tsoCircle then Obj.Kind := tsoEllipse;
      end;
    tsoArc, tsoSector, tsoSegment:
      begin
        Center := TransformTikZPoint(FCurrentTransform, Obj.Center);
        U.X := Obj.RadiusX;
        U.Y := 0;
        V.X := 0;
        V.Y := Obj.RadiusX;
        U := TransformTikZVector(FCurrentTransform, U);
        V := TransformTikZVector(FCurrentTransform, V);
        LengthU := VectorLength(U);
        LengthV := VectorLength(V);
        if (not AreOrthogonal(U, V)) or
          (Abs(LengthU - LengthV) > 1E-10 * Max(1, LengthU)) then
        begin
          Obj.Free;
          Unsupported(StatementSpan,
            'Scope transform shears or anisotropically scales a circular arc');
          Exit;
        end;
        Obj.Center := Center;
        Obj.RadiusX := (LengthU + LengthV) / 2;
        Obj.RadiusY := Obj.RadiusX;
        Angle := ArcTan2(U.Y, U.X);
        if Determinant >= 0 then
        begin
          Obj.StartAngle := Obj.StartAngle + Angle;
          Obj.EndAngle := Obj.EndAngle + Angle;
        end
        else
        begin
          Obj.StartAngle := Angle - Obj.StartAngle;
          Obj.EndAngle := Angle - Obj.EndAngle;
        end;
      end;
    tsoText, tsoImage:
      Obj.Center := TransformTikZPoint(FCurrentTransform, Obj.Center);
  end;
  Obj.SourceSpan := StatementSpan;
  Binding.ObjectIndex := FResult.Scene.ObjectCount;
  Binding.NativeObjectID := -1;
  Binding.StatementSpan := StatementSpan;
  Binding.Dependencies := Copy(Obj.Dependencies);
  FResult.Scene.AddObject(Obj);
  FResult.Scene.AddBinding(Binding);
end;

procedure TTikZEvaluator.ParseColorDefinition(var Index: SizeInt;
  Limit: SizeInt);
var
  StartIndex, I, N: SizeInt;
  GroupIndex: SizeInt;
  Name, Model, Spec, Normalized: string;
  Parts: TStringList;
  A, B, C: Double;
  DefSpan, TmpSpan: TTikZSourceSpan;
  Tmp: RawByteString;
begin
  StartIndex := Index;
  I := Index + 1;
  if not TryReadBraceGroup(I, Limit, GroupIndex, Tmp, TmpSpan) then
  begin
    Unsupported(TokenSpan(StartIndex), 'Malformed \\definecolor command');
    Exit;
  end;
  Name := TrimASCII(string(Tmp));
  if not TryReadBraceGroup(I, Limit, GroupIndex, Tmp, TmpSpan) then
  begin
    Unsupported(TokenSpan(StartIndex), 'Malformed \\definecolor command');
    Exit;
  end;
  Model := LowerASCII(TrimASCII(string(Tmp)));
  if not TryReadBraceGroup(I, Limit, GroupIndex, Tmp, TmpSpan) then
  begin
    Unsupported(TokenSpan(StartIndex), 'Malformed \\definecolor command');
    Exit;
  end;
  Spec := TrimASCII(string(Tmp));
  if Model = 'html' then
  begin
    if (Length(Spec) <> 6) then
    begin
      Unsupported(TokenSpan(StartIndex), 'Unsupported HTML color value');
      Exit;
    end;
    Normalized := '#' + Spec;
  end
  else if Model = 'rgb' then
  begin
    Parts := TStringList.Create;
    try
      Parts.Delimiter := ',';
      Parts.StrictDelimiter := True;
      Parts.DelimitedText := Spec;
      if (Parts.Count <> 3) or not ParseInvariantFloat(TrimASCII(Parts[0]), A)
        or not ParseInvariantFloat(TrimASCII(Parts[1]), B)
        or not ParseInvariantFloat(TrimASCII(Parts[2]), C) or
        (A < 0) or (A > 1) or (B < 0) or (B > 1) or (C < 0) or (C > 1) then
      begin
        Unsupported(TokenSpan(StartIndex), 'Unsupported rgb color value');
        Exit;
      end;
      Normalized := InvariantFloatText(A) + ',' + InvariantFloatText(B) + ',' +
        InvariantFloatText(C);
    finally
      Parts.Free;
    end;
  end
  else if Model = 'rgb255' then
  begin
    Parts := TStringList.Create;
    try
      Parts.Delimiter := ',';
      Parts.StrictDelimiter := True;
      Parts.DelimitedText := Spec;
      if (Parts.Count <> 3) or not ParseInvariantFloat(TrimASCII(Parts[0]), A)
        or not ParseInvariantFloat(TrimASCII(Parts[1]), B)
        or not ParseInvariantFloat(TrimASCII(Parts[2]), C) or
        (A < 0) or (A > 255) or (B < 0) or (B > 255) or (C < 0) or (C > 255) then
      begin
        Unsupported(TokenSpan(StartIndex), 'Unsupported RGB color value');
        Exit;
      end;
      Normalized := InvariantFloatText(A / 255) + ',' +
        InvariantFloatText(B / 255) + ',' + InvariantFloatText(C / 255);
    finally
      Parts.Free;
    end;
  end
  else if Model = 'gray' then
  begin
    if not ParseInvariantFloat(Spec, A) or (A < 0) or (A > 1) then
    begin
      Unsupported(TokenSpan(StartIndex), 'Unsupported gray color value');
      Exit;
    end;
    Normalized := InvariantFloatText(A) + ',' + InvariantFloatText(A) + ',' +
      InvariantFloatText(A);
  end
  else
  begin
    Unsupported(TokenSpan(StartIndex), 'Unsupported color model: ' + Model);
    Exit;
  end;
  N := FColors.IndexOfName(Name);
  if N < 0 then FColors.Add(Name + '=' + Normalized)
  else FColors.Values[Name] := Normalized;
  DefSpan := SpanBetween(StartIndex, I);
  N := FColorSpans.IndexOfName(Name);
  if N < 0 then FColorSpans.Add(Name + '=' + IntToStr(DefSpan.StartByte) + ':' +
    IntToStr(DefSpan.EndByte))
  else FColorSpans.Values[Name] := IntToStr(DefSpan.StartByte) + ':' +
    IntToStr(DefSpan.EndByte);
  Index := I;
end;

function ParseInvariantFloat(const Text: string; out Value: Double): Boolean;
var
  FS: TFormatSettings;
begin
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  Result := TryStrToFloat(Text, Value, FS);
end;

function InvariantFloatText(Value: Double): string;
var
  FS: TFormatSettings;
begin
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  Result := FloatToStr(Value, FS);
end;

function UnitToMillimeters(const UnitValue: string; out Factor: Double): Boolean;
begin
  Result := True;
  if SameText(UnitValue, 'mm') then Factor := 1
  else if SameText(UnitValue, 'cm') then Factor := 10
  else if SameText(UnitValue, 'in') then Factor := 25.4
  else if SameText(UnitValue, 'pt') then Factor := 25.4 / 72.27
  else if SameText(UnitValue, 'bp') then Factor := 25.4 / 72
  else if SameText(UnitValue, 'px') then Factor := 25.4 / 96
  else Result := False;
end;

function TTikZEvaluator.TryReadMeasure(var Index: SizeInt;
  out Value: Double; out MeasureUnit: string;
  out ValueSpan: TTikZSourceSpan): Boolean;
var
  SignValue: Double;
  StartIndex, NumberIndex: SizeInt;
  TextValue, NumericText: string;
  I: SizeInt;
begin
  Result := False;
  Value := 0;
  MeasureUnit := '';
  ValueSpan := SpanAtZero;
  Index := NextSignificant(Index, FDocument.TokenCount);
  StartIndex := Index;
  SignValue := 1;
  if (Index < FDocument.TokenCount) and
    (FDocument.TokenAt(Index).Kind = ttkOperator) and
    ((TokenText(Index) = '+') or (TokenText(Index) = '-')) then
  begin
    if TokenText(Index) = '-' then SignValue := -1;
    Inc(Index);
    Index := NextSignificant(Index, FDocument.TokenCount);
  end;
  NumberIndex := Index;
  if Index >= FDocument.TokenCount then Exit;
  if not (FDocument.TokenAt(Index).Kind in [ttkNumber, ttkDimension]) then
    Exit;
  TextValue := TokenText(Index);
  NumericText := TextValue;
  for I := 1 to Length(TextValue) do
    if not (((TextValue[I] >= '0') and (TextValue[I] <= '9')) or
      (TextValue[I] = '.') or (TextValue[I] = 'e') or
      (TextValue[I] = 'E') or (TextValue[I] = '+') or
      (TextValue[I] = '-')) then
    begin
      MeasureUnit := Copy(TextValue, I, MaxInt);
      NumericText := Copy(TextValue, 1, I - 1);
      Break;
    end;
  if not ParseInvariantFloat(NumericText, Value) then Exit;
  Value := Value * SignValue;
  ValueSpan := SpanBetween(StartIndex, NumberIndex + 1);
  Inc(Index);
  Result := True;
end;

function TTikZEvaluator.TryReadCoordinate(var Index: SizeInt; Limit: SizeInt;
  const RelativeBase: TTikZPoint; out Point: TTikZPoint;
  out UpdateRelativeBase: Boolean; out PointSpan, XSpan,
  YSpan: TTikZSourceSpan; out IsRelative: Boolean;
  out XScaleToMM, YScaleToMM: Double; out XUnit, YUnit: string): Boolean;
var
  GroupIndex, OpenIndex, CloseIndex: SizeInt;
  X, Y, XFactor, YFactor: Double;
  Token: TTikZToken;
  Relative: Boolean;
begin
  Result := False;
  Point.X := 0;
  Point.Y := 0;
  PointSpan := SpanAtZero;
  XSpan := SpanAtZero;
  YSpan := SpanAtZero;
  IsRelative := False;
  XScaleToMM := 0;
  YScaleToMM := 0;
  XUnit := '';
  YUnit := '';
  UpdateRelativeBase := True;
  Index := NextSignificant(Index, Limit);
  Relative := False;
  if (Index < Limit) and (FDocument.TokenAt(Index).Kind = ttkOperator) and
    (TokenText(Index) = '+') then
  begin
    Relative := True;
    Inc(Index);
    Index := NextSignificant(Index, Limit);
    if (Index < Limit) and (FDocument.TokenAt(Index).Kind = ttkOperator) and
      (TokenText(Index) = '+') then Inc(Index)
    else UpdateRelativeBase := False;
  end;
  Index := NextSignificant(Index, Limit);
  OpenIndex := Index;
  if (Index >= Limit) or (FOpenGroups[Index] < 0) then Exit;
  GroupIndex := FOpenGroups[Index];
  if FDocument.GroupAt(GroupIndex).Kind <> tgParen then Exit;
  CloseIndex := FDocument.GroupAt(GroupIndex).CloseToken;
  Inc(Index);
  if not TryReadMeasure(Index, X, XUnit, XSpan) then Exit;
  Index := NextSignificant(Index, CloseIndex);
  if (Index >= CloseIndex) or
    (FDocument.TokenAt(Index).Kind <> ttkComma) then Exit;
  Inc(Index);
  if not TryReadMeasure(Index, Y, YUnit, YSpan) then Exit;
  Index := NextSignificant(Index, CloseIndex);
  if Index <> CloseIndex then Exit;
  if (XUnit = '') <> (YUnit = '') then Exit;
  if (XUnit <> '') and not UnitToMillimeters(XUnit, XFactor) then Exit;
  if (YUnit <> '') and not UnitToMillimeters(YUnit, YFactor) then Exit;
  if (XUnit <> '') then
  begin
    XScaleToMM := XFactor;
    YScaleToMM := YFactor;
    X := X * XFactor;
    Y := Y * YFactor;
  end
  else
  begin
    XScaleToMM := FResult.Scene.PictureScaleX;
    YScaleToMM := FResult.Scene.PictureScaleY;
    X := X * XScaleToMM;
    Y := Y * YScaleToMM;
  end;
  if (FResult.Scene.PictureShearX <> 0) or
    (FResult.Scene.PictureShearY <> 0) then
  begin
    if (XUnit <> '') or (YUnit <> '') then Exit;
    Point.X := X + Y * FResult.Scene.PictureShearX;
    Point.Y := X * FResult.Scene.PictureShearY + Y;
  end
  else
  begin
    Point.X := X;
    Point.Y := Y;
  end;
  PointSpan := SpanBetween(OpenIndex, CloseIndex + 1);
  Token := FDocument.TokenAt(OpenIndex);
  if not Tick(Token.Span) then Exit;
  if Relative then
  begin
    Point.X := RelativeBase.X + Point.X;
    Point.Y := RelativeBase.Y + Point.Y;
  end;
  IsRelative := Relative;
  Index := CloseIndex + 1;
  Result := True;
end;

function TrimASCII(const Value: string): string;
var
  FirstAt, LastAt: SizeInt;
begin
  FirstAt := 1;
  LastAt := Length(Value);
  while (FirstAt <= LastAt) and (Ord(Value[FirstAt]) <= 32) do Inc(FirstAt);
  while (LastAt >= FirstAt) and (Ord(Value[LastAt]) <= 32) do Dec(LastAt);
  Result := Copy(Value, FirstAt, LastAt - FirstAt + 1);
end;

function LowerASCII(const Value: string): string;
var
  I: SizeInt;
begin
  Result := Value;
  for I := 1 to Length(Result) do
    if (Result[I] >= 'A') and (Result[I] <= 'Z') then
      Result[I] := Chr(Ord(Result[I]) + Ord('a') - Ord('A'));
end;

function TTikZEvaluator.ParseOptionGroup(GroupIndex: SizeInt):
  TTikZOptionValues;
var
  Group: TTikZGroup;
  I, StartAt, EqualsAt, PastAt, N: SizeInt;
  Tok: TTikZToken;
  Item: TTikZOptionValue;
begin
  Result := nil;
  if (GroupIndex < 0) or (GroupIndex >= FDocument.GroupCount) then Exit;
  Group := FDocument.GroupAt(GroupIndex);
  if Group.Kind <> tgBracket then Exit;
  StartAt := Group.OpenToken + 1;
  I := StartAt;
  while I < Group.CloseToken do
  begin
    I := NextSignificant(I, Group.CloseToken);
    if I >= Group.CloseToken then Break;
    StartAt := I;
    EqualsAt := -1;
    while I < Group.CloseToken do
    begin
      Tok := FDocument.TokenAt(I);
      if (Tok.Kind in [ttkOpenBrace, ttkOpenBracket, ttkOpenParen]) and
        (FOpenGroups[I] >= 0) then
      begin
        I := FDocument.GroupAt(FOpenGroups[I]).CloseToken + 1;
        Continue;
      end;
      if Tok.Kind = ttkComma then Break;
      if (Tok.Kind = ttkEquals) and (EqualsAt < 0) then EqualsAt := I;
      Inc(I);
    end;
    PastAt := I;
    while (PastAt > StartAt) and
      (FDocument.TokenAt(PastAt - 1).Kind = ttkWhitespace) do Dec(PastAt);
    if PastAt > StartAt then
    begin
      FillChar(Item, SizeOf(Item), 0);
      Item.OptionSpan := SpanBetween(StartAt, PastAt);
      if EqualsAt >= 0 then
      begin
        Item.Key := LowerASCII(TrimASCII(string(SourceText(
          SpanBetween(StartAt, EqualsAt)))));
        Item.KeySpan := SpanBetween(StartAt, EqualsAt);
        Item.ValueSpan := SpanBetween(EqualsAt + 1, PastAt);
        Item.Value := TrimASCII(string(SourceText(Item.ValueSpan)));
        Item.HasValue := True;
      end
      else
      begin
        Item.Key := LowerASCII(TrimASCII(string(SourceText(Item.OptionSpan))));
        Item.KeySpan := Item.OptionSpan;
        Item.Value := '';
        Item.ValueSpan := SpanAtZero;
        Item.HasValue := False;
      end;
      N := Length(Result);
      SetLength(Result, N + 1);
      Result[N] := Item;
    end;
    if (I < Group.CloseToken) and
      (FDocument.TokenAt(I).Kind = ttkComma) then Inc(I);
  end;
end;

function FindDependencyKind(Obj: TTikZSceneObject;
  Kind: TTikZDependencyKind): SizeInt;
var
  I: SizeInt;
begin
  for I := 0 to High(Obj.Dependencies) do
    if Obj.Dependencies[I].Kind = Kind then Exit(I);
  Result := -1;
end;

{ Splits a key list at top-level commas. }
function SplitTopLevel(const Text: string): TStringArray;
var
  I, Depth, Start, N: SizeInt;
begin
  Result := nil;
  Depth := 0;
  Start := 1;
  for I := 1 to Length(Text) + 1 do
  begin
    if (I <= Length(Text)) and (Text[I] in ['{', '(', '[']) then Inc(Depth)
    else if (I <= Length(Text)) and (Text[I] in ['}', ')', ']']) then Dec(Depth)
    else if (I > Length(Text)) or ((Text[I] = ',') and (Depth = 0)) then
    begin
      N := Length(Result);
      SetLength(Result, N + 1);
      Result[N] := TrimASCII(Copy(Text, Start, I - Start));
      Start := I + 1;
    end;
  end;
end;

// The literal fading transform TpX writes, with the keys shift (a braced
// coordinate), rotate, xscale and yscale in that order. Unitless shift
// coordinates use the picture basis.
function TTikZEvaluator.ParseFadingTransform(const Text: string;
  Obj: TTikZSceneObject): Boolean;
var
  S, Pair: string;
  Items, Axes: TStringArray;
  Values: array[0..3] of Double;
  Shift: array[0..1] of Double;
  I, Eq: SizeInt;
  Dependency: string;
const
  Keys: array[0..3] of string = ('shift', 'rotate', 'xscale', 'yscale');
begin
  Result := False;
  S := TrimASCII(Text);
  if (Length(S) >= 2) and (S[1] = '{') and (S[Length(S)] = '}') then
    S := Copy(S, 2, Length(S) - 2);
  Items := SplitTopLevel(S);
  if Length(Items) <> 4 then Exit;
  for I := 0 to 3 do
  begin
    Eq := Pos('=', Items[I]);
    if (Eq <= 0) or (TrimASCII(Copy(Items[I], 1, Eq - 1)) <> Keys[I]) then Exit;
    S := TrimASCII(Copy(Items[I], Eq + 1, MaxInt));
    if I > 0 then
    begin
      if not ParseInvariantFloat(S, Values[I]) then Exit;
      Continue;
    end;
    if (Length(S) < 4) or (S[1] <> '{') or (S[Length(S)] <> '}') then Exit;
    Pair := TrimASCII(Copy(S, 2, Length(S) - 2));
    if (Length(Pair) < 2) or (Pair[1] <> '(') or (Pair[Length(Pair)] <> ')') then
      Exit;
    Axes := SplitTopLevel(Copy(Pair, 2, Length(Pair) - 2));
    if Length(Axes) <> 2 then Exit;
    for Eq := 0 to 1 do
      if ParseInvariantFloat(Axes[Eq], Shift[Eq]) then
      begin
        if Eq = 0 then Shift[Eq] := Shift[Eq] * FResult.Scene.PictureScaleX
        else Shift[Eq] := Shift[Eq] * FResult.Scene.PictureScaleY;
      end
      else if not ParseLengthExpression(Axes[Eq], Shift[Eq], Dependency) or
        (Dependency <> '') then Exit;
  end;
  if (Values[2] <= 0) or (Values[3] <= 0) then Exit;
  Obj.FadingShift.X := Shift[0];
  Obj.FadingShift.Y := Shift[1];
  Obj.FadingRotation := Values[1];
  Obj.FadingXScale := Values[2];
  Obj.FadingYScale := Values[3];
  Result := True;
end;

{ Reads a fading declaration in the form TpX writes, starting at the
  \pgfinterruptpicture token Index. Returns the token after it, or -1 when
  the group is not exactly a TpX fading. }
function TTikZEvaluator.ParseFadingDeclaration(Index, Limit: SizeInt): SizeInt;
var
  J, P, Q, N: SizeInt;
  Tok: TTikZToken;
  Body, Name, A0Text, A1Text: string;
  Radial: Boolean;
  A0, A1: Double;
  Span: TTikZSourceSpan;
  function Between(const Prefix: string; From: SizeInt;
    out Value: string): SizeInt;
  var K, L: SizeInt;
  begin
    Result := 0;
    K := Pos(Prefix, Copy(Body, From, MaxInt));
    if K <= 0 then Exit;
    K := From + K - 1 + Length(Prefix);
    L := K;
    while (L <= Length(Body)) and (Body[L] <> ')') do Inc(L);
    if L > Length(Body) then Exit;
    Value := Copy(Body, K, L - K);
    Result := L;
  end;
  function Canonical(const Text: string; out Value: Double): Boolean;
  begin
    Result := ParseInvariantFloat(Text, Value) and (Value >= 0) and
      (Value <= 1) and (TikZFadingStopText(Value) = Text);
  end;
begin
  Result := -1;
  J := Index + 1;
  while J < Limit do
  begin
    Tok := FDocument.TokenAt(J);
    if (Tok.Kind = ttkControlWord) and (TokenText(J) = '\endpgfinterruptpicture') then
      Break;
    if (Tok.Kind in [ttkOpenBrace, ttkOpenBracket, ttkOpenParen]) and
      (FOpenGroups[J] >= 0) then
      J := FDocument.GroupAt(FOpenGroups[J]).CloseToken + 1
    else Inc(J);
  end;
  if J >= Limit then Exit;
  Body := string(SourceText(SpanBetween(Index + 1, J)));
  if Copy(Body, 1, 25) = '\pgfdeclareradialshading{' then
  begin
    Radial := True;
    P := 26;
  end
  else if Copy(Body, 1, 29) = '\pgfdeclarehorizontalshading{' then
  begin
    Radial := False;
    P := 30;
  end
  else Exit;
  Q := Pos('s}', Copy(Body, P, MaxInt));
  if Q <= 0 then Exit;
  Name := Copy(Body, P, Q - 1);
  if Between('gray(0bp)=(', P, A0Text) = 0 then Exit;
  if Radial then N := Between('gray(25bp)=(', P, A1Text)
  else N := Between('gray(75bp)=(', P, A1Text);
  if (N = 0) or not Canonical(A0Text, A0) or not Canonical(A1Text, A1) then Exit;
  { The name spells out the stops, and the whole body is the writer's. }
  if (Name <> TikZFadingName(Radial, A0Text, A1Text)) or
    (Body <> TikZFadingBody(Name, Radial, A0Text, A1Text)) then Exit;
  { The declaration owns its line: the trailing comment and line break. }
  Span := SpanBetween(Index, J + 1);
  P := Span.EndByte;
  if (P < Length(FSource)) and (FSource[P] = Ord('%')) then
    while (P < Length(FSource)) and not (FSource[P] in [10, 13]) do Inc(P);
  if (P < Length(FSource)) and (FSource[P] = 13) then Inc(P);
  if (P < Length(FSource)) and (FSource[P] = 10) then Inc(P);
  Span.EndByte := P;
  N := Length(FResult.Scene.Fadings);
  SetLength(FResult.Scene.Fadings, N + 1);
  FResult.Scene.Fadings[N].Name := Name;
  FResult.Scene.Fadings[N].Radial := Radial;
  FResult.Scene.Fadings[N].StartOpacity := A0;
  FResult.Scene.Fadings[N].EndOpacity := A1;
  FResult.Scene.Fadings[N].Span := Span;
  Result := J + 1;
end;

function SameScenePoint(const A, B: TTikZPoint): Boolean;
begin
  Result := (Abs(A.X - B.X) <= 1E-9 * Max(1, Abs(A.X))) and
    (Abs(A.Y - B.Y) <= 1E-9 * Max(1, Abs(A.Y)));
end;

function SameSceneValue(A, B: Double): Boolean;
begin
  Result := Abs(A - B) <= 1E-9 * Max(1, Abs(A));
end;

{ True when two scene objects describe the same path geometry. }
function SameSceneGeometry(A, B: TTikZSceneObject): Boolean;
var
  I: SizeInt;
begin
  Result := False;
  if (A.Kind <> B.Kind) or (Length(A.Commands) <> Length(B.Commands)) then Exit;
  for I := 0 to High(A.Commands) do
    if (A.Commands[I].Kind <> B.Commands[I].Kind) or
      not SameScenePoint(A.Commands[I].P1, B.Commands[I].P1) or
      ((A.Commands[I].Kind = tpcCubic) and
      (not SameScenePoint(A.Commands[I].P2, B.Commands[I].P2) or
      not SameScenePoint(A.Commands[I].P3, B.Commands[I].P3))) then Exit;
  Result := SameScenePoint(A.Center, B.Center) and
    SameScenePoint(A.Origin, B.Origin) and
    SameSceneValue(A.RadiusX, B.RadiusX) and
    SameSceneValue(A.RadiusY, B.RadiusY) and
    SameSceneValue(A.WidthMM, B.WidthMM) and
    SameSceneValue(A.HeightMM, B.HeightMM) and
    SameSceneValue(A.Rotation, B.Rotation) and
    SameSceneValue(A.StartAngle, B.StartAngle) and
    SameSceneValue(A.EndAngle, B.EndAngle);
end;

{ The statement after a faded fill completes it when it strokes the same
  geometry and paints nothing else, which is how TpX writes the stroke of
  such a fill. }
procedure TTikZEvaluator.LinkStrokeCompanion(FillIndex: SizeInt;
  const DefinedColors: string);
var
  Fill, Stroke: TTikZSceneObject;
  StrokeIndex: SizeInt;
begin
  StrokeIndex := FResult.Scene.ObjectCount - 1;
  if (FillIndex < 0) or (StrokeIndex <> FillIndex + 1) then Exit;
  Fill := FResult.Scene.ObjectAt(FillIndex);
  Stroke := FResult.Scene.ObjectAt(StrokeIndex);
  if (Fill.FadingName = '') or Fill.StrokeEnabled or
    not Stroke.StrokeEnabled or Stroke.FillEnabled or
    (Stroke.Gradient <> tgNone) or (Stroke.FadingName <> '') or
    not SameSceneGeometry(Fill, Stroke) then Exit;
  Fill.StrokeCompanion := StrokeIndex;
  Stroke.StrokeOf := FillIndex;
  Stroke.ColorsSinceFill := DefinedColors;
end;

function TTikZEvaluator.FindOption(const Values: TTikZOptionValues;
  const Key: string): SizeInt;
var
  I: SizeInt;
begin
  Result := -1;
  for I := High(Values) downto 0 do
    if Values[I].Key = LowerASCII(Key) then Exit(I);
end;

function TTikZEvaluator.ParseLengthExpression(const Text: string;
  out ValueMM: Double; out Dependency: string): Boolean;
var
  Compact, NumberText, UnitSuffix, MacroName: string;
  I, StarAt: SizeInt;
  Factor, Number: Double;
  M: SizeInt;
begin
  Result := False;
  ValueMM := 0;
  Dependency := '';
  UnitSuffix := '';
  Compact := '';
  for I := 1 to Length(Text) do
    if Ord(Text[I]) > 32 then Compact := Compact + Text[I];
  StarAt := Pos('*', Compact);
  MacroName := '';
  if StarAt > 0 then
  begin
    NumberText := Copy(Compact, 1, StarAt - 1);
    MacroName := Copy(Compact, StarAt + 1, MaxInt);
    if not ParseInvariantFloat(NumberText, Number) then Exit;
  end
  else if (Length(Compact) > 0) and (Compact[1] = '\') then
  begin
    Number := 1;
    MacroName := Compact;
  end
  else
  begin
    NumberText := Compact;
    for I := 1 to Length(NumberText) do
      if not (((NumberText[I] >= '0') and (NumberText[I] <= '9')) or
        (NumberText[I] = '.') or (NumberText[I] = 'e') or
        (NumberText[I] = 'E') or (NumberText[I] = '+') or
        (NumberText[I] = '-')) then
      begin
        UnitSuffix := Copy(NumberText, I, MaxInt);
        NumberText := Copy(NumberText, 1, I - 1);
        Break;
      end;
    if not ParseInvariantFloat(NumberText, Number) then Exit;
    if UnitSuffix = '' then Exit;
    if not UnitToMillimeters(UnitSuffix, Factor) then Exit;
    ValueMM := Number * Factor;
    Exit(True);
  end;
  if MacroName = '' then Exit;
  Dependency := MacroName;
  for M := 0 to High(FResult.Scene.MacroDefinitions) do
    if FResult.Scene.MacroDefinitions[M].Name = MacroName then
    begin
      ValueMM := Number * FResult.Scene.MacroDefinitions[M].ValueMM;
      Exit(True);
    end;
end;

{ Accept the core TikZ arrow specifications TpX writes: "-", "->", "<-",
  "<->" and the named "latex" and "stealth" tips at either end. }
function ParseTikZArrowSpec(const Spec: string;
  out BeginTip, EndTip: string): Boolean;
var
  Dash: SizeInt;
  Left, Right: string;
begin
  Result := False;
  BeginTip := '';
  EndTip := '';
  Dash := Pos('-', Spec);
  if (Dash = 0) or (Pos('-', Copy(Spec, Dash + 1, MaxInt)) > 0) then Exit;
  Left := TrimASCII(Copy(Spec, 1, Dash - 1));
  Right := TrimASCII(Copy(Spec, Dash + 1, MaxInt));
  if (Left = '<') or (Left = 'to') then BeginTip := 'to'
  else if (Left = 'latex') or (Left = 'stealth') then BeginTip := Left
  else if Left <> '' then Exit;
  if (Right = '>') or (Right = 'to') then EndTip := 'to'
  else if (Right = 'latex') or (Right = 'stealth') then EndTip := Right
  else if Right <> '' then Exit;
  Result := True;
end;

function TTikZEvaluator.ParseRGB(const Name: string; out RGB: LongWord): Boolean;
var
  Value: string;
  Parts: TStringList;
  R, G, B: Double;
  Hex: Int64;
begin
  Result := False;
  RGB := 0;
  if FColors.IndexOfName(Name) >= 0 then
  begin
    Value := FColors.Values[Name];
    if (Length(Value) = 7) and (Value[1] = '#') then
    begin
      if TryStrToInt64('$' + Copy(Value, 2, 6), Hex) then
      begin
        RGB := Hex and $FFFFFF;
        Exit(True);
      end;
    end;
    Parts := TStringList.Create;
    try
      Parts.Delimiter := ',';
      Parts.StrictDelimiter := True;
      Parts.DelimitedText := Value;
      if (Parts.Count = 3) and ParseInvariantFloat(Parts[0], R) and
        ParseInvariantFloat(Parts[1], G) and ParseInvariantFloat(Parts[2], B) and
        (R >= 0) and (R <= 1) and (G >= 0) and (G <= 1) and
        (B >= 0) and (B <= 1) then
      begin
        RGB := (LongWord(Round(R * 255)) shl 16) or
          (LongWord(Round(G * 255)) shl 8) or LongWord(Round(B * 255));
        Exit(True);
      end;
    finally
      Parts.Free;
    end;
  end;
  if Name = 'black' then RGB := $000000
  else if Name = 'white' then RGB := $FFFFFF
  else if Name = 'red' then RGB := $FF0000
  else if Name = 'green' then RGB := $00FF00
  else if Name = 'blue' then RGB := $0000FF
  else if Name = 'gray' then RGB := $808080
  else if Name = 'cyan' then RGB := $00FFFF
  else if Name = 'magenta' then RGB := $FF00FF
  else if Name = 'yellow' then RGB := $FFFF00
  else Exit;
  Result := True;
end;

procedure TTikZEvaluator.EvaluatePreamble;
var
  I, Limit, GroupIndex, N, Existing: SizeInt;
  Tok: TTikZToken;
  Name, MacroName, Dependency: string;
  Body: RawByteString;
  BodySpan, CommandSpan: TTikZSourceSpan;
  ValueMM: Double;
  Def: TTikZMacroDefinition;
begin
  Limit := FDocument.Region.FirstContentToken;
  if Limit < 0 then Limit := FDocument.TokenCount;
  I := 0;
  while I < Limit do
  begin
    Tok := FDocument.TokenAt(I);
    if Tok.Kind = ttkControlWord then
    begin
      Name := TokenText(I);
      if Name = '\definecolor' then
      begin
        N := I;
        ParseColorDefinition(N, Limit);
        if N > I then I := N else Inc(I);
        Continue;
      end;
      if Name = '\providecommand' then
      begin
        N := I + 1;
        if not TryReadBraceGroup(N, Limit, GroupIndex, Body, BodySpan) then
        begin
          Unsupported(Tok.Span, 'Malformed \\providecommand definition');
          Inc(I);
          Continue;
        end;
        MacroName := TrimASCII(string(Body));
        N := NextSignificant(N, Limit);
        if (N < Limit) and (FDocument.TokenAt(N).Kind = ttkOpenBracket) and
          (FOpenGroups[N] >= 0) then
            N := FDocument.GroupAt(FOpenGroups[N]).CloseToken + 1;
        if not TryReadBraceGroup(N, Limit, GroupIndex, Body, BodySpan) then
        begin
          Unsupported(Tok.Span, 'Malformed \\providecommand definition');
          Inc(I);
          Continue;
        end;
        if (Copy(MacroName, 1, 4) = '\tpx') and
          ParseLengthExpression(string(Body), ValueMM, Dependency) then
        begin
          Def.Name := MacroName;
          Def.ValueMM := ValueMM;
          Def.SourceSpan := SpanBetween(I, N);
          Def.ExpressionSpan := BodySpan;
          Existing := -1;
          for GroupIndex := 0 to High(FResult.Scene.MacroDefinitions) do
            if FResult.Scene.MacroDefinitions[GroupIndex].Name = MacroName then
              Existing := GroupIndex;
          if Existing < 0 then
          begin
            Existing := Length(FResult.Scene.MacroDefinitions);
            SetLength(FResult.Scene.MacroDefinitions, Existing + 1);
          end;
          FResult.Scene.MacroDefinitions[Existing] := Def;
          if MacroName = '\tpxLineWidth' then
            FResult.Scene.LineWidthBaseMM := ValueMM
          else if MacroName = '\tpxTextSize' then
            FResult.Scene.TextSizeMM := ValueMM
          else if MacroName = '\tpxDashSize' then
            FResult.Scene.DashSizeMM := ValueMM
          else if MacroName = '\tpxDotSize' then
            FResult.Scene.DotSizeMM := ValueMM;
        end
        else if Copy(MacroName, 1, 4) = '\tpx' then
          Unsupported(SpanBetween(I, N),
            'Unsupported expression in shared default macro ' + MacroName);
        I := N;
        Continue;
      end;
    end;
    Inc(I);
    if (I mod 1024 = 0) and not Tick(Tok.Span) then Exit;
  end;
end;

procedure TTikZEvaluator.EvaluatePictureHeader;
var
  I, HeaderLimit, GroupIndex, OptionIndex: SizeInt;
  Options: TTikZOptionValues;
  Key, Dependency: string;
  Value: Double;
begin
  I := FDocument.Region.BeginToken;
  if I < 0 then Exit;
  Inc(I);
  if not ReadGroup(I, tgBrace, GroupIndex) then
  begin
    Invalid(TokenSpan(FDocument.Region.BeginToken),
      'Malformed tikzpicture begin directive');
    Exit;
  end;
  I := NextSignificant(I, FDocument.TokenCount);
  if (I >= FDocument.TokenCount) or (FOpenGroups[I] < 0) then Exit;
  if FDocument.GroupAt(FOpenGroups[I]).Kind <> tgBracket then Exit;
  GroupIndex := FOpenGroups[I];
  HeaderLimit := FDocument.GroupAt(GroupIndex).CloseToken;
  Options := ParseOptionGroup(GroupIndex);
  for OptionIndex := 0 to High(Options) do
  begin
    Key := Options[OptionIndex].Key;
    if Key = 'x' then
    begin
      if not ParseLengthExpression(Options[OptionIndex].Value, Value, Dependency) then
        Unsupported(Options[OptionIndex].OptionSpan,
          'Unsupported x coordinate basis')
      else FResult.Scene.PictureScaleX := Value;
    end
    else if Key = 'y' then
    begin
      if not ParseLengthExpression(Options[OptionIndex].Value, Value, Dependency) then
        Unsupported(Options[OptionIndex].OptionSpan,
          'Unsupported y coordinate basis')
      else FResult.Scene.PictureScaleY := Value;
    end
    else if (Key = 'scale') or (Key = 'xscale') or (Key = 'yscale') then
    begin
      if not ParseInvariantFloat(Options[OptionIndex].Value, Value) then
      begin
        Unsupported(Options[OptionIndex].OptionSpan,
          'Unsupported dimensionless picture scale');
        Continue;
      end;
      if (Key = 'scale') or (Key = 'xscale') then
        FResult.Scene.PictureScaleX := FResult.Scene.PictureScaleX * Value;
      if (Key = 'scale') or (Key = 'yscale') then
        FResult.Scene.PictureScaleY := FResult.Scene.PictureScaleY * Value;
    end
    else if (Key = 'inner xsep') or (Key = 'inner ysep') or
      (Key = 'outer xsep') or (Key = 'outer ysep') then
    begin
      if (not ParseLengthExpression(Options[OptionIndex].Value, Value, Dependency))
        or (Abs(Value) > 1E-12) then
        Unsupported(Options[OptionIndex].OptionSpan,
          'Nonzero picture node separation changes the emitted geometry');
    end
    else if Key = 'miter limit' then
    begin
      if not ParseInvariantFloat(Options[OptionIndex].Value, Value) or
        (Value <= 0) then
        Unsupported(Options[OptionIndex].OptionSpan,
          'Miter limit must be a positive literal number')
      else FResult.Scene.MiterLimit := Value;
    end
    else
      Unsupported(Options[OptionIndex].OptionSpan,
        'Unsupported tikzpicture option: ' + Key);
  end;
end;

procedure TTikZEvaluator.EvaluatePicture;
var
  I, J, EndToken: SizeInt;
  Tok: TTikZToken;
  Command: string;
  ScopeStack: TTikZTransformStack;
  OpacityStack: TTikZScopeOpacityStack;
  ScopeTransform, Operation, PreviousTransform: TTikZAffineTransform;
  ScopeName: string;
  ScopeGroup, CloseToken, K: SizeInt;
  ScopeOptions: TTikZOptionValues;
  ScopeOption: TTikZOptionValue;
  DX, DY, Number: Double;
  Dependency: string;
  PendingFill, CountBefore: SizeInt;
  ColorsSincePending: string;

  function ReadScopeName(var Cursor: SizeInt; out Name: string): Boolean;
  var
    GroupToken, GroupNo, Close: SizeInt;
    GroupInfo: TTikZGroup;
  begin
    Result := False;
    Cursor := NextSignificant(Cursor, EndToken);
    if (Cursor >= EndToken) or (FOpenGroups[Cursor] < 0) or
      (FDocument.GroupAt(FOpenGroups[Cursor]).Kind <> tgBrace) then Exit;
    GroupToken := Cursor;
    GroupNo := FOpenGroups[GroupToken];
    GroupInfo := FDocument.GroupAt(GroupNo);
    Close := GroupInfo.CloseToken;
    Name := TrimASCII(string(SourceText(SpanBetween(GroupToken + 1, Close))));
    Cursor := Close + 1;
    Result := True;
  end;

  function ParseScopeShift(const Text: string; out ShiftX,
    ShiftY: Double): Boolean;
  var
    S, XText, YText: string;
    Comma: SizeInt;
    XVal, YVal: Double;
    function ParseAxis(const AxisText: string; DefaultScale: Double;
      out AxisValue: Double): Boolean;
    begin
      Result := False;
      if ParseLengthExpression(AxisText, AxisValue, Dependency) then
      begin
        if Dependency <> '' then Exit;
        Exit(True);
      end;
      if not ParseInvariantFloat(TrimASCII(AxisText), AxisValue) then Exit;
      AxisValue := AxisValue * DefaultScale;
      Result := True;
    end;
  begin
    Result := False;
    ShiftX := 0;
    ShiftY := 0;
    S := TrimASCII(Text);
    if (Length(S) >= 2) and (S[1] = '{') and
      (S[Length(S)] = '}') then S := TrimASCII(Copy(S, 2, Length(S) - 2));
    if (Length(S) < 3) or (S[1] <> '(') or (S[Length(S)] <> ')') then Exit;
    S := Copy(S, 2, Length(S) - 2);
    Comma := Pos(',', S);
    if Comma <= 0 then Exit;
    XText := TrimASCII(Copy(S, 1, Comma - 1));
    YText := TrimASCII(Copy(S, Comma + 1, MaxInt));
    if not ParseAxis(XText, FResult.Scene.PictureScaleX, XVal) or
      not ParseAxis(YText, FResult.Scene.PictureScaleY, YVal) then Exit;
    ShiftX := XVal;
    ShiftY := YVal;
    Result := True;
  end;

  procedure ApplyScopeOptions(GroupNo: SizeInt;
    out Transform: TTikZAffineTransform);
  var
    L: SizeInt;
  begin
    Transform := IdentityTikZTransform;
    ScopeOptions := ParseOptionGroup(GroupNo);
    for L := 0 to High(ScopeOptions) do
    begin
      ScopeOption := ScopeOptions[L];
      Operation := IdentityTikZTransform;
      if ScopeOption.Key = 'shift' then
      begin
        if not ParseScopeShift(ScopeOption.Value, DX, DY) then
        begin
          Unsupported(ScopeOption.OptionSpan,
            'Scope shift requires a literal supported coordinate');
          Continue;
        end;
        Operation.TX := DX;
        Operation.TY := DY;
      end
      else if ScopeOption.Key = 'rotate' then
      begin
        if not ParseInvariantFloat(ScopeOption.Value, Number) then
        begin
          Unsupported(ScopeOption.OptionSpan,
            'Scope rotation requires a literal angle in degrees');
          Continue;
        end;
        DX := Number * Pi / 180;
        Operation.XX := Cos(DX);
        Operation.XY := -Sin(DX);
        Operation.YX := Sin(DX);
        Operation.YY := Cos(DX);
      end
      else if (ScopeOption.Key = 'scale') or
        (ScopeOption.Key = 'xscale') or (ScopeOption.Key = 'yscale') then
      begin
        if not ParseInvariantFloat(ScopeOption.Value, Number) or
          (Abs(Number) <= 1E-12) then
        begin
          Unsupported(ScopeOption.OptionSpan,
            'Scope scale requires a nonzero literal number');
          Continue;
        end;
        if ScopeOption.Key <> 'yscale' then Operation.XX := Number;
        if ScopeOption.Key <> 'xscale' then Operation.YY := Number;
      end
      else if (ScopeOption.Key = 'transparency group') and
        not ScopeOption.HasValue then
        Continue
      else if ScopeOption.Key = 'opacity' then
      begin
        if not ParseInvariantFloat(TrimASCII(ScopeOption.Value), Number) or
          (Number < 0) or (Number > 1) then
          Unsupported(ScopeOption.OptionSpan,
            'Scope opacity requires a literal number from 0 to 1')
        else
        begin
          FScopeOpacity.Opacity := FScopeOpacity.Opacity * Number;
          FScopeOpacity.Option := ScopeOption;
        end;
        Continue;
      end
      else
      begin
        Unsupported(ScopeOption.OptionSpan,
          'Unsupported scope transform option: ' + ScopeOption.Key);
        Continue;
      end;
      { PGF applies TikZ transform options in source order to points: the
        first listed operation is innermost and the last is outermost. }
      Transform := ComposeTikZTransform(Transform, Operation);
    end;
    Transform := ComposeTikZTransform(FCurrentTransform, Transform);
  end;

begin
  FCurrentTransform := IdentityTikZTransform;
  FScopeOpacity.Opacity := 1;
  ScopeStack := nil;
  OpacityStack := nil;
  PendingFill := -1;
  ColorsSincePending := '';
  I := FDocument.Region.FirstContentToken;
  EndToken := FDocument.Region.PastLastContentToken;
  while I < EndToken do
  begin
    Tok := FDocument.TokenAt(I);
    if Tok.Kind in [ttkWhitespace, ttkComment] then
    begin
      Inc(I);
      Continue;
    end;
    if Tok.Kind = ttkControlWord then
    begin
      Command := TokenText(I);
      if (Command = '\begin') or (Command = '\end') then
      begin
        PendingFill := -1;
        J := I + 1;
        if not ReadScopeName(J, ScopeName) then
        begin
          Unsupported(Tok.Span, 'Malformed scope environment directive');
          Inc(I);
          Continue;
        end;
        if ScopeName <> 'scope' then
        begin
          Unsupported(Tok.Span,
            'Only the TikZ scope environment is supported');
          I := J;
          Continue;
        end;
        if Command = '\end' then
        begin
          if Length(ScopeStack) = 0 then
            Invalid(Tok.Span, 'TikZ scope closes without a matching begin')
          else
          begin
            FCurrentTransform := ScopeStack[High(ScopeStack)];
            SetLength(ScopeStack, Length(ScopeStack) - 1);
            FScopeOpacity := OpacityStack[High(OpacityStack)];
            SetLength(OpacityStack, Length(OpacityStack) - 1);
          end;
          I := J;
          Continue;
        end;
        PreviousTransform := FCurrentTransform;
        K := Length(ScopeStack);
        SetLength(ScopeStack, K + 1);
        ScopeStack[K] := PreviousTransform;
        SetLength(OpacityStack, K + 1);
        OpacityStack[K] := FScopeOpacity;
        J := NextSignificant(J, EndToken);
        ScopeTransform := IdentityTikZTransform;
        if (J < EndToken) and (FOpenGroups[J] >= 0) and
          (FDocument.GroupAt(FOpenGroups[J]).Kind = tgBracket) then
        begin
          ScopeGroup := FOpenGroups[J];
          CloseToken := FDocument.GroupAt(ScopeGroup).CloseToken;
          ApplyScopeOptions(ScopeGroup, ScopeTransform);
          J := CloseToken + 1;
        end
        else
          ScopeTransform := FCurrentTransform;
        FCurrentTransform := ScopeTransform;
        I := J;
        Continue;
      end;
      if Command = '\definecolor' then
      begin
        J := I;
        ParseColorDefinition(J, EndToken);
        if (PendingFill >= 0) and (J > I) then
        begin
          K := NextSignificant(I + 1, EndToken);
          if (K < EndToken) and (FOpenGroups[K] >= 0) then
            ColorsSincePending := ColorsSincePending + ',' +
              string(SourceText(SpanBetween(K + 1,
              FDocument.GroupAt(FOpenGroups[K]).CloseToken))) + ',';
        end;
        if J > I then I := J else Inc(I);
        Continue;
      end;
      if Command = '\pgfinterruptpicture' then
      begin
        J := ParseFadingDeclaration(I, EndToken);
        if J < 0 then
        begin
          Unsupported(Tok.Span,
            'Only the fading declarations TpX writes are supported in ' +
            '\pgfinterruptpicture');
          Inc(I);
        end
        else I := J;
        Continue;
      end;
      if (Command = '\path') or (Command = '\draw') or
        (Command = '\fill') or (Command = '\filldraw') or
        (Command = '\node') then
      begin
        J := I + 1;
        while J < EndToken do
        begin
          Tok := FDocument.TokenAt(J);
          if (Tok.Kind in [ttkOpenBrace, ttkOpenBracket, ttkOpenParen]) and
            (FOpenGroups[J] >= 0) then
          begin
            J := FDocument.GroupAt(FOpenGroups[J]).CloseToken + 1;
            Continue;
          end;
          if Tok.Kind = ttkSemicolon then Break;
          Inc(J);
        end;
        if J >= EndToken then
        begin
          Invalid(Tok.Span, 'TikZ statement is missing its semicolon');
          Exit;
        end;
        CountBefore := FResult.Scene.ObjectCount;
        ParseStatement(I, J, Copy(Command, 2, MaxInt));
        if FResult.Scene.ObjectCount = CountBefore + 1 then
        begin
          if PendingFill >= 0 then
            LinkStrokeCompanion(PendingFill, ColorsSincePending);
          PendingFill := -1;
          ColorsSincePending := '';
          if FResult.Scene.ObjectAt(CountBefore).FadingName <> '' then
            PendingFill := CountBefore;
        end
        else
        begin
          PendingFill := -1;
          ColorsSincePending := '';
        end;
        I := J + 1;
        Continue;
      end;
      Unsupported(Tok.Span, 'Unsupported drawing command ' + Command);
      Inc(I);
      Continue;
    end;
    Unsupported(Tok.Span, 'Unexpected token outside a drawing command');
    Inc(I);
  end;
  if Length(ScopeStack) <> 0 then
    Invalid(SpanAtZero, 'TikZ scope is not closed');
  FCurrentTransform := IdentityTikZTransform;
  FScopeOpacity.Opacity := 1;
end;

procedure TTikZEvaluator.Run;
var
  I: SizeInt;
  Diag: TTikZDiagnostic;
begin
  FResult.Outcome := tsoAccepted;
  if FDocument = nil then
  begin
    Invalid(SpanAtZero, 'TikZ syntax result is missing');
    Exit;
  end;
  for I := 0 to FDocument.DiagnosticCount - 1 do
  begin
    Diag := FDocument.DiagnosticAt(I);
    AddDiagnostic(Diag.Code, Diag.Severity, Diag.Span, Diag.Message);
  end;
  if FDocument.Outcome = tpoCancelled then
  begin
    FResult.Outcome := tsoCancelled;
    Exit;
  end;
  if FDocument.Outcome = tpoInvalid then FResult.Outcome := tsoInvalid
  else if FDocument.Outcome = tpoUnsupported then FResult.Outcome := tsoUnsupported;
  if not FDocument.Region.Found then
  begin
    Invalid(SpanAtZero, 'No uniquely selected TikZ picture is available');
    Exit;
  end;
  FResult.Scene.Source := FDocument.CopySource;
  EvaluatePreamble;
  if FResult.Outcome = tsoCancelled then Exit;
  EvaluatePictureHeader;
  if FResult.Outcome = tsoCancelled then Exit;
  EvaluatePicture;
end;

procedure TTikZEvaluator.ParseStatement(CommandIndex, Limit: SizeInt;
  const Command: string);
var
  I, GroupIndex, StyleGroup, BodyGroup, Semi: SizeInt;
  CommandName, Dependency, ColorName, TextAnchor, Body: string;
  StyleOptions: TTikZOptionValues;
  StyleSpan, StatementSpan, PointSpan, XSpan, YSpan: TTikZSourceSpan;
  Point, CurrentPoint, RelativeBase, FirstPoint: TTikZPoint;
  UpdateBase, IsRelative: Boolean;
  XScale, YScale, Value, Angle: Double;
  XUnit, YUnit: string;
  Obj: TTikZSceneObject;
  Commands: TTikZPathCommands;
  PathCommand: TTikZPathCommand;
  OpIndex, NextIndex, Control1Index: SizeInt;
  TempGroup: TTikZGroup;
  HavePoint, PathHasSegment, ClosedPath: Boolean;
  RotationDegrees: Double;
  RotationCenter: TTikZPoint;
  RotationSpan, RotationKeySpan, RotationOptionsSpan: TTikZSourceSpan;
  Rotated: Boolean;
  ArcStartDegrees, ArcEndDegrees, ArcRadiusMM, ArcRadiusScale: Double;
  ArcStartSpan, ArcEndSpan, ArcRadiusSpan: TTikZSourceSpan;
  ArcDependency, ArcUnit: string;
  ArcIsSector, ArcCycle: Boolean;
  ArcCenter, ArcStartPoint: TTikZPoint;
  ArcDistance, C, S, DX, DY: Double;
  DependencySource: TTikZDependencySource;

  procedure ParseStyle(AObj: TTikZSceneObject; AGroup: SizeInt;
    const RootCommand: string; IsNodeStyle: Boolean);
  var
    K, J, SepAt, OnAt, OffAt: SizeInt;
    Item: TTikZOptionValue;
    OptSpan, KeySpan, DefSpan: TTikZSourceSpan;
    CName, S, Pattern, PatternSearch, OnText, OffText, DefText: string;
    RGB: LongWord;
    ColorKind: TTikZDependencyKind;
    MacroIndex: SizeInt;
    OnMM, OffMM: Double;
    OnDependency, OffDependency: string;
    ColorSource, ValueSource: TTikZDependencySource;
    ShadeKind: TTikZGradientKind;
    ShadeAngle: Double;
    TopRGB, BottomRGB, InnerRGB, OuterRGB: LongWord;
    TopName, BottomName, InnerName, OuterName: string;
    HaveTop, HaveBottom, HaveInner, HaveOuter: Boolean;
    ShadeSpan, FadingSpan: TTikZSourceSpan;
    FitFading: Integer;
    HaveFadingTransform, LeftRight: Boolean;
    Fading: TTikZFadingDeclaration;
    Turn: Double;
    procedure AddFillPaint;
    begin
      AddDependency(AObj, tdFillPaint, tdepLocalStyle, Item.Key,
        Item.ValueSpan, Item.KeySpan, OptSpan, SpanAtZero, True,
        -1, -1, 1, '', False);
    end;
  begin
    AObj.LineWidthMM := FResult.Scene.LineWidthBaseMM;
    AObj.LineKind := tlSolid;
    AObj.StrokeRGB := $000000;
    AObj.FillRGB := $000000;
    AObj.StrokeOpacity := 1;
    AObj.FillOpacity := 1;
    AObj.Gradient := tgNone;
    AObj.GradientRGB := $000000;
    AObj.GradientAngle := 0;
    ShadeKind := tgNone;
    ShadeAngle := 0;
    HaveTop := False;
    HaveBottom := False;
    HaveInner := False;
    HaveOuter := False;
    ShadeSpan := SpanAtZero;
    FadingSpan := SpanAtZero;
    FitFading := -1;
    HaveFadingTransform := False;
    LeftRight := False;
    AObj.FadingName := '';
    AObj.FadingDeclaration := -1;
    AObj.StrokeEnabled := (RootCommand = 'draw') or (RootCommand = 'filldraw');
    AObj.FillEnabled := (RootCommand = 'fill') or (RootCommand = 'filldraw');
    OptSpan := SpanAtZero;
    if (AGroup >= 0) then
    begin
      OptSpan := FDocument.GroupAt(AGroup).Span;
      StyleOptions := ParseOptionGroup(AGroup);
    end
    else StyleOptions := nil;
    for K := 0 to High(StyleOptions) do
    begin
      Item := StyleOptions[K];
      CName := Item.Value;
      if IsNodeStyle and ((Item.Key = 'draw') or (Item.Key = 'fill')) then
      begin
        if not (Item.HasValue and (Item.Value = 'none')) then
          Unsupported(Item.OptionSpan,
            'TikZ node frames and fills are not represented by native text');
        Continue;
      end;
      if (not IsNodeStyle) and (Item.Key = 'text') then
      begin
        Unsupported(Item.OptionSpan,
          'TikZ text color must be specified on the node');
        Continue;
      end;
      if (Item.Key = 'draw') or (Item.Key = 'fill') or (Item.Key = 'text') then
      begin
        if Item.Key = 'draw' then AObj.StrokeEnabled := not SameText(CName, 'none')
        else if Item.Key = 'fill' then AObj.FillEnabled := not SameText(CName, 'none');
        if (Item.Key = 'text') and (CName = 'none') then
        begin
          Unsupported(Item.OptionSpan,
            'TikZ text color none is not represented by native text');
          Continue;
        end;
        if SameText(CName, 'none') then Continue;
        if CName = '' then CName := 'black';
        if not ParseRGB(CName, RGB) then
        begin
          Unsupported(Item.OptionSpan, 'Unresolved TikZ color: ' + CName);
          Continue;
        end;
        if Item.Key = 'fill' then
        begin
          AObj.FillRGB := RGB;
          AObj.FillColorName := CName;
        end
        else AObj.StrokeRGB := RGB;
        ColorSource := tdepLocalLiteral;
        DefSpan := SpanAtZero;
        if FColors.IndexOfName(CName) >= 0 then
        begin
          ColorSource := tdepNamedColor;
          DefText := FColorSpans.Values[CName];
          SepAt := Pos(':', DefText);
          if SepAt > 0 then
          begin
            DefSpan.StartByte := StrToIntDef(Copy(DefText, 1, SepAt - 1), 0);
            DefSpan.EndByte := StrToIntDef(Copy(DefText, SepAt + 1, MaxInt), 0);
          end;
        end;
        if Item.Key = 'fill' then ColorKind := tdFillColor
        else if Item.Key = 'text' then ColorKind := tdTextColor
        else ColorKind := tdStrokeColor;
        AddDependency(AObj, ColorKind, ColorSource, CName,
          Item.ValueSpan, Item.KeySpan, OptSpan, DefSpan,
          (ColorSource = tdepLocalLiteral) or (Item.Key = 'text'),
          -1, -1, 1, '', False);
      end
      else if not Item.HasValue then
      begin
        CName := TrimASCII(string(SourceText(Item.OptionSpan)));
        if Item.Key = 'draw' then
          AObj.StrokeEnabled := True
        else if Item.Key = 'fill' then
          AObj.FillEnabled := True
        else if (not IsNodeStyle) and
          ParseTikZArrowSpec(Item.Key, AObj.BeginArrow, AObj.EndArrow) then
          AddDependency(AObj, tdArrows, tdepLocalStyle, Item.Key,
            Item.KeySpan, Item.KeySpan, OptSpan, SpanAtZero, True,
            -1, -1, 1, '', False)
        else if (Item.Key = 'dashed') or (Item.Key = 'dotted') or
          (Item.Key = 'solid') then
        begin
          if Item.Key = 'solid' then
          begin
            AObj.LineKind := tlSolid;
            AObj.DashOnMM := 0;
            AObj.DashOffMM := 0;
          end
          else if Item.Key = 'dashed' then
          begin
            AObj.LineKind := tlDashed;
            AObj.DashOnMM := 2 * FResult.Scene.DashSizeMM;
            AObj.DashOffMM := FResult.Scene.DashSizeMM;
          end
          else
          begin
            AObj.LineKind := tlDotted;
            AObj.DashOnMM := AObj.LineWidthMM;
            AObj.DashOffMM := FResult.Scene.DotSizeMM;
          end;
          AddDependency(AObj, tdDashSize, tdepLocalStyle, Item.Key,
            Item.KeySpan, Item.KeySpan, OptSpan, SpanAtZero, True,
            -1, -1, 1, 'mm', False);
        end
        else if ParseRGB(CName, RGB) then
        begin
          AObj.StrokeRGB := RGB;
          DefSpan := SpanAtZero;
          DefText := FColorSpans.Values[CName];
          SepAt := Pos(':', DefText);
          if SepAt > 0 then
          begin
            DefSpan.StartByte := StrToIntDef(Copy(DefText, 1, SepAt - 1), 0);
            DefSpan.EndByte := StrToIntDef(Copy(DefText, SepAt + 1, MaxInt), 0);
          end;
          if IsNodeStyle then ColorKind := tdTextColor
          else ColorKind := tdStrokeColor;
          AddDependency(AObj, ColorKind, tdepNamedColor, CName,
            Item.OptionSpan, SpanAtZero, OptSpan, DefSpan, IsNodeStyle,
            -1, -1, 1, '', False);
        end
        else Unsupported(Item.OptionSpan,
          'Unsupported TikZ style or unresolved color: ' + CName);
      end
      else if Item.Key = 'line width' then
      begin
        if not ParseLengthExpression(Item.Value, Value, Dependency) then
        begin
          Unsupported(Item.OptionSpan, 'Unsupported line width expression');
          Continue;
        end;
        AObj.LineWidthMM := Value;
        DefSpan := SpanAtZero;
        for MacroIndex := 0 to High(FResult.Scene.MacroDefinitions) do
          if FResult.Scene.MacroDefinitions[MacroIndex].Name = Dependency then
            DefSpan := FResult.Scene.MacroDefinitions[MacroIndex].ExpressionSpan;
        if Dependency = '' then ValueSource := tdepLocalLiteral
        else ValueSource := tdepSharedDefaultMacro;
        AddDependency(AObj, tdLineWidth, ValueSource,
          Dependency, Item.ValueSpan, Item.KeySpan, OptSpan, DefSpan,
          True, -1, -1, 1, 'mm', False);
      end
      else if Item.Key = 'dash pattern' then
      begin
        Pattern := ' ' + CName + ' ';
        PatternSearch := ' ' + LowerASCII(CName) + ' ';
        OnAt := Pos(' on ', PatternSearch);
        OffAt := Pos(' off ', PatternSearch);
        if (OnAt = 0) or (OffAt <= OnAt) then
        begin
          Unsupported(Item.OptionSpan, 'Unsupported custom dash pattern');
          Continue;
        end;
        OnText := TrimASCII(Copy(Pattern, OnAt + 4, OffAt - OnAt - 4));
        OffText := TrimASCII(Copy(Pattern, OffAt + 5,
          Length(Pattern) - OffAt - 1));
        if not ParseLengthExpression(OnText, OnMM, OnDependency) or
          not ParseLengthExpression(OffText, OffMM, OffDependency) then
        begin
          Unsupported(Item.OptionSpan, 'Unsupported dash length expression');
          Continue;
        end;
        AObj.DashOnMM := OnMM;
        AObj.DashOffMM := OffMM;
        if (Abs(OnMM - 2 * OffMM) <= 1E-8) then
        begin
          AObj.LineKind := tlDashed;
          FResult.Scene.DashSizeMM := OffMM;
        end
        else if Abs(OnMM - AObj.LineWidthMM) <= 1E-8 then
        begin
          AObj.LineKind := tlDotted;
          FResult.Scene.DotSizeMM := OffMM;
        end
        else
          Unsupported(Item.OptionSpan,
            'Dash pattern is not representable by the native line styles');
        if (OnDependency = '') and (OffDependency = '') then
          ValueSource := tdepLocalLiteral
        else ValueSource := tdepSharedDefaultMacro;
        AddDependency(AObj, tdDashSize, ValueSource,
          OffDependency, Item.ValueSpan, Item.KeySpan, OptSpan, SpanAtZero,
          True, -1, -1, 1, 'mm', False);
      end
      else if IsNodeStyle and (Item.Key = 'anchor') then
      begin
        AObj.TextAnchor := LowerASCII(TrimASCII(CName));
        AddDependency(AObj, tdTextAnchor, tdepLocalLiteral, '',
          Item.ValueSpan, Item.KeySpan, OptSpan, SpanAtZero, True,
          -1, -1, 1, '', False);
      end
      else if IsNodeStyle and (Item.Key = 'rotate') then
      begin
        if not ParseInvariantFloat(CName, Angle) then
          Unsupported(Item.OptionSpan, 'Unsupported node rotation')
        else
        begin
          AObj.Rotation := Angle * Pi / 180;
          AddDependency(AObj, tdTextRotation, tdepLocalLiteral, '',
            Item.ValueSpan, Item.KeySpan, OptSpan, SpanAtZero, True,
            -1, -1, Pi / 180, 'deg', False);
        end;
      end
      else if (not IsNodeStyle) and ((Item.Key = 'left color') or
        (Item.Key = 'right color') or (Item.Key = 'top color') or
        (Item.Key = 'bottom color') or (Item.Key = 'inner color') or
        (Item.Key = 'outer color')) then
      begin
        { Applied in order with the TikZ semantics: left/right set the
          top/bottom colour and an angle of 90, top/bottom an angle of 0. }
        if not ParseRGB(TrimASCII(CName), RGB) then
        begin
          Unsupported(Item.OptionSpan, 'Unresolved TikZ color: ' + CName);
          Continue;
        end;
        ShadeSpan := Item.OptionSpan;
        AddFillPaint;
        if (Item.Key = 'inner color') or (Item.Key = 'outer color') then
        begin
          ShadeKind := tgRadial;
          if Item.Key = 'inner color' then
          begin
            InnerRGB := RGB;
            InnerName := TrimASCII(CName);
            HaveInner := True;
          end
          else
          begin
            OuterRGB := RGB;
            OuterName := TrimASCII(CName);
            HaveOuter := True;
          end;
        end
        else
        begin
          ShadeKind := tgLinear;
          if (Item.Key = 'left color') or (Item.Key = 'top color') then
          begin
            TopRGB := RGB;
            TopName := TrimASCII(CName);
            HaveTop := True;
          end
          else
          begin
            BottomRGB := RGB;
            BottomName := TrimASCII(CName);
            HaveBottom := True;
          end;
          if (Item.Key = 'left color') or (Item.Key = 'right color') then
            ShadeAngle := 90
          else ShadeAngle := 0;
          LeftRight := (Item.Key = 'left color') or (Item.Key = 'right color');
        end;
      end
      else if (not IsNodeStyle) and (Item.Key = 'shading angle') then
      begin
        if not ParseInvariantFloat(TrimASCII(CName), Value) then
          Unsupported(Item.OptionSpan, 'Unsupported TikZ shading angle')
        else ShadeAngle := Value;
        AddFillPaint;
      end
      else if (not IsNodeStyle) and (Item.Key = 'path fading') then
      begin
        { Only the fadings TpX declares itself (see EvaluatePicture). }
        MacroIndex := FResult.Scene.FindFading(TrimASCII(CName));
        if MacroIndex < 0 then
          Unsupported(Item.OptionSpan, 'Unsupported TikZ fading: ' + CName)
        else
        begin
          AObj.FadingName := TrimASCII(CName);
          AObj.FadingDeclaration := MacroIndex;
          FadingSpan := Item.OptionSpan;
        end;
        AddFillPaint;
      end
      else if (not IsNodeStyle) and (Item.Key = 'fit fading') then
      begin
        if TrimASCII(CName) = 'true' then FitFading := 1
        else if TrimASCII(CName) = 'false' then FitFading := 0
        else Unsupported(Item.OptionSpan, 'Unsupported TikZ fit fading: ' + CName);
        AddFillPaint;
      end
      else if (not IsNodeStyle) and (Item.Key = 'fading transform') then
      begin
        if ParseFadingTransform(CName, AObj) then HaveFadingTransform := True
        else Unsupported(Item.OptionSpan,
          'Unsupported TikZ fading transform: ' + CName);
        AddFillPaint;
      end
      else if (not IsNodeStyle) and (Item.Key = 'shading') then
      begin
        AddFillPaint;
        if TrimASCII(CName) = 'axis' then ShadeKind := tgLinear
        else if TrimASCII(CName) = 'radial' then ShadeKind := tgRadial
        else Unsupported(Item.OptionSpan, 'Unsupported TikZ shading: ' + CName);
      end
      else if (not IsNodeStyle) and ((Item.Key = 'opacity') or
        (Item.Key = 'fill opacity') or (Item.Key = 'draw opacity')) then
      begin
        if not ParseInvariantFloat(TrimASCII(CName), Value) or
          (Value < 0) or (Value > 1) then
          Unsupported(Item.OptionSpan, 'Unsupported TikZ opacity: ' + CName)
        else
        begin
          if Item.Key <> 'fill opacity' then AObj.StrokeOpacity := Value;
          if Item.Key <> 'draw opacity' then AObj.FillOpacity := Value;
          if Item.Key = 'opacity' then ColorKind := tdOpacity
          else if Item.Key = 'fill opacity' then ColorKind := tdFillOpacity
          else ColorKind := tdStrokeOpacity;
          AddDependency(AObj, ColorKind, tdepLocalLiteral, '',
            Item.ValueSpan, Item.KeySpan, OptSpan, SpanAtZero, True,
            -1, -1, 1, '', False);
        end;
      end
      else if (Item.Key = 'inner sep') or (Item.Key = 'outer sep') or
        (Item.Key = 'inner xsep') or (Item.Key = 'inner ysep') or
        (Item.Key = 'outer xsep') or (Item.Key = 'outer ysep') then
      begin
        if not ParseLengthExpression(CName, Value, Dependency) or
          (Abs(Value) > 1E-12) then
          Unsupported(Item.OptionSpan,
            'Nonzero node separation changes the native geometry');
      end
      else
        Unsupported(Item.OptionSpan, 'Unsupported TikZ option: ' + Item.Key);
    end;
    if ShadeKind = tgRadial then
    begin
      if not (HaveInner and HaveOuter) then
        Unsupported(ShadeSpan, 'A radial shading needs an inner and an outer color')
      else
      begin
        AObj.Gradient := tgRadial;
        AObj.FillEnabled := True;
        AObj.FillRGB := InnerRGB;
        AObj.GradientRGB := OuterRGB;
        AObj.FillColorName := InnerName;
        AObj.GradientColorName := OuterName;
      end;
    end
    else if ShadeKind = tgLinear then
    begin
      if not (HaveTop and HaveBottom) then
        Unsupported(ShadeSpan, 'An axis shading needs both end colors')
      else
      begin
        { The axis shading runs from bottom to top, rotated by its angle;
          left and right colours, a run from right to left, are kept as
          left to right. }
        AObj.Gradient := tgLinear;
        AObj.FillEnabled := True;
        AObj.GradientAngle := 90 + ShadeAngle;
        AObj.GradientAngle := AObj.GradientAngle -
          360 * Floor(AObj.GradientAngle / 360);
        if LeftRight and (Abs(AObj.GradientAngle - 180) < 1E-9) then
        begin
          AObj.GradientAngle := 0;
          AObj.FillRGB := TopRGB;
          AObj.GradientRGB := BottomRGB;
          AObj.FillColorName := TopName;
          AObj.GradientColorName := BottomName;
        end
        else
        begin
          AObj.FillRGB := BottomRGB;
          AObj.GradientRGB := TopRGB;
          AObj.FillColorName := BottomName;
          AObj.GradientColorName := TopName;
        end;
      end;
    end;
    { A fading is accepted exactly in the form TpX writes for a gradient
      whose ends differ in opacity: on an unstroked shading of the same
      kind, without a fill opacity of its own, fitted to the path when
      radial and placed along the gradient direction when linear. }
    if AObj.FadingName <> '' then
    begin
      Fading := FResult.Scene.Fadings[AObj.FadingDeclaration];
      { The fading runs from its start opacity along its rotation. }
      Turn := AObj.FadingRotation - AObj.GradientAngle;
      Turn := Turn - 360 * Round(Turn / 360);
      if (AObj.Gradient = tgNone) or
        (Fading.Radial <> (AObj.Gradient = tgRadial)) then
        Unsupported(FadingSpan,
          'This TikZ fading does not belong to the shading of its path')
      else if AObj.StrokeEnabled then
        Unsupported(FadingSpan,
          'A faded TikZ shading is drawn without a stroke; TpX strokes it ' +
          'as a separate path')
      else if (FindDependencyKind(AObj, tdFillOpacity) >= 0) or
        (FindDependencyKind(AObj, tdOpacity) >= 0) then
        Unsupported(FadingSpan,
          'A faded TikZ shading takes both opacities from its fading')
      else if (FCurrentTransform.XX <> 1) or (FCurrentTransform.XY <> 0) or
        (FCurrentTransform.YX <> 0) or (FCurrentTransform.YY <> 1) or
        (FCurrentTransform.TX <> 0) or (FCurrentTransform.TY <> 0) or
        (FResult.Scene.PictureShearX <> 0) or
        (FResult.Scene.PictureShearY <> 0) then
        Unsupported(FadingSpan,
          'A TikZ fading inside a transformed scope is not supported')
      else if FScopeOpacity.Opacity < 1 then
        Unsupported(FadingSpan,
          'A TikZ fading inside a translucent scope is not supported')
      else if Fading.Radial and ((FitFading <> 1) or HaveFadingTransform) then
        Unsupported(FadingSpan,
          'A radial TpX fading must be fitted to its path')
      else if not Fading.Radial and ((FitFading <> 0) or
        not HaveFadingTransform or (Abs(Turn) > 0.01)) then
        Unsupported(FadingSpan,
          'A linear TpX fading must follow the gradient direction')
      else
      begin
        AObj.FillOpacity := Fading.StartOpacity;
        AObj.GradientOpacity := Fading.EndOpacity;
      end;
    end
    else
    begin
      if (FitFading >= 0) or HaveFadingTransform then
        Unsupported(OptSpan, 'TikZ fading options need a TpX path fading');
      AObj.GradientOpacity := AObj.FillOpacity;
    end;
    if ((AObj.BeginArrow <> '') or (AObj.EndArrow <> '')) and
      not AObj.StrokeEnabled then
      Unsupported(OptSpan,
        'Arrow tips on an unstroked TikZ path are not native arrowheads');
  end;

  procedure AddPointDependencies(AObj: TTikZSceneObject; CommandIndex,
    PointSlot: SizeInt; const AXSpan, AYSpan: TTikZSourceSpan;
    XScale, YScale: Double; const AXUnit, AYUnit: string;
    Relative, UpdatesRelativeBase: Boolean);
  var
    SourceKind: TTikZDependencySource;
    N: SizeInt;
  begin
    SourceKind := tdepLocalLiteral;
    if (FResult.Scene.PictureShearX <> 0) or
      (FResult.Scene.PictureShearY <> 0) then SourceKind := tdepUnsupported;
    AddDependency(AObj, tdCoordinateX, SourceKind, '', AXSpan, SpanAtZero,
      SpanAtZero, SpanAtZero, SourceKind <> tdepUnsupported, CommandIndex,
      PointSlot, XScale, AXUnit, Relative);
    N := High(AObj.Dependencies);
    AObj.Dependencies[N].UpdatesRelativeBase := UpdatesRelativeBase;
    AddDependency(AObj, tdCoordinateY, SourceKind, '', AYSpan, SpanAtZero,
      SpanAtZero, SpanAtZero, SourceKind <> tdepUnsupported, CommandIndex,
      PointSlot, YScale, AYUnit, Relative);
    N := High(AObj.Dependencies);
    AObj.Dependencies[N].UpdatesRelativeBase := UpdatesRelativeBase;
  end;

  function MakeStyledObject: TTikZSceneObject;
  begin
    Result := TTikZSceneObject.Create;
    ParseStyle(Result, StyleGroup, Command, False);
  end;

  function ConsumeLiteral(var Cursor: SizeInt; const Literal: string): Boolean;
  var
    StartByte, J, L: SizeInt;
    T: TTikZToken;
  begin
    Result := False;
    Cursor := NextSignificant(Cursor, Limit);
    if Cursor >= Limit then Exit;
    T := FDocument.TokenAt(Cursor);
    StartByte := T.Span.StartByte;
    L := Length(Literal);
    for J := 1 to L do
    begin
      if (Cursor >= Limit) then Exit;
      T := FDocument.TokenAt(Cursor);
      if (T.Span.StartByte <> StartByte + J - 1) or
        (T.Span.EndByte <> T.Span.StartByte + 1) or
        (FSource[T.Span.StartByte] <> Ord(Literal[J])) then Exit;
      Inc(Cursor);
    end;
    Result := True;
  end;

  function ConsumeWord(var Cursor: SizeInt; const AWord: string): Boolean;
  begin
    Cursor := NextSignificant(Cursor, Limit);
    Result := (Cursor < Limit) and
      (LowerASCII(TokenText(Cursor)) = LowerASCII(AWord));
    if Result then Inc(Cursor);
  end;

  procedure AppendPathCommand(const ACommand: TTikZPathCommand);
  var
    N: SizeInt;
  begin
    Inc(FPathCommandCount);
    if (FOptions.MaxPathCommands > 0) and
      (FPathCommandCount > FOptions.MaxPathCommands) then
    begin
      Invalid(StatementSpan, 'TikZ import exceeds the path command limit');
      Exit;
    end;
    N := Length(Commands);
    SetLength(Commands, N + 1);
    Commands[N] := ACommand;
  end;

  procedure FlushPath(CreateNextObject: Boolean);
  var
    K: SizeInt;
    C, S: Double;
    procedure Rotate(var APoint: TTikZPoint);
    var
      DX, DY: Double;
    begin
      DX := APoint.X - RotationCenter.X;
      DY := APoint.Y - RotationCenter.Y;
      APoint.X := RotationCenter.X + C * DX - S * DY;
      APoint.Y := RotationCenter.Y + S * DX + C * DY;
    end;
  begin
    if (Length(Commands) = 0) then Exit;
    if Rotated then
    begin
      C := Cos(RotationDegrees * Pi / 180);
      S := Sin(RotationDegrees * Pi / 180);
      for K := 0 to High(Commands) do
        case Commands[K].Kind of
          tpcMove, tpcLine: Rotate(Commands[K].P1);
          tpcCubic:
            begin
              Rotate(Commands[K].P1);
              Rotate(Commands[K].P2);
              Rotate(Commands[K].P3);
            end;
        end;
    end;
    Obj.Kind := tsoPath;
    Obj.Commands := Copy(Commands);
    if Rotated then
    begin
      Obj.SourceRotation := RotationDegrees * Pi / 180;
      Obj.SourceRotationCenter := RotationCenter;
    end
    else
    begin
      Obj.SourceRotation := 0;
      Obj.SourceRotationCenter.X := 0;
      Obj.SourceRotationCenter.Y := 0;
    end;
    AddSemanticObject(Obj, StatementSpan);
    if CreateNextObject then Obj := MakeStyledObject else Obj := nil;
    Commands := nil;
    HavePoint := False;
    PathHasSegment := False;
    ClosedPath := False;
  end;

  function ReadPoint(var Cursor: SizeInt; const Base: TTikZPoint;
    PointSlot: SizeInt;
    out APoint: TTikZPoint; out ASpan, AXSpan, AYSpan: TTikZSourceSpan;
    out Relative: Boolean; out UpdatesBase: Boolean): Boolean;
  var
    SX, SY: Double;
    UX, UY: string;
  begin
    Result := TryReadCoordinate(Cursor, Limit, Base, APoint, UpdatesBase,
      ASpan, AXSpan, AYSpan, Relative, SX, SY, UX, UY);
    if Result then
    begin
      AddPointDependencies(Obj, Length(Commands), PointSlot, AXSpan, AYSpan,
        SX, SY, UX, UY, Relative, UpdatesBase);
      if not UpdatesBase and not Relative then
        Unsupported(ASpan, 'Coordinate does not update its relative base');
    end;
  end;

  function ParseRotateAround(const Text: string; out Degrees: Double;
    out Center: TTikZPoint): Boolean;
  var
    S, AngleText, XText, YText: string;
    ColonAt, OpenAt, CommaAt, CloseAt: SizeInt;
    X, Y: Double;
  begin
    Result := False;
    Degrees := 0;
    Center.X := 0;
    Center.Y := 0;
    S := TrimASCII(Text);
    if (Length(S) >= 2) and (S[1] = '{') and (S[Length(S)] = '}') then
      S := TrimASCII(Copy(S, 2, Length(S) - 2));
    ColonAt := Pos(':', S);
    OpenAt := Pos('(', S);
    CommaAt := Pos(',', S);
    CloseAt := Pos(')', S);
    if (ColonAt < 2) or (OpenAt <= ColonAt) or (CommaAt <= OpenAt) or
      (CloseAt <= CommaAt) then Exit;
    AngleText := TrimASCII(Copy(S, 1, ColonAt - 1));
    XText := TrimASCII(Copy(S, OpenAt + 1, CommaAt - OpenAt - 1));
    YText := TrimASCII(Copy(S, CommaAt + 1, CloseAt - CommaAt - 1));
    if not ParseInvariantFloat(AngleText, Degrees) or
      not ParseInvariantFloat(XText, X) or not ParseInvariantFloat(YText, Y) then
      Exit;
    Center.X := X * FResult.Scene.PictureScaleX;
    Center.Y := Y * FResult.Scene.PictureScaleY;
    Result := True;
  end;

  procedure ParseNode(IsAttached: Boolean; NodeStartToken: SizeInt);
  var
    Cursor, NodeStyle, NodeBodyGroup, IncludeStyle, IncludeFileGroup: SizeInt;
    NodePoint: TTikZPoint;
    NodeSpan, NXSpan, NYSpan, BodySpan, NodeSourceSpan: TTikZSourceSpan;
    Relative: Boolean;
    UpdatesBase: Boolean;
    BodyBytes, FileBytes: RawByteString;
    Opts: TTikZOptionValues;
    Width, Height: Double;
    WidthDep, HeightDep: string;
    WidthSpan, HeightSpan: TTikZSourceSpan;
    FoundImage, FoundWidth, FoundHeight: Boolean;
    K, N, ScanIndex, OptIndex: SizeInt;
    BodyGroupInfo: TTikZGroup;
    HasTextColor, HasFontSize, HasBaseline: Boolean;
    FontSizeMM, BaselineMM: Double;
    FontSizeSpan, BaselineSpan: TTikZSourceSpan;
    FontDependency, BaselineDependency: string;
    ParentStrokeRGB, ParsedColor: LongWord;
    BareColorName: string;
    ResumeObj: TTikZSceneObject;
    function FindGeneratedFontPayload(
      out PayloadSpan: TTikZSourceSpan): Boolean;
    var
      Scan, Cursor, NameGroup, ValueGroup, FontGroup, BaselineGroup,
        SelectFontToken, PayloadToken: SizeInt;
      NameText, ValueText, FontText, BaselineText: RawByteString;
      NameSpan, ValueSpan, FontSpan, FontBaselineSpan: TTikZSourceSpan;
    begin
      Result := False;
      PayloadSpan := BodySpan;
      Scan := NextSignificant(BodyGroupInfo.OpenToken + 1,
        BodyGroupInfo.CloseToken);
      if TokenText(Scan) <> '\pgfmathsetlengthmacro' then Exit;
      Cursor := Scan + 1;
      if not TryReadBraceGroup(Cursor, BodyGroupInfo.CloseToken,
        NameGroup, NameText, NameSpan) or
        (TrimASCII(string(NameText)) <> '\tpxObjectFontSize') or
        not TryReadBraceGroup(Cursor, BodyGroupInfo.CloseToken,
          ValueGroup, ValueText, ValueSpan) or
        (ValueSpan.StartByte <> FontSizeSpan.StartByte) or
        (ValueSpan.EndByte <> FontSizeSpan.EndByte) then Exit;

      Scan := NextSignificant(Cursor, BodyGroupInfo.CloseToken);
      if TokenText(Scan) <> '\pgfmathsetlengthmacro' then Exit;
      Cursor := Scan + 1;
      if not TryReadBraceGroup(Cursor, BodyGroupInfo.CloseToken,
        NameGroup, NameText, NameSpan) or
        (TrimASCII(string(NameText)) <> '\tpxObjectBaseline') or
        not TryReadBraceGroup(Cursor, BodyGroupInfo.CloseToken,
          ValueGroup, ValueText, ValueSpan) or
        (ValueSpan.StartByte <> BaselineSpan.StartByte) or
        (ValueSpan.EndByte <> BaselineSpan.EndByte) then Exit;

      Scan := NextSignificant(Cursor, BodyGroupInfo.CloseToken);
      if TokenText(Scan) <> '\fontsize' then Exit;
      Cursor := Scan + 1;
      if not TryReadBraceGroup(Cursor, BodyGroupInfo.CloseToken,
        FontGroup, FontText, FontSpan) or
        not TryReadBraceGroup(Cursor, BodyGroupInfo.CloseToken,
          BaselineGroup, BaselineText, FontBaselineSpan) or
        (TrimASCII(string(FontText)) <> '\tpxObjectFontSize') or
        (TrimASCII(string(BaselineText)) <> '\tpxObjectBaseline') then Exit;
      SelectFontToken := NextSignificant(Cursor, BodyGroupInfo.CloseToken);
      if TokenText(SelectFontToken) <> '\selectfont' then Exit;
      PayloadToken := NextSignificant(SelectFontToken + 1,
        BodyGroupInfo.CloseToken);
      if PayloadToken < BodyGroupInfo.CloseToken then
        PayloadSpan := SpanBetween(PayloadToken, BodyGroupInfo.CloseToken)
      else
      begin
        PayloadSpan := SpanAtZero;
        PayloadSpan.StartByte := BodySpan.EndByte;
        PayloadSpan.EndByte := BodySpan.EndByte;
        PayloadSpan.StartLine := BodySpan.EndLine;
        PayloadSpan.EndLine := BodySpan.EndLine;
        PayloadSpan.StartColumn := BodySpan.EndColumn;
        PayloadSpan.EndColumn := BodySpan.EndColumn;
      end;
      Result := True;
    end;
    procedure ReadTextFontMetrics(BodyGroupIndex: SizeInt);
    var
      Scan, Cursor, NameGroup, ValueGroup, FontGroup, BaselineGroup: SizeInt;
      NameText, FontText, BaselineText: RawByteString;
      NameSpan, ExprSpan, FontSpan, BaseSpan, PayloadSpan: TTikZSourceSpan;
      MacroValue: Double;
      SourceKind: TTikZDependencySource;
      DefinitionSpan: TTikZSourceSpan;
      MacroIndex: SizeInt;
    begin
      HasFontSize := False;
      HasBaseline := False;
      FontSizeMM := FResult.Scene.TextSizeMM;
      BaselineMM := 1.2 * FontSizeMM;
      FontSizeSpan := SpanAtZero;
      BaselineSpan := SpanAtZero;
      FontDependency := '';
      BaselineDependency := '';
      BodyGroupInfo := FDocument.GroupAt(BodyGroupIndex);
      Scan := BodyGroupInfo.OpenToken + 1;
      while Scan < BodyGroupInfo.CloseToken do
      begin
        if TokenText(Scan) = '\pgfmathsetlengthmacro' then
        begin
          Cursor := Scan + 1;
          if not TryReadBraceGroup(Cursor, BodyGroupInfo.CloseToken,
            NameGroup, NameText, NameSpan) or
            not TryReadBraceGroup(Cursor, BodyGroupInfo.CloseToken,
              ValueGroup, FontText, ExprSpan) then
          begin
            Unsupported(TokenSpan(Scan),
              'Malformed local TikZ text-size definition');
            Inc(Scan);
            Continue;
          end;
          NameText := TrimASCII(string(NameText));
          if NameText = '\tpxObjectFontSize' then
          begin
            if not ParseLengthExpression(string(FontText), MacroValue,
              FontDependency) or (MacroValue <= 0) then
              Unsupported(ExprSpan,
                'Unsupported local TikZ text-size expression')
            else
            begin
              FontSizeMM := MacroValue;
              FontSizeSpan := ExprSpan;
              HasFontSize := True;
            end;
          end
          else if NameText = '\tpxObjectBaseline' then
          begin
            if not ParseLengthExpression(string(FontText), MacroValue,
              BaselineDependency) or (MacroValue <= 0) then
              Unsupported(ExprSpan,
                'Unsupported local TikZ text-baseline expression')
            else
            begin
              BaselineMM := MacroValue;
              BaselineSpan := ExprSpan;
              HasBaseline := True;
            end;
          end
          else
            Unsupported(NameSpan,
              'Unsupported local TikZ text-size macro: ' + string(NameText));
          Scan := Cursor;
          Continue;
        end;
        if TokenText(Scan) = '\fontsize' then
        begin
          Cursor := Scan + 1;
          if not TryReadBraceGroup(Cursor, BodyGroupInfo.CloseToken,
            FontGroup, FontText, FontSpan) or
            not TryReadBraceGroup(Cursor, BodyGroupInfo.CloseToken,
              BaselineGroup, BaselineText, BaseSpan) then
          begin
            Unsupported(TokenSpan(Scan), 'Malformed TikZ fontsize wrapper');
            Inc(Scan);
            Continue;
          end;
          if (TrimASCII(string(FontText)) <> '\tpxObjectFontSize') or
            (TrimASCII(string(BaselineText)) <> '\tpxObjectBaseline') or
            not HasFontSize or not HasBaseline then
            Unsupported(SpanBetween(Scan, Cursor),
              'Unsupported TikZ fontsize wrapper');
          if HasFontSize and HasBaseline then
          begin
            if FontDependency = '' then SourceKind := tdepLocalLiteral
            else SourceKind := tdepSharedDefaultMacro;
            DefinitionSpan := SpanAtZero;
            for MacroIndex := 0 to High(FResult.Scene.MacroDefinitions) do
              if FResult.Scene.MacroDefinitions[MacroIndex].Name =
                FontDependency then
                DefinitionSpan :=
                  FResult.Scene.MacroDefinitions[MacroIndex].ExpressionSpan;
            AddDependency(Obj, tdTextSize, SourceKind, FontDependency,
              FontSizeSpan,
              SpanAtZero, SpanAtZero, DefinitionSpan, True, -1, -1, 1,
              'mm', False);

            if BaselineDependency = '' then SourceKind := tdepLocalLiteral
            else SourceKind := tdepSharedDefaultMacro;
            DefinitionSpan := SpanAtZero;
            for MacroIndex := 0 to High(FResult.Scene.MacroDefinitions) do
              if FResult.Scene.MacroDefinitions[MacroIndex].Name =
                BaselineDependency then
                DefinitionSpan :=
                  FResult.Scene.MacroDefinitions[MacroIndex].ExpressionSpan;
            AddDependency(Obj, tdTextBaseline, SourceKind,
              BaselineDependency, BaselineSpan, SpanAtZero, SpanAtZero,
              DefinitionSpan, True, -1, -1, 1, 'mm', False);
          end;
          Scan := Cursor;
          Continue;
        end;
        Inc(Scan);
      end;
      if HasFontSize then Obj.TextHeightMM := FontSizeMM;
      if HasBaseline then Obj.TextBaselineMM := BaselineMM;
      if HasFontSize and HasBaseline and
        FindGeneratedFontPayload(PayloadSpan) then
        Obj.TextPayloadSpan := PayloadSpan;
    end;
  begin
    NodeSpan := SpanAtZero;
    NXSpan := SpanAtZero;
    NYSpan := SpanAtZero;
    if IsAttached then
    begin
      NodePoint := CurrentPoint;
      Cursor := NextSignificant(I, Limit);
      NodeStyle := -1;
    end
    else
    begin
      Cursor := NextSignificant(CommandIndex + 1, Limit);
      if StyleGroup >= 0 then
        Cursor := FDocument.GroupAt(StyleGroup).CloseToken + 1;
      if not ConsumeWord(Cursor, 'at') then
      begin
        Unsupported(TokenSpan(CommandIndex),
          'Node must use an explicit at coordinate');
        Exit;
      end;
      if not TryReadCoordinate(Cursor, Limit, RelativeBase, NodePoint,
        UpdatesBase, NodeSpan, NXSpan, NYSpan, Relative, XScale, YScale,
        XUnit, YUnit) then
      begin
        Unsupported(TokenSpan(CommandIndex), 'Unsupported node coordinate');
        Exit;
      end;
      NodeStyle := StyleGroup;
    end;
    Cursor := NextSignificant(Cursor, Limit);
    if (Cursor < Limit) and (FOpenGroups[Cursor] >= 0) and
      (FDocument.GroupAt(FOpenGroups[Cursor]).Kind = tgBracket) then
    begin
      NodeStyle := FOpenGroups[Cursor];
      Cursor := FDocument.GroupAt(NodeStyle).CloseToken + 1;
    end;
    if IsAttached and (Obj <> nil) then
    begin
      ResumeObj := Obj;
      ParentStrokeRGB := Obj.StrokeRGB;
    end
    else
    begin
      ResumeObj := nil;
      ParentStrokeRGB := 0;
    end;
    Obj := TTikZSceneObject.Create;
    Obj.TextAnchor := 'center';
    ParseStyle(Obj, NodeStyle, 'node', True);
    Obj.StrokeEnabled := False;
    Obj.FillEnabled := False;
    HasTextColor := False;
    if NodeStyle >= 0 then
    begin
      Opts := ParseOptionGroup(NodeStyle);
      for OptIndex := 0 to High(Opts) do
      begin
        if Opts[OptIndex].Key = 'text' then
          HasTextColor := True;
        if not Opts[OptIndex].HasValue then
        begin
          BareColorName := TrimASCII(string(SourceText(
            Opts[OptIndex].OptionSpan)));
          if ParseRGB(BareColorName, ParsedColor) then
            HasTextColor := True;
        end;
      end;
    end;
    if IsAttached then
    begin
      if not HasTextColor then
        Obj.StrokeRGB := ParentStrokeRGB;
    end;
    Obj.Center := NodePoint;
    Obj.SourceFirst := NodePoint;
    Obj.SourceRotation := Obj.Rotation;
    if IsAttached then
    begin
      AddDependency(Obj, tdCoordinateX, tdepUnsupported, '', SpanAtZero,
        SpanAtZero, SpanAtZero, SpanAtZero, False, -1, -1, 0, '', False);
      AddDependency(Obj, tdCoordinateY, tdepUnsupported, '', SpanAtZero,
        SpanAtZero, SpanAtZero, SpanAtZero, False, -1, -1, 0, '', False);
    end
    else
    begin
      AddDependency(Obj, tdCoordinateX, tdepLocalLiteral, '', NXSpan,
        SpanAtZero, SpanAtZero, SpanAtZero, True, 0, 1, XScale, XUnit, Relative);
      AddDependency(Obj, tdCoordinateY, tdepLocalLiteral, '', NYSpan,
        SpanAtZero, SpanAtZero, SpanAtZero, True, 0, 1, YScale, YUnit, Relative);
    end;
    if not TryReadBraceGroup(Cursor, Limit, NodeBodyGroup, BodyBytes, BodySpan) then
    begin
      Obj.Free;
      Unsupported(TokenSpan(CommandIndex), 'Node is missing its text body');
      Exit;
    end;
    if IsAttached then
      NodeSourceSpan := SpanBetween(NodeStartToken, Cursor)
    else
      NodeSourceSpan := SpanBetween(NodeStartToken, Limit + 1);
    Obj.SourceSpan := NodeSourceSpan;
    Obj.TextBody := BodyBytes;
    Obj.TextPayloadSpan := BodySpan;
    Obj.TextContent := BodyBytes;
    Obj.TextHeightMM := FResult.Scene.TextSizeMM;
    Obj.TextBaselineMM := 1.2 * Obj.TextHeightMM;
    ReadTextFontMetrics(NodeBodyGroup);
    Obj.TextContent := SourceText(Obj.TextPayloadSpan);
    AddDependency(Obj, tdTextBody, tdepLocalLiteral, '', Obj.TextPayloadSpan,
      SpanAtZero, SpanAtZero, SpanAtZero, True, -1, -1, 1, '', False);
    FoundImage := False;
    FoundWidth := False;
    FoundHeight := False;
    Width := 0;
    Height := 0;
    WidthSpan := SpanAtZero;
    HeightSpan := SpanAtZero;
    WidthDep := '';
    HeightDep := '';
    IncludeStyle := -1;
    IncludeFileGroup := -1;
    FileBytes := '';
    BodyGroupInfo := FDocument.GroupAt(NodeBodyGroup);
    for ScanIndex := BodyGroupInfo.OpenToken + 1 to BodyGroupInfo.CloseToken - 1 do
    begin
      if TokenText(ScanIndex) = '\includegraphics' then
      begin
        FoundImage := True;
        N := NextSignificant(ScanIndex + 1, BodyGroupInfo.CloseToken);
        if (N < BodyGroupInfo.CloseToken) and (FOpenGroups[N] >= 0) and
          (FDocument.GroupAt(FOpenGroups[N]).Kind = tgBracket) then
        begin
          IncludeStyle := FOpenGroups[N];
          Opts := ParseOptionGroup(IncludeStyle);
          N := FDocument.GroupAt(IncludeStyle).CloseToken + 1;
          for OptIndex := 0 to High(Opts) do
          begin
            if Opts[OptIndex].Key = 'width' then
              FoundWidth := ParseLengthExpression(Opts[OptIndex].Value, Width, WidthDep)
            else if Opts[OptIndex].Key = 'height' then
              FoundHeight := ParseLengthExpression(Opts[OptIndex].Value, Height, HeightDep)
            else if Opts[OptIndex].Key = 'keepaspectratio' then
              Obj.ImageKeepAspectRatio := True
            else Unsupported(Opts[OptIndex].OptionSpan,
              'Unsupported includegraphics option: ' + Opts[OptIndex].Key);
          end;
          for OptIndex := 0 to High(Opts) do
          begin
            if Opts[OptIndex].Key = 'width' then WidthSpan := Opts[OptIndex].ValueSpan
            else if Opts[OptIndex].Key = 'height' then HeightSpan := Opts[OptIndex].ValueSpan;
          end;
        end;
        if not TryReadBraceGroup(N, BodyGroupInfo.CloseToken, IncludeFileGroup,
          FileBytes, BodySpan) then
          Unsupported(TokenSpan(ScanIndex), 'includegraphics is missing its file reference')
        else
        begin
          Obj.ImageReference := FileBytes;
          Obj.WidthMM := Width;
          Obj.HeightMM := Height;
          if FoundWidth and FoundHeight then
          begin
            Obj.Kind := tsoImage;
            AddDependency(Obj, tdWidth, tdepLocalLiteral, '', WidthSpan,
              SpanAtZero, SpanAtZero, SpanAtZero, True, -1, -1, 1, 'mm', False);
            AddDependency(Obj, tdHeight, tdepLocalLiteral, '', HeightSpan,
              SpanAtZero, SpanAtZero, SpanAtZero, True, -1, -1, 1, 'mm', False);
          end
          else Unsupported(TokenSpan(ScanIndex),
            'Images require explicit width and height for editable import');
        end;
        Break;
      end;
    end;
    if not FoundImage then Obj.Kind := tsoText;
    StatementSpan := NodeSourceSpan;
    AddSemanticObject(Obj, StatementSpan);
    if IsAttached then Obj := ResumeObj else Obj := nil;
    I := Cursor;
  end;

  procedure ParseRotationOptions(var Cursor: SizeInt);
  var
    ModifierGroup, ModifierIndex: SizeInt;
    ModifierOptions: TTikZOptionValues;
    Modifier: TTikZOptionValue;
  begin
    Cursor := NextSignificant(Cursor, Limit);
    if (Cursor >= Limit) or (FOpenGroups[Cursor] < 0) or
      (FDocument.GroupAt(FOpenGroups[Cursor]).Kind <> tgBracket) then Exit;
    ModifierGroup := FOpenGroups[Cursor];
    RotationOptionsSpan := FDocument.GroupAt(ModifierGroup).Span;
    ModifierOptions := ParseOptionGroup(ModifierGroup);
    ModifierIndex := FindOption(ModifierOptions, 'rotate around');
    if ModifierIndex < 0 then
    begin
      Unsupported(RotationOptionsSpan, 'Unsupported path modifier');
      Cursor := FDocument.GroupAt(ModifierGroup).CloseToken + 1;
      Exit;
    end;
    Modifier := ModifierOptions[ModifierIndex];
    if not ParseRotateAround(Modifier.Value, RotationDegrees, RotationCenter) then
      Unsupported(Modifier.OptionSpan, 'Unsupported rotate around expression')
    else
    begin
      Rotated := True;
      Obj.Rotation := RotationDegrees * Pi / 180;
      Obj.RotationCenter := RotationCenter;
      RotationSpan := Modifier.ValueSpan;
      RotationKeySpan := Modifier.KeySpan;
      AddDependency(Obj, tdRotation, tdepLocalLiteral, '', Modifier.ValueSpan,
        Modifier.KeySpan, RotationOptionsSpan, SpanAtZero, True, -1, -1,
        Pi / 180, 'deg', False);
    end;
    Cursor := FDocument.GroupAt(ModifierGroup).CloseToken + 1;
  end;

  function TryReadArcArguments(var Cursor: SizeInt;
    out StartDegrees, EndDegrees, RadiusMM, RadiusScale: Double;
    out Dependency, UnitName: string; out StartSpan, EndSpan,
    RadiusSpan: TTikZSourceSpan): Boolean;
  var
    ArcGroup, CloseToken, Colon1, Colon2, J, FirstToken, LastToken: SizeInt;
    GroupInfo: TTikZGroup;
    Spec, RadiusText, UnitText: string;
    ContentOffset, BaseByte: SizeInt;
    T: TTikZToken;
    procedure SetPartSpan(PartStart, PartEnd: SizeInt;
      out PartSpan: TTikZSourceSpan);
    var
      K: SizeInt;
      Found: Boolean;
    begin
      FillChar(PartSpan, SizeOf(PartSpan), 0);
      PartSpan.StartByte := BaseByte + PartStart;
      PartSpan.EndByte := BaseByte + PartEnd + 1;
      FirstToken := -1;
      LastToken := -1;
      for K := GroupInfo.OpenToken + 1 to GroupInfo.CloseToken - 1 do
      begin
        T := FDocument.TokenAt(K);
        if (T.Span.EndByte <= PartSpan.StartByte) or
          (T.Span.StartByte >= PartSpan.EndByte) then Continue;
        if FirstToken < 0 then FirstToken := K;
        LastToken := K;
      end;
      Found := (FirstToken >= 0) and (LastToken >= FirstToken);
      if Found then
      begin
        T := FDocument.TokenAt(FirstToken);
        PartSpan.StartLine := T.Span.StartLine;
        PartSpan.StartColumn := T.Span.StartColumn;
        T := FDocument.TokenAt(LastToken);
        PartSpan.EndLine := T.Span.EndLine;
        PartSpan.EndColumn := T.Span.EndColumn;
      end;
    end;
  begin
    Result := False;
    StartDegrees := 0;
    EndDegrees := 0;
    RadiusMM := 0;
    RadiusScale := 1;
    Dependency := '';
    UnitName := '';
    Cursor := NextSignificant(Cursor, Limit);
    if (Cursor >= Limit) or (FOpenGroups[Cursor] < 0) or
      (FDocument.GroupAt(FOpenGroups[Cursor]).Kind <> tgParen) then Exit;
    ArcGroup := FOpenGroups[Cursor];
    GroupInfo := FDocument.GroupAt(ArcGroup);
    CloseToken := GroupInfo.CloseToken;
    Spec := string(SourceText(GroupInfo.Span));
    if (Length(Spec) < 2) or (Spec[1] <> '(') or
      (Spec[Length(Spec)] <> ')') then Exit;
    Spec := Copy(Spec, 2, Length(Spec) - 2);
    ContentOffset := 0;
    while (Length(Spec) > 0) and (Ord(Spec[1]) <= 32) do
    begin
      Delete(Spec, 1, 1);
      Inc(ContentOffset);
    end;
    Spec := TrimASCII(Spec);
    Colon1 := Pos(':', Spec);
    if Colon1 <= 1 then Exit;
    Colon2 := Pos(':', Copy(Spec, Colon1 + 1, MaxInt));
    if Colon2 <= 1 then Exit;
    Inc(Colon2, Colon1);
    if (Pos(':', Copy(Spec, Colon2 + 1, MaxInt)) > 0) then Exit;
    if not ParseInvariantFloat(TrimASCII(Copy(Spec, 1, Colon1 - 1)),
      StartDegrees) or
      not ParseInvariantFloat(TrimASCII(Copy(Spec, Colon1 + 1,
        Colon2 - Colon1 - 1)), EndDegrees) then Exit;
    RadiusText := TrimASCII(Copy(Spec, Colon2 + 1, MaxInt));
    if not ParseLengthExpression(RadiusText, RadiusMM, Dependency) or
      (RadiusMM <= 0) then Exit;
    if Dependency = '' then
    begin
      UnitText := '';
      for J := 1 to Length(RadiusText) do
        if ((RadiusText[J] >= 'a') and (RadiusText[J] <= 'z')) or
          ((RadiusText[J] >= 'A') and (RadiusText[J] <= 'Z')) then
        begin
          UnitText := Copy(RadiusText, J, MaxInt);
          Break;
        end;
      if (UnitText = '') or not UnitToMillimeters(UnitText, RadiusScale) then
        Exit;
      UnitName := UnitText;
    end
    else UnitName := 'mm';
    BaseByte := GroupInfo.Span.StartByte + ContentOffset;
    SetPartSpan(1, Colon1 - 1, StartSpan);
    SetPartSpan(Colon1 + 1, Colon2 - 1, EndSpan);
    SetPartSpan(Colon2 + 1, Length(Spec), RadiusSpan);
    Cursor := CloseToken + 1;
    Result := True;
  end;

begin
  StyleGroup := -1;
  I := NextSignificant(CommandIndex + 1, Limit);
  if (I < Limit) and (FOpenGroups[I] >= 0) and
    (FDocument.GroupAt(FOpenGroups[I]).Kind = tgBracket) then
  begin
    StyleGroup := FOpenGroups[I];
    I := FDocument.GroupAt(StyleGroup).CloseToken + 1;
  end;
  StatementSpan := SpanBetween(CommandIndex, Limit + 1);
  RelativeBase.X := 0;
  RelativeBase.Y := 0;
  CurrentPoint.X := 0;
  CurrentPoint.Y := 0;
  if Command = 'node' then
  begin
    ParseNode(False, CommandIndex);
    Exit;
  end;
  Obj := MakeStyledObject;
  Commands := nil;
  HavePoint := False;
  PathHasSegment := False;
  ClosedPath := False;
  Rotated := False;
  RotationSpan := SpanAtZero;
  RotationKeySpan := SpanAtZero;
  RotationOptionsSpan := SpanAtZero;
  while I < Limit do
  begin
    I := NextSignificant(I, Limit);
    if I >= Limit then Break;
    if not Tick(TokenSpan(I)) then Break;
    if (FDocument.TokenAt(I).Kind = ttkOpenBracket) then
    begin
      ParseRotationOptions(I);
      Continue;
    end;
    if ConsumeWord(I, 'rectangle') then
    begin
      if not HavePoint then
      begin
        Unsupported(TokenSpan(I - 1), 'Rectangle has no origin coordinate');
        Break;
      end;
      if not ReadPoint(I, RelativeBase, 1, Point, PointSpan, XSpan, YSpan,
        IsRelative, UpdateBase) then
      begin
        Unsupported(TokenSpan(I), 'Unsupported rectangle size coordinate');
        Break;
      end;
      Obj.Kind := tsoRectangle;
      Obj.Origin := CurrentPoint;
      Obj.SourceFirst := CurrentPoint;
      Obj.SourceSecond := Point;
      Obj.Center.X := (CurrentPoint.X + Point.X) / 2;
      Obj.Center.Y := (CurrentPoint.Y + Point.Y) / 2;
      Obj.WidthMM := Point.X - CurrentPoint.X;
      Obj.HeightMM := Point.Y - CurrentPoint.Y;
      if Obj.WidthMM < 0 then
      begin
        Obj.Origin.X := Point.X;
        Obj.WidthMM := -Obj.WidthMM;
      end;
      if Obj.HeightMM < 0 then
      begin
        Obj.Origin.Y := Point.Y;
        Obj.HeightMM := -Obj.HeightMM;
      end;
      if Rotated then
      begin
        Obj.Rotation := RotationDegrees * Pi / 180;
        Obj.RotationCenter := RotationCenter;
      end;
      Obj.SourceRotation := Obj.Rotation;
      Obj.SourceRotationCenter := Obj.RotationCenter;
      AddDependency(Obj, tdWidth, tdepLocalLiteral, '', XSpan, SpanAtZero,
        SpanAtZero, SpanAtZero, True, 0, 1, XScale, XUnit, IsRelative);
      AddDependency(Obj, tdHeight, tdepLocalLiteral, '', YSpan, SpanAtZero,
        SpanAtZero, SpanAtZero, True, 0, 1, YScale, YUnit, IsRelative);
      if Rotated then Obj.RotationCenter := RotationCenter;
      if not Obj.StrokeEnabled and not Obj.FillEnabled and
        (Obj.LineWidthMM = 0) then
      begin
        FResult.Scene.HasOwnedBounds := True;
        FResult.Scene.OwnedBoundsSpan := StatementSpan;
        FResult.Scene.OwnedBoundsOrigin := Obj.Origin;
        FResult.Scene.OwnedBoundsWidthMM := Obj.WidthMM;
        FResult.Scene.OwnedBoundsHeightMM := Obj.HeightMM;
        Obj.Free;
      end
      else AddSemanticObject(Obj, StatementSpan);
      Obj := nil;
      Break;
    end;
    if ConsumeWord(I, 'circle') then
    begin
      GroupIndex := -1;
      I := NextSignificant(I, Limit);
      if (I >= Limit) or (FOpenGroups[I] < 0) or
        (FDocument.GroupAt(FOpenGroups[I]).Kind <> tgParen) then
      begin
        Unsupported(TokenSpan(I), 'Malformed circle radius');
        Break;
      end;
      GroupIndex := FOpenGroups[I];
      Semi := FDocument.GroupAt(GroupIndex).CloseToken;
      Inc(I);
      if not TryReadMeasure(I, Value, Dependency, PointSpan) or
        (NextSignificant(I, Semi) <> Semi) then
      begin
        Unsupported(TokenSpan(Semi), 'Unsupported circle radius');
        Break;
      end;
      if Dependency = '' then
      begin
        Value := Value * FResult.Scene.PictureScaleX;
        XScale := FResult.Scene.PictureScaleX;
        XUnit := '';
      end
      else
      begin
        UnitToMillimeters(Dependency, XScale);
        Value := Value * XScale;
      end;
      I := Semi + 1;
      Obj.Kind := tsoCircle;
      Obj.Center := CurrentPoint;
      Obj.SourceFirst := CurrentPoint;
      Obj.RadiusX := Value;
      Obj.RadiusY := Value;
      Obj.SourceSpan := StatementSpan;
      AddDependency(Obj, tdRadius, tdepLocalLiteral, '', PointSpan,
        SpanAtZero, SpanAtZero, SpanAtZero, True, -1, -1, XScale, XUnit, False);
      AddSemanticObject(Obj, StatementSpan);
      Obj := nil;
      Break;
    end;
    if ConsumeWord(I, 'ellipse') then
    begin
      GroupIndex := -1;
      I := NextSignificant(I, Limit);
      if (I >= Limit) or (FOpenGroups[I] < 0) or
        (FDocument.GroupAt(FOpenGroups[I]).Kind <> tgParen) then
      begin
        Unsupported(TokenSpan(I), 'Malformed ellipse radii');
        Break;
      end;
      GroupIndex := FOpenGroups[I];
      Semi := FDocument.GroupAt(GroupIndex).CloseToken;
      Inc(I);
      if not TryReadMeasure(I, Obj.RadiusX, XUnit, PointSpan) or
        not ConsumeWord(I, 'and') or
        not TryReadMeasure(I, Obj.RadiusY, YUnit, YSpan) or
        (NextSignificant(I, Semi) <> Semi) then
      begin
        Unsupported(TokenSpan(Semi), 'Unsupported ellipse radii');
        Break;
      end;
      if (XUnit = '') or (YUnit = '') or not UnitToMillimeters(XUnit, XScale)
        or not UnitToMillimeters(YUnit, YScale) then
        Unsupported(TokenSpan(Semi), 'Ellipse radii require supported units')
      else
      begin
        Obj.RadiusX := Obj.RadiusX * XScale;
        Obj.RadiusY := Obj.RadiusY * YScale;
        Obj.Kind := tsoEllipse;
        Obj.Center := CurrentPoint;
        Obj.SourceFirst := CurrentPoint;
        if Rotated then
        begin
          Obj.Rotation := RotationDegrees * Pi / 180;
          Obj.RotationCenter := RotationCenter;
        end;
        Obj.SourceRotation := Obj.Rotation;
        Obj.SourceRotationCenter := Obj.RotationCenter;
        AddDependency(Obj, tdRadius, tdepLocalLiteral, '', PointSpan,
          SpanAtZero, SpanAtZero, SpanAtZero, True, -1, 1, XScale, XUnit, False);
        AddDependency(Obj, tdRadius, tdepLocalLiteral, '', YSpan,
          SpanAtZero, SpanAtZero, SpanAtZero, True, -1, 2, YScale, YUnit, False);
        AddSemanticObject(Obj, StatementSpan);
        Obj := nil;
      end;
      I := Semi + 1;
      Break;
    end;
    if ConsumeWord(I, 'arc') then
    begin
      if not TryReadArcArguments(I, ArcStartDegrees, ArcEndDegrees,
        ArcRadiusMM, ArcRadiusScale, ArcDependency, ArcUnit,
        ArcStartSpan, ArcEndSpan, ArcRadiusSpan) then
      begin
        Unsupported(TokenSpan(I), 'Unsupported arc angles or radius');
        Break;
      end;
      ArcCycle := False;
      I := NextSignificant(I, Limit);
      if ConsumeLiteral(I, '--') then
      begin
        if not ConsumeWord(I, 'cycle') then
        begin
          Unsupported(TokenSpan(I), 'Only a closing cycle may follow an arc');
          Break;
        end;
        ArcCycle := True;
      end;
      if NextSignificant(I, Limit) <> Limit then
      begin
        Unsupported(TokenSpan(I), 'Unsupported path after arc');
        Break;
      end;
      if (Abs(FResult.Scene.PictureScaleX -
        FResult.Scene.PictureScaleY) > 1E-10) then
      begin
        Unsupported(StatementSpan,
          'Circular arcs under anisotropic picture scaling are not native circles');
        Break;
      end;
      ArcCenter.X := 0;
      ArcCenter.Y := 0;
      ArcIsSector := (Length(Commands) = 2) and
        (Commands[0].Kind = tpcMove) and
        (Commands[1].Kind = tpcLine);
      if ArcIsSector then
      begin
        if not ArcCycle then
        begin
          Unsupported(StatementSpan,
            'An arc beginning at the circle center must close as a sector');
          Break;
        end;
        ArcCenter := Commands[0].P1;
        ArcStartPoint := Commands[1].P1;
        ArcDistance := Sqrt(Sqr(ArcStartPoint.X - ArcCenter.X) +
          Sqr(ArcStartPoint.Y - ArcCenter.Y));
        if (Abs(ArcDistance - ArcRadiusMM) > 0.03) and
          (Abs(ArcDistance - ArcRadiusMM) > ArcRadiusMM * 0.01) then
        begin
          Unsupported(ArcRadiusSpan,
            'Sector start point does not match its arc radius');
          Break;
        end;
      end
      else if (Length(Commands) = 1) and
        (Commands[0].Kind = tpcMove) then
      begin
        ArcStartPoint := Commands[0].P1;
        ArcCenter.X := ArcStartPoint.X - ArcRadiusMM *
          Cos(ArcStartDegrees * Pi / 180);
        ArcCenter.Y := ArcStartPoint.Y - ArcRadiusMM *
          Sin(ArcStartDegrees * Pi / 180);
      end
      else
      begin
        Unsupported(StatementSpan,
          'Arc must start at a point or at a center followed by its start point');
        Break;
      end;
      if ArcIsSector then Obj.Kind := tsoSector
      else if ArcCycle then Obj.Kind := tsoSegment
      else Obj.Kind := tsoArc;
      if Rotated then
      begin
        C := Cos(RotationDegrees * Pi / 180);
        S := Sin(RotationDegrees * Pi / 180);
        DX := ArcCenter.X - RotationCenter.X;
        DY := ArcCenter.Y - RotationCenter.Y;
        ArcCenter.X := RotationCenter.X + C * DX - S * DY;
        ArcCenter.Y := RotationCenter.Y + S * DX + C * DY;
        ArcStartDegrees := ArcStartDegrees + RotationDegrees;
        ArcEndDegrees := ArcEndDegrees + RotationDegrees;
      end;
      Obj.Center := ArcCenter;
      Obj.RadiusX := ArcRadiusMM;
      Obj.RadiusY := ArcRadiusMM;
      Obj.StartAngle := ArcStartDegrees * Pi / 180;
      Obj.EndAngle := ArcEndDegrees * Pi / 180;
      if ArcDependency = '' then DependencySource := tdepLocalLiteral
      else DependencySource := tdepSharedDefaultMacro;
      AddDependency(Obj, tdRadius, DependencySource, ArcDependency,
        ArcRadiusSpan, SpanAtZero, SpanAtZero, SpanAtZero, True,
        -1, -1, ArcRadiusScale, ArcUnit, False);
      AddDependency(Obj, tdStartAngle, tdepLocalLiteral, '', ArcStartSpan,
        SpanAtZero, SpanAtZero, SpanAtZero, True, -1, -1, Pi / 180,
        'deg', False);
      AddDependency(Obj, tdEndAngle, tdepLocalLiteral, '', ArcEndSpan,
        SpanAtZero, SpanAtZero, SpanAtZero, True, -1, -1, Pi / 180,
        'deg', False);
      AddDependency(Obj, tdCoordinateX, tdepUnsupported, '',
        Commands[0].P1XSpan, SpanAtZero, SpanAtZero, SpanAtZero, False,
        0, 1, 0, '', False);
      AddDependency(Obj, tdCoordinateY, tdepUnsupported, '',
        Commands[0].P1YSpan, SpanAtZero, SpanAtZero, SpanAtZero, False,
        0, 1, 0, '', False);
      AddSemanticObject(Obj, StatementSpan);
      Obj := nil;
      Break;
    end;
    if ConsumeWord(I, 'cycle') then
    begin
      if not HavePoint then
        Unsupported(TokenSpan(I - 1), 'Cycle has no path to close')
      else
      begin
        FillChar(PathCommand, SizeOf(PathCommand), 0);
        PathCommand.Kind := tpcClose;
        AppendPathCommand(PathCommand);
        ClosedPath := True;
        PathHasSegment := True;
      end;
      Continue;
    end;
    if ConsumeWord(I, 'node') then
    begin
      if PathHasSegment then
        FlushPath(True);
      ParseNode(True, I - 1);
      Continue;
    end;
    if ConsumeLiteral(I, '--') then
    begin
      if not HavePoint then
      begin
        Unsupported(TokenSpan(I - 2), 'Line segment has no starting coordinate');
        Break;
      end;
      if ConsumeWord(I, 'cycle') then
      begin
        FillChar(PathCommand, SizeOf(PathCommand), 0);
        PathCommand.Kind := tpcClose;
        AppendPathCommand(PathCommand);
        ClosedPath := True;
        PathHasSegment := True;
        Continue;
      end;
      if not ReadPoint(I, RelativeBase, 1, Point, PointSpan, XSpan, YSpan,
        IsRelative, UpdateBase) then
      begin
        Unsupported(TokenSpan(I), 'Unsupported line endpoint coordinate');
        Break;
      end;
      FillChar(PathCommand, SizeOf(PathCommand), 0);
      PathCommand.Kind := tpcLine;
      PathCommand.P1 := Point;
      PathCommand.P1Span := PointSpan;
      PathCommand.P1XSpan := XSpan;
      PathCommand.P1YSpan := YSpan;
      AppendPathCommand(PathCommand);
      CurrentPoint := Point;
      if UpdateBase then RelativeBase := Point;
      PathHasSegment := True;
      ClosedPath := False;
      Continue;
    end;
    if ConsumeLiteral(I, '..') then
    begin
      if not HavePoint or not ConsumeWord(I, 'controls') then
      begin
        Unsupported(TokenSpan(I - 1), 'Unsupported Bezier path operator');
        Break;
      end;
      FillChar(PathCommand, SizeOf(PathCommand), 0);
      if not ReadPoint(I, RelativeBase, 1, Point, PointSpan, XSpan, YSpan,
        IsRelative, UpdateBase) then
      begin
        Unsupported(TokenSpan(I), 'Unsupported first Bezier control point');
        Break;
      end;
      PathCommand.P1 := Point;
      PathCommand.P1Span := PointSpan;
      PathCommand.P1XSpan := XSpan;
      PathCommand.P1YSpan := YSpan;
      if UpdateBase then RelativeBase := Point;
      if not ConsumeWord(I, 'and') or
        not ReadPoint(I, RelativeBase, 2, Point, PointSpan, XSpan, YSpan,
          IsRelative, UpdateBase) then
      begin
        Unsupported(TokenSpan(I), 'Unsupported second Bezier control point');
        Break;
      end;
      PathCommand.P2 := Point;
      PathCommand.P2Span := PointSpan;
      PathCommand.P2XSpan := XSpan;
      PathCommand.P2YSpan := YSpan;
      if UpdateBase then RelativeBase := Point;
      if not ConsumeLiteral(I, '..') or
        not ReadPoint(I, RelativeBase, 3, Point, PointSpan, XSpan, YSpan,
          IsRelative, UpdateBase) then
      begin
        Unsupported(TokenSpan(I), 'Unsupported Bezier endpoint');
        Break;
      end;
      PathCommand.Kind := tpcCubic;
      PathCommand.P3 := Point;
      PathCommand.P3Span := PointSpan;
      PathCommand.P3XSpan := XSpan;
      PathCommand.P3YSpan := YSpan;
      AppendPathCommand(PathCommand);
      CurrentPoint := Point;
      if UpdateBase then RelativeBase := Point;
      PathHasSegment := True;
      Continue;
    end;
    if (FOpenGroups[I] >= 0) and
      (FDocument.GroupAt(FOpenGroups[I]).Kind = tgParen) then
    begin
      if PathHasSegment then FlushPath(True);
      if (Obj = nil) then Obj := MakeStyledObject;
      if not ReadPoint(I, RelativeBase, 1, Point, PointSpan, XSpan, YSpan,
        IsRelative, UpdateBase) then
      begin
        Unsupported(TokenSpan(I), 'Unsupported path coordinate');
        Break;
      end;
      CurrentPoint := Point;
      if UpdateBase then RelativeBase := Point;
      FillChar(PathCommand, SizeOf(PathCommand), 0);
      PathCommand.Kind := tpcMove;
      PathCommand.P1 := Point;
      PathCommand.P1Span := PointSpan;
      PathCommand.P1XSpan := XSpan;
      PathCommand.P1YSpan := YSpan;
      AppendPathCommand(PathCommand);
      HavePoint := True;
      Continue;
    end;
    Unsupported(TokenSpan(I), 'Unsupported TikZ path construct');
    Break;
  end;
  if (Obj <> nil) and (Length(Commands) > 0) and PathHasSegment then
    FlushPath(False)
  else if Obj <> nil then Obj.Free;
end;

function EvaluateTikZ(const Syntax: TTikZSyntaxResult;
  const Options: TTikZImportOptions): TTikZSemanticResult;
var
  Evaluator: TTikZEvaluator;
begin
  Evaluator := TTikZEvaluator.Create(Syntax, Options);
  try
    Evaluator.Run;
    Result := Evaluator.ImportResult;
    Evaluator.FResult := nil;
  finally
    Evaluator.Free;
  end;
end;

end.

unit TikZImportTests;

{$mode delphi}{$H+}

interface

implementation

uses Classes, SysUtils, Math, TikZLexer, TikZSyntax, TikZImport, CoreTestSupport;

function BytesOf(const Value: RawByteString): TBytes;
var
  I: SizeInt;
begin
  Result := nil;
  SetLength(Result, Length(Value));
  for I := 1 to Length(Value) do Result[I - 1] := Ord(Value[I]);
end;

function RawStringOf(const Value: TBytes): RawByteString;
var
  I: SizeInt;
begin
  SetLength(Result, Length(Value));
  for I := 0 to High(Value) do Result[I + 1] := AnsiChar(Value[I]);
end;

function ParseAndEvaluate(const Value: RawByteString;
  out Syntax: TTikZSyntaxResult): TTikZSemanticResult;
begin
  Syntax := ParseTikZ(BytesOf(Value),
    DefaultTikZParseOptions(tikTexInput));
  Result := EvaluateTikZ(Syntax, DefaultTikZImportOptions);
end;

function FindDependency(const Obj: TTikZSceneObject;
  Kind: TTikZDependencyKind; out Dependency: TTikZPropertyDependency): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to High(Obj.Dependencies) do
    if Obj.Dependencies[I].Kind = Kind then
    begin
      Dependency := Obj.Dependencies[I];
      Exit(True);
    end;
  Result := False;
end;

procedure TestPGFScopeTransformOrder;
const
  Source = '\begin{tikzpicture}[x=1mm,y=1mm]' +
    '\path[line width=0mm] (-1,-1) rectangle +(4,4);' +
    '\begin{scope}[shift={(1mm,-2)},rotate=17.5]' +
    '\draw (0,0)--(2,0);\end{scope}\end{tikzpicture}';
var
  Syntax: TTikZSyntaxResult;
  Evaluated: TTikZSemanticResult;
  Obj: TTikZSceneObject;
begin
  Evaluated := ParseAndEvaluate(Source, Syntax);
  try
    CheckCore(Syntax.Outcome = tpoAccepted,
      'literal nested-scope source should pass syntax validation');
    CheckCore(Evaluated.Outcome = tsoAccepted,
      'literal shift and rotation should remain editable geometry');
    CheckCore(Evaluated.Scene.ObjectCount = 1,
      'scope should materialize one line object');
    CheckCore(Evaluated.Scene.HasOwnedBounds,
      'nonpainting zero-width bounds rectangle should be retained as bounds');
    CheckNearCore(Evaluated.Scene.OwnedBoundsWidthMM, 4, 0.00001,
      'owned bounds width (mm)');
    Obj := Evaluated.Scene.ObjectAt(0);
    CheckCore((Obj.Kind = tsoPath) and (Length(Obj.Commands) = 2),
      'scoped line endpoints were not retained');
    CheckNearCore(Obj.Commands[0].P1.X, 1, 0.00001,
      'shifted line start x (mm)');
    CheckNearCore(Obj.Commands[0].P1.Y, -2, 0.00001,
      'shifted line start y (mm)');
    CheckNearCore(Obj.Commands[1].P1.X, 2.907433901, 0.00001,
      'shift-then-rotate endpoint x (mm)');
    CheckNearCore(Obj.Commands[1].P1.Y, -1.398588401, 0.00001,
      'shift-then-rotate endpoint y (mm)');
  finally
    Evaluated.Free;
    Syntax.Free;
  end;
end;

procedure TestAttachedDevTikZLabelAndFont;
const
  Source = '\providecommand{\tpxTextSize}{10pt}' +
    '\definecolor{T}{rgb}{0.2,0.4,0.8}' +
    '\begin{tikzpicture}[x=1mm,y=1mm]' +
    '\draw[T] (2,3) node[anchor=base west,rotate=30]' +
    '{\pgfmathsetlengthmacro{\tpxObjectFontSize}{1.25*\tpxTextSize}' +
    '\pgfmathsetlengthmacro{\tpxObjectBaseline}{1.5*\tpxTextSize}' +
    '\fontsize{\tpxObjectFontSize}{\tpxObjectBaseline}\selectfont X};' +
    '\end{tikzpicture}';
var
  Syntax: TTikZSyntaxResult;
  Evaluated: TTikZSemanticResult;
  Obj: TTikZSceneObject;
  Dependency: TTikZPropertyDependency;
begin
  Evaluated := ParseAndEvaluate(Source, Syntax);
  try
    CheckCore((Syntax.Outcome = tpoAccepted) and
      (Evaluated.Outcome = tsoAccepted),
      'emitted attached-node syntax and literal font wrapper should be accepted');
    CheckCore(Evaluated.Scene.ObjectCount = 1,
      'move-only path scaffolding must not become an extra native object');
    Obj := Evaluated.Scene.ObjectAt(0);
    CheckCore(Obj.Kind = tsoText, 'attached node did not become editable text');
    CheckCore((Obj.TextAnchor = 'base west') and not Obj.StrokeEnabled,
      'attached label anchor or native text-frame profile changed');
    CheckCore(Obj.StrokeRGB = $3366CC,
      'attached label did not inherit the source color');
    CheckNearCore(Obj.Center.X, 2, 0.00001, 'attached label x (mm)');
    CheckNearCore(Obj.Center.Y, 3, 0.00001, 'attached label y (mm)');
    CheckNearCore(Obj.TextHeightMM, 1.25 * 10 * 25.4 / 72.27, 0.00001,
      'exported TeX font size (mm)');
    CheckCore(Pos('\pgfmathsetlengthmacro', string(Obj.TextBody)) = 1,
      'original TeX text body was not retained');
    CheckCore((Obj.TextContent = 'X') and
      (RawStringOf(Syntax.SourceSlice(Obj.TextPayloadSpan)) = 'X'),
      'recognized font wrapper must leave only the text payload in the model');
    CheckCore(FindDependency(Obj, tdTextBody, Dependency) and
      (RawStringOf(Syntax.SourceSlice(Dependency.ValueSpan)) = 'X'),
      'text body binding must cover the payload without the font wrapper');
    CheckCore(FindDependency(Obj, tdTextSize, Dependency),
      'font-size expression is missing its source binding');
    CheckCore((Dependency.DependencySource = tdepSharedDefaultMacro) and
      (Dependency.DependencyName = '\tpxTextSize') and
      Dependency.HasLocalOverride,
      'font-size binding did not retain its safe local override metadata');
    CheckCore(FindDependency(Obj, tdTextBaseline, Dependency),
      'baseline expression is missing its source binding');
    CheckCore((Dependency.DependencySource = tdepSharedDefaultMacro) and
      (Dependency.DependencyName = '\tpxTextSize') and
      Dependency.HasLocalOverride and
      (Dependency.ValueSpan.EndByte > Dependency.ValueSpan.StartByte),
      'baseline binding did not retain its local expression span');
    CheckNearCore(Obj.TextBaselineMM, 1.5 * 10 * 25.4 / 72.27, 0.00001,
      'exported TeX baseline size (mm)');
  finally
    Evaluated.Free;
    Syntax.Free;
  end;
end;

procedure TestNodeTextColorAndFrameProfile;
const
  Source = '\definecolor{brand}{rgb}{0.8,0.1,0.1}' +
    '\begin{tikzpicture}[x=1mm,y=1mm]' +
    '\draw[blue] (0,0)--(1,0) node[anchor=base,text=red]{attached};' +
    '\node[text=red,anchor=base] at (2,0) {standalone};' +
    '\node[text=brand] at (3,0) {named};' +
    '\end{tikzpicture}';
  UnsupportedSource = '\begin{tikzpicture}' +
    '\node[draw=red] at (0,0) {framed};' +
    '\node[fill=blue] at (1,0) {filled};' +
    '\end{tikzpicture}';
var
  Syntax: TTikZSyntaxResult;
  Evaluated: TTikZSemanticResult;
  Obj: TTikZSceneObject;
  Dependency: TTikZPropertyDependency;
  I: SizeInt;
  FoundDraw, FoundFill: Boolean;
begin
  Evaluated := ParseAndEvaluate(Source, Syntax);
  try
    CheckCore((Syntax.Outcome = tpoAccepted) and
      (Evaluated.Outcome = tsoAccepted),
      'local node text colors should remain in the editable profile');
    CheckCore(Evaluated.Scene.ObjectCount = 4,
      'colored path and its text nodes changed visible-object order');
    Obj := Evaluated.Scene.ObjectAt(1);
    CheckCore((Obj.Kind = tsoText) and not Obj.StrokeEnabled and
      (Obj.StrokeRGB = $FF0000),
      'attached text color must override path stroke without adding a frame');
    CheckCore(FindDependency(Obj, tdTextColor, Dependency) and
      (RawStringOf(Syntax.SourceSlice(Dependency.ValueSpan)) = 'red') and
      (RawStringOf(Syntax.SourceSlice(Dependency.PropertyKeySpan)) = 'text'),
      'attached text color must retain its local source binding');
    CheckCore(not FindDependency(Obj, tdStrokeColor, Dependency),
      'text-color binding must not point at the parent path stroke');
    Obj := Evaluated.Scene.ObjectAt(2);
    CheckCore((Obj.Kind = tsoText) and not Obj.StrokeEnabled and
      (Obj.StrokeRGB = $FF0000) and
      FindDependency(Obj, tdTextColor, Dependency),
      'standalone text color should set glyph RGB without a node frame');
    Obj := Evaluated.Scene.ObjectAt(3);
    CheckCore((Obj.Kind = tsoText) and
      FindDependency(Obj, tdTextColor, Dependency) and
      (Dependency.DependencySource = tdepNamedColor) and
      Dependency.HasLocalOverride and
      (RawStringOf(Syntax.SourceSlice(Dependency.PropertyKeySpan)) = 'text'),
      'named node color must retain a local text override site');
  finally
    Evaluated.Free;
    Syntax.Free;
  end;

  Evaluated := ParseAndEvaluate(UnsupportedSource, Syntax);
  try
    CheckCore((Syntax.Outcome = tpoAccepted) and
      (Evaluated.Outcome = tsoUnsupported),
      'native text import must reject unrepresented node frames and fills');
    FoundDraw := False;
    FoundFill := False;
    for I := 0 to High(Evaluated.Diagnostics) do
    begin
      if Pos('frames and fills', Evaluated.Diagnostics[I].Message) = 0 then
        Continue;
      if RawStringOf(Syntax.SourceSlice(Evaluated.Diagnostics[I].Span)) =
        'draw=red' then FoundDraw := True;
      if RawStringOf(Syntax.SourceSlice(Evaluated.Diagnostics[I].Span)) =
        'fill=blue' then FoundFill := True;
    end;
    CheckCore(FoundDraw and FoundFill,
      'unsupported node frame/fill diagnostics need exact source spans');
  finally
    Evaluated.Free;
    Syntax.Free;
  end;
end;

procedure TestArcSectorAndCircleMaterializationProfile;
const
  Source = '\begin{tikzpicture}[x=1mm,y=1mm]' +
    '\draw (4,0) arc (0:90:4mm);' +
    '\filldraw (0,0)--(4,0) arc (0:90:4mm)--cycle;' +
    '\draw (1,2) circle (3mm);' +
    '\draw (0,0) circle (0.1in);\end{tikzpicture}';
var
  Syntax: TTikZSyntaxResult;
  Evaluated: TTikZSemanticResult;
  Obj: TTikZSceneObject;
begin
  Evaluated := ParseAndEvaluate(Source, Syntax);
  try
    CheckCore((Syntax.Outcome = tpoAccepted) and
      (Evaluated.Outcome = tsoAccepted),
      'native arc, sector and circle forms should be semantically accepted');
    CheckCore(Evaluated.Scene.ObjectCount = 4,
      'arc, sector and circles must remain four ordered native objects');
    Obj := Evaluated.Scene.ObjectAt(0);
    CheckCore(Obj.Kind = tsoArc, 'open arc was changed to another shape');
    CheckNearCore(Obj.Center.X, 0, 0.00001, 'arc center x (mm)');
    CheckNearCore(Obj.Center.Y, 0, 0.00001, 'arc center y (mm)');
    CheckNearCore(Obj.RadiusX, 4, 0.00001, 'arc radius (mm)');
    CheckNearCore(Obj.EndAngle, Pi / 2, 0.00001, 'arc end angle (radians)');
    Obj := Evaluated.Scene.ObjectAt(1);
    CheckCore(Obj.Kind = tsoSector,
      '`-- cycle` arc closure should remain a filled sector');
    CheckNearCore(Obj.RadiusX, 4, 0.00001, 'sector radius (mm)');
    Obj := Evaluated.Scene.ObjectAt(2);
    CheckCore(Obj.Kind = tsoCircle, 'circle was changed to another shape');
    CheckNearCore(Obj.Center.X, 1, 0.00001, 'circle center x (mm)');
    CheckNearCore(Obj.Center.Y, 2, 0.00001, 'circle center y (mm)');
    CheckNearCore(Obj.RadiusX, 3, 0.00001, 'circle radius (mm)');
    Obj := Evaluated.Scene.ObjectAt(3);
    CheckCore(Obj.Kind = tsoCircle, 'inch-sized circle was not imported');
    CheckNearCore(Obj.RadiusX, 2.54, 0.00001,
      'TeX inch dimension should convert to millimeters');
  finally
    Evaluated.Free;
    Syntax.Free;
  end;
end;

procedure TestBareDrawingStyleFlags;
const
  Source = '\begin{tikzpicture}' +
    '\path[draw] (0,0)--(1,0);' +
    '\path[fill] (0,1) rectangle +(1,1);' +
    '\path[dashed] (0,2)--(1,2);' +
    '\path[dotted] (0,3)--(1,3);' +
    '\end{tikzpicture}';
var
  Syntax: TTikZSyntaxResult;
  Evaluated: TTikZSemanticResult;
  Obj: TTikZSceneObject;
begin
  Evaluated := ParseAndEvaluate(Source, Syntax);
  try
    CheckCore((Syntax.Outcome = tpoAccepted) and
      (Evaluated.Outcome = tsoAccepted),
      'bare draw, fill, dashed and dotted style flags should be accepted');
    CheckCore(Evaluated.Scene.ObjectCount = 4,
      'bare style flags changed the number or order of visible objects');
    Obj := Evaluated.Scene.ObjectAt(0);
    CheckCore(Obj.StrokeEnabled and (Obj.StrokeRGB = $000000),
      'bare draw should enable the default black stroke');
    Obj := Evaluated.Scene.ObjectAt(1);
    CheckCore(Obj.FillEnabled and (Obj.FillRGB = $000000) and
      not Obj.StrokeEnabled,
      'bare fill should enable the default black fill only');
    Obj := Evaluated.Scene.ObjectAt(2);
    CheckCore(Obj.LineKind = tlDashed,
      'bare dashed style flag should select the native dashed line style');
    Obj := Evaluated.Scene.ObjectAt(3);
    CheckCore(Obj.LineKind = tlDotted,
      'bare dotted style flag should select the native dotted line style');
  finally
    Evaluated.Free;
    Syntax.Free;
  end;
end;

procedure TestExactXColorGreen;
const
  GreenSource = '\begin{tikzpicture}' +
    '\path[fill=green] (0,0) rectangle +(1,1);' +
    '\end{tikzpicture}';
  WrongCaseSource = '\begin{tikzpicture}' +
    '\path[fill=Green] (0,0) rectangle +(1,1);' +
    '\end{tikzpicture}';
var
  Syntax: TTikZSyntaxResult;
  Evaluated: TTikZSemanticResult;
  Obj: TTikZSceneObject;
begin
  Evaluated := ParseAndEvaluate(GreenSource, Syntax);
  try
    CheckCore((Syntax.Outcome = tpoAccepted) and
      (Evaluated.Outcome = tsoAccepted) and
      (Evaluated.Scene.ObjectCount = 1),
      'the standard xcolor green fill should import as one object');
    Obj := Evaluated.Scene.ObjectAt(0);
    CheckCore(Obj.FillEnabled and (Obj.FillRGB = $00FF00),
      'xcolor green (rgb 0,1,0) must map to RGB $00FF00');
  finally
    Evaluated.Free;
    Syntax.Free;
  end;

  Evaluated := ParseAndEvaluate(WrongCaseSource, Syntax);
  try
    CheckCore(Syntax.Outcome = tpoAccepted,
      'case-sensitive color rejection should remain a semantic diagnostic');
    CheckCore(Evaluated.Outcome = tsoUnsupported,
      'undefined xcolor name Green must not alias lowercase green');
  finally
    Evaluated.Free;
    Syntax.Free;
  end;
end;

procedure TestOwnedStatementBindingSpans;
const
  Source = '\begin{tikzpicture}' +
    '\node at (0,0){label};' +
    '\draw (0,0)--(1,0);' +
    '\end{tikzpicture}';
var
  Syntax: TTikZSyntaxResult;
  Evaluated: TTikZSemanticResult;
  Binding: TTikZSourceBinding;
begin
  Evaluated := ParseAndEvaluate(Source, Syntax);
  try
    CheckCore((Syntax.Outcome = tpoAccepted) and
      (Evaluated.Outcome = tsoAccepted) and
      (Evaluated.Scene.ObjectCount = 2),
      'standalone node and path statements should remain editable');
    CheckCore(Evaluated.Scene.BindingCount = 2,
      'each visible object should retain one source binding');
    Binding := Evaluated.Scene.BindingAt(0);
    CheckCore(RawStringOf(Syntax.SourceSlice(Binding.StatementSpan)) =
      '\node at (0,0){label};',
      'standalone node binding must own its terminating semicolon');
    Binding := Evaluated.Scene.BindingAt(1);
    CheckCore(RawStringOf(Syntax.SourceSlice(Binding.StatementSpan)) =
      '\draw (0,0)--(1,0);',
      'path binding must own its terminating semicolon');
  finally
    Evaluated.Free;
    Syntax.Free;
  end;
end;

procedure TestRelativeCoordinateBases;
const
  Source = '\begin{tikzpicture}[x=1mm,y=1mm]' +
    '\draw (0,0)--+(1,0)--+(0,1)--++(2,0)--+(1,1)--++(0,2);' +
    '\end{tikzpicture}';
var
  Syntax: TTikZSyntaxResult;
  Evaluated: TTikZSemanticResult;
  Obj: TTikZSceneObject;
const
  ExpectedX: array[0..5] of Double = (0, 1, 0, 2, 3, 2);
  ExpectedY: array[0..5] of Double = (0, 0, 1, 0, 1, 2);
var
  I: SizeInt;
begin
  Evaluated := ParseAndEvaluate(Source, Syntax);
  try
    CheckCore((Syntax.Outcome = tpoAccepted) and
      (Evaluated.Outcome = tsoAccepted),
      'absolute, +, and ++ coordinates should remain editable');
    CheckCore(Evaluated.Scene.ObjectCount = 1,
      'relative coordinates should stay in one ordered path');
    Obj := Evaluated.Scene.ObjectAt(0);
    CheckCore(Length(Obj.Commands) = Length(ExpectedX),
      'relative coordinates changed the number of path points');
    for I := 0 to High(ExpectedX) do
    begin
      CheckNearCore(Obj.Commands[I].P1.X, ExpectedX[I], 0.00001,
        'relative-coordinate x');
      CheckNearCore(Obj.Commands[I].P1.Y, ExpectedY[I], 0.00001,
        'relative-coordinate y');
    end;
  finally
    Evaluated.Free;
    Syntax.Free;
  end;
end;


function LoadTikZFixture(const Name: string): TBytes;
var
  Stream: TFileStream;
  Root: string;
begin
  Root := GetEnvironmentVariable('TPX_CORE_FIXTURE_DIR');
  CheckCore(Root <> '', 'TPX_CORE_FIXTURE_DIR is not set');
  Stream := TFileStream.Create(IncludeTrailingPathDelimiter(Root) +
    'tikz' + PathDelim + Name, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, Stream.Size);
    if Stream.Size > 0 then Stream.ReadBuffer(Result[0], Stream.Size);
  finally
    Stream.Free;
  end;
end;

procedure TestDevTikZArrowAndHatchingGeometry;
var
  Source: TBytes;
  Syntax: TTikZSyntaxResult;
  Evaluated: TTikZSemanticResult;
  Obj: TTikZSceneObject;
  I: SizeInt;
begin
  Source := LoadTikZFixture('devtikz-arrow-hatching.tex');
  Syntax := ParseTikZ(Source, DefaultTikZParseOptions(tikTexInput));
  Evaluated := EvaluateTikZ(Syntax, DefaultTikZImportOptions);
  try
    CheckCore((Syntax.Outcome = tpoAccepted) and
      (Evaluated.Outcome = tsoAccepted),
      'actual DevTikZ arrow and hatching output should be importable');
    CheckCore(Evaluated.Scene.ObjectCount = 7,
      'bounds, arrow, hatch, and outline paths were lost or reordered');
    Obj := Evaluated.Scene.ObjectAt(0);
    CheckCore((Obj.Kind = tsoPath) and Obj.StrokeEnabled and
      not Obj.FillEnabled and (Obj.StrokeRGB = $FF0000) and
      (Length(Obj.Commands) = 2),
      'exported arrow shaft did not remain a red line');
    CheckNearCore(Obj.LineWidthMM, 0.5, 0.00001,
      'exported arrow shaft width (mm)');
    CheckNearCore(Obj.Commands[0].P1.X, 1, 0.00001,
      'exported arrow shaft start x');
    CheckNearCore(Obj.Commands[1].P1.X, 12, 0.00001,
      'exported arrow shaft end x');
    Obj := Evaluated.Scene.ObjectAt(1);
    CheckCore((Obj.Kind = tsoPath) and Obj.StrokeEnabled and
      Obj.FillEnabled and (Obj.StrokeRGB = $FF0000) and
      (Obj.FillRGB = $FF0000) and (Length(Obj.Commands) = 6) and
      (Obj.Commands[High(Obj.Commands)].Kind = tpcClose),
      'exported arrowhead polygon was not retained as filled geometry');
    CheckNearCore(Obj.Commands[1].P1.X, 7.8, 0.00001,
      'exported arrowhead upper wing x');
    CheckNearCore(Obj.Commands[1].P1.Y, 3.05, 0.00001,
      'exported arrowhead upper wing y');
    for I := 2 to 5 do
    begin
      Obj := Evaluated.Scene.ObjectAt(I);
      CheckCore((Obj.Kind = tsoPath) and Obj.StrokeEnabled and
        not Obj.FillEnabled and (Obj.StrokeRGB = $0000FF) and
        (Length(Obj.Commands) = 2),
        'each exported hatch stroke should remain an independent blue line');
      CheckNearCore(Obj.LineWidthMM, 0.125, 0.00001,
        'exported hatch stroke width (mm)');
    end;
    Obj := Evaluated.Scene.ObjectAt(6);
    CheckCore((Obj.Kind = tsoRectangle) and Obj.StrokeEnabled and
      not Obj.FillEnabled and (Obj.StrokeRGB = $000000),
      'exported rectangle outline was lost after the hatch paths');
    CheckNearCore(Obj.Origin.X, 2, 0.00001,
      'exported rectangle outline x');
    CheckNearCore(Obj.Origin.Y, 5, 0.00001,
      'exported rectangle outline y');
    CheckNearCore(Obj.WidthMM, 8, 0.00001,
      'exported rectangle outline width');
    CheckNearCore(Obj.HeightMM, 4, 0.00001,
      'exported rectangle outline height');
  finally
    Evaluated.Free;
    Syntax.Free;
  end;
end;

procedure TestOpacityOptions;
const
  Source = '\begin{tikzpicture}' +
    '\path[draw, fill=red, fill opacity=0.25] (0,0) rectangle +(1,1);' +
    '\path[draw, draw opacity=0.5] (0,2) -- (1,2);' +
    '\path[draw, fill=blue, opacity=0.75] (0,3) circle (1);' +
    '\end{tikzpicture}';
  OutOfRange = '\begin{tikzpicture}' +
    '\path[draw, opacity=1.5] (0,0) -- (1,1);' +
    '\end{tikzpicture}';
  { A translucent primitive with an arrowhead, as T_TikZ_Device writes it. }
  Group = '\begin{tikzpicture}' +
    '\begin{scope}[transparency group, opacity=0.5]' +
    '\path[draw=red] (0,0) -- (4,0);' +
    '\path[draw=red, fill=red] (4,0) -- (3,0.5) -- (3,-0.5) -- cycle;' +
    '\end{scope}' +
    '\path[draw] (0,1) -- (4,1);' +
    '\end{tikzpicture}';
  GroupWithOwnOpacity = '\begin{tikzpicture}' +
    '\begin{scope}[transparency group, opacity=0.5]' +
    '\path[draw, draw opacity=0.5] (0,0) -- (4,0);' +
    '\end{scope}' +
    '\end{tikzpicture}';
var
  Syntax: TTikZSyntaxResult;
  Evaluated: TTikZSemanticResult;
  Obj: TTikZSceneObject;
  Dependency: TTikZPropertyDependency;
begin
  Evaluated := ParseAndEvaluate(Source, Syntax);
  try
    CheckCore((Syntax.Outcome = tpoAccepted) and
      (Evaluated.Outcome = tsoAccepted) and
      (Evaluated.Scene.ObjectCount = 3),
      'fill opacity, draw opacity and opacity should import as native styles');
    Obj := Evaluated.Scene.ObjectAt(0);
    CheckCore((Obj.FillOpacity = 0.25) and (Obj.StrokeOpacity = 1),
      'fill opacity must set only the fill opacity');
    CheckCore(FindDependency(Obj, tdFillOpacity, Dependency) and
      (RawStringOf(Copy(BytesOf(Source), Dependency.ValueSpan.StartByte,
      Dependency.ValueSpan.EndByte - Dependency.ValueSpan.StartByte)) = '0.25'),
      'fill opacity must bind its source value for save-back');
    Obj := Evaluated.Scene.ObjectAt(1);
    CheckCore((Obj.StrokeOpacity = 0.5) and (Obj.FillOpacity = 1),
      'draw opacity must set only the stroke opacity');
    Obj := Evaluated.Scene.ObjectAt(2);
    CheckCore((Obj.StrokeOpacity = 0.75) and (Obj.FillOpacity = 0.75) and
      FindDependency(Obj, tdOpacity, Dependency),
      'opacity must set both opacities through one dependency');
  finally
    Evaluated.Free;
    Syntax.Free;
  end;
  Evaluated := ParseAndEvaluate(OutOfRange, Syntax);
  try
    CheckCore(Evaluated.Outcome = tsoUnsupported,
      'an opacity above 1 must not be clamped silently');
  finally
    Evaluated.Free;
    Syntax.Free;
  end;
  Evaluated := ParseAndEvaluate(Group, Syntax);
  try
    CheckCore((Evaluated.Outcome = tsoAccepted) and
      (Evaluated.Scene.ObjectCount = 3),
      'a transparency group scope with an opacity should import');
    Obj := Evaluated.Scene.ObjectAt(0);
    CheckCore((Obj.StrokeOpacity = 0.5) and (Obj.FillOpacity = 1),
      'an unfilled path in a translucent scope must take it for the stroke');
    Obj := Evaluated.Scene.ObjectAt(1);
    CheckCore((Obj.StrokeOpacity = 0.5) and (Obj.FillOpacity = 0.5),
      'paths in a translucent scope must take the scope opacity');
    CheckCore(FindDependency(Obj, tdOpacity, Dependency) and
      (Dependency.DependencySource = tdepUnsupported) and
      (RawStringOf(Copy(BytesOf(Group), Dependency.ValueSpan.StartByte,
      Dependency.ValueSpan.EndByte - Dependency.ValueSpan.StartByte)) = '0.5'),
      'the scope opacity must be bound to the scope option, not editable');
    Obj := Evaluated.Scene.ObjectAt(2);
    CheckCore((Obj.StrokeOpacity = 1) and
      not FindDependency(Obj, tdOpacity, Dependency),
      'the scope opacity must end with the scope');
  finally
    Evaluated.Free;
    Syntax.Free;
  end;
  Evaluated := ParseAndEvaluate(GroupWithOwnOpacity, Syntax);
  try
    CheckCore(Evaluated.Outcome = tsoUnsupported,
      'an opacity inside a translucent scope must not be multiplied silently');
  finally
    Evaluated.Free;
    Syntax.Free;
  end;
end;

procedure TestShadingOptions;
const
  Source = '\begin{tikzpicture}' +
    '\path[left color=red, right color=blue] (0,0) rectangle +(1,1);' +
    '\path[bottom color=red, top color=blue, shading angle=-60] (0,2) rectangle +(1,1);' +
    '\path[right color=blue, left color=red] (0,4) rectangle +(1,1);' +
    '\path[draw, inner color=red, outer color=white, fill opacity=0.5] (3,0) circle (1);' +
    '\end{tikzpicture}';
  ThreeStops = '\begin{tikzpicture}' +
    '\path[left color=red, middle color=green, right color=blue] (0,0) rectangle +(1,1);' +
    '\end{tikzpicture}';
var
  Syntax: TTikZSyntaxResult;
  Evaluated: TTikZSemanticResult;
  Obj: TTikZSceneObject;
  I: Integer;
begin
  Evaluated := ParseAndEvaluate(Source, Syntax);
  try
    CheckCore((Evaluated.Outcome = tsoAccepted) and
      (Evaluated.Scene.ObjectCount = 4),
      'axis and radial shadings should import as native gradients');
    for I := 0 to 2 do
    begin
      Obj := Evaluated.Scene.ObjectAt(I);
      CheckCore((Obj.Gradient = tgLinear) and Obj.FillEnabled and
        (Obj.FillRGB = $FF0000) and (Obj.GradientRGB = $0000FF),
        Format('axis shading %d should run from red to blue', [I]));
    end;
    CheckCore(Abs(Evaluated.Scene.ObjectAt(0).GradientAngle) < 1E-9,
      'left/right colors run at angle 0');
    CheckCore(Abs(Evaluated.Scene.ObjectAt(1).GradientAngle - 30) < 1E-9,
      'bottom/top colors with shading angle -60 run at 30 degrees');
    CheckCore(Abs(Evaluated.Scene.ObjectAt(2).GradientAngle) < 1E-9,
      'left/right colors in either order run at angle 0');
    Obj := Evaluated.Scene.ObjectAt(3);
    CheckCore((Obj.Gradient = tgRadial) and (Obj.FillRGB = $FF0000) and
      (Obj.GradientRGB = $FFFFFF) and (Obj.FillOpacity = 0.5),
      'inner/outer colors import as a radial gradient with its opacity');
  finally
    Evaluated.Free;
    Syntax.Free;
  end;
  Evaluated := ParseAndEvaluate(ThreeStops, Syntax);
  try
    CheckCore(Evaluated.Outcome = tsoUnsupported,
      'a middle color has no two-stop gradient form');
  finally
    Evaluated.Free;
    Syntax.Free;
  end;
end;

function EvaluateFragment(const Value: RawByteString;
  out Syntax: TTikZSyntaxResult): TTikZSemanticResult;
begin
  Syntax := ParseTikZ(BytesOf(Value),
    DefaultTikZParseOptions(tikFragmentInput));
  Result := EvaluateTikZ(Syntax, DefaultTikZImportOptions);
end;

{ The fixture is TpX's own export of four gradients whose ends differ in
  opacity: a stroked rectangle (blue to yellow, 1 to 0.2, 30 degrees), an
  unstroked radial disc (red, 1 to 0), a stroked rectangle (0.5 to 1,
  0 degrees) and an unstroked rectangle at 180 degrees (red to blue, 0.3 to
  0.9). }
procedure TestTpXFadingImport;
var
  Source: RawByteString;
  Syntax: TTikZSyntaxResult;
  Evaluated: TTikZSemanticResult;
  Obj: TTikZSceneObject;
  procedure Reject(const Variant, Reason: RawByteString);
  var
    S: TTikZSyntaxResult;
    E: TTikZSemanticResult;
  begin
    E := EvaluateFragment(Variant, S);
    try
      CheckCore(E.Outcome = tsoUnsupported, Reason);
      CheckCore((Length(E.Diagnostics) > 0) and
        (E.Diagnostics[High(E.Diagnostics)].Span.StartLine > 0),
        Reason + ' (with a source location)');
    finally
      E.Free;
      S.Free;
    end;
  end;
begin
  Source := RawStringOf(LoadTikZFixture('devtikz-faded-gradients.tikz'));
  Evaluated := EvaluateFragment(Source, Syntax);
  try
    if Length(Evaluated.Diagnostics) > 0 then
      CheckCore(Evaluated.Outcome = tsoAccepted,
        'TpX fading declarations and faded shadings should import: ' +
        Evaluated.Diagnostics[0].Message);
    CheckCore(Evaluated.Outcome = tsoAccepted,
      'TpX fading declarations and faded shadings should import');
    CheckCore((Evaluated.Scene.ObjectCount = 6) and
      (Length(Evaluated.Scene.Fadings) = 4),
      'four faded fills, two stroke paths and four declarations');
    Obj := Evaluated.Scene.ObjectAt(0);
    CheckCore((Obj.Gradient = tgLinear) and (Obj.FillRGB = $0000FF) and
      (Obj.GradientRGB = $FFFF00) and (Abs(Obj.GradientAngle - 30) < 1E-9),
      'the faded linear shading keeps its colours and angle');
    CheckNearCore(Obj.FillOpacity, 1, 1E-9, 'start opacity from the fading');
    CheckNearCore(Obj.GradientOpacity, 0.2, 1E-9, 'end opacity from the fading');
    CheckCore((Obj.StrokeCompanion = 1) and
      (Evaluated.Scene.ObjectAt(1).StrokeOf = 0) and
      Evaluated.Scene.ObjectAt(1).StrokeEnabled,
      'the stroke path completes the faded fill');
    CheckNearCore(Obj.FadingShift.X, 15, 1E-9, 'fading shift x (mm)');
    CheckNearCore(Obj.FadingXScale, 1.28577, 1E-9, 'fading ramp scale');
    Obj := Evaluated.Scene.ObjectAt(2);
    CheckCore((Obj.Gradient = tgRadial) and (Obj.FillRGB = $D00000) and
      (Obj.StrokeCompanion = -1) and (Obj.GradientOpacity = 0) and
      (Obj.FillOpacity = 1),
      'an unstroked radial fade has no stroke companion');
    Obj := Evaluated.Scene.ObjectAt(3);
    CheckCore((Obj.StrokeCompanion = 4) and (Abs(Obj.GradientAngle) < 1E-9) and
      (Abs(Obj.FillOpacity - 0.5) < 1E-9) and (Obj.GradientOpacity = 1),
      'a fade from 0.5 to 1 at 0 degrees');
    Obj := Evaluated.Scene.ObjectAt(5);
    CheckCore((Obj.Gradient = tgLinear) and
      (Abs(Obj.GradientAngle - 180) < 1E-9) and
      (Obj.FillRGB = $FF0000) and (Obj.GradientRGB = $0000FF),
      'bottom/top colours with shading angle 90 run at 180 degrees');
    CheckNearCore(Obj.FillOpacity, 0.3, 1E-9, 'start opacity at 180 degrees');
    CheckNearCore(Obj.GradientOpacity, 0.9, 1E-9, 'end opacity at 180 degrees');
  finally
    Evaluated.Free;
    Syntax.Free;
  end;
  Reject(StringReplace(Source, 'path fading=tpxfadeR1x0',
    'path fading=south', []), 'a foreign fading is unsupported');
  Reject(StringReplace(Source, 'gray(50bp)=(0)', 'gray(50bp)=(0.5)', []),
    'a fading declaration TpX does not write is unsupported');
  Reject(StringReplace(Source, 'tpxfadeR1x0s}{', 'tpxfadeR1x0p5s}{', []),
    'a fading whose name disagrees with its opacities is unsupported');
  Reject(StringReplace(Source, 'rotate=30,', 'rotate=40,', []),
    'a linear fading off the gradient direction is unsupported');
  Reject(StringReplace(Source, 'fit fading=true]', 'fit fading=false]', []),
    'a radial fading must be fitted');
  Reject(StringReplace(Source, 'inner color=F, outer color=G, path',
    'draw, inner color=F, outer color=G, path', []),
    'a faded shading must not be stroked');
  Reject(StringReplace(Source, 'inner color=F, outer color=G, path',
    'fill opacity=0.5, inner color=F, outer color=G, path', []),
    'a faded shading takes no fill opacity of its own');
  Reject(StringReplace(Source, 'inner color=F, outer color=G, path',
    'left color=F, right color=G, path', []),
    'a radial fading on an axis shading is unsupported');
  Reject(StringReplace(StringReplace(Source, '\path[inner color=F',
    '\begin{scope}[transparency group, opacity=0.5]\path[inner color=F',
    []), 'circle (5.00mm);', 'circle (5.00mm);\end{scope}', []),
    'a fading inside a translucent scope is unsupported');
end;

initialization
  RegisterCoreTest('tikz-opacity-options', TestOpacityOptions);
  RegisterCoreTest('tikz-shading-options', TestShadingOptions);
  RegisterCoreTest('tikz-tpx-fading-import', TestTpXFadingImport);
  RegisterCoreTest('tikz-pgf-scope-transform-order',
    TestPGFScopeTransformOrder);
  RegisterCoreTest('tikz-attached-devtikz-label-font',
    TestAttachedDevTikZLabelAndFont);
  RegisterCoreTest('tikz-node-text-color-profile',
    TestNodeTextColorAndFrameProfile);
  RegisterCoreTest('tikz-arc-sector-circle-semantics',
    TestArcSectorAndCircleMaterializationProfile);
  RegisterCoreTest('tikz-bare-drawing-style-flags',
    TestBareDrawingStyleFlags);
  RegisterCoreTest('tikz-exact-xcolor-green', TestExactXColorGreen);
  RegisterCoreTest('tikz-owned-statement-binding-spans',
    TestOwnedStatementBindingSpans);
  RegisterCoreTest('tikz-relative-coordinate-bases',
    TestRelativeCoordinateBases);
  RegisterCoreTest('tikz-devtikz-arrow-hatching-geometry',
    TestDevTikZArrowAndHatchingGeometry);

end.

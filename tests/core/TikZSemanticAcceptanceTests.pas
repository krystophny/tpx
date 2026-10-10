unit TikZSemanticAcceptanceTests;

{$mode delphi}{$H+}

interface

implementation

uses SysUtils, Classes, Math, TikZLexer, TikZSyntax, TikZImport,
  CoreTestSupport;

function AcceptanceFixtureDirectory: string;
begin
  Result := GetEnvironmentVariable('TPX_ACCEPTANCE_FIXTURE_DIR');
  CheckCore(Result <> '', 'TPX_ACCEPTANCE_FIXTURE_DIR is not set');
end;

function LoadAcceptanceFixture: TBytes;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(IncludeTrailingPathDelimiter(
    AcceptanceFixtureDirectory) + 'independent-scene.tex',
    fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, Stream.Size);
    if Stream.Size > 0 then Stream.ReadBuffer(Result[0], Stream.Size);
  finally
    Stream.Free;
  end;
end;

function BytesFromRaw(const Value: RawByteString): TBytes;
var
  I: SizeInt;
begin
  SetLength(Result, Length(Value));
  for I := 1 to Length(Value) do Result[I - 1] := Ord(Value[I]);
end;

function RawFromBytes(const Value: TBytes): RawByteString;
var
  I: SizeInt;
begin
  SetLength(Result, Length(Value));
  for I := 0 to High(Value) do Result[I + 1] := AnsiChar(Value[I]);
end;

function ParseAndEvaluate(const Source: TBytes;
  out Syntax: TTikZSyntaxResult): TTikZSemanticResult;
begin
  Syntax := ParseTikZ(Source, DefaultTikZParseOptions(tikTexInput));
  Result := EvaluateTikZ(Syntax, DefaultTikZImportOptions);
end;

procedure TestIndependentTikZSemanticScene;
var
  Source: TBytes;
  Syntax: TTikZSyntaxResult;
  Evaluated: TTikZSemanticResult;
  Scene: TTikZScene;
  Obj: TTikZSceneObject;
  Angle, ExpectedX, ExpectedY: Double;
begin
  Source := LoadAcceptanceFixture;
  CheckCore(Pos('%<TpX', RawFromBytes(Source)) = 0,
    'The independent TikZ fixture must not rely on TpX metadata');
  Evaluated := ParseAndEvaluate(Source, Syntax);
  try
    CheckCore(Syntax.Outcome = tpoAccepted,
      'Independent TeX wrapper was not accepted by the bounded parser');
    CheckCore(Syntax.Region.Found and not Syntax.Region.IsFragment,
      'Parser did not select the one enclosed tikzpicture');
    CheckCore(Evaluated.Outcome = tsoAccepted,
      'Supported independent scene was not semantically imported');
    Scene := Evaluated.Scene;
    CheckCore(Scene <> nil, 'Semantic import lost its scene');
    CheckCore(Scene.ObjectCount = 6,
      'Metadata-free fixture did not produce all six semantic objects');

    Obj := Scene.ObjectAt(0);
    CheckCore(Obj.Kind = tsoPath, 'First object is not the styled line');
    CheckCore(Length(Obj.Commands) = 2, 'Line path lost its endpoints');
    CheckCore(Obj.Commands[0].Kind = tpcMove, 'Line has no move command');
    CheckCore(Obj.Commands[1].Kind = tpcLine, 'Line endpoint is not a line');
    CheckNearCore(Obj.Commands[0].P1.X, 7.7712094323, 0.0001,
      'nested shift/rotation/scale line start x (mm)');
    CheckNearCore(Obj.Commands[0].P1.Y, 2.3657584558, 0.0001,
      'nested shift/rotation/scale line start y (mm)');
    CheckNearCore(Obj.Commands[1].P1.X, 11.6763934607, 0.0001,
      'nested shift/rotation/scale line end x (mm)');
    CheckNearCore(Obj.Commands[1].P1.Y, 3.2315169115, 0.0001,
      'nested shift/rotation/scale line end y (mm)');
    CheckCore(Obj.StrokeRGB = $336699, 'Line RGB color changed');
    CheckNearCore(Obj.LineWidthMM, 0.75, 0.0001, 'line width (mm)');
    CheckCore(Obj.LineKind = tlDashed, 'Line dash style was lost');
    CheckNearCore(Obj.DashOnMM, 2.0, 0.0001, 'line dash-on (mm)');
    CheckNearCore(Obj.DashOffMM, 1.0, 0.0001, 'line dash-off (mm)');

    Obj := Scene.ObjectAt(1);
    CheckCore((Obj.Kind = tsoPath) and (Length(Obj.Commands) = 2),
      'Cubic path object or commands were lost');
    CheckCore(Obj.Commands[1].Kind = tpcCubic,
      'Cubic controls were reduced to another path command');
    CheckNearCore(Obj.Commands[0].P1.X, 3.6495857898, 0.0001,
      'nested cubic start x');
    CheckNearCore(Obj.Commands[0].P1.Y, 2.4762960071, 0.0001,
      'nested cubic start y');
    CheckNearCore(Obj.Commands[1].P1.X, 7.5547698183, 0.0001,
      'nested cubic control 1 x');
    CheckNearCore(Obj.Commands[1].P1.Y, 3.3420544629, 0.0001,
      'nested cubic control 1 y');
    CheckNearCore(Obj.Commands[1].P2.X, 7.3383302044, 0.0001,
      'nested cubic control 2 x');
    CheckNearCore(Obj.Commands[1].P2.Y, 4.3183504700, 0.0001,
      'nested cubic control 2 y');
    CheckNearCore(Obj.Commands[1].P3.X, 11.2435142329, 0.0001,
      'nested cubic end x');
    CheckNearCore(Obj.Commands[1].P3.Y, 5.1841089257, 0.0001,
      'nested cubic end y');

    Obj := Scene.ObjectAt(2);
    CheckCore(Obj.Kind = tsoRectangle, 'Rotated rectangle changed object kind');
    CheckNearCore(Obj.Origin.X, 10.25, 0.0001, 'rectangle origin x');
    CheckNearCore(Obj.Origin.Y, 5.5, 0.0001, 'rectangle origin y');
    CheckNearCore(Obj.WidthMM, 8.5, 0.0001, 'rectangle width (mm)');
    CheckNearCore(Obj.HeightMM, 4.25, 0.0001, 'rectangle height (mm)');
    CheckNearCore(Obj.Rotation, 17.5 * Pi / 180, 0.000001,
      'rectangle rotation (radians)');
    CheckNearCore(Obj.RotationCenter.X, 10.25, 0.0001,
      'rectangle rotation center x');
    CheckNearCore(Obj.RotationCenter.Y, 5.5, 0.0001,
      'rectangle rotation center y');
    Angle := 17.5 * Pi / 180;
    ExpectedX := Obj.Origin.X + Obj.WidthMM * Cos(Angle)
      - Obj.HeightMM * Sin(Angle);
    ExpectedY := Obj.Origin.Y + Obj.WidthMM * Sin(Angle)
      + Obj.HeightMM * Cos(Angle);
    CheckNearCore(Obj.Origin.X + Obj.WidthMM * Cos(Angle),
      18.3565940814, 0.0001,
      'rotated rectangle width corner x');
    CheckNearCore(Obj.Origin.Y + Obj.WidthMM * Sin(Angle),
      8.0559992958, 0.0001,
      'rotated rectangle width corner y');
    CheckNearCore(Obj.Origin.X - Obj.HeightMM * Sin(Angle),
      8.9720003521, 0.0001,
      'rotated rectangle height corner x');
    CheckNearCore(Obj.Origin.Y + Obj.HeightMM * Cos(Angle),
      9.5532970407, 0.0001,
      'rotated rectangle height corner y');
    CheckNearCore(ExpectedX, 17.0785944335, 0.0001,
      'rotated rectangle opposite corner x');
    CheckNearCore(ExpectedY, 12.1092963365, 0.0001,
      'rotated rectangle opposite corner y');
    CheckNearCore(Obj.Center.X, 14.5, 0.0001,
      'rectangle local center x');
    CheckNearCore(Obj.Center.Y, 7.625, 0.0001,
      'rectangle local center y');
    CheckCore(Obj.StrokeRGB = $663399, 'Rectangle stroke RGB changed');
    CheckCore(Obj.FillRGB = $DDEEFF, 'Rectangle fill RGB changed');

    Obj := Scene.ObjectAt(3);
    CheckCore((Obj.Kind = tsoText) and (Obj.TextAnchor = 'base west'),
      'West text anchor was not preserved');
    CheckCore(Obj.TextBody = 'Label ' + UTF8Encode(WideChar($03B2)),
      'First Unicode text body was altered');
    CheckNearCore(Obj.Center.X, 1.25, 0.0001, 'west text x');
    CheckNearCore(Obj.Center.Y, 10.5, 0.0001, 'west text y');
    Obj := Scene.ObjectAt(4);
    CheckCore((Obj.Kind = tsoText) and (Obj.TextAnchor = 'base east'),
      'East text anchor was not preserved');
    CheckCore(Obj.TextBody = '$x^2$', 'TeX text body was altered');
    CheckNearCore(Obj.Center.X, 10.25, 0.0001, 'east text x');
    CheckNearCore(Obj.Center.Y, 14.0, 0.0001, 'east text y');

    Obj := Scene.ObjectAt(5);
    CheckCore(Obj.Kind = tsoImage, 'Image include was omitted');
    CheckCore(Obj.ImageReference = 'acceptance-image.png',
      'Image reference was changed');
    CheckNearCore(Obj.WidthMM, 4.0, 0.0001, 'image width (mm)');
    CheckNearCore(Obj.HeightMM, 3.0, 0.0001, 'image height (mm)');
    CheckNearCore(Obj.Center.X, 12.0, 0.0001, 'image anchor x');
    CheckNearCore(Obj.Center.Y, 11.0, 0.0001, 'image anchor y');
  finally
    Evaluated.Free;
    Syntax.Free;
  end;
end;

procedure TestUnsupportedTikZFeatureCannotBeSilentlyDropped;
var
  Source: TBytes;
  Syntax: TTikZSyntaxResult;
  Evaluated: TTikZSemanticResult;
  Text: RawByteString;
  EndAt: SizeInt;
begin
  Source := LoadAcceptanceFixture;
  Text := RawFromBytes(Source);
  EndAt := Pos('\end{tikzpicture}', Text);
  CheckCore(EndAt > 0, 'Fixture lost its selected picture terminator');
  Insert('\path[opacity=0.5] (0,0) -- (1,1);' + #10,
    Text, EndAt);
  Source := BytesFromRaw(Text);
  Evaluated := ParseAndEvaluate(Source, Syntax);
  try
    CheckCore(Syntax.Outcome = tpoAccepted,
      'Unrepresentable style should remain syntactically parseable');
    CheckCore(Evaluated.Outcome = tsoUnsupported,
      'Unrepresentable opacity was silently accepted for save-back');
  finally
    Evaluated.Free;
    Syntax.Free;
  end;
end;

procedure TestUnsupportedArrowTipIsNotSubstituted;
var
  Source: TBytes;
  Syntax: TTikZSyntaxResult;
  Evaluated: TTikZSemanticResult;
  Text: RawByteString;
  EndAt: SizeInt;
begin
  Source := LoadAcceptanceFixture;
  Text := RawFromBytes(Source);
  EndAt := Pos('\end{tikzpicture}', Text);
  CheckCore(EndAt > 0, 'Fixture lost its selected picture terminator');
  Insert('\path[->] (0,0) -- (1,1);' + #10, Text, EndAt);
  Source := BytesFromRaw(Text);
  Evaluated := ParseAndEvaluate(Source, Syntax);
  try
    CheckCore(Evaluated.Outcome = tsoUnsupported,
      'Unrepresentable arrow tip was accepted or silently replaced');
  finally
    Evaluated.Free;
    Syntax.Free;
  end;
end;

initialization
  RegisterCoreTest('independent-tikz-semantic-scene',
    TestIndependentTikZSemanticScene);
  RegisterCoreTest('unsupported-tikz-style-is-not-silently-dropped',
    TestUnsupportedTikZFeatureCannotBeSilentlyDropped);
  RegisterCoreTest('unsupported-tikz-arrow-is-not-substituted',
    TestUnsupportedArrowTipIsNotSubstituted);

end.

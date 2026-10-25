unit TikZImportDrawing;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, Drawings, GObjects, TikZImport, DocumentFormats;

type
  TTikZAssetResolver = function(const DocumentFileName, Reference: string;
    out LocalPath: string): Boolean;

function ResolveTikZLocalAsset(const DocumentFileName, Reference: string;
  out LocalPath: string): Boolean;
function MaterializeTikZScene(Scene: TTikZScene; Drawing: TDrawing2D;
  Context: TTikZImportContext; Resolver: TTikZAssetResolver;
  const DocumentFileName: string; Diagnostics: TStrings): Boolean;
function TikZDocumentLoader(Candidate: TDrawing2D; const FileName: TDocumentPath;
  const SourceBytes: RawByteString; Diagnostics: TStrings;
  out CodecContext: TObject): Boolean;
procedure RegisterTikZDocumentLoader;
{ Native arrowheads with a core TikZ arrow tip: h40 is the open "to" tip,
  h44 the filled "latex" triangle and h42 the notched "stealth" tip. }
function TikZTipArrowKind(const Tip: string): TArrowKind;
function ArrowKindTikZTip(Kind: TArrowKind; out Tip: string): Boolean;
{ Fading transform of a linear gradient whose ends differ in opacity, laid
  out on the box of the primitive's gradient fill as TpX writes it. }
function TikZLinearFadingOf(Primitive: TPrimitive2D; out Shift: TTikZPoint;
  out Rotation, XScale, YScale: Double): Boolean;

implementation

uses Contnrs, Types, Math, Graphics, Devices, Geometry, GObjBase, Bitmaps,
  TikZSyntax, TikZLexer, DocumentIO, Solid3D, Input, StrUtils;

type
  TTikZPointArray = array of TPoint2D;

function ScenePoint(const P: TTikZPoint): TPoint2D; inline;
begin
  Result := Point2D(P.X, P.Y);
end;

function SceneColor(RGB: LongWord): TColor; inline;
begin
  Result := RGBToColor((RGB shr 16) and $FF, (RGB shr 8) and $FF, RGB and $FF);
end;

procedure AddDiagnostic(Diagnostics: TStrings; const Span: TTikZSourceSpan;
  const Message: string);
begin
  if Diagnostics <> nil then
    Diagnostics.Add(Format('%d:%d: %s',
      [Span.StartLine, Span.StartColumn, Message]));
end;

function ImportFailureText(Diagnostics: TStrings;
  const Fallback: string): string;
begin
  if (Diagnostics <> nil) and (Diagnostics.Count > 0) then
    Result := Diagnostics[Diagnostics.Count - 1]
  else
    Result := Fallback;
end;

function ResolveTikZLocalAsset(const DocumentFileName, Reference: string;
  out LocalPath: string): Boolean;
var
  BaseDir: string;
begin
  Result := False;
  LocalPath := '';
  if (Reference = '') or (ExtractFileName(Reference) <> Reference) or
    (Pos(':', Reference) > 0) or (Pos('\', Reference) > 0) or
    (Pos('/', Reference) > 0) then Exit;
  if not (SameText(ExtractFileExt(Reference), '.png') or
    SameText(ExtractFileExt(Reference), '.jpg') or
    SameText(ExtractFileExt(Reference), '.jpeg') or
    SameText(ExtractFileExt(Reference), '.bmp')) then Exit;
  BaseDir := ExtractFilePath(ExpandFileName(DocumentFileName));
  LocalPath := ExpandFileName(BaseDir + Reference);
  Result := FileExists(LocalPath);
end;

function TikZTipArrowKind(const Tip: string): TArrowKind;
begin
  if Tip = 'to' then Result := arrH40
  else if Tip = 'latex' then Result := arrH44
  else if Tip = 'stealth' then Result := arrH42
  else Result := arrNone;
end;

function ArrowKindTikZTip(Kind: TArrowKind; out Tip: string): Boolean;
begin
  Result := True;
  case Kind of
    arrNone: Tip := '';
    arrH40: Tip := 'to';
    arrH44: Tip := 'latex';
    arrH42: Tip := 'stealth';
  else
    Tip := '';
    Result := False;
  end;
end;

procedure ApplyStrokeStyle(Primitive: TPrimitive2D;
  const Obj: TTikZSceneObject; Drawing: TDrawing2D);
begin
  if Obj.StrokeEnabled then
  begin
    case Obj.LineKind of
      tlDashed: Primitive.LineStyle := liDashed;
      tlDotted: Primitive.LineStyle := liDotted;
    else
      Primitive.LineStyle := liSolid;
    end;
    Primitive.LineColor := SceneColor(Obj.StrokeRGB);
    if Drawing.LineWidthBase > 0 then
      Primitive.LineWidth := Obj.LineWidthMM / Drawing.LineWidthBase
    else Primitive.LineWidth := 1;
  end
  else Primitive.LineStyle := liNone;
  Primitive.BeginArrowKind := TikZTipArrowKind(Obj.BeginArrow);
  Primitive.EndArrowKind := TikZTipArrowKind(Obj.EndArrow);
  Primitive.LineOpacity := Obj.StrokeOpacity;
end;

procedure ApplyStyle(Primitive: TPrimitive2D; const Obj: TTikZSceneObject;
  Drawing: TDrawing2D);
begin
  if Obj.Kind = tsoText then
  begin
    { TText2D.LineColor controls glyphs; node draw/fill is rejected by the
      semantic importer because native text has no matching frame primitive. }
    Primitive.LineColor := SceneColor(Obj.StrokeRGB);
    Primitive.LineStyle := liNone;
    Primitive.FillColor := clDefault;
    Exit;
  end;
  ApplyStrokeStyle(Primitive, Obj, Drawing);
  if Obj.FillEnabled then Primitive.FillColor := SceneColor(Obj.FillRGB)
  else Primitive.FillColor := clDefault;
  Primitive.FillOpacity := Obj.FillOpacity;
  case Obj.Gradient of
    tgLinear: Primitive.FillGradient := fgLinear;
    tgRadial: Primitive.FillGradient := fgRadial;
  else
    Primitive.FillGradient := fgNone;
  end;
  if Obj.Gradient <> tgNone then
  begin
    Primitive.GradientColor := SceneColor(Obj.GradientRGB);
    Primitive.GradientAngle := Obj.GradientAngle;
    Primitive.GradientOpacity := Obj.GradientOpacity;
  end;
end;

{ The box of the gradient fill of a primitive, as TPrimitive2D.DeviceDrawPiece
  hands it to the devices. }
function GradientBox(Primitive: TPrimitive2D; out Box: TRect2D): Boolean;
var
  I: Integer;
begin
  Result := False;
  Primitive.UpdateExtension(nil);
  for I := 0 to Primitive.Pieces.Count - 1 do
    if Primitive.Pieces[I].GetFillGradient(Primitive) <> fgNone then
    begin
      Box := Primitive.Pieces[I].GetBoundingBox;
      Exit(True);
    end;
end;

function TikZLinearFadingOf(Primitive: TPrimitive2D; out Shift: TTikZPoint;
  out Rotation, XScale, YScale: Double): Boolean;
var
  Paint: TPaintStyle;
  P1, P2: TPoint2D;
  A, B: TTikZPoint;
begin
  Paint := DefaultPaintStyle;
  Paint.Gradient := fgLinear;
  Paint.GradientAngle := Primitive.GradientAngle;
  Result := GradientBox(Primitive, Paint.Box) and HasGradient(Paint);
  if not Result then Exit;
  LinearGradientEnds(Paint, P1, P2);
  A.X := P1.X; A.Y := P1.Y;
  B.X := P2.X; B.Y := P2.Y;
  TikZLinearFadingTransform(A, B,
    PointDistance2D(Paint.Box.FirstEdge, Paint.Box.SecondEdge),
    Shift, Rotation, XScale, YScale);
end;

{ A linear TpX fading is placed along the gradient of its path; its
  transform must match the one TpX writes for the box of the native fill. }
function FadingMatchesBox(Primitive: TPrimitive2D; const Obj: TTikZSceneObject;
  Scene: TTikZScene): Boolean;
var
  Shift: TTikZPoint;
  Rotation, XScale, YScale: Double;
begin
  Result := True;
  if (Obj.FadingName = '') or (Obj.Gradient <> tgLinear) then Exit;
  if not TikZLinearFadingOf(Primitive, Shift, Rotation, XScale, YScale) then
    Exit(False);
  { The direction was checked against the shading during evaluation. }
  Result := SameTikZFadingTransform(Obj.FadingShift, Obj.FadingRotation,
    Obj.FadingXScale, Obj.FadingYScale, Shift, Obj.FadingRotation, XScale,
    YScale, Scene.PictureScaleX);
end;

function RotatePoint(const Point, Center: TPoint2D; Angle: Double): TPoint2D;
var
  X, Y, C, S: Double;
begin
  C := Cos(Angle);
  S := Sin(Angle);
  X := Point.X - Center.X;
  Y := Point.Y - Center.Y;
  Result.X := Center.X + C * X - S * Y;
  Result.Y := Center.Y + S * X + C * Y;
  Result.W := 1;
end;

function BuildPath(const Obj: TTikZSceneObject): TPrimitive2D;
var
  I, PointCount, N: SizeInt;
  HasCubic, Closed: Boolean;
  CurrentPoint, NextPoint, C1, C2: TPoint2D;
  Points: TTikZPointArray;
  Command: TTikZPathCommand;
  procedure Append(const P: TPoint2D);
  begin
    Points[N] := P;
    Inc(N);
  end;
begin
  Result := nil;
  HasCubic := False;
  Closed := False;
  PointCount := 0;
  for I := 0 to High(Obj.Commands) do
  begin
    Command := Obj.Commands[I];
    case Command.Kind of
      tpcMove: Inc(PointCount);
      tpcLine: Inc(PointCount, 3);
      tpcCubic: begin Inc(PointCount, 3); HasCubic := True; end;
      tpcClose: Closed := True;
    end;
  end;
  if (PointCount < 2) or (Obj.Commands[0].Kind <> tpcMove) then Exit;
  if not HasCubic then
  begin
    SetLength(Points, 1 + PointCount div 3);
    N := 0;
    Append(ScenePoint(Obj.Commands[0].P1));
    for I := 1 to High(Obj.Commands) do
      if Obj.Commands[I].Kind = tpcLine then
        Append(ScenePoint(Obj.Commands[I].P1));
    if Closed then Result := TPolygon2D.CreateSpec(-1, Points)
    else Result := TPolyline2D.CreateSpec(-1, Points);
    Exit;
  end;
  SetLength(Points, PointCount);
  N := 0;
  CurrentPoint := ScenePoint(Obj.Commands[0].P1);
  Append(CurrentPoint);
  for I := 1 to High(Obj.Commands) do
  begin
    Command := Obj.Commands[I];
    case Command.Kind of
      tpcLine:
        begin
          NextPoint := ScenePoint(Command.P1);
          C1 := Point2D(CurrentPoint.X + (NextPoint.X - CurrentPoint.X) / 3,
            CurrentPoint.Y + (NextPoint.Y - CurrentPoint.Y) / 3);
          C2 := Point2D(CurrentPoint.X + 2 * (NextPoint.X - CurrentPoint.X) / 3,
            CurrentPoint.Y + 2 * (NextPoint.Y - CurrentPoint.Y) / 3);
          Append(C1);
          Append(C2);
          Append(NextPoint);
          CurrentPoint := NextPoint;
        end;
      tpcCubic:
        begin
          Append(ScenePoint(Command.P1));
          Append(ScenePoint(Command.P2));
          NextPoint := ScenePoint(Command.P3);
          Append(NextPoint);
          CurrentPoint := NextPoint;
        end;
    end;
  end;
  if Closed then Result := TClosedBezierPath2D.CreateSpec(-1, Points)
  else Result := TBezierPath2D.CreateSpec(-1, Points);
end;

function BuildNativeObject(const Obj: TTikZSceneObject; Drawing: TDrawing2D;
  AssetResolver: TTikZAssetResolver; const DocumentFileName: string;
  AssetEntries: TStrings; Diagnostics: TStrings): TObject2D;
var
  Primitive: TPrimitive2D;
  P0, P1, P2, Center, U, V: TPoint2D;
  LocalPath: string;
  Entry: TBitmapEntry;
  Idx: SizeInt;
  Angle: Double;
  TextObj: TText2D;
begin
  Result := nil;
  Primitive := nil;
  case Obj.Kind of
    tsoPath: Primitive := BuildPath(Obj);
    tsoRectangle:
      begin
        Angle := Obj.Rotation;
        P0 := ScenePoint(Obj.Origin);
        if Angle <> 0 then
          P0 := RotatePoint(P0, ScenePoint(Obj.RotationCenter), Angle);
        U := Point2D(Obj.WidthMM * Cos(Angle), Obj.WidthMM * Sin(Angle));
        V := Point2D(-Obj.HeightMM * Sin(Angle), Obj.HeightMM * Cos(Angle));
        P2 := Point2D(P0.X + V.X, P0.Y + V.Y);
        P1 := Point2D(P0.X + U.X + V.X, P0.Y + U.Y + V.Y);
        Primitive := TRectangle2D.Create(-1);
        Primitive.Points[0] := P0;
        Primitive.Points[1] := P1;
        Primitive.Points[2] := P2;
      end;
    tsoCircle:
      begin
        Center := ScenePoint(Obj.Center);
        Primitive := TCircle2D.Create(-1);
        Primitive.Points[0] := Center;
        Primitive.Points[1] := Point2D(Center.X + Obj.RadiusX,
          Center.Y);
      end;
    tsoEllipse:
      begin
        Angle := Obj.Rotation;
        Center := ScenePoint(Obj.Center);
        if Angle <> 0 then
          Center := RotatePoint(Center, ScenePoint(Obj.RotationCenter), Angle);
        U := Point2D(Obj.RadiusX * Cos(Angle), Obj.RadiusX * Sin(Angle));
        V := Point2D(-Obj.RadiusY * Sin(Angle), Obj.RadiusY * Cos(Angle));
        P0 := Point2D(Center.X - U.X - V.X, Center.Y - U.Y - V.Y);
        P1 := Point2D(Center.X + U.X + V.X, Center.Y + U.Y + V.Y);
        P2 := Point2D(Center.X - U.X + V.X, Center.Y - U.Y + V.Y);
        Primitive := TEllipse2D.Create(-1);
        Primitive.Points[0] := P0;
        Primitive.Points[1] := P1;
        Primitive.Points[2] := P2;
      end;
    tsoArc:
      begin
        Center := ScenePoint(Obj.Center);
        Primitive := TArc2D.CreateSpec(-1, Center, Obj.RadiusX,
          Obj.StartAngle, Obj.EndAngle);
      end;
    tsoSector:
      begin
        Center := ScenePoint(Obj.Center);
        Primitive := TSector2D.CreateSpec(-1, Center, Obj.RadiusX,
          Obj.StartAngle, Obj.EndAngle);
      end;
    tsoSegment:
      begin
        Center := ScenePoint(Obj.Center);
        Primitive := TSegment2D.CreateSpec(-1, Center, Obj.RadiusX,
          Obj.StartAngle, Obj.EndAngle);
      end;
    tsoText:
      begin
        TextObj := TText2D.Create(-1);
        TextObj.Points[0] := ScenePoint(Obj.Center);
        TextObj.Height := Obj.TextHeightMM;
        TextObj.Rot := Obj.Rotation;
        TextObj.Text := string(Obj.TextContent);
        { Keep only the payload in the native model. Generated font wrappers
          stay in the source binding and are restored by source-preserving save. }
        if (Pos('$', string(Obj.TextContent)) > 0) or
          (Pos('\', string(Obj.TextContent)) > 0) then
          TextObj.TeXText := string(Obj.TextContent)
        else
          TextObj.TeXText := '';
        if Pos('west', Obj.TextAnchor) > 0 then
          TextObj.HAlignment := ahLeft
        else if Pos('east', Obj.TextAnchor) > 0 then
          TextObj.HAlignment := ahRight
        else TextObj.HAlignment := ahCenter;
        Primitive := TextObj;
      end;
    tsoImage:
      begin
        if not Assigned(AssetResolver) or
          not AssetResolver(DocumentFileName, string(Obj.ImageReference), LocalPath) then
        begin
          AddDiagnostic(Diagnostics, Obj.SourceSpan,
            'Image reference is not a permitted existing local asset: ' +
            string(Obj.ImageReference));
          Exit;
        end;
        Idx := Drawing.BitmapRegistry.IndexOf(LocalPath);
        if Idx >= 0 then Entry := Drawing.BitmapRegistry.Objects[Idx] as TBitmapEntry
        else
        begin
          Idx := AssetEntries.IndexOf(LocalPath);
          if Idx >= 0 then Entry := TBitmapEntry(AssetEntries.Objects[Idx])
          else
          begin
            { Keep source-local assets relative to the imported document so
              save-as staging can carry them to the destination directory. }
            Entry := TBitmapEntry.Create(LocalPath, DocumentFileName);
            if Entry.Kind = bek_None then
            begin
              Entry.Free;
              AddDiagnostic(Diagnostics, Obj.SourceSpan,
                'Image asset could not be decoded by the native bitmap loader');
              Exit;
            end;
            AssetEntries.AddObject(LocalPath, Entry);
          end;
        end;
        P0 := ScenePoint(Obj.Center);
        P1 := Point2D(P0.X + Obj.WidthMM, P0.Y + Obj.HeightMM);
        Result := TBitmap2D.CreateSpec(-1, P0, P1, Entry);
        (Result as TBitmap2D).KeepAspectRatio := Obj.ImageKeepAspectRatio;
        Exit;
      end;
  else
    AddDiagnostic(Diagnostics, Obj.SourceSpan,
      'This TikZ object has no exact native drawing primitive');
    Exit;
  end;
  if Primitive = nil then
  begin
    AddDiagnostic(Diagnostics, Obj.SourceSpan,
      'TikZ path could not be represented by a native drawing primitive');
    Exit;
  end;
  ApplyStyle(Primitive, Obj, Drawing);
  Result := Primitive;
end;

function MaterializeTikZScene(Scene: TTikZScene; Drawing: TDrawing2D;
  Context: TTikZImportContext; Resolver: TTikZAssetResolver;
  const DocumentFileName: string; Diagnostics: TStrings): Boolean;
var
  StagedObjects: TObjectList;
  AssetEntries: TStringList;
  Obj: TTikZSceneObject;
  Native: TObject2D;
  Entry: TBitmapEntry;
  I, J: SizeInt;
begin
  Result := False;
  if (Scene = nil) or (Drawing = nil) then Exit;
  if Drawing.ObjectsCount <> 0 then
  begin
    if Diagnostics <> nil then
      Diagnostics.Add('TikZ materialization requires an empty detached drawing');
    Exit;
  end;
  if (Context <> nil) and (Context.Scene <> Scene) then
  begin
    if Diagnostics <> nil then
      Diagnostics.Add('TikZ source context does not own the semantic scene');
    Exit;
  end;
  StagedObjects := TObjectList.Create(False);
  AssetEntries := TStringList.Create;
  try
    Drawing.PicScale := 1;
    Drawing.LineWidthBase := Scene.LineWidthBaseMM;
    Drawing.MiterLimit := Scene.MiterLimit;
    Drawing.DashSize := Scene.DashSizeMM;
    Drawing.DottedSize := Scene.DotSizeMM;
    Drawing.DefaultFontHeight := Scene.TextSizeMM;
    for I := 0 to Scene.ObjectCount - 1 do
    begin
      Obj := Scene.ObjectAt(I);
      if Obj.StrokeOf >= 0 then
      begin
        { The stroke TpX writes after a faded fill belongs to the fill's
          object; its statement stays bound to that object. }
        ApplyStrokeStyle(StagedObjects[Obj.StrokeOf] as TPrimitive2D, Obj,
          Drawing);
        StagedObjects.Add(nil);
        Continue;
      end;
      Native := BuildNativeObject(Obj, Drawing, Resolver, DocumentFileName,
        AssetEntries, Diagnostics);
      if Native = nil then Exit;
      StagedObjects.Add(Native);
      if (Native is TPrimitive2D) and
        not FadingMatchesBox(TPrimitive2D(Native), Obj, Scene) then
      begin
        AddDiagnostic(Diagnostics, Obj.SourceSpan,
          'The fading transform does not follow the gradient of its path');
        Exit;
      end;
    end;
    for I := 0 to AssetEntries.Count - 1 do
    begin
      Entry := TBitmapEntry(AssetEntries.Objects[I]);
      if Drawing.BitmapRegistry.IndexOf(AssetEntries[I]) >= 0 then
      begin
        { BuildNativeObject reuses registry entries before staging. }
        AssetEntries.Objects[I] := nil;
      end
      else
      begin
        Drawing.BitmapRegistry.AddObject(AssetEntries[I], Entry);
        AssetEntries.Objects[I] := nil;
      end;
    end;
    for I := 0 to StagedObjects.Count - 1 do
    begin
      if StagedObjects[I] = nil then
      begin
        { A stroke companion: Native is still the faded fill before it. }
        if Context <> nil then Context.BindNativeObject(I, Native.ID);
        Continue;
      end;
      Native := StagedObjects[I] as TObject2D;
      Drawing.AddObject(-1, Native);
      StagedObjects[I] := nil;
      if Context <> nil then Context.BindNativeObject(I, Native.ID);
    end;
    Result := True;
  finally
    for I := 0 to StagedObjects.Count - 1 do
      if StagedObjects[I] <> nil then StagedObjects[I].Free;
    for J := 0 to AssetEntries.Count - 1 do
      if AssetEntries.Objects[J] <> nil then AssetEntries.Objects[J].Free;
    AssetEntries.Free;
    StagedObjects.Free;
  end;
end;

const
  SolidBeginMarker = '% tpx-solid ';
  SolidEndMarker = '% tpx-solid-end ';

// Turns verified '% tpx-solid' blocks back into editable 3D objects. A block
// whose paths no longer match its digest stays as the plain paths it holds.
procedure RestoreTikZSolids(Drawing: TDrawing2D; Context: TTikZImportContext;
  const Source: RawByteString);
var
  Pos0, LineEnd, EndLine, EndLineEnd, BodyStart, BodyEnd, I, First, Last: SizeInt;
  Xml, Digest, Body: string;
  Lines: TStringList;
  Binding: TTikZSourceBinding;
  Loader: T_TpX_Loader;
  Solid: TSolid3D;
  Region: TTikZSolidRegion;
  Native: TGraphicObject;
  WidthTaken: Boolean;
  function FindFrom(const Needle: string; From: SizeInt): SizeInt;
  begin
    Result := PosEx(Needle, string(Source), From);
  end;
  function LineEndAt(From: SizeInt): SizeInt;
  begin
    Result := From;
    while (Result <= Length(Source)) and not (Source[Result] in [#10, #13]) do
      Inc(Result);
  end;
  function SkipBreak(From: SizeInt): SizeInt;
  begin
    Result := From;
    if (Result <= Length(Source)) and (Source[Result] = #13) then Inc(Result);
    if (Result <= Length(Source)) and (Source[Result] = #10) then Inc(Result);
  end;
begin
  Pos0 := FindFrom(SolidBeginMarker, 1);
  while Pos0 > 0 do
  begin
    LineEnd := LineEndAt(Pos0);
    Xml := Copy(string(Source), Pos0 + Length(SolidBeginMarker),
      LineEnd - Pos0 - Length(SolidBeginMarker));
    BodyStart := SkipBreak(LineEnd);
    EndLine := FindFrom(SolidEndMarker, BodyStart);
    if EndLine <= 0 then Break;
    EndLineEnd := LineEndAt(EndLine);
    Digest := Trim(Copy(string(Source), EndLine + Length(SolidEndMarker),
      EndLineEnd - EndLine - Length(SolidEndMarker)));
    BodyEnd := EndLine;
    Body := Copy(string(Source), BodyStart, BodyEnd - BodyStart);
    Lines := TStringList.Create;
    try
      Lines.Text := Body;
      for I := Lines.Count - 1 downto 0 do
        if Trim(Lines[I]) = '' then Lines.Delete(I);
      if SameText(Digest, SolidGeometryDigest(Lines)) then
      begin
        // Statements of the block, as 0-based source offsets.
        First := -1;
        Last := -1;
        for I := 0 to Context.BindingCount - 1 do
        begin
          Binding := Context.BindingAt(I);
          if (Binding.StatementSpan.StartByte >= BodyStart - 1) and
            (Binding.StatementSpan.EndByte <= BodyEnd - 1) then
          begin
            if First < 0 then First := I;
            Last := I;
          end;
        end;
        if First >= 0 then
        begin
          Loader := T_TpX_Loader.Create(Drawing);
          try
            Solid := Loader.SolidFromXml(Xml);
          finally
            Loader.Free;
          end;
          Native := Drawing.GetObject(Context.BindingAt(First).NativeObjectID);
          Drawing.AddObject(-1, Solid);
          if Native <> nil then Drawing.MoveObject(Solid.ID, Native.ID);
          WidthTaken := False;
          for I := First to Last do
          begin
            Native := Drawing.GetObject(Context.BindingAt(I).NativeObjectID);
            // Line width follows the base this import chose for all paths.
            if not WidthTaken and (Native is TPrimitive2D) and
              (TPrimitive2D(Native).LineStyle <> liNone) then
            begin
              Solid.LineWidth := TPrimitive2D(Native).LineWidth;
              Solid.UpdateExtension(Solid);
              WidthTaken := True;
            end;
            if Native <> nil then Drawing.DeleteObject(Native.ID);
          end;
          Region.Span.StartByte := Pos0 - 1;
          Region.Span.EndByte := EndLineEnd - 1;
          Region.NativeObjectID := Solid.ID;
          Region.FirstBinding := First;
          Region.LastBinding := Last;
          Context.AddSolidRegion(Region);
        end;
      end;
    finally
      Lines.Free;
    end;
    Pos0 := FindFrom(SolidBeginMarker, EndLineEnd);
  end;
end;

function TikZDocumentLoader(Candidate: TDrawing2D; const FileName: TDocumentPath;
  const SourceBytes: RawByteString; Diagnostics: TStrings;
  out CodecContext: TObject): Boolean;
var
  Source: TBytes;
  Syntax: TTikZSyntaxResult;
  Semantic: TTikZSemanticResult;
  Scene: TTikZScene;
  Context: TTikZImportContext;
  ParseOptions: TTikZParseOptions;
  I: SizeInt;
begin
  Result := False;
  CodecContext := nil;
  Syntax := nil;
  Semantic := nil;
  Scene := nil;
  Context := nil;
  SetLength(Source, Length(SourceBytes));
  for I := 0 to High(Source) do Source[I] := Byte(SourceBytes[I + 1]);
  try
    if SameText(ExtractFileExt(FileName), '.tex') then
      ParseOptions := DefaultTikZParseOptions(tikTexInput)
    else
      ParseOptions := DefaultTikZParseOptions(tikFragmentInput);
    Syntax := ParseTikZ(Source, ParseOptions);
    Semantic := EvaluateTikZ(Syntax, DefaultTikZImportOptions);
    if Semantic.Outcome <> tsoAccepted then
    begin
      if Diagnostics <> nil then
        for I := 0 to High(Semantic.Diagnostics) do
          AddDiagnostic(Diagnostics, Semantic.Diagnostics[I].Span,
            Semantic.Diagnostics[I].Message);
      raise EReadError.Create(ImportFailureText(Diagnostics,
        'TikZ source is invalid, unsupported, or cancelled'));
    end;
    Scene := Semantic.DetachScene;
    Context := TTikZImportContext.Create(Syntax, Scene);
    Syntax := nil;
    Scene := nil;
    if not MaterializeTikZScene(Context.Scene, Candidate, Context,
      @ResolveTikZLocalAsset, FileName, Diagnostics) then
      raise EReadError.Create(ImportFailureText(Diagnostics,
        'TikZ scene could not be materialized as native drawing objects'));
    RestoreTikZSolids(Candidate, Context, SourceBytes);
    CodecContext := Context;
    Context := nil;
    Result := True;
  finally
    Context.Free;
    Scene.Free;
    Semantic.Free;
    Syntax.Free;
  end;
end;

procedure RegisterTikZDocumentLoader;
var
  Format: TDocumentFormat;
begin
  Format.Id := 'tikz';
  Format.DisplayName := 'TikZ source';
  Format.Extensions := '.tikz;.tex';
  Format.CanOpen := True;
  Format.CanSaveBack := False;
  Format.RoundTripProfile :=
    'Editable supported semantics; local PNG, JPEG or BMP basename images only';
  Format.RuntimeRequirements := '';
  RegisterDocumentCodec(Format, @TikZDocumentLoader, nil);
end;

initialization
  RegisterTikZDocumentLoader;

end.

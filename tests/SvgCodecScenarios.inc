procedure CheckSvgNear(const Actual, Expected, Tolerance: Double;
  const Description: string);
begin
  Check(Abs(Actual - Expected) <= Tolerance,
    Description + ': got ' + FloatToStr(Actual) + ', expected ' +
    FloatToStr(Expected));
end;

function SvgAttribute(const Svg, Name: string): string;
var
  StartPos, EndPos: SizeInt;
  Marker: string;
begin
  Marker := Name + '="';
  StartPos := Pos(Marker, Svg);
  Check(StartPos > 0, 'serialized SVG is missing ' + Name + ' attribute');
  Inc(StartPos, Length(Marker));
  EndPos := StartPos;
  while (EndPos <= Length(Svg)) and (Svg[EndPos] <> '"') do Inc(EndPos);
  Check(EndPos <= Length(Svg), 'serialized SVG has unterminated ' + Name);
  Result := Copy(Svg, StartPos, EndPos - StartPos);
end;

function SvgFloat(const Value: string): Double;
var
  Settings: TFormatSettings;
  NumberText: string;
begin
  Settings := DefaultFormatSettings;
  Settings.DecimalSeparator := '.';
  NumberText := Trim(Value);
  if (Length(NumberText) >= 2) and
    (Copy(NumberText, Length(NumberText) - 1, 2) = 'mm') then
    Delete(NumberText, Length(NumberText) - 1, 2);
  Result := StrToFloat(NumberText, Settings);
end;

procedure FindSvgObjects(const List: TGraphicObjList; var Line: TLine2D;
  var Path: TBezierPath2D0; var Text: TText2D);
var
  Obj: TGraphicObject;
begin
  Obj := List.FirstObj;
  while Obj <> nil do
  begin
    if (Line = nil) and (Obj is TLine2D) then Line := TLine2D(Obj)
    else if (Path = nil) and (Obj is TBezierPath2D0) then
      Path := TBezierPath2D0(Obj)
    else if (Text = nil) and (Obj is TText2D) then Text := TText2D(Obj);
    if Obj is TGroup2D then
      FindSvgObjects(TGroup2D(Obj).Objects, Line, Path, Text);
    Obj := List.NextObj;
  end;
end;

procedure FindSvgLineAtX(const List: TGraphicObjList; const X: TRealType;
  var Line: TLine2D);
var
  Obj: TGraphicObject;
begin
  Obj := List.FirstObj;
  while Obj <> nil do
  begin
    if (Line = nil) and (Obj is TLine2D) and
      (Abs(TLine2D(Obj).Points[0].X - X) < 1E-4) then
      Line := TLine2D(Obj);
    if Obj is TGroup2D then
      FindSvgLineAtX(TGroup2D(Obj).Objects, X, Line);
    Obj := List.NextObj;
  end;
end;

function HasSvgPoint(const List: TGraphicObjList; const X, Y: TRealType): Boolean;
var
  Obj: TGraphicObject;
  I: Integer;
begin
  Obj := List.FirstObj;
  while Obj <> nil do
  begin
    if (Obj is TPrimitive2D) and
      (TPrimitive2D(Obj).Points <> nil) then
    begin
      for I := 0 to TPrimitive2D(Obj).Points.Count - 1 do
        if (Abs(TPrimitive2D(Obj).Points[I].X - X) < 1E-4) and
          (Abs(TPrimitive2D(Obj).Points[I].Y - Y) < 1E-4) then Exit(True);
    end;
    if (Obj is TGroup2D) and
      HasSvgPoint(TGroup2D(Obj).Objects, X, Y) then Exit(True);
    Obj := List.NextObj;
  end;
  Result := False;
end;

procedure FindSvgFilledPrimitive(const List: TGraphicObjList;
  const HtmlColor: string; var Primitive: TPrimitive2D);
var
  Obj: TGraphicObject;
begin
  Obj := List.FirstObj;
  while Obj <> nil do
  begin
    if (Primitive = nil) and (Obj is TPrimitive2D) and
      (ColorToHtml(TPrimitive2D(Obj).FillColor) = HtmlColor) then
      Primitive := TPrimitive2D(Obj);
    if Obj is TGroup2D then
      FindSvgFilledPrimitive(TGroup2D(Obj).Objects, HtmlColor, Primitive);
    Obj := List.NextObj;
  end;
end;

procedure FindSvgPrimitiveAtPoint(const List: TGraphicObjList;
  const X, Y: TRealType; var Primitive: TPrimitive2D);
var
  Obj: TGraphicObject;
  I: Integer;
begin
  Obj := List.FirstObj;
  while Obj <> nil do
  begin
    if (Primitive = nil) and (Obj is TPrimitive2D) and
      (TPrimitive2D(Obj).Points <> nil) then
      for I := 0 to TPrimitive2D(Obj).Points.Count - 1 do
        if (Abs(TPrimitive2D(Obj).Points[I].X - X) < 1E-4) and
          (Abs(TPrimitive2D(Obj).Points[I].Y - Y) < 1E-4) then
        begin
          Primitive := TPrimitive2D(Obj);
          Break;
        end;
    if (Primitive = nil) and (Obj is TGroup2D) then
      FindSvgPrimitiveAtPoint(TGroup2D(Obj).Objects, X, Y, Primitive);
    if Primitive <> nil then Exit;
    Obj := List.NextObj;
  end;
end;

procedure TestSvgCodec;
const
  Source: RawByteString =
    '<svg xmlns="http://www.w3.org/2000/svg" width="25.4mm" ' +
    'height="12.7mm" viewBox="0 0 96 48">' +
    '<line transform="translate(5 6) scale(2)" x1="1" y1="2" ' +
    'x2="3" y2="4" stroke="#123456" stroke-width="2" fill="none"/>' +
    '<path d="M 8 4 C 10 3 12 2 14 1" fill="none" ' +
    'stroke="#abcdef" stroke-width="0.5"/>' +
    '<line transform="matrix(0 1 -1 0 20 0)" x1="0" y1="0" ' +
    'x2="1" y2="0" stroke="#445566" stroke-width="1"/>' +
    '<rect x="30" y="10" width="4" height="6" ' +
    'transform="rotate(90 32 13)" fill="#ddeeff" stroke="#663399"/>' +
    '<text x="20" y="30" font-family="Arial" font-size="12px" ' +
    'text-anchor="middle" xml:space="preserve">A &amp; B</text></svg>';
  Unsupported: RawByteString =
    '<svg xmlns="http://www.w3.org/2000/svg" width="10mm" ' +
    'height="10mm" viewBox="0 0 10 10"><line x1="1" y1="1" ' +
    'x2="9" y2="9" stroke="black"/><image x="2" y="2" ' +
    'width="4" height="4" href="file:///tmp/should-not-be-read.png"/>' +
    '</svg>';
  UnknownPrefixAttribute: RawByteString =
    '<svg xmlns="http://www.w3.org/2000/svg" width="10mm" ' +
    'height="10mm" viewBox="0 0 10 10"><line x1="1" y1="1" ' +
    'x2="9" y2="9" font="Arial" stroke="black"/></svg>';
  StylesheetInstruction: RawByteString =
    '<?xml-stylesheet type="text/css" href="file:///tmp/remote.css"?>' +
    '<svg xmlns="http://www.w3.org/2000/svg" width="10mm" ' +
    'height="10mm" viewBox="0 0 10 10"><line x1="1" y1="1" ' +
    'x2="9" y2="9" stroke="black"/></svg>';
  StyledAnchor: RawByteString =
    '<svg xmlns="http://www.w3.org/2000/svg" width="10mm" ' +
    'height="10mm" viewBox="0 0 10 10"><text x="2" y="3" ' +
    'font-family="Arial" font-size="2" ' +
    'style="text-anchor:middle">center</text></svg>';
  InvalidStyledAnchor: RawByteString =
    '<svg xmlns="http://www.w3.org/2000/svg" width="10mm" ' +
    'height="10mm" viewBox="0 0 10 10"><text x="2" y="3" ' +
    'font-family="Arial" font-size="2" ' +
    'style="text-anchor:middle-ish">center</text></svg>';
  TruncatedGzip: array[0..9] of Byte =
    ($1F, $8B, $08, $00, $00, $00, $00, $00, $00, $03);
var
  Drawing, Reloaded: TDrawing2D;
  Line, ReloadedLine, MatrixLine: TLine2D;
  Path, ReloadedPath: TBezierPath2D0;
  Text, ReloadedText: TText2D;
  Filled, ReloadedStroke, ReloadedFilled: TPrimitive2D;
  Diagnostics: TStringList;
  CanSaveBack, ReloadedCanSaveBack, Failed: Boolean;
  CodecContext, ReloadedContext: TObject;
  Serialized, Deep: RawByteString;
  I, Cycle: Integer;
  Profile: TSvgDocumentProfile;
  Detail, TempName: string;
  BeforeCount: Integer;
  BadInput: TMemoryStream;
  TempStream: TFileStream;
  ExistingLine: TLine2D;
  AuxDrawing: TDrawing2D;
  AuxCanSave: Boolean;
  AuxContext: TObject;

  procedure CheckRootUnits(const Width, Height: string);
  var
    UnitSource: RawByteString;
    UnitSerialized: RawByteString;
    UnitLine: TLine2D;
    UnitPath: TBezierPath2D0;
    UnitText: TText2D;
    UnitReloaded: TDrawing2D;
    UnitReloadedContext: TObject;
    UnitReloadedCanSave: Boolean;
  begin
    Diagnostics.Clear;
    UnitSource := '<svg xmlns="http://www.w3.org/2000/svg" width="' +
      Width + '" height="' + Height + '" viewBox="-20 -10 96 48">' +
      '<line x1="-15" y1="-5" x2="-10" y2="0" stroke="#000000" ' +
      'stroke-width="1"/></svg>';
    AuxDrawing := LoadDocumentCandidate('svg', 'units.svg', UnitSource,
      AuxCanSave, AuxContext, Diagnostics);
    UnitReloaded := nil;
    UnitReloadedContext := nil;
    try
      Check(AuxCanSave, Width + ' root size was not editable: ' +
        Diagnostics.Text);
      CheckSvgNear(AuxDrawing.PicScale, 25.4 / 96, 1E-5,
        Width + ' root unit to millimetre conversion');
      UnitLine := nil;
      UnitPath := nil;
      UnitText := nil;
      FindSvgObjects(AuxDrawing.ObjectList, UnitLine, UnitPath, UnitText);
      Check(UnitLine <> nil, Width + ' independent line was not imported');
      CheckSvgNear(UnitLine.Points[0].X, -15, 1E-4,
        Width + ' negative x coordinate');
      CheckSvgNear(UnitLine.Points[0].Y, 43, 1E-4,
        Width + ' negative viewBox/y-up conversion');
      UnitSerialized := SerializeDocumentToBytes('svg', AuxDrawing,
        'units-roundtrip.svg', AuxContext);
      Check(SvgAttribute(string(UnitSerialized), 'viewBox') = '-20 -10 96 48',
        Width + ' same-format save changed a nonzero canvas origin');
      UnitReloaded := LoadDocumentCandidate('svg', 'units-roundtrip.svg',
        UnitSerialized, UnitReloadedCanSave, UnitReloadedContext, Diagnostics);
      Check(UnitReloadedCanSave, Width + ' serialized SVG became import-only');
      Check(HasSvgPoint(UnitReloaded.ObjectList, -15, 43),
        Width + ' round trip changed negative-coordinate geometry');
    finally
      FreeAndNil(UnitReloadedContext);
      FreeAndNil(UnitReloaded);
      FreeAndNil(AuxContext);
      FreeAndNil(AuxDrawing);
    end;
  end;

  procedure CheckDashRoundTrip;
  const
    ScaledDashSource: RawByteString =
      '<svg xmlns="http://www.w3.org/2000/svg" width="50.8mm" ' +
      'height="25.4mm" viewBox="0 0 96 48">' +
      '<line transform="scale(2)" x1="1" y1="2" x2="3" y2="4" ' +
      'stroke="black" stroke-width="0.5" ' +
      'style="stroke-dasharray:4px 2px"/>' +
      '<line transform="matrix(0 2 -2 0 20 0)" x1="10" y1="2" ' +
      'x2="12" y2="4" stroke="black" ' +
      'stroke-width="0.5" stroke-dasharray="2 1"/></svg>';
    MixedDashSource: RawByteString =
      '<svg xmlns="http://www.w3.org/2000/svg" width="25.4mm" ' +
      'height="12.7mm" viewBox="0 0 96 48">' +
      '<line x1="1" y1="1" x2="2" y2="2" stroke="black" ' +
      'stroke-dasharray="4 2"/>' +
      '<line x1="3" y1="1" x2="4" y2="2" stroke="black" ' +
      'stroke-dasharray="8 4"/></svg>';
    DottedDashSource: RawByteString =
      '<svg xmlns="http://www.w3.org/2000/svg" width="25.4mm" ' +
      'height="12.7mm" viewBox="0 0 96 48">' +
      '<line x1="1" y1="1" x2="2" y2="2" stroke="black" ' +
      'stroke-dasharray="1 1"/></svg>';
  var
    DashDrawing, DashReloaded: TDrawing2D;
    DashContext, DashReloadedContext: TObject;
    DashCanSave, DashReloadedCanSave: Boolean;
    DashText: string;
    DashParts: TStringList;
    ExpectedOffMM, ExpectedOnMM, OutputScale: Double;
  begin
    DashDrawing := nil;
    DashReloaded := nil;
    DashContext := nil;
    DashReloadedContext := nil;
    DashParts := TStringList.Create;
    try
      Diagnostics.Clear;
      DashDrawing := LoadDocumentCandidate('svg', 'scaled-dash.svg',
        ScaledDashSource, DashCanSave, DashContext, Diagnostics);
      Check(DashCanSave, 'consistent 2:1 dash profile was import-only: ' +
        Diagnostics.Text);
      Check(DashDrawing.ObjectsCount = 2,
        'supported dashed lines were not both imported');
      ExpectedOffMM := 4 * 25.4 / 96;
      ExpectedOnMM := 8 * 25.4 / 96;
      CheckSvgNear(DashDrawing.DashSize, ExpectedOffMM, 1E-5,
        'dash off length after px-to-mm conversion and scale transform');
      CheckSvgNear(DashDrawing.DashSize * 2, ExpectedOnMM, 1E-5,
        'dash on length after px-to-mm conversion and scale transform');
      Check(DashDrawing.PicScale > 0,
        'scaled dash source lost its physical viewBox scale');
      Serialized := SerializeDocumentToBytes('svg', DashDrawing,
        'scaled-dash-roundtrip.svg', DashContext);
      DashText := SvgAttribute(string(Serialized), 'stroke-dasharray');
      DashParts.Clear;
      ExtractStrings([',', ' ', #9, #10, #13], [], PChar(DashText), DashParts);
      Check(DashParts.Count = 2,
        'saved SVG did not retain a two-entry dash pattern');
      OutputScale := DashDrawing.PicScale;
      CheckSvgNear(SvgFloat(DashParts[0]) * OutputScale, ExpectedOnMM,
        1E-5, 'serialized dash on length in physical millimetres');
      CheckSvgNear(SvgFloat(DashParts[1]) * OutputScale, ExpectedOffMM,
        1E-5, 'serialized dash off length in physical millimetres');
      DashReloaded := LoadDocumentCandidate('svg', 'scaled-dash-roundtrip.svg',
        Serialized, DashReloadedCanSave, DashReloadedContext, Diagnostics);
      Check(DashReloadedCanSave,
        'serialized consistent dash pattern became import-only: ' +
        Diagnostics.Text);
      CheckSvgNear(DashReloaded.DashSize, ExpectedOffMM, 1E-5,
        'dash size changed after save and reload');
      FreeAndNil(DashReloadedContext);
      FreeAndNil(DashReloaded);
      FreeAndNil(DashContext);
      FreeAndNil(DashDrawing);

      Diagnostics.Clear;
      DashDrawing := LoadDocumentCandidate('svg', 'mixed-dash.svg',
        MixedDashSource, DashCanSave, DashContext, Diagnostics);
      Check(not DashCanSave,
        'different 2:1 dash lengths were marked save-back capable');
      Check(Pos('vary after transforms', Diagnostics.Text) > 0,
        'mixed dash lengths were not diagnosed');
      Check(DashDrawing.ObjectsCount = 2,
        'mixed dash profile prevented import of line geometry');
      FreeAndNil(DashContext);
      FreeAndNil(DashDrawing);

      Diagnostics.Clear;
      DashDrawing := LoadDocumentCandidate('svg', 'dotted-dash.svg',
        DottedDashSource, DashCanSave, DashContext, Diagnostics);
      Check(not DashCanSave,
        'dotted/custom dash pattern was marked save-back capable');
      Check(Pos('two-to-one', Diagnostics.Text) > 0,
        'dotted/custom dash limitation was not diagnosed');
      Check(DashDrawing.ObjectsCount = 1,
        'dotted import-only pattern prevented line geometry import');
    finally
      DashParts.Free;
      FreeAndNil(DashReloadedContext);
      FreeAndNil(DashReloaded);
      FreeAndNil(DashContext);
      FreeAndNil(DashDrawing);
    end;
  end;

  procedure CheckSourceSaveRejectsBitmap;
  var
    Mode: TTpXMode;
    SourcePath, TempName, ErrorText: string;
    SourceFile: TFileStream;
    BytesOnDisk: RawByteString;
    AcceptedBefore: TDiskRevision;
    LocalRevisionBefore: QWord;
    CanSaveBefore: Boolean;
  begin
    TempName := GetTempFileName(GetTempDir, 'tpx-svg-save-');
    DeleteFile(TempName);
    SourcePath := ChangeFileExt(TempName, '.svg');
    try
      SourceFile := TFileStream.Create(SourcePath, fmCreate);
      try
        SourceFile.WriteBuffer(Source[1], Length(Source));
      finally
        SourceFile.Free;
      end;
      Mode := TTpXMode(MainForm.EventManager.Mode);
      ErrorText := '';
      Check(Mode.OpenDocumentFromPath(TDocumentPath(SourcePath), ErrorText),
        'application open route rejected the verified SVG: ' + ErrorText);
      Check(Mode.Drawing.ObjectsCount = 5,
        'application open route did not commit the expected SVG geometry');
      Check(Mode.TpX_Manager.DocumentSession.SourceFormatId = 'svg',
        'application open route did not retain SVG source format');
      Check(Mode.TpX_Manager.DocumentSession.CanSaveBack,
        'verified SVG source was not eligible for same-format save');
      Check(Mode.TpX_Manager.DocumentSession.OriginalSourceBytes = Source,
        'application open route did not retain original SVG source bytes');
      AcceptedBefore := Mode.TpX_Manager.DocumentSession.AcceptedRevision;
      CanSaveBefore := Mode.TpX_Manager.DocumentSession.CanSaveBack;
      Mode.Drawing.AddObject(-1, TBitmap2D.Create(-1));
      Check(Mode.Drawing.History.IsChanged,
        'adding an unsupported live bitmap did not dirty the SVG document');
      LocalRevisionBefore := Mode.TpX_Manager.DocumentSession.LocalRevision;
      ErrorText := '';
      Check(not Mode.SaveDocumentToPath(TDocumentPath(SourcePath), 'svg',
        False, False, ErrorText),
        'same-source save accepted an unsupported live bitmap');
      Check(Pos('bitmap', LowerCase(ErrorText)) > 0,
        'rejected source save did not explain its unsupported bitmap');
      SourceFile := TFileStream.Create(SourcePath, fmOpenRead or fmShareDenyNone);
      try
        SetLength(BytesOnDisk, SourceFile.Size);
        if SourceFile.Size > 0 then SourceFile.ReadBuffer(BytesOnDisk[1],
          SourceFile.Size);
      finally
        SourceFile.Free;
      end;
      Check(BytesOnDisk = Source,
        'failed same-source save changed the original SVG bytes');
      Check(SameDiskRevision(ReadDocumentRevision(TDocumentPath(SourcePath)),
        AcceptedBefore), 'failed same-source save changed disk revision');
      Check(SameDiskRevision(Mode.TpX_Manager.DocumentSession.AcceptedRevision,
        AcceptedBefore), 'failed same-source save changed accepted revision');
      Check(Mode.TpX_Manager.DocumentSession.OriginalSourceBytes = Source,
        'failed same-source save replaced the preserved source snapshot');
      Check(Mode.TpX_Manager.DocumentSession.LocalRevision = LocalRevisionBefore,
        'failed same-source save changed the local scene revision');
      Check(Mode.Drawing.History.IsChanged,
        'failed same-source save cleared the dirty drawing state');
      Check(Mode.TpX_Manager.DocumentSession.CanSaveBack = CanSaveBefore,
        'failed same-source save changed source save-back eligibility');
    finally
      DeleteFile(SourcePath);
      DeleteFile(ChangeFileExt(SourcePath, '.tpx-recovery'));
    end;
  end;

  procedure CheckReferenceExpansionBounded;
  var
    UseSource: RawByteString;
    UseDrawing: TDrawing2D;
    UseContext: TObject;
    UseCanSave: Boolean;
    I: Integer;
  begin
    UseSource := '<svg xmlns="http://www.w3.org/2000/svg" ' +
      'xmlns:xlink="http://www.w3.org/1999/xlink" width="25.4mm" ' +
      'height="12.7mm" viewBox="0 0 96 48"><defs>' +
      '<line id="template" x1="1" y1="1" x2="2" y2="2" ' +
      'stroke="black"/></defs>' +
      '<line x1="3" y1="1" x2="4" y2="2" stroke="black"/>';
    for I := 1 to 1000 do
      UseSource := UseSource + '<use xlink:href="#template"/>';
    UseSource := UseSource + '</svg>';
    UseDrawing := nil;
    UseContext := nil;
    try
      Diagnostics.Clear;
      UseDrawing := LoadDocumentCandidate('svg', 'references.svg', UseSource,
        UseCanSave, UseContext, Diagnostics);
      Check(not UseCanSave,
        'SVG use references were marked save-back capable');
      Check(UseDrawing.ObjectsCount = 1,
        'unsupported use references expanded into duplicated model objects');
      Check(Diagnostics.Count > 0,
        'unsupported use references had no import-only diagnostic');
    finally
      FreeAndNil(UseContext);
      FreeAndNil(UseDrawing);
    end;
  end;

  procedure CheckSvgzImportOnly;
  const
    SvgzScene: RawByteString =
      '<svg xmlns="http://www.w3.org/2000/svg" width="25.4mm" ' +
      'height="12.7mm" viewBox="0 0 96 48"><line x1="1" y1="2" ' +
      'x2="3" y2="4" stroke="#123456" stroke-width="1"/></svg>';
  var
    GzipName: string;
    Gzip: gzFile;
    GzipBytes: RawByteString;
    GzipStream: TFileStream;
    GzipDrawing: TDrawing2D;
    GzipCanSave: Boolean;
    GzipContext: TObject;
    CloseResult: Integer;
  begin
    GzipName := GetTempFileName(GetTempDir, 'tpx-svgz-');
    Gzip := nil;
    GzipDrawing := nil;
    GzipContext := nil;
    try
      Gzip := gzopen(GzipName, 'wb');
      Check(Gzip <> nil, 'could not create a valid SVGZ fixture');
      try
        Check(gzwrite(Gzip, @SvgzScene[1], Length(SvgzScene)) =
          Length(SvgzScene), 'could not write SVGZ fixture bytes');
      finally
        CloseResult := gzclose(Gzip);
        Gzip := nil;
      end;
      Check(CloseResult = 0, 'could not finish SVGZ fixture');
      GzipStream := TFileStream.Create(GzipName, fmOpenRead);
      try
        SetLength(GzipBytes, GzipStream.Size);
        if GzipStream.Size > 0 then
          GzipStream.ReadBuffer(GzipBytes[1], GzipStream.Size);
      finally
        GzipStream.Free;
      end;
      Diagnostics.Clear;
      GzipDrawing := LoadDocumentCandidate('svgz', 'valid-source.svgz', GzipBytes,
        GzipCanSave, GzipContext, Diagnostics);
      Check(not GzipCanSave, 'valid SVGZ was marked save-back capable');
      Check((GzipDrawing <> nil) and (GzipDrawing.ObjectsCount = 1),
        'valid SVGZ did not import its line geometry');
      Check(Pos('SVGZ is import-only', Diagnostics.Text) > 0,
        'valid SVGZ import-only diagnostic was missing');
    finally
      if Gzip <> nil then gzclose(Gzip);
      FreeAndNil(GzipContext);
      FreeAndNil(GzipDrawing);
      DeleteFile(GzipName);
    end;
  end;
begin
  Check(IsDocumentCodecRegistered('svg'), 'SVG codec was not registered');
  Check(IsDocumentCodecRegistered('svgz'), 'SVGZ import codec was not registered');
  Diagnostics := TStringList.Create;
  Drawing := nil;
  Reloaded := nil;
  AuxDrawing := nil;
  AuxContext := nil;
  CodecContext := nil;
  ReloadedContext := nil;
  BadInput := nil;
  TempStream := nil;
  TempName := '';
  try
    Drawing := LoadDocumentCandidate('svg', 'independent.svg', Source,
      CanSaveBack, CodecContext, Diagnostics);
    Check(CanSaveBack, 'representable SVG was classified import-only: ' +
      Diagnostics.Text);
    Check(Drawing.ObjectsCount = 5,
      'independent SVG did not produce its five expected objects');
    CheckSvgNear(Drawing.PicScale, 25.4 / 96, 1E-5,
      '96 dpi physical unit scale');
    Line := nil;
    Path := nil;
    Text := nil;
    FindSvgObjects(Drawing.ObjectList, Line, Path, Text);
    Check((Line <> nil) and (Path <> nil) and (Text <> nil),
      'independent SVG geometry types were not imported');
    CheckSvgNear(Line.Points[0].X, 7, 1E-4,
      'translate/scale order on line start x');
    CheckSvgNear(Line.Points[0].Y, 38, 1E-4,
      'SVG y-down to TpX y-up on line start');
    CheckSvgNear(Line.Points[1].X, 11, 1E-4,
      'translate/scale order on line end x');
    CheckSvgNear(Line.Points[1].Y, 34, 1E-4,
      'SVG y-down to TpX y-up on line end');
    CheckSvgNear(Line.LineWidth * Drawing.PicScale, 4 * 25.4 / 96,
      1E-4, 'transformed stroke width in millimetres');
    Check(ColorToHtml(Line.LineColor) = '#123456',
      'line stroke color changed');
    MatrixLine := nil;
    FindSvgLineAtX(Drawing.ObjectList, 20, MatrixLine);
    Check(MatrixLine <> nil, 'orthogonal SVG matrix transform was lost');
    CheckSvgNear(MatrixLine.Points[0].Y, 48, 1E-4,
      'matrix transform and SVG y-up conversion');
    CheckSvgNear(MatrixLine.Points[1].Y, 47, 1E-4,
      'matrix transform second endpoint');
    Filled := nil;
    FindSvgFilledPrimitive(Drawing.ObjectList, '#DDEEFF', Filled);
    Check(Filled <> nil, 'SVG filled geometry lost its fill color');
    Check(Path.Points.Count = 4,
      'cubic path did not retain its four Bezier points');
    CheckSvgNear(Path.Points[0].X, 8, 1E-4, 'cubic start x');
    CheckSvgNear(Path.Points[0].Y, 44, 1E-4, 'cubic start y');
    CheckSvgNear(Path.Points[1].X, 10, 1E-4, 'first cubic control x');
    CheckSvgNear(Path.Points[1].Y, 45, 1E-4, 'first cubic control y');
    CheckSvgNear(Path.Points[3].X, 14, 1E-4, 'cubic end x');
    CheckSvgNear(Path.Points[3].Y, 47, 1E-4, 'cubic end y');
    Check(ColorToHtml(Path.LineColor) = '#ABCDEF',
      'cubic path stroke color changed');
    Check((Text.Text = 'A & B') and (Text.HAlignment = ahCenter),
      'SVG text content or text-anchor changed');
    Check(Text.Font.Name = 'Arial', 'SVG text font family changed');
    CheckSvgNear(Text.Points[0].X, 20, 1E-4, 'text x');
    CheckSvgNear(Text.Points[0].Y, 18, 1E-4, 'text baseline y');
    CheckSvgNear(Text.Height * Drawing.PicScale, 12 * 25.4 / 96,
      1E-4, 'CSS pixel font size in millimetres');

    Serialized := SerializeDocumentToBytes('svg', Drawing,
      'roundtrip.svg', CodecContext);
    CheckSvgNear(SvgFloat(SvgAttribute(string(Serialized), 'width')),
      25.4, 1E-4, 'same-format physical page width');
    CheckSvgNear(SvgFloat(SvgAttribute(string(Serialized), 'height')),
      12.7, 1E-4, 'same-format physical page height');
    Check(SvgAttribute(string(Serialized), 'viewBox') = '0 0 96 48',
      'same-format save changed the authored canvas bounds');
    Reloaded := LoadDocumentCandidate('svg', 'roundtrip.svg', Serialized,
      ReloadedCanSaveBack, ReloadedContext, Diagnostics);
    Check(ReloadedCanSaveBack,
      'TpX SVG output failed the editable profile: ' + Diagnostics.Text);
    Line := nil;
    Path := nil;
    Text := nil;
    ReloadedLine := nil;
    ReloadedPath := nil;
    ReloadedText := nil;
    FindSvgObjects(Drawing.ObjectList, Line, Path, Text);
    FindSvgObjects(Reloaded.ObjectList, ReloadedLine, ReloadedPath,
      ReloadedText);
    Check(ReloadedText <> nil, 'SVG save/reload lost supported text');
    Check(HasSvgPoint(Reloaded.ObjectList, Line.Points[0].X,
      Line.Points[0].Y), 'same-format save changed line coordinates');
    Check(HasSvgPoint(Reloaded.ObjectList, Path.Points[1].X,
      Path.Points[1].Y), 'same-format save changed Bezier control coordinates');
    ReloadedStroke := nil;
    FindSvgPrimitiveAtPoint(Reloaded.ObjectList, Line.Points[0].X,
      Line.Points[0].Y, ReloadedStroke);
    Check(ReloadedStroke <> nil, 'same-format save lost line styling');
    Check(ColorToHtml(ReloadedStroke.LineColor) = '#123456',
      'same-format save changed stroke color');
    CheckSvgNear(ReloadedStroke.LineWidth * Reloaded.PicScale,
      Line.LineWidth * Drawing.PicScale, 1E-4,
      'same-format stroke width in millimetres');
    ReloadedFilled := nil;
    FindSvgFilledPrimitive(Reloaded.ObjectList, '#DDEEFF', ReloadedFilled);
    Check(ReloadedFilled <> nil, 'same-format save changed fill color');
    CheckSvgNear(ReloadedText.Height * Reloaded.PicScale,
      Text.Height * Drawing.PicScale, 1E-4,
      'same-format text size round trip in millimetres');
    Check((ReloadedText.Text = Text.Text) and
      (ReloadedText.HAlignment = Text.HAlignment),
      'same-format text content or alignment round trip changed');

    for Cycle := 1 to 2 do
    begin
      Serialized := SerializeDocumentToBytes('svg', Reloaded,
        'roundtrip-cycle.svg', ReloadedContext);
      Check(SvgAttribute(string(Serialized), 'viewBox') = '0 0 96 48',
        'repeated save changed the source canvas bounds');
      FreeAndNil(ReloadedContext);
      FreeAndNil(Reloaded);
      Reloaded := LoadDocumentCandidate('svg', 'roundtrip-cycle.svg',
        Serialized, ReloadedCanSaveBack, ReloadedContext, Diagnostics);
      Check(ReloadedCanSaveBack,
        'repeated SVG round trip changed the editable profile');
      Check(HasSvgPoint(Reloaded.ObjectList, Line.Points[0].X,
        Line.Points[0].Y), 'repeated SVG round trip changed line coordinates');
      Check(HasSvgPoint(Reloaded.ObjectList, Path.Points[1].X,
        Path.Points[1].Y), 'repeated SVG round trip changed Bezier geometry');
    end;

    CheckRootUnits('2.54cm', '1.27cm');
    CheckRootUnits('1in', '0.5in');
    CheckRootUnits('72pt', '36pt');
    CheckRootUnits('96px', '48px');
    CheckDashRoundTrip;
    CheckReferenceExpansionBounded;
    CheckSvgzImportOnly;

    FreeAndNil(ReloadedContext);
    FreeAndNil(Reloaded);
    Drawing.Free;
    Drawing := nil;
    Drawing := LoadDocumentCandidate('svg', 'unsupported.svg', Unsupported,
      CanSaveBack, CodecContext, Diagnostics);
    Check(not CanSaveBack,
      'SVG with an external image reference was marked save-back capable');
    Check(Pos('image', LowerCase(Diagnostics.Text)) > 0,
      'unsupported image reference was not diagnosed');
    Check(Drawing.ObjectsCount = 1,
      'unsupported external image produced an unexpected imported object');

    Diagnostics.Clear;
    AuxDrawing := LoadDocumentCandidate('svg', 'unknown-attribute.svg',
      UnknownPrefixAttribute, AuxCanSave, AuxContext, Diagnostics);
    try
      Check(not AuxCanSave,
        'unknown font attribute was accepted by a prefix name match');
      Check(Pos('font', LowerCase(Diagnostics.Text)) > 0,
        'unknown font attribute was not diagnosed');
      Check(AuxDrawing.ObjectsCount = 1,
        'import-only unknown attribute lost representable line geometry');
    finally
      FreeAndNil(AuxContext);
      FreeAndNil(AuxDrawing);
    end;

    Diagnostics.Clear;
    AuxDrawing := LoadDocumentCandidate('svg', 'stylesheet-pi.svg',
      StylesheetInstruction, AuxCanSave, AuxContext, Diagnostics);
    try
      Check(not AuxCanSave,
        'XML stylesheet processing instruction was marked editable');
      Check(Pos('processing instruction', LowerCase(Diagnostics.Text)) > 0,
        'XML stylesheet processing instruction was not diagnosed');
      Check(AuxDrawing.ObjectsCount = 1,
        'stylesheet processing instruction prevented visual SVG import');
    finally
      FreeAndNil(AuxContext);
      FreeAndNil(AuxDrawing);
    end;

    Diagnostics.Clear;
    AuxDrawing := LoadDocumentCandidate('svg', 'style-anchor.svg',
      StyledAnchor, AuxCanSave, AuxContext, Diagnostics);
    try
      Text := nil;
      FindSvgObjects(AuxDrawing.ObjectList, Line, Path, Text);
      Check(AuxCanSave and (Text <> nil) and
        (Text.HAlignment = ahCenter),
        'valid inline text-anchor was not preserved: ' + Diagnostics.Text);
    finally
      FreeAndNil(AuxContext);
      FreeAndNil(AuxDrawing);
    end;

    Diagnostics.Clear;
    AuxDrawing := LoadDocumentCandidate('svg', 'bad-style-anchor.svg',
      InvalidStyledAnchor, AuxCanSave, AuxContext, Diagnostics);
    try
      Check(not AuxCanSave,
        'unsupported inline text-anchor was marked editable');
      Check(Pos('text-anchor', LowerCase(Diagnostics.Text)) > 0,
        'unsupported inline text-anchor was not diagnosed');
    finally
      FreeAndNil(AuxContext);
      FreeAndNil(AuxDrawing);
    end;

    CheckSourceSaveRejectsBitmap;

    ExistingLine := TLine2D.CreateSpec(-1, Point2D(3, 4), Point2D(5, 6));
    Drawing.AddObject(-1, ExistingLine);
    BeforeCount := Drawing.ObjectsCount;
    BadInput := TMemoryStream.Create;
    Deep := '<!DOCTYPE svg [<!ENTITY xxe SYSTEM "file:///etc/passwd">]>' +
      '<svg xmlns="http://www.w3.org/2000/svg" width="10mm" ' +
      'height="10mm" viewBox="0 0 10 10"><text>&xxe;</text></svg>';
    BadInput.WriteBuffer(Deep[1], Length(Deep));
    BadInput.Position := 0;
    Failed := False;
    try
      LoadSvgFromStream(BadInput, Drawing, False, Profile, Detail);
    except
      on E: ESvgCodec do Failed := True;
    end;
    Check(Failed, 'DOCTYPE/entity-bearing SVG was not rejected');
    Check(Drawing.ObjectsCount = BeforeCount,
      'rejected SVG changed the caller drawing before profile validation');
    CheckSvgNear(ExistingLine.Points[0].X, 3,
      1E-4, 'live candidate line after rejected SVG');

    BadInput.Clear;
    Deep := '<svg xmlns="http://www.w3.org/2000/svg" width="10mm" ' +
      'height="10mm" viewBox="0 0 10 10">';
    for I := 1 to 65 do Deep := Deep + '<g>';
    Deep := Deep + '<line x1="0" y1="0" x2="1" y2="1"/>';
    for I := 1 to 65 do Deep := Deep + '</g>';
    Deep := Deep + '</svg>';
    BadInput.WriteBuffer(Deep[1], Length(Deep));
    BadInput.Position := 0;
    Failed := False;
    try
      LoadSvgFromStream(BadInput, Drawing, False, Profile, Detail);
    except
      on E: ESvgCodec do Failed := True;
    end;
    Check(Failed, 'over-nested SVG was not rejected');
    Check(Drawing.ObjectsCount = BeforeCount,
      'deeply nested SVG changed the caller drawing');

    BadInput.Clear;
    Deep := '<svg xmlns="http://www.w3.org/2000/svg" width="10mm" ' +
      'height="10mm" viewBox="0 0 10 10"><path d="M 0 0 C 1 1"/>' +
      '</svg>';
    BadInput.WriteBuffer(Deep[1], Length(Deep));
    BadInput.Position := 0;
    Failed := False;
    try
      LoadSvgFromStream(BadInput, Drawing, False, Profile, Detail);
    except
      on E: ESvgCodec do Failed := True;
    end;
    Check(Failed, 'incomplete SVG path data was not rejected');
    Check(Drawing.ObjectsCount = BeforeCount,
      'malformed path changed the caller drawing');

    BadInput.Clear;
    Deep := '<svg xmlns="http://www.w3.org/2000/svg" width="10mm" ' +
      'height="10mm" viewBox="0 0 10 10"><text>' + #$FF +
      '</text></svg>';
    BadInput.WriteBuffer(Deep[1], Length(Deep));
    BadInput.Position := 0;
    Failed := False;
    try
      LoadSvgFromStream(BadInput, Drawing, False, Profile, Detail);
    except
      on E: ESvgCodec do Failed := True;
    end;
    Check(Failed, 'invalid UTF-8 SVG text was not rejected');
    Check(Drawing.ObjectsCount = BeforeCount,
      'invalid UTF-8 changed the caller drawing');

    TempName := GetTempFileName(GetTempDir, 'tpx-svgz-');
    TempStream := TFileStream.Create(TempName, fmCreate);
    try
      TempStream.WriteBuffer(TruncatedGzip, SizeOf(TruncatedGzip));
    finally
      FreeAndNil(TempStream);
    end;
    Failed := False;
    try
      LoadSvgzFile(TempName, Drawing, Detail);
    except
      on E: ESvgCodec do Failed := True;
    end;
    Check(Failed, 'truncated SVGZ data was not rejected');
    Check(Drawing.ObjectsCount = BeforeCount,
      'truncated SVGZ changed the caller drawing');

    AuxDrawing := TDrawing2D.Create(nil);
    try
      AuxDrawing.AddObject(-1,
        TLine2D.CreateSpec(-1, Point2D(1, 2), Point2D(3, 4)));
      AuxDrawing.AddObject(-1, TBitmap2D.Create(-1));
      Failed := False;
      try
        SerializeDocumentToBytes('svg', AuxDrawing, 'unsupported-live.svg');
      except
        on E: ESvgCodec do Failed := True;
      end;
      Check(Failed,
        'SVG saver silently dropped a live bitmap without its linked asset');
      Check(AuxDrawing.ObjectsCount = 2,
        'rejected SVG save changed the live drawing');
    finally
      FreeAndNil(AuxDrawing);
    end;
  finally
    TempStream.Free;
    if TempName <> '' then DeleteFile(TempName);
    BadInput.Free;
    FreeAndNil(ReloadedContext);
    Reloaded.Free;
    FreeAndNil(CodecContext);
    Drawing.Free;
    Diagnostics.Free;
  end;
end;

function ReadStagedFixture(const FileName: string): RawByteString;
var
  Stream: TFileStream;
begin
  Result := '';
  if not FileExists(FileName) then Exit;
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, Stream.Size);
    if Stream.Size > 0 then
      Stream.ReadBuffer(Result[1], Stream.Size);
  finally
    Stream.Free;
  end;
end;

procedure WriteStagedFixture(const FileName: string;
  const Contents: RawByteString);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    if Length(Contents) > 0 then
      Stream.WriteBuffer(Contents[1], Length(Contents));
  finally
    Stream.Free;
  end;
end;

procedure TestTpXStagedSidecars;
var
  Drawing: TDrawing2D;
  Destination: TMemoryStream;
  StagedAssets, FinalAssets: TStringList;
  Root, StageDir, FinalDir, FinalName, StageName: string;
  FinalEPS, FinalPDF, StageEPS, StagePDF, Marker: string;
  Serialized, KeepBytes: RawByteString;
  BeforeName: string;
  BeforeCount: Integer;
  PicScaleBefore, BorderBefore, LineWidthBefore, HatchingBefore: TRealType;
  Failed: Boolean;
  MarkerLines: TStringList;
  OldMetaPostPath, OldGhostscriptPath: string;
begin
  Root := IncludeTrailingPathDelimiter(SysUtils.GetTempDir(False))
    + 'tpx-staged-sidecars-' + IntToStr(GetProcessID);
  StageDir := IncludeTrailingPathDelimiter(Root) + 'stage';
  FinalDir := IncludeTrailingPathDelimiter(Root) + 'final';
  Check(ForceDirectories(StageDir) and ForceDirectories(FinalDir),
    'Could not create staged-save test directories');
  FinalName := IncludeTrailingPathDelimiter(FinalDir) + 'drawing.tpx';
  StageName := IncludeTrailingPathDelimiter(StageDir) + 'drawing.tpx';
  FinalEPS := ChangeFileExt(FinalName, '.eps');
  FinalPDF := ChangeFileExt(FinalName, '.pdf');
  StageEPS := ChangeFileExt(StageName, '.eps');
  StagePDF := ChangeFileExt(StageName, '.pdf');
  Marker := GetEnvironmentVariable('TPX_STAGED_TOOL_MARKER');
  Check((Marker <> '') and FileExists(GetEnvironmentVariable(
    'TPX_STAGED_META_CONVERTER')) and FileExists(GetEnvironmentVariable(
    'TPX_STAGED_GS_CONVERTER')),
    'Staged-save converter fixtures are not configured');

  WriteStagedFixture(FinalName, 'existing source');
  WriteStagedFixture(FinalEPS, 'existing eps');
  WriteStagedFixture(FinalPDF, 'existing pdf');
  Drawing := MainForm.TheDrawing;
  Drawing.Clear;
  Drawing.TeXFormat := tex_eps;
  Drawing.PdfTeXFormat := pdftex_pdf;
  Drawing.TeXFigure := fig_none;
  Drawing.TeXCenterFigure := False;
  Drawing.FileName := FinalName;
  Drawing.PicMagnif := 1.5;
  Drawing.PicScale := 2;
  Drawing.Border := 3;
  Drawing.LineWidthBase := 0.4;
  Drawing.HatchingStep := 2.5;
  Drawing.AddObject(-1, TLine2D.CreateSpec(-1,
    Point2D(1, 2), Point2D(31, 22)));
  BeforeName := Drawing.FileName;
  BeforeCount := Drawing.ObjectsCount;
  PicScaleBefore := Drawing.PicScale;
  BorderBefore := Drawing.Border;
  LineWidthBefore := Drawing.LineWidthBase;
  HatchingBefore := Drawing.HatchingStep;
  Destination := TMemoryStream.Create;
  StagedAssets := TStringList.Create;
  FinalAssets := TStringList.Create;
  MarkerLines := TStringList.Create;
  OldMetaPostPath := MetaPostPath;
  OldGhostscriptPath := GhostscriptPath;
  try
    Check(CanSafelySerializeTpXDocument(Drawing),
      'In-process EPS/PDF profile was not marked safe');
    Check(StoreToStream_TpXDocumentStaged(Drawing, Destination, FinalName,
      StageName, False, StagedAssets, FinalAssets),
      'Staged TpX writer produced no document');
    Check((StagedAssets.Count = 2) and (FinalAssets.Count = 2),
      'The EPS/PDF profile did not report both generated sidecars');
    Check((StagedAssets[0] = ExpandFileName(StageEPS)) and
      (FinalAssets[0] = ExpandFileName(FinalEPS)) and
      (StagedAssets[1] = ExpandFileName(StagePDF)) and
      (FinalAssets[1] = ExpandFileName(FinalPDF)),
      'Staged and final sidecar mappings changed');
    Check(Pos('%!PS-Adobe', string(ReadStagedFixture(StageEPS))) = 1,
      'The EPS profile did not generate an EPS sidecar');
    Check(Pos('%PDF-', string(ReadStagedFixture(StagePDF))) > 0,
      'The PDF profile did not generate a PDF sidecar');
    Check((ReadStagedFixture(FinalName) = 'existing source') and
      (ReadStagedFixture(FinalEPS) = 'existing eps') and
      (ReadStagedFixture(FinalPDF) = 'existing pdf'),
      'Serialization published over the final source or sidecars');
    SetLength(Serialized, Destination.Size);
    Destination.Position := 0;
    Destination.ReadBuffer(Serialized[1], Destination.Size);
    Check((Pos('%<TpX', string(Serialized)) > 0) and
      (Pos('\includegraphics{drawing}', string(Serialized)) > 0) and
      (Pos('\includegraphics{drawing.pdf}', string(Serialized)) > 0),
      'Generated TeX did not preserve the final basename and chosen formats');
    Check((Drawing.FileName = BeforeName) and
      (Drawing.ObjectsCount = BeforeCount) and
      (Drawing.TeXFormat = tex_eps) and
      (Drawing.PdfTeXFormat = pdftex_pdf) and
      (Drawing.PicScale = PicScaleBefore) and
      (Drawing.Border = BorderBefore) and
      (Drawing.LineWidthBase = LineWidthBefore) and
      (Drawing.HatchingStep = HatchingBefore),
      'Staged serialization changed document identity or model state');

    WriteStagedFixture(Marker, '');
    Destination.Size := 0;
    KeepBytes := 'keep';
    Destination.WriteBuffer(KeepBytes[1], Length(KeepBytes));
    Drawing.TeXFormat := tex_metapost;
    Drawing.PdfTeXFormat := pdftex_none;
    Check(not CanSafelySerializeTpXDocument(Drawing),
      'MetaPost profile was marked safe for background serialization');
    Failed := False;
    try
      StoreToStream_TpXDocumentStaged(Drawing, Destination, FinalName,
        StageName, False, StagedAssets, FinalAssets);
    except
      on E: EWriteError do Failed := True;
    end;
    Check(Failed, 'MetaPost profile did not require external-tool permission');
    Check(ReadStagedFixture(Marker) = '',
      'A denied MetaPost save executed its converter');
    Check((Destination.Size = Length(KeepBytes)) and
      (StagedAssets.Count = 0) and (FinalAssets.Count = 0),
      'Denied MetaPost save changed output or asset lists');

    MetaPostPath := GetEnvironmentVariable('TPX_STAGED_META_CONVERTER');
    Drawing.PicScale := 0;
    WriteStagedFixture(FinalName, 'existing source');
    WriteStagedFixture(ChangeFileExt(FinalName, '.mps'), 'existing mps');
    Failed := False;
    try
      StoreToStream_TpXDocumentStaged(Drawing, Destination, FinalName,
        StageName, False, StagedAssets, FinalAssets, True);
    except
      on E: Exception do Failed := True;
    end;
    Check(Failed, 'Failed MetaPost conversion reported a successful save');
    Check((ReadStagedFixture(Marker) <> '') and
      (ReadStagedFixture(FinalName) = 'existing source') and
      (ReadStagedFixture(ChangeFileExt(FinalName, '.mps')) = 'existing mps') and
      not FileExists(ChangeFileExt(StageName, '.mps')),
      'Failed MetaPost save changed a published source or sidecar');
    SetLength(Serialized, Destination.Size);
    Destination.Position := 0;
    Destination.ReadBuffer(Serialized[1], Destination.Size);
    Check(string(Serialized) = string(KeepBytes),
      'Failed MetaPost save changed its destination stream');
    Check(Drawing.FileName = BeforeName,
      'Failed MetaPost save changed the document filename');
    Check(Drawing.ObjectsCount = BeforeCount,
      'Failed MetaPost save changed the model object count');
    Check(Drawing.PicScale = 0,
      'Failed MetaPost save did not restore picture scale: ' +
      FloatToStr(Drawing.PicScale));
    Check(Drawing.Border = BorderBefore,
      'Failed MetaPost save did not restore border');
    Check(Drawing.LineWidthBase = LineWidthBefore,
      'Failed MetaPost save did not restore line width');
    Check(Drawing.HatchingStep = HatchingBefore,
      'Failed MetaPost save did not restore hatching step');
    MarkerLines.LoadFromFile(Marker);
    Check((MarkerLines.Count = 1) and
      (Pos('mpost-fixture', MarkerLines[0]) > 0),
      'The MetaPost failure fixture was not invoked exactly once');

    Drawing.TeXFormat := tex_eps;
    Drawing.PdfTeXFormat := pdftex_epstopdf;
    Check(not CanSafelySerializeTpXDocument(Drawing),
      'EPS-to-PDF profile was marked safe for background serialization');
    Failed := False;
    try
      StoreToStream_TpXDocumentStaged(Drawing, Destination, FinalName,
        StageName, False, StagedAssets, FinalAssets);
    except
      on E: EWriteError do Failed := True;
    end;
    MarkerLines.LoadFromFile(Marker);
    Check(Failed and (MarkerLines.Count = 1) and
      (Pos('mpost-fixture', MarkerLines[0]) > 0),
      'Denied EPS-to-PDF save did not fail before running Ghostscript');
    GhostscriptPath := GetEnvironmentVariable('TPX_STAGED_GS_CONVERTER');
    WriteStagedFixture(FinalName, 'existing source');
    WriteStagedFixture(FinalPDF, 'existing pdf');
    Failed := False;
    try
      StoreToStream_TpXDocumentStaged(Drawing, Destination, FinalName,
        StageName, False, StagedAssets, FinalAssets, True);
    except
      on E: Exception do Failed := True;
    end;
    Check(Failed, 'Failed EPS-to-PDF conversion reported a successful save');
    Check((ReadStagedFixture(Marker) <> '') and
      (ReadStagedFixture(FinalName) = 'existing source') and
      (ReadStagedFixture(FinalPDF) = 'existing pdf') and
      not FileExists(StagePDF),
      'Failed EPS-to-PDF save changed a published source or sidecar');
    MarkerLines.LoadFromFile(Marker);
    Check((MarkerLines.Count = 2) and
      (Pos('mpost-fixture', MarkerLines[0]) > 0) and
      (Pos('gs-fixture', MarkerLines[1]) > 0),
      'The Ghostscript failure fixture was not invoked exactly once');
  finally
    MetaPostPath := OldMetaPostPath;
    GhostscriptPath := OldGhostscriptPath;
    MarkerLines.Free;
    FinalAssets.Free;
    StagedAssets.Free;
    Destination.Free;
    DeleteFile(StageEPS);
    DeleteFile(StagePDF);
    DeleteFile(ChangeFileExt(StageName, '.mps'));
    DeleteFile(FinalName);
    DeleteFile(FinalEPS);
    DeleteFile(FinalPDF);
    DeleteFile(ChangeFileExt(FinalName, '.mps'));
    RemoveDir(StageDir);
    RemoveDir(FinalDir);
    RemoveDir(Root);
  end;
end;

procedure RunStagedToolFixture;
var
  Lines: TStringList;
  Marker, Name: string;
begin
  Marker := GetEnvironmentVariable('TPX_STAGED_TOOL_MARKER');
  Name := ExtractFileName(ParamStr(0));
  Lines := TStringList.Create;
  try
    if FileExists(Marker) then Lines.LoadFromFile(Marker);
    Lines.Add(Name);
    Lines.SaveToFile(Marker);
  finally
    Lines.Free;
  end;
  Halt(7);
end;

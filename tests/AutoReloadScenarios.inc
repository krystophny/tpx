procedure WriteReloadSource(const FileName: string; const Bytes: RawByteString);
var Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    if Length(Bytes) > 0 then Stream.WriteBuffer(Bytes[1], Length(Bytes));
  finally
    Stream.Free;
  end;
end;

function ReloadLineX2: Double; forward;

type
  TTrackedReloadContext = class
  public
    class var Destroyed: Boolean;
    destructor Destroy; override;
  end;

  TModalReloadDriver = class
  public
    Timer: TTimer;
    FileName: string;
    Bytes: RawByteString;
    ExpectedOldX2: Double;
    destructor Destroy; override;
    procedure ChangeSourceAndClose(Sender: TObject);
  end;

destructor TModalReloadDriver.Destroy;
begin
  Timer.Free;
  inherited Destroy;
end;

destructor TTrackedReloadContext.Destroy;
begin
  Destroyed := True;
  inherited Destroy;
end;

procedure TModalReloadDriver.ChangeSourceAndClose(Sender: TObject);
var Deadline: QWord;
begin
  Timer.Enabled := False;
  WriteReloadSource(FileName, Bytes);
  Deadline := GetTickCount64 + 6000;
  repeat
    Application.ProcessMessages;
    MainForm.ProcessPendingReload(GetTickCount64 + 1000);
    if MainForm.AutoReloadState.SettlePending then
    begin
      Check(Abs(ReloadLineX2 - ExpectedOldX2) < 0.0001,
        'External reload replaced scene objects while Properties was modal');
      PropertiesForm.ModalResult := mrCancel;
      Exit;
    end;
    Sleep(10);
  until GetTickCount64 >= Deadline;
  PropertiesForm.ModalResult := mrCancel;
  Check(False, 'Native file event did not reach the modal Properties barrier');
end;

procedure TestModalReloadBarrier(const FileName: string;
  const SourceBytes: RawByteString; OldX2: Double);
var Driver: TModalReloadDriver;
begin
  Driver := TModalReloadDriver.Create;
  try
    Driver.FileName := FileName;
    Driver.Bytes := SourceBytes;
    Driver.ExpectedOldX2 := OldX2;
    Driver.Timer := TTimer.Create(PropertiesForm);
    Driver.Timer.Interval := 100;
    Driver.Timer.OnTimer := Driver.ChangeSourceAndClose;
    Driver.Timer.Enabled := True;
    PropertiesForm.PObject := MainForm.TheDrawing.GetObject(0) as TObject2D;
    PropertiesForm.ModalResult := mrNone;
    PropertiesForm.ShowModal;
  finally
    Driver.Free;
  end;
end;

function FindMenuCaption(Root: TMenuItem; const Caption: string): TMenuItem;
var I: Integer;
begin
  if SameText(Root.Caption, Caption) then Exit(Root);
  for I := 0 to Root.Count - 1 do
  begin
    Result := FindMenuCaption(Root.Items[I], Caption);
    if Result <> nil then Exit;
  end;
  Result := nil;
end;

function ReloadLineX2: Double;
var Line: TLine2D;
begin
  Check(MainForm.TheDrawing.ObjectsCount > 0,
    'Expected the external source line to be present');
  Line := MainForm.TheDrawing.GetObject(0) as TLine2D;
  Result := Line.Points[1].X;
end;

procedure LoadReloadFixture(const FileName: string);
var ErrorText: string;
begin
  Check(BaseMode.OpenDocumentFromPath(FileName, ErrorText),
    'Production open route failed for reload fixture: ' + ErrorText);
  Check(not MainForm.AutoReloadState.WatchUnavailable,
    'Production open route did not bind a native source watch');
end;

procedure DriveReloadUntil(const TargetX: Double; WantInvalid,
  WantConflict: Boolean);
var
  Deadline: QWord;
  StateMatches: Boolean;
begin
  Deadline := GetTickCount64 + 6000;
  repeat
    Application.ProcessMessages;
    MainForm.ProcessPendingReload(GetTickCount64 + 1000);
    StateMatches := (MainForm.AutoReloadState.InvalidExternal = WantInvalid) and
      (MainForm.AutoReloadState.Conflict = WantConflict);
    if (Abs(ReloadLineX2 - TargetX) < 0.0001) and StateMatches then
    begin
      Exit;
    end;
    Sleep(10);
  until GetTickCount64 >= Deadline;
  Check(False, Format('Native external reload did not reach x2=%.4f; got %.4f, invalid=%s, conflict=%s',
    [TargetX, ReloadLineX2, BoolToStr(MainForm.AutoReloadState.InvalidExternal, True),
     BoolToStr(MainForm.AutoReloadState.Conflict, True)]));
end;

procedure DriveReloadRevisionUntil(const Revision: TDiskRevision);
var Deadline: QWord;
begin
  Deadline := GetTickCount64 + 6000;
  repeat
    Application.ProcessMessages;
    MainForm.ProcessPendingReload(GetTickCount64 + 1000);
    if SameDiskRevision(Revision,
      MainForm.EventManager.DocumentSession.AcceptedRevision) then Exit;
    Sleep(10);
  until GetTickCount64 >= Deadline;
  Check(False, 'Same-content replacement was not accepted as a new disk revision');
end;

procedure TestAutoReloadIntegration;
const
  InitialSource: RawByteString =
    '%<TpX v="5" TeXFormat="tikz" PdfTeXFormat="tikz">' + LineEnding +
    '%<line x1="0" y1="0" x2="20" y2="10"/>' + LineEnding +
    '%</TpX>' + LineEnding;
  StableSizeSource: RawByteString =
    '%<TpX v="5" TeXFormat="tikz" PdfTeXFormat="tikz">' + LineEnding +
    '%<line x1="0" y1="0" x2="30" y2="10"/>' + LineEnding +
    '%</TpX>' + LineEnding;
  InvalidSource: RawByteString = '%<TpX v="5" TeXFormat="tikz" PdfTeXFormat="tikz">' + LineEnding + '%<line';
  SameShapeSource: RawByteString =
    '%<TpX v="5" TeXFormat="tikz" PdfTeXFormat="tikz">' + LineEnding +
    '%<line x1="0" y1="0" x2="40" y2="10"/>' + LineEnding +
    '%</TpX>' + LineEnding;
  ExplicitSource: RawByteString =
    '%<TpX v="5" TeXFormat="tikz" PdfTeXFormat="tikz">' + LineEnding +
    '%<line x1="0" y1="0" x2="45" y2="10"/>' + LineEnding +
    '%</TpX>' + LineEnding;
  ModalSource: RawByteString =
    '%<TpX v="5" TeXFormat="tikz" PdfTeXFormat="tikz">' + LineEnding +
    '%<line x1="0" y1="0" x2="48" y2="10"/>' + LineEnding +
    '%</TpX>' + LineEnding;
  ConflictSource: RawByteString =
    '%<TpX v="5" TeXFormat="tikz" PdfTeXFormat="tikz">' + LineEnding +
    '%<line x1="0" y1="0" x2="50" y2="10"/>' + LineEnding +
    '%</TpX>' + LineEnding;
var
  FileName: string;
  SavedAge, SavedHistoryCount: LongInt;
  InitialRevision, SameStatRevision: TDiskRevision;
  TeXRevision: TDiskRevision;
  TeXDrawing: TDrawing2D;
  TeXObject: TText2D;
  TeXSourceBytes: RawByteString;
  SavedLatexPath: string;
  SavedTeXCompilationCount: Integer;
  ContextRevision: TDiskRevision;
  TrackedContext: TTrackedReloadContext;
  ContextBackupPath: string;
  ReplacementName: string;
  SavedView: TRect2D;
  SavedFocus: Boolean;
  SavedActiveControl: TWinControl;
  SavedGeneration: QWord;
  Deadline: QWord;
  SavedSourcePath: string;
  SaveAsName, OccupiedName, ErrorText: string;
  SaveSucceeded: Boolean;
  OriginalFileName: string;
  Line: TLine2D;
  ReloadMenuItem: TMenuItem;
begin
  FileName := IncludeTrailingPathDelimiter(GetTempDir) +
    'tpx-auto-reload-' + IntToStr(GetProcessID) + '.tpx';
  WriteReloadSource(FileName, InitialSource);
  MainForm.Show;
  MainForm.SetFocus;
  Application.ProcessMessages;
  AutoRefreshEnabled := True;
  MainForm.ApplyAutoReloadSettings;
  MainForm.ClearDocumentWatchBinding;
  MainForm.EventManager.DocumentSession.Clear;
  LoadReloadFixture(FileName);
  Check(not LiveTeXDocumentTrusted,
    'Opening an imported native source left TeX preview trusted');
  Check(not MainForm.TheDrawing.History.IsChanged,
    'Initial fixture did not establish a clean scene');
  Check(Abs(ReloadLineX2 - 20) < 0.0001, 'Initial line geometry was not loaded');

  { A native source refresh can introduce opaque TeX. It must remain inert
    until the user explicitly trusts this document. }
  TeXDrawing := TDrawing2D.CreateDetached;
  try
    TeXDrawing.FileName := FileName;
    TeXDrawing.AddObject(-1, TLine2D.CreateSpec(-1,
      Point2D(0, 0), Point2D(20, 10)));
    TeXDrawing.AddObject(-1, TText2D.CreateSpec(-1,
      Point2D(5, 20), 8, '\input{tpx-untrusted-preview}'));
    TeXSourceBytes := SerializeDocumentToBytes('tpx', TeXDrawing, FileName);
  finally
    TeXDrawing.Free;
  end;
  SavedLatexPath := LatexPath;
  LatexPath := FileName + '.missing-latex';
  SavedTeXCompilationCount := LiveTeXCompilationCount;
  WriteReloadSource(FileName, TeXSourceBytes);
  TeXRevision := ReadDocumentRevision(FileName);
  DriveReloadRevisionUntil(TeXRevision);
  Check(MainForm.TheDrawing.ObjectsCount = 2,
    'Native external TeX label was not installed in the scene');
  TeXObject := MainForm.TheDrawing.GetObject(1) as TText2D;
  Check(Pos('\input', TeXObject.TeXText) = 1,
    'Native external TeX input label did not survive parsing');
  Check(not LiveTeXDocumentTrusted,
    'External source refresh implicitly trusted TeX content');
  Deadline := GetTickCount64 + 600;
  repeat
    Application.ProcessMessages;
    Sleep(10);
  until GetTickCount64 >= Deadline;
  Check(not LiveTeXPending and
    (LiveTeXCompilationCount = SavedTeXCompilationCount),
    'Untrusted native TeX label queued or ran a preview compilation');
  LatexPath := SavedLatexPath;
  WriteReloadSource(FileName, InitialSource);
  TeXRevision := ReadDocumentRevision(FileName);
  DriveReloadRevisionUntil(TeXRevision);

  MainForm.LocalView.VisualRect := Rect2D(-40, -20, 60, 40);
  SavedView := MainForm.LocalView.VisualRect;
  MainForm.SetFocus;
  MainForm.ActiveControl := MainForm.LocalView;
  MainForm.LocalView.SetFocus;
  Application.ProcessMessages;
  SavedActiveControl := MainForm.ActiveControl;
  Check(SavedActiveControl = MainForm.LocalView,
    'Could not activate the drawing view before reload');
  {$IFNDEF LCLcocoa}
  SavedFocus := MainForm.LocalView.Focused;
  Check(SavedFocus, 'Could not focus the drawing view before reload');
  {$ENDIF}
  Line := MainForm.TheDrawing.GetObject(0) as TLine2D;
  MainForm.TheDrawing.SelectionAdd(Line);
  SavedHistoryCount := MainForm.TheDrawing.History.Count;
  InitialRevision := ReadDocumentRevision(FileName);
  SavedAge := FileAge(FileName);
  WriteReloadSource(FileName, StableSizeSource);
  Check(FileSetDate(FileName, SavedAge) = 0,
    'Could not restore fixture timestamp for same-size/same-mtime update');
  Check(Length(StableSizeSource) = Length(InitialSource),
    'Same-size rewrite fixture changed length');
  SameStatRevision := ReadDocumentRevision(FileName);
  Check((SameStatRevision.Size = InitialRevision.Size) and
    (SameStatRevision.ModifiedUTC = InitialRevision.ModifiedUTC) and
    (SameStatRevision.ContentDigest <> InitialRevision.ContentDigest),
    'Fixture did not produce changed bytes with unchanged size and mtime');
  DriveReloadUntil(30, False, False);
  Check(MainForm.TheDrawing.History.Count = 1,
    'Accepted external revision retained the prior undo chain');
  Check(MainForm.TheDrawing.SelectedObjects.Count = 1,
    'Compatible selection was not preserved across reload');
  Check(MainForm.ActiveControl = SavedActiveControl,
    'External reload changed the active drawing view');
  {$IFNDEF LCLcocoa}
  Check(MainForm.LocalView.Focused = SavedFocus,
    'External reload did not preserve view focus');
  {$ENDIF}
  Check((Abs(MainForm.LocalView.VisualRect.Left - SavedView.Left) < 0.0001) and
    (Abs(MainForm.LocalView.VisualRect.Top - SavedView.Top) < 0.0001) and
    (Abs(MainForm.LocalView.VisualRect.Right - SavedView.Right) < 0.0001) and
    (Abs(MainForm.LocalView.VisualRect.Bottom - SavedView.Bottom) < 0.0001),
    'External reload changed the pan/zoom rectangle');

  OriginalFileName := FileName;
  SaveSucceeded := BaseMode.SaveDocumentToPath(FileName, 'tpx', False, False,
    ErrorText);
  Check(SaveSucceeded,
    'In-place save failed after external reload: ' + ErrorText);
  Check(SameDiskRevision(ReadDocumentRevision(FileName),
    MainForm.EventManager.DocumentSession.AcceptedRevision),
    'In-place save did not accept the writer’s exact disk revision');
  OccupiedName := IncludeTrailingPathDelimiter(GetTempDir) +
    'tpx-auto-reload-occupied-' + IntToStr(GetProcessID) + '.tpx';
  WriteReloadSource(OccupiedName, InitialSource);
  Check(not BaseMode.SaveDocumentToPath(OccupiedName, 'tpx', True, False,
    ErrorText), 'Save As unexpectedly overwrote an existing document');
  Check(MainForm.EventManager.DocumentSession.SourcePath = OriginalFileName,
    'Failed Save As changed the active document binding');
  SaveAsName := IncludeTrailingPathDelimiter(GetTempDir) +
    'tpx-auto-reload-save-as-' + IntToStr(GetProcessID) + '.tpx';
  DeleteFile(SaveAsName);
  Check(BaseMode.SaveDocumentToPath(SaveAsName, 'tpx', True, False,
    ErrorText), 'Save As failed: ' + ErrorText);
  Check(MainForm.EventManager.DocumentSession.SourcePath = SaveAsName,
    'Successful Save As did not rebind the active source path');

  { A same-content replacement must advance the accepted file identity without
    dropping a source codec context or its recovery backup path. }
  TTrackedReloadContext.Destroyed := False;
  TrackedContext := TTrackedReloadContext.Create;
  ContextBackupPath := SaveAsName + '.recovery-backup';
  MainForm.EventManager.DocumentSession.AcceptSavedRevision(
    MainForm.EventManager.DocumentSession.OriginalSourceBytes,
    MainForm.EventManager.DocumentSession.AcceptedRevision,
    TrackedContext, ContextBackupPath);
  ContextRevision := MainForm.EventManager.DocumentSession.AcceptedRevision;
  ReplacementName := SaveAsName + '.replacement';
  DeleteFile(ReplacementName);
  WriteReloadSource(ReplacementName,
    MainForm.EventManager.DocumentSession.OriginalSourceBytes);
  if not RenameFile(ReplacementName, SaveAsName) then
  begin
    Check(DeleteFile(SaveAsName), 'Could not replace the reload fixture');
    Check(RenameFile(ReplacementName, SaveAsName),
      'Could not publish the replacement reload fixture');
  end;
  ContextRevision := ReadDocumentRevision(SaveAsName);
  Check(not SameDiskRevision(ContextRevision,
    MainForm.EventManager.DocumentSession.AcceptedRevision),
    'Same-content replacement did not change the disk identity');
  Check(ContextRevision.ContentDigest =
    MainForm.EventManager.DocumentSession.AcceptedRevision.ContentDigest,
    'Replacement fixture did not preserve the document bytes');
  DriveReloadRevisionUntil(ContextRevision);
  Check(MainForm.EventManager.DocumentSession.CodecContext = TrackedContext,
    'Same-content revision acceptance discarded the source codec context');
  Check(MainForm.EventManager.DocumentSession.RecoveryBackupPath =
    ContextBackupPath,
    'Same-content revision acceptance discarded the recovery backup path');
  Check(not TTrackedReloadContext.Destroyed,
    'Same-content revision acceptance destroyed the active codec context');
  MainForm.EventManager.DocumentSession.AcceptSavedRevision(
    MainForm.EventManager.DocumentSession.OriginalSourceBytes,
    MainForm.EventManager.DocumentSession.AcceptedRevision, nil, '');
  Check(TTrackedReloadContext.Destroyed,
    'Releasing the test codec context did not destroy it exactly once');

  WriteReloadSource(OriginalFileName, StableSizeSource);
  WriteReloadSource(SaveAsName, SameShapeSource);
  FileName := SaveAsName;
  DriveReloadUntil(40, False, False);

  SavedHistoryCount := MainForm.TheDrawing.History.Count;
  SavedSourcePath := MainForm.EventManager.DocumentSession.SourcePath;
  WriteReloadSource(FileName, InvalidSource);
  DriveReloadUntil(40, True, False);
  Check(MainForm.TheDrawing.History.Count = SavedHistoryCount,
    'Invalid external rewrite changed local history');
  Check(MainForm.EventManager.DocumentSession.SourcePath = SavedSourcePath,
    'Invalid external rewrite changed source identity');

  WriteReloadSource(FileName, SameShapeSource);
  DriveReloadUntil(40, False, False);
  WriteReloadSource(FileName, ExplicitSource);
  ReloadMenuItem := FindMenuCaption(MainForm.MainMenu1.Items,
    'Reload from disk…');
  Check(ReloadMenuItem <> nil, 'Explicit reload action is missing');
  ReloadMenuItem.Click;
  Check(Abs(ReloadLineX2 - 45) < 0.0001,
    Format('Explicit Reload did not read a clean document without a watcher event; x2=%.1f, invalid=%s, pending=%s, paused=%s',
      [ReloadLineX2, BoolToStr(MainForm.AutoReloadState.InvalidExternal, True),
       BoolToStr(MainForm.AutoReloadState.SettlePending, True),
       BoolToStr(MainForm.AutoReloadState.Paused, True)]));
  TestModalReloadBarrier(FileName, ModalSource, 45);
  DriveReloadUntil(48, False, False);
  MainForm.TheDrawing.Clear;
  MainForm.TheDrawing.History.Save;
  MainForm.TheDrawing.History.MarkDirty;
  MainForm.NotifyDocumentEdited;
  Check((MainForm.TheDrawing.ObjectsCount = 0) and
    MainForm.TheDrawing.History.IsChanged,
    'Could not construct a dirty empty drawing');
  WriteReloadSource(FileName, ConflictSource);
  Deadline := GetTickCount64 + 6000;
  repeat
    Application.ProcessMessages;
    MainForm.ProcessPendingReload(GetTickCount64 + 1000);
    if MainForm.AutoReloadState.Conflict then Break;
    Sleep(10);
  until GetTickCount64 >= Deadline;
  Check(MainForm.AutoReloadState.Conflict and
    (MainForm.TheDrawing.ObjectsCount = 0) and
    MainForm.TheDrawing.History.IsChanged,
    'External write replaced a dirty empty drawing instead of opening a conflict');

  ReloadMenuItem := FindMenuCaption(MainForm.MainMenu1.Items,
    'Keep local edits');
  Check(ReloadMenuItem <> nil, 'Keep local conflict action is missing');
  ReloadMenuItem.Click;
  Check(MainForm.AutoReloadState.Conflict,
    'Keep local action cleared the unresolved conflict');
  ReloadMenuItem := FindMenuCaption(MainForm.MainMenu1.Items,
    'Reload from disk…');
  Check(ReloadMenuItem <> nil, 'Explicit reload action is missing');
  PromptCount := 0;
  ReloadMenuItem.Click;
  Check((PromptCount = 1) and not MainForm.AutoReloadState.Conflict and
    not MainForm.TheDrawing.History.IsChanged,
    'Confirmed explicit reload did not resolve the dirty conflict');
  Check(Abs(ReloadLineX2 - 50) < 0.0001,
    'Explicit reload did not install the latest disk geometry');

  SavedGeneration := MainForm.EventManager.DocumentSession.WatchGeneration;
  MainForm.BeginSceneInteraction;
  WriteReloadSource(FileName, StableSizeSource);
  Deadline := GetTickCount64 + 6000;
  repeat
    Application.ProcessMessages;
    MainForm.ProcessPendingReload(GetTickCount64 + 1000);
    if MainForm.AutoReloadState.SettlePending then Break;
    Sleep(10);
  until GetTickCount64 >= Deadline;
  Check(MainForm.AutoReloadState.SettlePending and
    (Abs(ReloadLineX2 - 50) < 0.0001),
    'Active scene interaction did not defer replacement');
  MainForm.EndSceneInteraction;
  DriveReloadUntil(30, False, False);
  Check(MainForm.EventManager.DocumentSession.WatchGeneration = SavedGeneration,
    'External reload unexpectedly replaced the active watch generation');

  MainForm.BeginSceneInteraction;
  WriteReloadSource(FileName, ConflictSource);
  Deadline := GetTickCount64 + 6000;
  repeat
    Application.ProcessMessages;
    MainForm.ProcessPendingReload(GetTickCount64 + 1000);
    if MainForm.AutoReloadState.SettlePending then Break;
    Sleep(10);
  until GetTickCount64 >= Deadline;
  Check(MainForm.AutoReloadState.SettlePending,
    'No native callback was pending before close cancellation test');
  MainForm.Free;
  MainForm := nil;
  DeleteFile(FileName);
  DeleteFile(OriginalFileName);
  DeleteFile(OccupiedName);
end;

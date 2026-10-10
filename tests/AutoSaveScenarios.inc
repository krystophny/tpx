function AutoSaveLineX2(Drawing: TDrawing2D): Double;
var Line: TLine2D;
begin
  Check((Drawing <> nil) and (Drawing.ObjectsCount > 0),
    'Expected a source line in the drawing');
  Line := Drawing.GetObject(0) as TLine2D;
  Result := Line.Points[1].X;
end;

function SavedSourceLineX2(const FileName: string): Double;
var
  Snapshot: TDocumentSnapshot;
  Candidate: TDrawing2D;
  CodecContext: TObject;
  CanSaveBack: Boolean;
  Diagnostics: TStringList;
begin
  Snapshot := ReadDocumentSnapshot(FileName);
  Diagnostics := TStringList.Create;
  CodecContext := nil;
  Candidate := nil;
  try
    Candidate := LoadDocumentCandidate('tpx', FileName,
      Snapshot.SourceBytes, CanSaveBack, CodecContext, Diagnostics);
    Check(Candidate <> nil,
      'The saved source did not parse: ' + Diagnostics.Text);
    Result := AutoSaveLineX2(Candidate);
  finally
    Candidate.Free;
    CodecContext.Free;
    Diagnostics.Free;
  end;
end;

function SavedSourcePicScale(const FileName: string): Double;
var
  Snapshot: TDocumentSnapshot;
  Candidate: TDrawing2D;
  CodecContext: TObject;
  CanSaveBack: Boolean;
  Diagnostics: TStringList;
begin
  Snapshot := ReadDocumentSnapshot(FileName);
  Diagnostics := TStringList.Create;
  CodecContext := nil;
  Candidate := nil;
  try
    Candidate := LoadDocumentCandidate('tpx', FileName,
      Snapshot.SourceBytes, CanSaveBack, CodecContext, Diagnostics);
    Check(Candidate <> nil,
      'The saved source did not parse: ' + Diagnostics.Text);
    Result := Candidate.PicScale;
  finally
    Candidate.Free;
    CodecContext.Free;
    Diagnostics.Free;
  end;
end;

type
  TPictureScaleDriver = class
  public
    Timer: TTimer;
    Changed: Boolean;
    destructor Destroy; override;
    procedure ChangeScaleAndClose(Sender: TObject);
  end;

destructor TPictureScaleDriver.Destroy;
begin
  Timer.Free;
  inherited Destroy;
end;

procedure TPictureScaleDriver.ChangeScaleAndClose(Sender: TObject);
var I: Integer; Button: TComponent;
begin
  if not OptionsForm.Visible then Exit;
  for I := 0 to OptionsForm.TreeView1.Items.Count - 1 do
    if Pos('PicScale=', OptionsForm.TreeView1.Items[I].Text) = 1 then
    begin
      OptionsForm.TreeView1.Items[I].Text := 'PicScale=2';
      Changed := True;
      Break;
    end;
  for I := 0 to OptionsForm.ComponentCount - 1 do
  begin
    Button := OptionsForm.Components[I];
    if (Button is TButton) and SameText(TButton(Button).Caption, 'OK') then
    begin
      Timer.Enabled := False;
      TButton(Button).Click;
      Exit;
    end;
  end;
end;

procedure WaitForAutoSaveClean(const FileName: string);
var Deadline: QWord;
begin
  Deadline := GetTickCount64 + 6000;
  repeat
    Application.ProcessMessages;
    if not MainForm.TheDrawing.History.IsChanged then
    begin
      Check(SameDiskRevision(ReadDocumentRevision(FileName),
        MainForm.EventManager.DocumentSession.AcceptedRevision),
        'AutoSave cleaned history before accepting the published revision');
      Exit;
    end;
    Sleep(10);
  until GetTickCount64 >= Deadline;
  Check(False, 'Completed edit was not AutoSaved within six seconds');
end;

procedure TestAutoSaveIntegration;
const
  InitialSource: RawByteString =
    '%<TpX v="5" TeXFormat="tikz" PdfTeXFormat="tikz">' + LineEnding +
    '%<line x1="0" y1="0" x2="20" y2="10"/>' + LineEnding +
    '%</TpX>' + LineEnding;
  ExternalSource: RawByteString =
    '%<TpX v="5" TeXFormat="tikz" PdfTeXFormat="tikz">' + LineEnding +
    '%<line x1="0" y1="0" x2="55" y2="10"/>' + LineEnding +
    '%</TpX>' + LineEnding;
var
  FileName, ErrorText: string;
  Deadline: QWord;
  Line: TLine2D;
  ExpectedDelta, ExpectedEndX, ActualStartX, ActualEndX, SavedEndX: Double;
  CanvasX, CanvasY: Integer;
  MappedStart, MappedEnd: TPoint2D;
  AutoSaveItem: TMenuItem;
  ScaleDriver: TPictureScaleDriver;
  procedure CanvasPosition(const World: TPoint2D; out X, Y: Integer;
    out MappedWorld: TPoint2D);
  var CanvasPoint: TPoint2D;
  begin
    CanvasPoint := MainForm.LocalView.ViewportToScreen(World);
    X := Round(CanvasPoint.X);
    Y := Round(CanvasPoint.Y);
    MappedWorld := MainForm.LocalView.ScreenToViewport(Point2D(X, Y));
  end;
  procedure MouseDownAt(const World: TPoint2D);
  var X, Y: Integer; MappedWorld: TPoint2D;
  begin
    CanvasPosition(World, X, Y, MappedWorld);
    MainForm.LocalViewMouseDown2D(MainForm.LocalView, mbLeft, [],
      MappedWorld.X, MappedWorld.Y, X, Y);
  end;
  procedure MouseMoveAt(const World: TPoint2D; Shift: TShiftState);
  var X, Y: Integer; MappedWorld: TPoint2D;
  begin
    CanvasPosition(World, X, Y, MappedWorld);
    MainForm.LocalViewMouseMove2D(MainForm.LocalView, Shift,
      MappedWorld.X, MappedWorld.Y, X, Y);
  end;
  procedure MouseUpAt(const World: TPoint2D);
  var X, Y: Integer; MappedWorld: TPoint2D;
  begin
    CanvasPosition(World, X, Y, MappedWorld);
    MainForm.LocalViewMouseUp2D(MainForm.LocalView, mbLeft, [],
      MappedWorld.X, MappedWorld.Y, X, Y);
  end;
begin
  FileName := IncludeTrailingPathDelimiter(GetTempDir) +
    'tpx-auto-save-' + IntToStr(GetProcessID) + '.tpx';
  WriteReloadSource(FileName, InitialSource);
  MainForm.Show;
  Application.ProcessMessages;
  AutoRefreshEnabled := True;
  MainForm.ApplyAutoReloadSettings;
  Check(BaseMode.OpenDocumentFromPath(FileName, ErrorText),
    'Production open route failed for AutoSave fixture: ' + ErrorText);
  AutoSaveItem := FindMenuCaption(MainForm.MainMenu1.Items,
    'AutoSave this document');
  Check(AutoSaveItem <> nil, 'Visible AutoSave switch is missing');
  Check(not MainForm.CurrentAutoSaveEnabled,
    'AutoSave was not off by default for a new document');
  MainForm.ApplyAutoSavePreference(True);
  Check(MainForm.CurrentAutoSaveEnabled,
    'The per-document AutoSave switch did not turn on (path=' +
      string(MainForm.EventManager.DocumentSession.SourcePath) + ', format=' +
      MainForm.EventManager.DocumentSession.SourceFormatId + ', saveback=' +
      BoolToStr(MainForm.EventManager.DocumentSession.CanSaveBack, True) + ')');

  MainForm.LocalView.VisualRect := Rect2D(0, 0,
    MainForm.LocalView.ClientWidth, MainForm.LocalView.ClientHeight);
  Line := MainForm.TheDrawing.GetObject(0) as TLine2D;
  MainForm.TheDrawing.SelectionClear;
  MainForm.TheDrawing.SelectionAdd(Line);
  Check(MainForm.TheDrawing.SelectionFirst = MainForm.TheDrawing.GetObject(0),
    'The drag fixture did not select the source line');
  MainForm.EventManager.SendMessage(Msg_Escape, MainForm);
  Check(MainForm.EventManager.Mode = BaseMode,
    'The pointer fixture did not enter the normal selection mode');
  UseSnap := False;
  CanvasPosition(Point2D(10, 5), CanvasX, CanvasY, MappedStart);
  CanvasPosition(Point2D(25, 5), CanvasX, CanvasY, MappedEnd);
  ExpectedDelta := MappedEnd.X - MappedStart.X;
  ExpectedEndX := 20 + ExpectedDelta;
  MouseDownAt(Point2D(10, 5));
  MouseMoveAt(Point2D(25, 5), [ssLeft]);
  Deadline := GetTickCount64 + 1000;
  repeat
    Application.ProcessMessages;
    Sleep(10);
  until GetTickCount64 >= Deadline;
  Check(ReadDocumentSnapshot(FileName).SourceBytes = InitialSource,
    'AutoSave published while the user still held the pointer down');
  MouseUpAt(Point2D(25, 5));
  Line := MainForm.TheDrawing.GetObject(0) as TLine2D;
  ActualStartX := Line.Points[0].X;
  ActualEndX := Line.Points[1].X;
  Check((Abs(ActualStartX - ExpectedDelta) < 0.0001) and
    (Abs(ActualEndX - ExpectedEndX) < 0.0001),
    'The completed canvas drag did not apply its pointer-derived translation '
      + '(start=' + FloatToStr(ActualStartX) + ', end=' +
      FloatToStr(ActualEndX) + ', expected delta=' +
      FloatToStr(ExpectedDelta) + ')');
  WaitForAutoSaveClean(FileName);
  Check(Abs(SavedSourceLineX2(FileName) - ActualEndX) < 0.0001,
    'AutoSave did not publish the completed source edit (saved=' +
      FloatToStr(SavedSourceLineX2(FileName)) + ', dirty=' +
      BoolToStr(MainForm.TheDrawing.History.IsChanged, True) + ')');
  Check(MainForm.TheDrawing.History.CanUndo,
    'AutoSave discarded the undo chain');
  MainForm.Undo.Execute;
  Check(Abs(AutoSaveLineX2(MainForm.TheDrawing) - 20) < 0.0001,
    'Undo did not restore the pre-AutoSave drawing');
  Check(MainForm.TheDrawing.History.IsChanged,
    'Undo after AutoSave did not leave the drawing dirty');
  MainForm.ApplyAutoSavePreference(False);
  SavedEndX := SavedSourceLineX2(FileName);
  Check((not MainForm.CurrentAutoSaveEnabled) and
    (Abs(SavedEndX - ActualEndX) < 0.0001),
    'Undo unexpectedly rewrote the source while AutoSave was disabled '
      + '(saved=' + FloatToStr(SavedEndX) + ', prior=' +
      FloatToStr(ActualEndX) + ', enabled=' +
      BoolToStr(MainForm.CurrentAutoSaveEnabled, True) + ')');

  Application.CreateForm(TOptionsForm, OptionsForm);
  MainForm.ApplyAutoSavePreference(True);
  ScaleDriver := TPictureScaleDriver.Create;
  ScaleDriver.Timer := TTimer.Create(nil);
  ScaleDriver.Timer.Interval := 40;
  ScaleDriver.Timer.OnTimer := ScaleDriver.ChangeScaleAndClose;
  ScaleDriver.Timer.Enabled := True;
  try
    MainForm.PictureProperties.Execute;
    Check(ScaleDriver.Changed,
      'Picture properties did not accept the test scale edit');
  finally
    ScaleDriver.Free;
  end;
  WaitForAutoSaveClean(FileName);
  Check(Abs(SavedSourcePicScale(FileName) - 2) < 0.0001,
    'Completed picture-property edit did not reach AutoSave');

  Line := MainForm.TheDrawing.GetObject(0) as TLine2D;
  Line.Points[1] := Point2D(30, 10);
  MainForm.TheDrawing.History.Save;
  MainForm.NotifyDocumentEdited;
  MainForm.PopupMenuPdf.Items[6].Click;
  AutoSaveItem := FindMenuCaption(MainForm.MainMenu1.Items,
    'AutoSave this document');
  Check((AutoSaveItem <> nil) and AutoSaveItem.Checked and
    not AutoSaveItem.Enabled,
    'Unsafe output profile did not visibly suspend source AutoSave');
  Deadline := GetTickCount64 + 1500;
  repeat
    Application.ProcessMessages;
    Sleep(10);
  until GetTickCount64 >= Deadline;
  Check(Abs(SavedSourceLineX2(FileName) - 20) < 0.0001,
    'Queued AutoSave published a profile that requires an external converter');
  MainForm.PopupMenuPdf.Items[5].Click;
  WaitForAutoSaveClean(FileName);
  Check(Abs(SavedSourceLineX2(FileName) - 30) < 0.0001,
    'AutoSave did not resume after restoring a safe output profile');

  MainForm.ApplyAutoSavePreference(True);
  Line := MainForm.TheDrawing.GetObject(0) as TLine2D;
  Line.Points[1] := Point2D(40, 10);
  MainForm.TheDrawing.History.Save;
  MainForm.NotifyDocumentEdited;
  WriteReloadSource(FileName, ExternalSource);
  Deadline := GetTickCount64 + 6000;
  repeat
    Application.ProcessMessages;
    MainForm.ProcessPendingReload(GetTickCount64 + 1000);
    if MainForm.AutoReloadState.Conflict and MainForm.AutoSaveConflict then Break;
    Sleep(10);
  until GetTickCount64 >= Deadline;
  Check(MainForm.AutoReloadState.Conflict and MainForm.AutoSaveConflict,
    'External change did not suspend AutoSave for a dirty source');
  Deadline := GetTickCount64 + 1800;
  repeat
    Application.ProcessMessages;
    MainForm.ProcessPendingReload(GetTickCount64 + 1000);
    Sleep(10);
  until GetTickCount64 >= Deadline;
  Check(ReadDocumentSnapshot(FileName).SourceBytes = ExternalSource,
    'AutoSave overwrote an external revision after conflict detection');
  Check(MainForm.TheDrawing.History.IsChanged,
    'External conflict cleared the local dirty drawing');
end;

type
  TRecoveryDialogDriver = class
  public
    Timer: TTimer;
    Clicked: Boolean;
    destructor Destroy; override;
    procedure FindAndRecover(Sender: TObject);
  end;

destructor TRecoveryDialogDriver.Destroy;
begin
  Timer.Free;
  inherited Destroy;
end;

procedure TRecoveryDialogDriver.FindAndRecover(Sender: TObject);
var I, J: Integer; Dialog: TForm; Component: TComponent;
begin
  for I := 0 to Screen.FormCount - 1 do
  begin
    Dialog := Screen.Forms[I];
    if Dialog.Caption <> 'Recover a drawing' then Continue;
    for J := 0 to Dialog.ComponentCount - 1 do
    begin
      Component := Dialog.Components[J];
      if (Component is TListBox) then TListBox(Component).ItemIndex := 0;
      if (Component is TButton) and
        SameText(TButton(Component).Caption, 'Recover') then
      begin
        Timer.Enabled := False;
        Clicked := True;
        TButton(Component).Click;
        Exit;
      end;
    end;
  end;
end;

function AutoSaveRecoveryRoot: string;
begin
  Result := IncludeTrailingPathDelimiter(GetAppConfigDir(False)) +
    'AutoSaveRecovery';
end;

procedure TestAutoSaveRecoveryCreate;
var Store: TAutoSaveStore; Files: TStringList; Entry: TAutoSaveStoredRecord;
  Deadline: QWord; Bitmap: TBitmap; BitmapEntry: TBitmapEntry;
  BitmapObject: TBitmap2D; ImageFile: string;
begin
  MainForm.Show;
  Application.ProcessMessages;
  MainForm.TheDrawing.Clear;
  MainForm.TheDrawing.History.Clear;
  ImageFile := IncludeTrailingPathDelimiter(GetTempDir) +
    'autosave-pasted-image.bmp';
  Bitmap := TBitmap.Create;
  try
    Bitmap.SetSize(2, 2);
    Bitmap.Canvas.Brush.Color := clFuchsia;
    Bitmap.Canvas.FillRect(Rect(0, 0, 2, 2));
    Bitmap.SaveToFile(ImageFile);
  finally
    Bitmap.Free;
  end;
  BitmapEntry := MainForm.TheDrawing.RegisterBitmap(ImageFile);
  BitmapObject := TBitmap2D.CreateSpec(-1, Point2D(0, 0), Point2D(10, 10),
    BitmapEntry);
  MainForm.TheDrawing.AddObject(-1, BitmapObject);
  MainForm.TheDrawing.History.Save;
  MainForm.TheDrawing.History.MarkDirty;
  MainForm.NotifyDocumentEdited;
  Check(MainForm.EventManager.DocumentSession.SourcePath = '',
    'Untitled recovery fixture unexpectedly has a source path');
  Store := TAutoSaveStore.Create(AutoSaveRecoveryRoot);
  Files := TStringList.Create;
  try
    Deadline := GetTickCount64 + 6000;
    repeat
      Application.ProcessMessages;
      Store.ListDrafts(Files);
      if Files.Count > 0 then Break;
      Sleep(10);
    until GetTickCount64 >= Deadline;
    Check(Files.Count > 0,
      'Crash recovery did not persist an untitled bitmap drawing');
    Check(Store.LoadRecord(Files[0], Entry) and
      (Entry.Kind = asrDraft) and (Entry.SourcePath = '') and
      (Entry.PayloadFormatId = 'tpx-assets') and (Entry.Payload <> ''),
      'Untitled bitmap recovery record was incomplete');
    Check(DeleteFile(ImageFile),
      'Could not remove the pasted bitmap source before simulated restart');
  finally
    Files.Free;
    Store.Free;
  end;
end;

procedure TestAutoSaveEmptyRecoveryCreate;
const
  InitialSource: RawByteString =
    '%<TpX v="5" TeXFormat="none" PdfTeXFormat="none">' + LineEnding +
    '%<line x1="0" y1="0" x2="20" y2="10"/>' + LineEnding +
    '%</TpX>' + LineEnding;
var Store: TAutoSaveStore; Files: TStringList; Entry: TAutoSaveStoredRecord;
  Deadline: QWord; SourcePath, ErrorText: string;
begin
  SourcePath := GetEnvironmentVariable('TPX_AUTOSAVE_EMPTY_SOURCE');
  Check(SourcePath <> '', 'Empty-draft source path was not provided');
  WriteReloadSource(SourcePath, InitialSource);
  MainForm.Show;
  Application.ProcessMessages;
  Check(BaseMode.OpenDocumentFromPath(SourcePath, ErrorText),
    'Could not open empty-draft source: ' + ErrorText);
  MainForm.TheDrawing.Clear;
  MainForm.TheDrawing.History.Clear;
  MainForm.TheDrawing.History.Save;
  MainForm.TheDrawing.History.MarkDirty;
  MainForm.NotifyDocumentEdited;
  Check(MainForm.TheDrawing.ObjectsCount = 0,
    'Empty-draft fixture unexpectedly retained drawing objects');
  Check(MainForm.EventManager.DocumentSession.SourcePath = SourcePath,
    'Dirty empty draft lost its named source binding');
  Store := TAutoSaveStore.Create(AutoSaveRecoveryRoot);
  Files := TStringList.Create;
  try
    Deadline := GetTickCount64 + 6000;
    repeat
      Application.ProcessMessages;
      Store.ListDrafts(Files);
      if Files.Count > 0 then Break;
      Sleep(10);
    until GetTickCount64 >= Deadline;
    Check((Files.Count > 0) and Store.LoadRecord(Files[0], Entry) and
      (Entry.SourcePath = SourcePath) and (Entry.Payload <> '') and
      (Entry.BaseSourceBytes = InitialSource),
      'Crash recovery did not retain the dirty empty model and its source baseline');
    Check(ReadDocumentSnapshot(SourcePath).SourceBytes = InitialSource,
      'Recovery wrote over the unchanged source while saving an empty draft');
  finally
    Files.Free;
    Store.Free;
  end;
end;

procedure TestAutoSaveEmptyRecoveryRestore;
var Driver: TRecoveryDialogDriver; Deadline: QWord;
begin
  Driver := TRecoveryDialogDriver.Create;
  Driver.Timer := TTimer.Create(nil);
  Driver.Timer.Interval := 40;
  Driver.Timer.OnTimer := Driver.FindAndRecover;
  Driver.Timer.Enabled := True;
  try
    MainForm.Show;
    Deadline := GetTickCount64 + 8000;
    repeat
      Application.ProcessMessages;
      if Driver.Clicked and MainForm.TheDrawing.History.IsChanged then Break;
      Sleep(10);
    until GetTickCount64 >= Deadline;
    Check(Driver.Clicked, 'Startup did not offer the dirty empty draft');
    Check(MainForm.TheDrawing.History.IsChanged and
      (MainForm.TheDrawing.ObjectsCount = 0) and
      (MainForm.EventManager.DocumentSession.SourcePath =
        GetEnvironmentVariable('TPX_AUTOSAVE_EMPTY_SOURCE')) and
      (MainForm.EventManager.DocumentSession.LocalRevision > 0),
      'Restart recovery did not restore the dirty empty document');
    Check(ReadDocumentSnapshot(
      GetEnvironmentVariable('TPX_AUTOSAVE_EMPTY_SOURCE')).SourceBytes =
      '%<TpX v="5" TeXFormat="none" PdfTeXFormat="none">' + LineEnding +
      '%<line x1="0" y1="0" x2="20" y2="10"/>' + LineEnding +
      '%</TpX>' + LineEnding,
      'Restart recovery changed the accepted source bytes');
  finally
    Driver.Free;
  end;
end;

procedure TestAutoSaveUnresolvedImageWarning;
var BitmapEntry: TBitmapEntry; BitmapObject: TBitmap2D;
  Deadline: QWord; WarningText: string;
begin
  MainForm.Show;
  Application.ProcessMessages;
  MainForm.TheDrawing.Clear;
  MainForm.TheDrawing.History.Clear;
  BitmapEntry := MainForm.TheDrawing.RegisterBitmap(
    IncludeTrailingPathDelimiter(GetTempDir) + 'missing-recovery-image.bmp');
  BitmapObject := TBitmap2D.CreateSpec(-1, Point2D(0, 0), Point2D(10, 10),
    BitmapEntry);
  MainForm.TheDrawing.AddObject(-1, BitmapObject);
  MainForm.TheDrawing.History.Save;
  MainForm.TheDrawing.History.MarkDirty;
  MainForm.NotifyDocumentEdited;
  Deadline := GetTickCount64 + 6000;
  repeat
    Application.ProcessMessages;
    if MainForm.StatusBar1.Panels.Count > 0 then
      WarningText := MainForm.StatusBar1.Panels[
        MainForm.StatusBar1.Panels.Count - 1].Text;
    if Pos('omits 1 linked image', LowerCase(WarningText)) > 0 then Break;
    Sleep(10);
  until GetTickCount64 >= Deadline;
  Check(Pos('omits 1 linked image', LowerCase(WarningText)) > 0,
    'The current recovery draft did not warn about its unresolved bitmap link');
end;

procedure TestAutoSaveRecoveryRestore;
var Driver: TRecoveryDialogDriver; Deadline: QWord; BitmapObject: TBitmap2D;
  SavePath, ErrorText: string; Saved: Boolean;
begin
  Driver := TRecoveryDialogDriver.Create;
  Driver.Timer := TTimer.Create(nil);
  Driver.Timer.Interval := 40;
  Driver.Timer.OnTimer := Driver.FindAndRecover;
  Driver.Timer.Enabled := True;
  try
    MainForm.Show;
    Deadline := GetTickCount64 + 8000;
    repeat
      Application.ProcessMessages;
      if Driver.Clicked and MainForm.TheDrawing.History.IsChanged then Break;
      Sleep(10);
    until GetTickCount64 >= Deadline;
    Check(Driver.Clicked,
      'Startup did not offer an explicit recovery draft choice');
    Check(MainForm.TheDrawing.History.IsChanged and
      (MainForm.TheDrawing.ObjectsCount = 1) and
      (MainForm.EventManager.DocumentSession.SourcePath = '') and
      (MainForm.EventManager.DocumentSession.LocalRevision > 0),
      'Restart recovery did not restore the untitled bitmap drawing');
    BitmapObject := MainForm.TheDrawing.GetObject(0) as TBitmap2D;
    Check((BitmapObject.Bitmap <> nil) and (BitmapObject.Bitmap.Width = 2) and
      (BitmapObject.Bitmap.Height = 2) and
      (BitmapObject.Bitmap.Canvas.Pixels[0, 0] = clFuchsia),
      'Restart recovery lost the pasted bitmap pixels after its source was deleted');
    SavePath := IncludeTrailingPathDelimiter(GetTempDir) +
      'autosave-recovered-image.tpx';
    Saved := BaseMode.SaveDocumentToPath(SavePath, 'tpx', True, True, ErrorText);
    Check(Saved,
      'Recovered bitmap could not be saved: ' + ErrorText);
    Saved := BaseMode.OpenDocumentFromPath(SavePath, ErrorText);
    Check(Saved,
      'Saved recovery bitmap document could not be reopened: ' + ErrorText);
    BitmapObject := MainForm.TheDrawing.GetObject(0) as TBitmap2D;
    Check((BitmapObject.Bitmap <> nil) and (BitmapObject.Bitmap.Width = 2) and
      (BitmapObject.Bitmap.Height = 2) and
      (BitmapObject.Bitmap.Canvas.Pixels[0, 0] = clFuchsia),
      'Saving the recovered document did not preserve its bitmap pixels');
  finally
    Driver.Free;
  end;
end;

procedure TestAutoSaveNamedRecoveryCreate;
const
  InitialSource: RawByteString =
    '%<TpX v="5" TeXFormat="none" PdfTeXFormat="none">' + LineEnding +
    '%<line x1="0" y1="0" x2="20" y2="10"/>' + LineEnding +
    '%</TpX>' + LineEnding;
var Store: TAutoSaveStore; Files: TStringList; Entry: TAutoSaveStoredRecord;
  Deadline: QWord; Bitmap: TBitmap; BitmapEntry: TBitmapEntry;
  BitmapObject: TBitmap2D; ImageFile, ImageFile2, BitmapDirectory,
  SourcePath, ErrorText: string;
begin
  SourcePath := GetEnvironmentVariable('TPX_AUTOSAVE_NAMED_SOURCE');
  Check(SourcePath <> '', 'Named recovery source path was not provided');
  WriteReloadSource(SourcePath, InitialSource);
  MainForm.Show;
  Check(BaseMode.OpenDocumentFromPath(SourcePath, ErrorText),
    'Could not open named recovery source: ' + ErrorText);
  ImageFile := IncludeTrailingPathDelimiter(GetTempDir) +
    'foo.bmp';
  Bitmap := TBitmap.Create;
  try
    Bitmap.SetSize(2, 2);
    Bitmap.Canvas.Brush.Color := clFuchsia;
    Bitmap.Canvas.FillRect(Rect(0, 0, 2, 2));
    Bitmap.SaveToFile(ImageFile);
  finally
    Bitmap.Free;
  end;
  BitmapEntry := MainForm.TheDrawing.RegisterBitmap(ImageFile);
  BitmapObject := TBitmap2D.CreateSpec(-1, Point2D(0, 0), Point2D(10, 10),
    BitmapEntry);
  MainForm.TheDrawing.AddObject(-1, BitmapObject);
  BitmapDirectory := IncludeTrailingPathDelimiter(ExtractFilePath(SourcePath)) +
    'bitmaps';
  Check(ForceDirectories(BitmapDirectory),
    'Could not create the named source bitmap directory');
  ImageFile2 := IncludeTrailingPathDelimiter(BitmapDirectory) +
    'recovery-000001.bmp';
  Bitmap := TBitmap.Create;
  try
    Bitmap.SetSize(2, 2);
    Bitmap.Canvas.Brush.Color := clLime;
    Bitmap.Canvas.FillRect(Rect(0, 0, 2, 2));
    Bitmap.SaveToFile(ImageFile2);
  finally
    Bitmap.Free;
  end;
  BitmapEntry := MainForm.TheDrawing.RegisterBitmap(ImageFile2);
  Check(ExtractFileName(BitmapEntry.ImageLink) = 'recovery-000001.bmp',
    'The collision fixture did not retain the generated archive name');
  BitmapObject := TBitmap2D.CreateSpec(-1, Point2D(20, 0), Point2D(30, 10),
    BitmapEntry);
  MainForm.TheDrawing.AddObject(-1, BitmapObject);
  MainForm.TheDrawing.History.Save;
  MainForm.NotifyDocumentEdited;
  Store := TAutoSaveStore.Create(AutoSaveRecoveryRoot);
  Files := TStringList.Create;
  try
    Deadline := GetTickCount64 + 6000;
    repeat
      Application.ProcessMessages;
      Store.ListDrafts(Files);
      if Files.Count > 0 then Break;
      Sleep(10);
    until GetTickCount64 >= Deadline;
    Check(Files.Count > 0,
      'Crash recovery did not persist a named bitmap drawing');
    Check(Store.LoadRecord(Files[0], Entry) and (Entry.Kind = asrDraft) and
      (Entry.SourcePath = SourcePath) and
      (Entry.PayloadFormatId = 'tpx-assets') and (Entry.Payload <> ''),
      'Named bitmap recovery record was incomplete');
    Check(DeleteFile(ImageFile),
      'Could not remove the named bitmap source before simulated restart');
    Check(DeleteFile(ImageFile2),
      'Could not remove the colliding relative bitmap source before restart');
  finally
    Files.Free;
    Store.Free;
  end;
  { Persist the requested preference after the draft exists. The process exits
    before its newly armed source-save deadline, simulating a crash. }
  MainForm.ApplyAutoSavePreference(True);
end;

procedure TestAutoSaveNamedRecoveryRestore;
const
  InitialSource: RawByteString =
    '%<TpX v="5" TeXFormat="none" PdfTeXFormat="none">' + LineEnding +
    '%<line x1="0" y1="0" x2="20" y2="10"/>' + LineEnding +
    '%</TpX>' + LineEnding;
var Driver: TRecoveryDialogDriver; Deadline: QWord; BitmapObject: TBitmap2D;
  SourcePath, ErrorText: string; AutoSaveItem: TMenuItem; Saved: Boolean;
begin
  SourcePath := GetEnvironmentVariable('TPX_AUTOSAVE_NAMED_SOURCE');
  Check(SourcePath <> '', 'Named recovery source path was not provided');
  Driver := TRecoveryDialogDriver.Create;
  Driver.Timer := TTimer.Create(nil);
  Driver.Timer.Interval := 40;
  Driver.Timer.OnTimer := Driver.FindAndRecover;
  Driver.Timer.Enabled := True;
  try
    MainForm.Show;
    Deadline := GetTickCount64 + 8000;
    repeat
      Application.ProcessMessages;
      if Driver.Clicked and MainForm.TheDrawing.History.IsChanged then Break;
      Sleep(10);
    until GetTickCount64 >= Deadline;
    Check(Driver.Clicked, 'Startup did not offer the named recovery draft');
    Check(MainForm.EventManager.DocumentSession.SourcePath = SourcePath,
      'Named recovery lost its source binding');
    Check(MainForm.CurrentAutoSaveEnabled,
      'Named recovery did not retain the requested AutoSave preference');
    AutoSaveItem := FindMenuCaption(MainForm.MainMenu1.Items,
      'AutoSave this document');
    Check((AutoSaveItem <> nil) and AutoSaveItem.Checked and
      not AutoSaveItem.Enabled and
      (Pos('relocate', LowerCase(AutoSaveItem.Hint)) > 0),
      'AutoSave did not explain that recovered bitmap assets must be relocated');
    Check(MainForm.TheDrawing.History.IsChanged and
      (MainForm.TheDrawing.ObjectsCount = 3),
      'Named recovery did not restore the dirty bitmap drawing');
    BitmapObject := MainForm.TheDrawing.GetObject(1) as TBitmap2D;
    Check((BitmapObject.Bitmap <> nil) and (BitmapObject.Bitmap.Width = 2) and
      (BitmapObject.Bitmap.Canvas.Pixels[0, 0] = clFuchsia),
      'First colliding recovery asset lost its embedded bitmap pixels');
    BitmapObject := MainForm.TheDrawing.GetObject(2) as TBitmap2D;
    Check((BitmapObject.Bitmap <> nil) and (BitmapObject.Bitmap.Width = 2) and
      (BitmapObject.Bitmap.Canvas.Pixels[0, 0] = clLime),
      'Second colliding recovery asset was replaced by the first bitmap');
    Deadline := GetTickCount64 + 1800;
    repeat
      Application.ProcessMessages;
      MainForm.ProcessPendingReload(GetTickCount64 + 1000);
      Sleep(10);
    until GetTickCount64 >= Deadline;
    Check(ReadDocumentSnapshot(SourcePath).SourceBytes = InitialSource,
      'AutoSave published a primary document without its recovery bitmap assets (actual=' +
      IntToStr(Length(ReadDocumentSnapshot(SourcePath).SourceBytes)) + ' bytes; ' +
      AutoSaveItem.Hint + ')');
    Saved := BaseMode.SaveDocumentToPath(SourcePath, 'tpx', False, True,
      ErrorText);
    Check(Saved,
      'Manual save could not relocate the recovered bitmap assets: ' + ErrorText);
    Saved := BaseMode.OpenDocumentFromPath(SourcePath, ErrorText);
    Check(Saved, 'Saved named recovery did not reopen: ' + ErrorText);
    Check(MainForm.TheDrawing.ObjectsCount = 3,
      'Saved named recovery lost its bitmap object');
    BitmapObject := MainForm.TheDrawing.GetObject(1) as TBitmap2D;
    Check((BitmapObject.Bitmap <> nil) and (BitmapObject.Bitmap.Width = 2) and
      (BitmapObject.Bitmap.Canvas.Pixels[0, 0] = clFuchsia),
      'Manual save did not preserve the first bitmap pixels');
    BitmapObject := MainForm.TheDrawing.GetObject(2) as TBitmap2D;
    Check((BitmapObject.Bitmap <> nil) and (BitmapObject.Bitmap.Width = 2) and
      (BitmapObject.Bitmap.Canvas.Pixels[0, 0] = clLime),
      'Manual save did not preserve the second bitmap pixels');
  finally
    Driver.Free;
  end;
end;

procedure TestAutoSaveRecoveryWatchBinding;
const
  InitialSource: RawByteString =
    '%<TpX v="5" TeXFormat="none" PdfTeXFormat="none">' + LineEnding +
    '%<line x1="0" y1="0" x2="20" y2="10"/>' + LineEnding +
    '%</TpX>' + LineEnding;
  FirstExternal: RawByteString =
    '%<TpX v="5" TeXFormat="none" PdfTeXFormat="none">' + LineEnding +
    '%<line x1="0" y1="0" x2="30" y2="10"/>' + LineEnding +
    '%</TpX>' + LineEnding;
  SecondExternal: RawByteString =
    '%<TpX v="5" TeXFormat="none" PdfTeXFormat="none">' + LineEnding +
    '%<line x1="0" y1="0" x2="40" y2="10"/>' + LineEnding +
    '%</TpX>' + LineEnding;
var
  SourcePath: string;
  Store: TAutoSaveStore;
  EntryRevision: TDiskRevision;
  Driver: TRecoveryDialogDriver;
  Deadline: QWord;
  MenuItem: TMenuItem;
  ErrorText: string;
  Payload: RawByteString;

  procedure RecoverFirstDraft;
  begin
    Driver := TRecoveryDialogDriver.Create;
    Driver.Timer := TTimer.Create(nil);
    Driver.Timer.Interval := 40;
    Driver.Timer.OnTimer := Driver.FindAndRecover;
    Driver.Timer.Enabled := True;
    try
      MenuItem := FindMenuCaption(MainForm.MainMenu1.Items,
        'Recover drafts...');
      Check(MenuItem <> nil, 'The recovery menu item is unavailable');
      MenuItem.Click;
      Check(Driver.Clicked, 'The recovery dialog did not offer its draft');
    finally
      Driver.Free;
    end;
  end;

begin
  SourcePath := GetEnvironmentVariable('TPX_AUTOSAVE_WATCH_SOURCE');
  Check(SourcePath <> '', 'Recovery watch source path was not provided');
  WriteReloadSource(SourcePath, InitialSource);
  MainForm.Show;
  Application.ProcessMessages;
  Check(BaseMode.OpenDocumentFromPath(SourcePath, ErrorText),
    'Could not open the source for recovery-watch testing: ' + ErrorText);
  Check(MainForm.AutoReloadState.Open,
    'Opening a named source did not bind its watcher');
  Store := TAutoSaveStore.Create(AutoSaveRecoveryRoot);
  EntryRevision.ContentDigest := '';
  EntryRevision.Size := 0;
  EntryRevision.ModifiedUTC := 0;
  EntryRevision.Identity := '';
  EntryRevision.Exists := False;
  try
    Store.SaveDraft('watch-binding-test', '', 'tpx', 'tpx', EntryRevision,
      '', 1, 'not a TpX recovery model');
    RecoverFirstDraft;
    Check(MainForm.EventManager.DocumentSession.SourcePath = SourcePath,
      'A failed recovery changed the active source binding');
    Check(MainForm.AutoReloadState.Open,
      'A failed recovery closed the existing watcher');

    WriteReloadSource(SourcePath, FirstExternal);
    Deadline := GetTickCount64 + 6000;
    repeat
      Application.ProcessMessages;
      MainForm.ProcessPendingReload(GetTickCount64 + 1000);
      if (MainForm.TheDrawing.ObjectsCount > 0) and
        (Abs(AutoSaveLineX2(MainForm.TheDrawing) - 30) < 0.0001) then Break;
      Sleep(10);
    until GetTickCount64 >= Deadline;
    Check((MainForm.AutoReloadState.Open) and
      (Abs(AutoSaveLineX2(MainForm.TheDrawing) - 30) < 0.0001),
      'The original watcher did not remain functional after a failed recovery');

    Payload := SerializeTpXRecoveryBytes(MainForm.TheDrawing,
      TDocumentPath(UTF8String(SourcePath)));
    Store.SaveDraft('watch-binding-test', '', 'tpx', 'tpx', EntryRevision,
      '', 2, Payload);
    RecoverFirstDraft;
    Check(MainForm.EventManager.DocumentSession.SourcePath = '',
      'Untitled recovery retained the previous named source path');
    Check(not MainForm.AutoReloadState.Open,
      'Successful untitled recovery left the previous source watcher open');

    WriteReloadSource(SourcePath, SecondExternal);
    Deadline := GetTickCount64 + 1800;
    repeat
      Application.ProcessMessages;
      MainForm.ProcessPendingReload(GetTickCount64 + 1000);
      Sleep(10);
    until GetTickCount64 >= Deadline;
    Check(ReadDocumentSnapshot(SourcePath).SourceBytes = SecondExternal,
      'The external source update did not reach disk');
    Check((not MainForm.AutoReloadState.Open) and
      (MainForm.EventManager.DocumentSession.SourcePath = '') and
      (Abs(AutoSaveLineX2(MainForm.TheDrawing) - 30) < 0.0001),
      'External edits to the old source affected the recovered untitled drawing');
  finally
    Store.DeleteDraft('watch-binding-test');
    Store.Free;
    DeleteFile(SourcePath);
  end;
end;

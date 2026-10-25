program RuntimeTests;
{$mode Delphi}
{$IFDEF LCLCOCOA}{$modeswitch objectivec2}{$ENDIF}
uses
  {$IFDEF UNIX}{$IFNDEF CPUWASM32}cthreads,{$ENDIF}{$ENDIF}
  {$IFDEF UNIX}BaseUnix,{$ENDIF}
  Interfaces, Forms, SysUtils, Classes, Types, Math, Controls, Dialogs, ExtCtrls, Clipbrd, InterfaceBase, LCLType, LMessages, Process, LazFileUtils, LazUTF8,
  {$IFDEF LCLgtk2}Gtk2Int,{$ENDIF}
  {$IFDEF LCLcocoa}CocoaInt, CocoaAll, CocoaTextEdits, CocoaUtils,{$ENDIF}
  {$IFDEF LCLwin32}Win32Int,{$ENDIF}
  Settings0, MainUnit, Options, Table, Drawings, GObjects, Geometry, Manage, Modes, Input, Devices, Graphics, StdCtrls, ActnList, Menus, GObjBase, SysBasic, Preview, ViewPort, Modify, Output, Bitmaps, ClpbrdOp, ColorEtc, PlatformShortcuts, LiveTeX, DocumentFormats, DocumentIO, AutoSaveStore, AutoSaveRuntime, Pieces, TikZImport, TikZImportDrawing, TikZDocumentWriter, SvgCodec, Solid3D, StrUtils, EditSessions, PropertyEdits, PropertyEditCore, CompositionAwareControls, UserStorage, ObjectInspector, Buttons, gzio;

{$R ../src/MainUnit.lfm}
{$R ../src/Options.lfm}

type
  TTestBitmapEntry = class(TBitmapEntry)
  public
    procedure ConfigureLinkedPng(const AssetFileName,
      ParentFileName: string);
  end;

  {$IFDEF LCLgtk2}TNativeWidgetSet = TGtk2WidgetSet;{$ENDIF}
  {$IFDEF LCLcocoa}TNativeWidgetSet = TCocoaWidgetSet;{$ENDIF}
  {$IFDEF LCLwin32}TNativeWidgetSet = TWin32WidgetSet;{$ENDIF}
  TTestWidgetSet = class(TNativeWidgetSet)
    function GetKeyState(nVirtKey: Integer): SmallInt; override;
    function PromptUser(const Caption, Message: string; DialogType: LongInt;
      Buttons: PLongInt; ButtonCount, DefaultIndex, EscapeResult: LongInt): LongInt; override;
  end;
  {$IFDEF LCLCOCOA}
  TCompositionObserver = class
    Starts, Ends: Integer;
    Committed: Boolean;
    FinalText: string;
    DestroyOnEnd: Boolean;
    procedure Changed(Sender: TObject; Composing: Boolean);
  end;
  {$ENDIF}
  TObserver = class
    CloseRequests: Integer;
    OSOpenFirst, OSOpenSecond, OSOpenMarkerFirst,
      OSOpenMarkerSecond: string;
    OSOpenDeadline: QWord;
    OSOpenTimer: TTimer;
    OSOpenStage: Integer;
    OSOpenSucceeded: Boolean;
    procedure Closing(Sender: TObject; var Action: TCloseAction);
    procedure ClosingDefault(Sender: TObject; var Action: TCloseAction);
    procedure RunClipboardScenarioInEventLoop(Sender: TObject);
    procedure CheckOSOpenLaunchServices(Sender: TObject);
  end;
  TSourceWorkflowTimerObserver = class
    Timer: TTimer;
    Ticks: Integer;
    Previous: TNotifyEvent;
    procedure TimerTick(Sender: TObject);
  end;

var
  Observer: TObserver;
  PromptCount, ReplyButton, SaveErrorPromptCount, ExportErrorPromptCount: Integer;
  ShortcutModifierSimulation: Boolean = False;
  ShortcutModifiers: TShiftState = [];
  SourceWorkflowFaultPhase: TDocumentIOPhase;
  SourceWorkflowFaultTarget: TDocumentPath;
  SourceWorkflowFaultCount: Integer;
  SourceWorkflowWriteExternalBytes: Boolean;
  SourceWorkflowExternalBytes: RawByteString;
  SourceWorkflowIdleTarget: TDocumentPath;
  SourceWorkflowIdlePathExistsCount, SourceWorkflowIdleMetadataCount,
    SourceWorkflowIdleSnapshotCount, SourceWorkflowIdleWriteCount: Integer;
  ClipboardScenarioTimer: TTimer;
  ClipboardScenarioError: string;

{$IFDEF FPC}{$IFDEF WINDOWS}
function TpxCreateHardLinkW(NewFileName, ExistingFileName: PWideChar;
  SecurityAttributes: Pointer): LongBool; stdcall;
  external 'kernel32' name 'CreateHardLinkW';
{$ENDIF}{$ENDIF}

procedure Check(Condition: Boolean; const Message: string);
begin
  if not Condition then raise Exception.Create(Message);
end;

function WriteUTF8Marker(const FileName, Value: string): Boolean;
var
  Handle: THandle;
  Data: AnsiString;
begin
  Result := False;
  Handle := FileCreateUTF8(FileName);
  if Handle = THandle(-1) then Exit;
  Data := Value;
  try
    Result := (Length(Data) > 0) and
      (FileWrite(Handle, Data[1], Length(Data)) = Length(Data));
  finally
    FileClose(Handle);
  end;
end;

procedure TTestBitmapEntry.ConfigureLinkedPng(const AssetFileName,
  ParentFileName: string);
begin
  fParentFileName := ParentFileName;
  fImageLink := ExtractFileName(AssetFileName);
  fKind := bek_PNG;
end;

procedure TObserver.Closing(Sender: TObject; var Action: TCloseAction);
begin
  Inc(CloseRequests);
  Action := caNone;
end;

procedure TObserver.ClosingDefault(Sender: TObject; var Action: TCloseAction);
begin
  Inc(CloseRequests);
end;

procedure TSourceWorkflowTimerObserver.TimerTick(Sender: TObject);
begin
  Inc(Ticks);
  if Assigned(Previous) then Previous(Sender);
end;

procedure TObserver.CheckOSOpenLaunchServices(Sender: TObject);
var
  OpenedPath: TDocumentPath;
begin
  if GetTickCount64 >= OSOpenDeadline then
  begin
    OSOpenTimer.Enabled := False;
    Application.Terminate;
    Exit;
  end;
  OpenedPath := TDocumentPath(MainForm.TheDrawing.FileName);
  if (OSOpenStage = 0) and
    SameDocumentPath(TDocumentPath(OSOpenFirst), OpenedPath) then
  begin
    OSOpenSucceeded := WriteUTF8Marker(OSOpenMarkerFirst, 'FIRST');
    if not OSOpenSucceeded then
    begin
      OSOpenTimer.Enabled := False;
      Application.Terminate;
      Exit;
    end;
    OSOpenStage := 1;
    OSOpenDeadline := GetTickCount64 + 15000;
  end
  else if (OSOpenStage = 1) and
    SameDocumentPath(TDocumentPath(OSOpenSecond), OpenedPath) then
  begin
    OSOpenSucceeded := WriteUTF8Marker(OSOpenMarkerSecond, 'PASS');
    OSOpenTimer.Enabled := False;
    Application.Terminate;
  end;
end;

function AnswerPrompt(const Caption, Message: string; DialogType: LongInt;
  Buttons: PLongInt; ButtonCount, DefaultIndex, EscapeResult: LongInt;
  UseDefaultPos: Boolean; X, Y: LongInt): LongInt;
begin
  if (ParamStr(1) = 'auto-reload-integration') and
    (Pos('Reloading from disk will discard local edits', Message) > 0) then
  begin
    Inc(PromptCount);
    Exit(idButtonYes);
  end;
  if (ParamStr(1) = 'autosave-recovery-watch-binding') and
    (Pos('Could not recover the selected draft', Message) > 0) then
    Exit(idButtonOK);
  if (ParamStr(1) = 'bitmap-eps') and
    (Pos('Conversion of bitmap to EPS failed:', Message) > 0) then begin
    Inc(PromptCount);
    Exit(idButtonOK);
  end;
  if (ParamStr(1) = 'tpx-staged-sidecars') and
    ((Pos('MPS file not created', Message) > 0) or
     (Pos('PDF file not created', Message) > 0)) then
    Exit(idButtonOK);
  if (ParamStr(1) = 'property-dimensions') and
    ((Pos('Line width must', Message) > 0) or (Pos('Text height must', Message) > 0)) then
    Exit(idButtonOK);
  if (ParamStr(1) = 'tool-hint-save') and
    (Pos('Can not export', Message) > 0) then begin
    Inc(ExportErrorPromptCount);
    Check(Pos('Ghostscript did not create the image', Message) > 0,
      'Converter failure lost its useful error message: ' + Message);
    Exit(idButtonOK);
  end;
  if (ParamStr(1) = 'exit-save-failure') and
    (Pos('Can not save', Message) > 0) then begin
    Inc(SaveErrorPromptCount);
    Exit(idButtonOK);
  end;
  Check(Pos('Save current drawing?', Message) > 0, 'Unexpected dialog: ' + Message);
  Inc(PromptCount);
  Result := ReplyButton;
end;

function TTestWidgetSet.GetKeyState(nVirtKey: Integer): SmallInt;
begin
  if ShortcutModifierSimulation then begin
    Result := 0;
    if (nVirtKey = VK_CONTROL) and (ssCtrl in ShortcutModifiers) then Result := -1;
    if (nVirtKey = VK_LWIN) and (ssMeta in ShortcutModifiers) then Result := -1;
    if (nVirtKey = VK_SHIFT) and (ssShift in ShortcutModifiers) then Result := -1;
  end else Result := inherited GetKeyState(nVirtKey);
end;

function TTestWidgetSet.PromptUser(const Caption, Message: string;
  DialogType: LongInt; Buttons: PLongInt; ButtonCount, DefaultIndex,
  EscapeResult: LongInt): LongInt;
begin
  Result := AnswerPrompt(Caption, Message, DialogType, Buttons, ButtonCount,
    DefaultIndex, EscapeResult, True, 0, 0);
end;

{$I ViewportScenarios.inc}
{$I PlaytestScenarios.inc}
{$I TpXStagedSaveScenarios.inc}
{$I DocumentIOScenarios.inc}
{$I AutoReloadScenarios.inc}
{$I TikZImportScenarios.inc}
{$I TikZSaveScenarios.inc}
{$I TikZStyleScenarios.inc}
{$I SvgCodecScenarios.inc}
{$I Solid3DScenarios.inc}
{$I AutoSaveScenarios.inc}
{$I SourceFormatWorkflowScenarios.inc}
{$I EditSessionScenarios.inc}
{$I NonModalRecoveryScenarios.inc}
{$I PaintStyleScenarios.inc}

procedure RunBitmapFixtureConverter;
var
  Source, Destination: TFileStream;
  ExpectedExtension: string;
begin
  if GetEnvironmentVariable('TPX_EPS_FIXTURE_MODE') = 'nonzero' then Halt(7);
  if GetEnvironmentVariable('TPX_EPS_FIXTURE_MODE') = 'no-output' then Halt(0);
  Check(ParamCount = 2, 'Fixture converter expected input and output paths');
  ExpectedExtension := GetEnvironmentVariable('TPX_EPS_EXPECT_INPUT_EXT');
  if (ExpectedExtension <> '') and
    (LowerCase(ExtractFileExt(ParamStr(1))) <> ExpectedExtension) then Halt(8);
  Source := TFileStream.Create(
    GetEnvironmentVariableUTF8('TPX_EPS_FIXTURE'), fmOpenRead);
  try
    Destination := TFileStream.Create(ParamStr(2), fmCreate);
    try Destination.CopyFrom(Source, 0) finally Destination.Free end;
  finally Source.Free end;
end;

procedure RunPstoeditFixture;
var Source, Fixture, Destination: TFileStream; Bytes: RawByteString;
begin
  Check((ParamCount >= 4) and (ParamStr(3) = '-f') and
    (ParamStr(4) = 'emf'),
    'pstoedit fixture expected input, output, and EMF format arguments');
  Source := TFileStream.Create(ParamStr(1), fmOpenRead);
  try
    SetLength(Bytes, Source.Size);
    if Source.Size > 0 then Source.ReadBuffer(Bytes[1], Source.Size);
  finally
    Source.Free;
  end;
  if Pos('TPX_FIXTURE_FAIL', Bytes) > 0 then Halt(7);
  Fixture := TFileStream.Create(
    GetEnvironmentVariable('TPX_PSTOEDIT_EMF_FIXTURE'), fmOpenRead);
  try
    SetLength(Bytes, Fixture.Size);
    if Fixture.Size > 0 then Fixture.ReadBuffer(Bytes[1], Fixture.Size);
  finally
    Fixture.Free;
  end;
  Destination := TFileStream.Create(ParamStr(2), fmCreate);
  try
    if Length(Bytes) > 0 then Destination.WriteBuffer(Bytes[1], Length(Bytes));
  finally
    Destination.Free;
  end;
end;

procedure TestBitmapEps;
var
  Converted, Expected: Boolean;
begin
  PromptCount := 0;
  Bitmap2EpsPath := GetEnvironmentVariableUTF8('TPX_EPS_CONVERTER');
  Expected := GetEnvironmentVariable('TPX_EPS_EXPECT_SUCCESS') = '1';
  Converted := BitmapToEps(GetEnvironmentVariableUTF8('TPX_EPS_INPUT'),
    GetEnvironmentVariableUTF8('TPX_EPS_OUTPUT'));
  Check(Converted = Expected, 'BitmapToEps returned ' + BoolToStr(Converted, True));
  Check(PromptCount = Ord(not Expected), 'Unexpected bitmap conversion error count');
end;

procedure TestSvgLinkedImageCopy;
const
  Payload: RawByteString = 'linked image bytes'#0'preserve exactly';
var
  AssetName, AliasName, MissingName, SourceName, SvgName, SvgText: string;
  Asset, SavedAsset: TMemoryStream;
  Drawing: TDrawing2D;
  Entry: TTestBitmapEntry;
  Objects: TStringList;
{$IFDEF FPC}{$IFDEF WINDOWS}
  CopySucceeded: Boolean;
  WideAliasName, WideAssetName: UnicodeString;
{$ENDIF}{$ENDIF}
begin
  AssetName := ExpandFileName('linked-image.png');
  SourceName := ExpandFileName('source.tpx');
  SvgName := ExpandFileName('export.svg');
{$IFDEF WINDOWS}
  AliasName := ExpandFileName('linked-image-hardlink-' +
    UTF8Encode(UnicodeString(WideChar($00E4))) + '.png');
{$ELSE}
  AliasName := ExpandFileName('linked-image-hardlink.png');
{$ENDIF}
  MissingName := ExpandFileName('missing-copy-source.png');
  Asset := TMemoryStream.Create;
  SavedAsset := TMemoryStream.Create;
  Drawing := nil;
  Entry := nil;
  Objects := TStringList.Create;
  try
    DeleteFile(MissingName);
    Check(not CopyFile(MissingName, MissingName),
      'CopyFile reported success for a missing same-path source');
    Asset.WriteBuffer(Payload[1], Length(Payload));
    Asset.SaveToFile(AssetName);
    Entry := TTestBitmapEntry.Create('', '');
    Entry.ConfigureLinkedPng(AssetName, SourceName);
    Drawing := TDrawing2D.Create(nil);
    Drawing.FileName := SourceName;
    Drawing.AddObject(-1, TBitmap2D.CreateSpec(-1,
      Point2D(0, 0), Point2D(10, 10), Entry));
    ExportToFile(Drawing, SvgName, export_SVG);

    SavedAsset.LoadFromFile(AssetName);
    Check(SavedAsset.Size = Length(Payload),
      'SVG export truncated its same-directory linked image');
    Check((SavedAsset.Size = Length(Payload)) and
      CompareMem(SavedAsset.Memory, @Payload[1], Length(Payload)),
      'SVG export changed same-directory linked image bytes');
    Objects.LoadFromFile(SvgName);
    SvgText := Objects.Text;
    Check(Pos('xlink:href="linked-image.png"', SvgText) > 0,
      'SVG export did not retain the linked image reference');
{$IFDEF UNIX}
    if fpLink(PChar(AssetName), PChar(AliasName)) = 0 then
    begin
      Check(Entry.CopyImage(AssetName, AliasName),
        'CopyImage rejected a hard-link alias to the source');
      SavedAsset.Clear;
      SavedAsset.LoadFromFile(AssetName);
      Check((SavedAsset.Size = Length(Payload)) and
        CompareMem(SavedAsset.Memory, @Payload[1], Length(Payload)),
        'CopyImage truncated a hard-link alias to the source');
    end;
{$ENDIF}
{$IFDEF FPC}{$IFDEF WINDOWS}
    WideAliasName := UTF8Decode(AliasName);
    WideAssetName := UTF8Decode(AssetName);
    Check(TpxCreateHardLinkW(PWideChar(WideAliasName),
      PWideChar(WideAssetName), nil),
      'Could not create Windows hard-link test alias');
    CopySucceeded := Entry.CopyImage(AssetName, AliasName);
    SavedAsset.Clear;
    SavedAsset.LoadFromFile(AssetName);
    Check((SavedAsset.Size = Length(Payload)) and
      CompareMem(SavedAsset.Memory, @Payload[1], Length(Payload)),
      'CopyImage truncated a Windows hard-link alias to the source');
    Check(CopySucceeded,
      'CopyImage rejected a Windows hard-link alias to the source');
{$ENDIF}{$ENDIF}
  finally
    Objects.Free;
    Drawing.Free;
    Entry.Free;
    Asset.Free;
    SavedAsset.Free;
    DeleteFile(AssetName);
    DeleteFile(AliasName);
    DeleteFile(SvgName);
  end;
end;

procedure TestLiveTeXMissingTool;
var
  Bitmap: TBitmap;
  Started: QWord;
  SavedLatex: string;
begin
  SavedLatex := LatexPath;
  ShutdownLiveTeX;
  InitializeLiveTeX(nil);
  SetLiveTeXEnabled(True);
  LatexPath := IncludeTrailingPathDelimiter(GetTempDir) + 'tpx-absent-latex';
  Bitmap := TBitmap.Create;
  try
    Bitmap.SetSize(200, 100);
    Check(not DrawLiveTeX(Bitmap.Canvas, 100, Point2D(20, 80), 24, 0,
      '$x^2$', ahLeft, jvBaseline, clBlack), 'Missing TeX did not use fallback');
    Started := GetTickCount64;
    while LiveTeXPending do begin
      Application.ProcessMessages;
      Sleep(5);
      Check(GetTickCount64 - Started < 5000, 'Missing TeX left a stuck preview');
    end;
    Check(LiveTeXStatus <> '', 'Missing TeX did not expose a status');
    Check(not DrawLiveTeX(Bitmap.Canvas, 100, Point2D(20, 80), 24, 0,
      '$x^2$', ahLeft, jvBaseline, clBlack), 'Missing TeX discarded fallback');
    Check(not LiveTeXPending, 'Missing TeX was retried on every paint');
  finally
    Bitmap.Free;
    ShutdownLiveTeX;
    LatexPath := SavedLatex;
  end;
end;

procedure TestLiveTeXSettings;
begin
  Check(LiveTeXEnabled, 'Live LaTeX preview must default to enabled');
  SetLiveTeXEnabled(False);
  SaveSettings;
  SetLiveTeXEnabled(True);
  LoadSettings;
  Check(not LiveTeXEnabled, 'Live LaTeX preview preference did not persist');
end;

procedure TestUserPreferencesWrite;
begin
  SetLiveTeXEnabled(False);
  (MainForm.FindComponent('InspectorMenuItem') as TMenuItem).Click;
  MainForm.Inspector.ShowMore := True;
  SaveSettings;
end;

procedure TestUserPreferencesRead;
begin
  Check(not LiveTeXEnabled,
    'user preferences did not survive replacing the executable');
  Check(not MainForm.InspectorPanelVisible and
    not MainForm.Inspector.Parent.Visible and MainForm.InspectorShowMore and
    MainForm.Inspector.ShowMore,
    'inspector visibility or More state did not persist');
end;

procedure TestStoragePaths;
var
  Paths: TUserStoragePaths;
  ErrorText: string;
begin
  Paths := CurrentUserStoragePaths;
  Check(EnsureUserStorageDirectory(usaConfig, ErrorText), ErrorText);
  Check(EnsureUserStorageDirectory(usaState, ErrorText), ErrorText);
  Check(EnsureUserStorageDirectory(usaCache, ErrorText), ErrorText);
  if Paths.Portable then WriteLn('STORAGE_MODE=portable')
  else WriteLn('STORAGE_MODE=installed');
  WriteLn('STORAGE_CONFIG=', Paths.Config);
  WriteLn('STORAGE_STATE=', Paths.State);
  WriteLn('STORAGE_CACHE=', Paths.Cache);
end;

procedure TestLiveTeX;
const
  Formula: array[0..3] of string = ('$\alpha_i^2$', '$\omega_{ce}$',
    '$\nabla \times B$', '$\frac{1}{2}mv^2$');
var
  Bitmap: TBitmap;
  I, X, Y, Ink, Compilations: Integer;
  SavedPixels: string;
  Started: QWord;

  function Draw(Key: PtrUInt; const Source: string; Height: Double = 32;
    Rotation: Double = 0): Boolean;
  begin
    Bitmap.Canvas.Brush.Color := clWhite;
    Bitmap.Canvas.FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height));
    Result := DrawLiveTeX(Bitmap.Canvas, Key, Point2D(20, 80), Height,
      Rotation, Source, ahLeft, jvBaseline, clBlack);
  end;

  function Pixels: string;
  var
    Stream: TMemoryStream;
  begin
    Stream := TMemoryStream.Create;
    try
      Bitmap.SaveToStream(Stream);
      SetString(Result, PChar(Stream.Memory), Stream.Size);
    finally
      Stream.Free;
    end;
  end;

  procedure AwaitPreview;
  var
    Started: QWord;
  begin
    Started := GetTickCount64;
    while LiveTeXPending do begin
      Application.ProcessMessages;
      Sleep(5);
      Check(GetTickCount64 - Started < 20000, 'Live LaTeX preview timed out');
    end;
  end;

begin
  ShutdownLiveTeX;
  InitializeLiveTeX(nil);
  SetLiveTeXEnabled(True);
  Bitmap := TBitmap.Create;
  try
    Bitmap.SetSize(400, 180);
    Compilations := LiveTeXCompilationCount;
    for I := 0 to High(Formula) do
      Check(not Draw(100 + I, Formula[I]), 'Uncached formula returned a preview');
    Check(LiveTeXPending, 'Formula requests were not queued');
    AwaitPreview;
    Check(LiveTeXCompilationCount = Compilations + 1,
      'Multiple pending formulas did not share one LaTeX invocation');
    for I := 0 to High(Formula) do
      Check(Draw(100 + I, Formula[I]), 'Real formula did not render: ' + LiveTeXStatus);
    Ink := 0;
    for Y := 0 to Bitmap.Height - 1 do
      for X := 0 to Bitmap.Width - 1 do
        if ColorToRGB(Bitmap.Canvas.Pixels[X, Y]) <> ColorToRGB(clWhite) then Inc(Ink);
    Check(Ink > 50, 'Real formula produced an empty canvas image');
    SavedPixels := Pixels;
    Check(Draw(200, Formula[3]), 'Identical source did not reuse the cached preview');
    Check(Pixels = SavedPixels, 'Cached formula changed the rendered output');
    Compilations := LiveTeXCompilationCount;
    Check(Draw(200, Formula[3], 64, 0.2), 'Zoom/rotation lost the last preview');
    AwaitPreview;
    Check(LiveTeXCompilationCount = Compilations,
      'Zoom or rotation reran LaTeX for an existing formula');

    Check(Draw(200, '$\definitelyUndefinedTpXCommand$'),
      'Editing discarded the last good preview');
    SavedPixels := Pixels;
    AwaitPreview;
    Check(LiveTeXStatus <> '', 'Malformed TeX did not expose an error status');
    Check(Draw(200, '$\definitelyUndefinedTpXCommand$'),
      'Malformed TeX discarded the last good preview');
    Check(Pixels = SavedPixels, 'Failed preview replaced the last good pixels');
    Compilations := LiveTeXCompilationCount;
    Check(Draw(200, '$\anotherUndefinedTpXCommand$'),
      'Pending edit discarded the last good preview');
    Check(Draw(200, Formula[0]), 'Newer cached source was not applied immediately');
    SavedPixels := Pixels;
    AwaitPreview;
    Check(Draw(100, Formula[0]), 'Reference formula was lost');
    Check(Pixels = SavedPixels, 'Superseded source replaced the newest preview');
    Check(LiveTeXCompilationCount = Compilations,
      'Superseded pending edit still compiled');

    Check(not Draw(400, '$x_{new}$'), 'New source unexpectedly had a cached preview');
    Check(Draw(200, Formula[0]), 'Cache hit lost an existing preview');
    Check(LiveTeXPending, 'Cache hit canceled another object preview');
    AwaitPreview;
    Check(Draw(400, '$x_{new}$'), 'Other object preview stalled after a cache hit');

    Check(not Draw(500, '$\invalidBatchTpX$'), 'Invalid batch source was cached');
    Check(not Draw(501, '$z_{valid}$'), 'New batch source was cached');
    AwaitPreview;
    Check(Draw(501, '$z_{valid}$'), 'Malformed sibling blocked a valid batch label');
    Check(not Draw(500, '$\invalidBatchTpX$'), 'Malformed label lost its fallback');

    Check(not Draw(650, '$q_{queued}$'), 'Queued source was already cached');
    SetLiveTeXEnabled(False);
    SetLiveTeXEnabled(True);
    AwaitPreview;
    Check(Draw(650, '$q_{queued}$'), 'Toggling during debounce left the preview stuck');

    Check(not Draw(600, '{\count255=0\loop\advance\count255 by1' +
      '\ifnum\count255<2000000\repeat $w$}'), 'Slow source was already cached');
    FlushLiveTeX;
    Started := GetTickCount64;
    repeat
      Application.ProcessMessages;
      Sleep(5);
    until GetTickCount64 - Started >= 250;
    SetLiveTeXEnabled(False);
    SetLiveTeXEnabled(True);
    AwaitPreview;
    Check(Draw(600, '{\count255=0\loop\advance\count255 by1' +
      '\ifnum\count255<2000000\repeat $w$}'),
      'Toggling during an active compile left the preview stuck: ' + LiveTeXStatus);

    Check(not Draw(601, '\loop\iftrue\repeat'), 'Infinite source was already cached');
    FlushLiveTeX;
    Started := GetTickCount64;
    repeat
      Application.ProcessMessages;
      Sleep(5);
    until GetTickCount64 - Started >= 250;
    Started := GetTickCount64;
    Check(Draw(601, Formula[0]), 'New cached source did not supersede active TeX');
    AwaitPreview;
    Check(GetTickCount64 - Started < 3000, 'Superseded active TeX was not canceled');
    SavedPixels := Pixels;
    Check(Draw(100, Formula[0]) and (Pixels = SavedPixels),
      'Stale active result replaced the latest formula');

    MainForm.TheDrawing.TeXFormat := tex_tikz;
    ConfigureLiveTeX(MainForm.TheDrawing);
    Check(not Draw(700, '\tikz{\draw(0,0)--(1,1);}'), 'Package context was cached');
    AwaitPreview;
    Check(Draw(700, '\tikz{\draw(0,0)--(1,1);}'), 'Drawing packages were not loaded');
    MainForm.TheDrawing.TeXFormat := tex_eps;
    ConfigureLiveTeX(MainForm.TheDrawing);
    Check(Draw(700, '\tikz{\draw(0,0)--(1,1);}'), 'Context edit discarded last preview');
    AwaitPreview;
    Check(LiveTeXStatus <> '', 'Package context change did not invalidate TeX');
    Compilations := LiveTeXCompilationCount;
    MainForm.TheDrawing.TeXFormat := tex_tikz;
    ConfigureLiveTeX(MainForm.TheDrawing);
    Check(Draw(700, '\tikz{\draw(0,0)--(1,1);}'), 'Prior package context cache was lost');
    Check(LiveTeXCompilationCount = Compilations, 'Prior context was recompiled');

    MainForm.TheDrawing.TeXFigure := fig_none;
    MainForm.TheDrawing.TeXFigurePrologue := '\begingroup\newcommand{\tpxliveword}{Short}% figure macros';
    MainForm.TheDrawing.TeXPicPrologue := '\begingroup\newcommand{\tpxlivelabel}{\tpxliveword}';
    MainForm.TheDrawing.TeXPicEpilogue := '\endgroup';
    MainForm.TheDrawing.TeXFigureEpilogue := '\endgroup% end figure scope';
    ConfigureLiveTeX(MainForm.TheDrawing);
    Compilations := LiveTeXCompilationCount;
    Check(not Draw(800, '\tpxlivelabel% source'), 'Macro context was unexpectedly cached');
    Check(not Draw(801, '\tpxlivelabel{} two'), 'Second macro label was already cached');
    AwaitPreview;
    Check(LiveTeXCompilationCount = Compilations + 1,
      'Per-label macro definitions broke batch compilation');
    Check(Draw(800, '\tpxlivelabel% source'), 'Drawing macro did not render: ' + LiveTeXStatus);
    SavedPixels := Pixels;
    Check(Draw(801, '\tpxlivelabel{} two'), 'Macros leaked between batched labels');
    MainForm.TheDrawing.TeXFigurePrologue := '\begingroup\newcommand{\tpxliveword}{Much Longer}% changed macros';
    ConfigureLiveTeX(MainForm.TheDrawing);
    Compilations := LiveTeXCompilationCount;
    Check(Draw(800, '\tpxlivelabel% source'), 'Macro edit discarded last preview');
    Check(Pixels = SavedPixels, 'Macro edit changed pixels before completion');
    AwaitPreview;
    Check(LiveTeXCompilationCount = Compilations + 1,
      'Changed macro definition did not invalidate the preview cache');
    Check(Draw(800, '\tpxlivelabel% source') and (Pixels <> SavedPixels),
      'Changed macro definition did not replace the rendered text');
    MainForm.TheDrawing.TeXFigurePrologue := '';
    MainForm.TheDrawing.TeXFigureEpilogue := '';
    MainForm.TheDrawing.TeXPicPrologue := '';
    MainForm.TheDrawing.TeXPicEpilogue := '';
    ConfigureLiveTeX(MainForm.TheDrawing);

    SetLiveTeXEnabled(False);
    Compilations := LiveTeXCompilationCount;
    Check(not Draw(300, '$x_{disabled}$'), 'Disabled preview still rendered');
    Check(not LiveTeXPending, 'Disabled preview scheduled background work');
    Check(LiveTeXCompilationCount = Compilations, 'Disabled preview ran LaTeX');
    SetLiveTeXEnabled(True);
    Check(Draw(200, Formula[0]), 'Toggling preview discarded the reusable cache');
    ForgetLiveTeXObject(200);
  finally
    Bitmap.Free;
    ShutdownLiveTeX;
  end;
end;

procedure TestPreviewState;
var
  FileName, OriginalName: string;
  Drawing: TDrawing2D;
  procedure CheckState;
  begin
    Check(Drawing.FileName = OriginalName, 'Preview changed the drawing filename');
    Check(Drawing.IncludePath = 'include path', 'Preview changed the include path');
    Check(Drawing.TeXFormat = tex_tikz, 'Preview changed the TeX format');
    Check(Drawing.PdfTeXFormat = pdftex_tikz, 'Preview changed the PDFTeX format');
    Check(Drawing.TeXFigure = fig_figure, 'Preview changed the figure wrapper');
    Check(Drawing.TeXCenterFigure, 'Preview changed centering');
    Check(Drawing.Caption = 'Caption', 'Preview changed the caption');
    Check(Drawing.FigLabel = 'fig:test', 'Preview changed the label');
  end;
begin
  Drawing := MainForm.TheDrawing;
  OriginalName := Drawing.FileName;
  Drawing.IncludePath := 'include path';
  Drawing.TeXFormat := tex_tikz;
  Drawing.PdfTeXFormat := pdftex_tikz;
  Drawing.TeXFigure := fig_figure;
  Drawing.TeXCenterFigure := True;
  Drawing.Caption := 'Caption';
  Drawing.FigLabel := 'fig:test';
  Drawing.AddObject(-1, TLine2D.CreateSpec(-1, Point2D(0, 0), Point2D(20, 10)));
  FileName := GetTempFileName(SysUtils.GetTempDir(False), 'tpx-preview-');
  DeleteFile(FileName);
  FileName := FileName + '.tex';
  try
    StoreToFile_PreviewSource(Drawing, FileName, ltxview_Pdf);
    Check(FileExists(FileName), 'Preview source was not written');
    CheckState;
    StoreToFile_PreviewSource(Drawing, FileName + '/missing.tex', ltxview_Pdf);
    CheckState;
  finally
    DeleteFile(FileName);
    DeleteFile(ChangeFileExt(FileName, '') + '(TpX).tpx');
  end;
end;

procedure TestExternalTools;
var
  OriginalDir, Directory: string;
  Contents: TStringList;
begin
  OriginalDir := GetCurrentDir;
  Directory := GetTempFileName(SysUtils.GetTempDir(False), 'tpx-tools-');
  DeleteFile(Directory);
  Directory := Directory + ' with spaces';
  Check(CreateDir(Directory), 'Could not create tool test directory');
  Contents := TStringList.Create;
  try
{$IFDEF WINDOWS}
    Check(FileExec('cmd /c echo two words > result.txt', '', '', Directory,
{$ELSE}
    Check(FileExec('printf "%s" "two words" > result.txt', '', '', Directory,
{$ENDIF}
      True, True), 'External command failed');
    Check(GetCurrentDir = OriginalDir, 'External command changed working directory');
    Contents.LoadFromFile(Directory + PathDelim + 'result.txt');
    Check(Trim(Contents.Text) = 'two words', 'External command lost quoted arguments');
{$IFDEF WINDOWS}
    Check(not FileExec('cmd /c exit 7', '', '', Directory, True, True),
{$ELSE}
    Check(not FileExec('exit 7', '', '', Directory, True, True),
{$ENDIF}
      'External tool failure was reported as success');
    Check(GetCurrentDir = OriginalDir, 'Failed tool changed working directory');
  finally
    Contents.Free;
    DeleteFile(Directory + PathDelim + 'result.txt');
    RemoveDir(Directory);
  end;
end;

type
  TToolHintObserver = class
    Stage: Integer;
    ErrorText: string;
    procedure Tick(Sender: TObject);
  end;

procedure TToolHintObserver.Tick(Sender: TObject);
var
  Dialog: TCustomForm;
  I: Integer;
  CopyButton, CancelButton, SettingsButton: TBitBtn;
begin
  Dialog := Screen.ActiveCustomForm;
  if Dialog = nil then Exit;
  if (Stage = 44) and (Dialog.Caption = 'TpX settings') then
  begin
    if OptionsForm.POptList <> SettingsList then
      ErrorText := 'Open settings did not select TpX tool settings';
    Dialog.ModalResult := mrCancel;
    Stage := 45;
    TTimer(Sender).Enabled := False;
    Exit;
  end;
  if Dialog.Caption <> 'Install Ghostscript' then Exit;
  if (GetEnvironmentVariable('TPX_HINT_SCREENSHOT') <> '') and
    (Stage < 40) then
  begin
    Inc(Stage);
    Exit;
  end;
  CopyButton := nil;
  CancelButton := nil;
  SettingsButton := nil;
  for I := 0 to Dialog.ComponentCount - 1 do
    if Dialog.Components[I] is TBitBtn then
      case TBitBtn(Dialog.Components[I]).ModalResult of
        mrNo: CopyButton := TBitBtn(Dialog.Components[I]);
        mrCancel: CancelButton := TBitBtn(Dialog.Components[I]);
        mrYes: SettingsButton := TBitBtn(Dialog.Components[I]);
      end;
  try
    Check((CopyButton <> nil) and (CopyButton.Caption = 'Copy command'),
      'Missing-tool dialog has no Copy command button');
    Check((SettingsButton <> nil) and (SettingsButton.Caption = 'Open settings'),
      'Missing-tool dialog has no Open settings button');
    Check((CancelButton <> nil) and (CancelButton.Caption = 'Cancel'),
      'Missing-tool dialog has no Cancel button');
    Check(CopyButton.Width >= Dialog.Canvas.TextWidth(CopyButton.Caption),
      'Missing-tool dialog clips its Copy command label');
    if Stage = 43 then
    begin
      Stage := 44;
      SettingsButton.Click;
    end
    else if (Stage = 0) or (Stage = 40) then
    begin
      CopyButton.Click;
      Stage := 41;
    end
    else
    begin
      Check((Pos('ghostscript', LowerCase(Clipboard.AsText)) > 0),
        'Copy command did not put the install hint on the clipboard');
      TTimer(Sender).Enabled := False;
      CancelButton.Click;
      Stage := 42;
    end;
  except
    on E: Exception do
    begin
      ErrorText := E.Message;
      TTimer(Sender).Enabled := False;
      Dialog.ModalResult := mrCancel;
    end;
  end;
end;

procedure TestRequiredTools;
var
  Path, Expected: string;
  Tool: TRequiredTool;
  HadHint: Boolean;
begin
  Expected := GetEnvironmentVariable('TPX_RESOLVED_TOOL');
  Check(Expected <> '', 'Required-tool fixture path was not supplied');
  Path := '/stale/install/tpx-test-tool';
  Check(ResolveToolPath(Path, [ExtractFileName(Expected)]),
    'Stale configured executable was not found via PATH');
  Check(Path = Expected, 'Recovered executable path was not stored');
  Path := '/stale/install/missing';
  Check(not ResolveToolPath(Path, ['tpx-required-tool-does-not-exist-321']),
    'Missing executable was reported as available');
  ConsoleMessages := True;
  try
    for Tool := Low(TRequiredTool) to High(TRequiredTool) do
    begin
      HadHint := False;
      try
        ReportMissingTool(Tool, 'test export');
      except
        on E: ETpX_SysException do
        begin
          HadHint := True;
          Check((Pos('test export', E.Message) > 0) and
            (Pos('retry this action', E.Message) > 0),
            'CLI missing-tool hint did not explain the failed feature and recovery');
{$IFDEF DARWIN}
          Check(Pos('brew install', E.Message) > 0, 'macOS install command is missing');
{$ELSE}{$IFDEF MSWINDOWS}
          Check(Pos('https://', E.Message) > 0, 'Windows download link is missing');
{$ELSE}
          Check((Pos('sudo ', E.Message) > 0) or (Pos('package manager', E.Message) > 0),
            'Linux install hint is missing');
{$ENDIF}{$ENDIF}
        end;
      end;
      Check(HadHint, 'Missing tool did not propagate CLI failure');
    end;
  finally
    ConsoleMessages := False;
  end;
end;

procedure TestToolHintDialog;
var
  HintObserver: TToolHintObserver;
  Timer: TTimer;
  Cancelled: Boolean;
begin
  HintObserver := TToolHintObserver.Create;
  Timer := TTimer.Create(nil);
  try
    Timer.Interval := 100;
    Timer.OnTimer := HintObserver.Tick;
    Timer.Enabled := True;
    Cancelled := False;
    try
      ReportMissingTool(rtGhostscript, 'PNG/BMP export');
    except
      on E: EToolRequestCancelled do Cancelled := True;
    end;
    Check(Cancelled, 'Handled GUI hint did not cancel its caller');
    Check(HintObserver.ErrorText = '', HintObserver.ErrorText);
    Check(HintObserver.Stage = 42, 'Missing-tool dialog did not complete copy/cancel');
    OptionsForm := TOptionsForm.Create(nil);
    try
      HintObserver.Stage := 43;
      Timer.Enabled := True;
      Cancelled := False;
      try
        ReportMissingTool(rtGhostscript, 'PNG/BMP export');
      except
        on E: EToolRequestCancelled do Cancelled := True;
      end;
      Check(Cancelled, 'Open settings did not cancel the pending tool request');
      Check(HintObserver.ErrorText = '', HintObserver.ErrorText);
      Check(HintObserver.Stage = 45, 'Open settings did not open the settings dialog');
    finally
      FreeAndNil(OptionsForm);
    end;
  finally
    Timer.Free;
    HintObserver.Free;
  end;
end;

type
  TExportSaveDialog = class(TSaveDialog)
  public
    Target: string;
    function Execute: Boolean; override;
  end;
  TExportSaveMode = class(TTpXMode)
    function SaveAs: Word;
  end;

function TExportSaveDialog.Execute: Boolean;
var
  I: Integer;
  Format: TExportFormat;
begin
  FileName := Target;
  for I := 1 to DocumentSaveFormatCount + ExportFormatCount do
    if ExportFormatForFilterIndex(I, Format) and
      (Format.Kind = Ord(export_PNG)) then
    begin
      FilterIndex := I;
      Exit(True);
    end;
  raise Exception.Create('PNG save filter was not found');
end;

function TExportSaveMode.SaveAs: Word;
begin
  Result := DlgSaveDrawing(Drawing_NewFileName);
end;

procedure TestToolHintSave;
var
  OriginalDialog: TSaveDialog;
  SaveDialog: TExportSaveDialog;
  HintObserver: TToolHintObserver;
  Timer: TTimer;
  Name, OriginalPath, ResolvedPath, SourcePath, ErrorText: string;
  Contents: TStringList;
  I: Integer;
  Cancelled: Boolean;
begin
  Name := GetTempFileName(SysUtils.GetTempDir(False), 'tpx-hint-save-') + '.png';
  OriginalPath := GhostscriptPath;
  SourcePath := MainForm.TheDrawing.FileName;
  OriginalDialog := MainForm.DrawingSaveDlg;
  SaveDialog := TExportSaveDialog.Create(nil);
  HintObserver := TToolHintObserver.Create;
  Timer := TTimer.Create(nil);
  Contents := TStringList.Create;
  try
    SaveDialog.Target := Name;
    MainForm.DrawingSaveDlg := SaveDialog;
    MainForm.TheDrawing.AddObject(-1,
      TLine2D.CreateSpec(-1, Point2D(0, 0), Point2D(20, 10)));
    Timer.Interval := 100;
    Timer.OnTimer := HintObserver.Tick;
    ExportErrorPromptCount := 0;
    ResolvedPath := '';
{$IFDEF WINDOWS}
    if not ResolveToolPath(ResolvedPath, ['gswin64c.exe', 'gswin32c.exe', 'mgs.exe']) then
{$ELSE}
    if not ResolveToolPath(ResolvedPath, ['gs']) then
{$ENDIF}
    begin
      OptionsForm := TOptionsForm.Create(nil);
      try
        for I := 0 to 1 do
        begin
          GhostscriptPath := '/stale/ghostscript/gs';
          if I = 0 then HintObserver.Stage := 0
          else
          begin
            HintObserver.Stage := 43;
            Contents.Text := 'preserve existing export';
            Contents.SaveToFile(Name);
          end;
          Timer.Enabled := True;
          Check(TExportSaveMode(MainForm.EventManager.Mode).SaveAs = mrCancel,
            'Missing-tool Save As did not return cancellation');
          Check(HintObserver.ErrorText = '', HintObserver.ErrorText);
          if I = 0 then
          begin
            Check(HintObserver.Stage = 42, 'Copy/cancel did not finish');
            Check(not FileExists(Name), 'Cancelled export created output');
          end
          else
          begin
            Check(HintObserver.Stage = 45, 'Open settings did not finish');
            Contents.LoadFromFile(Name);
            Check(Trim(Contents.Text) = 'preserve existing export',
              'Open settings replaced the existing export');
          end;
          Check(ExportErrorPromptCount = 0,
            'Handled tool hint produced a second export error');
        end;
        MainForm.TheDrawing.TeXFormat := tex_none;
        MainForm.TheDrawing.PdfTeXFormat := pdftex_none;
        Check(BaseMode.SaveDocumentToPath(Name + '.tpx', 'tpx', True, False,
          ErrorText), 'Could not prepare native close fixture: ' + ErrorText);
        Contents.LoadFromFile(Name + '.tpx');
        SourcePath := MainForm.TheDrawing.FileName;
        MainForm.TheDrawing.PdfTeXFormat := pdftex_epstopdf;
        MainForm.TheDrawing.History.SetPropertiesChanged;
        MainForm.OnClose := Observer.Closing;
        Observer.CloseRequests := 0;
        ReplyButton := idButtonYes;
        PromptCount := 0;
        HintObserver.Stage := 0;
        Timer.Enabled := True;
        Cancelled := False;
        try
          MainForm.Close;
        except
          on E: EToolRequestCancelled do
          begin
            Cancelled := True;
            Application.HandleException(MainForm);
          end;
        end;
        Check(Cancelled and (HintObserver.Stage = 42),
          'Native document save did not propagate the handled tool cancellation');
        Check((PromptCount = 1) and (Observer.CloseRequests = 0) and
          not Application.Terminated and MainForm.TheDrawing.History.IsChanged and
          (MainForm.TheDrawing.ObjectsCount = 1) and
          (MainForm.TheDrawing.FileName = SourcePath),
          'Cancelling the tool request closed or discarded the dirty document');
        ErrorText := Contents.Text;
        Contents.LoadFromFile(Name + '.tpx');
        Check(Contents.Text = ErrorText,
          'Cancelled native save replaced its source document');
      finally FreeAndNil(OptionsForm) end;
    end
    else Writeln('SKIP actual Save As cancellation: Ghostscript is installed');
    Contents.Text := 'preserve existing export';
    Contents.SaveToFile(Name);
    GhostscriptPath := GetEnvironmentVariable('TPX_FAILED_TOOL');
    Check(FileExists(GhostscriptPath), 'Failing converter fixture is missing');
    Check(TExportSaveMode(MainForm.EventManager.Mode).SaveAs = mrCancel,
      'Failed converter did not return cancellation');
    Check(ExportErrorPromptCount = 1,
      'Real converter failure did not report its export error');
    Contents.LoadFromFile(Name);
    Check(Trim(Contents.Text) = 'preserve existing export',
      'Cancelled or failed export replaced the destination');
    Check(MainForm.TheDrawing.FileName = SourcePath,
      'Cancelled or failed export changed the drawing source');
  finally
    MainForm.DrawingSaveDlg := OriginalDialog;
    GhostscriptPath := OriginalPath;
    Contents.Free;
    Timer.Free;
    HintObserver.Free;
    SaveDialog.Free;
    DeleteFile(Name);
    DeleteFile(Name + '.tpx');
    DeleteFile(ChangeFileExt(Name, ''));
  end;
end;

procedure TestCheckFilePath;
var
  FilePath: string;
begin
  FilePath := GetEnvironmentVariable('TPX_FILEPATH_TOOL');
  Check(FilePath <> '', 'Test tool name was not supplied');
  Check(CheckFilePath(FilePath, 'Test tool'),
    'Executable in the first PATH component was not found');
  Check(FilePath = GetEnvironmentVariable('TPX_FILEPATH_TOOL'),
    'CheckFilePath changed the configured executable name');
end;

procedure TestClipboardFormatWidth;
begin
  Check(SizeOf(TpXClipboardFormat) = SizeOf(Pointer),
    'TpX clipboard format ID cannot hold a native format handle');
end;

procedure SendNativeKey(const Key: string);
var
  KeySender: TProcess;
  I: Integer;
begin
  KeySender := TProcess.Create(nil);
  try
    KeySender.Executable := FileSearch('xdotool', GetEnvironmentVariable('PATH'));
    Check(KeySender.Executable <> '', 'xdotool is required for the GTK shortcut test');
    KeySender.Parameters.Add('key');
    KeySender.Parameters.Add('--clearmodifiers');
    KeySender.Parameters.Add(Key);
    KeySender.Execute;
    { Deliver events while modifiers are held, as in a real event loop. }
    while KeySender.Running do
    begin
      Application.ProcessMessages;
      Sleep(1);
    end;
    Check(KeySender.ExitStatus = 0, 'xdotool failed to send ' + Key);
  finally
    KeySender.Free;
  end;
  for I := 1 to 5 do begin
    Application.ProcessMessages;
    Sleep(10);
  end;
end;

procedure SendNativeClick(X, Y: Integer);
var
  ClickSender: TProcess;
begin
  ClickSender := TProcess.Create(nil);
  try
    ClickSender.Executable := FileSearch('xdotool', GetEnvironmentVariable('PATH'));
    Check(ClickSender.Executable <> '', 'xdotool is required for the GTK shortcut test');
    ClickSender.Parameters.Add('mousemove');
    ClickSender.Parameters.Add('--sync');
    ClickSender.Parameters.Add(IntToStr(X));
    ClickSender.Parameters.Add(IntToStr(Y));
    ClickSender.Parameters.Add('click');
    ClickSender.Parameters.Add('1');
    ClickSender.Options := [poWaitOnExit];
    ClickSender.Execute;
    Check(ClickSender.ExitStatus = 0, 'xdotool failed to click the canvas');
  finally
    ClickSender.Free;
  end;
  Application.ProcessMessages;
  Sleep(20);
  Application.ProcessMessages;
end;

procedure TestEditableShortcutRouting;
var
  Line: TLine2D;
  CanvasPoint: TPoint;
begin
  { Let the startup recovery offer run before editing, as for a user. An edit
    made before the first message pass can write this session's own draft once
    the AutoSave coalescing delay has passed, which on a loaded machine happens
    before the queued offer runs and leaves its modal dialog waiting. }
  MainForm.Show;
  Application.ProcessMessages;
  Line := TLine2D.CreateSpec(-1, Point2D(0, 0), Point2D(20, 10));
  MainForm.TheDrawing.AddObject(-1, Line);

  MainForm.ComboBox6.Text := '0.2';
  MainForm.ComboBox6.SelStart := 0;
  MainForm.ComboBox6.SelLength := 0;
  MainForm.ComboBox6.SetFocus;
  Application.ProcessMessages;
  Check(MainForm.ActiveControl = MainForm.ComboBox6,
    'Editable combo was not focused before native keyboard input');

  SendNativeKey('ctrl+a');
  Check(MainForm.ComboBox6.SelText = '0.2',
    'Ctrl+A did not select text in the focused editable combo');
  Check(MainForm.TheDrawing.SelectedObjects.Count = 0,
    'Ctrl+A in the editable combo selected drawing objects');

  SendNativeKey('ctrl+c');
  Check(Clipboard.AsText = '0.2', 'Ctrl+C did not copy selected combo text');
  Check(MainForm.TheDrawing.SelectedObjects.Count = 0,
    'Ctrl+C in the editable combo dispatched the drawing Copy action');
  SendNativeKey('ctrl+shift+c');
  Check(Clipboard.AsText = '0.2',
    'Ctrl+Shift+C in the editable combo dispatched Copy as TikZ');
  Check(MainForm.TheDrawing.SelectedObjects.Count = 0,
    'Copy as TikZ in the editable combo selected drawing objects');

  SendNativeKey('ctrl+x');
  Check(MainForm.ComboBox6.Text = '', 'Ctrl+X did not cut selected combo text');
  Check(MainForm.TheDrawing.SelectedObjects.Count = 0,
    'Ctrl+X in the editable combo dispatched the drawing Cut action');
  SendNativeKey('ctrl+v');
  Check(MainForm.ComboBox6.Text = '0.2', 'Ctrl+V did not paste text into the combo');
  SendNativeKey('ctrl+z');
  Check(MainForm.TheDrawing.ObjectsCount = 1,
    'Ctrl+Z in the editable combo changed the drawing');
  SendNativeKey('ctrl+shift+z');
  Check(MainForm.TheDrawing.ObjectsCount = 1,
    'Ctrl+Shift+Z in the editable combo changed the drawing');
  Check(MainForm.TheDrawing.SelectedObjects.Count = 0,
    'Edit shortcuts in the editable combo selected drawing objects');

  MainForm.TheDrawing.SelectionAdd(Line);
  MainForm.ComboBox6.SelStart := 0;
  MainForm.ComboBox6.SelLength := Length(MainForm.ComboBox6.Text);
  MainForm.ComboBox6.SetFocus;
  Application.ProcessMessages;
  SendNativeKey('Delete');
  Check((MainForm.TheDrawing.ObjectsCount = 1) and
    (MainForm.TheDrawing.SelectedObjects.Count = 1),
    'Delete in the editable combo deleted the selected drawing object');
  Check(MainForm.ComboBox6.Text = '',
    'Delete did not erase selected text in the editable combo');

  MainForm.ComboBox6.SetFocus;
  Application.ProcessMessages;
  CanvasPoint := MainForm.LocalView.ClientToScreen(Point(20, 20));
  SendNativeClick(CanvasPoint.X, CanvasPoint.Y);
  Check(MainForm.ActiveControl = MainForm.LocalView,
    'Native canvas click did not move focus out of the editable combo');
  SendNativeKey('ctrl+a');
  Check(MainForm.TheDrawing.SelectedObjects.Count = 1,
    'Ctrl+A with canvas focus did not select the drawing object');
  SendNativeKey('ctrl+c');
  Check(Clipboard.HasFormat(TpXClipboardFormat),
    'Ctrl+C with canvas focus did not copy the drawing object');
  SendNativeKey('Delete');
  Check(MainForm.TheDrawing.ObjectsCount = 0,
    'Delete with canvas focus did not delete the selected drawing object');
end;

{$I FineTransformShortcutScenarios.inc}

procedure TestCanvasFocusTransfer;
begin
  MainForm.Show;
  Application.ProcessMessages;
  MainForm.ComboBox6.SetFocus;
  Application.ProcessMessages;
  Check(MainForm.ActiveControl = MainForm.ComboBox6,
    'Editable combo was not focused before the canvas mouse message');

  MainForm.LocalView.Perform(LM_LBUTTONDOWN, MK_LBUTTON, 0);
  Application.ProcessMessages;
  Check(MainForm.ActiveControl = MainForm.LocalView,
    'LCL canvas mouse-down did not transfer focus from the editable combo');
end;

procedure CheckPlatformActionShortcut(Action: TAction; Key: Word;
  Shift: TShiftState; const ActionName: string);
var
  Expected: TShortCut;
  DecodedKey: Word;
  DecodedShift: TShiftState;
begin
{$IFDEF DARWIN}
  Include(Shift, ssMeta);
{$ELSE}
  Include(Shift, ssCtrl);
{$ENDIF}
  Expected := Menus.ShortCut(Key, Shift);
  Check(Action.ShortCut = Expected, ActionName + ' shortcut uses the wrong modifier');
  ShortCutToKey(Action.ShortCut, DecodedKey, DecodedShift);
  Check((DecodedKey = Key) and (DecodedShift = Shift),
    ActionName + ' shortcut does not dispatch its displayed key and modifier');
end;

function DispatchActionShortcut(Key: Word; Modifiers: TShiftState): Boolean;
var
  Message: TLMKey;
begin
  ShortcutModifiers := Modifiers;
  ShortcutModifierSimulation := True;
  FillChar(Message, SizeOf(Message), 0);
  Message.CharCode := Key;
  try
    Result := MainForm.ActionList1.IsShortCut(Message);
  finally
    ShortcutModifierSimulation := False;
    ShortcutModifiers := [];
  end;
end;

procedure TestPlatformShortcuts;
var
  ExpectedCtrlW: TShortCut;
  Line: TLine2D;
  WrongModifier: TShiftState;
  RightModifier: TShiftState;
  TableWindow: TTableForm;
begin
  CheckPlatformActionShortcut(MainForm.Undo, Ord('Z'), [], 'Undo');
  CheckPlatformActionShortcut(MainForm.Redo, Ord('Z'), [ssShift], 'Redo');
  CheckPlatformActionShortcut(MainForm.ClipboardCopy, Ord('C'), [], 'Copy');
  CheckPlatformActionShortcut(MainForm.CopyAsTikZ, Ord('C'), [ssShift],
    'Copy as TikZ');
  CheckPlatformActionShortcut(MainForm.ClipboardCut, Ord('X'), [], 'Cut');
  CheckPlatformActionShortcut(MainForm.ClipboardPaste, Ord('V'), [], 'Paste');
  CheckPlatformActionShortcut(MainForm.SelectAll, Ord('A'), [], 'Select All');
  CheckPlatformActionShortcut(MainForm.NewDoc, Ord('N'), [], 'New');
  CheckPlatformActionShortcut(MainForm.OpenDoc, Ord('O'), [], 'Open');
  CheckPlatformActionShortcut(MainForm.SaveDoc, Ord('S'), [], 'Save');
  CheckPlatformActionShortcut(MainForm.SaveAs, Ord('S'), [ssShift], 'Save As');
  CheckPlatformActionShortcut(MainForm.Print, Ord('P'), [], 'Print');
  Check(MainForm.Undo1.ShortCut = MainForm.Undo.ShortCut,
    'Undo menu item does not show the action shortcut');
  ExpectedCtrlW := Menus.ShortCut(Ord('W'), [ssCtrl]);
  Check(MainForm.NewWindow.ShortCut = ExpectedCtrlW,
    'New Window must keep Ctrl+W because Command+W means Close Window on macOS');
  Check(MainForm.MoveUpPixel.ShortCut = Menus.ShortCut(VK_UP, [ssCtrl]),
    'Specialized Ctrl+Up movement shortcut was changed');

  Line := TLine2D.CreateSpec(-1, Point2D(0, 0), Point2D(10, 10));
  MainForm.TheDrawing.AddObject(-1, Line);
{$IFDEF DARWIN}
  WrongModifier := [ssCtrl];
  RightModifier := [ssMeta];
{$ELSE}
  WrongModifier := [ssMeta];
  RightModifier := [ssCtrl];
{$ENDIF}
  Check(not DispatchActionShortcut(Ord('A'), WrongModifier),
    'Select All dispatched with the wrong platform modifier');
  Check(MainForm.TheDrawing.SelectedObjects.Count = 0,
    'Wrong modifier changed drawing selection');
  Check(DispatchActionShortcut(Ord('A'), RightModifier),
    'Select All did not dispatch with the platform modifier');
  Check(MainForm.TheDrawing.SelectedObjects.Count = 1,
    'Select All shortcut did not select the drawing object');

  TableWindow := TTableForm.Create(nil);
  try
    CheckPlatformActionShortcut(TableWindow.Copy, Ord('C'), [], 'Table Copy');
    CheckPlatformActionShortcut(TableWindow.Paste, Ord('V'), [], 'Table Paste');
    CheckPlatformActionShortcut(TableWindow.SelectAll, Ord('A'), [], 'Table Select All');
  finally
    TableWindow.Free;
  end;
end;

procedure TestCopyAsTikZ;
var
  Drawing: TDrawing2D;
  Line: TPolyline2D;
  Rectangle: TRectangle2D;
  TeXLabel, UnicodeLabel: TText2D;
  Bitmap: TBitmap2D;
  ClipboardText, OutputName, FileNameBefore: string;
  OutputStream: TFileStream;
  DirtyBefore, CanUndoBefore, CanRedoBefore: Boolean;
  CountBefore, SelectionBefore: Integer;
begin
  Drawing := MainForm.TheDrawing;
  Drawing.Clear;
  Drawing.LineWidthBase := 0.5;
  Line := TPolyline2D.CreateSpec(-1,
    [Point2D(1.125, 2.375), Point2D(5.625, 8.875)]);
  Line.LineStyle := liSolid;
  Line.LineColor := clBlack;
  Drawing.AddObject(-1, Line);
  Rectangle := TRectangle2D.Create(-1);
  Rectangle.Points[0] := Point2D(10.125, 12.375);
  Rectangle.Points[1] := Point2D(14.75, 18.625);
  Rectangle.Points[2] := Point2D(10.125, 18.625);
  Rectangle.LineStyle := liSolid;
  Rectangle.LineColor := clBlack;
  Rectangle.FillColor := clBlue;
  Drawing.AddObject(-1, Rectangle);
  TeXLabel := TText2D.CreateSpec(-1, Point2D(2.75, 3.125),
    Drawing.DefaultFontHeight, '$x^2$');
  TeXLabel.TeXText := '$x^2$';
  TeXLabel.Rot := 0.37;
  Drawing.AddObject(-1, TeXLabel);
  UnicodeLabel := TText2D.CreateSpec(-1, Point2D(3.5, 4.25),
    Drawing.DefaultFontHeight, UTF8Encode(UnicodeString(WideChar($03B2))));
  Drawing.AddObject(-1, UnicodeLabel);

  Clipboard.AsText := 'preserve this clipboard text';
  CountBefore := Drawing.ObjectsCount;
  SelectionBefore := Drawing.SelectedObjects.Count;
  DirtyBefore := Drawing.History.IsChanged;
  CanUndoBefore := Drawing.History.CanUndo;
  CanRedoBefore := Drawing.History.CanRedo;
  FileNameBefore := string(Drawing.FileName);
  MainForm.CopyAsTikZ.Execute;
  ClipboardText := Clipboard.AsText;
  Check(Pos('% Generated from the current TpX drawing', ClipboardText) = 1,
    'Copy as TikZ did not put the generated fragment on the native clipboard');
  Check((Pos('\usepackage{tikz}', ClipboardText) > 0) and
    (Pos('1.125mm', ClipboardText) > 0) and
    (Pos('2.375mm', ClipboardText) > 0),
    'Copied TikZ omitted prerequisites or source-precision fractional coordinates');
  Check((Pos('rotate=', ClipboardText) > 0) and
    (Pos('$x^2$', ClipboardText) > 0) and
    (Pos(UTF8Encode(UnicodeString(WideChar($03B2))), ClipboardText) > 0),
    'Copied TikZ lost rotated TeX text or plain Unicode text');
  Check((Pos('\begin{tikzpicture}', ClipboardText) > 0) and
    (Pos('\end{tikzpicture}', ClipboardText) > 0),
    'Copied TikZ is not a complete picture fragment');
  Check((Drawing.ObjectsCount = CountBefore) and
    (Drawing.SelectedObjects.Count = SelectionBefore) and
    (Drawing.History.IsChanged = DirtyBefore) and
    (Drawing.History.CanUndo = CanUndoBefore) and
    (Drawing.History.CanRedo = CanRedoBefore) and
    (string(Drawing.FileName) = FileNameBefore),
    'Copy as TikZ changed drawing, selection, history, or document identity');

  OutputName := GetEnvironmentVariable('TPX_COPY_TIKZ_OUTPUT');
  if OutputName <> '' then
  begin
    OutputStream := TFileStream.Create(OutputName, fmCreate);
    try
      if ClipboardText <> '' then
        OutputStream.WriteBuffer(ClipboardText[1], Length(ClipboardText));
    finally
      OutputStream.Free;
    end;
  end;

  Drawing.Clear;
  Clipboard.AsText := 'empty drawing sentinel';
  MainForm.CopyAsTikZ.Execute;
  ClipboardText := Clipboard.AsText;
  Check((Pos('\begin{tikzpicture}', ClipboardText) > 0) and
    (Pos('\end{tikzpicture}', ClipboardText) > 0) and
    (Pos('\draw', ClipboardText) = 0) and
    (Pos('\fill', ClipboardText) = 0) and
    (Pos('\node', ClipboardText) = 0),
    'Empty drawing did not copy a complete empty picture');

  Bitmap := TBitmap2D.Create(-1);
  Drawing.AddObject(-1, Bitmap);
  Clipboard.AsText := 'unsupported asset sentinel';
  CountBefore := Drawing.ObjectsCount;
  DirtyBefore := Drawing.History.IsChanged;
  CanUndoBefore := Drawing.History.CanUndo;
  CanRedoBefore := Drawing.History.CanRedo;
  MainForm.CopyAsTikZ.Execute;
  Check(Clipboard.AsText = 'unsupported asset sentinel',
    'Unsupported bitmap asset partially replaced the clipboard');
  Check((Pos('bitmap', LowerCase(MainForm.StatusBar1.Panels[0].Text)) > 0) and
    (Drawing.ObjectsCount = CountBefore) and
    (Drawing.History.IsChanged = DirtyBefore) and
    (Drawing.History.CanUndo = CanUndoBefore) and
    (Drawing.History.CanRedo = CanRedoBefore),
    'Unsupported bitmap was not reported or changed drawing history');
end;

procedure TestColorBoxCustomState;
var
  ComboBox: TComboBox;
  InitialCount: Integer;
  CustomColor: TColor;
begin
  ComboBox := MainForm.ComboBox3;
  InitialCount := ComboBox.Items.Count;
  CustomColor := TColor($00123456);
  ColorBoxSet(ComboBox, CustomColor);
  Check(ComboBox.Items.Count = InitialCount,
    'Choosing a custom color grew the item list');
  Check(ComboBox.Items[ComboBox.ItemIndex] = 'Current color',
    'Custom color selection did not leave the Custom command available');
  Check(ColorBoxGet(ComboBox) = CustomColor,
    'Custom color selection changed its value');
  ColorBoxSet(ComboBox, CustomColor);
  Check(ComboBox.Items.Count = InitialCount,
    'Repeating a custom color grew the item list');
  Check(ComboBox.Items[ComboBox.ItemIndex] = 'Current color',
    'Repeated custom color selection collided with its command');
  Check(ColorBoxGet(ComboBox) = CustomColor,
    'Repeated custom color selection changed its value');
  ColorBoxSet(ComboBox, clRed);
  Check((ColorBoxGet(ComboBox) = clRed) and
    (ComboBox.Items[ComboBox.ItemIndex] = 'red'),
    'Selecting a named color changed its value');
  ColorBoxSet(ComboBox, clWhite);
  Check((ColorBoxGet(ComboBox) = clWhite) and
    (ComboBox.Items[ComboBox.ItemIndex] = 'white'),
    'Selecting the last named color lost its preset');
  ColorBoxSet(ComboBox, clBlack);
  Check((ColorBoxGet(ComboBox) = clBlack) and
    (ComboBox.Items[ComboBox.ItemIndex] = 'black'),
    'Selecting black lost its preset');
  ColorBoxSet(ComboBox, clDefault);
  Check(ColorBoxGet(ComboBox) = clDefault,
    'Selecting Default changed its value');
end;

procedure TestClipboardRoundTrip;
var
  Source, Target: TDrawing2D;
  Line: TLine2D;
begin
  Source := TDrawing2D.Create(nil);
  Target := TDrawing2D.Create(nil);
  try
    Source.AddObject(-1, TLine2D.CreateSpec(-1,
      Point2D(12.5, 34.25), Point2D(56.75, 78.125)));
    Source.SelectAll;
    Source.CopySelectionToClipboard;
    Check(ClipboardHasTpX, 'TpX format is absent after copying selection');
    Target.PasteFromClipboard;
    Check(Target.ObjectsCount = 1, 'Clipboard paste lost selected line');
    Line := Target.GetObject(0) as TLine2D;
    Check(IsSamePoint2D(Line.Points[0], Point2D(12.5, 34.25)),
      'Clipboard paste changed the selected line start');
    Check(IsSamePoint2D(Line.Points[1], Point2D(56.75, 78.125)),
      'Clipboard paste changed the selected line end');
  finally
    Target.Free;
    Source.Free;
  end;
end;

procedure TObserver.RunClipboardScenarioInEventLoop(Sender: TObject);
begin
  ClipboardScenarioTimer.Enabled := False;
  try
    if ParamStr(1) = 'copy-as-tikz' then
      TestCopyAsTikZ
    else
      TestClipboardRoundTrip;
  except
    on E: Exception do ClipboardScenarioError := E.Message;
  end;
  Application.Terminate;
end;

procedure TestShapeSnap;
var
  Drawing: TDrawing2D;
  Group: TGroup2D;
  RectObj: TRectangle2D;
  Hidden, Drawn: TLine2D;
  P, Start, Finish: TPoint2D;
begin
  Drawing := MainForm.TheDrawing;
  MainForm.LocalView.GridStep := 10;
  MainForm.LocalView.ShowGrid := True;
  MainForm.LocalView.VisualRect := Rect2D(0, 0,
    MainForm.LocalView.ClientWidth, MainForm.LocalView.ClientHeight);
  Drawing.AddObject(-1, TLine2D.CreateSpec(-1, Point2D(0, 0), Point2D(10, 10)));
  Drawing.AddObject(-1, TLine2D.CreateSpec(-1, Point2D(0, 1), Point2D(10.5, 10)));
  UseSnap := False;
  UseShapeSnap := False;
  P := MainForm.LocalView.GetSnappedPoint(Point2D(10.4, 10.2));
  Check(IsSamePoint2D(P, Point2D(10.4, 10.2)), 'Disabled shape snap moved a point');
  MainForm.UserEventExecute(MainForm.SnapToShapes);
  Check(UseShapeSnap and MainForm.SnapToShapes.Checked, 'Shape snap action failed');
  UseSnap := True;
  P := MainForm.LocalView.GetSnappedPoint(Point2D(10.4, 10.2));
  Check(IsSamePoint2D(P, Point2D(10.5, 10)), 'Nearest endpoint did not beat the grid');
  P := MainForm.LocalView.GetSnappedPoint(Point2D(103, 99));
  Check(IsSamePoint2D(P, Point2D(100, 100)), 'Grid fallback changed');
  UseSnap := False;
  Group := TGroup2D.CreateSpec(-1, [TLine2D.CreateSpec(-1,
    Point2D(30, 30), Point2D(32, 30))]);
  Drawing.AddObject(-1, Group);
  P := MainForm.LocalView.GetSnappedPoint(Point2D(32.2, 30));
  Check(IsSamePoint2D(P, Point2D(32, 30)), 'Grouped endpoint did not snap');
  RectObj := TRectangle2D.Create(-1);
  RectObj.Points[0] := Point2D(50, 50);
  RectObj.Points[1] := Point2D(70, 60);
  RectObj.Points[2] := Point2D(50, 60);
  Drawing.AddObject(-1, RectObj);
  P := MainForm.LocalView.GetSnappedPoint(Point2D(70.2, 50.2));
  Check(IsSamePoint2D(P, Point2D(70, 50)), 'Implicit rectangle corner did not snap');
  Hidden := TLine2D.CreateSpec(-1, Point2D(90, 90), Point2D(95, 95));
  Hidden.Visible := False;
  Drawing.AddObject(-1, Hidden);
  P := MainForm.LocalView.GetSnappedPoint(Point2D(95.2, 95));
  Check(IsSamePoint2D(P, Point2D(95.2, 95)), 'Hidden endpoint attracted a point');
  Hidden.Visible := True;
  Hidden.Layer := 1;
  Drawing.Layers[1].Visible := False;
  P := MainForm.LocalView.GetSnappedPoint(Point2D(95.2, 95));
  Check(IsSamePoint2D(P, Point2D(95.2, 95)), 'Hidden layer attracted a point');
  MainForm.LocalView.ZoomViewCenter(0.1);
  P := MainForm.LocalView.GetSnappedPoint(Point2D(10.5, 10.8));
  Check(IsSamePoint2D(P, Point2D(10.5, 10.8)), 'Snap tolerance grew beyond six pixels');
  P := MainForm.LocalView.GetSnappedPoint(Point2D(10.5, 10.4));
  Check(IsSamePoint2D(P, Point2D(10.5, 10)), 'Snap tolerance shrank below six pixels');
  MainForm.LocalView.VisualRect := Rect2D(0, 0,
    MainForm.LocalView.ClientWidth, MainForm.LocalView.ClientHeight);
  Start := MainForm.LocalView.ViewportToScreen(Point2D(30, 30));
  Finish := MainForm.LocalView.ViewportToScreen(Point2D(70, 50));
  MainForm.EventManager.SendMessage(Msg_InsertLine, nil);
  MainForm.EventManager.MouseDown(nil, mbLeft, [], Round(Start.X), Round(Start.Y)+1);
  MainForm.EventManager.MouseUp(nil, mbLeft, [], Round(Start.X), Round(Start.Y)+1);
  MainForm.EventManager.MouseDown(nil, mbLeft, [], Round(Finish.X), Round(Finish.Y)+1);
  MainForm.EventManager.MouseUp(nil, mbLeft, [], Round(Finish.X), Round(Finish.Y)+1);
  Drawn := MainForm.TheDrawing.ObjectList.LastObj as TLine2D;
  Check(MainForm.TheDrawing.SelectionFirst = Drawn, 'Release selected an older object');
  Check(IsSamePoint2D(Drawn.Points[0], Point2D(30, 30)), Format('Drawn start did not snap: %.4f, %.4f; enabled=%s',
    [Drawn.Points[0].X, Drawn.Points[0].Y, BoolToStr(UseShapeSnap, True)]));
  Check(IsSamePoint2D(Drawn.Points[1], Point2D(70, 50)), 'Drawn finish did not snap');
end;

procedure TestDrawing(const Scenario: string);
var
  Frame: TBitmap;
  Prim: TPrimitive2D;
  Start, Finish: TPoint2D;
begin
  UseSnap := False;
  if Scenario = 'draw-cancel' then begin
    MainForm.Show;
    PumpEvents;
  end;
  MainForm.LocalView.VisualRect := Rect2D(0, 0,
    MainForm.LocalView.ClientWidth, MainForm.LocalView.ClientHeight);
  Start := MainForm.LocalView.ScreenToViewport(Point2D(40, 50));
  Finish := MainForm.LocalView.ScreenToViewport(Point2D(160, 140));
  if Scenario = 'draw-rectangle' then
    MainForm.EventManager.SendMessage(Msg_InsertRectangle, nil)
  else MainForm.EventManager.SendMessage(Msg_InsertLine, nil);
  MainForm.EventManager.MouseDown(nil, mbRight, [], 20, 30);
  MainForm.EventManager.MouseUp(nil, mbRight, [], 20, 30);
  Check(MainForm.TheDrawing.ObjectsCount = 0, 'Right-click created an object');
  MainForm.EventManager.MouseDown(nil, mbLeft, [], 40, 50);
  if Scenario = 'draw-drag' then begin
    MainForm.EventManager.MouseMove(nil, [ssLeft], 160, 140);
    MainForm.EventManager.MouseUp(nil, mbLeft, [], 160, 140);
  end
  else begin
    if Scenario = 'draw-jitter' then
      MainForm.EventManager.MouseUp(nil, mbLeft, [], 41, 51)
    else MainForm.EventManager.MouseUp(nil, mbLeft, [], 40, 50);
    Check(MainForm.TheDrawing.ObjectsCount = 0, 'First click committed an object');
    MainForm.EventManager.MouseMove(nil, [], 160, 140);
    if Scenario = 'draw-cancel' then begin
      MainForm.LocalView.ShowGrid := False;
      MainForm.LocalView.ShowCrossHair := False;
      Frame := TBitmap.Create;
      try
        Frame.SetSize(MainForm.LocalView.ClientWidth, MainForm.LocalView.ClientHeight);
        MainForm.LocalView.RenderToCanvas(Frame.Canvas);
        Check(ColorToRGB(Frame.Canvas.Pixels[100, 95]) <> clWhite,
          'Insertion preview is absent before cancellation');
        MainForm.EventManager.SendMessage(Msg_Escape, nil);
        MainForm.LocalView.RenderToCanvas(Frame.Canvas);
        Check(ColorToRGB(Frame.Canvas.Pixels[100, 95]) = clWhite,
          'Cancellation retained the insertion preview');
        Check(MainForm.TheDrawing.ObjectsCount = 0, 'Cancel committed a shape');
        Check(MainForm.EventManager.Mode = BaseMode, 'Cancel retained insertion mode');
      finally
        Frame.Free;
      end;
      Exit;
    end;
    MainForm.EventManager.MouseDown(nil, mbLeft, [], 160, 140);
    MainForm.EventManager.MouseUp(nil, mbLeft, [], 160, 140);
  end;
  Check(MainForm.TheDrawing.ObjectsCount = 1, 'Drawing did not create one object');
  Prim := MainForm.TheDrawing.ObjectList.FirstObj as TPrimitive2D;
  Check(IsSamePoint2D(Prim.Points[0], Start), 'Drawing changed first point');
  Check(IsSamePoint2D(Prim.Points[1], Finish), 'Drawing changed second point');
  if Scenario = 'draw-rectangle' then
    Check(Prim is TRectangle2D, 'Rectangle tool created the wrong shape');
  Check(MainForm.EventManager.Mode = BaseMode, 'Finished drawing retained insertion mode');
end;

procedure TestDefaultView;
var Scale: TRealType;
begin
  MainForm.Show;
  Application.ProcessMessages;
  Check(Abs(MainForm.LocalView.PixelSize - 25.4 / Screen.PixelsPerInch) < 0.001,
    'Default view is not at physical screen scale');
  Check(Abs(MainForm.TheDrawing.LineWidthBase - 0.3) < 0.001,
    'Default view changed exported line width');
  MainForm.LocalView.ZoomViewCenter(0.5);
  Scale := MainForm.LocalView.PixelSize;
  MainForm.FormShow(MainForm);
  Application.ProcessMessages;
  Check(Abs(MainForm.LocalView.PixelSize - Scale) < 0.0001,
    'Repeated form show changed the chosen zoom');
end;

procedure TestToolbar;
var
  Bitmap: TBitmap;
  I, J: Integer;
  Combo: TComboBox;
begin
  MainForm.Font.Size := 16;
  MainForm.FormShow(MainForm);
  Bitmap := TBitmap.Create;
  try
    for I := 0 to MainForm.ComponentCount - 1 do
      if MainForm.Components[I] is TComboBox then begin
        Combo := MainForm.Components[I] as TComboBox;
        if not ((Combo.Parent = MainForm.PropertiesToolbar1) or
          (Combo.Parent = MainForm.PropertiesToolbar2)) then Continue;
        Bitmap.Canvas.Font.Assign(Combo.Font);
        for J := 0 to Combo.Items.Count - 1 do
          Check(Combo.Width >= Bitmap.Canvas.TextWidth(Combo.Items[J]) + 12,
            Combo.Name + ' clips ' + Combo.Items[J]);
      end;
    Bitmap.Canvas.Font.Assign(MainForm.Edit4.Font);
    Check(MainForm.Edit4.Width >= Bitmap.Canvas.TextWidth('-999.99') + 8,
      'Font-height entry clips numeric values');
  finally
    Bitmap.Free;
  end;
end;

procedure TestTextSelection;
var
  TextObj: TText2D;
  Box: TRect2D;
  Center: TPoint2D;
  Distance: TRealType;
  Alignment: THAlignment;
begin
  TextObj := TText2D.CreateSpec(-1, Point2D(10, 20), 5, 'Selectable text');
  try
    for Alignment := Low(THAlignment) to High(THAlignment) do begin
      TextObj.HAlignment := Alignment;
      TextObj.UpdateExtension(TextObj);
      Box := TextObj.BoundingBox;
      Center := BoxCenter(Box);
      Check(TextObj.OnMe(Center, 0.05, Distance) = PICK_INOBJECT,
        'Text center is not selectable');
      Check(TextObj.OnMe(Point2D(Box.Right + 0.02, Center.Y), 0.05, Distance)
        = PICK_INOBJECT, 'Text edge lacks selection tolerance');
      Check(TextObj.OnMe(Point2D(Box.Right + 1, Center.Y), 0.05, Distance)
        = PICK_NOOBJECT, 'Text selection extends too far');
      Check(TextObj.OnMe(TextObj.Points[0], 0.05, Distance) = 0,
        'Text insertion anchor lost priority');
    end;
    TextObj.Rot := Pi / 4;
    TextObj.UpdateExtension(TextObj);
    Check(TextObj.OnMe(BoxCenter(TextObj.BoundingBox), 0.05, Distance)
      = PICK_INOBJECT, 'Rotated text center is not selectable');
  finally
    TextObj.Free;
  end;
end;

procedure TestStartupFileName;
var
  Expected: string;
  ExpectedCount: Integer;
  Line: TLine2D;
  Marker: string;
begin
  Expected := GetEnvironmentVariableUTF8('TPX_STARTUP_EXPECTED');
  ExpectedCount := StrToInt(GetEnvironmentVariableUTF8('TPX_STARTUP_OBJECTS'));
  Check(MainForm.TheDrawing.FileName = Expected,
    'Startup document filename differs: ' + MainForm.TheDrawing.FileName);
  if ExpectedCount = 0 then
    Check(MainForm.Caption = Expected, 'New startup document title differs')
  else Check(MainForm.Caption = ExtractFileName(Expected), 'Loaded startup title differs');
  Check(MainForm.TheDrawing.ObjectsCount = ExpectedCount,
    'Startup loaded an unexpected object count');
  Check(not MainForm.TheDrawing.History.IsChanged, 'Startup document is dirty');
  if ExpectedCount = 1 then begin
    Line := MainForm.TheDrawing.GetObject(0) as TLine2D;
    Check((Line.Points[1].X = 20) and (Line.Points[1].Y = 10),
      'Startup changed loaded line geometry');
  end;
  Marker := GetEnvironmentVariableUTF8('TPX_STARTUP_MARKER');
  if Marker <> '' then
    Check(WriteUTF8Marker(Marker, 'PASS'), 'Could not write startup marker');
end;

procedure TestOSDocumentOpen;
var
  FirstPath, SecondPath: string;
  Marker: string;
  Deadline: QWord;
  DraftText: TEditUtf8Text;
  ErrorText: string;
  TextObj: TText2D;
  Session: TObjectEditSession;
  Epoch: QWord;
begin
  FirstPath := GetEnvironmentVariableUTF8('TPX_OS_OPEN_FIRST');
  SecondPath := GetEnvironmentVariableUTF8('TPX_OS_OPEN_SECOND');
  Marker := GetEnvironmentVariableUTF8('TPX_STARTUP_MARKER');
  Check((FirstPath <> '') and (SecondPath <> '') and (Marker <> ''),
    'OS open fixture paths or child marker are missing');
  Check(SameDocumentPath(TDocumentPath(SecondPath),
    TDocumentPath(MainForm.TheDrawing.FileName)),
    'Initial process did not open the requested second document');

  TextObj := TText2D.CreateSpec(-1, Point2D(2, 3), 7, 'OS open draft');
  MainForm.TheDrawing.AddObject(-1, TextObj);
  MainForm.TheDrawing.History.Save;
  MainForm.TheDrawing.History.SaveCheckSum;
  Epoch := MainForm.CurrentDocumentEpoch;
  Session := TObjectEditSession.Create(MainForm.TheDrawing, Epoch,
    [TextObj.ID]);
  try
    Check(MainForm.RegisterEditSession(Session),
      'OS-open draft session did not register');
    Check(Session.TrySetRawDraft(opfTextHeight, '1e', ErrorText),
      'OS-open invalid raw draft setup failed: ' + ErrorText);
    MainForm.IntfDropFiles([FirstPath, SecondPath]);
    Check(SameDocumentPath(TDocumentPath(SecondPath),
      TDocumentPath(MainForm.TheDrawing.FileName)),
      'OS open replaced the active document while its edit draft was invalid');
    Check(Session.IsActive and Session.HasRawDraft(opfTextHeight, DraftText) and
      (DraftText = '1e') and (TextObj.Height = 7),
      'OS open discarded or committed the invalid edit draft');
    Check(not FileExistsUTF8(Marker),
      'OS open started another window while its edit draft was invalid');
    Check(Session.Cancel(Epoch), 'OS-open invalid draft could not be canceled');
  finally
    MainForm.UnregisterEditSession(Session);
    Session.Free;
  end;

  MainForm.IntfDropFiles([FirstPath, SecondPath]);
  Check(SameDocumentPath(TDocumentPath(FirstPath),
    TDocumentPath(MainForm.TheDrawing.FileName)),
    'The running process did not open its first requested document');
  Deadline := GetTickCount64 + 10000;
  while not FileExistsUTF8(Marker) and (GetTickCount64 < Deadline) do Sleep(10);
  Check(FileExistsUTF8(Marker),
    'The second requested document did not open in a new process');
end;

{$IFDEF DARWIN}
procedure SeedLegacyMacSettings;
var
  FileName: string;
  Strings: TStringList;
begin
  FileName := IncludeTrailingPathDelimiter(GetAppConfigDir(False)) + 'TpX.ini';
  if not ForceDirectories(ExtractFileDir(FileName)) then
    raise Exception.Create('Could not create legacy settings directory');
  Strings := TStringList.Create;
  try
    Strings.Add('LineWidth_Default=2.25');
    Strings.SaveToFile(FileName);
  finally
    Strings.Free;
  end;
  WriteLn('LEGACY_SETTINGS_FILE=', FileName);
end;

procedure TestMacSettings;
var
  ConfigFile, BundleFile, ExpectedLegacy: string;
  ExpectedWidth: Double;
begin
  ConfigFile := IncludeTrailingPathDelimiter(UserStorageDirectory(usaConfig)) +
    'TpX.ini';
  BundleFile := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'TpX.ini';
  ExpectedLegacy := GetEnvironmentVariable('TPX_SETTINGS_EXPECT_LEGACY');
  if ExpectedLegacy <> '' then
  begin
    ExpectedWidth := StrToFloat(ExpectedLegacy);
    Check(Abs(LineWidthBase_Default - ExpectedWidth) < 0.000001,
      'Legacy bundle settings were not loaded');
  end
  else
    Check(not FileExists(BundleFile), 'Fresh bundle unexpectedly contains settings');

  Check(LiveTeXEnabled, 'Live LaTeX preview must default to enabled');
  SetLiveTeXEnabled(False);
  LineWidthBase_Default := 2.75;
  SaveSettings;
  Check(FileExists(ConfigFile), 'User settings file was not created');
  Check(ExpandFileName(ConfigFile) <> ExpandFileName(BundleFile),
    'User settings path points into the application bundle');
  if ExpectedLegacy = '' then
    Check(not FileExists(BundleFile), 'Saving wrote settings beside the application');

  SetLiveTeXEnabled(True);
  LineWidthBase_Default := 4.25;
  LoadSettings;
  Check(not LiveTeXEnabled, 'Live LaTeX preview preference did not persist');
  Check(Abs(LineWidthBase_Default - 2.75) < 0.000001,
    'Saved user setting did not reload');
  WriteLn('SETTINGS_FILE=', ConfigFile);
end;

procedure TestMacResourcePaths;
var
  Files: array[0..1] of string;
  I: Integer;
  ResourcePath, UserPath, HelpPath, ExamplePath: string;
  ResourceContents, UserContents: string;
  Strings: TStringList;
  ExpectBundle: Boolean;
  ExeDir: string;
  function ReadText(const FileName: string): string;
  var
    TextFile: TStringList;
  begin
    TextFile := TStringList.Create;
    try
      TextFile.LoadFromFile(FileName);
      Result := TextFile.Text;
    finally
      TextFile.Free;
    end;
  end;
begin
  ExpectBundle := GetEnvironmentVariable('TPX_RESOURCE_EXPECT_BUNDLE') = '1';
  ExeDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  Files[0] := 'preview.tex.inc';
  Files[1] := 'metapost.tex.inc';
  for I := Low(Files) to High(Files) do
  begin
    ResourcePath := TpXResourcePath(Files[I]);
    Check(FileExists(ResourcePath), 'Bundled template was not found: ' + ResourcePath);
    if ExpectBundle then
      Check(Pos('/Contents/Resources/', ResourcePath) > 0,
        'Template did not resolve under Contents/Resources: ' + ResourcePath)
    else
      Check(ExpandFileName(ResourcePath) = ExpandFileName(ExeDir + Files[I]),
        'Bare executable did not use its adjacent legacy template');
    ResourceContents := ReadText(ResourcePath);

    UserPath := TpXTemplatePath(Files[I]);
    Check(FileExists(UserPath), 'User template was not initialized: ' + UserPath);
    Check(ExpandFileName(UserPath) <> ExpandFileName(ResourcePath),
      'Editable template points at the sealed bundled resource');
    Check(ReadText(UserPath) = ResourceContents,
      'Packaged template default was not copied to the user settings directory');
    Strings := TStringList.Create;
    try
      Strings.Text := ResourceContents + '% user customization';
      Strings.SaveToFile(UserPath);
    finally
      Strings.Free;
    end;
    UserPath := TpXTemplatePath(Files[I]);
    UserContents := ReadText(UserPath);
    Check(Pos('% user customization', UserContents) > 0,
      'A user template override was replaced on the next lookup');
    Check(ReadText(ResourcePath) = ResourceContents,
      'Looking up or editing a template modified its packaged default');
    WriteLn('TEMPLATE_PATH=', Files[I], '=', UserPath);
  end;

  HelpPath := TpXResourcePath('help/tpx_tpxabout_tpx_drawing_tool.htm');
  Check(FileExists(HelpPath), 'Bundled help document was not found: ' + HelpPath);
  if ExpectBundle then
    Check(Pos('/Contents/Resources/help/', HelpPath) > 0,
      'Help did not resolve under Contents/Resources: ' + HelpPath)
  else
    Check(ExpandFileName(HelpPath) = ExpandFileName(ExeDir +
      'help/tpx_tpxabout_tpx_drawing_tool.htm'),
      'Bare executable did not use adjacent legacy help');
  WriteLn('HELP_PATH=', HelpPath);

  ExamplePath := TpXResourcePath('examples/quadratic-function.TpX');
  Check(FileExists(ExamplePath), 'Bundled example was not found: ' + ExamplePath);
  if ExpectBundle then
    Check(Pos('/Contents/Resources/examples/', ExamplePath) > 0,
      'Example did not resolve under Contents/Resources: ' + ExamplePath)
  else
    Check(ExpandFileName(ExamplePath) = ExpandFileName(ExeDir +
      'examples/quadratic-function.TpX'),
      'Bare executable did not use its adjacent example');
  WriteLn('EXAMPLE_PATH=', ExamplePath);
end;
{$ENDIF}

procedure TestOpenDialogAllFormats;
const
  Prefix = 'All supported formats|';
var
  Filter, AllPattern: string;
begin
  Check(OpenDialog_FilterIndex = 1,
    'File Open must preselect all supported formats');
  Filter := BuildOpenFileFilter(False);
  Check(Copy(Filter, 1, Length(Prefix)) = Prefix,
    'Open filter must start with all supported formats: ' + Filter);
  AllPattern := Copy(Filter, Length(Prefix) + 1, MaxInt);
  AllPattern := ';' + Copy(AllPattern, 1, Pos('|', AllPattern) - 1) + ';';
{$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
  Check((Pos(';*.[tT][pP][xX];', AllPattern) > 0) and
    (Pos(';*.[tT][iI][kK][zZ];', AllPattern) > 0) and
    (Pos(';*.[tT][eE][xX];', AllPattern) > 0) and
    (Pos(';*.[sS][vV][gG];', AllPattern) > 0),
    'All supported formats misses a case-insensitive pattern: ' + AllPattern);
{$ELSE}
  Check((Pos(';*.tpx;', AllPattern) > 0) and (Pos(';*.tikz;', AllPattern) > 0)
    and (Pos(';*.tex;', AllPattern) > 0) and (Pos(';*.svg;', AllPattern) > 0),
    'All supported formats misses a registered format: ' + AllPattern);
{$IFEND}
end;

procedure TestOpenExamples;
const
  ExampleCaptions: array[0..2] of string =
    ('Quadratic function', 'Vector addition', 'Double-slit interference');
  ExampleFiles: array[0..2] of string =
    ('quadratic-function.TpX', 'vector-addition.TpX', 'double-slit.TpX');
  ExpectedCounts: array[0..2] of Integer = (14, 12, 25);
  ExpectedTex: array[0..2] of string =
    ('$f(x)=x^2-1$', '$\vec{A}$', '$\Delta r=0$');
var
  ExampleMenu, Item: TMenuItem;
  ExamplePath, SavedPath, ErrorText: string;
  I, J: Integer;
  FoundTex: Boolean;
  Primitive: TPrimitive2D;
  Line: TLine2D;
  Arc: TArc2D;
  TextObject: TText2D;
begin
  ExampleMenu := FindMenuCaption(MainForm.MainMenu1.Items, 'Open Example');
  Check(ExampleMenu <> nil, 'File menu has no Open Example submenu');
  Check(ExampleMenu.Count = 3, 'Open Example must contain three drawings');
  for I := Low(ExampleCaptions) to High(ExampleCaptions) do
  begin
    Item := FindMenuCaption(ExampleMenu, ExampleCaptions[I]);
    Check(Item <> nil, 'Open Example is missing ' + ExampleCaptions[I]);
    Item.Click;
    ExamplePath := TpXResourcePath('examples/' + ExampleFiles[I]);
    Check((MainForm.EventManager.DocumentSession.SourcePath = '') and
      not MainForm.EventManager.DocumentSession.CanSaveBack and
      (MainForm.TheDrawing.FileName = Drawing_NewFileName) and
      (MainForm.EventManager.RecentFiles.IndexOf(ExamplePath) < 0),
      'Open Example did not detach the bundled document before editing');
    Check(not LiveTeXDocumentTrusted,
      'Opening an example trusted its TeX labels automatically');
    Check(MainForm.TheDrawing.ObjectsCount = ExpectedCounts[I],
      ExampleCaptions[I] + ' opened with ' +
      IntToStr(MainForm.TheDrawing.ObjectsCount) + ' objects; expected ' +
      IntToStr(ExpectedCounts[I]));
    FoundTex := False;
    for J := 0 to MainForm.TheDrawing.ObjectsCount - 1 do
    begin
      Primitive := MainForm.TheDrawing.GetObject(J) as TPrimitive2D;
      Check(not (Primitive is TBitmap2D),
        ExampleCaptions[I] + ' unexpectedly linked a bitmap');
      if Primitive is TText2D then
      begin
        TextObject := Primitive as TText2D;
        if TextObject.TeXText = ExpectedTex[I] then FoundTex := True;
      end;
    end;
    Check(FoundTex, ExampleCaptions[I] + ' lost its TeX source label');

    if I = 0 then
    begin
      Line := MainForm.TheDrawing.GetObject(0) as TLine2D;
      Check(Line.EndArrowKind <> arrNone,
        'Quadratic x-axis lost its arrow');
      Check(Abs(Line.Points[0].X - 6) < 0.0001,
        'Quadratic x-axis start changed');
      Check(Abs(Line.Points[1].X - 64) < 0.0001,
        'Quadratic x-axis end changed');
    end
    else if I = 1 then
    begin
      Line := MainForm.TheDrawing.GetObject(0) as TLine2D;
      Check(Line.EndArrowKind <> arrNone,
        'Vector arrow lost its arrowhead');
      Check((Abs(Line.Points[0].X - 12) < 0.0001) and
        (Abs(Line.Points[1].X - 42) < 0.0001) and
        (Abs(Line.Points[1].Y - 12) < 0.0001),
        'Vector arrow geometry changed');
    end
    else
    begin
      Check(MainForm.TheDrawing.GetObject(5) is TArc2D,
        'Double-slit example lost its first wavefront');
      Arc := MainForm.TheDrawing.GetObject(5) as TArc2D;
      Check(Abs(Arc.Radius - 6) < 0.0001,
        'Double-slit first wavefront has the wrong radius');
      Primitive := MainForm.TheDrawing.GetObject(8) as TPrimitive2D;
      Check(Primitive.LineStyle = liDashed,
        'Double-slit wavefront styles no longer distinguish the sources');
    end;

    SavedPath := IncludeTrailingPathDelimiter(GetCurrentDir) +
      'example-roundtrip-' + IntToStr(I) + '.TpX';
    Check(BaseMode.SaveDocumentToPath(SavedPath, 'tpx', True, True,
      ErrorText), 'Could not save ' + ExampleCaptions[I] + ': ' + ErrorText);
    Check(BaseMode.OpenDocumentFromPath(SavedPath, ErrorText),
      'Could not reopen ' + ExampleCaptions[I] + ': ' + ErrorText);
    Check((MainForm.TheDrawing.ObjectsCount = ExpectedCounts[I]) and
      (MainForm.EventManager.DocumentSession.SourceFormatId = 'tpx'),
      ExampleCaptions[I] + ' did not survive native save and reopen');
  end;
end;

procedure TestExit(const Scenario: string);
var
  ModeBefore: TMode;
  Line: TLine2D;
  Saved: TDrawing2D;
  Loader: T_TpX_Loader;
  Session: TDocumentSession;
  SaveName: string;
  SaveFailed: Boolean;
  procedure AcceptPendingNativeSource(const FileName: string);
  begin
    Session.AcceptSource(FileName, 'tpx', '',
      ReadDocumentRevision(FileName), True);
  end;
begin
  PromptCount := 0;
  SaveErrorPromptCount := 0;
  ReplyButton := idButtonNo;
  SaveFailed := False;
  Session := MainForm.EventManager.DocumentSession;
  Session.Clear;
  if Scenario = 'exit-empty-cancel' then
    MainForm.TheDrawing.History.MarkDirty
  else if Scenario <> 'exit-clean' then begin
    Line := TLine2D.CreateSpec(-1, Point2D(0, 0), Point2D(20, 10));
    MainForm.TheDrawing.AddObject(-1, Line);
    MainForm.TheDrawing.History.SetPropertiesChanged;
  end;
  if (Pos('exit-cancel', Scenario) = 1) or
    (Scenario = 'exit-empty-cancel') then ReplyButton := idButtonCancel;
  if Scenario = 'exit-save-failure' then begin
    MainForm.TheDrawing.FileName := IncludeTrailingPathDelimiter(
      SysUtils.GetTempDir(False)) + 'tpx-missing-save-parent-' + IntToStr(GetProcessID) +
      PathDelim + 'drawing.tpx';
    AcceptPendingNativeSource(MainForm.TheDrawing.FileName);
    ReplyButton := idButtonYes;
  end;
  SaveName := '';
  if (Scenario = 'exit-yes') or (Scenario = 'exit-destroy-yes') then begin
    SaveName := GetTempFileName(SysUtils.GetTempDir(False), 'tpx-save-');
    DeleteFile(SaveName);
    SaveName := SaveName + '.tpx';
    MainForm.TheDrawing.FileName := SaveName;
    AcceptPendingNativeSource(SaveName);
    ReplyButton := idButtonYes;
  end;
  ModeBefore := MainForm.EventManager.Mode;
  if Scenario = 'exit-fallback' then MainForm.OnExit(MainForm)
  else if Pos('exit-destroy', Scenario) = 1 then begin
    MainForm.OnClose := nil;
    MainForm.Close;
    Check(Application.Terminated, 'Accepted close did not terminate the application');
    MainForm.Free;
    MainForm := nil;
  end
  else if Scenario = 'exit-save-failure' then begin
    try
      MainForm.Close;
    except
      on E: Exception do SaveFailed := True;
    end;
    if SaveErrorPromptCount = 1 then SaveFailed := True;
  end
  else if Scenario = 'exit-close' then MainForm.Close
  else MainForm.UserEventExecute(MainForm.ExitProgram);
  if Scenario = 'exit-clean' then Check(PromptCount = 0, 'Clean drawing prompted')
  else Check(PromptCount = 1, 'Expected one save prompt, got ' + IntToStr(PromptCount));
  if (Pos('exit-cancel', Scenario) = 1) or
    (Scenario = 'exit-empty-cancel') then begin
    Check(Observer.CloseRequests = 0, 'Cancel requested closing');
    Check(MainForm.EventManager.Mode = ModeBefore, 'Cancel discarded the active mode');
    Check(MainForm.TheDrawing.ObjectsCount =
      Ord(Scenario <> 'exit-empty-cancel'), 'Cancel discarded the drawing');
    Check(MainForm.TheDrawing.History.IsChanged,
      'Cancel cleared the modified state');
    if Scenario = 'exit-cancel-retry' then begin
      ReplyButton := idButtonNo;
      MainForm.OnClose := Observer.ClosingDefault;
      MainForm.Close;
      Check(Application.Terminated, 'Accepted retry did not terminate the application');
      MainForm.Free;
      MainForm := nil;
      Check(PromptCount = 2, 'Retry after Cancel did not prompt once');
      Check(Observer.CloseRequests = 1, 'Retry after Cancel did not close');
    end;
  end
  else if Scenario = 'exit-save-failure' then begin
    Check(SaveFailed, 'Failed save did not block closing');
    Check(SaveErrorPromptCount = 1,
      'Failed save did not report its error and await acknowledgment');
    Check(Observer.CloseRequests = 0, 'Failed save requested closing');
    Check(MainForm.TheDrawing.History.IsChanged,
      'Failed save cleared the modified state');
    Check(MainForm.TheDrawing.ObjectsCount = 1,
      'Failed save discarded the drawing');
    ReplyButton := idButtonNo;
    MainForm.OnClose := Observer.ClosingDefault;
    MainForm.Close;
    Check(Application.Terminated, 'Accepted retry did not terminate the application');
    MainForm.Free;
    MainForm := nil;
    Check(PromptCount = 2, 'Retry after failed save did not prompt once');
    Check(Observer.CloseRequests = 1, 'Retry after failed save did not close');
  end
  else if Pos('exit-destroy', Scenario) <> 1 then
    Check(Observer.CloseRequests = 1, 'Exit failed to request closing');
  if (Scenario = 'exit-yes') or (Scenario = 'exit-destroy-yes') then begin
    Check(FileExists(SaveName), 'Save did not write the drawing');
    if Scenario = 'exit-yes' then
      Check(not MainForm.TheDrawing.History.IsChanged,
        'Saved drawing remains modified');
    Saved := TDrawing2D.Create(nil);
    Loader := T_TpX_Loader.Create(Saved);
    try
      Loader.LoadFromFile(SaveName);
      Check(Saved.ObjectsCount = 1, 'Saved drawing lost its line');
      Line := Saved.GetObject(0) as TLine2D;
      Check((Line.Points[1].X = 20) and (Line.Points[1].Y = 10),
        'Saved line geometry changed');
    finally
      Loader.Free;
      Saved.Free;
      DeleteFile(SaveName);
      DeleteFile(ChangeFileExt(SaveName, ''));
    end;
  end;
end;

begin
  try
    RegisterTikZDocumentCodec;
    if SameText(ChangeFileExt(ExtractFileName(ParamStr(0)), ''),
      'failed-tool-fixture') then Halt(7);
    // Test-only converter copies independently defined EPS fixtures, without UI.
    if SameText(ChangeFileExt(ExtractFileName(ParamStr(0)), ''), 'sam2p-fixture') then begin
      RunBitmapFixtureConverter;
      Halt(0);
    end;
    if SameText(ChangeFileExt(ExtractFileName(ParamStr(0)), ''),
      'pstoedit-fixture') then
    begin
      RunPstoeditFixture;
      Halt(0);
    end;
{$IFDEF DARWIN}
    if ParamStr(1) = 'seed-legacy-mac-settings' then
    begin
      SeedLegacyMacSettings;
      Halt(0);
    end;
{$ENDIF}
    if SameText(ChangeFileExt(ExtractFileName(ParamStr(0)), ''), 'mpost-fixture')
      or SameText(ChangeFileExt(ExtractFileName(ParamStr(0)), ''), 'gs-fixture') then
    begin
      RunStagedToolFixture;
      Halt(7);
    end;
    if (ParamStr(1) <> 'clipboard-format-width') and
      (ParamStr(1) <> 'clipboard-roundtrip') and
      (ParamStr(1) <> 'copy-as-tikz') and
      (ParamStr(1) <> 'fine-transform-shortcuts') then
      WidgetSet := TTestWidgetSet.Create;
    Application.Initialize;
    RequireDerivedFormResource := True;
    Application.CreateForm(TMainForm, MainForm);
    Observer := TObserver.Create;
    MainForm.OnClose := Observer.Closing;
    PromptDialogFunction := AnswerPrompt;
    if (ParamStr(1) = 'copy-as-tikz') or
      (ParamStr(1) = 'clipboard-roundtrip') then
    begin
      ClipboardScenarioTimer := TTimer.Create(nil);
      ClipboardScenarioTimer.Interval := 1;
      ClipboardScenarioTimer.OnTimer := Observer.RunClipboardScenarioInEventLoop;
      ClipboardScenarioTimer.Enabled := True;
      Application.Run;
      ClipboardScenarioTimer.Free;
      if ClipboardScenarioError <> '' then
        raise Exception.Create(ClipboardScenarioError);
      WriteLn('PASS ', ParamStr(1));
      Halt(0);
    end;
    if GetEnvironmentVariableUTF8('TPX_OS_OPEN_LS_FIRST') <> '' then
    begin
      Observer.OSOpenFirst := GetEnvironmentVariableUTF8('TPX_OS_OPEN_LS_FIRST');
      Observer.OSOpenSecond := GetEnvironmentVariableUTF8('TPX_OS_OPEN_LS_SECOND');
      Observer.OSOpenMarkerFirst :=
        GetEnvironmentVariableUTF8('TPX_OS_OPEN_LS_MARKER_FIRST');
      Observer.OSOpenMarkerSecond :=
        GetEnvironmentVariableUTF8('TPX_OS_OPEN_LS_MARKER_SECOND');
      Check((Observer.OSOpenSecond <> '') and
        (Observer.OSOpenMarkerFirst <> '') and
        (Observer.OSOpenMarkerSecond <> ''),
        'LaunchServices test paths or markers are missing');
      Observer.OSOpenDeadline := GetTickCount64 + 15000;
      Observer.OSOpenTimer := TTimer.Create(nil);
      Observer.OSOpenTimer.Interval := 20;
      Observer.OSOpenTimer.OnTimer := Observer.CheckOSOpenLaunchServices;
      Observer.OSOpenTimer.Enabled := True;
      Application.Run;
      Check(Observer.OSOpenSucceeded,
        'LaunchServices did not open both requested documents');
    end
    else if ParamStr(1) = 'external-open' then TestOSDocumentOpen
    else if GetEnvironmentVariableUTF8('TPX_STARTUP_EXPECTED') <> '' then TestStartupFileName
    else if Pos('viewport-', ParamStr(1)) = 1 then TestViewport(ParamStr(1))
    else if ParamStr(1) = 'live-tex-missing-tool' then TestLiveTeXMissingTool
    else if ParamStr(1) = 'live-tex' then TestLiveTeX
    else if ParamStr(1) = 'live-tex-settings' then TestLiveTeXSettings
    else if ParamStr(1) = 'user-preferences-write' then TestUserPreferencesWrite
    else if ParamStr(1) = 'user-preferences-read' then TestUserPreferencesRead
    else if ParamStr(1) = 'storage-paths' then TestStoragePaths
    else if ParamStr(1) = 'preview-state' then TestPreviewState
    else if ParamStr(1) = 'external-tools' then TestExternalTools
    else if ParamStr(1) = 'check-file-path' then TestCheckFilePath
    else if ParamStr(1) = 'required-tools' then TestRequiredTools
    else if ParamStr(1) = 'tool-hint-dialog' then TestToolHintDialog
    else if ParamStr(1) = 'tool-hint-save' then TestToolHintSave
    else if ParamStr(1) = 'clipboard-format-width' then TestClipboardFormatWidth
    else if ParamStr(1) = 'clipboard-roundtrip' then TestClipboardRoundTrip
    else if ParamStr(1) = 'copy-as-tikz' then TestCopyAsTikZ
    else if ParamStr(1) = 'editable-shortcut-routing' then TestEditableShortcutRouting
    else if ParamStr(1) = 'fine-transform-shortcuts' then TestFineTransformShortcuts
    else if ParamStr(1) = 'canvas-focus-transfer' then TestCanvasFocusTransfer
    else if ParamStr(1) = 'platform-shortcuts' then TestPlatformShortcuts
    else if ParamStr(1) = 'document-io' then TestDocumentIO
    else if ParamStr(1) = 'pstoedit-import' then TestPstoeditEmfImport
    else if ParamStr(1) = 'auto-reload-integration' then TestAutoReloadIntegration
    else if ParamStr(1) = 'autosave-integration' then TestAutoSaveIntegration
    else if ParamStr(1) = 'autosave-source-formats' then TestAutoSaveSourceFormats
    else if ParamStr(1) = 'source-format-tikz' then TestSourceFormatTikZWorkflow
    else if ParamStr(1) = 'source-format-svg' then TestSourceFormatSVGWorkflow
    else if ParamStr(1) = 'source-format-refresh-tikz' then TestSourceFormatRefreshTikZ
    else if ParamStr(1) = 'source-format-refresh-svg' then TestSourceFormatRefreshSVG
    else if ParamStr(1) = 'source-format-lifecycle-tikz' then
      TestSourceFormatBindingBurstTikZ
    else if ParamStr(1) = 'source-format-lifecycle-svg' then
      TestSourceFormatBindingBurstSVG
    else if ParamStr(1) = 'source-format-write-failures' then
      TestSourceFormatWriteFailures
    else if ParamStr(1) = 'autosave-recovery-create' then TestAutoSaveRecoveryCreate
    else if ParamStr(1) = 'autosave-empty-recovery-create' then TestAutoSaveEmptyRecoveryCreate
    else if ParamStr(1) = 'autosave-empty-recovery-restore' then TestAutoSaveEmptyRecoveryRestore
    else if ParamStr(1) = 'autosave-unresolved-image-warning' then TestAutoSaveUnresolvedImageWarning
    else if ParamStr(1) = 'autosave-recovery-restore' then TestAutoSaveRecoveryRestore
    else if ParamStr(1) = 'autosave-named-recovery-create' then TestAutoSaveNamedRecoveryCreate
    else if ParamStr(1) = 'autosave-named-recovery-restore' then TestAutoSaveNamedRecoveryRestore
    else if ParamStr(1) = 'autosave-recovery-watch-binding' then TestAutoSaveRecoveryWatchBinding
    else if ParamStr(1) = 'edit-session-preview-cancel' then TestEditSessionPreviewCancel
    else if ParamStr(1) = 'edit-session-recovery-payload' then TestEditSessionRecoveryPayload
    else if ParamStr(1) = 'edit-session-wide-field-recovery' then TestEditSessionWideFieldRecovery
    else if ParamStr(1) = 'edit-session-nested-recovery-rebind' then TestEditSessionNestedRecoveryRebind
    else if ParamStr(1) = 'edit-session-text-whitespace' then TestEditSessionTextWhitespaceSaveRoundTrip
    else if ParamStr(1) = 'edit-session-malformed-reference' then TestEditSessionMalformedLiteralReference
    else if ParamStr(1) = 'edit-session-autosave-cancel' then TestEditSessionAutoSaveCancel
    else if ParamStr(1) = 'edit-session-recovery-disabled' then TestEditSessionRecoveryDisabled
    else if ParamStr(1) = 'edit-session-deferred-reload' then TestEditSessionDeferredExternalReload
    else if ParamStr(1) = 'edit-session-form-destroy' then TestEditSessionFormDestroy
    else if ParamStr(1) = 'nonmodal-recovery-create' then TestNonModalRecoveryCreate
    else if ParamStr(1) = 'nonmodal-recovery-restore' then TestNonModalRecoveryRestore
    else if ParamStr(1) = 'color-box-custom-state' then TestColorBoxCustomState
    else if ParamStr(1) = 'shape-snap' then TestShapeSnap
    else if Pos('draw-', ParamStr(1)) = 1 then TestDrawing(ParamStr(1))
    else if ParamStr(1) = 'default-view' then TestDefaultView
    else if ParamStr(1) = 'toolbar' then TestToolbar
    else if ParamStr(1) = 'text-selection' then TestTextSelection
    else if ParamStr(1) = 'text-metrics' then TestTextMetrics
    else if ParamStr(1) = 'path-first-click' then TestPathFirstClick
    else if ParamStr(1) = 'async-tools' then TestAsyncTools
    else if ParamStr(1) = 'default-opener' then TestDefaultOpener
    else if ParamStr(1) = 'property-dimensions' then TestPropertyDimensions
    else if ParamStr(1) = 'inspector-mixed-selection' then TestInspectorMixedSelection
    else if ParamStr(1) = 'unknown-arrow-codes' then TestUnknownArrowCodes
    else if ParamStr(1) = 'bezier-arrow-direction' then TestBezierArrowDirection
    else if ParamStr(1) = 'inspector-doubleclick-selection' then TestInspectorDoubleClickSelection
    {$IFDEF LCLCOCOA}
    else if ParamStr(1) = 'cocoa-local-composition' then TestCocoaLocalComposition
    {$ENDIF}
    else if ParamStr(1) = 'inspector-property-families' then TestInspectorPropertyFamilies
    else if ParamStr(1) = 'inspector-visibility' then TestInspectorVisibility
    else if ParamStr(1) = 'font-choice' then TestFontChoice
    else if ParamStr(1) = 'conversion-save' then TestConversionSave
    else if ParamStr(1) = 'unsupported-exports' then TestUnsupportedExports
    else if ParamStr(1) = 'arc-flip' then TestArcFlip
    else if ParamStr(1) = 'arc-arrow-symmetry' then TestArcArrowSymmetry
    else if ParamStr(1) = 'selection-drag' then TestSelectionDrag
    else if ParamStr(1) = 'tpx-staged-sidecars' then TestTpXStagedSidecars
    else if ParamStr(1) = 'tikz-import-native' then TestTikZImportNativeScene
    else if ParamStr(1) = 'tikz-source-save' then TestTikZSourceSave
    else if ParamStr(1) = 'tikz-style-roundtrip' then TestTikZStyleRoundTrip
    else if ParamStr(1) = 'paint-opacity' then TestPaintOpacity
    else if ParamStr(1) = 'paint-gradient' then TestPaintGradient
    else if ParamStr(1) = 'paint-arrow-opacity' then TestPaintArrowOpacity
    else if ParamStr(1) = 'bitmap-eps' then TestBitmapEps
    else if ParamStr(1) = 'svg-linked-image-copy' then TestSvgLinkedImageCopy
    else if ParamStr(1) = 'svg-codec' then TestSvgCodec
    else if ParamStr(1) = 'solid3d-formats' then TestSolid3DFormats
    else if ParamStr(1) = 'solid3d-open-save' then TestSolid3DOpenSave
    else if ParamStr(1) = 'solid3d-editing' then TestSolid3DEditing
    else if ParamStr(1) = 'solid3d-insert-fit' then TestSolid3DInsertFit
    else if ParamStr(1) = 'solid3d-inspector' then TestSolid3DInspector
    else if ParamStr(1) = 'solid3d-torus-segment' then TestSolid3DTorusSegment
    else if ParamStr(1) = 'solid3d-torus-handles' then TestSolid3DTorusHandles
    else if ParamStr(1) = 'open-examples' then TestOpenExamples
    else if ParamStr(1) = 'open-dialog-all-formats' then TestOpenDialogAllFormats
    else if ParamStr(1) = 'labeled-preview' then TestLabeledPreview
{$IFDEF DARWIN}
    else if ParamStr(1) = 'mac-settings' then TestMacSettings
    else if ParamStr(1) = 'mac-resources' then TestMacResourcePaths
{$ENDIF}
    else TestExit(ParamStr(1));
    if GetEnvironmentVariableUTF8('TPX_OS_OPEN_LS_FIRST') <> '' then
      WriteLn('PASS launchservices-open')
    else if ParamStr(1) = 'external-open' then
      WriteLn('PASS external-open')
    else if GetEnvironmentVariableUTF8('TPX_STARTUP_EXPECTED') <> '' then
      WriteLn('PASS startup-file')
    else WriteLn('PASS ', ParamStr(1));
  except
    on E: Exception do begin
      WriteLn(StdErr, E.ClassName, ': ', E.Message);
      if ParamStr(1) = 'clipboard-roundtrip' then
        DumpExceptionBackTrace(StdErr);
      Halt(1);
    end;
  end;
end.

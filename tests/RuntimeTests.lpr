program RuntimeTests;
{$mode Delphi}
uses
  {$IFDEF UNIX}{$IFNDEF CPUWASM32}cthreads,{$ENDIF}{$ENDIF}
  Interfaces, Forms, SysUtils, Classes, Types, Math, Controls, Dialogs, Clipbrd, InterfaceBase, LCLType, LMessages, Process,
  {$IFDEF LCLgtk2}Gtk2Int,{$ENDIF}
  {$IFDEF LCLcocoa}CocoaInt,{$ENDIF}
  {$IFDEF LCLwin32}Win32Int,{$ENDIF}
  Settings0, MainUnit, Propert, Table, Drawings, GObjects, Geometry, Manage, Modes, Input, Devices, Graphics, StdCtrls, ActnList, Menus, GObjBase, SysBasic, Preview, ViewPort, Modify, Output, Bitmaps, ClpbrdOp, ColorEtc, PlatformShortcuts, LiveTeX;

{$R ../src/MainUnit.lfm}
{$R ../src/Propert.lfm}

type
  {$IFDEF LCLgtk2}TNativeWidgetSet = TGtk2WidgetSet;{$ENDIF}
  {$IFDEF LCLcocoa}TNativeWidgetSet = TCocoaWidgetSet;{$ENDIF}
  {$IFDEF LCLwin32}TNativeWidgetSet = TWin32WidgetSet;{$ENDIF}
  TTestWidgetSet = class(TNativeWidgetSet)
    function GetKeyState(nVirtKey: Integer): SmallInt; override;
    function PromptUser(const Caption, Message: string; DialogType: LongInt;
      Buttons: PLongInt; ButtonCount, DefaultIndex, EscapeResult: LongInt): LongInt; override;
  end;
  TObserver = class
    CloseRequests: Integer;
    procedure Closing(Sender: TObject; var Action: TCloseAction);
    procedure ClosingDefault(Sender: TObject; var Action: TCloseAction);
  end;

var
  Observer: TObserver;
  PromptCount, ReplyButton: Integer;
  ShortcutModifierSimulation: Boolean = False;
  ShortcutModifiers: TShiftState = [];

procedure Check(Condition: Boolean; const Message: string);
begin
  if not Condition then raise Exception.Create(Message);
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

function AnswerPrompt(const Caption, Message: string; DialogType: LongInt;
  Buttons: PLongInt; ButtonCount, DefaultIndex, EscapeResult: LongInt;
  UseDefaultPos: Boolean; X, Y: LongInt): LongInt;
begin
  if (ParamStr(1) = 'bitmap-eps') and
    (Pos('Conversion of bitmap to EPS failed:', Message) > 0) then begin
    Inc(PromptCount);
    Exit(idButtonOK);
  end;
  if (ParamStr(1) = 'property-dimensions') and
    ((Pos('Line width must', Message) > 0) or (Pos('Text height must', Message) > 0)) then
    Exit(idButtonOK);
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

procedure RunBitmapFixtureConverter;
var
  Source, Destination: TFileStream;
begin
  if GetEnvironmentVariable('TPX_EPS_FIXTURE_MODE') = 'nonzero' then Halt(7);
  if GetEnvironmentVariable('TPX_EPS_FIXTURE_MODE') = 'no-output' then Halt(0);
  Check(ParamCount = 2, 'Fixture converter expected input and output paths');
  Source := TFileStream.Create(GetEnvironmentVariable('TPX_EPS_FIXTURE'), fmOpenRead);
  try
    Destination := TFileStream.Create(ParamStr(2), fmCreate);
    try Destination.CopyFrom(Source, 0) finally Destination.Free end;
  finally Source.Free end;
end;

procedure TestBitmapEps;
var
  Converted, Expected: Boolean;
begin
  PromptCount := 0;
  Bitmap2EpsPath := GetEnvironmentVariable('TPX_EPS_CONVERTER');
  Expected := GetEnvironmentVariable('TPX_EPS_EXPECT_SUCCESS') = '1';
  Converted := BitmapToEps(GetEnvironmentVariable('TPX_EPS_INPUT'),
    GetEnvironmentVariable('TPX_EPS_OUTPUT'));
  Check(Converted = Expected, 'BitmapToEps returned ' + BoolToStr(Converted, True));
  Check(PromptCount = Ord(not Expected), 'Unexpected bitmap conversion error count');
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
    KeySender.Options := [poWaitOnExit];
    KeySender.Execute;
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
  Line := TLine2D.CreateSpec(-1, Point2D(0, 0), Point2D(20, 10));
  MainForm.TheDrawing.AddObject(-1, Line);
  MainForm.Show;
  Application.ProcessMessages;

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

procedure TestColorBoxCustomState;
var
  ComboBox: TComboBox;
  InitialCount: Integer;
  CustomColor: TColor;
begin
  ComboBox := PropertiesForm.ComboBox3;
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
begin
  Expected := GetEnvironmentVariable('TPX_STARTUP_EXPECTED');
  ExpectedCount := StrToInt(GetEnvironmentVariable('TPX_STARTUP_OBJECTS'));
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
end;

{$IFDEF DARWIN}
procedure TestMacSettings;
var
  ConfigFile, BundleFile, ExpectedLegacy: string;
  ExpectedWidth: Double;
begin
  ConfigFile := IncludeTrailingPathDelimiter(GetAppConfigDir(False)) + 'TpX.ini';
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
  ResourcePath, UserPath, HelpPath: string;
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
end;
{$ENDIF}

procedure TestExit(const Scenario: string);
var
  ModeBefore: TMode;
  Line: TLine2D;
  Saved: TDrawing2D;
  Loader: T_TpX_Loader;
  SaveName: string;
  SaveFailed: Boolean;
begin
  PromptCount := 0;
  ReplyButton := idButtonNo;
  SaveFailed := False;
  if Scenario <> 'exit-clean' then begin
    Line := TLine2D.CreateSpec(-1, Point2D(0, 0), Point2D(20, 10));
    MainForm.TheDrawing.AddObject(-1, Line);
    MainForm.TheDrawing.History.SetPropertiesChanged;
  end;
  if Pos('exit-cancel', Scenario) = 1 then ReplyButton := idButtonCancel;
  if Scenario = 'exit-save-failure' then begin
    MainForm.TheDrawing.FileName := IncludeTrailingPathDelimiter(
      SysUtils.GetTempDir(False)) + 'tpx-missing-save-parent-' + IntToStr(GetProcessID) +
      PathDelim + 'drawing.tpx';
    ReplyButton := idButtonYes;
  end;
  SaveName := '';
  if (Scenario = 'exit-yes') or (Scenario = 'exit-destroy-yes') then begin
    SaveName := GetTempFileName(SysUtils.GetTempDir(False), 'tpx-save-');
    DeleteFile(SaveName);
    SaveName := SaveName + '.tpx';
    MainForm.TheDrawing.FileName := SaveName;
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
  end
  else if Scenario = 'exit-close' then MainForm.Close
  else MainForm.UserEventExecute(MainForm.ExitProgram);
  if Scenario = 'exit-clean' then Check(PromptCount = 0, 'Clean drawing prompted')
  else Check(PromptCount = 1, 'Expected one save prompt, got ' + IntToStr(PromptCount));
  if Pos('exit-cancel', Scenario) = 1 then begin
    Check(Observer.CloseRequests = 0, 'Cancel requested closing');
    Check(MainForm.EventManager.Mode = ModeBefore, 'Cancel discarded the active mode');
    Check(MainForm.TheDrawing.ObjectsCount = 1, 'Cancel discarded the drawing');
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
    // Test-only converter copies independently defined EPS fixtures, without UI.
    if SameText(ChangeFileExt(ExtractFileName(ParamStr(0)), ''), 'sam2p-fixture') then begin
      RunBitmapFixtureConverter;
      Halt(0);
    end;
    if (ParamStr(1) <> 'clipboard-format-width') and
      (ParamStr(1) <> 'clipboard-roundtrip') then
      WidgetSet := TTestWidgetSet.Create;
    Application.Initialize;
    RequireDerivedFormResource := True;
    Application.CreateForm(TMainForm, MainForm);
    Application.CreateForm(TPropertiesForm, PropertiesForm);
    Observer := TObserver.Create;
    MainForm.OnClose := Observer.Closing;
    PromptDialogFunction := AnswerPrompt;
    if GetEnvironmentVariable('TPX_STARTUP_EXPECTED') <> '' then TestStartupFileName
    else if Pos('viewport-', ParamStr(1)) = 1 then TestViewport(ParamStr(1))
    else if ParamStr(1) = 'live-tex-missing-tool' then TestLiveTeXMissingTool
    else if ParamStr(1) = 'live-tex' then TestLiveTeX
    else if ParamStr(1) = 'live-tex-settings' then TestLiveTeXSettings
    else if ParamStr(1) = 'preview-state' then TestPreviewState
    else if ParamStr(1) = 'external-tools' then TestExternalTools
    else if ParamStr(1) = 'check-file-path' then TestCheckFilePath
    else if ParamStr(1) = 'clipboard-format-width' then TestClipboardFormatWidth
    else if ParamStr(1) = 'clipboard-roundtrip' then TestClipboardRoundTrip
    else if ParamStr(1) = 'editable-shortcut-routing' then TestEditableShortcutRouting
    else if ParamStr(1) = 'canvas-focus-transfer' then TestCanvasFocusTransfer
    else if ParamStr(1) = 'platform-shortcuts' then TestPlatformShortcuts
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
    else if ParamStr(1) = 'font-choice' then TestFontChoice
    else if ParamStr(1) = 'conversion-save' then TestConversionSave
    else if ParamStr(1) = 'unsupported-exports' then TestUnsupportedExports
    else if ParamStr(1) = 'bitmap-eps' then TestBitmapEps
    else if ParamStr(1) = 'labeled-preview' then TestLabeledPreview
{$IFDEF DARWIN}
    else if ParamStr(1) = 'mac-settings' then TestMacSettings
    else if ParamStr(1) = 'mac-resources' then TestMacResourcePaths
{$ENDIF}
    else TestExit(ParamStr(1));
    if GetEnvironmentVariable('TPX_STARTUP_EXPECTED') <> '' then
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

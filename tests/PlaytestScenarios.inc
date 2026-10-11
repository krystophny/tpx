{ Behavioral regressions from the real GTK2 playtest, issues 20-31. }
procedure TestTextMetrics;
var
  Bitmap: TBitmap;
  TextObj: TText2D;
  Width, Descent, Expected: Single;
  Size: TSize;
  Alignment: THAlignment;
  Style: TFontStyles;
  I: Integer;
  LabelText, Face: string;
begin
  Bitmap := TBitmap.Create;
  try
    for I := 0 to 2 do begin
      case I of
        0: begin Face := 'Serif'; Style := [] end;
        1: begin Face := 'Sans'; Style := [fsBold] end;
        2: begin Face := 'Monospace'; Style := [fsItalic] end;
      end;
      LabelText := 'Bphi ' + UTF8Encode(WideChar($3BB));
      Bitmap.Canvas.Font.Name := Face;
      Bitmap.Canvas.Font.Style := Style;
      Bitmap.Canvas.Font.Height := -1000;
      Size := Bitmap.Canvas.TextExtent(LabelText);
      Expected := Size.cx / 1000;
      GetTextDimension(UTF8Decode(LabelText), Face, Style, 1, Width, Descent);
      Check(Abs(Width - Expected) < 0.002, 'Text width disagrees with native font metrics');
      Check((Descent >= 0) and (Descent < 1), 'Invalid font descent');
      TextObj := TText2D.CreateSpec(-1, Point2D(10, 20), 7, LabelText);
      try
        TextObj.Font.Name := Face;
        TextObj.Font.Style := Style;
        for Alignment := Low(THAlignment) to High(THAlignment) do begin
          TextObj.HAlignment := Alignment;
          TextObj.UpdateExtension(TextObj);
          Check(Abs(TextObj.BoundingBox.Right - TextObj.BoundingBox.Left - Expected * 7)
            < 0.02, 'Text bounds do not contain the native label width');
        end;
      finally TextObj.Free end;
    end;
  finally Bitmap.Free end;
end;

procedure TestPathFirstClick;
const
  Tools: array[0..5] of Integer = (Msg_InsertPolyline, Msg_InsertPolygon,
    Msg_InsertCurve, Msg_InsertClosedCurve, Msg_InsertBezier, Msg_InsertClosedBezier);
var
  Tool, I: Integer;
  FoundFinish: Boolean;
  Start, Finish: TPoint2D;
  Prim: TPrimitive2D;
begin
  MainForm.Show;
  Application.ProcessMessages;
  MainForm.LocalView.VisualRect := Rect2D(0, 0,
    MainForm.LocalView.ClientWidth, MainForm.LocalView.ClientHeight);
  UseSnap := False; UseShapeSnap := False; UseAngularSnap := False;
  Start := MainForm.LocalView.ScreenToViewport(Point2D(80, 120));
  Finish := MainForm.LocalView.ScreenToViewport(Point2D(200, 130));
  for Tool in Tools do begin
    MainForm.EventManager.SendMessage(Tool, nil);
    { No mouse-move event before the first release. }
    MainForm.EventManager.MouseUp(nil, mbLeft, [], 80, 120);
    MainForm.EventManager.MouseMove(nil, [], 160, 180);
    MainForm.EventManager.MouseUp(nil, mbLeft, [], 160, 180);
    MainForm.EventManager.MouseMove(nil, [], 200, 130);
    MainForm.EventManager.MouseUp(nil, mbLeft, [], 200, 130);
    MainForm.EventManager.Mode.OnDblClick(nil, 200, 130);
    Prim := MainForm.TheDrawing.ObjectList.LastObj as TPrimitive2D;
    Check(IsSamePoint2D(Prim.Points[0], Start), 'Immediate first click retained the origin');
    FoundFinish := False;
    for I := 0 to Prim.Points.Count - 1 do
      if IsSamePoint2D(Prim.Points[I], Finish) then FoundFinish := True;
    Check(FoundFinish, 'Last anchor changed for tool ' + IntToStr(Tool));
  end;
end;

{$IFDEF WINDOWS}
function ReadAsyncScenarioFile(const Directory, FileName: string): string;
var
  Values: TStringList;
begin
  Result := '<missing>';
  if not FileExists(Directory + PathDelim + FileName) then Exit;
  Values := TStringList.Create;
  try
    try
      Values.LoadFromFile(Directory + PathDelim + FileName);
      Result := Trim(Values.Text);
      if Result = '' then Result := '<empty>';
    except
      on E: Exception do Result := '<read failed: ' + E.Message + '>';
    end;
  finally
    Values.Free;
  end;
  if Length(Result) > 1024 then
    Result := Copy(Result, 1, 1024) + ' [truncated]';
end;

function AsyncChildFailureDetails(const Directory: string): string;
begin
  Result := Format('started=%s; status=%s; error=%s',
    [ReadAsyncScenarioFile(Directory, 'async-child.started'),
     ReadAsyncScenarioFile(Directory, 'async-child.status'),
     ReadAsyncScenarioFile(Directory, 'async-child.error')]);
end;
{$ENDIF}

procedure TestAsyncTools;
const
{$IFDEF WINDOWS}
  WaitCommand = 'powershell -NoProfile -NonInteractive -Command "Start-Sleep -Milliseconds 100; Set-Content finished waited"';
{$ELSE}
  AsyncCommand = 'printf %s "$$" > childpid; sleep 1; printf done > finished';
  WaitCommand = 'sleep 0.1; printf waited > finished';
{$ENDIF}
var
  Directory, Marker, ChildPID: string;
  Started: QWord;
  Contents: TStringList;
  FailureDetails: string;
{$IFDEF WINDOWS}
  AsyncCommand: string;
{$ENDIF}
begin
  Directory := GetTempFileName(SysUtils.GetTempDir(False), 'tpx-async-');
  DeleteFile(Directory); Check(CreateDir(Directory), 'Could not make async directory');
  Marker := Directory + PathDelim + 'finished';
{$IFDEF WINDOWS}
  AsyncCommand := 'powershell -NoProfile -NonInteractive -Command "' +
    '$ErrorActionPreference = ''Stop''; try { Set-Content ' +
    QuotedStr(Directory + PathDelim + 'async-child.started') + ' started; ' +
    'Start-Sleep -Seconds 1; Set-Content finished done; Set-Content ' +
    QuotedStr(Directory + PathDelim + 'async-child.status') + ' completed ' +
    '} catch { $_ | Out-File ' +
    QuotedStr(Directory + PathDelim + 'async-child.error') + '; exit 1 }"';
{$ENDIF}
  Started := GetTickCount64;
  try
    Check(FileExec(AsyncCommand, '', '', Directory, True, False),
      'Async launch failed');
    Check(GetTickCount64 - Started < 700, 'Async command blocked its caller');
    Check(not FileExists(Marker), 'Child finished before async return');
    { Keep the prompt-return bound above; allow slower child startup on CI. }
    while not FileExists(Marker) and (GetTickCount64 - Started < 10000) do Sleep(20);
    if not FileExists(Marker) then begin
      FailureDetails := 'Async child did not finish';
{$IFDEF WINDOWS}
      FailureDetails := FailureDetails + ' (' + AsyncChildFailureDetails(Directory) + ')';
{$ENDIF}
      Check(False, FailureDetails);
    end;
{$IFDEF LINUX}
    Contents := TStringList.Create;
    try
      Contents.LoadFromFile(Directory + PathDelim + 'childpid');
      ChildPID := Trim(Contents.Text);
      Started := GetTickCount64;
      while DirectoryExists('/proc/' + ChildPID) and
        (GetTickCount64 - Started < 3000) do begin
        Application.ProcessMessages;
        Sleep(20);
      end;
      Check(not DirectoryExists('/proc/' + ChildPID), 'Async child was not reaped');
    finally Contents.Free end;
{$ENDIF}
    Check(FileExec(WaitCommand, '', '', Directory, True, True),
      'Waiting command failed');
    Contents := TStringList.Create;
    try
      Contents.LoadFromFile(Marker);
      Check(Trim(Contents.Text) = 'waited', 'Wait=True returned before completion');
    finally Contents.Free end;
  finally DeleteFile(Marker); DeleteFile(Directory + PathDelim + 'childpid');
    RemoveDir(Directory) end;
end;

procedure TestDefaultOpener;
var
  Marker, Document: string;
  Started: QWord;
  Contents: TStringList;
begin
  Marker := GetEnvironmentVariable('TPX_OPENER_LOG');
  Check(Marker <> '', 'Opener recorder is not configured');
  Document := ExtractFilePath(Marker) + 'a file ' + UTF8Encode(WideChar($3BB)) +
    ' ' + Chr(39) + ' $value.pdf';
  Contents := TStringList.Create;
  try Contents.SaveToFile(Document); finally Contents.Free end;
  OpenOrExec('', Document);
  Started := GetTickCount64;
  while not FileExists(Marker) and (GetTickCount64-Started < 3000) do Sleep(20);
  Check(FileExists(Marker), 'Default document opener was not called');
  Contents := TStringList.Create;
  try
    Contents.LoadFromFile(Marker);
    Check(Trim(Contents.Text) = Document, 'Default opener lost the document argument');
  finally Contents.Free; DeleteFile(Document) end;
end;

procedure TestPropertyDimensions;
const
  // Explicit string arrays keep short and long invalid tokens intact.
  InvalidHeights: array[0..4] of string = ('-1', '0', 'NaN', 'Inf', 'bad');
  InvalidWidths: array[0..3] of string = ('-1', 'NaN', 'Inf', 'bad');
  InvalidToolbarHeights: array[0..3] of string = ('-1', '0', 'NaN', 'bad');
var
  TextObj: TText2D;
  Bad: string;
  ErrorText: string;
  HistoryCount: Integer;
  HeightEditor, WidthEditor, FontEditor: TEdit;
  ErrorLabel, SelectionLabel: TLabel;
  LineStyleEditor: TComboBox;
  ActiveName: string;
  Key: Word;
  Shift: TShiftState;
begin
  MainForm.Show;
  Application.ProcessMessages;
  MainForm.RefreshInspectorSelection;
  SelectionLabel := MainForm.Inspector.FindComponent(
    'InspectorSelectionState') as TLabel;
  WidthEditor := MainForm.Inspector.EditorFor(opfLineWidth) as TEdit;
  HistoryCount := MainForm.TheDrawing.History.Count;
  Check((SelectionLabel.Caption = 'New-object defaults') and
    (not WidthEditor.Enabled) and
    (WidthEditor.Text = RealTypeToStr(MainForm.TheDrawing.New_LineWidth)) and
    (MainForm.TheDrawing.History.Count = HistoryCount),
    'Empty inspector did not show separate, read-only new-object defaults');
  TextObj := TText2D.CreateSpec(-1, Point2D(10, 20), 7, 'Original');
  MainForm.TheDrawing.AddObject(-1, TextObj);
  MainForm.TheDrawing.SelectionClear;
  MainForm.TheDrawing.SelectionAdd(TextObj);
  MainForm.RefreshInspectorSelection;
  HeightEditor := MainForm.Inspector.EditorFor(opfTextHeight) as TEdit;
  WidthEditor := MainForm.Inspector.EditorFor(opfLineWidth) as TEdit;
  FontEditor := MainForm.Inspector.EditorFor(opfFontName) as TEdit;
  ErrorLabel := MainForm.Inspector.FindComponent(
    'InspectorTextHeightError') as TLabel;
  for Bad in InvalidHeights do begin
    MainForm.Inspector.FocusField(opfTextHeight);
    HeightEditor.Text := Bad;
    Check(not MainForm.Inspector.PrepareForSelectionChange(ErrorText),
      'Invalid text height was accepted: ' + Bad);
    Check((TextObj.Height = 7) and (TextObj.Text = 'Original') and
      ErrorLabel.Visible, 'Invalid height preview changed the object');
    MainForm.Inspector.CancelActiveEdit;
  end;
  for Bad in InvalidWidths do begin
    MainForm.Inspector.FocusField(opfLineWidth);
    WidthEditor.Text := Bad;
    Check(not MainForm.Inspector.PrepareForSelectionChange(ErrorText),
      'Invalid stroke width was accepted: ' + Bad);
    Check((TextObj.LineWidth = 1) and (TextObj.LineStyle = liSolid),
      'Invalid width preview changed stroke properties');
    MainForm.Inspector.CancelActiveEdit;
  end;
  HistoryCount := MainForm.TheDrawing.History.Count;
  MainForm.Inspector.FocusField(opfLineWidth);
  WidthEditor.Text := '-';
  Check(not MainForm.Inspector.PrepareForSelectionChange(ErrorText),
    'Invalid width draft unexpectedly committed before changing fields');
  HeightEditor.SetFocus;
  Application.ProcessMessages;
  HeightEditor.Text := '9';
  Application.ProcessMessages;
  Check(MainForm.Inspector.HasActiveEdit and (WidthEditor.Text = '-') and
    (HeightEditor.Text = '7') and (TextObj.LineWidth = 1) and
    (TextObj.Height = 7) and
    (MainForm.TheDrawing.History.Count = HistoryCount),
    'Entering another field lost the active invalid draft or exposed an untracked value');
  MainForm.Inspector.CancelActiveEdit;
  HistoryCount := MainForm.TheDrawing.History.Count;
  MainForm.Inspector.FocusField(opfTextHeight);
  HeightEditor.Text := '7.5';
  Check(TextObj.Height = 7.5, 'Valid height draft was not previewed');
  LineStyleEditor := MainForm.Inspector.EditorFor(opfLineStyle) as TComboBox;
  Check(Assigned(LineStyleEditor.OnChange),
    'The line style control has no change handler');
  LineStyleEditor.OnChange(LineStyleEditor);
  Check(MainForm.Inspector.HasActiveEdit and
    (HeightEditor.Text = '7.5') and (TextObj.Height = 7.5) and
    (MainForm.TheDrawing.History.Count = HistoryCount),
    'An unchanged native combo notification committed the height draft');
  LineStyleEditor.SetFocus;
  Application.ProcessMessages;
  if MainForm.ActiveControl = nil then ActiveName := '<nil>'
  else ActiveName := MainForm.ActiveControl.Name;
  Check(MainForm.Inspector.HasActiveEdit and
    (HeightEditor.Text = '7.5') and (TextObj.Height = 7.5) and
    (MainForm.TheDrawing.History.Count = HistoryCount),
    'Focusing the style dropdown committed or replaced the height draft: ' +
    'active=' + BoolToStr(MainForm.Inspector.HasActiveEdit, True) +
    ', text=' + HeightEditor.Text +
    ', model=' + FloatToStr(TextObj.Height) +
    ', history=' + IntToStr(MainForm.TheDrawing.History.Count) +
    ', expectedHistory=' + IntToStr(HistoryCount) +
    ', activeControl=' + ActiveName);
  MainForm.LocalView.SetFocus;
  Application.ProcessMessages;
  Check(MainForm.Inspector.HasActiveEdit and
    (HeightEditor.Text = '7.5') and (TextObj.Height = 7.5) and
    (MainForm.TheDrawing.History.Count = HistoryCount),
    'Focus leaving the inspector committed or replaced the height draft');
  MainForm.WindowState := wsMinimized;
  Application.ProcessMessages;
  Check((MainForm.WindowState = wsMinimized) and
    MainForm.Inspector.HasActiveEdit and (HeightEditor.Text = '7.5') and
    (TextObj.Height = 7.5) and
    (MainForm.TheDrawing.History.Count = HistoryCount),
    'Window deactivation committed or replaced the active draft');
  MainForm.WindowState := wsNormal;
  MainForm.Show;
  Application.ProcessMessages;
  HeightEditor.SetFocus;
  Key := VK_TAB;
  Shift := [];
  HeightEditor.OnKeyDown(HeightEditor, Key, Shift);
  Check((Key = 0) and not MainForm.Inspector.HasActiveEdit and
    (MainForm.TheDrawing.History.Count = HistoryCount + 1) and
    (MainForm.ActiveControl = FontEditor),
    'Explicit Tab did not commit and move to the next inspector field');
  Key := VK_TAB;
  Shift := [ssShift];
  FontEditor.OnKeyDown(FontEditor, Key, Shift);
  Check((Key = 0) and (MainForm.ActiveControl = HeightEditor),
    'Shift+Tab did not return to the previous inspector field');
  MainForm.TheDrawing.History.Undo;
  TextObj := MainForm.TheDrawing.GetObject(0) as TText2D;
  Check(TextObj.Height = 7, 'Undo did not restore the draft before Tab commit');
  HistoryCount := MainForm.TheDrawing.History.Count;
  MainForm.Inspector.FocusField(opfTextHeight);
  HeightEditor.Text := '0.5';
  Check(TextObj.Height = 0.5, 'Valid height was not previewed');
  Check(MainForm.Inspector.PrepareForSelectionChange(ErrorText),
    'Fractional height did not commit: ' + ErrorText);
  Check((TextObj.Height = 0.5) and
    (MainForm.TheDrawing.History.Count = HistoryCount) and
    MainForm.TheDrawing.History.CanUndo and
    not MainForm.TheDrawing.History.CanRedo,
    'Fractional height did not replace the undone history branch with one step');
  MainForm.TheDrawing.History.Undo;
  TextObj := MainForm.TheDrawing.GetObject(0) as TText2D;
  Check(TextObj.Height = 7, 'One undo did not restore the original height');
  MainForm.TheDrawing.History.Redo;
  TextObj := MainForm.TheDrawing.GetObject(0) as TText2D;
  Check(TextObj.Height = 0.5, 'Redo did not restore the committed height');
  MainForm.TheDrawing.SelectionAdd(TextObj);
  MainForm.RefreshInspectorSelection;
  HistoryCount := MainForm.TheDrawing.History.Count;
  MainForm.Inspector.FocusField(opfTextHeight);
  HeightEditor.Text := '1.75';
  Check(TextObj.Height = 1.75, 'Valid preview did not update the object');
  MainForm.Inspector.CancelActiveEdit;
  Check((TextObj.Height = 0.5) and
    (MainForm.TheDrawing.History.Count = HistoryCount),
    'Cancel did not restore the exact value without adding history');
  MainForm.Inspector.FocusField(opfTextHeight);
  HeightEditor.Text := '0.5';
  Check(MainForm.Inspector.PrepareForSelectionChange(ErrorText),
    'No-op field commit failed');
  Check(MainForm.TheDrawing.History.Count = HistoryCount,
    'Focusing and recommitting the same value added history');
  MainForm.Inspector.FocusField(opfLineWidth);
  WidthEditor.Text := '0';
  Check(MainForm.Inspector.PrepareForSelectionChange(ErrorText),
    'Zero hairline width did not commit');
  Check(TextObj.LineWidth = 0, 'Zero hairline width was not applied');
  MainForm.TheDrawing.New_FontHeight := 0.5;
  MainForm.TheDrawing.New_LineWidth := 0;
  for Bad in InvalidToolbarHeights do begin
    MainForm.Edit4.Text := Bad;
    MainForm.ChangeProperties(MainForm.Edit4);
    MainForm.NumericPropertyExit(MainForm.Edit4);
    Check(TextObj.Height = 0.5, 'Toolbar accepted invalid height');
    Check(MainForm.Edit4.Text = '0.5', 'Toolbar did not restore valid height');
  end;
  MainForm.ComboBox6.Text := '-1';
  MainForm.ChangeProperties(MainForm.ComboBox6);
  MainForm.NumericPropertyExit(MainForm.ComboBox6);
  Check(TextObj.LineWidth = 0, 'Toolbar changed width after invalid input');
end;

procedure TestInspectorMixedSelection;
var
  Drawing: TDrawing2D;
  FirstLine, SecondLine: TLine2D;
  InnerGroup, OuterGroup: TGroup2D;
  WidthEditor: TEdit;
  StateLabel: TLabel;
  HistoryCount: Integer;
  Key: Word;
  Shift: TShiftState;
  Loader: T_TpX_Loader;
  FileName: string;
begin
  MainForm.Show;
  Application.ProcessMessages;
  Drawing := MainForm.TheDrawing;
  FirstLine := TLine2D.CreateSpec(-1, Point2D(3, 4), Point2D(20, 10));
  FirstLine.LineWidth := 0.5;
  FirstLine.LineStyle := liDashed;
  FirstLine.LineColor := clRed;
  SecondLine := TLine2D.CreateSpec(-1, Point2D(7, 9), Point2D(31, 18));
  SecondLine.LineWidth := 1.5;
  SecondLine.LineStyle := liDotted;
  SecondLine.LineColor := clBlue;
  InnerGroup := TGroup2D.CreateSpec(101, [FirstLine, SecondLine]);
  OuterGroup := TGroup2D.CreateSpec(100, [InnerGroup]);
  Drawing.AddObject(100, OuterGroup);

  FileName := GetTempFileName(SysUtils.GetTempDir(False),
    'tpx-inspector-mixed-');
  try
    Check(StoreToFile_TpX(Drawing, FileName, False),
      'Could not save the nested mixed-selection fixture');
    Drawing.Clear;
    Loader := T_TpX_Loader.Create(Drawing);
    try
      Loader.LoadFromFile(FileName);
    finally
      Loader.Free;
    end;
  finally
    DeleteFile(FileName);
  end;
  OuterGroup := Drawing.ObjectList.FirstObj as TGroup2D;
  InnerGroup := OuterGroup.Objects.FirstObj as TGroup2D;
  FirstLine := InnerGroup.Objects.FirstObj as TLine2D;
  SecondLine := InnerGroup.Objects.NextObj as TLine2D;
  Check((FirstLine.ID < 0) and (SecondLine.ID < 0),
    'Native reload did not preserve unassigned nested group children');
  Drawing.SelectionClear;
  Drawing.SelectionAdd(OuterGroup);
  MainForm.RefreshInspectorSelection;
  WidthEditor := MainForm.Inspector.EditorFor(opfLineWidth) as TEdit;
  StateLabel := MainForm.Inspector.FindComponent(
    'InspectorLineWidthState') as TLabel;
  Check((WidthEditor.Text = '') and (StateLabel.Caption = 'Mixed'),
    'Inspector did not expose a mixed line width');
  HistoryCount := Drawing.History.Count;
  WidthEditor.SetFocus;
  Application.ProcessMessages;
  Key := VK_TAB;
  Shift := [];
  WidthEditor.OnKeyDown(WidthEditor, Key, Shift);
  Check((FirstLine.LineWidth = 0.5) and (SecondLine.LineWidth = 1.5) and
    (Drawing.History.Count = HistoryCount),
    'Focusing and tabbing through a mixed field changed a target');
  WidthEditor.Text := '2.25';
  Check((FirstLine.LineWidth = 2.25) and (SecondLine.LineWidth = 2.25),
    'Editing a mixed width did not apply to every selected line');
  Check((FirstLine.LineStyle = liDashed) and (SecondLine.LineStyle = liDotted) and
    (FirstLine.LineColor = clRed) and (SecondLine.LineColor = clBlue) and
    IsSamePoint2D(FirstLine.Points[0], Point2D(3, 4)) and
    IsSamePoint2D(SecondLine.Points[1], Point2D(31, 18)),
    'Mixed width edit overwrote unedited line fields or geometry');
  Key := VK_TAB;
  Shift := [];
  WidthEditor.OnKeyDown(WidthEditor, Key, Shift);
  Check(Drawing.History.Count = HistoryCount + 1,
    'Mixed width edit did not commit as one history step');
  Drawing.History.Undo;
  OuterGroup := Drawing.GetObject(0) as TGroup2D;
  InnerGroup := OuterGroup.Objects.FirstObj as TGroup2D;
  FirstLine := InnerGroup.Objects.FirstObj as TLine2D;
  SecondLine := InnerGroup.Objects.NextObj as TLine2D;
  Check((FirstLine.LineWidth = 0.5) and (SecondLine.LineWidth = 1.5) and
    (FirstLine.LineStyle = liDashed) and (SecondLine.LineStyle = liDotted) and
    (FirstLine.LineColor = clRed) and (SecondLine.LineColor = clBlue),
    'Undo did not restore the distinct pre-edit mixed values');
end;

{$IFDEF LCLCOCOA}
function CocoaInspectorTextView(Memo: TCompositionAwareMemo): TCocoaTextView;
begin
  Result := TCocoaTextView(NSScrollView(Memo.Handle).documentView);
end;

procedure CocoaSetMarkedText(Memo: TCompositionAwareMemo;
  const Value: string);
var
  NativeText: NSString;
begin
  NativeText := NSStringUtf8(Value);
  CocoaInspectorTextView(Memo).setMarkedText_selectedRange_replacementRange(
    NativeText, NSMakeRange(NativeText.length, 0),
    NSMakeRange(NSNotFound, 0));
  NativeText.release;
end;

procedure CocoaCancelMarkedText(Memo: TCompositionAwareMemo);
var
  EmptyText: NSString;
begin
  EmptyText := NSString.string_;
  CocoaInspectorTextView(Memo).setMarkedText_selectedRange_replacementRange(
    EmptyText, NSMakeRange(0, 0), NSMakeRange(NSNotFound, 0));
end;

  procedure CocoaSetFieldMarkedText(Edit: TCompositionAwareEdit;
    const Value: string);
  var
    NativeText: NSString;
    FieldEditor: NSTextInputClientProtocol;
  begin
    FieldEditor := NSTextInputClientProtocol(
      NSTextField(Edit.Handle).currentEditor);
    Check(Assigned(FieldEditor),
      'Focused Cocoa TEdit has no native field editor');
    NativeText := NSStringUtf8(Value);
    FieldEditor.setMarkedText_selectedRange_replacementRange(NativeText,
      NSMakeRange(NativeText.length, 0), NSMakeRange(NSNotFound, 0));
    NativeText.release;
  end;

  procedure CocoaCancelFieldMarkedText(Edit: TCompositionAwareEdit);
  var
    EmptyText: NSString;
    FieldEditor: NSTextInputClientProtocol;
  begin
    FieldEditor := NSTextInputClientProtocol(
      NSTextField(Edit.Handle).currentEditor);
    Check(Assigned(FieldEditor),
      'Focused Cocoa TEdit lost its native field editor');
    EmptyText := NSString.string_;
    FieldEditor.setMarkedText_selectedRange_replacementRange(EmptyText,
      NSMakeRange(0, 0), NSMakeRange(NSNotFound, 0));
  end;

function CocoaControlCompositionPending(Control: TWinControl): Boolean;
begin
  if Control is TCompositionAwareMemo then
    Exit((Control as TCompositionAwareMemo).CompositionPending);
  if Control is TCompositionAwareEdit then
    Exit((Control as TCompositionAwareEdit).CompositionPending);
  Result := False;
end;

procedure CocoaDrainTextInputQueue(Control: TWinControl);
var
  Deadline: QWord;
begin
  Deadline := GetTickCount64 + 1000;
  while CocoaControlCompositionPending(Control) and
    (GetTickCount64 < Deadline) do
    NSRunLoop.currentRunLoop.runMode_beforeDate(NSDefaultRunLoopMode,
      NSDate.dateWithTimeIntervalSinceNow(0.01));
end;
{$ENDIF}

procedure TestInspectorPropertyFamilies;
var
  Drawing: TDrawing2D;
  LineObj: TLine2D;
  RectObj: TRectangle2D;
  TextObj: TText2D;
  StarObj: TStar2D;
  SymbolObj: TSymbol2D;
  BitmapObj: TBitmap2D;
  Combo: TComboBox;
  Edit: TEdit;
  Memo: TCompositionAwareMemo;
  InlineMemo: TCompositionAwareMemo;
{$IFDEF LCLCOCOA}
  InlineTextView: TCocoaTextView;
{$ENDIF}
  CheckBox: TCheckBox;
  ErrorText: string;
  FileName: string;
  Reloaded: TDrawing2D;
  Loader: T_TpX_Loader;
  ReloadedText: TText2D;
  StableInlineText: string;
  StablePlainStorage: AnsiString;
  StableInlineCaret: Integer;

  function InspectorTop(Control: TControl): Integer;
  begin
    Result := 0;
    while Control <> nil do
    begin
      Inc(Result, Control.Top);
      Control := Control.Parent;
    end;
  end;

  procedure CheckSectionOrder;
  var
    I, PreviousTop: Integer;
    Component: TComponent;
    Header: TButton;
  begin
    PreviousTop := -1;
    for I := 0 to 5 do
    begin
      Component := MainForm.Inspector.FindComponent(
        'InspectorSection' + IntToStr(I) + 'Header');
      Check(Component is TButton,
        'Inspector section heading is missing: ' + IntToStr(I));
      Header := Component as TButton;
      Check(InspectorTop(Header) > PreviousTop,
        'Inspector section heading ' + IntToStr(I) + ' at ' +
        IntToStr(InspectorTop(Header)) + ' follows ' + IntToStr(PreviousTop));
      PreviousTop := InspectorTop(Header);
    end;
    Check(InspectorTop(MainForm.Inspector.FindComponent(
      'InspectorSection0Header') as TButton) <
      InspectorTop(MainForm.Inspector.EditorFor(opfLineStyle)),
      'Line section heading appears below its fields');
    Check(InspectorTop(MainForm.Inspector.EditorFor(opfLineStyle)) <
      InspectorTop(MainForm.Inspector.EditorFor(opfLineWidth)),
      'Line section fields are reversed');
  end;

  {$IFDEF LCLCOCOA}
  procedure CheckCompositionEndWithoutTextChange;
  var
    TextView: TCocoaTextView;
    InspectorMemo: TCompositionAwareMemo;
    OriginalStorageText: AnsiString;
    OriginalDisplayText: string;
    Selection: NSRange;
  begin
    MainForm.Inspector.FocusField(opfText);
    InspectorMemo := MainForm.Inspector.EditorFor(opfText) as
      TCompositionAwareMemo;
    InspectorMemo.HandleNeeded;
    TextView := CocoaInspectorTextView(InspectorMemo);
    OriginalStorageText := TextObj.Text;
    OriginalDisplayText := InspectorMemo.Text;
    TextView.setSelectedRange(NSMakeRange(2, 3));
    Selection := TextView.selectedRange;
    Check((Selection.location = 2) and (Selection.length = 3),
      'Native Cocoa selection setup failed');
    Check(Assigned(InspectorMemo.OnMouseUp),
      'Inspector memo does not observe native caret/selection movement');
    InspectorMemo.OnMouseUp(InspectorMemo, mbLeft, [], 0, 0);
    Check((InspectorMemo.SelStart = 2) and (InspectorMemo.SelLength = 3),
      'Inspector memo did not observe the native selection before composition');
    TextView.supressTextChangeEvent := 1;
    try
      CocoaSetMarkedText(InspectorMemo, 'unfinished composition');
      Check(InspectorMemo.CompositionPending and
        MainForm.Inspector.HasActiveEdit,
        'Native Cocoa marked text did not hold the inspector edit gate');
      CocoaCancelMarkedText(InspectorMemo);
    finally
      TextView.supressTextChangeEvent := 0;
    end;
    CocoaDrainTextInputQueue(InspectorMemo);
    Check(not InspectorMemo.CompositionPending,
      'Native Cocoa END without text change left the inspector memo pending: ' +
      'pending=' + BoolToStr(InspectorMemo.CompositionPending, True) +
      ', nativeMarked=' + BoolToStr(TextView.hasMarkedText, True));
    Selection := TextView.selectedRange;
    Check((TextObj.Text = OriginalStorageText) and
      (InspectorMemo.Text = OriginalDisplayText),
      'Canceling native composition changed the inspector text value');
    Check((Selection.location = 2) and (Selection.length = 3) and
      (InspectorMemo.SelStart = 2) and (InspectorMemo.SelLength = 3),
      'Canceling native composition did not restore the moved selection');
    Check(MainForm.Inspector.PrepareForSelectionChange(ErrorText),
      'Canceled native composition still blocked inspector settlement: ' +
      ErrorText);
  end;

  procedure CheckFieldEditorCompositionEndWithoutTextChange;
  var
    InspectorEdit: TCompositionAwareEdit;
    OriginalText: string;
    OriginalRadius: TRealType;
  begin
    MainForm.Inspector.FocusField(opfRectangleRX);
    InspectorEdit := MainForm.Inspector.EditorFor(opfRectangleRX) as
      TCompositionAwareEdit;
    InspectorEdit.HandleNeeded;
    OriginalText := InspectorEdit.Text;
    OriginalRadius := RectObj.RX;
    CocoaSetFieldMarkedText(InspectorEdit, 'unfinished composition');
    Check(InspectorEdit.CompositionPending and
      MainForm.Inspector.HasActiveEdit,
      'Native Cocoa field-editor marked text did not hold the inspector gate');
    CocoaCancelFieldMarkedText(InspectorEdit);
    CocoaDrainTextInputQueue(InspectorEdit);
    Check(not InspectorEdit.CompositionPending,
      'Native Cocoa field-editor END without text change left TEdit pending: ' +
      'pending=' + BoolToStr(InspectorEdit.CompositionPending, True));
    Check(InspectorEdit.Text = OriginalText,
      'Canceling native field-editor composition changed the TEdit value (' +
      OriginalText + ' -> ' + InspectorEdit.Text + ')');
    Check(Abs(RectObj.RX - OriginalRadius) < 0.0001,
      'Uncommitted field-editor composition changed the rectangle radius');
    Check(MainForm.Inspector.PrepareForSelectionChange(ErrorText),
      'Canceled field-editor composition still blocked inspector settlement: ' +
      ErrorText);
  end;
  {$ENDIF}

  procedure SelectObject(Obj: TObject2D);
  begin
    Drawing.SelectionClear;
    Drawing.SelectionAdd(Obj);
    MainForm.RefreshInspectorSelection;
  end;

  procedure ChangeCombo(Field: TObjectPropertyField; Index: Integer);
  begin
    MainForm.Inspector.FocusField(Field);
    Combo := MainForm.Inspector.EditorFor(Field) as TComboBox;
    Combo.ItemIndex := Index;
    if Assigned(Combo.OnChange) then Combo.OnChange(Combo);
    Check(MainForm.Inspector.PrepareForSelectionChange(ErrorText),
      'Inspector combo edit failed: ' + IntToStr(Ord(Field)) + ': ' + ErrorText);
  end;

  procedure ChangeText(Field: TObjectPropertyField; const Value: string);
  begin
    MainForm.Inspector.FocusField(Field);
    if MainForm.Inspector.EditorFor(Field) is TMemo then
    begin
      Memo := MainForm.Inspector.EditorFor(Field) as TCompositionAwareMemo;
      Memo.Text := Value;
    end
    else if MainForm.Inspector.EditorFor(Field) is TComboBox then
    begin
      Combo := MainForm.Inspector.EditorFor(Field) as TComboBox;
      Combo.Text := Value;
      if Assigned(Combo.OnChange) then Combo.OnChange(Combo);
    end
    else
    begin
      Edit := MainForm.Inspector.EditorFor(Field) as TEdit;
      Edit.Text := Value;
    end;
    Check(MainForm.Inspector.PrepareForSelectionChange(ErrorText),
      'Inspector text edit failed: ' + IntToStr(Ord(Field)) + ': ' + ErrorText);
  end;

  procedure ChangeCheck(Field: TObjectPropertyField; Value: Boolean);
  begin
    MainForm.Inspector.FocusField(Field);
    CheckBox := MainForm.Inspector.EditorFor(Field) as TCheckBox;
    CheckBox.Checked := Value;
    if Assigned(CheckBox.OnChange) then CheckBox.OnChange(CheckBox);
    Check(MainForm.Inspector.PrepareForSelectionChange(ErrorText),
      'Inspector checkbox edit failed: ' + IntToStr(Ord(Field)) + ': ' + ErrorText);
  end;

  procedure RoundTripText(const Value: string);
  begin
    Memo := MainForm.Inspector.EditorFor(opfText) as TCompositionAwareMemo;
    MainForm.Inspector.FocusField(opfText);
    Memo.HandleNeeded;
    Check(Memo.HandleAllocated and (Memo.Parent <> nil),
      'Inspector memo was not a parented, handled native control');
    Memo.Text := Value;
    Check(Memo.EditText = Value,
      'Native memo edit text changed the exact input bytes');
    Check(MainForm.Inspector.PrepareForSelectionChange(ErrorText),
      'Inspector text draft did not commit: ' + ErrorText);
    Check(DecodeTpXTextForEdit(TextObj.Text) = LCLTextToUTF8Bytes(Value),
      'Inspector text draft did not reach its model before save');
    FileName := GetTempFileName(SysUtils.GetTempDir(False),
      'tpx-inspector-text-');
    try
      Check(StoreToFile_TpX(Drawing, FileName, False),
        'Inspector text draft did not save');
      Reloaded := TDrawing2D.Create(nil);
      Loader := T_TpX_Loader.Create(Reloaded);
      try
        Loader.LoadFromFile(FileName);
        ReloadedText := Reloaded.GetObject(TextObj.ID) as TText2D;
        Check(ReloadedText.Text = TextObj.Text,
          'Inspector stored text changed through native save/reload (' +
          IntToStr(Length(TextObj.Text)) + ' vs ' +
          IntToStr(Length(ReloadedText.Text)) + ' bytes)');
        Check(DecodeTpXTextForEdit(ReloadedText.Text) =
          LCLTextToUTF8Bytes(Value),
          'Inspector display text changed through native save/reload (' +
          IntToStr(Length(Value)) + ' vs ' +
          IntToStr(Length(DecodeTpXTextForEdit(ReloadedText.Text))) + ' bytes)');
      finally
        Loader.Free;
        Reloaded.Free;
      end;
    finally
      DeleteFile(FileName);
    end;
  end;

begin
  SetLiveTeXEnabled(False);
  MainForm.Show;
  MainForm.ShowInspector;
  Application.ProcessMessages;
  CheckSectionOrder;
  Drawing := MainForm.TheDrawing;

  LineObj := TLine2D.CreateSpec(-1, Point2D(1, 2), Point2D(20, 30));
  Drawing.AddObject(-1, LineObj);
  SelectObject(LineObj);
  ChangeCombo(opfLineStyle, Ord(liDashed));
  ChangeText(opfLineColor, '#1278B5');
  ChangeCombo(opfBeginArrowKind, Ord(arrH43));
  ChangeCombo(opfEndArrowKind, Ord(arrT20));
  ChangeText(opfArrowSizeFactor, '1.25');
  Check((LineObj.LineStyle = liDashed) and
    (LineObj.LineColor = HtmlToColor('#1278B5')) and
    (LineObj.BeginArrowKind = arrH43) and
    (LineObj.EndArrowKind = arrT20) and
    (Abs(LineObj.ArrowSizeFactor - 1.25) < 0.0001),
    'Inspector did not edit line and arrow properties through controls');

  RectObj := TRectangle2D.Create(-1);
  Drawing.AddObject(-1, RectObj);
  SelectObject(RectObj);
  ChangeCombo(opfHatching, Ord(haCross));
  ChangeText(opfHatchColor, '#13579B');
  ChangeText(opfFillColor, '#2468AC');
  Check((RectObj.Hatching = haCross) and
    (RectObj.HatchColor = HtmlToColor('#13579B')) and
    (RectObj.FillColor = HtmlToColor('#2468AC')),
    'Inspector did not edit fill and hatching properties through controls');
  ChangeText(opfRectangleRX, '1.25');
  ChangeText(opfRectangleRY, '2.75');
  Check((Abs(RectObj.RX - 1.25) < 0.0001) and
    (Abs(RectObj.RY - 2.75) < 0.0001),
    'Inspector did not edit both rounded-corner radii');
  MainForm.Inspector.FocusObject(RectObj);
  Check(MainForm.Inspector.EditorFor(opfFillColor).Visible,
    'Focusing a rectangle did not reveal its relevant inspector field');

  TextObj := TText2D.CreateSpec(-1, Point2D(5, 6), 7,
    'Legacy &#x3B2; &#x1F600;');
  TextObj.TeXText := '\alpha';
  Drawing.AddObject(-1, TextObj);
  SelectObject(TextObj);
  Memo := MainForm.Inspector.EditorFor(opfText) as TCompositionAwareMemo;
  Check(Memo.Text = 'Legacy β 😀',
    'Inspector did not decode stored Unicode character references');
  ChangeText(opfText, 'Changed plain β 😀');
  Check((TextObj.Text = 'Changed plain &#946; &#55357;&#56832;') and
    (TextObj.TeXText = '\alpha'),
    'Editing Text changed storage/TeX values: ' + TextObj.Text + ' / ' +
      TextObj.TeXText);
  ChangeCombo(opfHAlignment, Ord(ahRight));
  ChangeText(opfTextRotation, '90');
  Check((TextObj.HAlignment = ahRight) and
    (Abs(TextObj.Rot - Pi / 2) < 0.0001),
    'Inspector did not edit text alignment and angle');
  ChangeText(opfTeXText, '\beta');
  Check((TextObj.Text = 'Changed plain &#946; &#55357;&#56832;') and
    (TextObj.TeXText = '\beta'),
    'Editing TeX source changed the independent Text fallback');
  Drawing.TeXFormat := tex_none;
  Drawing.PdfTeXFormat := pdftex_none;
  RoundTripText('alpha');
  RoundTripText('alpha' + LineEnding);
  {$IFDEF LCLCOCOA}
  CheckCompositionEndWithoutTextChange;
  SelectObject(RectObj);
  CheckFieldEditorCompositionEndWithoutTextChange;
  StablePlainStorage := TextObj.Text;
  Check(MainForm.BeginInlineTextEdit(TextObj as TObject2D),
    'Existing text did not open the inline canvas editor');
  InlineMemo := MainForm.InlineTextMemo;
  InlineMemo.Text := 'stable source draft';
  InlineMemo.SelStart := 7;
  InlineMemo.SelLength := 0;
  StableInlineText := InlineMemo.EditText;
  StableInlineCaret := InlineMemo.SelStart;
  Check(TextObj.TeXText = '\beta',
    'Inline typing changed the model before Done');
  InlineTextView := CocoaInspectorTextView(InlineMemo);
  InlineTextView.supressTextChangeEvent := 1;
  try
    CocoaSetMarkedText(InlineMemo, 'unfinished composition');
    Check(InlineMemo.CompositionPending,
      'Native Cocoa inline preedit did not hold the edit session');
    CocoaCancelMarkedText(InlineMemo);
  finally
    InlineTextView.supressTextChangeEvent := 0;
  end;
  CocoaDrainTextInputQueue;
  Check(not InlineMemo.CompositionPending,
    'Canceled native inline composition left the session pending');
  Check(InlineMemo.EditText = StableInlineText,
    'Canceled native inline composition replaced the stable canvas draft');
  Check(InlineMemo.SelStart = StableInlineCaret,
    'Canceled native inline composition moved the canvas caret');
  Check((TextObj.TeXText = '\beta') and
    (TextObj.Text = StablePlainStorage),
    'Canceled native inline composition changed saved object text');
  Check(MainForm.CommitInlineTextEdit,
    'Stable inline source draft did not commit after composition cancel');
  Check((TextObj.TeXText = 'stable source draft') and
    (TextObj.Text = StablePlainStorage),
    'Inline source commit did not preserve the independent Text fallback');
  {$ENDIF}

  StarObj := TStar2D.CreateSpec(-1, Point2D(8, 9));
  Drawing.AddObject(-1, StarObj);
  SelectObject(StarObj);
  ChangeCombo(opfStarKind, Ord(starCross));
  ChangeText(opfStarSizeFactor, '1.75');
  Check((StarObj.StarKind = starCross) and
    (Abs(StarObj.StarSizeFactor - 1.75) < 0.0001),
    'Inspector did not edit star properties through controls');

  SymbolObj := TSymbol2D.CreateSpec(-1, Point2D(10, 11), 5);
  Drawing.AddObject(-1, SymbolObj);
  SelectObject(SymbolObj);
  ChangeCombo(opfSymbolKind, Ord(symDecision));
  ChangeText(opfSymbolDiameter, '7.5');
  ChangeText(opfSymbolRotation, '45');
  Check((SymbolObj.SymbolKind = symDecision) and
    (Abs(SymbolObj.Diameter - 7.5) < 0.0001) and
    (Abs(SymbolObj.Rot - Pi/4) < 0.0001),
    'Inspector did not edit symbol properties through controls');

  BitmapObj := TBitmap2D.CreateSpec(-1, Point2D(0, 0), Point2D(10, 10),
    Drawing.RegisterBitmap('inspector-old.png'));
  Drawing.AddObject(-1, BitmapObj);
  SelectObject(BitmapObj);
  ChangeText(opfBitmapLink, 'inspector-new.png');
  ChangeCheck(opfKeepAspectRatio, False);
  Check((BitmapObj.BitmapEntry.ImageLink = 'inspector-new.png') and
    not BitmapObj.KeepAspectRatio,
    'Inspector did not edit bitmap link and aspect ratio through controls');
end;

procedure TestFontChoice;
var
  TextObj: TText2D;
  Points: Integer;
  ErrorText: string;
  FontState: TLabel;
  FontNameEditor: TEdit;
  FontStyleEditor: TComboBox;
  HeightEditor: TEdit;
begin
  MainForm.Show;
  MainForm.ShowInspector;
  Application.ProcessMessages;
  TextObj := TText2D.CreateSpec(-1, Point2D(10, 20), 7, 'Size');
  MainForm.TheDrawing.AddObject(-1, TextObj);
  MainForm.TheDrawing.SelectionClear;
  MainForm.TheDrawing.SelectionAdd(TextObj);
  MainForm.RefreshInspectorSelection;
  FontNameEditor := MainForm.Inspector.EditorFor(opfFontName) as TEdit;
  FontState := MainForm.Inspector.FindComponent(
    'InspectorFontNameState') as TLabel;
  FontStyleEditor := MainForm.Inspector.EditorFor(opfFontStyle) as TComboBox;
  HeightEditor := MainForm.Inspector.EditorFor(opfTextHeight) as TEdit;
  for Points in [6, 36] do begin
    MainForm.Inspector.FocusField(opfFontName);
    FontNameEditor.Text := 'Serif';
    Check(MainForm.Inspector.PrepareForSelectionChange(ErrorText),
      'Font family did not commit: ' + ErrorText);
    MainForm.Inspector.FocusField(opfFontStyle);
    FontStyleEditor.ItemIndex := 1;
    if Assigned(FontStyleEditor.OnChange) then
      FontStyleEditor.OnChange(FontStyleEditor);
    Check(MainForm.Inspector.PrepareForSelectionChange(ErrorText),
      'Font style did not commit: ' + ErrorText);
    MainForm.Inspector.FocusField(opfTextHeight);
    HeightEditor.Text := RealTypeToStr(Points * 25.4 / 72);
    Check(MainForm.Inspector.PrepareForSelectionChange(ErrorText),
      'Text height did not commit: ' + ErrorText);
    Check(Abs(TextObj.Height - Points*25.4/72) < 0.0001,
      'Inspector did not preserve semantic text height');
    Check(TextObj.Font.Name = 'Serif', 'Inspector did not preserve font family');
    Check(fsBold in TextObj.Font.Style,
      'Font style was lost at ' + IntToStr(Points) + ' mm; editor=' +
      FontStyleEditor.Text);
  end;
  MainForm.Inspector.FocusField(opfFontName);
  FontNameEditor.Clear;
  Check(MainForm.Inspector.PrepareForSelectionChange(ErrorText),
    'Resetting the font family did not commit: ' + ErrorText);
  Check((TextObj.Font.Name = ' ') and (FontState.Caption = 'Inherited'),
    'Blank font family did not restore inherited-font behavior');
end;

procedure TestConversionSave;
var
  Drawing, Reloaded: TDrawing2D;
  Line: TLine2D;
  Invalid: TBitmap2D;
  Image: TBitmap;
  Loader: T_TpX_Loader;
  FileName, ImageName: string;
  Before, After: TStringList;
  Failed: Boolean;
begin
  Drawing := MainForm.TheDrawing;
  { Native bitmap roundtrip does not require external EPS conversion. }
  Drawing.TeXFormat := tex_none;
  Drawing.PdfTeXFormat := pdftex_none;
  Line := TLine2D.CreateSpec(-1, Point2D(3, 4), Point2D(20, 10));
  Drawing.AddObject(-1, Line); Drawing.SelectionAdd(Line);
  Failed := False;
  try ConvertSelected(Drawing, TBitmap2D) except on E: Exception do Failed := True end;
  Check(Failed and (Drawing.GetObject(0) = Line) and (Drawing.ObjectsCount = 1),
    'Unsupported bitmap conversion did not preserve the line');
  FileName := GetTempFileName(SysUtils.GetTempDir(False), 'tpx-safe-');
  ImageName := FileName + '.bmp';
  Before := TStringList.Create; After := TStringList.Create;
  try
    Check(StoreToFile_TpX(Drawing, FileName, False), 'Valid native save failed');
    Before.LoadFromFile(FileName);
    Invalid := TBitmap2D.Create(-1); Drawing.AddObject(-1, Invalid);
    Failed := False;
    try StoreToFile_TpX(Drawing, FileName, False) except on E: Exception do Failed := True end;
    After.LoadFromFile(FileName);
    Check(Failed and (Before.Text = After.Text), 'Failed save replaced a valid document');
    Drawing.ObjectList.Delete(Invalid.ID);
    Image := TBitmap.Create;
    try
      Image.SetSize(16, 8); Image.Canvas.Brush.Color := clRed;
      Image.Canvas.FillRect(Rect(0, 0, 16, 8)); Image.SaveToFile(ImageName);
    finally Image.Free end;
    Drawing.FileName := FileName;
    Drawing.AddObject(-1, TBitmap2D.CreateSpec(-1, Point2D(0, 0), Point2D(4, 2),
      Drawing.RegisterBitmap(ImageName)));
    Check(StoreToFile_TpX(Drawing, FileName, False), 'Valid linked bitmap did not save');
    Reloaded := TDrawing2D.Create(nil); Loader := T_TpX_Loader.Create(Reloaded);
    try
      Loader.LoadFromFile(FileName);
      Check((Reloaded.ObjectsCount = 2) and (Reloaded.GetObject(1) is TBitmap2D),
        'Linked bitmap did not survive native reload');
      Check(TBitmap2D(Reloaded.GetObject(1)).Bitmap.Width = 16, 'Reloaded image resource changed');
    finally Loader.Free; Reloaded.Free end;
  finally Before.Free; After.Free; DeleteFile(FileName); DeleteFile(ImageName) end;
end;

procedure TestUnsupportedExports;
var
  Format: ExportFormatKind;
  Name: string;
  Failed: Boolean;
  Original, Saved: TStringList;
begin
  Name := GetTempFileName(SysUtils.GetTempDir(False), 'tpx-export-');
  Original := TStringList.Create; Saved := TStringList.Create;
  try
    Original.Text := 'preserve me'; Original.SaveToFile(Name);
    for Format in [export_EMF, export_PNG, export_BMP] do begin
      Failed := False;
      try ExportToFile(MainForm.TheDrawing, Name, Format)
      except on E: Exception do Failed := True end;
      Saved.LoadFromFile(Name);
      Check(Failed and (Original.Text = Saved.Text), 'Unsupported export silently succeeded');
    end;
    MainForm.TheDrawing.AddObject(-1, TLine2D.CreateSpec(-1, Point2D(0, 0), Point2D(20, 10)));
    ExportToFile(MainForm.TheDrawing, Name, export_SVG);
    Saved.LoadFromFile(Name);
    Check(Pos('<svg', Saved.Text) > 0, 'Supported SVG export stopped working');
  finally Original.Free; Saved.Free; DeleteFile(Name) end;
end;

procedure TestLabeledPreview;
const
  LabelValues: array[0..1] of string = ('', 'fig:test');
  CaptionValues: array[0..1] of string = ('', 'Caption');
var
  Drawing: TDrawing2D;
  LabelValue, CaptionValue, Pdf, OriginalName: string;
  Figure: TeXFigureEnvKind;
  Log: TStringList;
begin
  Check((LabelValues[0] = '') and (LabelValues[1] = 'fig:test'),
    'Labeled preview fixtures must preserve the full label');
  Check((CaptionValues[0] = '') and (CaptionValues[1] = 'Caption'),
    'Labeled preview fixtures must preserve the full caption');
  Drawing := MainForm.TheDrawing;
  OriginalName := Drawing.FileName;
  Drawing.TeXFormat := tex_tikz; Drawing.PdfTeXFormat := pdftex_tikz;
  Drawing.AddObject(-1, TLine2D.CreateSpec(-1, Point2D(0, 0), Point2D(20, 10)));
  Log := TStringList.Create;
  try
    for Figure in [fig_none, fig_figure] do
      for LabelValue in LabelValues do
        for CaptionValue in CaptionValues do begin
          Drawing.TeXFigure := Figure; Drawing.FigLabel := LabelValue;
          Drawing.Caption := CaptionValue;
          Check(Run_LaTeX_Temp(Drawing, ltxview_Pdf, Pdf, True, False),
            'Standalone labeled preview failed to compile');
          Log.LoadFromFile(ChangeFileExt(Pdf, '.log'));
          Check(Pos('! LaTeX Error', Log.Text) = 0, 'Preview contains a LaTeX error');
          Check((Drawing.TeXFigure = Figure) and (Drawing.FigLabel = LabelValue)
            and (Drawing.Caption = CaptionValue) and (Drawing.FileName = OriginalName),
            'Standalone preview mutated drawing state');
        end;
  finally Log.Free end;
end;

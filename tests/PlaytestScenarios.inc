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
  HeightEditor, WidthEditor, AngleEditor: TEdit;
  ErrorLabel, SelectionLabel: TLabel;
  LineStyleEditor: TComboBox;
  ScrollControl: TScrollBox;
  ScrollPoint: TPoint;
  ScrollPosition: Integer;
  I: Integer;
  ActiveName: string;
  Key: Word;
  Shift: TShiftState;
  procedure CheckFieldVisibleInInspector(Editor: TControl;
    const FieldName: string);
  begin
    ScrollPoint := Editor.ClientToParent(Point(0, 0), ScrollControl);
    Check((ScrollPoint.Y >= 0) and
      (ScrollPoint.Y + Editor.Height <= ScrollControl.ClientHeight),
      FieldName + ' is outside the inspector viewport: top=' +
      IntToStr(ScrollPoint.Y) + ', height=' + IntToStr(Editor.Height) +
      ', viewport=' + IntToStr(ScrollControl.ClientHeight));
  end;
begin
  MainForm.Show;
  Application.ProcessMessages;
  MainForm.RefreshInspectorSelection;
  SelectionLabel := MainForm.Inspector.FindComponent(
    'InspectorSelectionState') as TLabel;
  WidthEditor := MainForm.Inspector.EditorFor(opfLineWidth) as TEdit;
  HistoryCount := MainForm.TheDrawing.History.Count;
  Check((SelectionLabel.Caption = 'No selection') and
    not MainForm.Inspector.FieldShown(opfLineWidth) and
    (MainForm.TheDrawing.History.Count = HistoryCount),
    'Empty inspector still listed property fields');
  TextObj := TText2D.CreateSpec(-1, Point2D(10, 20), 7, 'Original');
  MainForm.TheDrawing.AddObject(-1, TextObj);
  MainForm.TheDrawing.SelectionClear;
  MainForm.TheDrawing.SelectionAdd(TextObj);
  MainForm.RefreshInspectorSelection;
  HeightEditor := MainForm.Inspector.EditorFor(opfTextHeight) as TEdit;
  WidthEditor := MainForm.Inspector.EditorFor(opfLineWidth) as TEdit;
  AngleEditor := MainForm.Inspector.EditorFor(opfTextRotation) as TEdit;
  ScrollControl := nil;
  for I := 0 to MainForm.Inspector.ComponentCount - 1 do
    if MainForm.Inspector.Components[I] is TScrollBox then
    begin
      ScrollControl := TScrollBox(MainForm.Inspector.Components[I]);
      Break;
    end;
  Check(ScrollControl <> nil, 'Inspector scroll container was not created');
  ErrorLabel := MainForm.Inspector.FindComponent(
    'InspectorTextHeightError') as TLabel;
  for Bad in InvalidHeights do begin
    MainForm.Inspector.FocusField(opfTextHeight);
    CheckFieldVisibleInInspector(HeightEditor, 'Text height editor');
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
  ScrollPosition := ScrollControl.VertScrollBar.Position;
  HeightEditor.SetFocus;
  Application.ProcessMessages;
  Check(ScrollControl.VertScrollBar.Position = ScrollPosition,
    'A blocked invalid-field handoff scrolled to the rejected field');
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
  CheckFieldVisibleInInspector(HeightEditor, 'Text height editor after cancel');
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
    (MainForm.ActiveControl = AngleEditor),
    'Explicit Tab did not commit and move to the next inspector field');
  CheckFieldVisibleInInspector(AngleEditor, 'Text angle editor after Tab');
  Key := VK_TAB;
  Shift := [ssShift];
  AngleEditor.OnKeyDown(AngleEditor, Key, Shift);
  Check((Key = 0) and (MainForm.ActiveControl = HeightEditor),
    'Shift+Tab did not return to the previous inspector field');
  CheckFieldVisibleInInspector(HeightEditor, 'Text height editor after Shift+Tab');
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
  Check((WidthEditor.Text = '') and (WidthEditor.TextHint = 'mixed'),
    'Inspector did not expose a mixed line width');
  HistoryCount := Drawing.History.Count;
  MainForm.Inspector.FocusField(opfLineWidth);
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

procedure TestInspectorDoubleClickSelection;
var
  Drawing, Reloaded: TDrawing2D;
  Vertical, Horizontal, Outside: TRectangle2D;
  P, Q: TPoint2D;
  WidthEditor: TEdit;
  FillEditor: TComboBox;
  Key: Word;
  Shift: TShiftState;
  HistoryCount: Integer;
  FileName: string;
  Loader: T_TpX_Loader;

  function AddRectangle(ID: Integer; X, Y, W, H, Width: TRealType): TRectangle2D;
  begin
    Result := TRectangle2D.Create(ID);
    Result.Points[0] := Point2D(X, Y);
    Result.Points[1] := Point2D(X + W, Y + H);
    Result.Points[2] := Point2D(X, Y + H);
    Result.FillColor := clWhite;
    Result.LineColor := clBlack;
    Result.LineWidth := Width;
    Drawing.AddObject(ID, Result);
  end;

  procedure DoubleClickAt(const Point: TPoint2D);
  var Screen: TPoint2D;
  begin
    Screen := MainForm.LocalView.ViewportToScreen(Point);
    MainForm.EventManager.MouseDown(nil, mbLeft, [], Round(Screen.X), Round(Screen.Y));
    MainForm.EventManager.MouseUp(nil, mbLeft, [], Round(Screen.X), Round(Screen.Y));
    MainForm.EventManager.MouseDown(nil, mbLeft, [], Round(Screen.X), Round(Screen.Y));
    MainForm.EventManager.Mode.OnDblClick(nil, Round(Screen.X), Round(Screen.Y));
    MainForm.EventManager.MouseUp(nil, mbLeft, [], Round(Screen.X), Round(Screen.Y));
  end;
begin
  MainForm.Show;
  MainForm.ShowInspector;
  PumpEvents;
  Drawing := MainForm.TheDrawing;
  MainForm.LocalView.VisualRect := Rect2D(-10, -10, 100, 70);
  UseSnap := False;
  Vertical := AddRectangle(101, 20, 0, 10, 50, 0.5);
  Horizontal := AddRectangle(102, 0, 20, 50, 10, 0.75);
  Outside := AddRectangle(103, 70, 20, 10, 10, 1);
  P := MainForm.LocalView.ViewportToScreen(Point2D(-5, 55));
  Q := MainForm.LocalView.ViewportToScreen(Point2D(55, -5));
  MainForm.EventManager.MouseDown(nil, mbLeft, [], Round(P.X), Round(P.Y));
  MainForm.EventManager.MouseMove(nil, [ssLeft], Round(P.X) + 7, Round(P.Y) + 7);
  MainForm.EventManager.MouseMove(nil, [ssLeft], Round(Q.X), Round(Q.Y));
  MainForm.EventManager.MouseUp(nil, mbLeft, [], Round(Q.X), Round(Q.Y));
  Check(Drawing.SelectedObjects.Count = 2, 'Box selection did not select both rectangles');
  HistoryCount := Drawing.History.Count;
  DoubleClickAt(Point2D(25, 40));
  Check((Drawing.SelectedObjects.Count = 2) and
    (Drawing.SelectedObjects.Find(Vertical.ID) <> nil) and
    (Drawing.SelectedObjects.Find(Horizontal.ID) <> nil),
    'Double-click discarded the other selected rectangle');
  Check(Drawing.History.Count = HistoryCount, 'Opening properties edited the drawing');
  MainForm.Inspector.ShowMore := True;
  WidthEditor := MainForm.Inspector.EditorFor(opfLineWidth) as TEdit;
  Check((WidthEditor.Text = '') and (WidthEditor.TextHint = 'mixed'),
    'Double-click did not retain the mixed width of the full selection');
  MainForm.Inspector.FocusField(opfFillColor);
  FillEditor := MainForm.Inspector.EditorFor(opfFillColor) as TComboBox;
  FillEditor.Text := 'red';
  FillEditor.OnChange(FillEditor);
  Key := VK_TAB;
  Shift := [];
  FillEditor.OnKeyDown(FillEditor, Key, Shift);
  Check((Vertical.FillColor = clRed) and (Horizontal.FillColor = clRed) and
    (Outside.FillColor = clWhite), 'Fill edit did not respect the multi-selection');
  Check((Vertical.LineWidth = 0.5) and (Horizontal.LineWidth = 0.75),
    'Fill edit changed the distinct line widths');
  MainForm.Inspector.FocusField(opfLineWidth);
  WidthEditor.Text := '1.25';
  Key := VK_TAB;
  WidthEditor.OnKeyDown(WidthEditor, Key, Shift);
  Check((Vertical.LineWidth = 1.25) and (Horizontal.LineWidth = 1.25) and
    (Outside.LineWidth = 1), 'Width edit did not respect the multi-selection');
  FileName := GetTempFileName(SysUtils.GetTempDir(False), 'tpx-multiselect-');
  Reloaded := TDrawing2D.Create(nil);
  try
    Check(StoreToFile_TpX(Drawing, FileName, False), 'Could not save the edited cross');
    Loader := T_TpX_Loader.Create(Reloaded);
    try Loader.LoadFromFile(FileName); finally Loader.Free end;
    Vertical := Reloaded.ObjectList.FirstObj as TRectangle2D;
    Horizontal := Reloaded.ObjectList.NextObj as TRectangle2D;
    Check((Vertical.FillColor = clRed) and (Horizontal.FillColor = clRed) and
      (Vertical.LineWidth = 1.25) and (Horizontal.LineWidth = 1.25),
      'Bulk properties did not survive save and reopen');
    Check(IsSamePoint2D(Vertical.Points[0], Point2D(20, 0)) and
      IsSamePoint2D(Horizontal.Points[1], Point2D(50, 30)),
      'Opening properties or bulk editing changed the geometry');
  finally
    Reloaded.Free;
    DeleteFile(FileName);
  end;
  DoubleClickAt(Point2D(75, 25));
  Check((Drawing.SelectedObjects.Count = 1) and
    (Drawing.SelectedObjects.Find(Outside.ID) <> nil),
    'Double-clicking an unselected object retained the previous selection');
  Check(WidthEditor.Text = '1', 'Inspector did not rebind to the unselected object');
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

{$IFDEF LCLCOCOA}
procedure TCompositionObserver.Changed(Sender: TObject; Composing: Boolean);
begin
  if Composing then Inc(Starts)
  else
  begin
    Inc(Ends);
    if Sender is TCompositionAwareEdit then
    begin
      Committed := TCompositionAwareEdit(Sender).CompositionCommitted;
      FinalText := TCompositionAwareEdit(Sender).Text;
    end
    else
    begin
      Committed := TCompositionAwareMemo(Sender).CompositionCommitted;
      FinalText := TCompositionAwareMemo(Sender).Text;
    end;
    if DestroyOnEnd then TWinControl(Sender).Parent.Free;
  end;
end;

procedure TestCocoaLocalComposition;
var
  Form: TForm;
  Control: TWinControl;
  Edit: TCompositionAwareEdit;
  SecondEdit: TCompositionAwareEdit;
  Memo: TCompositionAwareMemo;
  Observer: TCompositionObserver;
  Native: NSTextView;
  Text: NSString;
  IsMemo: Boolean;
  Deadline: QWord;
  procedure Mark(const Value: string);
  begin
    Text := NSStringUtf8(Value);
    try
      Native.setMarkedText_selectedRange_replacementRange(Text,
        NSMakeRange(Text.length, 0), NSMakeRange(NSNotFound, 0));
    finally
      Text.release;
    end;
  end;
  procedure Drain;
  begin
    NSRunLoop.currentRunLoop.runMode_beforeDate(NSDefaultRunLoopMode,
      NSDate.dateWithTimeIntervalSinceNow(0.02));
  end;
begin
  for IsMemo := False to True do
  begin
    Form := TForm.CreateNew(nil);
    Observer := TCompositionObserver.Create;
    try
      if IsMemo then
      begin
        Memo := TCompositionAwareMemo.Create(Form);
        Memo.Parent := Form;
        Memo.Text := 'original';
        Memo.OnCompositionChanged := Observer.Changed;
        Control := Memo;
      end
      else
      begin
        Edit := TCompositionAwareEdit.Create(Form);
        Edit.Parent := Form;
        Edit.Text := 'original';
        Edit.OnCompositionChanged := Observer.Changed;
        Control := Edit;
      end;
      Form.Show;
      Control.SetFocus;
      Application.ProcessMessages;
      if IsMemo then Native := NSTextView(NSScrollView(Control.Handle).documentView)
      else Native := NSTextView(NSTextField(Control.Handle).currentEditor);
      Check(Native <> nil, 'Local composition control has no native editor');
      Native.setSelectedRange(NSMakeRange(0, 8));
      Mark('preedit');
      Check((Observer.Starts = 1) and CocoaControlCompositionPending(Control),
        'TpX control did not observe native composition start');
      Mark('replacement');
      Check(Observer.Starts = 1, 'Preedit replacement delivered a second start');
      Text := NSStringUtf8('committed');
      try
        Native.insertText_replacementRange(Text, NSMakeRange(NSNotFound, 0));
      finally
        Text.release;
      end;
      CocoaDrainTextInputQueue(Control);
      Check((Observer.Ends = 1) and Observer.Committed and
        (Observer.FinalText = 'committed'),
        'Composition end preceded committed text or lost commit intent');
      Mark('cancel');
      Mark('');
      // A new composition must cancel the previous deferred completion.
      Mark('restart');
      Drain;
      Check((Observer.Ends = 1) and CocoaControlCompositionPending(Control),
        'Stale cancellation completed a restarted composition');
      Native.unmarkText;
      CocoaDrainTextInputQueue(Control);
      Check((Observer.Ends = 2) and not Observer.Committed,
        Format('Unmark cancellation did not complete locally (memo=%s, ends=%d, committed=%s, marked=%s)',
          [BoolToStr(IsMemo, True), Observer.Ends,
           BoolToStr(Observer.Committed, True), BoolToStr(Native.hasMarkedText, True)]));
      Mark('destroy');
      Mark('');
      Form.Free;
      Form := nil;
      Drain;
      Check(Observer.Ends = 2, 'Completion called a destroyed control');
    finally
      Form.Free;
      Observer.Free;
    end;

    if not IsMemo then
    begin
      Form := TForm.CreateNew(nil);
      Observer := TCompositionObserver.Create;
      try
        Edit := TCompositionAwareEdit.Create(Form);
        Edit.Parent := Form;
        Edit.OnCompositionChanged := Observer.Changed;
        SecondEdit := TCompositionAwareEdit.Create(Form);
        SecondEdit.Parent := Form;
        SecondEdit.Top := 35;
        SecondEdit.OnCompositionChanged := Observer.Changed;
        Form.Show;
        Edit.SetFocus;
        Application.ProcessMessages;
        Native := NSTextView(NSTextField(Edit.Handle).currentEditor);
        Mark('old control');
        Mark('');
        SecondEdit.SetFocus;
        Application.ProcessMessages;
        Native := NSTextView(NSTextField(SecondEdit.Handle).currentEditor);
        Mark('new control');
        Edit.Free;
        Drain;
        Check(SecondEdit.CompositionPending,
          'Old field-editor owner released the new control composition');
        Mark('');
        CocoaDrainTextInputQueue(SecondEdit);
        Check(not SecondEdit.CompositionPending,
          'Rebound field editor did not complete the new composition');
      finally
        Form.Free;
        Observer.Free;
      end;
    end;

    // Completion is permitted to destroy the control and its native parent.
    Form := TForm.CreateNew(nil);
    Observer := TCompositionObserver.Create;
    try
      if IsMemo then
      begin
        Memo := TCompositionAwareMemo.Create(Form);
        Memo.Parent := Form;
        Memo.OnCompositionChanged := Observer.Changed;
        Control := Memo;
      end
      else
      begin
        Edit := TCompositionAwareEdit.Create(Form);
        Edit.Parent := Form;
        Edit.OnCompositionChanged := Observer.Changed;
        Control := Edit;
      end;
      Form.Show;
      Control.SetFocus;
      Application.ProcessMessages;
      if IsMemo then Native := NSTextView(NSScrollView(Control.Handle).documentView)
      else Native := NSTextView(NSTextField(Control.Handle).currentEditor);
      Mark('destroy in callback');
      Observer.DestroyOnEnd := True;
      Mark('');
      Deadline := GetTickCount64 + 1000;
      repeat
        Drain;
      until (Observer.Ends = 1) or (GetTickCount64 >= Deadline);
      Check(Observer.Ends = 1, 'Destructive completion was not delivered');
      Form := nil;
    finally
      Form.Free;
      Observer.Free;
    end;
  end;
end;
{$ENDIF}

procedure TestInspectorVisibility;
var
  Drawing: TDrawing2D;
  LineObj: TLine2D;
  TextObj: TText2D;
  MenuItem: TMenuItem;
  CloseButton: TSpeedButton;
  HeightEditor: TEdit;
  Title: TLabel;
  ErrorText: string;
  RectObj: TRectangle2D;
  Field: TObjectPropertyField;
  Key: Word;
  KeyMessage: TLMKey;
  procedure SelectOnly(Obj: TObject2D);
  begin
    Drawing.SelectionClear;
    Drawing.SelectionAdd(Obj);
    MainForm.RefreshInspectorSelection;
  end;
begin
  MainForm.Show;
  MainForm.ShowInspector;
  Application.ProcessMessages;
  Drawing := MainForm.TheDrawing;
  Title := MainForm.Inspector.FindComponent('InspectorSelectionState') as TLabel;
  MenuItem := MainForm.FindComponent('InspectorMenuItem') as TMenuItem;
  CloseButton := MainForm.Inspector.FindComponent('InspectorCloseButton')
    as TSpeedButton;
  Check((MenuItem <> nil) and (CloseButton <> nil),
    'Inspector toggle or close control is missing');
  Check(MenuItem.ShortCut = StandardShortcut(Ord('I'), []),
    'View > Inspector has no shortcut');

  LineObj := TLine2D.CreateSpec(-1, Point2D(1, 2), Point2D(20, 30));
  Drawing.AddObject(-1, LineObj);
  TextObj := TText2D.CreateSpec(-1, Point2D(10, 20), 7, 'Label');
  Drawing.AddObject(-1, TextObj);

  MainForm.Inspector.ShowMore := False;
  SelectOnly(LineObj);
  Check(Title.Caption = 'Line', 'Single line title: ' + Title.Caption);
  Check(not MainForm.Inspector.FieldShown(opfText) and
    not MainForm.Inspector.FieldShown(opfTextHeight) and
    not MainForm.Inspector.FieldShown(opfBitmapLink) and
    not MainForm.Inspector.FieldShown(opfLineStyle),
    'A line showed text, bitmap or collapsed style fields');
  MainForm.Inspector.ShowMore := True;
  Check(MainForm.Inspector.FieldShown(opfLineStyle) and
    MainForm.Inspector.FieldShown(opfEndArrowKind) and
    not MainForm.Inspector.FieldShown(opfFontName) and
    MainForm.InspectorShowMore,
    'More did not reveal only the line style fields');
  MainForm.Inspector.ShowMore := False;

  SelectOnly(TextObj);
  Check((Title.Caption = 'Text') and
    MainForm.Inspector.FieldShown(opfText) and
    MainForm.Inspector.FieldShown(opfTeXText) and
    MainForm.Inspector.FieldShown(opfTextHeight) and
    not MainForm.Inspector.FieldShown(opfSymbolKind) and
    not MainForm.Inspector.FieldShown(opfRectangleRX),
    'A text did not show exactly its own properties');

  Drawing.SelectionAdd(LineObj);
  MainForm.RefreshInspectorSelection;
  Check((Title.Caption = '2 objects') and
    MainForm.Inspector.FieldShown(opfText) = False,
    'A text plus line selection offered text-only fields');

  Drawing.SelectionClear;
  MainForm.RefreshInspectorSelection;
  Check((Title.Caption = 'No selection') and
    not MainForm.Inspector.FieldShown(opfLineStyle),
    'Empty selection still listed fields');

  CloseButton.Click;
  Check(not MainForm.InspectorPanelVisible and
    not MainForm.Inspector.Parent.Visible and not MenuItem.Checked,
    'The close control did not hide the inspector');
  MenuItem.Click;
  Check(MainForm.InspectorPanelVisible and
    MainForm.Inspector.Parent.Visible and MenuItem.Checked,
    'View > Inspector did not show the inspector again');

  { Typing into a focused field never triggers single-key canvas tools. }
  RectObj := TRectangle2D.Create(-1);
  RectObj.Points[0] := Point2D(30, 30);
  RectObj.Points[1] := Point2D(50, 45);
  RectObj.Points[2] := Point2D(30, 45);
  Drawing.AddObject(-1, RectObj);
  SelectOnly(RectObj);
  for Field in [opfFillColor, opfLineWidth, opfRectangleRX] do
  begin
    MainForm.Inspector.FocusField(Field);
    Application.ProcessMessages;
    Check(MainForm.ActiveControl = MainForm.Inspector.EditorFor(Field),
      'Field ' + IntToStr(Ord(Field)) + ' did not take focus');
    for Key in [VK_L, VK_T, VK_G, VK_E, VK_O, VK_SPACE, VK_ADD] do
    begin
      FillChar(KeyMessage, SizeOf(KeyMessage), 0);
      KeyMessage.CharCode := Key;
      Check(not MainForm.IsShortcut(KeyMessage),
        'Typing key ' + IntToStr(Key) + ' in field ' + IntToStr(Ord(Field)) +
        ' ran a canvas shortcut');
    end;
  end;
  MainForm.Inspector.CancelActiveEdit;

  { Hiding settles a pending edit instead of dropping it. }
  SelectOnly(TextObj);
  HeightEditor := MainForm.Inspector.EditorFor(opfTextHeight) as TEdit;
  MainForm.Inspector.FocusField(opfTextHeight);
  HeightEditor.Text := '9';
  MenuItem.Click;
  Check(not MainForm.InspectorPanelVisible and (TextObj.Height = 9) and
    not MainForm.Inspector.HasActiveEdit,
    'Hiding the inspector lost or kept open the pending edit');
  MenuItem.Click;
  Check(MainForm.Inspector.PrepareForSelectionChange(ErrorText), ErrorText);
end;

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
  FontStyleCombo, AlignmentCombo: TComboBox;
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
  HandoffHistory: Integer;
  TestKey: Word;
  TestShift: TShiftState;
  ShortcutMessage: TLMKey;
  ShortcutHandled: Boolean;
  SelectedID: Integer;
  SelectedAfter: Integer;
  TextObjectID: Integer;
  TextAnchor: TPoint2D;

  function InspectorTop(Control: TControl): Integer;
  begin
    Result := 0;
    while Control <> nil do
    begin
      Inc(Result, Control.Top);
      Control := Control.Parent;
    end;
  end;

  procedure CheckFieldOrder;
  begin
    MainForm.Inspector.ShowMore := True;
    Check(InspectorTop(MainForm.Inspector.EditorFor(opfLineStyle)) <
      InspectorTop(MainForm.Inspector.EditorFor(opfLineWidth)),
      'Line fields are reversed');
    MainForm.Inspector.ShowMore := False;
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

  procedure DispatchShortcut(Key: Word; Modifiers: TShiftState;
    out Handled: Boolean);
  begin
    FillChar(ShortcutMessage, SizeOf(ShortcutMessage), 0);
    ShortcutMessage.CharCode := Key;
    ShortcutModifiers := Modifiers;
    ShortcutModifierSimulation := True;
    try
      Handled := MainForm.IsShortcut(ShortcutMessage);
    finally
      ShortcutModifierSimulation := False;
      ShortcutModifiers := [];
    end;
  end;

  procedure CheckInspectorKeepsCanvasKey(Key: Word;
    Modifiers: TShiftState; const KeyName: string);
  begin
    SelectObject(TextObj);
    MainForm.Inspector.FocusField(opfFontStyle);
    FontStyleCombo.SetFocus;
    Application.ProcessMessages;
    Check(MainForm.ActiveControl = FontStyleCombo,
      KeyName + ' setup did not focus the Inspector combo');
    SelectedID := Drawing.SelectedObjects.FirstObj.ID;
    TextAnchor := TextObj.Points[0];
    DispatchShortcut(Key, Modifiers, ShortcutHandled);
    if Drawing.SelectedObjects.Count = 0 then
      SelectedAfter := -1
    else
      SelectedAfter := Drawing.SelectedObjects.FirstObj.ID;
    Check(not ShortcutHandled,
      KeyName + ' was consumed by a canvas shortcut while the Inspector owned focus; ' +
      'before=' + IntToStr(SelectedID) + ', after=' +
      IntToStr(SelectedAfter));
    Check((Drawing.SelectedObjects.Count = 1) and
      (Drawing.SelectedObjects.FirstObj.ID = SelectedID) and
      (SelectedID = TextObj.ID),
      KeyName + ' changed the selected Inspector target');
    Check(IsSamePoint2D(TextObj.Points[0], TextAnchor),
      KeyName + ' moved the selected text object');
  end;

begin
  SetLiveTeXEnabled(False);
  MainForm.Show;
  MainForm.ShowInspector;
  Application.ProcessMessages;
  Drawing := MainForm.TheDrawing;

  LineObj := TLine2D.CreateSpec(-1, Point2D(1, 2), Point2D(20, 30));
  Drawing.AddObject(-1, LineObj);
  SelectObject(LineObj);
  CheckFieldOrder;
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
  FontStyleCombo := MainForm.Inspector.EditorFor(opfFontStyle) as TComboBox;
  AlignmentCombo := MainForm.Inspector.EditorFor(opfHAlignment) as TComboBox;
  CheckInspectorKeepsCanvasKey(VK_TAB, [], 'Tab');
  CheckInspectorKeepsCanvasKey(VK_TAB, [ssShift], 'Shift+Tab');
  CheckInspectorKeepsCanvasKey(VK_DOWN, [], 'ArrowDown');
  SelectObject(TextObj);
  MainForm.ActiveControl := MainForm.LocalView;
  Application.ProcessMessages;
  DispatchShortcut(VK_TAB, [], ShortcutHandled);
  Check(ShortcutHandled and (Drawing.SelectedObjects.Count = 1) and
    (Drawing.SelectedObjects.FirstObj.ID = LineObj.ID),
    'Canvas Tab no longer dispatches Select next when the canvas owns focus');
  SelectObject(LineObj);
  MainForm.ActiveControl := MainForm.LocalView;
  Application.ProcessMessages;
  DispatchShortcut(VK_TAB, [ssShift], ShortcutHandled);
  Check(ShortcutHandled and (Drawing.SelectedObjects.Count = 1) and
    (Drawing.SelectedObjects.FirstObj.ID = TextObj.ID),
    'Canvas Shift+Tab no longer dispatches Select previous when the canvas owns focus');
  SelectObject(TextObj);
  HandoffHistory := Drawing.History.Count;
  MainForm.Inspector.FocusField(opfFontStyle);
  FontStyleCombo.ItemIndex := 1;
  if Assigned(FontStyleCombo.OnChange) then
    FontStyleCombo.OnChange(FontStyleCombo);
  Check(MainForm.Inspector.HasActiveEdit and
    (fsBold in TextObj.Font.Style) and
    (Drawing.History.Count = HandoffHistory),
    'First combo did not remain a live, uncommitted inspector edit');
  AlignmentCombo.SetFocus;
  Application.ProcessMessages;
  Check(MainForm.Inspector.HasActiveEdit and
    (fsBold in TextObj.Font.Style) and
    (Drawing.History.Count = HandoffHistory),
    'Moving between combo controls committed the first field before a change');
  AlignmentCombo.ItemIndex := Ord(ahCenter);
  if Assigned(AlignmentCombo.OnChange) then
    AlignmentCombo.OnChange(AlignmentCombo);
  Check((fsBold in TextObj.Font.Style) and
    (TextObj.HAlignment = ahCenter) and
    (Drawing.History.Count = HandoffHistory + 1),
    'Changing the second combo lost its new value while committing the first: '
    + 'style=' + IntToStr(FontStyleCombo.ItemIndex) + ', alignment=' +
    IntToStr(AlignmentCombo.ItemIndex) + ', modelAlignment=' +
    IntToStr(Ord(TextObj.HAlignment)) + ', history=' +
    IntToStr(Drawing.History.Count) + ', expectedHistory=' +
    IntToStr(HandoffHistory + 1));
  Check(MainForm.Inspector.PrepareForSelectionChange(ErrorText),
    'Second combo did not commit after a combo-to-combo handoff: ' + ErrorText);
  Check(Drawing.History.Count = HandoffHistory + 2,
    'The two combo edits did not produce separate undo boundaries');
  TextObjectID := TextObj.ID;
  Drawing.History.Undo;
  TextObj := Drawing.GetObject(TextObjectID) as TText2D;
  Check((fsBold in TextObj.Font.Style) and
    (TextObj.HAlignment = ahLeft),
    'Undo did not restore only the second combo edit');
  Drawing.History.Undo;
  TextObj := Drawing.GetObject(TextObjectID) as TText2D;
  Check(not (fsBold in TextObj.Font.Style) and
    (TextObj.HAlignment = ahLeft),
    'Undo did not restore the first combo edit');
  SelectObject(TextObj);
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
  TestKey := VK_RIGHT;
  TestShift := [];
  Check(Assigned(InlineMemo.OnKeyUp),
    'Inline editor did not install its caret tracking handler');
  InlineMemo.OnKeyUp(InlineMemo, TestKey, TestShift);
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
  CocoaDrainTextInputQueue(InlineMemo);
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
  Check((TextObj.Font.Name = ' ') and (FontNameEditor.TextHint = 'inherited'),
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

procedure TestSelectionDrag;
var
  Lines: array[0..1] of TLine2D;
  Before: array[0..1, 0..1] of TPoint2D;
  I: Integer;
  procedure Snapshot;
  var J: Integer;
  begin
    for J := 0 to 1 do begin
      Before[J, 0] := Lines[J].Points[0];
      Before[J, 1] := Lines[J].Points[1];
    end;
  end;
  procedure Fixture;
  begin
    MainForm.TheDrawing.SelectionClear;
    MainForm.EventManager.SendMessage(Msg_Escape, nil);
    MainForm.TheDrawing.SelectionAdd(Lines[0]);
    Snapshot;
    Check(MainForm.EventManager.Mode = BaseMode,
      'The selection fixture is not in the normal mode');
  end;
  procedure CheckUnmoved(const Context: string; First, Last: Integer);
  var J: Integer;
  begin
    for J := First to Last do
      Check(IsSamePoint2D(Lines[J].Points[0], Before[J, 0]) and
        IsSamePoint2D(Lines[J].Points[1], Before[J, 1]),
        Context + ' moved line ' + IntToStr(J));
  end;
begin
  UseSnap := False;
  MainForm.Show;
  PumpEvents;
  MainForm.LocalView.VisualRect := Rect2D(0, 0,
    MainForm.LocalView.ClientWidth, MainForm.LocalView.ClientHeight);
  Lines[0] := TLine2D.CreateSpec(-1,
    MainForm.LocalView.ScreenToViewport(Point2D(40, 50)),
    MainForm.LocalView.ScreenToViewport(Point2D(160, 50)));
  Lines[1] := TLine2D.CreateSpec(-1,
    MainForm.LocalView.ScreenToViewport(Point2D(40, 120)),
    MainForm.LocalView.ScreenToViewport(Point2D(160, 120)));
  for I := 0 to 1 do begin
    MainForm.TheDrawing.AddObject(-1, Lines[I]);
  end;
    { A press on empty canvas with a drag deselects and moves nothing. }
    Fixture;
    MainForm.EventManager.MouseDown(nil, mbLeft, [], 100, 200);
    MainForm.EventManager.MouseMove(nil, [ssLeft], 120, 213);
    MainForm.EventManager.MouseUp(nil, mbLeft, [], 120, 213);
    CheckUnmoved('Empty canvas drag', 0, 1);
    Check(MainForm.TheDrawing.SelectedObjects.Count = 0,
      'Empty canvas drag kept the old selection');
    Check(MainForm.EventManager.Mode = BaseMode,
      'Empty canvas drag left a transform mode');
    { A plain click on empty canvas only deselects. }
    Fixture;
    MainForm.EventManager.MouseDown(nil, mbLeft, [], 100, 200);
    MainForm.EventManager.MouseUp(nil, mbLeft, [], 100, 200);
    CheckUnmoved('Empty canvas click', 0, 1);
    Check(MainForm.TheDrawing.SelectedObjects.Count = 0,
      'Empty canvas click kept the selection');
    { Pointer jitter during a click is not a drag, on or off the object. }
    Fixture;
    MainForm.EventManager.MouseDown(nil, mbLeft, [], 100, 50);
    MainForm.EventManager.MouseMove(nil, [ssLeft], 103, 52);
    MainForm.EventManager.MouseUp(nil, mbLeft, [], 103, 52);
    CheckUnmoved('Click jitter on the selection', 0, 1);
    MainForm.EventManager.MouseDown(nil, mbLeft, [], 100, 120);
    MainForm.EventManager.MouseMove(nil, [ssLeft], 103, 122);
    MainForm.EventManager.MouseUp(nil, mbLeft, [], 103, 122);
    CheckUnmoved('Click jitter on another object', 0, 1);
    Check(MainForm.TheDrawing.SelectedObjects.Find(Lines[1].ID) <> nil,
      'Click on another object did not select it');
    { Pointer motion without a press seen by the canvas never drags the
      object that was clicked before. }
    Fixture;
    MainForm.EventManager.MouseDown(nil, mbLeft, [], 100, 50);
    MainForm.EventManager.MouseUp(nil, mbLeft, [], 100, 50);
    MainForm.EventManager.MouseMove(nil, [ssLeft], 100, 90);
    MainForm.EventManager.MouseUp(nil, mbLeft, [], 100, 90);
    CheckUnmoved('Motion without a press', 0, 1);
    { A press on another object with a drag moves that object only. }
    Fixture;
    MainForm.EventManager.MouseDown(nil, mbLeft, [], 100, 120);
    MainForm.EventManager.MouseMove(nil, [ssLeft], 100, 140);
    MainForm.EventManager.MouseUp(nil, mbLeft, [], 100, 140);
    CheckUnmoved('Dragging another object', 0, 0);
    Check(Abs(Lines[1].Points[0].Y - Before[1, 0].Y) > 1e-6,
      'Dragging an unselected object did not move it');
    { Dragging the selected object itself still moves it. }
    Fixture;
    MainForm.EventManager.MouseDown(nil, mbLeft, [], 100, 50);
    MainForm.EventManager.MouseMove(nil, [ssLeft], 100, 70);
    MainForm.EventManager.MouseUp(nil, mbLeft, [], 100, 70);
    Check(Abs(Lines[0].Points[0].Y - Before[0, 0].Y) > 1e-6,
      'Dragging the selected object no longer moves it');
end;

procedure TestArcFlip;
const
  Tolerance = 1e-6;
var
  Arc: TArc2D;
  Group: TContainer2D;
  Center: TPoint2D;
  CX, CY, R, SA, EA: TRealType;

  function Normalized(A: TRealType): TRealType;
  begin
    Result := A - 2 * Pi * Floor(A / (2 * Pi));
    if Result > 2 * Pi - Tolerance then Result := 0;
  end;

  procedure CheckArc(const Name: string; ExpectedSA, ExpectedEA: TRealType);
  begin
    Arc.GetArcParams(CX, CY, R, SA, EA);
    Check((Abs(CX - Center.X) < Tolerance) and (Abs(CY - Center.Y) < Tolerance)
      and (Abs(R - 10) < Tolerance), Name + ' moved or resized the arc');
    Check((Abs(Normalized(SA) - Normalized(ExpectedSA)) < Tolerance) and
      (Abs(Normalized(EA) - Normalized(ExpectedEA)) < Tolerance),
      Format('%s gave start %.4f and end %.4f instead of %.4f and %.4f',
      [Name, Normalized(SA), Normalized(EA), Normalized(ExpectedSA),
      Normalized(ExpectedEA)]));
  end;

  function NewArc(StartAngle, EndAngle: TRealType): TArc2D;
  begin
    Result := TArc2D.CreateSpec(-1, Center, 10, StartAngle, EndAngle);
    Result.BeginArrowKind := arrH41;
    Result.EndArrowKind := arrNone;
  end;

begin
  Center := Point2D(30, 20);
  { A 315 degree arc with its gap between 0 and 45 degrees. A mirror keeps
    the sweep: x -> -x maps angle t to 180 - t, y -> -y maps t to -t. }
  Arc := NewArc(Pi / 4, 2 * Pi);
  try
    Arc.Transform(Flip2D(Center, V2D(1, 0)));
    CheckArc('Horizontal flip', Pi, 3 * Pi / 4);
    Check((Arc.BeginArrowKind = arrNone) and (Arc.EndArrowKind = arrH41),
      'Horizontal flip moved the arrowhead to the other visual end');
    Arc.Transform(Flip2D(Center, V2D(1, 0)));
    CheckArc('Double horizontal flip', Pi / 4, 0);
    Check((Arc.BeginArrowKind = arrH41) and (Arc.EndArrowKind = arrNone),
      'Double horizontal flip did not restore the arrowheads');
    Arc.Transform(Flip2D(Center, V2D(0, 1)));
    CheckArc('Vertical flip', 0, -Pi / 4);
  finally
    Arc.Free;
  end;
  { A rotated arc from 75 to 30 degrees: mirrored to 150 .. 105 degrees. }
  Arc := NewArc(75 * Pi / 180, 30 * Pi / 180);
  try
    Arc.Transform(Flip2D(Center, V2D(1, 0)));
    CheckArc('Rotated arc flip', 150 * Pi / 180, 105 * Pi / 180);
    { Rotation keeps orientation, so it must not swap the ends. }
    Arc.Transform(RotateCenter2D(Pi / 2, Center));
    Arc.GetArcParams(CX, CY, R, SA, EA);
    Check(Abs(Normalized(EA - SA) - Normalized(-45 * Pi / 180)) < Tolerance,
      'Rotation changed the sweep of a mirrored arc');
  finally
    Arc.Free;
  end;
  { Grouped arcs are transformed through their compound. }
  Arc := NewArc(Pi / 4, 2 * Pi);
  Group := TContainer2D.CreateSpec(-1, [Arc]);
  try
    Group.Transform(Flip2D(Center, V2D(1, 0)));
    CheckArc('Grouped horizontal flip', Pi, 3 * Pi / 4);
  finally
    Group.Free;
  end;
end;

procedure TestUnsupportedExports;
var
  Format: ExportFormatKind;
  Name, OldGhostscriptPath: string;
  Failed: Boolean;
  Original, Saved: TStringList;
begin
  Name := GetTempFileName(SysUtils.GetTempDir(False), 'tpx-export-');
  Original := TStringList.Create; Saved := TStringList.Create;
  OldGhostscriptPath := GhostscriptPath;
  try
    Original.Text := 'preserve me'; Original.SaveToFile(Name);
    { EMF is unsupported; PNG/BMP preserve their target when a converter fails. }
    GhostscriptPath := GetEnvironmentVariable('TPX_FAILED_TOOL');
    Check(FileExists(GhostscriptPath), 'Failing converter fixture is missing');
    for Format in [export_EMF, export_PNG, export_BMP] do begin
      Failed := False;
      try ExportToFile(MainForm.TheDrawing, Name, Format)
      except on E: Exception do Failed := True end;
      Saved.LoadFromFile(Name);
      Check(Failed and (Original.Text = Saved.Text), 'Unsupported export silently succeeded');
    end;
    GhostscriptPath := OldGhostscriptPath;
    MainForm.TheDrawing.AddObject(-1, TLine2D.CreateSpec(-1, Point2D(0, 0), Point2D(20, 10)));
    ExportToFile(MainForm.TheDrawing, Name, export_SVG);
    Saved.LoadFromFile(Name);
    Check(Pos('<svg', Saved.Text) > 0, 'Supported SVG export stopped working');
  finally
    GhostscriptPath := OldGhostscriptPath;
    Original.Free; Saved.Free; DeleteFile(Name)
  end;
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

procedure TestUnknownArrowCodes;
const
  Header = '%<TpX v="5" TeXFormat="tikz" PdfTeXFormat="tikz" ArrowsSize="0.7" ' +
    'StarsSize="1" DefaultFontHeight="5" DefaultSymbolSize="30" ' +
    'ApproximationPrecision="0.01" PicScale="1" Border="2" BitmapRes="20000" ' +
    'HatchingStep="2" DottedSize="0.5" DashSize="1" LineWidth="0.3" ' +
    'TeXCenterFigure="1" TeXFigure="none">' + LineEnding;
var
  Drawing: TDrawing2D;
  Loader: T_TpX_Loader;
  Source: TStringList;
  FileName, Saved: string;
  Line: TLine2D;
  Stream: TFileStream;
  I: Integer;
begin
  Drawing := TDrawing2D.Create(nil);
  FileName := GetTempFileName(SysUtils.GetTempDir(False), 'tpx-arrow-codes-');
  Source := TStringList.Create;
  try
    Source.Text := Header +
      '%  <line x1="0" y1="0" x2="10" y2="0" arr1="999" arr2="h30" arrs="1" lw="1.00"/>' + LineEnding +
      '%  <line x1="0" y1="5" x2="10" y2="5" arr1="-3" arr2="h44" arrs="1" lw="1.00"/>' + LineEnding +
      '%</TpX>' + LineEnding;
    Source.SaveToFile(FileName);
    Loader := T_TpX_Loader.Create(Drawing);
    try Loader.LoadFromFile(FileName); finally Loader.Free end;
    Line := Drawing.ObjectList.FirstObj as TLine2D;
    Check((Line.BeginArrowKind = arrNone) and (Line.EndArrowKind = arrNone),
      'Unknown arrow codes did not load as no arrow');
    Line := Drawing.ObjectList.NextObj as TLine2D;
    Check((Line.BeginArrowKind = arrNone) and (Line.EndArrowKind = arrH44),
      'Valid arrow beside an out-of-range code was not preserved');
    Check(StoreToFile_TpX(Drawing, FileName, False),
      'Could not save the drawing with unknown arrow codes');
    Stream := TFileStream.Create(FileName, fmOpenRead);
    try
      SetLength(Saved, Stream.Size);
      Stream.ReadBuffer(Saved[1], Stream.Size);
    finally
      Stream.Free;
    end;
    for I := 1 to Length(Saved) do
      Check((Saved[I] >= #32) or (Saved[I] in [#9, #10, #13]),
        'Saved file contains a control byte');
    Check(Pos('arr2="h44"', Saved) > 0, 'Saved file lost the valid arrow');
    Check(Pos('h30', Saved) = 0, 'Saved file kept the unknown arrow code');
  finally
    Source.Free;
    Drawing.Free;
    DeleteFile(FileName);
  end;
end;

procedure TestBezierArrowDirection;
  function CentroidX(Piece: TPiece): TRealType;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to Piece.Count - 1 do Result := Result + Piece[I].X;
    Result := Result / Piece.Count;
  end;
var
  Curve: TBezierPath2D;
begin
  { Both end segments are much shorter than the h44 head, which must still
    point outward from the curve ends along the curve. }
  Curve := TBezierPath2D.CreateSpec(-1, [
    Point2D(0, 0), Point2D(0.8, -2), Point2D(1.7, 2), Point2D(2.5, 0),
    Point2D(6, 6), Point2D(11.5, 6), Point2D(15, 0),
    Point2D(15.8, 2), Point2D(16.7, -2), Point2D(17.5, 0)]);
  try
    Curve.BeginArrowKind := arrH44;
    Curve.EndArrowKind := arrH44;
    Curve.UpdateExtension(Curve);
    Check(Curve.Pieces.Count = 3, 'Bezier curve did not produce two arrowheads');
    Check(CentroidX(Curve.Pieces[1]) > 0.5,
      'Begin arrowhead of a curve with a short first segment points backwards');
    Check(CentroidX(Curve.Pieces[2]) < 17.5 - 0.5,
      'End arrowhead of a curve with a short last segment points backwards');
  finally
    Curve.Free;
  end;
end;

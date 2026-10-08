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

procedure TestAsyncTools;
const
{$IFDEF WINDOWS}
  AsyncCommand = 'powershell -NoProfile -NonInteractive -Command "Start-Sleep -Seconds 1; Set-Content finished done"';
  WaitCommand = 'powershell -NoProfile -NonInteractive -Command "Start-Sleep -Milliseconds 100; Set-Content finished waited"';
{$ELSE}
  AsyncCommand = 'sleep 1; printf done > finished';
  WaitCommand = 'sleep 0.1; printf waited > finished';
{$ENDIF}
var
  Directory, Marker: string;
  Started: QWord;
  Contents: TStringList;
begin
  Directory := GetTempFileName(SysUtils.GetTempDir(False), 'tpx-async-');
  DeleteFile(Directory); Check(CreateDir(Directory), 'Could not make async directory');
  Marker := Directory + PathDelim + 'finished';
  Started := GetTickCount64;
  try
    Check(FileExec(AsyncCommand, '', '', Directory, True, False),
      'Async launch failed');
    Check(GetTickCount64 - Started < 700, 'Async command blocked its caller');
    Check(not FileExists(Marker), 'Child finished before async return');
    while not FileExists(Marker) and (GetTickCount64 - Started < 4000) do Sleep(20);
    Check(FileExists(Marker), 'Async child did not finish');
    Check(FileExec(WaitCommand, '', '', Directory, True, True),
      'Waiting command failed');
    Contents := TStringList.Create;
    try
      Contents.LoadFromFile(Marker);
      Check(Trim(Contents.Text) = 'waited', 'Wait=True returned before completion');
    finally Contents.Free end;
  finally DeleteFile(Marker); RemoveDir(Directory) end;
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
var
  TextObj: TText2D;
  CanClose: Boolean;
  Bad: string;
begin
  TextObj := TText2D.CreateSpec(-1, Point2D(10, 20), 7, 'Original');
  MainForm.TheDrawing.AddObject(-1, TextObj);
  MainForm.TheDrawing.SelectionAdd(TextObj);
  PropertiesForm.PObject := TextObj;
  for Bad in ['-1', '0', 'NaN', 'Inf', 'bad'] do begin
    PropertiesForm.FormShow(nil);
    PropertiesForm.LabeledEdit1.Text := 'Changed';
    PropertiesForm.LabeledEdit2.Text := Bad;
    PropertiesForm.ModalResult := mrOK;
    CanClose := True;
    PropertiesForm.FormCloseQuery(nil, CanClose);
    Check(not CanClose, 'Invalid text height was accepted: ' + Bad);
    Check((TextObj.Height = 7) and (TextObj.Text = 'Original'), 'Invalid edit mutated text');
  end;
  for Bad in ['-1', 'NaN', 'Inf', 'bad'] do begin
    PropertiesForm.FormShow(nil);
    PropertiesForm.ComboBox6.Text := Bad;
    PropertiesForm.ComboBox1.ItemIndex := Ord(liNone);
    PropertiesForm.ModalResult := mrOK; CanClose := True;
    PropertiesForm.FormCloseQuery(nil, CanClose);
    Check(not CanClose, 'Invalid stroke width was accepted: ' + Bad);
    Check((TextObj.LineWidth = 1) and (TextObj.LineStyle = liSolid), 'Invalid width mutated style');
  end;
  PropertiesForm.FormShow(nil);
  PropertiesForm.LabeledEdit2.Text := '0.5';
  PropertiesForm.ComboBox6.Text := '0';
  PropertiesForm.ModalResult := mrOK; CanClose := True;
  PropertiesForm.FormCloseQuery(nil, CanClose);
  Check(CanClose and (TextObj.Height = 0.5) and (TextObj.LineWidth = 0),
    'Fractional height or zero hairline was rejected');
  MainForm.TheDrawing.New_FontHeight := 0.5;
  MainForm.TheDrawing.New_LineWidth := 0;
  for Bad in ['-1', '0', 'NaN', 'bad'] do begin
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

procedure TestFontChoice;
var
  TextObj: TText2D;
  Points: Integer;
  CanClose: Boolean;
begin
  TextObj := TText2D.CreateSpec(-1, Point2D(10, 20), 7, 'Size');
  MainForm.TheDrawing.AddObject(-1, TextObj);
  PropertiesForm.PObject := TextObj;
  for Points in [6, 36] do begin
    PropertiesForm.FormShow(nil);
    PropertiesForm.FontCheckBox.Checked := True;
    PropertiesForm.FontDialog1.Font.Name := 'Serif';
    PropertiesForm.FontDialog1.Font.Style := [fsBold];
    PropertiesForm.FontDialog1.Font.Size := Points;
    PropertiesForm.ApplyFontChoice;
    Check(TextObj.Height = 7, 'Chooser mutated the object before OK');
    PropertiesForm.ModalResult := mrCancel; CanClose := True;
    PropertiesForm.FormCloseQuery(nil, CanClose);
    Check(TextObj.Height = 7, 'Cancel changed semantic height');
    PropertiesForm.ModalResult := mrOK;
    PropertiesForm.FormCloseQuery(nil, CanClose);
    Check(CanClose and (Abs(TextObj.Height - Points*25.4/72) < 0.0001),
      'Font point size did not update semantic height');
    Check(fsBold in TextObj.Font.Style, 'Font style was lost');
    TextObj.Height := 7;
  end;
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
var
  Drawing: TDrawing2D;
  LabelValue, CaptionValue, Pdf, OriginalName: string;
  Figure: TeXFigureEnvKind;
  Log: TStringList;
begin
  Drawing := MainForm.TheDrawing;
  OriginalName := Drawing.FileName;
  Drawing.TeXFormat := tex_tikz; Drawing.PdfTeXFormat := pdftex_tikz;
  Drawing.AddObject(-1, TLine2D.CreateSpec(-1, Point2D(0, 0), Point2D(20, 10)));
  Log := TStringList.Create;
  try
    for Figure in [fig_none, fig_figure] do
      for LabelValue in ['', 'fig:test'] do
        for CaptionValue in ['', 'Caption'] do begin
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

procedure TestFineTransformShortcuts;
var
  Line: TLine2D;
  TextObj: TText2D;
  Editor: TCustomEdit;
  HistoryCount: Integer;

  procedure CheckLine(Angle, Length: Double; const Description: string);
  var
    DX, DY: Double;
  begin
    DX := Line.Points[1].X - Line.Points[0].X;
    DY := Line.Points[1].Y - Line.Points[0].Y;
    Check(Abs(DX - Length * Cos(DegToRad(Angle))) < 0.0001,
      Description + ': wrong horizontal extent');
    Check(Abs(DY - Length * Sin(DegToRad(Angle))) < 0.0001,
      Description + ': wrong vertical extent');
    Check((Abs(Line.Points[0].X + Line.Points[1].X - 20) < 0.0001) and
      (Abs(Line.Points[0].Y + Line.Points[1].Y) < 0.0001),
      Description + ': changed the selection center');
  end;

  procedure CheckTransformKeys(const Modifier: string);
  begin
    MainForm.LocalView.SetFocus;
    Application.ProcessMessages;
    Check(MainForm.ActiveControl = MainForm.LocalView, 'Canvas is not focused');
    SendNativeKey(Modifier + '+Left');
    CheckLine(1, 20, Modifier + '+Left');
    SendNativeKey(Modifier + '+Right');
    CheckLine(0, 20, Modifier + '+Right');
    SendNativeKey(Modifier + '+Up');
    CheckLine(0, 20.2, Modifier + '+Up');
    SendNativeKey(Modifier + '+Down');
    CheckLine(0, 20, Modifier + '+Down');
  end;

  procedure CheckEditorKeys(AEditor: TCustomEdit; const Description: string);
  var
    Key: string;
  begin
    Editor := AEditor;
    Editor.SetFocus;
    Application.ProcessMessages;
    Editor.SelStart := Length(Editor.Text);
    Editor.SelLength := 0;
    HistoryCount := MainForm.TheDrawing.History.Count;
    SendNativeKey('ctrl+shift+Left');
    Check(Editor.SelLength > 0, Description + ': word selection was intercepted');
    Editor.SelStart := 0;
    Editor.SelLength := 0;
    SendNativeKey('ctrl+shift+Right');
    Check(Editor.SelLength > 0, Description + ': forward word selection was intercepted');
    for Key in ['ctrl+shift+Up', 'ctrl+shift+Down'] do
      SendNativeKey(Key);
    Check(MainForm.TheDrawing.History.Count = HistoryCount,
      Description + ': transform shortcuts created a drawing edit');
    CheckLine(0, 20, Description);
  end;

begin
  MainForm.Show;
  Application.ProcessMessages;
  Line := TLine2D.CreateSpec(-1, Point2D(0, 0), Point2D(20, 0));
  MainForm.TheDrawing.AddObject(-1, Line);
  MainForm.TheDrawing.SelectionAdd(Line);
  MainForm.RefreshInspectorSelection;
  Application.ProcessMessages;

  Check(MainForm.Rotatecounterclockwise1deg1.ShortCut =
    Menus.ShortCut(VK_LEFT, [ssCtrl, ssShift]), 'Counterclockwise menu shortcut');
  Check(MainForm.Rotateclockwise1deg1.ShortCut =
    Menus.ShortCut(VK_RIGHT, [ssCtrl, ssShift]), 'Clockwise menu shortcut');
  Check(MainForm.Grow11.ShortCut = Menus.ShortCut(VK_UP, [ssCtrl, ssShift]),
    'Grow menu shortcut');
  Check(MainForm.Shrink11.ShortCut = Menus.ShortCut(VK_DOWN, [ssCtrl, ssShift]),
    'Shrink menu shortcut');
  CheckTransformKeys('ctrl+shift');
  CheckTransformKeys('ctrl+alt');
  SendNativeKey('alt+Left');
  CheckLine(90, 20, 'Coarse rotation');
  SendNativeKey('alt+Right');
  CheckLine(0, 20, 'Coarse reverse rotation');

  MainForm.ComboBox6.Text := '0.2';
  MainForm.ComboBox6.SetFocus;
  Application.ProcessMessages;
  MainForm.ComboBox6.SelStart := Length(MainForm.ComboBox6.Text);
  MainForm.ComboBox6.SelLength := 0;
  HistoryCount := MainForm.TheDrawing.History.Count;
  SendNativeKey('ctrl+shift+Left');
  Check(MainForm.ComboBox6.SelLength > 0, 'Toolbar combo lost word selection');
  SendNativeKey('ctrl+shift+Right');
  SendNativeKey('ctrl+shift+Up');
  SendNativeKey('ctrl+shift+Down');
  Check(MainForm.TheDrawing.History.Count = HistoryCount,
    'Toolbar combo dispatched a transform');
  CheckLine(0, 20, 'Toolbar combo');

  MainForm.Inspector.FocusField(opfLineWidth);
  CheckEditorKeys(MainForm.Inspector.EditorFor(opfLineWidth) as TCustomEdit,
    'Inspector edit');
  TextObj := TText2D.CreateSpec(-1, Point2D(40, 20), 7, 'alpha beta');
  MainForm.TheDrawing.AddObject(-1, TextObj);
  MainForm.TheDrawing.SelectionClear;
  MainForm.TheDrawing.SelectionAdd(TextObj);
  MainForm.RefreshInspectorSelection;
  MainForm.Inspector.FocusField(opfText);
  CheckEditorKeys(MainForm.Inspector.EditorFor(opfText) as TCustomEdit,
    'Inspector memo');
  Check((Abs(TextObj.Points[0].X - 40) < 0.0001) and
    (Abs(TextObj.Points[0].Y - 20) < 0.0001) and (Abs(TextObj.Rot) < 0.0001),
    'Inspector memo transformed selected text');
  Check(MainForm.BeginInlineTextEdit(TextObj), 'Could not start inline text edit');
  CheckEditorKeys(MainForm.InlineTextMemo, 'Inline memo');
  Check(MainForm.InlineTextEditActive, 'Transform keys ended inline editing');
  MainForm.CancelInlineTextEdit;
end;

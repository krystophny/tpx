program RuntimeTests;
{$mode Delphi}
uses
  Interfaces, Forms, SysUtils, Classes, Types, Math, Controls, Dialogs, InterfaceBase, LCLType,
  {$IFDEF LCLgtk2}Gtk2Int,{$ENDIF}
  {$IFDEF LCLcocoa}CocoaInt,{$ENDIF}
  {$IFDEF LCLwin32}Win32Int,{$ENDIF}
  Settings0, MainUnit, Propert, Drawings, GObjects, Geometry, Manage, Modes, Input, Devices, Graphics, StdCtrls, GObjBase, SysBasic, Preview, ViewPort, Modify, Output;

{$R ../src/MainUnit.lfm}
{$R ../src/Propert.lfm}

type
  {$IFDEF LCLgtk2}TNativeWidgetSet = TGtk2WidgetSet;{$ENDIF}
  {$IFDEF LCLcocoa}TNativeWidgetSet = TCocoaWidgetSet;{$ENDIF}
  {$IFDEF LCLwin32}TNativeWidgetSet = TWin32WidgetSet;{$ENDIF}
  TTestWidgetSet = class(TNativeWidgetSet)
    function PromptUser(const Caption, Message: string; DialogType: LongInt;
      Buttons: PLongInt; ButtonCount, DefaultIndex, EscapeResult: LongInt): LongInt; override;
  end;
  TObserver = class
    CloseRequests: Integer;
    procedure Closing(Sender: TObject; var Action: TCloseAction);
  end;

var
  Observer: TObserver;
  PromptCount, ReplyButton: Integer;

procedure Check(Condition: Boolean; const Message: string);
begin
  if not Condition then raise Exception.Create(Message);
end;

procedure TObserver.Closing(Sender: TObject; var Action: TCloseAction);
begin
  Inc(CloseRequests);
  Action := caNone;
end;

function AnswerPrompt(const Caption, Message: string; DialogType: LongInt;
  Buttons: PLongInt; ButtonCount, DefaultIndex, EscapeResult: LongInt;
  UseDefaultPos: Boolean; X, Y: LongInt): LongInt;
begin
  if (ParamStr(1) = 'property-dimensions') and
    ((Pos('Line width must', Message) > 0) or (Pos('Text height must', Message) > 0)) then
    Exit(idButtonOK);
  Check(Pos('Save current drawing?', Message) > 0, 'Unexpected dialog: ' + Message);
  Inc(PromptCount);
  Result := ReplyButton;
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

procedure TestExit(const Scenario: string);
var
  ModeBefore: TMode;
  Line: TLine2D;
  Saved: TDrawing2D;
  Loader: T_TpX_Loader;
  SaveName: string;
begin
  PromptCount := 0;
  ReplyButton := idButtonNo;
  if Scenario <> 'exit-clean' then begin
    Line := TLine2D.CreateSpec(-1, Point2D(0, 0), Point2D(20, 10));
    MainForm.TheDrawing.AddObject(-1, Line);
    MainForm.TheDrawing.History.SetPropertiesChanged;
  end;
  if Scenario = 'exit-cancel' then ReplyButton := idButtonCancel;
  SaveName := '';
  if Scenario = 'exit-yes' then begin
    SaveName := GetTempFileName(SysUtils.GetTempDir(False), 'tpx-save-');
    DeleteFile(SaveName);
    SaveName := SaveName + '.tpx';
    MainForm.TheDrawing.FileName := SaveName;
    ReplyButton := idButtonYes;
  end;
  ModeBefore := MainForm.EventManager.Mode;
  if Scenario = 'exit-fallback' then MainForm.OnExit(MainForm)
  else if Scenario = 'exit-close' then MainForm.Close
  else MainForm.UserEventExecute(MainForm.ExitProgram);
  if Scenario = 'exit-clean' then Check(PromptCount = 0, 'Clean drawing prompted')
  else Check(PromptCount = 1, 'Expected one save prompt, got ' + IntToStr(PromptCount));
  if Scenario = 'exit-cancel' then begin
    Check(Observer.CloseRequests = 0, 'Cancel requested closing');
    Check(MainForm.EventManager.Mode = ModeBefore, 'Cancel discarded the active mode');
    Check(MainForm.TheDrawing.ObjectsCount = 1, 'Cancel discarded the drawing');
  end
  else Check(Observer.CloseRequests = 1, 'Exit failed to request closing');
  if Scenario = 'exit-yes' then begin
    Check(FileExists(SaveName), 'Save did not write the drawing');
    Check(not MainForm.TheDrawing.History.IsChanged, 'Saved drawing remains modified');
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
    WidgetSet := TTestWidgetSet.Create;
    Application.Initialize;
    RequireDerivedFormResource := True;
    Application.CreateForm(TMainForm, MainForm);
    Application.CreateForm(TPropertiesForm, PropertiesForm);
    Observer := TObserver.Create;
    MainForm.OnClose := Observer.Closing;
    PromptDialogFunction := AnswerPrompt;
    if Pos('viewport-', ParamStr(1)) = 1 then TestViewport(ParamStr(1))
    else if ParamStr(1) = 'preview-state' then TestPreviewState
    else if ParamStr(1) = 'external-tools' then TestExternalTools
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
    else if ParamStr(1) = 'labeled-preview' then TestLabeledPreview
    else TestExit(ParamStr(1));
    WriteLn('PASS ', ParamStr(1));
  except
    on E: Exception do begin
      WriteLn(StdErr, E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.

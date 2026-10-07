program RuntimeTests;
{$mode Delphi}
uses
  Interfaces, Forms, SysUtils, Classes, Controls, Dialogs, InterfaceBase, LCLType,
  {$IFDEF LCLgtk2}Gtk2Int,{$ENDIF}
  {$IFDEF LCLcocoa}CocoaInt,{$ENDIF}
  Settings0, MainUnit, Propert, Drawings, GObjects, Geometry, Manage, Modes, Input, Devices, Graphics, StdCtrls;

{$R ../src/MainUnit.lfm}
{$R ../src/Propert.lfm}

type
  {$IFDEF LCLgtk2}TNativeWidgetSet = TGtk2WidgetSet;{$ENDIF}
  {$IFDEF LCLcocoa}TNativeWidgetSet = TCocoaWidgetSet;{$ENDIF}
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

procedure TestDrawing(const Scenario: string);
var
  Prim: TPrimitive2D;
  Start, Finish: TPoint2D;
begin
  UseSnap := False;
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
begin
  Check(Abs(MainForm.LocalView.PixelSize - 25.4 / Screen.PixelsPerInch) < 0.001,
    'Default view is not at physical screen scale');
  Check(Abs(MainForm.TheDrawing.LineWidthBase - 0.3) < 0.001,
    'Default view changed exported line width');
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
    SaveName := GetTempFileName(GetTempDir(False), 'tpx-save-');
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
    if Pos('draw-', ParamStr(1)) = 1 then TestDrawing(ParamStr(1))
    else if ParamStr(1) = 'default-view' then TestDefaultView
    else if ParamStr(1) = 'toolbar' then TestToolbar
    else if ParamStr(1) = 'text-selection' then TestTextSelection
    else TestExit(ParamStr(1));
    WriteLn('PASS ', ParamStr(1));
  except
    on E: Exception do begin
      WriteLn(StdErr, E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.

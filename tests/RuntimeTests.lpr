program RuntimeTests;
{$mode Delphi}
uses
  Interfaces, Forms, SysUtils, Classes, Controls, Dialogs, InterfaceBase, LCLType,
  {$IFDEF LCLgtk2}Gtk2Int,{$ENDIF}
  {$IFDEF LCLcocoa}CocoaInt,{$ENDIF}
  Settings0, MainUnit, Propert, Drawings, GObjects, Geometry, Manage, Modes, Input;

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
    TestExit(ParamStr(1));
    WriteLn('PASS ', ParamStr(1));
  except
    on E: Exception do begin
      WriteLn(StdErr, E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.

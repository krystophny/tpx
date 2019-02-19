unit Manage;

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Types, Classes, Controls, Contnrs;

type

  TDblClickEvent = procedure(Sender: TObject;
    X, Y: Integer) of object;

  TEventMessage = LongWord;
  TEventMessages = set of Byte;

  TMode = class;

  TEventManager = class
  private
    fModeStack: TObjectStack;
    fDataStack: TStack;
    fMode: TMode;
    fOnExit: TNotifyEvent;
    fLastMouseX, fLastMouseY: Integer;
    fShiftPressed, fCtrlPressed, fAltPressed: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function PushMode(AMode: TMode): TMode; virtual;
    function PopMode: TMode; virtual;
    procedure SendMessage(Msg: TEventMessage; Sender: TObject); virtual;
    procedure DblClick(Sender: TObject); virtual;
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean); virtual;
    procedure KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState); virtual;
    procedure KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState); virtual;
    property Mode: TMode read fMode;
    property DataStack: TStack read fDataStack;
    property OnExit: TNotifyEvent read fOnExit write fOnExit;
  end;

  TMode = class
  private
    fManager: TEventManager;
  public
    PassMessages: TEventMessages;
    StopMessages: TEventMessages;
    constructor Create;
    function PushMode(AMode: TMode): TMode; virtual;
    function PushData(Data: Pointer): Pointer; virtual;
    procedure PopSelf; virtual;
    function PopData: Pointer; virtual;
    procedure OnPush; virtual;
    procedure OnPop; virtual;
    procedure OnMessage(Msg: TEventMessage; Sender: TObject); virtual;
    procedure OnDblClick(Sender: TObject; X, Y: Integer); virtual;
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure OnMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure OnMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean); virtual;
    procedure OnKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState); virtual;
    procedure OnKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState); virtual;
    property Manager: TEventManager read fManager write fManager;
  end;

const
  Msg_Exit = 1;
  Msg_Stop = Msg_Exit + 1;
  Msg_User = Msg_Stop + 1;

implementation

{=========================================
  TEventManager
=========================================}

constructor TEventManager.Create;
begin
  inherited Create;
  fModeStack := TObjectStack.Create;
  fDataStack := TObjectStack.Create;
end;

destructor TEventManager.Destroy;
begin
  fModeStack.Free;
  fDataStack.Free;
  inherited Destroy;
end;

function TEventManager.PushMode(AMode: TMode): TMode;
begin
  fModeStack.Push(AMode);
  fMode := AMode;
  Result := AMode;
  AMode.Manager := Self;
  AMode.OnPush;
end;

function TEventManager.PopMode: TMode;
begin
  Result := fModeStack.Pop as TMode;
  Result.OnPop;
  if fModeStack.Count > 0 then
    fMode := fModeStack.Peek as TMode
  else
    fMode := nil;
  if not Assigned(fMode) then OnExit(Self);
end;

procedure TEventManager.SendMessage(Msg: TEventMessage;
  Sender: TObject);
begin
  if Assigned(fMode) then fMode.OnMessage(Msg, Sender);
end;

procedure TEventManager.DblClick(Sender: TObject);
begin
  fMode.OnDblClick(Sender, fLastMouseX, fLastMouseY);
end;

procedure TEventManager.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  fMode.OnMouseDown(Sender, Button, Shift, X, Y);
  fLastMouseX := X;
  fLastMouseY := Y;
end;

procedure TEventManager.MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  fMode.OnMouseMove(Sender, Shift, X, Y);
end;

procedure TEventManager.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  fMode.OnMouseUp(Sender, Button, Shift, X, Y);
end;

procedure TEventManager.MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
{$IFDEF VER140}
{$ELSE}
  if fShiftPressed then Shift := Shift + [SSShift];
  if fCtrlPressed then Shift := Shift + [SSCtrl];
  if fAltPressed then Shift := Shift + [SSAlt];
  {fShiftPressed := False;
  fCtrlPressed := False;
  fAltPressed := False;}
{$ENDIF}
  fMode.OnMouseWheel(Sender, Shift, WheelDelta, MousePos, Handled);
end;

procedure TEventManager.KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 16 then fShiftPressed := True;
  if Key = 17 then fCtrlPressed := True;
  if Key = 18 then fAltPressed := True;
end;

procedure TEventManager.KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 16 then fShiftPressed := False;
  if Key = 17 then fCtrlPressed := False;
  if Key = 18 then fAltPressed := False;
end;

{=========================================
  TMode
=========================================}

constructor TMode.Create;
begin
  inherited Create;
  PassMessages := [Msg_Exit];
  StopMessages := [Msg_Stop];
end;

function TMode.PushMode(AMode: TMode): TMode;
begin
  Result := fManager.PushMode(AMode);
end;

function TMode.PushData(Data: Pointer): Pointer;
begin
  Result := fManager.DataStack.Push(Data);
end;

procedure TMode.PopSelf;
begin
  fManager.PopMode;
end;

function TMode.PopData: Pointer;
begin
  Result := fManager.DataStack.Pop;
end;

procedure TMode.OnMessage(Msg: TEventMessage; Sender: TObject);
begin
  if Byte(Msg) in StopMessages then
  begin
    fManager.PopMode;
  end
  else if Byte(Msg) in PassMessages then
  begin
    fManager.PopMode;
    fManager.SendMessage(Msg, Sender);
  end;
end;

procedure TMode.OnDblClick(Sender: TObject; X, Y: Integer);
begin

end;

procedure TMode.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TMode.OnMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TMode.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TMode.OnMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin

end;

procedure TMode.OnKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

end;

procedure TMode.OnKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

end;

procedure TMode.OnPush;
begin

end;

procedure TMode.OnPop;
begin

end;

end.


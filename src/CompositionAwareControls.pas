unit CompositionAwareControls;

{$mode objfpc}{$H+}
{$IFDEF LCLCOCOA}{$modeswitch objectivec2}{$ENDIF}

interface

uses Classes, StdCtrls, LCLType, LMessages;

type
  TCompositionChangedEvent = procedure(Sender: TObject;
    Composing: Boolean) of object;

  TCompositionAwareEdit = class(TEdit)
  private
    FCompositionPending: Boolean;
    FOnCompositionChanged: TCompositionChangedEvent;
    procedure SetCompositionPending(Value: Boolean);
  protected
    procedure WndProc(var Message: TLMessage); override;
  public
    function SampleCompositionState: Boolean;
    procedure RefreshCompositionState;
    property CompositionPending: Boolean read FCompositionPending;
    property OnCompositionChanged: TCompositionChangedEvent
      read FOnCompositionChanged write FOnCompositionChanged;
  end;

  TCompositionAwareMemo = class(TMemo)
  private
    FCompositionPending: Boolean;
    FOnCompositionChanged: TCompositionChangedEvent;
    procedure SetCompositionPending(Value: Boolean);
  protected
    procedure WndProc(var Message: TLMessage); override;
  public
    function SampleCompositionState: Boolean;
    procedure RefreshCompositionState;
    property CompositionPending: Boolean read FCompositionPending;
    property OnCompositionChanged: TCompositionChangedEvent
      read FOnCompositionChanged write FOnCompositionChanged;
  end;

implementation

{$IFDEF LCLCOCOA}
uses CocoaAll;

function NativeViewHasMarkedText(View: NSView): Boolean;
begin
  Result := Assigned(View) and
    View.conformsToProtocol(objcprotocol(NSTextInputClientProtocol)) and
    (NSTextInputClientProtocol(View).hasMarkedText <> False);
end;

function NativeEditHasMarkedText(Control: TCompositionAwareEdit): Boolean;
var
  NativeEdit: NSTextField;
  CurrentEditor: NSText;
  FirstResponder: id;
begin
  Result := False;
  if not Control.HandleAllocated or (Control.Handle = 0) then Exit;
  NativeEdit := NSTextField(Control.Handle);
  if not Assigned(NativeEdit.window) then Exit;
  CurrentEditor := NSText(NativeEdit.currentEditor);
  if not Assigned(CurrentEditor) then Exit;
  FirstResponder := NativeEdit.window.firstResponder;
  if FirstResponder <> CurrentEditor then Exit;
  Result := NativeViewHasMarkedText(NSView(CurrentEditor));
end;

function NativeMemoHasMarkedText(Control: TCompositionAwareMemo): Boolean;
var
  NativeMemo: NSScrollView;
begin
  Result := False;
  if not Control.HandleAllocated or (Control.Handle = 0) then Exit;
  NativeMemo := NSScrollView(Control.Handle);
  Result := NativeViewHasMarkedText(NativeMemo.documentView);
end;
{$ENDIF}

procedure TCompositionAwareEdit.SetCompositionPending(Value: Boolean);
begin
  if FCompositionPending = Value then Exit;
  FCompositionPending := Value;
  if Assigned(FOnCompositionChanged) then
    FOnCompositionChanged(Self, Value);
end;

procedure TCompositionAwareEdit.RefreshCompositionState;
begin
  {$IFDEF LCLCOCOA}
  SetCompositionPending(NativeEditHasMarkedText(Self));
  {$ENDIF}
end;

function TCompositionAwareEdit.SampleCompositionState: Boolean;
begin
  {$IFDEF LCLCOCOA}
  Result := NativeEditHasMarkedText(Self);
  {$ELSE}
  Result := FCompositionPending;
  {$ENDIF}
end;

procedure TCompositionAwareEdit.WndProc(var Message: TLMessage);
var
  CompositionEnded: Boolean;
begin
  CompositionEnded := False;
  {$IFNDEF LCLCOCOA}
  if Message.Msg = LM_IM_COMPOSITION then
  begin
    if (Message.WParam and GTK_IM_FLAG_START) <> 0 then
      SetCompositionPending(True);
    CompositionEnded := (Message.WParam and GTK_IM_FLAG_END) <> 0;
  end;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  if Message.Msg = $010D then SetCompositionPending(True);
  if Message.Msg = $010E then CompositionEnded := True;
  {$ENDIF}

  {$IFDEF LCLCOCOA}
  if (Message.Msg = CM_TEXTCHANGED) and
    NativeEditHasMarkedText(Self) then SetCompositionPending(True);
  {$ENDIF}
  inherited WndProc(Message);
  if Message.Msg = CM_TEXTCHANGED then RefreshCompositionState;
  if CompositionEnded then
  begin
    {$IFDEF LCLCOCOA}
    RefreshCompositionState;
    {$ELSE}
    SetCompositionPending(False);
    {$ENDIF}
  end;
end;

procedure TCompositionAwareMemo.SetCompositionPending(Value: Boolean);
begin
  if FCompositionPending = Value then Exit;
  FCompositionPending := Value;
  if Assigned(FOnCompositionChanged) then
    FOnCompositionChanged(Self, Value);
end;

procedure TCompositionAwareMemo.RefreshCompositionState;
begin
  {$IFDEF LCLCOCOA}
  SetCompositionPending(NativeMemoHasMarkedText(Self));
  {$ENDIF}
end;

function TCompositionAwareMemo.SampleCompositionState: Boolean;
begin
  {$IFDEF LCLCOCOA}
  Result := NativeMemoHasMarkedText(Self);
  {$ELSE}
  Result := FCompositionPending;
  {$ENDIF}
end;

procedure TCompositionAwareMemo.WndProc(var Message: TLMessage);
var
  CompositionEnded: Boolean;
begin
  CompositionEnded := False;
  {$IFNDEF LCLCOCOA}
  if Message.Msg = LM_IM_COMPOSITION then
  begin
    if (Message.WParam and GTK_IM_FLAG_START) <> 0 then
      SetCompositionPending(True);
    CompositionEnded := (Message.WParam and GTK_IM_FLAG_END) <> 0;
  end;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  if Message.Msg = $010D then SetCompositionPending(True);
  if Message.Msg = $010E then CompositionEnded := True;
  {$ENDIF}

  {$IFDEF LCLCOCOA}
  if (Message.Msg = CM_TEXTCHANGED) and
    NativeMemoHasMarkedText(Self) then SetCompositionPending(True);
  {$ENDIF}
  inherited WndProc(Message);
  if Message.Msg = CM_TEXTCHANGED then RefreshCompositionState;
  if CompositionEnded then
  begin
    {$IFDEF LCLCOCOA}
    RefreshCompositionState;
    {$ELSE}
    SetCompositionPending(False);
    {$ENDIF}
  end;
end;

end.

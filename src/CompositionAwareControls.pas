unit CompositionAwareControls;

{$mode objfpc}{$H+}
{$IFDEF LCLCOCOA}{$modeswitch objectivec2}{$ENDIF}

interface

uses Classes, StdCtrls, LCLType, LMessages
  {$IFDEF LCLCOCOA}, CocoaComposition{$ENDIF};

type
  TCompositionChangedEvent = procedure(Sender: TObject;
    Composing: Boolean) of object;

  TCompositionAwareEdit = class(TEdit)
  private
    FCompositionPending: Boolean;
    FCompositionCommitted: Boolean;
    FOnCompositionChanged: TCompositionChangedEvent;
    {$IFDEF LCLCOCOA}
    FNativeComposition: TCocoaComposition;
    procedure NativeCompositionChanged(Composing, Committed: Boolean);
    procedure AttachNativeComposition;
    {$ENDIF}
    procedure SetCompositionPending(Value: Boolean);
  protected
    {$IFDEF LCLCOCOA}
    procedure DestroyWnd; override;
    procedure DoEnter; override;
    {$ENDIF}
    procedure WndProc(var Message: TLMessage); override;
  public
    function SampleCompositionState: Boolean;
    procedure RefreshCompositionState;
    property CompositionPending: Boolean read FCompositionPending;
    property CompositionCommitted: Boolean read FCompositionCommitted;
    property OnCompositionChanged: TCompositionChangedEvent
      read FOnCompositionChanged write FOnCompositionChanged;
  end;

  TCompositionAwareMemo = class(TMemo)
  private
    FCompositionPending: Boolean;
    FCompositionCommitted: Boolean;
    FOnCompositionChanged: TCompositionChangedEvent;
    {$IFDEF LCLCOCOA}
    FNativeComposition: TCocoaComposition;
    procedure NativeCompositionChanged(Composing, Committed: Boolean);
    procedure AttachNativeComposition;
    {$ENDIF}
    procedure SetCompositionPending(Value: Boolean);
    function GetEditText: string;
  protected
    {$IFDEF LCLCOCOA}
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure DoEnter; override;
    {$ENDIF}
    procedure WndProc(var Message: TLMessage); override;
  public
    function SampleCompositionState: Boolean;
    procedure RefreshCompositionState;
    property CompositionPending: Boolean read FCompositionPending;
    property CompositionCommitted: Boolean read FCompositionCommitted;
    property EditText: string read GetEditText;
    property OnCompositionChanged: TCompositionChangedEvent
      read FOnCompositionChanged write FOnCompositionChanged;
  end;

implementation

{$IFDEF LCLCOCOA}
uses CocoaAll, SysUtils;

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
procedure TCompositionAwareEdit.NativeCompositionChanged(
  Composing, Committed: Boolean);
begin
  FCompositionCommitted := Committed;
  SetCompositionPending(Composing);
end;

procedure TCompositionAwareEdit.AttachNativeComposition;
var NativeEdit: NSTextField; Editor: NSTextView;
begin
  if not HandleAllocated then Exit;
  if FNativeComposition = nil then
    FNativeComposition := TCocoaComposition.Create(@NativeCompositionChanged);
  NativeEdit := NSTextField(Handle);
  if not Assigned(NativeEdit.window) then Exit;
  Editor := NSTextView(NativeEdit.window.fieldEditor_forObject(True, NativeEdit));
  FNativeComposition.Attach(Editor, NativeEdit);
end;

procedure TCompositionAwareEdit.DestroyWnd;
begin
  FreeAndNil(FNativeComposition);
  FCompositionPending := False;
  inherited DestroyWnd;
end;

procedure TCompositionAwareEdit.DoEnter;
begin
  AttachNativeComposition;
  inherited DoEnter;
end;

procedure TCompositionAwareMemo.NativeCompositionChanged(
  Composing, Committed: Boolean);
begin
  FCompositionCommitted := Committed;
  SetCompositionPending(Composing);
end;

procedure TCompositionAwareMemo.AttachNativeComposition;
begin
  if not HandleAllocated then Exit;
  if FNativeComposition = nil then
    FNativeComposition := TCocoaComposition.Create(@NativeCompositionChanged);
  FNativeComposition.Attach(
    NSTextView(NSScrollView(Handle).documentView), nil);
end;

procedure TCompositionAwareMemo.CreateWnd;
begin
  inherited CreateWnd;
  AttachNativeComposition;
end;

procedure TCompositionAwareMemo.DestroyWnd;
begin
  FreeAndNil(FNativeComposition);
  FCompositionPending := False;
  inherited DestroyWnd;
end;

procedure TCompositionAwareMemo.DoEnter;
begin
  inherited DoEnter;
  AttachNativeComposition;
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
  if NativeEditHasMarkedText(Self) then
  begin
    if not FCompositionPending then FCompositionCommitted := False;
    SetCompositionPending(True);
  end;
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
    begin
      FCompositionCommitted := False;
      SetCompositionPending(True);
    end;
    if (Message.WParam and GTK_IM_FLAG_COMMIT) <> 0 then
      FCompositionCommitted := True;
    CompositionEnded := (Message.WParam and GTK_IM_FLAG_END) <> 0;
  end;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  if Message.Msg = $010D then
  begin
    FCompositionCommitted := False;
    SetCompositionPending(True);
  end;
  if (Message.Msg = $010F) and ((Message.LParam and $0800) <> 0) then
    FCompositionCommitted := True;
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
    if not NativeEditHasMarkedText(Self) then SetCompositionPending(False);
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
  if NativeMemoHasMarkedText(Self) then
  begin
    if not FCompositionPending then FCompositionCommitted := False;
    SetCompositionPending(True);
  end;
  {$ENDIF}
end;

function TCompositionAwareMemo.GetEditText: string;
begin
  Result := Text;
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
    begin
      FCompositionCommitted := False;
      SetCompositionPending(True);
    end;
    if (Message.WParam and GTK_IM_FLAG_COMMIT) <> 0 then
      FCompositionCommitted := True;
    CompositionEnded := (Message.WParam and GTK_IM_FLAG_END) <> 0;
  end;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  if Message.Msg = $010D then
  begin
    FCompositionCommitted := False;
    SetCompositionPending(True);
  end;
  if (Message.Msg = $010F) and ((Message.LParam and $0800) <> 0) then
    FCompositionCommitted := True;
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
    if not NativeMemoHasMarkedText(Self) then SetCompositionPending(False);
    {$ELSE}
    SetCompositionPending(False);
    {$ENDIF}
  end;
end;

end.

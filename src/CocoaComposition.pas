unit CocoaComposition;

{$mode objfpc}{$H+}{$modeswitch objectivec2}

interface

uses Classes, Controls, CocoaAll;

type
  TCocoaCompositionEvent = procedure(Composing, Committed: Boolean) of object;

  TCocoaComposition = class
  private
    FView: NSTextView;
    FOriginalClass, FNativeClass: pobjc_class;
    FOwner: id;
    FChanged: TCocoaCompositionEvent;
    FActive, FCommitted: Boolean;
    FGeneration, FQueuedGeneration: QWord;
    FNativeDispatchDepth: Integer;
    procedure CancelEnd;
    procedure BeginMarkedText;
    procedure QueueEnd;
    procedure FinishEnd;
    function OwnsEditor: Boolean;
  public
    constructor Create(const Changed: TCocoaCompositionEvent);
    destructor Destroy; override;
    procedure Attach(View: NSTextView; Owner: id);
    procedure Detach;
  end;

implementation

uses SysUtils, CocoaTextEdits, CocoaUtils;

type
  // No additional native ivars: only the bound instance changes class. The
  // LCL implementation still owns its text, delegate, undo and input handling.
  TTpXCocoaTextView = objcclass(TCocoaTextView)
    procedure setMarkedText_selectedRange_replacementRange(Text: id;
      Selection, Replacement: NSRange); override;
    procedure insertText_replacementRange(Text: id; Replacement: NSRange); override;
    procedure unmarkText; override;
    procedure tpxFinishComposition; message 'tpxFinishComposition';
  end;

  TTpXCocoaFieldEditor = objcclass(TCocoaFieldEditor)
    procedure setMarkedText_selectedRange_replacementRange(Text: id;
      Selection, Replacement: NSRange); override;
    procedure insertText_replacementRange(Text: id; Replacement: NSRange); override;
    procedure unmarkText; override;
    procedure setDelegate(NewDelegate: NSTextDelegateProtocol); override;
    procedure tpxFinishComposition; message 'tpxFinishComposition';
  end;

var
  Bindings: TList;

function FindBinding(View: id): TCocoaComposition;
var I: Integer;
begin
  Result := nil;
  if Bindings = nil then Exit;
  for I := 0 to Bindings.Count - 1 do
    if TCocoaComposition(Bindings[I]).FView = View then
      Exit(TCocoaComposition(Bindings[I]));
end;

constructor TCocoaComposition.Create(const Changed: TCocoaCompositionEvent);
begin
  inherited Create;
  FChanged := Changed;
end;

destructor TCocoaComposition.Destroy;
begin
  Detach;
  inherited Destroy;
end;

procedure TCocoaComposition.Attach(View: NSTextView; Owner: id);
var
  Previous: TCocoaComposition;
  NativeClass, CurrentClass, Ancestor: pobjc_class;
begin
  if (FView = View) and (FOwner = Owner) then Exit;
  Detach;
  if View = nil then Exit;
  Previous := FindBinding(View);
  if Previous <> nil then
  begin
    Previous.Detach;
    Previous.FChanged(False, False);
  end;
  if Owner <> nil then NativeClass := pobjc_class(TTpXCocoaFieldEditor)
  else NativeClass := pobjc_class(TTpXCocoaTextView);
  CurrentClass := object_getClass(View);
  FOriginalClass := class_getSuperclass(NativeClass);
  Ancestor := CurrentClass;
  while (Ancestor <> nil) and (Ancestor <> NativeClass) do
    Ancestor := class_getSuperclass(Ancestor);
  if (Ancestor = nil) and
    ((CurrentClass <> FOriginalClass) or
     (class_getInstanceSize(FOriginalClass) <> class_getInstanceSize(NativeClass))) then
    raise EInvalidOperation.Create('Unsupported Cocoa text editor class');
  FView := View;
  FOwner := Owner;
  FNativeClass := NativeClass;
  FView.retain;
  if Bindings = nil then Bindings := TList.Create;
  Bindings.Add(Self);
  if Ancestor = nil then object_setClass(FView, NativeClass);
end;

procedure TCocoaComposition.CancelEnd;
begin
  if FView <> nil then
    NSObject.cancelPreviousPerformRequestsWithTarget_selector_object(FView,
      ObjCSelector('tpxFinishComposition'), nil);
end;

procedure TCocoaComposition.Detach;
var View: NSTextView;
begin
  if FView = nil then Exit;
  CancelEnd;
  Bindings.Remove(Self);
  View := FView;
  FView := nil;
  FOwner := nil;
  FActive := False;
  Inc(FGeneration);
  // AppKit may have installed a KVO subclass after attachment. Removing that
  // subclass would bypass its observer teardown. Our native methods are inert
  // without a binding, so leave such a subclass to AppKit's normal lifetime.
  if object_getClass(View) = FNativeClass then
    object_setClass(View, FOriginalClass);
  View.release;
end;

function TCocoaComposition.OwnsEditor: Boolean;
begin
  Result := (FView <> nil) and
    ((FOwner = nil) or (id(FView.delegate) = FOwner));
end;

procedure TCocoaComposition.BeginMarkedText;
var WasActive: Boolean;
begin
  if not OwnsEditor then Exit;
  CancelEnd;
  Inc(FGeneration);
  WasActive := FActive;
  FActive := True;
  FCommitted := False;
  if not WasActive then FChanged(True, False);
end;

procedure TCocoaComposition.QueueEnd;
begin
  if not FActive then Exit;
  CancelEnd;
  FQueuedGeneration := FGeneration;
  FView.performSelector_withObject_afterDelay(
    ObjCSelector('tpxFinishComposition'), nil, 0);
end;

procedure TCocoaComposition.FinishEnd;
var Committed: Boolean;
begin
  if not FActive or (FQueuedGeneration <> FGeneration) then Exit;
  if OwnsEditor and FView.hasMarkedText then Exit;
  Committed := FCommitted;
  FActive := False;
  // The callback may destroy this binding and its owning form.
  FChanged(False, Committed);
end;

procedure BeforeMarkedText(View: id; Text: id);
var Binding: TCocoaComposition;
begin
  Binding := FindBinding(View);
  if (Binding <> nil) and (Binding.FNativeDispatchDepth = 0) and
    (Text <> nil) and (NSString(Text).length > 0) then
    Binding.BeginMarkedText;
  Binding := FindBinding(View);
  if Binding <> nil then Inc(Binding.FNativeDispatchDepth);
end;

procedure BeforeInsertText(View: id);
var Binding: TCocoaComposition;
begin
  Binding := FindBinding(View);
  if Binding = nil then Exit;
  // AppKit also inserts text internally during marked-text replacement and
  // unmarking. Only an outer insertText call signifies an IME commit.
  if (Binding.FNativeDispatchDepth = 0) and Binding.OwnsEditor and
    Binding.FActive then Binding.FCommitted := True;
  Inc(Binding.FNativeDispatchDepth);
end;

procedure BeforeUnmarkText(View: id);
var Binding: TCocoaComposition;
begin
  Binding := FindBinding(View);
  if Binding <> nil then Inc(Binding.FNativeDispatchDepth);
end;

procedure AfterNativeInput(View: id);
var Binding: TCocoaComposition;
begin
  // Re-resolve after native dispatch: text-change callbacks may destroy the
  // control, or rebind the shared field editor to a different TpX control.
  Binding := FindBinding(View);
  if Binding = nil then Exit;
  if Binding.FNativeDispatchDepth > 0 then Dec(Binding.FNativeDispatchDepth);
  if (Binding.FNativeDispatchDepth = 0) and
    (not Binding.OwnsEditor or not NSTextView(View).hasMarkedText) then
    Binding.QueueEnd;
end;

procedure TTpXCocoaTextView.setMarkedText_selectedRange_replacementRange(
  Text: id; Selection, Replacement: NSRange);
begin
  retain;
  try
    BeforeMarkedText(Self, Text);
    inherited setMarkedText_selectedRange_replacementRange(Text, Selection, Replacement);
    AfterNativeInput(Self);
  finally
    release;
  end;
end;

procedure TTpXCocoaTextView.insertText_replacementRange(Text: id; Replacement: NSRange);
begin
  retain;
  try
    BeforeInsertText(Self);
    inherited insertText_replacementRange(Text, Replacement);
    AfterNativeInput(Self);
  finally
    release;
  end;
end;

procedure TTpXCocoaTextView.unmarkText;
begin
  retain;
  try
    BeforeUnmarkText(Self);
    inherited unmarkText;
    AfterNativeInput(Self);
  finally
    release;
  end;
end;

procedure TTpXCocoaTextView.tpxFinishComposition;
var Binding: TCocoaComposition;
begin
  Binding := FindBinding(Self);
  if Binding <> nil then Binding.FinishEnd;
end;

procedure TTpXCocoaFieldEditor.setMarkedText_selectedRange_replacementRange(
  Text: id; Selection, Replacement: NSRange);
begin
  retain;
  try
    BeforeMarkedText(Self, Text);
    inherited setMarkedText_selectedRange_replacementRange(Text, Selection, Replacement);
    AfterNativeInput(Self);
  finally
    release;
  end;
end;

procedure TTpXCocoaFieldEditor.insertText_replacementRange(Text: id; Replacement: NSRange);
begin
  retain;
  try
    BeforeInsertText(Self);
    inherited insertText_replacementRange(Text, Replacement);
    AfterNativeInput(Self);
  finally
    release;
  end;
end;

procedure TTpXCocoaFieldEditor.unmarkText;
begin
  retain;
  try
    BeforeUnmarkText(Self);
    inherited unmarkText;
    AfterNativeInput(Self);
  finally
    release;
  end;
end;

procedure TTpXCocoaFieldEditor.setDelegate(NewDelegate: NSTextDelegateProtocol);
var Binding: TCocoaComposition;
begin
  retain;
  try
    inherited setDelegate(NewDelegate);
    Binding := FindBinding(Self);
    if (Binding <> nil) and not Binding.OwnsEditor then Binding.QueueEnd;
  finally
    release;
  end;
end;

procedure TTpXCocoaFieldEditor.tpxFinishComposition;
var Binding: TCocoaComposition;
begin
  Binding := FindBinding(Self);
  if Binding <> nil then Binding.FinishEnd;
end;

finalization
  Bindings.Free;
end.

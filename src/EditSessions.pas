unit EditSessions;

{$mode delphi}{$H+}

interface

uses Classes, SysUtils, Variants, Drawings, GObjBase,
  PropertyEdits, PropertyEditCore;

type
  TEditSessionChange = (escDraftChanged, escCommitted, escCancelled,
    escInvalidated);
  TEditSessionChangedEvent = procedure(Sender: TObject;
    Change: TEditSessionChange) of object;

  TRawEditDraft = record
    Field: TObjectPropertyField;
    Text: string;
  end;
  TRawEditDraftArray = array of TRawEditDraft;

  TObjectEditSession = class(TObject)
  private type
    TPropertySnapshot = record
      TargetID: Integer;
      Field: TObjectPropertyField;
      Value: Variant;
    end;
  private
    FDrawing: TDrawing2D;
    FDocumentEpoch: QWord;
    FModelGeneration: QWord;
    FTargetIDs: TEditTargetIDs;
    FProvisionalIDs: array of Integer;
    FSnapshots: array of TPropertySnapshot;
    FPreviewPatch: TObjectPropertyPatch;
    FRawDrafts: TRawEditDraftArray;
    FRawDraftPendingFields: TObjectPropertyFields;
    FGlobalRawDraftPending: Boolean;
    FCompositionPending: Boolean;
    FActive: Boolean;
    FLastError: string;
    FOnChanged: TEditSessionChangedEvent;
    FOnInteractionChanged: TEditSessionChangedEvent;
    procedure Changed(Change: TEditSessionChange);
    function IsCurrentDocument(CurrentDocumentEpoch: QWord): Boolean;
    function FindSnapshot(TargetID: Integer;
      Field: TObjectPropertyField): Integer;
    function CaptureNewSnapshot(TargetID: Integer;
      Field: TObjectPropertyField; out ErrorText: string): Boolean;
    function HasEffectiveChanges: Boolean;
    function HasDeferredChanges: Boolean;
    function HasProvisionalObjects: Boolean;
    function GetHasUncommittedWork: Boolean;
    function GetHasRawDraftPending: Boolean;
    procedure MergePatch(var Destination: TObjectPropertyPatch;
      const Source: TObjectPropertyPatch);
    function SettleError(const Text: string): Boolean;
  public
    constructor Create(Drawing: TDrawing2D; DocumentEpoch: QWord;
      const TargetIDs: array of Integer);
    function CopyTargetIDs: TEditTargetIDs;
    function Preview(const Patch: TObjectPropertyPatch;
      CurrentDocumentEpoch: QWord; out ErrorText: string): Boolean;
    function Commit(CurrentDocumentEpoch: QWord;
      out ErrorText: string): Boolean;
    function Cancel(CurrentDocumentEpoch: QWord): Boolean;
    function AddProvisional(Obj: TObject2D): Integer;
    procedure SetRawDraft(Field: TObjectPropertyField; const Text: string);
    function TrySetRawDraft(Field: TObjectPropertyField;
      const Text: string; out ErrorText: string): Boolean;
    procedure SetRawDraftPending(Pending: Boolean);
    procedure SetRawDraftPendingForField(Field: TObjectPropertyField;
      Pending: Boolean);
    procedure SetCompositionPending(Pending: Boolean);
    function CopyRawDrafts: TRawEditDraftArray;
    function HasRawDraft(Field: TObjectPropertyField;
      out Text: string): Boolean;
    property DocumentEpoch: QWord read FDocumentEpoch;
    property IsActive: Boolean read FActive;
    property HasUncommittedWork: Boolean read GetHasUncommittedWork;
    property HasRawDraftPending: Boolean read GetHasRawDraftPending;
    property CompositionPending: Boolean read FCompositionPending;
    property LastError: string read FLastError;
    property OnChanged: TEditSessionChangedEvent read FOnChanged
      write FOnChanged;
    property OnInteractionChanged: TEditSessionChangedEvent
      read FOnInteractionChanged write FOnInteractionChanged;
  end;

implementation

const
  MaxRawDraftBytes = 65536;

constructor TObjectEditSession.Create(Drawing: TDrawing2D;
  DocumentEpoch: QWord; const TargetIDs: array of Integer);
var ErrorText: string; I: Integer;
begin
  inherited Create;
  if Drawing = nil then
    raise EArgumentNilException.Create('Drawing');
  FDrawing := Drawing;
  FDocumentEpoch := DocumentEpoch;
  FModelGeneration := Drawing.ModelGeneration;
  FActive := True;
  FLastError := '';
  SetLength(FTargetIDs, 0);
  SetLength(FProvisionalIDs, 0);
  SetLength(FSnapshots, 0);
  SetLength(FRawDrafts, 0);
  FPreviewPatch.Fields := [];
  FPreviewPatch.Text := '';
  FPreviewPatch.TeXText := '';
  FPreviewPatch.FontName := '';
  FPreviewPatch.BitmapLink := '';
  FRawDraftPendingFields := [];
  FGlobalRawDraftPending := False;
  FCompositionPending := False;
  if Length(TargetIDs) > 0 then
  begin
    FTargetIDs := ExpandObjectEditTargetIDs(Drawing, TargetIDs, ErrorText);
    if Length(FTargetIDs) = 0 then
      raise EArgumentException.Create(ErrorText);
    for I := 0 to High(FTargetIDs) do
      if FTargetIDs[I] < 0 then
        raise EArgumentException.Create('Invalid edit target ID');
  end;
end;

procedure TObjectEditSession.Changed(Change: TEditSessionChange);
begin
  if Assigned(FOnInteractionChanged) then
    FOnInteractionChanged(Self, Change);
  if Assigned(FOnChanged) then FOnChanged(Self, Change);
end;

function TObjectEditSession.CopyTargetIDs: TEditTargetIDs;
var I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(FTargetIDs));
  for I := 0 to High(FTargetIDs) do Result[I] := FTargetIDs[I];
end;

function TObjectEditSession.IsCurrentDocument(
  CurrentDocumentEpoch: QWord): Boolean;
begin
  Result := FActive and (FDrawing <> nil) and
    (CurrentDocumentEpoch = FDocumentEpoch) and
    (FDrawing.ModelGeneration = FModelGeneration);
end;

function TObjectEditSession.SettleError(const Text: string): Boolean;
begin
  FLastError := Text;
  Result := False;
end;

function TObjectEditSession.FindSnapshot(TargetID: Integer;
  Field: TObjectPropertyField): Integer;
var I: Integer;
begin
  for I := 0 to High(FSnapshots) do
    if (FSnapshots[I].TargetID = TargetID) and
      (FSnapshots[I].Field = Field) then Exit(I);
  Result := -1;
end;

function TObjectEditSession.CaptureNewSnapshot(TargetID: Integer;
  Field: TObjectPropertyField; out ErrorText: string): Boolean;
var IDs: array[0..0] of Integer; ReadValue: TPropertyValue; N: Integer;
begin
  Result := False;
  if FindSnapshot(TargetID, Field) >= 0 then Exit(True);
  IDs[0] := TargetID;
  ReadValue := ReadObjectProperty(FDrawing, IDs, Field);
  if ReadValue.State <> pvsUniform then
  begin
    ErrorText := 'The edit target no longer supports this property';
    Exit;
  end;
  N := Length(FSnapshots);
  SetLength(FSnapshots, N + 1);
  FSnapshots[N].TargetID := TargetID;
  FSnapshots[N].Field := Field;
  FSnapshots[N].Value := ReadValue.Value;
  Result := True;
end;

function TObjectEditSession.HasEffectiveChanges: Boolean;
var I: Integer; IDs: array[0..0] of Integer; Current: TPropertyValue;
begin
  for I := 0 to High(FSnapshots) do
  begin
    IDs[0] := FSnapshots[I].TargetID;
    Current := ReadObjectProperty(FDrawing, IDs, FSnapshots[I].Field);
    if (Current.State <> pvsUniform) or
      not VarSameValue(Current.Value, FSnapshots[I].Value) then Exit(True);
  end;
  Result := False;
end;

function TObjectEditSession.HasDeferredChanges: Boolean;
var Current: TPropertyValue;
begin
  if not (opfBitmapLink in FPreviewPatch.Fields) then Exit(False);
  Current := ReadObjectProperty(FDrawing, FTargetIDs, opfBitmapLink);
  Result := (Current.State <> pvsUniform) or
    not VarSameValue(Current.Value, FPreviewPatch.BitmapLink);
end;

function TObjectEditSession.HasProvisionalObjects: Boolean;
begin
  Result := Length(FProvisionalIDs) > 0;
end;

function TObjectEditSession.GetHasUncommittedWork: Boolean;
begin
  Result := FActive and (HasRawDraftPending or FCompositionPending or
    HasEffectiveChanges or HasDeferredChanges or HasProvisionalObjects);
end;

function TObjectEditSession.GetHasRawDraftPending: Boolean;
begin
  Result := FGlobalRawDraftPending or
    (FRawDraftPendingFields <> []);
end;

procedure TObjectEditSession.MergePatch(var Destination: TObjectPropertyPatch;
  const Source: TObjectPropertyPatch);
var Field: TObjectPropertyField;
begin
  for Field := Low(TObjectPropertyField) to High(TObjectPropertyField) do
    if Field in Source.Fields then
    begin
      case Field of
        opfLineStyle: Destination.LineStyle := Source.LineStyle;
        opfLineColor: Destination.LineColor := Source.LineColor;
        opfLineWidth: Destination.LineWidth := Source.LineWidth;
        opfHatching: Destination.Hatching := Source.Hatching;
        opfHatchColor: Destination.HatchColor := Source.HatchColor;
        opfFillColor: Destination.FillColor := Source.FillColor;
        opfBeginArrowKind:
          Destination.BeginArrowKind := Source.BeginArrowKind;
        opfEndArrowKind: Destination.EndArrowKind := Source.EndArrowKind;
        opfArrowSizeFactor:
          Destination.ArrowSizeFactor := Source.ArrowSizeFactor;
        opfText: Destination.Text := Source.Text;
        opfTeXText: Destination.TeXText := Source.TeXText;
        opfTextHeight: Destination.TextHeight := Source.TextHeight;
        opfFontName: Destination.FontName := Source.FontName;
        opfFontStyle: Destination.FontStyle := Source.FontStyle;
        opfHAlignment: Destination.HAlignment := Source.HAlignment;
        opfTextRotation: Destination.TextRotation := Source.TextRotation;
        opfStarKind: Destination.StarKind := Source.StarKind;
        opfStarSizeFactor:
          Destination.StarSizeFactor := Source.StarSizeFactor;
        opfSymbolKind: Destination.SymbolKind := Source.SymbolKind;
        opfSymbolDiameter:
          Destination.SymbolDiameter := Source.SymbolDiameter;
        opfSymbolRotation:
          Destination.SymbolRotation := Source.SymbolRotation;
        opfRectangleRX: Destination.RectangleRX := Source.RectangleRX;
        opfRectangleRY: Destination.RectangleRY := Source.RectangleRY;
        opfBitmapLink: Destination.BitmapLink := Source.BitmapLink;
        opfKeepAspectRatio:
          Destination.KeepAspectRatio := Source.KeepAspectRatio;
      end;
      Include(Destination.Fields, Field);
    end;
end;

function TObjectEditSession.Preview(const Patch: TObjectPropertyPatch;
  CurrentDocumentEpoch: QWord; out ErrorText: string): Boolean;
var I: Integer; Field: TObjectPropertyField;
  ReversiblePatch: TObjectPropertyPatch;
begin
  Result := False;
  ErrorText := '';
  if not IsCurrentDocument(CurrentDocumentEpoch) then
  begin
    FLastError := 'The document changed while this edit was open';
    FActive := False;
    Changed(escInvalidated);
    ErrorText := FLastError;
    Exit;
  end;
  if not ValidateObjectPropertyPatch(FDrawing, FTargetIDs, Patch,
    ErrorText) then
  begin
    FLastError := ErrorText;
    Exit;
  end;
  for I := 0 to High(FTargetIDs) do
    for Field := Low(TObjectPropertyField) to High(TObjectPropertyField) do
      if Field in Patch.Fields then
        if not CaptureNewSnapshot(FTargetIDs[I], Field, ErrorText) then
        begin
          FLastError := ErrorText;
          Exit;
        end;
  ReversiblePatch := Patch;
  Exclude(ReversiblePatch.Fields, opfBitmapLink);
  if (ReversiblePatch.Fields <> []) and
    not ApplyObjectPropertyPatch(FDrawing, FTargetIDs, ReversiblePatch,
      ErrorText) then
    begin
      FLastError := ErrorText;
      Exit;
    end;
  MergePatch(FPreviewPatch, Patch);
  FRawDraftPendingFields := FRawDraftPendingFields - Patch.Fields;
  FLastError := '';
  Changed(escDraftChanged);
  Result := True;
end;

function TObjectEditSession.Commit(CurrentDocumentEpoch: QWord;
  out ErrorText: string): Boolean;
var ChangedModel: Boolean; FinalPatch: TObjectPropertyPatch;
begin
  Result := False;
  ErrorText := '';
  if not IsCurrentDocument(CurrentDocumentEpoch) then
  begin
    FLastError := 'The document changed while this edit was open';
    FActive := False;
    Changed(escInvalidated);
    ErrorText := FLastError;
    Exit;
  end;
  if HasRawDraftPending then
  begin
    ErrorText := 'Finish or discard the incomplete property draft first';
    FLastError := ErrorText;
    Exit;
  end;
  if FCompositionPending then
  begin
    ErrorText := 'Finish text composition before committing the edit';
    FLastError := ErrorText;
    Exit;
  end;
  if opfBitmapLink in FPreviewPatch.Fields then
  begin
    FinalPatch := FPreviewPatch;
    FinalPatch.Fields := [opfBitmapLink];
    if not ApplyObjectPropertyPatch(FDrawing, FTargetIDs, FinalPatch,
      ErrorText) then
    begin
      FLastError := ErrorText;
      Exit;
    end;
  end;
  ChangedModel := HasEffectiveChanges or HasProvisionalObjects;
  FActive := False;
  FRawDraftPendingFields := [];
  FGlobalRawDraftPending := False;
  FCompositionPending := False;
  if ChangedModel then
  begin
    if Assigned(FDrawing.OnChangeDrawing) then
      FDrawing.NotifyChanged
    else
    begin
      if FDrawing.History <> nil then FDrawing.History.SetPropertiesChanged;
      FDrawing.RepaintViewports;
    end;
  end;
  FLastError := '';
  Changed(escCommitted);
  Result := True;
end;

function TObjectEditSession.Cancel(CurrentDocumentEpoch: QWord): Boolean;
var I: Integer; Patch: TObjectPropertyPatch; ErrorText: string;
  IDs: array[0..0] of Integer; OnChange0: TOnChangeDrawing;
  BeforeSelection0: TBeforeSelectionChangeEvent;
begin
  Result := False;
  if not IsCurrentDocument(CurrentDocumentEpoch) then
  begin
    FLastError := 'The document changed; the old edit cannot be restored';
    FActive := False;
    Changed(escInvalidated);
    Exit;
  end;
  OnChange0 := FDrawing.OnChangeDrawing;
  BeforeSelection0 := FDrawing.OnBeforeSelectionChange;
  FDrawing.OnChangeDrawing := nil;
  FDrawing.OnBeforeSelectionChange := nil;
  FDrawing.BeginModelGenerationBatch;
  try
    for I := High(FSnapshots) downto 0 do
    begin
      Patch.Fields := [];
      Patch.Text := '';
      Patch.TeXText := '';
      Patch.FontName := '';
      Patch.BitmapLink := '';
      if not AssignObjectPropertyPatchValue(Patch, FSnapshots[I].Field,
        FSnapshots[I].Value, ErrorText) then
      begin
        FLastError := ErrorText;
        Exit;
      end;
      IDs[0] := FSnapshots[I].TargetID;
      if not ApplyObjectPropertyPatch(FDrawing, IDs, Patch, ErrorText) then
      begin
        FLastError := ErrorText;
        Exit;
      end;
    end;
    for I := High(FProvisionalIDs) downto 0 do
      if FDrawing.GetObject(FProvisionalIDs[I]) <> nil then
        FDrawing.DeleteObject(FProvisionalIDs[I]);
  finally
    FDrawing.EndModelGenerationBatch;
    FDrawing.OnChangeDrawing := OnChange0;
    FDrawing.OnBeforeSelectionChange := BeforeSelection0;
  end;
  FActive := False;
  FRawDraftPendingFields := [];
  FGlobalRawDraftPending := False;
  FCompositionPending := False;
  FLastError := '';
  Changed(escCancelled);
  Result := True;
end;

function TObjectEditSession.AddProvisional(Obj: TObject2D): Integer;
var N, I: Integer; OnChange0: TOnChangeDrawing;
begin
  if not FActive or (FDrawing.ModelGeneration <> FModelGeneration) then
    raise EInvalidOperation.Create('The edit session is no longer current');
  if Obj = nil then raise EArgumentNilException.Create('Obj');
  OnChange0 := FDrawing.OnChangeDrawing;
  FDrawing.OnChangeDrawing := nil;
  FDrawing.BeginModelGenerationBatch;
  try
    FDrawing.AddObject(-1, Obj);
    Result := Obj.ID;
  finally
    FDrawing.EndModelGenerationBatch;
    FDrawing.OnChangeDrawing := OnChange0;
  end;
  N := Length(FProvisionalIDs);
  SetLength(FProvisionalIDs, N + 1);
  FProvisionalIDs[N] := Result;
  N := Length(FTargetIDs);
  for I := 0 to N - 1 do
    if FTargetIDs[I] = Result then Exit;
  SetLength(FTargetIDs, N + 1);
  FTargetIDs[N] := Result;
  Changed(escDraftChanged);
end;

procedure TObjectEditSession.SetRawDraft(Field: TObjectPropertyField;
  const Text: string);
var ErrorText: string;
begin
  if not TrySetRawDraft(Field, Text, ErrorText) then
    raise EArgumentException.Create(ErrorText);
end;

function TObjectEditSession.TrySetRawDraft(Field: TObjectPropertyField;
  const Text: string; out ErrorText: string): Boolean;
var I, ExistingIndex: Integer; ByteCount: Int64;
begin
  Result := False;
  ErrorText := '';
  ExistingIndex := -1;
  ByteCount := Length(UTF8Encode(Text));
  for I := 0 to High(FRawDrafts) do
  begin
    if FRawDrafts[I].Field = Field then ExistingIndex := I
    else Inc(ByteCount, Length(UTF8Encode(FRawDrafts[I].Text)));
  end;
  Include(FRawDraftPendingFields, Field);
  if ByteCount > MaxRawDraftBytes then
  begin
    ErrorText := 'Property drafts are limited to 64 KiB in one edit session';
    FLastError := ErrorText;
    Changed(escDraftChanged);
    Exit;
  end;
  if ExistingIndex >= 0 then
    FRawDrafts[ExistingIndex].Text := Text
  else
  begin
    SetLength(FRawDrafts, Length(FRawDrafts) + 1);
    FRawDrafts[High(FRawDrafts)].Field := Field;
    FRawDrafts[High(FRawDrafts)].Text := Text;
  end;
  FLastError := '';
  Changed(escDraftChanged);
  Result := True;
end;

procedure TObjectEditSession.SetRawDraftPending(Pending: Boolean);
begin
  if FGlobalRawDraftPending = Pending then Exit;
  FGlobalRawDraftPending := Pending;
  Changed(escDraftChanged);
end;

procedure TObjectEditSession.SetRawDraftPendingForField(
  Field: TObjectPropertyField; Pending: Boolean);
begin
  if (Field in FRawDraftPendingFields) = Pending then Exit;
  if Pending then Include(FRawDraftPendingFields, Field)
  else Exclude(FRawDraftPendingFields, Field);
  Changed(escDraftChanged);
end;

procedure TObjectEditSession.SetCompositionPending(Pending: Boolean);
begin
  if FCompositionPending = Pending then Exit;
  FCompositionPending := Pending;
  Changed(escDraftChanged);
end;

function TObjectEditSession.CopyRawDrafts: TRawEditDraftArray;
var I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(FRawDrafts));
  for I := 0 to High(FRawDrafts) do Result[I] := FRawDrafts[I];
end;

function TObjectEditSession.HasRawDraft(Field: TObjectPropertyField;
  out Text: string): Boolean;
var I: Integer;
begin
  for I := 0 to High(FRawDrafts) do
    if FRawDrafts[I].Field = Field then
    begin
      Text := FRawDrafts[I].Text;
      Exit(True);
    end;
  Text := '';
  Result := False;
end;

end.

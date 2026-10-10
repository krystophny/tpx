unit EditSessions;

{$mode delphi}{$H+}

interface

uses Classes, SysUtils, Variants, Drawings, GObjBase,
  PropertyEdits, PropertyEditCore, DocumentFormats;

type
  TEditSessionPresentation = (espInspector, espInlineText);

  TEditSessionChange = (escDraftChanged, escCommitted, escCancelled,
    escInvalidated);
  TEditSessionChangedEvent = procedure(Sender: TObject;
    Change: TEditSessionChange) of object;

  TRawEditDraft = record
    Field: TObjectPropertyField;
    Text: TEditUtf8Text;
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
    FPresentation: TEditSessionPresentation;
    FTargetIDs: TEditTargetIDs;
    FProvisionalIDs: array of Integer;
    FSnapshots: array of TPropertySnapshot;
    FPreviewPatch: TObjectPropertyPatch;
    FRawDrafts: TRawEditDraftArray;
    FRawDraftPendingFields: TObjectPropertyFields;
    FGlobalRawDraftPending: Boolean;
    FCompositionPending: Boolean;
    FLastCommitChanged: Boolean;
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
    function IsProvisionalTarget(TargetID: Integer): Boolean;
    function Preview(const Patch: TObjectPropertyPatch;
      CurrentDocumentEpoch: QWord; out ErrorText: string): Boolean;
    function Commit(CurrentDocumentEpoch: QWord;
      out ErrorText: string): Boolean;
    function Cancel(CurrentDocumentEpoch: QWord): Boolean;
    function AddProvisional(Obj: TObject2D): Integer;
    procedure SetRawDraft(Field: TObjectPropertyField;
      const Text: TEditUtf8Text);
    function TrySetRawDraft(Field: TObjectPropertyField;
      const Text: TEditUtf8Text; out ErrorText: string): Boolean;
    procedure SetRawDraftPending(Pending: Boolean);
    procedure SetRawDraftPendingForField(Field: TObjectPropertyField;
      Pending: Boolean);
    procedure SetCompositionPending(Pending: Boolean);
    function CopyRawDrafts: TRawEditDraftArray;
    function HasRawDraft(Field: TObjectPropertyField;
      out Text: TEditUtf8Text): Boolean;
    function BuildRecoveryData(const FileName: TDocumentPath;
      out CommittedModel, AuxiliaryState: RawByteString;
      out ErrorText: string): Boolean;
    class function RestoreRecoveryState(Drawing: TDrawing2D;
      DocumentEpoch: QWord; const AuxiliaryState: RawByteString;
      out Session: TObjectEditSession; out ErrorText: string): Boolean;
    property DocumentEpoch: QWord read FDocumentEpoch;
    property Presentation: TEditSessionPresentation read FPresentation
      write FPresentation;
    property IsActive: Boolean read FActive;
    property HasUncommittedWork: Boolean read GetHasUncommittedWork;
    property HasRawDraftPending: Boolean read GetHasRawDraftPending;
    property CompositionPending: Boolean read FCompositionPending;
    property LastCommitChanged: Boolean read FLastCommitChanged;
    property LastError: string read FLastError;
    property OnChanged: TEditSessionChangedEvent read FOnChanged
      write FOnChanged;
    property OnInteractionChanged: TEditSessionChangedEvent
      read FOnInteractionChanged write FOnInteractionChanged;
  end;

implementation

uses DocumentIO, GObjects, Graphics, Geometry, Devices;

const
  MaxRawDraftBytes = 65536;
  MaxEditRecoveryBytes = 262144;
  MaxRecoveryTargets = 4096;
  MaxRecoveryPathDepth = 128;
  MaxRecoveryProvisionalObjects = 64;
  EditRecoveryMagic = 'TPXES002';
  LegacyEditRecoveryMagic = 'TPXES001';

type
  TRecoveryPath = array of LongWord;
  TRecoveryTarget = record
    Path: TRecoveryPath;
    ClassName: string;
  end;
  TRecoveryTargets = array of TRecoveryTarget;
  TRecoveryProvisional = record
    ObjectBytes: RawByteString;
  end;
  TRecoveryProvisionals = array of TRecoveryProvisional;

procedure WriteUInt32(Stream: TStream; Value: LongWord);
begin
  Stream.WriteBuffer(Value, SizeOf(Value));
end;

function ReadUInt32(Stream: TStream): LongWord;
begin
  Stream.ReadBuffer(Result, SizeOf(Result));
end;

procedure WriteRaw(Stream: TStream; const Value: RawByteString);
var N: LongWord;
begin
  if Length(Value) > High(LongWord) then
    raise EWriteError.Create('Recovery field is too large');
  N := Length(Value);
  WriteUInt32(Stream, N);
  if N > 0 then Stream.WriteBuffer(Value[1], N);
end;

function ReadRaw(Stream: TStream; MaxBytes: LongWord): RawByteString;
var N: LongWord;
begin
  N := ReadUInt32(Stream);
  if (N > MaxBytes) or (Int64(N) > Stream.Size - Stream.Position) then
    raise EReadError.Create('Recovery edit state is too large or incomplete');
  SetLength(Result, N);
  if N > 0 then Stream.ReadBuffer(Result[1], N);
end;

procedure WriteUtf8(Stream: TStream; const Value: TEditUtf8Text);
begin
  WriteRaw(Stream, RawByteString(Value));
end;

function ReadUtf8(Stream: TStream; MaxBytes: LongWord): TEditUtf8Text;
var Bytes: RawByteString;
begin
  Bytes := ReadRaw(Stream, MaxBytes);
  SetCodePage(Bytes, 65001, False);
  Result := TEditUtf8Text(Bytes);
end;

function PropertyFieldsToBits(Fields: TObjectPropertyFields): LongWord;
var Field: TObjectPropertyField;
begin
  Result := 0;
  for Field := Low(TObjectPropertyField) to High(TObjectPropertyField) do
    if Field in Fields then Result := Result or (LongWord(1) shl Ord(Field));
end;

function BitsToPropertyFields(Bits: LongWord): TObjectPropertyFields;
var Field: TObjectPropertyField;
begin
  Result := [];
  for Field := Low(TObjectPropertyField) to High(TObjectPropertyField) do
    if (Bits and (LongWord(1) shl Ord(Field))) <> 0 then Include(Result, Field);
  if (Bits shr (Ord(High(TObjectPropertyField)) + 1)) <> 0 then
    raise EReadError.Create('Recovery edit state contains unknown fields');
end;

procedure WriteFieldValue(Stream: TStream; const Patch: TObjectPropertyPatch;
  Field: TObjectPropertyField);
var I: LongInt; B: Byte; R: TRealType;
begin
  case Field of
    opfLineStyle: I := Ord(Patch.LineStyle);
    opfLineColor: I := LongInt(Patch.LineColor);
    opfHatching: I := Ord(Patch.Hatching);
    opfHatchColor: I := LongInt(Patch.HatchColor);
    opfFillColor: I := LongInt(Patch.FillColor);
    opfBeginArrowKind: I := Ord(Patch.BeginArrowKind);
    opfEndArrowKind: I := Ord(Patch.EndArrowKind);
    opfFontStyle:
      begin
        B := 0;
        Move(Patch.FontStyle, B, SizeOf(B));
        Stream.WriteBuffer(B, SizeOf(B));
        Exit;
      end;
    opfHAlignment: I := Ord(Patch.HAlignment);
    opfStarKind: I := Ord(Patch.StarKind);
    opfSymbolKind: I := Ord(Patch.SymbolKind);
    opfText: begin WriteRaw(Stream, RawByteString(Patch.Text)); Exit end;
    opfTeXText: begin WriteRaw(Stream, RawByteString(Patch.TeXText)); Exit end;
    opfFontName: begin WriteUtf8(Stream, Patch.FontName); Exit end;
    opfBitmapLink: begin WriteUtf8(Stream, Patch.BitmapLink); Exit end;
    opfKeepAspectRatio:
      begin
        B := Ord(Patch.KeepAspectRatio);
        Stream.WriteBuffer(B, SizeOf(B));
        Exit;
      end;
    opfLineWidth: R := Patch.LineWidth;
    opfArrowSizeFactor: R := Patch.ArrowSizeFactor;
    opfTextHeight: R := Patch.TextHeight;
    opfTextRotation: R := Patch.TextRotation;
    opfStarSizeFactor: R := Patch.StarSizeFactor;
    opfSymbolDiameter: R := Patch.SymbolDiameter;
    opfSymbolRotation: R := Patch.SymbolRotation;
    opfRectangleRX: R := Patch.RectangleRX;
    opfRectangleRY: R := Patch.RectangleRY;
  end;
  if Field in [opfLineWidth, opfArrowSizeFactor, opfTextHeight,
    opfTextRotation, opfStarSizeFactor, opfSymbolDiameter,
    opfSymbolRotation, opfRectangleRX, opfRectangleRY] then
    Stream.WriteBuffer(R, SizeOf(R))
  else
    Stream.WriteBuffer(I, SizeOf(I));
end;

procedure ReadFieldValue(Stream: TStream; var Patch: TObjectPropertyPatch;
  Field: TObjectPropertyField);
var I: LongInt; B: Byte; R: TRealType;
begin
  case Field of
    opfText: begin Patch.Text := AnsiString(ReadRaw(Stream, MaxEditRecoveryBytes)); Exit end;
    opfTeXText: begin Patch.TeXText := ReadUtf8(Stream, MaxEditRecoveryBytes); Exit end;
    opfFontName: begin Patch.FontName := ReadUtf8(Stream, MaxEditRecoveryBytes); Exit end;
    opfBitmapLink: begin Patch.BitmapLink := ReadUtf8(Stream, MaxEditRecoveryBytes); Exit end;
    opfKeepAspectRatio:
      begin
        Stream.ReadBuffer(B, SizeOf(B));
        if B > 1 then raise EReadError.Create('Invalid recovery boolean');
        Patch.KeepAspectRatio := B <> 0;
        Exit;
      end;
    opfFontStyle:
      begin
        Stream.ReadBuffer(B, SizeOf(B));
        Move(B, Patch.FontStyle, SizeOf(B));
        Exit;
      end;
  end;
  if Field in [opfLineWidth, opfArrowSizeFactor, opfTextHeight,
    opfTextRotation, opfStarSizeFactor, opfSymbolDiameter,
    opfSymbolRotation, opfRectangleRX, opfRectangleRY] then
  begin
    Stream.ReadBuffer(R, SizeOf(R));
    case Field of
      opfLineWidth: Patch.LineWidth := R;
      opfArrowSizeFactor: Patch.ArrowSizeFactor := R;
      opfTextHeight: Patch.TextHeight := R;
      opfTextRotation: Patch.TextRotation := R;
      opfStarSizeFactor: Patch.StarSizeFactor := R;
      opfSymbolDiameter: Patch.SymbolDiameter := R;
      opfSymbolRotation: Patch.SymbolRotation := R;
      opfRectangleRX: Patch.RectangleRX := R;
      opfRectangleRY: Patch.RectangleRY := R;
    end;
    Exit;
  end;
  Stream.ReadBuffer(I, SizeOf(I));
  case Field of
    opfLineStyle: Patch.LineStyle := TLineStyle(I);
    opfLineColor: Patch.LineColor := TColor(I);
    opfHatching: Patch.Hatching := THatching(I);
    opfHatchColor: Patch.HatchColor := TColor(I);
    opfFillColor: Patch.FillColor := TColor(I);
    opfBeginArrowKind: Patch.BeginArrowKind := TArrowKind(I);
    opfEndArrowKind: Patch.EndArrowKind := TArrowKind(I);
    opfHAlignment: Patch.HAlignment := THAlignment(I);
    opfStarKind: Patch.StarKind := TStarKind(I);
    opfSymbolKind: Patch.SymbolKind := TSymbolKind(I);
  end;
end;

procedure InitializePatch(var Patch: TObjectPropertyPatch);
begin
  FillChar(Patch, SizeOf(Patch), 0);
  Patch.Fields := [];
end;

function ReadObjectAtPath(List: TGraphicObjList;
  const Path: TRecoveryPath): TObject2D;
var Index: LongWord; Obj: TGraphicObject;
begin
  Result := nil;
  if (List = nil) or (Length(Path) = 0) then Exit;
  Obj := List.FirstObj;
  Index := 0;
  while (Index < Path[0]) and (Obj <> nil) do
  begin
    Obj := List.NextObj;
    Inc(Index);
  end;
  if Obj = nil then Exit;
  if not (Obj is TObject2D) then Exit;
  if Length(Path) = 1 then Exit(Obj as TObject2D);
  if not (Obj is TGroup2D) then Exit;
  Result := ReadObjectAtPath((Obj as TGroup2D).Objects,
    Copy(Path, 1, Length(Path) - 1));
end;

function FindObjectPath(List: TGraphicObjList; TargetID: Integer;
  var Path: TRecoveryPath; Depth: Integer): Boolean;
var Obj: TGraphicObject; Index: LongWord;
begin
  Result := False;
  if (List = nil) or (Depth >= MaxRecoveryPathDepth) then Exit;
  Index := 0;
  Obj := List.FirstObj;
  while Obj <> nil do
  begin
    SetLength(Path, Depth + 1);
    Path[Depth] := Index;
    if Obj.ID = TargetID then Exit(True);
    if (Obj is TGroup2D) and FindObjectPath(
      (Obj as TGroup2D).Objects, TargetID, Path, Depth + 1) then Exit(True);
    Inc(Index);
    Obj := List.NextObj;
  end;
  SetLength(Path, Depth);
end;

function IsSessionProvisional(Session: TObjectEditSession;
  ID: Integer): Boolean;
var I: Integer;
begin
  for I := 0 to High(Session.FProvisionalIDs) do
    if Session.FProvisionalIDs[I] = ID then Exit(True);
  Result := False;
end;

function EncodeSessionRecoveryState(Session: TObjectEditSession;
  out State: RawByteString; out ErrorText: string): Boolean;
var Stream, ObjectStream: TMemoryStream; Targets: TRecoveryTargets;
  Field: TObjectPropertyField; I, Count: Integer; Path: TRecoveryPath;
  Obj: TObject2D; Graphic: TGraphicObject; ClassIndex: Word;
  PendingBits: LongWord; DraftBytes: Int64; Bytes: RawByteString;
  FieldByte, PendingByte, PresentationByte: Byte;
begin
  Result := False;
  State := '';
  ErrorText := '';
  if Session.FCompositionPending then
  begin
    ErrorText := 'Recovery waits for text composition to finish';
    Exit;
  end;
  Stream := TMemoryStream.Create;
  try
    try
      SetLength(Targets, 0);
      for I := 0 to High(Session.FTargetIDs) do
      begin
        if IsSessionProvisional(Session, Session.FTargetIDs[I]) then Continue;
        Path := nil;
        if not FindObjectPath(Session.FDrawing.ObjectList,
          Session.FTargetIDs[I], Path, 0) then
          raise EWriteError.Create('An edit target has no stable recovery path');
        Obj := ReadObjectAtPath(Session.FDrawing.ObjectList, Path);
        if (Obj = nil) or (Obj.ID <> Session.FTargetIDs[I]) then
          raise EWriteError.Create('An edit target changed during recovery');
        if Length(Targets) >= MaxRecoveryTargets then
          raise EWriteError.Create('Too many edit targets for recovery');
        SetLength(Targets, Length(Targets) + 1);
        Targets[High(Targets)].Path := Path;
        Targets[High(Targets)].ClassName := Obj.ClassName;
      end;
      Stream.WriteBuffer(EditRecoveryMagic[1], Length(EditRecoveryMagic));
      PresentationByte := Ord(Session.FPresentation);
      Stream.WriteBuffer(PresentationByte, SizeOf(PresentationByte));
      WriteUInt32(Stream, Length(Targets));
      for I := 0 to High(Targets) do
      begin
        WriteUInt32(Stream, Length(Targets[I].Path));
        for Count := 0 to High(Targets[I].Path) do
          WriteUInt32(Stream, Targets[I].Path[Count]);
        WriteUtf8(Stream, LCLTextToUTF8Bytes(Targets[I].ClassName));
      end;
      if Length(Session.FProvisionalIDs) > MaxRecoveryProvisionalObjects then
        raise EWriteError.Create('Too many provisional objects for recovery');
      WriteUInt32(Stream, Length(Session.FProvisionalIDs));
      for I := 0 to High(Session.FProvisionalIDs) do
      begin
        Graphic := Session.FDrawing.GetObject(Session.FProvisionalIDs[I]);
        if not (Graphic is TText2D) then
          raise EWriteError.Create(
            'Only provisional text objects can be retained for recovery');
        ClassIndex := TpXFindClassIndex(Graphic.ClassName);
        Stream.WriteBuffer(ClassIndex, SizeOf(ClassIndex));
        ObjectStream := TMemoryStream.Create;
        try
          Graphic.SaveToStream(ObjectStream);
          if ObjectStream.Size > MaxEditRecoveryBytes then
            raise EWriteError.Create('A provisional text object is too large');
          SetLength(Bytes, ObjectStream.Size);
          ObjectStream.Position := 0;
          if ObjectStream.Size > 0 then
            ObjectStream.ReadBuffer(Bytes[1], ObjectStream.Size);
          WriteRaw(Stream, Bytes);
        finally
          ObjectStream.Free;
        end;
      end;
      WriteUInt32(Stream, PropertyFieldsToBits(Session.FPreviewPatch.Fields));
      for Field := Low(TObjectPropertyField) to High(TObjectPropertyField) do
        if Field in Session.FPreviewPatch.Fields then
          WriteFieldValue(Stream, Session.FPreviewPatch, Field);
      WriteUInt32(Stream, Length(Session.FRawDrafts));
      PendingBits := PropertyFieldsToBits(Session.FRawDraftPendingFields);
      DraftBytes := 0;
      for I := 0 to High(Session.FRawDrafts) do
      begin
        Inc(DraftBytes, Length(Session.FRawDrafts[I].Text));
        if DraftBytes > MaxRawDraftBytes then
          raise EWriteError.Create('Raw property drafts exceed the recovery limit');
        Field := Session.FRawDrafts[I].Field;
        FieldByte := Ord(Field);
        Stream.WriteBuffer(FieldByte, SizeOf(FieldByte));
        WriteUtf8(Stream, Session.FRawDrafts[I].Text);
      end;
      WriteUInt32(Stream, PendingBits);
      if Session.FGlobalRawDraftPending then PendingByte := 1
      else PendingByte := 0;
      Stream.WriteBuffer(PendingByte, SizeOf(PendingByte));
      if Stream.Size > MaxEditRecoveryBytes then
        raise EWriteError.Create('Edit recovery state exceeds 256 KiB');
      SetLength(State, Stream.Size);
      Stream.Position := 0;
      if Stream.Size > 0 then Stream.ReadBuffer(State[1], Stream.Size);
      Result := True;
    except
      on E: Exception do ErrorText := E.Message;
    end;
  finally
    Stream.Free;
  end;
end;

constructor TObjectEditSession.Create(Drawing: TDrawing2D;
  DocumentEpoch: QWord; const TargetIDs: array of Integer);
var ErrorText: string; I: Integer;
begin
  inherited Create;
  if Drawing = nil then
    raise EArgumentNilException.Create('Drawing');
  FDrawing := Drawing;
  FDocumentEpoch := DocumentEpoch;
  FPresentation := espInspector;
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
  FLastCommitChanged := False;
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

function TObjectEditSession.IsProvisionalTarget(TargetID: Integer): Boolean;
begin
  Result := IsSessionProvisional(Self, TargetID);
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
  FLastCommitChanged := ChangedModel;
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
    FLastCommitChanged := False;
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
  FLastCommitChanged := False;
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
  const Text: TEditUtf8Text);
var ErrorText: string;
begin
  if not TrySetRawDraft(Field, Text, ErrorText) then
    raise EArgumentException.Create(ErrorText);
end;

function TObjectEditSession.TrySetRawDraft(Field: TObjectPropertyField;
  const Text: TEditUtf8Text; out ErrorText: string): Boolean;
var I, ExistingIndex: Integer; ByteCount: Int64;
begin
  Result := False;
  ErrorText := '';
  ExistingIndex := -1;
  ByteCount := Length(Text);
  for I := 0 to High(FRawDrafts) do
  begin
    if FRawDrafts[I].Field = Field then ExistingIndex := I
    else Inc(ByteCount, Length(FRawDrafts[I].Text));
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
  out Text: TEditUtf8Text): Boolean;
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

function ApplyRecoveryValue(Drawing: TDrawing2D; TargetID: Integer;
  Field: TObjectPropertyField; const Value: Variant;
  out ErrorText: string): Boolean;
var Patch: TObjectPropertyPatch; IDs: array[0..0] of Integer;
begin
  InitializePatch(Patch);
  Result := AssignObjectPropertyPatchValue(Patch, Field, Value, ErrorText);
  if not Result then Exit;
  IDs[0] := TargetID;
  Result := ApplyObjectPropertyPatch(Drawing, IDs, Patch, ErrorText, False);
end;

function TObjectEditSession.BuildRecoveryData(const FileName: TDocumentPath;
  out CommittedModel, AuxiliaryState: RawByteString;
  out ErrorText: string): Boolean;
type
  TDetachedObject = record
    Obj: TGraphicObject;
    ID: Integer;
    AnchorID: Integer;
    Removed: Boolean;
  end;
  TDetachedObjects = array of TDetachedObject;
var
  CurrentValues: array of Variant;
  Detached: TDetachedObjects;
  OldOnChange: TOnChangeDrawing;
  OldBeforeSelection: TBeforeSelectionChangeEvent;
  OldOnAdd: TAddObjectEvent;
  OldFreeOnDelete: Boolean;
  Obj, NextObj: TGraphicObject;
  I, DetachedCount: Integer;
  ErrorText0: string;
  IDs: array[0..0] of Integer;
  ReadValue: TPropertyValue;
begin
  Result := False;
  CommittedModel := '';
  AuxiliaryState := '';
  ErrorText := '';
  if not FActive or (FDrawing = nil) or
    (FDrawing.ModelGeneration <> FModelGeneration) then
  begin
    ErrorText := 'The edit session is no longer current';
    Exit;
  end;
  if not EncodeSessionRecoveryState(Self, AuxiliaryState, ErrorText) then
    Exit;
  SetLength(CurrentValues, Length(FSnapshots));
  for I := 0 to High(FSnapshots) do
  begin
    CurrentValues[I] := Unassigned;
    if IsSessionProvisional(Self, FSnapshots[I].TargetID) or
      (FSnapshots[I].Field = opfBitmapLink) then
      Continue;
    IDs[0] := FSnapshots[I].TargetID;
    ReadValue := ReadObjectProperty(FDrawing, IDs, FSnapshots[I].Field);
    if ReadValue.State <> pvsUniform then
    begin
      ErrorText := 'An edited property could not be captured for recovery';
      Exit;
    end;
    CurrentValues[I] := ReadValue.Value;
  end;
  OldOnChange := FDrawing.OnChangeDrawing;
  OldBeforeSelection := FDrawing.OnBeforeSelectionChange;
  OldOnAdd := FDrawing.OnAddObject;
  OldFreeOnDelete := FDrawing.ObjectList.FreeOnDelete;
  FDrawing.OnChangeDrawing := nil;
  FDrawing.OnBeforeSelectionChange := nil;
  FDrawing.OnAddObject := nil;
  FDrawing.BeginModelGenerationBatch;
  SetLength(Detached, Length(FProvisionalIDs));
  DetachedCount := 0;
  try
    try
      for I := 0 to High(FSnapshots) do
        if not IsSessionProvisional(Self, FSnapshots[I].TargetID) and
          (FSnapshots[I].Field <> opfBitmapLink) then
        begin
          if not ApplyRecoveryValue(FDrawing, FSnapshots[I].TargetID,
            FSnapshots[I].Field, FSnapshots[I].Value, ErrorText) then
            raise EWriteError.Create(ErrorText);
        end;
      Obj := FDrawing.ObjectList.FirstObj;
      while Obj <> nil do
      begin
        NextObj := FDrawing.ObjectList.NextObj;
        if IsSessionProvisional(Self, Obj.ID) then
        begin
          Detached[DetachedCount].Obj := Obj;
          Detached[DetachedCount].ID := Obj.ID;
          Detached[DetachedCount].Removed := False;
          if NextObj = nil then Detached[DetachedCount].AnchorID := -1
          else Detached[DetachedCount].AnchorID := NextObj.ID;
          Inc(DetachedCount);
        end;
        Obj := NextObj;
      end;
      if DetachedCount <> Length(FProvisionalIDs) then
        raise EWriteError.Create('A provisional object is not in the drawing order');
      FDrawing.ObjectList.FreeOnDelete := False;
      for I := 0 to DetachedCount - 1 do
      begin
        if not FDrawing.ObjectList.Delete(Detached[I].ID) then
          raise EWriteError.Create('A provisional object could not be detached');
        Detached[I].Removed := True;
      end;
      CommittedModel := SerializeTpXRecoveryBytes(FDrawing, FileName);
      Result := True;
    except
      on E: Exception do
      begin
        ErrorText := E.Message;
        Result := False;
      end;
    end;
  finally
    try
      FDrawing.ObjectList.FreeOnDelete := False;
      for I := DetachedCount - 1 downto 0 do
      begin
        if not Detached[I].Removed then Continue;
        FDrawing.ObjectList.Add(Detached[I].Obj);
        if (Detached[I].AnchorID >= 0) and
          (FDrawing.GetObject(Detached[I].AnchorID) <> nil) then
          FDrawing.ObjectList.Move(Detached[I].ID, Detached[I].AnchorID);
      end;
    finally
      FDrawing.ObjectList.FreeOnDelete := OldFreeOnDelete;
    end;
    for I := High(FSnapshots) downto 0 do
      if not IsSessionProvisional(Self, FSnapshots[I].TargetID) and
        (FSnapshots[I].Field <> opfBitmapLink) and
        not VarIsEmpty(CurrentValues[I]) then
      begin
        ErrorText0 := '';
        if not ApplyRecoveryValue(FDrawing, FSnapshots[I].TargetID,
          FSnapshots[I].Field, CurrentValues[I], ErrorText0) then
        begin
          Result := False;
          ErrorText := 'The preview could not be restored after recovery: ' +
            ErrorText0;
        end;
      end;
    FDrawing.EndModelGenerationBatch;
    FDrawing.OnChangeDrawing := OldOnChange;
    FDrawing.OnBeforeSelectionChange := OldBeforeSelection;
    FDrawing.OnAddObject := OldOnAdd;
  end;
  if not Result then
  begin
    CommittedModel := '';
    AuxiliaryState := '';
  end;
end;

class function TObjectEditSession.RestoreRecoveryState(Drawing: TDrawing2D;
  DocumentEpoch: QWord; const AuxiliaryState: RawByteString;
  out Session: TObjectEditSession; out ErrorText: string): Boolean;
var
  Stream, ObjectStream: TMemoryStream;
  Magic: array[0..7] of Char;
  Targets: TRecoveryTargets;
  Provisionals: TRecoveryProvisionals;
  Objects: array of TObject2D;
  TargetIDs: TEditTargetIDs;
  RawDrafts: TRawEditDraftArray;
  Patch: TObjectPropertyPatch;
  PendingFields: TObjectPropertyFields;
  Field: TObjectPropertyField;
  FieldByte, GlobalPending: Byte;
  PresentationByte: Byte;
  HasPresentation: Boolean;
  Presentation: TEditSessionPresentation;
  FieldBits, PendingBits, Count, PathLength: LongWord;
  I, J: Integer;
  RawBytes: Int64;
  ClassIndex: Word;
  Graphic: TGraphicObject;
  ObjectClass: TGraphicObjectClass;
  Obj: TObject2D;
  ErrorText0: string;
begin
  Result := False;
  Session := nil;
  ErrorText := '';
  if (Drawing = nil) or (Length(AuxiliaryState) = 0) then
  begin
    ErrorText := 'Recovery edit state is empty';
    Exit;
  end;
  Stream := TMemoryStream.Create;
  try
    try
      if Length(AuxiliaryState) > MaxEditRecoveryBytes then
        raise EReadError.Create('Edit recovery state exceeds 256 KiB');
      Stream.WriteBuffer(AuxiliaryState[1], Length(AuxiliaryState));
      Stream.Position := 0;
      if Stream.Size < Length(EditRecoveryMagic) then
        raise EReadError.Create('Edit recovery state is incomplete');
      Stream.ReadBuffer(Magic, SizeOf(Magic));
      HasPresentation := CompareMem(@Magic[0], @EditRecoveryMagic[1], SizeOf(Magic));
      if not HasPresentation and not CompareMem(@Magic[0],
        @LegacyEditRecoveryMagic[1], SizeOf(Magic)) then
        raise EReadError.Create('Edit recovery signature is invalid');
      Presentation := espInspector;
      if HasPresentation then
      begin
        Stream.ReadBuffer(PresentationByte, SizeOf(PresentationByte));
        if PresentationByte > Ord(High(TEditSessionPresentation)) then
          raise EReadError.Create('Edit recovery presentation is invalid');
        Presentation := TEditSessionPresentation(PresentationByte);
      end;
      Count := ReadUInt32(Stream);
      if Count > MaxRecoveryTargets then
        raise EReadError.Create('Edit recovery has too many targets');
      SetLength(Targets, Count);
      for I := 0 to Integer(Count) - 1 do
      begin
        PathLength := ReadUInt32(Stream);
        if (PathLength = 0) or (PathLength > MaxRecoveryPathDepth) then
          raise EReadError.Create('Edit recovery target path is invalid');
        SetLength(Targets[I].Path, PathLength);
        for J := 0 to Integer(PathLength) - 1 do
          Targets[I].Path[J] := ReadUInt32(Stream);
        Targets[I].ClassName := ReadUtf8(Stream, 256);
        if Targets[I].ClassName = '' then
          raise EReadError.Create('Edit recovery target class is missing');
      end;
      Count := ReadUInt32(Stream);
      if Count > MaxRecoveryProvisionalObjects then
        raise EReadError.Create('Edit recovery has too many provisional objects');
      SetLength(Provisionals, Count);
      SetLength(Objects, Count);
      for I := 0 to Integer(Count) - 1 do
      begin
        Stream.ReadBuffer(ClassIndex, SizeOf(ClassIndex));
        ObjectClass := TpXFindClassByIndex(ClassIndex);
        if ObjectClass <> TText2D then
          raise EReadError.Create('Unsupported provisional recovery object');
        Provisionals[I].ObjectBytes := ReadRaw(Stream,
          MaxEditRecoveryBytes);
        ObjectStream := TMemoryStream.Create;
        try
          if Length(Provisionals[I].ObjectBytes) = 0 then
            raise EReadError.Create('Provisional text data is empty');
          ObjectStream.WriteBuffer(Provisionals[I].ObjectBytes[1],
            Length(Provisionals[I].ObjectBytes));
          ObjectStream.Position := 0;
          Graphic := ObjectClass.CreateFromStream(ObjectStream);
          if ObjectStream.Position <> ObjectStream.Size then
          begin
            Graphic.Free;
            raise EReadError.Create('Provisional text data has trailing bytes');
          end;
          if not (Graphic is TText2D) then
          begin
            Graphic.Free;
            raise EReadError.Create('Provisional recovery class mismatch');
          end;
          if (Graphic as TText2D).Points.Count <> 1 then
          begin
            Graphic.Free;
            raise EReadError.Create(
              'Recovered provisional text must have one anchor point');
          end;
          Objects[I] := Graphic as TObject2D;
        finally
          ObjectStream.Free;
        end;
      end;
      InitializePatch(Patch);
      FieldBits := ReadUInt32(Stream);
      Patch.Fields := BitsToPropertyFields(FieldBits);
      for Field := Low(TObjectPropertyField) to High(TObjectPropertyField) do
        if Field in Patch.Fields then ReadFieldValue(Stream, Patch, Field);
      Count := ReadUInt32(Stream);
      if Count > Ord(High(TObjectPropertyField)) + 1 then
        raise EReadError.Create('Edit recovery has too many raw drafts');
      SetLength(RawDrafts, Count);
      RawBytes := 0;
      for I := 0 to Integer(Count) - 1 do
      begin
        Stream.ReadBuffer(FieldByte, SizeOf(FieldByte));
        if FieldByte > Ord(High(TObjectPropertyField)) then
          raise EReadError.Create('Edit recovery raw draft field is invalid');
        RawDrafts[I].Field := TObjectPropertyField(FieldByte);
        for J := 0 to I - 1 do
          if RawDrafts[J].Field = RawDrafts[I].Field then
            raise EReadError.Create('Edit recovery repeats a raw draft field');
        RawDrafts[I].Text := ReadUtf8(Stream, MaxRawDraftBytes);
        Inc(RawBytes, Length(RawDrafts[I].Text));
        if RawBytes > MaxRawDraftBytes then
          raise EReadError.Create('Edit recovery raw drafts exceed 64 KiB');
      end;
      PendingBits := ReadUInt32(Stream);
      PendingFields := BitsToPropertyFields(PendingBits);
      Stream.ReadBuffer(GlobalPending, SizeOf(GlobalPending));
      if GlobalPending > 1 then
        raise EReadError.Create('Edit recovery pending state is invalid');
      if Stream.Position <> Stream.Size then
        raise EReadError.Create('Edit recovery state has trailing data');

      SetLength(TargetIDs, Length(Targets));
      for I := 0 to High(Targets) do
      begin
        Obj := ReadObjectAtPath(Drawing.ObjectList, Targets[I].Path);
        if (Obj = nil) or (Obj.ClassName <> Targets[I].ClassName) then
          raise EReadError.Create(
            'The recovered edit target no longer matches its saved location');
        for J := 0 to I - 1 do
          if TargetIDs[J] = Obj.ID then
            raise EReadError.Create('Edit recovery repeats a target location');
        TargetIDs[I] := Obj.ID;
      end;
      if (Length(TargetIDs) = 0) and (Length(Objects) = 0) and
        ((Length(RawDrafts) > 0) or (Patch.Fields <> [])) then
        raise EReadError.Create('Edit recovery has no target objects');

      Session := TObjectEditSession.Create(Drawing, DocumentEpoch, TargetIDs);
      Session.Presentation := Presentation;
      try
        for I := 0 to High(Objects) do
        begin
          Session.AddProvisional(Objects[I]);
          Objects[I] := nil;
        end;
        if Patch.Fields <> [] then
        begin
          if not Session.Preview(Patch, DocumentEpoch, ErrorText0) then
            raise EReadError.Create(ErrorText0);
        end;
        for I := 0 to High(RawDrafts) do
        begin
          if not Session.TrySetRawDraft(RawDrafts[I].Field,
            RawDrafts[I].Text, ErrorText0) then
            raise EReadError.Create(ErrorText0);
          if not (RawDrafts[I].Field in PendingFields) then
            Session.SetRawDraftPendingForField(RawDrafts[I].Field, False);
        end;
        Session.SetRawDraftPending(GlobalPending <> 0);
      except
        on E: Exception do
        begin
          if Session.IsActive then Session.Cancel(DocumentEpoch);
          FreeAndNil(Session);
          raise EReadError.Create(E.Message);
        end;
      end;
      Result := True;
    except
      on E: Exception do ErrorText := E.Message;
    end;
  finally
    for I := 0 to High(Objects) do Objects[I].Free;
    Stream.Free;
  end;
  if not Result then FreeAndNil(Session);
end;

end.

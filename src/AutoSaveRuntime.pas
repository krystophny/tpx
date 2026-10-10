unit AutoSaveRuntime;

{$mode delphi}{$H+}

interface

uses Classes, SysUtils, DocumentFormats, Drawings, AutoSaveCore,
  AutoSaveStore;

type
  TRecoveryAsset = record
    RelativePath: string;
    Bytes: RawByteString;
  end;
  TRecoveryAssetArray = array of TRecoveryAsset;
  TRecoveryPathArray = array of TDocumentPath;

  { Owns the private bitmap directory used by a restored native draft. }
  TRecoveryAssetContext = class(TObject)
  private
    FRootPath: TDocumentPath;
    FOwnedFiles: TRecoveryPathArray;
    procedure OwnFile(const FileName: TDocumentPath);
  public
    constructor Create(const TempRoot: TDocumentPath;
      const Assets: TRecoveryAssetArray);
    destructor Destroy; override;
    procedure AdoptDrawingFiles(Drawing: TDrawing2D);
    function CandidatePath: TDocumentPath;
    function HasOwnedAssets: Boolean;
  end;

  TAutoSaveRuntime = class
  private
    FCoordinator: TAutoSaveCoordinator;
    FStore: TAutoSaveStore;
    FDocumentKey: string;
    FLastStatus: string;
    FRecoveryWarning: string;
    FAvailabilityReason: string;
    FRequestedAutoSaveEnabled: Boolean;
    function CanPublishSource(Session: TDocumentSession;
      Drawing: TDrawing2D; out Reason: string): Boolean;
    function NewUntitledKey: string;
    procedure SaveRecoveryDraft(Session: TDocumentSession;
      Drawing: TDrawing2D; const Ticket: TAutoSaveTicket);
    function SaveSource(Session: TDocumentSession; Drawing: TDrawing2D;
      const Ticket: TAutoSaveTicket): Boolean;
  public
    constructor Create(const RecoveryRoot: string);
    destructor Destroy; override;
    function BuildTpXRecoveryPayload(Drawing: TDrawing2D;
      const SourcePath: TDocumentPath; out UnresolvedLinks: LongWord):
      RawByteString;
    procedure BindDocument(Session: TDocumentSession;
      Drawing: TDrawing2D; AutoSaveEnabled, RecoveryEnabled,
      Dirty: Boolean; NowMS: QWord; const RecoveryKey: string = '');
    procedure CloseDocument;
    procedure NotifyLocalEdit(LocalRevision, NowMS: QWord; Dirty: Boolean);
    procedure NotifyReloadCommitted(LocalRevision, WatchGeneration: QWord);
    procedure RefreshCapability(Session: TDocumentSession;
      Drawing: TDrawing2D; NowMS: QWord);
    procedure AcceptSavedRevision(WatchGeneration: QWord);
    procedure SetConflict;
    procedure SetStatus(const Value: string);
    procedure SetRecoveryWarning(const Value: string);
    function SetAutoSaveEnabled(Value: Boolean; NowMS: QWord): Boolean;
    procedure SetRecoveryEnabled(Value: Boolean; NowMS: QWord);
    function NextDelayMS(NowMS: QWord): Integer;
    function ProcessDue(Session: TDocumentSession; Drawing: TDrawing2D;
      NowMS: QWord; out SourceSaved: Boolean): Boolean;
    procedure UnpackTpXRecoveryPayload(const Payload: RawByteString;
      const TempRoot: TDocumentPath; out ModelBytes: RawByteString;
      out AssetContext: TRecoveryAssetContext;
      out UnresolvedLinks: LongWord);
    property Coordinator: TAutoSaveCoordinator read FCoordinator;
    property Store: TAutoSaveStore read FStore;
    property DocumentKey: string read FDocumentKey;
    property LastStatus: string read FLastStatus;
    property RecoveryWarning: string read FRecoveryWarning;
    property AvailabilityReason: string read FAvailabilityReason;
  end;

implementation

uses DocumentIO, Output, AutoSavePreferences, Bitmaps, MiscUtils, StrUtils;

const
  RecoveryPayloadMagic = 'TPXRA001';
  RecoveryPayloadHeaderBytes = 8 + 4 + 4 + 4;
  RecoveryAssetLimit = 8192;

procedure WriteUInt32(Stream: TStream; Value: LongWord);
var B: array[0..3] of Byte; I: Integer;
begin
  for I := 0 to 3 do B[I] := Byte((Value shr (I * 8)) and $FF);
  Stream.WriteBuffer(B, SizeOf(B));
end;

function ReadUInt32(Stream: TStream): LongWord;
var B: array[0..3] of Byte; I: Integer;
begin
  Stream.ReadBuffer(B, SizeOf(B));
  Result := 0;
  for I := 0 to 3 do Result := Result or (LongWord(B[I]) shl (I * 8));
end;

procedure WriteRaw(Stream: TStream; const Bytes: RawByteString);
begin
  WriteUInt32(Stream, Length(Bytes));
  if Length(Bytes) > 0 then Stream.WriteBuffer(Bytes[1], Length(Bytes));
end;

function ReadRaw(Stream: TStream; MaxLength: LongWord): RawByteString;
var N: LongWord;
begin
  N := ReadUInt32(Stream);
  if N > MaxLength then raise EReadError.Create('Recovery asset is too large');
  SetLength(Result, N);
  if N > 0 then Stream.ReadBuffer(Result[1], N);
end;

function RecoveryAssetPathIsSafe(const RelativePath: string): Boolean;
var Leaf: string;
begin
  Result := False;
  if Copy(RelativePath, 1, 8) <> 'bitmaps/' then Exit;
  Leaf := Copy(RelativePath, 9, MaxInt);
  if (Leaf = '') or (Leaf = '.') or (Leaf = '..') then Exit;
  if (Pos('/', Leaf) > 0) or (Pos('\', Leaf) > 0) or
    (Pos(':', Leaf) > 0) then Exit;
  Result := True;
end;

procedure WriteModelSlice(Stream: TStream; const Model: string;
  StartAt, Count: Integer);
begin
  if Count > 0 then Stream.WriteBuffer(Model[StartAt], Count);
end;

function RewriteSerializedBitmapLinks(const ModelBytes: RawByteString;
  const OldLinks, NewLinks: TStrings; out RewrittenCount: Integer):
  RawByteString;
var Model, NewValue: string; Stream: TMemoryStream;
  SearchAt, Cursor, TagStart, TagEnd, AttrStart, ValueStart, ValueEnd: Integer;
  LinkIndex: Integer;
begin
  Model := string(ModelBytes);
  RewrittenCount := 0;
  Stream := TMemoryStream.Create;
  try
    SearchAt := 1;
    Cursor := 1;
    repeat
      TagStart := PosEx('<bitmap', Model, SearchAt);
      if TagStart = 0 then Break;
      TagEnd := PosEx('>', Model, TagStart);
      if TagEnd = 0 then Break;
      AttrStart := PosEx('link="', Model, TagStart);
      LinkIndex := -1;
      if (AttrStart > 0) and (AttrStart < TagEnd) then
      begin
        ValueStart := AttrStart + Length('link="');
        ValueEnd := PosEx('"', Model, ValueStart);
        if (ValueEnd > ValueStart) and (ValueEnd <= TagEnd) then
          LinkIndex := OldLinks.IndexOf(
            Copy(Model, ValueStart, ValueEnd - ValueStart));
      end;
      if LinkIndex >= 0 then
      begin
        WriteModelSlice(Stream, Model, Cursor, ValueStart - Cursor);
        NewValue := NewLinks[LinkIndex];
        if NewValue <> '' then
          Stream.WriteBuffer(NewValue[1], Length(NewValue));
        WriteModelSlice(Stream, Model, ValueEnd,
          TagEnd - ValueEnd + 1);
        Inc(RewrittenCount);
      end
      else
        WriteModelSlice(Stream, Model, Cursor, TagEnd - Cursor + 1);
      Cursor := TagEnd + 1;
      SearchAt := Cursor;
    until False;
    WriteModelSlice(Stream, Model, Cursor, Length(Model) - Cursor + 1);
    SetLength(Result, Stream.Size);
    if Stream.Size > 0 then
    begin
      Stream.Position := 0;
      Stream.ReadBuffer(Result[1], Stream.Size);
    end;
  finally
    Stream.Free;
  end;
end;

constructor TRecoveryAssetContext.Create(const TempRoot: TDocumentPath;
  const Assets: TRecoveryAssetArray);
var I: Integer; AssetDirectory: TDocumentPath; RelativePath: string;
  FileName: TDocumentPath;
begin
  inherited Create;
  SetLength(FOwnedFiles, 0);
  FRootPath := CreateDocumentStagingDirectory(TempRoot);
  if FRootPath = '' then
    raise EWriteError.Create('Could not create a private recovery asset directory');
  AssetDirectory := IncludeTrailingPathDelimiter(FRootPath) + 'bitmaps';
  if (Length(Assets) > 0) and not EnsureDocumentDirectoryExists(AssetDirectory) then
    raise EWriteError.Create('Could not create the recovery bitmap directory');
  for I := 0 to High(Assets) do
  begin
    RelativePath := Assets[I].RelativePath;
    if not RecoveryAssetPathIsSafe(RelativePath) then
      raise EReadError.Create('Recovery payload contains an unsafe asset path');
    FileName := IncludeTrailingPathDelimiter(FRootPath) +
      StringReplace(RelativePath, '/', PathDelim, [rfReplaceAll]);
    OwnFile(FileName);
    WriteDocumentStagingFile(FileName, Assets[I].Bytes);
  end;
end;

destructor TRecoveryAssetContext.Destroy;
var I: Integer; AssetDirectory: TDocumentPath;
begin
  for I := 0 to High(FOwnedFiles) do DeleteDocumentFile(FOwnedFiles[I]);
  AssetDirectory := IncludeTrailingPathDelimiter(FRootPath) + 'bitmaps';
  if AssetDirectory <> '' then RemoveDocumentStagingDirectory(AssetDirectory);
  if FRootPath <> '' then RemoveDocumentStagingDirectory(FRootPath);
  FOwnedFiles := nil;
  inherited Destroy;
end;

procedure TRecoveryAssetContext.OwnFile(const FileName: TDocumentPath);
var I: Integer;
begin
  for I := 0 to High(FOwnedFiles) do
    if SameDocumentPath(FOwnedFiles[I], FileName) then Exit;
  SetLength(FOwnedFiles, Length(FOwnedFiles) + 1);
  FOwnedFiles[High(FOwnedFiles)] := FileName;
end;

procedure TRecoveryAssetContext.AdoptDrawingFiles(Drawing: TDrawing2D);
var I: Integer; Entry: TBitmapEntry; FileName: TDocumentPath;
  AssetDirectory: TDocumentPath;
begin
  if Drawing = nil then Exit;
  AssetDirectory := IncludeTrailingPathDelimiter(FRootPath) + 'bitmaps';
  for I := 0 to Drawing.BitmapRegistry.Count - 1 do
  begin
    Entry := Drawing.BitmapRegistry.Objects[I] as TBitmapEntry;
    FileName := TDocumentPath(Entry.GetFullLink);
    if SameDocumentPath(DocumentPathDirectory(FileName), AssetDirectory) and
      DocumentFileExists(FileName) then OwnFile(FileName);
  end;
end;

function TRecoveryAssetContext.CandidatePath: TDocumentPath;
begin
  Result := IncludeTrailingPathDelimiter(FRootPath) + 'recovered.tpx';
end;

function TRecoveryAssetContext.HasOwnedAssets: Boolean;
begin
  Result := Length(FOwnedFiles) > 0;
end;

function TAutoSaveRuntime.NewUntitledKey: string;
var ID: TGUID;
begin
  if CreateGUID(ID) <> 0 then
    raise Exception.Create('Could not create an AutoSave document identifier');
  Result := StringReplace(StringReplace(GUIDToString(ID), '{', '', []),
    '}', '', []);
  Result := StringReplace(Result, '-', '', [rfReplaceAll]);
end;

constructor TAutoSaveRuntime.Create(const RecoveryRoot: string);
begin
  inherited Create;
  FCoordinator := TAutoSaveCoordinator.Create;
  FStore := TAutoSaveStore.Create(RecoveryRoot);
end;

destructor TAutoSaveRuntime.Destroy;
begin
  FreeAndNil(FStore);
  FreeAndNil(FCoordinator);
  inherited Destroy;
end;

function TAutoSaveRuntime.CanPublishSource(Session: TDocumentSession;
  Drawing: TDrawing2D; out Reason: string): Boolean;
begin
  Result := False;
  Reason := '';
  if (Session = nil) or (Session.SourcePath = '') then
  begin
    Reason := 'Save the untitled drawing once before enabling AutoSave';
    Exit;
  end;
  if not Session.CanSaveBack then
  begin
    Reason := 'This imported document has no safe source writer';
    Exit;
  end;
  if (Session.CodecContext is TRecoveryAssetContext) and
    TRecoveryAssetContext(Session.CodecContext).HasOwnedAssets then
  begin
    Reason := 'Save this recovered drawing once to relocate its bitmap assets';
    Exit;
  end;
  if not SameText(Session.SourceFormatId, 'tpx') then
  begin
    Reason := 'AutoSave is not yet available for this source format';
    Exit;
  end;
  if not CanSerializeTpXDocumentWithoutSidecars(Drawing) then
  begin
    Reason := 'AutoSave is unavailable for this output profile';
    Exit;
  end;
  Result := True;
end;

procedure TAutoSaveRuntime.BindDocument(Session: TDocumentSession;
  Drawing: TDrawing2D; AutoSaveEnabled, RecoveryEnabled,
  Dirty: Boolean; NowMS: QWord; const RecoveryKey: string);
var Reason: string; CanSave: Boolean;
begin
  if Session = nil then
    raise Exception.Create('A document session is required');
  CanSave := CanPublishSource(Session, Drawing, Reason);
  FAvailabilityReason := Reason;
  FRequestedAutoSaveEnabled := AutoSaveEnabled;
  if RecoveryKey <> '' then
    FDocumentKey := RecoveryKey
  else if Session.SourcePath <> '' then
    FDocumentKey := DocumentPreferenceKey(Session.SourcePath)
  else
    FDocumentKey := NewUntitledKey;
  FCoordinator.BindDocument(Session.LocalRevision, Session.WatchGeneration,
    CanSave, AutoSaveEnabled, RecoveryEnabled, Dirty, NowMS);
  FLastStatus := '';
  FRecoveryWarning := '';
end;

procedure TAutoSaveRuntime.CloseDocument;
begin
  FCoordinator.CloseDocument;
  FDocumentKey := '';
  FLastStatus := '';
  FRecoveryWarning := '';
end;

procedure TAutoSaveRuntime.NotifyLocalEdit(LocalRevision, NowMS: QWord;
  Dirty: Boolean);
begin
  FCoordinator.NotifyLocalEdit(LocalRevision, NowMS, Dirty);
  if FCoordinator.LastFailure = '' then FLastStatus := '';
end;

procedure TAutoSaveRuntime.NotifyReloadCommitted(LocalRevision,
  WatchGeneration: QWord);
begin
  FCoordinator.NotifyReloadCommitted(LocalRevision, WatchGeneration);
  FLastStatus := '';
end;

procedure TAutoSaveRuntime.RefreshCapability(Session: TDocumentSession;
  Drawing: TDrawing2D; NowMS: QWord);
var Reason: string; CanSave, WasAvailable: Boolean;
begin
  CanSave := CanPublishSource(Session, Drawing, Reason);
  FAvailabilityReason := Reason;
  WasAvailable := FCoordinator.CanSaveBack;
  if WasAvailable = CanSave then Exit;
  FCoordinator.SetCanSaveBack(CanSave);
  FCoordinator.SetAutoSaveEnabled(FRequestedAutoSaveEnabled, NowMS);
  if not CanSave and FRequestedAutoSaveEnabled then
    FLastStatus := Reason
  else if FCoordinator.LastFailure = '' then
    FLastStatus := '';
end;

procedure TAutoSaveRuntime.AcceptSavedRevision(WatchGeneration: QWord);
var Ticket: TAutoSaveTicket;
begin
  Ticket := FCoordinator.CurrentTicket;
  if FCoordinator.AcceptSavedRevision(Ticket, WatchGeneration) then
  begin
    FLastStatus := '';
    try
      if FDocumentKey <> '' then FStore.DeleteDraft(FDocumentKey);
      FRecoveryWarning := '';
    except
      on E: Exception do
        FLastStatus := 'The saved document is clean, but its recovery draft remains: ' +
          E.Message;
    end;
  end;
end;

procedure TAutoSaveRuntime.SetConflict;
begin
  FCoordinator.SetConflict(True);
  FLastStatus := 'AutoSave paused: external changes conflict with local edits';
end;

procedure TAutoSaveRuntime.SetStatus(const Value: string);
begin
  FLastStatus := Value;
end;

procedure TAutoSaveRuntime.SetRecoveryWarning(const Value: string);
begin
  FRecoveryWarning := Value;
end;

function TAutoSaveRuntime.SetAutoSaveEnabled(Value: Boolean;
  NowMS: QWord): Boolean;
begin
  FRequestedAutoSaveEnabled := Value;
  FCoordinator.SetAutoSaveEnabled(Value, NowMS);
  Result := FCoordinator.AutoSaveEnabled;
  if Value and not Result then FLastStatus := FAvailabilityReason
  else if FCoordinator.LastFailure = '' then FLastStatus := '';
end;

procedure TAutoSaveRuntime.SetRecoveryEnabled(Value: Boolean;
  NowMS: QWord);
begin
  FCoordinator.SetRecoveryEnabled(Value, NowMS);
end;

function TAutoSaveRuntime.NextDelayMS(NowMS: QWord): Integer;
begin
  Result := FCoordinator.NextDelayMS(NowMS);
end;

function TAutoSaveRuntime.BuildTpXRecoveryPayload(Drawing: TDrawing2D;
  const SourcePath: TDocumentPath; out UnresolvedLinks: LongWord):
  RawByteString;
var ModelBytes, ChangedModel: RawByteString; Assets: TRecoveryAssetArray;
  OldLinks, NewLinks: TStringList; Entry: TBitmapEntry; Stream: TMemoryStream;
  I, LinkIndex, RewrittenCount: Integer; RelativePath: string;
  BitmapBytes: RawByteString; TotalBytes, EstimatedBytes: Int64;
begin
  if Drawing = nil then raise EWriteError.Create('A drawing is required');
  UnresolvedLinks := 0;
  ModelBytes := SerializeTpXRecoveryBytes(Drawing, SourcePath);
  ChangedModel := ModelBytes;
  OldLinks := TStringList.Create;
  NewLinks := TStringList.Create;
  try
    SetLength(Assets, 0);
    TotalBytes := Length(ModelBytes) + RecoveryPayloadHeaderBytes;
    for I := 0 to Drawing.BitmapRegistry.Count - 1 do
    begin
      Entry := Drawing.BitmapRegistry.Objects[I] as TBitmapEntry;
      if Entry.ImageLink = '' then
      begin
        Inc(UnresolvedLinks);
        Continue;
      end;
      LinkIndex := OldLinks.IndexOf(XmlReplaceChars(Entry.ImageLink));
      if LinkIndex >= 0 then Continue;
      if (Entry.Kind = bek_None) or (Entry.Bitmap = nil) or
        (Entry.Bitmap.Width <= 0) or (Entry.Bitmap.Height <= 0) then
      begin
        Inc(UnresolvedLinks);
        Continue;
      end;
      EstimatedBytes := Int64(Entry.Bitmap.Width) * Entry.Bitmap.Height * 4 + 4096;
      RelativePath := Format('bitmaps/recovery-%.6d.bmp',
        [Length(Assets) + 1]);
      if (Length(Assets) >= RecoveryAssetLimit) or
        (EstimatedBytes + Length(RelativePath) + 72 >
          AutoSaveMaxPayloadBytes - TotalBytes) then
      begin
        Inc(UnresolvedLinks);
        Continue;
      end;
      Stream := TMemoryStream.Create;
      try
        Entry.Bitmap.SaveToStream(Stream);
        if (Stream.Size <= 0) or (Stream.Size + Length(RelativePath) + 72 >
          AutoSaveMaxPayloadBytes - TotalBytes) then
        begin
          Inc(UnresolvedLinks);
          Continue;
        end;
        SetLength(BitmapBytes, Stream.Size);
        Stream.Position := 0;
        Stream.ReadBuffer(BitmapBytes[1], Stream.Size);
      finally
        Stream.Free;
      end;
      Inc(TotalBytes, Length(BitmapBytes) + Length(RelativePath) + 8);
      if TotalBytes > AutoSaveMaxPayloadBytes then
        raise EWriteError.Create('Recovery draft exceeds the configured payload limit');
      SetLength(Assets, Length(Assets) + 1);
      Assets[High(Assets)].RelativePath := RelativePath;
      Assets[High(Assets)].Bytes := BitmapBytes;
      OldLinks.Add(XmlReplaceChars(Entry.ImageLink));
      NewLinks.Add(XmlReplaceChars(RelativePath));
    end;
    ChangedModel := RewriteSerializedBitmapLinks(ModelBytes, OldLinks,
      NewLinks, RewrittenCount);
    if RewrittenCount < Length(Assets) then
      raise EWriteError.Create('A recovery bitmap is not referenced by the model');
  finally
    OldLinks.Free;
    NewLinks.Free;
  end;
  TotalBytes := Length(ChangedModel) + RecoveryPayloadHeaderBytes;
  for I := 0 to High(Assets) do
    Inc(TotalBytes, Length(Assets[I].Bytes) + Length(Assets[I].RelativePath) + 8);
  if TotalBytes > AutoSaveMaxPayloadBytes then
    raise EWriteError.Create('Recovery draft exceeds the configured payload limit');
  Stream := TMemoryStream.Create;
  try
    Stream.WriteBuffer(RecoveryPayloadMagic[1], Length(RecoveryPayloadMagic));
    WriteUInt32(Stream, Length(ChangedModel));
    WriteUInt32(Stream, UnresolvedLinks);
    WriteUInt32(Stream, Length(Assets));
    if Length(ChangedModel) > 0 then
      Stream.WriteBuffer(ChangedModel[1], Length(ChangedModel));
    for I := 0 to High(Assets) do
    begin
      WriteRaw(Stream, RawByteString(Assets[I].RelativePath));
      WriteRaw(Stream, Assets[I].Bytes);
    end;
    SetLength(Result, Stream.Size);
    Stream.Position := 0;
    if Stream.Size > 0 then Stream.ReadBuffer(Result[1], Stream.Size);
  finally
    Stream.Free;
  end;
end;

procedure TAutoSaveRuntime.UnpackTpXRecoveryPayload(
  const Payload: RawByteString; const TempRoot: TDocumentPath;
  out ModelBytes: RawByteString; out AssetContext: TRecoveryAssetContext;
  out UnresolvedLinks: LongWord);
var Stream: TMemoryStream; Magic: array[0..7] of Char;
  ModelLength, AssetCount: LongWord; I: Integer; TotalBytes: Int64;
  Assets: TRecoveryAssetArray; NameBytes: RawByteString;
  Names: TStringList;
begin
  ModelBytes := '';
  AssetContext := nil;
  UnresolvedLinks := 0;
  if Length(Payload) > AutoSaveMaxPayloadBytes then
    raise EReadError.Create('Recovery payload exceeds the configured limit');
  Stream := TMemoryStream.Create;
  Names := TStringList.Create;
  try
    if Length(Payload) > 0 then Stream.WriteBuffer(Payload[1], Length(Payload));
    Stream.Position := 0;
    if Stream.Size < RecoveryPayloadHeaderBytes then
      raise EReadError.Create('Recovery payload header is incomplete');
    Stream.ReadBuffer(Magic, SizeOf(Magic));
    if CompareMem(@Magic[0], @RecoveryPayloadMagic[1], SizeOf(Magic)) = False then
      raise EReadError.Create('Recovery payload signature is invalid');
    ModelLength := ReadUInt32(Stream);
    UnresolvedLinks := ReadUInt32(Stream);
    AssetCount := ReadUInt32(Stream);
    if (ModelLength > AutoSaveMaxPayloadBytes) or
      (AssetCount > RecoveryAssetLimit) or
      (Int64(ModelLength) + Stream.Position > Stream.Size) then
      raise EReadError.Create('Recovery payload header is invalid');
    SetLength(ModelBytes, ModelLength);
    if ModelLength > 0 then Stream.ReadBuffer(ModelBytes[1], ModelLength);
    SetLength(Assets, Integer(AssetCount));
    TotalBytes := ModelLength;
    if AssetCount > 0 then
    for I := 0 to Integer(AssetCount) - 1 do
    begin
      NameBytes := ReadRaw(Stream, 512);
      Assets[I].RelativePath := string(NameBytes);
      if not RecoveryAssetPathIsSafe(Assets[I].RelativePath) then
        raise EReadError.Create('Recovery payload contains an unsafe asset path');
      if Names.IndexOf(Assets[I].RelativePath) >= 0 then
        raise EReadError.Create('Recovery payload contains a duplicate asset path');
      Names.Add(Assets[I].RelativePath);
      Assets[I].Bytes := ReadRaw(Stream, AutoSaveMaxPayloadBytes);
      Inc(TotalBytes, Length(NameBytes) + Length(Assets[I].Bytes) + 8);
      if TotalBytes > AutoSaveMaxPayloadBytes then
        raise EReadError.Create('Recovery payload exceeds the configured limit');
    end;
    if Stream.Position <> Stream.Size then
      raise EReadError.Create('Recovery payload has trailing data');
  finally
    Names.Free;
    Stream.Free;
  end;
  AssetContext := TRecoveryAssetContext.Create(TempRoot, Assets);
end;

procedure TAutoSaveRuntime.SaveRecoveryDraft(Session: TDocumentSession;
  Drawing: TDrawing2D; const Ticket: TAutoSaveTicket);
var Payload: RawByteString; SourcePath: TDocumentPath; FormatId: string;
  UnresolvedLinks: LongWord;
begin
  if not FCoordinator.CanStart(aswRecoveryDraft, Ticket) then Exit;
  Payload := BuildTpXRecoveryPayload(Drawing, Session.SourcePath,
    UnresolvedLinks);
  if not FCoordinator.IsTicketCurrent(Ticket) then Exit;
  SourcePath := Session.SourcePath;
  FormatId := Session.SourceFormatId;
  if FormatId = '' then FormatId := 'tpx';
  FStore.SaveDraft(FDocumentKey, SourcePath, FormatId, 'tpx-assets',
    Session.AcceptedRevision, Session.OriginalSourceBytes,
    Session.LocalRevision, Payload);
  FCoordinator.WorkSucceeded(aswRecoveryDraft, Ticket);
  if UnresolvedLinks > 0 then
    FRecoveryWarning := Format(
      'Recovery draft omits %d linked image(s)', [UnresolvedLinks])
  else
    FRecoveryWarning := '';
  if FCoordinator.LastFailure = '' then FLastStatus := '';
end;

function TAutoSaveRuntime.SaveSource(Session: TDocumentSession;
  Drawing: TDrawing2D; const Ticket: TAutoSaveTicket): Boolean;
var CurrentRevision, NewRevision: TDiskRevision;
  PriorFile, BackupFile: TDocumentPath;
  SourceBytes: RawByteString;
  Reason: string;
begin
  Result := False;
  FLastStatus := '';
  if not CanPublishSource(Session, Drawing, Reason) then
  begin
    FAvailabilityReason := Reason;
    FCoordinator.SetCanSaveBack(False);
    FCoordinator.SetAutoSaveEnabled(FRequestedAutoSaveEnabled, GetTickCount64);
    FLastStatus := Reason;
    raise EWriteError.Create(Reason);
  end;
  if not FCoordinator.CanStart(aswSourceSave, Ticket) then Exit;
  CurrentRevision := ReadDocumentRevision(Session.SourcePath);
  if not SameDiskRevision(CurrentRevision, Session.AcceptedRevision) then
    raise EDocumentConflict.Create('The destination changed on disk');
  SourceBytes := SerializeDocumentToBytes(Session.SourceFormatId, Drawing,
    Session.SourcePath, Session.CodecContext);
  if not FCoordinator.IsTicketCurrent(Ticket) then Exit;
  PriorFile := FStore.SavePriorVersion(FDocumentKey,
    Session.SourcePath, Session.SourceFormatId,
    Session.AcceptedRevision, Session.LocalRevision,
    Session.OriginalSourceBytes);
  if PriorFile = '' then
    raise EWriteError.Create('The prior source version was not retained');
  WriteDocumentAtomically(Session.SourcePath, SourceBytes,
    Session.AcceptedRevision, True, NewRevision, BackupFile);
  Session.AcceptSavedRevision(SourceBytes, NewRevision,
    Session.CodecContext, '');
  Drawing.History.SaveCheckSum;
  if BackupFile <> '' then
    if not DeleteDocumentFile(BackupFile) then
      FLastStatus := 'AutoSave completed; a source backup could not be removed';
  try
    FStore.DeleteDraft(FDocumentKey);
    FRecoveryWarning := '';
  except
    on E: Exception do
      FLastStatus := 'AutoSave completed; the older recovery draft remains: ' +
        E.Message;
  end;
  FCoordinator.SourceSaveSucceeded(Ticket, Session.WatchGeneration);
  Result := not FCoordinator.Dirty;
end;

function TAutoSaveRuntime.ProcessDue(Session: TDocumentSession;
  Drawing: TDrawing2D; NowMS: QWord; out SourceSaved: Boolean): Boolean;
var Work: TAutoSaveWork; Ticket: TAutoSaveTicket;
begin
  SourceSaved := False;
  RefreshCapability(Session, Drawing, NowMS);
  Result := FCoordinator.TakeDue(NowMS, Work, Ticket);
  if not Result then Exit;
  if aswRecoveryDraft in Work then
    try
      SaveRecoveryDraft(Session, Drawing, Ticket);
    except
      on E: Exception do
      begin
        FCoordinator.WorkFailed(aswRecoveryDraft, Ticket, E.Message);
        FLastStatus := 'Recovery draft could not be saved: ' + E.Message;
      end;
    end;
  if (aswSourceSave in Work) and
    FCoordinator.CanStart(aswSourceSave, Ticket) then
    try
      SourceSaved := SaveSource(Session, Drawing, Ticket);
    except
      on E: EDocumentConflict do
      begin
        FCoordinator.SetConflict(True);
        FCoordinator.WorkFailed(aswSourceSave, Ticket, E.Message);
        FLastStatus := 'AutoSave paused: external changes conflict with local edits';
      end;
      on E: Exception do
      begin
        FCoordinator.WorkFailed(aswSourceSave, Ticket, E.Message);
        FLastStatus := 'AutoSave failed: ' + E.Message;
      end;
    end;
end;

end.

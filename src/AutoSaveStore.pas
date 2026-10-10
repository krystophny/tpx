unit AutoSaveStore;

{$mode delphi}{$H+}

interface

uses Classes, SysUtils, DocumentFormats;

const
  { Keep at most 20 active records and 513 MiB total, enough for two
    maximum-size document payloads plus their metadata. }
  AutoSaveMaxRetainedRecords = 20;
  AutoSaveMaxRetainedBytes: Int64 = 513 * 1024 * 1024;
  AutoSaveMaxRecordBytes: Int64 = 513 * 1024 * 1024;
  AutoSaveMaxPayloadBytes: Int64 = 268435456;

type
  TAutoSaveRecordKind = (asrDraft, asrPriorVersion);
  TAutoSaveStoredRecord = record
    Kind: TAutoSaveRecordKind;
    DocumentKey: string;
    SourcePath: string;
    SourceFormatId: string;
    PayloadFormatId: string;
    BaseRevision: TDiskRevision;
    BaseSourceBytes: RawByteString;
    LocalRevision: QWord;
    Payload: RawByteString;
  end;

  { Persistent recovery drafts and pre-publication source snapshots live below
    the application's config directory, never beside the document. }
  TAutoSaveStore = class
  private
    FRoot: string;
    procedure CleanAbandonedTemporaryFiles;
    function DraftFile(const DocumentKey: string): string;
    function NewVersionFile(const DocumentKey: string): string;
    procedure CheckDraftCapacity(NewSize: Int64;
      const ReplacingFile: string);
    procedure MakeRoom(NewSize: Int64; const ReplacingFile: string);
    procedure WriteRecord(const FileName: string;
      const Entry: TAutoSaveStoredRecord);
  public
    constructor Create(const Root: string);
    procedure SaveDraft(const DocumentKey, SourcePath, SourceFormatId,
      PayloadFormatId: string; const BaseRevision: TDiskRevision;
      const BaseSourceBytes: RawByteString; LocalRevision: QWord;
      const DraftBytes: RawByteString);
    function SavePriorVersion(const DocumentKey, SourcePath,
      SourceFormatId: string; const BaseRevision: TDiskRevision;
      LocalRevision: QWord;
      const SourceBytes: RawByteString): string;
    procedure DeleteDraft(const DocumentKey: string);
    procedure ListDrafts(Files: TStrings);
    procedure ListPriorVersions(const DocumentKey: string; Files: TStrings);
    function LoadRecord(const FileName: string;
      out Entry: TAutoSaveStoredRecord): Boolean;
    property Root: string read FRoot;
  end;

implementation

uses
  {$IFDEF UNIX}{$IFNDEF WASI} BaseUnix, Unix, {$ENDIF}{$ENDIF}
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  Math;

const
  RecordMagic = 'TPXREC03';
  RecordHeaderBytes = 8 + 1 + 4 + 4 + 4 + 4 + 4 + 4 + 8 + 8 + 1 + 8 + 8 + 8;

function SafeKey(const Value: string): Boolean;
var
  I: Integer;
begin
  Result := (Value <> '') and (Length(Value) <= 80);
  for I := 1 to Length(Value) do
    if not (Value[I] in ['a'..'z', 'A'..'Z', '0'..'9', '-', '_']) then
      Exit(False);
end;

function EncodedStringSize(const Value: string): Int64;
begin
  Result := 4 + Length(UTF8Encode(Value));
end;

function EncodedRecordSize(const Entry: TAutoSaveStoredRecord): Int64;
begin
  Result := RecordHeaderBytes + EncodedStringSize(Entry.DocumentKey) +
    EncodedStringSize(Entry.SourcePath) +
    EncodedStringSize(Entry.SourceFormatId) +
    EncodedStringSize(Entry.PayloadFormatId) +
    EncodedStringSize(Entry.BaseRevision.ContentDigest) +
    EncodedStringSize(Entry.BaseRevision.Identity) +
    Length(Entry.BaseSourceBytes) + Length(Entry.Payload);
end;

procedure WriteUtf8(Stream: TStream; const Value: string);
var
  Encoded: UTF8String;
  Len: Longint;
begin
  Encoded := UTF8Encode(Value);
  Len := Length(Encoded);
  Stream.WriteBuffer(Len, SizeOf(Len));
  if Len > 0 then Stream.WriteBuffer(Encoded[1], Len);
end;

function ReadUtf8(Stream: TStream): string;
var
  Encoded: UTF8String;
  Len: Longint;
begin
  Stream.ReadBuffer(Len, SizeOf(Len));
  if (Len < 0) or (Len > 1024 * 1024) or
    (Len > Stream.Size - Stream.Position) then
    raise Exception.Create('Invalid AutoSave record string length');
  SetLength(Encoded, Len);
  if Len > 0 then Stream.ReadBuffer(Encoded[1], Len);
  Result := string(UTF8Decode(Encoded));
end;

procedure WriteDiskRevision(Stream: TStream; const Revision: TDiskRevision);
var
  ExistsByte: Byte;
begin
  WriteUtf8(Stream, Revision.ContentDigest);
  Stream.WriteBuffer(Revision.Size, SizeOf(Revision.Size));
  Stream.WriteBuffer(Revision.ModifiedUTC, SizeOf(Revision.ModifiedUTC));
  WriteUtf8(Stream, Revision.Identity);
  if Revision.Exists then ExistsByte := 1 else ExistsByte := 0;
  Stream.WriteBuffer(ExistsByte, SizeOf(ExistsByte));
end;

procedure ReadDiskRevision(Stream: TStream; out Revision: TDiskRevision);
var
  ExistsByte: Byte;
begin
  Revision.ContentDigest := ReadUtf8(Stream);
  Stream.ReadBuffer(Revision.Size, SizeOf(Revision.Size));
  Stream.ReadBuffer(Revision.ModifiedUTC, SizeOf(Revision.ModifiedUTC));
  Revision.Identity := ReadUtf8(Stream);
  Stream.ReadBuffer(ExistsByte, SizeOf(ExistsByte));
  if ExistsByte > 1 then
    raise Exception.Create('Invalid AutoSave base revision flag');
  Revision.Exists := ExistsByte = 1;
end;

procedure FlushStream(Stream: TFileStream);
begin
  {$IFDEF UNIX}{$IFNDEF WASI}
  if fpfsync(Stream.Handle) <> 0 then
    raise Exception.Create('Could not flush AutoSave recovery data');
  {$ENDIF}{$ENDIF}
  {$IFDEF MSWINDOWS}
  if not FlushFileBuffers(Stream.Handle) then
    RaiseLastOSError;
  {$ENDIF}
end;

procedure FlushDirectory(const DirectoryName: string);
{$IFDEF UNIX}{$IFNDEF WASI}
var
  Handle: cint;
{$ENDIF}{$ENDIF}
begin
  {$IFDEF UNIX}{$IFNDEF WASI}
  Handle := fpOpen(PChar(DirectoryName), O_RdOnly);
  if Handle < 0 then
    raise Exception.Create('Could not open AutoSave recovery directory');
  try
    if fpfsync(Handle) <> 0 then
      raise Exception.Create('Could not flush AutoSave recovery directory');
  finally
    fpClose(Handle);
  end;
  {$ENDIF}{$ENDIF}
end;

procedure ReplaceFile(const SourceName, DestName: string);
begin
  {$IFDEF UNIX}
  {$IFNDEF WASI}
  if fpRename(PChar(SourceName), PChar(DestName)) <> 0 then
    RaiseLastOSError;
  {$ELSE}
  if not RenameFile(SourceName, DestName) then
    raise Exception.Create('Could not publish AutoSave recovery data');
  {$ENDIF}
  {$ELSE}
  {$IFDEF MSWINDOWS}
  if not MoveFileEx(PChar(SourceName), PChar(DestName),
    MOVEFILE_REPLACE_EXISTING or MOVEFILE_WRITE_THROUGH) then
    RaiseLastOSError;
  {$ELSE}
  if FileExists(DestName) and not DeleteFile(DestName) then
    raise Exception.Create('Could not replace AutoSave recovery data');
  if not RenameFile(SourceName, DestName) then
    raise Exception.Create('Could not publish AutoSave recovery data');
  {$ENDIF}
  {$ENDIF}
end;

function NewTemporaryName(const DestName: string): string;
var
  ID: TGUID;
begin
  if CreateGUID(ID) <> 0 then
    raise Exception.Create('Could not create AutoSave recovery identifier');
  Result := DestName + '.' +
    StringReplace(StringReplace(GUIDToString(ID), '{', '', []), '}', '', []) +
    '.tmp';
end;

constructor TAutoSaveStore.Create(const Root: string);
begin
  inherited Create;
  if Trim(Root) = '' then
    raise Exception.Create('AutoSave recovery directory is required');
  FRoot := IncludeTrailingPathDelimiter(ExpandFileName(Root));
  if not ForceDirectories(FRoot) and not DirectoryExists(FRoot) then
    raise Exception.Create('Could not create AutoSave recovery directory');
  {$IFDEF UNIX}{$IFNDEF WASI}
  if fpChmod(PChar(FRoot), &700) <> 0 then
    raise Exception.Create('Could not protect AutoSave recovery directory');
  {$ENDIF}{$ENDIF}
  CleanAbandonedTemporaryFiles;
end;

procedure TAutoSaveStore.CleanAbandonedTemporaryFiles;
var
  Search: TSearchRec;
  Changed: Boolean;
begin
  Changed := False;
  if FindFirst(FRoot + '*.tmp', faAnyFile, Search) <> 0 then Exit;
  try
    repeat
      if (Search.Attr and faDirectory) <> 0 then Continue;
      if (Now - Search.TimeStamp > 1) and DeleteFile(FRoot + Search.Name) then
        Changed := True;
    until FindNext(Search) <> 0;
  finally
    FindClose(Search);
  end;
  if Changed then FlushDirectory(FRoot);
end;

function TAutoSaveStore.DraftFile(const DocumentKey: string): string;
begin
  if not SafeKey(DocumentKey) then
    raise Exception.Create('Invalid AutoSave document key');
  Result := FRoot + DocumentKey + '.draft';
end;

function TAutoSaveStore.NewVersionFile(const DocumentKey: string): string;
var
  ID: TGUID;
  Token: string;
begin
  if not SafeKey(DocumentKey) then
    raise Exception.Create('Invalid AutoSave document key');
  if CreateGUID(ID) <> 0 then
    raise Exception.Create('Could not create AutoSave version identifier');
  Token := StringReplace(StringReplace(GUIDToString(ID), '{', '', []), '}', '', []);
  Result := FRoot + DocumentKey + '.' + Token + '.version';
end;

procedure TAutoSaveStore.WriteRecord(const FileName: string;
  const Entry: TAutoSaveStoredRecord);
var
  TempName: string;
  Stream: TFileStream;
  KindByte: Byte;
  PayloadSize: QWord;
begin
  if (Length(Entry.Payload) > AutoSaveMaxPayloadBytes) or
    (Length(Entry.BaseSourceBytes) > AutoSaveMaxPayloadBytes) or
    (EncodedRecordSize(Entry) > AutoSaveMaxRecordBytes) then
    raise Exception.Create('AutoSave recovery record exceeds the storage limit');
  TempName := NewTemporaryName(FileName);
  Stream := nil;
  try
    Stream := TFileStream.Create(TempName, fmCreate);
    Stream.WriteBuffer(RecordMagic[1], Length(RecordMagic));
    if Entry.Kind = asrDraft then KindByte := 1 else KindByte := 2;
    Stream.WriteBuffer(KindByte, SizeOf(KindByte));
    WriteUtf8(Stream, Entry.DocumentKey);
    WriteUtf8(Stream, Entry.SourcePath);
    WriteUtf8(Stream, Entry.SourceFormatId);
    WriteUtf8(Stream, Entry.PayloadFormatId);
    WriteDiskRevision(Stream, Entry.BaseRevision);
    Stream.WriteBuffer(Entry.LocalRevision, SizeOf(Entry.LocalRevision));
    PayloadSize := Length(Entry.BaseSourceBytes);
    Stream.WriteBuffer(PayloadSize, SizeOf(PayloadSize));
    if PayloadSize > 0 then
      Stream.WriteBuffer(Entry.BaseSourceBytes[1], Longint(PayloadSize));
    PayloadSize := Length(Entry.Payload);
    Stream.WriteBuffer(PayloadSize, SizeOf(PayloadSize));
    if PayloadSize > 0 then
      Stream.WriteBuffer(Entry.Payload[1], Longint(PayloadSize));
    FlushStream(Stream);
    FreeAndNil(Stream);
    ReplaceFile(TempName, FileName);
    FlushDirectory(ExtractFileDir(FileName));
  finally
    Stream.Free;
    if FileExists(TempName) then DeleteFile(TempName);
  end;
end;

procedure TAutoSaveStore.SaveDraft(const DocumentKey, SourcePath,
  SourceFormatId, PayloadFormatId: string;
  const BaseRevision: TDiskRevision;
  const BaseSourceBytes: RawByteString; LocalRevision: QWord;
  const DraftBytes: RawByteString);
var
  Entry: TAutoSaveStoredRecord;
  Target: string;
begin
  Entry.Kind := asrDraft;
  Entry.DocumentKey := DocumentKey;
  Entry.SourcePath := SourcePath;
  Entry.SourceFormatId := SourceFormatId;
  Entry.PayloadFormatId := PayloadFormatId;
  Entry.BaseRevision := BaseRevision;
  Entry.BaseSourceBytes := BaseSourceBytes;
  Entry.LocalRevision := LocalRevision;
  Entry.Payload := DraftBytes;
  Target := DraftFile(DocumentKey);
  CheckDraftCapacity(EncodedRecordSize(Entry), Target);
  WriteRecord(Target, Entry);
  MakeRoom(EncodedRecordSize(Entry), Target);
end;

function TAutoSaveStore.SavePriorVersion(const DocumentKey, SourcePath,
  SourceFormatId: string; const BaseRevision: TDiskRevision;
  LocalRevision: QWord;
  const SourceBytes: RawByteString): string;
var
  Entry: TAutoSaveStoredRecord;
begin
  Entry.Kind := asrPriorVersion;
  Entry.DocumentKey := DocumentKey;
  Entry.SourcePath := SourcePath;
  Entry.SourceFormatId := SourceFormatId;
  Entry.PayloadFormatId := SourceFormatId;
  Entry.BaseRevision := BaseRevision;
  Entry.BaseSourceBytes := '';
  Entry.LocalRevision := LocalRevision;
  Entry.Payload := SourceBytes;
  Result := NewVersionFile(DocumentKey);
  CheckDraftCapacity(EncodedRecordSize(Entry), '');
  WriteRecord(Result, Entry);
  MakeRoom(EncodedRecordSize(Entry), Result);
end;

procedure TAutoSaveStore.CheckDraftCapacity(NewSize: Int64;
  const ReplacingFile: string);
var
  Search: TSearchRec;
  Count: Integer;
  TotalSize: Int64;
begin
  Count := 0;
  TotalSize := 0;
  if FindFirst(FRoot + '*.draft', faAnyFile, Search) = 0 then
  try
    repeat
      if (Search.Attr and faDirectory) <> 0 then Continue;
      if (FRoot + Search.Name) = ReplacingFile then Continue;
      Inc(Count);
      Inc(TotalSize, Search.Size);
    until FindNext(Search) <> 0;
  finally
    FindClose(Search);
  end;
  if (Count + 1 > AutoSaveMaxRetainedRecords) or
    (TotalSize + NewSize > AutoSaveMaxRetainedBytes) then
    raise Exception.Create('AutoSave recovery storage limit reached; existing drafts were kept');
end;

procedure TAutoSaveStore.MakeRoom(NewSize: Int64;
  const ReplacingFile: string);
var
  Search: TSearchRec;
  Count: Integer;
  TotalSize: Int64;
  OldestName: string;
  OldestTime: TDateTime;
  FoundVersion: Boolean;
  OldestSize: Int64;
begin
  Count := 0;
  TotalSize := 0;
  if FindFirst(FRoot + '*.draft', faAnyFile, Search) = 0 then
  try
    repeat
      if (Search.Attr and faDirectory) <> 0 then Continue;
      if (FRoot + Search.Name) = ReplacingFile then Continue;
      Inc(Count);
      Inc(TotalSize, Search.Size);
    until FindNext(Search) <> 0;
  finally
    FindClose(Search);
  end;
  if FindFirst(FRoot + '*.version', faAnyFile, Search) = 0 then
  try
    repeat
      if (Search.Attr and faDirectory) <> 0 then Continue;
      if (FRoot + Search.Name) = ReplacingFile then Continue;
      Inc(Count);
      Inc(TotalSize, Search.Size);
    until FindNext(Search) <> 0;
  finally
    FindClose(Search);
  end;
  while (Count + 1 > AutoSaveMaxRetainedRecords) or
    (TotalSize + NewSize > AutoSaveMaxRetainedBytes) do
  begin
    OldestName := '';
    OldestTime := High(Longint);
    FoundVersion := False;
    if FindFirst(FRoot + '*.version', faAnyFile, Search) = 0 then
    try
      repeat
        if (Search.Attr and faDirectory) <> 0 then Continue;
        if (FRoot + Search.Name) = ReplacingFile then Continue;
        if (not FoundVersion) or (Search.TimeStamp < OldestTime) then
        begin
          FoundVersion := True;
          OldestTime := Search.TimeStamp;
          OldestName := FRoot + Search.Name;
          OldestSize := Search.Size;
        end;
      until FindNext(Search) <> 0;
    finally
      FindClose(Search);
    end;
    if not FoundVersion then
      raise Exception.Create('AutoSave recovery storage limit reached; existing drafts were kept');
    if not DeleteFile(OldestName) then
      raise Exception.Create('Could not trim an old AutoSave version');
    FlushDirectory(FRoot);
    Dec(Count);
    Dec(TotalSize, OldestSize);
  end;
end;

procedure TAutoSaveStore.DeleteDraft(const DocumentKey: string);
var
  FileName: string;
begin
  FileName := DraftFile(DocumentKey);
  if FileExists(FileName) and not DeleteFile(FileName) then
    raise Exception.Create('Could not remove saved AutoSave draft');
  FlushDirectory(FRoot);
end;

procedure TAutoSaveStore.ListDrafts(Files: TStrings);
var
  Search: TSearchRec;
begin
  Files.Clear;
  if FindFirst(FRoot + '*.draft', faAnyFile, Search) <> 0 then Exit;
  try
    repeat
      if (Search.Attr and faDirectory) = 0 then
        Files.Add(FRoot + Search.Name);
    until FindNext(Search) <> 0;
  finally
    FindClose(Search);
  end;
end;

procedure TAutoSaveStore.ListPriorVersions(const DocumentKey: string;
  Files: TStrings);
var
  Search: TSearchRec;
begin
  if not SafeKey(DocumentKey) then
    raise Exception.Create('Invalid AutoSave document key');
  Files.Clear;
  if FindFirst(FRoot + DocumentKey + '.*.version', faAnyFile,
    Search) <> 0 then Exit;
  try
    repeat
      if (Search.Attr and faDirectory) = 0 then
        Files.Add(FRoot + Search.Name);
    until FindNext(Search) <> 0;
  finally
    FindClose(Search);
  end;
end;

function TAutoSaveStore.LoadRecord(const FileName: string;
  out Entry: TAutoSaveStoredRecord): Boolean;
var
  Stream: TFileStream;
  Magic: array[0..7] of Char;
  KindByte: Byte;
  PayloadSize: QWord;
begin
  Entry.DocumentKey := '';
  Entry.SourcePath := '';
  Entry.SourceFormatId := '';
  Entry.PayloadFormatId := '';
  Entry.BaseRevision.ContentDigest := '';
  Entry.BaseRevision.Size := 0;
  Entry.BaseRevision.ModifiedUTC := 0;
  Entry.BaseRevision.Identity := '';
  Entry.BaseRevision.Exists := False;
  Entry.BaseSourceBytes := '';
  Entry.LocalRevision := 0;
  Entry.Payload := '';
  Entry.Kind := asrDraft;
  Result := False;
  Stream := nil;
  try
    try
      Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
      if Stream.Size < RecordHeaderBytes then Exit;
      Stream.ReadBuffer(Magic, SizeOf(Magic));
      if not CompareMem(@Magic[0], @RecordMagic[1], SizeOf(Magic)) then Exit;
      Stream.ReadBuffer(KindByte, SizeOf(KindByte));
      if (KindByte <> 1) and (KindByte <> 2) then Exit;
      Entry.DocumentKey := ReadUtf8(Stream);
      Entry.SourcePath := ReadUtf8(Stream);
      Entry.SourceFormatId := ReadUtf8(Stream);
      Entry.PayloadFormatId := ReadUtf8(Stream);
      ReadDiskRevision(Stream, Entry.BaseRevision);
      Stream.ReadBuffer(Entry.LocalRevision, SizeOf(Entry.LocalRevision));
      Stream.ReadBuffer(PayloadSize, SizeOf(PayloadSize));
      if (PayloadSize > AutoSaveMaxPayloadBytes) or
        (PayloadSize > QWord(Stream.Size - Stream.Position)) then Exit;
      SetLength(Entry.BaseSourceBytes, PayloadSize);
      if PayloadSize > 0 then
        Stream.ReadBuffer(Entry.BaseSourceBytes[1], Longint(PayloadSize));
      Stream.ReadBuffer(PayloadSize, SizeOf(PayloadSize));
      if (PayloadSize > AutoSaveMaxPayloadBytes) or
        (PayloadSize <> QWord(Stream.Size - Stream.Position)) then Exit;
      SetLength(Entry.Payload, PayloadSize);
      if PayloadSize > 0 then
        Stream.ReadBuffer(Entry.Payload[1], Longint(PayloadSize));
      if not SafeKey(Entry.DocumentKey) then Exit;
      if KindByte = 1 then Entry.Kind := asrDraft
      else Entry.Kind := asrPriorVersion;
      Result := True;
    except
      Result := False;
    end;
  finally
    Stream.Free;
  end;
end;

end.

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
    SourcePath: TDocumentPath;
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
    FRoot: TDocumentPath;
    procedure CleanAbandonedTemporaryFiles;
    function DraftFile(const DocumentKey: string): TDocumentPath;
    function NewVersionFile(const DocumentKey: string): TDocumentPath;
    procedure CheckDraftCapacity(NewSize: Int64;
      const ReplacingFile: TDocumentPath);
    procedure MakeRoom(NewSize: Int64; const ReplacingFile: TDocumentPath);
    procedure WriteRecord(const FileName: TDocumentPath;
      const Entry: TAutoSaveStoredRecord);
  public
    constructor Create(const Root: TDocumentPath);
    procedure SaveDraft(const DocumentKey: string; const SourcePath: TDocumentPath;
      const SourceFormatId,
      PayloadFormatId: string; const BaseRevision: TDiskRevision;
      const BaseSourceBytes: RawByteString; LocalRevision: QWord;
      const DraftBytes: RawByteString);
    function SavePriorVersion(const DocumentKey: string;
      const SourcePath: TDocumentPath;
      SourceFormatId: string; const BaseRevision: TDiskRevision;
      LocalRevision: QWord;
      const SourceBytes: RawByteString): TDocumentPath;
    procedure DeleteDraft(const DocumentKey: string);
    procedure ListDrafts(Files: TStrings);
    procedure ListPriorVersions(const DocumentKey: string; Files: TStrings);
    function LoadRecord(const FileName: TDocumentPath;
      out Entry: TAutoSaveStoredRecord): Boolean;
    property Root: TDocumentPath read FRoot;
  end;

implementation

uses
  {$IFDEF UNIX}{$IFNDEF WASI} BaseUnix, Unix, {$ENDIF}{$ENDIF}
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  Math;

const
  RecordMagic = 'TPXREC03';
  RecordHeaderBytes = 8 + 1 + 4 + 4 + 4 + 4 + 4 + 4 + 8 + 8 + 1 + 8 + 8 + 8;
{$IFDEF MSWINDOWS}
  AutoSaveMoveFileWriteThrough = $00000008;
{$ENDIF}

type
  TStoreFileInfo = record
    Name: TDocumentPath;
    Size: Int64;
    Modified: TDateTime;
    IsDirectory: Boolean;
  end;
  TStoreFileInfoArray = array of TStoreFileInfo;
{$IFDEF MSWINDOWS}
  TOwnedHandleStream = class(THandleStream)
  public
    destructor Destroy; override;
  end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function WideStorePath(const FileName: TDocumentPath): UnicodeString;
begin
  Result := UTF8Decode(FileName);
  if Length(Result) < MAX_PATH then Exit;
  if Copy(Result, 1, 4) = '\\?\' then Exit;
  if Copy(Result, 1, 2) = '\\' then
    Result := '\\?\UNC\' + Copy(Result, 3, MaxInt)
  else
    Result := '\\?\' + Result;
end;

destructor TOwnedHandleStream.Destroy;
begin
  if Handle <> INVALID_HANDLE_VALUE then Windows.CloseHandle(Handle);
  inherited Destroy;
end;
{$ENDIF}

function OpenStoreFile(const FileName: TDocumentPath;
  ForWrite: Boolean): TStream;
{$IFDEF MSWINDOWS}
var Handle: THandle; Access, ShareMode, Creation: DWORD; WideName: UnicodeString;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  if ForWrite then
  begin
    Access := GENERIC_WRITE;
    ShareMode := 0;
    Creation := CREATE_NEW;
  end
  else
  begin
    Access := GENERIC_READ;
    ShareMode := FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE;
    Creation := OPEN_EXISTING;
  end;
  WideName := WideStorePath(FileName);
  Handle := Windows.CreateFileW(PWideChar(WideName), Access, ShareMode, nil,
    Creation, FILE_ATTRIBUTE_NORMAL, 0);
  if Handle = INVALID_HANDLE_VALUE then RaiseLastOSError;
  try
    Result := TOwnedHandleStream.Create(Handle);
  except
    Windows.CloseHandle(Handle);
    raise;
  end;
{$ELSE}
  if ForWrite then Result := TFileStream.Create(FileName, fmCreate)
  else Result := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
{$ENDIF}
end;

function StoreFiles(const Pattern: TDocumentPath): TStoreFileInfoArray;
{$IFDEF MSWINDOWS}
var Search: WIN32_FIND_DATAW; SearchHandle: THandle; WidePattern: UnicodeString;
  Item: TStoreFileInfo; SystemTime: TSystemTime;
{$ELSE}
var Search: TSearchRec; Item: TStoreFileInfo;
{$ENDIF}
begin
  Result := nil;
{$IFDEF MSWINDOWS}
  WidePattern := WideStorePath(Pattern);
  SearchHandle := Windows.FindFirstFileW(PWideChar(WidePattern), Search);
  if SearchHandle = INVALID_HANDLE_VALUE then Exit;
  try
    repeat
      Item.Name := UTF8Encode(UnicodeString(PWideChar(@Search.cFileName[0])));
      Item.Size := (Int64(Search.nFileSizeHigh) shl 32) or Search.nFileSizeLow;
      if Windows.FileTimeToSystemTime(Search.ftLastWriteTime, SystemTime) then
        Item.Modified := SystemTimeToDateTime(SystemTime)
      else
        Item.Modified := 0;
      Item.IsDirectory := (Search.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0;
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Item;
    until not Windows.FindNextFileW(SearchHandle, Search);
  finally
    Windows.FindClose(SearchHandle);
  end;
{$ELSE}
  if FindFirst(Pattern, faAnyFile, Search) <> 0 then Exit;
  try
    repeat
      Item.Name := UTF8String(Search.Name);
      Item.Size := Search.Size;
      Item.Modified := Search.TimeStamp;
      Item.IsDirectory := (Search.Attr and faDirectory) <> 0;
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Item;
    until FindNext(Search) <> 0;
  finally
    FindClose(Search);
  end;
{$ENDIF}
end;

function HasPathSeparator(const FileName: TDocumentPath): Boolean;
begin
  Result := (Pos('/', FileName) > 0) or (Pos('\', FileName) > 0);
{$IFDEF MSWINDOWS}
  Result := Result or ((Length(FileName) >= 2) and (FileName[2] = ':'));
{$ENDIF}
end;

function FullStoreFileName(const Root, FileName: TDocumentPath): TDocumentPath;
begin
  if HasPathSeparator(FileName) then Result := NormalizeDocumentPath(FileName)
  else Result := Root + FileName;
end;

function SafeKey(const Value: string): Boolean;
var
  I: Integer;
begin
  Result := (Value <> '') and (Length(Value) <= 80);
  for I := 1 to Length(Value) do
    if not (Value[I] in ['a'..'z', 'A'..'Z', '0'..'9', '-', '_']) then
      Exit(False);
end;

function EncodedStringSize(const Value: UTF8String): Int64;
begin
  Result := 4 + Length(Value);
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

procedure WriteUtf8(Stream: TStream; const Value: UTF8String);
var
  Len: Longint;
begin
  Len := Length(Value);
  Stream.WriteBuffer(Len, SizeOf(Len));
  if Len > 0 then Stream.WriteBuffer(Value[1], Len);
end;

function ReadUtf8(Stream: TStream): UTF8String;
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
  Result := Encoded;
end;

procedure WriteDiskRevision(Stream: TStream; const Revision: TDiskRevision);
var
  ExistsByte: Byte;
begin
  WriteUtf8(Stream, UTF8String(Revision.ContentDigest));
  Stream.WriteBuffer(Revision.Size, SizeOf(Revision.Size));
  Stream.WriteBuffer(Revision.ModifiedUTC, SizeOf(Revision.ModifiedUTC));
  WriteUtf8(Stream, UTF8String(Revision.Identity));
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

procedure FlushStream(Stream: TStream);
begin
  {$IFDEF UNIX}{$IFNDEF WASI}
  if fpfsync(TFileStream(Stream).Handle) <> 0 then
    raise Exception.Create('Could not flush AutoSave recovery data');
  {$ENDIF}{$ENDIF}
  {$IFDEF MSWINDOWS}
  if not FlushFileBuffers(THandleStream(Stream).Handle) then
    RaiseLastOSError;
  {$ENDIF}
end;

procedure FlushDirectory(const DirectoryName: TDocumentPath);
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

procedure ReplaceFile(const SourceName, DestName: TDocumentPath);
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
  if not MoveFileExW(PWideChar(WideStorePath(SourceName)),
    PWideChar(WideStorePath(DestName)),
    MOVEFILE_REPLACE_EXISTING or AutoSaveMoveFileWriteThrough) then
    RaiseLastOSError;
  {$ELSE}
  if FileExists(DestName) and not DeleteFile(DestName) then
    raise Exception.Create('Could not replace AutoSave recovery data');
  if not RenameFile(SourceName, DestName) then
    raise Exception.Create('Could not publish AutoSave recovery data');
  {$ENDIF}
  {$ENDIF}
end;

function NewTemporaryName(const DestName: TDocumentPath): TDocumentPath;
var
  ID: TGUID;
begin
  if CreateGUID(ID) <> 0 then
    raise Exception.Create('Could not create AutoSave recovery identifier');
  Result := DestName + '.' +
    StringReplace(StringReplace(GUIDToString(ID), '{', '', []), '}', '', []) +
    '.tmp';
end;

constructor TAutoSaveStore.Create(const Root: TDocumentPath);
var NormalizedRoot: TDocumentPath;
begin
  inherited Create;
  if Trim(Root) = '' then
    raise Exception.Create('AutoSave recovery directory is required');
  NormalizedRoot := NormalizeDocumentPath(Root);
  FRoot := NormalizedRoot;
  if (FRoot <> '') and not (FRoot[Length(FRoot)] in ['/', '\']) then
    FRoot := FRoot + PathDelim;
  if not EnsureDocumentDirectoryExists(NormalizedRoot) then
    raise Exception.Create('Could not create AutoSave recovery directory');
  {$IFDEF UNIX}{$IFNDEF WASI}
  if fpChmod(PChar(NormalizedRoot), &700) <> 0 then
    raise Exception.Create('Could not protect AutoSave recovery directory');
  {$ENDIF}{$ENDIF}
  CleanAbandonedTemporaryFiles;
end;

procedure TAutoSaveStore.CleanAbandonedTemporaryFiles;
var
  Files: TStoreFileInfoArray;
  FileName: TDocumentPath;
  I: Integer;
  Changed: Boolean;
begin
  Changed := False;
  Files := StoreFiles(FRoot + '*.tmp');
  for I := 0 to High(Files) do
  begin
    if Files[I].IsDirectory then Continue;
    FileName := FRoot + Files[I].Name;
    if (Now - Files[I].Modified) > 1 then
      if DeleteDocumentFile(FileName) then
        Changed := True;
  end;
  if Changed then FlushDirectory(FRoot);
end;

function TAutoSaveStore.DraftFile(const DocumentKey: string): TDocumentPath;
begin
  if not SafeKey(DocumentKey) then
    raise Exception.Create('Invalid AutoSave document key');
  Result := FRoot + DocumentKey + '.draft';
end;

function TAutoSaveStore.NewVersionFile(const DocumentKey: string): TDocumentPath;
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

procedure TAutoSaveStore.WriteRecord(const FileName: TDocumentPath;
  const Entry: TAutoSaveStoredRecord);
var
  TempName: TDocumentPath;
  Stream: TStream;
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
    Stream := OpenStoreFile(TempName, True);
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
    FlushDirectory(DocumentPathDirectory(FileName));
  finally
    Stream.Free;
    if DocumentFileExists(TempName) then DeleteDocumentFile(TempName);
  end;
end;

procedure TAutoSaveStore.SaveDraft(const DocumentKey: string;
  const SourcePath: TDocumentPath; const SourceFormatId, PayloadFormatId: string;
  const BaseRevision: TDiskRevision;
  const BaseSourceBytes: RawByteString; LocalRevision: QWord;
  const DraftBytes: RawByteString);
var
  Entry: TAutoSaveStoredRecord;
  Target: TDocumentPath;
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

function TAutoSaveStore.SavePriorVersion(const DocumentKey: string;
  const SourcePath: TDocumentPath; SourceFormatId: string;
  const BaseRevision: TDiskRevision;
  LocalRevision: QWord;
  const SourceBytes: RawByteString): TDocumentPath;
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
  const ReplacingFile: TDocumentPath);
var
  Files: TStoreFileInfoArray;
  FileName: TDocumentPath;
  I: Integer;
  Count: Integer;
  TotalSize: Int64;
begin
  Count := 0;
  TotalSize := 0;
  Files := StoreFiles(FRoot + '*.draft');
  for I := 0 to High(Files) do
  begin
    if Files[I].IsDirectory then Continue;
    FileName := FRoot + Files[I].Name;
    if FileName = ReplacingFile then Continue;
    Inc(Count);
    Inc(TotalSize, Files[I].Size);
  end;
  if (Count + 1 > AutoSaveMaxRetainedRecords) or
    (TotalSize + NewSize > AutoSaveMaxRetainedBytes) then
    raise Exception.Create('AutoSave recovery storage limit reached; existing drafts were kept');
end;

procedure TAutoSaveStore.MakeRoom(NewSize: Int64;
  const ReplacingFile: TDocumentPath);
var
  Files: TStoreFileInfoArray;
  FileName: TDocumentPath;
  I: Integer;
  Count: Integer;
  TotalSize: Int64;
  OldestName: TDocumentPath;
  OldestTime: TDateTime;
  FoundVersion: Boolean;
  OldestSize: Int64;
begin
  Count := 0;
  TotalSize := 0;
  Files := StoreFiles(FRoot + '*.draft');
  for I := 0 to High(Files) do
  begin
    if Files[I].IsDirectory then Continue;
    FileName := FRoot + Files[I].Name;
    if FileName = ReplacingFile then Continue;
    Inc(Count);
    Inc(TotalSize, Files[I].Size);
  end;
  Files := StoreFiles(FRoot + '*.version');
  for I := 0 to High(Files) do
  begin
    if Files[I].IsDirectory then Continue;
    FileName := FRoot + Files[I].Name;
    if FileName = ReplacingFile then Continue;
    Inc(Count);
    Inc(TotalSize, Files[I].Size);
  end;
  while (Count + 1 > AutoSaveMaxRetainedRecords) or
    (TotalSize + NewSize > AutoSaveMaxRetainedBytes) do
  begin
    OldestName := '';
    OldestTime := High(Longint);
    FoundVersion := False;
    Files := StoreFiles(FRoot + '*.version');
    for I := 0 to High(Files) do
    begin
      if Files[I].IsDirectory then Continue;
      FileName := FRoot + Files[I].Name;
      if FileName = ReplacingFile then Continue;
      if (not FoundVersion) or (Files[I].Modified < OldestTime) then
      begin
        FoundVersion := True;
        OldestTime := Files[I].Modified;
        OldestName := FileName;
        OldestSize := Files[I].Size;
      end;
    end;
    if not FoundVersion then
      raise Exception.Create('AutoSave recovery storage limit reached; existing drafts were kept');
    if not DeleteDocumentFile(OldestName) then
      raise Exception.Create('Could not trim an old AutoSave version');
    FlushDirectory(FRoot);
    Dec(Count);
    Dec(TotalSize, OldestSize);
  end;
end;

procedure TAutoSaveStore.DeleteDraft(const DocumentKey: string);
var
  FileName: TDocumentPath;
begin
  FileName := DraftFile(DocumentKey);
  if DocumentFileExists(FileName) and not DeleteDocumentFile(FileName) then
    raise Exception.Create('Could not remove saved AutoSave draft');
  FlushDirectory(FRoot);
end;

procedure TAutoSaveStore.ListDrafts(Files: TStrings);
var
  StoredFiles: TStoreFileInfoArray;
  I: Integer;
begin
  Files.Clear;
  StoredFiles := StoreFiles(FRoot + '*.draft');
  for I := 0 to High(StoredFiles) do
    if not StoredFiles[I].IsDirectory then
      Files.Add(string(StoredFiles[I].Name));
end;

procedure TAutoSaveStore.ListPriorVersions(const DocumentKey: string;
  Files: TStrings);
var
  StoredFiles: TStoreFileInfoArray;
  I: Integer;
begin
  if not SafeKey(DocumentKey) then
    raise Exception.Create('Invalid AutoSave document key');
  Files.Clear;
  StoredFiles := StoreFiles(FRoot + DocumentKey + '.*.version');
  for I := 0 to High(StoredFiles) do
    if not StoredFiles[I].IsDirectory then
      Files.Add(string(StoredFiles[I].Name));
end;

function TAutoSaveStore.LoadRecord(const FileName: TDocumentPath;
  out Entry: TAutoSaveStoredRecord): Boolean;
var
  Stream: TStream;
  FullName: TDocumentPath;
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
      FullName := FullStoreFileName(FRoot, FileName);
      Stream := OpenStoreFile(FullName, False);
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

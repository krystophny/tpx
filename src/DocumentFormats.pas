unit DocumentFormats;

{$IFDEF FPC}{$MODE OBJFPC}{$H+}{$ENDIF}

interface

uses SysUtils, Classes;

type
  { All file paths crossing the document service boundary are UTF-8. }
  TDocumentPath = {$IFDEF FPC}UTF8String{$ELSE}string{$ENDIF};

  TDiskRevision = record
    ContentDigest: string;
    Size: Int64;
    ModifiedUTC: Int64;
    Identity: string;
    Exists: Boolean;
  end;

  TDocumentSnapshot = record
    SourceBytes: RawByteString;
    Revision: TDiskRevision;
  end;

  TDocumentFormat = record
    Id: string;
    DisplayName: string;
    Extensions: string;
    CanOpen: Boolean;
    CanSaveBack: Boolean;
    RoundTripProfile: string;
    RuntimeRequirements: string;
  end;

  TExportFormat = record
    Id: string;
    DisplayName: string;
    Extension: string;
    Available: Boolean;
    Kind: Integer;
  end;

  TDocumentSession = class
  private
    fLocalRevision: QWord;
    fWatchGeneration: QWord;
    fCodecContext: TObject;
  public
    SourcePath: TDocumentPath;
    RecoveryBackupPath: TDocumentPath;
    SourceFormatId: string;
    OriginalSourceBytes: RawByteString;
    AcceptedRevision: TDiskRevision;
    CanSaveBack: Boolean;
    Diagnostics: TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AcceptSource(const APath: TDocumentPath;
      const AFormatId: string;
      const ABytes: RawByteString; const ARevision: TDiskRevision;
      const ASaveBack: Boolean; ACodecContext: TObject = nil);
    { Accepts a path already normalized before the live scene commit. }
    procedure AcceptSourceNormalized(const APath: TDocumentPath;
      const AFormatId: string;
      const ABytes: RawByteString; const ARevision: TDiskRevision;
      const ASaveBack: Boolean; ACodecContext: TObject = nil);
    procedure AcceptExternalRevision(const ABytes: RawByteString;
      const ARevision: TDiskRevision; ACodecContext: TObject = nil);
    procedure AcceptSavedRevision(const ABytes: RawByteString;
      const ARevision: TDiskRevision; ACodecContext: TObject = nil;
      const ABackupPath: TDocumentPath = '');
    procedure RebindSavedSource(const APath: TDocumentPath;
      const AFormatId: string;
      const ABytes: RawByteString; const ARevision: TDiskRevision;
      const ASaveBack: Boolean; ACodecContext: TObject;
      const ABackupPath: TDocumentPath = '');
    procedure RebindSavedSourceNormalized(const APath: TDocumentPath;
      const AFormatId: string;
      const ABytes: RawByteString; const ARevision: TDiskRevision;
      const ASaveBack: Boolean; ACodecContext: TObject;
      const ABackupPath: TDocumentPath = '');
    procedure AdvanceLocalRevision;
    function NextWatchGeneration: QWord;
    function IsAcceptedRevision(const Revision: TDiskRevision): Boolean;
    property LocalRevision: QWord read fLocalRevision;
    property WatchGeneration: QWord read fWatchGeneration;
    property CodecContext: TObject read fCodecContext;
  end;

  EDocumentConflict = class(Exception);

const
  MaxDocumentBytes = 268435456;

function SameDiskRevision(const A, B: TDiskRevision): Boolean;
function DiskRevisionKey(const Revision: TDiskRevision): string;
function NormalizeDocumentPath(const FileName: TDocumentPath): TDocumentPath;
function SameDocumentPath(const A, B: TDocumentPath): Boolean;
function DocumentPathDirectory(const FileName: TDocumentPath): TDocumentPath;
function DocumentPathFileName(const FileName: TDocumentPath): TDocumentPath;
function DocumentPathExtension(const FileName: TDocumentPath): string;
function DocumentPathWithExtension(const FileName: TDocumentPath;
  const Extension: string): TDocumentPath;
function EnsureDocumentDirectoryExists(
  const Directory: TDocumentPath): Boolean;
function DocumentFileExists(const FileName: TDocumentPath): Boolean;
function DeleteDocumentFile(const FileName: TDocumentPath): Boolean;
function CreateDocumentStagingDirectory(
  const DestinationDirectory: TDocumentPath): TDocumentPath;
function RemoveDocumentStagingDirectory(
  const Directory: TDocumentPath): Boolean;
procedure WriteDocumentStagingFile(const FileName: TDocumentPath;
  const Bytes: RawByteString);
function ReadDocumentSnapshot(const FileName: TDocumentPath): TDocumentSnapshot;
function ReadDocumentRevision(const FileName: TDocumentPath): TDiskRevision;
procedure WriteDocumentAtomically(const FileName: TDocumentPath;
  const Bytes: RawByteString; const ExpectedRevision: TDiskRevision;
  const CheckExpected: Boolean; out NewRevision: TDiskRevision;
  out BackupFileName: TDocumentPath);
procedure WriteDocumentBundleAtomically(const FileName: TDocumentPath;
  const Bytes: RawByteString; const ExpectedRevision: TDiskRevision;
  const CheckExpected: Boolean; StagedAssets, FinalAssets: TStrings;
  out NewRevision: TDiskRevision; out BackupFileName: TDocumentPath);

function DocumentFormatCount: Integer;
function DocumentFormatAt(const Index: Integer): TDocumentFormat;
procedure RegisterDocumentFormat(const Format: TDocumentFormat);
function FindDocumentFormatById(const Id: string;
  out Format: TDocumentFormat): Boolean;
function DetectDocumentFormat(const FileName: TDocumentPath;
  const SourceBytes: RawByteString; out Format: TDocumentFormat): Boolean;
function BuildOpenFileFilter(const IncludeExternalFormats: Boolean): string;

function ExportFormatCount: Integer;
function ExportFormatAt(const Index: Integer): TExportFormat;
function DocumentSaveFormatCount: Integer;
function DocumentFormatForSaveFilterIndex(const FilterIndex: Integer;
  out Format: TDocumentFormat): Boolean;
function ExportFormatForFilterIndex(const FilterIndex: Integer;
  out Format: TExportFormat): Boolean;
function IsExportFormatAvailable(const Kind: Integer): Boolean;
function BuildSaveFileFilter: string;

implementation

uses md5
{$IFDEF FPC}
  {$IFDEF UNIX}, BaseUnix{$ENDIF}
  {$IFDEF WINDOWS}, Windows{$ENDIF}
{$ENDIF};

type
  TFileInfo = record
    Identity: string;
    Size: Int64;
    ModifiedUTC: Int64;
  end;

  TStagedAsset = record
    StagePath: TDocumentPath;
    FinalPath: TDocumentPath;
    BackupPath: TDocumentPath;
    StageRevision: TDiskRevision;
    OriginalRevision: TDiskRevision;
    HadOriginal: Boolean;
    BackupMoved: Boolean;
    Published: Boolean;
  end;

const
  FileIdentityUnavailable = 'path-identity';

{$IFDEF FPC}{$IFDEF UNIX}
function c_fsync(const FileDescriptor: LongInt): LongInt; cdecl;
  external 'c' name 'fsync';
{$ENDIF}{$ENDIF}

function FileInfoFromHandle(const Handle: THandle;
  const FileName: TDocumentPath): TFileInfo;
{$IFDEF FPC}
{$IFDEF UNIX}
var Info: BaseUnix.Stat;
{$ENDIF}
{$IFDEF WINDOWS}
var Info: TByHandleFileInformation;
{$ENDIF}
{$ENDIF}
begin
  Result.Identity := FileIdentityUnavailable + ':' +
    string(NormalizeDocumentPath(FileName));
  Result.Size := -1;
  Result.ModifiedUTC := 0;
{$IFDEF FPC}
{$IFDEF UNIX}
  if fpFStat(Handle, Info) = 0 then
  begin
    Result.Identity := IntToStr(Info.st_dev) + ':' + IntToStr(Info.st_ino);
    Result.Size := Info.st_size;
    Result.ModifiedUTC := Info.st_mtime;
  end;
{$ENDIF}
{$IFDEF WINDOWS}
  if GetFileInformationByHandle(Handle, Info) then
  begin
    Result.Identity := IntToHex(Info.dwVolumeSerialNumber, 8) + ':' +
      IntToHex(Info.nFileIndexHigh, 8) + IntToHex(Info.nFileIndexLow, 8);
    Result.Size := Int64(Info.nFileSizeHigh) shl 32 or Info.nFileSizeLow;
    Result.ModifiedUTC := Int64(Info.ftLastWriteTime.dwHighDateTime) shl 32 or
      Info.ftLastWriteTime.dwLowDateTime;
  end;
{$ENDIF}
{$ENDIF}
end;

var
  RegisteredFormats: array of TDocumentFormat;
{$IFDEF FPC}{$IFDEF WINDOWS}
  TempFileSequence: LongInt;
{$ENDIF}{$ENDIF}

{$IFDEF FPC}{$IFDEF WINDOWS}
function c_GetFullPathNameW(FileName: PWideChar; BufferLength: Cardinal;
  Buffer: PWideChar; FilePart: Pointer): Cardinal; stdcall;
  external 'kernel32' name 'GetFullPathNameW';

function WidePath(const FileName: TDocumentPath): UnicodeString;
begin
  Result := UTF8Decode(FileName);
end;

function ExtendedWidePath(const FileName: TDocumentPath): UnicodeString;
begin
  Result := WidePath(FileName);
  if Length(Result) < MAX_PATH then Exit;
  if Copy(Result, 1, 4) = '\\?\' then Exit;
  if Copy(Result, 1, 2) = '\\' then
    Result := '\\?\UNC\' + Copy(Result, 3, MaxInt)
  else
    Result := '\\?\' + Result;
end;

function OpenDocumentHandle(const FileName: TDocumentPath;
  const Access: DWORD): THandle;
var Name: UnicodeString;
begin
  Name := ExtendedWidePath(FileName);
  Result := Windows.CreateFileW(PWideChar(Name), Access,
    FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
end;

{$ENDIF}{$ENDIF}

function NormalizeDocumentPath(const FileName: TDocumentPath): TDocumentPath;
{$IFDEF FPC}{$IFDEF WINDOWS}
var WideInput, WideOutput: UnicodeString; Required, Written: Cardinal;
{$ENDIF}{$ENDIF}
begin
  if FileName = '' then Exit('');
{$IFDEF FPC}{$IFDEF WINDOWS}
  WideInput := UTF8Decode(FileName);
  Required := c_GetFullPathNameW(PWideChar(WideInput), 0, nil, nil);
  if Required = 0 then
    raise Exception.CreateFmt('Could not normalize document path: %s',
      [string(FileName)]);
  SetLength(WideOutput, Required);
  Written := c_GetFullPathNameW(PWideChar(WideInput), Required,
    PWideChar(WideOutput), nil);
  if (Written = 0) or (Written >= Required) then
    raise Exception.CreateFmt('Could not normalize document path: %s',
      [string(FileName)]);
  SetLength(WideOutput, Written);
  Result := UTF8Encode(WideOutput);
{$ELSE}
  Result := UTF8String(ExpandFileName(string(FileName)));
{$ENDIF}{$ELSE}
  Result := ExpandFileName(FileName);
{$ENDIF}
  while (Length(Result) > 1) and (Result[Length(Result)] = PathDelim) do
  begin
    if (Length(Result) = 3) and (Result[2] = ':') then Break;
    if (Length(Result) = 2) and (Result[1] = PathDelim) then Break;
    Delete(Result, Length(Result), 1);
  end;
end;

function SameDocumentPath(const A, B: TDocumentPath): Boolean;
var LeftPath, RightPath: TDocumentPath;
begin
  LeftPath := NormalizeDocumentPath(A);
  RightPath := NormalizeDocumentPath(B);
{$IFDEF WINDOWS}
  Result := SameText(LeftPath, RightPath);
{$ELSE}
  Result := LeftPath = RightPath;
{$ENDIF}
end;

function DocumentPathFileName(const FileName: TDocumentPath): TDocumentPath;
var I: Integer;
begin
  for I := Length(FileName) downto 1 do
    if (FileName[I] = '/') or (FileName[I] = '\') then
      Exit(Copy(FileName, I + 1, MaxInt));
  Result := FileName;
end;

function DocumentPathDirectory(const FileName: TDocumentPath): TDocumentPath;
var I: Integer;
begin
  for I := Length(FileName) downto 1 do
    if (FileName[I] = '/') or (FileName[I] = '\') then
    begin
      if I = 1 then Result := Copy(FileName, 1, 1)
      else Result := Copy(FileName, 1, I - 1);
      Exit;
    end;
  Result := '.';
end;

function DocumentPathExtension(const FileName: TDocumentPath): string;
var I, Dot: Integer;
begin
  Dot := 0;
  for I := Length(FileName) downto 1 do
  begin
    if (FileName[I] = '/') or (FileName[I] = '\') then Break;
    if FileName[I] = '.' then
    begin
      Dot := I;
      Break;
    end;
  end;
  if Dot = 0 then Result := ''
  else Result := LowerCase(Copy(FileName, Dot, MaxInt));
end;

function DocumentPathWithExtension(const FileName: TDocumentPath;
  const Extension: string): TDocumentPath;
var I, Dot: Integer;
begin
  Dot := 0;
  for I := Length(FileName) downto 1 do
  begin
    if (FileName[I] = '/') or (FileName[I] = '\') then Break;
    if FileName[I] = '.' then
    begin
      Dot := I;
      Break;
    end;
  end;
  if Dot = 0 then Result := FileName + Extension
  else Result := Copy(FileName, 1, Dot - 1) + Extension;
end;

function DocumentDirectoryExists(const Directory: TDocumentPath): Boolean;
{$IFDEF FPC}{$IFDEF WINDOWS}
var Name: UnicodeString; Attr: DWORD;
{$ENDIF}{$ENDIF}
begin
{$IFDEF FPC}{$IFDEF WINDOWS}
  Name := ExtendedWidePath(Directory);
  Attr := Windows.GetFileAttributesW(PWideChar(Name));
  Result := (Attr <> INVALID_FILE_ATTRIBUTES) and
    ((Attr and FILE_ATTRIBUTE_DIRECTORY) <> 0);
{$ELSE}
  Result := DirectoryExists(Directory);
{$ENDIF}{$ELSE}
  Result := DirectoryExists(Directory);
{$ENDIF}
end;

function EnsureDocumentDirectoryExists(const Directory: TDocumentPath): Boolean;
var Normalized, Parent: TDocumentPath;
{$IFDEF FPC}{$IFDEF WINDOWS}
  WideName: UnicodeString;
{$ENDIF}{$ENDIF}
begin
  Normalized := NormalizeDocumentPath(Directory);
  if DocumentDirectoryExists(Normalized) then Exit(True);
  Parent := DocumentPathDirectory(Normalized);
  if (Parent = Normalized) or (Parent = '') or
    not EnsureDocumentDirectoryExists(Parent) then Exit(False);
{$IFDEF FPC}{$IFDEF WINDOWS}
  WideName := ExtendedWidePath(Normalized);
  Result := Windows.CreateDirectoryW(PWideChar(WideName), nil) or
    DocumentDirectoryExists(Normalized);
{$ELSE}{$IFDEF UNIX}
  Result := (fpMkDir(PChar(Normalized), 493) = 0) or
    DocumentDirectoryExists(Normalized);
{$ELSE}
  Result := CreateDir(Normalized) or DocumentDirectoryExists(Normalized);
{$ENDIF}{$ENDIF}{$ELSE}
  Result := ForceDirectories(Normalized) or DocumentDirectoryExists(Normalized);
{$ENDIF}
end;

function DocumentFileExists(const FileName: TDocumentPath): Boolean;
{$IFDEF FPC}{$IFDEF WINDOWS}
var Name: UnicodeString; Attr, ErrorCode: DWORD;
{$ENDIF}{$ENDIF}
begin
{$IFDEF FPC}{$IFDEF WINDOWS}
  Name := ExtendedWidePath(FileName);
  Attr := Windows.GetFileAttributesW(PWideChar(Name));
  if Attr <> INVALID_FILE_ATTRIBUTES then Exit(True);
  ErrorCode := GetLastError;
  Result := (ErrorCode <> ERROR_FILE_NOT_FOUND) and
    (ErrorCode <> ERROR_PATH_NOT_FOUND);
{$ELSE}
  Result := FileExists(FileName);
{$ENDIF}{$ELSE}
  Result := FileExists(FileName);
{$ENDIF}
end;

function FileInfoFromPath(const FileName: TDocumentPath): TFileInfo;
{$IFDEF FPC}{$IFDEF WINDOWS}
var Handle: THandle;
{$ELSE}
var Stream: TFileStream;
{$ENDIF}{$ENDIF}
{$IFNDEF FPC}
var Stream: TFileStream;
{$ENDIF}
begin
{$IFDEF FPC}{$IFDEF WINDOWS}
  Handle := OpenDocumentHandle(FileName, FILE_READ_ATTRIBUTES);
  if Handle = INVALID_HANDLE_VALUE then RaiseLastOSError;
  try
    Result := FileInfoFromHandle(Handle, FileName);
  finally
    Windows.CloseHandle(Handle);
  end;
{$ELSE}
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result := FileInfoFromHandle(Stream.Handle, FileName);
  finally
    Stream.Free;
  end;
{$ENDIF}{$ELSE}
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result := FileInfoFromHandle(Stream.Handle, FileName);
  finally
    Stream.Free;
  end;
{$ENDIF}
end;

function TempFileInDirectory(const Directory: TDocumentPath;
  const Prefix: string): TDocumentPath;
{$IFDEF FPC}{$IFDEF WINDOWS}
var WideDirectory, WideName: UnicodeString; Handle: THandle;
  Attempt: Integer; Sequence: LongInt;
{$ENDIF}{$ENDIF}
begin
{$IFDEF FPC}{$IFDEF WINDOWS}
  WideDirectory := ExtendedWidePath(Directory);
  if (Length(WideDirectory) > 0) and
    not (WideDirectory[Length(WideDirectory)] in ['\', '/']) then
    WideDirectory := WideDirectory + PathDelim;
  for Attempt := 1 to 128 do
  begin
    Inc(TempFileSequence);
    Sequence := TempFileSequence;
    WideName := WideDirectory + UTF8Decode(Format('%s-%x-%x.tmp', [Prefix,
      Windows.GetCurrentProcessId, Windows.GetTickCount xor DWORD(Sequence)]));
    Handle := Windows.CreateFileW(PWideChar(WideName), GENERIC_WRITE, 0, nil,
      CREATE_NEW, FILE_ATTRIBUTE_TEMPORARY, 0);
    if Handle <> INVALID_HANDLE_VALUE then
    begin
      Windows.CloseHandle(Handle);
      Result := UTF8Encode(WideName);
      Exit;
    end;
    if GetLastError <> ERROR_FILE_EXISTS then Exit('');
  end;
  Result := '';
{$ELSE}
  Result := SysUtils.GetTempFileName(Directory, Prefix);
{$ENDIF}{$ELSE}
  Result := SysUtils.GetTempFileName(Directory, Prefix);
{$ENDIF}
end;

function DeleteDocumentFile(const FileName: TDocumentPath): Boolean;
{$IFDEF FPC}{$IFDEF WINDOWS}
var WideName: UnicodeString;
{$ENDIF}{$ENDIF}
begin
{$IFDEF FPC}{$IFDEF WINDOWS}
  WideName := ExtendedWidePath(FileName);
  Result := Windows.DeleteFileW(PWideChar(WideName));
{$ELSE}
  Result := SysUtils.DeleteFile(FileName);
{$ENDIF}{$ELSE}
  Result := SysUtils.DeleteFile(FileName);
{$ENDIF}
end;

function CreateDocumentStagingDirectory(
  const DestinationDirectory: TDocumentPath): TDocumentPath;
var Candidate: TDocumentPath; Attempt: Integer;
{$IFDEF FPC}{$IFDEF WINDOWS}
var WideName: UnicodeString;
{$ENDIF}{$ENDIF}
begin
  Result := '';
  if not DocumentDirectoryExists(DestinationDirectory) then Exit;
  for Attempt := 1 to 64 do
  begin
    Candidate := TempFileInDirectory(DestinationDirectory, 'tpx-stage');
    if Candidate = '' then Exit;
    DeleteDocumentFile(Candidate);
{$IFDEF FPC}{$IFDEF WINDOWS}
    WideName := ExtendedWidePath(Candidate);
    if Windows.CreateDirectoryW(PWideChar(WideName), nil) then
    begin
      Result := Candidate;
      Exit;
    end;
    if GetLastError <> ERROR_ALREADY_EXISTS then Exit;
{$ELSE}{$IFDEF UNIX}
    if fpMkDir(PChar(Candidate), 448) = 0 then
    begin
      Result := Candidate;
      Exit;
    end;
{$ELSE}
    if CreateDir(Candidate) then
    begin
      Result := Candidate;
      Exit;
    end;
{$ENDIF}{$ENDIF}{$ELSE}
    if CreateDir(Candidate) then
    begin
      Result := Candidate;
      Exit;
    end;
{$ENDIF}
  end;
end;

function RemoveDocumentStagingDirectory(
  const Directory: TDocumentPath): Boolean;
{$IFDEF FPC}{$IFDEF WINDOWS}
var WideName: UnicodeString;
{$ENDIF}{$ENDIF}
begin
{$IFDEF FPC}{$IFDEF WINDOWS}
  WideName := ExtendedWidePath(Directory);
  Result := Windows.RemoveDirectoryW(PWideChar(WideName));
{$ELSE}
{$IFDEF FPC}{$IFDEF UNIX}
  Result := fpRmDir(PChar(Directory)) = 0;
{$ELSE}
  Result := RemoveDir(Directory);
{$ENDIF}{$ENDIF}
{$ENDIF}{$ELSE}
  Result := RemoveDir(Directory);
{$ENDIF}
end;

function SameFileInfo(const A, B: TFileInfo): Boolean;
begin
  Result := (A.Identity = B.Identity) and (A.Size = B.Size) and
    (A.ModifiedUTC = B.ModifiedUTC);
end;

function SameDiskRevision(const A, B: TDiskRevision): Boolean;
begin
  Result := (A.Exists = B.Exists) and (A.ContentDigest = B.ContentDigest) and
    (A.Size = B.Size) and (A.ModifiedUTC = B.ModifiedUTC) and
    (A.Identity = B.Identity);
end;

function DiskRevisionKey(const Revision: TDiskRevision): string;
begin
  Result := IntToStr(Ord(Revision.Exists)) + ':' +
    IntToStr(Length(Revision.ContentDigest)) + ':' + Revision.ContentDigest +
    IntToStr(Length(Revision.Identity)) + ':' + Revision.Identity + ':' +
    IntToStr(Revision.Size) + ':' + IntToStr(Revision.ModifiedUTC);
end;

function IsSymbolicLink(const FileName: TDocumentPath): Boolean;
{$IFDEF FPC}{$IFDEF UNIX}
var Info: BaseUnix.Stat;
{$ENDIF}{$ENDIF}
{$IFDEF FPC}{$IFDEF WINDOWS}
var Attributes: DWORD; Name: UnicodeString;
{$ENDIF}{$ENDIF}
begin
  Result := False;
{$IFDEF FPC}{$IFDEF UNIX}
  if fpLStat(PChar(FileName), Info) = 0 then
    Result := FPS_ISLNK(Info.st_mode);
{$ENDIF}{$ENDIF}
{$IFDEF FPC}{$IFDEF WINDOWS}
  Name := ExtendedWidePath(FileName);
  Attributes := Windows.GetFileAttributesW(PWideChar(Name));
  Result := (Attributes <> INVALID_FILE_ATTRIBUTES) and
    ((Attributes and FILE_ATTRIBUTE_REPARSE_POINT) <> 0);
{$ENDIF}{$ENDIF}
end;

function ReadDocumentSnapshot(const FileName: TDocumentPath): TDocumentSnapshot;
var BeforeInfo, AfterInfo, PathInfo: TFileInfo; ReadSize: Int64;
  Path: TDocumentPath;
{$IFDEF FPC}{$IFDEF WINDOWS}
  Handle: THandle; BytesRead, SizeHigh, SizeLow, ErrorCode: DWORD;
  Offset, Chunk: LongInt;
{$ELSE}
  Stream: TFileStream;
{$ENDIF}{$ENDIF}
{$IFNDEF FPC}
  Stream: TFileStream;
{$ENDIF}
begin
  Path := NormalizeDocumentPath(FileName);
  if IsSymbolicLink(Path) then
    raise EReadError.Create('Symbolic-link document paths are not supported');
  Result.SourceBytes := '';
  Result.Revision.Exists := False;
  Result.Revision.Size := 0;
  Result.Revision.ModifiedUTC := 0;
  Result.Revision.Identity := '';
  Result.Revision.ContentDigest := '';
{$IFDEF FPC}{$IFDEF WINDOWS}
  Handle := OpenDocumentHandle(Path, GENERIC_READ);
  if Handle = INVALID_HANDLE_VALUE then RaiseLastOSError;
  try
    BeforeInfo := FileInfoFromHandle(Handle, Path);
    SetLastError(0);
    SizeLow := Windows.GetFileSize(Handle, @SizeHigh);
    ErrorCode := GetLastError;
    if (SizeLow = INVALID_FILE_SIZE) and (ErrorCode <> 0) then
      RaiseLastOSError;
    ReadSize := (Int64(SizeHigh) shl 32) or SizeLow;
    if (ReadSize < 0) or (ReadSize > MaxDocumentBytes) then
      raise EReadError.CreateFmt('Document exceeds the %d byte limit',
        [MaxDocumentBytes]);
    SetLength(Result.SourceBytes, ReadSize);
    Offset := 0;
    while Offset < Length(Result.SourceBytes) do
    begin
      Chunk := Length(Result.SourceBytes) - Offset;
      if not Windows.ReadFile(Handle, Result.SourceBytes[Offset + 1], Chunk,
        BytesRead, nil) then RaiseLastOSError;
      if BytesRead <> DWORD(Chunk) then
        raise EReadError.Create('Document ended while it was being read');
      Inc(Offset, BytesRead);
    end;
    AfterInfo := FileInfoFromHandle(Handle, Path);
  finally
    Windows.CloseHandle(Handle);
  end;
{$ELSE}
  Stream := TFileStream.Create(Path, fmOpenRead or fmShareDenyNone);
  try
    BeforeInfo := FileInfoFromHandle(Stream.Handle, Path);
    ReadSize := Stream.Size;
    if (ReadSize < 0) or (ReadSize > MaxDocumentBytes) then
      raise EReadError.CreateFmt('Document exceeds the %d byte limit',
        [MaxDocumentBytes]);
    SetLength(Result.SourceBytes, ReadSize);
    Stream.Position := 0;
    if ReadSize > 0 then Stream.ReadBuffer(Result.SourceBytes[1], ReadSize);
    AfterInfo := FileInfoFromHandle(Stream.Handle, Path);
  finally
    Stream.Free;
  end;
{$ENDIF}{$ELSE}
  Stream := TFileStream.Create(Path, fmOpenRead or fmShareDenyNone);
  try
    BeforeInfo := FileInfoFromHandle(Stream.Handle, Path);
    ReadSize := Stream.Size;
    if (ReadSize < 0) or (ReadSize > MaxDocumentBytes) then
      raise EReadError.CreateFmt('Document exceeds the %d byte limit',
        [MaxDocumentBytes]);
    SetLength(Result.SourceBytes, ReadSize);
    Stream.Position := 0;
    if ReadSize > 0 then Stream.ReadBuffer(Result.SourceBytes[1], ReadSize);
    AfterInfo := FileInfoFromHandle(Stream.Handle, Path);
  finally
    Stream.Free;
  end;
{$ENDIF}
  if IsSymbolicLink(Path) then
    raise EReadError.Create('Symbolic-link document paths are not supported');
  PathInfo := FileInfoFromPath(Path);
  if not SameFileInfo(BeforeInfo, AfterInfo) or
    not SameFileInfo(AfterInfo, PathInfo) or
    (Length(Result.SourceBytes) <> AfterInfo.Size) then
    raise EReadError.Create('Document changed while it was being read');
  Result.Revision.Exists := True;
  Result.Revision.ContentDigest := MD5Print(MD5String(Result.SourceBytes));
  Result.Revision.Size := Length(Result.SourceBytes);
  Result.Revision.ModifiedUTC := AfterInfo.ModifiedUTC;
  Result.Revision.Identity := AfterInfo.Identity;
end;

function ReadDocumentRevision(const FileName: TDocumentPath): TDiskRevision;
var Snapshot: TDocumentSnapshot; Path: TDocumentPath;
begin
  Path := NormalizeDocumentPath(FileName);
  if not DocumentFileExists(Path) then
  begin
    Result.ContentDigest := '';
    Result.Size := 0;
    Result.ModifiedUTC := 0;
    Result.Identity := '';
    Result.Exists := False;
    Exit;
  end;
  Snapshot := ReadDocumentSnapshot(Path);
  Result := Snapshot.Revision;
end;

constructor TDocumentSession.Create;
begin
  inherited Create;
  Diagnostics := TStringList.Create;
  Clear;
end;

destructor TDocumentSession.Destroy;
begin
  try
    FreeAndNil(fCodecContext);
  except
    { Context destruction must not prevent releasing the session itself. }
  end;
  Diagnostics.Free;
  inherited Destroy;
end;

procedure TDocumentSession.Clear;
begin
  try
    FreeAndNil(fCodecContext);
  except
  end;
  SourcePath := '';
  RecoveryBackupPath := '';
  SourceFormatId := '';
  OriginalSourceBytes := '';
  AcceptedRevision.ContentDigest := '';
  AcceptedRevision.Size := 0;
  AcceptedRevision.ModifiedUTC := 0;
  AcceptedRevision.Identity := '';
  AcceptedRevision.Exists := False;
  CanSaveBack := False;
  Diagnostics.Clear;
  Inc(fLocalRevision);
  Inc(fWatchGeneration);
end;

procedure TDocumentSession.AcceptSource(const APath: TDocumentPath;
  const AFormatId: string;
  const ABytes: RawByteString; const ARevision: TDiskRevision;
  const ASaveBack: Boolean; ACodecContext: TObject);
var NormalizedPath: TDocumentPath;
begin
  NormalizedPath := NormalizeDocumentPath(APath);
  AcceptSourceNormalized(NormalizedPath, AFormatId, ABytes, ARevision,
    ASaveBack, ACodecContext);
end;

procedure TDocumentSession.AcceptSourceNormalized(const APath: TDocumentPath;
  const AFormatId: string;
  const ABytes: RawByteString; const ARevision: TDiskRevision;
  const ASaveBack: Boolean; ACodecContext: TObject);
var OldContext: TObject;
begin
  OldContext := fCodecContext;
  SourcePath := APath;
  RecoveryBackupPath := '';
  SourceFormatId := AFormatId;
  OriginalSourceBytes := ABytes;
  AcceptedRevision := ARevision;
  CanSaveBack := ASaveBack;
  fCodecContext := ACodecContext;
  if (OldContext <> nil) and (OldContext <> ACodecContext) then
    try OldContext.Free except end;
  Diagnostics.Clear;
  fLocalRevision := 0;
end;

procedure TDocumentSession.AcceptExternalRevision(
  const ABytes: RawByteString; const ARevision: TDiskRevision;
  ACodecContext: TObject);
var OldContext: TObject;
begin
  OldContext := fCodecContext;
  OriginalSourceBytes := ABytes;
  AcceptedRevision := ARevision;
  fCodecContext := ACodecContext;
  Inc(fLocalRevision);
  Diagnostics.Clear;
  if (OldContext <> nil) and (OldContext <> ACodecContext) then
    try OldContext.Free except end;
end;

procedure TDocumentSession.AcceptSavedRevision(const ABytes: RawByteString;
  const ARevision: TDiskRevision; ACodecContext: TObject;
  const ABackupPath: TDocumentPath);
var OldContext: TObject;
begin
  OldContext := fCodecContext;
  OriginalSourceBytes := ABytes;
  AcceptedRevision := ARevision;
  CanSaveBack := True;
  fCodecContext := ACodecContext;
  RecoveryBackupPath := ABackupPath;
  Diagnostics.Clear;
  if (OldContext <> nil) and (OldContext <> ACodecContext) then
    try OldContext.Free except end;
end;

procedure TDocumentSession.RebindSavedSource(const APath: TDocumentPath;
  const AFormatId: string;
  const ABytes: RawByteString; const ARevision: TDiskRevision;
  const ASaveBack: Boolean; ACodecContext: TObject;
  const ABackupPath: TDocumentPath);
var NormalizedPath: TDocumentPath;
begin
  NormalizedPath := NormalizeDocumentPath(APath);
  RebindSavedSourceNormalized(NormalizedPath, AFormatId, ABytes, ARevision,
    ASaveBack, ACodecContext, ABackupPath);
end;

procedure TDocumentSession.RebindSavedSourceNormalized(
  const APath: TDocumentPath; const AFormatId: string;
  const ABytes: RawByteString; const ARevision: TDiskRevision;
  const ASaveBack: Boolean; ACodecContext: TObject;
  const ABackupPath: TDocumentPath);
var OldContext: TObject;
begin
  OldContext := fCodecContext;
  SourcePath := APath;
  RecoveryBackupPath := ABackupPath;
  SourceFormatId := AFormatId;
  OriginalSourceBytes := ABytes;
  AcceptedRevision := ARevision;
  CanSaveBack := ASaveBack;
  fCodecContext := ACodecContext;
  if (OldContext <> nil) and (OldContext <> ACodecContext) then
    try OldContext.Free except end;
  Diagnostics.Clear;
end;

procedure TDocumentSession.AdvanceLocalRevision;
begin
  Inc(fLocalRevision);
end;

function TDocumentSession.NextWatchGeneration: QWord;
begin
  Inc(fWatchGeneration);
  Result := fWatchGeneration;
end;

function TDocumentSession.IsAcceptedRevision(
  const Revision: TDiskRevision): Boolean;
begin
  Result := SameDiskRevision(AcceptedRevision, Revision);
end;

procedure WriteSynchronizedFile(const FileName: TDocumentPath;
  const Bytes: RawByteString);
{$IFDEF FPC}{$IFDEF WINDOWS}
var Handle: THandle; Offset, Chunk: LongInt; BytesWritten: DWORD;
{$ELSE}
{$IFDEF UNIX}
var FileDescriptor, Offset, Written: LongInt;
{$ELSE}
var Stream: TFileStream;
{$ENDIF}
{$ENDIF}{$ENDIF}
{$IFNDEF FPC}
var Stream: TFileStream;
{$ENDIF}
begin
{$IFDEF FPC}{$IFDEF WINDOWS}
  Handle := Windows.CreateFileW(PWideChar(ExtendedWidePath(FileName)), GENERIC_WRITE,
    FILE_SHARE_READ, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  if Handle = INVALID_HANDLE_VALUE then RaiseLastOSError;
  try
    Offset := 0;
    while Offset < Length(Bytes) do
    begin
      Chunk := Length(Bytes) - Offset;
      if not Windows.WriteFile(Handle, Bytes[Offset + 1], Chunk,
        BytesWritten, nil) then RaiseLastOSError;
      if BytesWritten = 0 then
        raise EWriteError.Create('Could not write staged document');
      Inc(Offset, BytesWritten);
    end;
    if not Windows.FlushFileBuffers(Handle) then RaiseLastOSError;
  finally
    Windows.CloseHandle(Handle);
  end;
{$ELSE}
{$IFDEF UNIX}
  FileDescriptor := fpOpen(PChar(FileName), O_WRONLY or O_CREAT or O_EXCL,
    384);
  if FileDescriptor < 0 then RaiseLastOSError;
  try
    Offset := 0;
    while Offset < Length(Bytes) do
    begin
      Written := fpWrite(FileDescriptor, @Bytes[Offset + 1],
        Length(Bytes) - Offset);
      if Written <= 0 then
        raise EWriteError.Create('Could not write staged document');
      Inc(Offset, Written);
    end;
    if c_fsync(FileDescriptor) <> 0 then
      raise EWriteError.Create('Could not flush staged document to disk');
  finally
    fpClose(FileDescriptor);
  end;
{$ELSE}
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    if Length(Bytes) > 0 then
      Stream.WriteBuffer(Bytes[1], Length(Bytes));
{$IFDEF FPC}
{$IFDEF UNIX}
    if c_fsync(Stream.Handle) <> 0 then
      raise EWriteError.Create('Could not flush staged document to disk');
{$ENDIF}
{$IFDEF WINDOWS}
    if not FlushFileBuffers(Stream.Handle) then
      RaiseLastOSError;
{$ENDIF}
{$ENDIF}
  finally
    Stream.Free;
  end;
{$ENDIF}
{$ENDIF}{$ELSE}
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    if Length(Bytes) > 0 then
      Stream.WriteBuffer(Bytes[1], Length(Bytes));
    Stream.Free;
  except
    Stream.Free;
    raise;
  end;
{$ENDIF}
end;

procedure WriteDocumentStagingFile(const FileName: TDocumentPath;
  const Bytes: RawByteString);
var Path: TDocumentPath;
begin
  Path := NormalizeDocumentPath(FileName);
  if DocumentFileExists(Path) or IsSymbolicLink(Path) then
    raise EWriteError.Create('A staging file already exists');
  WriteSynchronizedFile(Path, Bytes);
end;

function ReplaceFileAtomically(const SourceFile,
  DestFile: TDocumentPath): Boolean;
begin
{$IFDEF FPC}
{$IFDEF UNIX}
  Result := fpRename(PChar(SourceFile), PChar(DestFile)) = 0;
{$ELSE}
{$IFDEF WINDOWS}
  Result := Windows.MoveFileExW(PWideChar(ExtendedWidePath(SourceFile)),
    PWideChar(ExtendedWidePath(DestFile)), MOVEFILE_REPLACE_EXISTING);
{$ELSE}
  Result := RenameFile(SourceFile, DestFile);
{$ENDIF}
{$ENDIF}
{$ELSE}
  Result := RenameFile(SourceFile, DestFile);
{$ENDIF}
end;

procedure PreserveExistingPermissions(const ExistingFile,
  StageFile: TDocumentPath);
{$IFDEF FPC}{$IFDEF UNIX}
var Info: BaseUnix.Stat;
{$ENDIF}{$ENDIF}
begin
{$IFDEF FPC}{$IFDEF UNIX}
  if fpStat(PChar(ExistingFile), Info) <> 0 then RaiseLastOSError;
  if fpChmod(PChar(StageFile), Info.st_mode and $0FFF) <> 0 then
    RaiseLastOSError;
{$ENDIF}{$ENDIF}
end;

procedure WriteDocumentAtomically(const FileName: TDocumentPath;
  const Bytes: RawByteString; const ExpectedRevision: TDiskRevision;
  const CheckExpected: Boolean; out NewRevision: TDiskRevision;
  out BackupFileName: TDocumentPath);
var Dest, DestDir, StageFile, ExistingBackup: TDocumentPath;
  Existing: TDocumentSnapshot; StageRevision: TDiskRevision;
  HasExisting, StageCreated,
  BackupCreated, ReplaceSucceeded: Boolean;
begin
  if Length(Bytes) > MaxDocumentBytes then
    raise EWriteError.CreateFmt('Document exceeds the %d byte limit',
      [MaxDocumentBytes]);
  Dest := NormalizeDocumentPath(FileName);
  if IsSymbolicLink(Dest) then
    raise EWriteError.Create('Saving through a symbolic link is not supported');
  DestDir := DocumentPathDirectory(Dest);
  if (DestDir = '') or not DocumentDirectoryExists(DestDir) then
    raise EWriteError.Create('Destination directory does not exist');
  StageFile := TempFileInDirectory(DestDir, 'tpx');
  StageCreated := StageFile <> '';
  if not StageCreated then
    raise EWriteError.Create('Could not create a staging file');
  ExistingBackup := '';
  BackupCreated := False;
  ReplaceSucceeded := False;
  BackupFileName := '';
  try
    WriteSynchronizedFile(StageFile, Bytes);
    HasExisting := DocumentFileExists(Dest);
    if HasExisting then Existing := ReadDocumentSnapshot(Dest)
    else Existing.SourceBytes := '';
    if CheckExpected then
    begin
      if HasExisting then
      begin
        if not SameDiskRevision(Existing.Revision, ExpectedRevision) then
          raise EDocumentConflict.Create('The destination changed on disk');
      end
      else if ExpectedRevision.Exists then
        raise EDocumentConflict.Create('The destination was removed on disk');
    end;
    if HasExisting then PreserveExistingPermissions(Dest, StageFile);
    { Capture the identity and digest of our exact output before publishing.
      A destination read after rename could observe a later external writer. }
    StageRevision := ReadDocumentRevision(StageFile);
    if HasExisting then
    begin
      ExistingBackup := TempFileInDirectory(DestDir, 'bak');
      if ExistingBackup = '' then
        raise EWriteError.Create('Could not create a recoverable backup');
      BackupCreated := True;
      WriteSynchronizedFile(ExistingBackup, Existing.SourceBytes);
    end;
    { Recheck immediately before replace. This detects observed conflicts but
      cannot provide compare-and-swap against uncooperative writers. }
    if HasExisting then
    begin
      if not SameDiskRevision(ReadDocumentRevision(Dest), Existing.Revision) then
        raise EDocumentConflict.Create('The destination changed before replace');
    end
    else if DocumentFileExists(Dest) then
      raise EDocumentConflict.Create('The destination appeared before replace');
    if not ReplaceFileAtomically(StageFile, Dest) then
      raise EWriteError.CreateFmt('Could not replace %s', [Dest]);
    ReplaceSucceeded := True;
    StageCreated := False;
    NewRevision := StageRevision;
    if HasExisting then
    begin
      BackupFileName := ExistingBackup;
      BackupCreated := False;
    end;
  finally
    if StageCreated then DeleteDocumentFile(StageFile);
    if BackupCreated and not ReplaceSucceeded then
      DeleteDocumentFile(ExistingBackup);
  end;
end;

procedure WriteDocumentBundleAtomically(const FileName: TDocumentPath;
  const Bytes: RawByteString; const ExpectedRevision: TDiskRevision;
  const CheckExpected: Boolean; StagedAssets, FinalAssets: TStrings;
  out NewRevision: TDiskRevision; out BackupFileName: TDocumentPath);
var
  Dest: TDocumentPath;
  Assets: array of TStagedAsset;
  I, J, AssetCount: Integer;
  Committed, RollbackFailed: Boolean;

  procedure RollBackPublishedAssets;
  var K: Integer; CurrentRevision: TDiskRevision;
  begin
    for K := High(Assets) downto 0 do
      if Assets[K].BackupMoved or Assets[K].Published then
      try
        if Assets[K].Published then
        begin
          CurrentRevision := ReadDocumentRevision(Assets[K].FinalPath);
          if not SameDiskRevision(CurrentRevision,
            Assets[K].StageRevision) then
            raise EDocumentConflict.CreateFmt(
              'Asset changed during rollback: %s', [Assets[K].FinalPath]);
        end
        else if DocumentFileExists(Assets[K].FinalPath) then
          raise EDocumentConflict.CreateFmt(
            'Asset appeared during rollback: %s', [Assets[K].FinalPath]);
        if Assets[K].BackupMoved then
        begin
          if not ReplaceFileAtomically(Assets[K].BackupPath,
            Assets[K].FinalPath) then
            raise EWriteError.CreateFmt('Could not restore asset %s',
              [Assets[K].FinalPath]);
          Assets[K].BackupMoved := False;
        end
        else if not DeleteDocumentFile(Assets[K].FinalPath) then
          raise EWriteError.CreateFmt('Could not remove staged asset %s',
            [Assets[K].FinalPath]);
        Assets[K].Published := False;
      except
        on E: Exception do RollbackFailed := True;
      end;
  end;

begin
  if (StagedAssets = nil) <> (FinalAssets = nil) then
    raise EWriteError.Create('Staged and final asset lists must be paired');
  AssetCount := 0;
  if StagedAssets <> nil then
  begin
    if StagedAssets.Count <> FinalAssets.Count then
      raise EWriteError.Create('Staged and final asset lists differ in size');
    AssetCount := StagedAssets.Count;
  end;
  SetLength(Assets, AssetCount);
  Dest := NormalizeDocumentPath(FileName);
  for I := 0 to AssetCount - 1 do
  begin
    Assets[I].StagePath := NormalizeDocumentPath(StagedAssets[I]);
    Assets[I].FinalPath := NormalizeDocumentPath(FinalAssets[I]);
    Assets[I].BackupPath := '';
    Assets[I].HadOriginal := False;
    Assets[I].BackupMoved := False;
    Assets[I].Published := False;
    if SameDocumentPath(Assets[I].StagePath, Assets[I].FinalPath) or
      SameDocumentPath(Assets[I].FinalPath, Dest) then
      raise EWriteError.Create('An asset path overlaps another output');
    if IsSymbolicLink(Assets[I].StagePath) or
      IsSymbolicLink(Assets[I].FinalPath) then
      raise EWriteError.Create('Symbolic-link assets are not supported');
    Assets[I].HadOriginal := DocumentFileExists(Assets[I].FinalPath);
    if Assets[I].HadOriginal then
      PreserveExistingPermissions(Assets[I].FinalPath, Assets[I].StagePath);
    Assets[I].StageRevision := ReadDocumentRevision(Assets[I].StagePath);
    for J := 0 to I - 1 do
      if SameDocumentPath(Assets[J].FinalPath, Assets[I].FinalPath) then
        raise EWriteError.Create('An asset destination is duplicated');
    if Assets[I].HadOriginal then
      Assets[I].OriginalRevision := ReadDocumentRevision(Assets[I].FinalPath);
  end;

  Committed := False;
  RollbackFailed := False;
  BackupFileName := '';
  try
    try
      for I := 0 to AssetCount - 1 do
      begin
        if not EnsureDocumentDirectoryExists(
          DocumentPathDirectory(Assets[I].FinalPath)) then
          raise EWriteError.CreateFmt('Asset directory does not exist: %s',
            [Assets[I].FinalPath]);
        if Assets[I].HadOriginal then
        begin
          if not SameDiskRevision(ReadDocumentRevision(Assets[I].FinalPath),
            Assets[I].OriginalRevision) then
            raise EDocumentConflict.CreateFmt('Asset changed on disk: %s',
              [Assets[I].FinalPath]);
          Assets[I].BackupPath := TempFileInDirectory(
            DocumentPathDirectory(Assets[I].FinalPath), 'asset');
          if Assets[I].BackupPath = '' then
            raise EWriteError.Create('Could not create an asset rollback file');
          if not ReplaceFileAtomically(Assets[I].FinalPath,
            Assets[I].BackupPath) then
            raise EWriteError.CreateFmt('Could not stage existing asset %s',
              [Assets[I].FinalPath]);
          Assets[I].BackupMoved := True;
        end
        else if DocumentFileExists(Assets[I].FinalPath) then
          raise EDocumentConflict.CreateFmt('Asset appeared on disk: %s',
            [Assets[I].FinalPath]);
        if not ReplaceFileAtomically(Assets[I].StagePath,
          Assets[I].FinalPath) then
          raise EWriteError.CreateFmt('Could not publish asset %s',
            [Assets[I].FinalPath]);
        Assets[I].Published := True;
      end;
      WriteDocumentAtomically(Dest, Bytes, ExpectedRevision, CheckExpected,
        NewRevision, BackupFileName);
      Committed := True;
    except
      on E: Exception do
      begin
        RollBackPublishedAssets;
        if RollbackFailed then
          raise EWriteError.CreateFmt('%s; one or more assets could not be '
            + 'rolled back safely', [E.Message]);
        raise;
      end;
    end;
  finally
    for I := 0 to AssetCount - 1 do
      if (Assets[I].BackupPath <> '') and
        (not Assets[I].BackupMoved) then
        DeleteDocumentFile(Assets[I].BackupPath);
  end;

  if Committed then
    for I := 0 to AssetCount - 1 do
      if Assets[I].BackupMoved then
      begin
        DeleteDocumentFile(Assets[I].BackupPath);
        Assets[I].BackupMoved := False;
      end;
end;

function DocumentFormatCount: Integer;
begin
  Result := Length(RegisteredFormats);
end;

function DocumentFormatAt(const Index: Integer): TDocumentFormat;
begin
  Result.Id := '';
  Result.DisplayName := '';
  Result.Extensions := '';
  Result.CanOpen := False;
  Result.CanSaveBack := False;
  Result.RoundTripProfile := '';
  Result.RuntimeRequirements := '';
  if (Index >= 0) and (Index < Length(RegisteredFormats)) then
    Result := RegisteredFormats[Index];
end;

procedure RegisterDocumentFormat(const Format: TDocumentFormat);
var I, N: Integer;
begin
  if Format.Id = '' then
    raise Exception.Create('A document format requires a stable ID');
  for I := 0 to High(RegisteredFormats) do
    if SameText(RegisteredFormats[I].Id, Format.Id) then
    begin
      RegisteredFormats[I] := Format;
      Exit;
    end;
  N := Length(RegisteredFormats);
  SetLength(RegisteredFormats, N + 1);
  RegisteredFormats[N] := Format;
end;

function FindDocumentFormatById(const Id: string;
  out Format: TDocumentFormat): Boolean;
var I: Integer;
begin
  Result := False;
  Format.Id := '';
  for I := 0 to DocumentFormatCount - 1 do
  begin
    Format := DocumentFormatAt(I);
    if SameText(Format.Id, Id) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function DetectDocumentFormat(const FileName: TDocumentPath;
  const SourceBytes: RawByteString; out Format: TDocumentFormat): Boolean;
var Ext: string; I: Integer;
begin
  Result := False;
  Format.Id := '';
  Ext := DocumentPathExtension(FileName);
  for I := 0 to DocumentFormatCount - 1 do
  begin
    Format := DocumentFormatAt(I);
    if (Ext <> '') and (Pos(';' + Ext + ';',
      ';' + LowerCase(StringReplace(Format.Extensions, ',', ';',
        [rfReplaceAll])) + ';') > 0) then
    begin
      Result := Format.CanOpen;
      Exit;
    end;
  end;
  if Pos('%<TpX', SourceBytes) > 0 then
  begin
    Format := DocumentFormatAt(0);
    Result := True;
  end;
end;

function BuildOpenFileFilter(const IncludeExternalFormats: Boolean): string;
var I, Count: Integer; F: TDocumentFormat; AllPattern, Pattern: string;
  function Visible: Boolean;
  begin
    Result := F.CanOpen and (F.Extensions <> '') and
      ((F.RuntimeRequirements = '') or IncludeExternalFormats);
  end;
begin
  AllPattern := '';
  Count := 0;
  for I := 0 to DocumentFormatCount - 1 do
  begin
    F := DocumentFormatAt(I);
    if not Visible then Continue;
    Pattern := F.Extensions;
    if Pattern = '' then Continue;
    Pattern := StringReplace(Pattern, ';', ';*', [rfReplaceAll]);
    if (Count > 0) then AllPattern := AllPattern + ';';
    AllPattern := AllPattern + '*' + Pattern;
    Inc(Count);
  end;
  if Count = 0 then Result := ''
  else Result := 'All supported formats|' + AllPattern;
  for I := 0 to DocumentFormatCount - 1 do
  begin
    F := DocumentFormatAt(I);
    if not Visible then Continue;
    Result := Result + '|' + F.DisplayName + ' (' + F.Extensions + ')|';
    Pattern := StringReplace(F.Extensions, ';', ';*', [rfReplaceAll]);
    Result := Result + '*' + Pattern;
  end;
end;

function ExportFormatCount: Integer;
begin
  Result := 14;
end;

function IsExportFormatAvailable(const Kind: Integer): Boolean;
begin
  case Kind of
    0, 2, 5, 6, 7, 8, 9, 10, 11, 12, 13: Result := True;
{$IFDEF VER140}
    1, 3, 4: Result := True;
{$ELSE}
    1, 3, 4: Result := False;
{$ENDIF}
  else
    Result := False;
  end;
end;

function ExportFormatAt(const Index: Integer): TExportFormat;
const
  Names: array[0..13] of string = (
    'Scalable Vector Graphics (SVG)', 'Enhanced Metafile (EMF)',
    'Encapsulated PostScript (EPS)', 'Portable Network Graphics (PNG)',
    'Windows bitmap (BMP)', 'Portable document format (PDF)',
    'MetaPost (.mp)', 'MetaPost EPS output (.mps)', 'PDF from EPS',
    'LaTeX EPS (latex-dvips)', 'PDF from LaTeX EPS',
    'LaTeX custom', 'LaTeX preview source', 'PdfLaTeX preview source');
  Exts: array[0..13] of string = ('svg', 'emf', 'eps', 'png', 'bmp',
    'pdf', 'mp', 'mps', 'pdf', 'eps', 'pdf', '*', 'tex', 'tex');
begin
  Result.Id := '';
  Result.DisplayName := '';
  Result.Extension := '';
  Result.Available := False;
  Result.Kind := Index;
  if (Index < 0) or (Index >= ExportFormatCount) then Exit;
  Result.Id := 'export-' + IntToStr(Index);
  Result.DisplayName := Names[Index];
  Result.Extension := Exts[Index];
  Result.Available := IsExportFormatAvailable(Index);
end;

function DocumentSaveFormatCount: Integer;
var I: Integer; F: TDocumentFormat;
begin
  Result := 0;
  for I := 0 to DocumentFormatCount - 1 do
  begin
    F := DocumentFormatAt(I);
    if F.CanSaveBack and (F.Extensions <> '') then Inc(Result);
  end;
end;

function DocumentFormatForSaveFilterIndex(const FilterIndex: Integer;
  out Format: TDocumentFormat): Boolean;
var I, VisibleIndex: Integer; F: TDocumentFormat;
begin
  Result := False;
  Format.Id := '';
  VisibleIndex := 1;
  for I := 0 to DocumentFormatCount - 1 do
  begin
    F := DocumentFormatAt(I);
    if not F.CanSaveBack or (F.Extensions = '') then Continue;
    if VisibleIndex = FilterIndex then
    begin
      Format := F;
      Result := True;
      Exit;
    end;
    Inc(VisibleIndex);
  end;
end;

function IsDocumentSaveExtension(const Extension: string): Boolean;
var I, StartAt, StopAt: Integer; F: TDocumentFormat; Exts: string;
begin
  Result := False;
  for I := 0 to DocumentFormatCount - 1 do
  begin
    F := DocumentFormatAt(I);
    if not F.CanSaveBack then Continue;
    Exts := StringReplace(F.Extensions, ',', ';', [rfReplaceAll]) + ';';
    StartAt := 1;
    repeat
      StopAt := Pos(';', Copy(Exts, StartAt, MaxInt));
      if StopAt = 0 then Break;
      StopAt := StartAt + StopAt - 1;
      if SameText(Trim(Copy(Exts, StartAt, StopAt - StartAt)),
        '.' + Extension) then Exit(True);
      StartAt := StopAt + 1;
    until StartAt > Length(Exts);
  end;
end;

function ExportFormatForFilterIndex(const FilterIndex: Integer;
  out Format: TExportFormat): Boolean;
var I, VisibleIndex: Integer;
begin
  Result := False;
  Format.Id := '';
  if FilterIndex <= DocumentSaveFormatCount then Exit;
  VisibleIndex := DocumentSaveFormatCount + 1;
  for I := 0 to ExportFormatCount - 1 do
    if IsExportFormatAvailable(I) and
      not IsDocumentSaveExtension(ExportFormatAt(I).Extension) then
    begin
      if VisibleIndex = FilterIndex then
      begin
        Format := ExportFormatAt(I);
        Result := True;
        Exit;
      end;
      Inc(VisibleIndex);
    end;
end;

function BuildSaveFileFilter: string;
var I: Integer; F: TExportFormat; D: TDocumentFormat; Pattern: string;
begin
  Result := '';
  for I := 0 to DocumentFormatCount - 1 do
  begin
    D := DocumentFormatAt(I);
    if not D.CanSaveBack or (D.Extensions = '') then Continue;
    Pattern := StringReplace(D.Extensions, ',', ';', [rfReplaceAll]);
    Pattern := StringReplace(Pattern, ';', ';*', [rfReplaceAll]);
    Pattern := '*' + Pattern;
    if Result <> '' then Result := Result + '|';
    Result := Result + D.DisplayName + ' (' + D.Extensions + ')|' + Pattern;
  end;
  for I := 0 to ExportFormatCount - 1 do
  begin
    F := ExportFormatAt(I);
    if not F.Available or IsDocumentSaveExtension(F.Extension) then Continue;
    if F.Extension = '*' then Pattern := '*.*'
    else Pattern := '*.' + F.Extension;
    Result := Result + '|' + F.DisplayName + '|' + Pattern;
  end;
end;

procedure RegisterBuiltInFormats;
var F: TDocumentFormat;
begin
  F.Id := 'tpx';
  F.DisplayName := 'TpX drawing';
  F.Extensions := '.tpx';
  F.CanOpen := True;
  F.CanSaveBack := True;
  F.RoundTripProfile :=
    'Native XML is authoritative; generated TeX tail is output only';
  F.RuntimeRequirements := '';
  RegisterDocumentFormat(F);
  F.Id := 'emf';
  F.DisplayName := 'Enhanced Metafile';
  F.Extensions := '.emf';
  F.CanOpen := True;
  F.CanSaveBack := False;
  F.RoundTripProfile := 'Import only';
  RegisterDocumentFormat(F);
  F.Id := 'wmf';
  F.DisplayName := 'Windows Metafile';
  F.Extensions := '.wmf';
  RegisterDocumentFormat(F);
end;

initialization
  RegisterBuiltInFormats;

end.

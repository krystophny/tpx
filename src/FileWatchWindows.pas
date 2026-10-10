unit FileWatchWindows;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileWatch;

type
  TWindowsNotifyRecord = record
    Action: Cardinal;
    Name: UnicodeString;
  end;
  TWindowsNotifyRecords = array of TWindowsNotifyRecord;

{ Parses every FILE_NOTIFY_INFORMATION record in one completed buffer. }
function TryParseWindowsNotifyBuffer(const Buffer; BufferLength: Cardinal;
  out Records: TWindowsNotifyRecords): Boolean;

{$IFDEF FILEWATCH_TESTS}
procedure InjectWindowsCompletionForTest(Source: TFileChangeSource;
  SubscriptionID: QWord; Success: Boolean; BytesTransferred, ErrorCode: Cardinal;
  const Buffer);
function WindowsWatchPathProbeCountForTest(Source: TFileChangeSource): Integer;
function WindowsWatchCompletionCountForTest(Source: TFileChangeSource): Integer;
function WindowsWatchCancellationCountsForTest(Source: TFileChangeSource;
  out Requests, Completions: Integer): Boolean;
{$ENDIF}

implementation

uses
  SyncObjs;

type
  TWinHandle = PtrUInt;
  TWinOverlapped = packed record
    Internal: PtrUInt;
    InternalHigh: PtrUInt;
    Offset: Cardinal;
    OffsetHigh: Cardinal;
    EventHandle: TWinHandle;
  end;
  PWinOverlapped = ^TWinOverlapped;

const
  NotifyBufferSize = 60 * 1024;

type
  TWindowsFileChangeSource = class;
  TWindowsCompletionThread = class;
  TWindowsDirectoryWatch = class;

  TWindowsSubscription = class
  public
    ID: QWord;
    Generation: QWord;
    Path: UTF8String;
    Directory: UTF8String;
    FileName: UnicodeString;
    Exists: Boolean;
    Watch: TWindowsDirectoryWatch;
  end;

  TWindowsDirectoryWatch = class
  public
    Directory: UTF8String;
    Handle: TWinHandle;
    Buffer: array[0..NotifyBufferSize - 1] of Byte;
    Overlapped: TWinOverlapped;
    Subscriptions: TList;
    Pending: Boolean;
    Retired: Boolean;
    CaseSensitive: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  TWindowsFileChangeSource = class(TFileChangeSource)
  private
    FLock: TCriticalSection;
    FPort: TWinHandle;
    FWorker: TWindowsCompletionThread;
    FWatchers: TList;
    FSubscriptions: TList;
    FPendingReads: Integer;
    FStopping: Boolean;
    FStopPacketSeen: Boolean;
    FStarted: Boolean;
    {$IFDEF FILEWATCH_TESTS}
    FPathProbeCount: Integer;
    FCompletionCount: Integer;
    FCancelRequestCount: Integer;
    FCancelledCompletionCount: Integer;
    {$ENDIF}
    function FindWatcher(const Directory: UTF8String): TWindowsDirectoryWatch;
    function FindSubscription(ID: QWord): TWindowsSubscription;
    function OpenWatcher(const Directory: UTF8String): TWindowsDirectoryWatch;
    function ArmRead(Watch: TWindowsDirectoryWatch;
      OverflowRetries: Integer = 0): Boolean;
    procedure PublishForSubscription(Subscription: TWindowsSubscription;
      Kind: TFileChangeKind; const ErrorText: string = '');
    procedure PublishRescan(Watch: TWindowsDirectoryWatch;
      const ErrorText: string = '');
    procedure ProcessRecords(Watch: TWindowsDirectoryWatch;
      const Records: TWindowsNotifyRecords);
    procedure ProcessRecord(Watch: TWindowsDirectoryWatch;
      const NotifyRecord: TWindowsNotifyRecord);
    procedure ProcessCompletion(Watch: TWindowsDirectoryWatch;
      Success: Boolean; BytesTransferred, ErrorCode: Cardinal);
    procedure ProcessNotificationPayload(Watch: TWindowsDirectoryWatch;
      Success: Boolean; BytesTransferred, ErrorCode: Cardinal;
      const Buffer);
    procedure RetireWatcher(Watch: TWindowsDirectoryWatch;
      ReportError: Cardinal = 0);
    procedure FreeWatcher(Watch: TWindowsDirectoryWatch);
    procedure MarkDegraded(const ErrorText: string);
    function IsPathPresent(const Path: UTF8String): Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
    function Subscribe(const Path: UTF8String; Generation: QWord;
      out SubscriptionID: QWord): TFileWatchStatus; override;
    procedure Unsubscribe(SubscriptionID: QWord); override;
  end;

  TWindowsCompletionThread = class(TThread)
  private
    FOwner: TWindowsFileChangeSource;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TWindowsFileChangeSource);
  end;

const
  InvalidHandle = PtrUInt(-1);
  FileListDirectory = $0001;
  FileShareRead = $00000001;
  FileShareWrite = $00000002;
  FileShareDelete = $00000004;
  OpenExisting = 3;
  FileFlagBackupSemantics = $02000000;
  FileFlagOverlapped = $40000000;
  FileNotifyChangeFileName = $00000001;
  FileNotifyChangeAttributes = $00000004;
  FileNotifyChangeSize = $00000008;
  FileNotifyChangeLastWrite = $00000010;
  NotifyFilter = FileNotifyChangeFileName or FileNotifyChangeAttributes or
    FileNotifyChangeSize or FileNotifyChangeLastWrite;
  ErrorNotifyEnumDir = 1022;
  ErrorNotFound = 1168;
  WaitInfinite = $FFFFFFFF;
  ErrorIoPending = 997;
  ErrorOperationAborted = 995;
  ErrorAbandonedWait0 = 735;
  FileCaseSensitiveInfoClass = 23;
  FileCsFlagCaseSensitiveDir = $00000001;
  FileActionAdded = 1;
  FileActionRemoved = 2;
  FileActionModified = 3;
  FileActionRenamedOldName = 4;
  FileActionRenamedNewName = 5;
  CompareStringEqual = 2;

type
  TWinBool = LongBool;
  TFileCaseSensitiveInfo = record
    Flags: Cardinal;
  end;

function WinCreateFileW(FileName: PWideChar; DesiredAccess, ShareMode: Cardinal;
  SecurityAttributes: Pointer; CreationDisposition, FlagsAndAttributes: Cardinal;
  TemplateFile: TWinHandle): TWinHandle; stdcall; external 'kernel32' name 'CreateFileW';
function WinCreateIoCompletionPort(FileHandle, ExistingCompletionPort: TWinHandle;
  CompletionKey: PtrUInt; NumberOfConcurrentThreads: Cardinal): TWinHandle;
  stdcall; external 'kernel32' name 'CreateIoCompletionPort';
function WinGetQueuedCompletionStatus(CompletionPort: TWinHandle;
  var BytesTransferred: Cardinal; var CompletionKey: PtrUInt;
  var Overlapped: PWinOverlapped; Milliseconds: Cardinal): TWinBool;
  stdcall; external 'kernel32' name 'GetQueuedCompletionStatus';
function WinPostQueuedCompletionStatus(CompletionPort: TWinHandle;
  BytesTransferred: Cardinal; CompletionKey: PtrUInt;
  Overlapped: PWinOverlapped): TWinBool; stdcall;
  external 'kernel32' name 'PostQueuedCompletionStatus';
function WinReadDirectoryChangesW(Directory: TWinHandle; Buffer: Pointer;
  BufferLength: Cardinal; WatchSubtree: TWinBool; NotifyFilter: Cardinal;
  BytesReturned: Pointer; Overlapped: PWinOverlapped;
  CompletionRoutine: Pointer): TWinBool; stdcall;
  external 'kernel32' name 'ReadDirectoryChangesW';
function WinCancelIoEx(FileHandle: TWinHandle;
  Overlapped: PWinOverlapped): TWinBool; stdcall;
  external 'kernel32' name 'CancelIoEx';
function WinCloseHandle(Handle: TWinHandle): TWinBool; stdcall;
  external 'kernel32' name 'CloseHandle';
function WinGetLastError: Cardinal; stdcall; external 'kernel32' name 'GetLastError';
function WinGetFileAttributesW(FileName: PWideChar): Cardinal; stdcall;
  external 'kernel32' name 'GetFileAttributesW';
function WinGetFileInformationByHandleEx(Handle: TWinHandle; InfoClass: Integer;
  Info: Pointer; BufferSize: Cardinal): TWinBool; stdcall;
  external 'kernel32' name 'GetFileInformationByHandleEx';
function WinCompareStringOrdinal(String1: PWideChar; Count1: Integer;
  String2: PWideChar; Count2: Integer; IgnoreCase: LongInt): Integer; stdcall;
  external 'kernel32' name 'CompareStringOrdinal';

function TryParseWindowsNotifyBuffer(const Buffer; BufferLength: Cardinal;
  out Records: TWindowsNotifyRecords): Boolean;
type
  PRecordHeader = ^TRecordHeader;
  TRecordHeader = packed record
    NextEntryOffset: Cardinal;
    Action: Cardinal;
    FileNameLength: Cardinal;
  end;
var
  Offset, Remaining, NameBytes, MinimumLength, NextOffset: Cardinal;
  Entry: PRecordHeader;
  RecordCount: Integer;
begin
  Records := nil;
  Result := False;
  if (BufferLength = 0) or (BufferLength > NotifyBufferSize) then Exit;
  Offset := 0;
  RecordCount := 0;
  while Offset < BufferLength do
  begin
    Remaining := BufferLength - Offset;
    if Remaining < SizeOf(TRecordHeader) then Exit;
    Entry := PRecordHeader(PByte(@Buffer) + Offset);
    NameBytes := Entry^.FileNameLength;
    if (NameBytes = 0) or (NameBytes mod SizeOf(WideChar) <> 0) or
       (NameBytes > Remaining - SizeOf(TRecordHeader)) then Exit;
    MinimumLength := SizeOf(TRecordHeader) + NameBytes;
    if Entry^.NextEntryOffset = 0 then
    begin
      if (MinimumLength > Remaining) then Exit;
      NextOffset := BufferLength;
    end
    else
    begin
      if (Entry^.NextEntryOffset mod SizeOf(Cardinal) <> 0) or
         (Entry^.NextEntryOffset < MinimumLength) or
         (Entry^.NextEntryOffset > Remaining) then Exit;
      NextOffset := Offset + Entry^.NextEntryOffset;
    end;
    SetLength(Records, RecordCount + 1);
    Records[RecordCount].Action := Entry^.Action;
    SetLength(Records[RecordCount].Name, NameBytes div SizeOf(WideChar));
    if NameBytes > 0 then
      Move(PByte(Entry)[SizeOf(TRecordHeader)], Records[RecordCount].Name[1],
        NameBytes);
    Inc(RecordCount);
    if Entry^.NextEntryOffset = 0 then
    begin
      { A final record may leave padding in the I/O buffer. }
      Result := True;
      Exit;
    end;
    Offset := NextOffset;
  end;
  Result := RecordCount > 0;
end;

constructor TWindowsDirectoryWatch.Create;
begin
  inherited Create;
  Handle := InvalidHandle;
  Subscriptions := TList.Create;
end;

destructor TWindowsDirectoryWatch.Destroy;
begin
  Subscriptions.Free;
  inherited Destroy;
end;

constructor TWindowsCompletionThread.Create(AOwner: TWindowsFileChangeSource);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FOwner := AOwner;
end;

constructor TWindowsFileChangeSource.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FWatchers := TList.Create;
  FSubscriptions := TList.Create;
  FPort := InvalidHandle;
end;

destructor TWindowsFileChangeSource.Destroy;
begin
  Stop;
  FSubscriptions.Free;
  FWatchers.Free;
  FLock.Free;
  inherited Destroy;
end;

function Utf8ToWide(const Value: UTF8String): UnicodeString;
begin
  Result := UTF8Decode(Value);
end;

function PathIsPresent(const Path: UTF8String): Boolean;
var
  WidePath: UnicodeString;
begin
  WidePath := Utf8ToWide(Path);
  Result := WinGetFileAttributesW(PWideChar(WidePath)) <> Cardinal(-1);
end;

function Utf8FileName(const Path: UTF8String): UTF8String;
var
  I: Integer;
begin
  for I := Length(Path) downto 1 do
    if Path[I] in ['\', '/'] then
      Exit(Copy(Path, I + 1, MaxInt));
  Result := Path;
end;

function Utf8DirectoryName(const Path: UTF8String): UTF8String;
var
  I: Integer;
begin
  for I := Length(Path) downto 1 do
    if Path[I] in ['\', '/'] then
    begin
      if (I = 1) or ((I = 3) and (Path[2] = ':')) then
        Result := Copy(Path, 1, I)
      else
        Result := Copy(Path, 1, I - 1);
      Exit;
    end;
  Result := '';
end;

function CompareFileName(const A, B: UnicodeString;
  CaseSensitive: Boolean): Integer;
begin
  { CompareStringOrdinal requires 0 or 1 here; LongBool(True) is -1. }
  Result := WinCompareStringOrdinal(PWideChar(A), Length(A), PWideChar(B),
    Length(B), Ord(not CaseSensitive));
end;

function WindowsErrorText(ErrorCode: Cardinal): string;
var
  MessageText: string;
begin
  MessageText := SysErrorMessage(ErrorCode);
  if MessageText <> '' then
    Result := Format('Windows error %d: %s', [ErrorCode, Trim(MessageText)])
  else
    Result := Format('Windows error %d', [ErrorCode]);
end;

procedure TWindowsFileChangeSource.MarkDegraded(const ErrorText: string);
begin
  SetStatus(fwsDegraded);
  PublishEvent(MakeEvent(0, 0, '', fckBackendError, fwsDegraded, ErrorText));
end;

procedure TWindowsFileChangeSource.Start;
var
  ErrorCode: Cardinal;
begin
  FLock.Acquire;
  try
    if FStarted then Exit;
    FStopping := False;
    FStopPacketSeen := False;
    FPendingReads := 0;
    SetStatus(fwsStarting);
    FPort := WinCreateIoCompletionPort(InvalidHandle, 0, 0, 1);
    if FPort = 0 then
    begin
      ErrorCode := WinGetLastError;
      FPort := InvalidHandle;
      SetStatus(fwsError);
      PublishEvent(MakeEvent(0, 0, '', fckBackendError, fwsError,
        'Could not create Windows completion port: ' +
        WindowsErrorText(ErrorCode)));
      Exit;
    end;
    try
      FWorker := TWindowsCompletionThread.Create(Self);
      FWorker.Start;
      FStarted := True;
      SetStatus(fwsReady);
      PublishEvent(MakeEvent(0, 0, '', fckReady, fwsReady));
    except
      on E: Exception do
      begin
        ErrorCode := WinGetLastError;
        if FPort <> InvalidHandle then WinCloseHandle(FPort);
        FPort := InvalidHandle;
        FreeAndNil(FWorker);
        SetStatus(fwsError);
        PublishEvent(MakeEvent(0, 0, '', fckBackendError, fwsError,
          'Could not start Windows file watcher: ' + E.Message + ' ' +
          WindowsErrorText(ErrorCode)));
      end;
    end;
  finally
    FLock.Release;
  end;
end;

function TWindowsFileChangeSource.FindWatcher(
  const Directory: UTF8String): TWindowsDirectoryWatch;
var
  I: Integer;
  Candidate: TWindowsDirectoryWatch;
begin
  Result := nil;
  for I := 0 to FWatchers.Count - 1 do
  begin
    Candidate := TWindowsDirectoryWatch(FWatchers[I]);
    if (not Candidate.Retired) and (Candidate.Directory = Directory) then
      Exit(Candidate);
  end;
end;

function TWindowsFileChangeSource.FindSubscription(
  ID: QWord): TWindowsSubscription;
var
  I: Integer;
  Candidate: TWindowsSubscription;
begin
  Result := nil;
  for I := 0 to FSubscriptions.Count - 1 do
  begin
    Candidate := TWindowsSubscription(FSubscriptions[I]);
    if Candidate.ID = ID then Exit(Candidate);
  end;
end;

function TWindowsFileChangeSource.OpenWatcher(
  const Directory: UTF8String): TWindowsDirectoryWatch;
var
  WideDirectory: UnicodeString;
  CaseInfo: TFileCaseSensitiveInfo;
  ErrorCode: Cardinal;
begin
  Result := nil;
  WideDirectory := Utf8ToWide(Directory);
  Result := TWindowsDirectoryWatch.Create;
  Result.Directory := Directory;
  Result.Handle := WinCreateFileW(PWideChar(WideDirectory), FileListDirectory,
    FileShareRead or FileShareWrite or FileShareDelete, nil, OpenExisting,
    FileFlagBackupSemantics or FileFlagOverlapped, 0);
  if Result.Handle = InvalidHandle then
  begin
    ErrorCode := WinGetLastError;
    FreeAndNil(Result);
    MarkDegraded('Could not watch directory: ' + WindowsErrorText(ErrorCode));
    Exit;
  end;
  if WinCreateIoCompletionPort(Result.Handle, FPort, PtrUInt(Result), 0) = 0 then
  begin
    ErrorCode := WinGetLastError;
    WinCloseHandle(Result.Handle);
    Result.Handle := InvalidHandle;
    FreeAndNil(Result);
    MarkDegraded('Could not associate directory with completion port: ' +
      WindowsErrorText(ErrorCode));
    Exit;
  end;
  FillChar(CaseInfo, SizeOf(CaseInfo), 0);
  Result.CaseSensitive := WinGetFileInformationByHandleEx(Result.Handle,
    FileCaseSensitiveInfoClass, @CaseInfo, SizeOf(CaseInfo)) and
    ((CaseInfo.Flags and FileCsFlagCaseSensitiveDir) <> 0);
  FWatchers.Add(Result);
end;

function TWindowsFileChangeSource.ArmRead(Watch: TWindowsDirectoryWatch;
  OverflowRetries: Integer): Boolean;
var
  ErrorCode: Cardinal;
begin
  Result := False;
  if FStopping or Watch.Retired or Watch.Pending then Exit;
  FillChar(Watch.Overlapped, SizeOf(Watch.Overlapped), 0);
  Watch.Pending := True;
  Inc(FPendingReads);
  if WinReadDirectoryChangesW(Watch.Handle, @Watch.Buffer[0],
    SizeOf(Watch.Buffer), False, NotifyFilter, nil, @Watch.Overlapped, nil) then
    Exit(True);
  ErrorCode := WinGetLastError;
  if ErrorCode = ErrorIoPending then Exit(True);
  Watch.Pending := False;
  Dec(FPendingReads);
  if ErrorCode = ErrorNotifyEnumDir then
  begin
    PublishRescan(Watch, 'Windows discarded directory notifications');
    if OverflowRetries < 1 then Exit(ArmRead(Watch, OverflowRetries + 1));
  end;
  RetireWatcher(Watch, ErrorCode);
end;

procedure TWindowsFileChangeSource.PublishForSubscription(
  Subscription: TWindowsSubscription; Kind: TFileChangeKind;
  const ErrorText: string);
begin
  PublishEvent(MakeEvent(Subscription.ID, Subscription.Generation,
    Subscription.Path, Kind, Status, ErrorText));
end;

procedure TWindowsFileChangeSource.PublishRescan(
  Watch: TWindowsDirectoryWatch; const ErrorText: string);
begin
  if Watch.Subscriptions.Count = 0 then Exit;
  PublishEvent(MakeEvent(0, 0, '', fckRescanRequired, Status, ErrorText));
end;

function TWindowsFileChangeSource.IsPathPresent(const Path: UTF8String): Boolean;
begin
  {$IFDEF FILEWATCH_TESTS}
  Inc(FPathProbeCount);
  {$ENDIF}
  Result := PathIsPresent(Path);
end;

procedure TWindowsFileChangeSource.ProcessRecord(Watch: TWindowsDirectoryWatch;
  const NotifyRecord: TWindowsNotifyRecord);
var
  I: Integer;
  Subscription: TWindowsSubscription;
  Present: Boolean;
  Kind: TFileChangeKind;
  CompareResult: Integer;
begin
  for I := 0 to Watch.Subscriptions.Count - 1 do
  begin
    Subscription := TWindowsSubscription(Watch.Subscriptions[I]);
    CompareResult := CompareFileName(NotifyRecord.Name, Subscription.FileName,
      Watch.CaseSensitive);
    if CompareResult = 0 then
    begin
      PublishRescan(Watch, 'Windows could not compare a notification filename');
      Continue;
    end;
    if CompareResult <> CompareStringEqual then Continue;
    if not (NotifyRecord.Action in [FileActionAdded, FileActionRemoved,
      FileActionModified, FileActionRenamedOldName,
      FileActionRenamedNewName]) then
    begin
      PublishRescan(Watch, 'Windows returned an unknown file notification action');
      Continue;
    end;
    Present := IsPathPresent(Subscription.Path);
    if not Present then
    begin
      if Subscription.Exists then
      begin
        Subscription.Exists := False;
        PublishForSubscription(Subscription, fckDisappeared);
      end;
      Continue;
    end;
    if not Subscription.Exists then
    begin
      Subscription.Exists := True;
      PublishForSubscription(Subscription, fckReappeared);
      Continue;
    end;
    case NotifyRecord.Action of
      FileActionAdded, FileActionRemoved, FileActionRenamedOldName,
      FileActionRenamedNewName:
        Kind := fckReplaced;
      FileActionModified:
        Kind := fckContentChanged;
    else
      Kind := fckMetadataChanged;
    end;
    PublishForSubscription(Subscription, Kind);
  end;
end;

procedure TWindowsFileChangeSource.ProcessRecords(Watch: TWindowsDirectoryWatch;
  const Records: TWindowsNotifyRecords);
var
  I: Integer;
begin
  for I := 0 to Length(Records) - 1 do
    ProcessRecord(Watch, Records[I]);
end;

procedure TWindowsFileChangeSource.ProcessNotificationPayload(
  Watch: TWindowsDirectoryWatch; Success: Boolean;
  BytesTransferred, ErrorCode: Cardinal; const Buffer);
var
  Records: TWindowsNotifyRecords;
begin
  if (not Success) or (BytesTransferred = 0) then
  begin
    if Success or (ErrorCode = ErrorNotifyEnumDir) then
      PublishRescan(Watch, 'Windows directory notification detail was lost')
    else
      RetireWatcher(Watch, ErrorCode);
    Exit;
  end;
  if not TryParseWindowsNotifyBuffer(Buffer, BytesTransferred, Records) then
  begin
    PublishRescan(Watch, 'Windows returned malformed directory notifications');
    Exit;
  end;
  ProcessRecords(Watch, Records);
end;

procedure TWindowsFileChangeSource.ProcessCompletion(
  Watch: TWindowsDirectoryWatch; Success: Boolean;
  BytesTransferred, ErrorCode: Cardinal);
begin
  FLock.Acquire;
  try
    {$IFDEF FILEWATCH_TESTS}
    Inc(FCompletionCount);
    {$ENDIF}
    if not Watch.Pending then Exit;
    Watch.Pending := False;
    Dec(FPendingReads);
    {$IFDEF FILEWATCH_TESTS}
    if (not Success) and (ErrorCode = ErrorOperationAborted) then
      Inc(FCancelledCompletionCount);
    {$ENDIF}
    if FStopping or Watch.Retired then
    begin
      if not Watch.Pending then FreeWatcher(Watch);
      Exit;
    end;
    if (not Success) and (ErrorCode = ErrorOperationAborted) then
    begin
      RetireWatcher(Watch, ErrorCode);
      Exit;
    end;
    if (not Success) and (ErrorCode <> ErrorNotifyEnumDir) then
    begin
      RetireWatcher(Watch, ErrorCode);
      Exit;
    end;
    ProcessNotificationPayload(Watch, Success, BytesTransferred, ErrorCode,
      Watch.Buffer[0]);
    if not Watch.Retired then ArmRead(Watch);
  finally
    FLock.Release;
  end;
end;

procedure TWindowsFileChangeSource.FreeWatcher(Watch: TWindowsDirectoryWatch);
var
  I: Integer;
  Subscription: TWindowsSubscription;
begin
  if Watch = nil then Exit;
  if Watch.Pending then Exit;
  for I := 0 to Watch.Subscriptions.Count - 1 do
  begin
    Subscription := TWindowsSubscription(Watch.Subscriptions[I]);
    Subscription.Watch := nil;
  end;
  Watch.Subscriptions.Clear;
  FWatchers.Remove(Watch);
  if Watch.Handle <> InvalidHandle then
  begin
    WinCloseHandle(Watch.Handle);
    Watch.Handle := InvalidHandle;
  end;
  Watch.Free;
end;

procedure TWindowsFileChangeSource.RetireWatcher(
  Watch: TWindowsDirectoryWatch; ReportError: Cardinal);
var
  I: Integer;
  Subscription: TWindowsSubscription;
  ErrorText: string;
begin
  if Watch = nil then Exit;
  Watch.Retired := True;
  if ReportError <> 0 then
  begin
    ErrorText := WindowsErrorText(ReportError);
    SetStatus(fwsDegraded);
    for I := 0 to Watch.Subscriptions.Count - 1 do
    begin
      Subscription := TWindowsSubscription(Watch.Subscriptions[I]);
      PublishForSubscription(Subscription, fckBackendError, ErrorText);
    end;
  end;
  for I := 0 to Watch.Subscriptions.Count - 1 do
    TWindowsSubscription(Watch.Subscriptions[I]).Watch := nil;
  Watch.Subscriptions.Clear;
  if Watch.Pending then
  begin
    {$IFDEF FILEWATCH_TESTS}
    Inc(FCancelRequestCount);
    {$ENDIF}
    if not WinCancelIoEx(Watch.Handle, @Watch.Overlapped) then
    begin
      ReportError := WinGetLastError;
      if ReportError <> ErrorNotFound then
      begin
        SetStatus(fwsDegraded);
        ErrorText := 'Could not cancel Windows directory read: ' +
          WindowsErrorText(ReportError);
        PublishEvent(MakeEvent(0, 0, '', fckBackendError, fwsDegraded,
          ErrorText));
      end;
    end;
  end
  else
    FreeWatcher(Watch);
end;

function TWindowsFileChangeSource.Subscribe(const Path: UTF8String;
  Generation: QWord; out SubscriptionID: QWord): TFileWatchStatus;
var
  NormalizedPath, Directory: UTF8String;
  Watch: TWindowsDirectoryWatch;
  Subscription: TWindowsSubscription;
  IsNewWatch: Boolean;
begin
  SubscriptionID := 0;
  if Path = '' then Exit(fwsError);
  FLock.Acquire;
  try
    if not FStarted or FStopping then Exit(fwsStopped);
    NormalizedPath := NormalizeFileWatchPath(Path);
    Directory := Utf8DirectoryName(NormalizedPath);
    if (Directory = '') or (Utf8FileName(NormalizedPath) = '') then
      Exit(fwsError);
    Watch := FindWatcher(Directory);
    IsNewWatch := Watch = nil;
    if IsNewWatch then Watch := OpenWatcher(Directory);
    if Watch = nil then Exit(Status);

    Subscription := TWindowsSubscription.Create;
    Subscription.ID := AllocateSubscriptionID;
    Subscription.Generation := Generation;
    Subscription.Path := NormalizedPath;
    Subscription.Directory := Directory;
    Subscription.FileName := Utf8ToWide(Utf8FileName(NormalizedPath));
    Subscription.Exists := IsPathPresent(NormalizedPath);
    Subscription.Watch := Watch;
    FSubscriptions.Add(Subscription);
    Watch.Subscriptions.Add(Subscription);
    if IsNewWatch and not ArmRead(Watch) then
    begin
      SubscriptionID := Subscription.ID;
      Result := Status;
      Exit;
    end;
    SubscriptionID := Subscription.ID;
    PublishForSubscription(Subscription, fckReady);
    Result := Status;
  finally
    FLock.Release;
  end;
end;

procedure TWindowsFileChangeSource.Unsubscribe(SubscriptionID: QWord);
var
  Subscription: TWindowsSubscription;
  Watch: TWindowsDirectoryWatch;
begin
  if SubscriptionID = 0 then Exit;
  FLock.Acquire;
  try
    Subscription := FindSubscription(SubscriptionID);
    if Subscription = nil then Exit;
    Watch := Subscription.Watch;
    if Watch <> nil then Watch.Subscriptions.Remove(Subscription);
    FSubscriptions.Remove(Subscription);
    Subscription.Watch := nil;
    Subscription.Free;
    if (Watch <> nil) and (Watch.Subscriptions.Count = 0) then
      RetireWatcher(Watch);
  finally
    FLock.Release;
  end;
end;

procedure TWindowsFileChangeSource.Stop;
var
  I: Integer;
  Watch: TWindowsDirectoryWatch;
  Subscription: TWindowsSubscription;
  ErrorCode: Cardinal;
  Worker: TWindowsCompletionThread;
begin
  FLock.Acquire;
  try
    if not FStarted then
    begin
      if FPort <> InvalidHandle then WinCloseHandle(FPort);
      FPort := InvalidHandle;
      SetStatus(fwsStopped);
      Exit;
    end;
    FStopping := True;
    FStopPacketSeen := False;
    for I := 0 to FSubscriptions.Count - 1 do
    begin
      Subscription := TWindowsSubscription(FSubscriptions[I]);
      Subscription.Watch := nil;
      Subscription.Free;
    end;
    FSubscriptions.Clear;
    for I := FWatchers.Count - 1 downto 0 do
    begin
      Watch := TWindowsDirectoryWatch(FWatchers[I]);
      Watch.Subscriptions.Clear;
      Watch.Retired := True;
      if Watch.Pending then
      begin
        {$IFDEF FILEWATCH_TESTS}
        Inc(FCancelRequestCount);
        {$ENDIF}
        if not WinCancelIoEx(Watch.Handle, @Watch.Overlapped) then
        begin
          ErrorCode := WinGetLastError;
          if ErrorCode <> ErrorNotFound then
            PublishEvent(MakeEvent(0, 0, '', fckBackendError, fwsDegraded,
              'Could not cancel Windows directory read: ' +
              WindowsErrorText(ErrorCode)));
        end;
      end
      else
        FreeWatcher(Watch);
    end;
    if not WinPostQueuedCompletionStatus(FPort, 0, 0, nil) then
    begin
      ErrorCode := WinGetLastError;
      FStopPacketSeen := True;
      PublishEvent(MakeEvent(0, 0, '', fckBackendError, fwsDegraded,
        'Could not wake Windows watcher during shutdown: ' +
        WindowsErrorText(ErrorCode)));
      if FPendingReads = 0 then
      begin
        WinCloseHandle(FPort);
        FPort := InvalidHandle;
      end;
    end;
    Worker := FWorker;
  finally
    FLock.Release;
  end;

  if Worker <> nil then
  begin
    Worker.WaitFor;
    FLock.Acquire;
    try
      FreeAndNil(FWorker);
      FStarted := False;
      FStopping := False;
      FStopPacketSeen := False;
      if FPort <> InvalidHandle then WinCloseHandle(FPort);
      FPort := InvalidHandle;
      SetStatus(fwsStopped);
    finally
      FLock.Release;
    end;
  end;
end;

procedure TWindowsCompletionThread.Execute;
var
  BytesTransferred: Cardinal;
  CompletionKey: PtrUInt;
  Overlapped: PWinOverlapped;
  Success, ShouldExit: Boolean;
  ErrorCode: Cardinal;
  Watch: TWindowsDirectoryWatch;
begin
  while True do
  begin
    BytesTransferred := 0;
    CompletionKey := 0;
    Overlapped := nil;
    Success := WinGetQueuedCompletionStatus(FOwner.FPort,
      BytesTransferred, CompletionKey, Overlapped, WaitInfinite);
    ErrorCode := 0;
    if not Success then ErrorCode := WinGetLastError;
    if Overlapped = nil then
    begin
      FOwner.FLock.Acquire;
      try
        if (CompletionKey = 0) and Success then
          FOwner.FStopPacketSeen := True
        else if not Success then
        begin
          FOwner.SetStatus(fwsDegraded);
          FOwner.PublishEvent(FOwner.MakeEvent(0, 0, '', fckBackendError,
            fwsDegraded, 'Windows completion port failed: ' +
            WindowsErrorText(ErrorCode)));
          if (ErrorCode = ErrorAbandonedWait0) and FOwner.FStopping then
            FOwner.FStopPacketSeen := True;
        end;
        ShouldExit := FOwner.FStopping and FOwner.FStopPacketSeen and
          (FOwner.FPendingReads = 0);
      finally
        FOwner.FLock.Release;
      end;
      if ShouldExit then Break;
      Continue;
    end;
    Watch := TWindowsDirectoryWatch(CompletionKey);
    if (Watch = nil) or (@Watch.Overlapped <> Overlapped) then
    begin
      FOwner.FLock.Acquire;
      try
        FOwner.SetStatus(fwsDegraded);
        FOwner.PublishEvent(FOwner.MakeEvent(0, 0, '', fckBackendError,
          fwsDegraded, 'Windows returned an unknown directory completion'));
      finally
        FOwner.FLock.Release;
      end;
      Continue;
    end;
    FOwner.ProcessCompletion(Watch, Success, BytesTransferred, ErrorCode);
    FOwner.FLock.Acquire;
    try
      ShouldExit := FOwner.FStopping and FOwner.FStopPacketSeen and
        (FOwner.FPendingReads = 0);
    finally
      FOwner.FLock.Release;
    end;
    if ShouldExit then Break;
  end;
end;

function CreateWindowsFileChangeSource: TFileChangeSource;
begin
  Result := TWindowsFileChangeSource.Create;
end;

{$IFDEF FILEWATCH_TESTS}
procedure InjectWindowsCompletionForTest(Source: TFileChangeSource;
  SubscriptionID: QWord; Success: Boolean; BytesTransferred,
  ErrorCode: Cardinal; const Buffer);
var
  WindowsSource: TWindowsFileChangeSource;
  Subscription: TWindowsSubscription;
begin
  if not (Source is TWindowsFileChangeSource) then Exit;
  WindowsSource := TWindowsFileChangeSource(Source);
  WindowsSource.FLock.Acquire;
  try
    Subscription := WindowsSource.FindSubscription(SubscriptionID);
    if (Subscription <> nil) and (Subscription.Watch <> nil) and
       not Subscription.Watch.Retired then
      WindowsSource.ProcessNotificationPayload(Subscription.Watch, Success,
        BytesTransferred, ErrorCode, Buffer);
  finally
    WindowsSource.FLock.Release;
  end;
end;

function WindowsWatchPathProbeCountForTest(
  Source: TFileChangeSource): Integer;
var
  WindowsSource: TWindowsFileChangeSource;
begin
  Result := 0;
  if not (Source is TWindowsFileChangeSource) then Exit;
  WindowsSource := TWindowsFileChangeSource(Source);
  WindowsSource.FLock.Acquire;
  try
    Result := WindowsSource.FPathProbeCount;
  finally
    WindowsSource.FLock.Release;
  end;
end;

function WindowsWatchCompletionCountForTest(
  Source: TFileChangeSource): Integer;
var
  WindowsSource: TWindowsFileChangeSource;
begin
  Result := 0;
  if not (Source is TWindowsFileChangeSource) then Exit;
  WindowsSource := TWindowsFileChangeSource(Source);
  WindowsSource.FLock.Acquire;
  try
    Result := WindowsSource.FCompletionCount;
  finally
    WindowsSource.FLock.Release;
  end;
end;

function WindowsWatchCancellationCountsForTest(
  Source: TFileChangeSource; out Requests, Completions: Integer): Boolean;
var
  WindowsSource: TWindowsFileChangeSource;
begin
  Requests := 0;
  Completions := 0;
  Result := Source is TWindowsFileChangeSource;
  if not Result then Exit;
  WindowsSource := TWindowsFileChangeSource(Source);
  WindowsSource.FLock.Acquire;
  try
    Requests := WindowsSource.FCancelRequestCount;
    Completions := WindowsSource.FCancelledCompletionCount;
  finally
    WindowsSource.FLock.Release;
  end;
end;

{$ENDIF}

initialization
  RegisterFileChangeSourceFactory(@CreateWindowsFileChangeSource);

end.

end.

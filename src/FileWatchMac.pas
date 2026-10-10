unit FileWatchMac;

{$mode objfpc}{$H+}

interface

uses
  Classes, FileWatch, SyncObjs;

type
  TMacWatchSubscription = record
    ID, Generation: QWord;
    Path, ParentPath, TargetParentPath: string;
    ParentFD, TargetParentFD, FileFD: LongInt;
    ParentToken, TargetParentToken, FileToken: QWord;
    Device, Inode: QWord;
    HasIdentity, Degraded: Boolean;
  end;

  TMacFileChangeSource = class(TFileChangeSource)
  private
    FLock: TCriticalSection;
    FQueueFD: LongInt;
    FWorker: TThread;
    FStopping: Boolean;
    FBackendFailed: Boolean;
    FNextWatchToken: QWord;
    FSubscriptions: array of TMacWatchSubscription;
    {$IFDEF TPX_FILEWATCH_TESTS}
    FProbeCount: QWord;
    FRetiredSubscriptionID, FRetiredFileToken: QWord;
    function GetProbeCount: QWord;
    procedure ResetProbeCount;
    procedure InjectVnodeEvent(SubscriptionID: QWord; IsParent: Boolean;
      Flags: Word; ErrorCode: LongInt = 0);
    procedure InjectStaleFileEvent(SubscriptionID: QWord);
    {$ENDIF}
    function NextWatchToken: QWord;
    function FindSubscription(ID: QWord): LongInt;
    function FindWatch(Token: QWord; out Index: LongInt;
      out IsParent, IsTargetParent: Boolean): Boolean;
    procedure RunWorker;
    procedure ProcessKernelEvent(const Event: Pointer);
    procedure ReconcileFile(Index: LongInt; ForceRearm: Boolean = False);
    procedure ReconcileParent(Index: LongInt; Flags: LongWord;
      IsTargetParent: Boolean);
    function UpdateTargetParent(Index: LongInt): Boolean;
    procedure MarkDegraded(Index: LongInt; const ErrorText: string);
    procedure RefreshStatus;
    procedure CloseWatch(var FD: LongInt; var Token: QWord);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
    function Subscribe(const Path: string; Generation: QWord;
      out SubscriptionID: QWord): TFileWatchStatus; override;
    procedure Unsubscribe(SubscriptionID: QWord); override;
    {$IFDEF TPX_FILEWATCH_TESTS}
    property ProbeCount: QWord read GetProbeCount;
    procedure TestResetProbeCount;
    procedure TestInjectVnodeEvent(SubscriptionID: QWord; IsParent: Boolean;
      Flags: Word; ErrorCode: LongInt = 0);
    procedure TestInjectStaleFileEvent(SubscriptionID: QWord);
    {$ENDIF}
  end;

implementation

uses
  BaseUnix, SysUtils;

const
  EVFILT_VNODE = -4;
  EVFILT_USER = -10;
  EV_ADD = $0001;
  EV_DELETE = $0002;
  EV_CLEAR = $0020;
  EV_RECEIPT = $0040;
  EV_ERROR = $4000;
  NOTE_DELETE = $00000001;
  NOTE_WRITE = $00000002;
  NOTE_EXTEND = $00000004;
  NOTE_ATTRIB = $00000008;
  NOTE_RENAME = $00000020;
  NOTE_REVOKE = $00000040;
  NOTE_TRIGGER = $01000000;
  NOTE_WATCH = NOTE_DELETE or NOTE_WRITE or NOTE_EXTEND or NOTE_ATTRIB or
    NOTE_RENAME or NOTE_REVOKE;
  O_EVTONLY = $00008000;
  O_DIRECTORY = $00100000;
  O_CLOEXEC = $01000000;
  TARGET_PATH_BUFFER = 4096;
  CONTROL_TOKEN = QWord($FFFFFFFFFFFFFFFF);

type
  {$PACKRECORDS 4}
  TKernelEvent = record
    Ident: PtrUInt;
    Filter: SmallInt;
    Flags: Word;
    FFlags: LongWord;
    Data: PtrInt;
    UData: Pointer;
  end;
  {$PACKRECORDS DEFAULT}
  PKernelEvent = ^TKernelEvent;

  TMacWatcherThread = class(TThread)
  private
    FOwner: TMacFileChangeSource;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TMacFileChangeSource);
  end;

function kqueue: LongInt; cdecl; external name 'kqueue';
function kevent(KQueueFD: LongInt; ChangeList: PKernelEvent;
  ChangeCount: LongInt; EventList: PKernelEvent; EventCount: LongInt;
  Timeout: Pointer): LongInt; cdecl; external name 'kevent';
function __error: PLongInt; cdecl; external name '__error';
function realpath(Path, ResolvedPath: PChar): PChar; cdecl;
  external name 'realpath';

function LibcErrorCode: LongInt; inline;
var
  ErrnoPointer: PLongInt;
begin
  ErrnoPointer := __error;
  if ErrnoPointer = nil then Result := ESysEIO
  else Result := ErrnoPointer^;
end;

function EventToken(const Event: TKernelEvent): QWord; inline;
begin
  Result := QWord(PtrUInt(Event.UData));
end;

procedure FillKernelEvent(out Event: TKernelEvent; Ident: PtrUInt;
  Filter: SmallInt; Flags: Word; FFlags: LongWord; Data: PtrInt;
  Token: QWord);
begin
  FillChar(Event, SizeOf(Event), 0);
  Event.Ident := Ident;
  Event.Filter := Filter;
  Event.Flags := Flags;
  Event.FFlags := FFlags;
  Event.Data := Data;
  Event.UData := Pointer(PtrUInt(Token));
end;

function ApplyChange(KQueueFD: LongInt; const Change: TKernelEvent;
  out ErrorCode: LongInt): Boolean;
var
  Receipt: TKernelEvent;
  Request: TKernelEvent;
  Count: LongInt;
begin
  Request := Change;
  Request.Flags := Request.Flags or EV_RECEIPT;
  FillChar(Receipt, SizeOf(Receipt), 0);
  Count := kevent(KQueueFD, @Request, 1, @Receipt, 1, nil);
  if Count < 0 then begin
    ErrorCode := LibcErrorCode;
    Exit(False);
  end;
  if (Count <> 1) or ((Receipt.Flags and EV_ERROR) = 0) then begin
    ErrorCode := ESysEIO;
    Exit(False);
  end;
  ErrorCode := LongInt(Receipt.Data);
  Result := ErrorCode = 0;
end;

function AddVnode(KQueueFD, FD: LongInt; Token: QWord;
  out ErrorCode: LongInt): Boolean;
var
  Change: TKernelEvent;
begin
  FillKernelEvent(Change, PtrUInt(FD), EVFILT_VNODE, EV_ADD or EV_CLEAR,
    NOTE_WATCH, 0, Token);
  Result := ApplyChange(KQueueFD, Change, ErrorCode);
end;

procedure DeleteVnode(KQueueFD, FD: LongInt; Token: QWord);
var
  Change: TKernelEvent;
  ErrorCode: LongInt;
begin
  if (KQueueFD < 0) or (FD < 0) then Exit;
  FillKernelEvent(Change, PtrUInt(FD), EVFILT_VNODE, EV_DELETE, 0, 0, Token);
  ApplyChange(KQueueFD, Change, ErrorCode);
end;

function TriggerWorker(KQueueFD: LongInt): Boolean;
var
  Change: TKernelEvent;
begin
  FillKernelEvent(Change, 1, EVFILT_USER, 0, NOTE_TRIGGER, 0,
    CONTROL_TOKEN);
  Result := kevent(KQueueFD, @Change, 1, nil, 0, nil) >= 0;
end;

function FileIdentity(FD: LongInt; out Device, Inode: QWord): Boolean;
var
  Info: Stat;
begin
  FillChar(Info, SizeOf(Info), 0);
  Result := fpFStat(FD, Info) = 0;
  if Result then begin
    Device := QWord(Info.st_dev);
    Inode := QWord(Info.st_ino);
  end;
end;

function OpenForEvents(const Path: string; Directory: Boolean): LongInt;
var
  Flags: LongInt;
begin
  Flags := O_EVTONLY or O_CLOEXEC;
  if Directory then Flags := Flags or O_DIRECTORY;
  Result := fpOpen(PChar(Path), Flags);
end;

function ErrorDescription(ErrorCode: LongInt): string;
begin
  if ErrorCode = 0 then Result := 'Unknown kqueue error'
  else Result := SysErrorMessage(ErrorCode);
end;

constructor TMacWatcherThread.Create(AOwner: TMacFileChangeSource);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FOwner := AOwner;
end;

constructor TMacFileChangeSource.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FQueueFD := -1;
  FNextWatchToken := 1;
end;

destructor TMacFileChangeSource.Destroy;
begin
  Stop;
  FLock.Free;
  inherited Destroy;
end;

function TMacFileChangeSource.NextWatchToken: QWord;
begin
  Inc(FNextWatchToken);
  if (FNextWatchToken = 0) or (FNextWatchToken = CONTROL_TOKEN) then
    FNextWatchToken := 1;
  Result := FNextWatchToken;
end;

{$IFDEF TPX_FILEWATCH_TESTS}
function TMacFileChangeSource.GetProbeCount: QWord;
begin
  FLock.Acquire;
  try
    Result := FProbeCount;
  finally
    FLock.Release;
  end;
end;

procedure TMacFileChangeSource.ResetProbeCount;
begin
  FLock.Acquire;
  try
    FProbeCount := 0;
  finally
    FLock.Release;
  end;
end;
{$ENDIF}

function TMacFileChangeSource.FindSubscription(ID: QWord): LongInt;
var
  I: LongInt;
begin
  for I := 0 to High(FSubscriptions) do
    if FSubscriptions[I].ID = ID then Exit(I);
  Result := -1;
end;

function TMacFileChangeSource.FindWatch(Token: QWord; out Index: LongInt;
  out IsParent, IsTargetParent: Boolean): Boolean;
var
  I: LongInt;
begin
  for I := 0 to High(FSubscriptions) do begin
    if (Token <> 0) and (FSubscriptions[I].ParentToken = Token) then begin
      Index := I;
      IsParent := True;
      IsTargetParent := False;
      Exit(True);
    end;
    if (Token <> 0) and (FSubscriptions[I].TargetParentToken = Token) then begin
      Index := I;
      IsParent := True;
      IsTargetParent := True;
      Exit(True);
    end;
    if (Token <> 0) and (FSubscriptions[I].FileToken = Token) then begin
      Index := I;
      IsParent := False;
      IsTargetParent := False;
      Exit(True);
    end;
  end;
  Index := -1;
  IsParent := False;
  IsTargetParent := False;
  Result := False;
end;

procedure TMacFileChangeSource.CloseWatch(var FD: LongInt;
  var Token: QWord);
begin
  if FD >= 0 then begin
    DeleteVnode(FQueueFD, FD, Token);
    fpClose(FD);
  end;
  FD := -1;
  Token := 0;
end;

procedure TMacFileChangeSource.Start;
var
  Change: TKernelEvent;
  ErrorCode: LongInt;
begin
  FLock.Acquire;
  try
    if (FQueueFD >= 0) or FBackendFailed then Exit;
    SetStatus(fwsStarting);
    FStopping := False;
    FQueueFD := kqueue;
    if FQueueFD < 0 then begin
      FBackendFailed := True;
      SetStatus(fwsError);
      PublishEvent(MakeEvent(0, 0, '', fckBackendError, fwsError,
        ErrorDescription(LibcErrorCode)));
      Exit;
    end;
    FillKernelEvent(Change, 1, EVFILT_USER, EV_ADD or EV_CLEAR, 0, 0,
      CONTROL_TOKEN);
    if not ApplyChange(FQueueFD, Change, ErrorCode) then begin
      fpClose(FQueueFD);
      FQueueFD := -1;
      FBackendFailed := True;
      SetStatus(fwsError);
      PublishEvent(MakeEvent(0, 0, '', fckBackendError, fwsError,
        ErrorDescription(ErrorCode)));
      Exit;
    end;
    try
      FWorker := TMacWatcherThread.Create(Self);
      FWorker.Start;
    except
      FWorker.Free;
      FWorker := nil;
      fpClose(FQueueFD);
      FQueueFD := -1;
      FBackendFailed := True;
      SetStatus(fwsError);
      PublishEvent(MakeEvent(0, 0, '', fckBackendError, fwsError,
        'Could not start the kqueue worker thread'));
      Exit;
    end;
    SetStatus(fwsReady);
    PublishEvent(MakeEvent(0, 0, '', fckReady, fwsReady));
  finally
    FLock.Release;
  end;
end;

procedure TMacFileChangeSource.Stop;
var
  Worker: TThread;
  I: LongInt;
begin
  FLock.Acquire;
  try
    if FQueueFD < 0 then begin
      FBackendFailed := False;
      SetStatus(fwsStopped);
      Exit;
    end;
    FStopping := True;
    Worker := FWorker;
    TriggerWorker(FQueueFD);
  finally
    FLock.Release;
  end;

  if Worker <> nil then Worker.WaitFor;

  FLock.Acquire;
  try
    for I := 0 to High(FSubscriptions) do begin
      CloseWatch(FSubscriptions[I].FileFD, FSubscriptions[I].FileToken);
      CloseWatch(FSubscriptions[I].TargetParentFD,
        FSubscriptions[I].TargetParentToken);
      CloseWatch(FSubscriptions[I].ParentFD, FSubscriptions[I].ParentToken);
    end;
    SetLength(FSubscriptions, 0);
    FWorker := nil;
    fpClose(FQueueFD);
    FQueueFD := -1;
    FStopping := False;
    FBackendFailed := False;
    SetStatus(fwsStopped);
  finally
    FLock.Release;
  end;
  Worker.Free;
end;

function TMacFileChangeSource.Subscribe(const Path: string;
  Generation: QWord; out SubscriptionID: QWord): TFileWatchStatus;
var
  Normalized, Parent: string;
  Index, ErrorCode, FD: LongInt;
  Token: QWord;
  Device, Inode: QWord;
  Sub: TMacWatchSubscription;
begin
  SubscriptionID := 0;
  if Path = '' then Exit(fwsError);
  Normalized := NormalizeFileWatchPath(Path);
  Parent := ExtractFileDir(Normalized);
  if Parent = '' then Parent := PathDelim;
  FillChar(Sub, SizeOf(Sub), 0);
  Sub.ParentFD := -1;
  Sub.TargetParentFD := -1;
  Sub.FileFD := -1;
  Sub.ID := 0;
  Sub.Generation := Generation;
  Sub.Path := Normalized;
  Sub.ParentPath := Parent;

  FLock.Acquire;
  try
    if FQueueFD < 0 then Exit(fwsStopped);
    if FStopping or FBackendFailed then Exit(fwsError);
    Sub.ParentFD := OpenForEvents(Parent, True);
    if Sub.ParentFD < 0 then begin
      ErrorCode := GetLastOSError;
      SetStatus(fwsDegraded);
      PublishEvent(MakeEvent(0, Generation, Normalized, fckBackendError,
        fwsDegraded, 'Cannot watch parent directory: ' +
        ErrorDescription(ErrorCode)));
      Exit(fwsDegraded);
    end;
    Sub.ParentToken := NextWatchToken;
    if not AddVnode(FQueueFD, Sub.ParentFD, Sub.ParentToken, ErrorCode) then begin
      fpClose(Sub.ParentFD);
      Sub.ParentFD := -1;
      SetStatus(fwsDegraded);
      PublishEvent(MakeEvent(0, Generation, Normalized, fckBackendError,
        fwsDegraded, 'Cannot register parent directory: ' +
        ErrorDescription(ErrorCode)));
      Exit(fwsDegraded);
    end;
    Sub.ID := AllocateSubscriptionID;

    FD := OpenForEvents(Normalized, False);
    if FD >= 0 then begin
      Token := NextWatchToken;
      if AddVnode(FQueueFD, FD, Token, ErrorCode) and
        FileIdentity(FD, Device, Inode) then begin
        Sub.FileFD := FD;
        Sub.FileToken := Token;
        Sub.Device := Device;
        Sub.Inode := Inode;
        Sub.HasIdentity := True;
      end else begin
        if ErrorCode = 0 then ErrorCode := GetLastOSError;
        fpClose(FD);
        Sub.Degraded := True;
      end;
    end else if GetLastOSError <> ESysENOENT then begin
      Sub.Degraded := True;
      ErrorCode := GetLastOSError;
    end;

    Index := Length(FSubscriptions);
    SetLength(FSubscriptions, Index + 1);
    FSubscriptions[Index] := Sub;
    SubscriptionID := Sub.ID;
    UpdateTargetParent(Index);
    if Sub.Degraded then begin
      SetStatus(fwsDegraded);
      PublishEvent(MakeEvent(Sub.ID, Generation, Normalized,
        fckBackendError, fwsDegraded, 'Cannot watch the file: ' +
        ErrorDescription(ErrorCode)));
      Result := fwsDegraded;
    end else begin
      RefreshStatus;
      Result := Status;
    end;
    TriggerWorker(FQueueFD);
  finally
    FLock.Release;
  end;
end;

procedure TMacFileChangeSource.Unsubscribe(SubscriptionID: QWord);
var
  Index, I: LongInt;
begin
  FLock.Acquire;
  try
    Index := FindSubscription(SubscriptionID);
    if Index < 0 then Exit;
    {$IFDEF TPX_FILEWATCH_TESTS}
    FRetiredSubscriptionID := SubscriptionID;
    FRetiredFileToken := FSubscriptions[Index].FileToken;
    {$ENDIF}
    CloseWatch(FSubscriptions[Index].FileFD, FSubscriptions[Index].FileToken);
    CloseWatch(FSubscriptions[Index].ParentFD,
      FSubscriptions[Index].ParentToken);
    CloseWatch(FSubscriptions[Index].TargetParentFD,
      FSubscriptions[Index].TargetParentToken);
    for I := Index to High(FSubscriptions) - 1 do
      FSubscriptions[I] := FSubscriptions[I + 1];
    SetLength(FSubscriptions, Length(FSubscriptions) - 1);
    RefreshStatus;
    if FQueueFD >= 0 then TriggerWorker(FQueueFD);
  finally
    FLock.Release;
  end;
end;

{$IFDEF TPX_FILEWATCH_TESTS}
procedure TMacFileChangeSource.TestResetProbeCount;
begin
  ResetProbeCount;
end;

procedure TMacFileChangeSource.TestInjectVnodeEvent(SubscriptionID: QWord;
  IsParent: Boolean; Flags: Word; ErrorCode: LongInt);
begin
  InjectVnodeEvent(SubscriptionID, IsParent, Flags, ErrorCode);
end;

procedure TMacFileChangeSource.InjectVnodeEvent(SubscriptionID: QWord;
  IsParent: Boolean; Flags: Word; ErrorCode: LongInt);
var
  Index: LongInt;
  Token: QWord;
  Kernel: TKernelEvent;
begin
  FLock.Acquire;
  try
    Index := FindSubscription(SubscriptionID);
    if Index < 0 then Exit;
    Token := FSubscriptions[Index].FileToken;
    if IsParent then Token := FSubscriptions[Index].ParentToken;
    FillKernelEvent(Kernel, 0, EVFILT_VNODE, 0, Flags, ErrorCode, Token);
    if ErrorCode <> 0 then Kernel.Flags := EV_ERROR;
    ProcessKernelEvent(@Kernel);
  finally
    FLock.Release;
  end;
end;

procedure TMacFileChangeSource.TestInjectStaleFileEvent(
  SubscriptionID: QWord);
begin
  InjectStaleFileEvent(SubscriptionID);
end;

procedure TMacFileChangeSource.InjectStaleFileEvent(SubscriptionID: QWord);
var
  Kernel: TKernelEvent;
begin
  FLock.Acquire;
  try
    if (SubscriptionID <> FRetiredSubscriptionID) or
      (FRetiredFileToken = 0) then Exit;
    FillKernelEvent(Kernel, 0, EVFILT_VNODE, 0, NOTE_WRITE, 0,
      FRetiredFileToken);
    ProcessKernelEvent(@Kernel);
  finally
    FLock.Release;
  end;
end;
{$ENDIF}

procedure TMacFileChangeSource.RefreshStatus;
var
  I: LongInt;
begin
  if FBackendFailed or (FQueueFD < 0) then Exit;
  for I := 0 to High(FSubscriptions) do
    if FSubscriptions[I].Degraded then begin
      SetStatus(fwsDegraded);
      Exit;
    end;
  SetStatus(fwsReady);
end;

procedure TMacFileChangeSource.MarkDegraded(Index: LongInt;
  const ErrorText: string);
var
  SubID, Generation: QWord;
  Path: string;
begin
  if (Index < 0) or (Index > High(FSubscriptions)) then Exit;
  if FSubscriptions[Index].Degraded then Exit;
  FSubscriptions[Index].Degraded := True;
  SubID := FSubscriptions[Index].ID;
  Generation := FSubscriptions[Index].Generation;
  Path := FSubscriptions[Index].Path;
  SetStatus(fwsDegraded);
  PublishEvent(MakeEvent(SubID, Generation, Path, fckBackendError,
    fwsDegraded, ErrorText));
end;

procedure TMacFileChangeSource.ReconcileFile(Index: LongInt;
  ForceRearm: Boolean);
var
  FD, VerifyFD, ErrorCode, Attempt: LongInt;
  Token, Device, Inode, VerifyDevice, VerifyInode: QWord;
  HadIdentity, SameIdentity, TargetParentReady: Boolean;
  EventKind: TFileChangeKind;
  Path: string;
begin
  if (Index < 0) or (Index > High(FSubscriptions)) then Exit;
  {$IFDEF TPX_FILEWATCH_TESTS}
  Inc(FProbeCount);
  {$ENDIF}
  Path := FSubscriptions[Index].Path;
  for Attempt := 1 to 4 do begin
    FD := OpenForEvents(Path, False);
    if FD < 0 then begin
      ErrorCode := GetLastOSError;
      if ErrorCode = ESysENOENT then begin
        if FSubscriptions[Index].HasIdentity then begin
          CloseWatch(FSubscriptions[Index].FileFD,
            FSubscriptions[Index].FileToken);
          FSubscriptions[Index].HasIdentity := False;
          PublishEvent(MakeEvent(FSubscriptions[Index].ID,
            FSubscriptions[Index].Generation, Path, fckDisappeared,
            Status));
        end;
      end else begin
        MarkDegraded(Index, 'Cannot open watched file: ' +
          ErrorDescription(ErrorCode));
      end;
      Exit;
    end;

    if not FileIdentity(FD, Device, Inode) then begin
      ErrorCode := GetLastOSError;
      fpClose(FD);
      MarkDegraded(Index, 'Cannot inspect watched file: ' +
        ErrorDescription(ErrorCode));
      Exit;
    end;

    SameIdentity := FSubscriptions[Index].HasIdentity and
      (FSubscriptions[Index].Device = Device) and
      (FSubscriptions[Index].Inode = Inode);
    if SameIdentity and not ForceRearm then begin
      fpClose(FD);
      if not UpdateTargetParent(Index) then begin
        MarkDegraded(Index, 'Cannot confirm the target-directory watch');
        Exit;
      end;
      if FSubscriptions[Index].Degraded then begin
        FSubscriptions[Index].Degraded := False;
        RefreshStatus;
      end;
      Exit;
    end;

    Token := NextWatchToken;
    if not AddVnode(FQueueFD, FD, Token, ErrorCode) then begin
      fpClose(FD);
      MarkDegraded(Index, 'Cannot register file vnode: ' +
        ErrorDescription(ErrorCode));
      Exit;
    end;

    { Arm the new inode before checking the name again. A rename during this
      window leaves another directory event queued for reconciliation. }
    VerifyFD := OpenForEvents(Path, False);
    if VerifyFD < 0 then begin
      ErrorCode := GetLastOSError;
      CloseWatch(FD, Token);
      if ErrorCode = ESysENOENT then Continue;
      MarkDegraded(Index, 'Cannot verify watched path: ' +
        ErrorDescription(ErrorCode));
      Exit;
    end;
    if not FileIdentity(VerifyFD, VerifyDevice, VerifyInode) then begin
      ErrorCode := GetLastOSError;
      fpClose(VerifyFD);
      CloseWatch(FD, Token);
      MarkDegraded(Index, 'Cannot verify watched file: ' +
        ErrorDescription(ErrorCode));
      Exit;
    end;
    fpClose(VerifyFD);
    if (Device <> VerifyDevice) or (Inode <> VerifyInode) then begin
      CloseWatch(FD, Token);
      Continue;
    end;

    HadIdentity := FSubscriptions[Index].HasIdentity;
    if HadIdentity then
      CloseWatch(FSubscriptions[Index].FileFD,
        FSubscriptions[Index].FileToken);
    FSubscriptions[Index].FileFD := FD;
    FSubscriptions[Index].FileToken := Token;
    FSubscriptions[Index].Device := Device;
    FSubscriptions[Index].Inode := Inode;
    FSubscriptions[Index].HasIdentity := True;
    TargetParentReady := UpdateTargetParent(Index);
    if not TargetParentReady then
      MarkDegraded(Index, 'Cannot confirm the target-directory watch');
    EventKind := fckReappeared;
    if SameIdentity then EventKind := fckRescanRequired
    else if HadIdentity then EventKind := fckReplaced;
    if TargetParentReady then begin
      FSubscriptions[Index].Degraded := False;
      RefreshStatus;
    end;
    PublishEvent(MakeEvent(FSubscriptions[Index].ID,
      FSubscriptions[Index].Generation, Path, EventKind, Status));
    Exit;
  end;

  PublishEvent(MakeEvent(FSubscriptions[Index].ID,
    FSubscriptions[Index].Generation, Path, fckRescanRequired, Status,
    'The watched path changed repeatedly while its vnode was re-armed'));
end;

function TMacFileChangeSource.UpdateTargetParent(Index: LongInt): Boolean;
var
  Resolved: array[0..TARGET_PATH_BUFFER - 1] of AnsiChar;
  ResolvedFile, NewParentPath: string;
  NewFD, ErrorCode: LongInt;
  NewToken, Device, Inode, ParentDevice, ParentInode: QWord;
begin
  Result := False;
  if (Index < 0) or (Index > High(FSubscriptions)) then Exit;
  FillChar(Resolved, SizeOf(Resolved), 0);
  if realpath(PChar(FSubscriptions[Index].Path), @Resolved[0]) = nil then begin
    ErrorCode := LibcErrorCode;
    { Keep the last target-directory watch while a source is missing. }
    if ErrorCode <> ESysENOENT then
      MarkDegraded(Index, 'Cannot resolve watched file target: ' +
        ErrorDescription(ErrorCode));
    Exit;
  end;
  ResolvedFile := StrPas(@Resolved[0]);
  NewParentPath := ExtractFileDir(ResolvedFile);
  if NewParentPath = '' then NewParentPath := PathDelim;
  NewFD := OpenForEvents(NewParentPath, True);
  if NewFD < 0 then begin
    MarkDegraded(Index, 'Cannot watch target directory: ' +
      ErrorDescription(GetLastOSError));
    Exit;
  end;
  if not FileIdentity(NewFD, Device, Inode) then begin
    ErrorCode := GetLastOSError;
    fpClose(NewFD);
    MarkDegraded(Index, 'Cannot inspect target directory: ' +
      ErrorDescription(ErrorCode));
    Exit;
  end;

  if (FSubscriptions[Index].ParentFD >= 0) and
    FileIdentity(FSubscriptions[Index].ParentFD, ParentDevice, ParentInode) and
    (Device = ParentDevice) and (Inode = ParentInode) then begin
    fpClose(NewFD);
    CloseWatch(FSubscriptions[Index].TargetParentFD,
      FSubscriptions[Index].TargetParentToken);
    FSubscriptions[Index].TargetParentPath := NewParentPath;
    Exit(True);
  end;
  if (FSubscriptions[Index].TargetParentFD >= 0) and
    FileIdentity(FSubscriptions[Index].TargetParentFD, ParentDevice,
      ParentInode) and (Device = ParentDevice) and (Inode = ParentInode) then begin
    fpClose(NewFD);
    FSubscriptions[Index].TargetParentPath := NewParentPath;
    Exit(True);
  end;

  NewToken := NextWatchToken;
  if not AddVnode(FQueueFD, NewFD, NewToken, ErrorCode) then begin
    fpClose(NewFD);
    MarkDegraded(Index, 'Cannot register target directory: ' +
      ErrorDescription(ErrorCode));
    Exit;
  end;
  CloseWatch(FSubscriptions[Index].TargetParentFD,
    FSubscriptions[Index].TargetParentToken);
  FSubscriptions[Index].TargetParentFD := NewFD;
  FSubscriptions[Index].TargetParentToken := NewToken;
  FSubscriptions[Index].TargetParentPath := NewParentPath;
  Result := True;
end;

procedure TMacFileChangeSource.ReconcileParent(Index: LongInt;
  Flags: LongWord; IsTargetParent: Boolean);
var
  FD, ErrorCode: LongInt;
  Token: QWord;
  ParentPath: string;
begin
  if (Index < 0) or (Index > High(FSubscriptions)) then Exit;
  if (Flags and (NOTE_RENAME or NOTE_DELETE or NOTE_REVOKE)) <> 0 then begin
    if IsTargetParent then
      ParentPath := FSubscriptions[Index].TargetParentPath
    else
      ParentPath := FSubscriptions[Index].ParentPath;
    FD := OpenForEvents(ParentPath, True);
    if FD < 0 then begin
      MarkDegraded(Index, 'Parent directory was renamed, removed, or revoked: ' +
        ErrorDescription(GetLastOSError));
      Exit;
    end;
    Token := NextWatchToken;
    if not AddVnode(FQueueFD, FD, Token, ErrorCode) then begin
      fpClose(FD);
      MarkDegraded(Index, 'Cannot re-arm parent directory: ' +
        ErrorDescription(ErrorCode));
      Exit;
    end;
    if IsTargetParent then begin
      CloseWatch(FSubscriptions[Index].TargetParentFD,
        FSubscriptions[Index].TargetParentToken);
      FSubscriptions[Index].TargetParentFD := FD;
      FSubscriptions[Index].TargetParentToken := Token;
    end else begin
      CloseWatch(FSubscriptions[Index].ParentFD,
        FSubscriptions[Index].ParentToken);
      FSubscriptions[Index].ParentFD := FD;
      FSubscriptions[Index].ParentToken := Token;
    end;
    PublishEvent(MakeEvent(FSubscriptions[Index].ID,
      FSubscriptions[Index].Generation, FSubscriptions[Index].Path,
      fckRescanRequired, Status,
      'Parent directory watch was re-armed after a vnode change'));
  end;
  UpdateTargetParent(Index);
  ReconcileFile(Index);
end;

procedure TMacFileChangeSource.ProcessKernelEvent(const Event: Pointer);
var
  Kernel: TKernelEvent;
  Index: LongInt;
  IsParent, IsTargetParent: Boolean;
  Token: QWord;
  PreviousFileToken: QWord;
  Kind: TFileChangeKind;
begin
  Kernel := PKernelEvent(Event)^;
  Token := EventToken(Kernel);
  if Token = CONTROL_TOKEN then Exit;
  if not FindWatch(Token, Index, IsParent, IsTargetParent) then Exit;
  if (Kernel.Flags and EV_ERROR) <> 0 then begin
    MarkDegraded(Index, 'kqueue watch failed: ' +
      ErrorDescription(LongInt(Kernel.Data)));
    if IsParent then
      ReconcileParent(Index, NOTE_REVOKE, IsTargetParent)
    else begin
      CloseWatch(FSubscriptions[Index].FileFD,
        FSubscriptions[Index].FileToken);
      FSubscriptions[Index].HasIdentity := False;
      ReconcileFile(Index);
    end;
    Exit;
  end;
  if Kernel.Filter <> EVFILT_VNODE then Exit;
  if IsParent then begin
    ReconcileParent(Index, Kernel.FFlags, IsTargetParent);
    Exit;
  end;

  PreviousFileToken := FSubscriptions[Index].FileToken;
  ReconcileFile(Index, (Kernel.FFlags and NOTE_REVOKE) <> 0);
  if (PreviousFileToken = 0) or
    (PreviousFileToken <> FSubscriptions[Index].FileToken) then Exit;
  if (Kernel.FFlags and (NOTE_WRITE or NOTE_EXTEND)) <> 0 then
    Kind := fckContentChanged
  else if (Kernel.FFlags and NOTE_ATTRIB) <> 0 then
    Kind := fckMetadataChanged
  else
    Exit;
  PublishEvent(MakeEvent(FSubscriptions[Index].ID,
    FSubscriptions[Index].Generation, FSubscriptions[Index].Path,
    Kind, Status));
end;

procedure TMacFileChangeSource.RunWorker;
var
  Events: array[0..63] of TKernelEvent;
  Count, I, ErrorCode: LongInt;
begin
  while True do begin
    Count := kevent(FQueueFD, nil, 0, @Events[0], Length(Events), nil);
    if Count < 0 then begin
      ErrorCode := LibcErrorCode;
      if ErrorCode = ESysEINTR then Continue;
      FLock.Acquire;
      try
        if not FStopping then begin
          FBackendFailed := True;
          SetStatus(fwsError);
          PublishEvent(MakeEvent(0, 0, '', fckBackendError, fwsError,
            'kqueue wait failed: ' + ErrorDescription(ErrorCode)));
        end;
      finally
        FLock.Release;
      end;
      Exit;
    end;
    FLock.Acquire;
    try
      if FStopping then Exit;
      for I := 0 to Count - 1 do ProcessKernelEvent(@Events[I]);
    finally
      FLock.Release;
    end;
  end;
end;

function CreateMacFileChangeSource: TFileChangeSource;
begin
  Result := TMacFileChangeSource.Create;
end;

procedure TMacWatcherThread.Execute;
begin
  FOwner.RunWorker;
end;

initialization
  RegisterFileChangeSourceFactory(@CreateMacFileChangeSource);

end.

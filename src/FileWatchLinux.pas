unit FileWatchLinux;

{$mode objfpc}{$H+}

interface

{$IFDEF LINUX}

uses
  Classes, SysUtils, FileWatch;

type
  TLinuxFileChangeSource = class(TFileChangeSource)
  private
    FLock: TRTLCriticalSection;
    FSubscriptions: TList;
    FWorker: TThread;
    FInotifyFD: LongInt;
    FWakeReadFD: LongInt;
    FWakeWriteFD: LongInt;
    FStopping: Boolean;
    FStarted: Boolean;
    FTerminalError: Boolean;
    FPollReturns: QWord;
    procedure RunWorker;
    procedure ProcessBuffer(Buffer: PByte; Count: LongInt);
    procedure ProcessEvent(WatchDescriptor: LongInt; Mask: LongWord;
      Cookie: LongWord; const Name: string);
    procedure WakeWorker;
    function ReadStopping: Boolean;
    function ErrorText(const Operation: string; ErrorCode: LongInt): string;
    function AddDirectoryWatch(const Directory: string;
      out ErrorCode: LongInt): LongInt;
    procedure PublishInvalidation(const MessageText: string);
    procedure UpdateWatchStatus;
    procedure FailWorker(const MessageText: string);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
    function Subscribe(const Path: string; Generation: QWord;
      out SubscriptionID: QWord): TFileWatchStatus; override;
    procedure Unsubscribe(SubscriptionID: QWord); override;
    {$IFDEF TPX_FILEWATCH_TESTS}
    procedure InjectBufferForTest(Buffer: PByte; Count: LongInt);
    procedure InjectOverflowForTest;
    procedure InjectQueueOverflowForTest;
    procedure InjectWatchLossForTest(SubscriptionID: QWord);
    function PollReturnsForTest: QWord;
    {$ENDIF}
  end;

function CreateLinuxFileChangeSource: TFileChangeSource;

{$ENDIF}

implementation

{$IFDEF LINUX}

uses
  BaseUnix, ctypes;

const
  InAccess = $00000001;
  InModify = $00000002;
  InAttrib = $00000004;
  InCloseWrite = $00000008;
  InMovedFrom = $00000040;
  InMovedTo = $00000080;
  InCreate = $00000100;
  InDelete = $00000200;
  InDeleteSelf = $00000400;
  InMoveSelf = $00000800;
  InQueueOverflow = $00004000;
  InIgnored = $00008000;
  InIsDir = $40000000;
  WatchMask = InModify or InAttrib or InCloseWrite or InMovedFrom or
    InMovedTo or InCreate or InDelete or InDeleteSelf or InMoveSelf;
  InNonblock = $00000800;
  InCloexec = $00080000;
  PollIn = $0001;
  PollErr = $0008;
  PollHup = $0010;
  PollNval = $0020;
  ErrInterrupted = 4;
  ErrAgain = 11;

type
  TInotifyEventHeader = packed record
    WatchDescriptor: LongInt;
    Mask: LongWord;
    Cookie: LongWord;
    NameLength: LongWord;
  end;
  PInotifyEventHeader = ^TInotifyEventHeader;

  TPollFD = packed record
    FD: LongInt;
    Events: SmallInt;
    ReturnedEvents: SmallInt;
  end;
  PPollFD = ^TPollFD;

  TWatchSubscription = class
    ID: QWord;
    Generation: QWord;
    Path: string;
    ParentPath: string;
    BaseName: string;
    WatchDescriptor: LongInt;
    Present: Boolean;
  end;

  TFileWatchWorker = class(TThread)
  private
    FOwner: TLinuxFileChangeSource;
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TLinuxFileChangeSource);
  end;

function c_inotify_init1(Flags: cint): cint; cdecl; external 'c' name 'inotify_init1';
function c_inotify_add_watch(FD: cint; Path: PChar; Mask: cuint): cint; cdecl; external 'c' name 'inotify_add_watch';
function c_inotify_rm_watch(FD: cint; WatchDescriptor: cint): cint; cdecl; external 'c' name 'inotify_rm_watch';
function c_pipe2(FDs: PLongInt; Flags: cint): cint; cdecl; external 'c' name 'pipe2';
function c_poll(FDs: PPollFD; Count: csize_t; Timeout: cint): cint; cdecl; external 'c' name 'poll';
function c_read(FD: cint; Buffer: Pointer; Count: csize_t): PtrInt; cdecl; external 'c' name 'read';
function c_write(FD: cint; Buffer: Pointer; Count: csize_t): PtrInt; cdecl; external 'c' name 'write';
function c_close(FD: cint): cint; cdecl; external 'c' name 'close';
function c_errno_location: PLongInt; cdecl; external 'c' name '__errno_location';

function LastErrno: LongInt;
begin
  Result := c_errno_location^;
end;

constructor TFileWatchWorker.Create(Owner: TLinuxFileChangeSource);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FOwner := Owner;
end;

procedure TFileWatchWorker.Execute;
begin
  FOwner.RunWorker;
end;

constructor TLinuxFileChangeSource.Create;
begin
  inherited Create;
  InitCriticalSection(FLock);
  FSubscriptions := TList.Create;
  FInotifyFD := -1;
  FWakeReadFD := -1;
  FWakeWriteFD := -1;
end;

destructor TLinuxFileChangeSource.Destroy;
var
  I: Integer;
begin
  Stop;
  for I := 0 to FSubscriptions.Count - 1 do
    TObject(FSubscriptions[I]).Free;
  FSubscriptions.Free;
  DoneCriticalSection(FLock);
  inherited Destroy;
end;

function TLinuxFileChangeSource.ErrorText(const Operation: string;
  ErrorCode: LongInt): string;
begin
  Result := Operation + ': ' + SysErrorMessage(ErrorCode);
end;

function TLinuxFileChangeSource.ReadStopping: Boolean;
begin
  EnterCriticalSection(FLock);
  try
    Result := FStopping;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TLinuxFileChangeSource.WakeWorker;
var
  WakeByte: Byte;
begin
  if FWakeWriteFD < 0 then Exit;
  WakeByte := 1;
  c_write(FWakeWriteFD, @WakeByte, 1);
end;

function TLinuxFileChangeSource.AddDirectoryWatch(const Directory: string;
  out ErrorCode: LongInt): LongInt;
var
  EncodedDirectory: UTF8String;
begin
  EncodedDirectory := UTF8String(Directory) + #0;
  Result := c_inotify_add_watch(FInotifyFD, PChar(EncodedDirectory), WatchMask);
  if Result < 0 then ErrorCode := LastErrno else ErrorCode := 0;
end;

procedure TLinuxFileChangeSource.PublishInvalidation(const MessageText: string);
begin
  PublishEvent(MakeEvent(0, 0, '', fckRescanRequired, Status, MessageText));
end;

procedure TLinuxFileChangeSource.UpdateWatchStatus;
var
  I: Integer;
begin
  if not FStarted then Exit;
  if FTerminalError then
  begin
    SetStatus(fwsError);
    Exit;
  end;
  for I := 0 to FSubscriptions.Count - 1 do
    if TWatchSubscription(FSubscriptions[I]).WatchDescriptor < 0 then
    begin
      SetStatus(fwsDegraded);
      Exit;
    end;
  SetStatus(fwsReady);
end;

procedure TLinuxFileChangeSource.FailWorker(const MessageText: string);
begin
  EnterCriticalSection(FLock);
  try
    FTerminalError := True;
  finally
    LeaveCriticalSection(FLock);
  end;
  SetStatus(fwsError);
  PublishEvent(MakeEvent(0, 0, '', fckBackendError, fwsError, MessageText));
end;

procedure TLinuxFileChangeSource.Start;
var
  PipeFDs: array[0..1] of LongInt;
  ErrorCode: LongInt;
begin
  EnterCriticalSection(FLock);
  try
    if FStarted then Exit;
    SetStatus(fwsStarting);
    FStopping := False;
    FTerminalError := False;
    FInotifyFD := c_inotify_init1(InNonblock or InCloexec);
    if FInotifyFD < 0 then
    begin
      ErrorCode := LastErrno;
      SetStatus(fwsError);
      PublishEvent(MakeEvent(0, 0, '', fckBackendError, fwsError,
        ErrorText('inotify_init1', ErrorCode)));
      Exit;
    end;
    if c_pipe2(@PipeFDs[0], InNonblock or InCloexec) <> 0 then
    begin
      ErrorCode := LastErrno;
      c_close(FInotifyFD);
      FInotifyFD := -1;
      SetStatus(fwsError);
      PublishEvent(MakeEvent(0, 0, '', fckBackendError, fwsError,
        ErrorText('pipe2', ErrorCode)));
      Exit;
    end;
    FWakeReadFD := PipeFDs[0];
    FWakeWriteFD := PipeFDs[1];
    try
      FWorker := TFileWatchWorker.Create(Self);
      FWorker.Start;
      FStarted := True;
      SetStatus(fwsReady);
      PublishEvent(MakeEvent(0, 0, '', fckReady, fwsReady));
    except
      on E: Exception do
      begin
        if Assigned(FWorker) then
        begin
          FWorker.Free;
          FWorker := nil;
        end;
        c_close(FWakeReadFD);
        c_close(FWakeWriteFD);
        c_close(FInotifyFD);
        FWakeReadFD := -1;
        FWakeWriteFD := -1;
        FInotifyFD := -1;
        SetStatus(fwsError);
        PublishEvent(MakeEvent(0, 0, '', fckBackendError, fwsError,
          'Unable to start inotify worker: ' + E.Message));
      end;
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TLinuxFileChangeSource.Stop;
var
  I: Integer;
begin
  EnterCriticalSection(FLock);
  try
    if not FStarted then
    begin
      SetStatus(fwsStopped);
      Exit;
    end;
    FStopping := True;
  finally
    LeaveCriticalSection(FLock);
  end;

  WakeWorker;
  if Assigned(FWorker) then
  begin
    FWorker.WaitFor;
    FWorker.Free;
    FWorker := nil;
  end;

  EnterCriticalSection(FLock);
  try
    if FWakeReadFD >= 0 then c_close(FWakeReadFD);
    if FWakeWriteFD >= 0 then c_close(FWakeWriteFD);
    if FInotifyFD >= 0 then c_close(FInotifyFD);
    FWakeReadFD := -1;
    FWakeWriteFD := -1;
    FInotifyFD := -1;
    FStarted := False;
    FStopping := False;
    FTerminalError := False;
    for I := 0 to FSubscriptions.Count - 1 do
      TObject(FSubscriptions[I]).Free;
    FSubscriptions.Clear;
    SetStatus(fwsStopped);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TLinuxFileChangeSource.Subscribe(const Path: string;
  Generation: QWord; out SubscriptionID: QWord): TFileWatchStatus;
var
  I: Integer;
  Item: TWatchSubscription;
  NormalizedPath, ParentPath: string;
  WatchDescriptor, WatchError: LongInt;
begin
  SubscriptionID := 0;
  NormalizedPath := NormalizeFileWatchPath(Path);
  if (NormalizedPath = '') or (ExtractFileName(NormalizedPath) = '') then
    Exit(fwsError);

  EnterCriticalSection(FLock);
  try
    if not FStarted or FStopping or FTerminalError then Exit(Status);
    Item := TWatchSubscription.Create;
    Item.ID := AllocateSubscriptionID;
    Item.Generation := Generation;
    Item.Path := NormalizedPath;
    Item.ParentPath := ExtractFileDir(NormalizedPath);
    if Item.ParentPath = '' then Item.ParentPath := PathDelim;
    Item.BaseName := ExtractFileName(NormalizedPath);
    Item.Present := FileExists(NormalizedPath);
    Item.WatchDescriptor := -1;
    ParentPath := Item.ParentPath;
    WatchDescriptor := AddDirectoryWatch(ParentPath, WatchError);
    if WatchDescriptor >= 0 then Item.WatchDescriptor := WatchDescriptor;
    FSubscriptions.Add(Item);
    SubscriptionID := Item.ID;
    if WatchDescriptor >= 0 then
    begin
      for I := 0 to FSubscriptions.Count - 2 do
        if (TWatchSubscription(FSubscriptions[I]).WatchDescriptor < 0) and
          (TWatchSubscription(FSubscriptions[I]).ParentPath = Item.ParentPath) then
          TWatchSubscription(FSubscriptions[I]).WatchDescriptor := WatchDescriptor;
      UpdateWatchStatus;
      Result := Status;
    end
    else
    begin
      SetStatus(fwsDegraded);
      Result := fwsDegraded;
      PublishEvent(MakeEvent(Item.ID, Item.Generation, Item.Path,
        fckBackendError, fwsDegraded,
        ErrorText('inotify_add_watch ' + ParentPath, WatchError)));
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
  WakeWorker;
end;

procedure TLinuxFileChangeSource.Unsubscribe(SubscriptionID: QWord);
var
  I, J, WatchDescriptor: Integer;
  Item, Other: TWatchSubscription;
  StillUsed: Boolean;
begin
  if SubscriptionID = 0 then Exit;
  EnterCriticalSection(FLock);
  try
    for I := FSubscriptions.Count - 1 downto 0 do
    begin
      Item := TWatchSubscription(FSubscriptions[I]);
      if Item.ID <> SubscriptionID then Continue;
      WatchDescriptor := Item.WatchDescriptor;
      FSubscriptions.Delete(I);
      Item.Free;
      StillUsed := False;
      if WatchDescriptor >= 0 then
        for J := 0 to FSubscriptions.Count - 1 do
        begin
          Other := TWatchSubscription(FSubscriptions[J]);
          if Other.WatchDescriptor = WatchDescriptor then
          begin
            StillUsed := True;
            Break;
          end;
        end;
      if (not StillUsed) and (WatchDescriptor >= 0) and (FInotifyFD >= 0) then
        c_inotify_rm_watch(FInotifyFD, WatchDescriptor);
      UpdateWatchStatus;
      Break;
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
  WakeWorker;
end;

procedure TLinuxFileChangeSource.ProcessBuffer(Buffer: PByte; Count: LongInt);
var
  Offset, NameLength: LongInt;
  Header: TInotifyEventHeader;
  Name: string;
begin
  Offset := 0;
  while Offset < Count do
  begin
    if Count - Offset < SizeOf(TInotifyEventHeader) then
    begin
      PublishInvalidation('Malformed or truncated inotify event header');
      Exit;
    end;
    Move((Buffer + Offset)^, Header, SizeOf(Header));
    Inc(Offset, SizeOf(Header));
    if (Header.NameLength > 4096) or
      (Header.NameLength > LongWord(Count - Offset)) then
    begin
      PublishInvalidation('Malformed or truncated inotify event name');
      Exit;
    end;
    NameLength := Header.NameLength;
    Name := '';
    if NameLength > 0 then
    begin
      SetString(Name, PChar(Buffer + Offset), NameLength);
      NameLength := Pos(#0, Name);
      if NameLength > 0 then SetLength(Name, NameLength - 1);
    end;
    Inc(Offset, Header.NameLength);
    ProcessEvent(Header.WatchDescriptor, Header.Mask, Header.Cookie, Name);
  end;
end;

procedure TLinuxFileChangeSource.ProcessEvent(WatchDescriptor: LongInt;
  Mask: LongWord; Cookie: LongWord; const Name: string);
var
  I: Integer;
  Item: TWatchSubscription;
  Kind: TFileChangeKind;
  NewWatch, WatchError: LongInt;
  WatchLost, Degraded, ActiveWatch: Boolean;
  ErrorMessage: string;
begin
  if (Mask and InQueueOverflow) <> 0 then
  begin
    PublishInvalidation('inotify event queue overflow');
    Exit;
  end;

  EnterCriticalSection(FLock);
  try
    WatchLost := (Mask and (InIgnored or InDeleteSelf or InMoveSelf)) <> 0;
    if WatchLost then
    begin
      ActiveWatch := False;
      for I := 0 to FSubscriptions.Count - 1 do
        if TWatchSubscription(FSubscriptions[I]).WatchDescriptor =
          WatchDescriptor then
        begin
          ActiveWatch := True;
          Break;
        end;
      if not ActiveWatch then Exit;
      Degraded := False;
      for I := 0 to FSubscriptions.Count - 1 do
      begin
        Item := TWatchSubscription(FSubscriptions[I]);
        if Item.WatchDescriptor <> WatchDescriptor then Continue;
        Item.WatchDescriptor := -1;
        NewWatch := AddDirectoryWatch(Item.ParentPath, WatchError);
        if NewWatch >= 0 then Item.WatchDescriptor := NewWatch
        else Degraded := True;
      end;
      for I := 0 to FSubscriptions.Count - 1 do
        if TWatchSubscription(FSubscriptions[I]).WatchDescriptor < 0 then
          Degraded := True;
      if Degraded then SetStatus(fwsDegraded)
      else if FStarted then SetStatus(fwsReady);
      if Degraded then
        ErrorMessage := 'A watched parent directory was lost and could not be re-armed'
      else
        ErrorMessage := 'A watched parent directory was invalidated and re-armed';
      PublishInvalidation(ErrorMessage);
      if (Mask and InIgnored) <> 0 then Exit;
    end;

    if Name <> '' then
      for I := 0 to FSubscriptions.Count - 1 do
      begin
        Item := TWatchSubscription(FSubscriptions[I]);
        if (Item.WatchDescriptor <> WatchDescriptor) or
          (Item.BaseName <> Name) then Continue;

        if (Mask and (InMovedFrom or InDelete)) <> 0 then
        begin
          if Item.Present then
          begin
            Item.Present := False;
            PublishEvent(MakeEvent(Item.ID, Item.Generation, Item.Path,
              fckDisappeared, Status));
          end;
        end;

        if (Mask and (InMovedTo or InCreate)) <> 0 then
        begin
          if not Item.Present then Kind := fckReappeared
          else if (Mask and InMovedTo) <> 0 then Kind := fckReplaced
          else Kind := fckMetadataChanged;
          Item.Present := True;
          PublishEvent(MakeEvent(Item.ID, Item.Generation, Item.Path,
            Kind, Status));
        end;

        if (Mask and (InModify or InCloseWrite)) <> 0 then
          PublishEvent(MakeEvent(Item.ID, Item.Generation, Item.Path,
            fckContentChanged, Status));
        if (Mask and InAttrib) <> 0 then
          PublishEvent(MakeEvent(Item.ID, Item.Generation, Item.Path,
            fckMetadataChanged, Status));
      end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TLinuxFileChangeSource.RunWorker;
var
  FDs: array[0..1] of TPollFD;
  Buffer: array[0..65535] of Byte;
  WakeBytes: array[0..255] of Byte;
  PollResult: LongInt;
  ReadCount: PtrInt;
  ErrorCode: LongInt;
begin
  FDs[0].FD := FInotifyFD;
  FDs[0].Events := PollIn;
  FDs[0].ReturnedEvents := 0;
  FDs[1].FD := FWakeReadFD;
  FDs[1].Events := PollIn;
  FDs[1].ReturnedEvents := 0;
  while not ReadStopping do
  begin
    PollResult := c_poll(@FDs[0], 2, -1);
    if PollResult < 0 then
    begin
      ErrorCode := LastErrno;
      if ErrorCode = ErrInterrupted then Continue;
      FailWorker(ErrorText('poll', ErrorCode));
      Exit;
    end;
    EnterCriticalSection(FLock);
    try
      Inc(FPollReturns);
    finally
      LeaveCriticalSection(FLock);
    end;
    if (FDs[0].ReturnedEvents and (PollErr or PollHup or PollNval)) <> 0 then
    begin
      FailWorker('inotify descriptor became unusable');
      Exit;
    end;
    if (FDs[1].ReturnedEvents and (PollIn or PollHup or PollErr)) <> 0 then
    begin
      repeat
        ReadCount := c_read(FWakeReadFD, @WakeBytes[0], SizeOf(WakeBytes));
      until ReadCount <= 0;
      if ReadStopping then Exit;
    end;
    if (FDs[0].ReturnedEvents and PollIn) <> 0 then
      repeat
        ReadCount := c_read(FInotifyFD, @Buffer[0], SizeOf(Buffer));
        if ReadCount > 0 then ProcessBuffer(@Buffer[0], ReadCount)
        else if (ReadCount < 0) and (LastErrno = ErrInterrupted) then Continue
        else if (ReadCount < 0) and (LastErrno = ErrAgain) then Break
        else if ReadCount < 0 then
        begin
          ErrorCode := LastErrno;
          FailWorker(ErrorText('read inotify descriptor', ErrorCode));
          Exit;
        end;
      until ReadCount <= 0;
    FDs[0].ReturnedEvents := 0;
    FDs[1].ReturnedEvents := 0;
  end;
end;

{$IFDEF TPX_FILEWATCH_TESTS}
procedure TLinuxFileChangeSource.InjectBufferForTest(Buffer: PByte;
  Count: LongInt);
begin
  ProcessBuffer(Buffer, Count);
end;

procedure TLinuxFileChangeSource.InjectOverflowForTest;
begin
  ProcessEvent(-1, InQueueOverflow, 0, '');
  ProcessEvent(-1, InQueueOverflow, 0, '');
  ProcessEvent(-1, InQueueOverflow, 0, '');
end;

procedure TLinuxFileChangeSource.InjectQueueOverflowForTest;
var
  I: Integer;
begin
  for I := 1 to FileWatchQueueCapacity + 1 do
    PublishEvent(MakeEvent(1, 41, '', fckContentChanged, Status));
end;

procedure TLinuxFileChangeSource.InjectWatchLossForTest(
  SubscriptionID: QWord);
var
  I: Integer;
  WatchDescriptor: LongInt;
begin
  WatchDescriptor := -1;
  EnterCriticalSection(FLock);
  try
    for I := 0 to FSubscriptions.Count - 1 do
      if TWatchSubscription(FSubscriptions[I]).ID = SubscriptionID then
      begin
        WatchDescriptor := TWatchSubscription(FSubscriptions[I]).WatchDescriptor;
        Break;
      end;
  finally
    LeaveCriticalSection(FLock);
  end;
  if WatchDescriptor >= 0 then ProcessEvent(WatchDescriptor, InIgnored, 0, '');
end;

function TLinuxFileChangeSource.PollReturnsForTest: QWord;
begin
  EnterCriticalSection(FLock);
  try
    Result := FPollReturns;
  finally
    LeaveCriticalSection(FLock);
  end;
end;
{$ENDIF}

function CreateLinuxFileChangeSource: TFileChangeSource;
begin
  Result := TLinuxFileChangeSource.Create;
end;

initialization
  RegisterFileChangeSourceFactory(@CreateLinuxFileChangeSource);

{$ENDIF}

end.

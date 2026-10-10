unit FileWatch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$IFNDEF CPUWASM32}, SyncObjs{$ENDIF};

const
  FileWatchQueueCapacity = 256;

type
  TFileWatchStatus = (fwsStopped, fwsStarting, fwsReady, fwsDegraded,
    fwsUnsupported, fwsError);

  TFileChangeKind = (fckReady, fckContentChanged, fckReplaced,
    fckDisappeared, fckReappeared, fckMetadataChanged,
    fckRescanRequired, fckBackendError);

  TFileChangeEvent = record
    SubscriptionID: QWord;
    Generation: QWord;
    Path: string;
    Kind: TFileChangeKind;
    Status: TFileWatchStatus;
    ErrorText: string;
  end;

  TFileChangeSource = class;
  TFileChangeSourceFactory = function: TFileChangeSource;

  { Platform adapters publish copied event records. The queue never calls a
    document or UI object, and its overflow signal asks the consumer to rescan
    its own currently tracked sources. }
  TFileChangeSource = class
  private
    FQueue: array[0..FileWatchQueueCapacity - 1] of TFileChangeEvent;
    FQueueHead: Integer;
    FQueueCount: Integer;
    FOverflowPending: Boolean;
    FRescanPending: Boolean;
    {$IFNDEF CPUWASM32}
    FQueueLock: TCriticalSection;
    FQueueReady: TEvent;
    FStatusLock: TCriticalSection;
    FReady: TEvent;
    {$ENDIF}
    FStatus: TFileWatchStatus;
    FNextSubscriptionID: QWord;
    function ReadStatus: TFileWatchStatus;
    procedure LockQueue;
    procedure UnlockQueue;
    procedure LockStatus;
    procedure UnlockStatus;
  protected
    procedure PublishEvent(const Event: TFileChangeEvent);
    procedure SetStatus(Status: TFileWatchStatus);
    function AllocateSubscriptionID: QWord;
    class function MakeEvent(SubscriptionID, Generation: QWord;
      const Path: string; Kind: TFileChangeKind; Status: TFileWatchStatus;
      const ErrorText: string = ''): TFileChangeEvent; static;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Start; virtual; abstract;
    function WaitUntilReady(TimeoutMS: LongInt): TFileWatchStatus;
    procedure Stop; virtual; abstract;
    function Subscribe(const Path: string; Generation: QWord;
      out SubscriptionID: QWord): TFileWatchStatus; virtual; abstract;
    procedure Unsubscribe(SubscriptionID: QWord); virtual; abstract;

    function TryDequeue(out Event: TFileChangeEvent): Boolean;
    function WaitForEvent(TimeoutMS: LongInt): Boolean;
    property Status: TFileWatchStatus read ReadStatus;
  end;

procedure RegisterFileChangeSourceFactory(Factory: TFileChangeSourceFactory);
function CreateFileChangeSource: TFileChangeSource;
function NormalizeFileWatchPath(const Path: string): string;

implementation

type
  TUnavailableFileChangeSource = class(TFileChangeSource)
  public
    procedure Start; override;
    procedure Stop; override;
    function Subscribe(const Path: string; Generation: QWord;
      out SubscriptionID: QWord): TFileWatchStatus; override;
    procedure Unsubscribe(SubscriptionID: QWord); override;
  end;

{$IFNDEF CPUWASM32}
function WaitOnEvent(Event: TEvent; TimeoutMS: LongInt): TWaitResult;
begin
  if TimeoutMS < 0 then
    Result := Event.WaitFor(High(Cardinal))
  else
    Result := Event.WaitFor(Cardinal(TimeoutMS));
end;
{$ENDIF}

var
  RegisteredFactory: TFileChangeSourceFactory = nil;

constructor TFileChangeSource.Create;
begin
  inherited Create;
  {$IFNDEF CPUWASM32}
  FQueueLock := TCriticalSection.Create;
  FQueueReady := TEvent.Create(nil, True, False, '');
  FStatusLock := TCriticalSection.Create;
  FReady := TEvent.Create(nil, True, False, '');
  {$ENDIF}
  FStatus := fwsStopped;
end;

destructor TFileChangeSource.Destroy;
begin
  {$IFNDEF CPUWASM32}
  FReady.Free;
  FStatusLock.Free;
  FQueueReady.Free;
  FQueueLock.Free;
  {$ENDIF}
  inherited Destroy;
end;

procedure TFileChangeSource.LockQueue;
begin
  {$IFNDEF CPUWASM32}FQueueLock.Acquire;{$ENDIF}
end;

procedure TFileChangeSource.UnlockQueue;
begin
  {$IFNDEF CPUWASM32}FQueueLock.Release;{$ENDIF}
end;

procedure TFileChangeSource.LockStatus;
begin
  {$IFNDEF CPUWASM32}FStatusLock.Acquire;{$ENDIF}
end;

procedure TFileChangeSource.UnlockStatus;
begin
  {$IFNDEF CPUWASM32}FStatusLock.Release;{$ENDIF}
end;

function TFileChangeSource.ReadStatus: TFileWatchStatus;
begin
  LockStatus;
  try
    Result := FStatus;
  finally
    UnlockStatus;
  end;
end;

procedure TFileChangeSource.SetStatus(Status: TFileWatchStatus);
var
  WakeWaiters: Boolean;
begin
  WakeWaiters := False;
  LockStatus;
  try
    WakeWaiters := (Status = fwsStopped) and (FStatus <> fwsStopped);
    FStatus := Status;
    {$IFNDEF CPUWASM32}
    if Status = fwsStarting then
      FReady.ResetEvent;
    if Status in [fwsReady, fwsDegraded, fwsUnsupported, fwsError,
      fwsStopped] then
      FReady.SetEvent;
    {$ENDIF}
  finally
    UnlockStatus;
  end;
  if WakeWaiters then
  begin
    LockQueue;
    try
      {$IFNDEF CPUWASM32}
      FQueueReady.SetEvent;
      {$ENDIF}
    finally
      UnlockQueue;
    end;
  end;
end;

function TFileChangeSource.WaitUntilReady(TimeoutMS: LongInt): TFileWatchStatus;
begin
  {$IFDEF CPUWASM32}
  Result := ReadStatus;
  {$ELSE}
  Result := ReadStatus;
  if Result <> fwsStarting then Exit;
  WaitOnEvent(FReady, TimeoutMS);
  Result := ReadStatus;
  {$ENDIF}
end;

procedure TFileChangeSource.PublishEvent(const Event: TFileChangeEvent);
var
  Tail: Integer;
  GlobalRescan: Boolean;
begin
  GlobalRescan := (Event.Kind = fckRescanRequired) and
    (Event.SubscriptionID = 0);
  LockQueue;
  try
    if FQueueCount = FileWatchQueueCapacity then
    begin
      FQueueHead := 0;
      FQueueCount := 1;
      FQueue[0] := MakeEvent(0, 0, '', fckRescanRequired, ReadStatus,
        'Watcher event queue overflow; rescan tracked sources');
      FOverflowPending := True;
      FRescanPending := True;
    end
    else if FOverflowPending and (FQueueCount > 0) and
      (FQueue[FQueueHead].Kind = fckRescanRequired) then
    begin
      { Keep the single overflow hint until the consumer takes it. }
    end
    else if GlobalRescan and FRescanPending then
    begin
      { A pending global invalidation already asks the consumer to rescan. }
    end
    else
    begin
      Tail := (FQueueHead + FQueueCount) mod FileWatchQueueCapacity;
      FQueue[Tail] := Event;
      Inc(FQueueCount);
      if GlobalRescan then FRescanPending := True;
    end;
    {$IFNDEF CPUWASM32}FQueueReady.SetEvent;{$ENDIF}
  finally
    UnlockQueue;
  end;
end;

function TFileChangeSource.TryDequeue(out Event: TFileChangeEvent): Boolean;
begin
  LockQueue;
  try
    Result := FQueueCount > 0;
    if Result then
    begin
      Event := FQueue[FQueueHead];
      FQueueHead := (FQueueHead + 1) mod FileWatchQueueCapacity;
      Dec(FQueueCount);
      if (Event.Kind = fckRescanRequired) and
        (Event.SubscriptionID = 0) then
      begin
        FOverflowPending := False;
        FRescanPending := False;
      end;
    end;
    {$IFNDEF CPUWASM32}
    if FQueueCount = 0 then FQueueReady.ResetEvent;
    {$ENDIF}
  finally
    UnlockQueue;
  end;
end;

function TFileChangeSource.WaitForEvent(TimeoutMS: LongInt): Boolean;
begin
  if Status = fwsStopped then Exit(True);
  {$IFDEF CPUWASM32}
  LockQueue;
  try
    Result := FQueueCount > 0;
  finally
    UnlockQueue;
  end;
  {$ELSE}
  Result := WaitOnEvent(FQueueReady, TimeoutMS) = wrSignaled;
  if not Result and (Status = fwsStopped) then Result := True;
  {$ENDIF}
end;

function TFileChangeSource.AllocateSubscriptionID: QWord;
begin
  LockStatus;
  try
    Inc(FNextSubscriptionID);
    if FNextSubscriptionID = 0 then Inc(FNextSubscriptionID);
    Result := FNextSubscriptionID;
  finally
    UnlockStatus;
  end;
end;

class function TFileChangeSource.MakeEvent(SubscriptionID, Generation: QWord;
  const Path: string; Kind: TFileChangeKind; Status: TFileWatchStatus;
  const ErrorText: string): TFileChangeEvent;
begin
  Result.SubscriptionID := SubscriptionID;
  Result.Generation := Generation;
  Result.Path := Path;
  Result.Kind := Kind;
  Result.Status := Status;
  Result.ErrorText := ErrorText;
end;

procedure TUnavailableFileChangeSource.Start;
begin
  SetStatus(fwsUnsupported);
  PublishEvent(MakeEvent(0, 0, '', fckReady, fwsUnsupported,
    'No native file-change backend is available on this platform'));
end;

procedure TUnavailableFileChangeSource.Stop;
begin
  SetStatus(fwsStopped);
end;

function TUnavailableFileChangeSource.Subscribe(const Path: string;
  Generation: QWord; out SubscriptionID: QWord): TFileWatchStatus;
begin
  SubscriptionID := 0;
  Result := fwsUnsupported;
end;

procedure TUnavailableFileChangeSource.Unsubscribe(SubscriptionID: QWord);
begin
end;

procedure RegisterFileChangeSourceFactory(Factory: TFileChangeSourceFactory);
begin
  RegisteredFactory := Factory;
end;

function CreateFileChangeSource: TFileChangeSource;
begin
  if Assigned(RegisteredFactory) then Result := RegisteredFactory()
  else Result := TUnavailableFileChangeSource.Create;
end;

function NormalizeFileWatchPath(const Path: string): string;
begin
  if Path = '' then Exit('');
  Result := ExpandFileName(Path);
  while (Length(Result) > 1) and (Result[Length(Result)] = PathDelim) do
    Delete(Result, Length(Result), 1);
end;

end.

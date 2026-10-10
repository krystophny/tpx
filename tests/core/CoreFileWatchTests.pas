unit CoreFileWatchTests;

{$mode delphi}{$H+}

interface

implementation

uses Classes, SyncObjs, SysUtils, FileWatch, CoreTestSupport;

type
  TFakeFileChangeSource = class(TFileChangeSource)
  private
    FStarted: Boolean;
  public
    procedure Start; override;
    procedure Stop; override;
    function Subscribe(const Path: string; Generation: QWord;
      out SubscriptionID: QWord): TFileWatchStatus; override;
    procedure Unsubscribe(SubscriptionID: QWord); override;
    procedure Emit(const Event: TFileChangeEvent);
    procedure BeginStarting;
    procedure FinishStarting;
  end;

  TSourceWaiter = class(TThread)
  private
    FSource: TFileChangeSource;
    FWaitForReady: Boolean;
    FEntered: TEvent;
    FFinished: TEvent;
  public
    WaitResult: Boolean;
    WaitStatus: TFileWatchStatus;
    constructor Create(Source: TFileChangeSource; WaitForReady: Boolean;
      Entered, Finished: TEvent);
    procedure Execute; override;
  end;

procedure TFakeFileChangeSource.Start;
begin
  BeginStarting;
  FinishStarting;
  PublishEvent(MakeEvent(0, 0, '', fckReady, fwsReady));
end;

procedure TFakeFileChangeSource.BeginStarting;
begin
  SetStatus(fwsStarting);
  FStarted := True;
end;

procedure TFakeFileChangeSource.FinishStarting;
begin
  SetStatus(fwsReady);
end;

procedure TFakeFileChangeSource.Stop;
begin
  FStarted := False;
  SetStatus(fwsStopped);
end;

function TFakeFileChangeSource.Subscribe(const Path: string;
  Generation: QWord; out SubscriptionID: QWord): TFileWatchStatus;
begin
  SubscriptionID := 0;
  if not FStarted then Exit(Status);
  SubscriptionID := AllocateSubscriptionID;
  Result := Status;
end;

procedure TFakeFileChangeSource.Unsubscribe(SubscriptionID: QWord);
begin
end;

procedure TFakeFileChangeSource.Emit(const Event: TFileChangeEvent);
begin
  PublishEvent(Event);
end;

constructor TSourceWaiter.Create(Source: TFileChangeSource;
  WaitForReady: Boolean; Entered, Finished: TEvent);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FSource := Source;
  FWaitForReady := WaitForReady;
  FEntered := Entered;
  FFinished := Finished;
end;

procedure TSourceWaiter.Execute;
begin
  FEntered.SetEvent;
  if FWaitForReady then
    WaitStatus := FSource.WaitUntilReady(-1)
  else
    WaitResult := FSource.WaitForEvent(-1);
  FFinished.SetEvent;
end;

procedure CheckWaiterStarted(Waiter: TSourceWaiter; Entered: TEvent;
  const Description: string);
begin
  Waiter.Start;
  CheckCore(Entered.WaitFor(3000) = wrSignaled,
    Description + ': waiter thread did not start');
end;

procedure TestFileWatchQueueAndLifecycle;
var
  Source: TFakeFileChangeSource;
  Event: TFileChangeEvent;
  SubscriptionID: QWord;
  I: Integer;
  Entered, Finished: TEvent;
  Waiter: TSourceWaiter;
begin
  Source := TFakeFileChangeSource.Create;
  try
    Entered := TEvent.Create(nil, True, False, '');
    Finished := TEvent.Create(nil, True, False, '');
    try
      CheckCore(Source.WaitUntilReady(0) = fwsStopped,
        'readiness query on a stopped source must return immediately');
      Source.Start;
      CheckCore(Source.WaitUntilReady(-1) = fwsReady,
        'ready source returned an incorrect readiness state');
      CheckCore(Source.Subscribe('/tmp/source.tpx', 41, SubscriptionID) = fwsReady,
        'ready source rejected a subscription');
      CheckCore(SubscriptionID <> 0, 'subscription IDs must be nonzero');

      while Source.TryDequeue(Event) do ;
      Event := Default(TFileChangeEvent);
      Event.SubscriptionID := SubscriptionID;
      Event.Generation := 41;
      Event.Path := '/tmp/source.tpx';
      Event.Kind := fckContentChanged;
      Event.Status := fwsReady;
      Event.ErrorText := 'copied detail';
      Source.Emit(Event);
      Event.Path := '/tmp/changed-after-publish.tpx';
      Event.ErrorText := 'mutated after publish';
      CheckCore(Source.WaitForEvent(0), 'published event did not signal its queue');
      CheckCore(Source.TryDequeue(Event), 'published event was not dequeued');
      CheckCore((Event.SubscriptionID = SubscriptionID) and
        (Event.Generation = 41) and (Event.Path = '/tmp/source.tpx') and
        (Event.ErrorText = 'copied detail') and
        (Event.Kind = fckContentChanged) and (Event.Status = fwsReady),
        'queue did not preserve the immutable event record');
      CheckCore(not Source.TryDequeue(Event), 'queue retained an already-taken event');

      for I := 1 to FileWatchQueueCapacity + 10 do
      begin
        Event := Default(TFileChangeEvent);
        Event.SubscriptionID := SubscriptionID;
        Event.Generation := 41;
        Event.Path := '/tmp/source.tpx';
        Event.Kind := fckContentChanged;
        Event.Status := fwsReady;
        Source.Emit(Event);
      end;
      CheckCore(Source.TryDequeue(Event), 'overflow did not leave a rescan hint');
      CheckCore((Event.Kind = fckRescanRequired) and
        (Event.SubscriptionID = 0) and (Event.Generation = 0) and
        (Event.Path = ''), 'overflow must collapse hints to one global rescan');
      CheckCore(not Source.TryDequeue(Event),
        'queue overflow must coalesce all later hints until rescan is consumed');
      Event := Default(TFileChangeEvent);
      Event.Kind := fckRescanRequired;
      Event.Status := fwsReady;
      for I := 1 to 3 do Source.Emit(Event);
      CheckCore(Source.TryDequeue(Event) and
        (Event.Kind = fckRescanRequired),
        'backend invalidation did not publish a global rescan');
      CheckCore(not Source.TryDequeue(Event),
        'pending backend invalidations must coalesce to one global rescan');

      Source.BeginStarting;
      Waiter := TSourceWaiter.Create(Source, True, Entered, Finished);
      try
        CheckWaiterStarted(Waiter, Entered, 'ready notification');
        CheckCore(Finished.WaitFor(50) = wrTimeout,
          'readiness wait returned before the source left starting state');
        Source.FinishStarting;
        CheckCore(Finished.WaitFor(3000) = wrSignaled,
          'ready notification did not wake an indefinite waiter');
        Waiter.WaitFor;
        CheckCore(Waiter.WaitStatus = fwsReady,
          'ready waiter observed the wrong source status');
      finally
        Waiter.Free;
      end;

      Entered.ResetEvent;
      Finished.ResetEvent;
      Source.BeginStarting;
      Waiter := TSourceWaiter.Create(Source, True, Entered, Finished);
      try
        CheckWaiterStarted(Waiter, Entered, 'stopped readiness notification');
        CheckCore(Finished.WaitFor(50) = wrTimeout,
          'readiness wait returned before Stop changed the source state');
        Source.Stop;
        CheckCore(Finished.WaitFor(3000) = wrSignaled,
          'Stop did not wake an indefinite readiness waiter');
        Waiter.WaitFor;
        CheckCore(Waiter.WaitStatus = fwsStopped,
          'readiness waiter did not observe stopped status');
      finally
        Waiter.Free;
      end;

      Source.Start;
      while Source.TryDequeue(Event) do ;
      Entered.ResetEvent;
      Finished.ResetEvent;
      Waiter := TSourceWaiter.Create(Source, False, Entered, Finished);
      try
        CheckWaiterStarted(Waiter, Entered, 'stopped event notification');
        CheckCore(Finished.WaitFor(50) = wrTimeout,
          'event wait returned before an event or Stop was published');
        Source.Stop;
        CheckCore(Finished.WaitFor(3000) = wrSignaled,
          'Stop did not wake an indefinite event waiter');
        Waiter.WaitFor;
        CheckCore(Waiter.WaitResult,
          'event waiter did not report the terminal wake');
      finally
        Waiter.Free;
      end;

      Source.Start;
      CheckCore(Source.WaitUntilReady(0) = fwsReady,
        'readiness must be re-armed after restart');
    finally
      Finished.Free;
      Entered.Free;
    end;
  finally
    Source.Free;
  end;
end;

initialization
  RegisterCoreTest('filewatch-common-queue-and-lifecycle',
    TestFileWatchQueueAndLifecycle);

end.

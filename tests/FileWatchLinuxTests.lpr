program FileWatchLinuxTests;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, SyncObjs, FileWatch, FileWatchLinux;

type
  TWaiter = class(TThread)
  private
    FSource: TFileChangeSource;
    FEntered: TEvent;
    FFinished: TEvent;
  public
    WaitResult: Boolean;
    constructor Create(Source: TFileChangeSource; Entered, DoneEvent: TEvent);
    procedure Execute; override;
  end;

constructor TWaiter.Create(Source: TFileChangeSource; Entered,
  DoneEvent: TEvent);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FSource := Source;
  FEntered := Entered;
  FFinished := DoneEvent;
end;

procedure TWaiter.Execute;
begin
  FEntered.SetEvent;
  WaitResult := FSource.WaitForEvent(-1);
  FFinished.SetEvent;
end;

function KindName(Kind: TFileChangeKind): string;
begin
  case Kind of
    fckReady: Result := 'ready';
    fckContentChanged: Result := 'content';
    fckReplaced: Result := 'replaced';
    fckDisappeared: Result := 'disappeared';
    fckReappeared: Result := 'reappeared';
    fckMetadataChanged: Result := 'metadata';
    fckRescanRequired: Result := 'rescan';
    fckBackendError: Result := 'error';
  end;
end;

function StatusName(Status: TFileWatchStatus): string;
begin
  case Status of
    fwsStopped: Result := 'stopped';
    fwsStarting: Result := 'starting';
    fwsReady: Result := 'ready';
    fwsDegraded: Result := 'degraded';
    fwsUnsupported: Result := 'unsupported';
    fwsError: Result := 'error';
  end;
end;

function PopPart(var Text: string): string;
var
  Separator: SizeInt;
begin
  Separator := Pos('|', Text);
  if Separator = 0 then
  begin
    Result := Text;
    Text := '';
  end
  else
  begin
    Result := Copy(Text, 1, Separator - 1);
    Delete(Text, 1, Separator);
  end;
end;

procedure DrainEvents(Source: TFileChangeSource; TimeoutMS: LongInt);
var
  Event: TFileChangeEvent;
begin
  if not Source.WaitForEvent(TimeoutMS) then
  begin
    WriteLn('EMPTY');
    Flush(Output);
    Exit;
  end;
  while Source.TryDequeue(Event) do
  begin
    WriteLn('EVENT|', KindName(Event.Kind), '|', Event.SubscriptionID, '|',
      Event.Generation, '|', StatusName(Event.Status), '|', Event.Path, '|',
      Event.ErrorText);
    Flush(Output);
  end;
  WriteLn('DRAIN_DONE');
  Flush(Output);
end;

var
  Source: TLinuxFileChangeSource;
  Event: TFileChangeEvent;
  Path, Command, Argument, Rest: string;
  SubscriptionID, NewID, Generation: QWord;
  WaitMS, I, RestartCount: LongInt;
  SubscribeStatus: TFileWatchStatus;
  Entered, Finished: TEvent;
  Waiter: TWaiter;
begin
  if ParamCount <> 1 then Halt(2);
  Path := ParamStr(1);
  Source := TLinuxFileChangeSource.Create;
  try
    Source.Start;
    if Source.WaitUntilReady(5000) <> fwsReady then Halt(3);
    if Source.Subscribe(Path, 41, SubscriptionID) <> fwsReady then Halt(4);
    while Source.TryDequeue(Event) do ;
    WriteLn('READY|', SubscriptionID, '|41|', StatusName(Source.Status));
    Flush(Output);

    while not EOF(Input) do
    begin
      ReadLn(Command);
      Rest := Command;
      Argument := PopPart(Rest);
      if Argument = 'DRAIN' then
      begin
        WaitMS := 1500;
        if Rest <> '' then WaitMS := StrToIntDef(Rest, WaitMS);
        DrainEvents(Source, WaitMS);
      end
      else if Argument = 'SUB' then
      begin
        Generation := StrToQWordDef(Rest, 0);
        SubscribeStatus := Source.Subscribe(Path, Generation, NewID);
        WriteLn('SUB|', StatusName(SubscribeStatus), '|', NewID, '|', Generation);
        Flush(Output);
      end
      else if Argument = 'SUBPATH' then
      begin
        Generation := StrToQWordDef(PopPart(Rest), 0);
        SubscribeStatus := Source.Subscribe(Rest, Generation, NewID);
        WriteLn('SUB|', StatusName(SubscribeStatus), '|', NewID, '|', Generation);
        Flush(Output);
      end
      else if Argument = 'UNSUB' then
      begin
        SubscriptionID := StrToQWordDef(Rest, 0);
        Source.Unsubscribe(SubscriptionID);
        WriteLn('UNSUB|', SubscriptionID);
        Flush(Output);
      end
      else if Argument = 'INJECT_BAD' then
      begin
        Source.InjectBufferForTest(@Argument[1], 1);
        WriteLn('INJECTED');
        Flush(Output);
      end
      else if Argument = 'INJECT_OVERFLOW' then
      begin
        Source.InjectOverflowForTest;
        WriteLn('INJECTED');
        Flush(Output);
      end
      else if Argument = 'INJECT_QUEUE_OVERFLOW' then
      begin
        Source.InjectQueueOverflowForTest;
        WriteLn('INJECTED');
        Flush(Output);
      end
      else if Argument = 'INJECT_LOSS' then
      begin
        SubscriptionID := StrToQWordDef(Rest, 0);
        Source.InjectWatchLossForTest(SubscriptionID);
        WriteLn('INJECTED');
        Flush(Output);
      end
      else if Argument = 'POLL_COUNT' then
      begin
        WriteLn('POLL_COUNT|', Source.PollReturnsForTest);
        Flush(Output);
      end
      else if Argument = 'RESTARTS' then
      begin
        RestartCount := StrToIntDef(Rest, 0);
        for I := 1 to RestartCount do
        begin
          Source.Stop;
          Source.Start;
          if Source.WaitUntilReady(5000) <> fwsReady then Halt(5);
          if Source.Subscribe(Path, 41, NewID) <> fwsReady then Halt(6);
          while Source.TryDequeue(Event) do ;
        end;
        WriteLn('RESTARTED|', RestartCount);
        Flush(Output);
      end
      else if Argument = 'STOP_WAIT' then
      begin
        Entered := TEvent.Create(nil, True, False, '');
        Finished := TEvent.Create(nil, True, False, '');
        Waiter := TWaiter.Create(Source, Entered, Finished);
        try
          Waiter.Start;
          if Entered.WaitFor(3000) <> wrSignaled then Halt(7);
          if Finished.WaitFor(50) <> wrTimeout then Halt(11);
          Source.Stop;
          if Finished.WaitFor(3000) <> wrSignaled then Halt(8);
          Waiter.WaitFor;
          if not Waiter.WaitResult then Halt(9);
          WriteLn('STOPPED|waiter-awake');
          Flush(Output);
        finally
          Waiter.Free;
          Entered.Free;
          Finished.Free;
        end;
        Break;
      end
      else if Argument = 'STOP' then
      begin
        Source.Stop;
        WriteLn('STOPPED');
        Flush(Output);
        Break;
      end
      else
        Halt(10);
    end;
  finally
    Source.Free;
  end;
end.

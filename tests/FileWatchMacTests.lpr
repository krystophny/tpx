program FileWatchMacTests;

{$mode objfpc}{$H+}

uses
  cthreads, Classes, SysUtils, FileWatch, FileWatchMac;

function KindName(Kind: TFileChangeKind): string;
const
  Names: array[TFileChangeKind] of string = ('ready', 'content', 'replaced',
    'disappeared', 'reappeared', 'metadata', 'rescan', 'error');
begin
  Result := Names[Kind];
end;

procedure Drain(Source: TFileChangeSource);
var
  Event: TFileChangeEvent;
begin
  while Source.TryDequeue(Event) do
    WriteLn('EVENT|', KindName(Event.Kind), '|', Event.SubscriptionID, '|',
      Event.Generation, '|', Ord(Event.Status), '|', Event.Path);
  WriteLn('END');
  Flush(Output);
end;

procedure Require(Condition: Boolean; const MessageText: string);
begin
  if not Condition then raise Exception.Create(MessageText);
end;

procedure CycleSources(const Path: string; Count: LongInt);
var
  I: LongInt;
  Source: TFileChangeSource;
  SubscriptionID: QWord;
begin
  for I := 1 to Count do begin
    Source := CreateFileChangeSource;
    try
      Source.Start;
      Require(Source.WaitUntilReady(3000) = fwsReady,
        'cycle source did not become ready');
      Require(Source.Subscribe(Path, QWord(I), SubscriptionID) = fwsReady,
        'cycle subscription failed');
      Source.Unsubscribe(SubscriptionID);
      Source.Stop;
      Require(Source.Status = fwsStopped, 'cycle source did not stop');
    finally
      Source.Free;
    end;
  end;
end;

var
  Source: TFileChangeSource;
  MacSource: TMacFileChangeSource;
  Path, Command, Operation: string;
  SubscriptionID, Generation: QWord;
  Status: TFileWatchStatus;
  TimeoutMS, Count: LongInt;
  BeforeProbe, AfterProbe: QWord;
begin
  Source := nil;
  try
    Require(ParamCount = 1, 'usage: FileWatchMacTests <path>');
    Path := ParamStr(1);
    Source := CreateFileChangeSource;
    Require(Source is TMacFileChangeSource,
      'macOS factory did not return the native backend');
    Source.Start;
    Require(Source.WaitUntilReady(3000) = fwsReady,
      'watch source did not become ready');
    Status := Source.Subscribe(Path, 100, SubscriptionID);
    Require(Status = fwsReady, 'file subscription was not ready');
    Require(SubscriptionID <> 0, 'file subscription has no ID');
    Generation := 100;
    WriteLn('READY|', SubscriptionID);
    Flush(Output);

    while not EOF(Input) do begin
      ReadLn(Command);
      Operation := Trim(Copy(Command, 1, Pos(' ', Command) - 1));
      if Pos(' ', Command) = 0 then Operation := Trim(Command);
      TimeoutMS := StrToIntDef(Trim(Copy(Command, Length(Operation) + 1,
        MaxInt)), 1500);
      if Operation = 'POLL' then begin
        Source.WaitForEvent(TimeoutMS);
        Drain(Source);
      end else if Operation = 'IDLE' then begin
        MacSource := TMacFileChangeSource(Source);
        MacSource.TestResetProbeCount;
        BeforeProbe := MacSource.ProbeCount;
        Source.WaitForEvent(TimeoutMS);
        AfterProbe := MacSource.ProbeCount;
        WriteLn('IDLE|', BeforeProbe, '|', AfterProbe);
        Drain(Source);
      end else if Operation = 'UNSUB' then begin
        Source.Unsubscribe(SubscriptionID);
        WriteLn('UNSUBSCRIBED');
        WriteLn('END');
        Flush(Output);
      end else if Operation = 'REVOKEFILE' then begin
        TMacFileChangeSource(Source).TestInjectVnodeEvent(
          SubscriptionID, False, $00000040);
        WriteLn('INJECTED');
        WriteLn('END');
        Flush(Output);
      end else if Operation = 'REVOKEDIR' then begin
        TMacFileChangeSource(Source).TestInjectVnodeEvent(
          SubscriptionID, True, $00000040);
        WriteLn('INJECTED');
        WriteLn('END');
        Flush(Output);
      end else if Operation = 'LOSSFILE' then begin
        TMacFileChangeSource(Source).TestInjectVnodeEvent(
          SubscriptionID, False, $00004000, 5);
        WriteLn('INJECTED');
        WriteLn('END');
        Flush(Output);
      end else if Operation = 'STALE' then begin
        TMacFileChangeSource(Source).TestInjectStaleFileEvent(
          SubscriptionID);
        WriteLn('INJECTED');
        WriteLn('END');
        Flush(Output);
      end else if Operation = 'RESUB' then begin
        Inc(Generation);
        Status := Source.Subscribe(Path, Generation, SubscriptionID);
        Require(Status = fwsReady, 'resubscription was not ready');
        WriteLn('SUBSCRIBED|', SubscriptionID, '|', Generation);
        WriteLn('END');
        Flush(Output);
      end else if Operation = 'CYCLE' then begin
        Count := TimeoutMS;
        CycleSources(Path, Count);
        WriteLn('CYCLED|', Count);
        WriteLn('END');
        Flush(Output);
      end else if Operation = 'STOP' then begin
        Source.Unsubscribe(SubscriptionID);
        Source.Stop;
        WriteLn('STOPPED');
        Flush(Output);
        Break;
      end else
        raise Exception.Create('unknown command: ' + Command);
    end;
  except
    on E: Exception do begin
      WriteLn('FAIL|', E.Message);
      Flush(Output);
      Source.Free;
      Halt(1);
    end;
  end;
  Source.Free;
end.

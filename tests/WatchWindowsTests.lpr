program WatchWindowsTests;

{$mode objfpc}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, SyncObjs, Process, FileWatch, FileWatchWindows,
  FileWatchNative;

type
  TTestNotifyHeader = packed record
    NextEntryOffset: Cardinal;
    Action: Cardinal;
    FileNameLength: Cardinal;
  end;
  TByteBuffer = array of Byte;
  TTestFileTime = packed record
    LowDateTime: Cardinal;
    HighDateTime: Cardinal;
  end;
  PTestFileTime = ^TTestFileTime;

const
  WaitLimitMS = 10000;

function TestCreateFileW(FileName: PWideChar; DesiredAccess, ShareMode: Cardinal;
  SecurityAttributes: Pointer; CreationDisposition, FlagsAndAttributes: Cardinal;
  TemplateFile: PtrUInt): PtrUInt; stdcall; external 'kernel32' name 'CreateFileW';
function TestGetFileSizeEx(Handle: PtrUInt; var FileSize: Int64): LongBool;
  stdcall; external 'kernel32' name 'GetFileSizeEx';
function TestReadFile(Handle: PtrUInt; Buffer: Pointer; BytesToRead: Cardinal;
  var BytesRead: Cardinal; Overlapped: Pointer): LongBool; stdcall;
  external 'kernel32' name 'ReadFile';
function TestGetFileTime(Handle: PtrUInt; CreationTime, LastAccessTime,
  LastWriteTime: PTestFileTime): LongBool; stdcall;
  external 'kernel32' name 'GetFileTime';
function TestGetFileAttributesW(FileName: PWideChar): Cardinal; stdcall;
  external 'kernel32' name 'GetFileAttributesW';
function TestCloseHandle(Handle: PtrUInt): LongBool; stdcall;
  external 'kernel32' name 'CloseHandle';

procedure Check(Condition: Boolean; const MessageText: string);
begin
  if not Condition then raise Exception.Create(MessageText);
end;

function Aligned4(Value: Integer): Integer;
begin
  Result := (Value + 3) and not 3;
end;

procedure PutNotifyRecord(var Buffer: TByteBuffer; Offset: Integer;
  NextOffset, Action: Cardinal; const Name: UnicodeString);
var
  Header: TTestNotifyHeader;
begin
  Header.NextEntryOffset := NextOffset;
  Header.Action := Action;
  Header.FileNameLength := Length(Name) * SizeOf(WideChar);
  Move(Header, Buffer[Offset], SizeOf(Header));
  if Header.FileNameLength > 0 then
    Move(Name[1], Buffer[Offset + SizeOf(Header)], Header.FileNameLength);
end;

procedure TestNotificationParser;
var
  Buffer: TByteBuffer;
  Records: TWindowsNotifyRecords;
  Header: TTestNotifyHeader;
  FirstName, SecondName: UnicodeString;
  FirstSize, TotalSize: Integer;
begin
  FirstName := 'café α.tpx';
  SecondName := '雪 spaced.tpx';
  FirstSize := Aligned4(SizeOf(TTestNotifyHeader) +
    Length(FirstName) * SizeOf(WideChar));
  TotalSize := FirstSize + SizeOf(TTestNotifyHeader) +
    Length(SecondName) * SizeOf(WideChar);
  SetLength(Buffer, TotalSize);
  FillChar(Buffer[0], Length(Buffer), 0);
  PutNotifyRecord(Buffer, 0, FirstSize, 1, FirstName);
  PutNotifyRecord(Buffer, FirstSize, 0, 3, SecondName);
  Check(TryParseWindowsNotifyBuffer(Buffer[0], Length(Buffer), Records),
    'The parser rejected a valid multi-record notification');
  Check((Length(Records) = 2) and (Records[0].Name = FirstName) and
    (Records[1].Name = SecondName),
    'The parser did not preserve all UTF-16 names');

  Check(not TryParseWindowsNotifyBuffer(Buffer[0], SizeOf(TTestNotifyHeader) - 1,
    Records), 'The parser accepted a truncated record header');
  FillChar(Buffer[0], Length(Buffer), 0);
  Header.NextEntryOffset := 0;
  Header.Action := 1;
  Header.FileNameLength := 0;
  Move(Header, Buffer[0], SizeOf(Header));
  Check(not TryParseWindowsNotifyBuffer(Buffer[0], SizeOf(Header), Records),
    'The parser accepted an empty notification filename');

  Header.FileNameLength := 3;
  Move(Header, Buffer[0], SizeOf(Header));
  Check(not TryParseWindowsNotifyBuffer(Buffer[0], SizeOf(Header) + 3,
    Records), 'The parser accepted an odd UTF-16 name length');
  Header.FileNameLength := 8;
  Header.NextEntryOffset := 0;
  Move(Header, Buffer[0], SizeOf(Header));
  Check(not TryParseWindowsNotifyBuffer(Buffer[0], SizeOf(Header) + 2,
    Records), 'The parser accepted a truncated UTF-16 name');
  Header.FileNameLength := 2;
  Header.NextEntryOffset := 13;
  Move(Header, Buffer[0], SizeOf(Header));
  Check(not TryParseWindowsNotifyBuffer(Buffer[0], 16, Records),
    'The parser accepted a misaligned record offset');

  SetLength(Buffer, SizeOf(TTestNotifyHeader) + 4);
  FillChar(Buffer[0], Length(Buffer), 0);
  PutNotifyRecord(Buffer, 0, Length(Buffer), 1, 'ab');
  Check(not TryParseWindowsNotifyBuffer(Buffer[0], Length(Buffer), Records),
    'The parser accepted a nonfinal record without a following header');
end;

function Base64Utf8(const Value: UTF8String): string;
const
  Alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var
  I, A, B, C: Integer;
  Bytes: RawByteString;
begin
  Bytes := Value;
  Result := '';
  I := 1;
  while I <= Length(Bytes) do
  begin
    A := Ord(Bytes[I]);
    B := 0;
    C := 0;
    if I + 1 <= Length(Bytes) then B := Ord(Bytes[I + 1]);
    if I + 2 <= Length(Bytes) then C := Ord(Bytes[I + 2]);
    Result := Result + Alphabet[(A shr 2) + 1] +
      Alphabet[((A and 3) shl 4 or (B shr 4)) + 1];
    if I + 1 <= Length(Bytes) then
      Result := Result + Alphabet[((B and 15) shl 2 or (C shr 6)) + 1]
    else
      Result := Result + '=';
    if I + 2 <= Length(Bytes) then
      Result := Result + Alphabet[(C and 63) + 1]
    else
      Result := Result + '=';
    Inc(I, 3);
  end;
end;

procedure DrainEvents(Source: TFileChangeSource);
var
  Event: TFileChangeEvent;
begin
  while Source.TryDequeue(Event) do ;
end;

function WaitForSubscriptionEvent(Source: TFileChangeSource; ID: QWord;
  const ExpectedPath: UTF8String; ExpectedKind: TFileChangeKind;
  RequireKind: Boolean): Boolean;
var
  Event: TFileChangeEvent;
  Seen: Boolean;
  ProbeCount, CompletionCount: Integer;
begin
  Seen := False;
  if not Source.WaitForEvent(WaitLimitMS) then
  begin
    ProbeCount := WindowsWatchPathProbeCountForTest(Source);
    CompletionCount := WindowsWatchCompletionCountForTest(Source);
    raise Exception.CreateFmt(
      'No file-change event for subscription %d (status %d, completions %d, path probes %d)',
      [ID, Ord(Source.Status), CompletionCount, ProbeCount]);
  end;
  while Source.TryDequeue(Event) do
  begin
    if (Event.Kind = fckBackendError) then
      raise Exception.Create('Watcher reported a backend error: ' + Event.ErrorText);
    if (Event.Kind = fckRescanRequired) or
      ((Event.SubscriptionID = ID) and
       ((not RequireKind) or (Event.Kind = ExpectedKind))) then
      Seen := True;
    if (Event.Path <> '') and (Event.SubscriptionID <> ID) then
      raise Exception.Create('Watcher emitted an event for an unknown subscription');
    if Event.SubscriptionID = ID then
      Check((Event.Generation = 17) and (Event.Path = ExpectedPath),
        'Watcher did not preserve subscription generation and normalized path');
  end;
  Result := Seen;
  if RequireKind and not Result then
    raise Exception.CreateFmt('Expected watcher event kind %d was not observed',
      [Ord(ExpectedKind)]);
  if not RequireKind and not Result then
    raise Exception.Create('The filesystem change produced no watcher event');
end;

function ReadFileBytes(const Path: UTF8String;
  out ModifiedFileTime: QWord): RawByteString;
var
  Handle: PtrUInt;
  WidePath: UnicodeString;
  FileSize: Int64;
  Offset, BytesRead: Cardinal;
  ModifiedTime: TTestFileTime;
begin
  ModifiedFileTime := 0;
  WidePath := UTF8Decode(Path);
  Handle := TestCreateFileW(PWideChar(WidePath), $80000000,
    $00000001 or $00000002 or $00000004, nil, 3, 0, 0);
  if Handle = PtrUInt(-1) then RaiseLastOSError;
  try
    if not TestGetFileSizeEx(Handle, FileSize) then RaiseLastOSError;
    if (FileSize < 0) or (FileSize > High(Integer)) then
      raise EReadError.Create('Watcher test source has an invalid size');
    SetLength(Result, Integer(FileSize));
    Offset := 0;
    while Offset < Cardinal(FileSize) do
    begin
      if not TestReadFile(Handle, @Result[Offset + 1],
        Cardinal(FileSize) - Offset, BytesRead, nil) then RaiseLastOSError;
      if BytesRead = 0 then
        raise EReadError.Create('Watcher test source ended while reading');
      Inc(Offset, BytesRead);
    end;
    if not TestGetFileTime(Handle, nil, nil, @ModifiedTime) then
      RaiseLastOSError;
    ModifiedFileTime := (QWord(ModifiedTime.HighDateTime) shl 32) or
      ModifiedTime.LowDateTime;
  finally
    TestCloseHandle(Handle);
  end;
end;

function PathExists(const Path: UTF8String): Boolean;
var
  WidePath: UnicodeString;
begin
  WidePath := UTF8Decode(Path);
  Result := TestGetFileAttributesW(PWideChar(WidePath)) <> Cardinal(-1);
end;

procedure AssertFileBytes(const Path: UTF8String; ExpectedExists: Boolean;
  const ExpectedBytes: RawByteString);
var
  ModifiedFileTime: QWord;
  ActualBytes: RawByteString;
begin
  Check(PathExists(Path) = ExpectedExists, 'Source existence mismatch');
  if not ExpectedExists then Exit;
  ActualBytes := ReadFileBytes(Path, ModifiedFileTime);
  Check(ActualBytes = ExpectedBytes, 'Unexpected source file bytes');
end;

function MakeEvent(Name: string): TEvent;
begin
  Result := TEvent.Create(nil, False, False, Name);
end;

function StartHelper(const HelperPath: string; const Target: UTF8String;
  Events: array of TEvent; const EventNames: array of string): TProcess;
const
  EventParamNames: array[0..6] of string = (
    '-ReadyName', '-StartName', '-StepName', '-ContinueName',
    '-DoneName', '-ReleaseName', '-CaseChangedName');
var
  I: Integer;
begin
  Result := TProcess.Create(nil);
  try
    Result.Executable := 'powershell.exe';
    Result.Options := [poNoConsole];
    Result.Parameters.Add('-NoLogo');
    Result.Parameters.Add('-NoProfile');
    Result.Parameters.Add('-NonInteractive');
    Result.Parameters.Add('-ExecutionPolicy');
    Result.Parameters.Add('Bypass');
    Result.Parameters.Add('-File');
    Result.Parameters.Add(HelperPath);
    Result.Parameters.Add('-TargetBase64');
    Result.Parameters.Add(Base64Utf8(Target));
    for I := 0 to High(Events) do
    begin
      Result.Parameters.Add(EventParamNames[I]);
      Result.Parameters.Add(EventNames[I]);
    end;
    Result.Execute;
  except
    Result.Free;
    raise;
  end;
end;

procedure TestNativeWatcher(const HelperPath: string);
var
  Source: TFileChangeSource;
  Events: array[0..6] of TEvent;
  EventNames: array[0..6] of string;
  Process: TProcess;
  Directory, Path, NormalizedPath: UTF8String;
  Prefix: string;
  SubscriptionID, TemporaryID, CancelID: QWord;
  Event: TFileChangeEvent;
  Dummy: Byte;
  EventBuffer: TByteBuffer;
  EventFileName: UnicodeString;
  Malformed: array[0..15] of Byte;
  Header: TTestNotifyHeader;
  I, BeforeProbes, BeforeCompletions: Integer;
  CancelRequests, CancelCompletions: Integer;
  PreviousModifiedFileTime, CurrentModifiedFileTime: QWord;
  PreviousBytes, CurrentBytes: RawByteString;
  CaseRenameObserved: Boolean;
  SubscribeStatus: TFileWatchStatus;
  StartedAt: QWord;
  StepExists: Boolean;
  StepBytes: RawByteString;
begin
  Prefix := 'Local\TpXWatch64-' + IntToHex(GetTickCount64, 16);
  CaseRenameObserved := False;
  EventNames[0] := Prefix + '-ready';
  EventNames[1] := Prefix + '-start';
  EventNames[2] := Prefix + '-step';
  EventNames[3] := Prefix + '-continue';
  EventNames[4] := Prefix + '-done';
  EventNames[5] := Prefix + '-release';
  EventNames[6] := Prefix + '-case';
  for I := Low(Events) to High(Events) do Events[I] := MakeEvent(EventNames[I]);
  Source := CreateFileChangeSource;
  Process := nil;
  try
    Source.Start;
    Check(Source.WaitUntilReady(WaitLimitMS) = fwsReady,
      'The Windows notification source did not become ready');
    Directory := UTF8String(GetTempDir(False));
    while (Length(Directory) > 0) and
      (Directory[Length(Directory)] = PathDelim) do
      Delete(Directory, Length(Directory), 1);
    Directory := Directory + UTF8String(PathDelim) +
      UTF8String('TpX watcher Δ ') + UTF8String(IntToStr(GetTickCount64));
    Path := Directory + UTF8String(PathDelim) +
      UTF8String('drawing üñ spaced.tpx');
    Process := StartHelper(HelperPath, Path, Events, EventNames);
    Check(Events[0].WaitFor(WaitLimitMS) = wrSignaled,
      'The external writer did not reach its ready barrier');
    SubscribeStatus := Source.Subscribe(Path, 17, SubscriptionID);
    Check(SubscribeStatus = fwsReady,
      'The Windows notification source rejected a supported directory');
    NormalizedPath := NormalizeFileWatchPath(Path);
    DrainEvents(Source);

    Dummy := 0;
    InjectWindowsCompletionForTest(Source, SubscriptionID, False, 0, 1022, Dummy);
    Check(Source.TryDequeue(Event) and (Event.Kind = fckRescanRequired) and
      (Event.SubscriptionID = 0), 'Overflow did not request one global rescan');
    InjectWindowsCompletionForTest(Source, SubscriptionID, True, 0, 0, Dummy);
    Check(Source.TryDequeue(Event) and (Event.Kind = fckRescanRequired),
      'A zero-byte completion did not request a rescan');
    FillChar(Malformed, SizeOf(Malformed), 0);
    Header.NextEntryOffset := 0;
    Header.Action := 1;
    Header.FileNameLength := 3;
    Move(Header, Malformed[0], SizeOf(Header));
    InjectWindowsCompletionForTest(Source, SubscriptionID, True,
      SizeOf(Header) + 3, 0, Malformed[0]);
    Check(Source.TryDequeue(Event) and (Event.Kind = fckRescanRequired),
      'Malformed completion records did not request a rescan');
    Header.FileNameLength := 8;
    Move(Header, Malformed[0], SizeOf(Header));
    InjectWindowsCompletionForTest(Source, SubscriptionID, True,
      SizeOf(Header) + 2, 0, Malformed[0]);
    Check(Source.TryDequeue(Event) and (Event.Kind = fckRescanRequired),
      'A truncated completion record did not request a rescan');

    BeforeProbes := WindowsWatchPathProbeCountForTest(Source);
    BeforeCompletions := WindowsWatchCompletionCountForTest(Source);
    Check(not Source.WaitForEvent(750), 'An idle watcher produced a queue event');
    Check(WindowsWatchPathProbeCountForTest(Source) = BeforeProbes,
      'The idle watcher performed a recurring path probe');
    Check(WindowsWatchCompletionCountForTest(Source) = BeforeCompletions,
      'The idle watcher woke for a periodic completion');
    Events[1].SetEvent;

    for I := 0 to 10 do
    begin
      WriteLn('Waiting for filesystem scenario step ', I);
      Flush(Output);
      Check(Events[2].WaitFor(WaitLimitMS) = wrSignaled,
        'The external writer did not reach a step barrier');
      case I of
        0: begin StepExists := True; StepBytes := 'in-place-one'; end;
        1: begin StepExists := True; StepBytes := 'in-place-two'; end;
        2: begin StepExists := True; StepBytes := 'replacement-one'; end;
        3: begin StepExists := True; StepBytes := 'replacement-two'; end;
        4: begin StepExists := False; StepBytes := ''; end;
        5: begin StepExists := True; StepBytes := 'replacement-two'; end;
        6: begin StepExists := False; StepBytes := ''; end;
        7: begin StepExists := True; StepBytes := 'delete-recreate-final'; end;
        8: begin StepExists := True; StepBytes := 'delete-recreate-final'; end;
        9: begin StepExists := True; StepBytes := 'delete-recreate-final'; end;
        else begin StepExists := True; StepBytes := 'final-source-bytes'; end;
      end;
      if I = 9 then
      begin
        Check(not Source.WaitForEvent(500),
          'Unrelated sibling activity produced a watched-file event');
      end
      else
      begin
        if I = 0 then
        begin
          PreviousBytes := ReadFileBytes(Path, PreviousModifiedFileTime);
        end;
        if I = 4 then
          WaitForSubscriptionEvent(Source, SubscriptionID, NormalizedPath,
            fckDisappeared, True)
        else if I = 5 then
          WaitForSubscriptionEvent(Source, SubscriptionID, NormalizedPath,
            fckReappeared, True)
        else if I = 6 then
          WaitForSubscriptionEvent(Source, SubscriptionID, NormalizedPath,
            fckDisappeared, True)
        else if I = 7 then
          WaitForSubscriptionEvent(Source, SubscriptionID, NormalizedPath,
            fckReappeared, True)
        else if I = 8 then
        begin
          CaseRenameObserved := Events[6].WaitFor(0) = wrSignaled;
          if CaseRenameObserved then
            WaitForSubscriptionEvent(Source, SubscriptionID, NormalizedPath,
              fckReplaced, False);
        end
        else
          WaitForSubscriptionEvent(Source, SubscriptionID, NormalizedPath,
            fckContentChanged, False);
      end;
      if I = 0 then
      begin
        Check((Length(PreviousBytes) = Length('in-place-one')) and
          (PreviousBytes = 'in-place-one'),
          'Initial source bytes were not recorded');
      end
      else if I = 1 then
      begin
        CurrentBytes := ReadFileBytes(Path, CurrentModifiedFileTime);
        Check((Length(CurrentBytes) = Length(PreviousBytes)) and
          (CurrentModifiedFileTime = PreviousModifiedFileTime) and
          (CurrentBytes <> PreviousBytes),
          'Restored-mtime same-size content change was not detected');
      end;
      AssertFileBytes(Path, StepExists, StepBytes);
      Events[3].SetEvent;
    end;
    if CaseRenameObserved then
      WriteLn('Case-only rename was observed')
    else
      WriteLn('Case-only rename was not supported by this temporary directory');

    Check(Events[4].WaitFor(WaitLimitMS) = wrSignaled,
      'The external writer did not finish its scenario');
    AssertFileBytes(Path, True, 'final-source-bytes');

    DrainEvents(Source);
    Check(Source.Subscribe(Path, 18, TemporaryID) = fwsReady,
      'A second subscription to the watched directory failed');
    DrainEvents(Source);
    Source.Unsubscribe(SubscriptionID);
    EventFileName := UTF8Decode(UTF8String(ExtractFileName(Path)));
    SetLength(EventBuffer, SizeOf(TTestNotifyHeader) +
      Length(EventFileName) * SizeOf(WideChar));
    FillChar(EventBuffer[0], Length(EventBuffer), 0);
    PutNotifyRecord(EventBuffer, 0, 0, 3, EventFileName);
    InjectWindowsCompletionForTest(Source, TemporaryID, True,
      Length(EventBuffer), 0, EventBuffer[0]);
    Check(Source.TryDequeue(Event) and (Event.SubscriptionID = TemporaryID) and
      (Event.Generation = 18),
      'A completion after unsubscribe reached a stale subscription');
    Check(not Source.TryDequeue(Event),
      'A stale completion was delivered more than once');
    Source.Unsubscribe(TemporaryID);
    InjectWindowsCompletionForTest(Source, TemporaryID, True, 0, 0, Dummy);
    Check(not Source.TryDequeue(Event),
      'A completion injected after unsubscribe reached the queue');
    for I := 1 to 20 do
    begin
      Check(Source.Subscribe(Path, 100 + I, TemporaryID) = fwsReady,
        'Repeated subscription failed');
      DrainEvents(Source);
      Source.Unsubscribe(TemporaryID);
    end;
    Check(Source.Subscribe(Path, 999, CancelID) = fwsReady,
      'Could not arm a read for shutdown cancellation coverage');
    DrainEvents(Source);
    StartedAt := GetTickCount64;
    Source.Stop;
    Check(GetTickCount64 - StartedAt < 3000,
      'Cancellation drain exceeded the bounded shutdown limit');
    Check(WindowsWatchCancellationCountsForTest(Source, CancelRequests,
      CancelCompletions) and (CancelRequests > 0) and
      (CancelCompletions > 0),
      'Shutdown did not drain an actual cancelled overlapped read');
    Events[5].SetEvent;
    Check(Process.WaitOnExit(WaitLimitMS), 'The external writer did not exit');
    Check(Process.ExitCode = 0, 'The external writer reported a failure');
  finally
    if Process <> nil then
    begin
      Events[3].SetEvent;
      Events[5].SetEvent;
      if not Process.WaitOnExit(2000) then Process.Terminate(1);
      Process.Free;
    end;
    Source.Free;
    for I := Low(Events) to High(Events) do Events[I].Free;
  end;
end;

procedure TestCompletionPortFailure;
var
  Source: TFileChangeSource;
  Event: TFileChangeEvent;
  Path: UTF8String;
  SubscriptionID: QWord;
  BackendErrors, CancelRequests, CancelCompletions: Integer;
  StartedAt: QWord;
begin
  Source := CreateFileChangeSource;
  try
    Source.Start;
    Check(Source.WaitUntilReady(WaitLimitMS) = fwsReady,
      'The Windows source did not start for completion-port failure coverage');
    Path := UTF8String(IncludeTrailingPathDelimiter(GetTempDir(False))) +
      UTF8String('tpx-watch-fatal-completion-target.tpx');
    Check(Source.Subscribe(Path, 44, SubscriptionID) = fwsReady,
      'Could not arm an overlapped read for completion-port failure coverage');
    DrainEvents(Source);

    InjectWindowsCompletionPortFailureForTest(Source, 6);
    Check(Source.WaitForEvent(WaitLimitMS),
      'The terminal completion-port error was not reported');
    BackendErrors := 0;
    while Source.TryDequeue(Event) do
      if Event.Kind = fckBackendError then Inc(BackendErrors);
    Check((BackendErrors = 1) and (Source.Status = fwsDegraded) and
      (WindowsWatchCompletionPortFailureCountForTest(Source) = 1),
      'A fatal completion-port error did not terminate with one bounded report');
    Check(not Source.WaitForEvent(200) and
      (WindowsWatchCompletionPortFailureCountForTest(Source) = 1),
      'The fatal completion-port path continued reporting in a loop');

    StartedAt := GetTickCount64;
    Source.Stop;
    Check(GetTickCount64 - StartedAt < 3000,
      'Stop did not finish after a terminal completion-port error');
    Check(WindowsWatchCancellationCountsForTest(Source, CancelRequests,
      CancelCompletions) and (CancelRequests > 0) and
      (CancelCompletions > 0),
      'Stop did not cancel and drain the pending read after port failure');
  finally
    Source.Free;
  end;
end;

var
  HelperPath: string;
begin
  try
    TestNotificationParser;
    if ParamCount > 0 then HelperPath := ExpandFileName(ParamStr(1))
    else HelperPath := ExpandFileName('tests/watch_windows_helper.ps1');
    Check(FileExists(HelperPath), 'Missing external Windows watcher helper');
    TestNativeWatcher(HelperPath);
    TestCompletionPortFailure;
    WriteLn('Windows file watcher tests passed');
  except
    on E: Exception do
    begin
      WriteLn(StdErr, 'Windows file watcher test failed: ', E.Message);
      Halt(1);
    end;
  end;
end.

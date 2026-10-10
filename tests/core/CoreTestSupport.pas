unit CoreTestSupport;

{$mode delphi}{$H+}

interface

type
  TCoreTestProc = procedure;

procedure RegisterCoreTest(const Name: string; Test: TCoreTestProc);
procedure CheckCore(Condition: Boolean; const Message: string);
procedure CheckNearCore(Actual, Expected, Tolerance: Double;
  const Description: string);
function RunCoreTests: Integer;

implementation

uses SysUtils, Math, Classes;

type
  TCoreTest = record
    Name: string;
    Test: TCoreTestProc;
  end;
  TCoreResult = record
    Name: string;
    Status: string;
    Message: string;
  end;

var
  RegisteredTests: array of TCoreTest;
  Results: array of TCoreResult;

procedure RegisterCoreTest(const Name: string; Test: TCoreTestProc);
var
  I, N: Integer;
begin
  if (Name = '') or not Assigned(Test) then
    raise Exception.Create('Core test registration requires a name and callback');
  for I := 0 to High(RegisteredTests) do
    if RegisteredTests[I].Name = Name then
      raise Exception.Create('Duplicate core test name: ' + Name);
  N := Length(RegisteredTests);
  SetLength(RegisteredTests, N + 1);
  RegisteredTests[N].Name := Name;
  RegisteredTests[N].Test := Test;
end;

procedure CheckCore(Condition: Boolean; const Message: string);
begin
  if not Condition then
    raise Exception.Create(Message);
end;

function NumberText(Value: Double): string;
var
  Settings: TFormatSettings;
begin
  Settings := DefaultFormatSettings;
  Settings.DecimalSeparator := '.';
  Result := FloatToStrF(Value, ffGeneral, 15, 0, Settings);
end;

procedure CheckNearCore(Actual, Expected, Tolerance: Double;
  const Description: string);
begin
  if not (Abs(Actual - Expected) <= Tolerance) then
    raise Exception.CreateFmt('%s: expected %s +/- %s, got %s',
      [Description, NumberText(Expected), NumberText(Tolerance),
       NumberText(Actual)]);
end;

function JsonEscape(const Value: string): string;
var
  I: Integer;
begin
  Result := '"';
  for I := 1 to Length(Value) do
    case Value[I] of
      '"': Result := Result + '\"';
      '\': Result := Result + '\\';
      #8: Result := Result + '\b';
      #9: Result := Result + '\t';
      #10: Result := Result + '\n';
      #12: Result := Result + '\f';
      #13: Result := Result + '\r';
      else
        if Ord(Value[I]) < 32 then
          Result := Result + '\u00' + IntToHex(Ord(Value[I]), 2)
        else
          Result := Result + Value[I];
    end;
  Result := Result + '"';
end;

function ParamValue(const Prefix: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to ParamCount do
    if Copy(ParamStr(I), 1, Length(Prefix)) = Prefix then
      Exit(Copy(ParamStr(I), Length(Prefix) + 1, MaxInt));
end;

procedure SelfTestWrongGeometry;
begin
  CheckNearCore(5.0, 5.1, 0.01, 'fixture line length (intentional wrong expectation)');
end;

procedure SelfTestFailingScenario;
begin
  CheckCore(False, 'intentional scenario failure for runner self-check');
end;

procedure AddSelfTest(const Mode: string);
begin
  SetLength(RegisteredTests, 0);
  if Mode = 'wrong-geometry' then
    RegisterCoreTest('self-test-wrong-geometry', SelfTestWrongGeometry)
  else if Mode = 'scenario-failure' then
    RegisterCoreTest('self-test-scenario-failure', SelfTestFailingScenario)
  else
    raise Exception.Create('Unknown core runner self-test: ' + Mode);
end;

function IsSelected(const Name, Filter: string): Boolean;
begin
  Result := (Filter = '') or
    (Pos(LowerCase(Filter), LowerCase(Name)) > 0);
end;

function WriteReport(const FileName: string; Discovered, Selected,
  Passed, Failed: Integer): Boolean;
var
  F: TextFile;
  I: Integer;
begin
  Result := True;
  if FileName = '' then Exit;
  AssignFile(F, FileName);
  {$I-} Rewrite(F); {$I+}
  if IOResult <> 0 then Exit(False);
  try
    Write(F, '{"suite":"core","suite_count":1,"discovered":', Discovered,
      ',"selected":', Selected, ',"passed":', Passed,
      ',"failed":', Failed, ',"cases":[');
    for I := 0 to High(Results) do begin
      if I > 0 then Write(F, ',');
      Write(F, '{"name":', JsonEscape(Results[I].Name),
        ',"status":', JsonEscape(Results[I].Status),
        ',"message":', JsonEscape(Results[I].Message), '}');
    end;
    WriteLn(F, ']}');
  finally
    CloseFile(F);
  end;
end;

function RunCoreTests: Integer;
var
  I, Selected, Passed, Failed, Expected: Integer;
  Filter, ReportPath, SelfTest, Arg: string;
  ListOnly: Boolean;
  Code: Integer;
begin
  Result := 2;
  Filter := ParamValue('--filter=');
  ReportPath := ParamValue('--report=');
  SelfTest := ParamValue('--self-test=');
  Arg := ParamValue('--expected=');
  Expected := -1;
  if Arg <> '' then begin
    Val(Arg, Expected, Code);
    if Code <> 0 then begin
      WriteLn('Invalid --expected value: ', Arg);
      Exit;
    end;
  end;
  ListOnly := False;
  for I := 1 to ParamCount do begin
    Arg := ParamStr(I);
    if Arg = '--list' then ListOnly := True
    else if (Copy(Arg, 1, 9) <> '--filter=') and
            (Copy(Arg, 1, 11) <> '--expected=') and
            (Copy(Arg, 1, 9) <> '--report=') and
            (Copy(Arg, 1, 12) <> '--self-test=') then begin
      WriteLn('Unknown core test option: ', Arg);
      Exit;
    end;
  end;
  if SelfTest <> '' then begin
    try AddSelfTest(SelfTest)
    except on E: Exception do begin WriteLn(E.Message); Exit end end;
  end;
  if Length(RegisteredTests) = 0 then begin
    WriteLn('CORE TEST DISCOVERY FAILED: no tests registered');
    Exit;
  end;
  if (Expected >= 0) and (Length(RegisteredTests) <> Expected) then begin
    WriteLn('CORE TEST DISCOVERY FAILED: expected ', Expected,
      ' cases, discovered ', Length(RegisteredTests));
    Exit;
  end;
  if ListOnly then begin
    for I := 0 to High(RegisteredTests) do WriteLn(RegisteredTests[I].Name);
    WriteLn('CORE_TESTS discovered=', Length(RegisteredTests));
    Exit(0);
  end;
  SetLength(Results, 0);
  Selected := 0;
  Passed := 0;
  Failed := 0;
  for I := 0 to High(RegisteredTests) do
    if IsSelected(RegisteredTests[I].Name, Filter) then begin
      Inc(Selected);
      SetLength(Results, Length(Results) + 1);
      Results[High(Results)].Name := RegisteredTests[I].Name;
      try
        RegisteredTests[I].Test;
        Results[High(Results)].Status := 'passed';
        Inc(Passed);
        WriteLn('PASS ', RegisteredTests[I].Name);
      except
        on E: Exception do begin
          Results[High(Results)].Status := 'failed';
          Results[High(Results)].Message := E.ClassName + ': ' + E.Message;
          Inc(Failed);
          WriteLn('FAIL ', RegisteredTests[I].Name, ': ',
            Results[High(Results)].Message);
        end;
      end;
    end;
  WriteLn('CORE_TESTS suite=core discovered=', Length(RegisteredTests),
    ' selected=', Selected, ' passed=', Passed, ' failed=', Failed);
  if Selected = 0 then begin
    WriteLn('CORE TEST SELECTION FAILED: no cases matched filter "', Filter, '"');
    Failed := 1;
  end;
  if not WriteReport(ReportPath, Length(RegisteredTests), Selected, Passed, Failed) then begin
    WriteLn('CORE TEST REPORT FAILED: could not write ', ReportPath);
    Exit;
  end;
  if Failed = 0 then Result := 0 else Result := 1;
end;

end.

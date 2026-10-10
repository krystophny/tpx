program CoreTests;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, CoreTestSupport, CoreXmlFixtureTests, CoreFileWatchTests,
  TikZSyntaxTests, TikZImportTests, CoreAutoReloadTests;

var
  ExitStatus: Integer;
begin
  try
    ExitStatus := RunCoreTests;
  except
    on E: Exception do begin
      WriteLn('CORE TEST RUNNER ERROR: ', E.ClassName, ': ', E.Message);
      ExitStatus := 2;
    end;
  end;
  Flush(Output);
  if ExitStatus <> 0 then Halt(ExitStatus);
end.

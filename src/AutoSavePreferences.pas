unit AutoSavePreferences;

{$mode delphi}{$H+}

interface

uses Classes;

const
  DefaultAutoSaveEnabled = False;
  DefaultCrashRecoveryEnabled = True;

function DocumentPreferenceKey(const FileName: string): string;
function ReadDocumentAutoSavePreference(const Values: TStrings;
  const FileName: string; out Enabled: Boolean): Boolean;
procedure WriteDocumentAutoSavePreference(const Values: TStrings;
  const FileName: string; Enabled: Boolean);

implementation

uses SysUtils, md5;

function DocumentPreferenceKey(const FileName: string): string;
var
  CanonicalName: string;
begin
  CanonicalName := ExpandFileName(FileName);
  {$IFDEF MSWINDOWS}
  CanonicalName := LowerCase(CanonicalName);
  {$ENDIF}
  Result := MD5Print(MD5String(CanonicalName));
end;

function ReadDocumentAutoSavePreference(const Values: TStrings;
  const FileName: string; out Enabled: Boolean): Boolean;
var
  Key: string;
  Index: Integer;
begin
  if (FileName = '') or not Assigned(Values) then
  begin
    Enabled := DefaultAutoSaveEnabled;
    Exit(False);
  end;
  Key := DocumentPreferenceKey(FileName);
  Index := Values.IndexOfName(Key);
  Result := Index >= 0;
  if not Result then
  begin
    Enabled := DefaultAutoSaveEnabled;
    Exit;
  end;
  Enabled := Trim(Copy(Values[Index], Length(Key) + 2, MaxInt)) = '1';
end;

procedure WriteDocumentAutoSavePreference(const Values: TStrings;
  const FileName: string; Enabled: Boolean);
var
  Key: string;
begin
  if (FileName = '') or not Assigned(Values) then Exit;
  Key := DocumentPreferenceKey(FileName);
  if Enabled then Values.Values[Key] := '1'
  else Values.Values[Key] := '0';
end;

end.

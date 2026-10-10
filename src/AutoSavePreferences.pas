unit AutoSavePreferences;

{$mode delphi}{$H+}

interface

uses Classes, DocumentFormats;

const
  DefaultAutoSaveEnabled = False;
  DefaultCrashRecoveryEnabled = True;

function DocumentPreferenceKey(const FileName: TDocumentPath): string;
function ReadDocumentAutoSavePreference(const Values: TStrings;
  const FileName: TDocumentPath; out Enabled: Boolean): Boolean;
procedure WriteDocumentAutoSavePreference(const Values: TStrings;
  const FileName: TDocumentPath; Enabled: Boolean);

implementation

uses SysUtils, md5;

function SamePreferencePath(const LeftPath, RightPath: TDocumentPath): Boolean;
begin
  Result := SameDocumentPath(LeftPath, RightPath);
end;

function EncodePath(const FileName: TDocumentPath): string;
var I: Integer;
begin
  Result := '';
  for I := 1 to Length(FileName) do
    Result := Result + IntToHex(Ord(FileName[I]), 2);
end;

function HexNibble(C: Char): Integer;
begin
  if (C >= '0') and (C <= '9') then Exit(Ord(C) - Ord('0'));
  if (C >= 'a') and (C <= 'f') then Exit(Ord(C) - Ord('a') + 10);
  if (C >= 'A') and (C <= 'F') then Exit(Ord(C) - Ord('A') + 10);
  Result := -1;
end;

function TryDecodePreference(const Value: string; out FileName: TDocumentPath;
  out Enabled: Boolean): Boolean;
var I, HighNibble, LowNibble: Integer;
begin
  Result := False;
  FileName := '';
  Enabled := DefaultAutoSaveEnabled;
  if (Length(Value) < 2) or (Value[2] <> '|') or
    not ((Value[1] = '0') or (Value[1] = '1')) or
    (Odd(Length(Value) - 2)) then Exit;
  SetLength(FileName, (Length(Value) - 2) div 2);
  for I := 1 to Length(FileName) do
  begin
    HighNibble := HexNibble(Value[2 + I * 2 - 1]);
    LowNibble := HexNibble(Value[2 + I * 2]);
    if (HighNibble < 0) or (LowNibble < 0) then
    begin
      FileName := '';
      Exit;
    end;
    FileName[I] := AnsiChar((HighNibble shl 4) or LowNibble);
  end;
  Enabled := Value[1] = '1';
  Result := True;
end;

function EncodedPreference(Enabled: Boolean;
  const FileName: TDocumentPath): string;
begin
  if Enabled then Result := '1|' else Result := '0|';
  Result := Result + EncodePath(NormalizeDocumentPath(FileName));
end;

function DocumentPreferenceKey(const FileName: TDocumentPath): string;
var
  CanonicalName: TDocumentPath;
begin
  CanonicalName := NormalizeDocumentPath(FileName);
  Result := MD5Print(MD5String(RawByteString(CanonicalName)));
end;

function ReadDocumentAutoSavePreference(const Values: TStrings;
  const FileName: TDocumentPath; out Enabled: Boolean): Boolean;
var
  Key: string;
  Index: Integer;
  StoredPath: TDocumentPath;
  StoredEnabled: Boolean;
  StoredValue: string;
begin
  if (FileName = '') or not Assigned(Values) then
  begin
    Enabled := DefaultAutoSaveEnabled;
    Exit(False);
  end;
  for Index := 0 to Values.Count - 1 do
  begin
    StoredValue := Values.ValueFromIndex[Index];
    if TryDecodePreference(StoredValue, StoredPath, StoredEnabled) and
      SamePreferencePath(StoredPath, FileName) then
    begin
      Enabled := StoredEnabled;
      Exit(True);
    end;
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
  const FileName: TDocumentPath; Enabled: Boolean);
var
  Key: string;
  Index: Integer;
  StoredPath: TDocumentPath;
  StoredEnabled: Boolean;
  StoredValue: string;
begin
  if (FileName = '') or not Assigned(Values) then Exit;
  for Index := 0 to Values.Count - 1 do
  begin
    StoredValue := Values.ValueFromIndex[Index];
    if TryDecodePreference(StoredValue, StoredPath, StoredEnabled) and
      SamePreferencePath(StoredPath, FileName) then
    begin
      Values.ValueFromIndex[Index] := EncodedPreference(Enabled, FileName);
      Exit;
    end;
  end;
  Key := DocumentPreferenceKey(FileName);
  Values.Values[Key] := EncodedPreference(Enabled, FileName);
end;

end.

unit CoreUserStorageTests;

{$mode delphi}{$H+}
{$codepage utf8}

interface

implementation

uses SysUtils, Classes, UserStorage, CoreTestSupport
{$IFDEF UNIX}, BaseUnix{$ENDIF};

function NewScratchDirectory: string;
var ID: TGUID; Token: string; UnicodePart: UnicodeString;
begin
  CheckCore(CreateGUID(ID) = 0, 'could not create scratch directory id');
  Token := StringReplace(StringReplace(GUIDToString(ID), '{', '', []), '}', '', []);
  UnicodePart := 'TpX storage ' + WideChar($00FC) + WideChar($00DF);
  Result := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    UTF8Encode(UnicodePart) + ' ' + Token;
  CheckCore(ForceDirectories(Result), 'could not create scratch directory');
end;

procedure WriteBytes(const FileName: string; const Value: RawByteString);
var Stream: TFileStream;
begin
  ForceDirectories(ExtractFileDir(FileName));
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    if Value <> '' then Stream.WriteBuffer(Value[1], Length(Value));
  finally
    Stream.Free;
  end;
end;

function ReadBytes(const FileName: string): RawByteString;
var Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, Stream.Size);
    if Result <> '' then Stream.ReadBuffer(Result[1], Length(Result));
  finally
    Stream.Free;
  end;
end;

procedure RemoveTree(const DirectoryName: string);
var Search: TSearchRec; Item: string;
begin
  if not DirectoryExists(DirectoryName) then Exit;
  if FindFirst(IncludeTrailingPathDelimiter(DirectoryName) + '*',
    faAnyFile, Search) = 0 then
  try
    repeat
      if (Search.Name = '.') or (Search.Name = '..') then Continue;
      Item := IncludeTrailingPathDelimiter(DirectoryName) + Search.Name;
      if (Search.Attr and faDirectory) <> 0 then RemoveTree(Item)
      else DeleteFile(Item);
    until FindNext(Search) <> 0;
  finally
    FindClose(Search);
  end;
  RemoveDir(DirectoryName);
end;

procedure TestStoragePathResolution;
var E: TStorageEnvironment; P, Other: TUserStoragePaths;
begin
  FillChar(E, SizeOf(E), 0);
  E.Platform := spUnix;
  E.Home := '/home/one';
  E.XdgConfigHome := '/config space';
  E.XdgStateHome := '/state';
  E.XdgCacheHome := '/cache';
  P := ResolveUserStoragePaths(E);
  CheckCore(P.Config = '/config space/TpX', 'XDG config directory was ignored');
  CheckCore(P.State = '/state/TpX', 'XDG state directory was ignored');
  CheckCore(P.Cache = '/cache/TpX', 'XDG cache directory was ignored');

  E.XdgConfigHome := 'relative-config';
  E.XdgStateHome := '';
  E.XdgCacheHome := '';
  P := ResolveUserStoragePaths(E);
  CheckCore(P.Config = '/home/one/.config/TpX',
    'relative XDG config path did not use the home fallback');
  CheckCore(P.State = '/home/one/.local/state/TpX',
    'state fallback did not use the standard directory');
  CheckCore(P.Cache = '/home/one/.cache/TpX',
    'cache fallback did not use the standard directory');
  E.Home := '/home/two';
  Other := ResolveUserStoragePaths(E);
  CheckCore((P.Config <> Other.Config) and (P.State <> Other.State),
    'separate users resolved to shared storage');

  E.Platform := spMacOS;
  E.Home := '/Users/Chris';
  P := ResolveUserStoragePaths(E);
  CheckCore(P.Config = '/Users/Chris/Library/Preferences/TpX',
    'macOS preferences are outside Library/Preferences');
  CheckCore(P.State = '/Users/Chris/Library/Application Support/TpX',
    'macOS state is outside Library/Application Support');
  CheckCore(P.Cache = '/Users/Chris/Library/Caches/TpX',
    'macOS cache is outside Library/Caches');
  E.Home := 'relative-home';
  P := ResolveUserStoragePaths(E);
  CheckCore((P.Config = '') and (P.State = '') and (P.Cache = ''),
    'missing absolute home directory produced relative macOS paths');

  E.Platform := spWindows;
  E.AppData := 'C:\Users\Chris\AppData\Roaming';
  E.LocalAppData := 'C:\Users\Chris\AppData\Local';
  P := ResolveUserStoragePaths(E);
  CheckCore(P.Config = 'C:\Users\Chris\AppData\Roaming\TpX',
    'Windows preferences do not use APPDATA');
  CheckCore((P.State = 'C:\Users\Chris\AppData\Local\TpX') and
    (P.Cache = P.State), 'Windows state/cache do not use LOCALAPPDATA');
  E.Home := 'C:\wrong-home';
  E.UserProfile := 'D:\Users\Chris';
  E.AppData := 'relative-config';
  E.LocalAppData := '';
  P := ResolveUserStoragePaths(E);
  CheckCore(P.Config = 'D:\Users\Chris\AppData\Roaming\TpX',
    'relative APPDATA or HOME bypassed the Windows profile fallback');
  CheckCore(P.State = 'D:\Users\Chris\AppData\Local\TpX',
    'missing LOCALAPPDATA did not use the Windows profile fallback');
  E.PortableRequested := True;
  E.PortableBase := 'D:\TpX-portable-data';
  P := ResolveUserStoragePaths(E);
  CheckCore(P.Portable and (P.Config = E.PortableBase + '\config') and
    (P.State = E.PortableBase + '\state') and
    (P.Cache = E.PortableBase + '\cache'),
    'portable mode did not isolate the three storage areas');
end;

procedure TestSettingsMigrationAndFailedWrite;
var
  Root, Legacy, Current, BlockedParent: string;
  Loaded, Diagnostic: string;
  Original, Updated: RawByteString;
  OK: Boolean;
begin
  Root := NewScratchDirectory;
  try
    Legacy := IncludeTrailingPathDelimiter(Root) + 'legacy/TpX.ini';
    Current := IncludeTrailingPathDelimiter(Root) + 'config/TpX.ini';
    Original := 'LineWidth_Default=2.25'#13#10'IncludePath_Default=TeX';
    WriteBytes(Legacy, Original);
    OK := PrepareSettingsFile(Current, Legacy, Loaded, Diagnostic);
    CheckCore(OK and (Loaded = Current) and (Diagnostic = ''),
      'legacy settings were not migrated to the per-user file');
    CheckCore((ReadBytes(Current) = Original) and (ReadBytes(Legacy) = Original),
      'migration changed the bytes or removed the historical original');
    Updated := 'LineWidth_Default=3.5'#10;
    WriteBytes(Current, Updated);
    WriteBytes(Legacy, 'LineWidth_Default=9.5'#10);
    OK := PrepareSettingsFile(Current, Legacy, Loaded, Diagnostic);
    CheckCore(OK and (Loaded = Current) and (ReadBytes(Current) = Updated),
      'existing per-user settings did not take precedence');

    BlockedParent := IncludeTrailingPathDelimiter(Root) + 'blocked-parent';
    WriteBytes(BlockedParent, 'file blocks directory creation');
    Current := IncludeTrailingPathDelimiter(BlockedParent) + 'TpX.ini';
    OK := PrepareSettingsFile(Current, Legacy, Loaded, Diagnostic);
    CheckCore(OK and (Loaded = Legacy) and (Diagnostic <> ''),
      'failed migration did not load from the intact legacy file with a warning');
    CheckCore(ReadBytes(Legacy) = 'LineWidth_Default=9.5'#10,
      'failed migration altered the historical preferences');
  finally
    RemoveTree(Root);
  end;
end;

procedure TestUserTemplateCopy;
var Root, Source, Destination, ErrorText: string; Data: RawByteString;
begin
  Root := NewScratchDirectory;
  try
    Source := IncludeTrailingPathDelimiter(Root) + 'bundle/preview.tex.inc';
    Destination := IncludeTrailingPathDelimiter(Root) + 'user/preview.tex.inc';
    Data := '% default template'#13#10'\usepackage{tpX}';
    WriteBytes(Source, Data);
    CheckCore(InstallUserFileIfMissing(Source, Destination, ErrorText),
      'packaged template was not copied: ' + ErrorText);
    CheckCore(ReadBytes(Destination) = Data,
      'template copy did not preserve its exact content');
    WriteBytes(Destination, '% customized by the user');
    CheckCore(InstallUserFileIfMissing(Source, Destination, ErrorText) and
      (ReadBytes(Destination) = '% customized by the user'),
      'a later lookup replaced a user template');
  finally
    RemoveTree(Root);
  end;
end;

procedure TestStorageSymlinksAndSafePreferenceWrite;
var
  Root, Current, Legacy, Target, Loaded, Diagnostic, BlockedParent: string;
  LinkedDirectory, OutsideDirectory: string;
  Values: TStringList;
  OK: Boolean;
begin
  Root := NewScratchDirectory;
  Values := TStringList.Create;
  try
    Current := IncludeTrailingPathDelimiter(Root) + 'settings/TpX.ini';
    Legacy := IncludeTrailingPathDelimiter(Root) + 'old/TpX.ini';
    Target := IncludeTrailingPathDelimiter(Root) + 'outside.ini';
    WriteBytes(Target, 'LineWidth_Default=8'#10);
    Values.Add('LineWidth_Default=4.5');
    CheckCore(SaveSettingsFileSafely(Current, Values, Diagnostic),
      'safe preference write failed: ' + Diagnostic);
    CheckCore(ReadBytes(Current) = 'LineWidth_Default=4.5' + LineEnding,
      'safe preference write changed its contents');
{$IFDEF UNIX}
    ForceDirectories(ExtractFileDir(Legacy));
    CheckCore(fpSymlink(PChar(Target), PChar(Legacy)) = 0,
      'could not create a legacy symlink fixture');
    OK := PrepareSettingsFile(Current + '.missing', Legacy, Loaded, Diagnostic);
    CheckCore(OK and (Loaded = '') and (Diagnostic <> ''),
      'legacy symlink was followed during migration');
    CheckCore(ReadBytes(Target) = 'LineWidth_Default=8'#10,
      'legacy symlink handling changed its target');
    DeleteFile(Legacy);
    DeleteFile(Current);
    CheckCore(fpSymlink(PChar(Target), PChar(Current)) = 0,
      'could not create a settings symlink fixture');
    OK := PrepareSettingsFile(Current, Legacy + '.missing', Loaded, Diagnostic);
    CheckCore(not OK and (Loaded = '') and (Diagnostic <> ''),
      'existing preferences symlink was accepted');
    CheckCore(not SaveSettingsFileSafely(Current, Values, Diagnostic) and
      (Diagnostic <> '') and (ReadBytes(Target) = 'LineWidth_Default=8'#10),
      'safe preference write followed a symlink');

    BlockedParent := IncludeTrailingPathDelimiter(Root) + 'blocked-parent';
    WriteBytes(BlockedParent, 'blocking file');
    CheckCore(not SaveSettingsFileSafely(
      IncludeTrailingPathDelimiter(BlockedParent) + 'TpX.ini', Values,
      Diagnostic) and (Diagnostic <> '') and
      (ReadBytes(BlockedParent) = 'blocking file'),
      'failed preference write changed its blocking file');

    LinkedDirectory := IncludeTrailingPathDelimiter(Root) + 'linked-config';
    OutsideDirectory := IncludeTrailingPathDelimiter(Root) + 'outside-config';
    ForceDirectories(OutsideDirectory);
    CheckCore(fpSymlink(PChar(OutsideDirectory), PChar(LinkedDirectory)) = 0,
      'could not create a preferences-directory symlink fixture');
    Current := IncludeTrailingPathDelimiter(LinkedDirectory) + 'TpX.ini';
    OK := PrepareSettingsFile(Current, Legacy, Loaded, Diagnostic);
    CheckCore(not OK and (Loaded = '') and (Diagnostic <> '') and
      not FileExists(IncludeTrailingPathDelimiter(OutsideDirectory) + 'TpX.ini'),
      'migration followed a symbolic link in the preferences directory');
    CheckCore(not SaveSettingsFileSafely(Current, Values, Diagnostic) and
      (Diagnostic <> '') and
      not FileExists(IncludeTrailingPathDelimiter(OutsideDirectory) + 'TpX.ini'),
      'preference write followed a symbolic link in its directory');
    WriteBytes(IncludeTrailingPathDelimiter(OutsideDirectory) + 'TpX.ini',
      'LineWidth_Default=8'#10);
    Legacy := IncludeTrailingPathDelimiter(LinkedDirectory) + 'TpX.ini';
    Current := IncludeTrailingPathDelimiter(Root) + 'safe-config/TpX.ini';
    OK := PrepareSettingsFile(Current, Legacy, Loaded, Diagnostic);
    CheckCore(OK and (Loaded = '') and (Diagnostic <> '') and
      not FileExists(Current),
      'migration read through a symbolic link in the legacy directory');
    DeleteFile(LinkedDirectory);
{$ENDIF}
  finally
    Values.Free;
    RemoveTree(Root);
  end;
end;

initialization
  RegisterCoreTest('user-storage-path-resolution', TestStoragePathResolution);
  RegisterCoreTest('user-storage-settings-migration',
    TestSettingsMigrationAndFailedWrite);
  RegisterCoreTest('user-storage-template-copy', TestUserTemplateCopy);
  RegisterCoreTest('user-storage-symlink-safety',
    TestStorageSymlinksAndSafePreferenceWrite);
end.

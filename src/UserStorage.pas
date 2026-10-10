unit UserStorage;

{$mode delphi}{$H+}

interface

uses Classes, SysUtils;

type
  TUserStorageArea = (usaConfig, usaState, usaCache);
  TStoragePlatform = (spWindows, spUnix, spMacOS);
  TStorageEnvironment = record
    Platform: TStoragePlatform;
    Home, XdgConfigHome, XdgStateHome, XdgCacheHome: string;
    AppData, LocalAppData, UserProfile: string;
    ExecutablePath, PortableBase: string;
    PortableRequested: Boolean;
  end;
  TUserStoragePaths = record
    Config, State, Cache: string;
    Portable: Boolean;
  end;

function ResolveUserStoragePaths(const Environment: TStorageEnvironment):
  TUserStoragePaths;
function CurrentUserStoragePaths: TUserStoragePaths;
function UserStorageDirectory(Area: TUserStorageArea): string;
function EnsureUserStorageDirectory(Area: TUserStorageArea;
  out ErrorText: string): Boolean;
function UserStorageDiagnostics: string;
function PrepareSettingsFile(const CurrentFile, LegacyFile: string;
  out FileToLoad, Diagnostic: string): Boolean;
function SaveSettingsFileSafely(const FileName: string; Contents: TStrings;
  out ErrorText: string): Boolean;
function InstallUserFileIfMissing(const SourceFile, DestinationFile: string;
  out ErrorText: string): Boolean;

implementation

{$IFDEF UNIX}
uses BaseUnix;
{$ELSE}
{$IFDEF MSWINDOWS}
uses Windows;
{$ENDIF}
{$ENDIF}

const
  ApplicationStorageName = 'TpX';

var
  CachedPaths: TUserStoragePaths;
  PathsInitialized: Boolean = False;

type
  TOwnedHandleStream = class(THandleStream)
  public
    destructor Destroy; override;
  end;

destructor TOwnedHandleStream.Destroy;
begin
{$IFDEF UNIX}
  if Handle >= 0 then fpClose(Handle);
{$ELSE}
{$IFDEF MSWINDOWS}
  if Handle <> INVALID_HANDLE_VALUE then Windows.CloseHandle(Handle);
{$ENDIF}
{$ENDIF}
  inherited Destroy;
end;

function PathSeparator(Platform: TStoragePlatform): Char;
begin
  if Platform = spWindows then Result := '\' else Result := '/';
end;

function IsAbsolutePath(const Value: string;
  Platform: TStoragePlatform): Boolean;
begin
  if Value = '' then Exit(False);
  if Platform <> spWindows then Exit(Value[1] = '/');
  Result := ((Length(Value) >= 3) and (Value[2] = ':') and
    (Value[3] in ['/', '\'])) or
    ((Length(Value) >= 2) and (Value[1] in ['/', '\']) and
     (Value[2] in ['/', '\']));
end;

function JoinPath(const Base, Child: string;
  Platform: TStoragePlatform): string;
var Sep: Char;
begin
  Sep := PathSeparator(Platform);
  if Base = '' then Exit('');
  Result := Base;
  if not (Result[Length(Result)] in ['/', '\']) then Result := Result + Sep;
  Result := Result + Child;
end;

function AddApplicationName(const Base: string;
  Platform: TStoragePlatform): string;
begin
  Result := JoinPath(Base, ApplicationStorageName, Platform);
end;

function ResolveUserStoragePaths(const Environment: TStorageEnvironment):
  TUserStoragePaths;
var
  ConfigBase, StateBase, CacheBase, Home: string;
begin
  Result.Config := '';
  Result.State := '';
  Result.Cache := '';
  Result.Portable := Environment.PortableRequested;
  if Result.Portable then
  begin
    Result.Config := JoinPath(Environment.PortableBase, 'config',
      Environment.Platform);
    Result.State := JoinPath(Environment.PortableBase, 'state',
      Environment.Platform);
    Result.Cache := JoinPath(Environment.PortableBase, 'cache',
      Environment.Platform);
    Exit;
  end;

  if Environment.Platform = spWindows then
  begin
    Home := Environment.UserProfile;
    if Home = '' then Home := Environment.Home;
    if not IsAbsolutePath(Home, spWindows) then Home := '';
  end
  else
  begin
    Home := Environment.Home;
    if not IsAbsolutePath(Home, spUnix) then Home := '';
  end;
  case Environment.Platform of
    spWindows:
      begin
        ConfigBase := Environment.AppData;
        StateBase := Environment.LocalAppData;
        CacheBase := Environment.LocalAppData;
        if not IsAbsolutePath(ConfigBase, spWindows) then ConfigBase :=
          JoinPath(JoinPath(Home, 'AppData', spWindows), 'Roaming', spWindows);
        if not IsAbsolutePath(StateBase, spWindows) then StateBase :=
          JoinPath(JoinPath(Home, 'AppData', spWindows), 'Local', spWindows);
        if not IsAbsolutePath(CacheBase, spWindows) then CacheBase := StateBase;
      end;
    spMacOS:
      begin
        ConfigBase := JoinPath(JoinPath(Home, 'Library', spMacOS),
          'Preferences', spMacOS);
        StateBase := JoinPath(JoinPath(JoinPath(Home, 'Library', spMacOS),
          'Application Support', spMacOS), ApplicationStorageName, spMacOS);
        CacheBase := JoinPath(JoinPath(Home, 'Library', spMacOS),
          'Caches', spMacOS);
      end;
    else
      begin
        ConfigBase := Environment.XdgConfigHome;
        StateBase := Environment.XdgStateHome;
        CacheBase := Environment.XdgCacheHome;
        if not IsAbsolutePath(ConfigBase, spUnix) then
          ConfigBase := JoinPath(Home, '.config', spUnix);
        if not IsAbsolutePath(StateBase, spUnix) then
          StateBase := JoinPath(JoinPath(Home, '.local', spUnix),
            'state', spUnix);
        if not IsAbsolutePath(CacheBase, spUnix) then
          CacheBase := JoinPath(Home, '.cache', spUnix);
      end;
  end;
  Result.Config := AddApplicationName(ConfigBase, Environment.Platform);
  if Environment.Platform = spMacOS then
  begin
    Result.State := StateBase;
    Result.Cache := AddApplicationName(CacheBase, Environment.Platform);
  end
  else
  begin
    Result.State := AddApplicationName(StateBase, Environment.Platform);
    Result.Cache := AddApplicationName(CacheBase, Environment.Platform);
  end;
end;

function EnvironmentValue(const Name: string): string;
{$IFDEF MSWINDOWS}
var
  WideName, WideValue: UnicodeString;
  Capacity, Count: DWORD;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  WideName := UTF8Decode(Name);
  Capacity := Windows.GetEnvironmentVariableW(PWideChar(WideName), nil, 0);
  if Capacity = 0 then Exit('');
  repeat
    SetLength(WideValue, Capacity);
    Count := Windows.GetEnvironmentVariableW(PWideChar(WideName),
      PWideChar(WideValue), Capacity);
    if Count < Capacity then
    begin
      SetLength(WideValue, Count);
      Exit(UTF8Encode(WideValue));
    end;
    Capacity := Count + 1;
  until Capacity > 32768;
  raise Exception.Create('Windows environment value is too long: ' + Name);
{$ELSE}
  Result := GetEnvironmentVariable(Name);
{$ENDIF}
end;

function HasPortableArgument: Boolean;
var I: Integer;
begin
  Result := False;
  for I := 1 to ParamCount do
    if SameText(ParamStr(I), '--portable') then Exit(True);
end;

function PortableDataBase(const ExecutablePath: string): string;
var
  ExeDir, ContentsDir, BundleDir: string;
begin
  ExeDir := ExtractFilePath(ExecutablePath);
{$IFDEF DARWIN}
  ContentsDir := ExtractFileDir(ExcludeTrailingPathDelimiter(ExeDir));
  if SameText(ExtractFileName(ContentsDir), 'Contents') then
  begin
    BundleDir := ExtractFileDir(ContentsDir);
    Exit(JoinPath(ExtractFileDir(BundleDir), 'TpX-portable-data', spMacOS));
  end;
{$ENDIF}
{$IFDEF MSWINDOWS}
  Result := JoinPath(ExeDir, 'TpX-portable-data', spWindows);
{$ELSE}
  Result := JoinPath(ExeDir, 'TpX-portable-data', spUnix);
{$ENDIF}
end;

function PortableSentinelPath(const ExecutablePath: string): string;
var ExeDir, ContentsDir, BundleDir: string;
begin
  ExeDir := ExtractFilePath(ExecutablePath);
{$IFDEF DARWIN}
  ContentsDir := ExtractFileDir(ExcludeTrailingPathDelimiter(ExeDir));
  if SameText(ExtractFileName(ContentsDir), 'Contents') then
  begin
    BundleDir := ExtractFileDir(ContentsDir);
    Exit(JoinPath(ExtractFileDir(BundleDir), 'TpX.portable', spMacOS));
  end;
{$ENDIF}
  Result := JoinPath(ExeDir, 'TpX.portable',
    {$IFDEF MSWINDOWS}spWindows{$ELSE}spUnix{$ENDIF});
end;

function StorageFileExists(const FileName: string): Boolean; forward;

{$IFDEF MSWINDOWS}
function WindowsExecutablePath: string;
var
  WidePath: UnicodeString;
  Capacity, Count: DWORD;
begin
  Capacity := 512;
  repeat
    SetLength(WidePath, Capacity);
    Count := Windows.GetModuleFileNameW(0, PWideChar(WidePath), Capacity);
    if Count = 0 then RaiseLastOSError;
    if Count < Capacity then
    begin
      SetLength(WidePath, Count);
      Exit(UTF8Encode(WidePath));
    end;
    Capacity := Capacity * 2;
  until Capacity > 32768;
  raise Exception.Create('Windows executable path is too long');
end;
{$ENDIF}

function BuildStorageEnvironment: TStorageEnvironment;
begin
  Result.Home := '';
  Result.XdgConfigHome := '';
  Result.XdgStateHome := '';
  Result.XdgCacheHome := '';
  Result.AppData := '';
  Result.LocalAppData := '';
  Result.UserProfile := '';
  Result.ExecutablePath := '';
  Result.PortableBase := '';
  Result.PortableRequested := False;
{$IFDEF MSWINDOWS}
  Result.Platform := spWindows;
  Result.AppData := EnvironmentValue('APPDATA');
  Result.LocalAppData := EnvironmentValue('LOCALAPPDATA');
  Result.UserProfile := EnvironmentValue('USERPROFILE');
{$ELSE}
{$IFDEF DARWIN}
  Result.Platform := spMacOS;
{$ELSE}
  Result.Platform := spUnix;
  Result.XdgConfigHome := EnvironmentValue('XDG_CONFIG_HOME');
  Result.XdgStateHome := EnvironmentValue('XDG_STATE_HOME');
  Result.XdgCacheHome := EnvironmentValue('XDG_CACHE_HOME');
{$ENDIF}
{$ENDIF}
  Result.Home := EnvironmentValue('HOME');
{$IFDEF MSWINDOWS}
  Result.ExecutablePath := WindowsExecutablePath;
{$ELSE}
  Result.ExecutablePath := ExpandFileName(ParamStr(0));
{$ENDIF}
  Result.PortableRequested := HasPortableArgument or
    StorageFileExists(PortableSentinelPath(Result.ExecutablePath));
  if Result.PortableRequested then
    Result.PortableBase := PortableDataBase(Result.ExecutablePath);
end;

function CurrentUserStoragePaths: TUserStoragePaths;
begin
  if not PathsInitialized then
  begin
    CachedPaths := ResolveUserStoragePaths(BuildStorageEnvironment);
    PathsInitialized := True;
  end;
  Result := CachedPaths;
end;

function UserStorageDirectory(Area: TUserStorageArea): string;
var Paths: TUserStoragePaths;
begin
  Paths := CurrentUserStoragePaths;
  case Area of
    usaConfig: Result := Paths.Config;
    usaState: Result := Paths.State;
    usaCache: Result := Paths.Cache;
  end;
end;

function IsSymbolicLink(const FileName: string): Boolean;
{$IFDEF UNIX}
var Info: Stat;
{$ENDIF}
{$IFDEF MSWINDOWS}
var Attributes: DWORD; WideName: UnicodeString;
{$ENDIF}
begin
{$IFDEF UNIX}
  Result := (fpLStat(PChar(FileName), Info) = 0) and
    ((Info.st_mode and S_IFMT) = S_IFLNK);
{$ELSE}
{$IFDEF MSWINDOWS}
  WideName := UTF8Decode(FileName);
  Attributes := Windows.GetFileAttributesW(PWideChar(WideName));
  Result := (Attributes <> INVALID_FILE_ATTRIBUTES) and
    ((Attributes and FILE_ATTRIBUTE_REPARSE_POINT) <> 0);
{$ELSE}
  Result := False;
{$ENDIF}
{$ENDIF}
end;

function IsRegularNonLinkFile(const FileName: string): Boolean;
{$IFDEF UNIX}
var Info: Stat;
{$ENDIF}
{$IFDEF MSWINDOWS}
var Attributes: DWORD; WideName: UnicodeString;
{$ENDIF}
begin
{$IFDEF UNIX}
  Result := (fpLStat(PChar(FileName), Info) = 0) and
    ((Info.st_mode and S_IFMT) = S_IFREG);
{$ELSE}
{$IFDEF MSWINDOWS}
  WideName := UTF8Decode(FileName);
  Attributes := Windows.GetFileAttributesW(PWideChar(WideName));
  Result := (Attributes <> INVALID_FILE_ATTRIBUTES) and
    ((Attributes and (FILE_ATTRIBUTE_DIRECTORY or
      FILE_ATTRIBUTE_REPARSE_POINT)) = 0);
{$ELSE}
  Result := FileExists(FileName) and not DirectoryExists(FileName);
{$ENDIF}
{$ENDIF}
end;

function StorageFileExists(const FileName: string): Boolean;
{$IFDEF MSWINDOWS}
var Attributes: DWORD; WideName: UnicodeString;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  WideName := UTF8Decode(FileName);
  Attributes := Windows.GetFileAttributesW(PWideChar(WideName));
  Result := (Attributes <> INVALID_FILE_ATTRIBUTES) and
    ((Attributes and FILE_ATTRIBUTE_DIRECTORY) = 0);
{$ELSE}
  Result := FileExists(FileName);
{$ENDIF}
end;

function StorageDirectoryExists(const DirectoryName: string): Boolean;
{$IFDEF MSWINDOWS}
var Attributes: DWORD; WideName: UnicodeString;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  WideName := UTF8Decode(DirectoryName);
  Attributes := Windows.GetFileAttributesW(PWideChar(WideName));
  Result := (Attributes <> INVALID_FILE_ATTRIBUTES) and
    ((Attributes and FILE_ATTRIBUTE_DIRECTORY) <> 0);
{$ELSE}
  Result := DirectoryExists(DirectoryName);
{$ENDIF}
end;

function EnsureStorageDirectories(const DirectoryName: string): Boolean;
{$IFDEF MSWINDOWS}
var
  ParentDirectory: string;
  WideName: UnicodeString;
  ErrorCode: DWORD;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  if StorageDirectoryExists(DirectoryName) then Exit(True);
  ParentDirectory := ExcludeTrailingPathDelimiter(ExtractFileDir(
    ExcludeTrailingPathDelimiter(DirectoryName)));
  if (ParentDirectory = '') or SameText(ParentDirectory, DirectoryName) then
    Exit(False);
  if not EnsureStorageDirectories(ParentDirectory) then Exit(False);
  WideName := UTF8Decode(DirectoryName);
  if Windows.CreateDirectoryW(PWideChar(WideName), nil) then Exit(True);
  ErrorCode := Windows.GetLastError;
  Result := (ErrorCode = ERROR_ALREADY_EXISTS) and
    StorageDirectoryExists(DirectoryName);
  if not Result then Windows.SetLastError(ErrorCode);
{$ELSE}
  Result := StorageDirectoryExists(DirectoryName) or
    ForceDirectories(DirectoryName);
{$ENDIF}
end;

function DeleteStorageFile(const FileName: string): Boolean;
{$IFDEF MSWINDOWS}
var WideName: UnicodeString;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  WideName := UTF8Decode(FileName);
  Result := Windows.DeleteFileW(PWideChar(WideName));
{$ELSE}
  Result := DeleteFile(FileName);
{$ENDIF}
end;

function OpenStorageReadFile(const FileName: string): TStream;
{$IFDEF MSWINDOWS}
var Handle: THandle; WideName: UnicodeString;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  WideName := UTF8Decode(FileName);
  Handle := Windows.CreateFileW(PWideChar(WideName), GENERIC_READ,
    FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if Handle = INVALID_HANDLE_VALUE then RaiseLastOSError;
  Result := TOwnedHandleStream.Create(Handle);
{$ELSE}
  Result := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
{$ENDIF}
end;

function EnsureUserStorageDirectory(Area: TUserStorageArea;
  out ErrorText: string): Boolean;
var DirectoryName: string;
begin
  DirectoryName := UserStorageDirectory(Area);
  ErrorText := '';
  Result := False;
  if DirectoryName = '' then
  begin
    ErrorText := 'No per-user storage path is available';
    Exit;
  end;
  if IsSymbolicLink(ExcludeTrailingPathDelimiter(DirectoryName)) then
  begin
    ErrorText := 'Storage directory is a symbolic link: ' + DirectoryName;
    Exit;
  end;
  try
    if not StorageDirectoryExists(DirectoryName) and
      not EnsureStorageDirectories(DirectoryName) then
    begin
      ErrorText := 'Could not create storage directory: ' + DirectoryName;
      Exit;
    end;
    if not StorageDirectoryExists(DirectoryName) then
    begin
      ErrorText := 'Storage directory is not accessible: ' + DirectoryName;
      Exit;
    end;
  except
    on E: Exception do
    begin
      ErrorText := 'Could not use storage directory ' + DirectoryName + ': ' +
        E.Message;
      Exit;
    end;
  end;
  Result := True;
end;

function DiagnosticChildPath(const Base, Child: string): string;
begin
  if Base = '' then Exit('(unavailable)');
  Result := IncludeTrailingPathDelimiter(Base) + Child;
end;

function UserStorageDiagnostics: string;
var Paths: TUserStoragePaths;
begin
  Paths := CurrentUserStoragePaths;
  if Paths.Portable then Result := 'Mode: portable' + LineEnding
  else Result := 'Mode: installed' + LineEnding;
  Result := Result + 'Preferences: ' + DiagnosticChildPath(Paths.Config,
    'TpX.ini') + LineEnding + 'Recovery drafts: ' +
    DiagnosticChildPath(Paths.State, 'AutoSaveRecovery') + LineEnding +
    'TeX preview cache: ' + DiagnosticChildPath(Paths.Cache, 'preview-cache');
end;

{$IFDEF UNIX}
function CreateExclusiveFile(const FileName: string): TStream;
var Handle: cint;
begin
  Handle := fpOpen(PChar(FileName), O_WRONLY or O_CREAT or O_EXCL, &600);
  if Handle < 0 then RaiseLastOSError;
  Result := TOwnedHandleStream.Create(Handle);
end;
{$ELSE}
{$IFDEF MSWINDOWS}
function CreateExclusiveFile(const FileName: string): TStream;
var Handle: THandle; WideName: UnicodeString;
begin
  WideName := UTF8Decode(FileName);
  Handle := Windows.CreateFileW(PWideChar(WideName), GENERIC_WRITE, 0, nil,
    CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);
  if Handle = INVALID_HANDLE_VALUE then RaiseLastOSError;
  Result := TOwnedHandleStream.Create(Handle);
end;
{$ELSE}
function CreateExclusiveFile(const FileName: string): TStream;
begin
  Result := TFileStream.Create(FileName, fmCreate);
end;
{$ENDIF}
{$ENDIF}

function PublishWithoutReplacing(const TemporaryFile, DestinationFile: string):
  Boolean;
{$IFDEF UNIX}
begin
  Result := fpLink(PChar(TemporaryFile), PChar(DestinationFile)) = 0;
  if Result then fpUnlink(PChar(TemporaryFile));
end;
{$ELSE}
{$IFDEF MSWINDOWS}
var TemporaryWide, DestinationWide: UnicodeString;
begin
  TemporaryWide := UTF8Decode(TemporaryFile);
  DestinationWide := UTF8Decode(DestinationFile);
  Result := Windows.MoveFileW(PWideChar(TemporaryWide),
    PWideChar(DestinationWide));
end;
{$ELSE}
begin
  Result := not FileExists(DestinationFile) and
    RenameFile(TemporaryFile, DestinationFile);
end;
{$ENDIF}
{$ENDIF}

function ReplaceFileAtomically(const TemporaryFile, DestinationFile: string):
  Boolean;
{$IFDEF UNIX}
begin
  Result := fpRename(PChar(TemporaryFile), PChar(DestinationFile)) = 0;
end;
{$ELSE}
{$IFDEF MSWINDOWS}
var TemporaryWide, DestinationWide: UnicodeString;
begin
  TemporaryWide := UTF8Decode(TemporaryFile);
  DestinationWide := UTF8Decode(DestinationFile);
  Result := Windows.MoveFileExW(PWideChar(TemporaryWide),
    PWideChar(DestinationWide), MOVEFILE_REPLACE_EXISTING or
    MOVEFILE_WRITE_THROUGH);
end;
{$ELSE}
begin
  Result := RenameFile(TemporaryFile, DestinationFile);
end;
{$ENDIF}
{$ENDIF}

function PrepareSettingsFile(const CurrentFile, LegacyFile: string;
  out FileToLoad, Diagnostic: string): Boolean;
var
  TemporaryFile, StorageDirectory: string;
  InputStream, OutputStream: TStream;
  ID: TGUID;
  Buffer: array[0..65535] of Byte;
  Count: Longint;
begin
  FileToLoad := '';
  Diagnostic := '';
  Result := False;
  StorageDirectory := ExcludeTrailingPathDelimiter(ExtractFileDir(CurrentFile));
  if IsSymbolicLink(StorageDirectory) then
  begin
    Diagnostic := 'Preferences directory is a symbolic link; refusing to use it: ' +
      StorageDirectory;
    Exit;
  end;
  if IsSymbolicLink(CurrentFile) then
  begin
    Diagnostic := 'Preferences file is a symbolic link; refusing to read or ' +
      'overwrite it: ' + CurrentFile;
    Exit;
  end;
  if IsRegularNonLinkFile(CurrentFile) then
  begin
    FileToLoad := CurrentFile;
    Exit(True);
  end;
  if StorageFileExists(CurrentFile) or StorageDirectoryExists(CurrentFile) then
  begin
    Diagnostic := 'Preferences path is not a regular file: ' + CurrentFile;
    Exit;
  end;
  if LegacyFile = '' then Exit(True);
  StorageDirectory := ExcludeTrailingPathDelimiter(ExtractFileDir(LegacyFile));
  if IsSymbolicLink(StorageDirectory) then
  begin
    Diagnostic := 'Legacy preferences directory is a symbolic link; ' +
      'migration was skipped: ' + StorageDirectory;
    Exit(True);
  end;
  if IsSymbolicLink(LegacyFile) then
  begin
    Diagnostic := 'Legacy preferences are a symbolic link; migration was ' +
      'skipped: ' + LegacyFile;
    Exit(True);
  end;
  if not IsRegularNonLinkFile(LegacyFile) then Exit(True);

  TemporaryFile := '';
  try
    if not EnsureStorageDirectories(ExtractFileDir(CurrentFile)) then
      raise Exception.Create('Could not create ' +
        ExtractFileDir(CurrentFile));
    if CreateGUID(ID) <> 0 then
      raise Exception.Create('Could not generate a temporary file name');
    TemporaryFile := CurrentFile + '.migrate-' +
      StringReplace(StringReplace(GUIDToString(ID), '{', '', []), '}', '', []);
    InputStream := OpenStorageReadFile(LegacyFile);
    try
      OutputStream := CreateExclusiveFile(TemporaryFile);
      try
        repeat
          Count := InputStream.Read(Buffer, SizeOf(Buffer));
          if Count > 0 then OutputStream.WriteBuffer(Buffer, Count);
        until Count = 0;
      finally
        OutputStream.Free;
      end;
    finally
      InputStream.Free;
    end;
    if not PublishWithoutReplacing(TemporaryFile, CurrentFile) then
    begin
      DeleteStorageFile(TemporaryFile);
      if IsSymbolicLink(CurrentFile) then
        raise Exception.Create('Destination became a symbolic link')
      else if IsRegularNonLinkFile(CurrentFile) then
        FileToLoad := CurrentFile
      else
        raise Exception.Create('Could not publish migrated preferences');
    end
    else
      FileToLoad := CurrentFile;
    Result := True;
  except
    on E: Exception do
    begin
      if TemporaryFile <> '' then DeleteStorageFile(TemporaryFile);
      if IsRegularNonLinkFile(LegacyFile) then FileToLoad := LegacyFile;
      Diagnostic := 'Could not migrate legacy preferences to ' + CurrentFile +
        ': ' + E.Message + LineEnding + 'The original was kept; settings were ' +
        'loaded from ' + FileToLoad;
      Result := FileToLoad <> '';
    end;
  end;
end;

function SaveSettingsFileSafely(const FileName: string; Contents: TStrings;
  out ErrorText: string): Boolean;
var
  TemporaryFile, StorageDirectory: string;
  OutputStream: TStream;
  ID: TGUID;
begin
  Result := False;
  ErrorText := '';
  TemporaryFile := '';
  try
    StorageDirectory := ExcludeTrailingPathDelimiter(ExtractFileDir(FileName));
    if IsSymbolicLink(StorageDirectory) then
      raise Exception.Create('Preferences directory is a symbolic link');
    if IsSymbolicLink(FileName) then
      raise Exception.Create('Preferences file is a symbolic link');
    if StorageFileExists(FileName) and not IsRegularNonLinkFile(FileName) then
      raise Exception.Create('Preferences path is not a regular file');
    if not EnsureStorageDirectories(ExtractFileDir(FileName)) then
      raise Exception.Create('Could not create ' + ExtractFileDir(FileName));
    if CreateGUID(ID) <> 0 then
      raise Exception.Create('Could not generate a temporary file name');
    TemporaryFile := FileName + '.tmp-' +
      StringReplace(StringReplace(GUIDToString(ID), '{', '', []), '}', '', []);
    OutputStream := CreateExclusiveFile(TemporaryFile);
    try
      Contents.SaveToStream(OutputStream);
    finally
      OutputStream.Free;
    end;
    if IsSymbolicLink(FileName) then
      raise Exception.Create('Preferences file became a symbolic link');
    if not ReplaceFileAtomically(TemporaryFile, FileName) then
      raise Exception.Create('Could not replace preferences file');
    Result := True;
  except
    on E: Exception do ErrorText := 'Could not save preferences to ' +
      FileName + ': ' + E.Message;
  end;
  if (not Result) and (TemporaryFile <> '') then DeleteStorageFile(TemporaryFile);
end;

function InstallUserFileIfMissing(const SourceFile, DestinationFile: string;
  out ErrorText: string): Boolean;
var
  TemporaryFile, DestinationDirectory: string;
  InputStream, OutputStream: TStream;
  ID: TGUID;
  Buffer: array[0..65535] of Byte;
  Count: Longint;
begin
  Result := False;
  ErrorText := '';
  TemporaryFile := '';
  try
    DestinationDirectory := ExcludeTrailingPathDelimiter(
      ExtractFileDir(DestinationFile));
    if IsSymbolicLink(DestinationDirectory) then
      raise Exception.Create('User directory is a symbolic link');
    if IsSymbolicLink(DestinationFile) then
      raise Exception.Create('User file is a symbolic link');
    if IsRegularNonLinkFile(DestinationFile) then Exit(True);
    if StorageFileExists(DestinationFile) or
      StorageDirectoryExists(DestinationFile) then
      raise Exception.Create('User path is not a regular file');
    if not IsRegularNonLinkFile(SourceFile) then
      raise Exception.Create('Packaged source is missing or not a regular file');
    if not EnsureStorageDirectories(ExtractFileDir(DestinationFile)) then
      raise Exception.Create('Could not create ' +
        ExtractFileDir(DestinationFile));
    if CreateGUID(ID) <> 0 then
      raise Exception.Create('Could not generate a temporary file name');
    TemporaryFile := DestinationFile + '.copy-' +
      StringReplace(StringReplace(GUIDToString(ID), '{', '', []), '}', '', []);
    InputStream := OpenStorageReadFile(SourceFile);
    try
      OutputStream := CreateExclusiveFile(TemporaryFile);
      try
        repeat
          Count := InputStream.Read(Buffer, SizeOf(Buffer));
          if Count > 0 then OutputStream.WriteBuffer(Buffer, Count);
        until Count = 0;
      finally
        OutputStream.Free;
      end;
    finally
      InputStream.Free;
    end;
    if not PublishWithoutReplacing(TemporaryFile, DestinationFile) then
    begin
      DeleteStorageFile(TemporaryFile);
      if IsSymbolicLink(DestinationFile) then
        raise Exception.Create('User path became a symbolic link')
      else if IsRegularNonLinkFile(DestinationFile) then
        Exit(True)
      else
        raise Exception.Create('Could not publish user file');
    end;
    Result := True;
  except
    on E: Exception do ErrorText := 'Could not initialize ' + DestinationFile +
      ' from ' + SourceFile + ': ' + E.Message;
  end;
  if (not Result) and (TemporaryFile <> '') then DeleteStorageFile(TemporaryFile);
end;

end.

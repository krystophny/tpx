unit SysBasic;

interface

uses Types,
{$IFDEF FPC}
  LCLIntf, LCLType, LazBasic,
{$IFDEF LINUX}
  {oldlinux,}
{$ENDIF}
{$ELSE}
  Windows, ShellAPI, WinBasic,
{$ENDIF}
{$IFDEF FPC}
  {$IFDEF UNIX}BaseUnix,{$ENDIF}
{$ENDIF}
  SysUtils, Forms, Graphics;

type
  TRequiredTool = (rtGhostscript, rtLaTeX, rtPdfLaTeX, rtDvips);

function ResolveToolPath(var FilePath: string;
  const ExecutableNames: array of string): Boolean;
procedure ReportMissingTool(const Tool: TRequiredTool; const Purpose: string);
function CheckRequiredTool(var FilePath: string; const Tool: TRequiredTool;
  const Purpose: string): Boolean;

function CheckFilePath(var FilePath: string;
  const FileDescription: string): Boolean;
function PrepareFilePath(const FilePath: string): string;
function TpXResourcePath(const FileName: string): string;
function TpXTemplatePath(const FileName: string): string;
function FileExec(const aCmdLine, InFile, OutFile, Directory:
  string; aHide, aWait: Boolean): Boolean;
procedure OpenOrExec(const ViewerPath, FileName: string);
function QuoteShellArg(const Value: string): string;
function TryDimension(const Text: string; out Value: Single;
  const AllowZero: Boolean): Boolean;
function TryDeleteFile(const FileName: string): Boolean;
function RenameFile(const FileName1, FileName2: string): Boolean;
function CopyFile(const FileName1, FileName2: string): Boolean;
function SameExistingFile(const FileName1, FileName2: string): Boolean;
function GetTempDir: string;
procedure MessageBoxInfo(MsgStr: string);
procedure MessageBoxError(MsgStr: string);
function MessageBoxErrorAsk(MsgStr: string): Boolean;
function GetFontDescent(
  const FaceName: string;
  const Style: TFontStyles;
  const Charset: TFontCharSet): Single;
function GetFontInternalLeading(
  const FaceName: string;
  const Style: TFontStyles;
  const Charset: TFontCharSet): Single;
// Get text width end descent as a percentage of em height
procedure GetTextDimension(
  const WideText: Widestring;
  const FaceName: string;
  const Style: TFontStyles;
  const Charset: TFontCharSet;
  out Width, Descent: Single);
procedure ShowProgress(Percentage: Double);
function MulDiv(Number, Numerator, Denominator: Integer): Integer;
function SmallPointToPoint(const P: TSmallPoint): TPoint;
{$IFNDEF FPC}
procedure FastAntiAliasPicture(big_bmp, out_bmp: TBitmap);
{$ENDIF}
procedure StartTimer;
function GetTimer: Extended;

type

{: This is the base class from which any exception raised by
   library's functions are derived.

   If you want to define your own exception consider to derive
   it from this class.
}
  ETpX_SysException = class(Exception);

var
  StartTime: Cardinal;
  { Command-line conversion runs without a GUI: messages go to stderr. }
  ConsoleMessages: Boolean = False;
  ConsoleErrorReported: Boolean = False;

procedure WriteConsoleMessage(const MsgStr: string);

implementation

uses
//WinBasic,
  MainUnit, Dialogs, Controls, Buttons, Clipbrd, Classes, Math{$IFDEF FPC},
{$IFNDEF WASI}
  Process{$IFDEF MSWINDOWS}, ProcessUnicode{$ENDIF},
{$ENDIF}
  {$IFDEF WINDOWS}Windows,{$ENDIF} ExtCtrls
{$IFNDEF CPUWASM32}
  , UserStorage
{$ENDIF}
{$ENDIF};

{$IF DEFINED(FPC) AND DEFINED(MSWINDOWS)}
function c_GetTempPathW(BufferLength: Cardinal; Buffer: PWideChar): Cardinal;
  stdcall; external 'kernel32' name 'GetTempPathW';
{$ENDIF}

{$IF DEFINED(FPC) AND NOT DEFINED(WASI)}
type
{$IFDEF MSWINDOWS}
  TFileExecProcess = ProcessUnicode.TProcess;
{$ELSE}
  TFileExecProcess = TProcess;
{$ENDIF}

  TBackgroundTool = class(TComponent)
  private
    Child: TFileExecProcess;
    Timer: TTimer;
    procedure Poll(Sender: TObject);
  public
    constructor Create(AChild: TFileExecProcess); reintroduce;
    destructor Destroy; override;
  end;

constructor TBackgroundTool.Create(AChild: TFileExecProcess);
begin
  inherited Create(Application);
  Timer := TTimer.Create(Self);
  Timer.Enabled := False;
  Timer.Interval := 100;
  Timer.OnTimer := Poll;
  Child := AChild;
  Timer.Enabled := True;
end;

destructor TBackgroundTool.Destroy;
begin
  if Assigned(Timer) then Timer.Enabled := False;
  Child.Free;
  inherited Destroy;
end;

procedure TBackgroundTool.Poll(Sender: TObject);
begin
  if not Child.Running then Free;
end;
{$ENDIF}

function QuoteShellArg(const Value: string): string;
var
  Q: string;
begin
{$IFDEF UNIX}
  Q := Chr(39);
  Result := Q + StringReplace(Value, Q, Q + '\' + Q + Q, [rfReplaceAll]) + Q;
{$ELSE}
  Result := AnsiQuotedStr(Value, '"');
{$ENDIF}
end;

function TpXResourcePath(const FileName: string): string;
var
  ExeDir: string;
{$IFDEF DARWIN}
  ContentsDir, BundlePath: string;
{$ENDIF}
begin
  ExeDir := ExtractFilePath(Application.ExeName);
{$IFDEF DARWIN}
  ContentsDir := ExtractFileDir(ExcludeTrailingPathDelimiter(ExeDir));
  if SameText(ExtractFileName(ContentsDir), 'Contents') and
    FileExists(IncludeTrailingPathDelimiter(ContentsDir) + 'Info.plist') then
  begin
    BundlePath := IncludeTrailingPathDelimiter(
      IncludeTrailingPathDelimiter(ContentsDir) + 'Resources') + FileName;
    if FileExists(BundlePath) or DirectoryExists(BundlePath) then
    begin
      Result := BundlePath;
      Exit;
    end;
  end;
{$ENDIF}
  Result := IncludeTrailingPathDelimiter(ExeDir) + FileName;
end;

function TpXTemplatePath(const FileName: string): string;
{$IFDEF FPC}
{$IFNDEF CPUWASM32}
var
  ResourcePath, ErrorText: string;
{$ENDIF}
{$ELSE}
{$IFDEF DARWIN}
var
  ResourcePath, ConfigDir: string;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF FPC}
{$IFNDEF CPUWASM32}
  if (FileName = '') or (FileName = '.') or (FileName = '..') or
    (Pos('/', FileName) > 0) or (Pos('\', FileName) > 0) then
    raise Exception.Create('Invalid TpX template name: ' + FileName);
  Result := IncludeTrailingPathDelimiter(UserStorageDirectory(usaConfig)) +
    FileName;
  ResourcePath := TpXResourcePath(FileName);
  if not FileExists(ResourcePath) and not FileExists(Result) then
  begin
    if not EnsureUserStorageDirectory(usaConfig, ErrorText) then
      raise Exception.Create(ErrorText);
    Exit;
  end;
  if not EnsureUserStorageDirectory(usaConfig, ErrorText) then
    raise Exception.Create(ErrorText);
  if not InstallUserFileIfMissing(ResourcePath, Result, ErrorText) then
    raise Exception.Create(ErrorText);
{$ELSE}
  Result := TpXResourcePath(FileName);
{$ENDIF}
{$ELSE}
{$IFDEF DARWIN}
  ConfigDir := IncludeTrailingPathDelimiter(GetAppConfigDir(False));
  Result := ConfigDir + FileName;
  if FileExists(Result) then Exit;
  if not ForceDirectories(ExtractFileDir(Result)) and
    not DirectoryExists(ExtractFileDir(Result)) then
    raise Exception.Create('Could not create TpX settings directory');
  ResourcePath := TpXResourcePath(FileName);
  if FileExists(ResourcePath) and not CopyFile(ResourcePath, Result) then
    raise Exception.Create('Could not copy TpX template to user settings');
{$ELSE}
  Result := TpXResourcePath(FileName);
{$ENDIF}
{$ENDIF}
end;

function TryDimension(const Text: string; out Value: Single;
  const AllowZero: Boolean): Boolean;
begin
  Result := TryStrToFloat(Text, Value);
  if not Result then Exit;
  Result := not IsNan(Value) and not IsInfinite(Value) and
    ((Value > 0) or (AllowZero and (Value = 0)));
end;

function ResolveToolPath(var FilePath: string;
  const ExecutableNames: array of string): Boolean;
var
  SearchPath, Candidate, Configured: string;
  I: Integer;
{$IFDEF MSWINDOWS}
  Directories: TStringList;
  Entry: TSearchRec;
  Base: string;
  procedure AddSubdirectories(const Parent, Suffix: string);
  begin
    if FindFirst(IncludeTrailingPathDelimiter(Parent) + '*', faDirectory,
      Entry) <> 0 then Exit;
    try
      repeat
        if ((Entry.Attr and faDirectory) <> 0) and
          (Entry.Name <> '.') and (Entry.Name <> '..') then
          Directories.Add(IncludeTrailingPathDelimiter(Parent) +
            Entry.Name + Suffix);
      until FindNext(Entry) <> 0;
    finally
      SysUtils.FindClose(Entry);
    end;
  end;
{$ENDIF}
  function Usable(const Path: string): Boolean;
  begin
    Result := (Path <> '') and FileExists(Path);
{$IFDEF UNIX}{$IFNDEF CPUWASM32}
    if Result then Result := fpAccess(PChar(Path), X_OK) = 0;
{$ENDIF}{$ENDIF}
  end;
  function FindExecutable(const Name: string): Boolean;
  var
    Remaining, Directory: string;
    Separator: Integer;
  begin
    Result := False;
    if Name = '' then Exit;
    Remaining := SearchPath;
    while Remaining <> '' do
    begin
      Separator := Pos(PathSeparator, Remaining);
      if Separator = 0 then Separator := Length(Remaining) + 1;
      Directory := Copy(Remaining, 1, Separator - 1);
      Delete(Remaining, 1, Separator);
      Candidate := IncludeTrailingPathDelimiter(Directory) + Name;
      if Usable(Candidate) then
      begin
        FilePath := ExpandFileName(Candidate);
        Exit(True);
      end;
    end;
  end;
begin
  Result := False;
  Configured := Trim(FilePath);
  if (Length(Configured) > 1) and (Configured[1] in ['"', '''']) then
    Configured := AnsiDequotedStr(Configured, Configured[1]);
  if Usable(Configured) then
  begin
    FilePath := ExpandFileName(Configured);
    Exit(True);
  end;
  SearchPath := SysUtils.GetEnvironmentVariable('PATH');
  if (ExtractFilePath(Configured) = '') and FindExecutable(Configured) then
    Exit(True);
  for I := 0 to High(ExecutableNames) do
    if FindExecutable(ExecutableNames[I]) then Exit(True);
{$IFDEF DARWIN}
  SearchPath := '/Library/TeX/texbin:/opt/homebrew/bin:/usr/local/bin:/opt/local/bin';
{$ELSE}
{$IFDEF MSWINDOWS}
  Directories := TStringList.Create;
  try
    Base := SysUtils.GetEnvironmentVariable('ProgramFiles');
    AddSubdirectories(Base + '\gs', '\bin');
    Directories.Add(Base + '\MiKTeX\miktex\bin\x64');
    Base := SysUtils.GetEnvironmentVariable('ProgramFiles(x86)');
    AddSubdirectories(Base + '\gs', '\bin');
    Directories.Add(Base + '\MiKTeX\miktex\bin');
    Directories.Add(SysUtils.GetEnvironmentVariable('LOCALAPPDATA') +
      '\Programs\MiKTeX\miktex\bin\x64');
    AddSubdirectories('C:\texlive', '\bin\windows');
    SearchPath := '';
    for I := 0 to Directories.Count - 1 do
      SearchPath := SearchPath + Directories[I] + PathSeparator;
  finally
    Directories.Free;
  end;
{$ELSE}
  SearchPath := '/usr/local/bin:/usr/bin:/bin:/snap/bin';
{$ENDIF}{$ENDIF}
  for I := 0 to High(ExecutableNames) do
    if FindExecutable(ExecutableNames[I]) then Exit(True);
end;

procedure ReportMissingTool(const Tool: TRequiredTool; const Purpose: string);
{$IFNDEF CPUWASM32}
const
  Names: array[TRequiredTool] of string =
    ('Ghostscript', 'LaTeX', 'pdfLaTeX', 'dvips');
var
  InstallHint, CopyText, MessageText, PackageName, Distribution: string;
  Dialog: TForm;
  Button: TBitBtn;
  I, Choice, ButtonLeft, ButtonResult: Integer;
{$IFDEF LINUX}
  OSRelease: TStringList;
{$ENDIF}
{$ENDIF}
begin
{$IFNDEF CPUWASM32}
  CopyText := '';
  if Tool = rtGhostscript then PackageName := 'ghostscript'
  else PackageName := 'texlive-latex-extra texlive-pictures';
{$IFDEF DARWIN}
  if Tool = rtGhostscript then InstallHint := 'brew install ghostscript'
  else InstallHint := 'brew install --cask mactex-no-gui';
  CopyText := InstallHint;
  if Tool <> rtGhostscript then
    InstallHint := InstallHint + LineEnding +
      'Or download MacTeX: https://tug.org/mactex/';
{$ELSE}
{$IFDEF MSWINDOWS}
  if Tool = rtGhostscript then
    InstallHint := 'https://ghostscript.com/releases/gsdnld.html'
  else InstallHint := 'https://miktex.org/download';
  CopyText := InstallHint;
  InstallHint := 'Download and install: ' + InstallHint;
{$ELSE}
  Distribution := '';
{$IFDEF LINUX}
  OSRelease := TStringList.Create;
  try
    if FileExists('/etc/os-release') then
    begin
      OSRelease.LoadFromFile('/etc/os-release');
      Distribution := ' ' + OSRelease.Values['ID'] + ' ' +
        StringReplace(OSRelease.Values['ID_LIKE'], '"', '', [rfReplaceAll]) + ' ';
      Distribution := StringReplace(Distribution, '"', '', [rfReplaceAll]);
    end;
  finally
    OSRelease.Free;
  end;
{$ENDIF}
  if Pos(' arch ', Distribution) > 0 then
  begin
    if Tool <> rtGhostscript then PackageName := 'texlive-latexextra texlive-pictures';
    InstallHint := 'sudo pacman -S ' + PackageName;
  end
  else if (Pos(' fedora ', Distribution) > 0) or
    (Pos(' rhel ', Distribution) > 0) then
  begin
    if Tool <> rtGhostscript then PackageName := 'texlive-latex texlive-dvips texlive-pgf texlive-preview';
    InstallHint := 'sudo dnf install ' + PackageName;
  end
  else if (Pos(' debian ', Distribution) > 0) or
    (Pos(' ubuntu ', Distribution) > 0) then
    InstallHint := 'sudo apt install ' + PackageName
  else if Tool = rtGhostscript then
    InstallHint := 'Install Ghostscript using your package manager: https://ghostscript.com/releases/gsdnld.html'
  else InstallHint := 'Install TeX Live using your package manager: https://tug.org/texlive/';
{$ENDIF}{$ENDIF}
  if CopyText = '' then CopyText := InstallHint;
  MessageText := Names[Tool] + ' is needed for ' + Purpose +
    ', but TpX could not find it.' + LineEnding + LineEnding +
    InstallHint + LineEnding + LineEnding +
    'After installing, retry this action. You can also select the executable in TpX settings.';
  if ConsoleMessages then raise ETpX_SysException.Create(MessageText);
  Dialog := CreateMessageDialog(MessageText, mtInformation,
    [mbYes, mbNo, mbCancel]);
  try
    Dialog.Caption := 'Install ' + Names[Tool];
    ButtonLeft := 16;
    for I := 0 to Dialog.ComponentCount - 1 do
      if Dialog.Components[I] is TBitBtn then
      begin
        Button := TBitBtn(Dialog.Components[I]);
        ButtonResult := Button.ModalResult;
        Button.Kind := bkCustom;
        Button.Glyph.Clear;
        Button.ModalResult := ButtonResult;
        case ButtonResult of
          mrYes: Button.Caption := 'Open settings';
          mrNo: Button.Caption := 'Copy command';
          mrCancel: Button.Caption := 'Cancel';
        end;
        Button.Width := Max(Button.Width, Dialog.Canvas.TextWidth(Button.Caption) + 36);
        Button.Left := ButtonLeft;
        Inc(ButtonLeft, Button.Width + 10);
      end;
    Dialog.ClientWidth := Max(Dialog.ClientWidth, ButtonLeft + 6);
    repeat
      Choice := Dialog.ShowModal;
      if Choice = mrNo then Clipboard.AsText := CopyText;
    until Choice <> mrNo;
  finally
    Dialog.Free;
  end;
  if (Choice = mrYes) and Assigned(MainForm) then MainForm.TpXSettings.Execute;
{$ENDIF}
end;

function CheckRequiredTool(var FilePath: string; const Tool: TRequiredTool;
  const Purpose: string): Boolean;
begin
{$IFDEF CPUWASM32}
  Result := CheckFilePath(FilePath, Purpose);
{$ELSE}
  case Tool of
{$IFDEF MSWINDOWS}
    rtGhostscript: Result := ResolveToolPath(FilePath, ['gswin64c.exe', 'gswin32c.exe', 'mgs.exe']);
    rtLaTeX: Result := ResolveToolPath(FilePath, ['latex.exe']);
    rtPdfLaTeX: Result := ResolveToolPath(FilePath, ['pdflatex.exe']);
    rtDvips: Result := ResolveToolPath(FilePath, ['dvips.exe']);
{$ELSE}
    rtGhostscript: Result := ResolveToolPath(FilePath, ['gs']);
    rtLaTeX: Result := ResolveToolPath(FilePath, ['latex']);
    rtPdfLaTeX: Result := ResolveToolPath(FilePath, ['pdflatex']);
    rtDvips: Result := ResolveToolPath(FilePath, ['dvips']);
{$ENDIF}
  end;
  if not Result then ReportMissingTool(Tool, Purpose);
{$ENDIF}
end;

function CheckFilePath(var FilePath: string;
  const FileDescription: string): Boolean;
var
  I: Integer;
  TmpFilePath: string;
  // Checks whether the path exists.
  // Adds a space to the path to indicate problem.
  // If the path already starts with a space, error message does not show up.
begin
  Result := False;
  if Pos(' ', FilePath) = 1 then Exit;
  TmpFilePath := Trim(FilePath);
  if FileExists(TmpFilePath) then
  begin
    Result := True;
    Exit;
  end;
  if (TmpFilePath <> '') and
    not (TmpFilePath[1] in ['"', '''']) then
  begin
    I := Pos(' ', TmpFilePath);
    if I > 0 then TmpFilePath := Copy(TmpFilePath, 1, I - 1);
  end;
  if (TmpFilePath <> '') and (TmpFilePath[1] in ['"', '''']) then
    TmpFilePath := AnsiDequotedStr(TmpFilePath, TmpFilePath[1]);
  if TmpFilePath = '' then
  begin
    MessageBoxError(FileDescription + ' path is empty');
    FilePath := ' ';
    Exit;
  end;
  if ExtractFilePath(TmpFilePath) = '' then
  // If the path is not full, try to find the file
    TmpFilePath := FileSearch(
      TmpFilePath, '.' + PathSeparator +
      ExtractFilePath(ParamStr(0)) + PathSeparator +
      SysUtils.GetEnvironmentVariable('PATH'));
  if not FileExists(TmpFilePath) then
  begin
    MessageBoxError(
      FileDescription + ' path not found: ' + FilePath);
    FilePath := ' ' + FilePath;
    Exit;
  end;
  Result := True;
end;

function PrepareFilePath(const FilePath: string): string;
// Add quotes to execution file
var
  I: Integer;
begin
  if Pos(' ', FilePath) = 1 then Exit;
  Result := Trim(FilePath);
  if Result = '' then Exit;
  if FileExists(Result) then
  begin
    Result := AnsiQuotedStr(Result, '"');
    Exit;
  end;
  if not (Result[1] in ['"', '''']) then
  begin
    I := Pos(' ', Result);
    if I > 0 then Result :=
      AnsiQuotedStr(Copy(Result, 1, I - 1), '"')
        + Copy(Result, I, Length(Result));
  end;
end;

function FileExec(const aCmdLine, InFile, OutFile, Directory:
  string; aHide, aWait: Boolean): Boolean;
{$IFNDEF FPC}
var
  StartupInfo: TSTARTUPINFO;
  ProcessInfo: TPROCESSINFORMATION;
  aInput, aOutput: Integer;
  PDirectory: PChar;
  exitCode: DWORD;
  ExitCodeProcess: Longword;
{$ELSE}
  {$IFNDEF WASI}
var
  Child: TFileExecProcess;
{$IFDEF UNIX}
  Command: string;
{$ENDIF}
  {$ENDIF}
{$ENDIF}
begin
//  MessageBoxInfo(aCmdLine);
{$IFNDEF FPC}
  FillChar(StartupInfo, SizeOf(TSTARTUPINFO), 0);
  try
    //if aWait then aHide := False;
    if OutFile <> '' then
      aOutput := FileCreate(OutFile);
    if InFile <> '' then
      aInput := FileOpen(InFile, fmOpenRead or fmShareDenyNone);
    with StartupInfo do
    begin
      cb := SizeOf(TSTARTUPINFO);
      if aHide then
        wShowWindow := SW_HIDE
      else
        wShowWindow := SW_SHOWNORMAL;
      if (OutFile <> '') or (InFile <> '') then
      begin
        dwFlags := STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK
          or STARTF_USESTDHANDLES;
        if InFile <> '' then
          hStdInput := aInput
        else
          hStdInput := INVALID_HANDLE_VALUE;
        if OutFile <> '' then
          hStdOutput := aOutput
        else
          hStdOutput := INVALID_HANDLE_VALUE;
        hStdError := INVALID_HANDLE_VALUE;
      end
      else
        dwFlags := STARTF_USESHOWWINDOW or
          STARTF_FORCEONFEEDBACK;
    end;
    if Directory = '' then
      PDirectory := nil
    else
      PDirectory := PChar(Directory);
    Result := CreateProcess({lpApplicationName: PChar} nil,
   {lpCommandLine: PChar} PChar(aCmdLine),
    {lpProcessAttributes, lpThreadAttributes:      PSecurityAttributes;}
      nil, nil, {bInheritHandles: BOOL} False,
    {dwCreationFlags: DWORD} CREATE_NEW_CONSOLE or
      NORMAL_PRIORITY_CLASS,
      {lpEnvironment: Pointer} nil,
    {lpCurrentDirectory: PChar} PDirectory,
    {const lpStartupInfo:      TStartupInfo} StartupInfo,
    {var lpProcessInformation: TProcessInformation} ProcessInfo
      ) {: BOOL;      stdcall};
    if aWait then
      //if Result then
    begin
      WaitForInputIdle(ProcessInfo.hProcess, INFINITE);
      //WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
      repeat
        exitCode := WaitForSingleObject(ProcessInfo.hProcess, 100);
        Application.ProcessMessages;
        sleep(50);
      until exitCode <> WAIT_TIMEOUT;
      WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
      GetExitCodeProcess(ProcessInfo.hProcess, ExitCodeProcess);
      if ExitCodeProcess <> 0 then
      begin
        {Application.MessageBox(PChar('Exit code: ' + IntToStr(ExitCodeProcess)),
          'Error', MB_OK);}
        Result := False;
      end;
    end;
    FileClose(ProcessInfo.hProcess); { *Converted from CloseHandle* }
    FileClose(ProcessInfo.hThread); { *Converted from CloseHandle* }
  finally
    if OutFile <> '' then FileClose(aOutput);
    if InFile <> '' then FileClose(aInput);
  end;
{  if Result then
    Application.MessageBox('App successful',
      'Error', MB_OK);}
{$ELSE}
  {$IFDEF WASI}
  { External tools require the desktop version; explain every failed request. }
  Result := False;
  MessageBoxError(Format('"%s" cannot run in the browser: WebAssembly has no '
    + 'external programs. This feature needs the desktop version of TpX.',
    [ExtractFileName(aCmdLine)]));
  {$ELSE}
  Result := False;
  Child := TFileExecProcess.Create(nil);
  try
{$IFDEF MSWINDOWS}
    Child.CurrentDirectory := UTF8Decode(Directory);
{$ELSE}
    Child.CurrentDirectory := Directory;
{$ENDIF}
{$IFDEF UNIX}
    Command := aCmdLine;
    if InFile <> '' then Command := Command + ' < ' + QuoteShellArg(InFile);
    if OutFile <> '' then Command := Command + ' > ' + QuoteShellArg(OutFile);
    Child.Executable := '/bin/sh';
    Child.Parameters.Add('-c');
    Child.Parameters.Add(Command);
{$ELSE}
{$IFDEF MSWINDOWS}
    Child.CommandLine := UTF8Decode(aCmdLine);
    if aHide then Child.Options := Child.Options + [Process.poNoConsole];
{$ELSE}
    Child.CommandLine := aCmdLine;
    if aHide then Child.Options := Child.Options + [Process.poNoConsole];
{$ENDIF}
{$ENDIF}
    if aWait then Child.Options := Child.Options + [Process.poWaitOnExit];
    try
      Child.Execute;
      Result := not aWait or (Child.ExitStatus = 0);
      if not aWait then begin
        TBackgroundTool.Create(Child);
        Child := nil;
      end;
    except
      Result := False;
    end;
  finally
    Child.Free;
  end;
  {$ENDIF}
{$ENDIF}
  if ConsoleMessages and not Result then
    MessageBoxError('External command failed: ' + aCmdLine);
end;

procedure OpenOrExec(const ViewerPath, FileName: string);
begin
  if ViewerPath = '' then
  begin
{$IFDEF FPC}
    if not LCLIntf.OpenDocument(FileName) then
      MessageBoxError('Could not open the default viewer for ' + FileName);
{$ELSE}
    ShellExecute(Application.Handle, PChar('open'), PChar(FileName),
      nil, nil, SW_SHOW);
{$ENDIF}
  end
  else if not FileExec(Format('%s %s',
    [PrepareFilePath(ViewerPath), QuoteShellArg(FileName)]),
    '', '', '', False, False) then
    MessageBoxError('Could not launch viewer: ' + ViewerPath);
end;

function TryDeleteFile(const FileName: string): Boolean;
begin
  if FileExists(FileName)
    then
    Result := SysUtils.DeleteFile(PChar(FileName))
  else
    Result := True;
end;

{$IFDEF FPC}{$IFDEF WINDOWS}
function WindowsExtendedPath(const FileName: string): UnicodeString;
begin
  Result := UTF8Decode(FileName);
  if Length(Result) < MAX_PATH then Exit;
  if Copy(Result, 1, 4) = '\\?\' then Exit;
  if Copy(Result, 1, 2) = '\\' then
    Result := '\\?\UNC\' + Copy(Result, 3, MaxInt)
  else
    Result := '\\?\' + Result;
end;

function SameWindowsFileIdentity(const FileName1, FileName2: string): Boolean;
var
  WideName1, WideName2: UnicodeString;
  Handle1, Handle2: THandle;
  Info1, Info2: TByHandleFileInformation;
begin
  Result := False;
  WideName1 := WindowsExtendedPath(FileName1);
  WideName2 := WindowsExtendedPath(FileName2);
  Handle1 := Windows.CreateFileW(PWideChar(WideName1), 0,
    FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if Handle1 = INVALID_HANDLE_VALUE then Exit;
  try
    Handle2 := Windows.CreateFileW(PWideChar(WideName2), 0,
      FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil,
      OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if Handle2 = INVALID_HANDLE_VALUE then Exit;
    try
      if Windows.GetFileInformationByHandle(Handle1, Info1) and
        Windows.GetFileInformationByHandle(Handle2, Info2) then
        Result := (Info1.dwVolumeSerialNumber = Info2.dwVolumeSerialNumber) and
          (Info1.nFileIndexHigh = Info2.nFileIndexHigh) and
          (Info1.nFileIndexLow = Info2.nFileIndexLow);
    finally
      Windows.CloseHandle(Handle2);
    end;
  finally
    Windows.CloseHandle(Handle1);
  end;
end;
{$ENDIF}{$ENDIF}

function SameExistingFile(const FileName1, FileName2: string): Boolean;
var
  Path1, Path2: string;
{$IFDEF UNIX}
  Info1, Info2: Stat;
{$ENDIF}
begin
  Path1 := ExpandFileName(FileName1);
  Path2 := ExpandFileName(FileName2);
  Result := FileExists(Path1) and FileExists(Path2) and
    SameFileName(Path1, Path2);
{$IFDEF UNIX}
  if not Result and (fpStat(PChar(Path1), Info1) = 0) and
    (fpStat(PChar(Path2), Info2) = 0) then
    Result := (Info1.st_dev = Info2.st_dev) and
      (Info1.st_ino = Info2.st_ino);
{$ENDIF}
{$IFDEF FPC}{$IFDEF WINDOWS}
  if not Result then
    Result := SameWindowsFileIdentity(Path1, Path2);
{$ENDIF}{$ENDIF}
end;

function RenameFile(const FileName1, FileName2: string): Boolean;
begin
  TryDeleteFile(FileName2);
  Result := SysUtils.RenameFile(PChar(FileName1), PChar(FileName2))
end;

function CopyFile(const FileName1, FileName2: string): Boolean;
var
  FHandle1, FHandle2: Integer;
  NRead, NWritten: Word;
  Buffer: PChar;
begin
  Result := False;
  if SameExistingFile(FileName1, FileName2) then
  begin
    Result := True;
    Exit;
  end;
  FHandle1 := FileOpen(FileName1, fmOpenRead + fmShareDenyNone);
  if FHandle1 >= 0 then
  try
    FHandle2 := FileCreate(FileName2);
    try
      GetMem(Buffer, 16384);
      try
        FileSeek(FHandle1, 0, 0);
        repeat
          NRead := FileRead(FHandle1, Buffer^, 16384);
          NWritten := FileWrite(FHandle2, Buffer^, NRead);
        until (NRead = 0) or (NRead <> NWritten);
        if NRead = NWritten then Result := True;
      finally
        FreeMem(Buffer, 16384);
      end;
      FileSetDate(FHandle2, FileGetDate(FHandle1));
    finally
      FileClose(FHandle2);
    end;
  finally
    FileClose(FHandle1);
  end;
end;

function GetTempDir: string;
var
{$IF DEFINED(FPC) AND DEFINED(MSWINDOWS)}
  WidePath: UnicodeString;
  BufferLength, PathLength: Cardinal;
{$ELSE}
  Buffer: array[0..1023] of Char;
{$ENDIF}
begin
{$IFNDEF FPC}
  GetTempPath(SizeOf(Buffer) - 1, Buffer);
  SetString(Result, Buffer, StrLen(Buffer));
  Result := GetLongPath(Result);
{$ELSE}
{$IFDEF MSWINDOWS}
  BufferLength := 260;
  repeat
    SetLength(WidePath, BufferLength);
    PathLength := c_GetTempPathW(BufferLength, PWideChar(WidePath));
    if PathLength = 0 then
      raise EInOutError.Create('Windows could not determine the temporary directory');
    if PathLength < BufferLength then Break;
    if PathLength >= 32767 then
      raise EInOutError.Create('Windows temporary directory path is too long');
    BufferLength := PathLength + 1;
  until False;
  SetLength(WidePath, PathLength);
  Result := UTF8Encode(WidePath);
{$ELSE}
  Result := SysUtils.GetTempDir;
{$ENDIF}
{$ENDIF}
  Result := IncludeTrailingPathDelimiter(Result);
end;

procedure WriteConsoleMessage(const MsgStr: string);
{$IFDEF MSWINDOWS}
var
  ErrorHandle: THandle;
  Line: string;
  Offset: Integer;
  Written: DWORD;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  { The GUI RTL turns StdErr into a modal message box at shutdown, even
    when a CLI caller supplied a pipe. Write to that inherited handle. }
  ErrorHandle := Windows.GetStdHandle(Windows.STD_ERROR_HANDLE);
  if (ErrorHandle = 0) or (ErrorHandle = INVALID_HANDLE_VALUE) then Exit;
  Line := MsgStr + #13#10;
  Offset := 1;
  while Offset <= Length(Line) do
  begin
    if not Windows.WriteFile(ErrorHandle, Line[Offset],
      Length(Line) - Offset + 1, Written, nil) then Break;
    if Written = 0 then Break;
    Inc(Offset, Written);
  end;
{$ELSE}
{$I-}
  WriteLn(StdErr, MsgStr);
  Flush(StdErr);
{$I+}
  if IOResult <> 0 then ;
{$ENDIF}
end;

procedure MessageBoxInfo(MsgStr: string);
begin
  if ConsoleMessages then
  begin
    WriteConsoleMessage(MsgStr);
    Exit;
  end;
{$IFDEF VER140}
  Application.MessageBox(PChar(MsgStr), 'Info', MB_OK);
{$ELSE}
  Application.MessageBox(PChar(MsgStr), 'Info', 0);
{$ENDIF}
end;

procedure MessageBoxError(MsgStr: string);
begin
  if ConsoleMessages then
  begin
    ConsoleErrorReported := True;
    WriteConsoleMessage('TpX: ' + MsgStr);
    Exit;
  end;
{$IFDEF VER140}
  Application.MessageBox(PChar(MsgStr), 'Error', MB_OK);
{$ELSE}
  Application.MessageBox(PChar(MsgStr), 'Error', 0);
{$ENDIF}
end;

function MessageBoxErrorAsk(MsgStr: string): Boolean;
begin
  if ConsoleMessages then
  begin
    ConsoleErrorReported := True;
    WriteConsoleMessage('TpX: ' + MsgStr);
    Exit(False);
  end;
{$IFDEF VER140}
  Result := Application.MessageBox(PChar(MsgStr), 'Error',
    MB_OKCANCEL) = idOK;
{$ELSE}
  Result := Application.MessageBox(PChar(MsgStr), 'Error', 1) = 1;
{$ENDIF}
end;

function GetFontDescent(
  const FaceName: string;
  const Style: TFontStyles;
  const Charset: TFontCharSet): Single;
{$IFDEF VER140}
var
  ExtendedFont: TExtendedFont;
  //LogFont: TLogFontW;
  h_DC: HDC;
  h_FONT: HFONT;
  Text_Metric: tagTEXTMETRIC;
{$ENDIF}
begin
  Result := 0.2;
{$IFDEF VER140}
  ExtendedFont := TExtendedFont.Create;
  ExtendedFont.Italic := Byte(fsItalic in Style);
  if fsBold in Style then ExtendedFont.Weight := FW_BOLD;
  ExtendedFont.Charset := Charset;
  ExtendedFont.FaceName := FaceName;
  ExtendedFont.Height := -1000;
  h_DC := GetWindowDC(0);
  h_FONT := CreateFontIndirectA(ExtendedFont.LogFont);
  SelectObject(h_DC, h_FONT);
  GetTextMetrics(h_DC, Text_Metric);
  DeleteObject(h_FONT);
  ReleaseDC(0, h_DC);
  ExtendedFont.Free;
  Result := Text_Metric.tmDescent / 1000;
{$ENDIF}
end;

function GetFontInternalLeading(
  const FaceName: string;
  const Style: TFontStyles;
  const Charset: TFontCharSet): Single;
{$IFDEF VER140}
var
  ExtendedFont: TExtendedFont;
  //LogFont: TLogFontW;
  h_DC: HDC;
  h_FONT: HFONT;
  Text_Metric: tagTEXTMETRIC;
  TmpH: Integer;
{$ENDIF}
begin
  Result := 0.2;
{$IFDEF VER140}
  ExtendedFont := TExtendedFont.Create;
  ExtendedFont.FaceName := FaceName;
  ExtendedFont.Italic := Byte(fsItalic in Style);
  if fsBold in Style then ExtendedFont.Weight := FW_BOLD;
  ExtendedFont.Charset := Charset;
  TmpH := 1000;
  ExtendedFont.Height := -TmpH;
  h_DC := GetWindowDC(0);
  h_FONT := CreateFontIndirectA(ExtendedFont.LogFont);
  SelectObject(h_DC, h_FONT);
  GetTextMetrics(h_DC, Text_Metric);
  DeleteObject(h_FONT);
  ReleaseDC(0, h_DC);
  ExtendedFont.Free;
  Result := Text_Metric.tmInternalLeading / TmpH;
{$ENDIF}
end;

procedure GetTextDimension(
  const WideText: Widestring;
  const FaceName: string;
  const Style: TFontStyles;
  const Charset: TFontCharSet;
  out Width, Descent: Single);
var
  BMP: Graphics.TBitmap;
  S: Types.TSize;
  TmpH: Integer;
  ExtendedFont: TExtendedFont;
  Text_Metric: tagTEXTMETRIC;
begin
  ExtendedFont := TExtendedFont.Create;
  BMP := Graphics.TBitmap.Create;
  try
    ExtendedFont.Canvas := BMP.Canvas;
    ExtendedFont.FaceName := FaceName;
    ExtendedFont.Italic := Byte(fsItalic in Style);
    if fsBold in Style then ExtendedFont.Weight := FW_BOLD;
    ExtendedFont.Charset := Charset;
    TmpH := 1000;
    ExtendedFont.Height := -TmpH;
    GetTextMetrics(ExtendedFont.Canvas.Handle, Text_Metric);
    Descent := Text_Metric.tmDescent / TmpH;
    //tmAscent tmDescent
{$IFDEF VER140}
    Windows.GetTextExtentPoint32W(ExtendedFont.Canvas.Handle,
      PWideChar(WideText), Length(WideText), S);
{$ELSE}
    BMP.Canvas.Font.Name := FaceName;
    BMP.Canvas.Font.Style := Style;
    BMP.Canvas.Font.Charset := Charset;
    BMP.Canvas.Font.Height := -TmpH;
    S := BMP.Canvas.TextExtent(UTF8Encode(WideText));
    GetTextMetrics(BMP.Canvas.Handle, Text_Metric);
    Descent := Text_Metric.tmDescent / TmpH;
{$ENDIF}
    Width := S.CX / TmpH;
    //if S.CY <> 0 then Width := S.CX / S.CY    else Width := 1;
  finally
    ExtendedFont.Free;
    BMP.Free;
  end;
end;

procedure ShowProgress(Percentage: Double);
begin
  if MainForm <> nil then
  begin
    MainForm.ProgressBar1.Position := Round(Percentage * 100);
    Application.ProcessMessages;
  end;
end;

function MulDiv(Number, Numerator, Denominator: Integer): Integer;
begin
{$IFNDEF FPC}
  Result := Windows.MulDiv(Number, Numerator, Denominator);
{$ELSE}
  Result := LCLType.MulDiv(Number, Numerator, Denominator);
{$ENDIF}
end;

function SmallPointToPoint(const P: TSmallPoint): TPoint;
begin
{$IFDEF VER140}
  Result := Windows.SmallPointToPoint(P);
{$ELSE}
  Result.X := P.X;
  Result.Y := P.Y;
{$ENDIF}
end;

const
  MaxPixelCount = 32768;

type
  pRGBArray = ^TRGBArray;
  TRGBArray = array[0..MaxPixelCount - 1] of TRGBTriple;

{$IFNDEF FPC}

procedure FastAntiAliasPicture(big_bmp, out_bmp: TBitmap);
var
  X, Y, CX, CY: Integer;
  totr, totg, totb: Integer;
  Row1, Row2, Row3, DestRow: pRGBArray;
  I: Integer;
begin
  // For each row
  for Y := 0 to out_bmp.Height - 1 do
  begin
    // We compute samples of 3 x 3 pixels
    CY := Y * 3;
    // Get pointers to actual, previous and next rows in supersampled bitmap
    Row1 := big_bmp.ScanLine[CY];
    Row2 := big_bmp.ScanLine[CY + 1];
    Row3 := big_bmp.ScanLine[CY + 2];

    // Get a pointer to destination row in output bitmap
    DestRow := out_bmp.ScanLine[Y];

    // For each column...
    for X := 0 to out_bmp.Width - 1 do
    begin
      // We compute samples of 3 x 3 pixels
      CX := 3 * X;

      // Initialize result color
      totr := 0;
      totg := 0;
      totb := 0;

      // For each pixel in sample
      for I := 0 to 2 do
      begin
        // New red value
        totr := totr + Row1[CX + I].rgbtRed
          + Row2[CX + I].rgbtRed
          + Row3[CX + I].rgbtRed;
        // New green value
        totg := totg + Row1[CX + I].rgbtGreen
          + Row2[CX + I].rgbtGreen
          + Row3[CX + I].rgbtGreen;
        // New blue value
        totb := totb + Row1[CX + I].rgbtBlue
          + Row2[CX + I].rgbtBlue
          + Row3[CX + I].rgbtBlue;
      end;

      // Set output pixel colors
      DestRow[X].rgbtRed := totr div 9;
      DestRow[X].rgbtGreen := totg div 9;
      DestRow[X].rgbtBlue := totb div 9;
    end;
  end;
end;
{$ENDIF}

procedure StartTimer;
begin
  StartTime := GetTickCount;
end;

function GetTimer: Extended;
begin
  GetTimer := (GetTickCount - StartTime) / 1000;
  //(Time - StartTime) * 24 * 60 * 60;
end;

end.


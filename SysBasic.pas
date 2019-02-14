unit SysBasic;

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

{$IFDEF VER140}
{$ENDIF}

interface

uses Types,
{$IFDEF VER140}
  Windows, ShellAPI, WinBasic,
{$ELSE}
  LCLIntf, LCLType, LazBasic,
{$ENDIF}
  SysUtils, Forms, Graphics;

function FileExec(const aCmdLine, InFile, OutFile, Directory:
  string; aHide, aWait: Boolean): Boolean;
procedure OpenOrExec(const ViewerPath, FileName: string);
function TryDeleteFile(const FileName: string): Boolean;
function RenameFile(const FileName1, FileName2: string): Boolean;
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
procedure GetTextDimension(
  const WideText: Widestring;
  const FaceName: string;
  const Style: TFontStyles;
  const Charset: TFontCharSet;
  out Width, Descent: Single);
procedure ShowProgress(Percentage: Double);
function MulDiv(Number, Numerator, Denominator: Integer): Integer;
function SmallPointToPoint(const P: TSmallPoint): TPoint;
{$IFDEF VER140}
procedure FastAntiAliasPicture(big_bmp, out_bmp: TBitmap);
{$ENDIF}
procedure StartTimer;
function GetTimer: Extended;

var
  StartTime: Cardinal;

implementation

uses
//WinBasic,
  MainUnit;

function FileExec(const aCmdLine, InFile, OutFile, Directory:
  string; aHide, aWait: Boolean): Boolean;
{$IFDEF VER140}
var
  StartupInfo: TSTARTUPINFO;
  ProcessInfo: TPROCESSINFORMATION;
  aInput, aOutput: Integer;
  PDirectory: PChar;
  exitCode: DWORD;
  ExitCodeProcess: Longword;
{$ELSE}
var
  OldDir: string;
{$ENDIF}
begin
{$IFDEF VER140}
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
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
  finally
    if OutFile <> '' then FileClose(aOutput);
    if InFile <> '' then FileClose(aInput);
  end;
{  if Result then
    Application.MessageBox('App successful',
      'Error', MB_OK);}
{$ELSE}
  OldDir := GetCurrentDir;
  SetCurrentDir(Directory);
  Result := ExecuteProcess(aCmdLine, '') = 0;
  SetCurrentDir(OldDir);
{$ENDIF}
end;

procedure OpenOrExec(const ViewerPath, FileName: string);
begin
  if ViewerPath = '' then
{$IFDEF VER140}
    ShellExecute(Application.Handle,
      PChar('open'), PChar(FileName),
      nil {PChar(Parameters)}, nil {PChar(Directory)}, SW_SHOW)
{$ELSE}
    FileExec('"' + FileName + '"', '', '', '',
      False, False)
{$ENDIF}
  else
    FileExec(Format('%s "%s"',
      [ViewerPath, FileName]), '', '',
      {IncludeTrailingPathDelimiter(ExtractFilePath(FileName))}'',
      False, False);
end;

function TryDeleteFile(const FileName: string): Boolean;
begin
  if FileExists(FileName)
    then
    Result := SysUtils.DeleteFile(PChar(FileName))
  else
    Result := True;
end;

function RenameFile(const FileName1, FileName2: string): Boolean;
begin
  TryDeleteFile(FileName2);
  Result := SysUtils.RenameFile(PChar(FileName1), PChar(FileName2))
end;

function GetTempDir: string;
var
  Buffer: array[0..1023] of Char;
begin
{$IFDEF VER140}
  GetTempPath(SizeOf(Buffer) - 1, Buffer);
  SetString(Result, Buffer, StrLen(Buffer));
{$ELSE}
  Result := SysUtils.GetTempDir;
{$ENDIF}
  Result := IncludeTrailingPathDelimiter(Result);
end;

procedure MessageBoxInfo(MsgStr: string);
begin
{$IFDEF VER140}
  Application.MessageBox(PChar(MsgStr), 'Info', MB_OK);
{$ELSE}
  Application.MessageBox(PChar(MsgStr), 'Info', 0);
{$ENDIF}
end;

procedure MessageBoxError(MsgStr: string);
begin
{$IFDEF VER140}
  Application.MessageBox(PChar(MsgStr), 'Error', MB_OK);
{$ELSE}
  Application.MessageBox(PChar(MsgStr), 'Error', 0);
{$ENDIF}
end;

function MessageBoxErrorAsk(MsgStr: string): Boolean;
begin
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
  BMP: TBitmap;
  S: TSize;
  TmpH: Integer;
  ExtendedFont: TExtendedFont;
  Text_Metric: tagTEXTMETRIC;
begin
  ExtendedFont := TExtendedFont.Create;
  BMP := TBitmap.Create;
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
    S := ExtendedFont.Canvas.TextExtent(WideText);
{$ENDIF}
    Width := S.CX / TmpH;
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
{$IFDEF VER140}
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

{$IFDEF VER140}

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


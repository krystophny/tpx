unit Bitmaps;

interface

uses
  Graphics, Geometry, StrUtils, Classes;

type

  TBitmapEntryKind = (bek_None, bek_BMP, bek_PNG, bek_JPEG);

  TBitmapEntry = class(TObject)
  protected
    fImageLink: string;
    fBitmap: Graphics.TBitmap;
    fKind: TBitmapEntryKind;
    fParentFileName: string;
    fImageFiles: TStringList;
    function MakeImageLocal(
      const Link, ParentFileName: string): string;
    procedure SetImageKind(const Link: string);
    procedure SetImageLink(const Link: string);
  public
    constructor Create(const AImageLink: string;
      const ParentFileName: string);
    destructor Destroy; override;
    procedure RefreshParentFileName(
      const NewFileName: string);
    function GetFullLink: string;
    function GetOnlyName: string;
    function ImageFileExists(const FileName: string): Boolean;
    function CopyImage(
      const FileName1, FileName2: string): Boolean;
    function RequirePNGJPEG(const OutDir: string): Boolean;
    function RequireEPS(const OutDir: string): Boolean;
    function GetIncludeGraphics(const W, H: TRealType;
      const KeepAspectRatio: Boolean;
      const IncludePath: string): string;
    property Kind: TBitmapEntryKind read fKind;
    property ImageLink: string read fImageLink;
    property Bitmap: Graphics.TBitmap read fBitmap;
  end;

function BitmapToEps(const BitmapFileName: string;
  const OutFileName: string): Boolean;
var
{$IFNDEF LINUX}Bitmap2EpsPath: string = 'sam2p.exe';
{$ELSE}Bitmap2EpsPath: string = 'sam2p'; {$ENDIF}

{$I tpx.inc}

implementation
uses
  SysBasic,
{$IFNDEF FPC}
   Imaging.pngimage, Imaging.jpeg,
{$ENDIF}
  SysUtils;

function FixSam2pEps(const EpsFileName: string): Boolean;
// Remove colon from "%%BeginData:" in EPS file generated by sam2p v.0.43 for Windows
var
  FIn, FOut: TextFile;
  TmpEps, St: string;
  I: Integer;
begin
  Result := False;
  if not FileExists(EpsFileName) then Exit;
  AssignFile(FIn, EpsFileName);
  Reset(FIn);
  TmpEps := GetTempDir + '(bitmap2eps)eps.eps';
  AssignFile(FOut, TmpEps);
  Rewrite(FOut);
  I := 0;
  try
    while not Eof(FIn) do
    begin
      ReadLn(FIn, St);
      Inc(I);
      if I <= 50 then
        if Pos('%%BeginData:', St) = 1 then
        begin
          System.Delete(St, 12, 1);
          Result := True;
        end;
      WriteLn(FOut, St);
    end;
  finally
    CloseFile(FIn);
    CloseFile(FOut);
  end;
  if Result then
    Result := SysBasic.RenameFile(TmpEps, EpsFileName);
end;

function BitmapToEps(const BitmapFileName: string;
  const OutFileName: string): Boolean;
var
  TmpPath: string;
begin
  Result := False;
  if not CheckFilePath(Bitmap2EpsPath, 'Bitmap2Eps') then Exit;
  TryDeleteFile(OutFileName);
  TmpPath := PrepareFilePath(Bitmap2EpsPath);
  if Pos('bmeps', LowerCase(TmpPath)) > 0 then
      // create colored image when using bmeps
    TmpPath := TmpPath + ' -c';
  try
    Result := FileExec(Format('%s "%s" "%s"',
      [TmpPath, BitmapFileName, OutFileName]), '', '',
      '', True, True);
    if Result then
      Result := FileExists(OutFileName);
  finally
  end;
  if not Result then
  begin
    MessageBoxError('Conversion of bitmap to EPS failed: ' +
      BitmapFileName);
    Exit;
  end;
  if (Pos('sam2p', LowerCase(Bitmap2EpsPath)) > 0)
    and (LowerCase(ExtractFileExt(OutFileName)) = '.eps')
    then Result := FixSam2pEps(OutFileName);
end;

function StoreBitmapToPNG(BMP: Graphics.TBitmap;
  const FileName: string): Boolean;
var
  {$IFDEF FPC}
  PNG: TPortableNetworkGraphic;
  {$ELSE}
  PNG: TPNGObject;
  {$ENDIF}
begin
  Result := False;
  {$IFDEF FPC}
  PNG := TPortableNetworkGraphic.Create;
  {$ELSE}
  PNG := TPNGObject.Create;
  {$ENDIF}
  try
    PNG.Assign(BMP);
    PNG.SaveToFile(FileName);
    Result := True;
  finally
    PNG.Free;
  end;
end;

function LoadPng(BMP: Graphics.TBitmap; const FileName: string):
  Boolean;
var
  {$IFDEF FPC}
  PNG: TPortableNetworkGraphic;
  {$ELSE}
  PNG: TPNGObject;
  {$ENDIF}
begin
  Result := False;
  if not FileExists(FileName) then Exit;
  {$IFDEF FPC}
  PNG := TPortableNetworkGraphic.Create;
  {$ELSE}
  PNG := TPNGObject.Create;
  {$ENDIF}
  try
    PNG.LoadFromFile(FileName);
    BMP.Assign(PNG);
    Result := True;
  finally
    PNG.Free;
  end;
end;

function LoadJpeg(BMP: Graphics.TBitmap; const FileName: string):
  Boolean;
var
  JPG: TJPEGImage;
begin
  Result := False;
  if not FileExists(FileName) then Exit;
  JPG := TJPEGImage.Create;
  try
    JPG.LoadFromFile(FileName);
    BMP.Assign(JPG);
    Result := True;
  finally
    JPG.Free;
  end;
end;

{ --================ TBitmapEntry ==================-- }

constructor TBitmapEntry.Create(const AImageLink: string;
  const ParentFileName: string);
begin
  inherited Create;
  fParentFileName := ParentFileName;
  fBitmap := Graphics.TBitmap.Create;
  fImageFiles := TStringList.Create;
  fImageFiles.Sorted := True;
  fImageFiles.Duplicates := dupIgnore;
  SetImageLink(AImageLink);
end;

destructor TBitmapEntry.Destroy;
begin
  fBitmap.Free;
  fImageFiles.Free;
  inherited Destroy;
end;

function TBitmapEntry.MakeImageLocal(
  const Link, ParentFileName: string): string;
var
  Dir: string;
begin
  Result := Link;
  Dir := ExtractFilePath(ParentFileName);
  if ParentFileName = '' then
  else if Pos(Dir, Result) = 1 then
    System.Delete(Result, 1, Length(Dir))
  else
  begin
    Dir := Dir + 'bitmaps\';
    if not DirectoryExists(Dir) then CreateDir(Dir);
    CopyImage(Link, Dir + ExtractFileName(Result));
    Result := 'bitmaps\' + ExtractFileName(Result);
  end;
end;

procedure TBitmapEntry.SetImageKind(const Link: string);
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(fImageLink));
  if Ext = '.bmp' then fKind := bek_BMP
  else if Ext = '.png' then fKind := bek_PNG
  else if (Ext = '.jpg') or (Ext = '.jpeg') then fKind := bek_JPEG
  else fKind := bek_None;
  if Link <> '' then
  begin
    case fKind of
      bek_BMP:
        begin
          if not FileExists(Link) then fKind := bek_None else
          try
            fBitmap.LoadFromFile(Link);
          except
            fKind := bek_None;
          end;
        end;
      bek_PNG:
        if not LoadPng(fBitmap, Link) then fKind := bek_None;
      bek_JPEG:
        if not LoadJpeg(fBitmap, Link) then fKind := bek_None;
    end;
    if fKind <> bek_None then fImageFiles.Add(Link);
  end;
end;

procedure TBitmapEntry.SetImageLink(const Link: string);
begin
  if Link = '' then
  begin
    fImageLink := '';
    Exit;
  end;
  fImageLink := MakeImageLocal(Link, fParentFileName);
  SetImageKind(Link);
end;

function TBitmapEntry.GetFullLink: string;
begin
  if ExtractFileDrive(fImageLink) = '' then
    Result := ExtractFilePath(fParentFileName) + fImageLink
  else Result := fImageLink;
end;

function TBitmapEntry.GetOnlyName: string;
begin
  Result := ChangeFileExt(ExtractFileName(fImageLink), '');
end;

function TBitmapEntry.ImageFileExists(
  const FileName: string): Boolean;
begin
  Result := (fImageFiles.IndexOf(FileName) >= 0) and
    FileExists(FileName);
end;

procedure TBitmapEntry.RefreshParentFileName(
  const NewFileName: string);
var
  NewDir, NewLink: string;
begin
  if ExtractFileDrive(fImageLink) <> '' then
  begin
    fImageLink := MakeImageLocal(fImageLink, NewFileName);
    SetImageKind('');
    fParentFileName := NewFileName;
    Exit;
  end;
  NewLink := ExtractFilePath(NewFileName) + fImageLink;
  NewDir := ExtractFilePath(NewLink);
  if not DirectoryExists(NewDir)
    then ForceDirectories(NewDir);
  if not ImageFileExists(NewLink) then
    CopyImage(GetFullLink, NewLink);
  SetImageKind('');
  fParentFileName := NewFileName;
end;

function TBitmapEntry.CopyImage(
  const FileName1, FileName2: string): Boolean;
begin
  Result := CopyFile(FileName1, FileName2);
  if Result then fImageFiles.Add(FileName2);
end;

function TBitmapEntry.RequirePNGJPEG(
  const OutDir: string): Boolean;
var
  OutFileName: string;
begin
  Result := True;
  if fKind = bek_None then Exit;
  if fKind = bek_BMP then
    OutFileName := OutDir + GetOnlyName + '.png'
  else
    OutFileName := OutDir + ExtractFileName(fImageLink);
  if ImageFileExists(OutFileName) then Exit;
  if fKind = bek_BMP then
  begin
    Result := StoreBitmapToPNG(fBitmap, OutFileName);
    if Result then fImageFiles.Add(OutFileName);
    Exit;
  end;
  Result := CopyImage(GetFullLink, OutFileName);
end;

function TBitmapEntry.RequireEPS(
  const OutDir: string): Boolean;
var
  BitmapFileName, OutFileName: string;
begin
  Result := True;
  if fKind = bek_None then Exit;
  OutFileName := OutDir + GetOnlyName + '.eps';
  if (Pos(GetTempDir, OutDir) <> 1)
    and ImageFileExists(OutFileName) then Exit;
  if (fKind = bek_JPEG) or
    ((fKind = bek_BMP)
    and (Pos('sam2p', LowerCase(Bitmap2EpsPath)) > 0))
    or
    ((fKind = bek_PNG)
    and (Pos('bmeps', LowerCase(Bitmap2EpsPath)) > 0))
    then BitmapFileName := GetFullLink
  else if fKind = bek_PNG then
  begin
    BitmapFileName := GetTempDir + '(Bitmap2Eps)bmp.bmp';
    try
      fBitmap.SaveToFile(BitmapFileName);
    except
      Result := False;
      Exit;
    end;
  end
  else if fKind = bek_BMP then
  begin
    BitmapFileName := GetTempDir + '(Bitmap2Eps)png.png';
    try
      Result := StoreBitmapToPNG(fBitmap, BitmapFileName);
    except
      Result := False;
      Exit;
    end;
  end;
  Result := BitmapToEps(BitmapFileName, OutFileName);
  if Result then fImageFiles.Add(OutFileName);
  if Pos(GetTempDir + '(Bitmap2Eps)', BitmapFileName) = 1
    then TryDeleteFile(BitmapFileName);
end;

function TBitmapEntry.GetIncludeGraphics(const W, H: TRealType;
  const KeepAspectRatio: Boolean;
  const IncludePath: string): string;
begin
  Result := AnsiReplaceStr(ChangeFileExt(fImageLink, ''),
    '\', '/');
  if ExtractFileDrive(fImageLink) = '' then
    Result := IncludePath + Result;
  if KeepAspectRatio then
    Result := Format(
      '\includegraphics[width=%.2fmm]{%s}', [W, Result])
  else
    Result := Format(
      '\includegraphics[width=%.2fmm, height=%.2fmm]{%s}',
      [W, H, Result]);
end;

end.


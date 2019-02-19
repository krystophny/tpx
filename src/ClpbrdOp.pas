unit ClpbrdOp;

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface

uses Drawings, Graphics, Clipbrd,
{$IFDEF VER140}
  Windows,
{$ELSE}
  LCLIntf,
{$ENDIF}
  Classes;

var
  TpXClipboardFormat: Cardinal;

const
  TpXClipboardFormatString = 'TpX Clipboard Format';

type

  TClipboardFormat = Word;

{$IFDEF VER140}
procedure PutMetafileToClipboard(const MetaFile: TMetaFile);
procedure Import_MetafileFromClipboard(const Drawing: TDrawing2D);
procedure PasteMetafileFromClipboard(const Drawing: TDrawing2D);
procedure CopyToClipboardAsMetaFile(const Drawing: TDrawing2D);
{$ENDIF}
procedure PutStreamToClipboard(Format: TClipboardFormat;
  DoClear: Boolean; Stream: TStream; Size: Longint);
//TSY:
procedure GetStreamFromClipboard(Format: TClipboardFormat;
  Stream: TStream);
procedure GetStreamFromClipboardAsText(Stream: TStream);
function ClipboardHasFormat(Format: TClipboardFormat): Boolean;
function ClipboardHasMetafile: Boolean;
function ClipboardHasTpX: Boolean;

implementation

uses Input, MprtEMF, Output;

{$IFDEF VER140}

procedure PutMetafileToClipboard(const MetaFile: TMetaFile);
begin
  Clipboard.Assign(MetaFile);
end;

procedure Import_MetafileFromClipboard(const Drawing: TDrawing2D);
var
  EMF_Import: T_EMF_Import;
  MF: TMetaFile;
begin
  if not Clipboard.HasFormat(CF_METAFILEPICT) then Exit;
  MF := TMetaFile.Create;
  EMF_Import := T_EMF_Import.Create(Drawing);
  try
    MF.Assign(Clipboard);
    EMF_Import.LoadFromMF(MF);
    EMF_Import.ParseEmf;
//    TpX_Loader.XMLDoc.Save('--.tpx');
  finally
    MF.Free;
    EMF_Import.Free;
  end;
end;

procedure PasteMetafileFromClipboard(const Drawing: TDrawing2D);
var
  TmpDrawing: TDrawing2D;
  Stream: TMemoryStream;
begin
  TmpDrawing := TDrawing2D.Create(nil);
  Stream := TMemoryStream.Create;
  try
    Import_MetafileFromClipboard(TmpDrawing);
    TmpDrawing.SaveObjectsToStream(Stream);
    Stream.Position := 0;
    Drawing.LoadObjectsFromStream(Stream);
  finally
    TmpDrawing.Free;
    Stream.Free;
  end;
end;

procedure CopyToClipboardAsMetaFile(const Drawing: TDrawing2D);
var
  MetaFile: TMetaFile;
begin
  MetaFile := DrawingAsMetafile(Drawing);
  try
    PutMetafileToClipboard(MetaFile);
  finally
    MetaFile.Free;
  end;
end;

{$ENDIF}

{$IFDEF VER140}

procedure PutStreamToClipboard(Format: TClipboardFormat;
  DoClear: Boolean; Stream: TStream; Size: Longint);
var
  Len: Longint;
  Buffer: Pointer;
  Data: THandle;
begin
  Clipboard.Open;
  if DoClear then Clipboard.Clear;
  try
    Len := Stream.Size - Stream.Position;
    if Len > Size then Len := Size;
    Data := GlobalAlloc(gmem_Moveable or GMEM_DDESHARE
      {HeapAllocFlags}, Len);
    try
      if Data <> 0 then
      begin
        Buffer := GlobalLock(Data);
        try
          Stream.Read(Buffer^, Len);
          SetClipboardData(Format, Data);
        finally
          GlobalUnlock(Data);
        end;
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    Clipboard.Close;
  end;
end;

{$ELSE}

procedure PutStreamToClipboard(Format: TClipboardFormat;
  DoClear: Boolean; Stream: TStream; Size: Longint);
begin
  Clipboard.SetFormat(Format, Stream);
end;
{$ENDIF}


{$IFDEF VER140}

procedure GetStreamFromClipboard(Format: TClipboardFormat;
  Stream: TStream);
var
  Buffer: Pointer;
  Data: THandle;
begin
  Clipboard.Open;
  try
    Data := GetClipboardData(Format);
    if Data = 0 then Exit;
    Buffer := GlobalLock(Data);
    try
      Stream.Write(Buffer^, GlobalSize(Data));
    finally
      GlobalUnlock(Data);
    end;
  finally
    Clipboard.Close;
  end;
end;

{$ELSE}

procedure GetStreamFromClipboard(Format: TClipboardFormat;
  Stream: TStream);
begin
  Clipboard.GetFormat(Format, Stream);
end;
{$ENDIF}

procedure GetStreamFromClipboardAsText(Stream: TStream);
var
  Len: Integer;
  Str: string;
begin
  //GetStreamFromClipboard(CF_TEXT, Stream);
  Clipboard.Open;
  try
    if not Clipboard.HasFormat(CF_TEXT) then Exit;
    Str := Clipboard.AsText;
    Len := Length(Str);
    if Len > 0 then Stream.Write(Str[1], Len);
  finally
    Clipboard.Close;
  end;
end;


function ClipboardHasFormat(Format: TClipboardFormat): Boolean;
begin
  Result := Clipboard.HasFormat(Format);
end;

function ClipboardHasMetafile: Boolean;
begin
  Result := ClipboardHasFormat(CF_METAFILEPICT {CF_ENHMETAFILE});
end;

function ClipboardHasTpX: Boolean;
begin
  Result := ClipboardHasFormat(TpXClipboardFormat);
end;


initialization
  TpXClipboardFormat :=
    RegisterClipboardFormat(TpXClipboardFormatString);
end.

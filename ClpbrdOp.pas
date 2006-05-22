unit ClpbrdOp;

interface

uses CADSys4, Graphics, Clipbrd, Windows, Classes;

var
  TpXClipboardFormat: Cardinal;

const
  TpXClipboardFormatString = 'TpX Clipboard Format';

type

  TClipboardFormat = Word;

procedure PutMetafileToClipboard(const MetaFile: TMetaFile);
procedure Import_MetafileFromClipboard(const Drawing: TDrawing2D);
procedure PasteMetafileFromClipboard(const Drawing: TDrawing2D);
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

uses Input, EMF;

procedure PutMetafileToClipboard(const MetaFile: TMetaFile);
begin
  Clipboard.Assign(MetaFile);
end;

procedure Import_MetafileFromClipboard(const Drawing: TDrawing2D);
var
  EMF_Loader: T_EMF_Loader;
  MF: TMetaFile;
  TpX_Loader: T_TpX_Loader;
begin
  if not Clipboard.HasFormat(CF_METAFILEPICT) then Exit;
  MF := TMetaFile.Create;
  EMF_Loader := T_EMF_Loader.Create;
  TpX_Loader := T_TpX_Loader.Create(Drawing);
  try
    MF.Assign(Clipboard);
    EMF_Loader.LoadFromMF(MF);
    EMF_Loader.FillXML(TpX_Loader.XMLDoc);
//    TpX_Loader.XMLDoc.Save('--.TpX');
    TpX_Loader.ReadAll;
  finally
    MF.Free;
    EMF_Loader.Free;
    TpX_Loader.Free;
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
    Drawing.LoadObjectsFromStream(Stream, Drawing.Version);
  finally
    TmpDrawing.Free;
    Stream.Free;
  end;
end;

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


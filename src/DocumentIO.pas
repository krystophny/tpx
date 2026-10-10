unit DocumentIO;

{$IFDEF FPC}{$MODE OBJFPC}{$H+}{$ENDIF}

interface

uses Classes, SysUtils, Drawings, DocumentFormats;

type
  TDocumentLoader = function(Candidate: TDrawing2D; const FileName: string;
    const SourceBytes: RawByteString; Diagnostics: TStrings;
    out CodecContext: TObject): Boolean;
  TDocumentSaver = function(Drawing: TDrawing2D; const FileName: string;
    Destination: TStream; CodecContext: TObject): Boolean;

procedure RegisterDocumentCodec(const Format: TDocumentFormat;
  Loader: TDocumentLoader; Saver: TDocumentSaver);
function IsDocumentCodecRegistered(const FormatId: string): Boolean;
function LoadDocumentCandidate(const FormatId, FileName: string;
  const SourceBytes: RawByteString; out CanSaveBack: Boolean;
  out CodecContext: TObject; Diagnostics: TStrings = nil): TDrawing2D;
procedure CommitDocumentCandidate(Target, Candidate: TDrawing2D);
function SerializeDocumentToBytes(const FormatId: string;
  Drawing: TDrawing2D; const FileName: string;
  CodecContext: TObject = nil): RawByteString;

implementation

uses Contnrs, Input, Output;

type
  TDocumentCodec = class
    Format: TDocumentFormat;
    Loader: TDocumentLoader;
    Saver: TDocumentSaver;
  end;

var
  Codecs: TObjectList;

function FindCodec(const FormatId: string): TDocumentCodec;
var I: Integer; C: TDocumentCodec;
begin
  Result := nil;
  for I := 0 to Codecs.Count - 1 do
  begin
    C := Codecs[I] as TDocumentCodec;
    if SameText(C.Format.Id, FormatId) then
    begin
      Result := C;
      Exit;
    end;
  end;
end;

procedure RegisterDocumentCodec(const Format: TDocumentFormat;
  Loader: TDocumentLoader; Saver: TDocumentSaver);
var C: TDocumentCodec;
begin
  if Format.Id = '' then
    raise Exception.Create('A document codec requires a stable format ID');
  C := FindCodec(Format.Id);
  if C = nil then
  begin
    C := TDocumentCodec.Create;
    Codecs.Add(C);
  end;
  C.Format := Format;
  C.Loader := Loader;
  C.Saver := Saver;
  RegisterDocumentFormat(Format);
end;

function IsDocumentCodecRegistered(const FormatId: string): Boolean;
begin
  Result := FindCodec(FormatId) <> nil;
end;

function LoadDocumentCandidate(const FormatId, FileName: string;
  const SourceBytes: RawByteString; out CanSaveBack: Boolean;
  out CodecContext: TObject; Diagnostics: TStrings): TDrawing2D;
var C: TDocumentCodec;
begin
  C := FindCodec(FormatId);
  if (C = nil) or not Assigned(C.Loader) then
    raise EReadError.CreateFmt('No loader is registered for %s', [FormatId]);
  CodecContext := nil;
  Result := TDrawing2D.CreateDetached;
  try
    Result.FileName := ExpandFileName(FileName);
    CanSaveBack := C.Loader(Result, Result.FileName, SourceBytes, Diagnostics,
      CodecContext)
      and Assigned(C.Saver) and C.Format.CanSaveBack;
  except
    FreeAndNil(CodecContext);
    Result.Free;
    Result := nil;
    raise;
  end;
end;

procedure CommitDocumentCandidate(Target, Candidate: TDrawing2D);
begin
  if Target = nil then
    raise Exception.Create('A live drawing is required');
  Target.ReplaceContentFrom(Candidate);
end;

function SerializeDocumentToBytes(const FormatId: string;
  Drawing: TDrawing2D; const FileName: string;
  CodecContext: TObject): RawByteString;
var C: TDocumentCodec; Stream: TMemoryStream;
begin
  C := FindCodec(FormatId);
  if (C = nil) or not Assigned(C.Saver) then
    raise EWriteError.CreateFmt('No writer is registered for %s', [FormatId]);
  if Drawing = nil then
    raise EWriteError.Create('A drawing is required for serialization');
  Stream := TMemoryStream.Create;
  try
    if not C.Saver(Drawing, FileName, Stream, CodecContext) then
      raise EWriteError.CreateFmt('The %s writer did not produce a document',
        [FormatId]);
    if Stream.Size > MaxDocumentBytes then
      raise EWriteError.CreateFmt('Document exceeds the %d byte limit',
        [MaxDocumentBytes]);
    SetLength(Result, Stream.Size);
    if Stream.Size > 0 then
    begin
      Stream.Position := 0;
      Stream.ReadBuffer(Result[1], Stream.Size);
    end;
  finally
    Stream.Free;
  end;
end;

function LoadTpX(Candidate: TDrawing2D; const FileName: string;
  const SourceBytes: RawByteString; Diagnostics: TStrings;
  out CodecContext: TObject): Boolean;
var Loader: T_TpX_Loader;
begin
  CodecContext := nil;
  Loader := T_TpX_Loader.Create(Candidate);
  try
    Loader.LoadFromBytes(SourceBytes);
  finally
    Loader.Free;
  end;
  Result := True;
end;

function LoadMetafile(Candidate: TDrawing2D; const FileName: string;
  const SourceBytes: RawByteString; Diagnostics: TStrings;
  out CodecContext: TObject): Boolean;
var Stream: TMemoryStream; IsOld: Boolean;
begin
  CodecContext := nil;
  Stream := TMemoryStream.Create;
  try
    if Length(SourceBytes) > 0 then
      Stream.WriteBuffer(SourceBytes[1], Length(SourceBytes));
    Stream.Position := 0;
    IsOld := SameText(ExtractFileExt(FileName), '.wmf');
    Import_MetafileFromStream(Candidate, Stream, IsOld);
  finally
    Stream.Free;
  end;
  Result := False;
end;

function SaveTpX(Drawing: TDrawing2D; const FileName: string;
  Destination: TStream; CodecContext: TObject): Boolean;
begin
  Result := StoreToStream_TpX(Drawing, Destination, FileName, False);
end;

procedure RegisterBuiltInCodecs;
var F: TDocumentFormat;
begin
  FindDocumentFormatById('tpx', F);
  RegisterDocumentCodec(F, @LoadTpX, @SaveTpX);
  FindDocumentFormatById('emf', F);
  RegisterDocumentCodec(F, @LoadMetafile, nil);
  FindDocumentFormatById('wmf', F);
  RegisterDocumentCodec(F, @LoadMetafile, nil);
end;

initialization
  Codecs := TObjectList.Create(True);
  RegisterBuiltInCodecs;

finalization
  Codecs.Free;

end.

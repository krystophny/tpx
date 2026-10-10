unit DocumentIO;

{$IFDEF FPC}{$MODE OBJFPC}{$H+}{$ENDIF}

interface

uses Classes, SysUtils, Drawings, DocumentFormats;

type
  TDocumentLoader = function(Candidate: TDrawing2D;
    const FileName: TDocumentPath;
    const SourceBytes: RawByteString; Diagnostics: TStrings;
    out CodecContext: TObject): Boolean;
  TDocumentSaver = function(Drawing: TDrawing2D;
    const FileName: TDocumentPath;
    Destination: TStream; CodecContext: TObject): Boolean;
  TDocumentContextRebinder = procedure(LiveDrawing,
    ParsedDrawing: TDrawing2D; CodecContext: TObject);
  TDocumentSaveValidationPath = (dsvpFinalDestination, dsvpPrivateStage);

  { Prepares source bytes and linked assets without changing the destination.
    Publish uses the exact accepted disk revision; Accept transfers a freshly
    parsed codec context only after atomic publication succeeds. }
  TDocumentSaveTransaction = class
  private
    FFormatId: string;
    FPath, FStageDirectory, FStagingFileName: TDocumentPath;
    FExpectedRevision: TDiskRevision;
    FDrawing: TDrawing2D;
    FBytes: RawByteString;
    FCanSaveBack, FPublished, FCommitted: Boolean;
    FNewRevision: TDiskRevision;
    FBackupFileName: TDocumentPath;
    FCodecContext: TObject;
    FStagedAssets, FFinalAssets, FDiagnostics: TStringList;
  public
    constructor Create(const FormatId: string; Drawing: TDrawing2D;
      const FileName: TDocumentPath; const ExpectedRevision: TDiskRevision;
      CodecContext: TObject; const AllowExternalTools: Boolean);
    destructor Destroy; override;
    procedure Publish(out NewRevision: TDiskRevision;
      out BackupFileName: TDocumentPath);
    procedure AppendBrowserDownloadAssets(Paths: TStrings);
    procedure Accept(Session: TDocumentSession);
    procedure Commit(Session: TDocumentSession;
      out NewRevision: TDiskRevision; out BackupFileName: TDocumentPath);
    property SourceBytes: RawByteString read FBytes;
    property Diagnostics: TStringList read FDiagnostics;
    property CanSaveBack: Boolean read FCanSaveBack;
  end;

procedure RegisterDocumentCodec(const Format: TDocumentFormat;
  Loader: TDocumentLoader; Saver: TDocumentSaver;
  ContextRebinder: TDocumentContextRebinder = nil;
  SaveValidationPath: TDocumentSaveValidationPath = dsvpFinalDestination);
function IsDocumentCodecRegistered(const FormatId: string): Boolean;
function PstoeditEmfImportAvailable: Boolean;
function DocumentCodecUsesStagedValidationPath(
  const FormatId: string): Boolean;
procedure RebindDocumentCodecContext(const FormatId: string;
  LiveDrawing, ParsedDrawing: TDrawing2D; CodecContext: TObject);
function LoadDocumentCandidate(const FormatId: string;
  const FileName: TDocumentPath;
  const SourceBytes: RawByteString; out CanSaveBack: Boolean;
  out CodecContext: TObject; Diagnostics: TStrings = nil): TDrawing2D;
procedure CommitDocumentCandidate(Target, Candidate: TDrawing2D);
function SerializeDocumentToBytes(const FormatId: string;
  Drawing: TDrawing2D; const FileName: TDocumentPath;
  CodecContext: TObject = nil): RawByteString;
function SerializeDocumentForSave(const FormatId: string;
  Drawing: TDrawing2D; const FileName, StagingFileName: TDocumentPath;
  Destination: TStream; CodecContext: TObject; StagedAssets,
  FinalAssets: TStrings; const AllowExternalTools: Boolean = False): Boolean;
function SerializeTpXRecoveryBytes(Drawing: TDrawing2D;
  const FileName: TDocumentPath): RawByteString;

implementation

uses Contnrs, Input, Output, SysBasic, Bitmaps;

function IsAbsoluteDocumentLink(const Link: TDocumentPath): Boolean;
begin
{$IFDEF WINDOWS}
  Result := (Length(Link) > 0) and (Link[1] in ['/', '\']);
  if Length(Link) > 1 then Result := Result or (Link[2] = ':');
{$ELSE}
  Result := (Length(Link) > 0) and (Link[1] = '/');
{$ENDIF}
end;

type
  TDocumentCodec = class
    Format: TDocumentFormat;
    Loader: TDocumentLoader;
    Saver: TDocumentSaver;
    ContextRebinder: TDocumentContextRebinder;
    SaveValidationPath: TDocumentSaveValidationPath;
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

procedure StageRelativeBitmapAssets(Drawing: TDrawing2D;
  const Destination, StageDirectory, StagingFileName: TDocumentPath;
  const UseLinkBasenames: Boolean; StagedAssets, FinalAssets: TStrings;
  out HasStagedAssets: Boolean);
var I, J: Integer; Entry: TBitmapEntry;
  Link, SourcePath, FinalPath, StagePath, Directory: TDocumentPath;
  Snapshot: TDocumentSnapshot; Duplicate: Boolean;
begin
  if (Drawing = nil) or (StagedAssets = nil) or (FinalAssets = nil) then
    raise EWriteError.Create('A drawing and asset lists are required');
  HasStagedAssets := False;
  Directory := DocumentPathDirectory(Destination);
  for I := 0 to Drawing.BitmapRegistry.Count - 1 do
  begin
    Entry := Drawing.BitmapRegistry.Objects[I] as TBitmapEntry;
    Link := TDocumentPath(Entry.ImageLink);
    if Link = '' then Continue;
    if UseLinkBasenames then
    begin
      if IsAbsoluteDocumentLink(Link) or (Link = '.') or (Link = '..') or
        (Pos('/', string(Link)) > 0) or (Pos('\', string(Link)) > 0) or
        (Pos(':', string(Link)) > 0) or (Pos(#0, string(Link)) > 0) then
        raise EWriteError.CreateFmt(
          'This format requires bitmap links to be safe file basenames: "%s"',
          [string(Link)]);
    end
    else if IsAbsoluteDocumentLink(Link) then Continue;
    SourcePath := NormalizeDocumentPath(TDocumentPath(Entry.GetFullLink));
    FinalPath := NormalizeDocumentPath(Directory + PathDelim + Link);
    if SameDocumentPath(SourcePath, FinalPath) then Continue;
    Duplicate := False;
    for J := 0 to FinalAssets.Count - 1 do
      if SameDocumentPath(TDocumentPath(FinalAssets[J]), FinalPath) then
      begin
        Duplicate := True;
        Break;
      end;
    if Duplicate then Continue;
    Snapshot := ReadDocumentSnapshot(SourcePath);
    if UseLinkBasenames then
      StagePath := IncludeTrailingPathDelimiter(StageDirectory) + Link
    else
      StagePath := IncludeTrailingPathDelimiter(StageDirectory) +
        'bitmap-' + IntToStr(I) + DocumentPathExtension(Link);
    if SameDocumentPath(StagePath, StagingFileName) then
      raise EWriteError.Create('A bitmap asset collides with the staged document');
    StagedAssets.Add(string(StagePath));
    FinalAssets.Add(string(FinalPath));
    WriteDocumentStagingFile(StagePath, Snapshot.SourceBytes);
    HasStagedAssets := True;
  end;
end;

constructor TDocumentSaveTransaction.Create(const FormatId: string;
  Drawing: TDrawing2D; const FileName: TDocumentPath;
  const ExpectedRevision: TDiskRevision; CodecContext: TObject;
  const AllowExternalTools: Boolean);
var Stream: TMemoryStream; Candidate: TDrawing2D;
  CandidateFileName: TDocumentPath; HasStagedAssets,
  UseStagedValidation: Boolean; Format: TDocumentFormat;
begin
  inherited Create;
  FFormatId := FormatId;
  FDrawing := Drawing;
  FExpectedRevision := ExpectedRevision;
  FCodecContext := nil;
  FPublished := False;
  FCommitted := False;
  FStagedAssets := TStringList.Create;
  FFinalAssets := TStringList.Create;
  FDiagnostics := TStringList.Create;
  Stream := nil;
  Candidate := nil;
  try
    if Drawing = nil then
      raise EWriteError.Create('A drawing is required for serialization');
    if not FindDocumentFormatById(FormatId, Format) or
      not Format.CanSaveBack or not IsDocumentCodecRegistered(FormatId) then
      raise EWriteError.CreateFmt('No save-back codec is available for %s',
        [FormatId]);
    FPath := NormalizeDocumentPath(FileName);
    FStageDirectory := CreateDocumentStagingDirectory(
      DocumentPathDirectory(FPath));
    if FStageDirectory = '' then
      raise EWriteError.Create('Could not create a private staging directory');
    FStagingFileName := IncludeTrailingPathDelimiter(FStageDirectory) +
      DocumentPathFileName(FPath);
    Stream := TMemoryStream.Create;
    if not SerializeDocumentForSave(FormatId, Drawing, FPath, FStagingFileName,
      Stream, CodecContext, FStagedAssets, FFinalAssets, AllowExternalTools) then
      raise EWriteError.Create('The document could not be serialized');
    UseStagedValidation := DocumentCodecUsesStagedValidationPath(FormatId);
    StageRelativeBitmapAssets(Drawing, FPath, FStageDirectory,
      FStagingFileName, UseStagedValidation, FStagedAssets, FFinalAssets,
      HasStagedAssets);
    if Stream.Size > MaxDocumentBytes then
      raise EWriteError.CreateFmt('Document exceeds the %d byte limit',
        [MaxDocumentBytes]);
    SetLength(FBytes, Stream.Size);
    if Stream.Size > 0 then
    begin
      Stream.Position := 0;
      Stream.ReadBuffer(FBytes[1], Stream.Size);
    end;
    CandidateFileName := FPath;
    if UseStagedValidation and (FStagedAssets.Count > 0) then
    begin
      WriteDocumentStagingFile(FStagingFileName, FBytes);
      CandidateFileName := FStagingFileName;
    end;
    Candidate := LoadDocumentCandidate(FormatId, CandidateFileName, FBytes,
      FCanSaveBack, FCodecContext, FDiagnostics);
    if (Candidate = nil) or not FCanSaveBack then
      raise EWriteError.CreateFmt(
        'The serialized %s document cannot be safely reopened for editing',
        [FormatId]);
    RebindDocumentCodecContext(FormatId, Drawing, Candidate, FCodecContext);
  except
    Candidate.Free;
    Stream.Free;
    raise;
  end;
  Candidate.Free;
  Stream.Free;
end;

destructor TDocumentSaveTransaction.Destroy;
var I: Integer;
begin
  try FCodecContext.Free except end;
  if not FCommitted and (FStagedAssets <> nil) then
    for I := 0 to FStagedAssets.Count - 1 do
      try
        if DocumentFileExists(TDocumentPath(FStagedAssets[I])) then
          DeleteDocumentFile(TDocumentPath(FStagedAssets[I]));
      except
        { Preserve the original save failure; staged files are private temp data. }
      end;
  if FStagingFileName <> '' then
    try
      if DocumentFileExists(FStagingFileName) then
        DeleteDocumentFile(FStagingFileName);
    except
    end;
  if FStageDirectory <> '' then
    try
      RemoveDocumentStagingDirectory(FStageDirectory);
    except
    end;
  if FDiagnostics <> nil then FDiagnostics.Free;
  if FFinalAssets <> nil then FFinalAssets.Free;
  if FStagedAssets <> nil then FStagedAssets.Free;
  inherited Destroy;
end;

procedure TDocumentSaveTransaction.Publish(out NewRevision: TDiskRevision;
  out BackupFileName: TDocumentPath);
begin
  if FPublished then
    raise EWriteError.Create('The document save transaction was already published');
  WriteDocumentBundleAtomically(FPath, FBytes, FExpectedRevision, True,
    FStagedAssets, FFinalAssets, NewRevision, BackupFileName);
  FDrawing.RebindFileNameAfterAssetCommit(FPath);
  FNewRevision := NewRevision;
  FBackupFileName := BackupFileName;
  FPublished := True;
end;

procedure TDocumentSaveTransaction.AppendBrowserDownloadAssets(
  Paths: TStrings);
var
  I: Integer;
  Entry: TBitmapEntry;
  Link, Path: TDocumentPath;
  procedure AddUnique(const Value: TDocumentPath);
  var J: Integer;
  begin
    for J := 0 to Paths.Count - 1 do
      if SameDocumentPath(TDocumentPath(Paths[J]), Value) then Exit;
    Paths.Add(string(Value));
  end;
begin
  if Paths = nil then
    raise EArgumentNilException.Create('A browser download path list is required');
  for I := 0 to FFinalAssets.Count - 1 do
    AddUnique(TDocumentPath(FFinalAssets[I]));
  for I := 0 to FDrawing.BitmapRegistry.Count - 1 do
  begin
    Entry := FDrawing.BitmapRegistry.Objects[I] as TBitmapEntry;
    Link := TDocumentPath(Entry.ImageLink);
    if Link = '' then Continue;
    if IsAbsoluteDocumentLink(Link) or (Link = '.') or (Link = '..') or
      (Pos('/', string(Link)) > 0) or (Pos('\', string(Link)) > 0) or
      (Pos(':', string(Link)) > 0) or (Pos(#0, string(Link)) > 0) then
      raise EWriteError.CreateFmt(
        'This browser cannot download linked bitmap paths safely: "%s"',
        [string(Link)]);
    Path := NormalizeDocumentPath(DocumentPathDirectory(FPath) +
      PathDelim + Link);
    AddUnique(Path);
  end;
end;

procedure TDocumentSaveTransaction.Accept(Session: TDocumentSession);
var SameSource: Boolean;
begin
  if not FPublished then
    raise EWriteError.Create('The document save transaction was not published');
  if FCommitted then
    raise EWriteError.Create('The document save transaction was already accepted');
  if Session = nil then
    raise EWriteError.Create('A document session is required');
  SameSource := (Session.SourcePath <> '') and
    SameDocumentPath(Session.SourcePath, FPath) and
    SameText(Session.SourceFormatId, FFormatId) and Session.CanSaveBack;
  if SameSource then
    Session.AcceptSavedRevision(FBytes, FNewRevision, FCodecContext,
      FBackupFileName)
  else
    Session.RebindSavedSourceNormalized(FPath, FFormatId, FBytes,
      FNewRevision, FCanSaveBack, FCodecContext, FBackupFileName);
  FCodecContext := nil;
  Session.Diagnostics.Assign(FDiagnostics);
  FCommitted := True;
end;

procedure TDocumentSaveTransaction.Commit(Session: TDocumentSession;
  out NewRevision: TDiskRevision; out BackupFileName: TDocumentPath);
begin
  Publish(NewRevision, BackupFileName);
  Accept(Session);
end;

procedure RegisterDocumentCodec(const Format: TDocumentFormat;
  Loader: TDocumentLoader; Saver: TDocumentSaver;
  ContextRebinder: TDocumentContextRebinder;
  SaveValidationPath: TDocumentSaveValidationPath);
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
  C.ContextRebinder := ContextRebinder;
  C.SaveValidationPath := SaveValidationPath;
  RegisterDocumentFormat(Format);
end;

function IsDocumentCodecRegistered(const FormatId: string): Boolean;
begin
  Result := FindCodec(FormatId) <> nil;
end;

function PstoeditEmfImportAvailable: Boolean;
begin
{$IFDEF WASI}
  Result := False;
{$ELSE}
  Result := (Trim(PsToEditPath) <> '') and
    (Pos('emf', LowerCase(PsToEditFormat)) > 0) and
    (Pos('svg', LowerCase(PsToEditFormat)) = 0);
{$ENDIF}
end;

function DocumentCodecUsesStagedValidationPath(
  const FormatId: string): Boolean;
var C: TDocumentCodec;
begin
  C := FindCodec(FormatId);
  Result := (C <> nil) and
    (C.SaveValidationPath = dsvpPrivateStage);
end;

procedure RebindDocumentCodecContext(const FormatId: string;
  LiveDrawing, ParsedDrawing: TDrawing2D; CodecContext: TObject);
var C: TDocumentCodec;
begin
  C := FindCodec(FormatId);
  if (C <> nil) and Assigned(C.ContextRebinder) and
    (CodecContext <> nil) then
    C.ContextRebinder(LiveDrawing, ParsedDrawing, CodecContext);
end;

function LoadDocumentCandidate(const FormatId: string;
  const FileName: TDocumentPath;
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
    Result.FileName := NormalizeDocumentPath(FileName);
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
  Drawing: TDrawing2D; const FileName: TDocumentPath;
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

function SerializeDocumentForSave(const FormatId: string;
  Drawing: TDrawing2D; const FileName, StagingFileName: TDocumentPath;
  Destination: TStream; CodecContext: TObject; StagedAssets,
  FinalAssets: TStrings; const AllowExternalTools: Boolean): Boolean;
var C: TDocumentCodec;
begin
  if (Drawing = nil) or (Destination = nil) then
    raise EWriteError.Create('A drawing and destination stream are required');
  if (StagedAssets = nil) or (FinalAssets = nil) then
    raise EWriteError.Create('Asset lists are required for staged serialization');
  StagedAssets.Clear;
  FinalAssets.Clear;
  Destination.Size := 0;
  Destination.Position := 0;
  if SameText(FormatId, 'tpx') then
  begin
    Result := StoreToStream_TpXDocumentStaged(Drawing, Destination,
      FileName, StagingFileName, False, StagedAssets, FinalAssets,
      AllowExternalTools);
    if not Result then
      raise EWriteError.Create('The TpX writer did not produce a document');
    Exit;
  end;
  C := FindCodec(FormatId);
  if (C = nil) or not Assigned(C.Saver) then
    raise EWriteError.CreateFmt('No writer is registered for %s', [FormatId]);
  Result := C.Saver(Drawing, FileName, Destination, CodecContext);
  if not Result then
    raise EWriteError.CreateFmt('The %s writer did not produce a document',
      [FormatId]);
  if Destination.Size > MaxDocumentBytes then
    raise EWriteError.CreateFmt('Document exceeds the %d byte limit',
      [MaxDocumentBytes]);
end;

function SerializeTpXRecoveryBytes(Drawing: TDrawing2D;
  const FileName: TDocumentPath): RawByteString;
var Stream: TMemoryStream;
begin
  if Drawing = nil then
    raise EWriteError.Create('A drawing is required for serialization');
  Stream := TMemoryStream.Create;
  try
    if not StoreToStream_TpXModel(Drawing, Stream, FileName) then
      raise EWriteError.Create('The TpX model writer produced no document');
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

function LoadTpX(Candidate: TDrawing2D; const FileName: TDocumentPath;
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

function LoadMetafile(Candidate: TDrawing2D; const FileName: TDocumentPath;
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

function LoadPstoeditEmf(Candidate: TDrawing2D;
  const FileName: TDocumentPath; const SourceBytes: RawByteString;
  Diagnostics: TStrings; out CodecContext: TObject): Boolean;
var StageDirectory, InputFileName, OutputFileName: TDocumentPath;
  Extension, Command: string; Snapshot: TDocumentSnapshot;
  Stream: TMemoryStream;
begin
  CodecContext := nil;
{$IFDEF WASI}
  raise EReadError.Create(
    'EPS, PS, and PDF conversion is unavailable in the browser');
{$ELSE}
  if Trim(PsToEditPath) = '' then
    raise EReadError.Create('Configure a pstoedit executable to import EPS, PS, or PDF');
  if (Pos('emf', LowerCase(PsToEditFormat)) = 0) or
    (Pos('svg', LowerCase(PsToEditFormat)) > 0) then
    raise EReadError.CreateFmt(
      'The configured pstoedit format "%s" does not produce an importable EMF; choose an EMF output format',
      [PsToEditFormat]);
  Extension := DocumentPathExtension(FileName);
  if Extension = '' then Extension := '.ps';
  StageDirectory := CreateDocumentStagingDirectory(
    TDocumentPath(GetTempDir));
  if StageDirectory = '' then
    raise EReadError.Create('Could not create a private pstoedit staging directory');
  InputFileName := IncludeTrailingPathDelimiter(StageDirectory) +
    'source' + Extension;
  OutputFileName := IncludeTrailingPathDelimiter(StageDirectory) + 'converted.emf';
  try
    WriteDocumentStagingFile(InputFileName, SourceBytes);
    Command := PrepareFilePath(PsToEditPath) + ' ' +
      QuoteShellArg(string(InputFileName)) + ' ' +
      QuoteShellArg(string(OutputFileName)) + ' -f ' +
      QuoteShellArg(PsToEditFormat);
    if not FileExec(Command, '', '', string(StageDirectory), True, True) then
      raise EReadError.Create('The configured pstoedit conversion failed');
    if not DocumentFileExists(OutputFileName) then
      raise EReadError.Create('pstoedit completed without creating an EMF file');
    Snapshot := ReadDocumentSnapshot(OutputFileName);
    Stream := TMemoryStream.Create;
    try
      if Length(Snapshot.SourceBytes) > 0 then
        Stream.WriteBuffer(Snapshot.SourceBytes[1], Length(Snapshot.SourceBytes));
      Stream.Position := 0;
      Import_MetafileFromStream(Candidate, Stream, False);
    finally
      Stream.Free;
    end;
    Candidate.Comment := Format('Imported from %s via pstoedit %s',
      [DocumentPathFileName(FileName), DateTimeToStr(Now)]);
    Result := False;
  finally
    DeleteDocumentFile(InputFileName);
    DeleteDocumentFile(OutputFileName);
    RemoveDocumentStagingDirectory(StageDirectory);
  end;
{$ENDIF}
end;

function SaveTpX(Drawing: TDrawing2D; const FileName: TDocumentPath;
  Destination: TStream; CodecContext: TObject): Boolean;
begin
  Result := StoreToStream_TpXDocument(Drawing, Destination, FileName, False);
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
  FindDocumentFormatById('pstoedit-emf', F);
  RegisterDocumentCodec(F, @LoadPstoeditEmf, nil);
end;

initialization
  Codecs := TObjectList.Create(True);
  RegisterBuiltInCodecs;

finalization
  Codecs.Free;

end.

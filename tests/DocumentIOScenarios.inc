procedure WriteDocumentTestBytes(const FileName: string;
  const Bytes: RawByteString);
var Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    if Length(Bytes) > 0 then Stream.WriteBuffer(Bytes[1], Length(Bytes));
  finally
    Stream.Free;
  end;
end;

function ReadDocumentTestBytes(const FileName: string): RawByteString;
var Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, Stream.Size);
    if Stream.Size > 0 then Stream.ReadBuffer(Result[1], Stream.Size);
  finally
    Stream.Free;
  end;
end;

function ReadDocumentTestDate(const FileName: string): Int64;
{$IFDEF UNIX}
var Info: BaseUnix.Stat;
{$ELSE}
var Handle: THandle;
{$ENDIF}
begin
{$IFDEF UNIX}
  if fpStat(PChar(FileName), Info) <> 0 then RaiseLastOSError;
  Result := Info.st_mtime;
{$ELSE}
  Handle := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
  if Handle < 0 then RaiseLastOSError;
  try
    Result := FileGetDate(Handle);
  finally
    FileClose(Handle);
  end;
{$ENDIF}
end;

function SetDocumentTestDate(const FileName: string;
  const Age: Int64): LongInt;
{$IFDEF UNIX}
var Times: BaseUnix.UTimBuf;
{$ELSE}
var Handle: THandle;
{$ENDIF}
begin
{$IFDEF UNIX}
  Times.actime := Age;
  Times.modtime := Age;
  Result := fpUtime(PChar(FileName), @Times);
{$ELSE}
  Handle := FileOpen(FileName, fmOpenReadWrite or fmShareDenyNone);
  if Handle < 0 then RaiseLastOSError;
  try
    Result := FileSetDate(Handle, LongInt(Age));
  finally
    FileClose(Handle);
  end;
{$ENDIF}
end;

function CountDirectoryEntries(const Directory: string): Integer;
var Search: TSearchRec; Code: Integer;
begin
  Result := 0;
  Code := FindFirst(IncludeTrailingPathDelimiter(Directory) + '*',
    faAnyFile, Search);
  if Code = 0 then
  try
    repeat
      if (Search.Name <> '.') and (Search.Name <> '..') then Inc(Result);
      Code := FindNext(Search);
    until Code <> 0;
  finally
    FindClose(Search);
  end;
end;

procedure PrepareDocumentTestDirectory(const Name: string;
  out Directory, FileName: string);
begin
  Directory := IncludeTrailingPathDelimiter(GetCurrentDir) + Name;
  Check(not DirectoryExists(Directory), 'Document I/O test directory already exists');
  Check(CreateDir(Directory), 'Could not create document I/O test directory');
  FileName := IncludeTrailingPathDelimiter(Directory) + 'drawing.tpx';
end;

procedure TestDocumentRevisionConflict;
var Directory, FileName: string;
  Backup: TDocumentPath;
  Original, ExternalBytes: RawByteString;
  Expected, ExternalRevision, AfterFailure, NewRevision: TDiskRevision;
  OriginalDate: Int64; ConflictRaised: Boolean;
begin
  PrepareDocumentTestDirectory('issue62-conflict', Directory, FileName);
  Original := 'drawing-A';
  ExternalBytes := 'drawing-B';
  try
    WriteDocumentTestBytes(FileName, Original);
    OriginalDate := ReadDocumentTestDate(FileName);
    Check(OriginalDate <> -1, 'Could not read the original document timestamp');
    Check(SetDocumentTestDate(FileName, OriginalDate) = 0,
      'Could not normalize the original document timestamp');
    Expected := ReadDocumentRevision(FileName);

    WriteDocumentTestBytes(FileName, ExternalBytes);
    Check(SetDocumentTestDate(FileName, OriginalDate) = 0,
      'Could not restore the document timestamp');
    Check(ReadDocumentTestDate(FileName) = OriginalDate,
      'The external edit did not retain the original timestamp');
    ExternalRevision := ReadDocumentRevision(FileName);
    Check((ExternalRevision.Size = Expected.Size) and
      (ExternalRevision.ModifiedUTC = Expected.ModifiedUTC) and
      (ExternalRevision.Identity = Expected.Identity),
      'The external edit did not preserve the size, timestamp, and file identity');
    Check(ExternalRevision.ContentDigest <> Expected.ContentDigest,
      'The external edit did not change the content digest');

    ConflictRaised := False;
    Backup := '';
    try
      WriteDocumentAtomically(FileName, 'our-save', Expected, True,
        NewRevision, Backup);
    except
      on E: EDocumentConflict do ConflictRaised := True;
    end;
    Check(ConflictRaised, 'A same-size edit with restored timestamp was not rejected');
    Check(ReadDocumentTestBytes(FileName) = ExternalBytes,
      'A failed conflict check changed the current document bytes');
    AfterFailure := ReadDocumentRevision(FileName);
    Check(SameDiskRevision(AfterFailure, ExternalRevision),
      'A failed conflict check changed the current document revision');
    Check(Backup = '', 'A conflict exposed a backup file');
    Check(CountDirectoryEntries(Directory) = 1,
      'A conflict left a staging or backup file behind');
  finally
    if FileExists(FileName) then DeleteFile(FileName);
    RemoveDir(Directory);
  end;
end;

procedure TestDocumentSavedRevision;
var Directory, FileName: string;
  Backup: TDocumentPath;
  SavedBytes, ExternalBytes: RawByteString;
  Expected, SavedRevision, DiskRevision: TDiskRevision;
  OriginalDate: Int64;
  Session: TDocumentSession;
begin
  PrepareDocumentTestDirectory('issue62-saved-revision', Directory, FileName);
  SavedBytes := 'saved-version';
  ExternalBytes := 'other-version';
  Session := TDocumentSession.Create;
  try
    WriteDocumentTestBytes(FileName, 'before-save');
    Expected := ReadDocumentRevision(FileName);
    Backup := '';
    WriteDocumentAtomically(FileName, SavedBytes, Expected, True,
      SavedRevision, Backup);
    try
      Check(ReadDocumentTestBytes(FileName) = SavedBytes,
        'The successful save did not publish the requested bytes');
      DiskRevision := ReadDocumentRevision(FileName);
      Check(SameDiskRevision(SavedRevision, DiskRevision),
        'The writer did not return the revision of its published bytes');
      Check((Backup <> '') and FileExists(Backup),
        'The successful replacement did not preserve its prior version');
      Check(ReadDocumentTestBytes(Backup) = 'before-save',
        'The replacement backup did not contain the prior document');

      Session.AcceptSavedRevision(SavedBytes, SavedRevision);
      Check(Session.IsAcceptedRevision(DiskRevision),
        'The saved revision was not accepted by the document session');
      OriginalDate := ReadDocumentTestDate(FileName);
      WriteDocumentTestBytes(FileName, ExternalBytes);
      Check(SetDocumentTestDate(FileName, OriginalDate) = 0,
        'Could not restore the saved timestamp after an external edit');
      DiskRevision := ReadDocumentRevision(FileName);
      Check(DiskRevision.Size = SavedRevision.Size,
        'The subsequent external edit changed the saved document size');
{$IFDEF UNIX}
      Check(DiskRevision.ModifiedUTC = SavedRevision.ModifiedUTC,
        'The subsequent external edit changed the saved timestamp');
{$ENDIF}
      Check(DiskRevision.ContentDigest <> SavedRevision.ContentDigest,
        'The subsequent external edit did not change the saved content digest');
      Check(not Session.IsAcceptedRevision(DiskRevision),
        'The session accepted subsequent external content as its saved revision');
      Check(Session.OriginalSourceBytes = SavedBytes,
        'The session lost the exact bytes it saved');
    finally
      if (Backup <> '') and FileExists(Backup) then DeleteFile(Backup);
    end;
  finally
    Session.Free;
    if FileExists(FileName) then DeleteFile(FileName);
    RemoveDir(Directory);
  end;
end;

procedure TestDocumentUnixPermissions;
{$IFDEF UNIX}
var Directory, FileName: string;
  Backup: TDocumentPath;
  Expected, NewRevision: TDiskRevision;
  Info, BackupInfo: BaseUnix.Stat;
{$ENDIF}
begin
{$IFDEF UNIX}
  PrepareDocumentTestDirectory('issue62-permissions', Directory, FileName);
  try
    WriteDocumentTestBytes(FileName, 'permission-check');
    Check(fpChmod(PChar(FileName), $0180) = 0,
      'Could not set the existing document permissions to 0600');
    Expected := ReadDocumentRevision(FileName);
    Backup := '';
    WriteDocumentAtomically(FileName, 'replacement', Expected, True,
      NewRevision, Backup);
    try
      Check(fpStat(PChar(FileName), Info) = 0,
        'Could not inspect replacement document permissions');
      Check((Info.st_mode and $0FFF) = $0180,
        'Atomic replacement did not retain the existing 0600 permissions');
      Check(ReadDocumentTestBytes(FileName) = 'replacement',
        'The permissions-preserving replacement has incorrect content');
      Check(fpStat(PChar(Backup), BackupInfo) = 0,
        'Could not inspect replacement backup permissions');
      Check((BackupInfo.st_mode and $0FFF) = $0180,
        'Atomic replacement backup did not retain the existing 0600 permissions');
      Check(ReadDocumentTestBytes(Backup) = 'permission-check',
        'The permissions-preserving backup has incorrect content');
    finally
      if (Backup <> '') and FileExists(Backup) then DeleteFile(Backup);
    end;
  finally
    if FileExists(FileName) then DeleteFile(FileName);
    RemoveDir(Directory);
  end;
{$ENDIF}
end;

procedure TestMalformedDocumentCandidatePreservesLiveDrawing;
var Directory, FileName, BadFileName: string;
  BadBytes: RawByteString;
  Drawing: TDrawing2D;
  Line: TLine2D;
  Session: TDocumentSession;
  SourceRevision, AcceptedRevision: TDiskRevision;
  ViewRect: TRect2D;
  History: TDrawHistory;
  HistoryCount: Integer;
  Candidate: TDrawing2D;
  CodecContext: TObject;
  CanSaveBack, Failed: Boolean;
  OpenSucceeded: Boolean;
  ErrorText: string;
  LocalRevision: QWord;
  procedure CheckLiveState;
  begin
    Check((Drawing.ObjectsCount = 1) and
      (Drawing.ObjectList.FirstObj = Line),
      'Malformed input replaced or removed live geometry');
    Check(IsSamePoint2D(Line.Points[0], Point2D(1.25, 2.5)) and
      IsSamePoint2D(Line.Points[1], Point2D(8.75, 9.5)),
      'Malformed input changed live line geometry');
    Check((Drawing.SelectedObjects.Count = 1) and
      (Drawing.SelectionFirst = Line),
      'Malformed input changed the live selection');
    Check((Drawing.History = History) and Drawing.History.IsChanged and
      (Drawing.History.Count = HistoryCount),
      'Malformed input changed the dirty live history');
    Check((Drawing.FileName = FileName) and
      (MainForm.LocalView.VisualRect.Left = ViewRect.Left) and
      (MainForm.LocalView.VisualRect.Bottom = ViewRect.Bottom) and
      (MainForm.LocalView.VisualRect.Right = ViewRect.Right) and
      (MainForm.LocalView.VisualRect.Top = ViewRect.Top),
      'Malformed input changed the source path or viewport');
    Check((Session.SourcePath = FileName) and
      (Session.SourceFormatId = 'tpx') and
      (Session.OriginalSourceBytes = 'live-source-bytes') and
      SameDiskRevision(Session.AcceptedRevision, AcceptedRevision) and
      (Session.LocalRevision = LocalRevision),
      'Malformed input changed the accepted source identity');
  end;
begin
  PrepareDocumentTestDirectory('issue62-malformed-candidate', Directory,
    FileName);
  Drawing := MainForm.TheDrawing;
  Session := MainForm.EventManager.DocumentSession;
  Candidate := nil;
  CodecContext := nil;
  try
    Drawing.Clear;
    if Drawing.History = nil then
      Drawing.History := TDrawHistory.Create(Drawing);
    Drawing.History.Clear;
    Drawing.History.Save;
    Drawing.History.SaveCheckSum;
    Drawing.FileName := FileName;
    Line := TLine2D.CreateSpec(-1, Point2D(1.25, 2.5), Point2D(8.75, 9.5));
    Drawing.AddObject(-1, Line);
    Drawing.SelectionAdd(Line);
    Check(Drawing.History.IsChanged,
      'The live drawing setup did not establish a dirty history');
    MainForm.LocalView.VisualRect := Rect2D(-4, -3, 21, 17);
    ViewRect := MainForm.LocalView.VisualRect;

    SourceRevision.ContentDigest := 'live-source-digest';
    SourceRevision.Size := 41;
    SourceRevision.ModifiedUTC := 1234;
    SourceRevision.Identity := 'live-source-identity';
    SourceRevision.Exists := True;
    Session.AcceptSource(FileName, 'tpx', 'live-source-bytes',
      SourceRevision, True);
    AcceptedRevision := Session.AcceptedRevision;
    LocalRevision := Session.LocalRevision;
    History := Drawing.History;
    HistoryCount := History.Count;
    BadFileName := IncludeTrailingPathDelimiter(Directory) + 'malformed.tpx';
    BadBytes := '<picture>(0,0)' + LineEnding +
      '%<TpX v="5"><line x1="0"' + LineEnding + '</picture>';

    Failed := False;
    try
      Candidate := LoadDocumentCandidate('tpx', BadFileName,
        BadBytes, CanSaveBack,
        CodecContext);
    except
      on E: Exception do Failed := True;
    end;
    if Candidate <> nil then FreeAndNil(Candidate);
    FreeAndNil(CodecContext);
    Check(Failed, 'A malformed TpX candidate was accepted');
    CheckLiveState;

    WriteDocumentTestBytes(BadFileName, BadBytes);
    PromptCount := 0;
    OpenSucceeded := BaseMode.OpenDocumentFromPath(BadFileName, ErrorText);
    Check(not OpenSucceeded and (ErrorText <> ''),
      'The no-dialog Open route accepted malformed TpX input');
    Check(PromptCount = 0,
      'The no-dialog Open route displayed a save prompt');
    CheckLiveState;
  finally
    FreeAndNil(Candidate);
    FreeAndNil(CodecContext);
    Drawing.Clear;
    Session.Clear;
    if FileExists(BadFileName) then DeleteFile(BadFileName);
    RemoveDir(Directory);
  end;
end;

procedure TestDocumentCandidateCommitUsesCandidateOptions;
const SourceBytes: RawByteString = '<picture>(0,0)' + LineEnding +
  '%<TpX v="5" PicScale="2.75" FontName="Candidate Font">' + LineEnding +
  '%<text x="2" y="3" h="4" t="Candidate text"/>' + LineEnding +
  '%</TpX>' + LineEnding + '</picture>' + LineEnding;
var Directory, FileName: string;
  Drawing, Candidate: TDrawing2D;
  Text: TText2D;
  Piece: TTextPiece;
  CodecContext: TObject;
  CanSaveBack: Boolean;
begin
  PrepareDocumentTestDirectory('issue62-candidate-commit', Directory,
    FileName);
  Drawing := MainForm.TheDrawing;
  Candidate := nil;
  CodecContext := nil;
  try
    Drawing.Clear;
    if Drawing.History = nil then
      Drawing.History := TDrawHistory.Create(Drawing);
    Drawing.History.Clear;
    Drawing.History.Save;
    Drawing.History.SaveCheckSum;
    Drawing.AddObject(-1,
      TLine2D.CreateSpec(-1, Point2D(0, 0), Point2D(1, 1)));
    Check(Drawing.History.IsChanged,
      'The pre-commit drawing was not dirty');

    Candidate := LoadDocumentCandidate('tpx', FileName, SourceBytes,
      CanSaveBack, CodecContext);
    Check(CanSaveBack, 'TpX candidate did not preserve its editable source');
    Check((Candidate.ObjectsCount = 1) and
      (Candidate.ObjectList.FirstObj is TText2D),
      'TpX candidate did not load its text object');
    CommitDocumentCandidate(Drawing, Candidate);

    Check((Drawing.ObjectsCount = 1) and
      (Drawing.ObjectList.FirstObj is TText2D),
      'Committing the candidate did not replace the live geometry');
    Check(Abs(Drawing.PicScale - 2.75) < 0.0001,
      'The committed drawing did not adopt the candidate picture scale');
    Check(Drawing.FontName = 'Candidate Font',
      'The committed drawing did not adopt the candidate font name');
    Text := Drawing.ObjectList.FirstObj as TText2D;
    Check((Text.OwnerDrawing = Drawing) and (Text.ParentDrawing = Drawing),
      'The committed text object does not belong to the live drawing');
    Check(Text.FaceName = 'Candidate Font',
      'The committed text object did not inherit the candidate font');
    Check((Text.Pieces.Count = 1) and (Text.Pieces[0] is TTextPiece),
      'The committed text has no cached text piece');
    Piece := Text.Pieces[0] as TTextPiece;
    Check(Piece.FaceName = 'Candidate Font',
      'The cached text piece retained the old live drawing font');
    Check(not Drawing.History.IsChanged,
      'The committed candidate is incorrectly marked dirty');
  finally
    FreeAndNil(Candidate);
    FreeAndNil(CodecContext);
    Drawing.Clear;
    RemoveDir(Directory);
  end;
end;

procedure TestDocumentBundleRollbackOnPrimaryConflict;
const InitialBytes: RawByteString = '<picture>(0,0)' + LineEnding +
  '%<TpX v="5" PicScale="1" FontName="Original Font">' + LineEnding +
  '%</TpX>' + LineEnding + '</picture>' + LineEnding;
var Directory, FileName, EPSFileName, PDFFileName, ErrorText,
  RecoveryBackup: string;
  ProbeDirectory, ProbeBase: string;
  Drawing: TDrawing2D;
  Session: TDocumentSession;
  InitialRevision, SavedRevision, ExternalRevision: TDiskRevision;
  SavedSourceBytes, ExternalBytes, SavedEPSBytes, SavedPDFBytes,
    ExpectedEPSBytes, ExpectedPDFBytes:
    RawByteString;
  SavedDate: Int64;
  SavedLocalRevision: QWord;
  SavedEntryCount: Integer;
  Saved, SaveFailed: Boolean;
  ProbeStream: TMemoryStream;
  ProbeStaged, ProbeFinal: TStringList;
  I: Integer;
{$IFDEF UNIX}
  Info: BaseUnix.Stat;
{$ENDIF}
begin
  PrepareDocumentTestDirectory('issue62-bundle-rollback', Directory,
    FileName);
  EPSFileName := ChangeFileExt(FileName, '.eps');
  PDFFileName := ChangeFileExt(FileName, '.pdf');
  Drawing := MainForm.TheDrawing;
  Session := MainForm.EventManager.DocumentSession;
  ProbeDirectory := '';
  ProbeStream := nil;
  ProbeStaged := nil;
  ProbeFinal := nil;
  try
    WriteDocumentTestBytes(FileName, InitialBytes);
    WriteDocumentTestBytes(EPSFileName, 'old eps sidecar');
    WriteDocumentTestBytes(PDFFileName, 'old pdf sidecar');
{$IFDEF UNIX}
    Check(fpChmod(PChar(FileName), $0180) = 0,
      'Could not set the source document permissions to 0600');
    Check(fpChmod(PChar(EPSFileName), $0180) = 0,
      'Could not set the EPS sidecar permissions to 0600');
    Check(fpChmod(PChar(PDFFileName), $0180) = 0,
      'Could not set the PDF sidecar permissions to 0600');
{$ENDIF}
    InitialRevision := ReadDocumentRevision(FileName);
    Check(BaseMode.OpenDocumentFromPath(FileName, ErrorText),
      'Could not load the initial bundle source: ' + ErrorText);
    Check(SameDiskRevision(Session.AcceptedRevision, InitialRevision),
      'Opening the bundle source did not accept its current revision');
    Drawing.TeXFormat := tex_eps;
    Drawing.PdfTeXFormat := pdftex_pdf;
    Drawing.TeXFigure := fig_none;
    Drawing.TeXCenterFigure := False;
    Drawing.AddObject(-1,
      TLine2D.CreateSpec(-1, Point2D(1, 2), Point2D(31, 22)));
    Check(Drawing.History.IsChanged,
      'The first bundle save did not start from a dirty drawing');

    Saved := BaseMode.SaveDocumentToPath(FileName, 'tpx', False, False,
      ErrorText);
    Check(Saved, 'The first bundle save failed: ' + ErrorText);
    SavedEPSBytes := ReadDocumentTestBytes(EPSFileName);
    SavedPDFBytes := ReadDocumentTestBytes(PDFFileName);
    Check((Pos('%!PS-Adobe', string(SavedEPSBytes)) = 1) and
      (Pos('%PDF-', string(SavedPDFBytes)) > 0),
      'The save route did not publish fresh EPS and PDF sidecars');
    Check(not Drawing.History.IsChanged,
      'The first bundle save did not accept the saved drawing');
{$IFDEF UNIX}
    Check(fpStat(PChar(FileName), Info) = 0,
      'Could not inspect the saved source permissions');
    Check((Info.st_mode and $0FFF) = $0180,
      'The saved source did not retain its existing 0600 permissions');
    Check(fpStat(PChar(EPSFileName), Info) = 0,
      'Could not inspect the saved EPS permissions');
    Check((Info.st_mode and $0FFF) = $0180,
      'The saved EPS sidecar did not retain its existing 0600 permissions');
    Check(fpStat(PChar(PDFFileName), Info) = 0,
      'Could not inspect the saved PDF permissions');
    Check((Info.st_mode and $0FFF) = $0180,
      'The saved PDF sidecar did not retain its existing 0600 permissions');
{$ENDIF}

    SavedSourceBytes := Session.OriginalSourceBytes;
    SavedRevision := Session.AcceptedRevision;
    RecoveryBackup := Session.RecoveryBackupPath;
    Check((SavedSourceBytes = ReadDocumentTestBytes(FileName)) and
      SameDiskRevision(SavedRevision, ReadDocumentRevision(FileName)),
      'The successful bundle save did not accept its exact primary revision');

    Drawing.AddObject(-1,
      TLine2D.CreateSpec(-1, Point2D(4, 6), Point2D(22, 18)));
    Check(Drawing.History.IsChanged,
      'The conflicting bundle save did not start from a dirty drawing');
    SavedLocalRevision := Session.LocalRevision;
    ProbeDirectory := CreateDocumentStagingDirectory(Directory);
    Check(ProbeDirectory <> '',
      'Could not create a probe directory for the changed sidecars');
    ProbeBase := IncludeTrailingPathDelimiter(ProbeDirectory) +
      ExtractFileName(FileName);
    ProbeStream := TMemoryStream.Create;
    ProbeStaged := TStringList.Create;
    ProbeFinal := TStringList.Create;
    Check(StoreToStream_TpXDocumentStaged(Drawing, ProbeStream, FileName,
      ProbeBase, False, ProbeStaged, ProbeFinal),
      'Could not stage the changed drawing sidecars for comparison');
    Check((ProbeStaged.Count = 2) and (ProbeFinal.Count = 2),
      'The changed drawing did not stage both sidecars');
    ExpectedEPSBytes := ReadDocumentTestBytes(ProbeStaged[0]);
    ExpectedPDFBytes := ReadDocumentTestBytes(ProbeStaged[1]);
    Check((ExpectedEPSBytes <> SavedEPSBytes) and
      (ExpectedPDFBytes <> SavedPDFBytes),
      'The conflicting edit did not produce new EPS and PDF contents');
    for I := 0 to ProbeStaged.Count - 1 do
      if FileExists(ProbeStaged[I]) then DeleteFile(ProbeStaged[I]);
    FreeAndNil(ProbeStaged);
    FreeAndNil(ProbeFinal);
    FreeAndNil(ProbeStream);
    Check(RemoveDocumentStagingDirectory(ProbeDirectory),
      'Could not remove the sidecar comparison staging directory');
    ProbeDirectory := '';

    ExternalBytes := SavedSourceBytes;
    Check(Length(ExternalBytes) > 0,
      'The accepted primary source was unexpectedly empty');
    if ExternalBytes[1] = '!' then ExternalBytes[1] := '#'
    else ExternalBytes[1] := '!';
    SavedDate := ReadDocumentTestDate(FileName);
    WriteDocumentTestBytes(FileName, ExternalBytes);
    Check(SetDocumentTestDate(FileName, SavedDate) = 0,
      'Could not restore the accepted primary timestamp');
    ExternalRevision := ReadDocumentRevision(FileName);
    Check((ExternalRevision.Size = SavedRevision.Size) and
      (ExternalRevision.Identity = SavedRevision.Identity) and
      (ExternalRevision.ContentDigest <> SavedRevision.ContentDigest),
      'The external primary edit did not preserve its revision metadata');
{$IFDEF UNIX}
    Check(ExternalRevision.ModifiedUTC = SavedRevision.ModifiedUTC,
      'The external primary edit did not preserve its exact timestamp');
{$ELSE}
    Check(ReadDocumentTestDate(FileName) = SavedDate,
      'The external primary edit did not preserve its visible timestamp');
{$ENDIF}
    Check(ReadDocumentTestBytes(FileName) = ExternalBytes,
      'The external primary edit did not retain its bytes');
    SavedEntryCount := CountDirectoryEntries(Directory);

    SaveFailed := BaseMode.SaveDocumentToPath(FileName, 'tpx', False, False,
      ErrorText);
    Check(not SaveFailed and (Pos('changed on disk', ErrorText) > 0),
      'The bundle save did not fail at the primary revision check: ' +
      ErrorText);
    Check((ReadDocumentTestBytes(FileName) = ExternalBytes) and
      (ReadDocumentTestBytes(EPSFileName) = SavedEPSBytes) and
      (ReadDocumentTestBytes(PDFFileName) = SavedPDFBytes),
      'A primary conflict failed to restore the old source and sidecar bytes');
    Check((Session.SourcePath = FileName) and
      (Session.SourceFormatId = 'tpx') and
      (Session.OriginalSourceBytes = SavedSourceBytes) and
      SameDiskRevision(Session.AcceptedRevision, SavedRevision) and
      (Session.LocalRevision = SavedLocalRevision) and
      (Session.RecoveryBackupPath = RecoveryBackup),
      'A failed bundle save changed the accepted source identity');
    Check((Drawing.FileName = FileName) and Drawing.History.IsChanged and
      (Drawing.ObjectsCount = 2),
      'A failed bundle save changed the live drawing or dirty state');
    Check(CountDirectoryEntries(Directory) = SavedEntryCount,
      'A failed bundle save left staging or rollback files behind');
{$IFDEF UNIX}
    Check(fpStat(PChar(FileName), Info) = 0,
      'Could not inspect the rolled-back source permissions');
    Check((Info.st_mode and $0FFF) = $0180,
      'The rolled-back source did not retain its 0600 permissions');
    Check(fpStat(PChar(EPSFileName), Info) = 0,
      'Could not inspect the rolled-back EPS permissions');
    Check((Info.st_mode and $0FFF) = $0180,
      'The rolled-back EPS sidecar did not retain its 0600 permissions');
    Check(fpStat(PChar(PDFFileName), Info) = 0,
      'Could not inspect the rolled-back PDF permissions');
    Check((Info.st_mode and $0FFF) = $0180,
      'The rolled-back PDF sidecar did not retain its 0600 permissions');
{$ENDIF}
  finally
    if ProbeDirectory <> '' then
    begin
      if ProbeStaged <> nil then
        for I := 0 to ProbeStaged.Count - 1 do
          if FileExists(ProbeStaged[I]) then DeleteFile(ProbeStaged[I]);
      RemoveDocumentStagingDirectory(ProbeDirectory);
    end;
    FreeAndNil(ProbeStaged);
    FreeAndNil(ProbeFinal);
    FreeAndNil(ProbeStream);
    if (Session.RecoveryBackupPath <> '') and
      FileExists(Session.RecoveryBackupPath) then
      DeleteFile(Session.RecoveryBackupPath);
    Drawing.Clear;
    Session.Clear;
    if FileExists(FileName) then DeleteFile(FileName);
    if FileExists(EPSFileName) then DeleteFile(EPSFileName);
    if FileExists(PDFFileName) then DeleteFile(PDFFileName);
    RemoveDir(Directory);
  end;
end;

procedure TestDocumentModeSaveRoutes;
var Directory, UnusedFileName, SourceFileName, ExportFileName: string;
  Drawing, Candidate: TDrawing2D;
  Session: TDocumentSession;
  Snapshot: TDocumentSnapshot;
  SavedBytes: RawByteString;
  SavedRevision: TDiskRevision;
  CodecContext: TObject;
  CanSaveBack, Saved: Boolean;
  ErrorText: string;
begin
  PrepareDocumentTestDirectory('issue62-mode-save', Directory,
    UnusedFileName);
  SourceFileName := IncludeTrailingPathDelimiter(Directory) + 'empty.tpx';
  ExportFileName := IncludeTrailingPathDelimiter(Directory) + 'drawing.svg';
  Drawing := MainForm.TheDrawing;
  Session := MainForm.EventManager.DocumentSession;
  Candidate := nil;
  CodecContext := nil;
  try
    Drawing.Clear;
    if Drawing.History = nil then
      Drawing.History := TDrawHistory.Create(Drawing);
    Drawing.History.Clear;
    Drawing.History.Save;
    Drawing.History.SaveCheckSum;
    Session.Clear;
    Drawing.FileName := SourceFileName;
    Drawing.History.MarkDirty;
    Saved := BaseMode.SaveDocumentToPath(SourceFileName, 'tpx', True,
      True, ErrorText);
    Check(Saved, 'The native save route failed for an empty drawing: ' +
      ErrorText);
    Check(FileExists(SourceFileName),
      'The native save route did not create an empty drawing file');
    Check((Drawing.ObjectsCount = 0) and not Drawing.History.IsChanged,
      'Saving an empty drawing did not clear its modified state');
    Check((Drawing.FileName = SourceFileName) and
      (Session.SourcePath = SourceFileName) and
      (Session.SourceFormatId = 'tpx'),
      'The native save route did not bind the saved source');
    Snapshot := ReadDocumentSnapshot(SourceFileName);
    Check((Snapshot.SourceBytes <> '') and
      SameDiskRevision(Session.AcceptedRevision, Snapshot.Revision) and
      (Session.OriginalSourceBytes = Snapshot.SourceBytes),
      'The native save route did not accept the exact saved revision');

    Candidate := LoadDocumentCandidate('tpx', SourceFileName,
      Snapshot.SourceBytes, CanSaveBack, CodecContext);
    Check(CanSaveBack and (Candidate.ObjectsCount = 0),
      'The saved empty drawing cannot be reopened as a native document');
    FreeAndNil(Candidate);
    FreeAndNil(CodecContext);

    Drawing.AddObject(-1,
      TLine2D.CreateSpec(-1, Point2D(0, 0), Point2D(7, 5)));
    Check(Drawing.History.IsChanged,
      'Editing after the native save did not make the drawing dirty');
    SavedBytes := Session.OriginalSourceBytes;
    SavedRevision := Session.AcceptedRevision;
    ExportToFile(Drawing, ExportFileName, export_SVG);
    Check(FileExists(ExportFileName),
      'The SVG export route did not create its output');
    Check(Drawing.History.IsChanged,
      'An export incorrectly cleared the native drawing modified state');
    Check((Drawing.FileName = SourceFileName) and
      (Session.SourcePath = SourceFileName) and
      (Session.SourceFormatId = 'tpx') and
      (Session.OriginalSourceBytes = SavedBytes) and
      SameDiskRevision(Session.AcceptedRevision, SavedRevision),
      'An export replaced the native drawing source identity');

    ReplyButton := idButtonCancel;
    PromptCount := 0;
    Observer.CloseRequests := 0;
    MainForm.Close;
    Check((PromptCount = 1) and (Observer.CloseRequests = 0),
      'An export satisfied close-save without asking to save the drawing');
    Check(Drawing.History.IsChanged,
      'Canceling close after an export cleared the modified state');
  finally
    FreeAndNil(Candidate);
    FreeAndNil(CodecContext);
    Drawing.Clear;
    Session.Clear;
    if FileExists(SourceFileName) then DeleteFile(SourceFileName);
    if FileExists(ExportFileName) then DeleteFile(ExportFileName);
    RemoveDir(Directory);
  end;
end;

procedure TestDocumentIO;
begin
  TestDocumentRevisionConflict;
  TestDocumentSavedRevision;
  TestDocumentUnixPermissions;
  TestMalformedDocumentCandidatePreservesLiveDrawing;
  TestDocumentCandidateCommitUsesCandidateOptions;
  TestDocumentBundleRollbackOnPrimaryConflict;
  TestDocumentModeSaveRoutes;
end;

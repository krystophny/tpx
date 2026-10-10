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
var Directory, FileName, Backup: string;
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
var Directory, FileName, Backup: string;
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
var Directory, FileName, Backup: string;
  Expected, NewRevision: TDiskRevision;
  Info: BaseUnix.Stat;
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
    finally
      if (Backup <> '') and FileExists(Backup) then DeleteFile(Backup);
    end;
  finally
    if FileExists(FileName) then DeleteFile(FileName);
    RemoveDir(Directory);
  end;
{$ENDIF}
end;

procedure TestDocumentIO;
begin
  TestDocumentRevisionConflict;
  TestDocumentSavedRevision;
  TestDocumentUnixPermissions;
end;

unit CoreAutoSaveStoreTests;

{$mode delphi}{$H+}

interface

implementation

uses SysUtils, Classes, AutoSaveStore, DocumentFormats, CoreTestSupport;

function UnicodePathChar(Codepoint: Word): TDocumentPath;
var Value: UnicodeString;
begin
  SetLength(Value, 1);
  Value[1] := WideChar(Codepoint);
  Result := UTF8Encode(Value);
  CheckCore((Length(Result) = 2) and (Byte(Result[1]) = $CE) and
    (Byte(Result[2]) = (Codepoint and $FF)),
    'Unicode path fixture must contain the expected UTF-8 bytes');
end;

function EmptyRevision: TDiskRevision;
begin
  Result.ContentDigest := '';
  Result.Size := 0;
  Result.ModifiedUTC := 0;
  Result.Identity := '';
  Result.Exists := False;
end;

function Revision(const Digest: string; Size, ModifiedUTC: Int64;
  const Identity: string): TDiskRevision;
begin
  Result.ContentDigest := Digest;
  Result.Size := Size;
  Result.ModifiedUTC := ModifiedUTC;
  Result.Identity := Identity;
  Result.Exists := True;
end;

function NewScratchDirectory: string;
var
  ID: TGUID;
  Token: string;
begin
  CheckCore(CreateGUID(ID) = 0, 'could not create scratch directory id');
  Token := StringReplace(StringReplace(GUIDToString(ID), '{', '', []), '}', '', []);
  Result := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'tpx-autosave-test-' + Token;
  CheckCore(ForceDirectories(Result), 'could not create scratch directory');
end;

procedure RemoveFlatDirectory(const DirectoryName: string);
var
  Search: TSearchRec;
begin
  if not DirectoryExists(DirectoryName) then Exit;
  if FindFirst(IncludeTrailingPathDelimiter(DirectoryName) + '*',
    faAnyFile, Search) = 0 then
  try
    repeat
      if (Search.Name = '.') or (Search.Name = '..') then Continue;
      if (Search.Attr and faDirectory) = 0 then
        DeleteFile(IncludeTrailingPathDelimiter(DirectoryName) + Search.Name);
    until FindNext(Search) <> 0;
  finally
    FindClose(Search);
  end;
  RemoveDir(DirectoryName);
end;

procedure TestDraftAndPriorVersionRoundTrip;
var
  Root: string;
  Store: TAutoSaveStore;
  Files: TStringList;
  Entry: TAutoSaveStoredRecord;
  VersionFile: string;
  BaseRevision: TDiskRevision;
begin
  Root := NewScratchDirectory;
  Files := TStringList.Create;
  Store := TAutoSaveStore.Create(Root);
  try
    BaseRevision := EmptyRevision;
    Store.SaveDraft('untitled_1', '', '', 'tpx', BaseRevision, '', 0, '');
    Store.ListDrafts(Files);
    CheckCore((Files.Count = 1) and Store.LoadRecord(Files[0], Entry),
      'an empty untitled draft should be recoverable');
    CheckCore((Entry.Kind = asrDraft) and (Entry.Payload = '') and
      (Entry.PayloadFormatId = 'tpx'),
      'empty draft metadata and native payload format should round-trip');

    BaseRevision := Revision('digest-r1', 101, 2001, 'inode-r1');
    Store.SaveDraft('untitled_1', '/tmp/import.svg', 'svg', 'tpx',
      BaseRevision, 'source-baseline-bytes', 9, 'editable-scene-bytes');
    Store.ListDrafts(Files);
    CheckCore((Files.Count = 1) and Store.LoadRecord(Files[0], Entry),
      'a draft update should atomically replace its earlier revision');
    CheckCore((Entry.Payload = 'editable-scene-bytes') and
      (Entry.SourceFormatId = 'svg') and (Entry.PayloadFormatId = 'tpx') and
      SameDiskRevision(Entry.BaseRevision, BaseRevision) and
      (Entry.BaseSourceBytes = 'source-baseline-bytes') and
      (Entry.LocalRevision = 9),
      'recovery payload must be separate from untrusted source format/identity');

    BaseRevision := Revision('digest-r7', 700, 2007, 'inode-r7');
    VersionFile := Store.SavePriorVersion('sourcekey', '/tmp/base.tpx',
      'tpx', BaseRevision, 9, 'exact original source bytes');
    CheckCore(Store.LoadRecord(VersionFile, Entry),
      'pre-publication source snapshot should be readable');
    CheckCore((Entry.Kind = asrPriorVersion) and
      (Entry.Payload = 'exact original source bytes') and
      SameDiskRevision(Entry.BaseRevision, BaseRevision),
      'prior-version record should preserve exact source bytes and revision');
  finally
    Store.Free;
  end;
  Store := TAutoSaveStore.Create(Root);
  try
    Store.ListDrafts(Files);
    CheckCore((Files.Count = 1) and Store.LoadRecord(Files[0], Entry) and
      (Entry.LocalRevision = 9) and
      SameDiskRevision(Entry.BaseRevision,
        Revision('digest-r1', 101, 2001, 'inode-r1')) and
      (Entry.BaseSourceBytes = 'source-baseline-bytes'),
      'draft and exact reload baseline should survive a store restart');
    Store.DeleteDraft('untitled_1');
    Store.ListDrafts(Files);
    CheckCore(Files.Count = 0, 'accepted recovery should be explicitly removable');
  finally
    Store.Free;
    Files.Free;
    RemoveFlatDirectory(Root);
  end;
end;

procedure TestRetentionKeepsLatestAndProtectsDrafts;
var
  Root, CapRoot: string;
  Store: TAutoSaveStore;
  Files, Seen: TStringList;
  Entry: TAutoSaveStoredRecord;
  I: Integer;
  FoundLatest, Rejected: Boolean;
begin
  Root := NewScratchDirectory;
  CapRoot := IncludeTrailingPathDelimiter(Root) + 'cap';
  CheckCore(ForceDirectories(CapRoot), 'could not create cap-test directory');
  Store := TAutoSaveStore.Create(Root);
  Files := TStringList.Create;
  Seen := TStringList.Create;
  try
    for I := 1 to AutoSaveMaxRetainedRecords + 1 do
      Store.SavePriorVersion('history', '/tmp/history.tpx', 'tpx',
        EmptyRevision, I, 'version-' + IntToStr(I));
    Store.ListPriorVersions('history', Files);
    CheckCore(Files.Count <= AutoSaveMaxRetainedRecords,
      'prior versions must stay within the configured record limit');
    FoundLatest := False;
    for I := 0 to Files.Count - 1 do
      if Store.LoadRecord(Files[I], Entry) and
        (Entry.LocalRevision = AutoSaveMaxRetainedRecords + 1) and
        (Entry.Payload = 'version-' + IntToStr(AutoSaveMaxRetainedRecords + 1)) then
        FoundLatest := True;
    CheckCore(FoundLatest,
      'retention must protect the newly committed recoverable version');

    FreeAndNil(Store);
    Store := TAutoSaveStore.Create(CapRoot);
    for I := 1 to AutoSaveMaxRetainedRecords do
      Store.SaveDraft('draft_' + IntToStr(I), '', '', 'tpx', EmptyRevision,
        '', I, 'draft-' + IntToStr(I));
    Rejected := False;
    try
      Store.SaveDraft('draft_excess', '', '', 'tpx', EmptyRevision, '', 99,
        'excess');
    except
      on E: Exception do Rejected := True;
    end;
    CheckCore(Rejected, 'a full draft-only store must reject another record');
    Store.ListDrafts(Files);
    CheckCore(Files.Count = AutoSaveMaxRetainedRecords,
      'a failed capacity check must not discard any existing recovery drafts');
    Seen.Clear;
    for I := 0 to Files.Count - 1 do begin
      CheckCore(Store.LoadRecord(Files[I], Entry),
        'every retained draft should remain readable after capacity failure');
      Seen.Add(Entry.DocumentKey);
    end;
    for I := 1 to AutoSaveMaxRetainedRecords do
      CheckCore(Seen.IndexOf('draft_' + IntToStr(I)) >= 0,
        'capacity failure must preserve each prior draft');
  finally
    Store.Free;
    Files.Free;
    Seen.Free;
    RemoveFlatDirectory(CapRoot);
    RemoveFlatDirectory(Root);
  end;
end;

procedure TestUnicodeDirectoryAndSourcePath;
var
  Root, SourcePath: TDocumentPath;
  ID: TGUID;
  Token: string;
  Store: TAutoSaveStore;
  Files: TStringList;
  Entry: TAutoSaveStoredRecord;
begin
  CheckCore(CreateGUID(ID) = 0, 'could not create Unicode-path test id');
  Token := StringReplace(StringReplace(GUIDToString(ID), '{', '', []), '}', '', []);
  Root := TDocumentPath(UTF8String(GetCurrentDir)) + PathDelim + 'tpx-autosave-' +
    UnicodePathChar($03B1) + '-' + Token;
  SourcePath := Root + PathDelim + 'source-' + UnicodePathChar($03B2) + '.tpx';
  CheckCore(UTF8Encode(UTF8Decode(Root)) = Root,
    'the Unicode config directory must round-trip as UTF-8');
  Store := TAutoSaveStore.Create(Root);
  Files := TStringList.Create;
  try
    Store.SaveDraft('unicode_path', SourcePath, 'tpx', 'tpx', EmptyRevision,
      'baseline', 11, 'draft');
    Store.ListDrafts(Files);
    CheckCore((Files.Count = 1) and Store.LoadRecord(Files[0], Entry),
      'a record under a Unicode directory should survive list and reload');
    CheckCore((Entry.SourcePath = SourcePath) and
      (UTF8Encode(UTF8Decode(Entry.SourcePath)) = SourcePath),
      'the source identity inside a record must preserve non-ACP UTF-8');
    Store.DeleteDraft('unicode_path');
  finally
    Files.Free;
    Store.Free;
    CheckCore(RemoveDocumentStagingDirectory(Root),
      'the Unicode recovery directory should be removable after cleanup');
  end;
end;

initialization
  RegisterCoreTest('autosave-store-roundtrip-restart',
    @TestDraftAndPriorVersionRoundTrip);
  RegisterCoreTest('autosave-store-retention-failure',
    @TestRetentionKeepsLatestAndProtectsDrafts);
  RegisterCoreTest('autosave-store-unicode-paths',
    @TestUnicodeDirectoryAndSourcePath);

end.

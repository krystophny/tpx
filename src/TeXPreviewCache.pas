unit TeXPreviewCache;

{$mode objfpc}{$H+}

interface

uses Classes;

type
  TTeXPreviewCache = class
  private
    FDirectory, FLockName: string;
    FLock: TFileStream;
  public
    constructor Create(const Root: string);
    destructor Destroy; override;
    property Directory: string read FDirectory;
  end;

implementation

uses SysUtils, FileUtil
  {$IFDEF UNIX}, Unix{$ENDIF};

const
  CacheMarker = 'TpX live TeX cache v1';

function LockCache(Stream: TFileStream): Boolean;
begin
  {$IFDEF UNIX}
  // Do not assume Pascal sharing flags enforce a lock on every Unix target.
  Result := fpFlock(Stream.Handle, LOCK_EX or LOCK_NB) = 0;
  {$ELSE}
  // Windows enforces the stream's fmShareExclusive open in the OS.
  Result := True;
  {$ENDIF}
end;

procedure CleanAbandonedCaches(const Root: string);
var
  Search: TSearchRec;
  ID: TGUID;
  Name, Directory, Marker: string;
  Stream: TFileStream;
  RemoveLock: Boolean;
begin
  if FindFirst(Root + '*.lock', faAnyFile, Search) <> 0 then Exit;
  try
    repeat
      Name := ChangeFileExt(Search.Name, '');
      if (Search.Attr and faDirectory <> 0) or not TryStringToGUID(Name, ID) then Continue;
      Stream := nil;
      RemoveLock := False;
      try
        try
          Stream := TFileStream.Create(Root + Search.Name, fmOpenReadWrite or fmShareExclusive);
          if not LockCache(Stream) or (Stream.Size <> Length(CacheMarker)) then Continue;
          SetLength(Marker, Length(CacheMarker));
          Stream.ReadBuffer(Marker[1], Length(Marker));
          if Marker <> CacheMarker then Continue;
          Directory := Root + Name;
          RemoveLock := not DirectoryExists(Directory) or DeleteDirectory(Directory, False);
        except
          // A running instance, unreadable file or unknown cache is left alone.
        end;
      finally
        Stream.Free;
      end;
      if RemoveLock then DeleteFile(Root + Search.Name);
    until FindNext(Search) <> 0;
  finally
    FindClose(Search);
  end;
end;

constructor TTeXPreviewCache.Create(const Root: string);
var
  Base: string;
  ID: TGUID;
begin
  inherited Create;
  Base := IncludeTrailingPathDelimiter(ExpandFileName(Root));
  if not ForceDirectories(Base) then raise Exception.Create('Cannot create LaTeX preview cache');
  CleanAbandonedCaches(Base);
  CreateGUID(ID);
  FDirectory := Base + GUIDToString(ID);
  FLockName := FDirectory + '.lock';
  // Acquire the sibling lock before creating the directory. Publish the marker
  // only after locking, so another starting instance cannot collect this cache.
  FLock := TFileStream.Create(FLockName, fmCreate);
  if LockCache(FLock) then FLock.WriteBuffer(CacheMarker[1], Length(CacheMarker));
  if not ForceDirectories(FDirectory) then raise Exception.Create('Cannot create LaTeX preview cache');
end;

destructor TTeXPreviewCache.Destroy;
var
  Removed: Boolean;
begin
  Removed := (FDirectory = '') or not DirectoryExists(FDirectory) or
    DeleteDirectory(FDirectory, False);
  FLock.Free;
  if Removed and (FLockName <> '') then DeleteFile(FLockName);
  inherited Destroy;
end;

end.

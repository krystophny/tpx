unit BrowserDocumentStatus;

{$IFDEF FPC}{$MODE DELPHI}{$H+}{$ENDIF}

interface

uses DocumentFormats;

type
  TBrowserDocumentLifecycleEvent = procedure of object;

procedure SetBrowserDocumentDirty(Value: Boolean);
procedure SetBrowserDocumentSource(Value: Boolean);
procedure SetBrowserDocumentError(const MessageText: string);
procedure ResetBrowserDocumentStatus;
procedure SetBrowserDocumentLifecycleCallbacks(const OnEdited, OnReplaced,
  OnDownloadRequested: TBrowserDocumentLifecycleEvent);
procedure BrowserDocumentDidChange(IsDirty: Boolean);
procedure ReplaceBrowserDocument(HasSource: Boolean);
procedure BrowserDocumentDownloadRequested;
function BeginBrowserDownloadBundle(const MainPath: TDocumentPath): Boolean;
function AddBrowserDownloadPath(const Path: TDocumentPath): Boolean;
function FinishBrowserDownloadBundle(Success: Boolean): Boolean;

implementation

{$IFDEF CPUWASM32}
uses Classes, SysUtils;
{$ENDIF}

var
  BrowserDocumentEditedHandler: TBrowserDocumentLifecycleEvent = nil;
  BrowserDocumentReplacedHandler: TBrowserDocumentLifecycleEvent = nil;
  BrowserDocumentDownloadHandler: TBrowserDocumentLifecycleEvent = nil;

{$IFDEF CPUWASM32}
procedure BrowserSetDocumentDirty(Value: LongInt);
  external 'tpx' name 'browser_document_dirty';
procedure BrowserSetDocumentSource(Value: LongInt);
  external 'tpx' name 'browser_document_source';
procedure BrowserSetDocumentError(MessageText: PChar; MessageLength: LongInt);
  external 'tpx' name 'browser_document_error';
procedure BrowserResetDocumentStatus;
  external 'tpx' name 'browser_document_reset';
function BrowserBeginDownloadBundle(Path: PChar; PathLength: LongInt): LongInt;
  external 'tpx' name 'browser_download_begin';
function BrowserAddDownloadPath(Path: PChar; PathLength: LongInt): LongInt;
  external 'tpx' name 'browser_download_add';
function BrowserFinishDownloadBundle(Success: LongInt): LongInt;
  external 'tpx' name 'browser_download_finish';
function BrowserDownloadFile(Path: PChar; PathLength: LongInt; Data: Pointer;
  DataLength: LongInt): LongInt;
  external 'tpx' name 'browser_download_file';
var
  DownloadBundleActive: Boolean;
  DownloadBundlePaths: TStringList;
{$ENDIF}

procedure SetBrowserDocumentDirty(Value: Boolean);
begin
{$IFDEF CPUWASM32}
  BrowserSetDocumentDirty(Ord(Value));
{$ENDIF}
end;

procedure SetBrowserDocumentSource(Value: Boolean);
begin
{$IFDEF CPUWASM32}
  BrowserSetDocumentSource(Ord(Value));
{$ENDIF}
end;

procedure SetBrowserDocumentError(const MessageText: string);
begin
{$IFDEF CPUWASM32}
  BrowserSetDocumentError(PChar(MessageText), Length(MessageText));
{$ENDIF}
end;

procedure ResetBrowserDocumentStatus;
begin
{$IFDEF CPUWASM32}
  BrowserResetDocumentStatus;
{$ENDIF}
end;

procedure SetBrowserDocumentLifecycleCallbacks(const OnEdited, OnReplaced,
  OnDownloadRequested: TBrowserDocumentLifecycleEvent);
begin
  BrowserDocumentEditedHandler := OnEdited;
  BrowserDocumentReplacedHandler := OnReplaced;
  BrowserDocumentDownloadHandler := OnDownloadRequested;
end;

procedure BrowserDocumentDidChange(IsDirty: Boolean);
begin
  SetBrowserDocumentDirty(IsDirty);
  if Assigned(BrowserDocumentEditedHandler) then
    BrowserDocumentEditedHandler;
end;

procedure ReplaceBrowserDocument(HasSource: Boolean);
begin
  ResetBrowserDocumentStatus;
  SetBrowserDocumentSource(HasSource);
  SetBrowserDocumentDirty(False);
  if Assigned(BrowserDocumentReplacedHandler) then
    BrowserDocumentReplacedHandler;
end;

procedure BrowserDocumentDownloadRequested;
begin
  if Assigned(BrowserDocumentDownloadHandler) then
    BrowserDocumentDownloadHandler;
end;

function BeginBrowserDownloadBundle(const MainPath: TDocumentPath): Boolean;
begin
{$IFDEF CPUWASM32}
  Result := not DownloadBundleActive and
    (BrowserBeginDownloadBundle(PChar(MainPath), Length(MainPath)) <> 0);
  DownloadBundleActive := Result;
  if Result then
  begin
    if DownloadBundlePaths = nil then
      DownloadBundlePaths := TStringList.Create;
    DownloadBundlePaths.Clear;
    DownloadBundlePaths.Add(string(MainPath));
  end;
{$ELSE}
  Result := True;
{$ENDIF}
end;

function AddBrowserDownloadPath(const Path: TDocumentPath): Boolean;
begin
{$IFDEF CPUWASM32}
  Result := DownloadBundleActive and
    (BrowserAddDownloadPath(PChar(Path), Length(Path)) <> 0);
  if Result then
    DownloadBundlePaths.Add(string(Path));
{$ELSE}
  Result := True;
{$ENDIF}
end;

{$IFDEF CPUWASM32}
function SendDownloadBundleFiles: Boolean;
var
  I: Integer;
  Stream: TFileStream;
  Data: TBytes;
  Path: string;
begin
  Result := False;
  for I := 0 to DownloadBundlePaths.Count - 1 do
  begin
    Path := DownloadBundlePaths[I];
    try
      Stream := TFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
      try
        SetLength(Data, Stream.Size);
        if Length(Data) > 0 then
          Stream.ReadBuffer(Data[0], Length(Data));
      finally
        Stream.Free;
      end;
    except
      Exit;
    end;
    if BrowserDownloadFile(PChar(Path), Length(Path), Pointer(Data),
      Length(Data)) = 0 then
      Exit;
  end;
  Result := True;
end;
{$ENDIF}

function FinishBrowserDownloadBundle(Success: Boolean): Boolean;
begin
{$IFDEF CPUWASM32}
  if not DownloadBundleActive then Exit(False);
  DownloadBundleActive := False;
  if Success then
    Success := SendDownloadBundleFiles;
  Result := BrowserFinishDownloadBundle(Ord(Success)) <> 0;
{$ELSE}
  Result := Success;
{$ENDIF}
end;

end.

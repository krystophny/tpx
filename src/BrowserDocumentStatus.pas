unit BrowserDocumentStatus;

{$IFDEF FPC}{$MODE DELPHI}{$H+}{$ENDIF}

interface

uses DocumentFormats;

procedure SetBrowserDocumentDirty(Value: Boolean);
procedure SetBrowserDocumentSource(Value: Boolean);
procedure SetBrowserDocumentError(const MessageText: string);
function BeginBrowserDownloadBundle(const MainPath: TDocumentPath): Boolean;
function AddBrowserDownloadPath(const Path: TDocumentPath): Boolean;
function FinishBrowserDownloadBundle(Success: Boolean): Boolean;

implementation

{$IFDEF CPUWASM32}
procedure BrowserSetDocumentDirty(Value: LongInt);
  external 'tpx' name 'browser_document_dirty';
procedure BrowserSetDocumentSource(Value: LongInt);
  external 'tpx' name 'browser_document_source';
procedure BrowserSetDocumentError(MessageText: PChar; MessageLength: LongInt);
  external 'tpx' name 'browser_document_error';
function BrowserBeginDownloadBundle(Path: PChar; PathLength: LongInt): LongInt;
  external 'tpx' name 'browser_download_begin';
function BrowserAddDownloadPath(Path: PChar; PathLength: LongInt): LongInt;
  external 'tpx' name 'browser_download_add';
function BrowserFinishDownloadBundle(Success: LongInt): LongInt;
  external 'tpx' name 'browser_download_finish';
var
  DownloadBundleActive: Boolean;
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

function BeginBrowserDownloadBundle(const MainPath: TDocumentPath): Boolean;
begin
{$IFDEF CPUWASM32}
  Result := not DownloadBundleActive and
    (BrowserBeginDownloadBundle(PChar(MainPath), Length(MainPath)) <> 0);
  DownloadBundleActive := Result;
{$ELSE}
  Result := True;
{$ENDIF}
end;

function AddBrowserDownloadPath(const Path: TDocumentPath): Boolean;
begin
{$IFDEF CPUWASM32}
  Result := DownloadBundleActive and
    (BrowserAddDownloadPath(PChar(Path), Length(Path)) <> 0);
{$ELSE}
  Result := True;
{$ENDIF}
end;

function FinishBrowserDownloadBundle(Success: Boolean): Boolean;
begin
{$IFDEF CPUWASM32}
  if not DownloadBundleActive then Exit(False);
  DownloadBundleActive := False;
  Result := BrowserFinishDownloadBundle(Ord(Success)) <> 0;
{$ELSE}
  Result := Success;
{$ENDIF}
end;

end.

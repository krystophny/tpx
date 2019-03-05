{
=========================================
 Авторские права (c) 2002 Михаил Власов.
=========================================
 Modified by Alexander Tsyplakov, 2007 (TSY)
}

unit XUtils;

interface

uses
	Classes;

type
	TXList = class(TList)
	protected
		procedure ItemInserted(anItem: Pointer); virtual;
		procedure ItemRemoved(anItem: Pointer); virtual;
		procedure ClearItem(anItem: TObject); virtual;
	public
		destructor Destroy; override;

		function Add(anItem: Pointer): Integer;
		procedure Insert(anIndex: Integer; anItem: Pointer);
		function Remove(anItem: Pointer): Integer;
		function Delete(anIndex: Integer): TObject;
		procedure Clear; override;
		procedure RemoveAll;
	end;

	TRefObject = class(TObject)
	private
		FRefCount: Integer;
	protected
		procedure OnFree; virtual;
	public
		procedure AddRef;
		procedure Release;
		procedure Free(YourMustNotToCallThisMethodDirectly: Integer);

		property RefCount: Integer read FRefCount;
	end;

	TRefObjectList = class(TXList)
	protected
		procedure ItemInserted(anItem: Pointer); override;
		procedure ItemRemoved(anItem: Pointer); override;
		procedure ClearItem(anItem: TObject); override;
	public
		procedure FreeItems;
	end;
	
	TTextReader = class
	private
		Stream: TStream;
		Buffer: PChar;
		BufSize: Word;
		BufEnd: PChar;
		BufPos: PChar;

		procedure GotoNextChar;
		function GetPos: Longint;
		procedure SetPos(aPos: Longint);

	public
		ConvertOemToChar: Boolean;
		property Pos: Longint read GetPos write SetPos;

		constructor Create(aStream: TStream; aBufSize: Word);
		destructor Destroy; override;

		function ReadLine: string;
		procedure ReadLineToBuf(aBuf: PChar; aBufSize: Integer);
		function ReadChar: Char;
		function Eof: Boolean;
	end;

{Кодирует спец. символы для HTML.[brd]}
function EncodeHtmlString(const s: String): String;
{Разкодирует спец. символы из HTML.[brd]}
function DecodeHtmlString(const s: String): String;

function RO_Replace(var aDst{ : TRefObject}; aSrc: TRefObject): Boolean;
function RO_Init(var aDst{ : TRefObject}; aSrc: TRefObject): Boolean;
procedure RO_Release(var aDst{ : TRefObject});
procedure RO_Free(var aDst{ : TRefObject});

implementation

uses
	Math,
{$IFNDEF FPC}
{$IFNDEF NEXTGEN}
  Windows,
{$ENDIF NEXTGEN}
{$ELSE}
  LCLIntf,
{$ENDIF}
 SysUtils;
	
function EncodeHtmlString(const s: String): String;
begin
	Result := s;
	Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
	Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
	Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
	Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
	Result := StringReplace(Result, '''', '&apos;', [rfReplaceAll]);
end;

function DecodeHtmlString(const s: String): String;
begin
	Result := s;
	Result := StringReplace(Result, '&apos;', '''', [rfReplaceAll]);
	Result := StringReplace(Result, '&quot;', '"', [rfReplaceAll]);
	Result := StringReplace(Result, '&gt;', '>', [rfReplaceAll]);
	Result := StringReplace(Result, '&lt;', '<', [rfReplaceAll]);
	Result := StringReplace(Result, '&amp;', '&', [rfReplaceAll]);
end;

function RO_Replace(var aDst{ : TRefObject}; aSrc: TRefObject): Boolean;
begin
	if TRefObject(aDst) <> aSrc then begin
		TRefObject(aDst).Release;
		TRefObject(aDst) := aSrc;
		if Assigned(aSrc) then
			aSrc.AddRef;
		Result := True
	end
	else
		Result := False;
end;

function RO_Init(var aDst{ : TRefObject}; aSrc: TRefObject): Boolean;
begin
	TRefObject(aDst) := aSrc;
	Result := Assigned(aSrc);
	if Result then
		aSrc.AddRef;
end;

procedure RO_Release(var aDst{ : TRefObject});
begin
	if Assigned(TRefObject(aDst)) then begin
		TRefObject(aDst).Release;
		TRefObject(aDst) := nil;
	end;
end;

procedure RO_Free(var aDst{ : TRefObject});
var
	aDstRO: TRefObject absolute aDst;
begin
	if Assigned(aDstRO) then begin
		aDstRO.AddRef;
		try
			aDstRO.OnFree;
		finally
			aDstRO.Release
		end;
		aDstRO.Release;
		aDstRO := nil;
	end;
end;

{	TXList	}

destructor TXList.Destroy;
begin
	Clear;
	inherited Destroy;
end;

procedure TXList.ItemInserted(anItem: Pointer);
begin
end;

procedure TXList.ItemRemoved(anItem: Pointer);
begin
end;

function TXList.Add(anItem: Pointer): Integer;
begin
	Result := inherited Add(anItem);
	if Assigned(anItem) then
		ItemInserted(anItem);
end;

procedure TXList.Insert(anIndex: Integer; anItem: Pointer);
begin
	inherited Insert(anIndex, anItem);
	if Assigned(anItem) then
		ItemInserted(anItem);
end;

function TXList.Remove(anItem: Pointer): Integer;
begin
	Result := inherited Remove(anItem);
	if (Result <> -1) and Assigned(anItem)  then
		ItemRemoved(anItem);
end;

function TXList.Delete(anIndex: Integer): TObject;
begin
	Result := TObject(Items[anIndex]);
	inherited Delete(anIndex);
	if Assigned(Result) then
		ItemRemoved(Result);
end;

procedure TXList.Clear;
var
	i: Integer;
	anItem: TObject;
begin
	i := Count - 1;
	while i >= 0 do begin
		anItem := TObject(Items[i]);
		inherited Delete(i);
		if Assigned(anItem) then begin
			ItemRemoved(anItem);
			ClearItem(anItem);
		end;
		i := Count - 1;
	end;
	inherited Clear;
end;

procedure TXList.ClearItem(anItem: TObject);
begin
	anItem.Free;
end;

procedure TXList.RemoveAll;
var
	i: Integer;
begin
	i := Count - 1;
	while i >= 0 do begin
		Delete(i);
		i := Count - 1;
	end;
	inherited Clear;
end;

{ TRefObject }

procedure TRefObject.Free(YourMustNotToCallThisMethodDirectly: Integer);
begin
	raise Exception.CreateFmt(
		'Внутренняя ошибка: прямое уничтожение ссылочного объекта [%s]', [ClassName]);
end;

procedure TRefObject.AddRef;
begin
	Inc(FRefCount);
end;

procedure TRefObject.Release;
begin
	if Assigned(Self) then begin
		Dec(FRefCount);
		if FRefCount = 0 then begin
			FRefCount := 1;
			Destroy;
		end;
	end
end;

procedure TRefObject.OnFree;
begin
end;

{ TRefObjectList }

procedure TRefObjectList.ItemInserted(anItem: Pointer);
begin
	TRefObject(anItem).AddRef;
end;

procedure TRefObjectList.ItemRemoved(anItem: Pointer);
begin
	TRefObject(anItem).Release;
end;

procedure TRefObjectList.ClearItem(anItem: TObject);
begin
end;

procedure TRefObjectList.FreeItems;
var
	i: Integer;
	anItem: TRefObject;
begin
	for i := 0 to Count - 1 do begin
		anItem := TRefObject(inherited Items[i]);
		if Assigned(anItem) then begin
			inherited Items[i] := nil;
			RO_Free(anItem);
		end;
	end;
	inherited Clear;
end;

{	TTextReader }

constructor TTextReader.Create(aStream: TStream; aBufSize: Word);
begin
  inherited Create;
  Stream := aStream;
  BufSize := aBufSize;
  Buffer := AllocMem(aBufSize);
  BufEnd := Buffer + 1;
  BufPos := Buffer;
  GotoNextChar;
end;

destructor TTextReader.Destroy;
begin
	if Assigned(Buffer) then
    FreeMem(Buffer, BufSize);
  inherited Destroy;
end;

procedure TTextReader.GotoNextChar;
begin
  Inc(BufPos);
  if BufPos = BufEnd then begin
    BufEnd := Buffer + Min(BufSize, Stream.Size - Stream.Position);
    Stream.ReadBuffer(Buffer^, BufEnd - Buffer);
    BufPos := Buffer;
  end;
end;

function TTextReader.GetPos: Longint;
begin
	Result := Stream.Position - Longint(BufEnd - BufPos);
end;

procedure TTextReader.SetPos(aPos: Longint);
begin
	if aPos <> GetPos then begin
		Stream.Seek(aPos, soFromBeginning);
		BufEnd := Buffer + 1;
		BufPos := Buffer;
		GotoNextChar;
	end;
end;

function TTextReader.ReadLine: string;
var
  aChar: Char;
begin
  Result := '';
  while not Eof do begin
    aChar := BufPos^;
    if aChar in [^M,^J] then begin
      GotoNextChar;
      if not Eof and (BufPos^ in [^M,^J]) and (BufPos^ <> aChar) then
        GotoNextChar;
      Break;
    end;
    SetLength(Result, Length(Result) + 1);
		Result[Length(Result)] := aChar;
    GotoNextChar;
	end;
{$IFDEF VER140}
	if ConvertOemToChar and (Result <> '') then
		OemToCharBuff(PChar(Result), PChar(Result), Length(Result));
{$ENDIF}
end;

procedure TTextReader.ReadLineToBuf(aBuf: PChar; aBufSize: Integer);
var
	aChar: Char;
	aSaveBuf: PChar;
begin
	aSaveBuf := aBuf;
  while (not Eof) and (aBufSize > 0) do begin
    aChar := BufPos^;
    if aChar in [^M,^J] then begin
      GotoNextChar;
      if not Eof and (BufPos^ in [^M,^J]) and (BufPos^ <> aChar) then
        GotoNextChar;
      Break;
    end;
    aBuf^ := aChar;
    Inc(aBuf);
    Dec(aBufSize);
    GotoNextChar;
  end;
	aBuf^ := #0;
{$IFDEF VER140}
	if ConvertOemToChar then
		OemToCharBuff(aSaveBuf, aSaveBuf, aBuf - aSaveBuf);
{$ENDIF}
end;

function TTextReader.ReadChar: Char;
begin
	if not Eof then begin
		Result := BufPos^;
		GotoNextChar;
{$IFDEF VER140}
		if ConvertOemToChar then
			OemToCharBuff(@Result, @Result, 1);
{$ENDIF}
	end
	else
		Result := #0;
end;

function TTextReader.Eof: Boolean;
begin
	Result := BufPos = BufEnd;
end;

end.
 

{
===========================================
 Авторские права (c) 2002 Равиль Батыршин.
===========================================
}

unit XXml;

interface

uses
	SysUtils, Classes, XUtils;

type
	//Inner-used class
	TXmlReaderStream = class
	private
		FreeStream: Boolean;
		Stm: TStream;
		Reader: TTextReader;
		BackBuffer: String;

		function GetEof: Boolean;
	public
		constructor Create(aStm: TStream; aFreeStream: Boolean);
		destructor Destroy; override;

		function ReadChar(aRemoveChar: Boolean = True): Char;
		function Expect(const s: String; aRemoveChar: Boolean = True): Boolean;
		procedure SkipBlanks;
		procedure PutBack(c: Char); overload;
		procedure PutBack(const s: String); overload;

		property Eof: Boolean read GetEof;
	end;

	TXmlReaderTokenType = (
		xrtProcessingInstruction, //Set Name, Value
		xrtDocumentType, //Set Name, Value
		xrtComment, //Set Value+
		xrtCData, //Set Value+
		xrtElementBegin, //Set Name
		xrtElementEnd, //Set Name
		xrtElementAttribute, //Set Name, Value
		xrtText, //Set Value+
		xrtEof
	);

	TXmlReaderState = (
		xrsScan,
		xrsReadText,
		xrsReadElement,
		xrsReadComment,
		xrsReadCData
	);

	EXmlReader = class(Exception)
	end;

	TXmlReader = class
	private
		FPortionSize: Integer;
		FTokenType: TXmlReaderTokenType;
		FName: String;
		FValue: String;
		FBeginOfValue: Boolean;
		FEndOfValue: Boolean;
		FEndOfXml: Boolean;
		FElements: TStringList;

		State: TXmlReaderState;
		Stm: TXmlReaderStream;
		Buffer: PChar;

		function GetElementLevel: Integer;
		function GetElementPath: String;
	public
		constructor Create;
		destructor Destroy; override;

		procedure OpenXml(const aXml: String);
		procedure Open(const aFileName: String); overload;
		procedure Open(aStm: TStream; aFreeStream: Boolean = False); overload;
		procedure Close;

		function ReadNextToken: TXmlReaderTokenType;

		property PortionSize: Integer read FPortionSize write FPortionSize default 0;
		property TokenType: TXmlReaderTokenType read FTokenType;
		property Name: String read FName;
		property Value: String read FValue;
		property BeginOfValue: Boolean read FBeginOfValue;
		property EndOfValue: Boolean read FEndOfValue;
		property EndOfXml: Boolean read FEndOfXml;
		property Elements: TStringList read FElements;
		property ElementLevel: Integer read GetElementLevel;
		property ElementPath: String read GetElementPath;
	end;

implementation

const
	kBufferSize = 8192;

{ TXmlReaderStream }

constructor TXmlReaderStream.Create(aStm: TStream; aFreeStream: Boolean);
begin
	inherited Create;
	FreeStream := aFreeStream;
	Stm := aStm;
	Reader := TTextReader.Create(Stm, 4096);
	BackBuffer := '';
end;

destructor TXmlReaderStream.Destroy;
begin
	Reader.Free;
	if FreeStream then
		Stm.Free;
	inherited Destroy;
end;

function TXmlReaderStream.GetEof: Boolean;
begin
	Result := (BackBuffer = '') and Reader.Eof;
end;

function TXmlReaderStream.ReadChar(aRemoveChar: Boolean): Char;
var
	aLen: Integer;
begin
	aLen := Length(BackBuffer);
	if aLen <> 0 then begin
		Result := BackBuffer[aLen];
		if aRemoveChar then
			SetLength(BackBuffer, aLen - 1);
	end
	else if Reader.Eof then
		Result := #0
	else begin
		Result := Reader.ReadChar;
		if not aRemoveChar then
			PutBack(Result);
	end;
end;

function TXmlReaderStream.Expect(const s: String; aRemoveChar: Boolean): Boolean;
var
	c: Char;
	i: Integer;
begin
	Result := True;
	for i := 1 to Length(s) do begin
		if Eof then begin
			PutBack(Copy(s, 1, i - 1));
			Result := False;
			break;
		end;
		c := ReadChar;
		if c <> s[i] then begin
			PutBack(c);
			PutBack(Copy(s, 1, i - 1));
			Result := False;
			break;
		end;
	end;
	if Result and not aRemoveChar then
		PutBack(s);
end;

procedure TXmlReaderStream.SkipBlanks;
var
	c: Char;
begin
	while not Eof do begin
		c := ReadChar;
		if Ord(c) > 32 then begin
			PutBack(c);
			break;
		end;
	end;
end;

procedure TXmlReaderStream.PutBack(c: Char);
begin
	BackBuffer := BackBuffer + c;
end;

procedure TXmlReaderStream.PutBack(const s: String);
var
	s2: String;
	aLen, i: Integer;
begin
	aLen := Length(s);
	SetLength(s2, aLen);
	for i := 1 to aLen do
		s2[aLen - i + 1] := s[i];
	BackBuffer := BackBuffer + s2;
end;

{ TXmlReader }

constructor TXmlReader.Create;
begin
	inherited Create;
	FEndOfXml := True;
	GetMem(Buffer, kBufferSize);
	FElements := TStringList.Create;
end;

destructor TXmlReader.Destroy;
begin
	Close;
	FElements.Free;
	FreeMem(Buffer);
	inherited Destroy;
end;

function TXmlReader.GetElementLevel: Integer;
begin
	Result := Elements.Count;
end;

function TXmlReader.GetElementPath: String;
var
	i: Integer;
begin
	Result := '';
	for i := 0 to Elements.Count - 1 do
		Result := Result + '\' + Elements[i];
	Delete(Result, 1, 1);
end;

procedure TXmlReader.OpenXml(const aXml: String);
begin
	Open(TStringStream.Create(aXml), True);
end;

procedure TXmlReader.Open(const aFileName: String);
begin
	Open(TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite), True);
end;

procedure TXmlReader.Open(aStm: TStream; aFreeStream: Boolean);
begin
	Close;
	Stm := TXmlReaderStream.Create(aStm, aFreeStream);
	FTokenType := xrtEof;
	FEndOfXml := Stm.Eof;
	Elements.Clear;
	State := xrsScan;
end;

procedure TXmlReader.Close;
begin
	FreeAndNil(Stm);
	FTokenType := xrtEof;
	FEndOfXml := True;
	if Assigned(Elements) then
		Elements.Clear;
end;

function TXmlReader.ReadNextToken: TXmlReaderTokenType;

	procedure DoReadProcessingInstruction;
	var
		aBuf: PChar;
		aPrevC, c: Char;
		i: Integer;
	begin
		Stm.SkipBlanks;
		aBuf := Buffer;
		i := 0;
		while not Stm.Eof do begin
			c := Stm.ReadChar;
			if c in [#0..' ', '?', '>'] then begin
				aBuf^ := #0;
				FName := FName + Buffer;
				if not (c in [#0..' ']) then
					Stm.PutBack(c);
				break;
			end;
			aBuf^ := c;
			Inc(aBuf);
			Inc(i);
			if i = kBufferSize - 1 then begin
				aBuf^ := #0;
				FName := FName + Buffer;
				aBuf := Buffer;
				i := 0;
			end;
		end;
		if Name = '' then
			raise EXmlReader.Create('Ошибка в XML-документе: не задано имя в обрабатывающей инструкции');

		Stm.SkipBlanks;
		aPrevC := #0;
		aBuf := Buffer;
		i := 0;
		while not Stm.Eof do begin
			c := Stm.ReadChar;
			if (c = '>') and (aPrevC = '?') then begin
				aBuf^ := #0;
				FValue := FValue + Buffer;
				SetLength(FValue, Length(FValue) - 1);
				Result := xrtProcessingInstruction;
				State := xrsScan;
				break;
			end;
			aPrevC := c;
			aBuf^ := c;
			Inc(aBuf);
			Inc(i);
			if i = kBufferSize - 1 then begin
				aBuf^ := #0;
				FValue := FValue + Buffer;
				aBuf := Buffer;
				i := 0;
			end;
		end;
		if Result <> xrtProcessingInstruction then
			raise EXmlReader.Create('Ошибка в XML-документе: ожидается "?>"');
	end;

	procedure DoReadDocumentType;
	var
		aBuf: PChar;
		c: Char;
		aBracketCount, i: Integer;
	begin
		Stm.SkipBlanks;
		aBuf := Buffer;
		i := 0;
		while not Stm.Eof do begin
			c := Stm.ReadChar;
			if c in [#0..' ', '>'] then begin
				aBuf^ := #0;
				FName := FName + Buffer;
				if not (c in [#0..' ']) then
					Stm.PutBack(c);
				break;
			end;
			aBuf^ := c;
			Inc(aBuf);
			Inc(i);
			if i = kBufferSize - 1 then begin
				aBuf^ := #0;
				FName := FName + Buffer;
				aBuf := Buffer;
				i := 0;
			end;
		end;
		if Name = '' then
			raise EXmlReader.Create('Ошибка в XML-документе: не задано имя в типе документа');

		Stm.SkipBlanks;
		aBracketCount := 1;
		aBuf := Buffer;
		i := 0;
		while not Stm.Eof do begin
			c := Stm.ReadChar;
			if c = '<' then
				Inc(aBracketCount)
			else if c = '>' then
				Dec(aBracketCount);
			if aBracketCount = 0 then begin
				aBuf^ := #0;
				FValue := FValue + Buffer;
				Result := xrtDocumentType;
				State := xrsScan;
				break;
			end;
			aBuf^ := c;
			Inc(aBuf);
			Inc(i);
			if i = kBufferSize - 1 then begin
				aBuf^ := #0;
				FValue := FValue + Buffer;
				aBuf := Buffer;
				i := 0;
			end;
		end;
		if Result <> xrtDocumentType then
			raise EXmlReader.Create('Ошибка в XML-документе: ожидается ">" для типа документа');
	end;

	procedure DoReadComment;
	var
		aBuf: PChar;
		aPrevPrevC, aPrevC, c: Char;
		aLen, i: Integer;
	begin
		FBeginOfValue := State <> xrsReadComment;
		aPrevPrevC := #0;
		aPrevC := #0;
		aBuf := Buffer;
		i := 0;
		while not Stm.Eof do begin
			c := Stm.ReadChar;
			if (c = '>') and (aPrevC = '-') and (aPrevPrevC = '-') then begin
				aBuf^ := #0;
				FValue := FValue + Buffer;
				SetLength(FValue, Length(FValue) - 2);
				Result := xrtComment;
				State := xrsScan;
				break;
			end;
			aPrevPrevC := aPrevC;
			aPrevC := c;
			aBuf^ := c;
			Inc(aBuf);
			Inc(i);
			if i = kBufferSize - 1 then begin
				aBuf^ := #0;
				FValue := FValue + Buffer;
				aBuf := Buffer;
				i := 0;
				aLen := Length(Value);
				if (PortionSize > 0) and (aLen >= PortionSize) then begin
					Stm.PutBack(Copy(Value, aLen - 1, 2));
					SetLength(FValue, aLen - 2);
					Result := xrtComment;
					State := xrsReadComment;
					FEndOfValue := False;
					break;
				end;
			end;
		end;
		if Result <> xrtComment then
			raise EXmlReader.Create('Ошибка в XML-документе: ожидается "-->"');
	end;

	procedure DoReadCData;
	var
		aBuf: PChar;
		aPrevC, c: Char;
		aLen, i: Integer;
	begin
		FBeginOfValue := State <> xrsReadCData;
		aPrevC := #0;
		aBuf := Buffer;
		i := 0;
		while not Stm.Eof do begin
			c := Stm.ReadChar;
			if (c = ']') and (aPrevC = ']') then begin
				aBuf^ := #0;
				FValue := FValue + Buffer;
				SetLength(FValue, Length(FValue) - 1);
				Stm.SkipBlanks;
				if Stm.Eof or (Stm.ReadChar <> '>') then
					raise Exception.Create('Ошибка в XML-документе: ожидается "]]>"');
				Result := xrtCData;
				State := xrsScan;
				break;
			end;
			aPrevC := c;
			aBuf^ := c;
			Inc(aBuf);
			Inc(i);
			if i = kBufferSize - 1 then begin
				aBuf^ := #0;
				FValue := FValue + Buffer;
				aBuf := Buffer;
				i := 0;
				aLen := Length(Value);
				if (PortionSize > 0) and (aLen >= PortionSize) then begin
					Stm.PutBack(Value[aLen]);
					SetLength(FValue, aLen - 1);
					Result := xrtCData;
					State := xrsReadCData;
					FEndOfValue := False;
					break;
				end;
			end;
		end;
		if Result <> xrtCData then
			raise EXmlReader.Create('Ошибка в XML-документе: ожидается "]]"');
	end;

	procedure DoReadOpeningElement;
	var
		aBuf: PChar;
		c: Char;
		i: Integer;
	begin
		Stm.SkipBlanks;
		aBuf := Buffer;
		i := 0;
		while not Stm.Eof do begin
			c := Stm.ReadChar;
			if c in [#0..' ', '/', '>'] then begin
				aBuf^ := #0;
				FName := FName + Buffer;
				if Ord(c) <= 32 then begin
					Stm.SkipBlanks;
					if Stm.Eof then
						break;
					c := Stm.ReadChar;
				end;
				if c = '>' then
					State := xrsScan
				else begin
					Stm.PutBack(c);
					State := xrsReadElement;
				end;
				Result := xrtElementBegin;
				Elements.Add(Name);
				break;
			end;
			aBuf^ := c;
			Inc(aBuf);
			Inc(i);
			if i = kBufferSize - 1 then begin
				aBuf^ := #0;
				FName := FName + Buffer;
				aBuf := Buffer;
				i := 0;
			end;
		end;
		if Result <> xrtElementBegin then
			raise EXmlReader.Create('Ошибка в XML-документе: ожидается ">" для открывающего тэга элемента');
		if Name = '' then
			raise EXmlReader.Create('Ошибка в XML-документе: не задано имя открывающего тэга элемента');
	end;

	procedure DoReadClosingElement;
	var
		aBuf: PChar;
		c: Char;
		i: Integer;
	begin
		Stm.SkipBlanks;
		aBuf := Buffer;
		i := 0;
		while not Stm.Eof do begin
			c := Stm.ReadChar;
			if c in [#0..' ', '>'] then begin
				aBuf^ := #0;
				FName := FName + Buffer;
				if Ord(c) <= 32 then begin
					Stm.SkipBlanks;
					if Stm.Eof then
						break;
					c := Stm.ReadChar;
				end;
				if c <> '>' then
					break;
				i := Elements.Count - 1;
				if (i < 0) or (Elements[i] <> Name) then
					raise Exception.Create('Ошибка в XML-документе: несоответствие имени закрывающего тэга имени открывающему');
				Elements.Delete(i);
				Result := xrtElementEnd;
				State := xrsScan;
				break;
			end;
			aBuf^ := c;
			Inc(aBuf);
			Inc(i);
			if i = kBufferSize - 1 then begin
				aBuf^ := #0;
				FName := FName + Buffer;
				aBuf := Buffer;
				i := 0;
			end;
		end;
		if Result <> xrtElementEnd then
			raise EXmlReader.Create('Ошибка в XML-документе: ожидается ">" для закрывающего тэга элемента');
		if Name = '' then
			raise EXmlReader.Create('Ошибка в XML-документе: не задано имя закрывающего тэга элемента');
	end;

	procedure DoReadAttribute;
	var
		aBuf: PChar;
		c: Char;
		i: Integer;
		anEnclosingChar: Char;
	begin
		Stm.SkipBlanks;
		if not Stm.Eof and (Stm.ReadChar(False) = '/') then begin
			Stm.ReadChar;
			if Stm.Eof or (Stm.ReadChar <> '>') then
				raise EXmlReader.Create('Ошибка в XML-документе: ожидается "/>"');
			i := Elements.Count - 1;
			FName := Elements[i];
			Elements.Delete(i);
			Result := xrtElementEnd;
			State := xrsScan;
			exit;
		end;

		aBuf := Buffer;
		i := 0;
		while not Stm.Eof do begin
			c := Stm.ReadChar;
			if c in [#0..' ', '=', '>'] then begin
				aBuf^ := #0;
				FName := FName + Buffer;
				if Ord(c) <= 32 then begin
					Stm.SkipBlanks;
					if Stm.Eof then
						break;
					c := Stm.ReadChar;
				end;
				if c <> '=' then
					raise EXmlReader.Create('Ошибка в XML-документе: ожидается "=" для атрибута элемента');
				break;
			end;
			aBuf^ := c;
			Inc(aBuf);
			Inc(i);
			if i = kBufferSize - 1 then begin
				aBuf^ := #0;
				FName := FName + Buffer;
				aBuf := Buffer;
				i := 0;
			end;
		end;
		if Name = '' then
			raise EXmlReader.Create('Ошибка в XML-документе: не задано имя атрибута элемента');

		Stm.SkipBlanks;
		if Stm.Eof then
			raise EXmlReader.Create('Ошибка в XML-документе: не задано значение атрибута элемента');
		c := Stm.ReadChar;
		anEnclosingChar := c;
		if not (anEnclosingChar in ['"', '''']) then begin
			Stm.PutBack(c);
			anEnclosingChar := #0;
		end;
		aBuf := Buffer;
		i := 0;
		while not Stm.Eof do begin
			c := Stm.ReadChar;
			if ((anEnclosingChar <> #0) and (c = anEnclosingChar)) or
					((anEnclosingChar = #0) and (c in [#0..' ', '/', '>'])) then begin
				aBuf^ := #0;
				FValue := FValue + Buffer;
				if anEnclosingChar <> #0 then begin
					if Stm.Eof then
						break;
					c := Stm.ReadChar;
				end;
				if Ord(c) <= 32 then begin
					Stm.SkipBlanks;
					if Stm.Eof then
						break;
					c := Stm.ReadChar;
				end;
				if c = '>' then
					State := xrsScan
				else
					Stm.PutBack(c);
				Result := xrtElementAttribute;
				break;
			end;
			aBuf^ := c;
			Inc(aBuf);
			Inc(i);
			if i = kBufferSize - 1 then begin
				aBuf^ := #0;
				FValue := FValue + Buffer;
				aBuf := Buffer;
				i := 0;
			end;
		end;
		if Result <> xrtElementAttribute then
			raise EXmlReader.Create('Ошибка в XML-документе: ожидается атрибут элемента');
	end;

	procedure DoReadText;
	var
		aBuf: PChar;
		c: Char;
		aLen, i: Integer;
	begin
		FBeginOfValue := State <> xrsReadText;
		aBuf := Buffer;
		i := 0;
		while True do begin
			if Stm.Eof then begin
				aBuf^ := #0;
				FValue := FValue + Buffer;
				Result := xrtText;
				State := xrsScan;
				break;
			end;
			c := Stm.ReadChar;
			if c = '<' then begin
				aBuf^ := #0;
				FValue := FValue + Buffer;
				Stm.PutBack(c);
				Result := xrtText;
				State := xrsScan;
				break;
			end;
			aBuf^ := c;
			Inc(aBuf);
			Inc(i);
			if i = kBufferSize - 1 then begin
				aBuf^ := #0;
				FValue := FValue + Buffer;
				aBuf := Buffer;
				i := 0;
				aLen := Length(Value);
				if (PortionSize > 0) and (aLen >= PortionSize) then begin
					Stm.PutBack(Value[aLen]);
					SetLength(FValue, aLen - 1);
					Result := xrtText;
					State := xrsReadText;
					FEndOfValue := False;
					break;
				end;
			end;
		end;
		if Result <> xrtText then
			raise EXmlReader.Create('Ошибка в XML-документе: ожидается текст элемента');
	end;

var
	c: Char;
begin
	FName := '';
	FValue := '';
	FBeginOfValue := True;
	FEndOfValue := True;
	Result := xrtEof;
	if not EndOfXml and not Stm.Eof then
		case State of
			xrsReadText: DoReadText;
			xrsReadElement: DoReadAttribute;
			xrsReadComment: DoReadComment;
			xrsReadCData: DoReadCData;
			else begin //xrsScan
				if ElementLevel = 0 then
					Stm.SkipBlanks;
				if not Stm.Eof then begin
					c := Stm.ReadChar;
					if c = '<' then begin
						if Stm.Eof then
							raise Exception.Create('Ошибка в XML-документе: неожидаемое завершение текста');
						c := Stm.ReadChar;
						case c of
							'?': DoReadProcessingInstruction;
							'/': DoReadClosingElement;
							'!':
								if Stm.Expect('--') then
									DoReadComment
								else if Stm.Expect('[CDATA[') then
									DoReadCData
								else if Stm.Expect('DOCTYPE') then
									DoReadDocumentType
								else
									raise Exception.Create('Ошибка в XML-документе: неизвестная специальная инструкция');
							else begin
								Stm.PutBack(c);
								DoReadOpeningElement;
							end;
						end;
					end
					else begin
						Stm.PutBack(c);
						if ElementLevel = 0 then
							raise Exception.Create('Ошибка в XML-документе: текст вне элемента');
						DoReadText;
					end;
				end;
			end;
		end;
	if Result = xrtEof then
		FEndOfXml := True;
	FTokenType := Result;
end;

end.


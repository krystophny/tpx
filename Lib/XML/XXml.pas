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
    BackBuffer: string;

    function GetEOF: Boolean;
  public
    constructor Create(aStm: TStream; aFreeStream: Boolean);
    destructor Destroy; override;

    function ReadChar(aRemoveChar: Boolean = True): Char;
    function Expect(const S: string; aRemoveChar: Boolean = True): Boolean;
    procedure SkipBlanks;
    procedure PutBack(C: Char); overload;
    procedure PutBack(const S: string); overload;

    property EOF: Boolean read GetEOF;
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
    FName: string;
    FValue: string;
    FBeginOfValue: Boolean;
    FEndOfValue: Boolean;
    FEndOfXml: Boolean;
    FElements: TStringList;

    State: TXmlReaderState;
    Stm: TXmlReaderStream;
    Buffer: PChar;

    function GetElementLevel: Integer;
    function GetElementPath: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure OpenXml(const aXml: string);
    procedure Open(const AFileName: string); overload;
    procedure Open(aStm: TStream; aFreeStream: Boolean = False); overload;
    procedure Close;

    function ReadNextToken: TXmlReaderTokenType;

    property PortionSize: Integer read FPortionSize write FPortionSize default
      0;
    property TokenType: TXmlReaderTokenType read FTokenType;
    property Name: string read FName;
    property Value: string read FValue;
    property BeginOfValue: Boolean read FBeginOfValue;
    property EndOfValue: Boolean read FEndOfValue;
    property EndOfXml: Boolean read FEndOfXml;
    property Elements: TStringList read FElements;
    property ElementLevel: Integer read GetElementLevel;
    property ElementPath: string read GetElementPath;
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

function TXmlReaderStream.GetEOF: Boolean;
begin
  Result := (BackBuffer = '') and Reader.EOF;
end;

function TXmlReaderStream.ReadChar(aRemoveChar: Boolean): Char;
var
  aLen: Integer;
begin
  aLen := Length(BackBuffer);
  if aLen <> 0 then
  begin
    Result := BackBuffer[aLen];
    if aRemoveChar then
      SetLength(BackBuffer, aLen - 1);
  end
  else if Reader.EOF then
    Result := #0
  else
  begin
    Result := Reader.ReadChar;
    if not aRemoveChar then
      PutBack(Result);
  end;
end;

function TXmlReaderStream.Expect(const S: string; aRemoveChar: Boolean):
  Boolean;
var
  C: Char;
  I: Integer;
begin
  Result := True;
  for I := 1 to Length(S) do
  begin
    if EOF then
    begin
      PutBack(Copy(S, 1, I - 1));
      Result := False;
      Break;
    end;
    C := ReadChar;
    if C <> S[I] then
    begin
      PutBack(C);
      PutBack(Copy(S, 1, I - 1));
      Result := False;
      Break;
    end;
  end;
  if Result and not aRemoveChar then
    PutBack(S);
end;

procedure TXmlReaderStream.SkipBlanks;
var
  C: Char;
begin
  while not EOF do
  begin
    C := ReadChar;
    if Ord(C) > 32 then
    begin
      PutBack(C);
      Break;
    end;
  end;
end;

procedure TXmlReaderStream.PutBack(C: Char);
begin
  BackBuffer := BackBuffer + C;
end;

procedure TXmlReaderStream.PutBack(const S: string);
var
  S2: string;
  aLen, I: Integer;
begin
  aLen := Length(S);
  SetLength(S2, aLen);
  for I := 1 to aLen do
    S2[aLen - I + 1] := S[I];
  BackBuffer := BackBuffer + S2;
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

function TXmlReader.GetElementPath: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Elements.Count - 1 do
    Result := Result + '\' + Elements[I];
  Delete(Result, 1, 1);
end;

procedure TXmlReader.OpenXml(const aXml: string);
begin
  Open(TStringStream.Create(aXml), True);
end;

procedure TXmlReader.Open(const AFileName: string);
begin
  Open(TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite), True);
end;

procedure TXmlReader.Open(aStm: TStream; aFreeStream: Boolean);
begin
  Close;
  Stm := TXmlReaderStream.Create(aStm, aFreeStream);
  FTokenType := xrtEof;
  FEndOfXml := Stm.EOF;
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
    aPrevC, C: Char;
    I: Integer;
  begin
    Stm.SkipBlanks;
    aBuf := Buffer;
    I := 0;
    while not Stm.EOF do
    begin
      C := Stm.ReadChar;
      if C in [#0..' ', '?', '>'] then
      begin
        aBuf^ := #0;
        FName := FName + Buffer;
        if not (C in [#0..' ']) then
          Stm.PutBack(C);
        Break;
      end;
      aBuf^ := C;
      Inc(aBuf);
      Inc(I);
      if I = kBufferSize - 1 then
      begin
        aBuf^ := #0;
        FName := FName + Buffer;
        aBuf := Buffer;
        I := 0;
      end;
    end;
    if Name = '' then
      raise
        EXmlReader.Create('Ошибка в XML-документе: не задано имя в обрабатывающей инструкции');

    Stm.SkipBlanks;
    aPrevC := #0;
    aBuf := Buffer;
    I := 0;
    while not Stm.EOF do
    begin
      C := Stm.ReadChar;
      if (C = '>') and (aPrevC = '?') then
      begin
        aBuf^ := #0;
        FValue := FValue + Buffer;
        SetLength(FValue, Length(FValue) - 1);
        Result := xrtProcessingInstruction;
        State := xrsScan;
        Break;
      end;
      aPrevC := C;
      aBuf^ := C;
      Inc(aBuf);
      Inc(I);
      if I = kBufferSize - 1 then
      begin
        aBuf^ := #0;
        FValue := FValue + Buffer;
        aBuf := Buffer;
        I := 0;
      end;
    end;
    if Result <> xrtProcessingInstruction then
      raise EXmlReader.Create('Ошибка в XML-документе: ожидается "?>"');
  end;

  procedure DoReadDocumentType;
  var
    aBuf: PChar;
    C: Char;
    aBracketCount, I: Integer;
  begin
    Stm.SkipBlanks;
    aBuf := Buffer;
    I := 0;
    while not Stm.EOF do
    begin
      C := Stm.ReadChar;
      if C in [#0..' ', '>'] then
      begin
        aBuf^ := #0;
        FName := FName + Buffer;
        if not (C in [#0..' ']) then
          Stm.PutBack(C);
        Break;
      end;
      aBuf^ := C;
      Inc(aBuf);
      Inc(I);
      if I = kBufferSize - 1 then
      begin
        aBuf^ := #0;
        FName := FName + Buffer;
        aBuf := Buffer;
        I := 0;
      end;
    end;
    if Name = '' then
      raise
        EXmlReader.Create('Ошибка в XML-документе: не задано имя в типе документа');

    Stm.SkipBlanks;
    aBracketCount := 1;
    aBuf := Buffer;
    I := 0;
    while not Stm.EOF do
    begin
      C := Stm.ReadChar;
      if C = '<' then
        Inc(aBracketCount)
      else if C = '>' then
        Dec(aBracketCount);
      if aBracketCount = 0 then
      begin
        aBuf^ := #0;
        FValue := FValue + Buffer;
        Result := xrtDocumentType;
        State := xrsScan;
        Break;
      end;
      aBuf^ := C;
      Inc(aBuf);
      Inc(I);
      if I = kBufferSize - 1 then
      begin
        aBuf^ := #0;
        FValue := FValue + Buffer;
        aBuf := Buffer;
        I := 0;
      end;
    end;
    if Result <> xrtDocumentType then
      raise
        EXmlReader.Create('Ошибка в XML-документе: ожидается ">" для типа документа');
  end;

  procedure DoReadComment;
  var
    aBuf: PChar;
    aPrevPrevC, aPrevC, C: Char;
    aLen, I: Integer;
  begin
    FBeginOfValue := State <> xrsReadComment;
    aPrevPrevC := #0;
    aPrevC := #0;
    aBuf := Buffer;
    I := 0;
    while not Stm.EOF do
    begin
      C := Stm.ReadChar;
      if (C = '>') and (aPrevC = '-') and (aPrevPrevC = '-') then
      begin
        aBuf^ := #0;
        FValue := FValue + Buffer;
        SetLength(FValue, Length(FValue) - 2);
        Result := xrtComment;
        State := xrsScan;
        Break;
      end;
      aPrevPrevC := aPrevC;
      aPrevC := C;
      aBuf^ := C;
      Inc(aBuf);
      Inc(I);
      if I = kBufferSize - 1 then
      begin
        aBuf^ := #0;
        FValue := FValue + Buffer;
        aBuf := Buffer;
        I := 0;
        aLen := Length(Value);
        if (PortionSize > 0) and (aLen >= PortionSize) then
        begin
          Stm.PutBack(Copy(Value, aLen - 1, 2));
          SetLength(FValue, aLen - 2);
          Result := xrtComment;
          State := xrsReadComment;
          FEndOfValue := False;
          Break;
        end;
      end;
    end;
    if Result <> xrtComment then
      raise EXmlReader.Create('Ошибка в XML-документе: ожидается "-->"');
  end;

  procedure DoReadCData;
  var
    aBuf: PChar;
    aPrevC, C: Char;
    aLen, I: Integer;
  begin
    FBeginOfValue := State <> xrsReadCData;
    aPrevC := #0;
    aBuf := Buffer;
    I := 0;
    while not Stm.EOF do
    begin
      C := Stm.ReadChar;
      if (C = ']') and (aPrevC = ']') then
      begin
        aBuf^ := #0;
        FValue := FValue + Buffer;
        SetLength(FValue, Length(FValue) - 1);
        Stm.SkipBlanks;
        if Stm.EOF or (Stm.ReadChar <> '>') then
          raise Exception.Create('Ошибка в XML-документе: ожидается "]]>"');
        Result := xrtCData;
        State := xrsScan;
        Break;
      end;
      aPrevC := C;
      aBuf^ := C;
      Inc(aBuf);
      Inc(I);
      if I = kBufferSize - 1 then
      begin
        aBuf^ := #0;
        FValue := FValue + Buffer;
        aBuf := Buffer;
        I := 0;
        aLen := Length(Value);
        if (PortionSize > 0) and (aLen >= PortionSize) then
        begin
          Stm.PutBack(Value[aLen]);
          SetLength(FValue, aLen - 1);
          Result := xrtCData;
          State := xrsReadCData;
          FEndOfValue := False;
          Break;
        end;
      end;
    end;
    if Result <> xrtCData then
      raise EXmlReader.Create('Ошибка в XML-документе: ожидается "]]"');
  end;

  procedure DoReadOpeningElement;
  var
    aBuf: PChar;
    C: Char;
    I: Integer;
  begin
    Stm.SkipBlanks;
    aBuf := Buffer;
    I := 0;
    while not Stm.EOF do
    begin
      C := Stm.ReadChar;
      if C in [#0..' ', '/', '>'] then
      begin
        aBuf^ := #0;
        FName := FName + Buffer;
        if Ord(C) <= 32 then
        begin
          Stm.SkipBlanks;
          if Stm.EOF then
            Break;
          C := Stm.ReadChar;
        end;
        if C = '>' then
          State := xrsScan
        else
        begin
          Stm.PutBack(C);
          State := xrsReadElement;
        end;
        Result := xrtElementBegin;
        Elements.Add(Name);
        Break;
      end;
      aBuf^ := C;
      Inc(aBuf);
      Inc(I);
      if I = kBufferSize - 1 then
      begin
        aBuf^ := #0;
        FName := FName + Buffer;
        aBuf := Buffer;
        I := 0;
      end;
    end;
    if Result <> xrtElementBegin then
      raise
        EXmlReader.Create('Ошибка в XML-документе: ожидается ">" для открывающего тэга элемента');
    if Name = '' then
      raise
        EXmlReader.Create('Ошибка в XML-документе: не задано имя открывающего тэга элемента');
  end;

  procedure DoReadClosingElement;
  var
    aBuf: PChar;
    C: Char;
    I: Integer;
  begin
    Stm.SkipBlanks;
    aBuf := Buffer;
    I := 0;
    while not Stm.EOF do
    begin
      C := Stm.ReadChar;
      if C in [#0..' ', '>'] then
      begin
        aBuf^ := #0;
        FName := FName + Buffer;
        if Ord(C) <= 32 then
        begin
          Stm.SkipBlanks;
          if Stm.EOF then
            Break;
          C := Stm.ReadChar;
        end;
        if C <> '>' then
          Break;
        I := Elements.Count - 1;
        if (I < 0) or (Elements[I] <> Name) then
          raise
            Exception.Create('Ошибка в XML-документе: несоответствие имени закрывающего тэга имени открывающему');
        Elements.Delete(I);
        Result := xrtElementEnd;
        State := xrsScan;
        Break;
      end;
      aBuf^ := C;
      Inc(aBuf);
      Inc(I);
      if I = kBufferSize - 1 then
      begin
        aBuf^ := #0;
        FName := FName + Buffer;
        aBuf := Buffer;
        I := 0;
      end;
    end;
    if Result <> xrtElementEnd then
      raise
        EXmlReader.Create('Ошибка в XML-документе: ожидается ">" для закрывающего тэга элемента');
    if Name = '' then
      raise
        EXmlReader.Create('Ошибка в XML-документе: не задано имя закрывающего тэга элемента');
  end;

  procedure DoReadAttribute;
  var
    iBuf: Integer;
    C: Char;
    I: Integer;
    anEnclosingChar: Char;
  begin
    Stm.SkipBlanks;
    if not Stm.EOF and (Stm.ReadChar(False) = '/') then
    begin
      Stm.ReadChar;
      if Stm.EOF or (Stm.ReadChar <> '>') then
        raise EXmlReader.Create('Ошибка в XML-документе: ожидается "/>"');
      I := Elements.Count - 1;
      FName := Elements[I];
      Elements.Delete(I);
      Result := xrtElementEnd;
      State := xrsScan;
      Exit;
    end;

    iBuf := 0;
    I := 0;
    while not Stm.EOF do
    begin
      C := Stm.ReadChar;
      if C in [#0..' ', '=', '>'] then
      begin
        Buffer[iBuf] := #0;
        FName := FName + Buffer;
        if Ord(C) <= 32 then
        begin
          Stm.SkipBlanks;
          if Stm.EOF then
            Break;
          C := Stm.ReadChar;
        end;
        if C <> '=' then
          raise
            EXmlReader.Create('Ошибка в XML-документе: ожидается "=" для атрибута элемента');
        Break;
      end;
      Buffer[iBuf] := C;
      Inc(iBuf);
      Inc(I);
      if I = kBufferSize - 1 then
      begin
        Buffer[iBuf] := #0;
        FName := FName + Buffer;
        iBuf := 0;
        I := 0;
      end;
    end;
    if Name = '' then
      raise
        EXmlReader.Create('Ошибка в XML-документе: не задано имя атрибута элемента');

    Stm.SkipBlanks;
    if Stm.EOF then
      raise
        EXmlReader.Create('Ошибка в XML-документе: не задано значение атрибута элемента');
    C := Stm.ReadChar;
    anEnclosingChar := C;
    if not (anEnclosingChar in ['"', '''']) then
    begin
      Stm.PutBack(C);
      anEnclosingChar := #0;
    end;
    iBuf := 0;
    I := 0;
    while not Stm.EOF do
    begin
      C := Stm.ReadChar;
      if ((anEnclosingChar <> #0) and (C = anEnclosingChar)) or
        ((anEnclosingChar = #0) and (C in [#0..' ', '/', '>'])) then
      begin
        Buffer[iBuf] := #0;
        FValue := FValue + Buffer;
        if anEnclosingChar <> #0 then
        begin
          if Stm.EOF then
            Break;
          C := Stm.ReadChar;
        end;
        if Ord(C) <= 32 then
        begin
          Stm.SkipBlanks;
          if Stm.EOF then
            Break;
          C := Stm.ReadChar;
        end;
        if C = '>' then
          State := xrsScan
        else
          Stm.PutBack(C);
        Result := xrtElementAttribute;
        Break;
      end;
      Buffer[iBuf] := C;
      Inc(iBuf);
      Inc(I);
      if I = kBufferSize - 1 then
      begin
        Buffer[iBuf] := #0;
        FValue := FValue + Buffer;
        iBuf := 0;
        I := 0;
      end;
    end;
    if Result <> xrtElementAttribute then
      raise
        EXmlReader.Create('Ошибка в XML-документе: ожидается атрибут элемента * '
         + FName + anEnclosingChar + '=' + FValue + '%' + C + ' eof=' + IntToStr(Integer(False)));
  end;

  procedure DoReadText;
  var
    aBuf: PChar;
    C: Char;
    aLen, I: Integer;
  begin
    FBeginOfValue := State <> xrsReadText;
    aBuf := Buffer;
    I := 0;
    while True do
    begin
      if Stm.EOF then
      begin
        aBuf^ := #0;
        FValue := FValue + Buffer;
        Result := xrtText;
        State := xrsScan;
        Break;
      end;
      C := Stm.ReadChar;
      if C = '<' then
      begin
        aBuf^ := #0;
        FValue := FValue + Buffer;
        Stm.PutBack(C);
        Result := xrtText;
        State := xrsScan;
        Break;
      end;
      aBuf^ := C;
      Inc(aBuf);
      Inc(I);
      if I = kBufferSize - 1 then
      begin
        aBuf^ := #0;
        FValue := FValue + Buffer;
        aBuf := Buffer;
        I := 0;
        aLen := Length(Value);
        if (PortionSize > 0) and (aLen >= PortionSize) then
        begin
          Stm.PutBack(Value[aLen]);
          SetLength(FValue, aLen - 1);
          Result := xrtText;
          State := xrsReadText;
          FEndOfValue := False;
          Break;
        end;
      end;
    end;
    if Result <> xrtText then
      raise
        EXmlReader.Create('Ошибка в XML-документе: ожидается текст элемента');
  end;

var
  C: Char;
begin
  FName := '';
  FValue := '';
  FBeginOfValue := True;
  FEndOfValue := True;
  Result := xrtEof;
  if not EndOfXml and not Stm.EOF then
    case State of
      xrsReadText: DoReadText;
      xrsReadElement: DoReadAttribute;
      xrsReadComment: DoReadComment;
      xrsReadCData: DoReadCData;
    else
      begin //xrsScan
        if ElementLevel = 0 then
          Stm.SkipBlanks;
        if not Stm.EOF then
        begin
          C := Stm.ReadChar;
          if C = '<' then
          begin
            if Stm.EOF then
              raise
                Exception.Create('Ошибка в XML-документе: неожидаемое завершение текста');
            C := Stm.ReadChar;
            case C of
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
                  raise
                    Exception.Create('Ошибка в XML-документе: неизвестная специальная инструкция');
            else
              begin
                Stm.PutBack(C);
                DoReadOpeningElement;
              end;
            end;
          end
          else
          begin
            Stm.PutBack(C);
            if ElementLevel = 0 then
              raise
                Exception.Create('Ошибка в XML-документе: текст вне элемента');
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


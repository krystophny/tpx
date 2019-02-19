{-------------------------------------------------------------------------------
 Based on StitchSAX 1.1 - Trivial SAX parser for Delphi
             Copyright (C) 2002, Roman Poterin (poterin@mail.ru)
     This code is free software, you can use it without any restrictions.
Modified by Alexander Tsyplakov     
-------------------------------------------------------------------------------}

unit PrimSAX;

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface

uses IniFiles, Classes, SysUtils, XUtils, {IdStrings,} StrUtils, Contnrs;

type
{$IFDEF VER140}
  TAttributes = class(THashedStringList);
{$ELSE}
  TAttributes = class(TStringList);
{$ENDIF}

  //TSY: add tagExcl
  TStitchSAXTagType =
    (tagPI, tagStart, tagEnd, tagFull, tagComment, tagCDATA, tagText,
    tagDOCTYPE, tagExcl, tagUnknown);

  TStitchSAXTag = record
    TagType: TStitchSAXTagType;
    Name: string;
    Attributes: TAttributes;
    XML: string;
  end; // TStitchSAXTag

  TStringStack = class(TStringList)
  public
    procedure Push(const AString: string);
    function Pop: string;
  end; // TStringStack

  EStitchSAX = class(Exception)
  public
    constructor Create(const ErrorMessage: string); overload;

    constructor Create(
      const XML: string;
      const Position: Integer;
      const ErrorMessage: string = ''); overload;
  end; // EStitchSAX

  TSAXStartElementMethod = procedure(
    const Name: string;
    const Attributes: TAttributes) of object;

  TSAXEndElementMethod = procedure(const Name: string) of object;

  TSAXTextMethod = procedure(const Text: string) of object;

  TSAXCommentMethod = procedure(const Comment: string) of object;

  TSAX_CDATA_Method = procedure(const CDATA: string) of object;

  TSAX_DOCTYPE_Method = procedure(
    const DOCTYPE: string;
    const Attributes: TAttributes) of object;

  TEncoding = (enUnknown, enUTF8);

  TStitchSAX = class
  private
    XML: string;
    Attributes: TAttributes;
    XMLStack: TStringStack;
    Encoding: TEncoding;
    function NextTag: TStitchSAXTag;
    function CharsetDecode(const Value: string): string;
  public
    OnStartElement: TSAXStartElementMethod;
    OnEndElement: TSAXEndElementMethod;
    OnText: TSAXTextMethod;
    OnComment: TSAXCommentMethod;
    OnCDATA: TSAX_CDATA_Method;
    OnDOCTYPE: TSAX_DOCTYPE_Method;

    constructor Create(
      const OnStartElement: TSAXStartElementMethod;
      const OnEndElement: TSAXEndElementMethod;
      const OnText: TSAXTextMethod;
      const OnComment: TSAXCommentMethod);

    destructor Destroy; override;

    procedure ParseString(const Data: string);
    procedure ParseStream(const Stream: TStream);

    procedure BeginOfParsing;
    procedure ParseBlock(const Data: string);
    procedure EndOfParsing;
  end; // TStitchSAX

//TSY: Some variant of SAX which collects info automatically

  TPrimElement = class;

  TPrimElementMethod = procedure(
    var E: TPrimElement) of object;

  TPrimElement = class
  public
    Tag: string;
    Attributes: TAttributes;
    Text: string;
    OnEndElement: TPrimElementMethod;
    constructor Create;
    destructor Destroy; override;
  end;

  TTagProcs = class
  public
    StartProc: TPrimElementMethod;
    EndProc: TPrimElementMethod;
  end;

  TPrimSAX = class(TStitchSAX)
  private
    OnStartElement: TPrimElementMethod;
    OnEndElement: TPrimElementMethod;
    fStack: TObjectStack;
    fProcs: TStringList;
    procedure SAX_StartElement(
      const Name: string; const Attributes: TAttributes);
    procedure SAX_EndElement(const Name: string);
    procedure SAX_Text(const Text: string);
  public
    constructor Create(
      const OnStartElement, OnEndElement: TPrimElementMethod;
      const OnComment: TSAXCommentMethod);
    destructor Destroy; override;
    procedure AddProc(const Tag: string;
      const StartProc, EndProc: TPrimElementMethod);
  end;

implementation

uses SysBasic;

// TStringStack ////////////////////////////////////////////////////////////////

procedure TStringStack.Push(const AString: string);
begin
  Append(AString);
end; // Push

function TStringStack.Pop: string;
begin
  Result := Strings[Count - 1];
  Delete(Count - 1);
end; // Pop

// EStitchSAX //////////////////////////////////////////////////////////////////

constructor EStitchSAX.Create(const ErrorMessage: string);
begin
  inherited Create('Bad XML. ' + ErrorMessage);
end;

constructor EStitchSAX.Create(
  const XML: string;
  const Position: Integer;
  const ErrorMessage: string = '');

const
  Margin = 100;

var
  StartPosition: Integer;

begin
  if Position < Margin + 1 then StartPosition := 1
  else StartPosition := Position - Margin;

  inherited Create('Bad XML: ' + ErrorMessage
    + #$D#$A + Copy(XML, StartPosition, Position - StartPosition + 1));
end; // Create

// TStitchSAX //////////////////////////////////////////////////////////////////

constructor TStitchSAX.Create(
  const OnStartElement: TSAXStartElementMethod;
  const OnEndElement: TSAXEndElementMethod;
  const OnText: TSAXTextMethod;
  const OnComment: TSAXCommentMethod);
begin
  Self.OnStartElement := OnStartElement;
  Self.OnEndElement := OnEndElement;
  Self.OnText := OnText;
  Self.OnComment := OnComment;

  Attributes := TAttributes.Create;
  XMLStack := TStringStack.Create;
  OnCDATA := nil;
  OnDOCTYPE := nil;
end; // Create

destructor TStitchSAX.Destroy;
begin
  Attributes.Free;
  XMLStack.Free;
  inherited;
end; // Destroy

function TStitchSAX.NextTag: TStitchSAXTag;

var
  L, X: Integer;

  procedure SkipVoids;
  begin
    while (X <= L) and (XML[X] in [#$0..#$20]) do Inc(X);
  end; // SkipVoids

  function GetStartTag: TStitchSAXTagType;
  begin
    Result := tagUnknown;

    SkipVoids;

    if X > L then Exit;

    if XML[X] <> '<' then Result := tagText
    else
    begin
      Inc(X);

      if X > L then Exit;

      if XML[X] = '?' then
      begin
        Result := tagPI;
        Inc(X);
      end
      else if XML[X] = '/' then
      begin
        Result := tagEnd;
        Inc(X);
      end
      else if Copy(XML, X, 3) = '!--' then Result := tagComment
      else if Copy(XML, X, 8) = '![CDATA[' then
        Result := tagCDATA
      else if Copy(XML, X, 8) = '!DOCTYPE' then
        Result := tagDOCTYPE
      else if XML[X] = '!' then
      begin
        Result := tagExcl;
        Inc(X);
      end
      else Result := tagStart;
    end; // else
  end; // GetStartTag

  function GetText: string;
  begin
    Result := '';

    while (X <= L) and (XML[X] <> '<') do
    begin
      Result := Result + XML[X];
      Inc(X);
    end; // while

    if X <= L then
    begin
      Result := CharsetDecode(DecodeHtmlString({StrHtmlDecode(} Result));
      Dec(X);
    end;
  end; // GetText

  //TSY: temporary solution for Delphi 6
  function Pos_J(const SubStr: string; Str: string; I: Integer):
      Integer;
  var
    J: Integer;
  begin
    Delete(Str, 1, I - 1);
    J := Pos(SubStr, Str);
    if J = 0 then Pos_J := 0 else Pos_J := I + J - 1;
  end;

  function GetComment: string;
  var
    EndOfComment: Integer;
  begin
    Inc(X, 3);
    //EndOfComment := PosEx('-->', XML, X);
    EndOfComment := Pos_J('-->', XML, X);
    if EndOfComment = 0 then X := L + 1
    else
    begin
      Result := CharsetDecode(Copy(XML, X, EndOfComment - X));
      X := EndOfComment + 2;
    end;
  end; // GetComment

  function GetExcl: string;
  var
    Pos, EndPos: Integer;
  begin
    EndPos := X - 1;
    repeat
      Inc(EndPos);
      Pos := Pos_J('<', XML, EndPos);
      EndPos := Pos_J('>', XML, EndPos);
      if Pos > EndPos then Pos := 0;
    until Pos = 0;
    if EndPos = 0 then X := L + 1
    else
    begin
      Result := CharsetDecode(Copy(XML, X, EndPos - X));
      X := EndPos;
    end;
  end; // GetExcl

  function GetCDATA: string;
  var
    EndOfCDATA: Integer;
  begin
    Inc(X, 8);
    EndOfCDATA := Pos_J(']]>', XML, X);
    if EndOfCDATA = 0 then X := L + 1
    else
    begin
      Result := CharsetDecode(Copy(XML, X, EndOfCDATA - X));
      X := EndOfCDATA + 2;
    end;
  end; // GetCDATA

  function GetName: string;
  begin
    Result := '';

    while (X <= L) and not (XML[X] in [#$0..#$20, '>', '?', '/']) do
    begin
      if XML[X] in ['=', '"', '''', '<'] then
        raise EStitchSAX.Create(XML, X);
      Result := Result + XML[X];
      Inc(X);
    end; // while

    if (X <= L) and (Result = '') then raise EStitchSAX.Create(XML, X);
  end; // GetName

  function GetAttributes: TAttributes;

    function GetAttributeName: string;
    var
      WaitForEquity: Boolean;
    begin
      Result := '';

      if not (XML[X] in [#$0..#$20, '?', '/', '>']) then
        raise EStitchSAX.Create(XML, X);

      SkipVoids;

      if (X > L) or (XML[X] in ['?', '/', '>']) then Exit;

      WaitForEquity := False;

      while X <= L do
      begin
        if XML[X] = '=' then
        begin
          Inc(X);
          Break;
        end;

        if XML[X] in ['<', '>', '?', '/', '"', ''''] then
          raise EStitchSAX.Create(XML, X);

        if WaitForEquity then
        begin
          if not (XML[X] in [#$0..#$20]) then
            raise EStitchSAX.Create(XML, X);
        end
        else
        begin
          if XML[X] in [#$0..#$20] then WaitForEquity := True
          else Result := Result + XML[X];
        end;

        Inc(X);
      end; // while
    end; // GetAttributeName

    function GetAttributeValue: string;
    //TSY: suppot for single quotes
    var
      DoubleQuote: Boolean;
    begin
      Result := '';

      SkipVoids;

      if X > L then Exit;

      if not (XML[X] in ['"', '''']) then raise EStitchSAX.Create(XML, X);
      DoubleQuote := XML[X] = '"';

      Inc(X);

      while X <= L do
      begin
        if (DoubleQuote and (XML[X] = '"'))
          or (not DoubleQuote and (XML[X] = '''')) then
        begin
          Inc(X);
          Break;
        end;
        Result := Result + XML[X];
        Inc(X);
      end; // while

      Result := CharsetDecode(DecodeHtmlString({StrHtmlDecode(} Result));
    end; // GetAttributeValue

  var
    Name: string;

  begin
    Result := Attributes;
    Result.Clear;

    Name := GetAttributeName;
    while (X <= L) and (Name <> '') do
    begin
      if Result.IndexOfName(Name) <> -1 then
        raise EStitchSAX.Create(XML, X, 'Duplicate attribute: ');
      Result.Add(Name + '=' + GetAttributeValue);
      Name := GetAttributeName;
    end;
  end; // GetAttributes

  function GetEndTag: TStitchSAXTagType;
  begin
    Result := tagUnknown;

    SkipVoids;

    if X > L then Exit;

    if XML[X] = '>' then Result := tagStart
    else
    begin
      Inc(X);
      if X > L then Exit;
      if Copy(XML, X - 1, 2) = '?>' then Result := tagPI
      else if Copy(XML, X - 1, 2) = '/>' then Result := tagFull
      else raise EStitchSAX.Create(XML, X);
    end; // else
  end; // GetEndTag

//TST:
  function GetDOCTYPE: string;
    procedure GetItem;
    var
      ExclTag, Tag, Value: string;
    begin
      while (X <= L) and not (XML[X] in ['[', ']', '<']) do Inc(X);
      if X > L then Exit;
      if XML[X] in [']'] then Exit;
      if XML[X] in ['['] then raise EStitchSAX.Create(XML, X);
      Inc(X);
      if X > L then Exit;
      if not (XML[X] in ['!']) then raise EStitchSAX.Create(XML, X);
      Inc(X);
      ExclTag := GetName;
      if ExclTag = 'ENTITY' then
      begin
        SkipVoids;
        Tag := GetName;
        SkipVoids;
        if X > L then Exit;
        if not (XML[X] in ['"']) then raise EStitchSAX.Create(XML, X);
        Inc(X);
        Value := '';
        while (X <= L) and not (XML[X] in ['"']) do
        begin
          Value := Value + XML[X];
          Inc(X);
        end;
        if X > L then Exit;
        if not (XML[X] in ['"']) then raise EStitchSAX.Create(XML, X);
        Inc(X);
        while (X <= L) and not (XML[X] in ['[', ']', '<', '>']) do Inc(X);
        Attributes.Add(Tag + '=' + Value);
      end
      else
        while (X <= L) and not (XML[X] in ['[', ']', '<', '>']) do Inc(X);
      if X > L then Exit;
      if XML[X] in ['[', ']', '<'] then raise EStitchSAX.Create(XML, X);
      Inc(X);
    end;
  begin
    Inc(X, 8);
    Result := '';
    while (X <= L) and not (XML[X] in ['<', '>', '[', ']']) do
    begin
      Result := Result + XML[X];
      Inc(X);
    end;
    if X > L then Exit;
    if XML[X] in [']', '<']
      then raise EStitchSAX.Create(XML, X);
    if XML[X] = '>' then Exit;
    Inc(X);
    {while (X <= L) and not (XML[X] in ['[', ']']) do
    begin
      Inc(X);
    end;}
    while (X <= L) and not (XML[X] in ['[', ']']) do GetItem;
    if X > L then Exit;
    if XML[X] in ['[']
      then raise EStitchSAX.Create(XML, X);
    Inc(X);
    while (X <= L) and not (XML[X] in [#$0, '<', '>', '[', ']']) do
    begin
      Result := Result + XML[X];
      Inc(X);
    end;
    if X > L then Exit;
    if XML[X] in ['[', ']', '<']
      then raise EStitchSAX.Create(XML, X);
  end; // GetDOCTYPE

var
  BeginTag: TStitchSAXTagType;

begin // NextTag
  Result.TagType := tagUnknown;
  X := 1;
  L := Length(XML);

  BeginTag := GetStartTag;
  if X > L then Exit;

  case BeginTag of
    tagText:
      begin
        Result.Name := GetText;
        if X > L then Exit;
        Result.TagType := tagText;
      end;
    tagComment:
      begin
        Result.Name := GetComment;
        if X > L then Exit;
        Result.TagType := tagComment;
      end;
    tagCDATA:
      begin
        Result.Name := GetCDATA;
        if X > L then Exit;
        //Result.Attributes := Attributes;
        //Result.Attributes.Clear;
        Result.TagType := tagCDATA;
      end;
    tagDOCTYPE:
      begin
        Result.Name := GetDOCTYPE;
        if X > L then Exit;
        Result.Attributes := Attributes;
        Result.TagType := tagDOCTYPE;
      end
  else
    begin
      Result.Name := GetName;
      if X > L then Exit;

      if BeginTag = tagPI then Result.Name := '?' + Result.Name;

      //TSY:
      if BeginTag = tagExcl then
      begin
        Result.Name := GetExcl;
        Result.Attributes := Attributes;
        Result.Attributes.Clear;
      end
      else
        Result.Attributes := GetAttributes;
      if X > L then Exit;

      if (BeginTag = tagEnd) and (Result.Attributes.Count > 0) then
        raise EStitchSAX.Create(XML, X);

      Result.TagType := GetEndTag;
      //TSY:
      if BeginTag = tagExcl then
        Result.TagType := tagExcl;
      if X > L then Exit;

      if (BeginTag = tagEnd) and (Result.TagType = tagStart) then
        Result.TagType := tagEnd
      else if (Result.TagType <> BeginTag) and
        not ((BeginTag = tagStart) and (Result.TagType = tagFull))
        then raise EStitchSAX.Create(XML, X);
    end; // else
  end; // case

  Result.XML := Copy(XML, 1, X);
  Delete(XML, 1, X);
end; // NextTag

procedure TStitchSAX.ParseString(const Data: string);

  procedure ParseStringStream;
  // Вследствие заточенности SAX под распарсивание потоков неопределенной длины,
  // используется Delete(XML, 1, x). Поэтому рекомендуется для распарсивания
  // больших строк использовать ParseStream.
  var
    StringStream: TStringStream;
  begin
    StringStream := TStringStream.Create(Data);
    try
      ParseStream(StringStream);
    finally
      StringStream.Free;
    end;
  end; // ParseStringStream

begin
  if Length(Data) < 128 * 1024 then
  begin
    BeginOfParsing;
    ParseBlock(Data);
    EndOfParsing;
  end
  else ParseStringStream;
end; // ParseString

procedure TStitchSAX.ParseStream(const Stream: TStream);
const
  MaxBufferSize = 64 * 1024;
var
  Buffer: string;
  BufferSize, ReadBytes: Integer;
begin
  if Stream.Size = 0 then Exit;

  BeginOfParsing;

  if Stream.Size > MaxBufferSize then BufferSize := MaxBufferSize
  else BufferSize := Stream.Size;

  SetLength(Buffer, BufferSize);

{WARN UNSAFE_CODE OFF}
  ReadBytes := Stream.Read(Buffer[1], BufferSize);

  while ReadBytes = BufferSize do
  begin
    ParseBlock(Buffer);
    ReadBytes := Stream.Read(Buffer[1], BufferSize);
  end;
{WARN UNSAFE_CODE ON}

  SetLength(Buffer, ReadBytes);
  ParseBlock(Buffer);

  EndOfParsing;
end; // ParseStream

procedure TStitchSAX.BeginOfParsing;
begin
  XML := '';
  XMLStack.Clear;
  Encoding := enUnknown;
end; // BeginOfParsing

procedure TStitchSAX.ParseBlock(const Data: string);
var
  CurrentTag: TStitchSAXTag;
begin
  XML := XML + Data;
  repeat
    CurrentTag := NextTag;
    case CurrentTag.TagType of
      tagPI:
        begin
          OnStartElement(CurrentTag.Name, CurrentTag.Attributes);
          if SameText(CurrentTag.Attributes.Values['encoding'], 'utf-8')
            then Encoding := enUTF8;
          OnEndElement(CurrentTag.Name);
        end;
      tagExcl:
        begin

        end;
      tagFull:
        begin
          OnStartElement(CurrentTag.Name, CurrentTag.Attributes);
          OnEndElement(CurrentTag.Name);
        end;
      tagStart:
        begin
          XMLStack.Push(CurrentTag.Name);
          OnStartElement(CurrentTag.Name, CurrentTag.Attributes);
        end;
      tagEnd:
        begin
          if (XMLStack.Count = 0) or (XMLStack.Pop <> CurrentTag.Name) then
            raise EStitchSAX.Create('Mismatched tag end: ' + CurrentTag.Name);
          OnEndElement(CurrentTag.Name);
        end;
      tagText: OnText(CurrentTag.Name);
      tagComment: OnComment(CurrentTag.Name);
      tagCDATA: if Assigned(OnCDATA) then OnCDATA(CurrentTag.Name);
      tagDOCTYPE: if Assigned(OnDOCTYPE)
        then OnDOCTYPE(CurrentTag.Name, CurrentTag.Attributes);
    end; // case
  until CurrentTag.TagType = tagUnknown;
end; // ParseBlock

procedure TStitchSAX.EndOfParsing;
var
  I: Integer;
begin
  for I := 1 to Length(XML) do
  begin
    if not (XML[I] in [#$0..#$20]) then
      raise EStitchSAX.Create('Unexpected end of document: ' + #$D#$A + XML);
  end; // for

  if XMLStack.Count > 0 then
    raise EStitchSAX.Create('No tag end found: ' + XMLStack.Pop);
end; // EndOfParsing

function TStitchSAX.CharsetDecode(const Value: string): string;
begin
  if (Value <> '') and (Encoding = enUTF8) then
  begin
    Result := Utf8ToAnsi(Value);
    if Result = '' then
      raise EStitchSAX.Create(Value, Length(Value), 'Bad UTF-8:');
  end
  else Result := Value;
end; // CharsetDecode

     {*---- TPrimElement ----*}

constructor TPrimElement.Create;
begin
  inherited Create;
  Attributes := TAttributes.Create;
  Text := '';
  OnEndElement := nil;
end;

destructor TPrimElement.Destroy;
begin
  Attributes.Free;
  inherited Destroy;
end;

     {*---- TPrimElement ----*}

constructor TPrimSAX.Create(
  const OnStartElement, OnEndElement: TPrimElementMethod;
  const OnComment: TSAXCommentMethod);
begin
  inherited Create(
    SAX_StartElement, SAX_EndElement, SAX_Text, OnComment);
  fStack := TObjectStack.Create;
  fProcs := TStringList.Create;
  fProcs.Sorted := True;
  fProcs.CaseSensitive := True;
end;

destructor TPrimSAX.Destroy;
var
  I: Integer;
begin
  fStack.Free;
  for I := 0 to fProcs.Count - 1 do fProcs.Objects[I].Free;
  fProcs.Free;
  inherited Destroy;
end;

procedure TPrimSAX.SAX_StartElement(
  const Name: string; const Attributes: TAttributes);
var
  E: TPrimElement;
  I: Integer;
  TagProcs: TTagProcs;
begin
  E := TPrimElement.Create;
  E.Tag := Name;
  E.Attributes.Assign(Attributes);
  if fProcs.Find(Name, I) then
  begin
    TagProcs := fProcs.Objects[I] as TTagProcs;
    E.OnEndElement := TagProcs.EndProc;
    if Assigned(TagProcs.StartProc) then TagProcs.StartProc(E);
  end
  else if Assigned(OnStartElement) then OnStartElement(E);
  fStack.Push(E);
end;

procedure TPrimSAX.SAX_EndElement(const Name: string);
var
  E: TPrimElement;
begin
  E := fStack.Pop as TPrimElement;
  if Assigned(E.OnEndElement) then E.OnEndElement(E)
  else if Assigned(OnEndElement) then OnEndElement(E);
  E.Free;
end;

procedure TPrimSAX.SAX_Text(const Text: string);
var
  E: TPrimElement;
begin
  E := fStack.Peek as TPrimElement;
  E.Text := E.Text + Text;
end;

procedure TPrimSAX.AddProc(const Tag: string;
  const StartProc, EndProc: TPrimElementMethod);
var
  TagProcs: TTagProcs;
begin
  TagProcs := TTagProcs.Create;
  TagProcs.StartProc := StartProc;
  TagProcs.EndProc := EndProc;
  fProcs.AddObject(Tag, TagProcs);
end;

end.


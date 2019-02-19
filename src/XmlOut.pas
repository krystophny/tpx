unit XmlOut;

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface

uses Classes, StrUtils, Contnrs;

// A class for generating simple XML files

type

  TXmlElData = class(TObject)
    Tag: string;
    Empty: Boolean;
  end;

  TXmlOutput = class(TObject)
  protected
    fStream: TStream;
    fIndentStep: Integer;
    fIndent: Integer;
    fElStack: TObjectStack;
    fExpectEOL: Boolean;
    fPreserveSpace: Boolean;
    procedure Write(const Data: string);
  public
    EOL_Str: string;
    constructor Create;
    destructor Destroy; override;
    procedure SetStream(AStream: TStream; Indent: Integer);
    procedure DataAdded;
    procedure OpenTag(const Tag: string);
    procedure AddAttribute0(const Name, Value: string);
    procedure AddAttribute(const Name, Value: string);
    procedure OpenAttr(const Name: string);
    procedure AddAttrValue0(const Data: string);
    procedure AddAttrValue(const Data: string);
    procedure CloseAttr;
    procedure AddText(const Data: string);
    procedure AddTextInTag(const Tag: string;
      const Data: string; const Has_EOL: Boolean = False);
    procedure CloseTag;
    procedure AddComment(Data: string);
    procedure AddProcessingInstruction(
      const Target, Data: string);
    procedure NoEOL;
    property PreserveSpace: Boolean
      read fPreserveSpace write fPreserveSpace;
  end;

implementation

uses MiscUtils;

// =====================================================================
// TXmlOutput
// =====================================================================

constructor TXmlOutput.Create;
begin
  inherited Create;
  EOL_Str := EOL;
  fIndentStep := 2;
  fIndent := 0;
  fExpectEOL := False;
  fElStack := TObjectStack.Create;
  fPreserveSpace := False;
  fStream := nil;
end;

destructor TXmlOutput.Destroy;
begin
  fElStack.Free;
  inherited Destroy;
end;

procedure TXmlOutput.Write(const Data: string);
begin
  if Data = '' then Exit;
  fStream.Write(Data[1], Length(Data));
end;

procedure TXmlOutput.SetStream(
  AStream: TStream; Indent: Integer);
begin
  fStream.Free;
  fStream := AStream;
  fIndent := Indent;
  fElStack.Free;
  fElStack := TObjectStack.Create;
end;

procedure TXmlOutput.DataAdded;
begin
  if fElStack.Count > 0 then
  begin
    if (fElStack.Peek as TXmlElData).Empty then
    begin
      Write('>');
      fExpectEOL := True;
    end;
    (fElStack.Peek as TXmlElData).Empty := False;
  end;
  if fExpectEOL then
  begin
    if not fPreserveSpace then Write(EOL_Str);
    fExpectEOL := False;
  end;
end;

procedure TXmlOutput.OpenTag(const Tag: string);
var
  Data: TXmlElData;
begin
  DataAdded;
  Data := TXmlElData.Create;
  fElStack.Push(Data);
  Data.Tag := Tag;
  Data.Empty := True;
  if not fPreserveSpace then Write(DupeString(' ', fIndent));
  Write('<');
  Write(Tag);
  Inc(fIndent, fIndentStep);
end;

procedure TXmlOutput.AddAttribute0(
  const Name, Value: string);
begin
  Write(' ');
  Write(Name);
  Write('="');
  Write(Value);
  Write('"');
end;

procedure TXmlOutput.AddAttribute(
  const Name, Value: string);
begin
  Write(' ');
  Write(Name);
  Write('="');
  Write(XmlReplaceChars(Value));
  Write('"');
end;

procedure TXmlOutput.OpenAttr(const Name: string);
begin
  Write(' ');
  Write(Name);
  Write('="');
end;

procedure TXmlOutput.AddAttrValue0(const Data: string);
begin
  Write(Data);
end;

procedure TXmlOutput.AddAttrValue(const Data: string);
begin
  Write(XmlReplaceChars(Data));
end;

procedure TXmlOutput.CloseAttr;
begin
  Write('"');
end;

procedure TXmlOutput.AddText(const Data: string);
begin
  DataAdded;
  Write(XmlReplaceChars(Data));
end;

procedure TXmlOutput.AddTextInTag(const Tag: string;
  const Data: string; const Has_EOL: Boolean = False);
// The tag has only text inside
var PreserveSpace0: Boolean;
begin
  Write(XmlReplaceChars(Data));
  OpenTag(Tag);
  PreserveSpace0 := PreserveSpace;
  PreserveSpace := not Has_EOL;
  AddText(Data);
  CloseTag;
  PreserveSpace := PreserveSpace0;
end;

procedure TXmlOutput.CloseTag;
var
  Data: TXmlElData;
begin
  Data := fElStack.Pop as TXmlElData;
  Dec(fIndent, fIndentStep);
  if Data.Empty then
    Write('/>')
  else
  begin
    DataAdded;
    if not fPreserveSpace then Write(DupeString(' ', fIndent));
    Write('</');
    Write(Data.Tag);
    Write('>');
  end;
  fExpectEOL := True;
  Data.Free;
end;

procedure TXmlOutput.AddComment(Data: string);
begin
  DataAdded;
  Write('<!--');
  Data := AnsiReplaceStr(Data, '--', '-.-');
  Data := AnsiReplaceStr(Data, '--', '-.-');
  Write(Data);
  Write('-->');
  fExpectEOL := True;
end;

procedure TXmlOutput.AddProcessingInstruction(
  const Target, Data: string);
begin
  DataAdded;
  Write('<?');
  Write(Target);
  Write(' ');
  Write(Data);
  Write('?>');
  fExpectEOL := True;
end;

procedure TXmlOutput.NoEOL;
begin
  fExpectEOL := False;
end;

end.


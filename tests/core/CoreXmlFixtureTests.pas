unit CoreXmlFixtureTests;

{$mode delphi}{$H+}{$codepage utf8}

interface

implementation

uses SysUtils, Classes, XXmlDom, XUtils, TpXXmlSource, CoreTestSupport;

{ These cases cover production TpX source-envelope extraction and the XML DOM
  codec. They do not exercise Input's LCL scene loader. }

function FixtureDirectory: string;
begin
  Result := GetEnvironmentVariable('TPX_CORE_FIXTURE_DIR');
  CheckCore(Result <> '', 'TPX_CORE_FIXTURE_DIR is not set');
end;

function LoadFixture(const Name: string): TXmlDDocument;
var
  Path: string;
  Source: TFileStream;
begin
  Path := IncludeTrailingPathDelimiter(FixtureDirectory) + Name;
  Result := nil;
  RO_Init(Result, TXmlDDocument.Create);
  Source := TFileStream.Create(Path, fmOpenRead or fmShareDenyNone);
  try
    try
      Result.LoadXML(ExtractTpXXml(Source));
    except
      RO_Free(Result);
      raise;
    end;
  finally
    Source.Free;
  end;
end;

function ElementChildCount(Node: TXmlDNode): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Node.ChildNodes.Count - 1 do
    if Node.ChildNodes[I] is TXmlDElement then Inc(Result);
end;

function ElementChild(Node: TXmlDNode; Index: Integer): TXmlDElement;
var
  I, Found: Integer;
begin
  Found := 0;
  for I := 0 to Node.ChildNodes.Count - 1 do
    if Node.ChildNodes[I] is TXmlDElement then begin
      if Found = Index then Exit(Node.ChildNodes[I] as TXmlDElement);
      Inc(Found);
    end;
  raise Exception.CreateFmt('Missing element child %d under <%s>',
    [Index, Node.NodeName]);
end;

function AttributeNumber(Node: TXmlDElement; const Name: string): Double;
var
  Code: Integer;
  Value: Extended;
begin
  Val(Node.AttributeValueSt[Name], Value, Code);
  CheckCore(Code = 0, 'Invalid numeric attribute ' + Name + ' on <' +
    Node.NodeName + '>');
  Result := Value;
end;

procedure CheckAttribute(Node: TXmlDElement; const Name, Expected: string);
begin
  CheckCore(Node.AttributeValueSt[Name] = Expected,
    '<' + Node.NodeName + '> attribute ' + Name + ' expected "' +
    Expected + '", got "' + Node.AttributeValueSt[Name] + '"');
end;

procedure CheckUtf8Attribute(Node: TXmlDElement; const Name: string;
  const Expected: UTF8String);
var
  Actual: string;
begin
  Actual := Node.AttributeValueSt[Name];
  CheckCore((Length(Actual) = Length(Expected)) and
    ((Length(Actual) = 0) or
     CompareMem(@Actual[1], @Expected[1], Length(Actual))),
    '<' + Node.NodeName + '> attribute ' + Name + ' UTF-8 value changed');
end;

procedure TestIndependentTpXXmlCodecStructure;
var
  Document: TXmlDDocument;
  Root, Caption, Group, Line, Nested, TextObject, Bitmap, Curve: TXmlDElement;
begin
  Document := LoadFixture('semantic-scene.tpx');
  try
    Root := Document.DocumentElement;
    CheckCore(Assigned(Root) and (Root.NodeName = 'TpX'),
      'fixture root must be <TpX>');
    CheckAttribute(Root, 'v', '5');
    CheckNearCore(AttributeNumber(Root, 'PicScale'), 1.25, 1e-6,
      'drawing scale');
    CheckAttribute(Root, 'TeXFormat', 'tikz');

    Caption := ElementChild(Root, 0);
    CheckCore(Caption.NodeName = 'caption', 'caption metadata order changed');
    CheckAttribute(Caption, 'label', 'fig:core-fixture');
    CheckCore(Caption.Text = 'Independent fixture',
      'caption text was not decoded');

    Group := ElementChild(Root, 2);
    CheckCore(Group.NodeName = 'group', 'outer group must precede later geometry');
    CheckCore(ElementChildCount(Group) = 3,
      'outer group must contain line, nested group, and image in order');
    Line := ElementChild(Group, 0);
    CheckCore(Line.NodeName = 'line', 'first grouped object must be a line');
    CheckNearCore(AttributeNumber(Line, 'x1'), 1.125, 1e-6, 'line x1');
    CheckNearCore(AttributeNumber(Line, 'y1'), 2.25, 1e-6, 'line y1');
    CheckNearCore(AttributeNumber(Line, 'x2'), 9.5, 1e-6, 'line x2');
    CheckNearCore(AttributeNumber(Line, 'y2'), 5.75, 1e-6, 'line y2');
    CheckAttribute(Line, 'li', 'dash');
    CheckNearCore(AttributeNumber(Line, 'lw'), 0.4, 1e-6, 'line width');
    CheckAttribute(Line, 'lc', '#123456');

    Nested := ElementChild(Group, 1);
    CheckCore(Nested.NodeName = 'group', 'nested group order changed');
    TextObject := ElementChild(Nested, 0);
    CheckCore(TextObject.NodeName = 'text', 'nested text object missing');
    CheckUtf8Attribute(TextObject, 't', 'Café α');
    CheckAttribute(TextObject, 'tex', '\alpha_i^2');
    CheckNearCore(AttributeNumber(TextObject, 'rotdeg'), 30, 1e-6,
      'text rotation');
    CheckCore(ElementChildCount(Nested) = 2,
      'nested group must contain text followed by rectangle');
    Line := ElementChild(Nested, 1);
    CheckCore(Line.NodeName = 'rect', 'nested rectangle order changed');
    CheckNearCore(AttributeNumber(Line, 'w'), 2.5, 1e-6, 'rectangle width');
    CheckAttribute(Line, 'fill', '#e0f0ff');
    CheckAttribute(Line, 'ha', '1');
    CheckAttribute(Line, 'hc', '#808080');

    Bitmap := ElementChild(Group, 2);
    CheckCore(Bitmap.NodeName = 'bitmap', 'relative image link order changed');
    CheckAttribute(Bitmap, 'link', 'assets/plots/parallel.png');
    CheckNearCore(AttributeNumber(Bitmap, 'w'), 8.25, 1e-6, 'image width');
    CheckNearCore(AttributeNumber(Bitmap, 'h'), 4.75, 1e-6, 'image height');

    Curve := ElementChild(Root, 3);
    CheckCore(Curve.NodeName = 'curve', 'later curve must follow grouped objects');
    CheckCore(Pos('1.25,2.5', Curve.Text) > 0,
      'curve control coordinates were not retained');
  finally
    RO_Free(Document);
  end;
end;

procedure TestEmptyTpXFixture;
var
  Document: TXmlDDocument;
begin
  Document := LoadFixture('empty-drawing.tpx');
  try
    CheckCore(Assigned(Document.DocumentElement), 'empty drawing root is missing');
    CheckCore(ElementChildCount(Document.DocumentElement) = 0,
      'empty drawing fixture unexpectedly contains scene objects');
  finally
    RO_Free(Document);
  end;
end;

procedure TestInlineFirstChildDoesNotCloseRoot;
var
  Document: TXmlDDocument;
  Root, FirstLine, SecondLine: TXmlDElement;
begin
  Document := LoadFixture('inline-first-child.tpx');
  try
    Root := Document.DocumentElement;
    CheckCore(Assigned(Root) and (Root.NodeName = 'TpX'),
      'inline-child fixture root must be <TpX>');
    CheckCore(ElementChildCount(Root) = 2,
      'root extraction stopped at a self-closing child on the root line');
    FirstLine := ElementChild(Root, 0);
    SecondLine := ElementChild(Root, 1);
    CheckCore((FirstLine.NodeName = 'line') and
      (SecondLine.NodeName = 'line'), 'inline child geometry was not retained');
    CheckNearCore(AttributeNumber(FirstLine, 'x2'), 3, 1e-6,
      'inline first child endpoint');
    CheckNearCore(AttributeNumber(SecondLine, 'y2'), 8, 1e-6,
      'following child endpoint');
  finally
    RO_Free(Document);
  end;
end;

procedure TestMalformedDocumentRejected;
var
  Document: TXmlDDocument;
  Rejected: Boolean;
begin
  Document := nil;
  RO_Init(Document, TXmlDDocument.Create);
  Rejected := False;
  try
    try
      Document.LoadXML('<TpX><line x1="0"></TpX>');
    except
      on E: Exception do Rejected := True;
    end;
    CheckCore(Rejected,
      'production XML parser accepted mismatched closing elements');
  finally
    RO_Free(Document);
  end;
end;

initialization
  RegisterCoreTest('tpx-source-envelope-and-xml-codec', TestIndependentTpXXmlCodecStructure);
  RegisterCoreTest('empty-tpx-xml-document', TestEmptyTpXFixture);
  RegisterCoreTest('inline-first-child-does-not-close-root', TestInlineFirstChildDoesNotCloseRoot);
  RegisterCoreTest('malformed-xml-is-rejected', TestMalformedDocumentRejected);

end.

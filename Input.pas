unit Input;

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface

uses Types, SysUtils, Classes, Graphics, Contnrs,
  Drawings, GObjBase, GObjects,
{$IFDEF VER140}
  Variants,
{$ENDIF}
  XUtils, XXmlDom, Geometry, Devices;

{$I tpx.inc}

type

  T_Loader = class(TObject)
  protected
    fDrawing2D: TDrawing2D;
  public
    constructor Create(Drawing: TDrawing2D);
  end;

  T_TpX_Loader = class(T_Loader)
  private
    fStream: TStream;
    fXML: TXMLDDocument;
    fVersion: Integer;
    PicWidth, picHeight: TRealType;
  protected
    procedure ReadPrimitiveAttr(Obj: TPrimitive2D;
      XMLNode: TXMLDElement);
    procedure ReadArrows(XMLNode: TXMLDElement;
      Primitive2D: TPrimitive2D);
    function ReadLine(XMLNode: TXMLDElement): TPrimitive2D;
    procedure ReadRectOld(const XMLNode: TXMLDElement;
      var P0, P1, P2: TPoint2D);
    function ReadRect(XMLNode: TXMLDElement): TPrimitive2D;
    function ReadText(XMLNode: TXMLDElement): TPrimitive2D;
    function ReadEllipse(XMLNode: TXMLDElement): TPrimitive2D;
    function ReadCircle(XMLNode: TXMLDElement): TPrimitive2D;
    function ReadEllArc(XMLNode: TXMLDElement): TPrimitive2D;
    function ReadCircular(XMLNode: TXMLDElement;
      const Kind: TCircularKind): TPrimitive2D;
    function ReadSmooth(XMLNode: TXMLDElement): TPrimitive2D;
    function ReadBezier(XMLNode: TXMLDElement): TPrimitive2D;
    function ReadPolygon(XMLNode: TXMLDElement): TPrimitive2D;
    function ReadPolyline(XMLNode: TXMLDElement): TPrimitive2D;
    function ReadStar(XMLNode: TXMLDElement): TStar2D;
    function ReadSymbol(XMLNode: TXMLDElement): TSymbol2D;
    function ReadEntity(XMLNode: TXMLDElement): TPrimitive2D;
  public
    constructor Create(Drawing: TDrawing2D);
    destructor Destroy; override;
    procedure ReadHeader;
    procedure ReadEntities;
    procedure ReadAll;
    procedure LoadFromStream;
    procedure LoadFromClipboard;
    procedure LoadFromFile(const FileName: string);
    property XMLDoc: TXMLDDocument read fXML;
  end;

  T_Import = class(T_Loader)
  protected
    fCurrPrimitive: TPrimitive2D;
    fStream: TStream;
    fLst: TGraphicObjList;
    procedure GetObj(var Obj: TPrimitive2D);
    function AddPrimitive(
      const Obj: TPrimitive2D): TPrimitive2D; virtual;
  public
    constructor Create(Drawing: TDrawing2D);
    destructor Destroy; override;
    //set line color
    procedure LC(const Cl: TColor; Obj: TPrimitive2D = nil);
    //set line width
    procedure LW(const W: TRealType; Obj: TPrimitive2D = nil);
    //set line style: none, dot, dash or solid (default)
    procedure LI(const LineStyle: TLineStyle; Obj: TPrimitive2D =
      nil);
    //set hatching: 1,...,6
    procedure Ha(const Hatching: THatching; Obj: TPrimitive2D =
      nil);
    //set hatching color
    procedure HC(const Cl: TColor; Obj: TPrimitive2D = nil);
    //set fill color
    procedure Fill(const Cl: TColor; Obj: TPrimitive2D = nil);
    function AddLine(
      const X1, Y1, X2, Y2: TRealType): TPrimitive2D; overload;
    function AddLine(
      const P1, P2: TPoint2D): TPrimitive2D; overload;
    function AddText(
      const X, Y, H: TRealType; const Txt: string): TPrimitive2D;
      overload;
    function AddText(const P: TPoint2D;
      const H: TRealType; const Txt: string): TPrimitive2D;
      overload;
    function AddRect(
      const X, Y, W, H: TRealType): TPrimitive2D; overload;
    function AddRect(const P: TPoint2D;
      const W, H: TRealType): TPrimitive2D; overload;
    function AddRect(
      const P1, P2: TPoint2D): TPrimitive2D; overload;
    function AddRectAsPolygon(
      const X, Y, W, H: TRealType): TPrimitive2D;
    function AddRoundRect(
      const X, Y, W, H, RX, RY: TRealType): TPrimitive2D; overload;
    function AddRoundRect(const P: TPoint2D;
      const W, H, RX, RY: TRealType): TPrimitive2D; overload;
    function AddRoundRectAsBezier(
      const X, Y, W, H, RX, RY: TRealType): TPrimitive2D; overload;
    function AddRoundRectAsBezier(const P: TPoint2D;
      const W, H, RX, RY: TRealType): TPrimitive2D; overload;
    function AddCircle(const X, Y, R: TRealType): TPrimitive2D;
    function AddEllipse(
      const X, Y, RX, RY: TRealType): TPrimitive2D; overload;
    function AddEllipse(const P: TPoint2D;
      const RX, RY: TRealType): TPrimitive2D; overload;
    function AddEllipseAsBezier(
      const X, Y, RX, RY: TRealType): TPrimitive2D;
    function AddCircular(const P: TPoint2D;
      const R, SA, EA: TRealType;
      const Kind: TCircularKind): TPrimitive2D;
    function AddPolygon: TPrimitive2D;
    function AddPolyline: TPrimitive2D;
    function AddBezier: TPrimitive2D;
    function AddClosedBezier: TPrimitive2D;
    procedure Scale_LineWidth(const Scale: TRealType);
  end;

procedure FillPoints(var Primitive: TPrimitive2D;
  const Text: string; Pnts: TPointsSet2D);
procedure Import_Metafile(const Drawing: TDrawing2D;
  const MF_FileName: string;
  Lines: TStrings);
procedure Import_MetafileFromStream(const Drawing: TDrawing2D;
  const Stream: TStream; const IsOld: Boolean);
procedure Import_Eps(const Drawing: TDrawing2D;
  const InputFileName: string);
procedure Import_SVG(const Drawing: TDrawing2D;
  const SvgFileName: string);

var
  PsToEditPath: string = 'pstoedit.exe';
  PsToEditFormat: string = 'plot-svg';

implementation

uses Math, Forms, StrUtils, ColorEtc, Output, SysBasic,
  ClpbrdOp, MiscUtils, MprtSVG, MprtEMF;

{ --================ T_TpX_Loader ==================-- }

constructor T_Loader.Create(Drawing: TDrawing2D);
begin
  inherited Create;
  fDrawing2D := Drawing;
end;

{ --================ T_TpX_Loader ==================-- }

procedure T_TpX_Loader.ReadPrimitiveAttr(Obj: TPrimitive2D;
  XMLNode: TXMLDElement);
var
  St: string;
begin
  if Obj = nil then Exit;
  //Obj.LineStyle := liSolid;
  //Obj.LineWidth := 1;
  if XMLNode.AttributeNode['li'] <> nil then
  begin
    St := XMLNode.AttributeValue['li'];
    if St = 'none' then Obj.LineStyle := liNone
    else if St = 'dot' then Obj.LineStyle := liDotted
    else if St = 'dash' then Obj.LineStyle := liDashed
    else if St = '0' then Obj.LineStyle := liNone
    else if St = '3' then
    begin
      Obj.LineStyle := liDotted;
      Obj.LineWidth := 2;
    end
    else if St = '4' then
    begin
      Obj.LineStyle := liDashed;
      //Obj.LineWidth := 1;
    end;
  end;
  if XMLNode.AttributeNode['lw'] <> nil then
    Obj.LineWidth := XMLNode.AttributeValue['lw'];
  if XMLNode.AttributeNode['ha'] <> nil then
    Obj.Hatching := XMLNode.AttributeValue['ha']
  else Obj.Hatching := haNone;
  if XMLNode.AttributeNode['lc'] <> nil then
    Obj.LineColor := HtmlToColor(XMLNode.AttributeValue['lc'])
  else Obj.LineColor := Graphics.clDefault;
  if XMLNode.AttributeNode['hc'] <> nil then
    Obj.HatchColor := HtmlToColor(XMLNode.AttributeValue['hc'])
  else Obj.HatchColor := Graphics.clDefault;
  if XMLNode.AttributeNode['fill'] <> nil then
    Obj.FillColor := HtmlToColor(XMLNode.AttributeValue['fill'])
  else Obj.FillColor := Graphics.clDefault;
end;

procedure T_TpX_Loader.ReadArrows(XMLNode: TXMLDElement;
  Primitive2D: TPrimitive2D);
  function GetArr(const ID: string): TArrowKind;
  var
    I, J: Integer;
  begin
    Val(ID, I, J);
    if J = 0 then
    begin
      Result := TArrowKind(I + 1);
      Exit;
    end;
    for I := 1 to High(ArrowsIDs) do
      if ID = ArrowsIDs[I] then
      begin
        Result := TArrowKind(I);
        Break;
      end;
  end;
begin
  if XMLNode.AttributeNode['arr1'] <> nil then
    Primitive2D.BeginArrowKind :=
      GetArr(XMLNode.AttributeValueSt['arr1'])
  else Primitive2D.BeginArrowKind := arrNone;
  if XMLNode.AttributeNode['arr2'] <> nil then
    Primitive2D.EndArrowKind :=
      GetArr(XMLNode.AttributeValueSt['arr2'])
  else Primitive2D.EndArrowKind := arrNone;
  if XMLNode.AttributeNode['arrs'] <> nil then
    Primitive2D.ArrowSizeFactor := XMLNode.AttributeValue['arrs']
  else Primitive2D.ArrowSizeFactor := 1;
end;

function T_TpX_Loader.ReadLine(XMLNode: TXMLDElement):
  TPrimitive2D;
begin
  Result := TLine2D.CreateSpec(0,
    Point2D(XMLNode.AttributeValue['x1'],
    XMLNode.AttributeValue['y1']),
    Point2D(XMLNode.AttributeValue['x2'],
    XMLNode.AttributeValue['y2']));
  ReadArrows(XMLNode, Result);
  ReadPrimitiveAttr(Result, XMLNode);
end;

procedure T_TpX_Loader.ReadRectOld(const XMLNode: TXMLDElement;
  var P0, P1, P2: TPoint2D);
var
  A: TRealType;
begin
  P0 := Point2D(XMLNode.AttributeValue['x1'],
    XMLNode.AttributeValue['y1']);
  P1 := Point2D(XMLNode.AttributeValue['x2'],
    XMLNode.AttributeValue['y2']);
  if XMLNode.AttributeNode['rotdeg'] <> nil then
  begin
    A := DegToRad(XMLNode.AttributeValue['rotdeg']);
    P2 := Point2D(P0.X + Sin(A), P0.Y + Cos(A));
  end
  else if XMLNode.AttributeNode['rot'] <> nil then
  begin
    A := XMLNode.AttributeValue['rot'];
    P2 := Point2D(P0.X + Sin(A), P0.Y + Cos(A));
  end
  else
  begin
    P2 := Point2D(P0.X, P0.Y + 1);
  end;
end;

function T_TpX_Loader.ReadRect(XMLNode: TXMLDElement):
  TPrimitive2D;
var
  P0, P1, P2: TPoint2D;
  ARot, W, H, C, S: TRealType;
begin
  Result := TRectangle2D.Create(0);
  if fVersion <= 2 then ReadRectOld(XMLNode, P0, P1, P2)
  else
  begin
    P0 := Point2D(XMLNode.AttributeValue['x'],
      XMLNode.AttributeValue['y']);
    W := XMLNode.AttributeValue['w'];
    H := XMLNode.AttributeValue['h'];
    with Result as TRectangle2D do
    begin
      if XMLNode.AttributeNode['rx'] <> nil then
        RX := XMLNode.AttributeValue['rx']
      else RX := 0;
      if XMLNode.AttributeNode['ry'] <> nil then
        RY := XMLNode.AttributeValue['ry']
      else RY := 0;
    end;
    if XMLNode.AttributeNode['rotdeg'] <> nil then
      ARot := DegToRad(XMLNode.AttributeValue['rotdeg'])
    else if XMLNode.AttributeNode['rot'] <> nil then
      ARot := XMLNode.AttributeValue['rot']
    else ARot := 0;
    C := Cos(ARot);
    S := Sin(ARot);
    P2 := Point2D(P0.X - S * H, P0.Y + C * H);
    P1 := Point2D(P2.X + C * W, P2.Y + S * W);
  end;
  Result.Points[0] := P0;
  Result.Points[1] := P1;
  Result.Points[2] := P2;
  ReadPrimitiveAttr(Result, XMLNode);
end;

function T_TpX_Loader.ReadText(XMLNode: TXMLDElement):
  TPrimitive2D;
var
  X, Y, H: Single;
  Ch: Char;
  procedure ReadFont;
  var
    Data: TStringList;
    I: Integer;
    ID: string;
    XMLNodeF: TXMLDElement;
    Font: TFont;
  begin
    if not (XMLNode.FirstChild is TXMLDElement) then Exit;
    XMLNodeF := XMLNode.FirstChild as TXMLDElement;
    if XMLNodeF = nil then Exit;
    if XMLNodeF.TagName <> 'font' then Exit;
    Font := (Result as TText2D).Font;
    if XMLNodeF.AttributeNode['face'] <> nil then
      Font.Name := XMLNodeF.AttributeValueSt['face'];
    if XMLNodeF.AttributeNode['bf'] <> nil then
      if XMLNodeF.AttributeValue['bf'] = 1
        then Font.Style := Font.Style + [fsBold];
    if XMLNodeF.AttributeNode['it'] <> nil then
      if XMLNodeF.AttributeValue['it'] = 1
        then Font.Style := Font.Style + [fsItalic];
    if XMLNodeF.AttributeNode['charset'] <> nil then
      Font.Charset := XMLNodeF.AttributeValue['charset'];
    {Data := TStringList.Create;
    try
      for I := 0 to XMLNodeF.Attributes.Count - 1 do
      begin
        ID := XMLNodeF.Attributes[I].NodeName;
        Data.Values[ID] := XMLNodeF.AttributeValue[ID];
      end;
      LoadObjectProp((Result as TText2D).Font, Data);
    finally
      Data.Free;
    end;}
  end;
begin
  H := XMLNode.AttributeValue['h'];
  if fVersion <= 1 then H := H / 1.2;
  X := XMLNode.AttributeValue['x'];
  Y := XMLNode.AttributeValue['y'];
  Result := TText2D.CreateSpec(0,
    Point2D(X, Y), H, XMLNode.AttributeValueSt['t']);
  with Result as TText2D do
  begin
    Ch := string(XMLNode.AttributeValue['jh'])[1];
    case Ch of
      'l': HJustification := jhLeft;
      'r': HJustification := jhRight;
      'c': HJustification := jhCenter;
    end;
    Ch := string(XMLNode.AttributeValue['jv'])[1];
    case Ch of
      't': VJustification := jvTop;
      'b': VJustification := jvBottom;
      'c': VJustification := jvCenter;
      '0': VJustification := jvBaseline;
    end;
    if XMLNode.AttributeNode['tex'] <> nil then
      TeXText := XMLNode.AttributeValueSt['tex']
    else TeXText := '';
    if XMLNode.AttributeNode['rotdeg'] <> nil then
      Rot := DegToRad(XMLNode.AttributeValue['rotdeg'])
    else if XMLNode.AttributeNode['rot'] <> nil then
      Rot := XMLNode.AttributeValue['rot']
    else Rot := 0;
  end;
  ReadPrimitiveAttr(Result, XMLNode);
  ReadFont;
end;

function T_TpX_Loader.ReadEllipse(XMLNode: TXMLDElement):
  TPrimitive2D;
var
  CP: TPoint2D;
  DX, DY: Single;
  ARot: TRealType;
begin
  CP := Point2D(XMLNode.AttributeValue['x'],
    XMLNode.AttributeValue['y']);
  DX := XMLNode.AttributeValue['dx'];
  DY := XMLNode.AttributeValue['dy'];
  Result := TEllipse2D.Create(0);
  with Result as TEllipse2D do
  begin
    if XMLNode.AttributeNode['rotdeg'] <> nil then
      ARot := DegToRad(XMLNode.AttributeValue['rotdeg'])
    else if XMLNode.AttributeNode['rot'] <> nil then
      ARot := XMLNode.AttributeValue['rot']
    else ARot := 0;
    if fVersion <= 3 then ARot := -ARot;
    Points[0] := TransformPoint2D(
      ShiftPoint(CP, V2D(-DX / 2, -DY / 2)),
      RotateCenter2D(ARot, CP));
    Points[1] := TransformPoint2D(
      ShiftPoint(CP, V2D(DX / 2, DY / 2)),
      RotateCenter2D(ARot, CP));
    Points[2] :=
      ShiftPoint(Points[0], V2D(-Sin(ARot), Cos(ARot)));
  end;
  ReadPrimitiveAttr(Result, XMLNode);
end;

function T_TpX_Loader.ReadCircle(XMLNode: TXMLDElement):
  TPrimitive2D;
var
  CP: TPoint2D;
  D: Single;
begin
  CP := Point2D(XMLNode.AttributeValue['x'],
    XMLNode.AttributeValue['y']);
  D := XMLNode.AttributeValue['d'];
  Result := TCircle2D.Create(0);
  with Result as TCircle2D do
  begin
    Points[0] := CP;
    Points[1] := Point2D(CP.X, CP.Y + D / 2);
  end;
  ReadPrimitiveAttr(Result, XMLNode);
end;

function T_TpX_Loader.ReadEllArc(XMLNode: TXMLDElement):
  TPrimitive2D;
var
  X, Y, D, SA, EA: Single;
begin
  X := XMLNode.AttributeValue['x'];
  Y := XMLNode.AttributeValue['y'];
  D := XMLNode.AttributeValue['dx'];
  SA := XMLNode.AttributeValue['a1'];
  EA := XMLNode.AttributeValue['a2'];
  Result := TArc2D.CreateSpec(0, Point2D(X, Y), D / 2, SA, EA);
  (Result as TArc2D).StartAngle := SA;
  (Result as TArc2D).EndAngle := EA;
  ReadPrimitiveAttr(Result, XMLNode);
end;

function T_TpX_Loader.ReadCircular(XMLNode: TXMLDElement;
  const Kind: TCircularKind): TPrimitive2D;
var
  X, Y, D, SA, EA: Single;
begin
  X := XMLNode.AttributeValue['x'];
  Y := XMLNode.AttributeValue['y'];
  D := XMLNode.AttributeValue['d'];
  SA := XMLNode.AttributeValue['a1'];
  EA := XMLNode.AttributeValue['a2'];
  case Kind of
    ci_Arc: Result := TArc2D.CreateSpec(
        0, Point2D(X, Y), D / 2, SA, EA);
    ci_Sector: Result := TSector2D.CreateSpec(
        0, Point2D(X, Y), D / 2, SA, EA);
    ci_Segment: Result := TSegment2D.CreateSpec(
        0, Point2D(X, Y), D / 2, SA, EA);
  end;
  ReadArrows(XMLNode, Result);
  ReadPrimitiveAttr(Result, XMLNode);
end;

procedure FillPoints(var Primitive: TPrimitive2D;
  const Text: string; Pnts: TPointsSet2D);
var
  I, LenText, Len: Integer;
  X, Y: TRealType;
  PP: TPointsSet2D;
  procedure GetSpace;
  begin
    while (I <= LenText) and
      (Text[I] in [' ', ',', #9, #10, #13]) do Inc(I);
  end;
  function GetNum: TRealType;
  var
    Pos0: Integer;
  begin
    Pos0 := I;
    while (I <= LenText) and
      not (Text[I] in [' ', ',', #9, #10, #13]) do Inc(I);
    try
      Result := StrToRealType(Copy(Text, Pos0, I - Pos0), 0);
    except
      Result := 0;
    end;
  end;
begin
  Pnts.Clear;
  LenText := Length(Text);
  Len := 0;
  for I := 1 to LenText do
    if Text[I] = ',' then Inc(Len);
  PP := TPointsSet2D.Create(Len);
  try
    I := 1;
    GetSpace;
    while I <= LenText do
    begin
      X := GetNum;
      GetSpace;
      Y := GetNum;
      GetSpace;
      PP.Add(Point2D(X, Y));
    end;
    if PP.Count = 0 then FreeAndNil(Primitive)
    else Pnts.Copy(PP, 0, PP.Count - 1);
  finally
    PP.Free;
  end;
end;

function T_TpX_Loader.ReadSmooth(XMLNode: TXMLDElement):
  TPrimitive2D;
var
  IsClosed: Boolean;
begin
  if XMLNode.AttributeNode['closed'] <> nil
    then IsClosed := XMLNode.AttributeValue['closed'] <> 0
  else IsClosed := False;
  if IsClosed then
    Result := TClosedSmoothPath2D.CreateSpec(0, [Point2D(0, 0)])
  else Result := TSmoothPath2D.CreateSpec(0, [Point2D(0, 0)]);
  ReadArrows(XMLNode, Result);
  FillPoints(Result, XMLNode.Text, (Result as
    TSmoothPath2D0).Points);
  ReadPrimitiveAttr(Result, XMLNode);
end;

function T_TpX_Loader.ReadBezier(XMLNode: TXMLDElement):
  TPrimitive2D;
var
  IsClosed: Boolean;
begin
  if XMLNode.AttributeNode['closed'] <> nil
    then IsClosed := XMLNode.AttributeValue['closed'] <> 0
  else IsClosed := False;
  if IsClosed then
    Result := TClosedBezierPath2D.CreateSpec(0, [Point2D(0, 0)])
  else Result := TBezierPath2D.CreateSpec(0, [Point2D(0, 0)]);
  ReadArrows(XMLNode, Result);
  FillPoints(Result, XMLNode.Text, (Result as
    TBezierPath2D0).Points);
  ReadPrimitiveAttr(Result, XMLNode);
end;

function T_TpX_Loader.ReadPolygon(XMLNode: TXMLDElement):
  TPrimitive2D;
begin
  Result := TPolygon2D.CreateSpec(0, [Point2D(0, 0)]);
  FillPoints(Result, XMLNode.Text, (Result as TPolygon2D).Points);
  ReadPrimitiveAttr(Result, XMLNode);
end;

function T_TpX_Loader.ReadPolyline(XMLNode: TXMLDElement):
  TPrimitive2D;
begin
  Result := TPolyline2D.CreateSpec(0, [Point2D(0, 0)]);
  ReadArrows(XMLNode, Result);
  FillPoints(Result, XMLNode.Text, (Result as TPolyline2D).Points);
  ReadPrimitiveAttr(Result, XMLNode);
end;

function T_TpX_Loader.ReadStar(XMLNode: TXMLDElement):
  TStar2D;
var
  CP: TPoint2D;
  StarID: string;
  I: Integer;
begin
  CP := Point2D(XMLNode.AttributeValue['x'],
    XMLNode.AttributeValue['y']);
  Result := TStar2D.CreateSpec(0, CP);
  if XMLNode.AttributeNode['s'] <> nil then
  begin
    StarID := XMLNode.AttributeValue['s'];
    for I := 1 to High(StarsIDs) do
      if StarID = StarsIDs[I] then
      begin
        Result.StarKind := TStarKind(I);
        Break;
      end;
  end
  else Result.StarKind := starCircle;
  if XMLNode.AttributeNode['d'] <> nil then
    Result.StarSizeFactor := XMLNode.AttributeValue['d']
  else Result.StarSizeFactor := 1;
  ReadPrimitiveAttr(Result, XMLNode);
end;

function T_TpX_Loader.ReadSymbol(XMLNode: TXMLDElement): TSymbol2D;
var
  CP: TPoint2D;
  D: TRealType;
  SymID: string;
  I: Integer;
begin
  CP := Point2D(XMLNode.AttributeValue['x'],
    XMLNode.AttributeValue['y']);
  if XMLNode.AttributeNode['d'] <> nil then
    D := XMLNode.AttributeValue['d']
  else D := 30;
  Result := TSymbol2D.CreateSpec(0, CP, D);
  if XMLNode.AttributeNode['rotdeg'] <> nil then
    Result.Rot := DegToRad(XMLNode.AttributeValue['rotdeg'])
  else if XMLNode.AttributeNode['rot'] <> nil then
    Result.Rot := XMLNode.AttributeValue['rot']
  else Result.Rot := 0;
  if XMLNode.AttributeNode['s'] <> nil then
  begin
    SymID := XMLNode.AttributeValue['s'];
    for I := 0 to High(SymbolsIDs) do
      if SymID = SymbolsIDs[I] then
      begin
        Result.SymbolKind := TSymbolKind(I);
        Break;
      end;
  end
  else Result.SymbolKind := symDecision;
  ReadPrimitiveAttr(Result, XMLNode);
end;

function T_TpX_Loader.ReadEntity(XMLNode: TXMLDElement):
  TPrimitive2D;
var
  ID: string;
begin
  Result := nil;
  ID := XMLNode.NodeName;
  if ID = 'line' then Result := ReadLine(XMLNode)
  else if ID = 'ellipse' then Result := ReadEllipse(XMLNode)
  else if ID = 'rect' then Result := ReadRect(XMLNode)
  else if ID = 'text' then Result := ReadText(XMLNode)
  else if ID = 'circle' then Result := ReadCircle(XMLNode)
  else if ID = 'ellarc' then Result := ReadEllArc(XMLNode)
  else if ID = 'arc' then Result := ReadCircular(XMLNode, ci_Arc)
  else if ID = 'sector'
    then Result := ReadCircular(XMLNode, ci_Sector)
  else if ID = 'segment'
    then Result := ReadCircular(XMLNode, ci_Segment)
  else if (ID = 'smooth') or (ID = 'curve')
    then Result := ReadSmooth(XMLNode)
  else if (ID = 'bezier') then Result := ReadBezier(XMLNode)
  else if ID = 'polygon' then Result := ReadPolygon(XMLNode)
  else if (ID = 'path') or (ID = 'polyline')
    then Result := ReadPolyline(XMLNode)
  else if ID = 'star' then Result := ReadStar(XMLNode)
  else if ID = 'symbol' then Result := ReadSymbol(XMLNode)
  else Result := nil;
end;

constructor T_TpX_Loader.Create(Drawing: TDrawing2D);
begin
  inherited Create(Drawing);
  RO_Init(fXML, TXMLDDocument.Create);
end;

destructor T_TpX_Loader.Destroy;
begin
  RO_Free(fXML);
  inherited Destroy;
end;

function StringToChoice(Choices: string; ID: string): Integer;
var
  J: Integer;
begin
  Result := 0;
  while Choices <> '' do
  begin
    J := Pos(';', Choices);
    if J = 0 then J := Length(Choices) + 1;
    if AnsiSameText(Copy(Choices, 1, J - 1), ID) then Exit;
    Delete(Choices, 1, J);
    Inc(Result);
  end;
  Result := 0;
end;

procedure T_TpX_Loader.ReadHeader;
var
  Child: TXMLDNode;
  function GetString(ID: string; Default: string): string;
  begin
    with fXML.DocumentElement do
      if AttributeNode[ID] <> nil then
        Result := AttributeValueSt[ID] else Result := Default;
  end;
  function GetBoolean(ID: string; Default: Boolean): Boolean;
  var
    Attr: string;
  begin
    with fXML.DocumentElement do
      if AttributeNode[ID] <> nil then
      begin
        Attr := LowerCase(AttributeValue[ID]);
        if (Attr = '0') or (Attr = 'False') then
          Result := False
        else if (Attr = '1') or (Attr = 'True') then
          Result := True
        else Result := Default
      end
      else Result := Default;
  end;
  function GetRealType(ID: string; Default: TRealType):
      TRealType;
  begin
    with fXML.DocumentElement do
      if AttributeNode[ID] <> nil then
        Result := AttributeValue[ID] else Result := Default;
  end;
begin
  //if fXML.DocumentElement.HasAttribute('v') then
  with fXML.DocumentElement do
  begin
    fVersion := AttributeValue['v'];
    if fVersion > TpX_Format_Version then
      MessageBoxError(Format(
        'TpX file version (%d) is newer then TpX program can handle.' +
        ' Please, update the program', [fVersion]));
    if AttributeNode['TeXFormat'] <> nil then
      fDrawing2D.TeXFormat :=
        TeXFormatKind(StringToChoice(TeXFormat_Choice,
        AttributeValue['TeXFormat']))
    else fDrawing2D.TeXFormat := tex_eps;
    if AttributeNode['PdfTeXFormat'] <> nil then
      fDrawing2D.PdfTeXFormat :=
        PdfTeXFormatKind(StringToChoice(PdfTeXFormat_Choice,
        AttributeValue['PdfTeXFormat']))
    else fDrawing2D.PdfTeXFormat := pdftex_pdf;
    if AttributeNode['ArrowsSize'] <> nil then
      fDrawing2D.ArrowsSize := AttributeValue['ArrowsSize'];
    if AttributeNode['StarsSize'] <> nil then
      fDrawing2D.StarsSize := AttributeValue['StarsSize'];
    if AttributeNode['DefaultFontHeight'] <> nil then
      fDrawing2D.DefaultFontHeight :=
        AttributeValue['DefaultFontHeight'];
    if AttributeNode['FontName'] <> nil then
      fDrawing2D.FontName := AttributeValue['FontName'];
    if AttributeNode['DefaultSymbolSize'] <> nil then
      fDrawing2D.DefaultSymbolSize :=
        AttributeValue['DefaultSymbolSize'];
    if AttributeNode['PicWidth'] <> nil then
      PicWidth := AttributeValue['PicWidth']
    else PicWidth := 0;
    if AttributeNode['PicHeight'] <> nil then
      picHeight := AttributeValue['PicHeight']
    else picHeight := 0;
    if AttributeNode['PicScale'] <> nil then
      fDrawing2D.PicScale := AttributeValue['PicScale']
    else fDrawing2D.PicScale := 1;
    if AttributeNode['Border'] <> nil then
      fDrawing2D.Border := AttributeValue['Border']
    else fDrawing2D.Border := Border_Default;
    if AttributeNode['PicUnitLength'] <> nil then
      if AttributeValue['PicUnitLength'] <> 0 then
        fDrawing2D.BitmapRes :=
          1000 / AttributeValue['PicUnitLength'];
    if AttributeNode['BitmapRes'] <> nil then
      fDrawing2D.BitmapRes := AttributeValue['BitmapRes'];
    if AttributeNode['HatchingStep'] <> nil then
      fDrawing2D.HatchingStep :=
        AttributeValue['HatchingStep'];
    if AttributeNode['HatchingLineWidth'] <> nil then
      fDrawing2D.HatchingLineWidth :=
        AttributeValue['HatchingLineWidth']
    else fDrawing2D.HatchingLineWidth :=
      HatchingLineWidth_Default;
    if AttributeNode['DottedSize'] <> nil then
      fDrawing2D.DottedSize := AttributeValue['DottedSize'];
    if AttributeNode['DashSize'] <> nil then
      fDrawing2D.DashSize := AttributeValue['DashSize'];
    if AttributeNode['LineWidth'] <> nil then
      fDrawing2D.LineWidthBase := AttributeValue['LineWidth'];
    if AttributeNode['MiterLimit'] <> nil then
      fDrawing2D.MiterLimit := AttributeValue['MiterLimit']
    else fDrawing2D.MiterLimit := 10;
    if AttributeNode['TeXCenterFigure'] <> nil then
      fDrawing2D.TeXCenterFigure :=
        AttributeValue['TeXCenterFigure']
    else fDrawing2D.TeXCenterFigure := True;
    if AttributeNode['TeXFigure'] <> nil then
      fDrawing2D.TeXFigure :=
        TeXFigureEnvKind(StringToChoice(TeXFigure_Choice,
        AttributeValue['TeXFigure']))
    else fDrawing2D.TeXFigure := fig_figure;
    fDrawing2D.TeXFigurePlacement :=
      GetString('TeXFigurePlacement', '');
    fDrawing2D.TeXFigurePrologue :=
      GetString('TeXFigurePrologue', '');
    fDrawing2D.TeXFigureEpilogue :=
      GetString('TeXFigureEpilogue', '');
    fDrawing2D.TeXPicPrologue :=
      GetString('TeXPicPrologue', '');
    fDrawing2D.TeXPicEpilogue :=
      GetString('TeXPicEpilogue', '');
    fDrawing2D.PicMagnif :=
      GetRealType('PicMagnif', 1);
    fDrawing2D.MetaPostTeXText :=
      GetBoolean('MetaPostTeXText', True);
    fDrawing2D.IncludePath :=
      GetString('IncludePath', '');
  end;
  Child := fXML.DocumentElement.SelectSingleNode('caption');
  if Child <> nil then
  begin
    fDrawing2D.Caption := XmlUnReplaceChars(Trim(Child.Text));
    fDrawing2D.FigLabel
      := XmlUnReplaceChars(Trim(
      (Child as TXMLDElement).AttributeValueSt['label']));
  end
  else
  begin
    fDrawing2D.Caption := '';
    fDrawing2D.FigLabel := '';
  end;
  Child := fXML.DocumentElement.SelectSingleNode('comment');
  if Child <> nil
    then fDrawing2D.Comment := XmlUnReplaceChars(Trim(Child.Text))
  else fDrawing2D.Comment := '';
end;

{  if (fDrawing2D.Caption <> '') or
    (fDrawing2D.FigLabel <> '') then
    with fXML.DocumentElement.AddElement('caption') do
    begin
      Text := fDrawing2D.Caption;
      AttributeValue['label'] := fDrawing2D.FigLabel;
    end;
  if fDrawing2D.Comment <> '' then
    with fXML.DocumentElement.AddElement('comment') do
      Text := fDrawing2D.Comment;}

procedure T_TpX_Loader.ReadEntities;
var
  Tmp: TObject2D;
  I: Integer;
  Child: TXMLDNode;
  Lst: TGraphicObjList;
begin
  Lst := TGraphicObjList.Create;
  Lst.FreeOnClear := False;
  try
    for I := 0 to
      fXML.DocumentElement.ChildNodes.Count - 1 do
    begin
      Child := fXML.DocumentElement.ChildNodes[I];
      if Child is TXMLDElement then
        Tmp := ReadEntity(Child as TXMLDElement);
      if Assigned(Tmp) then
      begin
      //Tmp.Transform(Scale2D(fScale, fScale));
        Lst.Add(Tmp);
      end;
      if I mod 100 = 0 then
        ShowProgress(I / fXML.DocumentElement.ChildNodes.Count);
    end;
    fDrawing2D.AddList(Lst);
  finally
    Lst.Free;
  end;
end;

procedure T_TpX_Loader.ReadAll;
var
  Rect2D: TRect2D;
begin
  if fDrawing2D = nil then Exit;
  //if fStream = nil then Exit;
  if fXML.DocumentElement = nil then Exit;
  ReadHeader;
  ReadEntities;
  if fDrawing2D.PicScale = 0 then //for backward compatibility
  begin
    Rect2D := fDrawing2D.DrawingExtension;
    with Rect2D do
    begin
      if (Right > Left) and (Top > Bottom) then
      begin
        fDrawing2D.PicScale :=
          Min(PicWidth / (Right - Left),
          picHeight / (Top - Bottom));
      end;
    end;
    //PicWidth PicHeight
  end;
end;

procedure T_TpX_Loader.LoadFromStream;
var
  Lines1, Lines2: TStringList;
  I: Integer;
  St: string;
  Size: Integer;
  Started: Boolean;
begin
  Lines1 := TStringList.Create;
  Size := fStream.Size;
  SetLength(St, Size);
  fStream.Position := 0;
  fStream.ReadBuffer(St[1], Size);
  Lines1.Text := St;
  Lines2 := TStringList.Create;
  Started := False;
  for I := 0 to Lines1.Count - 1 do
  begin
    St := Lines1[I];
    if Pos('%', St) <> 1 then Continue;
    Delete(St, 1, 1);
    if not Started then
      if Pos('<TpX', St) > 0 then Started := True
      else Continue;
    Lines2.Add(St);
    if Pos('</TpX>', St) > 0 then Break;
  end;
  //MainUnit.MainForm.RichEdit1.Lines.Clear;
  //MainUnit.MainForm.RichEdit1.Lines.AddStrings(Lines2);
  //Lines2.SaveToFile('###');
  fXML.LoadXML(Lines2.Text);
end;

procedure T_TpX_Loader.LoadFromClipboard;
begin
  fStream.Free;
  fStream := TMemoryStream.Create;
  try
    fStream.Position := 0;
    GetStreamFromClipboardAsText(fStream);
    LoadFromStream;
  //fXML.LoadXML('<TpX><line/></TpX>');
    ReadAll;
  finally
    fStream.Free;
    fStream := nil;
  end;
end;

procedure T_TpX_Loader.LoadFromFile(const FileName: string);
begin
  fStream.Free;
  fStream := TFileStream.Create(FileName, fmOpenRead);
  try
    fStream.Position := 0;
    LoadFromStream;
    ReadAll;
    if Assigned(fDrawing2D) then
      fDrawing2D.FileName := FileName;
  finally
    fStream.Free;
    fStream := nil;
  end;
end;

{ --================ T_Import ==================-- }

constructor T_Import.Create(Drawing: TDrawing2D);
begin
  inherited Create(Drawing);
  fCurrPrimitive := nil;
  fStream := nil;
  fLst := TGraphicObjList.Create;
  fLst.FreeOnClear := False;
end;

destructor T_Import.Destroy;
begin
  fLst.Free;
  inherited Destroy;
end;

procedure T_Import.GetObj(var Obj: TPrimitive2D);
begin
  if Obj = nil then Obj := fCurrPrimitive;
end;

procedure T_Import.LC(const Cl: TColor; Obj: TPrimitive2D = nil);
begin
  GetObj(Obj);
  Obj.LineColor := Cl;
end;

procedure T_Import.LW(const W: TRealType; Obj: TPrimitive2D = nil);
begin
  GetObj(Obj);
  Obj.LineWidth := W;
end;

procedure T_Import.LI(const LineStyle: TLineStyle; Obj: TPrimitive2D
  = nil);
begin
  GetObj(Obj);
  Obj.LineStyle := LineStyle;
end;

procedure T_Import.Ha(const Hatching: THatching; Obj: TPrimitive2D
  = nil);
begin
  GetObj(Obj);
  Obj.Hatching := Hatching;
end;

procedure T_Import.HC(const Cl: TColor; Obj: TPrimitive2D = nil);
begin
  GetObj(Obj);
  Obj.HatchColor := Cl;
end;

procedure T_Import.Fill(const Cl: TColor; Obj: TPrimitive2D = nil);
begin
  GetObj(Obj);
  Obj.FillColor := Cl;
end;

function T_Import.AddPrimitive(
  const Obj: TPrimitive2D): TPrimitive2D;
begin
  if Assigned(Obj) then
  //fDrawing2D.AddObject(0, Obj);
    fLst.Add(Obj);
  fCurrPrimitive := Obj;
  Result := Obj;
end;

function T_Import.AddLine(
  const X1, Y1, X2, Y2: TRealType): TPrimitive2D;
begin
  Result := AddPrimitive(
    TLine2D.CreateSpec(0, Point2D(X1, Y1), Point2D(X2, Y2)));
end;

function T_Import.AddLine(
  const P1, P2: TPoint2D): TPrimitive2D;
begin
  Result := AddPrimitive(
    TLine2D.CreateSpec(0, P1, P2));
end;

function T_Import.AddText(
  const X, Y, H: TRealType; const Txt: string): TPrimitive2D;
begin
  Result := AddPrimitive(
    TText2D.CreateSpec(0, Point2D(X, Y), H, Txt));
end;

function T_Import.AddText(const P: TPoint2D;
  const H: TRealType; const Txt: string): TPrimitive2D;
begin
  Result := AddPrimitive(
    TText2D.CreateSpec(0, P, H, Txt));
end;

function T_Import.AddRect(
  const X, Y, W, H: TRealType): TPrimitive2D;
begin
  Result := AddPrimitive(TRectangle2D.Create(0));
  with Result do
  begin
    Points[0] := Point2D(X, Y);
    Points[1] := Point2D(X + W, Y + H);
    Points[2] := Point2D(X, Y + H);
  end;
end;

function T_Import.AddRect(const P: TPoint2D;
  const W, H: TRealType): TPrimitive2D;
begin
  Result := AddRect(P.X, P.Y, W, H);
end;

function T_Import.AddRect(
  const P1, P2: TPoint2D): TPrimitive2D;
begin
  Result := AddRect(P1, P2.X - P1.X, P2.Y - P1.Y);
end;

function T_Import.AddRectAsPolygon(
  const X, Y, W, H: TRealType): TPrimitive2D;
begin
  Result := AddPolygon;
  LinearizeRectangle(
    Result.Points, Point2D(X, Y), W, H, 0);
end;

function T_Import.AddRoundRect(
  const X, Y, W, H, RX, RY: TRealType): TPrimitive2D;
begin
  Result := AddRect(X, Y, W, H);
  (Result as TRectangle2D).RX := RX;
  if RY <> RX then
    (Result as TRectangle2D).RY := RY;
end;

function T_Import.AddRoundRect(const P: TPoint2D;
  const W, H, RX, RY: TRealType): TPrimitive2D;
begin
  Result := AddRoundRect(P.X, P.Y, W, H, RX, RY);
end;

function T_Import.AddRoundRectAsBezier(
  const X, Y, W, H, RX, RY: TRealType): TPrimitive2D;
begin
  Result := AddClosedBezier;
  RoundRectBezierPoints(Point2D(X, Y), W, H, RX, RY, 0,
    Result.Points);
end;

function T_Import.AddRoundRectAsBezier(const P: TPoint2D;
  const W, H, RX, RY: TRealType): TPrimitive2D;
begin
  Result := AddRoundRectAsBezier(P.X, P.Y, W, H, RX, RY);
end;

function T_Import.AddCircle(const X, Y, R: TRealType):
  TPrimitive2D;
begin
  Result := AddPrimitive(TCircle2D.Create(0));
  with Result do
  begin
    Points[0] := Point2D(X, Y);
    Points[1] := Point2D(X, Y + R);
  end;
end;

function T_Import.AddEllipse(
  const X, Y, RX, RY: TRealType): TPrimitive2D;
begin
  Result := AddPrimitive(TEllipse2D.Create(0));
  with Result do
  begin
    Points[0] := Point2D(X + RX, Y + RY);
    Points[1] := Point2D(X - RX, Y - RY);
    Points[2] := Point2D(X + RX, Y + RY - 1);
  end;
end;

function T_Import.AddEllipse(const P: TPoint2D;
  const RX, RY: TRealType): TPrimitive2D;
begin
  Result := AddEllipse(P.X, P.Y, RX, RY);
  {Result := AddPrimitive(TEllipse2D.Create(0));
  with Result do
  begin
    Points[0] := Point2D(X + RX, Y + RY);
    Points[1] := Point2D(X - RX, Y - RY);
    Points[2] := Point2D(X + RX, Y + RY - 1);
  end;}
end;

function T_Import.AddEllipseAsBezier(
  const X, Y, RX, RY: TRealType): TPrimitive2D;
begin
  Result := AddClosedBezier;
  EllipseBezierPoints8(Point2D(X, Y),
    RX, RY, 0, Result.Points);
end;

function T_Import.AddCircular(const P: TPoint2D;
  const R, SA, EA: TRealType;
  const Kind: TCircularKind): TPrimitive2D;
begin
  case Kind of
    ci_Arc: Result := AddPrimitive(TArc2D.CreateSpec(
        0, P, R, SA, EA));
    ci_Sector: Result := AddPrimitive(TSector2D.CreateSpec(
        0, P, R, SA, EA));
    ci_Segment: Result := AddPrimitive(TSegment2D.CreateSpec(
        0, P, R, SA, EA));
  end;
end;

function T_Import.AddPolygon: TPrimitive2D;
begin
  Result := AddPrimitive(
    TPolygon2D.CreateSpec(0, []));
end;

function T_Import.AddPolyline: TPrimitive2D;
begin
  Result := AddPrimitive(TPolyline2D.Create(0));
end;

function T_Import.AddBezier: TPrimitive2D;
begin
  Result := AddPrimitive(TBezierPath2D.Create(0));
end;

function T_Import.AddClosedBezier: TPrimitive2D;
begin
  Result := AddPrimitive(TClosedBezierPath2D.Create(0));
end;

procedure T_Import.Scale_LineWidth(const Scale: TRealType);
var
  Tmp: TObject2D;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  TmpIter := fLst.GetExclusiveIterator;
  try
    Tmp := TmpIter.First as TObject2D;
    while Tmp <> nil do
    begin
      if Tmp is TPrimitive2D then
      begin
        (Tmp as TPrimitive2D).LineWidth :=
          (Tmp as TPrimitive2D).LineWidth * Scale;
        Tmp := TmpIter.Next as TObject2D;
      end;
    end;
  finally
    TmpIter.Free;
  end;
end;

{===================== Import functions ===}

procedure Import_Metafile(const Drawing: TDrawing2D;
  const MF_FileName: string;
  Lines: TStrings);
var
  EMF_Import: T_EMF_Import;
begin
  Drawing.Clear;
  EMF_Import := T_EMF_Import.Create(Drawing);
  try
    EMF_Import.LoadFromFile(MF_FileName);
    if Lines <> nil then
    begin
      Lines.Clear;
      Lines.BeginUpdate;
      EMF_Import.pLogStrings := Lines;
    end;
    EMF_Import.ParseEmf;
    if Lines <> nil then Lines.EndUpdate;
  finally
    EMF_Import.Free;
  end;
  Drawing.Comment := Format('Imported from %s %s',
    [ExtractFileName(MF_FileName), DateTimeToStr(Now)]);
end;

procedure Import_MetafileFromStream(const Drawing: TDrawing2D;
  const Stream: TStream; const IsOld: Boolean);
var
  EMF_Import: T_EMF_Import;
begin
  Drawing.Clear;
  EMF_Import := T_EMF_Import.Create(Drawing);
  try
    EMF_Import.LoadFromStream(Stream, IsOld);
    EMF_Import.ParseEmf;
  finally
    EMF_Import.Free;
  end;
end;

procedure Import_Eps(const Drawing: TDrawing2D;
  const InputFileName: string);
var
  Ext, TempDir, TempFile: string;
  Res: Boolean;
  //emf wemf wemfc wemfnss plot-svg svg
begin
  {if not FileExists(PsToEditPath) then
  begin
    MessageBoxError('PsToEdit path not found');
    Exit;
  end;}
  if Pos('svg', PsToEditFormat) > 0 then Ext := 'svg' else
    Ext := 'emf';
  TempDir := GetTempDir;
  TempFile := TempDir + '(pic)TpX.' + Ext;
  TryDeleteFile(TempFile);
  try
    Res := FileExec(Format('"%s" "%s" "%s" -f %s',
      [PsToEditPath, InputFileName, TempFile, PsToEditFormat]), '',
      '',
      TempDir, True, True);
    if not FileExists(TempFile) then
      MessageBoxError('PsToEdit file not created')
    else
    begin
      if Ext = 'emf' then
        Import_Metafile(Drawing, TempFile, nil)
      else Import_SVG(Drawing, TempFile);
      Drawing.Comment :=
        Format('Imported from %s %s',
        [ExtractFileName(InputFileName), DateTimeToStr(Now)]);
    end;
  finally
    TryDeleteFile(TempFile);
  end;
end;

procedure Import_SVG(const Drawing: TDrawing2D;
  const SvgFileName: string);
var
  Imp: T_SVG_Import;
begin
  Imp := T_SVG_Import.Create(Drawing);
  try
    Drawing.Clear;
    Imp.ParseFromFile(SvgFileName);
    if Drawing.Comment <> '' then
      Drawing.Comment := Drawing.Comment + ' -- ';
    Drawing.Comment := Drawing.Comment +
      Format('Imported from %s %s',
      [ExtractFileName(SvgFileName), DateTimeToStr(Now)]);
  finally
    Imp.Free;
  end;
end;

end.


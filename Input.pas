unit Input;

interface

uses Types, SysUtils, Classes, Graphics, Contnrs,
  Variants, CADSys4, CS4Shapes,
  PrimSAX, XUtils, XXmlDom, Geometry;

type

  T_Loader = class(TObject)
  private
    fDrawing2D: TDrawing2D;
  public
    constructor Create(Drawing: TDrawing2D);
  end;

  T_TpX_Loader = class(T_Loader)
  private
    fStream: TStream;
    fXML: TXMLDDocument;
    fVersion: Single;
    PicWidth, PicHeight: TRealType;
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
      TheClass: TPrimitive2DClass): TPrimitive2D;
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
  private
    fCurrPrimitive: TPrimitive2D;
    fStream: TStream;
    fLst: TGraphicObjList;
    procedure GetObj(var Obj: TPrimitive2D);
  public
    constructor Create(Drawing: TDrawing2D);
    destructor Destroy; override;
    //set line color
    procedure LC(const Cl: TColor; Obj: TPrimitive2D = nil);
    //set line width
    procedure LW(const W: TRealType; Obj: TPrimitive2D = nil);
    //set line style: none, dot, dash or solid (default)
    procedure LI(const LineStyle: TLineStyle; Obj: TPrimitive2D = nil);
    //set hatching: 1,...,6
    procedure Ha(const Hatching: THatching; Obj: TPrimitive2D = nil);
    //set hatching color
    procedure HC(const Cl: TColor; Obj: TPrimitive2D = nil);
    //set fill color
    procedure Fill(const Cl: TColor; Obj: TPrimitive2D = nil);
    function AddPrimitive(const Obj: TPrimitive2D): TPrimitive2D;
    function AddLine(const X1, Y1, X2, Y2: TRealType): TPrimitive2D;
    function AddText(const X, Y, H: TRealType; const Txt: string): TPrimitive2D;
    function AddRect(const X1, Y1, X2, Y2: TRealType): TPrimitive2D;
    function AddCircle(const X, Y, R: TRealType): TPrimitive2D;
    function AddEllipse(const X, Y, RX, RY: TRealType): TPrimitive2D;
    function AddPolygon: TPrimitive2D;
    function AddPolyline: TPrimitive2D;
    function AddBezier: TPrimitive2D;
    function AddClosedBezier: TPrimitive2D;
  end;

  T_SVG_Import_State = class
    LineColor: TColor;
    LineWidth: TRealType;
    LineStyle: TLineStyle;
    Hatching: THatching;
    HatchColor: TColor;
    FillColor: TColor;
    T: TTransf2D;
    FontHeight: TRealType;
    HJustification: THJustification;
    TextPos: TPoint2D;
    Text2D: TText2D;
    procedure Inherit(const State: T_SVG_Import_State);
    procedure UpdateLC(const LineColor: TColor);
    procedure UpdateLW(const LineWidth: TRealType);
    procedure UpdateLI(const LineStyle: TLineStyle);
    procedure UpdateHA(const Hatching: THatching);
    procedure UpdateHC(const HatchColor: TColor);
    procedure UpdateFill(const FillColor: TColor);
    procedure UpdateT(const T: TTransf2D);
  end;

  T_SVG_Import = class(T_Import)
  private
    fSAX: TPrimSAX;
    fPathParser: T_SVG_Path_Parser;
    fPathPoints: TPointsSet2D;
    fPathAttributes: TAttributes;
    fStack: TObjectStack;
    fStyleAttributes: TAttributes;
    fScale: TRealType; //(user units measured in mm)
    viewBox_X, viewBox_Y, viewBox_W, viewBox_H: TRealType;
    fState: T_SVG_Import_State;
    fInStyle: Boolean;
    fCSS: TAttributes;
    fEntities: TAttributes;
    procedure PathOnClose(const X, Y: TRealType);
    procedure PathOnMoveTo(const X, Y: TRealType);
    procedure PathOnLineTo(const X, Y: TRealType);
    procedure PathOnBezierTo(const X1, Y1, X2, Y2, X3, Y3: TRealType);
    procedure StartElement(var E: TPrimElement);
    procedure SAX_Comment(const Comment: string);
    //function TransfPoint(P: TPoint2D): TPoint2D;
    function NewState: T_SVG_Import_State;
    procedure EndStateProc(var E: TPrimElement);
    procedure StyleStartProc(var E: TPrimElement);
    procedure StyleEndProc(var E: TPrimElement);
    procedure CDATAProc(const CDATA: string);
    procedure Entities_Proc(const DOCTYPE: string;
      const Attributes: TAttributes);
    function ReplaceEntities(const St: string): string;
    procedure SVGProc(var E: TPrimElement);
    procedure DescProc(var E: TPrimElement);
    procedure TitleProc(var E: TPrimElement);
    procedure StartGroupProc(var E: TPrimElement);
    procedure PathProc(var E: TPrimElement);
    procedure TextStartProc(var E: TPrimElement);
    procedure TextEndProc(var E: TPrimElement);
    procedure TSpanStartProc(var E: TPrimElement);
    procedure TSpanEndProc(var E: TPrimElement);
    procedure LineProc(var E: TPrimElement);
    procedure RectProc(var E: TPrimElement);
    procedure CircleProc(var E: TPrimElement);
    procedure EllipseProc(var E: TPrimElement);
    procedure PolygonProc(var E: TPrimElement);
    procedure PolylineProc(var E: TPrimElement);
    procedure ParseStyleAttributes(St: string);
    function StoreAttributes(const Attributes: TAttributes;
      const Tag: string): T_SVG_Import_State;
    procedure SetAttributes;
    procedure Scale_LineWidth(const Scale: TRealType; Lst: TGraphicObjList);
    procedure Correct_LineWidth;
  public
    constructor Create(Drawing: TDrawing2D);
    destructor Destroy; override;
    procedure ParseFromStream(const Stream: TStream);
    procedure ParseFromFile(const FileName: string);
  end;

function XMLNodeText(Node: TXMLDNode; Level: Integer): string;
procedure Import_Metafile(const Drawing: TDrawing2D;
  const MF_FileName: string;
  Lines: TStrings);
procedure Import_MetafileFromStream(const Drawing: TDrawing2D;
  const Stream: TStream; const IsOld: Boolean);
procedure Import_Eps(const Drawing: TDrawing2D;
  const InputFileName: string);
procedure Import_Svg(const Drawing: TDrawing2D;
  const SvgFileName: string);

var
  PsToEditPath: string = 'pstoedit.exe';
  PsToEditFormat: string = 'plot-svg';

implementation

uses Math, Forms, StrUtils, ColorEtc, EMF, Output, SysBasic, ClpbrdOp,
  MiscUtils, ZLib;

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
    Point2D(XMLNode.AttributeValue['x1'], XMLNode.AttributeValue['y1']),
    Point2D(XMLNode.AttributeValue['x2'], XMLNode.AttributeValue['y2']));
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
  (Result as TText2D).AutoSize := True;
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
    Points[0] := TransformPoint2D(Point2D(CP.X + DX / 2,
      CP.Y + DY / 2), RotateCenter2D(-ARot, CP));
    Points[1] := TransformPoint2D(Point2D(CP.X - DX / 2,
      CP.Y - DY / 2), RotateCenter2D(-ARot, CP));
    Points[2] :=
      Point2D(Points[0].X + Sin(ARot), Points[0].Y + Cos(ARot));
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
  TheClass: TPrimitive2DClass): TPrimitive2D;
var
  X, Y, D, SA, EA: Single;
begin
  X := XMLNode.AttributeValue['x'];
  Y := XMLNode.AttributeValue['y'];
  D := XMLNode.AttributeValue['d'];
  SA := XMLNode.AttributeValue['a1'];
  EA := XMLNode.AttributeValue['a2'];
  if TheClass = TArc2D then Result :=
    TArc2D.CreateSpec(0, Point2D(X, Y), D / 2, SA, EA)
  else if TheClass = TSector2D then Result :=
    TSector2D.CreateSpec(0, Point2D(X, Y), D / 2, SA, EA)
  else if TheClass = TSegment2D then Result :=
    TSegment2D.CreateSpec(0, Point2D(X, Y), D / 2, SA, EA);
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
      Result := StrToRealType(Copy(Text, Pos0, I - Pos0));
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
    else Pnts.Copy(PP, 0, Len - 1);
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
  FillPoints(Result, XMLNode.Text, (Result as TSmoothPath2D0).Points);
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
  FillPoints(Result, XMLNode.Text, (Result as TBezierPath2D0).Points);
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
  else if ID = 'arc' then Result := ReadCircular(XMLNode, TArc2D)
  else if ID = 'sector'
    then Result := ReadCircular(XMLNode, TSector2D)
  else if ID = 'segment'
    then Result := ReadCircular(XMLNode, TSegment2D)
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
  Rect: TRect2D;
  Child: TXMLDNode;
  function GetString(ID: string; Default: string): string;
  begin
    with fXML.DocumentElement do
      if AttributeNode[ID] <> nil then
        Result := AttributeValueSt[ID] else Result := Default;
  end;
  function GetBoolean(ID: string; Default: Boolean): Boolean;
  begin
    with fXML.DocumentElement do
      if AttributeNode[ID] <> nil then
        Result := AttributeValue[ID] else Result := Default;
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
    if fVersion > 3 then
      MessageBoxError(Format('TpX file version (%d) is newer then TpX program can handle. Please, update the program',
        [fVersion]));
    with Rect do
    begin
      Left := AttributeValue['l'];
      Top := AttributeValue['t'];
      Right := AttributeValue['r'];
      Bottom := AttributeValue['b'];
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
        PicHeight := AttributeValue['PicHeight']
      else PicHeight := 0;
      if AttributeNode['PicScale'] <> nil then
        fDrawing2D.PicScale := AttributeValue['PicScale']
      else fDrawing2D.PicScale := 1;
      if AttributeNode['Border'] <> nil then
        fDrawing2D.Border := AttributeValue['Border']
      else fDrawing2D.Border := Border_Default;
      if AttributeNode['PicUnitLength'] <> nil then
        fDrawing2D.PicUnitLength :=
          AttributeValue['PicUnitLength'];
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
      if AttributeNode['TeXMinLine'] <> nil then
        fDrawing2D.TeXMinLine := AttributeValue['TeXMinLine'];
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
  DrawOnAdd0: Boolean;
  Tmp: TObject2D;
  I: Integer;
  Child: TXMLDNode;
  Lst: TGraphicObjList;
begin
  DrawOnAdd0 := fDrawing2D.DrawOnAdd;
  fDrawing2D.DrawOnAdd := False;
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
  fDrawing2D.DrawOnAdd := DrawOnAdd0;
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
      if (Right <> Left) and (Top <> Bottom) then
      begin
        fDrawing2D.PicScale :=
          Min(PicWidth / (Right - Left),
          PicHeight / (Top - Bottom));
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
    if Pos('</TpX>', St) > 0 then Break;
    Lines2.Add(St);
  end;
  //MainUnit.MainForm.RichEdit1.Lines.Clear;
  //MainUnit.MainForm.RichEdit1.Lines.AddStrings(Lines2);
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

function XMLNodeText(Node: TXMLDNode; Level: Integer): string;
var
  I: Integer;
  ChildNodes: TXMLDNodeList;
  Attributes: TXmlDNamedNodeMap;
begin
  Result := DupeString('  ', Level) +
    '<' + Node.NodeName + '>';
  if Node is TXmlDCharacterData then
    Result := Result + Node.Text;
  Result := Result + EOL;
  Attributes := Node.Attributes;
  for I := 0 to Attributes.Count - 1 do
    Result := Result +
      DupeString('  ', Level + 2) +
      Attributes[I].NodeName + ' = "'
      + Attributes[I].NodeValue + '"' + EOL;
  ChildNodes := Node.ChildNodes;
  for I := 0 to ChildNodes.Count - 1 do
    Result := Result +
      XMLNodeText(ChildNodes[I], Level + 1) + EOL;
end;

procedure Import_Metafile(const Drawing: TDrawing2D;
  const MF_FileName: string;
  Lines: TStrings);
var
  EMF_Loader: T_EMF_Loader;
  TpX_Loader: T_TpX_Loader;
begin
  Drawing.Clear;
  TpX_Loader := T_TpX_Loader.Create(Drawing);
  try
    EMF_Loader := T_EMF_Loader.Create;
    try
      EMF_Loader.LoadFromFile(MF_FileName);
      EMF_Loader.FillXML(TpX_Loader.XMLDoc);
      TpX_Loader.ReadAll;
    finally
      EMF_Loader.Free;
    end;
    with Drawing do
    begin
      TeXFormat := TeXFormat_Default;
      PdfTeXFormat := PdfTeXFormat_Default;
      DefaultFontHeight := DefaultFontHeight_Default;
      Caption := '';
      FigLabel := '';
      Comment := Format('Imported from %s %s',
        [ExtractFileName(MF_FileName), DateTimeToStr(Now)]);
      PicUnitLength := PicUnitLength_Default;
      HatchingStep := HatchingStep_Default;
      DottedSize := DottedSize_Default;
      DashSize := DashSize_Default;
      TeXMinLine := TeXMinLine_Default;
      TeXCenterFigure := TeXCenterFigure_Default;
      TeXFigure := TeXFigure_Default;
      //LineWidth := LineWidth_Default;
      Border := Border_Default;
      PicMagnif := PicMagnif_Default;
      MetaPostTeXText := MetaPostTeXText_Default;
    end;
    if Lines <> nil then
    begin
      Lines.Clear;
      Lines.BeginUpdate;
      Lines.Text :=
        XMLNodeText(TpX_Loader.XMLDoc.DocumentElement, 0);
      Lines.EndUpdate;
    end;
  finally
    TpX_Loader.Free;
  end;
end;

procedure Import_MetafileFromStream(const Drawing: TDrawing2D;
  const Stream: TStream; const IsOld: Boolean);
var
  EMF_Loader: T_EMF_Loader;
  TpX_Loader: T_TpX_Loader;
begin
  Drawing.Clear;
  TpX_Loader := T_TpX_Loader.Create(Drawing);
  try
    EMF_Loader := T_EMF_Loader.Create;
    try
      EMF_Loader.LoadFromStream(Stream, IsOld);
      EMF_Loader.FillXML(TpX_Loader.XMLDoc);
      TpX_Loader.ReadAll;
    finally
      EMF_Loader.Free;
    end;
  finally
    TpX_Loader.Free;
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
  if Pos('svg', PsToEditFormat) > 0 then Ext := 'svg' else Ext := 'emf';
  TempDir := GetTempDir;
  TempFile := TempDir + '(pic)TpX.' + Ext;
  TryDeleteFile(TempFile);
  try
    Res := FileExec(Format('"%s" "%s" "%s" -f %s',
      [PsToEditPath, InputFileName, TempFile, PsToEditFormat]), '', '',
      TempDir, True, True);
    if not FileExists(TempFile) then
      MessageBoxError('PsToEdit file not created')
    else
    begin
      if Ext = 'emf' then
        Import_Metafile(Drawing, TempFile, nil)
      else Import_Svg(Drawing, TempFile);
      Drawing.Comment :=
        Format('Imported from %s %s',
        [ExtractFileName(InputFileName), DateTimeToStr(Now)]);
    end;
  finally
    TryDeleteFile(TempFile);
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

procedure T_Import.LI(const LineStyle: TLineStyle; Obj: TPrimitive2D = nil);
begin
  GetObj(Obj);
  Obj.LineStyle := LineStyle;
end;

procedure T_Import.Ha(const Hatching: THatching; Obj: TPrimitive2D = nil);
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

function T_Import.AddPrimitive(const Obj: TPrimitive2D): TPrimitive2D;
begin
  if Assigned(Obj) then
  //fDrawing2D.AddObject(0, Obj);
    fLst.Add(Obj);
  fCurrPrimitive := Obj;
  Result := Obj;
end;

function T_Import.AddLine(const X1, Y1, X2, Y2: TRealType): TPrimitive2D;
begin
  Result := AddPrimitive(
    TLine2D.CreateSpec(0, Point2D(X1, Y1), Point2D(X2, Y2)));
end;

function T_Import.AddText(const X, Y, H: TRealType;
  const Txt: string): TPrimitive2D;
begin
  Result := AddPrimitive(
    TText2D.CreateSpec(0, Point2D(X, Y), H, Txt));
end;

function T_Import.AddRect(const X1, Y1, X2, Y2: TRealType): TPrimitive2D;
var P0, P1, P2: TPoint2D;
begin
  Result := AddPrimitive(TRectangle2D.Create(0));
  with Result as TRectangle2D do
  begin
    Points[0] := Point2D(X1, Y1);
    Points[1] := Point2D(X2, Y2);
    Points[2] := Point2D(X1, Y2);
  end;
end;

function T_Import.AddCircle(const X, Y, R: TRealType): TPrimitive2D;
begin
  Result := AddPrimitive(TCircle2D.Create(0));
  with Result as TCircle2D do
  begin
    Points[0] := Point2D(X, Y);
    Points[1] := Point2D(X, Y + R);
  end;
end;

function T_Import.AddEllipse(const X, Y, RX, RY: TRealType): TPrimitive2D;
begin
  Result := AddPrimitive(TEllipse2D.Create(0));
  with Result as TEllipse2D do
  begin
    Points[0] := Point2D(X + RX, Y + RY);
    Points[1] := Point2D(X - RX, Y - RY);
    Points[2] := Point2D(X + RX, Y + RY - 1);
  end;
end;

function T_Import.AddPolygon: TPrimitive2D;
begin
  Result := AddPrimitive(
    TPolygon2D.CreateSpec(0, [Point2D(0, 0)]));
end;

function T_Import.AddPolyline: TPrimitive2D;
begin
  Result := AddPrimitive(
    TPolyline2D.CreateSpec(0, [Point2D(0, 0)]));
end;

function T_Import.AddBezier: TPrimitive2D;
begin
  Result := AddPrimitive(
    TBezierPath2D.CreateSpec(0, [Point2D(0, 0)]));
end;

function T_Import.AddClosedBezier: TPrimitive2D;
begin
  Result := AddPrimitive(
    TClosedBezierPath2D.CreateSpec(0, [Point2D(0, 0)]));
end;

{ --================ T_SVG_Import_State ==================-- }

procedure T_SVG_Import_State.Inherit(const State: T_SVG_Import_State);
begin
  LineColor := State.LineColor;
  LineWidth := State.LineWidth;
  LineStyle := State.LineStyle;
  Hatching := State.Hatching;
  HatchColor := State.HatchColor;
  FillColor := State.FillColor;
  T := State.T;
  FontHeight := State.FontHeight;
  HJustification := State.HJustification;
  TextPos := State.TextPos;
  Text2D := State.Text2D;
end;

procedure T_SVG_Import_State.UpdateLC(const LineColor: TColor);
begin
  Self.LineColor := LineColor;
end;

procedure T_SVG_Import_State.UpdateLW(const LineWidth: TRealType);
begin
  Self.LineWidth := LineWidth;
end;

procedure T_SVG_Import_State.UpdateLI(const LineStyle: TLineStyle);
begin
  Self.LineStyle := LineStyle;
end;

procedure T_SVG_Import_State.UpdateHA(const Hatching: THatching);
begin
  Self.Hatching := Hatching;
end;

procedure T_SVG_Import_State.UpdateHC(const HatchColor: TColor);
begin
  Self.HatchColor := HatchColor;
end;

procedure T_SVG_Import_State.UpdateFill(const FillColor: TColor);
begin
  Self.FillColor := FillColor;
end;

procedure T_SVG_Import_State.UpdateT(const T: TTransf2D);
begin
  Self.T := MultiplyTransform2D(T, Self.T);
end;

{ --================ T_SVG_Import ==================-- }

constructor T_SVG_Import.Create(Drawing: TDrawing2D);
  procedure AddProc(const Tag: string;
    const StartProc, EndProc: TPrimElementMethod);
  begin
    fSAX.AddProc(Tag, StartProc, EndProc);
    fSAX.AddProc('svg:' + Tag, StartProc, EndProc);
  end;
begin
  inherited Create(Drawing);
  fSAX := TPrimSAX.Create(
    StartElement, nil, SAX_Comment);
  AddProc('svg', SVGProc, nil);
  AddProc('desc', nil, DescProc);
  AddProc('title', nil, TitleProc);
  AddProc('g', StartGroupProc, EndStateProc);
  AddProc('path', PathProc, EndStateProc);
  AddProc('text', TextStartProc, TextEndProc);
  AddProc('tspan', TSpanStartProc, TSpanEndProc);
  AddProc('line', LineProc, EndStateProc);
  AddProc('rect', RectProc, EndStateProc);
  AddProc('circle', CircleProc, EndStateProc);
  AddProc('ellipse', EllipseProc, EndStateProc);
  AddProc('polygon', PolygonProc, EndStateProc);
  AddProc('polyline', PolylineProc, EndStateProc);
  AddProc('style', StyleStartProc, StyleEndProc);
  fSAX.OnCDATA := CDATAProc;
  fSAX.OnDOCTYPE := Entities_Proc;
  fPathParser := T_SVG_Path_Parser.Create(
    PathOnClose, PathOnMoveTo, PathOnLineTo, PathOnBezierTo);
  fPathPoints := TPointsSet2D.Create(0);
  fStack := TObjectStack.Create;
  fStyleAttributes := TAttributes.Create;
  fStyleAttributes.Delimiter := ';';
  fCSS := TAttributes.Create;
  fEntities := TAttributes.Create;
end;

destructor T_SVG_Import.Destroy;
begin
  fSAX.Free;
  fPathParser.Free;
  fStack.Free;
  fStyleAttributes.Free;
  fCSS.Free;
  fEntities.Free;
  inherited Destroy;
end;

{function T_SVG_Import.TransfPoint(P: TPoint2D): TPoint2D;
begin
  Result := Point2D(P.X, P.Y);
end;}

procedure T_SVG_Import.PathOnClose(const X, Y: TRealType);
begin
  if fPathParser.AllAsBezier then AddClosedBezier
  else AddPolygon;
  fCurrPrimitive.Points.Copy(fPathPoints, 0, fPathPoints.Count - 1);
  fPathPoints.Clear;
  SetAttributes;
end;

procedure T_SVG_Import.PathOnMoveTo(const X, Y: TRealType);
begin
  if fPathPoints.Count > 0 then
  begin
    if fPathParser.AllAsBezier then AddBezier
    else AddPolyline;
    fCurrPrimitive.Points.Copy(fPathPoints, 0, fPathPoints.Count - 1);
    SetAttributes;
  end;
  fPathPoints.Clear;
  fPathPoints.Add(Point2D(X, Y));
end;

procedure T_SVG_Import.PathOnLineTo(const X, Y: TRealType);
begin
  fPathPoints.Add(Point2D(X, Y));
end;

procedure T_SVG_Import.PathOnBezierTo(
  const X1, Y1, X2, Y2, X3, Y3: TRealType);
begin
  fPathPoints.Add(Point2D(X1, Y1));
  fPathPoints.Add(Point2D(X2, Y2));
  fPathPoints.Add(Point2D(X3, Y3));
end;

procedure T_SVG_Import.StartElement(var E: TPrimElement);
begin
end;

function GetRealTypeAttr(const Attributes: TAttributes;
  const ID: string; const Default: TRealType;
  const Scale: TRealType): TRealType;
//Scale --- user unit = Scale mm
var
  St, Units, NumSt: string;
  Len, Index: Integer;
const Data: array[0..9] of Single =
  (1, 3.5, 3.5, 1, 0.352777777777778, 4.23333333333333, 10, 1, 25.4, 0.01);
begin
  {if Attributes.IndexOfName(ID) < 0 then
  begin
    Result := Default;
    Exit;
  end;}
  St := Trim(Attributes.Values[ID]);
  if St = '' then
  begin
    Result := Default;
    Exit;
  end;
//The list of unit identifiers in SVG matches the list of unit identifiers in CSS:
// em, ex, px, pt, pc, cm, mm, in and percentages
  Len := Length(St);
  if St[Len] = '%' then
  begin
    Units := '%';
    NumSt := Trim(LeftStr(St, Len - 1));
  end
  else if Len > 1 then
  begin
    Units := RightStr(St, 2);
    NumSt := Trim(LeftStr(St, Len - 2));
  end;
  Index := CSV_Find('em,ex,px,pt,pc,cm,mm,in,%', Units);
  if Index = 0 then NumSt := St;
  if Pos(' ', NumSt) > 0 then NumSt := LeftStr(NumSt, Pos(' ', NumSt) - 1);
  if Pos(',', NumSt) > 0 then NumSt := LeftStr(NumSt, Pos(',', NumSt) - 1);
  Result := StrToFloat(NumSt);
  Result := Data[Index] * Result;
  if Index in [1, 2, 4, 5, 6, 7, 8] then Result := Result / Scale;
//em = 3.5/Scale (assume)
//ex = 3.5/Scale (assume)
//px = 1
//pt = 0.352777777777778/Scale
//pc = 4.23333333333333/Scale
//cm = 10/Scale
//mm = 1/Scale
//in = 25.4/Scale
//% = 0.01

{  For example, suppose that the user agent can determine from its environment that "1px" corresponds to "0.2822222mm" (i.e., 90dpi). Then, for all processing of SVG content:
"1pt" equals "1.25px" (and therefore 1.25 user units)
"1pc" equals "15px" (and therefore 15 user units)
"1mm" would be "3.543307px" (3.543307 user units)
"1cm" equals "35.43307px" (and therefore 35.43307 user units)
"1in" equals "90px" (and therefore 90 user units)}
//pc = 15/1.25 pt=3/0.25 pt=12pt
//pt=1.25/3.543307 mm=0.352777786401235mm=25.4/72mm=0.352777777777778mm
//pc=15/3.543307 mm = 4.23333343681482mm=25.4/6mm=4.23333333333333mm
//in=90/3.543307 mm = 25.4mm
//in=90/1.25 pt =72pt
end;

function T_SVG_Import.NewState: T_SVG_Import_State;
var
  OldState: T_SVG_Import_State;
begin
  if fStack.Count > 0 then
    OldState := fStack.Peek as T_SVG_Import_State
  else OldState := nil;
  Result := T_SVG_Import_State.Create;
  fStack.Push(Result);
  if Assigned(OldState) then Result.Inherit(OldState);
end;

procedure T_SVG_Import.EndStateProc(var E: TPrimElement);
begin
  fStack.Pop.Free;
end;

procedure ParseViewBox(VB: string; var X, Y, W, H: TRealType);
var
  List: TStringList;
begin
  X := 0;
  Y := 0;
  W := 100;
  H := 100;
  VB := AnsiReplaceStr(VB, ',', ' ');
  VB := AnsiReplaceStr(VB, '#10', ' ');
  VB := AnsiReplaceStr(VB, '#13', ' ');
  VB := AnsiReplaceStr(VB, '   ', ' ');
  VB := AnsiReplaceStr(VB, '  ', ' ');
  VB := AnsiReplaceStr(VB, '  ', ' ');
  List := TStringList.Create;
  try
    List.Delimiter := ' ';
    List.DelimitedText := VB;
    if List.Count < 4 then Exit;
    X := StrToRealType(List[0]);
    Y := StrToRealType(List[1]);
    W := StrToRealType(List[2]);
    H := StrToRealType(List[3]);
  finally
    List.Free;
  end;
end;

procedure T_SVG_Import.StyleStartProc(var E: TPrimElement);
begin
  fInStyle := True;
end;

procedure T_SVG_Import.StyleEndProc(var E: TPrimElement);
begin
  fInStyle := False;
end;

procedure ParseCSS(const CDATA: string; CSS: TAttributes);
var
  fParser: TSimpleParser;
  Name, Value: string;
  procedure TakeSpace;
  begin
    fParser.SkipAnyMult([#32, #13, #10, #9]);
  end;
  procedure GetItem;
  begin
    Name := '';
    Value := '';
    Name := fParser.GetAnyMult([#9..#255] - ['{', ']']);
    if fParser.ViewCh = '{' then
    begin
      fParser.GetCh;
      Value := fParser.GetAnyMult([#9..#255] - ['}', ']']);
    end;
    if fParser.ViewCh = '}' then fParser.GetCh;
    Name := Trim(Name);
    Value := Trim(Value);
  end;
begin
  CSS.Clear;
  fParser := TSimpleParser.Create;
  try
    fParser.SetSource(@CDATA);
    repeat
      GetItem;
      if Name <> '' then
      begin
        CSS.Add(Name + '=' + Value);
      end;
    until fParser.Finished;
  finally
    fParser.Free;
  end;
end;

procedure T_SVG_Import.CDATAProc(const CDATA: string);
begin
  if fInStyle then
  begin
    ParseCSS(CDATA, fCSS);
    //SysBasic.MessageBoxInfo(fCSS.Text);
  end;
end;

procedure T_SVG_Import.Entities_Proc(const DOCTYPE: string;
  const Attributes: TAttributes);
begin
  fEntities.Assign(Attributes);
end;

function T_SVG_Import.ReplaceEntities(const St: string): string;
var
  fParser: TSimpleParser;
  Entity: string;
  //WCh:WideChar;
  Ch: Char;
  function GetEntity: string;
  var
    I, J: Integer;
  begin
    Result := '';
    if fParser.ViewCh <> '&' then Exit;
    fParser.GetCh;
    Result := fParser.GetAnyMult([#9..#255] - [';']);
    if fParser.ViewCh <> ';' then
    begin
      Result := '&' + Result;
      Exit;
    end;
    fParser.GetCh;
    if (Result <> '') and (Result[1] = '#') then
    begin
      Val(Copy(Result, 2, Length(Result)), I, J);
      if J = 0 then
      begin
        //WCh := Chr(I);
        //Result := Chr(I);
        Ch := Chr(I);
        if Ord(Ch) = I then Result := Ch else
          Result := '&' + Result + ';';
        Exit;
      end;
    end;
    if fEntities.IndexOfName(Result) > 0 then
      Result := fEntities.Values[Result]
  end;
begin
  Result := '';
  fParser := TSimpleParser.Create;
  try
    fParser.SetSource(@St);
    repeat
      Result := Result + fParser.GetAnyMult([#9..#255] - ['&']);
      Entity := GetEntity;
      Result := Result + Entity;
    until fParser.Finished;
  finally
    fParser.Free;
  end;
end;

procedure T_SVG_Import.SVGProc(var E: TPrimElement);
var
  X_Size, Y_Size: TRealType;
begin
  if E.Attributes.IndexOfName('viewBox') >= 0 then
    ParseViewBox(E.Attributes.Values['viewBox'],
      viewBox_X, viewBox_Y, viewBox_W, viewBox_H)
  else
  begin
    viewBox_X := 0; viewBox_Y := 0; viewBox_W := 0; viewBox_H := 0;
  end;
  if E.Attributes.IndexOfName('width') >= 0 then
  begin
    X_Size := GetRealTypeAttr(E.Attributes, 'width', viewBox_W, 0.35278);
    if X_Size = 1 then X_Size := viewBox_W;
  end
  else X_Size := viewBox_W;
  if viewBox_W = 0 then viewBox_W := X_Size;
  X_Size := X_Size * 0.35278;
  if E.Attributes.IndexOfName('height') >= 0 then
  begin
    Y_Size := GetRealTypeAttr(E.Attributes, 'height', viewBox_H, 0.35278);
    if Y_Size = 1 then Y_Size := viewBox_H;
  end
  else Y_Size := viewBox_H;
  if viewBox_H = 0 then viewBox_H := Y_Size;
  Y_Size := Y_Size * 0.35278;
  if (E.Attributes.IndexOfName('viewBox') >= 0)
    and (viewBox_W > 0) and (viewBox_H > 0) then
    fScale := Sqrt(X_Size / viewBox_W * Y_Size / viewBox_H)
  else fScale := 0.35278;
  fDrawing2D.PicScale := fScale;
  fDrawing2D.LineWidthBase := fScale;
  //width="8in" height="8in" viewBox="0 0 1 1"
  fState := NewState;
  fState.LineColor := Graphics.clDefault;
  fState.LineWidth := 1;
  fState.LineStyle := liSolid; //liNone;
  fState.Hatching := haNone;
  fState.HatchColor := Graphics.clDefault;
  fState.FillColor := clSilver; //Graphics.clDefault;clBlack;
  fState.T := IdentityTransf2D;
  fState.FontHeight := 10;
  fState.HJustification := jhLeft;
  fState.TextPos := Point2D(0, 0);
  fState.Text2D := nil;
  fInStyle := False;
end;

procedure T_SVG_Import.DescProc(var E: TPrimElement);
begin
  fDrawing2D.Comment := ReplaceEntities(E.Text);
end;

procedure T_SVG_Import.TitleProc(var E: TPrimElement);
begin
  fDrawing2D.Caption := ReplaceEntities(E.Text);
end;

procedure T_SVG_Import.StartGroupProc(var E: TPrimElement);
begin
  StoreAttributes(E.Attributes, 'g');
end;

procedure T_SVG_Import.PathProc(var E: TPrimElement);
var
  PathData: string;
begin
  if E.Attributes.IndexOfName('d') < 0 then Exit;
  StoreAttributes(E.Attributes, 'path');
  PathData := E.Attributes.Values['d'];
  fPathParser.AllAsBezier := SVG_Path_HasCurves(PathData);
  fPathAttributes := E.Attributes;
  fPathParser.Parse(PathData);
  if fPathPoints.Count > 0 then
  begin
    if fPathParser.AllAsBezier then AddBezier
    else AddPolyline;
    fCurrPrimitive.Points.Copy(fPathPoints, 0, fPathPoints.Count - 1);
    fPathPoints.Clear;
    SetAttributes;
  end;
end;

procedure T_SVG_Import.TextStartProc(var E: TPrimElement);
begin
  fState := StoreAttributes(E.Attributes, 'text');
  fState.TextPos := Point2D(
    GetRealTypeAttr(E.Attributes, 'x', 0, fScale)
    + GetRealTypeAttr(E.Attributes, 'dx', 0, fScale),
    GetRealTypeAttr(E.Attributes, 'y', 0, fScale)
    + GetRealTypeAttr(E.Attributes, 'dy', 0, fScale));
  AddText(fState.TextPos.X, fState.TextPos.Y,
    fState.FontHeight, '');
  fState.Text2D := fCurrPrimitive as TText2D;
  (fCurrPrimitive as TText2D).HJustification :=
    fState.HJustification;
end;

procedure T_SVG_Import.TextEndProc(var E: TPrimElement);
begin
  fState := fStack.Peek as T_SVG_Import_State;
  fCurrPrimitive := fState.Text2D;
  fState.Text2D.Text :=
    fState.Text2D.Text + ReplaceEntities(Trim(E.Text));
  SetAttributes;
  fCurrPrimitive.LineColor := fCurrPrimitive.FillColor;
  fCurrPrimitive.FillColor := Graphics.clNone;
  EndStateProc(E);
end;

procedure T_SVG_Import.TSpanStartProc(var E: TPrimElement);
begin
  fState := StoreAttributes(E.Attributes, 'tspan');
  fState.TextPos := Point2D(
    GetRealTypeAttr(E.Attributes, 'x', fState.TextPos.X, fScale)
    + GetRealTypeAttr(E.Attributes, 'dx', 0, fScale),
    GetRealTypeAttr(E.Attributes, 'y', fState.TextPos.Y, fScale)
    + GetRealTypeAttr(E.Attributes, 'dy', 0, fScale));
  AddText(fState.TextPos.X, fState.TextPos.Y,
    fState.FontHeight, '');
  fState.Text2D := fCurrPrimitive as TText2D;
  (fCurrPrimitive as TText2D).HJustification :=
    fState.HJustification;
end;

procedure T_SVG_Import.TSpanEndProc(var E: TPrimElement);
begin
  fState := fStack.Peek as T_SVG_Import_State;
  fCurrPrimitive := fState.Text2D;
  fState.Text2D.Text :=
    fState.Text2D.Text + ReplaceEntities(Trim(E.Text));
  SetAttributes;
  fCurrPrimitive.LineColor := fCurrPrimitive.FillColor;
  fCurrPrimitive.FillColor := Graphics.clNone;
  EndStateProc(E);
end;

procedure T_SVG_Import.LineProc(var E: TPrimElement);
var
  X1, Y1, X2, Y2: TRealType;
begin
  StoreAttributes(E.Attributes, 'line');
  X1 := GetRealTypeAttr(E.Attributes, 'x1', 0, fScale);
  Y1 := GetRealTypeAttr(E.Attributes, 'y1', 0, fScale);
  X2 := GetRealTypeAttr(E.Attributes, 'x2', 0, fScale);
  Y2 := GetRealTypeAttr(E.Attributes, 'y2', 0, fScale);
  Self.fCurrPrimitive := AddLine(X1, Y1, X2, Y2);
  SetAttributes;
end;

procedure T_SVG_Import.RectProc(var E: TPrimElement);
var
  X, Y: TRealType;
begin
  StoreAttributes(E.Attributes, 'rect');
  X := GetRealTypeAttr(E.Attributes, 'x', 0, fScale);
  Y := GetRealTypeAttr(E.Attributes, 'y', 0, fScale);
  Self.fCurrPrimitive := AddRect(
    X, Y,
    X + GetRealTypeAttr(E.Attributes, 'width', 0, fScale),
    Y + GetRealTypeAttr(E.Attributes, 'height', 0, fScale));
//  <rect class="fil1" x="4833" y="5579" width="75" height="44"/>
  SetAttributes;
end;

procedure T_SVG_Import.CircleProc(var E: TPrimElement);
begin
  StoreAttributes(E.Attributes, 'circle');
  Self.fCurrPrimitive := AddCircle(
    GetRealTypeAttr(E.Attributes, 'cx', 0, fScale),
    GetRealTypeAttr(E.Attributes, 'cy', 0, fScale),
    GetRealTypeAttr(E.Attributes, 'r', 0, fScale));
  SetAttributes;
end;

procedure T_SVG_Import.EllipseProc(var E: TPrimElement);
begin
  StoreAttributes(E.Attributes, 'ellipse');
  Self.fCurrPrimitive := AddEllipse(
    GetRealTypeAttr(E.Attributes, 'cx', 0, fScale),
    GetRealTypeAttr(E.Attributes, 'cy', 0, fScale),
    GetRealTypeAttr(E.Attributes, 'rx', 0, fScale),
    GetRealTypeAttr(E.Attributes, 'ry', 0, fScale));
  SetAttributes;
end;

procedure T_SVG_Import.PolygonProc(var E: TPrimElement);
begin
  if E.Attributes.IndexOfName('points') < 0 then Exit;
  StoreAttributes(E.Attributes, 'polygon');
  AddPolygon;
  FillPoints(fCurrPrimitive, E.Attributes.Values['points'],
    fCurrPrimitive.Points);
  SetAttributes;
end;

procedure T_SVG_Import.PolylineProc(var E: TPrimElement);
begin
  StoreAttributes(E.Attributes, 'polyline');
  if E.Attributes.IndexOfName('points') < 0 then Exit;
  Self.fCurrPrimitive := AddPolyline;
  FillPoints(fCurrPrimitive, E.Attributes.Values['points'],
    fCurrPrimitive.Points);
  SetAttributes;
end;

procedure T_SVG_Import.SAX_Comment(const Comment: string);
begin
end;

procedure T_SVG_Import.ParseStyleAttributes(St: string);
var
  fParser: TSimpleParser;
  Name, Value: string;
  procedure TakeSpace;
  begin
    fParser.SkipAnyMult([#32, #13, #10, #9]);
  end;
  procedure GetAttr;
  begin
    Name := '';
    Value := '';
    Name := fParser.GetAnyMult([#9..#255] - [';', ':']);
    if fParser.ViewCh = ':' then
    begin
      fParser.GetCh;
      Value := fParser.GetAnyMult([#9..#255] - [';']);
    end;
    if fParser.ViewCh = ';' then fParser.GetCh;
    Name := Trim(Name);
    Value := Trim(Value);
  end;
begin
  fParser := TSimpleParser.Create;
  try
    fParser.SetSource(@St);
    fStyleAttributes.Clear;
    repeat
      GetAttr;
      if Name <> '' then
      begin
        fStyleAttributes.Add(Name + '=' + Value);
      end;
    until fParser.Finished;
  finally
    fParser.Free;
  end;
end;

function SVG_Color(ClStr: string): TColor;
var
  R, G, B: Integer;
  function GetByte(St: string): Byte;
  var
    Mult: Single;
  begin
    Mult := 1;
    if Pos('%', St) > 0 then
    begin
      Mult := 2.55;
      St := Trim(AnsiReplaceStr(ClStr, '%', ''));
    end;
    try
      Result := Round(StrToFloat(St) * Mult);
    except
      Result := 0;
    end;
  end;
begin
  Result := clSilver;
  ClStr := Trim(ClStr);
  if ClStr = '' then
    Exit;
  if Pos('url(', ClStr) > 0 then
    Exit;
  if Pos('rgb(', ClStr) > 0 then
  begin
    ClStr := AnsiReplaceStr(ClStr, 'rgb', '');
    ClStr := AnsiReplaceStr(ClStr, '(', '');
    ClStr := AnsiReplaceStr(ClStr, ')', '');
    R := GetByte(CSV_Item(ClStr, 1));
    G := GetByte(CSV_Item(ClStr, 2));
    B := GetByte(CSV_Item(ClStr, 3));
    Result := B shl 16 + G shl 8 + R;
    Exit;
  end;
  if (ClStr[1] = '#') and (Length(ClStr) = 4) then
    ClStr := '#' + ClStr[2] + ClStr[2] + ClStr[3] + ClStr[3]
      + ClStr[4] + ClStr[4];
  try
    Result := HtmlToColor(ClStr);
  finally
  end;
end;

function T_SVG_Import.StoreAttributes(const Attributes: TAttributes;
  const Tag: string): T_SVG_Import_State;
var
  St, CurrentColor: string;
  ScaleFactor: TRealType;
  CSS_Style: string;
begin
  Result := NewState;
  if Attributes.IndexOfName('class') >= 0 then
  begin
    CSS_Style := fCSS.Values['.' + Attributes.Values['class']];
    if CSS_Style = '' then
      CSS_Style := fCSS.Values[Tag + '.' + Attributes.Values['class']];
    if CSS_Style = '' then CSS_Style := fCSS.Values[Tag];
    Attributes.Add('style=' + CSS_Style);
  end;
  if Attributes.IndexOfName('style') >= 0 then
  begin
    ParseStyleAttributes(ReplaceEntities(Attributes.Values['style']));
    Attributes.AddStrings(fStyleAttributes);
    Attributes.Delete(Attributes.IndexOfName('style'));
  end;
  if Attributes.IndexOfName('color') >= 0 then
    CurrentColor := Attributes.Values['color']
  else CurrentColor := 'red';
  if Attributes.IndexOfName('fill') >= 0 then
  begin
    if Attributes.Values['fill'] = 'currentColor' then
      Attributes.Values['fill'] := CurrentColor;
    if Attributes.Values['fill'] = 'none' then
    begin
      Result.UpdateHA(haNone);
      Result.UpdateFill(Graphics.clDefault)
    end
    else if Attributes.Values['fill'] = 'currentColor' then
    begin
      Result.UpdateHA(haNone);
      Result.UpdateFill(clLtGray)
    end
    else if Pos('url(', Attributes.Values['fill']) > 0 then
    begin
      Result.UpdateHA(haDiagCross);
      Result.UpdateFill(Graphics.clDefault);
    end
    else
    begin
      Result.UpdateHA(haNone);
      Result.UpdateFill(SVG_Color(Attributes.Values['fill']));
    end;
  end;
  if Attributes.IndexOfName('stroke') >= 0 then
  begin
    if Attributes.Values['stroke'] = 'currentColor' then
      Attributes.Values['stroke'] := CurrentColor;
    if Attributes.Values['stroke'] = 'none'
      then Result.UpdateLI(liNone)
    else if Pos('url(', Attributes.Values['stroke']) > 0 then
    begin
      Result.UpdateLI(liDashed);
      Result.UpdateLC(clLtGray);
    end
    else
    begin
      Result.UpdateLI(liSolid);
      Result.UpdateLC(SVG_Color(Attributes.Values['stroke']));
    end;
    if Attributes.IndexOfName('stroke-dasharray') >= 0 then
      if Attributes.Values['stroke-dasharray'] <> 'none' then
        Result.UpdateLI(liDashed);
  end;
  //if Attributes.IndexOfName('stroke-width') >= 0 then
  begin
    St := Attributes.Values['transform'];
    if St <> '' then
      Result.UpdateT(SVG_Transform_Parse(St));
  end;
  ScaleFactor := IsotropicScale(Result.T);
  Result.UpdateLW(GetRealTypeAttr(Attributes, 'stroke-width', 1, fScale)
    * ScaleFactor);
  if Attributes.IndexOfName('font-size') >= 0 then
    Result.FontHeight :=
      GetRealTypeAttr(Attributes, 'font-size', 3.5278, fScale);
  if Attributes.IndexOfName('text-anchor') >= 0 then
    Result.HJustification := THJustification(CSV_Find(
      'start,middle,end',
      Attributes.Values['text-anchor']) - 1);
end;

procedure T_SVG_Import.SetAttributes;
begin
  if fCurrPrimitive = nil then Exit;
  fState := fStack.Peek as T_SVG_Import_State;
  LC(fState.LineColor);
  LW(fState.LineWidth);
  LI(fState.LineStyle);
  Ha(fState.Hatching);
  Fill(fState.FillColor);
  if not IsSameTransform2D(fState.T, IdentityTransf2D) then
    fCurrPrimitive.TransForm(fState.T);
end;

procedure T_SVG_Import.Scale_LineWidth(const Scale: TRealType; Lst:
  TGraphicObjList);
var
  Tmp: TObject2D;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  TmpIter := Lst.GetExclusiveIterator;
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

procedure T_SVG_Import.Correct_LineWidth;
//Correct libplot line width for SVG
var
  Tmp: TObject2D;
  TmpIter: TExclusiveGraphicObjIterator;
  MinLW: TRealType;
begin
  TmpIter := fLst.GetExclusiveIterator;
  MinLW := 0;
  try
    { Apply trasform to all objects. }
    Tmp := TmpIter.First as TObject2D;
    while Tmp <> nil do
    begin
      if (Tmp is TPrimitive2D) and not (Tmp is TText2D) then
      begin
        if (Tmp as TPrimitive2D).LineWidth > 0 then
          if ((Tmp as TPrimitive2D).LineWidth < MinLW) or (MinLW = 0) then
            MinLW := (Tmp as TPrimitive2D).LineWidth;
      end;
      Tmp := TmpIter.Next as TObject2D;
    end;
  finally
    TmpIter.Free;
  end;
  if (MinLW > 0) and ((MinLW < 0.1) or (MinLW > 10)) then
  begin
    Scale_LineWidth(1 / MinLW, fLst);
    fDrawing2D.LineWidthBase := fDrawing2D.LineWidthBase * MinLW;
  end;
end;

procedure T_SVG_Import.ParseFromStream(const Stream: TStream);
var
  DrawOnAdd0: Boolean;
  T: TTransf2D;
  OnChangeDrawing0: TOnChangeDrawing;
  ScaleLineWidth0: Boolean;
  procedure DeleteLibplotRect;
  var
    TmpObj: TGraphicObject;
  begin
    if fLst.Count = 0 then Exit;
    TmpObj := fLst.Find(0);
    if not (TmpObj is TRectangle2D) then Exit;
    if (TmpObj as TRectangle2D).Points.Count < 3 then Exit;
    if not IsSamePoint2D((TmpObj as TRectangle2D).Points[0], Point2D(0, 0))
      then Exit;
    if not IsSamePoint2D((TmpObj as TRectangle2D).Points[1], Point2D(1, 1))
      then Exit;
    fLst.Delete(0);
  end;
begin
  DrawOnAdd0 := fDrawing2D.DrawOnAdd;
  fDrawing2D.DrawOnAdd := False;
  OnChangeDrawing0 := fDrawing2D.OnChangeDrawing;
  fDrawing2D.OnChangeDrawing := nil;
  ScaleLineWidth0 := ScaleLineWidth;
  ScaleLineWidth := False;
  try
    fSAX.ParseStream(Stream);
    //SetLength(Buffer, Stream.Size); Buffer: string;
    //Stream.ReadBuffer(Buffer[1], Stream.Size);
    //fSAX.ParseString(Buffer);
    DeleteLibplotRect;
    T := MultiplyTransform2D(Flip2D(0, 1),
      Translate2D(0, viewBox_Y + viewBox_H));
    if Abs(fDrawing2D.PicScale / 203.2 - 1) < 1E-4 then
    begin
      {T := MultiplyTransform2D(T, Scale2D(576, 576));
      fDrawing2D.PicScale := 203.2 / 576;
      fDrawing2D.LineWidthBase := 203.2 / 576;
      Scale_LineWidth(576, fLst);}
      T := MultiplyTransform2D(T, Scale2D(203.2, 203.2));
      fDrawing2D.PicScale := 1;
      {fDrawing2D.LineWidthBase := 0.2;
      Scale_LineWidth(203.2 / 0.2, fLst);}
    end;
    Correct_LineWidth;
    fLst.TransForm(T);
    fDrawing2D.AddList(fLst);
  finally
    fDrawing2D.DrawOnAdd := DrawOnAdd0;
    fDrawing2D.OnChangeDrawing := OnChangeDrawing0;
    ScaleLineWidth := ScaleLineWidth0;
  end;
  fDrawing2D.NotifyChanged;
end;

{
ComressStream( aSource, aTarget : TStream; compressionRate : TCompressionLevel );
var comprStream : TCompressionStream;
begin
  // compression level : (clNone, clFastest, clDefault, clMax)
  comprStream := TCompressionStream.Create( compressionRate, aTarget );
 try
  comprStream.CopyFrom( aSource, aSource.Size );
  comprStream.CompressionRate;
 finally
  comprStream.Free;
 End;
End;
}

procedure DecompressStream(inStream, outStream: TStream);
const
  BufferSize = 4096;
var
  Len: Integer;
  ZStream: TDecompressionStream;
  Buffer: array[0..BufferSize - 1] of Byte;
begin
  ZStream := TDecompressionStream.Create(inStream);
  try
    repeat
      Len := ZStream.Read(Buffer, BufferSize);
      outStream.WriteBuffer(Buffer, Len);
    until Len = 0;
  finally
    ZStream.Free;
  end;
end;

procedure T_SVG_Import.ParseFromFile(const FileName: string);
var
  fStream: TFileStream;
  TmpStream: TMemoryStream;
  St: string;
begin
  fStream := TFileStream.Create(FileName, fmOpenRead);
  try
    //if LowerCase(ExtractFileExt(FileName)) = '.svgz' then //Does not work!!!
    if False then
    begin
      TmpStream := TMemoryStream.Create;
      try
        //Does not work!!!
        DecompressStream(fStream, TmpStream);
        ParseFromStream(TmpStream);
      finally
        TmpStream.Free;
      end;
    end
    else ParseFromStream(fStream);
  finally
    fStream.Free;
  end;
end;

procedure Import_Svg(const Drawing: TDrawing2D;
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


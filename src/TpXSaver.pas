unit TpXSaver;

interface

uses GObjBase, GObjects, Drawings, Output, XmlOut, Classes,
  SysUtils,
  Graphics, Solid3D;

// A class for saving TpX drawing

type
  T_TpX_Saver = class(TDrawingSaver)
  protected
    fXML: TXmlOutput;
    procedure WritePrimitiveAttr(Obj: TPrimitive2D);
    procedure WriteArrows(Obj: TPrimitive2D);
    procedure WriteLine2D(Obj: TLine2D); override;
    procedure WriteRectangle2D(Obj: TRectangle2D); override;
    procedure WriteEllipse2D(Obj: TEllipse2D); override;
    procedure WriteCircle2D(Obj: TCircle2D); override;
    procedure WriteCircular2D(Obj: TCircular2D); override;
    procedure WritePoly2D(Obj: TPolyline2D0); override;
    procedure WriteSmooth2D(Obj: TSmoothPath2D0); override;
    procedure WriteBezier2D(Obj: TBezierPath2D0); override;
    procedure WriteText2D(Obj: TText2D); override;
    procedure WriteStar2D(Obj: TStar2D); override;
    procedure WriteSymbol2D(Obj: TSymbol2D); override;
    procedure WriteBitmap2D(Obj: TBitmap2D); override;
    procedure StartGroup2D(Obj: TGroup2D); override;
    procedure FinishGroup2D(Obj: TGroup2D); override;
    procedure WriteCompound2D(Obj: TCompound2D); override;
    procedure WriteSolid3D(Obj: TSolid3D); override;
  public
    procedure WriteSolidElement(Obj: TSolid3D);
    constructor Create(Drawing: TDrawing2D); override;
    destructor Destroy; override;
    procedure WriteHeader; override;
    procedure WriteFooter; override;
    procedure WriteAllToStream; override;
  end;

// The <solid> element of a 3D object as one line of TpX XML, for embedding
// in SVG and TikZ sources.
function SolidToTpXXml(Drawing: TDrawing2D; Solid: TSolid3D;
  const PlacementElsewhere: Boolean = False;
  const WidthElsewhere: Boolean = False): string;


implementation

uses Geometry, Devices, ColorEtc, Math, Pieces;


function FF(const X: TRealType): string;
begin
  Result := Format('%.6g', [X]);
end;

function GetPathString(PP: TPointsSet2D; const EOL: string):
  string;
var
  P: TPoint2D;
  I, Pos, Len: Integer;
  PathStr: PChar;
  procedure WriteSt(const St: string);
  begin
    Move(St[1], PathStr[Pos], Length(St));
    Inc(Pos, Length(St));
  end;
begin
{$IFDEF VER140}
  Len := 20 * PP.Count + (PP.Count div 100) * 2 + 3;
{$ELSE}
  // FPC has a bug in FloatToStrF() function
  Len := 30 * PP.Count + (PP.Count div 100) * 2 + 3;
{$ENDIF}
  GetMem(PathStr, Len);
  Pos := 0;
  try
    for I := 0 to PP.Count - 1 do
    begin
      P := PP[I];
      begin
        if P.X > -1E+38 then WriteSt(FF(P.X))
        else if P.X = gp_MoveTo then WriteSt('M')
        else if P.X = gp_LineTo then WriteSt('L')
        else if P.X = gp_BezierTo then WriteSt('C')
        else if P.X = gp_Close then WriteSt('Z')
        else WriteSt(FF(P.X));
        WriteSt(',');
        WriteSt(FF(P.Y));
      end;
      if (I mod 100) = 88 then WriteSt(EOL);
      if I < PP.Count - 1 then WriteSt(' ');
    end;
    PathStr[Pos] := #0;
    Result := string(PathStr);
  finally
    FreeMem(PathStr, Len);
  end;
end;

{ --================ T_TpX_Saver ==================-- }

constructor T_TpX_Saver.Create(Drawing: TDrawing2D);
begin
  inherited Create(Drawing);
  fXML := TXmlOutput.Create;
  fXML.EOL_Str := EOL + '%';
end;

destructor T_TpX_Saver.Destroy;
begin
  fXML.Free;
  inherited Destroy;
end;

procedure T_TpX_Saver.WriteAllToStream;
var
  St: string;
  PicScale0: TRealType;
begin
  if fStream = nil then Exit;
  fXML.SetStream(fStream, 0);
  St := '%';
  fStream.WriteBuffer(St[1], 1);
  PicScale0 := fDrawing2D.PicScale;
  try
    WriteAll0;
  finally
    fDrawing2D.PicScale := PicScale0;
  end;
  fStream.WriteBuffer(EOL[1], Length(EOL));
end;

function ChoiceToString(Choices: string; Index: Integer):
  string;
var
  I, J: Integer;
begin
  for I := 0 to Index do
  begin
    J := Pos(';', Choices);
    if J = 0 then J := Length(Choices) + 1;
    Result := Copy(Choices, 1, J - 1);
    Delete(Choices, 1, J);
  end;
end;

function XmlReplaceChars(const St: string): string;
var
  I: Integer;
  function ReplaceChar(Ch: Char): string;
  begin
    case Ch of
      '<': Result := '&lt;';
      '>': Result := '&gt;';
      //'&': Result := '&amp;';
    else
      if Ch < #32 then
        Result := '&#' + IntToStr(Ord(Ch)) + ';'
      else
        Result := Ch;
    end;
  end;
begin
  Result := '';
  for I := 1 to Length(St) do
    Result := Result + ReplaceChar(St[I]);
end;

procedure T_TpX_Saver.WriteHeader;
begin
  fXML.OpenTag('TpX');
  fXML.AddAttribute('v', IntToStr(TpX_Format_Version));
  if fDrawing2D.TeXFormat <> tex_eps then
    fXML.AddAttribute('TeXFormat',
      ChoiceToString(TeXFormat_Choice, Ord(fDrawing2D.TeXFormat)));
  if fDrawing2D.PdfTeXFormat <> pdftex_pdf then
    fXML.AddAttribute('PdfTeXFormat',
      ChoiceToString(PdfTeXFormat_Choice,
      Ord(fDrawing2D.PdfTeXFormat)));
  fXML.AddAttribute('ArrowsSize', FF(fDrawing2D.ArrowsSize));
  fXML.AddAttribute('StarsSize', FF(fDrawing2D.StarsSize));
  fXML.AddAttribute('DefaultFontHeight',
    FF(fDrawing2D.DefaultFontHeight));
  if Trim(fDrawing2D.FontName) <> '' then
    fXML.AddAttribute('FontName', fDrawing2D.FontName);
  fXML.AddAttribute('DefaultSymbolSize',
    FF(fDrawing2D.DefaultSymbolSize));
  fXML.AddAttribute('ApproximationPrecision',
    FF(fDrawing2D.ApproximationPrecision));
  fXML.AddAttribute('PicScale', FF(fDrawing2D.PicScale));
  fXML.AddAttribute('Border', FF(fDrawing2D.Border));
  fXML.AddAttribute('BitmapRes', FF(fDrawing2D.BitmapRes));
  fXML.AddAttribute('HatchingStep',
    FF(fDrawing2D.HatchingStep));
  if fDrawing2D.HatchingLineWidth <> HatchingLineWidth_Default then
    fXML.AddAttribute('HatchingLineWidth',
      FF(fDrawing2D.HatchingLineWidth));
  fXML.AddAttribute('DottedSize', FF(fDrawing2D.DottedSize));
  fXML.AddAttribute('DashSize', FF(fDrawing2D.DashSize));
  fXML.AddAttribute('LineWidth', FF(fDrawing2D.LineWidthBase));
  if fDrawing2D.MiterLimit <> 10 then
    fXML.AddAttribute('MiterLimit', FF(fDrawing2D.MiterLimit));
  { Save the value explicitly; the legacy missing-attribute default is True,
    independent of the user's default for new drawings. }
  fXML.AddAttribute('TeXCenterFigure',
    IntToStr(Integer(fDrawing2D.TeXCenterFigure)));
  if fDrawing2D.TeXFigure <> fig_figure then
    fXML.AddAttribute('TeXFigure',
      ChoiceToString(TeXFigure_Choice, Ord(fDrawing2D.TeXFigure)));
  if fDrawing2D.TeXFigurePlacement <> ''
    then fXML.AddAttribute('TeXFigurePlacement',
      fDrawing2D.TeXFigurePlacement);
  if fDrawing2D.TeXFigurePrologue <> ''
    then fXML.AddAttribute('TeXFigurePrologue',
      fDrawing2D.TeXFigurePrologue);
  if fDrawing2D.TeXFigureEpilogue <> ''
    then fXML.AddAttribute('TeXFigureEpilogue',
      fDrawing2D.TeXFigureEpilogue);
  if fDrawing2D.TeXPicPrologue <> ''
    then fXML.AddAttribute('TeXPicPrologue',
      fDrawing2D.TeXPicPrologue);
  if fDrawing2D.TeXPicEpilogue <> ''
    then fXML.AddAttribute('TeXPicEpilogue',
      fDrawing2D.TeXPicEpilogue);
  if fDrawing2D.PicMagnif <> PicMagnif_Default
    then fXML.AddAttribute('PicMagnif',
      FF(fDrawing2D.PicMagnif));
  if fDrawing2D.FontSizeInTeX <> True
    then fXML.AddAttribute('FontSizeInTeX',
      IntToStr(Integer(fDrawing2D.FontSizeInTeX)));
  if fDrawing2D.MetaPostTeXText <> True
    then fXML.AddAttribute('MetaPostTeXText',
      IntToStr(Integer(fDrawing2D.MetaPostTeXText)));
  if fDrawing2D.IncludePath <> ''
    then fXML.AddAttribute('IncludePath',
      fDrawing2D.IncludePath);
  if (fDrawing2D.Caption <> '') or
    (fDrawing2D.FigLabel <> '') then
  begin
    fXML.OpenTag('caption');
    fXML.AddAttribute('label', fDrawing2D.FigLabel);
    fXML.AddText(fDrawing2D.Caption);
    fXML.CloseTag;
  end;
  if fDrawing2D.Comment <> '' then
  begin
    fXML.OpenTag('comment');
    fXML.PreserveSpace := True;
    fXML.AddText(fDrawing2D.Comment);
    fXML.CloseTag;
    fXML.PreserveSpace := False;
  end;
end;

procedure T_TpX_Saver.WriteFooter;
begin
  fXML.CloseTag; // TpX
end;

procedure T_TpX_Saver.WritePrimitiveAttr(Obj: TPrimitive2D);
begin
  if Obj.LineStyle <> liSolid then
    fXML.AddAttribute('li', GetLineStyleString(Obj.LineStyle));
  if Obj.LineWidth <> 1 then
    fXML.AddAttribute('lw', Format('%.2f', [Obj.LineWidth]));
  if Obj.Hatching <> haNone then
    fXML.AddAttribute('ha', IntToStr(Integer(Obj.Hatching)));
  if Obj.LineColor <> clDefault then
    fXML.AddAttribute('lc', ColorToHtml(Obj.LineColor));
  if Obj.HatchColor <> clDefault then
    fXML.AddAttribute('hc', ColorToHtml(Obj.HatchColor));
  if Obj.FillColor <> clDefault then
    fXML.AddAttribute('fill', ColorToHtml(Obj.FillColor));
end;

procedure T_TpX_Saver.WriteArrows(Obj: TPrimitive2D);
begin
  if Obj.BeginArrowKind <> arrNone then
    fXML.AddAttribute('arr1', ArrowsIDs[Ord(Obj.BeginArrowKind)]);
  if Obj.EndArrowKind <> arrNone then
    fXML.AddAttribute('arr2', ArrowsIDs[Ord(Obj.EndArrowKind)]);
  if Obj.ArrowSizeFactor <> 1 then
    fXML.AddAttribute('arrs', FF(Obj.ArrowSizeFactor));
end;

procedure T_TpX_Saver.WriteLine2D(Obj: TLine2D);
var
  P1, P2: TPoint2D;
begin
  fXML.OpenTag('line');
  with Obj do
  begin
    P1 := Points[0];
    fXML.AddAttribute('x1', FF(P1.X));
    fXML.AddAttribute('y1', FF(P1.Y));
    P2 := Points[1];
    fXML.AddAttribute('x2', FF(P2.X));
    fXML.AddAttribute('y2', FF(P2.Y));
    WriteArrows(Obj);
    WritePrimitiveAttr(Obj);
  end;
  fXML.CloseTag;
end;

procedure T_TpX_Saver.WriteRectangle2D(Obj: TRectangle2D);
var
  P0: TPoint2D;
  ARot, W, H: TRealType;
begin
  fXML.OpenTag('rect');
  with Obj do
  begin
    GetRectangleParameters(Points[0], Points[1], Points[2],
      P0, ARot, W, H, 1E-5);
    fXML.AddAttribute('x', FF(P0.X));
    fXML.AddAttribute('y', FF(P0.Y));
    fXML.AddAttribute('w', FF(W));
    fXML.AddAttribute('h', FF(H));
    if Obj.RX <> 0 then
      fXML.AddAttribute('rx', FF(Obj.RX));
    if Obj.RY <> 0 then
      fXML.AddAttribute('ry', FF(Obj.RY));
    if ARot <> 0 then
      fXML.AddAttribute('rotdeg', FF(RadToDeg(ARot)));
    WritePrimitiveAttr(Obj);
  end;
  fXML.CloseTag;
end;

procedure T_TpX_Saver.WriteEllipse2D(Obj: TEllipse2D);
var
  CP: TPoint2D;
  RX, RY, ARot: TRealType;
begin
  fXML.OpenTag('ellipse');
  with Obj do
  begin
    GetEllipseParams(CP, RX, RY, ARot);
    fXML.AddAttribute('x', FF(CP.X));
    fXML.AddAttribute('y', FF(CP.Y));
    fXML.AddAttribute('dx', FF(RX * 2));
    fXML.AddAttribute('dy', FF(RY * 2));
    if ARot <> 0 then
      fXML.AddAttribute('rotdeg', FF(RadToDeg(ARot)));
    WritePrimitiveAttr(Obj);
  end;
  fXML.CloseTag;
end;

procedure T_TpX_Saver.WriteCircle2D(Obj: TCircle2D);
var
  CP: TPoint2D;
begin
  fXML.OpenTag('circle');
  with Obj do
  begin
    CP := Points[0];
    fXML.AddAttribute('x', FF(CP.X));
    fXML.AddAttribute('y', FF(CP.Y));
    fXML.AddAttribute('d',
      FF(PointDistance2D(CP, Points[1]) * 2));
    WritePrimitiveAttr(Obj);
  end;
  fXML.CloseTag;
end;

procedure T_TpX_Saver.WriteCircular2D(Obj: TCircular2D);
var
  CX, CY, R, SA, EA: TRealType;
begin
  if Obj is TArc2D then fXML.OpenTag('arc')
  else if Obj is TSector2D then fXML.OpenTag('sector')
  else if Obj is TSegment2D then fXML.OpenTag('segment');

  begin
    if Obj is TArc2D then WriteArrows(Obj);
    Obj.GetArcParams(CX, CY, R, SA, EA);
    SA := SA - Floor(SA / (2 * Pi)) * 2 * Pi;
    EA := EA - Floor(EA / (2 * Pi)) * 2 * Pi;
    if EA < SA then EA := EA + 2 * Pi;
    fXML.AddAttribute('x', FF(CX));
    fXML.AddAttribute('y', FF(CY));
    fXML.AddAttribute('d', FF(R * 2));
    fXML.AddAttribute('a1', FF(SA));
    fXML.AddAttribute('a2', FF(EA));
    WritePrimitiveAttr(Obj);
  end;
  fXML.CloseTag;
end;

procedure T_TpX_Saver.WritePoly2D(Obj: TPolyline2D0);
begin
  if Obj is TPolygon2D then fXML.OpenTag('polygon')
  else fXML.OpenTag('polyline');
  begin
    if Obj is TPolyline2D then WriteArrows(Obj);
    WritePrimitiveAttr(Obj);
    fXML.PreserveSpace := True;
    fXML.AddText(GetPathString(Obj.Points, fXML.EOL_Str));
  end;
  fXML.CloseTag;
  fXML.PreserveSpace := False;
end;

procedure T_TpX_Saver.WriteSmooth2D(Obj: TSmoothPath2D0);
begin
  fXML.OpenTag('curve');
  begin
    if Obj is TClosedSmoothPath2D
      then fXML.AddAttribute('closed', '1')
    else WriteArrows(Obj);
    WritePrimitiveAttr(Obj);
    fXML.PreserveSpace := True;
    fXML.AddText(GetPathString(Obj.Points, fXML.EOL_Str));
  end;
  fXML.CloseTag;
  fXML.PreserveSpace := False;
end;

procedure T_TpX_Saver.WriteBezier2D(Obj: TBezierPath2D0);
begin
  fXML.OpenTag('bezier');
  begin
    if Obj is TClosedBezierPath2D
      then fXML.AddAttribute('closed', '1')
    else WriteArrows(Obj);
    WritePrimitiveAttr(Obj);
    fXML.PreserveSpace := True;
    fXML.AddText(GetPathString(Obj.Points, fXML.EOL_Str));
  end;
  fXML.CloseTag;
  fXML.PreserveSpace := False;
end;

procedure T_TpX_Saver.WriteText2D(Obj: TText2D);
var
  P: TPoint2D;
  procedure WriteFont;
  var
    Data: TStringList;
    I: Integer;
    ID: string;
  begin
    if Obj.Font.Name = ' ' then Exit;
    fXML.OpenTag('font');
    begin
      fXML.AddAttribute('face', Obj.Font.Name);
      if fsBold in Obj.Font.Style then
        fXML.AddAttribute('bf', '1');
      if fsItalic in Obj.Font.Style then
        fXML.AddAttribute('it', '1');
      fXML.AddAttribute('charset',
        IntToStr(Integer(Obj.Font.Charset)));
    end;
    fXML.CloseTag;
    {Data := TStringList.Create;
    try
      StoreObjectProp(Obj.Font, Data);
      for I := 0 to Data.Count - 1 do
      begin
        ID := Data.Names[I];
        XMLNodeF.AttributeValue[ID] := Data.Values[ID];
      end;
    finally
      Data.Free;
    end;}
  end;
begin
  fXML.OpenTag('text');
  with Obj do
  begin
    P := Points[0];
    fXML.AddAttribute('x', FF(P.X));
    fXML.AddAttribute('y', FF(P.Y));
    fXML.AddAttribute('t', Text);
    if TeXText <> '' then
      fXML.AddAttribute('tex', TeXText);
    fXML.AddAttribute('h', FF(Height));
    case HAlignment of
      //ahLeft: fXML.AddAttribute('ah', 'l'; //default
      ahCenter: fXML.AddAttribute('halign', 'c');
      ahRight: fXML.AddAttribute('halign', 'r');
    end;
    if Rot <> 0 then
      fXML.AddAttribute('rotdeg', FF(RadToDeg(Rot)));
    WritePrimitiveAttr(Obj);
    WriteFont;
  end;
  fXML.CloseTag;
end;

procedure T_TpX_Saver.WriteStar2D(Obj: TStar2D);
var
  P: TPoint2D;
begin
  fXML.OpenTag('star');
  with Obj do
  begin
    P := Points[0];
    fXML.AddAttribute('x', FF(P.X));
    fXML.AddAttribute('y', FF(P.Y));
    if StarKind <> starCircle then
      fXML.AddAttribute('s', StarsIDs[Ord(StarKind)]);
    if StarSizeFactor <> 1 then
      fXML.AddAttribute('d', FF(StarSizeFactor));
    WritePrimitiveAttr(Obj);
  end;
  fXML.CloseTag;
end;

procedure T_TpX_Saver.WriteSymbol2D(Obj: TSymbol2D);
var
  CP: TPoint2D;
begin
  fXML.OpenTag('symbol');
  with Obj do
  begin
    CP := Points[0];
    fXML.AddAttribute('x', FF(CP.X));
    fXML.AddAttribute('y', FF(CP.Y));
    if Rot <> 0 then
      fXML.AddAttribute('rotdeg', FF(RadToDeg(Rot)));
    fXML.AddAttribute('d', FF(Diameter));
    fXML.AddAttribute('s', SymbolsIDs[Ord(SymbolKind)]);
  end;
  WritePrimitiveAttr(Obj);
  fXML.CloseTag;
end;

procedure T_TpX_Saver.WriteBitmap2D(Obj: TBitmap2D);
var
  P: TPoint2D;
  W, H: TRealType;
  Link: string;
begin
  if not Assigned(Obj.BitmapEntry) then
    raise Exception.Create('Bitmap has no image resource');
  fXML.OpenTag('bitmap');
  with Obj do
  begin
    P := GetBitmapParams(W, H, KeepAspectRatio);
    fXML.AddAttribute('x', FF(P.X));
    fXML.AddAttribute('y', FF(P.Y));
    fXML.AddAttribute('w', FF(W));
    fXML.AddAttribute('h', FF(H));
    Link := BitmapEntry.ImageLink;
{$IFDEF CPUWASM32}
    { Browser-selected assets are stored at the virtual root. A downloaded
      source and its asset sit together, so emit a portable relative link. }
    if (Length(Link) > 1) and (Link[1] = '/') and
      (Pos('/', Copy(Link, 2, MaxInt)) = 0) then
      Link := ExtractFileName(Link);
{$ENDIF}
    fXML.AddAttribute('link', Link);
    if not KeepAspectRatio then
      fXML.AddAttribute('keepaspectratio', '0');
  end;
  fXML.CloseTag;
end;

procedure T_TpX_Saver.StartGroup2D(Obj: TGroup2D);
begin
  fXML.OpenTag('group');
end;

procedure T_TpX_Saver.FinishGroup2D(Obj: TGroup2D);
begin
  fXML.CloseTag;
end;

function FD(const X: Double): string;
begin
  Result := Format('%.10g', [X]);
end;

procedure T_TpX_Saver.WriteSolidElement(Obj: TSolid3D);
var
  I, K: Integer;
  Body: TSolidBody;
  Profile: TGraphicObject;
  St: string;
begin
  fXML.OpenTag('solid');
  fXML.AddAttribute('x1', FF(Obj.Points[0].X));
  fXML.AddAttribute('y1', FF(Obj.Points[0].Y));
  fXML.AddAttribute('x2', FF(Obj.Points[1].X));
  fXML.AddAttribute('y2', FF(Obj.Points[1].Y));
  fXML.AddAttribute('az', FD(Obj.Azimuth));
  fXML.AddAttribute('el', FD(Obj.Elevation));
  if Obj.Perspective then fXML.AddAttribute('persp', '1');
  if Obj.Hidden <> shRemoved then
    fXML.AddAttribute('hidden', SolidHiddenIDs[Obj.Hidden]);
  WritePrimitiveAttr(Obj);
  for I := 0 to Obj.BodyCount - 1 do
  begin
    Body := Obj.Bodies[I];
    fXML.OpenTag('body');
    fXML.AddAttribute('kind', SolidKindIDs[Body.Kind]);
    case Body.Kind of
      skBox:
        begin
          fXML.AddAttribute('w', FD(Body.Size1));
          fXML.AddAttribute('d', FD(Body.Size2));
          fXML.AddAttribute('h', FD(Body.Size3));
        end;
      skCylinder, skCone:
        begin
          fXML.AddAttribute('r', FD(Body.Size1));
          fXML.AddAttribute('h', FD(Body.Size2));
        end;
      skSphere: fXML.AddAttribute('r', FD(Body.Size1));
      skTorus:
        begin
          fXML.AddAttribute('r1', FD(Body.Size1));
          fXML.AddAttribute('r2', FD(Body.Size2));
        end;
      skExtrude: fXML.AddAttribute('depth', FD(Body.Size1));
      skRevolve:
        begin
          fXML.AddAttribute('angle', FD(Body.Size1));
          fXML.AddAttribute('start', FD(Body.Size2));
        end;
    end;
    if Body.Kind <> skBox then
      fXML.AddAttribute('segments', IntToStr(Body.SegU));
    if Body.Kind in [skSphere, skTorus] then
      fXML.AddAttribute('rings', IntToStr(Body.SegV));
    if not Body.IsIdentity then
    begin
      St := '';
      for K := 0 to 11 do
      begin
        if K > 0 then St := St + ' ';
        St := St + FD(Body.M[K]);
      end;
      fXML.AddAttribute('m', St);
    end;
    Profile := Body.Profiles.FirstObj;
    while Profile <> nil do
    begin
      WriteEntity(Profile as TObject2D);
      Profile := Body.Profiles.NextObj;
    end;
    fXML.CloseTag;
  end;
  fXML.CloseTag;
end;

procedure T_TpX_Saver.WriteSolid3D(Obj: TSolid3D);
var
  I: Integer;
  Piece: TPiece;
begin
  fXML.OpenTag('group');
  WriteSolidElement(Obj);
  // The rendered faces follow as plain polygons, so TpX versions without
  // 3D objects still show the picture; readers with 3D support skip them.
  for I := 0 to Obj.Pieces.Count - 1 do
  begin
    Piece := Obj.Pieces[I];
    if (Piece = nil) or (Piece.Count < 2) then Continue;
    if Piece.Closed then fXML.OpenTag('polygon')
    else fXML.OpenTag('polyline');
    if Piece.GetLineStyle(Obj) <> liSolid then
      fXML.AddAttribute('li', GetLineStyleString(Piece.GetLineStyle(Obj)));
    if Obj.LineWidth <> 1 then
      fXML.AddAttribute('lw', Format('%.2f', [Obj.LineWidth]));
    if Piece.GetHatching(Obj) <> haNone then
      fXML.AddAttribute('ha', IntToStr(Integer(Piece.GetHatching(Obj))));
    if Obj.LineColor <> clDefault then
      fXML.AddAttribute('lc', ColorToHtml(Obj.LineColor));
    if (Piece.GetHatching(Obj) <> haNone) and (Obj.HatchColor <> clDefault) then
      fXML.AddAttribute('hc', ColorToHtml(Obj.HatchColor));
    if (Piece.Fill = pfiDefault) and (Obj.FillColor <> clDefault) then
      fXML.AddAttribute('fill', ColorToHtml(Obj.FillColor));
    fXML.PreserveSpace := True;
    fXML.AddText(GetPathString(Piece, fXML.EOL_Str));
    fXML.CloseTag;
    fXML.PreserveSpace := False;
  end;
  fXML.CloseTag;
end;

function SolidToTpXXml(Drawing: TDrawing2D; Solid: TSolid3D;
  const PlacementElsewhere, WidthElsewhere: Boolean): string;
var
  Saver: T_TpX_Saver;
  Stream: TStringStream;
  Copy: TSolid3D;
  Origin: TPoint2D;
  Scale, Angle: Double;
begin
  Saver := T_TpX_Saver.Create(Drawing);
  Copy := nil;
  try
    if PlacementElsewhere or WidthElsewhere then
    begin
      Copy := TSolid3D.Create(-1);
      Copy.Assign(Solid);
      // The caller writes the placement as geometry; keep only its angle so
      // this record does not change when the drawing is moved.
      if PlacementElsewhere then
      begin
        Copy.GetPlacement(Origin, Scale, Angle);
        Copy.SetPlacement(Point2D(0, 0), 1, Angle);
      end;
      // Line width is read back from the faces in the reader's units.
      Copy.LineWidth := 1;
      Solid := Copy;
    end;
    Stream := TStringStream.Create('');
    Saver.fXML.EOL_Str := '';
    Saver.fXML.SetStream(Stream, 0);
    Saver.WriteSolidElement(Solid);
    Result := Stream.DataString;
    // SetStream releases the previous stream.
    Saver.fXML.SetStream(nil, 0);
  finally
    Saver.Free;
    Copy.Free;
  end;
end;

procedure T_TpX_Saver.WriteCompound2D(Obj: TCompound2D);
begin
  fXML.OpenTag('compound');
  WritePrimitiveAttr(Obj);
  fXML.PreserveSpace := True;
  fXML.AddText(GetPathString(Obj.Points, fXML.EOL_Str));
  fXML.PreserveSpace := False;
  fXML.CloseTag;
end;

end.


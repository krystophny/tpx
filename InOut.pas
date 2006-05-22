unit InOut;

interface

uses Types, SysUtils, Classes, Windows, Graphics, ComCtrls,
  Variants, CADSys4, CS4BaseTypes, CS4Shapes,
  XUtils, XXmlDom, PdfDoc, Gr32, Gr32_Polygons;

type

  T_CAD_Saver = class(TObject)
  private
    fDrawing2D: TDrawing2D;
    fStream: TStream;
    fW_MM, fH_MM, fExtLeft, fExtBottom, fExtTop,
      fFactorAbs, fFactorW, fFactorH, fFactorMM: TRealType;
  protected
    procedure MeasureDrawing;
    procedure WriteStream(Value: Variant);
    procedure WriteLnStream(Value: Variant);
    procedure WriteStreamPoint0(X, Y: TRealType); virtual;
    procedure WriteStreamPoint0Int(X, Y: Integer);
    function ConvertX(X: TRealType): TRealType; virtual;
    function ConvertY(Y: TRealType): TRealType; virtual;
    function ConvertPnt(Pnt: TPoint2D): TPoint2D;
    function ConvertPntInv(Point: TPoint2D): TPoint2D; virtual;
    procedure WriteStreamPoint(Pnt: TPoint2D); virtual;
    procedure WriteLine2D(Obj: TLine2D); virtual; abstract;
    procedure WriteRectangle2D(Obj: TRectangle2D); virtual;
      abstract;
    procedure WriteEllipse2D(Obj: TEllipse2D); virtual;
      abstract;
    procedure WriteCircle2D(Obj: TCircle2D); virtual;
      abstract;
    procedure WriteCircular2D(Obj: TCircular2D); virtual;
      abstract;
    procedure WriteSpline2D(Obj: TSpline2D0); virtual;
      abstract;
    procedure WritePoly2D(Obj: TPolyline2D0); virtual;
      abstract;
    procedure WriteText2D(Obj: TText2D); virtual;
      abstract;
    procedure WriteStar2D(Obj: TStar2D); virtual;
      abstract;
    procedure WriteSmooth2D(Obj: TSmoothPath2D0); virtual;
      abstract;
    procedure WriteEntity(Obj: TObject2D);
  public
    constructor Create(CAD: TDrawing2D); virtual;
    procedure WriteEntities;
    function GetPathString(PP: TPointsSet2D): string;
    procedure WriteHeader; virtual;
    procedure WriteFooter; virtual;
    procedure WriteAll; virtual;
    procedure WriteAllToStream; virtual;
    procedure WriteToTpX(Stream: TStream;
      const FileName: string); virtual;
    procedure StoreToFile(const FileName: string); virtual;
  end;

  T_CADSaverClass = class of T_CAD_Saver;

  T_TpX_Saver = class(T_CAD_Saver)
  private
    fXML: TXMLDDocument;
  protected
    procedure WritePrimitiveAttr(Obj: TPrimitive2D;
      XMLNode: TXMLDElement);
    procedure WriteLine2D(Obj: TLine2D); override;
    procedure WriteRectangle2D(Obj: TRectangle2D); override;
    procedure WriteEllipse2D(Obj: TEllipse2D); override;
    procedure WriteCircle2D(Obj: TCircle2D); override;
    procedure WriteCircular2D(Obj: TCircular2D); override;
    procedure WriteSpline2D(Obj: TSpline2D0); override;
    procedure WritePoly2D(Obj: TPolyline2D0); override;
    procedure WriteText2D(Obj: TText2D); override;
    procedure WriteStar2D(Obj: TStar2D); override;
    procedure WriteSmooth2D(Obj: TSmoothPath2D0); override;
  public
    constructor Create(CAD: TDrawing2D); override;
    destructor Destroy; override;
    procedure WriteHeader; override;
    procedure WriteAll; override;
    procedure WriteAllToStream; override;
    procedure StoreToClipboard;
  end;

  T_SVG_Export = class(T_CAD_Saver)
  private
    fXML: TXMLDDocument;
    fMagnif: TRealType;
  protected
    function ConvertY(Y: TRealType): TRealType; override;
    procedure WritePatterns(DefsNode: TXMLDElement);
    procedure WritePrimitiveAttr(Obj: TPrimitive2D;
      XMLNode: TXMLDElement);
    procedure WriteLine2D(Obj: TLine2D); override;
    procedure WriteRectangle2D(Obj: TRectangle2D); override;
    procedure WriteEllipse2D(Obj: TEllipse2D); override;
    procedure WriteCircle2D(Obj: TCircle2D); override;
    procedure WriteCircular2D(Obj: TCircular2D); override;
    procedure WriteSpline2D(Obj: TSpline2D0); override;
    procedure WriteBSpline2D(Obj: TBSpline2D);
    procedure WriteClosedBSpline2D(Obj: TClosedBSpline2D);
    procedure WriteCubicSpline2D(Obj: TSpline2D0);
    procedure WritePoly2D(Obj: TPolyline2D0); override;
    procedure WriteText2D(Obj: TText2D); override;
    procedure WriteStar2D(Obj: TStar2D); override;
    procedure WriteSmooth2D(Obj: TSmoothPath2D0); override;
  public
    constructor Create(CAD: TDrawing2D); override;
    destructor Destroy; override;
    procedure WriteHeader; override;
    function GetX(X: TRealType): Integer;
    function GetY(Y: TRealType): Integer;
    procedure WriteAll; override;
    procedure WriteAllToStream; override;
    procedure StoreToClipboard;
  end;

  T_PostScript_Export = class(T_CAD_Saver)
    fHatchingStep: TRealType;
  protected
    procedure WriteStreamPoint0(X, Y: TRealType); override;
    procedure WriteLineThickness(Kind: TLineKind);
    procedure WriteLine(P0, P1: TPoint2D; Kind: TLineKind);
    procedure WritePoly(PP: TPointsSet2D;
      Kind: TLineKind; Closed: Boolean);
    procedure WriteFont;
    procedure WriteLine2D(Obj: TLine2D); override;
    procedure WriteHatching(const P: TPointsSet2D;
      Hatching: THatching; Step: TRealType);
    procedure WriteRectangle2D(Obj: TRectangle2D); override;
    procedure WriteEllipse2D(Obj: TEllipse2D); override;
    procedure WriteCircle2D(Obj: TCircle2D); override;
    procedure WriteCircular2D(Obj: TCircular2D); override;
    procedure WriteSpline2D(Obj: TSpline2D0); override;
    procedure WriteBezierPath(const PP: TPointsSet2D;
      Kind: TLineKind);
    procedure WritePoly2D(Obj: TPolyline2D0); override;
    procedure WriteText2D(Obj: TText2D); override;
    procedure WriteStar2D(Obj: TStar2D); override;
    procedure WriteSmooth2D(Obj: TSmoothPath2D0); override;
  public
    FontName: string;
    constructor Create(CAD: TDrawing2D); override;
    procedure WriteHeader; override;
    procedure WriteFooter; override;
    procedure WriteToTpX(Stream: TStream;
      const FileName: string); override;
    procedure StoreToClipboard;
  end;

  T_TeX_Picture_Export = class(T_CAD_Saver)
  private
    fH, fW, fUnitLength, fHatchingStep: TRealType;
  protected
    procedure WriteStreamPoint0(X, Y: TRealType); override;
    procedure WriteLineThickness(Obj: TPrimitive2D);
    procedure WriteLine(P0, P1: TPoint2D; Kind: TLineKind);
    procedure WriteLine2D(Obj: TLine2D); override;
    procedure WriteHatching(const P: TPointsSet2D;
      Hatching: THatching; Step: TRealType);
    procedure WriteRectangle2D(Obj: TRectangle2D); override;
    procedure WriteEllipse2D(Obj: TEllipse2D); override;
    procedure WriteCircle2D(Obj: TCircle2D); override;
    procedure WriteCircular2D(Obj: TCircular2D); override;
    procedure WriteSpline2D(Obj: TSpline2D0); override;
    procedure WriteBSpline2D(Obj: TBSpline2D);
    procedure WriteClosedBSpline2D(Obj: TClosedBSpline2D);
    procedure WriteCubicSpline2D(Obj: TSpline2D0);
    procedure WritePoly2D(Obj: TPolyline2D0); override;
    procedure WriteText2D(Obj: TText2D); override;
    procedure WriteStar2D(Obj: TStar2D); override;
    procedure WriteSmooth2D(Obj: TSmoothPath2D0); override;
  public
    constructor Create(CAD: TDrawing2D); override;
    procedure WriteHeader; override;
    procedure WriteFooter; override;
    procedure StoreToClipboard;
  end;

  T_PSTricks_Export = class(T_CAD_Saver)
  private
    fH, fW, fUnitLength, fHatchingStep: TRealType;
  protected
    procedure WriteStreamPoint0(X, Y: TRealType); override;
    function LineArg(Kind: TLineKind): string;
    procedure WriteLine(P0, P1: TPoint2D; Kind: TLineKind);
    procedure WritePoly(PP: TPointsSet2D;
      Kind: TLineKind; Closed: Boolean);
    procedure WriteBezier(const P0, P1, P2, P3: TPoint2D;
      Kind: TLineKind);
    procedure WriteBezierPath(const PP: TPointsSet2D;
      Kind: TLineKind);
    procedure WriteLine2D(Obj: TLine2D); override;
    procedure WriteHatching(const P: TPointsSet2D;
      Hatching: THatching; Step: TRealType);
    procedure WriteRectangle2D(Obj: TRectangle2D); override;
    procedure WriteEllipse2D(Obj: TEllipse2D); override;
    procedure WriteCircle2D(Obj: TCircle2D); override;
    procedure WriteCircular2D(Obj: TCircular2D); override;
    procedure WriteSpline2D(Obj: TSpline2D0); override;
    procedure WritePoly2D(Obj: TPolyline2D0); override;
    procedure WriteText2D(Obj: TText2D); override;
    procedure WriteStar2D(Obj: TStar2D); override;
    procedure WriteSmooth2D(Obj: TSmoothPath2D0); override;
  public
    constructor Create(CAD: TDrawing2D); override;
    procedure WriteHeader; override;
    procedure WriteFooter; override;
    procedure StoreToClipboard;
  end;

  T_MetaPost_Export = class(T_CAD_Saver)
    fHatchingStep: TRealType;
  protected
    procedure WriteStreamPoint0(X, Y: TRealType); override;
    procedure WriteLineThickness(Kind: TLineKind);
    procedure WriteLine(P0, P1: TPoint2D; Kind: TLineKind);
    procedure WritePoly(PP: TPointsSet2D;
      Kind: TLineKind; Closed: Boolean);
    procedure WriteLine2D(Obj: TLine2D); override;
    procedure WriteHatching(const P: TPointsSet2D;
      Hatching: THatching; Step: TRealType);
    procedure WriteRectangle2D(Obj: TRectangle2D); override;
    procedure WriteEllipse2D(Obj: TEllipse2D); override;
    procedure WriteCircle2D(Obj: TCircle2D); override;
    procedure WriteCircular2D(Obj: TCircular2D); override;
    procedure WriteSpline2D(Obj: TSpline2D0); override;
    procedure WriteBezierPath(const PP: TPointsSet2D;
      Kind: TLineKind; Closed: Boolean);
    procedure WritePoly2D(Obj: TPolyline2D0); override;
    procedure WriteText2D(Obj: TText2D); override;
    procedure WriteStar2D(Obj: TStar2D); override;
    procedure WriteSmooth2D(Obj: TSmoothPath2D0); override;
  public
    procedure WriteHeader; override;
    procedure WriteFooter; override;
    procedure WriteToTpX(Stream: TStream;
      const FileName: string); override;
    procedure StoreToClipboard;
  end;

  T_PDF_Export = class(T_CAD_Saver)
  private
    fPDF: TPdfDoc;
    fHatchingStep: TRealType;
    procedure WriteLineKind(Kind: TLineKind);
    procedure WritePoly(PP: TPointsSet2D;
      Kind: TLineKind; Closed: Boolean);
    procedure WriteBezierPath(const PP: TPointsSet2D;
      Kind: TLineKind; Closed: Boolean);
    procedure WriteHatching(const P: TPointsSet2D;
      Hatching: THatching; Step: TRealType);
  protected
    procedure WriteLine2D(Obj: TLine2D); override;
    procedure WriteRectangle2D(Obj: TRectangle2D); override;
    procedure WriteEllipse2D(Obj: TEllipse2D); override;
    procedure WriteCircle2D(Obj: TCircle2D); override;
    procedure WriteCircular2D(Obj: TCircular2D); override;
    procedure WriteSpline2D(Obj: TSpline2D0); override;
    procedure WritePoly2D(Obj: TPolyline2D0); override;
    procedure WriteText2D(Obj: TText2D); override;
    procedure WriteStar2D(Obj: TStar2D); override;
    procedure WriteSmooth2D(Obj: TSmoothPath2D0); override;
  public
    constructor Create(CAD: TDrawing2D); override;
    destructor Destroy; override;
    procedure WriteHeader; override;
    procedure WriteAllToStream; override;
    procedure WriteToTpX(Stream: TStream;
      const FileName: string); override;
  end;

  T_Bitmap0_Export = class(T_CAD_Saver)
  private
    fBitmap: TBitmap32;
    fPolygon, fOutline: TPolygon32;
    fHatchingStep: TRealType;
    procedure WriteLine(P0, P1: TPoint2D; W: TRealType);
    procedure WriteBitmapPolygon(Polygon: TPolygon32);
    procedure FillPolygon(PP: TPointsSet2D; Closed: Boolean);
    procedure WritePoly(PP: TPointsSet2D;
      Kind: TLineKind; Closed: Boolean);
    procedure WriteHatching(const P: TPointsSet2D;
      Hatching: THatching; Step: TRealType);
  protected
    function ConvertY(Y: TRealType): TRealType; override;
    procedure WriteLine2D(Obj: TLine2D); override;
    procedure WriteRectangle2D(Obj: TRectangle2D); override;
    procedure WriteEllipse2D(Obj: TEllipse2D); override;
    procedure WriteCircle2D(Obj: TCircle2D); override;
    procedure WriteCircular2D(Obj: TCircular2D); override;
    procedure WriteSpline2D(Obj: TSpline2D0); override;
    procedure WritePoly2D(Obj: TPolyline2D0); override;
    procedure WriteText2D(Obj: TText2D); override;
    procedure WriteStar2D(Obj: TStar2D); override;
    procedure WriteSmooth2D(Obj: TSmoothPath2D0); override;
  public
    constructor Create(CAD: TDrawing2D); override;
    destructor Destroy; override;
    procedure WriteHeader; override;
    procedure WriteAll; override;
    procedure WriteAllToStream; override;
    procedure StoreToFile(const FileName: string); override;
  end;

  T_Bitmap_Export = class(T_Bitmap0_Export)
    procedure WriteToTpX(Stream: TStream;
      const FileName: string); override;
  end;

  T_PNG_Export = class(T_Bitmap0_Export)
  public
    procedure WriteToTpX(Stream: TStream;
      const FileName: string); override;
    procedure StoreToFile(const FileName: string); override;
  end;

  T_EMF_Export = class(T_CAD_Saver)
  public
    procedure WriteToTpX(Stream: TStream;
      const FileName: string); override;
    procedure StoreToFile(const FileName: string); override;
  end;

  T_EpsToPdf_Export = class(T_CAD_Saver)
  public
    procedure WriteToTpX(Stream: TStream;
      const FileName: string); override;
    procedure StoreToFile(const FileName: string); override;
  end;

  T_TpX_Loader = class(TObject)
  private
    fStream: TStream;
    fDrawing2D: TDrawing2D;
    fViewport2D: TCADViewport2D;
    fXML: TXMLDDocument;
    fVersion: Single;
  protected
    procedure ReadPrimitiveAttr(Obj: TPrimitive2D;
      XMLNode: TXMLDElement);
    function ReadLine(XMLNode: TXMLDElement): TPrimitive2D;
    function ReadRect(XMLNode: TXMLDElement): TPrimitive2D;
    function ReadText(XMLNode: TXMLDElement): TPrimitive2D;
    function ReadEllipse(XMLNode: TXMLDElement): TPrimitive2D;
    function ReadCircle(XMLNode: TXMLDElement): TPrimitive2D;
    function ReadEllArc(XMLNode: TXMLDElement): TPrimitive2D;
    function ReadCircular(XMLNode: TXMLDElement;
      TheClass: TPrimitive2DClass): TPrimitive2D;
    function ReadSpline(XMLNode: TXMLDElement): TPrimitive2D;
    function ReadSmooth(XMLNode: TXMLDElement): TPrimitive2D;
    function ReadPolygon(XMLNode: TXMLDElement): TPrimitive2D;
    function ReadPolyline(XMLNode: TXMLDElement): TPrimitive2D;
    function ReadStar(XMLNode: TXMLDElement): TPrimitive2D;
    function ReadEntity(XMLNode: TXMLDElement): TPrimitive2D;
  public
    constructor Create(CAD: TDrawing2D;
      Viewport2D: TCADViewport2D);
    destructor Destroy; override;
    procedure ReadHeader;
    procedure ReadEntities;
    procedure ReadAll;
    procedure LoadFromStream;
    procedure LoadFromClipboard;
    procedure LoadFromFile(const FileName: string);
    property XMLDoc: TXMLDDocument read fXML;
  end;

const
  EOL = #13#10;

procedure StoreToFile_Saver(const CAD: TDrawing2D;
  const FileName: string; AClass: T_CADSaverClass);
procedure StoreToFile_TpX(const CAD: TDrawing2D;
  const FileName: string;
  AClass_TeX, AClass_PdfTeX: T_CADSaverClass);
procedure StoreToFile_MPS(const CAD: TDrawing2D;
  const FileName: string);
procedure StoreToFile_EpsToPdf(const CAD: TDrawing2D;
  const FileName: string);
function FindPicturePath(TeXFileName: string; Line: Integer):
  string;
procedure pfb2pfa(const FileName_pfb, FileName_pfa: string);

var
  MetaPostPath: string = 'mpost.exe';
  Font_pfb_Path: string = '';
  EpsToPdfPath: string = 'epstopdf.exe';

implementation

uses Math, Dialogs, Forms, Clipbrd, StrUtils, MainUnit,
  PdfTypes, pngimage;

{ --================ T_CAD_Saver ==================-- }

constructor T_CAD_Saver.Create(CAD: TDrawing2D);
begin
  inherited Create;
  fDrawing2D := CAD;
  fStream := nil;
end;

procedure T_CAD_Saver.MeasureDrawing;
var
  ARect: TRect2D;
  B: TRealType;
begin
  B := fDrawing2D.Border;
  fW_MM := fDrawing2D.PicWidth - B * 2;
  fH_MM := fDrawing2D.PicHeight - B * 2;
  ARect := fDrawing2D.DrawingExtension;
  with ARect do
  begin
    if (Top <> Bottom) and (Right <> Left) then
    begin
      fW_MM := Min((Right - Left)
        / (Top - Bottom) * fH_MM, fW_MM);
      fH_MM := Min((Top - Bottom)
        / (Right - Left) * fW_MM, fH_MM);
      fFactorW := fW_MM / (Right - Left);
      fFactorH := fH_MM / (Top - Bottom);
      fFactorAbs := Sqrt(fFactorW * fFactorH);
      fFactorW := fFactorW * fFactorMM;
      fFactorH := fFactorH * fFactorMM;
      fExtLeft := Left - B / fFactorAbs;
      fExtBottom := Bottom - B / fFactorAbs;
      fExtTop := Top + B / fFactorAbs;
      fW_MM := fW_MM + B * 2;
      fH_MM := fH_MM + B * 2;
    end
    else
    begin
      fExtLeft := Left;
      fExtBottom := Bottom;
      fExtTop := Top;
    end;
  end;
end;

procedure T_CAD_Saver.WriteStream(Value: Variant);
var
  ValueSt: string;
begin
  ValueSt := Value;
  if ValueSt = '' then Exit;
  fStream.Write(ValueSt[1], Length(ValueSt));
end;

procedure T_CAD_Saver.WriteLnStream(Value: Variant);
var
  ValueSt: string;
begin
  ValueSt := Value;
  ValueSt := ValueSt + EOL;
  fStream.Write(ValueSt[1], Length(ValueSt));
end;

procedure T_CAD_Saver.WriteStreamPoint0(X, Y: TRealType);
begin
  WriteStream('(');
  WriteStream(X);
  WriteStream(',');
  WriteStream(Y);
  WriteStream(')');
end;

procedure T_CAD_Saver.WriteStreamPoint0Int(X, Y: Integer);
begin
  WriteStream('(');
  WriteStream(X);
  WriteStream(',');
  WriteStream(Y);
  WriteStream(')');
end;

function T_CAD_Saver.ConvertX(X: TRealType): TRealType;
begin
  Result := (X - fExtLeft) * fFactorW;
end;

function T_CAD_Saver.ConvertY(Y: TRealType): TRealType;
begin
  Result := (Y - fExtBottom) * fFactorH;
end;

function T_CAD_Saver.ConvertPnt(Pnt: TPoint2D):
  TPoint2D;
begin
  Result := Point2D(ConvertX(Pnt.X), ConvertY(Pnt.Y));
end;

function T_CAD_Saver.ConvertPntInv(Point: TPoint2D):
  TPoint2D;
begin
  Result := Point2D(Point.X / fFactorW + fExtLeft,
    Point.Y / fFactorH + fExtBottom);
end;

procedure T_CAD_Saver.WriteStreamPoint(Pnt: TPoint2D);
var
  Point: TPoint2D;
begin
  Point := ConvertPnt(Pnt);
  WriteStreamPoint0(Point.X, Point.Y);
end;

procedure T_CAD_Saver.WriteEntities;
var
  TmpIter: TGraphicObjIterator;
  TmpObj: TObject2D;
begin
  TmpIter := fDrawing2D.ObjectsIterator;
  try
    TmpObj := TmpIter.First as TObject2D;
    while TmpObj <> nil do
    begin
      WriteEntity(TmpObj);
      TmpObj := TmpIter.Next as TObject2D;
    end;
  finally
    TmpIter.Free;
  end;
end;

procedure T_CAD_Saver.WriteEntity(Obj: TObject2D);
begin
//try
  if Obj is TLine2D then
  begin
    WriteLine2D(Obj as TLine2D);
  end
  else if Obj is TEllipse2D then
  begin
    WriteEllipse2D(Obj as TEllipse2D);
  end
  else if Obj is TRectangle2D then
  begin
    WriteRectangle2D(Obj as TRectangle2D);
  end
  else if Obj is TCircle2D then
  begin
    WriteCircle2D(Obj as TCircle2D);
  end
  else if Obj is TCircular2D then
  begin
    WriteCircular2D(Obj as TCircular2D);
  end
  else if Obj is TSpline2D0 then
  begin
    WriteSpline2D(Obj as TSpline2D0);
  end
  else if Obj is TSmoothPath2D0 then
  begin
    WriteSmooth2D(Obj as TSmoothPath2D0);
  end
  else if Obj is TStar2D then
  begin
    WriteStar2D(Obj as TStar2D);
  end
  else if Obj is TPolyline2D0 then
  begin
    WritePoly2D(Obj as TPolyline2D0);
  end
  else if Obj is TText2D then
  begin
    WriteText2D(Obj as TText2D);
  end
    ;
 {except
 showmessage(Obj.ClassName);
 end;}
end;

function FF(const X: TRealType): string;
begin
  Result := Format('%.6g', [X]);
end;

function T_CAD_Saver.GetPathString(PP: TPointsSet2D): string;
var
  P: TPoint2D;
  I: Integer;
begin
  Result := '';
  for I := 0 to PP.Count - 1 do
  begin
    P := PP[I];
    Result := Result + FF(P.X) + ',' + FF(P.Y);
    if I < PP.Count - 1 then Result := Result + ' ';
  end;
end;

procedure T_CAD_Saver.WriteHeader;
begin

end;

procedure T_CAD_Saver.WriteFooter;
begin

end;

procedure T_CAD_Saver.WriteAll;
begin
  if fDrawing2D = nil then Exit;
  WriteHeader;
  WriteEntities;
  WriteFooter;
end;

procedure T_CAD_Saver.WriteAllToStream;
begin
  if fStream = nil then Exit;
  WriteAll;
end;

procedure T_CAD_Saver.WriteToTpX(Stream: TStream;
  const FileName: string);
var
  Stream0: TStream;
begin
  Stream0 := fStream;
  fStream := Stream;
  WriteAllToStream;
  fStream := Stream0;
end;

procedure T_CAD_Saver.StoreToFile(const FileName: string);
begin
  fStream.Free;
  fStream := TFileStream.Create(FileName, fmCreate);
  try
    WriteAllToStream;
  finally
    fStream.Free;
    fStream := nil;
  end;
end;

procedure StoreToFile_Saver(const CAD: TDrawing2D;
  const FileName: string; AClass: T_CADSaverClass);
var
  Saver: T_CAD_Saver;
begin
  Saver := AClass.Create(CAD);
  try
    Saver.StoreToFile(FileName);
  finally
    Saver.Free;
  end;
end;

procedure StoreToFile_TpX(const CAD: TDrawing2D;
  const FileName: string;
  AClass_TeX, AClass_PdfTeX: T_CADSaverClass);
var
  Stream: TFileStream;
  procedure WriteAsClass(AClass: T_CADSaverClass);
  var
    Saver: T_CAD_Saver;
  begin
    Saver := AClass.Create(CAD);
    try
      Saver.fStream := Stream;
      Saver.WriteToTpX(Stream, FileName);
    finally
      Saver.Free;
    end;
  end;
  procedure WriteLnStream(Value: Variant);
  var
    ValueSt: string;
  begin
    ValueSt := Value;
    ValueSt := ValueSt + EOL;
    Stream.Write(ValueSt[1], Length(ValueSt));
  end;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    WriteAsClass(T_TpX_Saver);
    if CAD.TeXFigurePrologue <> '' then
      WriteLnStream(CAD.TeXFigurePrologue);
    case CAD.TeXFigure of
      fig_figure:
        if CAD.TeXFigurePlacement <> '' then
          WriteLnStream(Format('\begin{figure}[%s]',
            [CAD.TeXFigurePlacement]))
        else
          WriteLnStream('\begin{figure}');
      fig_floating:
        if CAD.TeXFigurePlacement <> '' then
          WriteLnStream(Format(
            '\begin{floatingfigure}[%s]{%d mm}',
            [CAD.TeXFigurePlacement, Ceil(CAD.PicWidth)]))
        else
          WriteLnStream(Format(
            '\begin{floatingfigure}{%d mm}',
            [Ceil(CAD.PicWidth)]));
      fig_wrap:
        if CAD.TeXFigurePlacement <> '' then
          WriteLnStream(Format('\begin{wrapfigure}{%s}{%d mm}',
            [CAD.TeXFigurePlacement, Ceil(CAD.PicWidth)]))
        else
          WriteLnStream(Format('\begin{wrapfigure}{r}{%d mm}',
            [Ceil(CAD.PicWidth)]));
    end;
    if CAD.TeXCenterFigure then
      WriteLnStream('\begin{center}');
    if CAD.TeXPicPrologue <> '' then
      WriteLnStream(CAD.TeXPicPrologue);
    if AClass_PdfTeX <> AClass_TeX then
    begin
      WriteLnStream('\ifx\pdftexversion\undefined');
      WriteAsClass(AClass_TeX);
      WriteLnStream('\else');
      WriteAsClass(AClass_PdfTeX);
      WriteLnStream('\fi');
    end
    else WriteAsClass(AClass_TeX);
    if CAD.TeXPicEpilogue <> '' then
      WriteLnStream(CAD.TeXPicEpilogue);
    if (CAD.Caption <> '') or (CAD.FigLabel <> '') then
    begin
      if CAD.FigLabel <> '' then
        WriteLnStream(Format('\caption{\label{%s}%s}',
          [CAD.FigLabel, CAD.Caption]))
      else
        WriteLnStream(Format('\caption{%s}', [CAD.Caption]));
    end;
    if CAD.TeXCenterFigure then
      WriteLnStream('\end{center}');
    case CAD.TeXFigure of
      fig_figure:
        WriteLnStream('\end{figure}');
      fig_floating:
        WriteLnStream('\end{floatingfigure}');
      fig_wrap:
        WriteLnStream('\end{wrapfigure}');
    end;
    if CAD.TeXFigureEpilogue <> '' then
      WriteLnStream(CAD.TeXFigureEpilogue);
  finally
    Stream.Free;
  end;
end;

{ --================ T_TpX_Saver ==================-- }

constructor T_TpX_Saver.Create(CAD: TDrawing2D);
begin
  inherited Create(CAD);
  RO_Init(fXML, TXMLDDocument.Create);
end;

destructor T_TpX_Saver.Destroy;
begin
  RO_Free(fXML);
  inherited Destroy;
end;

procedure T_TpX_Saver.WriteAll;
begin
  fXML.LoadXML('');
  inherited WriteAll;
end;

procedure T_TpX_Saver.WriteAllToStream;
var
  I: Integer;
  Lines: TStringList;
  procedure WriteStream(ValueSt: string);
  begin
    fStream.Write(ValueSt[1], Length(ValueSt));
  end;
begin
  if fStream = nil then Exit;
  WriteAll;
  Lines := TStringList.Create;
  Lines.Text := fXML.XML;
  for I := 0 to Lines.Count - 1 do
    WriteStream('%' + Lines[I] + EOL);
  Lines.Free;
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

procedure T_TpX_Saver.WriteHeader;
var
  Rect: TRect2D;
begin
  Rect := fDrawing2D.DrawingExtension;
  fXML.LoadXML('<TpX/>');
  with fXML.DocumentElement do
  begin
    AttributeValue['v'] := 1;
    with Rect do
    begin
      AttributeValue['l'] := FF(Left);
      AttributeValue['t'] := FF(Top);
      AttributeValue['r'] := FF(Right);
      AttributeValue['b'] := FF(Bottom);
      if fDrawing2D.TeXFormat <> tex_tex then
        AttributeValue['TeXFormat'] :=
          ChoiceToString(TeXFormat_Choice,
          Ord(fDrawing2D.TeXFormat));
      if fDrawing2D.PdfTeXFormat <> pdftex_tex then
        AttributeValue['PdfTeXFormat'] :=
          ChoiceToString(PdfTeXFormat_Choice,
          Ord(fDrawing2D.PdfTeXFormat));
      AttributeValue['ArrowsSize'] := FF(fDrawing2D.ArrowsSize);
      AttributeValue['StarsSize'] := FF(fDrawing2D.StarsSize);
      AttributeValue['DefaultFontHeight'] :=
        FF(fDrawing2D.DefaultFontHeight);
      AttributeValue['PicWidth'] := fDrawing2D.PicWidth;
      AttributeValue['PicHeight'] := fDrawing2D.PicHeight;
      AttributeValue['Border'] := fDrawing2D.Border;
      AttributeValue['PicUnitLength'] :=
        FF(fDrawing2D.PicUnitLength);
      AttributeValue['HatchingStep'] :=
        FF(fDrawing2D.HatchingStep);
      AttributeValue['DottedSize'] := fDrawing2D.DottedSize;
      AttributeValue['DashSize'] := fDrawing2D.DashSize;
      AttributeValue['TeXMinLine'] := FF(fDrawing2D.TeXMinLine);
      AttributeValue['LineWidth'] := FF(fDrawing2D.LineWidth);
      if fDrawing2D.TeXCenterFigure <> TeXCenterFigure_Default
        then AttributeValue['TeXCenterFigure'] :=
        fDrawing2D.TeXCenterFigure;
      if fDrawing2D.TeXFigure <> fig_figure then
        AttributeValue['TeXFigure'] :=
          ChoiceToString(TeXFigure_Choice,
          Ord(fDrawing2D.TeXFigure));
      if fDrawing2D.TeXFigurePlacement <> ''
        then AttributeValue['TeXFigurePlacement'] :=
        fDrawing2D.TeXFigurePlacement;
      if fDrawing2D.TeXFigurePrologue <> ''
        then AttributeValue['TeXFigurePrologue'] :=
        fDrawing2D.TeXFigurePrologue;
      if fDrawing2D.TeXFigureEpilogue <> ''
        then AttributeValue['TeXFigureEpilogue'] :=
        fDrawing2D.TeXFigureEpilogue;
      if fDrawing2D.TeXPicPrologue <> ''
        then AttributeValue['TeXPicPrologue'] :=
        fDrawing2D.TeXPicPrologue;
      if fDrawing2D.TeXPicEpilogue <> ''
        then AttributeValue['TeXPicEpilogue'] :=
        fDrawing2D.TeXPicEpilogue;
      if fDrawing2D.SVGMagnif <> SVGMagnif_Default
        then AttributeValue['SVGMagnif'] :=
        fDrawing2D.SVGMagnif;
      if fDrawing2D.MetaPostTeXText <> True
        then AttributeValue['MetaPostTeXText'] :=
        fDrawing2D.MetaPostTeXText;
    end;
  end;
  if (fDrawing2D.Caption <> '') or
    (fDrawing2D.FigLabel <> '') then
    with fXML.DocumentElement.AddElement('caption') do
    begin
      Text := fDrawing2D.Caption;
      AttributeValue['label'] := fDrawing2D.FigLabel;
    end;
  if fDrawing2D.Comment <> '' then
    with fXML.DocumentElement.AddElement('comment') do
      Text := fDrawing2D.Comment;
end;

procedure T_TpX_Saver.WritePrimitiveAttr(Obj: TPrimitive2D;
  XMLNode: TXMLDElement);
begin
  if Obj.LineKind <> liThick then
    XMLNode.AttributeValue['li'] := Obj.LineKind;
  if Obj.Hatching <> haNone then
    XMLNode.AttributeValue['ha'] := Obj.Hatching;
end;

procedure T_TpX_Saver.WriteLine2D(Obj: TLine2D);
var
  P1, P2: TPoint2D;
  XMLNode: TXMLDElement;
begin
  XMLNode := fXML.DocumentElement.AddElement('line');
  with Obj do
  begin
    P1 := Points[0];
    XMLNode.AttributeValue['x1'] := FF(P1.X);
    XMLNode.AttributeValue['y1'] := FF(P1.Y);
    if BeginArrowKind <> arrNone then
      XMLNode.AttributeValue['arr1'] := Ord(BeginArrowKind) - 1;
    P2 := Points[1];
    XMLNode.AttributeValue['x2'] := FF(P2.X);
    XMLNode.AttributeValue['y2'] := FF(P2.Y);
    if EndArrowKind <> arrNone then
      XMLNode.AttributeValue['arr2'] := Ord(EndArrowKind) - 1;
    WritePrimitiveAttr(Obj, XMLNode);
  end;
end;

procedure T_TpX_Saver.WriteRectangle2D(Obj: TRectangle2D);
var
  P0, P1, P2: TPoint2D;
  A: TRealType;
  XMLNode: TXMLDElement;
begin
  XMLNode := fXML.DocumentElement.AddElement('rect');
  with Obj do
  begin
    P0 := Points[0];
    P1 := Points[1];
    P2 := Points[2];
    XMLNode.AttributeValue['x1'] := FF(P0.X);
    XMLNode.AttributeValue['y1'] := FF(P0.Y);
    XMLNode.AttributeValue['x2'] := FF(P1.X);
    XMLNode.AttributeValue['y2'] := FF(P1.Y);
    A := CalcRotationAngle(P0, P2);
    if A <> 0 then
      XMLNode.AttributeValue['rot'] := FF(A);
    WritePrimitiveAttr(Obj, XMLNode);
  end;
end;

procedure T_TpX_Saver.WriteText2D(Obj: TText2D);
var
  P: TPoint2D;
  XMLNode: TXMLDElement;
begin
  XMLNode := fXML.DocumentElement.AddElement('text');
  with Obj do
  begin
    P := Points[0];
    XMLNode.AttributeValue['x'] := FF(P.X);
    XMLNode.AttributeValue['y'] := FF(P.Y);
    XMLNode.AttributeValue['t'] := Text;
    if TeXText <> '' then
      XMLNode.AttributeValue['tex'] := TeXText;
    XMLNode.AttributeValue['h'] := FF(Height);
    case HJustification of
      jhLeft: XMLNode.AttributeValue['jh'] := 'l';
      jhCenter: XMLNode.AttributeValue['jh'] := 'c';
      jhRight: XMLNode.AttributeValue['jh'] := 'r';
    end;
    case VJustification of
      jvBottom: XMLNode.AttributeValue['jv'] := 'b';
      jvCenter: XMLNode.AttributeValue['jv'] := 'c';
      jvTop: XMLNode.AttributeValue['jv'] := 't';
    end;
    WritePrimitiveAttr(Obj, XMLNode);
  end;
end;

procedure T_TpX_Saver.WriteStar2D(Obj: TStar2D);
var
  P: TPoint2D;
  XMLNode: TXMLDElement;
begin
  XMLNode := fXML.DocumentElement.AddElement('star');
  with Obj do
  begin
    P := Points[0];
    XMLNode.AttributeValue['x'] := FF(P.X);
    XMLNode.AttributeValue['y'] := FF(P.Y);
  end;
end;

procedure T_TpX_Saver.WriteEllipse2D(Obj: TEllipse2D);
var
  XMLNode: TXMLDElement;
  CX, CY, RX, RY, ARot: TRealType;
begin
  XMLNode := fXML.DocumentElement.AddElement('ellipse');
  with Obj do
  begin
    GetEllipseParams(CX, CY, RX, RY, ARot);
    XMLNode.AttributeValue['x'] := FF(CX);
    XMLNode.AttributeValue['y'] := FF(CY);
    XMLNode.AttributeValue['dx'] := FF(RX * 2);
    XMLNode.AttributeValue['dy'] := FF(RY * 2);
    if ARot <> 0 then
      XMLNode.AttributeValue['rot'] := FF(ARot);
    WritePrimitiveAttr(Obj, XMLNode);
  end;
end;

procedure T_TpX_Saver.WriteCircle2D(Obj: TCircle2D);
var
  XMLNode: TXMLDElement;
  CP: TPoint2D;
begin
  XMLNode := fXML.DocumentElement.AddElement('circle');
  with Obj do
  begin
    CP := Points[0];
    XMLNode.AttributeValue['x'] := FF(CP.X);
    XMLNode.AttributeValue['y'] := FF(CP.Y);
    XMLNode.AttributeValue['d'] :=
      FF(PointDistance2D(CP, Points[1]) * 2);
    WritePrimitiveAttr(Obj, XMLNode);
  end;
end;

procedure T_TpX_Saver.WriteCircular2D(Obj: TCircular2D);
var
  CX, CY, R, SA, EA: TRealType;
  XMLNode: TXMLDElement;
begin
  if Obj is TArc2D then
    XMLNode := fXML.DocumentElement.AddElement('arc')
  else if Obj is TSector2D then
    XMLNode := fXML.DocumentElement.AddElement('sector')
  else if Obj is TSegment2D then
    XMLNode := fXML.DocumentElement.AddElement('segment');
  Obj.GetArcParams(CX, CY, R, SA, EA);
  SA := SA - Floor(SA / (2 * Pi)) * 2 * Pi;
  EA := EA - Floor(EA / (2 * Pi)) * 2 * Pi;
  if EA < SA then EA := EA + 2 * Pi;
  XMLNode.AttributeValue['x'] := FF(CX);
  XMLNode.AttributeValue['y'] := FF(CY);
  XMLNode.AttributeValue['d'] := FF(R * 2);
  XMLNode.AttributeValue['a1'] := FF(SA);
  XMLNode.AttributeValue['a2'] := FF(EA);
  WritePrimitiveAttr(Obj, XMLNode);
end;

procedure T_TpX_Saver.WriteSpline2D(Obj: TSpline2D0);
var
  XMLNode: TXMLDElement;
begin
  if Obj.Order = 3 then
    XMLNode := fXML.DocumentElement.AddElement('spline')
  else XMLNode := fXML.DocumentElement.AddElement('cspline');
  if Obj.IsClosed then XMLNode.AttributeValue['closed'] := '1';
  XMLNode.Text := GetPathString(Obj.Points);
  WritePrimitiveAttr(Obj, XMLNode);
end;

procedure T_TpX_Saver.WriteSmooth2D(Obj: TSmoothPath2D0);
var
  XMLNode: TXMLDElement;
begin
  XMLNode := fXML.DocumentElement.AddElement('curve');
  if Obj.IsClosed then XMLNode.AttributeValue['closed'] := '1';
  XMLNode.Text := GetPathString(Obj.Points);
  WritePrimitiveAttr(Obj, XMLNode);
end;

procedure T_TpX_Saver.WritePoly2D(Obj: TPolyline2D0);
var
  P1, PPrev: TPoint2D;
  I: Integer;
  XMLNode: TXMLDElement;
  PathSt: string;
begin
  if Obj is TPolygon2D
    then XMLNode := fXML.DocumentElement.AddElement('polygon')
  else XMLNode := fXML.DocumentElement.AddElement('polyline');
  PathSt := '';
  PPrev := Point2D(987123456, 987123456);
  with Obj do
  begin
    for I := 0 to Points.Count - 1 do
    begin
      P1 := Points[I];
      if not IsSamePoint2D(P1, PPrev) then
        PathSt := PathSt + FF(P1.X) + ',' + FF(P1.Y);
      PPrev := P1;
      if (I mod 100) = 88 then
        PathSt := PathSt + EOL;
      if I < Points.Count - 1 then PathSt := PathSt + ' ';
    end;
    WritePrimitiveAttr(Obj, XMLNode);
  end;
  XMLNode.Text := PathSt;
end;

procedure T_TpX_Saver.StoreToClipboard;
begin
  fStream.Free;
  fStream := TMemoryStream.Create;
  try
    WriteAllToStream;
    fStream.Position := 0;
    PutStreamToClipboard0(CF_TEXT, fStream, fStream.Size);
  finally
    fStream.Free;
    fStream := nil;
  end;
end;

{ --================ T_SVG_Export ==================-- }

constructor T_SVG_Export.Create(CAD: TDrawing2D);
begin
  inherited Create(CAD);
  RO_Init(fXML, TXMLDDocument.Create);
end;

destructor T_SVG_Export.Destroy;
begin
  RO_Free(fXML);
  inherited Destroy;
end;

procedure T_SVG_Export.WriteAll;
begin
  fXML.LoadXML('');
  inherited WriteAll;
end;

procedure T_SVG_Export.WriteAllToStream;
begin
  if fStream = nil then Exit;
  WriteAll;
  fXML.Save(fStream);
end;

procedure T_SVG_Export.WritePatterns(DefsNode: TXMLDElement);
var
  PattNode: TXMLDElement;
  HV, D: Integer;
  procedure FillPatt(ID, PathSt: string; Size: Integer);
  begin
    PattNode := DefsNode.AddElement('pattern');
    with PattNode do
    begin
      AttributeValue['id'] := ID;
      AttributeValue['width'] := Size;
      AttributeValue['height'] := Size;
      AttributeValue['patternUnits'] := 'userSpaceOnUse';
      with PattNode.AddElement('path') do
      begin
        AttributeValue['d'] := PathSt;
        AttributeValue['fill'] := 'none';
        AttributeValue['stroke'] := 'black';
        AttributeValue['stroke-width'] :=
          Format('%.1f',
          [fDrawing2D.LineWidth / 2 * fFactorMM]);
      end;
    end;
  end;
begin
  HV := Round(fDrawing2D.HatchingStep * fFactorMM / 2);
  D := Round(fDrawing2D.HatchingStep * fFactorMM * Sqrt(2));
  FillPatt('haH',
    Format('M 0,%d L %d,%d', [HV, HV * 2, HV]), HV * 2);
  FillPatt('haV',
    Format('M %d,0 L %d,%d', [HV, HV, HV * 2]), HV * 2);
  FillPatt('haFD', Format('M 0,0 L %d,%d', [D, D]), D);
  FillPatt('haBD', Format('M %d,0 L 0,%d', [D, D]), D);
  FillPatt('haC', Format('M 0,%d L %d,%d M %d,0 L %d,%d',
    [HV, HV * 2, HV, HV, HV, HV * 2]), HV * 2);
  FillPatt('haDC', Format('M 0,0 L %d,%d M %d,0 L 0,%d',
    [D, D, D, D]), D);
    {haHorizontal: St := 'lightpink';
    haVertical: St := 'lightgray';
    haFDiagonal: St := 'chocolate';
    haBDiagonal: St := 'mediumseagreen';
    haCross: St := 'orange';
    haDiagCross: St := 'darkcyan';}
end;

procedure T_SVG_Export.WriteHeader;
var
  XMLNode: TXMLDElement;
begin
  fMagnif := fDrawing2D.SVGMagnif;
  fFactorMM := fMagnif / fDrawing2D.PicUnitLength;
  MeasureDrawing;
  {fXML.Doctype := 'svg PUBLIC "-//W3C//DTD SVG 1.1//EN"' + EOL
    + ' "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"';}
  //  <?xml version="1.0" standalone="no"?>
  //fXML.LoadXML('<svg/>');
  fXML.InsertChild(TXmlDProcessingInstruction.Create('xml',
    'version="1.0" encoding="UTF-8"'));
  fXML.AddElement('svg');
  //fXML.AddElement('');
  //<!-- svg width="8cm" height="4cm" viewBox="0 0 800 400" -->
  with fXML.DocumentElement do
  begin
    AttributeValue['version'] := '1.1';
    AttributeValue['width'] := Format('%.2fmm',
      [fW_MM * fMagnif]);
    AttributeValue['height'] := Format('%.2fmm',
      [fH_MM * fMagnif]);
    AttributeValue['viewBox'] := Format('0 0 %.1f %.1f',
      [fW_MM * fFactorMM, fH_MM * fFactorMM]);
    fXML.DocumentElement.InsertChild(
      TXmlDComment.Create('Exported from TpX drawing'));
    XMLNode := fXML.DocumentElement.AddElement('title');
    XMLNode.Text := AnsiToUtf8(fDrawing2D.Caption);
    XMLNode := fXML.DocumentElement.AddElement('desc');
    XMLNode.Text := AnsiToUtf8(fDrawing2D.Comment);
    XMLNode := fXML.DocumentElement.AddElement('defs');
    WritePatterns(XMLNode);
  end;
end;

function T_SVG_Export.ConvertY(Y: TRealType): TRealType;
begin
  Result := (fExtTop - Y) * fFactorH;
end;

function T_SVG_Export.GetX(X: TRealType): Integer;
begin
  Result := Round(ConvertX(X));
end;

function T_SVG_Export.GetY(Y: TRealType): Integer;
begin
  Result := Round(ConvertY(Y));
end;

procedure T_SVG_Export.WritePrimitiveAttr(Obj: TPrimitive2D;
  XMLNode: TXMLDElement);
var
  St: string;
begin
  if Obj.LineKind = liNone then
    XMLNode.AttributeValue['stroke'] := 'none'
  else
  begin
    XMLNode.AttributeValue['stroke-miterlimit'] := 10;
    XMLNode.AttributeValue['stroke'] := 'black';
    case Obj.LineKind of
      liThick, liDotted:
        XMLNode.AttributeValue['stroke-width'] :=
          Format('%.1f', [fDrawing2D.LineWidth * 2 *
          fFactorMM]);
      liThin, liDashed:
        XMLNode.AttributeValue['stroke-width'] :=
          Format('%.1f', [fDrawing2D.LineWidth * fFactorMM]);
    end;
    case Obj.LineKind of
      liDotted:
        XMLNode.AttributeValue['stroke-dasharray']
          := Format('%.1f,%.1f',
          [fDrawing2D.LineWidth * 2 * fFactorMM,
          fDrawing2D.DottedSize * fFactorMM]);
      liDashed:
        XMLNode.AttributeValue['stroke-dasharray']
          := Format('%.1f,%.1f',
          [fDrawing2D.DashSize * 2 * fFactorMM,
          fDrawing2D.DashSize * fFactorMM]);
    end;
  end;
  case Obj.Hatching of
    haNone: St := 'none';
    haHorizontal: St := 'url(#haH)';
    haVertical: St := 'url(#haV)';
    haFDiagonal: St := 'url(#haFD)';
    haBDiagonal: St := 'url(#haBD)';
    haCross: St := 'url(#haC)';
    haDiagCross: St := 'url(#haDC)';
    //chocolate darkcyan lightpink mediumseagreen orange lightgray mediumvioletred
  else St := 'black';
  end;
  XMLNode.AttributeValue['fill'] := St;
end;

procedure T_SVG_Export.WriteLine2D(Obj: TLine2D);
var
  PP: TPointsSet2D;
  I: Integer;
  XMLNode: TXMLDElement;
  PathSt: string;
  procedure AddSt(St: string);
  begin
    PathSt := PathSt + St;
  end;
  procedure AddPoint(P: TPoint2D);
  begin
    AddSt(IntToStr(GetX(P.X)) + ','
      + IntToStr(GetY(P.Y)));
  end;
begin
  XMLNode := fXML.DocumentElement.AddElement('path');
  PP := TPointsSet2D.Create(10);
  Obj.FillPoints(PP);
  if PP.Count > 0 then
  begin
    PathSt := 'M ';
    AddPoint(PP[0]);
    for I := 1 to PP.Count - 1 do
    begin
      AddSt(' L ');
      AddPoint(PP[I]);
    end;
  end;
  PP.Free;
  XMLNode.AttributeValue['d'] := PathSt;
  WritePrimitiveAttr(Obj, XMLNode);
end;

procedure T_SVG_Export.WriteRectangle2D(Obj: TRectangle2D);
var
  P0, P1, P2, CP: TPoint2D;
  W, H, A: TRealType;
  XMLNode: TXMLDElement;
begin
  XMLNode := fXML.DocumentElement.AddElement('rect');
  with Obj do
  begin
    BeginUseProfilePoints;
    P0 := ProfilePoints[0];
    P1 := ProfilePoints[1];
    P2 := ProfilePoints[2];
    EndUseProfilePoints;
    CP := MidPoint(P0, P2);
    W := PointDistance2D(P1, P2);
    H := PointDistance2D(P0, P1);
    XMLNode.AttributeValue['x'] := GetX(CP.X - W / 2);
    XMLNode.AttributeValue['y'] := GetY(CP.Y + H / 2);
    XMLNode.AttributeValue['width'] := Round(W * fFactorW);
    XMLNode.AttributeValue['height'] := Round(H * fFactorH);
    A := CalcRotationAngle(P0, P1) + Pi;
    if A <> 0 then
      XMLNode.AttributeValue['transform'] :=
        'rotate(' + FF(A)
        + 'rad ' + IntToStr(GetX(CP.X))
        + ' ' + IntToStr(GetY(CP.Y)) + ')';
    WritePrimitiveAttr(Obj, XMLNode);
  end;
end;

procedure T_SVG_Export.WriteText2D(Obj: TText2D);
var
  P: TPoint2D;
  XMLNode: TXMLDElement;
  E: TRect2D;
  FontH, WW: TRealType;
begin
  XMLNode := fXML.DocumentElement.AddElement('text');
  with Obj do
  begin
    P := Points[0];
    XMLNode.AttributeValue['x'] := GetX(P.X);
    XMLNode.AttributeValue['y'] := GetY(P.Y);
    FontH := Height * fFactorH;
    XMLNode.AttributeValue['font-size'] := Round(FontH);
    XMLNode.Text := AnsiToUtf8(Text);
    E := GetExtension;
    WW := E.Right - E.Left;
    XMLNode.AttributeValue['textLength'] :=
      Round(WW * fFactorW);
    {case HJustification of
      jhCenter: XMLNode.AttributeValue['dx']
        := -Round(WW * fFactorW / 2);
      jhRight: XMLNode.AttributeValue['dx']
        := -Round(WW * fFactorW);
    end;}
    case VJustification of
      jvBottom:
        XMLNode.AttributeValue['dy'] := -Round(FontH * 0.2);
      jvCenter:
        XMLNode.AttributeValue['dy'] := Round(FontH * 0.3);
      jvTop:
        XMLNode.AttributeValue['dy'] := Round(FontH * 0.8);
    end;
    case HJustification of
      jhLeft: XMLNode.AttributeValue['text-anchor']
        := 'start';
      jhCenter: XMLNode.AttributeValue['text-anchor']
        := 'middle';
      jhRight: XMLNode.AttributeValue['text-anchor'] := 'end';
    end;
    {case VJustification of
      jvBottom: XMLNode.AttributeValue['dominant-baseline']
        := 'bottom';
      jvCenter: XMLNode.AttributeValue['dominant-baseline']
        := 'central';
      jvTop: XMLNode.AttributeValue['dominant-baseline']
        := 'top';
    end;}
    XMLNode.AttributeValue['fill'] := 'black';
    XMLNode.AttributeValue['style'] :=
      'font-family: ''Times New Roman''; font-weight:normal';
    //WritePrimitiveAttr(Obj, XMLNode);
  end;
end;

procedure T_SVG_Export.WriteStar2D(Obj: TStar2D);
var
  P: TPoint2D;
  XMLNode: TXMLDElement;
begin
  XMLNode := fXML.DocumentElement.AddElement('circle');
  with Obj do
  begin
    P := Points[0];
    XMLNode.AttributeValue['cx'] := GetX(P.X);
    XMLNode.AttributeValue['cy'] := GetY(P.Y);
    if Obj.OwnerCAD is TDrawing2D then
      XMLNode.AttributeValue['r'] := Round(
        (Obj.OwnerCAD as TDrawing2D).StarsSize * fFactorH)
    else XMLNode.AttributeValue['r'] := 1;
    XMLNode.AttributeValue['fill'] := 'black';
  end;
end;

procedure T_SVG_Export.WriteEllipse2D(Obj: TEllipse2D);
var
  XMLNode: TXMLDElement;
  CX, CY, RX, RY: TRealType;
  ARot: TRealType;
begin
  XMLNode := fXML.DocumentElement.AddElement('ellipse');
  with Obj do
  begin
    GetEllipseParams(CX, CY, RX, RY, ARot);
    XMLNode.AttributeValue['cx'] := GetX(CX);
    XMLNode.AttributeValue['cy'] := GetY(CY);
    XMLNode.AttributeValue['rx'] := Round(RX * fFactorW);
    XMLNode.AttributeValue['ry'] := Round(RY * fFactorH);
    if ARot <> 0 then
      XMLNode.AttributeValue['transform'] :=
        'rotate(' + FF(ARot)
        + 'rad ' + IntToStr(GetX(CX))
        + ' ' + IntToStr(GetY(CY)) + ')';
    WritePrimitiveAttr(Obj, XMLNode);
  end;
end;

procedure T_SVG_Export.WriteCircle2D(Obj: TCircle2D);
var
  XMLNode: TXMLDElement;
  CP: TPoint2D;
begin
  XMLNode := fXML.DocumentElement.AddElement('circle');
  with Obj do
  begin
    CP := Points[0];
    XMLNode.AttributeValue['cx'] := GetX(CP.X);
    XMLNode.AttributeValue['cy'] := GetY(CP.Y);
    XMLNode.AttributeValue['r'] := Round(
      PointDistance2D(CP, Points[1]) * fFactorH);
    WritePrimitiveAttr(Obj, XMLNode);
  end;
end;

procedure T_SVG_Export.WriteCircular2D(Obj: TCircular2D);
var
  CP: TPoint2D;
  R, SA, EA: TRealType;
  XMLNode: TXMLDElement;
  PathSt: string;
  procedure AddSt(St: string);
  begin
    PathSt := PathSt + St;
  end;
  procedure AddPoint(P: TPoint2D);
  begin
    AddSt(IntToStr(GetX(P.X)) + ','
      + IntToStr(GetY(P.Y)));
  end;
begin
  Obj.GetArcParams(CP.X, CP.Y, R, SA, EA);
  SA := SA - Floor(SA / (2 * Pi)) * 2 * Pi;
  EA := EA - Floor(EA / (2 * Pi)) * 2 * Pi;
  if EA < SA then EA := EA + 2 * Pi;
  XMLNode := fXML.DocumentElement.AddElement('path');
  PathSt := 'M ';
  AddPoint(Obj.Points[1]);
  AddSt(' A ' + IntToStr(Round(R * fFactorW))
    + ' ' + IntToStr(Round(R * fFactorH))
    + ' 0 ' + IntToStr(Integer(EA - SA > Pi)) + ' 0 ');
  AddPoint(Point2D(CP.X + R * Cos(EA), CP.Y + R * Sin(EA)));
  //if Obj is TArc2D then
  if Obj is TSector2D then
  begin
    AddSt(' L ');
    AddPoint(CP);
    AddSt(' L ');
    AddPoint(Obj.Points[1]);
    AddSt(' Z');
  end
  else if Obj is TSegment2D then
  begin
    AddSt(' L ');
    AddPoint(Obj.Points[1]);
    AddSt(' Z');
  end;
  XMLNode.AttributeValue['d'] := PathSt;
  WritePrimitiveAttr(Obj, XMLNode);
end;

procedure T_SVG_Export.WriteSpline2D(Obj: TSpline2D0);
begin
  if Obj is TBSpline2D
    then WriteBSpline2D(Obj as TBSpline2D)
  else if Obj is TCubicBSpline2D
    then WriteCubicSpline2D(Obj as TCubicBSpline2D)
  else if Obj is TClosedBSpline2D
    then WriteClosedBSpline2D(Obj as TClosedBSpline2D)
  else if Obj is TClosedCubicBSpline2D then
    WriteCubicSpline2D(Obj as TClosedCubicBSpline2D);
end;

procedure T_SVG_Export.WriteBSpline2D(Obj: TBSpline2D);
var
  PP: TPointsSet2D;
  I: Integer;
  XMLNode: TXMLDElement;
  PathSt: string;
  procedure AddSt(St: string);
  begin
    PathSt := PathSt + St;
  end;
  procedure AddPoint(P: TPoint2D);
  begin
    AddSt(IntToStr(GetX(P.X)) + ','
      + IntToStr(GetY(P.Y)));
  end;
begin
  PP := Obj.Points;
  if PP.Count < 2 then Exit;
  XMLNode := fXML.DocumentElement.AddElement('path');
  PathSt := 'M ';
  AddPoint(PP[0]);
  if PP.Count = 2 then
  begin
    AddSt(' L ');
    AddPoint(PP[1]);
  end
  else if PP.Count = 3 then
  begin
    AddSt(' Q ');
    AddPoint(PP[1]);
    AddSt(' ');
    AddPoint(PP[2]);
  end
  else
  begin
    AddSt(' Q ');
    AddPoint(PP[1]);
    AddSt(' ');
    AddPoint(MidPoint(PP[1], PP[2]));
    for I := 2 to PP.Count - 3 do
    begin
      AddSt(' T ');
      AddPoint(MidPoint(PP[I], PP[I + 1]));
    end;
    AddSt(' T ');
    AddPoint(PP[PP.Count - 1]);
  end;
  XMLNode.AttributeValue['d'] := PathSt;
  WritePrimitiveAttr(Obj, XMLNode);
end;

procedure T_SVG_Export.WriteClosedBSpline2D(Obj:
  TClosedBSpline2D);
var
  PP: TPointsSet2D;
  I, J: Integer;
  XMLNode: TXMLDElement;
  PathSt: string;
  procedure AddSt(St: string);
  begin
    PathSt := PathSt + St;
  end;
  procedure AddPoint(P: TPoint2D);
  begin
    AddSt(IntToStr(GetX(P.X)) + ','
      + IntToStr(GetY(P.Y)));
  end;
begin
  PP := Obj.Points;
  if PP.Count < 2 then Exit;
  XMLNode := fXML.DocumentElement.AddElement('path');
  PathSt := 'M ';
  if PP.Count = 2 then
  begin
    AddPoint(PP[0]);
    AddSt(' L ');
    AddPoint(PP[1]);
  end
  else
  begin
    AddPoint(MidPoint(PP[PP.Count - 1], PP[0]));
    AddSt(' Q ');
    AddPoint(PP[0]);
    AddSt(' ');
    AddPoint(MidPoint(PP[0], PP[1]));
    for I := 1 to PP.Count - 1 do
    begin
      AddSt(' T ');
      J := I + 1;
      if J = PP.Count then J := 0;
      AddPoint(MidPoint(PP[I], PP[J]));
    end;
    AddSt(' Z');
  end;
  XMLNode.AttributeValue['d'] := PathSt;
  WritePrimitiveAttr(Obj, XMLNode);
end;

procedure T_SVG_Export.WriteCubicSpline2D(Obj: TSpline2D0);
var
  I: Integer;
  XMLNode: TXMLDElement;
  PP: TPointsSet2D;
  PathSt: string;
  procedure AddSt(St: string);
  begin
    PathSt := PathSt + St;
  end;
  procedure AddPoint(P: TPoint2D);
  begin
    AddSt(IntToStr(GetX(P.X)) + ','
      + IntToStr(GetY(P.Y)));
  end;
begin
  if Obj.Points.Count < 2 then Exit;
  XMLNode := fXML.DocumentElement.AddElement('path');
  PP := nil;
  try
    Obj.BezierPoints(PP, IdentityTransf2D);
    PathSt := 'M ';
    AddPoint(PP[0]);
    for I := 1 to PP.Count - 1 do
    begin
      if I mod 3 = 1 then AddSt(' C');
      AddSt(' ');
      AddPoint(PP[I]);
    end;
    if Obj.IsClosed then AddSt(' Z');
  finally
    PP.Free;
  end;
  XMLNode.AttributeValue['d'] := PathSt;
  WritePrimitiveAttr(Obj, XMLNode);
end;

procedure T_SVG_Export.WriteSmooth2D(Obj: TSmoothPath2D0);
var
  I: Integer;
  XMLNode: TXMLDElement;
  PP: TPointsSet2D;
  PathSt: string;
  procedure AddSt(St: string);
  begin
    PathSt := PathSt + St;
  end;
  procedure AddPoint(P: TPoint2D);
  begin
    AddSt(IntToStr(GetX(P.X)) + ','
      + IntToStr(GetY(P.Y)));
  end;
begin
  if Obj.Points.Count < 2 then Exit;
  XMLNode := fXML.DocumentElement.AddElement('path');
  PP := nil;
  try
    Obj.BezierPoints(PP, IdentityTransf2D);
    PathSt := 'M ';
    AddPoint(PP[0]);
    for I := 1 to PP.Count - 1 do
    begin
      if I mod 3 = 1 then AddSt(' C');
      AddSt(' ');
      AddPoint(PP[I]);
    end;
    if Obj.IsClosed then AddSt(' Z');
  finally
    PP.Free;
  end;
  XMLNode.AttributeValue['d'] := PathSt;
  WritePrimitiveAttr(Obj, XMLNode);
end;

procedure T_SVG_Export.WritePoly2D(Obj: TPolyline2D0);
var
  P1, PPrev: TPoint2D;
  I: Integer;
  XMLNode: TXMLDElement;
  PathSt: string;
  procedure AddSt(St: string);
  begin
    PathSt := PathSt + St;
  end;
  procedure AddPoint(P: TPoint2D);
  begin
    AddSt(IntToStr(GetX(P.X)) + ','
      + IntToStr(GetY(P.Y)));
  end;
begin
  if Obj is TPolygon2D
    then XMLNode := fXML.DocumentElement.AddElement('polygon')
  else XMLNode := fXML.DocumentElement.AddElement('polyline');
  PathSt := '';
  PPrev := Point2D(987123456, 987123456);
  with Obj do
  begin
    for I := 0 to Points.Count - 1 do
    begin
      P1 := Points[I];
      if not IsSamePoint2D(P1, PPrev) then AddPoint(P1);
      PPrev := P1;
      if (I mod 100) = 88 then AddSt(EOL);
      if I < Points.Count - 1 then AddSt(' ');
    end;
    WritePrimitiveAttr(Obj, XMLNode);
  end;
  XMLNode.AttributeValue['points'] := PathSt;
end;

procedure T_SVG_Export.StoreToClipboard;
begin
  fStream.Free;
  fStream := TMemoryStream.Create;
  try
    WriteAllToStream;
    fStream.Position := 0;
    PutStreamToClipboard0(CF_TEXT, fStream, fStream.Size);
  finally
    fStream.Free;
    fStream := nil;
  end;
end;

procedure StoreToFile_SVG(const CAD: TDrawing2D;
  const FileName: string);
var
  SVG_Export: T_SVG_Export;
begin
  SVG_Export := T_SVG_Export.Create(CAD);
  try
    SVG_Export.StoreToFile(FileName);
  finally
    SVG_Export.Free;
  end;
end;

{ --================ T_PostScript_Export ==================-- }

procedure pfb2pfa_Stream(Stream_pfb, Stream_pfa: TStream;
  var FontName: string);
//	Program converts a binary MSDOS representation for a type1
//	PostScript font into a readable ASCII version. The MSDOS
//	newline (\r) is converted into the UNIX newline (\n).
//	The output is written in a file whose name is the name that
//	is provided on the command line or the basename of the input
//	file plus extension ".pfa".
var
  L, Len, I, J: Integer;
  T, C: Byte;
  St: string;
const
  NewLine = EOL; //\012 \n
  HEX_PER_LINE = 30;
  function GetByte: Byte;
  begin
    Stream_pfb.ReadBuffer(Result, 1);
  end;
  procedure WriteStream(const St: string);
  begin
    Stream_pfa.Write(St[1], Length(St));
  end;
begin
  while Stream_pfb.Position < Stream_pfb.Size do
  begin
    if GetByte <> 128 then
      ; // fatal("%s: not a pfb file.\n", pfbname);
    T := GetByte;
    case T of
      1:
        begin
          L := GetByte or GetByte shl 8 or GetByte shl 16 or
            GetByte shl 24;
          SetLength(St, L);
          Stream_pfb.ReadBuffer(St[1], L);
          J := Pos('/FontName', St);
          if J > 0 then
          begin
            J := J + 8;
            while St[J] <> '/' do Inc(J);
            Inc(J);
            I := J;
            while not (St[J] in [#10, ' ', '/']) do Inc(J);
            FontName := Copy(St, I, J - I);
          end;
          for I := 1 to L do
          begin
            if St[I] = #10 then WriteStream(NewLine)
            else WriteStream(St[I]);
          end;
        end;
      2:
        begin
          L := GetByte or GetByte shl 8 or GetByte shl 16 or
            GetByte shl 24;
          SetLength(St, L);
          Stream_pfb.ReadBuffer(St[1], L);
          for I := 1 to L do
          begin
            WriteStream(IntToHex(Byte(St[I]), 2));
            if (I mod HEX_PER_LINE) = 0 then
              WriteStream(NewLine);
          end;
          WriteStream(NewLine);
        end;
      3:
        begin
         //if (verbose) printf("End of file\n");
         //exit(0);
        end;
    else
       //fatal("Unknown field type: %d\n", t);
    end;
  end;
end;

procedure pfb2pfa(const FileName_pfb, FileName_pfa: string);
var
  Stream_pfb, Stream_pfa: TFileStream;
  FontName: string;
begin
  Stream_pfb := TFileStream.Create(FileName_pfb, fmOpenRead);
  Stream_pfa := TFileStream.Create(FileName_pfa, fmCreate);
  try
    pfb2pfa_Stream(Stream_pfb, Stream_pfa, FontName);
  finally
    Stream_pfb.Free;
    Stream_pfa.Free;
  end;
end;

constructor T_PostScript_Export.Create(CAD: TDrawing2D);
begin
  inherited Create(CAD);
end;

procedure T_PostScript_Export.WriteStreamPoint0(X, Y:
  TRealType);
begin
  WriteStream(Format('%.1f %.1f ', [X, Y]));
end;

procedure T_PostScript_Export.WriteLineThickness(Kind:
  TLineKind);
var
  A: TRealType;
begin
  A := fFactorMM;
  with fDrawing2D do
    case Kind of
      liNone: WriteStream('0 setlinewidth [] 0 setdash ');
      liThick: WriteStream(
          Format('%.2g setlinewidth [] 0 setdash ',
          [LineWidth * 2 * A]));
      liThin: WriteStream(
          Format('%.2g setlinewidth [] 0 setdash ',
          [LineWidth * A]));
      liDashed: WriteStream(
          Format('%.2g setlinewidth [%.2g %.2g] 0 setdash ',
          [LineWidth * A, DashSize * 2 * A, DashSize * A]));
      liDotted: WriteStream(
          Format('%.2g setlinewidth [%.2g %.2g] 0 setdash ',
          [LineWidth * 2 * A, LineWidth * 2 * A,
          DottedSize * A]));
    end;
end;

procedure T_PostScript_Export.WriteLine(P0, P1: TPoint2D;
  Kind: TLineKind);
begin
  //if Kind = liNone then Exit;
  WriteStream('newpath ');
  WriteLineThickness(Kind);
  WriteStreamPoint(P0);
  WriteStream('moveto ');
  WriteStreamPoint(P1);
  WriteStream('lineto ');
  WriteLnStream('stroke');
end;

procedure T_PostScript_Export.WritePoly(PP: TPointsSet2D;
  Kind: TLineKind; Closed: Boolean);
var
  I: Integer;
begin
  if PP.Count < 1 then Exit;
  //if Kind = liNone then Exit;
  WriteStream('newpath ');
  WriteLineThickness(Kind);
  WriteStreamPoint(PP[0]);
  WriteStream('moveto ');
  for I := 1 to PP.Count - 1 do
  begin
    WriteStreamPoint(PP[I]);
    WriteStream('lineto ');
    if (I mod 10) = 0 then WriteLnStream('');
  end;
  if Closed then WriteStream('closepath ');
  WriteLnStream('stroke');
end;

procedure T_PostScript_Export.WriteBezierPath(const PP:
  TPointsSet2D; Kind: TLineKind);
var
  I: Integer;
begin
  //if Kind = liNone then Exit;
  WriteStream('newpath ');
  WriteLineThickness(Kind);
  WriteStreamPoint(PP[0]);
  WriteStream('moveto ');
  for I := 1 to PP.Count - 1 do
  begin
    WriteStreamPoint(PP[I]);
    if I mod 3 = 0 then
    begin
      WriteLnStream('curveto');
    end;
  end;
  WriteLnStream('stroke');
end;

procedure T_PostScript_Export.WriteHeader;
begin //mm=2.845pt ??    // 2.8346 pixel per mm
  fFactorMM := 2.8346;
  MeasureDrawing;
  fHatchingStep := fDrawing2D.HatchingStep / fFactorAbs;
  WriteLnStream('%!PS-Adobe-3.0 EPSF-3.0');
  WriteLnStream('%%' + Format('BoundingBox: %d %d %d %d',
    [0, 0, Ceil(fW_MM * fFactorMM),
    Ceil(fH_MM * fFactorMM)])); // [0, 0, fW, fH]));
  WriteLnStream('%%Title: ' + fDrawing2D.Caption);
  WriteLnStream('%%Creator: Exported from TpX drawing');
  WriteLnStream('%%CreationDate: ' + DateTimeToStr(Now));
  WriteLnStream('%%EndComments');
  FontName := 'Times-Roman';
  WriteFont;
//  WriteLnStream('/Times-Roman findfont');  WriteLnStream('dup length dict begin');  WriteLnStream('  { 1 index /FID ne');  WriteLnStream('    {def}');  WriteLnStream('    {pop pop}');  WriteLnStream('      ifelse');  WriteLnStream('  } forall');
//  WriteLnStream('  /Encoding ISOLatin1Encoding def');  WriteLnStream('  currentdict');  WriteLnStream('end');  WriteLnStream('/Times-Roman exch definefont pop');
{
%%BoundingBox: 4 4 608 407
%%Title: (ARTWORK.EPS)
%%CreationDate: (10/17/89) (5:04 PM)
%%EndComments
...PostScript code for illustration..
showpage
%%EOF}
//%%Creator: SomeApplication
end;

procedure T_PostScript_Export.WriteFont;
var
  Stream_pfb: TFileStream;
begin
  if Font_pfb_Path = '' then Exit;
  if not FileExists(Font_pfb_Path) then
  begin
    Application.MessageBox(
      PChar(Format('Can not find font file %s',
      [Font_pfb_Path])),
      'Error', MB_OK);
    Exit;
  end;
  Stream_pfb :=
    TFileStream.Create(Font_pfb_Path, fmOpenRead);
  try
    WriteLnStream('%%BeginProlog');
    pfb2pfa_Stream(Stream_pfb, fStream, FontName);
    WriteLnStream('%%Endprolog');
  finally
    Stream_pfb.Free;
  end;
end;

procedure T_PostScript_Export.WriteFooter;
begin
  WriteLnStream('showpage');
  WriteLnStream('%%EOF');
end;

procedure T_PostScript_Export.WriteLine2D(Obj: TLine2D);
var
  PP: TPointsSet2D;
begin
  PP := TPointsSet2D.Create(10);
  try
    Obj.FillPoints(PP);
    WritePoly(PP, Obj.LineKind, False);
  finally
    PP.Free;
  end;
end;

procedure CalculateHatching(const P: TPointsSet2D;
  DX, DY, Step: TRealType;
  Lines: TPointsSet2D);
var
  PP: TPointsSet2D;
  PrevP, CurrP, IntersP: TPoint2D;
  PrevV, CurrV, A: TRealType;
  MinValue, MaxValue, Value0, C: TRealType;
  I, J, K: Integer;
  function GetValue(P: TPoint2D): TRealType;
  begin
    Result := P.X * DX + P.Y * DY;
  end;
begin
  if P.Count < 3 then Exit;
  Step := Step * Sqrt(Sqr(DX) + Sqr(DY));
  PP := TPointsSet2D.Create(P.Count);
  MinValue := MaxSingle;
  MaxValue := -MaxSingle;
  for I := 0 to P.Count - 1 do
  begin
    CurrV := GetValue(P[I]);
    if CurrV < MinValue then MinValue := CurrV;
    if CurrV > MaxValue then MaxValue := CurrV;
  end;
  // Collect intersection points in PP:
  Value0 := (Floor(MinValue / Step) + 1) * Step;
  for J := 0 to
    Ceil(MaxValue / Step) - Floor(MinValue / Step) - 2 do
  begin
    PP.Clear;
    C := Value0 + J * Step;
    CurrP := P[P.Count - 1];
    CurrV := GetValue(CurrP);
    for I := 0 to P.Count - 1 do
    begin
      PrevP := CurrP;
      PrevV := CurrV;
      CurrP := P[I];
      CurrV := GetValue(CurrP);
      if ((CurrV > C) and (PrevV > C)) or
        ((CurrV < C) and (PrevV < C))
        or (CurrV = PrevV) then Continue;
      A := (C - PrevV) / (CurrV - PrevV);
      IntersP := Point2D(A * CurrP.X + (1 - A) * PrevP.X,
        A * CurrP.Y + (1 - A) * PrevP.Y);
      PP.Add(IntersP);
    end;
    // Sort PP:
    for I := 0 to PP.Count - 1 do
    begin
      IntersP := PP[I];
      for K := I + 1 to PP.Count - 1 do
        if - DY * IntersP.X + DX * IntersP.Y
          > -DY * PP[K].X + DX * PP[K].Y then
        begin
          PP[I] := PP[K];
          PP[K] := IntersP;
          IntersP := PP[I];
        end;
    end;
    for K := 0 to (PP.Count div 2) * 2 - 1 do
      Lines.Add(PP[K]);
  end;
  PP.Free;
end;

type
  THatchingDirection = array[1..4] of Single;

const
  HatchingDirections: array[0..6] of
  THatchingDirection = ((0, 0, 0, 0), (0, 1, 0, 0),
    (1, 0, 0, 0), (1, 1, 0, 0), (1, -1, 0, 0), (1, 0, 0, 1),
    (1, 1, 1, -1));

procedure T_PostScript_Export.WriteHatching(const P:
  TPointsSet2D;
  Hatching: THatching; Step: TRealType);
var
  Lines: TPointsSet2D;
  DX, DY: TRealType;
  procedure WriteHatching0;
  var
    I: Integer;
  begin
    if (DX = 0) and (DY = 0) then Exit;
    CalculateHatching(P, DX, DY, Step, Lines);
    WriteStream('newpath ');
    with fDrawing2D do WriteStream(
        Format('%.2g setlinewidth [] 0 setdash ',
        [LineWidth / 2 * fFactorMM]));
    for I := 0 to Lines.Count div 2 - 1 do
    begin
      WriteStreamPoint(Lines[I * 2]);
      WriteStream('moveto ');
      WriteStreamPoint(Lines[I * 2 + 1]);
      WriteStream('lineto ');
      if (I mod 5) = 4 then WriteLnStream('');
    end;
    WriteLnStream('stroke');
  end;
begin
  if Hatching = haNone then Exit;
  Lines := TPointsSet2D.Create(10);
  DX := HatchingDirections[Ord(Hatching)][1];
  DY := HatchingDirections[Ord(Hatching)][2];
  WriteHatching0;
  DX := HatchingDirections[Ord(Hatching)][3];
  DY := HatchingDirections[Ord(Hatching)][4];
  Lines.Clear;
  WriteHatching0;
  Lines.Free;
end;

procedure T_PostScript_Export.WriteRectangle2D(Obj:
  TRectangle2D);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    try
      WriteHatching(ProfilePoints, Hatching,
        fHatchingStep);
      WritePoly(ProfilePoints, LineKind, True);
    finally
      EndUseProfilePoints;
    end;
  end;
end;

procedure T_PostScript_Export.WriteText2D(Obj: TText2D);
var
  P: TPoint2D;
  Point: TPoint2D;
  Rect: TRect2D;
  Width, W, H: TRealType;
  St: string;
begin
  with Obj do
  begin
    Rect := GetExtension;
    Width := (Rect.Right - Rect.Left);
    P := Points[0];
    Point := ConvertPnt(P);
    W := Width * fFactorW;
    case HJustification of
      //jhLeft: Point.X := Point.X;
      jhCenter: Point.X := Point.X - Round(W / 2);
      jhRight: Point.X := Point.X - Round(W);
    end;
    H := Height * fFactorH / 1.2;
    case VJustification of
      //jvBottom:
      jvCenter: Point.Y := Point.Y - H / 2;
      jvTop: Point.Y := Point.Y - H;
    end;
    Point.Y := Point.Y + H * 0.15;
    St := AnsiReplaceText(Text, '\', '\\');
    St := AnsiReplaceText(St, '(', '\(');
    St := AnsiReplaceText(St, ')', '\)');
    //WriteStreamPoint(P);
    WriteStream(Format('/%s findfont %d scalefont setfont newpath ',
      [FontName, Round(H)]));
    WriteStreamPoint0(Point.X, Point.Y);
    WriteLnStream('moveto ');
    //"string" stringwidth -> "wx" "wy"
    {stringwidth returns the length of the string ( ... ) and (usually) the value 0.0
    ex: the following code will determine the width of a string and center it on some background that is 200 units wide
    (PostScript) stringwidth pop
    200 exch sub 2 div
    0 rmoveto
    (PostScript) show    }
    case HJustification of
      //jhLeft: Point.X := Point.X;
      jhCenter:
        WriteLnStream(Format('(%s) stringwidth pop %.1f exch sub 2 div 0 rmoveto',
          [St, W]));
      jhRight:
        WriteLnStream(Format('(%s) stringwidth pop %.1f exch sub 0 rmoveto',
          [St, W]));
    end;
    WriteLnStream(Format('(%s) show stroke', [St]));
  end;
end;

procedure T_PostScript_Export.WriteStar2D(Obj: TStar2D);
var
  P: TPoint2D;
  StarsSize: TRealType;
begin
  with Obj do
  begin
    WriteStream('newpath ');
    WriteLineThickness(Obj.LineKind);
    P := Points[0];
    WriteStreamPoint(P);
    if Obj.OwnerCAD is TDrawing2D then
      StarsSize := (Obj.OwnerCAD as TDrawing2D).StarsSize
    else StarsSize := 1;
    WriteLnStream(Format('%.1f 0 360 arc fill', [StarsSize *
      fFactorW]));
  end;
end;

procedure T_PostScript_Export.WriteEllipse2D(Obj: TEllipse2D);
var
  PP: TPointsSet2D;
begin
  if Obj.Points.Count < 2 then Exit;
  PP := nil;
  try
    Obj.BezierPoints(PP, IdentityTransf2D);
    WriteBezierPath(PP, Obj.LineKind);
  finally
    PP.Free;
  end;
  with Obj do
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints, Obj.Hatching, fHatchingStep);
      EndUseProfilePoints;
    end;
end;

procedure T_PostScript_Export.WriteCircle2D(Obj: TCircle2D);
var
  CP: TPoint2D;
  R: TRealType;
begin
  with Obj do
  begin
    //if LineKind <> liNone then
    begin
      CP := Points[0];
      R := PointDistance2D(CP, Points[1]);
      WriteStream('newpath ');
      WriteLineThickness(Obj.LineKind);
      WriteStreamPoint(CP);
      WriteLnStream(Format('%.1f 0 360 arc stroke', [R *
        fFactorW]));
    end;
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints, Hatching, fHatchingStep);
      EndUseProfilePoints;
    end;
  end;
end;

procedure T_PostScript_Export.WriteCircular2D(Obj: TCircular2D);
var
  CP: TPoint2D;
  R, SA, EA: TRealType;
begin
  Obj.GetArcParams(CP.X, CP.Y, R, SA, EA);
  {ATmp := SA;
  SA := -EA;
  EA := -ATmp;
  SA := SA - Floor(SA / (2 * Pi)) * 2 * Pi;
  EA := EA - Floor(EA / (2 * Pi)) * 2 * Pi;}
  if EA < SA then EA := EA + 2 * Pi;
  with Obj do
  begin
    //if LineKind <> liNone then
    begin
      WriteStream('newpath ');
      WriteLineThickness(Obj.LineKind);
      if Obj is TSector2D then
      begin
        WriteStreamPoint(CP);
        WriteStream('moveto ');
      end;
      WriteStreamPoint(CP);
      WriteStream(Format('%.1f %.1f %.1f arc ',
        [R * fFactorW, SA * 180 / Pi, EA * 180 / Pi]));
      if Obj is TSector2D then
      begin
        WriteStream('closepath ');
      end
      else if Obj is TSegment2D then
        WriteStream('closepath ');
      WriteLnStream('stroke');
    end;
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints, Obj.Hatching, fHatchingStep);
      EndUseProfilePoints;
    end;
  end;
end;

procedure T_PostScript_Export.WriteSpline2D(Obj: TSpline2D0);
var
  PP: TPointsSet2D;
begin
  if Obj.Points.Count < 2 then Exit;
  PP := nil;
  try
    Obj.BezierPoints(PP, IdentityTransf2D);
    WriteBezierPath(PP, Obj.LineKind);
  finally
    PP.Free;
  end;
  with Obj do
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints, Obj.Hatching, fHatchingStep);
      EndUseProfilePoints;
    end;
end;

procedure T_PostScript_Export.WriteSmooth2D(Obj:
  TSmoothPath2D0);
var
  PP: TPointsSet2D;
begin
  if Obj.Points.Count < 2 then Exit;
  PP := nil;
  try
    Obj.BezierPoints(PP, IdentityTransf2D);
    WriteBezierPath(PP, Obj.LineKind);
  finally
    PP.Free;
  end;
  with Obj do
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints, Obj.Hatching, fHatchingStep);
      EndUseProfilePoints;
    end;
end;

procedure T_PostScript_Export.WritePoly2D(Obj: TPolyline2D0);
var
  PP: TPointsSet2D;
begin
  if Obj.Points.Count = 0 then Exit;
  PP := Obj.Points;
  WritePoly(PP, Obj.LineKind, Obj.IsClosed);
  WriteHatching(PP, Obj.Hatching, fHatchingStep);
end;

procedure T_PostScript_Export.WriteToTpX(Stream: TStream;
  const FileName: string);
begin
  fStream := nil;
  StoreToFile(ChangeFileExt(FileName, '.eps'));
  fStream := Stream;
  try
    WriteLnStream(Format(
      '  \includegraphics{%s}', //[width=%.1fcm,height=%.1fcm]
      [//fDrawing2D.PicWidth / 10, fDrawing2D.PicHeight / 10,
      ChangeFileExt(ExtractFileName(FileName), '')]));
  finally
    fStream := nil;
  end;
end;

procedure T_PostScript_Export.StoreToClipboard;
begin
  fStream.Free;
  fStream := TMemoryStream.Create;
  try
    WriteAll;
    fStream.Position := 0;
    PutStreamToClipboard0(CF_TEXT, fStream, fStream.Size);
  finally
    fStream.Free;
    fStream := nil;
  end;
end;

{ --================ T_TeX_Picture_Export ==================-- }

constructor T_TeX_Picture_Export.Create(CAD: TDrawing2D);
begin
  inherited Create(CAD);
end;

procedure T_TeX_Picture_Export.WriteStreamPoint0(X, Y:
  TRealType);
begin
  WriteStreamPoint0Int(Round(X), Round(Y));
end;

procedure T_TeX_Picture_Export.WriteLineThickness(Obj:
  TPrimitive2D);
begin
  case Obj.LineKind of
    liThick: WriteStream('\thicklines');
    liThin: WriteStream('\thinlines');
    liDashed: WriteStream('\thinlines');
    liDotted: WriteStream('\thicklines');
  end;
end;

procedure T_TeX_Picture_Export.WriteLine(P0, P1: TPoint2D;
  Kind: TLineKind);
begin
  if IsSamePoint2D(P0, P1) then
  begin
    WriteStream('\put');
    WriteStreamPoint(P0);
    WriteStream('{\picsquare}');
    Exit;
  end;
  if PointDistance2D(P0, P1) * fFactorW <
    fDrawing2D.TeXMinLine then
  begin
    WriteStream('\put');
    WriteStreamPoint(P0);
    WriteStream('{\picsquare}');
    WriteStream('\put');
    WriteStreamPoint(P1);
    WriteStream('{\picsquare}');
    Exit;
  end;
  case Kind of
    liThin, liThick:
      begin
        WriteStream('\lbezier');
        WriteStreamPoint(P0);
        WriteStreamPoint(P1);
      end;
    liDotted:
      begin
        WriteStream('\dottedline{');
        WriteStream(Round(fDrawing2D.DottedSize /
          fDrawing2D.PicUnitLength));
        WriteStream('}');
        WriteStreamPoint(P0);
        WriteStreamPoint(P1);
      end;
    liDashed:
      begin
        WriteStream('\dashline[33]{');
        WriteStream(Round(fDrawing2D.DashSize /
          fDrawing2D.PicUnitLength / 2));
        WriteStream('}');
        WriteStreamPoint(P0);
        WriteStreamPoint(P1);
      end;
  end;
end;

procedure T_TeX_Picture_Export.WriteHeader;
var
  Rect: TRect2D;
begin
  fUnitLength := fDrawing2D.PicUnitLength;
  fW := fDrawing2D.PicWidth / fUnitLength;
  fH := fDrawing2D.PicHeight / fUnitLength;
  Rect := fDrawing2D.DrawingExtension;
  with Rect do
  begin
    fExtLeft := Left;
    fExtBottom := Bottom;
    if (Top <> Bottom) and (Right <> Left) then
    begin
      fW := Round(Min((Right - Left)
        / (Top - Bottom) * fH, fW));
      fH := Round(Min((Top - Bottom)
        / (Right - Left) * fW, fH));
      fFactorW := fW / (Right - Left);
      fFactorH := fH / (Top - Bottom);
    end;
  end;
  fHatchingStep := fDrawing2D.HatchingStep;
  WriteLnStream('\clearpage');
  WriteLnStream(Format(
    '\setlength{\unitlength}{%.4g mm}', [fUnitLength]));
  WriteStream('\begin{picture}');
  WriteStreamPoint0(fW, fH);
  WriteLnStream('(0,0)');
end;

procedure T_TeX_Picture_Export.WriteFooter;
begin
  WriteLnStream('\end{picture}');
  WriteLnStream('\clearpage');
end;

procedure T_TeX_Picture_Export.WriteLine2D(Obj: TLine2D);
var
  PP: TPointsSet2D;
  I: Integer;
begin
  WriteLineThickness(Obj);
  PP := TPointsSet2D.Create(10);
  Obj.FillPoints(PP);
  for I := 0 to PP.Count - 2 do
    WriteLine(PP[I], PP[I + 1], Obj.LineKind);
  PP.Free;
  WriteLnStream('');
end;

procedure T_TeX_Picture_Export.WriteHatching(const P:
  TPointsSet2D;
  Hatching: THatching; Step: TRealType);
var
  Lines: TPointsSet2D;
  DX, DY: TRealType;
  IDX, IDY, Len: Integer;
  P1, P2, Point: TPoint2D;
  procedure WriteHatching0;
  var
    I: Integer;
  begin
    if (DX = 0) and (DY = 0) then Exit;
    CalculateHatching(P, DX, DY,
      Step / fFactorH / fUnitLength, Lines);
    if (Frac(DX) <> 0) or (Frac(DY) <> 0) then
      for I := 0 to Lines.Count div 2 - 1 do
      begin
        WriteStream('\drawline');
      //WriteStream('\lbezier');
        WriteStreamPoint(Lines[I * 2]);
        WriteStreamPoint(Lines[I * 2 + 1]);
      end
    else
    begin
      IDX := Round(DX);
      IDY := Round(DY);
      for I := 0 to Lines.Count div 2 - 1 do
      begin
      {WriteStream('\drawline');
      WriteStreamPoint(Lines[I * 2]);
      WriteStreamPoint(Lines[I * 2 + 1]);}
      {WriteStream('\put');
      WriteStreamPoint(Lines[I * 2]);
      WriteStream('{\line');
      WriteStreamPoint0(-IDY,IDX);
      WriteStream('{');
      WriteStream(Len);}
      //WriteStream('}}');
        P1 := Lines[I * 2];
        P1.X := P1.X - 2.5 * DY / fFactorW;
        P1.Y := P1.Y + 2.5 * DX / fFactorH;
        Point := ConvertPnt(P1);
        Point := Point2D(Round(Point.X), Round(Point.Y));
        P1 := ConvertPntInv(Point);
        P2 := Lines[I * 2 + 1];
        P2.X := P2.X + 2.5 * DY / fFactorW;
        P2.Y := P2.Y - 2.5 * DX / fFactorH;
        P2 := ConvertPntInv(ConvertPnt(P2));
        if DY <> 0 then
          Len := Trunc(fFactorW * (P1.X - P2.X) / DY)
        else Len := Trunc(fFactorH * (P2.Y - P1.Y) / DX);
        if Len > 0 then
        begin
          //WriteStream('\drawline');
          WriteStream('\lbezier');
          WriteStreamPoint0(Point.X, Point.Y);
          WriteStreamPoint0(Point.X - IDY * Len,
            Point.Y + IDX * Len);
        end;
      end;
    end;
  end;
begin
  if Hatching = haNone then Exit;
  WriteStream('\thinlines');
  Lines := TPointsSet2D.Create(10);
  DX := HatchingDirections[Ord(Hatching)][1];
  DY := HatchingDirections[Ord(Hatching)][2];
  WriteHatching0;
  DX := HatchingDirections[Ord(Hatching)][3];
  DY := HatchingDirections[Ord(Hatching)][4];
  Lines.Clear;
  WriteHatching0;
  Lines.Free;
end;

procedure T_TeX_Picture_Export.WriteRectangle2D(Obj:
  TRectangle2D);
var
  A: TRealType;
  P: TPointsSet2D;
  P3, P4: TPoint2D;
begin
  WriteLineThickness(Obj);
  P := TPointsSet2D.Create(4);
  with Obj do
  begin
    P[0] := Points[0];
    P[2] := Points[1];
    RectangleCalcPoints(P[0], P[2], Points[2], P3, P4, A);
    P[1] := P3;
    P[3] := P4;
    WriteLine(P[0], P[1], Obj.LineKind);
    WriteLine(P[1], P[2], Obj.LineKind);
    WriteLine(P[2], P[3], Obj.LineKind);
    WriteLine(P[3], P[0], Obj.LineKind);
  end;
  WriteHatching(P, Obj.Hatching, fHatchingStep);
  P.Free;
  WriteLnStream('');
end;

{
 FONT          size                 baselineskip
               10pt   11pt  12pt    10pt   11pt   12pt

\tiny          5pt    6pt   6pt     6pt    7pt    7pt
\scriptsize    7pt    8pt   8pt     8pt    9.5pt  9.5pt
\footnotesize  8pt    9pt   10pt    9.5pt  11pt   12pt
\small         9pt    10pt  11pt    11pt   12pt   13.6pt
\normalsize    10pt   11pt  12pt    12pt   13.6pt 14.5pt
\large         12pt   12pt  14pt    14pt   14pt   18pt
\Large         14pt   14pt  17pt    18pt   18pt   22pt
\LARGE         17pt   17pt  20pt    22pt   22pt   25pt
\huge          20pt   20pt  25pt    25pt   25pt   30pt
\Huge          25pt   25pt  25pt    30pt   30pt   30pt
}

{\tiny 5 \scriptsize 7 \footnotesize 8 \small 9 \normalsize 10
\large 12 \Large 14.4 \LARGE 17.28 \huge 20.74 \Huge 24.88}

function TeX_Replace_Special(const St: string): string;
// TeX special characters # \# $ \$ % \% & \& ~ \verb _ \_ ^ \^ \ $\backslash$ { \{ } \}
begin
  Result := AnsiReplaceStr(St, '\', '\backs-lash');
  Result := AnsiReplaceStr(Result, '#', '\#');
  Result := AnsiReplaceStr(Result, '$', '\$');
  Result := AnsiReplaceStr(Result, '%', '\%');
  Result := AnsiReplaceStr(Result, '&', '\&');
  Result := AnsiReplaceStr(Result, '{', '\{');
  Result := AnsiReplaceStr(Result, '}', '\}');
  Result := AnsiReplaceStr(Result, '~', '\symbol{126}'); //\~
  Result := AnsiReplaceStr(Result, '_', '\symbol{95}'); //\_
  Result := AnsiReplaceStr(Result, '^', '\symbol{94}'); // \^
  Result := AnsiReplaceStr(Result, '\backs-lash',
    '$\backslash$');
end;

procedure T_TeX_Picture_Export.WriteText2D(Obj: TText2D);
var
  P, Point: TPoint2D;
  Rect: TRect2D;
  Width, H: TRealType;
  St: string;
begin
  with Obj do
  begin
    WriteStream('\put');
    Rect := GetExtension;
    Width := (Rect.Right - Rect.Left);
    P := Points[0];
    Point := ConvertPnt(P);
    case HJustification of
      //jhLeft: Point.X := Point.X;
      jhCenter: Point.X := Point.X
        - Round(Width * fFactorW / 2);
      jhRight: Point.X := Point.X - Round(Width * fFactorW);
    end;
    case VJustification of
      //jvBottom:
      jvCenter: Point.Y := Point.Y
        - Round(Height * fFactorH / 2);
      jvTop: Point.Y := Point.Y - Round(Height * fFactorH);
    end;
    WriteStreamPoint0(Point.X, Point.Y);
    //WriteStreamPoint(P);
    WriteStream('{');
    H := Height * fFactorH;
    H := H * fUnitLength; //in mm
    H := H * 2.845; //in pt // mm=2.845pt
{\tiny 6 \scriptsize 8 \footnotesize 9.5 \small 11 \normalsize 12
\large 14 \Large 18 \LARGE 22 \huge 25 \Huge 30}
    {if H < 7 then WriteStream('\tiny')
    else if H < 8.7 then WriteStream('\scriptsize')
    else if H < 10.2 then WriteStream('\footnotesize')
    else if H < 11.5 then WriteStream('\small')
    else if H < 13 then WriteStream('\normalsize')
    else if H < 16 then WriteStream('\large')
    else if H < 20 then WriteStream('\Large')
    else if H < 23.3 then WriteStream('\LARGE')
    else if H < 27.3 then WriteStream('\huge')
    else WriteStream('\Huge');}
{\tiny 7 \scriptsize 9.5 \footnotesize 12 \small 13.6 \normalsize 14.5
\large 18 \Large 22 \LARGE 25 \huge 30 \Huge 30}
    if H < 8.2 then WriteStream('\tiny')
    else if H < 10.7 then WriteStream('\scriptsize')
    else if H < 12.8 then WriteStream('\footnotesize')
    else if H < 14 then WriteStream('\small')
    else if H < 16 then WriteStream('\normalsize')
    else if H < 20 then WriteStream('\large')
    else if H < 23.3 then WriteStream('\Large')
    else if H < 27.3 then WriteStream('\LARGE')
    else if H < 33 then WriteStream('\huge')
    else WriteStream('\Huge');
    //WriteStream('\fontsize{');    WriteStream(Round(H));    WriteStream('}{');    WriteStream(Round(H * 1.2));    //WriteStream('}');
    WriteStream('\makebox');
    WriteStreamPoint0(Width * fFactorW, Height * fFactorH);
    WriteStream('[');
//    WriteStream('{\framebox(0,0)[');
//    WriteStream('lb');
    case HJustification of
      jhLeft: WriteStream('l');
      jhCenter: WriteStream('c');
      jhRight: WriteStream('r');
    end;
    case VJustification of
      jvBottom: WriteStream('c'); //b
      jvCenter: WriteStream('c');
      jvTop: WriteStream('c'); //t
    end;
    if TeXText <> '' then St := TeXText
    else St := TeX_Replace_Special(Text);
    WriteStream(']{' + St);
    WriteStream('}}');
  end;
  WriteLnStream('');
end;

procedure T_TeX_Picture_Export.WriteStar2D(Obj: TStar2D);
var
  P: TPoint2D;
  StarsSize: TRealType;
  D: Integer;
begin
  WriteStream('\put');
  with Obj do
  begin
    P := Points[0];
    WriteStreamPoint(P);
    WriteStream('{\thinlines\circle*{');
    if Obj.OwnerCAD is TDrawing2D then
      StarsSize := (Obj.OwnerCAD as TDrawing2D).StarsSize
    else StarsSize := 1;
    D := Round(2 * StarsSize * fFactorW);
    WriteStream(D);
    WriteStream('}}');
    //\put(973,973){\thinlines\circle*{60}}
  end;
  WriteLnStream('');
end;

procedure T_TeX_Picture_Export.WriteEllipse2D(Obj: TEllipse2D);
var
  P: TPointsSet2D;
  P3, P4: TPoint2D;
  A: TRealType;
  procedure WriteArc(P0, P1, P2: TPoint2D);
  begin
    WriteStream('\rqbezier');
    WriteStreamPoint(P0);
    WriteStreamPoint(P1);
    WriteStreamPoint(P2);
    WriteStream('(');
    WriteStream(FF(1 / Sqrt(2)));
    WriteStream(')');
  end;
begin
  WriteLineThickness(Obj);
  with Obj do
  begin
    RectangleCalcPoints(Points[0], Points[1], Points[2],
      P3, P4, A);
    P := TPointsSet2D.Create(8);
    P[0] := Points[0];
    P[2] := P3;
    P[4] := Points[1];
    P[6] := P4;
    P[1] := MidPoint(P[0], P[2]);
    P[3] := MidPoint(P[2], P[4]);
    P[5] := MidPoint(P[4], P[6]);
    P[7] := MidPoint(P[6], P[0]);
    WriteArc(P[1], P[2], P[3]);
    WriteArc(P[3], P[4], P[5]);
    WriteArc(P[5], P[6], P[7]);
    WriteArc(P[7], P[0], P[1]);
    P.Free;
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints, Hatching, fHatchingStep);
      EndUseProfilePoints;
    end;
  end;
  WriteLnStream('');
end;

procedure T_TeX_Picture_Export.WriteCircle2D(Obj: TCircle2D);
var
  P: TPointsSet2D;
  CP: TPoint2D;
  R: TRealType;
  procedure WriteArc(P0, P1, P2: TPoint2D);
  begin
    WriteStream('\rqbezier');
    WriteStreamPoint(P0);
    WriteStreamPoint(P1);
    WriteStreamPoint(P2);
    WriteStream('(');
    WriteStream(FF(1 / Sqrt(2)));
    WriteStream(')');
  end;
begin
  WriteLineThickness(Obj);
  with Obj do
  begin
    CP := Points[0];
    R := PointDistance2D(CP, Points[1]);
    P := TPointsSet2D.Create(8);
    P[0] := Point2D(CP.X - R, CP.Y - R);
    P[1] := Point2D(CP.X - R, CP.Y);
    P[2] := Point2D(CP.X - R, CP.Y + R);
    P[3] := Point2D(CP.X, CP.Y + R);
    P[4] := Point2D(CP.X + R, CP.Y + R);
    P[5] := Point2D(CP.X + R, CP.Y);
    P[6] := Point2D(CP.X + R, CP.Y - R);
    P[7] := Point2D(CP.X, CP.Y - R);
    WriteArc(P[1], P[2], P[3]);
    WriteArc(P[3], P[4], P[5]);
    WriteArc(P[5], P[6], P[7]);
    WriteArc(P[7], P[0], P[1]);
    P.Free;
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints, Hatching, fHatchingStep);
      EndUseProfilePoints;
    end;
  end;
  WriteLnStream('');
end;

{
  P1, P2, P3: TPoint2D;
  Point2, Point3: TPoint;
  D: Integer;
 WriteStream('\put');
    P1 := Point2D(CX, CY);
    P2 := Point2D(0, 0);
    P3 := Point2D(RX * 2, RY * 2),
      ModelTransform);
    Point2 := ConvertPnt(P2);
    Point3 := ConvertPnt(P3);
    D := (Point3.X - Point2.X + Point3.Y - Point2.Y) div 2;}
    // TeX uses clockwise angle measuring
{    WriteStreamPoint(P1);
    WriteStream('{\arc{');
    WriteStream(D);
    WriteStream('}{');
    WriteStream(SA);
    WriteStream('}{');
    WriteStream(EA);}
 //   WriteStream('}}');

procedure T_TeX_Picture_Export.WriteCircular2D(Obj:
  TCircular2D);
var
  J, NSeg: Integer;
  CP, P0, P1, P2: TPoint2D;
  CX, CY, R, SA, EA, ATmp,
    A, A0, A1, A2: TRealType;
  function GetPoint(A, R: TRealType): TPoint2D;
  begin
    Result := Point2D(CP.X + R * Cos(A),
      CP.Y - R * Sin(A));
  end;
begin
  WriteLineThickness(Obj);
  Obj.GetArcParams(CX, CY, R, SA, EA);
  ATmp := SA;
  SA := -EA;
  EA := -ATmp;
  SA := SA - Floor(SA / (2 * Pi)) * 2 * Pi;
  EA := EA - Floor(EA / (2 * Pi)) * 2 * Pi;
  if EA < SA then EA := EA + 2 * Pi;
  with Obj do
  begin
    CP := Point2D(CX, CY);
    A := EA - SA;
    if A < 2 * Pi / 3 then NSeg := 1
    else if A < 4 * Pi / 3 then NSeg := 2
    else NSeg := 3;
    A := A / NSeg / 2;
    for J := 1 to NSeg do
    begin
      A2 := SA + J * 2 * A;
      A0 := A2 - 2 * A;
      A1 := A2 - A;
      P0 := GetPoint(A0, R);
      P1 := GetPoint(A1, R / Cos(A));
      P2 := GetPoint(A2, R);
      WriteStream('\rqbezier');
      WriteStreamPoint(P0);
      WriteStreamPoint(P1);
      WriteStreamPoint(P2);
      WriteStream('(');
      WriteStream(FF(Cos(A)));
      WriteStream(')');
      if Obj is TSector2D then
      begin
        WriteLine(CP, GetPoint(SA, R), Obj.LineKind);
        WriteLine(CP, GetPoint(EA, R), Obj.LineKind);
      end
      else if Obj is TSegment2D then
        WriteLine(GetPoint(SA, R), GetPoint(EA, R),
          Obj.LineKind);
    end;
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints, Obj.Hatching, fHatchingStep);
      EndUseProfilePoints;
    end;
  end;
  WriteLnStream('');
end;

procedure T_TeX_Picture_Export.WriteSpline2D(Obj: TSpline2D0);
begin
  if Obj is TBSpline2D
    then WriteBSpline2D(Obj as TBSpline2D)
  else if Obj is TCubicBSpline2D
    then WriteCubicSpline2D(Obj as TCubicBSpline2D)
  else if Obj is TClosedBSpline2D
    then WriteClosedBSpline2D(Obj as TClosedBSpline2D)
  else if Obj is TClosedCubicBSpline2D then
    WriteCubicSpline2D(Obj as TClosedCubicBSpline2D);
end;

procedure T_TeX_Picture_Export.WriteBSpline2D(Obj: TBSpline2D);
var
  I: Integer;
  PP: TPointsSet2D;
begin
  WriteLineThickness(Obj);
  PP := Obj.Points;
  for I := 1 to PP.Count - 2 do
  begin
    WriteStream('\qbezier');
    if I = 1 then WriteStreamPoint(PP[0])
    else WriteStreamPoint(Point2D(
        (PP[I - 1].X + PP[I].X) / 2,
        (PP[I - 1].Y + PP[I].Y) / 2));
    WriteStreamPoint(PP[I]);
    if I = PP.Count - 2
      then WriteStreamPoint(PP[I + 1])
    else WriteStreamPoint(Point2D(
        (PP[I + 1].X + PP[I].X) / 2,
        (PP[I + 1].Y + PP[I].Y) / 2));
  end;
  with Obj do
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints, Obj.Hatching, fHatchingStep);
      EndUseProfilePoints;
    end;
  WriteLnStream('');
end;

procedure T_TeX_Picture_Export.WriteClosedBSpline2D(Obj:
  TClosedBSpline2D);
var
  I, J: Integer;
  PP: TPointsSet2D;
begin
  WriteLineThickness(Obj);
  PP := Obj.Points;
  for I := 0 to PP.Count - 1 do
  begin
    WriteStream('\qbezier');
    J := I - 1;
    if J = -1 then J := PP.Count - 1;
    WriteStreamPoint(Point2D(
      (PP[J].X + PP[I].X) / 2,
      (PP[J].Y + PP[I].Y) / 2));
    WriteStreamPoint(PP[I]);
    J := I + 1;
    if J = PP.Count then J := 0;
    WriteStreamPoint(Point2D(
      (PP[J].X + PP[I].X) / 2,
      (PP[J].Y + PP[I].Y) / 2));
  end;
  with Obj do
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints, Obj.Hatching, fHatchingStep);
      EndUseProfilePoints;
    end;
  WriteLnStream('');
end;

procedure T_TeX_Picture_Export.WriteCubicSpline2D(Obj:
  TSpline2D0);
var
  I: Integer;
  PP: TPointsSet2D;
  P0: TPoint2D;
begin
  WriteLineThickness(Obj);
  if Obj.Points.Count < 2 then Exit;
  PP := nil;
  try
    Obj.BezierPoints(PP, IdentityTransf2D);
    P0 := PP[0];
    for I := 1 to PP.Count - 1 do
    begin
      if I mod 3 = 1 then
      begin
        WriteStream('\cbezier');
        WriteStreamPoint(P0);
      end;
      WriteStreamPoint(PP[I]);
      if I mod 3 = 0 then P0 := PP[I];
    end;
  finally
    PP.Free;
  end;
  with Obj do
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints, Obj.Hatching, fHatchingStep);
      EndUseProfilePoints;
    end;
  WriteLnStream('');
end;

procedure T_TeX_Picture_Export.WriteSmooth2D(Obj:
  TSmoothPath2D0);
var
  I: Integer;
  PP: TPointsSet2D;
  P0: TPoint2D;
begin
  WriteLineThickness(Obj);
  if Obj.Points.Count < 2 then Exit;
  PP := nil;
  try
    Obj.BezierPoints(PP, IdentityTransf2D);
    P0 := PP[0];
    for I := 1 to PP.Count - 1 do
    begin
      if I mod 3 = 1 then
      begin
        WriteStream('\cbezier');
        WriteStreamPoint(P0);
      end;
      WriteStreamPoint(PP[I]);
      if I mod 3 = 0 then P0 := PP[I];
    end;
  finally
    PP.Free;
  end;
  with Obj do
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints, Obj.Hatching, fHatchingStep);
      EndUseProfilePoints;
    end;
  WriteLnStream('');
end;

procedure T_TeX_Picture_Export.WritePoly2D(Obj: TPolyline2D0);
var
  I: Integer;
  PP: TPointsSet2D;
begin
  WriteLineThickness(Obj);
  if Obj.Points.Count = 0 then Exit;
  PP := Obj.Points;
  for I := 0 to PP.Count - 2 do
  begin
    WriteLine(PP[I], PP[I + 1], Obj.LineKind);
    if (I mod 100) = 99 then
    begin
      WriteLnStream('');
      WriteStream(' ');
    end;
  end;
  if Obj is TPolygon2D then
    WriteLine(PP[PP.Count - 1], PP[0], Obj.LineKind);
  WriteHatching(PP, Obj.Hatching, fHatchingStep);
  WriteLnStream('');
end;

procedure T_TeX_Picture_Export.StoreToClipboard;
begin
  fStream.Free;
  fStream := TMemoryStream.Create;
  try
    WriteAll;
    fStream.Position := 0;
    PutStreamToClipboard0(CF_TEXT, fStream, fStream.Size);
  finally
    fStream.Free;
    fStream := nil;
  end;
end;

{ --================ T_PSTricks_Export ==================-- }

constructor T_PSTricks_Export.Create(CAD: TDrawing2D);
begin
  inherited Create(CAD);
end;

procedure T_PSTricks_Export.WriteStreamPoint0(X, Y: TRealType);
begin
  WriteStreamPoint0Int(Round(X), Round(Y));
end;

function T_PSTricks_Export.LineArg(Kind: TLineKind):
  string;
begin
  case Kind of
    liThick, liDotted: Result := Format('linewidth=%.2fmm',
        [fDrawing2D.LineWidth * 2]);
    liThin, liDashed: Result :=
      Format('linewidth=%.2fmm', [fDrawing2D.LineWidth]);
  end;
  case Kind of
    liNone:
      Result := Result + 'linestyle=none';
    liDotted:
      Result := Result + ',linestyle=dotted';
    liDashed:
      Result := Result + ',linestyle=dashed';
  else
    Result := Result + ',linestyle=solid';
  end;
  Result := Format('[%s]', [Result]);
end;

procedure T_PSTricks_Export.WriteLine(P0, P1: TPoint2D;
  Kind: TLineKind);
begin
  if IsSamePoint2D(P0, P1) then Exit;
  WriteStream(Format('\psline%s', [LineArg(Kind)]));
  WriteStreamPoint(P0);
  WriteStreamPoint(P1);
end;

procedure T_PSTricks_Export.WritePoly(PP: TPointsSet2D;
  Kind: TLineKind; Closed: Boolean);
var
  I: Integer;
begin
  if PP.Count < 1 then Exit;
  if Closed then
    WriteStream(Format('\pspolygon%s', [LineArg(Kind)]))
  else
    WriteStream(Format('\psline%s', [LineArg(Kind)]));
  for I := 0 to PP.Count - 1 do
    WriteStreamPoint(PP[I]);
end;

procedure T_PSTricks_Export.WriteBezier(const P0, P1, P2, P3:
  TPoint2D; Kind: TLineKind);
begin
  WriteStream(Format('\psbezier%s', [LineArg(Kind)]));
  WriteStreamPoint(P0);
  WriteStreamPoint(P1);
  WriteStreamPoint(P2);
  WriteStreamPoint(P3);
end;

procedure T_PSTricks_Export.WriteBezierPath(const PP:
  TPointsSet2D; Kind: TLineKind);
var
  I: Integer;
  P0: TPoint2D;
begin
  if PP.Count < 1 then Exit;
  P0 := PP[0];
  for I := 0 to PP.Count div 3 - 1 do
  begin
    WriteBezier(PP[3 * I], PP[3 * I + 1], PP[3 * I + 2],
      PP[3 * I + 3], Kind);
  end;
end;

procedure T_PSTricks_Export.WriteHeader;
var
  Rect: TRect2D;
begin
  fUnitLength := fDrawing2D.PicUnitLength;
  fW := fDrawing2D.PicWidth / fUnitLength;
  fH := fDrawing2D.PicHeight / fUnitLength;
  Rect := fDrawing2D.DrawingExtension;
  with Rect do
  begin
    fExtLeft := Left;
    fExtBottom := Bottom;
    if (Top <> Bottom) and (Right <> Left) then
    begin
      fW := Round(Min((Right - Left)
        / (Top - Bottom) * fH, fW));
      fH := Round(Min((Top - Bottom)
        / (Right - Left) * fW, fH));
      fFactorW := fW / (Right - Left);
      fFactorH := fH / (Top - Bottom);
    end;
  end;
  fHatchingStep := fDrawing2D.HatchingStep;
  WriteLnStream('\clearpage');
  //WriteLnStream(Format('\psset{xunit=%.4g mm, yunit=%.4g mm, runit=%.4g mm}',    [fUnitLength, fUnitLength, fUnitLength]));
  WriteLnStream(Format('\psset{unit=%.4g mm}', [fUnitLength]));
  with fDrawing2D do
  begin
    WriteLnStream(Format('\psset{dotsize=%.2fmm 0}',
      [StarsSize]));
    //WriteLnStream(Format('\psset{hatchsep=%.2fmm}',      [HatchingStep]));
    WriteLnStream(Format('\psset{dotsep=%.2fmm}',
      [DottedSize]));
    WriteLnStream(Format('\psset{dash=%.2fmm %.2fmm}',
      [DashSize * 2, DashSize]));
    WriteLnStream(Format('\psset{linewidth=%.2fmm}',
      [LineWidth]));
    //WriteLnStream(Format('\psset{hatchwidth=%.2fmm}',      [LineWidth / 2]));
  end;
  WriteLnStream(Format('\begin {pspicture}(0,0)(%d,%d)',
    [Round(fW), Round(fH)]));
end;

procedure T_PSTricks_Export.WriteFooter;
begin
  WriteLnStream('\end{pspicture}');
  WriteLnStream('\clearpage');
end;

procedure T_PSTricks_Export.WriteLine2D(Obj: TLine2D);
var
  PP: TPointsSet2D;
begin
  PP := TPointsSet2D.Create(10);
  Obj.FillPoints(PP);
  WritePoly(PP, Obj.LineKind, False);
  PP.Free;
  WriteLnStream('');
end;

procedure T_PSTricks_Export.WriteHatching(const P: TPointsSet2D;
  Hatching: THatching; Step: TRealType);
var
  Lines: TPointsSet2D;
  DX, DY: TRealType;
  IDX, IDY, Len: Integer;
  P1, P2, Point: TPoint2D;
  procedure WriteHatching0;
  var
    I: Integer;
  begin
    if (DX = 0) and (DY = 0) then Exit;
    CalculateHatching(P, DX, DY,
      Step / fFactorH / fUnitLength, Lines);
    if (Frac(DX) <> 0) or (Frac(DY) <> 0) then
      for I := 0 to Lines.Count div 2 - 1 do
      begin
        WriteStream(Format('\psline[linewidth=%.2fmm,linestyle=solid]',
          [fDrawing2D.LineWidth / 2]));
        WriteStreamPoint(P[I * 2]);
        WriteStreamPoint(P[I * 2 + 1]);
      end
    else
    begin
      IDX := Round(DX);
      IDY := Round(DY);
      for I := 0 to Lines.Count div 2 - 1 do
      begin
        P1 := Lines[I * 2];
        P1.X := P1.X - 2.5 * DY / fFactorW;
        P1.Y := P1.Y + 2.5 * DX / fFactorH;
        Point := ConvertPnt(P1);
        Point := Point2D(Round(Point.X), Round(Point.Y));
        P1 := ConvertPntInv(Point);
        P2 := Lines[I * 2 + 1];
        P2.X := P2.X + 2.5 * DY / fFactorW;
        P2.Y := P2.Y - 2.5 * DX / fFactorH;
        P2 := ConvertPntInv(ConvertPnt(P2));
        if DY <> 0 then
          Len := Trunc(fFactorW * (P1.X - P2.X) / DY)
        else Len := Trunc(fFactorH * (P2.Y - P1.Y) / DX);
        if Len > 0 then
        begin
          WriteStream(Format('\psline[linewidth=%.2fmm,linestyle=solid]',
            [fDrawing2D.LineWidth / 2]));
          WriteStreamPoint0(Point.X, Point.Y);
          WriteStreamPoint0(Point.X - IDY * Len,
            Point.Y + IDX * Len);
        end;
      end;
    end;
  end;
begin
  if Hatching = haNone then Exit;
  Lines := TPointsSet2D.Create(10);
  DX := HatchingDirections[Ord(Hatching)][1];
  DY := HatchingDirections[Ord(Hatching)][2];
  WriteHatching0;
  DX := HatchingDirections[Ord(Hatching)][3];
  DY := HatchingDirections[Ord(Hatching)][4];
  Lines.Clear;
  WriteHatching0;
  Lines.Free;
end;

procedure T_PSTricks_Export.WriteRectangle2D(Obj: TRectangle2D);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    try
      WriteHatching(ProfilePoints, Hatching,
        fHatchingStep);
      WritePoly(ProfilePoints, LineKind, True);
    finally
      EndUseProfilePoints;
    end;
  end;
  WriteLnStream('');
end;

{
 FONT          size                 baselineskip
               10pt   11pt  12pt    10pt   11pt   12pt

\tiny          5pt    6pt   6pt     6pt    7pt    7pt
\scriptsize    7pt    8pt   8pt     8pt    9.5pt  9.5pt
\footnotesize  8pt    9pt   10pt    9.5pt  11pt   12pt
\small         9pt    10pt  11pt    11pt   12pt   13.6pt
\normalsize    10pt   11pt  12pt    12pt   13.6pt 14.5pt
\large         12pt   12pt  14pt    14pt   14pt   18pt
\Large         14pt   14pt  17pt    18pt   18pt   22pt
\LARGE         17pt   17pt  20pt    22pt   22pt   25pt
\huge          20pt   20pt  25pt    25pt   25pt   30pt
\Huge          25pt   25pt  25pt    30pt   30pt   30pt
}

{\tiny 5 \scriptsize 7 \footnotesize 8 \small 9 \normalsize 10
\large 12 \Large 14.4 \LARGE 17.28 \huge 20.74 \Huge 24.88}

procedure T_PSTricks_Export.WriteText2D(Obj: TText2D);
var
  P, Point: TPoint2D;
  Rect: TRect2D;
  Width, H: TRealType;
  St: string;
begin
  with Obj do
  begin
    WriteStream('\put');
    Rect := GetExtension;
    Width := (Rect.Right - Rect.Left);
    P := Points[0];
    Point := ConvertPnt(P);
    case HJustification of
      //jhLeft: Point.X := Point.X;
      jhCenter: Point.X := Point.X
        - Round(Width * fFactorW / 2);
      jhRight: Point.X := Point.X - Round(Width * fFactorW);
    end;
    case VJustification of
      //jvBottom:
      jvCenter: Point.Y := Point.Y
        - Round(Height * fFactorH / 2);
      jvTop: Point.Y := Point.Y - Round(Height * fFactorH);
    end;
    WriteStreamPoint0(Point.X, Point.Y);
    //WriteStreamPoint(P);
    WriteStream('{');
    H := Height * fFactorH;
    H := H * fUnitLength; //in mm
    H := H * 2.845; //in pt // mm=2.845pt
{\tiny 6 \scriptsize 8 \footnotesize 9.5 \small 11 \normalsize 12
\large 14 \Large 18 \LARGE 22 \huge 25 \Huge 30}
    {if H < 7 then WriteStream('\tiny')
    else if H < 8.7 then WriteStream('\scriptsize')
    else if H < 10.2 then WriteStream('\footnotesize')
    else if H < 11.5 then WriteStream('\small')
    else if H < 13 then WriteStream('\normalsize')
    else if H < 16 then WriteStream('\large')
    else if H < 20 then WriteStream('\Large')
    else if H < 23.3 then WriteStream('\LARGE')
    else if H < 27.3 then WriteStream('\huge')
    else WriteStream('\Huge');}
{\tiny 7 \scriptsize 9.5 \footnotesize 12 \small 13.6 \normalsize 14.5
\large 18 \Large 22 \LARGE 25 \huge 30 \Huge 30}
    if H < 8.2 then WriteStream('\tiny')
    else if H < 10.7 then WriteStream('\scriptsize')
    else if H < 12.8 then WriteStream('\footnotesize')
    else if H < 14 then WriteStream('\small')
    else if H < 16 then WriteStream('\normalsize')
    else if H < 20 then WriteStream('\large')
    else if H < 23.3 then WriteStream('\Large')
    else if H < 27.3 then WriteStream('\LARGE')
    else if H < 33 then WriteStream('\huge')
    else WriteStream('\Huge');
    //WriteStream('\fontsize{');    WriteStream(Round(H));    WriteStream('}{');    WriteStream(Round(H * 1.2));    //WriteStream('}');
    WriteStream('\makebox');
    WriteStreamPoint0(Width * fFactorW,
      Height * fFactorH);
    WriteStream('[');
//    WriteStream('{\framebox(0,0)[');
//    WriteStream('lb');
    case HJustification of
      jhLeft: WriteStream('l');
      jhCenter: WriteStream('c');
      jhRight: WriteStream('r');
    end;
    case VJustification of
      jvBottom: WriteStream('c'); //b
      jvCenter: WriteStream('c');
      jvTop: WriteStream('c'); //t
    end;
    if TeXText <> '' then St := TeXText
    else St := TeX_Replace_Special(Text);
    WriteStream(']{' + St);
    WriteStream('}}');
  end;
  WriteLnStream('');
end;

procedure T_PSTricks_Export.WriteStar2D(Obj: TStar2D);
var
  P: TPoint2D;
begin
  with Obj do
  begin
    P := Points[0];
    WriteStream('\psdots*[]');
    WriteStreamPoint(P);
  end;
  WriteLnStream('');
end;

procedure T_PSTricks_Export.WriteEllipse2D(Obj: TEllipse2D);
var
  PP: TPointsSet2D;
begin
  if Obj.Points.Count < 2 then Exit;
  PP := nil;
  try
    Obj.BezierPoints(PP, IdentityTransf2D);
    WriteBezierPath(PP, Obj.LineKind);
  finally
    PP.Free;
  end;
  with Obj do
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints, Obj.Hatching, fHatchingStep);
      EndUseProfilePoints;
    end;
  WriteLnStream('');
end;

procedure T_PSTricks_Export.WriteCircle2D(Obj: TCircle2D);
var
  CP: TPoint2D;
  R: TRealType;
begin
  with Obj do
  begin
    CP := Points[0];
    R := PointDistance2D(CP, Points[1]);
    WriteStream(Format('\pscircle%s', [LineArg(LineKind)]));
    WriteStreamPoint(CP);
    WriteStream(Format('{%d}', [Round(R * fFactorW)]));
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints, Hatching, fHatchingStep);
      EndUseProfilePoints;
    end;
  end;
  WriteLnStream('');
end;

procedure T_PSTricks_Export.WriteCircular2D(Obj: TCircular2D);
var
  CP: TPoint2D;
  R, SA, EA, Delt: TRealType;
  PP: TPointsSet2D;
  function GetPoint(A, R: TRealType): TPoint2D;
  begin
    Result := Point2D(CP.X + R * Cos(A),
      CP.Y + R * Sin(A));
  end;
begin
  Obj.GetArcParams(CP.X, CP.Y, R, SA, EA);
  if EA < SA then EA := EA + 2 * Pi;
  with Obj do
  begin
    if Obj is TSector2D then
      WriteStream(Format('\pswedge%s', [LineArg(LineKind)]))
    else
      WriteStream(Format('\psarc%s{-}', [LineArg(LineKind)]));
    WriteStreamPoint(CP);
    WriteStream(Format('{%d}{%.1f}{%.1f}',
      [Round(R * fFactorW), SA * 180 / Pi, EA * 180 / Pi]));
    if Obj is TSegment2D then
    begin
      PP := TPointsSet2D.Create(4);
      try
        Delt := fDrawing2D.LineWidth / fUnitLength
          / (R * fFactorW); // Draw miters
        PP.Add(GetPoint(SA + Delt, R));
        PP.Add(GetPoint(SA, R));
        PP.Add(GetPoint(EA, R));
        PP.Add(GetPoint(EA - Delt, R));
        WritePoly(PP, LineKind, False);
      finally
        PP.Free;
      end;
      //WriteLine(GetPoint(SA, R), GetPoint(EA, R),        LineKind);
    end;
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints, Obj.Hatching, fHatchingStep);
      EndUseProfilePoints;
    end;
  end;
  WriteLnStream('');
end;

procedure T_PSTricks_Export.WriteSpline2D(Obj: TSpline2D0);
var
  PP: TPointsSet2D;
begin
  if Obj.Points.Count < 2 then Exit;
  PP := nil;
  try
    Obj.BezierPoints(PP, IdentityTransf2D);
    WriteBezierPath(PP, Obj.LineKind);
  finally
    PP.Free;
  end;
  with Obj do
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints, Obj.Hatching, fHatchingStep);
      EndUseProfilePoints;
    end;
  WriteLnStream('');
end;

procedure T_PSTricks_Export.WriteSmooth2D(Obj: TSmoothPath2D0);
var
  PP: TPointsSet2D;
begin
  if Obj.Points.Count < 2 then Exit;
  PP := nil;
  try
    Obj.BezierPoints(PP, IdentityTransf2D);
    WriteBezierPath(PP, Obj.LineKind);
  finally
    PP.Free;
  end;
  with Obj do
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints, Obj.Hatching, fHatchingStep);
      EndUseProfilePoints;
    end;
  WriteLnStream('');
end;

procedure T_PSTricks_Export.WritePoly2D(Obj: TPolyline2D0);
begin
  with Obj do
  begin
    WriteHatching(Points, Hatching, fHatchingStep);
    WritePoly(Points, LineKind, IsClosed);
  end;
  WriteLnStream('');
end;

procedure T_PSTricks_Export.StoreToClipboard;
begin
  fStream.Free;
  fStream := TMemoryStream.Create;
  try
    WriteAll;
    fStream.Position := 0;
    PutStreamToClipboard0(CF_TEXT, fStream, fStream.Size);
  finally
    fStream.Free;
    fStream := nil;
  end;
end;

{ --================ T_MetaPost_Export ==================-- }

procedure T_MetaPost_Export.WriteStreamPoint0(X, Y:
  TRealType);
begin
  WriteStream(Format('(%.1fu,%.1fu)', [X, Y]));
end;

procedure T_MetaPost_Export.WriteLineThickness(Kind:
  TLineKind);
begin
  case Kind of
    liThick:
      WriteStream(Format(' withpen pencircle scaled %.2fmm',
        [fDrawing2D.LineWidth * 2]));
    liDotted:
      WriteStream(Format(' withpen pencircle scaled %.2fmm',
        [fDrawing2D.LineWidth * 2]));
    liThin:
      WriteStream(Format(' withpen pencircle scaled %.2fmm',
        [fDrawing2D.LineWidth])); //pensquare
    liDashed:
      WriteStream(Format(' withpen pencircle scaled %.2fmm',
        [fDrawing2D.LineWidth]));
  end;
  case Kind of
    liDotted:
      WriteStream(Format(' dashed dashpattern(on %.2fmm off %.2fmm)',
        [fDrawing2D.LineWidth * 2, fDrawing2D.DottedSize]));
    liDashed:
      WriteStream(Format(' dashed dashpattern(on %.2fmm off %.2fmm)',
        [fDrawing2D.DashSize * 2, fDrawing2D.DashSize]));
  end;
end;

procedure T_MetaPost_Export.WriteLine(P0, P1: TPoint2D;
  Kind: TLineKind);
begin
  if Kind = liNone then Exit;
  WriteStream('draw ');
  WriteStreamPoint(P0);
  WriteStream('--');
  WriteStreamPoint(P1);
  WriteLineThickness(Kind);
  WriteLnStream(';');
end;

procedure T_MetaPost_Export.WritePoly(PP: TPointsSet2D;
  Kind: TLineKind; Closed: Boolean);
var
  I: Integer;
begin
  if Kind = liNone then Exit;
  if PP.Count < 1 then Exit;
  WriteStream('draw ');
  WriteStreamPoint(PP[0]);
  for I := 1 to PP.Count - 1 do
  begin
    if (I mod 10) = 0 then WriteLnStream('  ');
    WriteStream('--');
    WriteStreamPoint(PP[I]);
  end;
  if Closed then WriteStream('--cycle');
  WriteLineThickness(Kind);
  WriteLnStream(';');
end;

procedure T_MetaPost_Export.WriteBezierPath(const PP:
  TPointsSet2D; Kind: TLineKind; Closed: Boolean);
var
  I: Integer;
begin
  if Kind = liNone then Exit;
  WriteStream('draw ');
  WriteStreamPoint(PP[0]);
  for I := 0 to PP.Count div 3 - 1 do
  begin
    WriteStream('..controls');
    WriteStreamPoint(PP[3 * I + 1]);
    WriteStream('and');
    WriteStreamPoint(PP[3 * I + 2]);
    WriteStream('..');
    WriteStreamPoint(PP[3 * I + 3]);
  end;
  if Closed then WriteStream('--cycle');
  WriteLineThickness(Kind);
  WriteLnStream(';');
end;

procedure T_MetaPost_Export.WriteHeader;
begin
  fFactorMM := 1 / fDrawing2D.PicUnitLength;
  MeasureDrawing;
  fHatchingStep := fDrawing2D.HatchingStep / fFactorAbs;
  WriteLnStream('%Exported from TpX drawing');
  WriteLnStream('%Caption: ' + fDrawing2D.Caption);
  WriteLnStream('%CreationDate: ' + DateTimeToStr(Now));
  WriteLnStream('beginfig(0);');
  WriteLnStream(Format('u=%.4g mm;',
    [fDrawing2D.PicUnitLength]));
  WriteLnStream('linecap:=butt;');
  WriteLnStream('linejoin:=mitered;');
end;

procedure T_MetaPost_Export.WriteFooter;
begin
  WriteLnStream('endfig;');
  WriteLnStream('end');
end;

procedure T_MetaPost_Export.WriteLine2D(Obj: TLine2D);
var
  PP: TPointsSet2D;
begin
  PP := TPointsSet2D.Create(10);
  try
    Obj.FillPoints(PP);
    WritePoly(PP, Obj.LineKind, False);
  finally
    PP.Free;
  end;
end;

procedure T_MetaPost_Export.WriteHatching(const P:
  TPointsSet2D;
  Hatching: THatching; Step: TRealType);
var
  Lines: TPointsSet2D;
  DX, DY: TRealType;
  procedure WriteHatching0;
  var
    I: Integer;
  begin
    if (DX = 0) and (DY = 0) then Exit;
    CalculateHatching(P, DX, DY, Step, Lines);
    for I := 0 to Lines.Count div 2 - 1 do
    begin
      WriteStream('draw ');
      WriteStreamPoint(Lines[I * 2]);
      WriteStream('--');
      WriteStreamPoint(Lines[I * 2 + 1]);
      WriteStream(Format(' withpen pencircle scaled %.2fmm',
        [fDrawing2D.LineWidth / 2]));
      WriteStream('; ');
    end;
    WriteLnStream('');
  end;
begin
  if Hatching = haNone then Exit;
  Lines := TPointsSet2D.Create(10);
  DX := HatchingDirections[Ord(Hatching)][1];
  DY := HatchingDirections[Ord(Hatching)][2];
  WriteHatching0;
  DX := HatchingDirections[Ord(Hatching)][3];
  DY := HatchingDirections[Ord(Hatching)][4];
  Lines.Clear;
  WriteHatching0;
  Lines.Free;
end;

procedure T_MetaPost_Export.WriteRectangle2D(Obj:
  TRectangle2D);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    try
      WriteHatching(ProfilePoints, Hatching,
        fHatchingStep);
      WritePoly(ProfilePoints, LineKind, True);
    finally
      EndUseProfilePoints;
    end;
  end;
end;

procedure T_MetaPost_Export.WriteText2D(Obj: TText2D);
var
  St: string;
begin
  with Obj do
  begin
    //WriteLnStream('defaultfont:="ftm";');
    //WriteLnStream('defaultfont:="Times-Roman";');
    WriteLnStream(Format('defaultscale:=%.1fu/fontsize defaultfont;',
      [Obj.Height * fFactorH]));
    //%.1fmm, Obj.Height * fFactorH
    case HJustification of
      jhLeft:
        case VJustification of
          jvBottom: WriteStream('label.urt');
          jvCenter: WriteStream('label.rt');
          jvTop: WriteStream('label.lrt');
        end;
      jhCenter:
        case VJustification of
          jvBottom: WriteStream('label.top');
          jvCenter: WriteStream('label');
          jvTop: WriteStream('label.bot');
        end;
      jhRight:
        case VJustification of
          jvBottom: WriteStream('label.ulft');
          jvCenter: WriteStream('label.lft');
          jvTop: WriteStream('label.llft');
        end;
    end;
    if TeXText <> '' then St := TeXText else St := Text;
    if fDrawing2D.MetaPostTeXText
      then WriteStream(Format('(btex %s etex,', [St]))
    else WriteStream(Format('("%s",', [Text]));
    WriteStreamPoint(Points[0]);
    WriteLnStream(');');
  end;
end;

procedure T_MetaPost_Export.WriteStar2D(Obj: TStar2D);
var
  StarsSize: TRealType;
begin
  with Obj do
  begin
    if Obj.OwnerCAD is TDrawing2D then
      StarsSize := (Obj.OwnerCAD as TDrawing2D).StarsSize
    else StarsSize := 1;
    WriteStream(Format('fill fullcircle scaled %.1fu shifted',
    //WriteStream(Format('draw fullcircle scaled %.1fu withpen pencircle scaled 0.05mm shifted',
      [StarsSize * fFactorH * 2]));
    WriteStreamPoint(Points[0]);
    WriteLnStream(';');
    {WriteLnStream('draw ');
    WriteStreamPoint(Points[0]);
    WriteLnStream(Format(' withpen pencircle scaled %.1fmm;',
      [StarsSize * 2]));}
  end;
end;

procedure T_MetaPost_Export.WriteEllipse2D(Obj: TEllipse2D);
var
  PP: TPointsSet2D;
begin
  if Obj.Points.Count < 2 then Exit;
  PP := nil;
  try
    Obj.BezierPoints(PP, IdentityTransf2D);
    WriteBezierPath(PP, Obj.LineKind, Obj.IsClosed);
  finally
    PP.Free;
  end;
  with Obj do
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints, Obj.Hatching, fHatchingStep);
      EndUseProfilePoints;
    end;
end;

procedure T_MetaPost_Export.WriteCircle2D(Obj: TCircle2D);
var
  CP: TPoint2D;
  R: TRealType;
begin
  with Obj do
  begin
    //if LineKind <> liNone then
    begin
      CP := Points[0];
      R := PointDistance2D(CP, Points[1]);
      WriteStream(Format('draw fullcircle scaled %.1fu shifted ',
        [R * fFactorH * 2]));
      WriteStreamPoint(Points[0]);
      WriteLineThickness(Obj.LineKind);
      WriteLnStream(';');
    end;
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints, Hatching, fHatchingStep);
      EndUseProfilePoints;
    end;
  end;
end;

procedure T_MetaPost_Export.WriteCircular2D(Obj: TCircular2D);
var
  PP: TPointsSet2D;
begin
  PP := nil;
  try
    Obj.BezierPoints(PP, IdentityTransf2D);
    WriteBezierPath(PP, Obj.LineKind, Obj.IsClosed);
  finally
    PP.Free;
  end;
  with Obj do
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints, Obj.Hatching, fHatchingStep);
      EndUseProfilePoints;
    end;
end;

procedure T_MetaPost_Export.WriteSpline2D(Obj: TSpline2D0);
var
  PP: TPointsSet2D;
begin
  if Obj.Points.Count < 2 then Exit;
  PP := nil;
  try
    Obj.BezierPoints(PP, IdentityTransf2D);
    WriteBezierPath(PP, Obj.LineKind, Obj.IsClosed);
  finally
    PP.Free;
  end;
  with Obj do
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints, Obj.Hatching, fHatchingStep);
      EndUseProfilePoints;
    end;
end;

procedure T_MetaPost_Export.WriteSmooth2D(Obj: TSmoothPath2D0);
var
  PP: TPointsSet2D;
begin
  if Obj.Points.Count < 2 then Exit;
  PP := nil;
  try
    Obj.BezierPoints(PP, IdentityTransf2D);
    WriteBezierPath(PP, Obj.LineKind, Obj.IsClosed);
  finally
    PP.Free;
  end;
  with Obj do
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints, Obj.Hatching, fHatchingStep);
      EndUseProfilePoints;
    end;
end;

procedure T_MetaPost_Export.WritePoly2D(Obj: TPolyline2D0);
var
  PP: TPointsSet2D;
begin
  if Obj.Points.Count = 0 then Exit;
  PP := Obj.Points;
  WritePoly(PP, Obj.LineKind, Obj.IsClosed);
  WriteHatching(PP, Obj.Hatching, fHatchingStep);
end;

procedure T_MetaPost_Export.WriteToTpX(Stream: TStream;
  const FileName: string);
begin
  fStream := nil;
  StoreToFile_MPS(fDrawing2D, ChangeFileExt(FileName, '.mps'));
  fStream := Stream;
  try
    WriteLnStream(Format('  \includegraphics{%s.mps}',
      [ChangeFileExt(ExtractFileName(FileName), '')]));
  finally
    fStream := nil;
  end;
end;

procedure T_MetaPost_Export.StoreToClipboard;
begin
  fStream.Free;
  fStream := TMemoryStream.Create;
  try
    WriteAll;
    fStream.Position := 0;
    PutStreamToClipboard0(CF_TEXT, fStream, fStream.Size);
  finally
    fStream.Free;
    fStream := nil;
  end;
end;

function FileExec(const aCmdLine, InFile, OutFile, Directory:
  string; aHide, aWait: Boolean): Boolean;
var
  StartupInfo: TSTARTUPINFO;
  ProcessInfo: TPROCESSINFORMATION;
  aInput, aOutput: Integer;
  PDirectory: PChar;
begin
  FillChar(StartupInfo, SizeOf(TSTARTUPINFO), 0);
  try
    if aWait then aHide := False;
    if OutFile <> '' then
      aOutput := FileCreate(OutFile);
    if InFile <> '' then
      aInput := FileOpen(InFile, fmOpenRead or fmShareDenyNone);
    with StartupInfo do
    begin
      cb := SizeOf(TSTARTUPINFO);
      if aHide then wShowWindow := SW_HIDE
      else wShowWindow := SW_SHOWNORMAL;
      if (OutFile <> '') or (InFile <> '') then
      begin
        dwFlags := STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK
          or STARTF_USESTDHANDLES;
        if InFile <> '' then hStdInput := aInput
        else hStdInput := INVALID_HANDLE_VALUE;
        if OutFile <> '' then hStdOutput := aOutput
        else hStdOutput := INVALID_HANDLE_VALUE;
        hStdError := INVALID_HANDLE_VALUE;
      end
      else dwFlags := STARTF_USESHOWWINDOW or
        STARTF_FORCEONFEEDBACK;
    end;
    if Directory = '' then PDirectory := nil
    else PDirectory := PChar(Directory);
    Result := CreateProcess({lpApplicationName: PChar} nil,
   {lpCommandLine: PChar} PChar(aCmdLine),
    {lpProcessAttributes, lpThreadAttributes:      PSecurityAttributes;}
      nil, nil, {bInheritHandles: BOOL} False,
    {dwCreationFlags: DWORD} NORMAL_PRIORITY_CLASS,
      {lpEnvironment: Pointer} nil,
    {lpCurrentDirectory: PChar} PDirectory,
    {const lpStartupInfo:      TStartupInfo} StartupInfo,
    {var lpProcessInformation: TProcessInformation} ProcessInfo
      ) {: BOOL;      stdcall};
    if aWait then
      //if Result then
    begin
      WaitForInputIdle(ProcessInfo.hProcess, INFINITE);
      WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    end;
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
  finally
    if OutFile <> '' then FileClose(aOutput);
    if InFile <> '' then FileClose(aInput);
  end;
end;

procedure StoreToFile_MPS(const CAD: TDrawing2D;
  const FileName: string);
var
  TempMP, TempMPS, TempMPLog: string;
  Res: Boolean;
  procedure TryDeleteFile(const FileName: string);
  begin
    if FileExists(FileName) then DeleteFile(PChar(FileName));
  end;
begin
  if not FileExists(MetaPostPath) then
  begin
    Application.MessageBox('MetaPost path not found',
      'Error', MB_OK);
    Exit;
  end;
  TempMP :=
    IncludeTrailingPathDelimiter(
    ExtractFilePath(FileName)) + '(tmp)TpX.mp';
  TempMPS := ChangeFileExt(TempMP, '.0');
  TempMPLog := ChangeFileExt(TempMP, '.log');
  StoreToFile_Saver(CAD, TempMP, T_MetaPost_Export);
  try
    //if FileExists(FileName) then DeleteFile(PChar(FileName));
    //WinExec(PChar(MetaPostPath + ' "' + TempMP + '"'), 0);
    //Res := FileExec(MetaPostPath + ' "' + TempMP + '"',      '', '', '', False, True);
    Res := FileExec(MetaPostPath + ' "(tmp)TpX.mp"', '', '',
      IncludeTrailingPathDelimiter(ExtractFilePath(FileName)),
      False, True);
    if FileExists(TempMPS) then
    begin
      TryDeleteFile(FileName);
      if not RenameFile(TempMPS, FileName) then
        Application.MessageBox('Can not rename. MPS file not created',
          'Error', MB_OK)
    end
    else
    begin
      if not FileExists(TempMPLog) then
        Application.MessageBox('MPS file not created',
          'Error', MB_OK)
      else if Application.MessageBox(
        'MPS file not created. Do you want to see log file?',
        'Error', MB_OKCANCEL) = idOK then
      begin
        FileExec('notepad ' + TempMPLog, '', '', '', True,
          False);
        Res := False;
      end;
    end;
  finally
    if Res then TryDeleteFile(TempMP);
    TryDeleteFile(TempMPS);
    if Res then TryDeleteFile(TempMPLog);
  end;
end;

{ --================ T_PDF_Export ==================-- }

constructor T_PDF_Export.Create(CAD: TDrawing2D);
begin
  inherited Create(CAD);
  fPDF := TPdfDoc.Create;
end;

destructor T_PDF_Export.Destroy;
begin
  fPDF.Free;
  inherited Destroy;
end;

procedure SetPDF_Dash(PDF: TPdfCanvas;
  aarray: array of Double; phase: Double);
var
  S: string;
  I: Integer;
begin
  S := '[';
  if (High(aarray) >= 0) and (aarray[0] <> 0) then
    for I := 0 to High(aarray) do
      S := S + _FloatToStrR(aarray[I]) + ' ';
  S := S + '] ' + _FloatToStrR(phase) + ' d'#10;
  _WriteString(S, PDF.Contents.Stream);
end;

procedure T_PDF_Export.WriteLineKind(Kind: TLineKind);
var
  A: TRealType;
begin
  A := fFactorMM;
  with fDrawing2D do
    with fPDF.Canvas do
    begin
      case Kind of
        liNone, liThin, liThick: SetDash([0], 0);
        liDashed: SetPDF_Dash(fPDF.Canvas,
            [DashSize * 2 * A, DashSize * A], 0);
        //SetDash([DashSize * 2 * A,            DashSize * A], 0);
        liDotted: SetPDF_Dash(fPDF.Canvas,
            [LineWidth * 2 * A, DottedSize * A], 0);
        //SetDash([LineWidth * 2 * A,            DottedSize * A], 0);
      end;
      case Kind of
        liNone: SetLineWidth(0);
        liThin, liDashed: SetLineWidth(LineWidth * A);
        liThick, liDotted: SetLineWidth(LineWidth * 2 * A);
      end;
    end;
end;

procedure T_PDF_Export.WritePoly(PP: TPointsSet2D;
  Kind: TLineKind; Closed: Boolean);
var
  I: Integer;
  P: TPoint2D;
begin
  if PP.Count < 1 then Exit;
  WriteLineKind(Kind);
  with fPDF.Canvas do
  begin
    P := ConvertPnt(PP[0]);
    MoveTo(P.X, P.Y);
    for I := 1 to PP.Count - 1 do
    begin
      P := ConvertPnt(PP[I]);
      LineTo(P.X, P.Y);
    end;
    if Closed then ClosePathStroke else Stroke;
  end;
end;

procedure T_PDF_Export.WriteBezierPath(const PP: TPointsSet2D;
  Kind: TLineKind; Closed: Boolean);
var
  I: Integer;
  P0, P1, P2, P3: TPoint2D;
begin
  if PP.Count < 1 then Exit;
  WriteLineKind(Kind);
  with fPDF.Canvas do
  begin
    P0 := ConvertPnt(PP[0]);
    MoveTo(P0.X, P0.Y);
    for I := 0 to PP.Count div 3 - 1 do
    begin
      P1 := ConvertPnt(PP[I * 3 + 1]);
      P2 := ConvertPnt(PP[I * 3 + 2]);
      P3 := ConvertPnt(PP[I * 3 + 3]);
      CurveToC(P1.X, P1.Y, P2.X, P2.Y, P3.X, P3.Y);
    end;
    if Closed then ClosePathStroke else Stroke;
  end;
end;

procedure T_PDF_Export.WriteHatching(const P: TPointsSet2D;
  Hatching: THatching; Step: TRealType);
var
  Lines: TPointsSet2D;
  DX, DY: TRealType;
  P0: TPoint2D;
  A: TRealType;
  procedure WriteHatching0;
  var
    I: Integer;
  begin
    if (DX = 0) and (DY = 0) then Exit;
    CalculateHatching(P, DX, DY, Step, Lines);
    with fPDF.Canvas do
    begin
      SetDash([0], 0);
      SetLineWidth(fDrawing2D.LineWidth / 2 * A);
      for I := 0 to Lines.Count div 2 - 1 do
      begin
        P0 := ConvertPnt(Lines[I * 2]);
        MoveTo(P0.X, P0.Y);
        P0 := ConvertPnt(Lines[I * 2 + 1]);
        LineTo(P0.X, P0.Y);
      end;
      Stroke;
    end;
  end;
begin
  if Hatching = haNone then Exit;
  A := fFactorMM;
  Lines := TPointsSet2D.Create(10);
  DX := HatchingDirections[Ord(Hatching)][1];
  DY := HatchingDirections[Ord(Hatching)][2];
  WriteHatching0;
  DX := HatchingDirections[Ord(Hatching)][3];
  DY := HatchingDirections[Ord(Hatching)][4];
  Lines.Clear;
  WriteHatching0;
  Lines.Free;
end;

procedure T_PDF_Export.WriteHeader;
var
  Dest: TPdfDestination;
begin
    //72 pixel per inch, (25.4/72 = 0.352778) mm in pixel
    // 2.8346 pixel per mm
  fFactorMM := 2.8346;
  MeasureDrawing;
  fHatchingStep := fDrawing2D.HatchingStep / fFactorAbs;
  with fPDF do
  begin
    NewDoc;
    DefaultPageWidth := Round(fW_MM * fFactorMM);
    DefaultPageHeight := Round(fH_MM * fFactorMM);
    AddPage;
    Dest := CreateDestination;
    with Dest do
    begin
      DestinationType := dtXYZ;
      Left := -10;
      Top := -10;
      Zoom := 1;
    end;
    Root.OpenAction := Dest;
  end;
end;

procedure T_PDF_Export.WriteLine2D(Obj: TLine2D);
var
  PP: TPointsSet2D;
begin
  PP := TPointsSet2D.Create(10);
  Obj.FillPoints(PP);
  WritePoly(PP, Obj.LineKind, False);
  //try  except    ShowMessage(IntToStr(PP.Count));  end;
  PP.Free;
  //WriteLine(Obj.Points[0], Obj.Points[1], Obj.LineKind);
end;

procedure T_PDF_Export.WriteRectangle2D(Obj: TRectangle2D);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    try
      WriteHatching(ProfilePoints, Hatching,
        fHatchingStep);
      WritePoly(ProfilePoints, LineKind, True);
    finally
      EndUseProfilePoints;
    end;
  end;
end;

procedure T_PDF_Export.WriteEllipse2D(Obj: TEllipse2D);
var
  PP: TPointsSet2D;
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    WriteHatching(ProfilePoints, Hatching, fHatchingStep);
    EndUseProfilePoints;
    PP := nil;
    try
      BezierPoints(PP, IdentityTransf2D);
      WriteBezierPath(PP, LineKind, IsClosed);
    finally
      PP.Free;
    end;
  end;
end;

procedure T_PDF_Export.WriteCircle2D(Obj: TCircle2D);
var
  PP: TPointsSet2D;
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    WriteHatching(ProfilePoints, Hatching, fHatchingStep);
    EndUseProfilePoints;
    PP := nil;
    try
      Obj.BezierPoints(PP, IdentityTransf2D);
      WriteBezierPath(PP, LineKind, IsClosed);
    finally
      PP.Free;
    end;
  end;
end;

procedure T_PDF_Export.WriteCircular2D(Obj: TCircular2D);
var
  PP: TPointsSet2D;
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    WriteHatching(ProfilePoints, Hatching, fHatchingStep);
    EndUseProfilePoints;
    PP := nil;
    try
      Obj.BezierPoints(PP, IdentityTransf2D);
      WriteBezierPath(PP, LineKind, IsClosed);
    finally
      PP.Free;
    end;
  end;
end;

procedure T_PDF_Export.WriteSpline2D(Obj: TSpline2D0);
var
  PP: TPointsSet2D;
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    WriteHatching(ProfilePoints, Hatching, fHatchingStep);
    EndUseProfilePoints;
    PP := nil;
    try
      Obj.BezierPoints(PP, IdentityTransf2D);
      WriteBezierPath(PP, LineKind, IsClosed);
    finally
      PP.Free;
    end;
  end;
end;

procedure T_PDF_Export.WriteSmooth2D(Obj: TSmoothPath2D0);
var
  PP: TPointsSet2D;
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    WriteHatching(ProfilePoints, Hatching, fHatchingStep);
    EndUseProfilePoints;
    PP := nil;
    try
      Obj.BezierPoints(PP, IdentityTransf2D);
      WriteBezierPath(PP, LineKind, IsClosed);
    finally
      PP.Free;
    end;
  end;
end;

procedure T_PDF_Export.WritePoly2D(Obj: TPolyline2D0);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    WriteHatching(ProfilePoints, Obj.Hatching, fHatchingStep);
    EndUseProfilePoints;
  end;
  WritePoly(Obj.Points, Obj.LineKind, Obj.IsClosed);
end;

procedure T_PDF_Export.WriteText2D(Obj: TText2D);
var
  FontH, HShift, VShift: TRealType;
  P: TPoint2D;
begin
  with Obj, fPDF.Canvas do
  begin
    P := Points[0];
    FontH := Height * fFactorH / 1.2;
    SetFont('Times-Roman', FontH);
    case HJustification of
      jhLeft: HShift := 0;
      jhCenter: HShift := TextWidth(Text) / 2;
      jhRight: HShift := TextWidth(Text);
    end;
    case VJustification of
      jvBottom: VShift := -0.2 * FontH;
      jvCenter: VShift := 0.3 * FontH;
      jvTop: VShift := 0.8 * FontH;
    end;
    TextOut(ConvertX(P.X) - HShift,
      ConvertY(P.Y) - VShift, Text);
      //TextRect(ARect: TPdfRect; Text: string;                            Alignment: TPdfAlignment; Clipping: boolean);
  end;
end;

procedure T_PDF_Export.WriteStar2D(Obj: TStar2D);
var
  CP: TPoint2D;
  R: TRealType;
begin
  CP := ConvertPnt(Obj.Points[0]);
  R := fDrawing2D.StarsSize * fFactorW;
  with fPDF.Canvas do
  begin
    Ellipse(CP.X - R, CP.Y - R, 2 * R, 2 * R);
    Eofill;
  end;
end;

procedure T_PDF_Export.WriteAllToStream;
begin
  if fStream = nil then Exit;
  WriteAll;
  fPDF.SaveToStream(fStream);
end;

procedure T_PDF_Export.WriteToTpX(Stream: TStream;
  const FileName: string);
begin
  fStream := nil;
  StoreToFile(ChangeFileExt(FileName, '.pdf'));
  fStream := Stream;
  try
    WriteLnStream(Format(
      '  \includegraphics{%s.pdf}', //[width=%.1fcm,height=%.1fcm]
      [//fDrawing2D.PicWidth / 10, fDrawing2D.PicHeight / 10,
      ChangeFileExt(ExtractFileName(FileName), '')]));
  finally
    fStream := nil;
  end;
end;

{ --================ T_Bitmap0_Export ==================-- }

constructor T_Bitmap0_Export.Create(CAD: TDrawing2D);
begin
  inherited Create(CAD);
  fBitmap := TBitmap32.Create;
  fPolygon := TPolygon32.Create;
  fOutline := TPolygon32.Create;
end;

destructor T_Bitmap0_Export.Destroy;
begin
  fBitmap.Free;
  fPolygon.Free;
  fOutline.Free;
  inherited Destroy;
end;

function T_Bitmap0_Export.ConvertY(Y: TRealType): TRealType;
begin
  Result := (fExtTop - Y) * fFactorH;
end;

function DashedOutline(const SD1, SD2: Single;
  const Polygon: TPolygon32): TPolygon32;
var
  J: Integer;
  DD1, DD2: TFixed;
  TmpPoly: TPolygon32;
  procedure FillSinglePoly(const PP: TArrayOfFixedPoint);
  var
    I, N: Integer;
    D, D0, D1, A0, A1: Single;
    P00, P11, P0, P1: TFixedPoint;
    function Get(const I: Integer): TFixedPoint;
    begin
      if I < Length(PP) then Result := PP[I]
      else Result := PP[0];
    end;
  begin
    if Length(PP) < 2 then Exit;

    if Polygon.Closed then N := Length(PP) + 1
    else N := Length(PP);
    D0 := 0;
    for I := 0 to N - 2 do
    begin
      P00 := Get(I);
      P11 := Get(I + 1);
      D := Hypot(P11.X - P00.X, P11.Y - P00.Y);
      while (D0 < D) and (D1 < D) do
      begin
        D1 := D0 + DD1;
        A0 := Max(D0 / D, 0);
        A1 := Min(D1 / D, 1);
        P0.X := P00.X + Round((P11.X - P00.X) * A0);
        P0.Y := P00.Y + Round((P11.Y - P00.Y) * A0);
        P1.X := P00.X + Round((P11.X - P00.X) * A1);
        P1.Y := P00.Y + Round((P11.Y - P00.Y) * A1);
        TmpPoly.NewLine;
        TmpPoly.Add(P0);
        TmpPoly.Add(P1);
        if D1 < D then D0 := D1 + DD2;
      end;
      D0 := D0 - D;
      D1 := 0;
    end;
  end;
begin
  //Polygon.BuildNormals;
  DD1 := Fixed(SD1);
  DD2 := Fixed(SD2);
  Result := TPolygon32.Create;
  TmpPoly := TPolygon32.Create;
  try
    with Polygon do
    begin
      TmpPoly.Closed := False;
      TmpPoly.Points := nil;
      for J := 0 to High(Points) do
        FillSinglePoly(Points[J]);
      Result.Free;
      Result := TmpPoly.Outline;
    end;
  finally
    TmpPoly.Free;
  end;
end;

procedure T_Bitmap0_Export.WriteLine(P0, P1: TPoint2D; W:
  TRealType);
var
  TmpPoly: TPolygon32;
begin
  if IsSamePoint2D(P0, P1) then Exit;
  fPolygon.Clear;
  fPolygon.Closed := False;
  P0 := ConvertPnt(P0);
  fPolygon.Add(FixedPoint(P0.X, P0.Y));
  P1 := ConvertPnt(P1);
  fPolygon.Add(FixedPoint(P1.X, P1.Y));
  TmpPoly := fPolygon.Outline;
  try
    fOutline.Free;
    fOutline := TmpPoly.Grow(Fixed(W / 2), 0);
  finally
    TmpPoly.Free;
  end;
  WriteBitmapPolygon(fOutline);
end;

procedure T_Bitmap0_Export.WriteBitmapPolygon(Polygon:
  TPolygon32);
begin
  Polygon.Antialiased := True; //False;
  Polygon.DrawFill(fBitmap, clBlack32)
end;

procedure T_Bitmap0_Export.FillPolygon(PP: TPointsSet2D;
  Closed: Boolean);
var
  I: Integer;
  P: TPoint2D;
begin
  with fDrawing2D, fBitmap do
  begin
    fPolygon.Clear;
    fPolygon.Closed := Closed;
    for I := 0 to PP.Count - 1 do
    begin
      P := ConvertPnt(PP[I]);
      fPolygon.Add(FixedPoint(P.X, P.Y));
    end;
  end;
end;

procedure T_Bitmap0_Export.WritePoly(PP: TPointsSet2D;
  Kind: TLineKind; Closed: Boolean);
var
  W: TRealType;
  TmpPoly: TPolygon32;
begin
  if PP.Count < 1 then Exit;
  if Kind = liNone then Exit;
  FillPolygon(PP, Closed);
  with fDrawing2D, fBitmap do
  begin
    case Kind of
      liThin, liThick: TmpPoly := fPolygon.Outline;
      liDashed:
        TmpPoly := DashedOutline(DashSize * 2 * fFactorMM,
          DashSize * fFactorMM, fPolygon);
      liDotted:
        TmpPoly := DashedOutline(LineWidth * 2 * fFactorMM,
          DottedSize * fFactorMM, fPolygon)
    end;
    case Kind of
      liThin, liDashed:
        W := LineWidth * fFactorMM;
      liThick, liDotted:
        W := LineWidth * 2 * fFactorMM;
    end;
  end;
  try
    fOutline.Free;
    fOutline := TmpPoly.Grow(Fixed(W / 2), 0.99);
    fOutline.FillMode := pfWinding;
  finally
    TmpPoly.Free;
  end;
  WriteBitmapPolygon(fOutline);
end;

procedure T_Bitmap0_Export.WriteHatching(const P: TPointsSet2D;
  Hatching: THatching; Step: TRealType);
var
  Lines: TPointsSet2D;
  DX, DY: TRealType;
  procedure WriteHatching0;
  var
    I: Integer;
  begin
    if (DX = 0) and (DY = 0) then Exit;
    CalculateHatching(P, DX, DY, Step, Lines);
    with fDrawing2D do
    begin
      for I := 0 to Lines.Count div 2 - 1 do
      begin
        WriteLine(Lines[I * 2], Lines[I * 2 + 1],
          LineWidth / 2 * fFactorMM);
        //P0 := ConvertPnt(Lines[I * 2]);
        //P1 := ConvertPnt(Lines[I * 2 + 1]);
        //fBitmap.LineFS(P0.X, P0.Y, P1.X, P1.Y, clBlack32);
      end;
    end;
  end;
begin
  if Hatching = haNone then Exit;
  Lines := TPointsSet2D.Create(10);
  DX := HatchingDirections[Ord(Hatching)][1];
  DY := HatchingDirections[Ord(Hatching)][2];
  WriteHatching0;
  DX := HatchingDirections[Ord(Hatching)][3];
  DY := HatchingDirections[Ord(Hatching)][4];
  Lines.Clear;
  WriteHatching0;
  Lines.Free;
end;

procedure T_Bitmap0_Export.WriteHeader;
begin
  fFactorMM := 0.5 / fDrawing2D.PicUnitLength;
  MeasureDrawing;
  fHatchingStep := fDrawing2D.HatchingStep / fFactorAbs;
  fBitmap.Width := Round(fW_MM * fFactorMM);
  fBitmap.Height := Round(fH_MM * fFactorMM);
  fBitmap.Clear(clWhite32);
  fBitmap.PenColor := clBlack32;
end;

procedure T_Bitmap0_Export.WriteAll;
begin
  if fDrawing2D = nil then Exit;
  fBitmap.BeginUpdate;
  WriteHeader;
  WriteEntities;
  fBitmap.EndUpdate;
end;

procedure T_Bitmap0_Export.WriteLine2D(Obj: TLine2D);
var
  PP: TPointsSet2D;
begin
  PP := TPointsSet2D.Create(10);
  Obj.FillPoints(PP);
  WritePoly(PP, Obj.LineKind, False);
  PP.Free;
end;

procedure T_Bitmap0_Export.WriteRectangle2D(Obj: TRectangle2D);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    try
      WriteHatching(ProfilePoints, Hatching,
        fHatchingStep);
      WritePoly(ProfilePoints, LineKind, True);
    finally
      EndUseProfilePoints;
    end;
  end;
end;

procedure T_Bitmap0_Export.WriteEllipse2D(Obj: TEllipse2D);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    WriteHatching(ProfilePoints, Hatching, fHatchingStep);
    WritePoly(ProfilePoints, LineKind, IsClosed);
    EndUseProfilePoints;
  end;
end;

procedure T_Bitmap0_Export.WriteCircle2D(Obj: TCircle2D);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    WriteHatching(ProfilePoints, Hatching, fHatchingStep);
    WritePoly(ProfilePoints, LineKind, IsClosed);
    EndUseProfilePoints;
  end;
end;

procedure T_Bitmap0_Export.WriteCircular2D(Obj: TCircular2D);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    WriteHatching(ProfilePoints, Obj.Hatching, fHatchingStep);
    WritePoly(ProfilePoints, LineKind, IsClosed);
    EndUseProfilePoints;
  end;
end;

procedure T_Bitmap0_Export.WriteSpline2D(Obj: TSpline2D0);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    WriteHatching(ProfilePoints, Obj.Hatching, fHatchingStep);
    WritePoly(ProfilePoints, LineKind, IsClosed);
    EndUseProfilePoints;
  end;
end;

procedure T_Bitmap0_Export.WriteSmooth2D(Obj: TSmoothPath2D0);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    WriteHatching(ProfilePoints, Obj.Hatching, fHatchingStep);
    WritePoly(ProfilePoints, LineKind, IsClosed);
    EndUseProfilePoints;
  end;
end;

procedure T_Bitmap0_Export.WritePoly2D(Obj: TPolyline2D0);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    WriteHatching(ProfilePoints, Obj.Hatching, fHatchingStep);
    EndUseProfilePoints;
  end;
  WritePoly(Obj.Points, Obj.LineKind, Obj.IsClosed);
end;

procedure T_Bitmap0_Export.WriteText2D(Obj: TText2D);
var
  FontH, HShift, VShift: TRealType;
  P: TPoint2D;
begin
  with Obj do
  begin
    fBitmap.PenColor := clBlack32;
    fBitmap.Font.Color := clBlack;
    fBitmap.Font.Name := LogFont.FaceName;
    FontH := Height * fFactorH;
    fBitmap.Font.Height := Round(FontH);
    P := Points[0];
    case HJustification of
      jhLeft: HShift := 0;
      jhCenter: HShift := fBitmap.TextWidth(Text) / 2;
      jhRight: HShift := fBitmap.TextWidth(Text);
    end;
    case VJustification of
      jvBottom: VShift := FontH;
      jvCenter: VShift := FontH / 2;
      jvTop: VShift := 0;
    end;
    fBitmap.TextoutW(Round(ConvertX(P.X) - HShift),
      Round(ConvertY(P.Y) - VShift), Text);
  end;
end;

procedure T_Bitmap0_Export.WriteStar2D(Obj: TStar2D);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    FillPolygon(ProfilePoints, True);
    WriteBitmapPolygon(fPolygon);
    EndUseProfilePoints;
  end;
end;

procedure T_Bitmap0_Export.WriteAllToStream;
begin
  if fStream = nil then Exit;
  WriteAll;
  fBitmap.SaveToStream(fStream);
end;

procedure T_Bitmap0_Export.StoreToFile(const FileName: string);
var
  aBitmap: TBitmap;
begin
  WriteAll;
  aBitmap := TBitmap.Create;
  try
    aBitmap.Assign(fBitmap);
    aBitmap.PixelFormat := pf24bit;
    aBitmap.SaveToFile(FileName);
  finally
    aBitmap.Free;
  end;
  //fBitmap.SaveToFile(FileName);
end;

{ --================ T_Bitmap_Export ==================-- }

procedure T_Bitmap_Export.WriteToTpX(Stream: TStream;
  const FileName: string);
var
  BBList: TStringList;
begin
  fStream := nil;
  StoreToFile(ChangeFileExt(FileName, '.bmp'));
  fStream := Stream;
  try
    WriteLnStream(Format(
      '  \includegraphics[width=%.3fcm,height=%.3fcm]{%s.bmp}',
      [fW_MM / 10, fH_MM / 10,
      ChangeFileExt(ExtractFileName(FileName), '')]));
  finally
    fStream := nil;
  end;
  BBList := TStringList.Create;
  try
    BBList.Add('%%' + Format('BoundingBox: %d %d %d %d',
      [0, 0, Ceil(fW_MM * 2.8346), Ceil(fH_MM * 2.8346)]));
    BBList.Add('%%Title: ' + fDrawing2D.Caption);
    BBList.Add('%%Creator: Exported from TpX drawing');
    BBList.Add('%%CreationDate: ' + DateTimeToStr(Now));
    BBList.SaveToFile(ChangeFileExt(FileName, '.bb'));
  finally
    BBList.Free;
  end;
end;

{ --================ T_PNG_Export ==================-- }

procedure T_PNG_Export.WriteToTpX(Stream: TStream;
  const FileName: string);
var
  BBList: TStringList;
begin
  fStream := nil;
  StoreToFile(ChangeFileExt(FileName, '.png'));
  fStream := Stream;
  try
    WriteLnStream(Format(
      '  \includegraphics[width=%.3fcm,height=%.3fcm]{%s.png}',
      [fW_MM / 10, fH_MM / 10,
      ChangeFileExt(ExtractFileName(FileName), '')]));
  finally
    fStream := nil;
  end;
  BBList := TStringList.Create;
  try
    BBList.Add('%%' + Format('BoundingBox: %d %d %d %d',
      [0, 0, Ceil(fW_MM * 2.8346), Ceil(fH_MM * 2.8346)]));
    BBList.Add('%%Title: ' + fDrawing2D.Caption);
    BBList.Add('%%Creator: Exported from TpX drawing');
    BBList.Add('%%CreationDate: ' + DateTimeToStr(Now));
    BBList.SaveToFile(ChangeFileExt(FileName, '.bb'));
  finally
    BBList.Free;
  end;
end;

procedure T_PNG_Export.StoreToFile(const FileName: string);
var
  aPNG: TPNGObject;
  aBitmap: TBitmap;
begin
  WriteAll;
  aPNG := TPNGObject.Create;
  aBitmap := TBitmap.Create;
  try
    //aBitmap.PixelFormat := pf32bit;
    aBitmap.Assign(fBitmap);
    aPNG.Assign(aBitmap);
    aPNG.SaveToFile(FileName);
  finally
    aPNG.Free;
    aBitmap.Free;
  end;
end;

{ --================ T_EMF_Export ==================-- }

procedure T_EMF_Export.WriteToTpX(Stream: TStream;
  const FileName: string);
var
  BBList: TStringList;
begin
  fStream := nil;
  fDrawing2D.SaveToFile_EMF(ChangeFileExt(FileName, '.emf'));
  fStream := Stream;
  try
  //fFactorMM := 0.5 / fDrawing2D.PicUnitLength;
    fFactorMM := 1;
    MeasureDrawing;
    WriteLnStream(Format(
      '  \includegraphics[width=%.3fcm,height=%.3fcm]{%s}',
      [fW_MM / 10, fH_MM / 10,
      ChangeFileExt(ExtractFileName(FileName), '.emf')]));
  finally
    fStream := nil;
  end;
  BBList := TStringList.Create;
  try
    BBList.Add('%%' + Format('BoundingBox: %d %d %d %d',
      [0, 0, Ceil(fW_MM * 2.8346), Ceil(fH_MM * 2.8346)]));
    BBList.Add('%%Title: ' + fDrawing2D.Caption);
    BBList.Add('%%Creator: Exported from TpX drawing');
    BBList.Add('%%CreationDate: ' + DateTimeToStr(Now));
    BBList.SaveToFile(ChangeFileExt(FileName, '.bb'));
  finally
    BBList.Free;
  end;
end;

procedure T_EMF_Export.StoreToFile(const FileName: string);
begin
  fDrawing2D.SaveToFile_EMF(FileName);
end;


{ --================ T_EpsToPdf_Export ==================-- }

procedure StoreToFile_EpsToPdf(const CAD: TDrawing2D;
  const FileName: string);
var
  TempEPS, TempPDF: string;
  Res: Boolean;
  function TryDeleteFile(const FileName: string): Boolean;
  begin
    if FileExists(FileName)
      then Result := SysUtils.DeleteFile(PChar(FileName))
    else Result := True;
  end;
begin
  if not FileExists(EpsToPdfPath) then
  begin
    Application.MessageBox('EpsToPdf path not found',
      'Error', MB_OK);
    Exit;
  end;
  TempEPS :=
    IncludeTrailingPathDelimiter(
    ExtractFilePath(FileName)) + '(tmp)TpX.eps';
  TempPDF := ChangeFileExt(TempEPS, '.pdf');
  StoreToFile_Saver(CAD, TempEPS, T_PostScript_Export);
  try
  //EpsToPdfPath: string = 'epstopdf.exe';
    Res := FileExec(Format('"%s" "%s"',
      [EpsToPdfPath, TempEPS]), '', '',
      IncludeTrailingPathDelimiter(ExtractFilePath(FileName)),
      False, True);
    if FileExists(TempPDF) then
    begin
      TryDeleteFile(FileName);
      if not RenameFile(TempPDF, FileName) then
        Application.MessageBox('Can not rename. PDF file not created',
          'Error', MB_OK);
    end
    else
      Application.MessageBox('PDF file not created',
        'Error', MB_OK);
  finally
    if Res then
      if not TryDeleteFile(TempEPS) then ;
      //Application.MessageBox('qqq',        'Error', MB_OK);
    TryDeleteFile(TempPDF);
  end;
end;

procedure T_EpsToPdf_Export.WriteToTpX(Stream: TStream;
  const FileName: string);
begin
  fStream := nil;
  StoreToFile_EpsToPdf(fDrawing2D,
    ChangeFileExt(FileName, '.pdf'));
  fStream := Stream;
  try
    WriteLnStream(Format('  \includegraphics{%s}',
      [ChangeFileExt(ExtractFileName(FileName), '.pdf')]));
  finally
    fStream := nil;
  end;
end;

procedure T_EpsToPdf_Export.StoreToFile(const FileName: string);
begin
  StoreToFile_EpsToPdf(fDrawing2D,
    ChangeFileExt(FileName, '.pdf'));
end;

{ --================ T_TpX_Loader ==================-- }

procedure T_TpX_Loader.ReadPrimitiveAttr(Obj: TPrimitive2D;
  XMLNode: TXMLDElement);
begin
  if XMLNode.AttributeNode['li'] <> nil then
    Obj.LineKind := XMLNode.AttributeValue['li']
  else Obj.LineKind := liThick;
  if XMLNode.AttributeNode['ha'] <> nil then
    Obj.Hatching := XMLNode.AttributeValue['ha']
  else Obj.Hatching := haNone;
end;

function T_TpX_Loader.ReadLine(XMLNode: TXMLDElement):
  TPrimitive2D;
begin
  Result := TLine2D.Create(0,
    Point2D(XMLNode.AttributeValue['x1'],
    XMLNode.AttributeValue['y1']),
    Point2D(XMLNode.AttributeValue['x2'],
    XMLNode.AttributeValue['y2']));
  if XMLNode.AttributeNode['arr1'] <> nil then
    (Result as TLine2D).BeginArrowKind :=
      TArrowKind(XMLNode.AttributeValue['arr1'] + 1);
  if XMLNode.AttributeNode['arr2'] <> nil then
    (Result as TLine2D).EndArrowKind :=
      TArrowKind(XMLNode.AttributeValue['arr2'] + 1);
  ReadPrimitiveAttr(Result, XMLNode);
end;

function T_TpX_Loader.ReadRect(XMLNode: TXMLDElement):
  TPrimitive2D;
var
  P0, P1, P2: TPoint2D;
  A: TRealType;
begin
  P0 := Point2D(XMLNode.AttributeValue['x1'],
    XMLNode.AttributeValue['y1']);
  P1 := Point2D(XMLNode.AttributeValue['x2'],
    XMLNode.AttributeValue['y2']);
  if XMLNode.AttributeNode['rot'] <> nil then
  begin
    A := XMLNode.AttributeValue['rot'];
    P2 := Point2D(P0.X + Sin(A), P0.Y + Cos(A));
  end
  else
  begin
    P2 := Point2D(P0.X, P0.Y + 1);
  end;
  Result := TRectangle2D.Create(0);
  (Result as TRectangle2D).Points[0] := P0;
  (Result as TRectangle2D).Points[1] := P1;
  (Result as TRectangle2D).Points[2] := P2;
  ReadPrimitiveAttr(Result, XMLNode);
end;

function T_TpX_Loader.ReadText(XMLNode: TXMLDElement):
  TPrimitive2D;
var
  X, Y, H: Single;
  Ch: Char;
begin
  //fViewport2D. fDrawing2D
  H := XMLNode.AttributeValue['h'];
  X := XMLNode.AttributeValue['x'];
  Y := XMLNode.AttributeValue['y'];
  Result := TText2D.Create(0,
    Point2D(X, Y), H, XMLNode.AttributeValue['t']);
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
    end;
    if XMLNode.AttributeNode['tex'] <> nil then
      TeXText := XMLNode.AttributeValue['tex']
    else TeXText := '';
  end;
  ReadPrimitiveAttr(Result, XMLNode);
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
    if XMLNode.AttributeNode['rot'] <> nil then
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
  Result := TArc2D.Create(0, Point2D(X, Y), D / 2, SA, EA);
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
    TArc2D.Create(0, Point2D(X, Y), D / 2, SA, EA)
  else if TheClass = TSector2D then Result :=
    TSector2D.Create(0, Point2D(X, Y), D / 2, SA, EA)
  else if TheClass = TSegment2D then Result :=
    TSegment2D.Create(0, Point2D(X, Y), D / 2, SA, EA);
  ReadPrimitiveAttr(Result, XMLNode);
end;

procedure AddPoints(Text: string; Pnts: TPointsSet2D);
var
  Lines: TStringList;
  I, Len: Integer;
  Pnts0: array of TPoint2D;
begin
  Pnts.Delete(0);
  Lines := TStringList.Create;
  Lines.Delimiter := ' ';
  Text := AnsiReplaceStr(Trim(Text), ',', ' ');
  Text := AnsiReplaceStr(Text, '  ', ' ');
  Text := AnsiReplaceStr(Text, '  ', ' ');
  Lines.DelimitedText := Text;
  Len := Lines.Count div 2;
  SetLength(Pnts0, Len);
  for I := 0 to Len - 1 do
    Pnts0[I] :=
      Point2D(StrToRealType(Trim(Lines[I * 2])),
      StrToRealType(Trim(Lines[I * 2 + 1])));
  Pnts.AddPoints(Pnts0);
end;

function T_TpX_Loader.ReadSpline(XMLNode: TXMLDElement):
  TPrimitive2D;
var
  IsClosed: Boolean;
  Order: Byte;
begin
  if XMLNode.NodeName = 'contour' then IsClosed := True
  else if XMLNode.AttributeNode['closed'] <> nil
    then IsClosed := XMLNode.AttributeValue['closed'] <> 0
  else IsClosed := False;
  if XMLNode.NodeName = 'cspline' then Order := 4
  else Order := 3;
  if Order = 3 then
    if IsClosed then
      Result := TClosedBSpline2D.Create(0, [Point2D(0, 0)])
    else Result := TBSpline2D.Create(0, [Point2D(0, 0)])
  else
    if IsClosed then
      Result := TClosedCubicBSpline2D.Create(0, [Point2D(0, 0)])
    else Result := TCubicBSpline2D.Create(0, [Point2D(0, 0)]);
  AddPoints(XMLNode.Text, (Result as TSpline2D0).Points);
  ReadPrimitiveAttr(Result, XMLNode);
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
    Result := TClosedSmoothPath2D.Create(0, [Point2D(0, 0)])
  else Result := TSmoothPath2D.Create(0, [Point2D(0, 0)]);
  AddPoints(XMLNode.Text, (Result as TSmoothPath2D0).Points);
  ReadPrimitiveAttr(Result, XMLNode);
end;

function T_TpX_Loader.ReadPolygon(XMLNode: TXMLDElement):
  TPrimitive2D;
begin
  Result := TPolygon2D.Create(0, [Point2D(0, 0)]);
  AddPoints(XMLNode.Text, (Result as TPolygon2D).Points);
  ReadPrimitiveAttr(Result, XMLNode);
end;

function T_TpX_Loader.ReadPolyline(XMLNode: TXMLDElement):
  TPrimitive2D;
begin
  Result := TPolyline2D.Create(0, [Point2D(0, 0)]);
  AddPoints(XMLNode.Text, (Result as TPolyline2D).Points);
  ReadPrimitiveAttr(Result, XMLNode);
end;

function T_TpX_Loader.ReadStar(XMLNode: TXMLDElement):
  TPrimitive2D;
var
  CP: TPoint2D;
begin
  CP := Point2D(XMLNode.AttributeValue['x'],
    XMLNode.AttributeValue['y']);
  Result := TStar2D.Create(0, CP);
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
  else if (ID = 'spline') or (ID = 'cspline')
    or (ID = 'contour') then Result := ReadSpline(XMLNode)
  else if (ID = 'smooth') or (ID = 'curve')
    then Result := ReadSmooth(XMLNode)
  else if ID = 'polygon' then Result := ReadPolygon(XMLNode)
  else if (ID = 'path') or (ID = 'polyline')
    then Result := ReadPolyline(XMLNode)
  else if ID = 'star' then Result := ReadStar(XMLNode)
  else Result := nil;
end;

constructor T_TpX_Loader.Create(CAD: TDrawing2D;
  Viewport2D: TCADViewport2D);
begin
  inherited Create;
  fDrawing2D := CAD;
  fViewport2D := Viewport2D;
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
        Result := AttributeValue[ID] else Result := Default;
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
      else fDrawing2D.TeXFormat := tex_tex;
      if AttributeNode['PdfTeXFormat'] <> nil then
        fDrawing2D.PdfTeXFormat :=
          PdfTeXFormatKind(StringToChoice(PdfTeXFormat_Choice,
          AttributeValue['PdfTeXFormat']))
      else fDrawing2D.PdfTeXFormat := pdftex_tex;
      if AttributeNode['ArrowsSize'] <> nil then
        fDrawing2D.ArrowsSize := AttributeValue['ArrowsSize'];
      if AttributeNode['StarsSize'] <> nil then
        fDrawing2D.StarsSize := AttributeValue['StarsSize'];
      if AttributeNode['DefaultFontHeight'] <> nil then
        fDrawing2D.DefaultFontHeight :=
          AttributeValue['DefaultFontHeight'];
      if AttributeNode['PicWidth'] <> nil then
        fDrawing2D.PicWidth := AttributeValue['PicWidth'];
      if AttributeNode['PicHeight'] <> nil then
        fDrawing2D.PicHeight := AttributeValue['PicHeight'];
      if AttributeNode['Border'] <> nil then
        fDrawing2D.Border := AttributeValue['Border']
      else fDrawing2D.Border := Border_Default;
      if AttributeNode['PicUnitLength'] <> nil then
        fDrawing2D.PicUnitLength :=
          AttributeValue['PicUnitLength'];
      if AttributeNode['HatchingStep'] <> nil then
        fDrawing2D.HatchingStep :=
          AttributeValue['HatchingStep'];
      if AttributeNode['DottedSize'] <> nil then
        fDrawing2D.DottedSize := AttributeValue['DottedSize'];
      if AttributeNode['DashSize'] <> nil then
        fDrawing2D.DashSize := AttributeValue['DashSize'];
      if AttributeNode['TeXMinLine'] <> nil then
        fDrawing2D.TeXMinLine := AttributeValue['TeXMinLine'];
      if AttributeNode['LineWidth'] <> nil then
        fDrawing2D.LineWidth := AttributeValue['LineWidth'];
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
      fDrawing2D.SVGMagnif :=
        GetRealType('SVGMagnif', 1.5);
      fDrawing2D.MetaPostTeXText :=
        GetBoolean('MetaPostTeXText', True);
    end;
  end;
  fViewport2D.VisualRect := Rect;
  Child := fXML.DocumentElement.SelectSingleNode('caption');
  if Child <> nil then
  begin
    fDrawing2D.Caption := Child.Text;
    fDrawing2D.FigLabel
      := (Child as TXMLDElement).AttributeValue['label'];
  end
  else
  begin
    fDrawing2D.Caption := '';
    fDrawing2D.FigLabel := '';
  end;
  Child := fXML.DocumentElement.SelectSingleNode('comment');
  if Child <> nil then fDrawing2D.Comment := Child.Text
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
    for I := 0 to fXML.DocumentElement.ChildNodes.Count - 1
      do
    begin
      Child := fXML.DocumentElement.ChildNodes[I];
      if Child is TXMLDElement then
        Tmp := ReadEntity(Child as TXMLDElement);
      if Assigned(Tmp) then
      begin
      //Tmp.Transform(Scale2D(fScale, fScale));
        Lst.Add(Tmp);
      end;
    end;
    fDrawing2D.AddList(Lst);
  finally
    Lst.Free;
  end;
  fDrawing2D.DrawOnAdd := DrawOnAdd0;
end;

procedure T_TpX_Loader.ReadAll;
begin
  if fDrawing2D = nil then Exit;
  //if fStream = nil then Exit;
  if fXML.DocumentElement = nil then Exit;
  ReadHeader;
  ReadEntities;
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

function FindPicturePath(TeXFileName: string; Line: Integer):
  string;
var
  Lines: TStringList;
  I: Integer;
  function ParseLine(St: string): Boolean;
  var
    J: Integer;
  begin
    Result := False;
    J := Pos('%', St);
    if J > 0 then
      Delete(St, J, Length(St));
    J := Pos('\input{', St);
    if J <= 0 then Exit;
    Delete(St, 1, J + 6);
    J := Pos('}', St);
    if J <= 0 then Exit;
    Delete(St, J, Length(St));
    if not AnsiContainsText(St, '.tpx') then Exit;
    St := Trim(StringReplace(St, '/', '\', [rfReplaceAll]));
    if ExtractFileDrive(St) = '' then
      FindPicturePath := ExtractFilePath(TeXFileName) + St
    else FindPicturePath := St;
    Result := True;
  end;
begin
  FindPicturePath := '';
  Lines := TStringList.Create;
  Lines.LoadFromFile(TeXFileName);
  I := Line;
  repeat
    if (I < 1) or (I > Lines.Count) then Break;
    if ParseLine(Lines[I - 1]) then Break;
    if I > Line then
      if 2 * Line - I > 0
        then I := 2 * Line - I
      else Inc(I)
    else
      if 2 * Line - I + 1 <= Lines.Count
        then I := 2 * Line - I + 1
      else Dec(I);
  until False;
  Lines.Free;
end;

end.


unit InOut;

interface

uses Types, SysUtils, Classes, Windows, Graphics, ComCtrls,
  Variants, CADSys4, CS4BaseTypes, CS4Shapes,
  XUtils, XXmlDom, PdfDoc, Gr32, Gr32_Polygons;

type

  T_PS_RGB = record R, G, B: TRealType; end;

  T_CAD_Saver = class(TObject)
  private
    fDrawing2D: TDrawing2D;
    fStream: TStream;
    fW_MM, fH_MM, fExtLeft, fExtBottom, fExtTop,
      fFactorW, fFactorH, fFactorMM: TRealType;
  protected
    procedure MeasureDrawing;
    procedure WriteStream(Value: Variant);
    procedure WriteLnStream(Value: Variant);
    procedure WriteStreamPoint0(const X, Y: TRealType); virtual;
    procedure WriteStreamPoint0Int(X, Y: Integer);
    function ConvertX(X: TRealType): TRealType; virtual;
    function ConvertY(Y: TRealType): TRealType; virtual;
    function ConvertPnt(Pnt: TPoint2D): TPoint2D;
    function ConvertPntInv(Point: TPoint2D): TPoint2D; virtual;
    procedure WriteStreamPoint(Pnt: TPoint2D); virtual;
    procedure WritePoly0(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineKind: TLineKind; const Hatching: THatching;
      const Closed: Boolean); virtual;
    procedure WritePiece(Piece: TPiece; Obj: TPrimitive2D); virtual;
    procedure WritePieces(Obj: TPrimitive2D); virtual;
    procedure WriteLine2D(Obj: TLine2D); virtual;
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
    procedure WriteSmooth2D(Obj: TSmoothPath2D0); virtual;
      abstract;
    procedure WriteEntity(Obj: TObject2D);
    function GetPathString(PP: TPointsSet2D): string;
    function GetTeXText(Obj: TText2D;
      UnitLength: TRealType): string;
  public
    constructor Create(Drawing: TDrawing2D); virtual;
    procedure WriteEntities;
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
    constructor Create(Drawing: TDrawing2D); override;
    destructor Destroy; override;
    procedure WriteHeader; override;
    procedure WriteAll; override;
    procedure WriteAllToStream; override;
    procedure StoreToClipboard;
  end;

  T_SVG_Export = class(T_CAD_Saver)
  private
    fXML: TXMLDDocument;
    fDefsNode: TXMLDElement;
    fPattIDs: TStringList;
  protected
    function ConvertY(Y: TRealType): TRealType; override;
    function RegisterPatt(Hatching: THatching;
      HatchColor, FillColor: TColor): string;
    procedure WritePrimitiveAttr0(LineColor,
      HatchColor, FillColor: TColor;
      LineKind: TLineKind; Hatching: THatching;
      XMLNode: TXMLDElement);
    procedure WritePrimitiveAttr(Obj: TPrimitive2D;
      XMLNode: TXMLDElement);
    procedure WritePoly0(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineKind: TLineKind; const Hatching: THatching;
      const Closed: Boolean); override;
    procedure WritePoly(PP: TPointsSet2D;
      Obj: TPrimitive2D; Closed: Boolean);
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
    constructor Create(Drawing: TDrawing2D); override;
    destructor Destroy; override;
    procedure WriteHeader; override;
    function GetX(X: TRealType): Integer;
    function GetY(Y: TRealType): Integer;
    procedure WriteAll; override;
    procedure WriteAllToStream; override;
    procedure StoreToClipboard;
  end;

  T_PostScript_Export = class(T_CAD_Saver)
  private
    fHatchingStep: TRealType;
  protected
    TextLabels: TStringList;
    procedure WriteStreamPoint0(const X, Y: TRealType); override;
    procedure WriteColor(Color: TColor);
    procedure WriteLineAttr(const LineKind: TLineKind; const LineColor: TColor);
    procedure WriteFill0(const FillColor: TColor; const LineKind: TLineKind);
    procedure WriteFill(Obj: TPrimitive2D);
    procedure WriteStroke(const LineKind: TLineKind; const LineColor: TColor);
    procedure WritePoly00(PP: TPointsSet2D;
      LineColor, HatchColor, FillColor: TColor;
      LineKind: TLineKind; Hatching: THatching;
      Closed: Boolean);
    procedure WritePoly0(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineKind: TLineKind; const Hatching: THatching;
      const Closed: Boolean); override;
    procedure WritePoly(PP: TPointsSet2D;
      Obj: TPrimitive2D; Closed: Boolean);
    procedure WriteFont; virtual;
    procedure WriteHatching(const P: TPointsSet2D;
      const Hatching: THatching;
      const HatchColor: TColor; Step: TRealType);
    procedure WriteRectangle2D(Obj: TRectangle2D); override;
    procedure WriteEllipse2D(Obj: TEllipse2D); override;
    procedure WriteCircle2D(Obj: TCircle2D); override;
    procedure WriteCircular2D(Obj: TCircular2D); override;
    procedure WriteSpline2D(Obj: TSpline2D0); override;
    procedure WriteBezierPath(const PP: TPointsSet2D;
      Obj: TPrimitive2D);
    procedure WritePoly2D(Obj: TPolyline2D0); override;
    procedure WriteText2D(Obj: TText2D); override;
    procedure WriteStar2D(Obj: TStar2D); override;
    procedure WriteSmooth2D(Obj: TSmoothPath2D0); override;
  public
    FontName: string;
    constructor Create(Drawing: TDrawing2D); override;
    destructor Destroy; override;
    procedure WriteHeader; override;
    procedure WriteFooter; override;
    procedure WriteToTpX(Stream: TStream;
      const FileName: string); override;
    procedure StoreToClipboard;
  end;

  // Export without text and font
  T_PostScript_Light_Export = class(T_PostScript_Export)
  protected
    procedure WriteFont; override;
    procedure WriteText2D(Obj: TText2D); override;
  end;

  T_TeX_Picture_Export = class(T_CAD_Saver)
  private
    fH, fW, fUnitLength, fHatchingStep: TRealType;
  protected
    procedure WriteStreamPoint0(const X, Y: TRealType); override;
    procedure WriteLineThickness0(LineKind: TLineKind);
    procedure WriteLineThickness(Obj: TPrimitive2D);
    procedure WritePoly0(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineKind: TLineKind; const Hatching: THatching;
      const Closed: Boolean); override;
    procedure WriteLine(P0, P1: TPoint2D; Kind: TLineKind);
    procedure WriteCBezier(P0, P1, P2, P3: TPoint2D);
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
    constructor Create(Drawing: TDrawing2D); override;
    procedure WriteHeader; override;
    procedure WriteFooter; override;
    procedure StoreToClipboard;
  end;

  TPathProcAttr = procedure(const PP: TPointsSet2D;
    const Attr: string; Closed: Boolean) of object;

  T_PSTricks_Export = class(T_CAD_Saver)
  private
    fH, fW, fUnitLength, fHatchingStep: TRealType;
    Colors: TStringList;
  protected
    procedure WriteStreamPoint0(const X, Y: TRealType); override;
    function WriteNewColor(Color: TColor): string;
    function LineArg(Kind: TLineKind; Color: string): string;
    procedure WriteLine(P0, P1: TPoint2D; Obj: TPrimitive2D);
    procedure WritePoly(const PP: TPointsSet2D;
      const Attr: string; Closed: Boolean);
    procedure WriteHatching(const P: TPointsSet2D;
      const Hatching: THatching;
      const HatchColor: TColor; Step: TRealType);
    procedure WritePath(const PP, HatchPP: TPointsSet2D;
      PathProc: TPathProcAttr;
      const LineColor, HatchColor, FillColor: TColor;
      const LineKind: TLineKind; const Hatching: THatching;
      const Closed: Boolean);
    procedure WritePoly0(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineKind: TLineKind; const Hatching: THatching;
      const Closed: Boolean); override;
    procedure WriteBezier(const P0, P1, P2, P3: TPoint2D;
      const Attr: string);
    procedure WriteBezierPath(const PP: TPointsSet2D;
      const Attr: string; Closed: Boolean);
    procedure WriteCircle(const PP: TPointsSet2D;
      const Attr: string; Closed: Boolean);
    procedure WriteCircular0(const PP: TPointsSet2D;
      const Attr: string; ObjClass: TClass);
    procedure WriteArc(const PP: TPointsSet2D;
      const Attr: string; Closed: Boolean);
    procedure WriteSector(const PP: TPointsSet2D;
      const Attr: string; Closed: Boolean);
    procedure WriteSegment(const PP: TPointsSet2D;
      const Attr: string; Closed: Boolean);
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
    constructor Create(Drawing: TDrawing2D); override;
    destructor Destroy; override;
    procedure WriteHeader; override;
    procedure WriteFooter; override;
    procedure StoreToClipboard;
  end;

  TPathProc = procedure(const PP: TPointsSet2D; Closed: Boolean) of object;

  T_MetaPost_Export = class(T_CAD_Saver)
    fHatchingStep: TRealType;
  protected
    procedure WriteStreamPoint0(const X, Y: TRealType); override;
    procedure WriteColor(const Color, DefaultColor: TColor);
    procedure WriteLineAttr(const LineKind: TLineKind;
      const LineColor: TColor);
    procedure WritePoly(const PP: TPointsSet2D; Closed: Boolean);
    procedure WriteBezierPath(const PP: TPointsSet2D;
      Closed: Boolean);
    procedure WriteCircle(const PP: TPointsSet2D;
      Closed: Boolean);
    procedure WriteHatching(const P: TPointsSet2D;
      const Hatching: THatching;
      const HatchColor: TColor; Step: TRealType);
    procedure WritePath(const PP, HatchPP: TPointsSet2D;
      PathProc: TPathProc;
      const LineColor, HatchColor, FillColor: TColor;
      const LineKind: TLineKind; const Hatching: THatching;
      const Closed: Boolean);
    procedure WritePoly0(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineKind: TLineKind; const Hatching: THatching;
      const Closed: Boolean); override;
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
    procedure WriteLineAttr(const LineKind: TLineKind;
      const LineColor: TColor);
    procedure WritePoly(const PP: TPointsSet2D; Closed: Boolean);
    procedure WriteBezierPath(const PP: TPointsSet2D;
      Closed: Boolean);
    procedure WriteHatching(const P: TPointsSet2D;
      const Hatching: THatching;
      const HatchColor: TColor; const Step: TRealType);
    procedure WritePath(const PP, HatchPP: TPointsSet2D;
      PathProc: TPathProc;
      const LineColor, HatchColor, FillColor: TColor;
      const LineKind: TLineKind; const Hatching: THatching;
      const Closed: Boolean);
  protected
    TextLabels: TStringList;
    procedure WritePoly0(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineKind: TLineKind; const Hatching: THatching;
      const Closed: Boolean); override;
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
    constructor Create(Drawing: TDrawing2D); override;
    destructor Destroy; override;
    procedure WriteHeader; override;
    procedure WriteAllToStream; override;
    procedure WriteToTpX(Stream: TStream;
      const FileName: string); override;
  end;

  // Export without text
  T_PDF_Light_Export = class(T_PDF_Export)
  protected
    procedure WriteText2D(Obj: TText2D); override;
  end;

  T_Bitmap0_Export = class(T_CAD_Saver)
  private
    fBitmap: TBitmap32;
    fPolygon, fOutline: TPolygon32;
    fHatchingStep: TRealType;
    procedure WriteLine(P0, P1: TPoint2D; W: TRealType; Color: TColor);
    procedure WriteBitmapPolygon(Polygon: TPolygon32; Color: TColor);
    procedure FillPolygon(PP: TPointsSet2D; Closed: Boolean);
    procedure WriteHatching(const P: TPointsSet2D;
      const Hatching: THatching;
      const HatchColor: TColor; const Step: TRealType);
  protected
    procedure WritePoly0(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineKind: TLineKind; const Hatching: THatching;
      const Closed: Boolean); override;
    function ConvertY(Y: TRealType): TRealType; override;
    procedure WriteRectangle2D(Obj: TRectangle2D); override;
    procedure WriteEllipse2D(Obj: TEllipse2D); override;
    procedure WriteCircle2D(Obj: TCircle2D); override;
    procedure WriteCircular2D(Obj: TCircular2D); override;
    procedure WriteSpline2D(Obj: TSpline2D0); override;
    procedure WritePoly2D(Obj: TPolyline2D0); override;
    procedure WriteText2D(Obj: TText2D); override;
    procedure WriteSmooth2D(Obj: TSmoothPath2D0); override;
  public
    constructor Create(Drawing: TDrawing2D); override;
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

  // uses "light" Postscript:
  T_EpsToPdf_Light_Export = class(T_EpsToPdf_Export)
  end;

  T_TpX_Loader = class(TObject)
  private
    fStream: TStream;
    fDrawing2D: TDrawing2D;
    fXML: TXMLDDocument;
    fVersion: Single;
    PicWidth, PicHeight: TRealType;
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

const
  EOL = #13#10;

type
  T_FM_Arr = array[0..255] of TRealType;

procedure StoreToFile_Saver(const Drawing: TDrawing2D;
  const FileName: string; AClass: T_CADSaverClass);
procedure StoreToFile_TpX0(const Drawing: TDrawing2D;
  const FileName: string;
  AClass_TeX, AClass_PdfTeX: T_CADSaverClass);
procedure StoreToFile_TpX(const Drawing: TDrawing2D;
  const FileName: string);
procedure StoreToFile_MPS(const Drawing: TDrawing2D;
  const FileName: string);
procedure StoreToFile_EpsToPdf(const Drawing: TDrawing2D;
  const FileName: string; Light: Boolean);
procedure ParseParamStr(var FileName, IncludePath: string);
procedure FindPicturePath(const TeXFileName: string; const Line: Integer;
  var PicturePath, IncludePath: string);
procedure pfb2pfa(const FileName_pfb, FileName_pfa: string);
procedure Parse_PL(const FileName: string; var Heights, Depths: T_FM_Arr);
function FileExec(const aCmdLine, InFile, OutFile, Directory:
  string; aHide, aWait: Boolean): Boolean;
function TryDeleteFile(const FileName: string): Boolean;
function GetTempDir: string;
function XMLNodeText(Node: TXMLDNode; Level: Integer): string;
procedure Import_EMF(Drawing: TDrawing2D; const EmfFileName: string;
  Lines: TStrings);
procedure Import_Eps(Drawing: TDrawing2D; const EpsFileName: string);

var
  MetaPostPath: string = 'mpost.exe';
  Font_pfb_Path: string = '';
  EpsToPdfPath: string = 'epstopdf.exe';
  PsToEditPath: string = 'pstoedit.exe';

implementation

uses Math, Dialogs, Forms, Clipbrd, StrUtils, MainUnit,
  PdfTypes, pngimage, ColorEtc, EMF;

function GetColor(C, Default: TColor): TColor;
begin
  if C = clDefault then Result := Default
  else Result := C;
end;

{ --================ T_CAD_Saver ==================-- }

constructor T_CAD_Saver.Create(Drawing: TDrawing2D);
begin
  inherited Create;
  fDrawing2D := Drawing;
  fStream := nil;
end;

procedure T_CAD_Saver.MeasureDrawing;
var
  ARect: TRect2D;
  B: TRealType;
begin
  B := fDrawing2D.Border;
  ARect := fDrawing2D.DrawingExtension;
  with ARect do
  begin
    fFactorW := fDrawing2D.PicScale * fFactorMM;
    fFactorH := fDrawing2D.PicScale * fFactorMM;
    fExtLeft := Left - B / fDrawing2D.PicScale;
    fExtBottom := Bottom - B / fDrawing2D.PicScale;
    fExtTop := Top + B / fDrawing2D.PicScale;
    fW_MM := (Right - Left) * fDrawing2D.PicScale + B * 2;
    fH_MM := (Top - Bottom) * fDrawing2D.PicScale + B * 2;
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

procedure T_CAD_Saver.WriteStreamPoint0(const X, Y: TRealType);
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

procedure T_CAD_Saver.WritePoly0(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineKind: TLineKind; const Hatching: THatching;
  const Closed: Boolean);
begin

end;

procedure T_CAD_Saver.WritePiece(Piece: TPiece;
  Obj: TPrimitive2D);
var
  IsClosed: Boolean;
begin
  if Piece.Count <= 1 then Exit;
  IsClosed := IsSamePoint2D(Piece[0], Piece[Piece.Count - 1]);
  WritePoly0(Piece, Piece.GetLineColor(Obj), Obj.HatchColor,
    Piece.GetFillColor(Obj), Piece.GetLineKind(Obj), Piece.GetHatching(Obj),
    IsClosed);
end;

procedure T_CAD_Saver.WritePieces(Obj: TPrimitive2D);
var
  I: Integer;
begin
  for I := 0 to Obj.Pieces.Count - 1 do
    if Obj.Pieces[I] <> nil then
      WritePiece(Obj.Pieces[I], Obj);
end;

procedure T_CAD_Saver.WriteEntities;
var
  TmpIter: TGraphicObjIterator;
  TmpObj: TObject2D;
  I: Integer;
begin
  TmpIter := fDrawing2D.ObjectsIterator;
  I := 0;
  try
    TmpObj := TmpIter.First as TObject2D;
    while TmpObj <> nil do
    begin
      WriteEntity(TmpObj);
      TmpObj := TmpIter.Next as TObject2D;
      Inc(I);
      if I mod 100 = 0 then
      begin
        MainForm.ProgressBar1.Position :=
          Round(I / TmpIter.Count * 100);
        Application.ProcessMessages;
      end;
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

procedure T_CAD_Saver.WriteLine2D(Obj: TLine2D);
begin
  WritePieces(Obj);
end;

procedure T_CAD_Saver.WriteStar2D(Obj: TStar2D);
begin
  WritePieces(Obj);
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

function PS_RGB(Color: TColor): T_PS_RGB;
begin
  Result.R := (Color and $000000FF) / $000000FF;
  Result.G := (Color and $0000FF00) / $0000FF00;
  Result.B := (Color and $00FF0000) / $00FF0000;
end;

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

function T_CAD_Saver.GetTeXText(Obj: TText2D;
  UnitLength: TRealType): string;
var
  P, Point: TPoint2D;
  Rect: TRect2D;
  Width, H: TRealType;
  St: string;
  RGB: T_PS_RGB;
begin
  with Obj do
  begin
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
    Result := Format('\put(%.1f, %.1f){', [Point.X, Point.Y]);
    H := Height * fFactorH * UnitLength * 2.845; //in pt // mm=2.845pt
    Result := Result + Format('\fontsize{%d}{%d}\selectfont',
      [Round(H / 1.2), Round(H)]);
    if Obj.LineColor <> clDefault then
    begin
      RGB := PS_RGB(Obj.LineColor);
      Result := Result + Format('\textcolor[rgb]{%.5g, %.5g, %.5g}{',
        [RGB.R, RGB.G, RGB.B]);
    end;
    Result := Result +
      Format('\makebox(%.1f, %.1f)[', [Width * fFactorW, Height * fFactorH]);
    case HJustification of
      jhLeft: Result := Result + 'l';
      jhCenter: Result := Result + 'c';
      jhRight: Result := Result + 'r';
    end;
    case VJustification of
      jvBottom: Result := Result + 't';
      jvCenter: Result := Result + 'c';
      jvTop: Result := Result + 'b';
    end;
    if TeXText <> '' then St := TeXText
    else St := TeX_Replace_Special(Text);
    Result := Result + ']{' + St + '\strut}}';
    if Obj.LineColor <> clDefault then Result := Result + '}';
  end;
end;

procedure T_CAD_Saver.WriteHeader;
begin

end;

procedure T_CAD_Saver.WriteFooter;
begin

end;

procedure T_CAD_Saver.WriteAll;
var
  Magnif, PicScale0, Border0, LineWidth0, HatchingStep0,
    DottedSize0, DashSize0: TRealType;
begin
  if fDrawing2D = nil then Exit;
  //Changing meaning of millimeters for output picture
  Magnif := fDrawing2D.PicMagnif;
  PicScale0 := fDrawing2D.PicScale;
  Border0 := fDrawing2D.Border;
  LineWidth0 := fDrawing2D.LineWidth;
  HatchingStep0 := fDrawing2D.HatchingStep;
  DottedSize0 := fDrawing2D.DottedSize;
  DashSize0 := fDrawing2D.DashSize;
  fDrawing2D.PicScale := fDrawing2D.PicScale * Magnif;
  fDrawing2D.Border := fDrawing2D.Border * Magnif;
  fDrawing2D.LineWidth := fDrawing2D.LineWidth * Magnif;
  fDrawing2D.HatchingStep := fDrawing2D.HatchingStep * Magnif;
  fDrawing2D.DottedSize := fDrawing2D.DottedSize * Magnif;
  fDrawing2D.DashSize := fDrawing2D.DashSize * Magnif;

  WriteHeader;
  WriteEntities;
  WriteFooter;

  fDrawing2D.PicScale := PicScale0;
  fDrawing2D.Border := Border0;
  fDrawing2D.LineWidth := LineWidth0;
  fDrawing2D.HatchingStep := HatchingStep0;
  fDrawing2D.DottedSize := DottedSize0;
  fDrawing2D.DashSize := DashSize0;
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

procedure StoreToFile_Saver(const Drawing: TDrawing2D;
  const FileName: string; AClass: T_CADSaverClass);
var
  Saver: T_CAD_Saver;
begin
  Saver := AClass.Create(Drawing);
  try
    Saver.StoreToFile(FileName);
  finally
    Saver.Free;
  end;
end;

procedure StoreToFile_TpX0(const Drawing: TDrawing2D;
  const FileName: string;
  AClass_TeX, AClass_PdfTeX: T_CADSaverClass);
var
  Stream: TFileStream;
  ARect: TRect2D;
  PicWidth: TRealType;
  procedure WriteAsClass(AClass: T_CADSaverClass);
  var
    Saver: T_CAD_Saver;
  begin
    Saver := AClass.Create(Drawing);
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
  ARect := Drawing.DrawingExtension;
  PicWidth := (ARect.Right - ARect.Left) * Drawing.PicScale
    + Drawing.Border * 2;
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    WriteAsClass(T_TpX_Saver);
    if Drawing.TeXFigurePrologue <> '' then
      WriteLnStream(Drawing.TeXFigurePrologue);
    case Drawing.TeXFigure of
      fig_figure:
        if Drawing.TeXFigurePlacement <> '' then
          WriteLnStream(Format('\begin{figure}[%s]',
            [Drawing.TeXFigurePlacement]))
        else
          WriteLnStream('\begin{figure}');
      fig_floating:
        if Drawing.TeXFigurePlacement <> '' then
          WriteLnStream(Format(
            '\begin{floatingfigure}[%s]{%d mm}',
            [Drawing.TeXFigurePlacement, Ceil(PicWidth)]))
        else
          WriteLnStream(Format(
            '\begin{floatingfigure}{%d mm}',
            [Ceil(PicWidth)]));
      fig_wrap:
        if Drawing.TeXFigurePlacement <> '' then
          WriteLnStream(Format('\begin{wrapfigure}{%s}{%d mm}',
            [Drawing.TeXFigurePlacement, Ceil(PicWidth)]))
        else
          WriteLnStream(Format('\begin{wrapfigure}{r}{%d mm}',
            [Ceil(PicWidth)]));
    end;
    //WriteLnStream('\clearpage');
    if Drawing.TeXCenterFigure then WriteLnStream('\centering');
    if Drawing.TeXPicPrologue <> '' then
      WriteLnStream(Drawing.TeXPicPrologue);
    if AClass_PdfTeX <> AClass_TeX then
    begin
      WriteLnStream('\ifx\pdftexversion\undefined');
      WriteAsClass(AClass_TeX);
      WriteLnStream('\else');
      WriteAsClass(AClass_PdfTeX);
      WriteLnStream('\fi');
    end
    else WriteAsClass(AClass_TeX);
    if Drawing.TeXPicEpilogue <> '' then
      WriteLnStream(Drawing.TeXPicEpilogue);
    if (Drawing.Caption <> '') or (Drawing.FigLabel <> '') then
    begin
      if Drawing.FigLabel <> '' then
        WriteLnStream(Format('\caption{\label{%s}%s}',
          [Drawing.FigLabel, Drawing.Caption]))
      else
        WriteLnStream(Format('\caption{%s}', [Drawing.Caption]));
    end;
    //WriteLnStream('\clearpage');
    case Drawing.TeXFigure of
      fig_figure:
        WriteLnStream('\end{figure}');
      fig_floating:
        WriteLnStream('\end{floatingfigure}');
      fig_wrap:
        WriteLnStream('\end{wrapfigure}');
    end;
    if Drawing.TeXFigureEpilogue <> '' then
      WriteLnStream(Drawing.TeXFigureEpilogue);
  finally
    Stream.Free;
  end;
end;

procedure StoreToFile_TpX(const Drawing: TDrawing2D;
  const FileName: string);
var
  AClass_TeX, AClass_PdfTeX: T_CADSaverClass;
begin
  //TheDrawing.UsePSTricks then
  case Drawing.TeXFormat of
    tex_pstricks:
      AClass_TeX := T_PSTricks_Export;
    tex_eps:
      AClass_TeX := T_PostScript_Light_Export;
    tex_bmp:
      AClass_TeX := T_Bitmap_Export;
    tex_png:
      AClass_TeX := T_PNG_Export;
    tex_metapost:
      AClass_TeX := T_MetaPost_Export;
    tex_emf:
      AClass_TeX := T_EMF_Export;
  else
    AClass_TeX := T_TeX_Picture_Export;
  end;
  case Drawing.PdfTeXFormat of
    pdftex_pdf:
      AClass_PdfTeX := T_PDF_Light_Export;
    pdftex_png:
      AClass_PdfTeX := T_PNG_Export;
    pdftex_metapost:
      AClass_PdfTeX := T_MetaPost_Export;
    pdftex_epstopdf:
      AClass_PdfTeX := T_EpsToPdf_Light_Export;
  else
    AClass_PdfTeX := T_TeX_Picture_Export;
  end;
  StoreToFile_TpX0(Drawing, FileName,
    AClass_TeX, AClass_PdfTeX);
end;

{ --================ T_TpX_Saver ==================-- }

constructor T_TpX_Saver.Create(Drawing: TDrawing2D);
begin
  inherited Create(Drawing);
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
  MemStream: TMemoryStream;
  procedure WriteStream(ValueSt: string);
  begin
    fStream.Write(ValueSt[1], Length(ValueSt));
  end;
begin
  if fStream = nil then Exit;
  WriteAll;
  Lines := TStringList.Create;
  MemStream := TMemoryStream.Create;
  try
    fXML.WriteToStream(MemStream);
    MemStream.Seek(0, soFromBeginning);
    Lines.LoadFromStream(MemStream);
    for I := 0 to Lines.Count - 1 do
      WriteStream('%' + Lines[I] + EOL);
  finally
    MemStream.Free;
    Lines.Free;
  end;
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
      if fDrawing2D.TeXFormat <> tex_eps then
        AttributeValue['TeXFormat'] :=
          ChoiceToString(TeXFormat_Choice,
          Ord(fDrawing2D.TeXFormat));
      if fDrawing2D.PdfTeXFormat <> pdftex_pdf then
        AttributeValue['PdfTeXFormat'] :=
          ChoiceToString(PdfTeXFormat_Choice,
          Ord(fDrawing2D.PdfTeXFormat));
      AttributeValue['ArrowsSize'] := FF(fDrawing2D.ArrowsSize);
      AttributeValue['StarsSize'] := FF(fDrawing2D.StarsSize);
      AttributeValue['DefaultFontHeight'] :=
        FF(fDrawing2D.DefaultFontHeight);
      {AttributeValue['PicWidth'] := fDrawing2D.PicWidth;
      AttributeValue['PicHeight'] := fDrawing2D.PicHeight;}
      AttributeValue['PicScale'] := fDrawing2D.PicScale;
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
      if fDrawing2D.PicMagnif <> PicMagnif_Default
        then AttributeValue['PicMagnif'] :=
        fDrawing2D.PicMagnif;
      if fDrawing2D.MetaPostTeXText <> True
        then AttributeValue['MetaPostTeXText'] :=
        fDrawing2D.MetaPostTeXText;
      if fDrawing2D.IncludePath <> ''
        then AttributeValue['IncludePath'] :=
        fDrawing2D.IncludePath;
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
  if Obj.LineColor <> clDefault then
    XMLNode.AttributeValue['lc'] := ColorToHtml(Obj.LineColor);
  if Obj.HatchColor <> clDefault then
    XMLNode.AttributeValue['hc'] := ColorToHtml(Obj.HatchColor);
  if Obj.FillColor <> clDefault then
    XMLNode.AttributeValue['fill'] := ColorToHtml(Obj.FillColor);
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
    if StarKind <> starCircle then
      XMLNode.AttributeValue['s'] := StarsIDs[Ord(StarKind)];
    WritePrimitiveAttr(Obj, XMLNode);
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

constructor T_SVG_Export.Create(Drawing: TDrawing2D);
begin
  inherited Create(Drawing);
  RO_Init(fXML, TXMLDDocument.Create);
  fPattIDs := TStringList.Create;
end;

destructor T_SVG_Export.Destroy;
begin
  RO_Free(fXML);
  fPattIDs.Free;
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
  fXML.WriteToStream(fStream);
end;

function T_SVG_Export.RegisterPatt(Hatching: THatching;
  HatchColor, FillColor: TColor): string;
var
  ID, PathSt, HatchColorSt, FillColorSt: string;
  J, HV, D, Size: Integer;
  PattNode: TXMLDElement;
begin
  if Hatching = haNone then
  begin
    if FillColor = clDefault then Result := 'none'
    else Result := ColorToHtml(FillColor);
    Exit;
  end;
  HV := Round(fDrawing2D.HatchingStep * fFactorMM / 2);
  D := Round(fDrawing2D.HatchingStep * fFactorMM * Sqrt(2));
  case Hatching of
    haHorizontal:
      begin
        ID := 'haH';
        Size := HV * 2;
        PathSt := Format('M 0,%d L %d,%d', [HV, HV * 2, HV]);
      end;
    haVertical:
      begin
        ID := 'haV';
        Size := HV * 2;
        PathSt := Format('M %d,0 L %d,%d', [HV, HV, HV * 2])
      end;
    haFDiagonal:
      begin
        ID := 'haFD';
        Size := D;
        PathSt := Format('M 0,0 L %d,%d', [D, D])
      end;
    haBDiagonal:
      begin
        ID := 'haBD';
        Size := D;
        PathSt := Format('M %d,0 L 0,%d', [D, D])
      end;
    haCross:
      begin
        ID := 'haC';
        Size := HV * 2;
        PathSt := Format('M 0,%d L %d,%d M %d,0 L %d,%d',
          [HV, HV * 2, HV, HV, HV, HV * 2])
      end;
    haDiagCross:
      begin
        ID := 'haDC';
        Size := D;
        PathSt := Format('M 0,0 L %d,%d M %d,0 L 0,%d',
          [D, D, D, D])
      end;
    //chocolate darkcyan lightpink mediumseagreen orange lightgray mediumvioletred
  else
    begin
      ID := 'haH';
      Size := HV * 2;
      PathSt := Format('M 0,%d L %d,%d', [HV, HV * 2, HV]);
    end;
  end;
  if HatchColor = clDefault then HatchColorSt := 'black'
  else
  begin
    HatchColorSt := ColorToHtml(HatchColor);
    ID := ID + HatchColorSt;
  end;
  J := Pos('#', ID);
  if J > 0 then Delete(ID, J, J);
  if FillColor <> clDefault then
  begin
    FillColorSt := ColorToHtml(FillColor);
    ID := ID + 'F' + FillColorSt;
  end;
  Result := 'url(#' + ID + ')';
  if fPattIDs.IndexOf(ID) > 0 then Exit;
  fPattIDs.Add(ID);
  PattNode := fDefsNode.AddElement('pattern');
  with PattNode do
  begin
    AttributeValue['id'] := ID;
    AttributeValue['width'] := Size;
    AttributeValue['height'] := Size;
    AttributeValue['patternUnits'] := 'userSpaceOnUse';
    if FillColor <> clDefault then
      with PattNode.AddElement('path') do
      begin
        AttributeValue['d'] := Format('M 0,0 L 0,%d L %d,%d L %d,0 L 0,0 Z',
          [Size, Size, Size, Size]);
        AttributeValue['fill'] := FillColorSt;
        AttributeValue['stroke'] := 'none';
        AttributeValue['stroke-width'] := 0;
      end;
    with PattNode.AddElement('path') do
    begin
      AttributeValue['d'] := PathSt;
      AttributeValue['fill'] := 'none';
      AttributeValue['stroke'] := HatchColorSt;
      AttributeValue['stroke-width'] :=
        {Format('%.4gmm',
        [fDrawing2D.LineWidth / 2]);}
      Format('%.1f',
        [fDrawing2D.LineWidth / 2 * fFactorMM]);
    end;
  end;
end;

procedure T_SVG_Export.WriteHeader;
var
  XMLNode: TXMLDElement;
begin
  fFactorMM := 1 / fDrawing2D.PicUnitLength;
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
      [fW_MM]);
    AttributeValue['height'] := Format('%.2fmm',
      [fH_MM]);
    AttributeValue['viewBox'] := Format('0 0 %.1f %.1f',
      [fW_MM * fFactorMM, fH_MM * fFactorMM]);
    fXML.DocumentElement.InsertChild(
      TXmlDComment.Create('Exported from TpX drawing'));
    XMLNode := fXML.DocumentElement.AddElement('title');
    XMLNode.Text := AnsiToUtf8(fDrawing2D.Caption);
    XMLNode := fXML.DocumentElement.AddElement('desc');
    XMLNode.Text := AnsiToUtf8(fDrawing2D.Comment);
    fDefsNode := fXML.DocumentElement.AddElement('defs');
    //WritePatterns(XMLNode);
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

procedure T_SVG_Export.WritePrimitiveAttr0(LineColor,
  HatchColor, FillColor: TColor;
  LineKind: TLineKind; Hatching: THatching;
  XMLNode: TXMLDElement);
begin
  if LineKind = liNone then
    XMLNode.AttributeValue['stroke'] := 'none'
  else
  begin
    XMLNode.AttributeValue['stroke-miterlimit'] := 10;
    if LineColor = clDefault then
      XMLNode.AttributeValue['stroke'] := 'black'
    else
      XMLNode.AttributeValue['stroke'] := ColorToHtml(LineColor);
    case LineKind of
      liThick, liDotted:
        XMLNode.AttributeValue['stroke-width'] :=
          Format('%.1f', [fDrawing2D.LineWidth * 2 *
          fFactorMM]);
      liThin, liDashed:
        XMLNode.AttributeValue['stroke-width'] :=
          Format('%.1f', [fDrawing2D.LineWidth * fFactorMM]);
    end;
    case LineKind of
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
  XMLNode.AttributeValue['fill']
    := RegisterPatt(Hatching, HatchColor, FillColor);
end;

procedure T_SVG_Export.WritePrimitiveAttr(Obj: TPrimitive2D;
  XMLNode: TXMLDElement);
begin
  WritePrimitiveAttr0(Obj.LineColor,
    Obj.HatchColor, Obj.FillColor,
    Obj.LineKind, Obj.Hatching, XMLNode);
end;

procedure T_SVG_Export.WritePoly0(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineKind: TLineKind; const Hatching: THatching;
  const Closed: Boolean);
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
  if Closed then XMLNode := fXML.DocumentElement.AddElement('polygon')
  else XMLNode := fXML.DocumentElement.AddElement('polyline');
  PathSt := '';
  PPrev := Point2D(987123456, 987123456);
  for I := 0 to PP.Count - 1 do
  begin
    P1 := PP[I];
    if not IsSamePoint2D(P1, PPrev) then AddPoint(P1);
    PPrev := P1;
    if (I mod 100) = 88 then AddSt(EOL);
    if I < PP.Count - 1 then AddSt(' ');
  end;
  WritePrimitiveAttr0(LineColor, HatchColor, FillColor,
    LineKind, Hatching, XMLNode);
  XMLNode.AttributeValue['points'] := PathSt;
end;

procedure T_SVG_Export.WritePoly(PP: TPointsSet2D;
  Obj: TPrimitive2D; Closed: Boolean);
begin
  WritePoly0(PP,
    Obj.LineColor, Obj.HatchColor, Obj.FillColor,
    Obj.LineKind, Obj.Hatching, Closed);
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
    if Obj.LineColor <> clDefault then
      XMLNode.AttributeValue['fill'] := ColorToHtml(Obj.LineColor)
    else
      XMLNode.AttributeValue['fill'] := 'black';
    XMLNode.AttributeValue['style'] :=
      'font-family: ''Times New Roman''; font-weight:normal';
    //WritePrimitiveAttr(Obj, XMLNode);
  end;
end;

procedure T_SVG_Export.WriteStar2D(Obj: TStar2D);
  //P: TPoint2D;
  //XMLNode: TXMLDElement;
begin
  inherited WriteStar2D(Obj);
  {XMLNode := fXML.DocumentElement.AddElement('circle');
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
  end;}
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
begin
  WritePoly(Obj.Points, Obj, Obj.IsClosed);
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

procedure StoreToFile_SVG(const Drawing: TDrawing2D;
  const FileName: string);
var
  SVG_Export: T_SVG_Export;
begin
  SVG_Export := T_SVG_Export.Create(Drawing);
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
  L, I, J: Integer;
  T: Byte;
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

constructor T_PostScript_Export.Create(Drawing: TDrawing2D);
begin
  inherited Create(Drawing);
  TextLabels := TStringList.Create;
end;

destructor T_PostScript_Export.Destroy;
begin
  TextLabels.Free;
  inherited Destroy;
end;

procedure T_PostScript_Export.WriteStreamPoint0(const X, Y: TRealType);
begin
  WriteStream(Format('%.1f %.1f ', [X, Y]));
end;

procedure T_PostScript_Export.WriteColor(Color: TColor);
var
  RGB: T_PS_RGB;
begin
  if Color = clDefault then RGB := PS_RGB(0)
  else RGB := PS_RGB(Color);
  WriteStream(
    Format('%.5g %.5g %.5g setrgbcolor ', [RGB.R, RGB.G, RGB.B]));
end;

procedure T_PostScript_Export.WriteLineAttr(const LineKind: TLineKind;
  const LineColor: TColor);
var
  A: TRealType;
begin
  A := fFactorMM;
  with fDrawing2D do
  begin
    case LineKind of
      liNone: ; //WriteStream('0 setlinewidth [] 0 setdash ');
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
  WriteColor(LineColor);
  //if Obj.LineColor <> clDefault then  begin    RGB := PS_RGB(Obj.LineColor);  end;
end;

procedure T_PostScript_Export.WriteFill0(const FillColor: TColor;
  const LineKind: TLineKind);
begin
  if LineKind <> liNone then WriteStream('gsave ');
  if FillColor = clDefault then Exit;
  WriteColor(FillColor);
  WriteStream('fill ');
end;

procedure T_PostScript_Export.WriteFill(Obj: TPrimitive2D);
begin
  WriteFill0(Obj.FillColor, Obj.LineKind);
end;

procedure T_PostScript_Export.WriteStroke(const LineKind: TLineKind;
  const LineColor: TColor);
begin
  if LineKind <> liNone then
  begin
    //if Obj.FillColor <> clDefault then
    WriteStream('grestore ');
    WriteLineAttr(LineKind, LineColor);
    WriteStream('stroke ');
  end;
  WriteLnStream('');
end;

procedure T_PostScript_Export.WritePoly00(PP: TPointsSet2D;
  LineColor, HatchColor, FillColor: TColor;
  LineKind: TLineKind; Hatching: THatching;
  Closed: Boolean);
var
  I: Integer;
begin
  if PP.Count < 1 then Exit;
  if (LineKind = liNone) and (FillColor = clDefault) then Exit;
  WriteStream('newpath ');
  WriteStreamPoint(PP[0]);
  WriteStream('moveto ');
  for I := 1 to PP.Count - 1 do
  begin
    WriteStreamPoint(PP[I]);
    WriteStream('lineto ');
    if (I mod 10) = 0 then WriteLnStream('');
  end;
  if Closed then WriteStream('closepath ');
end;

procedure T_PostScript_Export.WritePoly0(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineKind: TLineKind; const Hatching: THatching;
  const Closed: Boolean);
begin
  if PP.Count <= 1 then Exit;
  WritePoly00(PP, clDefault, clDefault,
    GetColor(FillColor, clBlack),
    liNone, haNone, Closed);
  WriteFill0(FillColor, LineKind);
  WriteHatching(PP, Hatching, HatchColor, fHatchingStep);
  WriteStroke(LineKind, LineColor);
end;

procedure T_PostScript_Export.WritePoly(PP: TPointsSet2D;
  Obj: TPrimitive2D; Closed: Boolean);
begin
  WritePoly00(PP, clDefault, clDefault,
    GetColor(Obj.FillColor, clBlack),
    liNone, haNone, Closed);
end;

procedure T_PostScript_Export.WriteBezierPath(const PP:
  TPointsSet2D; Obj: TPrimitive2D);
var
  I: Integer;
begin
  if (Obj.LineKind = liNone) and (Obj.FillColor = clDefault) then Exit;
  WriteStream('newpath ');
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
  //if Closed then WriteStream('closepath ');
end;

procedure T_PostScript_Export.WriteHeader;
begin //mm=2.845pt ??    // 2.8346 pixel per mm

  fFactorMM := 2.8346;
  MeasureDrawing;
  fHatchingStep := fDrawing2D.HatchingStep / fDrawing2D.PicScale;
  WriteLnStream('%!PS-Adobe-3.0 EPSF-3.0');
  WriteLnStream('%%' + Format('BoundingBox: %d %d %d %d',
    [0, 0, Ceil(fW_MM * fFactorMM),
    Ceil(fH_MM * fFactorMM)])); // [0, 0, fW, fH]));
  WriteLnStream('%%Title: ' + fDrawing2D.Caption);
  WriteLnStream('%%Creator: Exported from TpX drawing');
  WriteLnStream('%%CreationDate: ' + DateTimeToStr(Now));
  WriteLnStream('%%EndComments');
  FontName := 'Times-Roman';
  TextLabels.Clear;
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

procedure T_PostScript_Export.WriteHatching(const P: TPointsSet2D;
  const Hatching: THatching; const HatchColor: TColor; Step: TRealType);
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
    if HatchColor = clDefault then WriteColor(0)
    else WriteColor(HatchColor);
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
      WritePoly(ProfilePoints, Obj, True);
      WriteFill(Obj);
      WriteHatching(ProfilePoints,
        Obj.Hatching, Obj.HatchColor, fHatchingStep);
    finally
      EndUseProfilePoints;
    end;
    WriteStroke(Obj.LineKind, Obj.LineColor);
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
    if Obj.LineColor <> clDefault then
      WriteColor(Obj.LineColor)
    else
      WriteColor(clBlack);
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
  //P: TPoint2D;
  //StarsSize: TRealType;
begin
  inherited WriteStar2D(Obj);
  {with Obj do
  begin
    WriteStream('newpath ');
    WriteColor(0);
    P := Points[0];
    WriteStreamPoint(P);
    if Obj.OwnerCAD is TDrawing2D then
      StarsSize := (Obj.OwnerCAD as TDrawing2D).StarsSize
    else StarsSize := 1;
    WriteLnStream(Format('%.1f 0 360 arc fill',
      [StarsSize * fFactorW]));
  end;}
end;

procedure T_PostScript_Export.WriteEllipse2D(Obj: TEllipse2D);
var
  PP: TPointsSet2D;
begin
  if Obj.Points.Count < 2 then Exit;
  PP := nil;
  try
    Obj.BezierPoints(PP, IdentityTransf2D);
    WriteBezierPath(PP, Obj);
  finally
    PP.Free;
  end;
  WriteFill(Obj);
  with Obj do
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints,
        Obj.Hatching, Obj.HatchColor, fHatchingStep);
      EndUseProfilePoints;
    end;
  WriteStroke(Obj.LineKind, Obj.LineColor);
end;

procedure T_PostScript_Export.WriteCircle2D(Obj: TCircle2D);
var
  CP: TPoint2D;
  R: TRealType;
begin
  with Obj do
  begin
    if (Obj.LineKind <> liNone) or (Obj.FillColor <> clDefault) then
    begin
      CP := Points[0];
      R := PointDistance2D(CP, Points[1]);
      WriteStream('newpath ');
      WriteStreamPoint(CP);
      WriteLnStream(Format('%.1f 0 360 arc ', [R * fFactorW]));
      WriteFill(Obj);
    end;
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints,
        Obj.Hatching, Obj.HatchColor, fHatchingStep);
      EndUseProfilePoints;
    end;
    WriteStroke(Obj.LineKind, Obj.LineColor);
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
    if (Obj.LineKind <> liNone) or (Obj.FillColor <> clDefault) then
    begin
      WriteStream('newpath ');
      if Obj is TSector2D then
      begin
        WriteStreamPoint(CP);
        WriteStream('moveto ');
      end;
      WriteStreamPoint(CP);
      WriteStream(Format('%.1f %.1f %.1f arc ',
        [R * fFactorW, SA * 180 / Pi, EA * 180 / Pi]));
      if Obj.IsClosed then WriteStream('closepath ');
      WriteFill(Obj);
    end;
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints,
        Obj.Hatching, Obj.HatchColor, fHatchingStep);
      EndUseProfilePoints;
    end;
    WriteStroke(Obj.LineKind, Obj.LineColor);
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
    WriteBezierPath(PP, Obj);
  finally
    PP.Free;
  end;
  WriteFill(Obj);
  with Obj do
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints,
        Obj.Hatching, Obj.HatchColor, fHatchingStep);
      EndUseProfilePoints;
    end;
  WriteStroke(Obj.LineKind, Obj.LineColor);
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
    WriteBezierPath(PP, Obj);
  finally
    PP.Free;
  end;
  WriteFill(Obj);
  with Obj do
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints,
        Obj.Hatching, Obj.HatchColor, fHatchingStep);
      EndUseProfilePoints;
    end;
  WriteStroke(Obj.LineKind, Obj.LineColor);
end;

procedure T_PostScript_Export.WritePoly2D(Obj: TPolyline2D0);
var
  PP: TPointsSet2D;
begin
  if Obj.Points.Count = 0 then Exit;
  PP := Obj.Points;
  WritePoly(PP, Obj, Obj.IsClosed);
  WriteFill(Obj);
  WriteHatching(PP,
    Obj.Hatching, Obj.HatchColor, fHatchingStep);
  WriteStroke(Obj.LineKind, Obj.LineColor);
end;

procedure T_PostScript_Export.WriteToTpX(Stream: TStream;
  const FileName: string);
var
  H, W, UnitLength: TRealType;
  I: Integer;
begin
  fStream := nil;
  StoreToFile(ChangeFileExt(FileName, '.eps'));
  fStream := Stream;
  UnitLength := fDrawing2D.PicUnitLength;
  W := fW_MM / UnitLength;
  H := fH_MM / UnitLength;
  try
    WriteLnStream(Format(
      '  \setlength{\unitlength}{%.4g mm}', [UnitLength]));
    WriteLnStream(Format('  \begin{picture}(%.1f, %.1f)(0,0)', [W, H]));
    WriteLnStream(Format(
      '  \put(0,0){\includegraphics{%s%s}}', //[width=%.1fcm,height=%.1fcm]
      [//fDrawing2D.PicWidth / 10, fDrawing2D.PicHeight / 10,
      fDrawing2D.IncludePath,
        ChangeFileExt(ExtractFileName(FileName), '')]));
    for I := 0 to TextLabels.Count - 1 do
      WriteLnStream('  ' + TextLabels[I]);
    WriteLnStream('  \end{picture}');
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

{ --================ T_PostScript_Light_Export ==================-- }

procedure T_PostScript_Light_Export.WriteFont;
begin

end;

procedure T_PostScript_Light_Export.WriteText2D(Obj: TText2D);
var
  TempFW, TempFH: TRealType;
begin
  TempFW := fFactorW;
  TempFH := fFactorH;
  fFactorW := fFactorW / fDrawing2D.PicUnitLength / 2.8346;
  fFactorH := fFactorH / fDrawing2D.PicUnitLength / 2.8346;
  TextLabels.Add(GetTeXText(Obj, fDrawing2D.PicUnitLength));
  fFactorW := TempFW;
  fFactorH := TempFH;
end;

{ --================ T_TeX_Picture_Export ==================-- }

constructor T_TeX_Picture_Export.Create(Drawing: TDrawing2D);
begin
  inherited Create(Drawing);
end;

procedure T_TeX_Picture_Export.WriteStreamPoint0(const X, Y: TRealType);
begin
  WriteStreamPoint0Int(Round(X), Round(Y));
end;

procedure T_TeX_Picture_Export.WriteLineThickness0(LineKind: TLineKind);
begin
//\linethickness{dimension}
  case LineKind of
    liThick: WriteStream('\thicklines');
    liNone, liThin: WriteStream('\thinlines');
    liDashed: WriteStream('\thinlines');
    liDotted: WriteStream('\thicklines');
  end;
end;

procedure T_TeX_Picture_Export.WriteLineThickness(Obj:
  TPrimitive2D);
begin
  if (Obj.LineKind <> liNone) or (Obj.Hatching = haNone) then
    WriteLineThickness0(Obj.LineKind);
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
    liNone, liThin, liThick:
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

procedure T_TeX_Picture_Export.WriteCBezier(P0, P1, P2, P3: TPoint2D);
  function ConvertP(P: TPoint2D): TPoint2D;
  begin
    Result := Point2D(Round(ConvertX(P.X)), Round(ConvertY(P.Y)));
  end;
  procedure WritePoint(P: TPoint2D);
  begin
    WriteStreamPoint0Int(Round(P.X), Round(P.Y));
  end;
begin
  P0 := ConvertP(P0);
  P1 := ConvertP(P1);
  P2 := ConvertP(P2);
  P3 := ConvertP(P3);
  if IsSamePoint2D(P0, P1) or IsSamePoint2D(P0, P2)
    or IsSamePoint2D(P0, P3) then Exit;
  WriteStream('\cbezier');
  WritePoint(P0);
  WritePoint(P1);
  WritePoint(P2);
  WritePoint(P3);
end;

procedure T_TeX_Picture_Export.WritePoly0(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineKind: TLineKind; const Hatching: THatching;
  const Closed: Boolean);
var
  I: Integer;
begin
  if PP.Count = 0 then Exit;
  if (LineKind <> liNone) or (Hatching = haNone) then
  begin
    WriteLineThickness0(LineKind);
    for I := 0 to PP.Count - 2 do
    begin
      WriteLine(PP[I], PP[I + 1], LineKind);
      if (I mod 100) = 99 then
      begin
        WriteLnStream('');
        WriteStream(' ');
      end;
    end;
    if Closed then
      WriteLine(PP[PP.Count - 1], PP[0], LineKind);
  end;
  WriteHatching(PP, Hatching, fHatchingStep);
  WriteLnStream('');
end;

procedure T_TeX_Picture_Export.WriteHeader;
begin

  fUnitLength := fDrawing2D.PicUnitLength;
  fFactorMM := 1 / fUnitLength;
  MeasureDrawing;
  fW := fW_MM * fFactorMM;
  fH := fH_MM * fFactorMM;
  fHatchingStep := fDrawing2D.HatchingStep;
  //WriteLnStream('\clearpage');
  WriteLnStream(Format(
    '\setlength{\unitlength}{%.4g mm}', [fUnitLength]));
  WriteStream('\begin{picture}');
  WriteStreamPoint0(fW, fH);
  WriteLnStream('(0,0)');
end;

procedure T_TeX_Picture_Export.WriteFooter;
begin
  WriteLnStream('\end{picture}');
  //WriteLnStream('\clearpage');
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
      Step / fFactorH * fFactorMM, Lines);
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
    if (LineKind <> liNone) or (Hatching = haNone) then
    begin
      WriteLine(P[0], P[1], Obj.LineKind);
      WriteLine(P[1], P[2], Obj.LineKind);
      WriteLine(P[2], P[3], Obj.LineKind);
      WriteLine(P[3], P[0], Obj.LineKind);
    end;
  end;
  WriteHatching(P, Obj.Hatching, fHatchingStep);
  P.Free;
  WriteLnStream('');
end;

procedure T_TeX_Picture_Export.WriteText2D(Obj: TText2D);
begin
  WriteLnStream(GetTeXText(Obj, fUnitLength));
end;

procedure T_TeX_Picture_Export.WriteStar2D(Obj: TStar2D);
var
  P: TPoint2D;
  StarsSize: TRealType;
  D: Integer;
begin
  if Obj.StarKind = starCircle then
  begin
    WriteStream('\put');
    with Obj do
    begin
      P := Points[0];
      WriteStreamPoint(P); //\thicklines
      WriteStream('{\circle*{');
      if Obj.OwnerCAD is TDrawing2D then
      begin
        StarsSize := (Obj.OwnerCAD as TDrawing2D).StarsSize;
        {??case LineKind of
          liThin, liDashed:
            StarsSize := StarsSize +
              0.5 * (Obj.OwnerCAD as TDrawing2D).LineWidth;
          liThick, liDotted:
            StarsSize := StarsSize +
              1 * (Obj.OwnerCAD as TDrawing2D).LineWidth;
        end;}
      end
      else StarsSize := 1;
      D := Round(2 * StarsSize * fFactorW);
      WriteStream(D);
      WriteStream('}}');
    //\put(973,973){\thinlines\circle*{60}}
    end;
    WriteLnStream('');
  end
  else
    inherited WriteStar2D(Obj);
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
    if (LineKind <> liNone) or (Hatching = haNone) then
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
    end;
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
    if (LineKind <> liNone) or (Hatching = haNone) then
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
    end;
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
  if (Obj.LineKind <> liNone) or (Obj.Hatching = haNone) then
  begin
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
    end;
    with Obj do
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
  if (Obj.LineKind <> liNone) or (Obj.Hatching = haNone) then
  begin
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
  if (Obj.LineKind <> liNone) or (Obj.Hatching = haNone) then
  begin
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
  if (Obj.LineKind <> liNone) or (Obj.Hatching = haNone) then
  begin
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
begin
  WriteLineThickness(Obj);
  if Obj.Points.Count < 2 then Exit;
  if (Obj.LineKind <> liNone) or (Obj.Hatching = haNone) then
  begin
    PP := nil;
    try
      Obj.BezierPoints(PP, IdentityTransf2D);
      for I := 0 to (PP.Count - 4) div 3 do
        WriteCBezier(PP[I*3], PP[I*3 + 1], PP[I*3 + 2], PP[I*3 + 3]);
    finally
      PP.Free;
    end;
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
begin
  WritePoly0(Obj.Points,
    Obj.LineColor, Obj.HatchColor, Obj.FillColor,
    Obj.LineKind, Obj.Hatching, Obj.IsClosed);
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

constructor T_PSTricks_Export.Create(Drawing: TDrawing2D);
begin
  inherited Create(Drawing);
  Colors := TStringList.Create;
end;

destructor T_PSTricks_Export.Destroy;
begin
  Colors.Free;
  inherited Destroy;
end;

procedure T_PSTricks_Export.WriteStreamPoint0(const X, Y: TRealType);
begin
  WriteStreamPoint0Int(Round(X), Round(Y));
end;

function T_PSTricks_Export.WriteNewColor(Color: TColor): string;
var
  RGB: T_PS_RGB;
begin
  if Color = clDefault then
  begin
    Result := '';
    Exit;
  end;
  Result := ColorToHtml(Color);
  if Result[1] = '#' then Result[1] := 'C';
  if Colors.IndexOf(Result) >= 0 then Exit;
  Colors.Add(Result);
  RGB := PS_RGB(Color);
  WriteLnStream(Format('\newrgbcolor{%s}{%.5g %.5g %.5g}',
    [Result, RGB.R, RGB.G, RGB.B]));
end;

function T_PSTricks_Export.LineArg(Kind: TLineKind; Color: string):
  string;
begin
  case Kind of
    liThick, liDotted: Result := Format('linewidth=%.2fmm',
        [fDrawing2D.LineWidth * 2]);
    liThin, liDashed: Result :=
      Format('linewidth=%.2fmm', [fDrawing2D.LineWidth]);
//      fprintf(file, "%f %f %f setrgbcolor\n", r, g, b);
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
  if Color <> '' then
    Result := Result + ',linecolor=' + Color;
  Result := Format('[%s]', [Result]);
end;

procedure T_PSTricks_Export.WriteLine(P0, P1: TPoint2D;
  Obj: TPrimitive2D);
var
  C: string;
begin
  if IsSamePoint2D(P0, P1) then Exit;
  C := WriteNewColor(Obj.LineColor);
  WriteStream(Format('\psline%s', [LineArg(Obj.LineKind, C)]));
  WriteStreamPoint(P0);
  WriteStreamPoint(P1);
end;

procedure T_PSTricks_Export.WritePoly(const PP: TPointsSet2D;
  const Attr: string; Closed: Boolean);
var
  I: Integer;
begin
  if PP.Count < 1 then Exit;
  if Closed then
    WriteStream(Format('\pspolygon%s', [Attr]))
  else
    WriteStream(Format('\psline%s', [Attr]));
  for I := 0 to PP.Count - 1 do
    WriteStreamPoint(PP[I]);
end;

procedure T_PSTricks_Export.WriteHatching(const P: TPointsSet2D;
  const Hatching: THatching;
  const HatchColor: TColor; Step: TRealType);
var
  Lines: TPointsSet2D;
  DX, DY: TRealType;
  C: string;
  procedure WriteHatching0;
  var
    I: Integer;
  begin
    if (DX = 0) and (DY = 0) then Exit;
    CalculateHatching(P, DX, DY,
      Step / fFactorH / fUnitLength, Lines);
    for I := 0 to Lines.Count div 2 - 1 do
    begin
      if HatchColor = clDefault then C := ''
      else
        C := ',linecolor=' + WriteNewColor(HatchColor);
      WriteStream(Format('\psline[linewidth=%.2fmm,linestyle=solid%s]',
        [fDrawing2D.LineWidth / 2, C]));
      WriteStreamPoint(Lines[I * 2]);
      WriteStreamPoint(Lines[I * 2 + 1]);
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

procedure T_PSTricks_Export.WritePath(const PP, HatchPP: TPointsSet2D;
  PathProc: TPathProcAttr;
  const LineColor, HatchColor, FillColor: TColor;
  const LineKind: TLineKind; const Hatching: THatching;
  const Closed: Boolean);
var
  Attr: string;
begin
  if (FillColor <> clDefault) and (HatchPP <> nil) then
  begin
    Attr := '[linestyle=none,fillstyle=solid,fillcolor=' +
      WriteNewColor(FillColor) + ']';
    WritePoly(HatchPP, Attr, Closed);
  end;
  if HatchPP <> nil then
    WriteHatching(HatchPP, Hatching, HatchColor, fHatchingStep);
  if LineKind <> liNone then
  begin
    Attr := LineArg(LineKind, WriteNewColor(LineColor));
    PathProc(PP, Attr, Closed);
  end;
  WriteLnStream('');
end;

procedure T_PSTricks_Export.WritePoly0(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineKind: TLineKind; const Hatching: THatching;
  const Closed: Boolean);
begin
  WritePath(PP, PP, WritePoly, LineColor, HatchColor, FillColor,
    LineKind, Hatching, Closed);
end;

procedure T_PSTricks_Export.WriteBezier(const P0, P1, P2, P3: TPoint2D;
  const Attr: string);
begin
  WriteStream(Format('\psbezier%s', [Attr]));
  WriteStreamPoint(P0);
  WriteStreamPoint(P1);
  WriteStreamPoint(P2);
  WriteStreamPoint(P3);
end;

procedure T_PSTricks_Export.WriteBezierPath(const PP: TPointsSet2D;
  const Attr: string; Closed: Boolean);
var
  I: Integer;
  P0: TPoint2D;
begin
  if PP.Count < 1 then Exit;
  P0 := PP[0];
  for I := 0 to PP.Count div 3 - 1 do
  begin
    WriteBezier(PP[3 * I], PP[3 * I + 1], PP[3 * I + 2],
      PP[3 * I + 3], Attr);
  end;
end;

procedure T_PSTricks_Export.WriteCircle(const PP: TPointsSet2D;
  const Attr: string; Closed: Boolean);
begin
  WriteStream(Format('\pscircle%s', [Attr]));
  WriteStreamPoint(PP[0]);
  WriteStream(Format('{%.2f}',
    [PointDistance2D(PP[0], PP[1]) * fFactorW]));
end;

procedure T_PSTricks_Export.WriteCircular0(const PP: TPointsSet2D;
  const Attr: string; ObjClass: TClass);
var
  CP, P1, P2: TPoint2D;
  R, SA, EA, Delt: TRealType;
  PP2: TPointsSet2D;
  function GetPoint(A, R: TRealType): TPoint2D;
  begin
    Result := Point2D(CP.X + R * Cos(A), CP.Y + R * Sin(A));
  end;
begin
  CP := PP[0];
  P1 := PP[1];
  P2 := PP[2];
  R := PointDistance2D(CP, P1);
  SA := ArcTan2(P1.Y - CP.Y, P1.X - CP.X);
  EA := ArcTan2(P2.Y - CP.Y, P2.X - CP.X);
  if EA < SA then EA := EA + 2 * Pi;
  if ObjClass = TSector2D then
    WriteStream(Format('\pswedge%s', [Attr]))
  else
    WriteStream(Format('\psarc%s{-}', [Attr]));
  WriteStreamPoint(CP);
  WriteStream(Format('{%.2f}{%.2f}{%.2f}',
    [R * fFactorW, SA * 180 / Pi, EA * 180 / Pi]));
  if ObjClass = TSegment2D then
  begin
    PP2 := TPointsSet2D.Create(4);
    try
      Delt := fDrawing2D.LineWidth / fUnitLength
        / (R * fFactorW); // Draw miters
      PP2.Add(GetPoint(SA + Delt, R));
      PP2.Add(GetPoint(SA, R));
      PP2.Add(GetPoint(EA, R));
      PP2.Add(GetPoint(EA - Delt, R));
      WritePoly(PP2, Attr, False);
    finally
      PP2.Free;
    end;
  end;
end;

procedure T_PSTricks_Export.WriteArc(const PP: TPointsSet2D;
  const Attr: string; Closed: Boolean);
begin
  WriteCircular0(PP, Attr, TArc2D);
end;

procedure T_PSTricks_Export.WriteSector(const PP: TPointsSet2D;
  const Attr: string; Closed: Boolean);
begin
  WriteCircular0(PP, Attr, TSector2D);
end;

procedure T_PSTricks_Export.WriteSegment(const PP: TPointsSet2D;
  const Attr: string; Closed: Boolean);
begin
  WriteCircular0(PP, Attr, TSegment2D);
end;

procedure T_PSTricks_Export.WriteHeader;
begin

  fUnitLength := fDrawing2D.PicUnitLength;
  fFactorMM := 1 / fUnitLength;
  MeasureDrawing;
  fW := fW_MM * fFactorMM;
  fH := fH_MM * fFactorMM;
  fHatchingStep := fDrawing2D.HatchingStep;
  //WriteLnStream('\clearpage');
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
  //WriteLnStream('\clearpage');
end;

procedure T_PSTricks_Export.WriteRectangle2D(Obj: TRectangle2D);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    WritePath(ProfilePoints, ProfilePoints, WritePoly,
      Obj.LineColor, Obj.HatchColor, Obj.FillColor,
      Obj.LineKind, Obj.Hatching, True);
    EndUseProfilePoints;
  end;
end;

procedure T_PSTricks_Export.WriteText2D(Obj: TText2D);
begin
  WriteLnStream(GetTeXText(Obj, fUnitLength));
end;

procedure T_PSTricks_Export.WriteStar2D(Obj: TStar2D);
//var  P: TPoint2D;
begin
  {with Obj do
  begin
    P := Points[0];
    WriteStream('\psdots*[]');
    WriteStreamPoint(P);
  end;
  WriteLnStream('');}
  inherited WriteStar2D(Obj);
end;

procedure T_PSTricks_Export.WriteEllipse2D(Obj: TEllipse2D);
var
  PP: TPointsSet2D;
begin
  if Obj.Points.Count < 2 then Exit;
  with Obj do
  begin
    PP := nil;
    BeginUseProfilePoints;
    try
      Obj.BezierPoints(PP, IdentityTransf2D);
      WritePath(PP, ProfilePoints, WriteBezierPath,
        Obj.LineColor, Obj.HatchColor, Obj.FillColor,
        Obj.LineKind, Obj.Hatching, True);
    finally
      PP.Free;
      EndUseProfilePoints;
    end;
  end;
end;

procedure T_PSTricks_Export.WriteCircle2D(Obj: TCircle2D);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    WritePath(Points, ProfilePoints, WriteCircle,
      Obj.LineColor, Obj.HatchColor, Obj.FillColor,
      Obj.LineKind, Obj.Hatching, True);
    EndUseProfilePoints;
  end;
end;

procedure T_PSTricks_Export.WriteCircular2D(Obj: TCircular2D);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    if Obj is TSector2D then
      WritePath(Points, ProfilePoints, WriteSector,
        Obj.LineColor, Obj.HatchColor, Obj.FillColor,
        Obj.LineKind, Obj.Hatching, Obj.IsClosed)
    else if Obj is TSegment2D then
      WritePath(Points, ProfilePoints, WriteSegment,
        Obj.LineColor, Obj.HatchColor, Obj.FillColor,
        Obj.LineKind, Obj.Hatching, Obj.IsClosed)
    else
      WritePath(Points, ProfilePoints, WriteArc,
        Obj.LineColor, Obj.HatchColor, Obj.FillColor,
        Obj.LineKind, Obj.Hatching, Obj.IsClosed);
    EndUseProfilePoints;
  end;
end;

procedure T_PSTricks_Export.WriteSpline2D(Obj: TSpline2D0);
var
  PP: TPointsSet2D;
begin
  if Obj.Points.Count < 2 then Exit;
  with Obj do
  begin
    PP := nil;
    BeginUseProfilePoints;
    try
      Obj.BezierPoints(PP, IdentityTransf2D);
      WritePath(PP, ProfilePoints, WriteBezierPath,
        Obj.LineColor, Obj.HatchColor, Obj.FillColor,
        Obj.LineKind, Obj.Hatching, Obj.IsClosed);
    finally
      PP.Free;
      EndUseProfilePoints;
    end;
  end;
end;

procedure T_PSTricks_Export.WriteSmooth2D(Obj: TSmoothPath2D0);
var
  PP: TPointsSet2D;
begin
  if Obj.Points.Count < 2 then Exit;
  with Obj do
  begin
    PP := nil;
    BeginUseProfilePoints;
    try
      Obj.BezierPoints(PP, IdentityTransf2D);
      WritePath(PP, ProfilePoints, WriteBezierPath,
        Obj.LineColor, Obj.HatchColor, Obj.FillColor,
        Obj.LineKind, Obj.Hatching, Obj.IsClosed);
    finally
      PP.Free;
      EndUseProfilePoints;
    end;
  end;
end;

procedure T_PSTricks_Export.WritePoly2D(Obj: TPolyline2D0);
begin
  with Obj do
    WritePath(Points, Points, WritePoly,
      Obj.LineColor, Obj.HatchColor, Obj.FillColor,
      Obj.LineKind, Obj.Hatching, Obj.IsClosed);
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

const
  CMR_HT: array[0..255] of Real =
  (0.683332, 0.683332, 0.683332, 0.683332, 0.683332, 0.683332, 0.683332,
    0.683332, 0.683332, 0.683332, 0.683332, 0.694445, 0.694445, 0.694445,
    0.694445, 0.694445, 0.430555, 0.430555, 0.694445, 0.694445, 0.628473,
    0.694445, 0.567777, 0.694445, 0, 0.694445, 0.430555, 0.430555,
    0.527779, 0.683332, 0.683332, 0.731944, 0.430555, 0.694445, 0.694445,
    0.694445, 0.75, 0.75, 0.694445, 0.694445, 0.75, 0.75, 0.75, 0.583334,
    0.105556, 0.430555, 0.105556, 0.75, 0.644444, 0.644444, 0.644444,
    0.644444, 0.644444, 0.644444, 0.644444, 0.644444, 0.644444, 0.644444,
    0.430555, 0.430555, 0.5, 0.366875, 0.5, 0.694445, 0.694445, 0.683332,
    0.683332, 0.683332, 0.683332, 0.683332, 0.683332, 0.683332, 0.683332,
    0.683332, 0.683332, 0.683332, 0.683332, 0.683332, 0.683332, 0.683332,
    0.683332, 0.683332, 0.683332, 0.683332, 0.683332, 0.683332, 0.683332,
    0.683332, 0.683332, 0.683332, 0.683332, 0.75, 0.694445, 0.75,
    0.694445, 0.667859, 0.694445, 0.430555, 0.694445, 0.430555, 0.694445,
    0.430555, 0.694445, 0.430555, 0.694445, 0.667859, 0.667859, 0.694445,
    0.694445, 0.430555, 0.430555, 0.430555, 0.430555, 0.430555, 0.430555,
    0.430555, 0.61508, 0.430555, 0.430555, 0.430555, 0.430555, 0.430555,
    0.430555, 0.430555, 0.430555, 0.694445, 0.667859, 0.667859, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

  CMR_DP: array[0..255] of Real =
  (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.194445, 0, 0, 0,
    0, 0, 0, 0.170138, 0, 0, 0, 0.097223, 0, 0, 0.048612, 0, 0, 0,
    0.194443, 0.055555, 0.055555, 0, 0, 0.25, 0.25, 0, 0.083334, 0.194445,
    0, 0, 0.25, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.194445, 0.194445, -
    0.133125, 0.194445, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0.194445, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.25, 0, 0.25, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0.194445, 0, 0, 0.194445, 0, 0, 0, 0, 0, 0.194445,
    0.194445, 0, 0, 0, 0, 0, 0, 0, 0.194445, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0);

procedure Parse_PL(const FileName: string; var Heights, Depths: T_FM_Arr);
var //Parses *.pl font metric file
  FMList: TStringList;
  L, J, I: Integer;
  CurrOrd: Byte; //code of current character
  St, USt: string;
begin
  for I := 0 to 255 do
  begin
    Heights[I] := 0;
    Depths[I] := 0;
  end;
  CurrOrd := 0;
  FMList := TStringList.Create;
  try
    FMList.LoadFromFile(FileName);
    for L := 0 to FMList.Count - 1 do
    begin
      St := FMList[L];
      USt := UpperCase(St);
      J := Pos('CHARACTER O', USt);
      if J > 0 then //convert octal code to decimal
      begin
        Delete(St, 1, J + 10);
        St := Trim(St);
        CurrOrd := 0;
        for I := 1 to Length(St) do
          CurrOrd := CurrOrd * 8 + Ord(St[I]) - Ord('0');
        Continue;
      end;
      J := Pos('CHARACTER C', USt);
      if J > 0 then //get character code
      begin
        Delete(St, 1, J + 10);
        St := Trim(St);
        CurrOrd := Ord(St[1]);
        Continue;
      end;
      J := Pos('CHARHT R', USt);
      if J > 0 then //get character height
      begin
        Delete(St, 1, J + 7);
        J := Pos(')', St);
        if J = 0 then J := Length(St);
        St := Trim(Copy(St, 1, J - 1));
        Heights[CurrOrd] := StrToFloat(St);
        Continue;
      end;
      J := Pos('CHARDP R', USt);
      if J > 0 then //get character height
      begin
        Delete(St, 1, J + 7);
        J := Pos(')', St);
        if J = 0 then J := Length(St);
        St := Trim(Copy(St, 1, J - 1));
        Depths[CurrOrd] := StrToFloat(St);
        Continue;
      end;
    end;
  finally
    FMList.Free;
  end;
end;

procedure T_MetaPost_Export.WriteStreamPoint0(const X, Y: TRealType);
begin
  WriteStream(Format('(%.1fu,%.1fu)', [X, Y]));
end;

procedure T_MetaPost_Export.WriteColor(const Color, DefaultColor: TColor);
var
  RGB: T_PS_RGB;
begin
  if Color = clDefault then RGB := PS_RGB(DefaultColor)
  else RGB := PS_RGB(Color);
  WriteStream(Format(' withcolor (%.5g, %.5g, %.5g)',
    [RGB.R, RGB.G, RGB.B]));
end;

procedure T_MetaPost_Export.WriteLineAttr(const LineKind: TLineKind;
  const LineColor: TColor);
begin
  case LineKind of
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
  case LineKind of
    liDotted:
      WriteStream(Format(' dashed dashpattern(on %.2fmm off %.2fmm)',
        [fDrawing2D.LineWidth * 2, fDrawing2D.DottedSize
        ]));
    liDashed:
      WriteStream(Format(' dashed dashpattern(on %.2fmm off %.2fmm)',
        [fDrawing2D.DashSize * 2, fDrawing2D.DashSize
        ]));
  end;
  WriteColor(LineColor, clBlack);
end;

procedure T_MetaPost_Export.WritePoly(const PP: TPointsSet2D;
  Closed: Boolean);
var
  I: Integer;
begin
  WriteStream('pp := ');
  WriteStreamPoint(PP[0]);
  for I := 1 to PP.Count - 1 do
  begin
    if (I mod 10) = 0 then WriteLnStream('  ');
    WriteStream('--');
    WriteStreamPoint(PP[I]);
  end;
  WriteLnStream(';');
end;

procedure T_MetaPost_Export.WriteBezierPath(const PP: TPointsSet2D;
  Closed: Boolean);
var
  I: Integer;
begin
  WriteStream('pp := ');
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
  WriteLnStream(';');
end;

procedure T_MetaPost_Export.WriteCircle(const PP: TPointsSet2D;
  Closed: Boolean);
begin
  WriteStream(Format('pp := fullcircle scaled %.1fu shifted ',
    [PointDistance2D(PP[0], PP[1]) * fFactorH * 2]));
  WriteStreamPoint(PP[0]);
  WriteLnStream(';');
end;

procedure T_MetaPost_Export.WriteHatching(const P: TPointsSet2D;
  const Hatching: THatching;
  const HatchColor: TColor; Step: TRealType);
var
  Lines: TPointsSet2D;
  DX, DY: TRealType;
  RGB: T_PS_RGB;
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
      if HatchColor <> clDefault then
      begin
        RGB := PS_RGB(HatchColor);
        WriteStream(Format(' withcolor (%.5g, %.5g, %.5g)',
          [RGB.R, RGB.G, RGB.B]));
      end;
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

procedure T_MetaPost_Export.WritePath(const PP, HatchPP: TPointsSet2D;
  PathProc: TPathProc;
  const LineColor, HatchColor, FillColor: TColor;
  const LineKind: TLineKind; const Hatching: THatching;
  const Closed: Boolean);
begin
  if (PP = nil) or (PP.Count < 1) then Exit;
  if FillColor <> clDefault then
  begin
    PathProc(PP, Closed);
    WriteStream('fill pp');
    //if @PathProc <> @WriteCircle then??
    WriteStream('--cycle');
    WriteColor(FillColor, clBlack);
    WriteLnStream(';');
  end;
  if HatchPP <> nil then
    WriteHatching(HatchPP, Hatching, HatchColor, fHatchingStep);
  if LineKind = liNone then Exit;
  if FillColor = clDefault then PathProc(PP, Closed);
  WriteStream('draw pp');
  if Closed then WriteStream('--cycle');
  WriteLineAttr(LineKind, LineColor);
  WriteLnStream(';');
end;

procedure T_MetaPost_Export.WritePoly0(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineKind: TLineKind; const Hatching: THatching;
  const Closed: Boolean);
begin
  WritePath(PP, PP, WritePoly,
    LineColor, HatchColor, FillColor,
    LineKind, Hatching, Closed);
end;

procedure T_MetaPost_Export.WriteHeader;
  function WriteTexInclude: string;
  var
    IncludeFile: string;
    List: TStringList;
  begin
    Result := '';
    IncludeFile := ExtractFilePath(Application.ExeName) + 'metapost.tex.inc';
    List := TStringList.Create;
    try
      if FileExists(IncludeFile) then List.LoadFromFile(IncludeFile)
      else
      begin
        List.Add('\documentclass[a4paper,10pt]{article}');
    //\usepackage[english,russian]{babel} \usepackage[cp1251]{inputenc}
        //List.Add('\usepackage{color}');
        List.SaveToFile(IncludeFile);
      end;
      Result := List.Text;
    finally
      List.Free;
    end;
  end;
begin

  fFactorMM := 1 / fDrawing2D.PicUnitLength;
  MeasureDrawing;
  fHatchingStep := fDrawing2D.HatchingStep / fDrawing2D.PicScale;
  WriteLnStream('%Exported from TpX drawing');
  WriteLnStream('%Caption: ' + fDrawing2D.Caption);
  WriteLnStream('%CreationDate: ' + DateTimeToStr(Now));
  //WriteLnStream('');
  //WriteFontMacro;
  WriteLnStream('beginfig(0);');
  if fDrawing2D.MetaPostTeXText then
  begin
    WriteLnStream('verbatimtex %&latex');
    WriteStream(WriteTexInclude);
    WriteLnStream(' \begin{document} etex');
  end;
  WriteLnStream(Format('u=%.4g mm;',
    [fDrawing2D.PicUnitLength]));
  WriteLnStream('linecap:=butt;');
  WriteLnStream('linejoin:=mitered;');
  WriteLnStream('path pp;');
end;

procedure T_MetaPost_Export.WriteFooter;
begin
  WriteLnStream('endfig;');
  WriteLnStream('end');
end;

procedure T_MetaPost_Export.WriteRectangle2D(Obj:
  TRectangle2D);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    WritePath(ProfilePoints, ProfilePoints, WritePoly,
      Obj.LineColor, Obj.HatchColor, Obj.FillColor,
      Obj.LineKind, Obj.Hatching, True);
    EndUseProfilePoints;
  end;
end;

procedure T_MetaPost_Export.WriteText2D(Obj: TText2D);
var
  St: string;
  H, Alt, MaxDepth, MaxFontDepth, MaxHeight: TRealType;
  I: Integer;
begin
  MaxFontDepth := 0;
  for I := 0 to 255 do
    if CMR_DP[I] > MaxFontDepth then MaxFontDepth := CMR_DP[I];
  with Obj do
  begin
    //WriteLnStream('defaultfont:="ftm";');
    //WriteLnStream('defaultfont:="Times-Roman";');
    //WriteLnStream('defaultfont:="cmr10";');
    WriteLnStream(Format('defaultscale:=%.1fu/fontsize defaultfont;',
      [Obj.Height * fFactorH / 1.2])); //%.1fmm, Obj.Height * fFactorH
    if TeXText <> '' then St := TeXText else St := TeX_Replace_Special(Text);
    if not fDrawing2D.MetaPostTeXText then
    begin
      MaxDepth := 0;
      MaxHeight := 0;
      for I := 1 to Length(St) do
      begin
        Alt := CMR_DP[Ord(St[I])];
        if Alt > MaxDepth then MaxDepth := Alt;
        Alt := CMR_HT[Ord(St[I])];
        if Alt > MaxHeight then MaxHeight := Alt;
      end;
      case VJustification of
        jvBottom: Alt := MaxFontDepth - MaxDepth;
        jvCenter: Alt := {-(1 - MaxFontDepth) / 2 - MaxDepth}
          (MaxFontDepth - MaxDepth - 1 + MaxHeight) / 2;
        jvTop: Alt := -1 + MaxHeight {-1 - MaxDepth};
      end;
    end
    else Alt := 0;
    WriteStream(Format('textX:=%.2fu', [ConvertX(Points[0].X)]));
    case HJustification of
      jhLeft: WriteLnStream('-bboxmargin;');
      jhCenter: WriteStream(';');
      jhRight: WriteLnStream('+bboxmargin;');
    end;
    {WriteLnStream(Format(
      'textY:=%.2fu+defaultscale*(fontsize defaultfont)*(%.2f);',
      [ConvertY(Points[0].Y), Alt]));}
    WriteStream(Format('textY:=%.2fu',
      [ConvertY(Points[0].Y + Obj.Height * Alt / 1.2)]));
    case VJustification of
      jvBottom {, jvCenter}: WriteLnStream('-bboxmargin;');
      jvCenter: WriteStream(';');
      jvTop: WriteLnStream('+bboxmargin;');
    end;
    case HJustification of
      jhLeft:
        case VJustification of
          jvBottom {, jvCenter}: WriteStream('label.urt');
          jvCenter: WriteStream('label.rt');
          jvTop: WriteStream('label.lrt');
        end;
      jhCenter:
        case VJustification of
          jvBottom {, jvCenter}: WriteStream('label.top');
          jvCenter: WriteStream('label');
          jvTop: WriteStream('label.bot');
        end;
      jhRight:
        case VJustification of
          jvBottom {, jvCenter}: WriteStream('label.ulft');
          jvCenter: WriteStream('label.lft');
          jvTop: WriteStream('label.llft');
        end;
    end;
    if fDrawing2D.MetaPostTeXText then
    begin
      H := Obj.Height / 1.2 * fFactorH *
        fDrawing2D.PicUnitLength * 2.845; //in pt // mm=2.845pt
      WriteStream(Format('(btex %s\strut etex scaled %.2f,', [St, H / 10]))
    end
    else WriteStream(Format('("%s",', [Text]));
    //WriteStreamPoint(Points[0]);
    WriteStream('(textX, textY)');
    WriteStream(')');
    if Obj.LineColor <> clDefault
      then WriteColor(Obj.LineColor, clBlack);
    WriteLnStream(';');
  end;
end;

procedure T_MetaPost_Export.WriteStar2D(Obj: TStar2D);
//StarsSize: TRealType;
begin
  {with Obj do
  begin
    if Obj.OwnerCAD is TDrawing2D then
      StarsSize := (Obj.OwnerCAD as TDrawing2D).StarsSize
    else StarsSize := 1;
    WriteStream(Format('fill fullcircle scaled %.1fu shifted',
      [StarsSize * fFactorH * 2]));
    WriteStreamPoint(Points[0]);
    WriteLnStream(';');
  end;}
  inherited WriteStar2D(Obj);
end;

procedure T_MetaPost_Export.WriteEllipse2D(Obj: TEllipse2D);
var
  PP: TPointsSet2D;
begin
  PP := nil;
  with Obj do
  try
    BezierPoints(PP, IdentityTransf2D);
    BeginUseProfilePoints;
    WritePath(PP, ProfilePoints, WriteBezierPath,
      Obj.LineColor, Obj.HatchColor, Obj.FillColor,
      Obj.LineKind, Obj.Hatching, IsClosed);
    EndUseProfilePoints;
  finally
    PP.Free;
  end;
end;

procedure T_MetaPost_Export.WriteCircle2D(Obj: TCircle2D);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    WritePath(Points, ProfilePoints, WriteCircle,
      Obj.LineColor, Obj.HatchColor, Obj.FillColor,
      Obj.LineKind, Obj.Hatching, False);
    EndUseProfilePoints;
  end;
end;

procedure T_MetaPost_Export.WriteCircular2D(Obj: TCircular2D);
var
  PP: TPointsSet2D;
begin
  PP := nil;
  with Obj do
  try
    BezierPoints(PP, IdentityTransf2D);
    BeginUseProfilePoints;
    WritePath(PP, ProfilePoints, WriteBezierPath,
      Obj.LineColor, Obj.HatchColor, Obj.FillColor,
      Obj.LineKind, Obj.Hatching, IsClosed);
    EndUseProfilePoints;
  finally
    PP.Free;
  end;
end;

procedure T_MetaPost_Export.WriteSpline2D(Obj: TSpline2D0);
var
  PP: TPointsSet2D;
begin
  PP := nil;
  with Obj do
  try
    BezierPoints(PP, IdentityTransf2D);
    BeginUseProfilePoints;
    WritePath(PP, ProfilePoints, WriteBezierPath,
      Obj.LineColor, Obj.HatchColor, Obj.FillColor,
      Obj.LineKind, Obj.Hatching, IsClosed);
    EndUseProfilePoints;
  finally
    PP.Free;
  end;
end;

procedure T_MetaPost_Export.WriteSmooth2D(Obj: TSmoothPath2D0);
var
  PP: TPointsSet2D;
begin
  PP := nil;
  with Obj do
  try
    BezierPoints(PP, IdentityTransf2D);
    BeginUseProfilePoints;
    WritePath(PP, ProfilePoints, WriteBezierPath,
      Obj.LineColor, Obj.HatchColor, Obj.FillColor,
      Obj.LineKind, Obj.Hatching, IsClosed);
    EndUseProfilePoints;
  finally
    PP.Free;
  end;
end;

procedure T_MetaPost_Export.WritePoly2D(Obj: TPolyline2D0);
begin
  with Obj do
    WritePath(Points, Points, WritePoly,
      Obj.LineColor, Obj.HatchColor, Obj.FillColor,
      Obj.LineKind, Obj.Hatching, IsClosed);
end;

procedure T_MetaPost_Export.WriteToTpX(Stream: TStream;
  const FileName: string);
begin
  fStream := nil;
  StoreToFile_MPS(fDrawing2D, ChangeFileExt(FileName, '.mps'));
  fStream := Stream;
  try
    WriteLnStream(Format('  \includegraphics{%s%s.mps}',
      [fDrawing2D.IncludePath,
      ChangeFileExt(ExtractFileName(FileName), '')]));
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
    {dwCreationFlags: DWORD} CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
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

function TryDeleteFile(const FileName: string): Boolean;
begin
  if FileExists(FileName)
    then Result := SysUtils.DeleteFile(PChar(FileName))
  else Result := True;
end;

function GetTempDir: string;
var
  Buffer: array[0..1023] of Char;
begin
  GetTempPath(SizeOf(Buffer) - 1, Buffer);
  SetString(Result, Buffer, StrLen(Buffer));
  //IncludeTrailingPathDelimiter(??
end;

procedure StoreToFile_MPS(const Drawing: TDrawing2D;
  const FileName: string);
var
  TempDir, TempMP, TempMPS, TempMPLog: string;
  Res: Boolean;
begin
  if not FileExists(MetaPostPath) then
  begin
    Application.MessageBox('MetaPost path not found',
      'Error', MB_OK);
    Exit;
  end;
  TempDir := GetTempDir;
  TempMP := TempDir + '(tmp)TpX.mp';
  TempMPS := ChangeFileExt(TempMP, '.0');
  TempMPLog := ChangeFileExt(TempMP, '.log');
  StoreToFile_Saver(Drawing, TempMP, T_MetaPost_Export);
  try
    //if FileExists(FileName) then DeleteFile(PChar(FileName));
    //WinExec(PChar(MetaPostPath + ' "' + TempMP + '"'), 0);
    //Res := FileExec(MetaPostPath + ' "' + TempMP + '"',      '', '', '', False, True);
    Res := FileExec(MetaPostPath + ' "(tmp)TpX.mp"', '', '',
      TempDir, False, True);
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

constructor T_PDF_Export.Create(Drawing: TDrawing2D);
begin
  inherited Create(Drawing);
  fPDF := TPdfDoc.Create;
  TextLabels := TStringList.Create;
end;

destructor T_PDF_Export.Destroy;
begin
  fPDF.Free;
  TextLabels.Free;
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

procedure T_PDF_Export.WriteLineAttr(const LineKind: TLineKind;
  const LineColor: TColor);
var
  A: TRealType;
begin
  A := fFactorMM;
  with fDrawing2D do
    with fPDF.Canvas do
    begin
      case LineKind of
        liNone, liThin, liThick: SetDash([0], 0);
        liDashed: SetPDF_Dash(fPDF.Canvas,
            [DashSize * 2 * A, DashSize * A], 0);
        //SetDash([DashSize * 2 * A,            DashSize * A], 0);
        liDotted: SetPDF_Dash(fPDF.Canvas,
            [LineWidth * 2 * A, DottedSize * A], 0);
        //SetDash([LineWidth * 2 * A,            DottedSize * A], 0);
      end;
      case LineKind of
        liNone: SetLineWidth(0);
        liThin, liDashed: SetLineWidth(LineWidth * A);
        liThick, liDotted: SetLineWidth(LineWidth * 2 * A);
      end;
      if LineColor <> clDefault
        then SetRGBStrokeColor(LineColor)
      else SetRGBStrokeColor(0);
    end;
end;

procedure T_PDF_Export.WritePoly(const PP: TPointsSet2D;
  Closed: Boolean);
var
  I: Integer;
  P: TPoint2D;
begin
  with fPDF.Canvas do
  begin
    P := ConvertPnt(PP[0]);
    MoveTo(P.X, P.Y);
    for I := 1 to PP.Count - 1 do
    begin
      P := ConvertPnt(PP[I]);
      LineTo(P.X, P.Y);
    end;
    if Closed then Closepath;
  end;
end;

procedure T_PDF_Export.WriteBezierPath(const PP: TPointsSet2D;
  Closed: Boolean);
var
  I: Integer;
  P0, P1, P2, P3: TPoint2D;
begin
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
    if Closed then Closepath;
  end;
end;

procedure T_PDF_Export.WriteHatching(const P: TPointsSet2D;
  const Hatching: THatching;
  const HatchColor: TColor; const Step: TRealType);
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
      if HatchColor <> clDefault
        then SetRGBStrokeColor(HatchColor)
      else SetRGBStrokeColor(0);
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

procedure T_PDF_Export.WritePath(const PP, HatchPP: TPointsSet2D;
  PathProc: TPathProc;
  const LineColor, HatchColor, FillColor: TColor;
  const LineKind: TLineKind; const Hatching: THatching;
  const Closed: Boolean);
begin
  if PP.Count < 1 then Exit;
  with fPDF.Canvas do
  begin
    if FillColor <> clDefault then
    begin
      SetRGBFillColor(FillColor);
      PathProc(PP, Closed);
      Eofill;
    end;
    if HatchPP <> nil then
      WriteHatching(HatchPP, Hatching, HatchColor, fHatchingStep);
    if LineKind <> liNone then
    begin
      WriteLineAttr(LineKind, LineColor);
      PathProc(PP, Closed);
      Stroke;
    end;
  end;
end;

procedure T_PDF_Export.WritePoly0(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineKind: TLineKind; const Hatching: THatching;
  const Closed: Boolean);
begin
  WritePath(PP, PP, WritePoly,
    LineColor, HatchColor, FillColor, LineKind, Hatching, Closed);
end;

procedure T_PDF_Export.WriteHeader;
var
  Dest: TPdfDestination;
begin
    //72 pixel per inch, (25.4/72 = 0.352778) mm in pixel
    // 2.8346 pixel per mm

  fFactorMM := 2.8346;
  MeasureDrawing;
  fHatchingStep := fDrawing2D.HatchingStep / fDrawing2D.PicScale;
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
  TextLabels.Clear;
end;

procedure T_PDF_Export.WriteRectangle2D(Obj: TRectangle2D);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    try
      WritePath(ProfilePoints, ProfilePoints, WritePoly,
        Obj.LineColor, Obj.HatchColor, Obj.FillColor,
        Obj.LineKind, Obj.Hatching, True);
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
    PP := nil;
    BeginUseProfilePoints;
    try
      BezierPoints(PP, IdentityTransf2D);
      WritePath(PP, ProfilePoints, WriteBezierPath,
        Obj.LineColor, Obj.HatchColor, Obj.FillColor,
        Obj.LineKind, Obj.Hatching, IsClosed);
    finally
      PP.Free;
      EndUseProfilePoints;
    end;
  end;
end;

procedure T_PDF_Export.WriteCircle2D(Obj: TCircle2D);
var
  PP: TPointsSet2D;
begin
  with Obj do
  begin
    PP := nil;
    BeginUseProfilePoints;
    try
      BezierPoints(PP, IdentityTransf2D);
      WritePath(PP, ProfilePoints, WriteBezierPath,
        Obj.LineColor, Obj.HatchColor, Obj.FillColor,
        Obj.LineKind, Obj.Hatching, IsClosed);
    finally
      PP.Free;
      EndUseProfilePoints;
    end;
  end;
end;

procedure T_PDF_Export.WriteCircular2D(Obj: TCircular2D);
var
  PP: TPointsSet2D;
begin
  with Obj do
  begin
    PP := nil;
    BeginUseProfilePoints;
    try
      BezierPoints(PP, IdentityTransf2D);
      WritePath(PP, ProfilePoints, WriteBezierPath,
        Obj.LineColor, Obj.HatchColor, Obj.FillColor,
        Obj.LineKind, Obj.Hatching, IsClosed);
    finally
      PP.Free;
      EndUseProfilePoints;
    end;
  end;
end;

procedure T_PDF_Export.WriteSpline2D(Obj: TSpline2D0);
var
  PP: TPointsSet2D;
begin
  with Obj do
  begin
    PP := nil;
    BeginUseProfilePoints;
    try
      BezierPoints(PP, IdentityTransf2D);
      WritePath(PP, ProfilePoints, WriteBezierPath,
        Obj.LineColor, Obj.HatchColor, Obj.FillColor,
        Obj.LineKind, Obj.Hatching, IsClosed);
    finally
      PP.Free;
      EndUseProfilePoints;
    end;
  end;
end;

procedure T_PDF_Export.WriteSmooth2D(Obj: TSmoothPath2D0);
var
  PP: TPointsSet2D;
begin
  with Obj do
  begin
    PP := nil;
    BeginUseProfilePoints;
    try
      BezierPoints(PP, IdentityTransf2D);
      WritePath(PP, ProfilePoints, WriteBezierPath,
        Obj.LineColor, Obj.HatchColor, Obj.FillColor,
        Obj.LineKind, Obj.Hatching, IsClosed);
    finally
      PP.Free;
      EndUseProfilePoints;
    end;
  end;
end;

procedure T_PDF_Export.WritePoly2D(Obj: TPolyline2D0);
begin
  with Obj do
    WritePath(Points, Points, WritePoly,
      Obj.LineColor, Obj.HatchColor, Obj.FillColor,
      Obj.LineKind, Obj.Hatching, IsClosed);
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
    if Obj.LineColor <> clDefault then
      SetRGBFillColor(Obj.LineColor)
    else SetRGBFillColor(clBlack);
    TextOut(ConvertX(P.X) - HShift,
      ConvertY(P.Y) - VShift, Text);
      //TextRect(ARect: TPdfRect; Text: string;                            Alignment: TPdfAlignment; Clipping: boolean);
  end;
end;

procedure T_PDF_Export.WriteStar2D(Obj: TStar2D);
  //CP: TPoint2D;  R: TRealType;
begin
  {CP := ConvertPnt(Obj.Points[0]);
  R := fDrawing2D.StarsSize * fFactorW;
  with fPDF.Canvas do
  begin
    Ellipse(CP.X - R, CP.Y - R, 2 * R, 2 * R);
    if Obj.LineColor <> clDefault then
      SetRGBFillColor(Obj.LineColor)
    else SetRGBFillColor(0);
    Eofill;
  end;}
  inherited WriteStar2D(Obj);
end;

procedure T_PDF_Export.WriteAllToStream;
begin
  if fStream = nil then Exit;
  WriteAll;
  fPDF.SaveToStream(fStream);
end;

procedure T_PDF_Export.WriteToTpX(Stream: TStream;
  const FileName: string);
(*begin
  fStream := nil;
  StoreToFile(ChangeFileExt(FileName, '.pdf'));
  fStream := Stream;
  try
    WriteLnStream(Format(
      '  \includegraphics{%s%s.pdf}', //[width=%.1fcm,height=%.1fcm]
      [//fDrawing2D.PicWidth / 10, fDrawing2D.PicHeight / 10,
      fDrawing2D.IncludePath,
        ChangeFileExt(ExtractFileName(FileName), '')]));
  finally
    fStream := nil;
  end;
end;*)
var
  H, W, UnitLength: TRealType;
  I: Integer;
begin
  fStream := nil;
  StoreToFile(ChangeFileExt(FileName, '.pdf'));
  fStream := Stream;
  UnitLength := fDrawing2D.PicUnitLength;
  W := fW_MM / UnitLength;
  H := fH_MM / UnitLength;
  try
    WriteLnStream(Format(
      '  \setlength{\unitlength}{%.4g mm}', [UnitLength]));
    WriteLnStream(Format('  \begin{picture}(%.1f, %.1f)(0,0)', [W, H]));
    WriteLnStream(Format(
      '  \put(0,0){\includegraphics{%s%s.pdf}}', //[width=%.1fcm,height=%.1fcm]
      [//fDrawing2D.PicWidth / 10, fDrawing2D.PicHeight / 10,
      fDrawing2D.IncludePath,
        ChangeFileExt(ExtractFileName(FileName), '')]));
    for I := 0 to TextLabels.Count - 1 do
      WriteLnStream('  ' + TextLabels[I]);
    WriteLnStream('  \end{picture}');
  finally
    fStream := nil;
  end;
end;

{ --================ T_PDF_Light_Export ==================-- }

procedure T_PDF_Light_Export.WriteText2D(Obj: TText2D);
var
  TempFW, TempFH: TRealType;
begin
  TempFW := fFactorW;
  TempFH := fFactorH;
  fFactorW := fFactorW / fDrawing2D.PicUnitLength / 2.8346;
  fFactorH := fFactorH / fDrawing2D.PicUnitLength / 2.8346;
  TextLabels.Add(GetTeXText(Obj, fDrawing2D.PicUnitLength));
  fFactorW := TempFW;
  fFactorH := TempFH;
end;

{ --================ T_Bitmap0_Export ==================-- }

constructor T_Bitmap0_Export.Create(Drawing: TDrawing2D);
begin
  inherited Create(Drawing);
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

procedure T_Bitmap0_Export.WriteLine(P0, P1: TPoint2D;
  W: TRealType; Color: TColor);
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
  WriteBitmapPolygon(fOutline, Color);
end;

procedure T_Bitmap0_Export.WriteBitmapPolygon(Polygon:
  TPolygon32; Color: TColor);
begin
  Polygon.Antialiased := True; //False;
  Polygon.DrawFill(fBitmap, Color32(Color))
end;

procedure T_Bitmap0_Export.FillPolygon(PP: TPointsSet2D;
  Closed: Boolean);
var
  I, N1: Integer;
  P: TPoint2D;
begin
  with fDrawing2D, fBitmap do
  begin
    fPolygon.Clear;
    fPolygon.Closed := Closed;
    N1 := PP.Count - 1;
    if IsSamePoint2D(PP[0], PP[N1]) then Dec(N1);
    for I := 0 to N1 do
    begin
      P := ConvertPnt(PP[I]);
      fPolygon.Add(FixedPoint(P.X, P.Y));
    end;
  end;
end;

procedure T_Bitmap0_Export.WriteHatching(const P: TPointsSet2D;
  const Hatching: THatching;
  const HatchColor: TColor; const Step: TRealType);
var
  Lines: TPointsSet2D;
  DX, DY: TRealType;
  Color: TColor;
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
        if HatchColor = clDefault then Color := clBlack
        else Color := HatchColor;
        WriteLine(Lines[I * 2], Lines[I * 2 + 1],
          LineWidth / 2 * fFactorMM, Color);
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

procedure T_Bitmap0_Export.WritePoly0(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineKind: TLineKind; const Hatching: THatching;
  const Closed: Boolean);
var
  W: TRealType;
  TmpPoly: TPolygon32;
begin
  if PP.Count < 1 then Exit;
  {if (Obj.LineKind <> liNone) or (Obj.FillColor <> clDefault)
    then FillPolygon(PP, Closed);}
  if FillColor <> clDefault then
  begin
    FillPolygon(PP, Closed);
    WriteBitmapPolygon(fPolygon, FillColor);
  end;
  if Hatching <> haNone then
    WriteHatching(PP, Hatching, HatchColor, fHatchingStep);
  if LineKind = liNone then Exit;
  FillPolygon(PP, Closed);
  with fDrawing2D, fBitmap do
  begin
    case LineKind of
      liThin, liThick: TmpPoly := fPolygon.Outline;
      liDashed:
        TmpPoly := DashedOutline(DashSize * 2 * fFactorMM,
          DashSize * fFactorMM, fPolygon);
      liDotted:
        TmpPoly := DashedOutline(LineWidth * 2 * fFactorMM,
          DottedSize * fFactorMM, fPolygon)
    end;
    case LineKind of
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
  WriteBitmapPolygon(fOutline, LineColor);
end;

procedure T_Bitmap0_Export.WriteHeader;
begin

  fFactorMM := 0.5 / fDrawing2D.PicUnitLength;
  MeasureDrawing;
  fHatchingStep := fDrawing2D.HatchingStep / fDrawing2D.PicScale;
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

procedure T_Bitmap0_Export.WriteRectangle2D(Obj: TRectangle2D);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    try
      WritePoly0(ProfilePoints,
        Obj.LineColor, Obj.HatchColor, Obj.FillColor,
        Obj.LineKind, Obj.Hatching, True);
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
    WritePoly0(ProfilePoints,
      Obj.LineColor, Obj.HatchColor, Obj.FillColor,
      Obj.LineKind, Obj.Hatching, IsClosed);
    EndUseProfilePoints;
  end;
end;

procedure T_Bitmap0_Export.WriteCircle2D(Obj: TCircle2D);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    WritePoly0(ProfilePoints,
      Obj.LineColor, Obj.HatchColor, Obj.FillColor,
      Obj.LineKind, Obj.Hatching, IsClosed);
    EndUseProfilePoints;
  end;
end;

procedure T_Bitmap0_Export.WriteCircular2D(Obj: TCircular2D);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    WritePoly0(ProfilePoints,
      Obj.LineColor, Obj.HatchColor, Obj.FillColor,
      Obj.LineKind, Obj.Hatching, IsClosed);
    EndUseProfilePoints;
  end;
end;

procedure T_Bitmap0_Export.WriteSpline2D(Obj: TSpline2D0);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    WritePoly0(ProfilePoints,
      Obj.LineColor, Obj.HatchColor, Obj.FillColor,
      Obj.LineKind, Obj.Hatching, IsClosed);
    EndUseProfilePoints;
  end;
end;

procedure T_Bitmap0_Export.WriteSmooth2D(Obj: TSmoothPath2D0);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    WritePoly0(ProfilePoints,
      Obj.LineColor, Obj.HatchColor, Obj.FillColor,
      Obj.LineKind, Obj.Hatching, IsClosed);
    EndUseProfilePoints;
  end;
end;

procedure T_Bitmap0_Export.WritePoly2D(Obj: TPolyline2D0);
begin
  with Obj do
    WritePoly0(Obj.Points,
      Obj.LineColor, Obj.HatchColor, Obj.FillColor,
      Obj.LineKind, Obj.Hatching, Obj.IsClosed);
end;

procedure T_Bitmap0_Export.WriteText2D(Obj: TText2D);
var
  FontH, HShift, VShift: TRealType;
  P: TPoint2D;
begin
  with Obj do
  begin
    fBitmap.PenColor := clBlack32;
    if Obj.LineColor <> clDefault then
      fBitmap.Font.Color := Obj.LineColor
    else
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
      '  \includegraphics[width=%.3fcm,height=%.3fcm]{%s%s.bmp}',
      [fW_MM / 10, fH_MM / 10, fDrawing2D.IncludePath,
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
//      '  \includegraphics[bb=%d %d %d %d,width=%.3fcm,height=%.3fcm]{%s.png}',
//      [0, 0, Ceil(fW_MM * 2.8346), Ceil(fH_MM * 2.8346),fW_MM / 10, fH_MM / 10,
//      ChangeFileExt(ExtractFileName(FileName), '')]));
      '  \includegraphics[width=%.3fcm,height=%.3fcm]{%s%s.png}',
      [fW_MM / 10, fH_MM / 10, fDrawing2D.IncludePath,
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
  //fDrawing2D.SaveToFile_EMF(ChangeFileExt(FileName, '.emf'), 1);
  fDrawing2D.SaveToFile_EMF(ChangeFileExt(FileName, '.emf'));
  fStream := Stream;
  try
  //fFactorMM := 0.5 / fDrawing2D.PicUnitLength;
    fFactorMM := 1;
    MeasureDrawing;
    WriteLnStream(Format(
      '  \includegraphics[width=%.3fcm,height=%.3fcm]{%s%s}',
      [fW_MM / 10, fH_MM / 10, fDrawing2D.IncludePath,
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

procedure StoreToFile_EpsToPdf(const Drawing: TDrawing2D;
  const FileName: string; Light: Boolean);
var
  TempDir, TempEPS, TempPDF: string;
  Res: Boolean;
begin
  if not FileExists(EpsToPdfPath) then
  begin
    Application.MessageBox('EpsToPdf path not found',
      'Error', MB_OK);
    Exit;
  end;
  TempDir := GetTempDir;
  TempEPS := TempDir + '(tmp)TpX.eps';
  TempPDF := ChangeFileExt(TempEPS, '.pdf');
  if not Light then
    StoreToFile_Saver(Drawing, TempEPS, T_PostScript_Export)
  else StoreToFile_Saver(Drawing, TempEPS, T_PostScript_Light_Export);
  try
  //EpsToPdfPath: string = 'epstopdf.exe';
    Res := FileExec(Format('"%s" "%s"',
      [EpsToPdfPath, TempEPS]), '', '',
      TempDir, False, True);
    if FileExists(TempPDF) then
    begin
      if TempPDF <> FileName then
      begin
        TryDeleteFile(FileName);
        if not RenameFile(TempPDF, FileName) then
          Application.MessageBox('Can not rename. PDF file not created',
            'Error', MB_OK);
      end;
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
    ChangeFileExt(FileName, '.pdf'), Self is T_EpsToPdf_Light_Export);
  fStream := Stream;
  try
    WriteLnStream(Format('  \includegraphics{%s%s}',
      [fDrawing2D.IncludePath,
      ChangeFileExt(ExtractFileName(FileName), '.pdf')]));
  finally
    fStream := nil;
  end;
end;

procedure T_EpsToPdf_Export.StoreToFile(const FileName: string);
begin
  StoreToFile_EpsToPdf(fDrawing2D,
    ChangeFileExt(FileName, '.pdf'), Self is T_EpsToPdf_Light_Export);
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
  if XMLNode.AttributeNode['lc'] <> nil then
    Obj.LineColor := HtmlToColor(XMLNode.AttributeValue['lc'])
  else Obj.LineColor := clDefault;
  if XMLNode.AttributeNode['hc'] <> nil then
    Obj.HatchColor := HtmlToColor(XMLNode.AttributeValue['hc'])
  else Obj.HatchColor := clDefault;
  if XMLNode.AttributeNode['fill'] <> nil then
    Obj.FillColor := HtmlToColor(XMLNode.AttributeValue['fill'])
  else Obj.FillColor := clDefault;
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
  H := XMLNode.AttributeValue['h'];
  X := XMLNode.AttributeValue['x'];
  Y := XMLNode.AttributeValue['y'];
  Result := TText2D.Create(0,
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
    end;
    if XMLNode.AttributeNode['tex'] <> nil then
      TeXText := XMLNode.AttributeValueSt['tex']
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
  StarID: string;
  I: Integer;
begin
  CP := Point2D(XMLNode.AttributeValue['x'],
    XMLNode.AttributeValue['y']);
  Result := TStar2D.Create(0, CP);
  (Result as TStar2D).StarKind := starCircle;
  if XMLNode.AttributeNode['s'] <> nil then
  begin
    StarID := XMLNode.AttributeValue['s'];
    for I := 1 to High(StarsIDs) do
      if StarID = StarsIDs[I] then
      begin
        (Result as TStar2D).StarKind := TStarKind(I);
        Break;
      end;
  end;
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

constructor T_TpX_Loader.Create(Drawing: TDrawing2D);
begin
  inherited Create;
  fDrawing2D := Drawing;
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
      if AttributeNode['PicWidth'] <> nil then
        PicWidth := AttributeValue['PicWidth']
      else PicWidth := 0;
      if AttributeNode['PicHeight'] <> nil then
        PicHeight := AttributeValue['PicHeight']
      else PicHeight := 0;
      if AttributeNode['PicScale'] <> nil then
        fDrawing2D.PicScale := AttributeValue['PicScale']
      else fDrawing2D.PicScale := 0;
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
    fDrawing2D.Caption := Child.Text;
    fDrawing2D.FigLabel
      := (Child as TXMLDElement).AttributeValueSt['label'];
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
      begin
        MainForm.ProgressBar1.Position :=
          Round(I / fXML.DocumentElement.ChildNodes.Count * 100);
        Application.ProcessMessages;
      end;
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

procedure Import_EMF(Drawing: TDrawing2D; const EmfFileName: string;
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
      EMF_Loader.LoadFromFile(EmfFileName);
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
      Comment := Format('Imported from %s',
        [ExtractFileName(EmfFileName)]);
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

procedure Import_Eps(Drawing: TDrawing2D; const EpsFileName: string);
var
  TempDir, TempEMF: string;
  Res: Boolean;
begin
  if not FileExists(PsToEditPath) then
  begin
    Application.MessageBox('PsToEdit path not found',
      'Error', MB_OK);
    Exit;
  end;
  TempDir := GetTempDir;
  TempEMF := TempDir + '(tmp)TpX.emf';
  try
    Res := FileExec(Format('"%s" "%s" "%s" -f emf',
      [PsToEditPath, EpsFileName, TempEMF]), '', '',
      TempDir, False, True);
    if not FileExists(TempEMF) then
      Application.MessageBox('EMF file not created',
        'Error', MB_OK)
    else
    begin
      Import_EMF(Drawing, TempEMF, nil);
      Drawing.Comment := Format('Imported from %s',
        [ExtractFileName(EpsFileName)]);
    end;
  finally
    TryDeleteFile(TempEMF);
  end;
end;

procedure ParseParamStr(var FileName, IncludePath: string);
var
  I: Integer;
  LineNumber: Integer;
  St: string;
begin
  LineNumber := 1;
  FileName := '';
  IncludePath := '';
  //if ParamCount > 0 then ShowMessage(CmdLine);
  for I := 1 to ParamCount do
  begin
    St := LowerCase(ParamStr(I));
    if Pos('-f', St) = 1 then
    begin
      Delete(St, 1, 2);
      FileName := ExpandFileName(AnsiDequotedStr(St, '"'));
      //ShowMessage(FileName);
      Exit;
    end;
    if Pos('-l', St) = 1 then
    begin
      Delete(St, 1, 2);
      try
        LineNumber := StrToInt(St);
      except
        ShowMessage(ParamStr(I));
      end
    end;
    if Pos('-i', St) = 1 then
    begin
      Delete(St, 1, 2);
      FileName := AnsiDequotedStr(St, '"');
    end;
  end;
  if FileName <> '' then
    FindPicturePath(FileName, LineNumber, FileName, IncludePath);
end;

procedure FindPicturePath(const TeXFileName: string;
  const Line: Integer;
  var PicturePath, IncludePath: string);
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
    IncludePath :=
      StringReplace(ExtractFilePath(St), '\', '/', [rfReplaceAll]);
    if ExtractFileDrive(St) = '' then
      PicturePath := ExtractFilePath(TeXFileName) + St
    else PicturePath := St;
    Result := True;
  end;
begin
  PicturePath := '';
  IncludePath := '';
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


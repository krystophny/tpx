unit InOut;

interface

uses Types, SysUtils, Classes, Windows, Graphics, ComCtrls,
  Variants, CADSys4, CS4BaseTypes, CS4Shapes,
  XUtils, XXmlDom, PdfDoc, Gr32, Gr32_Polygons, Geometry;

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
      const LineStyle: TLineStyle; const LineWidth: TRealType; const Hatching:
      THatching;
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
    procedure WriteBezier2D(Obj: TBezierPath2D0); virtual;
      abstract;
    procedure WriteEntity(Obj: TObject2D);
    function GetPathString(PP: TPointsSet2D): string;
    function GetTeXText(Obj: TText2D;
      UnitLength: TRealType): string;
    function GetFontDescent: TRealType; virtual;
  public
    constructor Create(Drawing: TDrawing2D); virtual;
    procedure WriteEntities;
    procedure WriteHeader; virtual;
    procedure WriteFooter; virtual;
    procedure WriteAll0;
    procedure WriteAll; virtual;
    procedure WriteAllToStream; virtual;
    procedure WriteToTpX(Stream: TStream;
      const FileName: string); virtual;
    function StoreToFile(const FileName: string): Boolean; virtual;
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
    procedure WriteBezier2D(Obj: TBezierPath2D0); override;
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
    FFontName: string;
    fDescent: TRealType;
  protected
    function ConvertY(Y: TRealType): TRealType; override;
    function RegisterPatt(Hatching: THatching;
      HatchColor, FillColor: TColor): string;
    procedure WritePrimitiveAttr0(const LineColor,
      HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching;
      MiterLimit: TRealType; XMLNode: TXMLDElement);
    procedure WritePrimitiveAttr(Obj: TPrimitive2D;
      XMLNode: TXMLDElement);
    procedure WritePoly0(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType; const Hatching:
      THatching;
      const Closed: Boolean); override;
    procedure WritePoly(PP: TPointsSet2D;
      Obj: TPrimitive2D; Closed: Boolean);
    procedure WriteBezier(PP: TPointsSet2D;
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
    procedure WriteBezier2D(Obj: TBezierPath2D0); override;
    function GetFontDescent: TRealType; override;
  public
    constructor Create(Drawing: TDrawing2D); override;
    destructor Destroy; override;
    procedure WriteHeader; override;
    function GetX(X: TRealType): Integer;
    function GetY(Y: TRealType): Integer;
    procedure WriteAll; override;
    procedure WriteAllToStream; override;
  end;

  T_PostScript_Export = class(T_CAD_Saver)
  private
    fHatchingStep: TRealType;
    fDescent: TRealType;
  protected
    TextLabels: TStringList;
    procedure WriteStreamPoint0(const X, Y: TRealType); override;
    procedure WriteColor(Color: TColor);
    procedure WriteLineAttr(const LineStyle: TLineStyle; const LineWidth:
      TRealType; const LineColor: TColor);
    procedure WriteFill0(const FillColor: TColor; const LineStyle: TLineStyle);
    procedure WriteFill(Obj: TPrimitive2D);
    procedure WriteStroke(const LineStyle: TLineStyle; const LineWidth:
      TRealType; const LineColor: TColor);
    procedure WritePoly00(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching;
      const Closed: Boolean);
    procedure WritePoly0(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType; const Hatching:
      THatching;
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
    procedure WriteBezier2D(Obj: TBezierPath2D0); override;
  public
    FontName: string;
    constructor Create(Drawing: TDrawing2D); override;
    destructor Destroy; override;
    procedure WriteHeader; override;
    procedure WriteFooter; override;
    procedure WriteToTpX(Stream: TStream;
      const FileName: string); override;
  end;

  // Export without text and font
  T_PostScript_Light_Export = class(T_PostScript_Export)
  protected
    procedure WriteFont; override;
    procedure WriteText2D(Obj: TText2D); override;
  end;

  T_EpsToPdf_Export = class(T_PostScript_Export)
  public
    function StoreToFile(const FileName: string): Boolean; override;
  end;

  // uses "light" Postscript (without text):
  T_EpsToPdf_Light_Export = class(T_EpsToPdf_Export)
  protected
    procedure WriteFont; override;
    procedure WriteText2D(Obj: TText2D); override;
  end;

  T_TeX_Picture_Export = class(T_CAD_Saver)
  private
    fH, fW, fUnitLength, fHatchingStep: TRealType;
  protected
    procedure WriteStreamPoint0(const X, Y: TRealType); override;
    procedure WriteLineThickness0(const LineWidth: TRealType);
    procedure WriteLineThickness(Obj: TPrimitive2D);
    procedure WritePoly0(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType; const Hatching:
      THatching;
      const Closed: Boolean); override;
    procedure WriteLine(P0, P1: TPoint2D; const LineStyle: TLineStyle;
      const LineWidth: TRealType);
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
    procedure WriteBezier2D(Obj: TBezierPath2D0); override;
  public
    constructor Create(Drawing: TDrawing2D); override;
    procedure WriteHeader; override;
    procedure WriteFooter; override;
  end;

  TPathProcAttr = procedure(const PP: TPointsSet2D;
    const Attr: string; const LineWidth: TRealType;
    Closed: Boolean) of object;

  T_PSTricks_Export = class(T_CAD_Saver)
  private
    fH, fW, fUnitLength, fHatchingStep: TRealType;
    Colors: TStringList;
  protected
    procedure WriteStreamPoint0(const X, Y: TRealType); override;
    function WriteNewColor(Color: TColor): string;
    function LineArg(const LineStyle: TLineStyle; const LineWidth: TRealType;
      Color: string): string;
    procedure WriteLine(P0, P1: TPoint2D; Obj: TPrimitive2D);
    procedure WritePoly(const PP: TPointsSet2D;
      const Attr: string; const LineWidth: TRealType; Closed: Boolean);
    procedure WriteHatching(const P: TPointsSet2D;
      const Hatching: THatching;
      const HatchColor: TColor; Step: TRealType);
    procedure WritePath(const PP, HatchPP: TPointsSet2D;
      PathProc: TPathProcAttr;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType; const Hatching:
      THatching;
      const Closed: Boolean);
    procedure WritePoly0(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType; const Hatching:
      THatching;
      const Closed: Boolean); override;
    {procedure WriteBezier(const P0, P1, P2, P3: TPoint2D;
      const Attr: string);}
    procedure WriteBezierPath(const PP: TPointsSet2D;
      const Attr: string; const LineWidth: TRealType; Closed: Boolean);
    procedure WriteCircle(const PP: TPointsSet2D;
      const Attr: string; const LineWidth: TRealType; Closed: Boolean);
    procedure WriteCircular0(const PP: TPointsSet2D;
      const Attr: string; const LineWidth: TRealType; ObjClass: TClass);
    procedure WriteArc(const PP: TPointsSet2D;
      const Attr: string; const LineWidth: TRealType; Closed: Boolean);
    procedure WriteSector(const PP: TPointsSet2D;
      const Attr: string; const LineWidth: TRealType; Closed: Boolean);
    procedure WriteSegment(const PP: TPointsSet2D;
      const Attr: string; const LineWidth: TRealType; Closed: Boolean);
    procedure WriteRectangle2D(Obj: TRectangle2D); override;
    procedure WriteEllipse2D(Obj: TEllipse2D); override;
    procedure WriteCircle2D(Obj: TCircle2D); override;
    procedure WriteCircular2D(Obj: TCircular2D); override;
    procedure WriteSpline2D(Obj: TSpline2D0); override;
    procedure WritePoly2D(Obj: TPolyline2D0); override;
    procedure WriteText2D(Obj: TText2D); override;
    procedure WriteStar2D(Obj: TStar2D); override;
    procedure WriteSmooth2D(Obj: TSmoothPath2D0); override;
    procedure WriteBezier2D(Obj: TBezierPath2D0); override;
  public
    constructor Create(Drawing: TDrawing2D); override;
    destructor Destroy; override;
    procedure WriteHeader; override;
    procedure WriteFooter; override;
  end;

  TPathKind = (path_Fill, path_Stroke, path_FillStroke);

  TPathProcPathKind = procedure(const PP: TPointsSet2D;
    const PathKind: TPathKind; Closed: Boolean) of object;

  T_PGF_Export = class(T_CAD_Saver)
  private
    fUnitLength, fHatchingStep: TRealType;
    CurrColor: TColor;
    CurrLineWidth: TRealType;
    CurrLineStyle: TLineStyle;
  protected
    procedure WriteStreamPoint0(const X, Y: TRealType); override;
    function GetColor(Color: TColor): string;
    procedure WriteColor(Color: TColor);
    procedure WriteDash(LineStyle: TLineStyle);
    procedure WriteLineWidth(W: TRealType);
    procedure WriteLineThickness0(const LineStyle: TLineStyle;
      const LineWidth: TRealType);
    procedure WriteLineThickness(Obj: TPrimitive2D);
    procedure WritePoly(const PP: TPointsSet2D;
      const PathKind: TPathKind; Closed: Boolean);
    procedure WriteHatching(const P: TPointsSet2D;
      const Hatching: THatching;
      const HatchColor: TColor; Step: TRealType);
    procedure WritePathKind(const PathKind: TPathKind);
    function GetPathKindString(const PathKind: TPathKind): string;
    procedure WritePath(const PP, HatchPP: TPointsSet2D;
      PathProc: TPathProcPathKind;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType; const Hatching:
      THatching;
      const Closed: Boolean);
    procedure WritePoly0(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType; const Hatching:
      THatching;
      const Closed: Boolean); override;
    procedure WriteBezierPath(const PP: TPointsSet2D;
      const PathKind: TPathKind; Closed: Boolean);
    procedure WriteCircle(const PP: TPointsSet2D;
      const PathKind: TPathKind; Closed: Boolean);
    procedure WriteEllipse(const PP: TPointsSet2D;
      const PathKind: TPathKind; Closed: Boolean);
    procedure WriteRectangle2D(Obj: TRectangle2D); override;
    procedure WriteEllipse2D(Obj: TEllipse2D); override;
    procedure WriteCircle2D(Obj: TCircle2D); override;
    procedure WriteCircular2D(Obj: TCircular2D); override;
    procedure WriteSpline2D(Obj: TSpline2D0); override;
    procedure WritePoly2D(Obj: TPolyline2D0); override;
    procedure WriteText2D(Obj: TText2D); override;
    procedure WriteStar2D(Obj: TStar2D); override;
    procedure WriteSmooth2D(Obj: TSmoothPath2D0); override;
    procedure WriteBezier2D(Obj: TBezierPath2D0); override;
  public
    procedure WriteHeader; override;
    procedure WriteFooter; override;
  end;

  TPathProc = procedure(const PP: TPointsSet2D; Closed: Boolean) of object;

  T_MetaPost_Export = class(T_CAD_Saver)
    fHatchingStep: TRealType;
  protected
    procedure WriteStreamPoint0(const X, Y: TRealType); override;
    procedure WriteColor(const Color, DefaultColor: TColor);
    procedure WriteLineAttr(const LineStyle: TLineStyle; const LineWidth:
      TRealType;
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
      const LineStyle: TLineStyle; const LineWidth: TRealType; const Hatching:
      THatching;
      const Closed: Boolean);
    procedure WritePoly0(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType; const Hatching:
      THatching;
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
    procedure WriteBezier2D(Obj: TBezierPath2D0); override;
  public
    procedure WriteHeader; override;
    procedure WriteFooter; override;
    procedure WriteToTpX(Stream: TStream;
      const FileName: string); override;
  end;

  T_PDF_Export = class(T_CAD_Saver)
  private
    fPDF: TPdfDoc;
    fHatchingStep: TRealType;
    procedure WriteLineAttr(const LineStyle: TLineStyle; const LineWidth:
      TRealType;
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
      const LineStyle: TLineStyle; const LineWidth: TRealType; const Hatching:
      THatching;
      const Closed: Boolean);
  protected
    TextLabels: TStringList;
    procedure WritePoly0(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType; const Hatching:
      THatching;
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
    procedure WriteBezier2D(Obj: TBezierPath2D0); override;
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
      const LineStyle: TLineStyle; const LineWidth: TRealType; const Hatching:
      THatching;
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
    procedure WriteBezier2D(Obj: TBezierPath2D0); override;
  public
    constructor Create(Drawing: TDrawing2D); override;
    destructor Destroy; override;
    procedure WriteHeader; override;
    procedure WriteAll; override;
    procedure WriteAllToStream; override;
    function StoreToFile(const FileName: string): Boolean; override;
  end;

  T_Bitmap_Export = class(T_Bitmap0_Export)
    procedure WriteToTpX(Stream: TStream;
      const FileName: string); override;
  end;

  T_PNG_Export = class(T_Bitmap0_Export)
  public
    procedure WriteToTpX(Stream: TStream;
      const FileName: string); override;
    function StoreToFile(const FileName: string): Boolean; override;
  end;

  T_EMF_Export = class(T_CAD_Saver)
  public
    procedure WriteToTpX(Stream: TStream;
      const FileName: string); override;
    function StoreToFile(const FileName: string): Boolean; override;
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
    function ReadBezier(XMLNode: TXMLDElement): TPrimitive2D;
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
function StoreToFile_MPS(const Drawing: TDrawing2D;
  const FileName: string): Boolean;
function Run_EpsToPdf(const EpsFileName, PdfFileName: string): Boolean;
procedure ParseParamStr(var FileName, IncludePath: string);
procedure FindPicturePath(const TeXFileName: string; const Line: Integer;
  var PicturePath, IncludePath: string);
procedure pfb2pfa(const FileName_pfb, FileName_pfa: string;
  var Descent: TRealType);
procedure Parse_PL(const FileName: string; var Heights, Depths: T_FM_Arr);
function FileExec(const aCmdLine, InFile, OutFile, Directory:
  string; aHide, aWait: Boolean): Boolean;
procedure OpenOrExec(const ViewerPath, FileName: string);
function TryDeleteFile(const FileName: string): Boolean;
function GetTempDir: string;
function XMLNodeText(Node: TXMLDNode; Level: Integer): string;
procedure Import_Metafile(const Drawing: TDrawing2D;
  const MF_FileName: string;
  Lines: TStrings);
procedure Import_MetafileFromStream(const Drawing: TDrawing2D;
  const Stream: TStream; const IsOld: Boolean);
procedure Import_MetafileFromClipboard(const Drawing: TDrawing2D);
procedure PasteMetafileFromClipboard(const Drawing: TDrawing2D);
procedure Import_Eps(const Drawing: TDrawing2D;
  const EpsFileName: string);

var
  MetaPostPath: string = 'mpost.exe';
  Font_pfb_Path: string = '';
  EpsToPdfPath: string = 'epstopdf.exe';
  PsToEditPath: string = 'pstoedit.exe';
  PsToEditFormat: string = 'emf';

implementation

uses Math, Dialogs, Forms, Clipbrd, StrUtils, MainUnit,
  PdfTypes, pngimage, ColorEtc, EMF, PreView, ShellAPI;

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
  B, S: TRealType;
begin
  B := fDrawing2D.Border;
  ARect := fDrawing2D.DrawingExtension;
  with ARect do
  begin
    S := fDrawing2D.PicScale;
    if S <= 0 then S := 1;
    fFactorW := S * fFactorMM;
    fFactorH := S * fFactorMM;
    fExtLeft := Left - B / S;
    fExtBottom := Bottom - B / S;
    fExtTop := Top + B / S;
    fW_MM := (Right - Left) * S + B * 2;
    fH_MM := (Top - Bottom) * S + B * 2;
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
  const LineStyle: TLineStyle; const LineWidth: TRealType; const Hatching:
  THatching;
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
    Piece.GetFillColor(Obj), Piece.GetLineStyle(Obj),
    Piece.GetLineWidth(Obj), Piece.GetHatching(Obj),
    IsClosed);
end;

procedure T_CAD_Saver.WritePieces(Obj: TPrimitive2D);
var
  I: Integer;
begin
  if Obj.Pieces = nil then Exit;
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
  else if Obj is TBezierPath2D0 then
  begin
    WriteBezier2D(Obj as TBezierPath2D0);
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
  Result := AnsiReplaceStr(Result, ' ', '~');
end;

function GetTeXTextFontSize(Obj: TText2D;
  FactorH, UnitLength: TRealType): string;
var
  H: TRealType;
begin
  with Obj do
  begin
    H := Height * FactorH * UnitLength * 2.845; //in pt // mm=2.845pt
    // TeX uses small points (1 inch = 72.27pt),
    // not big points (1 inch = 72bp) as Adobe PS/PDF/SVG
    Result := Format('\fontsize{%.2f}{%.2f}\selectfont ',
      [H, H * 1.2]);
  end;
end;

function GetTeXTextFontColor(Obj: TText2D): string;
var
  RGB: T_PS_RGB;
begin
  with Obj do
  begin
    if Obj.LineColor <> clDefault then
    begin
      RGB := PS_RGB(Obj.LineColor);
      Result := Result + Format('\textcolor[rgb]{%.5g, %.5g, %.5g}',
        [RGB.R, RGB.G, RGB.B]);
    end;
  end;
end;

function GetTeXTextMakebox(Obj: TText2D;
  FactorW, FactorH, UnitLength: TRealType): string;
var
  Rect: TRect2D;
  Width: TRealType;
  St: string;
begin
  with Obj do
  begin
    Rect := GetExtension0;
    Width := (Rect.Right - Rect.Left);
    Result := GetTeXTextFontSize(Obj, FactorH, UnitLength);
    Result := Result + GetTeXTextFontColor(Obj);
    if Obj.LineColor <> clDefault then Result := Result + '{';
    Result := Result +
      Format('\makebox(%.1f, %.1f)[', [Width * FactorW, Height * FactorH]);
    case HJustification of
      jhLeft: Result := Result + 'l';
      jhCenter: Result := Result + 'c';
      jhRight: Result := Result + 'r';
    end;
    case VJustification of
      jvBottom: Result := Result + 'b';
      jvCenter: Result := Result + 'c';
      jvTop: Result := Result + 't';
    end;
    if TeXText <> '' then St := TeXText
    else St := TeX_Replace_Special(Text);
    Result := Result + ']{' + St + {} '\strut}';
    if Obj.LineColor <> clDefault then Result := Result + '}';
    if Rot = 0 then
    else
    begin
      Result :=
        Format('\rotatebox{%.2f}{%s}', //\frame{} \fbox{}
        [RadToDeg(Rot), Result]);
    end;
  end;
end;

procedure GetTeXTextPoint(var P: TPoint2D; Obj: TText2D;
  FactorW, FactorH: TRealType);
var
  Rect: TRect2D;
  D: TVector2D;
  BL: TPoint2D;
  T: TTransf2D;
begin
  //Exit;
  with Obj do
  begin
    Rect := GetExtension0;
    BL := Rect.FirstEdge;
    //Windows and TeX fonts are vertically aligned in different ways!
    case VJustification of
      jvBottom: BL := Point2D(BL.X, BL.Y + Obj.Height * (-0.1));
      jvCenter: BL := Point2D(BL.X, BL.Y + Obj.Height * (-0.0));
      jvTop: BL := Point2D(BL.X, BL.Y + Obj.Height * (+0.1));
      jvBaseline: ;
    end;
    BL := Point2D(BL.X, BL.Y + Obj.Height * (-0.03));
    T := RotateCenter2D(Rot, Points[0]);
    BL := TransformPoint2D(BL, T);
    Rect := TransformBoundingBox2D(Rect, T);
    D := Vector2D(Points[0], Point2D(Rect.Left, BL.Y));
    D := TransformVector2D(D, Scale2D(FactorW, FactorH));
    P := ShiftPoint(P, D);
  end;
end;

function T_CAD_Saver.GetTeXText(Obj: TText2D;
  UnitLength: TRealType): string;
var
  P: TPoint2D;
begin
  with Obj do
  begin
    P := ConvertPnt(Points[0]);
    GetTeXTextPoint(P, Obj, fFactorW, fFactorH);
    Result := Format('\put(%.1f, %.1f){%s}', [P.X, P.Y,
      GetTeXTextMakebox(Obj, fFactorW, fFactorH, UnitLength)]);
    //Result := Result + EOL + Format('\put(%.1f, %.1f){\circle{14}}',      [P.X, P.Y]);
    //P := ConvertPnt(Points[0]);
    //Result := Result + EOL + Format('\put(%.1f, %.1f){\circle{24}}',      [P.X, P.Y]);
  end;
end;

function T_CAD_Saver.GetFontDescent: TRealType;
begin
  Result := 0.2;
end;

procedure T_CAD_Saver.WriteHeader;
begin

end;

procedure T_CAD_Saver.WriteFooter;
begin

end;

procedure T_CAD_Saver.WriteAll0;
begin
  if fDrawing2D.PicScale <= 0
    then fDrawing2D.PicScale := 1;
  WriteHeader;
  WriteEntities;
  WriteFooter;
end;

procedure T_CAD_Saver.WriteAll;
var
  Magnif, PicScale0, Border0, LineWidth0, HatchingStep0,
    DottedSize0, DashSize0: TRealType;
begin
  if fDrawing2D = nil then Exit;
  //Changing meaning of millimeters for output picture
  Magnif := fDrawing2D.PicMagnif;
  if Magnif <> 1 then
  begin
    PicScale0 := fDrawing2D.PicScale;
    Border0 := fDrawing2D.Border;
    LineWidth0 := fDrawing2D.LineWidthBase;
    HatchingStep0 := fDrawing2D.HatchingStep;
    DottedSize0 := fDrawing2D.DottedSize;
    DashSize0 := fDrawing2D.DashSize;
    fDrawing2D.PicScale := fDrawing2D.PicScale * Magnif;
    if fDrawing2D.PicScale <= 0 then fDrawing2D.PicScale := 1;
    fDrawing2D.Border := fDrawing2D.Border * Magnif;
    fDrawing2D.LineWidthBase := fDrawing2D.LineWidthBase * Magnif;
    fDrawing2D.HatchingStep := fDrawing2D.HatchingStep * Magnif;
    fDrawing2D.DottedSize := fDrawing2D.DottedSize * Magnif;
    fDrawing2D.DashSize := fDrawing2D.DashSize * Magnif;
  end;
  WriteAll0;
  if Magnif <> 1 then
  begin
    fDrawing2D.PicScale := PicScale0;
    fDrawing2D.Border := Border0;
    fDrawing2D.LineWidthBase := LineWidth0;
    fDrawing2D.HatchingStep := HatchingStep0;
    fDrawing2D.DottedSize := DottedSize0;
    fDrawing2D.DashSize := DashSize0;
  end;
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

function T_CAD_Saver.StoreToFile(const FileName: string): Boolean;
begin
  fStream.Free;
  fStream := TFileStream.Create(FileName, fmCreate);
  Result := False;
  try
    WriteAllToStream;
  finally
    fStream.Free;
    fStream := nil;
    Result := FileExists(FileName);
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
      //WriteLnStream('\ifx\pdftexversion\undefined');
      WriteLnStream('\ifpdf');
      WriteAsClass(AClass_PdfTeX);
      WriteLnStream('\else');
      WriteAsClass(AClass_TeX);
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
    tex_pgf:
      AClass_TeX := T_PGF_Export;
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
    pdftex_pgf:
      AClass_PdfTeX := T_PGF_Export;
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
  WriteAll0;
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
  WriteAll0;
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
      if Ch < #32 then Result := '&#' + IntToStr(Ord(Ch)) + ';'
      else Result := Ch;
    end;
  end;
begin
  Result := '';
  for I := 1 to Length(St) do
    Result := Result + ReplaceChar(St[I]);
end;

function XmlUnReplaceChars(const St: string): string;
begin
  Result := St;
  Result := AnsiReplaceStr(Result, '&lt;', '<');
  Result := AnsiReplaceStr(Result, '&gt;', '>');
  Result := AnsiReplaceStr(Result, '&amp;', '&');
  Result := AnsiReplaceStr(Result, '&quot;', '"');
  Result := AnsiReplaceStr(Result, '&apos;', '''');
  Result := AnsiReplaceStr(Result, '&#9;', #9);
  Result := AnsiReplaceStr(Result, '&#10;', #10);
  Result := AnsiReplaceStr(Result, '&#13;', #13);
end;

procedure T_TpX_Saver.WriteHeader;
var
  Rect: TRect2D;
begin
  Rect := fDrawing2D.DrawingExtension;
  fXML.LoadXML('<TpX/>');
  with fXML.DocumentElement do
  begin
    AttributeValue['v'] := 2;
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
      if Trim(fDrawing2D.FontName) <> '' then
        AttributeValue['FontName'] := fDrawing2D.FontName;
      {AttributeValue['PicWidth'] := fDrawing2D.PicWidth;
      AttributeValue['PicHeight'] := fDrawing2D.PicHeight;}
      AttributeValue['PicScale'] := fDrawing2D.PicScale;
      AttributeValue['Border'] := fDrawing2D.Border;
      AttributeValue['PicUnitLength'] :=
        FF(fDrawing2D.PicUnitLength);
      AttributeValue['HatchingStep'] :=
        FF(fDrawing2D.HatchingStep);
      if fDrawing2D.HatchingLineWidth <> HatchingLineWidth_Default then
        AttributeValue['HatchingLineWidth'] :=
          FF(fDrawing2D.HatchingLineWidth);
      AttributeValue['DottedSize'] := fDrawing2D.DottedSize;
      AttributeValue['DashSize'] := fDrawing2D.DashSize;
      AttributeValue['TeXMinLine'] := FF(fDrawing2D.TeXMinLine);
      AttributeValue['LineWidth'] := FF(fDrawing2D.LineWidthBase);
      if fDrawing2D.MiterLimit <> 10 then
        AttributeValue['MiterLimit'] := FF(fDrawing2D.MiterLimit);
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
      Text := XmlReplaceChars(fDrawing2D.Caption);
      AttributeValue['label'] := XmlReplaceChars(fDrawing2D.FigLabel);
    end;
  if fDrawing2D.Comment <> '' then
    with fXML.DocumentElement.AddElement('comment') do
      Text := XmlReplaceChars(fDrawing2D.Comment);
end;

procedure T_TpX_Saver.WritePrimitiveAttr(Obj: TPrimitive2D;
  XMLNode: TXMLDElement);
begin
  if Obj.LineStyle <> liSolid then
    XMLNode.AttributeValue['li'] := GetLineStyleString(Obj.LineStyle);
  if Obj.LineWidth <> 1 then
    XMLNode.AttributeValue['lw'] := Format('%.2f', [Obj.LineWidth]);
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
  ARot: TRealType;
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
    ARot := CalcAngle2Points(P0, P2);
    if ARot <> 0 then
      XMLNode.AttributeValue['rotdeg'] := FF(RadToDeg(ARot));
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
      //jhLeft: XMLNode.AttributeValue['jh'] := 'l'; //default
      jhCenter: XMLNode.AttributeValue['jh'] := 'c';
      jhRight: XMLNode.AttributeValue['jh'] := 'r';
    end;
    case VJustification of
      jvBottom: XMLNode.AttributeValue['jv'] := 'b';
      jvCenter: XMLNode.AttributeValue['jv'] := 'c';
      jvTop: XMLNode.AttributeValue['jv'] := 't';
      //jvBaseline: XMLNode.AttributeValue['jv'] := '0'; //default
    end;
    if Rot <> 0 then
      XMLNode.AttributeValue['rotdeg'] := FF(RadToDeg(Rot));
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
      XMLNode.AttributeValue['rotdeg'] := FF(RadToDeg(ARot));
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

procedure T_TpX_Saver.WriteBezier2D(Obj: TBezierPath2D0);
var
  XMLNode: TXMLDElement;
begin
  XMLNode := fXML.DocumentElement.AddElement('bezier');
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
        [fDrawing2D.LineWidthBase * fDrawing2D.HatchingLineWidth
        * fFactorMM]);
    end;
  end;
end;

procedure T_SVG_Export.WriteHeader;
var
  XMLNode: TXMLDElement;
begin
  fFactorMM := 1 / fDrawing2D.PicUnitLength * 10;
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
    //AttributeValue['fill-rule'] := 'evenodd';
    AttributeValue['fill-rule'] := 'nonzero';
    AttributeValue['stroke-miterlimit'] :=
      Format('%.5g', [fDrawing2D.MiterLimit]);
    if fDrawing2D.FontName <> '' then FFontName := fDrawing2D.FontName
    else if FontName_Default <> '' then FFontName := FontName_Default
    else FFontName := 'Times New Roman';
    fDescent := GetFontDescent;
    AttributeValue['style'] :=
      'font-family: ''' + FFontName + '''; font-weight:normal';
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

procedure T_SVG_Export.WritePrimitiveAttr0(const LineColor,
  HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching;
  MiterLimit: TRealType; XMLNode: TXMLDElement);
begin
  if LineStyle = liNone then
    XMLNode.AttributeValue['stroke'] := 'none'
  else
  begin
    //XMLNode.AttributeValue['stroke-miterlimit'] := MiterLimit;
    if LineColor = clDefault then
      XMLNode.AttributeValue['stroke'] := 'black'
    else
      XMLNode.AttributeValue['stroke'] := ColorToHtml(LineColor);
    if LineStyle <> liNone then
      XMLNode.AttributeValue['stroke-width'] :=
        Format('%.1f', [fDrawing2D.LineWidthBase * LineWidth *
        fFactorMM]);
    case LineStyle of
      liDotted:
        XMLNode.AttributeValue['stroke-dasharray']
          := Format('%.1f,%.1f',
          [fDrawing2D.LineWidthBase * 2 * fFactorMM,
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
    Obj.LineStyle, Obj.LineWidth,
    Obj.Hatching, fDrawing2D.MiterLimit, XMLNode);
end;

procedure T_SVG_Export.WritePoly0(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType; const Hatching:
  THatching;
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
    LineStyle, LineWidth, Hatching, fDrawing2D.MiterLimit, XMLNode);
  XMLNode.AttributeValue['points'] := PathSt;
end;

procedure T_SVG_Export.WritePoly(PP: TPointsSet2D;
  Obj: TPrimitive2D; Closed: Boolean);
begin
  WritePoly0(PP,
    Obj.LineColor, Obj.HatchColor, Obj.FillColor,
    Obj.LineStyle, Obj.LineWidth, Obj.Hatching, Closed);
end;

procedure T_SVG_Export.WriteBezier(PP: TPointsSet2D;
  Obj: TPrimitive2D; Closed: Boolean);
var
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
  PathSt := 'M ';
  AddPoint(PP[0]);
  for I := 1 to PP.Count - 1 do
  begin
    if I mod 3 = 1 then AddSt(' C');
    AddSt(' ');
    AddPoint(PP[I]);
  end;
  if Closed then AddSt(' Z');
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
    A := CalcAngle2Points(P0, P1) + Pi;
    if A <> 0 then
    begin
      XMLNode.AttributeValue['transform'] :=
        'rotate(' + FF(A)
        + 'rad ' + IntToStr(GetX(CP.X))
        + ' ' + IntToStr(GetY(CP.Y)) + ')';
    end;
    WritePrimitiveAttr(Obj, XMLNode);
  end;
end;

function T_SVG_Export.GetFontDescent: TRealType;
var
  ExtendedFont: TExtendedFont;
  //LogFont: TLogFontW;
  h_DC: HDC;
  h_FONT: HFONT;
  Text_Metric: tagTEXTMETRIC;
  I: TRealType;
begin
  ExtendedFont := TExtendedFont.Create;
  ExtendedFont.FaceName := FFontName;
  ExtendedFont.Height := -1000;
  h_DC := GetWindowDC(0);
  h_FONT := CreateFontIndirectA(ExtendedFont.LogFont);
  SelectObject(h_DC, h_FONT);
  GetTextMetrics(h_DC, Text_Metric);
  DeleteObject(h_FONT);
  ReleaseDC(0, h_DC);
  ExtendedFont.Free;
  I := Text_Metric.tmDescent / 1000;
  Result := I;
end;

procedure T_SVG_Export.WriteText2D(Obj: TText2D);
var
  XMLNode: TXMLDElement;
  St: string;
  P: TPoint2D;
  D: TVector2D;
begin
  XMLNode := fXML.DocumentElement.AddElement('text');
  with Obj do
  begin
    P := Points[0];
    D.X := 0;
    case VJustification of
      jvBottom: D.Y := fDescent;
      jvCenter: D.Y := fDescent - 0.5;
      jvTop: D.Y := fDescent - 1;
      jvBaseline: D.Y := 0;
    end;
    if Rot <> 0 then D := TransformVector2D(D, Rotate2D(Rot));
    D := TransformVector2D(D, Scale2D(Height, Height));
    P := ShiftPoint(P, D); //ConvertPnt(
    XMLNode.AttributeValue['x'] := GetX(P.X);
    XMLNode.AttributeValue['y'] := GetY(P.Y);
    XMLNode.AttributeValue['font-size'] := Round(Height * fFactorH);
    if Rot <> 0 then
      XMLNode.AttributeValue['transform'] :=
        'rotate(' + FF(-Rot)
        + 'rad ' + IntToStr(GetX(P.X))
        + ' ' + IntToStr(GetY(P.Y)) + ')';
    //if Rot <> 0 then XMLNode.AttributeValue['rotate'] := Format('%.2f', [-RadToDeg(Rot)]);
    St := Text;
    //St := AnsiReplaceStr(St, '   ', ' _ ');
    //St := AnsiReplaceStr(St, '   ', ' _ ');
    //St := AnsiReplaceStr(St, '  ', ' _');
    St := AnsiToUtf8(St);
    XMLNode.Text := St;
    XMLNode.AttributeValue['xml:space'] := 'preserve';
    case HJustification of
      jhLeft: XMLNode.AttributeValue['text-anchor'] := 'start';
      jhCenter: XMLNode.AttributeValue['text-anchor'] := 'middle';
      jhRight: XMLNode.AttributeValue['text-anchor'] := 'end';
    end;
    if Obj.LineColor <> clDefault then
      XMLNode.AttributeValue['fill'] := ColorToHtml(Obj.LineColor)
    else
      XMLNode.AttributeValue['fill'] := 'black';
    //WritePrimitiveAttr(Obj, XMLNode);
  end;
  {XMLNode := fXML.DocumentElement.AddElement('circle');
  XMLNode.AttributeValue['cx'] := GetX(P.X);
  XMLNode.AttributeValue['cy'] := GetY(P.Y);
  XMLNode.AttributeValue['r'] := 100;
  XMLNode.AttributeValue['fill'] := 'black';}
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
  PP: TPointsSet2D;
begin
  if Obj.Points.Count < 2 then Exit;
  PP := nil;
  try
    Obj.BezierPoints(PP, IdentityTransf2D);
    WriteBezier(PP, Obj, Obj.IsClosed);
  finally
    PP.Free;
  end;
end;

procedure T_SVG_Export.WriteSmooth2D(Obj: TSmoothPath2D0);
var
  PP: TPointsSet2D;
begin
  if Obj.Points.Count < 2 then Exit;
  PP := nil;
  try
    Obj.BezierPoints(PP, IdentityTransf2D);
    WriteBezier(PP, Obj, Obj.IsClosed);
  finally
    PP.Free;
  end;
end;

procedure T_SVG_Export.WriteBezier2D(Obj: TBezierPath2D0);
var
  PP: TPointsSet2D;
begin
  if Obj.Points.Count < 2 then Exit;
  PP := nil;
  try
    Obj.BezierPoints(PP, IdentityTransf2D);
    WriteBezier(PP, Obj, Obj.IsClosed);
  finally
    PP.Free;
  end;
end;

procedure T_SVG_Export.WritePoly2D(Obj: TPolyline2D0);
begin
  WritePoly(Obj.Points, Obj, Obj.IsClosed);
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
  var FontName: string; var Descent: TRealType);
//	Program converts a binary MSDOS representation for a type1
//	PostScript font into a readable ASCII version. The MSDOS
//	newline (\r) is converted into the UNIX newline (\n).
//	The output is written in a file whose name is the name that
//	is provided on the command line or the basename of the input
//	file plus extension ".pfa".
var
  L, I, J: Integer;
  T: Byte;
  St, DescentSt: string;
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
            FontName := Trim(Copy(St, I, J - I));
          end;
          J := Pos('/FontBBox', St);
          if J > 0 then
          begin
            J := J + 8;
            while St[J] <> '{' do Inc(J);
            Inc(J);
            while St[J] in [#10, #13, ' '] do Inc(J);
            while not (St[J] in [#10, ' ', '/', '}']) do Inc(J);
            while St[J] in [#10, #13, ' '] do Inc(J);
            I := J;
            while not (St[J] in [#10, ' ', '/', '}']) do Inc(J);
            DescentSt := Trim(Copy(St, I, J - I));
            Val(DescentSt, I, J);
            if J > 0 then I := 200;
            Descent := Abs(I) / 1000;
            //Application.MessageBox(PChar(DescentSt), nil);
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

procedure pfb2pfa(const FileName_pfb, FileName_pfa: string;
  var Descent: TRealType);
var
  Stream_pfb, Stream_pfa: TFileStream;
  FontName: string;
begin
  Stream_pfb := TFileStream.Create(FileName_pfb, fmOpenRead);
  Stream_pfa := TFileStream.Create(FileName_pfa, fmCreate);
  try
    pfb2pfa_Stream(Stream_pfb, Stream_pfa, FontName, Descent);
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

procedure T_PostScript_Export.WriteLineAttr(const LineStyle: TLineStyle; const
  LineWidth: TRealType;
  const LineColor: TColor);
var
  A: TRealType;
begin
  A := fFactorMM;
  with fDrawing2D do
  begin
    case LineStyle of
      liNone: ; //WriteStream('0 setlinewidth [] 0 setdash ');
      liSolid: WriteStream(
          Format('%.2f setlinewidth [] 0 setdash ',
          [LineWidthBase * LineWidth * A]));
      liDashed: WriteStream(
          Format('%.2f setlinewidth [%.2f %.2f] 0 setdash ',
          [LineWidthBase * LineWidth * A, DashSize * 2 * A, DashSize * A]));
      liDotted: WriteStream(
          Format('%.2f setlinewidth [%.2f %.2f] 0 setdash ',
          [LineWidthBase * LineWidth * A, LineWidthBase * 2 * A,
          DottedSize * A]));
    end;
  end;
  WriteColor(LineColor);
  if fDrawing2D.MiterLimit <> 10 then
    WriteStream(
      Format('%.2g setmiterlimit ', [fDrawing2D.MiterLimit]));
  //if Obj.LineColor <> clDefault then  begin    RGB := PS_RGB(Obj.LineColor);  end;
end;

procedure T_PostScript_Export.WriteFill0(const FillColor: TColor;
  const LineStyle: TLineStyle);
begin
  if LineStyle <> liNone then WriteStream('gsave ');
  if FillColor = clDefault then Exit;
  WriteColor(FillColor);
  //WriteStream('eofill '); // even-odd aka alternate fill rule
  WriteStream('fill '); // nonzero winding fill rule
end;

procedure T_PostScript_Export.WriteFill(Obj: TPrimitive2D);
begin
  WriteFill0(Obj.FillColor, Obj.LineStyle);
end;

procedure T_PostScript_Export.WriteStroke(const LineStyle: TLineStyle; const
  LineWidth: TRealType;
  const LineColor: TColor);
begin
  if LineStyle <> liNone then
  begin
    //if Obj.FillColor <> clDefault then
    WriteStream('grestore ');
    WriteLineAttr(LineStyle, LineWidth, LineColor);
    WriteStream('stroke ');
  end;
  WriteLnStream('');
end;

procedure T_PostScript_Export.WritePoly00(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching;
  const Closed: Boolean);
var
  I: Integer;
begin
  if PP.Count < 1 then Exit;
  if (LineStyle = liNone) and (FillColor = clDefault) then Exit;
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
  const LineStyle: TLineStyle; const LineWidth: TRealType; const Hatching:
  THatching;
  const Closed: Boolean);
begin
  if PP.Count <= 1 then Exit;
  WritePoly00(PP, clDefault, clDefault,
    GetColor(FillColor, clBlack),
    liNone, 1, haNone, Closed);
  WriteFill0(FillColor, LineStyle);
  WriteHatching(PP, Hatching, HatchColor, fHatchingStep);
  WriteStroke(LineStyle, LineWidth, LineColor);
end;

procedure T_PostScript_Export.WritePoly(PP: TPointsSet2D;
  Obj: TPrimitive2D; Closed: Boolean);
begin
  WritePoly00(PP, clDefault, clDefault,
    GetColor(Obj.FillColor, clBlack),
    liNone, 1, haNone, Closed);
end;

procedure T_PostScript_Export.WriteBezierPath(const PP:
  TPointsSet2D; Obj: TPrimitive2D);
var
  I: Integer;
begin
  if (Obj.LineStyle = liNone) and (Obj.FillColor = clDefault) then Exit;
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
  fDescent := 0.2;
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
    pfb2pfa_Stream(Stream_pfb, fStream, FontName, fDescent);
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

type TFillRule = (fr_Winding, fr_Alternate);

procedure CalculateHatching(const P: TPointsSet2D;
  const DX, DY: TRealType; Step: TRealType;
  const Lines: TPointsSet2D; const FillRule: TFillRule);
var
  PP: TPointsSet2D;
  PrevP, CurrP, IntersP: TPoint2D;
  PrevV, CurrV, A: TRealType;
  MinValue, MaxValue, Value0, C: TRealType;
  I, J, K: Integer;
  Sgns: array of Boolean;
  TmpSgn: Boolean;
  SgnCount: Integer;
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
  SetLength(Sgns, P.Count);
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
      Sgns[PP.Count - 1] := CurrV > PrevV;
    end;
    // Sort PP:
    for I := 0 to PP.Count - 1 do
    begin
      IntersP := PP[I];
      TmpSgn := Sgns[I];
      for K := I + 1 to PP.Count - 1 do
        if - DY * IntersP.X + DX * IntersP.Y
          > -DY * PP[K].X + DX * PP[K].Y then
        begin
          PP[I] := PP[K];
          PP[K] := IntersP;
          IntersP := PP[I];
          Sgns[I] := Sgns[K];
          Sgns[K] := TmpSgn;
          TmpSgn := Sgns[I];
        end;
    end;
    if FillRule = fr_Winding then
    begin
      SgnCount := 0;
      for K := 0 to (PP.Count div 2) * 2 - 1 do
      begin
        if SgnCount = 0 then Lines.Add(PP[K]);
        if Sgns[K] then Inc(SgnCount) else Dec(SgnCount);
        if SgnCount = 0 then Lines.Add(PP[K]);
      end;
    end
    else //FillRule = fr_Alternate
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
    CalculateHatching(P, DX, DY, Step, Lines, fr_Winding);
    WriteStream('newpath ');
    with fDrawing2D do WriteStream(
        Format('%.2f setlinewidth [] 0 setdash ',
        [LineWidthBase * HatchingLineWidth * fFactorMM]));
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
    WriteStroke(Obj.LineStyle, Obj.LineWidth, Obj.LineColor);
  end;
end;

{procedure T_PDF_Export.WriteText2D(Obj: TText2D);
var
  FontH: TRealType;
  D: TVector2D;
  P: TPoint2D;
  T: TTransf2D;
begin
  with Obj, fPDF.Canvas do
  begin
    P := Points[0];
    FontH := Height * fFactorH ;// / 1.2;
    SetFont('Times-Roman', FontH);
    case HJustification of
      jhLeft: D.X := 0;
      jhCenter: D.X := -TextWidth(Text) / 2;
      jhRight: D.X := -TextWidth(Text);
    end;
    case VJustification of
      jvBottom: D.Y := 0.2 * FontH;
      jvCenter: D.Y := -0.3 * FontH;
      jvTop: D.Y := -0.8 * FontH;
    end;
    if Obj.LineColor <> clDefault then
      SetRGBFillColor(Obj.LineColor)
    else SetRGBFillColor(clBlack);
    if Rot <> 0 then
    begin
      T := Rotate2D(Rot);
      D := TransformVector2D(D, T);
    end;
    P := ShiftPoint(ConvertPnt(P), D);
    BeginText;
    if Rot = 0 then
      MoveTextPoint(P.X, P.Y)
    else
      SetPDF_TextMatrix(fPDF.Canvas,
        MultiplyTransform2D(T, Translate2D(P.X, P.Y)));
    ShowText(Text);
    EndText;
  end;
end;}

procedure T_PostScript_Export.WriteText2D(Obj: TText2D);
var
  P: TPoint2D;
  D: TVector2D;
  Rect: TRect2D;
  St: string;
begin
  with Obj do
  begin
    Rect := GetExtension;
    P := Points[0];
    case VJustification of
      jvBottom: D.Y := fDescent;
      jvCenter: D.Y := fDescent - 0.5;
      jvTop: D.Y := fDescent - 1;
      jvBaseline: D.Y := 0;
    end;
    D.X := 0;
    if Rot <> 0 then D := TransformVector2D(D, Rotate2D(Rot));
    D := TransformVector2D(D, Scale2D(Height, Height));
    P := ShiftPoint(P, D);
    St := AnsiReplaceText(Text, '\', '\\');
    St := AnsiReplaceText(St, '(', '\(');
    St := AnsiReplaceText(St, ')', '\)');
    WriteStream(Format('/%s findfont %d scalefont setfont newpath ',
      [FontName, Round(Height * fFactorH)]));
    if Obj.LineColor <> clDefault then
      WriteColor(Obj.LineColor)
    else
      WriteColor(clBlack);
    WriteStreamPoint(P);
    WriteLnStream('moveto ');
    //"string" stringwidth -> "wx" "wy"
    {stringwidth returns the length of the string ( ... ) and (usually) the value 0.0}
    case HJustification of
      jhLeft: D.X := 0;
      jhCenter: D.X := 0.5;
      jhRight: D.X := 1;
    end;
    if D.X <> 0 then
      if Rot = 0 then
        WriteStream(Format('(%s) stringwidth pop %.5g mul 0 rmoveto',
          [St, -D.X * Cos(Rot)]))
      else
        WriteStream(Format('(%s) stringwidth pop %.5g mul (%s) stringwidth pop %.5g mul rmoveto',
          [St, -D.X * Cos(Rot), St, -D.X * Sin(Rot)]));
    if Rot <> 0 then
      WriteStream(Format(' %.5g rotate', [RadToDeg(Rot)]));
    if (D.X <> 0) or (Rot <> 0) then WriteLnStream('');
    WriteLnStream(Format('(%s) show stroke', [St]));
    if Rot <> 0 then
      WriteStream('initmatrix ');
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
  WriteStroke(Obj.LineStyle, Obj.LineWidth, Obj.LineColor);
end;

procedure T_PostScript_Export.WriteCircle2D(Obj: TCircle2D);
var
  CP: TPoint2D;
  R: TRealType;
begin
  with Obj do
  begin
    if (Obj.LineStyle <> liNone) or (Obj.FillColor <> clDefault) then
    begin
      CP := Points[0];
      R := PointDistance2D(CP, Points[1]);
      WriteStream('newpath ');
      WriteStreamPoint(CP);
      WriteLnStream(Format('%.2f 0 360 arc ', [R * fFactorW]));
      WriteStream('closepath ');
      WriteFill(Obj);
    end;
    if Hatching <> haNone then
    begin
      BeginUseProfilePoints;
      WriteHatching(ProfilePoints,
        Obj.Hatching, Obj.HatchColor, fHatchingStep);
      EndUseProfilePoints;
    end;
    WriteStroke(Obj.LineStyle, Obj.LineWidth, Obj.LineColor);
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
    if (Obj.LineStyle <> liNone) or (Obj.FillColor <> clDefault) then
    begin
      WriteStream('newpath ');
      if Obj is TSector2D then
      begin
        WriteStreamPoint(CP);
        WriteStream('moveto ');
      end;
      WriteStreamPoint(CP);
      WriteStream(Format('%.1f %.1f %.1f arc ',
        [R * fFactorW, RadToDeg(SA), RadToDeg(EA)]));
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
    WriteStroke(Obj.LineStyle, Obj.LineWidth, Obj.LineColor);
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
  WriteStroke(Obj.LineStyle, Obj.LineWidth, Obj.LineColor);
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
  WriteStroke(Obj.LineStyle, Obj.LineWidth, Obj.LineColor);
end;

procedure T_PostScript_Export.WriteBezier2D(Obj:
  TBezierPath2D0);
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
  WriteStroke(Obj.LineStyle, Obj.LineWidth, Obj.LineColor);
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
  WriteStroke(Obj.LineStyle, Obj.LineWidth, Obj.LineColor);
end;

procedure T_PostScript_Export.WriteToTpX(Stream: TStream;
  const FileName: string);
var
  H, W, UnitLength: TRealType;
  I: Integer;
  EpsFileName: string;
begin
  fStream := nil;
  if (Self is T_EpsToPdf_Export) or
    (Self is T_EpsToPdf_Light_Export) then
    EpsFileName := ChangeFileExt(FileName, '.pdf')
  else EpsFileName := ChangeFileExt(FileName, '.eps');
  if not StoreToFile(EpsFileName) then Exit;
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

{ --================ T_EpsToPdf_Export ==================-- }

function Run_EpsToPdf(const EpsFileName, PdfFileName: string): Boolean;
begin
  if not FileExists(EpsToPdfPath) then
  begin
    Application.MessageBox('EpsToPdf path not found',
      'Error', MB_OK);
    Exit;
  end;
  try
    TryDeleteFile(PdfFileName);
    Result := FileExec(Format('"%s" "%s" --outfile="%s"',
      [EpsToPdfPath, EpsFileName, PdfFileName]), '', '',
      '', False, True);
    if not FileExists(PdfFileName) then
    begin
      Application.MessageBox('PDF file not created ',
        'Error', MB_OK);
      Result := False;
    end;
  finally
  end;
end;

function T_EpsToPdf_Export.StoreToFile(const FileName: string):
  Boolean;
var
  TempEPS: string;
begin
  TempEPS := GetTempDir + '(eps)TpX.eps';
  TryDeleteFile(TempEPS);
  Result := False;
  try
    Result := inherited StoreToFile(TempEPS);
    if Result then Result := Run_EpsToPdf(TempEPS, FileName);
  finally
    Result := True;
    if Result then TryDeleteFile(TempEPS);
  end;
end;

{ --================ T_EpsToPdf_Light_Export ==================-- }

procedure T_EpsToPdf_Light_Export.WriteFont;
begin

end;

procedure T_EpsToPdf_Light_Export.WriteText2D(Obj: TText2D);
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

procedure T_TeX_Picture_Export.WriteLineThickness0(
  const LineWidth: TRealType);
begin
//\linethickness{dimension}
  if LineWidth <= 1.5 then WriteStream('\thinlines')
  else WriteStream('\thicklines');
end;

procedure T_TeX_Picture_Export.WriteLineThickness(Obj:
  TPrimitive2D);
begin
  if (Obj.LineStyle <> liNone) or (Obj.Hatching = haNone) then
    WriteLineThickness0(Obj.LineWidth);
end;

procedure T_TeX_Picture_Export.WriteLine(P0, P1: TPoint2D;
  const LineStyle: TLineStyle;
  const LineWidth: TRealType);
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
  case LineStyle of
    liNone, liSolid:
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
  const LineStyle: TLineStyle; const LineWidth: TRealType; const Hatching:
  THatching;
  const Closed: Boolean);
var
  I: Integer;
begin
  if PP.Count = 0 then Exit;
  if (LineStyle <> liNone) or (Hatching = haNone) then
  begin
    WriteLineThickness0(LineWidth);
    for I := 0 to PP.Count - 2 do
    begin
      WriteLine(PP[I], PP[I + 1], LineStyle, LineWidth);
      if (I mod 100) = 99 then
      begin
        WriteLnStream('');
        WriteStream(' ');
      end;
    end;
    if Closed then
      WriteLine(PP[PP.Count - 1], PP[0], LineStyle, LineWidth);
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
      Step / fFactorH * fFactorMM, Lines, fr_Winding);
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
    if (LineStyle <> liNone) or (Hatching = haNone) then
    begin
      WriteLine(P[0], P[1], Obj.LineStyle, Obj.LineWidth);
      WriteLine(P[1], P[2], Obj.LineStyle, Obj.LineWidth);
      WriteLine(P[2], P[3], Obj.LineStyle, Obj.LineWidth);
      WriteLine(P[3], P[0], Obj.LineStyle, Obj.LineWidth);
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
        if LineStyle <> liNone then
          StarsSize := StarsSize +
            0.5 * (Obj.OwnerCAD as TDrawing2D).LineWidthBase *
            Obj.LineWidth / (Obj.OwnerCAD as TDrawing2D).PicScale;
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
    if (LineStyle <> liNone) or (Hatching = haNone) then
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
    if (LineStyle <> liNone) or (Hatching = haNone) then
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
  if (Obj.LineStyle <> liNone) or (Obj.Hatching = haNone) then
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
          WriteLine(CP, GetPoint(SA, R), Obj.LineStyle, Obj.LineWidth);
          WriteLine(CP, GetPoint(EA, R), Obj.LineStyle, Obj.LineWidth);
        end
        else if Obj is TSegment2D then
          WriteLine(GetPoint(SA, R), GetPoint(EA, R),
            Obj.LineStyle, Obj.LineWidth);
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
  if (Obj.LineStyle <> liNone) or (Obj.Hatching = haNone) then
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
  if (Obj.LineStyle <> liNone) or (Obj.Hatching = haNone) then
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
  if (Obj.LineStyle <> liNone) or (Obj.Hatching = haNone) then
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
  if (Obj.LineStyle <> liNone) or (Obj.Hatching = haNone) then
  begin
    PP := nil;
    try
      Obj.BezierPoints(PP, IdentityTransf2D);
      for I := 0 to (PP.Count - 4) div 3 do
        WriteCBezier(PP[I * 3], PP[I * 3 + 1], PP[I * 3 + 2], PP[I * 3 + 3]);
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

procedure T_TeX_Picture_Export.WriteBezier2D(Obj:
  TBezierPath2D0);
var
  I: Integer;
  PP: TPointsSet2D;
begin
  WriteLineThickness(Obj);
  if Obj.Points.Count < 2 then Exit;
  if (Obj.LineStyle <> liNone) or (Obj.Hatching = haNone) then
  begin
    PP := nil;
    try
      Obj.BezierPoints(PP, IdentityTransf2D);
      for I := 0 to (PP.Count - 4) div 3 do
        WriteCBezier(PP[I * 3], PP[I * 3 + 1], PP[I * 3 + 2], PP[I * 3 + 3]);
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
    Obj.LineStyle, Obj.LineWidth, Obj.Hatching, Obj.IsClosed);
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

function T_PSTricks_Export.LineArg(const LineStyle: TLineStyle;
  const LineWidth: TRealType;
  Color: string): string;
begin
  if LineStyle <> liNone then
    Result := Format('linewidth=%.2fmm',
      [fDrawing2D.LineWidthBase * LineWidth]);
  case LineStyle of
    liNone:
      Result := Result + 'linestyle=none';
    liDotted:
      Result := Result + ',linestyle=dotted';
    liDashed:
      Result := Result + ',linestyle=dashed';
  else
    Result := Result + ',linestyle=solid';
  end;
  //Result := Result + ',linearc=0.0003mm';
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
  WriteStream(Format('\psline%s', [LineArg(Obj.LineStyle, Obj.LineWidth, C)]));
  WriteStreamPoint(P0);
  WriteStreamPoint(P1);
end;

procedure T_PSTricks_Export.WritePoly(const PP: TPointsSet2D;
  const Attr: string; const LineWidth: TRealType; Closed: Boolean);
var
  I: Integer;
begin
  if PP.Count < 1 then Exit;
//  if fDrawing2D.MiterLimit <> 10 then
//    WriteLnStream(Format('\pscustom{\code{%.2f setmiterlimit}',
//      [fDrawing2D.MiterLimit]));
//\pscustom{%
//    \code{1 setlinejoin}
//    \psline(0,0)(1,2)(2,0)}
  if Closed then
    WriteStream(Format('\pspolygon%s', [Attr]))
  else
    WriteStream(Format('\psline%s', [Attr]));
  for I := 0 to PP.Count - 1 do
    WriteStreamPoint(PP[I]);
//  if fDrawing2D.MiterLimit <> 10 then    WriteStream('}');
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
      Step / fFactorH / fUnitLength, Lines, fr_Winding);
    for I := 0 to Lines.Count div 2 - 1 do
    begin
      if HatchColor = clDefault then C := ''
      else
        C := ',linecolor=' + WriteNewColor(HatchColor);
      WriteStream(Format('\psline[linewidth=%.2fmm,linestyle=solid%s]',
        [fDrawing2D.LineWidthBase * fDrawing2D.HatchingLineWidth, C]));
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
  const LineStyle: TLineStyle; const LineWidth: TRealType; const Hatching:
  THatching;
  const Closed: Boolean);
var
  Attr: string;
begin
  if (FillColor <> clDefault) and (HatchPP <> nil) then
  begin
    Attr := '[linestyle=none,fillstyle=solid,fillcolor=' +
      WriteNewColor(FillColor) + ']';
    PathProc(PP, Attr, LineWidth, Closed);
  end;
  if HatchPP <> nil then
    WriteHatching(HatchPP, Hatching, HatchColor, fHatchingStep);
  if LineStyle <> liNone then
  begin
    Attr := LineArg(LineStyle, LineWidth, WriteNewColor(LineColor));
    PathProc(PP, Attr, LineWidth, Closed);
  end;
  WriteLnStream('');
end;

procedure T_PSTricks_Export.WritePoly0(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType; const Hatching:
  THatching;
  const Closed: Boolean);
begin
  WritePath(PP, PP, WritePoly, LineColor, HatchColor, FillColor,
    LineStyle, LineWidth, Hatching, Closed);
end;

procedure T_PSTricks_Export.WriteBezierPath(const PP: TPointsSet2D;
  const Attr: string; const LineWidth: TRealType; Closed: Boolean);
var
  I: Integer;
begin
  if PP.Count < 1 then Exit;
  WriteStream(Format('\pscustom%s{', [Attr]));
  for I := 0 to PP.Count div 3 - 1 do
  begin
    WriteStream('\psbezier');
    WriteStreamPoint(PP[3 * I]);
    WriteStreamPoint(PP[3 * I + 1]);
    WriteStreamPoint(PP[3 * I + 2]);
    WriteStreamPoint(PP[3 * I + 3]);
  end;
  WriteStream('}');
end;

procedure T_PSTricks_Export.WriteCircle(const PP: TPointsSet2D;
  const Attr: string; const LineWidth: TRealType; Closed: Boolean);
begin
  WriteStream(Format('\pscircle%s', [Attr]));
  WriteStreamPoint(PP[0]);
  WriteStream(Format('{%.2f}',
    [PointDistance2D(PP[0], PP[1]) * fFactorW
    + fDrawing2D.LineWidthBase * LineWidth / fUnitLength / 2]));
end;

procedure T_PSTricks_Export.WriteCircular0(const PP: TPointsSet2D;
  const Attr: string; const LineWidth: TRealType; ObjClass: TClass);
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
  begin
    WriteStream(Format('\pswedge%s', [Attr]));
    R := R + fDrawing2D.LineWidthBase * LineWidth / fUnitLength / 2 / fFactorW;
  end
  else
    WriteStream(Format('\psarc%s{-}', [Attr]));
  WriteStreamPoint(CP);
  WriteStream(Format('{%.2f}{%.2f}{%.2f}',
    [R * fFactorW, RadToDeg(SA), RadToDeg(EA)]));
  if ObjClass = TSegment2D then
  begin
    PP2 := TPointsSet2D.Create(4);
    try
      Delt := fDrawing2D.LineWidthBase / fUnitLength
        / (R * fFactorW); // Draw miters
      PP2.Add(GetPoint(SA + Delt, R));
      PP2.Add(GetPoint(SA, R));
      PP2.Add(GetPoint(EA, R));
      PP2.Add(GetPoint(EA - Delt, R));
      WritePoly(PP2, Attr, LineWidth, False);
    finally
      PP2.Free;
    end;
  end;
end;

procedure T_PSTricks_Export.WriteArc(const PP: TPointsSet2D;
  const Attr: string; const LineWidth: TRealType; Closed: Boolean);
begin
  WriteCircular0(PP, Attr, LineWidth, TArc2D);
end;

procedure T_PSTricks_Export.WriteSector(const PP: TPointsSet2D;
  const Attr: string; const LineWidth: TRealType; Closed: Boolean);
begin
  WriteCircular0(PP, Attr, LineWidth, TSector2D);
end;

procedure T_PSTricks_Export.WriteSegment(const PP: TPointsSet2D;
  const Attr: string; const LineWidth: TRealType; Closed: Boolean);
begin
  WriteCircular0(PP, Attr, LineWidth, TSegment2D);
end;

procedure T_PSTricks_Export.WriteHeader;
begin
  fUnitLength := fDrawing2D.PicUnitLength / 10;
  fFactorMM := 1 / fUnitLength;
  MeasureDrawing;
  fW := fW_MM * fFactorMM;
  fH := fH_MM * fFactorMM;
  fHatchingStep := fDrawing2D.HatchingStep;
  //WriteLnStream('\clearpage');
  //WriteLnStream(Format('\psset{xunit=%.4g mm, yunit=%.4g mm, runit=%.4g mm}',    [fUnitLength, fUnitLength, fUnitLength]));
  WriteLnStream(Format('\psset{unit=%.5g mm}', [fUnitLength]));
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
      [LineWidthBase]));
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
      Obj.LineStyle, Obj.LineWidth, Obj.Hatching, True);
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
        Obj.LineStyle, Obj.LineWidth, Obj.Hatching, True);
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
      Obj.LineStyle, Obj.LineWidth, Obj.Hatching, True);
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
        Obj.LineStyle, Obj.LineWidth, Obj.Hatching, Obj.IsClosed)
    else if Obj is TSegment2D then
      WritePath(Points, ProfilePoints, WriteSegment,
        Obj.LineColor, Obj.HatchColor, Obj.FillColor,
        Obj.LineStyle, Obj.LineWidth, Obj.Hatching, Obj.IsClosed)
    else
      WritePath(Points, ProfilePoints, WriteArc,
        Obj.LineColor, Obj.HatchColor, Obj.FillColor,
        Obj.LineStyle, Obj.LineWidth, Obj.Hatching, Obj.IsClosed);
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
        Obj.LineStyle, Obj.LineWidth, Obj.Hatching, Obj.IsClosed);
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
        Obj.LineStyle, Obj.LineWidth, Obj.Hatching, Obj.IsClosed);
    finally
      PP.Free;
      EndUseProfilePoints;
    end;
  end;
end;

procedure T_PSTricks_Export.WriteBezier2D(Obj: TBezierPath2D0);
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
        Obj.LineStyle, Obj.LineWidth, Obj.Hatching, Obj.IsClosed);
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
      Obj.LineStyle, Obj.LineWidth, Obj.Hatching, Obj.IsClosed);
end;

{ --================ T_PGF_Export ==================-- }

procedure T_PGF_Export.WriteStreamPoint0(const X, Y: TRealType);
begin
  WriteStream(Format('{\pgfxy(%.2f,%.2f)}', [X, Y]));
end;

function T_PGF_Export.GetColor(Color: TColor): string;
var
  RGB: T_PS_RGB;
begin
  if Color = CurrColor then
  begin
    Result := '';
    Exit;
  end;
  CurrColor := Color;
  RGB := PS_RGB(Color);
  Result := Format('\color[rgb]{%.5g,%.5g,%.5g}',
    [RGB.R, RGB.G, RGB.B]);
end;

procedure T_PGF_Export.WriteColor(Color: TColor);
begin
  WriteStream(GetColor(Color));
end;

procedure T_PGF_Export.WriteDash(LineStyle: TLineStyle);
begin
  if LineStyle = CurrLineStyle then Exit;
  CurrLineStyle := LineStyle;
  case LineStyle of
    liSolid:
      WriteStream('\pgfsetdash{}{0mm}');
    liDotted:
      WriteStream(Format('\pgfsetdash{{%.2fmm}{%.2fmm}}{0mm}',
        [fDrawing2D.LineWidthBase * 2,
        fDrawing2D.DottedSize]));
    liDashed:
      WriteStream(Format('\pgfsetdash{{%.2fmm}{%.2fmm}}{0mm}',
        [fDrawing2D.DashSize * 2,
        fDrawing2D.DashSize]));
  end;
end;

procedure T_PGF_Export.WriteLineWidth(W: TRealType);
begin
  if W = CurrLineWidth then Exit;
  CurrLineWidth := W;
  WriteStream(Format('\pgfsetlinewidth{%.2fmm}', [W]));
end;

procedure T_PGF_Export.WriteLineThickness0(const LineStyle: TLineStyle;
  const LineWidth: TRealType);
begin
  WriteDash(LineStyle);
  if LineStyle <> liNone then
    WriteLineWidth(fDrawing2D.LineWidthBase * LineWidth);
end;

procedure T_PGF_Export.WriteLineThickness(Obj: TPrimitive2D);
begin
  WriteLineThickness0(Obj.LineStyle, Obj.LineWidth);
end;

procedure T_PGF_Export.WritePoly(const PP: TPointsSet2D;
  const PathKind: TPathKind; Closed: Boolean);
var
  I: Integer;
begin
  if PP.Count < 1 then Exit;
  WriteStream('\pgfmoveto');
  WriteStreamPoint(PP[0]);
  for I := 1 to PP.Count - 1 do
  begin
    WriteStream('\pgflineto');
    WriteStreamPoint(PP[I]);
    if I mod 100 = 0 then
    begin
      WriteLnStream('');
      WriteStream(' ');
    end;
  end;
  if Closed then
    WriteStream('\pgfclosepath');
  WritePathKind(PathKind);
end;

procedure T_PGF_Export.WriteHatching(const P: TPointsSet2D;
  const Hatching: THatching;
  const HatchColor: TColor; Step: TRealType);
var
  Lines: TPointsSet2D;
  DX, DY: TRealType;
  procedure WriteHatching0;
  var
    I: Integer;
  begin
    if (DX = 0) and (DY = 0) then Exit;
    CalculateHatching(P, DX, DY, Step / fFactorH, Lines, fr_Winding);
    if HatchColor = clDefault then WriteColor(clBlack)
    else WriteColor(HatchColor);
    WriteDash(liSolid);
    WriteLineWidth(fDrawing2D.LineWidthBase * fDrawing2D.HatchingLineWidth);
    for I := 0 to Lines.Count div 2 - 1 do
    begin
      WriteStream('\pgfline');
      WriteStreamPoint(Lines[I * 2]);
      WriteStreamPoint(Lines[I * 2 + 1]);
      if Succ(I) mod 100 = 0 then
      begin
        WriteLnStream('');
        WriteStream(' ');
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

procedure T_PGF_Export.WritePathKind(const PathKind: TPathKind);
begin
  case PathKind of
    path_Fill: WriteStream('\pgffill');
    path_Stroke: WriteStream('\pgfstroke');
    path_FillStroke: WriteStream('\pgffillstroke');
  end;
end;

function T_PGF_Export.GetPathKindString(const PathKind: TPathKind): string;
begin
  case PathKind of
    path_Fill: Result := '[fill]';
    path_Stroke: Result := '[stroke]';
    path_FillStroke: Result := '[fillstroke]';
  end;
end;

procedure T_PGF_Export.WritePath(const PP, HatchPP: TPointsSet2D;
  PathProc: TPathProcPathKind;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType; const Hatching:
  THatching;
  const Closed: Boolean);
begin
  if (FillColor <> clDefault) and (HatchPP <> nil) then
  begin
    WriteColor(FillColor);
    PathProc(PP, path_Fill, Closed);
  end;
  if HatchPP <> nil then
    WriteHatching(HatchPP, Hatching, HatchColor, fHatchingStep);
  if LineStyle <> liNone then
  begin
    WriteColor(LineColor);
    WriteLineThickness0(LineStyle, LineWidth);
    PathProc(PP, path_Stroke, Closed);
  end;
  WriteLnStream('');
end;

procedure T_PGF_Export.WritePoly0(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType; const Hatching:
  THatching;
  const Closed: Boolean);
begin
  WritePath(PP, PP, WritePoly, LineColor, HatchColor, FillColor,
    LineStyle, LineWidth, Hatching, Closed);
end;

procedure T_PGF_Export.WriteBezierPath(const PP: TPointsSet2D;
  const PathKind: TPathKind; Closed: Boolean);
var
  I: Integer;
begin
  if PP.Count < 1 then Exit;
  WriteStream('\pgfmoveto');
  WriteStreamPoint(PP[0]);
  for I := 0 to PP.Count div 3 - 1 do
  begin
    WriteStream('\pgfcurveto');
    WriteStreamPoint(PP[3 * I + 1]);
    WriteStreamPoint(PP[3 * I + 2]);
    WriteStreamPoint(PP[3 * I + 3]);
    if Succ(I) mod 50 = 0 then
    begin
      WriteLnStream('');
      WriteStream(' ');
    end;
  end;
  if Closed then
    WriteStream('\pgfclosepath');
  WritePathKind(PathKind);
end;

procedure T_PGF_Export.WriteCircle(const PP: TPointsSet2D;
  const PathKind: TPathKind; Closed: Boolean);
begin
  WriteStream('\pgfcircle');
  WriteStream(GetPathKindString(PathKind));
  WriteStreamPoint(PP[0]);
  WriteStream(Format('{%.2fmm}',
    [PointDistance2D(PP[0], PP[1]) * fFactorW]));
end;

procedure T_PGF_Export.WriteEllipse(const PP: TPointsSet2D;
  const PathKind: TPathKind; Closed: Boolean);
var P0, P1, P2, P3, P4: TPoint2D;
  RX, RY, ARot: TRealType;
  CP: TPoint2D;
begin
  WriteStream('\pgfellipse');
  WriteStream(GetPathKindString(PathKind));
  P0 := PP[0];
  P1 := PP[1];
  P2 := PP[2];
  GetEllipseParams0(P0, P1, P2, P3, P4, CP.X, CP.Y, RX, RY, ARot);
  P0 := ConvertPnt(P0);
  P1 := ConvertPnt(P1);
  P3 := ConvertPnt(P3);
  WriteStreamPoint(CP);
  WriteStreamPoint0((P0.X - P3.X) / 2, (P0.Y - P3.Y) / 2);
  WriteStreamPoint0((P1.X - P3.X) / 2, (P1.Y - P3.Y) / 2);
end;

procedure T_PGF_Export.WriteHeader;
begin
  fUnitLength := fDrawing2D.PicUnitLength / 10;
  fFactorMM := 1;
  MeasureDrawing;
//  fW := fW_MM * fFactorMM;
//  fH := fH_MM * fFactorMM;
  fHatchingStep := fDrawing2D.HatchingStep;
  WriteLnStream('\setlength{\unitlength}{1mm}');
  WriteLnStream(Format('\begin{pgfpicture}{0mm}{0mm}{%.2fmm}{%.2fmm}',
    [fW_MM, fH_MM]));
  WriteLnStream('\pgfsetxvec{\pgfpoint{1mm}{0mm}}');
  WriteLnStream('\pgfsetyvec{\pgfpoint{0mm}{1mm}}');
  if fDrawing2D.MiterLimit <> 10 then
    if fDrawing2D.MiterLimit < 1.01 then
      WriteLnStream('\pgfsetmiterlimit{1.01pt}')
    else
      WriteLnStream(Format('\pgfsetmiterlimit{%.2fpt}',
        [fDrawing2D.MiterLimit]));
  WriteColor(clBlack);
  WriteLineWidth(fDrawing2D.LineWidthBase);
  WriteDash(liSolid);
  WriteLnStream('');
end;

procedure T_PGF_Export.WriteFooter;
begin
  WriteLnStream('\end{pgfpicture}');
end;

procedure T_PGF_Export.WriteRectangle2D(Obj: TRectangle2D);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    WritePath(ProfilePoints, ProfilePoints, WritePoly,
      Obj.LineColor, Obj.HatchColor, Obj.FillColor,
      Obj.LineStyle, Obj.LineWidth, Obj.Hatching, True);
    EndUseProfilePoints;
  end;
end;

function GetPgfBox(Obj: TText2D;
  FactorW, FactorH, UnitLength: TRealType): string;
var
  Rect: TRect2D;
  Width: TRealType;
  JustStrH, JustStrV, St: string;
begin
  with Obj do
  begin
    Rect := GetExtension0;
    Width := (Rect.Right - Rect.Left);
    Result := GetTeXTextFontSize(Obj, FactorH, UnitLength);
    Result := Result + GetTeXTextFontColor(Obj);
    if Obj.LineColor <> clDefault then Result := Result + '{';

    case HJustification of
      jhLeft: JustStrH := 'left';
      jhCenter: JustStrH := 'center'; //??
      jhRight: JustStrH := 'right';
    end;
    case VJustification of
      jvBaseline: JustStrV := 'baseline';
      jvBottom: JustStrV := 'bottom';
      jvCenter: JustStrV := 'center'; //??
      jvTop: JustStrV := 'top';
    end;
    if TeXText <> '' then St := TeXText
    else St := TeX_Replace_Special(Text);
    Result := Result + Format('{\pgfbox[%s,%s]{%s\strut}}',
      [JustStrH, JustStrV, St]);

    //Result := Result +
    //  Format('\makebox(%.1f, %.1f)[', [Width * FactorW, Height * FactorH]);
    //Result := Result + ']{' + St + {} '\strut}';
    if Obj.LineColor <> clDefault then Result := Result + '}';
    //???::
    if Rot = 0 then
    else
    begin
      Result :=
        Format('\rotatebox{%.2f}{%s}', //\frame{} \fbox{}
        [RadToDeg(Rot), Result]);
    end;
  end;
//  '\pgfputat{\pgfxy(1,1)}{\pgfbox[center,center]{Hi!}}'
end;

procedure T_PGF_Export.WriteText2D(Obj: TText2D);
var
  P: TPoint2D;
begin
  with Obj do
  begin
    if Obj.LineColor = clDefault then
      WriteStream(GetColor(clBlack));
    WriteStream('\pgfputat');
    P := ConvertPnt(Points[0]);
    GetTeXTextPoint(P, Obj, fFactorW, fFactorH);
    WriteStreamPoint0(P.X, P.Y);
    WriteLnStream(Format('{\pgfbox[bottom,left]{%s}}',
      [GetTeXTextMakebox(Obj, fFactorW, fFactorH, 1)]));
//    WriteLnStream('{' + GetPgfBox(Obj, fFactorW, fFactorH, 1) + '}');
  end;
end;

procedure T_PGF_Export.WriteStar2D(Obj: TStar2D);
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

procedure T_PGF_Export.WriteEllipse2D(Obj: TEllipse2D);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    WritePath(Points, ProfilePoints, WriteEllipse,
      Obj.LineColor, Obj.HatchColor, Obj.FillColor,
      Obj.LineStyle, Obj.LineWidth, Obj.Hatching, True);
    EndUseProfilePoints;
  end;
end;

procedure T_PGF_Export.WriteCircle2D(Obj: TCircle2D);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    WritePath(Points, ProfilePoints, WriteCircle,
      Obj.LineColor, Obj.HatchColor, Obj.FillColor,
      Obj.LineStyle, Obj.LineWidth, Obj.Hatching, True);
    EndUseProfilePoints;
  end;
end;

procedure T_PGF_Export.WriteCircular2D(Obj: TCircular2D);
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
        Obj.LineStyle, Obj.LineWidth, Obj.Hatching, Obj.IsClosed);
    finally
      PP.Free;
      EndUseProfilePoints;
    end;
  end;
end;

procedure T_PGF_Export.WriteSpline2D(Obj: TSpline2D0);
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
        Obj.LineStyle, Obj.LineWidth, Obj.Hatching, Obj.IsClosed);
    finally
      PP.Free;
      EndUseProfilePoints;
    end;
  end;
end;

procedure T_PGF_Export.WriteSmooth2D(Obj: TSmoothPath2D0);
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
        Obj.LineStyle, Obj.LineWidth, Obj.Hatching, Obj.IsClosed);
    finally
      PP.Free;
      EndUseProfilePoints;
    end;
  end;
end;

procedure T_PGF_Export.WriteBezier2D(Obj: TBezierPath2D0);
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
        Obj.LineStyle, Obj.LineWidth, Obj.Hatching, Obj.IsClosed);
    finally
      PP.Free;
      EndUseProfilePoints;
    end;
  end;
end;

procedure T_PGF_Export.WritePoly2D(Obj: TPolyline2D0);
begin
  with Obj do
    WritePath(Points, Points, WritePoly,
      Obj.LineColor, Obj.HatchColor, Obj.FillColor,
      Obj.LineStyle, Obj.LineWidth, Obj.Hatching, Obj.IsClosed);
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

procedure T_MetaPost_Export.WriteLineAttr(const LineStyle: TLineStyle; const
  LineWidth: TRealType;
  const LineColor: TColor);
begin
  if LineStyle <> liNone then
    WriteStream(Format(' withpen pencircle scaled %.2fmm',
      [fDrawing2D.LineWidthBase * LineWidth]));
  case LineStyle of
    liDotted:
      WriteStream(Format(' dashed dashpattern(on %.2fmm off %.2fmm)',
        [fDrawing2D.LineWidthBase * 2, fDrawing2D.DottedSize
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
    CalculateHatching(P, DX, DY, Step, Lines, fr_Winding);
    for I := 0 to Lines.Count div 2 - 1 do
    begin
      WriteStream('draw ');
      WriteStreamPoint(Lines[I * 2]);
      WriteStream('--');
      WriteStreamPoint(Lines[I * 2 + 1]);
      WriteStream(Format(' withpen pencircle scaled %.2fmm',
        [fDrawing2D.LineWidthBase * fDrawing2D.HatchingLineWidth]));
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
  const LineStyle: TLineStyle; const LineWidth: TRealType; const Hatching:
  THatching;
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
  if LineStyle = liNone then Exit;
  if FillColor = clDefault then PathProc(PP, Closed);
  WriteStream('draw pp');
  if Closed then WriteStream('--cycle');
  WriteLineAttr(LineStyle, LineWidth, LineColor);
  WriteLnStream(';');
end;

procedure T_MetaPost_Export.WritePoly0(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType; const Hatching:
  THatching;
  const Closed: Boolean);
begin
  WritePath(PP, PP, WritePoly,
    LineColor, HatchColor, FillColor,
    LineStyle, LineWidth, Hatching, Closed);
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
  if fDrawing2D.MiterLimit <> 10 then
    WriteLnStream(Format('miterlimit:=%.2g;', [fDrawing2D.MiterLimit]));
  WriteLnStream('path pp;');
  WriteStream('picture pic;');
  //WriteStream('bboxmargin := 0;');
  WriteStream('labeloffset := 0;');
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
      Obj.LineStyle, Obj.LineWidth, Obj.Hatching, True);
    EndUseProfilePoints;
  end;
end;

procedure T_MetaPost_Export.WriteText2D(Obj: TText2D);
var
  St, StRot: string;
  H, MaxDepth, MaxFontDepth, MaxHeight: TRealType;
  D: TVector2D;
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
      [Obj.Height * fFactorH {/ 1.2}])); //%.1fmm, Obj.Height * fFactorH
    if TeXText <> '' then St := TeXText else St := TeX_Replace_Special(Text);
    if not fDrawing2D.MetaPostTeXText then
    begin
      MaxDepth := 0;
      MaxHeight := 0;
      for I := 1 to Length(St) do
      begin
        D.Y := CMR_DP[Ord(St[I])];
        if D.Y > MaxDepth then MaxDepth := D.Y;
        D.Y := CMR_HT[Ord(St[I])];
        if D.Y > MaxHeight then MaxHeight := D.Y;
      end;
      case VJustification of
        jvBottom: D.Y := MaxFontDepth - MaxDepth;
        jvCenter: D.Y := {-(1 - MaxFontDepth) / 2 - MaxDepth}
          (MaxFontDepth - MaxDepth - 1 + MaxHeight) / 2;
        jvTop: D.Y := -1 + MaxHeight {-1 - MaxDepth};
        jvBaseline: D.Y := 0;
      end;
    end
    else
      case VJustification of
        jvBaseline: D.Y := 0;
        jvBottom: D.Y := -0.1 - 0.05;
        jvCenter: D.Y := 0 - 0.05;
        jvTop: D.Y := 0.1 - 0.05;
      end;
    D.X := 0;
    if Rot <> 0 then D := TransformVector2D(D, Rotate2D(Rot));
    D := TransformVector2D(D, Scale2D(Height, Height));
    WriteStream(Format('textX:=%.2fu;',
      [ConvertX(Points[0].X + D.X)]));
    WriteStream(Format('textY:=%.2fu;',
      [ConvertY(Points[0].Y + D.Y)]));
    WriteStream('pic := thelabel');
    case HJustification of
      jhLeft:
        case VJustification of
          jvBaseline: WriteStream('.urt');
          jvBottom {, jvCenter}: WriteStream('.urt');
          jvCenter: WriteStream('.rt');
          jvTop: WriteStream('.lrt');
        end;
      jhCenter:
        case VJustification of
          jvBaseline: WriteStream('.top');
          jvBottom {, jvCenter}: WriteStream('.top');
          jvCenter: ;
          jvTop: WriteStream('.bot');
        end;
      jhRight:
        case VJustification of
          jvBaseline: WriteStream('.ulft');
          jvBottom {, jvCenter}: WriteStream('.ulft');
          jvCenter: WriteStream('.lft');
          jvTop: WriteStream('.llft');
        end;
    end;
    if fDrawing2D.MetaPostTeXText then
    begin
      H := Obj.Height * fFactorH *
        fDrawing2D.PicUnitLength * 2.845; //in pt // mm=2.845pt
      if VJustification = jvBaseline then
        WriteStream(Format('(btex \raisebox{0pt}[0pt][0pt]{\fontsize{10}{12}\selectfont %s\strut} etex scaled %.2f,(0, 0));',
          [St, H / 10]))
      else
        WriteStream(Format('(btex \fontsize{10}{12}\selectfont %s\strut etex scaled %.2f,(0, 0));', //\frame{}
          [St, H / 10]))
          // \setlength{\baselineskip}{?pt}           \fontsize{12}{12}
    end
    else WriteStream(Format('("%s",(0, 0));', [Text]));
    if Rot <> 0 then
      StRot := Format(' rotatedaround((textX, textY),%.2f)', [RadToDeg(Rot)])
    else StRot := '';
    WriteLnStream(Format('draw pic shifted (textX, textY)%s', [StRot]));
    if Obj.LineColor <> clDefault
      then WriteColor(Obj.LineColor, clBlack);
    WriteLnStream(';');
    //WriteLnStream('pic := bbox pic;');
    //WriteLnStream(Format('draw bbox pic shifted (textX, textY)%s withcolor 0.6white;', [StRot]));
    //WriteLnStream('draw fullcircle scaled 1 shifted (textX, textY);');
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
      Obj.LineStyle, Obj.LineWidth, Obj.Hatching, IsClosed);
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
      Obj.LineStyle, Obj.LineWidth, Obj.Hatching, False);
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
      Obj.LineStyle, Obj.LineWidth, Obj.Hatching, IsClosed);
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
      Obj.LineStyle, Obj.LineWidth, Obj.Hatching, IsClosed);
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
      Obj.LineStyle, Obj.LineWidth, Obj.Hatching, IsClosed);
    EndUseProfilePoints;
  finally
    PP.Free;
  end;
end;

procedure T_MetaPost_Export.WriteBezier2D(Obj: TBezierPath2D0);
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
      Obj.LineStyle, Obj.LineWidth, Obj.Hatching, IsClosed);
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
      Obj.LineStyle, Obj.LineWidth, Obj.Hatching, IsClosed);
end;

procedure T_MetaPost_Export.WriteToTpX(Stream: TStream;
  const FileName: string);
begin
  fStream := nil;
  if not StoreToFile_MPS(fDrawing2D, ChangeFileExt(FileName, '.mps'))
    then Exit;
  fStream := Stream;
  try
    WriteLnStream(Format('  \includegraphics{%s%s.mps}',
      [fDrawing2D.IncludePath,
      ChangeFileExt(ExtractFileName(FileName), '')]));
  finally
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
  exitCode: DWORD;
  ExitCodeProcess: Longword;
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
      //WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
      repeat
        exitCode := WaitForSingleObject(ProcessInfo.hProcess, 100);
        Application.ProcessMessages;
        sleep(50);
      until exitCode <> WAIT_TIMEOUT;
      WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
      GetExitCodeProcess(ProcessInfo.hProcess, ExitCodeProcess);
      if ExitCodeProcess <> 0 then
      begin
        {Application.MessageBox(PChar('Exit code: ' + IntToStr(ExitCodeProcess)),
          'Error', MB_OK);}
        Result := False;
      end;
    end;
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
  finally
    if OutFile <> '' then FileClose(aOutput);
    if InFile <> '' then FileClose(aInput);
  end;
{  if Result then
    Application.MessageBox('App successful',
      'Error', MB_OK);}
end;

procedure OpenOrExec(const ViewerPath, FileName: string);
begin
  if ViewerPath = '' then
    ShellExecute(Application.Handle,
      PChar('open'), PChar(FileName),
      nil {PChar(Parameters)}, nil {PChar(Directory)}, SW_SHOW)
  else
    FileExec(Format('"%s" "%s"',
      [ViewerPath, FileName]), '', '',
      {IncludeTrailingPathDelimiter(ExtractFilePath(FileName))}'',
      False, False);
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

function StoreToFile_MPS(const Drawing: TDrawing2D;
  const FileName: string): Boolean;
var
  TempDir, TempMP, TempMPS, TempMPLog: string;
begin
  Result := False;
  if not FileExists(MetaPostPath) then
  begin
    Application.MessageBox('MetaPost path not found',
      'Error', MB_OK);
    Exit;
  end;
  TempDir := GetTempDir;
  TempMP := TempDir + '(pic)TpX.mp';
  TempMPS := ChangeFileExt(TempMP, '.0');
  TempMPLog := ChangeFileExt(TempMP, '.log');
  TryDeleteFile(TempMP);
  TryDeleteFile(TempMPS);
  TryDeleteFile(TempMPLog);
  StoreToFile_Saver(Drawing, TempMP, T_MetaPost_Export);
  try
    //if FileExists(FileName) then DeleteFile(PChar(FileName));
    //WinExec(PChar(MetaPostPath + ' "' + TempMP + '"'), 0);
    //Res := FileExec(MetaPostPath + ' "' + TempMP + '"',      '', '', '', False, True);
    Result := FileExec(MetaPostPath + ' -tex=latex "(pic)TpX.mp"', '', '',
      TempDir, False, True);
    if FileExists(TempMPS) then
    begin
      TryDeleteFile(FileName);
      TryDeleteFile(TempMPLog);
      if not RenameFile(TempMPS, FileName) then
      begin
        Application.MessageBox('Can not rename. MPS file not created',
          'Error', MB_OK);
        Result := False;
      end;
    end
    else
    begin
      if not FileExists(TempMPLog) then
        Application.MessageBox('MPS file not created',
          'Error', MB_OK)
      else if Application.MessageBox(
        'MPS file not created. Do you want to see log file?',
        'Error', MB_OKCANCEL) = idOK then
        OpenOrExec(TextViewerPath, TempMPLog);
      Result := False;
    end;
  finally
    if Result then TryDeleteFile(TempMP);
    TryDeleteFile(TempMPS);
    if Result then TryDeleteFile(TempMPLog);
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

procedure T_PDF_Export.WriteLineAttr(const LineStyle: TLineStyle; const
  LineWidth: TRealType;
  const LineColor: TColor);
var
  A: TRealType;
begin
  A := fFactorMM;
  with fDrawing2D do
    with fPDF.Canvas do
    begin
      case LineStyle of
        liNone, liSolid: SetDash([0], 0);
        liDashed: SetPDF_Dash(fPDF.Canvas,
            [DashSize * 2 * A, DashSize * A], 0);
        //SetDash([DashSize * 2 * A,            DashSize * A], 0);
        liDotted: SetPDF_Dash(fPDF.Canvas,
            [LineWidthBase * 2 * A, DottedSize * A], 0);
        //SetDash([LineWidth * 2 * A,            DottedSize * A], 0);
      end;
      if LineStyle = liNone then SetLineWidth(0)
      else SetLineWidth(LineWidthBase * LineWidth * A);
      if LineColor <> clDefault
        then SetRGBStrokeColor(LineColor)
      else SetRGBStrokeColor(0);
      if MiterLimit <> 10 then
        _WriteString(Format('%.2g M'#10, [MiterLimit]),
          fPDF.Canvas.Contents.Stream);
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
    CalculateHatching(P, DX, DY, Step, Lines, fr_Winding);
    with fPDF.Canvas do
    begin
      if HatchColor <> clDefault
        then SetRGBStrokeColor(HatchColor)
      else SetRGBStrokeColor(0);
      SetDash([0], 0);
      SetLineWidth(fDrawing2D.LineWidthBase * fDrawing2D.HatchingLineWidth * A);
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
  const LineStyle: TLineStyle; const LineWidth: TRealType; const Hatching:
  THatching;
  const Closed: Boolean);
begin
  if PP.Count < 1 then Exit;
  with fPDF.Canvas do
  begin
    if FillColor <> clDefault then
    begin
      SetRGBFillColor(FillColor);
      PathProc(PP, Closed);
      //Eofill; // even-odd aka alternate fill rule
      Fill; // nonzero winding fill rule
    end;
    if HatchPP <> nil then
      WriteHatching(HatchPP, Hatching, HatchColor, fHatchingStep);
    if LineStyle <> liNone then
    begin
      WriteLineAttr(LineStyle, LineWidth, LineColor);
      PathProc(PP, Closed);
      Stroke;
    end;
  end;
end;

procedure T_PDF_Export.WritePoly0(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType; const Hatching:
  THatching;
  const Closed: Boolean);
begin
  WritePath(PP, PP, WritePoly, LineColor, HatchColor, FillColor,
    LineStyle, LineWidth, Hatching, Closed);
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
  // compress PDF:
  fPDF.CompressionMethod := cmFlateDecode;
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
        Obj.LineStyle, Obj.LineWidth, Obj.Hatching, True);
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
        Obj.LineStyle, Obj.LineWidth, Obj.Hatching, IsClosed);
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
        Obj.LineStyle, Obj.LineWidth, Obj.Hatching, IsClosed);
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
        Obj.LineStyle, Obj.LineWidth, Obj.Hatching, IsClosed);
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
        Obj.LineStyle, Obj.LineWidth, Obj.Hatching, IsClosed);
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
        Obj.LineStyle, Obj.LineWidth, Obj.Hatching, IsClosed);
    finally
      PP.Free;
      EndUseProfilePoints;
    end;
  end;
end;

procedure T_PDF_Export.WriteBezier2D(Obj: TBezierPath2D0);
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
        Obj.LineStyle, Obj.LineWidth, Obj.Hatching, IsClosed);
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
      Obj.LineStyle, Obj.LineWidth, Obj.Hatching, IsClosed);
end;

procedure SetPDF_TextMatrix(PDF: TPdfCanvas;
  const T: TTransf2D);
var
  S: string;
begin
  S := _FloatToStrR(T[1, 1]) + ' ' +
    _FloatToStrR(T[1, 2]) + ' ' +
    _FloatToStrR(T[2, 1]) + ' ' +
    _FloatToStrR(T[2, 2]) + ' ' +
    _FloatToStrR(T[3, 1]) + ' ' +
    _FloatToStrR(T[3, 2]) + ' Tm'#10;
  _WriteString(S, PDF.Contents.Stream);
end;

procedure T_PDF_Export.WriteText2D(Obj: TText2D);
var
  HText: TRealType;
  D: TVector2D;
  P: TPoint2D;
  T: TTransf2D;
  fDescent: TRealType;
begin
  with Obj, fPDF.Canvas do
  begin
    P := Points[0];
    HText := Height * fFactorH;
    SetFont('Times-Roman', HText);
    //SetFont('Arial', HText);
    case HJustification of
      jhLeft: D.X := 0;
      jhCenter: D.X := -TextWidth(Text) / 2;
      jhRight: D.X := -TextWidth(Text);
    end;
    fDescent := 0.216; // - for Times-Roman 0.195??
    //fDescent := 0.212; // - for Arial
    //See PdfFonts: TIMES_DISC_INT_TABLE (KEY: 'Descent'; VAL: -216),
    case VJustification of
      jvBottom: D.Y := 0 + fDescent;
      jvCenter: D.Y := -0.5 + fDescent;
      jvTop: D.Y := -1 + fDescent;
      jvBaseline: D.Y := 0;
    end;
    D.Y := D.Y * HText;
    if Obj.LineColor <> clDefault then
      SetRGBFillColor(Obj.LineColor)
    else SetRGBFillColor(clBlack);
    if Rot <> 0 then
    begin
      T := Rotate2D(Rot);
      D := TransformVector2D(D, T);
    end;
    P := ShiftPoint(ConvertPnt(P), D);
    BeginText;
    if Rot = 0 then
      MoveTextPoint(P.X, P.Y)
    else
      SetPDF_TextMatrix(fPDF.Canvas,
        MultiplyTransform2D(T, Translate2D(P.X, P.Y)));
    ShowText(Text);
    EndText;
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
    Eo fill;
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
  if not StoreToFile(ChangeFileExt(FileName, '.pdf')) then
  begin
    fStream := Stream;
    Exit;
  end;
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
    if Closed and IsSamePoint2D(PP[0], PP[N1]) then Dec(N1);
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
    CalculateHatching(P, DX, DY, Step, Lines, fr_Winding);
    with fDrawing2D do
    begin
      for I := 0 to Lines.Count div 2 - 1 do
      begin
        if HatchColor = clDefault then Color := clBlack
        else Color := HatchColor;
        WriteLine(Lines[I * 2], Lines[I * 2 + 1],
          LineWidthBase * HatchingLineWidth * fFactorMM, Color);
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
  const LineStyle: TLineStyle; const LineWidth: TRealType; const Hatching:
  THatching;
  const Closed: Boolean);
var
  W: TRealType;
  TmpPoly: TPolygon32;
begin
  if PP.Count < 1 then Exit;
  {if (Obj.LineStyle <> liNone) or (Obj.FillColor <> clDefault)
    then FillPolygon(PP, Closed);}
  if FillColor <> clDefault then
  begin
    FillPolygon(PP, Closed);
    fPolygon.FillMode := pfWinding; //pfAlternate
    WriteBitmapPolygon(fPolygon, FillColor);
  end;
  if Hatching <> haNone then
    WriteHatching(PP, Hatching, HatchColor, fHatchingStep);
  if LineStyle = liNone then Exit;
  FillPolygon(PP, Closed);
  with fDrawing2D, fBitmap do
  begin
    case LineStyle of
      liSolid: TmpPoly := fPolygon.Outline;
      liDashed:
        TmpPoly := DashedOutline(DashSize * 2 * fFactorMM,
          DashSize * fFactorMM, fPolygon);
      liDotted:
        TmpPoly := DashedOutline(LineWidthBase * 2 * fFactorMM,
          DottedSize * fFactorMM, fPolygon)
    end;
    W := LineWidthBase * LineWidth * fFactorMM;
  end;
  try
    fOutline.Free;
    fOutline := TmpPoly.Grow(Fixed(W / 2),
      1 - 2 / Sqr(fDrawing2D.MiterLimit));
        //Link between MiterLimit and EdgeSharpness
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
        Obj.LineStyle, Obj.LineWidth, Obj.Hatching, True);
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
      Obj.LineStyle, Obj.LineWidth, Obj.Hatching, IsClosed);
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
      Obj.LineStyle, Obj.LineWidth, Obj.Hatching, IsClosed);
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
      Obj.LineStyle, Obj.LineWidth, Obj.Hatching, IsClosed);
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
      Obj.LineStyle, Obj.LineWidth, Obj.Hatching, IsClosed);
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
      Obj.LineStyle, Obj.LineWidth, Obj.Hatching, IsClosed);
    EndUseProfilePoints;
  end;
end;

procedure T_Bitmap0_Export.WriteBezier2D(Obj: TBezierPath2D0);
begin
  with Obj do
  begin
    BeginUseProfilePoints;
    WritePoly0(ProfilePoints,
      Obj.LineColor, Obj.HatchColor, Obj.FillColor,
      Obj.LineStyle, Obj.LineWidth, Obj.Hatching, IsClosed);
    EndUseProfilePoints;
  end;
end;

procedure T_Bitmap0_Export.WritePoly2D(Obj: TPolyline2D0);
begin
  with Obj do
    WritePoly0(Obj.Points,
      Obj.LineColor, Obj.HatchColor, Obj.FillColor,
      Obj.LineStyle, Obj.LineWidth, Obj.Hatching, Obj.IsClosed);
end;

procedure RenderTextW_Rot(const B0: TBitmap32;
  X, Y: Integer; const Text: Widestring; AALevel: Integer; Color: TColor32;
  const FontName: string; const FontSize: Double;
  const HJustification: THJustification;
  const VJustification: TVJustification;
  const Rot: Double);
var
  LogFont2: TLOGFONT;
  B, B2: TBitmap32;
  StockBitmap: TBitmap;
  SZ: TSize;
  Rect, BoundRect: TRect2D;
  D, V: TVector2D;
  P: TPoint2D;
  T: TTransf2D;
  Alpha: TColor32;
  StockCanvas: TCanvas;
  PaddedText: Widestring;
  ExtendedFont: TExtendedFont;
  Text_Metric: tagTEXTMETRIC;
  procedure TextBlueToAlpha(const B: TBitmap32; const Color:
    TColor32);
  var
    I: Integer;
    P: PColor32;
    C: TColor32;
  begin
  // convert blue channel to alpha and fill the color
    P := @B.Bits[0];
    for I := 0 to B.Width * B.Height - 1 do
    begin
      C := P^;
      if C <> 0 then
      begin
        C := P^ shl 24; // transfer blue channel to alpha
        C := C + Color;
        P^ := C;
      end;
      Inc(P);
    end;
  end;
  procedure TextScaleDown(const B, B2: TBitmap32; const N: Integer;
    const Color: TColor32); // use only the blue channel
  var
    I, J, X, Y, P, Q, SZ, S: Integer;
    Src: PColor32;
    Dst: PColor32;
  begin
    SZ := 1 shl N - 1;
    Dst := B.PixelPtr[0, 0];
    for J := 0 to B.Height - 1 do
    begin
      Y := J shl N;
      for I := 0 to B.Width - 1 do
      begin
        X := I shl N;
        S := 0;
        for Q := Y to Y + SZ do
        begin
          Src := B2.PixelPtr[X, Q];
          for P := X to X + SZ do
          begin
            S := S + Integer(Src^ and $000000FF);
            Inc(Src);
          end;
        end;
        S := S shr N shr N;
        Dst^ := TColor32(S shl 24) + Color;
        Inc(Dst);
      end;
    end;
  end;
begin
  if B0.Empty then Exit;
  Alpha := Color shr 24;
  Color := Color and $00FFFFFF;
  //AALevel := Constrain(AALevel, 0, 4);
  PaddedText := Text { + ' '};
  ExtendedFont := TExtendedFont.Create;
  ExtendedFont.Free;
  ExtendedFont := TExtendedFont.Create;

  B := TBitmap32.Create;
  try
    if AALevel = 0 then
    begin
{$IFDEF CLX}
      B.Font := Font;
      SZ := B.TextExtentW(PaddedText);
      B.SetSize(SZ.CX, SZ.CY);
{$ELSE}
      SZ := B0.TextExtentW(PaddedText);
      B.SetSize(SZ.CX, SZ.CY);
      B.Font := B0.Font;
{$ENDIF}
      B.Clear(0);
      B.Font.Color := clWhite;
      B.TextOut(0, 0, Text);
      TextBlueToAlpha(B, Color);
    end
    else
    begin
      StockBitmap := TBitmap.Create;
      StockBitmap.Width := 8;
      StockBitmap.Height := 8;
      StockCanvas := StockBitmap.Canvas;
      StockCanvas.Lock;
      try
        //StockCanvas.Font := B0.Font;
        //StockCanvas.Font.Size := B0.Font.Size shl AALevel;
        //StockCanvas.Font.Name := 'Times New Roman';
        //StockCanvas.Font.Height :=
        //  -Round(FontSize * (1 shl AALevel));
        ExtendedFont.Canvas := StockCanvas;
        ExtendedFont.FaceName := FontName;
        ExtendedFont.Height :=
          -Round(FontSize * (1 shl AALevel));
{$IFDEF CLX}
        SZ := StockCanvas.TextExtent(PaddedText);
{$ELSE}
        {Windows.GetTextExtentPoint32W(StockCanvas.Handle, PWideChar(PaddedText),
          Length(PaddedText), SZ);}
        SZ := StockCanvas.TextExtent(PaddedText);
{$ENDIF}
        GetTextMetrics(StockCanvas.Handle, Text_Metric);
        //SZ.CX := (SZ.CX shr AALevel + 1) shl AALevel;
        //SZ.CY := (SZ.CY shr AALevel + 1) shl AALevel;
        Rect := Rect2D(-Text_Metric.tmAveCharWidth, 0,
          SZ.CX + Text_Metric.tmAveCharWidth, -SZ.CY);
        case HJustification of
          jhLeft: V.X := 0;
          jhCenter: V.X := SZ.CX / 2;
          jhRight: V.X := SZ.CX;
        end;
        case VJustification of
          jvTop: V.Y := 0 - Text_Metric.tmInternalLeading;
          jvCenter: V.Y := -(SZ.CY + Text_Metric.tmInternalLeading) / 2 + 0.25;
          jvBottom: V.Y := -SZ.CY;
          jvBaseline: V.Y := -SZ.CY + Text_Metric.tmDescent + 0.5;
        end;
        //T := RotateCenter2D(Rot, Points[0]);
        T := Rotate2D(DegToRad(Rot / 10));
        BoundRect := TransformBoundingBox2D(Rect, T);
        BoundRect.Left := BoundRect.Left - Text_Metric.tmAveCharWidth;
        BoundRect.Right := BoundRect.Right + Text_Metric.tmAveCharWidth;
        V := TransformVector2D(V, T);
        SZ.CX := Round(BoundRect.Right - BoundRect.Left);
        SZ.CY := Round(BoundRect.Top - BoundRect.Bottom);
        D.X := Max(-BoundRect.Left, 0);
        D.Y := Max(BoundRect.Top, 0);
        B2 := TBitmap32.Create;
        try
          //StockCanvas.Font.Size := B0.Font.Size shl AALevel;
          B2.SetSize(SZ.CX, SZ.CY);
          B2.Clear(0);
          B2.Font := StockCanvas.Font;
          B2.Font.Size := B0.Font.Size shl AALevel;
          B2.Font.Color := clWhite;
          {GetObject(GetStockObject(DEFAULT_GUI_FONT), SizeOf(LogFont2),
            @LogFont2);
          LogFont2.lfFaceName := 'Times New Roman';}
          LogFont2 := ExtendedFont.LogFont;
          LogFont2.lfEscapement := Round(Rot);
          LogFont2.lfOrientation := LogFont2.lfEscapement;
          //LogFont2.lfHeight := -Round(FontSize * (1 shl AALevel));
          //Windows.SetTextAlign(B2.Canvas.Handle, TA_LEFT + TA_BOTTOM);
          B2.Font.Handle := CreateFontIndirect(LogFont2);
          //B2.TextoutW(0, 0, Text);
          {B2.Canvas.Pen.Color := clWhite;
          B2.Canvas.Pen.Width := 5;
          B2.Canvas.MoveTo(1, 1);
          B2.Canvas.LineTo(B2.Width - 2, 1);
          B2.Canvas.LineTo(B2.Width - 2, B2.Height - 2);
          B2.Canvas.LineTo(1, B2.Height - 2);
          B2.Canvas.LineTo(1, 1);}
          B2.TextoutW(Round(D.X), Round(D.Y), Text);
          //B2.SaveToFile('C:\WRK\-.bmp');
          B2.StretchFilter := sfLinear;
          B.SetSize(SZ.CX shr AALevel, SZ.CY shr AALevel);
          TextScaleDown(B, B2, AALevel, Color);
        finally
          B2.Free;
        end;
      finally
        StockCanvas.Unlock;
        StockBitmap.Free;
      end;
    end;

    B.DrawMode := dmBlend;
    B.MasterAlpha := Alpha;

    B.DrawTo(B0, X - (Round(D.X + V.X { + Text_Metric.tmAveCharWidth}) shr
      AALevel + 1),
      Y - (Round(D.Y - V.Y) shr AALevel + 1));
    {B0.Canvas.Pen.Width := 3;
    B0.Canvas.Pen.Color := clBlue;
    B0.Canvas.MoveTo(X - 3, Y - 3);
    B0.Canvas.LineTo(X - 3, Y + 3);
    B0.Canvas.LineTo(X + 3, Y + 3);
    B0.Canvas.LineTo(X + 3, Y - 3);
    B0.Canvas.LineTo(X - 3, Y - 3);}
  finally
    B.Free;
    //ExtendedFont.Canvas := nil;
    //ExtendedFont.Free;
  end;
end;

procedure T_Bitmap0_Export.WriteText2D(Obj: TText2D);
var
  FontH: TRealType;
  P: TPoint2D;
  FontName: string;
begin
  with Obj do
  begin
    fBitmap.PenColor := clBlack32;
    if Obj.LineColor <> clDefault then
      fBitmap.Font.Color := Obj.LineColor
    else
      fBitmap.Font.Color := clBlack;
    if fDrawing2D.FontName <> '' then FontName := fDrawing2D.FontName
    else if FontName_Default <> '' then FontName := FontName_Default
    else FontName := 'Times New Roman';
    FontH := Height * fFactorH;
    fBitmap.Font.Height := Round(FontH);
    P := Points[0];
    RenderTextW_Rot(fBitmap,
      Round(ConvertX(P.X)),
      Round(ConvertY(P.Y)), Text, 2 {0-4},
      Color32(fBitmap.Font.Color), FontName, Round(FontH),
      Obj.HJustification, Obj.VJustification,
      Round(RadToDeg(Rot) * 10));
  end;
end;

procedure T_Bitmap0_Export.WriteAllToStream;
begin
  if fStream = nil then Exit;
  WriteAll;
  fBitmap.SaveToStream(fStream);
end;

function T_Bitmap0_Export.StoreToFile(const FileName: string):
  Boolean;
var
  aBitmap: TBitmap;
begin
  WriteAll;
  aBitmap := TBitmap.Create;
  Result := False;
  try
    aBitmap.Assign(fBitmap);
    aBitmap.PixelFormat := pf24bit;
    aBitmap.SaveToFile(FileName);
  finally
    aBitmap.Free;
    Result := FileExists(FileName);
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
  if not StoreToFile(ChangeFileExt(FileName, '.bmp')) then
  begin
    fStream := Stream;
    Exit;
  end;
  fStream := Stream;
  try
    WriteLnStream(Format(
      '  \includegraphics[width=%.3fcm,height=%.3fcm]{%s%s.bmp}',
      [fW_MM / 10, fH_MM / 10, fDrawing2D.IncludePath,
      ChangeFileExt(ExtractFileName(FileName), '')]));
//\item[natwidth,natheight] Again an alternative to |bb|.
// |natheight=h,natwidth=w| is equivalent to |bb = 0 0 h w|.
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
  if not StoreToFile(ChangeFileExt(FileName, '.png')) then
  begin
    fStream := Stream;
    Exit;
  end;
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

function T_PNG_Export.StoreToFile(const FileName: string):
  Boolean;
var
  aPNG: TPNGObject;
  aBitmap: TBitmap;
begin
  WriteAll;
  aPNG := TPNGObject.Create;
  aBitmap := TBitmap.Create;
  Result := False;
  try
    //aBitmap.PixelFormat := pf32bit;
    aBitmap.Assign(fBitmap);
    aPNG.Assign(aBitmap);
    aPNG.SaveToFile(FileName);
  finally
    aPNG.Free;
    aBitmap.Free;
    Result := FileExists(FileName);
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

function T_EMF_Export.StoreToFile(const FileName: string):
  Boolean;
begin
  Result := fDrawing2D.SaveToFile_EMF(FileName);
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
  Result := TLine2D.CreateSpec(0,
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
  ReadPrimitiveAttr(Result, XMLNode);
end;

procedure AddPoints(var Primitive: TPrimitive2D; Text: string; Pnts:
  TPointsSet2D);
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
  if Pnts.Count = 0 then FreeAndNil(Primitive);
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
      Result := TClosedBSpline2D.CreateSpec(0, [Point2D(0, 0)])
    else Result := TBSpline2D.CreateSpec(0, [Point2D(0, 0)])
  else
    if IsClosed then
      Result := TClosedCubicBSpline2D.CreateSpec(0, [Point2D(0, 0)])
    else Result := TCubicBSpline2D.CreateSpec(0, [Point2D(0, 0)]);
  AddPoints(Result, XMLNode.Text, (Result as TSpline2D0).Points);
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
    Result := TClosedSmoothPath2D.CreateSpec(0, [Point2D(0, 0)])
  else Result := TSmoothPath2D.CreateSpec(0, [Point2D(0, 0)]);
  AddPoints(Result, XMLNode.Text, (Result as TSmoothPath2D0).Points);
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
  AddPoints(Result, XMLNode.Text, (Result as TBezierPath2D0).Points);
  ReadPrimitiveAttr(Result, XMLNode);
end;

function T_TpX_Loader.ReadPolygon(XMLNode: TXMLDElement):
  TPrimitive2D;
begin
  Result := TPolygon2D.CreateSpec(0, [Point2D(0, 0)]);
  AddPoints(Result, XMLNode.Text, (Result as TPolygon2D).Points);
  ReadPrimitiveAttr(Result, XMLNode);
end;

function T_TpX_Loader.ReadPolyline(XMLNode: TXMLDElement):
  TPrimitive2D;
begin
  Result := TPolyline2D.CreateSpec(0, [Point2D(0, 0)]);
  AddPoints(Result, XMLNode.Text, (Result as TPolyline2D).Points);
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
  Result := TStar2D.CreateSpec(0, CP);
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
  else if (ID = 'bezier') then Result := ReadBezier(XMLNode)
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
    if fVersion > 2 then
      Application.MessageBox(
        PChar(Format('TpX file version is newer then TpX program can handle. Please, update the program',
        [Font_pfb_Path])),
        'Error', MB_OK);
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

procedure Import_MetafileFromClipboard(const Drawing: TDrawing2D);
var
  EMF_Loader: T_EMF_Loader;
  MF: TMetaFile;
  TpX_Loader: T_TpX_Loader;
begin
  if not Clipboard.HasFormat(CF_METAFILEPICT) then Exit;
  MF := TMetaFile.Create;
  EMF_Loader := T_EMF_Loader.Create;
  TpX_Loader := T_TpX_Loader.Create(Drawing);
  try
    MF.Assign(Clipboard);
    EMF_Loader.LoadFromMF(MF);
    EMF_Loader.FillXML(TpX_Loader.XMLDoc);
    TpX_Loader.XMLDoc.Save('--.TpX');
    TpX_Loader.ReadAll;
  finally
    MF.Free;
    EMF_Loader.Free;
    TpX_Loader.Free;
  end;
end;

procedure PasteMetafileFromClipboard(const Drawing: TDrawing2D);
var
  TmpDrawing: TDrawing2D;
  Stream: TMemoryStream;
begin
  TmpDrawing := TDrawing2D.Create(nil);
  Stream := TMemoryStream.Create;
  try
    Import_MetafileFromClipboard(TmpDrawing);
    TmpDrawing.SaveObjectsToStream(Stream);
    Stream.Position := 0;
    Drawing.LoadObjectsFromStream(Stream, Drawing.Version);
  finally
    TmpDrawing.Free;
    Stream.Free;
  end;
end;

procedure Import_Eps(const Drawing: TDrawing2D; const EpsFileName: string);
var
  TempDir, TempEMF: string;
  Res: Boolean;
  //emf wemf wemfc wemfnss
begin
  if not FileExists(PsToEditPath) then
  begin
    Application.MessageBox('PsToEdit path not found',
      'Error', MB_OK);
    Exit;
  end;
  TempDir := GetTempDir;
  TempEMF := TempDir + '(pic)TpX.emf';
  TryDeleteFile(TempEMF);
  try
    Res := FileExec(Format('"%s" "%s" "%s" -f %s',
      [PsToEditPath, EpsFileName, TempEMF, PsToEditFormat]), '', '',
      TempDir, False, True);
    if not FileExists(TempEMF) then
      Application.MessageBox('EMF file not created',
        'Error', MB_OK)
    else
    begin
      Import_Metafile(Drawing, TempEMF, nil);
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


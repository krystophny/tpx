unit Output;

{$IFNDEF VER140}
{$MODE Delphi}
{$DEF LAZ_POWERPDF}
{$ENDIF}

interface

uses SysUtils, Classes, Graphics, Drawings, GObjects,
{$IFDEF VER140}
  Gr32, Gr32_Polygons, WinBasic,
{$ENDIF}
  Geometry, Pieces, Devices;

type

  TDrawingSaver = class(TObject)
  protected
    fDrawing2D: TDrawing2D;
    fStream: TStream;
    fW_MM, fH_MM, fExtLeft, fExtBottom, fExtTop,
      fFactorT, // world to device
      fFactorMM: // mm to device
    TRealType;
    fExtRect: TRect2D;
    fDvipsFixBB: Boolean;
    procedure MeasureDrawing;
    procedure WriteStream(Value: Variant);
    procedure WriteLnStream(Value: Variant);
    procedure WriteStreamPoint0(const X, Y: TRealType); virtual;
    procedure WriteStreamPoint0Int(X, Y: Integer);
    function ConvertX(X: TRealType): TRealType; virtual;
    function ConvertY(Y: TRealType): TRealType; virtual;
    function ConvertPnt(Pnt: TPoint2D): TPoint2D;
    procedure WriteStreamPoint(Pnt: TPoint2D); virtual;
    procedure WritePoly0(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType; const
      Hatching:
      THatching;
      const Closed: Boolean); virtual;
    procedure WritePolyBezier0(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType; const
      Hatching:
      THatching;
      const Closed: Boolean); virtual;
    procedure WritePiece(Piece: TPiece; Obj: TPrimitive2D);
      virtual;
    procedure WritePieces(Obj: TPrimitive2D); virtual;
    procedure WriteContainer2D(Obj: TContainer2D); virtual;
    procedure WriteLine2D(Obj: TLine2D); virtual;
    procedure WriteRectangle2D(Obj: TRectangle2D); virtual;
      abstract;
    procedure WriteEllipse2D(Obj: TEllipse2D); virtual;
      abstract;
    procedure WriteCircle2D(Obj: TCircle2D); virtual;
      abstract;
    procedure WriteCircular2D(Obj: TCircular2D); virtual;
      abstract;
    procedure WritePoly2D(Obj: TPolyline2D0); virtual;
    procedure WriteText2D(Obj: TText2D); virtual;
      abstract;
    procedure WriteStar2D(Obj: TStar2D); virtual;
    procedure WriteSymbol2D(Obj: TSymbol2D); virtual;
    procedure WriteSmooth2D(Obj: TSmoothPath2D0); virtual;
    procedure WriteBezier2D(Obj: TBezierPath2D0); virtual;
    procedure WriteEntity(Obj: TObject2D);
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

  TDrawingSaverClass = class of TDrawingSaver;

  T_Device_Export = class(TDrawingSaver)
  protected
    fDevice: TDevice;
    function GetTransform: TTransf2D; virtual;
    procedure WritePiece(Piece: TPiece; Obj: TPrimitive2D);
      override;
    procedure WriteContainer2D(Obj: TContainer2D); override;
//    procedure WriteLine2D(Obj: TLine2D); override;
    procedure WriteRectangle2D(Obj: TRectangle2D); override;
    procedure WriteEllipse2D(Obj: TEllipse2D); override;
    procedure WriteCircle2D(Obj: TCircle2D); override;
    procedure WriteCircular2D(Obj: TCircular2D); override;
    procedure WriteText2D(Obj: TText2D); override;
  public
    constructor Create(Drawing: TDrawing2D); override;
    procedure WriteHeader; override;
    procedure WriteFooter; override;
    procedure WriteAllToStream; override;
  end;

  T_SVG_Export = class(T_Device_Export)
  protected
    function GetTransform: TTransf2D; override;
  public
    constructor Create(Drawing: TDrawing2D); override;
    destructor Destroy; override;
    procedure WriteHeader; override;
  end;

  T_TeX_Picture_Export = class(T_Device_Export)
  protected
    function GetTransform: TTransf2D; override;
  public
    constructor Create(Drawing: TDrawing2D); override;
    destructor Destroy; override;
    procedure WriteHeader; override;
  end;

  T_TikZ_Export = class(T_Device_Export)
  protected
    function GetTransform: TTransf2D; override;
  public
    constructor Create(Drawing: TDrawing2D); override;
    destructor Destroy; override;
    procedure WriteHeader; override;
  end;

  T_PGF_Export = class(T_Device_Export)
  protected
    function GetTransform: TTransf2D; override;
  public
    constructor Create(Drawing: TDrawing2D); override;
    destructor Destroy; override;
    procedure WriteHeader; override;
  end;

  T_None_Saver = class(TDrawingSaver)
  public
    procedure WriteAll; override;
    procedure WriteAllToStream; override;
  end;

  T_PostScript_Export = class(T_Device_Export)
  protected
    function GetTransform: TTransf2D; override;
  public
    constructor Create(Drawing: TDrawing2D); override;
    destructor Destroy; override;
    procedure WriteHeader; override;
    procedure WriteToTpX(Stream: TStream;
      const FileName: string); override;
  end;

  T_PostScript_Light_Export = class(T_PostScript_Export)
  // Export without text and font
  protected
  public
    constructor Create(Drawing: TDrawing2D); override;
  end;

  T_EpsToPdf_Export = class(T_PostScript_Export)
  public
    constructor Create(Drawing: TDrawing2D); override;
    function StoreToFile(const FileName: string): Boolean;
      override;
  end;

  T_EpsToPdf_Light_Export = class(T_EpsToPdf_Export)
  // uses "light" Postscript (without text):
  protected
  public
    constructor Create(Drawing: TDrawing2D); override;
  end;

  T_PSTricks_Export = class(T_Device_Export)
  protected
    function GetTransform: TTransf2D; override;
  public
    constructor Create(Drawing: TDrawing2D); override;
    destructor Destroy; override;
    procedure WriteHeader; override;
  end;

  T_MetaPost_Export = class(T_Device_Export)
  protected
    function GetTransform: TTransf2D; override;
  public
    constructor Create(Drawing: TDrawing2D); override;
    destructor Destroy; override;
    procedure WriteHeader; override;
    procedure WriteToTpX(Stream: TStream;
      const FileName: string); override;
  end;

  T_PDF_Export = class(T_Device_Export)
  protected
    function GetTransform: TTransf2D; override;
  public
    constructor Create(Drawing: TDrawing2D); override;
    destructor Destroy; override;
    procedure WriteHeader; override;
    procedure WriteToTpX(Stream: TStream;
      const FileName: string); override;
  end;

  T_PDF_Light_Export = class(T_PDF_Export)
  // PDF export without text
  protected
  public
    constructor Create(Drawing: TDrawing2D); override;
  end;

{$IFDEF VER140}
  T_Bitmap0_Export = class(T_Device_Export)
  protected
    function GetTransform: TTransf2D; override;
  public
    constructor Create(Drawing: TDrawing2D); override;
    destructor Destroy; override;
    procedure WriteHeader; override;
    function StoreToFile(const FileName: string): Boolean;
      override;
  end;

  T_BMP_Export = class(T_Bitmap0_Export)
    procedure WriteToTpX(Stream: TStream;
      const FileName: string); override;
  end;

  T_PNG_Export = class(T_Bitmap0_Export)
  public
    procedure WriteToTpX(Stream: TStream;
      const FileName: string); override;
    function StoreToFile(const FileName: string): Boolean;
      override;
  end;

  T_EMF_Dvc_Export = class(T_Device_Export)
  protected
    fMetaFile: Graphics.TMetaFile;
    function GetTransform: TTransf2D; override;
  public
    constructor CreateWithMF(const Drawing: TDrawing2D;
      const AMetaFile: Graphics.TMetaFile);
    destructor Destroy; override;
    procedure WriteHeader; override;
  end;


  T_EMF_Export = class(TDrawingSaver)
  public
    procedure WriteToTpX(Stream: TStream;
      const FileName: string); override;
    function StoreToFile(const FileName: string): Boolean;
      override;
  end;
{$ENDIF}

const
  EOL = #13#10;

type
  T_FM_Arr = array[0..255] of TRealType;

procedure ExportToFile(const Drawing: TDrawing2D;
  const FileName: string; const ExportFormat: ExportFormatKind);
procedure StoreToFile_Saver(const Drawing: TDrawing2D;
  const FileName: string; AClass: TDrawingSaverClass);
// A hack to make dvips use a more correct bounding box
function DvipsFixBB_RuleStr(const ExtRect: TRect2D;
  const FactorMM: TRealType): string;
procedure StoreToFile_TpX0(const Drawing: TDrawing2D;
  const FileName: string;
  AClass_TeX, AClass_PdfTeX: TDrawingSaverClass;
  const DvipsFixBB: Boolean);
procedure StoreToFile_TpX(const Drawing: TDrawing2D;
  const FileName: string;
  const DvipsFixBB: Boolean);
function StoreToFile_MPS(const Drawing: TDrawing2D;
  const FileName: string): Boolean;
procedure StoreToFile_LaTeX_EPS(const Drawing: TDrawing2D;
  const FileName: string);
procedure StoreToFile_LaTeX_PDF(const Drawing: TDrawing2D;
  const FileName: string);
procedure LaTeX_Custom_Parse(var Device, Ext, Ext2: string);
procedure StoreToFile_LaTeX_Custom(const Drawing: TDrawing2D;
  const FileName: string);
procedure SetOutputFormats(const OutputFormats: string;
  ADrawing: TDrawing2D);
function CheckCommandLine: Boolean;
procedure ParseParameters(
  var FileName, IncludePath, OutputFormats: string);
procedure FindPicturePath(const TeXFileName: string; const Line:
  Integer;
  var PicturePath, IncludePath: string);
function TeX_Replace_Special(const St: WideString): string;
function GetTeXTextFontSize(
  Height, UnitLength: TRealType): string;
function GetTeXTextFontColor(LineColor: TColor): string;
function Get_TeXText(
  const LineColor: TColor; const Style: TFontStyles;
  const WideText: WideString; const TeXText: AnsiString): string;
function GetTeXTextMakebox0(
  const Height, ARot, UnitLength: TRealType;
  const LineColor: TColor; const HJustification: THJustification;
  const VJustification: TVJustification; const Style: TFontStyles;
  const WideText: WideString; const TeXText: AnsiString): string;
procedure ShiftTeXTextPoint(var P: TPoint2D;
  VJustification: TVJustification; Height, Rot: TRealType);
{$IFDEF VER140}
function DrawingAsMetafile(Drawing: TDrawing2D): TMetaFile;
{$ENDIF}
procedure StoreToFile_EMF(const Drawing: TDrawing2D;
  const FileName: string);

var
  MetaPostPath: string = 'mpost.exe';
  Font_pfb_Path: string = '';
  GhostscriptCustomKeys: string = '-r300 -sDEVICE=png256';
    //pdfwrite, bmp256

implementation

uses Math, StrUtils, ColorEtc, PreView,
  Settings0, Input, SysBasic,
{$IFDEF VER140}
  pngimage, Gr32Add, DevGr32,
{$ENDIF}
  MiscUtils, DevPGF, DevTeXPc, TpXSaver,
  DevCanvas, DevSVG, DevTikZ, DevPS, DevPDF, DevPSTr, DevMP;

function GetColor(C, Default: TColor): TColor;
begin
  if C = clDefault then
    Result := Default
  else
    Result := C;
end;

{ --================ TDrawingSaver ==================-- }

constructor TDrawingSaver.Create(Drawing: TDrawing2D);
begin
  inherited Create;
  fDrawing2D := Drawing;
  fStream := nil;
  fDvipsFixBB := False;
end;

procedure TDrawingSaver.MeasureDrawing;
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
    fFactorT := S * fFactorMM;
    fExtLeft := Left - B / S;
    fExtBottom := Bottom - B / S;
    fExtTop := Top + B / S;
    fW_MM := (Right - Left) * S + B * 2;
    fH_MM := (Top - Bottom) * S + B * 2;
    fExtRect := EnlargeBoxDelta2D(ARect, B / S);
  end;
end;

procedure TDrawingSaver.WriteStream(Value: Variant);
var
  ValueSt: string;
begin
  ValueSt := Value;
  if ValueSt = '' then Exit;
  fStream.Write(ValueSt[1], Length(ValueSt));
end;

procedure TDrawingSaver.WriteLnStream(Value: Variant);
var
  ValueSt: string;
begin
  ValueSt := Value;
  ValueSt := ValueSt + EOL;
  fStream.Write(ValueSt[1], Length(ValueSt));
end;

procedure TDrawingSaver.WriteStreamPoint0(const X, Y: TRealType);
begin
  WriteStream('(');
  WriteStream(X);
  WriteStream(',');
  WriteStream(Y);
  WriteStream(')');
end;

procedure TDrawingSaver.WriteStreamPoint0Int(X, Y: Integer);
begin
  WriteStream('(');
  WriteStream(X);
  WriteStream(',');
  WriteStream(Y);
  WriteStream(')');
end;

function TDrawingSaver.ConvertX(X: TRealType): TRealType;
begin
  Result := (X - fExtLeft) * fFactorT;
end;

function TDrawingSaver.ConvertY(Y: TRealType): TRealType;
begin
  Result := (Y - fExtBottom) * fFactorT;
end;

function TDrawingSaver.ConvertPnt(Pnt: TPoint2D):
  TPoint2D;
begin
  Result := Point2D(ConvertX(Pnt.X), ConvertY(Pnt.Y));
end;

procedure TDrawingSaver.WriteStreamPoint(Pnt: TPoint2D);
var
  Point: TPoint2D;
begin
  Point := ConvertPnt(Pnt);
  WriteStreamPoint0(Point.X, Point.Y);
end;

procedure TDrawingSaver.WritePoly0(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Closed: Boolean);
begin

end;

procedure TDrawingSaver.WritePolyBezier0(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Closed: Boolean);
begin

end;

procedure TDrawingSaver.WritePiece(Piece: TPiece;
  Obj: TPrimitive2D);
var
  Circle: TCircle2D;
begin
  if Piece.Count < 1 then Exit;
  if Piece is TLinPath then
    WritePoly0(Piece, Piece.GetLineColor(Obj), Obj.HatchColor,
      Piece.GetFillColor(Obj), Piece.GetLineStyle(Obj),
      Piece.GetLineWidth(Obj), Piece.GetHatching(Obj),
      (Piece as TLinPath).Closed)
  else if Piece is TBezierPath then
    WritePolyBezier0(Piece, Piece.GetLineColor(Obj),
      Obj.HatchColor,
      Piece.GetFillColor(Obj), Piece.GetLineStyle(Obj),
      Piece.GetLineWidth(Obj), Piece.GetHatching(Obj),
      (Piece as TBezierPath).Closed)
  else if Piece is TCirclePiece then
  begin
    Circle := TCircle2D.Create(-1);
    Circle.Assign(Obj);
    Circle.LineColor := Piece.GetLineColor(Obj);
    Circle.HatchColor := Piece.GetHatchColor(Obj);
    Circle.FillColor := Piece.GetFillColor(Obj);
    Circle.LineStyle := Piece.GetLineStyle(Obj);
    Circle.LineWidth := Piece.GetLineWidth(Obj);
    Circle.Hatching := Piece.GetHatching(Obj);
    WriteCircle2D(Circle);
    Circle.Free;
  end;
end;

procedure TDrawingSaver.WritePieces(Obj: TPrimitive2D);
var
  I: Integer;
begin
  if Obj.Pieces = nil then Exit;
  for I := 0 to Obj.Pieces.Count - 1 do
    if Obj.Pieces[I] <> nil then
      WritePiece(Obj.Pieces[I], Obj);
end;

procedure TDrawingSaver.WriteEntities;
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
      try
        WriteEntity(TmpObj);
        TmpObj := TmpIter.Next as TObject2D;
        Inc(I);
        if I mod 100 = 0 then ShowProgress(I / TmpIter.Count);
      except
        MessageBoxError(TmpObj.ClassName);
      end;
    end;
  finally
    TmpIter.Free;
  end;
end;

procedure TDrawingSaver.WriteEntity(Obj: TObject2D);
begin
  if Obj is TContainer2D then
    WriteContainer2D(Obj as TContainer2D)
  else if Obj is TLine2D then
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
  else if Obj is TSymbol2D then
  begin
    WriteSymbol2D(Obj as TSymbol2D);
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

procedure TDrawingSaver.WriteContainer2D(Obj: TContainer2D);
begin
end;

procedure TDrawingSaver.WriteLine2D(Obj: TLine2D);
begin
  WritePieces(Obj);
end;

procedure TDrawingSaver.WritePoly2D(Obj: TPolyline2D0);
begin
  WritePieces(Obj);
end;

procedure TDrawingSaver.WriteBezier2D(Obj: TBezierPath2D0);
begin
  WritePieces(Obj);
end;

procedure TDrawingSaver.WriteSmooth2D(Obj: TSmoothPath2D0);
begin
  WritePieces(Obj);
end;

procedure TDrawingSaver.WriteStar2D(Obj: TStar2D);
begin
  WritePieces(Obj);
end;

procedure TDrawingSaver.WriteSymbol2D(Obj: TSymbol2D);
begin
  WritePieces(Obj);
end;

function FF(const X: TRealType): string;
begin
  Result := Format('%.6g', [X]);
end;

function TeX_Replace_Special(const St: WideString): string;
// TeX special characters # \# $ \$ % \% & \& ~ \verb _ \_ ^ \^ \ $\backslash$ { \{ } \}
begin
  Result := St;
  Result := AnsiReplaceStr(Result, '\', '\backs-lash');
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

function GetTeXTextFontSize(
  Height, UnitLength: TRealType): string;
var
  H: TRealType;
begin
  H := Height * UnitLength * 2.84527559; //in pt // mm=2.84527559pt
    // TeX uses small points (1 inch = 72.27pt),
    // not big points (1 inch = 72bp) as Adobe PS/PDF/SVG
  Result := Format('\fontsize{%.2f}{%.2f}\selectfont ',
    [H, H * 1.2]);
end;

function GetTeXTextFontColor(LineColor: TColor): string;
var
  RGB: T_PS_RGB;
begin
  if LineColor <> clDefault then
  begin
    RGB := PS_RGB(LineColor);
    Result := Result + Format('\textcolor[rgb]{%.5g, %.5g, %.5g}',
      [RGB.R, RGB.G, RGB.B]);
  end;
end;

function Get_TeXText(
  const LineColor: TColor; const Style: TFontStyles;
  const WideText: WideString; const TeXText: AnsiString): string;
begin
  if TeXText <> '' then
    Result := TeXText
  else
    Result := TeX_Replace_Special(WideText);
  if LineColor <> clDefault then
    Result := GetTeXTextFontColor(LineColor) + '{' + Result + '}';
  if fsBold in Style then Result := '\textbf{' + Result + '}';
  if fsItalic in Style then Result := '\textit{' + Result + '}';
end;

function GetTeXTextMakebox0(
  const Height, ARot, UnitLength: TRealType;
  const LineColor: TColor; const HJustification: THJustification;
  const VJustification: TVJustification; const Style: TFontStyles;
  const WideText: WideString; const TeXText: AnsiString): string;
var
  St: string;
begin
  Result := GetTeXTextFontSize(Height, UnitLength);
  St := Get_TeXText(LineColor, Style, WideText, TeXText);
  if VJustification <> jvBaseline then
  begin
    Result := Result + '\makebox(0,0)[';
    case HJustification of
      jhLeft: Result := Result + 'l';
      jhCenter: Result := Result + '';
      jhRight: Result := Result + 'r';
    end;
    case VJustification of
      jvBottom: Result := Result + 'b';
      jvCenter: Result := Result + '';
      jvTop: Result := Result + 't';
    end;
    Result := Result + ']{' + St + '\strut}';
  end
  else
  begin
    if ARot <> 0 then Result := Result + '\smash{';
    case HJustification of
      jhLeft:
        if ARot = 0 then
          Result := Result + St
        else
          Result := Result + '\makebox[0pt][l]{' + St + '}';
      jhCenter: Result := Result + '\makebox[0pt]{' + St + '}';
      jhRight: Result := Result + '\makebox[0pt][r]{' + St + '}';
    end;
    if ARot <> 0 then Result := Result + '}';
  end;
  if ARot = 0 then
  else
  begin
    Result :=
      Format('\rotatebox{%.2f}{%s}', //\frame{} (\fbox{}-adds space)
      [RadToDeg(ARot), Result]);
  end;
end;

procedure ShiftTeXTextPoint(var P: TPoint2D;
  VJustification: TVJustification; Height, Rot: TRealType);
var
  D: TVector2D;
  R: TRealType;
begin
  case VJustification of
    jvBottom: R := (-0.1);
    jvCenter: R := (-0.0);
    jvTop: R := (+0.1);
    jvBaseline: R := 0;
  end;
  //R := R - 0.03;
  D := PolarVector(Height * R, Rot + Pi / 2);
  P := ShiftPoint(P, D);
end;

function TDrawingSaver.GetFontDescent: TRealType;
begin
  Result := 0.2;
end;

procedure TDrawingSaver.WriteHeader;
begin

end;

procedure TDrawingSaver.WriteFooter;
begin

end;

procedure TDrawingSaver.WriteAll0;
begin
  if fDrawing2D.PicScale <= 0
    then fDrawing2D.PicScale := 1;
  WriteHeader;
  WriteEntities;
  WriteFooter;
end;

procedure TDrawingSaver.WriteAll;
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

procedure TDrawingSaver.WriteAllToStream;
begin
  if fStream = nil then Exit;
  WriteAll;
end;

procedure TDrawingSaver.WriteToTpX(Stream: TStream;
  const FileName: string);
var
  Stream0: TStream;
begin
  Stream0 := fStream;
  fStream := Stream;
  WriteAllToStream;
  fStream := Stream0;
end;

function TDrawingSaver.StoreToFile(const FileName: string):
  Boolean;
//var  FileStream : TFileStream;
begin
  fStream.Free;
  fStream := TFileStream.Create(FileName, fmCreate);
  //FileStream := TFileStream.Create(FileName, fmCreate);
  //fStream := TMemoryStream.Create;
  Result := False;
  try
    WriteAllToStream;
    //fStream.Position := 0;
    //FileStream.CopyFrom(fStream,fStream.Size);
  finally
    fStream.Free;
    //FileStream.Free;
    fStream := nil;
    Result := FileExists(FileName);
  end;
end;

procedure ExportToFile(const Drawing: TDrawing2D;
  const FileName: string; const ExportFormat: ExportFormatKind);
begin
  case ExportFormat of
    export_SVG:
      begin
//        StartTimer;
        StoreToFile_Saver(Drawing,
          FileName, T_SVG_Export {T_TikZ_Export});
//        MessageBoxInfo(FloatToStr(GetTimer));
      end;
    export_EMF:
      StoreToFile_EMF(Drawing, FileName);
    export_EPS:
      StoreToFile_Saver(Drawing,
        FileName, T_PostScript_Export);
{$IFDEF VER140}
    export_PNG:
    //TheDrawing.SaveToFile_PNG(FileName)
      StoreToFile_Saver(Drawing,
        FileName, T_PNG_Export);
    export_BMP:
    //TheDrawing.SaveToFile_Bitmap(FileName)
      StoreToFile_Saver(Drawing,
        FileName, T_BMP_Export);
{$ENDIF}
    export_PDF:
      StoreToFile_Saver(Drawing,
        FileName, T_PDF_Export);
    export_metapost:
      StoreToFile_Saver(Drawing,
        FileName, T_MetaPost_Export);
    export_mps:
      StoreToFile_MPS(Drawing, FileName);
    export_epstopdf:
      //StoreToFile_EpsToPdf(Drawing, FileName, False);
      StoreToFile_Saver(Drawing,
        FileName, T_EpsToPdf_Export);
    export_latexeps:
      StoreToFile_LaTeX_EPS(Drawing, FileName);
    export_latexpdf:
      StoreToFile_LaTeX_PDF(Drawing, FileName);
    export_latexcustom:
      StoreToFile_LaTeX_Custom(Drawing, FileName);
  end;
end;

procedure StoreToFile_Saver(const Drawing: TDrawing2D;
  const FileName: string; AClass: TDrawingSaverClass);
var
  Saver: TDrawingSaver;
begin
  Saver := AClass.Create(Drawing);
  try
    Saver.StoreToFile(FileName);
  finally
    Saver.Free;
  end;
end;

function DvipsFixBB_RuleStr(const ExtRect: TRect2D;
  const FactorMM: TRealType): string;
begin
  if FactorMM <= 0 then Exit;
  Result := Format(
    '\textcolor{white}{\rule[-%.2fmm]{%.2fmm}{%0:.2fmm}}\hskip -%1:.2fmm\vskip -%0:.2fmm%%',
    [(ExtRect.Top - ExtRect.Bottom) / FactorMM,
    (ExtRect.Right - ExtRect.Left) / FactorMM]);
end;

procedure StoreToFile_TpX0(const Drawing: TDrawing2D;
  const FileName: string;
  AClass_TeX, AClass_PdfTeX: TDrawingSaverClass;
  const DvipsFixBB: Boolean);
var
  Stream: TFileStream;
  ARect: TRect2D;
  PicWidth: TRealType;
  procedure WriteAsClass(AClass: TDrawingSaverClass);
  var
    Saver: TDrawingSaver;
  begin
    Saver := AClass.Create(Drawing);
    try
      Saver.fStream := Stream;
      Saver.fDvipsFixBB := DvipsFixBB;
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
  if (Drawing.TeXFigure = fig_floating)
    or (Drawing.TeXFigure = fig_wrap) then
  begin
    ARect := Drawing.DrawingExtension;
    PicWidth := (ARect.Right - ARect.Left) * Drawing.PicScale
      + Drawing.Border * 2;
  end;
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    WriteAsClass(T_TpX_Saver);
    if //(Drawing.TeXFigure <> fig_none)      and
      (Drawing.TeXFigurePrologue <> '') then
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
    if Drawing.TeXCenterFigure
      and ((Drawing.TeXFormat <> tex_none)
      or (Drawing.PdfTeXFormat <> pdftex_none)) then
      if (Drawing.TeXFigure <> fig_none)
        then
        WriteLnStream('\centering')
      else
        WriteLnStream('\begin{center}');
    if Drawing.TeXPicPrologue <> '' then
      WriteLnStream(Drawing.TeXPicPrologue + '%');
    if AClass_PdfTeX <> AClass_TeX then
    begin
      //WriteLnStream('\ifx\pdftexversion\undefined');
      WriteLnStream('\ifpdf');
      WriteAsClass(AClass_PdfTeX);
      WriteLnStream('\else');
      WriteAsClass(AClass_TeX);
      WriteLnStream('\fi');
    end
    else
      WriteAsClass(AClass_TeX);
    if Drawing.TeXPicEpilogue <> '' then
      WriteLnStream(Drawing.TeXPicEpilogue + '%');
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
    if Drawing.TeXCenterFigure
      and ((Drawing.TeXFormat <> tex_none)
      or (Drawing.PdfTeXFormat <> pdftex_none))
      and (Drawing.TeXFigure = fig_none)
      then WriteLnStream('\end{center}');
    if //(Drawing.TeXFigure <> fig_none)      and
      (Drawing.TeXFigureEpilogue <> '') then
      WriteLnStream(Drawing.TeXFigureEpilogue);
  finally
    Stream.Free;
  end;
end;

procedure StoreToFile_TpX(const Drawing: TDrawing2D;
  const FileName: string;
  const DvipsFixBB: Boolean);
var
  AClass_TeX, AClass_PdfTeX: TDrawingSaverClass;
begin
  case Drawing.TeXFormat of
    tex_pgf:
      AClass_TeX := T_PGF_Export;
    tex_pstricks:
      AClass_TeX := T_PSTricks_Export;
    tex_eps:
      AClass_TeX := T_PostScript_Light_Export;
    tex_metapost:
      AClass_TeX := T_MetaPost_Export;
    tex_tikz:
      AClass_TeX := T_TikZ_Export;
{$IFDEF VER140}
    tex_bmp:
      AClass_TeX := T_BMP_Export;
    tex_png:
      AClass_TeX := T_PNG_Export;
    tex_emf:
      AClass_TeX := T_EMF_Export;
{$ENDIF}
    tex_none:
      AClass_TeX := T_None_Saver;
  else
    AClass_TeX := T_TeX_Picture_Export;
  end;
  case Drawing.PdfTeXFormat of
    pdftex_pgf:
      AClass_PdfTeX := T_PGF_Export;
    pdftex_pdf:
      AClass_PdfTeX := T_PDF_Light_Export;
{$IFDEF VER140}
    pdftex_png:
      AClass_PdfTeX := T_PNG_Export;
{$ENDIF}
    pdftex_metapost:
      AClass_PdfTeX := T_MetaPost_Export;
    pdftex_tikz:
      AClass_PdfTeX := T_TikZ_Export;
    pdftex_epstopdf:
      AClass_PdfTeX := T_EpsToPdf_Light_Export;
    pdftex_none:
      AClass_PdfTeX := T_None_Saver;
  else
    AClass_PdfTeX := T_TeX_Picture_Export;
  end;
  StoreToFile_TpX0(Drawing, FileName,
    AClass_TeX, AClass_PdfTeX, DvipsFixBB);
end;

{ --================ T_Device_Export ==================-- }

constructor T_Device_Export.Create(Drawing: TDrawing2D);
begin
  inherited Create(Drawing);
  fFactorMM := 1;
end;

function T_Device_Export.GetTransform: TTransf2D;
begin
  Result := IdentityTransf2D;
  Result[1, 1] := fFactorT;
  Result[2, 1] := 0;
  Result[3, 1] := fExtLeft * fFactorT;
  Result[1, 2] := 0;
  Result[2, 2] := fFactorT;
  Result[3, 2] := fExtTop * fFactorT;
end;

procedure T_Device_Export.WriteHeader;
begin
  fDevice.LineWidthBase := fDrawing2D.LineWidthBase;
  fDevice.DottedSize := fDrawing2D.DottedSize;
  fDevice.DashSize := fDrawing2D.DashSize;
  fDevice.MiterLimit := fDrawing2D.MiterLimit;
  fDevice.HatchingStep := fDrawing2D.HatchingStep;
  fDevice.HatchingLineWidth := fDrawing2D.HatchingLineWidth;
  MeasureDrawing;
  //fDevice.W_MM := fW_MM;
  //fDevice.H_MM := fH_MM;
  fDevice.HatchingStep := fDrawing2D.HatchingStep;
  fDevice.HatchingLineWidth := fDrawing2D.HatchingLineWidth;
  fDevice.T := GetTransform;
  fDevice.FactorMM := fFactorMM;
  fDevice.WriteHeader(fExtRect);
end;

procedure T_Device_Export.WriteFooter;
begin
  fDevice.WriteFooter;
end;

procedure T_Device_Export.WriteAllToStream;
begin
  if fStream = nil then Exit;
  (fDevice as TStreamDevice).Stream := fStream;
  WriteAll;
end;

procedure T_Device_Export.WritePiece(Piece: TPiece;
  Obj: TPrimitive2D);
begin
  Obj.DeviceDrawPiece(Piece, IdentityTransf2D, Self.fDevice,
    Rect2D(0, 0, 0, 0));
end;

procedure T_Device_Export.WriteContainer2D(Obj: TContainer2D);
begin
  Obj.DeviceDraw(IdentityTransf2D, Self.fDevice,
    Rect2D(0, 0, 0, 0));
end;

procedure T_Device_Export.WriteRectangle2D(Obj: TRectangle2D);
begin
  Obj.DeviceDraw(IdentityTransf2D, Self.fDevice,
    Rect2D(0, 0, 0, 0));
end;

procedure T_Device_Export.WriteEllipse2D(Obj: TEllipse2D);
begin
  Obj.DeviceDraw(IdentityTransf2D, Self.fDevice,
    Rect2D(0, 0, 0, 0));
end;

procedure T_Device_Export.WriteCircle2D(Obj: TCircle2D);
begin
  Obj.DeviceDraw(IdentityTransf2D, Self.fDevice,
    Rect2D(0, 0, 0, 0));
end;

procedure T_Device_Export.WriteCircular2D(Obj: TCircular2D);
begin
  Obj.DeviceDraw(IdentityTransf2D, Self.fDevice,
    Rect2D(0, 0, 0, 0));
end;

procedure T_Device_Export.WriteText2D(Obj: TText2D);
begin
  Obj.DeviceDraw(IdentityTransf2D, Self.fDevice,
    Rect2D(0, 0, 0, 0));
end;

{ --================ T_TeX_Picture_Export ==================-- }

constructor T_TeX_Picture_Export.Create(Drawing: TDrawing2D);
begin
  inherited Create(Drawing);
  fDevice := T_TeX_Picture_Device.Create;
end;

destructor T_TeX_Picture_Export.Destroy;
begin
  fDevice.Free;
  inherited Destroy;
end;

function T_TeX_Picture_Export.GetTransform: TTransf2D;
begin
  Result := IdentityTransf2D;
end;

procedure T_TeX_Picture_Export.WriteHeader;
begin
  if fDrawing2D.PicScale = 0 then fDrawing2D.PicScale := 1;
  fFactorMM := 1 / fDrawing2D.PicScale;
  (fDevice as T_TeX_Picture_Device).DvipsFixBB := fDvipsFixBB;
  inherited WriteHeader;
end;

{ --================ T_TikZ_Export ==================-- }

constructor T_TikZ_Export.Create(Drawing: TDrawing2D);
begin
  inherited Create(Drawing);
  fDevice := T_TikZ_Device.Create;
end;

destructor T_TikZ_Export.Destroy;
begin
  fDevice.Free;
  inherited Destroy;
end;

function T_TikZ_Export.GetTransform: TTransf2D;
begin
  Result := IdentityTransf2D;
end;

procedure T_TikZ_Export.WriteHeader;
begin
  if fDrawing2D.PicScale = 0 then fDrawing2D.PicScale := 1;
  fFactorMM := 1 / fDrawing2D.PicScale;
  (fDevice as T_TikZ_Device).DvipsFixBB := fDvipsFixBB;
  inherited WriteHeader;
end;

{ --================ T_PGF_Export ==================-- }

constructor T_PGF_Export.Create(Drawing: TDrawing2D);
begin
  inherited Create(Drawing);
  fDevice := T_PGF_Device.Create;
end;

destructor T_PGF_Export.Destroy;
begin
  fDevice.Free;
  inherited Destroy;
end;

function T_PGF_Export.GetTransform: TTransf2D;
begin
  Result := IdentityTransf2D;
end;

procedure T_PGF_Export.WriteHeader;
begin
  if fDrawing2D.PicScale = 0 then fDrawing2D.PicScale := 1;
  fFactorMM := 1 / fDrawing2D.PicScale;
  (fDevice as T_PGF_Device).DvipsFixBB := fDvipsFixBB;
  inherited WriteHeader;
end;


{ --================ T_None_Saver ==================-- }

procedure T_None_Saver.WriteAll;
begin
end;

procedure T_None_Saver.WriteAllToStream;
begin
end;

{ --================ T_SVG_Export ==================-- }

constructor T_SVG_Export.Create(Drawing: TDrawing2D);
begin
  inherited Create(Drawing);
  fDevice := TSvgDevice.Create(Drawing);
end;

destructor T_SVG_Export.Destroy;
begin
  fDevice.Free;
  inherited Destroy;
end;

function T_SVG_Export.GetTransform: TTransf2D;
begin
  Result := IdentityTransf2D;
  {Result[1, 1] := fFactorT;
  Result[2, 1] := 0;
  Result[3, 1] := -fExtLeft * fFactorT;
  Result[1, 2] := 0;
  Result[2, 2] := -fFactorT;
  Result[3, 2] := fExtTop * fFactorT;}
  Result[2, 2] := -1;
end;

procedure T_SVG_Export.WriteHeader;
begin
  if fDrawing2D.PicScale = 0 then fDrawing2D.PicScale := 1;
  fFactorMM := 1 / fDrawing2D.PicScale;
  inherited WriteHeader;
end;

{ --================ T_PostScript_Export ==================-- }

constructor T_PostScript_Export.Create(Drawing: TDrawing2D);
begin
  inherited Create(Drawing);
  fDevice := TPostScriptDevice.Create(Drawing);
  // 2.83464567 pt per mm (Adobe pt = LaTeX bp)
end;

destructor T_PostScript_Export.Destroy;
begin
  fDevice.Free;
  inherited Destroy;
end;

function T_PostScript_Export.GetTransform: TTransf2D;
begin
  Result := IdentityTransf2D;
  Result[1, 1] := fFactorT;
  Result[2, 1] := 0;
  Result[3, 1] := -fExtLeft * fFactorT;
  Result[1, 2] := 0;
  Result[2, 2] := fFactorT;
  Result[3, 2] := -fExtBottom * fFactorT {fExtTop * fFactorT};
//  Result := inherited GetTransform;
end;

procedure T_PostScript_Export.WriteHeader;
begin
  fFactorMM := 2.83464567;
  inherited WriteHeader;
end;

procedure T_PostScript_Export.WriteToTpX(Stream: TStream;
  const FileName: string);
var
  I: Integer;
  EpsFileName: string;
begin
  fStream := nil;
  if (Self is T_EpsToPdf_Export) or
    (Self is T_EpsToPdf_Light_Export) then
    EpsFileName := ChangeFileExt(FileName, '.pdf')
  else
    EpsFileName := ChangeFileExt(FileName, '.eps');
  if not StoreToFile(EpsFileName) then Exit;
  fStream := Stream;
  try
    WriteLnStream('  \setlength{\unitlength}{1bp}%');
    WriteLnStream(Format('  \begin{picture}(%.2f, %.2f)(0,0)',
      [fW_MM * 2.83464567, fH_MM * 2.83464567]));
    WriteLnStream(Format(
      '  \put(0,0){\includegraphics{%s%s}}', //[width=%.1fmm,height=%.1fmm]
      [//fDrawing2D.PicWidth, fDrawing2D.PicHeight,
      fDrawing2D.IncludePath,
        ChangeFileExt(ExtractFileName(FileName), '')]));
    for I := 0 to (fDevice as TPostScriptDevice).TextLabels.Count -
      1 do
      WriteLnStream('  ' + (fDevice as
        TPostScriptDevice).TextLabels[I]);
    WriteLnStream('  \end{picture}%');
  finally
    fStream := nil;
  end;
end;

{ --================ T_PostScript_Light_Export ==================-- }

constructor T_PostScript_Light_Export.Create(Drawing: TDrawing2D);
begin
  inherited Create(Drawing);
  (fDevice as TPostScriptDevice).Light := True;
end;

{ --================ T_EpsToPdf_Export ==================-- }

function Run_EpsToPdf(
  const EpsFileName, PdfFileName: string): Boolean;
begin
  Result := False;
  try
    if not TryDeleteFile(PdfFileName) then
      MessageBoxError('Can not delete PDF file')
    else
    begin
// EpsToPdf script is no more used
//      Result := FileExec(Format('%s "%s" --outfile="%s"',
//        [EpsToPdfPath, EpsFileName, PdfFileName]), '', '',
//        '' {GetTempDir}, True, True);
      // Use ghostscript directly to convert eps to pdf.
      // Eps should be prepared accordingly (page size, page origin)
      Result := FileExec(Format(
        '%s -dSAFER -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite' +
        ' -dAutoRotatePages=/None -sOutputFile="%s" "%s"',
        [GhostscriptPath, PdfFileName, EpsFileName]), '', '',
        GetTempDir, True, True);

      if not FileExists(PdfFileName) then
      begin
        MessageBoxError('PDF file not created');
        Result := False;
      end;
    end;
  finally
  end;
end;

constructor T_EpsToPdf_Export.Create(Drawing: TDrawing2D);
begin
  inherited Create(Drawing);
  (fDevice as TPostScriptDevice).ForPdf := True;
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

constructor T_EpsToPdf_Light_Export.Create(Drawing: TDrawing2D);
begin
  inherited Create(Drawing);
  (fDevice as TPostScriptDevice).Light := True;
  (fDevice as TPostScriptDevice).ForPdf := True;
end;

{ --================ T_PSTricks_Export ==================-- }

constructor T_PSTricks_Export.Create(Drawing: TDrawing2D);
begin
  inherited Create(Drawing);
  fDevice := T_PSTricks_Device.Create;
end;

destructor T_PSTricks_Export.Destroy;
begin
  fDevice.Free;
  inherited Destroy;
end;

function T_PSTricks_Export.GetTransform: TTransf2D;
begin
  Result := IdentityTransf2D;
end;

procedure T_PSTricks_Export.WriteHeader;
begin
  if fDrawing2D.PicScale = 0 then fDrawing2D.PicScale := 1;
  fFactorMM := 1 / fDrawing2D.PicScale;
  (fDevice as T_PSTricks_Device).DvipsFixBB := fDvipsFixBB;
  inherited WriteHeader;
end;

{ --================ T_MetaPost_Export2 ==================-- }

function StoreToFile_MPS(const Drawing: TDrawing2D;
  const FileName: string): Boolean;
var
  TempDir, TempMP, TempMPS, TempMPLog: string;
begin
  Result := False;
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
    Result := FileExec(MetaPostPath +
      ' -tex=latex -quiet "(pic)TpX.mp"', '', '',
      TempDir, True, True);
//    Result := FileExec(MetaPostPath + ' -tex=latex "' + TempMP + '"', '', '',
//      '', True, True);
//    MessageBoxError('MPS');
    if FileExists(TempMPS) then
    begin
      TryDeleteFile(FileName);
      TryDeleteFile(TempMPLog);
      if not SysBasic.RenameFile(TempMPS, FileName) then
      begin
        MessageBoxError('Can not rename. MPS file not created');
        Result := False;
      end;
    end
    else
    begin
      if not FileExists(TempMPLog) then
        MessageBoxError('MPS file not created')
      else if MessageBoxErrorAsk(
        'MPS file not created. Do you want to see log file?')
        then
        OpenOrExec(TextViewerPath, TempMPLog);
      Result := False;
    end;
  finally
    if Result then TryDeleteFile(TempMP);
    TryDeleteFile(TempMPS);
    if Result then TryDeleteFile(TempMPLog);
  end;
end;

constructor T_MetaPost_Export.Create(Drawing: TDrawing2D);
begin
  inherited Create(Drawing);
  fDevice := T_MetaPost_Device.Create(Drawing);
end;

destructor T_MetaPost_Export.Destroy;
begin
  fDevice.Free;
  inherited Destroy;
end;

function T_MetaPost_Export.GetTransform: TTransf2D;
begin
  Result := IdentityTransf2D;
end;

procedure T_MetaPost_Export.WriteHeader;
begin
  if fDrawing2D.PicScale = 0 then fDrawing2D.PicScale := 1;
  fFactorMM := 1 / fDrawing2D.PicScale;
  inherited WriteHeader;
end;

procedure T_MetaPost_Export.WriteToTpX(Stream: TStream;
  const FileName: string);
begin
  fStream := nil;
  if not StoreToFile_MPS(fDrawing2D, ChangeFileExt(FileName,
    '.mps'))
    then Exit;
  fStream := Stream;
  try
    WriteLnStream(Format('\includegraphics{%s%s.mps}%%',
      [fDrawing2D.IncludePath,
      ChangeFileExt(ExtractFileName(FileName), '')]));
  finally
    fStream := nil;
  end;
end;

{ --================ T_PDF_Export ==================-- }

constructor T_PDF_Export.Create(Drawing: TDrawing2D);
begin
  inherited Create(Drawing);
  fDevice := TPdfDevice.Create(Drawing);
end;

destructor T_PDF_Export.Destroy;
begin
  fDevice.Free;
  inherited Destroy;
end;

function T_PDF_Export.GetTransform: TTransf2D;
begin
  Result := IdentityTransf2D;
  Result[1, 1] := fFactorT;
  Result[2, 1] := 0;
  Result[3, 1] := -fExtLeft * fFactorT;
  Result[1, 2] := 0;
  Result[2, 2] := fFactorT;
  Result[3, 2] := -fExtBottom * fFactorT;
end;

procedure T_PDF_Export.WriteHeader;
begin
  fFactorMM := 2.83464567;
  //72 pixel per inch, (25.4/72 = 0.352778) mm in pixel
  // 2.83464567 pt per mm (Adobe pt = LaTeX bp)
  inherited WriteHeader;
end;

procedure T_PDF_Export.WriteToTpX(Stream: TStream;
  const FileName: string);
var
  I: Integer;
begin
  fStream := nil;
  if not StoreToFile(ChangeFileExt(FileName, '.pdf')) then
  begin
    fStream := Stream;
    Exit;
  end;
  fStream := Stream;
  try
    WriteLnStream('  \setlength{\unitlength}{1bp}%');
    WriteLnStream(Format('  \begin{picture}(%.2f, %.2f)(0,0)',
      [fW_MM * 2.83464567, fH_MM * 2.83464567]));
    WriteLnStream(Format(
      '  \put(0,0){\includegraphics{%s%s.pdf}}', //[width=%.1fcm,height=%.1fcm]
      [//fDrawing2D.PicWidth / 10, fDrawing2D.PicHeight / 10,
      fDrawing2D.IncludePath,
        ChangeFileExt(ExtractFileName(FileName), '')]));
    for I := 0 to (fDevice as TPdfDevice).TextLabels.Count - 1 do
      WriteLnStream('  ' + (fDevice as TPdfDevice).TextLabels[I]);
    WriteLnStream('  \end{picture}%');
  finally
    fStream := nil;
  end;
end;

{ --================ T_PDF_Light_Export ==================-- }

constructor T_PDF_Light_Export.Create(Drawing: TDrawing2D);
begin
  inherited Create(Drawing);
  (fDevice as TPdfDevice).Light := True;
end;

{$IFDEF VER140}

{ --================ T_Bitmap0_Export ==================-- }

constructor T_Bitmap0_Export.Create(Drawing: TDrawing2D);
begin
  inherited Create(Drawing);
  fDevice := TGr32Device.Create(Drawing);
end;

destructor T_Bitmap0_Export.Destroy;
begin
  fDevice.Free;
  inherited Destroy;
end;

function T_Bitmap0_Export.GetTransform: TTransf2D;
begin
  Result := IdentityTransf2D;
  Result[1, 1] := fFactorT;
  Result[2, 1] := 0;
  Result[3, 1] := -fExtLeft * fFactorT;
  Result[1, 2] := 0;
  Result[2, 2] := -fFactorT;
  Result[3, 2] := fExtTop * fFactorT;
end;

procedure T_Bitmap0_Export.WriteHeader;
begin
  if fDrawing2D.PicScale = 0 then fDrawing2D.PicScale := 1;
  fFactorMM := 1 / fDrawing2D.PicUnitLength;
  inherited WriteHeader;
end;

function T_Bitmap0_Export.StoreToFile(const FileName: string):
  Boolean;
begin
  WriteAll;
  (fDevice as TGr32Device).StoreToFileBMP(FileName);
end;

{ --================ T_BMP_Export ==================-- }

procedure WriteBB(const W_MM, H_MM: TRealType;
  const Caption, FileName: string);
var
  BBList: TStringList;
begin
  BBList := TStringList.Create;
  try
    BBList.Add('%%' + Format('BoundingBox: %d %d %d %d',
      [0, 0, Ceil(W_MM * 2.8346), Ceil(H_MM * 2.8346)]));
    BBList.Add('%%' +
      Format('HiResBoundingBox: %d %d %.2f %.2f',
      [0, 0, W_MM * 2.8346, H_MM * 2.8346]));
    BBList.Add('%%Title: ' + Caption);
    BBList.Add('%%Creator: Exported from TpX drawing');
    BBList.Add('%%CreationDate: ' + DateTimeToStr(Now));
    BBList.SaveToFile(ChangeFileExt(FileName, '.bb'));
  finally
    BBList.Free;
  end;
end;

procedure T_BMP_Export.WriteToTpX(Stream: TStream;
  const FileName: string);
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
      '  \includegraphics[width=%.2fmm,height=%.2fmm]{%s%s.bmp}',
      [fW_MM, fH_MM, fDrawing2D.IncludePath,
      ChangeFileExt(ExtractFileName(FileName), '')]));
//\item[natwidth,natheight] Again an alternative to |bb|.
// |natheight=h,natwidth=w| is equivalent to |bb = 0 0 h w|.
  finally
    fStream := nil;
  end;
  WriteBB(fW_MM, fH_MM, fDrawing2D.Caption, FileName);
end;

{ --================ T_PNG_Export ==================-- }

procedure T_PNG_Export.WriteToTpX(Stream: TStream;
  const FileName: string);
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
      '  \includegraphics[width=%.2fmm,height=%.2fmm]{%s%s.png}',
      [fW_MM, fH_MM, fDrawing2D.IncludePath,
      ChangeFileExt(ExtractFileName(FileName), '')]));
  finally
    fStream := nil;
  end;
  WriteBB(fW_MM, fH_MM, fDrawing2D.Caption, FileName);
end;

function T_PNG_Export.StoreToFile(const FileName: string):
  Boolean;
begin
  WriteAll;
  (fDevice as TGr32Device).StoreToFilePNG(FileName);
end;

{$ENDIF}

{ --================ T_EMF_Dvc_Export ==================-- }

{$IFDEF VER140}

function DrawingAsMetafile(Drawing: TDrawing2D): TMetaFile;
var
  Exp: T_EMF_Dvc_Export;
begin
  Result := Graphics.TMetaFile.Create;
  Exp := T_EMF_Dvc_Export.CreateWithMF(Drawing, Result);
  try
    Exp.WriteAll;
  finally
    Exp.Free;
  end;
end;

constructor T_EMF_Dvc_Export.CreateWithMF(const Drawing:
  TDrawing2D;
  const AMetaFile: TMetaFile);
begin
  Create(Drawing);
  fDevice := TCanvasDevice.Create;
  fMetaFile := AMetaFile;
end;

destructor T_EMF_Dvc_Export.Destroy;
begin
  (fDevice as TCanvasDevice).Cnv.Free;
  fDevice.Free;
  inherited Destroy;
end;

function T_EMF_Dvc_Export.GetTransform: TTransf2D;
begin
  Result := IdentityTransf2D;
  Result[1, 1] := fFactorT;
  Result[2, 1] := 0;
  Result[3, 1] := -fExtLeft * fFactorT;
  Result[1, 2] := 0;
  Result[2, 2] := -fFactorT;
  Result[3, 2] := fExtTop * fFactorT;
end;

procedure T_EMF_Dvc_Export.WriteHeader;
var
  PpMM_X, PpMM_Y: Double;
  procedure Get_Resolution(var PpMM_X, PpMM_Y: Double);
  var
    TmpCnv: TMetaFileCanvas;
    TmpMF: TMetaFile;
  begin
    //How to get device PPI?//  DC: HDC;
{    DC := GetDC(0);
    PpMM_X := GetDeviceCaps(DC, LOGPIXELSX);
    PpMM_Y := GetDeviceCaps(DC, LOGPIXELSY);
    ReleaseDC(0,DC);}
    TmpMF := TMetaFile.Create;
    TmpCnv := TMetaFileCanvas.Create(TmpMF, 0);
    TmpCnv.Free;
    GetMetaFileResolution(TmpMF, PpMM_X, PpMM_Y);
    TmpMF.Free;
  end;
begin
  //fFactorMM := 1 / fDrawing2D.PicUnitLength;
  fFactorMM := 100; // 100 pixels per mm
  inherited WriteHeader;
  Get_Resolution(PpMM_X, PpMM_Y);
  fMetaFile.Enhanced := True;
  fMetaFile.Width := Round(fW_MM * PpMM_X);
  fMetaFile.Height := Round(fH_MM * PpMM_Y);
  (fDevice as TCanvasDevice).Cnv
    := TMetaFileCanvas.CreateWithComment(fMetaFile, 0,
    'Created by TpX drawing tool',
    fDrawing2D.Caption + ' :: ' +
    fDrawing2D.Comment);
  // Scale metafile units
  ChangeCanvasResolution((fDevice as TCanvasDevice).Cnv,
    Round(1000 * fFactorMM), Round(1000 * fFactorMM),
    Round(1000 * PpMM_X), Round(1000 * PpMM_Y));
end;

{$ENDIF}

procedure StoreToFile_EMF(const Drawing: TDrawing2D;
  const FileName: string);
{$IFDEF VER140}
var
  MetaFile: TMetaFile;
{$ENDIF}
begin
{$IFDEF VER140}
  //StopRepaint;
  MetaFile := DrawingAsMetafile(Drawing);
  try
    MetaFile.SaveToFile(FileName);
  finally
    MetaFile.Free;
  end;
{$ENDIF}
end;

{ --================ T_EMF_Export ==================-- }

{$IFDEF VER140}

procedure T_EMF_Export.WriteToTpX(Stream: TStream;
  const FileName: string);
begin
  fStream := nil;
  //Drawing2D.SaveToFile_EMF(ChangeFileExt(FileName, '.emf'), 1);
  StoreToFile_EMF(fDrawing2D, ChangeFileExt(FileName, '.emf'));
  fStream := Stream;
  try
    fFactorMM := 1;
    MeasureDrawing;
    WriteLnStream(Format(
      '  \includegraphics[width=%.3fmm,height=%.3fmm]{%s%s}',
      [fW_MM, fH_MM, fDrawing2D.IncludePath,
      ChangeFileExt(ExtractFileName(FileName), '.emf')]));
  finally
    fStream := nil;
  end;
  WriteBB(fW_MM, fH_MM, fDrawing2D.Caption, FileName);
end;

function T_EMF_Export.StoreToFile(const FileName: string):
  Boolean;
begin
  StoreToFile_EMF(fDrawing2D, FileName);
end;
{$ENDIF}

function StoreToFile_LaTeX_EPS0(const Drawing: TDrawing2D;
  var TempEPS: string): Boolean;
var
  TempDvi: string;
begin
  Result := False;
  Run_LaTeX_Temp(Drawing, ltxview_Dvi, TempDvi, True, True);
  TempEPS := ChangeFileExt(TempDvi, '.eps');
  if not TryDeleteFile(TempEPS) then
  begin
    MessageBoxError('Can not delete EPS file');
    Exit;
  end;
  FileExec(Format('%s -E "%s" -o "%s"',
    [DviPsPath, ExtractFileName(TempDvi),
    ExtractFileName(TempEPS)]),
      '', '', GetTempDir, True, True);
  if not FileExists(TempEPS) then
  begin
    MessageBoxError('EPS file not created');
    Exit;
  end;
  Result := True;
end;

procedure StoreToFile_LaTeX_EPS(const Drawing: TDrawing2D;
  const FileName: string);
var
  TempEPS: string;
begin
  if not StoreToFile_LaTeX_EPS0(Drawing, TempEPS) then Exit;
  SysBasic.RenameFile(TempEPS, FileName);
end;

procedure StoreToFile_LaTeX_PDF(const Drawing: TDrawing2D;
  const FileName: string);
var
  TempEPS: string;
  procedure Eps_Prepare(const TempEPS: string);
  var
    Lines: TStringList;
    I, J: Integer;
    Left, Bottom, Right, Top: Integer;
    BB_St: string;
  begin
    Lines := TStringList.Create;
    try
      Lines.LoadFromFile(TempEPS);
      for I := 0 to Lines.Count - 1 do
      begin
        J := Pos('%%BoundingBox:', Lines[I]);
        if J = 1 then
        begin
          BB_St := Lines[I];
          BB_St := Trim(Copy(BB_St, 15, Length(BB_St)));
          BB_St := AnsiReplaceStr(BB_St, ' ', ',');
          Left := StrToInt(CSV_Item(BB_St, 1));
          Bottom := StrToInt(CSV_Item(BB_St, 2));
          Right := StrToInt(CSV_Item(BB_St, 3));
          Top := StrToInt(CSV_Item(BB_St, 4));
          Lines[I] := '%%' + Format('BoundingBox: 0 0 %d %d',
            [Right - Left, Top - Bottom]);
          Lines.Insert(I + 1, Format('%d %d translate',
            [-Left, -Bottom]));
          Lines.Insert(I + 1,
            Format('<< /PageSize [%d %d] >> setpagedevice',
            [Right - Left, Top - Bottom]));
          Break;
        end;
      end;
      Lines.SaveToFile(TempEPS);
    finally
      Lines.Free;
    end;
  end;
begin
  if not StoreToFile_LaTeX_EPS0(Drawing, TempEPS) then Exit;
//  Prepare Eps created by dvips with -E key for conversion with Ghostscript
  Eps_Prepare(TempEPS);
  Run_EpsToPdf(TempEPS, FileName);
  TryDeleteFile(TempEPS);
end;

procedure LaTeX_Custom_Parse(var Device, Ext, Ext2: string);
const
  Devices: array[1..47] of string[8] =
  ('bmp16', 'bmp16m', 'bmp256', 'bmp32b', 'bmpgray', 'bmpmono',
    'bmpsep1', 'bmpsep8', 'psdcmyk', 'psdrgb',
    'epswrite', 'jpeggray', 'jpeg', 'pbmraw', 'pbm',
    'pcx16', 'pcx24b', 'pcx256', 'pcxcmyk', 'pcxgray', 'pcxmono',
    'pdfwrite', 'pgmraw', 'pgm', 'pgnmraw', 'pgnm',
    'png16', 'png16m', 'png256', 'pngalpha', 'pnggray', 'pngmono',
    'pnmraw', 'pnm', 'psmono', 'pswrite',
    'tiff12nc', 'tiff24nc', 'tiff32nc', 'tiffcrle', 'tiffg32d',
    'tiffg3',
    'tiffg4', 'tiffgray', 'tifflzw', 'tiffpack', 'tiffsep');
  Extensions: array[1..13] of string[4] =
  ('bmp', 'psd', 'eps', 'jpeg', 'pbm', 'pcx',
    'pdf', 'pgm', 'pgnm', 'png', 'pnm', 'ps', 'tiff');
var
  I, J: Integer;
begin
  Device := '';
  Ext := '';
  Ext2 := '';
  if Pos('sDEVICE', GhostscriptCustomKeys) = 0 then Exit;
  for I := 1 to High(Devices) do
  begin
    if Pos(Devices[I], GhostscriptCustomKeys) > 0 then
    begin
      Device := Devices[I];
      for J := 1 to High(Extensions) do
        if Pos(Extensions[J], Device) > 0 then
        begin
          Ext := Extensions[J];
          Break;
        end;
      if Ext = 'jpeg' then Ext2 := 'jpg';
      if Ext = 'tiff' then Ext2 := 'tif';
      Break;
    end;
  end;
end;

procedure StoreToFile_LaTeX_Custom(const Drawing: TDrawing2D;
  const FileName: string);
var
  TempEPS: string;
begin
  if not StoreToFile_LaTeX_EPS0(Drawing, TempEPS) then Exit;
  GhostScriptConvert(TempEPS, GhostscriptCustomKeys, FileName);
  //TryDeleteFile(TempEPS);
end;


procedure ParseParameters0(var FileName, IncludePath,
  OutputFile, OutputFormats, ExportFormats: string);
var
  I: Integer;
  LineNumber: Integer;
  TeXFileName: string;
  OptList: TStringList;
const
  ShortOpt: array[1..6] of Char =
  ('f', 'i', 'l', 'o', 'm', 'x');
  LongOpt: array[1..6] of ShortString =
  ('file', 'texinput', 'texline', 'output', 'format', 'export');
begin
  LineNumber := 1;
  FileName := '';
  IncludePath := '';
  OutputFile := '';
  OutputFormats := '';
  ExportFormats := '';
  TeXFileName := '';
  OptList := TStringList.Create;
  try
    ParseCmdLine(System.CmdLine, OptList);
    for I := 1 to High(ShortOpt) do
      if OptList.IndexOfName(LongOpt[I]) >= 0 then
        OptList[I] := ShortOpt[I] + '=' +
          OptList.Values[LongOpt[I]];
    FileName := OptList.Values['f'];
    if FileName = '' then FileName := OptList.Values['$FileName'];
    FileName := ExpandFileName(FileName);
    if OptList.IndexOfName('i') >= 0 then
      TeXFileName := OptList.Values['i'];
    if OptList.IndexOfName('l') >= 0 then
    begin
      try
        LineNumber := StrToInt(OptList.Values['l']);
      except
        MessageBoxError('Line number: ' + OptList.Values['l']);
      end
    end;
    if OptList.IndexOfName('o') >= 0 then
    begin
      OutputFile := OptList.Values['o'];
      if OutputFile = '' then
        OutputFile := '$Default'
    end;
    if OptList.IndexOfName('m') >= 0 then
      OutputFormats := OptList.Values['m'];
    if OptList.IndexOfName('x') >= 0 then
      ExportFormats := OptList.Values['x'];
    //MessageBoxInfo(OptList.DelimitedText);
    //MessageBoxInfo(TeXFileName);
  finally
    OptList.Free;
  end;
  if TeXFileName <> '' then
    FindPicturePath(TeXFileName, LineNumber, FileName,
      IncludePath);
end;

procedure SetOutputFormats(const OutputFormats: string;
  ADrawing: TDrawing2D);
var
  OutputFormat1, OutputFormat2: string;
  I: Integer;
begin
  if OutputFormats = '' then Exit;
  OutputFormat1 := CSV_Item(OutputFormats, 1);
  OutputFormat2 := CSV_Item(OutputFormats, 2);
  if OutputFormat1 = '' then OutputFormat1 := 'none';
  if OutputFormat2 = '' then OutputFormat2 := OutputFormat1;
  I := CSV_Find(TeXFormat_Choice, OutputFormat1);
  if I > 0 then
    ADrawing.TeXFormat := TeXFormatKind(I - 1);
  I := CSV_Find(PdfTeXFormat_Choice, OutputFormat2);
  if I > 0 then
    ADrawing.PdfTeXFormat := PdfTeXFormatKind(I - 1);
end;

function CheckCommandLine: Boolean;
var
  FileName, IncludePath: string;
  OutputFile, OutputFormats, ExportFormats: string;
  ADrawing: TDrawing2D;
  Loader: T_TpX_Loader;
  Ext: string;
  I, J: Integer;
  function GetExportDefaultExt(const J: Integer): string;
  var
    Ext, Ext2: string;
  begin
    Result := CSV_Item(ExportDefaultExt, J);
    if Result = '*' then
    begin
      LaTeX_Custom_Parse(Result, Ext, Ext2);
      Result := Ext;
    end;
  end;
begin
  Result := True;
  ParseParameters0(FileName, IncludePath,
    OutputFile, OutputFormats, ExportFormats);
  if (OutputFile = '') and (ExportFormats = '') then Exit;
  if FileName = '' then Exit;
    //ShowMessage(FileName);
  if not FileExists(FileName) then Exit;
  LoadSettings;
  ADrawing := TDrawing2D.Create(nil);
  ADrawing.IncludePath := IncludePath;
  try
    Ext := LowerCase(ExtractFileExt(FileName));
    if (Ext = '.emf') or (Ext = '.wmf') then
      Import_Metafile(ADrawing, FileName, nil)
    else if (Ext = '.svg') then
      Import_SVG(ADrawing, FileName)
    else if (Ext = '.eps') or (Ext = '.ps') or (Ext = '.pdf') then
      Import_Eps(ADrawing, FileName)
    else
    begin
      Loader := T_TpX_Loader.Create(ADrawing);
      try
        if Ext = '' then
        begin
          Ext := '.TpX';
          FileName := ChangeFileExt(FileName, '.TpX');
        end;
        Loader.LoadFromFile(FileName);
      finally
        Loader.Free;
      end;
    end;
    if ExportFormats <> '' then
    begin
      if (OutputFile = '') or (OutputFile = '$Default') then
        OutputFile := ChangeFileExt(FileName, '') + '-export';
      I := 1;
      repeat
        Ext := Trim(CSV_Item(ExportFormats, I));
        if Ext <> '' then
        begin
          J := CSV_Find(ExportFormat_Choice, Ext);
          if J > 0 then
            ExportToFile(ADrawing,
              ChangeFileExt(OutputFile, '.' +
              GetExportDefaultExt(J)),
              ExportFormatKind(J - 1));
        end;
        Inc(I);
      until Ext = '';
    end
    else
    begin
      SetOutputFormats(OutputFormats, ADrawing);
      if (OutputFile = '') or (OutputFile = '$Default') or (Ext =
        '') then
        OutputFile := ChangeFileExt(FileName, '.TpX');
      StoreToFile_TpX(ADrawing, OutputFile, False);
    end;
  finally
    ADrawing.Free;
  end;
  Result := False;
end;

procedure ParseParameters(
  var FileName, IncludePath, OutputFormats: string);
var
  OutputFile, ExportFormats: string;
begin
  ParseParameters0(FileName, IncludePath,
    OutputFile, OutputFormats, ExportFormats);
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
    else
      PicturePath := St;
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
        then
        I := 2 * Line - I
      else
        Inc(I)
    else if 2 * Line - I + 1 <= Lines.Count
      then
      I := 2 * Line - I + 1
    else
      Dec(I);
  until False;
  Lines.Free;
end;

end.


unit DevPSTr;

interface

uses SysUtils, Classes, Geometry, Graphics, Devices;

type

// A class for pstricks output

  T_PSTricks_Device = class(TFileDevice)
  protected
    fColors: TStringList;
    fCurrDash: TLineStyle;
    fAttributes: string;
    function WriteNewColor(Color: TColor): string;
    procedure AddAttr(const Attr: string);
    procedure PrepareAttr(const LineStyle: TLineStyle;
      const LineWidth: TRealType;
      const LineColor, FillColor: TColor; Closed: Boolean);
    procedure WriteAttr;
    procedure WriteHatchingLines(const Lines: TPointsSet2D;
      const HatchColor: TColor; const LineWidth: TRealType);
      override;
    procedure Bezier(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching; const Transf: TTransf2D;
      const Closed: Boolean);
    procedure Circle(const CP: TPoint2D; const R: TRealType;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching);
    procedure Circular(const CP: TPoint2D; R, SA, EA: TRealType;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching; const Kind: TCircularKind);
    procedure RotText(P: TPoint2D; H, ARot: TRealType;
      WideText: WideString; TeXText: AnsiString;
      const HAlignment: THAlignment;
      const LineColor: TColor;
      const FaceName: AnsiString;
      const Charset: TFontCharSet; const Style: TFontStyles);
    procedure Bitmap(P: TPoint2D; W, H: TRealType;
      const KeepAspectRatio: Boolean; BitmapEntry: TObject);
  public
    DvipsFixBB: Boolean;
    FontSizeInTeX: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Poly(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching; const Transf: TTransf2D;
      const Closed: Boolean); override;
    procedure WriteHeader(ExtRect: TRect2D); override;
    procedure WriteFooter; override;
  end;

implementation

uses ColorEtc, Output, Bitmaps;

// =====================================================================
// T_PSTricks_Device
// =====================================================================

constructor T_PSTricks_Device.Create;
begin
  inherited Create;
  DvipsFixBB := False;
//  fDisjointFill := True;
  OnBezier := Bezier;
  OnCircle := Circle;
  OnCircular := Circular;
  OnRotText := RotText;
  OnBitmap := Bitmap;
  fHasBezier := True;
  fHasClosedBezier := True;
  fHasArc := True;
  fHasSector := True;
  fHasSegment := True;
  fHasNativeHatching := False;
  fColors := TStringList.Create;
  fCurrDash := liNone;
end;

destructor T_PSTricks_Device.Destroy;
begin
  fColors.Free;
  inherited Destroy;
end;

function T_PSTricks_Device.WriteNewColor(Color: TColor): string;
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
  if fColors.IndexOf(Result) >= 0 then Exit;
  fColors.Add(Result);
  RGB := PS_RGB(Color);
  if Color <> clBlack then
    WriteLnStream(Format('\newrgbcolor{%s}{%.5g %.5g %.5g}',
      [Result, RGB.R, RGB.G, RGB.B]));
end;

procedure T_PSTricks_Device.AddAttr(const Attr: string);
begin
  if fAttributes = '' then fAttributes := Attr
  else fAttributes := fAttributes + ',' + Attr;
end;

procedure T_PSTricks_Device.PrepareAttr(
  const LineStyle: TLineStyle;
  const LineWidth: TRealType; const LineColor, FillColor: TColor;
  Closed: Boolean);
var
  C: string;
begin
  fAttributes := '';
  if FillColor <> clDefault then
    AddAttr('fillstyle=solid,fillcolor=' +
      WriteNewColor(FillColor));
  if (LineStyle <> liNone) and (LineStyle <> liSolid)
    and (fCurrDash <> LineStyle) then
  begin
    case LineStyle of
      liDotted:
        WriteLnStream(Format('\psset{dash=%.2fmm %.2fmm}',
          [fLineWidthBase * LineWidth, fDottedSize]));
      liDashed:
        WriteLnStream(Format('\psset{dash=%.2fmm %.2fmm}',
          [fDashSize * 2, fDashSize]));
    end;
    fCurrDash := LineStyle;
  end;
  if (LineStyle <> liNone) and (LineWidth <> 1) then
    AddAttr(Format('linewidth=%.2fmm',
      [fLineWidthBase * LineWidth]));
  case LineStyle of
    liNone: AddAttr('linestyle=none');
    liDotted, liDashed: AddAttr('linestyle=dashed');
  end;
  if LineColor <> clBlack then
  begin
    C := WriteNewColor(LineColor);
    if C <> '' then AddAttr('linecolor=' + C);
  end;
end;

procedure T_PSTricks_Device.WriteAttr;
begin
  if fAttributes <> '' then
    WriteStream('[' + fAttributes + ']');
end;

procedure T_PSTricks_Device.WriteHeader(ExtRect: TRect2D);
begin
  if fFactorMM = 0 then fFactorMM := 1;
  WriteStream('{'); // Make settings local
  if DvipsFixBB then
    WriteLnStream(DvipsFixBB_RuleStr(ExtRect, fFactorMM));
  WriteLnStream(Format('\psset{unit=%.2fmm,dashadjust=false}',
    [1 / fFactorMM]));
  WriteLnStream(Format('\begin{pspicture}(%.2f,%.2f)(%.2f,%.2f)',
    [ExtRect.Left, ExtRect.Bottom, ExtRect.Right, ExtRect.Top]));
  WriteLnStream(Format('\psset{linewidth=%.2fmm}',
    [fLineWidthBase]));
end;

procedure T_PSTricks_Device.WriteFooter;
begin
  WriteLnStream('\end{pspicture}}%');
end;

procedure T_PSTricks_Device.WriteHatchingLines(
  const Lines: TPointsSet2D;
  const HatchColor: TColor; const LineWidth: TRealType);
var
  C: string;
  I: Integer;
begin
  if Lines.Count < 2 then Exit;
  Lines.TransformPoints(T);
  if HatchColor = clBlack then
    C := ''
  else
    C := ',linecolor=' + WriteNewColor(HatchColor);
  WriteLnStream(Format('{\psset{linewidth=%.2fmm%s}',
    [fLineWidthBase * LineWidth, C]));
  for I := 0 to Lines.Count div 2 - 1 do
  begin
    WriteStream('\psline');
    WriteStreamPoint(Lines[I * 2]);
    WriteStreamPoint(Lines[I * 2 + 1]);
  end;
  WriteLnStream('}');
end;

procedure T_PSTricks_Device.Poly(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D;
  const Closed: Boolean);
var
  I: Integer;
begin
  if (LineStyle = liNone) and (PP.Count < 3) then Exit;
  if PP.Count < 1 then Exit;
  PrepareAttr(LineStyle, LineWidth, LineColor, FillColor, Closed);
//  if fDrawing2D.MiterLimit <> 10 then
//    WriteLnStream(Format('\pscustom{\code{%.2f setmiterlimit}',
//      [fDrawing2D.MiterLimit]));
//\pscustom{%
//    \code{1 setlinejoin}
//    \psline(0,0)(1,2)(2,0)}
  if Closed then WriteStream('\pspolygon')
  else WriteStream('\psline');
  WriteAttr;
  for I := 0 to PP.Count - 1 do
    WriteStreamPoint(PP[I]);
//  if fDrawing2D.MiterLimit <> 10 then    WriteStream('}');
  WriteLnStream('');
end;

procedure T_PSTricks_Device.Bezier(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D;
  const Closed: Boolean);
var
  I: Integer;
begin
  if PP.Count < 1 then Exit;
  PrepareAttr(LineStyle, LineWidth, LineColor, FillColor, Closed);
  WriteStream('\pscustom');
  WriteAttr;
  WriteStream('{');
  WriteStream('\moveto');
  WriteStreamPoint(PP[0]);
  for I := 0 to PP.Count div 3 - 1 do
  begin
//    WriteStream('\psbezier');
    WriteStream('\curveto');
//    WriteStreamPoint(PP[3 * I]);
    WriteStreamPoint(PP[3 * I + 1]);
    WriteStreamPoint(PP[3 * I + 2]);
    WriteStreamPoint(PP[3 * I + 3]);
  end;
  if Closed then WriteStream('\closepath');
  WriteStream('}');
  WriteLnStream('');
end;

procedure T_PSTricks_Device.Circle(const CP: TPoint2D; const R:
  TRealType;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching);
begin
  PrepareAttr(LineStyle, LineWidth, LineColor, FillColor, True);
  AddAttr('dimen=middle');
  WriteStream('\pscircle');
  WriteAttr;
  WriteStreamPoint(CP);
  if fFactorMM = 0 then fFactorMM := 1;
  WriteStream(Format('{%.2f}', [R * fTScale]));
  // + fLineWidthBase * LineWidth * FactorMM / 2
  WriteLnStream('');
end;

procedure T_PSTricks_Device.Circular(const CP: TPoint2D; R, SA, EA:
  TRealType;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Kind: TCircularKind);
var
  Delt: TRealType;
  function GetPoint(A, R: TRealType): TPoint2D;
  begin
    Result := Point2D(CP.X + R * Cos(A), CP.Y + R * Sin(A));
  end;
begin
  PrepareAttr(LineStyle, LineWidth, LineColor, FillColor, True);
  if EA < SA then EA := EA + 2 * Pi;
  if Kind = ci_Sector then
  begin
    AddAttr('dimen=middle');
    WriteStream('\pswedge');
    WriteAttr;
//    R := R + fLineWidthBase * LineWidth * FactorMM / 2 / fTScale;
  end
  else
  begin
    WriteStream('\psarc');
    WriteAttr;
    WriteStream('{-}');
  end;
  WriteStreamPoint(CP);
  WriteStream(Format('{%.2f}{%.2f}{%.2f}',
    [R * fTScale, RadToDeg(SA), RadToDeg(EA)]));
  WriteLnStream('');
  if Kind = ci_Segment then
  begin
    Delt := fLineWidthBase * FactorMM
      / (R * fTScale); // Draw miters
    PrepareAttr(LineStyle, LineWidth, LineColor, FillColor, True);
    WriteStream('\psline');
    WriteAttr;
    WriteStreamPoint(GetPoint(SA + Delt, R));
    WriteStreamPoint(GetPoint(SA, R));
    WriteStreamPoint(GetPoint(EA, R));
    WriteStreamPoint(GetPoint(EA - Delt, R));
    WriteLnStream('');
  end;
end;

procedure T_PSTricks_Device.RotText(P: TPoint2D; H, ARot:
  TRealType;
  WideText: WideString; TeXText: AnsiString;
  const HAlignment: THAlignment;
  const LineColor: TColor;
  const FaceName: AnsiString;
  const Charset: TFontCharSet; const Style: TFontStyles);
begin
  ShiftTeXTextPoint(P, H, ARot);
  if fFactorMM = 0 then fFactorMM := 1;
  WriteLnStream(Format('\put(%.2f,%.2f){%s}', [P.X, P.Y,
    GetTeXTextMakebox0(
      H, ARot, 1 / fFactorMM, LineColor, HAlignment,
      Style, WideText, TeXText, FontSizeInTeX)]));
end;

procedure T_PSTricks_Device.Bitmap(P: TPoint2D; W, H: TRealType;
  const KeepAspectRatio: Boolean; BitmapEntry: TObject);
var
  BE: TBitmapEntry;
  OutDir: string;
begin
  BE := BitmapEntry as TBitmapEntry;
  OutDir := ExtractFilePath(BE.GetFullLink);
  if not BE.RequireEPS(OutDir) then Exit;
  if fFactorMM = 0 then fFactorMM := 1;
  WriteLnStream(Format('\put(%.2f,%.2f){%s}',
    [P.X, P.Y, BE.GetIncludeGraphics(
      W / fFactorMM, H / fFactorMM, KeepAspectRatio,
      IncludePath)]));
end;

end.


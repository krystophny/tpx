unit DevTikZ;

interface

uses SysUtils, Classes, Geometry, Graphics, Devices;

type

// A class for TikZ output

  T_TikZ_Device = class(TFileDevice)
  protected
    fCurrLineColor, fCurrFillColor, fCurrTextColor, fCurrGradColor: TColor;
    fHatchPass: Boolean;
    fFadings: TStringList;
    { Colour definitions before an opacity group; the scope forgets the
      ones made inside it. }
    fGroupLineColor, fGroupFillColor, fGroupTextColor,
      fGroupGradColor: TColor;
    function GradientFading: Boolean;
    function DeclareFading: string;
    function ShadingOptions: string;
    procedure DefineColor(Color: TColor; ID: Char);
    procedure WriteAttr(
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching);
    procedure HatchingLine(P0, P1: TPoint2D;
      const LineColor: TColor; const LineStyle: TLineStyle;
      const LineWidth: TRealType); override;
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
    procedure RotEllipse(const CP: TPoint2D;
      const RX, RY, ARot: TRealType;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching);
    procedure RotRect(const P: TPoint2D;
      const W, H, ARot: TRealType;
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
      const VAlignment: TVAlignment;
      const LineColor: TColor;
      const FaceName: AnsiString;
      const Charset: TFontCharSet; const Style: TFontStyles);
    procedure Bitmap(P: TPoint2D; W, H: TRealType;
      const KeepAspectRatio: Boolean; BitmapEntry: TObject);
  public
    FontSizeInTeX: Boolean;
    DefaultFontHeight: TRealType;
    DvipsFixBB: Boolean;
    constructor Create;
    destructor Destroy; override;
    function NeedsDisjointFill: Boolean; override;
    function SupportsOpacityGroup: Boolean; override;
    procedure BeginOpacityGroup(const Opacity: TRealType;
      const Box: TRect2D; const LineWidth: TRealType); override;
    procedure EndOpacityGroup; override;
    procedure Poly(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching; const Transf: TTransf2D;
      const Closed: Boolean); override;
    procedure WriteHeader(ExtRect: TRect2D); override;
    procedure WriteFooter; override;
  end;

implementation

uses Math, ColorEtc, Output, Bitmaps;

// =====================================================================
// T_TikZ_Device
// =====================================================================

constructor T_TikZ_Device.Create;
begin
  inherited Create;
  DvipsFixBB := False;
  DefaultFontHeight := 5;
  OnBezier := Bezier;
  OnCircle := Circle;
  OnRotEllipse := RotEllipse;
  OnRotRect := RotRect;
  OnCircular := Circular;
  OnRotText := RotText;
  OnBitmap := Bitmap;
  fHasBezier := True;
  fHasClosedBezier := True;
  fHasArc := True;
  fHasSector := True;
  fHasSegment := True;
  fHasNativeHatching := False;
  fCurrLineColor := clNone;
  fCurrFillColor := clNone;
  fCurrTextColor := clNone;
  fCurrGradColor := clNone;
  fFadings := TStringList.Create;
end;

destructor T_TikZ_Device.Destroy;
begin
  fFadings.Free;
  inherited Destroy;
end;

{ A gradient whose two ends differ in opacity needs a fading; the fading
  would also act on the stroke, so the stroke is then drawn on its own. }
function T_TikZ_Device.GradientFading: Boolean;
begin
  Result := HasGradient(fPaint) and
    (Abs(ClampOpacity(fPaint.FillOpacity) -
    ClampOpacity(fPaint.GradientOpacity)) > 0.0005);
end;

function T_TikZ_Device.NeedsDisjointFill: Boolean;
begin
  Result := inherited NeedsDisjointFill or GradientFading;
end;

function T_TikZ_Device.SupportsOpacityGroup: Boolean;
begin
  Result := True;
end;

procedure T_TikZ_Device.BeginOpacityGroup(const Opacity: TRealType;
  const Box: TRect2D; const LineWidth: TRealType);
begin
  fGroupLineColor := fCurrLineColor;
  fGroupFillColor := fCurrFillColor;
  fGroupTextColor := fCurrTextColor;
  fGroupGradColor := fCurrGradColor;
  WriteLnStream('\begin{scope}[transparency group, opacity=' +
    OpacityText(Opacity) + ']');
end;

procedure T_TikZ_Device.EndOpacityGroup;
begin
  WriteLnStream('\end{scope}');
  fCurrLineColor := fGroupLineColor;
  fCurrFillColor := fGroupFillColor;
  fCurrTextColor := fGroupTextColor;
  fCurrGradColor := fGroupGradColor;
end;

{ Declares the fading of the current gradient once per picture and returns
  its name. A fading is a luminosity mask (white opaque, black clear) laid
  out like the matching TikZ shading. pgf declarations inside a picture
  need \pgfinterruptpicture. }
function T_TikZ_Device.DeclareFading: string;
var
  A0, A1: string;
begin
  A0 := OpacityText(fPaint.FillOpacity);
  A1 := OpacityText(fPaint.GradientOpacity);
  if fPaint.Gradient = fgRadial then Result := 'tpxfadeR'
  else Result := 'tpxfadeL';
  Result := Result + IntToStr(Round(ClampOpacity(fPaint.FillOpacity) * 1000)) +
    'x' + IntToStr(Round(ClampOpacity(fPaint.GradientOpacity) * 1000));
  if fFadings.IndexOf(Result) >= 0 then Exit;
  fFadings.Add(Result);
  WriteStream('\pgfinterruptpicture');
  if fPaint.Gradient = fgRadial then
    WriteStream(Format('\pgfdeclareradialshading{%ss}{\pgfpointorigin}' +
      '{gray(0bp)=(%s);gray(25bp)=(%s);gray(50bp)=(%s)}', [Result, A0, A1, A1]))
  else
    WriteStream(Format('\pgfdeclarehorizontalshading{%ss}{100bp}' +
      '{gray(0bp)=(%s);gray(25bp)=(%s);gray(75bp)=(%s);gray(100bp)=(%s)}',
      [Result, A0, A0, A1, A1]));
  WriteStream(Format('\pgfdeclarefading{%s}{\pgfuseshading{%ss}}',
    [Result, Result]));
  WriteLnStream('\endpgfinterruptpicture%');
end;

function FormatTikZNumber(const X: TRealType): string;
var
  Settings: TFormatSettings;
begin
  Settings := DefaultFormatSettings;
  Settings.DecimalSeparator := '.';
  Result := FloatToStrF(X, ffGeneral, 6, 0, Settings);
end;

{ Shading options for the current gradient fill (colours F and G). TikZ
  stretches its axis and radial shadings to the path's bounding box with
  the rules TPaintStyle describes. }
function T_TikZ_Device.ShadingOptions: string;
var
  Angle, UnitBP, L, D: TRealType;
  P1, P2, C0, C1: TPoint2D;
begin
  if fPaint.Gradient = fgRadial then
    Result := 'inner color=F, outer color=G'
  else
  begin
    Angle := fPaint.GradientAngle - 360 * Floor(fPaint.GradientAngle / 360);
    if Abs(Angle) < 1E-9 then
      Result := 'left color=F, right color=G'
    else
      Result := 'bottom color=F, top color=G, shading angle=' +
        FormatTikZNumber(Angle - 90);
  end;
  if not GradientFading then
  begin
    if not IsOpaque(fPaint.FillOpacity) then
      Result := Result + ', fill opacity=' + OpacityText(fPaint.FillOpacity);
    Exit;
  end;
  Result := Result + ', path fading=' + DeclareFading;
  if fPaint.Gradient = fgRadial then
    Result := Result + ', fit fading=true'
  else
  begin
    { Place the fading in picture coordinates so it follows the colour
      axis exactly: centred on the box, along the gradient direction, its
      50bp ramp scaled to the gradient length and its height to cover the
      box. }
    LinearGradientEnds(fPaint, P1, P2);
    P1 := TransformPoint2D(P1, T);
    P2 := TransformPoint2D(P2, T);
    C0 := TransformPoint2D(fPaint.Box.FirstEdge, T);
    C1 := TransformPoint2D(fPaint.Box.SecondEdge, T);
    UnitBP := StrToFloat(Format('%.2f', [1 / fFactorMM])) * 72 / 25.4;
    L := PointDistance2D(P1, P2);
    D := PointDistance2D(C0, C1);
    Result := Result + Format(', fit fading=false, fading transform=' +
      '{shift={(%s,%s)}, rotate=%s, xscale=%s, yscale=%s}',
      [FormatTikZNumber((P1.X + P2.X) / 2), FormatTikZNumber((P1.Y + P2.Y) / 2),
      FormatTikZNumber(RadToDeg(ArcTan2(P2.Y - P1.Y, P2.X - P1.X))),
      FormatTikZNumber(L * UnitBP / 50), FormatTikZNumber(D * UnitBP / 50)]);
  end;
end;

procedure T_TikZ_Device.WriteHeader(ExtRect: TRect2D);
begin
  fFadings.Clear;
  if fFactorMM = 0 then fFactorMM := 1;
//  if DvipsFixBB then
//    WriteLnStream(DvipsFixBB_RuleStr(ExtRect, fFactorMM));
  WriteLnStream('\begingroup%');
  WriteLnStream(Format('\providecommand{\tpxLineWidth}{%.6gmm}%%', [fLineWidthBase]));
  WriteLnStream(Format('\providecommand{\tpxTextSize}{%.6gpt}%%',
    [DefaultFontHeight / fFactorMM * 2.84527559]));
  WriteLnStream(Format('\providecommand{\tpxDashSize}{%.6gmm}%%', [fDashSize]));
  WriteLnStream(Format('\providecommand{\tpxDotSize}{%.6gmm}%%', [fDottedSize]));
  if DvipsFixBB then
    WriteLnStream('\beginpgfgraphicnamed{\pgfactualjobname}%');
  WriteStream(Format(
    '\begin{tikzpicture}[x=%.2fmm, y=%.2fmm, inner xsep=0pt, inner ysep=0pt, outer xsep=0pt, outer ysep=0pt', //ysep=-1.2pt
    [1 / fFactorMM, 1 / fFactorMM]));
  if fMiterLimit <> 10 then
    WriteStream(Format(
      ', miter limit = %.2f', [fMiterLimit]));
  WriteLnStream(']');
  WriteStream('\path[line width=0mm] ');
  WriteStreamPoint0(ExtRect.Left, ExtRect.Bottom);
  WriteStream(' rectangle +');
  WriteStreamPoint0(ExtRect.Right - ExtRect.Left, ExtRect.Top -
    ExtRect.Bottom);
  WriteLnStream(';');
end;

procedure T_TikZ_Device.WriteFooter;
begin
  WriteLnStream('\end{tikzpicture}%');
  if DvipsFixBB then
    WriteLnStream('\endpgfgraphicnamed');
  WriteLnStream('\endgroup%');
end;

{function TikZGetColor(Color: TColor): string;
var
  RGB: T_PS_RGB;
begin
  RGB := PS_RGB(Color);
  Result := Format('r%dg%db%d',
    [Round(RGB.R * 255), Round(RGB.G * 255), Round(RGB.B * 255)]);
end;}

procedure T_TikZ_Device.DefineColor(Color: TColor; ID: Char);
var
  RGB: T_PS_RGB;
begin
  case ID of
    'L': if Color = fCurrLineColor then Exit
      else fCurrLineColor := Color;
    'F': if Color = fCurrFillColor then Exit
      else fCurrFillColor := Color;
    'T': if Color = fCurrTextColor then Exit
      else fCurrTextColor := Color;
    'G': if Color = fCurrGradColor then Exit
      else fCurrGradColor := Color;
  end;
  RGB := PS_RGB(Color);
  WriteLnStream(Format('\definecolor{%s}{rgb}{%.3g,%.3g,%.3g}',
    [ID, RGB.R, RGB.G, RGB.B]));
end;

{\draw[cap=rect] (0,0 ) -- (1,0);
\draw[cap=butt] (0,.5) -- (1,.5);
\draw[cap=round] (0,1 ) -- (1,1);
\draw[join=round] (0,0) -- ++(.5,1) -- ++(.5,-1);
\draw[join=bevel] (1.25,0) -- ++(.5,1) -- ++(.5,-1);
\draw[join=miter] (2.5,0) -- ++(.5,1) -- ++(.5,-1);
\draw[miter limit=25]
[even odd rule]
[nonzero rule]}

procedure T_TikZ_Device.WriteAttr(
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching);
var
  NAttr: Integer;
  Shaded: Boolean;
  Shading: string;
  procedure AddAttr(const Attr: string);
  begin
    if NAttr = 0 then
      WriteStream(Attr)
    else
      WriteStream(', ' + Attr);
    Inc(NAttr);
  end;
begin
  if LineStyle <> liNone then
    if LineColor <> clDefault then DefineColor(LineColor, 'L')
    else DefineColor(clBlack, 'L');
  if FillColor <> clDefault then DefineColor(FillColor, 'F');
  Shaded := (FillColor <> clDefault) and HasGradient(fPaint);
  if Shaded then
  begin
    DefineColor(fPaint.GradientColor, 'G');
    Shading := ShadingOptions;
  end;
  WriteStream('\path');
  WriteStream('[');
  NAttr := 0;
  if LineStyle <> liNone then
  begin
    AddAttr(Format('line width=%.6g*\tpxLineWidth', [LineWidth]));
    AddAttr('draw=L');
  end;
  if Shaded then AddAttr(Shading)
  else if FillColor <> clDefault then AddAttr('fill=F');
  { Hatching lines belong to the fill and take its opacity. }
  if (LineStyle <> liNone) and fHatchPass and
    not IsOpaque(fPaint.FillOpacity) then
    AddAttr('draw opacity=' + OpacityText(fPaint.FillOpacity))
  else if (LineStyle <> liNone) and not fHatchPass and
    not IsOpaque(fPaint.LineOpacity) then
    AddAttr('draw opacity=' + OpacityText(fPaint.LineOpacity));
  if (FillColor <> clDefault) and not Shaded and
    not IsOpaque(fPaint.FillOpacity) then
    AddAttr('fill opacity=' + OpacityText(fPaint.FillOpacity));
  case LineStyle of
    liDotted:
      begin
        AddAttr(Format('dash pattern=on %.6g*\tpxLineWidth off \tpxDotSize',
          [LineWidth]));
      end;
    liDashed:
      begin
        AddAttr('dash pattern=on 2*\tpxDashSize off \tpxDashSize');
      end;
  end;
  WriteStream('] ');
end;

procedure T_TikZ_Device.HatchingLine(P0, P1: TPoint2D;
  const LineColor: TColor; const LineStyle: TLineStyle;
  const LineWidth: TRealType);
begin
  WriteStream(Format(' (%.2f,%.2f) -- (%.2f,%.2f)',
    [P0.X, P0.Y, P1.X, P1.Y]));
end;

procedure T_TikZ_Device.WriteHatchingLines(const Lines:
  TPointsSet2D;
  const HatchColor: TColor; const LineWidth: TRealType);
var
  I: Integer;
begin
  Lines.TransformPoints(T);
  fHatchPass := True;
  try
    WriteAttr(HatchColor, HatchColor, clDefault,
      liSolid, LineWidth, haNone);
  finally
    fHatchPass := False;
  end;
  for I := 0 to Lines.Count div 2 - 1 do
  begin
    HatchingLine(Lines[I * 2], Lines[I * 2 + 1],
      HatchColor, liSolid, LineWidth);
  end;
  WriteLnStream(';');
end;

procedure T_TikZ_Device.Poly(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D;
  const Closed: Boolean);
var
  P1, PPrev: TPoint2D;
  I: Integer;
  PathSt: string;
  procedure AddSt(const St: string);
  begin
    PathSt := PathSt + St;
  end;
  procedure AddPoint(P: TPoint2D);
  begin
    P := TransformPoint2D(P, T);
    AddSt(Format('(%.2f,%.2f)', [P.X, P.Y]));
  end;
begin
  if PP.Count = 0 then Exit;
  WriteAttr(LineColor, HatchColor, FillColor,
    LineStyle, LineWidth, Hatching);
  PathSt := '';
  PPrev := Point2D(MaxInt, MaxInt);
  for I := 0 to PP.Count - 1 do
  begin
    P1 := PP[I];
    if not IsSamePoint2D(P1, PPrev) then
    begin
      if I > 0 then AddSt(' -- ');
      AddPoint(P1);
      if (I mod 50) = 44 then AddSt(EOL);
    end;
    PPrev := P1;
  end;
  WriteStream(PathSt);
  if Closed then WriteStream(' -- cycle');
  WriteLnStream(';');
end;

procedure T_TikZ_Device.Bezier(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D;
  const Closed: Boolean);
var
  I: Integer;
  PathSt: string;
  procedure AddSt(const St: string);
  begin
    PathSt := PathSt + St;
  end;
  procedure AddPoint(P: TPoint2D);
  begin
    P := TransformPoint2D(P, T);
    AddSt(Format('(%.2f,%.2f)', [P.X, P.Y]));
  end;
begin
  WriteAttr(LineColor, HatchColor, FillColor,
    LineStyle, LineWidth, Hatching);
  PathSt := '';
  AddPoint(PP[0]);
  for I := 1 to PP.Count - 1 do
  begin
    case I mod 3 of
      1: AddSt(' .. controls ');
      2: AddSt(' and ');
      0: AddSt(' .. ');
    end;
    if (I mod 50) = 44 then AddSt(EOL);
    AddPoint(PP[I]);
  end;
  WriteStream(PathSt);
  if Closed then WriteStream(' -- cycle');
  WriteLnStream(';');
end;

procedure T_TikZ_Device.Circle(const CP: TPoint2D; const R:
  TRealType;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching);
begin
  WriteAttr(LineColor, HatchColor, FillColor,
    LineStyle, LineWidth, Hatching);
  WriteStreamPoint(CP);
  if fFactorMM = 0 then fFactorMM := 1;
  WriteLnStream(Format(' circle (%.2fmm);', [R / fFactorMM]));
end;

procedure T_TikZ_Device.RotEllipse(const CP: TPoint2D;
  const RX, RY, ARot: TRealType;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching);
begin
  WriteAttr(LineColor, HatchColor, FillColor,
    LineStyle, LineWidth, Hatching);
  WriteStreamPoint(CP);
  if ARot <> 0 then
  begin
    WriteStream(Format(' [rotate around={%d:',
      [Round(RadToDeg(ARot))]));
    WriteStreamPoint(CP);
    WriteStream('}]');
  end;
  if fFactorMM = 0 then fFactorMM := 1;
  WriteLnStream(Format(' ellipse (%.2fmm and %.2fmm);',
    [RX / fFactorMM, RY / fFactorMM]));
end;

procedure T_TikZ_Device.RotRect(const P: TPoint2D;
  const W, H, ARot: TRealType;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching);
begin
  WriteAttr(LineColor, HatchColor, FillColor,
    LineStyle, LineWidth, Hatching);
  WriteStreamPoint(P);
  if ARot <> 0 then
  begin
    WriteStream(Format(' [rotate around={%d:',
      [Round(RadToDeg(ARot))]));
    WriteStreamPoint(P);
    WriteStream('}]');
  end;
  WriteStream(' rectangle +');
  WriteStreamPoint0(W, H);
  WriteLnStream(';');
end;

procedure T_TikZ_Device.Circular(
  const CP: TPoint2D; R, SA, EA: TRealType;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Kind: TCircularKind);
var
  P: TPoint2D;
begin
  if SA > EA then
    if EA > 0 then
      SA := SA - 2 * Pi
    else
      EA := EA + 2 * Pi;
  P := Point2D(CP.X + R * Cos(SA), CP.Y + R * Sin(SA));
  WriteAttr(LineColor, HatchColor, FillColor,
    LineStyle, LineWidth, Hatching);
  if Kind = ci_Sector then
  begin
    WriteStreamPoint(CP);
    WriteStream(' -- ');
  end;
  WriteStreamPoint(P);
  if fFactorMM = 0 then fFactorMM := 1;
  WriteStream(Format(' arc (%d:%d:%.2fmm)',
    [Round(RadToDeg(SA)), Round(RadToDeg(EA)), R / fFactorMM]));
  if Kind <> ci_Arc then
    WriteStream(' -- cycle');
  WriteLnStream(';');
end;

procedure T_TikZ_Device.RotText(P: TPoint2D; H, ARot: TRealType;
  WideText: WideString; TeXText: AnsiString;
  const HAlignment: THAlignment;
  const VAlignment: TVAlignment;
  const LineColor: TColor;
  const FaceName: AnsiString;
  const Charset: TFontCharSet; const Style: TFontStyles);
var
  AnchorSt, St, SizeSt: string;
  Ratio: TRealType;
begin
  if LineColor <> clDefault then DefineColor(LineColor, 'T');
  WriteStream('\draw');
  if LineColor <> clDefault then WriteStream('[T] ');
  ShiftTeXTextPoint(P, H, ARot);
  WriteStreamPoint(P);
  AnchorSt := 'base';
  case HAlignment of
    ahLeft: AnchorSt := AnchorSt + ' west';
    ahCenter: ;
    ahRight: AnchorSt := AnchorSt + ' east';
  end;
  if Copy(AnchorSt, 1, 1) = ' ' then Delete(AnchorSt, 1, 1);
  if ARot <> 0 then
    AnchorSt := AnchorSt +
      Format(',rotate=%d', [Round(RadToDeg(ARot))]);
  WriteStream(' node[anchor=' + AnchorSt + ']');
  St := Get_TeXText(LineColor, Style, WideText, TeXText);
//  St := St + '\strut';
  if fFactorMM = 0 then fFactorMM := 1;
  SizeSt := '';
  if FontSizeInTeX and (DefaultFontHeight > 0) then begin
    Ratio := H / DefaultFontHeight;
    SizeSt := Format('\pgfmathsetlengthmacro{\tpxObjectFontSize}{%.6g*\tpxTextSize}' +
      '\pgfmathsetlengthmacro{\tpxObjectBaseline}{%.6g*\tpxTextSize}' +
      '\fontsize{\tpxObjectFontSize}{\tpxObjectBaseline}\selectfont ',
      [Ratio, Ratio * 1.2]);
  end
  else SizeSt := GetTeXTextFontSize(H, 1 / fFactorMM, FontSizeInTeX);
  WriteStream('{' + SizeSt + St + '}');
  WriteLnStream(';');
end;

procedure T_TikZ_Device.Bitmap(P: TPoint2D; W, H: TRealType;
  const KeepAspectRatio: Boolean; BitmapEntry: TObject);
var
  BE: TBitmapEntry;
  OutDir: string;
begin
  BE := BitmapEntry as TBitmapEntry;
  OutDir := ExtractFilePath(BE.GetFullLink);
  if not BE.RequirePNGJPEG(OutDir) then Exit;
  // pdfLaTeX includes the PNG or JPEG; the EPS copy only serves latex+dvips.
  if Bitmap2EpsAvailable then BE.RequireEPS(OutDir);
  WriteStream('\node at ');
  WriteStreamPoint(P);
  WriteStream('[anchor=south west]');
  if fFactorMM = 0 then fFactorMM := 1;
  WriteLnStream('{' + BE.GetIncludeGraphics(
    W / fFactorMM, H / fFactorMM, KeepAspectRatio,
    IncludePath) + '};');
end;

end.


unit DevTikZ;

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface

uses SysUtils, Geometry, Graphics, Devices;

type

// A class for TikZ output

  T_TikZ_Device = class(TFileDevice)
  protected
    fCurrLineColor, fCurrFillColor, fCurrTextColor: TColor;
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
      const LineColor: TColor;
      const FaceName: AnsiString;
      const Charset: TFontCharSet; const Style: TFontStyles);
    procedure Bitmap(P: TPoint2D; W, H: TRealType;
      const KeepAspectRatio: Boolean; BitmapEntry: TObject);
  public
    FontSizeInTeX: Boolean;
    DvipsFixBB: Boolean;
    constructor Create;
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
// T_TikZ_Device
// =====================================================================

constructor T_TikZ_Device.Create;
begin
  inherited Create;
  DvipsFixBB := False;
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
end;

procedure T_TikZ_Device.WriteHeader(ExtRect: TRect2D);
begin
  if fFactorMM = 0 then fFactorMM := 1;
//  if DvipsFixBB then
//    WriteLnStream(DvipsFixBB_RuleStr(ExtRect, fFactorMM));
  if DvipsFixBB then
    WriteLnStream('\beginpgfgraphicnamed{\jobname}%');
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
  WriteStream('\path');
  WriteStream('[');
  NAttr := 0;
  if LineStyle <> liNone then
  begin
    AddAttr(Format('line width=%.2fmm',
      [fLineWidthBase * LineWidth]));
    AddAttr('draw=L');
  end;
  if FillColor <> clDefault then AddAttr('fill=F');
  case LineStyle of
    liDotted:
      begin
        AddAttr(Format('dash pattern=on %.2fmm off %.2fmm',
          [fLineWidthBase * LineWidth, fDottedSize]));
      end;
    liDashed:
      begin
        AddAttr(Format('dash pattern=on %.2fmm off %.2fmm',
          [fDashSize * 2, fDashSize]));
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
  WriteAttr(HatchColor, HatchColor, clDefault,
    liSolid, LineWidth, haNone);
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
  const LineColor: TColor;
  const FaceName: AnsiString;
  const Charset: TFontCharSet; const Style: TFontStyles);
var
  AnchorSt, St: string;
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
  WriteStream('{' +
    GetTeXTextFontSize(H, 1 / fFactorMM, FontSizeInTeX)
    + St + '}');
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
  if not BE.RequireEPS(OutDir) then Exit;
  WriteStream('\node at ');
  WriteStreamPoint(P);
  WriteStream('[anchor=south west]');
  if fFactorMM = 0 then fFactorMM := 1;
  WriteLnStream('{' + BE.GetIncludeGraphics(
    W / fFactorMM, H / fFactorMM, KeepAspectRatio,
    IncludePath) + '};');
end;

end.


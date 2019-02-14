unit DevPGF;

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface

uses SysUtils, Geometry, Graphics, Devices;

type

  TPathKind = (path_Fill, path_Stroke, path_FillStroke);

// A class for Pgf output

  T_PGF_Device = class(TStreamDevice)
  protected
    fCurrColor: TColor;
    fCurrLineWidth: TRealType;
    fCurrLineStyle: TLineStyle;
    procedure WriteStreamPoint0(const X, Y: TRealType); override;
    function GetColor(Color: TColor): string;
    procedure WriteColor(Color: TColor);
    procedure WriteDash(LineStyle: TLineStyle);
    procedure WriteLineWidth(W: TRealType);
    procedure WritePathAttr(
      const LineColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType);
    procedure WritePathKind(const PathKind: TPathKind);
    function GetPathKindString(const PathKind: TPathKind): string;
    function GetPathKind(const FillColor: TColor;
      const LineStyle: TLineStyle): TPathKind;
{procedure WritePath(const PP, HatchPP: TPointsSet2D;
  PathProc: TPathProcPathKind;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType; const Hatching:
  THatching;
  const Closed: Boolean);}
  //procedure WritePoly(PP: TPointsSet2D;
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
    procedure RotText(P: TPoint2D; H, ARot: TRealType;
      WideText: WideString; TeXText: AnsiString;
      const HJustification: THJustification;
      const VJustification: TVJustification;
      const LineColor: TColor;
      const FaceName: AnsiString;
      const Charset: TFontCharSet; const Style: TFontStyles);
    procedure HatchingLine(P0, P1: TPoint2D;
      const LineColor: TColor; const LineStyle: TLineStyle;
      const LineWidth: TRealType); override;
    procedure WriteHatchingLines(const Lines: TPointsSet2D;
      const HatchColor: TColor; const LineWidth: TRealType);
      override;
  public
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

uses ColorEtc, Output;

// =====================================================================
// T_PGF_Device
// =====================================================================

constructor T_PGF_Device.Create;
begin
  inherited Create;
  DvipsFixBB := False;
  OnBezier := Bezier;
  OnCircle := Circle;
  OnRotEllipse := RotEllipse;
  OnRotText := RotText;
  fHasBezier := True;
  fHasClosedBezier := True;
  fHasArc := True;
  fHasSector := True;
  fHasSegment := True;
  fHasNativeHatching := False;
  fCurrColor := clNone;
  fDisjointFill := True;
end;

procedure T_PGF_Device.WriteStreamPoint0(const X, Y: TRealType);
begin
  WriteStream(Format('{\pgfxy(%.2f,%.2f)}', [X, Y]));
end;

function T_PGF_Device.GetColor(Color: TColor): string;
var
  RGB: T_PS_RGB;
begin
  if Color = fCurrColor then
  begin
    Result := '';
    Exit;
  end;
  fCurrColor := Color;
  RGB := PS_RGB(Color);
  Result := Format('\color[rgb]{%.5g,%.5g,%.5g}',
    [RGB.R, RGB.G, RGB.B]);
end;

procedure T_PGF_Device.WriteColor(Color: TColor);
begin
  WriteStream(GetColor(Color));
end;

procedure T_PGF_Device.WriteDash(LineStyle: TLineStyle);
begin
  if LineStyle = fCurrLineStyle then Exit;
  fCurrLineStyle := LineStyle;
  case LineStyle of
    liSolid:
      WriteStream('\pgfsetdash{}{0mm}');
    liDotted:
      WriteStream(Format('\pgfsetdash{{%.2fmm}{%.2fmm}}{0mm}',
        [fLineWidthBase * 2, fDottedSize]));
    liDashed:
      WriteStream(Format('\pgfsetdash{{%.2fmm}{%.2fmm}}{0mm}',
        [fDashSize * 2, fDashSize]));
  end;
end;

procedure T_PGF_Device.WriteLineWidth(W: TRealType);
begin
  if W = fCurrLineWidth then Exit;
  fCurrLineWidth := W;
  WriteStream(Format('\pgfsetlinewidth{%.2fmm}', [W]));
end;

procedure T_PGF_Device.WritePathAttr(
  const LineColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType);
begin
  if (FillColor <> clDefault) then
  begin
    WriteColor(FillColor);
  end;
  if LineStyle <> liNone then
  begin
    if LineColor <> clDefault then WriteColor(LineColor)
    else WriteColor(clBlack);
    WriteDash(LineStyle);
    if LineStyle <> liNone then
      WriteLineWidth(fLineWidthBase * LineWidth);
  end;
end;

procedure T_PGF_Device.WritePathKind(const PathKind: TPathKind);
begin
  case PathKind of
    path_Fill: WriteStream('\pgffill');
    path_Stroke: WriteStream('\pgfstroke');
    path_FillStroke: WriteStream('\pgffillstroke');
  end;
end;

function T_PGF_Device.GetPathKindString(const PathKind: TPathKind):
  string;
begin
  case PathKind of
    path_Fill: Result := '[fill]';
    path_Stroke: Result := '[stroke]';
    path_FillStroke: Result := '[fillstroke]';
  end;
end;

function T_PGF_Device.GetPathKind(
  const FillColor: TColor;
  const LineStyle: TLineStyle): TPathKind;
begin
  if FillColor <> clDefault then
    if LineStyle <> liNone then
      Result := path_FillStroke
    else
      Result := path_Fill
  else
    Result := path_Stroke;
end;

procedure T_PGF_Device.HatchingLine(P0, P1: TPoint2D;
  const LineColor: TColor; const LineStyle: TLineStyle;
  const LineWidth: TRealType);
begin
  WriteStream('\pgfline');
  WriteStreamPoint(P0);
  WriteStreamPoint(P1);
end;

procedure T_PGF_Device.WriteHatchingLines(const Lines:
  TPointsSet2D;
  const HatchColor: TColor; const LineWidth: TRealType);
var
  I: Integer;
begin
  if HatchColor = clDefault then
    WriteColor(clBlack)
  else
    WriteColor(HatchColor);
  WriteDash(liSolid);
  WriteLineWidth(fLineWidthBase * fHatchingLineWidth);
  Lines.TransformPoints(T);
  for I := 0 to Lines.Count div 2 - 1 do
  begin
    HatchingLine(Lines[I * 2], Lines[I * 2 + 1],
      HatchColor, liSolid, LineWidth);
    if Succ(I) mod 100 = 0 then
    begin
      WriteLnStream('');
      WriteStream(' ');
    end;
  end;
  WriteLnStream('');
end;

procedure T_PGF_Device.WriteHeader(ExtRect: TRect2D);
begin
  if fFactorMM = 0 then fFactorMM := 1;
  if DvipsFixBB then
    WriteLnStream(DvipsFixBB_RuleStr(ExtRect, fFactorMM));
  WriteLnStream(Format('\begin{pgfpicture}{%.2fmm}{%.2fmm}{%.2fmm}{%.2fmm}',
    [ExtRect.Left / fFactorMM, ExtRect.Bottom / fFactorMM,
    ExtRect.Right / fFactorMM, ExtRect.Top / fFactorMM]));
  WriteLnStream(Format(
    '\pgfsetxvec{\pgfpoint{%.2fmm}{0mm}}', [1 / fFactorMM]));
  WriteLnStream(Format(
    '\pgfsetyvec{\pgfpoint{0mm}{%.2fmm}}', [1 / fFactorMM]));
  if fMiterLimit <> 10 then
    if fMiterLimit < 1.01 then
      WriteLnStream('\pgfsetmiterlimit{1.01}')
    else
      WriteLnStream(Format('\pgfsetmiterlimit{%.2f}',
        [fMiterLimit]));
  WriteColor(clBlack);
  WriteLineWidth(fLineWidthBase);
  WriteDash(liSolid);
  WriteLnStream('');
end;

procedure T_PGF_Device.WriteFooter;
begin
  WriteLnStream('\end{pgfpicture}%');
end;

procedure T_PGF_Device.Poly(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D;
  const Closed: Boolean);
var
  I: Integer;
begin
  WritePathAttr(LineColor, FillColor, LineStyle, LineWidth);
  if PP.Count < 1 then Exit;
  WriteStream('\pgfmoveto');
  WriteStreamPointT(PP[0], Transf);
  for I := 1 to PP.Count - 1 do
  begin
    WriteStream('\pgflineto');
    WriteStreamPointT(PP[I], Transf);
    if I mod 100 = 0 then
    begin
      WriteLnStream('');
      WriteStream(' ');
    end;
  end;
  if Closed then
    WriteStream('\pgfclosepath');
  WritePathKind(GetPathKind(FillColor, LineStyle));
  WriteLnStream('')
end;

procedure T_PGF_Device.Bezier(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D;
  const Closed: Boolean);
var
  I: Integer;
begin
  WritePathAttr(LineColor, FillColor, LineStyle, LineWidth);
  if PP = nil then Exit;
  if PP.Count < 1 then Exit;
  WriteStream('\pgfmoveto');
  WriteStreamPointT(PP[0], Transf);
  for I := 0 to PP.Count div 3 - 1 do
  begin
    WriteStream('\pgfcurveto');
    WriteStreamPointT(PP[3 * I + 1], Transf);
    WriteStreamPointT(PP[3 * I + 2], Transf);
    WriteStreamPointT(PP[3 * I + 3], Transf);
    if Succ(I) mod 50 = 0 then
    begin
      WriteLnStream('');
      WriteStream(' ');
    end;
  end;
  if Closed then
    WriteStream('\pgfclosepath');
  WritePathKind(GetPathKind(FillColor, LineStyle));
  WriteLnStream('')
end;

procedure T_PGF_Device.Circle(const CP: TPoint2D; const R:
  TRealType;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching);
begin
  WritePathAttr(LineColor, FillColor, LineStyle, LineWidth);
  WriteStream('\pgfcircle');
  WriteStream(GetPathKindString(GetPathKind(FillColor,
    LineStyle)));
  WriteStreamPoint(CP);
  if fFactorMM = 0 then fFactorMM := 1;
  WriteStream(Format('{%.2fmm}', [R / fFactorMM]));
  WriteLnStream('')
end;

procedure T_PGF_Device.RotEllipse(const CP: TPoint2D;
  const RX, RY, ARot: TRealType;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching);
begin
  WritePathAttr(LineColor, FillColor, LineStyle, LineWidth);
  WriteStream('\pgfellipse'); //pgfpathellipse
  WriteStream(GetPathKindString(GetPathKind(FillColor,
    LineStyle)));
  WriteStreamPoint(CP);
  WriteStreamPoint(ShiftPoint(Point2D(0, 0), PolarVector(RX,
    ARot)));
  WriteStreamPoint(ShiftPoint(Point2D(0, 0), PolarVector(RY, ARot +
    Pi / 2)));
  WriteLnStream('')
end;

procedure T_PGF_Device.RotText(P: TPoint2D; H, ARot: TRealType;
  WideText: WideString; TeXText: AnsiString;
  const HJustification: THJustification;
  const VJustification: TVJustification;
  const LineColor: TColor;
  const FaceName: AnsiString;
  const Charset: TFontCharSet; const Style: TFontStyles);
begin
  if LineColor = clDefault then WriteStream(GetColor(clBlack));
  WriteStream('\pgfputat');
  ShiftTeXTextPoint(P, VJustification, H, ARot);
  WriteStreamPoint(P);
  if fFactorMM = 0 then fFactorMM := 1;
  WriteLnStream(Format('{\pgfbox[bottom,left]{%s}}',
    [GetTeXTextMakebox0(H, ARot, 1 / fFactorMM, LineColor,
      HJustification,
      VJustification, Style, WideText, TeXText)]));
end;

end.


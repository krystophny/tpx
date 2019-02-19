unit DevTeXPc;

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface

uses SysUtils, Geometry, Graphics, Devices, Math;

type

// A class for LaTeX picure environment output

  T_TeX_Picture_Device = class(TFileDevice)
  protected
    fPrec: Integer;
    function PointStr0(const X, Y: TRealType): string;
    function PointStr(const P: TPoint2D): string;
    function IsSamePointFF(
      P1, P2: TPoint2D; const Transf: TTransf2D): Boolean;
    procedure WriteStreamPoint0(const X, Y: TRealType); override;
    procedure WriteLineThickness0(const LineStyle: TLineStyle;
      const LineWidth: TRealType);
    procedure WriteLine(P0, P1: TPoint2D; const LineStyle:
      TLineStyle;
      const LineWidth: TRealType; const Transf: TTransf2D);
    procedure WriteCBezier(P0, P1, P2, P3: TPoint2D;
      const Transf: TTransf2D);
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
    procedure HatchingLine(P0, P1: TPoint2D;
      const LineColor: TColor; const LineStyle: TLineStyle;
      const LineWidth: TRealType); override;
    procedure WriteHatchingLines(const Lines: TPointsSet2D;
      const HatchColor: TColor; const LineWidth: TRealType);
      override;
  public
    DvipsFixBB: Boolean;
    FontSizeInTeX: Boolean;
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
// T_TeX_Picture_Device
// =====================================================================

constructor T_TeX_Picture_Device.Create;
begin
  inherited Create;
  DvipsFixBB := False;
  OnBezier := Bezier;
  OnCircle := Circle;
  OnRotEllipse := RotEllipse;
//  OnCircular := Circular;
  OnRotText := RotText;
  fHasBezier := True;
  fHasClosedBezier := True;
  fHasArc := True;
  fHasSector := True;
  fHasSegment := True;
  fHasNativeHatching := False;
  //fDisjointFill := True;
end;

function T_TeX_Picture_Device.PointStr0(const X, Y: TRealType):
  string;
begin
  Result := Format('(%s,%s)', [FF_N(X, fPrec), FF_N(Y, fPrec)]);
end;

function T_TeX_Picture_Device.PointStr(const P: TPoint2D): string;
begin
  Result := PointStr0(P.X, P.Y);
end;

function T_TeX_Picture_Device.IsSamePointFF(P1, P2: TPoint2D;
  const Transf: TTransf2D): Boolean;
begin
  P1 := TransformPoint2D(P1, Transf);
  P2 := TransformPoint2D(P2, Transf);
  Result := PointStr(P1) = PointStr(P2);
end;

procedure T_TeX_Picture_Device.WriteStreamPoint0(const X, Y:
  TRealType);
begin
  WriteStream(PointStr0(X, Y));
end;

procedure T_TeX_Picture_Device.WriteLineThickness0(
  const LineStyle: TLineStyle;
  const LineWidth: TRealType);
begin
  if LineStyle = liNone then Exit;
  if (LineWidth <= 1.5) then
    WriteStream('\thinlines')
  else
    WriteStream('\thicklines');
end;

procedure T_TeX_Picture_Device.WriteLine(P0, P1: TPoint2D;
  const LineStyle: TLineStyle;
  const LineWidth: TRealType; const Transf: TTransf2D);
begin
  if IsSamePointFF(P0, P1, Transf) then
  begin
    WriteStream('\put');
    WriteStreamPointT(P0, Transf);
    WriteStream('{\picsquare}');
    Exit;
  end;
  case LineStyle of
    liNone, liSolid:
      begin
        WriteStream('\lbezier');
        WriteStreamPointT(P0, Transf);
        WriteStreamPointT(P1, Transf);
      end;
    liDotted:
      begin
        WriteStream('\dottedline{');
        WriteStream(Format('%.2f',
          [fDottedSize * fFactorMM +
          fLineWidthBase * LineWidth * fFactorMM]));
        WriteStream('}');
        WriteStreamPointT(P0, Transf);
        WriteStreamPointT(P1, Transf);
      end;
    liDashed:
      begin
      // [33] is stretch - the persent of gap relative to dash length (1/3).
      // Dash length is twice fDashSize and gap is equal to fDashSize.
      // The real step is different because epic tries not to put partial dash
        WriteStream('\dashline[33]{');
        WriteStream(Format('%.2f',
          [fDashSize * fFactorMM * 2]));
        WriteStream('}');
        WriteStreamPointT(P0, Transf);
        WriteStreamPointT(P1, Transf);
      end;
  end;
end;

procedure T_TeX_Picture_Device.WriteCBezier(P0, P1, P2, P3:
  TPoint2D;
  const Transf: TTransf2D);
begin
  //?? sometimes \cbezier breaks
  if IsSamePointFF(P0, P1, Transf) and IsSamePointFF(P0, P2, Transf)
    and IsSamePointFF(P1, P3, Transf) then Exit;
  WriteStream('\cbezier');
  WriteStreamPointT(P0, Transf);
  WriteStreamPointT(P1, Transf);
  WriteStreamPointT(P2, Transf);
  WriteStreamPointT(P3, Transf);
end;

procedure T_TeX_Picture_Device.HatchingLine(P0, P1: TPoint2D;
  const LineColor: TColor; const LineStyle: TLineStyle;
  const LineWidth: TRealType);
begin
  if IsSamePointFF(P0, P1, IdentityTransf2D) then Exit;
  WriteStream('\drawline');
  WriteStreamPoint(P0);
  WriteStreamPoint(P1);
end;

procedure T_TeX_Picture_Device.WriteHatchingLines(const Lines:
  TPointsSet2D;
  const HatchColor: TColor; const LineWidth: TRealType);
var
  I: Integer;
begin
  WriteStream('\thinlines');
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

procedure T_TeX_Picture_Device.WriteHeader(ExtRect: TRect2D);
begin
  fPrec := 2;
  if fFactorMM = 0 then fFactorMM := 1;
  if DvipsFixBB then
    WriteLnStream(DvipsFixBB_RuleStr(ExtRect, fFactorMM));
  WriteLnStream(Format(
    '\setlength{\unitlength}{%.2f mm}%%', [1 / fFactorMM]));
  WriteStream('\begin{picture}');
  WriteStreamPoint0(ExtRect.Right - ExtRect.Left,
    ExtRect.Top - ExtRect.Bottom);
  WriteStreamPoint0(ExtRect.Left, ExtRect.Bottom);
  WriteLnStream('');
end;

procedure T_TeX_Picture_Device.WriteFooter;
begin
  WriteLnStream('\end{picture}%');
end;

procedure T_TeX_Picture_Device.Poly(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D;
  const Closed: Boolean);
var
  I: Integer;
begin
  if PP.Count < 2 then Exit;
  if (LineStyle = liNone) {and (Hatching <> haNone)} then Exit;
  WriteLineThickness0(LineStyle, LineWidth);
  for I := 0 to PP.Count - 2 do
  begin
    WriteLine(PP[I], PP[I + 1], LineStyle, LineWidth, Transf);
    if (I mod 100) = 99 then
    begin
      WriteLnStream('');
      WriteStream(' ');
    end;
  end;
  if Closed and not IsSamePointFF(PP[PP.Count - 1], PP[0], Transf)
    then
    WriteLine(PP[PP.Count - 1], PP[0], LineStyle, LineWidth,
      Transf);
  WriteLnStream('');
end;

procedure T_TeX_Picture_Device.Bezier(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D;
  const Closed: Boolean);
var
  I: Integer;
begin
  if PP.Count < 2 then Exit;
  if (LineStyle = liNone) {and (Hatching <> haNone)} then Exit;
  WriteLineThickness0(LineStyle, LineWidth);
  for I := 0 to (PP.Count - 4) div 3 do
    WriteCBezier(PP[I * 3], PP[I * 3 + 1], PP[I * 3 + 2], PP[I * 3
      + 3],
        Transf);
  if (I mod 50) = 49 then
  begin
    WriteLnStream('');
    WriteStream(' ');
  end;
  if Closed and not IsSamePointFF(PP[PP.Count - 1], PP[0], Transf)
    then
    WriteLine(PP[PP.Count - 1], PP[0], LineStyle, LineWidth,
      Transf);
  WriteLnStream('')
end;

procedure T_TeX_Picture_Device.Circle(const CP: TPoint2D; const R:
  TRealType;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching);
begin
  RotEllipse(CP, R, R, 0,
    LineColor, HatchColor, FillColor,
    LineStyle, LineWidth, Hatching);
end;

procedure T_TeX_Picture_Device.RotEllipse(const CP: TPoint2D;
  const RX, RY, ARot: TRealType;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching);
var
  PP: TPointsSet2D;
  T: TTransf2D;
  V: TVector2D;
  procedure WriteArc(P0, P1, P2: TPoint2D);
  begin
    WriteStream('\rqbezier');
    WriteStreamPoint(P0);
    WriteStreamPoint(P1);
    WriteStreamPoint(P2);
    WriteStream('(');
    WriteStream(Format('%.5f', [1 / Sqrt(2)]));
    WriteStream(')');
  end;
begin
  if (LineStyle = liNone) {and (Hatching <> haNone)} then Exit;
  WriteLineThickness0(LineStyle, LineWidth);
  T := Rotate2D(ARot);
  PP := TPointsSet2D.Create(8);
  V.X := RX;
  V.Y := RY;
  V := TransformVector2D(V, T);
  PP[0] := ShiftPoint(CP, V);
  PP[4] := ShiftPointScale(CP, V, -1);
  V.X := -RX;
  V.Y := RY;
  V := TransformVector2D(V, T);
  PP[2] := ShiftPoint(CP, V);
  PP[6] := ShiftPointScale(CP, V, -1);
  PP[1] := MidPoint(PP[0], PP[2]);
  PP[3] := MidPoint(PP[2], PP[4]);
  PP[5] := MidPoint(PP[4], PP[6]);
  PP[7] := MidPoint(PP[6], PP[0]);
  WriteArc(PP[1], PP[2], PP[3]);
  WriteArc(PP[3], PP[4], PP[5]);
  WriteArc(PP[5], PP[6], PP[7]);
  WriteArc(PP[7], PP[0], PP[1]);
  PP.Free;
  WriteLnStream('');
end;

procedure T_TeX_Picture_Device.Circular(const CP: TPoint2D; R, SA,
  EA: TRealType;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Kind: TCircularKind);
var
  J, NSeg: Integer;
  P0, P1, P2: TPoint2D;
  CX, ATmp,
    A, A0, A1, A2: TRealType;
  function GetPoint(A, R: TRealType): TPoint2D;
  begin
    Result := Point2D(CP.X + R * Cos(A),
      CP.Y - R * Sin(A));
  end;
begin
  if (LineStyle = liNone) {and (Hatching <> haNone)} then Exit;
  WriteLineThickness0(LineStyle, LineWidth);
  ATmp := SA;
  SA := -EA;
  EA := -ATmp;
  SA := SA - Floor(SA / (2 * Pi)) * 2 * Pi;
  EA := EA - Floor(EA / (2 * Pi)) * 2 * Pi;
  if EA < SA then EA := EA + 2 * Pi;
  A := EA - SA;
  if A < 2 * Pi / 3 then
    NSeg := 1
  else if A < 4 * Pi / 3 then
    NSeg := 2
  else
    NSeg := 3;
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
    WriteStream(FF_N(Cos(A), fPrec));
    WriteStream(')');
    if Kind = ci_Sector then
    begin
      WriteLine(CP, GetPoint(SA, R), LineStyle, LineWidth,
        IdentityTransf2D);
      WriteLine(CP, GetPoint(EA, R), LineStyle, LineWidth,
        IdentityTransf2D);
    end
    else if Kind = ci_Segment then
      WriteLine(GetPoint(SA, R), GetPoint(EA, R),
        LineStyle, LineWidth, IdentityTransf2D);
  end;
  WriteLnStream('');
end;

procedure T_TeX_Picture_Device.RotText(P: TPoint2D; H, ARot:
  TRealType;
  WideText: WideString; TeXText: AnsiString;
  const HAlignment: THAlignment;   
  const VAlignment: TVAlignment;
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

end.


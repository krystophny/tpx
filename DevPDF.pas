unit DevPDF;

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface

uses Classes, SysUtils, StrUtils, Geometry, Drawings, Math,
  Pieces, GObjects, Graphics, Devices, PdfDoc;


type

// A class for PDF export

  TPdfDevice = class(TStreamDevice)
  protected
    fDrawing2D: TDrawing2D;
    fPDF: TPdfDoc;
    FontName: string;
    procedure WriteAttr(const LineStyle: TLineStyle;
      const LineWidth: TRealType; const LineColor, FillColor:
      TColor;
      Closed: Boolean);
    procedure FinishPath(const LineStyle: TLineStyle;
      const FillColor: TColor; const Closed: Boolean);
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
    procedure RotText(P: TPoint2D; H, ARot: TRealType;
      WideText: WideString; TeXText: AnsiString;
      const HJustification: THJustification;
      const VJustification: TVJustification;
      const LineColor: TColor;
      const FaceName: AnsiString;
      const Charset: TFontCharSet; const Style: TFontStyles);
  public
    Light: Boolean;
    TextLabels: TStringList;
    constructor Create(Drawing: TDrawing2D);
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

uses ColorEtc, SysBasic, Output, PdfTypes, PdfFonts;

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

// =====================================================================
// TPdfDevice
// =====================================================================

constructor TPdfDevice.Create(Drawing: TDrawing2D);
begin
  inherited Create;
  fDrawing2D := Drawing;
  fPDF := TPdfDoc.Create;
  fDisjointFill := True;
  OnBezier := Bezier;
  OnRotText := RotText;
  fHasBezier := True;
  fHasClosedBezier := True;
  Light := False;
  TextLabels := TStringList.Create;
end;

destructor TPdfDevice.Destroy;
begin
  TextLabels.Free;
  fPDF.Free;
  inherited Destroy;
end;

procedure TPdfDevice.WriteAttr(const LineStyle: TLineStyle;
  const LineWidth: TRealType; const LineColor, FillColor: TColor;
  Closed: Boolean);
begin
  if FillColor <> clDefault then
    fPDF.Canvas.SetRGBFillColor(FillColor);
  if LineStyle = liNone then Exit;
  case LineStyle of
    liSolid: fPDF.Canvas.SetDash([0], 0);
    liDashed: SetPDF_Dash(fPDF.Canvas,
        [fDashSize * 2 * fFactorMM, fDashSize * fFactorMM], 0);
        //SetDash([DashSize * 2 * A,            DashSize * A], 0);
    liDotted: SetPDF_Dash(fPDF.Canvas,
        [fLineWidthBase * 2 * fFactorMM, fDottedSize * fFactorMM],
        0);
        //SetDash([LineWidth * 2 * A,            DottedSize * A], 0);
  end;
  fPDF.Canvas.SetLineWidth(fLineWidthBase * LineWidth * fFactorMM);
  if LineColor <> clDefault
    then fPDF.Canvas.SetRGBStrokeColor(LineColor)
  else fPDF.Canvas.SetRGBStrokeColor(clBlack);
  if fMiterLimit <> 10
    then _WriteString(Format('%.2g M'#10, [fMiterLimit]),
      fPDF.Canvas.Contents.Stream);
end;

procedure TPdfDevice.FinishPath(const LineStyle: TLineStyle;
  const FillColor: TColor; const Closed: Boolean);
begin
  if Closed then fPDF.Canvas.ClosePath;
  if FillColor <> clDefault then
    fPDF.Canvas.Fill; // nonzero winding fill rule
      //Eofill; // even-odd aka alternate fill rule
  if LineStyle <> liNone then fPDF.Canvas.Stroke;
end;

procedure TPdfDevice.HatchingLine(P0, P1: TPoint2D;
  const LineColor: TColor; const LineStyle: TLineStyle;
  const LineWidth: TRealType);
begin
  fPDF.Canvas.MoveTo(P0.X, P0.Y);
  fPDF.Canvas.LineTo(P1.X, P1.Y);
end;

procedure TPdfDevice.WriteHatchingLines(const Lines: TPointsSet2D;
  const HatchColor: TColor; const LineWidth: TRealType);
var
  I: Integer;
begin
  if HatchColor <> clDefault
    then fPDF.Canvas.SetRGBStrokeColor(HatchColor)
  else fPDF.Canvas.SetRGBStrokeColor(clBlack);
  fPDF.Canvas.SetDash([0], 0);
  fPDF.Canvas.SetLineWidth(fLineWidthBase * LineWidth * fFactorMM);
  Lines.TransformPoints(T);
  for I := 0 to Lines.Count div 2 - 1 do
    HatchingLine(Lines[I * 2], Lines[I * 2 + 1],
      HatchColor, liSolid, LineWidth);
  fPDF.Canvas.Stroke;
end;

procedure TPdfDevice.WriteHeader(ExtRect: TRect2D);
var
  Dest: TPdfDestination;
begin
  ExtRect := TransformRect2D(ExtRect, fT);
    // Transform drawing rectangle
  fPDF.CompressionMethod := cmFlateDecode; // Compress PDF
  begin
    fPDF.NewDoc;
    fPDF.DefaultPageWidth := Ceil(ExtRect.Right - ExtRect.Left);
    fPDF.DefaultPageHeight := Ceil(ExtRect.Top - ExtRect.Bottom);
    fPDF.AddPage;
    Dest := fPDF.CreateDestination;
    with Dest do
    begin
      DestinationType := dtXYZ;
      Left := -10;
      Top := -10;
      Zoom := 1;
    end;
    fPDF.Root.OpenAction := Dest;
  end;
  TextLabels.Clear;
end;

procedure TPdfDevice.WriteFooter;
begin
  fPDF.SaveToStream(fStream);
end;

procedure TPdfDevice.Poly(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D;
  const Closed: Boolean);
var
  I: Integer;
  P: TPoint2D;
begin
  WriteAttr(LineStyle, LineWidth, LineColor, FillColor, Closed);
  P := TransformPoint2D(PP[0], MultTransf(Transf));
  fPDF.Canvas.MoveTo(P.X, P.Y);
  for I := 1 to PP.Count - 1 do
  begin
    P := TransformPoint2D(PP[I], MultTransf(Transf));
    fPDF.Canvas.LineTo(P.X, P.Y);
  end;
  FinishPath(LineStyle, FillColor, Closed);
end;

procedure TPdfDevice.Bezier(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D;
  const Closed: Boolean);
var
  I: Integer;
  P0, P1, P2, P3: TPoint2D;
begin
  WriteAttr(LineStyle, LineWidth, LineColor, FillColor, Closed);
  P0 := TransformPoint2D(PP[0], MultTransf(Transf));
  fPDF.Canvas.MoveTo(P0.X, P0.Y);
  for I := 0 to PP.Count div 3 - 1 do
  begin
    P1 := TransformPoint2D(PP[I * 3 + 1], MultTransf(Transf));
    P2 := TransformPoint2D(PP[I * 3 + 2], MultTransf(Transf));
    P3 := TransformPoint2D(PP[I * 3 + 3], MultTransf(Transf));
    fPDF.Canvas.CurveToC(P1.X, P1.Y, P2.X, P2.Y, P3.X, P3.Y);
  end;
  FinishPath(LineStyle, FillColor, Closed);
end;

procedure TPdfDevice.RotText(
  P: TPoint2D; H, ARot: TRealType;
  WideText: WideString; TeXText: AnsiString;
  const HJustification: THJustification;
  const VJustification: TVJustification;
  const LineColor: TColor;
  const FaceName: AnsiString;
  const Charset: TFontCharSet; const Style: TFontStyles);
var
  D: TVector2D;
  T: TTransf2D;
  fDescent: TRealType;
begin
  if Light then
  begin
    ShiftTeXTextPoint(P, VJustification, H, ARot);
    if fFactorMM = 0 then fFactorMM := 1;
    TextLabels.Add(Format('\put(%.2f,%.2f){%s}', [P.X, P.Y,
      GetTeXTextMakebox0(
        H, ARot, 1 / fFactorMM, LineColor, HJustification,
        VJustification, Style, WideText, TeXText)]));
    Exit;
  end;
  fPDF.Canvas.SetFont('Times-Roman', H);
    //SetFont('Arial', HText);
  case HJustification of
    jhLeft: D.X := 0;
    jhCenter: D.X := -fPDF.Canvas.TextWidth(WideText) / 2;
    jhRight: D.X := -fPDF.Canvas.TextWidth(WideText);
  end;
  fDescent := 0.216; // - for Times-Roman 0.195?
    //fDescent := 0.212; // - for Arial
    //See PdfFonts: TIMES_DISC_INT_TABLE (KEY: 'Descent'; VAL: -216),
  case VJustification of
    jvBottom: D.Y := 0 + fDescent;
    jvCenter: D.Y := -0.5 + fDescent;
    jvTop: D.Y := -1 + fDescent;
    jvBaseline: D.Y := 0;
  end;
  D.Y := D.Y * H;
  if LineColor <> clDefault
    then fPDF.Canvas.SetRGBFillColor(LineColor)
  else fPDF.Canvas.SetRGBFillColor(clBlack);
  if ARot <> 0 then
  begin
    T := Rotate2D(ARot);
    D := TransformVector2D(D, T);
  end;
    //P := ShiftPoint(ConvertPnt(P), D);
  P := ShiftPoint(P, D);
  fPDF.Canvas.BeginText;
  if ARot = 0 then fPDF.Canvas.MoveTextPoint(P.X, P.Y)
  else SetPDF_TextMatrix(fPDF.Canvas,
      MultiplyTransform2D(T, Translate2D(P.X, P.Y)));
  fPDF.Canvas.ShowText(WideText);
  fPDF.Canvas.EndText;
end;

end.


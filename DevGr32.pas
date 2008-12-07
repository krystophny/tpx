unit DevGr32;

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface

uses Classes, SysUtils, StrUtils,
{$IFDEF VER140}
  Gr32, Gr32_Polygons,
{$ENDIF}
  Geometry, Drawings, Pieces, GObjects, Graphics, Devices;

type

// A class for bmp export. Based on Graphics32 library

  TGr32Device = class(TStreamDevice)
  protected
    fDrawing2D: TDrawing2D;
    fBitmap: TBitmap32;
    fPolygon, fOutline: TPolygon32;
    procedure WriteLine(P0, P1: TPoint2D; W: TRealType; Color:
      TColor);
    procedure WriteBitmapPolygon(Polygon: TPolygon32; Color:
      TColor);
    procedure FillPolygon(
      const PP: TPointsSet2D; const Closed: Boolean;
      const Transf: TTransf2D);
    procedure WriteHatchingLines(const Lines: TPointsSet2D;
      const HatchColor: TColor; const LineWidth: TRealType);
      override;
    procedure RotText(P: TPoint2D; H, ARot: TRealType;
      WideText: WideString; TeXText: AnsiString;
      const HAlignment: THAlignment;
      const LineColor: TColor;
      const FaceName: AnsiString;
      const Charset: TFontCharSet; const Style: TFontStyles);
    procedure Bitmap(P: TPoint2D; W, H: TRealType;
      const KeepAspectRatio: Boolean; BitmapEntry: TObject);
    procedure GenPath(const GP: TGenericPath;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching; const Transf: TTransf2D);
  public
    DevEOL: string;
    constructor Create(Drawing: TDrawing2D);
    destructor Destroy; override;
    procedure Poly(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching; const Transf: TTransf2D;
      const Closed: Boolean); override;
    procedure WriteHeader(ExtRect: TRect2D); override;
    procedure WriteFooter; override;
    function StoreToFileBMP(const FileName: string): Boolean;
    function StoreToFilePNG(const FileName: string): Boolean;
  end;

implementation

uses ColorEtc, SysBasic,
{$IFDEF VER140}
  pngimage, Gr32Add,
{$ENDIF}
  Math, DateUtils, Types, Bitmaps;

function DashedOutline(const SD1, SD2: Single;
  const Polygon: TPolygon32): TPolygon32;
var
  J: Integer;
  DD1, DD2: Gr32.TFixed;
  TmpPoly: TPolygon32;
  procedure FillSinglePoly(const PP: TArrayOfFixedPoint);
  var
    I, N: Integer;
    D, D0, D1, A0, A1: Single;
    P00, P11, P0, P1: TFixedPoint;
    function Get(const I: Integer): TFixedPoint;
    begin
      if I < Length(PP) then
        Result := PP[I]
      else
        Result := PP[0];
    end;
  begin
    if Length(PP) < 2 then Exit;

    if Polygon.Closed then
      N := Length(PP) + 1
    else
      N := Length(PP);
    D0 := 0;
    for I := 0 to N - 2 do
    begin
      P00 := Get(I);
      P11 := Get(I + 1);
      D := Hypot(P11.X - P00.X, P11.Y - P00.Y);
      while (D0 < D) and (D1 < D) do
      begin
        D1 := D0 + DD1;
        if D > 0 then A0 := Max(D0 / D, 0)
        else A0 := 0;
        if D > 0 then A1 := Min(D1 / D, 1)
        else A1 := 1;
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
  DD1 := Gr32.Fixed(SD1);
  DD2 := Gr32.Fixed(SD2);
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

// =====================================================================
// TGr32Device
// =====================================================================

constructor TGr32Device.Create(Drawing: TDrawing2D);
begin
  inherited Create;
  fDrawing2D := Drawing;
  fDisjointFill := True;
  OnRotText := RotText;
  OnBitmap := Bitmap;
  OnGenPath := GenPath;
  fHasNativeHatching := False;
  fBitmap := TBitmap32.Create;
  fPolygon := TPolygon32.Create;
  fOutline := TPolygon32.Create;
end;

destructor TGr32Device.Destroy;
begin
  fBitmap.Free;
  fPolygon.Free;
  fOutline.Free;
  inherited Destroy;
end;

procedure TGr32Device.WriteLine(P0, P1: TPoint2D;
  W: TRealType; Color: TColor);
var
  TmpPoly: TPolygon32;
begin
  if IsSamePoint2D(P0, P1) then Exit;
  fPolygon.Clear;
  fPolygon.Closed := False;
  fPolygon.Add(FixedPoint(P0.X, P0.Y));
  fPolygon.Add(FixedPoint(P1.X, P1.Y));
  TmpPoly := fPolygon.Outline;
  try
    fOutline.Free;
    fOutline := TmpPoly.Grow(Gr32.Fixed(W / 2), 0);
  finally
    TmpPoly.Free;
  end;
  WriteBitmapPolygon(fOutline, Color);
end;

procedure TGr32Device.WriteBitmapPolygon(Polygon:
  TPolygon32; Color: TColor);
begin
  Polygon.Antialiased := True; //False;
  Polygon.DrawFill(fBitmap, Color32(Color))
end;

procedure TGr32Device.FillPolygon(
  const PP: TPointsSet2D; const Closed: Boolean;
  const Transf: TTransf2D);
var
  I, N1: Integer;
  P: TPoint2D;
begin
  with fBitmap do
  begin
    fPolygon.Clear;
    fPolygon.Closed := Closed;
    N1 := PP.Count - 1;
    if Closed then
      while (N1 > 0) and IsSamePoint2D(PP[0], PP[N1]) do
        Dec(N1);
    for I := 0 to N1 do
    begin
      P := TransformPoint2D(PP[I], MultTransf(Transf));
      fPolygon.Add(FixedPoint(P.X, P.Y));
    end;
  end;
end;

procedure TGr32Device.WriteHatchingLines(const Lines: TPointsSet2D;
  const HatchColor: TColor; const LineWidth: TRealType);
var
  Color: TColor;
  I: Integer;
begin
  Lines.TransformPoints(fT);
  for I := 0 to Lines.Count div 2 - 1 do
  begin
    if HatchColor = clDefault then
      Color := clBlack
    else
      Color := HatchColor;
    WriteLine(Lines[I * 2], Lines[I * 2 + 1],
      fLineWidthBase * LineWidth * fFactorMM, Color);
  end;
end;

procedure TGr32Device.WriteHeader(ExtRect: TRect2D);
begin
  fBitmap.BeginUpdate;
  ExtRect := TransformRect2D(ExtRect, fT);
  fBitmap.Width := Round(ExtRect.Right - ExtRect.Left);
  fBitmap.Height := Round(ExtRect.Bottom - ExtRect.Top);
  fBitmap.Clear(clWhite32);
  fBitmap.PenColor := clBlack32;
end;

procedure TGr32Device.WriteFooter;
begin
  fBitmap.EndUpdate;
end;

procedure TGr32Device.Poly(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D;
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
    FillPolygon(PP, Closed, Transf);
    fPolygon.FillMode := pfWinding; //pfAlternate
    WriteBitmapPolygon(fPolygon, FillColor);
  end;
  if LineStyle = liNone then Exit;
  FillPolygon(PP, Closed, Transf);
  with fBitmap do
  begin
    case LineStyle of
      liSolid: TmpPoly := fPolygon.Outline;
      liDashed:
        TmpPoly := DashedOutline(fDashSize * 2 * fFactorMM,
          fDashSize * fFactorMM, fPolygon);
      liDotted:
        TmpPoly := DashedOutline(fLineWidthBase * LineWidth *
          fFactorMM,
          fDottedSize * fFactorMM, fPolygon)
    end;
    W := fLineWidthBase * LineWidth * fFactorMM;
  end;
  try
    fOutline.Free;
    if fMiterLimit <= 0 then fMiterLimit := 1;
    fOutline := TmpPoly.Grow(Gr32.Fixed(W / 2),
      1 - 2 / Sqr(fMiterLimit));
        //Link between MiterLimit and EdgeSharpness
    fOutline.FillMode := pfWinding;
  finally
    TmpPoly.Free;
  end;
  WriteBitmapPolygon(fOutline, LineColor);
end;

procedure TGr32Device.RotText(
  P: TPoint2D; H, ARot: TRealType;
  WideText: WideString; TeXText: AnsiString;
  const HAlignment: THAlignment;
  const LineColor: TColor;
  const FaceName: AnsiString;
  const Charset: TFontCharSet; const Style: TFontStyles);
var
  FontH: TRealType;
begin
  fBitmap.PenColor := clBlack32;
  if LineColor <> clDefault then
    fBitmap.Font.Color := LineColor
  else
    fBitmap.Font.Color := clBlack;
  FontH := H;
  fBitmap.Font.Height := Round(FontH);
  RenderTextW_Rot(fBitmap,
    Round(P.X), Round(P.Y), WideText, 2 {0-4},
    Color32(fBitmap.Font.Color), FaceName, Round(FontH),
    fsBold in Style, fsItalic in Style, Charset,
    Ord(HAlignment), 
    Round(RadToDeg(ARot) * 10));
end;

procedure TGr32Device.Bitmap(P: TPoint2D; W, H: TRealType;
  const KeepAspectRatio: Boolean; BitmapEntry: TObject);
var
  DstRect, SrcRect: TRect;
  Src: TBitmap32;
begin
  Src := TBitmap32.Create;
  try
    Src.Assign((BitmapEntry as TBitmapEntry).Bitmap);
    SrcRect := Rect(0, 0, Src.Width, Src.Height);
    DstRect := Rect(Round(P.X), Round(P.Y - H),
      Round(P.X + W), Round(P.Y));
    fBitmap.Draw(DstRect, SrcRect, Src);
  finally
    Src.Free;
  end;
end;

procedure TGr32Device.GenPath(const GP: TGenericPath;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D);
var
  PP: TPointsSet2D;
  LinGP: TGenericPath;
  Kind: TPathItemKind;
  StartP, CurrP, P1, P2, P3: TPoint2D;
begin
  PP := TPointsSet2D.Create(0);
  LinGP := TGenericPath.Create(PP);
  try
    GP.Linearize(LinGP, fApproximationPrecision);
    LinGP.StartIterations;
    while LinGP.GetNext(Kind, P1, P2, P3) do
      case Kind of
        pik_MoveTo:
          begin

            StartP := P1;
            CurrP := P1;
          end;
        pik_LineTo:
          begin

            CurrP := P1;
          end;
        pik_Close:
          begin

            CurrP := StartP;
          end;
      end;
  finally
    PP.Free;
    LinGP.Free;
  end;
end;

function TGr32Device.StoreToFileBMP(const FileName: string):
  Boolean;
var
  aBitmap: Graphics.TBitmap;
begin
  aBitmap := Graphics.TBitmap.Create;
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

function TGr32Device.StoreToFilePNG(const FileName: string):
  Boolean;
var
  aPNG: TPNGObject;
  aBitmap: Graphics.TBitmap;
  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
  pHYsChunk: TChunkpHYs;
begin
  aPNG := TPNGObject.Create;
  aBitmap := Graphics.TBitmap.Create;
  Result := False;
  try
    //aBitmap.PixelFormat := pf32bit;
    aBitmap.Assign(fBitmap);
    aPNG.Assign(aBitmap);
    aPNG.AddztXt('Caption', fDrawing2D.Caption); // AddtEXt('
    aPNG.AddztXt('Comment', fDrawing2D.Comment);
    aPNG.AddztXt('Creator', 'TpX drawing tool');
    with TChunktIME(aPNG.Chunks.Add(TChunktIME)) do
    begin
      DecodeDateTime(Now, AYear, AMonth, ADay,
        AHour, AMinute, ASecond, AMilliSecond);
      Year := AYear;
      Month := AMonth;
      Day := ADay;
      Hour := AHour;
      Minute := AMinute;
      Second := ASecond;
    end;
    pHYsChunk := aPNG.Chunks.Add(TChunkpHYs) as TChunkpHYs;
    if fDrawing2D.BitmapRes > 0 then
      pHYsChunk.PPUnitX := Round(fDrawing2D.BitmapRes)
    else pHYsChunk.PPUnitX := 1;
    pHYsChunk.PPUnitY := pHYsChunk.PPUnitX;
    pHYsChunk.UnitType := utMeter;
    aPNG.SaveToFile(FileName);
  finally
    aPNG.Free;
    aBitmap.Free;
    Result := FileExists(FileName);
  end;
end;



end.


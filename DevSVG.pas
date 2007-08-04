unit DevSVG;

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface

uses Classes, SysUtils, StrUtils, Math, Geometry, Drawings,
  Pieces, GObjBase, GObjects, Graphics, Devices, XmlOut;

type

// A class for SVG export

  TSvgDevice = class(TStreamDevice)
  protected
    fDrawing2D: TDrawing2D;
    fXML: TXmlOutput;
    fPattIDs: TStringList;
    fGlobalFaceName: string;
    fGlobalDescent: TRealType;
    procedure SetStream(AStream: TStream); override;
    procedure RegisterPatt(const Hatching: THatching;
      const HatchColor, FillColor: TColor);
    procedure RegisterPatterns;
    procedure WritePrimitiveAttr0(const LineColor,
      HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching;
      MiterLimit: TRealType);
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
    procedure Rect(const P: TPoint2D;
      const W, H: TRealType;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching);
    procedure Circular(const CP: TPoint2D; R, SA, EA: TRealType;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching; const Kind: TCircularKind);
    procedure RotText(P: TPoint2D; H, ARot: TRealType;
      WideText: WideString; TeXText: AnsiString;
      const HJustification: THJustification;
      const VJustification: TVJustification;
      const LineColor: TColor;
      const FaceName: AnsiString;
      const Charset: TFontCharSet; const Style: TFontStyles);
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
  end;

implementation

uses ColorEtc, SysBasic;

function GetPattID(Hatching: THatching;
  HatchColor, FillColor: TColor): string;
var
  J: Integer;
begin
  case Hatching of
    haHorizontal: Result := 'haH';
    haVertical: Result := 'haV';
    haFDiagonal: Result := 'haFD';
    haBDiagonal: Result := 'haBD';
    haCross: Result := 'haC';
    haDiagCross: Result := 'haDC';
  else
    Result := 'haNone';
  end;
  if HatchColor <> clDefault then
    Result := Result + ColorToHtml(HatchColor);
  J := Pos('#', Result);
  if J > 0 then Delete(Result, J, J);
  if FillColor <> clDefault then
    Result := Result + 'F' + ColorToHtml(FillColor);
end;

// =====================================================================
// TSvgDevice
// =====================================================================

constructor TSvgDevice.Create(Drawing: TDrawing2D);
begin
  inherited Create;
  fXML := TXmlOutput.Create;
  DevEOL := EOL; // + '%';
  fXML.EOL_Str := DevEOL;
  fDrawing2D := Drawing;
  fPattIDs := TStringList.Create;
  OnBezier := Bezier;
  OnCircle := Circle;
  OnRotEllipse := RotEllipse;
  OnRect := Rect;
  OnCircular := Circular;
  OnRotText := RotText;
  fHasBezier := True;
  fHasClosedBezier := True;
  fHasArc := True;
  fHasSector := True;
  fHasSegment := True;
  fHasNativeHatching := True;
end;

destructor TSvgDevice.Destroy;
begin
  fXML.Free;
  fPattIDs.Free;
  inherited Destroy;
end;

procedure TSvgDevice.SetStream(AStream: TStream);
begin
  inherited SetStream(AStream);
  fXML.SetStream(AStream, 0);
end;

procedure TSvgDevice.RegisterPatt(const Hatching: THatching;
  const HatchColor, FillColor: TColor);
var
  ID, PathSt, HatchColorSt, FillColorSt: string;
  HV, D, Size: TRealType;
begin
  ID := GetPattID(Hatching, HatchColor, FillColor);
  if fPattIDs.IndexOf(ID) >= 0 then Exit;
  fPattIDs.Add(ID);
  fXML.OpenTag('pattern');
  fXML.AddAttribute('id', ID);
  HV := fHatchingStep * fFactorMM / 2;
  D := fHatchingStep * fFactorMM * Sqrt(2);
  case Hatching of
    haHorizontal:
      begin
        Size := HV * 2;
        PathSt := Format('M 0,%.2f L %.2f,%.2f', [HV, HV * 2, HV]);
      end;
    haVertical:
      begin
        Size := HV * 2;
        PathSt := Format('M %.2f,0 L %.2f,%.2f', [HV, HV, HV * 2])
      end;
    haFDiagonal:
      begin
        Size := D;
        PathSt := Format('M 0,0 L %.2f,%.2f', [D, D])
      end;
    haBDiagonal:
      begin
        Size := D;
        PathSt := Format('M %.2f,0 L 0,%.2f', [D, D])
      end;
    haCross:
      begin
        Size := HV * 2;
        PathSt :=
          Format('M 0,%.2f L %.2f,%.2f M %.2f,0 L %.2f,%.2f',
          [HV, HV * 2, HV, HV, HV, HV * 2])
      end;
    haDiagCross:
      begin
        Size := D;
        PathSt := Format('M 0,0 L %.2f,%.2f M %.2f,0 L 0,%.2f',
          [D, D, D, D])
      end;
  else
    begin
      Size := HV * 2;
      PathSt := Format('M 0,%.2f L %.2f,%.2f', [HV, HV * 2, HV]);
    end;
  end;
  if HatchColor = clDefault then
    HatchColorSt := 'black'
  else
    HatchColorSt := ColorToHtml(HatchColor);
  if FillColor <> clDefault then
    FillColorSt := ColorToHtml(FillColor);
  fXML.AddAttribute('width', FloatToStr(Size));
  fXML.AddAttribute('height', FloatToStr(Size));
  fXML.AddAttribute('patternUnits', 'userSpaceOnUse');
  if FillColor <> clDefault then
  begin
    fXML.OpenTag('path');
    fXML.AddAttribute('d',
      Format('M 0,0 L 0,%.2f L %.2f,%.2f L %.2f,0 L 0,0 Z',
      [Size, Size, Size, Size]));
    fXML.AddAttribute('fill', FillColorSt);
    fXML.AddAttribute('stroke', 'none');
    fXML.AddAttribute('stroke-width', '0');
    fXML.CloseTag;
  end;
  begin
    fXML.OpenTag('path');
    fXML.AddAttribute('d', PathSt);
    fXML.AddAttribute('fill', 'none');
    fXML.AddAttribute('stroke', HatchColorSt);
    fXML.AddAttribute('stroke-width',
      Format('%.2f', [fLineWidthBase
      * fHatchingLineWidth * fFactorMM]));
    fXML.CloseTag;
  end;
  fXML.CloseTag;
end;

procedure TSvgDevice.RegisterPatterns;
var
  TmpIter: TGraphicObjIterator;
  // Recursive registering
  procedure RegisterPatterns0(const Iter: TGraphicObjIterator;
    const DoShowProgress: Boolean);
  var
    TmpIter: TGraphicObjIterator;
    Prim: TPrimitive2D;
    TmpObj: TGraphicObject;
    I: Integer;
  begin
    I := 0;
    TmpObj := Iter.First as TPrimitive2D;
    while TmpObj <> nil do
    begin
      try
        if TmpObj is TContainer2D then
        begin
          TmpIter := (TmpObj as TContainer2D).Objects.GetIterator;
          try
            RegisterPatterns0(TmpIter, False);
          finally
            TmpIter.Free;
          end;
        end
        else if TmpObj is TPrimitive2D then
        begin
          Prim := TmpObj as TPrimitive2D;
          if Prim.Hatching <> haNone then
            RegisterPatt(Prim.Hatching, Prim.HatchColor,
              Prim.FillColor);
        end;
        TmpObj := Iter.Next;
        if DoShowProgress then
        begin
          Inc(I);
          if I mod 100 = 0 then ShowProgress(I / Iter.Count);
        end;
      except
        MessageBoxError(TmpObj.ClassName);
      end;
    end;
  end;
begin
  TmpIter := fDrawing2D.ObjectsIterator;
  try
    RegisterPatterns0(TmpIter, True);
  finally
    TmpIter.Free;
  end;
end;

procedure TSvgDevice.WriteHeader(ExtRect: TRect2D);
begin
  {fXML.Doctype := 'svg PUBLIC "-//W3C//DTD SVG 1.1//EN"' + DevEOL
    + ' "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"';}
  //  <?xml version="1.0" standalone="no"?>
  //fXML.LoadXML('<svg/>');
  fXML.AddProcessingInstruction('xml',
    'version="1.0" encoding="UTF-8"');
  fXML.OpenTag('svg');
  //<!-- svg width="8cm" height="4cm" viewBox="0 0 800 400" -->
  fXML.AddAttribute('version', '1.1');
  ExtRect := TransformRect2D(ExtRect, fT);
  fXML.AddAttribute('width', Format('%.2fmm',
    [(ExtRect.Right - ExtRect.Left) / fFactorMM]));
  fXML.AddAttribute('height', Format('%.2fmm',
    [Abs(ExtRect.Top - ExtRect.Bottom) / fFactorMM]));
  fXML.AddAttribute('viewBox', Format('%.2f %.2f %.2f %.2f',
    [ExtRect.Left, ExtRect.Top,
    ExtRect.Right - ExtRect.Left, ExtRect.Bottom - ExtRect.Top]));
    //fXML.AddAttribute('fill-rule', 'evenodd';
  fXML.AddAttribute('fill-rule', 'nonzero');
  fXML.AddAttribute('stroke-miterlimit',
    Format('%.5g', [fMiterLimit]));
  if fDrawing2D.FontName <> '' then
    fGlobalFaceName := fDrawing2D.FontName
  else if FontName_Default <> '' then
    fGlobalFaceName := FontName_Default
  else
    fGlobalFaceName := 'Times New Roman';
  fGlobalDescent :=
    SysBasic.GetFontDescent(fGlobalFaceName, [], 0);
  fXML.AddAttribute('style',
    'font-family: ''' + fGlobalFaceName +
    '''; font-weight:normal');
  fXML.AddAttribute('xmlns',
    'http://www.w3.org/2000/svg');
  fXML.AddComment('Created by TpX drawing tool');
  if fDrawing2D.Caption <> '' then
  begin
    fXML.OpenTag('title');
    fXML.AddText(AnsiToUtf8(fDrawing2D.Caption));
    fXML.CloseTag;
  end;
  if fDrawing2D.Comment <> '' then
  begin
    fXML.OpenTag('desc');
    fXML.AddText(AnsiToUtf8(fDrawing2D.Comment));
    fXML.CloseTag;
  end;
  begin
    fXML.OpenTag('defs');
    fPattIDs.Clear;
    RegisterPatterns;
    fXML.CloseTag;
  end;
end;

procedure TSvgDevice.WriteFooter;
begin
  fXML.CloseTag;
end;

function FF2(const X: TRealType): string;
begin
  Result := Format('%.2f', [X]);
end;

procedure TSvgDevice.WritePrimitiveAttr0(const LineColor,
  HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching;
  MiterLimit: TRealType);
begin
  if LineStyle = liNone then
    fXML.AddAttribute('stroke', 'none')
  else
  begin
    //fXML.AddAttribute('stroke-miterlimit', MiterLimit;
    if LineColor = clDefault then
      fXML.AddAttribute('stroke', 'black')
    else
      fXML.AddAttribute('stroke', ColorToHtml(LineColor));
    if LineStyle <> liNone then
      fXML.AddAttribute('stroke-width',
        Format('%.2f', [fLineWidthBase
        * LineWidth * fFactorMM]));
    case LineStyle of
      liDotted:
        fXML.AddAttribute('stroke-dasharray', Format('%.1f,%.1f',
          [fLineWidthBase * 2 * fFactorMM,
          fDottedSize * fFactorMM]));
      liDashed:
        fXML.AddAttribute('stroke-dasharray',
          Format('%.1f,%.1f',
          [fDashSize * 2 * fFactorMM,
          fDashSize * fFactorMM]));
    end;
  end;
  if Hatching <> haNone then
    fXML.AddAttribute('fill',
      'url(#' + GetPattID(Hatching, HatchColor, FillColor) + ')')
  else if FillColor <> clDefault then
    fXML.AddAttribute('fill', ColorToHtml(FillColor))
  else
    fXML.AddAttribute('fill', 'none');
end;

procedure TSvgDevice.Poly(PP: TPointsSet2D;
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
    //fXML.AddAttrValue0(St);
  end;
  procedure AddPoint(P: TPoint2D);
  begin
    P := TransformPoint2D(P, MultTransf(Transf));
    AddSt(Format('%.2f,%.2f', [P.X, P.Y]));
  end;
begin
  if Closed then
    fXML.OpenTag('polygon')
  else
    fXML.OpenTag('polyline');
  WritePrimitiveAttr0(LineColor, HatchColor, FillColor,
    LineStyle, LineWidth, Hatching, fMiterLimit);
  PathSt := '';
//  fXML.OpenAttr('points');
  PPrev := Point2D(MaxInt, MaxInt);
  for I := 0 to PP.Count - 1 do
  begin
    P1 := PP[I];
    if not IsSamePoint2D(P1, PPrev) then
    begin
      AddPoint(P1);
      if (I mod 100) = 88 then AddSt(DevEOL);
      if I < PP.Count - 1 then AddSt(' ');
    end;
    PPrev := P1;
  end;
  fXML.AddAttribute0('points', PathSt);
//  fXML.CloseAttr;
  fXML.CloseTag;
end;

procedure TSvgDevice.Bezier(PP: TPointsSet2D;
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
//    fXML.AddAttrValue0(St);
  end;
  procedure AddPoint(P: TPoint2D);
  begin
    P := TransformPoint2D(P, MultTransf(Transf));
    AddSt(Format('%.2f,%.2f', [P.X, P.Y]));
  end;
begin
  fXML.OpenTag('path');
  WritePrimitiveAttr0(LineColor, HatchColor, FillColor,
    LineStyle, LineWidth, Hatching, fMiterLimit);
  PathSt := '';
//  fXML.OpenAttr('d');
  AddSt('M ');
  AddPoint(PP[0]);
  for I := 1 to PP.Count - 1 do
  begin
    if (I mod 100) = 88 then AddSt(DevEOL);
    if I mod 3 = 1 then AddSt(' C');
    AddSt(' ');
    AddPoint(PP[I]);
  end;
  if Closed then AddSt(' Z');
  fXML.AddAttribute0('d', PathSt);
//  fXML.CloseAttr;
  fXML.CloseTag;
end;

procedure TSvgDevice.Rect(const P: TPoint2D;
  const W, H: TRealType;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching);
begin
  fXML.OpenTag('rect');
  fXML.AddAttribute('x', FF2(P.X));
  fXML.AddAttribute('y', FF2(P.Y - H));
  fXML.AddAttribute('width', FF2(W));
  fXML.AddAttribute('height', FF2(H));
    //A := Pi / 2 + 2 * Pi - TwoPointsAngle(P1, P0);
  {if ARot <> 0 then
    fXML.AddAttribute('transform',
      Format('rotate(%.2f %.2f %.2f)',
      [RadToDeg(ARot), P.X, P.Y]));}
  WritePrimitiveAttr0(LineColor, HatchColor, FillColor,
    LineStyle, LineWidth, Hatching, fMiterLimit);
  fXML.CloseTag;
end;

const

  SymbolFont_Low: array[0..94] of Word =
  // from x20 to x7E (32-126)
  ($0020, $0021, $2200, $0023, $2203, $0025, $0026, $220B, $0028,
    $0029, $2217, $002B, $002C, $2212, $002E, $002F, $0030, $0031,
    $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A,
    $003B, $003C, $003D, $003E, $003F, $2245, $0391, $0392, $03A7,
    $0394, $0395, $03A6, $0393, $0397, $0399, $03D1, $039A, $039B,
    $039C, $039D, $039F, $03A0, $0398, $03A1, $03A3, $03A4, $03A5,
    $03C2, $03A9, $039E, $03A8, $0396, $005B, $2234, $005D, $22A5,
    $005F, $F8E5, $03B1, $03B2, $03C7, $03B4, $03B5, $03C6, $03B3,
    $03B7, $03B9, $03D5, $03BA, $03BB, $03BC, $03BD, $03BF, $03C0,
    $03B8, $03C1, $03C3, $03C4, $03C5, $03D6, $03C9, $03BE, $03C8,
    $03B6, $007B, $007C, $007D, $223C);

  SymbolFont_High: array[0..94] of Word =
  // from xA0 to xFE (160-254)
  ($20AC, $03D2, $2032, $2264, $2044, $221E, $0192, $2663, $2666,
    $2665, $2660, $2194, $2190, $2191, $2192, $2193, $00B0, $00B1,
    $2033, $2265, $00D7, $221D, $2202, $2022, $00F7, $2260, $2261,
    $2248, $2026, $F8E6, $F8E7, $21B5, $2135, $2111, $211C, $2118,
    $2297, $2295, $2205, $2229, $222A, $2283, $2287, $2284, $2282,
    $2286, $2208, $2209, $2220, $2207, $F6DA, $F6D9, $F6DB, $220F,
    $221A, $22C5, $00AC, $2227, $2228, $21D4, $21D0, $21D1, $21D2,
    $21D3, $25CA, $2329, $F8E8, $F8E9, $F8EA, $2211, $F8EB, $F8EC,
    $F8ED, $F8EE, $F8EF, $F8F0, $F8F1, $F8F2, $F8F3, $F8F4, $0021,
    $232A, $222B, $2320, $F8F5, $2321, $F8F6, $F8F7, $F8F8, $F8F9,
    $F8FA, $F8FB, $F8FC, $F8FD, $F8FE);

procedure TSvgDevice.RotText(
  P: TPoint2D; H, ARot: TRealType;
  WideText: WideString; TeXText: AnsiString;
  const HJustification: THJustification;
  const VJustification: TVJustification;
  const LineColor: TColor;
  const FaceName: AnsiString;
  const Charset: TFontCharSet; const Style: TFontStyles);
var
  PNew: TPoint2D;
  D: TVector2D;
  Descent: TRealType;
  procedure WriteFont;
  var
    AddFont: string;
    I: Integer;
  begin
    //"Times", Times-Roman,'Times Roman','Times New Roman', Georgia, serif
    //Arial, Helvetica, Geneva, 'Lucida Sans Unicode', sans-serif
    //'Courier New', Courier, 'Lucida Console', monospace
    if FaceName = ' ' then Exit;
    if AnsiContainsText(FaceName, 'arial')
      or AnsiContainsText(FaceName, 'helv')
      or AnsiContainsText(FaceName, 'geneva')
      or AnsiContainsText(FaceName, 'sans')
      or AnsiContainsText(FaceName, 'tahoma')
      or AnsiContainsText(FaceName, 'verdana')
      then
      AddFont := ', sans-serif'
    else if AnsiContainsText(FaceName, 'times')
      or AnsiContainsText(FaceName, 'georgia')
      or AnsiContainsText(FaceName, 'garamond')
      or AnsiContainsText(FaceName, 'bookman')
      or AnsiContainsText(FaceName, 'palatino')
      or AnsiContainsText(FaceName, 'serif')
      then
      AddFont := ', serif'
    else if AnsiContainsText(FaceName, 'courier')
      or AnsiContainsText(FaceName, 'console')
      or AnsiContainsText(FaceName, 'tipew')
      or AnsiContainsText(FaceName, 'mono')
      then
      AddFont := ', monospace';
    if FaceName <> 'Symbol' then
      fXML.AddAttribute('font-family', FaceName + AddFont);
    if fsBold in Style then
      fXML.AddAttribute('font-weight', 'bold');
    if fsItalic in Style then
      fXML.AddAttribute('font-style', 'italic');
    if FaceName = 'Symbol' then
      fXML.AddAttribute('font-family', 'serif');
        //Times New Roman, Times,
  end;
  function SymbToUnicode(WCh: WideChar): WideChar;
  var
    I: Integer;
  begin
    I := Ord(WCh);
    if I > 61440 then I := I - 61440;
    if (I >= 32) and (I <= 126) then
      Result := WideChar(SymbolFont_Low[I - 32])
    else if (I >= 160) and (I <= 254) then
      Result := WideChar(SymbolFont_High[I - 160])
    else
      Result := '!';
  end;
  function EncodeSymbString(const WSt: WideString): WideString;
  var
    I: Integer;
  begin
    Result := '';
    for I := 1 to Length(WSt) do
      Result := Result + SymbToUnicode(WSt[I]);
  end;
begin
  fXML.OpenTag('text');
  fXML.PreserveSpace := True;
  if FaceName = ' ' then
    Descent := fGlobalDescent
  else
    Descent := GetFontDescent(FaceName, Style, Charset);
  D.X := 0;
  case VJustification of
    jvBottom: D.Y := Descent;
    jvCenter: D.Y := Descent - 0.5;
    jvTop: D.Y := Descent - 1;
    jvBaseline: D.Y := 0;
  end;
  if ARot <> 0 then D := TransformVector2D(D, Rotate2D(ARot));
  D := TransformVector2D(D, Scale2D(H, -H));
  PNew := ShiftPoint(P, D); //ConvertPnt(
  fXML.AddAttribute('x', FF2(PNew.X));
  fXML.AddAttribute('y', FF2(PNew.Y));
  fXML.AddAttribute('font-size', FF2(H));
  if ARot <> 0 then
    fXML.AddAttribute('transform',
      Format('rotate(%.2f %.2f %.2f)',
      [RadToDeg(-ARot), PNew.X, PNew.Y]));
  fXML.AddAttribute('xml:space', 'preserve');
  case HJustification of
    jhLeft: fXML.AddAttribute('text-anchor', 'start');
    jhCenter: fXML.AddAttribute('text-anchor', 'middle');
    jhRight: fXML.AddAttribute('text-anchor', 'end');
  end;
  if LineColor <> clDefault then
    fXML.AddAttribute('fill', ColorToHtml(LineColor))
  else
    fXML.AddAttribute('fill', 'black');
  WriteFont;
    //St := AnsiToUtf8(St);
  if FaceName = 'Symbol' then
    fXML.AddText(Utf8Encode(EncodeSymbString(WideText)))
  else fXML.AddText(Utf8Encode(WideText));
  fXML.CloseTag;
  fXML.PreserveSpace := False;
end;

procedure TSvgDevice.RotEllipse(const CP: TPoint2D;
  const RX, RY, ARot: TRealType;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching);
begin
  fXML.OpenTag('ellipse');
  fXML.AddAttribute('cx', FF2(CP.X));
  fXML.AddAttribute('cy', FF2(CP.Y));
  fXML.AddAttribute('rx', FF2(RX));
  fXML.AddAttribute('ry', FF2(RY));
  if ARot <> 0 then
    fXML.AddAttribute('transform',
      Format('rotate(%.2f %.2f %.2f)',
      [RadToDeg(-ARot), CP.X, CP.Y]));
  WritePrimitiveAttr0(LineColor, HatchColor, FillColor,
    LineStyle, LineWidth, Hatching, fMiterLimit);
  fXML.CloseTag;
end;

procedure TSvgDevice.Circle(const CP: TPoint2D; const R: TRealType;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching);
begin
  fXML.OpenTag('circle');
  fXML.AddAttribute('cx', FF2(CP.X));
  fXML.AddAttribute('cy', FF2(CP.Y));
  fXML.AddAttribute('r', FF2(R));
  WritePrimitiveAttr0(LineColor, HatchColor, FillColor,
    LineStyle, LineWidth, Hatching, fMiterLimit);
  fXML.CloseTag;
end;

procedure TSvgDevice.Circular(const CP: TPoint2D; R, SA, EA:
  TRealType;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Kind: TCircularKind);
var
  PathSt: string;
  P1: TPoint2D;
  procedure AddSt(const St: string);
  begin
    PathSt := PathSt + St;
  end;
  procedure AddPoint(P: TPoint2D);
  begin
    AddSt(Format('%.2f,%.2f', [P.X, P.Y]));
  end;
begin
  SA := SA - Floor(SA / (2 * Pi)) * 2 * Pi;
  EA := EA - Floor(EA / (2 * Pi)) * 2 * Pi;
  if EA < SA then EA := EA + 2 * Pi;
  fXML.OpenTag('path');
  PathSt := 'M ';
  P1 := Point2D(CP.X + R * Cos(SA), CP.Y - R * Sin(SA));
  AddPoint(P1);
  AddSt(' A ' + FF2(R)
    + ' ' + FF2(R)
    + ' 0 ' + IntToStr(Integer(EA - SA > Pi)) + ' 0 ');
  AddPoint(Point2D(CP.X + R * Cos(EA), CP.Y - R * Sin(EA)));
  //if Obj is TArc2D then
  if Kind = ci_Sector then
  begin
    AddSt(' L ');
    AddPoint(CP);
    AddSt(' L ');
    AddPoint(P1);
    AddSt(' Z');
  end
  else if Kind = ci_Segment then
  begin
    AddSt(' L ');
    AddPoint(P1);
    AddSt(' Z');
  end;
  fXML.AddAttribute('d', PathSt);
  WritePrimitiveAttr0(LineColor, HatchColor, FillColor,
    LineStyle, LineWidth, Hatching, fMiterLimit);
  fXML.CloseTag;
end;

end.


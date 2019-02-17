unit DevPS;

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface

uses Classes, SysUtils, StrUtils, Geometry, Drawings, Math,
  Pieces, GObjects, Graphics, Devices;

type

// A class for PostScript export

  TPostScriptDevice = class(TStreamDevice)
  protected
    fDrawing2D: TDrawing2D;
    FontName: string;
    procedure WriteFont; virtual;
    procedure WriteStreamPoint0(const X, Y: TRealType); override;
    procedure WriteColor(Color: TColor);
    procedure WriteLineAttr(const LineStyle: TLineStyle;
      const LineWidth: TRealType; const LineColor: TColor);
    procedure WriteFill(const FillColor: TColor;
      const LineStyle: TLineStyle);
    procedure WriteStroke(const LineStyle: TLineStyle;
      const LineWidth: TRealType; const LineColor: TColor);
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
    procedure GenPath(const GP: TGenericPath;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching; const Transf: TTransf2D);
  public
    Light: Boolean;
    ForPdf: Boolean;
    TextLabels: TStringList;
    FontSizeInTeX: Boolean;
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

uses ColorEtc, SysBasic, Output, Bitmaps;

procedure pfb2pfa_Stream(Stream_pfb, Stream_pfa: TStream;
  var FontName: string);
//	Program converts a binary MSDOS representation for a type1
//	PostScript font into a readable ASCII version. The MSDOS
//	newline (\r) is converted into the UNIX newline (\n).
//	The output is written in a file whose name is the name that
//	is provided on the command line or the basename of the input
//	file plus extension ".pfa".
var
  L, I, J: Integer;
  T: Byte;
  St: string;
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
            while St[J] <> '/' do
              Inc(J);
            Inc(J);
            I := J;
            while not (St[J] in [#10, ' ', '/']) do
              Inc(J);
            FontName := Trim(Copy(St, I, J - I));
          end;
          for I := 1 to L do
          begin
            if St[I] = #10 then
              WriteStream(NewLine)
            else
              WriteStream(St[I]);
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

procedure pfb2pfa(const FileName_pfb, FileName_pfa: string);
var
  Stream_pfb, Stream_pfa: TFileStream;
  FontName: string;
begin
  Stream_pfb := TFileStream.Create(FileName_pfb, fmOpenRead);
  Stream_pfa := TFileStream.Create(FileName_pfa, fmCreate);
  try
    pfb2pfa_Stream(Stream_pfb, Stream_pfa, FontName);
  finally
    Stream_pfb.Free;
    Stream_pfa.Free;
  end;
end;

// =====================================================================
// TPostScriptDevice
// =====================================================================

constructor TPostScriptDevice.Create(Drawing: TDrawing2D);
begin
  inherited Create;
  fDrawing2D := Drawing;
  OnBezier := Bezier;
  OnCircle := Circle;
  OnCircular := Circular;
  OnRotText := RotText;
  OnBitmap := Bitmap;
  OnGenPath := GenPath;
  fHasBezier := True;
  fHasClosedBezier := True;
  fHasArc := True;
  fHasSector := True;
  fHasSegment := True;
  Light := False;
  ForPdf := False;
  TextLabels := TStringList.Create;
end;

destructor TPostScriptDevice.Destroy;
begin
  TextLabels.Free;
  inherited Destroy;
end;

procedure TPostScriptDevice.WriteFont;
var
  Stream_pfb: TFileStream;
begin
  if Font_pfb_Path = '' then Exit;
  if not FileExists(Font_pfb_Path) then
  begin
    MessageBoxError(Format('Can not find font file %s',
      [Font_pfb_Path]));
    Exit;
  end;
  Stream_pfb :=
    TFileStream.Create(Font_pfb_Path, fmOpenRead);
  try
    WriteLnStream('%%BeginProlog');
    pfb2pfa_Stream(Stream_pfb, fStream, FontName);
    WriteLnStream('%%Endprolog');
  finally
    Stream_pfb.Free;
  end;
end;

procedure TPostScriptDevice.WriteStreamPoint0(const X, Y:
  TRealType);
begin
  WriteStream(Format('%.2f %.2f ', [X, Y]));
end;

procedure TPostScriptDevice.WriteColor(Color: TColor);
var
  RGB: T_PS_RGB;
begin
  if Color = clDefault then
    RGB := PS_RGB(0)
  else
    RGB := PS_RGB(Color);
  WriteStream(
    Format('%.5g %.5g %.5g setrgbcolor ', [RGB.R, RGB.G, RGB.B]));
end;

procedure TPostScriptDevice.WriteLineAttr(const LineStyle:
  TLineStyle;
  const LineWidth: TRealType; const LineColor: TColor);
begin
  with fDrawing2D do
  begin
    case LineStyle of
      liNone: ; //WriteStream('0 setlinewidth [] 0 setdash ');
      liSolid: WriteStream(
          Format('%.2f setlinewidth [] 0 setdash ',
          [LineWidthBase * LineWidth * fFactorMM]));
      liDashed: WriteStream(
          Format('%.2f setlinewidth [%.2f %.2f] 0 setdash ',
          [LineWidthBase * LineWidth * fFactorMM,
          DashSize * 2 * fFactorMM, DashSize * fFactorMM]));
      liDotted: WriteStream(
          Format('%.2f setlinewidth [%.2f %.2f] 0 setdash ',
          [LineWidthBase * LineWidth * fFactorMM,
          LineWidthBase * LineWidth * fFactorMM, DottedSize *
            fFactorMM]));
    end;
  end;
  WriteColor(LineColor);
  if fDrawing2D.MiterLimit <> 10 then
    WriteStream(
      Format('%.2g setmiterlimit ', [fDrawing2D.MiterLimit]));
  //if Obj.LineColor <> clDefault then  begin    RGB := PS_RGB(Obj.LineColor);  end;
end;

procedure TPostScriptDevice.WriteFill(const FillColor: TColor;
  const LineStyle: TLineStyle);
begin
  if LineStyle <> liNone then WriteStream('gsave ');
  if FillColor = clDefault then Exit;
  WriteColor(FillColor);
  //WriteStream('eofill '); // even-odd aka alternate fill rule
  WriteStream('fill '); // nonzero winding fill rule
end;

procedure TPostScriptDevice.WriteStroke(const LineStyle:
  TLineStyle;
  const LineWidth: TRealType; const LineColor: TColor);
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

procedure TPostScriptDevice.HatchingLine(P0, P1: TPoint2D;
  const LineColor: TColor; const LineStyle: TLineStyle;
  const LineWidth: TRealType);
begin
  if IsSamePoint2D(P0, P1) then Exit;
  WriteStreamPoint(P0);
  WriteStream('moveto ');
  WriteStreamPoint(P1);
  WriteStream('lineto ');
end;

procedure TPostScriptDevice.WriteHatchingLines(const Lines:
  TPointsSet2D;
  const HatchColor: TColor; const LineWidth: TRealType);
var
  I: Integer;
begin
  WriteStream('newpath ');
  WriteLnStream(Format('%.2f setlinewidth [] 0 setdash',
    [fLineWidthBase * LineWidth * fFactorMM]));
  WriteStream(' ');
  Lines.TransformPoints(T);
  for I := 0 to Lines.Count div 2 - 1 do
  begin
    HatchingLine(Lines[I * 2], Lines[I * 2 + 1],
      HatchColor, liSolid, 0);
    if Succ(I) mod 5 = 0 then
    begin
      WriteLnStream('');
      WriteStream(' ');
    end;
  end;
  if HatchColor = clDefault then WriteColor(clBlack)
  else WriteColor(HatchColor);
  WriteLnStream('stroke');
end;

procedure TPostScriptDevice.WriteHeader(ExtRect: TRect2D);
begin
  ExtRect := TransformRect2D(ExtRect, fT);
    // Transform drawing rectangle
  WriteLnStream('%!PS-Adobe-3.0 EPSF-3.0');
  WriteLnStream('%%' + Format('BoundingBox: %d %d %d %d',
    [0, 0, Ceil(ExtRect.Right - ExtRect.Left),
    Ceil(ExtRect.Top - ExtRect.Bottom)]));
  WriteLnStream('%%' + Format('HiResBoundingBox: %d %d %.2f %.2f',
    [0, 0, ExtRect.Right - ExtRect.Left,
    ExtRect.Top - ExtRect.Bottom])); // * fFactorMM
  WriteLnStream('%%Title: ' + fDrawing2D.Caption);
  WriteLnStream('%%Creator: TpX drawing tool');
  WriteLnStream('%%CreationDate: ' + DateTimeToStr(Now));
  WriteLnStream('%%EndComments');
  if ForPdf then
    // set page size for conversion to pdf using ghostscript
    WriteLnStream(Format(
      '<< /PageSize [%.2f %.2f] >> setpagedevice',
      [ExtRect.Right - ExtRect.Left,
      ExtRect.Top - ExtRect.Bottom]));
  FontName := 'Times-Roman';
  TextLabels.Clear;
  if not Light then WriteFont;
end;

procedure TPostScriptDevice.WriteFooter;
begin
  WriteLnStream('showpage');
  WriteLnStream('%%EOF');
end;

function FF2(const X: TRealType): string;
begin
  Result := Format('%.2f', [X]);
end;

procedure TPostScriptDevice.Poly(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D;
  const Closed: Boolean);
var
  I: Integer;
begin
  if PP.Count < 1 then Exit;
  if (LineStyle = liNone) and (FillColor = clDefault) then Exit;
  WriteStream('newpath ');
  WriteStreamPointT(PP[0], Transf);
  WriteStream('moveto ');
  for I := 1 to PP.Count - 1 do
  begin
    WriteStreamPointT(PP[I], Transf);
    WriteStream('lineto ');
    if (I mod 10) = 0 then WriteLnStream('');
  end;
  if Closed then WriteStream('closepath ');
  WriteFill(FillColor, LineStyle);
  WriteStroke(LineStyle, LineWidth, LineColor);
end;

procedure TPostScriptDevice.Bezier(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D;
  const Closed: Boolean);
var
  I: Integer;
begin
  if (LineStyle = liNone) and (FillColor = clDefault) then Exit;
  WriteStream('newpath ');
  WriteStreamPointT(PP[0], Transf);
  WriteStream('moveto ');
  for I := 1 to PP.Count - 1 do
  begin
    WriteStreamPointT(PP[I], Transf);
    if I mod 3 = 0 then
    begin
      WriteLnStream('curveto');
    end;
  end;
  if Closed then WriteStream('closepath ');
  WriteFill(FillColor, LineStyle);
  WriteStroke(LineStyle, LineWidth, LineColor);
end;

procedure TPostScriptDevice.Circle(const CP: TPoint2D; const R:
  TRealType;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching);
begin
  if (LineStyle <> liNone) or (FillColor <> clDefault) then
  begin
    WriteStream('newpath ');
    WriteStreamPoint(CP);
    WriteLnStream(Format('%.2f 0 360 arc ', [R]));
    WriteStream('closepath ');
    WriteFill(FillColor, LineStyle);
  end;
  WriteStroke(LineStyle, LineWidth, LineColor);
end;

procedure TPostScriptDevice.Circular(const CP: TPoint2D; R, SA, EA:
  TRealType;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Kind: TCircularKind);
begin
  if EA < SA then EA := EA + 2 * Pi;
  if (LineStyle <> liNone) or (FillColor <> clDefault) then
  begin
    WriteStream('newpath ');
    if Kind = ci_Sector then
    begin
      WriteStreamPoint(CP);
      WriteStream('moveto ');
    end;
    WriteStreamPoint(CP);
    WriteStream(Format('%.2f %.2f %.2f arc ',
      [R, RadToDeg(SA), RadToDeg(EA)]));
    if Kind <> ci_Arc then WriteStream('closepath ');
    WriteFill(FillColor, LineStyle);
  end;
  WriteStroke(LineStyle, LineWidth, LineColor);
end;

procedure TPostScriptDevice.RotText(
  P: TPoint2D; H, ARot: TRealType;
  WideText: WideString; TeXText: AnsiString;
  const HAlignment: THAlignment;
  const VAlignment: TVAlignment;
  const LineColor: TColor;
  const FaceName: AnsiString;
  const Charset: TFontCharSet; const Style: TFontStyles);
var
  D: TVector2D;
  St: string;
begin
  if Light then
  begin
    ShiftTeXTextPoint(P, H, ARot);
    if fFactorMM = 0 then fFactorMM := 1;
    TextLabels.Add(Format('\put(%.2f,%.2f){%s}', [P.X, P.Y,
      GetTeXTextMakebox0(
        H, ARot, 1 / fFactorMM, LineColor, HAlignment,
        Style, WideText, TeXText, FontSizeInTeX)]));
    Exit;
  end;
  D.Y := 0;
  D.X := 0;
  if ARot <> 0 then D := TransformVector2D(D, Rotate2D(ARot));
  D := TransformVector2D(D, Scale2D(H, H));
  P := ShiftPoint(P, D);
  St := AnsiReplaceText(WideText, '\', '\\');
  St := AnsiReplaceText(St, '(', '\(');
  St := AnsiReplaceText(St, ')', '\)');
  WriteStream(Format('/%s findfont %d scalefont setfont newpath ',
    [FontName, Round(H)]));
  if LineColor <> clDefault then
    WriteColor(LineColor)
  else
    WriteColor(clBlack);
  WriteStreamPoint(P);
  WriteLnStream('moveto ');
  case HAlignment of
    ahLeft: D.X := 0;
    ahCenter: D.X := 0.5;
    ahRight: D.X := 1;
  end;
  // "string" stringwidth -> "wx" "wy"
  //  stringwidth returns the length of the string ( ... ) and (usually) the value 0.0
  if D.X <> 0 then
    if ARot = 0 then
      WriteStream(Format('(%s) stringwidth pop %.5g mul 0 rmoveto',
        [St, -D.X * Cos(ARot)]))
    else
      WriteStream(Format('(%s) stringwidth pop %.5g mul (%s) stringwidth pop %.5g mul rmoveto',
        [St, -D.X * Cos(ARot), St, -D.X * Sin(ARot)]));
  if ARot <> 0 then
    WriteStream(Format(' %.5g rotate', [RadToDeg(ARot)]));
  if (D.X <> 0) or (ARot <> 0) then WriteLnStream('');
  WriteLnStream(Format('(%s) show stroke', [St]));
  if ARot <> 0 then
    WriteStream('initmatrix ');
end;

procedure TPostScriptDevice.Bitmap(P: TPoint2D; W, H: TRealType;
  const KeepAspectRatio: Boolean; BitmapEntry: TObject);
var
  St: string;
  I: Integer;
  BE: TBitmapEntry;
  FEps: TextFile;
  Start: Boolean;
  SX, SY: TRealType;
begin
  BE := BitmapEntry as TBitmapEntry;
  if not BE.RequireEPS(GetTempDir) then Exit;
  AssignFile(FEps, GetTempDir + BE.GetOnlyName + '.eps');
  Reset(FEps);
  Start := True;
  SX := W / BE.Bitmap.Width;
  SY := H / BE.Bitmap.Height;
  try
    while not Eof(FEps) do
    begin
      ReadLn(FEps, St);
      // Seeking for the start of bitmap data
      if Start then
      begin
        if Pos('%', St) = 1 then Continue;
        Start := False;
        // The first line without % found
        WriteLnStream('save');
        WriteLnStream(Format('%.2f %.2f translate',
          [P.X, P.Y]));
        WriteLnStream(Format('%.2f %.2f scale',
          [SX, SY]));
        WriteLnStream(St);
        Continue;
      end;
      // Delete unnecessary stuff
      if Pos('%', St) <> 1 then
      begin
        // showpage command must be removed from PS
        I := Pos('showpage', St);
        if I > 0 then Delete(St, I, 8);
      end
      else
        if (Pos('%%EOF', St) = 1) or (Pos('%%Trailer', St) = 1)
          then St := '%';
      WriteLnStream(St);
    end;
    WriteLnStream('restore');
  finally
    CloseFile(FEps);
  end;
end;

procedure TPostScriptDevice.GenPath(const GP: TGenericPath;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D);
var
  Kind: TPathItemKind;
  P1, P2, P3: TPoint2D;
begin
  if (LineStyle = liNone) and (FillColor = clDefault) then Exit;
  WriteStream('newpath ');
  GP.StartIterations;
  while GP.GetNext(Kind, P1, P2, P3) do
    case Kind of
      pik_MoveTo:
        begin
          WriteStreamPointT(P1, Transf);
          WriteStream('moveto ');
        end;
      pik_LineTo:
        begin
          WriteStreamPointT(P1, Transf);
          WriteStream('lineto ');
        end;
      pik_BezierTo:
        begin
          WriteStreamPointT(P1, Transf);
          WriteStreamPointT(P2, Transf);
          WriteStreamPointT(P3, Transf);
          WriteLnStream('curveto ');
        end;
      pik_Close: WriteStream('closepath ');
    end;
  WriteFill(FillColor, LineStyle);
  WriteStroke(LineStyle, LineWidth, LineColor);
end;

end.


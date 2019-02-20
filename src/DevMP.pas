unit DevMP;

interface

uses SysUtils, Classes, Geometry, Graphics, Devices, Drawings,
  Forms;

type

// A class for metapost output

  T_MetaPost_Device = class(TFileDevice)
  protected
    fDrawing2D: TDrawing2D;
    fExtRect: TRect2D;
    procedure WriteStreamPoint0(const X, Y: TRealType); override;
    procedure WriteColor(const Color, DefaultColor: TColor);
    procedure WriteLineAttr(const LineStyle: TLineStyle;
      const LineWidth: TRealType; const LineColor: TColor);
    procedure WriteFill(const FillColor: TColor;
      const LineStyle: TLineStyle);
    procedure WriteStroke(const LineStyle: TLineStyle;
      const LineWidth: TRealType; const LineColor: TColor;
      const Closed: Boolean);
    procedure Poly(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching; const Transf: TTransf2D;
      const Closed: Boolean); override;
    procedure Bezier(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching; const Transf: TTransf2D;
      const Closed: Boolean);
    procedure Circle(const CP: TPoint2D; const R: TRealType;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching);
    procedure RotText(P: TPoint2D; H, ARot: TRealType;
      WideText: WideString; TeXText: AnsiString;
      const HAlignment: THAlignment;
      const VAlignment: TVAlignment;
      const LineColor: TColor;
      const FaceName: AnsiString;
      const Charset: TFontCharSet; const Style: TFontStyles);
    procedure GenPath(const GP: TGenericPath;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching; const Transf: TTransf2D);
    procedure WriteHatchingLines(const Lines: TPointsSet2D;
      const HatchColor: TColor; const LineWidth: TRealType);
      override;
  public
    FontSizeInTeX: Boolean;
    constructor Create(Drawing: TDrawing2D);
    destructor Destroy; override;
    procedure WriteHeader(ExtRect: TRect2D); override;
    procedure WriteFooter; override;
  end;

implementation

uses ColorEtc, Output, StrUtils;

// =====================================================================
// T_MetaPost_Device
// =====================================================================

constructor T_MetaPost_Device.Create;
begin
  inherited Create;
  fDrawing2D := Drawing;
  fDisjointFill := True;
  OnBezier := Bezier;
  OnCircle := Circle;
  OnRotText := RotText;
  OnGenPath := GenPath;
  fHasBezier := True;
  fHasClosedBezier := True;
  fHasNativeHatching := False;
end;

destructor T_MetaPost_Device.Destroy;
begin
  inherited Destroy;
end;

procedure T_MetaPost_Device.WriteStreamPoint0(const X, Y:
  TRealType);
begin
  WriteStream(Format('(%.2fu,%.2fu)', [X, Y]));
end;

procedure T_MetaPost_Device.WriteColor(const Color, DefaultColor:
  TColor);
var
  RGB: T_PS_RGB;
begin
  if Color = clDefault then
    RGB := PS_RGB(DefaultColor)
  else
    RGB := PS_RGB(Color);
  WriteStream(Format(' withcolor (%.3f,%.3f,%.3f)',
    [RGB.R, RGB.G, RGB.B]));
end;

procedure T_MetaPost_Device.WriteLineAttr(
  const LineStyle: TLineStyle;
  const LineWidth: TRealType; const LineColor: TColor);
begin
  if LineStyle <> liNone then
    WriteStream(Format(' withpen pencircle scaled %.2fmm',
      [fLineWidthBase * LineWidth]));
  case LineStyle of
    liDotted:
      WriteStream(Format(' dashed dashpattern(on %.2fmm off %.2fmm)',
        [fLineWidthBase * LineWidth, fDottedSize]));
    liDashed:
      WriteStream(Format(' dashed dashpattern(on %.2fmm off %.2fmm)',
        [fDashSize * 2, fDashSize]));
  end;
  WriteColor(LineColor, clBlack);
end;

procedure T_MetaPost_Device.WriteFill(const FillColor: TColor;
  const LineStyle: TLineStyle);
begin
  if FillColor = clDefault then Exit;
  WriteStream('fill pp');
  WriteStream('--cycle');
  WriteColor(FillColor, clBlack);
  WriteLnStream(';');
end;

procedure T_MetaPost_Device.WriteStroke(const LineStyle:
  TLineStyle;
  const LineWidth: TRealType; const LineColor: TColor;
  const Closed: Boolean);
begin
  if LineStyle = liNone then Exit;
  WriteStream('draw pp');
  if Closed then WriteStream('--cycle');
  WriteLineAttr(LineStyle, LineWidth, LineColor);
  WriteLnStream(';');
end;

function GetTexInclude: string;
var
  IncludeFile: string;
  List: TStringList;
begin
  Result := '';
  IncludeFile := ExtractFilePath(Application.ExeName) +
    'metapost.tex.inc';
  List := TStringList.Create;
  try
    if FileExists(IncludeFile) then
      List.LoadFromFile(IncludeFile)
    else
    begin
      List.Add('\documentclass[a4paper,10pt]{article}');
    //\usepackage[english,russian]{babel} \usepackage[cp1251]{inputenc}
        //List.Add('\usepackage{color}');
      List.SaveToFile(IncludeFile);
    end;
    Result := List.Text;
  finally
    List.Free;
  end;
end;

procedure T_MetaPost_Device.WriteHeader(ExtRect: TRect2D);
  function ClearedComment(const St: string): string;
  begin
    Result := AnsiReplaceStr(St, EOL, #10);
    Result := AnsiReplaceStr(Result, #13, #10);
    Result := AnsiReplaceStr(Result, #10, EOL + '%');
  end;
begin
  fExtRect := ExtRect;
  WriteLnStream('%Exported from TpX drawing');
  if fDrawing2D.Caption <> '' then
    WriteLnStream('%Caption: ' +
      ClearedComment(fDrawing2D.Caption));
  if fDrawing2D.Comment <> '' then
    WriteLnStream('%Comment: ' +
      ClearedComment(fDrawing2D.Comment));
  WriteLnStream('%CreationDate: ' + DateTimeToStr(Now));
  WriteLnStream('beginfig(0);');
  if fDrawing2D.MetaPostTeXText then
  begin
    WriteLnStream('verbatimtex %&latex');
    WriteStream(GetTexInclude);
    WriteLnStream(' \begin{document} etex');
  end;
  if fFactorMM <> 0 then
    WriteStream(Format('u=%.3fmm; ', [1 / fFactorMM]));
  WriteStream('linecap:=butt; ');
  WriteStream('linejoin:=mitered; ');
  if fDrawing2D.MiterLimit <> 10 then
    WriteStream(Format('miterlimit:=%.2g; ',
      [fDrawing2D.MiterLimit]));
  WriteStream('path pp; ');
  WriteStream('picture pic; ');
  //WriteStream('bboxmargin := 0;');
  WriteLnStream('labeloffset:=0;');
end;

procedure T_MetaPost_Device.WriteFooter;
begin
  WriteLnStream(Format(
    'setbounds currentpicture to (%.2fu,%.2fu)--(%.2fu,%.2fu)--(%.2fu,%.2fu)--(%.2fu,%.2fu)--cycle;',
    [fExtRect.Left, fExtRect.Bottom, fExtRect.Right,
    fExtRect.Bottom,
      fExtRect.Right, fExtRect.Top, fExtRect.Left, fExtRect.Top]));
  WriteLnStream('endfig;');
  WriteLnStream('end');
end;

procedure T_MetaPost_Device.WriteHatchingLines(const Lines:
  TPointsSet2D;
  const HatchColor: TColor; const LineWidth: TRealType);
var
  RGB: T_PS_RGB;
  I: Integer;
begin
  Lines.TransformPoints(T);
  for I := 0 to Lines.Count div 2 - 1 do
  begin
    WriteStream('draw ');
    WriteStreamPoint(Lines[I * 2]);
    WriteStream('--');
    WriteStreamPoint(Lines[I * 2 + 1]);
    WriteStream(Format(' withpen pencircle scaled %.2fmm',
      [fLineWidthBase * LineWidth]));
    if HatchColor <> clDefault then
    begin
      RGB := PS_RGB(HatchColor);
      WriteStream(Format(' withcolor (%.3f, %.3f, %.3f)',
        [RGB.R, RGB.G, RGB.B]));
    end;
    WriteStream('; ');
  end;
  WriteLnStream('');
end;

procedure T_MetaPost_Device.Poly(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D;
  const Closed: Boolean);
var
  I: Integer;
begin
  if PP.Count < 2 then Exit;
  WriteStream('pp:=');
  WriteStreamPoint(PP[0]);
  for I := 1 to PP.Count - 1 do
  begin
    if (I mod 10) = 0 then WriteLnStream('  ');
    WriteStream('--');
    WriteStreamPoint(PP[I]);
  end;
  WriteLnStream(';');
  WriteFill(FillColor, LineStyle);
  WriteStroke(LineStyle, LineWidth, LineColor, Closed);
end;

procedure T_MetaPost_Device.Bezier(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D;
  const Closed: Boolean);
var
  I: Integer;
begin
  if PP.Count < 4 then Exit;
  WriteStream('pp:=');
  WriteStreamPoint(PP[0]);
  for I := 0 to PP.Count div 3 - 1 do
  begin
    WriteStream('..controls');
    WriteStreamPoint(PP[3 * I + 1]);
    WriteStream('and');
    WriteStreamPoint(PP[3 * I + 2]);
    WriteStream('..');
    WriteStreamPoint(PP[3 * I + 3]);
  end;
  WriteLnStream(';');
  WriteFill(FillColor, LineStyle);
  WriteStroke(LineStyle, LineWidth, LineColor, Closed);
end;

procedure T_MetaPost_Device.Circle(const CP: TPoint2D; const R:
  TRealType;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching);
begin
  WriteStream(Format('pp:=fullcircle scaled %.2fu shifted ',
    [R * fTScale * 2]));
  WriteStreamPoint(CP);
  WriteLnStream(';');
  WriteFill(FillColor, LineStyle);
  WriteStroke(LineStyle, LineWidth, LineColor, False);
end;

procedure T_MetaPost_Device.RotText(P: TPoint2D;
  H, ARot: TRealType;
  WideText: WideString; TeXText: AnsiString;
  const HAlignment: THAlignment;
  const VAlignment: TVAlignment;
  const LineColor: TColor;
  const FaceName: AnsiString;
  const Charset: TFontCharSet; const Style: TFontStyles);
var
  St, StRot: string;
  I: Integer;
begin
  if not fDrawing2D.MetaPostTeXText and FontSizeInTeX then
    WriteLnStream(Format(
      'defaultscale:=%.2fu/fontsize defaultfont;', [H]));
  St := Get_TeXText(clDefault, Style, WideText, TeXText);
    //LineColor
  WriteStream(Format('textX:=%.2fu; ', [P.X]));
  WriteStream(Format('textY:=%.2fu; ', [P.Y]));
  WriteStream('pic:=thelabel');
  case HAlignment of
    ahLeft:
      WriteStream('.urt');
    ahCenter:
      WriteStream('.top');
    ahRight:
      WriteStream('.ulft');
  end;
  if fDrawing2D.MetaPostTeXText then
  begin
    if FontSizeInTeX then
      WriteStream(Format(//\strut
        '(btex \raisebox{0pt}[0pt][0pt]{\fontsize{10}{12}\selectfont %s} etex scaled %.2f,(0,0)); ',
        [St, H * 2.84527559 / 10 / fFactorMM])) // mm=2.84527559pt
        // Use scaled 10pt font
    else
      WriteStream(Format(//\strut
        '(btex \raisebox{0pt}[0pt][0pt]{%s} etex,(0,0)); ',
        [St]));
  end
  else
    WriteStream(Format('("%s",(0,0)); ', [WideText]));
  if ARot <> 0 then
    StRot := Format(' rotatedaround((textX, textY),%.2f)',
      [RadToDeg(ARot)])
  else
    StRot := '';
  WriteStream(Format('draw pic shifted (textX, textY)%s',
    [StRot]));
  if LineColor <> clDefault
    then WriteColor(LineColor, clBlack);
  WriteLnStream(';');
end;

procedure T_MetaPost_Device.GenPath(const GP: TGenericPath;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D);
var
  Kind: TPathItemKind;
  P1, P2, P3: TPoint2D;
  Start: Boolean;
begin
  WriteStream('pp:=');
  Start := True;
  GP.StartIterations;
  while GP.GetNext(Kind, P1, P2, P3) do
    case Kind of
      pik_MoveTo:
        begin
          if Start then Start := False else WriteStream('..');
          WriteStreamPoint(P1);
          //WriteStreamPointT(P1, Transf);
        end;
      pik_LineTo:
        begin
          WriteStream('--');
          WriteStreamPoint(P1);
        end;
      pik_BezierTo:
        begin
          WriteStream('..controls');
          WriteStreamPoint(P1);
          WriteStream('and');
          WriteStreamPoint(P2);
          WriteStream('..');
          WriteStreamPoint(P3);
        end;
      pik_Close: WriteStream('--cycle');
    end;
  WriteLnStream(';');
  WriteFill(FillColor, LineStyle);
  WriteStroke(LineStyle, LineWidth, LineColor, False);
end;

end.


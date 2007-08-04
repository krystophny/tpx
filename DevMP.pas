unit DevMP;

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface

uses SysUtils, Classes, Geometry, Graphics, Devices, Drawings,
Forms;

type

// A class for metapost output

  T_MetaPost_Device = class(TStreamDevice)
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
      const HJustification: THJustification;
      const VJustification: TVJustification;
      const LineColor: TColor;
      const FaceName: AnsiString;
      const Charset: TFontCharSet; const Style: TFontStyles);
    procedure WriteHatchingLines(const Lines: TPointsSet2D;
      const HatchColor: TColor; const LineWidth: TRealType);
      override;
  public
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

procedure T_MetaPost_Device.WriteLineAttr(const LineStyle:
  TLineStyle;
  const LineWidth: TRealType; const LineColor: TColor);
begin
  if LineStyle <> liNone then
    WriteStream(Format(' withpen pencircle scaled %.2fmm',
      [fLineWidthBase * LineWidth]));
  case LineStyle of
    liDotted:
      WriteStream(Format(' dashed dashpattern(on %.2fmm off %.2fmm)',
        [fLineWidthBase * 2, fDottedSize]));
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

const
  CMR_HT: array[0..255] of Real =
  (0.683332, 0.683332, 0.683332, 0.683332, 0.683332, 0.683332,
    0.683332,
    0.683332, 0.683332, 0.683332, 0.683332, 0.694445, 0.694445,
      0.694445,
    0.694445, 0.694445, 0.430555, 0.430555, 0.694445, 0.694445,
      0.628473,
    0.694445, 0.567777, 0.694445, 0, 0.694445, 0.430555, 0.430555,
    0.527779, 0.683332, 0.683332, 0.731944, 0.430555, 0.694445,
      0.694445,
    0.694445, 0.75, 0.75, 0.694445, 0.694445, 0.75, 0.75, 0.75,
      0.583334,
    0.105556, 0.430555, 0.105556, 0.75, 0.644444, 0.644444,
      0.644444,
    0.644444, 0.644444, 0.644444, 0.644444, 0.644444, 0.644444,
      0.644444,
    0.430555, 0.430555, 0.5, 0.366875, 0.5, 0.694445, 0.694445,
      0.683332,
    0.683332, 0.683332, 0.683332, 0.683332, 0.683332, 0.683332,
      0.683332,
    0.683332, 0.683332, 0.683332, 0.683332, 0.683332, 0.683332,
      0.683332,
    0.683332, 0.683332, 0.683332, 0.683332, 0.683332, 0.683332,
      0.683332,
    0.683332, 0.683332, 0.683332, 0.683332, 0.75, 0.694445, 0.75,
    0.694445, 0.667859, 0.694445, 0.430555, 0.694445, 0.430555,
      0.694445,
    0.430555, 0.694445, 0.430555, 0.694445, 0.667859, 0.667859,
      0.694445,
    0.694445, 0.430555, 0.430555, 0.430555, 0.430555, 0.430555,
      0.430555,
    0.430555, 0.61508, 0.430555, 0.430555, 0.430555, 0.430555,
      0.430555,
    0.430555, 0.430555, 0.430555, 0.694445, 0.667859, 0.667859, 0,
      0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

  CMR_DP: array[0..255] of Real =
  (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.194445, 0,
    0, 0,
    0, 0, 0, 0.170138, 0, 0, 0, 0.097223, 0, 0, 0.048612, 0, 0, 0,
    0.194443, 0.055555, 0.055555, 0, 0, 0.25, 0.25, 0, 0.083334,
      0.194445,
    0, 0, 0.25, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.194445, 0.194445,
      -
    0.133125, 0.194445, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0,
    0, 0.194445, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.25, 0, 0.25, 0, 0, 0,
      0, 0,
    0, 0, 0, 0, 0.194445, 0, 0, 0.194445, 0, 0, 0, 0, 0, 0.194445,
    0.194445, 0, 0, 0, 0, 0, 0, 0, 0.194445, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0);

procedure T_MetaPost_Device.RotText(P: TPoint2D; H, ARot:
  TRealType;
  WideText: WideString; TeXText: AnsiString;
  const HJustification: THJustification;
  const VJustification: TVJustification;
  const LineColor: TColor;
  const FaceName: AnsiString;
  const Charset: TFontCharSet; const Style: TFontStyles);
var
  St, StRot: string;
  HH, MaxDepth, MaxFontDepth, MaxHeight: TRealType;
  D: TVector2D;
  I: Integer;
begin
  MaxFontDepth := 0;
  for I := 0 to 255 do
    if CMR_DP[I] > MaxFontDepth then MaxFontDepth := CMR_DP[I];
    //WriteLnStream('defaultfont:="ftm";');
    //WriteLnStream('defaultfont:="Times-Roman";');
    //WriteLnStream('defaultfont:="cmr10";');
  WriteLnStream(Format('defaultscale:=%.2fu/fontsize defaultfont;',
    [H]));
  St := Get_TeXText(LineColor, Style, WideText, TeXText);
  if not fDrawing2D.MetaPostTeXText then
  begin
    MaxDepth := 0;
    MaxHeight := 0;
    for I := 1 to Length(St) do
    begin
      D.Y := CMR_DP[Ord(St[I])];
      if D.Y > MaxDepth then MaxDepth := D.Y;
      D.Y := CMR_HT[Ord(St[I])];
      if D.Y > MaxHeight then MaxHeight := D.Y;
    end;
    case VJustification of
      jvBottom: D.Y := MaxFontDepth - MaxDepth;
      jvCenter: D.Y := {-(1 - MaxFontDepth) / 2 - MaxDepth}
        (MaxFontDepth - MaxDepth - 1 + MaxHeight) / 2;
      jvTop: D.Y := -1 + MaxHeight {-1 - MaxDepth};
      jvBaseline: D.Y := 0;
    end;
  end
  else
    case VJustification of
      jvBaseline: D.Y := 0;
      jvBottom: D.Y := -0.1 - 0.05;
      jvCenter: D.Y := 0 - 0.05;
      jvTop: D.Y := 0.1 - 0.05;
    end;
  D.X := 0;
  if ARot <> 0 then D := TransformVector2D(D, Rotate2D(ARot));
  D := TransformVector2D(D, Scale2D(H, H));
  WriteStream(Format('textX:=%.2fu; ',
    [P.X + D.X]));
  WriteStream(Format('textY:=%.2fu; ',
    [P.Y + D.Y]));
  WriteStream('pic:=thelabel');
  case HJustification of
    jhLeft:
      case VJustification of
        jvBaseline: WriteStream('.urt');
        jvBottom {, jvCenter}: WriteStream('.urt');
        jvCenter: WriteStream('.rt');
        jvTop: WriteStream('.lrt');
      end;
    jhCenter:
      case VJustification of
        jvBaseline: WriteStream('.top');
        jvBottom {, jvCenter}: WriteStream('.top');
        jvCenter: ;
        jvTop: WriteStream('.bot');
      end;
    jhRight:
      case VJustification of
        jvBaseline: WriteStream('.ulft');
        jvBottom {, jvCenter}: WriteStream('.ulft');
        jvCenter: WriteStream('.lft');
        jvTop: WriteStream('.llft');
      end;
  end;
  if fDrawing2D.MetaPostTeXText then
  begin
    HH := H * 2.84527559;
        //in pt // mm=2.845pt
    if VJustification = jvBaseline then
      WriteStream(Format('(btex \raisebox{0pt}[0pt][0pt]{\fontsize{10}{12}\selectfont %s\strut} etex scaled %.2f,(0,0)); ',
        [St, HH / 10]))
    else
      WriteStream(Format('(btex \fontsize{10}{12}\selectfont %s\strut etex scaled %.2f,(0,0)); ', //\frame{}
        [St, HH / 10]))
          // \setlength{\baselineskip}{?pt}           \fontsize{12}{12}
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

end.


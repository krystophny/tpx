unit Gr32Add; //Addition to Gr32 functionality

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface

uses 
Graphics, Math, 
{$IFDEF VER140}
Windows, Gr32
{$ELSE}
LCLIntf
{$ENDIF}
;

procedure RenderTextW_Rot(const B0: TBitmap32;
  X, Y: Integer; const Text: Widestring; AALevel: Integer; Color: TColor32;
  const FontName: string; const FontSize: Double;
  const Bold, Italic: Boolean;
  const Charset: TFontCharSet;
  const HJustification, VJustification: Integer;
  const Rot: Double);

implementation

uses 
{$IFDEF VER140}
WinBasic, 
{$ELSE}
{$ENDIF}
Drawings, Geometry;


//  THJustification = (jhLeft, jhCenter, jhRight);
//  TVJustification = (jvBaseline, jvBottom, jvCenter, jvTop);

procedure RenderTextW_Rot(const B0: TBitmap32;
  X, Y: Integer; const Text: Widestring; AALevel: Integer; Color: TColor32;
  const FontName: string; const FontSize: Double;
  const Bold, Italic: Boolean;
  const Charset: TFontCharSet;
  const HJustification, VJustification: Integer;
  const Rot: Double);
var
  LogFont2: TLOGFONT;
  B, B2: TBitmap32;
  StockBitmap: Graphics.TBitmap;
  SZ: TSize;
  Rect, BoundRect: TRect2D;
  D, V: TVector2D;
  P: TPoint2D;
  T: TTransf2D;
  Alpha: TColor32;
  StockCanvas: TCanvas;
  PaddedText: Widestring;
  ExtendedFont: TExtendedFont;
  Text_Metric: tagTEXTMETRIC;
  procedure TextBlueToAlpha(const B: TBitmap32; const Color:
    TColor32);
  var
    I: Integer;
    P: PColor32;
    C: TColor32;
  begin
  // convert blue channel to alpha and fill the color
    P := @B.Bits[0];
    for I := 0 to B.Width * B.Height - 1 do
    begin
      C := P^;
      if C <> 0 then
      begin
        C := P^ shl 24; // transfer blue channel to alpha
        C := C + Color;
        P^ := C;
      end;
      Inc(P);
    end;
  end;
  procedure TextScaleDown(const B, B2: TBitmap32; const N: Integer;
    const Color: TColor32); // use only the blue channel
  var
    I, J, X, Y, P, Q, SZ, S: Integer;
    Src: PColor32;
    Dst: PColor32;
  begin
    SZ := 1 shl N - 1;
    Dst := B.PixelPtr[0, 0];
    for J := 0 to B.Height - 1 do
    begin
      Y := J shl N;
      for I := 0 to B.Width - 1 do
      begin
        X := I shl N;
        S := 0;
        for Q := Y to Y + SZ do
        begin
          Src := B2.PixelPtr[X, Q];
          for P := X to X + SZ do
          begin
            S := S + Integer(Src^ and $000000FF);
            Inc(Src);
          end;
        end;
        S := S shr N shr N;
        Dst^ := TColor32(S shl 24) + Color;
        Inc(Dst);
      end;
    end;
  end;
begin
  if B0.Empty then Exit;
  Alpha := Color shr 24;
  Color := Color and $00FFFFFF;
  //AALevel := Constrain(AALevel, 0, 4);
  PaddedText := Text { + ' '};
  ExtendedFont := TExtendedFont.Create;
  ExtendedFont.Free;
  ExtendedFont := TExtendedFont.Create;

  B := TBitmap32.Create;
  try
    if AALevel = 0 then
    begin
{$IFDEF CLX}
      B.Font := Font;
      SZ := B.TextExtentW(PaddedText);
      B.SetSize(SZ.CX, SZ.CY);
{$ELSE}
      SZ := B0.TextExtentW(PaddedText);
      B.SetSize(SZ.CX, SZ.CY);
      B.Font := B0.Font;
{$ENDIF}
      B.Clear(0);
      B.Font.Color := clWhite;
      B.TextOut(0, 0, Text);
      TextBlueToAlpha(B, Color);
    end
    else
    begin
      StockBitmap := Graphics.TBitmap.Create;
      StockBitmap.Width := 8;
      StockBitmap.Height := 8;
      StockCanvas := StockBitmap.Canvas;
      StockCanvas.Lock;
      try
        //StockCanvas.Font := B0.Font;
        //StockCanvas.Font.Size := B0.Font.Size shl AALevel;
        //StockCanvas.Font.Name := 'Times New Roman';
        //StockCanvas.Font.Height :=
        //  -Round(FontSize * (1 shl AALevel));
        ExtendedFont.Canvas := StockCanvas;
        ExtendedFont.FaceName := FontName;
        ExtendedFont.Height :=
          -Round(FontSize * (1 shl AALevel));
      ExtendedFont.Italic := Byte(Italic);
      if Bold then ExtendedFont.Weight := FW_BOLD;
      ExtendedFont.Charset := Charset;
{$IFDEF CLX}
        SZ := StockCanvas.TextExtent(PaddedText);
{$ELSE}
        {Windows.GetTextExtentPoint32W(StockCanvas.Handle, PWideChar(PaddedText),
          Length(PaddedText), SZ);}
        SZ := StockCanvas.TextExtent(PaddedText);
{$ENDIF}
        GetTextMetrics(StockCanvas.Handle, Text_Metric);
        //SZ.CX := (SZ.CX shr AALevel + 1) shl AALevel;
        //SZ.CY := (SZ.CY shr AALevel + 1) shl AALevel;
        Rect := Rect2D(-Text_Metric.tmAveCharWidth, 0,
          SZ.CX + Text_Metric.tmAveCharWidth, -SZ.CY);
        case HJustification of
          0 {jhLeft}: V.X := 0;
          1 {jhCenter}: V.X := SZ.CX / 2;
          2 {jhRight}: V.X := SZ.CX;
        end;
        case VJustification of
          3 {jvTop}: V.Y := 0 - Text_Metric.tmInternalLeading;
          2 {jvCenter}:
            V.Y := -(SZ.CY + Text_Metric.tmInternalLeading) / 2 + 0.25;
          1 {jvBottom}: V.Y := -SZ.CY;
          0 {jvBaseline}: V.Y := -SZ.CY + Text_Metric.tmDescent + 0.5;
        end;
        //T := RotateCenter2D(Rot, Points[0]);
        T := Rotate2D(DegToRad(Rot / 10));
        BoundRect := TransformBoundingBox2D(Rect, T);
        BoundRect.Left := BoundRect.Left - Text_Metric.tmAveCharWidth;
        BoundRect.Right := BoundRect.Right + Text_Metric.tmAveCharWidth;
        V := TransformVector2D(V, T);
        SZ.CX := Round(BoundRect.Right - BoundRect.Left);
        SZ.CY := Round(BoundRect.Top - BoundRect.Bottom);
        D.X := Max(-BoundRect.Left, 0);
        D.Y := Max(BoundRect.Top, 0);
        B2 := TBitmap32.Create;
        try
          //StockCanvas.Font.Size := B0.Font.Size shl AALevel;
          B2.SetSize(SZ.CX, SZ.CY);
          B2.Clear(0);
          B2.Font := StockCanvas.Font;
          B2.Font.Size := B0.Font.Size shl AALevel;
          B2.Font.Color := clWhite;
          {GetObject(GetStockObject(DEFAULT_GUI_FONT), SizeOf(LogFont2),
            @LogFont2);
          LogFont2.lfFaceName := 'Times New Roman';}
          LogFont2 := ExtendedFont.LogFont;
          LogFont2.lfEscapement := Round(Rot);
          LogFont2.lfOrientation := LogFont2.lfEscapement;
          //LogFont2.lfHeight := -Round(FontSize * (1 shl AALevel));
          //Windows.SetTextAlign(B2.Canvas.Handle, TA_LEFT + TA_BOTTOM);
          B2.Font.Handle := CreateFontIndirect(LogFont2);
          //B2.TextoutW(0, 0, Text);
          {B2.Canvas.Pen.Color := clWhite;
          B2.Canvas.Pen.Width := 5;
          B2.Canvas.MoveTo(1, 1);
          B2.Canvas.LineTo(B2.Width - 2, 1);
          B2.Canvas.LineTo(B2.Width - 2, B2.Height - 2);
          B2.Canvas.LineTo(1, B2.Height - 2);
          B2.Canvas.LineTo(1, 1);}
          B2.TextoutW(Round(D.X), Round(D.Y), Text);
          //B2.SaveToFile('C:\WRK\-.bmp');
          B2.StretchFilter := sfLinear;
          B.SetSize(SZ.CX shr AALevel, SZ.CY shr AALevel);
          TextScaleDown(B, B2, AALevel, Color);
        finally
          B2.Free;
        end;
      finally
        StockCanvas.Unlock;
        StockBitmap.Free;
      end;
    end;

    B.DrawMode := dmBlend;
    B.MasterAlpha := Alpha;

    B.DrawTo(B0, X - (Round(D.X + V.X { + Text_Metric.tmAveCharWidth}) shr
      AALevel + 1),
      Y - (Round(D.Y - V.Y) shr AALevel + 1));
    {B0.Canvas.Pen.Width := 3;
    B0.Canvas.Pen.Color := clBlue;
    B0.Canvas.MoveTo(X - 3, Y - 3);
    B0.Canvas.LineTo(X - 3, Y + 3);
    B0.Canvas.LineTo(X + 3, Y + 3);
    B0.Canvas.LineTo(X + 3, Y - 3);
    B0.Canvas.LineTo(X - 3, Y - 3);}
  finally
    B.Free;
    //ExtendedFont.Canvas := nil;
    //ExtendedFont.Free;
  end;
end;

end.


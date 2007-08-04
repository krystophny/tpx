unit DevCanvas;

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface

uses Types, Classes,
{$IFDEF VER140}
  Windows, WinBasic,
{$ELSE}
  LCLIntf, LazBasic,
{$ENDIF}
  Graphics, Geometry, Devices, ColorEtc;

{$IFNDEF VER140}
{$I EMF_Defs.inc}
{$ENDIF}

type

  TDrawPathProc = procedure(const Canvas: TCanvas;
    const IsClosed: Boolean; const Pnts: array of TPoint);

  TCanvasDevice = class(TDevice)
  protected
    fCnv: TCanvas;
    fFaceName: string;
    procedure Bezier(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching; const Transf: TTransf2D;
      const Closed: Boolean); virtual;
    procedure RotText(P: TPoint2D; H, ARot: TRealType;
      WideText: Widestring; TeXText: AnsiString;
      const HJustification: THJustification;
      const VJustification: TVJustification;
      const LineColor: TColor;
      const AFaceName: AnsiString;
      const Charset: TFontCharSet; const Style: TFontStyles);
    procedure HatchingLine(P0, P1: TPoint2D;
      const LineColor: TColor; const LineStyle: TLineStyle;
      const LineWidth: TRealType); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Poly(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching; const Transf: TTransf2D;
      const Closed: Boolean); override;
    procedure BoundingBox2D(const Box, Clip: TRect2D;
      const S: TTransf2D);
    property Cnv: TCanvas read fCnv write fCnv;
    property FaceName: string write fFaceName;
  end;

  TRubberCanvasDevice = class(TCanvasDevice)
  protected
    procedure Bezier(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching; const Transf: TTransf2D;
      const Closed: Boolean); override;
  public
    constructor Create;
    procedure Poly(PP: TPointsSet2D;
      const LineColor, HatchColor, FillColor: TColor;
      const LineStyle: TLineStyle; const LineWidth: TRealType;
      const Hatching: THatching; const Transf: TTransf2D;
      const Closed: Boolean); override;
  end;

  { Drawing functions}

procedure CnvDrawLine(const Cnv: TCanvas;
  const P0, P1: TPoint2D);
procedure CnvDrawPolyline(const Cnv: TCanvas;
  Pnts: array of TPoint; const N: Integer);
procedure CnvDrawPolygon(const Cnv: TCanvas;
  Pnts: array of TPoint; const N: Integer);
procedure CnvDrawBezierPath(const Cnv: TCanvas;
  const Closed: Boolean; const Pnts: array of TPoint);
procedure CnvDrawPolylinePath(const Canvas: TCanvas;
  const IsClosed: Boolean; const Pnts: array of TPoint);

{    Vect should be of type PVectPoints2D, Count is the number of points. }
procedure Draw2DSubSetAsPolygon(const Vect: Pointer; Count:
  Integer;
  const Cnv: TCanvas;
  const Clip, Extent: TRect2D;
  const S: TTransf2D;
  const StartIdx, EndIdx: Integer);
procedure Draw2DSubSetAsPolyline(const Vect: Pointer; Count:
  Integer;
  const Cnv: TCanvas;
  const Clip, Extent: TRect2D;
  const S: TTransf2D;
  const StartIdx, EndIdx: Integer;
  const ToBeClosed: Boolean);
  {: This is an utility procedure useful when you want to mark a control point of
     a graphic object.

     It draws a small square of size <I=Wdt> centered at <I=X,Y> on the
     canvas <I=Cnv>. All the parameters are refered to window's screen
     coordinates. <I=Wdt> is half of the side of the square.
  }
procedure CnvDrawControlPoint(const Cnv: TCanvas;
  const P: TPoint2D; const Wdt: Integer);
procedure CnvDrawRoundControlPoint(const Cnv: TCanvas;
  const P: TPoint2D; const Wdt: Integer);
procedure CnvDrawControlPoint3(const Cnv: TCanvas;
  const P: TPoint2D; const Wdt: Integer);
  {: This function can be used to draw a rectangle onto a
     canvas.

     The rectangle being drawed is first transformed by <I=MT>,
     then it is clipped within the specified region and
     then mapped on the screen with the matrix <I=S>.

     Parameters:

     <LI=<I=Cnv> is the canvas on which the rectangle is drawed.>
     <LI=<I=Box> is the rectangle to be drawed.>
     <LI=<I=Clip> is the clipping rectangle.>
     <LI=<I=MT> is the transform matrix to be applied to Box.>
     <LI=<I=S> is the mapping matrix used to map the clipped Box to the screen.>
  }
procedure DrawRect2DAsPolyline(const Cnv: TCanvas;
  const Box, Clip: TRect2D; const MT, S: TTransf2D);
  {: This function can be used to draw a box onto a canvas.

     The box being drawed is first transformed by
     <I=MT>, then it is clipped within the specified region and
     then mapped on the screen with the matrix <I=S>.

     The rectangle is filled with the canvas brush.

     Parameters:

     <LI=<I=Cnv> is the canvas on which the rectangle is drawed.>
     <LI=<I=Box> is the rectangle to be drawed.>
     <LI=<I=Clip> is the clipping rectangle.>
     <LI=<I=MT> is the transform matrix to be applied to Box>
     <LI=<I=S> is the mapping matrix used to map the clipped Boxto the screen.>
  }
procedure DrawRect2DAsPolygon(const Cnv: TCanvas; const
  Box, Clip: TRect2D; const MT, S: TTransf2D);
  {: This function can be used to draw a line onto a canvas.

     The line being drawed is clipped within the specified
     region and then mapped on the screen with the matrix <I=S>.

     Parameters:

     <LI=<I=Cnv> is the canvas on which the line is drawed.>
     <LI=<I=P1> is the first point of the line.>
     <LI=<I=P2> is the second point of the line.>
     <LI=<I=Clip>. This is the clipping rectangle.>
     <LI=<I=S> is the mapping matrix used to map the clipped line to the screen.>
  }
function DrawLine2D(const Cnv: TCanvas; P1, P2:
  TPoint2D; const Clip: TRect2D; const S: TTransf2D): Boolean;
  {: This function can be used to draw a bounding box onto
     a canvas.

     The box being drawed is first transformed by <I=MT>.
     This transformation keep the bounding box nature of
     the rectangle, so the transformed box has the edges
     parallel to the axis of the world.

     After that it is mapped onto the screen and clipped
     against the clipping region.

     Parameters:

     <LI=<I=Cnv> is the canvas on which the rectangle is drawed.>
     <LI=<I=Box> is the rectangle to be drawed.>
     <LI=<I=Clip> is the clipping rectangle.>
     <LI=<I=MT> is the transform matrix to be applied to <I=Box>.>
     <LI=<I=S> is the mapping matrix used to map the clipped
      <I=Box> to the screen.>
  }
procedure DrawBoundingBox2D(const Cnv: TCanvas; Box,
  Clip: TRect2D; const S: TTransf2D);


    {: Draw the entire set of the points in the set onto a Canvas as
       a closed polygon filled with the current brush of the Canvas.

       <I=Cnv> is the destination canvas on which the set will
       be drawed; <I=Clip> is the clipping rectangle in canvas
       coordinates used to clip the output; <I=Extent> is the
       extension of the set as returned from <See Property=TPointsSet2D@Extension>;
       <I=S> is the viewport transformation as returned from
       <See Property=TViewport@ViewportToScreenTransform>.

       The set will be drawed as a closed polyline (it will be
       closed automatically by the method) with the outline drawed
       with the current pen and the interior filled with the
       current brush of the canvas.

       <B=Note 1>: The <I=Clip> parameter is of type <See Type=TRect2D>
       but it is referred to convas coordinates that are of type <I=TRect>.
       For this you have to use the <See Function=RectToRect2D> function
       to pass the <I=ClipRect> property of the canvas.

       <B=Note2>: The clipping region of the canvas have to be the
       entire canvas extension (that is no user defined clipping region
       must be is set).
    }
procedure DrawAsPolygonFill(PP: TPointsSet2D; const Cnv: TCanvas;
  const
  Clip, Extent: TRect2D; const S: TTransf2D);
    //TSY:
procedure DrawAsPolygonStroke(PP: TPointsSet2D; const Cnv:
  TCanvas;
  const
  Clip, Extent: TRect2D; const S: TTransf2D);
    {: Draw the entire set of the points in the set onto a Canvas
       as a polyline.

       <I=Cnv> is the destination canvas on which the set will
       be drawed; <I=Clip> is the clipping rectangle in canvas
       coordinates used to clip the output; <I=Extent> is the
       extension of the set as returned from
       <See Property=TPointsSet2D@Extension>;
       <I=S> is the viewport transformation as returned from
       <See Property=TViewport@ViewportToScreenTransform>.

       The set will be drawed as a polyline (not closed by the
       method) with the outline drawed with the current pen of
       the canvas and not filled.

       <B=Note 1>: The <I=Clip> parameter is of type <See Type=TRect2D>
       but it is referred to convas coordinates that are of type <I=TRect>.
       For this you have to use the <See Function=RectToRect2D> function
       to pass the <I=ClipRect> property of the canvas.

       <B=Note 2>: The clipping region of the canvas have to be the
       entire canvas extension (that is no user defined clipping region
       must be is set).
    }
procedure DrawAsPolyline(PP: TPointsSet2D; const Cnv: TCanvas; const
  Clip, Extent: TRect2D; const S: TTransf2D);

implementation

uses Math, Drawings, SysBasic;

procedure CnvDrawLine(const Cnv: TCanvas;
  const P0, P1: TPoint2D);
begin
  Cnv.MoveTo(Round(P0.X), Round(P0.Y));
  Cnv.LineTo(Round(P1.X), Round(P1.Y));
end;

procedure CnvDrawPolyline(const Cnv: TCanvas;
  Pnts: array of TPoint; const N: Integer);
begin
  if N < 2 then Exit;
{$IFDEF VER140}
  BeginPath(Cnv.Handle);
  MoveToEx(Cnv.Handle, Pnts[0].X, Pnts[0].Y, nil);
  PolyLineTo(Cnv.Handle, Pnts[1], N - 1);
  EndPath(Cnv.Handle);
{$ELSE}
  Cnv.Polyline(@Pnts, N);
{$ENDIF}
end;

procedure CnvDrawPolygon(const Cnv: TCanvas;
  Pnts: array of TPoint; const N: Integer);
begin
  if N < 2 then Exit;
{$IFDEF VER140}
  BeginPath(Cnv.Handle);
  MoveToEx(Cnv.Handle, Pnts[0].X, Pnts[0].Y, nil);
  PolyLineTo(Cnv.Handle, Pnts[1], N - 1);
  CloseFigure(Cnv.Handle);
  EndPath(Cnv.Handle);
{$ELSE}
  Cnv.Polygon(Pnts, N);
{$ENDIF}
end;

procedure CnvDrawPolylinePath(const Canvas: TCanvas;
  const IsClosed: Boolean; const Pnts: array of TPoint);
begin
  if IsClosed then
    CnvDrawPolygon(Canvas, Pnts, Length(Pnts))
  else
    CnvDrawPolyline(Canvas, Pnts, Length(Pnts));
end;

procedure CnvDrawBezierPath(const Cnv: TCanvas;
  const Closed: Boolean; const Pnts: array of TPoint);
begin
  if Length(Pnts) < 2 then Exit;
{$IFDEF VER140}
  BeginPath(Cnv.Handle);
  MoveToEx(Cnv.Handle, Pnts[0].X, Pnts[0].Y, nil);
  PolyBezierTo(Cnv.Handle, Pnts[1], Length(Pnts) - 1);
  if Closed then CloseFigure(Cnv.Handle);
  EndPath(Cnv.Handle);
{$ELSE}
  if Length(Pnts) = 4 then
// Is there some bug in Lazarus code of PolyBezier
//   for 1 segment and Continuous path? (Under Windows only?)
    Cnv.PolyBezier(Pnts, Length(Pnts), Closed, False)
  else if Closed then
    Cnv.PolyBezier(Pnts, Length(Pnts) - 1, True, True)
  else
    Cnv.PolyBezier(Pnts, Length(Pnts), False, True)
      ;
{$ENDIF}
end;

// Drawing functions ~~~~~~~~~~~~~~~~~~~~~~~~

procedure Draw2DSubSetAsPolygon(const Vect: Pointer; Count:
  Integer;
  const Cnv: TCanvas;
  const Clip, Extent: TRect2D;
  const S: TTransf2D;
  const StartIdx, EndIdx: Integer);
type
  TPoints = array[0..0] of TPoint;
var
  VisPoints, VisPoints1, Cont: Integer;
  TmpPt1, TmpPt2: TPoint2D;
  TmpPts, FirstClipPts: ^TPoints;
  ClipRes: TClipResult;
  TmpClip: TRect2D;
begin
  if (Count = 0) or (EndIdx > Count) or (StartIdx > EndIdx) then
    Exit;
  GetMem(TmpPts, Count * 3 * SizeOf(TPoint));
  GetMem(FirstClipPts, Count * 3 * SizeOf(TPoint));
  try
    VisPoints := 0;
    VisPoints1 := 0;
    if IsBoxAllInBox2D(TransformRect2D(Extent, S), Clip) then
      for Cont := StartIdx to EndIdx do
      begin
        TmpPt1 := TransformPoint2D(PVectPoints2D(Vect)^[Cont],
          S);
        TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
        Inc(VisPoints);
      end
    else
    begin
      TmpClip := ReorderRect2D(Clip);
      for Cont := StartIdx to EndIdx do
      begin
        TmpPt1 := TransformPoint2D(PVectPoints2D(Vect)^[Cont],
          S);
        if Cont < EndIdx then
          TmpPt2 := TransformPoint2D(PVectPoints2D(Vect)^[Cont +
            1], S)
        else
          TmpPt2 :=
            TransformPoint2D(PVectPoints2D(Vect)^[StartIdx], S);
        ClipRes := _ClipLineLeftRight2D(TmpClip, TmpPt1,
          TmpPt2);
        if not (ccNotVisible in ClipRes) then
        begin
          FirstClipPts^[VisPoints1] := Point2DToPoint(TmpPt1);
          Inc(VisPoints1);
        end;
        if ccSecond in ClipRes then
        begin
          FirstClipPts^[VisPoints1] := Point2DToPoint(TmpPt2);
          Inc(VisPoints1);
        end;
      end;
      FirstClipPts^[VisPoints1] := FirstClipPts^[0];
      Inc(VisPoints1);
      VisPoints := 0;
      for Cont := 0 to VisPoints1 - 2 do
      begin
        TmpPt1 := PointToPoint2D(FirstClipPts^[Cont]);
        TmpPt2 := PointToPoint2D(FirstClipPts^[Cont + 1]);
        ClipRes := _ClipLineUpBottom2D(TmpClip, TmpPt1, TmpPt2);
        if not (ccNotVisible in ClipRes) then
        begin
          TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
          Inc(VisPoints);
        end;
        if ccSecond in ClipRes then
        begin
          TmpPts^[VisPoints] := Point2DToPoint(TmpPt2);
          Inc(VisPoints);
        end;
      end;
    end;
    if (VisPoints > 0) then
      CnvDrawPolygon(Cnv, TmpPts^, VisPoints);
  finally
    FreeMem(TmpPts, Count * 3 * SizeOf(TPoint));
    FreeMem(FirstClipPts, Count * 3 * SizeOf(TPoint));
  end;
end;

procedure Draw2DSubSetAsPolyline(const Vect: Pointer; Count:
  Integer;
  const Cnv: TCanvas;
  const Clip, Extent: TRect2D;
  const S: TTransf2D;
  const StartIdx, EndIdx: Integer;
  const ToBeClosed: Boolean);
type
  TPoints = array[0..0] of TPoint;
var
  VisPoints, Cont, AllocatedMem: Integer;
  TmpPt1, TmpPt2: TPoint2D;
  TmpPts: ^TPoints;
  ClipRes: TClipResult;
  TmpClip: TRect2D;
begin
  if (Count = 0) or (EndIdx > Count) or (StartIdx > EndIdx) then
    Exit;
  if ToBeClosed then
    AllocatedMem := (Count + 1) * SizeOf(TPoint)
  else
    AllocatedMem := Count * SizeOf(TPoint);
  GetMem(TmpPts, AllocatedMem);
  try
    VisPoints := 0;
    if IsBoxAllInBox2D(TransformRect2D(Extent, S), Clip) then
    begin
      for Cont := StartIdx to EndIdx do
      begin
        TmpPt1 := TransformPoint2D(PVectPoints2D(Vect)^[Cont],
          S);
        TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
        Inc(VisPoints);
      end;
      if ToBeClosed then
      begin
        TmpPt1 :=
          TransformPoint2D(PVectPoints2D(Vect)^[StartIdx], S);
        TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
        Inc(VisPoints);
      end;
      ClipRes := [ccVisible];
    end
    else if ToBeClosed then
    begin
      TmpClip := ReorderRect2D(Clip);
      for Cont := StartIdx + 1 to EndIdx + 1 do
      begin
        TmpPt1 := TransformPoint2D(PVectPoints2D(Vect)^[Cont -
          1], S);
        if Cont > EndIdx then
          TmpPt2 :=
            TransformPoint2D(PVectPoints2D(Vect)^[StartIdx], S)
        else
          TmpPt2 := TransformPoint2D(PVectPoints2D(Vect)^[Cont],
            S);
        ClipRes := _ClipLine2D(TmpClip, TmpPt1, TmpPt2);
        if not (ccNotVisible in ClipRes) then
        begin
          TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
          Inc(VisPoints);
        end;
        if ccSecond in ClipRes then
        begin
          TmpPts^[VisPoints] := Point2DToPoint(TmpPt2);
          Inc(VisPoints);
          CnvDrawPolyline(Cnv, TmpPts^, VisPoints);
          VisPoints := 0;
        end;
      end;
      if not (ccNotVisible in ClipRes) then
      begin
        TmpPts^[VisPoints] := Point2DToPoint(TmpPt2);
        Inc(VisPoints);
      end;
    end
    else
    begin
      TmpClip := ReorderRect2D(Clip);
      for Cont := StartIdx + 1 to EndIdx do
      begin
        TmpPt1 := TransformPoint2D(PVectPoints2D(Vect)^[Cont -
          1], S);
        TmpPt2 := TransformPoint2D(PVectPoints2D(Vect)^[Cont],
          S);
        ClipRes := _ClipLine2D(TmpClip, TmpPt1, TmpPt2);
        if not (ccNotVisible in ClipRes) then
        begin
          TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
          Inc(VisPoints);
        end;
        if ccSecond in ClipRes then
        begin
          TmpPts^[VisPoints] := Point2DToPoint(TmpPt2);
          Inc(VisPoints);
          CnvDrawPolyline(Cnv, TmpPts^, VisPoints);
          VisPoints := 0;
        end;
      end;
      if not (ccNotVisible in ClipRes) then
      begin
        TmpPts^[VisPoints] := Point2DToPoint(TmpPt2);
        Inc(VisPoints);
      end;
    end;
    if (VisPoints > 0) then
      CnvDrawPolyline(Cnv, TmpPts^, VisPoints);
  finally
    FreeMem(TmpPts, AllocatedMem);
  end;
end;

procedure CnvDrawControlPoint(const Cnv: TCanvas;
  const P: TPoint2D; const Wdt: Integer);
var
  TmpWdt, X, Y: Integer;
begin
  with Cnv do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clSilver;
    Pen.Style := psSolid;
    TmpWdt := Wdt div 2;
    X := Round(P.X);
    Y := Round(P.Y);
    Cnv.Rectangle(X - TmpWdt, Y - TmpWdt, X + TmpWdt, Y + TmpWdt);
  end;
end;

procedure CnvDrawRoundControlPoint(const Cnv: TCanvas;
  const P: TPoint2D; const Wdt: Integer);
var
  TmpWdt, X, Y: Integer;
begin
  with Cnv do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clSkyBlue;
    Pen.Style := psSolid;
    TmpWdt := Wdt div 2;
    X := Round(P.X);
    Y := Round(P.Y);
    Cnv.Ellipse(X - TmpWdt, Y - TmpWdt, X + TmpWdt, Y + TmpWdt);
  end;
end;

procedure CnvDrawControlPoint3(const Cnv: TCanvas;
  const P: TPoint2D; const Wdt: Integer);
var
  TmpWdt, X, Y: Integer;
begin
  with Cnv do
  begin
    Pen.Style := psSolid;
    TmpWdt := Wdt div 2;
    X := Round(P.X);
    Y := Round(P.Y);
    Brush.Style := bsSolid;
    Brush.Color := clBlack;
    Cnv.Rectangle(X - TmpWdt, Y - TmpWdt, X + TmpWdt, Y + TmpWdt);
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    Cnv.Ellipse(X - TmpWdt, Y - TmpWdt, X + TmpWdt, Y + TmpWdt);
  end;
end;

procedure DrawRect2DAsPolyline(const Cnv: TCanvas;
  const Box, Clip: TRect2D; const MT, S: TTransf2D);
type
  TPoints = array[0..0] of TPoint;
var
  VisPoints, Cont: Integer;
  TmpPt1, TmpPt2: TPoint2D;
  TmpPts: ^TPoints;
  BoxPts: ^TVectPoints2D;
  ClipRes: TClipResult;
  TmpClip: TRect2D;
begin
  GetMem(TmpPts, 5 * SizeOf(TPoint));
  GetMem(BoxPts, 5 * SizeOf(TPoint2D));
  try
    VisPoints := 0;
    Cont := 0;
    BoxPts^[Cont] := TransformPoint2D(Box.FirstEdge, MT);
    BoxPts^[Cont + 1] := TransformPoint2D(Point2D(Box.Left,
      Box.Top), MT);
    BoxPts^[Cont + 2] := TransformPoint2D(Box.SecondEdge, MT);
    BoxPts^[Cont + 3] := TransformPoint2D(Point2D(Box.Right,
      Box.Bottom), MT);
    BoxPts^[Cont + 4] := TransformPoint2D(Box.FirstEdge, MT);
    if IsBoxAllInBox2D(TransformRect2D(Box, S), Clip) then
      for Cont := 0 to 4 do
      begin
        TmpPt1 := TransformPoint2D(PVectPoints2D(BoxPts)^[Cont],
          S);
        TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
        Inc(VisPoints);
        ClipRes := [ccVisible];
      end
    else
    begin
      TmpClip := ReorderRect2D(Clip);
      for Cont := 1 to 4 do
      begin
        TmpPt1 := TransformPoint2D(PVectPoints2D(BoxPts)^[Cont -
          1], S);
        TmpPt2 := TransformPoint2D(PVectPoints2D(BoxPts)^[Cont],
          S);
        ;
        ClipRes := _ClipLine2D(TmpClip, TmpPt1, TmpPt2);
        if not (ccNotVisible in ClipRes) then
        begin
          TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
          Inc(VisPoints);
        end;
        if ccSecond in ClipRes then
        begin
          TmpPts^[VisPoints] := Point2DToPoint(TmpPt2);
          Inc(VisPoints);
          CnvDrawPolyline(Cnv, TmpPts^, VisPoints);
          VisPoints := 0;
        end;
      end;
      if not (ccNotVisible in ClipRes) then
      begin
        TmpPts^[VisPoints] := Point2DToPoint(TmpPt2);
        Inc(VisPoints);
      end;
    end;
    if (VisPoints > 0) then
      CnvDrawPolyline(Cnv, TmpPts^, VisPoints);
  finally
    FreeMem(BoxPts, 5 * SizeOf(TPoint2D));
    FreeMem(TmpPts, 5 * SizeOf(TPoint));
  end;
end;

procedure DrawRect2DAsPolygon(const Cnv: TCanvas; const
  Box, Clip: TRect2D; const MT, S: TTransf2D);
type
  TPoints = array[0..0] of TPoint;
var
  VisPoints, VisPoints1, Cont: Integer;
  TmpPt1, TmpPt2: TPoint2D;
  TmpPts, FirstClipPts: ^TPoints;
  BoxPts: ^TVectPoints2D;
  ClipRes: TClipResult;
  TmpClip: TRect2D;
begin
  GetMem(TmpPts, 15 * SizeOf(TPoint));
  GetMem(FirstClipPts, 15 * SizeOf(TPoint));
  GetMem(BoxPts, 5 * SizeOf(TPoint2D));
  try
    Cont := 0;
    BoxPts^[Cont] := TransformPoint2D(Box.FirstEdge, MT);
    BoxPts^[Cont + 1] := TransformPoint2D(Point2D(Box.Left,
      Box.Top), MT);
    BoxPts^[Cont + 2] := TransformPoint2D(Box.SecondEdge, MT);
    BoxPts^[Cont + 3] := TransformPoint2D(Point2D(Box.Right,
      Box.Bottom), MT);
    BoxPts^[Cont + 4] := TransformPoint2D(Box.FirstEdge, MT);
    VisPoints := 0;
    VisPoints1 := 0;
    if IsBoxAllInBox2D(TransformRect2D(Box, S), Clip) then
      for Cont := 0 to 4 do
      begin
        TmpPt1 := TransformPoint2D(PVectPoints2D(BoxPts)^[Cont],
          S);
        TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
        Inc(VisPoints);
      end
    else
    begin
      TmpClip := ReorderRect2D(Clip);
      for Cont := 0 to 4 do
      begin
        TmpPt1 := TransformPoint2D(PVectPoints2D(BoxPts)^[Cont],
          S);
        if Cont < 4 then
          TmpPt2 := TransformPoint2D(PVectPoints2D(BoxPts)^[Cont
            + 1], S)
        else
          TmpPt2 := TransformPoint2D(PVectPoints2D(BoxPts)^[0],
            S);
        ClipRes := _ClipLineLeftRight2D(TmpClip, TmpPt1,
          TmpPt2);
        if not (ccNotVisible in ClipRes) then
        begin
          FirstClipPts^[VisPoints1] := Point2DToPoint(TmpPt1);
          Inc(VisPoints1);
        end;
        if ccSecond in ClipRes then
        begin
          FirstClipPts^[VisPoints1] := Point2DToPoint(TmpPt2);
          Inc(VisPoints1);
        end;
      end;
      FirstClipPts^[VisPoints1].X := FirstClipPts^[0].X;
      FirstClipPts^[VisPoints1].Y := FirstClipPts^[0].Y;
      Inc(VisPoints1);
      VisPoints := 0;
      for Cont := 0 to VisPoints1 - 2 do
      begin
        TmpPt1 := PointToPoint2D(FirstClipPts^[Cont]);
        TmpPt2 := PointToPoint2D(FirstClipPts^[Cont + 1]);
        ClipRes := _ClipLineUpBottom2D(TmpClip, TmpPt1, TmpPt2);
        if not (ccNotVisible in ClipRes) then
        begin
          TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
          Inc(VisPoints);
        end;
        if ccSecond in ClipRes then
        begin
          TmpPts^[VisPoints] := Point2DToPoint(TmpPt2);
          Inc(VisPoints);
        end;
      end;
    end;
    if (VisPoints > 0) then
      CnvDrawPolygon(Cnv, TmpPts^, VisPoints);
  finally
    FreeMem(BoxPts, 5 * SizeOf(TPoint2D));
    FreeMem(TmpPts, 15 * SizeOf(TPoint));
    FreeMem(FirstClipPts, 15 * SizeOf(TPoint));
  end;
end;

function DrawLine2D(const Cnv: TCanvas; P1, P2:
  TPoint2D; const Clip: TRect2D; const S: TTransf2D): Boolean;
var
  TmpPt1, TmpPt2: TPoint;
  TmpPt12D, TmpPt22D: TPoint2D;
  ClipRes: TClipResult;
  TmpBox: TRect2D;
  TmpClip: TRect2D;
begin
  TmpBox.FirstEdge := P1;
  TmpBox.SecondEdge := P2;
  TmpBox := ReorderRect2D(TmpBox);
  if IsBoxAllInBox2D(TransformRect2D(TmpBox, S), Clip) then
  begin
    Result := True;
    TmpPt1 := Point2DToPoint(TransformPoint2D(P1, S));
    TmpPt2 := Point2DToPoint(TransformPoint2D(P2, S));
    Cnv.MoveTo(TmpPt1.X, TmpPt1.Y);
    Cnv.LineTo(TmpPt2.X, TmpPt2.Y);
  end
  else
  begin
    TmpClip := ReorderRect2D(Clip);
    TmpPt12D := TransformPoint2D(P1, S);
    TmpPt22D := TransformPoint2D(P2, S);
    ;
    ClipRes := _ClipLine2D(TmpClip, TmpPt12D, TmpPt22D);
    Result := not (ccNotVisible in ClipRes);
    if Result then
    begin
      TmpPt1 := Point2DToPoint(TmpPt12D);
      TmpPt2 := Point2DToPoint(TmpPt22D);
      Cnv.MoveTo(TmpPt1.X, TmpPt1.Y);
      Cnv.LineTo(TmpPt2.X, TmpPt2.Y);
    end;
  end;
end;

procedure DrawBoundingBox2D(const Cnv: TCanvas; Box,
  Clip: TRect2D; const S: TTransf2D);
begin
  DrawRect2DAsPolyline(Cnv, Box, Clip, IdentityTransf2D, S)
end;

procedure DrawAsPolygonFill(PP: TPointsSet2D;
  const Cnv: TCanvas;
  const Clip, Extent: TRect2D;
  const S: TTransf2D);
var
  PenStyle0: TPenStyle;
begin
  PenStyle0 := Cnv.Pen.Style;
  Cnv.Pen.Style := psClear;
  Draw2DSubSetAsPolygon(PP.PointsPointer, PP.Count,
    Cnv, Clip, Extent, S,
    0, PP.Count - 1);
  Cnv.Pen.Style := PenStyle0;
end;

procedure DrawAsPolygonStroke(PP: TPointsSet2D;
  const Cnv: TCanvas;
  const Clip, Extent: TRect2D; const S: TTransf2D);
var
  BrushStyle0: TBrushStyle;
  BrushColor0: TColor;
begin
  BrushStyle0 := Cnv.Brush.Style;
  BrushColor0 := Cnv.Brush.Color;
  Cnv.Brush.Style := bsClear;
  Draw2DSubSetAsPolygon(PP.PointsPointer, PP.Count,
    Cnv, Clip, Extent, S,
    0, PP.Count - 1);
  Cnv.Brush.Style := BrushStyle0;
  Cnv.Brush.Color := BrushColor0;
end;

procedure DrawAsPolyline(PP: TPointsSet2D;
  const Cnv: TCanvas;
  const Clip, Extent: TRect2D;
  const S: TTransf2D);
begin
  Draw2DSubSetAsPolyline(PP.PointsPointer, PP.Count,
    Cnv, Clip, Extent, S,
    0, PP.Count - 1, False);
end;

{$IFDEF VER140}

procedure DrawNative0(const Cnv: TCanvas;
  const PathProc: TDrawPathProc;
  const ClipRect2D: TRect2D;
  const IsClosed: Boolean;
  const Pnts: array of TPoint;
  const LineStyle: TLineStyle;
  const Line_Width: TRealType;
  const Hatching: THatching;
  const LineColor, FillColor, HatchColor: TColor;
  const LineWidthBase, FactorMM, MiterLimit: TRealType);
var
  BrushStyle0: TBrushStyle;
  BrushColor0: TColor;
  PenStyle, PenStyle0: TPenStyle;
  PenColor0: TColor;
  LOGBRUSH: tagLOGBRUSH;
  LOGPEN: tagLOGPEN;
  //MiterLimit0: Single;
  procedure SetPen(PenStyle: TPenStyle;
    PenColor: TColor);
  begin
    with LOGBRUSH do
    begin
      lbStyle := BS_SOLID;
      lbColor := PenColor;
      lbHatch := 0;
    end;
    Cnv.Pen.Handle := ExtCreatePen(
      PS_GEOMETRIC or Ord(PenStyle) {PS_Solid} or PS_EndCap_Flat or
      PS_Join_Miter,
      Cnv.Pen.Width, LOGBRUSH, 0, nil);
    {with LOGPEN do
    begin
      lopnStyle := Ord(PenStyle);// or PS_JOIN_MITER
      lopnWidth := Point(Cnv.Canvas.Pen.Width, 0);
      lopnColor := PenColor;
    end;
    Cnv.Canvas.Pen.Handle := CreatePenIndirect(LOGPEN);}
  end;
begin
  //Windows GDI is a wonder!
  PenStyle0 := Cnv.Pen.Style;
  PenColor0 := Cnv.Pen.Color;
  SetPolyFillMode(Cnv.Handle, WINDING);
   // winding aka nonzero fill rule
  with LOGBRUSH do
  begin
    if Hatching <> haNone then
      lbStyle := BS_HATCHED
    else if FillColor = clDefault then
      lbStyle := BS_HOLLOW
    else
      lbStyle := BS_SOLID;
    if Hatching <> haNone then
      if HatchColor = clDefault then
        lbColor := clGray
      else
        lbColor := HatchColor
    else if FillColor = clDefault then
      lbColor := clNone
    else
      lbColor := FillColor;
    lbHatch := Ord(Hatching) - 1;
  end;
  Cnv.Brush.Handle := CreateBrushIndirect(LOGBRUSH);
  //If brush style is BS_HATCHED then BkColor is used to fill background!
  if FillColor = clDefault then
  begin
    SetBkColor(Cnv.Handle, {not ColorToRGB(LOGBRUSH.lbColor)}
      clNone);
    Windows.SetBkMode(Cnv.Handle, 0);
  end
  else
  begin
    SetBkColor(Cnv.Handle, FillColor);
    Windows.SetBkMode(Cnv.Handle, 1);
  end;
  PathProc(Cnv, IsClosed, Pnts);
  if LineStyle = liNone then
    SetPen(psClear, clRed)
  else
  begin
    SetMiterLimit(Cnv.Handle, MiterLimit, nil);
    {GetMiterLimit(Cnv.Canvas.Handle, MiterLimit0);
    MiterLimit0 := MiterLimit + MiterLimit0 * 0;}
    case LineStyle of
      liSolid:
        PenStyle := psSolid;
      liDotted:
        PenStyle := psDot;
      liDashed:
        PenStyle := psDash;
    end;
    Cnv.Pen.Width :=
      Max(Round(LineWidthBase * Line_Width * FactorMM),
      1 {Cnv.Canvas.Pen.Width});
    if LineColor = clDefault
      then
      SetPen(PenStyle, clBlack)
    else
      SetPen(PenStyle, LineColor);
  end;
  if LineStyle <> liNone then
    if (Hatching <> haNone) or (FillColor <> clDefault) then
      if not IsClosed then
      begin
        //StrokeAndFillPath is not suitable for opened figures,
        // because it closes all such figures!
        FillPath(Cnv.Handle);
        PathProc(Cnv, IsClosed, Pnts);
        StrokePath(Cnv.Handle);
      end
      else
        StrokeAndFillPath(Cnv.Handle)
    else
      StrokePath(Cnv.Handle)
  else
    FillPath(Cnv.Handle);
  Cnv.Pen.Style := PenStyle0;
  Cnv.Pen.Color := PenColor0;
end;

{$ELSE}

procedure DrawNative0(const Cnv: TCanvas;
  const PathProc: TDrawPathProc;
  const ClipRect2D: TRect2D;
  const IsClosed: Boolean;
  const Pnts: array of TPoint;
  const LineStyle: TLineStyle;
  const Line_Width: TRealType;
  const Hatching: THatching;
  const LineColor, FillColor, HatchColor: TColor;
  const LineWidthBase, FactorMM, MiterLimit: TRealType);
var
  BrushStyle0: TBrushStyle;
  BrushColor0: TColor;
  PenStyle, PenStyle0: TPenStyle;
  PenColor0: TColor;
begin
  PenStyle0 := Cnv.Pen.Style;
  PenColor0 := Cnv.Pen.Color;
  BrushStyle0 := Cnv.Brush.Style;
  BrushColor0 := Cnv.Brush.Color;
  Cnv.Pen.Mode := pmCopy;
  if FillColor = clDefault then
  begin
    Cnv.Brush.Color := clRed;
    Cnv.Brush.Style := bsClear;
  end
  else
  begin
    Cnv.Brush.Color := FillColor;
    Cnv.Brush.Style := bsSolid;
  end;
  if LineStyle = liNone then
  begin
    Cnv.Pen.Style := psClear;
    Cnv.Pen.Color := clRed;
  end
  else
  begin
    //SetMiterLimit(Cnv.Handle, MiterLimit, nil);
    case LineStyle of
      liSolid:
        Cnv.Pen.Style := psSolid;
      liDotted:
        Cnv.Pen.Style := psDot;
      liDashed:
        Cnv.Pen.Style := psDash;
    end;
    Cnv.Pen.Width :=
      Max(Round(LineWidthBase * Line_Width * FactorMM), 1);
    if LineColor = clDefault then
      Cnv.Pen.Color := clBlack
    else
      Cnv.Pen.Color := LineColor;
  end;
  PathProc(Cnv, IsClosed, Pnts);
  if (LineStyle <> liNone) and (LineStyle <> liSolid) and
    (Cnv.Pen.Width > 1) then
  begin
    Cnv.Pen.Width := 1;
    Cnv.Brush.Style := bsClear;
    Cnv.Pen.Mode := pmNotCopy;
    PathProc(Cnv, IsClosed, Pnts);
  end;
  Cnv.Pen.Style := PenStyle0;
  Cnv.Pen.Color := PenColor0;
  Cnv.Brush.Style := BrushStyle0;
  Cnv.Brush.Color := BrushColor0;
end;
{$ENDIF}

// =====================================================================
// TCanvasDevice
// =====================================================================

constructor TCanvasDevice.Create;
begin
  inherited Create;
  fCnv := nil;
  OnRotText := RotText;
{$IFNDEF LINUX}
  OnBezier := Bezier;
//  OnCircle := Circle;
//  OnRotEllipse := RotEllipse;
//  OnRect := Rect;
//  OnCircular := Circular;
  fHasBezier := True;
  fHasClosedBezier := True;
  fHasArc := True;
  fHasSector := True;
  fHasSegment := True;
{$ELSE}
  fHasBezier := False;
  fHasClosedBezier := False;
  fHasArc := False;
  fHasSector := False;
  fHasSegment := False;
{$ENDIF}
{$IFDEF VER140}
{$ELSE}
  fDisjointFill := True;
{$ENDIF}
  fHasNativeHatching := False;
end;

destructor TCanvasDevice.Destroy;
begin
  inherited Destroy;
end;

procedure TCanvasDevice.Poly(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D;
  const Closed: Boolean);
var
  PathProc: TDrawPathProc;
  Pnts: array of TPoint;
  I: Integer;
begin
  if PP.Count <= 1 then Exit;
  SetLength(Pnts, PP.Count);
  for I := 0 to PP.Count - 1 do
    Pnts[I] := Point2DToPoint(TransformPoint2D(PP[I],
      MultTransf(Transf)));
  DrawNative0(fCnv,
    CnvDrawPolylinePath, Rect2D(0, 0, 0, 0),
    Closed, Pnts, LineStyle, LineWidth, Hatching,
    LineColor, FillColor, HatchColor,
    fLineWidthBase, fFactorMM, fMiterLimit);
end;

procedure TCanvasDevice.Bezier(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D;
  const Closed: Boolean);
var
  PathProc: TDrawPathProc;
  Pnts: array of TPoint;
  I: Integer;
begin
  if PP.Count <= 1 then Exit;
  SetLength(Pnts, PP.Count);
  for I := 0 to PP.Count - 1 do
    Pnts[I] := Point2DToPoint(TransformPoint2D(PP[I],
      MultTransf(Transf)));
  DrawNative0(fCnv,
    CnvDrawBezierPath, Rect2D(0, 0, 0, 0),
    Closed, Pnts, LineStyle, LineWidth, Hatching,
    LineColor, FillColor, HatchColor,
    fLineWidthBase, fFactorMM, fMiterLimit);
end;

procedure TCanvasDevice.RotText(
  P: TPoint2D; H, ARot: TRealType;
  WideText: Widestring; TeXText: AnsiString;
  const HJustification: THJustification;
  const VJustification: TVJustification;
  const LineColor: TColor;
  const AFaceName: AnsiString;
  const Charset: TFontCharSet; const Style: TFontStyles);
{$IFDEF VER140}
{$ELSE}
const
  TA_LEFT = 0;
  TA_RIGHT = 2;
  TA_CENTER = 6;
  TA_TOP = 0;
  TA_BOTTOM = 8;
  TA_BASELINE = 24;
{$ENDIF}
var
  Pnt: TPoint;
  TmpP: TPoint2D;
  D: TVector2D;
  TmpTransf: TTransf2D;
  AlignFlags: Word;
  AExtFont: TExtendedFont;
  InternalLeading: Single;
  S: Types.TSize;
//  Text_Metric: tagTEXTMETRIC;
begin
  AExtFont := TExtendedFont.Create;
  if AFaceName <> '' then
    AExtFont.FaceName := AFaceName
  else if fFaceName <> '' then
    AExtFont.FaceName := fFaceName
  else
    AExtFont.FaceName := 'Times New Roman';
{$IFDEF VER140}
  if VJustification = jvTop then
    InternalLeading := GetFontInternalLeading(
      AExtFont.FaceName, Style, Charset);
{$ENDIF}
  fCnv.Brush.Style := bsClear;
  { Build up the DrawText rect. }
  AExtFont.Canvas := fCnv;
  if LineColor <> clDefault then
    AExtFont.Canvas.Font.Color := LineColor
  else
    AExtFont.Canvas.Font.Color := clBlack;
{$IFDEF VER140}
  SetGraphicsMode(fCnv.Handle, GM_ADVANCED); //move somewhere ?
{$ENDIF}
  //Negative Height to set height without internal leading
  AExtFont.Height := -Round(H);
  AExtFont.Italic := Byte(fsItalic in Style);
  if fsBold in Style then AExtFont.Weight := FW_BOLD;
  AExtFont.Charset := Charset;
  ARot := ARot - Floor(ARot / (2 * Pi)) * 2 * Pi;
  AExtFont.Angle := Round(ARot / Pi * 1800);
//  GetTextMetrics(AExtFont.Canvas.Handle, Text_Metric);
//  InternalLeading := Text_Metric.tmInternalLeading / Round(H);
  D.X := 0;
{$IFDEF VER140}
  case VJustification of
    jvBottom: D.Y := 0;
    jvCenter: D.Y := 0.5 * H;
    jvTop: D.Y := -InternalLeading * H;
    jvBaseline: D.Y := 0;
  end;
{$ELSE}
  S := fCnv.TextExtent(WideText);
  //fCnv.TextStyle.Alignment;
  TmpP := PointToPoint2D(Types.Point(S.CX, S.CY));
  case HJustification of
    jhLeft: D.X := 0;
    jhCenter: D.X := TmpP.X / 2;
    jhRight: D.X := TmpP.X;
  end;
  {D.X := 0;
  case HJustification of
    jhLeft: fCnv.TextStyle.Alignment := taLeftJustify;
    jhCenter: fCnv.TextStyle.Alignment := taCenter;
    jhRight: fCnv.TextStyle.Alignment := taRightJustify;
  end;}
  case VJustification of
    jvBottom: D.Y := -1.2 * H;
    jvCenter: D.Y := -0.7 * H;
    jvTop: D.Y := -0.2;
    jvBaseline: D.Y := -1.0 * H;
  end;
{$ENDIF}
  Pnt := Point2DToPoint(P);
  if ARot <> 0 then D := TransformVector2D(D, Rotate2D(ARot));
      //D := TransformVector2D(D, Scale2D(Height, Height));
      //P := ShiftPoint(P, D);
  Pnt.X := Pnt.X - Round(D.X);
  Pnt.Y := Pnt.Y + Round(D.Y);
{$IFDEF VER140}
  AlignFlags := 0;
  case HJustification of
    jhLeft: AlignFlags := AlignFlags + TA_LEFT;
    jhCenter: AlignFlags := AlignFlags + TA_CENTER;
    jhRight: AlignFlags := AlignFlags + TA_RIGHT;
  end;
  case VJustification of
    jvTop: AlignFlags := AlignFlags + TA_TOP;
    jvCenter: AlignFlags := AlignFlags + TA_BOTTOM;
    jvBottom: AlignFlags := AlignFlags + TA_BOTTOM;
    jvBaseline: AlignFlags := AlignFlags + TA_BASELINE;
  end;
  Windows.SetTextAlign(fCnv.Handle, AlignFlags);
  Windows.TextOutW(fCnv.Handle, Pnt.X, Pnt.Y,
    PWideChar(WideText), Length(WideText));
{$ELSE}
  fCnv.TextOut(Pnt.X, Pnt.Y, WideText);
{$ENDIF}
  AExtFont.Free;
end;

procedure TCanvasDevice.HatchingLine(P0, P1: TPoint2D;
  const LineColor: TColor; const LineStyle: TLineStyle;
  const LineWidth: TRealType);
var
  TmpPt0, TmpPt1: TPoint;
begin
  Cnv.Pen.Mode := pmCopy;
  case LineStyle of
    liSolid:
      Cnv.Pen.Style := psSolid;
    liDotted:
      Cnv.Pen.Style := psDot;
    liDashed:
      Cnv.Pen.Style := psDash;
  end;
  Cnv.Pen.Color := LineColor;
  Cnv.Pen.Width :=
    Max(Round(fLineWidthBase * LineWidth * fFactorMM), 1);
  TmpPt0 := Point2DToPoint(TransformPoint2D(P0, fT));
  TmpPt1 := Point2DToPoint(TransformPoint2D(P1, fT));
  Cnv.MoveTo(TmpPt0.X, TmpPt0.Y);
  Cnv.LineTo(TmpPt1.X, TmpPt1.Y);
end;

procedure TCanvasDevice.BoundingBox2D(
  const Box, Clip: TRect2D;
  const S: TTransf2D);
begin
  DrawBoundingBox2D(Cnv, Box, Clip, S);
end;

// =====================================================================
// TRubberCanvasDevice
// =====================================================================

procedure DrawOutlineNative0(const Cnv: TCanvas;
  const PathProc: TDrawPathProc;
  const ClipRect2D: TRect2D;
  const IsClosed: Boolean;
  const Pnts: array of TPoint);
begin
  //BrushStyle0 := Cnv.Brush.Style;
  //BrushColor0 := Cnv.Brush.Color;
  Cnv.Brush.Style := bsClear;
  Cnv.Pen.Mode := pmNot;
  Cnv.Pen.Style := psDash;
  Cnv.Pen.Width := 1;
  PathProc(Cnv, IsClosed, Pnts);
{$IFDEF VER140}
  StrokePath(Cnv.Handle);
{$ELSE}
{$ENDIF}
  //Cnv.Brush.Style := BrushStyle0;
  //Cnv.Brush.Color := BrushColor0;
end;

constructor TRubberCanvasDevice.Create;
begin
  inherited Create;
  fHasNativeHatching := True;
  fDisjointFill := False;
  fTextAsRect := True;
end;

procedure TRubberCanvasDevice.Bezier(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D;
  const Closed: Boolean);
var
  PathProc: TDrawPathProc;
  Pnts: array of TPoint;
  I: Integer;
begin
  if PP.Count <= 1 then Exit;
  SetLength(Pnts, PP.Count);
  for I := 0 to PP.Count - 1 do
    Pnts[I] := Point2DToPoint(TransformPoint2D(PP[I],
      MultTransf(Transf)));
  DrawOutlineNative0(fCnv,
    CnvDrawBezierPath, Rect2D(0, 0, 0, 0),
    Closed, Pnts);
end;

procedure TRubberCanvasDevice.Poly(PP: TPointsSet2D;
  const LineColor, HatchColor, FillColor: TColor;
  const LineStyle: TLineStyle; const LineWidth: TRealType;
  const Hatching: THatching; const Transf: TTransf2D;
  const Closed: Boolean);
var
  PathProc: TDrawPathProc;
  Pnts: array of TPoint;
  I: Integer;
begin
  if PP.Count <= 1 then Exit;
  SetLength(Pnts, PP.Count);
  for I := 0 to PP.Count - 1 do
    Pnts[I] := Point2DToPoint(TransformPoint2D(PP[I],
      MultTransf(Transf)));
  DrawOutlineNative0(fCnv,
    CnvDrawPolylinePath, Rect2D(0, 0, 0, 0),
    Closed, Pnts);
end;

end.

